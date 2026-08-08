{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
program SedaiBasicCompiler;

{$mode objfpc}{$H+}
{$codepage UTF8}

{ ============================================================================
  SedaiBasicCompiler (sbc) - Compile BASIC source to .basc bytecode

  Usage: sbc <source.bas> [output.basc]

  If output.basc is not specified, the compiled file is saved in the
  current directory with the same name as the source file but with .basc
  extension.

  Exit codes:
    0 = Success
    1 = Error (file not found, compilation error, etc.)
  ============================================================================ }

// Include shared optimization flags
{$I OptimizationFlags.inc}

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SedaiConsoleState,
  Classes, SysUtils,
  // Preprocessor (runs before lexing)
  SedaiPreprocessor,
  // Dialect auto-detection (line numbers => classic, otherwise Modern)
  SedaiRunner,
  // Lexer/Parser
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser,
  // Bytecode
  SedaiSSATypes, SedaiSSA,
  SedaiWasmEmitter, SedaiWasmControl, SedaiWasmBackend,
  SedaiBytecodeTypes, SedaiBytecodeCompiler,
  // Register Allocation
  SedaiRegAlloc,
  // Peephole and Superinstructions
  SedaiPeephole, SedaiSuperinstructions,
  // NOP Compaction
  SedaiNopCompaction,
  // Register Compaction
  SedaiRegisterCompaction,
  // Serialization
  SedaiBytecodeSerializer;

// Include version information
{$I Version.inc}

{ Get system architecture string }
function GetSystemArchitecture: string;
begin
  {$IFDEF CPUX86_64}
    {$IFDEF WINDOWS}
    Result := 'x86_64-win64';
    {$ENDIF}
    {$IFDEF LINUX}
    Result := 'x86_64-linux';
    {$ENDIF}
    {$IFDEF DARWIN}
    Result := 'x86_64-darwin';
    {$ENDIF}
  {$ELSE}
    {$IFDEF CPUI386}
      {$IFDEF WINDOWS}
      Result := 'i386-win32';
      {$ENDIF}
      {$IFDEF LINUX}
      Result := 'i386-linux';
      {$ENDIF}
    {$ELSE}
      {$IFDEF CPUAARCH64}
      Result := 'aarch64';
      {$ELSE}
      Result := 'unknown';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

{ Print version banner }
procedure PrintVersion;
begin
  WriteLn('SedaiBasic Compiler (sbc) ver. ', SEDAIBASIC_VERSION, ' [', SEDAIBASIC_RELEASE_DATE, '] for ', GetSystemArchitecture);
  WriteLn(SEDAIBASIC_COPYRIGHT);
end;

{ Print help information }
procedure PrintHelp;
begin
  PrintVersion;
  WriteLn;
  WriteLn('Usage: sbc <source.bas> [output.basc] [options]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  source.bas      Input BASIC source file');
  WriteLn('  output.basc     Output compiled bytecode file (optional)');
  WriteLn('                  If not specified, saves to current directory');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --help, -h      Show this help message');
  WriteLn('  --verbose, -v   Show compilation progress');
  WriteLn('  --quiet, -q     Suppress all output except errors');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  --target wasm   Emit a WebAssembly module (.wasm) instead of bytecode');
  WriteLn('                  An opcode the backend does not cover is REFUSED with a');
  WriteLn('                  message: in the browser there is no interpreter to fall');
  WriteLn('                  back into, so it must never emit code that runs and lies.');
  WriteLn;
  WriteLn('  sbc program.bas                 Compile to ./program.basc');
  WriteLn('  sbc program.bas --target wasm   Compile to ./program.wasm');
  WriteLn('  sbc program.bas out.basc        Compile to out.basc');
  WriteLn('  sbc program.bas -v              Compile with verbose output');
end;

{ --target wasm: stop after register allocation and hand the SSA to the WASM
  backend instead of the bytecode compiler. Both consume the same program at the
  same point in the pipeline (job/docs/PIANO_WASM.md sec.5-bis). }
var
  OptTargetWasm: Boolean = False;

{ Compile BASIC source to bytecode file }
function CompileFile(const SourceFile, OutputFile: string; Verbose: Boolean): Boolean;
var
  Source: TStringList;
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  WasmBackend: TWasmBackend;
  BytecodeProgram: TBytecodeProgram;
  Serializer: TBytecodeSerializer;
  i, removed: Integer;
  HasLineNums: Boolean;
  QBLangDetected: Boolean;
  {$IFNDEF DISABLE_REG_ALLOC}
  RegAlloc: TLinearScanAllocator;
  {$ENDIF}
begin
  Result := False;
  SSAProgram := nil;
  BytecodeProgram := nil;

  // Load source
  if Verbose then
    WriteLn('Loading source: ', SourceFile);

  Source := TStringList.Create;
  try
    if not FileExists(SourceFile) then
    begin
      WriteLn('ERROR: File not found: ', SourceFile);
      Exit;
    end;

    Source.LoadFromFile(SourceFile);

    if Verbose then
      WriteLn('Source loaded (', Source.Count, ' lines)');

    // Pre-filter: remove Markdown fence lines
    removed := 0;
    for i := Source.Count - 1 downto 0 do
    begin
      if Pos('```', Trim(Source[i])) = 1 then
      begin
        Source.Delete(i);
        Inc(removed);
      end;
    end;
    if Verbose and (removed > 0) then
      WriteLn('Pre-filter: removed ', removed, ' fence line(s)');

    // === PREPROCESSOR === (FreeBASIC #define/#undef/#ifdef/#ifndef/#else/#endif/#include).
    // Pure text->text pass before lexing; #include paths resolve relative to the source file.
    try
      // -lang qb ('#lang "qb"' / the '$lang: "qb" metacommand) takes QB PRINT number spacing
      // (FB zone width + TRAILING space after numerics, fbc-verified; fblite does NOT). Detected
      // on the RAW text: the preprocessor strips both directive forms before the old site ran.
      QBLangDetected := DetectQBLang(Source.Text);
      { The target has to reach the PREPROCESSOR, because a program that offers a
        file-writing mode must be able to keep that branch out of a WASM module
        entirely - the backend refuses an uncovered opcode for being PRESENT, and
        a run-time If does not remove it. }
      GTargetIsWasm := OptTargetWasm;
      Source.Text := PreprocessSource(Source.Text, ExtractFilePath(ExpandFileName(SourceFile)), SourceFile);
    except
      on E: EPreprocessorError do
      begin
        WriteLn('ERROR: ', E.Message);
        Exit;
      end;
    end;

    // Dialect auto-selected by content: a program that uses line numbers is classic;
    // otherwise FreeBASIC/Modern (mirrors sb's inline pipeline and TSedaiRunner).
    HasLineNums := TSedaiRunner.SourceHasLineNumbers(Source.Text);

    // === LEXING ===
    if Verbose then
      WriteLn('Lexing...');

    Lexer := TLexerFSM.Create;
    try
      Lexer.SetHasLineNumbers(HasLineNums);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;
      Lexer.PreScanOptions;

      try
        TokenList := Lexer.ScanAllTokensFast;
        if Verbose then
          WriteLn('Tokenized ', Lexer.TokenCount, ' tokens');
      except
        on E: Exception do
        begin
          WriteLn('ERROR during lexing: ', E.Message);
          Exit;
        end;
      end;

      // === PARSING ===
      if Verbose then
        WriteLn('Parsing...');

      Parser := CreatePackratParser;
      try
        try
          ParserResult := Parser.Parse(TokenList);

          if not ParserResult.Success then
          begin
            WriteLn('ERROR: Parsing failed!');
            if ParserResult.Errors.Count > 0 then
              WriteLn('  ', ParserResult.Errors[0].ToString);
            Exit;
          end;

          if Verbose then
            WriteLn('Parsing successful');
        except
          on E: Exception do
          begin
            WriteLn('ERROR during parsing: ', E.Message);
            Exit;
          end;
        end;
      finally
        Parser.Free;
      end;

      // === SSA GENERATION ===
      if Verbose then
        WriteLn('Generating SSA...');

      SSAGen := TSSAGenerator.Create;
      try
        // Dialect gate for FB lexical scope: MODERN when the source has no line numbers
        // (mirrors the lexer config above), CLASSIC otherwise.
        SSAGen.ModernMode := not HasLineNums;
        try
          SSAProgram := SSAGen.Generate(ParserResult.AST);

          if not Assigned(SSAProgram) then
          begin
            WriteLn('ERROR: SSA generation failed!');
            Exit;
          end;
        except
          on E: Exception do
          begin
            WriteLn('ERROR during SSA generation: ', E.Message);
            Exit;
          end;
        end;

        // === SSA OPTIMIZATIONS ===
        if Verbose then
          WriteLn('Optimizing SSA...');

        // Dead Block Elimination
        {$IFNDEF DISABLE_DBE}
        try
          {$IFNDEF DISABLE_SUB_INLINING}
          try SSAProgram.RunSubInlining; except end;   // unification: before everything
          {$ENDIF}
          SSAProgram.RunDBE;
        except
          on E: Exception do
            WriteLn('WARNING: Dead block elimination failed: ', E.Message);
        end;
        {$ENDIF}

        // Dominator Tree
        {$IFNDEF DISABLE_DOMINATOR_TREE}
        try
          SSAProgram.BuildDominatorTree;
        except
          on E: Exception do
          begin
            WriteLn('ERROR: Dominator tree construction failed: ', E.Message);
            Exit;
          end;
        end;

        // SSA Construction
        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        try
          SSAProgram.RunSSAConstruction;
        except
          on E: Exception do
          begin
            WriteLn('ERROR: SSA construction failed: ', E.Message);
            Exit;
          end;
        end;
        {$ENDIF}
        {$ENDIF}

        // GVN or CSE
        {$IFNDEF DISABLE_GVN}
        {$IFDEF DISABLE_CSE}
        try SSAProgram.RunGVN; except end;
        {$ENDIF}
        {$ENDIF}

        {$IFNDEF DISABLE_CSE}
        {$IFDEF DISABLE_GVN}
        try SSAProgram.RunCSE; except end;
        {$ENDIF}
        {$ENDIF}

        // Other optimizations
        {$IFNDEF DISABLE_ALGEBRAIC}
        try SSAProgram.RunAlgebraic; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_STRENGTH_RED}
        try SSAProgram.RunStrengthReduction; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_GOSUB_INLINE}
        try SSAProgram.RunGosubInlining; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_CONST_PROP}
        try SSAProgram.RunConstProp; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_COPY_PROP}
        try SSAProgram.RunCopyProp; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_LICM}
        try SSAProgram.RunLICM; except end;
        {$ENDIF}

        {$IFNDEF DISABLE_LOOP_UNROLL}
        try
          SSAProgram.ClearDomTree;
          SSAProgram.BuildDominatorTree;
          SSAProgram.RunLoopUnrolling;
        except
        end;
        {$ENDIF}

        {$IFNDEF DISABLE_DCE}
        try SSAProgram.RunDCE; except end;
        {$ENDIF}

        // B4 bounds-check elimination hints (after DCE, before PHI elimination)
        {$IFNDEF DISABLE_RANGE_ANALYSIS}
        try SSAProgram.RunRangeAnalysis; except end;
        {$ENDIF}

        // PHI Elimination
        {$IFNDEF DISABLE_PHI_ELIM}
        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        try
          SSAProgram.RunPhiElimination;
        except
          on E: Exception do
          begin
            WriteLn('ERROR: PHI elimination failed: ', E.Message);
            Exit;
          end;
        end;
        {$ENDIF}
        {$ENDIF}

        // Copy Coalescing
        {$IFNDEF DISABLE_COPY_COAL}
        try SSAProgram.RunCopyCoalescing; except end;
        {$ENDIF}

        // String temp fusion -- see TSSAProgram.RunStringTempFusion.
        if GetEnvironmentVariable('STRFUSE') <> '0' then
        begin
          try SSAProgram.RunStringTempFusion; except end;
          try SSAProgram.RunAscMidFusion; except end;
          try SSAProgram.RunStringTempFusion; except end;
          try SSAProgram.RunConcatCharFusion; except end;
          try SSAProgram.RunConcatDeadSourceMark; except end;
        end;

        // Register Allocation
        {$IFNDEF DISABLE_REG_ALLOC}
        RegAlloc := TLinearScanAllocator.Create(SSAProgram);
        try
          try
            RegAlloc.Run;
          except
            on E: Exception do
            begin
              WriteLn('ERROR: Register allocation failed: ', E.Message);
              Exit;
            end;
          end;
        finally
          RegAlloc.Free;
        end;
        {$ENDIF}

        // === WASM BACKEND (--target wasm) ===
        if OptTargetWasm then
        begin
          WasmBackend := TWasmBackend.Create(SSAProgram, not HasLineNums);
          // "OPTION DIGITS n" reaches the backend the same way it reaches the
          // VM. Without it the module would print a different number of digits
          // than the interpreter for the very same source, and the differential
          // would be right to call that a defect.
          if ParserResult.OptionDigits > 0 then
            WasmBackend.FloatDigits := ParserResult.OptionDigits;
          // -lang qb changes PRINT spacing; same channel as the digit count.
          WasmBackend.QBLang := QBLangDetected;
          try
            if not WasmBackend.Compile then
            begin
              WriteLn('ERROR: WASM backend refused this program: ', WasmBackend.ErrorMessage);
              Exit;
            end;
            WasmBackend.SaveToFile(OutputFile);
            // The global count is printed, not buried: it is the number that says
            // whether "shared register -> global" is still the right answer.
            WriteLn(Format('WASM: %d function(s), %d shared register(s) promoted to globals',
                           [WasmBackend.RegionCount, WasmBackend.GlobalCount]));
            Result := True;
          finally
            WasmBackend.Free;
          end;
          Exit;
        end;

        // === BYTECODE COMPILATION ===
        if Verbose then
          WriteLn('Compiling bytecode...');

        Compiler := TBytecodeCompiler.Create;
        try
          try
            BytecodeProgram := Compiler.Compile(SSAProgram);

            if not Assigned(BytecodeProgram) then
            begin
              WriteLn('ERROR: Bytecode compilation failed!');
              Exit;
            end;
            // Record the source dialect so the VM can pick dialect-aware behaviour when
            // running the .basc (mirrors SSAGen.ModernMode above; persisted by the serializer).
            BytecodeProgram.ModernMode := not HasLineNums;
            // "OPTION DIGITS n", same channel as the dialect above.
            // ⚠️ NOT persisted by the serializer yet: a .basc loses it.
            BytecodeProgram.OptionDigits := ParserResult.OptionDigits;
            // ERMN reports the module an error came from. fbc bakes the source PATH as passed on
            // its command line (native separators) into the executable - so do we (mirrors sb).
            {$IFDEF WINDOWS}
            BytecodeProgram.ModuleName := StringReplace(SourceFile, '/', '\', [rfReplaceAll]);
            {$ELSE}
            BytecodeProgram.ModuleName := SourceFile;
            {$ENDIF}
            BytecodeProgram.QBLang := QBLangDetected;

          except
            on E: Exception do
            begin
              WriteLn('ERROR during bytecode compilation: ', E.Message);
              Exit;
            end;
          end;

          // === BYTECODE OPTIMIZATIONS ===
          if Verbose then
            WriteLn('Optimizing bytecode...');

          // Peephole optimization
          {$IFNDEF DISABLE_PEEPHOLE}
          try RunPeephole(BytecodeProgram); except end;
          {$ENDIF}

          // Superinstructions
          {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
          try RunSuperinstructions(BytecodeProgram); except end;
          {$ENDIF}

          // NOP Compaction
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_NOP_COMPACTION}
          try RunNopCompaction(BytecodeProgram); except end;
          {$ENDIF}
          {$ENDIF}

          // Peephole pass 2
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_PEEPHOLE}
          try
            RunPeephole(BytecodeProgram);
            {$IFNDEF DISABLE_NOP_COMPACTION}
            RunNopCompaction(BytecodeProgram);
            {$ENDIF}
          except
          end;
          {$ENDIF}
          {$ENDIF}

          // Register Compaction
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_REG_COMPACTION}
          try RunRegisterCompaction(BytecodeProgram); except end;
          {$ENDIF}
          {$ENDIF}

          // === SERIALIZATION ===
          if Verbose then
            WriteLn('Saving bytecode to: ', OutputFile);

          Serializer := TBytecodeSerializer.Create;
          try
            try
              Serializer.SaveToFile(BytecodeProgram, OutputFile);
              Result := True;

              if Verbose then
              begin
                WriteLn;
                WriteLn('Compilation successful!');
                WriteLn('  Instructions: ', BytecodeProgram.GetInstructionCount);
                WriteLn('  Variables:    ', BytecodeProgram.GetVariableCount);
                WriteLn('  Strings:      ', BytecodeProgram.StringConstants.Count);
                WriteLn('  Output file:  ', OutputFile);
              end;
            except
              on E: Exception do
              begin
                WriteLn('ERROR saving bytecode: ', E.Message);
                Exit;
              end;
            end;
          finally
            Serializer.Free;
          end;

        finally
          BytecodeProgram.Free;
          Compiler.Free;
        end;

      finally
        SSAProgram.Free;
        SSAGen.Free;
      end;

      ParserResult.Free;

    finally
      Lexer.Free;
    end;

  finally
    Source.Free;
  end;
end;

var
  SourceFile, OutputFile: string;
  OptVerbose, OptQuiet, OptHelp: Boolean;
  i: Integer;
  Param: string;

begin
  try
    // Set console code page to UTF-8
    {$IFDEF WINDOWS}
    SetupConsoleUTF8;   // saves + restores the parent console's code pages; no-op when redirected
    {$ENDIF}

    // Parse command-line parameters
    SourceFile := '';
    OutputFile := '';
    OptVerbose := False;
    OptQuiet := False;
    OptHelp := False;

    for i := 1 to ParamCount do
    begin
      Param := ParamStr(i);
      if (Param = '--help') or (Param = '-h') or (Param = '-?') then
        OptHelp := True
      else if (Param = '--verbose') or (Param = '-v') then
        OptVerbose := True
      else if (Param = '--quiet') or (Param = '-q') then
        OptQuiet := True
      else if (Param = '--target=wasm') or (Param = '--target-wasm') then
        OptTargetWasm := True
      else if (Param = '--target') and (i < ParamCount) then
      begin
        if SameText(ParamStr(i + 1), 'wasm') then OptTargetWasm := True
        else if not SameText(ParamStr(i + 1), 'bytecode') then
        begin
          WriteLn('ERROR: unknown target "', ParamStr(i + 1), '" (bytecode, wasm)');
          ExitCode := 1;
          Exit;
        end;
      end
      else if SameText(Param, 'wasm') and (i > 1) and (ParamStr(i - 1) = '--target') then
        // consumed by the branch above
      else if (Pos('-', Param) <> 1) then
      begin
        // Positional argument
        if SourceFile = '' then
          SourceFile := Param
        else if OutputFile = '' then
          OutputFile := Param;
      end;
    end;

    // Show help if requested or no file provided
    if OptHelp or (SourceFile = '') then
    begin
      PrintHelp;
      if SourceFile = '' then
        ExitCode := 1;
      Exit;
    end;

    // Determine output file
    if OutputFile = '' then
      if OptTargetWasm then
        OutputFile := ChangeFileExt(ExtractFileName(SourceFile), '.wasm')
      else
        OutputFile := ChangeFileExt(ExtractFileName(SourceFile), '.basc');

    // Show banner if not quiet
    if not OptQuiet then
      PrintVersion;

    // Compile
    if CompileFile(SourceFile, OutputFile, OptVerbose) then
    begin
      if not OptQuiet and not OptVerbose then
        WriteLn('Compiled: ', ExtractFileName(SourceFile), ' -> ', OutputFile);
      ExitCode := 0;
    end
    else
      ExitCode := 1;

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
