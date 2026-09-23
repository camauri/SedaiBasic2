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
  // M5.2 threading: on Unix the cthreads unit MUST be first so BeginThread uses pthreads.
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SedaiConsoleState,
  Classes, SysUtils,
  // Preprocessor (runs before lexing)
  SedaiPreprocessor,
  // Where headers and libraries live (sedai.conf / SEDAI_* / fbc on the PATH) - see LoadConfig below
  SedaiConfig, SedaiMemoryMode,
  // --fuse: sb + .basc + trailer, a program that IS an executable (DIVERGENZE 573)
  SedaiFused,
  // Installs GPPTypeSizeHook: see the note in SedaiTypeSizeProbe. Linked for its initialization.
  SedaiTypeSizeProbe,
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
  WriteLn('  --fuse          Build a stand-alone EXECUTABLE: the sb runtime with the');
  WriteLn('                  program inside (default name: the source without extension).');
  WriteLn('                  C libraries that look next to "the executable" find the');
  WriteLn('                  program''s directory, as with an fbc executable. Also takes');
  WriteLn('                  an existing .basc: sbc --fuse prog.basc');
  WriteLn('  --runtime FILE  The sb to fuse with (default: the sb beside sbc). A Windows');
  WriteLn('                  sb.exe makes a Windows executable.');
  WriteLn('  --jit, --home   With --fuse: run with the loop JIT / in the program''s');
  WriteLn('                  directory, as sb --jit / sb --home would');
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
  WriteLn('  sbc --fuse program.bas          Build ./program, a stand-alone executable');
end;

{ --target wasm: stop after register allocation and hand the SSA to the WASM
  backend instead of the bytecode compiler. Both consume the same program at the
  same point in the pipeline (job/docs/PIANO_WASM.md sec.5-bis). }
var
  OptTargetWasm: Boolean = False;

{ Compile BASIC source to bytecode file }
function CompileFile(const SourceFile, OutputFile: string; Verbose: Boolean;
  ExtraModules: TStrings = nil): Boolean;
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
  MemSrc, MemErr: string;
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
    // ⭐ Several modules become one compilation, in fbc's measured order: the non-main modules first
    // (their module-level code runs before the main module's), the main module last. The rule lives in
    // TSedaiRunner.LoadProgramModules so that sb and sbc cannot drift apart on it.
    TSedaiRunner.LoadProgramModules(Source, SourceFile, ExtraModules);

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
      // ⛔ THE CONFIGURATION, exactly as sb reads it. sbc never called LoadConfig, so ConfigList and
      // ConfigValue answered empty, FreeBASIC's include tree was never found, "#include crt.bi" was
      // dropped in silence and every C call was "Array not declared: PRINTF" - while sb ran the same
      // file. A missing include was not refused either (guard m894e). basc_sweep: 4 SBCFAIL + 1 DIFF.
      LoadConfig(ExtractFilePath(ExpandFileName(SourceFile)));
      // ⭐ THE MEMORY MODE, resolved where sb resolves it: after the configuration, before anything is generated
      // (SedaiMemoryMode). A WASM module keeps the isolated model - its backend has one of its own and no FFI -
      // so an explicit fb there is refused instead of ignored.
      if not ResolveMemoryMode(GMemoryModeFlag, GMemoryMode, MemSrc, MemErr) then
      begin
        WriteLn('ERROR: ', MemErr);
        Exit;
      end;
      if OptTargetWasm then
      begin
        if (GMemoryModeFlag <> '') and (GMemoryMode = mmFB) then
        begin
          WriteLn('ERROR: --memory=fb: the WASM target has only the strict memory mode');
          Exit;
        end;
        GMemoryMode := mmStrict;
        MemSrc := 'wasm target';
      end;
      NoteMemoryMode(GMemoryMode, MemSrc);
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
        SSAGen.NativeMemory := GMemoryMode = mmFB;
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
          // ⛔ ...and ARGUMENT-SLOT FORWARDING right after it, as `sb` does: without it an inlined call keeps its
          // XferStore/XferLoad pairs, and spectral_1w ran 23.5 s from its .basc against 14.5 s from source.
          try SSAProgram.RunXferForwarding; except end;
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
        // ⛔ ...and INDEX REDUCTION after it, as sb does: the pipelines had drifted, and a .basc lost what sb keeps.
        try SSAProgram.RunIndexReduction; except end;

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
        // "acc += tab[Asc(Mid(s,i,1))+1]" fused AFTER register allocation, as sb's pipeline does (see SedaiBasicVM.lpr):
        // without it reverse-complement ran 2073 ms from its .basc against 981 ms from source.
        try SSAProgram.RunAppendMappedFusion; except end;
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
            BytecodeProgram.NativeMemory := GMemoryMode = mmFB;   // the memory mode travels in the .basc header
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

          // ⛔⛔⛔ SUPERINSTRUCTIONS ARE NOT FUSED HERE, AND THIS IS THE POINT OF THE PASS, NOT AN
          // OMISSION. Whether fusing pays depends on which ENGINE runs the bytecode: the
          // interpreter wants it, the loop JIT is destroyed by it (it has arms for 3 of the 72
          // superinstructions, so a fused hot loop bails whole). RunSuperinstructions carries a
          // GJitWillRun gate for exactly that - and `sbc` cannot set it, because `sbc` does not
          // know how its output will be run. It always fused, and a `.basc` cannot be un-fused.
          //
          // 📊 3 Sep 2026, fannkuch-redux-modern N=11, one binary: from the source `--jit` took
          // 2 019 ms and compiled 7 loops; from THIS compiler's output `--jit` took 21 641 ms and
          // compiled ZERO. Ten and a half times, on the same program, decided here.
          //
          // ⇒ The runner fuses instead, at load, where the engine is known: SedaiSuperinstructions'
          // FuseAtLoad, called by `sb` and `sbv` on every .basc. An interpreted .basc is therefore
          // exactly as fast as before; a JIT-ed one is no longer crippled.
          // ⚠️ If a fourth runner learns to load a .basc, it calls FuseAtLoad too - or it silently
          // runs unfused bytecode, which is a 10% interpreter loss nothing would report.

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
  ExtraModules: TStringList;
  OptVerbose, OptQuiet, OptHelp: Boolean;
  i: Integer;
  Param: string;
  OptFuse: Boolean;
  FuseFlags: LongWord;
  FuseRuntime, FuseBasc, FuseTmp, Head: string;
  FuseSkipNext: Boolean;
  FuseSecond: string;
  HeadStream: TFileStream;

begin
    // ⛔ EVERY unit declares {$codepage UTF8}, so a string LITERAL carries code page 65001 - while a
    // string BUILT at run time carries DefaultSystemCodePage, which is CP_ACP (0) because no cwstring
    // unit is linked. FPC compares the two code pages on every concatenation: when they differ it takes
    // ansistr_concat_COMPLEX, which converts BOTH operands to UnicodeString, concatenates, and converts
    // the result back. So `s := s + 'literal'` - the single most common statement in this compiler - was
    // paying a full UTF-16 round trip, and AnsiCompareText paid the same conversion on both arguments.
    // Naming the two code pages the same makes both take the byte path. Measured: concatenation 7.0x,
    // AnsiCompareText 3.5x (200k iterations, 98 -> 14 ms and 21 -> 6 ms).
    SetMultiByteConversionCodePage(CP_UTF8);
  try
    // Set console code page to UTF-8
    {$IFDEF WINDOWS}
    SetupConsoleUTF8;   // saves + restores the parent console's code pages; no-op when redirected
    {$ENDIF}

    // Parse command-line parameters
    SourceFile := '';
    OutputFile := '';
    ExtraModules := TStringList.Create;
    OptVerbose := False;
    OptQuiet := False;
    OptHelp := False;
    OptFuse := False;
    FuseFlags := 0;
    FuseRuntime := '';
    FuseSkipNext := False;
    FuseSecond := '';

    for i := 1 to ParamCount do
    begin
      Param := ParamStr(i);
      if FuseSkipNext then
        FuseSkipNext := False
      else if Param = '--fuse' then
        OptFuse := True
      else if (Param = '--runtime') and (i < ParamCount) then
      begin
        FuseRuntime := ParamStr(i + 1);
        FuseSkipNext := True;
      end
      else if Param = '--jit' then
        FuseFlags := FuseFlags or FUSED_FLAG_JIT
      else if Param = '--home' then
        FuseFlags := FuseFlags or FUSED_FLAG_HOME
      else if (Param = '--help') or (Param = '-h') or (Param = '-?') then
        OptHelp := True
      else if (Param = '--verbose') or (Param = '-v') then
        OptVerbose := True
      else if (Param = '--quiet') or (Param = '-q') then
        OptQuiet := True
      else if Pos(MEMORY_MODE_FLAG, LowerCase(Param)) = 1 then
        GMemoryModeFlag := Copy(Param, Length(MEMORY_MODE_FLAG) + 1, MaxInt)   // fb | strict (SedaiMemoryMode)
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
        // ⭐ A POSITIONAL ENDING IN .bas IS A MODULE, and the first of them is the MAIN module
        // (DIVERGENZE 162) - which is how FreeBASIC spells it: "fbc main.bas b.bas c.bas". Anything
        // else is the output name, so "sbc a.bas out.basc" keeps working exactly as before.
        // ⛔ This is also what closes a data-loss bug: before, the second .bas was taken as the OUTPUT
        // and the compiler wrote bytecode over the user's second source, silently.
        if SameText(ExtractFileExt(Param), '.bas') then
        begin
          if SourceFile = '' then SourceFile := Param
          else ExtraModules.Add(Param);
        end
        else if OutputFile = '' then
          OutputFile := Param
        else if FuseSecond = '' then
          FuseSecond := Param;   // "sbc --fuse prog.basc out": the first is the INPUT, this is the output
      end;
    end;

    // ⭐ --fuse from an existing .basc: the positional that would be the output name is the INPUT.
    FuseBasc := '';
    if OptFuse and (SourceFile = '') and SameText(ExtractFileExt(OutputFile), '.basc') then
    begin
      FuseBasc := OutputFile;
      OutputFile := FuseSecond;
    end;

    if (FuseSecond <> '') and (FuseBasc = '') then
    begin
      WriteLn(ErrOutput, 'ERROR: unexpected argument "', FuseSecond, '"');
      ExitCode := 1;
      Exit;
    end;

    // Show help if requested or no file provided
    if OptHelp or ((SourceFile = '') and (FuseBasc = '')) then
    begin
      PrintHelp;
      if SourceFile = '' then
        ExitCode := 1;
      Exit;
    end;

    // ⛔⛔ AN OUTPUT NAMED *.bas IS A SOURCE FILE, AND WRITING BYTECODE OVER IT DESTROYS IT.
    // "sbc a.bas b.bas" is how FreeBASIC spells a MULTI-MODULE build - fbc compiles both sources into
    // one program - and here the second positional is the OUTPUT name, so that command silently
    // overwrote the user's second source with a .basc image. Measured 7 Sep 2026: b.bas came back as
    // "OS/2 graphic array" and its text was gone. ⇒ Refuse it, and NAME the reason, because the user
    // who typed it was not asking for an output file at all (DIVERGENZE 162).
    // ⚠️ The refusal is on the EXTENSION only: any other output name still works exactly as before,
    // and "sb prog.bas arg1 arg2" is untouched - there the extra words are the PROGRAM's arguments,
    // which is what a compiled binary does and what fbc's own runtime does.
    // ⚠️ Unreachable through the positional route since a .bas argument became a MODULE, and kept as a
    // guard because the hazard is real: writing bytecode over a source DESTROYS it.
    if (OutputFile <> '') and SameText(ExtractFileExt(OutputFile), '.bas') then
    begin
      WriteLn(ErrOutput, 'ERROR: refusing to write bytecode over "', OutputFile,
              '": a .bas file is SOURCE, and this would destroy it.');
      WriteLn(ErrOutput, '  If you meant an output file, give it a .basc extension.');
      ExitCode := 1;
      Exit;
    end;

    // ⛔ --jit / --home / --runtime shape a FUSED executable only: accepted and ignored on a plain compile they
    // would be the flag that did nothing and said nothing.
    if not OptFuse and ((FuseFlags <> 0) or (FuseRuntime <> '')) then
    begin
      WriteLn(ErrOutput, 'ERROR: --jit, --home and --runtime apply to --fuse only (sb takes --jit/--home at run time)');
      ExitCode := 1;
      Exit;
    end;

    if OptFuse then
    begin
      // ⛔ Every refusal here is LOUD: a fused file that half-worked would be a program that runs somebody
      // else's bytes, or none. What cannot be fused is named before anything is compiled.
      if OptTargetWasm then
      begin
        WriteLn(ErrOutput, 'ERROR: --fuse makes a native executable; it cannot be combined with --target wasm');
        ExitCode := 1;
        Exit;
      end;
      if FuseRuntime = '' then
      begin
        FuseRuntime := ExtractFilePath(SelfExecutablePath) + 'sb';
        {$IFDEF WINDOWS}FuseRuntime := FuseRuntime + '.exe';{$ENDIF}
      end;
      if not FileExists(FuseRuntime) then
      begin
        WriteLn(ErrOutput, 'ERROR: --fuse needs the sb runtime, and "', FuseRuntime, '" does not exist (give it with --runtime)');
        ExitCode := 1;
        Exit;
      end;
      // The runtime decides the target: a PE (MZ) makes a Windows executable, which is named .exe.
      Head := '';
      HeadStream := TFileStream.Create(FuseRuntime, fmOpenRead or fmShareDenyNone);
      try
        SetLength(Head, 4);
        if HeadStream.Read(Head[1], 4) < 4 then Head := '';
      finally
        HeadStream.Free;
      end;
      if (Copy(Head, 1, 2) <> 'MZ') and (Head <> #$7F'ELF') then
      begin
        WriteLn(ErrOutput, 'ERROR: "', FuseRuntime, '" is neither an ELF nor a PE executable');
        ExitCode := 1;
        Exit;
      end;
      if OutputFile = '' then
      begin
        if SourceFile <> '' then OutputFile := ChangeFileExt(ExtractFileName(SourceFile), '')
        else OutputFile := ChangeFileExt(ExtractFileName(FuseBasc), '');
        if Copy(Head, 1, 2) = 'MZ' then OutputFile := OutputFile + '.exe';
      end;
      if SameText(ExtractFileExt(OutputFile), '.bas') or SameText(ExtractFileExt(OutputFile), '.basc') then
      begin
        WriteLn(ErrOutput, 'ERROR: refusing to write an executable over "', OutputFile, '"');
        ExitCode := 1;
        Exit;
      end;
      if not OptQuiet then
        PrintVersion;
      FuseTmp := '';
      if FuseBasc = '' then
      begin
        FuseTmp := OutputFile + '.fuse-tmp.basc';
        if not CompileFile(SourceFile, FuseTmp, OptVerbose, ExtraModules) then
        begin
          DeleteFile(FuseTmp);
          ExitCode := 1;
          Exit;
        end;
        FuseBasc := FuseTmp;
      end;
      try
        WriteFusedExecutable(FuseRuntime, FuseBasc, OutputFile, FuseFlags);
      finally
        if FuseTmp <> '' then DeleteFile(FuseTmp);
      end;
      if not OptQuiet then
        if SourceFile <> '' then
          WriteLn('Fused: ', ExtractFileName(SourceFile), ' + ', FuseRuntime, ' -> ', OutputFile)
        else
          WriteLn('Fused: ', ExtractFileName(FuseBasc), ' + ', FuseRuntime, ' -> ', OutputFile);
      ExitCode := 0;
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
    if CompileFile(SourceFile, OutputFile, OptVerbose, ExtraModules) then
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
