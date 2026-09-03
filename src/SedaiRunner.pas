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
unit SedaiRunner;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiRunner - Unified loader for BASIC source (.bas) and bytecode (.basc)

  This unit provides a unified interface for loading and executing BASIC
  programs, regardless of whether they are source files or pre-compiled
  bytecode.

  Usage:
    var
      Runner: TSedaiRunner;
      Program_: TBytecodeProgram;
    begin
      Runner := TSedaiRunner.Create;
      try
        Program_ := Runner.Load('program.bas');  // or 'program.basc'
        // Use Program_ with TBytecodeVM...
      finally
        Runner.Free;
      end;
    end;

  The runner automatically:
  - Detects file type by extension (.bas = source, .basc = bytecode)
  - Compiles .bas files through the full optimization pipeline
  - Loads .basc files directly via the serializer
  ============================================================================ }

interface

uses
  Classes, SysUtils,
  SedaiBytecodeTypes, SedaiBytecodeSerializer;

type
  { File type detection }
  TSedaiFileType = (
    sftUnknown,
    sftSource,    // .bas - BASIC source code
    sftBytecode   // .basc - Pre-compiled bytecode
  );

  { TSedaiRunner - Unified program loader }
  TSedaiRunner = class
  private
    FVerbose: Boolean;
    FLastError: string;
    FSkipSuperinstructions: Boolean;
    FFreeBasicMode: Boolean;   // FreeBASIC/Modern dialect: no line numbers

    function CompileSource(const SourceFile: string): TBytecodeProgram;
    function LoadBytecode(const BytecodeFile: string): TBytecodeProgram;
  public
    constructor Create;

    { Detect file type from extension }
    class function DetectFileType(const FileName: string): TSedaiFileType;

    { True if the source uses classic line numbers (a logical line begins with an
      integer). Used to auto-select the dialect at LOAD: line numbers => classic,
      otherwise FreeBASIC/Modern. }
    class function SourceHasLineNumbers(const Source: string): Boolean;

    { Load program from file (auto-detects type) }
    function Load(const FileName: string): TBytecodeProgram;

    { Load from specific type }
    function LoadFromSource(const SourceFile: string): TBytecodeProgram;
    function LoadFromBytecode(const BytecodeFile: string): TBytecodeProgram;

    { Properties }
    property Verbose: Boolean read FVerbose write FVerbose;
    property LastError: string read FLastError;
    property SkipSuperinstructions: Boolean read FSkipSuperinstructions write FSkipSuperinstructions;
    // FreeBASIC/Modern dialect (no line numbers). Also auto-enabled by a .fb/.fbas
    // source extension. Default False = classic BASIC (line numbers optional).
    property FreeBasicMode: Boolean read FFreeBasicMode write FFreeBasicMode;
  end;

  { Exception for runner errors }
  ESedaiRunnerError = class(Exception);

implementation

uses
  // Preprocessor (runs before lexing)
  SedaiPreprocessor,
  // Lexer/Parser
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser,
  // SSA and Bytecode
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeCompiler,
  // Register Allocation
  SedaiRegAlloc,
  // Optimizations
  SedaiPeephole, SedaiSuperinstructions,
  // SedaiAot: only for RunSuperinstructionsAot - the fusion pass told where the AOT will go.
  SedaiAot,
  SedaiNopCompaction, SedaiRegisterCompaction;

{ ⛔ PERCHE' ESISTE (20 ago 2026). I ventidue passi di ottimizzazione erano invocati come
  `try SSAProgram.RunX; except end;` - un except NUDO. Se un passo sollevava un'eccezione:
    - nessuno lo sapeva: niente messaggio, niente codice d'uscita, niente;
    - il passo si fermava A META', lasciando l'IR in uno stato PARZIALE - meta' trasformazioni
      applicate e meta' no, che non e' come non averlo eseguito;
    - la compilazione proseguiva come se niente fosse.
  Un audit sul corpus (162 programmi) ha trovato TRE passi che non cambiano un byte, e con
  l'except nudo non era distinguibile «non trova niente» da «esplode alla prima istruzione».

  Ora il fallimento PARLA. E OPT_STRICT=1 lo fa RILANCIARE, cosi' le reti possono pretendere che
  nessun passo fallisca invece di misurarne solo il risultato finale. }
procedure OptPassFailed(const PassName: string; E: Exception);
begin
  WriteLn(ErrOutput, '[OPT] il passo ', PassName, ' e'' FALLITO: ',
          E.ClassName, ': ', E.Message,
          ' - l''IR resta nello stato parziale in cui il passo si e'' interrotto');
  Flush(ErrOutput);
  if GetEnvironmentVariable('OPT_STRICT') = '1' then
    raise Exception.CreateFmt('OPT_STRICT: il passo %s e'' fallito (%s: %s)',
                              [PassName, E.ClassName, E.Message]);
end;


{ Include optimization flags }
{$I OptimizationFlags.inc}

{ ============================================================================
  TSedaiRunner
  ============================================================================ }

constructor TSedaiRunner.Create;
begin
  inherited Create;
  FVerbose := False;
  FLastError := '';
end;

class function TSedaiRunner.DetectFileType(const FileName: string): TSedaiFileType;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if (Ext = '.bas') then
    Result := sftSource
  else if (Ext = '.basc') then
    Result := sftBytecode
  else
    Result := sftUnknown;
end;

class function TSedaiRunner.SourceHasLineNumbers(const Source: string): Boolean;
// Heuristic: the program uses line numbers iff some logical line begins with an integer. A modern
// (FreeBASIC) statement never starts a line with a bare number; a classic line is "<number>
// <statement>". A FreeBASIC line-CONTINUATION line does not begin a logical one, so its first
// character decides nothing: "a = 1 + _" / "    2 + _" / "    3" must not read as line number 3.
// ⚠️ That rule was in the original and I dropped it while making this comment-aware, on the written
// claim that it was "irrelevant here". It is not, and m214_linecont caught it on the next run.
//
// ⛔⛔ IT MUST NOT LOOK INSIDE COMMENTS OR STRINGS, AND IT USED TO. This runs BEFORE the lexer - it is
// what CONFIGURES the lexer - so it has to do its own skipping, and skipping nothing meant that
// A COMMENT COULD CHANGE THE DIALECT OF THE WHOLE PROGRAM. The FreeBASIC manual ends dozens of its
// examples with a block comment showing the expected output:
//     /' Output:
//      0
//      1
//     '/
// and that " 0" made the program CLASSIC. Everything downstream moved with it - Commodore prints a
// trailing space after a number, POS and CSRLIN count from 0, PAINT floods by the seed's colour, the
// error messages change - because of a comment. Ten of the manual's own examples were affected.
//
// ⚠️ The duplication with the parser (which decides the same thing from ttLineNumber and cannot be
// fooled, because by then the lexer has removed the comments) is NOT removable: something has to
// answer before there are any tokens. So this one is made to agree with it instead.
var
  i, n, Depth: Integer;
  AtLineStart: Boolean;
  LastSig: Char;           // last significant character of the line, for the '_' continuation rule
  Ch: Char;
begin
  Result := False;
  n := Length(Source);
  i := 1;
  Depth := 0;              // /' ... '/ nesting
  AtLineStart := True;     // no non-blank character seen yet on this logical line
  LastSig := #0;
  while i <= n do
  begin
    Ch := Source[i];
    if Depth > 0 then                                   // inside a block comment
    begin
      if (Ch = '/') and (i < n) and (Source[i + 1] = '''') then
      begin Inc(Depth); Inc(i, 2); Continue; end;
      if (Ch = '''') and (i < n) and (Source[i + 1] = '/') then
      begin Dec(Depth); Inc(i, 2); Continue; end;
      if Ch = #10 then                                  // a comment line starts no statement
      begin
        AtLineStart := LastSig <> '_';
        LastSig := #0;
      end;
      Inc(i);
      Continue;
    end;
    if Ch = #13 then begin Inc(i); Continue; end;
    if Ch = #10 then
    begin
      // A line ending in '_' continues into the next one, which therefore begins no statement.
      AtLineStart := LastSig <> '_';
      LastSig := #0;
      Inc(i);
      Continue;
    end;
    if (Ch = ' ') or (Ch = #9) then begin Inc(i); Continue; end;
    if (Ch = '/') and (i < n) and (Source[i + 1] = '''') then
    begin Inc(Depth); Inc(i, 2); Continue; end;          // /' opens a block comment
    if Ch = '''' then
    begin
      while (i <= n) and (Source[i] <> #10) do Inc(i);   // ' runs to end of line
      Continue;
    end;
    if Ch = '"' then
    begin
      Inc(i);                                           // a string literal, skipped whole
      while (i <= n) and (Source[i] <> '"') and (Source[i] <> #10) do Inc(i);
      if (i <= n) and (Source[i] = '"') then Inc(i);
      AtLineStart := False;
      LastSig := '"';
      Continue;
    end;
    if AtLineStart then                                 // the first real character decides
    begin
      if (Ch >= '0') and (Ch <= '9') then Exit(True);
      AtLineStart := False;
    end;
    LastSig := Ch;
    Inc(i);
  end;
end;

function TSedaiRunner.Load(const FileName: string): TBytecodeProgram;
var
  FileType: TSedaiFileType;
begin
  Result := nil;
  FLastError := '';

  if not FileExists(FileName) then
  begin
    FLastError := Format('File not found: %s', [FileName]);
    raise ESedaiRunnerError.Create(FLastError);
  end;

  FileType := DetectFileType(FileName);

  case FileType of
    sftSource:
      Result := LoadFromSource(FileName);
    sftBytecode:
      Result := LoadFromBytecode(FileName);
    else
    begin
      FLastError := Format('Unknown file type: %s (expected .bas or .basc)', [FileName]);
      raise ESedaiRunnerError.Create(FLastError);
    end;
  end;
end;

function TSedaiRunner.LoadFromSource(const SourceFile: string): TBytecodeProgram;
begin
  Result := CompileSource(SourceFile);
end;

function TSedaiRunner.LoadFromBytecode(const BytecodeFile: string): TBytecodeProgram;
begin
  Result := LoadBytecode(BytecodeFile);
end;

function TSedaiRunner.LoadBytecode(const BytecodeFile: string): TBytecodeProgram;
var
  Serializer: TBytecodeSerializer;
begin
  if FVerbose then
    WriteLn('Loading bytecode: ', BytecodeFile);

  Serializer := TBytecodeSerializer.Create;
  try
    try
      Result := Serializer.LoadFromFile(BytecodeFile);
      // ⛔⛔ THE FUSION PASS RUNS HERE AND NOT IN `sbc`, because whether it pays depends on the
      // ENGINE and only the runner knows which one. See the header of FuseAtLoad: a `.basc` that
      // arrived fused made `--jit` ten and a half times slower than the same program from source,
      // because the loop JIT bails whole on a superinstruction and cannot un-fuse one.
      // ⚠️ It is inside LoadBytecode rather than beside its two callers on purpose - `sbv` reaches
      // a .basc through here, and a third runner that learns to would otherwise run UNFUSED
      // bytecode and lose ~10% with nothing to report it.
      if not FSkipSuperinstructions then FuseAtLoad(Result);
      if FVerbose then
        WriteLn('Loaded ', Result.GetInstructionCount, ' instructions');
    except
      on E: Exception do
      begin
        FLastError := Format('Failed to load bytecode: %s', [E.Message]);
        raise ESedaiRunnerError.Create(FLastError);
      end;
    end;
  finally
    Serializer.Free;
  end;
end;

function TSedaiRunner.CompileSource(const SourceFile: string): TBytecodeProgram;
var
  Source: TStringList;
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  i, removed: Integer;
  UseFreeBasic: Boolean;
  {$IFNDEF DISABLE_REG_ALLOC}
  RegAlloc: TLinearScanAllocator;
  {$ENDIF}
begin
  Result := nil;
  FLastError := '';

  if FVerbose then
    WriteLn('Compiling source: ', SourceFile);

  Source := TStringList.Create;
  try
    Source.LoadFromFile(SourceFile);

    if FVerbose then
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

    // === PREPROCESSOR === (FreeBASIC #define/#undef/#ifdef/.../#include) before lexing.
    try
      Source.Text := PreprocessSource(Source.Text, GetCurrentDir);
    except
      on E: EPreprocessorError do
      begin
        // The outer try..finally frees Source; just record the error and bail.
        FLastError := Format('Preprocessor error: %s', [E.Message]);
        Exit(nil);
      end;
    end;

    // === LEXING ===
    // Dialect auto-selected at LOAD by content: a program with line numbers is
    // classic; otherwise FreeBASIC/Modern (no line numbers). FreeBasicMode forces
    // Modern. Spaces-between-tokens and case-insensitivity hold in both dialects.
    UseFreeBasic := FFreeBasicMode or (not SourceHasLineNumbers(Source.Text));
    Lexer := TLexerFSM.Create;
    try
      Lexer.SetHasLineNumbers(not UseFreeBasic);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;
      Lexer.PreScanOptions;

      try
        TokenList := Lexer.ScanAllTokensFast;
        if FVerbose then
          WriteLn('Tokenized ', Lexer.TokenCount, ' tokens');
      except
        on E: Exception do
        begin
          FLastError := Format('Lexer error: %s', [E.Message]);
          raise ESedaiRunnerError.Create(FLastError);
        end;
      end;

      // === PARSING ===
      Parser := CreatePackratParser;
      try
        try
          ParserResult := Parser.Parse(TokenList);

          if not ParserResult.Success then
          begin
            if ParserResult.Errors.Count > 0 then
              FLastError := Format('Parse error: %s', [ParserResult.Errors[0].ToString])
            else
              FLastError := 'Parse error: unknown';
            raise ESedaiRunnerError.Create(FLastError);
          end;
        except
          on E: ESedaiRunnerError do
            raise;
          on E: Exception do
          begin
            FLastError := Format('Parser error: %s', [E.Message]);
            raise ESedaiRunnerError.Create(FLastError);
          end;
        end;
      finally
        Parser.Free;
      end;

      // === SSA GENERATION ===
      SSAGen := TSSAGenerator.Create;
      try
        try
          SSAProgram := SSAGen.Generate(ParserResult.AST);

          if not Assigned(SSAProgram) then
          begin
            FLastError := 'SSA generation failed';
            raise ESedaiRunnerError.Create(FLastError);
          end;
        except
          on E: ESedaiRunnerError do
            raise;
          on E: Exception do
          begin
            FLastError := Format('SSA generation error: %s', [E.Message]);
            raise ESedaiRunnerError.Create(FLastError);
          end;
        end;

        // === SSA OPTIMIZATIONS ===
        {$IFNDEF DISABLE_DBE}
        {$IFNDEF DISABLE_SUB_INLINING}
        try SSAProgram.RunSubInlining; except on E: Exception do OptPassFailed('SubInlining', E); end;   // unification: before everything
        {$ENDIF}
        try SSAProgram.RunDBE; except on E: Exception do OptPassFailed('DBE', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_DOMINATOR_TREE}
        try
          SSAProgram.BuildDominatorTree;
        except
          on E: Exception do
          begin
            FLastError := Format('Dominator tree error: %s', [E.Message]);
            raise ESedaiRunnerError.Create(FLastError);
          end;
        end;

        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        try
          SSAProgram.RunSSAConstruction;
        except
          on E: Exception do
          begin
            FLastError := Format('SSA construction error: %s', [E.Message]);
            raise ESedaiRunnerError.Create(FLastError);
          end;
        end;
        {$ENDIF}
        {$ENDIF}

        // GVN or CSE
        {$IFNDEF DISABLE_GVN}
        {$IFDEF DISABLE_CSE}
        try SSAProgram.RunGVN; except on E: Exception do OptPassFailed('GVN', E); end;
        {$ENDIF}
        {$ENDIF}

        {$IFNDEF DISABLE_CSE}
        {$IFDEF DISABLE_GVN}
        try SSAProgram.RunCSE; except on E: Exception do OptPassFailed('CSE', E); end;
        {$ENDIF}
        {$ENDIF}

        // Other optimizations
        {$IFNDEF DISABLE_ALGEBRAIC}
        try SSAProgram.RunAlgebraic; except on E: Exception do OptPassFailed('Algebraic', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_STRENGTH_RED}
        try SSAProgram.RunStrengthReduction; except on E: Exception do OptPassFailed('StrengthReduction', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_GOSUB_INLINE}
        try SSAProgram.RunGosubInlining; except on E: Exception do OptPassFailed('GosubInlining', E); end;
        {$ENDIF}

        // ⛔ CONST_PROP RIMOSSO DALLA PIPELINE (21 ago 2026). Non e' stato spento: e' STACCATO,
        // perche' non poteva funzionare. Il passo cerca ssaStoreVar / ssaLoadVar per trovare le
        // variabili BASIC assegnate una volta sola con un valore costante - e la generazione SSA
        // non emette quei due opcode NEMMENO UNA VOLTA (zero siti in SedaiSSA.pas): le variabili
        // sono promosse a registri durante la costruzione dell'SSA. Il passo e' del 25 gen 2025 e
        // l'IR gli e' cambiato sotto senza che nessuno ripercorresse le sue ipotesi.
        //
        // 📊 Che non facesse nulla era gia' misurato: l'audit del 20 ago (job/tests/tools/opt_audit.sh)
        // ha spento un passo alla volta su 162 programmi, e spegnere CONST_PROP non cambiava UN BYTE.
        // Cio' che mancava era il PERCHE', e con l'except nudo di allora «non trova niente» e
        // «esplode alla prima istruzione» erano indistinguibili.
        //
        // L'unita' SedaiConstProp resta in albero con una nota in testa: per rianimarla servirebbe
        // riscriverla sui REGISTRI invece che sulle variabili, e a quel punto sarebbe un passo nuovo.

        {$IFNDEF DISABLE_COPY_PROP}
        try SSAProgram.RunCopyProp; except on E: Exception do OptPassFailed('CopyProp', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_LICM}
        try SSAProgram.RunLICM; except on E: Exception do OptPassFailed('LICM', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_LOOP_UNROLL}
        try
          SSAProgram.ClearDomTree;
          SSAProgram.BuildDominatorTree;
          SSAProgram.RunLoopUnrolling;
        except on E: Exception do OptPassFailed('LoopUnrolling', E); end;
        {$ENDIF}

        {$IFNDEF DISABLE_DCE}
        try SSAProgram.RunDCE; except on E: Exception do OptPassFailed('DCE', E); end;
        {$ENDIF}

        // B4 bounds-check elimination hints (after DCE, before PHI elimination)
        {$IFNDEF DISABLE_RANGE_ANALYSIS}
        try SSAProgram.RunRangeAnalysis; except on E: Exception do OptPassFailed('RangeAnalysis', E); end;
        {$ENDIF}

        // PHI Elimination
        {$IFNDEF DISABLE_PHI_ELIM}
        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        try
          SSAProgram.RunPhiElimination;
        except
          on E: Exception do
          begin
            FLastError := Format('PHI elimination error: %s', [E.Message]);
            raise ESedaiRunnerError.Create(FLastError);
          end;
        end;
        {$ENDIF}
        {$ENDIF}

        // Copy Coalescing
        {$IFNDEF DISABLE_COPY_COAL}
        try SSAProgram.RunCopyCoalescing; except on E: Exception do OptPassFailed('CopyCoalescing', E); end;
        {$ENDIF}

        // String temp fusion: let a string primitive write straight into its destination register.
        // Here, on the SSA, and not in the bytecode peephole -- the AOT compiles from THIS form, so
        // rewriting only the bytecode would leave the two describing different programs (it did, and
        // it miscompiled Str() under --aot). Before register allocation, while a temporary still has
        // exactly one definition and one use. STRFUSE=0 turns it off.
        if GetEnvironmentVariable('STRFUSE') <> '0' then
        begin
          try SSAProgram.RunStringTempFusion; except on E: Exception do OptPassFailed('StringTempFusion', E); end;
          try SSAProgram.RunAscMidFusion; except on E: Exception do OptPassFailed('AscMidFusion', E); end;
          try SSAProgram.RunStringTempFusion; except on E: Exception do OptPassFailed('StringTempFusion', E); end;
          try SSAProgram.RunConcatCharFusion; except on E: Exception do OptPassFailed('ConcatCharFusion', E); end;
          try SSAProgram.RunConcatDeadSourceMark; except on E: Exception do OptPassFailed('ConcatDeadSourceMark', E); end;
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
              FLastError := Format('Register allocation error: %s', [E.Message]);
              raise ESedaiRunnerError.Create(FLastError);
            end;
          end;
        finally
          RegAlloc.Free;
        end;
        {$ENDIF}

        // === BYTECODE COMPILATION ===
        Compiler := TBytecodeCompiler.Create;
        try
          try
            Result := Compiler.Compile(SSAProgram);

            if not Assigned(Result) then
            begin
              FLastError := 'Bytecode compilation failed';
              raise ESedaiRunnerError.Create(FLastError);
            end;
          except
            on E: ESedaiRunnerError do
              raise;
            on E: Exception do
            begin
              FLastError := Format('Bytecode compilation error: %s', [E.Message]);
              raise ESedaiRunnerError.Create(FLastError);
            end;
          end;

          // === BYTECODE OPTIMIZATIONS ===
          {$IFNDEF DISABLE_PEEPHOLE}
          try RunPeephole(Result); except on E: Exception do OptPassFailed('Peephole', E); end;
          {$ENDIF}

          {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
          // The engine gate lives inside RunSuperinstructions - four callers, one place.
          if not FSkipSuperinstructions then
            try RunSuperinstructionsAot(Result, SSAProgram); except on E: Exception do OptPassFailed('Superinstructions', E); end;
          {$ENDIF}

          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_NOP_COMPACTION}
          try RunNopCompaction(Result); except on E: Exception do OptPassFailed('NopCompaction', E); end;
          {$ENDIF}
          {$ENDIF}

          // Peephole pass 2
          // ⛔ IL SUO INTERRUTTORE NON ERA COLLEGATO (trovato il 21 ago 2026). Questo blocco era
          // protetto da DISABLE_PEEPHOLE - lo stesso flag della PRIMA passata - mentre
          // DISABLE_PEEPHOLE_PASS2 era l'unico dei 26 flag che NESSUN {$IFNDEF} consultava.
          // Conseguenza: l'audit che spegne un passo alla volta lo dava per «inerte, spegnerlo non
          // cambia un byte», il che era banalmente vero per un flag che non spegne niente. Un passo
          // inerte e un interruttore scollegato danno lo STESSO zero, e dai numeri non si distinguono.
          //
          // 📊 MISURATO per la prima volta col flag collegato (21 ago 2026): accesa contro spenta,
          // 158 programmi, ZERO differenze e ZERO righe. E' inerte davvero. Ma NON e' il caso di
          // CONST_PROP, che non puo' funzionare per costruzione: questa non trova nulla su QUESTO
          // corpus, il che e' una cosa piu' debole. Resta collegata e attiva; se un giorno il tempo
          // di compilazione conta, e' il primo passo da togliere - con una rimisura, non a memoria.
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_PEEPHOLE}
          {$IFNDEF DISABLE_PEEPHOLE_PASS2}
          try
            RunPeephole(Result);
            {$IFNDEF DISABLE_NOP_COMPACTION}
            RunNopCompaction(Result);
            {$ENDIF}
          except on E: Exception do OptPassFailed('Peephole2', E); end;
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}

          // Register Compaction
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_REG_COMPACTION}
          try RunRegisterCompaction(Result); except on E: Exception do OptPassFailed('RegisterCompaction', E); end;
          {$ENDIF}
          {$ENDIF}

          if FVerbose then
            WriteLn('Compiled ', Result.GetInstructionCount, ' instructions');

        finally
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

end.
