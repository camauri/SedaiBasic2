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

    function CompileSource(const SourceFile: string): TBytecodeProgram;
    function LoadBytecode(const BytecodeFile: string): TBytecodeProgram;
  public
    constructor Create;

    { Detect file type from extension }
    class function DetectFileType(const FileName: string): TSedaiFileType;

    { Load program from file (auto-detects type) }
    function Load(const FileName: string): TBytecodeProgram;

    { Load from specific type }
    function LoadFromSource(const SourceFile: string): TBytecodeProgram;
    function LoadFromBytecode(const BytecodeFile: string): TBytecodeProgram;

    { Properties }
    property Verbose: Boolean read FVerbose write FVerbose;
    property LastError: string read FLastError;
    property SkipSuperinstructions: Boolean read FSkipSuperinstructions write FSkipSuperinstructions;
  end;

  { Exception for runner errors }
  ESedaiRunnerError = class(Exception);

implementation

uses
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
  SedaiNopCompaction, SedaiRegisterCompaction;

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

    // === LEXING ===
    Lexer := TLexerFSM.Create;
    try
      Lexer.SetHasLineNumbers(True);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;

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
        try SSAProgram.RunDBE; except end;
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
        try SSAProgram.RunCopyCoalescing; except end;
        {$ENDIF}

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
          try RunPeephole(Result); except end;
          {$ENDIF}

          {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
          if not FSkipSuperinstructions then
            try RunSuperinstructions(Result); except end;
          {$ENDIF}

          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_NOP_COMPACTION}
          try RunNopCompaction(Result); except end;
          {$ENDIF}
          {$ENDIF}

          // Peephole pass 2
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_PEEPHOLE}
          try
            RunPeephole(Result);
            {$IFNDEF DISABLE_NOP_COMPACTION}
            RunNopCompaction(Result);
            {$ENDIF}
          except
          end;
          {$ENDIF}
          {$ENDIF}

          // Register Compaction
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_REG_COMPACTION}
          try RunRegisterCompaction(Result); except end;
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
