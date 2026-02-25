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
unit SedaiImmediateCompiler;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiImmediateCompiler - Lightweight compiler for immediate BASIC commands

  This unit provides a simplified compilation pipeline for immediate commands
  entered at the console prompt (e.g., PRINT 2+3, A=5, etc.).

  Unlike the full SedaiRunner pipeline, this compiler:
  - Skips heavy optimizations (SSA construction, LICM, loop unrolling, etc.)
  - Uses minimal optimizations suitable for single-line commands
  - Is designed for fast compilation of small code snippets

  Usage:
    var
      Compiler: TImmediateCompiler;
      Program_: TBytecodeProgram;
    begin
      Compiler := TImmediateCompiler.Create;
      try
        Program_ := Compiler.CompileStatement('PRINT 2+3');
        if Assigned(Program_) then
        begin
          // Execute with TBytecodeVM
          VM.LoadProgram(Program_);
          VM.Run;
          Program_.Free;
        end;
      finally
        Compiler.Free;
      end;
    end;
  ============================================================================ }

interface

uses
  Classes, SysUtils,
  SedaiBytecodeTypes, SedaiAST;

type
  { TImmediateCompiler - Lightweight compiler for immediate commands }
  TImmediateCompiler = class
  private
    FLastError: string;           // Brief error (for ?SYNTAX ERROR IN <line>)
    FLastErrorLine: Integer;      // BASIC line number where error occurred
    FLastErrorFileLine: Integer;  // Source file line number where error occurred
    FLastErrorColumn: Integer;    // Column where error occurred
    FLastErrorVerbose: string;    // Verbose error details (for OPTION VERBOSE)
    FVerbose: Boolean;
  public
    constructor Create;

    { Compile a single statement (e.g., "PRINT 2+3") }
    function CompileStatement(const Statement: string): TBytecodeProgram;

    { Compile from an already parsed AST }
    function CompileFromAST(AST: TASTNode): TBytecodeProgram;

    { Compile a full program from source text (multiple lines) }
    function CompileProgram(const Source: string): TBytecodeProgram;

    property LastError: string read FLastError;
    property LastErrorLine: Integer read FLastErrorLine;
    property LastErrorFileLine: Integer read FLastErrorFileLine;
    property LastErrorColumn: Integer read FLastErrorColumn;
    property LastErrorVerbose: string read FLastErrorVerbose;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

  { Exception for immediate compiler errors }
  EImmediateCompilerError = class(Exception);

implementation

uses
  SedaiLexerFSM, SedaiLexerTypes, SedaiTokenList,
  SedaiParserTypes, SedaiParserResults, SedaiParserErrors, SedaiPackratParser,
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeCompiler,
  SedaiRegAlloc,
  SedaiPeephole, SedaiNopCompaction;

{ ============================================================================
  TImmediateCompiler
  ============================================================================ }

constructor TImmediateCompiler.Create;
begin
  inherited Create;
  FLastError := '';
  FLastErrorLine := 0;
  FLastErrorColumn := 0;
  FLastErrorVerbose := '';
  FVerbose := False;
end;

procedure ClearErrorState(var LastError: string; var LastErrorLine, LastErrorColumn: Integer; var LastErrorVerbose: string);
begin
  LastError := '';
  LastErrorLine := 0;
  LastErrorColumn := 0;
  LastErrorVerbose := '';
end;

function TImmediateCompiler.CompileStatement(const Statement: string): TBytecodeProgram;
var
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  RegAlloc: TLinearScanAllocator;
  ParseError: TParserError;
begin
  Result := nil;
  ClearErrorState(FLastError, FLastErrorLine, FLastErrorColumn, FLastErrorVerbose);
  FLastErrorFileLine := 0;

  if Trim(Statement) = '' then
  begin
    FLastError := 'Empty statement';
    Exit;
  end;

  // === LEXING ===
  Lexer := TLexerFSM.Create;
  try
    Lexer.SetHasLineNumbers(False);  // Immediate commands don't have line numbers
    Lexer.SetRequireSpacesBetweenTokens(True);
    Lexer.SetCaseSensitive(False);
    Lexer.Source := Statement + #13#10;  // Ensure newline termination

    try
      TokenList := Lexer.ScanAllTokensFast;
      if not Assigned(TokenList) or (TokenList.Count = 0) then
      begin
        FLastError := 'SYNTAX ERROR';
        Exit;
      end;
    except
      on E: Exception do
      begin
        FLastError := 'SYNTAX ERROR';
        FLastErrorVerbose := 'Lexer error: ' + E.Message;
        Exit;
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
          begin
            ParseError := ParserResult.Errors[0];
            // Extract line/column for C128-style error format
            if ParseError.BasicLineNumber > 0 then
              FLastErrorLine := ParseError.BasicLineNumber
            else
              FLastErrorLine := ParseError.Line;
            FLastErrorFileLine := ParseError.Line;  // Always store file line
            FLastErrorColumn := ParseError.Column;
            FLastError := 'SYNTAX ERROR';
            // Use ToString for complete error with positions (line:col)
            FLastErrorVerbose := ParseError.ToString;
          end
          else
            FLastError := 'SYNTAX ERROR';
          ParserResult.Free;
          Exit;
        end;
      except
        on E: Exception do
        begin
          FLastError := 'SYNTAX ERROR';
          FLastErrorVerbose := E.Message;
          Exit;
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
          ParserResult.Free;
          Exit;
        end;
      except
        on E: Exception do
        begin
          FLastError := Format('SSA generation error: %s', [E.Message]);
          ParserResult.Free;
          Exit;
        end;
      end;

      // === MINIMAL SSA OPTIMIZATIONS ===
      // For immediate commands, we only apply essential optimizations
      try SSAProgram.RunDBE; except end;
      try SSAProgram.RunConstProp; except end;
      try SSAProgram.RunDCE; except end;

      // Register Allocation
      RegAlloc := TLinearScanAllocator.Create(SSAProgram);
      try
        try
          RegAlloc.Run;
        except
          on E: Exception do
          begin
            FLastError := Format('Register allocation error: %s', [E.Message]);
            ParserResult.Free;
            Exit;
          end;
        end;
      finally
        RegAlloc.Free;
      end;

      // === BYTECODE COMPILATION ===
      Compiler := TBytecodeCompiler.Create;
      try
        try
          Result := Compiler.Compile(SSAProgram);

          if not Assigned(Result) then
          begin
            FLastError := 'Bytecode compilation failed';
            ParserResult.Free;
            Exit;
          end;
        except
          on E: Exception do
          begin
            FLastError := Format('Bytecode compilation error: %s', [E.Message]);
            ParserResult.Free;
            Exit;
          end;
        end;

        // === MINIMAL BYTECODE OPTIMIZATIONS ===
        try RunPeephole(Result); except end;
        try RunNopCompaction(Result); except end;

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
end;

function TImmediateCompiler.CompileFromAST(AST: TASTNode): TBytecodeProgram;
var
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  RegAlloc: TLinearScanAllocator;
begin
  Result := nil;
  FLastError := '';

  if not Assigned(AST) then
  begin
    FLastError := 'AST is nil';
    Exit;
  end;

  // === SSA GENERATION ===
  SSAGen := TSSAGenerator.Create;
  try
    try
      SSAProgram := SSAGen.Generate(AST);

      if not Assigned(SSAProgram) then
      begin
        FLastError := 'SSA generation failed';
        Exit;
      end;
    except
      on E: Exception do
      begin
        FLastError := Format('SSA generation error: %s', [E.Message]);
        Exit;
      end;
    end;

    // === MINIMAL SSA OPTIMIZATIONS ===
    try SSAProgram.RunDBE; except end;
    try SSAProgram.RunConstProp; except end;
    try SSAProgram.RunDCE; except end;

    // Register Allocation
    RegAlloc := TLinearScanAllocator.Create(SSAProgram);
    try
      try
        RegAlloc.Run;
      except
        on E: Exception do
        begin
          FLastError := Format('Register allocation error: %s', [E.Message]);
          Exit;
        end;
      end;
    finally
      RegAlloc.Free;
    end;

    // === BYTECODE COMPILATION ===
    Compiler := TBytecodeCompiler.Create;
    try
      try
        Result := Compiler.Compile(SSAProgram);

        if not Assigned(Result) then
        begin
          FLastError := 'Bytecode compilation failed';
          Exit;
        end;
      except
        on E: Exception do
        begin
          FLastError := Format('Bytecode compilation error: %s', [E.Message]);
          Exit;
        end;
      end;

      // === MINIMAL BYTECODE OPTIMIZATIONS ===
      try RunPeephole(Result); except end;
      try RunNopCompaction(Result); except end;

    finally
      Compiler.Free;
    end;

  finally
    SSAProgram.Free;
    SSAGen.Free;
  end;
end;

function TImmediateCompiler.CompileProgram(const Source: string): TBytecodeProgram;
var
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  RegAlloc: TLinearScanAllocator;
  ParseError: TParserError;
begin
  Result := nil;
  ClearErrorState(FLastError, FLastErrorLine, FLastErrorColumn, FLastErrorVerbose);
  FLastErrorFileLine := 0;

  if Trim(Source) = '' then
  begin
    FLastError := 'SYNTAX ERROR';
    Exit;
  end;

  // === LEXING ===
  Lexer := TLexerFSM.Create;
  try
    Lexer.SetHasLineNumbers(True);  // Programs have line numbers
    Lexer.SetRequireSpacesBetweenTokens(True);
    Lexer.SetCaseSensitive(False);
    Lexer.Source := Source;

    try
      TokenList := Lexer.ScanAllTokensFast;
      if not Assigned(TokenList) or (TokenList.Count = 0) then
      begin
        FLastError := 'SYNTAX ERROR';
        Exit;
      end;

    except
      on E: Exception do
      begin
        FLastError := 'SYNTAX ERROR';
        FLastErrorVerbose := 'Lexer error: ' + E.Message;
        Exit;
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
          begin
            ParseError := ParserResult.Errors[0];
            // Extract line/column for C128-style error format
            if ParseError.BasicLineNumber > 0 then
              FLastErrorLine := ParseError.BasicLineNumber
            else
              FLastErrorLine := ParseError.Line;
            FLastErrorFileLine := ParseError.Line;  // Always store file line
            FLastErrorColumn := ParseError.Column;
            FLastError := 'SYNTAX ERROR';
            // Use ToString for complete error with positions (line:col)
            FLastErrorVerbose := ParseError.ToString;
          end
          else
            FLastError := 'SYNTAX ERROR';
          ParserResult.Free;
          Exit;
        end;
      except
        on E: Exception do
        begin
          FLastError := 'SYNTAX ERROR';
          FLastErrorVerbose := E.Message;
          Exit;
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
          ParserResult.Free;
          Exit;
        end;
      except
        on E: Exception do
        begin
          FLastError := Format('SSA generation error: %s', [E.Message]);
          ParserResult.Free;
          Exit;
        end;
      end;

      // === FULL SSA OPTIMIZATIONS for programs ===
      try SSAProgram.RunDBE; except end;
      try
        SSAProgram.BuildDominatorTree;
        SSAProgram.RunSSAConstruction;
      except end;
      try SSAProgram.RunConstProp; except end;
      try SSAProgram.RunCopyProp; except end;
      try SSAProgram.RunAlgebraic; except end;
      try SSAProgram.RunStrengthReduction; except end;
      try SSAProgram.RunDCE; except end;
      try SSAProgram.RunPhiElimination; except end;
      try SSAProgram.RunCopyCoalescing; except end;

      // Register Allocation
      RegAlloc := TLinearScanAllocator.Create(SSAProgram);
      try
        try
          RegAlloc.Run;
        except
          on E: Exception do
          begin
            FLastError := Format('Register allocation error: %s', [E.Message]);
            ParserResult.Free;
            Exit;
          end;
        end;
      finally
        RegAlloc.Free;
      end;

      // === BYTECODE COMPILATION ===
      Compiler := TBytecodeCompiler.Create;
      try
        try
          Result := Compiler.Compile(SSAProgram);

          if not Assigned(Result) then
          begin
            FLastError := 'Bytecode compilation failed';
            ParserResult.Free;
            Exit;
          end;
        except
          on E: Exception do
          begin
            FLastError := Format('Bytecode compilation error: %s', [E.Message]);
            ParserResult.Free;
            Exit;
          end;
        end;

        // === BYTECODE OPTIMIZATIONS ===
        try RunPeephole(Result); except end;
        try RunNopCompaction(Result); except end;

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
end;

end.
