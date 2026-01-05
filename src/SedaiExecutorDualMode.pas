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
unit SedaiExecutorDualMode;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Variants,
  SedaiAST, SedaiParserTypes,
  SedaiExecutor, SedaiExecutorContext, SedaiExecutionResult, SedaiOutputInterface,
  SedaiBytecodeTypes, SedaiBytecodeCompiler, SedaiBytecodeVM, SedaiBytecodeDisassembler;

type
  { TExecutionMode - Execution backend selection }
  TExecutionMode = (
    emInterpreter,    // Traditional AST tree-walking interpreter (default)
    emBytecodeVM,     // Bytecode virtual machine (5-7x faster)
    emJIT             // JIT compiler (future, 10-30x faster)
  );

  { TSedaiExecutorDualMode - Dual-mode executor supporting both interpretation and VM
    Extends existing interpreter with optional bytecode compilation and execution }
  TSedaiExecutorDualMode = class
  private
    // Execution mode
    FExecutionMode: TExecutionMode;

    // Backends
    FInterpreter: TSedaiExecutor;
    FBytecodeCompiler: TSedaiBytecodeCompiler;
    FBytecodeVM: TSedaiBytecodeVM;

    // Shared context
    FContext: TExecutorContext;
    FOutput: ISedaiOutputInterface;

    // Bytecode caching
    FCompiledProgram: TBytecodeProgram;
    FCachedAST: TASTNode;

    // Statistics
    FCompileTime: Double;
    FExecutionTime: Double;

    // Options
    FEnableBytecodeCache: Boolean;
    FDumpBytecode: Boolean;
    FBytecode DumpPath: string;

  public
    constructor Create(AContext: TExecutorContext; AOutput: ISedaiOutputInterface);
    destructor Destroy; override;

    // Main execution interface
    function Execute(AST: TASTNode): TExecutionResult;

    // Mode control
    procedure SetExecutionMode(Mode: TExecutionMode);
    function GetExecutionMode: TExecutionMode;

    // Bytecode operations
    function CompileToBytecode(AST: TASTNode): TBytecodeProgram;
    function DisassembleBytecode: string;
    procedure DumpBytecodeToFile(const FileName: string);

    // Statistics
    function GetCompileTime: Double;
    function GetExecutionTime: Double;
    function GetTotalTime: Double;
    function GetSpeedup: Double;  // vs interpreter baseline

    // Options
    property ExecutionMode: TExecutionMode read FExecutionMode write SetExecutionMode;
    property EnableBytecodeCache: Boolean read FEnableBytecodeCache write FEnableBytecodeCache;
    property DumpBytecode: Boolean read FDumpBytecode write FDumpBytecode;
    property BytecodeDumpPath: string read FBytecodeDumpPath write FBytecodeDumpPath;
  end;

implementation

uses
  DateUtils, SysUtils;

{ TSedaiExecutorDualMode }

constructor TSedaiExecutorDualMode.Create(AContext: TExecutorContext; AOutput: ISedaiOutputInterface);
begin
  inherited Create;

  FContext := AContext;
  FOutput := AOutput;

  // Default mode: Interpreter (safe, compatible)
  FExecutionMode := emInterpreter;

  // Create interpreter (always available as fallback)
  // Note: TSedaiExecutor constructor needs OutputDevice and InputDevice
  // We'll need to adapt this based on actual interface
  // For now, assume a simple wrapper
  FInterpreter := nil;  // TODO: Create with proper interfaces

  // Create bytecode components (lazy initialization)
  FBytecodeCompiler := nil;
  FBytecodeVM := nil;
  FCompiledProgram := nil;
  FCachedAST := nil;

  // Options
  FEnableBytecodeCache := True;
  FDumpBytecode := False;
  FBytecodeDumpPath := 'bytecode_dump.txt';

  // Statistics
  FCompileTime := 0;
  FExecutionTime := 0;
end;

destructor TSedaiExecutorDualMode.Destroy;
begin
  if Assigned(FCompiledProgram) then
    FCompiledProgram.Free;

  if Assigned(FBytecodeVM) then
    FBytecodeVM.Free;

  if Assigned(FBytecodeCompiler) then
    FBytecodeCompiler.Free;

  if Assigned(FInterpreter) then
    FInterpreter.Free;

  inherited Destroy;
end;

procedure TSedaiExecutorDualMode.SetExecutionMode(Mode: TExecutionMode);
begin
  FExecutionMode := Mode;

  // Invalidate cache when switching modes
  if FExecutionMode <> emBytecodeVM then
  begin
    if Assigned(FCompiledProgram) then
    begin
      FCompiledProgram.Free;
      FCompiledProgram := nil;
    end;
    FCachedAST := nil;
  end;
end;

function TSedaiExecutorDualMode.GetExecutionMode: TExecutionMode;
begin
  Result := FExecutionMode;
end;

function TSedaiExecutorDualMode.Execute(AST: TASTNode): TExecutionResult;
var
  StartTime, EndTime: TDateTime;
begin
  Result := TExecutionResult.Create;

  try
    case FExecutionMode of
      emInterpreter:
        begin
          // Use traditional interpreter
          StartTime := Now;
          if Assigned(FInterpreter) then
          begin
            Result := FInterpreter.Execute(AST);
            EndTime := Now;
            FExecutionTime := MilliSecondsBetween(EndTime, StartTime);
            FCompileTime := 0;
          end
          else
          begin
            Result.Success := False;
            Result.ErrorMessage := 'Interpreter not initialized';
          end;
        end;

      emBytecodeVM:
        begin
          // Use bytecode VM
          StartTime := Now;

          // Check cache
          if FEnableBytecodeCache and Assigned(FCompiledProgram) and (FCachedAST = AST) then
          begin
            // Cache hit - skip compilation
            FCompileTime := 0;
          end
          else
          begin
            // Cache miss - compile
            FCompiledProgram := CompileToBytecode(AST);
            if not Assigned(FCompiledProgram) then
            begin
              Result.Success := False;
              Result.ErrorMessage := 'Bytecode compilation failed';
              Exit;
            end;

            FCachedAST := AST;
            FCompileTime := MilliSecondsBetween(Now, StartTime);

            // Dump bytecode if requested
            if FDumpBytecode then
              DumpBytecodeToFile(FBytecodeDumpPath);
          end;

          // Execute bytecode
          StartTime := Now;
          try
            // Lazy create VM
            if not Assigned(FBytecodeVM) then
              FBytecodeVM := TSedaiBytecodeVM.Create(FContext, FOutput);

            FBytecodeVM.Execute(FCompiledProgram);

            EndTime := Now;
            FExecutionTime := MilliSecondsBetween(EndTime, StartTime);

            Result.Success := True;
            Result.StatementsExecuted := FBytecodeVM.InstructionsExecuted;
            Result.ExecutionTime := FExecutionTime;

          except
            on E: Exception do
            begin
              Result.Success := False;
              Result.ErrorMessage := E.Message;
            end;
          end;
        end;

      emJIT:
        begin
          // JIT not yet implemented - fallback to VM
          Result.Success := False;
          Result.ErrorMessage := 'JIT compiler not yet implemented';
        end;
    end;

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := 'Execution error: ' + E.Message;
    end;
  end;
end;

function TSedaiExecutorDualMode.CompileToBytecode(AST: TASTNode): TBytecodeProgram;
var
  StartTime, EndTime: TDateTime;
begin
  Result := nil;

  // Lazy create compiler
  if not Assigned(FBytecodeCompiler) then
    FBytecodeCompiler := TSedaiBytecodeCompiler.Create;

  StartTime := Now;

  try
    Result := FBytecodeCompiler.Compile(AST);

    EndTime := Now;
    FCompileTime := MilliSecondsBetween(EndTime, StartTime);

    // Check for errors
    if not Assigned(Result) then
    begin
      if FBytecodeCompiler.Errors.Count > 0 then
        WriteLn('Compilation errors:')
      else
        WriteLn(FBytecodeCompiler.Errors.Text);
    end;

  except
    on E: Exception do
    begin
      WriteLn('Compilation exception: ', E.Message);
      Result := nil;
    end;
  end;
end;

function TSedaiExecutorDualMode.DisassembleBytecode: string;
var
  Disasm: TSedaiBytecodeDisassembler;
begin
  if not Assigned(FCompiledProgram) then
  begin
    Result := 'No bytecode available. Compile first.';
    Exit;
  end;

  Disasm := TSedaiBytecodeDisassembler.Create;
  try
    Result := Disasm.Disassemble(FCompiledProgram);
  finally
    Disasm.Free;
  end;
end;

procedure TSedaiExecutorDualMode.DumpBytecodeToFile(const FileName: string);
var
  Disasm: TSedaiBytecodeDisassembler;
begin
  if not Assigned(FCompiledProgram) then
    Exit;

  Disasm := TSedaiBytecodeDisassembler.Create;
  try
    Disasm.DisassembleToFile(FCompiledProgram, FileName);
  finally
    Disasm.Free;
  end;
end;

function TSedaiExecutorDualMode.GetCompileTime: Double;
begin
  Result := FCompileTime;
end;

function TSedaiExecutorDualMode.GetExecutionTime: Double;
begin
  Result := FExecutionTime;
end;

function TSedaiExecutorDualMode.GetTotalTime: Double;
begin
  Result := FCompileTime + FExecutionTime;
end;

function TSedaiExecutorDualMode.GetSpeedup: Double;
begin
  // TODO: Calculate speedup vs baseline interpreter
  // This requires running both modes and comparing times
  Result := 1.0;
end;

end.
