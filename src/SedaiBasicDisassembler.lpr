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
program SedaiBasicDisassembler;

{$mode objfpc}{$H+}
{$codepage UTF8}

{ ============================================================================
  SedaiBasicDisassembler (sbd) - Disassemble .basc bytecode files

  Usage: sbd <program.basc> [options]

  Reads a compiled .basc bytecode file and displays human-readable
  disassembly output.

  Options:
    --help, -h       Show help message
    --verbose, -v    Show additional details (header info, variables, arrays)
    --raw, -r        Show raw instruction bytes
    --no-header      Skip header information
    --no-strings     Skip string constants table
    --no-vars        Skip variables table
    --no-arrays      Skip arrays table
    --output, -o     Output to file instead of stdout

  Exit codes:
    0 = Success
    1 = Error (file not found, invalid format, etc.)
  ============================================================================ }

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, fgl,
  SedaiBytecodeTypes, SedaiBytecodeSerializer, SedaiSSATypes;

// Include version information
{$I Version.inc}

type
  TDisasmOptions = record
    Verbose: Boolean;
    ShowRaw: Boolean;
    ShowHeader: Boolean;
    ShowStrings: Boolean;
    ShowVars: Boolean;
    ShowArrays: Boolean;
    OutputFile: string;
  end;

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
  WriteLn('SedaiBasic Disassembler (sbd) ver. ', SEDAIBASIC_VERSION, ' [', SEDAIBASIC_RELEASE_DATE, '] for ', GetSystemArchitecture);
  WriteLn(SEDAIBASIC_COPYRIGHT);
end;

{ Print help information }
procedure PrintHelp;
begin
  PrintVersion;
  WriteLn;
  WriteLn('Usage: sbd <program.basc> [options]');
  WriteLn;
  WriteLn('Arguments:');
  WriteLn('  program.basc    Compiled bytecode file to disassemble');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --help, -h      Show this help message');
  WriteLn('  --verbose, -v   Show additional details (header, checksums)');
  WriteLn('  --raw, -r       Show raw instruction bytes (hex dump)');
  WriteLn('  --no-header     Skip file header information');
  WriteLn('  --no-strings    Skip string constants table');
  WriteLn('  --no-vars       Skip variables table');
  WriteLn('  --no-arrays     Skip arrays table');
  WriteLn('  -o <file>       Output to file instead of stdout');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  sbd program.basc              Disassemble to stdout');
  WriteLn('  sbd program.basc -v           Verbose disassembly');
  WriteLn('  sbd program.basc -o out.txt   Disassemble to file');
  WriteLn('  sbd program.basc --raw        Show raw bytes');
end;

{ Get register type name }
function RegTypeToString(RegType: Byte): string;
begin
  case RegType of
    0: Result := 'Int';
    1: Result := 'Float';
    2: Result := 'String';
  else
    Result := Format('Unknown(%d)', [RegType]);
  end;
end;

{ Get SSA register type name }
function SSARegTypeToString(RegType: TSSARegisterType): string;
begin
  case RegType of
    srtInt: Result := 'Int';
    srtFloat: Result := 'Float';
    srtString: Result := 'String';
  else
    Result := 'Unknown';
  end;
end;

{ Format immediate value based on opcode context }
function FormatImmediate(OpCode: Byte; Immediate: Int64): string;
begin
  // For float constants, decode as Double
  if OpCode = Ord(bcLoadConstFloat) then
    Result := Format('%f', [Double(Pointer(@Immediate)^)])
  // For superinstruction float consts (150-153)
  else if OpCode in [150, 151, 152, 153, 181] then
    Result := Format('%f', [Double(Pointer(@Immediate)^)])
  else
    Result := IntToStr(Immediate);
end;

{ Format operands based on instruction opcode }
function FormatOperands(const Instr: TBytecodeInstruction; Prog: TBytecodeProgram): string;
var
  Op: TBytecodeOp;
begin
  Result := '';

  // Handle superinstructions (opcodes 100+) separately
  if Instr.OpCode >= 100 then
  begin
    case Instr.OpCode of
      // Fused compare-and-branch (Int) - opcodes 100-105
      100..105: Result := Format('R%d, R%d, %d', [Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused compare-and-branch (Float) - opcodes 110-115
      110..115: Result := Format('R%d, R%d, %d', [Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused arithmetic-to-dest (Int) - opcodes 120-122
      120..122: Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);
      // Fused arithmetic-to-dest (Float) - opcodes 130-133
      130..133: Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);
      // Fused constant arithmetic (Int) - opcodes 140-142
      140..142: Result := Format('R%d, R%d, %d', [Instr.Dest, Instr.Src1, Instr.Immediate]);
      // Fused constant arithmetic (Float) - opcodes 150-153
      150..153: Result := Format('R%d, R%d, %f', [Instr.Dest, Instr.Src1, Double(Pointer(@Instr.Immediate)^)]);
      // Fused compare-zero-and-branch (Int) - opcodes 160-161
      160, 161: Result := Format('R%d, %d', [Instr.Src1, Instr.Immediate]);
      // Fused compare-zero-and-branch (Float) - opcodes 170-171
      170, 171: Result := Format('R%d, %d', [Instr.Src1, Instr.Immediate]);
      // Fused array-store-constant - opcodes 180-182
      180: Result := Format('ARR[%d], idx=R%d, %d', [Instr.Src1, Instr.Src2, Instr.Immediate]);
      181: Result := Format('ARR[%d], idx=R%d, %f', [Instr.Src1, Instr.Src2, Double(Pointer(@Instr.Immediate)^)]);
      182: Result := Format('ARR[%d], idx=R%d, [%d]', [Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Fused loop increment-and-branch - opcodes 190-193
      190..193: Result := Format('R%d, R%d, R%d, %d', [Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      // FMA - opcodes 200-203
      200, 201: Result := Format('R%d, R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      202, 203: Result := Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
      // Array Load + Arithmetic - opcodes 210-212
      210, 211: Result := Format('R%d, ARR[%d], idx=R%d, acc=R%d', [Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      212: Result := Format('R%d, ARR[%d], idx=R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
      // Square-Sum patterns - opcodes 220-221
      220, 221: Result := Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
      // Mul-Mul and Add-Sqrt - opcodes 230-231
      230: Result := Format('R%d, R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2, Instr.Immediate]);
      231: Result := Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
      // Array Load + Branch - opcodes 240-241
      240, 241: Result := Format('ARR[%d], idx=R%d, %d', [Instr.Src1, Instr.Src2, Instr.Immediate]);
      // Array Swap (Int) - opcode 250
      250: Result := Format('ARR[%d], idx1=R%d, idx2=R%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
      // Self-increment/decrement (Int) - opcodes 251-252
      251, 252: Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);
      // Array Load to register (Int) - opcode 253
      253: Result := Format('R%d, ARR[%d], idx=R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
      // Array Copy/Move Element - opcodes 254-255
      254: Result := Format('ARR[%d][R%d] = ARR[%d][R%d]', [Instr.Dest, Instr.Src2, Instr.Src1, Instr.Src2]);
      255: Result := Format('ARR[%d][R%d] = ARR[%d][R%d]', [Instr.Dest, Instr.Src2, Instr.Dest, Instr.Src1]);
      // Array Reverse Range - opcode 156
      156: Result := Format('ARR[%d], R%d..R%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
      // Array Shift Left - opcode 157
      157: Result := Format('ARR[%d], R%d..R%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
    else
      Result := Format('(unknown opcode %d)', [Instr.OpCode]);
    end;
    Exit;
  end;

  Op := TBytecodeOp(Instr.OpCode);

  case Op of
    bcLoadConstInt:
      Result := Format('R%d, %d', [Instr.Dest, Instr.Immediate]);

    bcLoadConstFloat:
      Result := Format('R%d, %f', [Instr.Dest, Double(Pointer(@Instr.Immediate)^)]);

    bcLoadConstString:
      begin
        Result := Format('R%d, [%d]', [Instr.Dest, Instr.Immediate]);
        // Optionally show string value
        if Assigned(Prog) and Assigned(Prog.StringConstants) and
           (Instr.Immediate >= 0) and (Instr.Immediate < Prog.StringConstants.Count) then
          Result := Result + Format(' "%s"', [Prog.StringConstants[Instr.Immediate]]);
      end;

    bcCopyInt, bcCopyFloat, bcCopyString:
      Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcPowFloat:
      Result := Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    bcNegInt, bcNegFloat:
      Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcIntToFloat, bcFloatToInt:
      Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    // Comparison operators
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
      Result := Format('R%d, R%d, R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    // Math functions
    bcMathAbs, bcMathSgn, bcMathInt, bcMathSqr,
    bcMathSin, bcMathCos, bcMathTan, bcMathExp, bcMathLog:
      Result := Format('R%d, R%d', [Instr.Dest, Instr.Src1]);

    bcMathRnd:
      Result := Format('R%d', [Instr.Dest]);

    bcJump, bcCall:
      Result := Format('%d', [Instr.Immediate]);

    bcJumpIfZero, bcJumpIfNotZero:
      Result := Format('R%d, %d', [Instr.Src1, Instr.Immediate]);

    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcPrintInt, bcPrintIntLn:
      Result := Format('R%d', [Instr.Src1]);

    bcPrintComma, bcPrintSemicolon, bcPrintNewLine:
      Result := '';  // No operands

    bcPrintTab, bcPrintSpc:
      Result := Format('%d', [Instr.Immediate]);

    bcInput, bcInputInt, bcInputFloat, bcInputString:
      Result := Format('R%d', [Instr.Dest]);

    // Array operations
    bcArrayDim:
      Result := Format('ARR[%d]', [Instr.Src1]);

    bcArrayLoad:
      Result := Format('R%d, ARR[%d], idx=R%d', [Instr.Dest, Instr.Src1, Instr.Src2]);

    bcArrayStore:
      Result := Format('ARR[%d], idx=R%d, value=R%d', [Instr.Src1, Instr.Src2, Instr.Dest]);

    // Typed array operations
    bcArrayLoadInt:
      Result := Format('IntR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadFloat:
      Result := Format('FloatR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayLoadString:
      Result := Format('StrR%d, ARR[%d], idx=IntR%d', [Instr.Dest, Instr.Src1, Instr.Src2]);
    bcArrayStoreInt:
      Result := Format('ARR[%d], idx=IntR%d, value=IntR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreFloat:
      Result := Format('ARR[%d], idx=IntR%d, value=FloatR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);
    bcArrayStoreString:
      Result := Format('ARR[%d], idx=IntR%d, value=StrR%d', [Instr.Src1, Instr.Src2, Instr.Dest]);

    bcEnd, bcStop, bcReturn, bcNop:
      Result := '';  // No operands
  end;
end;

{ Disassemble bytecode file }
function DisassembleFile(const InputFile: string; const Options: TDisasmOptions): Boolean;
var
  Serializer: TBytecodeSerializer;
  BytecodeProgram: TBytecodeProgram;
  i, j: Integer;
  VarInfo: TVariableInfo;
  ArrInfo: TSSAArrayInfo;
  Instr: TBytecodeInstruction;
  OutFile: TextFile;
  UseFile: Boolean;
  Line: string;

  procedure Output(const S: string);
  begin
    if UseFile then
      WriteLn(OutFile, S)
    else
      WriteLn(S);
  end;

begin
  Result := False;
  UseFile := Options.OutputFile <> '';

  // Load bytecode
  Serializer := TBytecodeSerializer.Create;
  try
    try
      BytecodeProgram := Serializer.LoadFromFile(InputFile);
    except
      on E: Exception do
      begin
        WriteLn('ERROR: ', E.Message);
        Exit;
      end;
    end;

    try
      // Open output file if specified
      if UseFile then
      begin
        AssignFile(OutFile, Options.OutputFile);
        Rewrite(OutFile);
      end;

      try
        // === HEADER ===
        if Options.ShowHeader then
        begin
          Output('=== BYTECODE FILE INFO ===');
          Output(Format('File:              %s', [InputFile]));
          Output(Format('Instructions:      %d', [BytecodeProgram.GetInstructionCount]));
          Output(Format('Variables:         %d', [BytecodeProgram.GetVariableCount]));
          Output(Format('String Constants:  %d', [BytecodeProgram.StringConstants.Count]));
          Output(Format('Arrays:            %d', [BytecodeProgram.GetArrayCount]));
          if Options.Verbose then
            Output(Format('Entry Point:       %d', [BytecodeProgram.EntryPoint]));
          Output('');
        end;

        // === STRING CONSTANTS ===
        if Options.ShowStrings and (BytecodeProgram.StringConstants.Count > 0) then
        begin
          Output('=== STRING CONSTANTS ===');
          for i := 0 to BytecodeProgram.StringConstants.Count - 1 do
            Output(Format('[%3d] "%s"', [i, BytecodeProgram.StringConstants[i]]));
          Output('');
        end;

        // === VARIABLES ===
        if Options.ShowVars and (BytecodeProgram.GetVariableCount > 0) then
        begin
          Output('=== VARIABLES ===');
          for i := 0 to BytecodeProgram.GetVariableCount - 1 do
          begin
            VarInfo := BytecodeProgram.GetVariable(i);
            Line := Format('[%3d] %-20s Type=%-6s', [i, VarInfo.Name, RegTypeToString(VarInfo.RegType)]);
            if VarInfo.IsArray then
              Line := Line + Format(' Array[%d]', [VarInfo.ArraySize]);
            Output(Line);
          end;
          Output('');
        end;

        // === ARRAYS ===
        if Options.ShowArrays and (BytecodeProgram.GetArrayCount > 0) then
        begin
          Output('=== ARRAYS ===');
          for i := 0 to BytecodeProgram.GetArrayCount - 1 do
          begin
            ArrInfo := BytecodeProgram.GetArray(i);
            Line := Format('[%3d] %-20s Type=%-6s Dims=%d',
              [i, ArrInfo.Name, SSARegTypeToString(ArrInfo.ElementType), ArrInfo.DimCount]);
            if (ArrInfo.DimCount > 0) and (Length(ArrInfo.Dimensions) > 0) then
            begin
              Line := Line + ' (';
              for j := 0 to ArrInfo.DimCount - 1 do
              begin
                if j >= Length(ArrInfo.Dimensions) then
                  Break;
                if j > 0 then
                  Line := Line + ', ';
                Line := Line + IntToStr(ArrInfo.Dimensions[j]);
              end;
              Line := Line + ')';
            end;
            if Options.Verbose then
              Line := Line + Format(' ArrayIndex=%d', [ArrInfo.ArrayIndex]);
            Output(Line);
          end;
          Output('');
        end;

        // === INSTRUCTIONS ===
        Output('=== BYTECODE INSTRUCTIONS ===');

        if Options.ShowRaw then
        begin
          // Show raw format with hex bytes
          Output('  Addr  OpCode  Dest  Src1  Src2  Immediate          SourceLine  Disassembly');
          Output('  ----  ------  ----  ----  ----  -----------------  ----------  -----------');
        end;

        // Disassemble instructions directly (safer than using TBytecodeDisassembler)
        for i := 0 to BytecodeProgram.GetInstructionCount - 1 do
        begin
          Instr := BytecodeProgram.GetInstruction(i);
          if Options.ShowRaw then
          begin
            Line := Format('%6d    %3d  %4d  %4d  %4d  %17s  %10d  %s',
              [i,
               Instr.OpCode,
               Instr.Dest,
               Instr.Src1,
               Instr.Src2,
               FormatImmediate(Instr.OpCode, Instr.Immediate),
               BytecodeProgram.GetSourceLine(i),  // SourceLine now from Source Map
               OpcodeToString(Instr.OpCode)]);
          end
          else
          begin
            Line := Format('%4d: %-20s', [i, OpcodeToString(Instr.OpCode)]);
            // Add operands based on opcode
            Line := Line + FormatOperands(Instr, BytecodeProgram);
          end;
          Output(Line);
        end;

        Output('');
        Output(Format('=== END (Total: %d instructions) ===', [BytecodeProgram.GetInstructionCount]));

        Result := True;

      finally
        if UseFile then
          CloseFile(OutFile);
      end;

    finally
      BytecodeProgram.Free;
    end;

  finally
    Serializer.Free;
  end;
end;

var
  InputFile: string;
  Options: TDisasmOptions;
  OptHelp: Boolean;
  i: Integer;
  Param: string;

begin
  try
    // Set console code page to UTF-8
    {$IFDEF WINDOWS}
    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);
    {$ENDIF}

    // Initialize options with defaults
    FillChar(Options, SizeOf(Options), 0);
    Options.ShowHeader := True;
    Options.ShowStrings := True;
    Options.ShowVars := True;
    Options.ShowArrays := True;

    // Parse command-line parameters
    InputFile := '';
    OptHelp := False;

    i := 1;
    while i <= ParamCount do
    begin
      Param := ParamStr(i);
      if (Param = '--help') or (Param = '-h') or (Param = '-?') then
        OptHelp := True
      else if (Param = '--verbose') or (Param = '-v') then
        Options.Verbose := True
      else if (Param = '--raw') or (Param = '-r') then
        Options.ShowRaw := True
      else if Param = '--no-header' then
        Options.ShowHeader := False
      else if Param = '--no-strings' then
        Options.ShowStrings := False
      else if Param = '--no-vars' then
        Options.ShowVars := False
      else if Param = '--no-arrays' then
        Options.ShowArrays := False
      else if (Param = '-o') or (Param = '--output') then
      begin
        Inc(i);
        if i <= ParamCount then
          Options.OutputFile := ParamStr(i)
        else
        begin
          WriteLn('ERROR: -o requires a filename');
          ExitCode := 1;
          Exit;
        end;
      end
      else if (Pos('-', Param) <> 1) and (InputFile = '') then
        InputFile := Param;

      Inc(i);
    end;

    // Show help if requested or no file provided
    if OptHelp or (InputFile = '') then
    begin
      PrintHelp;
      if InputFile = '' then
        ExitCode := 1;
      Exit;
    end;

    // Check if file exists
    if not FileExists(InputFile) then
    begin
      WriteLn('ERROR: File not found: ', InputFile);
      ExitCode := 1;
      Exit;
    end;

    // Check extension
    if LowerCase(ExtractFileExt(InputFile)) <> '.basc' then
      WriteLn('WARNING: File does not have .basc extension');

    // Disassemble
    if DisassembleFile(InputFile, Options) then
      ExitCode := 0
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
