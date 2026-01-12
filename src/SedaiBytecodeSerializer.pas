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
unit SedaiBytecodeSerializer;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiBytecodeSerializer - Serialize/Deserialize TBytecodeProgram to .basc

  File Format (.basc):
  ┌────────────────────────────────┐
  │ Header (32 bytes)              │
  │  - Magic: "BASC" (4 bytes)     │
  │  - Version: u16                │
  │  - Flags: u16                  │
  │  - InstructionCount: u32       │
  │  - StringConstCount: u32       │
  │  - VariableCount: u32          │
  │  - ArrayCount: u32             │
  │  - Reserved: u32               │
  │  - Reserved: u32               │
  │  - Checksum: u32               │
  ├────────────────────────────────┤
  │ String Constants Table         │
  │  - For each: Length(u32) + UTF8│
  ├────────────────────────────────┤
  │ Variables Table                │
  │  - For each: TVariableInfo     │
  ├────────────────────────────────┤
  │ Bytecode Instructions          │
  │  - Packed TBytecodeInstruction │
  └────────────────────────────────┘
  ============================================================================ }

interface

uses
  Classes, SysUtils, Math, SedaiBytecodeTypes, SedaiSSATypes;

const
  BASC_MAGIC: array[0..3] of AnsiChar = 'BASC';
  BASC_VERSION = 1;

  // Flags
  BASC_FLAG_DEBUG_INFO = $0001;  // Contains source line mapping (always included)

type
  { TBascHeader - File header structure }
  TBascHeader = packed record
    Magic: array[0..3] of AnsiChar;
    Version: Word;
    Flags: Word;
    InstructionCount: LongWord;
    StringConstCount: LongWord;
    VariableCount: LongWord;
    ArrayCount: LongWord;
    Reserved1: LongWord;
    Reserved2: LongWord;
    Checksum: LongWord;
  end;

  { Pointer to bytecode instruction for checksum calculation }
  PBytecodeInstruction = ^TBytecodeInstruction;

  { TBytecodeSerializer }
  TBytecodeSerializer = class
  private
    FIncludeDebugInfo: Boolean;

    procedure WriteString(Stream: TStream; const S: string);
    function ReadString(Stream: TStream): string;
    function CalculateChecksum(Program_: TBytecodeProgram): LongWord;
  public
    constructor Create;

    { Save bytecode program to file }
    procedure SaveToFile(Program_: TBytecodeProgram; const FileName: string);
    procedure SaveToStream(Program_: TBytecodeProgram; Stream: TStream);

    { Load bytecode program from file }
    function LoadFromFile(const FileName: string): TBytecodeProgram;
    function LoadFromStream(Stream: TStream): TBytecodeProgram;

    { Options }
    property IncludeDebugInfo: Boolean read FIncludeDebugInfo write FIncludeDebugInfo;
  end;

  { Exception for serialization errors }
  EBytecodeSerializerError = class(Exception);

implementation

{ ============================================================================
  TBytecodeSerializer
  ============================================================================ }

constructor TBytecodeSerializer.Create;
begin
  inherited Create;
  FIncludeDebugInfo := True;  // Include source line info by default
end;

procedure TBytecodeSerializer.WriteString(Stream: TStream; const S: string);
var
  Len: LongWord;
  UTF8Str: UTF8String;
begin
  UTF8Str := UTF8Encode(S);
  Len := Length(UTF8Str);
  Stream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    Stream.WriteBuffer(UTF8Str[1], Len);
end;

function TBytecodeSerializer.ReadString(Stream: TStream): string;
var
  Len: LongWord;
  UTF8Str: UTF8String;
begin
  Stream.ReadBuffer(Len, SizeOf(Len));
  if Len > 0 then
  begin
    SetLength(UTF8Str, Len);
    Stream.ReadBuffer(UTF8Str[1], Len);
    Result := UTF8Decode(UTF8Str);
  end
  else
    Result := '';
end;

function TBytecodeSerializer.CalculateChecksum(Program_: TBytecodeProgram): LongWord;
var
  InstrCount, StrCount, VarCount, ArrCount: Integer;
begin
  // Simple checksum based on counts only - avoid instruction iteration
  // which can trigger access violations during certain compiler optimizations
  if Assigned(Program_.StringConstants) then
    StrCount := Program_.StringConstants.Count
  else
    StrCount := 0;

  InstrCount := Program_.GetInstructionCount;
  VarCount := Program_.GetVariableCount;
  ArrCount := Program_.GetArrayCount;

  // Simple checksum: combine counts with different shifts
  Result := LongWord(InstrCount);
  Result := Result xor (LongWord(StrCount) shl 8);
  Result := Result xor (LongWord(VarCount) shl 16);
  Result := Result xor (LongWord(ArrCount) shl 24);
  // Add a simple hash mixing
  Result := Result xor (Result shr 11);
  Result := Result * 2654435769;  // Golden ratio constant
  Result := Result xor (Result shr 16);
end;

procedure TBytecodeSerializer.SaveToStream(Program_: TBytecodeProgram; Stream: TStream);
var
  Header: TBascHeader;
  i, j: Integer;
  Instr: TBytecodeInstruction;
  SourceLine: Integer;  // Retrieved from Source Map
  VarInfo: TVariableInfo;
  ArrInfo: TSSAArrayInfo;
  RegType: Byte;
  IsArray: Byte;
  DimCount: LongWord;
  DimSize: LongWord;
begin
  // Validate program
  if not Assigned(Program_) then
    raise EBytecodeSerializerError.Create('Program is nil');

  // Prepare header
  FillChar(Header, SizeOf(Header), 0);
  Move(BASC_MAGIC[0], Header.Magic[0], 4);
  Header.Version := BASC_VERSION;
  Header.Flags := BASC_FLAG_DEBUG_INFO;  // Source line info is always included in instructions

  Header.InstructionCount := Program_.GetInstructionCount;
  if Assigned(Program_.StringConstants) then
    Header.StringConstCount := Program_.StringConstants.Count
  else
    Header.StringConstCount := 0;
  Header.VariableCount := Program_.GetVariableCount;
  Header.ArrayCount := Program_.GetArrayCount;
  Header.Reserved1 := 0;
  Header.Reserved2 := 0;

  Header.Checksum := CalculateChecksum(Program_);

  // Write header
  Stream.WriteBuffer(Header, SizeOf(Header));

  // Write string constants
  if Assigned(Program_.StringConstants) then
    for i := 0 to Program_.StringConstants.Count - 1 do
      WriteString(Stream, Program_.StringConstants[i]);

  // Write variables (name + metadata)
  for i := 0 to Program_.GetVariableCount - 1 do
  begin
    try
      VarInfo := Program_.GetVariable(i);
      WriteString(Stream, VarInfo.Name);
      RegType := VarInfo.RegType;
      Stream.WriteBuffer(RegType, SizeOf(RegType));
      IsArray := Ord(VarInfo.IsArray);
      Stream.WriteBuffer(IsArray, SizeOf(IsArray));
      Stream.WriteBuffer(VarInfo.ArraySize, SizeOf(VarInfo.ArraySize));
    except
      on E: Exception do
        raise EBytecodeSerializerError.CreateFmt('Error writing variable %d: %s', [i, E.Message]);
    end;
  end;

  // Write arrays (TSSAArrayInfo)
  for i := 0 to Program_.GetArrayCount - 1 do
  begin
    try
      ArrInfo := Program_.GetArray(i);
      WriteString(Stream, ArrInfo.Name);
      RegType := Byte(ArrInfo.ElementType);
      Stream.WriteBuffer(RegType, SizeOf(RegType));
      // Use actual array length, not DimCount (which may be inconsistent)
      DimCount := Length(ArrInfo.Dimensions);
      Stream.WriteBuffer(DimCount, SizeOf(DimCount));
      // Write dimension sizes
      for j := 0 to DimCount - 1 do
      begin
        DimSize := ArrInfo.Dimensions[j];
        Stream.WriteBuffer(DimSize, SizeOf(DimSize));
      end;
      Stream.WriteBuffer(ArrInfo.ArrayIndex, SizeOf(ArrInfo.ArrayIndex));
    except
      on E: Exception do
        raise EBytecodeSerializerError.CreateFmt('Error writing array %d: %s', [i, E.Message]);
    end;
  end;

  // Write instructions (packed format with source line from Source Map)
  for i := 0 to Program_.GetInstructionCount - 1 do
  begin
    try
      Instr := Program_.GetInstruction(i);
      SourceLine := Program_.GetSourceLine(i);  // Get from Source Map
      // Write each field separately for portability
      Stream.WriteBuffer(Instr.OpCode, SizeOf(Instr.OpCode));
      Stream.WriteBuffer(Instr.Dest, SizeOf(Instr.Dest));
      Stream.WriteBuffer(Instr.Src1, SizeOf(Instr.Src1));
      Stream.WriteBuffer(Instr.Src2, SizeOf(Instr.Src2));
      Stream.WriteBuffer(Instr.Immediate, SizeOf(Instr.Immediate));
      Stream.WriteBuffer(SourceLine, SizeOf(SourceLine));  // Write source line from map
    except
      on E: Exception do
        raise EBytecodeSerializerError.CreateFmt('Error writing instruction %d: %s', [i, E.Message]);
    end;
  end;
end;

procedure TBytecodeSerializer.SaveToFile(Program_: TBytecodeProgram; const FileName: string);
var
  Stream: TFileStream;
begin
  // Validate inputs
  if not Assigned(Program_) then
    raise EBytecodeSerializerError.Create('Cannot save: Program is nil');

  if FileName = '' then
    raise EBytecodeSerializerError.Create('Cannot save: filename is empty');

  // Create directory if it doesn't exist
  if ExtractFilePath(FileName) <> '' then
    ForceDirectories(ExtractFilePath(FileName));

  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Program_, Stream);
  finally
    Stream.Free;
  end;
end;

function TBytecodeSerializer.LoadFromStream(Stream: TStream): TBytecodeProgram;
var
  Header: TBascHeader;
  i, j: Integer;
  Instr: TBytecodeInstruction;
  SourceLine: Integer;  // Read from file, stored in Source Map
  VarInfo: TVariableInfo;
  ArrInfo: TSSAArrayInfo;
  StrConst: string;
  Checksum: LongWord;
  RegType, IsArray: Byte;
  DimCount, DimSize: LongWord;
begin
  Result := TBytecodeProgram.Create;
  try
    // Read header
    Stream.ReadBuffer(Header, SizeOf(Header));

    // Validate magic
    if (Header.Magic[0] <> 'B') or (Header.Magic[1] <> 'A') or
       (Header.Magic[2] <> 'S') or (Header.Magic[3] <> 'C') then
      raise EBytecodeSerializerError.Create('Invalid .basc file: wrong magic number');

    // Validate version
    if Header.Version > BASC_VERSION then
      raise EBytecodeSerializerError.CreateFmt(
        'Unsupported .basc version: %d (max supported: %d)',
        [Header.Version, BASC_VERSION]);

    // Read string constants
    for i := 0 to Header.StringConstCount - 1 do
    begin
      StrConst := ReadString(Stream);
      Result.StringConstants.Add(StrConst);
    end;

    // Read variables
    for i := 0 to Header.VariableCount - 1 do
    begin
      VarInfo.Name := ReadString(Stream);
      Stream.ReadBuffer(RegType, SizeOf(RegType));
      VarInfo.RegType := RegType;
      Stream.ReadBuffer(IsArray, SizeOf(IsArray));
      VarInfo.IsArray := IsArray <> 0;
      Stream.ReadBuffer(VarInfo.ArraySize, SizeOf(VarInfo.ArraySize));
      Result.AddVariable(VarInfo);
    end;

    // Read arrays
    for i := 0 to Header.ArrayCount - 1 do
    begin
      FillChar(ArrInfo, SizeOf(ArrInfo), 0);
      ArrInfo.Name := ReadString(Stream);
      Stream.ReadBuffer(RegType, SizeOf(RegType));
      ArrInfo.ElementType := TSSARegisterType(RegType);
      Stream.ReadBuffer(DimCount, SizeOf(DimCount));
      ArrInfo.DimCount := DimCount;
      SetLength(ArrInfo.Dimensions, DimCount);
      for j := 0 to DimCount - 1 do
      begin
        Stream.ReadBuffer(DimSize, SizeOf(DimSize));
        ArrInfo.Dimensions[j] := DimSize;
      end;
      Stream.ReadBuffer(ArrInfo.ArrayIndex, SizeOf(ArrInfo.ArrayIndex));
      Result.AddArrayInfo(ArrInfo);
    end;

    // Read instructions (SourceLine is now stored in Source Map, not in instruction)
    for i := 0 to Header.InstructionCount - 1 do
    begin
      FillChar(Instr, SizeOf(Instr), 0);
      Stream.ReadBuffer(Instr.OpCode, SizeOf(Instr.OpCode));
      Stream.ReadBuffer(Instr.Dest, SizeOf(Instr.Dest));
      Stream.ReadBuffer(Instr.Src1, SizeOf(Instr.Src1));
      Stream.ReadBuffer(Instr.Src2, SizeOf(Instr.Src2));
      Stream.ReadBuffer(Instr.Immediate, SizeOf(Instr.Immediate));
      Stream.ReadBuffer(SourceLine, SizeOf(SourceLine));  // Read from file
      Result.AddInstructionWithLine(Instr, SourceLine);   // Store in Source Map
    end;

    // Validate checksum
    Checksum := CalculateChecksum(Result);
    if Checksum <> Header.Checksum then
      raise EBytecodeSerializerError.CreateFmt(
        'Checksum mismatch: file may be corrupted (expected: %x, got: %x)',
        [Header.Checksum, Checksum]);

  except
    Result.Free;
    raise;
  end;
end;

function TBytecodeSerializer.LoadFromFile(const FileName: string): TBytecodeProgram;
var
  Stream: TFileStream;
begin
  if not FileExists(FileName) then
    raise EBytecodeSerializerError.CreateFmt('File not found: %s', [FileName]);

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
