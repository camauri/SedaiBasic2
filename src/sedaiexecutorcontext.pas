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
unit SedaiExecutorContext;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Variants, fgl, Contnrs, SedaiExecutorTypes, SedaiExceptionHelpers,
  SedaiArenaAllocator;  // SESSION 14.5: Fast arena allocation for arrays

type
  // === ARRAY STORAGE TYPES ===
  TArrayStorageType = (astVariant, astInt64, astDouble, astString);

  // === PHASE 3: TYPED VARIABLE STORAGE (NO VARIANT!) ===
  // Maps VarIndex -> Type + TypedArrayIndex for ZERO-overhead variable access
  TVarTypeInfo = record
    VarType: TBasicVariableType;  // bvtInteger, bvtDouble, bvtString, etc.
    ArrayIndex: Integer;          // Index in appropriate typed array (FIntVariables, etc.)
  end;

  // === BASIC ARRAY CLASS WITH TYPED STORAGE ===
  TBasicArray = class
  private
    FDimensions: TArrayDimensions;
    FStorageType: TArrayStorageType;
    FTotalSize: Integer;

    // SESSION 14.5: Arena allocation support
    FArenaAllocated: Boolean;     // True if memory comes from arena
    FArenaPtr: Pointer;           // Raw pointer to arena memory (if FArenaAllocated=True)

    // Tagged union - only one of these is allocated (unless using arena)
    FDataVariant: TVariantDynArray;
    FDataInt64: array of Int64;
    FDataDouble: array of Double;
    FDataString: array of String;

    function CalculateIndex(const Indices: array of Integer): Integer;

  public
    constructor Create(const Dimensions: TArrayDimensions); overload;
    constructor Create(const Dimensions: TArrayDimensions; StorageType: TArrayStorageType); overload;
    constructor Create(const Dimensions: TArrayDimensions; StorageType: TArrayStorageType; Arena: TArenaAllocator); overload;  // SESSION 14.5
    destructor Destroy; override;

    // Generic Variant access (slower, for compatibility)
    function GetElement(const Indices: array of Integer): Variant;
    procedure SetElement(const Indices: array of Integer; const Value: Variant);
    function GetElement1D(Index: Integer): Variant;
    procedure SetElement1D(Index: Integer; const Value: Variant);

    // FAST: Typed access (direct native arrays, no Variant overhead)
    function GetElementInt64(Index: Integer): Int64; inline;
    procedure SetElementInt64(Index: Integer; Value: Int64); inline;
    function GetElementDouble(Index: Integer): Double; inline;
    procedure SetElementDouble(Index: Integer; Value: Double); inline;
    function GetElementString(Index: Integer): string; inline;
    procedure SetElementString(Index: Integer; const Value: string); inline;

    // SESSION 14.3: UNCHECKED variants (NO bounds check - caller guarantees safety!)
    function GetElementInt64Unchecked(Index: Integer): Int64; inline;
    procedure SetElementInt64Unchecked(Index: Integer; Value: Int64); inline;
    function GetElementDoubleUnchecked(Index: Integer): Double; inline;
    procedure SetElementDoubleUnchecked(Index: Integer; Value: Double); inline;

    property Dimensions: TArrayDimensions read FDimensions;
    property TotalSize: Integer read FTotalSize;
    property StorageType: TArrayStorageType read FStorageType;
  end;

  // === STORAGE MAPS ===
  TArrayMap = specialize TFPGMap<string, TBasicArray>;

  // === DATA STORAGE ===
  TDataList = class(TFPList)
  private
    FDataPointer: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddData(const Value: Variant);
    function ReadData: Variant;
    procedure ResetPointer;
    function IsAtEnd: Boolean;

    property DataPointer: Integer read FDataPointer write FDataPointer;
  end;

  // === BREAKPOINT MANAGEMENT ===
  TBreakpointList = class(TFPList)
  public
    procedure AddBreakpoint(LineNumber: Integer);
    procedure RemoveBreakpoint(LineNumber: Integer);
    function HasBreakpoint(LineNumber: Integer): Boolean;
    procedure ClearBreakpoints;
  end;

  // === EXECUTION CONTEXT ===

  { TExecutorContext }

  TExecutorContext = class
  private
    FVariables: TVariableMap;
    FArrays: TArrayMap;
    FFunctions: TFunctionMap;
    FDataList: TDataList;
    FBreakpoints: TBreakpointList;
    FInputSource: TStrings;
    FOutputTarget: TStrings;
    FInputIndex: Integer;
    FArrayIndexMode: TArrayIndexMode;  // PHASE 4F: Array dimensioning semantics

    // SESSION 14.5: Fast arena allocator for temporary arrays
    FArena: TArenaAllocator;

    // === TIER 3: STRING INTERNING (Optimization #1) ===
    // Eliminates repeated UpperCase() allocations for variable names
    FInternedStrings: TFPHashList;  // Cache for canonical (uppercased) names
    FUseStringInterning: Boolean;   // TIER 4: Flag to disable string interning

    // === ARRAY ACCESS CACHE (Tier 1 Optimization) ===
    FArrayCacheName: string;
    FArrayCacheObject: TBasicArray;
    FArrayCacheValid: Boolean;
    FArrayCacheStorageType: TArrayStorageType;  // TIER 4: Cache storage type

    // === INDEXED VARIABLE ACCESS (Tier 2 Optimization) ===
    // Direct indexed access - eliminates hash lookup and UpperCase() overhead
    FIndexedVariables: array of Variant;   // LEGACY: For untyped variables only
    FIndexedArrays: array of TBasicArray;  // Direct indexed array storage

    // === PHASE 3: TYPED VARIABLE STORAGE (ZERO Variant overhead!) ===
    FVarTypeMap: array of TVarTypeInfo;    // Maps VarIndex -> Type + TypedArrayIndex
    FIntVariables: array of Int64;         // Typed storage for Integer% and Boolean
    FFloatVariables: array of Double;      // Typed storage for Float! and Double#
    FStringVariables: array of String;     // Typed storage for String$

    procedure InitializeVariables;
    procedure InvalidateArrayCache; inline;

    // TIER 3: String interning helper (private)
    function InternString(const S: string): string; inline;

    // Type detection helpers (private)
    function ConvertToType(const Value: Variant; VarType: TBasicVariableType): Variant;
    function IsTypeMismatch(const Value: Variant; VarType: TBasicVariableType): Boolean;
    function InferVariableType(const VarName: string): TBasicVariableType; inline;

  public
    constructor Create;
    destructor Destroy; override;

    // Variable management
    function GetVariable(const Name: string): Variant; inline;
    
    // Type detection (public - needed by executor)
    function GetVariableType(const Name: string): TBasicVariableType;
    procedure SetVariable(const Name: string; const Value: Variant);
    function HasVariable(const Name: string): Boolean; inline;
    procedure ClearVariables;
    
    // === TIER 1.5: FAST NATIVE TYPE ACCESS ===
    // Direct type access - bypasses Variant type checking overhead
    // Use these in hot paths (loops, array ops, arithmetic)
    function GetVariableAsInt(const Name: string): Int64; inline;
    function GetVariableAsFloat(const Name: string): Double; inline;
    function GetVariableAsDouble(const Name: string): Double; inline;  // Alias for clarity
    function GetVariableAsString(const Name: string): string; inline;
    function GetVariableAsBool(const Name: string): Boolean; inline;

    // === TIER 4: UNIFIED TYPE+VALUE ACCESS ===
    // Single hash lookup for type + value - eliminates double lookup overhead
    function GetVariableTyped(const Name: string; out VarType: TBasicVariableType): Variant;
    
    procedure SetVariableInt(const Name: string; Value: Int64); inline;
    procedure SetVariableFloat(const Name: string; Value: Double); inline;
    procedure SetVariableDouble(const Name: string; Value: Double); inline;  // Alias
    procedure SetVariableString(const Name: string; const Value: string); inline;
    procedure SetVariableBool(const Name: string; Value: Boolean); inline;
    
    function GetVariableNames: TStringList;

    // Array management
    procedure DimensionArray(const Name: string; const Dimensions: TArrayDimensions);
    function GetArrayElement(const Name: string; const Indices: array of Integer): Variant; overload;
    function GetArrayElement(const Name: string; Index: Integer): Variant; overload;
    procedure SetArrayElement(const Name: string; const Indices: array of Integer; const Value: Variant); overload;
    procedure SetArrayElement(const Name: string; Index: Integer; const Value: Variant); overload;
    function HasArray(const Name: string): Boolean;
    function GetArrayStorageType(const Name: string): TArrayStorageType; inline; // TIER 4
    function GetCachedArrayStorageType: TArrayStorageType; inline; // TIER 4: From cache

    // TIER 4: Single lookup optimization for array access
    function TryGetArrayElement(const Name: string; Index: Integer; out ArrResult: Variant): Boolean;

    procedure ClearArrays;
    
    // === OPTIMIZED ARRAY ACCESS (Tier 1) ===
    function GetArrayElementFast(const Name: string; Index: Integer): Variant; inline;
    procedure SetArrayElementFast(const Name: string; Index: Integer; const Value: Variant); inline;

    // === TIER 4: TYPED ARRAY ACCESS (no Variant overhead) ===
    function GetArrayElementInt64Fast(const Name: string; Index: Integer): Int64; inline;
    procedure SetArrayElementInt64Fast(const Name: string; Index: Integer; Value: Int64); inline;
    function GetArrayElementDoubleFast(const Name: string; Index: Integer): Double; inline;
    procedure SetArrayElementDoubleFast(const Name: string; Index: Integer; Value: Double); inline;

    // === INDEXED VARIABLE ACCESS (Tier 2 - FASTEST!) ===
    // Direct indexed access - used by bytecode VM for maximum performance
    // Index comes from TVariableTable - no string operations, no hash lookup
    function GetVariableByIndex(VarIndex: Integer): Variant; inline;
    procedure SetVariableByIndex(VarIndex: Integer; const Value: Variant); inline;
    function GetVariableByIndexInt(VarIndex: Integer): Integer; inline;
    procedure SetVariableByIndexInt(VarIndex: Integer; Value: Integer); inline;
    function GetVariableByIndexFloat(VarIndex: Integer): Double; inline;
    procedure SetVariableByIndexFloat(VarIndex: Integer; Value: Double); inline;
    function GetVariableByIndexStr(VarIndex: Integer): string; inline;
    procedure SetVariableByIndexStr(VarIndex: Integer; const Value: string); inline;

    // Indexed array access
    function GetArrayElementByIndex(ArrayIndex, ElementIndex: Integer): Variant; inline;
    procedure SetArrayElementByIndex(ArrayIndex, ElementIndex: Integer; const Value: Variant); inline;
    procedure DimensionArrayByIndex(ArrayIndex: Integer; const Dimensions: TArrayDimensions); overload;
    procedure DimensionArrayByIndex(ArrayIndex: Integer; const Dimensions: TArrayDimensions; StorageType: TArrayStorageType); overload;

    // FAST: Typed indexed array access (no Variant overhead)
    function GetArrayElementByIndexInt64(ArrayIndex, ElementIndex: Integer): Int64; inline;
    procedure SetArrayElementByIndexInt64(ArrayIndex, ElementIndex: Integer; Value: Int64); inline;
    function GetArrayElementByIndexDouble(ArrayIndex, ElementIndex: Integer): Double; inline;
    procedure SetArrayElementByIndexDouble(ArrayIndex, ElementIndex: Integer; Value: Double); inline;

    // SESSION 14.3: UNCHECKED typed array access (NO bounds check - for loop unrolling!)
    function GetArrayElementByIndexInt64Unchecked(ArrayIndex, ElementIndex: Integer): Int64; inline;
    procedure SetArrayElementByIndexInt64Unchecked(ArrayIndex, ElementIndex: Integer; Value: Int64); inline;
    function GetArrayElementByIndexDoubleUnchecked(ArrayIndex, ElementIndex: Integer): Double; inline;
    procedure SetArrayElementByIndexDoubleUnchecked(ArrayIndex, ElementIndex: Integer; Value: Double); inline;

    // Initialize indexed storage (must be called after variable table is populated)
    procedure AllocateIndexedStorage(VarCount, ArrayCount: Integer); overload;
    procedure AllocateIndexedStorage(VarTable: TObject; ArrayCount: Integer); overload;

    // Function management
    procedure RegisterFunction(const Name: string; Func: TBuiltinFunction);
    function CallFunction(const Name: string; const Args: array of Variant): Variant;
    function HasFunction(const Name: string): Boolean;

    // TIER 4: Single lookup optimization - eliminates double hash lookup
    function TryCallFunction(const Name: string; const Args: array of Variant; out FuncResult: Variant): Boolean;

    // Data management (for DATA/READ statements)
    procedure AddDataValue(const Value: Variant);
    function ReadDataValue: Variant;
    procedure ResetDataPointer;

    // Breakpoint management
    procedure AddBreakpoint(LineNumber: Integer);
    procedure RemoveBreakpoint(LineNumber: Integer);
    function HasBreakpoint(LineNumber: Integer): Boolean;
    procedure ClearBreakpoints;

    // I/O operations
    function ReadInput(const Prompt: string = ''): string;
    procedure WriteOutput(const Text: string);
    procedure WriteOutputLine(const Text: string);
    procedure SetInputSource(Source: TStrings);
    procedure SetOutputTarget(Target: TStrings);

    // Utility
    procedure Reset;

    // SESSION 14.6: Pointer arithmetic support - get direct pointer to array data
    function GetDataPtrInt64(ArrayIndex, ElementIndex: Integer): Pointer; inline;
    function GetDataPtrDouble(ArrayIndex, ElementIndex: Integer): Pointer; inline;

    // PHASE 4F: Array dimensioning mode
    property ArrayIndexMode: TArrayIndexMode read FArrayIndexMode write FArrayIndexMode;
  end;

implementation

uses
  Math, SedaiBytecodeTypes;

{ TBasicArray }

constructor TBasicArray.Create(const Dimensions: TArrayDimensions);
begin
  // Default: use Variant storage (backward compatible)
  Create(Dimensions, astVariant);
end;

constructor TBasicArray.Create(const Dimensions: TArrayDimensions; StorageType: TArrayStorageType);
var
  i: Integer;
begin
  inherited Create;
  FDimensions := Copy(Dimensions);
  FStorageType := StorageType;
  FArenaAllocated := False;  // SESSION 14.5: Not using arena
  FArenaPtr := nil;

  FTotalSize := 1;
  for i := 0 to Length(FDimensions) - 1 do
    FTotalSize := FTotalSize * FDimensions[i];

  // Allocate storage based on type (tagged union - only one allocated)
  case FStorageType of
    astVariant:
      begin
        SetLength(FDataVariant, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataVariant[i] := 0;
      end;

    astInt64:
      begin
        SetLength(FDataInt64, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataInt64[i] := 0;
      end;

    astDouble:
      begin
        SetLength(FDataDouble, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataDouble[i] := 0.0;
      end;

    astString:
      begin
        SetLength(FDataString, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataString[i] := '';
      end;
  end;
end;

// SESSION 14.5: Arena-allocated array constructor
constructor TBasicArray.Create(const Dimensions: TArrayDimensions; StorageType: TArrayStorageType; Arena: TArenaAllocator);
var
  i: Integer;
  ByteSize: NativeUInt;
  P: Pointer;
begin
  inherited Create;
  FDimensions := Copy(Dimensions);
  FStorageType := StorageType;

  FTotalSize := 1;
  for i := 0 to Length(FDimensions) - 1 do
    FTotalSize := FTotalSize * FDimensions[i];

  // SESSION 14.5: Attempt arena allocation (only for numeric types!)
  // String and Variant types still use heap (managed memory requirement)
  if Assigned(Arena) and (StorageType in [astInt64, astDouble]) then
  begin
    // Calculate required bytes
    case StorageType of
      astInt64: ByteSize := FTotalSize * SizeOf(Int64);
      astDouble: ByteSize := FTotalSize * SizeOf(Double);
      else ByteSize := 0;
    end;

    // Try to allocate from arena
    P := Arena.Allocate(ByteSize);
    if Assigned(P) then
    begin
      // SUCCESS: Arena allocation worked!
      FArenaAllocated := True;
      FArenaPtr := P;

      // Initialize memory to zero
      FillChar(P^, ByteSize, 0);
      Exit;  // Done - skip heap allocation
    end;
    // FALLBACK: Arena full - use heap allocation below
  end;

  // FALLBACK: Use heap allocation (same as non-arena constructor)
  FArenaAllocated := False;
  FArenaPtr := nil;

  case FStorageType of
    astVariant:
      begin
        SetLength(FDataVariant, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataVariant[i] := 0;
      end;

    astInt64:
      begin
        SetLength(FDataInt64, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataInt64[i] := 0;
      end;

    astDouble:
      begin
        SetLength(FDataDouble, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataDouble[i] := 0.0;
      end;

    astString:
      begin
        SetLength(FDataString, FTotalSize);
        for i := 0 to FTotalSize - 1 do
          FDataString[i] := '';
      end;
  end;
end;

destructor TBasicArray.Destroy;
begin
  // SESSION 14.5: Arena-allocated memory is NOT freed (arena owns it!)
  if not FArenaAllocated then
  begin
    // Free heap-allocated storage
    case FStorageType of
      astVariant: SetLength(FDataVariant, 0);
      astInt64: SetLength(FDataInt64, 0);
      astDouble: SetLength(FDataDouble, 0);
      astString: SetLength(FDataString, 0);
    end;
  end;
  // Arena pointer is simply forgotten - arena will reset it later
  SetLength(FDimensions, 0);
  inherited Destroy;
end;

function TBasicArray.CalculateIndex(const Indices: array of Integer): Integer;
var
  i: Integer;
begin
  if Length(Indices) <> Length(FDimensions) then
    RaiseArrayDimensionError(Length(FDimensions), Length(Indices));

  Result := 0;
  for i := 0 to Length(Indices) - 1 do
  begin
    if (Indices[i] < 0) or (Indices[i] >= FDimensions[i]) then
      RaiseArrayIndexOutOfBounds(Indices[i], FDimensions[i] - 1);

    Result := Result * FDimensions[i] + Indices[i];
  end;
end;

function TBasicArray.GetElement(const Indices: array of Integer): Variant;
var
  Index: Integer;
begin
  Index := CalculateIndex(Indices);
  case FStorageType of
    astVariant: Result := FDataVariant[Index];
    astInt64: Result := FDataInt64[Index];
    astDouble: Result := FDataDouble[Index];
    astString: Result := FDataString[Index];
  end;
end;

procedure TBasicArray.SetElement(const Indices: array of Integer; const Value: Variant);
var
  Index: Integer;
begin
  Index := CalculateIndex(Indices);
  case FStorageType of
    astVariant: FDataVariant[Index] := Value;
    astInt64: FDataInt64[Index] := Int64(Value);
    astDouble: FDataDouble[Index] := Double(Value);
    astString: FDataString[Index] := VarToStr(Value);
  end;
end;

function TBasicArray.GetElement1D(Index: Integer): Variant;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);
  case FStorageType of
    astVariant: Result := FDataVariant[Index];
    astInt64: Result := FDataInt64[Index];
    astDouble: Result := FDataDouble[Index];
    astString: Result := FDataString[Index];
  end;
end;

procedure TBasicArray.SetElement1D(Index: Integer; const Value: Variant);
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);
  case FStorageType of
    astVariant: FDataVariant[Index] := Value;
    astInt64: FDataInt64[Index] := Int64(Value);
    astDouble: FDataDouble[Index] := Double(Value);
    astString: FDataString[Index] := VarToStr(Value);
  end;
end;

// FAST: Typed access methods (direct native arrays, no Variant overhead)
function TBasicArray.GetElementInt64(Index: Integer): Int64; inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);

  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astInt64 then
      Result := PInt64(FArenaPtr)[Index]
    else if FStorageType = astDouble then
      Result := Trunc(PDouble(FArenaPtr)[Index])
    else
      Result := 0;
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astInt64 then
      Result := FDataInt64[Index]
    else if FStorageType = astVariant then
      Result := Int64(FDataVariant[Index])
    else if FStorageType = astDouble then
      Result := Trunc(FDataDouble[Index])
    else
      Result := 0;
  end;
end;

procedure TBasicArray.SetElementInt64(Index: Integer; Value: Int64); inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);

  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astInt64 then
      PInt64(FArenaPtr)[Index] := Value
    else if FStorageType = astDouble then
      PDouble(FArenaPtr)[Index] := Value;
    // No string support for arena
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astInt64 then
      FDataInt64[Index] := Value
    else if FStorageType = astVariant then
      FDataVariant[Index] := Value
    else if FStorageType = astDouble then
      FDataDouble[Index] := Value
    else
      FDataString[Index] := IntToStr(Value);
  end;
end;

function TBasicArray.GetElementDouble(Index: Integer): Double; inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);

  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astDouble then
      Result := PDouble(FArenaPtr)[Index]
    else if FStorageType = astInt64 then
      Result := PInt64(FArenaPtr)[Index]
    else
      Result := 0.0;
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astDouble then
      Result := FDataDouble[Index]
    else if FStorageType = astInt64 then
      Result := FDataInt64[Index]
    else if FStorageType = astVariant then
      Result := Double(FDataVariant[Index])
    else
      Result := 0.0;
  end;
end;

procedure TBasicArray.SetElementDouble(Index: Integer; Value: Double); inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);

  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astDouble then
      PDouble(FArenaPtr)[Index] := Value
    else if FStorageType = astInt64 then
      PInt64(FArenaPtr)[Index] := Trunc(Value);
    // No string support for arena
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astDouble then
      FDataDouble[Index] := Value
    else if FStorageType = astInt64 then
      FDataInt64[Index] := Trunc(Value)
    else if FStorageType = astVariant then
      FDataVariant[Index] := Value
    else
      FDataString[Index] := FloatToStr(Value);
  end;
end;

function TBasicArray.GetElementString(Index: Integer): string; inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);
  if FStorageType = astString then
    Result := FDataString[Index]
  else if FStorageType = astVariant then
    Result := VarToStr(FDataVariant[Index])
  else if FStorageType = astInt64 then
    Result := IntToStr(FDataInt64[Index])
  else
    Result := FloatToStr(FDataDouble[Index]);
end;

procedure TBasicArray.SetElementString(Index: Integer; const Value: string); inline;
begin
  if (Index < 0) or (Index >= FTotalSize) then
    RaiseArrayIndexOutOfBounds(Index, FTotalSize - 1);
  if FStorageType = astString then
    FDataString[Index] := Value
  else if FStorageType = astVariant then
    FDataVariant[Index] := Value
  else if FStorageType = astInt64 then
    FDataInt64[Index] := StrToInt64Def(Value, 0)
  else
    FDataDouble[Index] := StrToFloatDef(Value, 0.0);
end;

// SESSION 14.3: UNCHECKED variants (NO bounds check - for loop unrolling!)
// SAFETY: Caller MUST guarantee Index is valid (0 <= Index < FTotalSize)
function TBasicArray.GetElementInt64Unchecked(Index: Integer): Int64; inline;
begin
  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astInt64 then
      Result := PInt64(FArenaPtr)[Index]
    else if FStorageType = astDouble then
      Result := Trunc(PDouble(FArenaPtr)[Index])
    else
      Result := 0;
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astInt64 then
      Result := FDataInt64[Index]
    else if FStorageType = astVariant then
      Result := Int64(FDataVariant[Index])
    else if FStorageType = astDouble then
      Result := Trunc(FDataDouble[Index])
    else
      Result := 0;
  end;
end;

procedure TBasicArray.SetElementInt64Unchecked(Index: Integer; Value: Int64); inline;
begin
  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astInt64 then
      PInt64(FArenaPtr)[Index] := Value
    else if FStorageType = astDouble then
      PDouble(FArenaPtr)[Index] := Value;
    // No string support for arena
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astInt64 then
      FDataInt64[Index] := Value
    else if FStorageType = astVariant then
      FDataVariant[Index] := Value
    else if FStorageType = astDouble then
      FDataDouble[Index] := Value
    else
      FDataString[Index] := IntToStr(Value);
  end;
end;

function TBasicArray.GetElementDoubleUnchecked(Index: Integer): Double; inline;
begin
  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astDouble then
      Result := PDouble(FArenaPtr)[Index]
    else if FStorageType = astInt64 then
      Result := PInt64(FArenaPtr)[Index]
    else
      Result := 0.0;
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astDouble then
      Result := FDataDouble[Index]
    else if FStorageType = astInt64 then
      Result := FDataInt64[Index]
    else if FStorageType = astVariant then
      Result := Double(FDataVariant[Index])
    else
      Result := 0.0;
  end;
end;

procedure TBasicArray.SetElementDoubleUnchecked(Index: Integer; Value: Double); inline;
begin
  // SESSION 14.5: Arena-allocated arrays use raw pointer access
  if FArenaAllocated then
  begin
    if FStorageType = astDouble then
      PDouble(FArenaPtr)[Index] := Value
    else if FStorageType = astInt64 then
      PInt64(FArenaPtr)[Index] := Trunc(Value);
    // No string support for arena
  end
  else
  begin
    // Heap-allocated: use dynamic arrays
    if FStorageType = astDouble then
      FDataDouble[Index] := Value
    else if FStorageType = astInt64 then
      FDataInt64[Index] := Trunc(Value)
    else if FStorageType = astVariant then
      FDataVariant[Index] := Value
    else
      FDataString[Index] := FloatToStr(Value);
  end;
end;

{ TDataList }

constructor TDataList.Create;
begin
  inherited Create;
  FDataPointer := 0;
end;

destructor TDataList.Destroy;
var
  i: Integer;
  VarPtr: PVariant;
begin
  for i := 0 to Count - 1 do
  begin
    VarPtr := PVariant(Items[i]);
    Dispose(VarPtr);
  end;
  inherited Destroy;
end;

procedure TDataList.AddData(const Value: Variant);
var
  VarPtr: PVariant;
begin
  New(VarPtr);
  VarPtr^ := Value;
  Add(VarPtr);
end;

function TDataList.ReadData: Variant;
var
  VarPtr: PVariant;
begin
  if IsAtEnd then
    raise Exception.Create('Out of DATA');

  VarPtr := PVariant(Items[FDataPointer]);
  Result := VarPtr^;
  Inc(FDataPointer);
end;

procedure TDataList.ResetPointer;
begin
  FDataPointer := 0;
end;

function TDataList.IsAtEnd: Boolean;
begin
  Result := FDataPointer >= Count;
end;

{ TBreakpointList }

procedure TBreakpointList.AddBreakpoint(LineNumber: Integer);
begin
  if not HasBreakpoint(LineNumber) then
    Add(Pointer(LineNumber));
end;

procedure TBreakpointList.RemoveBreakpoint(LineNumber: Integer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if PtrInt(Items[i]) = LineNumber then
    begin
      Delete(i);
      Break;
    end;
  end;
end;

function TBreakpointList.HasBreakpoint(LineNumber: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if PtrInt(Items[i]) = LineNumber then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TBreakpointList.ClearBreakpoints;
begin
  inherited Clear;
end;

{ TExecutorContext }

constructor TExecutorContext.Create;
begin
  inherited Create;
  FVariables := TVariableMap.Create;
  FArrays := TArrayMap.Create;
  FFunctions := TFunctionMap.Create;
  FDataList := TDataList.Create;
  FBreakpoints := TBreakpointList.Create;
  FInputSource := nil;
  FOutputTarget := nil;
  FInputIndex := 0;
  FArrayIndexMode := aimMaxIndex;  // PHASE 4F: Default to BASIC style (0..N)

  // SESSION 14.5: Create arena allocator (10MB buffer for temporary arrays)
  FArena := TArenaAllocator.Create(10 * 1024 * 1024);  // 10MB arena

  // TIER 3: String interning cache
  FInternedStrings := TFPHashList.Create;
  FUseStringInterning := False;  // TIER 4: Disabled (causes -10-15ms regression)

  // Initialize array cache
  FArrayCacheName := '';
  FArrayCacheObject := nil;
  FArrayCacheValid := False;

  // Initialize indexed storage
  SetLength(FIndexedVariables, 0);
  SetLength(FIndexedArrays, 0);

  InitializeVariables;
end;

destructor TExecutorContext.Destroy;
var
  i: Integer;
  P: PString;
begin
  ClearArrays;

  // TIER 3: Free string interning cache - dispose allocated strings
  if Assigned(FInternedStrings) then
  begin
    for i := 0 to FInternedStrings.Count - 1 do
    begin
      P := PString(FInternedStrings.Items[i]);
      if Assigned(P) then
        Dispose(P);
    end;
    FInternedStrings.Free;
  end;

  FArena.Free;  // SESSION 14.5: Free arena allocator
  FBreakpoints.Free;
  FDataList.Free;
  FFunctions.Free;
  FArrays.Free;
  FVariables.Free;
  inherited Destroy;
end;

procedure TExecutorContext.InitializeVariables;
begin
  // Initialize any default variables here
end;

function TExecutorContext.GetVariableType(const Name: string): TBasicVariableType;
var
  LastChar: Char;
begin
  if Length(Name) = 0 then
  begin
    Result := bvtNumber;
    Exit;
  end;

  LastChar := Name[Length(Name)];
  case LastChar of
    '%': Result := bvtInteger;
    '!': Result := bvtSingle;
    '#': Result := bvtDouble;
    '$': Result := bvtString;
    else Result := bvtNumber;
  end;
end;

// PHASE 3: Helper for inferring variable type from name suffix
function TExecutorContext.InferVariableType(const VarName: string): TBasicVariableType; inline;
var
  LastChar: Char;
begin
  if Length(VarName) = 0 then
  begin
    Result := bvtNumber;
    Exit;
  end;

  LastChar := VarName[Length(VarName)];
  case LastChar of
    '%': Result := bvtInteger;
    '!': Result := bvtSingle;
    '#': Result := bvtDouble;
    '$': Result := bvtString;
    else Result := bvtNumber;
  end;
end;

// TIER 3: String interning - eliminates repeated UpperCase() allocations
// Returns canonical (uppercased) string, reusing cached instances
// TIER 4: Can be disabled via FUseStringInterning flag
function TExecutorContext.InternString(const S: string): string; inline;
var
  Upper: string;
  Index: Integer;
  CachedStr: PString;
begin
  // TIER 4: Bypass interning if disabled
  if not FUseStringInterning then
  begin
    Result := UpperCase(S);
    Exit;
  end;

  Upper := UpperCase(S);

  // Try to find cached version in hash list
  Index := FInternedStrings.FindIndexOf(Upper);

  if Index >= 0 then
  begin
    // Cache hit - return cached string pointer
    CachedStr := PString(FInternedStrings.Items[Index]);
    Result := CachedStr^;
  end
  else
  begin
    // Cache miss - allocate new string and store
    New(CachedStr);
    CachedStr^ := Upper;
    FInternedStrings.Add(Upper, CachedStr);
    Result := Upper;
  end;
end;

function TExecutorContext.ConvertToType(const Value: Variant; VarType: TBasicVariableType): Variant;
begin
  case VarType of
    bvtInteger:
      begin
        if VarIsStr(Value) then
          RaiseTypeMismatch('Integer conversion')
        else
          Result := Trunc(Double(Value)); // Truncate, don't round
      end;

    bvtSingle:
      begin
        if VarIsStr(Value) then
          RaiseTypeMismatch('Single conversion')
        else
          Result := Single(Double(Value));
      end;

    bvtDouble:
      begin
        if VarIsStr(Value) then
          RaiseTypeMismatch('Double conversion')
        else
          Result := Double(Value);
      end;

    bvtString:
      begin
        Result := VarToStr(Value);
      end;

    bvtNumber:
      begin
        // Normal numeric variable - keep the original type
        Result := Value;
      end;

    bvtArray:
      begin
        // Gli array sono gestiti separatamente
        Result := Value;
      end;
  end;
end;

function TExecutorContext.IsTypeMismatch(const Value: Variant; VarType: TBasicVariableType): Boolean;
begin
  case VarType of
    bvtInteger, bvtSingle, bvtDouble, bvtNumber:
      Result := VarIsStr(Value) and not VarIsNumeric(Value);
    bvtString:
      Result := False; // Strings accept anything (with conversion)
    bvtArray:
      Result := False; // Arrays are handled separately
    else
      Result := False;
  end;
end;

function TExecutorContext.GetVariable(const Name: string): Variant;
var
  CanonicalName: string;
begin
  // TIER 3: Use string interning to eliminate repeated UpperCase() allocations
  CanonicalName := InternString(Name);
  if FVariables.TryGetData(CanonicalName, Result) then
    Exit  // Trovato
  else
    Result := 0; // BASIC default value
end;

procedure TExecutorContext.SetVariable(const Name: string; const Value: Variant);
var
  CanonicalName: string;
  VarType: TBasicVariableType;
  ConvertedValue: Variant;
begin
  // TIER 3: Use string interning
  CanonicalName := InternString(Name);
  VarType := GetVariableType(CanonicalName);

  // *** TYPE CHECKING E CONVERSIONE ***
  if IsTypeMismatch(Value, VarType) then
    RaiseTypeMismatch('SetVariable: ' + Name);

  ConvertedValue := ConvertToType(Value, VarType);
  FVariables[CanonicalName] := ConvertedValue;
end;

function TExecutorContext.HasVariable(const Name: string): Boolean;
begin
  // TIER 3: Use string interning
  Result := FVariables.IndexOf(InternString(Name)) >= 0;
end;

procedure TExecutorContext.ClearVariables;
begin
  FVariables.Clear;
end;

function TExecutorContext.GetVariableNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FVariables.Count - 1 do
    Result.Add(FVariables.Keys[i]);
end;

procedure TExecutorContext.DimensionArray(const Name: string; const Dimensions: TArrayDimensions);
var
  CanonicalName: string;
  ExistingArray: TBasicArray;
  AdjustedDimensions: TArrayDimensions;
  i: Integer;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning

  // Remove existing array if present
  if HasArray(CanonicalName) then
  begin
    ExistingArray := FArrays[CanonicalName];
    FArrays.Remove(CanonicalName);
    ExistingArray.Free;
  end;

  // Adjust dimensions based on array index mode
  // NOTE: This should be configured via parser options
  SetLength(AdjustedDimensions, Length(Dimensions));
  for i := 0 to Length(Dimensions) - 1 do
  begin
    // For Commodore BASIC compatibility: DIM A(10) means indices 0..10 (11 elements)
    // For modern BASIC: DIM A(10) means 10 elements (indices 0..9)
    // We'll default to Commodore style for now
    AdjustedDimensions[i] := Dimensions[i] + 1; // Add 1 for Commodore BASIC style
  end;

  FArrays[CanonicalName] := TBasicArray.Create(AdjustedDimensions);
  InvalidateArrayCache;  // Array structure changed
end;

function TExecutorContext.GetArrayElement(const Name: string; const Indices: array of Integer): Variant;
var
  CanonicalName: string;
  Arr: TBasicArray;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if not HasArray(CanonicalName) then
    RaiseArrayNotDimensioned(Name);

  Arr := FArrays[CanonicalName];
  Result := Arr.GetElement(Indices);
end;

function TExecutorContext.GetArrayElement(const Name: string; Index: Integer): Variant;
var
  CanonicalName: string;
  Arr: TBasicArray;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if not HasArray(CanonicalName) then
    RaiseArrayNotDimensioned(Name);

  Arr := FArrays[CanonicalName];
  Result := Arr.GetElement1D(Index);
end;

procedure TExecutorContext.SetArrayElement(const Name: string; const Indices: array of Integer; const Value: Variant);
var
  CanonicalName: string;
  Arr: TBasicArray;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if not HasArray(CanonicalName) then
    RaiseArrayNotDimensioned(Name);

  Arr := FArrays[CanonicalName];
  Arr.SetElement(Indices, Value);
end;

procedure TExecutorContext.SetArrayElement(const Name: string; Index: Integer; const Value: Variant);
var
  CanonicalName: string;
  Arr: TBasicArray;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if not HasArray(CanonicalName) then
    RaiseArrayNotDimensioned(Name);

  Arr := FArrays[CanonicalName];
  Arr.SetElement1D(Index, Value);
end;

function TExecutorContext.HasArray(const Name: string): Boolean;
begin
  // TIER 3: Use string interning
  Result := FArrays.IndexOf(InternString(Name)) >= 0;
end;

// TIER 4: Get array storage type for typed access optimization
function TExecutorContext.GetArrayStorageType(const Name: string): TArrayStorageType; inline;
var
  Arr: TBasicArray;
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);
  Arr := FArrays[CanonicalName];
  if Assigned(Arr) then
    Result := Arr.StorageType
  else
    Result := astVariant; // Fallback
end;

// TIER 4: Get storage type from cache (no lookup needed)
function TExecutorContext.GetCachedArrayStorageType: TArrayStorageType; inline;
begin
  Result := FArrayCacheStorageType;
end;

// TIER 4: Single lookup - combines HasArray + GetArrayElementFast
// Eliminates double hash lookup overhead (saves 1 lookup per array access)
function TExecutorContext.TryGetArrayElement(const Name: string; Index: Integer; out ArrResult: Variant): Boolean;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
  begin
    ArrResult := FArrayCacheObject.GetElement1D(Index);
    Exit(True);
  end;

  // Cache miss - try lookup
  CanonicalName := InternString(Name);
  if FArrays.IndexOf(CanonicalName) >= 0 then
  begin
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheStorageType := FArrayCacheObject.StorageType;
    FArrayCacheValid := True;
    ArrResult := FArrayCacheObject.GetElement1D(Index);
    Exit(True);
  end;

  ArrResult := Null;
  Exit(False);
end;

procedure TExecutorContext.ClearArrays;
var
  i: Integer;
begin
  for i := 0 to FArrays.Count - 1 do
    FArrays.Data[i].Free;
  FArrays.Clear;
  InvalidateArrayCache;
end;

procedure TExecutorContext.RegisterFunction(const Name: string; Func: TBuiltinFunction);
begin
  // TIER 3: Use string interning
  FFunctions[InternString(Name)] := Func;
end;

function TExecutorContext.CallFunction(const Name: string; const Args: array of Variant): Variant;
var
  CanonicalName: string;
  Index: Integer;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  Index := FFunctions.IndexOf(CanonicalName);
  if Index >= 0 then
    Result := FFunctions.Data[Index](Args)
  else
    raise Exception.CreateFmt('Unknown function: %s', [Name]);
end;

function TExecutorContext.HasFunction(const Name: string): Boolean;
begin
  // TIER 3: Use string interning
  Result := FFunctions.IndexOf(InternString(Name)) >= 0;
end;

// TIER 4: Single lookup - combines HasFunction + CallFunction
// Eliminates double hash lookup overhead (saves 1 lookup per function call)
function TExecutorContext.TryCallFunction(const Name: string; const Args: array of Variant; out FuncResult: Variant): Boolean;
var
  CanonicalName: string;
  Index: Integer;
begin
  CanonicalName := InternString(Name);
  Index := FFunctions.IndexOf(CanonicalName);
  if Index >= 0 then
  begin
    FuncResult := FFunctions.Data[Index](Args);
    Exit(True);
  end;
  FuncResult := Null;
  Exit(False);
end;

procedure TExecutorContext.AddDataValue(const Value: Variant);
begin
  FDataList.AddData(Value);
end;

function TExecutorContext.ReadDataValue: Variant;
begin
  Result := FDataList.ReadData;
end;

procedure TExecutorContext.ResetDataPointer;
begin
  FDataList.ResetPointer;
end;

procedure TExecutorContext.AddBreakpoint(LineNumber: Integer);
begin
  FBreakpoints.AddBreakpoint(LineNumber);
end;

procedure TExecutorContext.RemoveBreakpoint(LineNumber: Integer);
begin
  FBreakpoints.RemoveBreakpoint(LineNumber);
end;

function TExecutorContext.HasBreakpoint(LineNumber: Integer): Boolean;
begin
  Result := FBreakpoints.HasBreakpoint(LineNumber);
end;

procedure TExecutorContext.ClearBreakpoints;
begin
  FBreakpoints.ClearBreakpoints;
end;

function TExecutorContext.ReadInput(const Prompt: string): string;
begin
  if Assigned(FInputSource) then
  begin
    if FInputIndex < FInputSource.Count then
    begin
      Result := FInputSource[FInputIndex];
      Inc(FInputIndex);
    end
    else
      Result := '';
  end
  else
  begin
    if Prompt <> '' then
      Write(Prompt);
    ReadLn(Result);
  end;
end;

procedure TExecutorContext.WriteOutput(const Text: string);
begin
  if Assigned(FOutputTarget) then
  begin
    if FOutputTarget.Count > 0 then
      FOutputTarget[FOutputTarget.Count - 1] := FOutputTarget[FOutputTarget.Count - 1] + Text
    else
      FOutputTarget.Add(Text);
  end
  else
    System.Write(Text);
end;

procedure TExecutorContext.WriteOutputLine(const Text: string);
begin
  if Assigned(FOutputTarget) then
    FOutputTarget.Add(Text)
  else
    WriteLn(Text);
end;

procedure TExecutorContext.SetInputSource(Source: TStrings);
begin
  FInputSource := Source;
  FInputIndex := 0;
end;

procedure TExecutorContext.SetOutputTarget(Target: TStrings);
begin
  FOutputTarget := Target;
end;

procedure TExecutorContext.Reset;
begin
  ClearVariables;
  ClearArrays;
  ResetDataPointer;
  ClearBreakpoints;

  // SESSION 14.5: Reset arena to reuse memory for next program run
  FArena.Reset;
end;

{ Array cache optimization methods }

procedure TExecutorContext.InvalidateArrayCache; inline;
begin
  FArrayCacheValid := False;
end;

function TExecutorContext.GetArrayElementFast(const Name: string; Index: Integer): Variant; inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
  begin
    // Cache hit - direct access
    Result := FArrayCacheObject.GetElement1D(Index);
  end
  else
  begin
    // Cache miss - update cache and access
    CanonicalName := InternString(Name);  // TIER 3: String interning
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheStorageType := FArrayCacheObject.StorageType;  // TIER 4: Cache type
    FArrayCacheValid := True;
    Result := FArrayCacheObject.GetElement1D(Index);
  end;
end;

procedure TExecutorContext.SetArrayElementFast(const Name: string; Index: Integer; const Value: Variant); inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
  begin
    // Cache hit - direct access
    FArrayCacheObject.SetElement1D(Index, Value);
  end
  else
  begin
    // Cache miss - update cache and access
    CanonicalName := InternString(Name);  // TIER 3: String interning
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheValid := True;
    FArrayCacheObject.SetElement1D(Index, Value);
  end;
end;

// === TIER 4: TYPED ARRAY ACCESS (no Variant overhead) ===
// Direct typed access - bypasses Variant boxing/unboxing
// Uses array cache like GetArrayElementFast for performance

function TExecutorContext.GetArrayElementInt64Fast(const Name: string; Index: Integer): Int64; inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
    Result := FArrayCacheObject.GetElementInt64(Index)
  else
  begin
    // Cache miss - update cache including storage type
    CanonicalName := InternString(Name);
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheStorageType := FArrayCacheObject.StorageType;
    FArrayCacheValid := True;
    Result := FArrayCacheObject.GetElementInt64(Index);
  end;
end;

procedure TExecutorContext.SetArrayElementInt64Fast(const Name: string; Index: Integer; Value: Int64); inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
    FArrayCacheObject.SetElementInt64(Index, Value)
  else
  begin
    // Cache miss - update cache and access
    CanonicalName := InternString(Name);
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheValid := True;
    FArrayCacheObject.SetElementInt64(Index, Value);
  end;
end;

function TExecutorContext.GetArrayElementDoubleFast(const Name: string; Index: Integer): Double; inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
    Result := FArrayCacheObject.GetElementDouble(Index)
  else
  begin
    // Cache miss - update cache and access
    CanonicalName := InternString(Name);
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheValid := True;
    Result := FArrayCacheObject.GetElementDouble(Index);
  end;
end;

procedure TExecutorContext.SetArrayElementDoubleFast(const Name: string; Index: Integer; Value: Double); inline;
var
  CanonicalName: string;
begin
  // Check cache first
  if FArrayCacheValid and (Name = FArrayCacheName) then
    FArrayCacheObject.SetElementDouble(Index, Value)
  else
  begin
    // Cache miss - update cache and access
    CanonicalName := InternString(Name);
    FArrayCacheObject := FArrays[CanonicalName];
    FArrayCacheName := Name;
    FArrayCacheValid := True;
    FArrayCacheObject.SetElementDouble(Index, Value);
  end;
end;

// === TIER 1.5: FAST NATIVE TYPE ACCESS ===
// Direct native type access - Variant is only used internally for storage
// These methods eliminate Variant type checking overhead in hot loops
//
// KEY INSIGHT: Int64/Double/String -> Variant conversion is FAST (compiler intrinsic)
//              Variant type checking (VarType, VarIsNumeric, etc.) is SLOW (function calls)
//              So we store as Variant but read/write with explicit types

function TExecutorContext.GetVariableAsInt(const Name: string): Int64; inline;
var
  V: Variant;
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if FVariables.TryGetData(CanonicalName, V) then
  begin
    // Direct Variant -> Int64 conversion (fast!)
    if VarIsNumeric(V) then
      Result := Int64(V)
    else if VarIsStr(V) then
      Result := StrToInt64Def(VarToStr(V), 0)
    else
      Result := 0;
  end
  else
    Result := 0;  // BASIC default
end;

function TExecutorContext.GetVariableAsFloat(const Name: string): Double; inline;
var
  V: Variant;
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if FVariables.TryGetData(CanonicalName, V) then
  begin
    // Direct Variant -> Double conversion (fast!)
    if VarIsNumeric(V) then
      Result := Double(V)
    else if VarIsStr(V) then
      Result := StrToFloatDef(VarToStr(V), 0.0)
    else
      Result := 0.0;
  end
  else
    Result := 0.0;  // BASIC default
end;

function TExecutorContext.GetVariableAsDouble(const Name: string): Double; inline;
begin
  Result := GetVariableAsFloat(Name);  // Alias for clarity
end;

function TExecutorContext.GetVariableAsString(const Name: string): string; inline;
var
  V: Variant;
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if FVariables.TryGetData(CanonicalName, V) then
    Result := VarToStr(V)  // Direct conversion
  else
    Result := '';  // BASIC default for strings
end;

function TExecutorContext.GetVariableAsBool(const Name: string): Boolean; inline;
var
  V: Variant;
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  if FVariables.TryGetData(CanonicalName, V) then
  begin
    // BASIC semantics: 0 = false, non-zero = true
    if VarIsNumeric(V) then
      Result := (Double(V) <> 0.0)
    else if VarIsStr(V) then
      Result := (VarToStr(V) <> '')
    else
      Result := False;
  end
  else
    Result := False;  // BASIC default
end;

// TIER 4: Unified type + value access - single hash lookup
// Eliminates double lookup overhead in EvaluateIdentifier
function TExecutorContext.GetVariableTyped(const Name: string; out VarType: TBasicVariableType): Variant;
var
  CanonicalName: string;
begin
  // Infer type from name suffix (no lookup needed)
  VarType := InferVariableType(Name);

  // Single hash lookup for value
  CanonicalName := InternString(Name);
  if not FVariables.TryGetData(CanonicalName, Result) then
  begin
    // Variable not found - return BASIC defaults
    case VarType of
      bvtInteger: Result := Int64(0);
      bvtSingle, bvtDouble, bvtNumber: Result := Double(0.0);
      bvtString: Result := '';
      else Result := Null;
    end;
  end;
end;

procedure TExecutorContext.SetVariableInt(const Name: string; Value: Int64); inline;
var
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  // FAST: Int64 -> Variant is compiler intrinsic, no overhead
  FVariables[CanonicalName] := Value;
end;

procedure TExecutorContext.SetVariableFloat(const Name: string; Value: Double); inline;
var
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  // FAST: Double -> Variant is compiler intrinsic
  FVariables[CanonicalName] := Value;
end;

procedure TExecutorContext.SetVariableDouble(const Name: string; Value: Double); inline;
begin
  SetVariableFloat(Name, Value);  // Alias
end;

procedure TExecutorContext.SetVariableString(const Name: string; const Value: string); inline;
var
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  // FAST: String -> Variant is compiler intrinsic
  FVariables[CanonicalName] := Value;
end;

procedure TExecutorContext.SetVariableBool(const Name: string; Value: Boolean); inline;
var
  CanonicalName: string;
begin
  CanonicalName := InternString(Name);  // TIER 3: String interning
  // Store as integer (BASIC convention: 0 or -1)
  if Value then
    FVariables[CanonicalName] := Int64(-1)
  else
    FVariables[CanonicalName] := Int64(0);
end;

// === TIER 2: INDEXED VARIABLE ACCESS (FASTEST!) ===
// These methods provide direct array-indexed access to variables
// Used by the bytecode VM for maximum performance - ZERO hash lookups, ZERO string ops

// LEGACY: Old method for backward compatibility (keeps Variant storage)
procedure TExecutorContext.AllocateIndexedStorage(VarCount, ArrayCount: Integer);
var
  i: Integer;
begin
  // Allocate indexed variable storage (LEGACY - uses Variants)
  SetLength(FIndexedVariables, VarCount);
  for i := 0 to VarCount - 1 do
    FIndexedVariables[i] := 0;  // BASIC default

  // Allocate indexed array storage
  SetLength(FIndexedArrays, ArrayCount);
  for i := 0 to ArrayCount - 1 do
    FIndexedArrays[i] := nil;
end;

// PHASE 3: NEW typed variable storage allocator (ZERO Variant overhead!)
procedure TExecutorContext.AllocateIndexedStorage(VarTable: TObject; ArrayCount: Integer);
var
  VTable: TVariableTable;
  VarCount, I: Integer;
  IntCount, FloatCount, StrCount: Integer;
  VarName: string;
  VarType: TBasicVariableType;
begin
  // Cast TObject to TVariableTable (avoids circular dependency)
  VTable := TVariableTable(VarTable);
  VarCount := VTable.Count;

  // Initialize type map
  SetLength(FVarTypeMap, VarCount);
  IntCount := 0;
  FloatCount := 0;
  StrCount := 0;

  // First pass: count variables by type
  for I := 0 to VarCount - 1 do
  begin
    VarName := VTable[I];
    VarType := InferVariableType(VarName);

    case VarType of
      bvtInteger:
        begin
          FVarTypeMap[I].VarType := bvtInteger;
          FVarTypeMap[I].ArrayIndex := IntCount;
          Inc(IntCount);
        end;
      bvtSingle, bvtDouble, bvtNumber:
        begin
          // All numeric types map to Double storage
          FVarTypeMap[I].VarType := bvtDouble;
          FVarTypeMap[I].ArrayIndex := FloatCount;
          Inc(FloatCount);
        end;
      bvtString:
        begin
          FVarTypeMap[I].VarType := bvtString;
          FVarTypeMap[I].ArrayIndex := StrCount;
          Inc(StrCount);
        end;
      else
        // Unknown/Array types: fallback to Float storage
        FVarTypeMap[I].VarType := bvtNumber;
        FVarTypeMap[I].ArrayIndex := FloatCount;
        Inc(FloatCount);
    end;
  end;

  // Allocate typed arrays
  SetLength(FIntVariables, IntCount);
  SetLength(FFloatVariables, FloatCount);
  SetLength(FStringVariables, StrCount);

  // Initialize to BASIC defaults (0, 0.0, "")
  for I := 0 to IntCount - 1 do
    FIntVariables[I] := 0;
  for I := 0 to FloatCount - 1 do
    FFloatVariables[I] := 0.0;
  for I := 0 to StrCount - 1 do
    FStringVariables[I] := '';

  // Allocate legacy Variant storage (for untyped variables, if needed)
  SetLength(FIndexedVariables, VarCount);
  for I := 0 to VarCount - 1 do
    FIndexedVariables[I] := 0;

  // Allocate indexed array storage
  SetLength(FIndexedArrays, ArrayCount);
  for I := 0 to ArrayCount - 1 do
    FIndexedArrays[I] := nil;
end;

// PHASE 3: Variant access (for compatibility) - wraps typed storage
function TExecutorContext.GetVariableByIndex(VarIndex: Integer): Variant; inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        Result := FIntVariables[TypeInfo.ArrayIndex];
      bvtDouble, bvtNumber:
        Result := FFloatVariables[TypeInfo.ArrayIndex];
      bvtString:
        Result := FStringVariables[TypeInfo.ArrayIndex];
      else
        Result := 0;
    end;
  end
  else
    Result := 0;  // BASIC default
end;

procedure TExecutorContext.SetVariableByIndex(VarIndex: Integer; const Value: Variant); inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        FIntVariables[TypeInfo.ArrayIndex] := VarAsType(Value, varInt64);
      bvtDouble, bvtNumber:
        FFloatVariables[TypeInfo.ArrayIndex] := VarAsType(Value, varDouble);
      bvtString:
        FStringVariables[TypeInfo.ArrayIndex] := VarToStr(Value);
    end;
  end;
end;

// PHASE 3: TYPED variable access (NO Variant overhead!)
function TExecutorContext.GetVariableByIndexInt(VarIndex: Integer): Integer; inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        Result := FIntVariables[TypeInfo.ArrayIndex];
      bvtDouble, bvtNumber:
        Result := Trunc(FFloatVariables[TypeInfo.ArrayIndex]);  // Auto-convert
      bvtString:
        Result := StrToIntDef(FStringVariables[TypeInfo.ArrayIndex], 0);  // Auto-convert
      else
        Result := 0;  // Type mismatch
    end;
  end
  else
    Result := 0;
end;

procedure TExecutorContext.SetVariableByIndexInt(VarIndex: Integer; Value: Integer); inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        FIntVariables[TypeInfo.ArrayIndex] := Value;
      bvtDouble, bvtNumber:
        FFloatVariables[TypeInfo.ArrayIndex] := Value;  // Auto-convert
      bvtString:
        FStringVariables[TypeInfo.ArrayIndex] := IntToStr(Value);  // Auto-convert
    end;
  end;
end;

// PHASE 3: TYPED variable access (NO Variant overhead!)
function TExecutorContext.GetVariableByIndexFloat(VarIndex: Integer): Double; inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        Result := FIntVariables[TypeInfo.ArrayIndex];  // Auto-convert
      bvtDouble, bvtNumber:
        Result := FFloatVariables[TypeInfo.ArrayIndex];
      bvtString:
        Result := StrToFloatDef(FStringVariables[TypeInfo.ArrayIndex], 0.0);  // Auto-convert
      else
        Result := 0.0;  // Type mismatch
    end;
  end
  else
    Result := 0.0;
end;

procedure TExecutorContext.SetVariableByIndexFloat(VarIndex: Integer; Value: Double); inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        FIntVariables[TypeInfo.ArrayIndex] := Trunc(Value);  // Auto-convert
      bvtDouble, bvtNumber:
        FFloatVariables[TypeInfo.ArrayIndex] := Value;
      bvtString:
        FStringVariables[TypeInfo.ArrayIndex] := FloatToStr(Value);  // Auto-convert
    end;
  end;
end;

// PHASE 3: TYPED variable access - String (NO Variant overhead!)
function TExecutorContext.GetVariableByIndexStr(VarIndex: Integer): string; inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        Result := IntToStr(FIntVariables[TypeInfo.ArrayIndex]);  // Auto-convert
      bvtDouble, bvtNumber:
        Result := FloatToStr(FFloatVariables[TypeInfo.ArrayIndex]);  // Auto-convert
      bvtString:
        Result := FStringVariables[TypeInfo.ArrayIndex];
      else
        Result := '';  // Type mismatch
    end;
  end
  else
    Result := '';  // BASIC default for strings
end;

procedure TExecutorContext.SetVariableByIndexStr(VarIndex: Integer; const Value: string); inline;
var
  TypeInfo: TVarTypeInfo;
begin
  if (VarIndex >= 0) and (VarIndex < Length(FVarTypeMap)) then
  begin
    TypeInfo := FVarTypeMap[VarIndex];
    case TypeInfo.VarType of
      bvtInteger:
        FIntVariables[TypeInfo.ArrayIndex] := StrToInt64Def(Value, 0);  // Auto-convert
      bvtDouble, bvtNumber:
        FFloatVariables[TypeInfo.ArrayIndex] := StrToFloatDef(Value, 0.0);  // Auto-convert
      bvtString:
        FStringVariables[TypeInfo.ArrayIndex] := Value;
    end;
  end;
end;

function TExecutorContext.GetArrayElementByIndex(ArrayIndex, ElementIndex: Integer): Variant; inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    Result := FIndexedArrays[ArrayIndex].GetElement1D(ElementIndex)
  else
    Result := 0;  // BASIC default
end;

procedure TExecutorContext.SetArrayElementByIndex(ArrayIndex, ElementIndex: Integer; const Value: Variant); inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    FIndexedArrays[ArrayIndex].SetElement1D(ElementIndex, Value);
end;

procedure TExecutorContext.DimensionArrayByIndex(ArrayIndex: Integer; const Dimensions: TArrayDimensions);
begin
  // Default: use Variant storage (backward compatible)
  DimensionArrayByIndex(ArrayIndex, Dimensions, astVariant);
end;

procedure TExecutorContext.DimensionArrayByIndex(ArrayIndex: Integer; const Dimensions: TArrayDimensions; StorageType: TArrayStorageType);
var
  AdjustedDimensions: TArrayDimensions;
  i: Integer;
begin
  if (ArrayIndex < 0) or (ArrayIndex >= Length(FIndexedArrays)) then
    Exit;

  // Free existing array if present
  if Assigned(FIndexedArrays[ArrayIndex]) then
    FIndexedArrays[ArrayIndex].Free;

  // PHASE 4F: Adjust dimensions based on language semantics
  SetLength(AdjustedDimensions, Length(Dimensions));

  case FArrayIndexMode of
    aimMaxIndex:
      // Commodore BASIC style: DIM A(10) creates 11 elements (indices 0..10)
      for i := 0 to Length(Dimensions) - 1 do
        AdjustedDimensions[i] := Dimensions[i] + 1;

    aimElementCount:
      // Modern language style: DIM A(10) creates 10 elements (indices 0..9)
      for i := 0 to Length(Dimensions) - 1 do
        AdjustedDimensions[i] := Dimensions[i];
  end;

  // SESSION 14.5: TYPED ARRAYS with arena allocation support
  // Try arena first for numeric types, fallback to heap if arena full
  FIndexedArrays[ArrayIndex] := TBasicArray.Create(AdjustedDimensions, StorageType, FArena);
end;

// FAST: Typed indexed array access (direct native arrays, no Variant overhead)
function TExecutorContext.GetArrayElementByIndexInt64(ArrayIndex, ElementIndex: Integer): Int64; inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    Result := FIndexedArrays[ArrayIndex].GetElementInt64(ElementIndex)
  else
    Result := 0;
end;

procedure TExecutorContext.SetArrayElementByIndexInt64(ArrayIndex, ElementIndex: Integer; Value: Int64); inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    FIndexedArrays[ArrayIndex].SetElementInt64(ElementIndex, Value);
end;

function TExecutorContext.GetArrayElementByIndexDouble(ArrayIndex, ElementIndex: Integer): Double; inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    Result := FIndexedArrays[ArrayIndex].GetElementDouble(ElementIndex)
  else
    Result := 0.0;
end;

procedure TExecutorContext.SetArrayElementByIndexDouble(ArrayIndex, ElementIndex: Integer; Value: Double); inline;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
    FIndexedArrays[ArrayIndex].SetElementDouble(ElementIndex, Value);
end;

// SESSION 14.3: UNCHECKED array access - NO bounds check for loop unrolling!
// SAFETY: Caller MUST guarantee ArrayIndex and ElementIndex are valid
function TExecutorContext.GetArrayElementByIndexInt64Unchecked(ArrayIndex, ElementIndex: Integer): Int64; inline;
begin
  // NO BOUNDS CHECK - direct access for maximum performance in unrolled loops
  Result := FIndexedArrays[ArrayIndex].GetElementInt64Unchecked(ElementIndex);
end;

procedure TExecutorContext.SetArrayElementByIndexInt64Unchecked(ArrayIndex, ElementIndex: Integer; Value: Int64); inline;
begin
  // NO BOUNDS CHECK - direct access for maximum performance in unrolled loops
  FIndexedArrays[ArrayIndex].SetElementInt64Unchecked(ElementIndex, Value);
end;

function TExecutorContext.GetArrayElementByIndexDoubleUnchecked(ArrayIndex, ElementIndex: Integer): Double; inline;
begin
  // NO BOUNDS CHECK - direct access for maximum performance in unrolled loops
  Result := FIndexedArrays[ArrayIndex].GetElementDoubleUnchecked(ElementIndex);
end;

procedure TExecutorContext.SetArrayElementByIndexDoubleUnchecked(ArrayIndex, ElementIndex: Integer; Value: Double); inline;
begin
  // NO BOUNDS CHECK - direct access for maximum performance in unrolled loops
  FIndexedArrays[ArrayIndex].SetElementDoubleUnchecked(ElementIndex, Value);
end;

// SESSION 14.6: Direct pointer access for pointer arithmetic optimization
function TExecutorContext.GetDataPtrInt64(ArrayIndex, ElementIndex: Integer): Pointer; inline;
var
  Arr: TBasicArray;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
  begin
    Arr := FIndexedArrays[ArrayIndex];
    // Return pointer to element in array
    if Arr.FArenaAllocated then
      Result := @PInt64(Arr.FArenaPtr)[ElementIndex]
    else if Arr.FStorageType = astInt64 then
      Result := @Arr.FDataInt64[ElementIndex]
    else if Arr.FStorageType = astDouble then
      Result := @Arr.FDataDouble[ElementIndex]  // Allow access to Double arrays as Int64
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TExecutorContext.GetDataPtrDouble(ArrayIndex, ElementIndex: Integer): Pointer; inline;
var
  Arr: TBasicArray;
begin
  if (ArrayIndex >= 0) and (ArrayIndex < Length(FIndexedArrays)) and Assigned(FIndexedArrays[ArrayIndex]) then
  begin
    Arr := FIndexedArrays[ArrayIndex];
    // Return pointer to element in array
    if Arr.FArenaAllocated then
      Result := @PDouble(Arr.FArenaPtr)[ElementIndex]
    else if Arr.FStorageType = astDouble then
      Result := @Arr.FDataDouble[ElementIndex]
    else if Arr.FStorageType = astInt64 then
      Result := @Arr.FDataInt64[ElementIndex]  // Allow access to Int64 arrays as Double
    else
      Result := nil;
  end
  else
    Result := nil;
end;

end.
