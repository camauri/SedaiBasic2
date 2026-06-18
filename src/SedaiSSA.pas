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
unit SedaiSSA;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

// Include shared optimization flags - modified to force recompilation
{$I OptimizationFlags.inc}
{$I DebugFlags.inc}

interface

uses
  Classes, SysUtils, StrUtils, Variants, Math, Generics.Collections,
  SedaiLexerTypes, SedaiLexerToken, SedaiParserTypes, SedaiAST,
  SedaiSSATypes;

type
  { Loop info for FOR/NEXT implementation }
  TLoopInfo = record
    VarName: string;
    VarReg: TSSAValue;
    EndValue: TSSAValue;
    StepValue: TSSAValue;
    StepIsNegative: Boolean;  // True if STEP is known to be negative at compile time
    NeedRuntimeCheck: Boolean;  // True if STEP direction determined at runtime
    CondLabel: string;          // Primary condition label (or LE label if runtime check)
    CondLabelGE: string;        // GE label if NeedRuntimeCheck, empty otherwise
    BodyLabel: string;
    EndLabel: string;
  end;

  { User-defined function info for DEF FN }
  TUserFunctionDef = record
    Name: string;           // Function name (without FN prefix)
    ParamName: string;      // Parameter variable name
    BodyNode: TASTNode;     // AST node for the function body expression
  end;

  { UDT/record type info (M3). Each field maps to a (bank, slot) within the record block. }
  TUDTField = record
    Name: string;
    Bank: TSSARegisterType;
    Slot: Integer;          // index within that bank's slot array of the instance
    NestedType: string;     // UDT type name if this field is itself a record (else ''); held as an int handle
  end;
  TUDTType = record
    Name: string;
    Fields: array of TUDTField;
    NInt, NFloat, NStr: Integer;   // per-bank slot counts (for bcRecordNew)
    Parent: string;                // M4.2: base type name (EXTENDS), or '' — single inheritance
    Node: TASTNode;                // the antTypeDecl (to fill fields on demand, parent-first)
    Filled: Boolean;               // M4.2: fields resolved (cycle-safe fill guard)
  end;

  { SSA Generator - converts AST to SSA IR }
  TSSAGenerator = class
  private
    FProgram: TSSAProgram;
    FCurrentBlock: TSSABasicBlock;
    FLabelCounter: Integer;
    FCurrentLineNumber: Integer;  // Current BASIC line number being processed
    FLastResult: TSSAValue;    // Last expression result
    FVarMap: TStringList;      // Maps variable name to register index (stored as object)
    FLoopStack: array of TLoopInfo;  // Stack for nested loops
    FUserFunctions: specialize TDictionary<string, TUserFunctionDef>;  // User-defined functions (DEF FN)
    FConstFloatRegs: specialize TDictionary<Integer, Double>;   // Maps float register to constant value
    FConstIntRegs: specialize TDictionary<Integer, Integer>;    // Maps int register to constant value
    // SUB/FUNCTION declarations (M2). Bodies are NOT lowered at their definition point
    // (they must not run as part of module flow); they are collected here and lowered
    // after the module's END, each into its own block region reachable only via ssaCallSub.
    FDeferredProcs: array of TASTNode;
    FProcedureNames: TStringList;  // UPPERCASE names of declared SUB/FUNCTION (call resolution)
    FProcDecls: specialize TDictionary<string, TASTNode>;  // name -> antProcedureDecl (param info)
    // Context while lowering a procedure body (M2): used to lower "fname = expr" / RETURN expr
    // (FUNCTION result) and EXIT SUB/FUNCTION / RETURN to a frame return.
    FInProcedure: Boolean;
    FCurrentProcName: string;
    FCurrentProcRetType: TSSARegisterType;
    FCurrentProcIsFunction: Boolean;
    FCurrentThisType: string;   // M4.1: owner UDT type while lowering a method body (THIS's type)
    // UDT/record support (M3)
    FUDTs: array of TUDTType;            // declared record types
    FVarRecordType: TStringList;         // var name (UPPER) -> UDT type name (UPPER)
    FVarExplicitType: TStringList;       // var name (UPPER) -> TSSARegisterType (Objects[]) for DIM..AS
    FArrayRecordType: TStringList;       // array name (UPPER) -> element UDT type name (UPPER)
    FNeededDispatchers: TStringList;     // M4.3: "TYPE|METHOD" pairs needing a virtual dispatcher

    function GenerateUniqueLabel(const Prefix: string): string;
    function GetVariableType(const VarName: string): TSSARegisterType;
    function GetOrAllocateVariable(const VarName: string): TSSAValue;
    procedure PreAllocateVariables(Node: TASTNode);  // Pre-scan AST to allocate all variable registers
    procedure PreProcessData(Node: TASTNode);  // Pre-scan AST to collect all DATA statements first
    procedure ProcessStatement(Node: TASTNode);
    // SUB/FUNCTION (M2): pre-scan the AST to register all declarations up front (so CALL
    // sites resolve parameter info even for procedures defined later); collect a declaration
    // for deferred lowering; lower a CALL site; lower all bodies after the module END.
    procedure PreCollectProcedures(Node: TASTNode);
    procedure CollectProcedureDecl(Node: TASTNode);
    procedure ProcessProcedureCall(Node: TASTNode);
    // Stage arguments into transfer slots and emit ssaCallSub (shared by CALL and FUNCTION).
    procedure EmitProcedureCall(const Name: string; ArgListNode: TASTNode);
    procedure StageCallArgs(const ParamOwnerName: string; ArgListNode: TASTNode);  // args -> xfer
    procedure EmitCallSubLabel(const LabelName: string);  // ssaCallSub(label) + block split
    // OOP virtual dispatch (M4.3)
    function IsSubtypeOf(const U, T: string): Boolean;
    function MethodNeedsDispatch(const TypeName, MethNm: string): Boolean;
    procedure GenerateDispatchers;
    procedure LowerDeferredProcedures;
    function ProcedureLabelName(const Name: string): string;
    // UDT/record support (M3)
    procedure RegisterUDTs(Node: TASTNode);        // pre-scan TYPE declarations (2 passes)
    procedure CollectUDTNames(Node: TASTNode);     // pass 1: register type names (empty)
    procedure FillUDTFields(Node: TASTNode);       // pass 2: fill fields (all names known)
    procedure FillOneUDT(Idx: Integer);            // fill one type's fields (parent-first)
    function ResolveMethodLabel(const TypeName, MethNm: string): string;  // walk inheritance
    procedure RegisterRecordVars(Node: TASTNode);  // pre-scan DIM..AS (record/explicit-typed vars)
    procedure RegisterTypedVar(const VarName, TypeName: string);  // record var or explicit-bank var
    function VarRecordTypeName(const VarName: string): string;    // '' if not a record var
    procedure EmitRecordInit(const HandleVal: TSSAValue; UDTIdx: Integer);  // alloc nested records
    procedure EmitConstructorCall(const HandleVal: TSSAValue; const TypeName: string;
                                  ArgsNode: TASTNode = nil);  // M4.4 (ArgsNode: M4.4b ctor args)
    procedure EmitRecordCopy(const DestHandle, SrcHandle: TSSAValue; UDTIdx: Integer);  // value-copy
    function FindUDT(const TypeName: string): Integer;        // -1 if not a UDT
    function UDTFieldBankSlot(UDTIdx: Integer; const FieldName: string;
                              out Bank: TSSARegisterType; out Slot: Integer;
                              out NestedType: string): Boolean;
    function TypeNameToBank(const TypeName, FieldName: string): TSSARegisterType;
    procedure ProcessMemberAccess(Node: TASTNode; out Result: TSSAValue);  // read rec.field
    procedure ProcessMemberStore(MemberNode, ExprNode: TASTNode);          // rec.field = expr
    // Resolve a member-access object (a record variable or an array-of-UDT element) to its
    // handle register and UDT type name. False if it is not a record object.
    function ResolveRecordObject(ObjNode: TASTNode; out HandleVal: TSSAValue;
                                 out TypeName: string): Boolean;
    // OOP (M4.1): UDT type name of an object expression (no code emitted); method dispatch.
    function ObjectTypeName(ObjNode: TASTNode): string;
    procedure ProcessMethodCall(ObjNode: TASTNode; const ObjType, MethNm: string;
                                ArgsNode: TASTNode; out Result: TSSAValue);
    // Map parameter at Index (within ParamList) to its transfer-bank type and per-bank slot.
    function ParamBankAndSlot(ParamList: TASTNode; Index: Integer; out RT: TSSARegisterType): Integer;
    procedure EmitXferStore(RT: TSSARegisterType; Slot: Integer; const Val: TSSAValue);
    procedure EmitXferLoad(RT: TSSARegisterType; Slot: Integer; const DestReg: TSSAValue);
    procedure FixForwardReferences;  // PHASE 3 TIER 3: Fix forward GOTO/GOSUB references
    procedure ProcessExpression(Node: TASTNode; out Result: TSSAValue); overload;
    procedure ProcessExpression(Node: TASTNode; out Result: TSSAValue; const DestHint: TSSAValue); overload;
    procedure ProcessAssignment(Node: TASTNode);
    procedure ProcessArrayStore(Node: TASTNode);
    procedure ProcessPrint(Node: TASTNode);
    procedure ProcessInput(Node: TASTNode);
    procedure ProcessDim(Node: TASTNode);
    procedure ProcessForLoop(Node: TASTNode);
    procedure ProcessDoLoop(Node: TASTNode);
    procedure ProcessBlock(Node: TASTNode);
    procedure ProcessDefFn(Node: TASTNode);
    procedure ProcessNext(Node: TASTNode);
    procedure ProcessIfStatement(Node: TASTNode);
    procedure ProcessGoto(Node: TASTNode);
    procedure ProcessGosub(Node: TASTNode);
    procedure ProcessOnGoto(Node: TASTNode);
    // Block label for a GOTO/GOSUB target: 'LABEL_<NAME>' for a named label
    // (identifier, case-insensitive), 'LINE_<n>' for a classic line number.
    function JumpLabelName(LabelNode: TASTNode): string;
    procedure ProcessOnGosub(Node: TASTNode);
    procedure ProcessBox(Node: TASTNode);
    procedure ProcessCircle(Node: TASTNode);
    procedure ProcessDraw(Node: TASTNode);
    procedure ProcessLocate(Node: TASTNode);
    procedure ProcessGraphics(Node: TASTNode);
    procedure ProcessScnClr(Node: TASTNode);
    procedure ProcessColor(Node: TASTNode);
    procedure ProcessSetColor(Node: TASTNode);
    procedure ProcessWidth(Node: TASTNode);
    procedure ProcessScale(Node: TASTNode);
    procedure ProcessPaint(Node: TASTNode);
    procedure ProcessWindow(Node: TASTNode);
    procedure ProcessSShape(Node: TASTNode);
    procedure ProcessGShape(Node: TASTNode);
    procedure ProcessGList(Node: TASTNode);
    procedure ProcessPLoad(Node: TASTNode);
    procedure ProcessPSave(Node: TASTNode);
    procedure ProcessPRst(Node: TASTNode);
    // Sound commands
    procedure ProcessVol(Node: TASTNode);
    procedure ProcessSound(Node: TASTNode);
    procedure ProcessEnvelope(Node: TASTNode);
    procedure ProcessTempo(Node: TASTNode);
    procedure ProcessPlay(Node: TASTNode);
    procedure ProcessFilter(Node: TASTNode);
    // DATA/READ/RESTORE
    procedure ProcessData(Node: TASTNode);
    procedure ProcessRead(Node: TASTNode);
    procedure ProcessRestore(Node: TASTNode);
    // Input commands
    procedure ProcessGet(Node: TASTNode);
    procedure ProcessGetkey(Node: TASTNode);
    // Formatted output
    procedure ProcessPrintUsing(Node: TASTNode);
    procedure ProcessPudef(Node: TASTNode);
    procedure ProcessChar(Node: TASTNode);
    // File operations
    procedure ProcessLoad(Node: TASTNode);
    procedure ProcessSave(Node: TASTNode);
    procedure ProcessVerify(Node: TASTNode);
    procedure ProcessBload(Node: TASTNode);
    procedure ProcessBsave(Node: TASTNode);
    procedure ProcessBoot(Node: TASTNode);
    // Disk file I/O
    procedure ProcessDopen(Node: TASTNode);
    procedure ProcessDclose(Node: TASTNode);
    // File data I/O
    procedure ProcessGetFile(Node: TASTNode);
    procedure ProcessInputFile(Node: TASTNode);
    procedure ProcessPrintFile(Node: TASTNode);
    procedure ProcessCmd(Node: TASTNode);
    procedure ProcessAppend(Node: TASTNode);
    procedure ProcessDclear(Node: TASTNode);
    procedure ProcessRecord(Node: TASTNode);
    // Sprite commands
    procedure ProcessSprite(Node: TASTNode);
    procedure ProcessMovspr(Node: TASTNode);
    procedure ProcessSprcolor(Node: TASTNode);
    procedure ProcessSprsav(Node: TASTNode);
    procedure ProcessCollision(Node: TASTNode);
    procedure ProcessSprdef(Node: TASTNode);
    procedure ProcessSprsave(Node: TASTNode);
    procedure ProcessSprload(Node: TASTNode);
    procedure ProcessSprsize(Node: TASTNode);
    procedure ProcessSprform(Node: TASTNode);
    // System commands
    procedure ProcessRun(Node: TASTNode);
    procedure ProcessList(Node: TASTNode);
    procedure ProcessNew(Node: TASTNode);
    procedure ProcessDelete(Node: TASTNode);
    procedure ProcessRenumber(Node: TASTNode);
    procedure ProcessCatalog(Node: TASTNode);
    // File management commands (executed directly in VM)
    procedure ProcessCopyFile(Node: TASTNode);
    procedure ProcessScratch(Node: TASTNode);
    procedure ProcessRenameFile(Node: TASTNode);
    procedure ProcessConcat(Node: TASTNode);
    procedure ProcessMkdir(Node: TASTNode);
    procedure ProcessChdir(Node: TASTNode);
    procedure ProcessMoveFile(Node: TASTNode);
    {$IFDEF WEB_MODE}
    procedure ProcessWebCommand(Node: TASTNode);
    {$ENDIF}
    procedure EmitInstruction(OpCode: TSSAOpCode; Dest, Src1, Src2, Src3: TSSAValue);

    // Constant register tracking helpers
    procedure RecordConstantRegister(const Val: TSSAValue);
    function TryGetConstantFloat(RegIndex: Integer; out ConstValue: Double): Boolean;
    function TryGetConstantInt(RegIndex: Integer; out ConstValue: Integer): Boolean;
    procedure InvalidateRegister(RegIndex: Integer; RegType: TSSARegisterType);

    // Type conversion helpers
    function EnsureIntRegister(const Val: TSSAValue): TSSAValue;
    function EnsureFloatRegister(const Val: TSSAValue): TSSAValue;
    function EnsureStringRegister(const Val: TSSAValue): TSSAValue;
    // Instruction emission with immediate
    procedure EmitInstructionWithImmediate(Op: TSSAOpCode; const Dest, Src1, Src2: TSSAValue; ImmediateValue: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    function Generate(AST: TASTNode): TSSAProgram;
    property Program_: TSSAProgram read FProgram;
  end;

implementation

{$IFDEF DEBUG_SSA}
uses SedaiDebug;
{$ENDIF}

const
  // Transfer-register slot reserved for a FUNCTION's return value (per bank). Kept well
  // above any parameter slot count so it never collides with an argument slot.
  XFER_RESULT_SLOT = 255;

constructor TSSAGenerator.Create;
begin
  inherited Create;
  FProgram := nil;
  FCurrentBlock := nil;
  FLabelCounter := 0;
  FCurrentLineNumber := 0;
  FLastResult := MakeSSAValue(svkNone);
  FVarMap := TStringList.Create;
  FVarMap.Sorted := True;
  FVarMap.Duplicates := dupIgnore;
  FVarMap.CaseSensitive := False;  // BASIC is case-insensitive
  SetLength(FLoopStack, 0);
  FUserFunctions := specialize TDictionary<string, TUserFunctionDef>.Create;
  FConstFloatRegs := specialize TDictionary<Integer, Double>.Create;
  FConstIntRegs := specialize TDictionary<Integer, Integer>.Create;
  SetLength(FDeferredProcs, 0);
  FProcedureNames := TStringList.Create;
  FProcedureNames.CaseSensitive := False;
  FProcDecls := specialize TDictionary<string, TASTNode>.Create;
  SetLength(FUDTs, 0);
  FVarRecordType := TStringList.Create;
  FVarRecordType.CaseSensitive := False;
  FVarExplicitType := TStringList.Create;
  FVarExplicitType.CaseSensitive := False;
  FArrayRecordType := TStringList.Create;
  FArrayRecordType.CaseSensitive := False;
  FNeededDispatchers := TStringList.Create;
  FNeededDispatchers.Duplicates := dupIgnore;
  FNeededDispatchers.Sorted := True;
end;

destructor TSSAGenerator.Destroy;
begin
  FVarMap.Free;
  FUserFunctions.Free;
  FConstFloatRegs.Free;
  FConstIntRegs.Free;
  FProcedureNames.Free;
  FProcDecls.Free;
  FVarRecordType.Free;
  FVarExplicitType.Free;
  FArrayRecordType.Free;
  FNeededDispatchers.Free;
  inherited Destroy;
end;

function TSSAGenerator.GenerateUniqueLabel(const Prefix: string): string;
begin
  Inc(FLabelCounter);
  Result := Format('%s_%d', [Prefix, FLabelCounter]);
end;

function TSSAGenerator.GetVariableType(const VarName: string): TSSARegisterType;
var
  LastChar: Char;
  Idx: Integer;
begin
  if Length(VarName) = 0 then Exit(srtFloat);
  // Explicit type from DIM..AS (M3) wins over the name-suffix convention (record vars are
  // int handles; builtin-typed vars use their declared bank).
  if Assigned(FVarExplicitType) then
  begin
    Idx := FVarExplicitType.IndexOf(UpperCase(VarName));
    if Idx >= 0 then
      Exit(TSSARegisterType(PtrInt(FVarExplicitType.Objects[Idx])));
  end;
  LastChar := VarName[Length(VarName)];
  if LastChar = '$' then
    Result := srtString
  else if LastChar = '%' then
    Result := srtInt
  else
    Result := srtFloat;  // Default BASIC type
end;

function TSSAGenerator.GetOrAllocateVariable(const VarName: string): TSSAValue;
var
  Idx: Integer;
  RegType: TSSARegisterType;
  RegIndex: Integer;
  PackedInfo: PtrInt;
begin
  Idx := FVarMap.IndexOf(VarName);
  if Idx >= 0 then
  begin
    // Variable already allocated
    // Packing: RegType in bits 16-23, RegIndex in bits 0-15 (supports 0-65535)
    PackedInfo := PtrInt(FVarMap.Objects[Idx]);
    RegType := TSSARegisterType(PackedInfo shr 16);
    RegIndex := PackedInfo and $FFFF;  // 16-bit mask for 65536 registers
    Result := MakeSSARegister(RegType, RegIndex);
  end
  else
  begin
    // Allocate new register for this variable
    RegType := GetVariableType(VarName);
    RegIndex := FProgram.AllocRegister(RegType);
    // Pack RegType in upper bits, RegIndex in lower 16 bits
    PackedInfo := (Ord(RegType) shl 16) or RegIndex;
    FVarMap.AddObject(VarName, TObject(PackedInfo));
    FProgram.AddVariable(VarName);
    // Map variable to register in TSSAProgram for optimization passes
    FProgram.MapVariableToRegister(VarName, RegType, RegIndex);
    Result := MakeSSARegister(RegType, RegIndex);
  end;
end;

{ PreAllocateVariables - Scan AST to find all BASIC variables and allocate registers
  This separates variable registers from temporary expression registers, preventing conflicts }
procedure TSSAGenerator.PreAllocateVariables(Node: TASTNode);
var
  i: Integer;
  VarName: string;
begin
  if Node = nil then Exit;

  // TYPE declarations (M3) contain field/type identifiers that are NOT variables — don't
  // descend into them (would otherwise allocate bogus registers for field/type names).
  if Node.NodeType = antTypeDecl then Exit;

  case Node.NodeType of
    antAssignment:
    begin
      // Left side is the variable being assigned
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);  // Pre-allocate register for this variable
      end;
    end;

    antIdentifier:
    begin
      // This is a variable reference - allocate if not already done
      VarName := VarToStr(Node.Value);
      GetOrAllocateVariable(VarName);
    end;

    antForLoop:
    begin
      // FOR loop variable
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);
      end;
    end;

    antInput:
    begin
      // INPUT variable
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);
      end;
    end;

    antRead:
    begin
      // READ variables - each child is a variable to read into
      for i := 0 to Node.ChildCount - 1 do
      begin
        if Node.GetChild(i).NodeType = antIdentifier then
        begin
          VarName := VarToStr(Node.GetChild(i).Value);
          GetOrAllocateVariable(VarName);
        end;
      end;
    end;

    antGet, antGetkey:
    begin
      // GET/GETKEY variable - must be pre-allocated like other input commands
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        VarName := VarToStr(Node.GetChild(0).Value);
        GetOrAllocateVariable(VarName);
      end;
    end;
  end;

  // Recursively scan all children
  for i := 0 to Node.ChildCount - 1 do
    PreAllocateVariables(Node.GetChild(i));
end;

{ PreProcessData - Scan AST to find all DATA statements and emit them first
  This ensures the DATA pool is populated before any READ is executed,
  regardless of where DATA statements appear in the source code }
procedure TSSAGenerator.PreProcessData(Node: TASTNode);
var
  i: Integer;
  Child: TASTNode;
begin
  if Node = nil then Exit;

  // If this is a DATA statement, process it now
  if Node.NodeType = antData then
  begin
    ProcessData(Node);
    Exit;
  end;

  // Recursively scan all children
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    // For container nodes (program, line numbers, statements), check their children for DATA
    if (Child.NodeType = antLineNumber) or (Child.NodeType = antProgram) or
       (Child.NodeType = antStatement) then
      PreProcessData(Child)
    else if Child.NodeType = antData then
      ProcessData(Child);
  end;
end;

procedure TSSAGenerator.EmitInstruction(OpCode: TSSAOpCode; Dest, Src1, Src2, Src3: TSSAValue);
var
  Instr: TSSAInstruction;
begin
  // CRITICAL: After GOTO/RETURN, FCurrentBlock can be nil (block is terminated)
  // Don't emit instructions after block termination
  if not Assigned(FCurrentBlock) then
    Exit;

  Instr := TSSAInstruction.Create(OpCode);
  Instr.Dest := Dest;
  Instr.Src1 := Src1;
  Instr.Src2 := Src2;
  Instr.Src3 := Src3;
  // Always emit SourceLine for error reporting and TRON trace
  Instr.SourceLine := FCurrentLineNumber;
  FCurrentBlock.AddInstruction(Instr);
end;

// Overload without hint - creates empty hint internally
procedure TSSAGenerator.ProcessExpression(Node: TASTNode; out Result: TSSAValue);
var
  EmptyHint: TSSAValue;
begin
  EmptyHint.Kind := svkNone;
  EmptyHint.RegType := srtFloat;
  EmptyHint.RegIndex := 0;
  ProcessExpression(Node, Result, EmptyHint);
end;

// Main implementation with destination hint
procedure TSSAGenerator.ProcessExpression(Node: TASTNode; out Result: TSSAValue; const DestHint: TSSAValue);
var
  Left, Right: TSSAValue;
  DestReg: Integer;
  OpCode: TSSAOpCode;
  FuncName, VarName: string;
  FuncRetType: TSSARegisterType;  // M2: user FUNCTION return type
  MethodObjNode: TASTNode;        // M4.1: object of a method call
  MethodOwnerType, MethodLabelName: string;  // M4.1
  ArgValue, ArgReg: TSSAValue;
  TempReg, IntReg: Integer;
  IntRegVal, TempVal: TSSAValue;
  ArgListNode: TASTNode;
  TempFloat: Double;
  TempInt: Integer;
  TempStr: string;
  ValCode: Integer;
  // Array access variables
  ArrName: string;
  ArrayIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  IndicesNode: TASTNode;
  Indices: array of TSSAValue;
  i, j: Integer;
  ArrayRef: TSSAValue;
  UseHint: Boolean;
  // Linear index calculation for N-D arrays
  LinearIndex, StrideVal, MulResult, AddResult: TSSAValue;
  Stride: Int64;
  // RGBA function handling
  RVal, GVal, BVal, AVal: TSSAValue;
  RGBAResult: Int64;
  RReg, GReg, BReg, AReg: TSSAValue;
  // String function handling
  Arg2Value, Arg3Value, Arg2Reg, Arg3Reg: TSSAValue;
  // User function handling
  FnDef: TUserFunctionDef;
  OldParamValue: TSSAValue;
begin
  if Node = nil then
  begin
    Result := MakeSSAValue(svkNone);
    Exit;
  end;

  case Node.NodeType of
    antLiteral:
    begin
      if VarIsNumeric(Node.Value) then
      begin
        if Frac(Double(Node.Value)) = 0 then
          Result := MakeSSAConstInt(Int64(Node.Value))
        else
          Result := MakeSSAConstFloat(Double(Node.Value));
      end
      else
      begin
        // Try to convert string to number (handles cases like file handle "1")
        TempStr := VarToStr(Node.Value);
        TempFloat := 0;
        Val(TempStr, TempFloat, ValCode);
        if ValCode = 0 then
        begin
          // Successfully parsed as number
          if Frac(TempFloat) = 0 then
            Result := MakeSSAConstInt(Int64(Trunc(TempFloat)))
          else
            Result := MakeSSAConstFloat(TempFloat);
        end
        else
          Result := MakeSSAConstString(TempStr);
      end;
    end;

    antIdentifier:
    begin
      // Return the register assigned to this variable
      VarName := VarToStr(Node.Value);
      Result := GetOrAllocateVariable(VarName);
    end;

    antSpecialVariable:
    begin
      // Handle special system variables: TI, TI$, DT$
      VarName := UpperCase(VarToStr(Node.Value));
      if VarName = 'TI' then
      begin
        // TI returns jiffies (1/60 sec) since interpreter start - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadTI, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'TI$' then
      begin
        // TI$ returns current time as HHMMSS string
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaLoadTIS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'DT$' then
      begin
        // DT$ returns current date as YYYYMMDD string
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaLoadDTS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'CWD$' then
      begin
        // CWD$ returns current working directory
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaLoadCWDS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'EL' then
      begin
        // EL returns last error line number - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadEL, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'ER' then
      begin
        // ER returns last error code - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadER, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'ERR$' then
      begin
        // ERR$ (variable form) returns last error message - string
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaLoadERRS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        Result := MakeSSAValue(svkNone);  // Unknown special variable
    end;

    antUserFunction:
    begin
      // Handle user-defined functions (FN)
      // Child[0] = Function name (identifier)
      // Child[1] = Argument expression
      if Node.ChildCount >= 2 then
      begin
        // Get function name
        FuncName := UpperCase(VarToStr(Node.GetChild(0).Value));

        // Look up the function definition
        if FUserFunctions.ContainsKey(FuncName) then
        begin
          FnDef := FUserFunctions[FuncName];

          // Evaluate the argument
          ProcessExpression(Node.GetChild(1), ArgValue);

          // Convert argument to float if needed (DEF FN uses float math)
          ArgReg := EnsureFloatRegister(ArgValue);

          // Save the current value of the local parameter variable (if it exists)
          // and bind the argument to it
          OldParamValue := GetOrAllocateVariable(FnDef.ParamName);

          // Store argument into the parameter variable by copying the register
          EmitInstruction(ssaCopyFloat, OldParamValue, ArgReg,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));

          // Evaluate the function body expression
          ProcessExpression(FnDef.BodyNode, Result);

          // Restore the old parameter value (optional - C128 doesn't do this)
          // For now, we leave the parameter with its new value like C128 does
        end
        else
        begin
          // Function not defined - return 0
          Result := MakeSSAConstFloat(0);
        end;
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antParentheses:
    begin
      // Parentheses just contain an expression - evaluate it
      if Node.ChildCount > 0 then
        ProcessExpression(Node.GetChild(0), Result)
      else
        Result := MakeSSAValue(svkNone);
    end;

    antUnaryOp:
    begin
      // Handle unary operators (negation and NOT)
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), Left);

        // Check if this is NOT (bitwise) or negation (-)
        if Node.Token.TokenType = ttBitwiseNOT then
        begin
          // Bitwise NOT - always works with integers
          // Fold constant at compile time
          if Left.Kind = svkConstInt then
          begin
            Result := MakeSSAConstInt(not Left.ConstInt);
          end
          else if Left.Kind = svkConstFloat then
          begin
            // Convert float constant to int, then NOT
            Result := MakeSSAConstInt(not Trunc(Left.ConstFloat));
          end
          else
          begin
            // Convert float to int if needed
            if Left.RegType = srtFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              TempVal := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Left := TempVal;
            end;
            // Apply bitwise NOT
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaBitwiseNot, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
        end
        else if Node.Token.TokenType = ttOpAdd then
        begin
          // Unary plus - just return the operand unchanged
          Result := Left;
        end
        else
        begin
          // Negation (-)
          // ALWAYS fold negation of constants (this is basic code generation, not optimization)
          // Generating "LoadConst + Neg + Copy" for "-1.16" is wasteful even in unoptimized code
          if Left.Kind = svkConstFloat then
          begin
            // Negate float constant at compile time
            Result := MakeSSAConstFloat(-Left.ConstFloat);
          end
          else if Left.Kind = svkConstInt then
          begin
            // Negate int constant at compile time
            Result := MakeSSAConstInt(-Left.ConstInt);
          end
          else
          begin
            // When constant folding disabled, must materialize constants first
            if Left.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtFloat);
              TempVal := MakeSSARegister(srtFloat, TempReg);
              EmitInstruction(ssaLoadConstFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Left := TempVal;
            end
            else if Left.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              TempVal := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Left := TempVal;
            end;

            // Now apply negation to register
            if Left.RegType = srtFloat then
            begin
              DestReg := FProgram.AllocRegister(srtFloat);
              Result := MakeSSARegister(srtFloat, DestReg);
              EmitInstruction(ssaNegFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
            begin
              // Int register
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);
              EmitInstruction(ssaNegInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end;
          end;
        end;
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antBinaryOp:
    begin
      ProcessExpression(Node.GetChild(0), Left);
      ProcessExpression(Node.GetChild(1), Right);

      // PHASE 3 TIER 3: Unwrap register-held constants for constant folding
      // If Left is a register holding a constant, treat it as a constant
      if (Left.Kind = svkRegister) and (Left.RegType = srtFloat) then
      begin
        if TryGetConstantFloat(Left.RegIndex, TempFloat) then
          Left := MakeSSAConstFloat(TempFloat);
      end
      else if (Left.Kind = svkRegister) and (Left.RegType = srtInt) then
      begin
        if TryGetConstantInt(Left.RegIndex, TempInt) then
          Left := MakeSSAConstInt(TempInt);
      end;

      // Same for Right operand
      if (Right.Kind = svkRegister) and (Right.RegType = srtFloat) then
      begin
        if TryGetConstantFloat(Right.RegIndex, TempFloat) then
          Right := MakeSSAConstFloat(TempFloat);
      end
      else if (Right.Kind = svkRegister) and (Right.RegType = srtInt) then
      begin
        if TryGetConstantInt(Right.RegIndex, TempInt) then
          Right := MakeSSAConstInt(TempInt);
      end;

      // ALWAYS perform basic algebraic simplification (even with optimizations disabled)
      // This is not an "optimization" but correct code generation - we shouldn't generate
      // instructions for operations that can be resolved at compile time

      // Case 1: Both operands are constants - fold completely
      if (Left.Kind = svkConstFloat) and (Right.Kind = svkConstFloat) then
      begin
        case Node.Token.TokenType of
          ttOpAdd: Result := MakeSSAConstFloat(Left.ConstFloat + Right.ConstFloat);
          ttOpSub: Result := MakeSSAConstFloat(Left.ConstFloat - Right.ConstFloat);
          ttOpMul: Result := MakeSSAConstFloat(Left.ConstFloat * Right.ConstFloat);
          ttOpDiv:
            if Right.ConstFloat <> 0.0 then
              Result := MakeSSAConstFloat(Left.ConstFloat / Right.ConstFloat)
            else
              Result := MakeSSAValue(svkNone);  // Let runtime handle div by zero
        else
          Result := MakeSSAValue(svkNone);
        end;

        if Result.Kind <> svkNone then
          Exit;  // Done - constant folded!
      end
      else if (Left.Kind = svkConstInt) and (Right.Kind = svkConstInt) then
      begin
        case Node.Token.TokenType of
          ttOpAdd: Result := MakeSSAConstInt(Left.ConstInt + Right.ConstInt);
          ttOpSub: Result := MakeSSAConstInt(Left.ConstInt - Right.ConstInt);
          ttOpMul: Result := MakeSSAConstInt(Left.ConstInt * Right.ConstInt);
          ttOpDiv:
            if Right.ConstInt <> 0 then
              Result := MakeSSAConstInt(Left.ConstInt div Right.ConstInt)
            else
              Result := MakeSSAValue(svkNone);
        else
          Result := MakeSSAValue(svkNone);
        end;

        if Result.Kind <> svkNone then
          Exit;  // Done - constant folded!
      end;

      // Case 2: One operand is constant with algebraic identity
      // Mul by 0 = 0, Mul by 1 = other, Add 0 = other, etc.
      if (Left.Kind = svkConstFloat) or (Right.Kind = svkConstFloat) then
      begin
        case Node.Token.TokenType of
          ttOpMul:
          begin
            // 0 * x = 0, x * 0 = 0
            if ((Left.Kind = svkConstFloat) and (Left.ConstFloat = 0.0)) or
               ((Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0)) then
            begin
              Result := MakeSSAConstFloat(0.0);
              Exit;
            end;
            // 1 * x = x, x * 1 = x
            if (Left.Kind = svkConstFloat) and (Left.ConstFloat = 1.0) then
            begin
              Result := Right;
              Exit;
            end;
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 1.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
          ttOpAdd:
          begin
            // 0 + x = x, x + 0 = x
            if (Left.Kind = svkConstFloat) and (Left.ConstFloat = 0.0) then
            begin
              Result := Right;
              Exit;
            end;
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
          ttOpSub:
          begin
            // x - 0 = x
            if (Right.Kind = svkConstFloat) and (Right.ConstFloat = 0.0) then
            begin
              Result := Left;
              Exit;
            end;
          end;
        end;
      end;

      // Materialize constants into registers
      if Left.Kind in [svkConstInt, svkConstFloat, svkConstString] then
      begin
        if Left.Kind = svkConstInt then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          TempVal := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstIntRegs.AddOrSetValue(TempReg, Left.ConstInt);  // Track constant value
          Left := TempVal;
        end
        else if Left.Kind = svkConstFloat then
        begin
          TempReg := FProgram.AllocRegister(srtFloat);
          TempVal := MakeSSARegister(srtFloat, TempReg);
          EmitInstruction(ssaLoadConstFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstFloatRegs.AddOrSetValue(TempReg, Left.ConstFloat);  // Track constant value
          Left := TempVal;
        end
        else if Left.Kind = svkConstString then
        begin
          TempReg := FProgram.AllocRegister(srtString);
          TempVal := MakeSSARegister(srtString, TempReg);
          EmitInstruction(ssaLoadConstString, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Left := TempVal;
        end;
      end;

      // Type coercion: convert int to float when mixing types FIRST
      // Check if we need type conversion and fold constants during conversion
      if (Left.RegType = srtFloat) and (Right.Kind = svkConstInt) then
      begin
        // Constant folding: int constant will become float, skip LoadConstInt
        Right := MakeSSAConstFloat(Double(Right.ConstInt));
      end
      else if (Left.Kind = svkConstInt) and (Right.RegType = srtFloat) then
      begin
        // Constant folding: int constant will become float, skip LoadConstInt
        Left := MakeSSAConstFloat(Double(Left.ConstInt));
      end;

      // Load constants into registers (after type coercion check)
      if Right.Kind in [svkConstInt, svkConstFloat, svkConstString] then
      begin
        if Right.Kind = svkConstInt then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          TempVal := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstIntRegs.AddOrSetValue(TempReg, Right.ConstInt);  // Track constant value
          Right := TempVal;
        end
        else if Right.Kind = svkConstFloat then
        begin
          TempReg := FProgram.AllocRegister(srtFloat);
          TempVal := MakeSSARegister(srtFloat, TempReg);
          EmitInstruction(ssaLoadConstFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FConstFloatRegs.AddOrSetValue(TempReg, Right.ConstFloat);  // Track constant value
          Right := TempVal;
        end
        else if Right.Kind = svkConstString then
        begin
          TempReg := FProgram.AllocRegister(srtString);
          TempVal := MakeSSARegister(srtString, TempReg);
          EmitInstruction(ssaLoadConstString, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Right := TempVal;
        end;
      end;

      // Handle remaining type conversions (non-constant cases)
      if (Left.RegType = srtFloat) and (Right.RegType = srtInt) then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        TempVal := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Right := TempVal;
      end
      else if (Left.RegType = srtInt) and (Right.RegType = srtFloat) then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        TempVal := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Left := TempVal;
      end;

      // Check if this is a comparison operator (result is always Int boolean)
      case Node.Token.TokenType of
        ttOpEq, ttOpNeq, ttOpLt, ttOpGt, ttOpLe, ttOpGe:
        begin
          // Comparisons always return integer 0 or 1
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Determine comparison type based on operand types
          if (Left.RegType = srtString) or (Right.RegType = srtString) then
          begin
            // String comparison. There are no Le/Ge string opcodes, so synthesise
            // them: A<=B == NOT(A>B), A>=B == NOT(A<B). The comparison is emitted
            // with the Gt/Lt opcode here and the result is inverted just after the
            // shared emit below.
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqString;
              ttOpNeq: OpCode := ssaCmpNeString;
              ttOpLt: OpCode := ssaCmpLtString;
              ttOpGt: OpCode := ssaCmpGtString;
              ttOpLe: OpCode := ssaCmpGtString;   // inverted below
              ttOpGe: OpCode := ssaCmpLtString;   // inverted below
            else
              OpCode := ssaCmpEqString;
            end;
          end
          else if (Left.RegType = srtFloat) or (Right.RegType = srtFloat) then
          begin
            // Float comparison
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqFloat;
              ttOpNeq: OpCode := ssaCmpNeFloat;
              ttOpLt: OpCode := ssaCmpLtFloat;
              ttOpGt: OpCode := ssaCmpGtFloat;
              ttOpLe: OpCode := ssaCmpLeFloat;
              ttOpGe: OpCode := ssaCmpGeFloat;
            else
              OpCode := ssaCmpEqFloat;
            end;
          end
          else
          begin
            // Integer comparison
            case Node.Token.TokenType of
              ttOpEq: OpCode := ssaCmpEqInt;
              ttOpNeq: OpCode := ssaCmpNeInt;
              ttOpLt: OpCode := ssaCmpLtInt;
              ttOpGt: OpCode := ssaCmpGtInt;
              ttOpLe: OpCode := ssaCmpLeInt;
              ttOpGe: OpCode := ssaCmpGeInt;
            else
              OpCode := ssaCmpEqInt;
            end;
          end;

          if ((Left.RegType = srtString) or (Right.RegType = srtString)) and
             ((Node.Token.TokenType = ttOpLe) or (Node.Token.TokenType = ttOpGe)) then
          begin
            // String <= / >= : emit the Gt/Lt comparison into a temp, then set
            // Result := (temp = 0) to invert it. Each register is written once (SSA).
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(OpCode, TempVal, Left, Right, MakeSSAValue(svkNone));
            IntReg := FProgram.AllocRegister(srtInt);
            IntRegVal := MakeSSARegister(srtInt, IntReg);
            EmitInstruction(ssaLoadConstInt, IntRegVal, MakeSSAConstInt(0),
                           MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            EmitInstruction(ssaCmpEqInt, Result, TempVal, IntRegVal, MakeSSAValue(svkNone));
          end
          else
            EmitInstruction(OpCode, Result, Left, Right, MakeSSAValue(svkNone));
        end;

        // Bitwise operators (AND, OR, XOR) - result is always Int
        ttBitwiseAND, ttBitwiseOR, ttBitwiseXOR:
        begin
          // Bitwise operators always return integer
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Convert operands to int if needed (for bitwise evaluation)
          if Left.RegType = srtFloat then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaFloatToInt, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Left := TempVal;
          end;
          if Right.RegType = srtFloat then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaFloatToInt, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Right := TempVal;
          end;

          case Node.Token.TokenType of
            ttBitwiseAND: OpCode := ssaBitwiseAnd;
            ttBitwiseOR: OpCode := ssaBitwiseOr;
            ttBitwiseXOR: OpCode := ssaBitwiseXor;
          else
            OpCode := ssaBitwiseAnd;
          end;

          EmitInstruction(OpCode, Result, Left, Right, MakeSSAValue(svkNone));
        end;

      else
        // SPECIAL CASE: String concatenation (string + string or string + any)
        if (Node.Token.TokenType = ttOpAdd) and
           ((Left.RegType = srtString) or (Right.RegType = srtString)) then
        begin
          // String concatenation
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrConcat, Result, Left, Right, MakeSSAValue(svkNone));
        end
        // Arithmetic operators
        // SPECIAL CASE: Division always returns float (even for int/int)
        else if Node.Token.TokenType = ttOpDiv then
        begin
          // Convert both operands to float if needed
          if Left.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            TempVal := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Left := TempVal;
          end;
          if Right.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            TempVal := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Right := TempVal;
          end;

          // Result is always float for division
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);
          OpCode := ssaDivFloat;
        end
        // MOD: float if any operand is float, otherwise integer
        else if Node.Token.TokenType = ttOpMod then
        begin
          if (Left.RegType = srtFloat) or (Right.RegType = srtFloat) then
          begin
            // Float MOD: convert int operands to float if needed
            if Left.RegType = srtInt then
            begin
              TempReg := FProgram.AllocRegister(srtFloat);
              TempVal := MakeSSARegister(srtFloat, TempReg);
              EmitInstruction(ssaIntToFloat, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Left := TempVal;
            end;
            if Right.RegType = srtInt then
            begin
              TempReg := FProgram.AllocRegister(srtFloat);
              TempVal := MakeSSARegister(srtFloat, TempReg);
              EmitInstruction(ssaIntToFloat, TempVal, Right, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Right := TempVal;
            end;
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
            OpCode := ssaModFloat;
          end
          else
          begin
            // Integer MOD: both operands are already int
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            OpCode := ssaModInt;
          end;
        end
        // Determine result type and allocate register (use hint if compatible)
        else if (Left.RegType = srtFloat) or (Right.RegType = srtFloat) then
        begin
          // Check if we can use the destination hint
          UseHint := (DestHint.Kind = svkRegister) and (DestHint.RegType = srtFloat);
          if UseHint then
          begin
            Result := DestHint;  // Use the suggested destination
          end
          else
          begin
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
          end;
          case Node.Token.TokenType of
            ttOpAdd: OpCode := ssaAddFloat;
            ttOpSub: OpCode := ssaSubFloat;
            ttOpMul: OpCode := ssaMulFloat;
            ttOpPow: OpCode := ssaPowFloat;
          else
            OpCode := ssaAddFloat;
          end;
        end
        else
        begin
          // Check if we can use the destination hint
          UseHint := (DestHint.Kind = svkRegister) and (DestHint.RegType = srtInt);
          if UseHint then
          begin
            Result := DestHint;  // Use the suggested destination
          end
          else
          begin
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
          end;
          case Node.Token.TokenType of
            ttOpAdd: OpCode := ssaAddInt;
            ttOpSub: OpCode := ssaSubInt;
            ttOpMul: OpCode := ssaMulInt;
            ttOpMod: OpCode := ssaModInt;
          else
            OpCode := ssaAddInt;
          end;
        end;

        EmitInstruction(OpCode, Result, Left, Right, MakeSSAValue(svkNone));
      end;
    end;

    antFunctionCall:
    begin
      // Handle built-in functions
      if Node.ChildCount > 0 then
      begin
        FuncName := UpperCase(VarToStr(Node.Value));
        ArgListNode := Node.GetChild(0);

        // User-defined FUNCTION (M2): stage args, call, read the result transfer slot into
        // a fresh register of the function's return type (by name suffix).
        if FProcedureNames.IndexOf(FuncName) >= 0 then
        begin
          EmitProcedureCall(FuncName, ArgListNode);
          FuncRetType := GetVariableType(FuncName);
          DestReg := FProgram.AllocRegister(FuncRetType);
          Result := MakeSSARegister(FuncRetType, DestReg);
          EmitXferLoad(FuncRetType, XFER_RESULT_SLOT, Result);
          Exit;
        end;

        // Handle RGBA function specially (4 integer args -> 1 integer result)
        if FuncName = 'RGBA' then
        begin
          // RGBA(r, g, b, a) returns 32-bit integer color value
          // Result = (a << 24) | (r << 16) | (g << 8) | b
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 4) then
          begin
            // Get all 4 arguments as integers
            ProcessExpression(ArgListNode.GetChild(0), RVal);  // R
            ProcessExpression(ArgListNode.GetChild(1), GVal);  // G
            ProcessExpression(ArgListNode.GetChild(2), BVal);  // B
            ProcessExpression(ArgListNode.GetChild(3), AVal);  // A

            // Check if all arguments are constants - if so, compute at compile time
            if (RVal.Kind = svkConstInt) and (GVal.Kind = svkConstInt) and
               (BVal.Kind = svkConstInt) and (AVal.Kind = svkConstInt) then
            begin
              // Constant folding: compute RGBA at compile time
              RGBAResult := ((AVal.ConstInt and $FF) shl 24) or
                            ((RVal.ConstInt and $FF) shl 16) or
                            ((GVal.ConstInt and $FF) shl 8) or
                            (BVal.ConstInt and $FF);

              // Return as integer constant (will be converted to float by caller if needed)
              Result := MakeSSAConstInt(RGBAResult);
            end
            else if (RVal.Kind = svkConstFloat) and (GVal.Kind = svkConstFloat) and
                    (BVal.Kind = svkConstFloat) and (AVal.Kind = svkConstFloat) then
            begin
              // Constant folding for float constants: convert to int and compute
              RGBAResult := ((Trunc(AVal.ConstFloat) and $FF) shl 24) or
                            ((Trunc(RVal.ConstFloat) and $FF) shl 16) or
                            ((Trunc(GVal.ConstFloat) and $FF) shl 8) or
                            (Trunc(BVal.ConstFloat) and $FF);

              // Return as integer constant
              Result := MakeSSAConstInt(RGBAResult);
            end
            else
            begin
              // Non-constant arguments - use RGBA instruction with PhiSources
              // This path may have issues with optimization passes not updating PhiSources
              RReg := EnsureIntRegister(RVal);
              GReg := EnsureIntRegister(GVal);
              BReg := EnsureIntRegister(BVal);
              AReg := EnsureIntRegister(AVal);

              // Allocate result register (integer)
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);

              // Emit RGBA instruction: Dest=result, Src1=R, Src2=G, Src3=B, PhiSources[0]=A
              EmitInstruction(ssaGraphicRGBA, Result, RReg, GReg, BReg);
              FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1].AddPhiSource(AReg, nil);
            end;
          end
          else
            raise Exception.Create('RGBA requires 4 arguments: RGBA(r, g, b, a)');
        end
        else if FuncName = 'RDOT' then
        begin
          // RDOT(n) returns pixel cursor info: 0=x, 1=y, 2=color at PC
          // Returns integer value
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);

            // Convert argument to int register if needed
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;

            // Allocate result register (integer)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);

            // Emit ssaGraphicRdot: Dest=result, Src1=which (0/1/2)
            EmitInstruction(ssaGraphicRdot, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('RDOT requires 1 argument: RDOT(n) where n=0,1,2');
        end
        else if FuncName = 'RGR' then
        begin
          // RGR(n) returns graphics mode info
          // n=0: current graphics mode (0-11)
          // Future: n=1..n for other info
          // For now, RGR(0) or RGR() returns current mode

          // Check if we have an argument (optional, defaults to 0)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            // Convert argument to int register if needed
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
          end
          else
          begin
            // No argument - default to 0 (current mode)
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;

          // Allocate result register (integer)
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Emit ssaGraphicGetMode: Dest=result, Src1=which
          EmitInstruction(ssaGraphicGetMode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        // === SYSTEM FUNCTIONS ===
        else if FuncName = 'FRE' then
        begin
          // FRE(x) returns available memory in bytes
          // The argument is ignored (C64 compatibility - would select memory bank)
          // We always return physical available memory

          // Process argument (for syntax compatibility, but ignore value)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          // Argument is ignored - FRE always returns available RAM

          // Allocate result register (integer - returns bytes as Int64)
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Emit ssaFre: Dest=result
          EmitInstruction(ssaFre, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'PEEK' then
        begin
          // PEEK(address) returns value at memory-mapped address
          // Process the address argument
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          // Ensure address is an integer register
          ArgReg := EnsureIntRegister(ArgValue);

          // Allocate result register (integer - returns byte value 0-255)
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);

          // Emit ssaPeek: Dest=result, Src1=address
          EmitInstruction(ssaPeek, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        // === STRING FUNCTIONS ===
        else if (FuncName = 'LEN') then
        begin
          // LEN(str) - returns integer length
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaStrLen, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'ASC') then
        begin
          // ASC(str) - returns integer ASCII code of first char
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaStrAsc, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'CHR$') then
        begin
          // CHR$(n) - returns string char from ASCII code
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureIntRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrChr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'ERR$') then
        begin
          // ERR$(n) - returns error message for error code n
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureIntRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrErr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'STR$') then
        begin
          // STR$(n) - returns string from number
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureFloatRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrStr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'VAL') then
        begin
          // VAL(str) - returns float from string
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);
          EmitInstruction(ssaStrVal, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'HEX$') then
        begin
          // HEX$(n) - returns 4-char hex string
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureIntRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrHex, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'LEFT$') then
        begin
          // LEFT$(str, n) - returns leftmost n chars
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // string
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // count
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureIntRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtString);
            Result := MakeSSARegister(srtString, DestReg);
            EmitInstruction(ssaStrLeft, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'RIGHT$') then
        begin
          // RIGHT$(str, n) - returns rightmost n chars
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // string
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // count
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureIntRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtString);
            Result := MakeSSARegister(srtString, DestReg);
            EmitInstruction(ssaStrRight, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'MID$') then
        begin
          // MID$(str, start [,length]) - returns substring
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // string
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // start
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureIntRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtString);
            Result := MakeSSARegister(srtString, DestReg);
            // Length is optional - pass in Src3 (as constant or register)
            if ArgListNode.ChildCount >= 3 then
            begin
              ProcessExpression(ArgListNode.GetChild(2), Arg3Value);
              // Pass length as Src3 - can be constant or register
              Arg3Reg := EnsureIntRegister(Arg3Value);
              EmitInstruction(ssaStrMid, Result, ArgReg, Arg2Reg, Arg3Reg);
            end
            else
              // No length specified - use MaxInt for "rest of string"
              EmitInstruction(ssaStrMid, Result, ArgReg, Arg2Reg, MakeSSAConstInt(MaxInt));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'INSTR') then
        begin
          // INSTR(str1, str2 [,start]) - find str2 in str1, return position
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // str1
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // str2
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureStringRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            // Start position is optional
            if ArgListNode.ChildCount >= 3 then
            begin
              ProcessExpression(ArgListNode.GetChild(2), Arg3Value);
              if Arg3Value.Kind = svkConstInt then
                EmitInstructionWithImmediate(ssaStrInstr, Result, ArgReg, Arg2Reg, Arg3Value.ConstInt)
              else if Arg3Value.Kind = svkConstFloat then
                EmitInstructionWithImmediate(ssaStrInstr, Result, ArgReg, Arg2Reg, Trunc(Arg3Value.ConstFloat))
              else
                EmitInstructionWithImmediate(ssaStrInstr, Result, ArgReg, Arg2Reg, 1); // Default start=1
            end
            else
              EmitInstructionWithImmediate(ssaStrInstr, Result, ArgReg, Arg2Reg, 0); // 0 means use Pos (start from 1)
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'DEC') then
        begin
          // DEC(hexstring) - converts hex string to decimal integer
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaStrDec, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'LOGN') then
        begin
          // LOGN(base, x) - logarithm of x with base n
          // Result = ln(x) / ln(base) using LogN function in Pascal
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // base
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // x
            ArgReg := EnsureFloatRegister(ArgValue);
            Arg2Reg := EnsureFloatRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
            EmitInstruction(ssaMathLogN, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else
        begin
          // Standard math functions (single argument, float result)
          if (ArgListNode <> nil) then
          begin
            // If it's an argument list node, get first child
            if (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount > 0) then
              ProcessExpression(ArgListNode.GetChild(0), ArgValue)
            else
              // Otherwise, the node itself is the argument
              ProcessExpression(ArgListNode, ArgValue);
          end
          else
          begin
            // No arguments
            Result := MakeSSAValue(svkNone);
            Exit;
          end;

          // Convert argument to float register if needed
          if ArgValue.Kind = svkConstFloat then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaLoadConstFloat, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else if ArgValue.Kind = svkConstInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            IntReg := FProgram.AllocRegister(srtInt);
            IntRegVal := MakeSSARegister(srtInt, IntReg);
            EmitInstruction(ssaLoadConstInt, IntRegVal, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            EmitInstruction(ssaIntToFloat, ArgReg, IntRegVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else if ArgValue.RegType = srtInt then
          begin
            TempReg := FProgram.AllocRegister(srtFloat);
            ArgReg := MakeSSARegister(srtFloat, TempReg);
            EmitInstruction(ssaIntToFloat, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            ArgReg := ArgValue;

          // Allocate result register (float)
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);

          // Emit appropriate math function
          if FuncName = 'ABS' then
            OpCode := ssaMathAbs
          else if FuncName = 'SGN' then
            OpCode := ssaMathSgn
          else if FuncName = 'INT' then
            OpCode := ssaMathInt
          else if FuncName = 'SQR' then
            OpCode := ssaMathSqr
          else if FuncName = 'SIN' then
            OpCode := ssaMathSin
          else if FuncName = 'COS' then
            OpCode := ssaMathCos
          else if FuncName = 'TAN' then
            OpCode := ssaMathTan
          else if (FuncName = 'EXP') then
            OpCode := ssaMathExp
          else if (FuncName = 'LOG') or (FuncName = 'LN') then
            OpCode := ssaMathLog
          else if FuncName = 'LOG10' then
            OpCode := ssaMathLog10
          else if FuncName = 'LOG2' then
            OpCode := ssaMathLog2
          else if (FuncName = 'ATN') or (FuncName = 'ATAN') then
            OpCode := ssaMathAtn
          else if FuncName = 'RND' then
            OpCode := ssaMathRnd
          else
            OpCode := ssaNop;

          if OpCode <> ssaNop then
            EmitInstruction(OpCode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antGraphicsFunction:
    begin
      // Handle graphics functions: RGBA, RDOT, RGR
      // Same structure as antFunctionCall
      if Node.ChildCount > 0 then
      begin
        FuncName := UpperCase(VarToStr(Node.Value));
        ArgListNode := Node.GetChild(0);

        // Handle RGBA function specially (4 integer args -> 1 integer result)
        if FuncName = 'RGBA' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 4) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);
            ProcessExpression(ArgListNode.GetChild(1), GVal);
            ProcessExpression(ArgListNode.GetChild(2), BVal);
            ProcessExpression(ArgListNode.GetChild(3), AVal);

            if (RVal.Kind = svkConstInt) and (GVal.Kind = svkConstInt) and
               (BVal.Kind = svkConstInt) and (AVal.Kind = svkConstInt) then
            begin
              RGBAResult := ((AVal.ConstInt and $FF) shl 24) or
                            ((RVal.ConstInt and $FF) shl 16) or
                            ((GVal.ConstInt and $FF) shl 8) or
                            (BVal.ConstInt and $FF);
              Result := MakeSSAConstInt(RGBAResult);
            end
            else if (RVal.Kind = svkConstFloat) and (GVal.Kind = svkConstFloat) and
                    (BVal.Kind = svkConstFloat) and (AVal.Kind = svkConstFloat) then
            begin
              RGBAResult := ((Trunc(AVal.ConstFloat) and $FF) shl 24) or
                            ((Trunc(RVal.ConstFloat) and $FF) shl 16) or
                            ((Trunc(GVal.ConstFloat) and $FF) shl 8) or
                            (Trunc(BVal.ConstFloat) and $FF);
              Result := MakeSSAConstInt(RGBAResult);
            end
            else
            begin
              RReg := EnsureIntRegister(RVal);
              GReg := EnsureIntRegister(GVal);
              BReg := EnsureIntRegister(BVal);
              AReg := EnsureIntRegister(AVal);
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);
              EmitInstruction(ssaGraphicRGBA, Result, RReg, GReg, BReg);
              FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1].AddPhiSource(AReg, nil);
            end;
          end
          else
            raise Exception.Create('RGBA requires 4 arguments: RGBA(r, g, b, a)');
        end
        else if FuncName = 'RDOT' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaGraphicRdot, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('RDOT requires 1 argument: RDOT(n) where n=0,1,2');
        end
        else if FuncName = 'RGR' then
        begin
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            if ArgValue.Kind = svkConstInt then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if ArgValue.Kind = svkConstFloat then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
            begin
              TempReg := FProgram.AllocRegister(srtInt);
              ArgReg := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaFloatToInt, ArgReg, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end
            else
              ArgReg := ArgValue;
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGraphicGetMode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'POS' then
        begin
          // POS(x) - return cursor column position
          // Argument is a dummy, ignored
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGraphicPos, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'RCLR' then
        begin
          // RCLR(n) - return color of source n (0-6), 0-based result
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGraphicRclr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'GETCOLOR' then
        begin
          // GETCOLOR(n) - return color of source n (0-6), 0-based result
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGetColor, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'RWINDOW' then
        begin
          // RWINDOW(n) - return window info (0=lines, 1=cols, 2=screen width)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGraphicRwindow, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
          Result := MakeSSAValue(svkNone);
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antInputFunction:
    begin
      // Handle input functions: RWINDOW, POS, etc.
      if Node.ChildCount > 0 then
      begin
        FuncName := UpperCase(VarToStr(Node.Value));
        ArgListNode := Node.GetChild(0);

        if FuncName = 'RWINDOW' then
        begin
          // RWINDOW(n) - return window info (0=lines, 1=cols, 2=screen width)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
          end
          else
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            ArgReg := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaGraphicRwindow, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = 'POS' then
        begin
          // POS(x) - return cursor column position (argument is ignored, C128 compatibility)
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            // Process argument but ignore it (C128 compatibility)
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          end;
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          // POS uses dummy arg of 0
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          EmitInstruction(ssaGraphicPos, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
          Result := MakeSSAValue(svkNone);
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antSpriteFunction:
    begin
      // Handle sprite query functions: BUMP, RSPCOLOR, RSPPOS, RSPRITE
      // AST structure: antSpriteFunction with Value=FuncName
      //   Child 0: antArgumentList with 1-2 args
      // All return float values (BASIC variables are float by default)
      FuncName := Node.Attributes.Values['sprite_func'];
      if FuncName = '' then
        FuncName := UpperCase(VarToStr(Node.Value));

      if Node.ChildCount > 0 then
        ArgListNode := Node.GetChild(0)
      else
        ArgListNode := nil;

      if FuncName = 'BUMP' then
      begin
        // BUMP(n) - n: 1=sprite-sprite, 2=sprite-background
        if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
        begin
          ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          ArgReg := EnsureIntRegister(ArgValue);
        end
        else
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
        DestReg := FProgram.AllocRegister(srtFloat);
        Result := MakeSSARegister(srtFloat, DestReg);
        EmitInstruction(ssaBump, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if FuncName = 'RSPCOLOR' then
      begin
        // RSPCOLOR(n) - n: 1=MC1, 2=MC2
        if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
        begin
          ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          ArgReg := EnsureIntRegister(ArgValue);
        end
        else
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
        DestReg := FProgram.AllocRegister(srtFloat);
        Result := MakeSSARegister(srtFloat, DestReg);
        EmitInstruction(ssaRspcolor, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if FuncName = 'RSPPOS' then
      begin
        // RSPPOS(sprite, attr) - attr: 0=X, 1=Y, 2=speed
        if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
        begin
          ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          ArgReg := EnsureIntRegister(ArgValue);
          ProcessExpression(ArgListNode.GetChild(1), Arg2Value);
          Arg2Reg := EnsureIntRegister(Arg2Value);
        end
        else
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          TempReg := FProgram.AllocRegister(srtInt);
          Arg2Reg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, Arg2Reg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
        DestReg := FProgram.AllocRegister(srtFloat);
        Result := MakeSSARegister(srtFloat, DestReg);
        EmitInstruction(ssaRsppos, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
      end
      else if FuncName = 'RSPRITE' then
      begin
        // RSPRITE(sprite, attr) - attr: 0=enabled, 1=color, 2=priority, 3=scalex, 4=scaley, 5=mode
        if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
        begin
          ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          ArgReg := EnsureIntRegister(ArgValue);
          ProcessExpression(ArgListNode.GetChild(1), Arg2Value);
          Arg2Reg := EnsureIntRegister(Arg2Value);
        end
        else
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          TempReg := FProgram.AllocRegister(srtInt);
          Arg2Reg := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, Arg2Reg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
        DestReg := FProgram.AllocRegister(srtFloat);
        Result := MakeSSARegister(srtFloat, DestReg);
        EmitInstruction(ssaRsprite, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
      end
      else
        Result := MakeSSAValue(svkNone);
    end;

    antMemberAccess:
      // Record field read: obj.field (M3)
      ProcessMemberAccess(Node, Result);

    antArrayAccess:
    begin
      // Array element access: A(5) or M(I,J)
      // Structure: antArrayAccess
      //   antIdentifier: array name
      //   antExpressionList: indices

      if Node.ChildCount < 2 then
        begin
          Result := MakeSSAValue(svkNone);
          Exit;
        end;

        // Method call obj.method(args) (M4.1): child0 is a member access whose field is a
        // method of the object's type. Pass the object as the implicit THIS argument.
        if Node.GetChild(0).NodeType = antMemberAccess then
        begin
          MethodObjNode := Node.GetChild(0).GetChild(0);
          MethodOwnerType := ObjectTypeName(MethodObjNode);
          if MethodOwnerType <> '' then
          begin
            MethodLabelName := ResolveMethodLabel(MethodOwnerType, VarToStr(Node.GetChild(0).Value));
            if MethodLabelName <> '' then
            begin
              ProcessMethodCall(MethodObjNode, MethodOwnerType, VarToStr(Node.GetChild(0).Value),
                                Node.GetChild(1), Result);
              Exit;
            end;
          end;
        end;

        // First child is array name
        if Node.GetChild(0).NodeType <> antIdentifier then
        begin
          Result := MakeSSAValue(svkNone);
          Exit;
        end;

        ArrName := VarToStr(Node.GetChild(0).Value);

        // User-defined FUNCTION (M2): "name(args)" parses as array access, but if the name
        // is a declared FUNCTION it is a call. Stage args, call, read the result slot.
        if FProcedureNames.IndexOf(UpperCase(ArrName)) >= 0 then
        begin
          EmitProcedureCall(UpperCase(ArrName), Node.GetChild(1));
          FuncRetType := GetVariableType(ArrName);
          DestReg := FProgram.AllocRegister(FuncRetType);
          Result := MakeSSARegister(FuncRetType, DestReg);
          EmitXferLoad(FuncRetType, XFER_RESULT_SLOT, Result);
          Exit;
        end;

        ArrayIdx := FProgram.FindArray(ArrName);
        if ArrayIdx < 0 then
          raise Exception.CreateFmt('Array not declared: %s', [ArrName]);

        ArrInfo := FProgram.GetArray(ArrayIdx);
        IndicesNode := Node.GetChild(1);  // antExpressionList

        // Evaluate each index expression
        SetLength(Indices, IndicesNode.ChildCount);
        for i := 0 to IndicesNode.ChildCount - 1 do
        begin
          ProcessExpression(IndicesNode.GetChild(i), Indices[i]);

          // Materialize constants into int registers and convert float registers to int
          if Indices[i].Kind = svkConstInt then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end
          else if Indices[i].Kind = svkConstFloat then
          begin
            // Convert float constant to int
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(Trunc(Indices[i].ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end
          else if (Indices[i].Kind = svkRegister) and (Indices[i].RegType = srtFloat) then
          begin
            // Convert float register to int (FOR loop counters are float)
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaFloatToInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Indices[i] := TempVal;
          end;
        end;

        // Calculate linear index at compile-time using row-major order formula:
        // LinearIndex = i0 * (d1*d2*...*dn) + i1 * (d2*...*dn) + ... + i(n-1)
        // This eliminates the need for consecutive registers at runtime
        if Length(Indices) = 1 then
        begin
          // 1D array: linear index = first index
          LinearIndex := Indices[0];
        end
        else
        begin
          // N-D array: compute linear index
          // Start with index 0 (will accumulate the sum)
          TempReg := FProgram.AllocRegister(srtInt);
          LinearIndex := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaLoadConstInt, LinearIndex, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

          for i := 0 to High(Indices) do
          begin
            // Calculate stride for dimension i: product of dimensions i+1 to n-1
            // NOTE: ArrInfo.Dimensions[j] already contains the actual size (N+1 for DIM A(N))
            Stride := 1;
            for j := i + 1 to High(ArrInfo.Dimensions) do
              Stride := Stride * ArrInfo.Dimensions[j];

            if Stride = 1 then
            begin
              // Last dimension: just add the index
              TempReg := FProgram.AllocRegister(srtInt);
              AddResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaAddInt, AddResult, LinearIndex, Indices[i], MakeSSAValue(svkNone));
              LinearIndex := AddResult;
            end
            else
            begin
              // Multiply index by stride, then add to accumulator
              TempReg := FProgram.AllocRegister(srtInt);
              StrideVal := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

              TempReg := FProgram.AllocRegister(srtInt);
              MulResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));

              TempReg := FProgram.AllocRegister(srtInt);
              AddResult := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaAddInt, AddResult, LinearIndex, MulResult, MakeSSAValue(svkNone));
              LinearIndex := AddResult;
            end;
          end;
        end;

        // Allocate result register with array element type
        DestReg := FProgram.AllocRegister(ArrInfo.ElementType);
        Result := MakeSSARegister(ArrInfo.ElementType, DestReg);
        ArrayRef := MakeSSAArrayRef(ArrayIdx, ArrInfo.ElementType);

        // Emit ssaArrayLoad instruction with pre-computed linear index
        // Dest, ArrayRef, LinearIndex, None
        EmitInstruction(ssaArrayLoad, Result, ArrayRef, LinearIndex, MakeSSAValue(svkNone));
    end;

    {$IFDEF WEB_MODE}
    antWebVariable:
    begin
      // Web variables: METHOD$, PATH$, QUERY$ - no arguments
      FuncName := UpperCase(VarToStr(Node.Value));
      DestReg := FProgram.AllocRegister(srtString);
      Result := MakeSSARegister(srtString, DestReg);

      if FuncName = 'METHOD$' then
        EmitInstruction(ssaWebMethod, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if FuncName = 'PATH$' then
        EmitInstruction(ssaWebPath, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if FuncName = 'QUERY$' then
        EmitInstruction(ssaWebQuery, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        Result := MakeSSAValue(svkNone);
    end;

    antWebFunction:
    begin
      // Web functions: GET$, POST$, GETRAW$, POSTRAW$, HTML$, URL$, HEADER$
      FuncName := UpperCase(VarToStr(Node.Value));

      if Node.ChildCount > 0 then
      begin
        ArgListNode := Node.GetChild(0);

        // Functions with 1 argument: GET$, POST$, GETRAW$, POSTRAW$, HTML$, URL$, HEADER$
        if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
        begin
          ProcessExpression(ArgListNode.GetChild(0), ArgValue);
          ArgReg := EnsureStringRegister(ArgValue);
        end
        else
        begin
          // Empty string as default
          TempReg := FProgram.AllocRegister(srtString);
          ArgReg := MakeSSARegister(srtString, TempReg);
          EmitInstruction(ssaLoadConstString, ArgReg, MakeSSAConstString(''), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;

        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);

        if FuncName = 'GET$' then
          EmitInstruction(ssaWebGetParam, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'POST$' then
          EmitInstruction(ssaWebPostParam, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'GETRAW$' then
          EmitInstruction(ssaWebGetRaw, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'POSTRAW$' then
          EmitInstruction(ssaWebPostRaw, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'HTML$' then
          EmitInstruction(ssaWebHtmlEncode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'URL$' then
          EmitInstruction(ssaWebUrlEncode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if FuncName = 'HEADER$' then
          EmitInstruction(ssaWebHeader, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
          Result := MakeSSAValue(svkNone);
      end
      else
        Result := MakeSSAValue(svkNone);
    end;
    {$ENDIF}

  else
    Result := MakeSSAValue(svkNone);
  end;
end;

procedure TSSAGenerator.ProcessAssignment(Node: TASTNode);
var
  VarNode, ExprNode: TASTNode;
  VarName, LhsRecType, RhsRecType: string;
  ExprValue, VarReg, SrcRecHandle: TSSAValue;
  CopyOp: TSSAOpCode;
begin
  if Node.ChildCount < 2 then Exit;

  VarNode := Node.GetChild(0);
  ExprNode := Node.GetChild(1);

  // Record field store: obj.field = expr (M3)
  if VarNode.NodeType = antMemberAccess then
  begin
    ProcessMemberStore(VarNode, ExprNode);
    Exit;
  end;

  // Check if target is array access (array store)
  if VarNode.NodeType = antArrayAccess then
  begin
    ProcessArrayStore(Node);
    Exit;
  end;

  // Check if target is special variable (TI$ can be assigned)
  if VarNode.NodeType = antSpecialVariable then
  begin
    VarName := UpperCase(VarToStr(VarNode.Value));
    if VarName = 'TI$' then
    begin
      // TI$ = "HHMMSS" - set time offset
      ProcessExpression(ExprNode, ExprValue);
      // Ensure we have a string register
      if ExprValue.Kind = svkConstString then
      begin
        VarReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
        EmitInstruction(ssaLoadConstString, VarReg, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        ExprValue := VarReg;
      end;
      EmitInstruction(ssaStoreTIS, MakeSSAValue(svkNone), ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    // DT$ and TI are read-only - ignore assignments
    Exit;
  end;

  if VarNode.NodeType <> antIdentifier then Exit;
  VarName := VarToStr(VarNode.Value);

  // FUNCTION result (M2): "fname = expr" inside a FUNCTION body sets the return value.
  // Also accepts the unqualified method name inside a FUNCTION method (M4.1): for a method
  // PT.SUM, "SUM = expr" works (the qualified "PT.SUM = expr" would parse as member access).
  // Evaluate the expression and stage it into the result transfer slot (delivered to the
  // caller on bcReturnSub). QB semantics: execution continues; the actual return is at END.
  if FInProcedure and FCurrentProcIsFunction and
     ((UpperCase(VarName) = FCurrentProcName) or
      (Pos('.', FCurrentProcName) > 0) and
      (UpperCase(VarName) = Copy(FCurrentProcName, Pos('.', FCurrentProcName) + 1, MaxInt))) then
  begin
    ProcessExpression(ExprNode, ExprValue);
    EmitXferStore(FCurrentProcRetType, XFER_RESULT_SLOT, ExprValue);
    Exit;
  end;

  // Value-semantics record assignment (FreeBASIC): "a = b" between UDT variables copies b's fields
  // into a's own instance (memberwise, recursive for nested UDTs) — it does NOT alias the handle.
  // The source may be a record var, array element or member access (ResolveRecordObject handles
  // those); a function-call source returning a UDT is covered later (return-by-value increment).
  LhsRecType := VarRecordTypeName(VarName);
  if (LhsRecType <> '') and ResolveRecordObject(ExprNode, SrcRecHandle, RhsRecType) then
  begin
    VarReg := GetOrAllocateVariable(VarName);          // a's handle (own instance from its DIM)
    EmitRecordCopy(VarReg, SrcRecHandle, FindUDT(LhsRecType));
    Exit;
  end;

  // Get or allocate register for this variable
  VarReg := GetOrAllocateVariable(VarName);

  // Evaluate expression with destination hint to avoid unnecessary copies
  ProcessExpression(ExprNode, ExprValue, VarReg);

  // If expression result is a constant, load it directly to variable register
  if ExprValue.Kind in [svkConstInt, svkConstFloat, svkConstString] then
  begin
    // Check if type conversion is needed
    if (ExprValue.Kind = svkConstInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int constant to float constant at compile time and load directly
      // This eliminates LoadConstInt + IntToFloat pattern
      ExprValue := MakeSSAConstFloat(Double(ExprValue.ConstInt));
      EmitInstruction(ssaLoadConstFloat, VarReg, ExprValue,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Direct load
      case ExprValue.Kind of
        svkConstInt:
          EmitInstruction(ssaLoadConstInt, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        svkConstFloat:
          EmitInstruction(ssaLoadConstFloat, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        svkConstString:
          EmitInstruction(ssaLoadConstString, VarReg, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
  end
  else if (ExprValue.RegType <> VarReg.RegType) or (ExprValue.RegIndex <> VarReg.RegIndex) then
  begin
    // Expression result needs conversion or is in a different register
    // Type conversion takes priority over same-type copy
    if (ExprValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert INT register to FLOAT
      EmitInstruction(ssaIntToFloat, VarReg, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (ExprValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert FLOAT register to INT
      EmitInstruction(ssaFloatToInt, VarReg, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ExprValue.RegIndex <> VarReg.RegIndex then
    begin
      // Same type but different register - direct copy
      case VarReg.RegType of
        srtInt: CopyOp := ssaCopyInt;
        srtFloat: CopyOp := ssaCopyFloat;
        srtString: CopyOp := ssaCopyString;
      end;
      EmitInstruction(CopyOp, VarReg, ExprValue,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;
  // else: ExprValue is already in VarReg thanks to hint - no copy needed!
end;

procedure TSSAGenerator.ProcessPrint(Node: TASTNode);
var
  i: Integer;
  ExprValue, RegValue, ArgValue, ArgReg: TSSAValue;
  DestReg: Integer;
  Child, ArgNode: TASTNode;
  SeparatorChar, FuncName: string;
  EndsWithSeparator: Boolean;
begin
  // Handle empty PRINT statement (just newline)
  if Node.ChildCount = 0 then
  begin
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    // Reset reverse mode after PRINT (C128 behavior)
    EmitInstruction(ssaPrintEnd, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  // Check if PRINT ends with a separator (;  or ,)
  // This determines if we emit a final newline or not
  EndsWithSeparator := (Node.GetChild(Node.ChildCount - 1).NodeType = antSeparator);

  // Process each child
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    // Handle separator nodes - emit appropriate separator instruction
    if Child.NodeType = antSeparator then
    begin
      SeparatorChar := VarToStr(Child.Value);
      if SeparatorChar = ',' then
        EmitInstruction(ssaPrintComma, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if SeparatorChar = ';' then
        EmitInstruction(ssaPrintSemicolon, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Continue;
    end;

    // Handle TAB(n) and SPC(n) as special PRINT formatting functions
    // They are parsed as antFunctionCall nodes with function name TAB or SPC
    if Child.NodeType = antFunctionCall then
    begin
      FuncName := UpperCase(VarToStr(Child.Value));
      if (FuncName = 'TAB') or (FuncName = 'SPC') then
      begin
        // Get the argument (position/count)
        if Child.ChildCount > 0 then
        begin
          ArgNode := Child.GetChild(0);
          // Handle argument list node
          if (ArgNode.NodeType = antArgumentList) and (ArgNode.ChildCount > 0) then
            ProcessExpression(ArgNode.GetChild(0), ArgValue)
          else
            ProcessExpression(ArgNode, ArgValue);

          // Ensure we have an integer register
          ArgReg := EnsureIntRegister(ArgValue);

          // Emit the appropriate instruction
          if FuncName = 'TAB' then
            EmitInstruction(ssaPrintTab, MakeSSAValue(svkNone), ArgReg,
                           MakeSSAValue(svkNone), MakeSSAValue(svkNone))
          else // SPC
            EmitInstruction(ssaPrintSpc, MakeSSAValue(svkNone), ArgReg,
                           MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
        Continue;
      end;
    end;

    ProcessExpression(Child, ExprValue);

    // If expression is a constant, load it into a register first
    if ExprValue.Kind in [svkConstInt, svkConstFloat, svkConstString] then
    begin
      // Allocate register based on constant type
      DestReg := FProgram.AllocRegister(ExprValue.RegType);
      RegValue := MakeSSARegister(ExprValue.RegType, DestReg);

      // Emit load constant instruction
      case ExprValue.Kind of
        svkConstInt:
        begin
          EmitInstruction(ssaLoadConstInt, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
        svkConstFloat:
        begin
          EmitInstruction(ssaLoadConstFloat, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
        svkConstString:
        begin
          EmitInstruction(ssaLoadConstString, RegValue, ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ExprValue := RegValue;
        end;
      end;
    end;

    // Emit appropriate print instruction based on type
    case ExprValue.RegType of
      srtInt:
        EmitInstruction(ssaPrintInt, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtFloat:
        EmitInstruction(ssaPrint, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtString:
        EmitInstruction(ssaPrintString, MakeSSAValue(svkNone), ExprValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Emit newline ONLY if PRINT does NOT end with a separator
  // PRINT A     -> newline
  // PRINT A;    -> no newline
  // PRINT A,    -> no newline
  // PRINT A; B  -> newline (separator is between, not at end)
  if not EndsWithSeparator then
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Reset reverse mode after PRINT (C128 behavior: reverse only affects current PRINT)
  EmitInstruction(ssaPrintEnd, MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessInput(Node: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  PromptStr: string;
  PromptReg, VarReg: TSSAValue;
  PromptRegIdx: Integer;
  VarName: string;
  VarNames: array of string;
  VarCount: Integer;
  IsMultiple: Boolean;
begin
  // INPUT has structure: optional prompt string, separator, variable names
  // Examples:
  // INPUT "Enter value: "; N    -> prompt + single variable
  // INPUT N                      -> no prompt, single variable
  // INPUT A, B, C                -> no prompt, multiple variables
  // INPUT "Values"; A, B, C      -> prompt + multiple variables
  //
  // Commodore 128 behavior:
  // - Single variable: "? " on same line, cursor waits
  // - Multiple variables: each "?" on separate line, starting from NEXT line
  // - Prompt is printed before the "?" (on same line for single, then newline for multiple)

  PromptStr := '';
  SetLength(VarNames, 0);
  VarCount := 0;

  // Scan children to find prompt (if any) and all variable names
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    case Child.NodeType of
      antLiteral:
      begin
        // This is the prompt string
        PromptStr := VarToStr(Child.Value);
      end;
      antIdentifier:
      begin
        // This is a variable to store input - collect all of them
        Inc(VarCount);
        SetLength(VarNames, VarCount);
        VarNames[VarCount - 1] := VarToStr(Child.Value);
      end;
      antSeparator:
        // Skip separators
        Continue;
    end;
  end;

  // If we don't have any variable names, nothing to do
  if VarCount = 0 then Exit;

  IsMultiple := (VarCount > 1);

  // If there's a prompt, load it into a register (to be used by INPUT instruction)
  // The prompt is NOT printed separately - INPUT instruction handles it for retry support
  if PromptStr <> '' then
  begin
    PromptRegIdx := FProgram.AllocRegister(srtString);
    PromptReg := MakeSSARegister(srtString, PromptRegIdx);
    EmitInstruction(ssaLoadConstString, PromptReg, MakeSSAConstString(PromptStr),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    PromptReg := MakeSSAValue(svkNone);

  // Process each variable
  for i := 0 to VarCount - 1 do
  begin
    VarName := VarNames[i];

    // Get or allocate register for the target variable
    VarReg := GetOrAllocateVariable(VarName);

    // For multiple variables: newline only BEFORE the FIRST "?"
    // After first input, ReadLine already moves to new line after ENTER
    if IsMultiple and (i = 0) then
    begin
      EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    // Emit INPUT instruction based on variable type
    // Src1 contains the prompt register (for retry support), only for FIRST variable
    // The INPUT instruction itself will print prompt + "? "
    case VarReg.RegType of
      srtInt:
        if i = 0 then
          EmitInstruction(ssaInputInt, VarReg, PromptReg,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
          EmitInstruction(ssaInputInt, VarReg, MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtFloat:
        if i = 0 then
          EmitInstruction(ssaInputFloat, VarReg, PromptReg,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
          EmitInstruction(ssaInputFloat, VarReg, MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtString:
        if i = 0 then
          EmitInstruction(ssaInputString, VarReg, PromptReg,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
          EmitInstruction(ssaInputString, VarReg, MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;
end;

procedure TSSAGenerator.ProcessArrayStore(Node: TASTNode);
var
  TargetNode, ExprNode: TASTNode;
  ArrName: string;
  ArrayIdx: Integer;
  ArrInfo: TSSAArrayInfo;
  IndicesNode: TASTNode;
  Indices: array of TSSAValue;
  i, j, TempReg: Integer;
  ArrayRef, ExprValue, TempVal, LinearIndex, StrideVal, MulResult, AddResult: TSSAValue;
  FloatVal: Double;
  IntVal: Int64;
  Stride: Int64;
begin
  // Array store: A(5) = 10
  // Node structure: antAssignment
  //   antArrayAccess
  //     antIdentifier: array name
  //     antExpressionList: indices
  //   expression (value to store)

  if Node.ChildCount < 2 then Exit;

  TargetNode := Node.GetChild(0);  // antArrayAccess
  ExprNode := Node.GetChild(1);     // Value expression

  if TargetNode.NodeType <> antArrayAccess then Exit;
  if TargetNode.ChildCount < 2 then Exit;

  // Get array name
  if TargetNode.GetChild(0).NodeType <> antIdentifier then Exit;
  ArrName := VarToStr(TargetNode.GetChild(0).Value);

  // Find array in program
  ArrayIdx := FProgram.FindArray(ArrName);
  if ArrayIdx < 0 then
    raise Exception.CreateFmt('Array not declared: %s', [ArrName]);

  ArrInfo := FProgram.GetArray(ArrayIdx);
  IndicesNode := TargetNode.GetChild(1);  // antExpressionList

  // Evaluate each index expression
  SetLength(Indices, IndicesNode.ChildCount);
  for i := 0 to IndicesNode.ChildCount - 1 do
  begin
    ProcessExpression(IndicesNode.GetChild(i), Indices[i]);

    // Materialize constants into int registers and convert float registers to int
    if Indices[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if Indices[i].Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(Trunc(Indices[i].ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if (Indices[i].Kind = svkRegister) and (Indices[i].RegType = srtFloat) then
    begin
      // Convert float register to int (FOR loop counters are float)
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end;
  end;

  // Calculate linear index at compile-time using row-major order formula:
  // LinearIndex = i0 * (d1*d2*...*dn) + i1 * (d2*...*dn) + ... + i(n-1)
  // This eliminates the need for consecutive registers at runtime
  if Length(Indices) = 1 then
  begin
    // 1D array: linear index = first index
    LinearIndex := Indices[0];
  end
  else
  begin
    // N-D array: compute linear index
    // Start with index 0 (will accumulate the sum)
    TempReg := FProgram.AllocRegister(srtInt);
    LinearIndex := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, LinearIndex, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    for i := 0 to High(Indices) do
    begin
      // Calculate stride for dimension i: product of dimensions i+1 to n-1
      // NOTE: ArrInfo.Dimensions[j] already contains the actual size (N+1 for DIM A(N))
      Stride := 1;
      for j := i + 1 to High(ArrInfo.Dimensions) do
        Stride := Stride * ArrInfo.Dimensions[j];

      if Stride = 1 then
      begin
        // Last dimension: just add the index
        TempReg := FProgram.AllocRegister(srtInt);
        AddResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, Indices[i], MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end
      else
      begin
        // Multiply index by stride, then add to accumulator
        TempReg := FProgram.AllocRegister(srtInt);
        StrideVal := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

        TempReg := FProgram.AllocRegister(srtInt);
        MulResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));

        TempReg := FProgram.AllocRegister(srtInt);
        AddResult := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, MulResult, MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end;
    end;
  end;

  // Evaluate value expression
  ProcessExpression(ExprNode, ExprValue);

  {$IFNDEF DISABLE_CONSTANT_FOLDING}
  // OPTIMIZATION: Constant folding for type conversions
  // Convert constants at compile-time instead of emitting runtime instructions
  if ExprValue.Kind = svkConstInt then
  begin
    // Float array + Int constant → convert at compile-time
    if ArrInfo.ElementType = srtFloat then
    begin
      FloatVal := Double(ExprValue.ConstInt);
      ExprValue := MakeSSAValue(svkConstFloat);
      ExprValue.ConstFloat := FloatVal;
    end;
  end
  else if ExprValue.Kind = svkConstFloat then
  begin
    // Int array + Float constant → convert at compile-time
    if ArrInfo.ElementType = srtInt then
    begin
      IntVal := Trunc(ExprValue.ConstFloat);
      ExprValue := MakeSSAValue(svkConstInt);
      ExprValue.ConstInt := IntVal;
    end;
  end;
  {$ENDIF}

  // Materialize constant value into register
  if ExprValue.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    TempVal := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end
  else if ExprValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end
  else if ExprValue.Kind = svkConstString then
  begin
    TempReg := FProgram.AllocRegister(srtString);
    TempVal := MakeSSARegister(srtString, TempReg);
    EmitInstruction(ssaLoadConstString, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ExprValue := TempVal;
  end;

  // Type conversion: value must match array element type
  if ExprValue.Kind = svkRegister then
  begin
    // Float array + Int value → convert Int to Float
    if (ArrInfo.ElementType = srtFloat) and (ExprValue.RegType = srtInt) then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ExprValue := TempVal;
    end
    // Int array + Float value → convert Float to Int
    else if (ArrInfo.ElementType = srtInt) and (ExprValue.RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ExprValue := TempVal;
    end;
  end;

  ArrayRef := MakeSSAArrayRef(ArrayIdx, ArrInfo.ElementType);

  // Emit ssaArrayStore instruction with pre-computed linear index
  // Dest = value to store
  // Src1 = ArrayRef
  // Src2 = LinearIndex (single pre-computed index)
  // Src3 = None (no longer needed - linear index handles all dimensions)
  EmitInstruction(ssaArrayStore, ExprValue, ArrayRef, LinearIndex, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessDim(Node: TASTNode);
var
  ArrName: string;
  ElementType: TSSARegisterType;
  DimsNode, DimExpr, ArrayDeclNode: TASTNode;
  Dimensions: array of Integer;
  DimCount, i, j, ArrayIdx: Integer;
  DimValue: TSSAValue;
  DimValues: array of TSSAValue;  // Store dimension values (registers or constants)
  DimRegs: array of Integer;      // Register indices for variable dimensions
  DimRegTypes: array of TSSARegisterType;  // Register types for variable dimensions
  ArrayRef: TSSAValue;
  TotalElements: Int64;
  HasVariableDims: Boolean;
  DimInstr: TSSAInstruction;
  VarDimCount: Integer;
  RecTypeName: string;     // M3: DIM..AS type name
  RecUDTIdx: Integer;      // M3
  RecHandleVal: TSSAValue; // M3
  RecArrUDTIdx: Integer;   // M3.1: array-of-UDT element type index (-1 if not)
  RecPacked: Int64;        // M3.1: packed slot counts for bcRecordNewArray
const
  MAX_ARRAY_ELEMENTS = 125000000;  // 125M elements max (~1GB for 500x500x500 matrix)
begin
  // DIM can declare multiple arrays:
  // DIM A(10)                    -> single array
  // DIM A1(10), A2(10, 10)       -> multiple arrays
  // DIM C(5, 5, 5, 5, 5)         -> multidimensional (up to 13 dimensions like C128)

  if Node.ChildCount < 1 then Exit;

  // Parser generates AST structure:
  // antDim -> antArrayDecl(s)
  // Each antArrayDecl -> antIdentifier, antDimensions

  // Process all array declarations (support for multiple arrays in one DIM)
  for j := 0 to Node.ChildCount - 1 do
  begin
    ArrayDeclNode := Node.GetChild(j);

    // Only process antArrayDecl nodes (modern AST format)
    if ArrayDeclNode.NodeType <> antArrayDecl then
      Continue;

    // antArrayDecl contains identifier and dimensions
    if ArrayDeclNode.ChildCount < 2 then Continue;
    if ArrayDeclNode.GetChild(0).NodeType <> antIdentifier then Continue;

    ArrName := VarToStr(ArrayDeclNode.GetChild(0).Value);
    DimsNode := ArrayDeclNode.GetChild(1);

    // Typed scalar declaration (M3): "DIM name AS typename" — child[1] is an antIdentifier
    // (the type), not antDimensions. A UDT type allocates a record instance and stores its
    // handle in the variable; a builtin type needs no allocation (type already recorded).
    if DimsNode.NodeType = antIdentifier then
    begin
      RecTypeName := UpperCase(VarToStr(DimsNode.Value));
      RecUDTIdx := FindUDT(RecTypeName);
      if RecUDTIdx >= 0 then
      begin
        RecHandleVal := GetOrAllocateVariable(UpperCase(ArrName));
        EmitInstruction(ssaRecordNew, RecHandleVal,
                        MakeSSAConstInt(FUDTs[RecUDTIdx].NInt),
                        MakeSSAConstInt(FUDTs[RecUDTIdx].NFloat),
                        MakeSSAConstInt(FUDTs[RecUDTIdx].NStr or (Int64(RecUDTIdx) shl 32)));  // Imm: strCount | typeId<<32
        EmitRecordInit(RecHandleVal, RecUDTIdx);  // allocate any nested-UDT field instances
        // M4.4: run the constructor (if any). M4.4b: a "DIM v AS T(args)" attaches the ctor
        // argument list as child[2] (antArgumentList).
        if (ArrayDeclNode.ChildCount >= 3) and
           (ArrayDeclNode.GetChild(2).NodeType = antArgumentList) then
          EmitConstructorCall(RecHandleVal, RecTypeName, ArrayDeclNode.GetChild(2))
        else
          EmitConstructorCall(RecHandleVal, RecTypeName);
      end;
      Continue;
    end;

    // Array of UDT (M3.1): "DIM name(dims) AS udt" — child[2] is the element type. Such an
    // array stores record handles, so its element type is int.
    RecArrUDTIdx := -1;
    if (ArrayDeclNode.ChildCount >= 3) and (ArrayDeclNode.GetChild(2).NodeType = antIdentifier) then
      RecArrUDTIdx := FindUDT(UpperCase(VarToStr(ArrayDeclNode.GetChild(2).Value)));
    if RecArrUDTIdx >= 0 then
      ElementType := srtInt
    else
      // Determine element type from array name suffix
      ElementType := GetVariableType(ArrName);

    // Validate dimensions node
    if DimsNode.NodeType <> antDimensions then
      raise Exception.CreateFmt('Invalid array dimensions for: %s', [ArrName]);

    // Extract dimension sizes
    DimCount := DimsNode.ChildCount;

    // Safety check: must have at least one dimension
    if DimCount < 1 then
      raise Exception.CreateFmt('Array must have at least 1 dimension: %s', [ArrName]);

    SetLength(Dimensions, DimCount);
    SetLength(DimValues, DimCount);
    TotalElements := 1;

    for i := 0 to DimCount - 1 do
    begin
      DimExpr := DimsNode.GetChild(i);
      ProcessExpression(DimExpr, DimValue);

      // Store dimension value for VM (needed for variable dimensions)
      DimValues[i] := DimValue;

      // BASIC semantics: DIM A(N) allocates N+1 elements [0..N]
      // Dimensions can be constants or variables - evaluated once at DIM execution
      if DimValue.Kind = svkConstInt then
      begin
        Dimensions[i] := Integer(DimValue.ConstInt) + 1;

        // Safety check for constant dimensions
        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array dimension must be positive: %s[%d] = %d', [ArrName, i, Dimensions[i]-1]);

        // Calculate total elements (with overflow check)
        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkConstFloat then
      begin
        Dimensions[i] := Integer(Trunc(DimValue.ConstFloat)) + 1;

        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array dimension must be positive: %s[%d] = %d', [ArrName, i, Dimensions[i]-1]);

        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkRegister then
      begin
        // Variable dimension: DIM A(S%) - VM will read value from register
        // Use 0 as placeholder - actual size determined at runtime
        Dimensions[i] := 0;
        // Cannot validate size at compile time
        TotalElements := -1;
      end
      else
        raise Exception.CreateFmt('Invalid array dimension expression: %s', [ArrName]);
    end;

    // Declare array in SSA program
    ArrayIdx := FProgram.DeclareArray(ArrName, ElementType, Dimensions);

    // Store dimension registers (for any variable dimensions, even if mixed with constants)
    SetLength(DimRegs, DimCount);
    SetLength(DimRegTypes, DimCount);
    HasVariableDims := False;
    for i := 0 to DimCount - 1 do
    begin
      if DimValues[i].Kind = svkRegister then
      begin
        DimRegs[i] := DimValues[i].RegIndex;
        DimRegTypes[i] := DimValues[i].RegType;
        HasVariableDims := True;
      end
      else
      begin
        DimRegs[i] := -1;  // Not a register (constant)
        DimRegTypes[i] := srtInt;
      end;
    end;

    // Only store registers if at least one dimension is variable
    if HasVariableDims then
      FProgram.SetArrayDimRegisters(ArrayIdx, DimRegs, DimRegTypes);

    // Emit ssaArrayDim instruction
    // Dest = not used
    // Src1 = array reference (contains metadata index)
    // VM will use Src1.ArrayIndex to look up metadata and allocate
    ArrayRef := MakeSSAArrayRef(ArrayIdx, ElementType);
    EmitInstruction(ssaArrayDim, MakeSSAValue(svkNone), ArrayRef,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // CRITICAL: Attach variable dimension registers as PhiSources on the
    // ssaArrayDim instruction so that DCE can see them as dependencies.
    // Without this, DCE may eliminate the definitions of dimension registers
    // (they are only referenced through metadata, not through Src1/Src2/Src3),
    // causing the array to be allocated with wrong sizes at runtime.
    if HasVariableDims and Assigned(FCurrentBlock) then
    begin
      DimInstr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
      VarDimCount := 0;
      for i := 0 to DimCount - 1 do
        if DimValues[i].Kind = svkRegister then
          Inc(VarDimCount);
      SetLength(DimInstr.PhiSources, VarDimCount);
      VarDimCount := 0;
      for i := 0 to DimCount - 1 do
      begin
        if DimValues[i].Kind = svkRegister then
        begin
          DimInstr.PhiSources[VarDimCount].Value := DimValues[i];
          DimInstr.PhiSources[VarDimCount].FromBlock := nil;
          Inc(VarDimCount);
        end;
      end;
    end;

    // Array of UDT (M3.1): now that the int-handle array is dimensioned, eagerly allocate
    // one record instance per element and store the handles (bcRecordNewArray).
    if RecArrUDTIdx >= 0 then
    begin
      RecPacked := (Int64(FUDTs[RecArrUDTIdx].NInt) and $FFFF)
                or ((Int64(FUDTs[RecArrUDTIdx].NFloat) and $FFFF) shl 16)
                or ((Int64(FUDTs[RecArrUDTIdx].NStr) and $FFFF) shl 32)
                or ((Int64(RecArrUDTIdx) and $FFFF) shl 48);   // typeId for stamping each element
      EmitInstruction(ssaRecordNewArray, MakeSSAValue(svkNone),
                      MakeSSAArrayRef(ArrayIdx, srtInt), MakeSSAConstInt(RecPacked),
                      MakeSSAValue(svkNone));
    end;
  end;
end;

procedure TSSAGenerator.ProcessForLoop(Node: TASTNode);
var
  VarName: string;
  StartValue, EndValue, StepValue: TSSAValue;
  VarReg: TSSAValue;
  CondLabel, BodyLabel, EndLabel: string;
  CmpReg: Integer;
  LoopInfo: TLoopInfo;
  TempReg: Integer;
  TempVal: TSSAValue;
  PrevBlock, CondBlock, BodyBlock, GEBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
  StepNode: TASTNode;
  StepIsNegative: Boolean;
  StepSignReg: Integer;  // Register to hold STEP sign check result
  UseLELabel, UseGELabel, CheckResultLabel: string;  // Labels for runtime step direction
  NeedRuntimeCheck: Boolean;  // True if STEP direction must be determined at runtime
begin
  if Node.ChildCount < 3 then Exit;
  if Node.GetChild(0).NodeType <> antIdentifier then Exit;

  VarName := VarToStr(Node.GetChild(0).Value);
  VarReg := GetOrAllocateVariable(VarName);

  // Evaluate start, end, and step values
  ProcessExpression(Node.GetChild(1), StartValue);
  ProcessExpression(Node.GetChild(2), EndValue);

  // Check if STEP is negative by examining AST node
  StepIsNegative := False;
  NeedRuntimeCheck := False;
  if Node.ChildCount > 3 then
  begin
    StepNode := Node.GetChild(3);
    // Check if STEP is a unary minus (negative constant or expression)
    if StepNode.NodeType = antUnaryOp then
      StepIsNegative := True;
    ProcessExpression(StepNode, StepValue);

    // Also check if the evaluated STEP value is a negative constant
    // This handles cases where STEP is a variable containing a negative value
    if not StepIsNegative then
    begin
      if (StepValue.Kind = svkConstInt) and (StepValue.ConstInt < 0) then
        StepIsNegative := True
      else if (StepValue.Kind = svkConstFloat) and (StepValue.ConstFloat < 0) then
        StepIsNegative := True
      else if StepValue.Kind = svkRegister then
        // STEP is a runtime variable - we need runtime check to determine direction
        NeedRuntimeCheck := True;
    end;
  end
  else
    StepValue := MakeSSAConstInt(1);  // Default step = 1

  // Initialize loop variable with start value (do this first!)
  // Convert to match VarReg type to avoid register collision
  if StartValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
      EmitInstruction(ssaLoadConstFloat, VarReg, MakeSSAConstFloat(StartValue.ConstInt), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstInt, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if StartValue.Kind = svkConstFloat then
    EmitInstruction(ssaLoadConstFloat, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else if StartValue.Kind = svkRegister then
  begin
    // StartValue is in a register - must convert type to match VarReg if needed
    if (StartValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int StartValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaCopyFloat, VarReg, TempVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (StartValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float StartValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaCopyInt, VarReg, TempVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Same type, just copy
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCopyInt, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, VarReg, StartValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Materialize constants into registers for EndValue and StepValue (after var init!)
  // Convert to same type as VarReg for proper comparison
  if EndValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
    begin
      // Convert int constant to float
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(EndValue.ConstInt),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end;
  end
  else if EndValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EndValue := TempVal;
  end
  else if EndValue.Kind = svkRegister then
  begin
    // EndValue is already in a register (e.g., result of sqr(n))
    // MUST copy to a dedicated register to prevent overwriting during loop
    // AND convert type to match VarReg if needed
    if (EndValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int EndValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else if (EndValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float EndValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end
    else
    begin
      // Same type, just copy
      TempReg := FProgram.AllocRegister(EndValue.RegType);
      TempVal := MakeSSARegister(EndValue.RegType, TempReg);
      if EndValue.RegType = srtInt then
        EmitInstruction(ssaCopyInt, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, TempVal, EndValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EndValue := TempVal;
    end;
  end;

  if StepValue.Kind = svkConstInt then
  begin
    if VarReg.RegType = srtFloat then
    begin
      // Convert int constant to float
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(StepValue.ConstInt),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end;
  end
  else if StepValue.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    TempVal := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    StepValue := TempVal;
  end
  else if StepValue.Kind = svkRegister then
  begin
    // StepValue is already in a register
    // MUST copy to a dedicated register to prevent overwriting during loop
    // AND convert type to match VarReg if needed
    if (StepValue.RegType = srtInt) and (VarReg.RegType = srtFloat) then
    begin
      // Convert int StepValue to float to match VarReg
      TempReg := FProgram.AllocRegister(srtFloat);
      TempVal := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else if (StepValue.RegType = srtFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float StepValue to int to match VarReg
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end
    else
    begin
      // Same type, just copy
      TempReg := FProgram.AllocRegister(StepValue.RegType);
      TempVal := MakeSSARegister(StepValue.RegType, TempReg);
      if StepValue.RegType = srtInt then
        EmitInstruction(ssaCopyInt, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCopyFloat, TempVal, StepValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      StepValue := TempVal;
    end;
  end;

  // Generate labels
  BodyLabel := GenerateUniqueLabel('for_body');
  EndLabel := GenerateUniqueLabel('for_end');

  // PHASE 3 TIER 3: Save current block before creating new ones
  PrevBlock := FCurrentBlock;

  if NeedRuntimeCheck then
  begin
    // STEP sign unknown at compile-time - generate runtime check and two separate paths

    // Check STEP < 0
    // Materialize zero constant into a register first
    TempReg := FProgram.AllocRegister(VarReg.RegType);
    TempVal := MakeSSARegister(VarReg.RegType, TempReg);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(0.0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    StepSignReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLtInt, MakeSSARegister(VarReg.RegType, StepSignReg), StepValue, TempVal, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLtFloat, MakeSSARegister(VarReg.RegType, StepSignReg), StepValue, TempVal, MakeSSAValue(svkNone));

    // Generate two condition labels
    UseLELabel := GenerateUniqueLabel('for_cond_le');
    UseGELabel := GenerateUniqueLabel('for_cond_ge');

    // Jump to GE label if STEP < 0
    EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(UseGELabel), MakeSSARegister(VarReg.RegType, StepSignReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // ===== LE Condition Block (STEP >= 0) =====
    FCurrentBlock := FProgram.CreateBlock(UseLELabel);
    CondBlock := FCurrentBlock;
    CondLabel := UseLELabel;

    // PHASE 3 TIER 3: Connect edge PrevBlock → LE CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(CondBlock);
      CondBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', CondBlock.LabelName);
      {$ENDIF}
    end;

    CmpReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaJump, MakeSSALabel(BodyLabel), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // ===== GE Condition Block (STEP < 0) =====
    FCurrentBlock := FProgram.CreateBlock(UseGELabel);
    GEBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → GE CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(GEBlock);
      GEBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', GEBlock.LabelName);
      {$ENDIF}
    end;

    CmpReg := FProgram.AllocRegister(VarReg.RegType);
    if VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpGeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpGeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    // Fallthrough to body
  end
  else
  begin
    // STEP direction known at compile-time - use single condition label
    CondLabel := GenerateUniqueLabel('for_cond');

    // Jump to condition check
    EmitInstruction(ssaJump, MakeSSALabel(CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Condition check block
    FCurrentBlock := FProgram.CreateBlock(CondLabel);
    CondBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → CondBlock
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(CondBlock);
      CondBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', CondBlock.LabelName);
      {$ENDIF}
    end;

    // Compare loop variable with end value
    CmpReg := FProgram.AllocRegister(VarReg.RegType);

    if StepIsNegative then
    begin
      // Negative step: use >= comparison
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCmpGeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCmpGeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    end
    else
    begin
      // Positive or zero step: use <= comparison
      if VarReg.RegType = srtInt then
        EmitInstruction(ssaCmpLeInt, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaCmpLeFloat, MakeSSARegister(VarReg.RegType, CmpReg), VarReg, EndValue, MakeSSAValue(svkNone));
    end;

    // Jump to body if condition is true (!=0), otherwise jump to end
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), MakeSSARegister(VarReg.RegType, CmpReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Body block - subsequent statements will be added here until NEXT
  FCurrentBlock := FProgram.CreateBlock(BodyLabel);
  BodyBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edge CondBlock → BodyBlock (true branch)
  CondBlock.AddSuccessor(BodyBlock);
  BodyBlock.AddPredecessor(CondBlock);
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Edge: ', CondBlock.LabelName, ' → ', BodyBlock.LabelName, ' (true)');
  {$ENDIF}

  // Push loop info onto stack
  LoopInfo.VarName := VarName;
  LoopInfo.VarReg := VarReg;
  LoopInfo.EndValue := EndValue;
  LoopInfo.StepValue := StepValue;
  LoopInfo.StepIsNegative := StepIsNegative;
  LoopInfo.NeedRuntimeCheck := NeedRuntimeCheck;
  LoopInfo.CondLabel := CondLabel;
  if NeedRuntimeCheck then
    LoopInfo.CondLabelGE := UseGELabel
  else
    LoopInfo.CondLabelGE := '';
  LoopInfo.BodyLabel := BodyLabel;
  LoopInfo.EndLabel := EndLabel;
  SetLength(FLoopStack, Length(FLoopStack) + 1);
  FLoopStack[High(FLoopStack)] := LoopInfo;
end;

procedure TSSAGenerator.ProcessDoLoop(Node: TASTNode);
var
  BodyLabel, CondLabel, EndLabel: string;
  ConditionNode, BodyNode: TASTNode;
  CondValue: TSSAValue;
  CmpReg: Integer;
  PrevBlock, CondBlock, BodyBlock: TSSABasicBlock;
  ConditionType: string;
  ConditionPosition: string;
  IsWhileLoop: Boolean;
  HasCondition: Boolean;
  i: Integer;
  LoopInfo: TLoopInfo;
begin
  // DO/LOOP structure:
  //   Child[0] = Body (block of statements)
  //   Child[1] = Condition (optional)
  //   Attributes: ConditionType = WHILE | UNTIL | ''
  //   Attributes: ConditionPosition = TOP | BOTTOM | ''

  if Node.ChildCount < 1 then Exit;

  BodyNode := Node.GetChild(0);
  ConditionNode := nil;
  if Node.ChildCount > 1 then
    ConditionNode := Node.GetChild(1);

  ConditionType := Node.Attributes.Values['ConditionType'];
  ConditionPosition := Node.Attributes.Values['ConditionPosition'];
  IsWhileLoop := (ConditionType = 'WHILE');
  HasCondition := (ConditionType <> '');

  // Generate labels
  BodyLabel := GenerateUniqueLabel('do_body');
  CondLabel := GenerateUniqueLabel('do_cond');
  EndLabel := GenerateUniqueLabel('do_end');

  // Push loop info for EXIT support
  LoopInfo.VarName := '';  // DO loops don't have a loop variable
  LoopInfo.VarReg := MakeSSAValue(svkNone);
  LoopInfo.EndValue := MakeSSAValue(svkNone);
  LoopInfo.StepValue := MakeSSAValue(svkNone);
  LoopInfo.StepIsNegative := False;
  LoopInfo.NeedRuntimeCheck := False;
  LoopInfo.CondLabel := CondLabel;
  LoopInfo.CondLabelGE := '';
  LoopInfo.BodyLabel := BodyLabel;
  LoopInfo.EndLabel := EndLabel;
  SetLength(FLoopStack, Length(FLoopStack) + 1);
  FLoopStack[High(FLoopStack)] := LoopInfo;

  // Save current block for CFG construction
  PrevBlock := FCurrentBlock;

  if HasCondition and (ConditionPosition = 'TOP') then
  begin
    // DO WHILE/UNTIL condition - check condition BEFORE body
    // Jump to condition check first
    EmitInstruction(ssaJump, MakeSSALabel(CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Condition block
    FCurrentBlock := FProgram.CreateBlock(CondLabel);
    CondBlock := FCurrentBlock;

    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(CondBlock);
      CondBlock.AddPredecessor(PrevBlock);
    end;

    // Evaluate condition
    if Assigned(ConditionNode) then
    begin
      ProcessExpression(ConditionNode, CondValue);

      // Ensure condition is in a register
      if CondValue.Kind <> svkRegister then
      begin
        CmpReg := FProgram.AllocRegister(srtInt);
        if CondValue.Kind = svkConstInt then
          EmitInstruction(ssaLoadConstInt, MakeSSARegister(srtInt, CmpReg), CondValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if CondValue.Kind = svkConstFloat then
          EmitInstruction(ssaLoadConstFloat, MakeSSARegister(srtFloat, CmpReg), CondValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        CondValue := MakeSSARegister(srtInt, CmpReg);
      end;

      // WHILE: continue if condition TRUE, exit if FALSE
      // UNTIL: continue if condition FALSE, exit if TRUE
      if IsWhileLoop then
        EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), CondValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(EndLabel), CondValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    // Body block
    FCurrentBlock := FProgram.CreateBlock(BodyLabel);
    BodyBlock := FCurrentBlock;

    if Assigned(CondBlock) then
    begin
      CondBlock.AddSuccessor(BodyBlock);
      BodyBlock.AddPredecessor(CondBlock);
    end;

    // Process body statements
    if Assigned(BodyNode) then
    begin
      for i := 0 to BodyNode.ChildCount - 1 do
        ProcessStatement(BodyNode.GetChild(i));
    end;

    // Jump back to condition check
    EmitInstruction(ssaJump, MakeSSALabel(CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // End block
    FCurrentBlock := FProgram.CreateBlock(EndLabel);
  end
  else
  begin
    // DO ... LOOP [WHILE/UNTIL condition] - check condition AFTER body (or no condition)

    // Jump to body (could also fall through)
    EmitInstruction(ssaJump, MakeSSALabel(BodyLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Body block
    FCurrentBlock := FProgram.CreateBlock(BodyLabel);
    BodyBlock := FCurrentBlock;

    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(BodyBlock);
      BodyBlock.AddPredecessor(PrevBlock);
    end;

    // Process body statements
    if Assigned(BodyNode) then
    begin
      for i := 0 to BodyNode.ChildCount - 1 do
        ProcessStatement(BodyNode.GetChild(i));
    end;

    if HasCondition and Assigned(ConditionNode) then
    begin
      // Evaluate condition at bottom
      ProcessExpression(ConditionNode, CondValue);

      // Ensure condition is in a register
      if CondValue.Kind <> svkRegister then
      begin
        CmpReg := FProgram.AllocRegister(srtInt);
        if CondValue.Kind = svkConstInt then
          EmitInstruction(ssaLoadConstInt, MakeSSARegister(srtInt, CmpReg), CondValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else if CondValue.Kind = svkConstFloat then
          EmitInstruction(ssaLoadConstFloat, MakeSSARegister(srtFloat, CmpReg), CondValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        CondValue := MakeSSARegister(srtInt, CmpReg);
      end;

      // WHILE: loop back if condition TRUE, exit if FALSE
      // UNTIL: loop back if condition FALSE, exit if TRUE
      if IsWhileLoop then
      begin
        EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(BodyLabel), CondValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // Explicit jump to end when condition is FALSE
        EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        EmitInstruction(ssaJumpIfZero, MakeSSALabel(BodyLabel), CondValue,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // Explicit jump to end when condition is TRUE
        EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;

      // End block - code after the loop continues here
      FCurrentBlock := FProgram.CreateBlock(EndLabel);
    end
    else
    begin
      // Infinite loop: DO ... LOOP (no condition)
      EmitInstruction(ssaJump, MakeSSALabel(BodyLabel), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));

      // End label for potential EXIT statements
      FCurrentBlock := FProgram.CreateBlock(EndLabel);
    end;
  end;

  // Pop loop info from stack
  SetLength(FLoopStack, Length(FLoopStack) - 1);
end;

procedure TSSAGenerator.ProcessBlock(Node: TASTNode);
var
  i: Integer;
begin
  // BEGIN/BEND block - simply process all child statements
  for i := 0 to Node.ChildCount - 1 do
    ProcessStatement(Node.GetChild(i));
end;

procedure TSSAGenerator.ProcessDefFn(Node: TASTNode);
var
  FnDef: TUserFunctionDef;
  NameNode, ParamListNode, BodyNode: TASTNode;
begin
  // DEF FN structure:
  //   Child[0] = Function name (identifier)
  //   Child[1] = Parameter list (dimensions node with single param)
  //   Child[2] = Body expression

  if Node.ChildCount < 3 then Exit;

  NameNode := Node.GetChild(0);
  ParamListNode := Node.GetChild(1);
  BodyNode := Node.GetChild(2);

  // Get function name
  FnDef.Name := UpperCase(VarToStr(NameNode.Value));

  // Get parameter name (first child of param list)
  if ParamListNode.ChildCount > 0 then
    FnDef.ParamName := UpperCase(VarToStr(ParamListNode.GetChild(0).Value))
  else
    FnDef.ParamName := '';

  // Store body node reference for later evaluation
  FnDef.BodyNode := BodyNode;

  // Register the function
  FUserFunctions.AddOrSetValue(FnDef.Name, FnDef);

  // DEF FN doesn't generate any runtime code - it just registers the function
  // The function body is evaluated inline when FN is called
end;

procedure TSSAGenerator.ProcessNext(Node: TASTNode);
var
  LoopInfo: TLoopInfo;
  NewVarReg: Integer;
  TempReg: Integer;
  TempVal: TSSAValue;
  BodyBlock, CondBlock, EndBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  // Pop loop info from stack
  if Length(FLoopStack) = 0 then Exit;  // Error: NEXT without FOR
  LoopInfo := FLoopStack[High(FLoopStack)];
  SetLength(FLoopStack, Length(FLoopStack) - 1);

  // Increment loop variable by step
  if LoopInfo.VarReg.RegType = srtInt then
  begin
    NewVarReg := FProgram.AllocRegister(srtInt);
    EmitInstruction(ssaAddInt, MakeSSARegister(srtInt, NewVarReg), LoopInfo.VarReg, LoopInfo.StepValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaCopyInt, LoopInfo.VarReg, MakeSSARegister(srtInt, NewVarReg), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    NewVarReg := FProgram.AllocRegister(srtFloat);
    EmitInstruction(ssaAddFloat, MakeSSARegister(srtFloat, NewVarReg), LoopInfo.VarReg, LoopInfo.StepValue, MakeSSAValue(svkNone));
    EmitInstruction(ssaCopyFloat, LoopInfo.VarReg, MakeSSARegister(srtFloat, NewVarReg), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Jump back to condition check
  if LoopInfo.NeedRuntimeCheck then
  begin
    // Runtime check: need to test STEP sign again and jump to appropriate condition label
    // Materialize zero constant into a register first
    TempReg := FProgram.AllocRegister(LoopInfo.VarReg.RegType);
    TempVal := MakeSSARegister(LoopInfo.VarReg.RegType, TempReg);
    if LoopInfo.VarReg.RegType = srtInt then
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaLoadConstFloat, TempVal, MakeSSAConstFloat(0.0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    NewVarReg := FProgram.AllocRegister(LoopInfo.VarReg.RegType);
    if LoopInfo.VarReg.RegType = srtInt then
      EmitInstruction(ssaCmpLtInt, MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg), LoopInfo.StepValue, TempVal, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaCmpLtFloat, MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg), LoopInfo.StepValue, TempVal, MakeSSAValue(svkNone));

    // If STEP < 0, jump to GE label, else jump to LE label
    EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(LoopInfo.CondLabelGE), MakeSSARegister(LoopInfo.VarReg.RegType, NewVarReg),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaJump, MakeSSALabel(LoopInfo.CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    // Standard case: jump to single condition label
    EmitInstruction(ssaJump, MakeSSALabel(LoopInfo.CondLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // PHASE 3 TIER 3: Add back-edge BodyBlock → CondBlock
  BodyBlock := FCurrentBlock;
  CondBlock := FProgram.FindBlock(LoopInfo.CondLabel);
  if Assigned(CondBlock) then
  begin
    BodyBlock.AddSuccessor(CondBlock);
    CondBlock.AddPredecessor(BodyBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', BodyBlock.LabelName, ' → ', CondBlock.LabelName, ' (BACK-EDGE)');
    {$ENDIF}
  end;

  // Create end block for code after the loop
  FCurrentBlock := FProgram.CreateBlock(LoopInfo.EndLabel);
  EndBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Add false branch CondBlock → EndBlock
  if Assigned(CondBlock) then
  begin
    CondBlock.AddSuccessor(EndBlock);
    EndBlock.AddPredecessor(CondBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', CondBlock.LabelName, ' → ', EndBlock.LabelName, ' (false)');
    {$ENDIF}
  end;
end;

procedure TSSAGenerator.ProcessIfStatement(Node: TASTNode);
var
  CondValue: TSSAValue;
  ThenLabel, ElseLabel, EndLabel: string;
  i: Integer;
  Child: TASTNode;
  HasElse: Boolean;
  PrevBlock, ThenBlock, ElseBlock, EndBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount < 1 then Exit;

  // Evaluate condition (should return Int 0 or 1)
  ProcessExpression(Node.GetChild(0), CondValue);

  // Generate unique labels
  ThenLabel := GenerateUniqueLabel('then');
  ElseLabel := GenerateUniqueLabel('else');
  EndLabel := GenerateUniqueLabel('endif');

  // Check if there's an ELSE clause
  HasElse := False;
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Assigned(Child) and (Child.NodeType = antElse) then
    begin
      HasElse := True;
      Break;
    end;
  end;

  // PHASE 3 TIER 3: Save current block before creating new ones
  PrevBlock := FCurrentBlock;

  // Jump if condition is zero (false) to else or end
  if HasElse then
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(ElseLabel), CondValue,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(EndLabel), CondValue,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // THEN block
  FCurrentBlock := FProgram.CreateBlock(ThenLabel);
  ThenBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edge PrevBlock → ThenBlock (true branch)
  if Assigned(PrevBlock) then
  begin
    PrevBlock.AddSuccessor(ThenBlock);
    ThenBlock.AddPredecessor(PrevBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', ThenBlock.LabelName, ' (true)');
    {$ENDIF}
  end;

  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Assigned(Child) and (Child.NodeType = antThen) then
    begin
      ProcessStatement(Child);
      Break;
    end;
  end;

  // FIX: Save the actual last block of THEN branch (might have changed due to nested control flow)
  ThenBlock := FCurrentBlock;

  // Jump to end after THEN - but ONLY if the branch didn't already terminate (GOTO/RETURN/END)
  if Assigned(FCurrentBlock) then
    EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // ELSE block (if present)
  if HasElse then
  begin
    FCurrentBlock := FProgram.CreateBlock(ElseLabel);
    ElseBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Connect edge PrevBlock → ElseBlock (false branch)
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(ElseBlock);
      ElseBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', ElseBlock.LabelName, ' (false)');
      {$ENDIF}
    end;

    for i := 1 to Node.ChildCount - 1 do
    begin
      Child := Node.GetChild(i);
      if Assigned(Child) and (Child.NodeType = antElse) then
      begin
        ProcessStatement(Child);
        Break;
      end;
    end;

    // FIX: Save the actual last block of ELSE branch (might have changed due to nested control flow)
    ElseBlock := FCurrentBlock;
  end;

  // End block
  FCurrentBlock := FProgram.CreateBlock(EndLabel);
  EndBlock := FCurrentBlock;

  // PHASE 3 TIER 3: Connect edges to EndBlock
  // ThenBlock → EndBlock (unconditional jump after THEN)
  if Assigned(ThenBlock) then
  begin
    ThenBlock.AddSuccessor(EndBlock);
    EndBlock.AddPredecessor(ThenBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', ThenBlock.LabelName, ' → ', EndBlock.LabelName);
    {$ENDIF}
  end;

  if HasElse then
  begin
    // ElseBlock → EndBlock (fall-through after ELSE)
    if Assigned(ElseBlock) then
    begin
      ElseBlock.AddSuccessor(EndBlock);
      EndBlock.AddPredecessor(ElseBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', ElseBlock.LabelName, ' → ', EndBlock.LabelName);
      {$ENDIF}
    end;
  end
  else
  begin
    // PrevBlock → EndBlock (false branch when no ELSE)
    if Assigned(PrevBlock) then
    begin
      PrevBlock.AddSuccessor(EndBlock);
      EndBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', EndBlock.LabelName, ' (false, no ELSE)');
      {$ENDIF}
    end;
  end;
end;

function TSSAGenerator.JumpLabelName(LabelNode: TASTNode): string;
begin
  if LabelNode.NodeType = antIdentifier then
    Result := 'LABEL_' + UpperCase(VarToStr(LabelNode.Value))   // named label (case-insensitive)
  else
    Result := 'LINE_' + VarToStr(LabelNode.Value);              // classic line number
end;

procedure TSSAGenerator.ProcessGoto(Node: TASTNode);
var
  LabelNode: TASTNode;
  LabelName: string;
  SourceBlock, TargetBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount = 0 then Exit;
  LabelNode := Node.GetChild(0);
  LabelName := JumpLabelName(LabelNode);

  // PHASE 3 TIER 3: Save current block before jump
  SourceBlock := FCurrentBlock;

  EmitInstruction(ssaJump, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // PHASE 3 TIER 3: Connect edge SourceBlock → TargetBlock
  // Note: TargetBlock might not exist yet (forward GOTO), will be connected later
  TargetBlock := FProgram.FindBlock(LabelName);
  if Assigned(TargetBlock) and Assigned(SourceBlock) then
  begin
    SourceBlock.AddSuccessor(TargetBlock);
    TargetBlock.AddPredecessor(SourceBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', SourceBlock.LabelName, ' → ', TargetBlock.LabelName, ' (GOTO)');
    {$ENDIF}
  end;

  // PHASE 3 TIER 3: GOTO terminates the current block - no fall-through
  FCurrentBlock := nil;
end;

procedure TSSAGenerator.ProcessGosub(Node: TASTNode);
var
  LabelNode: TASTNode;
  LabelName: string;
  SourceBlock, TargetBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  if Node.ChildCount = 0 then Exit;
  LabelNode := Node.GetChild(0);
  LabelName := JumpLabelName(LabelNode);

  // PHASE 3 TIER 3: Save current block before call
  SourceBlock := FCurrentBlock;

  EmitInstruction(ssaCall, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // PHASE 3 TIER 3: Connect edge SourceBlock → TargetBlock (subroutine entry)
  // Note: TargetBlock might not exist yet (forward GOSUB), will be connected later
  TargetBlock := FProgram.FindBlock(LabelName);
  if Assigned(TargetBlock) and Assigned(SourceBlock) then
  begin
    SourceBlock.AddSuccessor(TargetBlock);
    TargetBlock.AddPredecessor(SourceBlock);
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('[SSA] Edge: ', SourceBlock.LabelName, ' → ', TargetBlock.LabelName, ' (GOSUB)');
    {$ENDIF}
  end;

  // NOTE: Unlike GOTO, GOSUB does NOT set FCurrentBlock := nil
  // The fall-through edge LINE_750 → LINE_760 represents where the program counter
  // continues after RETURN. Variables flow via the RETURN edge (LINE_9030 → LINE_760).
end;

{ ProcessOnGoto - Handle ON expr GOTO line1, line2, ... statement
  Evaluates the expression and jumps to the corresponding line number.
  If expression = 1, jump to line1; if = 2, jump to line2; etc.
  If out of range (< 1 or > number of targets), fall through to next statement.
}
procedure TSSAGenerator.ProcessOnGoto(Node: TASTNode);
var
  SelectorNode, TargetListNode, TargetNode: TASTNode;
  SelectorValue, SelectorReg, TempReg, CmpReg: TSSAValue;
  i, TempRegNum, CmpRegNum: Integer;
  LabelName, EndLabel: string;
  TargetBlock: TSSABasicBlock;
begin
  if Node.ChildCount < 2 then Exit;

  SelectorNode := Node.GetChild(0);       // Expression (selector value)
  TargetListNode := Node.GetChild(1);     // List of target line numbers

  // Evaluate selector expression
  ProcessExpression(SelectorNode, SelectorValue);
  SelectorReg := EnsureIntRegister(SelectorValue);

  // Create end label for fall-through case
  EndLabel := GenerateUniqueLabel('ON_GOTO_END');

  // Generate conditional jumps for each target
  for i := 0 to TargetListNode.ChildCount - 1 do
  begin
    TargetNode := TargetListNode.GetChild(i);
    LabelName := JumpLabelName(TargetNode);

    // Compare selector with (i + 1)
    TempRegNum := FProgram.AllocRegister(srtInt);
    TempReg := MakeSSARegister(srtInt, TempRegNum);
    EmitInstruction(ssaLoadConstInt, TempReg,
                   MakeSSAConstInt(i + 1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // If selector = (i+1), jump to target
    CmpRegNum := FProgram.AllocRegister(srtInt);
    CmpReg := MakeSSARegister(srtInt, CmpRegNum);
    EmitInstruction(ssaCmpEqInt, CmpReg,
                   SelectorReg, TempReg,
                   MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfNotZero, MakeSSALabel(LabelName),
                   CmpReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Connect CFG edge
    TargetBlock := FProgram.FindBlock(LabelName);
    if Assigned(TargetBlock) and Assigned(FCurrentBlock) then
    begin
      FCurrentBlock.AddSuccessor(TargetBlock);
      TargetBlock.AddPredecessor(FCurrentBlock);
    end;
  end;

  // Fall-through case (selector out of range) - create end block
  FCurrentBlock := FProgram.CreateBlock(EndLabel);
end;

{ ProcessOnGosub - Handle ON expr GOSUB line1, line2, ... statement
  Similar to ON GOTO but calls subroutine instead of jumping.
}
procedure TSSAGenerator.ProcessOnGosub(Node: TASTNode);
var
  SelectorNode, TargetListNode, TargetNode: TASTNode;
  SelectorValue, SelectorReg, TempReg, CmpReg: TSSAValue;
  i, TempRegNum, CmpRegNum: Integer;
  LabelName, NextLabel, EndLabel: string;
  TargetBlock: TSSABasicBlock;
begin
  if Node.ChildCount < 2 then Exit;

  SelectorNode := Node.GetChild(0);       // Expression (selector value)
  TargetListNode := Node.GetChild(1);     // List of target line numbers

  // Evaluate selector expression
  ProcessExpression(SelectorNode, SelectorValue);
  SelectorReg := EnsureIntRegister(SelectorValue);

  // Create end label for fall-through case
  EndLabel := GenerateUniqueLabel('ON_GOSUB_END');

  // Generate conditional calls for each target
  for i := 0 to TargetListNode.ChildCount - 1 do
  begin
    TargetNode := TargetListNode.GetChild(i);
    LabelName := JumpLabelName(TargetNode);
    NextLabel := GenerateUniqueLabel('ON_GOSUB_NEXT');

    // Compare selector with (i + 1)
    TempRegNum := FProgram.AllocRegister(srtInt);
    TempReg := MakeSSARegister(srtInt, TempRegNum);
    EmitInstruction(ssaLoadConstInt, TempReg,
                   MakeSSAConstInt(i + 1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // If selector <> (i+1), skip to next check
    CmpRegNum := FProgram.AllocRegister(srtInt);
    CmpReg := MakeSSARegister(srtInt, CmpRegNum);
    EmitInstruction(ssaCmpEqInt, CmpReg,
                   SelectorReg, TempReg,
                   MakeSSAValue(svkNone));
    EmitInstruction(ssaJumpIfZero, MakeSSALabel(NextLabel),
                   CmpReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Call subroutine
    EmitInstruction(ssaCall, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Jump to end after call returns
    EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // Connect CFG edge
    TargetBlock := FProgram.FindBlock(LabelName);
    if Assigned(TargetBlock) and Assigned(FCurrentBlock) then
    begin
      FCurrentBlock.AddSuccessor(TargetBlock);
      TargetBlock.AddPredecessor(FCurrentBlock);
    end;

    // Create next check block for the next iteration
    FCurrentBlock := FProgram.CreateBlock(NextLabel);
  end;

  // The last ON_GOSUB_NEXT block is now current but empty
  // Add a jump to EndLabel so it has a proper successor (fall-through case)
  EmitInstruction(ssaJump, MakeSSALabel(EndLabel), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Fall-through case (selector out of range) - create end block
  FCurrentBlock := FProgram.CreateBlock(EndLabel);
end;

{ ProcessGraphics - Handle GRAPHIC command for setting graphics mode
  Syntax: GRAPHIC [mode [, clear [, param3]]]
  Parameters:
    0: mode   - graphics mode (0-11), optional, default 8 (gm80x50Text)
    1: clear  - clear screen (0 or 1), optional, default 0
    2: param3 - additional parameter for mode 11 (SDL2 mode index), optional

  Mode validation is done at runtime in the VM to support variables/expressions.
  Invalid modes (< 0 or > 11) will raise ?ILLEGAL QUANTITY ERROR.
}
procedure TSSAGenerator.ProcessGraphics(Node: TASTNode);
const
  DEFAULT_GRAPHIC_MODE = 8;  // gm80x50Text - 80x50 text mode
var
  i, TempReg: Integer;
  ParamValues: array[0..2] of TSSAValue;
  ParamRegs: array[0..2] of TSSAValue;
  ParamCount: Integer;
begin
  // Initialize all parameters to None
  for i := 0 to 2 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;

  // GRAPHIC without parameters: use default mode
  if ParamCount < 1 then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[0] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[0], MakeSSAConstInt(DEFAULT_GRAPHIC_MODE),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    // Evaluate each parameter expression (supports variables, expressions, functions)
    for i := 0 to Min(ParamCount - 1, 2) do
    begin
      ProcessExpression(Node.GetChild(i), ParamValues[i]);

      // Materialize constants into registers or convert to int as needed
      if ParamValues[i].Kind = svkConstInt then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkConstFloat then
      begin
        // Convert float constant to int for mode parameter
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(Trunc(ParamValues[i].ConstFloat)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkRegister then
      begin
        // Check if register is float - if so, convert to int
        if ParamValues[i].RegType = srtFloat then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
          EmitInstruction(ssaFloatToInt, ParamRegs[i], ParamValues[i],
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
          // Int register - use directly
          ParamRegs[i] := ParamValues[i];
      end
      else
        ParamRegs[i] := MakeSSAValue(svkNone);
    end;
  end;

  // Set defaults for optional parameters
  // clear defaults to 0 (preserve buffer content)
  // First-time initialization is handled in SetGraphicMode when buffer doesn't exist yet
  if (ParamCount < 2) or (ParamRegs[1].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[1] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[1], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // param3 defaults to 0
  if (ParamCount < 3) or (ParamRegs[2].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[2] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[2], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicSetMode instruction
  // Dest=None (no result), Src1=mode, Src2=clear, Src3=param3
  EmitInstruction(ssaGraphicSetMode, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);
end;

{ ProcessScnClr - Handle SCNCLR command (Screen Clear)
  Syntax: SCNCLR [mode]
  If mode is specified (0-5), clears that mode's screen
  If mode is omitted (-1), clears the current mode's screen

  Text modes (0, 5): Clear text buffer and scrollback, cursor to 0,0
  Graphics modes (1-4): Clear graphics buffer with background color
}
procedure TSSAGenerator.ProcessScnClr(Node: TASTNode);
var
  ModeVal, ModeReg: TSSAValue;
  TempReg: Integer;
begin
  if FCurrentBlock = nil then Exit;

  // Check if mode is specified
  if Node.ChildCount > 0 then
  begin
    // Mode specified - use it
    ProcessExpression(Node.GetChild(0), ModeVal);
    ModeReg := EnsureIntRegister(ModeVal);
  end
  else
  begin
    // No mode specified - use -1 to indicate current mode
    TempReg := FProgram.AllocRegister(srtInt);
    ModeReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ModeReg, MakeSSAConstInt(-1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaScnClr with mode (-1 = current, 0-5 = specific)
  EmitInstruction(ssaScnClr, MakeSSAValue(svkNone),
                 ModeReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessBox - Handle BOX command for drawing rectangles
  Syntax: BOX color, x1, y1, x2, y2 [, angle [, filled [, fill_color]]]
  Parameters:
    0: color     - border color (32-bit RGBA or palette index)
    1: x1        - first corner X
    2: y1        - first corner Y
    3: x2        - second corner X
    4: y2        - second corner Y
    5: angle     - rotation angle in degrees (optional, default 0)
    6: filled    - 1=filled, 0=outline only (optional, default 0)
    7: fill_color - fill color (optional, default = border color)
}
procedure TSSAGenerator.ProcessBox(Node: TASTNode);
var
  i, TempReg: Integer;
  ParamValues: array[0..7] of TSSAValue;
  ParamRegs: array[0..7] of TSSAValue;
  ParamCount: Integer;
  Instr: TSSAInstruction;
begin
  // Initialize all parameters to None
  for i := 0 to 7 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;
  if ParamCount < 5 then
  begin
    // BOX requires at least 5 parameters: color, x1, y1, x2, y2
    WriteLn(StdErr, 'BOX: requires at least 5 parameters (color, x1, y1, x2, y2)');
    Exit;
  end;

  // Evaluate each parameter expression
  for i := 0 to Min(ParamCount - 1, 7) do
  begin
    ProcessExpression(Node.GetChild(i), ParamValues[i]);

    // Materialize constants into registers
    if ParamValues[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamValues[i].Kind = svkConstFloat then
    begin
      // Parameters 0-4 (color, x1, y1, x2, y2) and 6-7 (filled, fill_color) must be int
      // Only parameter 5 (angle) should be float
      if i = 5 then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaLoadConstFloat, ParamRegs[i], ParamValues[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // Convert float constant to int for coordinate/color parameters
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i],
                       MakeSSAConstInt(Trunc(ParamValues[i].ConstFloat)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end
    else if ParamValues[i].Kind = svkRegister then
      ParamRegs[i] := ParamValues[i]
    else
      ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Convert float registers to int for parameters 0-4, 6-7 (VM expects int for coordinates)
  for i := 0 to 4 do
  begin
    if (ParamRegs[i].Kind = svkRegister) and (ParamRegs[i].RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      EmitInstruction(ssaFloatToInt, MakeSSARegister(srtInt, TempReg),
                     ParamRegs[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
    end;
  end;

  // Set defaults for optional parameters
  // angle defaults to 0 - MUST be float for VM compatibility
  if (ParamCount < 6) or (ParamRegs[5].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[5] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[5], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if ParamRegs[5].RegType = srtInt then
  begin
    // Convert int angle to float - VM expects float register
    TempReg := FProgram.AllocRegister(srtFloat);
    EmitInstruction(ssaIntToFloat, MakeSSARegister(srtFloat, TempReg),
                   ParamRegs[5], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ParamRegs[5] := MakeSSARegister(srtFloat, TempReg);
  end;

  // filled defaults to 0 (outline only)
  if (ParamCount < 7) or (ParamRegs[6].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ParamRegs[6] := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ParamRegs[6], MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if (ParamRegs[6].Kind = svkRegister) and (ParamRegs[6].RegType = srtFloat) then
  begin
    // Convert float to int for filled parameter
    TempReg := FProgram.AllocRegister(srtInt);
    EmitInstruction(ssaFloatToInt, MakeSSARegister(srtInt, TempReg),
                   ParamRegs[6], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ParamRegs[6] := MakeSSARegister(srtInt, TempReg);
  end;

  // fill_color defaults to border color (ParamRegs[0])
  if (ParamCount < 8) or (ParamRegs[7].Kind = svkNone) then
    ParamRegs[7] := ParamRegs[0]
  else if (ParamRegs[7].Kind = svkRegister) and (ParamRegs[7].RegType = srtFloat) then
  begin
    // Convert float to int for fill_color parameter
    TempReg := FProgram.AllocRegister(srtInt);
    EmitInstruction(ssaFloatToInt, MakeSSARegister(srtInt, TempReg),
                   ParamRegs[7], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ParamRegs[7] := MakeSSARegister(srtInt, TempReg);
  end;

  // Emit ssaGraphicBox instruction
  // We use Dest=None (no result), Src1=color, Src2=x1, Src3=y1
  // Additional parameters stored in PhiSources: x2, y2, angle, filled, fill_color
  EmitInstruction(ssaGraphicBox, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);

  // Add remaining parameters as PhiSources
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(ParamRegs[3], nil);  // x2
  Instr.AddPhiSource(ParamRegs[4], nil);  // y2
  Instr.AddPhiSource(ParamRegs[5], nil);  // angle
  Instr.AddPhiSource(ParamRegs[6], nil);  // filled
  Instr.AddPhiSource(ParamRegs[7], nil);  // fill_color
end;

{ ProcessCircle - Handle CIRCLE command for drawing circles, ellipses, arcs, polygons
  Syntax: CIRCLE [color], x, y, xr [, yr [, sa [, ea [, angle [, inc]]]]]
  Parameters:
    0: color  - color source (0=bg, 1=fg, 2=mc1, 3=mc2)
    1: x      - center X coordinate
    2: y      - center Y coordinate
    3: xr     - X radius (scaled)
    4: yr     - Y radius (optional, default = xr for circle)
    5: sa     - starting arc angle in degrees (optional, default 0)
    6: ea     - ending arc angle in degrees (optional, default 360)
    7: angle  - rotation angle in degrees (optional, default 0)
    8: inc    - degrees between segments (optional, default 2)
}
procedure TSSAGenerator.ProcessCircle(Node: TASTNode);
var
  i, TempReg: Integer;
  ParamValues: array[0..8] of TSSAValue;
  ParamRegs: array[0..8] of TSSAValue;
  ParamCount: Integer;
  Instr: TSSAInstruction;
begin
  // Initialize all parameters to None
  for i := 0 to 8 do
  begin
    ParamValues[i] := MakeSSAValue(svkNone);
    ParamRegs[i] := MakeSSAValue(svkNone);
  end;

  // Get parameter count
  ParamCount := Node.ChildCount;
  if ParamCount < 4 then
  begin
    // CIRCLE requires at least 4 parameters: color, x, y, xr
    WriteLn(StdErr, 'CIRCLE: requires at least 4 parameters (color, x, y, xr)');
    Exit;
  end;

  // Evaluate each parameter expression
  for i := 0 to Min(ParamCount - 1, 8) do
  begin
    ProcessExpression(Node.GetChild(i), ParamValues[i]);

    // Parameters 5-8 (sa, ea, angle, inc) must be float registers
    // Parameters 0-4 (color, x, y, xr, yr) are integers
    if i >= 5 then
    begin
      // Force float for angle parameters
      if ParamValues[i].Kind = svkConstInt then
      begin
        // Convert int constant to float constant
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaLoadConstFloat, ParamRegs[i],
                       MakeSSAConstFloat(ParamValues[i].ConstInt),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkConstFloat then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaLoadConstFloat, ParamRegs[i], ParamValues[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkRegister then
      begin
        // Convert int register to float if needed
        if ParamValues[i].RegType = srtInt then
          ParamRegs[i] := EnsureFloatRegister(ParamValues[i])
        else
          ParamRegs[i] := ParamValues[i];
      end
      else
        ParamRegs[i] := MakeSSAValue(svkNone);
    end
    else
    begin
      // Integer parameters (color, x, y, xr, yr)
      if ParamValues[i].Kind = svkConstInt then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamValues[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkConstFloat then
      begin
        // Convert float constant to int for coordinate params
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i],
                       MakeSSAConstInt(Round(ParamValues[i].ConstFloat)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamValues[i].Kind = svkRegister then
      begin
        // Convert float register to int if needed (VM expects int for coordinates)
        if ParamValues[i].RegType = srtFloat then
        begin
          TempReg := FProgram.AllocRegister(srtInt);
          EmitInstruction(ssaFloatToInt, MakeSSARegister(srtInt, TempReg),
                         ParamValues[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        end
        else
          ParamRegs[i] := ParamValues[i];
      end
      else
        ParamRegs[i] := MakeSSAValue(svkNone);
    end;
  end;

  // Set defaults for optional parameters
  // yr defaults to xr (make a circle)
  if (ParamCount < 5) or (ParamRegs[4].Kind = svkNone) then
    ParamRegs[4] := ParamRegs[3];  // yr = xr

  // sa (start angle) defaults to 0
  if (ParamCount < 6) or (ParamRegs[5].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[5] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[5], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // ea (end angle) defaults to 360
  if (ParamCount < 7) or (ParamRegs[6].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[6] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[6], MakeSSAConstFloat(360.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // angle (rotation) defaults to 0
  if (ParamCount < 8) or (ParamRegs[7].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[7] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[7], MakeSSAConstFloat(0.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // inc (increment) defaults to 2
  if (ParamCount < 9) or (ParamRegs[8].Kind = svkNone) then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    ParamRegs[8] := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, ParamRegs[8], MakeSSAConstFloat(2.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicCircle instruction
  // We use Dest=None (no result), Src1=color, Src2=x, Src3=y
  // Additional parameters stored in PhiSources: xr, yr, sa, ea, angle, inc
  EmitInstruction(ssaGraphicCircle, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);

  // Add remaining parameters as PhiSources
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(ParamRegs[3], nil);  // xr
  Instr.AddPhiSource(ParamRegs[4], nil);  // yr
  Instr.AddPhiSource(ParamRegs[5], nil);  // sa
  Instr.AddPhiSource(ParamRegs[6], nil);  // ea
  Instr.AddPhiSource(ParamRegs[7], nil);  // angle
  Instr.AddPhiSource(ParamRegs[8], nil);  // inc
end;

{ ProcessDraw - Handle DRAW command for drawing dots and lines
  Syntax: DRAW [color], x1, y1 [TO x2, y2] [TO x3, y3] ...
  Parameters:
    0: color - color source (0=bg, 1=fg, 2=mc1, 3=mc2), can be nil/omitted
    1: x1    - first point X coordinate
    2: y1    - first point Y coordinate
    3+: x2,y2, x3,y3... - additional points for TO segments

  Each ssaGraphicDraw instruction draws from current PC to (x,y)
  First point positions PC without drawing (unless only 1 point = draw dot)
  TO segments draw lines from current PC to next point
}
procedure TSSAGenerator.ProcessDraw(Node: TASTNode);
var
  i, TempReg, ParamCount, PointCount: Integer;
  ColorVal, XVal, YVal: TSSAValue;
  ColorReg, XReg, YReg: TSSAValue;
  Instr: TSSAInstruction;
  HasColor: Boolean;
begin
  ParamCount := Node.ChildCount;

  // Minimum: color (or nil), x1, y1 = at least 2 non-nil params
  // Parse: [color], x1, y1 [TO x2, y2]...
  // If first param is nil (comma without value), color is omitted

  if ParamCount < 2 then
  begin
    WriteLn(StdErr, 'DRAW: requires at least x, y coordinates');
    Exit;
  end;

  // Check if color is provided (first child could be nil for omitted color)
  HasColor := (ParamCount >= 3) and Assigned(Node.GetChild(0));

  // Process color if provided
  if HasColor then
  begin
    ProcessExpression(Node.GetChild(0), ColorVal);
    if ColorVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ColorReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ColorReg, ColorVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ColorVal.Kind = svkRegister then
      ColorReg := ColorVal
    else
    begin
      // Default color = 1 (foreground)
      TempReg := FProgram.AllocRegister(srtInt);
      ColorReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ColorReg, MakeSSAConstInt(1),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    i := 1;  // Start processing points from index 1
  end
  else
  begin
    // No color provided - use default (1 = foreground)
    TempReg := FProgram.AllocRegister(srtInt);
    ColorReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ColorReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    // Skip nil first child if color was omitted with comma
    if (ParamCount > 0) and not Assigned(Node.GetChild(0)) then
      i := 1
    else
      i := 0;
  end;

  // Process coordinate pairs
  // Each pair: (x, y)
  // First pair: move PC (mode=0), subsequent pairs: draw line (mode=1)
  PointCount := 0;
  while i < ParamCount - 1 do
  begin
    // Process X coordinate
    if not Assigned(Node.GetChild(i)) then
    begin
      Inc(i);
      Continue;
    end;
    ProcessExpression(Node.GetChild(i), XVal);
    if XVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      XReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, XReg, XVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if XVal.Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      XReg := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, XReg, XVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if XVal.Kind = svkRegister then
      XReg := XVal
    else
      XReg := MakeSSAValue(svkNone);
    Inc(i);

    // Process Y coordinate
    if i >= ParamCount then Break;
    if not Assigned(Node.GetChild(i)) then
    begin
      Inc(i);
      Continue;
    end;
    ProcessExpression(Node.GetChild(i), YVal);
    if YVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      YReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, YReg, YVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if YVal.Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtFloat);
      YReg := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, YReg, YVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if YVal.Kind = svkRegister then
      YReg := YVal
    else
      YReg := MakeSSAValue(svkNone);
    Inc(i);

    // Emit ssaGraphicDraw instruction
    // Dest=None, Src1=color, Src2=x, Src3=y
    // PhiSource[0] = mode: 0=move only (first point), 1=draw line (TO points)
    EmitInstruction(ssaGraphicDraw, MakeSSAValue(svkNone),
                   ColorReg, XReg, YReg);
    Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];

    // Mode: 0 for first point (move PC), 1 for subsequent (draw line)
    // If only one point total, mode=2 means draw dot
    if PointCount = 0 then
    begin
      // Check if this is the only point
      if i >= ParamCount then
        Instr.AddPhiSource(MakeSSAConstInt(2), nil)  // Single point = draw dot
      else
        Instr.AddPhiSource(MakeSSAConstInt(0), nil); // First point = move PC
    end
    else
      Instr.AddPhiSource(MakeSSAConstInt(1), nil);  // TO point = draw line

    Inc(PointCount);
  end;
end;

{ ProcessLocate - Handle LOCATE command for positioning pixel cursor
  Syntax: LOCATE x, y
  Parameters:
    0: x - pixel cursor X coordinate
    1: y - pixel cursor Y coordinate
}
procedure TSSAGenerator.ProcessLocate(Node: TASTNode);
var
  TempReg: Integer;
  XVal, YVal: TSSAValue;
  XReg, YReg: TSSAValue;
begin
  if Node.ChildCount < 2 then
  begin
    WriteLn(StdErr, 'LOCATE: requires x, y coordinates');
    Exit;
  end;

  // Process X coordinate
  ProcessExpression(Node.GetChild(0), XVal);
  if XVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    XReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, XReg, XVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if XVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    XReg := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, XReg, XVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if XVal.Kind = svkRegister then
    XReg := XVal
  else
    XReg := MakeSSAValue(svkNone);

  // Process Y coordinate
  ProcessExpression(Node.GetChild(1), YVal);
  if YVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    YReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, YReg, YVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if YVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtFloat);
    YReg := MakeSSARegister(srtFloat, TempReg);
    EmitInstruction(ssaLoadConstFloat, YReg, YVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if YVal.Kind = svkRegister then
    YReg := YVal
  else
    YReg := MakeSSAValue(svkNone);

  // Emit ssaGraphicLocate instruction
  // Dest=None, Src1=x, Src2=y, Src3=None
  EmitInstruction(ssaGraphicLocate, MakeSSAValue(svkNone),
                 XReg, YReg, MakeSSAValue(svkNone));
end;

{ ProcessColor - Handle COLOR command for setting screen area colors
  Syntax: COLOR source, color
  Source: 0=background, 1=foreground, 2=multicolor1, 3=multicolor2,
          4=border, 5=character, 6=80col-background
}
procedure TSSAGenerator.ProcessColor(Node: TASTNode);
var
  SourceVal, ColorVal: TSSAValue;
  SourceReg, ColorReg: TSSAValue;
  TempReg: Integer;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 2 then
  begin
    WriteLn(StdErr, 'COLOR: requires source, color');
    Exit;
  end;

  // Process source number
  ProcessExpression(Node.GetChild(0), SourceVal);
  if SourceVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    SourceReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, SourceReg, SourceVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if SourceVal.Kind = svkRegister then
    SourceReg := SourceVal
  else
    SourceReg := MakeSSAValue(svkNone);

  // Process color value
  ProcessExpression(Node.GetChild(1), ColorVal);
  if ColorVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ColorReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ColorReg, ColorVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if ColorVal.Kind = svkRegister then
    ColorReg := ColorVal
  else
    ColorReg := MakeSSAValue(svkNone);

  // Emit ssaGraphicColor: Src1=source, Src2=color
  EmitInstruction(ssaGraphicColor, MakeSSAValue(svkNone),
                 SourceReg, ColorReg, MakeSSAValue(svkNone));
end;

{ ProcessSetColor - Handle SETCOLOR command for modifying palette entries
  Syntax: SETCOLOR index, R, G, B [, A]
  Index: 0-255 (palette entry index)
  R, G, B: 0-255 (color components)
  A: 0-255 (alpha, optional, defaults to 255)
}
procedure TSSAGenerator.ProcessSetColor(Node: TASTNode);
var
  IndexVal, RVal, GVal, BVal, AVal: TSSAValue;
  IndexReg, RReg, GReg, BReg, AReg: TSSAValue;
  TempReg: Integer;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 4 then
  begin
    WriteLn(StdErr, 'SETCOLOR: requires index, R, G, B [, A]');
    Exit;
  end;

  // Process palette index
  ProcessExpression(Node.GetChild(0), IndexVal);
  IndexReg := EnsureIntRegister(IndexVal);

  // Process R component
  ProcessExpression(Node.GetChild(1), RVal);
  RReg := EnsureIntRegister(RVal);

  // Process G component
  ProcessExpression(Node.GetChild(2), GVal);
  GReg := EnsureIntRegister(GVal);

  // Process B component
  ProcessExpression(Node.GetChild(3), BVal);
  BReg := EnsureIntRegister(BVal);

  // Process optional A (alpha) component - defaults to 255
  if Node.ChildCount >= 5 then
  begin
    ProcessExpression(Node.GetChild(4), AVal);
    AReg := EnsureIntRegister(AVal);
  end
  else
  begin
    // Default alpha = 255
    TempReg := FProgram.AllocRegister(srtInt);
    AReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, AReg, MakeSSAConstInt(255),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaSetColor: Src1=index, Src2=R, Dest=G (packed in Immediate with B and A)
  // We use Immediate to store additional values: low word = B, high word = A
  EmitInstruction(ssaSetColor, GReg, IndexReg, RReg, BReg);
  // Add alpha as phi source to the last instruction
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(AReg, nil);
end;

{ ProcessWidth - Handle WIDTH command for setting line width
  Syntax: WIDTH n (1 or 2)
}
procedure TSSAGenerator.ProcessWidth(Node: TASTNode);
var
  WidthVal, WidthReg: TSSAValue;
  TempReg: Integer;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'WIDTH: requires width value');
    Exit;
  end;

  ProcessExpression(Node.GetChild(0), WidthVal);
  if WidthVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    WidthReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, WidthReg, WidthVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if WidthVal.Kind = svkRegister then
    WidthReg := WidthVal
  else
    WidthReg := MakeSSAValue(svkNone);

  // Emit ssaGraphicWidth: Src1=width
  EmitInstruction(ssaGraphicWidth, MakeSSAValue(svkNone),
                 WidthReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessScale - Handle SCALE command for coordinate scaling
  Syntax: SCALE n [,xmax, ymax]
  n=0: turn off, n=1: turn on with optional max coordinates
}
procedure TSSAGenerator.ProcessScale(Node: TASTNode);
var
  EnableVal, XMaxVal, YMaxVal: TSSAValue;
  EnableReg, XMaxReg, YMaxReg: TSSAValue;
  TempReg: Integer;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'SCALE: requires enable flag');
    Exit;
  end;

  // Process enable flag
  ProcessExpression(Node.GetChild(0), EnableVal);
  if EnableVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    EnableReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, EnableReg, EnableVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if EnableVal.Kind = svkRegister then
    EnableReg := EnableVal
  else
    EnableReg := MakeSSAValue(svkNone);

  // Process optional xmax
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), XMaxVal);
    if XMaxVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      XMaxReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, XMaxReg, XMaxVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if XMaxVal.Kind = svkRegister then
      XMaxReg := XMaxVal
    else
      XMaxReg := MakeSSAValue(svkNone);
  end
  else
  begin
    // Pass 0 to indicate "use C128 default" (1023 for hi-res, 2047 for multicolor)
    TempReg := FProgram.AllocRegister(srtInt);
    XMaxReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, XMaxReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Process optional ymax
  if Node.ChildCount > 2 then
  begin
    ProcessExpression(Node.GetChild(2), YMaxVal);
    if YMaxVal.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      YMaxReg := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, YMaxReg, YMaxVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if YMaxVal.Kind = svkRegister then
      YMaxReg := YMaxVal
    else
      YMaxReg := MakeSSAValue(svkNone);
  end
  else
  begin
    // Pass 0 to indicate "use C128 default" (1023 for all modes)
    TempReg := FProgram.AllocRegister(srtInt);
    YMaxReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, YMaxReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicScale: Src1=enable, Src2=xmax, Src3=ymax
  EmitInstruction(ssaGraphicScale, MakeSSAValue(svkNone),
                 EnableReg, XMaxReg, YMaxReg);
end;

{ ProcessPaint - Handle PAINT command for flood fill
  Syntax: PAINT [source], x, y [,mode]
  source: 0-3 color source (optional, default=1)
  x, y: starting coordinates
  mode: 0=bounded by source, 1=bounded by non-background
}
procedure TSSAGenerator.ProcessPaint(Node: TASTNode);
var
  SourceVal, XVal, YVal, ModeVal: TSSAValue;
  SourceReg, XReg, YReg, ModeReg: TSSAValue;
  Instr: TSSAInstruction;
  TempReg: Integer;

  function MaterializeInt(Val: TSSAValue): TSSAValue;
  begin
    if Val.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(Trunc(Val.ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkRegister then
    begin
      // Convert float register to int if needed
      if Val.RegType = srtFloat then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        Result := Val;
    end
    else
      Result := MakeSSAValue(svkNone);
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 2 then
  begin
    WriteLn(StdErr, 'PAINT: requires at least x, y coordinates');
    Exit;
  end;

  // PAINT can have 2, 3, or 4 params
  // 2 params: x, y (source=1, mode=0)
  // 3 params: source, x, y OR x, y, mode (ambiguous, assume source, x, y)
  // 4 params: source, x, y, mode
  if Node.ChildCount = 2 then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    SourceReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, SourceReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ProcessExpression(Node.GetChild(0), XVal);
    XReg := MaterializeInt(XVal);
    ProcessExpression(Node.GetChild(1), YVal);
    YReg := MaterializeInt(YVal);
    TempReg := FProgram.AllocRegister(srtInt);
    ModeReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ModeReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if Node.ChildCount = 3 then
  begin
    ProcessExpression(Node.GetChild(0), SourceVal);
    SourceReg := MaterializeInt(SourceVal);
    ProcessExpression(Node.GetChild(1), XVal);
    XReg := MaterializeInt(XVal);
    ProcessExpression(Node.GetChild(2), YVal);
    YReg := MaterializeInt(YVal);
    TempReg := FProgram.AllocRegister(srtInt);
    ModeReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ModeReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    ProcessExpression(Node.GetChild(0), SourceVal);
    SourceReg := MaterializeInt(SourceVal);
    ProcessExpression(Node.GetChild(1), XVal);
    XReg := MaterializeInt(XVal);
    ProcessExpression(Node.GetChild(2), YVal);
    YReg := MaterializeInt(YVal);
    ProcessExpression(Node.GetChild(3), ModeVal);
    ModeReg := MaterializeInt(ModeVal);
  end;

  // Emit ssaGraphicPaint: Src1=source, Src2=x, Src3=y, PhiSources[0]=mode
  EmitInstruction(ssaGraphicPaint, MakeSSAValue(svkNone),
                 SourceReg, XReg, YReg);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(ModeReg, nil);
end;

{ ProcessWindow - Handle WINDOW command for text window definition
  Syntax: WINDOW col1, row1, col2, row2 [,clear]
}
procedure TSSAGenerator.ProcessWindow(Node: TASTNode);
var
  Col1Val, Row1Val, Col2Val, Row2Val, ClearVal: TSSAValue;
  Col1Reg, Row1Reg, Col2Reg, Row2Reg, ClearReg: TSSAValue;
  Instr: TSSAInstruction;
  TempReg: Integer;

  function MaterializeInt(Val: TSSAValue): TSSAValue;
  begin
    if Val.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(Trunc(Val.ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkRegister then
    begin
      if Val.RegType = srtInt then
        Result := Val  // Already int, use as-is
      else if Val.RegType = srtFloat then
      begin
        // Convert float register to int register
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        Result := MakeSSAValue(svkNone);  // String - invalid
    end
    else
      Result := MakeSSAValue(svkNone);
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 4 then
  begin
    WriteLn(StdErr, 'WINDOW: requires col1, row1, col2, row2');
    Exit;
  end;

  ProcessExpression(Node.GetChild(0), Col1Val);
  Col1Reg := MaterializeInt(Col1Val);
  ProcessExpression(Node.GetChild(1), Row1Val);
  Row1Reg := MaterializeInt(Row1Val);
  ProcessExpression(Node.GetChild(2), Col2Val);
  Col2Reg := MaterializeInt(Col2Val);
  ProcessExpression(Node.GetChild(3), Row2Val);
  Row2Reg := MaterializeInt(Row2Val);

  // Optional clear flag
  if Node.ChildCount > 4 then
  begin
    ProcessExpression(Node.GetChild(4), ClearVal);
    ClearReg := MaterializeInt(ClearVal);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ClearReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ClearReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicWindow: Src1=col1, Src2=row1, Src3=col2, PhiSources[0]=row2, [1]=clear
  EmitInstruction(ssaGraphicWindow, MakeSSAValue(svkNone),
                 Col1Reg, Row1Reg, Col2Reg);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Row2Reg, nil);
  Instr.AddPhiSource(ClearReg, nil);
end;

{ ProcessSShape - Handle SSHAPE command for saving bitmap to string
  Syntax: SSHAPE A$, x1, y1 [,x2, y2]
}
procedure TSSAGenerator.ProcessSShape(Node: TASTNode);
var
  X1Val, Y1Val, X2Val, Y2Val: TSSAValue;
  X1Reg, Y1Reg, X2Reg, Y2Reg: TSSAValue;
  DestVar: TSSAValue;
  Instr: TSSAInstruction;
  TempReg: Integer;

  function MaterializeInt(Val: TSSAValue): TSSAValue;
  begin
    if Val.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(Trunc(Val.ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkRegister then
    begin
      // Convert float register to int if needed
      if Val.RegType = srtFloat then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        Result := Val;
    end
    else
      Result := MakeSSAValue(svkNone);
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 3 then
  begin
    WriteLn(StdErr, 'SSHAPE: requires string var, x1, y1');
    Exit;
  end;

  // First child is the destination string variable
  // Get the register associated with this variable (like ProcessAssignment does)
  if Node.GetChild(0).NodeType = antIdentifier then
    DestVar := GetOrAllocateVariable(VarToStr(Node.GetChild(0).Value))
  else
  begin
    WriteLn(StdErr, 'SSHAPE: first parameter must be a string variable');
    Exit;
  end;

  ProcessExpression(Node.GetChild(1), X1Val);
  X1Reg := MaterializeInt(X1Val);
  ProcessExpression(Node.GetChild(2), Y1Val);
  Y1Reg := MaterializeInt(Y1Val);

  // Optional x2, y2
  if Node.ChildCount > 3 then
  begin
    ProcessExpression(Node.GetChild(3), X2Val);
    X2Reg := MaterializeInt(X2Val);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    X2Reg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, X2Reg, MakeSSAConstInt(-1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  if Node.ChildCount > 4 then
  begin
    ProcessExpression(Node.GetChild(4), Y2Val);
    Y2Reg := MaterializeInt(Y2Val);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    Y2Reg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, Y2Reg, MakeSSAConstInt(-1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicSShape: Dest=string var, Src1=x1, Src2=y1, Src3=x2, PhiSources[0]=y2
  EmitInstruction(ssaGraphicSShape, DestVar, X1Reg, Y1Reg, X2Reg);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Y2Reg, nil);
end;

{ ProcessGShape - Handle GSHAPE command for loading string to bitmap
  Syntax: GSHAPE A$, x, y [,mode]
  mode: 0=as-is, 1=invert, 2=OR, 3=AND, 4=XOR
}
procedure TSSAGenerator.ProcessGShape(Node: TASTNode);
var
  XVal, YVal, ModeVal: TSSAValue;
  SrcVar, XReg, YReg, ModeReg: TSSAValue;
  Instr: TSSAInstruction;
  TempReg: Integer;

  function MaterializeInt(Val: TSSAValue): TSSAValue;
  begin
    if Val.Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkConstFloat then
    begin
      // Convert float constant to int
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(Trunc(Val.ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Val.Kind = svkRegister then
    begin
      // Convert float register to int if needed
      if Val.RegType = srtFloat then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        Result := Val;
    end
    else
      Result := MakeSSAValue(svkNone);
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'GSHAPE: requires string variable');
    Exit;
  end;

  // First child is the source string variable
  ProcessExpression(Node.GetChild(0), SrcVar);

  // Optional x, y coordinates (default to PC)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), XVal);
    XReg := MaterializeInt(XVal);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    XReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, XReg, MakeSSAConstInt(-1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  if Node.ChildCount > 2 then
  begin
    ProcessExpression(Node.GetChild(2), YVal);
    YReg := MaterializeInt(YVal);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    YReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, YReg, MakeSSAConstInt(-1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Optional mode
  if Node.ChildCount > 3 then
  begin
    ProcessExpression(Node.GetChild(3), ModeVal);
    ModeReg := MaterializeInt(ModeVal);
  end
  else
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    ModeReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, ModeReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit ssaGraphicGShape: Src1=string, Src2=x, Src3=y, PhiSources[0]=mode
  EmitInstruction(ssaGraphicGShape, MakeSSAValue(svkNone),
                 SrcVar, XReg, YReg);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(ModeReg, nil);
end;

{ ProcessGList - Handle GLIST command for listing SDL2 video modes }
procedure TSSAGenerator.ProcessGList(Node: TASTNode);
begin
  if FCurrentBlock = nil then Exit;
  // GLIST has no parameters
  EmitInstruction(ssaGraphicGList, MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessPLoad - Handle PLOAD command for loading palette from JSON file
  Syntax: PLOAD "filename"
}
procedure TSSAGenerator.ProcessPLoad(Node: TASTNode);
var
  FileNameVal, FileNameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'PLOAD requires a filename');
    Exit;
  end;

  // Get filename string
  ProcessExpression(Node.GetChild(0), FileNameVal);
  FileNameReg := EnsureStringRegister(FileNameVal);

  // Emit PLOAD instruction
  EmitInstruction(ssaPLoad, MakeSSAValue(svkNone),
                 FileNameReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessPSave - Handle PSAVE command for saving palette to JSON file
  Syntax: PSAVE "filename"
}
procedure TSSAGenerator.ProcessPSave(Node: TASTNode);
var
  FileNameVal, FileNameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'PSAVE requires a filename');
    Exit;
  end;

  // Get filename string
  ProcessExpression(Node.GetChild(0), FileNameVal);
  FileNameReg := EnsureStringRegister(FileNameVal);

  // Emit PSAVE instruction
  EmitInstruction(ssaPSave, MakeSSAValue(svkNone),
                 FileNameReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessPRst - Handle PRST command for resetting palette to C64 default
  Syntax: PRST
}
procedure TSSAGenerator.ProcessPRst(Node: TASTNode);
begin
  if FCurrentBlock = nil then Exit;
  // PRST has no parameters - just emit the instruction
  EmitInstruction(ssaPRst, MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ============================================================================
  SOUND COMMANDS
  ============================================================================ }

{ ProcessVol - Handle VOL command for setting master volume
  Syntax: VOL n (0-15)
}
procedure TSSAGenerator.ProcessVol(Node: TASTNode);
var
  TempReg: Integer;
  VolVal, VolReg: TSSAValue;
begin
  if FCurrentBlock = nil then
    Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'VOL: requires volume parameter (0-15)');
    Exit;
  end;

  // Process volume expression
  ProcessExpression(Node.GetChild(0), VolVal);

  // Materialize to int register if needed
  if VolVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    VolReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, VolReg, VolVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if VolVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    VolReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, VolReg, MakeSSAConstInt(Trunc(VolVal.ConstFloat)),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if (VolVal.Kind = svkRegister) and (VolVal.RegType = srtFloat) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    VolReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaFloatToInt, VolReg, VolVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    VolReg := VolVal;

  // Emit ssaSoundVol: Src1 = volume (0-15)
  EmitInstruction(ssaSoundVol, MakeSSAValue(svkNone),
                 VolReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessSound - Handle SOUND command for sound effects
  Syntax: SOUND vc, freq, dur [, dir [, min [, sv [, wf [, pw]]]]]
  Parameters:
    0: vc    - voice (1-3)
    1: freq  - frequency (0-65535)
    2: dur   - duration in 1/60 sec
    3: dir   - sweep direction (0=up, 1=down, 2=oscillate) [optional]
    4: min   - minimum frequency for sweep [optional]
    5: sv    - step value for sweep [optional]
    6: wf    - waveform (0=tri, 1=saw, 2=pulse, 3=noise) [optional]
    7: pw    - pulse width (0-4095) [optional]
}
procedure TSSAGenerator.ProcessSound(Node: TASTNode);
var
  TempReg, i: Integer;
  ParamVals: array[0..7] of TSSAValue;
  ParamRegs: array[0..7] of TSSAValue;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 3 then
  begin
    WriteLn(StdErr, 'SOUND: requires at least voice, frequency, duration');
    Exit;
  end;

  // Initialize all params to none/zero
  for i := 0 to 7 do
    ParamVals[i] := MakeSSAValue(svkNone);

  // Process available parameters
  for i := 0 to Min(Node.ChildCount - 1, 7) do
    ProcessExpression(Node.GetChild(i), ParamVals[i]);

  // Materialize parameters to registers
  for i := 0 to 7 do
  begin
    if ParamVals[i].Kind = svkNone then
    begin
      // Default value: 0
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(0),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamVals[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamVals[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamVals[i].Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(Trunc(ParamVals[i].ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (ParamVals[i].Kind = svkRegister) and (ParamVals[i].RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, ParamRegs[i], ParamVals[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      ParamRegs[i] := ParamVals[i];
  end;

  // Emit ssaSoundSound: Src1=voice, Src2=freq, Src3=duration
  // Additional params packed via PhiSources
  Instr := TSSAInstruction.Create(ssaSoundSound);
  Instr.Dest := MakeSSAValue(svkNone);
  Instr.Src1 := ParamRegs[0];  // voice
  Instr.Src2 := ParamRegs[1];  // freq
  Instr.Src3 := ParamRegs[2];  // duration
  Instr.SourceLine := Node.SourceLine;
  // Pack remaining params as PhiSources
  Instr.AddPhiSource(ParamRegs[3], nil);  // dir
  Instr.AddPhiSource(ParamRegs[4], nil);  // min
  Instr.AddPhiSource(ParamRegs[5], nil);  // sv
  Instr.AddPhiSource(ParamRegs[6], nil);  // wf
  Instr.AddPhiSource(ParamRegs[7], nil);  // pw
  FCurrentBlock.AddInstruction(Instr);
end;

{ ProcessEnvelope - Handle ENVELOPE command for defining instruments
  Syntax: ENVELOPE e[, a[, d[, s[, r[, wf[, pw]]]]]]
  Parameters:
    0: e   - envelope number (0-9)
    1: a   - attack rate (0-15) [optional]
    2: d   - decay rate (0-15) [optional]
    3: s   - sustain level (0-15) [optional]
    4: r   - release rate (0-15) [optional]
    5: wf  - waveform (0-4) [optional]
    6: pw  - pulse width (0-4095) [optional]
}
procedure TSSAGenerator.ProcessEnvelope(Node: TASTNode);
var
  TempReg, i: Integer;
  ParamVals: array[0..6] of TSSAValue;
  ParamRegs: array[0..6] of TSSAValue;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'ENVELOPE: requires at least envelope number (0-9)');
    Exit;
  end;

  // Initialize all params to none
  for i := 0 to 6 do
    ParamVals[i] := MakeSSAValue(svkNone);

  // Process available parameters
  for i := 0 to Min(Node.ChildCount - 1, 6) do
    ProcessExpression(Node.GetChild(i), ParamVals[i]);

  // Materialize parameters to registers
  for i := 0 to 6 do
  begin
    if ParamVals[i].Kind = svkNone then
    begin
      // Default: use -1 to indicate "use predefined default"
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(-1),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamVals[i].Kind = svkConstInt then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamVals[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ParamVals[i].Kind = svkConstFloat then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(Trunc(ParamVals[i].ConstFloat)),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if (ParamVals[i].Kind = svkRegister) and (ParamVals[i].RegType = srtFloat) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, ParamRegs[i], ParamVals[i],
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      ParamRegs[i] := ParamVals[i];
  end;

  // Emit ssaSoundEnvelope: Src1=envelope#
  // ADSR + wf + pw packed via PhiSources
  Instr := TSSAInstruction.Create(ssaSoundEnvelope);
  Instr.Dest := MakeSSAValue(svkNone);
  Instr.Src1 := ParamRegs[0];  // envelope number
  Instr.Src2 := MakeSSAValue(svkNone);
  Instr.Src3 := MakeSSAValue(svkNone);
  Instr.SourceLine := Node.SourceLine;
  // Pack ADSR + wf + pw as PhiSources
  Instr.AddPhiSource(ParamRegs[1], nil);  // attack
  Instr.AddPhiSource(ParamRegs[2], nil);  // decay
  Instr.AddPhiSource(ParamRegs[3], nil);  // sustain
  Instr.AddPhiSource(ParamRegs[4], nil);  // release
  Instr.AddPhiSource(ParamRegs[5], nil);  // waveform
  Instr.AddPhiSource(ParamRegs[6], nil);  // pulse width
  FCurrentBlock.AddInstruction(Instr);
end;

{ ProcessTempo - Handle TEMPO command for setting playback speed
  Syntax: TEMPO n (0-255)
}
procedure TSSAGenerator.ProcessTempo(Node: TASTNode);
var
  TempReg: Integer;
  TempoVal, TempoReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'TEMPO: requires tempo parameter (0-255)');
    Exit;
  end;

  // Process tempo expression
  ProcessExpression(Node.GetChild(0), TempoVal);

  // Materialize to int register if needed
  if TempoVal.Kind = svkConstInt then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    TempoReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, TempoReg, TempoVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if TempoVal.Kind = svkConstFloat then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    TempoReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaLoadConstInt, TempoReg, MakeSSAConstInt(Trunc(TempoVal.ConstFloat)),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if (TempoVal.Kind = svkRegister) and (TempoVal.RegType = srtFloat) then
  begin
    TempReg := FProgram.AllocRegister(srtInt);
    TempoReg := MakeSSARegister(srtInt, TempReg);
    EmitInstruction(ssaFloatToInt, TempoReg, TempoVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    TempoReg := TempoVal;

  // Emit ssaSoundTempo: Src1 = tempo (0-255)
  EmitInstruction(ssaSoundTempo, MakeSSAValue(svkNone),
                 TempoReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessPlay - Handle PLAY command for playing music strings
  Syntax: PLAY "music string"
  The string contains notes (C,D,E,F,G,A,B), durations (W,H,Q,I,S),
  control characters (V=voice, O=octave, T=envelope, U=volume, X=filter)
}
procedure TSSAGenerator.ProcessPlay(Node: TASTNode);
var
  TempReg: Integer;
  StrVal, StrReg: TSSAValue;
begin
  if FCurrentBlock = nil then
    Exit;

  if Node.ChildCount < 1 then
  begin
    WriteLn(StdErr, 'PLAY: requires music string parameter');
    Exit;
  end;

  // Process string expression
  ProcessExpression(Node.GetChild(0), StrVal);

  // Handle different value types
  if StrVal.Kind = svkConstString then
  begin
    TempReg := FProgram.AllocRegister(srtString);
    StrReg := MakeSSARegister(srtString, TempReg);
    EmitInstruction(ssaLoadConstString, StrReg, StrVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else if (StrVal.Kind = svkRegister) and (StrVal.RegType = srtString) then
    StrReg := StrVal
  else
  begin
    WriteLn(StdErr, 'PLAY: parameter must be a string');
    Exit;
  end;

  // Emit ssaSoundPlay: Src1 = string register
  EmitInstruction(ssaSoundPlay, MakeSSAValue(svkNone),
                 StrReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

{ ProcessFilter - Handle FILTER command for SID filter parameters
  Syntax: FILTER cf, lp, bp, hp, res
  Parameters:
    0: cf  - cutoff frequency (0-2047)
    1: lp  - low-pass filter (0=off, 1=on)
    2: bp  - band-pass filter (0=off, 1=on)
    3: hp  - high-pass filter (0=off, 1=on)
    4: res - resonance (0-15)
}
procedure TSSAGenerator.ProcessFilter(Node: TASTNode);
var
  TempReg, i: Integer;
  ParamVals: array[0..4] of TSSAValue;
  ParamRegs: array[0..4] of TSSAValue;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount < 5 then
  begin
    WriteLn(StdErr, 'FILTER: requires all 5 parameters (cf, lp, bp, hp, res)');
    Exit;
  end;

  // Process all 5 parameters
  for i := 0 to 4 do
    ProcessExpression(Node.GetChild(i), ParamVals[i]);

  // Materialize parameters to registers
  // Parameter 0 (cutoff frequency) must be a FLOAT register (VM reads FFloatRegs)
  // Parameters 1-4 (lp, bp, hp, res) must be INT registers (VM reads FIntRegs)
  for i := 0 to 4 do
  begin
    if i = 0 then
    begin
      // Cutoff frequency -> float register
      if ParamVals[i].Kind = svkConstInt then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaLoadConstFloat, ParamRegs[i],
                       MakeSSAConstFloat(Double(ParamVals[i].ConstInt)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamVals[i].Kind = svkConstFloat then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaLoadConstFloat, ParamRegs[i], ParamVals[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if (ParamVals[i].Kind = svkRegister) and (ParamVals[i].RegType = srtInt) then
      begin
        TempReg := FProgram.AllocRegister(srtFloat);
        ParamRegs[i] := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, ParamRegs[i], ParamVals[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        ParamRegs[i] := ParamVals[i];  // Already a float register
    end
    else
    begin
      // Parameters 1-4 -> int registers
      if ParamVals[i].Kind = svkConstInt then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i], ParamVals[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if ParamVals[i].Kind = svkConstFloat then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaLoadConstInt, ParamRegs[i], MakeSSAConstInt(Trunc(ParamVals[i].ConstFloat)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if (ParamVals[i].Kind = svkRegister) and (ParamVals[i].RegType = srtFloat) then
      begin
        TempReg := FProgram.AllocRegister(srtInt);
        ParamRegs[i] := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, ParamRegs[i], ParamVals[i],
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        ParamRegs[i] := ParamVals[i];
    end;
  end;

  // Emit ssaSoundFilter: Src1=cf, Src2=lp, Src3=bp
  // hp and res packed via PhiSources
  Instr := TSSAInstruction.Create(ssaSoundFilter);
  Instr.Dest := MakeSSAValue(svkNone);
  Instr.Src1 := ParamRegs[0];  // cutoff frequency
  Instr.Src2 := ParamRegs[1];  // low-pass
  Instr.Src3 := ParamRegs[2];  // band-pass
  Instr.SourceLine := Node.SourceLine;
  // Pack remaining params as PhiSources
  Instr.AddPhiSource(ParamRegs[3], nil);  // high-pass
  Instr.AddPhiSource(ParamRegs[4], nil);  // resonance
  FCurrentBlock.AddInstruction(Instr);
end;

{ DATA/READ/RESTORE implementation }
procedure TSSAGenerator.ProcessData(Node: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  DataVal: TSSAValue;
begin
  // Note: No FCurrentBlock check here - DATA statements just add values
  // to the DATA pool and don't need a current block

  // Each child is a literal value to add to the DATA pool
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Child.NodeType = antLiteral then
    begin
      // Determine type of data and emit appropriate ssaDataAdd
      if VarIsStr(Child.Value) then
      begin
        DataVal := MakeSSAConstString(string(Child.Value));
        EmitInstruction(ssaDataAdd, MakeSSAValue(svkNone), DataVal,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarIsOrdinal(Child.Value) then
      begin
        DataVal := MakeSSAConstInt(Integer(Child.Value));
        EmitInstruction(ssaDataAdd, MakeSSAValue(svkNone), DataVal,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // Float
        DataVal := MakeSSAConstFloat(Double(Child.Value));
        EmitInstruction(ssaDataAdd, MakeSSAValue(svkNone), DataVal,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
  end;
end;

procedure TSSAGenerator.ProcessRead(Node: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  VarName: string;
  DestReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // Each child is a variable to read into
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    if Child.NodeType = antIdentifier then
    begin
      VarName := VarToStr(Child.Value);
      DestReg := GetOrAllocateVariable(VarName);

      // Emit ssaDataRead with type hint from variable suffix
      if VarName.EndsWith('$') then
        EmitInstruction(ssaDataRead, DestReg, MakeSSAConstInt(Ord(srtString)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if VarName.EndsWith('%') then
        EmitInstruction(ssaDataRead, DestReg, MakeSSAConstInt(Ord(srtInt)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        // Default to float
        EmitInstruction(ssaDataRead, DestReg, MakeSSAConstInt(Ord(srtFloat)),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if Child.NodeType = antArrayAccess then
    begin
      // Array element - more complex, need to read into temp then store to array
      // For now just emit a warning, can be implemented later
      WriteLn(StdErr, '[SSA] READ into array elements not yet implemented');
    end;
  end;
end;

procedure TSSAGenerator.ProcessRestore(Node: TASTNode);
var
  LineVal: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  if Node.ChildCount > 0 then
  begin
    // RESTORE with line number
    LineVal := MakeSSAConstInt(Integer(Node.GetChild(0).Value));
    EmitInstruction(ssaDataRestore, MakeSSAValue(svkNone), LineVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    // RESTORE without line number - reset to beginning
    EmitInstruction(ssaDataRestore, MakeSSAValue(svkNone), MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

{ Input commands implementation }
procedure TSSAGenerator.ProcessGet(Node: TASTNode);
var
  VarName: string;
  DestReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // GET A$ - non-blocking character input
  // Child[0] = variable to store character
  if Node.ChildCount > 0 then
  begin
    VarName := string(Node.GetChild(0).Value);
    DestReg := GetOrAllocateVariable(VarName);
    EmitInstruction(ssaGet, DestReg, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessGetkey(Node: TASTNode);
var
  VarName: string;
  DestReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // GETKEY A$ - blocking character input (waits for keypress)
  // Child[0] = variable to store character
  if Node.ChildCount > 0 then
  begin
    VarName := string(Node.GetChild(0).Value);
    DestReg := GetOrAllocateVariable(VarName);
    EmitInstruction(ssaGetkey, DestReg, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

{ Formatted output implementation }
procedure TSSAGenerator.ProcessPrintUsing(Node: TASTNode);
var
  i: Integer;
  FormatVal, ValueVal: TSSAValue;
  FormatReg, ValueReg: TSSAValue;
  Instr: TSSAInstruction;
  EndsWithSeparator: Boolean;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 2 then Exit; // Need at least format and one value

  // PRINT USING format$; value1, value2, ...
  // Child[0] = format string
  // Child[1..n] = values to print (may include separator nodes)
  ProcessExpression(Node.GetChild(0), FormatVal);
  // Ensure format is in a string register (constants need to be loaded)
  FormatReg := EnsureStringRegister(FormatVal);

  // Check if statement ends with separator (suppress newline)
  EndsWithSeparator := (Node.ChildCount > 1) and
                       (Node.GetChild(Node.ChildCount - 1).NodeType = antSeparator);

  // Emit one instruction per value
  for i := 1 to Node.ChildCount - 1 do
  begin
    if Node.GetChild(i).NodeType = antSeparator then
      Continue; // Skip separators

    ProcessExpression(Node.GetChild(i), ValueVal);
    // Ensure value is in a float register (constants need to be loaded)
    ValueReg := EnsureFloatRegister(ValueVal);

    // Emit PRINT USING with format and value (both must be registers)
    Instr := TSSAInstruction.Create(ssaPrintUsing);
    Instr.Dest := MakeSSAValue(svkNone);
    Instr.Src1 := FormatReg;  // Format string register
    Instr.Src2 := ValueReg;   // Value register
    Instr.Src3 := MakeSSAValue(svkNone);
    Instr.SourceLine := Node.SourceLine;
    FCurrentBlock.AddInstruction(Instr);
  end;

  // Add newline unless statement ends with separator (; or ,)
  if not EndsWithSeparator then
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Reset reverse mode after PRINT USING (C128 behavior)
  EmitInstruction(ssaPrintEnd, MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessPudef(Node: TASTNode);
var
  FormatVal: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // PUDEF " ,.$" - redefine PRINT USING format characters
  // Child[0] = format string (4 chars: filler, comma, decimal, dollar)
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FormatVal);
    EmitInstruction(ssaPudef, MakeSSAValue(svkNone), FormatVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessChar(Node: TASTNode);
var
  ModeVal, ColVal, RowVal, TextVal, ReverseVal: TSSAValue;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 4 then Exit; // Need mode, col, row, text

  // CHAR mode, col, row, "text" [,reverse]
  // All parameters MUST be materialized into registers because the bytecode
  // compiler only maps svkRegister operands. Constants (svkConstInt/svkConstString)
  // would be silently dropped, leaving register index 0 in the bytecode.
  ProcessExpression(Node.GetChild(0), ModeVal);
  ModeVal := EnsureIntRegister(ModeVal);
  ProcessExpression(Node.GetChild(1), ColVal);
  ColVal := EnsureIntRegister(ColVal);
  ProcessExpression(Node.GetChild(2), RowVal);
  RowVal := EnsureIntRegister(RowVal);
  ProcessExpression(Node.GetChild(3), TextVal);
  TextVal := EnsureStringRegister(TextVal);

  if Node.ChildCount > 4 then
  begin
    ProcessExpression(Node.GetChild(4), ReverseVal);
    ReverseVal := EnsureIntRegister(ReverseVal);
  end
  else
    ReverseVal := MakeSSAConstInt(0);

  // Emit CHAR instruction with all parameters
  Instr := TSSAInstruction.Create(ssaChar);
  Instr.Dest := MakeSSAValue(svkNone);
  Instr.Src1 := ModeVal;
  Instr.Src2 := ColVal;
  Instr.Src3 := RowVal;
  Instr.SourceLine := Node.SourceLine;
  // Pack text and reverse as PhiSources
  Instr.AddPhiSource(TextVal, nil);
  Instr.AddPhiSource(ReverseVal, nil);
  FCurrentBlock.AddInstruction(Instr);
end;

procedure TSSAGenerator.ProcessLoad(Node: TASTNode);
var
  FilenameVal: TSSAValue;
  FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // LOAD "filename"
  // The filename is the first child (should be a string expression)
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    // Ensure we have a string register
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    // No filename provided - emit empty string
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit LOAD instruction with filename in Src1
  EmitInstruction(ssaLoad, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSave(Node: TASTNode);
var
  FilenameVal: TSSAValue;
  FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // SAVE "filename"
  // The filename is the first child (should be a string expression)
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    // Ensure we have a string register
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    // No filename provided - emit empty string
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit SAVE instruction with filename in Src1
  EmitInstruction(ssaSave, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessVerify(Node: TASTNode);
var
  FilenameVal, FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // VERIFY "filename"
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaVerify, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessBload(Node: TASTNode);
var
  FilenameVal, FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // BLOAD "filename"
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaBload, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessBsave(Node: TASTNode);
var
  FilenameVal, FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // BSAVE "filename"
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaBsave, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessBoot(Node: TASTNode);
var
  FilenameVal, FilenameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // BOOT "filename"
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), FilenameVal);
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaBoot, MakeSSAValue(svkNone), FilenameReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessDopen(Node: TASTNode);
var
  HandleVal, FilenameVal, ModeVal: TSSAValue;
  HandleReg, FilenameReg, ModeReg: TSSAValue;
  HandleNameIdx: Integer;
  HandleChild: TASTNode;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { DOPEN #handle, "filename" [, mode$]
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1: Filename (string expression)
      Child 2: Mode (optional string expression)

    SSA encoding (handle in Src1, not Dest, to avoid SSA versioning issues):
      Dest = none (no output)
      Src1 = handle register (int)
      Src2 = filename register (string)
      Src3 = mode register (string, or svkNone if not specified) }

  HandleNameIdx := -1;

  // Parse handle (first child)
  if Node.ChildCount > 0 then
  begin
    HandleChild := Node.GetChild(0);
    if HandleChild.NodeType = antLiteral then
    begin
      // Numeric handle: #1, #2, etc.
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end
    else if HandleChild.NodeType = antIdentifier then
    begin
      // Check if this is a "#N" style handle (lexer merged # and number)
      // e.g., "#1", "#2" tokenized as single identifier
      HandleStr := VarToStr(HandleChild.Value);
      if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
         (HandleStr[2] in ['0'..'9']) then
      begin
        // Extract numeric handle from "#N" format
        HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // Named handle: #MYFILE - store name in string constants
        HandleNameIdx := FProgram.AllocRegister(srtInt);  // Use as temporary identifier
        // Create a temporary register to hold the handle number (will be assigned at runtime)
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // Store handle name as string constant for runtime lookup
        HandleNameIdx := FProgram.AllocRegister(srtString);
      end;
    end
    else
    begin
      // Fallback: treat as expression
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end;
  end
  else
  begin
    // No handle - error, but provide default
    HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Parse filename (second child)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), FilenameVal);
    FilenameReg := EnsureStringRegister(FilenameVal);
  end
  else
  begin
    // No filename - emit empty string
    FilenameReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, FilenameReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Parse optional mode (third child)
  if Node.ChildCount > 2 then
  begin
    ProcessExpression(Node.GetChild(2), ModeVal);
    ModeReg := EnsureStringRegister(ModeVal);
  end
  else
  begin
    // Default mode: "R" (read)
    ModeReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, ModeReg, MakeSSAConstString('R'),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit DOPEN instruction
  // Dest = none, Src1 = handle, Src2 = filename, Src3 = mode
  EmitInstruction(ssaDopen, MakeSSAValue(svkNone), HandleReg, FilenameReg, ModeReg);
end;

procedure TSSAGenerator.ProcessDclose(Node: TASTNode);
var
  HandleVal: TSSAValue;
  HandleReg: TSSAValue;
  HandleChild: TASTNode;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { DCLOSE #handle
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)

    SSA encoding (handle in Src1, not Dest, to avoid SSA versioning issues):
      Dest = none
      Src1 = handle register (int) }

  // Parse handle (first child)
  if Node.ChildCount > 0 then
  begin
    HandleChild := Node.GetChild(0);
    if HandleChild.NodeType = antLiteral then
    begin
      // Numeric handle: #1, #2, etc.
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end
    else if HandleChild.NodeType = antIdentifier then
    begin
      // Check if this is a "#N" style handle (lexer merged # and number)
      HandleStr := VarToStr(HandleChild.Value);
      if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
         (HandleStr[2] in ['0'..'9']) then
      begin
        // Extract numeric handle from "#N" format
        HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // Named handle: #MYFILE - for now, treat as 0 (runtime will resolve)
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end
    else
    begin
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end;
  end
  else
  begin
    // No handle - close all? Use handle 0
    HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit DCLOSE instruction
  // Dest = none, Src1 = handle
  EmitInstruction(ssaDclose, MakeSSAValue(svkNone), HandleReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessAppend(Node: TASTNode);
var
  HandleVal, DataVal: TSSAValue;
  HandleReg, DataReg: TSSAValue;
  HandleChild: TASTNode;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { APPEND #handle, data
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1: Data expression to append

    SSA encoding (handle in Src1, not Dest, to avoid SSA versioning issues):
      Dest = none
      Src1 = handle register (int)
      Src2 = data register (string) }

  // Parse handle (first child)
  if Node.ChildCount > 0 then
  begin
    HandleChild := Node.GetChild(0);
    if HandleChild.NodeType = antLiteral then
    begin
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end
    else if HandleChild.NodeType = antIdentifier then
    begin
      // Check if this is a "#N" style handle (lexer merged # and number)
      HandleStr := VarToStr(HandleChild.Value);
      if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
         (HandleStr[2] in ['0'..'9']) then
      begin
        HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end
    else
    begin
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end;
  end
  else
  begin
    HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Parse data expression (second child)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), DataVal);
    DataReg := EnsureStringRegister(DataVal);
  end
  else
  begin
    DataReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, DataReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit APPEND instruction
  // Dest = none, Src1 = handle, Src2 = data
  EmitInstruction(ssaAppend, MakeSSAValue(svkNone), HandleReg, DataReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessDclear(Node: TASTNode);
begin
  if FCurrentBlock = nil then Exit;

  { DCLEAR - close all open file handles
    No parameters needed

    SSA encoding:
      No operands, just the instruction }

  EmitInstruction(ssaDclear, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessRecord(Node: TASTNode);
var
  HandleVal, PosVal: TSSAValue;
  HandleReg, PosReg: TSSAValue;
  HandleChild: TASTNode;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { RECORD #handle, position
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1: Position expression (byte offset)

    SSA encoding (handle in Src1, not Dest, to avoid SSA versioning issues):
      Dest = none
      Src1 = handle register (int)
      Src2 = position register (int) }

  // Parse handle (first child)
  if Node.ChildCount > 0 then
  begin
    HandleChild := Node.GetChild(0);
    if HandleChild.NodeType = antLiteral then
    begin
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end
    else if HandleChild.NodeType = antIdentifier then
    begin
      // Check if this is a "#N" style handle (lexer merged # and number)
      HandleStr := VarToStr(HandleChild.Value);
      if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
         (HandleStr[2] in ['0'..'9']) then
      begin
        HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end
    else
    begin
      ProcessExpression(HandleChild, HandleVal);
      HandleReg := EnsureIntRegister(HandleVal);
    end;
  end
  else
  begin
    HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(1),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Parse position expression (second child)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), PosVal);
    PosReg := EnsureIntRegister(PosVal);
  end
  else
  begin
    // Default position: 0 (beginning of file)
    PosReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, PosReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  // Emit RECORD instruction
  // Dest = none, Src1 = handle, Src2 = position
  EmitInstruction(ssaRecord, MakeSSAValue(svkNone), HandleReg, PosReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessGetFile(Node: TASTNode);
var
  HandleVal, HandleReg, VarReg: TSSAValue;
  HandleChild, VarChild: TASTNode;
  VarName: string;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { GET# handle, variable
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1: Variable to store character

    SSA encoding:
      Dest = variable register (string)
      Src1 = handle register (int) }

  // Need at least 2 children (handle and variable)
  if Node.ChildCount < 2 then Exit;

  // Parse handle (first child)
  HandleChild := Node.GetChild(0);
  if HandleChild.NodeType = antLiteral then
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end
  else if HandleChild.NodeType = antIdentifier then
  begin
    // Check if this is a "#N" style handle (lexer merged # and number)
    HandleStr := VarToStr(HandleChild.Value);
    if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
       (HandleStr[2] in ['0'..'9']) then
    begin
      HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Named handle - for now, treat as 0
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end
  else
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end;

  // Parse variable (second child)
  VarChild := Node.GetChild(1);
  VarName := string(VarChild.Value);
  VarReg := GetOrAllocateVariable(VarName);

  // Emit GET# instruction: Dest=variable, Src1=handle
  EmitInstruction(ssaGetFile, VarReg, HandleReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessInputFile(Node: TASTNode);
var
  HandleVal, HandleReg, VarReg: TSSAValue;
  HandleChild, VarChild: TASTNode;
  VarName: string;
  i: Integer;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { INPUT# handle, var1 [, var2 ...]
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1+: Variables to store input

    SSA encoding:
      For each variable:
        Dest = variable register
        Src1 = handle register (int) }

  // Need at least 2 children (handle and at least one variable)
  if Node.ChildCount < 2 then Exit;

  // Parse handle (first child)
  HandleChild := Node.GetChild(0);
  if HandleChild.NodeType = antLiteral then
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end
  else if HandleChild.NodeType = antIdentifier then
  begin
    // Check if this is a "#N" style handle (lexer merged # and number)
    HandleStr := VarToStr(HandleChild.Value);
    if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
       (HandleStr[2] in ['0'..'9']) then
    begin
      HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end
  else
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end;

  // Process each variable (children 1+)
  for i := 1 to Node.ChildCount - 1 do
  begin
    VarChild := Node.GetChild(i);
    if VarChild.NodeType = antIdentifier then
    begin
      VarName := string(VarChild.Value);
      VarReg := GetOrAllocateVariable(VarName);
      // Emit INPUT# instruction for each variable
      EmitInstruction(ssaInputFile, VarReg, HandleReg,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    // Skip separators and other nodes
  end;
end;

procedure TSSAGenerator.ProcessPrintFile(Node: TASTNode);
var
  HandleVal, HandleReg, ExprVal, ExprReg: TSSAValue;
  HandleChild, Child: TASTNode;
  i: Integer;
  SeparatorChar: string;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { PRINT# handle [, expr1 [; expr2 ...]]
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1+: Expressions and separators (like PRINT)

    SSA encoding:
      Uses ssaPrintFile for expressions with handle in Src2 }

  // Need at least 1 child (handle)
  if Node.ChildCount < 1 then Exit;

  // Parse handle (first child)
  HandleChild := Node.GetChild(0);
  if HandleChild.NodeType = antLiteral then
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end
  else if HandleChild.NodeType = antIdentifier then
  begin
    // Check if this is a "#N" style handle (lexer merged # and number)
    HandleStr := VarToStr(HandleChild.Value);
    if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
       (HandleStr[2] in ['0'..'9']) then
    begin
      HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end
  else
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end;

  // If only handle (no expressions), this is PRINT# with no data
  // Should write just a newline to the file (like empty PRINT on screen)
  if Node.ChildCount = 1 then
  begin
    EmitInstruction(ssaPrintFileNewLine, MakeSSAValue(svkNone), HandleReg,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  // Process each expression (children 1+)
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    // Handle separator nodes
    if Child.NodeType = antSeparator then
    begin
      SeparatorChar := VarToStr(Child.Value);
      // For file output, separators are handled similarly to screen
      // Comma = tab, Semicolon = no separator
      if SeparatorChar = ',' then
        EmitInstruction(ssaPrintComma, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else if SeparatorChar = ';' then
        EmitInstruction(ssaPrintSemicolon, MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Continue;
    end;

    // Process expression
    ProcessExpression(Child, ExprVal);

    // Emit PRINT# instruction with expression value and handle
    // Dest = value to print, Src1 = file handle
    if ExprVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      ExprReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, ExprReg, ExprVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaPrintFile, ExprReg, HandleReg,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ExprVal.Kind = svkConstString then
    begin
      ExprReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
      EmitInstruction(ssaLoadConstString, ExprReg, ExprVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaPrintFile, ExprReg, HandleReg,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      // Variable or expression - emit directly (type is preserved in register)
      EmitInstruction(ssaPrintFile, ExprVal, HandleReg,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Check if we need a newline at the end
  // If the last child is not a separator, add newline TO THE FILE (not screen)
  if (Node.ChildCount > 1) and (Node.GetChild(Node.ChildCount - 1).NodeType <> antSeparator) then
  begin
    // Use ssaPrintFileNewLine with handle in Src1 to write CR to the file
    EmitInstruction(ssaPrintFileNewLine, MakeSSAValue(svkNone),
                   HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessCmd(Node: TASTNode);
var
  HandleVal, HandleReg, ExprVal, ExprReg: TSSAValue;
  HandleChild, Child: TASTNode;
  i: Integer;
  HandleStr: string;
  HandleNum: Integer;
begin
  if FCurrentBlock = nil then Exit;

  { CMD handle [, expression]
    AST structure:
      Child 0: Handle (antLiteral for numeric, antIdentifier for named)
      Child 1+: Optional expression(s) to print after redirection starts

    SSA encoding:
      Dest = none
      Src1 = handle register (int)
      Src2 = optional expression to print }

  // Need at least 1 child (handle)
  if Node.ChildCount < 1 then Exit;

  // Parse handle (first child)
  HandleChild := Node.GetChild(0);
  if HandleChild.NodeType = antLiteral then
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end
  else if HandleChild.NodeType = antIdentifier then
  begin
    // Check if this is a "#N" style handle (lexer merged # and number)
    HandleStr := VarToStr(HandleChild.Value);
    if (Length(HandleStr) > 1) and (HandleStr[1] = '#') and
       (HandleStr[2] in ['0'..'9']) then
    begin
      HandleNum := StrToIntDef(Copy(HandleStr, 2, Length(HandleStr) - 1), 1);
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(HandleNum),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end
  else
  begin
    ProcessExpression(HandleChild, HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
  end;

  // Emit CMD instruction to redirect output
  EmitInstruction(ssaCmd, MakeSSAValue(svkNone), HandleReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // Process optional expressions (children 1+)
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);

    // Skip separators
    if Child.NodeType = antSeparator then
      Continue;

    // Process expression and print it
    ProcessExpression(Child, ExprVal);

    // Emit print instruction for the expression
    // CMD uses regular PRINT since output is already redirected
    if ExprVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      ExprReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, ExprReg, ExprVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaPrint, ExprReg, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ExprVal.Kind = svkConstString then
    begin
      ExprReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
      EmitInstruction(ssaLoadConstString, ExprReg, ExprVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitInstruction(ssaPrintString, ExprReg, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else if ExprVal.RegType = srtString then
    begin
      EmitInstruction(ssaPrintString, ExprVal, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
    begin
      EmitInstruction(ssaPrint, ExprVal, MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;
end;

// ============================================================================
// SPRITE COMMANDS SSA GENERATION
// ============================================================================

procedure TSSAGenerator.ProcessSprite(Node: TASTNode);
var
  ParamVal, ParamReg: TSSAValue;
  ParamRegs: array[0..6] of TSSAValue;
  i, ParamCount: Integer;
  Instr: TSSAInstruction;
begin
  if FCurrentBlock = nil then Exit;

  { SPRITE n [,enabled] [,color] [,priority] [,scalex] [,scaley] [,mode]
    AST structure:
      Child 0: Sprite number (1-256)
      Child 1: Enabled (0/1) - optional
      Child 2: Color - optional
      Child 3: Priority (0=front, 1=back) - optional
      Child 4: ScaleX - optional
      Child 5: ScaleY - optional
      Child 6: Mode (0=normal, 1=multicolor) - optional

    SSA encoding:
      ssaSprite: Src1=spriteNum, Src2=enabled, Src3=color
      Additional params via PhiSources: priority, scalex, scaley, mode }

  ParamCount := Node.ChildCount;
  if ParamCount < 1 then Exit;

  // Initialize all params to none
  for i := 0 to 6 do
    ParamRegs[i] := MakeSSAValue(svkNone);

  // Process each parameter
  for i := 0 to ParamCount - 1 do
  begin
    if i > 6 then Break;
    ProcessExpression(Node.GetChild(i), ParamVal);
    if ParamVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      ParamReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, ParamReg, ParamVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ParamRegs[i] := ParamReg;
    end
    else
      ParamRegs[i] := ParamVal;
  end;

  // Emit ssaSprite: Src1=n, Src2=enabled, Src3=color
  // Additional params via PhiSources
  Instr := TSSAInstruction.Create(ssaSprite);
  Instr.Dest := MakeSSAValue(svkNone);
  Instr.Src1 := ParamRegs[0];  // sprite number
  Instr.Src2 := ParamRegs[1];  // enabled
  Instr.Src3 := ParamRegs[2];  // color
  Instr.SourceLine := Node.SourceLine;
  // Pack additional params as PhiSources
  Instr.AddPhiSource(ParamRegs[3], nil);  // priority
  Instr.AddPhiSource(ParamRegs[4], nil);  // scalex
  Instr.AddPhiSource(ParamRegs[5], nil);  // scaley
  Instr.AddPhiSource(ParamRegs[6], nil);  // mode
  FCurrentBlock.AddInstruction(Instr);
end;

procedure TSSAGenerator.ProcessMovspr(Node: TASTNode);
var
  ParamVal, ParamReg: TSSAValue;
  ParamRegs: array[0..2] of TSSAValue;
  MovsprMode: Integer;
  ModeStr: string;
  i, ParamCount: Integer;
  OpCode: TSSAOpCode;
begin
  if FCurrentBlock = nil then Exit;

  { MOVSPR n, x, y        (absolute mode 0)
    MOVSPR n, +x, +y      (relative mode 1)
    MOVSPR n, dist;angle  (polar mode 2)
    MOVSPR n, angle#speed (auto mode 3)

    AST structure:
      Child 0: Sprite number
      Child 1: X / distance / angle
      Child 2: Y / angle / speed
      Attribute 'movspr_mode': '0', '1', '2', or '3'

    SSA encoding:
      ssaMovsprAbs: Src1=n, Src2=x, Src3=y
      ssaMovsprRel: Src1=n, Src2=dx, Src3=dy
      ssaMovsprPolar: Src1=n, Src2=dist, Src3=angle
      ssaMovsprAuto: Src1=n, Src2=angle, Src3=speed }

  ParamCount := Node.ChildCount;
  if ParamCount < 3 then Exit;

  // Get MOVSPR mode from attribute
  ModeStr := Node.Attributes.Values['movspr_mode'];
  if ModeStr = '' then ModeStr := '0';
  MovsprMode := StrToIntDef(ModeStr, 0);

  // Initialize params
  for i := 0 to 2 do
    ParamRegs[i] := MakeSSAValue(svkNone);

  // Process each parameter
  for i := 0 to 2 do
  begin
    if i >= ParamCount then Break;
    ProcessExpression(Node.GetChild(i), ParamVal);
    if ParamVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      ParamReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, ParamReg, ParamVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      ParamRegs[i] := ParamReg;
    end
    else
      ParamRegs[i] := ParamVal;
  end;

  // Select opcode based on mode
  case MovsprMode of
    0: OpCode := ssaMovsprAbs;
    1: OpCode := ssaMovsprRel;
    2: OpCode := ssaMovsprPolar;
    3: OpCode := ssaMovsprAuto;
  else
    OpCode := ssaMovsprAbs;
  end;

  EmitInstruction(OpCode, MakeSSAValue(svkNone),
                 ParamRegs[0], ParamRegs[1], ParamRegs[2]);
end;

procedure TSSAGenerator.ProcessSprcolor(Node: TASTNode);
var
  MC1Val, MC2Val, MC1Reg, MC2Reg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  { SPRCOLOR [mc1] [,mc2]
    AST structure:
      Child 0: Multicolor 1 - optional
      Child 1: Multicolor 2 - optional

    SSA encoding:
      ssaSprcolor: Src1=mc1, Src2=mc2 }

  MC1Reg := MakeSSAValue(svkNone);
  MC2Reg := MakeSSAValue(svkNone);

  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), MC1Val);
    if MC1Val.Kind in [svkConstFloat, svkConstInt] then
    begin
      MC1Reg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, MC1Reg, MC1Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      MC1Reg := MC1Val;
  end;

  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), MC2Val);
    if MC2Val.Kind in [svkConstFloat, svkConstInt] then
    begin
      MC2Reg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, MC2Reg, MC2Val,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      MC2Reg := MC2Val;
  end;

  EmitInstruction(ssaSprcolor, MakeSSAValue(svkNone),
                 MC1Reg, MC2Reg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprsav(Node: TASTNode);
var
  SrcVal, DstVal, SrcReg, DstReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  { SPRSAV source, dest
    AST structure:
      Child 0: Source (sprite number or string variable)
      Child 1: Destination (string variable or sprite number)

    SSA encoding:
      ssaSprsav: Src1=source, Src2=dest }

  if Node.ChildCount < 2 then Exit;

  ProcessExpression(Node.GetChild(0), SrcVal);
  ProcessExpression(Node.GetChild(1), DstVal);

  // Ensure registers
  if SrcVal.Kind in [svkConstFloat, svkConstInt] then
  begin
    SrcReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
    EmitInstruction(ssaLoadConstFloat, SrcReg, SrcVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    SrcReg := SrcVal;

  if DstVal.Kind in [svkConstFloat, svkConstInt] then
  begin
    DstReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
    EmitInstruction(ssaLoadConstFloat, DstReg, DstVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    DstReg := DstVal;

  EmitInstruction(ssaSprsav, MakeSSAValue(svkNone),
                 SrcReg, DstReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessCollision(Node: TASTNode);
var
  TypeVal, LineVal, TypeReg, LineReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  { COLLISION type [,line]
    AST structure:
      Child 0: Collision type (1=sprite-sprite, 2=sprite-display)
      Child 1: Line number for GOSUB - optional (0 to disable)

    SSA encoding:
      ssaCollision: Src1=type, Src2=line }

  if Node.ChildCount < 1 then Exit;

  ProcessExpression(Node.GetChild(0), TypeVal);
  if TypeVal.Kind in [svkConstFloat, svkConstInt] then
  begin
    TypeReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, TypeReg, TypeVal,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    TypeReg := TypeVal;

  LineReg := MakeSSAValue(svkNone);
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), LineVal);
    if LineVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      LineReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, LineReg, LineVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      LineReg := LineVal;
  end;

  EmitInstruction(ssaCollision, MakeSSAValue(svkNone),
                 TypeReg, LineReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprdef(Node: TASTNode);
var
  NumVal, NumReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  { SPRDEF [n]
    AST structure:
      Child 0: Sprite number (1-8) - optional, defaults to 1
    SSA encoding:
      ssaSpriteDef: Src1 = sprite number (float register, like other sprite ops) }

  if Node.ChildCount >= 1 then
  begin
    ProcessExpression(Node.GetChild(0), NumVal);
    if NumVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      NumReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, NumReg, NumVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      NumReg := NumVal;
  end
  else
  begin
    // No argument: default to sprite 1
    NumReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
    EmitInstruction(ssaLoadConstFloat, NumReg, MakeSSAConstFloat(1.0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaSpriteDef, MakeSSAValue(svkNone),
                 NumReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprsave(Node: TASTNode);
var
  FileNameVal, FileNameReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 1 then Exit;  { SPRSAVE "filename" }

  ProcessExpression(Node.GetChild(0), FileNameVal);
  FileNameReg := EnsureStringRegister(FileNameVal);

  EmitInstruction(ssaSprsave, MakeSSAValue(svkNone),
                 FileNameReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprload(Node: TASTNode);
var
  FileNameVal, FileNameReg, FlagVal, FlagReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 1 then Exit;  { SPRLOAD "filename" [, usefilecolors] }

  ProcessExpression(Node.GetChild(0), FileNameVal);
  FileNameReg := EnsureStringRegister(FileNameVal);

  // Optional 2nd arg = "use file colours" flag (int register, default 0).
  if Node.ChildCount >= 2 then
  begin
    ProcessExpression(Node.GetChild(1), FlagVal);
    if FlagVal.Kind in [svkConstFloat, svkConstInt] then
    begin
      FlagReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, FlagReg, FlagVal,
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      FlagReg := FlagVal;
  end
  else
  begin
    FlagReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, FlagReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaSprload, MakeSSAValue(svkNone),
                 FileNameReg, FlagReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprsize(Node: TASTNode);
var
  V, R: array[0..2] of TSSAValue;
  I: Integer;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 3 then Exit;  { SPRSIZE n, width, height }
  for I := 0 to 2 do
  begin
    ProcessExpression(Node.GetChild(I), V[I]);
    if V[I].Kind in [svkConstFloat, svkConstInt] then
    begin
      R[I] := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaLoadConstFloat, R[I], V[I], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end
    else
      R[I] := V[I];
  end;
  // Dest=height, Src1=sprite number, Src2=width (the three regs the generic emitter maps).
  EmitInstruction(ssaSprsize, R[2], R[0], R[1], MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessSprform(Node: TASTNode);
var
  NumVal, NumReg, FmtVal, FmtReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount < 2 then Exit;  { SPRFORM n, format }
  ProcessExpression(Node.GetChild(0), NumVal);
  if NumVal.Kind in [svkConstFloat, svkConstInt] then
  begin
    NumReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
    EmitInstruction(ssaLoadConstFloat, NumReg, NumVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    NumReg := NumVal;
  ProcessExpression(Node.GetChild(1), FmtVal);
  if FmtVal.Kind in [svkConstFloat, svkConstInt] then
  begin
    FmtReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
    EmitInstruction(ssaLoadConstFloat, FmtReg, FmtVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    FmtReg := FmtVal;
  EmitInstruction(ssaSprform, MakeSSAValue(svkNone), NumReg, FmtReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessRun(Node: TASTNode);
var
  LineNumVal: TSSAValue;
  ImmediateVal: Integer;
begin
  if FCurrentBlock = nil then Exit;

  // RUN [linenum]
  ImmediateVal := 0;  // Default: run from beginning
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), LineNumVal);
    // If it's a constant integer, extract the value
    if LineNumVal.Kind = svkConstInt then
      ImmediateVal := LineNumVal.ConstInt;
  end;

  // RUN uses Immediate for line number
  EmitInstruction(ssaRun, MakeSSAValue(svkNone), MakeSSAConstInt(ImmediateVal),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessList(Node: TASTNode);
var
  RangeVal, RangeReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // LIST [start[-end]]
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), RangeVal);
    RangeReg := EnsureStringRegister(RangeVal);
  end
  else
  begin
    RangeReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, RangeReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaList, MakeSSAValue(svkNone), RangeReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessNew(Node: TASTNode);
begin
  if FCurrentBlock = nil then Exit;

  // NEW - no parameters
  EmitInstruction(ssaNew, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessDelete(Node: TASTNode);
var
  StartVal, EndVal, StartReg, EndReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // DELETE [start[-end]]
  // Parser provides: 0 children = delete nothing
  //                  1 child = single line delete
  //                  2 children = range delete (start, end)
  // Special values: start=0 means "from beginning", end=-1 means "to end"

  if Node.ChildCount = 0 then
  begin
    // No parameters - error or do nothing
    StartReg := MakeSSAConstInt(0);
    EndReg := MakeSSAConstInt(0);
  end
  else if Node.ChildCount = 1 then
  begin
    // Single line: DELETE 100
    ProcessExpression(Node.GetChild(0), StartVal);
    StartReg := EnsureIntRegister(StartVal);
    EndReg := StartReg; // Same line for start and end
  end
  else
  begin
    // Range: DELETE 10-50 or DELETE -100 or DELETE 100-
    ProcessExpression(Node.GetChild(0), StartVal);
    StartReg := EnsureIntRegister(StartVal);
    ProcessExpression(Node.GetChild(1), EndVal);
    EndReg := EnsureIntRegister(EndVal);
  end;

  EmitInstruction(ssaDelete, MakeSSAValue(svkNone), StartReg, EndReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessRenumber(Node: TASTNode);
var
  NewStartVal, IncrementVal, OldStartVal: TSSAValue;
  NewStartReg, IncrementReg, OldStartReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // RENUMBER [new_start [,increment [,old_start]]]
  // Defaults: new_start=10, increment=10, old_start=first line

  // Parse new starting line number (default 10)
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), NewStartVal);
    NewStartReg := EnsureIntRegister(NewStartVal);
  end
  else
  begin
    NewStartReg := MakeSSAConstInt(10);
  end;

  // Parse increment (default 10)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), IncrementVal);
    IncrementReg := EnsureIntRegister(IncrementVal);
  end
  else
  begin
    IncrementReg := MakeSSAConstInt(10);
  end;

  // Parse old starting line number (default 0 = first line)
  if Node.ChildCount > 2 then
  begin
    ProcessExpression(Node.GetChild(2), OldStartVal);
    OldStartReg := EnsureIntRegister(OldStartVal);
  end
  else
  begin
    OldStartReg := MakeSSAConstInt(0);
  end;

  EmitInstruction(ssaRenumber, MakeSSAValue(svkNone), NewStartReg, IncrementReg,
                 OldStartReg);
end;

procedure TSSAGenerator.ProcessCatalog(Node: TASTNode);
var
  PathVal, PathReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // CATALOG/DIR [path]
  if Node.ChildCount > 0 then
  begin
    ProcessExpression(Node.GetChild(0), PathVal);
    PathReg := EnsureStringRegister(PathVal);
  end
  else
  begin
    PathReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, PathReg, MakeSSAConstString(''),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaCatalog, MakeSSAValue(svkNone), PathReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessCopyFile(Node: TASTNode);
var
  SrcVal, DstVal, OverwriteVal: TSSAValue;
  SrcReg, DstReg, OverwriteReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // COPY "src", "dest" [, overwrite]
  // Minimum 2 parameters: source and destination
  if Node.ChildCount < 2 then
  begin
    raise Exception.Create('COPY requires source and destination parameters');
  end;

  // Process source path
  ProcessExpression(Node.GetChild(0), SrcVal);
  SrcReg := EnsureStringRegister(SrcVal);

  // Process destination path
  ProcessExpression(Node.GetChild(1), DstVal);
  DstReg := EnsureStringRegister(DstVal);

  // Process optional overwrite flag (default 0)
  if Node.ChildCount > 2 then
  begin
    ProcessExpression(Node.GetChild(2), OverwriteVal);
    OverwriteReg := EnsureIntRegister(OverwriteVal);
  end
  else
  begin
    OverwriteReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, OverwriteReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaCopyFile, MakeSSAValue(svkNone), SrcReg, DstReg, OverwriteReg);
end;

procedure TSSAGenerator.ProcessScratch(Node: TASTNode);
var
  PatternVal, FlagsVal: TSSAValue;
  PatternReg, FlagsReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // SCRATCH "pattern" [, flags]
  // flags: 1 = silent (no ?FILE NOT FOUND), 2 = force (no confirmation), 3 = both
  if Node.ChildCount < 1 then
  begin
    raise Exception.Create('SCRATCH requires a file pattern');
  end;

  // Process pattern
  ProcessExpression(Node.GetChild(0), PatternVal);
  PatternReg := EnsureStringRegister(PatternVal);

  // Process optional flags (default 0)
  if Node.ChildCount > 1 then
  begin
    ProcessExpression(Node.GetChild(1), FlagsVal);
    FlagsReg := EnsureIntRegister(FlagsVal);
  end
  else
  begin
    FlagsReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, FlagsReg, MakeSSAConstInt(0),
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  EmitInstruction(ssaScratch, MakeSSAValue(svkNone), PatternReg, FlagsReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessRenameFile(Node: TASTNode);
var
  OldVal, NewVal: TSSAValue;
  OldReg, NewReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // RENAME "old", "new"
  if Node.ChildCount < 2 then
  begin
    raise Exception.Create('RENAME requires old and new filename');
  end;

  // Process old name
  ProcessExpression(Node.GetChild(0), OldVal);
  OldReg := EnsureStringRegister(OldVal);

  // Process new name
  ProcessExpression(Node.GetChild(1), NewVal);
  NewReg := EnsureStringRegister(NewVal);

  EmitInstruction(ssaRenameFile, MakeSSAValue(svkNone), OldReg, NewReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessConcat(Node: TASTNode);
var
  SrcVal, DstVal: TSSAValue;
  SrcReg, DstReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // CONCAT "src", "dest"
  if Node.ChildCount < 2 then
  begin
    raise Exception.Create('CONCAT requires source and destination');
  end;

  // Process source path
  ProcessExpression(Node.GetChild(0), SrcVal);
  SrcReg := EnsureStringRegister(SrcVal);

  // Process destination path
  ProcessExpression(Node.GetChild(1), DstVal);
  DstReg := EnsureStringRegister(DstVal);

  EmitInstruction(ssaConcat, MakeSSAValue(svkNone), SrcReg, DstReg,
                 MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessMkdir(Node: TASTNode);
var
  PathVal: TSSAValue;
  PathReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // MKDIR "path"
  if Node.ChildCount < 1 then
  begin
    raise Exception.Create('MKDIR requires a path');
  end;

  ProcessExpression(Node.GetChild(0), PathVal);
  PathReg := EnsureStringRegister(PathVal);

  EmitInstruction(ssaMkdir, MakeSSAValue(svkNone), PathReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessChdir(Node: TASTNode);
var
  PathVal: TSSAValue;
  PathReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // CHDIR "path"
  if Node.ChildCount < 1 then
  begin
    raise Exception.Create('CHDIR requires a path');
  end;

  ProcessExpression(Node.GetChild(0), PathVal);
  PathReg := EnsureStringRegister(PathVal);

  EmitInstruction(ssaChdir, MakeSSAValue(svkNone), PathReg,
                 MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessMoveFile(Node: TASTNode);
var
  SrcVal, DstVal: TSSAValue;
  SrcReg, DstReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // MOVE "src", "dest"
  if Node.ChildCount < 2 then
  begin
    raise Exception.Create('MOVE requires source and destination');
  end;

  // Process source path
  ProcessExpression(Node.GetChild(0), SrcVal);
  SrcReg := EnsureStringRegister(SrcVal);

  // Process destination path
  ProcessExpression(Node.GetChild(1), DstVal);
  DstReg := EnsureStringRegister(DstVal);

  EmitInstruction(ssaMoveFile, MakeSSAValue(svkNone), SrcReg, DstReg,
                 MakeSSAValue(svkNone));
end;

{$IFDEF WEB_MODE}
procedure TSSAGenerator.ProcessWebCommand(Node: TASTNode);
var
  CmdName: string;
  NameVal, ValueVal, StatusVal: TSSAValue;
  NameReg, ValueReg, StatusReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  CmdName := UpperCase(VarToStr(Node.Value));

  // SETHEADER name, value
  if CmdName = 'SETHEADER' then
  begin
    if Node.ChildCount < 2 then
    begin
      raise Exception.Create('SETHEADER requires name and value');
    end;

    // Process header name
    ProcessExpression(Node.GetChild(0), NameVal);
    NameReg := EnsureStringRegister(NameVal);

    // Process header value
    ProcessExpression(Node.GetChild(1), ValueVal);
    ValueReg := EnsureStringRegister(ValueVal);

    EmitInstruction(ssaWebSetHeader, MakeSSAValue(svkNone), NameReg, ValueReg,
                   MakeSSAValue(svkNone));
  end
  // STATUS code
  else if CmdName = 'STATUS' then
  begin
    if Node.ChildCount < 1 then
    begin
      raise Exception.Create('STATUS requires code');
    end;

    // Process status code
    ProcessExpression(Node.GetChild(0), StatusVal);
    StatusReg := EnsureIntRegister(StatusVal);

    EmitInstruction(ssaWebStatus, MakeSSAValue(svkNone), StatusReg,
                   MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
  begin
    raise Exception.CreateFmt('Unknown web command: %s', [CmdName]);
  end;
end;
{$ENDIF}

{ PHASE 3 TIER 3: Fix forward GOTO/GOSUB references and RETURN edges }
procedure TSSAGenerator.FixForwardReferences;
var
  i, j, k, SuccIdx: Integer;
  Block, ReturnBlock, GosubBlock, ReturnTarget: TSSABasicBlock;
  Instr: TSSAInstruction;
  TargetBlock: TSSABasicBlock;
  TargetLabel: string;
begin
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Fixing forward GOTO/GOSUB references...');
  {$ENDIF}

  // Pass 1: Fix forward GOTO/GOSUB edges
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    for j := 0 to Block.Instructions.Count - 1 do
    begin
      Instr := Block.Instructions[j];

      // Check for GOTO, GOSUB, SUB/FUNCTION call, or conditional jumps with a label operand.
      // ssaCallSub mirrors ssaCall (GOSUB): it terminates its block with two successors
      // (procedure entry + return point), so the procedure block gets a predecessor and is
      // reachable from entry (clean single-entry CFG for the dominator tree).
      if (Instr.OpCode in [ssaJump, ssaCall, ssaCallSub, ssaJumpIfZero, ssaJumpIfNotZero]) and (Instr.Dest.Kind = svkLabel) then
      begin
        TargetLabel := Instr.Dest.LabelName;
        TargetBlock := FProgram.FindBlock(TargetLabel);

        if Assigned(TargetBlock) then
        begin
          // Check if edge already exists
          if Block.Successors.IndexOf(TargetBlock) = -1 then
          begin
            // Add missing edge
            Block.AddSuccessor(TargetBlock);
            TargetBlock.AddPredecessor(Block);
            {$IFDEF DEBUG_SSA}
            if DebugSSA then
            begin
              if Instr.OpCode = ssaJump then
                WriteLn('[SSA] Edge: ', Block.LabelName, ' → ', TargetLabel, ' (GOTO forward ref)')
              else
                WriteLn('[SSA] Edge: ', Block.LabelName, ' → ', TargetLabel, ' (GOSUB forward ref)');
            end;
            {$ENDIF}
          end;
        end
        {$IFDEF DEBUG_SSA}
        else if DebugSSA then
          WriteLn('[SSA] WARNING: Target block not found: ', TargetLabel);
        {$ENDIF}
        ;
      end;
    end;
  end;

  // Pass 2: Fix RETURN edges
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Fixing RETURN edges...');
  {$ENDIF}
  for i := 0 to FProgram.Blocks.Count - 1 do
  begin
    Block := FProgram.Blocks[i];

    // Check if this block contains RETURN
    for j := 0 to Block.Instructions.Count - 1 do
    begin
      if Block.Instructions[j].OpCode = ssaReturn then
      begin
        ReturnBlock := Block;

        // Find all GOSUB blocks and connect RETURN to their successors
        for k := 0 to FProgram.Blocks.Count - 1 do
        begin
          GosubBlock := FProgram.Blocks[k];

          // Check if this block contains GOSUB (ssaCall)
          if (GosubBlock.Instructions.Count > 0) and
             (GosubBlock.Instructions[GosubBlock.Instructions.Count-1].OpCode = ssaCall) then
          begin
            // The return target is the fall-through successor of the GOSUB block
            if GosubBlock.Successors.Count > 0 then
            begin
              // Find the fall-through successor (not the CALL target)
              for SuccIdx := 0 to GosubBlock.Successors.Count - 1 do
              begin
                ReturnTarget := TSSABasicBlock(GosubBlock.Successors[SuccIdx]);

                // Skip the CALL target - return point is the other successor
                if ReturnTarget.LabelName <> GosubBlock.Instructions[GosubBlock.Instructions.Count-1].Dest.LabelName then
                begin
                  // Add edge from RETURN block to return point
                  if ReturnBlock.Successors.IndexOf(ReturnTarget) = -1 then
                  begin
                    ReturnBlock.AddSuccessor(ReturnTarget);
                    ReturnTarget.AddPredecessor(ReturnBlock);
                    {$IFDEF DEBUG_SSA}
                    if DebugSSA then
                      WriteLn('[SSA] Edge: ', ReturnBlock.LabelName, ' → ', ReturnTarget.LabelName, ' (RETURN)');
                    {$ENDIF}
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TSSAGenerator.ProcedureLabelName(const Name: string): string;
begin
  Result := 'PROC_' + UpperCase(Name);
end;

function TSSAGenerator.TypeNameToBank(const TypeName, FieldName: string): TSSARegisterType;
// Map a declared field/var type name to a register bank. Empty type => infer by name suffix.
var
  T: string;
begin
  T := UpperCase(TypeName);
  if T = '' then
    Exit(GetVariableType(FieldName));
  if (T = 'INTEGER') or (T = 'LONG') or (T = 'SHORT') or (T = 'BYTE') or
     (T = 'UBYTE') or (T = 'USHORT') or (T = 'UINTEGER') or (T = 'ULONG') or
     (T = 'LONGINT') or (T = 'ULONGINT') or (T = 'BOOLEAN') then
    Result := srtInt
  else if (T = 'SINGLE') or (T = 'DOUBLE') then
    Result := srtFloat
  else if (T = 'STRING') or (T = 'ZSTRING') or (T = 'WSTRING') then
    Result := srtString
  else
    Result := GetVariableType(FieldName);  // unknown (e.g. nested UDT, deferred): fall back to suffix
end;

function TSSAGenerator.FindUDT(const TypeName: string): Integer;
var
  i: Integer;
  U: string;
begin
  U := UpperCase(TypeName);
  for i := 0 to High(FUDTs) do
    if FUDTs[i].Name = U then Exit(i);
  Result := -1;
end;

function TSSAGenerator.UDTFieldBankSlot(UDTIdx: Integer; const FieldName: string;
  out Bank: TSSARegisterType; out Slot: Integer; out NestedType: string): Boolean;
var
  i: Integer;
  F: string;
begin
  Result := False;
  NestedType := '';
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  F := UpperCase(FieldName);
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
    if FUDTs[UDTIdx].Fields[i].Name = F then
    begin
      Bank := FUDTs[UDTIdx].Fields[i].Bank;
      Slot := FUDTs[UDTIdx].Fields[i].Slot;
      NestedType := FUDTs[UDTIdx].Fields[i].NestedType;
      Exit(True);
    end;
end;

procedure TSSAGenerator.RegisterUDTs(Node: TASTNode);
// Two passes so a TYPE may reference another TYPE declared later (forward reference).
begin
  CollectUDTNames(Node);   // pass 1: all type names known
  FillUDTFields(Node);     // pass 2: resolve fields (incl. nested-UDT fields)
end;

procedure TSSAGenerator.CollectUDTNames(Node: TASTNode);
var
  i, n: Integer;
  Name: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antTypeDecl then
  begin
    Name := UpperCase(VarToStr(Node.Value));
    if FindUDT(Name) < 0 then
    begin
      n := Length(FUDTs);
      SetLength(FUDTs, n + 1);
      FUDTs[n].Name := Name;
      SetLength(FUDTs[n].Fields, 0);
      FUDTs[n].NInt := 0; FUDTs[n].NFloat := 0; FUDTs[n].NStr := 0;
      FUDTs[n].Parent := UpperCase(Node.Attributes.Values['EXTENDS']);  // '' if none (M4.2)
      FUDTs[n].Node := Node;
      FUDTs[n].Filled := False;
    end;
    Exit;
  end;
  for i := 0 to Node.ChildCount - 1 do
    CollectUDTNames(Node.GetChild(i));
end;

procedure TSSAGenerator.FillOneUDT(Idx: Integer);
// Resolve one type's field layout. With EXTENDS, the parent is filled first and its fields are
// copied at the same (bank, slot); the child's own fields are appended (prefix layout), so code
// expecting the parent works on a child handle. Cycle-safe via the Filled guard.
var
  j, n, PIdx: Integer;
  FieldNode, TypeNode: TASTNode;
  Bank: TSSARegisterType;
  cInt, cFloat, cStr: Integer;
  TypeName, FieldName, NestedT: string;
begin
  if (Idx < 0) or (Idx > High(FUDTs)) then Exit;
  if FUDTs[Idx].Filled then Exit;
  FUDTs[Idx].Filled := True;   // set before recursing to break any EXTENDS cycle

  cInt := 0; cFloat := 0; cStr := 0;
  SetLength(FUDTs[Idx].Fields, 0);
  // Inherit the parent's fields first (same banks/slots), continuing the per-bank counters.
  if FUDTs[Idx].Parent <> '' then
  begin
    PIdx := FindUDT(FUDTs[Idx].Parent);
    if PIdx >= 0 then
    begin
      FillOneUDT(PIdx);
      for j := 0 to High(FUDTs[PIdx].Fields) do
      begin
        n := Length(FUDTs[Idx].Fields);
        SetLength(FUDTs[Idx].Fields, n + 1);
        FUDTs[Idx].Fields[n] := FUDTs[PIdx].Fields[j];
      end;
      cInt := FUDTs[PIdx].NInt; cFloat := FUDTs[PIdx].NFloat; cStr := FUDTs[PIdx].NStr;
    end;
  end;

  // Append this type's own fields.
  if Assigned(FUDTs[Idx].Node) then
    for j := 0 to FUDTs[Idx].Node.ChildCount - 1 do
    begin
      FieldNode := FUDTs[Idx].Node.GetChild(j);
      if FieldNode.NodeType <> antIdentifier then Continue;
      FieldName := VarToStr(FieldNode.Value);
      TypeNode := nil;
      if FieldNode.ChildCount > 0 then TypeNode := FieldNode.GetChild(0);
      TypeName := '';
      if Assigned(TypeNode) then TypeName := UpperCase(VarToStr(TypeNode.Value));
      NestedT := '';
      if (TypeName <> '') and (FindUDT(TypeName) >= 0) then
      begin
        Bank := srtInt;        // nested record field: int handle to the nested instance
        NestedT := TypeName;
      end
      else if TypeName <> '' then
        Bank := TypeNameToBank(TypeName, FieldName)
      else
        Bank := GetVariableType(FieldName);
      n := Length(FUDTs[Idx].Fields);
      SetLength(FUDTs[Idx].Fields, n + 1);
      FUDTs[Idx].Fields[n].Name := UpperCase(FieldName);
      FUDTs[Idx].Fields[n].Bank := Bank;
      FUDTs[Idx].Fields[n].NestedType := NestedT;
      case Bank of
        srtFloat:  begin FUDTs[Idx].Fields[n].Slot := cFloat; Inc(cFloat); end;
        srtString: begin FUDTs[Idx].Fields[n].Slot := cStr;   Inc(cStr);   end;
      else
        begin FUDTs[Idx].Fields[n].Slot := cInt; Inc(cInt); end;
      end;
    end;
  FUDTs[Idx].NInt := cInt; FUDTs[Idx].NFloat := cFloat; FUDTs[Idx].NStr := cStr;
end;

procedure TSSAGenerator.FillUDTFields(Node: TASTNode);
// Resolve all registered types' fields (parent-first via FillOneUDT). Node is unused now that
// each type carries its decl node, but kept for the call shape.
var
  i: Integer;
begin
  for i := 0 to High(FUDTs) do
    FillOneUDT(i);
end;

function TSSAGenerator.ResolveMethodLabel(const TypeName, MethNm: string): string;
// Find a method by walking up the inheritance chain: Child.method, then Parent.method, ...
var
  T, Lbl: string;
  Idx, Guard: Integer;
begin
  Result := '';
  T := UpperCase(TypeName);
  Guard := 0;
  while (T <> '') and (Guard < 64) do
  begin
    Lbl := T + '.' + UpperCase(MethNm);
    if FProcDecls.ContainsKey(Lbl) then Exit(Lbl);
    Idx := FindUDT(T);
    if Idx < 0 then Break;
    T := FUDTs[Idx].Parent;
    Inc(Guard);
  end;
end;

function TSSAGenerator.IsSubtypeOf(const U, T: string): Boolean;
// True if U is T or a (transitive) subtype of T.
var
  cur, tu: string;
  idx, guard: Integer;
begin
  Result := False;
  cur := UpperCase(U); tu := UpperCase(T);
  guard := 0;
  while (cur <> '') and (guard < 64) do
  begin
    if cur = tu then Exit(True);
    idx := FindUDT(cur);
    if idx < 0 then Break;
    cur := FUDTs[idx].Parent;
    Inc(guard);
  end;
end;

function TSSAGenerator.MethodNeedsDispatch(const TypeName, MethNm: string): Boolean;
// True if a call on static type TypeName to MethNm is polymorphic: some subtype of TypeName
// has its own override of MethNm that differs from TypeName's static resolution.
var
  baseLbl, m: string;
  i: Integer;
begin
  Result := False;
  baseLbl := ResolveMethodLabel(TypeName, MethNm);
  if baseLbl = '' then Exit;
  m := UpperCase(MethNm);
  for i := 0 to High(FUDTs) do
    if IsSubtypeOf(FUDTs[i].Name, TypeName) and
       FProcDecls.ContainsKey(FUDTs[i].Name + '.' + m) and
       (FUDTs[i].Name + '.' + m <> baseLbl) then
      Exit(True);
end;

procedure TSSAGenerator.GenerateDispatchers;
// Synthesize a virtual-dispatch procedure for each needed (type, method). The dispatcher reads
// THIS's runtime type-id and bcCallSub's the concrete override (args are already staged in the
// transfer registers by the call site, and the result is delivered there too — so the dispatcher
// only forwards). All calls are static (label) bcCallSubs, keeping the CFG well-formed.
var
  d, i: Integer;
  T, m, BaseLbl, DispLbl, ULbl, CallLabel, NextLabel: string;
  HReg, TReg, UidReg, CmpReg: TSSAValue;
begin
  for d := 0 to FNeededDispatchers.Count - 1 do
  begin
    T := Copy(FNeededDispatchers[d], 1, Pos('|', FNeededDispatchers[d]) - 1);
    m := Copy(FNeededDispatchers[d], Pos('|', FNeededDispatchers[d]) + 1, MaxInt);
    BaseLbl := ResolveMethodLabel(T, m);
    if BaseLbl = '' then Continue;
    DispLbl := ProcedureLabelName('VDISP.' + T + '.' + m);

    FCurrentBlock := FProgram.GetOrCreateBlock(DispLbl);
    // THIS handle = int transfer slot 0 (THIS is always parameter 0); read its runtime type-id.
    HReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaXferLoadInt, HReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAConstInt(0));
    TReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaRecordTypeId, TReg, HReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));

    // One test per concrete override in T's subtree: if type-id matches, tail-call it and return.
    for i := 0 to High(FUDTs) do
      if IsSubtypeOf(FUDTs[i].Name, T) and FProcDecls.ContainsKey(FUDTs[i].Name + '.' + m) then
      begin
        ULbl := ProcedureLabelName(FUDTs[i].Name + '.' + m);
        UidReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, UidReg, MakeSSAConstInt(i), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        CmpReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaCmpEqInt, CmpReg, TReg, UidReg, MakeSSAValue(svkNone));  // CmpReg = (tid = i)
        CallLabel := GenerateUniqueLabel('vdcall');
        NextLabel := GenerateUniqueLabel('vdnext');
        // if not equal (CmpReg = 0) jump to the next test; else fall through to the call block.
        EmitInstruction(ssaJumpIfZero, MakeSSALabel(NextLabel), CmpReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // Call block (equal): tail-call the override, then return (result already in xfer slot).
        FProgram.GetOrCreateBlock(CallLabel);
        if Assigned(FCurrentBlock) and (FCurrentBlock.Successors.IndexOf(FProgram.FindBlock(CallLabel)) = -1) then
        begin
          FCurrentBlock.AddSuccessor(FProgram.FindBlock(CallLabel));
          FProgram.FindBlock(CallLabel).AddPredecessor(FCurrentBlock);
        end;
        FCurrentBlock := FProgram.FindBlock(CallLabel);
        EmitCallSubLabel(ULbl);
        EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        FCurrentBlock := FProgram.GetOrCreateBlock(NextLabel);  // continue with the next test
      end;

    // Default: no subtype matched — call the statically resolved base method, then return.
    EmitCallSubLabel(ProcedureLabelName(BaseLbl));
    EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    FCurrentBlock := nil;
  end;
end;

procedure TSSAGenerator.RegisterRecordVars(Node: TASTNode);
// Pre-scan DIM..AS declarations: record-typed vars hold an int handle (FVarRecordType),
// builtin-typed vars get an explicit bank (FVarExplicitType). Runs before variable
// pre-allocation so the handle/explicit type is honoured.
var
  i, k: Integer;
  Decl, ParamList, ParamNode: TASTNode;
  VarName, TypeName: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antDim then
  begin
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if Decl.NodeType <> antArrayDecl then Continue;
      // DIM name AS type  -> typed scalar (child[1] = antIdentifier type). M4.4b: a parameterised
      // "DIM v AS T(args)" also has child[2] = antArgumentList, so accept ChildCount >= 2 here
      // (the array-of-UDT case has child[1] = antDimensions, not antIdentifier, so no overlap).
      if (Decl.ChildCount >= 2) and (Decl.GetChild(1).NodeType = antIdentifier) then
        RegisterTypedVar(UpperCase(VarToStr(Decl.GetChild(0).Value)),
                         UpperCase(VarToStr(Decl.GetChild(1).Value)))
      // DIM name(dims) AS type  -> array of UDT (child[2] = antIdentifier type): record the
      // element type; the array itself is an int (handle) array.
      else if (Decl.ChildCount >= 3) and (Decl.GetChild(2).NodeType = antIdentifier) then
      begin
        TypeName := UpperCase(VarToStr(Decl.GetChild(2).Value));
        if FindUDT(TypeName) >= 0 then
          FArrayRecordType.Values[UpperCase(VarToStr(Decl.GetChild(0).Value))] := TypeName;
      end;
    end;
    Exit;
  end;
  if Node.NodeType = antProcedureDecl then
  begin
    // FUNCTION return type (M3.2): the name node (child 0) may carry a type child
    // ("FUNCTION f(...) AS rettype") — type the function name so its result slot is correct.
    if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) and
       (Node.GetChild(0).ChildCount >= 1) and (Node.GetChild(0).GetChild(0).NodeType = antIdentifier) then
    begin
      VarName := UpperCase(VarToStr(Node.GetChild(0).Value));
      TypeName := UpperCase(VarToStr(Node.GetChild(0).GetChild(0).Value));
      if TypeName <> '' then RegisterTypedVar(VarName, TypeName);
    end;
    // Record-typed (or explicit-typed) parameters: "param AS type" (M3.1). The parameter
    // node is antIdentifier(param) with a child antIdentifier(type).
    for k := 0 to Node.ChildCount - 1 do
    begin
      ParamList := Node.GetChild(k);
      if ParamList.NodeType <> antParameterList then Continue;
      for i := 0 to ParamList.ChildCount - 1 do
      begin
        ParamNode := ParamList.GetChild(i);
        if (ParamNode.NodeType = antIdentifier) and (ParamNode.ChildCount >= 1) and
           (ParamNode.GetChild(0).NodeType = antIdentifier) then
        begin
          VarName := UpperCase(VarToStr(ParamNode.Value));
          TypeName := UpperCase(VarToStr(ParamNode.GetChild(0).Value));
          if TypeName <> '' then RegisterTypedVar(VarName, TypeName);
        end;
      end;
    end;
    // fall through to recurse into the body (local DIMs etc.)
  end;
  for i := 0 to Node.ChildCount - 1 do
    RegisterRecordVars(Node.GetChild(i));
end;

procedure TSSAGenerator.RegisterTypedVar(const VarName, TypeName: string);
// Record-typed var -> int handle (+ FVarRecordType); builtin-typed -> explicit bank.
var
  Bank: TSSARegisterType;
begin
  if VarName = '' then Exit;
  if FindUDT(TypeName) >= 0 then
  begin
    FVarRecordType.Values[VarName] := TypeName;
    if FVarExplicitType.IndexOf(VarName) < 0 then
      FVarExplicitType.AddObject(VarName, TObject(PtrInt(Ord(srtInt))));  // handle is int
  end
  else
  begin
    Bank := TypeNameToBank(TypeName, VarName);
    if FVarExplicitType.IndexOf(VarName) < 0 then
      FVarExplicitType.AddObject(VarName, TObject(PtrInt(Ord(Bank))));
  end;
end;

function TSSAGenerator.VarRecordTypeName(const VarName: string): string;
// Returns the UDT type name of a record variable, or '' if it isn't a record variable.
begin
  // THIS is method-local: its type is the owner of the method currently being lowered, not the
  // single global "THIS" entry (which different methods would otherwise overwrite).
  if (FCurrentThisType <> '') and (UpperCase(VarName) = 'THIS') then
    Exit(FCurrentThisType);
  if FVarRecordType.IndexOfName(UpperCase(VarName)) < 0 then
    Result := ''
  else
    Result := FVarRecordType.Values[UpperCase(VarName)];
end;

procedure TSSAGenerator.EmitRecordInit(const HandleVal: TSSAValue; UDTIdx: Integer);
// After a record instance is allocated, recursively allocate one instance for each nested-UDT
// field and link its handle into the parent's int slot (so a.b.c works without manual init).
var
  i, NestedUDT: Integer;
  NestedHandle: TSSAValue;
begin
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
    if FUDTs[UDTIdx].Fields[i].NestedType <> '' then
    begin
      NestedUDT := FindUDT(FUDTs[UDTIdx].Fields[i].NestedType);
      if NestedUDT < 0 then Continue;
      NestedHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaRecordNew, NestedHandle,
                      MakeSSAConstInt(FUDTs[NestedUDT].NInt),
                      MakeSSAConstInt(FUDTs[NestedUDT].NFloat),
                      MakeSSAConstInt(FUDTs[NestedUDT].NStr or (Int64(NestedUDT) shl 32)));
      EmitRecordInit(NestedHandle, NestedUDT);   // deeper nesting
      EmitConstructorCall(NestedHandle, FUDTs[NestedUDT].Name);  // M4.4: construct the nested member
      EmitInstruction(ssaRecordStoreInt, MakeSSAValue(svkNone), HandleVal,
                      NestedHandle, MakeSSAConstInt(FUDTs[UDTIdx].Fields[i].Slot));
    end;
end;

procedure TSSAGenerator.EmitConstructorCall(const HandleVal: TSSAValue; const TypeName: string;
  ArgsNode: TASTNode);
// M4.4: if TypeName (or an ancestor) declares a CONSTRUCTOR, call it on the just-allocated instance.
// The constructor is a SUB whose implicit THIS is the instance handle, so we stage the handle into
// the int transfer slot 0 (THIS is parameter 0) and bcCallSub the resolved label. No virtual
// dispatch: at allocation the runtime type equals the static type, so the most-derived ctor is exact.
// M4.4b: ArgsNode (DIM v AS T(args)) supplies the constructor arguments — staged into the declared
// parameter slots that follow THIS.
var
  Lbl: string;
  Decl, ParamList: TASTNode;
  i, Slot: Integer;
  RT: TSSARegisterType;
  ArgVal: TSSAValue;
begin
  Lbl := ResolveMethodLabel(TypeName, 'CONSTRUCTOR');
  if Lbl = '' then Exit;
  EmitXferStore(srtInt, 0, HandleVal);              // THIS handle -> int xfer slot 0
  if Assigned(ArgsNode) and FProcDecls.TryGetValue(Lbl, Decl) and Assigned(Decl) and
     (Decl.ChildCount >= 2) then
  begin
    ParamList := Decl.GetChild(1);                  // includes the implicit THIS at index 0
    for i := 0 to ArgsNode.ChildCount - 1 do
    begin
      if i + 1 >= ParamList.ChildCount then Break;  // ignore surplus args (no overloading yet)
      ProcessExpression(ArgsNode.GetChild(i), ArgVal);
      Slot := ParamBankAndSlot(ParamList, i + 1, RT);  // +1: skip the implicit THIS parameter
      EmitXferStore(RT, Slot, ArgVal);
    end;
  end;
  EmitCallSubLabel(ProcedureLabelName(Lbl));
end;

procedure TSSAGenerator.EmitRecordCopy(const DestHandle, SrcHandle: TSSAValue; UDTIdx: Integer);
// Value-semantics copy (FreeBASIC): copy each field of the source instance into the destination
// instance (which already owns its own storage). Nested-UDT members are copied recursively (deep
// copy into the destination's existing nested instance), so the result shares no handles with the
// source. Using UDTIdx's field set (the static LHS type) gives correct slicing when the source is a
// subtype: only the LHS type's prefix fields are copied.
var
  i, NestedUDT, Slot: Integer;
  Bank: TSSARegisterType;
  Tmp, DNest, SNest: TSSAValue;
  LoadOp, StoreOp: TSSAOpCode;
begin
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
  begin
    Slot := FUDTs[UDTIdx].Fields[i].Slot;
    if FUDTs[UDTIdx].Fields[i].NestedType <> '' then
    begin
      // Nested record member: load both handles (int slot), deep-copy into the destination's own.
      NestedUDT := FindUDT(FUDTs[UDTIdx].Fields[i].NestedType);
      if NestedUDT < 0 then Continue;
      DNest := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaRecordLoadInt, DNest, DestHandle, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
      SNest := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaRecordLoadInt, SNest, SrcHandle, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
      EmitRecordCopy(DNest, SNest, NestedUDT);
    end
    else
    begin
      Bank := FUDTs[UDTIdx].Fields[i].Bank;
      case Bank of
        srtFloat:  begin LoadOp := ssaRecordLoadFloat;  StoreOp := ssaRecordStoreFloat;  end;
        srtString: begin LoadOp := ssaRecordLoadString; StoreOp := ssaRecordStoreString; end;
      else
        begin LoadOp := ssaRecordLoadInt; StoreOp := ssaRecordStoreInt; end;
      end;
      Tmp := MakeSSARegister(Bank, FProgram.AllocRegister(Bank));
      EmitInstruction(LoadOp, Tmp, SrcHandle, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
      EmitInstruction(StoreOp, MakeSSAValue(svkNone), DestHandle, Tmp, MakeSSAConstInt(Slot));
    end;
  end;
end;

function TSSAGenerator.ObjectTypeName(ObjNode: TASTNode): string;
// The UDT type name of an object expression, without emitting any code. Empty if not a record.
var
  ArrName, ParentType, NestedT: string;
  Bank: TSSARegisterType;
  Slot: Integer;
begin
  Result := '';
  if ObjNode = nil then Exit;
  if ObjNode.NodeType = antIdentifier then
    Result := VarRecordTypeName(VarToStr(ObjNode.Value))
  else if ObjNode.NodeType = antArrayAccess then
  begin
    if (ObjNode.ChildCount >= 1) and (ObjNode.GetChild(0).NodeType = antIdentifier) then
    begin
      ArrName := UpperCase(VarToStr(ObjNode.GetChild(0).Value));
      if FArrayRecordType.IndexOfName(ArrName) >= 0 then
        Result := FArrayRecordType.Values[ArrName];
    end;
  end
  else if ObjNode.NodeType = antMemberAccess then
  begin
    ParentType := ObjectTypeName(ObjNode.GetChild(0));
    if UDTFieldBankSlot(FindUDT(ParentType), VarToStr(ObjNode.Value), Bank, Slot, NestedT) then
      Result := NestedT;
  end;
end;

procedure TSSAGenerator.ProcessMethodCall(ObjNode: TASTNode; const ObjType, MethNm: string;
  ArgsNode: TASTNode; out Result: TSSAValue);
// Lower obj.method(args): pass the object handle as the implicit THIS first argument, then the
// declared args. A monomorphic call goes straight to the resolved method; a polymorphic one goes
// through a generated virtual dispatcher (chosen by the instance's runtime type-id). Read the
// result for a FUNCTION method.
var
  TmpArgs, Decl: TASTNode;
  i: Integer;
  RT: TSSARegisterType;
  DestVal: TSSAValue;
  IsFunc: Boolean;
  MethodLabel: string;
begin
  Result := MakeSSAValue(svkNone);
  MethodLabel := ResolveMethodLabel(ObjType, MethNm);   // static (base) target
  if MethodLabel = '' then Exit;

  // Build an argument list with the object (THIS) prepended.
  TmpArgs := TASTNode.Create(antArgumentList, ObjNode.Token);
  try
    TmpArgs.AddChild(ObjNode.Clone);
    if Assigned(ArgsNode) then
      for i := 0 to ArgsNode.ChildCount - 1 do
        TmpArgs.AddChild(ArgsNode.GetChild(i).Clone);
    if MethodNeedsDispatch(ObjType, MethNm) then
    begin
      // Polymorphic: stage per the base signature, then call the virtual dispatcher.
      StageCallArgs(MethodLabel, TmpArgs);
      FNeededDispatchers.Add(UpperCase(ObjType) + '|' + UpperCase(MethNm));
      EmitCallSubLabel(ProcedureLabelName('VDISP.' + UpperCase(ObjType) + '.' + UpperCase(MethNm)));
    end
    else
      EmitProcedureCall(MethodLabel, TmpArgs);   // monomorphic: direct static call
  finally
    TmpArgs.Free;
  end;

  IsFunc := False;
  if FProcDecls.TryGetValue(MethodLabel, Decl) and Assigned(Decl) then
    IsFunc := UpperCase(VarToStr(Decl.Value)) = 'FUNCTION';
  if IsFunc then
  begin
    RT := GetVariableType(MethodLabel);   // method return bank (record handle => int)
    DestVal := MakeSSARegister(RT, FProgram.AllocRegister(RT));
    EmitXferLoad(RT, XFER_RESULT_SLOT, DestVal);
    Result := DestVal;
  end;
end;

function TSSAGenerator.ResolveRecordObject(ObjNode: TASTNode; out HandleVal: TSSAValue;
  out TypeName: string): Boolean;
var
  ArrName, ParentType, NestedT: string;
  ParentHandle, NestedHandle: TSSAValue;
  ParentUDT, Slot: Integer;
  Bank: TSSARegisterType;
begin
  Result := False;
  TypeName := '';
  if ObjNode.NodeType = antIdentifier then
  begin
    // Record variable (or record-typed parameter): handle is the variable's int register.
    TypeName := VarRecordTypeName(VarToStr(ObjNode.Value));
    if TypeName = '' then Exit;
    HandleVal := GetOrAllocateVariable(UpperCase(VarToStr(ObjNode.Value)));
    Result := True;
  end
  else if ObjNode.NodeType = antArrayAccess then
  begin
    // Array-of-UDT element: arr(i) evaluates to the element's record handle (int).
    if (ObjNode.ChildCount < 1) or (ObjNode.GetChild(0).NodeType <> antIdentifier) then Exit;
    ArrName := UpperCase(VarToStr(ObjNode.GetChild(0).Value));
    if FArrayRecordType.IndexOfName(ArrName) < 0 then Exit;
    TypeName := FArrayRecordType.Values[ArrName];
    ProcessExpression(ObjNode, HandleVal);
    Result := True;
  end
  else if ObjNode.NodeType = antMemberAccess then
  begin
    // Chained access (a.b.c): the parent (a.b) is itself a nested-UDT field; load its handle.
    if not ResolveRecordObject(ObjNode.GetChild(0), ParentHandle, ParentType) then Exit;
    ParentUDT := FindUDT(ParentType);
    if not UDTFieldBankSlot(ParentUDT, VarToStr(ObjNode.Value), Bank, Slot, NestedT) then Exit;
    if NestedT = '' then Exit;   // parent.field is not itself a record
    NestedHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaRecordLoadInt, NestedHandle, ParentHandle,
                    MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
    HandleVal := NestedHandle;
    TypeName := NestedT;
    Result := True;
  end;
end;

procedure TSSAGenerator.ProcessMemberAccess(Node: TASTNode; out Result: TSSAValue);
// Lower a record field read "obj.field" to ssaRecordLoad<bank>(dest, handle, slot). If the
// member is not a field but a (no-arg) method of the object's type, lower a method call.
var
  TypeName, NestedT, MethodLbl: string;
  UDTIdx, Slot: Integer;
  Bank: TSSARegisterType;
  HandleVal, DestVal: TSSAValue;
  Op: TSSAOpCode;
begin
  Result := MakeSSAValue(svkNone);
  if Node.ChildCount < 1 then Exit;
  TypeName := ObjectTypeName(Node.GetChild(0));
  if TypeName = '' then Exit;
  UDTIdx := FindUDT(TypeName);
  if not UDTFieldBankSlot(UDTIdx, VarToStr(Node.Value), Bank, Slot, NestedT) then
  begin
    // Not a field — try a no-argument method call obj.method (M4.1), walking inheritance (M4.2).
    MethodLbl := ResolveMethodLabel(TypeName, VarToStr(Node.Value));
    if MethodLbl <> '' then
      ProcessMethodCall(Node.GetChild(0), TypeName, VarToStr(Node.Value), nil, Result);
    Exit;
  end;
  if not ResolveRecordObject(Node.GetChild(0), HandleVal, TypeName) then Exit;

  DestVal := MakeSSARegister(Bank, FProgram.AllocRegister(Bank));
  case Bank of
    srtFloat:  Op := ssaRecordLoadFloat;
    srtString: Op := ssaRecordLoadString;
  else
    Op := ssaRecordLoadInt;
  end;
  EmitInstruction(Op, DestVal, HandleVal, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
  Result := DestVal;
end;

procedure TSSAGenerator.ProcessMemberStore(MemberNode, ExprNode: TASTNode);
// Lower "obj.field = expr" to ssaRecordStore<bank>(handle, value, slot).
var
  TypeName, NestedT: string;
  UDTIdx, Slot: Integer;
  Bank: TSSARegisterType;
  HandleVal, ExprVal: TSSAValue;
  Op: TSSAOpCode;
begin
  if MemberNode.ChildCount < 1 then Exit;
  // Evaluate the RHS first, then resolve the target (object handle). Order matters only for
  // side effects; both are emitted before the store.
  if not ResolveRecordObject(MemberNode.GetChild(0), HandleVal, TypeName) then Exit;
  UDTIdx := FindUDT(TypeName);
  if not UDTFieldBankSlot(UDTIdx, VarToStr(MemberNode.Value), Bank, Slot, NestedT) then Exit;

  ProcessExpression(ExprNode, ExprVal);
  case Bank of
    srtFloat:  begin ExprVal := EnsureFloatRegister(ExprVal);  Op := ssaRecordStoreFloat; end;
    srtString: begin ExprVal := EnsureStringRegister(ExprVal); Op := ssaRecordStoreString; end;
  else
    begin ExprVal := EnsureIntRegister(ExprVal); Op := ssaRecordStoreInt; end;
  end;
  EmitInstruction(Op, MakeSSAValue(svkNone), HandleVal, ExprVal, MakeSSAConstInt(Slot));
end;

procedure TSSAGenerator.CollectProcedureDecl(Node: TASTNode);
// Record a SUB/FUNCTION declaration for deferred lowering (see LowerDeferredProcedures).
// AST layout: child 0 = name (antIdentifier), child 1 = antParameterList, rest = body.
var
  NameNode: TASTNode;
  Name: string;
  n: Integer;
begin
  if Node.ChildCount = 0 then Exit;
  NameNode := Node.GetChild(0);
  if NameNode.NodeType <> antIdentifier then Exit;
  Name := UpperCase(VarToStr(NameNode.Value));
  if FProcDecls.ContainsKey(Name) then Exit;  // already registered by the pre-scan
  n := Length(FDeferredProcs);
  SetLength(FDeferredProcs, n + 1);
  FDeferredProcs[n] := Node;
  if FProcedureNames.IndexOf(Name) < 0 then
    FProcedureNames.Add(Name);
  FProcDecls.AddOrSetValue(Name, Node);
end;

procedure TSSAGenerator.PreCollectProcedures(Node: TASTNode);
// Pre-scan: register every SUB/FUNCTION declaration before the main walk, so a CALL placed
// before the procedure definition still resolves its parameter types/slots.
var
  i: Integer;
begin
  if Node = nil then Exit;
  if Node.NodeType = antProcedureDecl then
    CollectProcedureDecl(Node);
  for i := 0 to Node.ChildCount - 1 do
    PreCollectProcedures(Node.GetChild(i));
end;

function TSSAGenerator.ParamBankAndSlot(ParamList: TASTNode; Index: Integer;
  out RT: TSSARegisterType): Integer;
// Walk parameters [0..Index] counting per bank; the slot is the running count for the
// bank of the Index-th parameter. Caller and callee call this with the SAME param list,
// so they agree on slot assignment. Parameter type is taken from the BASIC name suffix.
var
  i, cInt, cFloat, cStr: Integer;
  t: TSSARegisterType;
begin
  cInt := 0; cFloat := 0; cStr := 0;
  Result := 0; RT := srtInt;
  for i := 0 to Index do
  begin
    if i >= ParamList.ChildCount then Break;
    t := GetVariableType(VarToStr(ParamList.GetChild(i).Value));
    RT := t;
    case t of
      srtFloat:  begin Result := cFloat; Inc(cFloat); end;
      srtString: begin Result := cStr;   Inc(cStr);   end;
    else
      begin Result := cInt; Inc(cInt); end;
    end;
  end;
end;

procedure TSSAGenerator.EmitXferStore(RT: TSSARegisterType; Slot: Integer; const Val: TSSAValue);
// Stage a value into a transfer-register slot (Src1=value reg, Src3=const slot -> Immediate).
var
  Op: TSSAOpCode;
  V: TSSAValue;
begin
  case RT of
    srtFloat:  begin Op := ssaXferStoreFloat;  V := EnsureFloatRegister(Val); end;
    srtString: begin Op := ssaXferStoreString; V := EnsureStringRegister(Val); end;
  else
    begin Op := ssaXferStoreInt; V := EnsureIntRegister(Val); end;
  end;
  EmitInstruction(Op, MakeSSAValue(svkNone), V, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
end;

procedure TSSAGenerator.EmitXferLoad(RT: TSSARegisterType; Slot: Integer; const DestReg: TSSAValue);
// Load a transfer-register slot into a register (Dest=reg, Src3=const slot -> Immediate).
var
  Op: TSSAOpCode;
begin
  case RT of
    srtFloat:  Op := ssaXferLoadFloat;
    srtString: Op := ssaXferLoadString;
  else
    Op := ssaXferLoadInt;
  end;
  EmitInstruction(Op, DestReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
end;

procedure TSSAGenerator.StageCallArgs(const ParamOwnerName: string; ArgListNode: TASTNode);
// Evaluate each argument, coerce to the parameter's type, and stage it into the matching
// transfer slot. The parameter layout is taken from ParamOwnerName's declaration (so a
// virtual call stages per the base method's signature; the override has the same signature).
var
  Decl, ParamList, ArgExpr: TASTNode;
  i, NArgs, Slot: Integer;
  RT: TSSARegisterType;
  ArgVal: TSSAValue;
begin
  ParamList := nil;
  if FProcDecls.TryGetValue(ParamOwnerName, Decl) and Assigned(Decl) and (Decl.ChildCount >= 2) then
    ParamList := Decl.GetChild(1);
  if Assigned(ArgListNode) and
     (ArgListNode.NodeType in [antArgumentList, antExpressionList]) and Assigned(ParamList) then
  begin
    NArgs := ArgListNode.ChildCount;
    if NArgs > ParamList.ChildCount then NArgs := ParamList.ChildCount;
    for i := 0 to NArgs - 1 do
    begin
      ArgExpr := ArgListNode.GetChild(i);
      ProcessExpression(ArgExpr, ArgVal);
      Slot := ParamBankAndSlot(ParamList, i, RT);
      EmitXferStore(RT, Slot, ArgVal);
    end;
  end;
end;

procedure TSSAGenerator.EmitCallSubLabel(const LabelName: string);
// Emit ssaCallSub(label) and split the block (like GOSUB): the call block ends with ssaCallSub
// and gets two successors — the procedure entry and a fresh return-point block — so the
// procedure block has a predecessor and the dominator tree stays well-formed.
var
  CallBlock, ContBlock, ProcEntry: TSSABasicBlock;
begin
  if not Assigned(FCurrentBlock) then
    FCurrentBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel('call'));
  EmitInstruction(ssaCallSub, MakeSSALabel(LabelName), MakeSSAValue(svkNone),
                  MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  CallBlock := FCurrentBlock;
  ContBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel('aftercall'));
  if Assigned(CallBlock) then
  begin
    if CallBlock.Successors.IndexOf(ContBlock) = -1 then
    begin
      CallBlock.AddSuccessor(ContBlock);
      ContBlock.AddPredecessor(CallBlock);
    end;
    ProcEntry := FProgram.FindBlock(LabelName);
    if Assigned(ProcEntry) and (CallBlock.Successors.IndexOf(ProcEntry) = -1) then
    begin
      CallBlock.AddSuccessor(ProcEntry);
      ProcEntry.AddPredecessor(CallBlock);
    end;
  end;
  FCurrentBlock := ContBlock;
end;

procedure TSSAGenerator.EmitProcedureCall(const Name: string; ArgListNode: TASTNode);
// Static call: stage args then ssaCallSub to the named procedure.
begin
  if not Assigned(FCurrentBlock) then
    FCurrentBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel('call'));
  StageCallArgs(Name, ArgListNode);
  EmitCallSubLabel(ProcedureLabelName(Name));
end;

procedure TSSAGenerator.ProcessProcedureCall(Node: TASTNode);
// Lower a statement-level CALL: stage args + ssaCallSub. Execution resumes in the
// return-point block created by EmitProcedureCall.
var
  ArgList: TASTNode;
begin
  ArgList := nil;
  if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antArgumentList) then
    ArgList := Node.GetChild(0);
  EmitProcedureCall(UpperCase(VarToStr(Node.Value)), ArgList);
end;

procedure TSSAGenerator.LowerDeferredProcedures;
// Lower every collected SUB/FUNCTION body into its own block region. Must be called AFTER
// the module END is emitted so procedure blocks sit beyond it (never reached by fall-through;
// only via ssaCallSub). Each body ends with ssaReturnSub.
var
  i, j, Slot: Integer;
  Proc, NameNode, ParamList: TASTNode;
  Name, LabelName: string;
  RT: TSSARegisterType;
  ParamReg: TSSAValue;
begin
  for i := 0 to High(FDeferredProcs) do
  begin
    Proc := FDeferredProcs[i];
    if Proc.ChildCount = 0 then Continue;
    NameNode := Proc.GetChild(0);
    if NameNode.NodeType <> antIdentifier then Continue;
    Name := UpperCase(VarToStr(NameNode.Value));
    LabelName := ProcedureLabelName(Name);

    // Establish procedure context (for "fname = expr" results, RETURN, EXIT SUB/FUNCTION).
    FInProcedure := True;
    FCurrentProcName := Name;
    FCurrentProcIsFunction := (UpperCase(VarToStr(Proc.Value)) = 'FUNCTION');
    FCurrentProcRetType := GetVariableType(Name);
    // Method body (M4.1): the owner type (before the '.') is THIS's type while lowering here.
    if Pos('.', Name) > 0 then
      FCurrentThisType := Copy(Name, 1, Pos('.', Name) - 1)
    else
      FCurrentThisType := '';

    // Entry block for the procedure body.
    FCurrentBlock := FProgram.GetOrCreateBlock(LabelName);

    // Prologue: copy each parameter from its transfer slot into the parameter register
    // (the callee's local for that name). Child 1 = antParameterList.
    if (Proc.ChildCount >= 2) and (Proc.GetChild(1).NodeType = antParameterList) then
    begin
      ParamList := Proc.GetChild(1);
      for j := 0 to ParamList.ChildCount - 1 do
      begin
        ParamReg := GetOrAllocateVariable(VarToStr(ParamList.GetChild(j).Value));
        Slot := ParamBankAndSlot(ParamList, j, RT);
        EmitXferLoad(RT, Slot, ParamReg);
      end;
    end;

    // Body statements begin at child index 2 (0 = name, 1 = antParameterList).
    for j := 2 to Proc.ChildCount - 1 do
      ProcessStatement(Proc.GetChild(j));

    // Guarantee a trailing return frame (covers a fall-off-the-end body).
    if not Assigned(FCurrentBlock) then
      FCurrentBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel(LabelName + '_END'));
    EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    FCurrentBlock := nil;   // procedure body terminated
    FInProcedure := False;
    FCurrentProcName := '';
    FCurrentThisType := '';
  end;
end;

procedure TSSAGenerator.ProcessStatement(Node: TASTNode);
var
  LineNum: Integer;
  LabelName: string;
  i: Integer;
  PrevBlock, NewBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
  SecondsVal: TSSAValue;  // For SLEEP command
  FPSReg: TSSAValue;  // For FRAME command
  KeyNumVal, KeyTextVal, KeyNumReg, KeyTextReg: TSSAValue;  // For KEY command
  ExprResult, LineNumReg: TSSAValue;  // For TRAP command
  AddrVal, AddrReg, ValueReg: TSSAValue;  // For POKE command
  ExitKind: string;       // M2: EXIT/RETURN kind
  IsExitStmt: Boolean;    // M2
  RetVal: TSSAValue;      // M2: RETURN expr / FUNCTION result
begin
  if Node = nil then Exit;

  // Handle line numbers
  if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antLineNumber) then
  begin
    LineNum := Integer(Node.GetChild(0).Value);
    LabelName := 'LINE_' + IntToStr(LineNum);

    // PHASE 3 TIER 3: Save previous block before creating new one
    PrevBlock := FCurrentBlock;

    // Get existing or create new block for this line (prevents duplicate blocks)
    FCurrentBlock := FProgram.GetOrCreateBlock(LabelName);
    NewBlock := FCurrentBlock;

    // PHASE 3 TIER 3: Add fall-through edge PrevBlock → NewBlock
    if Assigned(PrevBlock) and Assigned(NewBlock) and (PrevBlock <> NewBlock) then
    begin
      PrevBlock.AddSuccessor(NewBlock);
      NewBlock.AddPredecessor(PrevBlock);
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
        WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', NewBlock.LabelName, ' (fall-through)');
      {$ENDIF}
    end;
  end;

  case Node.NodeType of
    antLineNumber:
    begin
      // When we encounter a line number node, check if block was already created
      // by the parent antStatement processing (at lines 3671-3693)
      LineNum := Integer(Node.Value);
      LabelName := 'LINE_' + IntToStr(LineNum);

      // Track current line number for error reporting
      FCurrentLineNumber := LineNum;

      // Only create a new block if we're not already in a block with this label
      // (The parent antStatement may have already created it)
      if not Assigned(FCurrentBlock) or (FCurrentBlock.LabelName <> LabelName) then
      begin
        // PHASE 3 TIER 3: Save previous block before creating new one
        PrevBlock := FCurrentBlock;

        // Get existing or create new block for this line (prevents duplicate blocks)
        FCurrentBlock := FProgram.GetOrCreateBlock(LabelName);
        NewBlock := FCurrentBlock;

        // PHASE 3 TIER 3: Add fall-through edge PrevBlock → NewBlock
        if Assigned(PrevBlock) and Assigned(NewBlock) and (PrevBlock <> NewBlock) then
        begin
          PrevBlock.AddSuccessor(NewBlock);
          NewBlock.AddPredecessor(PrevBlock);
          {$IFDEF DEBUG_SSA}
          if DebugSSA then
            WriteLn('[SSA] Edge: ', PrevBlock.LabelName, ' → ', NewBlock.LabelName, ' (fall-through)');
          {$ENDIF}
        end;
      end;
    end;
    antProcedureDecl:
      // SUB / FUNCTION declaration. The body is NOT lowered inline (it must not run as
      // part of module flow); it is deferred and lowered after the module END (see
      // Generate / LowerDeferredProcedures), each into its own block region reachable
      // only via ssaCallSub.
      CollectProcedureDecl(Node);
    antProcedureCall:
      ProcessProcedureCall(Node);
    antTypeDecl:
      ;  // UDT declaration: registered in the pre-scan (RegisterUDTs); nothing to emit here.
    antMemberAccess, antArrayAccess, antFunctionCall:
      // Statement-level call for side effects (e.g. obj.method(args), or a function/array
      // expression used as a statement). Lower as an expression and discard the result.
      ProcessExpression(Node, RetVal);
    antLabel:
    begin
      // Named label "name:" — start a new basic block 'LABEL_<NAME>' (the GOTO/GOSUB
      // target), with a fall-through edge from the previous block. Mirrors the
      // antLineNumber handling above (case-insensitive name).
      LabelName := 'LABEL_' + UpperCase(VarToStr(Node.Value));
      if not Assigned(FCurrentBlock) or (FCurrentBlock.LabelName <> LabelName) then
      begin
        PrevBlock := FCurrentBlock;
        FCurrentBlock := FProgram.GetOrCreateBlock(LabelName);
        NewBlock := FCurrentBlock;
        if Assigned(PrevBlock) and Assigned(NewBlock) and (PrevBlock <> NewBlock) then
        begin
          PrevBlock.AddSuccessor(NewBlock);
          NewBlock.AddPredecessor(PrevBlock);
        end;
      end;
    end;
    antAssignment: ProcessAssignment(Node);
    antConst:
    begin
      // CONST is just an assignment with a constant value
      // Process the child assignment node
      if Node.ChildCount > 0 then
        ProcessAssignment(Node.GetChild(0));
    end;
    antPrint: ProcessPrint(Node);
    antInput: ProcessInput(Node);
    antDim: ProcessDim(Node);
    antDef: ProcessDefFn(Node);
    antForLoop: ProcessForLoop(Node);
    antDoLoop: ProcessDoLoop(Node);
    antBlock: ProcessBlock(Node);
    antNext: ProcessNext(Node);
    antIf: ProcessIfStatement(Node);
    antGoto: ProcessGoto(Node);
    antGosub: ProcessGosub(Node);
    antOnGoto: ProcessOnGoto(Node);
    antOnGosub: ProcessOnGosub(Node);
    antBox: ProcessBox(Node);
    antCircle: ProcessCircle(Node);
    antDraw: ProcessDraw(Node);
    antLocate: ProcessLocate(Node);
    antGraphics: ProcessGraphics(Node);
    antScnClr: ProcessScnClr(Node);
    antColor: ProcessColor(Node);
    antSetColor: ProcessSetColor(Node);
    antWidth: ProcessWidth(Node);
    antScale: ProcessScale(Node);
    antPaint: ProcessPaint(Node);
    antWindow: ProcessWindow(Node);
    antSShape: ProcessSShape(Node);
    antGShape: ProcessGShape(Node);
    antGList: ProcessGList(Node);
    antPLoad: ProcessPLoad(Node);
    antPSave: ProcessPSave(Node);
    antPRst: ProcessPRst(Node);
    // Sound commands
    antVol: ProcessVol(Node);
    antSound: ProcessSound(Node);
    antEnvelope: ProcessEnvelope(Node);
    antTempo: ProcessTempo(Node);
    antPlay: ProcessPlay(Node);
    antFilter: ProcessFilter(Node);
    antReturn:
    begin
      ExitKind := UpperCase(VarToStr(Node.Value));   // 'EXIT[ kind]' or 'RETURN'
      IsExitStmt := Assigned(Node.Token) and (UpperCase(Node.Token.Value) = 'EXIT');
      if IsExitStmt then
      begin
        // EXIT SUB / EXIT FUNCTION inside a procedure -> frame return.
        if FInProcedure and ((Pos('SUB', ExitKind) > 0) or (Pos('FUNCTION', ExitKind) > 0)) then
        begin
          EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else if Length(FLoopStack) > 0 then
        begin
          // EXIT FOR/DO/WHILE - jump to the current loop's EndLabel
          EmitInstruction(ssaJump, MakeSSALabel(FLoopStack[High(FLoopStack)].EndLabel),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else if FInProcedure then
        begin
          // Bare EXIT inside a procedure with no enclosing loop -> frame return.
          EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else
        begin
          // EXIT outside loop at module level - treat as END (or could be error)
          WriteLn(StdErr, 'Warning: EXIT outside loop at line ', FCurrentLineNumber);
          EmitInstruction(ssaEnd, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end;
      end
      else if FInProcedure then
      begin
        // RETURN inside a procedure -> frame return. RETURN expr (FreeBASIC) inside a
        // FUNCTION also stages the result.
        if (Node.ChildCount > 0) and FCurrentProcIsFunction then
        begin
          ProcessExpression(Node.GetChild(0), RetVal);
          EmitXferStore(FCurrentProcRetType, XFER_RESULT_SLOT, RetVal);
        end;
        EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        FCurrentBlock := nil;
      end
      else
      begin
        // Normal RETURN statement (GOSUB return)
        EmitInstruction(ssaReturn, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // PHASE 3 TIER 3: RETURN terminates the current block - no fall-through
        FCurrentBlock := nil;
      end;
    end;
    antEnd:
    begin
      EmitInstruction(ssaEnd, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      // PHASE 3 TIER 3: END terminates the current block - no fall-through
      FCurrentBlock := nil;
    end;
    antStop:
    begin
      EmitInstruction(ssaStop, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      // PHASE 3 TIER 3: STOP terminates the current block - no fall-through
      FCurrentBlock := nil;
    end;
    antFast:
    begin
      EmitInstruction(ssaFast, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antSlow:
    begin
      EmitInstruction(ssaSlow, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antSleep:
    begin
      // SLEEP n - delay for n seconds
      // Child[0] = expression for seconds
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), SecondsVal);
        EmitInstruction(ssaSleep, MakeSSAValue(svkNone), SecondsVal,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // SLEEP without parameter - default to 1 second
        EmitInstruction(ssaSleep, MakeSSAValue(svkNone), MakeSSAConstInt(1),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antFrame:
    begin
      // FRAME [fps] - wait for frame sync
      // Child[0] = optional FPS expression (default 60)
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), SecondsVal);
        FPSReg := EnsureIntRegister(SecondsVal);
        EmitInstruction(ssaFrame, MakeSSAValue(svkNone), FPSReg,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // FRAME without parameter - default to 60 fps
        FPSReg := EnsureIntRegister(MakeSSAConstInt(60));
        EmitInstruction(ssaFrame, MakeSSAValue(svkNone), FPSReg,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antKey:
    begin
      // KEY [n, "text"] - define or list function keys
      // No children = list all keys
      // Child[0] = key number, Child[1] = key text
      if Node.ChildCount >= 2 then
      begin
        // KEY n, "text" - define key
        ProcessExpression(Node.GetChild(0), KeyNumVal);
        ProcessExpression(Node.GetChild(1), KeyTextVal);
        KeyNumReg := EnsureIntRegister(KeyNumVal);
        KeyTextReg := EnsureStringRegister(KeyTextVal);
        EmitInstruction(ssaKey, MakeSSAValue(svkNone), KeyNumReg, KeyTextReg, MakeSSAValue(svkNone));
      end
      else if Node.ChildCount = 1 then
      begin
        // KEY n - clear key definition
        ProcessExpression(Node.GetChild(0), KeyNumVal);
        KeyNumReg := EnsureIntRegister(KeyNumVal);
        EmitInstruction(ssaKey, MakeSSAValue(svkNone), KeyNumReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // KEY - list all keys (0 = list mode, valid keys are 1-12)
        KeyNumReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, KeyNumReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        EmitInstruction(ssaKey, MakeSSAValue(svkNone), KeyNumReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    // Debug/Trace - these are RUNTIME instructions that switch between RunFast and RunDebug
    // TRON switches to RunDebug (trace mode with line numbers)
    // TROFF switches to RunFast (pure speed, no trace)
    antTron:
    begin
      EmitInstruction(ssaTron, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antTroff:
    begin
      EmitInstruction(ssaTroff, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    // Error handling
    antTrap:
    begin
      // TRAP linenum - set error handler line
      // Child[0] = line number expression
      // NOTE: Pass constant directly to avoid SSA versioning issues
      if Node.ChildCount >= 1 then
      begin
        ProcessExpression(Node.GetChild(0), ExprResult);
        // If it's a constant, pass it directly; otherwise use register
        if ExprResult.Kind = svkConstInt then
          EmitInstruction(ssaTrap, MakeSSAValue(svkNone), ExprResult,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
        begin
          LineNumReg := EnsureIntRegister(ExprResult);
          EmitInstruction(ssaTrap, MakeSSAValue(svkNone), LineNumReg,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
      end
      else
      begin
        // TRAP without line number - disable error handler
        EmitInstruction(ssaTrap, MakeSSAValue(svkNone), MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antResume:
    begin
      // RESUME [line] - continue at error line or specified line
      if Node.ChildCount > 0 then
      begin
        // RESUME <line> - jump to specific line
        ProcessExpression(Node.GetChild(0), ExprResult);
        LineNumReg := EnsureIntRegister(ExprResult);
        EmitInstruction(ssaResume, MakeSSAValue(svkNone), LineNumReg,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
      begin
        // Plain RESUME - continue at error line (Src1 = svkNone means use FResumePC)
        EmitInstruction(ssaResume, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antResumeNext:
    begin
      // RESUME NEXT - continue at next statement after error
      EmitInstruction(ssaResumeNext, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    // DATA/READ/RESTORE
    antData: ; // DATA is pre-processed in PreProcessData, skip here
    antRead: ProcessRead(Node);
    antRestore: ProcessRestore(Node);
    // Input commands
    antGet: ProcessGet(Node);
    antGetkey: ProcessGetkey(Node);
    // Formatted output
    antPrintUsing: ProcessPrintUsing(Node);
    antPudef: ProcessPudef(Node);
    antChar: ProcessChar(Node);
    // File operations
    antLoad: ProcessLoad(Node);
    antSave: ProcessSave(Node);
    antVerify: ProcessVerify(Node);
    antBload: ProcessBload(Node);
    antBsave: ProcessBsave(Node);
    antBoot: ProcessBoot(Node);
    // Disk file I/O
    antDopen: ProcessDopen(Node);
    antDclose: ProcessDclose(Node);
    antOpen: ProcessDopen(Node);   // OPEN is alias for DOPEN
    antClose: ProcessDclose(Node); // CLOSE is alias for DCLOSE
    // File data I/O
    antGetFile: ProcessGetFile(Node);
    antInputFile: ProcessInputFile(Node);
    antPrintFile: ProcessPrintFile(Node);
    antCmd: ProcessCmd(Node);
    antAppend: ProcessAppend(Node);
    antDclear: ProcessDclear(Node);
    antRecord: ProcessRecord(Node);
    // Sprite commands
    antSprite: ProcessSprite(Node);
    antMovspr: ProcessMovspr(Node);
    antSprcolor: ProcessSprcolor(Node);
    antSprsav: ProcessSprsav(Node);
    antCollision: ProcessCollision(Node);
    antSprdef: ProcessSprdef(Node);
    antSprsave: ProcessSprsave(Node);
    antSprload: ProcessSprload(Node);
    antSprsize: ProcessSprsize(Node);
    antSprform: ProcessSprform(Node);
    // System commands
    antRun: ProcessRun(Node);
    antList: ProcessList(Node);
    antNew: ProcessNew(Node);
    antDelete: ProcessDelete(Node);
    antRenumber: ProcessRenumber(Node);
    antCatalog: ProcessCatalog(Node);
    // File management commands
    antCopy: ProcessCopyFile(Node);
    antScratch: ProcessScratch(Node);
    antRenameFile: ProcessRenameFile(Node);
    antConcat: ProcessConcat(Node);
    antMkdir: ProcessMkdir(Node);
    antChdir: ProcessChdir(Node);
    antMove: ProcessMoveFile(Node);
    antClear:
    begin
      // CLR - clear all variables
      EmitInstruction(ssaClear, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antPoke:
    begin
      // POKE address, value - write value to memory-mapped address
      // Child[0] = address expression, Child[1] = value expression
      if Node.ChildCount >= 2 then
      begin
        ProcessExpression(Node.GetChild(0), AddrVal);
        ProcessExpression(Node.GetChild(1), ExprResult);
        AddrReg := EnsureIntRegister(AddrVal);
        ValueReg := EnsureIntRegister(ExprResult);
        // Emit ssaPoke: Src1=address, Src2=value
        EmitInstruction(ssaPoke, MakeSSAValue(svkNone), AddrReg, ValueReg, MakeSSAValue(svkNone));
      end;
    end;
    antProgram, antStatement, antThen, antElse:
    begin
      // Process children
      for i := 0 to Node.ChildCount - 1 do
        ProcessStatement(Node.GetChild(i));
    end;
    {$IFDEF WEB_MODE}
    antWebCommand:
    begin
      // Web commands: SETHEADER, STATUS
      ProcessWebCommand(Node);
    end;
    {$ENDIF}
  else
    // Unhandled node type - still process children in case they contain statements
    // This ensures that nested statements are not silently dropped
    begin
      WriteLn(StdErr, '[SSA] WARNING: Unhandled node type ', Ord(Node.NodeType), ' - processing children');
      for i := 0 to Node.ChildCount - 1 do
        ProcessStatement(Node.GetChild(i));
    end;
  end;
end;

function TSSAGenerator.Generate(AST: TASTNode): TSSAProgram;
var
  LastBlock: TSSABasicBlock;
  LastInstr: TSSAInstruction;
begin
  FProgram := TSSAProgram.Create;
  FLabelCounter := 0;

  // PRE-ALLOCATE ALL VARIABLE REGISTERS FIRST
  // This prevents conflicts between variable registers and temporary expression registers
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Pre-allocating variable registers...');
  {$ENDIF}
  // UDT/record types (M3): register TYPE declarations and DIM..AS variable types BEFORE
  // variable pre-allocation, so record vars are allocated as int handles and DIM..AS builtin
  // vars use their declared bank.
  RegisterUDTs(AST);
  RegisterRecordVars(AST);

  PreAllocateVariables(AST);
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Variable registers pre-allocated: ', FVarMap.Count, ' variables');
  {$ENDIF}

  // Create entry block
  FCurrentBlock := FProgram.CreateBlock('_entry');

  // PRE-PROCESS DATA STATEMENTS FIRST
  // In BASIC, DATA statements are collected before program execution,
  // so READ can access them regardless of their position in the source
  PreProcessData(AST);

  // PRE-COLLECT SUB/FUNCTION DECLARATIONS so CALL sites can resolve parameter info even
  // for procedures defined later in the source (forward references).
  PreCollectProcedures(AST);

  // Process AST (DATA statements will be skipped since they're already processed).
  // SUB/FUNCTION declarations are collected (not lowered) during this walk.
  ProcessStatement(AST);

  // Add END to the LAST MODULE block, BEFORE procedure bodies are appended, so module
  // flow halts at END and never falls through into the procedure region.
  if FProgram.Blocks.Count > 0 then
  begin
    LastBlock := FProgram.Blocks[FProgram.Blocks.Count - 1];
    if (LastBlock.Instructions.Count = 0) or
       (LastBlock.Instructions[LastBlock.Instructions.Count-1].OpCode <> ssaEnd) then
    begin
      // Switch to last block to add END there
      FCurrentBlock := LastBlock;
      EmitInstruction(ssaEnd, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                     MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;

  // Lower SUB/FUNCTION bodies into their own block region (after the module END).
  LowerDeferredProcedures;

  // M4.3: synthesize virtual-dispatch procedures for every polymorphic (type, method) used.
  GenerateDispatchers;

  // PHASE 3 TIER 3: Fix forward GOTO/GOSUB references (now that all blocks exist,
  // including procedure bodies).
  FixForwardReferences;

  Result := FProgram;
end;

{ Helper methods for constant register tracking }

procedure TSSAGenerator.RecordConstantRegister(const Val: TSSAValue);
begin
  if Val.Kind <> svkRegister then
    Exit;

  // This method is called after emitting LoadConstFloat/LoadConstInt
  // The register now holds a known constant value
  case Val.RegType of
    srtFloat:
      begin
        // We can't directly get the const value here, so this will be called
        // from the LoadConst emission site with the constant value
      end;
    srtInt:
      begin
        // Same as above
      end;
  end;
end;

function TSSAGenerator.TryGetConstantFloat(RegIndex: Integer; out ConstValue: Double): Boolean;
begin
  Result := FConstFloatRegs.TryGetValue(RegIndex, ConstValue);
end;

function TSSAGenerator.TryGetConstantInt(RegIndex: Integer; out ConstValue: Integer): Boolean;
begin
  Result := FConstIntRegs.TryGetValue(RegIndex, ConstValue);
end;

procedure TSSAGenerator.InvalidateRegister(RegIndex: Integer; RegType: TSSARegisterType);
begin
  // Remove register from constant tracking when it gets reassigned
  case RegType of
    srtFloat: FConstFloatRegs.Remove(RegIndex);
    srtInt: FConstIntRegs.Remove(RegIndex);
  end;
end;

function TSSAGenerator.EnsureIntRegister(const Val: TSSAValue): TSSAValue;
var
  TempReg: Integer;
  FloatReg: Integer;
  FloatRegVal: TSSAValue;
begin
  // Ensure value is in an integer register
  case Val.Kind of
    svkConstInt:
    begin
      // Load constant into integer register
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkConstFloat:
    begin
      // Convert float constant to integer: load float, then convert
      FloatReg := FProgram.AllocRegister(srtFloat);
      FloatRegVal := MakeSSARegister(srtFloat, FloatReg);
      EmitInstruction(ssaLoadConstFloat, FloatRegVal, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      TempReg := FProgram.AllocRegister(srtInt);
      Result := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaFloatToInt, Result, FloatRegVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkRegister:
    begin
      if Val.RegType = srtInt then
        // Already an int register
        Result := Val
      else if Val.RegType = srtFloat then
      begin
        // Convert float register to int
        TempReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, TempReg);
        EmitInstruction(ssaFloatToInt, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        // String or other - return as-is (will likely cause error later)
        Result := Val;
    end;
  else
    // For other kinds, return as-is
    Result := Val;
  end;
end;

function TSSAGenerator.EnsureFloatRegister(const Val: TSSAValue): TSSAValue;
var
  TempReg: Integer;
  IntReg: Integer;
  IntRegVal: TSSAValue;
begin
  // Ensure value is in a float register
  case Val.Kind of
    svkConstFloat:
    begin
      // Load constant into float register
      TempReg := FProgram.AllocRegister(srtFloat);
      Result := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaLoadConstFloat, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkConstInt:
    begin
      // Convert int constant to float: load int, then convert
      IntReg := FProgram.AllocRegister(srtInt);
      IntRegVal := MakeSSARegister(srtInt, IntReg);
      EmitInstruction(ssaLoadConstInt, IntRegVal, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      TempReg := FProgram.AllocRegister(srtFloat);
      Result := MakeSSARegister(srtFloat, TempReg);
      EmitInstruction(ssaIntToFloat, Result, IntRegVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkRegister:
    begin
      if Val.RegType = srtFloat then
        // Already a float register
        Result := Val
      else if Val.RegType = srtInt then
      begin
        // Convert int register to float
        TempReg := FProgram.AllocRegister(srtFloat);
        Result := MakeSSARegister(srtFloat, TempReg);
        EmitInstruction(ssaIntToFloat, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        // String or other - return as-is (will likely cause error later)
        Result := Val;
    end;
  else
    // For other kinds, return as-is
    Result := Val;
  end;
end;

function TSSAGenerator.EnsureStringRegister(const Val: TSSAValue): TSSAValue;
var
  TempReg: Integer;
begin
  // Ensure value is in a string register
  case Val.Kind of
    svkConstString:
    begin
      // Load constant into string register
      TempReg := FProgram.AllocRegister(srtString);
      Result := MakeSSARegister(srtString, TempReg);
      EmitInstruction(ssaLoadConstString, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    svkRegister:
    begin
      if Val.RegType = srtString then
        // Already a string register
        Result := Val
      else if Val.RegType = srtFloat then
      begin
        // Convert float to string using STR$
        TempReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, TempReg);
        EmitInstruction(ssaFloatToString, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if Val.RegType = srtInt then
      begin
        // Convert int to string using IntToString
        TempReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, TempReg);
        EmitInstruction(ssaIntToString, Result, Val, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else
        // Return as-is
        Result := Val;
    end;
  else
    // For other kinds, return as-is
    Result := Val;
  end;
end;

procedure TSSAGenerator.EmitInstructionWithImmediate(Op: TSSAOpCode; const Dest, Src1, Src2: TSSAValue; ImmediateValue: Int64);
begin
  // Create instruction with immediate value passed in Src3 as a constant integer
  // This is used for functions like MID$ (length) and INSTR (start position)
  EmitInstruction(Op, Dest, Src1, Src2, MakeSSAConstInt(ImmediateValue));
end;

end.
