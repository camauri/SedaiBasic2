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
  SedaiSSATypes, SedaiBasicKeywords, SedaiNamespace, SedaiStaticLocals;

type
  { Loop info for FOR/NEXT implementation }
  // Loop kind, for multi-level EXIT/CONTINUE which target the N-th enclosing loop of a given kind.
  // WHILE/WEND is not a standalone construct here (DO WHILE is a DO), so only FOR and DO exist.
  TLoopKind = (lkFor, lkDo);
  TLoopInfo = record
    LoopKind: TLoopKind;
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
    ContLabel: string;          // CONTINUE target (FOR: increment; DO-top/WHILE: cond; DO-bottom: bottom cond; infinite: body)
    ContUsed: Boolean;          // True if a CONTINUE in the body referenced ContLabel (gates a dedicated block)
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
    PtrPointee: string;     // pointee UDT type if this field is a "T PTR" (else ''); held as an int handle
    WidthCode: Integer;     // B1.5: narrow width code for a sub-64-bit/SINGLE field (0 = full width)
    IsWString: Boolean;     // WSTRING field: srtString storage (UTF-8) but LEN/MID count by codepoint
  end;
  TUDTType = record
    Name: string;
    Fields: array of TUDTField;
    NInt, NFloat, NStr: Integer;   // per-bank slot counts (for bcRecordNew)
    Parent: string;                // M4.2: base type name (EXTENDS), or '' — single inheritance
    Node: TASTNode;                // the antTypeDecl (to fill fields on demand, parent-first)
    Filled: Boolean;               // M4.2: fields resolved (cycle-safe fill guard)
    IsUnion: Boolean;              // UNION: all fields of the same bank overlap (share slot 0)
  end;

  { Lexical scope frame (FreeBASIC -lang fb scoping, MODERN mode only). The scope stack runs
    module (outermost) -> procedure-root -> nested compound blocks (innermost). An explicit DIM
    binds a name in the innermost frame (shadowing); an implicit first-use binds at the nearest
    proc-root/module frame. Resolution walks innermost->outermost, stopping at a proc-root (so a
    plain module DIM is invisible inside a SUB) — except DIM SHARED globals, which resolve to the
    module register everywhere. In CLASSIC mode the stack is inert and resolution stays global. }
  TScopeKind = (skModule, skProcRoot, skBlock);
  TScopeFrame = record
    Kind: TScopeKind;
    Bindings: TStringList;   // NAME(UPPER) -> packed (RegType<<16 | RegIndex), like FVarMap entries
    Dtors: TStringList;      // "VAR|TYPE" UDT instances to destruct at frame exit (block frames)
    RecMarkEmitted: Boolean; // ssaRecMarkPush emitted for this frame (block frames only)
    IsLoopBody: Boolean;     // this block frame is a loop body (EXIT FOR/DO unwinds down to it inclusive)
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
    FCurrentProcRetRecType: string;   // V3: UDT type the current FUNCTION returns by value (else '')
    FCurrentResultHandle: TSSAValue;  // V3: register holding the caller's result-instance handle
    FCurrentProcLocalRecs: TStringList;  // V5: "VARNAME|TYPENAME" of the proc's DIM'd local UDTs
    FCurrentProcByvalRecs: TStringList;  // V5d: "VARNAME|TYPENAME" of the proc's BYVAL UDT param copies
    FCurrentProcByrefScalars: TStringList;  // BYREF: explicit-BYREF scalar params; Objects[] = packed (RT<<16 | slot)
                                            //        their final register value is written back to the slot at each return
    FCurrentProcAddrParams: TStringList;    // BYREF-return funcs: BYREF params carried as addresses (auto-deref);
                                            //   ValueFromIndex = pointee type name. Lets "RETURN param" return a reference.
    FModuleRecordVars: TStringList;      // V5b: "VARNAME|TYPENAME" of module-scope DIM'd UDTs (globals)
    FModuleDtorSlots: TStringList;        // V5e: module global var (UPPER) -> reserved int xfer slot (Objects[]=slot)
                                          //      holds its handle frame-independently so an END inside a SUB can
                                          //      still destroy it (its module register is saved/hidden by the frame).
    FSharedVars: TStringList;            // M6: DIM SHARED scalar names -> transfer slot (Objects[]=slot); also the "is shared" marker for scope resolution
    // Refinement #2 (cross-thread SHARED scalars): a DIM SHARED scalar is backed by a 1-element global
    // array (global arrays live in the shared FArrays, so any thread sees live updates — FreeBASIC's
    // shared-memory model). Maps the UPPER-case name -> its array index. Every read/write of the name is
    // routed to element 0 of this array, so the value is never a per-thread register.
    FSharedScalarArr: TStringList;       // name (UPPER) -> array index (Objects[]=PtrInt arrayIdx)
    FStaticMembers: TStringList;         // OOP: static member variables "TYPE.FIELD" (UPPER), backed by a shared global scalar
    FPointerVars: TStringList;           // FreeBASIC pointers: var name (UPPER) -> pointee type name (e.g. "INTEGER")
    FRefVars: TStringList;               // FreeBASIC reference variables (DIM BYREF r AS T = target): name
                                         // (UPPER) -> pointee type. r's register is int (it carries target's
                                         // address); reads/writes auto-dereference, like a BYREF-return param.
    FRawCollectChanged: Boolean;         // CollectRawPtrVars fixpoint: a new raw var was discovered this pass
    FRawPtrVars: TStringList;            // FreeBASIC raw pointers: var (UPPER) ever assigned from Allocate/CAllocate/
                                         // Reallocate. Its value is a RAWPTR_TAG byte offset → deref/arithmetic use the
                                         // raw byte heap (SizeOf-scaled), not the managed FArrays/record path.
    FFixedLenVars: TStringList;          // fixed-length string vars (UPPER) -> capacity (DIM s AS STRING/WSTRING * n);
                                         // assignments truncate to the capacity (codepoints if also a WSTRING).
    FRedimMultiArrays: TStringList;      // array names (UPPER) that appear in a multi-dim REDIM → their multi-dim
                                         // element access computes the linear index from RUNTIME dimensions
                                         // (push/resolve), since REDIM changes the strides; others stay const-folded.
    FWStringVars: TStringList;           // FreeBASIC WSTRING vars (UPPER): share the srtString bank but hold UTF-8 bytes
                                         // whose LEN/MID/LEFT/RIGHT count/index by Unicode codepoint (not byte). Assignment/
                                         // concat/copy/PRINT are unchanged (UTF-8 in, UTF-8 out); only width-aware ops differ.
    FAddrLocalVars: TStringList;         // @-taken LOCALS (in a SUB/FUNCTION): name (UPPER) -> type name. Backed by
                                         // a per-frame 1-field record (handle in hidden "<name>$REC"), so the address
                                         // is distinct per recursion level (a module-level @-taken var stays SHARED-backed).
    FByrefRetFuncs: TStringList;         // FreeBASIC BYREF function results: func name (UPPER) -> pointee type name
    FCurrentProcByrefRet: Boolean;       // lowering a "FUNCTION f() BYREF AS T" (returns an address)
    FVarWidthCode: TStringList;          // B1.5 phase 2: name (UPPER) -> narrow width code (Objects[]=PtrInt 1..7)
    FVarPrintKind: TStringList;          // B1.5 phase C: name (UPPER) -> 1=BOOLEAN, 2=unsigned-64 (print form)
    FArrayElemWidth: TStringList;        // B1.5: array name (UPPER) -> element narrow width code (1..7)
    FSwapTempSeq: Integer;               // SWAP: unique counter for synthesized snapshot temp names
    FDefTypeBank: array['A'..'Z'] of Integer;  // DEFINT/DEFSTR...: initial letter -> bank (0/1/2), -1=unset
    FInDispatcher: Boolean;              // M6: true while emitting a virtual dispatcher (skip shared sync)
    FBlockHandledVars: TStringList;      // M8: var names destructed block-scoped (excluded from frame/module dtors)
    FCurrentTopLevelLabels: TStringList;  // GOTO-unwind: names of labels at block-depth 0 in the current frame
                                          //   (module or proc). A GOTO to one of them exits every open block scope.
    FModernMode: Boolean;                // FB scope: True = MODERN (lexical scope); False = CLASSIC (global-by-name)
    FScopeStack: array of TScopeFrame;   // FB scope: proc-root + block frames (innermost = High); module = FVarMap
    FScopeSerial: Integer;               // FB scope: monotonic id for unique scoped internal names (NAME@serial)
    FCurrentThisType: string;   // M4.1: owner UDT type while lowering a method body (THIS's type)
    // UDT/record support (M3)
    FUDTs: array of TUDTType;            // declared record types
    FTypeAliases: TStringList;           // FB "TYPE alias AS underlying": alias (UPPER) -> underlying (UPPER)
    FVarRecordType: TStringList;         // var name (UPPER) -> UDT type name (UPPER)
    FVarExplicitType: TStringList;       // var name (UPPER) -> TSSARegisterType (Objects[]) for DIM..AS
    FArrayRecordType: TStringList;       // array name (UPPER) -> element UDT type name (UPPER)
    FNeededDispatchers: TStringList;     // M4.3: "TYPE|METHOD" pairs needing a virtual dispatcher

    function GenerateUniqueLabel(const Prefix: string): string;
    function GetVariableType(const VarName: string): TSSARegisterType;
    function GetOrAllocateVariable(const VarName: string): TSSAValue;
    // FB lexical scope (MODERN). ResolveExisting walks FScopeStack innermost->outermost (stopping at a
    // proc-root for non-shared names) then falls back to the module namespace (FVarMap). BindOrResolve
    // resolves a use, or binds a new register: explicit DIM in the innermost frame (shadowing), implicit
    // first-use at the nearest proc-root/module. DeclareVariable is the explicit-declaration entry point.
    function ResolveExisting(const VarName: string; out Reg: TSSAValue): Boolean;
    function BindOrResolve(const VarName: string; IsExplicitDecl: Boolean;
                           UseForcedType: Boolean = False; ForcedType: TSSARegisterType = srtInt): TSSAValue;
    function DeclareVariable(const VarName: string): TSSAValue;
    function DeclareVariableTyped(const VarName: string; RegType: TSSARegisterType): TSSAValue;  // explicit DIM with a known bank
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
    procedure EnsureObjectBaseType;                // OOP: register the built-in empty OBJECT base type (RTTI root)
    procedure CollectUDTNames(Node: TASTNode);     // pass 1: register type names (empty)
    procedure FillUDTFields(Node: TASTNode);       // pass 2: fill fields (all names known)
    procedure FillOneUDT(Idx: Integer);            // fill one type's fields (parent-first)
    function ResolveMethodLabel(const TypeName, MethNm: string): string;  // walk inheritance
    function ResolveConstructorLabel(const TypeName, ArgSig: string): string;  // M4.4g: by type signature
    function BankToChar(Bank: TSSARegisterType): Char;          // M4.4g: I/F/S bank code for a ctor signature
    function CtorSigFromParams(ParamList: TASTNode): string;    // M4.4g: type signature of a ctor's params
    function FindCtorWithDefaults(const TypeName: string; ArgCount: Integer): string;  // M4.4h: defaulted ctor
    procedure RegisterRecordVars(Node: TASTNode);  // pre-scan DIM..AS (record/explicit-typed vars)
    procedure RegisterTypedVar(const VarName, TypeName: string);  // record var or explicit-bank var
    function VarRecordTypeName(const VarName: string): string;    // '' if not a record var
    procedure EmitRecordInit(const HandleVal: TSSAValue; UDTIdx: Integer);  // alloc nested records
    procedure EmitConstructorCall(const HandleVal: TSSAValue; const TypeName: string;
                                  ArgsNode: TASTNode = nil);  // M4.4 (ArgsNode: M4.4b ctor args)
    procedure EmitDestructorCall(const HandleVal: TSSAValue; const TypeName: string);  // V5
    function FindBaseCall(Node: TASTNode): TASTNode;    // M4.4f: the body's explicit BASE call node (or nil)
    procedure CollectLocalRecordVars(Node: TASTNode);  // V5: gather a proc body's DIM'd local UDTs
    procedure EmitFrameDestructors;                     // V5: dtor calls for the current frame
    procedure EmitByrefParamStore;                      // BYREF: callee — write explicit-BYREF scalar params back to their slots
    procedure EmitByrefWriteback(const ParamOwnerName: string; ArgListNode: TASTNode);  // BYREF: caller — copy slots back into variable args
    procedure CollectModuleRecordVars(Node: TASTNode);  // V5b: gather module-scope DIM'd UDTs (skip procs)
    function TypeNeedsDestruction(const TypeName: string): Boolean;  // V5e: type (recursively) has a DESTRUCTOR
    procedure AssignModuleDtorSlots;                    // V5e: reserve an int xfer slot per destructor-bearing global
    procedure EmitModuleDestructors(UseSlots: Boolean = False);  // V5b/V5e: dtor calls for globals at program end
                                                        //   UseSlots=True (END-in-proc): read handles from reserved slots
    function InnermostBlockFrameIdx: Integer;            // M8/FB: topmost skBlock frame index, or -1
    procedure BlockScopeEnter(IsLoopBody: Boolean = False);  // M8: open a block scope (loop/IF/SCOPE) (mark push)
    procedure BlockScopeExit;                            // M8: close it (destructors + mark pop, then drop)
    procedure EmitBlockScopeCleanup(Idx: Integer);       // M8: destructors + mark pop for scope Idx (no drop)
    procedure EmitAllBlockScopesCleanup;                 // M8: unwind every active block scope (early frame exit)
    procedure CollectTopLevelLabels(Parent: TASTNode; StartIdx: Integer);  // GOTO-unwind: depth-0 labels of a frame
    procedure ScanTopLevelLabels(Node: TASTNode; var Depth: Integer);      // recursive worker for the above
    procedure EmitExitLoopCleanup;                       // M8: unwind blocks down to the loop body (EXIT FOR/DO)
    procedure EmitExitLoopCleanupN(LoopLevels: Integer); // multi-level EXIT/CONTINUE (Exit For, For)
    function FindEnclosingLoop(Kind: TLoopKind; Levels: Integer; out LoopIdx, AllDepth: Integer): Boolean;
    procedure CollectSharedVars(Node: TASTNode);        // M6: gather DIM SHARED scalars + assign slots
    procedure CollectStaticMembers(Node: TASTNode);     // OOP: gather TYPE static member vars, back each with a shared global
    procedure EmitStaticMemberAllocs;                   // OOP: allocate the static members' backing arrays at program start
    function StaticMemberBackingName(ObjNode: TASTNode; const FieldName: string): string;  // "TYPE.FIELD" backing name, or '' if not static
    procedure AddSharedVarSlot(const VName: string);    // M6: assign one shared scalar its transfer slot
    // Refinement #2: cross-thread SHARED scalars backed by a 1-element global array.
    function IsSharedScalar(const Name: string): Boolean;                       // name is a SHARED scalar (array-backed)?
    function MakeSharedScalarAccess(const Name: string; const Tok: TLexerToken): TASTNode;  // build name(0) array-access node
    procedure EmitSharedScalarStoreVal(const Name: string; const Val: TSSAValue);  // store an SSA value into name(0)
    // FreeBASIC pointers: record pointer-var pointee types and back every address-taken (@x) scalar
    // with a 1-element global array (reusing the SHARED-scalar machinery), so it has a stable address.
    procedure CollectAddressTakenVars(Node: TASTNode);
    procedure GatherByrefRetFuncNames(Node: TASTNode; Names: TStringList);   // byref-ret funcs with a BYREF param
    procedure MarkByrefRetCallArgs(Node: TASTNode; Names, Dict: TStringList); // mark their call args address-taken
    procedure CollectDimVarBanks(Node: TASTNode; Dict: TStringList);
    procedure MarkAddressTaken(Node: TASTNode; Dict: TStringList; InProc: Boolean = False);
    function PointeeBankOf(const PtrName: string): TSSARegisterType;            // bank of *p from p's declared pointee
    function PointerUDTType(const PtrName: string): string;                     // pointee UDT type if p is a "T PTR" (T a UDT), else ''
    function IsAddrParam(const Name: string): Boolean;                          // BYREF-return address-carrying param?
    function AddrParamBank(const Name: string): TSSARegisterType;               // pointee bank of an address param
    function IsRefVar(const Name: string): Boolean;                             // BYREF reference variable (auto-deref)?
    function RefVarBank(const Name: string): TSSARegisterType;                  // pointee bank of a reference variable
    function IsRawPtr(const Name: string): Boolean;                             // raw (Allocate-backed) pointer var?
    procedure CollectRawPtrVars(Node: TASTNode);                                // pre-scan: mark ptrs assigned from Allocate*
    procedure CollectWStringVars(Node: TASTNode);                               // pre-scan: mark DIM ... AS WSTRING vars
    procedure CollectRedimMultiArrays(Node: TASTNode);                          // pre-scan: arrays in a multi-dim REDIM
    function EmitArrayLinearIndex(const Indices: array of TSSAValue; const ArrInfo: TSSAArrayInfo;
                                  const ArrName: string): TSSAValue;            // row-major linear index (const or runtime)
    function IsWStringVar(const Name: string): Boolean;                         // declared WSTRING var (UTF-8, codepoint LEN)?
    function IsWStringExpr(Node: TASTNode): Boolean;                            // expression that yields a WSTRING value?
    function IsAllocCall(Node: TASTNode; out FuncU: string): Boolean;           // Node = ALLOCATE/CALLOCATE/REALLOCATE(...)?
    function RawPtrExprName(Node: TASTNode): string;                            // raw pointer var of a raw ptr expr (p, p±n), else ''
    procedure EmitRawPtrArith(Node: TASTNode; out Result: TSSAValue);           // p±n raw pointer arithmetic (SizeOf-scaled)
    function RawTypeCodeOf(const PtrName: string): Integer;                      // raw element type code for *p / p[i]
    function RawElemSizeOf(const PtrName: string): Int64;                        // SizeOf(pointee) in bytes
    function TypeSizeBytes(const TypeName: string): Int64;                       // SizeOf(T) in bytes (FB sizes)
    procedure EmitRawAlloc(CallNode: TASTNode; out Result: TSSAValue);           // ALLOCATE/CALLOCATE/REALLOCATE → raw ptr
    procedure EmitRawMemOp(const FuncU: string; ArgsNode: TASTNode; out Result: TSSAValue);  // FB_MEMCOPY/FB_MEMMOVE/CLEAR/FB_MEMCOPYCLEAR
    function IsAddrLocal(const Name: string): Boolean;                          // @-taken LOCAL (per-frame record-backed)?
    function AddrLocalBank(const Name: string): TSSARegisterType;               // bank of an @-taken local
    function AddrLocalHandle(const Name: string): TSSAValue;                    // its per-frame record handle (hidden var)
    function UDTFieldPtrPointee(UDTIdx: Integer; const FieldName: string): string;  // pointee UDT of a "T PTR" field, else ''
    function UDTFieldIsWString(UDTIdx: Integer; const FieldName: string): Boolean;  // field declared AS WSTRING?
    function DerefedType(Node: TASTNode): string;                               // FB type of *<expr> (multi-level aware)
    function DerefOperandBank(Node: TASTNode): TSSARegisterType;                // bank of *<expr> incl. pointer arithmetic (p+n)
    procedure EmitArrayElementAddress(Node: TASTNode; out Result: TSSAValue);   // @arr(i) → packed element address
    procedure EmitFieldAddress(MemberNode: TASTNode; out Result: TSSAValue);    // @obj.field → record-field pointer
    procedure EmitNewObject(Node: TASTNode; out Result: TSSAValue);             // NEW T [(args)] → heap record handle
    procedure EmitDeleteObject(Node: TASTNode);                                 // DELETE p → run destructor on the pointee
    function EmitPointerIndexAddress(const PtrName: string; IndicesNode: TASTNode): TSSAValue; // p[i] → address (p + i)
    function EmitVarAddress(const Name: string): TSSAValue;                     // packed address of a backed scalar (0=NULL)
    function IsByrefRetFunc(const Name: string): Boolean;                       // FUNCTION declared BYREF AS T?
    function ByrefRetPointeeBank(const Name: string): TSSARegisterType;         // bank of a byref function's pointee
    // FB lexical scope (MODERN only). ScopePushFrame/ScopePopFrame manage FScopeStack; the block
    // variants emit the M8 record-mark instructions. Inert in CLASSIC mode (callers gate on FModernMode).
    procedure ScopePushFrame(Kind: TScopeKind);
    procedure ScopePopFrame;
    procedure EmitSharedSyncOut;                         // M6: store shared-global registers -> their slots
    procedure EmitSharedSyncIn;                          // M6: load shared-global slots -> their registers
    procedure EmitRecordCopy(const DestHandle, SrcHandle: TSSAValue; UDTIdx: Integer);  // value-copy
    procedure EmitUserFunctionCall(const Name: string; ArgsNode: TASTNode; out Result: TSSAValue);  // V3
    function FindUDT(const TypeName: string): Integer;        // -1 if not a UDT
    function CanonicalType(const TypeName: string): string;   // resolve FB TYPE-alias chain to its base
    function UDTFieldBankSlot(UDTIdx: Integer; const FieldName: string;
                              out Bank: TSSARegisterType; out Slot: Integer;
                              out NestedType: string): Boolean;
    function UDTFieldWidthCode(UDTIdx: Integer; const FieldName: string): Integer;  // B1.5: field narrow width
    function TypeNameToBank(const TypeName, FieldName: string): TSSARegisterType;
    function NarrowConstInt(Value: Int64; WidthCode: Integer): Int64;  // B1.5 compile-time fold
    function TypeNameWidthCode(const TypeName: string): Integer;        // B1.5 phase 2: type -> narrow code
    procedure RecordVarWidth(const VarName, TypeName: string);          // B1.5 phase 2: remember a var's width
    function ApplyNarrowCode(W: Integer; Value: TSSAValue): TSSAValue;  // narrow by an explicit width code
    function ApplyScalarNarrow(const VarName: string; Value: TSSAValue): TSSAValue;  // narrow on scalar store
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
    function TryStaticMethodCall(ObjNode: TASTNode; const MethNm: string;     // TypeName.method(args) (static member, no instance)
                                 ArgsNode: TASTNode; out CallResult: TSSAValue): Boolean;
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
    procedure ProcessErase(Node: TASTNode);
    procedure ProcessRedim(Node: TASTNode);
    procedure ProcessSwap(Node: TASTNode);
    procedure ProcessDefType(Node: TASTNode);
    procedure CollectDefTypes(Node: TASTNode);
    procedure ProcessMidStatement(Node: TASTNode);
    procedure ProcessLRSetStatement(Node: TASTNode; IsLeft: Boolean);
    procedure EmitMidSubstring(ArgsNode: TASTNode; out Result: TSSAValue);
    procedure EmitWStr(ArgsNode: TASTNode; out Result: TSSAValue);   // FreeBASIC WSTR(x) -> wide string
    procedure EmitWriteFileValues(Node: TASTNode; const HandleReg: TSSAValue; ToConsole: Boolean);  // WRITE [#n] CSV
    procedure EmitStringFill(ArgsNode: TASTNode; out Result: TSSAValue);
    procedure EmitWStringFill(ArgsNode: TASTNode; out Result: TSSAValue);  // WSTRING(n,cp) -> n wide chars
    function InferExprBank(Node: TASTNode): TSSARegisterType;
    procedure EmitIif(ArgsNode: TASTNode; out Result: TSSAValue);
    // FreeBASIC bit/byte macros (LOBYTE/HIBYTE/LOWORD/HIWORD/BIT/BITSET/BITRESET) and CBOOL,
    // lowered to existing integer bitwise/shift/compare SSA ops (no new opcodes).
    procedure EmitBitMacro(const FuncName: string; ArgsNode: TASTNode; out Result: TSSAValue);
    // FreeBASIC ARRAYLEN(arr): total element count = product over dims of (ubound-lbound+1).
    procedure EmitArrayLen(ArgsNode: TASTNode; out Result: TSSAValue);
    // FreeBASIC bare string functions (CHR/STR/LEFT/RIGHT) routed to their $-suffixed forms.
    procedure EmitBareStringFunc(const DollarName: string; ArrayAccessNode: TASTNode; out Result: TSSAValue);
    // FreeBASIC short-circuit operators a ANDALSO b / a ORELSE b (lowered via the IIF/IF mechanism).
    procedure EmitShortCircuit(Node: TASTNode; out Result: TSSAValue);
    // FreeBASIC RTTI: obj IS Type -> -1 if obj's runtime type is Type or a subtype, else 0.
    procedure EmitIsCheck(ObjNode: TASTNode; const TypeName: string; out Result: TSSAValue);
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
    procedure ProcessBeep(Node: TASTNode);   // BEEP: console bell (emit CHR(7), no newline)
    procedure ProcessScreenRes(Node: TASTNode);  // SCREENRES w, h (FreeBASIC graphics)
    procedure ProcessGfxPset(Node: TASTNode);    // PSET (x, y) [, color]
    procedure ProcessGfxPaint(Node: TASTNode);   // PAINT (x, y) [, color] (flood fill)
    procedure ProcessGfxLine(Node: TASTNode);     // LINE (x1,y1)-(x2,y2) [,color] [,B|BF]
    procedure ProcessGfxCircle(Node: TASTNode);   // CIRCLE (x, y), r [, color]
    procedure ProcessPalette(Node: TASTNode);      // PALETTE [GET] [index, r, g, b] / reset
    procedure ProcessGfxColor(Node: TASTNode);     // COLOR [fg][,bg] (FreeBASIC draw colour)
    function  DefaultDrawColorReg: TSSAValue;       // omitted-colour default = current draw foreground
    procedure ProcessImageDestroy(Node: TASTNode);  // IMAGEDESTROY handle
    procedure ProcessImageInfo(Node: TASTNode);      // IMAGEINFO handle, w, h
    function  EmitGetmouse(Node, ArgListNode: TASTNode): TSSAValue;  // GETMOUSE(x,y[,w][,b][,c]) -> status
    function  EmitGetJoystick(Node, ArgListNode: TASTNode): TSSAValue;  // GETJOYSTICK(id,buttons,a1..a8) -> status
    procedure ProcessGfxSetmouse(Node: TASTNode);    // SETMOUSE [x][,y][,visibility][,clip]
    procedure ProcessGfxGet(Node: TASTNode);         // GET (x1,y1)-(x2,y2), dst
    procedure ProcessGfxPut(Node: TASTNode);         // PUT (x,y), src [, mode]
    procedure ProcessScreenInfo(Node: TASTNode);     // SCREENINFO w, h [, depth, ...]
    procedure ProcessScreenSet(Node: TASTNode);      // SCREENSET work[,visible] / FLIP
    procedure ProcessPCopy(Node: TASTNode);          // PCOPY src,dst / SCREENCOPY
    procedure ProcessGfxWindow(Node: TASTNode);      // WINDOW [SCREEN] (x1,y1)-(x2,y2)
    procedure ProcessGfxView(Node: TASTNode);        // VIEW [SCREEN] (x1,y1)-(x2,y2)
    procedure ProcessGfxScreen(Node: TASTNode);      // SCREEN mode [, depth, num_pages]
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
    procedure ProcessRmdir(Node: TASTNode);
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
    // FB scope dialect gate: True = MODERN (FreeBASIC lexical scope), False = CLASSIC (BASIC v7
    // global-by-name). Set by the compile driver from the LOAD-time dialect (line numbers => CLASSIC).
    property ModernMode: Boolean read FModernMode write FModernMode;
  end;

implementation

{$IFDEF DEBUG_SSA}
uses SedaiDebug;
{$ENDIF}

const
  // Transfer-register slot reserved for a FUNCTION's return value (per bank). Kept well
  // above any parameter slot count so it never collides with an argument slot.
  XFER_RESULT_SLOT = 255;
  // Int transfer slot carrying the caller-allocated result-instance handle for a FUNCTION that
  // returns a UDT by value (V3 return-by-value): the callee copies its return value into it.
  XFER_RESULT_HANDLE_SLOT = 254;
  // M6 (SHARED): module-global scalars are backed by dedicated transfer slots (which survive the
  // bcCallSub frame save/restore). Assigned from this base upward, per bank — kept well below the
  // result slots (254/255) and above any realistic argument-slot count (parameters use slots 0..N).
  SHARED_SLOT_BASE = 128;

constructor TSSAGenerator.Create;
var
  DefCh: Char;
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
  FCurrentProcLocalRecs := TStringList.Create;
  FCurrentProcByvalRecs := TStringList.Create;
  FCurrentProcByrefScalars := TStringList.Create;
  FCurrentProcAddrParams := TStringList.Create;
  FModuleRecordVars := TStringList.Create;
  FTypeAliases := TStringList.Create;
  FModuleDtorSlots := TStringList.Create;
  FModuleDtorSlots.CaseSensitive := False;
  FSharedVars := TStringList.Create;
  FSharedVars.CaseSensitive := False;
  FSharedScalarArr := TStringList.Create;
  FStaticMembers := TStringList.Create;
  FStaticMembers.CaseSensitive := False;
  FPointerVars := TStringList.Create;
  FAddrLocalVars := TStringList.Create;
  FRefVars := TStringList.Create;
  FRawPtrVars := TStringList.Create;
  FWStringVars := TStringList.Create;
  FWStringVars.CaseSensitive := False;
  FRedimMultiArrays := TStringList.Create;
  FRedimMultiArrays.CaseSensitive := False;
  FFixedLenVars := TStringList.Create;
  FFixedLenVars.CaseSensitive := False;
  FByrefRetFuncs := TStringList.Create;
  FSharedScalarArr.CaseSensitive := False;
  FVarWidthCode := TStringList.Create;
  FVarWidthCode.CaseSensitive := False;
  FVarPrintKind := TStringList.Create;
  FVarPrintKind.CaseSensitive := False;
  FArrayElemWidth := TStringList.Create;
  FArrayElemWidth.CaseSensitive := False;
  FInDispatcher := False;
  FBlockHandledVars := TStringList.Create;
  FBlockHandledVars.CaseSensitive := False;
  FCurrentTopLevelLabels := TStringList.Create;
  FCurrentTopLevelLabels.CaseSensitive := False;
  FModernMode := False;            // default CLASSIC; the compile driver flips this for MODERN sources
  SetLength(FScopeStack, 0);
  for DefCh := 'A' to 'Z' do FDefTypeBank[DefCh] := -1;  // DEFtype: no default-by-letter initially
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
  FCurrentProcLocalRecs.Free;
  FCurrentProcByvalRecs.Free;
  FCurrentProcByrefScalars.Free;
  FCurrentProcAddrParams.Free;
  FModuleRecordVars.Free;
  FTypeAliases.Free;
  FModuleDtorSlots.Free;
  FSharedVars.Free;
  FSharedScalarArr.Free;
  FStaticMembers.Free;
  FPointerVars.Free;
  FAddrLocalVars.Free;
  FRefVars.Free;
  FRawPtrVars.Free;
  FWStringVars.Free;
  FRedimMultiArrays.Free;
  FFixedLenVars.Free;
  FByrefRetFuncs.Free;
  FVarWidthCode.Free;
  FVarPrintKind.Free;
  FArrayElemWidth.Free;
  FBlockHandledVars.Free;
  FCurrentTopLevelLabels.Free;
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
  begin
    // FreeBASIC DEFINT/DEFSTR...: a bare name (no suffix, no explicit DIM..AS type) takes the
    // default bank set for its initial letter, if any; otherwise the classic float default.
    Result := srtFloat;
    if (UpCase(VarName[1]) >= 'A') and (UpCase(VarName[1]) <= 'Z') and
       (FDefTypeBank[UpCase(VarName[1])] >= 0) then
      Result := TSSARegisterType(FDefTypeBank[UpCase(VarName[1])]);
  end;
end;

function TSSAGenerator.ResolveExisting(const VarName: string; out Reg: TSSAValue): Boolean;
// FB lexical scope (MODERN): find an existing binding for VarName. Walk the scope stack
// innermost->outermost; stop at a proc-root for a non-shared name (so a plain module DIM is invisible
// inside a SUB), but let DIM SHARED globals fall through to the module namespace. The module namespace
// is FVarMap (case-insensitive). Returns False if the name is not yet bound anywhere visible here.
var
  f, idx: Integer;
  nameU: string;
  isShared: Boolean;
  pk: PtrInt;
begin
  Result := False;
  nameU := UpperCase(VarName);
  isShared := FSharedVars.IndexOf(nameU) >= 0;
  for f := High(FScopeStack) downto 0 do
  begin
    idx := FScopeStack[f].Bindings.IndexOf(nameU);
    if idx >= 0 then
    begin
      pk := PtrInt(FScopeStack[f].Bindings.Objects[idx]);
      Reg := MakeSSARegister(TSSARegisterType(pk shr 16), pk and $FFFF);
      Exit(True);
    end;
    if (FScopeStack[f].Kind = skProcRoot) and not isShared then
      Exit(False);   // proc isolation: do not consult the module namespace from inside a procedure
  end;
  idx := FVarMap.IndexOf(VarName);
  if idx >= 0 then
  begin
    pk := PtrInt(FVarMap.Objects[idx]);
    Reg := MakeSSARegister(TSSARegisterType(pk shr 16), pk and $FFFF);
    Result := True;
  end;
end;

function TSSAGenerator.BindOrResolve(const VarName: string; IsExplicitDecl: Boolean;
  UseForcedType: Boolean = False; ForcedType: TSSARegisterType = srtInt): TSSAValue;
// FB lexical scope (MODERN): resolve a use, or allocate+bind a new register. An explicit DIM binds in
// the innermost scope frame (shadowing any outer same-name binding); an implicit first-use (or an
// explicit DIM at pure module level) resolves if already present, else binds at the nearest proc-root,
// else in the module namespace (FVarMap). Scoped bindings get a unique internal name (NAME@serial) so
// the optimizer's variable->register map never collides across scopes/shadows.
// UseForcedType: an explicit DIM with a known declared bank (DIM x AS <type>) binds in that bank rather
// than via GetVariableType — the latter is keyed by bare name globally (first declaration wins), which
// would mis-bank a same-named variable that two procedures DIM with different types.
var
  nameU, internalName: string;
  targetIdx, f: Integer;
  RegType: TSSARegisterType;
  RegIndex: Integer;
  pk: PtrInt;
begin
  // A use, or an explicit declaration at module level, first tries to resolve an existing binding.
  if (not IsExplicitDecl) or (Length(FScopeStack) = 0) then
    if ResolveExisting(VarName, Result) then Exit;

  nameU := UpperCase(VarName);
  // Choose the binding frame: explicit DIM -> innermost frame; implicit -> nearest proc-root (else module).
  if IsExplicitDecl and (Length(FScopeStack) > 0) then
    targetIdx := High(FScopeStack)
  else
  begin
    targetIdx := -1;
    for f := High(FScopeStack) downto 0 do
      if FScopeStack[f].Kind = skProcRoot then begin targetIdx := f; Break; end;
  end;

  if UseForcedType then RegType := ForcedType
  else RegType := GetVariableType(VarName);
  RegIndex := FProgram.AllocRegister(RegType);
  pk := (Ord(RegType) shl 16) or RegIndex;

  if targetIdx >= 0 then
  begin
    FScopeStack[targetIdx].Bindings.AddObject(nameU, TObject(pk));
    Inc(FScopeSerial);
    internalName := VarName + '@' + IntToStr(FScopeSerial);
    FProgram.AddVariable(internalName);
    FProgram.MapVariableToRegister(internalName, RegType, RegIndex);
  end
  else
  begin
    // Module / global namespace (legacy global-by-name; bare source name).
    FVarMap.AddObject(VarName, TObject(pk));
    FProgram.AddVariable(VarName);
    FProgram.MapVariableToRegister(VarName, RegType, RegIndex);
  end;
  Result := MakeSSARegister(RegType, RegIndex);
end;

function TSSAGenerator.DeclareVariable(const VarName: string): TSSAValue;
// FB lexical scope: explicit-declaration entry point (DIM/REDIM/STATIC/VAR, parameters, FUNCTION
// result, FOR..AS counter). In CLASSIC it is just the legacy allocate-or-reuse.
begin
  if not FModernMode then Exit(GetOrAllocateVariable(VarName));
  Result := BindOrResolve(VarName, True);
end;

function TSSAGenerator.DeclareVariableTyped(const VarName: string; RegType: TSSARegisterType): TSSAValue;
// FB lexical scope: explicit DIM with a known declared bank (DIM x AS <type>). MODERN binds the new
// scoped variable in RegType directly, so two procedures that DIM the same name with different types do
// not collide on the single global bare-name type table. CLASSIC keeps the legacy global-by-name path.
begin
  if not FModernMode then Exit(GetOrAllocateVariable(VarName));
  Result := BindOrResolve(VarName, True, True, RegType);
end;

function TSSAGenerator.GetOrAllocateVariable(const VarName: string): TSSAValue;
var
  Idx: Integer;
  RegType: TSSARegisterType;
  RegIndex: Integer;
  PackedInfo: PtrInt;
begin
  // MODERN: route through the scope-aware resolver (a use / implicit first-use).
  if FModernMode then
  begin
    Result := BindOrResolve(VarName, False);
    Exit;
  end;
  // CLASSIC: legacy global-by-name (unchanged BASIC v7 semantics).
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
  ArgNode: TASTNode;              // WSTRING: the source AST node of a string-function argument (width detect)
  MethodOwnerType, MethodLabelName: string;  // M4.1
  ArgValue, ArgReg: TSSAValue;
  MaskValue, MaskReg: TSSAValue;   // FORMAT(num, mask): the mask string operand
  TempReg, IntReg: Integer;
  IntRegVal, TempVal: TSSAValue;
  ArgListNode: TASTNode;
  TempFloat: Double;
  TempInt: Integer;
  TempStr: string;
  ValCode: Integer;
  ConvW: Integer;   // B1.5: integer-narrowing width code for a Cxxx conversion (0 = full width)
  SelImm: Integer;  // date/time function selector encoded in the opcode Immediate
  NarrowReg: TSSAValue;
  // Array access variables
  ArrName, ArrName2, ArrNameU: string;
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
  RevLen, RevSum, RevOne, RevCnt, RevPrefix: TSSAValue;  // INSTRREV(str,sub,start) lowering temps
  TrimMode: Integer;  // bcStrTrimSet mode: 0/1/2 side (both/left/right) | 4 = Any (char-set) form
  IsAny: Boolean;     // FreeBASIC "Any" modifier on the set/substring argument
  OpLhsType, OpLabel: string;  // operator overloading: UDT operand type + resolved operator label
  OpArgs: TASTNode;
  AddrNode: TASTNode;          // VARPTR/PROCPTR: synthesized "@arg" node lowered via the address path
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
    antProcAddress:
    begin
      // @name : either the address of a data variable (FreeBASIC pointers) or, when name is a SUB,
      // its entry PC. A data variable that is address-taken was backed by a 1-element global array
      // (CollectAddressTakenVars); its packed address is (arrayId+1) shl POINTER_ARRAY_SHIFT so 0
      // stays a NULL sentinel. A child is present for @arr(i) (index list → array element) and
      // @obj.field (member access → record-field pointer).
      if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antMemberAccess) then
        EmitFieldAddress(Node.GetChild(0), Result)
      else if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antArrayAccess) then
        EmitArrayElementAddress(Node.GetChild(0), Result)
      else if VarRecordTypeName(VarToStr(Node.Value)) <> '' then
        // @obj where obj is a UDT value variable: its handle IS the pointer (managed-reference model).
        Result := EnsureIntRegister(GetOrAllocateVariable(UpperCase(VarToStr(Node.Value))))
      else if IsAddrLocal(VarToStr(Node.Value)) then
      begin
        // @local: a record-field pointer into this frame's backing record (slot 0) — distinct per call.
        Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaRefAddrField, Result, AddrLocalHandle(VarToStr(Node.Value)),
                        MakeSSAValue(svkNone), MakeSSAConstInt(0));
      end
      else if IsSharedScalar(VarToStr(Node.Value)) then
        Result := EmitVarAddress(VarToStr(Node.Value))
      else
      begin
        // @subname → the named SUB's entry PC. The PROC_<name> label resolves to a PC at bytecode
        // time (like bcCallSub's target); FixForwardReferences also adds a CFG edge to the proc
        // block so an address-taken-only SUB is not dead-block-eliminated.
        Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadProcAddr, Result,
                        MakeSSALabel(ProcedureLabelName(VarToStr(Node.Value))),
                        MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;

    antNew:
      // FreeBASIC "NEW T"/"NEW T(args)" used as an expression (RHS of "p = NEW T"). The classic
      // program-editing NEW carries no Value and is handled as a statement, so this branch only fires
      // for the FB allocator form (Value = the type name).
      EmitNewObject(Node, Result);

    antCast:
    begin
      // FreeBASIC CAST/CPTR(type, expr). A pointer target type is a value passthrough (the raw byte
      // offset / managed handle is reinterpreted, not changed — the receiving variable's declared type
      // drives the deref). A scalar target type converts the value to that bank.
      ArrName2 := UpperCase(VarToStr(Node.Value));
      ProcessExpression(Node.GetChild(0), Left);
      if (Length(ArrName2) >= 4) and (Copy(ArrName2, Length(ArrName2) - 3, 4) = ' PTR') then
        Result := EnsureIntRegister(Left)                   // pointer cast: value passthrough
      else if (ArrName2 = 'DOUBLE') or (ArrName2 = 'SINGLE') then
        Result := EnsureFloatRegister(Left)
      else
        Result := EnsureIntRegister(Left);                  // scalar int conversion (truncates float)
    end;

    antDeref:
    begin
      // *p where p is a UDT pointer: the value is the record handle itself (managed-reference model),
      // so *p just evaluates p (no load). Lets "q = *p" alias and "(*p).field" resolve.
      if (Node.GetChild(0).NodeType = antIdentifier) and
         (PointerUDTType(VarToStr(Node.GetChild(0).Value)) <> '') then
      begin
        Result := EnsureIntRegister(GetOrAllocateVariable(UpperCase(VarToStr(Node.GetChild(0).Value))));
        Exit;
      end;
      // *p / *(p±n) where p is a RAW (Allocate'd) pointer: load SizeOf(pointee) bytes from the raw heap.
      // RawPtrExprName resolves the pointer var (for its element type) through the arithmetic.
      if RawPtrExprName(Node.GetChild(0)) <> '' then
      begin
        ArrName2 := RawPtrExprName(Node.GetChild(0));
        ProcessExpression(Node.GetChild(0), Left);   // raw byte address (arithmetic already scaled)
        Left := EnsureIntRegister(Left);
        if PointeeBankOf(ArrName2) = srtFloat then
        begin
          Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
          EmitInstruction(ssaRawLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAConstInt(RawTypeCodeOf(ArrName2)));
        end
        else
        begin
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaRawLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAConstInt(RawTypeCodeOf(ArrName2)));
        end;
        Exit;
      end;
      // *p (rvalue): load the pointee value. Address = p's int value; the pointee bank comes from p's
      // declared pointer type (FPointerVars), defaulting to int.
      ProcessExpression(Node.GetChild(0), Left);
      Left := EnsureIntRegister(Left);
      FuncRetType := DerefOperandBank(Node.GetChild(0));
      DestReg := FProgram.AllocRegister(FuncRetType);
      Result := MakeSSARegister(FuncRetType, DestReg);
      case FuncRetType of
        srtFloat:  EmitInstruction(ssaRefLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        srtString: EmitInstruction(ssaRefLoadString, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      else
        EmitInstruction(ssaRefLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;

    antThreadCreate:
    begin
      // THREADCREATE(@sub, arg) / THREADCALL sub(args) → spawn an OS worker running SUB; evaluates to an
      // int handle. child0 = @sub (lowered to ssaLoadProcAddr, an int reg = entry PC); child1 =
      // antArgumentList. The args are staged into the transfer slots per SUB's parameter signature
      // (StageCallArgs, like a normal call); the spawn snapshots the transfer slots into the worker's
      // context, whose prologue loads the parameters — so workers take typed, multi-argument parameters.
      StageCallArgs(VarToStr(Node.GetChild(0).Value), Node.GetChild(1));
      ProcessExpression(Node.GetChild(0), Left);
      Left := EnsureIntRegister(Left);
      Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaThreadCreate, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antThreadSelf:
    begin
      // THREADSELF() → the current thread's handle (int; 0 on the main thread). No operands.
      Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaThreadSelf, Result, MakeSSAValue(svkNone),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antMutexCreate:
    begin
      // MUTEXCREATE() → a fresh mutex handle (int). No operands.
      Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaMutexCreate, Result, MakeSSAValue(svkNone),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antCondCreate:
    begin
      // CONDCREATE() → a fresh condition-variable handle (int). No operands.
      Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaCondCreate, Result, MakeSSAValue(svkNone),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antLiteral:
    begin
      if VarIsNumeric(Node.Value) then
      begin
        // Key the int/float bank off the literal's Variant type (how it was parsed), NOT off the
        // value: a float literal like 1.0 is an integer-valued Double and must stay a float, else
        // 1.0/3.0 would fold as integer 1 div 3 = 0. Integer literals (incl. >2^31) stay integers.
        if VarIsFloat(Node.Value) then
          Result := MakeSSAConstFloat(Double(Node.Value))
        else
          Result := MakeSSAConstInt(Int64(Node.Value));
      end
      else
      begin
        TempStr := VarToStr(Node.Value);
        // A genuine quoted string literal (lexer token ttStringLiteral) is ALWAYS a string, even when its
        // text looks numeric ("." , "5", ".5"). Without this, Val() below silently turned such literals
        // into numbers, so e.g. LEN(".") was 0 and INSTR(s, ".") was wrong. Only synthesized/ambiguous
        // literals (no string token — e.g. a file handle written bare) fall through to the Val() guess.
        if (Node.Token <> nil) and (Node.Token.TokenType = ttStringLiteral) then
          Result := MakeSSAConstString(TempStr)
        else
        begin
          // Try to convert string to number (handles cases like file handle "1")
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
    end;

    antIdentifier:
    begin
      VarName := VarToStr(Node.Value);
      // FreeBASIC boolean constants: TRUE = -1, FALSE = 0 (MODERN only; in CLASSIC v7 they are
      // ordinary variables that default to 0, so the dialect gate preserves v7 behaviour).
      if FModernMode and (UpperCase(VarName) = kTRUE) then
      begin
        Result := MakeSSAConstInt(-1);
        Exit;
      end;
      if FModernMode and (UpperCase(VarName) = kFALSE) then
      begin
        Result := MakeSSAConstInt(0);
        Exit;
      end;
      // FreeBASIC CURDIR$ / CURDIR used bare (no parentheses): the current working directory.
      if FModernMode and ((UpperCase(VarName) = kCURDIRS) or (UpperCase(VarName) = kCURDIR)) then
      begin
        Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
        EmitInstruction(ssaCurDir, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Exit;
      end;
      // FreeBASIC EXEPATH used bare: directory of the running program.
      if FModernMode and (UpperCase(VarName) = kEXEPATH) then
      begin
        Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
        EmitInstruction(ssaExePath, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Exit;
      end;
      // FreeBASIC COMMAND$ / COMMAND used bare = COMMAND$(-1): the whole command line (space-separated args).
      if FModernMode and ((UpperCase(VarName) = kCOMMAND) or (UpperCase(VarName) = kCOMMANDS)) then
      begin
        ArgReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(-1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
        EmitInstruction(ssaCommand, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Exit;
      end;
      // BYREF-return address param: the register holds the caller variable's address — reading the
      // parameter dereferences it (load the pointee through the address).
      if IsAddrParam(VarName) then
      begin
        Left := EnsureIntRegister(GetOrAllocateVariable(VarName));
        FuncRetType := AddrParamBank(VarName);
        Result := MakeSSARegister(FuncRetType, FProgram.AllocRegister(FuncRetType));
        case FuncRetType of
          srtFloat:  EmitInstruction(ssaRefLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          srtString: EmitInstruction(ssaRefLoadString, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        else
          EmitInstruction(ssaRefLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
      end
      // BYREF reference variable (DIM BYREF r AS T = target): r's register carries target's address —
      // reading r dereferences it (load the pointee through the address), so r reads as target.
      else if IsRefVar(VarName) then
      begin
        Left := EnsureIntRegister(GetOrAllocateVariable(VarName));
        FuncRetType := RefVarBank(VarName);
        Result := MakeSSARegister(FuncRetType, FProgram.AllocRegister(FuncRetType));
        case FuncRetType of
          srtFloat:  EmitInstruction(ssaRefLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          srtString: EmitInstruction(ssaRefLoadString, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        else
          EmitInstruction(ssaRefLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end;
      end
      // @-taken local: read its per-frame backing record (field slot 0) through the hidden handle.
      else if IsAddrLocal(VarName) then
      begin
        FuncRetType := AddrLocalBank(VarName);
        Result := MakeSSARegister(FuncRetType, FProgram.AllocRegister(FuncRetType));
        case FuncRetType of
          srtFloat:  EmitInstruction(ssaRecordLoadFloat, Result, AddrLocalHandle(VarName), MakeSSAValue(svkNone), MakeSSAConstInt(0));
          srtString: EmitInstruction(ssaRecordLoadString, Result, AddrLocalHandle(VarName), MakeSSAValue(svkNone), MakeSSAConstInt(0));
        else
          EmitInstruction(ssaRecordLoadInt, Result, AddrLocalHandle(VarName), MakeSSAValue(svkNone), MakeSSAConstInt(0));
        end;
      end
      // Refinement #2: a SHARED scalar is backed by a 1-element global array — read element 0 (a live,
      // cross-thread load), not a per-thread register.
      else if IsSharedScalar(VarName) then
      begin
        ArgListNode := MakeSharedScalarAccess(VarName, Node.Token);
        ProcessExpression(ArgListNode, Result);
        ArgListNode.Free;
      end
      else
        // Return the register assigned to this variable
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
      else if (VarName = 'INKEY') or (VarName = 'INKEY$') then
      begin
        // INKEY / INKEY$ (FreeBASIC/QB): non-blocking read of one key; "" if none. Reuses the
        // GET non-blocking key opcode as an expression (side-effecting: consumes one keystroke).
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaGet, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'CSRLIN' then
      begin
        // CSRLIN (FreeBASIC/QB): current text cursor row (parallels POS(0) for the column).
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaCsrlin, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if (VarName = 'EL') or (VarName = 'ERL') then
      begin
        // EL / ERL (FreeBASIC) returns last error line number - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadEL, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if (VarName = 'ER') or (VarName = 'ERR') then
      begin
        // ER / ERR (FreeBASIC) returns last error code - integer
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
      else if VarName = 'DS' then
      begin
        // DS (Commodore): disk status code = last file-operation error code (0 = OK) - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadDS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'DS$' then
      begin
        // DS$ (Commodore): disk status message line "NN, MESSAGE,00,00" - string
        DestReg := FProgram.AllocRegister(srtString);
        Result := MakeSSARegister(srtString, DestReg);
        EmitInstruction(ssaLoadDSS, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if VarName = 'ST' then
      begin
        // ST (Commodore): Kernal I/O status byte (bit 6 = EOF on the last GET#) - integer
        DestReg := FProgram.AllocRegister(srtInt);
        Result := MakeSSARegister(srtInt, DestReg);
        EmitInstruction(ssaLoadST, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
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
      // FreeBASIC short-circuit operators a ANDALSO b / a ORELSE b: only the taken side of b is
      // evaluated, so lower via the IIF/IF mechanism (a real branch) instead of the eager path below.
      if (Node.ChildCount >= 2) and Assigned(Node.Token) and
         ((Node.Token.TokenType = ttOpAndAlso) or (Node.Token.TokenType = ttOpOrElse)) then
      begin
        EmitShortCircuit(Node, Result);
        Exit;
      end;
      // FreeBASIC RTTI: obj IS Type. The right child is a type name (an identifier), not a value.
      if (Node.ChildCount >= 2) and Assigned(Node.Token) and (Node.Token.TokenType = ttOpIs) then
      begin
        EmitIsCheck(Node.GetChild(0), VarToStr(Node.GetChild(1).Value), Result);
        Exit;
      end;
      // FreeBASIC raw pointer arithmetic: "p + n" / "p - n" (and "n + p") where p is a raw pointer.
      // The index is scaled by SizeOf(pointee) — the result is a raw byte pointer. (Managed pointers
      // keep element-unit arithmetic via the normal numeric lowering below.)
      if (Node.ChildCount >= 2) and Assigned(Node.Token) and
         ((Node.Token.TokenType = ttOpAdd) or (Node.Token.TokenType = ttOpSub)) and
         (RawPtrExprName(Node) <> '') then
      begin
        EmitRawPtrArith(Node, Result);
        Exit;
      end;
      // Operator overloading (FreeBASIC): if both operands are UDT handles of the same type T and a
      // user "OPERATOR <sym>(a AS T, b AS T)" is defined, lower a call to it (label T.OPERATOR<sym>)
      // instead of a numeric op. Resolved by the left operand's static type; direct binary form (the
      // chained "(a+b)+c" case relies on the result type being inferable — v1 covers direct operands).
      if (Node.ChildCount >= 2) and Assigned(Node.Token) then
      begin
        OpLhsType := ObjectTypeName(Node.GetChild(0));
        if (OpLhsType <> '') and (ObjectTypeName(Node.GetChild(1)) = OpLhsType) then
        begin
          OpLabel := ResolveMethodLabel(OpLhsType, 'OPERATOR' + VarToStr(Node.Token.Value));
          if OpLabel <> '' then
          begin
            OpArgs := TASTNode.Create(antArgumentList, Node.Token);
            OpArgs.AddChild(Node.GetChild(0).Clone);
            OpArgs.AddChild(Node.GetChild(1).Clone);
            EmitUserFunctionCall(OpLabel, OpArgs, Result);
            OpArgs.Free;
            Exit;
          end;
        end;
      end;

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
            // `/` is always floating-point division (both C64 BASIC and FreeBASIC), even for
            // integer operands - so 10/4 = 2.5, matching the non-const lowering above. Folding
            // to integer `div` here made the constant case disagree with the runtime.
            if Right.ConstInt <> 0 then
              Result := MakeSSAConstFloat(Left.ConstInt / Right.ConstInt)
            else
              Result := MakeSSAValue(svkNone);
          ttOpIntDiv:  // \ integer division (FreeBASIC), truncates toward zero
            if Right.ConstInt <> 0 then
              Result := MakeSSAConstInt(Left.ConstInt div Right.ConstInt)
            else
              Result := MakeSSAValue(svkNone);
          ttOpShl: Result := MakeSSAConstInt(Left.ConstInt shl Right.ConstInt);
          ttOpShr: Result := MakeSSAConstInt(Left.ConstInt shr Right.ConstInt);
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

        // Bitwise operators (AND, OR, XOR, EQV, IMP) - result is always Int
        ttBitwiseAND, ttBitwiseOR, ttBitwiseXOR, ttOpEqv, ttOpImp:
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
            ttBitwiseAND: EmitInstruction(ssaBitwiseAnd, Result, Left, Right, MakeSSAValue(svkNone));
            ttBitwiseOR: EmitInstruction(ssaBitwiseOr, Result, Left, Right, MakeSSAValue(svkNone));
            ttBitwiseXOR: EmitInstruction(ssaBitwiseXor, Result, Left, Right, MakeSSAValue(svkNone));
            ttOpEqv:
              begin
                // a EQV b = NOT (a XOR b): bitwise equivalence.
                TempReg := FProgram.AllocRegister(srtInt);
                TempVal := MakeSSARegister(srtInt, TempReg);
                EmitInstruction(ssaBitwiseXor, TempVal, Left, Right, MakeSSAValue(svkNone));
                EmitInstruction(ssaBitwiseNot, Result, TempVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              end;
            ttOpImp:
              begin
                // a IMP b = (NOT a) OR b: bitwise implication.
                TempReg := FProgram.AllocRegister(srtInt);
                TempVal := MakeSSARegister(srtInt, TempReg);
                EmitInstruction(ssaBitwiseNot, TempVal, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
                EmitInstruction(ssaBitwiseOr, Result, TempVal, Right, MakeSSAValue(svkNone));
              end;
          else
            EmitInstruction(ssaBitwiseAnd, Result, Left, Right, MakeSSAValue(svkNone));
          end;
        end;

      else
        // FreeBASIC "&": ALWAYS string concatenation; coerce both operands to their string form
        // (numeric operands become their STR$ representation).
        if Node.Token.TokenType = ttOpConcat then
        begin
          // Materialize numeric constants into a register first (EnsureStringRegister only coerces
          // registers / const strings), then convert each operand to its string form.
          if Left.Kind = svkConstInt then Left := EnsureIntRegister(Left)
          else if Left.Kind = svkConstFloat then Left := EnsureFloatRegister(Left);
          if Right.Kind = svkConstInt then Right := EnsureIntRegister(Right)
          else if Right.Kind = svkConstFloat then Right := EnsureFloatRegister(Right);
          Left := EnsureStringRegister(Left);
          Right := EnsureStringRegister(Right);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrConcat, Result, Left, Right, MakeSSAValue(svkNone));
          Exit;   // concat emits its own instruction; do NOT fall through to the trailing arithmetic emit
        end
        // SPECIAL CASE: String concatenation (string + string or string + any)
        else if (Node.Token.TokenType = ttOpAdd) and
           ((Left.RegType = srtString) or (Right.RegType = srtString)) then
        begin
          // String concatenation
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrConcat, Result, Left, Right, MakeSSAValue(svkNone));
          Exit;   // concat emits its own instruction; do NOT fall through to the trailing arithmetic emit
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
        // Exponentiation (^): like FreeBASIC, always operates in and returns floating point (there is no
        // integer power opcode). Without this, integer "a ^ b" fell through to ssaAddInt (a + b)!
        else if Node.Token.TokenType = ttOpPow then
        begin
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
          OpCode := ssaPowFloat;
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
        // FreeBASIC \ (integer division), SHL, SHR: always integer; truncate any float operand.
        else if (Node.Token.TokenType = ttOpIntDiv) or (Node.Token.TokenType = ttOpShl) or
                (Node.Token.TokenType = ttOpShr) then
        begin
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
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          case Node.Token.TokenType of
            ttOpIntDiv: OpCode := ssaDivInt;
            ttOpShl: OpCode := ssaShl;
            ttOpShr: OpCode := ssaShr;
          else
            OpCode := ssaDivInt;
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

        // User-defined FUNCTION (M2): stage args, call, read the result (scalar via transfer slot,
        // or UDT by value via V3 return-by-value — see EmitUserFunctionCall).
        if FProcedureNames.IndexOf(FuncName) >= 0 then
        begin
          EmitUserFunctionCall(FuncName, ArgListNode, Result);
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
        else if FuncName = 'POINT' then
        begin
          // FreeBASIC POINT(x, y) -> pixel color (read from the graphics backend).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            ProcessExpression(ArgListNode.GetChild(1), RVal);     RReg := EnsureIntRegister(RVal);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaGfxPoint, Result, ArgReg, RReg, MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('POINT requires 2 arguments: POINT(x, y)');
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
          // LEN(str) - returns integer length. For a WSTRING argument the result is the Unicode codepoint
          // count (ssaStrLenW) rather than the byte length (ssaStrLen); the UTF-8 storage is the same.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgNode := ArgListNode.GetChild(0);
          end
          else if ArgListNode <> nil then
          begin
            ProcessExpression(ArgListNode, ArgValue);
            ArgNode := ArgListNode;
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          if IsWStringExpr(ArgNode) then
            EmitInstruction(ssaStrLenW, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
          else
            EmitInstruction(ssaStrLen, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = kFREEFILE) then
        begin
          // FreeBASIC FREEFILE - no argument; the lowest unused file number (handle slot is ignored).
          ArgReg := EnsureIntRegister(MakeSSAConstInt(0));
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaFileQuery, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(1));
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
        else if (FuncName = kWCHR) then
        begin
          // WCHR(n) - the wide character for Unicode codepoint n, encoded as UTF-8 bytes (unlike CHR$,
          // which emits a single raw byte). Single-codepoint form (v1).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureIntRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          EmitInstruction(ssaStrWChr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
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
        else if (FuncName = 'HEX$') or (FuncName = kWHEX) then
        begin
          // HEX$(n) / WHEX(n) - hex string (WHEX = wide; ASCII hex digits are identical UTF-8)
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
        else if (FuncName = 'OCT') or (FuncName = 'BIN') or (FuncName = kWOCT) or (FuncName = kWBIN) then
        begin
          // OCT/BIN/WOCT/WBIN(n) - octal/binary string of an integer (no leading zeros). The W* forms are
          // wide; the digits are ASCII so the bytes are identical to the narrow form.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureIntRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          if (FuncName = 'OCT') or (FuncName = kWOCT) then
            EmitInstruction(ssaStrOct, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
          else
            EmitInstruction(ssaStrBin, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'VALINT') or (FuncName = 'VALLNG') or (FuncName = 'VALUINT') or
                (FuncName = kVALULNG) then
        begin
          // VALINT/VALLNG/VALUINT(s) - parse leading integer from a string (0 if none).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if ArgListNode <> nil then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          ArgReg := EnsureStringRegister(ArgValue);
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaStrValInt, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if (FuncName = 'LBOUND') or (FuncName = 'UBOUND') then
        begin
          // LBOUND/UBOUND(arrayname [, dim]) - bound of a dimension (1-based dim, default 1).
          if ArgListNode = nil then begin Result := MakeSSAValue(svkNone); Exit; end;
          if (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            IndicesNode := ArgListNode.GetChild(0)   // reuse IndicesNode as the array-name node
          else
            IndicesNode := ArgListNode;
          // The array name may arrive as a bare identifier or wrapped in an array-access node.
          if IndicesNode.NodeType = antArrayAccess then
            ArrName := VarToStr(IndicesNode.GetChild(0).Value)
          else
            ArrName := VarToStr(IndicesNode.Value);
          ArrayIdx := FProgram.FindArray(ArrName);
          if ArrayIdx < 0 then
            raise Exception.CreateFmt('%s: array not declared: %s', [FuncName, ArrName]);
          ArrInfo := FProgram.GetArray(ArrayIdx);
          ArrayRef := MakeSSAArrayRef(ArrayIdx, srtInt);

          // 0-based dimension index in an int register: (dim - 1), default 0.
          TempReg := FProgram.AllocRegister(srtInt);
          ArgReg := MakeSSARegister(srtInt, TempReg);
          if (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(1), ArgValue);
            if ArgValue.Kind = svkConstInt then
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(ArgValue.ConstInt - 1), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
            else if ArgValue.Kind = svkConstFloat then
              EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(Trunc(ArgValue.ConstFloat) - 1), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
            else
            begin
              IntRegVal := EnsureIntRegister(ArgValue);
              TempReg := FProgram.AllocRegister(srtInt);
              TempVal := MakeSSARegister(srtInt, TempReg);
              EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              EmitInstruction(ssaSubInt, ArgReg, IntRegVal, TempVal, MakeSSAValue(svkNone));
            end;
          end
          else
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));

          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          if FuncName = 'LBOUND' then
            EmitInstruction(ssaArrayLBound, Result, ArrayRef, ArgReg, MakeSSAValue(svkNone))
          else
            EmitInstruction(ssaArrayUBound, Result, ArrayRef, ArgReg, MakeSSAValue(svkNone));
        end
        else if (FuncName = 'INSTRREV') then
        begin
          // INSTRREV(str, sub [, start]) -> int position of the last occurrence of sub (Dest=int).
          // With an explicit `start` (FreeBASIC), only matches that BEGIN at or before `start` count:
          // lowered (like the MID statement, with existing ops, no new opcode) as
          //   INSTRREV( LEFT$(str, start + LEN(sub) - 1), sub )
          // so a match starting at position P survives iff P + LEN(sub) - 1 <= start + LEN(sub) - 1,
          // i.e. P <= start. Assumes start >= 1 (the usual case); a non-positive runtime start narrows
          // to a short prefix rather than searching from the end (use the 2-arg form for from-the-end).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);   // str
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value);  // substring / char set
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureStringRegister(Arg2Value);
            // FreeBASIC "Any" form: the 2nd argument is a character SET; find the last single char in
            // it (ssaStrInstrRevAny) rather than the whole substring.
            IsAny := ArgListNode.GetChild(1).Attributes.Values['ANYSET'] = '1';
            if ArgListNode.ChildCount >= 3 then
            begin
              ProcessExpression(ArgListNode.GetChild(2), Arg3Value);  // start (1-based)
              Arg3Reg := EnsureIntRegister(Arg3Value);
              if IsAny then
              begin
                // single-char matches: a match at P survives iff P <= start -> LEFT$(str, start)
                RevPrefix := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
                EmitInstruction(ssaStrLeft, RevPrefix, ArgReg, Arg3Reg, MakeSSAValue(svkNone));
              end
              else
              begin
                // substring: a match at P survives iff P <= start -> LEFT$(str, start + LEN(sub) - 1)
                RevLen := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
                EmitInstruction(ssaStrLen, RevLen, Arg2Reg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
                RevSum := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
                EmitInstruction(ssaAddInt, RevSum, Arg3Reg, RevLen, MakeSSAValue(svkNone));
                RevOne := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
                EmitInstruction(ssaLoadConstInt, RevOne, MakeSSAConstInt(1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
                RevCnt := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
                EmitInstruction(ssaSubInt, RevCnt, RevSum, RevOne, MakeSSAValue(svkNone));
                RevPrefix := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
                EmitInstruction(ssaStrLeft, RevPrefix, ArgReg, RevCnt, MakeSSAValue(svkNone));
              end;
              ArgReg := RevPrefix;   // search within the restricted prefix
            end;
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            if IsAny then
              EmitInstruction(ssaStrInstrRevAny, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone))
            else if (ArgListNode.ChildCount = 2) and IsWStringExpr(ArgListNode.GetChild(0)) then
              // WSTRING haystack (no start arg): codepoint position of the last occurrence.
              EmitInstruction(ssaStrInstrRevW, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone))
            else
              EmitInstruction(ssaStrInstrRev, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'SPACE') or (FuncName = 'SPACE$') or (FuncName = kWSPACE) then
        begin
          // SPACE(n) -> string of n spaces (Dest=string, single int arg).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
            DestReg := FProgram.AllocRegister(srtString);
            Result := MakeSSARegister(srtString, DestReg);
            EmitInstruction(ssaStrSpace, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'LTRIM') or (FuncName = 'RTRIM') or (FuncName = 'TRIM') or
                (FuncName = 'UCASE') or (FuncName = 'UCASE$') or
                (FuncName = 'LCASE') or (FuncName = 'LCASE$') then
        begin
          // FreeBASIC single-arg string functions: one string in, string out (B1.2).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);
            ArgReg := EnsureStringRegister(ArgValue);
            DestReg := FProgram.AllocRegister(srtString);
            Result := MakeSSARegister(srtString, DestReg);
            // FreeBASIC trimset form: LTRIM/RTRIM/TRIM(s, set) trims the `set` substring from the
            // end(s). One opcode (bcStrTrimSet) with the side encoded as a constant in Src3->Immediate
            // (0=both, 1=left, 2=right). The `Any` (character-set) keyword form is not yet supported.
            if ((FuncName = 'LTRIM') or (FuncName = 'RTRIM') or (FuncName = 'TRIM')) and
               (ArgListNode.ChildCount >= 2) then
            begin
              ProcessExpression(ArgListNode.GetChild(1), Arg2Value);   // trimset
              Arg2Reg := EnsureStringRegister(Arg2Value);
              if FuncName = 'LTRIM' then TrimMode := 1                  // left
              else if FuncName = 'RTRIM' then TrimMode := 2             // right
              else TrimMode := 0;                                       // both (TRIM)
              // FreeBASIC "Any" form: trim any character in the set (bit 4) instead of the substring.
              if ArgListNode.GetChild(1).Attributes.Values['ANYSET'] = '1' then
                TrimMode := TrimMode or 4;
              EmitInstruction(ssaStrTrimSet, Result, ArgReg, Arg2Reg, MakeSSAConstInt(TrimMode));
            end
            else
            begin
              if FuncName = 'LTRIM' then OpCode := ssaStrLTrim
              else if FuncName = 'RTRIM' then OpCode := ssaStrRTrim
              else if FuncName = 'TRIM' then OpCode := ssaStrTrim
              else if (FuncName = 'UCASE') or (FuncName = 'UCASE$') then OpCode := ssaStrUCase
              else OpCode := ssaStrLCase;
              EmitInstruction(OpCode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end;
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
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
            if IsWStringExpr(ArgListNode.GetChild(0)) then  // WSTRING: n is a codepoint count
              EmitInstruction(ssaStrLeftW, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone))
            else
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
            if IsWStringExpr(ArgListNode.GetChild(0)) then  // WSTRING: n is a codepoint count
              EmitInstruction(ssaStrRightW, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone))
            else
              EmitInstruction(ssaStrRight, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        else if (FuncName = 'MID$') then
          // MID$(str, start [,length]) - v7 substring function (works in both dialects).
          EmitMidSubstring(ArgListNode, Result)
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
            else if IsWStringExpr(ArgListNode.GetChild(0)) then
              // WSTRING haystack (no start arg): return a codepoint position, not a byte position.
              EmitInstruction(ssaStrInstrW, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone))
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
        else if (FuncName = 'ATAN2') then
        begin
          // ATAN2(y, x) - two-argument arctangent; Src1 = y, Src2 = x.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue);  // y
            ProcessExpression(ArgListNode.GetChild(1), Arg2Value); // x
            ArgReg := EnsureFloatRegister(ArgValue);
            Arg2Reg := EnsureFloatRegister(Arg2Value);
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
            EmitInstruction(ssaMathAtan2, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
          end
          else begin Result := MakeSSAValue(svkNone); Exit; end;
        end
        // FreeBASIC bare date/time functions (no arguments): NOW/TIMER (numeric serial / seconds),
        // DATE/TIME (formatted strings). Registered as MODERN-only keywords, parsed with an empty
        // argument list. Immediate selects which: NOW=0/TIMER=1, DATE=0/TIME=1.
        else if (FuncName = kNOW) or (FuncName = kTIMER) then
        begin
          DestReg := FProgram.AllocRegister(srtFloat);
          Result := MakeSSARegister(srtFloat, DestReg);
          if FuncName = kTIMER then SelImm := 1 else SelImm := 0;
          EmitInstruction(ssaDateNow, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
        end
        else if (FuncName = kDATEFN) or (FuncName = kTIMEFN) then
        begin
          DestReg := FProgram.AllocRegister(srtString);
          Result := MakeSSARegister(srtString, DestReg);
          if FuncName = kTIMEFN then SelImm := 1 else SelImm := 0;
          EmitInstruction(ssaDateStr, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
        end
        else if (FuncName = 'CINT') or (FuncName = 'CLNG') or (FuncName = 'CLNGINT') or
                (FuncName = kCULNGINT) or
                (FuncName = 'CSHORT') or (FuncName = 'CBYTE') or (FuncName = 'CUBYTE') or
                (FuncName = 'CUSHORT') or (FuncName = 'CUINT') or (FuncName = 'CULNG') then
        begin
          // FreeBASIC integer conversion functions: round-to-nearest (banker's rounding)
          // toward an integer result, then wrap/sign-extend to the type's width (B1.5).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if (ArgListNode <> nil) and (ArgListNode.NodeType <> antArgumentList) then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          // Width code for the narrowing: signed/unsigned 8/16/32-bit; 0 = full 64-bit (CINT/CUINT/CLNGINT).
          if FuncName = 'CBYTE' then ConvW := 1
          else if FuncName = 'CUBYTE' then ConvW := 2
          else if FuncName = 'CSHORT' then ConvW := 3
          else if FuncName = 'CUSHORT' then ConvW := 4
          else if FuncName = 'CLNG' then ConvW := 5
          else if FuncName = 'CULNG' then ConvW := 6
          else ConvW := 0;

          if ArgValue.Kind = svkConstFloat then
            Result := MakeSSAConstInt(Round(ArgValue.ConstFloat))
          else if ArgValue.Kind = svkConstInt then
            Result := MakeSSAConstInt(ArgValue.ConstInt)
          else if (ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtFloat) then
          begin
            // Round-to-even (banker's): bcFloatRound, distinct from truncating ssaFloatToInt.
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaFloatRound, Result, ArgValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            // Already an integer value (register or string-coerced) - pass through.
            Result := EnsureIntRegister(ArgValue);

          // Narrow to the declared width: fold constants, else emit bcNarrowInt.
          if ConvW <> 0 then
          begin
            if Result.Kind = svkConstInt then
              Result := MakeSSAConstInt(NarrowConstInt(Result.ConstInt, ConvW))
            else
            begin
              NarrowReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
              EmitInstruction(ssaNarrowInt, NarrowReg, Result, MakeSSAValue(svkNone), MakeSSAConstInt(ConvW));
              Result := NarrowReg;
            end;
          end;
        end
        else if (FuncName = 'CDBL') or (FuncName = 'CSNG') then
        begin
          // FreeBASIC float conversion functions: produce a float value. CSNG rounds to true
          // single precision (held in the Double bank) via bcNarrowSingle (B1.5).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
            ProcessExpression(ArgListNode.GetChild(0), ArgValue)
          else if (ArgListNode <> nil) and (ArgListNode.NodeType <> antArgumentList) then
            ProcessExpression(ArgListNode, ArgValue)
          else begin Result := MakeSSAValue(svkNone); Exit; end;

          if ArgValue.Kind = svkConstInt then
            Result := MakeSSAConstFloat(ArgValue.ConstInt)
          else if ArgValue.Kind = svkConstFloat then
            Result := MakeSSAConstFloat(ArgValue.ConstFloat)
          else if (ArgValue.Kind = svkConstString) or
                  ((ArgValue.Kind = svkRegister) and (ArgValue.RegType = srtString)) then
          begin
            // String operand: parse as a number (VAL semantics), e.g. CDBL("3.14").
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            EmitInstruction(ssaStrVal, Result, EnsureStringRegister(ArgValue), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            Result := EnsureFloatRegister(ArgValue);

          if FuncName = 'CSNG' then
          begin
            if Result.Kind = svkConstFloat then
              Result := MakeSSAConstFloat(Single(Result.ConstFloat))
            else
            begin
              NarrowReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
              EmitInstruction(ssaNarrowSingle, NarrowReg, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              Result := NarrowReg;
            end;
          end;
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
          else if FuncName = 'ACOS' then
            OpCode := ssaMathAcos
          else if FuncName = 'ASIN' then
            OpCode := ssaMathAsin
          else if FuncName = 'FIX' then
            OpCode := ssaMathFix
          else if FuncName = 'FRAC' then
            OpCode := ssaMathFrac
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
        else if FuncName = 'RGB' then
        begin
          // FreeBASIC RGB(r, g, b) = RGBA(r, g, b, 255): opaque colour. Same packed layout as RGBA
          // (alpha in bits 24-31), so it reuses ssaGraphicRGBA with a constant 255 alpha register.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 3) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);
            ProcessExpression(ArgListNode.GetChild(1), GVal);
            ProcessExpression(ArgListNode.GetChild(2), BVal);
            if (RVal.Kind = svkConstInt) and (GVal.Kind = svkConstInt) and (BVal.Kind = svkConstInt) then
              Result := MakeSSAConstInt((Int64($FF) shl 24) or
                          ((RVal.ConstInt and $FF) shl 16) or
                          ((GVal.ConstInt and $FF) shl 8) or
                          (BVal.ConstInt and $FF))
            else if (RVal.Kind = svkConstFloat) and (GVal.Kind = svkConstFloat) and (BVal.Kind = svkConstFloat) then
              Result := MakeSSAConstInt((Int64($FF) shl 24) or
                          ((Trunc(RVal.ConstFloat) and $FF) shl 16) or
                          ((Trunc(GVal.ConstFloat) and $FF) shl 8) or
                          (Trunc(BVal.ConstFloat) and $FF))
            else
            begin
              RReg := EnsureIntRegister(RVal);
              GReg := EnsureIntRegister(GVal);
              BReg := EnsureIntRegister(BVal);
              AReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
              EmitInstruction(ssaLoadConstInt, AReg, MakeSSAConstInt(255), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
              DestReg := FProgram.AllocRegister(srtInt);
              Result := MakeSSARegister(srtInt, DestReg);
              EmitInstruction(ssaGraphicRGBA, Result, RReg, GReg, BReg);
              FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1].AddPhiSource(AReg, nil);
            end;
          end
          else
            raise Exception.Create('RGB requires 3 arguments: RGB(r, g, b)');
        end
        else if FuncName = '__PALGET' then
        begin
          // Internal helper for PALETTE GET: __PALGET(index, which) -> palette component (0=r,1=g,2=b),
          // each 0-255. Reads via FGraphics.GetPaletteColor; the `which` selector is a constant literal.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            ProcessExpression(ArgListNode.GetChild(1), RVal);     // which (constant 0/1/2)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            // which goes in Src3 -> Immediate
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaGfxPalGet, Result, ArgReg, MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaGfxPalGet, Result, ArgReg, MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('__PALGET requires 2 arguments');
        end
        else if FuncName = 'IMAGECREATE' then
        begin
          // IMAGECREATE(w, h [, color]) -> image handle. Default fill is the transparent key (magenta,
          // RGB 255,0,255) so an untouched image blits as transparent under PUT TRANS.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);  // w
            ProcessExpression(ArgListNode.GetChild(1), RVal);     RReg := EnsureIntRegister(RVal);          // h
            if ArgListNode.ChildCount >= 3 then
            begin
              ProcessExpression(ArgListNode.GetChild(2), GVal); GReg := EnsureIntRegister(GVal);
            end
            else
            begin
              GReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
              EmitInstruction(ssaLoadConstInt, GReg, MakeSSAConstInt(Int64($FFFF00FF)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            end;
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaGfxImageCreate, Result, ArgReg, RReg, GReg);   // Src3=color -> Immediate
          end
          else
            raise Exception.Create('IMAGECREATE requires at least 2 arguments: IMAGECREATE(w, h [, color])');
        end
        else if FuncName = '__IMGINFO' then
        begin
          // Internal helper for IMAGEINFO: __IMGINFO(handle, which) -> width (0) / height (1).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            ProcessExpression(ArgListNode.GetChild(1), RVal);     // which (constant 0/1)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaGfxImageInfo, Result, ArgReg, MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaGfxImageInfo, Result, ArgReg, MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('__IMGINFO requires 2 arguments');
        end
        else if FuncName = '__SCRINFO' then
        begin
          // Internal helper for SCREENINFO: __SCRINFO(which) -> w(0)/h(1)/depth(2)/bpp(3)/pitch(4)/rate(5).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);     // which (constant)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaGfxScreenInfo, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaGfxScreenInfo, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('__SCRINFO requires 1 argument');
        end
        else if FuncName = 'PMAP' then
        begin
          // PMAP(coord, n) -> mapped coordinate (n: 0=lx->px,1=ly->py,2=px->lx,3=py->ly).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);  // coord
            ProcessExpression(ArgListNode.GetChild(1), RVal);     // n (constant)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaGfxPMap, Result, ArgReg, MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaGfxPMap, Result, ArgReg, MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('PMAP requires 2 arguments: PMAP(coord, n)');
        end
        else if FuncName = 'MULTIKEY' then
        begin
          // MULTIKEY(scancode) -> -1 if the key is held, 0 otherwise.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaMultikey, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('MULTIKEY requires 1 argument: MULTIKEY(scancode)');
        end
        else if FuncName = 'GETMOUSE' then
          // GETMOUSE(x, y [, wheel] [, buttons] [, clip]) -> status (0 ok / 1 no mouse). Writes each
          // provided lvalue by reference; see EmitGetmouse. Works as an expression or a bare statement.
          Result := EmitGetmouse(Node, ArgListNode)
        else if FuncName = '__MOUSEAXIS' then
        begin
          // Internal helper for GETMOUSE: __MOUSEAXIS(which) -> cached component (0=x,1=y,2=wheel,3=buttons,
          // 4=clip). `which` is a constant literal; it rides in the Immediate like __SCRINFO's selector.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);   // which (constant)
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaMouseAxis, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaMouseAxis, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('__MOUSEAXIS requires 1 argument');
        end
        else if FuncName = 'GETJOYSTICK' then
          // GETJOYSTICK(id, buttons, a1..a8) -> status (0 ok / 1 no device). Writes each provided lvalue
          // by reference (buttons int, a1..a8 single); see EmitGetJoystick. Expression or bare statement.
          Result := EmitGetJoystick(Node, ArgListNode)
        else if FuncName = '__JOYBTN' then
        begin
          // Internal helper for GETJOYSTICK: cached joystick button bitmask (int).
          DestReg := FProgram.AllocRegister(srtInt);
          Result := MakeSSARegister(srtInt, DestReg);
          EmitInstruction(ssaJoyBtn, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else if FuncName = '__JOYAXIS' then
        begin
          // Internal helper for GETJOYSTICK: cached joystick axis value (single). `which` (0..7) rides in
          // the Immediate like __SCRINFO/__MOUSEAXIS; the result is a FLOAT register.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), RVal);   // which (constant)
            DestReg := FProgram.AllocRegister(srtFloat);
            Result := MakeSSARegister(srtFloat, DestReg);
            if RVal.Kind = svkConstInt then
              EmitInstruction(ssaJoyAxis, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), RVal)
            else
              EmitInstruction(ssaJoyAxis, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), EnsureIntRegister(RVal));
          end
          else
            raise Exception.Create('__JOYAXIS requires 1 argument');
        end
        else if FuncName = 'STICK' then
        begin
          // STICK(axis) -> gaming-device axis position (1..200, or 0 if not attached). axis 0..3.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaStick, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('STICK requires 1 argument: STICK(axis)');
        end
        else if FuncName = 'STRIG' then
        begin
          // STRIG(button) -> gaming-device button state (-1 pressed / 0 not). button 0..7.
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            DestReg := FProgram.AllocRegister(srtInt);
            Result := MakeSSARegister(srtInt, DestReg);
            EmitInstruction(ssaStrig, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('STRIG requires 1 argument: STRIG(button)');
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
        else if FuncName = 'POINT' then
        begin
          // FreeBASIC POINT(x, y) -> pixel color (read from the graphics backend).
          if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 2) then
          begin
            ProcessExpression(ArgListNode.GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
            ProcessExpression(ArgListNode.GetChild(1), RVal);     RReg := EnsureIntRegister(RVal);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaGfxPoint, Result, ArgReg, RReg, MakeSSAValue(svkNone));
          end
          else
            raise Exception.Create('POINT requires 2 arguments: POINT(x, y)');
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
          end
          // Static member method: "TypeName.method(args)" with no instance (TypeName is a type, not a var).
          else if TryStaticMethodCall(MethodObjNode, VarToStr(Node.GetChild(0).Value),
                                      Node.GetChild(1), Result) then
            Exit;
        end;

        // First child is array name
        if Node.GetChild(0).NodeType <> antIdentifier then
        begin
          Result := MakeSSAValue(svkNone);
          Exit;
        end;

        ArrName := VarToStr(Node.GetChild(0).Value);

        // FreeBASIC raw heap: ALLOCATE(n)/CALLOCATE(n)/REALLOCATE(p,n) as an expression → raw pointer;
        // DEALLOCATE(p) as an expression-statement → free (yields a dummy 0). Parse as array-access (not
        // a declared array/function).
        if IsAllocCall(Node, ArrName2) then
        begin
          EmitRawAlloc(Node, Result);
          Exit;
        end;
        if (UpperCase(ArrName) = 'DEALLOCATE') and (FProgram.FindArray(ArrName) < 0) and
           (Node.GetChild(1).NodeType in [antArgumentList, antExpressionList]) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), Left);
          EmitInstruction(ssaRawFree, MakeSSAValue(svkNone), EnsureIntRegister(Left), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC raw-memory block ops on the byte heap (MODERN only): FB_MEMCOPY(dst,src,bytes),
        // FB_MEMMOVE(dst,src,bytes), CLEAR(dst,value,bytes), FB_MEMCOPYCLEAR(dst,dstlen,src,srclen).
        // dst/src are raw pointer values (from Allocate); v1 takes the pointer directly. Valid as a
        // statement, or (memcopy/memmove) as an expression yielding the destination pointer.
        if FModernMode and (FProgram.FindArray(ArrName) < 0) and
           (Node.GetChild(1).NodeType in [antArgumentList, antExpressionList]) and
           ((UpperCase(ArrName) = kFBMEMCOPY) or (UpperCase(ArrName) = kFBMEMMOVE) or
            (UpperCase(ArrName) = kCLEAR) or (UpperCase(ArrName) = kFBMEMCOPYCLEAR)) then
        begin
          EmitRawMemOp(UpperCase(ArrName), Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC SIZEOF(T) / LEN(T) — compile-time byte size of a type. The argument is a type name
        // (identifier); a pointer variable argument resolves to pointer size (8). Used for "Allocate(n *
        // SizeOf(Integer))". (Only the single-identifier form; "SizeOf(T PTR)" needs type-arg parsing.)
        if (UpperCase(ArrName) = 'SIZEOF') and (FProgram.FindArray(ArrName) < 0) and
           (Node.GetChild(1).NodeType in [antArgumentList, antExpressionList]) and (Node.GetChild(1).ChildCount = 1) and
           (Node.GetChild(1).GetChild(0).NodeType = antIdentifier) then
        begin
          ArrName2 := UpperCase(VarToStr(Node.GetChild(1).GetChild(0).Value));
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          if (FPointerVars.IndexOfName(ArrName2) >= 0) or IsRawPtr(ArrName2) then
            EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(8), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
          else
            EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(TypeSizeBytes(ArrName2)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // User-defined FUNCTION (M2): "name(args)" parses as array access, but if the name
        // is a declared FUNCTION it is a call. Stage args, call, read the result slot.
        if FProcedureNames.IndexOf(UpperCase(ArrName)) >= 0 then
        begin
          EmitUserFunctionCall(UpperCase(ArrName), Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC bare MID as the substring function (MODERN only): "MID(s, start [,len])" parses as
        // array access, but in FreeBASIC MID is the function (MID$ stays the v7 form; in CLASSIC MID is
        // a plain identifier/array). Only when MID is not actually a declared array.
        if FModernMode and (UpperCase(ArrName) = 'MID') and (FProgram.FindArray(ArrName) < 0) then
        begin
          EmitMidSubstring(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC STRING(n, ch) / v7-QB STRING$(n, ch): n copies of a character. STRING parses as
        // array access (STRING is a type name, not a registered keyword). STRING$ works in both
        // dialects; bare STRING is the FB form (MODERN only). Only when not actually a declared array.
        if (FProgram.FindArray(ArrName) < 0) and
           ((UpperCase(ArrName) = 'STRING$') or (FModernMode and (UpperCase(ArrName) = 'STRING'))) then
        begin
          EmitStringFill(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC WSTRING(n, cp) function: n copies of the wide (UTF-8) char for codepoint cp. WSTRING
        // also names the type; as a call (MODERN, not a declared array) it is the fill function.
        if FModernMode and (UpperCase(ArrName) = kWSTRING) and (FProgram.FindArray(ArrName) < 0) then
        begin
          EmitWStringFill(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC IIF(cond, a, b): short-circuit conditional expression. Parses as array access
        // (IIF is not a registered keyword); intercept in MODERN when it is not a declared array.
        if FModernMode and (UpperCase(ArrName) = 'IIF') and (FProgram.FindArray(ArrName) < 0) then
        begin
          EmitIif(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC WSTR(x): convert a number/string to a wide string (MODERN; parses as array access,
        // WSTR is not a registered keyword). Only when not a declared array.
        if FModernMode and (UpperCase(ArrName) = kWSTR) and (FProgram.FindArray(ArrName) < 0) then
        begin
          EmitWStr(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC SADD(s) / STRPTR(s): raw byte-heap pointer to a NUL-terminated copy of the string's
        // bytes (a read-only snapshot — the managed string has no stable mutable buffer address). STRPTR
        // returns a pointer to the string data, equivalent to SADD for var-length strings. MODERN, not an array.
        if FModernMode and ((UpperCase(ArrName) = kSADD) or (UpperCase(ArrName) = kSTRPTR)) and
           (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureStringRegister(ArgValue);
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaStrSAdd, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC bit/byte macros (LOBYTE/HIBYTE/LOWORD/HIWORD/BIT/BITSET/BITRESET) and CBOOL: pure
        // integer functions, parse as array access (not registered keywords). MODERN, not a declared array.
        if FModernMode and (FProgram.FindArray(ArrName) < 0) and
           ((UpperCase(ArrName) = kLOBYTE) or (UpperCase(ArrName) = kHIBYTE) or
            (UpperCase(ArrName) = kLOWORD) or (UpperCase(ArrName) = kHIWORD) or
            (UpperCase(ArrName) = kBIT) or (UpperCase(ArrName) = kBITSET) or
            (UpperCase(ArrName) = kBITRESET) or (UpperCase(ArrName) = kCBOOL)) then
        begin
          EmitBitMacro(UpperCase(ArrName), Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC ARRAYLEN(arr): total element count. Not a registered keyword; intercept in MODERN
        // when ARRAYLEN itself is not a declared array. (Its argument names the array to measure.)
        if FModernMode and (UpperCase(ArrName) = kARRAYLEN) and (FProgram.FindArray(ArrName) < 0) then
        begin
          EmitArrayLen(Node.GetChild(1), Result);
          Exit;
        end;

        // FreeBASIC FILEEXISTS(path): -1 if the file exists, else 0. MODERN, not a declared array.
        if FModernMode and (UpperCase(ArrName) = kFILEEXISTS) and (FProgram.FindArray(ArrName) < 0) and
           (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureStringRegister(ArgValue);
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaFileExists, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC bare string functions: CHR/STR/LEFT/RIGHT are the canonical FB names for the
        // $-suffixed functions. MODERN, parenthesised call, not a declared array.
        if FModernMode and (FProgram.FindArray(ArrName) < 0) then
        begin
          if UpperCase(ArrName) = kCHR then begin EmitBareStringFunc(kCHRS, Node, Result); Exit; end;
          if UpperCase(ArrName) = kSTR then begin EmitBareStringFunc(kSTRS, Node, Result); Exit; end;
          if UpperCase(ArrName) = kLEFT then begin EmitBareStringFunc(kLEFTS, Node, Result); Exit; end;
          if UpperCase(ArrName) = kRIGHT then begin EmitBareStringFunc(kRIGHTS, Node, Result); Exit; end;
        end;

        // FreeBASIC FILELEN(path): file size in bytes (0 if absent). MODERN, not a declared array.
        if FModernMode and (UpperCase(ArrName) = kFILELEN) and (FProgram.FindArray(ArrName) < 0) and
           (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureStringRegister(ArgValue);
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaFileLen, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC CURDIR$() / CURDIR() (parenthesised form): current working directory.
        if FModernMode and ((UpperCase(ArrName) = kCURDIRS) or (UpperCase(ArrName) = kCURDIR)) and
           (FProgram.FindArray(ArrName) < 0) then
        begin
          Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
          EmitInstruction(ssaCurDir, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC EXEPATH() (parenthesised form): directory of the running program.
        if FModernMode and (UpperCase(ArrName) = kEXEPATH) and (FProgram.FindArray(ArrName) < 0) then
        begin
          Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
          EmitInstruction(ssaExePath, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC ENVIRON$(name) / ENVIRON(name): value of an environment variable.
        if FModernMode and ((UpperCase(ArrName) = kENVIRONS) or (UpperCase(ArrName) = kENVIRON)) and
           (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureStringRegister(ArgValue);
          Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
          EmitInstruction(ssaEnviron, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC COMMAND$(index) / COMMAND(index): the index-th command-line argument (index<0 = whole
        // command line, 0 = executable name, n>=1 = n-th arg, '' if out of range). Empty parens = -1.
        if FModernMode and ((UpperCase(ArrName) = kCOMMAND) or (UpperCase(ArrName) = kCOMMANDS)) and
           (FProgram.FindArray(ArrName) < 0) then
        begin
          if Node.GetChild(1).ChildCount >= 1 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
          end
          else
          begin
            ArgReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaLoadConstInt, ArgReg, MakeSSAConstInt(-1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
          EmitInstruction(ssaCommand, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC FORMAT(num [, mask]) / FORMAT$(...): formatted number string. Numeric masks
        // (0 # , . % and scientific E+/E-/e+/e-, plus literals) are supported in v1; date/time masks
        // are deferred. The value goes in the Immediate float-register slot (like DATEADD's serial).
        if FModernMode and ((UpperCase(ArrName) = kFORMAT) or (UpperCase(ArrName) = kFORMATS)) and
           (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureFloatRegister(ArgValue);          // the number to format
          if Node.GetChild(1).ChildCount >= 2 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(1), MaskValue);
            MaskReg := EnsureStringRegister(MaskValue);
          end
          else
          begin
            MaskReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
            EmitInstruction(ssaLoadConstString, MaskReg, MakeSSAConstString(''), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
          Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
          EmitInstruction(ssaStrFormat, Result, MaskReg, MakeSSAValue(svkNone), ArgReg);  // Src3=value float -> Immediate
          Exit;
        end;

        // FreeBASIC POINT(x, y): read a pixel's colour from the graphics backend. Intercepted by name
        // (POINT is NOT a reserved keyword, so "Point" stays usable as a type/array name). Only when it
        // is not a declared array, UDT type, or user function.
        if FModernMode and (UpperCase(ArrName) = kPOINT) and
           (FProgram.FindArray(ArrName) < 0) and (FindUDT(UpperCase(ArrName)) < 0) and
           (FProcedureNames.IndexOf(UpperCase(ArrName)) < 0) and
           (Node.GetChild(1).NodeType in [antArgumentList, antExpressionList]) and
           (Node.GetChild(1).ChildCount >= 2) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue); ArgReg := EnsureIntRegister(ArgValue);
          ProcessExpression(Node.GetChild(1).GetChild(1), MaskValue); MaskReg := EnsureIntRegister(MaskValue);
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaGfxPoint, Result, ArgReg, MaskReg, MakeSSAValue(svkNone));
          Exit;
        end;

        // FreeBASIC VARPTR(v) / PROCPTR(p): the address of a variable / procedure. Both are exactly
        // "@arg" in our model — synthesize an antProcAddress for the argument and process it (the @
        // lowering handles a scalar / array element / UDT field|handle / SHARED global / SUB entry PC).
        // MODERN, not a declared array.
        if FModernMode and ((UpperCase(ArrName) = kVARPTR) or (UpperCase(ArrName) = kPROCPTR)) and
           (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ArgNode := Node.GetChild(1).GetChild(0);   // the variable / procedure operand
          if ArgNode.NodeType = antIdentifier then
            // @scalar / @sub: historical shape (Value = name, no child).
            AddrNode := TASTNode.CreateWithValue(antProcAddress, UpperCase(VarToStr(ArgNode.Value)), ArgNode.Token)
          else
          begin
            // @arr(i) / @obj.field: keep the operand subtree as child0.
            AddrNode := TASTNode.Create(antProcAddress, ArgNode.Token);
            AddrNode.AddChild(ArgNode.Clone);
          end;
          ProcessExpression(AddrNode, Result);
          AddrNode.Free;
          Exit;
        end;

        // FreeBASIC date/time functions taking arguments. Intercepted by name (not registered as
        // keywords) so common identifiers like YEAR/MONTH/DAY/SECOND/MINUTE/HOUR/WEEKDAY stay usable as
        // variables. MODERN, not a declared array. Date serial = Double (FPC TDateTime epoch 1899-12-30).
        if FModernMode and (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ArrNameU := UpperCase(ArrName);
          // YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY(serial) -> int. Immediate selects the field.
          SelImm := -1;
          if ArrNameU = kYEARFN then SelImm := 0
          else if ArrNameU = kMONTHFN then SelImm := 1
          else if ArrNameU = kDAYFN then SelImm := 2
          else if ArrNameU = kHOURFN then SelImm := 3
          else if ArrNameU = kMINUTEFN then SelImm := 4
          else if ArrNameU = kSECONDFN then SelImm := 5
          else if ArrNameU = kWEEKDAY then SelImm := 6;
          if SelImm >= 0 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureFloatRegister(ArgValue);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaDateDecode, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // DATESERIAL(y,m,d) / TIMESERIAL(h,m,s) -> float serial. 3 int args: Src1, Src2, Src3-as-reg.
          if ((ArrNameU = kDATESERIAL) or (ArrNameU = kTIMESERIAL)) and (Node.GetChild(1).ChildCount >= 3) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ProcessExpression(Node.GetChild(1).GetChild(1), Arg2Value);
            ProcessExpression(Node.GetChild(1).GetChild(2), Arg3Value);
            ArgReg := EnsureIntRegister(ArgValue);
            Arg2Reg := EnsureIntRegister(Arg2Value);
            Arg3Reg := EnsureIntRegister(Arg3Value);
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            if ArrNameU = kTIMESERIAL then
              EmitInstruction(ssaTimeSerial, Result, ArgReg, Arg2Reg, Arg3Reg)
            else
              EmitInstruction(ssaDateSerial, Result, ArgReg, Arg2Reg, Arg3Reg);
            Exit;
          end;
          // DATEVALUE(s) / TIMEVALUE(s) -> float serial parsed from a string. Immediate selects.
          if (ArrNameU = kDATEVALUE) or (ArrNameU = kTIMEVALUE) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureStringRegister(ArgValue);
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            if ArrNameU = kTIMEVALUE then SelImm := 1 else SelImm := 0;
            EmitInstruction(ssaDateValue, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // ISDATE(s) -> int bool.
          if ArrNameU = kISDATE then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureStringRegister(ArgValue);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaIsDate, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            Exit;
          end;
          // MONTHNAME(n) / WEEKDAYNAME(n) -> string. Immediate selects.
          if (ArrNameU = kMONTHNAME) or (ArrNameU = kWEEKDAYNAME) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
            Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
            if ArrNameU = kWEEKDAYNAME then SelImm := 1 else SelImm := 0;
            EmitInstruction(ssaDateName, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // DATEADD(interval$, number, serial) -> float serial. Src1=string, Src2=int n, Src3=float serial reg.
          if (ArrNameU = kDATEADD) and (Node.GetChild(1).ChildCount >= 3) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ProcessExpression(Node.GetChild(1).GetChild(1), Arg2Value);
            ProcessExpression(Node.GetChild(1).GetChild(2), Arg3Value);
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureIntRegister(Arg2Value);
            Arg3Reg := EnsureFloatRegister(Arg3Value);
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            EmitInstruction(ssaDateAdd, Result, ArgReg, Arg2Reg, Arg3Reg);
            Exit;
          end;
          // DATEDIFF(interval$, serial1, serial2) -> int. Src1=string, Src2=float s1, Src3=float s2 reg.
          if (ArrNameU = kDATEDIFF) and (Node.GetChild(1).ChildCount >= 3) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ProcessExpression(Node.GetChild(1).GetChild(1), Arg2Value);
            ProcessExpression(Node.GetChild(1).GetChild(2), Arg3Value);
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureFloatRegister(Arg2Value);
            Arg3Reg := EnsureFloatRegister(Arg3Value);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaDateDiff, Result, ArgReg, Arg2Reg, Arg3Reg);
            Exit;
          end;
          // DATEPART(interval$, serial) -> int. Src1=string, Src2=float serial.
          if (ArrNameU = kDATEPART) and (Node.GetChild(1).ChildCount >= 2) then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ProcessExpression(Node.GetChild(1).GetChild(1), Arg2Value);
            ArgReg := EnsureStringRegister(ArgValue);
            Arg2Reg := EnsureFloatRegister(Arg2Value);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaDatePart, Result, ArgReg, Arg2Reg, MakeSSAValue(svkNone));
            Exit;
          end;
        end;

        // FreeBASIC numeric serialization (B3): MKI$/MKL$/MKD$/MKS$/MKSHORT/MKLONGINT pack a number into
        // a fixed-width binary string; CVI/CVL/CVD/CVS/CVSHORT/CVLONGINT unpack one back. Intercepted by
        // name (not registered as keywords). MODERN, not a declared array. Byte width goes in the Immediate.
        // The MK* names accept both the bare and the '$' suffixed form. Widths are FB-faithful on x64.
        if FModernMode and (FProgram.FindArray(ArrName) < 0) and (Node.GetChild(1).ChildCount >= 1) then
        begin
          ArrNameU := UpperCase(ArrName);
          // MK*: integer -> binary string. Width 2/4/8 selects the encoding.
          SelImm := -1;
          if (ArrNameU = kMKI) or (ArrNameU = kMKIS) then SelImm := 8         // Integer = 8 bytes on x64
          else if (ArrNameU = kMKLONGINT) or (ArrNameU = kMKLONGINTS) then SelImm := 8
          else if (ArrNameU = kMKL) or (ArrNameU = kMKLS) then SelImm := 4
          else if (ArrNameU = kMKSHORT) or (ArrNameU = kMKSHORTS) then SelImm := 2;
          if SelImm > 0 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureIntRegister(ArgValue);
            Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
            EmitInstruction(ssaStrMkInt, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // MK*: float -> binary string (MKS single = 4 bytes, MKD double = 8 bytes).
          SelImm := -1;
          if (ArrNameU = kMKS) or (ArrNameU = kMKSS) then SelImm := 4
          else if (ArrNameU = kMKD) or (ArrNameU = kMKDS) then SelImm := 8;
          if SelImm > 0 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureFloatRegister(ArgValue);
            Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
            EmitInstruction(ssaStrMkFloat, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // CV*: binary string -> integer. Width 2/4/8 selects the (sign-extended) decoding.
          SelImm := -1;
          if ArrNameU = kCVI then SelImm := 8                                 // Integer = 8 bytes on x64
          else if ArrNameU = kCVLONGINT then SelImm := 8
          else if ArrNameU = kCVL then SelImm := 4
          else if ArrNameU = kCVSHORT then SelImm := 2;
          if SelImm > 0 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureStringRegister(ArgValue);
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaStrCvInt, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
          // CV*: binary string -> float (CVS single = 4 bytes, CVD double = 8 bytes).
          SelImm := -1;
          if ArrNameU = kCVS then SelImm := 4
          else if ArrNameU = kCVD then SelImm := 8;
          if SelImm > 0 then
          begin
            ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
            ArgReg := EnsureStringRegister(ArgValue);
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            EmitInstruction(ssaStrCvFloat, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
            Exit;
          end;
        end;

        // FreeBASIC EOF(#n)/LOF(#n)/LOC(#n): file query returning int. Parsed as array access (not
        // registered as keywords, so common names like `loc` stay usable as variables). MODERN, not a
        // declared array. Query code: EOF=0, LOF=2, LOC=3 (matches bcFileQuery).
        if FModernMode and (FProgram.FindArray(ArrName) < 0) and
           ((UpperCase(ArrName) = kEOF) or (UpperCase(ArrName) = kLOF) or
            (UpperCase(ArrName) = kLOC) or (UpperCase(ArrName) = kSEEK)) and
           (Node.GetChild(1).ChildCount >= 1) then
        begin
          ProcessExpression(Node.GetChild(1).GetChild(0), ArgValue);
          ArgReg := EnsureIntRegister(ArgValue);
          Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          if UpperCase(ArrName) = kLOF then ValCode := 2
          else if UpperCase(ArrName) = kLOC then ValCode := 3
          else if UpperCase(ArrName) = kSEEK then ValCode := 4
          else ValCode := 0;
          EmitInstruction(ssaFileQuery, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAConstInt(ValCode));
          Exit;
        end;

        // FreeBASIC RAW pointer indexing: "p[i]" where p is an Allocate'd raw pointer. The byte address
        // is p + i*SizeOf(pointee); load SizeOf(pointee) bytes from the raw heap with the pointee's type.
        if (FProgram.FindArray(ArrName) < 0) and IsRawPtr(ArrName) and
           (Node.GetChild(1).NodeType = antExpressionList) and (Node.GetChild(1).ChildCount = 1) then
        begin
          Left := EmitPointerIndexAddress(ArrName, Node.GetChild(1));   // raw-scaled (see helper)
          if PointeeBankOf(ArrName) = srtFloat then
          begin
            Result := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
            EmitInstruction(ssaRawLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAConstInt(RawTypeCodeOf(ArrName)));
          end
          else
          begin
            Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaRawLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAConstInt(RawTypeCodeOf(ArrName)));
          end;
          Exit;
        end;

        // FreeBASIC pointer indexing: "p[i]" (also "p(i)") where p is a declared pointer, not an array.
        // Lower to *(p + i): compute the address then load the pointee with p's declared bank.
        if (FProgram.FindArray(ArrName) < 0) and (FPointerVars.IndexOfName(UpperCase(ArrName)) >= 0) and
           (Node.GetChild(1).NodeType = antExpressionList) and (Node.GetChild(1).ChildCount = 1) then
        begin
          Left := EmitPointerIndexAddress(ArrName, Node.GetChild(1));
          FuncRetType := PointeeBankOf(ArrName);
          DestReg := FProgram.AllocRegister(FuncRetType);
          Result := MakeSSARegister(FuncRetType, DestReg);
          case FuncRetType of
            srtFloat:  EmitInstruction(ssaRefLoadFloat, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            srtString: EmitInstruction(ssaRefLoadString, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          else
            EmitInstruction(ssaRefLoadInt, Result, Left, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
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

        // FreeBASIC explicit lower bounds ("lb TO ub"): the dimension's allocated size is ub-lb+1 and the
        // heap is 0-based, so map each source index to a 0-based offset by subtracting its lower bound.
        for i := 0 to High(Indices) do
          if (i <= High(ArrInfo.LowerBounds)) and (ArrInfo.LowerBounds[i] <> 0) then
          begin
            TempReg := FProgram.AllocRegister(srtInt);
            TempVal := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(ArrInfo.LowerBounds[i]),
                            MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            TempReg := FProgram.AllocRegister(srtInt);
            AddResult := MakeSSARegister(srtInt, TempReg);
            EmitInstruction(ssaSubInt, AddResult, Indices[i], TempVal, MakeSSAValue(svkNone));
            Indices[i] := AddResult;
          end;

        // Row-major linear index (compile-time const strides, or runtime push/resolve for REDIM'd arrays).
        LinearIndex := EmitArrayLinearIndex(Indices, ArrInfo, ArrName);

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
  VarNode, ExprNode, SharedAssign: TASTNode;
  VarName, LhsRecType, RhsRecType, AllocFuncU: string;
  ExprValue, VarReg, SrcRecHandle: TSSAValue;
  CopyOp: TSSAOpCode;
  DerefBank: TSSARegisterType;   // FreeBASIC "*p = expr": bank of the pointee
  FixedCap: Integer;             // fixed-length string capacity (DIM s AS STRING/WSTRING * n)
  FixedCapReg, FixedTrunc: TSSAValue;
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

  // FreeBASIC RAW pointer-deref store: "*p = expr" / "*(p±n) = expr" where p is Allocate'd → store
  // SizeOf(T) bytes to the raw heap at the (arithmetic-scaled) byte address.
  if (VarNode.NodeType = antDeref) and (RawPtrExprName(VarNode.GetChild(0)) <> '') then
  begin
    AllocFuncU := RawPtrExprName(VarNode.GetChild(0));   // reuse local for the raw pointer name
    ProcessExpression(VarNode.GetChild(0), VarReg);
    VarReg := EnsureIntRegister(VarReg);
    ProcessExpression(ExprNode, ExprValue);
    if PointeeBankOf(AllocFuncU) = srtFloat then
    begin
      ExprValue := EnsureFloatRegister(ExprValue);
      EmitInstruction(ssaRawStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAConstInt(RawTypeCodeOf(AllocFuncU)));
    end
    else
    begin
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRawStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAConstInt(RawTypeCodeOf(AllocFuncU)));
    end;
    Exit;
  end;

  // FreeBASIC pointer-deref store: *p = expr. Store the value into the pointee cell at p's address
  // (= p's int value). The pointee bank comes from p's declared pointer type.
  if VarNode.NodeType = antDeref then
  begin
    ProcessExpression(VarNode.GetChild(0), VarReg);
    VarReg := EnsureIntRegister(VarReg);
    DerefBank := DerefOperandBank(VarNode.GetChild(0));
    ProcessExpression(ExprNode, ExprValue);
    case DerefBank of
      srtFloat:
        begin
          ExprValue := EnsureFloatRegister(ExprValue);
          EmitInstruction(ssaRefStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
      srtString:
        begin
          ExprValue := EnsureStringRegister(ExprValue);
          EmitInstruction(ssaRefStoreString, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRefStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
    end;
    Exit;
  end;

  // FreeBASIC BYREF function result as an lvalue: "f(args) = expr". The call returns the address of
  // the referenced variable; store expr through it (parsed like an array access / call target).
  if (VarNode.NodeType = antArrayAccess) and (VarNode.ChildCount >= 1) and
     (VarNode.GetChild(0).NodeType = antIdentifier) and
     IsByrefRetFunc(VarToStr(VarNode.GetChild(0).Value)) then
  begin
    VarName := VarToStr(VarNode.GetChild(0).Value);
    if VarNode.ChildCount >= 2 then
      EmitProcedureCall(VarName, VarNode.GetChild(1))
    else
      EmitProcedureCall(VarName, nil);
    VarReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitXferLoad(srtInt, XFER_RESULT_SLOT, VarReg);   // VarReg = returned address
    DerefBank := ByrefRetPointeeBank(VarName);
    ProcessExpression(ExprNode, ExprValue);
    case DerefBank of
      srtFloat:
        begin
          ExprValue := EnsureFloatRegister(ExprValue);
          EmitInstruction(ssaRefStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
      srtString:
        begin
          ExprValue := EnsureStringRegister(ExprValue);
          EmitInstruction(ssaRefStoreString, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRefStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
    end;
    Exit;
  end;

  // FreeBASIC RAW pointer indexing as an lvalue: "p[i] = expr" where p is Allocate'd. Store SizeOf(T)
  // bytes at p + i*SizeOf(T) into the raw heap.
  if (VarNode.NodeType = antArrayAccess) and (VarNode.ChildCount >= 2) and
     (VarNode.GetChild(0).NodeType = antIdentifier) and
     (FProgram.FindArray(VarToStr(VarNode.GetChild(0).Value)) < 0) and
     IsRawPtr(VarToStr(VarNode.GetChild(0).Value)) and
     (VarNode.GetChild(1).NodeType = antExpressionList) and (VarNode.GetChild(1).ChildCount = 1) then
  begin
    VarName := VarToStr(VarNode.GetChild(0).Value);
    VarReg := EmitPointerIndexAddress(VarName, VarNode.GetChild(1));
    ProcessExpression(ExprNode, ExprValue);
    if PointeeBankOf(VarName) = srtFloat then
    begin
      ExprValue := EnsureFloatRegister(ExprValue);
      EmitInstruction(ssaRawStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAConstInt(RawTypeCodeOf(VarName)));
    end
    else
    begin
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRawStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAConstInt(RawTypeCodeOf(VarName)));
    end;
    Exit;
  end;

  // FreeBASIC pointer indexing as an lvalue: "p[i] = expr" (also "p(i) = expr") ≡ *(p + i) = expr.
  if (VarNode.NodeType = antArrayAccess) and (VarNode.ChildCount >= 2) and
     (VarNode.GetChild(0).NodeType = antIdentifier) and
     (FProgram.FindArray(VarToStr(VarNode.GetChild(0).Value)) < 0) and
     (FPointerVars.IndexOfName(UpperCase(VarToStr(VarNode.GetChild(0).Value))) >= 0) and
     (VarNode.GetChild(1).NodeType = antExpressionList) and (VarNode.GetChild(1).ChildCount = 1) then
  begin
    VarName := VarToStr(VarNode.GetChild(0).Value);
    VarReg := EmitPointerIndexAddress(VarName, VarNode.GetChild(1));
    DerefBank := PointeeBankOf(VarName);
    ProcessExpression(ExprNode, ExprValue);
    case DerefBank of
      srtFloat:
        begin
          ExprValue := EnsureFloatRegister(ExprValue);
          EmitInstruction(ssaRefStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
      srtString:
        begin
          ExprValue := EnsureStringRegister(ExprValue);
          EmitInstruction(ssaRefStoreString, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
        end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRefStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
    end;
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

  // BYREF reference variable (DIM BYREF r AS T = target): assigning to r stores through the address it
  // carries (mutating target), not into r's own register — so "r = v" writes target.
  if IsRefVar(VarName) then
  begin
    ProcessExpression(ExprNode, ExprValue);
    VarReg := EnsureIntRegister(GetOrAllocateVariable(VarName));   // the carried address
    case RefVarBank(VarName) of
      srtFloat:  begin ExprValue := EnsureFloatRegister(ExprValue);  EmitInstruction(ssaRefStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone)); end;
      srtString: begin ExprValue := EnsureStringRegister(ExprValue); EmitInstruction(ssaRefStoreString, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone)); end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRefStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
    end;
    Exit;
  end;

  // FreeBASIC raw heap: "p = Allocate(n)" / "CAllocate(n)" / "Reallocate(q,n)" — allocate/resize a byte
  // block and store the raw pointer (a RAWPTR_TAG byte offset) into p.
  if IsAllocCall(ExprNode, AllocFuncU) then
  begin
    EmitRawAlloc(ExprNode, ExprValue);
    EmitInstruction(ssaCopyInt, GetOrAllocateVariable(VarName), ExprValue, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  // Fixed-length string (DIM s AS STRING/WSTRING * n): truncate the assigned value to the capacity
  // (codepoints for a WSTRING, bytes otherwise). Plain string scalars only — SHARED/@-taken fall through.
  FixedCap := StrToIntDef(FFixedLenVars.Values[UpperCase(VarName)], 0);
  if (FixedCap > 0) and not IsSharedScalar(VarName) and not IsAddrLocal(VarName) then
  begin
    ProcessExpression(ExprNode, ExprValue);
    ExprValue := EnsureStringRegister(ExprValue);
    FixedCapReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, FixedCapReg, MakeSSAConstInt(FixedCap), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    FixedTrunc := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    if IsWStringVar(VarName) then
      EmitInstruction(ssaStrLeftW, FixedTrunc, ExprValue, FixedCapReg, MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaStrLeft, FixedTrunc, ExprValue, FixedCapReg, MakeSSAValue(svkNone));
    EmitInstruction(ssaCopyString, GetOrAllocateVariable(VarName), FixedTrunc, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  // Refinement #2: a builtin SHARED scalar is backed by a 1-element global array — store to element 0 (a
  // live cross-thread write), reusing the array-store lowering. A SHARED UDT scalar is excluded here: its
  // handle never changes (value semantics), so "p = q" falls through to the memberwise record copy below.
  if IsSharedScalar(VarName) and (VarRecordTypeName(VarName) = '') then
  begin
    SharedAssign := TASTNode.Create(antAssignment, VarNode.Token);
    SharedAssign.AddChild(MakeSharedScalarAccess(VarName, VarNode.Token));
    SharedAssign.AddChild(ExprNode.Clone);
    ProcessArrayStore(SharedAssign);
    SharedAssign.Free;
    Exit;
  end;

  // @-taken local: store into its per-frame backing record (field slot 0) through the hidden handle.
  if IsAddrLocal(VarName) then
  begin
    ProcessExpression(ExprNode, ExprValue);
    case AddrLocalBank(VarName) of
      srtFloat:  begin ExprValue := EnsureFloatRegister(ExprValue);  EmitInstruction(ssaRecordStoreFloat, MakeSSAValue(svkNone), AddrLocalHandle(VarName), ExprValue, MakeSSAConstInt(0)); end;
      srtString: begin ExprValue := EnsureStringRegister(ExprValue); EmitInstruction(ssaRecordStoreString, MakeSSAValue(svkNone), AddrLocalHandle(VarName), ExprValue, MakeSSAConstInt(0)); end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRecordStoreInt, MakeSSAValue(svkNone), AddrLocalHandle(VarName), ExprValue, MakeSSAConstInt(0));
    end;
    Exit;
  end;

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
    // FreeBASIC BYREF result: return the ADDRESS of the named (address-backed) variable, not its
    // value, so the caller can read or write through it. The returned variable must be address-backed
    // (a SHARED/global scalar or one whose address is taken); a local would dangle (v1: not enforced).
    if FCurrentProcByrefRet and (ExprNode.NodeType = antIdentifier) then
    begin
      // Returning an address-carrying BYREF param yields the address it already holds (a reference into
      // the caller's variable, the min(a,b)=0 idiom); any other named var returns its backing address.
      if IsAddrParam(VarToStr(ExprNode.Value)) then
        EmitXferStore(srtInt, XFER_RESULT_SLOT, EnsureIntRegister(GetOrAllocateVariable(VarToStr(ExprNode.Value))))
      else
        EmitXferStore(srtInt, XFER_RESULT_SLOT, EmitVarAddress(VarToStr(ExprNode.Value)));
      Exit;
    end;
    ProcessExpression(ExprNode, ExprValue);
    // V3: a UDT result is copied (by value) into the caller-allocated result instance; a scalar
    // result is staged into the result transfer slot as before.
    if FCurrentProcRetRecType <> '' then
      EmitRecordCopy(FCurrentResultHandle, ExprValue, FindUDT(FCurrentProcRetRecType))
    else
      EmitXferStore(FCurrentProcRetType, XFER_RESULT_SLOT, ExprValue);
    Exit;
  end;

  // BYREF-return address param: assigning to the parameter stores through the address it carries
  // (mutating the caller's variable), not into the local register.
  if IsAddrParam(VarName) then
  begin
    ProcessExpression(ExprNode, ExprValue);
    VarReg := EnsureIntRegister(GetOrAllocateVariable(VarName));   // the carried address
    case AddrParamBank(VarName) of
      srtFloat:  begin ExprValue := EnsureFloatRegister(ExprValue);  EmitInstruction(ssaRefStoreFloat, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone)); end;
      srtString: begin ExprValue := EnsureStringRegister(ExprValue); EmitInstruction(ssaRefStoreString, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone)); end;
    else
      ExprValue := EnsureIntRegister(ExprValue);
      EmitInstruction(ssaRefStoreInt, MakeSSAValue(svkNone), VarReg, ExprValue, MakeSSAValue(svkNone));
    end;
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

  // B1.5 phase 2: if VarName was DIM'd with a sub-64-bit integer type or SINGLE, wrap/round the
  // value to that width before storing (FreeBASIC narrows at the store). No-op for wide/untracked
  // vars. Emitting into a fresh register forces the store below to copy the narrowed value.
  ExprValue := ApplyScalarNarrow(VarName, ExprValue);

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
    else if (ExprValue.Kind = svkConstFloat) and (VarReg.RegType = srtInt) then
    begin
      // Convert float constant to int at compile time (truncate toward zero, matching the runtime
      // ssaFloatToInt path). Without this a float const was loaded straight into an int register
      // (e.g. DIM x AS INTEGER : x = 3.9 stored garbage / 0).
      ExprValue := MakeSSAConstInt(Trunc(ExprValue.ConstFloat));
      EmitInstruction(ssaLoadConstInt, VarReg, ExprValue,
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
  PKidx: Integer;   // B1.5 phase C: print-kind index for a BOOLEAN / unsigned-64 identifier
begin
  // FreeBASIC console WRITE: quoted-CSV to the screen (child 0 is a placeholder; values are children 1+).
  if Node.Attributes.Values['WRITECSV'] = '1' then
  begin
    EmitWriteFileValues(Node, MakeSSAValue(svkNone), True);
    Exit;
  end;

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

    // B1.5 phase C: printing a BOOLEAN variable yields "true"/"false"; a 64-bit unsigned variable
    // (UInteger/ULongInt) prints unsigned. Only the direct `PRINT var` form is special-cased.
    if (Child.NodeType = antIdentifier) and (ExprValue.RegType = srtInt) then
    begin
      PKidx := FVarPrintKind.IndexOf(UpperCase(VarToStr(Child.Value)));
      if PKidx >= 0 then
      begin
        if PtrInt(FVarPrintKind.Objects[PKidx]) = 1 then
          EmitInstruction(ssaPrintBool, MakeSSAValue(svkNone), ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone))
        else
          EmitInstruction(ssaPrintUInt, MakeSSAValue(svkNone), ExprValue,
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        Continue;
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

  // FreeBASIC explicit lower bounds ("lb TO ub"): map each source index to a 0-based offset by
  // subtracting its dimension's lower bound (the heap is 0-based, size is ub-lb+1).
  for i := 0 to High(Indices) do
    if (i <= High(ArrInfo.LowerBounds)) and (ArrInfo.LowerBounds[i] <> 0) then
    begin
      TempReg := FProgram.AllocRegister(srtInt);
      TempVal := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(ArrInfo.LowerBounds[i]),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      TempReg := FProgram.AllocRegister(srtInt);
      AddResult := MakeSSARegister(srtInt, TempReg);
      EmitInstruction(ssaSubInt, AddResult, Indices[i], TempVal, MakeSSAValue(svkNone));
      Indices[i] := AddResult;
    end;

  // Row-major linear index (compile-time const strides, or runtime push/resolve for REDIM'd arrays).
  LinearIndex := EmitArrayLinearIndex(Indices, ArrInfo, ArrName);

  // Evaluate value expression
  ProcessExpression(ExprNode, ExprValue);

  // B1.5: a narrow element type (DIM a(n) AS BYTE/SHORT/.../SINGLE) wraps/rounds the value on store.
  j := FArrayElemWidth.IndexOf(UpperCase(ArrName));
  if j >= 0 then
    ExprValue := ApplyNarrowCode(PtrInt(FArrayElemWidth.Objects[j]), ExprValue);

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
  DimsNode, DimExpr, DimChild, ArrayDeclNode: TASTNode;
  Dimensions: array of Integer;
  LowerBounds: array of Integer;  // FB "lb TO ub": constant lower bound per dimension (0 = default)
  HasLowerBounds: Boolean;
  LbVal: TSSAValue;
  DimCount, i, j, ArrayIdx, WIdx: Integer;
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
  ArrElemTypeName: string; // declared "AS <type>" element type for an array (empty if none)
  RecPacked: Int64;        // M3.1: packed slot counts for bcRecordNewArray
  InitAssign: TASTNode;    // M4.4e: synthesized assignment for a "DIM v AS T = expr" initializer
  BlkIdx: Integer;         // M8/FB: innermost open block scope (-1 if none) for block-scoped UDT dtors
  MDtorSlotIdx: Integer;   // V5e: index into FModuleDtorSlots for a module global's handle slot (-1 if none)
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

    // (VAR x = expr is rewritten to a typed-scalar DIM by RegisterRecordVars, so it arrives here as an
    // ordinary "DIM x AS T = expr" — no VAR-specific handling needed.)

    // DIM BYREF r AS T = target (FreeBASIC reference variable): bind r to target's address. child[2] is
    // "@target" (an antProcAddress); evaluate it to the packed address and store it into r's int
    // register. r is already registered in FRefVars (RegisterRecordVars), so subsequent reads/writes of
    // r auto-dereference through this address.
    if (ArrayDeclNode.Attributes.Values['BYREF'] = '1') and IsRefVar(ArrName) and
       (ArrayDeclNode.ChildCount >= 3) then
    begin
      ProcessExpression(ArrayDeclNode.GetChild(2), RecHandleVal);   // @target -> packed address (int)
      EmitInstruction(ssaCopyInt, GetOrAllocateVariable(UpperCase(ArrName)), EnsureIntRegister(RecHandleVal),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Continue;
    end;

    // Typed scalar declaration (M3): "DIM name AS typename" — child[1] is an antIdentifier
    // (the type), not antDimensions. A UDT type allocates a record instance and stores its
    // handle in the variable; a builtin type needs no allocation (type already recorded).
    if DimsNode.NodeType = antIdentifier then
    begin
      RecTypeName := UpperCase(VarToStr(DimsNode.Value));
      // @-taken LOCAL scalar: back it with a per-frame 1-field record (reclaimed at frame exit, distinct
      // per recursion). Allocate the record, store its handle in the hidden "<name>$REC", apply any
      // "= expr" through it; reads/writes/@ of the name route through the handle (recursion-safe @local).
      if (ArrayDeclNode.Attributes.Values['ADDRLOCAL'] = '1') and FInProcedure then
      begin
        // Register this local for the current procedure (per-proc set, cleared at the prologue) so its
        // reads/writes/@ route through the record. Type from the AS-type child.
        if FAddrLocalVars.IndexOfName(UpperCase(ArrName)) < 0 then
          FAddrLocalVars.Add(UpperCase(ArrName) + '=' + RecTypeName);
        RecHandleVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        case AddrLocalBank(UpperCase(ArrName)) of
          srtFloat:  EmitInstruction(ssaRecordNew, RecHandleVal, MakeSSAConstInt(0), MakeSSAConstInt(1), MakeSSAConstInt(0));
          srtString: EmitInstruction(ssaRecordNew, RecHandleVal, MakeSSAConstInt(0), MakeSSAConstInt(0), MakeSSAConstInt(1));
        else
          EmitInstruction(ssaRecordNew, RecHandleVal, MakeSSAConstInt(1), MakeSSAConstInt(0), MakeSSAConstInt(0));
        end;
        EmitInstruction(ssaCopyInt, AddrLocalHandle(UpperCase(ArrName)), RecHandleVal,
                        MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        // "DIM v AS T = expr": store the initializer through the record (slot 0), reusing ProcessAssignment.
        if (ArrayDeclNode.ChildCount >= 3) and (ArrayDeclNode.GetChild(2).NodeType <> antArgumentList) then
        begin
          InitAssign := TASTNode.Create(antAssignment, ArrayDeclNode.GetChild(0).Token);
          InitAssign.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(ArrName), ArrayDeclNode.GetChild(0).Token));
          InitAssign.AddChild(ArrayDeclNode.GetChild(2).Clone);
          ProcessAssignment(InitAssign);
          InitAssign.Free;
        end;
        Continue;
      end;
      // Refinement #2: a SHARED scalar is backed by a 1-element global array (registered in
      // CollectSharedVars). Emit its array allocation; then for a UDT scalar allocate the record in the
      // shared region and store its handle into element 0; for a builtin scalar apply any "= expr".
      if (ArrayDeclNode.Attributes.Values['SHARED'] = '1') and IsSharedScalar(UpperCase(ArrName)) then
      begin
        ArrayIdx := PtrInt(FSharedScalarArr.Objects[FSharedScalarArr.IndexOf(UpperCase(ArrName))]);
        ArrayRef := MakeSSAArrayRef(ArrayIdx, FProgram.GetArray(ArrayIdx).ElementType);
        EmitInstruction(ssaArrayDim, MakeSSAValue(svkNone), ArrayRef,
                        MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        RecUDTIdx := FindUDT(RecTypeName);
        if RecUDTIdx >= 0 then
        begin
          // Shared UDT scalar: allocate the record in the SHARED region (immediate bit 48), construct it,
          // and store its handle into element 0 — so any thread reaches the same instance. (Nested-UDT
          // field instances are still per-thread; flat UDTs are fully cross-thread.)
          RecHandleVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
          EmitInstruction(ssaRecordNew, RecHandleVal,
                          MakeSSAConstInt(FUDTs[RecUDTIdx].NInt),
                          MakeSSAConstInt(FUDTs[RecUDTIdx].NFloat),
                          MakeSSAConstInt(FUDTs[RecUDTIdx].NStr or (Int64(RecUDTIdx) shl 32) or (Int64(1) shl 48)));
          EmitRecordInit(RecHandleVal, RecUDTIdx);
          if (ArrayDeclNode.ChildCount >= 3) and (ArrayDeclNode.GetChild(2).NodeType = antArgumentList) then
            EmitConstructorCall(RecHandleVal, RecTypeName, ArrayDeclNode.GetChild(2))
          else
            EmitConstructorCall(RecHandleVal, RecTypeName);
          EmitSharedScalarStoreVal(UpperCase(ArrName), RecHandleVal);  // publish the handle into name(0)
          // V5e: a module-global UDT with a destructor stashes its handle in a reserved transfer slot so
          // an END inside any SUB can still destroy it (the same as a non-shared global).
          if not FInProcedure then
          begin
            MDtorSlotIdx := FModuleDtorSlots.IndexOf(UpperCase(ArrName));
            if MDtorSlotIdx >= 0 then
              EmitXferStore(srtInt, PtrInt(FModuleDtorSlots.Objects[MDtorSlotIdx]), RecHandleVal);
          end;
        end
        else if (ArrayDeclNode.ChildCount >= 3) and (ArrayDeclNode.GetChild(2).NodeType <> antArgumentList) then
        begin
          InitAssign := TASTNode.Create(antAssignment, ArrayDeclNode.GetChild(0).Token);
          InitAssign.AddChild(MakeSharedScalarAccess(UpperCase(ArrName), ArrayDeclNode.GetChild(0).Token));
          InitAssign.AddChild(ArrayDeclNode.GetChild(2).Clone);
          ProcessArrayStore(InitAssign);
          InitAssign.Free;
        end;
        Continue;
      end;
      RecUDTIdx := FindUDT(RecTypeName);
      if RecUDTIdx >= 0 then
      begin
        // DeclareVariableTyped (explicit, UDT handle = int bank): in MODERN a DIM inside a block binds
        // the name block-locally (shadowing any outer same-name binding); at module level / CLASSIC it
        // is allocate-or-reuse. Forcing the int bank keeps the binding correct even if the same name is
        // DIM'd with a different type in another scope (the global type table is first-declaration-wins).
        RecHandleVal := DeclareVariableTyped(UpperCase(ArrName), srtInt);
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
        // M8: a UDT DIM'd inside a block (loop body, or — MODERN — an IF branch / SCOPE block) is
        // block-scoped — register it for destruction + reclamation at the block's exit (BlockScopeExit),
        // and exclude it from the frame/module destructors (which would otherwise also destroy it, on a
        // stale/reused handle).
        BlkIdx := InnermostBlockFrameIdx;
        if BlkIdx >= 0 then
        begin
          FScopeStack[BlkIdx].Dtors.Add(UpperCase(ArrName) + '|' + RecTypeName);
          if FBlockHandledVars.IndexOf(UpperCase(ArrName)) < 0 then
            FBlockHandledVars.Add(UpperCase(ArrName));
        end
        // V5e: a true module global (not in a procedure frame, not block-scoped) with a destructor gets
        // its handle stashed in a reserved frame-independent slot, so an END inside any SUB can still
        // destroy it. The handle is stable for the program's life (value semantics never reassign it).
        else if not FInProcedure then
        begin
          MDtorSlotIdx := FModuleDtorSlots.IndexOf(UpperCase(ArrName));
          if MDtorSlotIdx >= 0 then
            EmitXferStore(srtInt, PtrInt(FModuleDtorSlots.Objects[MDtorSlotIdx]), RecHandleVal);
        end;
      end
      else
        // Builtin typed scalar (DIM x AS INTEGER/SINGLE/STRING): bind the name now so a DIM inside a
        // block is block-local in MODERN (and resolves before any later use). Legacy reuse otherwise.
        // Bind in the DECLARED bank, so the same name DIM'd with different types in separate scopes does
        // not collide on the global first-declaration-wins type table (GetVariableType).
        begin
          DeclareVariableTyped(UpperCase(ArrName), TypeNameToBank(RecTypeName, UpperCase(ArrName)));
          RecordVarWidth(UpperCase(ArrName), RecTypeName);  // B1.5 phase 2: narrow on store to a sub-64-bit type
        end;
      // M4.4e: general initializer "DIM v AS T = expr" — child[2] is the init expression (not the
      // ctor-args antArgumentList). After allocation/construction, assign it: a scalar store, or a
      // value-copy when both sides are UDTs (reuses ProcessAssignment's existing semantics). The
      // synthesized node is transient and freed here; its cloned expression is owned by it.
      if (ArrayDeclNode.ChildCount >= 3) and
         (ArrayDeclNode.GetChild(2).NodeType <> antArgumentList) then
      begin
        InitAssign := TASTNode.Create(antAssignment, ArrayDeclNode.GetChild(0).Token);
        InitAssign.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(ArrName),
                                                     ArrayDeclNode.GetChild(0).Token));
        InitAssign.AddChild(ArrayDeclNode.GetChild(2).Clone);
        ProcessAssignment(InitAssign);
        InitAssign.Free;
      end;
      Continue;
    end;

    // Element type of "DIM name(dims) [AS type]". child[2] (if present) is the AS-type identifier.
    //   - AS <udt>      → an array of record handles (element type int); element UDT tracked below.
    //   - AS <builtin>  → use the DECLARED type (STRING/INTEGER/DOUBLE/...), NOT the name suffix —
    //                     otherwise "DIM a(n) AS STRING" (no $ suffix) was mis-typed as numeric.
    //   - no AS-type    → infer from the array name suffix ("DIM a$(n)", "DIM a%(n)", ...).
    RecArrUDTIdx := -1;
    ArrElemTypeName := '';
    if (ArrayDeclNode.ChildCount >= 3) and (ArrayDeclNode.GetChild(2).NodeType = antIdentifier) then
    begin
      ArrElemTypeName := UpperCase(VarToStr(ArrayDeclNode.GetChild(2).Value));
      RecArrUDTIdx := FindUDT(ArrElemTypeName);
    end;
    if RecArrUDTIdx >= 0 then
      ElementType := srtInt
    else if ArrElemTypeName <> '' then
      ElementType := TypeNameToBank(ArrElemTypeName, ArrName)
    else
      ElementType := GetVariableType(ArrName);

    // B1.5: remember a narrow element width (DIM a(n) AS BYTE/.../SINGLE) so element stores wrap to it.
    if (RecArrUDTIdx < 0) and (ArrElemTypeName <> '') then
    begin
      WIdx := FArrayElemWidth.IndexOf(UpperCase(ArrName));
      if TypeNameWidthCode(ArrElemTypeName) <> 0 then
      begin
        if WIdx >= 0 then
          FArrayElemWidth.Objects[WIdx] := TObject(PtrInt(TypeNameWidthCode(ArrElemTypeName)))
        else
          FArrayElemWidth.AddObject(UpperCase(ArrName), TObject(PtrInt(TypeNameWidthCode(ArrElemTypeName))));
      end
      else if WIdx >= 0 then
        FArrayElemWidth.Delete(WIdx);  // re-DIM to a wide element type clears prior narrowing
    end;

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
    SetLength(LowerBounds, DimCount);
    HasLowerBounds := False;
    TotalElements := 1;

    for i := 0 to DimCount - 1 do
    begin
      // A dimension is either a bare upper-bound expression (lower bound 0) or a FreeBASIC explicit
      // bound antDimRange(lb, ub). The lower bound must be a compile-time constant; the upper bound may
      // be constant or a variable (runtime-sized). Allocated count for the dimension = ub - lb + 1.
      DimChild := DimsNode.GetChild(i);
      LowerBounds[i] := 0;
      if DimChild.NodeType = antDimRange then
      begin
        ProcessExpression(DimChild.GetChild(0), LbVal);
        if LbVal.Kind = svkConstInt then
          LowerBounds[i] := Integer(LbVal.ConstInt)
        else if LbVal.Kind = svkConstFloat then
          LowerBounds[i] := Integer(Trunc(LbVal.ConstFloat))
        else
          raise Exception.CreateFmt('Array lower bound must be a constant: %s', [ArrName]);
        if LowerBounds[i] <> 0 then HasLowerBounds := True;
        DimExpr := DimChild.GetChild(1);
      end
      else
        DimExpr := DimChild;
      ProcessExpression(DimExpr, DimValue);

      // Store dimension value for VM (needed for variable dimensions)
      DimValues[i] := DimValue;

      // BASIC semantics: DIM A(N) allocates N+1 elements [0..N]; FB "lb TO ub" allocates ub-lb+1 [lb..ub].
      // Dimensions can be constants or variables - evaluated once at DIM execution.
      if DimValue.Kind = svkConstInt then
      begin
        Dimensions[i] := Integer(DimValue.ConstInt) - LowerBounds[i] + 1;

        // Safety check for constant dimensions
        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array upper bound must be >= lower bound: %s[%d]', [ArrName, i]);

        // Calculate total elements (with overflow check)
        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkConstFloat then
      begin
        Dimensions[i] := Integer(Trunc(DimValue.ConstFloat)) - LowerBounds[i] + 1;

        if Dimensions[i] <= 0 then
          raise Exception.CreateFmt('Array upper bound must be >= lower bound: %s[%d]', [ArrName, i]);

        TotalElements := TotalElements * Dimensions[i];
        if TotalElements > MAX_ARRAY_ELEMENTS then
          raise Exception.CreateFmt('Array %s too large: %d elements (max %d)', [ArrName, TotalElements, MAX_ARRAY_ELEMENTS]);
      end
      else if DimValue.Kind = svkRegister then
      begin
        // Variable upper bound: DIM A(S%) or DIM A(lb TO S%) - VM computes size = ub - lb + 1 at runtime.
        // Use 0 as placeholder - actual size determined at runtime.
        Dimensions[i] := 0;
        // Cannot validate size at compile time
        TotalElements := -1;
      end
      else
        raise Exception.CreateFmt('Invalid array dimension expression: %s', [ArrName]);
    end;

    // Declare array in SSA program
    ArrayIdx := FProgram.DeclareArray(ArrName, ElementType, Dimensions);
    if HasLowerBounds then
      FProgram.SetArrayLowerBounds(ArrayIdx, LowerBounds);

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

procedure TSSAGenerator.ProcessErase(Node: TASTNode);
// ERASE arr [, arr ...] (B1.4): reset each named array's elements to default.
var
  j, ArrayIdx: Integer;
  ArrName: string;
  Child: TASTNode;
begin
  for j := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(j);
    if Child.NodeType <> antIdentifier then Continue;
    ArrName := UpperCase(VarToStr(Child.Value));
    ArrayIdx := FProgram.FindArray(ArrName);
    if ArrayIdx < 0 then
      raise Exception.CreateFmt('ERASE: array not declared: %s', [ArrName]);
    EmitInstruction(ssaArrayErase, MakeSSAValue(svkNone),
                    MakeSSAArrayRef(ArrayIdx, FProgram.GetArray(ArrayIdx).ElementType),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.ProcessRedim(Node: TASTNode);
// REDIM [PRESERVE] arr(ub [, ub ...]) [, ...] (B1.4): re-dimension an existing array at runtime.
// The array must already be declared (DIM first); each dimension's original lower bound is kept (only
// the upper bounds / size change). PRESERVE keeps the flat element order up to the new size.
var
  j, di, ArrayIdx: Integer;
  ArrName: string;
  ArrayDeclNode, DimsNode, DimChild, DimExpr: TASTNode;
  UbValue, UbReg: TSSAValue;
  PreserveFlag: Int64;
begin
  if Node.Attributes.Values['PRESERVE'] = '1' then PreserveFlag := 1 else PreserveFlag := 0;
  for j := 0 to Node.ChildCount - 1 do
  begin
    ArrayDeclNode := Node.GetChild(j);
    if ArrayDeclNode.NodeType <> antArrayDecl then Continue;
    if ArrayDeclNode.ChildCount < 2 then Continue;
    if ArrayDeclNode.GetChild(0).NodeType <> antIdentifier then Continue;
    ArrName := UpperCase(VarToStr(ArrayDeclNode.GetChild(0).Value));
    ArrayIdx := FProgram.FindArray(ArrName);
    if ArrayIdx < 0 then
      raise Exception.CreateFmt('REDIM: array not declared (DIM it first): %s', [ArrName]);

    DimsNode := ArrayDeclNode.GetChild(1);
    if DimsNode.NodeType <> antDimensions then
      raise Exception.CreateFmt('REDIM: invalid dimensions for %s', [ArrName]);
    if DimsNode.ChildCount < 1 then
      raise Exception.CreateFmt('REDIM: missing dimensions for %s', [ArrName]);

    if DimsNode.ChildCount = 1 then
    begin
      // 1-D REDIM: bare upper bound, or antDimRange(lb, ub) — keep the original lower bound, take ub.
      DimChild := DimsNode.GetChild(0);
      if DimChild.NodeType = antDimRange then DimExpr := DimChild.GetChild(1) else DimExpr := DimChild;
      ProcessExpression(DimExpr, UbValue);
      UbReg := EnsureIntRegister(UbValue);
      EmitInstruction(ssaArrayRedim, MakeSSAValue(svkNone),
                      MakeSSAArrayRef(ArrayIdx, FProgram.GetArray(ArrayIdx).ElementType),
                      UbReg, MakeSSAConstInt(PreserveFlag));
    end
    else
    begin
      // Multi-dim REDIM a(u0, u1, ...): push each upper bound, then commit. The VM accumulates the
      // bounds (bcArrayRedimPush) and reshapes on commit (bcArrayRedimN), keeping each dim's lower bound.
      for di := 0 to DimsNode.ChildCount - 1 do
      begin
        DimChild := DimsNode.GetChild(di);
        if DimChild.NodeType = antDimRange then DimExpr := DimChild.GetChild(1) else DimExpr := DimChild;
        ProcessExpression(DimExpr, UbValue);
        UbReg := EnsureIntRegister(UbValue);
        EmitInstruction(ssaArrayRedimPush, MakeSSAValue(svkNone), UbReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
      EmitInstruction(ssaArrayRedimN, MakeSSAValue(svkNone),
                      MakeSSAArrayRef(ArrayIdx, FProgram.GetArray(ArrayIdx).ElementType),
                      MakeSSAValue(svkNone), MakeSSAConstInt(PreserveFlag));
    end;
  end;
end;

procedure TSSAGenerator.ProcessDefType(Node: TASTNode);
// DEFINT/DEFSTR... : record the default bank for each covered initial letter. Consulted by
// GetVariableType for a bare name (no suffix / no explicit type) declared after this point.
var
  Bank, i: Integer;
  Letters: string;
  c: Char;
begin
  Bank := Integer(Node.Value);
  Letters := Node.Attributes.Values['LETTERS'];
  for i := 1 to Length(Letters) do
  begin
    c := UpCase(Letters[i]);
    if (c >= 'A') and (c <= 'Z') then FDefTypeBank[c] := Bank;
  end;
end;

procedure TSSAGenerator.CollectDefTypes(Node: TASTNode);
// Pre-pass: populate the DEFINT/DEFSTR letter->bank table from all DEFtype statements BEFORE
// variables are pre-allocated (PreAllocateVariables binds them via GetVariableType). Treated as
// module-global (positional scoping not modelled): put DEFtype at the top of the program.
var
  i: Integer;
begin
  if Node = nil then Exit;
  if Node.NodeType = antDefType then ProcessDefType(Node);
  for i := 0 to Node.ChildCount - 1 do
    CollectDefTypes(Node.GetChild(i));
end;

procedure TSSAGenerator.ProcessSwap(Node: TASTNode);
// SWAP a, b (FreeBASIC): exchange the values of two lvalues. We snapshot a's current value
// into a freshly-named typed temp, then reuse ProcessAssignment for "a = b" and "b = tmp" so
// every lvalue kind (scalar, array element, UDT member) is handled by the existing store paths.
// This lowers to the classic T=A:A=B:B=T idiom, which the copy-prop soundness fix already covers.
var
  LeftNode, RightNode, AsnAB, AsnBTmp, TmpRef: TASTNode;
  ValA, TmpReg: TSSAValue;
  TmpName, Suffix: string;
  CopyOp: TSSAOpCode;
begin
  if Node.ChildCount < 2 then Exit;
  LeftNode := Node.GetChild(0);
  RightNode := Node.GetChild(1);

  // Snapshot the current value of the first operand (scalar / array / member read).
  ProcessExpression(LeftNode, ValA);
  // A non-register result means the operand is not an lvalue (e.g. a literal): nothing to swap.
  if ValA.Kind in [svkConstInt, svkConstFloat, svkConstString] then Exit;

  // Pick a temp whose name-suffix forces the bank matching the snapshot value's type.
  case ValA.RegType of
    srtInt:    begin Suffix := '%'; CopyOp := ssaCopyInt; end;
    srtString: begin Suffix := '$'; CopyOp := ssaCopyString; end;
  else
    begin Suffix := ''; CopyOp := ssaCopyFloat; end;   // float is the default bank
  end;
  TmpName := '__SWP' + IntToStr(FSwapTempSeq) + Suffix;
  Inc(FSwapTempSeq);

  // tmp = old A  (a real snapshot register, stable across the cross-store below)
  TmpReg := GetOrAllocateVariable(TmpName);
  EmitInstruction(CopyOp, TmpReg, ValA, MakeSSAValue(svkNone), MakeSSAValue(svkNone));

  // a = b  (re-dispatches on a's lvalue kind via the existing assignment machinery)
  AsnAB := TASTNode.Create(antAssignment, Node.Token);
  AsnAB.AddChild(LeftNode.Clone);
  AsnAB.AddChild(RightNode.Clone);
  ProcessAssignment(AsnAB);
  AsnAB.Free;

  // b = tmp
  TmpRef := TASTNode.CreateWithValue(antIdentifier, TmpName, Node.Token);
  AsnBTmp := TASTNode.Create(antAssignment, Node.Token);
  AsnBTmp.AddChild(RightNode.Clone);
  AsnBTmp.AddChild(TmpRef);
  ProcessAssignment(AsnBTmp);
  AsnBTmp.Free;
end;

procedure TSSAGenerator.EmitMidSubstring(ArgsNode: TASTNode; out Result: TSSAValue);
// Lower MID$/MID(str, start [,length]) to a substring (ssaStrMid). ArgsNode children: str, start [,len]
// (accepts an antArgumentList from MID$ or an antExpressionList from a bare MID(...) intercept).
var
  ArgValue, Arg2Value, Arg3Value, ArgReg, Arg2Reg, Arg3Reg: TSSAValue;
  IsW: Boolean;  // WSTRING source: start/length are codepoint-based (ssaStrMidW)
begin
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 2) then begin Result := MakeSSAValue(svkNone); Exit; end;
  IsW := IsWStringExpr(ArgsNode.GetChild(0));
  ProcessExpression(ArgsNode.GetChild(0), ArgValue);   // string
  ProcessExpression(ArgsNode.GetChild(1), Arg2Value);  // start
  ArgReg := EnsureStringRegister(ArgValue);
  Arg2Reg := EnsureIntRegister(Arg2Value);
  Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
  if ArgsNode.ChildCount >= 3 then
  begin
    ProcessExpression(ArgsNode.GetChild(2), Arg3Value);  // length
    Arg3Reg := EnsureIntRegister(Arg3Value);
    if IsW then EmitInstruction(ssaStrMidW, Result, ArgReg, Arg2Reg, Arg3Reg)
    else EmitInstruction(ssaStrMid, Result, ArgReg, Arg2Reg, Arg3Reg);
  end
  else
  begin
    // No length -> rest of string. Pass LEN(str) as the length REGISTER (the VM clamps to the chars
    // remaining). A constant here is wrong: ssaStrMid encodes its length operand as a register index.
    // For a WSTRING the codepoint length (ssaStrLenW) is the natural cap (a byte length would also work
    // since Utf8SubCP clamps to the end, but the codepoint count keeps the bytecode self-consistent).
    Arg3Reg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    if IsW then EmitInstruction(ssaStrLenW, Arg3Reg, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else EmitInstruction(ssaStrLen, Arg3Reg, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    if IsW then EmitInstruction(ssaStrMidW, Result, ArgReg, Arg2Reg, Arg3Reg)
    else EmitInstruction(ssaStrMid, Result, ArgReg, Arg2Reg, Arg3Reg);
  end;
end;

procedure TSSAGenerator.EmitWStr(ArgsNode: TASTNode; out Result: TSSAValue);
// Lower FreeBASIC WSTR(x) to a wide string. In the UTF-8 storage model a numeric argument becomes its
// STR$ text (ASCII digits are valid UTF-8); a string argument passes through unchanged (its bytes are
// already UTF-8). The WSTR(...) node is recognised as wide by IsWStringExpr, so a downstream LEN/MID on
// the result is codepoint-based. ArgsNode is an antArgumentList / antExpressionList; child 0 = value.
var
  ArgValue, ArgReg: TSSAValue;
begin
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 1) then begin Result := MakeSSAValue(svkNone); Exit; end;
  ProcessExpression(ArgsNode.GetChild(0), ArgValue);
  if ArgValue.RegType = srtString then
    Result := EnsureStringRegister(ArgValue)      // already UTF-8 bytes: widen is a no-op on storage
  else
  begin
    ArgReg := EnsureFloatRegister(ArgValue);
    Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaStrStr, Result, ArgReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

function TSSAGenerator.InferExprBank(Node: TASTNode): TSSARegisterType;
// Best-effort, non-emitting type of an expression — used only to pick the bank/suffix of the IIF
// result temp. Covers the realistic IIF branch shapes (literals, variables, string functions,
// concatenation/arithmetic); falls back to float when unknown.
  function IsBareStringFunc(const Nm: string): Boolean;
  begin
    Result := (Nm = 'MID') or (Nm = 'STRING') or (Nm = 'UCASE') or (Nm = 'LCASE') or
              (Nm = 'LTRIM') or (Nm = 'RTRIM') or (Nm = 'TRIM') or (Nm = 'SPACE');
  end;
var
  Nm: string;
  ai: Integer;
  L, R: TSSARegisterType;
begin
  Result := srtFloat;
  if Node = nil then Exit;
  case Node.NodeType of
    antLiteral:
      if ((Node.Token <> nil) and (Node.Token.TokenType = ttStringLiteral)) or VarIsStr(Node.Value) then
        Result := srtString
      else if VarIsFloat(Node.Value) then
        Result := srtFloat
      else
        Result := srtInt;
    antIdentifier:
      Result := GetVariableType(VarToStr(Node.Value));
    antArrayAccess:
      if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        Nm := UpperCase(VarToStr(Node.GetChild(0).Value));
        if (Length(Nm) > 0) and ((Nm[Length(Nm)] = '$') or IsBareStringFunc(Nm)) then
          Result := srtString
        else
        begin
          ai := FProgram.FindArray(Nm);
          if ai >= 0 then Result := FProgram.GetArray(ai).ElementType
          else Result := srtInt;
        end;
      end;
    antBinaryOp:
      begin
        L := InferExprBank(Node.GetChild(0));
        if Node.ChildCount > 1 then R := InferExprBank(Node.GetChild(1)) else R := L;
        if (L = srtString) or (R = srtString) then Result := srtString
        else if (L = srtFloat) or (R = srtFloat) then Result := srtFloat
        else Result := srtInt;
      end;
    antUnaryOp:
      if Node.ChildCount > 0 then Result := InferExprBank(Node.GetChild(0));
  end;
end;

procedure TSSAGenerator.EmitIif(ArgsNode: TASTNode; out Result: TSSAValue);
// IIF(cond, a, b) (FreeBASIC): short-circuit conditional expression — only the taken branch is
// evaluated. Lowered by REUSING ProcessIfStatement (the proven IF path, with correct PHIs at the
// merge): synthesize "IF cond THEN tmp = a ELSE tmp = b", process it, then read tmp. The result
// bank is taken from the TRUE branch (FB widens to float if either is float — not modelled here).
var
  IfNode, ThenNode, ElseNode, Asn: TASTNode;
  Bank: TSSARegisterType;
  TmpName, Suffix: string;
begin
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 3) then begin Result := MakeSSAValue(svkNone); Exit; end;

  Bank := InferExprBank(ArgsNode.GetChild(1));
  case Bank of
    srtString: Suffix := '$';
    srtInt: Suffix := '%';
  else
    Suffix := '';
  end;
  TmpName := '__IIF' + IntToStr(FSwapTempSeq) + Suffix;
  Inc(FSwapTempSeq);

  IfNode := TASTNode.Create(antIf, ArgsNode.Token);
  IfNode.AddChild(ArgsNode.GetChild(0).Clone);   // condition

  ThenNode := TASTNode.Create(antThen, ArgsNode.Token);
  Asn := TASTNode.Create(antAssignment, ArgsNode.Token);
  Asn.AddChild(TASTNode.CreateWithValue(antIdentifier, TmpName, ArgsNode.Token));
  Asn.AddChild(ArgsNode.GetChild(1).Clone);       // true value
  ThenNode.AddChild(Asn);
  IfNode.AddChild(ThenNode);

  ElseNode := TASTNode.Create(antElse, ArgsNode.Token);
  Asn := TASTNode.Create(antAssignment, ArgsNode.Token);
  Asn.AddChild(TASTNode.CreateWithValue(antIdentifier, TmpName, ArgsNode.Token));
  Asn.AddChild(ArgsNode.GetChild(2).Clone);       // false value
  ElseNode.AddChild(Asn);
  IfNode.AddChild(ElseNode);

  ProcessIfStatement(IfNode);
  IfNode.Free;

  Result := GetOrAllocateVariable(TmpName);
end;

procedure TSSAGenerator.EmitBitMacro(const FuncName: string; ArgsNode: TASTNode; out Result: TSSAValue);
// FreeBASIC bit/byte macros + CBOOL, lowered to existing integer SSA ops (no new opcodes).
// All operands are materialized into integer registers (EnsureIntRegister handles constants), so
// the bitwise/shift/compare ops always see register operands.
//   LOBYTE(x)      = x AND &HFF              HIBYTE(x)   = (x SHR 8)  AND &HFF
//   LOWORD(x)      = x AND &HFFFF            HIWORD(x)   = (x SHR 16) AND &HFFFF
//   BIT(x, b)      = (x SHR b) AND 1
//   BITSET(x, b)   = x OR  (1 SHL b)         BITRESET(x, b) = x AND NOT (1 SHL b)
//   CBOOL(x)       = -1 if x <> 0 else 0     (matches the VM's -1/0 boolean convention)
var
  V0, V1, A0, A1, Tmp, OneShl, NotShl: TSSAValue;

  function NewIntReg: TSSAValue;
  begin
    Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  end;

  function IntConst(C: Int64): TSSAValue;
  begin
    Result := EnsureIntRegister(MakeSSAConstInt(C));
  end;

begin
  Result := MakeSSAValue(svkNone);
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 1) then Exit;

  ProcessExpression(ArgsNode.GetChild(0), V0);
  A0 := EnsureIntRegister(V0);
  Result := NewIntReg;

  if FuncName = kLOBYTE then
    EmitInstruction(ssaBitwiseAnd, Result, A0, IntConst($FF), MakeSSAValue(svkNone))
  else if FuncName = kLOWORD then
    EmitInstruction(ssaBitwiseAnd, Result, A0, IntConst($FFFF), MakeSSAValue(svkNone))
  else if FuncName = kHIBYTE then
  begin
    Tmp := NewIntReg;
    EmitInstruction(ssaShr, Tmp, A0, IntConst(8), MakeSSAValue(svkNone));
    EmitInstruction(ssaBitwiseAnd, Result, Tmp, IntConst($FF), MakeSSAValue(svkNone));
  end
  else if FuncName = kHIWORD then
  begin
    Tmp := NewIntReg;
    EmitInstruction(ssaShr, Tmp, A0, IntConst(16), MakeSSAValue(svkNone));
    EmitInstruction(ssaBitwiseAnd, Result, Tmp, IntConst($FFFF), MakeSSAValue(svkNone));
  end
  else if FuncName = kCBOOL then
    EmitInstruction(ssaCmpNeInt, Result, A0, IntConst(0), MakeSSAValue(svkNone))
  else if (FuncName = kBIT) or (FuncName = kBITSET) or (FuncName = kBITRESET) then
  begin
    if ArgsNode.ChildCount < 2 then begin Result := A0; Exit; end;   // missing bit index: pass x through
    ProcessExpression(ArgsNode.GetChild(1), V1);
    A1 := EnsureIntRegister(V1);
    if FuncName = kBIT then
    begin
      Tmp := NewIntReg;
      EmitInstruction(ssaShr, Tmp, A0, A1, MakeSSAValue(svkNone));            // x SHR b
      EmitInstruction(ssaBitwiseAnd, Result, Tmp, IntConst(1), MakeSSAValue(svkNone));   // AND 1
    end
    else if FuncName = kBITSET then
    begin
      OneShl := NewIntReg;
      EmitInstruction(ssaShl, OneShl, IntConst(1), A1, MakeSSAValue(svkNone));   // 1 SHL b
      EmitInstruction(ssaBitwiseOr, Result, A0, OneShl, MakeSSAValue(svkNone));  // x OR (1 SHL b)
    end
    else  // kBITRESET
    begin
      OneShl := NewIntReg;
      NotShl := NewIntReg;
      EmitInstruction(ssaShl, OneShl, IntConst(1), A1, MakeSSAValue(svkNone));    // 1 SHL b
      EmitInstruction(ssaBitwiseNot, NotShl, OneShl, MakeSSAValue(svkNone), MakeSSAValue(svkNone));  // NOT (1 SHL b)
      EmitInstruction(ssaBitwiseAnd, Result, A0, NotShl, MakeSSAValue(svkNone));  // x AND NOT (1 SHL b)
    end;
  end;
end;

procedure TSSAGenerator.EmitBareStringFunc(const DollarName: string; ArrayAccessNode: TASTNode; out Result: TSSAValue);
// FreeBASIC's canonical bare names (CHR/STR/LEFT/RIGHT) are aliases of the $-suffixed functions we
// already implement. Synthesize the equivalent antFunctionCall ("CHR$"/...) with the same argument
// list and process it through the existing string-function lowering — no duplicate code.
var
  SynthCall, ArgList, IdxNode: TASTNode;
  i: Integer;
begin
  Result := MakeSSAValue(svkNone);
  SynthCall := TASTNode.CreateWithValue(antFunctionCall, DollarName, ArrayAccessNode.Token);
  try
    // Rebuild a proper antArgumentList from the array-access index list (the string-function
    // handlers expect an antArgumentList, not the raw index-list node).
    ArgList := TASTNode.Create(antArgumentList, ArrayAccessNode.Token);
    if ArrayAccessNode.ChildCount >= 2 then
    begin
      IdxNode := ArrayAccessNode.GetChild(1);
      for i := 0 to IdxNode.ChildCount - 1 do
        ArgList.AddChild(IdxNode.GetChild(i).Clone);
    end;
    SynthCall.AddChild(ArgList);   // argument list -> child 0
    ProcessExpression(SynthCall, Result);
  finally
    SynthCall.Free;
  end;
end;

procedure TSSAGenerator.EmitArrayLen(ArgsNode: TASTNode; out Result: TSSAValue);
// FreeBASIC ARRAYLEN(arr): total number of elements = product over every dimension of
// (UBOUND(arr,d) - LBOUND(arr,d) + 1). Emitted at runtime from the existing bound ops so it is
// correct for fixed and REDIM'd arrays alike. The argument is an array name (bare or array-access).
var
  NameNode: TASTNode;
  ArrName: string;
  ArrayIdx, DimCount, d: Integer;
  ArrayRef, Acc, One, DimReg, Ub, Lb, Diff, Cnt, NewAcc: TSSAValue;
begin
  Result := MakeSSAValue(svkNone);
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 1) then Exit;
  NameNode := ArgsNode.GetChild(0);
  if NameNode.NodeType = antArrayAccess then ArrName := VarToStr(NameNode.GetChild(0).Value)
  else ArrName := VarToStr(NameNode.Value);
  ArrayIdx := FProgram.FindArray(ArrName);
  if ArrayIdx < 0 then Exit;                       // not an array -> leave Result as none
  DimCount := FProgram.GetArray(ArrayIdx).DimCount;
  if DimCount < 1 then DimCount := 1;
  ArrayRef := MakeSSAArrayRef(ArrayIdx, srtInt);
  One := EnsureIntRegister(MakeSSAConstInt(1));
  Acc := EnsureIntRegister(MakeSSAConstInt(1));     // accumulator = 1
  for d := 0 to DimCount - 1 do
  begin
    DimReg := EnsureIntRegister(MakeSSAConstInt(d));  // 0-based dimension index
    Ub := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    Lb := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaArrayUBound, Ub, ArrayRef, DimReg, MakeSSAValue(svkNone));
    EmitInstruction(ssaArrayLBound, Lb, ArrayRef, DimReg, MakeSSAValue(svkNone));
    Diff := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaSubInt, Diff, Ub, Lb, MakeSSAValue(svkNone));   // ub - lb
    Cnt := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaAddInt, Cnt, Diff, One, MakeSSAValue(svkNone)); // + 1
    NewAcc := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaMulInt, NewAcc, Acc, Cnt, MakeSSAValue(svkNone));
    Acc := NewAcc;
  end;
  Result := Acc;
end;

procedure TSSAGenerator.EmitShortCircuit(Node: TASTNode; out Result: TSSAValue);
// a ANDALSO b  ->  IIF(a, b, 0)   (b evaluated only when a is nonzero)
// a ORELSE  b  ->  IIF(a, -1, b)  (b evaluated only when a is zero)
// EmitIif emits a real IF, so only the taken branch runs => genuine short-circuit. The IIF result
// is normalised to a FreeBASIC boolean (-1/0). For the usual integer/boolean operands this is exact.
var
  ArgsNode: TASTNode;
  IifVal, Zero: TSSAValue;
begin
  Result := MakeSSAValue(svkNone);
  if Node.ChildCount < 2 then Exit;
  ArgsNode := TASTNode.Create(antArgumentList, Node.Token);
  try
    ArgsNode.AddChild(Node.GetChild(0).Clone);          // cond = a
    if Node.Token.TokenType = ttOpAndAlso then
    begin
      ArgsNode.AddChild(Node.GetChild(1).Clone);                               // true  = b
      ArgsNode.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Node.Token));  // false = 0
    end
    else
    begin
      ArgsNode.AddChild(TASTNode.CreateWithValue(antLiteral, -1, Node.Token)); // true  = -1
      ArgsNode.AddChild(Node.GetChild(1).Clone);                               // false = b
    end;
    EmitIif(ArgsNode, IifVal);
  finally
    ArgsNode.Free;
  end;
  Zero := EnsureIntRegister(MakeSSAConstInt(0));
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaCmpNeInt, Result, EnsureIntRegister(IifVal), Zero, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.EmitIsCheck(ObjNode: TASTNode; const TypeName: string; out Result: TSSAValue);
// obj IS Type (FreeBASIC RTTI). The set of concrete types satisfying "is-a Type" — Type itself plus
// every descendant — is known at compile time, so this lowers to: tid = typeid(obj); result = OR over
// {i : FUDTs[i] is-a Type} of (tid = i). No runtime hierarchy walk, no new opcodes. Yields -1/0.
var
  TU: string;
  Handle, Tid, Acc, Idx, Cmp, NewAcc: TSSAValue;
  i: Integer;
  Matched: Boolean;
begin
  Result := MakeSSAConstInt(0);
  TU := UpperCase(TypeName);
  if FindUDT(TU) < 0 then Exit;                    // RHS is not a known type -> always 0
  ProcessExpression(ObjNode, Handle);              // obj evaluates to its record handle (int)
  Handle := EnsureIntRegister(Handle);
  Tid := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaRecordTypeId, Tid, Handle, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  Matched := False;
  Acc := MakeSSAValue(svkNone);
  for i := 0 to High(FUDTs) do
    if IsSubtypeOf(FUDTs[i].Name, TU) then
    begin
      Idx := EnsureIntRegister(MakeSSAConstInt(i));
      Cmp := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaCmpEqInt, Cmp, Tid, Idx, MakeSSAValue(svkNone));   // (tid = i) -> -1/0
      if not Matched then begin Acc := Cmp; Matched := True; end
      else
      begin
        NewAcc := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaBitwiseOr, NewAcc, Acc, Cmp, MakeSSAValue(svkNone));
        Acc := NewAcc;
      end;
    end;
  if Matched then Result := Acc;                   // else no concrete type matches -> stays 0
end;

procedure TSSAGenerator.EmitStringFill(ArgsNode: TASTNode; out Result: TSSAValue);
// Lower STRING(count, ch) -> count copies of a character (ssaStrString). ArgsNode children:
// count, ch. ch may be a string (its first character's code is used) or a numeric char code.
var
  CountVal, ChVal, CountReg, CodeReg, StrReg: TSSAValue;
begin
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 2) then begin Result := MakeSSAValue(svkNone); Exit; end;
  ProcessExpression(ArgsNode.GetChild(0), CountVal);
  CountReg := EnsureIntRegister(CountVal);
  ProcessExpression(ArgsNode.GetChild(1), ChVal);
  if (ChVal.Kind = svkConstString) or
     ((ChVal.Kind = svkRegister) and (ChVal.RegType = srtString)) then
  begin
    // String char argument: take ASC of its first character.
    StrReg := EnsureStringRegister(ChVal);
    CodeReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaStrAsc, CodeReg, StrReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    CodeReg := EnsureIntRegister(ChVal);  // numeric char code (int/float coerced to int)
  Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
  EmitInstruction(ssaStrString, Result, CountReg, CodeReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.EmitWStringFill(ArgsNode: TASTNode; out Result: TSSAValue);
// Lower WSTRING(count, cp) -> count copies of the UTF-8 char for Unicode codepoint cp (ssaStrWStringN).
// cp may be a wide-char string (its first codepoint is used) or a numeric codepoint. Wide form of STRING.
var
  CountVal, ChVal, CountReg, CodeReg, StrReg: TSSAValue;
begin
  if (ArgsNode = nil) or (ArgsNode.ChildCount < 2) then begin Result := MakeSSAValue(svkNone); Exit; end;
  ProcessExpression(ArgsNode.GetChild(0), CountVal);
  CountReg := EnsureIntRegister(CountVal);
  ProcessExpression(ArgsNode.GetChild(1), ChVal);
  if (ChVal.Kind = svkConstString) or
     ((ChVal.Kind = svkRegister) and (ChVal.RegType = srtString)) then
  begin
    // String char argument: ASC gives the first BYTE — acceptable for an ASCII char; a full wide-char
    // first-codepoint decode is a v1 limitation (use the numeric codepoint form for non-ASCII).
    StrReg := EnsureStringRegister(ChVal);
    CodeReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaStrAsc, CodeReg, StrReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else
    CodeReg := EnsureIntRegister(ChVal);  // numeric codepoint
  Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
  EmitInstruction(ssaStrWStringN, Result, CountReg, CodeReg, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessMidStatement(Node: TASTNode);
// MID(target, start [, len]) = source (FreeBASIC, MODERN): overwrite a substring of target in
// place; target's length is unchanged. Lowered with existing string ops (no new opcode):
//   midsrc = LEFT$( (len given ? LEFT$(source,len) : source), LEN(target)-start+1 )
//   target = LEFT$(target, start-1) + midsrc + MID$(target, start + LEN(midsrc))
// The trailing concat lands in a temp string var, then ProcessAssignment writes it back to the
// target lvalue (scalar / array element / member, uniformly). Assumes start >= 1 (1-based, as in FB).

  function NS: TSSAValue;  // fresh string register
  begin Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString)); end;
  function NI: TSSAValue;  // fresh int register
  begin Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt)); end;

var
  TargetNode, StartNode, LenNode, SourceNode, AsnNode, TmpRef: TASTNode;
  TextVal, StartVal, SrcVal, LenVal: TSSAValue;
  TextReg, StartReg, SrcReg, SrcCapReg, MidSrcReg: TSSAValue;
  LenTextReg, AvailTmpReg, AvailReg, OneReg, StartM1Reg, PrefixReg: TSSAValue;
  LenMidReg, SuffixStartReg, SuffixReg, R1Reg, ResultReg: TSSAValue;
  TmpName: string;
  HasLen: Boolean;
  NoneV: TSSAValue;
begin
  if Node.ChildCount < 3 then Exit;
  NoneV := MakeSSAValue(svkNone);
  TargetNode := Node.GetChild(0);
  StartNode := Node.GetChild(1);
  HasLen := Node.ChildCount >= 4;
  if HasLen then
  begin
    LenNode := Node.GetChild(2);
    SourceNode := Node.GetChild(3);
  end
  else
  begin
    LenNode := nil;
    SourceNode := Node.GetChild(2);
  end;

  // Read inputs.
  ProcessExpression(TargetNode, TextVal); TextReg := EnsureStringRegister(TextVal);
  ProcessExpression(StartNode, StartVal); StartReg := EnsureIntRegister(StartVal);
  ProcessExpression(SourceNode, SrcVal);  SrcReg := EnsureStringRegister(SrcVal);

  // source capped to len: LEFT$(source, len) when a length was given.
  if HasLen then
  begin
    ProcessExpression(LenNode, LenVal);
    SrcCapReg := NS;
    EmitInstruction(ssaStrLeft, SrcCapReg, SrcReg, EnsureIntRegister(LenVal), NoneV);
  end
  else
    SrcCapReg := SrcReg;

  OneReg := NI; EmitInstruction(ssaLoadConstInt, OneReg, MakeSSAConstInt(1), NoneV, NoneV);

  // avail = LEN(target) - start + 1
  LenTextReg := NI;  EmitInstruction(ssaStrLen, LenTextReg, TextReg, NoneV, NoneV);
  AvailTmpReg := NI; EmitInstruction(ssaSubInt, AvailTmpReg, LenTextReg, StartReg, NoneV);
  AvailReg := NI;    EmitInstruction(ssaAddInt, AvailReg, AvailTmpReg, OneReg, NoneV);

  // midsrc = LEFT$(srcCap, avail) -> the characters that actually overwrite, clamped to room left.
  MidSrcReg := NS; EmitInstruction(ssaStrLeft, MidSrcReg, SrcCapReg, AvailReg, NoneV);

  // prefix = LEFT$(target, start-1)
  StartM1Reg := NI; EmitInstruction(ssaSubInt, StartM1Reg, StartReg, OneReg, NoneV);
  PrefixReg := NS;  EmitInstruction(ssaStrLeft, PrefixReg, TextReg, StartM1Reg, NoneV);

  // suffix = MID$(target, start + LEN(midsrc), LEN(target))  [rest of the original string after the
  // overwrite]. Length is passed as the LEN(target) register (always >= the remaining count, which the
  // VM clamps) — NOT a constant, since ssaStrMid encodes its length operand as a register index.
  LenMidReg := NI;      EmitInstruction(ssaStrLen, LenMidReg, MidSrcReg, NoneV, NoneV);
  SuffixStartReg := NI; EmitInstruction(ssaAddInt, SuffixStartReg, StartReg, LenMidReg, NoneV);
  SuffixReg := NS;      EmitInstruction(ssaStrMid, SuffixReg, TextReg, SuffixStartReg, LenTextReg);

  // result = prefix + midsrc + suffix, into a named temp string var so it can be assigned back.
  TmpName := '__MID' + IntToStr(FSwapTempSeq) + '$';
  Inc(FSwapTempSeq);
  ResultReg := GetOrAllocateVariable(TmpName);
  R1Reg := NS; EmitInstruction(ssaStrConcat, R1Reg, PrefixReg, MidSrcReg, NoneV);
  EmitInstruction(ssaStrConcat, ResultReg, R1Reg, SuffixReg, NoneV);

  // target = result  (dispatches on the target lvalue kind via the existing assignment machinery)
  AsnNode := TASTNode.Create(antAssignment, Node.Token);
  AsnNode.AddChild(TargetNode.Clone);
  TmpRef := TASTNode.CreateWithValue(antIdentifier, TmpName, Node.Token);
  AsnNode.AddChild(TmpRef);
  ProcessAssignment(AsnNode);
  AsnNode.Free;
end;

procedure TSSAGenerator.ProcessLRSetStatement(Node: TASTNode; IsLeft: Boolean);
// LSET/RSET dst, src (FreeBASIC/QBasic): justify src into dst's string buffer, preserving dst's
// current length. src is truncated from the right if longer than the buffer, else padded with
// spaces (LSET pads on the right / left-justifies; RSET pads on the left / right-justifies).
// Lowered with existing string ops (no new opcode):
//   N      = LEN(dst)
//   capped = LEFT$(src, N)                 ' truncate from the right when too long
//   pad    = SPACE$(N - LEN(capped))       ' >= 0
//   dst    = capped + pad   (LSET)  |  pad + capped  (RSET)
// The result lands in a named temp string var, then ProcessAssignment writes it back to the dst
// lvalue (scalar / array element / member, uniformly).

  function NS: TSSAValue;  // fresh string register
  begin Result := MakeSSARegister(srtString, FProgram.AllocRegister(srtString)); end;
  function NI: TSSAValue;  // fresh int register
  begin Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt)); end;

var
  DstNode, SrcNode, AsnNode, TmpRef: TASTNode;
  DstVal, SrcVal: TSSAValue;
  DstReg, SrcReg, NReg, CappedReg, LCapReg, PadNReg, PadReg, ResultReg: TSSAValue;
  TmpName: string;
  NoneV: TSSAValue;
begin
  if Node.ChildCount < 2 then Exit;
  NoneV := MakeSSAValue(svkNone);
  DstNode := Node.GetChild(0);
  SrcNode := Node.GetChild(1);

  // Read inputs.
  ProcessExpression(DstNode, DstVal); DstReg := EnsureStringRegister(DstVal);
  ProcessExpression(SrcNode, SrcVal); SrcReg := EnsureStringRegister(SrcVal);

  // N = LEN(dst): the buffer size to preserve.
  NReg := NI; EmitInstruction(ssaStrLen, NReg, DstReg, NoneV, NoneV);

  // capped = LEFT$(src, N): truncate from the right when src is longer than the buffer.
  CappedReg := NS; EmitInstruction(ssaStrLeft, CappedReg, SrcReg, NReg, NoneV);

  // pad = SPACE$(N - LEN(capped)).  LEN(capped) = min(LEN(src), N) so the count is always >= 0.
  LCapReg := NI;  EmitInstruction(ssaStrLen, LCapReg, CappedReg, NoneV, NoneV);
  PadNReg := NI;  EmitInstruction(ssaSubInt, PadNReg, NReg, LCapReg, NoneV);
  PadReg := NS;   EmitInstruction(ssaStrSpace, PadReg, PadNReg, NoneV, NoneV);

  // result = capped + pad (left-justify) | pad + capped (right-justify), into a named temp string var.
  TmpName := '__LRSET' + IntToStr(FSwapTempSeq) + '$';
  Inc(FSwapTempSeq);
  ResultReg := GetOrAllocateVariable(TmpName);
  if IsLeft then
    EmitInstruction(ssaStrConcat, ResultReg, CappedReg, PadReg, NoneV)
  else
    EmitInstruction(ssaStrConcat, ResultReg, PadReg, CappedReg, NoneV);

  // dst = result  (dispatches on the dst lvalue kind via the existing assignment machinery)
  AsnNode := TASTNode.Create(antAssignment, Node.Token);
  AsnNode.AddChild(DstNode.Clone);
  TmpRef := TASTNode.CreateWithValue(antIdentifier, TmpName, Node.Token);
  AsnNode.AddChild(TmpRef);
  ProcessAssignment(AsnNode);
  AsnNode.Free;
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

  // M8: open a block scope for this loop body (mark push at body entry; UDT DIMs in the body are
  // destructed and reclaimed each iteration). Paired with BlockScopeExit in ProcessNext.
  BlockScopeEnter(True);   // loop body: EXIT FOR unwinds down to here

  // Push loop info onto stack
  LoopInfo.LoopKind := lkFor;
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
  // CONTINUE FOR target: a dedicated block placed at the increment (materialized in ProcessNext only
  // if a CONTINUE actually references it, so loops without CONTINUE keep byte-identical bytecode).
  LoopInfo.ContLabel := GenerateUniqueLabel('for_cont');
  LoopInfo.ContUsed := False;
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
  LoopInfo.LoopKind := lkDo;
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
  // CONTINUE DO/WHILE target. Top-tested loops re-check the condition (CondLabel); the bottom-tested
  // form needs a dedicated block at the trailing condition (materialized below only if referenced);
  // an unconditional DO...LOOP simply re-runs the body (BodyLabel).
  if HasCondition and (ConditionPosition = 'TOP') then
    LoopInfo.ContLabel := CondLabel
  else if HasCondition then
    LoopInfo.ContLabel := GenerateUniqueLabel('do_cont')
  else
    LoopInfo.ContLabel := BodyLabel;
  LoopInfo.ContUsed := False;
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

    BlockScopeEnter(True);   // M8: open this loop body's block scope (EXIT DO unwinds down to here)
    // Process body statements
    if Assigned(BodyNode) then
    begin
      for i := 0 to BodyNode.ChildCount - 1 do
        ProcessStatement(BodyNode.GetChild(i));
    end;
    BlockScopeExit;    // M8: destruct + reclaim the body's DIM'd UDTs before the back-edge

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

    BlockScopeEnter(True);   // M8: open this loop body's block scope (EXIT DO unwinds down to here)
    // Process body statements
    if Assigned(BodyNode) then
    begin
      for i := 0 to BodyNode.ChildCount - 1 do
        ProcessStatement(BodyNode.GetChild(i));
    end;
    BlockScopeExit;    // M8: destruct + reclaim the body's DIM'd UDTs before the back-edge

    // CONTINUE DO lands here (after per-iteration cleanup, at the trailing condition test). Only
    // materialized when a CONTINUE referenced it; otherwise the condition stays inline.
    if HasCondition and Assigned(ConditionNode) and FLoopStack[High(FLoopStack)].ContUsed then
    begin
      CondBlock := FProgram.CreateBlock(FLoopStack[High(FLoopStack)].ContLabel);
      if Assigned(FCurrentBlock) then
      begin
        FCurrentBlock.AddSuccessor(CondBlock);
        CondBlock.AddPredecessor(FCurrentBlock);
      end;
      FCurrentBlock := CondBlock;
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
  // BEGIN/BEND compound block. FB lexical scope (MODERN): it is its own block scope — a DIM here is
  // block-local (shadowing) and its UDTs are destructed at the block end.
  if FModernMode then BlockScopeEnter;
  for i := 0 to Node.ChildCount - 1 do
    ProcessStatement(Node.GetChild(i));
  if FModernMode then BlockScopeExit;
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
  BodyBlock, CondBlock, EndBlock, ContBlock: TSSABasicBlock;  // PHASE 3 TIER 3: CFG construction
begin
  // Pop loop info from stack
  if Length(FLoopStack) = 0 then Exit;  // Error: NEXT without FOR
  LoopInfo := FLoopStack[High(FLoopStack)];
  SetLength(FLoopStack, Length(FLoopStack) - 1);

  // M8: close this loop body's block scope (destruct its DIM'd UDTs + reclaim) before the back-edge.
  BlockScopeExit;

  // CONTINUE FOR lands here (after the per-iteration block cleanup, at the increment). Only emitted
  // when a CONTINUE referenced it; otherwise the increment stays inline and the bytecode is unchanged.
  if LoopInfo.ContUsed then
  begin
    ContBlock := FProgram.CreateBlock(LoopInfo.ContLabel);
    if Assigned(FCurrentBlock) then   // body falls through into the increment block
    begin
      FCurrentBlock.AddSuccessor(ContBlock);
      ContBlock.AddPredecessor(FCurrentBlock);
    end;
    FCurrentBlock := ContBlock;
  end;

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

  // FB lexical scope (MODERN): the THEN branch is its own block scope — a DIM here is block-local
  // (shadowing) and its UDTs are destructed at the branch end (before the jump to EndLabel).
  if FModernMode then BlockScopeEnter;
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Assigned(Child) and (Child.NodeType = antThen) then
    begin
      ProcessStatement(Child);
      Break;
    end;
  end;
  if FModernMode then BlockScopeExit;

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

    if FModernMode then BlockScopeEnter;   // FB scope: the ELSE branch is its own block scope
    for i := 1 to Node.ChildCount - 1 do
    begin
      Child := Node.GetChild(i);
      if Assigned(Child) and (Child.NodeType = antElse) then
      begin
        ProcessStatement(Child);
        Break;
      end;
    end;
    if FModernMode then BlockScopeExit;

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

  // GOTO-unwind: if this GOTO jumps to a label at block-depth 0 of the current frame (a top-level
  // label, outside every block scope), it leaves all currently-open block scopes — run their
  // destructors and pop their record marks first (innermost-first), exactly as EXIT/RETURN do. Without
  // this, a GOTO out of a FOR/IF/SCOPE body that DIM'd a UDT would skip its destructor and leak the
  // record until frame exit. The frames are not dropped (the loop's normal end still cleans the other
  // path). Targets at an intermediate depth are left untouched (conservative; rare and ill-defined).
  if (LabelNode.NodeType = antIdentifier) and (InnermostBlockFrameIdx >= 0) and
     (FCurrentTopLevelLabels.IndexOf(UpperCase(VarToStr(LabelNode.Value))) >= 0) then
    EmitAllBlockScopesCleanup;

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

procedure TSSAGenerator.ProcessScreenRes(Node: TASTNode);
// SCREENRES w, h [, depth [, num_pages]] : set the graphics screen resolution (routed to the backend).
// depth is accepted-and-ignored; num_pages (a compile-time constant, default 1) sets up page flipping
// and is carried in Src3 -> Immediate (the VM reads it as the literal page count, not a register).
var
  WVal, HVal, NVal, WReg, HReg: TSSAValue;
  NumPages: Int64;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 2) then Exit;
  ProcessExpression(Node.GetChild(0), WVal); WReg := EnsureIntRegister(WVal);
  ProcessExpression(Node.GetChild(1), HVal); HReg := EnsureIntRegister(HVal);
  NumPages := 1;
  if Node.ChildCount >= 4 then
  begin
    ProcessExpression(Node.GetChild(3), NVal);   // num_pages (constant)
    if NVal.Kind = svkConstInt then NumPages := NVal.ConstInt
    else if NVal.Kind = svkConstFloat then NumPages := Trunc(NVal.ConstFloat);
  end;
  if NumPages < 1 then NumPages := 1;
  EmitInstruction(ssaGfxScreenRes, MakeSSAValue(svkNone), WReg, HReg, MakeSSAConstInt(NumPages));
end;

procedure TSSAGenerator.ProcessGfxPset(Node: TASTNode);
// PSET (x, y) [, color] : set a pixel. Children: x, y [, color]. Color in Src3 -> Immediate (int reg).
var
  XVal, YVal, CVal, XReg, YReg, CReg: TSSAValue;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 2) then Exit;
  ProcessExpression(Node.GetChild(0), XVal); XReg := EnsureIntRegister(XVal);
  ProcessExpression(Node.GetChild(1), YVal); YReg := EnsureIntRegister(YVal);
  if Node.ChildCount >= 3 then
  begin
    ProcessExpression(Node.GetChild(2), CVal); CReg := EnsureIntRegister(CVal);
  end
  else
    CReg := DefaultDrawColorReg;   // omitted colour -> current draw foreground (COLOR)
  EmitInstruction(ssaGfxPset, MakeSSAValue(svkNone), XReg, YReg, CReg);   // Src3=color -> Immediate
end;

procedure TSSAGenerator.ProcessGfxPaint(Node: TASTNode);
// PAINT (x, y) [, color] : flood fill from (x,y) with color. Children: x, y [, color]. Color in Src3
// -> Immediate (int reg). (FB's optional border-colour argument is deferred.)
var
  XVal, YVal, CVal, XReg, YReg, CReg: TSSAValue;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 2) then Exit;
  ProcessExpression(Node.GetChild(0), XVal); XReg := EnsureIntRegister(XVal);
  ProcessExpression(Node.GetChild(1), YVal); YReg := EnsureIntRegister(YVal);
  if Node.ChildCount >= 3 then
  begin
    ProcessExpression(Node.GetChild(2), CVal); CReg := EnsureIntRegister(CVal);
  end
  else
    CReg := DefaultDrawColorReg;   // omitted colour -> current draw foreground (COLOR)
  EmitInstruction(ssaGfxPaint, MakeSSAValue(svkNone), XReg, YReg, CReg);   // Src3=color -> Immediate
end;

procedure TSSAGenerator.ProcessGfxLine(Node: TASTNode);
// LINE (x1,y1)-(x2,y2) [,color] [,B|BF] : draw a line, a box outline (B) or a filled box (BF).
// Children: x1, y1, x2, y2 [, color]. The SHAPE attribute selects the shape ('' / 'B' / 'BF').
// Packed for the compiler as Src1=x1, Src2=y1, Src3=x2, PhiSources[0]=y2, [1]=color, [2]=shape flag.
var
  X1V, Y1V, X2V, Y2V, CV: TSSAValue;
  X1R, Y1R, X2R, Y2R, CR: TSSAValue;
  Instr: TSSAInstruction;
  Shape: string;
  Flag: Int64;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 4) then Exit;
  ProcessExpression(Node.GetChild(0), X1V); X1R := EnsureIntRegister(X1V);
  ProcessExpression(Node.GetChild(1), Y1V); Y1R := EnsureIntRegister(Y1V);
  ProcessExpression(Node.GetChild(2), X2V); X2R := EnsureIntRegister(X2V);
  ProcessExpression(Node.GetChild(3), Y2V); Y2R := EnsureIntRegister(Y2V);
  if Node.ChildCount >= 5 then
  begin
    ProcessExpression(Node.GetChild(4), CV); CR := EnsureIntRegister(CV);
  end
  else
    CR := DefaultDrawColorReg;   // omitted colour -> current draw foreground (COLOR)
  Shape := UpperCase(Node.Attributes.Values['SHAPE']);
  if Shape = 'BF' then Flag := 2
  else if Shape = 'B' then Flag := 1
  else Flag := 0;
  EmitInstruction(ssaGfxLine, MakeSSAValue(svkNone), X1R, Y1R, X2R);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Y2R, nil);
  Instr.AddPhiSource(CR, nil);
  Instr.AddPhiSource(MakeSSAConstInt(Flag), nil);
end;

procedure TSSAGenerator.ProcessGfxCircle(Node: TASTNode);
// CIRCLE (x, y), r [, color] : a circle of radius r (drawn via the ellipse primitive, RX=RY=r).
// Children: x, y, r [, color]. Packed as Src1=x, Src2=y, Src3=r, PhiSources[0]=color.
var
  XV, YV, RV, CV, XR, YR, RR, CR: TSSAValue;
  Instr: TSSAInstruction;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 3) then Exit;
  ProcessExpression(Node.GetChild(0), XV); XR := EnsureIntRegister(XV);
  ProcessExpression(Node.GetChild(1), YV); YR := EnsureIntRegister(YV);
  ProcessExpression(Node.GetChild(2), RV); RR := EnsureIntRegister(RV);
  if Node.ChildCount >= 4 then
  begin
    ProcessExpression(Node.GetChild(3), CV); CR := EnsureIntRegister(CV);
  end
  else
    CR := DefaultDrawColorReg;   // omitted colour -> current draw foreground (COLOR)
  EmitInstruction(ssaGfxCircle, MakeSSAValue(svkNone), XR, YR, RR);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(CR, nil);
end;

procedure TSSAGenerator.ProcessPalette(Node: TASTNode);
// PALETTE (FreeBASIC). OP attribute selects the form:
//   RESET -> ssaGfxPaletteReset (no operands).
//   SET   -> pack (r,g,b,255) into a colour via ssaGraphicRGBA, then ssaGfxPalette(index, colour).
//   GET   -> read entry index into the r,g,b variables. Lowered to three synthetic assignments
//            "var = __PALGET(index, which)" so the full assignment machinery handles the store; the
//            __PALGET graphics function (ssaGfxPalGet) reads FGraphics.GetPaletteColor and extracts
//            the requested 0-255 component. The index expression is cloned per component.
var
  Op: string;
  IdxV, RV, GV, BV, AReg, ColorReg, IdxR: TSSAValue;
  Instr: TSSAInstruction;
  i: Integer;
  Assign, Call, ArgList, WhichLit: TASTNode;
begin
  if FCurrentBlock = nil then Exit;
  Op := UpperCase(Node.Attributes.Values['OP']);

  if Op = 'RESET' then
  begin
    EmitInstruction(ssaGfxPaletteReset, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

  if Node.ChildCount < 4 then Exit;

  if Op = 'GET' then
  begin
    // var(i) = __PALGET(index, i)  for i = 0 (r), 1 (g), 2 (b)
    for i := 0 to 2 do
    begin
      WhichLit := TASTNode.CreateWithValue(antLiteral, i, Node.Token);
      ArgList := TASTNode.Create(antArgumentList, Node.Token);
      ArgList.AddChild(Node.GetChild(0).Clone);   // index (cloned per component)
      ArgList.AddChild(WhichLit);
      Call := TASTNode.CreateWithValue(antGraphicsFunction, '__PALGET', Node.Token);
      Call.AddChild(ArgList);
      Assign := TASTNode.Create(antAssignment, Node.Token);
      Assign.AddChild(Node.GetChild(i + 1).Clone); // destination variable r/g/b
      Assign.AddChild(Call);
      ProcessStatement(Assign);
      Assign.Free;   // frees the synthetic subtree (Call/ArgList/clones)
    end;
    Exit;
  end;

  // SET: index, r, g, b (components 0-255). The engine palette is ABGR ($AABBGGRR), so pack the colour
  // by reusing the RGBA instruction with R and B swapped: RGBA(b, g, r, 255) = (255<<24)|(b<<16)|(g<<8)|r.
  ProcessExpression(Node.GetChild(0), IdxV); IdxR := EnsureIntRegister(IdxV);
  ProcessExpression(Node.GetChild(1), RV);   RV := EnsureIntRegister(RV);
  ProcessExpression(Node.GetChild(2), GV);   GV := EnsureIntRegister(GV);
  ProcessExpression(Node.GetChild(3), BV);   BV := EnsureIntRegister(BV);
  AReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaLoadConstInt, AReg, MakeSSAConstInt(255), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  ColorReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaGraphicRGBA, ColorReg, BV, GV, RV);   // (B,G,R) -> ABGR packing
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(AReg, nil);
  EmitInstruction(ssaGfxPalette, MakeSSAValue(svkNone), IdxR, ColorReg, MakeSSAValue(svkNone));
end;

function TSSAGenerator.DefaultDrawColorReg: TSSAValue;
// Colour register used when a drawing statement omits its colour: the current draw foreground (set by
// COLOR; defaults to white). Read at runtime via ssaGfxForeColor so a prior COLOR takes effect. (Safe
// under LICM: in GlobalVariableSemantics mode no register-dest instruction is hoisted.)
begin
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaGfxForeColor, Result, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessGfxColor(Node: TASTNode);
// COLOR [fg] [, bg] (FreeBASIC): set the current draw foreground/background colour. Either may be
// omitted; the HASFG/HASBG attributes (and Immediate flag bits) tell the VM which to update.
var
  FgReg, BgReg, ChildVal: TSSAValue;
  HasFg, HasBg: Boolean;
  Flags: Int64;
  Idx: Integer;
begin
  if FCurrentBlock = nil then Exit;
  HasFg := Node.Attributes.Values['HASFG'] = '1';
  HasBg := Node.Attributes.Values['HASBG'] = '1';
  Idx := 0;
  FgReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaLoadConstInt, FgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  BgReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaLoadConstInt, BgReg, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  if HasFg then
  begin
    ProcessExpression(Node.GetChild(Idx), ChildVal); FgReg := EnsureIntRegister(ChildVal); Inc(Idx);
  end;
  if HasBg then
  begin
    ProcessExpression(Node.GetChild(Idx), ChildVal); BgReg := EnsureIntRegister(ChildVal);
  end;
  Flags := 0;
  if HasFg then Flags := Flags or 1;
  if HasBg then Flags := Flags or 2;
  EmitInstruction(ssaGfxColor, MakeSSAValue(svkNone), FgReg, BgReg, MakeSSAConstInt(Flags));   // Src3=flags -> Immediate
end;

procedure TSSAGenerator.ProcessImageDestroy(Node: TASTNode);
// IMAGEDESTROY handle : free the image surface.
var HVal, HReg: TSSAValue;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 1) then Exit;
  ProcessExpression(Node.GetChild(0), HVal); HReg := EnsureIntRegister(HVal);
  EmitInstruction(ssaGfxImageDestroy, MakeSSAValue(svkNone), HReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.ProcessImageInfo(Node: TASTNode);
// IMAGEINFO handle, w, h : write the image's width/height into the w and h variables. Lowered to two
// synthetic assignments "var = __IMGINFO(handle, which)" reusing the assignment machinery (handle cloned).
var
  Assign, Call, ArgList, WhichLit: TASTNode;
  i: Integer;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 3) then Exit;
  for i := 0 to 1 do
  begin
    WhichLit := TASTNode.CreateWithValue(antLiteral, i, Node.Token);
    ArgList := TASTNode.Create(antArgumentList, Node.Token);
    ArgList.AddChild(Node.GetChild(0).Clone);    // handle (cloned per query)
    ArgList.AddChild(WhichLit);
    Call := TASTNode.CreateWithValue(antGraphicsFunction, '__IMGINFO', Node.Token);
    Call.AddChild(ArgList);
    Assign := TASTNode.Create(antAssignment, Node.Token);
    Assign.AddChild(Node.GetChild(i + 1).Clone); // destination variable (w then h)
    Assign.AddChild(Call);
    ProcessStatement(Assign);
    Assign.Free;
  end;
end;

function TSSAGenerator.EmitGetmouse(Node, ArgListNode: TASTNode): TSSAValue;
// GETMOUSE(x, y [, wheel] [, buttons] [, clip]) : snapshot the mouse once (ssaGetmouse, Dest=status), then
// write each provided lvalue argument from the cached component via a synthetic "var = __MOUSEAXIS(which)"
// assignment (reusing the assignment machinery, exactly like IMAGEINFO/SCREENINFO). An omitted slot is an
// empty antLiteral placeholder and is skipped. Returns the status register (0 = ok, 1 = no mouse/off-window).
var
  StatusReg: TSSAValue;
  Child, WhichLit, InnerArgs, Call, Assign: TASTNode;
  i, MaxArgs: Integer;
begin
  StatusReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaGetmouse, StatusReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) then
  begin
    MaxArgs := ArgListNode.ChildCount;
    if MaxArgs > 5 then MaxArgs := 5;   // x,y,wheel,buttons,clip
    for i := 0 to MaxArgs - 1 do
    begin
      Child := ArgListNode.GetChild(i);
      // An empty argument slot ("GetMouse(x,,b)") parses to a bare antLiteral -> no target for that field.
      if (Child = nil) or (Child.NodeType = antLiteral) then Continue;
      WhichLit := TASTNode.CreateWithValue(antLiteral, i, Node.Token);
      InnerArgs := TASTNode.Create(antArgumentList, Node.Token);
      InnerArgs.AddChild(WhichLit);
      Call := TASTNode.CreateWithValue(antGraphicsFunction, '__MOUSEAXIS', Node.Token);
      Call.AddChild(InnerArgs);
      Assign := TASTNode.Create(antAssignment, Node.Token);
      Assign.AddChild(Child.Clone);   // destination lvalue (x/y/wheel/buttons/clip)
      Assign.AddChild(Call);
      ProcessStatement(Assign);
      Assign.Free;
    end;
  end;
  Result := StatusReg;
end;

function TSSAGenerator.EmitGetJoystick(Node, ArgListNode: TASTNode): TSSAValue;
// GETJOYSTICK(id, buttons, a1 .. a8) : snapshot gaming device `id` (ssaGetJoystick, Src1=id, Dest=status),
// then write each provided lvalue from the cache: buttons (int) via __JOYBTN(), a1..a8 (single) via
// __JOYAXIS(which). Mirrors EmitGetmouse; buttons is index 1, axes are indices 2..9 (which = i-2).
// Returns the status register (0 = ok, 1 = no device -> buttons 0, axes -1000).
var
  StatusReg, IdVal, IdReg: TSSAValue;
  Child, WhichLit, InnerArgs, Call, Assign: TASTNode;
  i, MaxArgs: Integer;
begin
  StatusReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) and (ArgListNode.ChildCount >= 1) then
  begin
    ProcessExpression(ArgListNode.GetChild(0), IdVal);   // id (rvalue input)
    IdReg := EnsureIntRegister(IdVal);
  end
  else
    IdReg := MakeSSAValue(svkNone);
  EmitInstruction(ssaGetJoystick, StatusReg, IdReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  if (ArgListNode <> nil) and (ArgListNode.NodeType = antArgumentList) then
  begin
    MaxArgs := ArgListNode.ChildCount;
    if MaxArgs > 10 then MaxArgs := 10;   // id + buttons + a1..a8
    for i := 1 to MaxArgs - 1 do
    begin
      Child := ArgListNode.GetChild(i);
      if (Child = nil) or (Child.NodeType = antLiteral) then Continue;   // empty slot -> no target
      if i = 1 then
      begin
        Call := TASTNode.CreateWithValue(antGraphicsFunction, '__JOYBTN', Node.Token);  // buttons (int)
        Call.AddChild(TASTNode.Create(antArgumentList, Node.Token));   // empty arg list (ChildCount>0 to be intercepted)
      end
      else
      begin
        WhichLit := TASTNode.CreateWithValue(antLiteral, i - 2, Node.Token);            // axis which (0..7)
        InnerArgs := TASTNode.Create(antArgumentList, Node.Token);
        InnerArgs.AddChild(WhichLit);
        Call := TASTNode.CreateWithValue(antGraphicsFunction, '__JOYAXIS', Node.Token); // axis (single)
        Call.AddChild(InnerArgs);
      end;
      Assign := TASTNode.Create(antAssignment, Node.Token);
      Assign.AddChild(Child.Clone);
      Assign.AddChild(Call);
      ProcessStatement(Assign);
      Assign.Free;
    end;
  end;
  Result := StatusReg;
end;

procedure TSSAGenerator.ProcessGfxSetmouse(Node: TASTNode);
// SETMOUSE [x] [, y] [, visibility] [, clip] : move the mouse and/or set its visibility. Each omitted field
// is -1 ("no change"). v1 wires x, y and visibility to the input provider (clip is parsed but ignored).
// Emitted as Src1=x, Src2=y, Src3=visibility (the compiler packs visibility into Immediate[0-15]).
var
  XR, YR, VR: TSSAValue;

  function ChildRegOrNoChange(Idx: Integer): TSSAValue;
  var V: TSSAValue;
  begin
    if (Node.ChildCount > Idx) and (Node.GetChild(Idx) <> nil) and
       (Node.GetChild(Idx).NodeType <> antLiteral) then
    begin
      ProcessExpression(Node.GetChild(Idx), V);
      Result := EnsureIntRegister(V);
    end
    else
    begin
      Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(-1), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end;
begin
  if FCurrentBlock = nil then Exit;
  XR := ChildRegOrNoChange(0);   // x (-1 = no change)
  YR := ChildRegOrNoChange(1);   // y
  VR := ChildRegOrNoChange(2);   // visibility (1 show, 0 hide, -1 no change)
  EmitInstruction(ssaSetmouse, MakeSSAValue(svkNone), XR, YR, VR);
end;

procedure TSSAGenerator.ProcessGfxGet(Node: TASTNode);
// GET (x1,y1)-(x2,y2), dst : capture a screen rectangle into image surface dst. Children x1,y1,x2,y2,dst.
// Packed as Src1=x1, Src2=y1, Src3=x2, PhiSources[0]=y2, PhiSources[1]=dst handle.
var
  X1V, Y1V, X2V, Y2V, DV, X1R, Y1R, X2R, Y2R, DR: TSSAValue;
  Instr: TSSAInstruction;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 5) then Exit;
  ProcessExpression(Node.GetChild(0), X1V); X1R := EnsureIntRegister(X1V);
  ProcessExpression(Node.GetChild(1), Y1V); Y1R := EnsureIntRegister(Y1V);
  ProcessExpression(Node.GetChild(2), X2V); X2R := EnsureIntRegister(X2V);
  ProcessExpression(Node.GetChild(3), Y2V); Y2R := EnsureIntRegister(Y2V);
  ProcessExpression(Node.GetChild(4), DV);  DR  := EnsureIntRegister(DV);
  EmitInstruction(ssaGfxGet, MakeSSAValue(svkNone), X1R, Y1R, X2R);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Y2R, nil);
  Instr.AddPhiSource(DR, nil);
end;

procedure TSSAGenerator.ProcessGfxPut(Node: TASTNode);
// PUT (x,y), src [, mode] : blit image src onto the screen at (x,y). Children x,y,src; MODE attribute =
// blit-mode ordinal. Packed as Src1=x, Src2=y, Src3=src handle, PhiSources[0]=mode (constant).
var
  XV, YV, SV, XR, YR, SR: TSSAValue;
  Instr: TSSAInstruction;
  Mode: Int64;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 3) then Exit;
  ProcessExpression(Node.GetChild(0), XV); XR := EnsureIntRegister(XV);
  ProcessExpression(Node.GetChild(1), YV); YR := EnsureIntRegister(YV);
  ProcessExpression(Node.GetChild(2), SV); SR := EnsureIntRegister(SV);
  Mode := StrToIntDef(Node.Attributes.Values['MODE'], 0);
  EmitInstruction(ssaGfxPut, MakeSSAValue(svkNone), XR, YR, SR);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(MakeSSAConstInt(Mode), nil);
end;

procedure TSSAGenerator.ProcessScreenInfo(Node: TASTNode);
// SCREENINFO w, h [, depth, bpp, pitch, rate] : write the screen's info into the variables. Lowered to
// synthetic "var = __SCRINFO(which)" assignments (which: 0=w,1=h,2=depth,3=bpp,4=pitch,5=rate).
var
  Assign, Call, ArgList, WhichLit: TASTNode;
  i: Integer;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 1) then Exit;
  for i := 0 to Node.ChildCount - 1 do
  begin
    WhichLit := TASTNode.CreateWithValue(antLiteral, i, Node.Token);
    ArgList := TASTNode.Create(antArgumentList, Node.Token);
    ArgList.AddChild(WhichLit);
    Call := TASTNode.CreateWithValue(antGraphicsFunction, '__SCRINFO', Node.Token);
    Call.AddChild(ArgList);
    Assign := TASTNode.Create(antAssignment, Node.Token);
    Assign.AddChild(Node.GetChild(i).Clone);   // destination variable
    Assign.AddChild(Call);
    ProcessStatement(Assign);
    Assign.Free;
  end;
end;

procedure TSSAGenerator.ProcessScreenSet(Node: TASTNode);
// SCREENSET work[,visible] / FLIP : select the work/visible page. Src1=work, Src2=visible, Src3=flags
// (bit0=hasWork, bit1=hasVisible, bit2=swap). FLIP with no args swaps the pages; FLIP v[,w] sets them
// (note FB's FLIP arg order is visible first, then work). Omitted page regs are loaded with 0.
var
  Op: string;
  WorkReg, VisReg, V0, V1: TSSAValue;
  Flags: Int64;

  function ZeroReg: TSSAValue;
  begin
    Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

begin
  if FCurrentBlock = nil then Exit;
  Op := UpperCase(Node.Attributes.Values['OP']);
  Flags := 0;
  WorkReg := ZeroReg;
  VisReg := ZeroReg;
  if Op = 'FLIP' then
  begin
    if Node.ChildCount = 0 then
      Flags := 4                                   // swap work<->visible
    else
    begin
      ProcessExpression(Node.GetChild(0), V0); VisReg := EnsureIntRegister(V0); Flags := Flags or 2;  // visible
      if Node.ChildCount >= 2 then
      begin
        ProcessExpression(Node.GetChild(1), V1); WorkReg := EnsureIntRegister(V1); Flags := Flags or 1;  // work
      end;
    end;
  end
  else  // SET: work[,visible]
  begin
    if Node.ChildCount >= 1 then
    begin
      ProcessExpression(Node.GetChild(0), V0); WorkReg := EnsureIntRegister(V0); Flags := Flags or 1;
    end;
    if Node.ChildCount >= 2 then
    begin
      ProcessExpression(Node.GetChild(1), V1); VisReg := EnsureIntRegister(V1); Flags := Flags or 2;
    end;
  end;
  EmitInstruction(ssaGfxScreenSet, MakeSSAValue(svkNone), WorkReg, VisReg, MakeSSAConstInt(Flags));
end;

procedure TSSAGenerator.ProcessPCopy(Node: TASTNode);
// PCOPY src,dst / SCREENCOPY [src][,dst] : copy one page onto another. Src1=src, Src2=dst, Src3=flags
// (bit0=hasSrc, bit1=hasDst). An omitted src defaults to the work page, dst to the visible page (VM).
var
  SrcReg, DstReg, V0, V1: TSSAValue;
  Flags: Int64;

  function ZeroReg: TSSAValue;
  begin
    Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

begin
  if FCurrentBlock = nil then Exit;
  Flags := 0;
  SrcReg := ZeroReg;
  DstReg := ZeroReg;
  if Node.ChildCount >= 1 then
  begin
    ProcessExpression(Node.GetChild(0), V0); SrcReg := EnsureIntRegister(V0); Flags := Flags or 1;
  end;
  if Node.ChildCount >= 2 then
  begin
    ProcessExpression(Node.GetChild(1), V1); DstReg := EnsureIntRegister(V1); Flags := Flags or 2;
  end;
  EmitInstruction(ssaGfxPCopy, MakeSSAValue(svkNone), SrcReg, DstReg, MakeSSAConstInt(Flags));
end;

procedure TSSAGenerator.ProcessGfxWindow(Node: TASTNode);
// WINDOW [SCREEN] (x1,y1)-(x2,y2) : set the logical coordinate system (no bounds = disable). Packed as
// Src1=x1, Src2=y1, Src3=x2, PhiSources[0]=y2, PhiSources[1]=flags (bit0=hasBounds, bit1=SCREEN no-flip).
var
  X1V, Y1V, X2V, Y2V, X1R, Y1R, X2R, Y2R: TSSAValue;
  Instr: TSSAInstruction;
  Flags: Int64;

  function ZeroReg: TSSAValue;
  begin
    Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount >= 4 then
  begin
    ProcessExpression(Node.GetChild(0), X1V); X1R := EnsureIntRegister(X1V);
    ProcessExpression(Node.GetChild(1), Y1V); Y1R := EnsureIntRegister(Y1V);
    ProcessExpression(Node.GetChild(2), X2V); X2R := EnsureIntRegister(X2V);
    ProcessExpression(Node.GetChild(3), Y2V); Y2R := EnsureIntRegister(Y2V);
    Flags := 1;                                    // has-bounds
    if Node.Attributes.Values['SCREEN'] = '1' then Flags := Flags or 2;
  end
  else
  begin
    X1R := ZeroReg; Y1R := ZeroReg; X2R := ZeroReg; Y2R := ZeroReg;
    Flags := 0;                                    // disable -> identity
  end;
  EmitInstruction(ssaGfxWindow, MakeSSAValue(svkNone), X1R, Y1R, X2R);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Y2R, nil);
  Instr.AddPhiSource(MakeSSAConstInt(Flags), nil);
end;

procedure TSSAGenerator.ProcessGfxView(Node: TASTNode);
// VIEW [SCREEN] (x1,y1)-(x2,y2) : set the viewport (no bounds = reset). Packed as Src1=x1, Src2=y1,
// Src3=x2, PhiSources[0]=y2, PhiSources[1]=flags (bit0=hasBounds, bit1=SCREEN no-offset). Mirrors WINDOW.
var
  X1V, Y1V, X2V, Y2V, X1R, Y1R, X2R, Y2R: TSSAValue;
  Instr: TSSAInstruction;
  Flags: Int64;

  function ZeroReg: TSSAValue;
  begin
    Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

begin
  if FCurrentBlock = nil then Exit;
  if Node.ChildCount >= 4 then
  begin
    ProcessExpression(Node.GetChild(0), X1V); X1R := EnsureIntRegister(X1V);
    ProcessExpression(Node.GetChild(1), Y1V); Y1R := EnsureIntRegister(Y1V);
    ProcessExpression(Node.GetChild(2), X2V); X2R := EnsureIntRegister(X2V);
    ProcessExpression(Node.GetChild(3), Y2V); Y2R := EnsureIntRegister(Y2V);
    Flags := 1;
    if Node.Attributes.Values['SCREEN'] = '1' then Flags := Flags or 2;
  end
  else
  begin
    X1R := ZeroReg; Y1R := ZeroReg; X2R := ZeroReg; Y2R := ZeroReg;
    Flags := 0;
  end;
  EmitInstruction(ssaGfxView, MakeSSAValue(svkNone), X1R, Y1R, X2R);
  Instr := FCurrentBlock.Instructions[FCurrentBlock.Instructions.Count - 1];
  Instr.AddPhiSource(Y2R, nil);
  Instr.AddPhiSource(MakeSSAConstInt(Flags), nil);
end;

procedure TSSAGenerator.ProcessGfxScreen(Node: TASTNode);
// SCREEN mode [, depth [, num_pages]] : numbered graphics mode. Src1=mode; num_pages (child 2, a
// compile-time constant, default 1) in Src3 -> Immediate. depth (child 1) is accepted-and-ignored.
var
  ModeVal, ModeReg, NVal: TSSAValue;
  NumPages: Int64;
begin
  if (FCurrentBlock = nil) or (Node.ChildCount < 1) then Exit;
  ProcessExpression(Node.GetChild(0), ModeVal); ModeReg := EnsureIntRegister(ModeVal);
  NumPages := 1;
  if Node.ChildCount >= 3 then
  begin
    ProcessExpression(Node.GetChild(2), NVal);
    if NVal.Kind = svkConstInt then NumPages := NVal.ConstInt
    else if NVal.Kind = svkConstFloat then NumPages := Trunc(NVal.ConstFloat);
  end;
  if NumPages < 1 then NumPages := 1;
  EmitInstruction(ssaGfxScreen, MakeSSAValue(svkNone), ModeReg, MakeSSAValue(svkNone), MakeSSAConstInt(NumPages));
end;

procedure TSSAGenerator.ProcessBeep(Node: TASTNode);
// BEEP (FreeBASIC/QB): ring the console bell. Lowered to printing the BEL character (CHR 7) with no
// newline — no dedicated opcode needed. On a terminal this beeps; headless it emits the byte.
var
  R: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;
  R := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
  EmitInstruction(ssaLoadConstString, R, MakeSSAConstString(Chr(7)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  EmitInstruction(ssaPrintString, MakeSSAValue(svkNone), R, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
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
      else if FModernMode then
      begin
        // FreeBASIC: "AS #f" — the handle is a variable holding the file number; evaluate it.
        ProcessExpression(HandleChild, HandleVal);
        HandleReg := EnsureIntRegister(HandleVal);
      end
      else
      begin
        // Legacy (CLASSIC) named handle: #MYFILE - placeholder handle 0.
        HandleReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, HandleReg, MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
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
      else if FModernMode then
      begin
        // FreeBASIC: the handle is a variable holding the file number; evaluate it.
        ProcessExpression(HandleChild, HandleVal);
        HandleReg := EnsureIntRegister(HandleVal);
      end
      else
      begin
        // Legacy named handle: #MYFILE - placeholder 0.
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

  // FreeBASIC binary GET #n, [pos], var: read sizeof(var) bytes into a scalar (int/float v1). The
  // handle child is clean (antLiteral number or antIdentifier variable, no "#N" merge).
  if Node.Attributes.Values['BIN'] = '1' then
  begin
    ProcessExpression(Node.GetChild(0), HandleVal);
    HandleReg := EnsureIntRegister(HandleVal);
    if (Node.Attributes.Values['HASPOS'] = '1') and (Node.ChildCount >= 3) then
    begin
      ProcessExpression(Node.GetChild(2), HandleVal);   // optional position: seek first
      EmitInstruction(ssaSeekSet, MakeSSAValue(svkNone), HandleReg, EnsureIntRegister(HandleVal), MakeSSAValue(svkNone));
    end;
    VarChild := Node.GetChild(1);
    VarReg := GetOrAllocateVariable(string(VarChild.Value));
    if VarReg.RegType = srtFloat then
      EmitInstruction(ssaGetBinFloat, VarReg, HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else if VarReg.RegType = srtString then
      EmitInstruction(ssaGetBinStr, VarReg, HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaGetBinInt, VarReg, HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;

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
    else if FModernMode then
    begin
      ProcessExpression(HandleChild, HandleVal);   // FreeBASIC: handle is a variable
      HandleReg := EnsureIntRegister(HandleVal);
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
      // LINE INPUT# reads a whole line (no comma split) into a string variable; plain INPUT# reads a
      // comma/newline-delimited field (typed int/float/string by the compiler from the var's bank).
      if Node.Attributes.Values['LINEINPUT'] = '1' then
        EmitInstruction(ssaInputFileLine, VarReg, HandleReg,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaInputFile, VarReg, HandleReg,
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    // Skip separators and other nodes
  end;
end;

procedure TSSAGenerator.EmitWriteFileValues(Node: TASTNode; const HandleReg: TSSAValue; ToConsole: Boolean);
// FreeBASIC WRITE #n, v1, v2, ...: write each value comma-separated; strings are wrapped in double
// quotes; numbers use their plain string form; a trailing newline terminates the record (so a matching
// INPUT# reads the fields back). Children 1+ are values (separators are ignored — WRITE always uses ',').
var
  i, vc: Integer;
  Child: TASTNode;
  ExprVal, StrReg: TSSAValue;

  procedure EmitStr(const R: TSSAValue);   // write one string register to the file or the console
  begin
    if ToConsole then
      EmitInstruction(ssaPrintString, MakeSSAValue(svkNone), R, MakeSSAValue(svkNone), MakeSSAValue(svkNone))
    else
      EmitInstruction(ssaPrintFile, R, HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;

  procedure EmitLit(const S: string);
  var R: TSSAValue;
  begin
    R := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
    EmitInstruction(ssaLoadConstString, R, MakeSSAConstString(S), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitStr(R);
  end;

begin
  vc := 0;
  for i := 1 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Child.NodeType = antSeparator then Continue;
    if vc > 0 then EmitLit(',');
    ProcessExpression(Child, ExprVal);
    if (ExprVal.Kind = svkConstString) or ((ExprVal.Kind = svkRegister) and (ExprVal.RegType = srtString)) then
    begin
      EmitLit('"');
      EmitStr(EnsureStringRegister(ExprVal));
      EmitLit('"');
    end
    else if (ExprVal.Kind = svkConstInt) or ((ExprVal.Kind = svkRegister) and (ExprVal.RegType = srtInt)) then
    begin
      StrReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
      EmitInstruction(ssaIntToString, StrReg, EnsureIntRegister(ExprVal), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitStr(StrReg);
    end
    else
    begin
      StrReg := MakeSSARegister(srtString, FProgram.AllocRegister(srtString));
      EmitInstruction(ssaFloatToString, StrReg, EnsureFloatRegister(ExprVal), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      EmitStr(StrReg);
    end;
    Inc(vc);
  end;
  if ToConsole then
    EmitInstruction(ssaPrintNewLine, MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else
    EmitInstruction(ssaPrintFileNewLine, MakeSSAValue(svkNone), HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
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
    else if FModernMode then
    begin
      ProcessExpression(HandleChild, HandleVal);   // FreeBASIC: handle is a variable
      HandleReg := EnsureIntRegister(HandleVal);
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

  // FreeBASIC SEEK #n, pos: set the 1-based file position (reuses the antPrintFile node, tagged SEEK).
  if Node.Attributes.Values['SEEK'] = '1' then
  begin
    if Node.ChildCount >= 2 then
    begin
      ProcessExpression(Node.GetChild(1), ExprVal);
      EmitInstruction(ssaSeekSet, MakeSSAValue(svkNone), HandleReg,
                     EnsureIntRegister(ExprVal), MakeSSAValue(svkNone));
    end;
    Exit;
  end;

  // FreeBASIC binary PUT #n, [pos], var: write sizeof(var) bytes of a scalar (int/float v1).
  if Node.Attributes.Values['PUTBIN'] = '1' then
  begin
    if (Node.Attributes.Values['HASPOS'] = '1') and (Node.ChildCount >= 3) then
    begin
      ProcessExpression(Node.GetChild(2), ExprVal);   // optional position: seek first
      EmitInstruction(ssaSeekSet, MakeSSAValue(svkNone), HandleReg, EnsureIntRegister(ExprVal), MakeSSAValue(svkNone));
    end;
    if Node.ChildCount >= 2 then
    begin
      ProcessExpression(Node.GetChild(1), ExprVal);   // the value to write
      if (ExprVal.Kind = svkConstFloat) or ((ExprVal.Kind = svkRegister) and (ExprVal.RegType = srtFloat)) then
        EmitInstruction(ssaPutBinFloat, MakeSSAValue(svkNone), HandleReg, EnsureFloatRegister(ExprVal), MakeSSAValue(svkNone))
      else if (ExprVal.Kind = svkConstString) or ((ExprVal.Kind = svkRegister) and (ExprVal.RegType = srtString)) then
        EmitInstruction(ssaPutBinStr, MakeSSAValue(svkNone), HandleReg, EnsureStringRegister(ExprVal), MakeSSAValue(svkNone))
      else
        EmitInstruction(ssaPutBinInt, MakeSSAValue(svkNone), HandleReg, EnsureIntRegister(ExprVal), MakeSSAValue(svkNone));
    end;
    Exit;
  end;

  // FreeBASIC WRITE #n, ...: comma-separated values, strings quoted, then a newline.
  if Node.Attributes.Values['WRITE'] = '1' then
  begin
    EmitWriteFileValues(Node, HandleReg, False);
    Exit;
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
    else if FModernMode then
    begin
      ProcessExpression(HandleChild, HandleVal);   // FreeBASIC: handle is a variable
      HandleReg := EnsureIntRegister(HandleVal);
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

  // FreeBASIC "DELETE p": the MODERN parser attaches the pointer expression (an identifier) as child0.
  // The classic line-delete form attaches line-number literals instead, so an antIdentifier child0
  // unambiguously selects the FB form.
  if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
  begin
    EmitDeleteObject(Node);
    Exit;
  end;

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

procedure TSSAGenerator.ProcessRmdir(Node: TASTNode);
var
  PathVal: TSSAValue;
  PathReg: TSSAValue;
begin
  if FCurrentBlock = nil then Exit;

  // RMDIR "path"
  if Node.ChildCount < 1 then
    raise Exception.Create('RMDIR requires a path');

  ProcessExpression(Node.GetChild(0), PathVal);
  PathReg := EnsureStringRegister(PathVal);

  EmitInstruction(ssaRmdir, MakeSSAValue(svkNone), PathReg,
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
      if OpIn(Instr.OpCode, [ssaJump, ssaCall, ssaCallSub, ssaJumpIfZero, ssaJumpIfNotZero]) and (Instr.Dest.Kind = svkLabel) then
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
        // A user GOTO/GOSUB/ON ... (or CALL) to a line number, label or procedure that
        // was never defined. Without this guard the unresolved label resolves to PC 0 at
        // bytecode time, sending control back to the program start: that turns a stray
        // GOSUB into an unbounded recursion that overflows the return stack (access
        // violation). Internal compiler labels never carry these prefixes, so the prefix
        // filter limits the diagnostic to genuine user targets.
        else if (Copy(TargetLabel, 1, 5) = 'LINE_') then
          raise Exception.CreateFmt('Undefined line number: %s', [Copy(TargetLabel, 6, MaxInt)])
        else if (Copy(TargetLabel, 1, 6) = 'LABEL_') then
          raise Exception.CreateFmt('Undefined label: %s', [Copy(TargetLabel, 7, MaxInt)])
        else if (Copy(TargetLabel, 1, 5) = 'PROC_') then
          raise Exception.CreateFmt('Undefined procedure: %s', [Copy(TargetLabel, 6, MaxInt)])
        {$IFDEF DEBUG_SSA}
        else if DebugSSA then
          WriteLn('[SSA] WARNING: Target block not found: ', TargetLabel);
        {$ENDIF}
        ;
      end;

      // M5.2: @sub (ssaLoadProcAddr) takes a procedure's address (PROC_<name> label in Src1).
      // Add a CFG edge to the proc block so an address-taken-only worker SUB stays reachable
      // (otherwise DBE removes it and the label never resolves → bcThreadCreate would spawn at PC 0).
      // The edge is for liveness only; the current thread never branches there (bcThreadCreate falls
      // through), and the worker enters via its own context StartPC.
      if (Instr.OpCode = ssaLoadProcAddr) and (Instr.Src1.Kind = svkLabel) then
      begin
        TargetLabel := Instr.Src1.LabelName;
        TargetBlock := FProgram.FindBlock(TargetLabel);
        if Assigned(TargetBlock) then
        begin
          if Block.Successors.IndexOf(TargetBlock) = -1 then
          begin
            Block.AddSuccessor(TargetBlock);
            TargetBlock.AddPredecessor(Block);
          end;
        end
        else if (Copy(TargetLabel, 1, 5) = 'PROC_') then
          raise Exception.CreateFmt('Undefined procedure (address-of @): %s', [Copy(TargetLabel, 6, MaxInt)]);
      end;

      // FreeBASIC error handling: ssaOnError (ON ERROR GOTO label) / ssaResumeLabel (RESUME label)
      // carry the target label in Src1. We do NOT add a CFG edge here: the handler block is reached
      // dynamically (on error / RESUME), exactly like a TRAP handler, so it is kept alive by DBE
      // (referenced-label preservation) and the dominator validation tolerates its missing
      // predecessor (HasTrap relaxation). Adding a successor edge here would instead give a
      // non-terminator block >1 successors and fail CFG validation. We only diagnose a missing target.
      if OpIn(Instr.OpCode, [ssaOnError, ssaResumeLabel]) and (Instr.Src1.Kind = svkLabel) then
      begin
        TargetLabel := Instr.Src1.LabelName;
        if not Assigned(FProgram.FindBlock(TargetLabel)) then
        begin
          if (Copy(TargetLabel, 1, 6) = 'LABEL_') then
            raise Exception.CreateFmt('Undefined error-handler label: %s', [Copy(TargetLabel, 7, MaxInt)])
          else if (Copy(TargetLabel, 1, 5) = 'LINE_') then
            raise Exception.CreateFmt('Undefined error-handler line: %s', [Copy(TargetLabel, 6, MaxInt)]);
        end;
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

function TSSAGenerator.NarrowConstInt(Value: Int64; WidthCode: Integer): Int64;
// B1.5 compile-time fold of bcNarrowInt: 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32.
begin
  case WidthCode of
    1: Result := Int64(ShortInt(Value and $FF));
    2: Result := Value and $FF;
    3: Result := Int64(SmallInt(Value and $FFFF));
    4: Result := Value and $FFFF;
    5: Result := Int64(LongInt(Value and $FFFFFFFF));
    6: Result := Value and $FFFFFFFF;
  else
    Result := Value;
  end;
end;

function TSSAGenerator.TypeNameWidthCode(const TypeName: string): Integer;
// Map a declared type name to a narrowing width code for STORE narrowing (B1.5). 0 = no narrowing.
// 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32 (Long/ULong are 32-bit in FB).
// INTEGER/LONGINT (=64-bit here), UINTEGER/ULONGINT and DOUBLE need no bit narrowing.
// NOTE: SINGLE is intentionally NOT narrowed on store (returns 0): rounding a SINGLE-typed variable
// to single precision is faithful for storage but exposes the representation error in PRINT (we lack
// single-precision display formatting, so 1.65 would show as 1.6499...). True single-precision storage
// is deferred until print formats single-typed values with single precision. The explicit CSNG()
// conversion still rounds to single (separate path), as the user asked for it explicitly.
var
  T: string;
begin
  T := CanonicalType(TypeName);   // resolve FB TYPE-alias before the builtin width match
  if T = 'BYTE' then Result := 1
  else if T = 'UBYTE' then Result := 2
  else if T = 'SHORT' then Result := 3
  else if T = 'USHORT' then Result := 4
  else if T = 'LONG' then Result := 5
  else if T = 'ULONG' then Result := 6
  else Result := 0;
end;

procedure TSSAGenerator.RecordVarWidth(const VarName, TypeName: string);
// Remember a declared scalar's narrowing width (so stores wrap to the declared type) and its print
// form (BOOLEAN -> true/false, UInteger/ULongInt -> unsigned-64).
var
  W, Idx, PK: Integer;
  Nm, T: string;
begin
  Nm := UpperCase(VarName);
  T := UpperCase(TypeName);
  // Narrowing width (phase 2).
  W := TypeNameWidthCode(TypeName);
  Idx := FVarWidthCode.IndexOf(Nm);
  if W = 0 then
  begin
    if Idx >= 0 then FVarWidthCode.Delete(Idx);  // re-DIM to a wide type clears any prior narrowing
  end
  else if Idx >= 0 then
    FVarWidthCode.Objects[Idx] := TObject(PtrInt(W))
  else
    FVarWidthCode.AddObject(Nm, TObject(PtrInt(W)));
  // Print form (phase C). UByte/UShort/ULong are stored as positive Int64 after narrowing, so they
  // print correctly as signed already; only the 64-bit unsigned types need the unsigned print form.
  if T = 'BOOLEAN' then PK := 1
  else if (T = 'UINTEGER') or (T = 'ULONGINT') then PK := 2
  else PK := 0;
  Idx := FVarPrintKind.IndexOf(Nm);
  if PK = 0 then
  begin
    if Idx >= 0 then FVarPrintKind.Delete(Idx);
  end
  else if Idx >= 0 then
    FVarPrintKind.Objects[Idx] := TObject(PtrInt(PK))
  else
    FVarPrintKind.AddObject(Nm, TObject(PtrInt(PK)));
end;

function TSSAGenerator.ApplyScalarNarrow(const VarName: string; Value: TSSAValue): TSSAValue;
// On store to a narrow-typed scalar, narrow the value to the declared width. No-op for untracked vars.
var
  Idx: Integer;
begin
  Idx := FVarWidthCode.IndexOf(UpperCase(VarName));
  if Idx < 0 then Exit(Value);
  Result := ApplyNarrowCode(PtrInt(FVarWidthCode.Objects[Idx]), Value);
end;

function TSSAGenerator.ApplyNarrowCode(W: Integer; Value: TSSAValue): TSSAValue;
// Wrap/sign-extend (int widths 1..6) or round to single (7) a value before a store to a narrow-typed
// destination. Folds constants; otherwise emits bcNarrowInt / bcNarrowSingle. W=0 -> no-op.
var
  NarrowReg: TSSAValue;
begin
  Result := Value;
  if W = 7 then
  begin
    // SINGLE: round a float value to single precision.
    if Value.Kind = svkConstFloat then
      Result := MakeSSAConstFloat(Single(Value.ConstFloat))
    else if (Value.Kind = svkRegister) and (Value.RegType = srtFloat) then
    begin
      NarrowReg := MakeSSARegister(srtFloat, FProgram.AllocRegister(srtFloat));
      EmitInstruction(ssaNarrowSingle, NarrowReg, Value, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Result := NarrowReg;
    end;
  end
  else if (W >= 1) and (W <= 6) then
  begin
    // Integer widths: wrap/sign-extend. A float value first truncates to int (assignment to an
    // integer-typed scalar), then narrows.
    if Value.Kind = svkConstInt then
      Result := MakeSSAConstInt(NarrowConstInt(Value.ConstInt, W))
    else if Value.Kind = svkConstFloat then
      Result := MakeSSAConstInt(NarrowConstInt(Trunc(Value.ConstFloat), W))
    else if (Value.Kind = svkRegister) and (Value.RegType = srtInt) then
    begin
      NarrowReg := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaNarrowInt, NarrowReg, Value, MakeSSAValue(svkNone), MakeSSAConstInt(W));
      Result := NarrowReg;
    end;
    // A float register reaching here is handled by the existing store conversion (float->int), so
    // narrowing it would require an extra int temp; deferred (rare: DIM b AS BYTE = floatExpr).
  end;
end;

function TSSAGenerator.TypeNameToBank(const TypeName, FieldName: string): TSSARegisterType;
// Map a declared field/var type name to a register bank. Empty type => infer by name suffix.
var
  T: string;
begin
  T := UpperCase(TypeName);
  if T = '' then
    Exit(GetVariableType(FieldName));
  // FreeBASIC pointer "<type> PTR": stored as an int handle (the address).
  if (Length(T) >= 4) and (Copy(T, Length(T) - 3, 4) = ' PTR') then
    Exit(srtInt);
  T := CanonicalType(T);   // resolve FB TYPE-alias (e.g. "int32" -> "LONG") before the builtin match
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

function TSSAGenerator.CanonicalType(const TypeName: string): string;
// Resolve a FreeBASIC "TYPE alias AS underlying" chain to its base type name. A non-alias name is
// returned unchanged (UPPER). Guarded against accidental alias cycles. PTR-suffixed names are not
// aliased here (handle types are resolved at their own call sites).
var
  T, Next: string;
  Guard, Idx: Integer;
begin
  T := UpperCase(TypeName);
  if FTypeAliases.Count = 0 then Exit(T);
  Guard := 0;
  while Guard < 32 do
  begin
    Idx := FTypeAliases.IndexOfName(T);
    if Idx < 0 then Break;
    Next := UpperCase(FTypeAliases.ValueFromIndex[Idx]);
    if (Next = '') or (Next = T) then Break;
    T := Next;
    Inc(Guard);
  end;
  Result := T;
end;

function TSSAGenerator.FindUDT(const TypeName: string): Integer;
var
  i: Integer;
  U: string;
begin
  U := CanonicalType(TypeName);
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

function TSSAGenerator.UDTFieldWidthCode(UDTIdx: Integer; const FieldName: string): Integer;
// B1.5: the narrow width code of a field (0 = full width / not found).
var
  i: Integer;
  F: string;
begin
  Result := 0;
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  F := UpperCase(FieldName);
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
    if FUDTs[UDTIdx].Fields[i].Name = F then Exit(FUDTs[UDTIdx].Fields[i].WidthCode);
end;

function TSSAGenerator.UDTFieldPtrPointee(UDTIdx: Integer; const FieldName: string): string;
// The pointee UDT type of a "T PTR" field (held as an int handle), or '' — used to resolve chained
// pointer-field access such as "node->nxt->val".
var
  i: Integer;
  F: string;
begin
  Result := '';
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  F := UpperCase(FieldName);
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
    if FUDTs[UDTIdx].Fields[i].Name = F then Exit(FUDTs[UDTIdx].Fields[i].PtrPointee);
end;

function TSSAGenerator.UDTFieldIsWString(UDTIdx: Integer; const FieldName: string): Boolean;
// True if the named field is declared AS WSTRING (so obj.field LEN/MID index by codepoint).
var
  i: Integer;
  F: string;
begin
  Result := False;
  if (UDTIdx < 0) or (UDTIdx > High(FUDTs)) then Exit;
  F := UpperCase(FieldName);
  for i := 0 to High(FUDTs[UDTIdx].Fields) do
    if FUDTs[UDTIdx].Fields[i].Name = F then Exit(FUDTs[UDTIdx].Fields[i].IsWString);
end;

procedure TSSAGenerator.RegisterUDTs(Node: TASTNode);
// Two passes so a TYPE may reference another TYPE declared later (forward reference).
begin
  EnsureObjectBaseType;    // OOP: the built-in OBJECT base type must exist before TYPE names are collected
  CollectUDTNames(Node);   // pass 1: all type names known
  FillUDTFields(Node);     // pass 2: resolve fields (incl. nested-UDT fields)
end;

procedure TSSAGenerator.EnsureObjectBaseType;
// FreeBASIC OBJECT: the built-in base type that provides RTTI. We model it as an empty UDT (no fields)
// so that "TYPE X EXTENDS Object" has a real (harmless) base, "X IS Object" is true for any derived
// type (IsSubtypeOf walks to OBJECT), and "DIM v AS Object" declares a generic object handle. Our
// virtual dispatch is type-id based, so no actual vtable pointer field is needed.
var
  n: Integer;
begin
  if FindUDT('OBJECT') >= 0 then Exit;
  n := Length(FUDTs);
  SetLength(FUDTs, n + 1);
  FUDTs[n].Name := 'OBJECT';
  SetLength(FUDTs[n].Fields, 0);
  FUDTs[n].NInt := 0; FUDTs[n].NFloat := 0; FUDTs[n].NStr := 0;
  FUDTs[n].Parent := '';
  FUDTs[n].Node := nil;
  FUDTs[n].Filled := True;     // empty: nothing to fill
  FUDTs[n].IsUnion := False;
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
    // FreeBASIC "TYPE alias AS underlying": a one-line type synonym, not a record. Register it in
    // the alias map (resolved via CanonicalType at the type resolvers) and do not create a UDT.
    if Node.Attributes.Values['ALIAS'] <> '' then
    begin
      if FTypeAliases.IndexOfName(Name) < 0 then
        FTypeAliases.Values[Name] := UpperCase(Node.Attributes.Values['ALIAS']);
      Exit;
    end;
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
      FUDTs[n].IsUnion := (Node.Attributes.Values['UNION'] = '1');  // UNION: overlap same-bank fields
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
  TypeName, FieldName, NestedT, PtrPointeeT: string;
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
      if FieldNode.Attributes.Values['STATIC'] = '1' then Continue;  // static member: backed by a global, no per-instance slot
      FieldName := VarToStr(FieldNode.Value);
      TypeNode := nil;
      if FieldNode.ChildCount > 0 then TypeNode := FieldNode.GetChild(0);
      TypeName := '';
      if Assigned(TypeNode) then TypeName := UpperCase(VarToStr(TypeNode.Value));
      NestedT := '';
      PtrPointeeT := '';
      if (TypeName <> '') and (FindUDT(TypeName) >= 0) then
      begin
        Bank := srtInt;        // nested record field: int handle to the nested instance
        NestedT := TypeName;
      end
      else if TypeName <> '' then
      begin
        Bank := TypeNameToBank(TypeName, FieldName);
        // A "T PTR" field where T is a UDT holds an int handle to a heap record. Record the pointee so
        // chained pointer-field access (p->nxt->val) and p->ptrfield-> resolve correctly.
        if (Length(TypeName) > 4) and (Copy(TypeName, Length(TypeName) - 3, 4) = ' PTR') and
           (FindUDT(Trim(Copy(TypeName, 1, Length(TypeName) - 4))) >= 0) then
          PtrPointeeT := Trim(Copy(TypeName, 1, Length(TypeName) - 4));
      end
      else
        Bank := GetVariableType(FieldName);
      n := Length(FUDTs[Idx].Fields);
      SetLength(FUDTs[Idx].Fields, n + 1);
      FUDTs[Idx].Fields[n].Name := UpperCase(FieldName);
      FUDTs[Idx].Fields[n].Bank := Bank;
      FUDTs[Idx].Fields[n].NestedType := NestedT;
      FUDTs[Idx].Fields[n].PtrPointee := PtrPointeeT;
      FUDTs[Idx].Fields[n].IsWString := (TypeName = 'WSTRING');  // codepoint LEN/MID on obj.field
      if NestedT = '' then
        FUDTs[Idx].Fields[n].WidthCode := TypeNameWidthCode(TypeName)  // B1.5: narrow field on store
      else
        FUDTs[Idx].Fields[n].WidthCode := 0;
      if FUDTs[Idx].IsUnion then
        // UNION: every member of a bank overlaps at slot 0 of that bank (faithful same-bank
        // aliasing — write one member, read another of the same type back). The per-bank count
        // is at most 1 slot. Cross-bank reinterpretation (int<->float bytes) is not modelled.
        case Bank of
          srtFloat:  begin FUDTs[Idx].Fields[n].Slot := 0; if cFloat < 1 then cFloat := 1; end;
          srtString: begin FUDTs[Idx].Fields[n].Slot := 0; if cStr   < 1 then cStr   := 1; end;
        else
          begin FUDTs[Idx].Fields[n].Slot := 0; if cInt < 1 then cInt := 1; end;
        end
      else
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

function TSSAGenerator.BankToChar(Bank: TSSARegisterType): Char;
// M4.4g: one-char code of a register bank for a constructor type signature.
begin
  case Bank of
    srtFloat:  Result := 'F';
    srtString: Result := 'S';
  else
    Result := 'I';   // int (incl. UDT handles)
  end;
end;

function TSSAGenerator.CtorSigFromParams(ParamList: TASTNode): string;
// M4.4g: the type signature of a constructor's explicit parameters (THIS at index 0 excluded), one
// bank char per param, e.g. "II", "IS", "" (no params). A UDT-typed param is an int handle ('I').
var
  i: Integer;
  p: TASTNode;
  tname: string;
begin
  Result := '';
  if ParamList = nil then Exit;
  for i := 1 to ParamList.ChildCount - 1 do          // skip the implicit THIS at 0
  begin
    p := ParamList.GetChild(i);
    // The type child is at index 0 when present; for an untyped "param = default" the only child is
    // the default expression, so skip it (HASDEFAULT + single child => no type).
    tname := '';
    if (p.ChildCount >= 1) and (p.GetChild(0).NodeType = antIdentifier) and
       not ((p.Attributes.Values['HASDEFAULT'] = '1') and (p.ChildCount = 1)) then
      tname := UpperCase(VarToStr(p.GetChild(0).Value));
    if FindUDT(tname) >= 0 then
      Result := Result + 'I'                          // UDT handle
    else
      Result := Result + BankToChar(TypeNameToBank(tname, VarToStr(p.Value)));
  end;
end;

function TSSAGenerator.ResolveConstructorLabel(const TypeName, ArgSig: string): string;
// M4.4g: find a constructor by TYPE SIGNATURE, walking up the inheritance chain. Each CONSTRUCTOR's
// label encodes its parameter bank signature as "TYPE.CONSTRUCTOR#<sig>" (e.g. "#II", "#IS", "#"),
// so same-arity/different-type overloads coexist. Resolution: exact signature match first; if none,
// fall back to ARITY (BASIC's loose typing — an int arg may target a float/string-prefixed param,
// coerced at staging): the first ctor of the same parameter count. A subtype with no matching ctor
// inherits the parent's.
var
  T, Lbl, Pref: string;
  Idx, Guard, k: Integer;
begin
  Result := '';
  T := UpperCase(TypeName);
  Guard := 0;
  while (T <> '') and (Guard < 64) do
  begin
    // 1) exact signature
    Lbl := T + '.CONSTRUCTOR#' + ArgSig;
    if FProcDecls.ContainsKey(Lbl) then Exit(Lbl);
    // 2) arity fallback: first ctor of T with the same parameter count (= length of its signature)
    Pref := T + '.CONSTRUCTOR#';
    for k := 0 to FProcedureNames.Count - 1 do
      if (Copy(FProcedureNames[k], 1, Length(Pref)) = Pref) and
         (Length(FProcedureNames[k]) - Length(Pref) = Length(ArgSig)) then
        Exit(FProcedureNames[k]);
    Idx := FindUDT(T);
    if Idx < 0 then Break;
    T := FUDTs[Idx].Parent;
    Inc(Guard);
  end;
end;

function TSSAGenerator.FindCtorWithDefaults(const TypeName: string; ArgCount: Integer): string;
// M4.4h: find a constructor (walking inheritance) callable with ArgCount arguments thanks to default
// parameters — i.e. it has M >= ArgCount parameters and every parameter beyond the ArgCount-th carries
// a default value (HASDEFAULT). Prefers the fewest parameters (closest match). Used as a last resort
// after exact-signature and arity resolution fail. The caller fills the missing defaults.
var
  T, Pref, Lbl, Best: string;
  Idx, Guard, k, j, MParams, BestM: Integer;
  Decl, ParamList, pj: TASTNode;
  ok: Boolean;
begin
  Result := '';
  T := UpperCase(TypeName);
  Guard := 0;
  while (T <> '') and (Guard < 64) do
  begin
    Pref := T + '.CONSTRUCTOR#';
    Best := ''; BestM := MaxInt;
    for k := 0 to FProcedureNames.Count - 1 do
    begin
      Lbl := FProcedureNames[k];
      if Copy(Lbl, 1, Length(Pref)) <> Pref then Continue;
      if not (FProcDecls.TryGetValue(Lbl, Decl) and Assigned(Decl) and (Decl.ChildCount >= 2)) then Continue;
      ParamList := Decl.GetChild(1);
      MParams := ParamList.ChildCount - 1;            // explicit params (THIS excluded)
      if MParams < ArgCount then Continue;            // not enough parameters even with no defaults
      ok := True;                                     // every param beyond ArgCount must be defaulted
      for j := ArgCount to MParams - 1 do
      begin
        pj := ParamList.GetChild(j + 1);              // +1: skip THIS
        if pj.Attributes.Values['HASDEFAULT'] <> '1' then begin ok := False; Break; end;
      end;
      if ok and (MParams < BestM) then begin Best := Lbl; BestM := MParams; end;
    end;
    if Best <> '' then Exit(Best);
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
  // M6: a dispatcher only forwards (it never holds the shared-global registers), so its internal
  // bcCallSub must NOT emit shared sync — that would clobber the slots the real caller already set.
  FInDispatcher := True;
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
  FInDispatcher := False;
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
      // VAR x = expr (FreeBASIC type inference): child[1] is the initializer expression, NOT a type.
      // REWRITE it here into the exact shape of a typed scalar DIM — antArrayDecl(name, <typeIdent>, expr)
      // — by inferring the bank and inserting the corresponding type name as a new child[1] (the init
      // expression shifts to child[2]). After this the node is indistinguishable from "DIM x AS T = expr",
      // so every downstream pass (pre-allocation, ProcessDim, regalloc) handles it identically — no
      // VAR-specific code path. This pre-pass runs in source order before pre-allocation, so an earlier
      // VAR's inferred type is visible to a later VAR that references it. Idempotent (INFER cleared).
      if Decl.Attributes.Values['INFER'] = '1' then
      begin
        if (Decl.ChildCount >= 2) and (Decl.GetChild(0).NodeType = antIdentifier) then
        begin
          VarName := UpperCase(VarToStr(Decl.GetChild(0).Value));
          case InferExprBank(Decl.GetChild(1)) of
            srtString: TypeName := 'STRING';
            srtInt:    TypeName := 'INTEGER';
          else
            TypeName := 'DOUBLE';
          end;
          Decl.InsertChild(1, TASTNode.CreateWithValue(antIdentifier, TypeName, Decl.GetChild(0).Token));
          Decl.Attributes.Values['INFER'] := '0';
          RegisterTypedVar(VarName, TypeName);
        end;
        Continue;
      end;
      // DIM BYREF r AS T = target: a reference variable. Its register carries target's address (int);
      // record the pointee type in FRefVars (drives the auto-deref bank) and force r's bank to INT, so
      // pre-allocation gives it an int register. The "@target" init (child[2]) backs the target via the
      // address-taken pre-pass automatically.
      if (Decl.Attributes.Values['BYREF'] = '1') and (Decl.ChildCount >= 2) and
         (Decl.GetChild(0).NodeType = antIdentifier) and (Decl.GetChild(1).NodeType = antIdentifier) then
      begin
        VarName := UpperCase(VarToStr(Decl.GetChild(0).Value));
        TypeName := UpperCase(VarToStr(Decl.GetChild(1).Value));
        if FRefVars.IndexOfName(VarName) < 0 then
          FRefVars.Add(VarName + '=' + TypeName);
        RegisterTypedVar(VarName, 'INTEGER');   // r holds an address -> int register
        Continue;
      end;
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
        // M7: a parameter with a default value carries the default expression as its last child. The
        // type child (from "AS type") is present at index 0 only when there is more than just the
        // default — i.e. skip the type read for an untyped "param = expr".
        if (ParamNode.NodeType = antIdentifier) and (ParamNode.ChildCount >= 1) and
           (ParamNode.GetChild(0).NodeType = antIdentifier) and
           not ((ParamNode.Attributes.Values['HASDEFAULT'] = '1') and (ParamNode.ChildCount = 1)) then
        begin
          VarName := UpperCase(VarToStr(ParamNode.Value));
          TypeName := UpperCase(VarToStr(ParamNode.GetChild(0).Value));
          // A BYREF scalar param of a BYREF-return FUNCTION is an address carrier (it holds the caller
          // variable's address). Register it as INT regardless of its declared (float/string) type, so
          // its register/transfer-slot are int (an address); the declared type is used only to type the
          // auto-deref (FCurrentProcAddrParams, set in the prologue).
          if (Node.Attributes.Values['BYREFRET'] = '1') and
             (ParamNode.Attributes.Values['BYREF'] = '1') and
             (FindUDT(TypeName) < 0) then
            RegisterTypedVar(VarName, 'INTEGER')
          else if TypeName <> '' then RegisterTypedVar(VarName, TypeName);
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
// M4.4d/g: constructors are overloaded by TYPE SIGNATURE (with arity fallback): the arguments are
// evaluated first to build their bank signature, then the matching ctor is resolved
// (ResolveConstructorLabel) and the argument values are staged into its parameter slots (coerced to
// each parameter's bank). Evaluating before staging also avoids transfer-slot clobber from a nested
// call inside an argument expression.
var
  Lbl, ArgSig: string;
  Decl, ParamList, PNode: TASTNode;
  i, Slot, ArgCount: Integer;
  RT: TSSARegisterType;
  ArgVals: array of TSSAValue;
  DefVal: TSSAValue;
begin
  if Assigned(ArgsNode) then ArgCount := ArgsNode.ChildCount else ArgCount := 0;
  // Evaluate each argument once and accumulate its bank signature.
  SetLength(ArgVals, ArgCount);
  ArgSig := '';
  for i := 0 to ArgCount - 1 do
  begin
    ProcessExpression(ArgsNode.GetChild(i), ArgVals[i]);
    ArgSig := ArgSig + BankToChar(ArgVals[i].RegType);
  end;
  Lbl := ResolveConstructorLabel(TypeName, ArgSig);
  // M4.4h: if no ctor matches the given count, try one that is callable via default parameters.
  if Lbl = '' then Lbl := FindCtorWithDefaults(TypeName, ArgCount);
  if Lbl = '' then Exit;
  EmitXferStore(srtInt, 0, HandleVal);              // THIS handle -> int xfer slot 0
  if FProcDecls.TryGetValue(Lbl, Decl) and Assigned(Decl) and (Decl.ChildCount >= 2) then
  begin
    ParamList := Decl.GetChild(1);                  // includes the implicit THIS at index 0
    for i := 0 to ArgCount - 1 do
    begin
      if i + 1 >= ParamList.ChildCount then Break;  // defensive: arity already matched the resolution
      Slot := ParamBankAndSlot(ParamList, i + 1, RT);  // +1: skip the implicit THIS parameter
      EmitXferStore(RT, Slot, ArgVals[i]);          // coerced to the parameter's bank
    end;
    // M4.4h: fill any trailing parameters the call omitted with their default values (evaluated here,
    // in the caller's context), coerced to each parameter's bank — like M7 for SUB/FUNCTION. The
    // parameter at ParamList index k corresponds to the (k-1)-th explicit argument (THIS is index 0).
    for i := ArgCount + 1 to ParamList.ChildCount - 1 do
    begin
      PNode := ParamList.GetChild(i);
      if (PNode.Attributes.Values['HASDEFAULT'] = '1') and (PNode.ChildCount >= 1) then
      begin
        ProcessExpression(PNode.GetChild(PNode.ChildCount - 1), DefVal);
        Slot := ParamBankAndSlot(ParamList, i, RT);
        EmitXferStore(RT, Slot, DefVal);
      end;
    end;
  end;
  EmitCallSubLabel(ProcedureLabelName(Lbl));
end;

procedure TSSAGenerator.EmitDestructorCall(const HandleVal: TSSAValue; const TypeName: string);
// V5: if TypeName (or an ancestor) declares a DESTRUCTOR, call it on the instance (THIS handle in
// int transfer slot 0). Destructors take no arguments. Emitted at scope exit, before the records
// are reclaimed (V2), so the body still sees valid storage.
// V5c: destruction is recursive — after running the object's own destructor body, each nested-UDT
// member is destroyed in reverse declaration order (C++-like: destructor, then members). This runs
// even when TypeName has no destructor of its own, so that nested members with destructors still fire.
var
  Lbl: string;
  UDTIdx, i, Slot: Integer;
  NestedHandle: TSSAValue;
begin
  UDTIdx := FindUDT(UpperCase(TypeName));
  // 1) the object's own destructor body first (so it still sees its members alive).
  Lbl := ResolveMethodLabel(TypeName, 'DESTRUCTOR');
  if Lbl <> '' then
  begin
    EmitXferStore(srtInt, 0, HandleVal);
    EmitCallSubLabel(ProcedureLabelName(Lbl));
  end;
  // 2) then destroy nested-UDT members, reverse declaration order (inherited fields included — they
  //    are part of FUDTs[UDTIdx].Fields via the prefix layout, so a single pass covers the whole object).
  if UDTIdx >= 0 then
    for i := High(FUDTs[UDTIdx].Fields) downto 0 do
      if FUDTs[UDTIdx].Fields[i].NestedType <> '' then
      begin
        Slot := FUDTs[UDTIdx].Fields[i].Slot;
        NestedHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaRecordLoadInt, NestedHandle, HandleVal,
                        MakeSSAValue(svkNone), MakeSSAConstInt(Slot));   // load nested member handle
        EmitDestructorCall(NestedHandle, FUDTs[UDTIdx].Fields[i].NestedType);
      end;
end;

function TSSAGenerator.FindBaseCall(Node: TASTNode): TASTNode;
// M4.4f: return the first explicit BASE(...) call (an antProcedureCall named "BASE") in the subtree,
// or nil. Used to hoist the base-constructor call into the ctor prologue (base-first), and to
// suppress the automatic default-base chaining when the body chains the base itself.
var
  i: Integer;
begin
  Result := nil;
  if Node = nil then Exit;
  if (Node.NodeType = antProcedureCall) and (UpperCase(VarToStr(Node.Value)) = 'BASE') then
    Exit(Node);
  for i := 0 to Node.ChildCount - 1 do
  begin
    Result := FindBaseCall(Node.GetChild(i));
    if Assigned(Result) then Exit;
  end;
end;

procedure TSSAGenerator.CollectLocalRecordVars(Node: TASTNode);
// V5: recursively gather the DIM'd UDT (record) variables in a procedure body, in textual order,
// as "VARNAME|TYPENAME" entries in FCurrentProcLocalRecs (used to emit destructor calls at exit).
var
  k, c: Integer;
  Decl: TASTNode;
  VName, TName: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antDim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 2) and
         (Decl.GetChild(1).NodeType = antIdentifier) then          // DIM v AS T (typed scalar)
      begin
        TName := UpperCase(VarToStr(Decl.GetChild(1).Value));
        if FindUDT(TName) >= 0 then
        begin
          VName := UpperCase(VarToStr(Decl.GetChild(0).Value));
          FCurrentProcLocalRecs.Add(VName + '|' + TName);
        end;
      end;
    end;
  for c := 0 to Node.ChildCount - 1 do
    CollectLocalRecordVars(Node.GetChild(c));   // recurse into nested blocks (IF/FOR/...)
end;

procedure TSSAGenerator.EmitFrameDestructors;
// V5: emit destructor calls for the current procedure's DIM'd local UDTs, in reverse construction
// order, on each frame-exit path. Each acts on the variable's current instance handle.
// V5d: BYVAL UDT parameter copies (V4) are destroyed too — after the locals (they were "constructed"
// first, at entry, so they die last), also in reverse order.
var
  i, bar: Integer;
  VName, TName: string;
begin
  if (FCurrentProcLocalRecs <> nil) then
    for i := FCurrentProcLocalRecs.Count - 1 downto 0 do
    begin
      bar := Pos('|', FCurrentProcLocalRecs[i]);
      if bar <= 0 then Continue;
      VName := Copy(FCurrentProcLocalRecs[i], 1, bar - 1);
      TName := Copy(FCurrentProcLocalRecs[i], bar + 1, MaxInt);
      if FBlockHandledVars.IndexOf(VName) >= 0 then Continue;   // M8: destructed block-scoped
      EmitDestructorCall(GetOrAllocateVariable(VName), TName);
    end;
  if (FCurrentProcByvalRecs <> nil) then
    for i := FCurrentProcByvalRecs.Count - 1 downto 0 do
    begin
      bar := Pos('|', FCurrentProcByvalRecs[i]);
      if bar <= 0 then Continue;
      VName := Copy(FCurrentProcByvalRecs[i], 1, bar - 1);
      TName := Copy(FCurrentProcByvalRecs[i], bar + 1, MaxInt);
      EmitDestructorCall(GetOrAllocateVariable(VName), TName);
    end;
end;

procedure TSSAGenerator.EmitByrefParamStore;
// BYREF: on each frame-exit path, write every explicit-BYREF scalar parameter's CURRENT register value
// back into its transfer slot, so the caller can copy it into the variable argument after the call.
// Must run before the bank save/restore (bcReturnSub) — the transfer slots survive it, the registers do
// not. The slot is the same one the caller staged the argument into (ParamBankAndSlot agreement).
var
  i: Integer;
  pk, Slot: Integer;
  RT: TSSARegisterType;
begin
  if FCurrentProcByrefScalars = nil then Exit;
  for i := 0 to FCurrentProcByrefScalars.Count - 1 do
  begin
    pk := PtrInt(FCurrentProcByrefScalars.Objects[i]);
    RT := TSSARegisterType(pk shr 16);
    Slot := pk and $FFFF;
    EmitXferStore(RT, Slot, GetOrAllocateVariable(FCurrentProcByrefScalars[i]));
  end;
end;

procedure TSSAGenerator.EmitByrefWriteback(const ParamOwnerName: string; ArgListNode: TASTNode);
// BYREF: after a call returns, copy each explicit-BYREF scalar parameter's final value (left in its
// transfer slot by EmitByrefParamStore) back into the variable argument the caller passed. Only simple
// variable arguments are lvalues we can write to; a non-variable argument (literal/expression) is left
// untouched. UDT-typed parameters are skipped — they alias the caller's instance via the handle (so
// mutations already persist through the heap), and only scalars live in a register that needs copy-back.
var
  Decl, ParamList, ParamI, ArgExpr: TASTNode;
  i, NArgs, Slot: Integer;
  RT: TSSARegisterType;
  TypeChild: TASTNode;
begin
  if not Assigned(ArgListNode) then Exit;
  if not (ArgListNode.NodeType in [antArgumentList, antExpressionList]) then Exit;
  if not (FProcDecls.TryGetValue(ParamOwnerName, Decl) and Assigned(Decl) and (Decl.ChildCount >= 2)) then Exit;
  ParamList := Decl.GetChild(1);
  if ParamList = nil then Exit;
  NArgs := ArgListNode.ChildCount;
  if NArgs > ParamList.ChildCount then NArgs := ParamList.ChildCount;
  for i := 0 to NArgs - 1 do
  begin
    ParamI := ParamList.GetChild(i);
    if ParamI.Attributes.Values['BYREF'] <> '1' then Continue;   // only explicit BYREF opts in
    // Skip UDT-typed parameters (handle-aliased): a type child that names a declared TYPE.
    if (ParamI.ChildCount >= 1) and (ParamI.GetChild(0).NodeType = antIdentifier) then
    begin
      TypeChild := ParamI.GetChild(0);
      if FindUDT(UpperCase(VarToStr(TypeChild.Value))) >= 0 then Continue;
    end;
    ArgExpr := ArgListNode.GetChild(i);
    if (ArgExpr = nil) or (ArgExpr.NodeType <> antIdentifier) then Continue;  // only a variable is writable
    Slot := ParamBankAndSlot(ParamList, i, RT);
    // BYREF-return function: an int BYREF param was passed by ADDRESS (StageCallArgs), not copy-in/out,
    // so the slot holds an address — do NOT write it back into the argument (mutations already went
    // through the address). Other params (non-byref-ret, or non-int) keep the normal copy-back.
    if IsByrefRetFunc(ParamOwnerName) and (RT = srtInt) then Continue;
    EmitXferLoad(RT, Slot, GetOrAllocateVariable(UpperCase(VarToStr(ArgExpr.Value))));
  end;
end;

procedure TSSAGenerator.CollectModuleRecordVars(Node: TASTNode);
// V5b: recursively gather the module-scope DIM'd UDT (record) variables (the program's globals), in
// textual order, as "VARNAME|TYPENAME" in FModuleRecordVars. Mirrors CollectLocalRecordVars but does
// NOT descend into procedure bodies (their DIMs are proc-local, handled by EmitFrameDestructors) so
// only true globals are collected.
var
  k, c: Integer;
  Decl: TASTNode;
  VName, TName: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antProcedureDecl then Exit;   // proc-local DIMs are not globals
  if Node.NodeType = antDim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 2) and
         (Decl.GetChild(1).NodeType = antIdentifier) then          // DIM v AS T (typed scalar)
      begin
        TName := UpperCase(VarToStr(Decl.GetChild(1).Value));
        if FindUDT(TName) >= 0 then
        begin
          VName := UpperCase(VarToStr(Decl.GetChild(0).Value));
          FModuleRecordVars.Add(VName + '|' + TName);
        end;
      end;
    end;
  for c := 0 to Node.ChildCount - 1 do
    CollectModuleRecordVars(Node.GetChild(c));   // recurse into nested blocks (IF/FOR/...), not procs
end;

function TSSAGenerator.TypeNeedsDestruction(const TypeName: string): Boolean;
// V5e: True if TypeName — or, recursively, any of its nested-UDT members (and inherited members via
// the prefix layout) — declares a DESTRUCTOR. Mirrors what EmitDestructorCall would actually emit, so
// we only reserve handle slots (and run a module dtor) for globals that have observable destruction.
// Cycle-safe: the UDT graph is a DAG (a field cannot be its own enclosing type by value), so plain
// recursion terminates; a depth cap is a defensive backstop.
  function Rec(const T: string; Depth: Integer): Boolean;
  var
    UDTIdx, i: Integer;
  begin
    Result := False;
    if Depth > 64 then Exit;                         // defensive guard against pathological input
    if ResolveMethodLabel(T, 'DESTRUCTOR') <> '' then Exit(True);
    UDTIdx := FindUDT(UpperCase(T));
    if UDTIdx < 0 then Exit;
    for i := 0 to High(FUDTs[UDTIdx].Fields) do
      if (FUDTs[UDTIdx].Fields[i].NestedType <> '') and
         Rec(FUDTs[UDTIdx].Fields[i].NestedType, Depth + 1) then
        Exit(True);
  end;
begin
  Result := Rec(TypeName, 0);
end;

procedure TSSAGenerator.AssignModuleDtorSlots;
// V5e: reserve one int transfer slot per destructor-bearing module global, so its handle can be stored
// at construction (module scope) and read back at an END inside a procedure — where the global's module
// register is hidden under the saved frame and name resolution would isolate it. Slots grow DOWN from
// XFER_RESULT_HANDLE_SLOT-1, kept disjoint from the int SHARED region (which grows up from
// SHARED_SLOT_BASE). If the two would collide, we stop assigning (graceful: that global's END-in-proc
// destructor is skipped, exactly as before this feature). Module-end destruction is unaffected (it reads
// the live module register directly).
var
  i, bar, sharedIntCount, nextSlot: Integer;
  VName, TName: string;
begin
  FModuleDtorSlots.Clear;
  if (FModuleRecordVars = nil) or (FModuleRecordVars.Count = 0) then Exit;
  sharedIntCount := 0;                               // int SHARED slots occupy SHARED_SLOT_BASE .. base+count-1
  if FSharedVars <> nil then
    for i := 0 to FSharedVars.Count - 1 do
      if GetVariableType(FSharedVars[i]) = srtInt then Inc(sharedIntCount);
  nextSlot := XFER_RESULT_HANDLE_SLOT - 1;           // 253, growing down
  for i := 0 to FModuleRecordVars.Count - 1 do       // construction order (assignment order is irrelevant)
  begin
    bar := Pos('|', FModuleRecordVars[i]);
    if bar <= 0 then Continue;
    VName := Copy(FModuleRecordVars[i], 1, bar - 1);
    TName := Copy(FModuleRecordVars[i], bar + 1, MaxInt);
    if FModuleDtorSlots.IndexOf(VName) >= 0 then Continue;   // one slot per name
    if not TypeNeedsDestruction(TName) then Continue;        // no destructor anywhere -> no slot needed
    if nextSlot <= SHARED_SLOT_BASE + sharedIntCount - 1 then Break;  // would collide with SHARED region
    FModuleDtorSlots.AddObject(VName, TObject(PtrInt(nextSlot)));
    Dec(nextSlot);
  end;
end;

procedure TSSAGenerator.EmitModuleDestructors(UseSlots: Boolean = False);
// V5b: emit destructor calls for the program's module-scope DIM'd UDTs (globals), in reverse
// construction order, at program end (just before the implicit halt). Globals live below every frame
// mark, so V2 never reclaims them; their destructors run here for deterministic RAII at shutdown.
// Storage is not freed (the program is ending) — only the destructor's observable effects (e.g. I/O).
// V5e: UseSlots=True is the END-inside-a-procedure path: the global's module register is unreachable
// (saved under the active frame) and name resolution would isolate the proc-root, so we read each
// handle from its reserved frame-independent slot (FModuleDtorSlots) instead. A global with no reserved
// slot (slot space exhausted) is skipped on this path.
var
  i, bar, slotIdx: Integer;
  VName, TName: string;
  H: TSSAValue;
begin
  if (FModuleRecordVars = nil) or (FModuleRecordVars.Count = 0) then Exit;
  for i := FModuleRecordVars.Count - 1 downto 0 do
  begin
    bar := Pos('|', FModuleRecordVars[i]);
    if bar <= 0 then Continue;
    VName := Copy(FModuleRecordVars[i], 1, bar - 1);
    TName := Copy(FModuleRecordVars[i], bar + 1, MaxInt);
    if FBlockHandledVars.IndexOf(VName) >= 0 then Continue;   // M8: destructed block-scoped
    if UseSlots then
    begin
      slotIdx := FModuleDtorSlots.IndexOf(VName);
      if slotIdx < 0 then Continue;                           // no frame-independent handle available here
      H := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitXferLoad(srtInt, PtrInt(FModuleDtorSlots.Objects[slotIdx]), H);
      EmitDestructorCall(H, TName);
    end
    else
      EmitDestructorCall(GetOrAllocateVariable(VName), TName);
  end;
end;

function TSSAGenerator.InnermostBlockFrameIdx: Integer;
// Index of the innermost open block scope (the topmost skBlock frame), or -1 if no block is open.
// Block frames are always pushed above the proc-root, so the innermost block is the stack top when
// the top is a skBlock; if the top is the proc-root (or the stack is empty) no block is open.
begin
  Result := High(FScopeStack);
  if (Result < 0) or (FScopeStack[Result].Kind <> skBlock) then Result := -1;
end;

procedure TSSAGenerator.BlockScopeEnter(IsLoopBody: Boolean);
// M8/FB: open a block scope (loop body, or — MODERN — an IF branch / SCOPE block). Emit bcRecMarkPush
// (snapshot the record high-water mark) and push a block frame; DIMs lowered in the body register
// their UDT destructors here and (MODERN) bind their names here (block-local, shadowing). IsLoopBody
// marks the frame as a loop body so EXIT FOR/DO can unwind down to it (inclusive) across nested blocks.
begin
  ScopePushFrame(skBlock);
  EmitInstruction(ssaRecMarkPush, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                  MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  FScopeStack[High(FScopeStack)].RecMarkEmitted := True;
  FScopeStack[High(FScopeStack)].IsLoopBody := IsLoopBody;
end;

procedure TSSAGenerator.EmitExitLoopCleanup;
// M8: on EXIT FOR/DO/WHILE, unwind block scopes innermost-first down to AND INCLUDING the nearest loop
// body frame (so any IF/SCOPE blocks nested between the EXIT and the loop are cleaned too). Frames are
// NOT dropped — this is one CFG path; the loop's own NEXT/LOOP end pops them on the fall-through path.
begin
  EmitExitLoopCleanupN(1);
end;

procedure TSSAGenerator.EmitExitLoopCleanupN(LoopLevels: Integer);
// Multi-level EXIT/CONTINUE (FreeBASIC "Exit For, For"): unwind block scopes innermost-first down to
// AND INCLUDING the LoopLevels-th enclosing loop-body frame (counting ALL loop bodies passed, of any
// kind, plus the IF/SCOPE blocks between them). LoopLevels=1 == EmitExitLoopCleanup.
var
  k, seen: Integer;
begin
  if LoopLevels < 1 then LoopLevels := 1;
  seen := 0;
  for k := High(FScopeStack) downto 0 do
    if FScopeStack[k].Kind = skBlock then
    begin
      EmitBlockScopeCleanup(k);
      if FScopeStack[k].IsLoopBody then
      begin
        Inc(seen);
        if seen >= LoopLevels then Break;
      end;
    end;
end;

function TSSAGenerator.FindEnclosingLoop(Kind: TLoopKind; Levels: Integer;
  out LoopIdx, AllDepth: Integer): Boolean;
// Locate the Levels-th enclosing loop of the given Kind, scanning FLoopStack from the top. LoopIdx is
// its index in FLoopStack; AllDepth is how many loops (of ANY kind) lie from the top down to and
// including it — i.e. how many loop-body frames the scope cleanup must pass. Returns False if there is
// no such loop (caller then falls back to the innermost).
var
  k, seenKind, allSeen: Integer;
begin
  Result := False; LoopIdx := -1; AllDepth := 0;
  seenKind := 0; allSeen := 0;
  for k := High(FLoopStack) downto 0 do
  begin
    Inc(allSeen);
    if FLoopStack[k].LoopKind = Kind then
    begin
      Inc(seenKind);
      if seenKind >= Levels then
      begin
        LoopIdx := k; AllDepth := allSeen; Result := True; Exit;
      end;
    end;
  end;
end;

procedure TSSAGenerator.EmitBlockScopeCleanup(Idx: Integer);
// M8: run the destructors (reverse construction order) for the block frame at Idx, then bcRecMarkPop
// to reclaim its records. Does NOT drop the scope — used both by the normal exit (which then drops)
// and by early exits (EXIT loop / EXIT SUB / RETURN), which only emit cleanup for the path taken.
var
  i, bar: Integer;
  L: TStringList;
  VName, TName: string;
begin
  if (Idx < 0) or (Idx > High(FScopeStack)) or (FScopeStack[Idx].Kind <> skBlock) then Exit;
  L := FScopeStack[Idx].Dtors;
  if L <> nil then
    for i := L.Count - 1 downto 0 do
    begin
      bar := Pos('|', L[i]);
      if bar <= 0 then Continue;
      VName := Copy(L[i], 1, bar - 1);
      TName := Copy(L[i], bar + 1, MaxInt);
      EmitDestructorCall(GetOrAllocateVariable(VName), TName);
    end;
  if FScopeStack[Idx].RecMarkEmitted then
    EmitInstruction(ssaRecMarkPop, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.BlockScopeExit;
// M8: normal block exit — clean up the innermost block frame, then drop it. Only emit the cleanup on a
// live path: if the branch already terminated (GOTO/RETURN/END left FCurrentBlock nil), an early-exit
// path has emitted its own unwinding (EmitAllBlockScopesCleanup) — here we just drop the frame to keep
// the scope stack balanced.
begin
  if InnermostBlockFrameIdx < 0 then Exit;
  if Assigned(FCurrentBlock) then
    EmitBlockScopeCleanup(High(FScopeStack));
  ScopePopFrame;
end;

procedure TSSAGenerator.EmitAllBlockScopesCleanup;
// M8: on an early frame exit from inside one or more blocks (EXIT loop / EXIT SUB / EXIT FUNCTION /
// RETURN), unwind every active block scope innermost-first, before the frame destructors. The scopes
// are NOT dropped (this is one CFG path; the blocks' own normal ends still emit cleanup on the other).
var
  k: Integer;
begin
  for k := High(FScopeStack) downto 0 do
    if FScopeStack[k].Kind = skBlock then
      EmitBlockScopeCleanup(k);
end;

procedure TSSAGenerator.ScanTopLevelLabels(Node: TASTNode; var Depth: Integer);
// GOTO-unwind worker: in-order walk of a frame's statement stream, recording the names of antLabels at
// block-depth 0 into FCurrentTopLevelLabels. Only FOR/NEXT change depth here because the FOR body is a
// FLAT sibling sequence (FOR and NEXT are separate statements with the body in between); IF/DO/WHILE/
// SCOPE bodies are AST-nested under their own node, so we simply do NOT recurse into them — any label
// inside is, by construction, not depth-0. antProgram/antStatement are transparent wrappers (recurse,
// threading Depth so a FOR opened in one wrapper still covers following siblings).
var
  i: Integer;
begin
  if Node = nil then Exit;
  case Node.NodeType of
    antForLoop: Inc(Depth);                         // body follows as siblings until antNext
    antNext: if Depth > 0 then Dec(Depth);
    antLabel: if Depth = 0 then FCurrentTopLevelLabels.Add(UpperCase(VarToStr(Node.Value)));
    antProgram, antStatement:
      for i := 0 to Node.ChildCount - 1 do
        ScanTopLevelLabels(Node.GetChild(i), Depth);
    // antIf / antDoLoop / antScope / antThen / antElse / antProcedureDecl / ... : nested scopes whose
    // labels are not depth-0 — do not recurse (and procedures collect their own labels separately).
  end;
end;

procedure TSSAGenerator.CollectTopLevelLabels(Parent: TASTNode; StartIdx: Integer);
// GOTO-unwind: (re)build FCurrentTopLevelLabels for the frame whose top-level statements are Parent's
// children from StartIdx (module: the program root from 0; a procedure: its body, after name+params).
var
  i, Depth: Integer;
begin
  FCurrentTopLevelLabels.Clear;
  if Parent = nil then Exit;
  Depth := 0;
  for i := StartIdx to Parent.ChildCount - 1 do
    ScanTopLevelLabels(Parent.GetChild(i), Depth);
end;

procedure TSSAGenerator.ScopePushFrame(Kind: TScopeKind);
// FB lexical scope (MODERN): push a new scope frame with empty binding/dtor tables. Inert in CLASSIC
// (callers gate on FModernMode), so the stack is empty there and resolution stays global-by-name.
var
  F: TScopeFrame;
begin
  F.Kind := Kind;
  F.Bindings := TStringList.Create;
  F.Bindings.CaseSensitive := False;
  F.Dtors := TStringList.Create;
  F.Dtors.CaseSensitive := False;
  F.RecMarkEmitted := False;
  F.IsLoopBody := False;
  SetLength(FScopeStack, Length(FScopeStack) + 1);
  FScopeStack[High(FScopeStack)] := F;
end;

procedure TSSAGenerator.ScopePopFrame;
// FB lexical scope: drop the innermost frame and free its tables. (Block-frame destructor emission and
// record-mark pop are handled by the block-scope path; this only releases the bookkeeping.)
begin
  if Length(FScopeStack) = 0 then Exit;
  FScopeStack[High(FScopeStack)].Bindings.Free;
  FScopeStack[High(FScopeStack)].Dtors.Free;
  SetLength(FScopeStack, Length(FScopeStack) - 1);
end;

procedure TSSAGenerator.AddSharedVarSlot(const VName: string);
// M6: assign one global its dedicated transfer slot, counted per-bank from SHARED_SLOT_BASE upward
// across the WHOLE module (FSharedVars is the single source of truth, so the per-bank count is derived
// from it — never from a local counter that resets between AST nodes). The slot survives the bcCallSub
// frame save/restore. A UDT instance variable is a plain int handle, so a DIM SHARED UDT shares that
// handle through an int slot (the record itself lives in the shared heap); under FB scope a NON-shared
// module UDT is isolated inside procedures, so the handle must travel via the slot to be visible.
// Already-shared names are idempotent no-ops.
var
  Bank: TSSARegisterType;
  cnt, j: Integer;
begin
  if FSharedVars.IndexOf(VName) >= 0 then Exit;        // already shared
  Bank := GetVariableType(VName);
  cnt := 0;
  for j := 0 to FSharedVars.Count - 1 do
    if GetVariableType(FSharedVars[j]) = Bank then Inc(cnt);
  // Guard the slot range: never reach the reserved result slots (254/255). If a bank runs out of
  // shared slots the variable is simply left non-shared (a clean limit, not a corruption).
  if SHARED_SLOT_BASE + cnt < XFER_RESULT_HANDLE_SLOT then
    FSharedVars.AddObject(VName, TObject(PtrInt(SHARED_SLOT_BASE + cnt)));
end;

procedure TSSAGenerator.CollectSharedVars(Node: TASTNode);
// M6: gather the DIM SHARED scalar variables (parser marks the antArrayDecl with the 'SHARED'
// attribute) and assign each a dedicated transfer slot via AddSharedVarSlot. Those slots survive the
// bcCallSub frame save/restore, so a SUB/FUNCTION sees the module's value. v1 supports scalar
// (int/float/string) shared globals only; SHARED arrays are left for later (UDTs are already shared
// via the record heap). DIM SHARED is the only sharing mechanism (the QuickBASIC `SHARED x` statement
// inside a procedure is not a -lang fb feature).
var
  i, k, ai: Integer;
  Decl: TASTNode;
  VNameU, TypeNameU: string;
  ElemBank: TSSARegisterType;
begin
  if Node = nil then Exit;
  // (single flat pass over the whole AST: DIM SHARED is module-level; nested DIMs aren't marked)
  if Node.NodeType = antDim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.Attributes.Values['SHARED'] = '1') and
         (Decl.ChildCount >= 2) and (Decl.GetChild(1).NodeType = antIdentifier) then  // typed scalar only
      begin
        VNameU := UpperCase(VarToStr(Decl.GetChild(0).Value));
        AddSharedVarSlot(VNameU);                       // keep the "is shared" marker (scope resolution)
        // Refinement #2: a SHARED scalar is backed by a 1-element global array, so it lives in the shared
        // FArrays and is visible/live across threads. A builtin scalar stores its value; a UDT scalar
        // stores its (int) record handle — the record itself is allocated in the shared region at DIM.
        TypeNameU := UpperCase(VarToStr(Decl.GetChild(1).Value));
        if FSharedScalarArr.IndexOf(VNameU) < 0 then
        begin
          if FindUDT(TypeNameU) >= 0 then
            ElemBank := srtInt                           // UDT scalar: the array element is the record handle
          else
            ElemBank := TypeNameToBank(TypeNameU, VNameU);
          ai := FProgram.DeclareArray(VNameU, ElemBank, [1]);   // 1-element global array, same name
          FSharedScalarArr.AddObject(VNameU, TObject(PtrInt(ai)));
        end;
      end;
    end;
  for i := 0 to Node.ChildCount - 1 do
    CollectSharedVars(Node.GetChild(i));
end;

function TSSAGenerator.IsSharedScalar(const Name: string): Boolean;
begin
  Result := (FSharedScalarArr <> nil) and (FSharedScalarArr.IndexOf(UpperCase(Name)) >= 0);
end;

function TSSAGenerator.MakeSharedScalarAccess(const Name: string; const Tok: TLexerToken): TASTNode;
// Build the AST for "name(0)" — antArrayAccess(antIdentifier(name), antExpressionList(antLiteral 0)) —
// so reads and writes of a SHARED scalar reuse the existing array load/store lowering (element 0 of its
// backing 1-element global array). The caller owns and frees the returned node.
var
  IdxList: TASTNode;
begin
  Result := TASTNode.Create(antArrayAccess, Tok);
  Result.AddChild(TASTNode.CreateWithValue(antIdentifier, UpperCase(Name), Tok));
  IdxList := TASTNode.Create(antExpressionList, Tok);
  IdxList.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Tok));
  Result.AddChild(IdxList);
end;

procedure TSSAGenerator.CollectStaticMembers(Node: TASTNode);
// OOP static member variables: a TYPE field declared "Static x AS t" has ONE storage shared by every
// instance. Back each with a 1-element global array named "TYPE.FIELD" (reusing the DIM SHARED scalar
// machinery, so it is visible from methods/threads). v1 supports builtin scalar static members.
var
  i, k, ai: Integer;
  FieldNode: TASTNode;
  tn, fn, ftype, backing: string;
  bank: TSSARegisterType;
begin
  if Node = nil then Exit;
  if Node.NodeType = antTypeDecl then
  begin
    tn := UpperCase(VarToStr(Node.Value));
    for k := 0 to Node.ChildCount - 1 do
    begin
      FieldNode := Node.GetChild(k);
      if (FieldNode.NodeType = antIdentifier) and (FieldNode.Attributes.Values['STATIC'] = '1') then
      begin
        fn := UpperCase(VarToStr(FieldNode.Value));
        ftype := '';
        if FieldNode.ChildCount > 0 then ftype := UpperCase(VarToStr(FieldNode.GetChild(0).Value));
        bank := TypeNameToBank(ftype, fn);                 // builtin scalar bank (int/float/string)
        backing := tn + '.' + fn;
        if FStaticMembers.IndexOf(backing) < 0 then
        begin
          ai := FProgram.DeclareArray(backing, bank, [1]);  // 1-element global array, same dotted name
          FSharedScalarArr.AddObject(backing, TObject(PtrInt(ai)));
          FStaticMembers.Add(backing);
        end;
      end;
    end;
  end;
  for i := 0 to Node.ChildCount - 1 do
    CollectStaticMembers(Node.GetChild(i));
end;

procedure TSSAGenerator.EmitStaticMemberAllocs;
// Allocate the static members' backing 1-element arrays once at program start (they have no DIM).
var
  i, ai: Integer;
  ArrayRef: TSSAValue;
begin
  if FStaticMembers = nil then Exit;
  for i := 0 to FStaticMembers.Count - 1 do
  begin
    ai := PtrInt(FSharedScalarArr.Objects[FSharedScalarArr.IndexOf(FStaticMembers[i])]);
    ArrayRef := MakeSSAArrayRef(ai, FProgram.GetArray(ai).ElementType);
    EmitInstruction(ssaArrayDim, MakeSSAValue(svkNone), ArrayRef,
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

function TSSAGenerator.StaticMemberBackingName(ObjNode: TASTNode; const FieldName: string): string;
// If ObjNode.FieldName is a static member (accessed via the type name OR via an instance), return the
// backing shared-scalar name "DECLTYPE.FIELD" (walking the inheritance chain); otherwise ''.
var
  at, t, key: string;
  idx: Integer;
begin
  Result := '';
  if ObjNode = nil then Exit;
  if (ObjNode.NodeType = antIdentifier) and (FindUDT(UpperCase(VarToStr(ObjNode.Value))) >= 0) then
    at := UpperCase(VarToStr(ObjNode.Value))     // TypeName.field (no instance)
  else
    at := ObjectTypeName(ObjNode);               // instance.field
  if at = '' then Exit;
  t := at;
  while t <> '' do
  begin
    key := t + '.' + UpperCase(FieldName);
    if FStaticMembers.IndexOf(key) >= 0 then Exit(key);
    idx := FindUDT(t);
    if idx < 0 then Break;
    t := UpperCase(FUDTs[idx].Parent);
  end;
end;

procedure TSSAGenerator.EmitSharedScalarStoreVal(const Name: string; const Val: TSSAValue);
// Store an already-computed SSA value into element 0 of a SHARED scalar's backing array (used to publish
// a freshly-constructed UDT record handle). Coerces to the array element type.
var
  ai: Integer;
  et: TSSARegisterType;
  ArrayRef, Idx0, V: TSSAValue;
begin
  ai := PtrInt(FSharedScalarArr.Objects[FSharedScalarArr.IndexOf(UpperCase(Name))]);
  et := FProgram.GetArray(ai).ElementType;
  Idx0 := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaLoadConstInt, Idx0, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  case et of
    srtFloat:  V := EnsureFloatRegister(Val);
    srtString: V := EnsureStringRegister(Val);
  else
    V := EnsureIntRegister(Val);
  end;
  ArrayRef := MakeSSAArrayRef(ai, et);
  EmitInstruction(ssaArrayStore, V, ArrayRef, Idx0, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.CollectDimVarBanks(Node: TASTNode; Dict: TStringList);
// Pass 1: gather every @-taken NAME (antProcAddress operand) into Dict, and record pointee types of
// pointer-typed DIMs ("type PTR") in FPointerVars (for typing "*p").
var
  i, k: Integer;
  Decl: TASTNode;
  VNameU, TypeNameU: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antProcAddress then
  begin
    VNameU := UpperCase(VarToStr(Node.Value));
    if Dict.IndexOf(VNameU) < 0 then Dict.Add(VNameU);
  end;
  // VARPTR(x) takes the address of a DATA variable x (= @x), so x must be backed with stable storage —
  // mark it @-taken here, just like an antProcAddress operand. (PROCPTR's argument is a procedure: its
  // @sub entry-PC path needs no data backing, so it is not collected here.)
  if (Node.NodeType = antArrayAccess) and (Node.ChildCount >= 2) and
     (Node.GetChild(0).NodeType = antIdentifier) and
     (UpperCase(VarToStr(Node.GetChild(0).Value)) = kVARPTR) and
     (Node.GetChild(1).ChildCount >= 1) and (Node.GetChild(1).GetChild(0).NodeType = antIdentifier) then
  begin
    VNameU := UpperCase(VarToStr(Node.GetChild(1).GetChild(0).Value));
    if Dict.IndexOf(VNameU) < 0 then Dict.Add(VNameU);
  end;
  if Node.NodeType = antDim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 2) and
         (Decl.GetChild(0).NodeType = antIdentifier) and (Decl.GetChild(1).NodeType = antIdentifier) then
      begin
        VNameU := UpperCase(VarToStr(Decl.GetChild(0).Value));
        TypeNameU := UpperCase(VarToStr(Decl.GetChild(1).Value));
        if (Length(TypeNameU) >= 4) and (Copy(TypeNameU, Length(TypeNameU) - 3, 4) = ' PTR') and
           (FPointerVars.IndexOfName(VNameU) < 0) then
          FPointerVars.Add(VNameU + '=' + Trim(Copy(TypeNameU, 1, Length(TypeNameU) - 4)));
      end;
    end;
  for i := 0 to Node.ChildCount - 1 do
    CollectDimVarBanks(Node.GetChild(i), Dict);
end;

procedure TSSAGenerator.MarkAddressTaken(Node: TASTNode; Dict: TStringList; InProc: Boolean);
// Pass 2: route each typed-scalar DIM whose variable is @-taken. A MODULE-level var is marked SHARED
// (CollectSharedVars backs it with a 1-element global array — one stable cell, correct lifetime). A var
// declared INSIDE a SUB/FUNCTION is instead recorded in FAddrLocalVars: it gets a per-frame 1-field
// record (ProcessDim), so its address is distinct per recursion level (a shared cell would collide).
var
  i, k: Integer;
  Decl: TASTNode;
  VNameU, VTypeU: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antDim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 2) and
         (Decl.GetChild(0).NodeType = antIdentifier) and (Decl.GetChild(1).NodeType = antIdentifier) then
      begin
        VNameU := UpperCase(VarToStr(Decl.GetChild(0).Value));
        VTypeU := UpperCase(VarToStr(Decl.GetChild(1).Value));
        // Only builtin scalars need backing: a UDT variable is already a stable record handle, so @obj
        // just reads that handle (the backing machinery would wrongly turn the UDT into a 1-element array).
        if (Dict.IndexOf(VNameU) >= 0) and (FindUDT(VTypeU) < 0) then
        begin
          if InProc then
            Decl.Attributes.Values['ADDRLOCAL'] := '1'    // per-frame record-backed (recursion-safe)
          else
            Decl.Attributes.Values['SHARED'] := '1';      // module: one shared 1-element global array
        end;
      end;
    end;
  if Node.NodeType = antProcedureDecl then InProc := True;
  for i := 0 to Node.ChildCount - 1 do
    MarkAddressTaken(Node.GetChild(i), Dict, InProc);
end;

function ByrefRetDeclHasByrefParam(Node: TASTNode): Boolean;
// True if Node is a FUNCTION declared BYREF-return ("FUNCTION f() BYREF AS T") with at least one
// explicit-BYREF parameter — the shape that can return a reference to one of its parameters.
var
  PList: TASTNode;
  i: Integer;
begin
  Result := False;
  if (Node = nil) or (Node.NodeType <> antProcedureDecl) then Exit;
  if Node.Attributes.Values['BYREFRET'] <> '1' then Exit;
  if (Node.ChildCount < 2) or (Node.GetChild(1).NodeType <> antParameterList) then Exit;
  PList := Node.GetChild(1);
  for i := 0 to PList.ChildCount - 1 do
    if PList.GetChild(i).Attributes.Values['BYREF'] = '1' then Exit(True);
end;

procedure TSSAGenerator.GatherByrefRetFuncNames(Node: TASTNode; Names: TStringList);
// Collect the names of FUNCTIONs that are BYREF-return AND take a BYREF parameter (so a call's args
// may need to be address-backed, to let "RETURN param" yield a reference into the caller's variable).
var
  i: Integer;
begin
  if Node = nil then Exit;
  if ByrefRetDeclHasByrefParam(Node) and (Node.GetChild(0).NodeType = antIdentifier) then
    if Names.IndexOf(UpperCase(VarToStr(Node.GetChild(0).Value))) < 0 then
      Names.Add(UpperCase(VarToStr(Node.GetChild(0).Value)));
  for i := 0 to Node.ChildCount - 1 do
    GatherByrefRetFuncNames(Node.GetChild(i), Names);
end;

procedure TSSAGenerator.MarkByrefRetCallArgs(Node: TASTNode; Names, Dict: TStringList);
// For every call "f(args)" to a byref-return function in Names, mark each identifier argument as
// address-taken (added to Dict), so it is backed by a stable address the function can return.
var
  i: Integer;
  ArgsNode: TASTNode;
begin
  if Node = nil then Exit;
  if (Node.NodeType = antArrayAccess) and (Node.ChildCount >= 2) and
     (Node.GetChild(0).NodeType = antIdentifier) and
     (Names.IndexOf(UpperCase(VarToStr(Node.GetChild(0).Value))) >= 0) and
     (Node.GetChild(1).NodeType in [antArgumentList, antExpressionList]) then
  begin
    ArgsNode := Node.GetChild(1);
    for i := 0 to ArgsNode.ChildCount - 1 do
      if (ArgsNode.GetChild(i).NodeType = antIdentifier) and
         (Dict.IndexOf(UpperCase(VarToStr(ArgsNode.GetChild(i).Value))) < 0) then
        Dict.Add(UpperCase(VarToStr(ArgsNode.GetChild(i).Value)));
  end;
  for i := 0 to Node.ChildCount - 1 do
    MarkByrefRetCallArgs(Node.GetChild(i), Names, Dict);
end;

procedure TSSAGenerator.CollectAddressTakenVars(Node: TASTNode);
// Runs BEFORE CollectSharedVars: marks @-taken declared scalars SHARED so they become array-backed
// (addressable). Also records pointer-var pointee types in FPointerVars. In addition, arguments passed
// to a BYREF-return function are address-backed so it can return a reference to them (min(a,b)=0).
var
  Dict, ByrefRetNames: TStringList;
begin
  if Node = nil then Exit;
  Dict := TStringList.Create;
  ByrefRetNames := TStringList.Create;
  try
    CollectDimVarBanks(Node, Dict);   // collect @-taken names + pointee types
    GatherByrefRetFuncNames(Node, ByrefRetNames);
    if ByrefRetNames.Count > 0 then
      MarkByrefRetCallArgs(Node, ByrefRetNames, Dict);   // back the args of byref-return calls
    MarkAddressTaken(Node, Dict);     // mark their DIMs SHARED
  finally
    Dict.Free;
    ByrefRetNames.Free;
  end;
end;

function TSSAGenerator.PointeeBankOf(const PtrName: string): TSSARegisterType;
// Bank of the pointee for "*p", from p's declared pointer type (default int if unknown).
var
  idx: Integer;
  Pointee: string;
begin
  Result := srtInt;
  idx := FPointerVars.IndexOfName(UpperCase(PtrName));
  if idx >= 0 then
  begin
    Pointee := FPointerVars.ValueFromIndex[idx];
    Result := TypeNameToBank(Pointee, PtrName);
  end;
end;

function TSSAGenerator.IsAddrParam(const Name: string): Boolean;
// Is Name a BYREF-return address-carrying parameter (its register holds the caller variable's address,
// so reads/writes auto-dereference)? Only ever non-empty inside a byref-return function.
begin
  Result := (FCurrentProcAddrParams <> nil) and (FCurrentProcAddrParams.IndexOfName(UpperCase(Name)) >= 0);
end;

function TSSAGenerator.AddrParamBank(const Name: string): TSSARegisterType;
// The pointee bank of an address parameter (from its declared scalar type).
var
  idx: Integer;
begin
  Result := srtInt;
  idx := FCurrentProcAddrParams.IndexOfName(UpperCase(Name));
  if idx >= 0 then Result := TypeNameToBank(FCurrentProcAddrParams.ValueFromIndex[idx], Name);
end;

function TSSAGenerator.IsRefVar(const Name: string): Boolean;
// A FreeBASIC reference variable (DIM BYREF r AS T = target): its register carries target's address, so
// reads/writes auto-dereference (like a BYREF-return address param, but bound at its declaration).
begin
  Result := FRefVars.IndexOfName(UpperCase(Name)) >= 0;
end;

function TSSAGenerator.RefVarBank(const Name: string): TSSARegisterType;
// The pointee bank of a reference variable (from its declared scalar type).
var
  idx: Integer;
begin
  Result := srtInt;
  idx := FRefVars.IndexOfName(UpperCase(Name));
  if idx >= 0 then Result := TypeNameToBank(FRefVars.ValueFromIndex[idx], Name);
end;

function TSSAGenerator.IsRawPtr(const Name: string): Boolean;
// A pointer variable whose value is a raw byte-heap offset (it was assigned from Allocate/CAllocate/
// Reallocate). Its deref and arithmetic use the raw heap (SizeOf-scaled), not the managed path.
begin
  Result := FRawPtrVars.IndexOf(UpperCase(Name)) >= 0;
end;

function TSSAGenerator.IsAllocCall(Node: TASTNode; out FuncU: string): Boolean;
// Recognise ALLOCATE(n) / CALLOCATE(n) / REALLOCATE(p,n) — they parse as an array-access (call) whose
// name is not a declared array/function. FuncU receives the upper-case function name.
begin
  Result := False; FuncU := '';
  if (Node = nil) or (Node.NodeType <> antArrayAccess) or (Node.ChildCount < 2) then Exit;
  if Node.GetChild(0).NodeType <> antIdentifier then Exit;
  FuncU := UpperCase(VarToStr(Node.GetChild(0).Value));
  if (FuncU = 'ALLOCATE') or (FuncU = 'CALLOCATE') or (FuncU = 'REALLOCATE') then
    Result := FProgram.FindArray(FuncU) < 0;
end;

procedure TSSAGenerator.EmitRawPtrArith(Node: TASTNode; out Result: TSSAValue);
// Lower "p + n" / "p - n" / "n + p" (raw pointer ± integer) to ptrVal ± n*SizeOf(pointee), a raw byte
// pointer. The raw pointer side is identified by RawPtrExprName; the other side is the integer index.
var
  PtrName: string;
  PtrSide, IntSide: TASTNode;
  PtrVal, IntVal, SzVal, Scaled: TSSAValue;
  sz: Int64;
begin
  // Decide which child is the raw pointer (left for +/-, or right only for +).
  if RawPtrExprName(Node.GetChild(0)) <> '' then
  begin PtrSide := Node.GetChild(0); IntSide := Node.GetChild(1); end
  else
  begin PtrSide := Node.GetChild(1); IntSide := Node.GetChild(0); end;
  PtrName := RawPtrExprName(PtrSide);
  ProcessExpression(PtrSide, PtrVal); PtrVal := EnsureIntRegister(PtrVal);
  ProcessExpression(IntSide, IntVal); IntVal := EnsureIntRegister(IntVal);
  sz := RawElemSizeOf(PtrName);
  if sz > 1 then
  begin
    SzVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, SzVal, MakeSSAConstInt(sz), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Scaled := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaMulInt, Scaled, IntVal, SzVal, MakeSSAValue(svkNone));
    IntVal := Scaled;
  end;
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  if Node.Token.TokenType = ttOpSub then
    EmitInstruction(ssaSubInt, Result, PtrVal, IntVal, MakeSSAValue(svkNone))
  else
    EmitInstruction(ssaAddInt, Result, PtrVal, IntVal, MakeSSAValue(svkNone));
end;

function TSSAGenerator.RawPtrExprName(Node: TASTNode): string;
// If Node is a raw-pointer expression — a raw pointer identifier, "(p)", or "p + n"/"p - n"/"n + p"
// where p is raw — return the raw pointer variable name (for its element type); else ''.
begin
  Result := '';
  if Node = nil then Exit;
  while (Node.NodeType = antParentheses) and (Node.ChildCount >= 1) do
    Node := Node.GetChild(0);
  if Node.NodeType = antIdentifier then
  begin
    if IsRawPtr(VarToStr(Node.Value)) then Result := UpperCase(VarToStr(Node.Value));
  end
  else if (Node.NodeType = antBinaryOp) and (Node.ChildCount >= 2) and Assigned(Node.Token) and
          ((Node.Token.TokenType = ttOpAdd) or (Node.Token.TokenType = ttOpSub)) then
  begin
    Result := RawPtrExprName(Node.GetChild(0));
    if (Result = '') and (Node.Token.TokenType = ttOpAdd) then Result := RawPtrExprName(Node.GetChild(1));
  end;
end;

function TSSAGenerator.RawTypeCodeOf(const PtrName: string): Integer;
// Raw element type code for *p / p[i], from the pointer's declared pointee type.
var
  T: string;
begin
  T := UpperCase(FPointerVars.Values[UpperCase(PtrName)]);
  if (T = 'BYTE') or (T = 'UBYTE') then Result := RTC_I8
  else if (T = 'SHORT') or (T = 'USHORT') then Result := RTC_I16
  else if (T = 'LONG') or (T = 'ULONG') then Result := RTC_I32
  else if T = 'SINGLE' then Result := RTC_SINGLE
  else if T = 'DOUBLE' then Result := RTC_DOUBLE
  else Result := RTC_I64;   // INTEGER/LONGINT/UINTEGER/ULONGINT (our INTEGER is 64-bit)
end;

function TSSAGenerator.RawElemSizeOf(const PtrName: string): Int64;
// SizeOf(pointee) in bytes — scales raw pointer arithmetic and p[i] indexing.
begin
  case RawTypeCodeOf(PtrName) of
    RTC_I8: Result := 1;
    RTC_I16: Result := 2;
    RTC_I32, RTC_SINGLE: Result := 4;
  else
    Result := 8;   // I64 / DOUBLE
  end;
end;

function TSSAGenerator.TypeSizeBytes(const TypeName: string): Int64;
// SizeOf(T) in bytes for the FreeBASIC SizeOf() operator. Scalars use FB widths (our INTEGER is 64-bit);
// a "... PTR" or unknown type is pointer-sized (8); a UDT sums its fields' sizes.
var
  T: string;
  u, j: Integer;
begin
  T := UpperCase(Trim(TypeName));
  if (Length(T) >= 4) and (Copy(T, Length(T) - 3, 4) = ' PTR') then Exit(8);
  T := CanonicalType(T);   // resolve FB TYPE-alias before the builtin/UDT size match
  if (T = 'BYTE') or (T = 'UBYTE') or (T = 'BOOLEAN') then Result := 1
  else if (T = 'SHORT') or (T = 'USHORT') then Result := 2
  else if (T = 'LONG') or (T = 'ULONG') then Result := 4
  else if T = 'SINGLE' then Result := 4
  else if (T = 'INTEGER') or (T = 'UINTEGER') or (T = 'LONGINT') or (T = 'ULONGINT') or (T = 'DOUBLE') then Result := 8
  else
  begin
    u := FindUDT(T);
    if u >= 0 then
    begin
      Result := 0;
      for j := 0 to High(FUDTs[u].Fields) do
        case FUDTs[u].Fields[j].Bank of
          srtFloat: Result := Result + 8;
          srtString: Result := Result + 8;   // string field = handle/pointer-sized in our model
        else Result := Result + 8;
        end;
      if Result = 0 then Result := 8;
    end
    else
      Result := 8;   // unknown / pointer-sized default
  end;
end;

procedure TSSAGenerator.EmitRawAlloc(CallNode: TASTNode; out Result: TSSAValue);
// Lower ALLOCATE(n)/CALLOCATE(n) → bcRawAlloc(n bytes); REALLOCATE(p,n) → bcRawRealloc(p, n bytes).
// The byte count is taken verbatim (FreeBASIC Allocate is byte-granular; SizeOf supplies the math).
var
  FuncU: string;
  CountV, OldV: TSSAValue;
  ArgsNode: TASTNode;
begin
  IsAllocCall(CallNode, FuncU);
  ArgsNode := CallNode.GetChild(1);
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  if FuncU = 'REALLOCATE' then
  begin
    ProcessExpression(ArgsNode.GetChild(0), OldV);   OldV := EnsureIntRegister(OldV);
    ProcessExpression(ArgsNode.GetChild(1), CountV); CountV := EnsureIntRegister(CountV);
    EmitInstruction(ssaRawRealloc, Result, OldV, CountV, MakeSSAValue(svkNone));
  end
  else
  begin
    ProcessExpression(ArgsNode.GetChild(0), CountV); CountV := EnsureIntRegister(CountV);
    EmitInstruction(ssaRawAlloc, Result, CountV, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.EmitRawMemOp(const FuncU: string; ArgsNode: TASTNode; out Result: TSSAValue);
// FreeBASIC raw-memory block ops on the byte heap. The byte count register is carried in Src3 (the
// compiler maps Src3 -> Immediate). FB_MEMCOPYCLEAR is composed from a memcopy plus a clear of the
// tail (no dedicated opcode): rest pointer = dst + srclen (a raw pointer is a tagged byte offset, so a
// small integer add keeps the tag), rest length = dstlen - srclen.
var
  DstR, SrcR, BytesR, ValR, DstLenR, SrcLenR, RestPtr, RestLen, ZeroR, TmpV: TSSAValue;
begin
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  if (FuncU = kFBMEMCOPY) or (FuncU = kFBMEMMOVE) then
  begin
    ProcessExpression(ArgsNode.GetChild(0), TmpV); DstR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(1), TmpV); SrcR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(2), TmpV); BytesR := EnsureIntRegister(TmpV);
    if FuncU = kFBMEMCOPY then
      EmitInstruction(ssaRawMemCopy, Result, DstR, SrcR, BytesR)   // Result = destination pointer (FB returns dst)
    else
      EmitInstruction(ssaRawMemMove, Result, DstR, SrcR, BytesR);
  end
  else if FuncU = kCLEAR then
  begin
    ProcessExpression(ArgsNode.GetChild(0), TmpV); DstR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(1), TmpV); ValR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(2), TmpV); BytesR := EnsureIntRegister(TmpV);
    EmitInstruction(ssaRawClear, MakeSSAValue(svkNone), DstR, ValR, BytesR);
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end
  else  // kFBMEMCOPYCLEAR: (dst, dstlen, src, srclen)
  begin
    ProcessExpression(ArgsNode.GetChild(0), TmpV); DstR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(1), TmpV); DstLenR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(2), TmpV); SrcR := EnsureIntRegister(TmpV);
    ProcessExpression(ArgsNode.GetChild(3), TmpV); SrcLenR := EnsureIntRegister(TmpV);
    // copy the first srclen bytes
    EmitInstruction(ssaRawMemCopy, MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt)), DstR, SrcR, SrcLenR);
    // rest pointer = dst + srclen
    RestPtr := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaAddInt, RestPtr, DstR, SrcLenR, MakeSSAValue(svkNone));
    // rest length = dstlen - srclen
    RestLen := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaSubInt, RestLen, DstLenR, SrcLenR, MakeSSAValue(svkNone));
    // clear the rest to zero
    ZeroR := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, ZeroR, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    EmitInstruction(ssaRawClear, MakeSSAValue(svkNone), RestPtr, ZeroR, RestLen);
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  end;
end;

procedure TSSAGenerator.CollectRawPtrVars(Node: TASTNode);
// Pre-scan (run to a fixpoint by the caller): a pointer variable is RAW if it is assigned from
// ALLOCATE/CALLOCATE/REALLOCATE, from a pointer CAST/CPTR of a raw value, or copied from another raw
// pointer. Raw-ness then drives deref/arithmetic onto the byte heap regardless of statement order.
var
  i: Integer;
  Lhs, Rhs: TASTNode;
  FuncU, LhsU, TypeU: string;

  procedure MarkRaw(const N: string);
  begin
    if FRawPtrVars.IndexOf(UpperCase(N)) < 0 then
    begin
      FRawPtrVars.Add(UpperCase(N));
      FRawCollectChanged := True;
    end;
  end;

begin
  if Node = nil then Exit;
  if (Node.NodeType = antAssignment) and (Node.ChildCount >= 2) and (Node.GetChild(0).NodeType = antIdentifier) then
  begin
    Lhs := Node.GetChild(0);
    Rhs := Node.GetChild(1);
    LhsU := UpperCase(VarToStr(Lhs.Value));
    if IsAllocCall(Rhs, FuncU) then
      MarkRaw(LhsU)
    else if Rhs.NodeType = antCast then
    begin
      // p = CAST(<ptr type>, X): raw if X is an Allocate call or an already-raw pointer var.
      TypeU := UpperCase(VarToStr(Rhs.Value));
      if (Length(TypeU) >= 4) and (Copy(TypeU, Length(TypeU) - 3, 4) = ' PTR') and (Rhs.ChildCount >= 1) then
        if IsAllocCall(Rhs.GetChild(0), FuncU) or
           ((Rhs.GetChild(0).NodeType = antIdentifier) and IsRawPtr(VarToStr(Rhs.GetChild(0).Value))) then
          MarkRaw(LhsU);
    end
    else if (Rhs.NodeType = antIdentifier) and IsRawPtr(VarToStr(Rhs.Value)) then
      MarkRaw(LhsU)   // p = q, q raw
    else if FModernMode and (Rhs.NodeType = antArrayAccess) and (Rhs.ChildCount >= 1) and
            (Rhs.GetChild(0).NodeType = antIdentifier) and
            ((UpperCase(VarToStr(Rhs.GetChild(0).Value)) = kSADD) or
             (UpperCase(VarToStr(Rhs.GetChild(0).Value)) = kSTRPTR)) and
            (FProgram.FindArray(VarToStr(Rhs.GetChild(0).Value)) < 0) then
      MarkRaw(LhsU);   // p = SADD(s)/STRPTR(s): a raw byte-heap pointer -> deref p[i] onto the byte heap
  end;
  for i := 0 to Node.ChildCount - 1 do
    CollectRawPtrVars(Node.GetChild(i));
end;

procedure TSSAGenerator.CollectWStringVars(Node: TASTNode);
// Pre-scan: record every variable, parameter or FUNCTION result declared AS WSTRING (by name, into the
// flat FWStringVars). These share the srtString bank (UTF-8 byte storage) but their LEN/MID/LEFT/RIGHT
// count/index by Unicode codepoint. Declaration shapes: DIM scalar = antArrayDecl(name, "WSTRING"); a
// parameter = antIdentifier(name) with a type child "WSTRING" inside an antParameterList; a FUNCTION
// result = the procedure's name node with a type child "WSTRING" (the result is referenced by the bare
// function name inside the body, so marking that name also makes a call f(...) detect as wide).
var
  i: Integer;
  NameNode, P: TASTNode;

  procedure MarkW(const N: string);
  begin
    if (N <> '') and (FWStringVars.IndexOf(UpperCase(N)) < 0) then
      FWStringVars.Add(UpperCase(N));
  end;

begin
  if Node = nil then Exit;
  // DIM v AS WSTRING [* n]      : antArrayDecl(child0=name, child1=type ident).
  // DIM a(n) AS WSTRING [* n]   : antArrayDecl(child0=name, child1=dimensions, child2=type ident).
  // Marking the array name makes a(i) detect as wide too (IsWStringExpr checks IsWStringVar on the name).
  if (Node.NodeType = antArrayDecl) and (Node.ChildCount >= 2) and
     (Node.GetChild(0).NodeType = antIdentifier) then
  begin
    P := nil;
    if Node.GetChild(1).NodeType = antIdentifier then
      P := Node.GetChild(1)                                   // scalar typed: type at child 1
    else if (Node.ChildCount >= 3) and (Node.GetChild(2).NodeType = antIdentifier) then
      P := Node.GetChild(2);                                  // array typed: type at child 2
    if (P <> nil) and (UpperCase(VarToStr(P.Value)) = 'WSTRING') then
      MarkW(VarToStr(Node.GetChild(0).Value));
    // Fixed-length string capacity (DIM s AS STRING/WSTRING * n): record a positive constant capacity so
    // ProcessAssignment truncates stores. Only the scalar form carries a numeric FIXEDLEN attribute.
    if (Node.GetChild(1).NodeType = antIdentifier) and (Node.Attributes.IndexOfName('FIXEDLEN') >= 0) and
       (StrToIntDef(Node.Attributes.Values['FIXEDLEN'], -1) > 0) then
      FFixedLenVars.Values[UpperCase(VarToStr(Node.GetChild(0).Value))] := Node.Attributes.Values['FIXEDLEN'];
  end;
  // FUNCTION ... AS WSTRING : child0 = name node, its child0 = return-type identifier.
  if (Node.NodeType = antProcedureDecl) and (Node.ChildCount >= 1) then
  begin
    NameNode := Node.GetChild(0);
    if (NameNode.NodeType = antIdentifier) and (NameNode.ChildCount >= 1) and
       (NameNode.GetChild(0).NodeType = antIdentifier) and
       (UpperCase(VarToStr(NameNode.GetChild(0).Value)) = 'WSTRING') then
      MarkW(VarToStr(NameNode.Value));   // bare fname = wide result; also makes a call detect wide
  end;
  // Parameter AS WSTRING : antIdentifier(param) with a type child "WSTRING", inside an antParameterList.
  if Node.NodeType = antParameterList then
    for i := 0 to Node.ChildCount - 1 do
    begin
      P := Node.GetChild(i);
      if (P.NodeType = antIdentifier) and (P.ChildCount >= 1) and
         (P.GetChild(0).NodeType = antIdentifier) and
         (UpperCase(VarToStr(P.GetChild(0).Value)) = 'WSTRING') then
        MarkW(VarToStr(P.Value));
    end;
  for i := 0 to Node.ChildCount - 1 do
    CollectWStringVars(Node.GetChild(i));
end;

function TSSAGenerator.IsWStringVar(const Name: string): Boolean;
begin
  Result := FWStringVars.IndexOf(UpperCase(Name)) >= 0;
end;

procedure TSSAGenerator.CollectRedimMultiArrays(Node: TASTNode);
// Pre-scan: record every array name that appears in a REDIM with more than one dimension.
var
  i, k: Integer;
  Decl, Dims: TASTNode;
  Nm: string;
begin
  if Node = nil then Exit;
  if Node.NodeType = antRedim then
    for k := 0 to Node.ChildCount - 1 do
    begin
      Decl := Node.GetChild(k);
      if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 2) and
         (Decl.GetChild(0).NodeType = antIdentifier) and (Decl.GetChild(1).NodeType = antDimensions) and
         (Decl.GetChild(1).ChildCount > 1) then
      begin
        Nm := UpperCase(VarToStr(Decl.GetChild(0).Value));
        if FRedimMultiArrays.IndexOf(Nm) < 0 then FRedimMultiArrays.Add(Nm);
      end;
    end;
  for i := 0 to Node.ChildCount - 1 do
    CollectRedimMultiArrays(Node.GetChild(i));
end;

function TSSAGenerator.EmitArrayLinearIndex(const Indices: array of TSSAValue;
  const ArrInfo: TSSAArrayInfo; const ArrName: string): TSSAValue;
// Row-major linear index for a multi-dimensional array access. For arrays that are multi-dim REDIM'd
// (FRedimMultiArrays) the strides change at runtime, so push each index and resolve from the array's
// CURRENT dimensions (bcArrayIdxPush/Resolve); every other array keeps the fast compile-time const path.
var
  i, j, TempReg, ai: Integer;
  Stride: Int64;
  StrideVal, MulResult, AddResult, LinIdx: TSSAValue;
begin
  if Length(Indices) = 1 then Exit(Indices[0]);

  if FRedimMultiArrays.IndexOf(UpperCase(ArrName)) >= 0 then
  begin
    ai := FProgram.FindArray(ArrName);
    for i := 0 to High(Indices) do
      EmitInstruction(ssaArrayIdxPush, MakeSSAValue(svkNone), Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    LinIdx := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaArrayIdxResolve, LinIdx,
                    MakeSSAArrayRef(ai, FProgram.GetArray(ai).ElementType),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit(LinIdx);
  end;

  // Compile-time const-folded strides (row-major).
  TempReg := FProgram.AllocRegister(srtInt);
  LinIdx := MakeSSARegister(srtInt, TempReg);
  EmitInstruction(ssaLoadConstInt, LinIdx, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  for i := 0 to High(Indices) do
  begin
    Stride := 1;
    for j := i + 1 to High(ArrInfo.Dimensions) do
      Stride := Stride * ArrInfo.Dimensions[j];
    if Stride = 1 then
    begin
      AddResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaAddInt, AddResult, LinIdx, Indices[i], MakeSSAValue(svkNone));
      LinIdx := AddResult;
    end
    else
    begin
      StrideVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      MulResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));
      AddResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaAddInt, AddResult, LinIdx, MulResult, MakeSSAValue(svkNone));
      LinIdx := AddResult;
    end;
  end;
  Result := LinIdx;
end;

function TSSAGenerator.IsWStringExpr(Node: TASTNode): Boolean;
// True if the expression yields a WSTRING value: a WSTRING variable, a WSTR(...) conversion, a
// parenthesised WSTRING, or a string concatenation/expression with a WSTRING operand (the result of
// '&' on wide data is still wide). Conservative: anything else is treated as a plain (byte) STRING.
var
  NameU: string;
begin
  Result := False;
  if Node = nil then Exit;
  case Node.NodeType of
    antIdentifier:
      Result := IsWStringVar(VarToStr(Node.Value));
    antParentheses:
      Result := (Node.ChildCount >= 1) and IsWStringExpr(Node.GetChild(0));
    antMemberAccess:
      // obj.field where the field is declared AS WSTRING. Resolve the object's UDT type (no code
      // emitted) and look up the field. Value = field name, child 0 = the object expression.
      if Node.ChildCount >= 1 then
        Result := UDTFieldIsWString(FindUDT(ObjectTypeName(Node.GetChild(0))), VarToStr(Node.Value));
    antBinaryOp:
      // Concatenation (or any binary string op): wide if either operand is wide.
      Result := (Node.ChildCount >= 2) and
                (IsWStringExpr(Node.GetChild(0)) or IsWStringExpr(Node.GetChild(1)));
    antArrayAccess, antFunctionCall:
      // A call is wide if it is WSTR(...) (the canonical wide producer) or a call to a FUNCTION declared
      // AS WSTRING (its name was marked in FWStringVars). The call name is child0.Value (else Node.Value).
      begin
        if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
          NameU := UpperCase(VarToStr(Node.GetChild(0).Value))
        else
          NameU := UpperCase(VarToStr(Node.Value));
        Result := (NameU = kWSTR) or (NameU = kWCHR) or (NameU = kWSTRING) or IsWStringVar(NameU);
      end;
  end;
end;

function TSSAGenerator.IsAddrLocal(const Name: string): Boolean;
// An @-taken variable declared inside the CURRENT SUB/FUNCTION: backed by a per-frame 1-field record so
// its address is distinct per recursion level. FAddrLocalVars is rebuilt per procedure (populated by
// ProcessDim, cleared at the prologue), so a module-level @-taken var of the same name is never matched.
begin
  Result := FAddrLocalVars.IndexOfName(UpperCase(Name)) >= 0;
end;

function TSSAGenerator.AddrLocalBank(const Name: string): TSSARegisterType;
var
  idx: Integer;
begin
  Result := srtInt;
  idx := FAddrLocalVars.IndexOfName(UpperCase(Name));
  if idx >= 0 then Result := TypeNameToBank(FAddrLocalVars.ValueFromIndex[idx], Name);
end;

function TSSAGenerator.AddrLocalHandle(const Name: string): TSSAValue;
// The hidden INT variable holding this @-taken local's per-frame record handle. ProcessDim allocates
// the record and stores its handle here; reads/writes/@ of the local route through it. The name is
// forced to the int bank (its trailing letter would otherwise default to float, and a value-converting
// EnsureIntRegister would corrupt the handle bit pattern).
var
  HName: string;
begin
  HName := UpperCase(Name) + '$REC';
  if FVarExplicitType.IndexOf(HName) < 0 then
    FVarExplicitType.AddObject(HName, TObject(PtrInt(Ord(srtInt))));
  Result := GetOrAllocateVariable(HName);
end;

function TSSAGenerator.PointerUDTType(const PtrName: string): string;
// If PtrName is declared "DIM p AS T PTR" where T is a user UDT, return T; else ''. Such a pointer
// carries a record handle directly (the managed-reference model), so p.field / p->field resolve to
// record access on p's int value, and @obj / NEW T produce that handle.
var
  idx: Integer;
begin
  Result := '';
  idx := FPointerVars.IndexOfName(UpperCase(PtrName));
  if idx >= 0 then
  begin
    if FindUDT(UpperCase(FPointerVars.ValueFromIndex[idx])) >= 0 then
      Result := UpperCase(FPointerVars.ValueFromIndex[idx]);
  end;
end;

function TSSAGenerator.DerefedType(Node: TASTNode): string;
// The FreeBASIC type string of "*Node" (one dereference). Drives multi-level pointers: an identifier
// pointer yields its declared pointee (FPointerVars, e.g. "DOUBLE" or "DOUBLE PTR"); a nested deref
// "*(*p)" strips one more PTR level; pointer arithmetic follows the pointer operand of +/-. '' if Node
// is not dereferenceable.
var
  idx: Integer;
  T: string;
begin
  Result := '';
  if Node = nil then Exit;
  while (Node.NodeType = antParentheses) and (Node.ChildCount >= 1) do
    Node := Node.GetChild(0);
  if Node.NodeType = antIdentifier then
  begin
    idx := FPointerVars.IndexOfName(UpperCase(VarToStr(Node.Value)));
    if idx >= 0 then Result := UpperCase(FPointerVars.ValueFromIndex[idx]);
  end
  else if Node.NodeType = antDeref then
  begin
    // *(*inner): the type of *inner must itself be a pointer; deref it one more level.
    T := DerefedType(Node.GetChild(0));
    if (Length(T) > 4) and (Copy(T, Length(T) - 3, 4) = ' PTR') then
      Result := Trim(Copy(T, 1, Length(T) - 4));
  end
  else if (Node.NodeType = antBinaryOp) and (Node.ChildCount >= 2) then
  begin
    if (Node.GetChild(0).NodeType = antIdentifier) and
       (FPointerVars.IndexOfName(UpperCase(VarToStr(Node.GetChild(0).Value))) >= 0) then
      Result := DerefedType(Node.GetChild(0))
    else if (Node.GetChild(1).NodeType = antIdentifier) and
            (FPointerVars.IndexOfName(UpperCase(VarToStr(Node.GetChild(1).Value))) >= 0) then
      Result := DerefedType(Node.GetChild(1));
  end;
end;

function TSSAGenerator.DerefOperandBank(Node: TASTNode): TSSARegisterType;
// Bank of the pointee for "*<expr>". A "T PTR" pointee (a multi-level pointer) is an address → int
// bank; otherwise the declared scalar bank. Defaults to int when the type is unknown.
var
  T: string;
begin
  T := DerefedType(Node);
  if T = '' then
    Result := srtInt
  else
    Result := TypeNameToBank(T, '');
end;

procedure TSSAGenerator.EmitArrayElementAddress(Node: TASTNode; out Result: TSSAValue);
// @arr(i [,j...]) — the packed address of an array element. Node is the antArrayAccess subtree
// (child0 = array name identifier, child1 = index expression list). The base array id is resolved by
// name; the linear (row-major) element index is computed exactly like an array read, then folded into
// the low bits of the address: packedAddr = ((arrayId+1) shl POINTER_ARRAY_SHIFT) + linearIndex. This
// makes plain integer arithmetic on the pointer ("p + 1") advance by one element, FreeBASIC-style.
var
  ArrName: string;
  ArrayIdx, i, j: Integer;
  ArrInfo: TSSAArrayInfo;
  IndicesNode: TASTNode;
  Indices: array of TSSAValue;
  LinearIndex, TempVal, AddResult, StrideVal, MulResult, BaseVal: TSSAValue;
  TempReg, Stride: Integer;
begin
  ArrName := VarToStr(Node.GetChild(0).Value);
  ArrayIdx := FProgram.FindArray(ArrName);
  if ArrayIdx < 0 then
    raise Exception.CreateFmt('Cannot take address of element of undeclared array: %s', [ArrName]);
  ArrInfo := FProgram.GetArray(ArrayIdx);
  IndicesNode := Node.GetChild(1);  // antExpressionList of indices

  SetLength(Indices, IndicesNode.ChildCount);
  for i := 0 to IndicesNode.ChildCount - 1 do
  begin
    ProcessExpression(IndicesNode.GetChild(i), Indices[i]);
    if Indices[i].Kind = svkConstInt then
    begin
      TempVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if Indices[i].Kind = svkConstFloat then
    begin
      TempVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(Trunc(Indices[i].ConstFloat)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end
    else if (Indices[i].Kind = svkRegister) and (Indices[i].RegType = srtFloat) then
    begin
      TempVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaFloatToInt, TempVal, Indices[i], MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      Indices[i] := TempVal;
    end;
  end;

  // Map each index to a 0-based offset honoring explicit lower bounds (lb TO ub).
  for i := 0 to High(Indices) do
    if (i <= High(ArrInfo.LowerBounds)) and (ArrInfo.LowerBounds[i] <> 0) then
    begin
      TempVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaLoadConstInt, TempVal, MakeSSAConstInt(ArrInfo.LowerBounds[i]),
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      AddResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaSubInt, AddResult, Indices[i], TempVal, MakeSSAValue(svkNone));
      Indices[i] := AddResult;
    end;

  // Row-major linear index (same formula as antArrayAccess).
  if Length(Indices) = 1 then
    LinearIndex := Indices[0]
  else
  begin
    LinearIndex := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, LinearIndex, MakeSSAConstInt(0), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    for i := 0 to High(Indices) do
    begin
      Stride := 1;
      for j := i + 1 to High(ArrInfo.Dimensions) do
        Stride := Stride * ArrInfo.Dimensions[j];
      if Stride = 1 then
      begin
        AddResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, Indices[i], MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end
      else
      begin
        StrideVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaLoadConstInt, StrideVal, MakeSSAConstInt(Stride), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        MulResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaMulInt, MulResult, Indices[i], StrideVal, MakeSSAValue(svkNone));
        AddResult := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
        EmitInstruction(ssaAddInt, AddResult, LinearIndex, MulResult, MakeSSAValue(svkNone));
        LinearIndex := AddResult;
      end;
    end;
  end;

  // packedAddr = baseConst + linearIndex, baseConst = (arrayId+1) shl POINTER_ARRAY_SHIFT.
  BaseVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaLoadConstInt, BaseVal,
                  MakeSSAConstInt((Int64(ArrayIdx) + 1) shl POINTER_ARRAY_SHIFT),
                  MakeSSAValue(svkNone), MakeSSAValue(svkNone));
  TempReg := FProgram.AllocRegister(srtInt);
  Result := MakeSSARegister(srtInt, TempReg);
  EmitInstruction(ssaAddInt, Result, BaseVal, LinearIndex, MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.EmitNewObject(Node: TASTNode; out Result: TSSAValue);
// FreeBASIC "NEW T" / "NEW T(args)": allocate a record on the heap and run its constructor; evaluates
// to the record handle (int), assignable to a "T PTR". The record is allocated in the SHARED region
// (immediate bit 48) so it is NOT reclaimed at the allocating frame's exit — the pointer keeps it
// alive until DELETE. Member access through the pointer (p->field) routes via the handle as usual.
var
  NewType: string;
  UDTIdx: Integer;
begin
  NewType := UpperCase(VarToStr(Node.Value));
  UDTIdx := FindUDT(NewType);
  if UDTIdx < 0 then
    raise Exception.CreateFmt('NEW requires a known TYPE, got "%s"', [NewType]);
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaRecordNew, Result,
                  MakeSSAConstInt(FUDTs[UDTIdx].NInt), MakeSSAConstInt(FUDTs[UDTIdx].NFloat),
                  MakeSSAConstInt(FUDTs[UDTIdx].NStr or (Int64(UDTIdx) shl 32) or (Int64(1) shl 48)));
  EmitRecordInit(Result, UDTIdx);                 // allocate nested-UDT members
  if (Node.ChildCount > 0) and (Node.GetChild(0).NodeType = antArgumentList) then
    EmitConstructorCall(Result, NewType, Node.GetChild(0))
  else
    EmitConstructorCall(Result, NewType);
end;

procedure TSSAGenerator.EmitDeleteObject(Node: TASTNode);
// FreeBASIC "DELETE p": run the pointee's destructor (if any), then release the heap record and
// recycle its slot (ssaRecordFree). Node child0 = the pointer expression (must be a UDT pointer).
var
  PtrName, PtrType: string;
  HandleReg: TSSAValue;
begin
  if Node.ChildCount < 1 then Exit;
  if Node.GetChild(0).NodeType <> antIdentifier then
    raise Exception.Create('DELETE expects a pointer variable');
  PtrName := VarToStr(Node.GetChild(0).Value);
  PtrType := PointerUDTType(PtrName);
  if PtrType = '' then
    raise Exception.CreateFmt('DELETE expects a UDT pointer, "%s" is not one', [PtrName]);
  HandleReg := EnsureIntRegister(GetOrAllocateVariable(UpperCase(PtrName)));
  EmitDestructorCall(HandleReg, PtrType);
  EmitInstruction(ssaRecordFree, MakeSSAValue(svkNone), HandleReg, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

procedure TSSAGenerator.EmitFieldAddress(MemberNode: TASTNode; out Result: TSSAValue);
// @obj.field — the address of a record field. Resolve the owning record's handle (ResolveRecordObject,
// which handles obj / arr(i) / nested a.b) and the field's slot, then emit ssaRefAddrField to pack a
// record-field pointer (RECPTR_TAG | handle | slot) at runtime. The pointee bank is carried by the
// pointer's declared type (FPointerVars), as for any other pointer.
var
  TypeName, NestedT: string;
  UDTIdx, Slot: Integer;
  Bank: TSSARegisterType;
  HandleVal: TSSAValue;
begin
  Result := MakeSSAValue(svkNone);
  if MemberNode.ChildCount < 1 then Exit;
  TypeName := ObjectTypeName(MemberNode.GetChild(0));
  if TypeName = '' then
    raise Exception.Create('Cannot take address of field: object is not a record');
  UDTIdx := FindUDT(TypeName);
  if not UDTFieldBankSlot(UDTIdx, VarToStr(MemberNode.Value), Bank, Slot, NestedT) then
    raise Exception.CreateFmt('Cannot take address of unknown field "%s" of type %s',
                              [VarToStr(MemberNode.Value), TypeName]);
  if not ResolveRecordObject(MemberNode.GetChild(0), HandleVal, TypeName) then
    raise Exception.Create('Cannot take address of field: unresolved record object');
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaRefAddrField, Result, HandleVal, MakeSSAValue(svkNone), MakeSSAConstInt(Slot));
end;

function TSSAGenerator.EmitPointerIndexAddress(const PtrName: string; IndicesNode: TASTNode): TSSAValue;
// p[i] (FreeBASIC pointer indexing) ≡ *(p + i): returns the address register p + i. For a MANAGED
// pointer the low bits are the element offset, so adding i advances one element. For a RAW (Allocate'd)
// pointer the address is a byte offset, so the index is scaled by SizeOf(pointee) — FreeBASIC semantics.
var
  PtrReg, IdxVal, SzVal, ScaledIdx: TSSAValue;
begin
  PtrReg := EnsureIntRegister(GetOrAllocateVariable(PtrName));
  ProcessExpression(IndicesNode.GetChild(0), IdxVal);
  IdxVal := EnsureIntRegister(IdxVal);
  if IsRawPtr(PtrName) and (RawElemSizeOf(PtrName) > 1) then
  begin
    SzVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaLoadConstInt, SzVal, MakeSSAConstInt(RawElemSizeOf(PtrName)), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    ScaledIdx := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaMulInt, ScaledIdx, IdxVal, SzVal, MakeSSAValue(svkNone));
    IdxVal := ScaledIdx;
  end;
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  EmitInstruction(ssaAddInt, Result, PtrReg, IdxVal, MakeSSAValue(svkNone));
end;

function TSSAGenerator.EmitVarAddress(const Name: string): TSSAValue;
// The packed address of an address-backed scalar (its 1-element backing array, offset 0). The id is
// folded into the high bits: ((arrayId+1) shl POINTER_ARRAY_SHIFT); 0 stays NULL. Emits a constant
// load. Returns a 0 constant when the variable is not backed (a deref would then fault).
var
  idx: Integer;
begin
  idx := FSharedScalarArr.IndexOf(UpperCase(Name));
  Result := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
  if idx >= 0 then
    EmitInstruction(ssaLoadConstInt, Result,
                    MakeSSAConstInt((Int64(PtrInt(FSharedScalarArr.Objects[idx])) + 1) shl POINTER_ARRAY_SHIFT),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone))
  else
    EmitInstruction(ssaLoadConstInt, Result, MakeSSAConstInt(0),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
end;

function TSSAGenerator.IsByrefRetFunc(const Name: string): Boolean;
begin
  Result := FByrefRetFuncs.IndexOfName(UpperCase(Name)) >= 0;
end;

function TSSAGenerator.ByrefRetPointeeBank(const Name: string): TSSARegisterType;
var
  idx: Integer;
begin
  Result := srtInt;
  idx := FByrefRetFuncs.IndexOfName(UpperCase(Name));
  if idx >= 0 then
    Result := TypeNameToBank(FByrefRetFuncs.ValueFromIndex[idx], Name);
end;

procedure TSSAGenerator.EmitSharedSyncOut;
// M6: store each shared global's current register value into its dedicated transfer slot, so a
// callee (or the caller, after the call) reads the up-to-date value across the frame save/restore.
var
  i: Integer;
  V: TSSAValue;
begin
  if (FSharedVars = nil) or (FSharedVars.Count = 0) then Exit;
  for i := 0 to FSharedVars.Count - 1 do
  begin
    if IsSharedScalar(FSharedVars[i]) then Continue;   // array-backed: lives in shared FArrays, no slot sync
    V := GetOrAllocateVariable(FSharedVars[i]);
    EmitXferStore(V.RegType, PtrInt(FSharedVars.Objects[i]), V);
  end;
end;

procedure TSSAGenerator.EmitSharedSyncIn;
// M6: load each shared global's dedicated transfer slot back into its register (at a SUB prologue,
// and in the caller right after a call returns) so the register reflects the latest shared value.
var
  i: Integer;
  V: TSSAValue;
begin
  if (FSharedVars = nil) or (FSharedVars.Count = 0) then Exit;
  for i := 0 to FSharedVars.Count - 1 do
  begin
    if IsSharedScalar(FSharedVars[i]) then Continue;   // array-backed: lives in shared FArrays, no slot sync
    V := GetOrAllocateVariable(FSharedVars[i]);
    EmitXferLoad(V.RegType, PtrInt(FSharedVars.Objects[i]), V);
  end;
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

procedure TSSAGenerator.EmitUserFunctionCall(const Name: string; ArgsNode: TASTNode;
  out Result: TSSAValue);
// Lower a call to a user FUNCTION/SUB and produce its result value. A scalar result is read from
// the result transfer slot as before. A UDT result uses V3 return-by-value: the caller allocates
// the result instance HERE (so it lives in the caller's frame and survives the callee's frame-
// record reclamation), passes its handle in XFER_RESULT_HANDLE_SLOT, and the callee copies the
// return value into it. Ordering matters: allocate the result instance, stage the arguments
// (nested calls reuse the reserved slots and must finish first), then stage our result handle.
var
  FuncRetType: TSSARegisterType;
  RecType: string;
  UDTIdx: Integer;
  RcHandle, AddrVal: TSSAValue;
begin
  RecType := VarRecordTypeName(Name);
  if RecType <> '' then
  begin
    UDTIdx := FindUDT(RecType);
    RcHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitInstruction(ssaRecordNew, RcHandle,
                    MakeSSAConstInt(FUDTs[UDTIdx].NInt), MakeSSAConstInt(FUDTs[UDTIdx].NFloat),
                    MakeSSAConstInt(FUDTs[UDTIdx].NStr or (Int64(UDTIdx) shl 32)));
    EmitRecordInit(RcHandle, UDTIdx);                 // allocate nested-UDT members (no ctor calls)
    StageCallArgs(Name, ArgsNode);                    // evaluate args first (inner calls finish)
    EmitXferStore(srtInt, XFER_RESULT_HANDLE_SLOT, RcHandle);
    EmitCallSubLabel(ProcedureLabelName(Name));
    EmitByrefWriteback(Name, ArgsNode);   // BYREF: copy explicit-BYREF scalar params back into variable args
    Result := RcHandle;
  end
  else if IsByrefRetFunc(Name) then
  begin
    // FreeBASIC BYREF result used as an rvalue: the function returns an address; load it and then
    // dereference through it to read the pointee value. (Lvalue use "(f())=rhs" is handled in
    // ProcessAssignment, which stores through the same address instead.)
    EmitProcedureCall(Name, ArgsNode);
    AddrVal := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
    EmitXferLoad(srtInt, XFER_RESULT_SLOT, AddrVal);
    FuncRetType := ByrefRetPointeeBank(Name);
    Result := MakeSSARegister(FuncRetType, FProgram.AllocRegister(FuncRetType));
    case FuncRetType of
      srtFloat:  EmitInstruction(ssaRefLoadFloat, Result, AddrVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      srtString: EmitInstruction(ssaRefLoadString, Result, AddrVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    else
      EmitInstruction(ssaRefLoadInt, Result, AddrVal, MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
  end
  else
  begin
    EmitProcedureCall(Name, ArgsNode);
    FuncRetType := GetVariableType(Name);
    Result := MakeSSARegister(FuncRetType, FProgram.AllocRegister(FuncRetType));
    EmitXferLoad(FuncRetType, XFER_RESULT_SLOT, Result);
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
  begin
    Result := VarRecordTypeName(VarToStr(ObjNode.Value));
    if Result = '' then Result := PointerUDTType(VarToStr(ObjNode.Value));  // UDT pointer: p.field / p->field
  end
  else if ObjNode.NodeType = antDeref then
    Result := PointerUDTType(VarToStr(ObjNode.GetChild(0).Value))           // (*p).field
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
    begin
      Result := NestedT;
      // A pointer field (T PTR) holds a handle to a T — used for chained access "p->nxt->val".
      if Result = '' then Result := UDTFieldPtrPointee(FindUDT(ParentType), VarToStr(ObjNode.Value));
    end;
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
  i, UDTIdx: Integer;
  RT: TSSARegisterType;
  DestVal, RcHandle: TSSAValue;
  IsFunc: Boolean;
  MethodLabel, RetRecType: string;
begin
  Result := MakeSSAValue(svkNone);
  MethodLabel := ResolveMethodLabel(ObjType, MethNm);   // static (base) target
  if MethodLabel = '' then Exit;
  RetRecType := VarRecordTypeName(MethodLabel);          // V3: '' unless it returns a UDT by value

  // Build an argument list with the object (THIS) prepended.
  TmpArgs := TASTNode.Create(antArgumentList, ObjNode.Token);
  try
    TmpArgs.AddChild(ObjNode.Clone);
    if Assigned(ArgsNode) then
      for i := 0 to ArgsNode.ChildCount - 1 do
        TmpArgs.AddChild(ArgsNode.GetChild(i).Clone);
    // V3 return-by-value: allocate the result instance (caller frame) before staging args; the
    // method copies its return value into it (the handle flows in XFER_RESULT_HANDLE_SLOT).
    if RetRecType <> '' then
    begin
      UDTIdx := FindUDT(RetRecType);
      RcHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitInstruction(ssaRecordNew, RcHandle,
                      MakeSSAConstInt(FUDTs[UDTIdx].NInt), MakeSSAConstInt(FUDTs[UDTIdx].NFloat),
                      MakeSSAConstInt(FUDTs[UDTIdx].NStr or (Int64(UDTIdx) shl 32)));
      EmitRecordInit(RcHandle, UDTIdx);
    end;
    StageCallArgs(MethodLabel, TmpArgs);                 // THIS + declared args (base signature)
    if RetRecType <> '' then
      EmitXferStore(srtInt, XFER_RESULT_HANDLE_SLOT, RcHandle);
    if MethodNeedsDispatch(ObjType, MethNm) then
    begin
      // Polymorphic: call the virtual dispatcher (it forwards the staged args + result handle).
      FNeededDispatchers.Add(UpperCase(ObjType) + '|' + UpperCase(MethNm));
      EmitCallSubLabel(ProcedureLabelName('VDISP.' + UpperCase(ObjType) + '.' + UpperCase(MethNm)));
    end
    else
      EmitCallSubLabel(ProcedureLabelName(MethodLabel));  // monomorphic: direct static call
    // BYREF: copy explicit-BYREF scalar params back into variable args. TmpArgs aligns 1:1 with the
    // method's params (THIS at index 0, never BYREF), so the shared writeback helper works as-is.
    EmitByrefWriteback(MethodLabel, TmpArgs);
  finally
    TmpArgs.Free;
  end;

  if RetRecType <> '' then
  begin
    Result := RcHandle;   // UDT result: the caller-allocated copy
    Exit;
  end;
  IsFunc := False;
  if FProcDecls.TryGetValue(MethodLabel, Decl) and Assigned(Decl) then
    IsFunc := UpperCase(VarToStr(Decl.Value)) = kFUNCTION;
  if IsFunc then
  begin
    RT := GetVariableType(MethodLabel);   // method return bank (record handle => int)
    DestVal := MakeSSARegister(RT, FProgram.AllocRegister(RT));
    EmitXferLoad(RT, XFER_RESULT_SLOT, DestVal);
    Result := DestVal;
  end;
end;

function TSSAGenerator.TryStaticMethodCall(ObjNode: TASTNode; const MethNm: string;
  ArgsNode: TASTNode; out CallResult: TSSAValue): Boolean;
// FreeBASIC static member method: "TypeName.method(args)" invoked WITHOUT an instance. ObjNode names a
// declared UDT (not a variable). The method is invoked with a dummy THIS (handle 0) — a static method
// does not dereference THIS, so the slot is harmless. Returns False when this is not a static-method
// call (ObjNode is not a type name, or the type has no such method) so the caller can fall through.
var
  TypeName: string;
  DummyThis: TASTNode;
begin
  Result := False;
  CallResult := MakeSSAValue(svkNone);
  if (ObjNode = nil) or (ObjNode.NodeType <> antIdentifier) then Exit;
  TypeName := UpperCase(VarToStr(ObjNode.Value));
  if FindUDT(TypeName) < 0 then Exit;                        // not a declared type name
  if ResolveMethodLabel(TypeName, MethNm) = '' then Exit;    // type has no such method
  DummyThis := TASTNode.CreateWithValue(antLiteral, 0, ObjNode.Token);
  try
    ProcessMethodCall(DummyThis, TypeName, MethNm, ArgsNode, CallResult);
  finally
    DummyThis.Free;
  end;
  Result := True;
end;

function TSSAGenerator.ResolveRecordObject(ObjNode: TASTNode; out HandleVal: TSSAValue;
  out TypeName: string): Boolean;
var
  ArrName, ParentType, NestedT: string;
  ParentHandle, NestedHandle: TSSAValue;
  ParentUDT, Slot: Integer;
  Bank: TSSARegisterType;
  SharedTmp: TASTNode;
begin
  Result := False;
  TypeName := '';
  if ObjNode.NodeType = antIdentifier then
  begin
    // Record variable (or record-typed parameter): handle is the variable's int register.
    TypeName := VarRecordTypeName(VarToStr(ObjNode.Value));
    if TypeName = '' then
    begin
      // UDT pointer (DIM p AS T PTR): p's int value IS the record handle (managed-reference model).
      TypeName := PointerUDTType(VarToStr(ObjNode.Value));
      if TypeName = '' then Exit;
      HandleVal := EnsureIntRegister(GetOrAllocateVariable(UpperCase(VarToStr(ObjNode.Value))));
      Result := True;
      Exit;
    end;
    // Refinement #2: a SHARED UDT scalar keeps its (shared) record handle in element 0 of its backing
    // array, so read the handle from there — the record lives in the shared region (cross-thread).
    if IsSharedScalar(VarToStr(ObjNode.Value)) then
    begin
      SharedTmp := MakeSharedScalarAccess(VarToStr(ObjNode.Value), ObjNode.Token);
      ProcessExpression(SharedTmp, HandleVal);
      SharedTmp.Free;
    end
    else
      HandleVal := GetOrAllocateVariable(UpperCase(VarToStr(ObjNode.Value)));
    Result := True;
  end
  else if ObjNode.NodeType = antDeref then
  begin
    // (*p).field where p is a UDT pointer: the deref yields p's handle directly.
    TypeName := PointerUDTType(VarToStr(ObjNode.GetChild(0).Value));
    if TypeName = '' then Exit;
    HandleVal := EnsureIntRegister(GetOrAllocateVariable(UpperCase(VarToStr(ObjNode.GetChild(0).Value))));
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
    // Chained access (a.b.c): the parent (a.b) is itself a nested-UDT field, or a pointer field
    // (a->b where b is "T PTR"). Either way the field stores an int handle to the target record; load
    // it and continue. For a pointer field the handle is the pointee; for a nested record it is the
    // embedded instance.
    if not ResolveRecordObject(ObjNode.GetChild(0), ParentHandle, ParentType) then Exit;
    ParentUDT := FindUDT(ParentType);
    if not UDTFieldBankSlot(ParentUDT, VarToStr(ObjNode.Value), Bank, Slot, NestedT) then Exit;
    if NestedT = '' then
      NestedT := UDTFieldPtrPointee(ParentUDT, VarToStr(ObjNode.Value));
    if NestedT = '' then Exit;   // parent.field is neither a record nor a UDT pointer
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
  TypeName, NestedT, MethodLbl, SMBack: string;
  UDTIdx, Slot: Integer;
  Bank: TSSARegisterType;
  HandleVal, DestVal: TSSAValue;
  Op: TSSAOpCode;
  AccNode: TASTNode;
begin
  Result := MakeSSAValue(svkNone);
  if Node.ChildCount < 1 then Exit;
  // OOP static member variable (via type name or instance): read from its shared global scalar.
  SMBack := StaticMemberBackingName(Node.GetChild(0), VarToStr(Node.Value));
  if SMBack <> '' then
  begin
    AccNode := MakeSharedScalarAccess(SMBack, Node.Token);
    try ProcessExpression(AccNode, Result); finally AccNode.Free; end;
    Exit;
  end;
  TypeName := ObjectTypeName(Node.GetChild(0));
  if TypeName = '' then
  begin
    // Static member method called with no args via the type name: "TypeName.method".
    TryStaticMethodCall(Node.GetChild(0), VarToStr(Node.Value), nil, Result);
    Exit;
  end;
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
// Lower "obj.field = expr" to ssaRecordStore<bank>(handle, value, slot). If the member is not a field
// but a PROPERTY setter (FreeBASIC), lower a method call obj.<prop>.SET(expr) instead.
var
  TypeName, NestedT, SMBack: string;
  UDTIdx, Slot: Integer;
  Bank: TSSARegisterType;
  HandleVal, ExprVal, DummyVal: TSSAValue;
  Op: TSSAOpCode;
  SetterArgs, StoreAssign: TASTNode;
begin
  if MemberNode.ChildCount < 1 then Exit;
  // OOP static member variable (via type name or instance): store to its shared global scalar.
  SMBack := StaticMemberBackingName(MemberNode.GetChild(0), VarToStr(MemberNode.Value));
  if SMBack <> '' then
  begin
    StoreAssign := TASTNode.Create(antAssignment, MemberNode.Token);
    StoreAssign.AddChild(MakeSharedScalarAccess(SMBack, MemberNode.Token));
    StoreAssign.AddChild(ExprNode.Clone);
    try ProcessArrayStore(StoreAssign); finally StoreAssign.Free; end;
    Exit;
  end;
  // Evaluate the RHS first, then resolve the target (object handle). Order matters only for
  // side effects; both are emitted before the store.
  if not ResolveRecordObject(MemberNode.GetChild(0), HandleVal, TypeName) then Exit;
  UDTIdx := FindUDT(TypeName);
  if not UDTFieldBankSlot(UDTIdx, VarToStr(MemberNode.Value), Bank, Slot, NestedT) then
  begin
    // Not a field — a PROPERTY setter? obj.prop = expr -> SUB Type.prop.SET(expr).
    if ResolveMethodLabel(TypeName, VarToStr(MemberNode.Value) + '.SET') <> '' then
    begin
      SetterArgs := TASTNode.Create(antArgumentList, MemberNode.Token);
      SetterArgs.AddChild(ExprNode.Clone);
      ProcessMethodCall(MemberNode.GetChild(0), TypeName, VarToStr(MemberNode.Value) + '.SET',
                        SetterArgs, DummyVal);
      SetterArgs.Free;
    end;
    Exit;
  end;

  ProcessExpression(ExprNode, ExprVal);
  // B1.5: a field declared with a narrow integer type or SINGLE wraps/rounds the value on store.
  case Bank of
    srtFloat:  begin ExprVal := EnsureFloatRegister(ExprVal);  ExprVal := ApplyNarrowCode(UDTFieldWidthCode(UDTIdx, VarToStr(MemberNode.Value)), ExprVal); Op := ssaRecordStoreFloat; end;
    srtString: begin ExprVal := EnsureStringRegister(ExprVal); Op := ssaRecordStoreString; end;
  else
    begin ExprVal := EnsureIntRegister(ExprVal); ExprVal := ApplyNarrowCode(UDTFieldWidthCode(UDTIdx, VarToStr(MemberNode.Value)), ExprVal); Op := ssaRecordStoreInt; end;
  end;
  EmitInstruction(Op, MakeSSAValue(svkNone), HandleVal, ExprVal, MakeSSAConstInt(Slot));
end;

procedure TSSAGenerator.CollectProcedureDecl(Node: TASTNode);
// Record a SUB/FUNCTION declaration for deferred lowering (see LowerDeferredProcedures).
// AST layout: child 0 = name (antIdentifier), child 1 = antParameterList, rest = body.
var
  NameNode: TASTNode;
  Name, Pointee: string;
  n: Integer;
begin
  if Node.ChildCount = 0 then Exit;
  NameNode := Node.GetChild(0);
  if NameNode.NodeType <> antIdentifier then Exit;
  Name := UpperCase(VarToStr(NameNode.Value));
  // M4.4g: re-encode a CONSTRUCTOR's label with its parameter TYPE signature (the parser only knew
  // the arity). "TYPE.CONSTRUCTOR#<arity>" -> "TYPE.CONSTRUCTOR#<sig>" (e.g. "#IS"), so same-arity
  // different-type overloads get distinct labels. Done here, before keying FProcDecls, so the whole
  // pipeline (resolution + lowering) sees the signature label.
  if (Pos('.CONSTRUCTOR#', Name) > 0) and (Node.ChildCount >= 2) and
     (Node.GetChild(1).NodeType = antParameterList) then
  begin
    Name := Copy(Name, 1, Pos('#', Name)) + CtorSigFromParams(Node.GetChild(1));
    NameNode.Value := Name;
  end;
  if FProcDecls.ContainsKey(Name) then Exit;  // already registered by the pre-scan
  n := Length(FDeferredProcs);
  SetLength(FDeferredProcs, n + 1);
  FDeferredProcs[n] := Node;
  if FProcedureNames.IndexOf(Name) < 0 then
    FProcedureNames.Add(Name);
  FProcDecls.AddOrSetValue(Name, Node);
  // FreeBASIC BYREF function result: the function returns an address; record its pointee type (the
  // declared return type, attached as the name node's type child by the parser).
  if (Node.Attributes.Values['BYREFRET'] = '1') and (FByrefRetFuncs.IndexOfName(Name) < 0) then
  begin
    Pointee := 'INTEGER';
    if (NameNode.ChildCount >= 1) and (NameNode.GetChild(0).NodeType = antIdentifier) then
      Pointee := UpperCase(VarToStr(NameNode.GetChild(0).Value));
    FByrefRetFuncs.Add(Name + '=' + Pointee);
  end;
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
  Decl, ParamList, ArgExpr, ParamI: TASTNode;
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
      Slot := ParamBankAndSlot(ParamList, i, RT);
      // BYREF-return function with a BYREF (int) param: pass the ADDRESS of the argument variable, not
      // its value, so the function can return a reference into the caller's variable (min(a,b)=0). Gated
      // to int-banked params (address and slot are both int). The arg was address-backed by
      // CollectAddressTakenVars; EmitVarAddress yields its stable packed address.
      if IsByrefRetFunc(ParamOwnerName) and (RT = srtInt) and
         (ParamList.GetChild(i).Attributes.Values['BYREF'] = '1') and
         (ArgExpr.NodeType = antIdentifier) then
        EmitXferStore(srtInt, Slot, EmitVarAddress(VarToStr(ArgExpr.Value)))
      else
      begin
        ProcessExpression(ArgExpr, ArgVal);
        EmitXferStore(RT, Slot, ArgVal);
      end;
    end;
    // M7: default arguments — for each trailing parameter the call omitted, stage its default value
    // (the parameter node's last child, marked 'HASDEFAULT'), evaluated in the caller's context.
    for i := NArgs to ParamList.ChildCount - 1 do
    begin
      ParamI := ParamList.GetChild(i);
      if (ParamI.Attributes.Values['HASDEFAULT'] = '1') and (ParamI.ChildCount >= 1) then
      begin
        ProcessExpression(ParamI.GetChild(ParamI.ChildCount - 1), ArgVal);
        Slot := ParamBankAndSlot(ParamList, i, RT);
        EmitXferStore(RT, Slot, ArgVal);
      end;
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
  // M6: push shared globals to their slots before the call so the callee sees the latest values
  // (the callee's prologue loads them back). Skipped inside a virtual dispatcher, which only forwards
  // and never holds the shared registers — syncing there would clobber the slots with stale data.
  if not FInDispatcher then EmitSharedSyncOut;
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
  // M6: reload shared globals from their slots so the caller observes any change the callee made.
  if not FInDispatcher then EmitSharedSyncIn;
end;

procedure TSSAGenerator.EmitProcedureCall(const Name: string; ArgListNode: TASTNode);
// Static call: stage args then ssaCallSub to the named procedure.
begin
  if not Assigned(FCurrentBlock) then
    FCurrentBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel('call'));
  StageCallArgs(Name, ArgListNode);
  EmitCallSubLabel(ProcedureLabelName(Name));
  EmitByrefWriteback(Name, ArgListNode);   // BYREF: copy explicit-BYREF scalar params back into variable args
end;

procedure TSSAGenerator.ProcessProcedureCall(Node: TASTNode);
// Lower a statement-level CALL: stage args + ssaCallSub. Execution resumes in the
// return-point block created by EmitProcedureCall.
var
  ArgList: TASTNode;
  ParentType: string;
  OwnerUDT: Integer;
  PtrVal: TSSAValue;
begin
  ArgList := nil;
  if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antArgumentList) then
    ArgList := Node.GetChild(0);
  // FreeBASIC "Deallocate(p)": free the raw byte block p points at. (Not a declared SUB; intercepted.)
  if (UpperCase(VarToStr(Node.Value)) = 'DEALLOCATE') and Assigned(ArgList) and (ArgList.ChildCount >= 1) then
  begin
    ProcessExpression(ArgList.GetChild(0), PtrVal);
    EmitInstruction(ssaRawFree, MakeSSAValue(svkNone), EnsureIntRegister(PtrVal), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    Exit;
  end;
  // M4.4f: BASE[(args)] inside a constructor body calls the owner type's parent constructor on THIS.
  // It is normally HOISTED into the prologue (base-first) by LowerDeferredProcedures, which marks the
  // node 'HOISTED' — so here we just skip it. (The fallback path below covers any unhoisted BASE,
  // e.g. one not reached by the prologue scan.)
  if (UpperCase(VarToStr(Node.Value)) = 'BASE') then
  begin
    if Node.Attributes.Values['HOISTED'] = '1' then Exit;   // already run in the prologue
    if FCurrentThisType <> '' then
    begin
      OwnerUDT := FindUDT(FCurrentThisType);
      if OwnerUDT >= 0 then
      begin
        ParentType := FUDTs[OwnerUDT].Parent;
        if ParentType <> '' then
          EmitConstructorCall(GetOrAllocateVariable('THIS'), ParentType, ArgList);
      end;
    end;
    Exit;
  end;
  EmitProcedureCall(UpperCase(VarToStr(Node.Value)), ArgList);
end;

procedure TSSAGenerator.LowerDeferredProcedures;
// Lower every collected SUB/FUNCTION body into its own block region. Must be called AFTER
// the module END is emitted so procedure blocks sit beyond it (never reached by fall-through;
// only via ssaCallSub). Each body ends with ssaReturnSub.
var
  i, j, Slot, PUDT, OwnerUDT, WIdx: Integer;
  Proc, NameNode, ParamList, ParamNodeJ, BaseCallNode: TASTNode;
  Name, LabelName, ParentType: string;
  RT: TSSARegisterType;
  ParamReg, LcHandle: TSSAValue;
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
    FCurrentProcIsFunction := (UpperCase(VarToStr(Proc.Value)) = kFUNCTION);
    FCurrentProcRetType := GetVariableType(Name);
    FCurrentProcByrefRet := IsByrefRetFunc(Name);         // BYREF result: the function returns an address
    FCurrentProcRetRecType := VarRecordTypeName(Name);   // V3: '' unless it returns a UDT by value
    FCurrentResultHandle := MakeSSAValue(svkNone);
    FCurrentProcLocalRecs.Clear;                          // V5: gather DIM'd local UDTs for dtors
    FCurrentProcByvalRecs.Clear;                          // V5d: BYVAL UDT param copies (filled in prologue)
    FCurrentProcByrefScalars.Clear;                       // BYREF: explicit-BYREF scalar params (filled in prologue)
    FCurrentProcAddrParams.Clear;                         // BYREF-return: address-carrying params (filled in prologue)
    FAddrLocalVars.Clear;                                 // @-taken locals of THIS proc (filled by ProcessDim)
    CollectTopLevelLabels(Proc, 2);                       // GOTO-unwind: this proc's block-depth-0 labels (body starts at child 2)
    CollectLocalRecordVars(Proc);
    // Method body (M4.1): the owner type (before the '.') is THIS's type while lowering here.
    if Pos('.', Name) > 0 then
      FCurrentThisType := Copy(Name, 1, Pos('.', Name) - 1)
    else
      FCurrentThisType := '';

    // FB lexical scope (MODERN): open the procedure-root scope frame. Parameters, THIS, the FUNCTION
    // result handle and the body's locals/implicit names bind here; resolution stops at this frame for
    // non-shared names, so a plain module DIM is invisible inside the procedure (DIM SHARED still
    // resolves to the module register). Popped after the body + frame destructors.
    if FModernMode then ScopePushFrame(skProcRoot);

    // Entry block for the procedure body.
    FCurrentBlock := FProgram.GetOrCreateBlock(LabelName);

    // Prologue: copy each parameter from its transfer slot into the parameter register
    // (the callee's local for that name). Child 1 = antParameterList.
    if (Proc.ChildCount >= 2) and (Proc.GetChild(1).NodeType = antParameterList) then
    begin
      ParamList := Proc.GetChild(1);
      for j := 0 to ParamList.ChildCount - 1 do
      begin
        ParamNodeJ := ParamList.GetChild(j);
        ParamReg := GetOrAllocateVariable(VarToStr(ParamNodeJ.Value));
        Slot := ParamBankAndSlot(ParamList, j, RT);
        EmitXferLoad(RT, Slot, ParamReg);
        // B1.5: a parameter declared with a narrow integer type (child 0 = AS-type identifier) wraps
        // the incoming argument to that width, in place. Only when it lands in the int bank.
        if (RT = srtInt) and (ParamNodeJ.ChildCount >= 1) and
           (ParamNodeJ.GetChild(0).NodeType = antIdentifier) then
        begin
          WIdx := TypeNameWidthCode(VarToStr(ParamNodeJ.GetChild(0).Value));
          if (WIdx >= 1) and (WIdx <= 6) then
            EmitInstruction(ssaNarrowInt, ParamReg, ParamReg, MakeSSAValue(svkNone), MakeSSAConstInt(WIdx));
        end;
        // V4: a BYVAL UDT parameter gets its own copy (the caller passed a handle = BYREF default,
        // so copy the caller's record into a fresh local instance that the body mutates instead).
        // The local copy lives in the callee frame and is reclaimed at frame exit (V2).
        if (ParamNodeJ.Attributes.Values['BYVAL'] = '1') and (ParamNodeJ.ChildCount >= 1) then
        begin
          PUDT := FindUDT(UpperCase(VarToStr(ParamNodeJ.GetChild(0).Value)));
          if PUDT >= 0 then
          begin
            LcHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
            EmitInstruction(ssaRecordNew, LcHandle,
                            MakeSSAConstInt(FUDTs[PUDT].NInt), MakeSSAConstInt(FUDTs[PUDT].NFloat),
                            MakeSSAConstInt(FUDTs[PUDT].NStr or (Int64(PUDT) shl 32)));
            EmitRecordInit(LcHandle, PUDT);
            EmitRecordCopy(LcHandle, ParamReg, PUDT);              // copy caller's record in
            EmitInstruction(ssaCopyInt, ParamReg, LcHandle,        // param var := local copy handle
                            MakeSSAValue(svkNone), MakeSSAValue(svkNone));
            // V5d: this BYVAL copy is frame-owned -> destruct it at frame exit (after the locals).
            FCurrentProcByvalRecs.Add(UpperCase(VarToStr(ParamNodeJ.Value)) + '|' +
                                      UpperCase(VarToStr(ParamNodeJ.GetChild(0).Value)));
          end;
        end
        // BYREF-return function: an int BYREF param is an ADDRESS carrier (the caller staged @arg). The
        // param register holds the caller variable's stable address; reads/writes auto-deref through it
        // and "RETURN param" yields that address. No copy-out (the deref already hits the caller's cell).
        else if FCurrentProcByrefRet and (RT = srtInt) and (ParamNodeJ.Attributes.Values['BYREF'] = '1') and
                (ParamNodeJ.ChildCount >= 1) and (ParamNodeJ.GetChild(0).NodeType = antIdentifier) and
                (FindUDT(UpperCase(VarToStr(ParamNodeJ.GetChild(0).Value))) < 0) then
          FCurrentProcAddrParams.Add(UpperCase(VarToStr(ParamNodeJ.Value)) + '=' +
                                     UpperCase(VarToStr(ParamNodeJ.GetChild(0).Value)))
        // BYREF: an explicit-BYREF *scalar* parameter is written back to its slot at each return so the
        // caller can copy it into the variable argument. UDT params are excluded (they alias the
        // caller's instance through the handle — mutations already persist via the heap).
        else if (ParamNodeJ.Attributes.Values['BYREF'] = '1') and
                not ((ParamNodeJ.ChildCount >= 1) and (ParamNodeJ.GetChild(0).NodeType = antIdentifier) and
                     (FindUDT(UpperCase(VarToStr(ParamNodeJ.GetChild(0).Value))) >= 0)) then
          FCurrentProcByrefScalars.AddObject(UpperCase(VarToStr(ParamNodeJ.Value)),
                                             TObject(PtrInt((Ord(RT) shl 16) or Slot)));
      end;
    end;
    // V3: a FUNCTION returning a UDT by value receives the caller-allocated result instance handle
    // in a reserved int transfer slot; the return paths copy the value into it.
    if FCurrentProcRetRecType <> '' then
    begin
      FCurrentResultHandle := MakeSSARegister(srtInt, FProgram.AllocRegister(srtInt));
      EmitXferLoad(srtInt, XFER_RESULT_HANDLE_SLOT, FCurrentResultHandle);
    end;

    // M6: prologue — load shared globals from their slots into their registers, so the body sees the
    // caller's values (the frame save/restore otherwise hides them). Done before auto-chaining so any
    // base ctor call (which syncs around itself) starts from valid shared registers.
    EmitSharedSyncIn;

    // M4.4d/f: base-constructor initialisation runs in the prologue, BEFORE the ctor body — so the
    // base subobject is constructed first regardless of where it appears in the source. If the body
    // chains the base explicitly with BASE(args), that call is HOISTED here (and marked so the body
    // skips it); otherwise the parent's default (#0) constructor is auto-chained. The parent's own
    // prologue chains further up; inherited fields sit at the same prefix slots, so the base ctor
    // initialises them before the child overrides/extends them.
    if (Pos('.CONSTRUCTOR#', Name) > 0) and (FCurrentThisType <> '') then
    begin
      OwnerUDT := FindUDT(FCurrentThisType);
      if OwnerUDT >= 0 then
      begin
        ParentType := FUDTs[OwnerUDT].Parent;
        if ParentType <> '' then
        begin
          BaseCallNode := FindBaseCall(Proc);
          if Assigned(BaseCallNode) then
          begin
            // explicit BASE(args): run the chosen base ctor now, then mark the node hoisted so its
            // in-body occurrence is skipped (ProcessProcedureCall checks the attribute).
            if (BaseCallNode.ChildCount >= 1) and (BaseCallNode.GetChild(0).NodeType = antArgumentList) then
              EmitConstructorCall(GetOrAllocateVariable('THIS'), ParentType, BaseCallNode.GetChild(0))
            else
              EmitConstructorCall(GetOrAllocateVariable('THIS'), ParentType, nil);
            BaseCallNode.Attributes.Values['HOISTED'] := '1';
          end
          else if ResolveConstructorLabel(ParentType, '') <> '' then
            EmitConstructorCall(GetOrAllocateVariable('THIS'), ParentType, nil);  // default base ctor
        end;
      end;
    end;

    // Body statements begin at child index 2 (0 = name, 1 = antParameterList).
    for j := 2 to Proc.ChildCount - 1 do
      ProcessStatement(Proc.GetChild(j));

    // Guarantee a trailing return frame (covers a fall-off-the-end body).
    if not Assigned(FCurrentBlock) then
      FCurrentBlock := FProgram.GetOrCreateBlock(GenerateUniqueLabel(LabelName + '_END'));
    EmitFrameDestructors;   // V5: destroy local UDTs on the fall-through exit path
    EmitSharedSyncOut;      // M6: publish shared-global changes to their slots before returning
    EmitByrefParamStore;    // BYREF: publish explicit-BYREF scalar params back to their slots
    EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                    MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    if FModernMode then ScopePopFrame;   // FB scope: close the procedure-root frame
    FCurrentBlock := nil;   // procedure body terminated
    FInProcedure := False;
    FCurrentProcName := '';
    FCurrentThisType := '';
    FCurrentProcRetRecType := '';
    FCurrentResultHandle := MakeSSAValue(svkNone);
    FCurrentProcLocalRecs.Clear;
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
  ArgReg: TSSAValue;      // For SETDATE/SETTIME string argument
  SelImm: Integer;        // For SETDATE/SETTIME selector
  KeyNumVal, KeyTextVal, KeyNumReg, KeyTextReg: TSSAValue;  // For KEY command
  ExprResult, LineNumReg: TSSAValue;  // For TRAP command
  AddrVal, AddrReg, ValueReg: TSSAValue;  // For POKE command
  ExitKind: string;       // M2: EXIT/RETURN kind
  IsExitStmt: Boolean;    // M2
  IsContinueStmt: Boolean; // FreeBASIC CONTINUE
  ExitLevels, ExitLoopIdx, ExitAllDepth: Integer;  // multi-level EXIT/CONTINUE
  ExitLoopKind: TLoopKind;
  RetVal: TSSAValue;      // M2: RETURN expr / FUNCTION result
  OpCode: TSSAOpCode;     // M5.4: selected mutex op
begin
  if Node = nil then Exit;

  // MODERN (FreeBASIC, no line numbers): stamp each statement's physical source line into
  // FCurrentLineNumber so emitted instructions carry it in the source map. This makes ERL and
  // RESUME NEXT work (both rely on GetSourceLine / FindPCAfterLine). CLASSIC keeps using the
  // BASIC line numbers set by antLineNumber below.
  if FModernMode and (Node.SourceLine > 0) then
    FCurrentLineNumber := Node.SourceLine;

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
    antThreadWait, antThreadDetach:
    begin
      // THREADWAIT/THREADDETACH handle — join or detach the worker named by the int handle (child0).
      ProcessExpression(Node.GetChild(0), ExprResult);
      ExprResult := EnsureIntRegister(ExprResult);
      if Node.NodeType = antThreadDetach then OpCode := ssaThreadDetach else OpCode := ssaThreadWait;
      EmitInstruction(OpCode, MakeSSAValue(svkNone), ExprResult,
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antRandomize:
    begin
      // RANDOMIZE [seed] — seed the RNG. With a seed expression: Src1 = seed reg, Immediate = 1.
      // Without: seed from the system timer (Immediate = 0); Src1 holds a dummy 0 register so
      // register compaction always sees a valid Src1.
      if Node.ChildCount >= 1 then
      begin
        ProcessExpression(Node.GetChild(0), ExprResult);
        ExprResult := EnsureIntRegister(ExprResult);
        EmitInstruction(ssaRandomize, MakeSSAValue(svkNone), ExprResult,
                        MakeSSAValue(svkNone), MakeSSAConstInt(1));
      end
      else
      begin
        ExprResult := EnsureIntRegister(MakeSSAConstInt(0));
        EmitInstruction(ssaRandomize, MakeSSAValue(svkNone), ExprResult,
                        MakeSSAValue(svkNone), MakeSSAConstInt(0));
      end;
    end;

    antMutexLock, antMutexUnlock, antMutexDestroy:
    begin
      // MUTEXLOCK/UNLOCK/DESTROY handle — operate on the mutex named by the int handle (child0).
      ProcessExpression(Node.GetChild(0), ExprResult);
      ExprResult := EnsureIntRegister(ExprResult);
      case Node.NodeType of
        antMutexUnlock:  OpCode := ssaMutexUnlock;
        antMutexDestroy: OpCode := ssaMutexDestroy;
      else
        OpCode := ssaMutexLock;
      end;
      EmitInstruction(OpCode, MakeSSAValue(svkNone), ExprResult,
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

    antCondWait:
    begin
      // CONDWAIT cond, mutex — atomically release mutex, wait on cond, reacquire mutex.
      ProcessExpression(Node.GetChild(0), ExprResult);
      ExprResult := EnsureIntRegister(ExprResult);    // cond handle
      ProcessExpression(Node.GetChild(1), AddrVal);
      AddrVal := EnsureIntRegister(AddrVal);          // mutex handle
      EmitInstruction(ssaCondWait, MakeSSAValue(svkNone), ExprResult, AddrVal, MakeSSAValue(svkNone));
    end;

    antCondSignal, antCondBroadcast, antCondDestroy:
    begin
      // CONDSIGNAL/CONDBROADCAST/CONDDESTROY cond — operate on the cond var named by the int handle.
      ProcessExpression(Node.GetChild(0), ExprResult);
      ExprResult := EnsureIntRegister(ExprResult);
      case Node.NodeType of
        antCondBroadcast: OpCode := ssaCondBroadcast;
        antCondDestroy:   OpCode := ssaCondDestroy;
      else
        OpCode := ssaCondSignal;
      end;
      EmitInstruction(OpCode, MakeSSAValue(svkNone), ExprResult,
                      MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;

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
    antMemberAccess, antArrayAccess, antFunctionCall, antGraphicsFunction:
      // Statement-level call for side effects (e.g. obj.method(args), a function/array expression used
      // as a statement, or GETMOUSE(x,y) called for its by-reference writes). Lower and discard the result.
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
    antErase: ProcessErase(Node);
    antRedim: ProcessRedim(Node);
    antSwap: ProcessSwap(Node);
    antDefType: ProcessDefType(Node);
    antMidStatement: ProcessMidStatement(Node);
    antLSet: ProcessLRSetStatement(Node, True);
    antRSet: ProcessLRSetStatement(Node, False);
    antEnum:
      // ENUM members lower to a sequence of plain assignments (member = value), like CONST.
      for i := 0 to Node.ChildCount - 1 do
        if Node.GetChild(i).NodeType = antAssignment then
          ProcessAssignment(Node.GetChild(i));
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
    antBeep: ProcessBeep(Node);
    antScreenRes: ProcessScreenRes(Node);
    antGfxPset: ProcessGfxPset(Node);
    antGfxPaint: ProcessGfxPaint(Node);
    antGfxLine: ProcessGfxLine(Node);
    antGfxCircle: ProcessGfxCircle(Node);
    antPalette: ProcessPalette(Node);
    antGfxColor: ProcessGfxColor(Node);
    antImageDestroy: ProcessImageDestroy(Node);
    antImageInfo: ProcessImageInfo(Node);
    antGfxGet: ProcessGfxGet(Node);
    antGfxPut: ProcessGfxPut(Node);
    antScreenInfo: ProcessScreenInfo(Node);
    antScreenSet: ProcessScreenSet(Node);
    antPCopy: ProcessPCopy(Node);
    antGfxWindow: ProcessGfxWindow(Node);
    antGfxView: ProcessGfxView(Node);
    antGfxScreen: ProcessGfxScreen(Node);
    antGfxSetmouse: ProcessGfxSetmouse(Node);
    antGfxNop: ;  // SCREENLOCK/UNLOCK/SYNC/WINDOWTITLE/VIEW PRINT: accept-and-ignore (no code emitted)
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
      ExitKind := UpperCase(VarToStr(Node.Value));   // 'EXIT[ kind]' / 'CONTINUE[ kind]' / 'RETURN'
      IsExitStmt := Assigned(Node.Token) and (UpperCase(Node.Token.Value) = kEXIT);
      IsContinueStmt := Assigned(Node.Token) and (UpperCase(Node.Token.Value) = kCONTINUE);
      if IsContinueStmt then
      begin
        // FreeBASIC CONTINUE: skip the rest of the current iteration and jump to the innermost loop's
        // continue point (FOR: increment; DO WHILE/WHILE: top condition; DO...LOOP cond: bottom test;
        // DO...LOOP: body top). The kind word (FOR/DO/WHILE) is informational in v1. Clean up the
        // current iteration's block scope (same unwinding as EXIT, down to and including the loop body).
        if Length(FLoopStack) > 0 then
        begin
          ExitLevels := StrToIntDef(Node.Attributes.Values[ATTR_LOOP_LEVELS], 1);
          ExitLoopIdx := High(FLoopStack);   // default: innermost
          ExitAllDepth := 1;
          if ExitLevels > 1 then
          begin
            if Pos(kFOR, ExitKind) > 0 then ExitLoopKind := lkFor else ExitLoopKind := lkDo;
            if not FindEnclosingLoop(ExitLoopKind, ExitLevels, ExitLoopIdx, ExitAllDepth) then
            begin ExitLoopIdx := High(FLoopStack); ExitAllDepth := 1; end;
          end;
          FLoopStack[ExitLoopIdx].ContUsed := True;
          EmitExitLoopCleanupN(ExitAllDepth);   // M8: destruct + reclaim the abandoned iterations' scopes
          EmitInstruction(ssaJump, MakeSSALabel(FLoopStack[ExitLoopIdx].ContLabel),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else
          WriteLn(StdErr, 'Warning: CONTINUE outside loop at line ', FCurrentLineNumber);
      end
      else if IsExitStmt then
      begin
        // EXIT SUB / EXIT FUNCTION inside a procedure -> frame return.
        if FInProcedure and ((Pos(kSUB, ExitKind) > 0) or (Pos(kFUNCTION, ExitKind) > 0)) then
        begin
          EmitAllBlockScopesCleanup;   // M8: unwind active loop block scopes before the frame exit
          EmitFrameDestructors;   // V5
          EmitSharedSyncOut;      // M6: publish shared-global changes before returning
          EmitByrefParamStore;    // BYREF: publish explicit-BYREF scalar params back to their slots
          EmitInstruction(ssaReturnSub, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else if Length(FLoopStack) > 0 then
        begin
          // EXIT FOR/DO/WHILE - clean up the loop's block scope (and any IF/SCOPE blocks nested between
          // here and the loop body), then jump to its EndLabel. Multi-level "Exit For, For" targets the
          // N-th enclosing loop of that kind and unwinds every block scope down to and including it.
          ExitLevels := StrToIntDef(Node.Attributes.Values[ATTR_LOOP_LEVELS], 1);
          ExitLoopIdx := High(FLoopStack);   // default: innermost
          ExitAllDepth := 1;
          if ExitLevels > 1 then
          begin
            if Pos(kFOR, ExitKind) > 0 then ExitLoopKind := lkFor else ExitLoopKind := lkDo;
            if not FindEnclosingLoop(ExitLoopKind, ExitLevels, ExitLoopIdx, ExitAllDepth) then
            begin ExitLoopIdx := High(FLoopStack); ExitAllDepth := 1; end;
          end;
          EmitExitLoopCleanupN(ExitAllDepth);   // M8
          EmitInstruction(ssaJump, MakeSSALabel(FLoopStack[ExitLoopIdx].EndLabel),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          FCurrentBlock := nil;
        end
        else if FInProcedure then
        begin
          // Bare EXIT inside a procedure with no enclosing loop -> frame return.
          EmitAllBlockScopesCleanup;   // M8
          EmitFrameDestructors;   // V5
          EmitSharedSyncOut;      // M6: publish shared-global changes before returning
          EmitByrefParamStore;    // BYREF: publish explicit-BYREF scalar params back to their slots
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
          // FreeBASIC BYREF result: stage the ADDRESS of the returned variable. An address-carrying
          // BYREF param already holds the caller variable's address (min(a,b)=0); any other named var
          // returns its own backing address.
          if FCurrentProcByrefRet and (Node.GetChild(0).NodeType = antIdentifier) and
             IsAddrParam(VarToStr(Node.GetChild(0).Value)) then
            EmitXferStore(srtInt, XFER_RESULT_SLOT, EnsureIntRegister(GetOrAllocateVariable(VarToStr(Node.GetChild(0).Value))))
          else if FCurrentProcByrefRet and (Node.GetChild(0).NodeType = antIdentifier) then
            EmitXferStore(srtInt, XFER_RESULT_SLOT, EmitVarAddress(VarToStr(Node.GetChild(0).Value)))
          else
          begin
            ProcessExpression(Node.GetChild(0), RetVal);
            // V3: UDT result copied by value into the caller's result instance; scalar via xfer slot.
            if FCurrentProcRetRecType <> '' then
              EmitRecordCopy(FCurrentResultHandle, RetVal, FindUDT(FCurrentProcRetRecType))
            else
              EmitXferStore(FCurrentProcRetType, XFER_RESULT_SLOT, RetVal);
          end;
        end;
        EmitAllBlockScopesCleanup;   // M8: unwind active loop block scopes before the frame exit
        EmitFrameDestructors;   // V5: destroy local UDTs before returning
        EmitSharedSyncOut;      // M6: publish shared-global changes before returning
        EmitByrefParamStore;    // BYREF: publish explicit-BYREF scalar params back to their slots
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
      // V5b: an explicit `END` is a program halt — run global destructors first (reverse construction
      // order), before the ssaEnd. V5e: an END *inside a procedure* runs them too, but the globals'
      // module registers are hidden under the active frame, so it reads each handle from its reserved
      // frame-independent slot (UseSlots=True).
      if not FInProcedure then
        EmitModuleDestructors(False)
      else
        EmitModuleDestructors(True);
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
    antSetClock:
    begin
      // SETDATE str / SETTIME str - set the VM-internal current date/time. Node.Value = SETDATE/SETTIME.
      if Node.ChildCount > 0 then
      begin
        ProcessExpression(Node.GetChild(0), SecondsVal);
        ArgReg := EnsureStringRegister(SecondsVal);
        if UpperCase(VarToStr(Node.Value)) = kSETTIME then SelImm := 1 else SelImm := 0;
        EmitInstruction(ssaSetClock, MakeSSAValue(svkNone), ArgReg,
                       MakeSSAValue(svkNone), MakeSSAConstInt(SelImm));
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
      // RESUME [NEXT | <line> | <label> | 0] - continue after an error
      if Node.ChildCount > 0 then
      begin
        if Node.GetChild(0).NodeType = antIdentifier then
        begin
          // FreeBASIC: RESUME <label> - jump to a named label (label -> resolved PC in Immediate)
          EmitInstruction(ssaResumeLabel, MakeSSAValue(svkNone),
                         MakeSSALabel(JumpLabelName(Node.GetChild(0))),
                         MakeSSAValue(svkNone), MakeSSAValue(svkNone));
        end
        else
        begin
          ProcessExpression(Node.GetChild(0), ExprResult);
          // RESUME 0 == plain RESUME (continue at the faulting statement)
          if (ExprResult.Kind = svkConstInt) and (ExprResult.ConstInt = 0) then
            EmitInstruction(ssaResume, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                           MakeSSAValue(svkNone), MakeSSAValue(svkNone))
          else
          begin
            // RESUME <line> - jump to a specific classic line number
            LineNumReg := EnsureIntRegister(ExprResult);
            EmitInstruction(ssaResume, MakeSSAValue(svkNone), LineNumReg,
                           MakeSSAValue(svkNone), MakeSSAValue(svkNone));
          end;
        end;
      end
      else
      begin
        // Plain RESUME - continue at error line (Src1 = svkNone means use FResumePC)
        EmitInstruction(ssaResume, MakeSSAValue(svkNone), MakeSSAValue(svkNone),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end;
    end;
    antOnError:
    begin
      // ON [LOCAL] ERROR GOTO <label|0>
      // Child[0] = target: an identifier (label) installs a label-based handler;
      // a numeric 0 disables; a positive number is a classic line-number handler (reuse TRAP).
      if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        // Label-based handler (FreeBASIC MODERN). Src1 carries the handler label ->
        // resolved to the handler PC in Immediate (jump fixup, like @sub / GOTO).
        EmitInstruction(ssaOnError, MakeSSAValue(svkNone),
                       MakeSSALabel(JumpLabelName(Node.GetChild(0))),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
      end
      else if Node.ChildCount >= 1 then
      begin
        // Numeric target: 0 disables, positive selects a classic line-number handler.
        ProcessExpression(Node.GetChild(0), ExprResult);
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
        // ON ERROR GOTO with no target -> disable
        EmitInstruction(ssaTrap, MakeSSAValue(svkNone), MakeSSAConstInt(0),
                       MakeSSAValue(svkNone), MakeSSAValue(svkNone));
    end;
    antErrorStmt:
    begin
      // ERROR <n> - raise a user-defined runtime error with number n (Src1 = number register).
      // The VM raises a coded exception; the run-loop handler sets ERR=n and jumps to any active
      // ON ERROR / TRAP handler, exactly like a built-in runtime error.
      if Node.ChildCount >= 1 then
      begin
        ProcessExpression(Node.GetChild(0), ExprResult);
        LineNumReg := EnsureIntRegister(ExprResult);
        EmitInstruction(ssaRaiseError, MakeSSAValue(svkNone), LineNumReg,
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
    antRmdir: ProcessRmdir(Node);
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
  DefCh: Char;
begin
  FProgram := TSSAProgram.Create;
  FLabelCounter := 0;

  // FreeBASIC NAMESPACE: flatten namespace blocks into mangled, module-level declarations before any
  // pre-scan walks the AST. No-op when the program has no NAMESPACE (keyword is MODERN-only anyway).
  FlattenNamespaces(AST);

  // FreeBASIC STATIC locals: rewrite each proc-level STATIC into a hoisted, uniquely-named DIM SHARED
  // global (persistent across calls, initialised once). No-op without STATIC declarations.
  LowerStaticLocals(AST);

  // FB lexical scope: reset the scope stack (module scope is FVarMap itself; the stack holds only
  // proc-root and block frames, pushed during lowering in MODERN mode). Inert in CLASSIC.
  while Length(FScopeStack) > 0 do ScopePopFrame;
  FScopeSerial := 0;

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

  // M6: collect DIM SHARED scalars and assign them dedicated transfer slots (runs after type
  // registration so each var's bank is known). These slots survive the bcCallSub save/restore.
  FSharedVars.Clear;
  FSharedScalarArr.Clear;
  FPointerVars.Clear;
  FAddrLocalVars.Clear;
  FRawPtrVars.Clear;
  FWStringVars.Clear;
  FByrefRetFuncs.Clear;
  FVarWidthCode.Clear;
  FVarPrintKind.Clear;
  FArrayElemWidth.Clear;
  // FreeBASIC pointers: mark each address-taken (@x) declared scalar SHARED so the next pass backs it
  // with a 1-element global array (a stable address); also records pointee types in FPointerVars.
  CollectAddressTakenVars(AST);
  CollectSharedVars(AST);
  // OOP static member variables: back each "Static x AS t" type field with a shared global scalar.
  FStaticMembers.Clear;
  CollectStaticMembers(AST);
  // FreeBASIC raw pointers: vars assigned from Allocate/CAllocate/Reallocate (and CAST/copies of raw).
  // Iterate to a fixpoint so raw-ness propagates through copies regardless of statement order.
  // (Stage 2 byte-backing of address-taken arrays was withdrawn: a managed/raw mix is unsound at function
  // boundaries — a "T PTR" parameter can receive both a managed @x and a raw @arr/Allocate pointer. So
  // @arr stays managed; only Allocate buffers are raw. CollectAddrTakenArrays is intentionally not called.)
  repeat
    FRawCollectChanged := False;
    CollectRawPtrVars(AST);
  until not FRawCollectChanged;
  // FreeBASIC WSTRING: record vars/fields declared AS WSTRING so width-aware ops (LEN/MID/...) count by
  // Unicode codepoint. Same srtString bank (UTF-8 storage) → no new register bank, existing ops intact.
  FFixedLenVars.Clear;
  CollectWStringVars(AST);
  // REDIM multi-dim: an array re-dimensioned with >1 dimension must compute its element linear index
  // from RUNTIME dimensions (strides change on REDIM). Collect those array names; their multi-dim access
  // uses the push/resolve path. Arrays never multi-dim-REDIM'd keep the fast const-folded strides.
  FRedimMultiArrays.Clear;
  CollectRedimMultiArrays(AST);

  // M8: reset block-scope state (block-scoped record reclamation; the scope stack itself was reset above).
  FBlockHandledVars.Clear;

  // V5b: collect module-scope DIM'd UDTs (globals) once, so both an explicit `END` statement and the
  // implicit fall-through halt can emit their destructor calls (in reverse construction order).
  FModuleRecordVars.Clear;
  CollectModuleRecordVars(AST);

  // FreeBASIC DEFINT/DEFSTR...: collect default-type-by-initial BEFORE pre-allocating variables,
  // so a bare name gets its DEF bank (PreAllocateVariables binds via GetVariableType).
  for DefCh := 'A' to 'Z' do FDefTypeBank[DefCh] := -1;
  CollectDefTypes(AST);

  PreAllocateVariables(AST);
  {$IFDEF DEBUG_SSA}
  if DebugSSA then
    WriteLn('[SSA] Variable registers pre-allocated: ', FVarMap.Count, ' variables');
  {$ENDIF}

  // Create entry block
  FCurrentBlock := FProgram.CreateBlock('_entry');

  // OOP: allocate static member variables' backing arrays at program start (no DIM to trigger it).
  EmitStaticMemberAllocs;

  // PRE-PROCESS DATA STATEMENTS FIRST
  // In BASIC, DATA statements are collected before program execution,
  // so READ can access them regardless of their position in the source
  PreProcessData(AST);

  // PRE-COLLECT SUB/FUNCTION DECLARATIONS so CALL sites can resolve parameter info even
  // for procedures defined later in the source (forward references).
  PreCollectProcedures(AST);

  // V5e: reserve a frame-independent int slot for each destructor-bearing global, so an END inside a
  // SUB/FUNCTION can still destroy it (its module register is hidden under the active frame). Must run
  // AFTER PreCollectProcedures (TypeNeedsDestruction resolves DESTRUCTOR labels via FProcDecls) and
  // BEFORE ProcessStatement (which stores each global's handle into its slot at construction). Uses the
  // int SHARED count (CollectSharedVars, already done) to keep the slot ranges disjoint.
  AssignModuleDtorSlots;

  // GOTO-unwind: collect the module frame's block-depth-0 labels, so a GOTO out of a FOR/IF/SCOPE body
  // to a top-level label cleans up the exited block scopes (see ProcessGoto).
  CollectTopLevelLabels(AST, 0);

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
      // V5b: run destructors for module-scope DIM'd UDTs (globals) on the normal fall-through exit,
      // before halting. The dtor calls split the block (bcCallSub), so END goes to FCurrentBlock
      // after they are emitted. Globals' storage is not reclaimed (program is ending); only the
      // destructors' observable effects run. (An explicit `END` statement runs them via antEnd.)
      EmitModuleDestructors;
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

  // FB lexical scope: release the module frame (and any frame left dangling by an error path).
  while Length(FScopeStack) > 0 do ScopePopFrame;

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
