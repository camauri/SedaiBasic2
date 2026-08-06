unit SedaiWasmControl;

{ ============================================================================
  SedaiWasmControl - arbitrary control flow inside structured WebAssembly.

  Step 2 of job/docs/PIANO_WASM.md, and it comes second on purpose: WASM has no
  arbitrary jumps, only block/loop/if plus br to a nesting level, while our SSA
  hands us a CFG with whatever edges the program had. Recovering structure from
  an arbitrary CFG is the relooper problem. This unit is the way out the plan
  chose: one loop with a br_table on the index of the current block. Slower than
  recovered structure, never wrong, and - the part that matters - it works on an
  irreducible CFG, which a relooper cannot handle without duplicating code.

  The shape, for three blocks:

      i32.const <entry>          ; the state variable holds "which block"
      local.set  $state
      block $exit
       loop $dispatch
        block $a3                ; br_table default - an impossible state
         block $a2
          block $a1
           block $a0
            local.get $state
            br_table 0 1 2 3
           end                   ; br 0 arrives HERE
           BODY 0
          end                    ; br 1 arrives here
          BODY 1
         end                     ; br 2 arrives here
         BODY 2
        end                      ; the default arrives here
        unreachable
       end
      end

  So body i is enclosed by $a(i+1)..$aN, then the loop, then $exit:
  the loop sits at depth N-i and $exit at N-i+1, which is why nothing outside
  this unit should ever compute a branch depth by hand. EmitGoto and friends
  take block indices and work the depth out, including any scopes the caller
  opened inside a body (EnterScope/ExitScope).

  Two properties worth naming:

  - Falling off the end of body i lands exactly at body i+1, so a jump to the
    next block costs NOTHING. EmitGotoTerminal exploits that; straight-line
    code pays no dispatch at all. It is only valid as a body's last
    instruction, which is why it is a separate method and not a cleverness
    hidden inside EmitGoto.
  - The br_table default is 'unreachable', not a fallthrough. An out-of-range
    state is a compiler bug, and PIANO_WASM.md sec.4 is explicit: never emit
    something that runs and lies. A trap is the loud version.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SedaiWasmEmitter;

const
  { The engine ceiling on br_table entries, and therefore on the number of CFG
    blocks one dispatch region can hold. MEASURED, not read off a spec: 65520
    validates, instantiates and computes the right answer (958 KB module);
    65521 is refused with "invalid table count". Nesting depth is NOT the limit
    - 50000 nested blocks are fine.
    For scale, the largest program measured here (bas/demo/sedai_lbm.bas, 785
    lines) has 300 blocks, so the headroom is over 200x. The guard exists
    anyway: a limit has to be refused with a clear message, never discovered by
    the browser. }
  WASM_MAX_BR_TABLE_TARGETS = 65520;

type
  EWasmControl = class(Exception);

  TWasmDispatch = class
  private
    FBodies: array of TWasmBuf;
    FExtraScopes: array of Integer;
    FStateLocal: LongWord;
    FCount: Integer;
    procedure CheckIndex(Index: Integer; const Who: string);
    function LoopDepth(Index: Integer): LongWord;
    function ExitDepth(Index: Integer): LongWord;
  public
    { BlockCount is the number of CFG blocks; StateLocal is the index of an i32
      local reserved for the block number. }
    constructor Create(ABlockCount: Integer; AStateLocal: LongWord);
    destructor Destroy; override;

    { Where the caller emits the code of block Index. Owned here. }
    function Body(Index: Integer): TWasmBuf;

    { Tell the dispatcher when a body opens or closes a structured scope of its
      own, so branch depths stay right. }
    procedure EnterScope(Index: Integer);
    procedure ExitScope(Index: Integer);

    { Jump from block FromIndex to block ToIndex. Always emits a real branch,
      so it is correct anywhere in the body. }
    procedure EmitGoto(FromIndex, ToIndex: Integer);

    { Same, but as the LAST instruction of the body: a jump to the immediately
      following block then costs nothing, because falling off the end of body i
      lands at body i+1. Raises if used where that is not the case. }
    procedure EmitGotoTerminal(FromIndex, ToIndex: Integer);

    { Conditional edge. The i32 condition must already be on the stack. }
    procedure EmitBranch(FromIndex, IfTrue, IfFalse: Integer);

    { Leave the whole dispatch region (not the function - use wopReturn for that). }
    procedure EmitLeave(FromIndex: Integer);

    { True when EmitGotoTerminal would emit nothing. }
    function FallsThrough(FromIndex, ToIndex: Integer): Boolean;

    { Assemble the region into Dest. Bodies must already be filled in. }
    procedure Emit(Dest: TWasmBuf; EntryIndex: Integer);

    property BlockCount: Integer read FCount;
    property StateLocal: LongWord read FStateLocal;
  end;

implementation

constructor TWasmDispatch.Create(ABlockCount: Integer; AStateLocal: LongWord);
var
  i: Integer;
begin
  inherited Create;
  if ABlockCount < 1 then
    raise EWasmControl.Create('a dispatch region needs at least one block');
  if ABlockCount > WASM_MAX_BR_TABLE_TARGETS then
    raise EWasmControl.CreateFmt(
      'a dispatch region of %d blocks exceeds the engine ceiling of %d br_table ' +
      'entries; the region has to be split before emission',
      [ABlockCount, WASM_MAX_BR_TABLE_TARGETS]);
  FCount := ABlockCount;
  FStateLocal := AStateLocal;
  SetLength(FBodies, FCount);
  SetLength(FExtraScopes, FCount);
  for i := 0 to FCount - 1 do
  begin
    FBodies[i] := TWasmBuf.Create;
    FExtraScopes[i] := 0;
  end;
end;

destructor TWasmDispatch.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do FBodies[i].Free;
  inherited Destroy;
end;

procedure TWasmDispatch.CheckIndex(Index: Integer; const Who: string);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EWasmControl.CreateFmt('%s: block %d is outside 0..%d',
                                 [Who, Index, FCount - 1]);
end;

function TWasmDispatch.LoopDepth(Index: Integer): LongWord;
begin
  // body i is inside $a(i+1)..$aN, so the loop is N-i levels out, plus whatever
  // the body opened for itself
  Result := LongWord(FCount - Index + FExtraScopes[Index]);
end;

function TWasmDispatch.ExitDepth(Index: Integer): LongWord;
begin
  Result := LoopDepth(Index) + 1;
end;

function TWasmDispatch.Body(Index: Integer): TWasmBuf;
begin
  CheckIndex(Index, 'Body');
  Result := FBodies[Index];
end;

procedure TWasmDispatch.EnterScope(Index: Integer);
begin
  CheckIndex(Index, 'EnterScope');
  Inc(FExtraScopes[Index]);
end;

procedure TWasmDispatch.ExitScope(Index: Integer);
begin
  CheckIndex(Index, 'ExitScope');
  if FExtraScopes[Index] = 0 then
    raise EWasmControl.CreateFmt('ExitScope: block %d has no open scope', [Index]);
  Dec(FExtraScopes[Index]);
end;

function TWasmDispatch.FallsThrough(FromIndex, ToIndex: Integer): Boolean;
begin
  Result := (ToIndex = FromIndex + 1) and (ToIndex < FCount) and
            (FExtraScopes[FromIndex] = 0);
end;

procedure TWasmDispatch.EmitGoto(FromIndex, ToIndex: Integer);
var
  B: TWasmBuf;
begin
  CheckIndex(FromIndex, 'EmitGoto (from)');
  CheckIndex(ToIndex, 'EmitGoto (to)');
  B := FBodies[FromIndex];
  B.I32Const(ToIndex);
  B.LocalSet(FStateLocal);
  B.Br(LoopDepth(FromIndex));
end;

procedure TWasmDispatch.EmitGotoTerminal(FromIndex, ToIndex: Integer);
begin
  CheckIndex(FromIndex, 'EmitGotoTerminal (from)');
  CheckIndex(ToIndex, 'EmitGotoTerminal (to)');
  if FallsThrough(FromIndex, ToIndex) then Exit;   // the fallthrough IS the jump
  EmitGoto(FromIndex, ToIndex);
end;

procedure TWasmDispatch.EmitBranch(FromIndex, IfTrue, IfFalse: Integer);
var
  B: TWasmBuf;
  D: LongWord;
begin
  CheckIndex(FromIndex, 'EmitBranch (from)');
  CheckIndex(IfTrue, 'EmitBranch (true)');
  CheckIndex(IfFalse, 'EmitBranch (false)');
  B := FBodies[FromIndex];
  // the 'if' is one more level of nesting for the branches inside it
  D := LoopDepth(FromIndex) + 1;
  B.BlockStart(wopIf, WASM_BLOCKTYPE_EMPTY);
    B.I32Const(IfTrue);  B.LocalSet(FStateLocal); B.Br(D);
  B.Op(wopElse);
    B.I32Const(IfFalse); B.LocalSet(FStateLocal); B.Br(D);
  B.EndOp;
end;

procedure TWasmDispatch.EmitLeave(FromIndex: Integer);
begin
  CheckIndex(FromIndex, 'EmitLeave');
  FBodies[FromIndex].Br(ExitDepth(FromIndex));
end;

procedure TWasmDispatch.Emit(Dest: TWasmBuf; EntryIndex: Integer);
var
  i: Integer;
  Targets: array of LongWord;
begin
  CheckIndex(EntryIndex, 'Emit (entry)');
  for i := 0 to FCount - 1 do
    if FExtraScopes[i] <> 0 then
      raise EWasmControl.CreateFmt('block %d left %d scope(s) open',
                                   [i, FExtraScopes[i]]);

  Dest.I32Const(EntryIndex);
  Dest.LocalSet(FStateLocal);
  Dest.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);   // $exit
  Dest.BlockStart(wopLoop, WASM_BLOCKTYPE_EMPTY);    // $dispatch

  // N+1 nested blocks: one landing pad per block, plus one for the default
  for i := FCount downto 0 do
    Dest.BlockStart(wopBlock, WASM_BLOCKTYPE_EMPTY);

  SetLength(Targets, FCount);
  for i := 0 to FCount - 1 do Targets[i] := LongWord(i);
  Dest.LocalGet(FStateLocal);
  Dest.BrTable(Targets, LongWord(FCount));

  for i := 0 to FCount - 1 do
  begin
    Dest.EndOp;                    // closes $a(i); the branch for block i lands here
    Dest.Append(FBodies[i]);
  end;

  Dest.EndOp;                      // closes $a(N): the default landing pad
  Dest.Op(wopUnreachable);         // an out-of-range state is a compiler bug
  Dest.EndOp;                      // closes the loop
  Dest.EndOp;                      // closes $exit
end;

end.
