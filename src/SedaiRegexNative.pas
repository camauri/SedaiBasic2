unit SedaiRegexNative;

{ ============================================================================
  SedaiRegexNative - compiles a TDfa into x86-64 machine code.

  This is the step the whole "own engines" direction is for: a pattern is
  almost always a LITERAL in the source, so the automaton it denotes is known
  at compile time and can become code rather than a table walked at run time.
  The emitter and the executable-memory allocator are the AOT's own
  (SedaiX86Emitter), so this is the same machinery that compiles BASIC, pointed
  at a different small language.

  Shape of the generated function - one basic block per DFA state:

      state_k:  [mov r13, r12]          ; only when k accepts: remember the end
                cmp  r12, rsi           ; input exhausted?
                jae  attempt_done
                movzx eax, [rbx+r12]    ; the next byte
                inc  r12
                <compare chain>         ; -> state_t, or fall through
                jmp  attempt_done       ; no transition: dead

  The interpreted engine spends, per byte, a table load whose address depends on
  both the state and the input. Here the state is the PROGRAM COUNTER: it costs
  nothing to hold and nothing to look up, and a state with one outgoing byte
  becomes a single `cmp al, imm8` + `je`.

  ⚠️ What this does NOT change is the number of unpredictable BRANCHES, which is
  what the interpreted scan turned out to be bound by. Whether removing the
  table loads is worth anything on top of that is a question for the stopwatch,
  not for this comment - see the measurement in the session notes.

  Registers (all callee-saved, and nothing here calls out, so no spilling):
      rbx = Data      rsi = Len       rdi = p (attempt start)
      r12 = i         r13 = last      r14 = the argument record
      rax, rcx, rdx = scratch
  ============================================================================ }

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses
  SysUtils, SedaiX86Emitter, SedaiAutomaton;

type
  // One record instead of five arguments: it makes the entry sequence identical
  // on Win64 and System V, which otherwise disagree from the very first
  // register - and a scanner is not worth two calling conventions.
  TScanArgs = record
    Data: PByte;      // +0
    Len: PtrInt;      // +8
    From: PtrInt;     // +16
    MStart: PtrInt;   // +24  (out)
    MEnd: PtrInt;     // +32  (out)
  end;
  PScanArgs = ^TScanArgs;

  TDfaScanFn = function(A: PScanArgs): PtrInt; cdecl;

  TNativeDfa = class
  private
    FMem: TExecMem;
    FFn: TDfaScanFn;
    FDfa: TDfa;          // kept alive: the code holds a pointer into its pair table
    FStates, FBytes: Integer;
  public
    destructor Destroy; override;
    property Fn: TDfaScanFn read FFn;
    property StateCount: Integer read FStates;
    property CodeBytes: Integer read FBytes;
  end;

// Compile Dfa to native code, or nil when it is not worth it / not possible.
function CompileDfaNative(Dfa: TDfa): TNativeDfa;

implementation

const
  MAX_STATES  = 512;   // beyond this the code is bigger than the i-cache pays for
  MAX_RANGES  = 12;    // per state; a state needing more stays interpreted

type
  TRange = record Lo, Hi: Byte; end;
  TFixup = record Off, Target: Integer; end;   // Target: state id, or a NEG label

const
  L_ATTEMPT_DONE = -1;
  L_ADVANCE      = -2;
  L_NOT_FOUND    = -3;
  L_SCAN_NEXT    = -4;

destructor TNativeDfa.Destroy;
begin
  FMem.Free;
  inherited Destroy;
end;

function CompileDfaNative(Dfa: TDfa): TNativeDfa;
var
  E: TX86Emitter;
  StateOff: array of Integer;
  Fixups: array of TFixup;
  NFix: Integer;
  OffAttemptDone, OffAdvance, OffNotFound, OffScanNext: Integer;
  NS, s, b, i, k, t, TargetOff: Integer;
  Tr: array[0..255] of Integer;
  Targets: array[0..255] of Integer;
  NTarget, Best, BestCount, Cnt: Integer;
  Rg: array[0..255] of TRange;
  NRg, TotRg: Integer;
  Mem: TExecMem;
  R: TNativeDfa;

  procedure AddFix(AOff, ATarget: Integer);
  begin
    if NFix = Length(Fixups) then SetLength(Fixups, NFix * 2 + 16);
    Fixups[NFix].Off := AOff;
    Fixups[NFix].Target := ATarget;
    Inc(NFix);
  end;

  // jmp rel32 / jcc rel32 to a label resolved at the end.
  procedure JmpTo(Target: Integer);
  begin
    E.Emit8($E9); AddFix(E.Len, Target); E.Emit32(0);
  end;
  procedure JccTo(CC: Byte; Target: Integer);
  begin
    E.Emit8($0F); E.Emit8(CC); AddFix(E.Len, Target); E.Emit32(0);
  end;

  procedure PushR(Reg: Integer);
  begin
    if Reg >= 8 then E.Emit8($41);
    E.Emit8($50 or (Reg and 7));
  end;
  procedure PopR(Reg: Integer);
  begin
    if Reg >= 8 then E.Emit8($41);
    E.Emit8($58 or (Reg and 7));
  end;
  // mov dst, src (64-bit)
  procedure MovRR(Dst, Src: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if Src >= 8 then rex := rex or $04;
    if Dst >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($C0 or ((Src and 7) shl 3) or (Dst and 7));
  end;
  // mov dst, [base+disp8]
  procedure MovLoad(Dst, Base, Disp: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if Dst >= 8 then rex := rex or $04;
    if Base >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($8B);
    E.Emit8($40 or ((Dst and 7) shl 3) or (Base and 7));
    E.Emit8(Byte(Disp));
  end;
  // mov [base+disp8], src
  procedure MovStore(Base, Disp, Src: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if Src >= 8 then rex := rex or $04;
    if Base >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($89);
    E.Emit8($40 or ((Src and 7) shl 3) or (Base and 7));
    E.Emit8(Byte(Disp));
  end;
  // mov reg, imm32 sign-extended to 64
  procedure MovImm32(Reg: Integer; Imm: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if Reg >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($C7);
    E.Emit8($C0 or (Reg and 7));
    E.Emit32(LongWord(Imm));
  end;
  procedure MovImm64(Reg: Integer; Imm: QWord);
  var rex: Byte;
  begin
    rex := $48;
    if Reg >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($B8 or (Reg and 7));
    E.Emit64(Imm);
  end;
  // cmp a, b (64-bit)
  procedure CmpRR(A, B2: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if B2 >= 8 then rex := rex or $04;
    if A >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($39);
    E.Emit8($C0 or ((B2 and 7) shl 3) or (A and 7));
  end;
  procedure IncR(Reg: Integer);
  var rex: Byte;
  begin
    rex := $48;
    if Reg >= 8 then rex := rex or $01;
    E.Emit8(rex); E.Emit8($FF); E.Emit8($C0 or (Reg and 7));
  end;
  // movzx eax, byte [base + idx]
  procedure MovzxIdx(Base, Idx: Integer);
  var rex: Byte;
  begin
    rex := 0;
    if Idx >= 8 then rex := rex or $02;      // REX.X
    if Base >= 8 then rex := rex or $01;     // REX.B
    if rex <> 0 then E.Emit8($40 or rex);
    E.Emit8($0F); E.Emit8($B6);
    E.Emit8($04);                            // mod=00 reg=eax rm=100 (SIB)
    E.Emit8(((Idx and 7) shl 3) or (Base and 7));
  end;
  procedure CmpAlImm(Imm: Byte);
  begin
    E.Emit8($3C); E.Emit8(Imm);
  end;

  // The byte set leading to Target, as ranges. Returns the count.
  function RangesFor(Target: Integer; out N: Integer): Boolean;
  var j: Integer;
  begin
    N := 0;
    j := 0;
    while j < 256 do
    begin
      if Tr[j] = Target then
      begin
        if N >= MAX_RANGES then Exit(False);
        Rg[N].Lo := Byte(j);
        while (j < 255) and (Tr[j + 1] = Target) do Inc(j);
        Rg[N].Hi := Byte(j);
        Inc(N);
      end;
      Inc(j);
    end;
    Result := True;
  end;

begin
  Result := nil;
  if not Dfa.Materialise(MAX_STATES) then Exit;
  NS := Dfa.DfaStates;
  if NS <= 1 then Exit;                       // nothing but the dead state

  E := TX86Emitter.Create;
  try
    SetLength(StateOff, NS);
    for i := 0 to NS - 1 do StateOff[i] := -1;
    NFix := 0;
    SetLength(Fixups, 256);

    // ---- prologue -------------------------------------------------------
    PushR(RBX); PushR(RSI); PushR(RDI);
    PushR(R12); PushR(R13); PushR(R14); PushR(R15);
    MovRR(R14, ABI_ARG0);                     // the argument record
    MovLoad(RBX, R14, 0);                     // Data
    MovLoad(RSI, R14, 8);                     // Len
    MovLoad(RDI, R14, 16);                    // p
    // The pair table's address, ONCE. Rebuilding it with a 10-byte movabs
    // inside the skip loop is what a first cut did, and it cost 60% of that
    // loop - an address that never changes has no business being in it.
    if Dfa.HasPairFilter then
      MovImm64(R15, QWord(PtrUInt(Dfa.PairTableAddr)));

    // ---- scan_next ------------------------------------------------------
    OffScanNext := E.Len;
    CmpRR(RDI, RSI);
    JccTo($87, L_NOT_FOUND);                  // ja: p > Len, nothing left

    if Dfa.HasPairFilter then
    begin
      // while p < Len-1 and the pair (Data[p],Data[p+1]) is not a possible
      // match start: p++. One BT against an 8 KB bitmap whose address is baked
      // into the code - the object outlives the code, see TNativeDfa.FDfa.
      k := E.Len;                             // pf_loop
      MovRR(RAX, RSI);
      E.EmitBytes([$48, $83, $E8, $01]);      // sub rax, 1
      CmpRR(RDI, RAX);
      E.EmitBytes([$0F, $83]); i := E.Len; E.Emit32(0);   // jae pf_done
      E.EmitBytes([$0F, $B6, $0C, $3B]);      // movzx ecx, byte [rbx+rdi]
      E.EmitBytes([$C1, $E1, $08]);           // shl ecx, 8
      E.EmitBytes([$0F, $B6, $54, $3B, $01]); // movzx edx, byte [rbx+rdi+1]
      E.EmitBytes([$09, $D1]);                // or ecx, edx
      E.EmitBytes([$41, $0F, $A3, $0F]);      // bt [r15], ecx
      E.EmitBytes([$0F, $82]); t := E.Len; E.Emit32(0);   // jb (CF=1) pf_done
      IncR(RDI);
      E.Emit8($E9); E.Emit32(LongWord(k - (E.Len + 4)));  // jmp pf_loop
      E.Patch32(i, LongWord(E.Len - (i + 4)));
      E.Patch32(t, LongWord(E.Len - (t + 4)));
    end;

    // ---- one anchored attempt ------------------------------------------
    MovRR(R12, RDI);                          // i = p
    MovImm32(R13, -1);                        // last = -1
    JmpTo(Dfa.StartState);

    // ---- one block per state -------------------------------------------
    for s := 0 to NS - 1 do
    begin
      if s = 0 then Continue;                 // the dead state has no block
      StateOff[s] := E.Len;
      if Dfa.Accepting(s) then MovRR(R13, R12);   // last = i
      CmpRR(R12, RSI);
      JccTo($83, L_ATTEMPT_DONE);             // jae: input exhausted
      MovzxIdx(RBX, R12);                     // al = Data[i]
      IncR(R12);

      for b := 0 to 255 do Tr[b] := Dfa.Transition(s, Byte(b));
      // Every distinct target INCLUDING the dead state, which is a target like
      // any other - it just happens to be spelled "give up on this attempt".
      //
      // ⛔ The first version collected only the NON-dead targets and made the
      // most frequent of those the fall-through. That is wrong the moment a
      // state has a single outgoing byte: with nothing else to compare against,
      // the fall-through swallowed EVERY byte, so "a" matched every character
      // in the input. The implicit case is dead, and dead must be in the
      // running for the fall-through like everything else.
      NTarget := 0; Best := -1; BestCount := -1; TotRg := 0;
      for b := 0 to 255 do
      begin
        k := 0;
        while (k < NTarget) and (Targets[k] <> Tr[b]) do Inc(k);
        if k = NTarget then begin Targets[NTarget] := Tr[b]; Inc(NTarget); end;
      end;
      for k := 0 to NTarget - 1 do
      begin
        Cnt := 0;
        for b := 0 to 255 do if Tr[b] = Targets[k] then Inc(Cnt);
        if Cnt > BestCount then begin BestCount := Cnt; Best := Targets[k]; end;
      end;
      // Price the dispatch before emitting any of it: a state needing more
      // ranges than the cap stays interpreted rather than becoming a comparison
      // chain longer than the table walk it replaces.
      for k := 0 to NTarget - 1 do
        if Targets[k] <> Best then
        begin
          if not RangesFor(Targets[k], NRg) then begin FreeAndNil(E); Exit; end;
          Inc(TotRg, NRg);
        end;
      if TotRg > MAX_RANGES then begin FreeAndNil(E); Exit; end;

      for k := 0 to NTarget - 1 do
      begin
        if Targets[k] = Best then Continue;
        RangesFor(Targets[k], NRg);
        if Targets[k] = DFA_DEAD then t := L_ATTEMPT_DONE else t := Targets[k];
        for i := 0 to NRg - 1 do
          if Rg[i].Lo = Rg[i].Hi then
          begin
            CmpAlImm(Rg[i].Lo);
            JccTo($84, t);                    // je
          end
          else
          begin
            CmpAlImm(Rg[i].Lo);
            E.EmitBytes([$0F, $82]); TargetOff := E.Len; E.Emit32(0);  // jb skip
            CmpAlImm(Rg[i].Hi);
            JccTo($86, t);                    // jbe -> inside the range
            E.Patch32(TargetOff, LongWord(E.Len - (TargetOff + 4)));
          end;
      end;
      if Best = DFA_DEAD then JmpTo(L_ATTEMPT_DONE) else JmpTo(Best);
    end;

    // ---- attempt_done ---------------------------------------------------
    OffAttemptDone := E.Len;
    E.EmitBytes([$49, $83, $FD, $00]);        // cmp r13, 0
    JccTo($8C, L_ADVANCE);                    // jl: no match at this position
    MovStore(R14, 24, RDI);                   // MStart = p
    MovStore(R14, 32, R13);                   // MEnd = last
    MovImm32(RAX, 1);
    E.EmitBytes([$41, $5F, $41, $5E, $41, $5D, $41, $5C]);  // pop r15,r14,r13,r12
    E.Emit8($5F); E.Emit8($5E); E.Emit8($5B);              // pop rdi, rsi, rbx
    E.Emit8($C3);

    OffAdvance := E.Len;
    IncR(RDI);
    JmpTo(L_SCAN_NEXT);

    OffNotFound := E.Len;
    E.EmitBytes([$31, $C0]);                  // xor eax, eax
    E.EmitBytes([$41, $5F, $41, $5E, $41, $5D, $41, $5C]);
    E.Emit8($5F); E.Emit8($5E); E.Emit8($5B);
    E.Emit8($C3);

    // ---- resolve ---------------------------------------------------------
    for i := 0 to NFix - 1 do
    begin
      case Fixups[i].Target of
        L_ATTEMPT_DONE: TargetOff := OffAttemptDone;
        L_ADVANCE:      TargetOff := OffAdvance;
        L_NOT_FOUND:    TargetOff := OffNotFound;
        L_SCAN_NEXT:    TargetOff := OffScanNext;
      else
        TargetOff := StateOff[Fixups[i].Target];
      end;
      if TargetOff < 0 then begin FreeAndNil(E); Exit; end;
      E.Patch32(Fixups[i].Off, LongWord(TargetOff - (Fixups[i].Off + 4)));
    end;

    Mem := TExecMem.Create(E);
    if Mem.Ptr = nil then begin Mem.Free; FreeAndNil(E); Exit; end;
    R := TNativeDfa.Create;
    R.FMem := Mem;
    R.FFn := TDfaScanFn(Mem.Ptr);
    R.FDfa := Dfa;
    R.FStates := NS;
    R.FBytes := E.Len;
    Result := R;
  finally
    E.Free;
  end;
end;

end.
