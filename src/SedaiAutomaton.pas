unit SedaiAutomaton;

{ ============================================================================
  SedaiAutomaton - byte-level finite automata: Thompson NFA construction and a
  lazily-built DFA over it.

  This unit knows NOTHING about regular-expression SYNTAX. It is the reusable
  core: anything shaped like "small language -> automaton -> fast scan" builds
  its NFA through TNfaBuilder and runs it through TDfa. Regex syntax lives in
  SedaiRegexEngine; a JSON or HTTP scanner would sit beside it, not on top of it.

  Why an automaton at all: a backtracker re-examines input on every alternative
  it tries, so its cost per input byte depends on the pattern. A DFA visits each
  input byte ONCE and does a table lookup - the cost per byte is a constant, and
  a small one. Measured on this machine, FPC's RegExpr spends ~140 cycles per
  input byte on the regex-redux patterns; a table-driven DFA should be a small
  number of cycles. That gap is the whole reason this unit exists.

  The DFA is built LAZILY: the subset construction runs one transition at a time,
  the first time that (state, byte) pair is actually reached. A pattern whose
  full DFA would be enormous still costs only the states the input walks through,
  which is what makes the construction safe to run on untrusted patterns.

  ⚠️ SEMANTICS: a DFA is naturally leftmost-LONGEST (POSIX). Perl and PCRE are
  leftmost-FIRST, where alternation order decides: "a|ab" matches "a" there and
  "ab" here. The two agree whenever no alternative is a prefix of another, which
  is true of every pattern this is currently used for, but it is a real
  difference and the caller has to know it - see SedaiRegexEngine's header.
  ============================================================================ }

{$mode objfpc}{$H+}

interface

type
  // A set of bytes, 256 bits. The transition test is a bit test, so a class of
  // any size costs the same as a single literal.
  TByteClass = array[0..31] of Byte;

  TNfaKind = (
    nkClass,   // consume one byte if it is in Cls, then go to Next1
    nkSplit,   // consume nothing, go to BOTH Next1 and Next2
    nkMatch    // accepting state
  );

  TNfaState = record
    Kind: TNfaKind;
    Cls: Integer;          // index into the class table (nkClass only)
    Next1, Next2: Integer; // -1 = dangling, patched during construction
  end;

  // A fragment under construction: an entry state plus the list of out-edges
  // that have not been given a destination yet. An out-edge is encoded as
  // state*2 + which, which being 0 for Next1 and 1 for Next2 - the standard
  // Thompson patch-list, and the reason concatenation is O(dangling ends)
  // rather than a walk of the whole fragment.
  TFrag = record
    Start: Integer;
    Outs: array of Integer;
  end;

  { The reusable NFA builder. Callers describe their language with the fragment
    operations and never touch state indices. }
  TNfaBuilder = class
  private
    FStates: array of TNfaState;
    FNState: Integer;
    FClasses: array of TByteClass;
    FNClass: Integer;
    function NewState(AKind: TNfaKind; ACls: Integer): Integer;
    function AddClass(const C: TByteClass): Integer;
    procedure Patch(const Outs: array of Integer; Target: Integer);
  public
    constructor Create;
    // --- fragment construction -------------------------------------------
    function FragClass(const C: TByteClass): TFrag;   // one byte from a set
    function FragByte(B: Byte): TFrag;                // one specific byte
    function FragEmpty: TFrag;                        // matches the empty string
    function Concat(const A, B: TFrag): TFrag;        // A then B
    function Alternate(const A, B: TFrag): TFrag;     // A or B
    function Star(const A: TFrag; Greedy: Boolean = True): TFrag;      // A*
    function Plus(const A: TFrag; Greedy: Boolean = True): TFrag;      // A+
    function Optional(const A: TFrag; Greedy: Boolean = True): TFrag;  // A?
    // Close the program: the fragment's dangling ends go to a Match state.
    function Finish(const A: TFrag): Integer;         // returns the entry state
    // The bytes a fragment can consume FIRST, and whether it can match nothing
    // at all. Together these decide whether two alternatives can ever match at
    // the same position - which is the only thing that separates POSIX from
    // Perl semantics, so it is what the syntax layer's guard is built on.
    procedure FirstOf(const A: TFrag; out C: TByteClass; out Nullable: Boolean);
    property StateCount: Integer read FNState;
  end;

  { The lazily-built DFA. One instance per compiled pattern; not thread-safe
    (it mutates its cache as it runs), so a shared compiled pattern needs one
    DFA per thread or a lock. }
  TDfa = class
  private
    // The NFA, copied in at construction so the builder can go away.
    FKind: array of TNfaKind;
    FCls: array of Integer;
    FNext1, FNext2: array of Integer;
    FClasses: array of TByteClass;
    FNfaStart: Integer;
    // Subset-construction scratch, kept as fields so a scan allocates nothing.
    FMark: array of Integer;       // NFA state -> generation it was last added in
    FGen: Integer;
    FList: array of Integer;       // the set being built
    FNList: Integer;
    // The DFA state table. Sets are stored end to end in FSetData; FSetOff[i]
    // and FSetLen[i] delimit state i's set.
    FSetData: array of Integer;
    FSetOff, FSetLen: array of Integer;
    FAccept: array of Boolean;
    FNDfa: Integer;
    // Transitions: FTrans[state*256 + b]. -1 means "not computed yet", which is
    // what makes the construction lazy.
    FTrans: array of Integer;
    // Hash index over the state sets, so an existing set is found without a
    // linear scan of every DFA state built so far.
    FBucket: array of Integer;     // hash -> first state, chained through FChain
    FChain: array of Integer;
    FMask: Integer;
    FStart: Integer;
    FFirstBytes: TByteClass;       // bytes that can begin a match (see below)
    FHasFirst: Boolean;
    // Two-byte prefilter: bit (b1*256+b2) is set when a match can begin with that
    // PAIR. 8 KB, so it lives in L1, and the test costs the same as the one-byte
    // one while rejecting far more.
    //
    // ⭐ This exists because of a measurement, not a hunch. With only the
    // one-byte filter, scanning 8 MB of DNA for "agggtaaa|tttaccct" took 77 ms,
    // of which a pattern whose first byte never occurs took 11 - so the scan
    // loop was 14% of it and the other 86% was RESTARTING at candidate
    // positions. {a,t} makes 60% of DNA a candidate; the pairs {ag,tt} make
    // about 15%. Fewer restarts is the whole optimisation: each one ends on a
    // data-dependent branch that this input cannot predict.
    FPair: array[0..8191] of Byte;
    FHasPair: Boolean;
    // Three-byte prefilter. A full table would be 2^24 bits = 2 MB, which no
    // cache holds, so the triple is HASHED into 16 bits and shares the pair
    // table's 8 KB footprint. Collisions are safe by construction: they can only
    // let a non-candidate through, never reject a real match, and the cost of
    // one is a single failed attempt.
    FTriple: array[0..8191] of Byte;
    FHasTriple: Boolean;
    // The two prefilters are built ON DEMAND, not by the constructor - see
    // BuildFilters. False means the scan cascade simply falls back to the
    // first-byte filter: slower, never wrong.
    FFiltersBuilt: Boolean;
    procedure ClosureAdd(S: Integer);
    function InternSet: Integer;
    function ComputeTrans(State: Integer; B: Byte): Integer;
    procedure ComputeFirstBytes;
    procedure ComputePairFilter;
    procedure ComputeTripleFilter;
  public
    constructor Create(ABuilder: TNfaBuilder; ANfaStart: Integer);
    // Longest match ANCHORED at Start (0-based) in Data[0..Len-1]. Returns the
    // end offset (exclusive), or -1 when nothing matches here. A zero-length
    // match returns Start.
    function MatchAt(Data: PByte; Len, Start: Integer): Integer;
    // The leftmost-longest match at or after From. This is the entry point the
    // scanners should use: it folds the "try the next position" loop INSIDE the
    // method, so the cost is one call PER MATCH FOUND rather than one per
    // candidate position. On the DNA patterns that is the difference between a
    // few calls and half a million - measured, it was most of the engine's
    // remaining overhead.
    function FindNext(Data: PByte; Len, From: Integer;
                      out MStart, MEnd: Integer): Boolean;
    // The set of bytes a match can begin with, or nothing when a match can begin
    // with anything. Skipping positions that cannot start a match is what keeps
    // the "try every position" search linear in practice: for the DNA patterns
    // it rejects half the input with one bit test per byte.
    property FirstBytes: TByteClass read FFirstBytes;
    property HasFirstBytes: Boolean read FHasFirst;
    property DfaStates: Integer read FNDfa;
    // --- for a code generator ---------------------------------------------
    // Native code cannot be lazy: every state and every transition has to exist
    // before a single byte is emitted. Materialise returns False when the DFA
    // exceeds MaxStates, which is the signal to stay interpreted rather than
    // spend unbounded time and memory compiling a pathological pattern.
    function Materialise(MaxStates: Integer): Boolean;
    // Build the pair and triple prefilters. EAGER work whose payer is the SCAN:
    // enumerating them computes ~500 subset transitions, which a megabyte-scale
    // scan repays many times over (+97% and +40% when they went in) and a
    // sixteen-byte subject never repays at all. So the constructor no longer
    // does it and the two callers decide instead:
    //   - a pattern going into the CACHE calls BuildFilters before it is
    //     published, because a cached DFA is shared and must be read-only
    //     while it is scanned, and because its cost is amortised over every
    //     later use anyway;
    //   - a per-call pattern leaves it to EnsureFilters, which pays only when
    //     the subject is big enough to earn it back.
    procedure BuildFilters;
    // Build the filters only if this subject is worth them. Threshold measured:
    // the enumeration costs ~47 us and the filters save ~7 ns per scanned byte,
    // so it repays around 7 KB; 16 KB keeps a comfortable margin.
    procedure EnsureFilters(SubjectLen: Integer);
    function Accepting(S: Integer): Boolean;
    function Transition(S: Integer; B: Byte): Integer;
    function StartState: Integer;
    function PairTableAddr: Pointer;
    property HasPairFilter: Boolean read FHasPair;
  end;

type
  // Typed-array views used to reach the transition table and the accept flags
  // through ONE indirection in the scan loop instead of a dynamic-array header
  // load per byte.
  PIntArr = ^TIntArr;
  TIntArr = array[0..(MaxInt div 8)] of Integer;
  PBoolArr = ^TBoolArr;
  TBoolArr = array[0..(MaxInt div 2)] of Boolean;

const
  DFA_DEAD = 0;    // DFA state 0 is always the empty set: no match can follow

var
  // Diagnostic A/B, set from the outside (REGEX_NOFILTER=1 in SedaiRegexEngine).
  // The two prefilters are built EAGERLY by the constructor and they are not
  // cheap: ComputePairFilter walks first-byte x 256 and ComputeTripleFilter
  // first-byte x 256 x 256, and every transition they miss computes an NFA
  // closure. Amortised over a 50 MB scan that is the best money the engine
  // spends; charged to a single short subject it is the whole compile cost.
  // Turning them off answers which of those two a workload is - and it is only
  // ever a MEASUREMENT: without the filters the scan is slower, never wrong.
  GDfaSkipFilters: Boolean = False;
  // How many transitions the subset construction has computed, and how many
  // DFAs have been built. Counters, not a stopwatch: a timer says a thing is
  // slow, a counter says WHAT it did too much of.
  GDfaBuilds: Int64 = 0;
  GDfaTransBuilt: Int64 = 0;

// --- byte-class helpers ------------------------------------------------------
procedure ClsClear(out C: TByteClass);
procedure ClsAdd(var C: TByteClass; B: Byte);
procedure ClsAddRange(var C: TByteClass; Lo, Hi: Byte);
procedure ClsAddAll(var C: TByteClass);
procedure ClsNegate(var C: TByteClass);
function ClsHas(const C: TByteClass; B: Byte): Boolean; inline;
function ClsEmpty(const C: TByteClass): Boolean;

implementation

{ ---------------- byte classes ---------------- }

procedure ClsClear(out C: TByteClass);
var i: Integer;
begin
  for i := 0 to 31 do C[i] := 0;
end;

procedure ClsAdd(var C: TByteClass; B: Byte);
begin
  C[B shr 3] := C[B shr 3] or (1 shl (B and 7));
end;

procedure ClsAddRange(var C: TByteClass; Lo, Hi: Byte);
var i: Integer;
begin
  for i := Lo to Hi do ClsAdd(C, Byte(i));
end;

procedure ClsAddAll(var C: TByteClass);
var i: Integer;
begin
  for i := 0 to 31 do C[i] := $FF;
end;

procedure ClsNegate(var C: TByteClass);
var i: Integer;
begin
  for i := 0 to 31 do C[i] := Byte(not C[i]);
end;

function ClsHas(const C: TByteClass; B: Byte): Boolean;
begin
  Result := (C[B shr 3] and (1 shl (B and 7))) <> 0;
end;

function ClsEmpty(const C: TByteClass): Boolean;
var i: Integer;
begin
  for i := 0 to 31 do
    if C[i] <> 0 then Exit(False);
  Result := True;
end;

{ ---------------- TNfaBuilder ---------------- }

constructor TNfaBuilder.Create;
begin
  inherited Create;
  SetLength(FStates, 64);
  SetLength(FClasses, 16);
  FNState := 0;
  FNClass := 0;
end;

function TNfaBuilder.NewState(AKind: TNfaKind; ACls: Integer): Integer;
begin
  if FNState = Length(FStates) then SetLength(FStates, FNState * 2);
  FStates[FNState].Kind := AKind;
  FStates[FNState].Cls := ACls;
  FStates[FNState].Next1 := -1;
  FStates[FNState].Next2 := -1;
  Result := FNState;
  Inc(FNState);
end;

function TNfaBuilder.AddClass(const C: TByteClass): Integer;
begin
  if FNClass = Length(FClasses) then SetLength(FClasses, FNClass * 2);
  FClasses[FNClass] := C;
  Result := FNClass;
  Inc(FNClass);
end;

procedure TNfaBuilder.Patch(const Outs: array of Integer; Target: Integer);
var i, e: Integer;
begin
  for i := 0 to High(Outs) do
  begin
    e := Outs[i];
    if (e and 1) = 0 then FStates[e shr 1].Next1 := Target
    else FStates[e shr 1].Next2 := Target;
  end;
end;

function TNfaBuilder.FragClass(const C: TByteClass): TFrag;
var s: Integer;
begin
  s := NewState(nkClass, AddClass(C));
  Result.Start := s;
  SetLength(Result.Outs, 1);
  Result.Outs[0] := s * 2;            // the Next1 edge is dangling
end;

function TNfaBuilder.FragByte(B: Byte): TFrag;
var C: TByteClass;
begin
  ClsClear(C);
  ClsAdd(C, B);
  Result := FragClass(C);
end;

function TNfaBuilder.FragEmpty: TFrag;
var s: Integer;
begin
  // A split whose two branches both dangle to the same place: consumes nothing
  // and leaves one edge to patch. Cheaper than a special epsilon state kind.
  s := NewState(nkSplit, -1);
  Result.Start := s;
  SetLength(Result.Outs, 2);
  Result.Outs[0] := s * 2;
  Result.Outs[1] := s * 2 + 1;
end;

function TNfaBuilder.Concat(const A, B: TFrag): TFrag;
begin
  Patch(A.Outs, B.Start);
  Result.Start := A.Start;
  Result.Outs := Copy(B.Outs);
end;

function TNfaBuilder.Alternate(const A, B: TFrag): TFrag;
var s, n, i: Integer;
begin
  s := NewState(nkSplit, -1);
  FStates[s].Next1 := A.Start;
  FStates[s].Next2 := B.Start;
  Result.Start := s;
  n := Length(A.Outs) + Length(B.Outs);
  SetLength(Result.Outs, n);
  for i := 0 to High(A.Outs) do Result.Outs[i] := A.Outs[i];
  for i := 0 to High(B.Outs) do Result.Outs[Length(A.Outs) + i] := B.Outs[i];
end;

function TNfaBuilder.Star(const A: TFrag; Greedy: Boolean): TFrag;
var s: Integer;
begin
  // Greediness is recorded in the branch ORDER. A DFA takes the longest match
  // regardless, so it makes no difference here - but the field is kept because
  // a backtracking fallback built from the same NFA would need it.
  s := NewState(nkSplit, -1);
  if Greedy then FStates[s].Next1 := A.Start else FStates[s].Next2 := A.Start;
  Patch(A.Outs, s);
  Result.Start := s;
  SetLength(Result.Outs, 1);
  if Greedy then Result.Outs[0] := s * 2 + 1 else Result.Outs[0] := s * 2;
end;

function TNfaBuilder.Plus(const A: TFrag; Greedy: Boolean): TFrag;
var s: Integer;
begin
  s := NewState(nkSplit, -1);
  if Greedy then FStates[s].Next1 := A.Start else FStates[s].Next2 := A.Start;
  Patch(A.Outs, s);
  Result.Start := A.Start;             // one pass through A is mandatory
  SetLength(Result.Outs, 1);
  if Greedy then Result.Outs[0] := s * 2 + 1 else Result.Outs[0] := s * 2;
end;

function TNfaBuilder.Optional(const A: TFrag; Greedy: Boolean): TFrag;
var s, i: Integer;
begin
  s := NewState(nkSplit, -1);
  if Greedy then FStates[s].Next1 := A.Start else FStates[s].Next2 := A.Start;
  Result.Start := s;
  SetLength(Result.Outs, Length(A.Outs) + 1);
  for i := 0 to High(A.Outs) do Result.Outs[i] := A.Outs[i];
  if Greedy then Result.Outs[Length(A.Outs)] := s * 2 + 1
  else Result.Outs[Length(A.Outs)] := s * 2;
end;

function TNfaBuilder.Finish(const A: TFrag): Integer;
var m: Integer;
begin
  m := NewState(nkMatch, -1);
  Patch(A.Outs, m);
  Result := A.Start;
end;

procedure TNfaBuilder.FirstOf(const A: TFrag; out C: TByteClass;
                              out Nullable: Boolean);
var
  Seen: array of Boolean;
  Stack: array of Integer;
  NSt, s, i: Integer;

  procedure Push(x: Integer);
  begin
    // A dangling edge means the fragment can be LEFT without consuming a byte:
    // that is exactly what nullable means, and it has to be reported because a
    // nullable alternative matches everywhere, overlapping every other one.
    if x < 0 then begin Nullable := True; Exit; end;
    if Seen[x] then Exit;
    Seen[x] := True;
    Stack[NSt] := x; Inc(NSt);
  end;

begin
  ClsClear(C);
  Nullable := False;
  SetLength(Seen, FNState);
  SetLength(Stack, FNState + 1);
  NSt := 0;
  Push(A.Start);
  while NSt > 0 do
  begin
    Dec(NSt);
    s := Stack[NSt];
    case FStates[s].Kind of
      nkSplit:
        begin
          Push(FStates[s].Next1);
          Push(FStates[s].Next2);
        end;
      nkClass:
        // Consumes: its class joins the first set and the walk stops here.
        for i := 0 to 31 do C[i] := C[i] or FClasses[FStates[s].Cls][i];
      nkMatch:
        Nullable := True;
    end;
  end;
end;

{ ---------------- TDfa ---------------- }

constructor TDfa.Create(ABuilder: TNfaBuilder; ANfaStart: Integer);
var i, n: Integer;
begin
  inherited Create;
  n := ABuilder.FNState;
  SetLength(FKind, n); SetLength(FCls, n);
  SetLength(FNext1, n); SetLength(FNext2, n);
  for i := 0 to n - 1 do
  begin
    FKind[i] := ABuilder.FStates[i].Kind;
    FCls[i] := ABuilder.FStates[i].Cls;
    FNext1[i] := ABuilder.FStates[i].Next1;
    FNext2[i] := ABuilder.FStates[i].Next2;
  end;
  SetLength(FClasses, ABuilder.FNClass);
  for i := 0 to ABuilder.FNClass - 1 do FClasses[i] := ABuilder.FClasses[i];
  FNfaStart := ANfaStart;

  SetLength(FMark, n);
  for i := 0 to n - 1 do FMark[i] := -1;
  FGen := 0;
  SetLength(FList, n + 1);

  FMask := 1023;
  SetLength(FBucket, FMask + 1);
  for i := 0 to FMask do FBucket[i] := -1;
  // Room for EIGHT states, not 64. The transition table is 256 entries per
  // state and every one of them has to be stamped -1 ("not computed yet"), so
  // the old figure charged 16384 stores plus 64 KB of allocation to every DFA
  // ever built - including the ones that end up with four states. Growth
  // doubles and stamps only the new region (see InternSet), so a big automaton
  // pays a handful of reallocations it can well afford, while a short-lived
  // pattern stops paying for room it never uses.
  SetLength(FChain, 8);
  SetLength(FSetOff, 8); SetLength(FSetLen, 8); SetLength(FAccept, 8);
  SetLength(FSetData, 64);
  SetLength(FTrans, 8 * 256);
  for i := 0 to High(FTrans) do FTrans[i] := -1;
  FNDfa := 0;

  // State 0 is the empty set - the dead state. Interning it first means a
  // transition that leads nowhere compares against a constant.
  FNList := 0;
  InternSet;

  // The start state: the closure of the NFA entry.
  Inc(FGen);
  FNList := 0;
  ClosureAdd(FNfaStart);
  FStart := InternSet;

  // The first-byte set stays eager: it is one pass over the start set, it is
  // what makes the skip loop possible at all, and it is the input the other two
  // filters refine.
  ComputeFirstBytes;
  Inc(GDfaBuilds);
end;

procedure TDfa.BuildFilters;
begin
  if FFiltersBuilt then Exit;
  FFiltersBuilt := True;             // set first: the work below is idempotent
  if GDfaSkipFilters then Exit;      // diagnostic A/B, REGEX_NOFILTER=1
  ComputePairFilter;
  ComputeTripleFilter;
end;

procedure TDfa.EnsureFilters(SubjectLen: Integer);
const
  FILTER_MIN_SUBJECT = 16384;
begin
  if FFiltersBuilt then Exit;
  if SubjectLen >= FILTER_MIN_SUBJECT then BuildFilters;
end;

procedure TDfa.ClosureAdd(S: Integer);
begin
  if S < 0 then Exit;
  if FMark[S] = FGen then Exit;         // already in this set
  FMark[S] := FGen;
  if FKind[S] = nkSplit then
  begin
    ClosureAdd(FNext1[S]);
    ClosureAdd(FNext2[S]);
  end
  else
  begin
    FList[FNList] := S;
    Inc(FNList);
  end;
end;

function TDfa.InternSet: Integer;
var
  i, j, h, st, off: Integer;
  same: Boolean;
begin
  // Insertion sort: the sets are small and nearly ordered because ClosureAdd
  // walks the NFA in construction order. A canonical order is what lets two
  // sets be compared - and hashed - as sequences.
  for i := 1 to FNList - 1 do
  begin
    j := i;
    while (j > 0) and (FList[j - 1] > FList[j]) do
    begin
      h := FList[j - 1]; FList[j - 1] := FList[j]; FList[j] := h;
      Dec(j);
    end;
  end;
  h := FNList;
  for i := 0 to FNList - 1 do h := (h * 31 + FList[i]) and MaxInt;
  h := h and FMask;
  st := FBucket[h];
  while st >= 0 do
  begin
    if FSetLen[st] = FNList then
    begin
      same := True;
      off := FSetOff[st];
      for i := 0 to FNList - 1 do
        if FSetData[off + i] <> FList[i] then begin same := False; Break; end;
      if same then Exit(st);
    end;
    st := FChain[st];
  end;
  // Not seen before: append it.
  if FNDfa = Length(FSetOff) then
  begin
    SetLength(FSetOff, FNDfa * 2);
    SetLength(FSetLen, FNDfa * 2);
    SetLength(FAccept, FNDfa * 2);
    SetLength(FChain, FNDfa * 2);
    SetLength(FTrans, FNDfa * 2 * 256);
    for i := FNDfa * 256 to High(FTrans) do FTrans[i] := -1;
  end;
  off := 0;
  if FNDfa > 0 then off := FSetOff[FNDfa - 1] + FSetLen[FNDfa - 1];
  while off + FNList > Length(FSetData) do SetLength(FSetData, Length(FSetData) * 2);
  FSetOff[FNDfa] := off;
  FSetLen[FNDfa] := FNList;
  FAccept[FNDfa] := False;
  for i := 0 to FNList - 1 do
  begin
    FSetData[off + i] := FList[i];
    if FKind[FList[i]] = nkMatch then FAccept[FNDfa] := True;
  end;
  FChain[FNDfa] := FBucket[h];
  FBucket[h] := FNDfa;
  Result := FNDfa;
  Inc(FNDfa);
end;

function TDfa.ComputeTrans(State: Integer; B: Byte): Integer;
var i, s, off, len: Integer;
begin
  Inc(GDfaTransBuilt);
  Inc(FGen);
  FNList := 0;
  off := FSetOff[State];
  len := FSetLen[State];
  for i := 0 to len - 1 do
  begin
    s := FSetData[off + i];
    if (FKind[s] = nkClass) and ClsHas(FClasses[FCls[s]], B) then
      ClosureAdd(FNext1[s]);
  end;
  Result := InternSet;
  // InternSet may have grown FTrans, so index it only now.
  FTrans[State * 256 + B] := Result;
end;

procedure TDfa.ComputeFirstBytes;
var i, s, off, len, b: Integer;
begin
  // Which bytes have a transition out of the start state. When the start state
  // can also accept (the pattern matches the empty string) or every byte leads
  // somewhere, there is nothing to skip and the caller must try every position.
  ClsClear(FFirstBytes);
  FHasFirst := False;
  if FAccept[FStart] then Exit;
  off := FSetOff[FStart];
  len := FSetLen[FStart];
  for i := 0 to len - 1 do
  begin
    s := FSetData[off + i];
    if FKind[s] <> nkClass then Continue;
    for b := 0 to 255 do
      if ClsHas(FClasses[FCls[s]], Byte(b)) then ClsAdd(FFirstBytes, Byte(b));
  end;
  FHasFirst := not ClsEmpty(FFirstBytes);
  for b := 0 to 255 do
    if not ClsHas(FFirstBytes, Byte(b)) then Exit;
  FHasFirst := False;                   // every byte can start a match
end;

procedure TDfa.ComputePairFilter;
const
  STATE_BUDGET = 4096;   // a pattern whose DFA explodes keeps the byte filter
var
  b1, b2, s1, t, i, nset: Integer;
begin
  FHasPair := False;
  for i := 0 to 8191 do FPair[i] := 0;
  if not FHasFirst then Exit;           // nothing to refine
  nset := 0;
  for b1 := 0 to 255 do
  begin
    if not ClsHas(FFirstBytes, Byte(b1)) then Continue;
    t := FTrans[FStart * 256 + b1];
    if t < 0 then t := ComputeTrans(FStart, Byte(b1));
    if t = DFA_DEAD then Continue;
    s1 := t;
    // A one-byte match is already complete here, so ANY second byte (or none)
    // must be allowed through: the filter may never reject a real match.
    if FAccept[s1] then
    begin
      for b2 := 0 to 255 do
      begin
        i := b1 * 256 + b2;
        FPair[i shr 3] := FPair[i shr 3] or (1 shl (i and 7));
      end;
      Inc(nset, 256);
      Continue;
    end;
    for b2 := 0 to 255 do
    begin
      t := FTrans[s1 * 256 + b2];
      if t < 0 then
      begin
        if FNDfa > STATE_BUDGET then Exit;   // give up, FHasPair stays False
        t := ComputeTrans(s1, Byte(b2));
      end;
      if t = DFA_DEAD then Continue;
      i := b1 * 256 + b2;
      FPair[i shr 3] := FPair[i shr 3] or (1 shl (i and 7));
      Inc(nset);
    end;
  end;
  // Only worth the second byte load when it actually narrows things down. The
  // threshold is generous: even a small reduction removes restarts, and a
  // restart costs far more than the extra test.
  FHasPair := (nset > 0) and (nset < 45000);
end;

// The hash a triple is folded through. Must be cheap - it runs once per input
// byte - and must spread the low bits, because for a 4-letter alphabet like DNA
// that is all that varies.
function TripleHash(b0, b1, b2: Integer): Integer; inline;
begin
  Result := (((b0 shl 8) or b1) xor (b2 shl 5)) and $FFFF;
end;

procedure TDfa.ComputeTripleFilter;
const
  TRIPLE_CAP = 20000;   // past this the filter stops discriminating and the
                        // extra byte is pure cost: keep the pair one instead
var
  b0, b1, b2, s1, s2, t, i, n: Integer;
begin
  FHasTriple := False;
  for i := 0 to 8191 do FTriple[i] := 0;
  if not FHasPair then Exit;
  n := 0;
  for b0 := 0 to 255 do
  begin
    if not ClsHas(FFirstBytes, Byte(b0)) then Continue;
    t := FTrans[FStart * 256 + b0];
    if t < 0 then t := ComputeTrans(FStart, Byte(b0));
    if t = DFA_DEAD then Continue;
    // A match can already be complete after one or two bytes, so a third byte
    // says nothing about whether one starts here: no filter is possible.
    if FAccept[t] then Exit;
    s1 := t;
    for b1 := 0 to 255 do
    begin
      t := FTrans[s1 * 256 + b1];
      if t < 0 then t := ComputeTrans(s1, Byte(b1));
      if t = DFA_DEAD then Continue;
      if FAccept[t] then Exit;
      s2 := t;
      for b2 := 0 to 255 do
      begin
        t := FTrans[s2 * 256 + b2];
        if t < 0 then t := ComputeTrans(s2, Byte(b2));
        if t = DFA_DEAD then Continue;
        i := TripleHash(b0, b1, b2);
        if (FTriple[i shr 3] and (1 shl (i and 7))) = 0 then
        begin
          FTriple[i shr 3] := FTriple[i shr 3] or (1 shl (i and 7));
          Inc(n);
          if n > TRIPLE_CAP then Exit;   // bails early: the enumeration itself
                                         // is bounded, not just the result
        end;
      end;
    end;
  end;
  FHasTriple := n > 0;
end;

function TDfa.Accepting(S: Integer): Boolean;
begin
  Result := FAccept[S];
end;

function TDfa.Transition(S: Integer; B: Byte): Integer;
begin
  Result := FTrans[S * 256 + B];
  if Result < 0 then Result := ComputeTrans(S, B);
end;

function TDfa.StartState: Integer;
begin
  Result := FStart;
end;

function TDfa.PairTableAddr: Pointer;
begin
  Result := @FPair[0];
end;

function TDfa.Materialise(MaxStates: Integer): Boolean;
var
  s, b: Integer;
begin
  // Walk every state that exists, computing all 256 of its transitions; new
  // states appear as we go and are picked up because the loop re-reads FNDfa.
  s := 0;
  while s < FNDfa do
  begin
    for b := 0 to 255 do
      if FTrans[s * 256 + b] < 0 then ComputeTrans(s, Byte(b));
    if FNDfa > MaxStates then Exit(False);
    Inc(s);
  end;
  Result := True;
end;

function TDfa.FindNext(Data: PByte; Len, From: Integer;
                       out MStart, MEnd: Integer): Boolean;
var
  p, st, t, i, last: Integer;
  Tr: PIntArr;
  Acc: PBoolArr;
  b: Byte;
begin
  Result := False;
  MStart := -1; MEnd := -1;
  // Cached ONCE per call rather than per byte. ⚠️ FTrans is reallocated when the
  // lazy construction interns a new state, so Tr is re-fetched after every
  // ComputeTrans - which is the cold path and costs nothing once the DFA for
  // this pattern has settled.
  Tr := PIntArr(FTrans);
  Acc := PBoolArr(FAccept);
  p := From;
  while p <= Len do
  begin
    if FHasTriple then
    begin
      // Three bytes, hashed. Strictly instead of the pair test, never as well
      // as: paying for both would give back what the extra selectivity buys.
      // The last two positions have no triple and fall through to the attempt.
      while p < Len - 2 do
      begin
        t := (((Data[p] shl 8) or Data[p + 1]) xor (Data[p + 2] shl 5)) and $FFFF;
        if (FTriple[t shr 3] and (1 shl (t and 7))) <> 0 then Break;
        Inc(p);
      end;
      if p >= Len then Exit;
    end
    else if FHasPair then
    begin
      // One test per position, on the PAIR. The last position has no pair and
      // falls through to the attempt: a one-byte match may legitimately begin
      // there, and letting it through costs one wasted attempt per call.
      //
      // ⛔ Two variants were measured and are WORSE, do not re-try them blind:
      //  - carrying the pair across iterations to save the second load: no
      //    change at all (the loads were never the cost);
      //  - testing the cheap 32-byte byte-table FIRST and the 8 KB pair table
      //    only on survivors: 40 -> 77 ms, nearly twice as slow. It adds a
      //    BRANCH, and this loop is bound by branch misprediction rather than
      //    by work, so a branch costs more than the test it avoids. That is the
      //    same lesson as the AOT's hot loop: count what the machine cannot
      //    predict, not what it has to execute.
      while p < Len - 1 do
      begin
        t := (Data[p] shl 8) or Data[p + 1];
        if (FPair[t shr 3] and (1 shl (t and 7))) <> 0 then Break;
        Inc(p);
      end;
      if p >= Len then Exit;
    end
    else if FHasFirst then
    begin
      // Positions that cannot begin a match, discarded with one bit test each.
      // Never entered for an empty-matching pattern (HasFirstBytes is false
      // there), which is what keeps the end-of-input position reachable.
      while p < Len do
      begin
        b := Data[p];
        if (FFirstBytes[b shr 3] and (1 shl (b and 7))) <> 0 then Break;
        Inc(p);
      end;
      if p >= Len then Exit;
    end;
    st := FStart;
    last := -1;
    if Acc^[st] then last := p;
    i := p;
    while i < Len do
    begin
      t := Tr^[(st shl 8) or Data[i]];
      if t < 0 then
      begin
        t := ComputeTrans(st, Data[i]);
        Tr := PIntArr(FTrans);          // the table may have moved
        Acc := PBoolArr(FAccept);
      end;
      if t = DFA_DEAD then Break;
      st := t;
      Inc(i);
      if Acc^[st] then last := i;       // longest match so far
    end;
    if last >= 0 then
    begin
      MStart := p;
      MEnd := last;
      Exit(True);
    end;
    Inc(p);
  end;
end;

function TDfa.MatchAt(Data: PByte; Len, Start: Integer): Integer;
var
  st, t, i: Integer;
begin
  st := FStart;
  Result := -1;
  if FAccept[st] then Result := Start;
  i := Start;
  while i < Len do
  begin
    // The hot loop: one table lookup, one test, one store. Everything else in
    // this unit exists to keep these three instructions the whole cost.
    t := FTrans[st * 256 + Data[i]];
    if t < 0 then t := ComputeTrans(st, Data[i]);
    if t = DFA_DEAD then Break;
    st := t;
    Inc(i);
    if FAccept[st] then Result := i;    // longest match so far
  end;
end;

end.
