unit SedaiRegexEngine;

{ ============================================================================
  SedaiRegexEngine - regular-expression SYNTAX on top of SedaiAutomaton.

  This is the half that knows what "[^>]*" means. The automaton underneath knows
  only bytes, states and transitions, so a JSON or HTTP scanner would sit BESIDE
  this unit rather than on top of it - which is the whole reason the two are
  separate files.

  ⛔ It deliberately does not implement everything. A pattern using a construct
  outside the regular subset - backreferences, lookaround, anchors, counted
  repetition - fails to COMPILE here and the caller falls back to the library
  engine. Same shape as the AOT's vector loops: a fast path, a check, and the
  path that was always there underneath. Shipping does not have to wait for
  100% coverage, and the worst case of an unsupported pattern is exactly the
  behaviour of the day before.

  ⚠️ THE SEMANTIC DIFFERENCE, stated once and loudly. A DFA is leftmost-LONGEST
  (POSIX). Perl, PCRE and FPC's RegExpr are leftmost-FIRST: there, alternation
  order decides, so "a|ab" against "ab" matches "a", while here it matches "ab".
  The two agree whenever no alternative can match a prefix of what another
  matches at the same position. Every pattern this engine is currently used for
  satisfies that, and CompilePattern REFUSES the ones that might not - see
  AlternationIsPrefixFree. That refusal is what makes the swap observationally
  invisible rather than a silent change of meaning.
  ============================================================================ }

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

interface

uses
  SysUtils, SedaiAutomaton, SedaiRegexNative;

type
  TCompiledRegex = class
  private
    FBuilder: TNfaBuilder;
    FDfa: TDfa;
    FNative: TNativeDfa;      // nil = the interpreted scan is used
  public
    destructor Destroy; override;
    property Dfa: TDfa read FDfa;
    property Native: TNativeDfa read FNative;
  end;

// Compile Pattern. Returns nil when the pattern uses anything outside the
// regular subset this engine implements - the caller must then use its fallback.
function CompilePattern(const Pattern: string): TCompiledRegex;

// The one callers should use. Returns a compiled pattern, or nil when the
// pattern must go to the fallback engine. Owned says who frees it: False means
// the cache holds it and the caller must NOT, True means the caller must.
//
// ⚠️ Compiling is cheap next to scanning a megabyte, and ruinous next to
// scanning twenty bytes - a program calling RegexCount inside a loop over short
// strings would rebuild the whole automaton every iteration. Hence the cache.
//
// SubjectLen is how many bytes this call is about to scan, and it is what lets
// the engine DECLINE. Past the cache cap a pattern cannot be amortised, so on a
// short subject the honest answer is nil: use the library. Without that the
// engine was 14.7x slower than the thing it replaces on patterns built from
// data, which is the one shape where a DFA can never win.
function AcquirePattern(const Pattern: string; out Owned: Boolean;
                        SubjectLen: Integer): TCompiledRegex;

// Non-overlapping leftmost-longest matches of a compiled pattern in S.
function RegexEngineCount(RX: TCompiledRegex; const S: string): Int64;

// Every non-overlapping match replaced by Repl (literal text, no group refs).
function RegexEngineReplace(RX: TCompiledRegex; const S, Repl: string): string;

// ⛔ True when handing this pattern to FPC's RegExpr would KILL THE PROCESS.
//
// TRegExpr 0.987 writes outside its own loop stack on a LAZY quantifier whose operand can match the
// empty string. "(a*)??" or "(a|)??" against any subject ends in an access violation (0xC0000005,
// verified - not a stack overflow), because the non-greedy OP_LOOP arm decrements LoopStackIdx to
// zero and then indexes LoopStack[0], and that array starts at 1. Nothing in our wrapper can catch
// it: it is an out-of-bounds WRITE, so the run that does not crash has corrupted something instead.
//
// The compile-time guard that would have caught it is right there in the unit - "operand could be
// empty" - and it is skipped for '?' alone (regexpr.pas: `and (op <> '?')`), which is precisely the
// quantifier that is wrong.
//
// So the pattern is refused BEFORE the library sees it, and REGEXCOUNT answers 0 the way it does for
// any pattern it cannot honour. A wrong answer is a poor outcome; a dead process is a worse one, and
// this is the only lever we own until the pattern stops needing the library at all.
// 🥅 job/tests/bas/bug_regex_lazy_nullable_crash.bas
function PatternKillsLibrary(const Pattern: string): Boolean;

implementation

type
  TParser = record
    P: PChar;
    Last: PChar;
    B: TNfaBuilder;
    OK: Boolean;
    // Cleared by any alternation whose branches could match at the SAME
    // position - overlapping first bytes, or a branch that matches nothing.
    // While it survives, POSIX and Perl cannot disagree about this pattern
    // however different the branch lengths are, and the fixed-length test is
    // not needed. Checked at EVERY nesting level because ParseAlt recurses.
    Disjoint: Boolean;
  end;

function ParseAlt(var Q: TParser): TFrag; forward;

procedure Fail(var Q: TParser);
begin
  Q.OK := False;
end;

function AtEnd(const Q: TParser): Boolean; inline;
begin
  Result := Q.P > Q.Last;
end;

function Peek(const Q: TParser): Char; inline;
begin
  if Q.P > Q.Last then Result := #0 else Result := Q.P^;
end;

// One escape sequence, as a byte class. Returns False for escapes this engine
// does not implement - notably \1..\9, which are backreferences and are NOT
// regular: no finite automaton can express them, so they must fall back.
function ParseEscape(var Q: TParser; out C: TByteClass): Boolean;
var ch: Char;
begin
  Result := False;
  ClsClear(C);
  if AtEnd(Q) then Exit;
  ch := Q.P^;
  Inc(Q.P);
  case ch of
    'd': ClsAddRange(C, Ord('0'), Ord('9'));
    'D': begin ClsAddRange(C, Ord('0'), Ord('9')); ClsNegate(C); end;
    'w': begin
           ClsAddRange(C, Ord('a'), Ord('z')); ClsAddRange(C, Ord('A'), Ord('Z'));
           ClsAddRange(C, Ord('0'), Ord('9')); ClsAdd(C, Ord('_'));
         end;
    'W': begin
           ClsAddRange(C, Ord('a'), Ord('z')); ClsAddRange(C, Ord('A'), Ord('Z'));
           ClsAddRange(C, Ord('0'), Ord('9')); ClsAdd(C, Ord('_'));
           ClsNegate(C);
         end;
    's': begin
           ClsAdd(C, 32); ClsAdd(C, 9); ClsAdd(C, 10); ClsAdd(C, 11);
           ClsAdd(C, 12); ClsAdd(C, 13);
         end;
    'S': begin
           ClsAdd(C, 32); ClsAdd(C, 9); ClsAdd(C, 10); ClsAdd(C, 11);
           ClsAdd(C, 12); ClsAdd(C, 13); ClsNegate(C);
         end;
    'n': ClsAdd(C, 10);
    'r': ClsAdd(C, 13);
    't': ClsAdd(C, 9);
    'f': ClsAdd(C, 12);
    'v': ClsAdd(C, 11);
    '0': ClsAdd(C, 0);
    '1'..'9': Exit;              // backreference: not regular, fall back
    'b', 'B', 'A', 'Z', 'z', 'G': Exit;   // word/text anchors: not implemented
    'x': Exit;                   // hex escapes: not implemented yet
  else
    ClsAdd(C, Byte(ch));         // \. \| \\ \[ and friends: the literal itself
  end;
  Result := True;
end;

// Exactly one byte in the class, and which one. `\ ` and `\-` are single bytes and may bound a
// range; `\d` and `\w` are SETS and may not - PCRE2 rejects "[\d-x]" outright rather than guessing.
function ClsSingleByte(const C: TByteClass; out B: Byte): Boolean;
var
  i, n: Integer;
begin
  n := 0;
  B := 0;
  for i := 0 to 255 do
    if ClsHas(C, Byte(i)) then
    begin
      Inc(n);
      if n > 1 then Exit(False);
      B := Byte(i);
    end;
  Result := n = 1;
end;

// A bracketed class: [abc] [^abc] [a-z] with escapes inside.
//
// 🐛 An endpoint may be an ESCAPE on EITHER side, and until 2026-08-04 only the right side was
// handled. An escape on the left took a `Continue` that skipped the range test entirely, so
//
//     [\ -~]     read as { ' ', '-', '~' }   instead of the printable range 0x20..0x7E
//     [^\ -~]    matched almost every byte   instead of almost none
//
// - a SILENT wrong answer from the fast engine, which is the one class of defect the fallback design
// cannot absorb. Found by the differential fuzzer against PCRE2 (job/tests/tools/regex_diff.py) on a
// pattern taken from CPython's own standard library, not by reading this function.
// 🥅 job/tests/bas/bug_regex_class_range.bas
function ParseBracket(var Q: TParser; out C: TByteClass): Boolean;
var
  Neg: Boolean;
  Lo, Hi: Byte;
  E: TByteClass;
  i: Integer;
  First, LoIsByte: Boolean;
begin
  Result := False;
  ClsClear(C);
  Neg := False;
  if Peek(Q) = '^' then begin Neg := True; Inc(Q.P); end;
  First := True;
  while not AtEnd(Q) and ((Q.P^ <> ']') or First) do
  begin
    First := False;
    // ---- the left-hand item: an escape (one byte, or a whole set) or a plain byte
    if Q.P^ = '\' then
    begin
      Inc(Q.P);
      if not ParseEscape(Q, E) then Exit;
      LoIsByte := ClsSingleByte(E, Lo);
      if not LoIsByte then
      begin
        // A set escape contributes its members and cannot open a range. If a '-' follows and is not
        // the closing literal, PCRE2 calls the whole class an error; refusing here hands the pattern
        // to the library rather than inventing a meaning for it.
        if (not AtEnd(Q)) and (Q.P^ = '-') and (Q.P < Q.Last) and ((Q.P + 1)^ <> ']') then Exit;
        for i := 0 to 31 do C[i] := C[i] or E[i];
        Continue;
      end;
    end
    else
    begin
      if Q.P^ = '[' then
        // [:alpha:] and friends. Not implemented; a literal '[' inside a class is
        // legal too, so this is only refused when it opens a posix class.
        if (Q.P < Q.Last) and ((Q.P + 1)^ = ':') then Exit;
      Lo := Byte(Q.P^);
      Inc(Q.P);
    end;
    // ---- a range, unless the '-' is the last character before ']' (then literal)
    if (not AtEnd(Q)) and (Q.P^ = '-') and (Q.P < Q.Last) and ((Q.P + 1)^ <> ']') then
    begin
      Inc(Q.P);
      if AtEnd(Q) then Exit;
      if Q.P^ = '\' then
      begin
        Inc(Q.P);
        if not ParseEscape(Q, E) then Exit;
        // Same rule at this end: only a single-byte escape can close a range.
        if not ClsSingleByte(E, Hi) then Exit;
      end
      else
      begin
        Hi := Byte(Q.P^);
        Inc(Q.P);
      end;
      if Hi < Lo then Exit;
      ClsAddRange(C, Lo, Hi);
    end
    else
      ClsAdd(C, Lo);
  end;
  if AtEnd(Q) or (Q.P^ <> ']') then Exit;   // unterminated
  Inc(Q.P);
  if Neg then ClsNegate(C);
  Result := True;
end;

function ParseAtom(var Q: TParser): TFrag;
var
  C: TByteClass;
begin
  Result.Start := -1;
  SetLength(Result.Outs, 0);
  if AtEnd(Q) then begin Fail(Q); Exit; end;
  case Q.P^ of
    '(':
      begin
        Inc(Q.P);
        // (?: (?= (?! (?< ... all of these are outside the plain regular subset
        // or need capture semantics this engine does not model.
        if Peek(Q) = '?' then begin Fail(Q); Exit; end;
        Result := ParseAlt(Q);
        if not Q.OK then Exit;
        if Peek(Q) <> ')' then begin Fail(Q); Exit; end;
        Inc(Q.P);
      end;
    '[':
      begin
        Inc(Q.P);
        if not ParseBracket(Q, C) then begin Fail(Q); Exit; end;
        Result := Q.B.FragClass(C);
      end;
    '.':
      begin
        Inc(Q.P);
        // ModifierS=False in the library wrapper: '.' does not match a newline,
        // which is what PCRE and Python do by default and what the patterns are
        // written for. Keeping the same rule here is part of not changing meaning.
        ClsClear(C); ClsAddAll(C);
        C[10 shr 3] := C[10 shr 3] and not (1 shl (10 and 7));
        Result := Q.B.FragClass(C);
      end;
    '\':
      begin
        Inc(Q.P);
        if not ParseEscape(Q, C) then begin Fail(Q); Exit; end;
        Result := Q.B.FragClass(C);
      end;
    '^', '$':
      begin
        // Anchors need position context the plain byte automaton has no notion
        // of. Deliberately unimplemented rather than approximated.
        Fail(Q); Exit;
      end;
    '*', '+', '?':
      begin
        Fail(Q); Exit;                    // quantifier with nothing to repeat
      end;
  else
    begin
      ClsClear(C);
      ClsAdd(C, Byte(Q.P^));
      Inc(Q.P);
      Result := Q.B.FragClass(C);
    end;
  end;
end;

function ParseRepeat(var Q: TParser): TFrag;
begin
  Result := ParseAtom(Q);
  if not Q.OK then Exit;
  while not AtEnd(Q) do
  begin
    case Q.P^ of
      '*', '+', '?':
        begin
          // ⛔ A LAZY quantifier is a BACKTRACKER'S concept. It says which of several possible
          // matches to PREFER, and a DFA has no preference to express: the subset construction
          // explores every path at once and leftmost-LONGEST is simply what falls out. The builder
          // takes a Greedy flag - it orders the split's two edges, which matters to a backtracker -
          // and the DFA construction cannot honour it.
          // 🐛 Accepting `*?` and then answering greedily is a SILENT WRONG ANSWER, and it was one:
          //      RegexReplace("<a><b>", "<.+?>", "#")   is "##" everywhere else, and was "#" here
          //      RegexCount("aaa", "a+?")               is 3 everywhere else, and was 1 here
          // The semantic guard below never caught it because it inspects ALTERNATION branch lengths,
          // and this is not an alternation. So the pattern is REFUSED and the Perl-compatible
          // library takes it - the same treatment an ambiguous alternation already gets, and the
          // reason the answers a program sees stay PCRE-compatible throughout.
          // 🥅 job/tests/bas/bug_regex_lazy_quantifier.bas
          if (Q.P < Q.Last) and ((Q.P + 1)^ = '?') then begin Fail(Q); Exit; end;
          case Q.P^ of
            '*': Result := Q.B.Star(Result, True);
            '+': Result := Q.B.Plus(Result, True);
            '?': Result := Q.B.Optional(Result, True);
          end;
          Inc(Q.P);
        end;
      '{':
        begin
          // Counted repetition {n,m}. Expressible by expansion, but not done
          // yet - and an unimplemented construct must FAIL rather than be
          // silently read as a literal brace.
          Fail(Q); Exit;
        end;
    else
      Break;
    end;
  end;
end;

function ParseConcat(var Q: TParser): TFrag;
var
  R, Nx: TFrag;
  Any: Boolean;
begin
  Any := False;
  R.Start := -1;
  while (not AtEnd(Q)) and (Q.P^ <> '|') and (Q.P^ <> ')') do
  begin
    Nx := ParseRepeat(Q);
    if not Q.OK then begin Result := R; Exit; end;
    if Any then R := Q.B.Concat(R, Nx) else begin R := Nx; Any := True; end;
  end;
  if not Any then R := Q.B.FragEmpty;
  Result := R;
end;

function ParseAlt(var Q: TParser): TFrag;
var
  R, Nx: TFrag;
  C, Acc: TByteClass;
  Nul: Boolean;
  i: Integer;
  First: Boolean;
begin
  R := ParseConcat(Q);
  if not Q.OK then begin Result := R; Exit; end;
  // While the branches go by, accumulate their FIRST-byte sets. Two branches
  // whose first bytes are disjoint can never both match at the same position,
  // so no question of which one "wins" ever arises - see DisjointAlts.
  ClsClear(Acc);
  First := True;
  Q.B.FirstOf(R, C, Nul);
  if Nul then Q.Disjoint := False
  else for i := 0 to 31 do Acc[i] := C[i];
  while (not AtEnd(Q)) and (Q.P^ = '|') do
  begin
    First := False;
    Inc(Q.P);
    Nx := ParseConcat(Q);
    if not Q.OK then begin Result := R; Exit; end;
    Q.B.FirstOf(Nx, C, Nul);
    if Nul then Q.Disjoint := False
    else
      for i := 0 to 31 do
      begin
        if (Acc[i] and C[i]) <> 0 then Q.Disjoint := False;   // they overlap
        Acc[i] := Acc[i] or C[i];
      end;
    R := Q.B.Alternate(R, Nx);
  end;
  if First then { a single branch: no alternation here, nothing to decide } ;
  Result := R;
end;

{ ---------------- the leftmost-first / leftmost-longest guard ---------------- }

// Can two top-level alternatives match at the same position with DIFFERENT
// lengths? That is precisely when POSIX (this engine) and Perl (the library)
// disagree, so a pattern where it is possible is refused rather than answered
// differently. The test is deliberately CONSERVATIVE and syntactic: it demands
// that every top-level alternative have the same fixed length. That covers the
// literal-and-class alternations these patterns are made of - "agggtaaa|tttaccct",
// "aND|caN|Ha[DS]|WaS" - and refuses anything it cannot be sure about.
// The three below walk the pattern's structure computing each construct's LENGTH,
// where -1 means "variable". They refuse (return False) exactly when an
// alternation has two or more branches that could match different numbers of
// bytes at the same position - the only situation where leftmost-longest and
// leftmost-first can disagree.
//
// ⛔ The first version of this test asked `Pos('|', Pattern) = 0` and then
// required every branch to be fixed-length. That is wrong twice over: an
// ESCAPED pipe is not an alternation (it refused `\|[^|][^|]*\|`, one of
// regex-redux's own patterns, for no reason), and an alternation nested inside
// a group was never examined at all. Structure has to be parsed, not searched.
function AltLen(const S: string; var Idx: Integer; out Len: Integer): Boolean; forward;

// One atom, with its bracket/group/escape skipping. Len is its byte length, or
// -1 when the atom itself is variable (a group containing a quantifier).
function AtomLen(const S: string; var Idx: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 1;
  if Idx > Length(S) then Exit;
  case S[Idx] of
    '(':
      begin
        Inc(Idx);
        if not AltLen(S, Idx, Len) then Exit;
        if (Idx > Length(S)) or (S[Idx] <> ')') then Exit;
        Inc(Idx);
      end;
    '[':
      begin
        Inc(Idx);
        if (Idx <= Length(S)) and (S[Idx] = '^') then Inc(Idx);
        if (Idx <= Length(S)) and (S[Idx] = ']') then Inc(Idx);   // ']' first is a literal
        while (Idx <= Length(S)) and (S[Idx] <> ']') do
        begin
          if S[Idx] = '\' then Inc(Idx);
          Inc(Idx);
        end;
        if Idx > Length(S) then Exit;
        Inc(Idx);
        Len := 1;
      end;
    '\':
      begin
        Inc(Idx, 2);
        if Idx > Length(S) + 1 then Exit;
        Len := 1;
      end;
  else
    begin
      Inc(Idx);
      Len := 1;
    end;
  end;
  Result := True;
end;

// A sequence of quantified atoms, up to '|' or ')'.
function ConcatLen(const S: string; var Idx: Integer; out Len: Integer): Boolean;
var
  n, aLen: Integer;
  Variable: Boolean;
begin
  Result := False;
  n := 0;
  Variable := False;
  while (Idx <= Length(S)) and (S[Idx] <> '|') and (S[Idx] <> ')') do
  begin
    if not AtomLen(S, Idx, aLen) then Exit;
    if (Idx <= Length(S)) and
       ((S[Idx] = '*') or (S[Idx] = '+') or (S[Idx] = '?')) then
    begin
      Inc(Idx);
      if (Idx <= Length(S)) and (S[Idx] = '?') then Inc(Idx);   // lazy form
      Variable := True;
    end
    else if aLen < 0 then Variable := True
    else Inc(n, aLen);
  end;
  if Variable then Len := -1 else Len := n;
  Result := True;
end;

function AltLen(const S: string; var Idx: Integer; out Len: Integer): Boolean;
var
  bl, nbr: Integer;
begin
  Result := False;
  Len := -1;
  nbr := 0;
  repeat
    if not ConcatLen(S, Idx, bl) then Exit;
    Inc(nbr);
    if nbr = 1 then Len := bl
    else if (bl < 0) or (Len < 0) or (bl <> Len) then
      Exit;                     // two branches that can match different lengths
    if (Idx <= Length(S)) and (S[Idx] = '|') then Inc(Idx) else Break;
  until False;
  Result := True;
end;

function AlternationIsPrefixFree(const Pattern: string): Boolean;
var
  Idx, L: Integer;
begin
  Idx := 1;
  Result := AltLen(Pattern, Idx, L) and (Idx > Length(Pattern));
end;

{ ------------- the crash guard on the FALLBACK library (see the interface) ----
  A syntactic scan that answers one question: does the pattern apply a LAZY
  quantifier to something that can match the empty string? That is the shape
  TRegExpr indexes its loop stack out of bounds on.

  Three properties are deliberate:
   - TOTAL. It never fails and never raises; every branch advances or ends. It
     runs on patterns this engine cannot itself parse, so "give up" is not an
     option available to it.
   - CONSERVATIVE in the safe direction. Where it cannot tell, it says nullable,
     because a false alarm costs one pattern its answer and a miss costs the
     process.
   - NARROW. A pattern without a '?' anywhere never enters the scan, so the
     ordinary case pays one Pos().
  --------------------------------------------------------------------------- }
type
  TNulScan = record
    S: string;
    I: Integer;          // 1-based, like the string
    Danger: Boolean;
  end;

function ScanAlt(var Z: TNulScan; Depth: Integer): Boolean; forward;

// Returns: can this atom match the empty string?
function ScanAtom(var Z: TNulScan; Depth: Integer): Boolean;
begin
  Result := False;
  if Z.I > Length(Z.S) then Exit(True);          // nothing left: the empty string
  case Z.S[Z.I] of
    '(':
      begin
        Inc(Z.I);
        if (Z.I <= Length(Z.S)) and (Z.S[Z.I] = '?') then
        begin
          // (?: (?i) (?P<n> (?= ... - step over whatever header this is. Getting the KIND of group
          // wrong does not matter here; only what is inside it does.
          Inc(Z.I);
          while (Z.I <= Length(Z.S)) and (Z.S[Z.I] <> ':') and (Z.S[Z.I] <> ')')
                and (Z.S[Z.I] <> '>') do Inc(Z.I);
          if (Z.I <= Length(Z.S)) and (Z.S[Z.I] <> ')') then Inc(Z.I);
        end;
        if Depth > 32 then                        // absurd nesting: stop, and assume the worst
        begin
          Z.Danger := True;
          Exit(True);
        end;
        Result := ScanAlt(Z, Depth + 1);
        if (Z.I <= Length(Z.S)) and (Z.S[Z.I] = ')') then Inc(Z.I);
      end;
    '[':
      begin
        Inc(Z.I);
        if (Z.I <= Length(Z.S)) and (Z.S[Z.I] = '^') then Inc(Z.I);
        if (Z.I <= Length(Z.S)) and (Z.S[Z.I] = ']') then Inc(Z.I);   // ']' first is a literal
        while (Z.I <= Length(Z.S)) and (Z.S[Z.I] <> ']') do
        begin
          if Z.S[Z.I] = '\' then Inc(Z.I);
          Inc(Z.I);
        end;
        if Z.I <= Length(Z.S) then Inc(Z.I);
      end;
    '\':
      begin
        Inc(Z.I);
        if Z.I <= Length(Z.S) then
        begin
          // the word and text boundaries are ZERO WIDTH, so they match empty
          case Z.S[Z.I] of
            'b', 'B', 'A', 'Z', 'z', 'G': Result := True;
          end;
          Inc(Z.I);
        end;
      end;
    '^', '$':
      begin
        Inc(Z.I);
        Result := True;                           // an anchor consumes nothing
      end;
  else
    Inc(Z.I);
  end;
end;

// One atom and its quantifiers. Sets Danger when a LAZY one is applied to something nullable.
function ScanPiece(var Z: TNulScan; Depth: Integer): Boolean;
var
  Nul, MinZero, Lazy: Boolean;
  j: Integer;
begin
  Nul := ScanAtom(Z, Depth);
  while (Z.I <= Length(Z.S)) and (Z.S[Z.I] in ['*', '+', '?', '{']) do
  begin
    MinZero := False;
    if Z.S[Z.I] = '{' then
    begin
      // A '{' not followed by a digit is a literal brace, not a quantifier.
      j := Z.I + 1;
      if (j > Length(Z.S)) or not (Z.S[j] in ['0'..'9']) then Break;
      MinZero := Z.S[j] = '0';
      while (j <= Length(Z.S)) and (Z.S[j] <> '}') do Inc(j);
      Z.I := j;
      if Z.I <= Length(Z.S) then Inc(Z.I);
    end
    else
    begin
      MinZero := Z.S[Z.I] <> '+';
      Inc(Z.I);
    end;
    Lazy := (Z.I <= Length(Z.S)) and (Z.S[Z.I] = '?');
    if Lazy then Inc(Z.I);
    if Lazy and Nul then Z.Danger := True;
    if MinZero then Nul := True;
  end;
  Result := Nul;
end;

function ScanConcat(var Z: TNulScan; Depth: Integer): Boolean;
var
  AllNul: Boolean;
  Was: Integer;
begin
  AllNul := True;                                 // an empty branch matches the empty string
  while (Z.I <= Length(Z.S)) and (Z.S[Z.I] <> '|') and (Z.S[Z.I] <> ')') do
  begin
    Was := Z.I;
    if not ScanPiece(Z, Depth) then AllNul := False;
    if Z.I <= Was then Inc(Z.I);                  // never stand still, whatever the input
  end;
  Result := AllNul;
end;

function ScanAlt(var Z: TNulScan; Depth: Integer): Boolean;
begin
  Result := ScanConcat(Z, Depth);
  while (Z.I <= Length(Z.S)) and (Z.S[Z.I] = '|') do
  begin
    Inc(Z.I);
    if ScanConcat(Z, Depth) then Result := True;  // one nullable branch is enough
  end;
end;

function PatternKillsLibrary(const Pattern: string): Boolean;
var
  Z: TNulScan;
begin
  if Pos('?', Pattern) = 0 then Exit(False);      // no lazy form can exist without one
  Z.S := Pattern;
  Z.I := 1;
  Z.Danger := False;
  ScanAlt(Z, 0);
  Result := Z.Danger;
end;

{ ---------------- public ---------------- }

destructor TCompiledRegex.Destroy;
begin
  FNative.Free;      // before FDfa: the emitted code holds a pointer into it
  FDfa.Free;
  FBuilder.Free;
  inherited Destroy;
end;

// REGEX_NATIVE=1 compiles each pattern's DFA to machine code (SedaiRegexNative).
// ⛔ OFF by default, and that is a MEASURED verdict rather than caution: on the
// regex-redux patterns it is 4% SLOWER than the interpreted table walk (COUNT
// 427 -> 446 ms over 8 MB). It removes table loads, which were never the cost;
// the scan is bound by unpredictable BRANCHES, and a compare chain has exactly
// the same ones. Kept in the tree because the machinery is the interesting part
// and a scanner over structured input - JSON, HTTP - has far more predictable
// branches than a regex over random DNA. Re-measure there before believing it.
function NativeWanted: Boolean;
begin
  Result := GetEnvironmentVariable('REGEX_NATIVE') = '1';
end;

function CompilePattern(const Pattern: string): TCompiledRegex;
var
  Q: TParser;
  F: TFrag;
  Entry: Integer;
  RX: TCompiledRegex;
  FixedOK: Boolean;
begin
  Result := nil;
  if Pattern = '' then Exit;
  // Cheap syntactic test first; the structural one needs the NFA, so it is
  // decided during the parse and re-checked below.
  FixedOK := AlternationIsPrefixFree(Pattern);
  RX := TCompiledRegex.Create;
  try
    RX.FBuilder := TNfaBuilder.Create;
    Q.P := PChar(Pattern);
    Q.Last := Q.P + Length(Pattern) - 1;
    Q.B := RX.FBuilder;
    Q.OK := True;
    Q.Disjoint := True;
    F := ParseAlt(Q);
    if (not Q.OK) or (not AtEnd(Q)) then
    begin
      // Not fully consumed means a stray ')' or an unimplemented construct.
      FreeAndNil(RX);
      Exit;
    end;
    // ⭐ TWO sufficient conditions, and either will do. Every branch the same
    // fixed length, OR no two branches able to start at the same byte. The
    // second is what lets regex-redux's strip pattern through - its two
    // alternatives begin with '>' and with a newline, so they cannot both match
    // anywhere, and their wildly different lengths are irrelevant. The purely
    // syntactic test refused it,
    // and that refusal was costing a fifth of regex-redux to the old library.
    if not (FixedOK or Q.Disjoint) then
    begin
      FreeAndNil(RX);
      Exit;
    end;
    Entry := RX.FBuilder.Finish(F);
    RX.FDfa := TDfa.Create(RX.FBuilder, Entry);
    if NativeWanted then
    begin
      // Native code cannot be lazy about ANYTHING: it reads HasPairFilter at
      // emit time and bakes the answer into the instruction stream, so the
      // filters have to exist first or the emitted scan silently loses them.
      // The A/B against the interpreted walk is only fair if both have it.
      RX.FDfa.BuildFilters;
      RX.FNative := CompileDfaNative(RX.FDfa);
    end;
    Result := RX;
  except
    RX.Free;
    Result := nil;
  end;
end;

{ ---------------- the compiled-pattern cache ---------------------------------

  Two things make this safe to share between threads, and both matter:

  1. A cached entry's DFA is MATERIALISED - every state and every transition is
     computed before it is published. A lazily-built DFA mutates its own tables
     as it scans, so two threads scanning through one would race; a materialised
     one is read-only, and the scan writes nothing but its own locals.
  2. Entries are NEVER evicted. Only the lookup and the insert take the lock;
     scanning happens outside it, with a bare pointer to the entry. If entries
     could be evicted, that pointer could be freed under a thread still using
     it - and the fix for THAT is reference counting, which costs an atomic on
     every call. A bounded cache that stops accepting new patterns has no such
     hazard: past the cap, patterns are simply compiled per call and owned by
     the caller, which is exactly the behaviour of the day before there was a
     cache.

  A nil entry is cached too: a pattern that will never compile here should be
  parsed once, not on every call, or the fallback path pays for the attempt
  forever.
  ------------------------------------------------------------------------- }
const
  CACHE_MAX = 64;
  // Below this many bytes a pattern that CANNOT be cached is not worth
  // compiling: see the GCacheFull branch below for the arithmetic.
  OWNED_MIN_SUBJECT = 4096;
  // States a cached DFA may have. The table is 1 KB per state, so this is the
  // memory a single cached pattern is allowed to cost; a bigger automaton is
  // still usable, just not cacheable.
  MATERIALISE_CAP = 1024;

var
  GCacheLock: TRTLCriticalSection;
  GCacheKey: array[0..CACHE_MAX - 1] of string;
  GCacheHash: array[0..CACHE_MAX - 1] of Cardinal;
  GCacheVal: array[0..CACHE_MAX - 1] of TCompiledRegex;   // nil = known-uncompilable
  GCacheN: Integer = 0;
  GCacheFull: Boolean = False;
  GCacheOff: Integer = -1;   // -1 = the environment has not been read yet
  GRegexDiag: Boolean = False;
  GRegexNoFastCopy: Boolean = False;   // REGEX_NOSLAB=1: the A/B arm, see RegexEngineReplace

function PatHash(const S: string): Cardinal;
var i: Integer;
begin
  Result := 2166136261;
  for i := 1 to Length(S) do
    Result := (Result xor Byte(S[i])) * 16777619;
end;

function AcquirePattern(const Pattern: string; out Owned: Boolean;
                        SubjectLen: Integer): TCompiledRegex;
var
  h: Cardinal;
  i: Integer;
  RX: TCompiledRegex;
begin
  Owned := False;
  // REGEX_CACHE=0 compiles on every call, which is what this replaced: the A/B
  // on one binary, so the cache's value is a measurement and not a claim.
  if GCacheOff < 0 then
    if GetEnvironmentVariable('REGEX_CACHE') = '0' then GCacheOff := 1 else GCacheOff := 0;
  if GCacheOff = 1 then
  begin
    Owned := True;
    Exit(CompilePattern(Pattern));
  end;
  h := PatHash(Pattern);
  // ⚡ NO try..finally around the lock, and the reason is measured rather than stylistic: on win64
  // FPC implements try..finally with a setjmp-style frame, and installing one cost ~0.7 us per call
  // - roughly HALF the price of a cache hit, on a lookup whose real work is a hash and a compare.
  // The lock is released explicitly on every exit below instead. That is only safe because nothing
  // between here and the release can raise: array indexing, an integer compare and a string compare.
  // ⚠️ If anything that can fail is ever added inside this region, the frame has to come back.
  EnterCriticalSection(GCacheLock);
  begin
    for i := 0 to GCacheN - 1 do
      if (GCacheHash[i] = h) and (GCacheKey[i] = Pattern) then
      begin
        LeaveCriticalSection(GCacheLock);
        Exit(GCacheVal[i]);
      end;
    if GCacheFull then
    begin
      // Past the cap there is nothing to amortise over: this pattern is
      // compiled for THIS call and thrown away. Whether that is a good deal
      // depends entirely on the subject.
      //
      // Building a DFA costs ~38 us. Scanning with one saves ~50 ns per byte
      // against the backtracker, so the trade repays somewhere under a
      // kilobyte - above the threshold, compile; below it, DECLINE and let the
      // library have it. A DFA pays up front to be fast later, and on a
      // sixteen-byte subject there is no later: measured, that is where the
      // engine was 14.7x SLOWER than the thing it replaces.
      //
      // ⚠️ nil is NOT cached here - the caller's cache-insert path is not
      // reached on this branch - so declining is a decision about one call and
      // never becomes a verdict about the pattern.
      // The lock is already being released for good here: both exits below are
      // final, so there is nothing to re-acquire for.
      LeaveCriticalSection(GCacheLock);
      if SubjectLen < OWNED_MIN_SUBJECT then Exit(nil);
      Owned := True;
      Exit(CompilePattern(Pattern));
    end;
  end;
  LeaveCriticalSection(GCacheLock);

  // Compile OUTSIDE the lock: it is the slow part, and two threads warming
  // different patterns should not queue behind each other.
  RX := CompilePattern(Pattern);
  if (RX <> nil) and not RX.FDfa.Materialise(MATERIALISE_CAP) then
  begin
    // Too big to publish read-only. Give it to the caller instead of racing -
    // and leave its filters unbuilt, so this per-call pattern is charged for
    // them only if it actually scans enough to earn them back.
    Owned := True;
    Exit(RX);
  end;
  // Everything a shared DFA will ever need must exist BEFORE it is published:
  // once other threads can reach it, the invariant that keeps the hot path
  // lock-free is that nobody writes to it. Building the filters here is also
  // where they belong on cost grounds - a cached pattern is one that gets used
  // again, so the enumeration is amortised over every later call.
  if RX <> nil then RX.FDfa.BuildFilters;

  EnterCriticalSection(GCacheLock);
  try
    // Another thread may have inserted the same pattern while we compiled.
    for i := 0 to GCacheN - 1 do
      if (GCacheHash[i] = h) and (GCacheKey[i] = Pattern) then
      begin
        RX.Free;
        Exit(GCacheVal[i]);
      end;
    if GCacheN < CACHE_MAX then
    begin
      GCacheKey[GCacheN] := Pattern;
      GCacheHash[GCacheN] := h;
      GCacheVal[GCacheN] := RX;
      Inc(GCacheN);
      if GCacheN = CACHE_MAX then GCacheFull := True;
      Exit(RX);                    // borrowed: the cache owns it now
    end;
    Owned := True;                 // filled up in the meantime
    Result := RX;
  finally
    LeaveCriticalSection(GCacheLock);
  end;
end;

function RegexEngineCount(RX: TCompiledRegex; const S: string): Int64;
var
  D: PByte;
  Dfa: TDfa;
  Len, p, ms, me: Integer;
  A: TScanArgs;
begin
  Result := 0;
  Len := Length(S);
  // An empty subject answers 0 here to match the caller's rule - see RegexCountMatches in
  // SedaiBytecodeVM: the fallback library cannot match an empty subject at all, so answering the
  // PCRE2 way on this path alone would make the engine choice observable.
  if Len = 0 then Exit;
  D := PByte(PChar(S));
  Dfa := RX.FDfa;
  // Now that the subject's size is known, decide whether this scan is worth the
  // prefilters. A cached DFA already has them and this is a no-op - which is
  // what keeps a SHARED automaton read-only here.
  Dfa.EnsureFilters(Len);
  p := 0;
  Inc(GDfaBytes, Len);               // once per SCAN, see the counter comment in SedaiAutomaton
  // One call per MATCH, not one per candidate position: the position loop lives
  // inside FindNext. ⚠️ A pattern that can match the empty string also matches
  // just past the last byte - "a*" over "aaa" is two matches - which is why
  // FindNext scans to Len inclusive. Losing that cost exactly one match per
  // call, and it was the differential net that noticed, not a reading of the code.
  if RX.FNative <> nil then
  begin
    A.Data := D; A.Len := Len;
    while p <= Len do
    begin
      A.From := p;
      if RX.FNative.Fn(@A) = 0 then Break;
      Inc(Result);
      if A.MEnd > A.MStart then p := A.MEnd else p := A.MStart + 1;
    end;
    Exit;
  end;
  while p <= Len do
  begin
    if not Dfa.FindNext(D, Len, p, ms, me) then Break;
    Inc(Result);
    if me > ms then p := me else p := ms + 1;   // a zero-length match must advance
  end;
end;

function RegexEngineReplace(RX: TCompiledRegex; const S, Repl: string): string;
var
  D, Dst, Src: PByte;
  Dfa: TDfa;
  Len, p, ms, me, i, NM, OutLen, SrcPos, DstPos, SegLen, RL, DstLim, SrcLim: Integer;
  MStart, MEnd: array of Integer;
  A: TScanArgs;
  Nat: TNativeDfa;
  RBuf: array[0..15] of Byte;
  Matched: Int64;
begin
  Result := S;
  Len := Length(S);
  if Len = 0 then Exit;              // see RegexEngineCount: uniform with the fallback library
  D := PByte(PChar(S));
  Dfa := RX.FDfa;
  Dfa.EnsureFilters(Len);            // see RegexEngineCount
  Inc(GDfaBytes, Len);               // once per SCAN, see the counter comment in SedaiAutomaton
  NM := 0;
  Matched := 0;
  SetLength(MStart, 64);
  SetLength(MEnd, 64);
  p := 0;
  // Same walk as RegexEngineCount, keeping the spans so the output can be
  // measured before it is built. `<= Len` for the same empty-match reason.
  Nat := RX.FNative;
  A.Data := D; A.Len := Len;
  while p <= Len do
  begin
    if Nat <> nil then
    begin
      A.From := p;
      if Nat.Fn(@A) = 0 then Break;
      ms := A.MStart; me := A.MEnd;
    end
    else if not Dfa.FindNext(D, Len, p, ms, me) then Break;
    if NM = Length(MStart) then
    begin
      SetLength(MStart, NM * 2);
      SetLength(MEnd, NM * 2);
    end;
    MStart[NM] := ms;
    MEnd[NM] := me;
    Inc(Matched, me - ms);           // the walk already has both ends in registers, so the
    Inc(NM);                         // separate sizing loop was a whole extra pass over 28MB
    if me > ms then p := me else p := ms + 1;
  end;
  if NM = 0 then Exit;
  // One allocation of exactly the right size, then assemble into it.
  RL := Length(Repl);
  OutLen := Len - Matched + Int64(NM) * RL;
  Result := '';
  SetLength(Result, OutLen);
  Src := D;                          // S still owns the buffer: D stayed valid across the two lines above
  Dst := PByte(PChar(Result));
  DstPos := 0;
  SrcPos := 0;
  if GRegexNoFastCopy then
  begin
    for i := 0 to NM - 1 do          // the shape this replaced, kept as the A/B arm
    begin
      SegLen := MStart[i] - SrcPos;
      if SegLen > 0 then
      begin
        Move((Src + SrcPos)^, (Dst + DstPos)^, SegLen);
        Inc(DstPos, SegLen);
      end;
      if RL > 0 then
      begin
        Move(Repl[1], (Dst + DstPos)^, RL);
        Inc(DstPos, RL);
      end;
      SrcPos := MEnd[i];
    end;
  end
  else
  begin
    // ⭐ MEASURE THE GAP, NOT THE TOTAL. The five substitutions of regex-redux leave
    // FOURTEEN bytes between matches on average (3,5M matches over 50MB), so this loop
    // calls Move seven million times with a length Move spends most of its time DECIDING
    // how to handle - 17 ns apiece, and almost none of it copying. Two QWord stores do any
    // gap up to 16 bytes with no call and no branch.
    // ⚠️ THE GUARD IS ROOM, NOT LENGTH. Over-WRITING the destination is harmless: writes
    // are strictly sequential, so every byte past DstPos+SegLen belongs to a later write
    // that will cover it. Over-READING the subject is NOT harmless (the page after it need
    // not be mapped), so the last 16 bytes of either side take the general path.
    FillChar(RBuf, SizeOf(RBuf), 0);
    if (RL > 0) and (RL <= 16) then Move(Repl[1], RBuf[0], RL);
    DstLim := OutLen - 16;
    SrcLim := Len - 16;
    for i := 0 to NM - 1 do
    begin
      SegLen := MStart[i] - SrcPos;
      if SegLen > 0 then
      begin
        if (SegLen <= 16) and (SrcPos <= SrcLim) and (DstPos <= DstLim) then
        begin
          PQWord(Dst + DstPos)^ := PQWord(Src + SrcPos)^;
          PQWord(Dst + DstPos + 8)^ := PQWord(Src + SrcPos + 8)^;
        end
        else
          Move((Src + SrcPos)^, (Dst + DstPos)^, SegLen);
        Inc(DstPos, SegLen);
      end;
      if RL > 0 then
      begin
        // RBuf, not Repl: a 16-byte read off a three-byte string is the same fault, and a
        // local array is the only place the over-read is guaranteed to land in our own frame.
        if (RL <= 16) and (DstPos <= DstLim) then
        begin
          PQWord(Dst + DstPos)^ := PQWord(@RBuf[0])^;
          PQWord(Dst + DstPos + 8)^ := PQWord(@RBuf[8])^;
        end
        else
          Move(Repl[1], (Dst + DstPos)^, RL);
        Inc(DstPos, RL);
      end;
      SrcPos := MEnd[i];
    end;
  end;
  SegLen := Len - SrcPos;
  if SegLen > 0 then Move((Src + SrcPos)^, (Dst + DstPos)^, SegLen);
end;

var
  i: Integer;

initialization
  InitCriticalSection(GCacheLock);
  // REGEX_NOFILTER=1 builds every DFA without its two prefilters, and
  // REGEX_DIAG=1 reports what the construction actually did. Together they
  // answer "what does a FRESH pattern spend its time on", which a stopwatch on
  // the whole call cannot: the filters are eager work whose payer is the SCAN,
  // so a workload of short subjects is charged for something it never uses.
  GDfaSkipFilters := GetEnvironmentVariable('REGEX_NOFILTER') = '1';
  // REGEX_NOVEC=1 keeps the scalar filters and drops the SSE2 one: the A/B for the vector prefilter
  // on a single binary.
  GDfaSkipVec := GetEnvironmentVariable('REGEX_NOVEC') = '1';
  // REGEX_NOSLAB=1 assembles the replacement one Move per segment, the way it was before the
  // gaps were measured. Same binary, so the A/B is not a build-to-build reading.
  GRegexNoFastCopy := GetEnvironmentVariable('REGEX_NOSLAB') = '1';
  GRegexDiag := GetEnvironmentVariable('REGEX_DIAG') = '1';

finalization
  if GRegexDiag then
  begin
    WriteLn(ErrOutput, 'regex: dfas=', GDfaBuilds, ' transitions=', GDfaTransBuilt,
            ' cached=', GCacheN, ' filters=', not GDfaSkipFilters);
    // The fall-through rate: of every position offered to FindNext, how many survived the filters
    // and started a DFA walk. That ratio - not a stopwatch - says whether a phase is filter-bound.
    if GDfaBytes > 0 then
      WriteLn(ErrOutput, '  bytes=', GDfaBytes, ' attempts=', GDfaAttempts,
              ' (', (GDfaAttempts * 1000) div GDfaBytes, ' per mille)',
              ' steps=', GDfaSteps,
              ' steps/attempt=', (GDfaSteps * 100) div (GDfaAttempts + 1), '/100');
    // Per pattern, the DFA STATE COUNT - because that is what decides whether a 16-state SIMD walk
    // (Sheng-style: one pshufb per byte, the transition table addressed by the INPUT byte instead of
    // by the state, so the load leaves the dependency chain) could replace the table walk for it.
    // ⚠️ States BUILT, and the DFA is built lazily: this is what the scan actually reached, not the
    // whole reachable automaton. After a full pass over real input the two coincide in practice, but
    // a short subject will under-report. `DECLINED` = the pattern was refused and went to the library.
    for i := 0 to GCacheN - 1 do
      if GCacheVal[i] <> nil then
        WriteLn(ErrOutput, '  states=', GCacheVal[i].FDfa.DfaStates:4,
                ' vec=', GCacheVal[i].FDfa.VecPrefixLen:2, 'x', GCacheVal[i].FDfa.VecPrefixCount:2,
                '   ', GCacheKey[i])
      else
        WriteLn(ErrOutput, '  DECLINED      ', GCacheKey[i]);
  end;
  for i := 0 to GCacheN - 1 do GCacheVal[i].Free;
  DoneCriticalSection(GCacheLock);

end.
