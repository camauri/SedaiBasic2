unit SedaiNamespace;

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

// FreeBASIC NAMESPACE flattening (AST -> AST), run once before SSA generation.
//
// A NAMESPACE groups declarations (TYPE / SUB / FUNCTION / CONST / DIM) under a name. This pass
// rewrites the AST so the rest of the compiler never has to know about namespaces:
//   * every declared member is renamed to its mangled "NS.member" name (a plain dotted string —
//     opaque to the downstream string-keyed name resolution: variables in FVarMap, procedures as
//     PROC_NS.NAME labels, UDTs via FindUDT("NS.T"));
//   * a qualified reference "NS.member" (parsed as a member-access whose base is a namespace name)
//     is collapsed to the mangled identifier;
//   * an unqualified reference inside the namespace body that matches a member is prefixed (unless a
//     local/parameter of the enclosing procedure shadows it);
//   * the antNamespace wrappers are removed, hoisting the (now mangled) members to module level.
//
// With no NAMESPACE in the program the pass is a pure no-op: NamespaceNames stays empty, so neither
// the collapse nor the prefix rule ever fires, and record "obj.field" accesses are untouched.
//
// v1 scope: data TYPEs, free SUB/FUNCTION, CONST, module-level DIM (implicitly shared, per FB),
// qualified access from outside, unqualified resolution inside, namespace reopening, and nesting
// (dotted "NAMESPACE Outer.Inner" or physically nested blocks). Deferred: methods of a namespaced
// TYPE, USING, the "..global" duplicate-symbol escape, Alias, anonymous namespaces.

interface

uses
  SedaiParserTypes, sedaiast;

// Mutates AST in place. Safe to call unconditionally (no-op without NAMESPACE blocks).
procedure FlattenNamespaces(AST: TASTNode);

implementation

uses
  Classes, SysUtils, Variants, contnrs;

type
  TNsContext = class
    NamespaceNames: TStringList;   // every effective namespace prefix (UPPER), e.g. FORMS, OUTER.INNER
    MemberKeys: TStringList;       // "PREFIX|MEMBER" for each declared member (membership test)
    GlobalNames: TStringList;      // names declared at MODULE level, outside every namespace.
                                   // ⭐ Each carries the TOP-LEVEL INDEX it was declared at, in
                                   // Objects[]: fbc's screen on an imported name is POSITIONAL.
    CurTopIndex: Integer;          // the module-level statement currently being rewritten (MaxInt = unknown)
    CurNsPrefix: string;           // the namespace whose own children we are rewriting ('' = none)
    CurNsIndex: Integer;           // ...and which of its children (MaxInt = unknown)
                                   // ⛔ They WIN over a name a USING imported: fbc resolves an
                                   // unqualified reference against the global scope before the
                                   // imported ones, so "Dim Shared v" beside "Using A" (which also
                                   // has a v) means the global v. Without this the import silently
                                   // took over a name the program had declared itself.
    // ⭐ What each namespace ITSELF imports, as "PREFIX|IMPORTED". A "Using N" brings in what N can
    // SEE, not only what N declares: fbc's namespace/using2 nests three deep (ns3 uses ns2, ns2 uses
    // ns1) and reads a name of ns1 through ns3. Without this the import stopped one level down and the
    // name resolved to nothing - it printed 0.
    NsUsings: TStringList;
    // ⭐ ...and, separately, the TYPE names the program declares at MODULE level. GlobalNames above is
    // the right authority for a VARIABLE reference and the wrong one for a TYPE SLOT: BASIC is
    // case-insensitive, so "Dim p As P" names the variable and the type alike, and the variable's own
    // name in GlobalNames blocked the import for the TYPE occurrence beside it. A type slot asks this
    // set instead - only a module-level TYPE can outrank an imported one.
    GlobalTypeNames: TStringList;
    // ⭐ ...and the FIELD names of every TYPE the program declares, "TYPE=F1,F2,...". Inside a member
    // procedure a bare field name is an implicit-THIS reference and SHADOWS a namespace member exactly
    // as a parameter does - and nothing here knew it: a namespace declaring "Type foo" with a field
    // "bar" AND a "Type bar" beside it had "bar = 1234" inside foo's constructor rewritten to
    // "NS.BAR", so the field was never written and every read of it answered 0. fbc's dim/auto_var2
    // writes that cross-shadowed pair on purpose.
    TypeFieldNames: TStringList;
    constructor Create;
    destructor Destroy; override;
    function IsMember(const Prefix, Name: string): Boolean;
    // The imports of Prefix, its imports' imports, and so on. Cycle-safe: a namespace already in Acc
    // is never expanded twice, which is what makes a mutual "using" terminate.
    procedure AddUsingClosure(const Prefix: string; Acc: TStringList);
    function MemberIndex(const Prefix, Name: string): Integer;
  end;

constructor TNsContext.Create;
begin
  NsUsings := TStringList.Create;
  NsUsings.Duplicates := dupIgnore;
  NsUsings.Sorted := True;
  GlobalTypeNames := TStringList.Create;
  TypeFieldNames := TStringList.Create;
  TypeFieldNames.CaseSensitive := False;
  GlobalTypeNames.Duplicates := dupIgnore;
  GlobalTypeNames.Sorted := True;
  NamespaceNames := TStringList.Create;
  NamespaceNames.Duplicates := dupIgnore;
  NamespaceNames.Sorted := True;
  MemberKeys := TStringList.Create;
  MemberKeys.Duplicates := dupIgnore;
  MemberKeys.Sorted := True;
  GlobalNames := TStringList.Create;
  CurTopIndex := MaxInt;
  CurNsPrefix := '';
  CurNsIndex := MaxInt;
  GlobalNames.Duplicates := dupIgnore;
  GlobalNames.Sorted := True;
end;

destructor TNsContext.Destroy;
begin
  TypeFieldNames.Free;
  NamespaceNames.Free;
  MemberKeys.Free;
  GlobalNames.Free;
  GlobalTypeNames.Free;
  NsUsings.Free;
  inherited Destroy;
end;

function TNsContext.IsMember(const Prefix, Name: string): Boolean;
begin
  Result := MemberKeys.IndexOf(Prefix + '|' + Name) >= 0;
end;

function TNsContext.MemberIndex(const Prefix, Name: string): Integer;
// Which child of its namespace declared this member. -1 if it is not a member at all.
var
  i: Integer;
begin
  Result := -1;
  i := MemberKeys.IndexOf(Prefix + '|' + Name);
  if i >= 0 then Result := Integer(PtrInt(MemberKeys.Objects[i]));
end;

procedure TNsContext.AddUsingClosure(const Prefix: string; Acc: TStringList);
// Every namespace Prefix can SEE through its own USING directives, transitively. Acc doubles as the
// visited set, so a cycle ("namespace a : using b : end namespace" and the mirror) terminates on the
// first repeat instead of recursing for ever.
var
  i: Integer;
  Key, Imported: string;
begin
  if (Acc = nil) or (Prefix = '') then Exit;
  Key := UpperCase(Prefix) + '|';
  for i := 0 to NsUsings.Count - 1 do
    if Copy(NsUsings[i], 1, Length(Key)) = Key then
    begin
      Imported := Copy(NsUsings[i], Length(Key) + 1, MaxInt);
      if Acc.IndexOf(Imported) < 0 then
      begin
        Acc.Add(Imported);
        AddUsingClosure(Imported, Acc);
      end;
    end;
end;

// Forward declarations (mutual references in pass 1).
// Register the MEMBERS of an ENUM declared inside a namespace, and mangle their declared names, so an
// unqualified use inside the namespace resolves to them and one outside does not see them at all.
procedure CollectEnumMemberNames(EnumNode: TASTNode; const Prefix: string; Ctx: TNsContext);
var
  i: Integer;
  Item, NameNode: TASTNode;
  Nm: string;
begin
  for i := 0 to EnumNode.ChildCount - 1 do
  begin
    Item := EnumNode.GetChild(i);
    NameNode := nil;
    if (Item.NodeType = antAssignment) and (Item.ChildCount >= 1) and
       (Item.GetChild(0).NodeType = antIdentifier) then
      NameNode := Item.GetChild(0)
    else if Item.NodeType = antIdentifier then
      NameNode := Item;
    if NameNode = nil then Continue;
    Nm := UpperCase(VarToStr(NameNode.Value));
    if Nm <> '' then Ctx.MemberKeys.Add(Prefix + '|' + Nm);
  end;
end;

procedure CollectDimMembers(DimNode: TASTNode; const Prefix: string; Ctx: TNsContext); forward;
procedure CollectConstMembers(ConstNode: TASTNode; const Prefix: string; Ctx: TNsContext); forward;

// Replace child at Index with NewChild, freeing the detached old subtree (tokens are not owned by
// AST nodes, so a collapsed node may safely reuse the old base token).
procedure ReplaceChildAt(Parent: TASTNode; Index: Integer; NewChild: TASTNode);
var
  Old: TASTNode;
begin
  Old := TASTNode(Parent.Children[Index]);
  if Old = NewChild then Exit;
  Parent.Children.Extract(Old);
  Parent.Children.Insert(Index, NewChild);
  Old.Free;
end;

// Join an enclosing prefix with a (possibly already dotted) namespace name.
function CombinePrefix(const Outer, Name: string): string;
begin
  if Outer = '' then Result := UpperCase(Name)
  else Result := Outer + '.' + UpperCase(Name);
end;

// The declared name of a member declaration node (the base name to mangle/collect), or '' if the
// node is not a top-level declaration we namespace.
function MemberDeclName(Node: TASTNode): string;
begin
  Result := '';
  case Node.NodeType of
    antTypeDecl:
      Result := UpperCase(VarToStr(Node.Value));
    antProcedureDecl:
      // child0 = name identifier; a method "T.foo" (dotted) is not a free namespace member (v1).
      if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
      begin
        Result := UpperCase(VarToStr(Node.GetChild(0).Value));
        // ⛔ AN OVERLOADED PROCEDURE'S NAME CARRIES ITS SIGNATURE by the time this pass runs -
        // RegisterOverloadLabel rewrites it to "G~II" during parsing, and namespace flattening happens
        // afterwards. Keyed under the decorated name, the member was invisible: a call to "g()" from
        // INSIDE its own namespace resolved to the GLOBAL g instead, while "n.g()" from outside worked
        // and a VARIABLE of the same shape worked too. That difference is what named it.
        if Pos('~', Result) > 0 then Result := Copy(Result, 1, Pos('~', Result) - 1);
        if Pos('.', Result) > 0 then Result := '';
      end;
  end;
end;

// PASS 1 — collect namespace names and member keys (no mutation).
procedure CollectNamespaces(Node: TASTNode; const Prefix: string; Ctx: TNsContext);
var
  i, j, k: Integer;
  Child, Decl: TASTNode;
  ChildPrefix, MemName: string;
begin
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if Child.NodeType = antNamespace then
    begin
      ChildPrefix := CombinePrefix(Prefix, VarToStr(Child.Value));
      Ctx.NamespaceNames.Add(ChildPrefix);
      // ⛔ A DOTTED declaration - "Namespace a.b.c" - is the same thing as three nested blocks, and it
      // has to register the same NAMES: the reader walks "a.b.c.v" one dot at a time and needs to know
      // that "a" and "a.b" are namespaces too. Registering only the full path made a dotted namespace
      // silently unreadable - "a.b.v" answered 0 - while spelling the blocks out worked, and writing a
      // "Namespace a" anywhere else in the file made the dotted one start working. That difference is
      // what isolated it. fbc's own tests/namespace/ declares them this way throughout.
      for k := 2 to Length(ChildPrefix) do
        if ChildPrefix[k] = '.' then
          if Ctx.NamespaceNames.IndexOf(Copy(ChildPrefix, 1, k - 1)) < 0 then
            Ctx.NamespaceNames.Add(Copy(ChildPrefix, 1, k - 1));
      // Collect this namespace's direct member declarations.
      for j := 0 to Child.ChildCount - 1 do
      begin
        Decl := Child.GetChild(j);
        if Decl.NodeType = antDim then
          // DIM holds one antArrayDecl per declared variable.
          CollectDimMembers(Decl, ChildPrefix, Ctx)
        else if Decl.NodeType = antConst then
          CollectConstMembers(Decl, ChildPrefix, Ctx)
        else
        begin
          MemName := MemberDeclName(Decl);
          if MemName <> '' then Ctx.MemberKeys.AddObject(ChildPrefix + '|' + MemName, TObject(PtrInt(j)));
          // ⛔ AN ENUM'S MEMBERS ARE MEMBERS OF THE NAMESPACE TOO, and registering only the enum's own
          // NAME let them LEAK to module level: with an "E1.B = 2" outside and an "E1.B = 12" inside a
          // namespace, an unqualified B answered 12 - the namespace one - where fbc answers 2, and
          // "NS.B" answered nothing at all where fbc answers 12. The member names are mangled with the
          // rest, so both spellings land on the same declaration.
          if Decl.NodeType = antEnum then
            CollectEnumMemberNames(Decl, ChildPrefix, Ctx);
        end;
      end;
      // Recurse for physically nested namespaces.
      CollectNamespaces(Child, ChildPrefix, Ctx);
    end
    else
      CollectNamespaces(Child, Prefix, Ctx);
  end;
end;

procedure CollectDimMembers(DimNode: TASTNode; const Prefix: string; Ctx: TNsContext);
var
  i: Integer;
  Decl: TASTNode;
  Nm: string;
begin
  for i := 0 to DimNode.ChildCount - 1 do
  begin
    Decl := DimNode.GetChild(i);
    if (Decl.NodeType = antArrayDecl) and (Decl.ChildCount >= 1) and
       (Decl.GetChild(0).NodeType = antIdentifier) then
    begin
      Nm := UpperCase(VarToStr(Decl.GetChild(0).Value));
      if Nm <> '' then Ctx.MemberKeys.Add(Prefix + '|' + Nm);
      // FreeBASIC: a variable declared in a namespace is implicitly static/shared (visible in the
      // namespace's own procedures), so route it through the M6 DIM SHARED mechanism.
      Decl.Attributes.Values['SHARED'] := '1';
    end;
  end;
end;

procedure CollectConstMembers(ConstNode: TASTNode; const Prefix: string; Ctx: TNsContext);
var
  Assign, Target: TASTNode;
  Nm: string;
begin
  if (ConstNode.ChildCount >= 1) and (ConstNode.GetChild(0).NodeType = antAssignment) then
  begin
    Assign := ConstNode.GetChild(0);
    if (Assign.ChildCount >= 1) and (Assign.GetChild(0).NodeType = antIdentifier) then
    begin
      Target := Assign.GetChild(0);
      Nm := UpperCase(VarToStr(Target.Value));
      if Nm <> '' then Ctx.MemberKeys.Add(Prefix + '|' + Nm);
    end;
  end;
end;

// Collect names that shadow a namespace member within a procedure body (parameters + DIM'd locals),
// so an unqualified use of such a name is NOT prefixed.
//
// ⛔⛔ A LOCAL SHADOWS FROM ITS DECLARATION ONWARD, NOT FOR THE WHOLE BODY. This walked the entire
// procedure up front, so a "Dim x" written LATER suppressed the namespace prefix for every use BEFORE
// it: inside "Namespace N : Dim Shared x = 2 : Sub s() : Print x : Dim x = 3", fbc prints the member
// (2) and we printed the global (1). Measured 25 Aug 2026 - fbc's own namespace/global3 spends 37
// assertions on exactly this shape.
// ⇒ The DIM'd locals are therefore added AS THE WALK REACHES THEM (see RewriteRefs), and this routine
//   now collects only what is in scope for the WHOLE body: the parameters.
procedure CollectTypeFieldNames(Node: TASTNode; Ctx: TNsContext);
// Every TYPE's field names, at any depth, filed as "TYPE=F1,F2,...". See TNsContext.TypeFieldNames.
var
  i: Integer;
  Nm, Acc: string;
  Fld: TASTNode;
begin
  if (Node = nil) or (Ctx = nil) then Exit;
  if Node.NodeType = antTypeDecl then
  begin
    Nm := UpperCase(VarToStr(Node.Value));
    if Nm <> '' then
    begin
      Acc := Ctx.TypeFieldNames.Values[Nm];
      for i := 0 to Node.ChildCount - 1 do
      begin
        Fld := Node.GetChild(i);
        if (Fld <> nil) and (Fld.NodeType in [antIdentifier, antArrayDecl]) then
        begin
          if (Fld.NodeType = antArrayDecl) and (Fld.ChildCount >= 1) and
             (Fld.GetChild(0).NodeType = antIdentifier) then
            Acc := Acc + ',' + UpperCase(VarToStr(Fld.GetChild(0).Value))
          else if Fld.NodeType = antIdentifier then
            Acc := Acc + ',' + UpperCase(VarToStr(Fld.Value));
        end;
      end;
      Ctx.TypeFieldNames.Values[Nm] := Acc;
    end;
  end;
  for i := 0 to Node.ChildCount - 1 do CollectTypeFieldNames(Node.GetChild(i), Ctx);
end;

procedure CollectOwnerFieldNames(Node: TASTNode; Ctx: TNsContext; Shadow: TStringList);
// A member procedure "T.m" is written inside T's scope: T's FIELDS shadow namespace members there,
// because a bare one of them means "this.<field>". Added beside the parameters, for the same reason.
var
  Nm, Owner, Acc, One: string;
  P: Integer;
begin
  if (Node = nil) or (Ctx = nil) or (Shadow = nil) then Exit;
  // ⛔ The NAME is child 0, not the node's own Value - and by now it may carry an overload signature
  // ("~II") or a constructor's parameter tail ("#I:TV"), both written during parsing. Cut them off:
  // the owner is what stands before the LAST dot of the bare name.
  if (Node.ChildCount < 1) or (Node.GetChild(0).NodeType <> antIdentifier) then Exit;
  Nm := UpperCase(VarToStr(Node.GetChild(0).Value));
  P := Pos('#', Nm); if P > 0 then Nm := Copy(Nm, 1, P - 1);
  P := Pos('~', Nm); if P > 0 then Nm := Copy(Nm, 1, P - 1);
  P := LastDelimiter('.', Nm);
  if P <= 1 then Exit;
  Owner := Copy(Nm, 1, P - 1);
  Acc := Ctx.TypeFieldNames.Values[Owner];
  while Acc <> '' do
  begin
    P := Pos(',', Acc);
    if P = 0 then begin One := Acc; Acc := ''; end
    else begin One := Copy(Acc, 1, P - 1); Acc := Copy(Acc, P + 1, MaxInt); end;
    if (One <> '') and (Shadow.IndexOf(One) < 0) then Shadow.Add(One);
  end;
end;

procedure CollectParamNames(Node: TASTNode; Shadow: TStringList);
var
  i: Integer;
  Child: TASTNode;
begin
  if Node.NodeType = antParameterList then
  begin
    for i := 0 to Node.ChildCount - 1 do
      if Node.GetChild(i).NodeType = antIdentifier then
        Shadow.Add(UpperCase(VarToStr(Node.GetChild(i).Value)));
    Exit;
  end;
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    if (Child.NodeType <> antNamespace) and (Child.NodeType <> antProcedureDecl) then
      CollectParamNames(Child, Shadow);
  end;
end;

// The names a DIM/declaration node binds locally, appended to Shadow. Called by the walk at the moment
// the declaration is REACHED, so it shadows from there on and not before.
procedure NoteDeclared(Shadow: TStringList; const N: string; AtIndex: Integer);
// Record a declared name with the position it was declared at. ⛔ THE FIRST DECLARATION WINS, and it
// is written out rather than left to the list: a SORTED TStringList with dupIgnore does NOT keep the
// first entry's Object on a duplicate AddObject, so the LAST position silently won - which put a
// module DIM's name at a later procedure's index and made it invisible where it should be seen.
begin
  if Shadow.IndexOf(N) < 0 then Shadow.AddObject(N, TObject(PtrInt(AtIndex)));
end;

procedure AddDeclaredNames(Node: TASTNode; Shadow: TStringList; AtIndex: Integer = 0);
var
  i: Integer;
begin
  if Node = nil then Exit;
  // ⛔ A REDIM IS NOT A DECLARATION. Its target is an antArrayDecl exactly like a DIM's, so a
  // module-level "ReDim arr1(0 To 5)" put ARR1 in the list of names the program declares itself - and
  // that list is what BEATS a "Using" import. The reference then resolved to a fresh module array
  // while "nx.arr1" named the imported one: two arrays under one name, and the qualified spelling
  // answered -1 where the unqualified one answered 5. ⭐ The FIXED-size case never showed it, because
  // nothing REDIMs one.
  if Node.NodeType = antRedim then Exit;
  if (Node.NodeType = antArrayDecl) and (Node.ChildCount >= 1) and
     (Node.GetChild(0).NodeType = antIdentifier) then
    NoteDeclared(Shadow, UpperCase(VarToStr(Node.GetChild(0).Value)), AtIndex);
  // ⛔⛔ ...AND A MODULE-LEVEL SUB/FUNCTION IS A DECLARED NAME TOO, which this list did not know: it
  // collected DIMs only. So "Function bar" of the program's own beside a "Using ns1" that also has a
  // bar meant the IMPORTED one - "print bar" answered 1 where fbc answers 2 - while the same program
  // written with "Dim Shared bar" resolved correctly, which is the pair that names it. The rule was in
  // one path and not its sibling ([[a-rule-one-path-has-and-the-other-does-not]]).
  // ⚠️ fbc goes further and REFUSES the program when the declaration comes AFTER the Using ("error 4:
  // Duplicated definition"), so nothing it accepts is harmed by our list having no position: it only
  // ever decides in favour of the program's own name, which is what fbc does whenever it compiles at
  // all. DIVERGENZE 89. fbc suite namespace/global2.
  // ⛔ A dotted name is a METHOD body ("Sub UDT.proc"), not a module-level name of its own.
  // ⛔⛔ ...AND A PROCEDURE'S BODY IS NOT THE MODULE'S. The descent below follows every antDim it
  // meets, and a procedure node's children ARE its body - so "Sub p() : Dim As Integer foo" put FOO in
  // the list of names the MODULE declares. As a SET that was merely too generous; with a POSITION it
  // is a wrong answer, because the position recorded is the procedure's and not the module DIM's.
  // A procedure contributes its own NAME and nothing from inside it.
  if (Node.NodeType = antProcedureDecl) then
  begin
    if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) and
       (Pos('.', VarToStr(Node.GetChild(0).Value)) = 0) then
      NoteDeclared(Shadow, UpperCase(VarToStr(Node.GetChild(0).Value)), AtIndex);
    Exit;
  end;
  for i := 0 to Node.ChildCount - 1 do
    if Node.GetChild(i).NodeType in [antArrayDecl, antDim] then
      AddDeclaredNames(Node.GetChild(i), Shadow, AtIndex);
end;

function GlobalNameVisibleAt(Ctx: TNsContext; const V: string): Boolean;
// ⭐ fbc's screen on an imported name is POSITIONAL, and this is where the position is spent. A name
// the program declares at MODULE level beats an import - but only from the point it is DECLARED:
//   "function bar : 2 : end function        : sub p() : using ns1 : print bar"   -> 2, the program's
//   "sub p() : using ns1 : print bar : ...  : function bar : 2 : end function"   -> 1, the import's
// Both compile under fbc and they answer differently, so a SET of names cannot express it: the entry
// carries the top-level index it was declared at, and CurTopIndex says where we are now.
// ⚠️ MaxInt means "position unknown" (a walk that is not the module's own children), and there every
// declared name is treated as visible - which is the answer this test gave before it had a position.
var
  Idx: Integer;
begin
  Result := False;
  if Ctx = nil then Exit;
  Idx := Ctx.GlobalNames.IndexOf(V);
  if Idx < 0 then Exit;
  Result := Integer(PtrInt(Ctx.GlobalNames.Objects[Idx])) <= Ctx.CurTopIndex;
end;

// Resolve an unqualified member name V against the active prefix chain (innermost first). Returns
// the mangled "PREFIX.V" if V is a member of some enclosing namespace, else ''.
function ResolveUnqualified(const ActivePrefix, V: string; Ctx: TNsContext;
                            Using: TStringList; TypeSlot: Boolean = False): string;
// ...and then against the namespaces a USING has brought into scope. Tried AFTER the enclosing chain,
// so a name of the namespace one is written INSIDE always wins over an imported one - which is what
// fbc does and the only order that keeps an existing program's meaning.
var
  P, Member: string;
  DotPos, u: Integer;
  Closure: TStringList;
  MemberIsLater: Boolean;
begin
  Result := '';
  P := ActivePrefix;
  Member := '';
  MemberIsLater := False;
  while P <> '' do
  begin
    if Ctx.IsMember(P, V) then
    begin
      Member := P + '.' + V;
      // ⭐ ...BUT ONLY IF IT IS DECLARED BY NOW. fbc's screen is POSITIONAL: a member of the namespace
      // we are written inside, declared BELOW this point, does not shadow what a "Using" brought in.
      //   namespace ns2 : sub p() : using ns1 : print bar : end sub : function bar : 2 : ...
      // answers ns1's bar, not ns2's - fbc's own namespace/import_method asserts exactly that.
      // ⛔ The screen is a TIE-BREAK against an import and NOTHING ELSE: with no import offering the
      // name, a later member still resolves, or every forward reference inside a namespace would
      // break. That is why Member is remembered here and returned below rather than being skipped.
      MemberIsLater := (P = Ctx.CurNsPrefix) and (Ctx.MemberIndex(P, V) > Ctx.CurNsIndex);
      Break;
    end;
    DotPos := LastDelimiter('.', P);
    if DotPos = 0 then Break;
    P := Copy(P, 1, DotPos - 1);
  end;
  if (Member <> '') and (not MemberIsLater) then Exit(Member);
  // ⛔ ...but a MODULE-LEVEL name of the program's own wins over every import: fbc resolves an
  // unqualified reference against the global scope first. "Dim Shared v" beside a "Using A" that also
  // has a v means the global v, and without this test the import silently took the name over.
  // ⭐ ...AND THROUGH WHAT THOSE NAMESPACES THEMSELVES IMPORT. A "Using N" brings in what N can SEE,
  // not only what N declares - fbc's namespace/using2 nests three deep and reads a name of the
  // innermost through the outermost. The closure is built here rather than when the directive is seen,
  // because a namespace may be REOPENED after the import and gain more imports later.
  // ⛔ ...AND A TYPE SLOT ASKS A DIFFERENT SET. The guard just above is the right authority for a
  // VARIABLE reference and the wrong one for the type of a declaration: BASIC is case-insensitive, so
  // "Dim p As P" names the variable and the type alike, and the variable's own name in GlobalNames
  // blocked the import for the TYPE beside it - the record was then built from a type that exists
  // nowhere and every field read 0, while renaming the variable made the identical program work
  // (fbc suite namespace/var-named-as-udt, which writes that pair on purpose). Only a module-level
  // TYPE can outrank an imported one.
  if (Using <> nil) and
     (((not TypeSlot) and (not GlobalNameVisibleAt(Ctx, V))) or
      (TypeSlot and (Ctx.GlobalTypeNames.IndexOf(V) < 0))) then
  begin
    Closure := TStringList.Create;
    try
      Closure.Assign(Using);
      for u := 0 to Using.Count - 1 do Ctx.AddUsingClosure(Using[u], Closure);
      for u := 0 to Closure.Count - 1 do
        if Ctx.IsMember(Closure[u], V) then Exit(Closure[u] + '.' + V);
    finally
      Closure.Free;
    end;
  end;
  // No import offered the name: a member of an enclosing namespace stands, wherever it was declared.
  if Member <> '' then Exit(Member);
end;

// Re-resolve the TYPE NAMES carried in an overload signature's tail against the enclosing namespace
// and the USING imports. The tail is everything after the first ':' of a "~<banks><widths>:<names>"
// decoration; the names are COMMA-SEPARATED and POSITIONAL, with '-' where a parameter contributes no
// type name, so splitting on the commas and rebuilding is exact.
//
// ⛔ WHY IT HAS TO HAPPEN HERE. ProcSigFromParams writes that tail while the file is being PARSED,
// long before a namespace exists, so it holds the source's LITERAL spelling. This pass is where the
// active prefix and the imports are known, and it is the last place the name is still a string that
// anyone rewrites. ⭐ ResolveUnqualified answers '' for a name it does not own, so a builtin, a
// pointer spelling, a placeholder and a type declared OUTSIDE the namespace are all left untouched -
// which is what keeps a program whose overloads take no UDT byte-identical.
function ResolveSigTypeNames(const Sig, ActivePrefix: string; Ctx: TNsContext;
                             Using: TStringList): string;
var
  ColonPos, i: Integer;
  Head, Tail, Part, Res, Acc: string;
  Parts: TStringList;
begin
  Result := Sig;
  ColonPos := Pos(':', Sig);
  if ColonPos = 0 then Exit;
  Head := Copy(Sig, 1, ColonPos);
  Tail := Copy(Sig, ColonPos + 1, MaxInt);
  if Tail = '' then Exit;
  Parts := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := ',';
    Parts.DelimitedText := Tail;
    Acc := '';
    for i := 0 to Parts.Count - 1 do
    begin
      Part := UpperCase(Trim(Parts[i]));
      // '-' is the placeholder for a parameter with no type name; a dotted name is already qualified;
      // a pointer spelling ("T PTR") is left whole, since the tail records it as one token and the
      // call side spells it the same way.
      // ⚠️ MISURATO IL 31 AGO E NON E' QUI. Un tipo ANNIDATO dentro un namespace ("foo.bar1", un Enum
      // dentro Type foo) non e' qualificato malgrado il punto, e qualificarne la TESTA qui e' un ramo
      // MORTO: la suite non si muove di un test. La riduzione dice dov'e' il confine, e sono tre
      // sonde: enum annidati FUORI da un namespace ✅, enum NON annidati DENTRO ✅, annidati + dentro ❌
      // (fbc structs/enum_decl: il secondo overload non viene mai chiamato, la scelta cade sull'ARITA').
      // ⇒ Il disaccordo sta fra la firma e cio' che il SITO DI CHIAMATA chiede del tipo dichiarato
      // dell'argomento, non nella firma da sola. Prossima mossa: stampare le due stringhe.
      if (Part <> '') and (Part <> '-') and (Pos('.', Part) = 0) and (Pos(' ', Part) = 0) then
      begin
        Res := ResolveUnqualified(ActivePrefix, Part, Ctx, Using);
        if Res <> '' then Part := Res;
      end;
      if Acc <> '' then Acc := Acc + ',';
      Acc := Acc + Part;
    end;
    Result := Head + Acc;
  finally
    Parts.Free;
  end;
end;

function FlattenDottedName(Node: TASTNode): string;
// Render a member-access chain "A.B.C" as the dotted string a namespace prefix is spelled with, or ''
// when the chain is not made of plain names. Used to read "Using Outer.Inner".
begin
  Result := '';
  if Node = nil then Exit;
  if Node.NodeType = antIdentifier then Exit(VarToStr(Node.Value));
  if (Node.NodeType <> antMemberAccess) or (Node.ChildCount < 1) then Exit;
  Result := FlattenDottedName(Node.GetChild(0));
  if Result = '' then Exit;
  Result := Result + '.' + VarToStr(Node.Value);
end;

function ResolveNamespacePrefix(const ActivePrefix, Base: string; Ctx: TNsContext;
                                Using: TStringList): string;
// The full name of a namespace referred to by the PARTIAL name Base - through the enclosing chain
// first, then through what a USING has imported. '' when Base does not name one.
var
  P: string;
  DotPos, u: Integer;
begin
  Result := '';
  P := ActivePrefix;
  while P <> '' do
  begin
    if Ctx.NamespaceNames.IndexOf(P + '.' + Base) >= 0 then Exit(P + '.' + Base);
    DotPos := LastDelimiter('.', P);
    if DotPos = 0 then Break;
    P := Copy(P, 1, DotPos - 1);
  end;
  if Using <> nil then
    for u := 0 to Using.Count - 1 do
      if Ctx.NamespaceNames.IndexOf(Using[u] + '.' + Base) >= 0 then Exit(Using[u] + '.' + Base);
end;

function UsingDirectiveName(Node: TASTNode; Ctx: TNsContext; const ActivePrefix: string): string;
// The namespace a "Using N" statement names, or '' when this is not one.
// ⛔ "USING" IS NOT PARSED AS A DIRECTIVE AT ALL: at statement level it is routed to PRINT USING (the
// Commodore format clause), so it arrives here as antPrintUsing with the namespace name as its only
// child. That is enough to tell the two apart without touching the parser - a real PRINT USING carries
// a format STRING, and this one carries a name that IS a declared namespace. If no namespace of that
// name exists the node is left exactly as it was.
var
  Nm, P: string;
  DotPos: Integer;
begin
  Result := '';
  if (Node = nil) or (Node.NodeType <> antPrintUsing) or (Node.ChildCount <> 1) then Exit;
  if Node.GetChild(0).NodeType = antIdentifier then
    Nm := UpperCase(VarToStr(Node.GetChild(0).Value))
  else if Node.GetChild(0).NodeType = antMemberAccess then
    Nm := UpperCase(FlattenDottedName(Node.GetChild(0)))
  else
    Exit;
  if Nm = '' then Exit;
  if Ctx.NamespaceNames.IndexOf(Nm) >= 0 then Exit(Nm);
  // ⭐ ...and the name may be RELATIVE to where the directive stands: "Namespace reimp1.bar : Using foo"
  // means reimp1.foo, exactly as an unqualified reference would. Walk the enclosing chain outwards.
  P := ActivePrefix;
  while P <> '' do
  begin
    if Ctx.NamespaceNames.IndexOf(P + '.' + Nm) >= 0 then Exit(P + '.' + Nm);
    DotPos := LastDelimiter('.', P);
    if DotPos = 0 then Break;
    P := Copy(P, 1, DotPos - 1);
  end;
end;

// PASS 2 — rewrite references (and member declaration names) bottom-up. Returns the node to use in
// place of Node (Node itself, unless it is a collapsed member-access replaced by a new identifier).
// ActivePrefix = current namespace ('' at module level). Shadow = names bound as params/locals of
// the enclosing procedure (not to be prefixed). The caller owns freeing a replaced node.
function RewriteRefs(Node: TASTNode; const ActivePrefix: string;
                     Shadow: TStringList; Ctx: TNsContext; Using: TStringList): TASTNode;
var
  i, k, m: Integer;
  ChildPrefix, BaseName, Mangled, V, Qual: string;
  NewNode, BaseId, FieldNd, DeclNd: TASTNode;
  UseShadow, UseUsing: TStringList;
  Drop: array of Integer;
  UsingNs: string;
  SigPos, DotPos: Integer;
  BaseV, SigV, SavedNsPrefix: string;
  SavedNsIndex: Integer;
  AliasV, AliasSig, AliasQ: string;
begin
  Result := Node;

  // Determine the prefix/shadow for descending into children.
  ChildPrefix := ActivePrefix;
  UseShadow := Shadow;

  // ⛔⛔ A TYPE ALIAS'S TARGET LIVES IN AN ATTRIBUTE, AND THIS WALK ONLY EVER SAW CHILD NODES.
  // "Type A As A_" inside a namespace kept the bare "A_", so the alias pointed at a type that exists
  // nowhere and every use of A fell back to the default width: SizeOf(A) answered 8 where fbc answers
  // 24, and a field read through it answered the handle. The identical program outside a namespace was
  // right - the tell for a rule one path has and its sibling does not. It is a TYPE SLOT, so it asks
  // with TypeSlot=True, and a FORWARD target resolves too: members are collected in a pre-pass, so a
  // type declared further down is a member already. fbc's typedef/incomplete asserts the qualified
  // spelling by name.
  if (Node <> nil) and (Node.NodeType = antTypeDecl) and (Ctx <> nil) and
     (Node.Attributes.Values['ALIAS'] <> '') then
  begin
    AliasV := UpperCase(Node.Attributes.Values['ALIAS']);
    AliasSig := '';
    while (Length(AliasV) > 4) and (Copy(AliasV, Length(AliasV) - 3, 4) = ' PTR') do
    begin
      AliasSig := ' PTR' + AliasSig;
      AliasV := TrimRight(Copy(AliasV, 1, Length(AliasV) - 4));
    end;
    if (AliasV <> '') and (Pos('.', AliasV) = 0) then
    begin
      AliasQ := ResolveUnqualified(ActivePrefix, AliasV, Ctx, Using, True);
      if AliasQ <> '' then Node.Attributes.Values['ALIAS'] := AliasQ + AliasSig;
    end;
  end;

  if Node.NodeType = antNamespace then
    ChildPrefix := CombinePrefix(ActivePrefix, VarToStr(Node.Value))
  else if Node.NodeType = antProcedureDecl then
  begin
    // A procedure introduces its own parameter/local scope: those names shadow namespace members.
    UseShadow := TStringList.Create;
    UseShadow.Duplicates := dupIgnore;
    UseShadow.Sorted := True;
    CollectParamNames(Node, UseShadow);
    CollectOwnerFieldNames(Node, Ctx, UseShadow);
  end;

  // Recurse into children first (bottom-up), replacing each in place if needed.
  // ⭐ A "Using N" seen among the children brings N into scope FOR THE CHILDREN THAT FOLLOW IT, and only
  // within this node - which is exactly FreeBASIC's rule and needs no new machinery: the imported prefix
  // joins the chain ResolveUnqualified already walks. The directive itself is then DROPPED, so nothing
  // downstream ever sees a stray PRINT USING that would take the next PRINT's format with it.
  UseUsing := Using;
  SetLength(Drop, 0);
  SavedNsPrefix := '';
  SavedNsIndex := MaxInt;
  if Ctx <> nil then
  begin
    SavedNsPrefix := Ctx.CurNsPrefix;
    SavedNsIndex := Ctx.CurNsIndex;
  end;
  for i := 0 to Node.ChildCount - 1 do
  begin
    // Where we are, in module-level statements: what a "Using" is allowed to take over depends on it.
    if (Node.NodeType = antProgram) and (Ctx <> nil) then Ctx.CurTopIndex := i;
    // ...and the same question one level in: which child of THIS namespace we are rewriting.
    if (Node.NodeType = antNamespace) and (Ctx <> nil) then
    begin
      Ctx.CurNsPrefix := ChildPrefix;
      Ctx.CurNsIndex := i;
    end;
    UsingNs := UsingDirectiveName(Node.GetChild(i), Ctx, ChildPrefix);
    if UsingNs <> '' then
    begin
      if UseUsing = Using then
      begin
        UseUsing := TStringList.Create;
        if Using <> nil then UseUsing.Assign(Using);
      end;
      if UseUsing.IndexOf(UsingNs) < 0 then UseUsing.Add(UsingNs);
      // ...and remember that THIS namespace imports it, so importing THIS one later brings it along.
      if (Ctx <> nil) and (ChildPrefix <> '') then
        Ctx.NsUsings.Add(UpperCase(ChildPrefix) + '|' + UsingNs);
      SetLength(Drop, Length(Drop) + 1);
      Drop[High(Drop)] := i;
      Continue;
    end;
    // A DIM shadows the namespace member FROM HERE ON: add its names before descending, so its own
    // declared name is not prefixed and every later sibling sees the local. Cloned per node, so a
    // Scope block's locals do not leak out of it.
    // ⛔ ONLY INSIDE A PROCEDURE. At namespace level a DIM is a MEMBER declaration and must be MANGLED,
    // not shadowed - treating it as a local made the member's own name skip the prefix, so "N.x" never
    // existed and every reference to it read as undeclared. UseShadow is non-nil exactly from the
    // procedure node down, which is the same marker the parameter rule already uses.
    if (UseShadow <> nil) and (Node.GetChild(i).NodeType = antDim) then
    begin
      if UseShadow = Shadow then
      begin
        UseShadow := TStringList.Create;
        UseShadow.Duplicates := dupIgnore;
        UseShadow.Sorted := True;
        if Shadow <> nil then UseShadow.Assign(Shadow);
      end;
      AddDeclaredNames(Node.GetChild(i), UseShadow);
    end;
    // ⛔ A FIELD'S NAME IS NOT A REFERENCE TO ANYTHING. This walk is bottom-up, so a TYPE's field nodes
    // were rewritten before the antTypeDecl guard further down could Exit - and a namespace that
    // declares both "Dim Shared As Long i1" and a TYPE with a field "i1" had the FIELD renamed to
    // "NS.I1". The type then had no field called I1 at all: "@UDT.x.i1" answered "unknown field", and
    // without the module-level name of the same spelling the very same program worked. Its TYPE child
    // still has to be resolved (a field declared "As UDT" inside a namespace means "NS.UDT"), so the
    // field node is stepped OVER, not skipped.
    // ⭐ A DECLARATION'S TYPE SLOT IS RESOLVED AS A TYPE, not as an ordinary identifier. The slot is
    // child 1 of "Dim v As T" and child 2 of "Dim v(dims) As T" - the same two shapes the SSA tests -
    // and it is asked with TypeSlot=True, which changes WHICH module-level set can outrank an import
    // (see ResolveUnqualified). Everything else in the declaration keeps the ordinary rewrite.
    if Node.NodeType = antArrayDecl then
    begin
      m := -1;
      if (Node.ChildCount >= 2) and (Node.GetChild(1).NodeType = antIdentifier) then m := 1
      else if (Node.ChildCount >= 3) and (Node.GetChild(1).NodeType = antDimensions) and
              (Node.GetChild(2).NodeType = antIdentifier) then m := 2;
      // ⛔⛔ AND NOT ONLY WHEN THERE IS AN IMPORT. This was gated on "UseUsing <> nil", so a namespace
      // with no "Using" at all never asked the question - and then the generic rewrite below decided,
      // which asks the VARIABLE authority: "Dim As Integer foo" beside "Type foo" in the same
      // namespace left the type slot unqualified, the record was built from a type that exists
      // nowhere, and every field read 0. A declaration's type slot is a TYPE SLOT whether or not
      // anything was imported; ResolveUnqualified already handles a nil import list.
      if i = m then
      begin
        DeclNd := Node.GetChild(i);
        V := UpperCase(VarToStr(DeclNd.Value));
        SigV := '';
        while (Length(V) > 4) and (Copy(V, Length(V) - 3, 4) = ' PTR') do
        begin
          SigV := ' PTR' + SigV;
          V := TrimRight(Copy(V, 1, Length(V) - 4));
        end;
        if (V <> '') and (Pos('.', V) = 0) then
        begin
          Qual := ResolveUnqualified(ChildPrefix, V, Ctx, UseUsing, True);
          if Qual <> '' then
          begin
            DeclNd.Value := Qual + SigV;
            Continue;                       // resolved as a TYPE; the generic rewrite must not re-do it
          end;
        end
        // ⛔⛔ ...AND A PARTIALLY QUALIFIED SLOT IS STILL A TYPE SLOT. The test above is "no dot at
        // all", so "Dim As ns_a.shape v" written from the ENCLOSING namespace was handed to the generic
        // rewrite - which asks the VARIABLE authority - and came out as "NS_A.SHAPE", a type that
        // exists nowhere: the real one is "Q.NS_A.SHAPE". The variable then got no record at all, so
        // "v.x = 99 : Print v.x" answered 0 and passing it to a SUB that reads a field was an
        // ACCESS VIOLATION. ⭐ The very same declaration written OUT IN FULL worked, which is what said
        // it was the spelling and not the nesting (fbc's own namespace/dups_qkwd, reduced to fifteen
        // lines). [[two-spellings-of-one-thing-that-disagree-name-the-missing-path]]
        //
        // The HEAD is what needs resolving, and ResolveNamespacePrefix is the funnel that already does
        // it for every other partially qualified reference: enclosing chain first, then the imports.
        // ⚠️ A nested TYPE ("T.U") is a dotted spelling too, and it must NOT be touched - the head is
        // not a namespace, so that helper answers '' and this arm declines, which is the closed side.
        else if (V <> '') and (Pos('.', V) > 0) then
        begin
          Qual := ResolveNamespacePrefix(ChildPrefix, Copy(V, 1, Pos('.', V) - 1), Ctx, UseUsing);
          if Qual <> '' then
          begin
            DeclNd.Value := Qual + Copy(V, Pos('.', V), MaxInt) + SigV;
            Continue;
          end;
        end;
      end;
    end;
    // ⛔ ...AND A FIELD IS NOT ALWAYS AN antIdentifier. A member declared with the DIM shape arrives as
    // an antArrayDecl, and this guard did not cover it: a namespace declaring "Type foo" with a field
    // "bar" AND a "Type bar" beside it had foo's FIELD renamed to "NS.BAR", so foo had no field called
    // BAR at all and "f.bar" read 0 - while "g.foo" on the mirror pair worked, which is what said the
    // guard was keyed on the node's SHAPE and not on its meaning. fbc's dim/auto_var2 writes that
    // cross-shadowed pair on purpose. Its children are still rewritten: a field declared "As UDT"
    // inside a namespace means "NS.UDT".
    if (Node.NodeType = antTypeDecl) and
       (Node.GetChild(i).NodeType in [antIdentifier, antArrayDecl]) then
    begin
      FieldNd := Node.GetChild(i);
      for k := 0 to FieldNd.ChildCount - 1 do
      begin
        NewNode := RewriteRefs(FieldNd.GetChild(k), ChildPrefix, UseShadow, Ctx, UseUsing);
        if NewNode <> FieldNd.GetChild(k) then
          ReplaceChildAt(FieldNd, k, NewNode);
      end;
      Continue;
    end;
    NewNode := RewriteRefs(Node.GetChild(i), ChildPrefix, UseShadow, Ctx, UseUsing);
    if NewNode <> Node.GetChild(i) then
      ReplaceChildAt(Node, i, NewNode);   // frees the old child, installs the rewritten one
  end;
  // The descent may have moved the "where we are" marker into a nested namespace; this node's own
  // rewriting below belongs to the scope it started in.
  if Ctx <> nil then
  begin
    Ctx.CurNsPrefix := SavedNsPrefix;
    Ctx.CurNsIndex := SavedNsIndex;
  end;
  for i := High(Drop) downto 0 do
    Node.RemoveChildAt(Drop[i]);          // the directive has done its work; it is not a statement

  if UseShadow <> Shadow then
    UseShadow.Free;
  if UseUsing <> Using then
    UseUsing.Free;

  // ⛔ A METHOD IS DECLARED AS "T.foo", AND THE TYPE IT BELONGS TO IS MANGLED right below. The type
  // became "N1.T" while its constructor kept the name "T.CONSTRUCTOR#", so the two stopped naming the
  // same thing and EVERY member procedure of a type declared inside a Namespace was silently never
  // called - no diagnostic, because the type itself resolves and its field DEFAULTS still appear,
  // which is exactly what made it read as "the type works". MemberDeclName declines a dotted name (it
  // is not a free member of the namespace) and the identifier branch below declines a dotted name too
  // (a dot means "already qualified"): the rule lived in the TYPE path and in neither of the two paths
  // that carry the METHOD - the same shape as [[a-rule-one-path-has-and-the-other-does-not]].
  // The owner half is mangled with the very rule the type gets, so the two agree by construction.
  if (Node.NodeType = antProcedureDecl) and (ActivePrefix <> '') and
     (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) and
     (Node.Attributes.Values['GLOBALSCOPE'] <> '1') then
  begin
    V := UpperCase(VarToStr(Node.GetChild(0).Value));
    // ⛔⛔ ...AND A CONSTRUCTOR'S TYPE TAIL IS NOT DEAD TEXT EITHER. A method's label carries its
    // parameter signature after a '#' ("TU.CONSTRUCTOR#I:TV"), written by ProcSigFromParams while the
    // file is being PARSED - so it holds the source's LITERAL spelling. The '~' half of exactly this
    // rule has been resolved through ResolveSigTypeNames since the overload work; the '#' half was
    // not, so a constructor declared inside a namespace kept "#I:TV" while the CALL side, built after
    // flattening, asked for "#I:NS1.TV". The two named different things and ResolveConstructorLabel
    // found nothing: "Sub s( ByVal u As TU )" called with a TV ran no constructor at all INSIDE a
    // namespace while the identical program outside one ran it. fbc's structs/udt-ops-1..3 and
    // udt-init-ops-* declare every one of their types inside one.
    SigPos := Pos('#', V);
    if SigPos > 0 then
    begin
      SigV := ResolveSigTypeNames(Copy(V, SigPos, MaxInt), ActivePrefix, Ctx, Using);
      V := Copy(V, 1, SigPos - 1);
      Node.GetChild(0).Value := V + SigV;
    end
    else
      SigV := '';
    DotPos := Pos('.', V);
    if DotPos > 1 then
    begin
      BaseV := Copy(V, 1, DotPos - 1);
      // ⛔ ...OR A NESTED NAMESPACE OF IT. The guard asks "is PROCB a MEMBER of PROCA?", and
      // CollectNamespaces records a nested namespace in NamespaceNames and NEVER in MemberKeys - so
      // "Private Function procB.f2()" written inside "Namespace procA" kept the bare name procB.f2,
      // the declaration landed nowhere, and "procA.procB.f2()" was refused as an undeclared array.
      // ⚠️ And it was worse than a refusal one spelling over: "print procB.f2()" from inside procA
      // printed 0 where fbc refuses the program outright. The rule was written for "T.method" - where
      // T is a TYPE, and types ARE member keys - and a NAMESPACE is the other thing a dotted head can
      // name here.
      if Ctx.IsMember(ActivePrefix, BaseV) or
         (Ctx.NamespaceNames.IndexOf(ActivePrefix + '.' + BaseV) >= 0) then
        Node.GetChild(0).Value := ActivePrefix + '.' + V + SigV;
    end;
  end;

  // ⛔⛔ ...AND THE BASE MAY BE AN IMPORTED NAME, WITH NO NAMESPACE OF OUR OWN AROUND US. The block
  // below is guarded on being INSIDE a namespace, so a type declared at module level (or in a Scope)
  // that extends a name brought in by "Using N" was never looked at: the SSA searched for the parent
  // by its bare name, found nothing, and the derived type inherited NOTHING with no diagnostic -
  // "Type T2 Extends T1" after "Using N" gave T2 its own fields only, and the initialiser then said
  // "too many expressions". The QUALIFIED spelling "Extends N.T1" worked, which is the pair that named
  // it. fbc's own structs/scope-type-1 writes the imported form.
  // Asked with TypeSlot=True, because a base IS a type slot: a module-level VARIABLE of the same
  // spelling must not block the import (see ResolveUnqualified).
  if (Node.NodeType = antTypeDecl) and (ActivePrefix = '') and (UseUsing <> nil) then
  begin
    BaseV := UpperCase(Node.Attributes.Values['EXTENDS']);
    if (BaseV <> '') and (Pos('.', BaseV) = 0) then
    begin
      Qual := ResolveUnqualified(ActivePrefix, BaseV, Ctx, UseUsing, True);
      if Qual <> '' then Node.Attributes.Values['EXTENDS'] := Qual;
    end;
  end;

  // antTypeDecl name lives in Value (not a child identifier): mangle it here.
  if (Node.NodeType = antTypeDecl) and (ActivePrefix <> '') then
  begin
    V := UpperCase(VarToStr(Node.Value));
    // ⛔ ...unless the program asked for the GLOBAL one with a leading '.': that is precisely a request
    // NOT to be resolved against the enclosing namespace.
    if (Pos('.', V) = 0) and (Node.Attributes.Values['GLOBALSCOPE'] <> '1') and
       Ctx.IsMember(ActivePrefix, V) then
      Node.Value := ActivePrefix + '.' + V;
    // ⛔ ...AND SO DOES THE NAME OF ITS BASE. The declaration's own name was mangled here and the
    // EXTENDS attribute beside it was not - the word did not appear ONCE in this unit - so the SSA
    // looked the parent up by its bare name, found nothing, and the derived type inherited NOTHING
    // with no diagnostic at all: sizeof gave its own fields only and the base's fields read rubbish.
    // Same rule, same node, one of the two halves written.
    BaseV := UpperCase(Node.Attributes.Values['EXTENDS']);
    if (BaseV <> '') and (Pos('.', BaseV) = 0) then
    begin
      if Ctx.IsMember(ActivePrefix, BaseV) then
        Node.Attributes.Values['EXTENDS'] := ActivePrefix + '.' + BaseV
      else if UseUsing <> nil then
      begin
        // ...and a base the enclosing namespace does NOT declare may still be an IMPORTED one.
        Qual := ResolveUnqualified(ActivePrefix, BaseV, Ctx, UseUsing, True);
        if Qual <> '' then Node.Attributes.Values['EXTENDS'] := Qual;
      end;
    end;
    Exit;
  end;

  // Collapse a namespace-qualified access "NS.member" into a single mangled identifier.
  if Node.NodeType = antMemberAccess then
  begin
    if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
    begin
      BaseId := Node.GetChild(0);
      BaseName := UpperCase(VarToStr(BaseId.Value));
      // ⭐ A PARTIALLY QUALIFIED CHAIN. After "Using nested.multi.lev1", the reference
      // "lev2.lev3.value" names nested.multi.lev1.lev2.lev3.value - the base is a namespace only once
      // the import (or the enclosing chain) is put in front of it. Resolved here, and the collapse
      // then CASCADES: the innermost pair becomes one mangled name, which is itself a namespace, so
      // the level above collapses in turn.
      if (Ctx.NamespaceNames.IndexOf(BaseName) < 0) and (Pos('.', BaseName) = 0) then
      begin
        Qual := ResolveNamespacePrefix(ActivePrefix, BaseName, Ctx, Using);
        if Qual <> '' then BaseName := Qual;
      end;
      // ⛔ ...and the ENUM'S NAME IN THE MIDDLE. "NS.E1.B" names the same member as "NS.B" once the
      // enum's members are members of NS: the base has already collapsed to "NS.E1", which is not a
      // namespace, so the chain stopped there and read as a record field - 0. Drop the middle component
      // when what is left IS a namespace that has the member.
      if (Ctx.NamespaceNames.IndexOf(BaseName) < 0) and (LastDelimiter('.', BaseName) > 0) then
      begin
        Qual := Copy(BaseName, 1, LastDelimiter('.', BaseName) - 1);
        if (Ctx.NamespaceNames.IndexOf(Qual) >= 0) and
           Ctx.IsMember(Qual, UpperCase(VarToStr(Node.Value))) then
          BaseName := Qual;
      end;
      if Ctx.NamespaceNames.IndexOf(BaseName) >= 0 then
      begin
        // ⛔ ...AND A QUALIFIED NAME REACHES WHAT THAT NAMESPACE CAN SEE, NOT ONLY WHAT IT DECLARES.
        // "Namespace B : Using A : End Namespace" makes "B.x" name A's x - fbc's own namespace/using2,
        // using_reimp2 and using all read a member through one or two USING hops. ResolveUnqualified
        // has had that closure since m661 and this path did not: the very shape of
        // [[a-rule-one-path-has-and-the-other-does-not]]. Worse than a refusal - the name was mangled
        // to "B.X", which exists nowhere, and the read answered 0 in silence.
        // ⚠️ Only when B does NOT declare the name itself: a namespace's own member always wins over an
        // imported one, which is the order ResolveUnqualified already keeps.
        V := UpperCase(VarToStr(Node.Value));
        if not Ctx.IsMember(BaseName, V) then
        begin
          UseUsing := TStringList.Create;
          try
            Ctx.AddUsingClosure(BaseName, UseUsing);
            for k := 0 to UseUsing.Count - 1 do
              // ⛔ ...AND WHAT IT CAN SEE INCLUDES A NESTED NAMESPACE, not only a MEMBER. A nested
              // namespace is recorded in NamespaceNames and NEVER in MemberKeys (CollectNamespaces
              // says so, and the same trap is written down at the procedure-declaration branch above),
              // so "Namespace ns_c : Using ns_b" left "ns_c.inner" - where inner is a namespace of
              // ns_b - matching nothing: it mangled to "NS_C.INNER", which exists nowhere, and
              // "ns_c.inner.foo" then read as a record field and answered 0. The direct spelling
              // "ns_b.inner.foo" worked, which is what said the gap was in the IMPORT hop.
              // fbc suite namespace/using_nested.
              if Ctx.IsMember(UseUsing[k], V) or
                 (Ctx.NamespaceNames.IndexOf(UseUsing[k] + '.' + V) >= 0) then
              begin
                BaseName := UseUsing[k];
                Break;
              end;
          finally
            UseUsing.Free;
          end;
        end;
        Mangled := BaseName + '.' + V;
        NewNode := TASTNode.CreateWithValue(antIdentifier, Mangled, BaseId.Token);
        Result := NewNode;          // caller frees old Node
        Exit;
      end;
    end;
    Exit;                            // a real record field access: leave alone
  end;

  // Prefix an unqualified identifier that names a member of the active (or enclosing) namespace.
  // ⛔ ...AND "@x" IS ONE OF THEM. An address-of node is not an antIdentifier - it carries its name in
  // Value with no child at all - so this branch never saw it and "@i" inside a namespace kept the bare
  // name "I", which exists nowhere after flattening: it fell off the end of the @ chain as "Undefined
  // procedure (address-of @): I". What proves the lowering itself is sound is that VARPTR(i) on the
  // very same member WORKS: VarPtr synthesises its antProcAddress AFTER this pass, from a name that is
  // already mangled. So the defect was never in @ - it was the name arriving unmangled.
  // ⛔ ...AND A CALL WRITTEN AS A STATEMENT IS ONE OF THEM TOO. "p1 7" inside a namespace is an
  // antProcedureCall, which carries its name in Value with an ARGUMENT LIST as its child - so neither
  // arm above ever saw it, and the name stayed bare while the very same call written as an EXPRESSION
  // ("print p1(7)") was prefixed and worked. antProcedureCall appeared nowhere in this unit.
  // ⛔ ...AND A CAST CARRIES A TYPE NAME IN Value TOO. "Cast(Byte1, i2) = ..." inside a namespace kept
  // the bare "BYTE1", which after flattening is a type that exists nowhere: the upcast-slice branch in
  // the SSA asks FindUDT for it, gets -1, and the statement was refused with a message about a missing
  // "Operator Cast() ByRef" - while the SAME PROGRAM WITHOUT THE NAMESPACE worked. It is the third
  // name-in-Value node, after "@x" and a statement call, and each was found the same way: by a test
  // rather than by reading this list. A cast's type may carry a " PTR" tail, which rides along
  // untouched exactly as an overload's signature does.
  if (Node.NodeType = antIdentifier) or
     ((Node.NodeType = antProcAddress) and (Node.ChildCount = 0)) or
     (Node.NodeType = antProcedureCall) or
     (Node.NodeType = antCast) or
     ((Node.NodeType = antNew) and (VarToStr(Node.Value) <> '')) then
  begin
    V := UpperCase(VarToStr(Node.Value));
    // ⛔ ...and GLOBALSCOPE is a request NOT to resolve against the enclosing namespace: ".v" inside a
    // namespace means the MODULE-LEVEL v. There are two identifier sites in this pass and the rule has
    // to be in BOTH - with it only in the first, ".g()" (a call, rewritten there) answered the global
    // one while ".v" (a plain read, rewritten here) still answered the namespace's.
    // ⛔ AN OVERLOADED PROCEDURE'S NAME CARRIES ITS SIGNATURE - "G~II" - because RegisterOverloadLabel
    // decorates it during parsing and this pass runs afterwards. Resolve on the BASE and put the
    // signature back, or the DECLARATION keeps its bare "G~II" while the CALL becomes "N.G" and the
    // two stop naming the same thing ("Array not declared: N.G").
    SigPos := Pos('~', V);
    if SigPos > 0 then
    begin
      BaseV := Copy(V, 1, SigPos - 1);
      SigV := Copy(V, SigPos, MaxInt);
      // ⛔⛔ ...AND THE TYPE NAMES INSIDE THAT SIGNATURE ARE NOT DEAD TEXT. The tail after ':' is the
      // list of the parameters' TYPE names, written by ProcSigFromParams while the file is being
      // PARSED - long before a namespace exists - so it holds the LITERAL spelling of the source. The
      // base was resolved here and the tail was copied back verbatim, which is why a declaration
      // inside a namespace kept "~I:T1" while the CALL side, built after flattening, asked for
      // "~I:FOO.T1". The two then named different things, and ResolveCallLabel does NOT raise when no
      // tail matches: it falls back to ARITY and takes the first candidate of the right count, so two
      // overloads on two imported types both ran the FIRST one - a wrong answer, silently.
      // ⭐ Resolved with ResolveUnqualified, the very function that already resolves ordinary
      // identifiers here: it answers '' for anything it does not own, so a type declared OUTSIDE the
      // namespace (and every builtin, and every '-' placeholder) is left exactly as it was. The tail
      // is comma-separated and positional, so splitting and rebuilding it is exact.
      SigV := ResolveSigTypeNames(SigV, ActivePrefix, Ctx, Using);
    end
    else
    begin
      BaseV := V;
      SigV := '';
    end;
    // ⛔ ...AND A TYPE NAME CAN CARRY A " PTR" TAIL, which is not part of the name to resolve. It rides
    // along untouched, exactly as an overload's signature does. Without this "Dim As Byte1 Ptr p = @i2"
    // inside a namespace kept the bare "BYTE1 PTR" - a type that exists nowhere after flattening - and
    // "p->b1" answered 1 instead of the field, while the same two lines outside a namespace were right.
    // A space cannot occur in a variable name, so this can only ever peel a type.
    while (Length(BaseV) > 4) and (Copy(BaseV, Length(BaseV) - 3, 4) = ' PTR') do
    begin
      SigV := ' PTR' + SigV;
      BaseV := TrimRight(Copy(BaseV, 1, Length(BaseV) - 4));
    end;
    // ⛔ ...or a USING has brought a namespace into scope. The test used to be "we are INSIDE a
    // namespace" (ActivePrefix <> ''), which is right for the enclosing-chain rule and wrong the moment
    // an import exists: a "Using N" at MODULE level leaves the active prefix empty, so the whole
    // prefixing branch was skipped and the imported names resolved to nothing at all.
    if (Pos('.', BaseV) = 0) and (BaseV <> '') and
       ((ActivePrefix <> '') or ((Using <> nil) and (Using.Count > 0))) and
       (Node.Attributes.Values['GLOBALSCOPE'] <> '1') and
       ((Shadow = nil) or (Shadow.IndexOf(BaseV) < 0)) then
    begin
      Mangled := ResolveUnqualified(ActivePrefix, BaseV, Ctx, Using);
      if Mangled <> '' then Node.Value := Mangled + SigV;
    end
    // ⭐ ...AND A GLOBALSCOPE NAME STILL REACHES WHAT A "USING" IMPORTED. A leading '.' asks not to be
    // resolved against the ENCLOSING namespace; it does not ask to ignore the imports, because a
    // module-level "Using N" puts N's members into exactly the scope the dot names. fbc's own
    // namespace/using reads ".bar" of a namespace imported at module level, and namespace/global and
    // dups2/dups3 do the same. Without this the dot resolved to nothing and the read answered 0.
    // ⚠️ ResolveUnqualified declines any name the program declares at module level (GlobalNames), so
    // ".v" beside a "Dim Shared v" still means the module-level v - the rule the dot was written for.
    // The enclosing chain is passed as '' on purpose: the dot asked for none of it.
    else if (Pos('.', BaseV) = 0) and (BaseV <> '') and
            (Node.Attributes.Values['GLOBALSCOPE'] = '1') and
            (Using <> nil) and (Using.Count > 0) and
            ((Shadow = nil) or (Shadow.IndexOf(BaseV) < 0)) then
    begin
      Mangled := ResolveUnqualified('', BaseV, Ctx, Using);
      if Mangled <> '' then Node.Value := Mangled + SigV;
    end;
  end;
end;

// PASS 3 — remove antNamespace wrappers, hoisting their (already rewritten) children to the parent
// statement list, preserving order. Recurses so nested namespaces flatten too.
procedure HoistNamespaces(Node: TASTNode);
var
  i, Base, k: Integer;
  Child, GrandChild: TASTNode;
begin
  i := 0;
  while i < Node.Children.Count do
  begin
    Child := TASTNode(Node.Children[i]);
    if Child.NodeType = antNamespace then
    begin
      HoistNamespaces(Child);                 // flatten any nested namespaces first
      Node.Children.Extract(Child);           // detach wrapper without freeing its children
      Base := i;
      k := 0;
      while Child.Children.Count > 0 do
      begin
        GrandChild := TASTNode(Child.Children[0]);
        Child.Children.Extract(GrandChild);
        Node.Children.Insert(Base + k, GrandChild);
        Inc(k);
      end;
      Child.Free;                             // empty wrapper
      i := Base + k;
    end
    else
    begin
      HoistNamespaces(Child);
      Inc(i);
    end;
  end;
end;

procedure FlattenNamespaces(AST: TASTNode);
var
  Ctx: TNsContext;
  gi: Integer;
  FwdList: TStringList;
begin
  if AST = nil then Exit;
  Ctx := TNsContext.Create;
  try
    CollectNamespaces(AST, '', Ctx);
    if Ctx.NamespaceNames.Count = 0 then Exit;   // no namespaces: nothing to do
    // The names the program declares at MODULE level, so an import cannot take one over.
    // ⭐ FIRST the FORWARD declarations, at position 0. A "Declare Function bar" emits no node, so the
    // only position BAR had was its BODY's - and a body written under the "Using" lost to the import:
    // "print bar" answered the namespace's 1 where fbc answers the program's 2. Position 0 = visible
    // from the top, which is what the oracle does: fbc answers 2 whether the Declare stands before the
    // Using or after it. (Without any Declare at all fbc REFUSES the program - "error 4: Duplicated
    // definition" - so nothing it compiles is harmed by this being unconditional.) NoteDeclared keeps
    // the FIRST entry, so a later real definition does not push the position back down. DIVERGENZE 98.
    FwdList := TStringList.Create;
    try
      FwdList.Delimiter := ',';
      FwdList.StrictDelimiter := True;
      FwdList.DelimitedText := AST.Attributes.Values['FWDDECL'];
      for gi := 0 to FwdList.Count - 1 do
        if Trim(FwdList[gi]) <> '' then
          NoteDeclared(Ctx.GlobalNames, UpperCase(Trim(FwdList[gi])), 0);
    finally
      FwdList.Free;
    end;
    for gi := 0 to AST.ChildCount - 1 do
      if AST.GetChild(gi).NodeType <> antNamespace then
      begin
        AddDeclaredNames(AST.GetChild(gi), Ctx.GlobalNames, gi);
        // ...and the module-level TYPE names on their own, for the type-slot question.
        if (AST.GetChild(gi).NodeType = antTypeDecl) and (VarToStr(AST.GetChild(gi).Value) <> '') then
          Ctx.GlobalTypeNames.Add(UpperCase(VarToStr(AST.GetChild(gi).Value)));
      end;
    CollectTypeFieldNames(AST, Ctx);
    RewriteRefs(AST, '', nil, Ctx, nil);
    HoistNamespaces(AST);
  finally
    Ctx.Free;
  end;
end;

end.
