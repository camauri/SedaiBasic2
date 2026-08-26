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
                                   // ⛔ They WIN over a name a USING imported: fbc resolves an
                                   // unqualified reference against the global scope before the
                                   // imported ones, so "Dim Shared v" beside "Using A" (which also
                                   // has a v) means the global v. Without this the import silently
                                   // took over a name the program had declared itself.
    constructor Create;
    destructor Destroy; override;
    function IsMember(const Prefix, Name: string): Boolean;
  end;

constructor TNsContext.Create;
begin
  NamespaceNames := TStringList.Create;
  NamespaceNames.Duplicates := dupIgnore;
  NamespaceNames.Sorted := True;
  MemberKeys := TStringList.Create;
  MemberKeys.Duplicates := dupIgnore;
  MemberKeys.Sorted := True;
  GlobalNames := TStringList.Create;
  GlobalNames.Duplicates := dupIgnore;
  GlobalNames.Sorted := True;
end;

destructor TNsContext.Destroy;
begin
  NamespaceNames.Free;
  MemberKeys.Free;
  GlobalNames.Free;
  inherited Destroy;
end;

function TNsContext.IsMember(const Prefix, Name: string): Boolean;
begin
  Result := MemberKeys.IndexOf(Prefix + '|' + Name) >= 0;
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
          if MemName <> '' then Ctx.MemberKeys.Add(ChildPrefix + '|' + MemName);
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
procedure AddDeclaredNames(Node: TASTNode; Shadow: TStringList);
var
  i: Integer;
begin
  if Node = nil then Exit;
  if (Node.NodeType = antArrayDecl) and (Node.ChildCount >= 1) and
     (Node.GetChild(0).NodeType = antIdentifier) then
    Shadow.Add(UpperCase(VarToStr(Node.GetChild(0).Value)));
  for i := 0 to Node.ChildCount - 1 do
    if Node.GetChild(i).NodeType in [antArrayDecl, antDim] then
      AddDeclaredNames(Node.GetChild(i), Shadow);
end;

// Resolve an unqualified member name V against the active prefix chain (innermost first). Returns
// the mangled "PREFIX.V" if V is a member of some enclosing namespace, else ''.
function ResolveUnqualified(const ActivePrefix, V: string; Ctx: TNsContext;
                            Using: TStringList): string;
// ...and then against the namespaces a USING has brought into scope. Tried AFTER the enclosing chain,
// so a name of the namespace one is written INSIDE always wins over an imported one - which is what
// fbc does and the only order that keeps an existing program's meaning.
var
  P: string;
  DotPos, u: Integer;
begin
  Result := '';
  P := ActivePrefix;
  while P <> '' do
  begin
    if Ctx.IsMember(P, V) then Exit(P + '.' + V);
    DotPos := LastDelimiter('.', P);
    if DotPos = 0 then Break;
    P := Copy(P, 1, DotPos - 1);
  end;
  // ⛔ ...but a MODULE-LEVEL name of the program's own wins over every import: fbc resolves an
  // unqualified reference against the global scope first. "Dim Shared v" beside a "Using A" that also
  // has a v means the global v, and without this test the import silently took the name over.
  if (Using <> nil) and (Ctx.GlobalNames.IndexOf(V) < 0) then
    for u := 0 to Using.Count - 1 do
      if Ctx.IsMember(Using[u], V) then Exit(Using[u] + '.' + V);
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
  i: Integer;
  ChildPrefix, BaseName, Mangled, V, Qual: string;
  NewNode, BaseId: TASTNode;
  UseShadow, UseUsing: TStringList;
  Drop: array of Integer;
  UsingNs: string;
  SigPos, DotPos: Integer;
  BaseV, SigV: string;
begin
  Result := Node;

  // Determine the prefix/shadow for descending into children.
  ChildPrefix := ActivePrefix;
  UseShadow := Shadow;

  if Node.NodeType = antNamespace then
    ChildPrefix := CombinePrefix(ActivePrefix, VarToStr(Node.Value))
  else if Node.NodeType = antProcedureDecl then
  begin
    // A procedure introduces its own parameter/local scope: those names shadow namespace members.
    UseShadow := TStringList.Create;
    UseShadow.Duplicates := dupIgnore;
    UseShadow.Sorted := True;
    CollectParamNames(Node, UseShadow);
  end;

  // Recurse into children first (bottom-up), replacing each in place if needed.
  // ⭐ A "Using N" seen among the children brings N into scope FOR THE CHILDREN THAT FOLLOW IT, and only
  // within this node - which is exactly FreeBASIC's rule and needs no new machinery: the imported prefix
  // joins the chain ResolveUnqualified already walks. The directive itself is then DROPPED, so nothing
  // downstream ever sees a stray PRINT USING that would take the next PRINT's format with it.
  UseUsing := Using;
  SetLength(Drop, 0);
  for i := 0 to Node.ChildCount - 1 do
  begin
    UsingNs := UsingDirectiveName(Node.GetChild(i), Ctx, ChildPrefix);
    if UsingNs <> '' then
    begin
      if UseUsing = Using then
      begin
        UseUsing := TStringList.Create;
        if Using <> nil then UseUsing.Assign(Using);
      end;
      if UseUsing.IndexOf(UsingNs) < 0 then UseUsing.Add(UsingNs);
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
    NewNode := RewriteRefs(Node.GetChild(i), ChildPrefix, UseShadow, Ctx, UseUsing);
    if NewNode <> Node.GetChild(i) then
      ReplaceChildAt(Node, i, NewNode);   // frees the old child, installs the rewritten one
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
        Node.GetChild(0).Value := ActivePrefix + '.' + V;
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
    if (BaseV <> '') and (Pos('.', BaseV) = 0) and Ctx.IsMember(ActivePrefix, BaseV) then
      Node.Attributes.Values['EXTENDS'] := ActivePrefix + '.' + BaseV;
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
        Mangled := BaseName + '.' + UpperCase(VarToStr(Node.Value));
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
  if (Node.NodeType = antIdentifier) or
     ((Node.NodeType = antProcAddress) and (Node.ChildCount = 0)) or
     (Node.NodeType = antProcedureCall) then
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
    end
    else
    begin
      BaseV := V;
      SigV := '';
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
begin
  if AST = nil then Exit;
  Ctx := TNsContext.Create;
  try
    CollectNamespaces(AST, '', Ctx);
    if Ctx.NamespaceNames.Count = 0 then Exit;   // no namespaces: nothing to do
    // The names the program declares at MODULE level, so an import cannot take one over.
    for gi := 0 to AST.ChildCount - 1 do
      if AST.GetChild(gi).NodeType <> antNamespace then
        AddDeclaredNames(AST.GetChild(gi), Ctx.GlobalNames);
    RewriteRefs(AST, '', nil, Ctx, nil);
    HoistNamespaces(AST);
  finally
    Ctx.Free;
  end;
end;

end.
