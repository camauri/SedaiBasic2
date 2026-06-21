unit SedaiNamespace;

{$mode objfpc}{$H+}

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
end;

destructor TNsContext.Destroy;
begin
  NamespaceNames.Free;
  MemberKeys.Free;
  inherited Destroy;
end;

function TNsContext.IsMember(const Prefix, Name: string): Boolean;
begin
  Result := MemberKeys.IndexOf(Prefix + '|' + Name) >= 0;
end;

// Forward declarations (mutual references in pass 1).
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
        if Pos('.', Result) > 0 then Result := '';
      end;
  end;
end;

// PASS 1 — collect namespace names and member keys (no mutation).
procedure CollectNamespaces(Node: TASTNode; const Prefix: string; Ctx: TNsContext);
var
  i, j: Integer;
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
procedure CollectShadowNames(Node: TASTNode; Shadow: TStringList);
var
  i: Integer;
  Child: TASTNode;
begin
  case Node.NodeType of
    antParameterList:
      for i := 0 to Node.ChildCount - 1 do
        if Node.GetChild(i).NodeType = antIdentifier then
          Shadow.Add(UpperCase(VarToStr(Node.GetChild(i).Value)));
    antArrayDecl:
      if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
        Shadow.Add(UpperCase(VarToStr(Node.GetChild(0).Value)));
  end;
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.GetChild(i);
    // Do not descend into nested namespaces here (different scope).
    if Child.NodeType <> antNamespace then
      CollectShadowNames(Child, Shadow);
  end;
end;

// Resolve an unqualified member name V against the active prefix chain (innermost first). Returns
// the mangled "PREFIX.V" if V is a member of some enclosing namespace, else ''.
function ResolveUnqualified(const ActivePrefix, V: string; Ctx: TNsContext): string;
var
  P: string;
  DotPos: Integer;
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
end;

// PASS 2 — rewrite references (and member declaration names) bottom-up. Returns the node to use in
// place of Node (Node itself, unless it is a collapsed member-access replaced by a new identifier).
// ActivePrefix = current namespace ('' at module level). Shadow = names bound as params/locals of
// the enclosing procedure (not to be prefixed). The caller owns freeing a replaced node.
function RewriteRefs(Node: TASTNode; const ActivePrefix: string;
                     Shadow: TStringList; Ctx: TNsContext): TASTNode;
var
  i: Integer;
  ChildPrefix, BaseName, Mangled, V: string;
  NewNode, BaseId: TASTNode;
  UseShadow: TStringList;
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
    CollectShadowNames(Node, UseShadow);
  end;

  // Recurse into children first (bottom-up), replacing each in place if needed.
  for i := 0 to Node.ChildCount - 1 do
  begin
    NewNode := RewriteRefs(Node.GetChild(i), ChildPrefix, UseShadow, Ctx);
    if NewNode <> Node.GetChild(i) then
      ReplaceChildAt(Node, i, NewNode);   // frees the old child, installs the rewritten one
  end;

  if UseShadow <> Shadow then
    UseShadow.Free;

  // antTypeDecl name lives in Value (not a child identifier): mangle it here.
  if (Node.NodeType = antTypeDecl) and (ActivePrefix <> '') then
  begin
    V := UpperCase(VarToStr(Node.Value));
    if (Pos('.', V) = 0) and Ctx.IsMember(ActivePrefix, V) then
      Node.Value := ActivePrefix + '.' + V;
    Exit;
  end;

  // Collapse a namespace-qualified access "NS.member" into a single mangled identifier.
  if Node.NodeType = antMemberAccess then
  begin
    if (Node.ChildCount >= 1) and (Node.GetChild(0).NodeType = antIdentifier) then
    begin
      BaseId := Node.GetChild(0);
      BaseName := UpperCase(VarToStr(BaseId.Value));
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
  if Node.NodeType = antIdentifier then
  begin
    V := UpperCase(VarToStr(Node.Value));
    if (Pos('.', V) = 0) and (V <> '') and (ActivePrefix <> '') and
       ((Shadow = nil) or (Shadow.IndexOf(V) < 0)) then
    begin
      Mangled := ResolveUnqualified(ActivePrefix, V, Ctx);
      if Mangled <> '' then Node.Value := Mangled;
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
begin
  if AST = nil then Exit;
  Ctx := TNsContext.Create;
  try
    CollectNamespaces(AST, '', Ctx);
    if Ctx.NamespaceNames.Count = 0 then Exit;   // no namespaces: nothing to do
    RewriteRefs(AST, '', nil, Ctx);
    HoistNamespaces(AST);
  finally
    Ctx.Free;
  end;
end;

end.
