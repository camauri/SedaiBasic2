unit SedaiStaticLocals;

{$mode objfpc}{$H+}

// FreeBASIC STATIC locals lowering (AST -> AST), run once before SSA generation (after namespace
// flattening). A `STATIC name AS T [= expr]` declared inside a SUB/FUNCTION is a local whose storage
// PERSISTS across calls and is initialised once. Our VM saves/restores the whole register bank per
// call, so an ordinary local cannot persist — but a module-level `DIM SHARED` scalar can (it is backed
// by a global 1-element array, visible and persistent inside procedures, per M6).
//
// So this pass rewrites each proc-level STATIC into a uniquely-named module global:
//   * pick a mangled name "STATIC.<procindex>.<name>" (private per procedure -> two procs with the
//     same STATIC name do not collide; opaque dotted string, like the namespace mangling);
//   * hoist a "DIM SHARED <mangled> AS T [= expr]" to the top of the program (declared + initialised
//     ONCE at program start; the optional initializer must be a constant — it is evaluated at module
//     scope, where the procedure's parameters/locals do not exist);
//   * rename every reference to <name> inside that procedure body to <mangled> (field names of
//     member accesses are left alone);
//   * remove the original STATIC declaration from the procedure body.
//
// A module-level STATIC (not inside a procedure) is already persistent, so it is just demoted to a
// plain DIM (the STATIC attribute is cleared). With no STATIC declarations the pass is a no-op.

interface

uses
  SedaiParserTypes, sedaiast;

// Mutates AST in place. Safe to call unconditionally (no-op without STATIC declarations).
procedure LowerStaticLocals(AST: TASTNode);

implementation

uses
  Classes, SysUtils, Variants, SedaiLexerToken;

// Rename every antIdentifier named FromU (UPPER) to ToName, recursively — but never the field name of
// a member access (that lives in the antMemberAccess node's Value, and names a record field, not the
// static variable).
procedure RenameRefs(Node: TASTNode; const FromU, ToName: string);
var
  i: Integer;
begin
  if Node = nil then Exit;
  if Node.NodeType = antMemberAccess then
  begin
    // Value is the field name (leave it); only the object part (children) can reference the static.
    for i := 0 to Node.ChildCount - 1 do
      RenameRefs(Node.GetChild(i), FromU, ToName);
    Exit;
  end;
  if (Node.NodeType = antIdentifier) and (UpperCase(VarToStr(Node.Value)) = FromU) then
    Node.Value := ToName;
  for i := 0 to Node.ChildCount - 1 do
    RenameRefs(Node.GetChild(i), FromU, ToName);
end;

// Build the hoisted "DIM SHARED <mangled> AS <typeName> [= initClone]" node for one static.
function BuildSharedDecl(const Mangled, TypeName: string; InitClone: TASTNode;
                        const Tok: TLexerToken): TASTNode;
var
  DimNode, DeclNode: TASTNode;
begin
  DimNode := TASTNode.Create(antDim, Tok);
  DeclNode := TASTNode.Create(antArrayDecl, Tok);
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, Mangled, Tok));   // child0 = name
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, Tok));  // child1 = type
  if InitClone <> nil then
    DeclNode.AddChild(InitClone);                                            // child2 = initializer
  DeclNode.Attributes.Values['SHARED'] := '1';   // module global, persistent, visible inside procedures
  DimNode.AddChild(DeclNode);
  Result := DimNode;
end;

// Lower the STATIC declarations inside one procedure: rewrite each to a hoisted DIM SHARED (collected
// into Hoisted) and rename its references in the body. ProcIdx makes the mangled name unique.
procedure LowerProc(Proc: TASTNode; ProcIdx: Integer; Hoisted: TFPList);
var
  Decls: TFPList;                 // the STATIC antArrayDecl nodes found in this proc
  Parents: TFPList;               // the owning antDim of each
  i: Integer;
  DimNode, Decl, NameNode, TypeNode, InitClone: TASTNode;
  VName, TName, Mangled: string;

  procedure CollectStatics(N: TASTNode);
  var k: Integer; D: TASTNode;
  begin
    if N = nil then Exit;
    if N.NodeType = antDim then
      for k := 0 to N.ChildCount - 1 do
      begin
        D := N.GetChild(k);
        if (D.NodeType = antArrayDecl) and (D.Attributes.Values['STATIC'] = '1') and
           (D.ChildCount >= 2) and (D.GetChild(0).NodeType = antIdentifier) and
           (D.GetChild(1).NodeType = antIdentifier) then
        begin
          Decls.Add(D);
          Parents.Add(N);
        end;
      end;
    for k := 0 to N.ChildCount - 1 do
      // Do not descend into a nested procedure (its statics belong to a different scope/index).
      if N.GetChild(k).NodeType <> antProcedureDecl then
        CollectStatics(N.GetChild(k));
  end;

  // FreeBASIC "SUB|FUNCTION ... Static": mark every scalar body-local DIM as static so it is lowered to a
  // persistent global. Only typed-scalar DIMs (child[1] = type identifier) are covered — array locals and
  // implicitly-declared variables are not made static by the modifier (a v1 limitation). Parameters live
  // in the ParamList (not an antDim), so they are never touched.
  procedure MarkAllScalarStatics(N: TASTNode);
  var k: Integer; D: TASTNode;
  begin
    if N = nil then Exit;
    if N.NodeType = antDim then
      for k := 0 to N.ChildCount - 1 do
      begin
        D := N.GetChild(k);
        if (D.NodeType = antArrayDecl) and (D.ChildCount >= 2) and
           (D.GetChild(0).NodeType = antIdentifier) and (D.GetChild(1).NodeType = antIdentifier) then
          D.Attributes.Values['STATIC'] := '1';
      end;
    for k := 0 to N.ChildCount - 1 do
      if N.GetChild(k).NodeType <> antProcedureDecl then
        MarkAllScalarStatics(N.GetChild(k));
  end;

begin
  Decls := TFPList.Create;
  Parents := TFPList.Create;
  try
    if Proc.Attributes.Values['ALLSTATIC'] = '1' then MarkAllScalarStatics(Proc);
    CollectStatics(Proc);
    for i := 0 to Decls.Count - 1 do
    begin
      Decl := TASTNode(Decls[i]);
      DimNode := TASTNode(Parents[i]);
      NameNode := Decl.GetChild(0);
      TypeNode := Decl.GetChild(1);
      VName := UpperCase(VarToStr(NameNode.Value));
      TName := UpperCase(VarToStr(TypeNode.Value));
      Mangled := 'STATIC.' + IntToStr(ProcIdx) + '.' + VName;
      // A constant initializer (child[2], an expression, not a ctor argument list) is kept and moved to
      // the module-level DIM SHARED so it runs once at program start.
      if (Decl.ChildCount >= 3) and (Decl.GetChild(2).NodeType <> antArgumentList) then
        InitClone := Decl.GetChild(2).Clone
      else
        InitClone := nil;
      Hoisted.Add(BuildSharedDecl(Mangled, TName, InitClone, NameNode.Token));
      // Rename references to the static in the procedure body, then drop the declaration. An antDim
      // left empty afterwards is harmless (ProcessDim exits early when it has no children).
      RenameRefs(Proc, VName, Mangled);
      DimNode.Children.Remove(Decl);   // owns its children -> frees the declaration node
    end;
  finally
    Decls.Free;
    Parents.Free;
  end;
end;

// Walk the whole program, lowering each procedure's statics; also demote any module-level STATIC to a
// plain DIM (clearing the attribute). Returns proc count via the recursive index.
procedure WalkProcs(Node: TASTNode; var ProcIdx: Integer; Hoisted: TFPList);
var
  i, k: Integer;
  Dim, Decl: TASTNode;
begin
  if Node = nil then Exit;
  for i := 0 to Node.ChildCount - 1 do
  begin
    if Node.GetChild(i).NodeType = antProcedureDecl then
    begin
      LowerProc(Node.GetChild(i), ProcIdx, Hoisted);
      Inc(ProcIdx);
    end
    else
    begin
      // Module-level STATIC: already persistent -> demote to a plain DIM (clear the attribute).
      if Node.GetChild(i).NodeType = antDim then
      begin
        Dim := Node.GetChild(i);
        for k := 0 to Dim.ChildCount - 1 do
        begin
          Decl := Dim.GetChild(k);
          if (Decl.NodeType = antArrayDecl) and (Decl.Attributes.Values['STATIC'] = '1') then
            Decl.Attributes.Values['STATIC'] := '0';
        end;
      end;
      WalkProcs(Node.GetChild(i), ProcIdx, Hoisted);
    end;
  end;
end;

procedure LowerStaticLocals(AST: TASTNode);
var
  ProcIdx, i: Integer;
  Hoisted: TFPList;
begin
  if AST = nil then Exit;
  Hoisted := TFPList.Create;
  try
    ProcIdx := 0;
    WalkProcs(AST, ProcIdx, Hoisted);
    // Prepend the hoisted "DIM SHARED" declarations to the top of the program, in collection order, so
    // each static global is declared and initialised before any procedure that uses it runs.
    for i := Hoisted.Count - 1 downto 0 do
      AST.Children.Insert(0, TASTNode(Hoisted[i]));
  finally
    Hoisted.Free;
  end;
end;

end.
