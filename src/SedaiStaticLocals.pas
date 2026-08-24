unit SedaiStaticLocals;

{$mode objfpc}{$H+}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

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
  // ⭐ "@z" DOES NOT HOLD AN IDENTIFIER. The parser keeps the historical shape for the bare form -
  // an antProcAddress whose VALUE is the name and which has no children - so walking only
  // antIdentifier left it naming a variable this pass had just renamed away, and the SSA failed with
  // "Undefined procedure (address-of @): Z". Every other spelling was renamed and worked, which is
  // why it read as a defect of STATIC rather than of the rename: "@z(i)" and "@z.f" keep their
  // operand as a CHILD identifier and were reached; only the one-node form was not.
  // (The form WITH children is left to the recursion below, which renames the child.)
  if (Node.NodeType = antProcAddress) and (Node.ChildCount = 0) and
     (UpperCase(VarToStr(Node.Value)) = FromU) then
    Node.Value := ToName;
  for i := 0 to Node.ChildCount - 1 do
    RenameRefs(Node.GetChild(i), FromU, ToName);
end;

// Build the hoisted "DIM SHARED <mangled> AS <typeName> [= initClone]" node for one static.
// Src is the ORIGINAL declaration, and it is here for its ATTRIBUTES: this path REBUILDS the node
// (unlike the array path, which clones) so it can move only a constant initializer, and rebuilding
// dropped everything the parser had stamped on the declaration. FIXEDLEN is the one that showed:
// "Static As ZString * 32 z" hoisted to a plain variable-length global, so "@z" handed back a managed
// string reference where a raw buffer address was expected and the deref failed. The attributes
// describe the TYPE, not the storage class, so they belong on the hoisted declaration - only STATIC
// itself is replaced by SHARED.
function BuildSharedDecl(Src: TASTNode; const Mangled, TypeName: string; InitClone: TASTNode;
                        const Tok: TLexerToken): TASTNode;
var
  DimNode, DeclNode: TASTNode;
  a: Integer;
begin
  DimNode := TASTNode.Create(antDim, Tok);
  DeclNode := TASTNode.Create(antArrayDecl, Tok);
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, Mangled, Tok));   // child0 = name
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, Tok));  // child1 = type
  if InitClone <> nil then
    DeclNode.AddChild(InitClone);                                            // child2 = initializer
  if Src <> nil then
    for a := 0 to Src.Attributes.Count - 1 do
      DeclNode.Attributes.Values[Src.Attributes.Names[a]] := Src.Attributes.ValueFromIndex[a];
  DeclNode.Attributes.Values['STATIC'] := '0';
  DeclNode.Attributes.Values['SHARED'] := '1';   // module global, persistent, visible inside procedures
  DimNode.AddChild(DeclNode);
  Result := DimNode;
end;

// Build the hoisted module-level declaration for a STATIC ARRAY: a clone of the declaration itself,
// renamed to the mangled name and marked SHARED. Cloning (rather than rebuilding) keeps the dimension
// list, the element type and the "= { ... }" initializer — with its ARRAYINIT/LEVELSIZES attributes —
// exactly as the parser produced them.
function BuildSharedArrayDecl(Decl: TASTNode; const Mangled: string): TASTNode;
var
  DimNode, DeclClone: TASTNode;
begin
  DeclClone := Decl.Clone;
  DeclClone.GetChild(0).Value := Mangled;
  DeclClone.Attributes.Values['STATIC'] := '0';
  DeclClone.Attributes.Values['SHARED'] := '1';   // module global: persistent and visible inside procedures
  DimNode := TASTNode.Create(antDim, Decl.Token);
  DimNode.AddChild(DeclClone);
  Result := DimNode;
end;

// True when every bound in a dimension list is an integer literal, so the array can be sized once at
// module scope. A non-literal bound may read the PROCEDURE's own parameters or locals ("STATIC a(0 To n)",
// which FreeBASIC sizes on the first call): those names do not exist at module scope, so such a
// declaration is sized inside the procedure instead — see BuildOnceRedim.
function DimsAreLiteral(Dims: TASTNode): Boolean;
var
  i, k, v: Integer;
  D: TASTNode;

  function IsLitInt(N: TASTNode): Boolean;
  begin
    Result := (N <> nil) and (N.NodeType = antLiteral) and TryStrToInt(Trim(VarToStr(N.Value)), v);
  end;

begin
  Result := False;
  if Dims = nil then Exit;
  // "STATIC a()" — an empty subscript list has no bound to evaluate anywhere, so the hoisted declaration
  // is already complete: the program's own REDIM sizes it.
  if Dims.ChildCount = 0 then begin Result := True; Exit; end;
  for i := 0 to Dims.ChildCount - 1 do
  begin
    D := Dims.GetChild(i);
    if D = nil then Exit;
    if D.NodeType = antDimRange then
    begin
      if D.Attributes.Values['ELLIPSIS'] = '1' then Continue;   // size deduced from the initializer
      for k := 0 to D.ChildCount - 1 do
        if not IsLitInt(D.GetChild(k)) then Exit;
    end
    else if not IsLitInt(D) then
      Exit;
  end;
  Result := True;
end;

// Build the hoisted "DIM SHARED <mangled>() AS <typeName>" for a variable-bounded static array: an empty
// (REDIM-sizable) module global. The sizing itself stays in the procedure (BuildOnceRedim).
function BuildSharedVarlenDecl(const Mangled, TypeName: string; const Tok: TLexerToken): TASTNode;
var
  DimNode, DeclNode: TASTNode;
begin
  DimNode := TASTNode.Create(antDim, Tok);
  DeclNode := TASTNode.Create(antArrayDecl, Tok);
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, Mangled, Tok));   // child0 = name
  DeclNode.AddChild(TASTNode.Create(antDimensions));                          // child1 = "()" (no bounds yet)
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, TypeName, Tok));  // child2 = element type
  DeclNode.Attributes.Values['VARLEN'] := '1';
  DeclNode.Attributes.Values['SHARED'] := '1';
  DimNode.AddChild(DeclNode);
  Result := DimNode;
end;

// Build the in-procedure guard that sizes a variable-bounded static array exactly once:
//     IF <flag> THEN <flag> = 0 : REDIM <mangled>(<the original bounds>)
// The flag is a module global initialised to 1 (BuildOnceFlagDecl), so the REDIM runs on the FIRST call
// with the bounds THAT call computes, and every later call keeps the array it left behind — which is what
// FreeBASIC does. The condition is a bare identifier on purpose: a synthesised comparison would have to
// carry an operator token, and a synthesised binary op lowers from its TOKEN, not its Value.
function BuildOnceRedim(const Mangled, FlagName: string; Dims: TASTNode;
                        const Tok: TLexerToken): TASTNode;
var
  IfNode, ThenNode, Assign, RedimNode, DeclNode: TASTNode;
begin
  IfNode := TASTNode.Create(antIf, Tok);
  IfNode.AddChild(TASTNode.CreateWithValue(antIdentifier, FlagName, Tok));   // child0 = condition
  ThenNode := TASTNode.Create(antThen, Tok);
  IfNode.AddChild(ThenNode);

  Assign := TASTNode.Create(antAssignment, Tok);
  Assign.AddChild(TASTNode.CreateWithValue(antIdentifier, FlagName, Tok));
  Assign.AddChild(TASTNode.CreateWithValue(antLiteral, 0, Tok));
  ThenNode.AddChild(Assign);

  RedimNode := TASTNode.Create(antRedim, Tok);
  DeclNode := TASTNode.Create(antArrayDecl, Tok);
  DeclNode.AddChild(TASTNode.CreateWithValue(antIdentifier, Mangled, Tok));
  DeclNode.AddChild(Dims.Clone);                     // the bounds as written, evaluated in the procedure
  RedimNode.AddChild(DeclNode);
  ThenNode.AddChild(RedimNode);

  Result := IfNode;
end;

// Lower the STATIC declarations inside one procedure: rewrite each to a hoisted DIM SHARED (collected
// into Hoisted) and rename its references in the body. ProcIdx makes the mangled name unique.
procedure LowerProc(Proc: TASTNode; ProcIdx: Integer; Hoisted: TFPList);
var
  Decls: TFPList;                 // the STATIC antArrayDecl nodes found in this proc
  Parents: TFPList;               // the owning antDim of each
  Grands: TFPList;                // the block that owns the antDim (where a sizing guard is spliced in)
  i, SlotIdx: Integer;
  DimNode, Decl, NameNode, TypeNode, InitClone, GrandNode, InitOne: TASTNode;
  VName, TName, Mangled, FlagName: string;

  procedure CollectStatics(N: TASTNode);
  var k, j: Integer; D, Child: TASTNode;
  begin
    if N = nil then Exit;
    for j := 0 to N.ChildCount - 1 do
    begin
      Child := N.GetChild(j);
      if Child.NodeType <> antDim then Continue;
      for k := 0 to Child.ChildCount - 1 do
      begin
        D := Child.GetChild(k);
        // Two shapes reach here: a typed scalar (child[1] = type identifier) and an array
        // (child[1] = antDimensions, child[2] = element type). Both hoist the same way.
        if (D.NodeType = antArrayDecl) and (D.Attributes.Values['STATIC'] = '1') and
           (D.ChildCount >= 2) and (D.GetChild(0).NodeType = antIdentifier) and
           ((D.GetChild(1).NodeType = antIdentifier) or (D.GetChild(1).NodeType = antDimensions)) then
        begin
          Decls.Add(D);
          Parents.Add(Child);
          Grands.Add(N);
        end;
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
  Grands := TFPList.Create;
  try
    if Proc.Attributes.Values['ALLSTATIC'] = '1' then MarkAllScalarStatics(Proc);
    CollectStatics(Proc);
    for i := 0 to Decls.Count - 1 do
    begin
      Decl := TASTNode(Decls[i]);
      DimNode := TASTNode(Parents[i]);
      GrandNode := TASTNode(Grands[i]);
      NameNode := Decl.GetChild(0);
      TypeNode := Decl.GetChild(1);
      VName := UpperCase(VarToStr(NameNode.Value));
      Mangled := 'STATIC.' + IntToStr(ProcIdx) + '.' + VName;
      if TypeNode.NodeType = antDimensions then
      begin
        // Array static. With literal bounds (the common case, and the only one FreeBASIC lets carry an
        // "= { ... }" initializer — a var-len array cannot be initialised) the declaration hoists whole,
        // dimensions and initializer included. With computed bounds it cannot: they may read the
        // procedure's own parameters, which do not exist at module scope. Then the module keeps an empty
        // array and the procedure sizes it once, on its first call.
        TName := '';
        if (Decl.ChildCount >= 3) and (Decl.GetChild(2).NodeType = antIdentifier) then
          TName := UpperCase(VarToStr(Decl.GetChild(2).Value));
        if DimsAreLiteral(TypeNode) or (Decl.Attributes.Values['ARRAYINIT'] = '1') or (TName = '') then
          Hoisted.Add(BuildSharedArrayDecl(Decl, Mangled))
        else
        begin
          FlagName := Mangled + '.UNSIZED';
          InitOne := TASTNode.CreateWithValue(antLiteral, 1, NameNode.Token);
          Hoisted.Add(BuildSharedVarlenDecl(Mangled, TName, NameNode.Token));
          Hoisted.Add(BuildSharedDecl(nil, FlagName, 'INTEGER', InitOne, NameNode.Token));
          SlotIdx := GrandNode.Children.IndexOf(DimNode);
          if SlotIdx < 0 then SlotIdx := GrandNode.ChildCount - 1;
          GrandNode.Children.Insert(SlotIdx + 1,
                                    BuildOnceRedim(Mangled, FlagName, TypeNode, NameNode.Token));
        end;
      end
      else
      begin
        TName := UpperCase(VarToStr(TypeNode.Value));
        // A constant initializer (child[2], an expression, not a ctor argument list) is kept and moved to
        // the module-level DIM SHARED so it runs once at program start.
        if (Decl.ChildCount >= 3) and (Decl.GetChild(2).NodeType <> antArgumentList) then
          InitClone := Decl.GetChild(2).Clone
        else
          InitClone := nil;
        Hoisted.Add(BuildSharedDecl(Decl, Mangled, TName, InitClone, NameNode.Token));
      end;
      // Rename references to the static in the procedure body, then drop the declaration. An antDim
      // left empty afterwards is harmless (ProcessDim exits early when it has no children).
      RenameRefs(Proc, VName, Mangled);
      DimNode.Children.Remove(Decl);   // owns its children -> frees the declaration node
    end;
  finally
    Decls.Free;
    Parents.Free;
    Grands.Free;
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
