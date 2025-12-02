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
unit SedaiAST;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, Variants, contnrs, Math,
  SedaiLexerTypes, SedaiLexerToken, SedaiParserTypes;

type
  { TASTNode - Main AST Node class }
  TASTNode = class
  private
    FNodeType: TASTNodeType;
    FToken: TLexerToken;       // Associated token (if any)
    FParent: TASTNode;
    FChildren: TFPObjectList;  // Child nodes
    FSourceLine: Integer;
    FSourceColumn: Integer;
    FValue: Variant;           // Node value (for literals)
    FAttributes: TStringList;  // Additional attributes

    // Lazy evaluation flags for performance
    FDisplayStringCached: string;
    FDisplayStringValid: Boolean;

  protected
    procedure DoChildAdded(Child: TASTNode); virtual;
    procedure DoChildRemoved(Child: TASTNode); virtual;
    function BuildDisplayString: string; virtual;
    procedure InvalidateCache; virtual;

  public
    constructor Create(ANodeType: TASTNodeType; AToken: TLexerToken = nil); overload;
    constructor CreateWithValue(ANodeType: TASTNodeType; const AValue: Variant; AToken: TLexerToken = nil); overload;
    destructor Destroy; override;

    // === TREE MANAGEMENT ===
    function AddChild(Child: TASTNode): Integer;
    function InsertChild(Index: Integer; Child: TASTNode): Integer;
    function RemoveChild(Child: TASTNode): Boolean;
    function RemoveChildAt(Index: Integer): Boolean;
    procedure ClearChildren;

    // === NAVIGATION ===
    function GetChild(Index: Integer): TASTNode;
    function GetChildCount: Integer;
    function GetFirstChild: TASTNode;
    function GetLastChild: TASTNode;
    function GetNextSibling: TASTNode;
    function GetPreviousSibling: TASTNode;
    function GetRoot: TASTNode;
    function GetDepth: Integer;

    // === SEARCH ===
    function FindChild(NodeType: TASTNodeType): TASTNode;
    function FindChildren(NodeType: TASTNodeType): TFPObjectList;
    function FindDescendant(NodeType: TASTNodeType): TASTNode;
    function FindDescendants(NodeType: TASTNodeType): TFPObjectList;

    // === UTILITIES ===
    function Clone: TASTNode; virtual;
    function IsLeaf: Boolean;
    function IsRoot: Boolean;
    function HasChildren: Boolean;
    function ContainsNodeType(NodeType: TASTNodeType): Boolean;

    // === VALIDATION ===
    function Validate: Boolean; virtual;
    function GetValidationErrors: TStringList; virtual;

    // === SERIALIZATION ===
    function ToString: string; override;
    function ToDebugString(Indent: Integer = 0): string; virtual;
    function GetDisplayString: string; virtual;

    // === VISITOR PATTERN ===
    procedure Accept(Visitor: TObject); virtual;

    // === PROPERTIES ===
    property NodeType: TASTNodeType read FNodeType write FNodeType;
    property Token: TLexerToken read FToken write FToken;
    property Parent: TASTNode read FParent write FParent;
    property Children: TFPObjectList read FChildren;
    property ChildCount: Integer read GetChildCount;
    property Child[Index: Integer]: TASTNode read GetChild; default;
    property FirstChild: TASTNode read GetFirstChild;
    property LastChild: TASTNode read GetLastChild;
    property SourceLine: Integer read FSourceLine write FSourceLine;
    property SourceColumn: Integer read FSourceColumn write FSourceColumn;
    property Value: Variant read FValue write FValue;
    property Attributes: TStringList read FAttributes;
    property DisplayString: string read GetDisplayString;
  end;

// === AST NODE CREATION HELPERS ===
function CreateLiteralNode(const Value: Variant; Token: TLexerToken = nil): TASTNode;
function CreateIdentifierNode(const Name: string; Token: TLexerToken = nil): TASTNode;
function CreateBinaryOpNode(OpType: TTokenType; Left, Right: TASTNode; Token: TLexerToken = nil): TASTNode;
function CreateUnaryOpNode(OpType: TTokenType; Operand: TASTNode; Token: TLexerToken = nil): TASTNode;
function CreateFunctionCallNode(const FuncName: string; Args: TASTNode; Token: TLexerToken = nil): TASTNode;

// === CONSTANT FOLDING HELPERS ===
function IsConstantNode(Node: TASTNode): Boolean;
function TryFoldBinaryOp(OpType: TTokenType; Left, Right: TASTNode; Token: TLexerToken; out FoldedNode: TASTNode): Boolean;

implementation

{ TASTNode }

constructor TASTNode.Create(ANodeType: TASTNodeType; AToken: TLexerToken);
begin
  inherited Create;
  FNodeType := ANodeType;
  FToken := AToken;
  FParent := nil;
  FChildren := TFPObjectList.Create(True); // Own children
  FValue := Unassigned;
  FAttributes := TStringList.Create;
  FDisplayStringValid := False;

  // Set source location from token if available
  if Assigned(AToken) then
  begin
    FSourceLine := AToken.Line;
    FSourceColumn := AToken.Column;
  end
  else
  begin
    FSourceLine := 0;
    FSourceColumn := 0;
  end;
end;

constructor TASTNode.CreateWithValue(ANodeType: TASTNodeType; const AValue: Variant; AToken: TLexerToken);
begin
  Create(ANodeType, AToken);
  FValue := AValue;
end;

destructor TASTNode.Destroy;
begin
  FAttributes.Free;
  FChildren.Free; // This will free all child nodes
  inherited Destroy;
end;

procedure TASTNode.InvalidateCache;
begin
  FDisplayStringValid := False;
end;

function TASTNode.BuildDisplayString: string;
begin
  if not VarIsEmpty(FValue) then
    Result := Format('%s: %s', [NodeTypeToString(FNodeType), VarToStr(FValue)])
  else
    Result := NodeTypeToString(FNodeType);
end;

procedure TASTNode.DoChildAdded(Child: TASTNode);
begin
  if Assigned(Child) then
    Child.FParent := Self;
end;

procedure TASTNode.DoChildRemoved(Child: TASTNode);
begin
  if Assigned(Child) then
    Child.FParent := nil;
end;

function TASTNode.AddChild(Child: TASTNode): Integer;
begin
  Result := FChildren.Add(Child);
  DoChildAdded(Child);
end;

function TASTNode.InsertChild(Index: Integer; Child: TASTNode): Integer;
begin
  FChildren.Insert(Index, Child);
  DoChildAdded(Child);
  Result := Index;
end;

function TASTNode.RemoveChild(Child: TASTNode): Boolean;
var
  Index: Integer;
begin
  Index := FChildren.IndexOf(Child);
  Result := Index >= 0;
  if Result then
  begin
    DoChildRemoved(Child);
    FChildren.Delete(Index);
  end;
end;

function TASTNode.RemoveChildAt(Index: Integer): Boolean;
var
  AChild: TASTNode;
begin
  Result := (Index >= 0) and (Index < FChildren.Count);
  if Result then
  begin
    AChild := TASTNode(FChildren[Index]);
    DoChildRemoved(AChild);
    FChildren.Delete(Index);
  end;
end;

procedure TASTNode.ClearChildren;
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    DoChildRemoved(TASTNode(FChildren[i]));
  FChildren.Clear;
end;

function TASTNode.GetChild(Index: Integer): TASTNode;
begin
  if (Index >= 0) and (Index < FChildren.Count) then
    Result := TASTNode(FChildren[Index])
  else
    Result := nil;
end;

function TASTNode.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TASTNode.GetFirstChild: TASTNode;
begin
  if FChildren.Count > 0 then
    Result := TASTNode(FChildren[0])
  else
    Result := nil;
end;

function TASTNode.GetLastChild: TASTNode;
begin
  if FChildren.Count > 0 then
    Result := TASTNode(FChildren[FChildren.Count - 1])
  else
    Result := nil;
end;

function TASTNode.GetNextSibling: TASTNode;
var
  Index: Integer;
begin
  Result := nil;
  if Assigned(FParent) then
  begin
    Index := FParent.FChildren.IndexOf(Self);
    if (Index >= 0) and (Index < FParent.FChildren.Count - 1) then
      Result := TASTNode(FParent.FChildren[Index + 1]);
  end;
end;

function TASTNode.GetPreviousSibling: TASTNode;
var
  Index: Integer;
begin
  Result := nil;
  if Assigned(FParent) then
  begin
    Index := FParent.FChildren.IndexOf(Self);
    if Index > 0 then
      Result := TASTNode(FParent.FChildren[Index - 1]);
  end;
end;

function TASTNode.GetRoot: TASTNode;
begin
  Result := Self;
  while Assigned(Result.FParent) do
    Result := Result.FParent;
end;

function TASTNode.GetDepth: Integer;
begin
  Result := 0;
  if Assigned(FParent) then
    Result := FParent.GetDepth + 1;
end;

function TASTNode.FindChild(NodeType: TASTNodeType): TASTNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FChildren.Count - 1 do
  begin
    if TASTNode(FChildren[i]).NodeType = NodeType then
    begin
      Result := TASTNode(FChildren[i]);
      Break;
    end;
  end;
end;

function TASTNode.FindChildren(NodeType: TASTNodeType): TFPObjectList;
var
  i: Integer;
  AChild: TASTNode;
begin
  Result := TFPObjectList.Create(False); // Don't own the children
  for i := 0 to FChildren.Count - 1 do
  begin
    AChild := TASTNode(FChildren[i]);
    if AChild.NodeType = NodeType then
      Result.Add(AChild);
  end;
end;

function TASTNode.FindDescendant(NodeType: TASTNodeType): TASTNode;
var
  i: Integer;
  AChild: TASTNode;
begin
  // First check direct children
  Result := FindChild(NodeType);
  if Assigned(Result) then
    Exit;

  // Then check descendants
  for i := 0 to FChildren.Count - 1 do
  begin
    AChild := TASTNode(FChildren[i]);
    Result := AChild.FindDescendant(NodeType);
    if Assigned(Result) then
      Break;
  end;
end;

function TASTNode.FindDescendants(NodeType: TASTNodeType): TFPObjectList;
var
  i, j: Integer;
  AChild: TASTNode;
  ChildResults: TFPObjectList;
begin
  Result := TFPObjectList.Create(False);

  // Add direct children of matching type
  for i := 0 to FChildren.Count - 1 do
  begin
    AChild := TASTNode(FChildren[i]);
    if AChild.NodeType = NodeType then
      Result.Add(AChild);
  end;

  // Add descendants
  for i := 0 to FChildren.Count - 1 do
  begin
    AChild := TASTNode(FChildren[i]);
    ChildResults := AChild.FindDescendants(NodeType);
    try
      // Manually add all items from ChildResults to Result
      for j := 0 to ChildResults.Count - 1 do
        Result.Add(ChildResults[j]);
    finally
      ChildResults.Free;
    end;
  end;
end;

function TASTNode.Clone: TASTNode;
var
  i: Integer;
  ChildClone: TASTNode;
begin
  Result := TASTNode.CreateWithValue(FNodeType, FValue, FToken);
  Result.FSourceLine := FSourceLine;
  Result.FSourceColumn := FSourceColumn;
  Result.FAttributes.Assign(FAttributes);

  // Clone children
  for i := 0 to FChildren.Count - 1 do
  begin
    ChildClone := TASTNode(FChildren[i]).Clone;
    Result.AddChild(ChildClone);
  end;
end;

function TASTNode.IsLeaf: Boolean;
begin
  Result := FChildren.Count = 0;
end;

function TASTNode.IsRoot: Boolean;
begin
  Result := FParent = nil;
end;

function TASTNode.HasChildren: Boolean;
begin
  Result := FChildren.Count > 0;
end;

function TASTNode.ContainsNodeType(NodeType: TASTNodeType): Boolean;
begin
  Result := Assigned(FindDescendant(NodeType));
end;

function TASTNode.Validate: Boolean;
begin
  // Base validation - override in subclasses
  Result := True;
end;

function TASTNode.GetValidationErrors: TStringList;
begin
  Result := TStringList.Create;
  if not Validate then
    Result.Add(Format('Node validation failed for %s', [NodeTypeToString(FNodeType)]));
end;

function TASTNode.ToString: string;
begin
  Result := GetDisplayString;
end;

function TASTNode.GetDisplayString: string;
begin
  if not FDisplayStringValid then
  begin
    FDisplayStringCached := BuildDisplayString;
    FDisplayStringValid := True;
  end;
  Result := FDisplayStringCached;
end;

function TASTNode.ToDebugString(Indent: Integer): string;
var
  i: Integer;
  IndentStr: string;
  AChild: TASTNode;
  ValueStr: string;
begin
  IndentStr := StringOfChar(' ', Indent * 2);

  if not VarIsEmpty(FValue) then
    ValueStr := Format(' = %s', [VarToStr(FValue)])
  else
    ValueStr := '';

  Result := Format('%s%s%s [Line: %d, Col: %d]',
    [IndentStr, NodeTypeToString(FNodeType), ValueStr, FSourceLine, FSourceColumn]);

  if Assigned(FToken) then
    Result := Result + Format(' (Token: %s)', [FToken.Value]);

  Result := Result + sLineBreak;

  for i := 0 to FChildren.Count - 1 do
  begin
    AChild := TASTNode(FChildren[i]);
    Result := Result + AChild.ToDebugString(Indent + 1);
  end;
end;

procedure TASTNode.Accept(Visitor: TObject);
begin
  // Placeholder for visitor pattern - implement in specialized visitors
end;

// === AST NODE CREATION HELPERS ===

function CreateLiteralNode(const Value: Variant; Token: TLexerToken): TASTNode;
begin
  Result := TASTNode.CreateWithValue(antLiteral, Value, Token);
end;

function CreateIdentifierNode(const Name: string; Token: TLexerToken): TASTNode;
begin
  Result := TASTNode.CreateWithValue(antIdentifier, Name, Token);
end;

// === CONSTANT FOLDING ===

function IsConstantNode(Node: TASTNode): Boolean;
begin
  Result := Assigned(Node) and (Node.NodeType = antLiteral);
end;

function TryFoldBinaryOp(OpType: TTokenType; Left, Right: TASTNode; Token: TLexerToken; out FoldedNode: TASTNode): Boolean;
var
  LeftVal, RightVal, FoldedVal: Variant;
  LeftNum, RightNum: Extended;
  LeftInt, RightInt: Int64;
  IsInteger: Boolean;
  OpToken: TTokenType;
begin
  Result := False;
  FoldedNode := nil;
  
  // Both operands must be constant literals
  if not (IsConstantNode(Left) and IsConstantNode(Right)) then
    Exit;
  
  LeftVal := Left.Value;
  RightVal := Right.Value;
  
  // Try to extract numeric values
  try
    // Check if both are integers for integer operations
    IsInteger := VarIsOrdinal(LeftVal) and VarIsOrdinal(RightVal);
    
    if IsInteger then
    begin
      LeftInt := LeftVal;
      RightInt := RightVal;
    end
    else
    begin
      LeftNum := LeftVal;
      RightNum := RightVal;
    end;
    
    // Extract token type
    OpToken := OpType;
    
    // Fold based on operator type
    case OpToken of
      ttOpAdd:  // +
        if IsInteger then
          FoldedVal := LeftInt + RightInt
        else
          FoldedVal := LeftNum + RightNum;
          
      ttOpSub:  // -
        if IsInteger then
          FoldedVal := LeftInt - RightInt
        else
          FoldedVal := LeftNum - RightNum;
          
      ttOpMul:  // *
        if IsInteger then
          FoldedVal := LeftInt * RightInt
        else
          FoldedVal := LeftNum * RightNum;
          
      ttOpDiv:  // /
        begin
          if RightNum = 0 then
            Exit; // Don't fold division by zero
          FoldedVal := LeftNum / RightNum;
        end;
        
      ttOpMod:  // MOD
        begin
          if RightInt = 0 then
            Exit;
          FoldedVal := LeftInt mod RightInt;
        end;
        
      ttOpPow:  // ^
        FoldedVal := Power(LeftNum, RightNum);
        
      ttOpEq:  // =
        FoldedVal := Ord(LeftVal = RightVal);
        
      ttOpNeq:  // <> or !=
        FoldedVal := Ord(LeftVal <> RightVal);
        
      ttOpLt:  // <
        FoldedVal := Ord(LeftNum < RightNum);
        
      ttOpGt:  // >
        FoldedVal := Ord(LeftNum > RightNum);
        
      ttOpLe:  // <=
        FoldedVal := Ord(LeftNum <= RightNum);
        
      ttOpGe:  // >=
        FoldedVal := Ord(LeftNum >= RightNum);
    else
      Exit; // Unsupported operator
    end;
    
    // Create folded literal node
    FoldedNode := CreateLiteralNode(FoldedVal, Token);
    Result := True;
  except
    // If any error occurs during folding, don't fold
    FoldedNode := nil;
    Result := False;
  end;
end;

function CreateBinaryOpNode(OpType: TTokenType; Left, Right: TASTNode; Token: TLexerToken): TASTNode;
var
  FoldedNode: TASTNode;
begin
  // CONSTANT FOLDING DISABLED - causes performance degradation
  // The folded node with children creates a more complex AST that slows execution
  // and increases statement count dramatically (75496 vs 20004)
  (*
  // Try constant folding first
  if TryFoldBinaryOp(OpType, Left, Right, Token, FoldedNode) then
  begin
    // Folding succeeded - return the folded literal node
    Result := FoldedNode;
    Exit;
  end;
  *)
  
  // Create normal binary op node
  Result := TASTNode.CreateWithValue(antBinaryOp, Ord(OpType), Token);
  Result.AddChild(Left);
  Result.AddChild(Right);
end;

function CreateUnaryOpNode(OpType: TTokenType; Operand: TASTNode; Token: TLexerToken): TASTNode;
begin
  Result := TASTNode.CreateWithValue(antUnaryOp, Ord(OpType), Token);
  Result.AddChild(Operand);
end;

function CreateFunctionCallNode(const FuncName: string; Args: TASTNode; Token: TLexerToken): TASTNode;
begin
  Result := TASTNode.CreateWithValue(antFunctionCall, FuncName, Token);
  if Assigned(Args) then
    Result.AddChild(Args);
end;

end.

