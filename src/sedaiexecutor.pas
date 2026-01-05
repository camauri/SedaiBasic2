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
unit SedaiExecutor;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

// aggressive compilation options
{$OPTIMIZATION LEVEL4}
{$OPTIMIZATION PEEPHOLE}
{$OPTIMIZATION REGVAR}
{$R-} {$Q-} {$I-}

interface

uses
  Classes, SysUtils, fgl, Contnrs, Variants, Math,
  SedaiLexerTypes, SedaiLexerToken, SedaiLexerFSM, SedaiTokenList,
  SedaiParserTypes, SedaiParserResults, SedaiPackratParser, SedaiAST,
  SedaiExecutorTypes, SedaiExecutorErrors,
  SedaiExecutionResult, SedaiExecutorContext, SedaiProgramMemory,
  SedaiOutputInterface, SedaiDateTimeUtils;
  // ARCHITECTURE FIX: Removed SedaiSDL2GraphicsOutput dependency
  // Executor uses only IOutputDevice/IInputDevice interfaces for maximum flexibility

const
  // TIER 4: Magic numbers extracted to constants
  DEFAULT_MAX_BUFFER_LINES = 50;    // Buffer lines before flush
  MIN_BUFFER_LINES_GRAPHICS = 1;     // Force flush in graphics mode
  FLUSH_INTERVAL_MS = 16;            // ~60 FPS flush rate
  SLOW_STATEMENT_THRESHOLD_US = 100.0; // Log if statement > 100 microseconds
  DEFAULT_TAB_SPACING = 10;          // TAB spacing for PRINT comma
  MAX_GRAPHICS_MODE = 7;             // Valid graphics mode range: 0-6 (C128) + 7 (SDL2 dynamic)
  MAX_SPLIT_LINE = 25;               // Valid split line range: -1 to 25

type
  { TIER 4: Integer-keyed hash map for line numbers (faster than string keys) }
  TLineNumberMap = specialize TFPGMap<Integer, Integer>;

  { TForLoopContext - Loop state for optimization }
  TForLoopContext = class
  public
    VarName: string;
    StartValue: Variant;
    EndValue: Variant;
    StepValue: Variant;
    LoopStartPC: Integer;
    LoopEndPC: Integer;  // PC of NEXT statement
    // Performance cache
    EndValueFloat: Double;
    StepValueFloat: Double;
    CurrentValueFloat: Double;  // Cache current value to avoid GetVariable
    // Specialization flags
    IsSimpleLoop: Boolean;  // Can use fast path?
    constructor Create(const AVarName: string; AStart, AEnd, AStep: Variant; AStartPC: Integer);
  end;
  
  { TSedaiExecutor - Pure execution engine with correct I/O }
  TSedaiExecutor = class
  private
    FContext: TExecutorContext;
    FOutputDevice: IOutputDevice;
    FInputDevice: IInputDevice;
    FRunning: Boolean;
    FDebugMode: Boolean;
    FTraceMode: Boolean;
    FStepMode: Boolean;
    FCurrentLineNumber: Integer;
    FLastError: string;

    // Reference to program memory for LIST command
    FProgramMemory: TProgramMemory;

    // Execution stack for GOSUB/RETURN
    FGosubStack: TFPList;

    // FOR loop stack
    FForLoopStack: TFPList;

    // Program state
    FProgramAST: TASTNode;
    FProgramCounter: Integer;
    FStatementList: TFPObjectList;

    // TAB spacing for comma in PRINT
    FTabSpacing: Integer;

    // Base path for file operations
    FBasePath: string;

    // LineNumber -> StatementIndex (TIER 4: Integer keys, no IntToStr overhead)
    FLineNumberMap: TLineNumberMap;

    // === ADDED OPTIMIZATIONS ===

    // Output buffering to reduce SDL calls
    FOutputBuffer: TStringList;
    FBufferedOutput: Boolean;
    FMaxBufferLines: Integer;

    // Print formatting buffer to improve PRINT
    FPrintLineBuffer: string;
    FPrintCurrentCol: Integer;

    // Performance counters
    FStatementExecutions: Integer;
    FPrintCalls: Integer;
    FLastFlushTime: QWord;

    // Random number generation counter
    FRndCounter: Cardinal;

    // Built-in functions
    constructor Create(OutputDevice: IOutputDevice; InputDevice: IInputDevice;
      const BasePath: string);
    function EvaluateArrayAccess(Node: TASTNode): Variant;  // TIER 4: Split from EvaluateArrayOrFunction
    procedure RegisterBuiltinFunctions;

    // Expression evaluation
    function EvaluateExpression(ExprNode: TASTNode): Variant;
    function EvaluateBinaryOp(OpNode: TASTNode): Variant;
    function EvaluateUnaryOp(OpNode: TASTNode): Variant;
    function EvaluateFunctionCall(FuncNode: TASTNode): Variant;
    // TIER 4: EvaluateArrayAccess removed (never used, replaced by EvaluateArrayOrFunction)
    function EvaluateIdentifier(IdNode: TASTNode): Variant;
    
    // === QUICK WIN C: Type-specific evaluation helpers ===
    function DetectNodeType(Node: TASTNode): TBasicVariableType; inline;
    function EvaluateAsInteger(Node: TASTNode): Int64; inline;
    function EvaluateAsFloat(Node: TASTNode): Double; inline;
    function IsNumericType(VType: TBasicVariableType): Boolean; inline;

    // === TIER 4: Helper to eliminate duplication ===
    procedure ParseArgumentList(ArgList: TASTNode; out Args: TVariantDynArray);

    // Statement execution
    procedure ExecutePrintStatement(PrintNode: TASTNode);
    procedure ExecuteInputStatement(InputNode: TASTNode);
    procedure ExecuteAssignmentStatement(AssignNode: TASTNode);
    procedure ExecuteIfStatement(IfNode: TASTNode);
    procedure ExecuteForLoop(ForNode: TASTNode);
    procedure ExecuteWhileLoop(WhileNode: TASTNode);
    procedure ExecuteDoLoop(DoNode: TASTNode);
    procedure ExecuteGotoStatement(GotoNode: TASTNode);
    procedure ExecuteGosubStatement(GosubNode: TASTNode);
    procedure ExecuteGraphicsStatement(Node: TASTNode);
    procedure ExecuteReturnStatement(ReturnNode: TASTNode);
    procedure ExecuteRunStatement(RunNode: TASTNode);
    procedure ExecuteEndStatement(EndNode: TASTNode);
    procedure ExecuteRemStatement(RemNode: TASTNode);
    procedure ExecuteDimStatement(DimNode: TASTNode);
    procedure ExecuteDataStatement(DataNode: TASTNode);
    procedure ExecuteConstStatement(ConstNode: TASTNode);
    procedure ExecuteReadStatement(ReadNode: TASTNode);
    procedure ExecuteNextStatement(NextNode: TASTNode);
    procedure ExecuteListStatement(ListNode: TASTNode);
    procedure ExecuteStopStatement(StopNode: TASTNode);

    // File operations
    procedure ExecuteLoadStatement(LoadNode: TASTNode);
    function FindProgramFile(const FileName: string): string;
    function HasExplicitPath(const FileName: string): Boolean;

    // Flow control helpers
    function FindLineNumber(LineNum: Integer): Integer;
    procedure GotoLine(LineNum: Integer);
    procedure CallSubroutine(LineNum: Integer);
    procedure ReturnFromSubroutine;
    procedure SkipToNext(const VarName: string);
    
    // === TIER 1 OPTIMIZATION: Loop Specialization ===
    function FindMatchingNext(const VarName: string; StartPC: Integer): Integer;
    function IsSimpleForLoop(StartPC, EndPC: Integer): Boolean;
    procedure ExecuteSimpleForLoop(ForContext: TForLoopContext);

    // Built-in math functions
    function BuiltinAbs(const Args: array of Variant): Variant;
    function BuiltinInt(const Args: array of Variant): Variant;
    function BuiltinSqr(const Args: array of Variant): Variant;
    function BuiltinSin(const Args: array of Variant): Variant;
    function BuiltinCos(const Args: array of Variant): Variant;
    function BuiltinTan(const Args: array of Variant): Variant;
    function BuiltinRnd(const Args: array of Variant): Variant;
    function BuiltinSgn(const Args: array of Variant): Variant;

    // Built-in string functions
    function BuiltinLen(const Args: array of Variant): Variant;
    function BuiltinMid(const Args: array of Variant): Variant;
    function BuiltinLeft(const Args: array of Variant): Variant;
    function BuiltinRight(const Args: array of Variant): Variant;
    function BuiltinAsc(const Args: array of Variant): Variant;
    function BuiltinChr(const Args: array of Variant): Variant;
    function BuiltinSpc(const Args: array of Variant): Variant;
    function BuiltinStr(const Args: array of Variant): Variant;
    function BuiltinTab(const Args: array of Variant): Variant;
    function BuiltinVal(const Args: array of Variant): Variant;

    // Error handling
    procedure RaiseRuntimeError(const Message: string);
    procedure HandleRuntimeError(E: Exception);

    // Build the line number lookup table once
    procedure BuildLineNumberMap;

    // === ADDED OPTIMIZATION METHODS ===
    procedure InitializeOptimizedStructures;
    procedure FlushOutputBuffer;
    procedure AddToOutputBuffer(const Text: string; IsNewLine: Boolean = False);
    procedure OptimizedPrint(const Text: string; ClearBackground: Boolean = False);
    procedure OptimizedPrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure FlushPrintLine;
    procedure AddToPrintBuffer(const Text: string; const Separator: string = '');
    function FormatPrintValue(const Value: Variant): string;

  public
    constructor Create(OutputDevice: IOutputDevice; InputDevice: IInputDevice);
    destructor Destroy; override;

    // Main execution interface - only AST execution, no parsing
    function Execute(ProgramAST: TASTNode): TExecutionResult;
    function ExecuteStep: Boolean;
    procedure ExecuteStatement(StmtNode: TASTNode);
    procedure Stop;
    procedure Reset;

    // Program control
    procedure SetBreakpoint(LineNumber: Integer);
    procedure ClearBreakpoint(LineNumber: Integer);
    procedure ClearAllBreakpoints;

    // Debug interface
    procedure EnableDebugMode(Enabled: Boolean = True);
    procedure EnableTraceMode(Enabled: Boolean = True);
    procedure EnableStepMode(Enabled: Boolean = True);

    // Variable inspection
    function GetVariableValue(const VarName: string): Variant;
    procedure SetVariableValue(const VarName: string; const Value: Variant);
    function GetVariableList: TStringList;

    // I/O redirection
    procedure SetInputSource(Source: TStrings);
    procedure SetOutputTarget(Target: TStrings);

    // === PUBLIC OPTIMIZATIONS ===
    procedure EnableBufferedOutput(Enabled: Boolean = True);
    procedure SetBufferSize(MaxLines: Integer);

    // Properties
    property Context: TExecutorContext read FContext;
    property OutputDevice: IOutputDevice read FOutputDevice write FOutputDevice;
    property InputDevice: IInputDevice read FInputDevice write FInputDevice;
    property Running: Boolean read FRunning;
    property DebugMode: Boolean read FDebugMode;
    property TraceMode: Boolean read FTraceMode;
    property StepMode: Boolean read FStepMode;
    property CurrentLineNumber: Integer read FCurrentLineNumber;
    property LastError: string read FLastError;
    property ProgramMemory: TProgramMemory read FProgramMemory write FProgramMemory;

    property TabSpacing: Integer read FTabSpacing write FTabSpacing;

    property BasePath: string read FBasePath write FBasePath;

    // Performance properties
    property StatementExecutions: Integer read FStatementExecutions;
    property PrintCalls: Integer read FPrintCalls;
    property BufferedOutput: Boolean read FBufferedOutput;
  end;

implementation

uses
  StrUtils, DateUtils;

// Utility functions for type conversions
function VarToBool(const V: Variant): Boolean; inline;
begin
  // FAST PATH: direct access for common types
  case TVarData(V).VType of
    varBoolean: Result := TVarData(V).VBoolean;
    varInteger: Result := TVarData(V).VInteger <> 0;
    varSmallint: Result := TVarData(V).VSmallint <> 0;
    varByte: Result := TVarData(V).VByte <> 0;
    varDouble: Result := TVarData(V).VDouble <> 0.0;
    varSingle: Result := TVarData(V).VSingle <> 0.0;
    varNull, varEmpty: Result := False;
    else
      // SLOW PATH: for complex types
      if VarIsNull(V) then
        Result := False
      else if VarIsNumeric(V) then
        Result := Double(V) <> 0.0
      else if VarIsStr(V) then
        Result := VarToStr(V) <> ''
      else
        Result := Boolean(V);
  end;
end;

function VarToInt(const V: Variant): Integer; inline;
begin
  // FAST PATH: direct access for common types
  case TVarData(V).VType of
    varInteger: Result := TVarData(V).VInteger;
    varSmallint: Result := TVarData(V).VSmallint;
    varByte: Result := TVarData(V).VByte;
    varDouble: Result := Trunc(TVarData(V).VDouble);
    varSingle: Result := Trunc(TVarData(V).VSingle);
    varNull, varEmpty: Result := 0;
    else
      // SLOW PATH: for complex types
      if VarIsNull(V) then
        Result := 0
      else
        Result := Trunc(Double(V));
  end;
end;

function VarToFloat(const V: Variant): Double; inline;
begin
  // FAST PATH: direct access for common types
  case TVarData(V).VType of
    varDouble: Result := TVarData(V).VDouble;
    varInteger: Result := TVarData(V).VInteger;
    varSmallint: Result := TVarData(V).VSmallint;
    varByte: Result := TVarData(V).VByte;
    varSingle: Result := TVarData(V).VSingle;
    varNull, varEmpty: Result := 0.0;
    else
      // SLOW PATH: for complex types (strings, etc.)
      if VarIsNull(V) then
        Result := 0.0
      else if VarIsNumeric(V) then
        Result := Double(V)
      else if VarIsStr(V) then
      begin
        if not TryStrToFloat(VarToStr(V), Result) then
          Result := 0.0;
      end
      else
        Result := 0.0;
  end;
end;

{ TForLoopContext }

constructor TForLoopContext.Create(const AVarName: string; AStart, AEnd, AStep: Variant; AStartPC: Integer);
begin
  inherited Create;
  VarName := AVarName;
  StartValue := AStart;
  EndValue := AEnd;
  StepValue := AStep;
  LoopStartPC := AStartPC;
  // Pre-convert for performance
  EndValueFloat := VarToFloat(AEnd);
  StepValueFloat := VarToFloat(AStep);
  CurrentValueFloat := VarToFloat(AStart);
end;

{ TSedaiExecutor }

constructor TSedaiExecutor.Create(OutputDevice: IOutputDevice; InputDevice: IInputDevice);
begin
  inherited Create;

  if not Assigned(OutputDevice) then
    raise Exception.Create('OutputDevice cannot be nil');
  if not Assigned(InputDevice) then
    raise Exception.Create('InputDevice cannot be nil');

  // Default base path to current directory
  FBasePath := GetCurrentDir;

  FContext := TExecutorContext.Create;
  FOutputDevice := OutputDevice;
  FInputDevice := InputDevice;
  FGosubStack := TFPList.Create;
  FForLoopStack := TFPList.Create;
  FStatementList := TFPObjectList.Create(False);
  FRunning := False;
  FDebugMode := False;
  FTraceMode := False;
  FStepMode := False;
  FCurrentLineNumber := 0;
  FProgramCounter := 0;

  // Program memory reference is set externally
  FProgramMemory := nil;

  // Default print comma spacing, Commodore BASIC style
  FTabSpacing := DEFAULT_TAB_SPACING;

  // === OPTIMIZATION INITIALIZATION ===
  InitializeOptimizedStructures;

  // Initialize RND counter
  FRndCounter := 0;

  RegisterBuiltinFunctions;
end;

constructor TSedaiExecutor.Create(OutputDevice: IOutputDevice; InputDevice: IInputDevice; const BasePath: string);
begin
  // Call the main constructor
  Create(OutputDevice, InputDevice);

  // Set the custom base path
  if DirectoryExists(BasePath) then
    FBasePath := BasePath
  else
    FBasePath := GetCurrentDir;
end;

destructor TSedaiExecutor.Destroy;
var
  i: Integer;
begin
  for i := 0 to FForLoopStack.Count - 1 do
    TForLoopContext(FForLoopStack[i]).Free;
  FForLoopStack.Free;

  if Assigned(FLineNumberMap) then
    FLineNumberMap.Free;

  FStatementList.Free;
  FGosubStack.Free;

  // === OPTIMIZATION CLEANUP ===
  if Assigned(FOutputBuffer) then
    FOutputBuffer.Free;

  FContext.Free;

  inherited Destroy;
end;

// === OPTIMIZATION IMPLEMENTATION ===

procedure TSedaiExecutor.InitializeOptimizedStructures;
begin
  // Output buffering
  FOutputBuffer := TStringList.Create;
  FBufferedOutput := True;
  FMaxBufferLines := DEFAULT_MAX_BUFFER_LINES;  // Buffer lines before flush

  // Print line buffering
  FPrintLineBuffer := '';
  FPrintCurrentCol := 0;

  // Performance counters
  FStatementExecutions := 0;
  FPrintCalls := 0;
  FLastFlushTime := GetTickCount64;
end;

procedure TSedaiExecutor.FlushOutputBuffer;
var
  i: Integer;
begin
  if not Assigned(FOutputBuffer) or (FOutputBuffer.Count = 0) then Exit;

  // Send entire buffer to output device at once
  for i := 0 to FOutputBuffer.Count - 1 do
  begin
    if FOutputBuffer[i] = #1 then  // Marker for newline
      FOutputDevice.NewLine
    else
      FOutputDevice.Print(FOutputBuffer[i]);
  end;

  FOutputBuffer.Clear;
  FLastFlushTime := GetTickCount64;

  // Ensure it gets rendered
  FOutputDevice.Present;
end;

procedure TSedaiExecutor.AddToOutputBuffer(const Text: string; IsNewLine: Boolean = False);
begin
  if not FBufferedOutput then
  begin
    if IsNewLine then
      FOutputDevice.NewLine
    else
      FOutputDevice.Print(Text);
    Exit;
  end;

  if IsNewLine then
    FOutputBuffer.Add(#1)  // Special marker for newline
  else
    FOutputBuffer.Add(Text);

  // Auto-flush if buffer is full
  if FOutputBuffer.Count >= FMaxBufferLines then
    FlushOutputBuffer;
end;

procedure TSedaiExecutor.OptimizedPrint(const Text: string; ClearBackground: Boolean = False);
begin
  Inc(FPrintCalls);
  AddToOutputBuffer(Text, False);
end;

procedure TSedaiExecutor.OptimizedPrintLn(const Text: string; ClearBackground: Boolean = False);
begin
  Inc(FPrintCalls);
  AddToOutputBuffer(Text, False);
  AddToOutputBuffer('', True);  // Newline
end;

procedure TSedaiExecutor.FlushPrintLine;
begin
  if FPrintLineBuffer <> '' then
  begin
    AddToOutputBuffer(FPrintLineBuffer, False);
    FPrintLineBuffer := '';
    FPrintCurrentCol := 0;
  end;
end;

procedure TSedaiExecutor.AddToPrintBuffer(const Text: string; const Separator: string = '');
var
  TargetCol: Integer;
  SpacesToAdd: Integer;
begin
  if Separator = ',' then
  begin
    // Calculate next tab zone
    TargetCol := ((FPrintCurrentCol div FTabSpacing) + 1) * FTabSpacing;
    SpacesToAdd := TargetCol - FPrintCurrentCol;
    if SpacesToAdd > 0 then
    begin
      FPrintLineBuffer := FPrintLineBuffer + StringOfChar(' ', SpacesToAdd);
      FPrintCurrentCol := TargetCol;
    end;
  end;

  FPrintLineBuffer := FPrintLineBuffer + Text;
  FPrintCurrentCol := FPrintCurrentCol + Length(Text);
end;

function TSedaiExecutor.FormatPrintValue(const Value: Variant): string;
var
  FloatValue: Double;
  OldDecimalSeparator: Char;
begin
  // Commodore BASIC style number formatting (optimized)
  if VarIsNumeric(Value) then
  begin
    FloatValue := VarToFloat(Value);

    OldDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := '.';
    try
      Result := Format('%.8f', [FloatValue]);
      // Remove trailing zeros
      while (Length(Result) > 1) and (Result[Length(Result)] = '0') and (Pos('.', Result) > 0) do
        Delete(Result, Length(Result), 1);
      if (Length(Result) > 0) and (Result[Length(Result)] = '.') then
        Delete(Result, Length(Result), 1);
    finally
      DefaultFormatSettings.DecimalSeparator := OldDecimalSeparator;
    end;

    // Add leading space for positive numbers
    if FloatValue >= 0 then
      Result := ' ' + Result;
  end
  else
    Result := VarToStr(Value);
end;

procedure TSedaiExecutor.EnableBufferedOutput(Enabled: Boolean = True);
begin
  if not Enabled and FBufferedOutput then
    FlushOutputBuffer;
  FBufferedOutput := Enabled;
end;

procedure TSedaiExecutor.SetBufferSize(MaxLines: Integer);
begin
  FMaxBufferLines := MaxLines;
  if FMaxBufferLines < 1 then
    FMaxBufferLines := MIN_BUFFER_LINES_GRAPHICS;
end;

procedure TSedaiExecutor.RegisterBuiltinFunctions;
begin
  FContext.RegisterFunction('ABS', @BuiltinAbs);
  FContext.RegisterFunction('INT', @BuiltinInt);
  FContext.RegisterFunction('SQR', @BuiltinSqr);
  FContext.RegisterFunction('SIN', @BuiltinSin);
  FContext.RegisterFunction('COS', @BuiltinCos);
  FContext.RegisterFunction('TAN', @BuiltinTan);
  FContext.RegisterFunction('RND', @BuiltinRnd);
  FContext.RegisterFunction('SGN', @BuiltinSgn);

  FContext.RegisterFunction('LEN', @BuiltinLen);
  FContext.RegisterFunction('MID$', @BuiltinMid);
  FContext.RegisterFunction('LEFT$', @BuiltinLeft);
  FContext.RegisterFunction('RIGHT$', @BuiltinRight);
  FContext.RegisterFunction('ASC', @BuiltinAsc);
  FContext.RegisterFunction('CHR$', @BuiltinChr);
  FContext.RegisterFunction('SPC', @BuiltinSpc);
  FContext.RegisterFunction('STR$', @BuiltinStr);
  FContext.RegisterFunction('TAB', @BuiltinTab);
  FContext.RegisterFunction('VAL', @BuiltinVal);
end;

function TSedaiExecutor.Execute(ProgramAST: TASTNode): TExecutionResult;
var
  i: Integer;
  Timer: THiResTimer;
  PreviousPC: Integer;
  ElapsedMs: Double;
  CurrentStatement: TASTNode;
begin
  Result := TExecutionResult.Create;

  // *** PRECISE TIMING WITH THiResTimer ***
  Timer := CreateHiResTimer;  // Starts automatically

  try
    if not Assigned(ProgramAST) then
    begin
      Result.Success := False;
      Result.ErrorMessage := 'No program to execute';
      Exit;
    end;

    FProgramAST := ProgramAST;
    FRunning := True;
    FProgramCounter := 0;
    FCurrentLineNumber := 0;

    // Reset performance counters
    FStatementExecutions := 0;
    FPrintCalls := 0;

    // Build the statement list from the AST
    FStatementList.Clear;
    for i := 0 to ProgramAST.ChildCount - 1 do
      FStatementList.Add(ProgramAST.Child[i]);

    // Build the line number map
    BuildLineNumberMap;

    // CORRECT EXECUTION WITH PROGRAM COUNTER
    while FRunning and (FProgramCounter < FStatementList.Count) do
    begin
      if FOutputDevice.ShouldQuit or FInputDevice.ShouldQuit then
      begin
        FRunning := False;
        Break;
      end;

      try
        PreviousPC := FProgramCounter;

        if FStepMode then
        begin
          if not ExecuteStep then
            Break;
        end
        else
        begin
          // *** FIX: Skip antProgram nodes to avoid double execution ***
          CurrentStatement := TASTNode(FStatementList[FProgramCounter]);
          if Assigned(CurrentStatement) and (CurrentStatement.NodeType = antProgram) then
          begin
            // Skip Program nodes - they're just containers
            Inc(FProgramCounter);
            Continue;
          end;

          ExecuteStatement(TASTNode(FStatementList[FProgramCounter]));

          // Only increment PC if it hasn't changed (e.g. from GOTO)
          if FProgramCounter = PreviousPC then
            Inc(FProgramCounter);
        end;

        // === PERIODIC FLUSH FOR PERFORMANCE ===
        if (GetTickCount64 - FLastFlushTime) > FLUSH_INTERVAL_MS then  // ~60 FPS
          FlushOutputBuffer;

      except
        on E: TExecutorBreakException do
          Break;
        on E: TExecutorStopException do
        begin
          FRunning := False;
          Break;
        end;
        on E: Exception do
        begin
          HandleRuntimeError(E);
          Result.Success := False;
          Result.ErrorMessage := E.Message;
          OptimizedPrintLn('ERROR: ' + E.Message);
          Break;
        end;
      end;
    end;

    // === FINAL FLUSH ===
    FlushOutputBuffer;

    // *** CALCULATE EXECUTION TIME WITH HIGH PRECISION ***
    // TIER 4: Stop timer IMMEDIATELY after execution for accurate measurements
    ElapsedMs := Timer.ElapsedMilliseconds;

    // Store results FIRST (no overhead in timing)
    Result.Success := True;
    Result.ExecutionTime := Round(ElapsedMs);  // In milliseconds
    Result.StatementsExecuted := FStatementExecutions;

    // *** BENCHMARK OUTPUT (AFTER timing, doesn't affect measurements) ***
    WriteLn('');
    WriteLn('=== EXECUTION BENCHMARK ===');
    WriteLn('Execution completed in ', ElapsedMs:0:3, ' ms');
    WriteLn('Statements executed: ', FStatementExecutions);
    if FStatementExecutions > 0 then
      WriteLn('Average per statement: ', (ElapsedMs / FStatementExecutions * 1000):0:6, ' μs');
    if ElapsedMs > 1000 then
      WriteLn('(', Timer.ElapsedSeconds:0:2, ' seconds)')
    else if ElapsedMs < 1 then
      WriteLn('(', Timer.ElapsedMicroseconds:0:0, ' microseconds)');
    WriteLn('============================');

    FOutputDevice.Present;

  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := E.Message;
      HandleRuntimeError(E);
      OptimizedPrintLn('FATAL ERROR: ' + E.Message);
    end;
  end;

  FRunning := False;
end;

procedure TSedaiExecutor.ExecuteInputStatement(InputNode: TASTNode);
var
  i: Integer;
  VarName: string;
  Value: string;
  NumValue: Double;
  Child: TASTNode;
  PromptMessage: string;
  VariableNodes: array of TASTNode;
  VarCount: Integer;
begin
  // === FLUSH BUFFER BEFORE INPUT ===
  FlushOutputBuffer;

  PromptMessage := '';
  VarCount := 0;
  SetLength(VariableNodes, InputNode.ChildCount);

  for i := 0 to InputNode.ChildCount - 1 do
  begin
    Child := InputNode.Child[i];
    if Assigned(Child) then
    begin
      if Child.NodeType = antLiteral then
        PromptMessage := VarToStr(Child.Value)
      else if Child.NodeType = antIdentifier then
      begin
        VariableNodes[VarCount] := Child;
        Inc(VarCount);
      end;
    end;
  end;

  for i := 0 to VarCount - 1 do
  begin
    Child := VariableNodes[i];
    VarName := VarToStr(Child.Value);

    // Determine variable type from suffix
    if (Length(VarName) > 0) then
    begin
      case VarName[Length(VarName)] of
        '$':  // String variable
          begin
            if PromptMessage <> '' then
            begin
              Value := FInputDevice.ReadLine(PromptMessage + '? ', False, False, True);
              PromptMessage := '';
            end
            else
              Value := FInputDevice.ReadLine('? ', False, False, True);
            
            FContext.SetVariable(VarName, Value);
          end;

        '%':  // Integer variable (only whole numbers, no decimals)
          begin
            if PromptMessage <> '' then
            begin
              Value := FInputDevice.ReadLine(PromptMessage + '? ', False, True, False);  // NumericOnly=True, AllowDecimal=False
              PromptMessage := '';
            end
            else
              Value := FInputDevice.ReadLine('? ', False, True, False);

            if TryStrToFloat(Value, NumValue) then
              FContext.SetVariable(VarName, Trunc(NumValue))  // Truncate to integer
            else
              FContext.SetVariable(VarName, 0);
          end;

        else  // Numeric variable (float/double): '!', '#' or no suffix
          begin
            if PromptMessage <> '' then
            begin
              Value := FInputDevice.ReadLine(PromptMessage + '? ', False, True, True);  // NumericOnly=True, AllowDecimal=True
              PromptMessage := '';
            end
            else
              Value := FInputDevice.ReadLine('? ', False, True, True);

            if TryStrToFloat(Value, NumValue) then
              FContext.SetVariable(VarName, NumValue)
            else
              FContext.SetVariable(VarName, 0);
          end;
      end;
    end;
  end;
end;

procedure TSedaiExecutor.ExecuteStatement(StmtNode: TASTNode);
var
  StatementTimer: THiResTimer;
  StatementTime: Double;
  i: Integer;
  Child: TASTNode;
begin
  if not Assigned(StmtNode) then Exit;

  // === INCREMENT PERFORMANCE COUNTER ===
  case StmtNode.NodeType of
    antForLoop, antAssignment, antPrint, antInput, antIf, antGoto,
    antGosub, antReturn, antNext, antDim, antEnd, antStop:
      Inc(FStatementExecutions);  // Only these!
  end;

  // *** PROFILING FOR SLOW STATEMENTS (only if debug active) ***
  if FDebugMode then
    StatementTimer := CreateHiResTimer;

  // === ORIGINAL ExecuteStatement CODE ===
  if StmtNode.NodeType = antProgram then
  begin
    for i := 0 to StmtNode.ChildCount - 1 do
    begin
      Child := StmtNode.Child[i];
      if Assigned(Child) then
        ExecuteStatement(Child);
    end;
    Exit;
  end;

  if StmtNode.NodeType = antLineNumber then
  begin
    FCurrentLineNumber := VarToInt(StmtNode.Value);
    Exit;
  end;

  if (StmtNode.NodeType = antThen) or (StmtNode.NodeType = antElse) then
  begin
    for i := 0 to StmtNode.ChildCount - 1 do
    begin
      Child := StmtNode.Child[i];
      if Assigned(Child) then
        ExecuteStatement(Child);
    end;
    Exit;
  end;

  if FDebugMode and FContext.HasBreakpoint(FCurrentLineNumber) then
    raise TExecutorBreakException.CreateFmt('Breakpoint at line %d', [FCurrentLineNumber]);

  case StmtNode.NodeType of
    antPrint: ExecutePrintStatement(StmtNode);
    antInput: ExecuteInputStatement(StmtNode);
    antAssignment: ExecuteAssignmentStatement(StmtNode);
    antIf: ExecuteIfStatement(StmtNode);
    antForLoop: ExecuteForLoop(StmtNode);
    antWhileLoop: ExecuteWhileLoop(StmtNode);
    antDoLoop: ExecuteDoLoop(StmtNode);
    antGoto: ExecuteGotoStatement(StmtNode);
    antGosub: ExecuteGosubStatement(StmtNode);
    antReturn: ExecuteReturnStatement(StmtNode);
    antRun: ExecuteRunStatement(StmtNode);
    antEnd: ExecuteEndStatement(StmtNode);
    antStop: ExecuteStopStatement(StmtNode);
    antRem: ExecuteRemStatement(StmtNode);
    antDim: ExecuteDimStatement(StmtNode);
    antData: ExecuteDataStatement(StmtNode);
    antConst: ExecuteConstStatement(StmtNode);
    antRead: ExecuteReadStatement(StmtNode);
    antNext: ExecuteNextStatement(StmtNode);
    antList: ExecuteListStatement(StmtNode);
    antLoad: ExecuteLoadStatement(StmtNode);
    antGraphics: ExecuteGraphicsStatement(StmtNode);
  end;

  // *** PROFILING: show slow statements on normal console ***
  {$IFDEF DEBUG}
  if FDebugMode then
  begin
    StatementTime := StatementTimer.ElapsedMicroseconds;
    if StatementTime > SLOW_STATEMENT_THRESHOLD_US then  // Log slow statements
      WriteLn('SLOW STATEMENT: Line ', FCurrentLineNumber, ' took ', StatementTime:0:1, ' μs');
  end;
  {$ENDIF}
end;

procedure TSedaiExecutor.ExecutePrintStatement(PrintNode: TASTNode);
var
  i: Integer;
  Value: Variant;
  Child: TASTNode;
  ValueStr: string;
  Separator: string;
  HasTrailingSeparator: Boolean;
begin
  // === RESET PRINT LINE BUFFER ===
  FPrintLineBuffer := '';
  FPrintCurrentCol := 0;
  HasTrailingSeparator := False;

  for i := 0 to PrintNode.ChildCount - 1 do
  begin
    Child := PrintNode.Child[i];
    if Assigned(Child) then
    begin
      // If it's a separator
      if Child.NodeType = antSeparator then
      begin
        Separator := VarToStr(Child.Value);
        AddToPrintBuffer('', Separator);
        HasTrailingSeparator := True;
        Continue;
      end;

      HasTrailingSeparator := False;

      // Process the value
      Value := EvaluateExpression(Child);
      ValueStr := FormatPrintValue(Value);

      // Add to line buffer
      AddToPrintBuffer(ValueStr);
    end;
  end;

  // Flush current line
  FlushPrintLine;

  // If there's no trailing separator, go to new line
  if not HasTrailingSeparator then
    AddToOutputBuffer('', True);  // Newline
end;

procedure TSedaiExecutor.ExecuteAssignmentStatement(AssignNode: TASTNode);
var
  LeftSide: TASTNode;
  VarName: string;
  Value: Variant;
  IndicesNode: TASTNode;
  Indices: array of Integer;
  SingleIndex: Integer;  // TIER 3: Stack allocation for single index (no heap alloc!)
  i: Integer;
begin
  if AssignNode.ChildCount < 2 then
    RaiseRuntimeError('Invalid assignment statement');

  LeftSide := AssignNode.Child[0];
  Value := EvaluateExpression(AssignNode.Child[1]);

  case LeftSide.NodeType of
    antIdentifier:
    begin
      // Simple variable assignment
      VarName := VarToStr(LeftSide.Value);
      FContext.SetVariable(VarName, Value);
    end;

    antArrayAccess:
    begin
      // Array element assignment
      if LeftSide.ChildCount < 2 then
        RaiseRuntimeError('Invalid array assignment');

      VarName := VarToStr(LeftSide.Child[0].Value);
      IndicesNode := LeftSide.Child[1];

      // Handle both single expression and expression list
      if IndicesNode.NodeType = antExpressionList then
      begin
        // Multi-dimensional array - use dynamic array
        SetLength(Indices, IndicesNode.ChildCount);
        for i := 0 to IndicesNode.ChildCount - 1 do
          Indices[i] := VarToInt(EvaluateExpression(IndicesNode.Child[i]));

        // Set multi-dimensional array element
        try
          FContext.SetArrayElement(VarName, Indices, Value);
        except
          on E: Exception do
            RaiseRuntimeError(Format('Array assignment error for %s: %s', [VarName, E.Message]));
        end;
      end
      else
      begin
        // === TIER 3 OPTIMIZATION: Single index - NO dynamic array allocation! ===
        // Most common case in SIEVE: FLAGS(I) = 0 - thousands of times
        // Use stack-allocated variable instead of heap-allocated dynamic array
        SingleIndex := VarToInt(EvaluateExpression(IndicesNode));

        // Direct access with single index (bypasses array allocation overhead)
        try
          FContext.SetArrayElementFast(VarName, SingleIndex, Value);
        except
          on E: Exception do
            RaiseRuntimeError(Format('Array assignment error for %s: %s', [VarName, E.Message]));
        end;
      end;
    end;

    else
      RaiseRuntimeError('Left side of assignment must be a variable or array element');
  end;
end;

procedure TSedaiExecutor.ExecuteIfStatement(IfNode: TASTNode);
var
  Condition: Variant;
  i: Integer;
  Child: TASTNode;
begin
  if IfNode.ChildCount < 1 then
    RaiseRuntimeError('Invalid IF statement');

  Condition := EvaluateExpression(IfNode.Child[0]);

  if VarToBool(Condition) then
  begin
    // Look for the THEN node among children
    for i := 1 to IfNode.ChildCount - 1 do
    begin
      Child := IfNode.Child[i];
      if Assigned(Child) and (Child.NodeType = antThen) then
      begin
        ExecuteStatement(Child);
        Exit;
      end;
    end;
  end
  else
  begin
    // Look for the ELSE node among children
    for i := 1 to IfNode.ChildCount - 1 do
    begin
      Child := IfNode.Child[i];
      if Assigned(Child) and (Child.NodeType = antElse) then
      begin
        ExecuteStatement(Child);
        Exit;
      end;
    end;
  end;
end;

procedure TSedaiExecutor.ExecuteForLoop(ForNode: TASTNode);
var
  VarName: string;
  StartValue, EndValue, StepValue: Variant;
  ForContext: TForLoopContext;
  NextPC: Integer;
begin
  if ForNode.ChildCount < 3 then
    RaiseRuntimeError('Invalid FOR statement');

  if ForNode.Child[0].NodeType <> antIdentifier then
    RaiseRuntimeError('FOR variable must be an identifier');
  VarName := VarToStr(ForNode.Child[0].Value);

  StartValue := EvaluateExpression(ForNode.Child[1]);
  EndValue := EvaluateExpression(ForNode.Child[2]);

  if ForNode.ChildCount > 3 then
    StepValue := EvaluateExpression(ForNode.Child[3])
  else
    StepValue := 1;

  FContext.SetVariable(VarName, StartValue);

  ForContext := TForLoopContext.Create(VarName, StartValue, EndValue, StepValue, FProgramCounter + 1);
  
  // === TIER 1 OPTIMIZATION: Try to detect simple loop for fast path ===
  NextPC := FindMatchingNext(VarName, FProgramCounter + 1);
  if NextPC >= 0 then
  begin
    ForContext.LoopEndPC := NextPC;
    ForContext.IsSimpleLoop := IsSimpleForLoop(FProgramCounter + 1, NextPC);
  end
  else
  begin
    ForContext.LoopEndPC := -1;
    ForContext.IsSimpleLoop := False;
  end;
  
  FForLoopStack.Add(ForContext);

  // Check if loop should execute at all
  if ((ForContext.StepValueFloat > 0) and (VarToFloat(StartValue) > ForContext.EndValueFloat)) or
     ((ForContext.StepValueFloat < 0) and (VarToFloat(StartValue) < ForContext.EndValueFloat)) then
  begin
    // Empty loop - skip to NEXT
    SkipToNext(VarName);
    FForLoopStack.Delete(FForLoopStack.Count - 1);
    ForContext.Free;
  end
  else if ForContext.IsSimpleLoop then
  begin
    // === FAST PATH: Execute specialized loop ===
    ExecuteSimpleForLoop(ForContext);

    // Clean up
    FForLoopStack.Delete(FForLoopStack.Count - 1);
    ForContext.Free;
  end;
  // else: Normal path - will be handled by ExecuteNextStatement
end;

procedure TSedaiExecutor.ExecuteNextStatement(NextNode: TASTNode);
var
  VarName: string;
  ForContext: TForLoopContext;
  CurrentValueFloat, NewValueFloat: Double;
  ShouldContinue: Boolean;
begin
  if FForLoopStack.Count = 0 then
    RaiseRuntimeError('NEXT without FOR');

  if (NextNode.ChildCount > 0) and (NextNode.Child[0].NodeType = antIdentifier) then
    VarName := VarToStr(NextNode.Child[0].Value)
  else
    VarName := '';

  ForContext := TForLoopContext(FForLoopStack[FForLoopStack.Count - 1]);

  if (VarName <> '') and (VarName <> ForContext.VarName) then
    RaiseRuntimeError(Format('NEXT variable mismatch: expected %s, got %s', [ForContext.VarName, VarName]));

  // ULTRA-OPTIMIZED: Use local cache, completely avoid GetVariable!
  NewValueFloat := ForContext.CurrentValueFloat + ForContext.StepValueFloat;
  ForContext.CurrentValueFloat := NewValueFloat;  // Update cache
  FContext.SetVariable(ForContext.VarName, NewValueFloat);  // Synchronize variable

  // OPTIMIZED: Use cache for comparison (avoid conversions)
  if ForContext.StepValueFloat > 0 then
    ShouldContinue := NewValueFloat <= ForContext.EndValueFloat
  else
    ShouldContinue := NewValueFloat >= ForContext.EndValueFloat;

  if ShouldContinue then
  begin
    // Continue loop - return to beginning
    FProgramCounter := ForContext.LoopStartPC;
  end
  else
  begin
    // End loop - remove from stack
    ForContext.Free;
    FForLoopStack.Delete(FForLoopStack.Count - 1);
  end;
end;

procedure TSedaiExecutor.ExecuteWhileLoop(WhileNode: TASTNode);
var
  Condition: Variant;
  BodyStart: Integer;
begin
  if WhileNode.ChildCount < 2 then
    RaiseRuntimeError('WHILE requires condition and body');

  BodyStart := FProgramCounter;

  repeat
    Condition := EvaluateExpression(WhileNode.Child[0]);
    if VarToBool(Condition) then
    begin
      ExecuteStatement(WhileNode.Child[1]);
      FProgramCounter := BodyStart; // Loop back
    end
    else
      Break;
  until not FRunning;
end;

procedure TSedaiExecutor.ExecuteDoLoop(DoNode: TASTNode);
var
  Condition: Variant;
  BodyStart: Integer;
begin
  if DoNode.ChildCount < 2 then
    RaiseRuntimeError('DO-UNTIL requires body and condition');

  BodyStart := FProgramCounter;

  repeat
    ExecuteStatement(DoNode.Child[0]);
    Condition := EvaluateExpression(DoNode.Child[1]);
    if not VarToBool(Condition) then
      FProgramCounter := BodyStart // Loop back
    else
      Break;
  until not FRunning;
end;

procedure TSedaiExecutor.ExecuteGotoStatement(GotoNode: TASTNode);
var
  LineNum: Integer;
  TargetIndex: Integer;
begin
  if GotoNode.ChildCount < 1 then
    RaiseRuntimeError('GOTO requires a line number');

  LineNum := VarToInt(EvaluateExpression(GotoNode.Child[0]));

  // Use hash search O(1)
  TargetIndex := FindLineNumber(LineNum);
  if TargetIndex < 0 then
    RaiseRuntimeError(Format('Line number %d not found', [LineNum]));

  FProgramCounter := TargetIndex;
end;

procedure TSedaiExecutor.ExecuteGosubStatement(GosubNode: TASTNode);
var
  LineNum: Integer;
begin
  if GosubNode.ChildCount < 1 then
    RaiseRuntimeError('GOSUB requires a line number');

  LineNum := VarToInt(EvaluateExpression(GosubNode.Child[0]));
  CallSubroutine(LineNum);
end;

procedure TSedaiExecutor.ExecuteGraphicsStatement(Node: TASTNode);
var
  Mode: TGraphicMode;
  ModeValue, ClearValue, Param3Value: Variant;
  ModeInt: Integer;
  ClearBuffer: Boolean;
  Param3: Integer;  // Split line for modes 0-6, SDL2 mode index for mode 7
begin
  // Initialize defaults
  Mode := gm40ColText;
  ClearBuffer := True;
  Param3 := -1;  // -1 = use mode default

  // Parameter 1: Mode (required)
  // Modes 0-6: C128 compatible, mode 7: SDL2 dynamic (param3 = SDL2 mode index from GLIST)
  if Node.ChildCount > 0 then
  begin
    ModeValue := EvaluateExpression(Node.Child[0]);
    if not VarIsNull(ModeValue) then
    begin
      ModeInt := VarAsType(ModeValue, varInteger);
      if (ModeInt < 0) or (ModeInt > Ord(High(TGraphicMode))) then
        raise TExecutorException.CreateFmt('Graphics mode must be 0-%d', [Ord(High(TGraphicMode))]);
      Mode := TGraphicMode(ModeInt);
    end;
  end;

  // Parameter 2: Clear (optional)
  if Node.ChildCount > 1 then
  begin
    ClearValue := EvaluateExpression(Node.Child[1]);
    if not VarIsNull(ClearValue) then
      ClearBuffer := VarAsType(ClearValue, varInteger) <> 0;
  end;

  // Parameter 3: Split line (modes 0-6) or SDL2 mode index (mode 7)
  if Node.ChildCount > 2 then
  begin
    Param3Value := EvaluateExpression(Node.Child[2]);
    if not VarIsNull(Param3Value) then
    begin
      Param3 := VarAsType(Param3Value, varInteger);
      // For mode 7 (SDL2 dynamic), Param3 is SDL2 mode index from GLIST
      // For modes 0-6, Param3 is split line (-1 to MAX_SPLIT_LINE)
      if Mode <> gmSDL2Dynamic then
      begin
        if (Param3 < -1) or (Param3 > MAX_SPLIT_LINE) then
          raise TExecutorException.CreateFmt('Split line must be -1 to %d', [MAX_SPLIT_LINE]);
      end;
      // For SDL2 mode: Param3 validation will be done by SetGraphicMode
    end;
  end;

  // Apply graphics mode
  if not FOutputDevice.SetGraphicMode(Mode, ClearBuffer, Param3) then
    raise TExecutorException.Create('Failed to set graphics mode ' + IntToStr(Ord(Mode)));
end;

procedure TSedaiExecutor.ExecuteReturnStatement(ReturnNode: TASTNode);
begin
  ReturnFromSubroutine;
end;

procedure TSedaiExecutor.ExecuteRunStatement(RunNode: TASTNode);
var
  ProgramAST: TASTNode;
begin
  // Defensive logging to help track access violations originating from program execution
  if not Assigned(FProgramMemory) then
  begin
    OptimizedPrintLn('No program memory available');
    Exit;
  end;

  if FProgramMemory.GetLineCount = 0 then
  begin
    OptimizedPrintLn('No program to run');
    Exit;
  end;

  WriteLn('DEBUG: TSedaiExecutor.ExecuteRunStatement called. ProgramMemory assigned=',
    BoolToStr(Assigned(FProgramMemory), True), ' LineCount=', FProgramMemory.GetLineCount);

  try
    try
      // Build AST from program memory - may raise parse exceptions
      ProgramAST := FProgramMemory.GetOrBuildAST;
    except
      on E: Exception do
      begin
        WriteLn('ERROR: Exception while building program AST: ', E.ClassName, ' - ', E.Message);
        OptimizedPrintLn('?SYNTAX ERROR IN PROGRAM');
        Exit;
      end;
    end;

    if not Assigned(ProgramAST) then
    begin
      OptimizedPrintLn('Failed to parse program');
      Exit;
    end;

    // Reset and execute inside a protected block to catch runtime exceptions
    try
      Reset;
      Execute(ProgramAST);
    except
      on E: Exception do
      begin
        WriteLn('ERROR: Exception during program execution: ', E.ClassName, ' - ', E.Message);
        OptimizedPrintLn('?ERROR IN PROGRAM: ' + E.Message);
      end;
    end;

  except
    on E: Exception do
    begin
      WriteLn('FATAL: Unhandled exception in ExecuteRunStatement: ', E.ClassName, ' - ', E.Message);
      OptimizedPrintLn('FATAL ERROR: ' + E.Message);
    end;
  end;
end;

procedure TSedaiExecutor.ExecuteEndStatement(EndNode: TASTNode);
begin
  FRunning := False;
  FlushOutputBuffer;  // === FINAL FLUSH ===
end;

procedure TSedaiExecutor.ExecuteRemStatement(RemNode: TASTNode);
begin
  // REM statements do nothing in execution
  if FTraceMode then
  begin
    OptimizedPrintLn('REM: ' + VarToStr(RemNode.Value));
  end;
end;

procedure TSedaiExecutor.ExecuteDimStatement(DimNode: TASTNode);
var
  i, j: Integer;
  ArrayDecl: TASTNode;
  VarName: string;
  DimensionsNode: TASTNode;
  Dimensions: TArrayDimensions;
begin


  // Process each array declaration
  for i := 0 to DimNode.ChildCount - 1 do
  begin
    ArrayDecl := DimNode.Child[i];


    //if Assigned(ArrayDecl) then
    //  WriteLn('DEBUG: Child ', i, ' NodeType: ', NodeTypeToString(ArrayDecl.NodeType));

    if not Assigned(ArrayDecl) or (ArrayDecl.NodeType <> antArrayDecl) then
    begin

      Continue;
    end;



    if ArrayDecl.ChildCount < 2 then
    begin

      RaiseRuntimeError('Invalid array declaration in DIM statement');
      Continue;
    end;

    // Get variable name
    VarName := UpperCase(VarToStr(ArrayDecl.Child[0].Value));


    // Get dimensions
    DimensionsNode := ArrayDecl.Child[1];
    if not Assigned(DimensionsNode) or (DimensionsNode.NodeType <> antDimensions) then
    begin
      RaiseRuntimeError('Expected dimensions in array declaration');
      Continue;
    end;

    // Evaluate dimension expressions
    SetLength(Dimensions, DimensionsNode.ChildCount);
    for j := 0 to DimensionsNode.ChildCount - 1 do
    begin
      try
        Dimensions[j] := VarToInt(EvaluateExpression(DimensionsNode.Child[j]));
        if Dimensions[j] < 0 then
          RaiseRuntimeError(Format('Array dimension cannot be negative: %d', [Dimensions[j]]));
      except
        on E: Exception do
          RaiseRuntimeError(Format('Error evaluating array dimension: %s', [E.Message]));
      end;
    end;

    // Create the array
    try
      FContext.DimensionArray(VarName, Dimensions);








    except
      on E: Exception do
        RaiseRuntimeError(Format('Error creating array %s: %s', [VarName, E.Message]));
    end;



  end;
end;

procedure TSedaiExecutor.ExecuteDataStatement(DataNode: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  Value: Variant;
begin
  for i := 0 to DataNode.ChildCount - 1 do
  begin
    Child := DataNode.Child[i];
    if Assigned(Child) then
    begin
      Value := EvaluateExpression(Child);
      FContext.AddDataValue(Value);
    end;
  end;
end;

procedure TSedaiExecutor.ExecuteReadStatement(ReadNode: TASTNode);
var
  i: Integer;
  Child: TASTNode;
  VarName: string;
  Value: Variant;
begin
  for i := 0 to ReadNode.ChildCount - 1 do
  begin
    Child := ReadNode.Child[i];
    if Assigned(Child) and (Child.NodeType = antIdentifier) then
    begin
      VarName := VarToStr(Child.Value);
      Value := FContext.ReadDataValue;
      FContext.SetVariable(VarName, Value);
    end;
  end;
end;

procedure TSedaiExecutor.ExecuteListStatement(ListNode: TASTNode);
var
  i: Integer;
  LineNumbers: TList;
  LineNum: Integer;
  LineText: string;
  StartLine, EndLine: Integer;
  Output: string;
  Param1, Param2: string;
  DashPos: Integer;
begin
  if not Assigned(FProgramMemory) then
  begin
    OptimizedPrintLn('No program memory available');
    Exit;
  end;

  StartLine := 0;
  EndLine := MaxInt;

  // Handle LIST parameters
  if ListNode.ChildCount > 0 then
  begin
    // If there's a single parameter, it could be:
    // - A single number (LIST 100)
    // - A range with dash (LIST 10-50)
    if ListNode.ChildCount = 1 then
    begin
      Param1 := VarToStr(EvaluateExpression(ListNode.Child[0]));
      DashPos := Pos('-', Param1);

      if DashPos > 0 then
      begin
        // It's a range like "10-50"
        OptimizedPrintLn('?SYNTAX ERROR');
        Exit;
      end
      else
      begin
        // It's a single line number
        StartLine := VarToInt(EvaluateExpression(ListNode.Child[0]));
        EndLine := StartLine;
      end;
    end
    else if ListNode.ChildCount = 2 then
    begin
      // Two separate parameters: LIST 10, 50
      StartLine := VarToInt(EvaluateExpression(ListNode.Child[0]));
      EndLine := VarToInt(EvaluateExpression(ListNode.Child[1]));
    end
    else
    begin
      // Too many parameters
      OptimizedPrintLn('?SYNTAX ERROR');
      Exit;
    end;
  end;

  LineNumbers := TList.Create;
  try
    FProgramMemory.GetLineNumbers(LineNumbers);

    for i := 0 to LineNumbers.Count - 1 do
    begin
      LineNum := PtrInt(LineNumbers[i]);
      if (LineNum >= StartLine) and (LineNum <= EndLine) then
      begin
        LineText := FProgramMemory.GetLineText(LineNum);
        Output := IntToStr(LineNum) + ' ' + LineText;
        OptimizedPrintLn(Output);

        // TODO: Here we should implement scroll control
        // For now continue printing everything
      end;
    end;
  finally
    LineNumbers.Free;
  end;

  FlushOutputBuffer;  // === FLUSH FOR LIST ===
end;

procedure TSedaiExecutor.ExecuteStopStatement(StopNode: TASTNode);
begin
  OptimizedPrintLn('BREAK in line ' + IntToStr(FCurrentLineNumber));
  FlushOutputBuffer;
  raise TExecutorStopException.Create('STOP statement');
end;

procedure TSedaiExecutor.ExecuteLoadStatement(LoadNode: TASTNode);
var
  FileName: string;
  FullPath: string;
  FileContent: TStringList;
  i: Integer;
begin
  if LoadNode.ChildCount < 1 then
    RaiseRuntimeError('LOAD requires a filename');

  FileName := VarToStr(EvaluateExpression(LoadNode.Child[0]));

  // Remove quotes if present
  if (Length(FileName) >= 2) and (FileName[1] = '"') and (FileName[Length(FileName)] = '"') then
    FileName := Copy(FileName, 2, Length(FileName) - 2);

  // Find complete file path
  FullPath := FindProgramFile(FileName);

  if FullPath = '' then
  begin
    OptimizedPrintLn('?FILE NOT FOUND ERROR');
    Exit;
  end;

  // Load the file
  FileContent := TStringList.Create;
  try
    try
      FileContent.LoadFromFile(FullPath);

      // Reset the executor
      Reset;

      // Clear program memory if available
      if Assigned(FProgramMemory) then
        FProgramMemory.Clear;

      // Here you should integrate with the parser to load the program
      // For now just print load confirmation
      OptimizedPrintLn('READY.');

    except
      on E: Exception do
      begin
        OptimizedPrintLn('?LOAD ERROR: ' + E.Message);
      end;
    end;
  finally
    FileContent.Free;
  end;
end;

function TSedaiExecutor.FindProgramFile(const FileName: string): string;
var
  SearchPaths: array[0..2] of string;
  Extensions: array[0..1] of string;
  i, j: Integer;
  TestPath: string;
begin
  Result := '';

  // If the file already has an explicit path, use it directly
  if HasExplicitPath(FileName) then
  begin
    if FileExists(FileName) then
      Result := FileName;
    Exit;
  end;

  // Define search paths
  SearchPaths[0] := FBasePath;
  SearchPaths[1] := ConcatPaths([FBasePath, 'bas']);
  SearchPaths[2] := ConcatPaths([FBasePath, 'BAS']);

  // Define extensions to try
  Extensions[0] := '.bas';
  Extensions[1] := '.BAS';

  // First try the file name as-is in all paths
  for i := 0 to High(SearchPaths) do
  begin
    TestPath := ConcatPaths([SearchPaths[i], FileName]);
    if FileExists(TestPath) then
    begin
      Result := TestPath;
      Exit;
    end;
  end;

  // Then try adding extensions if there aren't any already
  if ExtractFileExt(FileName) = '' then
  begin
    for i := 0 to High(SearchPaths) do
    begin
      for j := 0 to High(Extensions) do
      begin
        TestPath := ConcatPaths([SearchPaths[i], FileName + Extensions[j]]);
        if FileExists(TestPath) then
        begin
          Result := TestPath;
          Exit;
        end;
      end;
    end;
  end;
end;

function TSedaiExecutor.HasExplicitPath(const FileName: string): Boolean;
begin
  // Check if the filename contains path separators
  Result := (Pos('\', FileName) > 0) or
            (Pos('/', FileName) > 0) or
            (Pos(':', FileName) > 0);  // For Windows drive letters
end;

function TSedaiExecutor.EvaluateExpression(ExprNode: TASTNode): Variant;
begin
  if not Assigned(ExprNode) then
  begin
    Result := Null;
    Exit;
  end;

  case ExprNode.NodeType of
    antLiteral: Result := ExprNode.Value;
    antIdentifier: Result := EvaluateIdentifier(ExprNode);
    antBinaryOp: Result := EvaluateBinaryOp(ExprNode);
    antUnaryOp: Result := EvaluateUnaryOp(ExprNode);

    // TIER 4 REFACTORING: Separate focused functions
    antFunctionCall: Result := EvaluateFunctionCall(ExprNode);
    antArrayAccess: Result := EvaluateArrayAccess(ExprNode);

    antParentheses:
      if ExprNode.ChildCount > 0 then
        Result := EvaluateExpression(ExprNode.Child[0])
      else
        Result := 0;
    else
      RaiseRuntimeError(Format('Cannot evaluate expression type: %s', [NodeTypeToString(ExprNode.NodeType)]));
  end;
end;

// TIER 4: Helper to eliminate code duplication
// Used by EvaluateArrayOrFunction and EvaluateFunctionCall
procedure TSedaiExecutor.ParseArgumentList(ArgList: TASTNode; out Args: TVariantDynArray);
var
  i: Integer;
begin
  SetLength(Args, 0);
  if Assigned(ArgList) then
  begin
    SetLength(Args, ArgList.ChildCount);
    for i := 0 to ArgList.ChildCount - 1 do
      Args[i] := EvaluateExpression(ArgList.Child[i]);
  end;
end;

// TIER 4 REFACTORING: Split EvaluateArrayOrFunction into two focused functions
// This handles ONLY array access (antArrayAccess)
// Function calls go through EvaluateFunctionCall
function TSedaiExecutor.EvaluateArrayAccess(Node: TASTNode): Variant;
var
  ArrayName: string;
  Args: array of Variant;
  Indices: array of Integer;
  SingleIndex: Integer;  // TIER 4: Stack allocation for single index
  i: Integer;
  ArgList: TASTNode;
begin
  // Extract array name from first child
  if (Node.ChildCount > 0) and Assigned(Node.Child[0]) then
    ArrayName := UpperCase(VarToStr(Node.Child[0].Value))
  else
  begin
    RaiseRuntimeError('Invalid array access structure');
    Exit;
  end;

  // Get argument list from second child
  if Node.ChildCount > 1 then
    ArgList := Node.Child[1]
  else
    ArgList := nil;

  // Parse indices
  ParseArgumentList(ArgList, Args);

  if Length(Args) = 0 then
    RaiseRuntimeError(Format('Array access requires at least one index: %s', [ArrayName]));

  // === TIER 4: SINGLE LOOKUP OPTIMIZATION ===
  if Length(Args) = 1 then
  begin
    // Single index - most common in SIEVE (FLAGS(I))
    // Single lookup - combines HasArray + GetArrayElementFast
    SingleIndex := VarToInt(Args[0]);
    if FContext.TryGetArrayElement(ArrayName, SingleIndex, Result) then
      Exit;
  end
  else
  begin
    // Multi-dimensional - rare case
    if FContext.HasArray(ArrayName) then
    begin
      SetLength(Indices, Length(Args));
      for i := 0 to Length(Args) - 1 do
        Indices[i] := VarToInt(Args[i]);
      Result := FContext.GetArrayElement(ArrayName, Indices);
      Exit;
    end;
  end;

  // Array not found
  RaiseRuntimeError(Format('Undeclared array: %s', [ArrayName]));
end;

function TSedaiExecutor.EvaluateBinaryOp(OpNode: TASTNode): Variant;
var
  Left, Right: Variant;
  OpType: TTokenType;
  LeftNode, RightNode: TASTNode;
  LeftType, RightType: TBasicVariableType;
  LeftInt, RightInt: Int64;
  LeftFloat, RightFloat: Double;
  UsedFastPath: Boolean;
begin
  if OpNode.ChildCount < 2 then
    RaiseRuntimeError('Binary operation requires two operands');

  LeftNode := OpNode.Child[0];
  RightNode := OpNode.Child[1];
  OpType := TTokenType(VarToInt(OpNode.Value));

  // === QUICK WIN C: Type-specific dispatch ===
  // Detect types BEFORE evaluation to avoid Variant overhead
  LeftType := DetectNodeType(LeftNode);
  RightType := DetectNodeType(RightNode);
  UsedFastPath := False;

  // FAST PATH 1: Both operands are integers (most common in SIEVE)
  if (LeftType = bvtInteger) and (RightType = bvtInteger) then
  begin
    LeftInt := EvaluateAsInteger(LeftNode);
    RightInt := EvaluateAsInteger(RightNode);

    case OpType of
      ttOpAdd:
      begin
        Result := LeftInt + RightInt;
        UsedFastPath := True;
      end;
      ttOpSub:
      begin
        Result := LeftInt - RightInt;
        UsedFastPath := True;
      end;
      ttOpMul:
      begin
        Result := LeftInt * RightInt;
        UsedFastPath := True;
      end;
      ttOpDiv:
      begin
        if RightInt = 0 then
          RaiseRuntimeError('Division by zero')
        else
          Result := LeftInt / RightInt; // Keep as float for division
        UsedFastPath := True;
      end;
      ttOpMod:
      begin
        if RightInt = 0 then
          RaiseRuntimeError('Division by zero in MOD operation')
        else
          Result := LeftInt mod RightInt;
        UsedFastPath := True;
      end;
      ttOpEq:
      begin
        Result := LeftInt = RightInt;
        UsedFastPath := True;
      end;
      ttOpNeq:
      begin
        Result := LeftInt <> RightInt;
        UsedFastPath := True;
      end;
      ttOpLt:
      begin
        Result := LeftInt < RightInt;
        UsedFastPath := True;
      end;
      ttOpGt:
      begin
        Result := LeftInt > RightInt;
        UsedFastPath := True;
      end;
      ttOpLe:
      begin
        Result := LeftInt <= RightInt;
        UsedFastPath := True;
      end;
      ttOpGe:
      begin
        Result := LeftInt >= RightInt;
        UsedFastPath := True;
      end;
    end;
    
    if UsedFastPath then
      Exit;
  end;

  // FAST PATH 2: Both operands are numeric (int or float)
  if IsNumericType(LeftType) and IsNumericType(RightType) then
  begin
    LeftFloat := EvaluateAsFloat(LeftNode);
    RightFloat := EvaluateAsFloat(RightNode);

    case OpType of
      ttOpAdd:
      begin
        Result := LeftFloat + RightFloat;
        UsedFastPath := True;
      end;
      ttOpSub:
      begin
        Result := LeftFloat - RightFloat;
        UsedFastPath := True;
      end;
      ttOpMul:
      begin
        Result := LeftFloat * RightFloat;
        UsedFastPath := True;
      end;
      ttOpDiv:
      begin
        if RightFloat = 0.0 then
          RaiseRuntimeError('Division by zero')
        else
          Result := LeftFloat / RightFloat;
        UsedFastPath := True;
      end;
      ttOpPow:
      begin
        Result := Power(LeftFloat, RightFloat);
        UsedFastPath := True;
      end;
      ttOpMod:
      begin
        if RightFloat = 0.0 then
          RaiseRuntimeError('Division by zero in MOD operation')
        else
          Result := Trunc(LeftFloat) mod Trunc(RightFloat);  // MOD truncates to integer
        UsedFastPath := True;
      end;
      ttOpEq:
      begin
        Result := LeftFloat = RightFloat;
        UsedFastPath := True;
      end;
      ttOpNeq:
      begin
        Result := LeftFloat <> RightFloat;
        UsedFastPath := True;
      end;
      ttOpLt:
      begin
        Result := LeftFloat < RightFloat;
        UsedFastPath := True;
      end;
      ttOpGt:
      begin
        Result := LeftFloat > RightFloat;
        UsedFastPath := True;
      end;
      ttOpLe:
      begin
        Result := LeftFloat <= RightFloat;
        UsedFastPath := True;
      end;
      ttOpGe:
      begin
        Result := LeftFloat >= RightFloat;
        UsedFastPath := True;
      end;
    end;
    
    if UsedFastPath then
      Exit;
  end;

  // SLOW PATH: Mixed types, strings, or complex expressions
  // Evaluate as Variant and use standard Variant operators
  Left := EvaluateExpression(LeftNode);
  Right := EvaluateExpression(RightNode);

  case OpType of
    ttOpAdd: Result := Left + Right;
    ttOpSub: Result := Left - Right;
    ttOpMul: Result := Left * Right;
    ttOpDiv:
      if VarToFloat(Right) = 0 then
        RaiseRuntimeError('Division by zero')
      else
        Result := VarToFloat(Left) / VarToFloat(Right);
    ttOpPow: Result := Power(VarToFloat(Left), VarToFloat(Right));
    ttOpMod:
      if VarToFloat(Right) = 0 then
        RaiseRuntimeError('Division by zero in MOD operation')
      else
        Result := Trunc(VarToFloat(Left)) mod Trunc(VarToFloat(Right));
    ttOpEq: Result := Left = Right;
    ttOpNeq: Result := Left <> Right;
    ttOpLt: Result := Left < Right;
    ttOpGt: Result := Left > Right;
    ttOpLe: Result := Left <= Right;
    ttOpGe: Result := Left >= Right;
    ttBitwiseAND: Result := VarToBool(Left) and VarToBool(Right);
    ttBitwiseOR: Result := VarToBool(Left) or VarToBool(Right);
    else
      RaiseRuntimeError(Format('Unknown binary operator: %d', [Ord(OpType)]));
  end;
end;

function TSedaiExecutor.EvaluateUnaryOp(OpNode: TASTNode): Variant;
var
  Operand: Variant;
  OpType: TTokenType;
begin
  if OpNode.ChildCount < 1 then
    RaiseRuntimeError('Unary operation requires one operand');

  Operand := EvaluateExpression(OpNode.Child[0]);
  OpType := TTokenType(VarToInt(OpNode.Value));

  case OpType of
    ttOpAdd: Result := Operand;
    ttOpSub: Result := -VarToFloat(Operand);
    ttBitwiseNOT: Result := not VarToBool(Operand);
    else
      RaiseRuntimeError(Format('Unknown unary operator: %d', [Ord(OpType)]));
  end;
end;

function TSedaiExecutor.EvaluateFunctionCall(FuncNode: TASTNode): Variant;
var
  FuncName: string;
  Args: array of Variant;
  ArgList: TASTNode;
begin
  FuncName := UpperCase(VarToStr(FuncNode.Value));

  // TIER 4 REFACTORING: Use ParseArgumentList helper
  if FuncNode.ChildCount > 0 then
    ArgList := FuncNode.Child[0]
  else
    ArgList := nil;

  ParseArgumentList(ArgList, Args);

  Result := FContext.CallFunction(FuncName, Args);
end;

function TSedaiExecutor.EvaluateIdentifier(IdNode: TASTNode): Variant;
var
  VarName: string;
  VarType: TBasicVariableType;
begin
  VarName := VarToStr(IdNode.Value);

  // === TIER 4 OPTIMIZATION: Single unified lookup (type + value) ===
  // Replaces TIER 1.5 double lookup (GetVariableType + GetVariableAsX)
  // Eliminates one hash lookup per variable access (~20,000+ in SIEVE)
  Result := FContext.GetVariableTyped(VarName, VarType);
end;

// === QUICK WIN C: Type-specific evaluation helpers ===

function TSedaiExecutor.IsNumericType(VType: TBasicVariableType): Boolean;
begin
  Result := VType in [bvtInteger, bvtSingle, bvtDouble, bvtNumber];
end;

function TSedaiExecutor.DetectNodeType(Node: TASTNode): TBasicVariableType;
var
  VarName: string;
begin
  if not Assigned(Node) then
  begin
    Result := bvtNumber; // Default to generic number for unknown
    Exit;
  end;

  case Node.NodeType of
    antLiteral:
    begin
      // Detect if it's an integer or float literal
      if VarIsType(Node.Value, varInteger) or VarIsType(Node.Value, varInt64) then
        Result := bvtInteger
      else if (VarType(Node.Value) in [varSingle, varDouble]) then
        Result := bvtDouble
      else if VarIsStr(Node.Value) then
        Result := bvtString
      else
        Result := bvtNumber;
    end;
    
    antIdentifier:
    begin
      VarName := VarToStr(Node.Value);
      Result := FContext.GetVariableType(VarName);
    end;
    
    antArrayAccess, antFunctionCall, antBinaryOp, antUnaryOp:
      Result := bvtNumber; // Unknown - requires evaluation, default to generic
    
    else
      Result := bvtNumber; // Default to generic number
  end;
end;

function TSedaiExecutor.EvaluateAsInteger(Node: TASTNode): Int64;
var
  VarName: string;
begin
  case Node.NodeType of
    antLiteral:
      Result := Int64(Node.Value);
    
    antIdentifier:
    begin
      VarName := VarToStr(Node.Value);
      Result := FContext.GetVariableAsInt(VarName);
    end;
    
    else
      // Fallback: evaluate as Variant then convert
      Result := Int64(EvaluateExpression(Node));
  end;
end;

function TSedaiExecutor.EvaluateAsFloat(Node: TASTNode): Double;
var
  VarName: string;
begin
  case Node.NodeType of
    antLiteral:
      Result := Double(Node.Value);
    
    antIdentifier:
    begin
      VarName := VarToStr(Node.Value);
      Result := FContext.GetVariableAsFloat(VarName);
    end;
    
    else
      // Fallback: evaluate as Variant then convert
      Result := Double(EvaluateExpression(Node));
  end;
end;

// === FLOW CONTROL ===
// TIER 4 REFACTORING: Direct integer lookup, no IntToStr overhead
function TSedaiExecutor.FindLineNumber(LineNum: Integer): Integer;
var
  Index: Integer;
begin
  if not Assigned(FLineNumberMap) then
  begin
    Result := -1;
    Exit;
  end;

  // Direct integer key lookup - much faster than string hash
  Index := FLineNumberMap.IndexOf(LineNum);
  if Index >= 0 then
    Result := FLineNumberMap.Data[Index]
  else
    Result := -1;
end;

procedure TSedaiExecutor.GotoLine(LineNum: Integer);
var
  Index: Integer;
begin
  Index := FindLineNumber(LineNum);
  if Index < 0 then
    RaiseRuntimeError(Format('Line number %d not found', [LineNum]));

  FProgramCounter := Index;
end;

procedure TSedaiExecutor.CallSubroutine(LineNum: Integer);
begin
  FGosubStack.Add(Pointer(FProgramCounter + 1));
  GotoLine(LineNum);
end;

procedure TSedaiExecutor.ReturnFromSubroutine;
begin
  if FGosubStack.Count = 0 then
    RaiseRuntimeError('RETURN without GOSUB');

  FProgramCounter := PtrInt(FGosubStack[FGosubStack.Count - 1]);
  FGosubStack.Delete(FGosubStack.Count - 1);
end;

procedure TSedaiExecutor.SkipToNext(const VarName: string);
var
  i: Integer;
  Statement: TASTNode;
  ForCount: Integer;
begin
  ForCount := 0;
  for i := FProgramCounter + 1 to FStatementList.Count - 1 do
  begin
    Statement := TASTNode(FStatementList[i]);
    if Statement.NodeType = antForLoop then
      Inc(ForCount)
    else if Statement.NodeType = antNext then
    begin
      if ForCount = 0 then
      begin
        FProgramCounter := i;
        Exit;
      end;
      Dec(ForCount);
    end;
  end;
  RaiseRuntimeError('FOR without NEXT');
end;

// Built-in functions placeholders
function TSedaiExecutor.BuiltinAbs(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('ABS requires exactly one argument');
  Result := Abs(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinInt(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('INT requires exactly one argument');
  Result := Floor(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinSqr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('SQR requires exactly one argument');
  Result := Sqrt(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinSin(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('SIN requires exactly one argument');
  Result := Sin(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinCos(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('COS requires exactly one argument');
  Result := Cos(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinTan(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('TAN requires exactly one argument');
  Result := Tan(VarToFloat(Args[0]));
end;

function TSedaiExecutor.BuiltinRnd(const Args: array of Variant): Variant;
var
  x, y, s: Double;
begin
  case Length(Args) of
    1: begin
      // RND(x): returns float [0.0, 1.0)
      // If x <> 0: set RandSeed using x + counter
      // If x = 0: continue sequence
      x := VarToFloat(Args[0]);
      if x <> 0 then
      begin
        Inc(FRndCounter);
        if FRndCounter > High(Cardinal) - 1 then
          FRndCounter := 0;
        RandSeed := Trunc(Abs(x)) + Integer(FRndCounter);
      end;
      Result := Random;
    end;

    2: begin
      // RND(x, y): returns Int64 in range [x, y)
      x := VarToFloat(Args[0]);  // min
      y := VarToFloat(Args[1]);  // max

      Inc(FRndCounter);
      if FRndCounter > High(Cardinal) - 1 then
        FRndCounter := 0;
      RandSeed := Integer(FRndCounter);

      // If x > y, use Int64.MaxValue as limit
      if x > y then
        y := High(Int64);

      Result := RandomRange(Trunc(x), Trunc(y));
    end;

    3: begin
      // RND(x, y, s): returns Int64 in range [x, y)
      // If s <> 0: use s + counter as seed
      x := VarToFloat(Args[0]);  // min
      y := VarToFloat(Args[1]);  // max
      s := VarToFloat(Args[2]);  // seed modifier

      Inc(FRndCounter);
      if FRndCounter > High(Cardinal) - 1 then
        FRndCounter := 0;
      if s <> 0 then
        RandSeed := Trunc(Abs(s)) + Integer(FRndCounter)
      else
        RandSeed := Integer(FRndCounter);

      // If x > y, use Int64.MaxValue as limit
      if x > y then
        y := High(Int64);

      Result := RandomRange(Trunc(x), Trunc(y));
    end;

    else
      RaiseRuntimeError('RND takes 1, 2 or 3 arguments');
  end;
end;

function TSedaiExecutor.BuiltinSgn(const Args: array of Variant): Variant;
var
  Value: Double;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('SGN requires exactly one argument');
  Value := VarToFloat(Args[0]);
  if Value > 0 then
    Result := 1
  else if Value < 0 then
    Result := -1
  else
    Result := 0;
end;

// === BUILT-IN STRING FUNCTIONS ===
function TSedaiExecutor.BuiltinLen(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('LEN requires exactly one argument');
  Result := Length(VarToStr(Args[0]));
end;

function TSedaiExecutor.BuiltinMid(const Args: array of Variant): Variant;
var
  Str: string;
  Start, Len: Integer;
begin
  if (Length(Args) < 2) or (Length(Args) > 3) then
    RaiseRuntimeError('MID$ requires 2 or 3 arguments');

  Str := VarToStr(Args[0]);
  Start := VarToInt(Args[1]);

  if Length(Args) = 3 then
    Len := VarToInt(Args[2])
  else
    Len := Length(Str) - Start + 1;

  Result := Copy(Str, Start, Len);
end;

function TSedaiExecutor.BuiltinLeft(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 2 then
    RaiseRuntimeError('LEFT$ requires exactly two arguments');
  Result := Copy(VarToStr(Args[0]), 1, VarToInt(Args[1]));
end;

function TSedaiExecutor.BuiltinRight(const Args: array of Variant): Variant;
var
  Str: string;
  Len: Integer;
begin
  if Length(Args) <> 2 then
    RaiseRuntimeError('RIGHT$ requires exactly two arguments');
  Str := VarToStr(Args[0]);
  Len := VarToInt(Args[1]);
  Result := Copy(Str, Length(Str) - Len + 1, Len);
end;

function TSedaiExecutor.BuiltinAsc(const Args: array of Variant): Variant;
var
  Str: string;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('ASC requires exactly one argument');
  Str := VarToStr(Args[0]);
  if Length(Str) = 0 then
    Result := 0
  else
    Result := Ord(Str[1]);
end;

function TSedaiExecutor.BuiltinChr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('CHR$ requires exactly one argument');
  Result := Chr(VarToInt(Args[0]));
end;

function TSedaiExecutor.BuiltinSpc(const Args: array of Variant): Variant;
var
  SpaceCount: Integer;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('SPC requires exactly one argument');

  SpaceCount := VarToInt(Args[0]);

  if SpaceCount < 0 then
    SpaceCount := 0;

  Result := StringOfChar(' ', SpaceCount);
  FPrintCurrentCol := FPrintCurrentCol + SpaceCount;
end;

function TSedaiExecutor.BuiltinStr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('STR$ requires exactly one argument');
  Result := VarToStr(Args[0]);
end;

function TSedaiExecutor.BuiltinTab(const Args: array of Variant): Variant;
var
  TargetColumn: Integer;
  SpacesToAdd: Integer;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('TAB requires exactly one argument');

  TargetColumn := VarToInt(Args[0]);

  if TargetColumn < 1 then
    TargetColumn := 1;

  // Calculate spaces needed to reach target column
  SpacesToAdd := TargetColumn - FPrintCurrentCol;

  if SpacesToAdd > 0 then
  begin
    Result := StringOfChar(' ', SpacesToAdd);
    FPrintCurrentCol := TargetColumn;
  end
  else
  begin
    // If already past position, go to new line (Commodore behavior)
    Result := #13#10 + StringOfChar(' ', TargetColumn - 1);
    FPrintCurrentCol := TargetColumn;
  end;
end;

function TSedaiExecutor.BuiltinVal(const Args: array of Variant): Variant;
var
  Str: string;
  Value: Double;
begin
  if Length(Args) <> 1 then
    RaiseRuntimeError('VAL requires exactly one argument');
  Str := VarToStr(Args[0]);
  if TryStrToFloat(Str, Value) then
    Result := Value
  else
    Result := 0;
end;

procedure TSedaiExecutor.RaiseRuntimeError(const Message: string);
begin
  FLastError := Message;
  raise Exception.Create('Runtime error: ' + Message);
end;

procedure TSedaiExecutor.HandleRuntimeError(E: Exception);
begin
  FLastError := E.Message;
  if FDebugMode then
    OptimizedPrintLn('ERROR: ' + E.Message);
end;

// TIER 4 REFACTORING: Integer keys eliminate IntToStr overhead
procedure TSedaiExecutor.BuildLineNumberMap;
var
  i: Integer;
  Statement: TASTNode;
  LineNumber: Integer;
begin
  if not Assigned(FLineNumberMap) then
    FLineNumberMap := TLineNumberMap.Create;

  FLineNumberMap.Clear;

  for i := 0 to FStatementList.Count - 1 do
  begin
    Statement := TASTNode(FStatementList[i]);
    if Assigned(Statement) and (Statement.NodeType = antLineNumber) then
    begin
      LineNumber := VarToInt(Statement.Value);
      FLineNumberMap.Add(LineNumber, i);  // Direct integer key, no IntToStr!
    end;
  end;
end;

// === EXECUTION CONTROL ===
function TSedaiExecutor.ExecuteStep: Boolean;
var
  Statement: TASTNode;
begin
  Result := False;

  if not FRunning or (FProgramCounter >= FStatementList.Count) then
    Exit;

  Statement := TASTNode(FStatementList[FProgramCounter]);

  try
    ExecuteStatement(Statement);
    Inc(FProgramCounter);

    // === FLUSH DURING STEP MODE ===
    FlushOutputBuffer;

    Result := True;

  except
    on E: TExecutorBreakException do
      Result := False;
    on E: TExecutorStopException do
    begin
      FRunning := False;
      Result := False;
    end;
    on E: Exception do
    begin
      HandleRuntimeError(E);
      Result := False;
    end;
  end;
end;

procedure TSedaiExecutor.Stop;
begin
  FRunning := False;
  FlushOutputBuffer;  // === FLUSH WHEN STOPPING ===
end;

procedure TSedaiExecutor.Reset;
var
  i: Integer;
begin
  FRunning := False;
  FProgramCounter := 0;
  FCurrentLineNumber := 0;
  FGosubStack.Clear;

  // Clear FOR loop stack
  for i := 0 to FForLoopStack.Count - 1 do
    TForLoopContext(FForLoopStack[i]).Free;
  FForLoopStack.Clear;

  // Clear execution context
  FContext.ClearVariables;
  FContext.ClearArrays;
  FContext.ResetDataPointer;

  // === RESET PERFORMANCE COUNTERS ===
  FStatementExecutions := 0;
  FPrintCalls := 0;

  // === CLEAR OUTPUT BUFFER ===
  if Assigned(FOutputBuffer) then
    FOutputBuffer.Clear;
end;

procedure TSedaiExecutor.SetBreakpoint(LineNumber: Integer);
begin
  // TODO: Implement
end;

procedure TSedaiExecutor.ClearBreakpoint(LineNumber: Integer);
begin
  // TODO: Implement
end;

procedure TSedaiExecutor.ClearAllBreakpoints;
begin
  // TODO: Implement
end;

procedure TSedaiExecutor.EnableDebugMode(Enabled: Boolean);
begin
  FDebugMode := Enabled;
end;

procedure TSedaiExecutor.EnableTraceMode(Enabled: Boolean);
begin
  FTraceMode := Enabled;
end;

procedure TSedaiExecutor.EnableStepMode(Enabled: Boolean);
begin
  FStepMode := Enabled;
end;

function TSedaiExecutor.GetVariableValue(const VarName: string): Variant;
begin
  Result := FContext.GetVariable(VarName);
end;

procedure TSedaiExecutor.SetVariableValue(const VarName: string; const Value: Variant);
begin
  FContext.SetVariable(VarName, Value);
end;

function TSedaiExecutor.GetVariableList: TStringList;
begin
  Result := FContext.GetVariableNames;
end;

procedure TSedaiExecutor.SetInputSource(Source: TStrings);
begin
  FContext.SetInputSource(Source);
end;

procedure TSedaiExecutor.SetOutputTarget(Target: TStrings);
begin
  FContext.SetOutputTarget(Target);
end;

{ ============================================================================
  TIER 1 OPTIMIZATION: Loop Specialization
  
  Detect simple FOR loops and execute them via fast path, bypassing the
  full AST walker and statement dispatcher overhead.
  
  A "simple loop" is one that:
  - Has no GOTO/GOSUB/RETURN statements inside
  - Has no nested FOR loops
  - Only contains assignments, expressions, and array operations
  
  Expected speedup: 25-30% for loop-heavy code like SIEVE benchmark
  ============================================================================ }

function TSedaiExecutor.FindMatchingNext(const VarName: string; StartPC: Integer): Integer;
var
  i: Integer;
  Statement: TASTNode;
  ForCount: Integer;
begin
  Result := -1;
  ForCount := 0;
  
  for i := StartPC to FStatementList.Count - 1 do
  begin
    Statement := TASTNode(FStatementList[i]);
    if not Assigned(Statement) then Continue;
    
    if Statement.NodeType = antForLoop then
      Inc(ForCount)
    else if Statement.NodeType = antNext then
    begin
      if ForCount = 0 then
      begin
        Result := i;
        Exit;
      end;
      Dec(ForCount);
    end;
  end;
end;

function TSedaiExecutor.IsSimpleForLoop(StartPC, EndPC: Integer): Boolean;
var
  i: Integer;
  Stmt: TASTNode;
begin
  Result := False;
  
  // Sanity check
  if (StartPC < 0) or (EndPC >= FStatementList.Count) or (StartPC >= EndPC) then
    Exit;
  
  // Scan all statements in loop body
  for i := StartPC to EndPC - 1 do
  begin
    Stmt := TASTNode(FStatementList[i]);
    if not Assigned(Stmt) then Continue;
    
    // Check for flow control statements that break fast path
    case Stmt.NodeType of
      antGoto, antGosub, antReturn, antEnd, antStop:
        Exit;  // Complex flow - cannot specialize
        
      antForLoop:
        // TIER 1 OPTIMIZATION: Allow nested loops!
        // Nested loops will be handled recursively by the normal FOR/NEXT logic
        // within ExecuteSimpleForLoop's call to ExecuteStatement
        Continue;  // Nested loops are now allowed
        
      antIf:
        // TIER 2 OPTIMIZATION: Allow simple IFs in fast path
        // Only disallow IF with embedded GOTO/GOSUB/RETURN
        // Most IFs in loops are simple conditions (IF A=0 THEN X=1)
        Continue;  // Allow IFs - they'll be evaluated normally
        
      // These are OK for fast path:
      antAssignment, antPrint, antLineNumber, antNext:
        Continue;
        
      else
        // Unknown statement - play it safe
        Exit;
    end;
  end;
  
  Result := True;
end;

// TIER 4 REFACTORING: Unified loop with integer fast path specialization
procedure TSedaiExecutor.ExecuteSimpleForLoop(ForContext: TForLoopContext);
var
  SavedPC: Integer;
  Stmt: TASTNode;
  ShouldContinue: Boolean;
  // Integer fast path variables
  CurrentInt, EndInt, StepInt: Int64;
  IsIntegerLoop: Boolean;
begin
  // Detect integer-only loops (most common in BASIC)
  IsIntegerLoop := (Frac(ForContext.CurrentValueFloat) = 0) and
                   (Frac(ForContext.EndValueFloat) = 0) and
                   (Frac(ForContext.StepValueFloat) = 0);

  // Initialize integer variables (suppress compiler warnings)
  CurrentInt := 0;
  EndInt := 0;
  StepInt := 0;

  if IsIntegerLoop then
  begin
    CurrentInt := Trunc(ForContext.CurrentValueFloat);
    EndInt := Trunc(ForContext.EndValueFloat);
    StepInt := Trunc(ForContext.StepValueFloat);
  end;

  SavedPC := FProgramCounter;

  repeat
    // Execute loop body statements
    FProgramCounter := ForContext.LoopStartPC;

    while FProgramCounter < ForContext.LoopEndPC do
    begin
      Stmt := TASTNode(FStatementList[FProgramCounter]);

      if Assigned(Stmt) and (Stmt.NodeType <> antNext) then
      begin
        ExecuteStatement(Stmt);
        Inc(FStatementExecutions);
      end;

      Inc(FProgramCounter);

      if not FRunning then
        Break;
    end;

    if not FRunning then
      Break;

    // Increment and test: specialized by loop type
    if IsIntegerLoop then
    begin
      // INTEGER FAST PATH: Native Int64 arithmetic
      CurrentInt := CurrentInt + StepInt;
      FContext.SetVariableInt(ForContext.VarName, CurrentInt);

      if StepInt > 0 then
        ShouldContinue := CurrentInt <= EndInt
      else
        ShouldContinue := CurrentInt >= EndInt;
    end
    else
    begin
      // FLOAT PATH: Variant arithmetic
      ForContext.CurrentValueFloat := ForContext.CurrentValueFloat + ForContext.StepValueFloat;
      FContext.SetVariable(ForContext.VarName, ForContext.CurrentValueFloat);

      // Float comparison
      if ForContext.StepValueFloat > 0 then
        ShouldContinue := ForContext.CurrentValueFloat <= ForContext.EndValueFloat
      else
        ShouldContinue := ForContext.CurrentValueFloat >= ForContext.EndValueFloat;
    end;

  until not ShouldContinue;

  // Restore PC to AFTER the NEXT statement
  FProgramCounter := ForContext.LoopEndPC + 1;
end;

procedure TSedaiExecutor.ExecuteConstStatement(ConstNode: TASTNode);
var
  Assignment: TASTNode;
begin
  // CONST name = value
  // For now, execute as a normal assignment
  // Future: Mark variable as constant in symbol table for Constant Propagation

  if (ConstNode.ChildCount > 0) and (ConstNode.Child[0].NodeType = antAssignment) then
  begin
    Assignment := ConstNode.Child[0];
    ExecuteAssignmentStatement(Assignment);
  end;
  // If CONST appears during execution, simply process the assignment
end;

end.
