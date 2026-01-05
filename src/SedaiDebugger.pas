{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Debugger unit for SedaiBasic VM
 * Provides breakpoints, stepping, variable inspection
 *}
unit SedaiDebugger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Debugger execution state }
  TDebuggerState = (
    dsInactive,      // Debugger not active
    dsRunning,       // Running normally (with trace)
    dsStepping,      // Step mode - pause after each line
    dsPaused,        // Paused at breakpoint or after step
    dsStepOver,      // Step over - execute current line including calls
    dsStepOut        // Step out - run until return from current call
  );

  { Breakpoint information }
  TBreakpoint = record
    LineNumber: Integer;   // BASIC source line number
    Enabled: Boolean;      // Can be temporarily disabled
    HitCount: Integer;     // Number of times hit
  end;
  TBreakpointArray = array of TBreakpoint;

  { Callback when debugger pauses }
  TDebuggerPauseEvent = procedure(Sender: TObject; LineNumber: Integer;
                                   const Reason: string) of object;

  { Callback for trace output }
  TDebuggerTraceEvent = procedure(Sender: TObject; LineNumber: Integer) of object;

  { The debugger class }
  TSedaiDebugger = class
  private
    FState: TDebuggerState;
    FBreakpoints: TBreakpointArray;
    FBreakpointCount: Integer;
    FCurrentLine: Integer;
    FLastLine: Integer;
    FCallDepth: Integer;           // Track call depth for step over/out
    FStepOutTargetDepth: Integer;  // Target depth for step out
    FOnPause: TDebuggerPauseEvent;
    FOnTrace: TDebuggerTraceEvent;
    FTraceOutput: Boolean;         // Output [line] during execution
    function FindBreakpoint(LineNumber: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Debugger control }
    procedure Activate;            // Enable debugger (TRON)
    procedure Deactivate;          // Disable debugger (TROFF)
    procedure Reset;               // Reset state for new run

    { Execution control }
    procedure Continue;            // Continue execution (CONT)
    procedure StepLine;            // Step one source line (STEP)
    procedure StepOver;            // Step over calls
    procedure StepOut;             // Step out of current call

    { Breakpoint management }
    procedure SetBreakpoint(LineNumber: Integer);
    procedure ClearBreakpoint(LineNumber: Integer);
    procedure ClearAllBreakpoints;
    procedure EnableBreakpoint(LineNumber: Integer; Enable: Boolean);
    function HasBreakpoint(LineNumber: Integer): Boolean;
    function GetBreakpointCount: Integer;
    function GetBreakpointLine(Index: Integer): Integer;

    { Called by VM during execution }
    function ShouldPause(LineNumber: Integer): Boolean;
    procedure NotifyLineChange(LineNumber: Integer);
    procedure NotifyCall;          // GOSUB/function call
    procedure NotifyReturn;        // RETURN

    { State queries }
    function IsActive: Boolean;
    function IsPaused: Boolean;
    function IsRunning: Boolean;

    { Properties }
    property State: TDebuggerState read FState;
    property CurrentLine: Integer read FCurrentLine;
    property TraceOutput: Boolean read FTraceOutput write FTraceOutput;
    property OnPause: TDebuggerPauseEvent read FOnPause write FOnPause;
    property OnTrace: TDebuggerTraceEvent read FOnTrace write FOnTrace;
  end;

implementation

{ TSedaiDebugger }

constructor TSedaiDebugger.Create;
begin
  inherited Create;
  SetLength(FBreakpoints, 0);
  FBreakpointCount := 0;
  FState := dsInactive;
  FCurrentLine := 0;
  FLastLine := 0;
  FCallDepth := 0;
  FStepOutTargetDepth := 0;
  FTraceOutput := True;  // Default: show line numbers when tracing
end;

destructor TSedaiDebugger.Destroy;
begin
  SetLength(FBreakpoints, 0);
  inherited Destroy;
end;

function TSedaiDebugger.FindBreakpoint(LineNumber: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FBreakpointCount - 1 do
    if FBreakpoints[i].LineNumber = LineNumber then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TSedaiDebugger.Activate;
begin
  if FState = dsInactive then
    FState := dsRunning;
end;

procedure TSedaiDebugger.Deactivate;
begin
  FState := dsInactive;
  FCurrentLine := 0;
  FLastLine := 0;
  FCallDepth := 0;
end;

procedure TSedaiDebugger.Reset;
begin
  FCurrentLine := 0;
  FLastLine := 0;
  FCallDepth := 0;
  FStepOutTargetDepth := 0;
  if FState <> dsInactive then
    FState := dsRunning;
end;

procedure TSedaiDebugger.Continue;
begin
  if FState = dsPaused then
    FState := dsRunning;
end;

procedure TSedaiDebugger.StepLine;
begin
  FState := dsStepping;
end;

procedure TSedaiDebugger.StepOver;
begin
  FState := dsStepOver;
  FStepOutTargetDepth := FCallDepth;
end;

procedure TSedaiDebugger.StepOut;
begin
  if FCallDepth > 0 then
  begin
    FState := dsStepOut;
    FStepOutTargetDepth := FCallDepth - 1;
  end
  else
    // Already at top level, just step
    StepLine;
end;

procedure TSedaiDebugger.SetBreakpoint(LineNumber: Integer);
var
  Idx: Integer;
begin
  Idx := FindBreakpoint(LineNumber);
  if Idx >= 0 then
  begin
    // Already exists, just enable it
    FBreakpoints[Idx].Enabled := True;
  end
  else
  begin
    // Add new breakpoint
    if FBreakpointCount >= Length(FBreakpoints) then
      SetLength(FBreakpoints, Length(FBreakpoints) + 16);
    FBreakpoints[FBreakpointCount].LineNumber := LineNumber;
    FBreakpoints[FBreakpointCount].Enabled := True;
    FBreakpoints[FBreakpointCount].HitCount := 0;
    Inc(FBreakpointCount);
  end;
end;

procedure TSedaiDebugger.ClearBreakpoint(LineNumber: Integer);
var
  Idx, i: Integer;
begin
  Idx := FindBreakpoint(LineNumber);
  if Idx >= 0 then
  begin
    // Remove by shifting
    for i := Idx to FBreakpointCount - 2 do
      FBreakpoints[i] := FBreakpoints[i + 1];
    Dec(FBreakpointCount);
  end;
end;

procedure TSedaiDebugger.ClearAllBreakpoints;
begin
  FBreakpointCount := 0;
end;

procedure TSedaiDebugger.EnableBreakpoint(LineNumber: Integer; Enable: Boolean);
var
  Idx: Integer;
begin
  Idx := FindBreakpoint(LineNumber);
  if Idx >= 0 then
    FBreakpoints[Idx].Enabled := Enable;
end;

function TSedaiDebugger.HasBreakpoint(LineNumber: Integer): Boolean;
var
  Idx: Integer;
begin
  Idx := FindBreakpoint(LineNumber);
  Result := (Idx >= 0) and FBreakpoints[Idx].Enabled;
end;

function TSedaiDebugger.GetBreakpointCount: Integer;
begin
  Result := FBreakpointCount;
end;

function TSedaiDebugger.GetBreakpointLine(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FBreakpointCount) then
    Result := FBreakpoints[Index].LineNumber
  else
    Result := -1;
end;

function TSedaiDebugger.ShouldPause(LineNumber: Integer): Boolean;
var
  Idx: Integer;
begin
  Result := False;

  if FState = dsInactive then
    Exit;

  // Check breakpoint
  Idx := FindBreakpoint(LineNumber);
  if (Idx >= 0) and FBreakpoints[Idx].Enabled then
  begin
    Inc(FBreakpoints[Idx].HitCount);
    Result := True;
    Exit;
  end;

  // Check stepping modes
  case FState of
    dsStepping:
      // Pause on every new line
      Result := (LineNumber <> FLastLine);

    dsStepOver:
      // Pause when back at same or lower call depth
      Result := (LineNumber <> FLastLine) and (FCallDepth <= FStepOutTargetDepth);

    dsStepOut:
      // Pause when call depth drops below target
      Result := FCallDepth < FStepOutTargetDepth;
  end;
end;

procedure TSedaiDebugger.NotifyLineChange(LineNumber: Integer);
var
  Reason: string;
begin
  if FState = dsInactive then
    Exit;

  FLastLine := FCurrentLine;
  FCurrentLine := LineNumber;

  // Trace output
  if FTraceOutput and (LineNumber > 0) and (LineNumber <> FLastLine) then
  begin
    if Assigned(FOnTrace) then
      FOnTrace(Self, LineNumber);
  end;

  // Check if we should pause
  if ShouldPause(LineNumber) then
  begin
    FState := dsPaused;

    if HasBreakpoint(LineNumber) then
      Reason := 'Breakpoint'
    else
      Reason := 'Step';

    if Assigned(FOnPause) then
      FOnPause(Self, LineNumber, Reason);
  end;
end;

procedure TSedaiDebugger.NotifyCall;
begin
  Inc(FCallDepth);
end;

procedure TSedaiDebugger.NotifyReturn;
begin
  if FCallDepth > 0 then
    Dec(FCallDepth);
end;

function TSedaiDebugger.IsActive: Boolean;
begin
  Result := FState <> dsInactive;
end;

function TSedaiDebugger.IsPaused: Boolean;
begin
  Result := FState = dsPaused;
end;

function TSedaiDebugger.IsRunning: Boolean;
begin
  Result := FState in [dsRunning, dsStepping, dsStepOver, dsStepOut];
end;

end.
