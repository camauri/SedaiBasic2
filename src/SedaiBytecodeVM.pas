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
unit SedaiBytecodeVM;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I ConfigFlags.inc}
{$I DebugFlags.inc}
{$I ProfilerFlags.inc}

interface

uses
  Classes, SysUtils, Math, Variants, StrUtils,
  SedaiBytecodeTypes, SedaiOutputInterface, SedaiSSATypes,
  SedaiConsoleBehavior, SedaiDebugger, SedaiExecutorErrors,
  SedaiMemoryMapper
  {$IFDEF ENABLE_PROFILER}, SedaiProfiler{$ENDIF}
  {$IFDEF WITH_SEDAI_AUDIO}, SedaiAudioTypes, SedaiAudioBackend, SedaiSIDEvo{$ENDIF}
  {$IFDEF WEB_MODE}, SedaiWebIO{$ENDIF};

type
  { Forward declaration }
  TBytecodeVM = class;

  { Callback for file commands (LOAD, SAVE) executed from program }
  TFileCommandEvent = procedure(Sender: TBytecodeVM; const Command, Filename: string;
                                var Handled: Boolean) of object;

  { Callback for disk file I/O commands (DOPEN, DCLOSE, etc.)
    Handle: File handle (1-255 for numbers, or string identifier like "MYFILE")
    Filename: Path to file (for DOPEN)
    Mode: Access mode string ("R", "W", "RW", "A") with optional sharing ("R,EXCLUSIVE", etc.)
    ErrorCode: 0 on success, non-zero on error }
  TDiskFileEvent = procedure(Sender: TBytecodeVM; const Command: string;
                             Handle: Integer; const HandleName, Filename, Mode: string;
                             var ErrorCode: Integer) of object;

  { Callback for file data I/O commands (GET#, INPUT#, PRINT#, CMD)
    Handle: File handle number
    Command: 'GET#', 'INPUT#', 'PRINT#', 'CMD'
    Data: For PRINT#/CMD - data to write; For GET#/INPUT# - receives data read
    ErrorCode: 0 on success, non-zero on error }
  TFileDataEvent = procedure(Sender: TBytecodeVM; const Command: string;
                             Handle: Integer; var Data: string;
                             var ErrorCode: Integer) of object;

  { Event poll callback for keeping UI responsive during VM execution }
  TEventPollCallback = function: Boolean of object;

  { Array storage structure }
  TArrayStorage = record
    ElementType: Byte;        // 0=Int, 1=Float, 2=String (maps to TSSARegisterType)
    DimCount: Integer;
    Dimensions: array of Integer;
    TotalSize: Integer;
    IntData: array of Int64;
    FloatData: array of Double;
    StringData: array of string;
  end;

  { Bytecode VM - register-based virtual machine }
  TBytecodeVM = class
  private
    FIntRegs: array of Int64;
    FFloatRegs: array of Double;
    FStringRegs: array of string;
    FTempIntRegs: array of Int64;
    FTempFloatRegs: array of Double;
    FTempFStringRegs: array of string;
    FIntRegCount: Integer;       // Current size of int register arrays
    FFloatRegCount: Integer;     // Current size of float register arrays
    FStringRegCount: Integer;    // Current size of string register arrays
    FProgram: TBytecodeProgram;
    FPC: Integer;
    FRunning: Boolean;
    FStopped: Boolean;        // True if stopped by STOP command (can CONT)
    FStoppedPC: Integer;      // PC to resume from when CONT is called
    FTraceActive: Boolean;    // True = trace mode on (TRON), outputs line numbers
    FLastSourceLine: Integer; // Last source line executed (for trace output)
    FCallStack: array of Integer;
    FCallStackPtr: Integer;
    FOutputDevice: IOutputDevice;
    FInputDevice: IInputDevice;
    FMemoryMapper: IMemoryMapper;  // Memory-mapped PEEK/POKE support
    FConsoleBehavior: TConsoleBehavior;
    FOwnsConsoleBehavior: Boolean;
    FCursorCol: Integer;  // Track cursor column for TAB zones
    // Time tracking for TI and TI$
    FStartTicks: QWord;     // Milliseconds since system start when VM started
    FTimeOffset: Int64;     // TI$ offset in milliseconds from real time
    // Function key definitions (1-12)
    FFunctionKeys: array[1..12] of string;
    FVarMap: TStringList;
    FArrays: array of TArrayStorage;
    // DATA pool for DATA/READ/RESTORE statements
    FDataPool: array of Variant;
    FDataIndex: Integer;  // Current read position in DATA pool
    // PUDEF format characters (filler, comma, decimal, dollar)
    FPudefFiller: Char;
    FPudefComma: Char;
    FPudefDecimal: Char;
    FPudefDollar: Char;
    // File command callback (LOAD, SAVE from program)
    FOnFileCommand: TFileCommandEvent;
    // Disk file I/O callback (DOPEN, DCLOSE, etc.)
    FOnDiskFile: TDiskFileEvent;
    // File data I/O callback (GET#, INPUT#, PRINT#, CMD)
    FOnFileData: TFileDataEvent;
    // Current CMD file handle (0 = screen, >0 = redirected to file)
    FCmdHandle: Integer;
    FSwapTempInt: Int64;  // Temp variable for ArraySwapInt superinstruction
    // Event polling callback for UI responsiveness
    FEventPollCallback: TEventPollCallback;
    FEventPollInterval: Integer;
    // Temp variables for ArrayReverseRange and ArrayShiftLeft
    FStartIdx, FEndIdx, FArrIdxTmp, FLoopIdx: Integer;
    FFirstVal: Int64;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    FInstructionsExecuted: Int64;
    {$ENDIF}
    {$IFDEF ENABLE_PROFILER}
    FProfiler: TProfiler;
    {$ENDIF}
    // Debugger support (always available, but only used in DEBUG_MODE via RunDebug)
    FDebugger: TSedaiDebugger;
    // Error state for EL, ER, ERR$ variables
    FLastErrorLine: Integer;      // EL: Last error line number
    FLastErrorCode: Integer;      // ER: Last error code
    FLastErrorMessage: string;    // ERR$: Last error message (variable form)
    FTrueValue: Int64;            // TRUE value: -1 (Commodore BASIC) or 1 (modern BASIC)
    FC128InputMode: Boolean;      // True = C128 mode (accept all, show ?REDO), False = input mask (reject invalid chars)
    // TRAP/RESUME error handling state
    FTrapLine: Integer;           // Line to jump to on error (0 = no trap)
    FTrapPC: Integer;             // PC to jump to on error (resolved from FTrapLine)
    FResumePC: Integer;           // PC to resume from after RESUME
    FInErrorHandler: Boolean;     // True if currently in error handler
    {$IFDEF WEB_MODE}
    FWebContext: TObject;         // TWebContext for web mode (forward reference)
    {$ENDIF}
    {$IFDEF WITH_SEDAI_AUDIO}
    FAudioInitialized: Boolean;
    FAudioBackend: TSedaiAudioBackend;   // SAF audio backend
    FSIDEvo: TSedaiSIDEvo;       // SID emulator for advanced audio
    FAudioTempo: Integer;        // Current tempo (0-255, default 8)
    FAudioEnvelopes: array[0..9] of record  // 10 envelope slots
      Attack, Decay, Sustain, Release: Single;
      Waveform: Integer;
      PulseWidth: Single;
    end;
    procedure ExecutePlayString(const MusicStr: string);
    procedure CooperativeSleep(Milliseconds: Integer);
    {$ENDIF}
    procedure ExecuteInstruction(const Instr: TBytecodeInstruction);
    procedure ExecuteSuperinstruction(const Instr: TBytecodeInstruction);
    // Group-specific dispatch handlers
    procedure ExecuteStringOp(const Instr: TBytecodeInstruction);
    procedure ExecuteMathOp(const Instr: TBytecodeInstruction);
    procedure ExecuteArrayOp(const Instr: TBytecodeInstruction);
    procedure ExecuteIOOp(const Instr: TBytecodeInstruction);
    procedure ExecuteSpecialVarOp(const Instr: TBytecodeInstruction);
    procedure ExecuteGraphicsOp(const Instr: TBytecodeInstruction);
    procedure ExecuteSoundOp(const Instr: TBytecodeInstruction);
    procedure ExecuteSpriteOp(const Instr: TBytecodeInstruction);
    procedure ExecuteFileIOOp(const Instr: TBytecodeInstruction);
    {$IFDEF WEB_MODE}
    procedure ExecuteWebOp(const Instr: TBytecodeInstruction);
    {$ENDIF}
    // File management operations (executed directly in VM)
    procedure ExecuteCopyFile(const Src, Dest: string; Overwrite: Boolean);
    procedure ExecuteScratch(const Pattern: string; Force: Boolean);
    procedure ExecuteRenameFile(const OldName, NewName: string);
    procedure ExecuteConcat(const Src, Dest: string);
    procedure ExecuteMkdir(const Path: string);
    procedure ExecuteChdir(const Path: string);
    procedure ExecuteMoveFile(const Src, Dest: string);
    procedure InitializeRegisters;
    procedure ClearAllVariables;
    procedure EnsureRegisterCapacity(RegType: TSSARegisterType; MinIndex: Integer);
    procedure CheckFloatValid(RegIndex: Integer; const OpName: string);
    function FormatUsingString(const FormatStr: string; Value: Double): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadProgram(Program_: TBytecodeProgram);
    procedure ClearProgram;  // Clear program reference (use before freeing the program externally)
    procedure SetOutputDevice(Device: IOutputDevice);
    procedure SetInputDevice(Device: IInputDevice);
    procedure SetMemoryMapper(Mapper: IMemoryMapper);
    procedure SetConsoleBehavior(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean = False);
    procedure ApplyPreset(Preset: TConsolePreset);
    function GetConsoleBehavior: TConsoleBehavior;
    procedure Run;       // Default execution - calls RunFast
    procedure RunFast;   // Optimized execution loop - no profiler/debug support
    procedure RunDebug;  // Debug execution loop - TRON trace + profiler support
    // procedure RunSwitchedGoto;  // Disabled - replaced by template-based approach
    procedure Step;
    procedure Reset;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    function GetInstructionsExecuted: Int64;
    property InstructionsExecuted: Int64 read FInstructionsExecuted;
    {$ENDIF}
    function FindPCForSourceLine(SourceLine: Integer): Integer;
    {$IFDEF ENABLE_PROFILER}
    procedure SetProfiler(AProfiler: TProfiler);
    property Profiler: TProfiler read FProfiler write FProfiler;
    {$ENDIF}
    property PC: Integer read FPC;
    property Running: Boolean read FRunning;
    property Stopped: Boolean read FStopped;  // True if program was stopped by STOP
    procedure Continue;  // Resume execution after STOP (CONT command)
    // Debugger support
    procedure SetDebugger(ADebugger: TSedaiDebugger);
    property Debugger: TSedaiDebugger read FDebugger write FDebugger;
    property OnFileCommand: TFileCommandEvent read FOnFileCommand write FOnFileCommand;
    property OnDiskFile: TDiskFileEvent read FOnDiskFile write FOnDiskFile;
    property OnFileData: TFileDataEvent read FOnFileData write FOnFileData;
    property CmdHandle: Integer read FCmdHandle;  // Current CMD output redirect handle
    {$IFDEF WEB_MODE}
    procedure SetWebContext(AContext: TObject);
    {$ENDIF}
    // Error state for EL, ER, ERR$ system variables
    procedure SetErrorState(ALine, ACode: Integer; const AMessage: string);
    procedure ClearErrorState;
    property LastErrorLine: Integer read FLastErrorLine;
    property LastErrorCode: Integer read FLastErrorCode;
    property LastErrorMessage: string read FLastErrorMessage;
    // TRUE value for comparisons (-1 = Commodore BASIC, 1 = modern BASIC)
    procedure SetTrueValue(AValue: Int64);
    property TrueValue: Int64 read FTrueValue write FTrueValue;
    // C128 INPUT mode: True = accept all then show ?REDO FROM START, False = input mask
    property C128InputMode: Boolean read FC128InputMode write FC128InputMode;
    // Function key definitions (for console expansion)
    function GetFunctionKey(KeyNum: Integer): string;
  end;

implementation

{$IFDEF WITH_SEDAI_AUDIO}
const
  AUDIO_SAMPLE_RATE = 44100;
  AUDIO_BUFFER_SIZE = 1024;

var
  // Global reference to SIDEvo for SAF audio callback
  GSIDEvoInstance: TSedaiSIDEvo = nil;

// SAF audio callback - stereo float output
var
  GCallbackCount: Integer = 0;
  GLastSamplePrinted: Boolean = False;
  GMaxSampleSeen: Single = 0;
  GTotalSamples: Int64 = 0;  // For test tone phase
  GTestToneEnabled: Boolean = False;  // Set to True to test audio output

procedure SAFAudioCallback(AOutput: PSingle; AFrameCount: Integer; AUserData: Pointer);
var
  I: Integer;
  Sample: Single;
  Phase: Single;
begin
  Inc(GCallbackCount);

  // TEST MODE: Generate a 440 Hz sine wave to verify audio output works
  if GTestToneEnabled then
  begin
    for I := 0 to AFrameCount - 1 do
    begin
      Phase := (GTotalSamples + I) * 440.0 * 2.0 * Pi / 44100.0;
      Sample := Sin(Phase) * 0.3;  // 30% volume
      AOutput[I * 2] := Sample;      // Left
      AOutput[I * 2 + 1] := Sample;  // Right
    end;
    Inc(GTotalSamples, AFrameCount);
    Exit;
  end;

  if not Assigned(GSIDEvoInstance) then
  begin
    // Silence (stereo interleaved)
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := 0.0;
    {$IFDEF DEBUG_AUDIO}
    if (GCallbackCount mod 100) = 1 then
      WriteLn('[DEBUG_AUDIO] Callback #', GCallbackCount, ' - NO SIDEvo instance!');
    {$ENDIF}
    Exit;
  end;

  // Generate samples from SIDEvo and output stereo
  for I := 0 to AFrameCount - 1 do
  begin
    Sample := GSIDEvoInstance.GenerateSample;
    {$IFDEF DEBUG_AUDIO}
    // Track max sample for debug
    if Abs(Sample) > GMaxSampleSeen then
      GMaxSampleSeen := Abs(Sample);
    {$ENDIF}
    // Amplify signal (SIDEvo ReSID-exact output is normalized but quiet)
    Sample := Sample * 3.0;
    // Clamp to valid range
    if Sample > 1.0 then Sample := 1.0;
    if Sample < -1.0 then Sample := -1.0;
    // Output stereo (same sample to both channels)
    AOutput[I * 2] := Sample;      // Left
    AOutput[I * 2 + 1] := Sample;  // Right
  end;

  {$IFDEF DEBUG_AUDIO}
  if (GCallbackCount mod 100) = 1 then
    WriteLn('[DEBUG_AUDIO] Callback #', GCallbackCount, ' frames=', AFrameCount,
            ' MaxSample=', GMaxSampleSeen:0:6, ' MasterVol=', GSIDEvoInstance.MasterVolume:0:2);
  {$ENDIF}
end;
{$ENDIF}

constructor TBytecodeVM.Create;
{$IFDEF WITH_SEDAI_AUDIO}
var
  i: Integer;
{$ENDIF}
begin
  inherited Create;
  FProgram := nil;
  FPC := 0;
  FRunning := False;
  FCallStackPtr := 0;
  FCursorCol := 0;
  // Initialize time tracking
  FStartTicks := GetTickCount64;
  FTimeOffset := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  SetLength(FCallStack, 256);
  FVarMap := TStringList.Create;
  FVarMap.Sorted := True;
  // Create default console behavior (Commodore 64 style)
  FConsoleBehavior := TConsolePresets.CreateCommodore64;
  FOwnsConsoleBehavior := True;
  // Initialize CMD handle (0 = output to screen)
  FCmdHandle := 0;
  // Initialize event polling (nil = disabled)
  FEventPollCallback := nil;
  FEventPollInterval := 10000;  // Poll every 10000 instructions by default
  // Initialize error state for EL, ER, ERR$
  FLastErrorLine := 0;
  FLastErrorCode := 0;
  FLastErrorMessage := '';
  // Initialize TRUE value (default: -1 for Commodore BASIC compatibility)
  FTrueValue := -1;
  // Initialize TRAP/RESUME state
  FTrapLine := 0;
  FTrapPC := -1;
  FResumePC := -1;
  FInErrorHandler := False;
  InitializeRegisters;
  {$IFDEF WITH_SEDAI_AUDIO}
  // Initialize SAF audio backend
  FAudioInitialized := False;
  FAudioBackend := nil;

  // Create SIDEvo instance
  FSIDEvo := TSedaiSIDEvo.Create;
  FSIDEvo.Initialize(1);  // 1 group = 8 voices

  // Set global reference for callback
  GSIDEvoInstance := FSIDEvo;

  // Create and configure SAF audio backend
  {$IFDEF DEBUG_AUDIO}
  WriteLn('[DEBUG_AUDIO] Creating TSedaiAudioBackend...');
  {$ENDIF}
  FAudioBackend := TSedaiAudioBackend.Create;
  FAudioBackend.SetSampleRate(AUDIO_SAMPLE_RATE);
  FAudioBackend.SetDesiredBufferSize(AUDIO_BUFFER_SIZE);
  FAudioBackend.SetChannels(2);  // Stereo output
  FAudioBackend.SetCallback(@SAFAudioCallback, nil);
  FAudioBackend.SetMode(bmCallback);

  {$IFDEF DEBUG_AUDIO}
  WriteLn('[DEBUG_AUDIO] Calling FAudioBackend.Initialize...');
  {$ENDIF}
  if FAudioBackend.Initialize then
  begin
    {$IFDEF DEBUG_AUDIO}
    WriteLn('[DEBUG_AUDIO] Initialize OK, calling Start...');
    {$ENDIF}
    if FAudioBackend.Start then
    begin
      FAudioInitialized := True;
      {$IFDEF DEBUG_AUDIO}
      WriteLn('[DEBUG_AUDIO] SAF Audio initialized and started OK');
      WriteLn('[DEBUG_AUDIO]   Sample rate: ', FAudioBackend.SampleRate);
      WriteLn('[DEBUG_AUDIO]   Buffer size: ', FAudioBackend.BufferSize);
      WriteLn('[DEBUG_AUDIO]   Mode: bmCallback');
      {$ENDIF}
    end
    {$IFDEF DEBUG_AUDIO}
    else
      WriteLn('[DEBUG_AUDIO] FAudioBackend.Start FAILED')
    {$ENDIF}
    ;
  end
  {$IFDEF DEBUG_AUDIO}
  else
    WriteLn('[DEBUG_AUDIO] SAF Audio initialization FAILED')
  {$ENDIF}
  ;

  FAudioTempo := 8;  // Default tempo (C128 default)

  // Initialize default envelopes with piano-like ADSR values
  // Envelope 0 (T0) is the default instrument
  for i := 0 to 9 do
  begin
    FAudioEnvelopes[i].Attack := 0.01;    // Quick attack (10ms)
    FAudioEnvelopes[i].Decay := 0.1;      // Short decay (100ms)
    FAudioEnvelopes[i].Sustain := 0.7;    // 70% sustain level
    FAudioEnvelopes[i].Release := 0.2;    // Medium release (200ms)
    FAudioEnvelopes[i].Waveform := 1;     // Sawtooth (good default)
    FAudioEnvelopes[i].PulseWidth := 0.5; // 50% duty cycle for pulse
  end;
  {$ENDIF}
end;

destructor TBytecodeVM.Destroy;
begin
  {$IFDEF WITH_SEDAI_AUDIO}
  // Stop and shutdown SAF audio backend
  if Assigned(FAudioBackend) then
  begin
    FAudioBackend.Stop;
    FAudioBackend.Shutdown;
    FreeAndNil(FAudioBackend);
  end;
  // Clear global reference before freeing
  GSIDEvoInstance := nil;
  if Assigned(FSIDEvo) then
    FSIDEvo.Free;
  {$ENDIF}
  if FOwnsConsoleBehavior and Assigned(FConsoleBehavior) then
    FConsoleBehavior.Free;
  FVarMap.Free;
  inherited Destroy;
end;

function TBytecodeVM.GetFunctionKey(KeyNum: Integer): string;
begin
  // Return function key definition (1-12 are valid)
  if (KeyNum >= 1) and (KeyNum <= 12) then
    Result := FFunctionKeys[KeyNum]
  else
    Result := '';
end;

procedure TBytecodeVM.InitializeRegisters;
var i: Integer;
begin
  // Initialize with minimum register slots
  FIntRegCount := MIN_REGISTER_SLOTS;
  FFloatRegCount := MIN_REGISTER_SLOTS;
  FStringRegCount := MIN_REGISTER_SLOTS;

  SetLength(FIntRegs, FIntRegCount);
  SetLength(FFloatRegs, FFloatRegCount);
  SetLength(FStringRegs, FStringRegCount);
  SetLength(FTempIntRegs, FIntRegCount);
  SetLength(FTempFloatRegs, FFloatRegCount);
  SetLength(FTempFStringRegs, FStringRegCount);

  for i := 0 to FIntRegCount - 1 do
  begin
    FIntRegs[i] := 0;
    FTempIntRegs[i] := 0;
  end;

  for i := 0 to FFloatRegCount - 1 do
  begin
    FFloatRegs[i] := 0.0;
    FTempFloatRegs[i] := 0.0;
  end;

  for i := 0 to FStringRegCount - 1 do
  begin
    FStringRegs[i] := '';
    FTempFStringRegs[i] := '';
  end;
end;

procedure TBytecodeVM.ClearAllVariables;
var i: Integer;
begin
  // Clear all registers (reset to default values)
  for i := 0 to FIntRegCount - 1 do
    FIntRegs[i] := 0;
  for i := 0 to FFloatRegCount - 1 do
    FFloatRegs[i] := 0.0;
  for i := 0 to FStringRegCount - 1 do
    FStringRegs[i] := '';
  // Clear all arrays
  for i := 0 to High(FArrays) do
  begin
    SetLength(FArrays[i].IntData, 0);
    SetLength(FArrays[i].FloatData, 0);
    SetLength(FArrays[i].StringData, 0);
    FArrays[i].TotalSize := 0;
  end;
  SetLength(FArrays, 0);
end;

function TBytecodeVM.FormatUsingString(const FormatStr: string; Value: Double): string;
var
  i, j, TotalWidth, IntDigits, DecDigits: Integer;
  HasDollar, FloatDollar, HasCommas, IsNegative: Boolean;
  IntPart, FormattedInt, FormattedDec: string;
  AbsValue: Double;
  DollarChar, DecimalChar, FillerChar: Char;
begin
  // PUDEF characters: FPudefFiller, FPudefComma, FPudefDecimal, FPudefDollar
  DollarChar := FPudefDollar;
  DecimalChar := FPudefDecimal;
  FillerChar := FPudefFiller;

  // Parse format string
  // Format examples: "######.##", "#$####.##" (floating $), "###,###.##" (with commas)
  TotalWidth := 0;
  IntDigits := 0;
  DecDigits := 0;
  HasDollar := False;
  FloatDollar := False;
  HasCommas := False;

  // Count format characters
  i := 1;
  while i <= Length(FormatStr) do
  begin
    case FormatStr[i] of
      '#': begin
        Inc(TotalWidth);
        Inc(IntDigits);
      end;
      '$': begin
        HasDollar := True;
        // Floating dollar if preceded by #
        if (i > 1) and (FormatStr[i-1] = '#') then
        begin
          FloatDollar := True;
          Dec(IntDigits); // One # is for dollar position
        end;
      end;
      '.': begin
        // Count decimal digits after the dot
        DecDigits := 0;
        j := i + 1;
        while (j <= Length(FormatStr)) and (FormatStr[j] = '#') do
        begin
          Inc(DecDigits);
          Inc(j);
        end;
        IntDigits := IntDigits;  // IntDigits already counted
        i := j - 1;  // Skip to end of decimals
      end;
      ',': HasCommas := True;
    end;
    Inc(i);
  end;

  // Handle sign
  IsNegative := Value < 0;
  AbsValue := Abs(Value);

  // Format decimal part
  if DecDigits > 0 then
  begin
    FormattedDec := Format('%.*f', [DecDigits, Frac(AbsValue)]);
    // Remove leading "0." from decimal part
    if Length(FormattedDec) > 2 then
      FormattedDec := Copy(FormattedDec, 3, Length(FormattedDec) - 2)
    else
      FormattedDec := StringOfChar('0', DecDigits);
  end
  else
    FormattedDec := '';

  // Format integer part
  IntPart := IntToStr(Trunc(AbsValue));

  // Add thousand separators if requested
  if HasCommas then
  begin
    FormattedInt := '';
    j := 0;
    for i := Length(IntPart) downto 1 do
    begin
      if (j > 0) and (j mod 3 = 0) then
        FormattedInt := FPudefComma + FormattedInt;
      FormattedInt := IntPart[i] + FormattedInt;
      Inc(j);
    end;
  end
  else
    FormattedInt := IntPart;

  // Build result
  if DecDigits > 0 then
    Result := FormattedInt + DecimalChar + FormattedDec
  else
    Result := FormattedInt;

  // Add negative sign if needed
  if IsNegative then
    Result := '-' + Result;

  // Calculate target width (IntDigits + decimal point + DecDigits)
  if DecDigits > 0 then
    TotalWidth := IntDigits + 1 + DecDigits
  else
    TotalWidth := IntDigits;

  // Handle floating dollar: dollar takes one position
  if FloatDollar then
    Inc(TotalWidth);

  // Pad to width with filler
  if Length(Result) < TotalWidth then
    Result := StringOfChar(FillerChar, TotalWidth - Length(Result)) + Result;

  // Insert floating dollar sign (replaces leftmost filler)
  if FloatDollar then
  begin
    // Find first non-filler position
    for i := 1 to Length(Result) do
    begin
      if Result[i] <> FillerChar then
      begin
        // Insert dollar just before first digit
        if i > 1 then
          Result[i-1] := DollarChar
        else
          Result := DollarChar + Result;
        Break;
      end;
    end;
  end
  else if HasDollar and not FloatDollar then
  begin
    // Fixed dollar at start
    Result := DollarChar + Result;
  end;
end;

{$IFDEF WITH_SEDAI_AUDIO}
{ Cooperative sleep that processes SDL2 events to prevent "not responding" }
procedure TBytecodeVM.CooperativeSleep(Milliseconds: Integer);
const
  SLICE_MS = 16;  // Process events every ~16ms (60 FPS)
var
  Remaining, SleepTime: Integer;
begin
  Remaining := Milliseconds;
  while Remaining > 0 do
  begin
    // Determine sleep slice
    if Remaining > SLICE_MS then
      SleepTime := SLICE_MS
    else
      SleepTime := Remaining;

    // Sleep for this slice
    Sleep(SleepTime);
    Dec(Remaining, SleepTime);

    // Process SDL2 events to keep window responsive
    if Assigned(FInputDevice) then
      FInputDevice.ProcessEvents;

    // Update display
    if Assigned(FOutputDevice) then
      FOutputDevice.Present;
  end;
end;

procedure TBytecodeVM.ExecutePlayString(const MusicStr: string);
{ Parse and execute C128 BASIC PLAY music string
  Control characters:
    Vn = Voice (1-3)
    On = Octave (0-6, default 4)
    Tn = Tune envelope (0-9)
    Un = Volume (0-15, same as VOL command)
    Xn = Filter (0=off, 1=on)
  Duration prefixes:
    W = Whole, H = Half, Q = Quarter, I = Eighth, S = Sixteenth
  Notes: C D E F G A B
  Modifiers: # = sharp, $ = flat, . = dotted
  Special: R = rest, M = wait for voices to finish
}
const
  // Note frequencies for octave 4 (A4 = 440 Hz)
  NoteFreqBase: array[0..11] of Single = (
    261.63,  // C4
    277.18,  // C#4
    293.66,  // D4
    311.13,  // D#4
    329.63,  // E4
    349.23,  // F4
    369.99,  // F#4
    392.00,  // G4
    415.30,  // G#4
    440.00,  // A4
    466.16,  // A#4
    493.88   // B4
  );
var
  Pos, Len: Integer;
  Ch: Char;
  Voice, Octave, Envelope, Volume: Integer;
  FilterOn: Boolean;
  Duration: Integer;  // in jiffies (1/60 sec)
  NoteIndex: Integer;
  Freq: Single;
  Sharp, Flat, Dotted: Boolean;
  Waveform: Word;

  function ParseNumber: Integer;
  var
    NumStr: string;
  begin
    NumStr := '';
    while (Pos <= Len) and (MusicStr[Pos] in ['0'..'9']) do
    begin
      NumStr := NumStr + MusicStr[Pos];
      Inc(Pos);
    end;
    if NumStr = '' then
      Result := 0
    else
      Result := StrToIntDef(NumStr, 0);
  end;

  function GetSIDEvoWaveform(WaveIdx: Integer): Word;
  begin
    case WaveIdx of
      0: Result := SIDEVO_WAVE_TRIANGLE;
      1: Result := SIDEVO_WAVE_SAWTOOTH;
      2: Result := SIDEVO_WAVE_PULSE;
      3: Result := SIDEVO_WAVE_NOISE;
    else
      Result := SIDEVO_WAVE_SAWTOOTH;
    end;
  end;

begin
  if MusicStr = '' then Exit;

  // Defaults
  Voice := 1;
  Octave := 4;
  Envelope := 0;
  Volume := 15;  // Max volume (0-15 like VOL command)
  FilterOn := False;
  Duration := 15;  // Quarter note default at tempo 8

  Pos := 1;
  Len := Length(MusicStr);

  while Pos <= Len do
  begin
    Ch := UpCase(MusicStr[Pos]);
    Inc(Pos);

    case Ch of
      ' ': ; // Skip spaces

      'V': Voice := ParseNumber;  // Voice 1-3
      'O': Octave := ParseNumber; // Octave 0-6
      'T': Envelope := ParseNumber; // Envelope 0-9
      'U': Volume := ParseNumber;   // Volume 0-15
      'X': FilterOn := (ParseNumber = 1); // Filter on/off

      // Duration prefixes
      'W': Duration := 60;  // Whole note
      'H': Duration := 30;  // Half note
      'Q': Duration := 15;  // Quarter note
      'I': Duration := 8;   // Eighth note
      'S': Duration := 4;   // Sixteenth note

      // Notes C D E F G A B
      'C', 'D', 'E', 'F', 'G', 'A', 'B':
      begin
        // Map note letter to index
        case Ch of
          'C': NoteIndex := 0;
          'D': NoteIndex := 2;
          'E': NoteIndex := 4;
          'F': NoteIndex := 5;
          'G': NoteIndex := 7;
          'A': NoteIndex := 9;
          'B': NoteIndex := 11;
        else
          NoteIndex := 0;
        end;

        // Check for modifiers (look ahead)
        Sharp := False;
        Flat := False;
        Dotted := False;

        // Check previous character for modifiers (C128 puts them before note)
        // Actually in C128 syntax, modifiers come BEFORE the note
        // But we already consumed the note, so check what's next
        while (Pos <= Len) and (MusicStr[Pos] in ['#', '$', '.']) do
        begin
          case MusicStr[Pos] of
            '#': Sharp := True;
            '$': Flat := True;
            '.': Dotted := True;
          end;
          Inc(Pos);
        end;

        // Apply sharp/flat
        if Sharp then Inc(NoteIndex);
        if Flat then Dec(NoteIndex);
        if NoteIndex < 0 then NoteIndex := 0;
        if NoteIndex > 11 then NoteIndex := 11;

        // Calculate frequency with octave adjustment
        Freq := NoteFreqBase[NoteIndex] * Power(2, Octave - 4);

        // Apply dotted duration
        if Dotted then
          Duration := (Duration * 3) div 2;

        // Play the note using SIDEvo
        if (Voice >= 1) and (Voice <= 8) and Assigned(FSIDEvo) and Assigned(FAudioBackend) then
        begin
          // Get waveform from envelope
          if (Envelope >= 0) and (Envelope <= 9) then
            Waveform := GetSIDEvoWaveform(FAudioEnvelopes[Envelope].Waveform)
          else
            Waveform := SIDEVO_WAVE_SAWTOOTH;

          {$IFDEF DEBUG_AUDIO}
          WriteLn('[DEBUG_AUDIO] PLAY NOTE: V', Voice, ' ', Freq:0:1, 'Hz Wave=', Waveform);
          {$ENDIF}

          // Lock audio to prevent race conditions with callback
          FAudioBackend.Lock;
          try
            // Configure voice
            FSIDEvo.SetFrequencyHz(Voice - 1, Freq);
            FSIDEvo.SetWaveform(Voice - 1, Waveform);
            FSIDEvo.SetVoiceVolume(Voice - 1, Volume / 15.0);

            // Set ADSR from envelope (SIDEvo uses 0.0-1.0 for level ratios)
            FSIDEvo.SetADSR(Voice - 1,
              FAudioEnvelopes[Envelope].Attack,
              FAudioEnvelopes[Envelope].Decay,
              FAudioEnvelopes[Envelope].Sustain,
              FAudioEnvelopes[Envelope].Release);

            // Trigger note (gate on)
            FSIDEvo.GateOn(Voice - 1);
          finally
            FAudioBackend.Unlock;
          end;

          // Wait for note duration (outside lock to allow callback to run)
          {$IFDEF DEBUG_AUDIO}
          WriteLn('[DEBUG_AUDIO] Sleeping for ', Duration * 1000 * 8 div (60 * FAudioTempo), ' ms (tempo=', FAudioTempo, ')');
          {$ENDIF}
          CooperativeSleep(Duration * 1000 * 8 div (60 * FAudioTempo));

          // Stop the note (gate off - triggers release phase)
          {$IFDEF DEBUG_AUDIO}
          WriteLn('[DEBUG_AUDIO] FSIDEvo.GateOff(', Voice - 1, ')');
          {$ENDIF}
          FAudioBackend.Lock;
          try
            FSIDEvo.GateOff(Voice - 1);
          finally
            FAudioBackend.Unlock;
          end;
        end;
      end;

      'R': // Rest - wait without playing
      begin
        CooperativeSleep(Duration * 1000 * 8 div (60 * FAudioTempo));
      end;

      'M': ; // Wait for voices - not implemented yet

      '#': ; // Sharp modifier (handled with note)
      '$': ; // Flat modifier (handled with note)
      '.': ; // Dotted modifier (handled with note)
    end;
  end;
end;
{$ENDIF}

procedure TBytecodeVM.EnsureRegisterCapacity(RegType: TSSARegisterType; MinIndex: Integer);
var
  OldSize, NewSize, i: Integer;
begin
  case RegType of
    srtInt:
    begin
      if MinIndex >= FIntRegCount then
      begin
        OldSize := FIntRegCount;
        // Double the size or use MinIndex + 1, whichever is larger (but cap at MAX)
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for integer registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        // Grow both working and temp register arrays
        SetLength(FIntRegs, NewSize);
        SetLength(FTempIntRegs, NewSize);

        // Initialize new slots to zero
        for i := OldSize to NewSize - 1 do
        begin
          FIntRegs[i] := 0;
          FTempIntRegs[i] := 0;
        end;

        FIntRegCount := NewSize;
      end;
    end;

    srtFloat:
    begin
      if MinIndex >= FFloatRegCount then
      begin
        OldSize := FFloatRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for float registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(FFloatRegs, NewSize);
        SetLength(FTempFloatRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          FFloatRegs[i] := 0.0;
          FTempFloatRegs[i] := 0.0;
        end;

        FFloatRegCount := NewSize;
      end;
    end;

    srtString:
    begin
      if MinIndex >= FStringRegCount then
      begin
        OldSize := FStringRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for string registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(FStringRegs, NewSize);
        SetLength(FTempFStringRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          FStringRegs[i] := '';
          FTempFStringRegs[i] := '';
        end;

        FStringRegCount := NewSize;
      end;
    end;
  end;
end;

procedure TBytecodeVM.CheckFloatValid(RegIndex: Integer; const OpName: string);
begin
  if IsNan(FFloatRegs[RegIndex]) then
    raise Exception.CreateFmt('NaN detected in R%d after %s', [RegIndex, OpName]);
  if IsInfinite(FFloatRegs[RegIndex]) then
    raise Exception.CreateFmt('Infinity detected in R%d after %s', [RegIndex, OpName]);
end;

procedure TBytecodeVM.LoadProgram(Program_: TBytecodeProgram);
var
  i: Integer;
  Instr: TBytecodeInstruction;
  MaxIntReg, MaxFloatReg, MaxStringReg: Integer;
begin
  FProgram := Program_;

  // Scan bytecode to determine maximum register indices used
  MaxIntReg := -1;
  MaxFloatReg := -1;
  MaxStringReg := -1;

  for i := 0 to FProgram.GetInstructionCount - 1 do
  begin
    Instr := FProgram.GetInstruction(i);

    // Handle superinstructions (opcode >= bcGroupSuper) separately
    if Instr.OpCode >= bcGroupSuper then
    begin
      case Instr.OpCode of
        // Fused compare-and-branch (Int) - use IntRegs for Src1, Src2
        bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        // Fused compare-and-branch (Float) - use FloatRegs for Src1, Src2
        bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;
        // Fused arithmetic-to-dest (Int) - use IntRegs for Dest, Src1
        bcAddIntTo, bcSubIntTo, bcMulIntTo:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused arithmetic-to-dest (Float) - use FloatRegs for Dest, Src1
        bcAddFloatTo, bcSubFloatTo, bcMulFloatTo, bcDivFloatTo:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused constant arithmetic (Int) - use IntRegs for Dest, Src1
        bcAddIntConst, bcSubIntConst, bcMulIntConst:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused constant arithmetic (Float) - use FloatRegs for Dest, Src1
        bcAddFloatConst, bcSubFloatConst, bcMulFloatConst, bcDivFloatConst:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused compare-zero-and-branch (Int) - use IntRegs for Src1
        bcBranchEqZeroInt, bcBranchNeZeroInt:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // Fused compare-zero-and-branch (Float) - use FloatRegs for Src1
        bcBranchEqZeroFloat, bcBranchNeZeroFloat:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;
        // Fused array-store-constant - use IntRegs for Src2 (index register)
        bcArrayStoreIntConst, bcArrayStoreFloatConst, bcArrayStoreStringConst:
        begin
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        // Fused loop increment-and-branch - use IntRegs for Dest, Src1, Src2
        bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // FMA (Fused Multiply-Add) - use FloatRegs for Dest, Src1, Src2, Immediate
        bcMulAddFloat, bcMulSubFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;  // c register
        end;
        bcMulAddToFloat, bcMulSubToFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // Array Load + Arithmetic - use FloatRegs for Dest, Immediate; IntRegs for Src2
        bcArrayLoadAddFloat, bcArrayLoadSubFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index register
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;  // acc register
        end;
        bcArrayLoadDivAddFloat:  // Immediate encodes two registers
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxFloatReg then MaxFloatReg := Instr.Immediate and $FFFF;
          if ((Instr.Immediate shr 16) and $FFFF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 16) and $FFFF;
        end;

        // Square-Sum patterns - use FloatRegs for Dest, Src1, Src2
        bcSquareSumFloat, bcAddSquareFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // Mul-Mul - use FloatRegs for Dest, Src1, Src2, Immediate
        bcMulMulFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;
        end;

        // Add-Sqrt - use FloatRegs for Dest, Src1, Src2
        bcAddSqrtFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // Array Load + Branch - use IntRegs for Src2
        bcArrayLoadIntBranchNZ, bcArrayLoadIntBranchZ:
        begin
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index register
        end;
      end;
    end
    else
    begin
      // Check destination and source registers based on standard opcode
      case TBytecodeOp(Instr.OpCode) of
        // Int dest, int sources
        bcLoadConstInt, bcCopyInt, bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
        bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
        bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // InputInt: int Dest (result), string Src1 (prompt, optional)
        bcInputInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;  // prompt is string
        end;

        // FloatToInt: int Dest, float Src1
        bcFloatToInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // StringToInt: int Dest, string Src1
        bcStringToInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Float comparisons: int Dest (0/1 result), float Src1, float Src2
        bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // String comparisons: int Dest (0/1 result), string Src1, string Src2
        bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // ArrayLoadInt: int Dest (result), int Src2 (index)
        bcArrayLoadInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // Int source (Src1) for branch
        bcJumpIfZero, bcJumpIfNotZero:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // Float dest, float sources
        bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
        bcPowFloat, bcNegFloat,
        bcMathAbs, bcMathSgn, bcMathInt, bcMathSqr, bcMathSin, bcMathCos, bcMathTan,
        bcMathExp, bcMathLog, bcMathAtn, bcMathRnd:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // InputFloat/Input: float Dest (result), string Src1 (prompt, optional)
        bcInput, bcInputFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;  // prompt is string
        end;

        // Type conversions with mixed register types
        // IntToFloat: float Dest, int Src1
        bcIntToFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // StringToFloat: float Dest, string Src1
        bcStringToFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // ArrayLoadFloat: float Dest (result), int Src2 (index)
        bcArrayLoadFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // String dest, string sources
        bcLoadConstString, bcCopyString, bcStrConcat,
        bcStrLeft, bcStrRight, bcStrMid,
        bcInputString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // GET/GETKEY: string Dest (character result)
        bcGet, bcGetkey:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
        end;

        // IntToString: string Dest, int Src1
        bcIntToString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // FloatToString: string Dest, float Src1
        bcFloatToString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // ArrayLoadString: string Dest (result), int Src2 (index)
        bcArrayLoadString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index is int
        end;

        // String Src1 (source) -> int Dest
        bcStrLen, bcStrAsc, bcStrDec:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Int Src1 -> String Dest (CHR$, HEX$, ERR$)
        bcStrChr, bcStrHex, bcStrErr:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // Float Src1 -> String Dest (STR$)
        bcStrStr:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // String Src1 -> Float Dest (VAL)
        bcStrVal:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // INSTR(haystack$, needle$[, start]) -> int Dest
        bcStrInstr:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;  // haystack
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;  // needle
        end;

        // Print/PrintLn: float in Src1
        bcPrint, bcPrintLn:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // PrintString/PrintStringLn: string in Src1
        bcPrintString, bcPrintStringLn:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // ArrayStore: Dest is value register, Src2 is index (int)
        bcArrayStoreInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;  // index
        end;
        bcArrayStoreFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;      // index
        end;
        bcArrayStoreString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;  // value
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;        // index
        end;

        // ArrayLoad: Dest is result, Src2 is index (int)
        // Note: bcArrayLoadInt/Float/String already handled above

        // Graphics operations with multiple registers packed in Immediate
        // bcGraphicBox: Src1=color(int), Src2=x1(int), Dest=y1(int)
        // Immediate: x2(12) | y2(12) | angle(12) | filled(12) | fill_color(12)
        bcGraphicBox:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // color
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // x1
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // y1
          if (Instr.Immediate and $FFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFF;  // x2
          if ((Instr.Immediate shr 12) and $FFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 12) and $FFF;  // y2
          if ((Instr.Immediate shr 24) and $FFF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 24) and $FFF;  // angle (float)
          if ((Instr.Immediate shr 36) and $FFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 36) and $FFF;  // filled
        end;

        // bcGraphicCircle: Src1=color(int), Src2=x(int), Dest=y(int)
        // Immediate: xr(10) | yr(10) | sa(10) | ea(10) | angle(10) | inc(10) = 60 bits
        bcGraphicCircle:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // color
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // x
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // y
          if (Instr.Immediate and $3FF) > MaxIntReg then MaxIntReg := Instr.Immediate and $3FF;  // xr
          if ((Instr.Immediate shr 10) and $3FF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 10) and $3FF;  // yr
          if ((Instr.Immediate shr 20) and $3FF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 20) and $3FF;  // sa (float)
          if ((Instr.Immediate shr 30) and $3FF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 30) and $3FF;  // ea (float)
          if ((Instr.Immediate shr 40) and $3FF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 40) and $3FF;  // angle (float)
          if ((Instr.Immediate shr 50) and $3FF) > MaxFloatReg then MaxFloatReg := (Instr.Immediate shr 50) and $3FF;  // inc (float)
        end;

        // bcGraphicPaint: Src1=source(int), Src2=x(int), Dest=y(int), Immediate=mode(int)
        bcGraphicPaint:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // source
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // x
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // y
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;  // mode
        end;

        // bcGraphicWindow: Src1=col1(int), Src2=row1(int), Dest=col2(int)
        // Immediate bits 0-15 = row2 register(int), bits 16-31 = clear register(int)
        bcGraphicWindow:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // col1
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // row1
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // col2
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;  // row2
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;  // clear
        end;

        // bcGraphicSShape: Dest=string reg, Src1=x1(int), Src2=y1(int)
        // Immediate bits 0-15 = x2 register(int), bits 16-31 = y2 register(int)
        bcGraphicSShape:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;   // result string
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x1
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y1
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;  // x2
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;  // y2
        end;

        // bcGraphicGShape: Src1=string reg, Src2=x(int), Dest=y(int), Immediate=mode
        bcGraphicGShape:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;   // shape string
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // x
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // y
        end;

        // bcSetColor: Src1=index(int), Src2=R(int), Dest=G(int)
        // Immediate: B(12) | A(12)
        bcSetColor:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // palette index
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // R
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // G
          if (Instr.Immediate and $FFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFF;  // B
          if ((Instr.Immediate shr 12) and $FFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 12) and $FFF;  // A
        end;

        // bcPLoad: Src1=filename string reg
        bcPLoad:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;   // filename
        end;

        // bcPSave: Src1=filename string reg
        bcPSave:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;   // filename
        end;

        // bcKey: Src1=key number (int), Src2=key text (string, optional)
        bcKey:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;         // key number
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;   // key text (optional)
        end;
      end;
    end;
  end;

  // Reset VM state first (this initializes registers to MIN_REGISTER_SLOTS)
  Reset;

  // Then ensure we have enough capacity for all registers used
  if MaxIntReg >= 0 then
    EnsureRegisterCapacity(srtInt, MaxIntReg);
  if MaxFloatReg >= 0 then
    EnsureRegisterCapacity(srtFloat, MaxFloatReg);
  if MaxStringReg >= 0 then
    EnsureRegisterCapacity(srtString, MaxStringReg);

end;

procedure TBytecodeVM.ClearProgram;
begin
  // Clear the program reference to avoid dangling pointers
  // Call this BEFORE freeing a program that was loaded externally
  FProgram := nil;
end;

procedure TBytecodeVM.SetOutputDevice(Device: IOutputDevice);
begin
  FOutputDevice := Device;
end;

procedure TBytecodeVM.SetInputDevice(Device: IInputDevice);
begin
  FInputDevice := Device;
end;

procedure TBytecodeVM.SetMemoryMapper(Mapper: IMemoryMapper);
begin
  FMemoryMapper := Mapper;
end;

procedure TBytecodeVM.SetConsoleBehavior(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean);
begin
  if FOwnsConsoleBehavior and Assigned(FConsoleBehavior) then
    FConsoleBehavior.Free;

  FConsoleBehavior := ABehavior;
  FOwnsConsoleBehavior := OwnsBehavior;
end;

procedure TBytecodeVM.ApplyPreset(Preset: TConsolePreset);
begin
  if Assigned(FConsoleBehavior) then
    FConsoleBehavior.ApplyPreset(Preset);
end;

function TBytecodeVM.GetConsoleBehavior: TConsoleBehavior;
begin
  Result := FConsoleBehavior;
end;

procedure TBytecodeVM.Reset;
begin
  FPC := 0;
  FRunning := False;
  FCallStackPtr := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  InitializeRegisters;
  FVarMap.Clear;
  // Reset DATA pool
  SetLength(FDataPool, 0);
  FDataIndex := 0;
  // Reset PUDEF to defaults
  FPudefFiller := ' ';
  FPudefComma := ',';
  FPudefDecimal := '.';
  FPudefDollar := '$';
end;

{$IFDEF ENABLE_INSTRUCTION_COUNTING}
function TBytecodeVM.GetInstructionsExecuted: Int64;
begin
  Result := FInstructionsExecuted;
end;
{$ENDIF}

function TBytecodeVM.FindPCForSourceLine(SourceLine: Integer): Integer;
begin
  // Delegate to TBytecodeProgram's Source Map implementation
  Result := FProgram.FindPCForLine(SourceLine);
  // If exact line not found, return 0 (start of program)
  if Result < 0 then
    Result := 0;
end;

{$IFDEF ENABLE_PROFILER}
procedure TBytecodeVM.SetProfiler(AProfiler: TProfiler);
begin
  FProfiler := AProfiler;
end;
{$ENDIF}

procedure TBytecodeVM.SetDebugger(ADebugger: TSedaiDebugger);
begin
  FDebugger := ADebugger;
end;

procedure TBytecodeVM.SetErrorState(ALine, ACode: Integer; const AMessage: string);
begin
  FLastErrorLine := ALine;
  FLastErrorCode := ACode;
  FLastErrorMessage := AMessage;
end;

procedure TBytecodeVM.ClearErrorState;
begin
  FLastErrorLine := 0;
  FLastErrorCode := 0;
  FLastErrorMessage := '';
end;

procedure TBytecodeVM.SetTrueValue(AValue: Int64);
begin
  FTrueValue := AValue;
end;

procedure TBytecodeVM.ExecuteInstruction(const Instr: TBytecodeInstruction);
var
  Group: Word;
  SleepMs: Integer;
  KeyNum, KeyIdx, CharIdx: Integer;
  KeyText: string;
  Ch: Char;
  InQuotes: Boolean;
begin
  // Two-level dispatch: extract group from high byte
  Group := Instr.OpCode shr 8;

  case Group of
    0: ; // Core VM - fall through to inline dispatch below for performance
    1: begin ExecuteStringOp(Instr); Exit; end;
    2: begin ExecuteMathOp(Instr); Exit; end;
    3: begin ExecuteArrayOp(Instr); Exit; end;
    4: begin ExecuteIOOp(Instr); Exit; end;
    5: begin ExecuteSpecialVarOp(Instr); Exit; end;
    6: begin ExecuteFileIOOp(Instr); Exit; end;
    7: begin ExecuteSpriteOp(Instr); Exit; end;
    {$IFDEF WEB_MODE}
    8: begin ExecuteWebOp(Instr); Exit; end;
    {$ENDIF}
    10: begin ExecuteGraphicsOp(Instr); Exit; end;
    11: begin ExecuteSoundOp(Instr); Exit; end;
    200..255: begin ExecuteSuperinstruction(Instr); Exit; end;
  else
    raise Exception.CreateFmt('Unknown opcode group %d at PC=%d', [Group, FPC]);
  end;

  // Group 0: Core VM operations - inline for performance
  case Instr.OpCode of
    bcLoadConstInt: FIntRegs[Instr.Dest] := Instr.Immediate;
    bcLoadConstFloat: FFloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^);
    bcLoadConstString:
      if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
        FStringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate];
    bcCopyInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1];
    bcCopyFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1];
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace copies to R38 specifically (the problematic register in n-body)
        if Instr.Dest = 38 then
        begin
          WriteLn(StdErr, 'CopyFloat at PC=', FPC, ': R[', Instr.Dest, '] ← R[', Instr.Src1, ']');
          WriteLn(StdErr, '  Source R[', Instr.Src1, '] = ', FFloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  Dest   R[', Instr.Dest, '] = ', FFloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcCopyString: FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1];
    bcAddInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] + FIntRegs[Instr.Src2];
    bcSubInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] - FIntRegs[Instr.Src2];
    bcMulInt: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] * FIntRegs[Instr.Src2];
    bcDivInt:
      if FIntRegs[Instr.Src2] <> 0 then
        FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] div FIntRegs[Instr.Src2]
      else raise Exception.Create('Division by zero');
    bcModInt:
      if FIntRegs[Instr.Src2] <> 0 then
        FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] mod FIntRegs[Instr.Src2]
      else raise Exception.Create('Modulo by zero');
    bcNegInt: FIntRegs[Instr.Dest] := -FIntRegs[Instr.Src1];
    bcAddFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'AddFloat');
        {$ENDIF}
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace additions to R41 and R43 (sum of squares in n-body)
        if (Instr.Dest = 41) or (Instr.Dest = 43) then
        begin
          WriteLn(StdErr, 'AddFloat at PC=', FPC, ': R[', Instr.Dest, '] = R[', Instr.Src1, '] + R[', Instr.Src2, ']');
          WriteLn(StdErr, '  R[', Instr.Src1, '] = ', FFloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  R[', Instr.Src2, '] = ', FFloatRegs[Instr.Src2]:0:17);
          WriteLn(StdErr, '  R[', Instr.Dest, '] = ', FFloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcSubFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] - FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'SubFloat');
        {$ENDIF}
      end;
    bcMulFloat:
      begin
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Instr.Dest, 'MulFloat');
        {$ENDIF}
      end;
    bcDivFloat:
      begin
        if Abs(FFloatRegs[Instr.Src2]) >= 1e-300 then
        begin
          FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] / FFloatRegs[Instr.Src2];
          {$IFDEF DEBUG_FLOAT_CHECKS}
          CheckFloatValid(Instr.Dest, 'DivFloat');
          {$ENDIF}
        end
        else
          raise EZeroDivide.Create('Division by zero');
      end;
    bcPowFloat: FFloatRegs[Instr.Dest] := Power(FFloatRegs[Instr.Src1], FFloatRegs[Instr.Src2]);
    bcNegFloat: FFloatRegs[Instr.Dest] := -FFloatRegs[Instr.Src1];
    bcIntToFloat: FFloatRegs[Instr.Dest] := FIntRegs[Instr.Src1];
    bcFloatToInt: FIntRegs[Instr.Dest] := Trunc(FFloatRegs[Instr.Src1]);
    // Comparison operators - Int (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqInt: if FIntRegs[Instr.Src1] = FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpNeInt: if FIntRegs[Instr.Src1] <> FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpLtInt: if FIntRegs[Instr.Src1] < FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpGtInt: if FIntRegs[Instr.Src1] > FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpLeInt: if FIntRegs[Instr.Src1] <= FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpGeInt: if FIntRegs[Instr.Src1] >= FIntRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    // Comparison operators - Float (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqFloat: if FFloatRegs[Instr.Src1] = FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpNeFloat: if FFloatRegs[Instr.Src1] <> FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpLtFloat: if FFloatRegs[Instr.Src1] < FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpGtFloat: if FFloatRegs[Instr.Src1] > FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpLeFloat: if FFloatRegs[Instr.Src1] <= FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpGeFloat: if FFloatRegs[Instr.Src1] >= FFloatRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    // Comparison operators - String (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqString: if FStringRegs[Instr.Src1] = FStringRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpNeString: if FStringRegs[Instr.Src1] <> FStringRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpLtString: if FStringRegs[Instr.Src1] < FStringRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    bcCmpGtString: if FStringRegs[Instr.Src1] > FStringRegs[Instr.Src2] then FIntRegs[Instr.Dest] := FTrueValue else FIntRegs[Instr.Dest] := 0;
    // Bitwise operators
    bcBitwiseAnd: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] and FIntRegs[Instr.Src2];
    bcBitwiseOr: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] or FIntRegs[Instr.Src2];
    bcBitwiseXor: FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] xor FIntRegs[Instr.Src2];
    bcBitwiseNot: FIntRegs[Instr.Dest] := not FIntRegs[Instr.Src1];
    // Control flow
    bcJump: FPC := Instr.Immediate - 1;
    bcJumpIfZero:
      if FIntRegs[Instr.Src1] = 0 then FPC := Instr.Immediate - 1;
    bcJumpIfNotZero:
      if FIntRegs[Instr.Src1] <> 0 then FPC := Instr.Immediate - 1;
    bcCall:
      begin
        FCallStack[FCallStackPtr] := FPC;
        Inc(FCallStackPtr);
        FPC := Instr.Immediate - 1;
      end;
    bcReturn:
      if FCallStackPtr > 0 then
      begin
        Dec(FCallStackPtr);
        FPC := FCallStack[FCallStackPtr];
      end;
    // System commands
    bcEnd:
      begin
        FRunning := False;
        FStopped := False;  // END clears stopped state
      end;
    bcStop:
      begin
        FRunning := False;
        FStopped := True;             // Mark as stopped (can CONT)
        FStoppedPC := FPC + 1;        // Save PC for resume (next instruction)
        if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print('BREAK');
          FOutputDevice.NewLine;
        end;
      end;
    bcFast: if Assigned(FOutputDevice) then FOutputDevice.SetFastMode(True);
    bcSlow: if Assigned(FOutputDevice) then FOutputDevice.SetFastMode(False);
    bcSleep:
      begin
        if Instr.Immediate > 0 then
          SleepMs := Instr.Immediate * 1000
        else if Instr.Src1 < FFloatRegCount then
          SleepMs := Trunc(FFloatRegs[Instr.Src1] * 1000)
        else
          SleepMs := 1000;
        if SleepMs < 0 then SleepMs := 0;
        if SleepMs > 65535000 then SleepMs := 65535000;
        while (SleepMs > 0) and FRunning do
        begin
          if SleepMs > 50 then
          begin
            Sleep(50);
            Dec(SleepMs, 50);
          end
          else
          begin
            Sleep(SleepMs);
            SleepMs := 0;
          end;
          if Assigned(FInputDevice) then
          begin
            FInputDevice.ProcessEvents;
            if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
            begin
              FRunning := False;
              FInputDevice.ClearStopRequest;
            end;
          end;
        end;
      end;
    bcKey:
      begin
        // KEY n, "text" - define function key
        // KEY n - clear key definition
        // KEY (0) - list all keys (valid keys are 1-12)
        KeyNum := FIntRegs[Instr.Src1];
        if KeyNum = 0 then
        begin
          // List all function key definitions
          if Assigned(FOutputDevice) then
          begin
            for KeyIdx := 1 to 12 do
            begin
              // Format as proper BASIC concatenation: "TEXT"+CHR$(n)
              KeyText := '';
              InQuotes := False;
              for CharIdx := 1 to Length(FFunctionKeys[KeyIdx]) do
              begin
                Ch := FFunctionKeys[KeyIdx][CharIdx];
                if Ord(Ch) < 32 then
                begin
                  // Control character - close quotes if open, add +CHR$(n)
                  if InQuotes then
                  begin
                    KeyText := KeyText + '"';
                    InQuotes := False;
                  end;
                  if KeyText <> '' then
                    KeyText := KeyText + '+';
                  KeyText := KeyText + 'CHR$(' + IntToStr(Ord(Ch)) + ')';
                end
                else
                begin
                  // Normal character - open quotes if needed
                  if not InQuotes then
                  begin
                    if KeyText <> '' then
                      KeyText := KeyText + '+';
                    KeyText := KeyText + '"';
                    InQuotes := True;
                  end;
                  KeyText := KeyText + Ch;
                end;
              end;
              // Close quotes if still open
              if InQuotes then
                KeyText := KeyText + '"';
              // Show "" for undefined keys
              if KeyText = '' then
                KeyText := '""';
              FOutputDevice.Print('F' + IntToStr(KeyIdx) + ': ' + KeyText);
              FOutputDevice.NewLine;
            end;
          end;
        end
        else if (KeyNum >= 1) and (KeyNum <= 12) then
        begin
          // Define or clear function key
          if Instr.Src2 < FStringRegCount then
            FFunctionKeys[KeyNum] := FStringRegs[Instr.Src2]
          else
            FFunctionKeys[KeyNum] := '';  // Clear key
        end;
      end;
    bcTron:
      begin
        // TRON - Enable trace mode
        FTraceActive := True;
        FLastSourceLine := 0;  // Reset last line
      end;
    bcTroff:
      begin
        // TROFF - Disable trace mode
        FTraceActive := False;
      end;
    bcTrap:
      begin
        // TRAP linenum - Set error handler line
        // If Immediate >= 0, use it directly (constant line number)
        // If Immediate = -1, use register R[Src1] (variable line number)
        if Instr.Immediate >= 0 then
          FTrapLine := Instr.Immediate
        else
          FTrapLine := FIntRegs[Instr.Src1];
        if FTrapLine > 0 then
        begin
          // Resolve line number to PC
          // For now, we store the line and resolve at error time
          // using the program's line number map
          FTrapPC := -1;  // Will be resolved when error occurs
        end
        else
        begin
          // Disable trap handler
          FTrapLine := 0;
          FTrapPC := -1;
        end;
      end;
    bcResume:
      begin
        // RESUME [line] - Resume at error line or specified line
        if FInErrorHandler then
        begin
          if Instr.Immediate > 0 then
          begin
            // RESUME <line> with constant line number in Immediate
            FPC := FindPCForSourceLine(Instr.Immediate);
          end
          else if Instr.Src1 > 0 then
          begin
            // RESUME <line> with line number in register
            FPC := FindPCForSourceLine(FIntRegs[Instr.Src1]);
          end
          else if FResumePC >= 0 then
          begin
            // Plain RESUME - resume at error line
            FPC := FResumePC;
          end;
          FInErrorHandler := False;
          Exit;  // Don't increment PC
        end;
        // If not in error handler, just continue
      end;
    bcResumeNext:
      begin
        // RESUME NEXT - Resume at next instruction after error
        if FInErrorHandler and (FResumePC >= 0) then
        begin
          // Jump to the instruction AFTER the one that caused the error
          FPC := FResumePC + 1;
          FInErrorHandler := False;
          Exit;  // Don't increment PC - we already set it
        end;
        // If not in error handler, just continue
      end;
    bcNop: ;
    bcClear: ClearAllVariables;
    // DATA/READ/RESTORE
    bcDataAdd:
      begin
        // Add value to DATA pool
        // Src1 = type (0=Int, 1=Float, 2=String)
        // Immediate = value (int/float bits, or string pool index)
        SetLength(FDataPool, Length(FDataPool) + 1);
        case TSSARegisterType(Instr.Src1) of
          srtInt:
            FDataPool[High(FDataPool)] := Instr.Immediate;
          srtFloat:
            FDataPool[High(FDataPool)] := Double(Pointer(@Instr.Immediate)^);
          srtString:
            FDataPool[High(FDataPool)] := FProgram.StringConstants[Instr.Immediate];
        end;
      end;
    bcDataReadInt:
      begin
        // Read next DATA value into int register
        if FDataIndex < Length(FDataPool) then
        begin
          // Use VarAsType for proper Variant to Int64 conversion
          FIntRegs[Instr.Dest] := VarAsType(FDataPool[FDataIndex], varInt64);
          Inc(FDataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataReadFloat:
      begin
        // Read next DATA value into float register
        if FDataIndex < Length(FDataPool) then
        begin
          // Use VarAsType for proper Variant to Double conversion
          FFloatRegs[Instr.Dest] := VarAsType(FDataPool[FDataIndex], varDouble);
          Inc(FDataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataReadString:
      begin
        // Read next DATA value into string register
        if FDataIndex < Length(FDataPool) then
        begin
          FStringRegs[Instr.Dest] := string(FDataPool[FDataIndex]);
          Inc(FDataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataRestore:
      begin
        // Reset DATA pointer
        // Immediate = line number (0 = beginning, ignored for now - line-specific restore not implemented)
        FDataIndex := 0;
      end;
    // Input commands
    bcGet:
      begin
        // GET A$ - non-blocking character input
        // Returns empty string if no key pressed
        if Assigned(FInputDevice) then
        begin
          FInputDevice.ProcessEvents;
          FStringRegs[Instr.Dest] := FInputDevice.GetLastChar;
        end
        else
          FStringRegs[Instr.Dest] := '';
      end;
    bcGetkey:
      begin
        // GETKEY A$ - blocking character input (waits for any keypress)
        if Assigned(FInputDevice) then
        begin
          FInputDevice.EnableTextInput;
          try
            repeat
              FInputDevice.ProcessEvents;
              // Check for CTRL+C or quit
              if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
              begin
                FRunning := False;
                FInputDevice.ClearStopRequest;
                Break;
              end;
              if FInputDevice.HasChar then
                Break;  // Got a character, exit loop
              // Update screen to show blinking cursor while waiting
              if Assigned(FOutputDevice) then
                FOutputDevice.Present;
              Sleep(10);  // Prevent busy-wait
            until False;
            // Only read character if we didn't exit due to CTRL+C
            if FRunning then
              FStringRegs[Instr.Dest] := FInputDevice.GetLastChar
            else
              FStringRegs[Instr.Dest] := '';
          finally
            FInputDevice.DisableTextInput;
          end;
        end
        else
          FStringRegs[Instr.Dest] := '';
      end;
    // Formatted output
    bcPrintUsing:
      begin
        // PRINT USING format$; value
        // Src1 = format string register, Src2 = value register
        if Assigned(FOutputDevice) then
        begin
          // Apply format string to value
          // This is a simplified implementation - full PRINT USING is more complex
          FOutputDevice.Print(FormatUsingString(FStringRegs[Instr.Src1], FFloatRegs[Instr.Src2]));
        end;
      end;
    bcPudef:
      begin
        // PUDEF " ,.$" - redefine PRINT USING format characters
        // Immediate = string constant index, or Src1 = string register
        if Instr.Src1 <> 0 then
        begin
          // String from register
          if Length(FStringRegs[Instr.Src1]) >= 1 then
            FPudefFiller := FStringRegs[Instr.Src1][1];
          if Length(FStringRegs[Instr.Src1]) >= 2 then
            FPudefComma := FStringRegs[Instr.Src1][2];
          if Length(FStringRegs[Instr.Src1]) >= 3 then
            FPudefDecimal := FStringRegs[Instr.Src1][3];
          if Length(FStringRegs[Instr.Src1]) >= 4 then
            FPudefDollar := FStringRegs[Instr.Src1][4];
        end
        else
        begin
          // String from constant pool
          if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
          begin
            if Length(FProgram.StringConstants[Instr.Immediate]) >= 1 then
              FPudefFiller := FProgram.StringConstants[Instr.Immediate][1];
            if Length(FProgram.StringConstants[Instr.Immediate]) >= 2 then
              FPudefComma := FProgram.StringConstants[Instr.Immediate][2];
            if Length(FProgram.StringConstants[Instr.Immediate]) >= 3 then
              FPudefDecimal := FProgram.StringConstants[Instr.Immediate][3];
            if Length(FProgram.StringConstants[Instr.Immediate]) >= 4 then
              FPudefDollar := FProgram.StringConstants[Instr.Immediate][4];
          end;
        end;
      end;
    bcChar:
      begin
        // CHAR mode, col, row, "text" [,reverse]
        // Src1 = mode, Src2 = col, Dest = row (repurposed)
        // Immediate low 16 bits = text register, high 16 bits = reverse register
        if Assigned(FOutputDevice) then
        begin
          // Output text at specified position
          // This is a simplified implementation
          FOutputDevice.SetCursor(Integer(FIntRegs[Instr.Src2]), Integer(FIntRegs[Instr.Dest]));
          FOutputDevice.Print(FStringRegs[Instr.Immediate and $FFFF]);
        end;
      end;
    bcLoad:
      begin
        // LOAD "filename" - Load and run program from file
        // Src1 = string register with filename
        if Assigned(FOnFileCommand) then
        begin
          FRunning := False;  // Stop current execution
          FOnFileCommand(Self, 'LOAD', FStringRegs[Instr.Src1], FRunning);
          // If Handled is set to True, execution continues; otherwise it stops
        end
        else
          raise Exception.Create('LOAD command not supported: no handler assigned');
      end;
    bcSave:
      begin
        // SAVE "filename" - Save program to file
        // Src1 = string register with filename
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;  // Default: continue after SAVE
          FOnFileCommand(Self, 'SAVE', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('SAVE command not supported: no handler assigned');
      end;
    bcVerify:
      begin
        // VERIFY "filename" - Verify program against file
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          FOnFileCommand(Self, 'VERIFY', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('VERIFY command not supported: no handler assigned');
      end;
    bcBload:
      begin
        // BLOAD "filename" - Load bytecode from file
        if Assigned(FOnFileCommand) then
        begin
          FRunning := False;
          FOnFileCommand(Self, 'BLOAD', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('BLOAD command not supported: no handler assigned');
      end;
    bcBsave:
      begin
        // BSAVE "filename" - Save bytecode to file
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          FOnFileCommand(Self, 'BSAVE', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('BSAVE command not supported: no handler assigned');
      end;
    bcBoot:
      begin
        // BOOT "filename" - Load and run bytecode
        if Assigned(FOnFileCommand) then
        begin
          FRunning := False;
          FOnFileCommand(Self, 'BOOT', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('BOOT command not supported: no handler assigned');
      end;
    bcRun:
      begin
        // RUN [linenum] - Run program
        if Assigned(FOnFileCommand) then
        begin
          FRunning := False;
          FOnFileCommand(Self, 'RUN', IntToStr(Instr.Immediate), FRunning);
        end
        else
          raise Exception.Create('RUN command not supported: no handler assigned');
      end;
    bcList:
      begin
        // LIST [start-end] - List program
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          FOnFileCommand(Self, 'LIST', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('LIST command not supported: no handler assigned');
      end;
    bcNew:
      begin
        // NEW - Clear program
        if Assigned(FOnFileCommand) then
        begin
          FRunning := False;
          FOnFileCommand(Self, 'NEW', '', FRunning);
        end
        else
          raise Exception.Create('NEW command not supported: no handler assigned');
      end;
    bcDelete:
      begin
        // DELETE [start[-end]] - Delete program lines
        // Src1 = start line, Src2 = end line (same as start for single line)
        // Special values: start=0 means from beginning, end=-1 means to end
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          // Format: "start-end" for range, "line" for single line
          if FIntRegs[Instr.Src1] = FIntRegs[Instr.Src2] then
            FOnFileCommand(Self, 'DELETE', IntToStr(FIntRegs[Instr.Src1]), FRunning)
          else
            FOnFileCommand(Self, 'DELETE', IntToStr(FIntRegs[Instr.Src1]) + '-' + IntToStr(FIntRegs[Instr.Src2]), FRunning);
        end
        else
          raise Exception.Create('DELETE command not supported: no handler assigned');
      end;
    bcRenumber:
      begin
        // RENUMBER [new[,inc[,old]]] - Renumber program lines
        // Src1 = new start line (default 10)
        // Src2 = increment (default 10)
        // Immediate = old start line (default 0 = first line)
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          // Format: "new,inc,old"
          FOnFileCommand(Self, 'RENUMBER',
            IntToStr(FIntRegs[Instr.Src1]) + ',' +
            IntToStr(FIntRegs[Instr.Src2]) + ',' +
            IntToStr(FIntRegs[Instr.Immediate and $FFFF]), FRunning);
        end
        else
          raise Exception.Create('RENUMBER command not supported: no handler assigned');
      end;
    bcCatalog:
      begin
        // CATALOG/DIR - List directory (requires console callback)
        if Assigned(FOnFileCommand) then
        begin
          FRunning := True;
          FOnFileCommand(Self, 'CATALOG', FStringRegs[Instr.Src1], FRunning);
        end
        else
          raise Exception.Create('CATALOG command not supported: no handler assigned');
      end;

    // === FILE MANAGEMENT COMMANDS (executed directly in VM) ===
    bcCopyFile:
      begin
        // COPY "src", "dest" [, overwrite]
        // Src1 = source path, Src2 = dest path, Dest = overwrite flag (int reg)
        ExecuteCopyFile(FStringRegs[Instr.Src1], FStringRegs[Instr.Src2],
                       FIntRegs[Instr.Dest] <> 0);
      end;

    bcScratch:
      begin
        // SCRATCH "pattern" [, force]
        // Src1 = pattern, Src2 = force flag (int reg)
        ExecuteScratch(FStringRegs[Instr.Src1], FIntRegs[Instr.Src2] <> 0);
      end;

    bcRenameFile:
      begin
        // RENAME "old", "new"
        // Src1 = old name, Src2 = new name
        ExecuteRenameFile(FStringRegs[Instr.Src1], FStringRegs[Instr.Src2]);
      end;

    bcConcat:
      begin
        // CONCAT "src", "dest"
        // Src1 = source, Src2 = dest (append src to dest)
        ExecuteConcat(FStringRegs[Instr.Src1], FStringRegs[Instr.Src2]);
      end;

    bcMkdir:
      begin
        // MKDIR "path"
        // Src1 = path
        ExecuteMkdir(FStringRegs[Instr.Src1]);
      end;

    bcChdir:
      begin
        // CHDIR "path"
        // Src1 = path
        ExecuteChdir(FStringRegs[Instr.Src1]);
      end;

    bcMoveFile:
      begin
        // MOVE "src", "dest"
        // Src1 = source, Src2 = dest
        ExecuteMoveFile(FStringRegs[Instr.Src1], FStringRegs[Instr.Src2]);
      end;

  end; // case Op (standard bytecode)
end;

procedure TBytecodeVM.ExecuteSuperinstruction(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
begin
  // Superinstructions use sub-opcode (low byte) for dispatch
  // Full opcode is 0xC800 + SubOp (group 200)
  SubOp := Instr.OpCode and $FF;

  case SubOp of
    // Fused compare-and-branch (Int) - sub-opcodes 0-5
    0: // bcBranchEqInt: if (r[src1] == r[src2]) goto target
      if FIntRegs[Instr.Src1] = FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    1: // bcBranchNeInt
      if FIntRegs[Instr.Src1] <> FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    2: // bcBranchLtInt
      if FIntRegs[Instr.Src1] < FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    3: // bcBranchGtInt
      if FIntRegs[Instr.Src1] > FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    4: // bcBranchLeInt
      if FIntRegs[Instr.Src1] <= FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    5: // bcBranchGeInt
      if FIntRegs[Instr.Src1] >= FIntRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;

    // Fused compare-and-branch (Float) - sub-opcodes 10-15
    10: // bcBranchEqFloat
      if FFloatRegs[Instr.Src1] = FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    11: // bcBranchNeFloat
      if FFloatRegs[Instr.Src1] <> FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    12: // bcBranchLtFloat
      if FFloatRegs[Instr.Src1] < FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    13: // bcBranchGtFloat
      if FFloatRegs[Instr.Src1] > FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    14: // bcBranchLeFloat
      if FFloatRegs[Instr.Src1] <= FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;
    15: // bcBranchGeFloat
      if FFloatRegs[Instr.Src1] >= FFloatRegs[Instr.Src2] then
        FPC := Instr.Immediate - 1;

    // Fused arithmetic-to-dest (Int) - sub-opcodes 20-22
    20: // bcAddIntTo: r[dest] += r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] + FIntRegs[Instr.Src1];
    21: // bcSubIntTo: r[dest] -= r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] - FIntRegs[Instr.Src1];
    22: // bcMulIntTo: r[dest] *= r[src1]
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Dest] * FIntRegs[Instr.Src1];

    // Fused arithmetic-to-dest (Float) - sub-opcodes 30-33
    30: // bcAddFloatTo: r[dest] += r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] + FFloatRegs[Instr.Src1];
    31: // bcSubFloatTo: r[dest] -= r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] - FFloatRegs[Instr.Src1];
    32: // bcMulFloatTo: r[dest] *= r[src1]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] * FFloatRegs[Instr.Src1];
    33: // bcDivFloatTo: r[dest] /= r[src1]
      if FFloatRegs[Instr.Src1] <> 0.0 then
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] / FFloatRegs[Instr.Src1]
      else
        raise Exception.Create('Division by zero');

    // Fused constant arithmetic (Int) - sub-opcodes 40-42
    40: // bcAddIntConst: r[dest] = r[src1] + immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] + Instr.Immediate;
    41: // bcSubIntConst: r[dest] = r[src1] - immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] - Instr.Immediate;
    42: // bcMulIntConst: r[dest] = r[src1] * immediate
      FIntRegs[Instr.Dest] := FIntRegs[Instr.Src1] * Instr.Immediate;

    // Fused constant arithmetic (Float) - sub-opcodes 50-53
    50: // bcAddFloatConst
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^);
    51: // bcSubFloatConst
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^);
    52: // bcMulFloatConst
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^);
    53: // bcDivFloatConst
      if Double(Pointer(@Instr.Immediate)^) <> 0.0 then
        FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^)
      else
        raise Exception.Create('Division by zero');

    // Fused compare-zero-and-branch (Int) - sub-opcodes 60-61
    60: // bcBranchEqZeroInt
      if FIntRegs[Instr.Src1] = 0 then
        FPC := Instr.Immediate - 1;
    61: // bcBranchNeZeroInt
      if FIntRegs[Instr.Src1] <> 0 then
        FPC := Instr.Immediate - 1;

    // Fused compare-zero-and-branch (Float) - sub-opcodes 70-71
    70: // bcBranchEqZeroFloat
      if FFloatRegs[Instr.Src1] = 0.0 then
        FPC := Instr.Immediate - 1;
    71: // bcBranchNeZeroFloat
      if FFloatRegs[Instr.Src1] <> 0.0 then
        FPC := Instr.Immediate - 1;

    // Fused array-store-constant - sub-opcodes 80-82
    80: // bcArrayStoreIntConst
      FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] := Instr.Immediate;
    81: // bcArrayStoreFloatConst
      FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^);
    82: // bcArrayStoreStringConst
      FArrays[Instr.Src1].StringData[FIntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate];

    // Fused loop increment-and-branch (Int) - sub-opcodes 90-93
    90: // bcAddIntToBranchLe: r[dest] += r[src1]; if (r[dest] <= r[src2]) goto target
      begin
        Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] <= FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    91: // bcAddIntToBranchLt: r[dest] += r[src1]; if (r[dest] < r[src2]) goto target
      begin
        Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] < FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    92: // bcSubIntToBranchGe: r[dest] -= r[src1]; if (r[dest] >= r[src2]) goto target
      begin
        Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] >= FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;
    93: // bcSubIntToBranchGt: r[dest] -= r[src1]; if (r[dest] > r[src2]) goto target
      begin
        Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
        if FIntRegs[Instr.Dest] > FIntRegs[Instr.Src2] then
          FPC := Instr.Immediate - 1;
      end;

    // FMA (Fused Multiply-Add) - sub-opcodes 100-103
    100: // bcMulAddFloat: dest = c + a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] + FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    101: // bcMulSubFloat: dest = c - a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] - FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    102: // bcMulAddToFloat: dest += a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] + FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];
    103: // bcMulSubToFloat: dest -= a*b
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Dest] - FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2];

    // Array Load + Arithmetic - sub-opcodes 110-112
    110: // bcArrayLoadAddFloat: dest = acc + arr[idx]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]];
    111: // bcArrayLoadSubFloat: dest = acc - arr[idx]
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]];
    112: // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Immediate and $FFFF] +
        FArrays[Instr.Src1].FloatData[FIntRegs[Instr.Src2]] / FFloatRegs[(Instr.Immediate shr 16) and $FFFF];

    // Square-Sum patterns - sub-opcodes 120-121
    120: // bcSquareSumFloat: dest = x*x + y*y
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src1] +
                                FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Src2];
    121: // bcAddSquareFloat: dest = sum + x*x
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Src2];

    // Mul-Mul and Add-Sqrt - sub-opcodes 130-131
    130: // bcMulMulFloat: dest = a*b*c
      FFloatRegs[Instr.Dest] := FFloatRegs[Instr.Src1] * FFloatRegs[Instr.Src2] * FFloatRegs[Instr.Immediate];
    131: // bcAddSqrtFloat: dest = sqrt(a+b)
      FFloatRegs[Instr.Dest] := Sqrt(FFloatRegs[Instr.Src1] + FFloatRegs[Instr.Src2]);

    // Array Load + Branch - sub-opcodes 140-141
    140: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
      if FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] <> 0 then
        FPC := Instr.Immediate - 1;
    141: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
      if FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] = 0 then
        FPC := Instr.Immediate - 1;

    // Array Reverse Range - sub-opcode 156
    156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
      begin
        FStartIdx := FIntRegs[Instr.Src2];
        FEndIdx := FIntRegs[Instr.Dest] - 1;
        FArrIdxTmp := Instr.Src1;
        while FStartIdx < FEndIdx do
        begin
          FSwapTempInt := FArrays[FArrIdxTmp].IntData[FStartIdx];
          FArrays[FArrIdxTmp].IntData[FStartIdx] := FArrays[FArrIdxTmp].IntData[FEndIdx];
          FArrays[FArrIdxTmp].IntData[FEndIdx] := FSwapTempInt;
          Inc(FStartIdx);
          Dec(FEndIdx);
        end;
      end;

    // Array Shift Left - sub-opcode 157
    157: // bcArrayShiftLeft: shift left and rotate first to end+1
      begin
        FStartIdx := FIntRegs[Instr.Src2];
        FEndIdx := FIntRegs[Instr.Dest];
        FArrIdxTmp := Instr.Src1;
        FFirstVal := FArrays[FArrIdxTmp].IntData[FStartIdx];
        FLoopIdx := FStartIdx;
        while FLoopIdx <= FEndIdx do
        begin
          FArrays[FArrIdxTmp].IntData[FLoopIdx] := FArrays[FArrIdxTmp].IntData[FLoopIdx + 1];
          Inc(FLoopIdx);
        end;
        FArrays[FArrIdxTmp].IntData[FEndIdx + 1] := FFirstVal;
      end;

    // Array Swap (Int) - sub-opcode 250
    250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
      begin
        FSwapTempInt := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];
        FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Dest]];
        FArrays[Instr.Src1].IntData[FIntRegs[Instr.Dest]] := FSwapTempInt;
      end;

    // Self-increment/decrement (Int) - sub-opcodes 251-252
    251: // bcAddIntSelf: r[dest] += r[src1]
      Inc(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);
    252: // bcSubIntSelf: r[dest] -= r[src1]
      Dec(FIntRegs[Instr.Dest], FIntRegs[Instr.Src1]);

    // Array Load to register (Int) - sub-opcode 253
    253: // bcArrayLoadIntTo: r[dest] = arr[src1][r[src2]]
      FIntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];

    // Array Copy Element - sub-opcode 254
    254: // bcArrayCopyElement: arr_dest[idx] = arr_src[idx]
      FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[FIntRegs[Instr.Src2]];

    // Array Move Element - sub-opcode 255
    255: // bcArrayMoveElement: arr[dest_idx] = arr[src_idx]
      FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[FIntRegs[Instr.Src1]];

  else
    raise Exception.CreateFmt('Unknown superinstruction sub-opcode %d (full: %d) at PC=%d',
      [SubOp, Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.Step;
var
  Instr: TBytecodeInstruction;
  CurrentSourceLine: Integer;
begin
  if (FProgram = nil) or (FPC >= FProgram.GetInstructionCount) then
  begin
    FRunning := False;
    Exit;
  end;
  Instr := FProgram.GetInstruction(FPC);

  // TRON trace output: print line number when it changes
  // SourceLine > 0 only when compiled with TRON (debug mode) active
  CurrentSourceLine := FProgram.GetSourceLine(FPC);
  if (CurrentSourceLine > 0) and (CurrentSourceLine <> FLastSourceLine) then
  begin
    FLastSourceLine := CurrentSourceLine;
    if Assigned(FOutputDevice) then
      FOutputDevice.Print('[' + IntToStr(CurrentSourceLine) + ']');
  end;

  {$IFDEF ENABLE_PROFILER}
  // Profiler: BeforeInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
    FProfiler.BeforeInstruction(FPC, Instr.OpCode);
  {$ENDIF}

  ExecuteInstruction(Instr);

  {$IFDEF ENABLE_PROFILER}
  // Profiler: AfterInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
  begin
    FProfiler.AfterInstruction(FPC, Instr.OpCode);
    // Track superinstructions
    if Instr.OpCode >= bcGroupSuper then
      FProfiler.OnSuperinstruction(Instr.OpCode, 1);
  end;
  {$ENDIF}

  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  Inc(FInstructionsExecuted);
  {$ENDIF}
  Inc(FPC);
end;

procedure TBytecodeVM.Run;
begin
  // Default execution: use fast mode
  // If profiler is attached or debug needed, caller should use RunDebug
  RunFast;
end;

{ RunFast - Optimized execution loop
  - Direct pointer access to instruction array (no method calls)
  - Inline dispatch (no procedure calls for each instruction)
  - Range checking disabled in critical path
  - No profiler/debug support for maximum speed }
procedure TBytecodeVM.RunFast;
{$I RunTemplate.inc}

{ RunDebug - Debug execution loop
  - Same as RunFast but with TRON trace and profiler support
  - TROFF switches back to RunFast }
{$DEFINE DEBUG_MODE}
procedure TBytecodeVM.RunDebug;
{$I RunTemplate.inc}
{$UNDEF DEBUG_MODE}

{ NOTE: Old RunFast/RunDebug code removed - now generated from RunTemplate.inc }

{ NOTE: Old RunFast/RunSwitchedGoto code removed - now generated from RunTemplate.inc }

{ ============================================================================
  GROUP-SPECIFIC DISPATCH HANDLERS
  These procedures handle opcodes from non-core groups (1-11, 200-255)
  Each group has its own procedure for cleaner organization and better
  instruction cache locality.
  ============================================================================ }

procedure TBytecodeVM.ExecuteStringOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  Len, StartPos, Count: Integer;
  S: string;
begin
  SubOp := Instr.OpCode and $FF;  // Extract sub-opcode (low byte)
  case SubOp of
    0: // bcStrConcat
      FStringRegs[Instr.Dest] := FStringRegs[Instr.Src1] + FStringRegs[Instr.Src2];
    1: // bcStrLen
      FIntRegs[Instr.Dest] := Length(FStringRegs[Instr.Src1]);
    2: // bcStrLeft
      begin
        Len := FIntRegs[Instr.Src2];
        if Len < 0 then Len := 0;
        FStringRegs[Instr.Dest] := Copy(FStringRegs[Instr.Src1], 1, Len);
      end;
    3: // bcStrRight
      begin
        Len := FIntRegs[Instr.Src2];
        S := FStringRegs[Instr.Src1];
        if Len < 0 then Len := 0;
        if Len > Length(S) then Len := Length(S);
        FStringRegs[Instr.Dest] := Copy(S, Length(S) - Len + 1, Len);
      end;
    4: // bcStrMid - MID$(s, start, len)
      begin
        // Src2 = start position register (int)
        // Immediate = length register index (low 16 bits)
        StartPos := FIntRegs[Instr.Src2];
        Count := FIntRegs[Instr.Immediate and $FFFF];
        if StartPos < 1 then StartPos := 1;
        if Count < 0 then Count := 0;
        FStringRegs[Instr.Dest] := Copy(FStringRegs[Instr.Src1], StartPos, Count);
      end;
    5: // bcStrAsc
      begin
        S := FStringRegs[Instr.Src1];
        if Length(S) > 0 then
          FIntRegs[Instr.Dest] := Ord(S[1])
        else
          FIntRegs[Instr.Dest] := 0;
      end;
    6: // bcStrChr
      FStringRegs[Instr.Dest] := Chr(FIntRegs[Instr.Src1] and $FF);
    7: // bcStrStr - STR$(n)
      FStringRegs[Instr.Dest] := FConsoleBehavior.FormatNumber(FFloatRegs[Instr.Src1]);
    8: // bcStrVal - VAL(s)
      begin
        S := Trim(FStringRegs[Instr.Src1]);
        if not TryStrToFloat(S, FFloatRegs[Instr.Dest]) then
          FFloatRegs[Instr.Dest] := 0.0;
      end;
    9: // bcStrHex - HEX$(n) - full INT64 range, no leading zeros
      begin
        S := IntToHex(FIntRegs[Instr.Src1], 1);  // Minimum 1 digit
        // IntToHex with digits=1 still pads, so trim leading zeros
        while (Length(S) > 1) and (S[1] = '0') do
          Delete(S, 1, 1);
        FStringRegs[Instr.Dest] := S;
      end;
    10: // bcStrInstr - INSTR(haystack, needle [,start])
      begin
        // Src1 = haystack, Src2 = needle, Immediate = start position (1-based)
        StartPos := 1;
        if Instr.Immediate > 0 then
          StartPos := FIntRegs[Instr.Immediate and $FFFF];
        if StartPos < 1 then StartPos := 1;
        FIntRegs[Instr.Dest] := Pos(FStringRegs[Instr.Src2],
          Copy(FStringRegs[Instr.Src1], StartPos, MaxInt));
        if FIntRegs[Instr.Dest] > 0 then
          Inc(FIntRegs[Instr.Dest], StartPos - 1);
      end;
    11: // bcStrErr - ERR$(n)
      FStringRegs[Instr.Dest] := SedaiExecutorErrors.GetErrorCodeDescription(FIntRegs[Instr.Src1]);
  else
    raise Exception.CreateFmt('Unknown string opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteMathOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcMathSin
      FFloatRegs[Instr.Dest] := Sin(FFloatRegs[Instr.Src1]);
    1: // bcMathCos
      FFloatRegs[Instr.Dest] := Cos(FFloatRegs[Instr.Src1]);
    2: // bcMathTan
      FFloatRegs[Instr.Dest] := Tan(FFloatRegs[Instr.Src1]);
    3: // bcMathAtn
      FFloatRegs[Instr.Dest] := ArcTan(FFloatRegs[Instr.Src1]);
    4: // bcMathLog
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := Ln(FFloatRegs[Instr.Src1])
      else
        raise Exception.Create('LOG of non-positive number');
    5: // bcMathExp
      FFloatRegs[Instr.Dest] := Exp(FFloatRegs[Instr.Src1]);
    6: // bcMathSqr
      begin
        if FFloatRegs[Instr.Src1] < 0 then
          raise Exception.CreateFmt('Square root of negative number: %.17e', [FFloatRegs[Instr.Src1]])
        else
          FFloatRegs[Instr.Dest] := Sqrt(FFloatRegs[Instr.Src1]);
      end;
    7: // bcMathAbs
      FFloatRegs[Instr.Dest] := Abs(FFloatRegs[Instr.Src1]);
    8: // bcMathSgn
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := 1
      else if FFloatRegs[Instr.Src1] < 0 then
        FFloatRegs[Instr.Dest] := -1
      else
        FFloatRegs[Instr.Dest] := 0;
    9: // bcMathInt
      FFloatRegs[Instr.Dest] := Floor(FFloatRegs[Instr.Src1]);
    10: // bcMathRnd
      FFloatRegs[Instr.Dest] := Random;
    11: // bcMathLog10
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := Log10(FFloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOG10 of non-positive number');
    12: // bcMathLog2
      if FFloatRegs[Instr.Src1] > 0 then
        FFloatRegs[Instr.Dest] := Log2(FFloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOG2 of non-positive number');
    13: // bcMathLogN
      begin
        // LOGN(base, x) - Src1 = base, Src2 = x
        if (FFloatRegs[Instr.Src1] > 0) and (FFloatRegs[Instr.Src1] <> 1) and (FFloatRegs[Instr.Src2] > 0) then
          FFloatRegs[Instr.Dest] := LogN(FFloatRegs[Instr.Src1], FFloatRegs[Instr.Src2])
        else if FFloatRegs[Instr.Src1] <= 0 then
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN base must be positive')
        else if FFloatRegs[Instr.Src1] = 1 then
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN base cannot be 1')
        else
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN of non-positive number');
      end;
    14: // bcStrDec - DEC(hexstring) - convert hex string to decimal integer
      begin
        // Src1 is string register, Dest is int register
        // FStringRegs is used, result goes to FIntRegs
        try
          FIntRegs[Instr.Dest] := StrToInt64('$' + FStringRegs[Instr.Src1]);
        except
          on E: Exception do
            raise Exception.CreateFmt('?ILLEGAL QUANTITY ERROR: Invalid hex string "%s"', [FStringRegs[Instr.Src1]]);
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown math opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteArrayOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ArrayIdx, LinearIdx, i, ProdDims: Integer;
  ArrInfo: TSSAArrayInfo;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcArrayLoad (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        case FArrays[ArrayIdx].ElementType of
          0: FIntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
          1: FFloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
          2: FStringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
        end;
      end;
    1: // bcArrayStore (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        case FArrays[ArrayIdx].ElementType of
          0: FArrays[ArrayIdx].IntData[LinearIdx] := FIntRegs[Instr.Dest];
          1: FArrays[ArrayIdx].FloatData[LinearIdx] := FFloatRegs[Instr.Dest];
          2: FArrays[ArrayIdx].StringData[LinearIdx] := FStringRegs[Instr.Dest];
        end;
      end;
    2: // bcArrayDim
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= FProgram.GetArrayCount) then
          raise Exception.CreateFmt('Invalid array index: %d', [ArrayIdx]);
        ArrInfo := FProgram.GetArray(ArrayIdx);
        if ArrayIdx >= Length(FArrays) then
          SetLength(FArrays, ArrayIdx + 1);
        FArrays[ArrayIdx].ElementType := Byte(ArrInfo.ElementType);
        FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
        SetLength(FArrays[ArrayIdx].Dimensions, ArrInfo.DimCount);
        for i := 0 to ArrInfo.DimCount - 1 do
        begin
          if ArrInfo.Dimensions[i] = 0 then
          begin
            if (i < Length(ArrInfo.DimRegisters)) and (ArrInfo.DimRegisters[i] >= 0) then
            begin
              case ArrInfo.DimRegTypes[i] of
                srtInt: FArrays[ArrayIdx].Dimensions[i] := FIntRegs[ArrInfo.DimRegisters[i]] + 1;
                srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(FFloatRegs[ArrInfo.DimRegisters[i]]) + 1;
              else
                raise Exception.CreateFmt('Invalid dimension register type for array %s', [ArrInfo.Name]);
              end;
            end
            else
              raise Exception.CreateFmt('Array %s has undefined variable dimension %d', [ArrInfo.Name, i]);
          end
          else
            FArrays[ArrayIdx].Dimensions[i] := ArrInfo.Dimensions[i];
        end;
        ProdDims := 1;
        for i := 0 to ArrInfo.DimCount - 1 do
          ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[i];
        FArrays[ArrayIdx].TotalSize := ProdDims;
        case ArrInfo.ElementType of
          srtInt:
            begin
              SetLength(FArrays[ArrayIdx].IntData, ProdDims);
              for i := 0 to ProdDims - 1 do FArrays[ArrayIdx].IntData[i] := 0;
            end;
          srtFloat:
            begin
              SetLength(FArrays[ArrayIdx].FloatData, ProdDims);
              for i := 0 to ProdDims - 1 do FArrays[ArrayIdx].FloatData[i] := 0.0;
            end;
          srtString:
            begin
              SetLength(FArrays[ArrayIdx].StringData, ProdDims);
              for i := 0 to ProdDims - 1 do FArrays[ArrayIdx].StringData[i] := '';
            end;
        end;
      end;
    3: // bcArrayLoadInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FIntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    4: // bcArrayLoadFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FFloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    5: // bcArrayLoadString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FStringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    6: // bcArrayStoreInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].IntData[LinearIdx] := FIntRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
    7: // bcArrayStoreFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].FloatData[LinearIdx] := FFloatRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
    8: // bcArrayStoreString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := FIntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].StringData[LinearIdx] := FStringRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
  else
    raise Exception.CreateFmt('Unknown array opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteIOOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  PrintStr, InputStr: string;
  InputVal: Double;
  NextTabCol, TabIdx: Integer;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcPrint (float)
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FFloatRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    1: // bcPrintLn (float)
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FFloatRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FOutputDevice.Present;
        FCursorCol := 0;
      end;
    2: // bcPrintString
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    3: // bcPrintStringLn
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatString(FStringRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FOutputDevice.Present;
        FCursorCol := 0;
      end;
    4: // bcPrintInt
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FIntRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        Inc(FCursorCol, Length(PrintStr));
      end;
    5: // bcPrintIntLn
      if Assigned(FOutputDevice) then
      begin
        PrintStr := FConsoleBehavior.FormatNumber(FIntRegs[Instr.Src1]);
        FOutputDevice.Print(PrintStr);
        FOutputDevice.NewLine;
        FOutputDevice.Present;
        FCursorCol := 0;
      end;
    6: // bcPrintComma
      if Assigned(FOutputDevice) then
      begin
        NextTabCol := FConsoleBehavior.GetNextTabPosition(FCursorCol);
        if NextTabCol = 0 then
        begin
          FOutputDevice.NewLine;
          FCursorCol := 0;
        end
        else if FConsoleBehavior.CommaAction = caTabZone then
        begin
          while FCursorCol < NextTabCol do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caFixedSpaces then
        begin
          for TabIdx := 1 to FConsoleBehavior.CommaSpaces do
          begin
            FOutputDevice.Print(' ');
            Inc(FCursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caNewLine then
        begin
          FOutputDevice.NewLine;
          FCursorCol := 0;
        end;
      end;
    7: // bcPrintSemicolon
      if Assigned(FOutputDevice) then
      begin
        case FConsoleBehavior.SemicolonAction of
          saNoSpace: ;
          saSpaceAfter, saSpaceBoth:
            begin
              FOutputDevice.Print(' ');
              Inc(FCursorCol);
            end;
          saSpaceBefore: ;
        end;
      end;
    8: // bcPrintTab
      if Assigned(FOutputDevice) then
      begin
        // TAB(n) positions cursor at column n (0-indexed)
        // TAB(0) = first column, TAB(20) = 21st column
        // If cursor is already at or past column n, do nothing (no wrap)
        NextTabCol := FIntRegs[Instr.Src1];
        if NextTabCol < 0 then NextTabCol := 0;
        // Only move forward if we're before the target column
        while FCursorCol < NextTabCol do
        begin
          FOutputDevice.Print(' ');
          Inc(FCursorCol);
        end;
        // If FCursorCol >= NextTabCol, do nothing (as per C128 behavior)
      end;
    9: // bcPrintSpc
      if Assigned(FOutputDevice) then
      begin
        // Src1 = register containing space count (always a register from SSA)
        TabIdx := FIntRegs[Instr.Src1];
        while TabIdx > 0 do
        begin
          FOutputDevice.Print(' ');
          Inc(FCursorCol);
          Dec(TabIdx);
        end;
      end;
    10: // bcPrintNewLine
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.NewLine;
        FCursorCol := 0;
      end;
    11: // bcPrintEnd - Reset reverse mode after PRINT (C128 behavior)
      if Assigned(FOutputDevice) then
        FOutputDevice.ResetPrintState;
    12: // bcInput (generic float)
      if Assigned(FInputDevice) then
      begin
        // Print initial prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
            FOutputDevice.Print(FStringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars
          InputStr := FInputDevice.ReadLine('? ', False, not FC128InputMode, True);
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            FRunning := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            FFloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
              FOutputDevice.Print(FStringRegs[Instr.Src1]);
          end;
        until False;
      end;
    13: // bcInputInt
      if Assigned(FInputDevice) then
      begin
        // Print initial prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
            FOutputDevice.Print(FStringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars (no decimal for int)
          InputStr := Trim(FInputDevice.ReadLine('? ', False, not FC128InputMode, False));
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            FRunning := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            if (InputVal >= Low(Int64)) and (InputVal <= High(Int64)) then
            begin
              FIntRegs[Instr.Dest] := Trunc(InputVal);
              Break;
            end
            else if Assigned(FOutputDevice) then
            begin
              FOutputDevice.Print('?REDO FROM START');
              FOutputDevice.NewLine;
              // Reprint prompt for retry
              if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
                FOutputDevice.Print(FStringRegs[Instr.Src1]);
            end;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
              FOutputDevice.Print(FStringRegs[Instr.Src1]);
          end;
        until False;
      end;
    14: // bcInputFloat
      if Assigned(FInputDevice) then
      begin
        // Print initial prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
            FOutputDevice.Print(FStringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars
          InputStr := Trim(FInputDevice.ReadLine('? ', False, not FC128InputMode, True));
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            FRunning := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            FFloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
              FOutputDevice.Print(FStringRegs[Instr.Src1]);
          end;
        until False;
      end;
    15: // bcInputString
      if Assigned(FInputDevice) then
      begin
        // Print prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(FStringRegs)) then
            FOutputDevice.Print(FStringRegs[Instr.Src1]);
        end;
        FStringRegs[Instr.Dest] := FInputDevice.ReadLine('? ', False, False, False);
        if FInputDevice.ShouldStop then
        begin
          FRunning := False;
          FInputDevice.ClearStopRequest;
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown I/O opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteSpecialVarOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  TimeCurrentTime: TDateTime;
  TimeH, TimeM, TimeS, TimeMS: Word;
  TimeStr: string;
  TimeHH, TimeMM, TimeSS: Integer;
  TimeTargetMs, TimeCurrentMs: Int64;
  TimeCH, TimeCM, TimeCS, TimeCMS: Word;
  DateY, DateM, DateD: Word;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcLoadTI - return jiffies (1/60 sec) since interpreter start
      FIntRegs[Instr.Dest] := ((GetTickCount64 - FStartTicks) * 60) div 1000;
    1: // bcLoadTIS - return current time as "HHMMSS" string
      begin
        TimeCurrentTime := Now + (FTimeOffset / 86400000);
        DecodeTime(TimeCurrentTime, TimeH, TimeM, TimeS, TimeMS);
        FStringRegs[Instr.Dest] := Format('%.2d%.2d%.2d', [TimeH, TimeM, TimeS]);
      end;
    2: // bcStoreTIS - set time offset
      begin
        TimeStr := FStringRegs[Instr.Src1];
        if Length(TimeStr) >= 6 then
        begin
          TimeHH := StrToIntDef(Copy(TimeStr, 1, 2), 0);
          TimeMM := StrToIntDef(Copy(TimeStr, 3, 2), 0);
          TimeSS := StrToIntDef(Copy(TimeStr, 5, 2), 0);
          if TimeHH > 23 then TimeHH := 23;
          if TimeMM > 59 then TimeMM := 59;
          if TimeSS > 59 then TimeSS := 59;
          TimeTargetMs := (Int64(TimeHH) * 3600 + Int64(TimeMM) * 60 + Int64(TimeSS)) * 1000;
          TimeCurrentTime := Now;
          DecodeTime(TimeCurrentTime, TimeCH, TimeCM, TimeCS, TimeCMS);
          TimeCurrentMs := (Int64(TimeCH) * 3600 + Int64(TimeCM) * 60 + Int64(TimeCS)) * 1000 + TimeCMS;
          FTimeOffset := TimeTargetMs - TimeCurrentMs;
        end;
      end;
    3: // bcLoadDTS - return current date as "YYYYMMDD" string
      begin
        DecodeDate(Date, DateY, DateM, DateD);
        FStringRegs[Instr.Dest] := Format('%.4d%.2d%.2d', [DateY, DateM, DateD]);
      end;
    4: // bcFre - return available memory in bytes
      begin
        {$IFDEF WINDOWS}
        FIntRegs[Instr.Dest] := GetFPCHeapStatus.CurrHeapFree;
        {$ELSE}
        FIntRegs[Instr.Dest] := GetFPCHeapStatus.CurrHeapFree;
        {$ENDIF}
      end;
    5: // bcLoadEL - return last error line number
      FIntRegs[Instr.Dest] := FLastErrorLine;
    6: // bcLoadER - return last error code
      FIntRegs[Instr.Dest] := FLastErrorCode;
    7: // bcLoadERRS - return last error message (variable form)
      FStringRegs[Instr.Dest] := FLastErrorMessage;
    8: // bcPeek - read from memory-mapped location
      begin
        if Assigned(FMemoryMapper) then
          FIntRegs[Instr.Dest] := FMemoryMapper.Peek(FIntRegs[Instr.Src1])
        else
          FIntRegs[Instr.Dest] := 0;  // No memory mapper = return 0
      end;
    9: // bcPoke - write to memory-mapped location
      begin
        if Assigned(FMemoryMapper) then
          FMemoryMapper.Poke(FIntRegs[Instr.Src1], FIntRegs[Instr.Src2]);
        // If no memory mapper, silently ignore (like real hardware)
      end;
  else
    raise Exception.CreateFmt('Unknown special variable opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteGraphicsOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  DrawMode: Integer;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcGraphicRGBA
      FIntRegs[Instr.Dest] :=
        ((FIntRegs[Instr.Immediate and $FFFF] and $FF) shl 24) or
        ((FIntRegs[Instr.Src1] and $FF) shl 16) or
        ((FIntRegs[Instr.Src2] and $FF) shl 8) or
        (FIntRegs[(Instr.Immediate shr 16) and $FFFF] and $FF);
    1: // bcGraphicSetMode
      if Assigned(FOutputDevice) then
        FOutputDevice.SetGraphicMode(
          TGraphicMode(FIntRegs[Instr.Src1] and $F),
          FIntRegs[Instr.Src2] <> 0,
          FIntRegs[Instr.Immediate and $FFFF]
        );
    2: // bcGraphicBox
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.DrawBoxWithColor(
          FIntRegs[Instr.Src2],
          FIntRegs[Instr.Dest],
          FIntRegs[(Instr.Immediate) and $FFF],
          FIntRegs[(Instr.Immediate shr 12) and $FFF],
          UInt32(FIntRegs[Instr.Src1]),
          FFloatRegs[(Instr.Immediate shr 24) and $FFF],
          FIntRegs[(Instr.Immediate shr 36) and $FFF] <> 0
        );
        FOutputDevice.Present;
      end;
    3: // bcGraphicCircle
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.DrawCircleWithColor(
          FIntRegs[Instr.Src2],
          FIntRegs[Instr.Dest],
          FIntRegs[(Instr.Immediate) and $3FF],
          FIntRegs[(Instr.Immediate shr 10) and $3FF],
          UInt32(FIntRegs[Instr.Src1]),
          FFloatRegs[(Instr.Immediate shr 20) and $3FF],
          FFloatRegs[(Instr.Immediate shr 30) and $3FF],
          FFloatRegs[(Instr.Immediate shr 40) and $3FF],
          FFloatRegs[(Instr.Immediate shr 50) and $3FF]
        );
        FOutputDevice.Present;
      end;
    4: // bcGraphicDraw
      if Assigned(FOutputDevice) then
      begin
        DrawMode := Instr.Immediate and $7FFF;
        case DrawMode of
          0: FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
          1:
            begin
              FOutputDevice.DrawLine(
                FOutputDevice.GetPixelCursorX,
                FOutputDevice.GetPixelCursorY,
                FIntRegs[Instr.Src2],
                FIntRegs[Instr.Dest],
                UInt32(FIntRegs[Instr.Src1])
              );
              FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
            end;
          2:
            begin
              FOutputDevice.SetPixel(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest], UInt32(FIntRegs[Instr.Src1]));
              FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
            end;
        end;
        FOutputDevice.Present;
      end;
    5: // bcGraphicLocate
      if Assigned(FOutputDevice) then
        FOutputDevice.SetPixelCursor(FIntRegs[Instr.Src1], FIntRegs[Instr.Src2]);
    6: // bcGraphicRdot
      if Assigned(FOutputDevice) then
      begin
        case FIntRegs[Instr.Src1] of
          0: FIntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorX;
          1: FIntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorY;
          2: FIntRegs[Instr.Dest] := FOutputDevice.GetPixelIndex(
               FOutputDevice.GetPixelCursorX, FOutputDevice.GetPixelCursorY);
        else
          FIntRegs[Instr.Dest] := 0;
        end;
      end
      else
        FIntRegs[Instr.Dest] := 0;
    7: // bcGraphicGetMode
      if Assigned(FOutputDevice) then
      begin
        case FIntRegs[Instr.Src1] of
          0: FIntRegs[Instr.Dest] := Ord(FOutputDevice.GetGraphicMode);
        else
          FIntRegs[Instr.Dest] := 0;
        end;
      end
      else
        FIntRegs[Instr.Dest] := 0;
    8: // bcGraphicColor - COLOR source, color
      if Assigned(FOutputDevice) then
      begin
        // Src1 = source (0-6), Src2 = color
        FOutputDevice.SetColorSource(FIntRegs[Instr.Src1], FIntRegs[Instr.Src2]);
      end;
    9: // bcGraphicWidth - WIDTH n (1 or 2)
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.SetLineWidth(FIntRegs[Instr.Src1]);
      end;
    10: // bcGraphicScale - SCALE n [,xmax, ymax]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = enable (0/1), Src2 = xmax, Dest = ymax
        FOutputDevice.SetScale(FIntRegs[Instr.Src1] <> 0, FIntRegs[Instr.Src2], FIntRegs[Instr.Dest]);
      end;
    11: // bcGraphicPaint - PAINT source, x, y, mode
      if Assigned(FOutputDevice) then
      begin
        // Src1 = source, Src2 = x, Dest = y, Immediate = mode
        // All parameters are INT registers
        FOutputDevice.FloodFill(FIntRegs[Instr.Src1],
          FIntRegs[Instr.Src2], FIntRegs[Instr.Dest], FIntRegs[Instr.Immediate and $FFFF]);
      end;
    12: // bcGraphicWindow - WINDOW col1, row1, col2, row2 [,clear]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = col1, Src2 = row1, Dest = col2
        // Immediate bits 0-15 = row2 register, bits 16-31 = clear register
        FOutputDevice.SetWindow(FIntRegs[Instr.Src1], FIntRegs[Instr.Src2],
          FIntRegs[Instr.Dest], FIntRegs[Instr.Immediate and $FFFF],
          FIntRegs[(Instr.Immediate shr 16) and $FFFF] <> 0);
      end;
    13: // bcGraphicSShape - SSHAPE A$, x1, y1 [,x2, y2]
      if Assigned(FOutputDevice) then
      begin
        // Dest = string reg index, Src1 = x1, Src2 = y1 (INT)
        // Immediate bits 0-15 = x2, bits 16-31 = y2 (INT)
        FStringRegs[Instr.Dest] := FOutputDevice.SaveShape(
          FIntRegs[Instr.Src1], FIntRegs[Instr.Src2],
          FIntRegs[Instr.Immediate and $FFFF], FIntRegs[(Instr.Immediate shr 16) and $FFFF]);
      end;
    14: // bcGraphicGShape - GSHAPE A$, x, y [,mode]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = string reg index, Src2 = x, Dest = y (INT), Immediate = mode
        FOutputDevice.LoadShape(FStringRegs[Instr.Src1],
          FIntRegs[Instr.Src2], FIntRegs[Instr.Dest], Instr.Immediate);
        FOutputDevice.Present;
      end;
    15: // bcGraphicGList - GLIST
      begin
        // List SDL2 video modes - TODO: Implement actual mode listing via FOutputDevice
      end;
    16: // bcGraphicPos - POS(x)
      begin
        // Return cursor column position (0-indexed, consistent with TAB)
        // Use FCursorCol which is tracked by the VM during PRINT operations
        FIntRegs[Instr.Dest] := FCursorCol;
      end;
    17: // bcGraphicRclr - RCLR(n)
      if Assigned(FOutputDevice) then
      begin
        // Return color of source n
        FIntRegs[Instr.Dest] := FOutputDevice.GetColorSourceDirect(FIntRegs[Instr.Src1]);
      end
      else
        FIntRegs[Instr.Dest] := 0;
    18: // bcGraphicRwindow - RWINDOW(n)
      if Assigned(FOutputDevice) then
      begin
        // Return window info: 0=lines, 1=cols, 2=screen width
        case FIntRegs[Instr.Src1] of
          0: FIntRegs[Instr.Dest] := FOutputDevice.GetWindowLines;
          1: FIntRegs[Instr.Dest] := FOutputDevice.GetWindowCols;
          2: FIntRegs[Instr.Dest] := FOutputDevice.GetScreenWidth;
        else
          FIntRegs[Instr.Dest] := 0;
        end;
      end
      else
        FIntRegs[Instr.Dest] := 0;
    19: // bcSetColor - SETCOLOR index, R, G, B [, A]
      if Assigned(FOutputDevice) then
      begin
        // Src1=index, Src2=R, Dest=G, Immediate: B(12) | A(12)
        FOutputDevice.SetPaletteColorRGBA(
          FIntRegs[Instr.Src1],                              // index
          Byte(FIntRegs[Instr.Src2]),                        // R
          Byte(FIntRegs[Instr.Dest]),                        // G
          Byte(FIntRegs[Instr.Immediate and $FFF]),          // B
          Byte(FIntRegs[(Instr.Immediate shr 12) and $FFF])  // A
        );
      end;
    20: // bcGetColor - GETCOLOR(index)
      // Returns RGBA value from palette at given index (0-255)
      if Assigned(FOutputDevice) then
        FIntRegs[Instr.Dest] := Int64(FOutputDevice.GetPaletteColor(FIntRegs[Instr.Src1]))
      else
        FIntRegs[Instr.Dest] := 0;
    21: // bcScnClr - SCNCLR [mode]
      if Assigned(FOutputDevice) then
        FOutputDevice.ClearScreen(FIntRegs[Instr.Src1]);
    22: // bcPLoad - PLOAD "filename"
      if Assigned(FOutputDevice) then
      begin
        if not FOutputDevice.LoadPaletteFromJSON(FStringRegs[Instr.Src1]) then
        begin
          // Set error state for BASIC error handling
          FLastErrorMessage := FOutputDevice.GetLastPaletteError;
          FLastErrorCode := 100;  // Palette error code
          FLastErrorLine := FProgram.GetSourceLine(FPC);
        end;
      end;
    23: // bcPSave - PSAVE "filename"
      if Assigned(FOutputDevice) then
      begin
        if not FOutputDevice.SavePaletteToJSON(FStringRegs[Instr.Src1]) then
        begin
          // Set error state for BASIC error handling
          FLastErrorMessage := FOutputDevice.GetLastPaletteError;
          FLastErrorCode := 101;  // Palette save error code
          FLastErrorLine := FProgram.GetSourceLine(FPC);
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown graphics opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteSoundOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  {$IFDEF WITH_SEDAI_AUDIO}
  VoiceIdx: Integer;
  DurationMs: Integer;
  {$ENDIF}
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcSoundVol
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized and Assigned(FSIDEvo) and Assigned(FAudioBackend) then
      begin
        FAudioBackend.Lock;
        try
          FSIDEvo.SetMasterVolume(FIntRegs[Instr.Src1] / 15.0);
        finally
          FAudioBackend.Unlock;
        end;
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    1: // bcSoundSound
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized and Assigned(FSIDEvo) and Assigned(FAudioBackend) then
      begin
        // SOUND voice, freq, duration [,dir, minfreq, sweeptime, waveform, pulsewidth]
        // Src1 = voice (int), Src2 = freq (int, SID frequency 0-65535), Dest = duration (int in jiffies)
        // Immediate bits 32-39 = waveform (0=triangle, 1=saw, 2=pulse, 3=noise)
        VoiceIdx := FIntRegs[Instr.Src1] - 1;
        DurationMs := FIntRegs[Instr.Dest] * 1000 div 60;

        FAudioBackend.Lock;
        try
          // Convert SID frequency to Hz: SID_value * PAL_clock / 16777216
          // Simplified: SID_value * 0.0596 (for PAL 985248 Hz clock)
          FSIDEvo.SetFrequencyHz(VoiceIdx, FIntRegs[Instr.Src2] * 0.0596);
          case (Instr.Immediate shr 32) and $FF of
            0: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_TRIANGLE);
            1: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_SAWTOOTH);
            2: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_PULSE);
            3: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_NOISE);
          else
            FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_SAWTOOTH);
          end;
          FSIDEvo.GateOn(VoiceIdx);
        finally
          FAudioBackend.Unlock;
        end;

        // Wait for duration (outside lock to allow callback to run)
        if DurationMs > 0 then
        begin
          CooperativeSleep(DurationMs);
          FAudioBackend.Lock;
          try
            FSIDEvo.GateOff(VoiceIdx);
          finally
            FAudioBackend.Unlock;
          end;
        end;
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    2: // bcSoundEnvelope
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized then
        if (FIntRegs[Instr.Src1] >= 0) and (FIntRegs[Instr.Src1] <= 9) then
        begin
          FAudioEnvelopes[FIntRegs[Instr.Src1]].Attack := ((Instr.Immediate) and $FF) / 15.0;
          FAudioEnvelopes[FIntRegs[Instr.Src1]].Decay := ((Instr.Immediate shr 8) and $FF) / 15.0;
          FAudioEnvelopes[FIntRegs[Instr.Src1]].Sustain := ((Instr.Immediate shr 16) and $FF) / 15.0;
          FAudioEnvelopes[FIntRegs[Instr.Src1]].Release := ((Instr.Immediate shr 24) and $FF) / 15.0;
          FAudioEnvelopes[FIntRegs[Instr.Src1]].Waveform := (Instr.Immediate shr 32) and $FF;
          FAudioEnvelopes[FIntRegs[Instr.Src1]].PulseWidth := ((Instr.Immediate shr 40) and $FFF) / 4095.0;
        end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    3: // bcSoundTempo
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized then
      begin
        FAudioTempo := FIntRegs[Instr.Src1];
        if FAudioTempo < 1 then FAudioTempo := 1;
        if FAudioTempo > 255 then FAudioTempo := 255;
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    4: // bcSoundPlay
      {$IFDEF WITH_SEDAI_AUDIO}
      begin
        {$IFDEF DEBUG_AUDIO}
        WriteLn('[DEBUG_AUDIO] PLAY called, AudioInit=', FAudioInitialized, ' String="', FStringRegs[Instr.Src1], '"');
        {$ENDIF}
        if FAudioInitialized then
          ExecutePlayString(FStringRegs[Instr.Src1]);
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    5: // bcSoundFilter
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized and Assigned(FSIDEvo) and Assigned(FAudioBackend) then
      begin
        FAudioBackend.Lock;
        try
          // FILTER cutoff, lowpass, bandpass, highpass, resonance
          // Src1 = cutoff frequency register (float)
          // Src2 = lowpass register (int 0/1)
          // Dest = bandpass register (int 0/1)
          // Immediate bits 0-7 = highpass register index (int 0/1)
          // Immediate bits 8-15 = resonance register index (int 0-15)
          // Set filter mode (LP, BP, HP as booleans)
          FSIDEvo.SetFilterMode(
            FIntRegs[Instr.Src2] <> 0,                    // lowpass
            FIntRegs[Instr.Dest] <> 0,                    // bandpass
            FIntRegs[Instr.Immediate and $FF] <> 0       // highpass
          );
          // Set cutoff: convert Hz (0-20000) to 11-bit value (0-2047)
          FSIDEvo.SetFilterCutoff(Round(FFloatRegs[Instr.Src1] / 20000.0 * 2047));
          // Set resonance: 0-15 range
          FSIDEvo.SetFilterResonance(FIntRegs[(Instr.Immediate shr 8) and $FF] and $0F);
        finally
          FAudioBackend.Unlock;
        end;
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
  else
    raise Exception.CreateFmt('Unknown sound opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteSpriteOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  SpriteNum, Enabled, Priority, Mode: Integer;
  X, Y, ScaleX, ScaleY, Dist, Angle, Speed: Double;
  Color, MC1, MC2: Integer;
begin
  { Group 7: Sprite operations (0x07xx)
    Opcodes:
      0 = SPRITE n [,enabled] [,color] [,priority] [,scalex] [,scaley] [,mode]
      1 = MOVSPR n, x, y (absolute)
      2 = MOVSPR n, +x, +y (relative)
      3 = MOVSPR n, dist;angle (polar)
      4 = MOVSPR n, angle#speed (auto)
      5 = SPRCOLOR [mc1] [,mc2]
      6 = SPRSAV src, dst
      7 = COLLISION type [,line]
      8 = BUMP(n)
      9 = RSPCOLOR(n)
      10 = RSPPOS(sprite, n)
      11 = RSPRITE(sprite, n)

    Register encoding:
      SPRITE: Src1=n, Src2=enabled, Dest=color
              Immediate bits: priority(12) | scalex(12) | scaley(12) | mode(12)
      MOVSPR*: Src1=n, Src2=x/dist/angle, Src3=y/angle/speed (mapped to Dest)
      SPRCOLOR: Src1=mc1, Src2=mc2
      SPRSAV: Src1=src, Src2=dst
      COLLISION: Src1=type, Src2=line
  }

  SubOp := Instr.OpCode and $FF;

  case SubOp of
    0: // bcSprite
      begin
        // SPRITE n [,enabled] [,color] [,priority] [,scalex] [,scaley] [,mode]
        SpriteNum := Round(FFloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;

        Enabled := 1;  // Default enabled
        if Instr.Src2 <> 0 then
          Enabled := Round(FFloatRegs[Instr.Src2]);

        Color := 1;  // Default color
        if Instr.Dest <> 0 then
          Color := Round(FFloatRegs[Instr.Dest]);

        Priority := (Instr.Immediate) and $FFF;
        ScaleX := 1.0;
        ScaleY := 1.0;
        Mode := 0;

        // TODO: When ISpriteManager is implemented, call:
        // FSpriteManager.SetSprite(SpriteNum, Enabled, Color, Priority, ScaleX, ScaleY, Mode);
      end;

    1: // bcMovsprAbs
      begin
        // MOVSPR n, x, y (absolute position)
        SpriteNum := Round(FFloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        X := FFloatRegs[Instr.Src2];
        Y := FFloatRegs[Instr.Dest];  // Dest is repurposed for y

        // TODO: FSpriteManager.MoveSpriteAbs(SpriteNum, X, Y);
      end;

    2: // bcMovsprRel
      begin
        // MOVSPR n, +x, +y (relative movement)
        SpriteNum := Round(FFloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        X := FFloatRegs[Instr.Src2];
        Y := FFloatRegs[Instr.Dest];

        // TODO: FSpriteManager.MoveSpriteRel(SpriteNum, X, Y);
      end;

    3: // bcMovsprPolar
      begin
        // MOVSPR n, dist;angle (polar movement)
        SpriteNum := Round(FFloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        Dist := FFloatRegs[Instr.Src2];
        Angle := FFloatRegs[Instr.Dest];

        // TODO: FSpriteManager.MoveSpritePolar(SpriteNum, Dist, Angle);
      end;

    4: // bcMovsprAuto
      begin
        // MOVSPR n, angle#speed (automatic continuous movement)
        SpriteNum := Round(FFloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        Angle := FFloatRegs[Instr.Src2];
        Speed := FFloatRegs[Instr.Dest];

        // TODO: FSpriteManager.MoveSpriteAuto(SpriteNum, Angle, Speed);
      end;

    5: // bcSprcolor
      begin
        // SPRCOLOR [mc1] [,mc2]
        MC1 := 0;
        MC2 := 0;
        if Instr.Src1 <> 0 then
          MC1 := Round(FFloatRegs[Instr.Src1]);
        if Instr.Src2 <> 0 then
          MC2 := Round(FFloatRegs[Instr.Src2]);

        // TODO: FSpriteManager.SetSpriteMulticolors(MC1, MC2);
      end;

    6: // bcSprsav
      begin
        // SPRSAV src, dst (save/load sprite data)
        // TODO: Implement sprite data save/load
      end;

    7: // bcCollision
      begin
        // COLLISION type [,line]
        // TODO: FSpriteManager.SetCollisionHandler(Type, LineNumber);
      end;

    8: // bcBump - function returning collision bitmask
      begin
        // BUMP(n) - returns collision bitmask
        // TODO: Result := FSpriteManager.GetCollisionStatus(n);
        FIntRegs[Instr.Dest] := 0;  // No collision for now
      end;

    9: // bcRspcolor - function returning multicolor value
      begin
        // RSPCOLOR(n) - returns multicolor n (1 or 2)
        // TODO: Result := FSpriteManager.GetMulticolor(n);
        FIntRegs[Instr.Dest] := 0;
      end;

    10: // bcRsppos - function returning sprite position/speed
      begin
        // RSPPOS(sprite, attr) - returns position/speed
        // attr: 0=X, 1=Y, 2=speed
        // TODO: Result := FSpriteManager.GetSpritePosition(sprite, attr);
        FFloatRegs[Instr.Dest] := 0.0;
      end;

    11: // bcRsprite - function returning sprite attribute
      begin
        // RSPRITE(sprite, attr) - returns sprite attribute
        // attr: 0=enabled, 1=color, 2=priority, 3=scalex, 4=scaley, 5=mode
        // TODO: Result := FSpriteManager.GetSpriteAttribute(sprite, attr);
        FIntRegs[Instr.Dest] := 0;
      end;

  else
    raise Exception.CreateFmt('Unknown sprite opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

procedure TBytecodeVM.ExecuteFileIOOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ErrorCode: Integer;
  HandleNum: Integer;
  HandleName, Filename, Mode, Data: string;
begin
  { Group 6: File I/O operations (0x06xx)
    Opcodes:
      0 = DOPEN #handle, "filename" [, mode$]
      1 = DCLOSE #handle
      2 = OPEN (legacy, maps to DOPEN)
      3 = CLOSE (legacy, maps to DCLOSE)

    Register encoding:
      DOPEN: Dest = handle register (int), Src1 = filename register (string),
             Src2 = mode register (string, optional), Immediate = handle name string index
      DCLOSE: Dest = handle register (int), Immediate = handle name string index
  }

  SubOp := Instr.OpCode and $FF;
  ErrorCode := 0;

  case SubOp of
    0, 2: // bcDopen, bcOpen
      begin
        // DOPEN #handle, "filename" [, mode$]
        HandleNum := FIntRegs[Instr.Dest];
        Filename := FStringRegs[Instr.Src1];

        // Mode is optional, default to "R" (read)
        if Instr.Src2 > 0 then
          Mode := FStringRegs[Instr.Src2]
        else
          Mode := 'R';

        // Handle name (for named handles like #MYFILE)
        if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
          HandleName := FProgram.StringConstants[Instr.Immediate]
        else
          HandleName := '';

        if Assigned(FOnDiskFile) then
        begin
          FOnDiskFile(Self, 'DOPEN', HandleNum, HandleName, Filename, Mode, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('DOPEN error %d opening file: %s', [ErrorCode, Filename]);
        end
        else
          raise Exception.Create('DOPEN command not supported: no handler assigned');
      end;

    1, 3: // bcDclose, bcClose
      begin
        // DCLOSE #handle
        HandleNum := FIntRegs[Instr.Dest];

        // Handle name (for named handles)
        if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
          HandleName := FProgram.StringConstants[Instr.Immediate]
        else
          HandleName := '';

        if Assigned(FOnDiskFile) then
        begin
          FOnDiskFile(Self, 'DCLOSE', HandleNum, HandleName, '', '', ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('DCLOSE error %d closing handle: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('DCLOSE command not supported: no handler assigned');
      end;

    4: // bcGetFile - GET# file, var
      begin
        { GET# file, var - Read one character from file
          Dest = file handle register (int)
          Src1 = variable register index to store result (string) }
        HandleNum := FIntRegs[Instr.Dest];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'GET#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('GET# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Store result in string register
          if Instr.Src1 >= 0 then
            FStringRegs[Instr.Src1] := Data;
        end
        else
          raise Exception.Create('GET# command not supported: no handler assigned');
      end;

    5: // bcInputFile - INPUT# file, vars
      begin
        { INPUT# file, var - Read data from file
          Dest = file handle register (int)
          Src1 = variable register index to store result }
        HandleNum := FIntRegs[Instr.Dest];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'INPUT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('INPUT# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Store result in string register
          if Instr.Src1 >= 0 then
            FStringRegs[Instr.Src1] := Data;
        end
        else
          raise Exception.Create('INPUT# command not supported: no handler assigned');
      end;

    6: // bcPrintFile - PRINT# file, exprs
      begin
        { PRINT# file, data - Write data to file
          Dest = file handle register (int)
          Src1 = data register (string) }
        HandleNum := FIntRegs[Instr.Dest];
        Data := FStringRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# error %d writing to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('PRINT# command not supported: no handler assigned');
      end;

    7: // bcCmd - CMD file [, expr]
      begin
        { CMD file - Redirect output to file
          Dest = file handle register (int)
          When handle is 0 or PRINT# is called without data, output returns to screen }
        HandleNum := FIntRegs[Instr.Dest];

        // If PRINT# is called alone (empty data), restore output to screen
        if (Instr.Src1 >= 0) and (FStringRegs[Instr.Src1] = '') then
        begin
          FCmdHandle := 0;  // Reset to screen
        end
        else
        begin
          FCmdHandle := HandleNum;  // Redirect output to this file

          // If there's initial data to write, send it
          if (Instr.Src1 >= 0) and (FStringRegs[Instr.Src1] <> '') then
          begin
            Data := FStringRegs[Instr.Src1];
            if Assigned(FOnFileData) then
            begin
              FOnFileData(Self, 'CMD', HandleNum, Data, ErrorCode);
              if ErrorCode <> 0 then
                raise Exception.CreateFmt('CMD error %d: %d', [ErrorCode, HandleNum]);
            end;
          end;
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown file I/O opcode %d at PC=%d', [Instr.OpCode, FPC]);
  end;
end;

{ ========== FILE MANAGEMENT COMMANDS (executed directly in VM) ========== }

procedure TBytecodeVM.ExecuteCopyFile(const Src, Dest: string; Overwrite: Boolean);
var
  SrcStream, DstStream: TFileStream;
  SearchRec: TSearchRec;
  SrcDir, SrcPattern, SrcFullPath, DstFullPath: string;
  HasWildcard: Boolean;
begin
  // Check for wildcards in source
  HasWildcard := (Pos('*', Src) > 0) or (Pos('?', Src) > 0);

  // Extract directory and pattern
  SrcDir := ExtractFilePath(Src);
  if SrcDir = '' then
    SrcDir := GetCurrentDir;
  SrcPattern := ExtractFileName(Src);

  if HasWildcard then
  begin
    // Wildcard copy - destination must be a directory
    if not DirectoryExists(Dest) then
      raise Exception.Create('?DESTINATION MUST BE A DIRECTORY FOR WILDCARDS');

    if FindFirst(IncludeTrailingPathDelimiter(SrcDir) + SrcPattern,
                 faAnyFile and not faDirectory, SearchRec) = 0 then
    begin
      try
        repeat
          SrcFullPath := IncludeTrailingPathDelimiter(SrcDir) + SearchRec.Name;
          DstFullPath := IncludeTrailingPathDelimiter(Dest) + SearchRec.Name;

          // Check overwrite
          if FileExists(DstFullPath) and not Overwrite then
            Continue;

          // Copy file
          SrcStream := TFileStream.Create(SrcFullPath, fmOpenRead or fmShareDenyWrite);
          try
            DstStream := TFileStream.Create(DstFullPath, fmCreate);
            try
              DstStream.CopyFrom(SrcStream, SrcStream.Size);
            finally
              DstStream.Free;
            end;
          finally
            SrcStream.Free;
          end;
        until FindNext(SearchRec) <> 0;
      finally
        SysUtils.FindClose(SearchRec);
      end;
    end
    else
      raise Exception.Create('?FILE NOT FOUND');
  end
  else
  begin
    // Single file copy
    if not FileExists(Src) then
      raise Exception.Create('?FILE NOT FOUND: ' + ExtractFileName(Src));

    // Determine destination
    if DirectoryExists(Dest) then
      DstFullPath := IncludeTrailingPathDelimiter(Dest) + ExtractFileName(Src)
    else
      DstFullPath := Dest;

    // Check overwrite
    if FileExists(DstFullPath) and not Overwrite then
      raise Exception.Create('?FILE EXISTS: ' + ExtractFileName(DstFullPath));

    // Copy file
    SrcStream := TFileStream.Create(Src, fmOpenRead or fmShareDenyWrite);
    try
      DstStream := TFileStream.Create(DstFullPath, fmCreate);
      try
        DstStream.CopyFrom(SrcStream, SrcStream.Size);
      finally
        DstStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  end;
end;

procedure TBytecodeVM.ExecuteScratch(const Pattern: string; Force: Boolean);
var
  SearchRec: TSearchRec;
  SrcDir, SrcPattern, FullPath: string;
begin
  // Extract directory and pattern
  SrcDir := ExtractFilePath(Pattern);
  if SrcDir = '' then
    SrcDir := GetCurrentDir;
  SrcPattern := ExtractFileName(Pattern);

  if FindFirst(IncludeTrailingPathDelimiter(SrcDir) + SrcPattern,
               faAnyFile and not faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        FullPath := IncludeTrailingPathDelimiter(SrcDir) + SearchRec.Name;

        // Delete file
        if not SysUtils.DeleteFile(FullPath) then
        begin
          if not Force then
            raise Exception.Create('?CANNOT DELETE: ' + SearchRec.Name);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end
  else
  begin
    if not Force then
      raise Exception.Create('?FILE NOT FOUND');
  end;
end;

procedure TBytecodeVM.ExecuteRenameFile(const OldName, NewName: string);
begin
  if not FileExists(OldName) then
    raise Exception.Create('?FILE NOT FOUND: ' + ExtractFileName(OldName));

  if FileExists(NewName) then
    raise Exception.Create('?FILE EXISTS: ' + ExtractFileName(NewName));

  if not SysUtils.RenameFile(OldName, NewName) then
    raise Exception.Create('?CANNOT RENAME FILE');
end;

procedure TBytecodeVM.ExecuteConcat(const Src, Dest: string);
var
  SrcStream, DstStream: TFileStream;
begin
  // Source must exist
  if not FileExists(Src) then
    raise Exception.Create('?FILE NOT FOUND: ' + ExtractFileName(Src));

  // Destination must exist (we append to it)
  if not FileExists(Dest) then
    raise Exception.Create('?FILE NOT FOUND: ' + ExtractFileName(Dest));

  // Open source for reading
  SrcStream := TFileStream.Create(Src, fmOpenRead or fmShareDenyWrite);
  try
    // Open destination for appending
    DstStream := TFileStream.Create(Dest, fmOpenReadWrite or fmShareDenyWrite);
    try
      DstStream.Seek(0, soFromEnd);  // Move to end
      DstStream.CopyFrom(SrcStream, SrcStream.Size);
    finally
      DstStream.Free;
    end;
  finally
    SrcStream.Free;
  end;
end;

procedure TBytecodeVM.ExecuteMkdir(const Path: string);
begin
  if DirectoryExists(Path) then
    raise Exception.Create('?DIRECTORY EXISTS: ' + Path);

  if not ForceDirectories(Path) then
    raise Exception.Create('?CANNOT CREATE DIRECTORY: ' + Path);
end;

procedure TBytecodeVM.ExecuteChdir(const Path: string);
begin
  if not DirectoryExists(Path) then
    raise Exception.Create('?DIRECTORY NOT FOUND: ' + Path);

  if not SetCurrentDir(Path) then
    raise Exception.Create('?CANNOT CHANGE DIRECTORY: ' + Path);
end;

procedure TBytecodeVM.ExecuteMoveFile(const Src, Dest: string);
var
  DstFullPath: string;
begin
  if not FileExists(Src) then
    raise Exception.Create('?FILE NOT FOUND: ' + ExtractFileName(Src));

  // Determine destination
  if DirectoryExists(Dest) then
    DstFullPath := IncludeTrailingPathDelimiter(Dest) + ExtractFileName(Src)
  else
    DstFullPath := Dest;

  if FileExists(DstFullPath) then
    raise Exception.Create('?FILE EXISTS: ' + ExtractFileName(DstFullPath));

  // Try rename first (works if same volume)
  if not SysUtils.RenameFile(Src, DstFullPath) then
  begin
    // If rename fails (different volumes), copy then delete
    ExecuteCopyFile(Src, DstFullPath, False);
    if not SysUtils.DeleteFile(Src) then
      raise Exception.Create('?CANNOT DELETE SOURCE AFTER MOVE');
  end;
end;

procedure TBytecodeVM.Continue;
begin
  if not FStopped then
    raise Exception.Create('?CAN''T CONTINUE ERROR');
  if FProgram = nil then
    raise Exception.Create('?CAN''T CONTINUE ERROR');
  if FStoppedPC >= FProgram.GetInstructionCount then
    raise Exception.Create('?CAN''T CONTINUE ERROR');

  // Resume execution from saved position
  FPC := FStoppedPC;
  FRunning := True;
  FStopped := False;

  // Continue the execution loop
  while FRunning and (FPC < FProgram.GetInstructionCount) do Step;

  // Reset FAST mode when program ends
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;

{$IFDEF WEB_MODE}
procedure TBytecodeVM.SetWebContext(AContext: TObject);
begin
  FWebContext := AContext;
end;

procedure TBytecodeVM.ExecuteWebOp(const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ParamName, Value: string;
  WebCtx: TWebContext;
begin
  { Group 8: Web operations (0x08xx) - WEB_MODE only
    Opcodes:
      $01 = GET$("name")      - HTML-escaped query parameter
      $02 = POST$("name")     - HTML-escaped POST parameter
      $03 = GETRAW$("name")   - raw query parameter
      $04 = POSTRAW$("name")  - raw POST parameter
      $05 = HTML$(s)          - escape HTML entities
      $06 = URL$(s)           - URL encode
      $07 = METHOD$           - "GET" or "POST"
      $08 = PATH$             - requested path
      $09 = QUERY$            - full query string
      $0A = HEADER$("name")   - request header
      $0B = SETHEADER         - set response header
      $0C = STATUS            - set HTTP status code
  }

  if not Assigned(FWebContext) then
    raise Exception.Create('Web context not initialized');

  WebCtx := TWebContext(FWebContext);
  SubOp := Instr.OpCode and $FF;

  case SubOp of
    $01: // bcWebGetParam - GET$("name")
      begin
        ParamName := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := WebCtx.GetParam(ParamName);
      end;

    $02: // bcWebPostParam - POST$("name")
      begin
        ParamName := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := WebCtx.PostParam(ParamName);
      end;

    $03: // bcWebGetRaw - GETRAW$("name")
      begin
        ParamName := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := WebCtx.GetParamRaw(ParamName);
      end;

    $04: // bcWebPostRaw - POSTRAW$("name")
      begin
        ParamName := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := WebCtx.PostParamRaw(ParamName);
      end;

    $05: // bcWebHtmlEncode - HTML$(s)
      begin
        Value := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := HtmlEncode(Value);
      end;

    $06: // bcWebUrlEncode - URL$(s)
      begin
        Value := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := UrlEncode(Value);
      end;

    $07: // bcWebMethod - METHOD$
      begin
        FStringRegs[Instr.Dest] := WebCtx.Method;
      end;

    $08: // bcWebPath - PATH$
      begin
        FStringRegs[Instr.Dest] := WebCtx.Path;
      end;

    $09: // bcWebQuery - QUERY$
      begin
        FStringRegs[Instr.Dest] := WebCtx.QueryString;
      end;

    $0A: // bcWebHeader - HEADER$("name")
      begin
        ParamName := FStringRegs[Instr.Src1];
        FStringRegs[Instr.Dest] := WebCtx.GetHeader(ParamName);
      end;

    $0B: // bcWebSetHeader - SETHEADER name, value
      begin
        ParamName := FStringRegs[Instr.Src1];
        Value := FStringRegs[Instr.Src2];
        WebCtx.SetResponseHeader(ParamName, Value);
      end;

    $0C: // bcWebStatus - STATUS code
      begin
        WebCtx.ResponseStatus := FIntRegs[Instr.Src1];
      end;
  else
    raise Exception.CreateFmt('Unknown web opcode $%x at PC=%d', [SubOp, FPC]);
  end;
end;
{$ENDIF}

end.
