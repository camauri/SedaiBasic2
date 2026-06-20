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
  SedaiMemoryMapper, SedaiSpriteTypes, SedaiExecutionContext, SedaiDrawQueue
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

  { SPRDEF callback: runs the modal sprite editor for the given sprite number.
    Returns True if the VM should stop (e.g. the window was closed). }
  TSpriteEditorCallback = function(SpriteNum: Integer): Boolean of object;

  { Array storage structure }
  TArrayStorage = record
    ElementType: Byte;        // 0=Int, 1=Float, 2=String (maps to TSSARegisterType)
    DimCount: Integer;
    Dimensions: array of Integer;   // element count per dimension
    LowerBounds: array of Integer;  // lower bound per dimension (B1.4: LBOUND/UBOUND)
    TotalSize: Integer;
    IntData: array of Int64;
    FloatData: array of Double;
    StringData: array of string;
  end;

  { TRecordStorage moved to SedaiExecutionContext (M5.2b: the record heap is per-context). }

  { Bytecode VM - register-based virtual machine }
  TBytecodeVM = class
  private
    // M5.1: all per-thread-of-control execution state (register banks, PC, call/frame
    // stacks, transfer slots, RAII marks, error/TRAP state, scratch) lives in FCtx, so a
    // future worker thread can run its own context over the shared program/heap (S16.2).
    // The VM owns exactly one context; the single-threaded run uses it — a no-op relocation.
    FCtx: TExecutionContext;
    // M5.3 (render command queue, S16.3): the rule once OS threads land (M5.2) is that only
    // the render-owner thread touches the SDL device. Worker threads enqueue graphics/sprite
    // opcodes here (resolved); the owner drains them at the present cadence. FHasWorkers gates
    // the whole mechanism: it is False until M5.2, so today every graphics op runs inline on
    // the owner thread and the queue/guard add no behaviour and no overhead (provable no-op).
    FDrawQueue: TDrawCommandQueue;
    FDrainCtx: TExecutionContext;       // scratch context used to replay a drained command
    FRenderOwnerThreadId: TThreadID;    // the thread allowed to render (set when Run begins)
    FHasWorkers: Boolean;               // True once a worker context has been spawned (M5.2)
    // M5.2: live worker threads. Each entry is a TWorkerSpawn (declared in the implementation);
    // a Threadcreate handle is (index + 1), 0 = invalid. Guarded by FWorkerLock since a worker may
    // itself spawn/join. The table is append-only for the program's life (joined entries stay,
    // so a stale handle is harmless); contexts/spawn records are freed in the VM destructor.
    FWorkerThreads: array of TObject;
    FWorkerLock: TRTLCriticalSection;
    // M5.4: mutex table. Each entry is a heap-allocated TRTLCriticalSection (pointer kept stable so a
    // held lock survives table growth); a Mutexcreate handle is (index + 1), 0 = invalid, nil = destroyed.
    // FMutexTableLock guards only the table (lookup/append), never the user mutex itself.
    FMutexes: array of Pointer;
    FMutexTableLock: TRTLCriticalSection;
    // M5.4: condition-variable table. Each entry is a TCondVar (declared in the implementation),
    // handle = index + 1, nil = destroyed. FCondTableLock guards only the table; each TCondVar has its
    // own internal lock + per-waiter RTLEvent list (see CondWait/CondSignal/CondBroadcast).
    FCondVars: array of Pointer;
    FCondTableLock: TRTLCriticalSection;
    // M5.2c: shared UDT-record region for cross-thread record access. Records reachable through shared
    // storage (arrays of UDT — their handles live in the global FArrays) are allocated here instead of a
    // thread's per-context heap, so any thread routes field access to the same instance. Each entry is a
    // separately heap-allocated record (stable pointer → a handle stays valid when the outer array grows).
    // A handle with SHARED_REC_FLAG set indexes here; otherwise it indexes the active context's heap.
    FSharedRecords: array of PRecordStorage;
    FSharedRecordCount: Integer;
    FSharedRecLock: TRTLCriticalSection;
    FProgram: TBytecodeProgram;
    FOutputDevice: IOutputDevice;
    FInputDevice: IInputDevice;
    FMemoryMapper: IMemoryMapper;  // Memory-mapped PEEK/POKE support
    FConsoleBehavior: TConsoleBehavior;
    FOwnsConsoleBehavior: Boolean;
    // Time tracking for TI and TI$
    FStartTicks: QWord;     // Milliseconds since system start when VM started
    FTimeOffset: Int64;     // TI$ offset in milliseconds from real time
    FLastFrameTick: QWord;  // Last FRAME sync tick for drift-free timing
    // Function key definitions (1-12)
    FFunctionKeys: array[1..12] of string;
    FVarMap: TStringList;
    FArrays: array of TArrayStorage;
    // UDT/record heap is per-context (FCtx.Records / FCtx.RecordCount) since M5.2b.
    // DATA pool for DATA/READ/RESTORE statements (the read cursor FCtx.DataIndex is per-context)
    FDataPool: array of Variant;
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
    // Sprite manager (nil in CLI mode — sprite commands become no-ops)
    FSpriteManager: ISpriteManager;
    // Event polling callback for UI responsiveness
    FEventPollCallback: TEventPollCallback;
    FEventPollInterval: Integer;
    // SPRDEF modal sprite editor callback (set by the SDL console; nil elsewhere)
    FSpriteEditorCallback: TSpriteEditorCallback;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
    FInstructionsExecuted: Int64;
    {$ENDIF}
    {$IFDEF ENABLE_PROFILER}
    FProfiler: TProfiler;
    {$ENDIF}
    // Debugger support (always available, but only used in DEBUG_MODE via RunDebug)
    FDebugger: TSedaiDebugger;
    // Error state for EL, ER, ERR$ (FCtx.LastError*) and TRAP/RESUME (FCtx.Trap*) is per-context.
    FTrueValue: Int64;            // TRUE value: -1 (Commodore BASIC) or 1 (modern BASIC)
    FC128InputMode: Boolean;      // True = C128 mode (accept all, show ?REDO), False = input mask (reject invalid chars)
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
    procedure ExecutePlayString(Ctx: TExecutionContext; const MusicStr: string);
    procedure CooperativeSleep(Ctx: TExecutionContext; Milliseconds: Integer);
    {$ENDIF}
    procedure ExecuteInstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSuperinstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    // Group-specific dispatch handlers
    procedure ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure EraseArray(ArrayIdx: Integer);                                   // B1.4: ERASE
    procedure RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean);       // B1.4: REDIM

    procedure ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpecialVarOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSoundOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpriteOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteFileIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$IFDEF WEB_MODE}
    procedure ExecuteWebOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$ENDIF}
    // File management operations (executed directly in VM)
    procedure ExecuteCopyFile(const Src, Dest: string; Overwrite: Boolean);
    procedure ExecuteScratch(const Pattern: string; Force: Boolean; Silent: Boolean = False);
    procedure ExecuteRenameFile(const OldName, NewName: string);
    procedure ExecuteConcat(const Src, Dest: string);
    procedure ExecuteMkdir(const Path: string);
    procedure ExecuteChdir(const Path: string);
    procedure ExecuteMoveFile(const Src, Dest: string);
    procedure InitializeRegisters;
    procedure ClearAllVariables;
    procedure EnsureRegisterCapacity(Ctx: TExecutionContext; RegType: TSSARegisterType; MinIndex: Integer);
    procedure FramePush(Ctx: TExecutionContext);   // bcCallSub: snapshot whole register banks
    procedure FramePop(Ctx: TExecutionContext);    // bcReturnSub: restore whole register banks
    // M5.2 OS threading: spawn/join workers running their own TExecutionContext over the shared
    // program/heap (FreeBASIC shared-memory model). SetupWorkerContext sizes a fresh context's banks;
    // SpawnWorker BeginThreads a worker (returns the handle); JoinWorker waits on it; RunWorker is the
    // worker-thread body (Spawn is a TWorkerSpawn) that primes a synthetic call frame and runs the loop.
    procedure SetupWorkerContext(WCtx: TExecutionContext);
    function SpawnWorker(EntryPC: Int64; SpawnerCtx: TExecutionContext): Int64;
    procedure JoinWorker(Handle: Int64);
    procedure DetachWorker(Handle: Int64);   // M5.5: mark a worker detached (not explicitly joined)
    procedure RunWorker(Spawn: TObject);
    procedure CleanupWorkers;   // join any survivors + free spawn records/contexts (destructor)
    // M5.4 mutexes (FB API): thin wrappers over TRTLCriticalSection, addressed by integer handle.
    function CreateMutex: Int64;
    procedure LockMutex(Handle: Int64);
    procedure UnlockMutex(Handle: Int64);
    procedure DestroyMutex(Handle: Int64);
    procedure CleanupMutexes;   // free any surviving mutexes (destructor)
    // M5.4 condition variables (FB API): a mutex-released wait + signal/broadcast, built on per-waiter
    // RTLEvents (sticky, so no lost wakeup). CondWait takes the cond and the user's mutex handle.
    function CreateCond: Int64;
    procedure CondWaitOp(CondHandle, MutexHandle: Int64);
    procedure CondSignalOp(CondHandle: Int64);
    procedure CondBroadcastOp(CondHandle: Int64);
    procedure DestroyCond(CondHandle: Int64);
    procedure CleanupConds;     // free any surviving condition variables (destructor)
    function AllocRecord(Ctx: TExecutionContext; IntC, FloatC, StrC, TypeId: Integer): Integer;  // M3: new record instance -> handle
    // M5.2c: allocate in the shared region (cross-thread); ResolveRec routes a handle to its record.
    function AllocSharedRecord(IntC, FloatC, StrC, TypeId: Integer): Int64;
    function ResolveRec(Ctx: TExecutionContext; Handle: Int64): PRecordStorage; inline;
    procedure CleanupSharedRecords;   // free the shared region (destructor)
    procedure RecordNewArrayInit(Ctx: TExecutionContext; ArrayId: Integer; PackedCounts: Int64);  // M3.1: fill UDT array
    procedure CheckFloatValid(Ctx: TExecutionContext; RegIndex: Integer; const OpName: string);
    function FormatUsingString(const FormatStr: string; Value: Double): string;
    // M5.1: per-context accessors for the read-only PC/Running/Stopped/LastError* properties.
    function GetPC: Integer;
    function GetRunning: Boolean;
    function GetStopped: Boolean;
    function GetLastErrorLine: Integer;
    function GetLastErrorCode: Integer;
    function GetLastErrorMessage: string;
    // M5.3: render command queue. No-ops on the single-threaded path (FHasWorkers = False).
    function IsRenderOwner: Boolean; inline;
    procedure EnqueueDeferredOp(Ctx: TExecutionContext; Kind: TDrawCommandKind; const Instr: TBytecodeInstruction);
    procedure DrainDrawQueue;
    procedure PresentFrame;  // drain deferred draws (if any) then present — the per-frame hook
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadProgram(Program_: TBytecodeProgram);
    procedure ClearProgram;  // Clear program reference (use before freeing the program externally)
    procedure SetOutputDevice(Device: IOutputDevice);
    procedure SetInputDevice(Device: IInputDevice);
    procedure SetMemoryMapper(Mapper: IMemoryMapper);
    procedure SetSpriteManager(Manager: ISpriteManager);
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
    property PC: Integer read GetPC;
    property Running: Boolean read GetRunning;
    property Stopped: Boolean read GetStopped;  // True if program was stopped by STOP
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
    property LastErrorLine: Integer read GetLastErrorLine;
    property LastErrorCode: Integer read GetLastErrorCode;
    property LastErrorMessage: string read GetLastErrorMessage;
    // TRUE value for comparisons (-1 = Commodore BASIC, 1 = modern BASIC)
    procedure SetTrueValue(AValue: Int64);
    property TrueValue: Int64 read FTrueValue write FTrueValue;
    // C128 INPUT mode: True = accept all then show ?REDO FROM START, False = input mask
    property C128InputMode: Boolean read FC128InputMode write FC128InputMode;
    // Function key definitions (for console expansion)
    function GetFunctionKey(KeyNum: Integer): string;
    // Event polling callback (for deferred rendering during VM execution)
    property EventPollCallback: TEventPollCallback read FEventPollCallback write FEventPollCallback;
    property EventPollInterval: Integer read FEventPollInterval write FEventPollInterval;
    // SPRDEF modal sprite editor callback (set by the SDL console; nil = no-op)
    property SpriteEditorCallback: TSpriteEditorCallback read FSpriteEditorCallback write FSpriteEditorCallback;
  end;

implementation

type
  // M5.2: one record per spawned worker thread. Carries everything the RTL thread function needs:
  // the VM (shared program/heap/runtime), the worker's own TExecutionContext, the SUB entry PC and
  // the int parameter. Lives in the VM's FWorkerThreads table for the program's lifetime.
  TWorkerSpawn = class
    VM: TBytecodeVM;
    Ctx: TExecutionContext;
    EntryPC: Integer;
    ThreadId: TThreadID;
    Handle: Int64;       // M5.5: this worker's Threadcreate handle (so it can answer THREADSELF)
    Joined: Boolean;
    Detached: Boolean;   // M5.5: THREADDETACH — won't be explicitly joined (cleaned up at program end)
  end;

  // M5.4: a mutex is a heap-allocated critical section, kept by pointer so a held lock survives
  // table growth (the dynamic array stores these pointers; the records themselves never move).
  PMutex = ^TRTLCriticalSection;

  // M5.4: a condition variable. ILock guards Waiters, a FIFO of per-waiter RTLEvents. Each waiter
  // gets its own event (sticky → a set-before-wait still wakes it, so no lost wakeup), so broadcast
  // is just "set them all". The associated user mutex is passed to CondWait, not stored here.
  TCondVar = class
    ILock: TRTLCriticalSection;
    Waiters: array of PRTLEvent;
  end;

const
  // M5.2c: a record handle with this bit set lives in the VM shared region (cross-thread); the
  // remaining bits are the index. Plain handles (bit clear) index the active context's per-thread heap.
  SHARED_REC_FLAG = Int64(1) shl 62;
  SHARED_REC_MASK = SHARED_REC_FLAG - 1;

threadvar
  // M5.2: the execution context the current thread runs. nil on the main thread (which uses the VM's
  // FCtx); set by WorkerThreadEntry to the worker's own context before it enters the run loop. Read
  // once per Run (RunTemplate.inc) so the hot path stays register-direct — the point of M5.2a.
  GActiveCtx: TExecutionContext;
  // M5.5: the current thread's Threadcreate handle (THREADSELF reads it). 0 on the main thread.
  GSelfHandle: Int64;

function WorkerThreadEntry(p: Pointer): PtrInt;
// RTL thread entry (BeginThread): bind this thread's active context, run the worker SUB, then exit.
var
  Spawn: TWorkerSpawn;
begin
  Spawn := TWorkerSpawn(p);
  GActiveCtx := Spawn.Ctx;
  GSelfHandle := Spawn.Handle;   // M5.5: THREADSELF inside this worker returns its own handle
  try
    Spawn.VM.RunWorker(Spawn);
  except
    // A worker must never propagate an exception past the RTL thread boundary (it would abort the
    // process). v1: swallow it — the join still completes. (Proper per-thread error reporting: M5.5.)
  end;
  GActiveCtx := nil;
  Result := 0;
end;

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
  // M5.1: the per-context execution state must exist before any field below is touched.
  FCtx := TExecutionContext.Create;
  // M5.3: render command queue + scratch replay context. Dormant until M5.2 sets FHasWorkers.
  FDrawQueue := TDrawCommandQueue.Create;
  FDrainCtx := TExecutionContext.Create;
  FRenderOwnerThreadId := GetCurrentThreadID;
  FHasWorkers := False;
  // M5.2: the main context starts at the program EntryPoint (StartPC = -1); workers override it.
  FCtx.StartPC := -1;
  FDrainCtx.StartPC := -1;
  SetLength(FWorkerThreads, 0);
  InitCriticalSection(FWorkerLock);
  SetLength(FMutexes, 0);
  InitCriticalSection(FMutexTableLock);
  SetLength(FCondVars, 0);
  InitCriticalSection(FCondTableLock);
  SetLength(FSharedRecords, 0);
  FSharedRecordCount := 0;
  InitCriticalSection(FSharedRecLock);
  FProgram := nil;
  FCtx.PC := 0;
  FCtx.Running := False;
  FCtx.CallStackPtr := 0;
  FCtx.FrameSaveIntTop := 0;
  FCtx.FrameSaveFloatTop := 0;
  FCtx.FrameSaveStrTop := 0;
  FCtx.FrameRecBaseTop := 0;
  FCtx.BlockRecMarkTop := 0;
  SetLength(FCtx.Records, 0);
  FCtx.RecordCount := 0;
  FCtx.CursorCol := 0;
  // Initialize time tracking
  FStartTicks := GetTickCount64;
  FTimeOffset := 0;
  FLastFrameTick := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  SetLength(FCtx.CallStack, 256);
  // Transfer-register banks (M2): fixed capacity is plenty (slots = per-bank parameter
  // counts of a single call, which is small).
  SetLength(FCtx.XferInt, 256);
  SetLength(FCtx.XferFloat, 256);
  SetLength(FCtx.XferStr, 256);
  FVarMap := TStringList.Create;
  FVarMap.Sorted := True;
  // Create default console behavior (Commodore 64 style)
  FConsoleBehavior := TConsolePresets.CreateCommodore64;
  FOwnsConsoleBehavior := True;
  // Initialize CMD handle (0 = output to screen)
  FCmdHandle := 0;
  // Initialize event polling (nil = disabled)
  FEventPollCallback := nil;
  FSpriteEditorCallback := nil;
  FEventPollInterval := 10000;  // Poll every 10000 instructions by default
  // Initialize error state for EL, ER, ERR$
  FCtx.LastErrorLine := 0;
  FCtx.LastErrorCode := 0;
  FCtx.LastErrorMessage := '';
  // Initialize TRUE value (default: -1 for Commodore BASIC compatibility)
  FTrueValue := -1;
  // Initialize TRAP/RESUME state
  FCtx.TrapLine := 0;
  FCtx.TrapPC := -1;
  FCtx.ResumePC := -1;
  FCtx.InErrorHandler := False;
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
  // M5.2: join any worker still running, then free its spawn record + context.
  CleanupWorkers;
  DoneCriticalSection(FWorkerLock);
  // M5.4: free any sync primitives the program left undestroyed.
  CleanupConds;
  DoneCriticalSection(FCondTableLock);
  CleanupMutexes;
  DoneCriticalSection(FMutexTableLock);
  // M5.2c: free the shared UDT-record region.
  CleanupSharedRecords;
  DoneCriticalSection(FSharedRecLock);
  FCtx.Free;
  FDrainCtx.Free;
  FDrawQueue.Free;
  inherited Destroy;
end;

function TBytecodeVM.IsRenderOwner: Boolean;
begin
  Result := GetCurrentThreadID = FRenderOwnerThreadId;
end;

{ EnqueueDeferredOp — snapshot a graphics/sprite opcode and the resolved register banks it
  reads, for the render-owner thread to replay later. Only reached from a worker thread
  (FHasWorkers and not IsRenderOwner); dormant on the single-threaded path. }
procedure TBytecodeVM.EnqueueDeferredOp(Ctx: TExecutionContext; Kind: TDrawCommandKind; const Instr: TBytecodeInstruction);
var
  Cmd: TDrawCommand;
begin
  Cmd.Kind := Kind;
  Cmd.Instr := Instr;
  // Copy the producer's whole register banks so the owner can read any operand the handler
  // touches without per-opcode marshaling. (M5.2 may narrow this to the touched registers.)
  Cmd.IntRegs := Copy(Ctx.IntRegs, 0, Ctx.IntRegCount);
  Cmd.FloatRegs := Copy(Ctx.FloatRegs, 0, Ctx.FloatRegCount);
  Cmd.StringRegs := Copy(Ctx.StringRegs, 0, Ctx.StringRegCount);
  FDrawQueue.Enqueue(Cmd);
end;

{ DrainDrawQueue — replay every queued command on the real device. Runs only on the render-
  owner thread, at the present cadence. Each command's register snapshot is installed into a
  scratch context so the existing opcode handlers (which read Ctx.*) replay unchanged. }
procedure TBytecodeVM.DrainDrawQueue;
var
  Items: array of TDrawCommand;
  n, i: Integer;
begin
  if FDrawQueue.IsEmpty then Exit;
  SetLength(Items, 4096);
  n := FDrawQueue.DequeueAll(Items);
  for i := 0 to n - 1 do
  begin
    // Replay each command against its register snapshot via the scratch context, which is
    // passed explicitly to the opcode handlers (M5.2 parameter-threading).
    FDrainCtx.IntRegs := Items[i].IntRegs;
    FDrainCtx.FloatRegs := Items[i].FloatRegs;
    FDrainCtx.StringRegs := Items[i].StringRegs;
    FDrainCtx.IntRegCount := Length(Items[i].IntRegs);
    FDrainCtx.FloatRegCount := Length(Items[i].FloatRegs);
    FDrainCtx.StringRegCount := Length(Items[i].StringRegs);
    case Items[i].Kind of
      dckGraphics: ExecuteGraphicsOp(FDrainCtx, Items[i].Instr);
      dckSprite:   ExecuteSpriteOp(FDrainCtx, Items[i].Instr);
    end;
  end;
end;

{ PresentFrame — the once-per-frame render hook: replay any deferred worker draws, then present.
  On the single-threaded path FHasWorkers is False, so this is exactly FOutputDevice.Present. }
procedure TBytecodeVM.PresentFrame;
begin
  if FHasWorkers then DrainDrawQueue;
  if Assigned(FOutputDevice) then FOutputDevice.Present;
end;

function TBytecodeVM.GetPC: Integer;
begin
  Result := FCtx.PC;
end;

function TBytecodeVM.GetRunning: Boolean;
begin
  Result := FCtx.Running;
end;

function TBytecodeVM.GetStopped: Boolean;
begin
  Result := FCtx.Stopped;
end;

function TBytecodeVM.GetLastErrorLine: Integer;
begin
  Result := FCtx.LastErrorLine;
end;

function TBytecodeVM.GetLastErrorCode: Integer;
begin
  Result := FCtx.LastErrorCode;
end;

function TBytecodeVM.GetLastErrorMessage: string;
begin
  Result := FCtx.LastErrorMessage;
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
  FCtx.IntRegCount := MIN_REGISTER_SLOTS;
  FCtx.FloatRegCount := MIN_REGISTER_SLOTS;
  FCtx.StringRegCount := MIN_REGISTER_SLOTS;

  SetLength(FCtx.IntRegs, FCtx.IntRegCount);
  SetLength(FCtx.FloatRegs, FCtx.FloatRegCount);
  SetLength(FCtx.StringRegs, FCtx.StringRegCount);
  SetLength(FCtx.TempIntRegs, FCtx.IntRegCount);
  SetLength(FCtx.TempFloatRegs, FCtx.FloatRegCount);
  SetLength(FCtx.TempFStringRegs, FCtx.StringRegCount);

  for i := 0 to FCtx.IntRegCount - 1 do
  begin
    FCtx.IntRegs[i] := 0;
    FCtx.TempIntRegs[i] := 0;
  end;

  for i := 0 to FCtx.FloatRegCount - 1 do
  begin
    FCtx.FloatRegs[i] := 0.0;
    FCtx.TempFloatRegs[i] := 0.0;
  end;

  for i := 0 to FCtx.StringRegCount - 1 do
  begin
    FCtx.StringRegs[i] := '';
    FCtx.TempFStringRegs[i] := '';
  end;
end;

procedure TBytecodeVM.ClearAllVariables;
var i: Integer;
begin
  // Clear all registers (reset to default values)
  for i := 0 to FCtx.IntRegCount - 1 do
    FCtx.IntRegs[i] := 0;
  for i := 0 to FCtx.FloatRegCount - 1 do
    FCtx.FloatRegs[i] := 0.0;
  for i := 0 to FCtx.StringRegCount - 1 do
    FCtx.StringRegs[i] := '';
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
procedure TBytecodeVM.CooperativeSleep(Ctx: TExecutionContext; Milliseconds: Integer);
const
  SLICE_MS = 16;  // Process events every ~16ms (60 FPS)
var
  Remaining, SleepTime: Integer;
begin
  Remaining := Milliseconds;
  while Remaining > 0 do
  begin
    // Stop/quit requested (e.g. CTRL+END or window close during playback): bail out.
    if not Ctx.Running then
      Exit;

    // Determine sleep slice
    if Remaining > SLICE_MS then
      SleepTime := SLICE_MS
    else
      SleepTime := Remaining;

    // Sleep for this slice
    Sleep(SleepTime);
    Dec(Remaining, SleepTime);

    // Run the FULL event/render cycle each slice — not just raw event polling —
    // so audio playback (PLAY/SOUND, which block on note durations via this sleep)
    // stays cooperative with keyboard I/O and rendering: fullscreen toggle (CTRL+F),
    // render-target reset (ALT+TAB), sprite auto-movement and stop/quit all work
    // while a sound is playing. Falls back to bare polling if no callback is set.
    if Assigned(FEventPollCallback) then
    begin
      if FEventPollCallback() then
      begin
        Ctx.Running := False;  // stop/quit requested: abort the wait
        Exit;
      end;
    end
    else
    begin
      if Assigned(FInputDevice) then
        FInputDevice.ProcessEvents;
      if Assigned(FOutputDevice) then
        PresentFrame;
    end;
  end;
end;

procedure TBytecodeVM.ExecutePlayString(Ctx: TExecutionContext; const MusicStr: string);
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
  Voice, Octave, Envelope: Integer;
  FilterOn: Boolean;
  Duration: Integer;  // in jiffies (1/60 sec)
  NoteIndex: Integer;
  Freq: Single;
  Sharp, Flat, Dotted: Boolean;
  NextSharp, NextFlat, NextDotted: Boolean;  // C128 prefix modifiers
  Waveform: Word;
  SavedMasterVolume: Single;
  AutoVolume: Boolean;

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

  // Flush display so any preceding PRINT is visible before blocking on playback
  if Assigned(FInputDevice) then
    FInputDevice.ProcessEvents;
  if Assigned(FOutputDevice) then
    PresentFrame;

  // Auto-set master volume if zero (C128: PLAY enables audio automatically)
  AutoVolume := False;
  SavedMasterVolume := FSIDEvo.MasterVolume;
  if SavedMasterVolume = 0.0 then
  begin
    AutoVolume := True;
    FAudioBackend.Lock;
    try
      FSIDEvo.SetMasterVolume(8.0 / 15.0);  // VOL 8 equivalent
    finally
      FAudioBackend.Unlock;
    end;
  end;

  // Defaults
  Voice := 1;
  Octave := 4;
  Envelope := 0;
  FilterOn := False;
  NextSharp := False;
  NextFlat := False;
  NextDotted := False;
  Duration := 24;  // Quarter note default (C128: 24 jiffies)

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
      'U': begin  // Volume 0-15 (C128: sets master volume, not per-voice)
        FAudioBackend.Lock;
        try
          FSIDEvo.SetMasterVolume(ParseNumber / 15.0);
        finally
          FAudioBackend.Unlock;
        end;
      end;
      'X': FilterOn := (ParseNumber = 1); // Filter on/off

      // Duration prefixes (C128 jiffies: W=96, H=48, Q=24, I=12, S=6)
      'W': Duration := 96;  // Whole note
      'H': Duration := 48;  // Half note
      'Q': Duration := 24;  // Quarter note
      'I': Duration := 12;  // Eighth note
      'S': Duration := 6;   // Sixteenth note

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

        // Apply prefix modifiers (C128: #/$/. can precede the note, e.g. s#d = sixteenth D-sharp)
        Sharp := NextSharp;
        Flat := NextFlat;
        Dotted := NextDotted;
        NextSharp := False;
        NextFlat := False;
        NextDotted := False;

        // Also check for post-note modifiers (look ahead)
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

            // Set pulse width if pulse waveform
            if Waveform = SIDEVO_WAVE_PULSE then
              FSIDEvo.SetPulseWidth(Voice - 1, FAudioEnvelopes[Envelope].PulseWidth);

            // Set ADSR from envelope (SIDEvo uses 0.0-1.0 for level ratios)
            FSIDEvo.SetADSR(Voice - 1,
              FAudioEnvelopes[Envelope].Attack,
              FAudioEnvelopes[Envelope].Decay,
              FAudioEnvelopes[Envelope].Sustain,
              FAudioEnvelopes[Envelope].Release);

            // Route voice through filter if enabled
            if FilterOn then
              FSIDEvo.SetFilterVoiceRouting(Voice = 1, Voice = 2, Voice = 3, False)
            else
              FSIDEvo.SetFilterVoiceRouting(False, False, False, False);

            // Reset envelope to avoid ADSR delay bug (Sustain=15 -> $FF wrap -> HoldZero)
            FSIDEvo.ResetVoiceEnvelope(Voice - 1);

            // Trigger note (gate on)
            FSIDEvo.GateOn(Voice - 1);
          finally
            FAudioBackend.Unlock;
          end;

          // Wait for note duration (outside lock to allow callback to run)
          {$IFDEF DEBUG_AUDIO}
          WriteLn('[DEBUG_AUDIO] Sleeping for ', Duration * 1000 * 16 div (60 * FAudioTempo), ' ms (tempo=', FAudioTempo, ')');
          {$ENDIF}
          CooperativeSleep(Ctx, Duration * 1000 * 16 div (60 * FAudioTempo));

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
        CooperativeSleep(Ctx, Duration * 1000 * 16 div (60 * FAudioTempo));
      end;

      'M': ; // Wait for voices - not implemented yet

      '#': NextSharp := True;   // C128 prefix sharp (applied to next note)
      '$': NextFlat := True;    // C128 prefix flat (applied to next note)
      '.': NextDotted := True;  // C128 prefix dotted (applied to next note)
    end;
  end;

  // Restore master volume to 0 if we auto-set it
  if AutoVolume then
  begin
    FAudioBackend.Lock;
    try
      FSIDEvo.SetMasterVolume(0.0);
    finally
      FAudioBackend.Unlock;
    end;
  end;
end;
{$ENDIF}

procedure TBytecodeVM.FramePush(Ctx: TExecutionContext);
// Snapshot the whole register banks onto the flat per-bank save stacks (one frame).
// Counts are invariant during a run (banks are sized before execution), so bcReturnSub
// pops exactly the same number of slots.
var
  i: Integer;
begin
  // Grow save stacks if needed (defensive; usually sized once).
  if Ctx.FrameSaveIntTop + Ctx.IntRegCount > Length(Ctx.FrameSaveInt) then
    SetLength(Ctx.FrameSaveInt, Ctx.FrameSaveIntTop + Ctx.IntRegCount + 256);
  if Ctx.FrameSaveFloatTop + Ctx.FloatRegCount > Length(Ctx.FrameSaveFloat) then
    SetLength(Ctx.FrameSaveFloat, Ctx.FrameSaveFloatTop + Ctx.FloatRegCount + 256);
  if Ctx.FrameSaveStrTop + Ctx.StringRegCount > Length(Ctx.FrameSaveStr) then
    SetLength(Ctx.FrameSaveStr, Ctx.FrameSaveStrTop + Ctx.StringRegCount + 256);
  for i := 0 to Ctx.IntRegCount - 1 do
    Ctx.FrameSaveInt[Ctx.FrameSaveIntTop + i] := Ctx.IntRegs[i];
  Inc(Ctx.FrameSaveIntTop, Ctx.IntRegCount);
  for i := 0 to Ctx.FloatRegCount - 1 do
    Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop + i] := Ctx.FloatRegs[i];
  Inc(Ctx.FrameSaveFloatTop, Ctx.FloatRegCount);
  for i := 0 to Ctx.StringRegCount - 1 do
    Ctx.FrameSaveStr[Ctx.FrameSaveStrTop + i] := Ctx.StringRegs[i];
  Inc(Ctx.FrameSaveStrTop, Ctx.StringRegCount);
  // RAII (V2): remember where this frame's record allocations begin.
  if Ctx.FrameRecBaseTop >= Length(Ctx.FrameRecBase) then
    SetLength(Ctx.FrameRecBase, Ctx.FrameRecBaseTop + 256);
  if Ctx.FrameRecBaseTop >= Length(Ctx.FrameBlockMarkTop) then
    SetLength(Ctx.FrameBlockMarkTop, Ctx.FrameRecBaseTop + 256);
  Ctx.FrameRecBase[Ctx.FrameRecBaseTop] := Ctx.RecordCount;
  Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop] := Ctx.BlockRecMarkTop;  // M8: remember the block-mark depth
  Inc(Ctx.FrameRecBaseTop);
end;

procedure TBytecodeVM.FramePop(Ctx: TExecutionContext);
// Restore the whole register banks from the top frame on the save stacks.
var
  i: Integer;
begin
  Dec(Ctx.FrameSaveIntTop, Ctx.IntRegCount);
  for i := 0 to Ctx.IntRegCount - 1 do
    Ctx.IntRegs[i] := Ctx.FrameSaveInt[Ctx.FrameSaveIntTop + i];
  Dec(Ctx.FrameSaveFloatTop, Ctx.FloatRegCount);
  for i := 0 to Ctx.FloatRegCount - 1 do
    Ctx.FloatRegs[i] := Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop + i];
  Dec(Ctx.FrameSaveStrTop, Ctx.StringRegCount);
  for i := 0 to Ctx.StringRegCount - 1 do
    Ctx.StringRegs[i] := Ctx.FrameSaveStr[Ctx.FrameSaveStrTop + i];
  // RAII (V2): release the records this frame allocated (locals/temporaries) by rolling the
  // high-water mark back. Slots become reusable by the next AllocRecord. A UDT result has already
  // been copied into the caller-allocated instance (which lives below this frame's mark).
  if Ctx.FrameRecBaseTop > 0 then
  begin
    Dec(Ctx.FrameRecBaseTop);
    if Ctx.FrameRecBase[Ctx.FrameRecBaseTop] < Ctx.RecordCount then
      Ctx.RecordCount := Ctx.FrameRecBase[Ctx.FrameRecBaseTop];
    // M8: discard any block marks this frame left dangling (e.g. EXIT SUB from inside a loop).
    Ctx.BlockRecMarkTop := Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop];
  end;
end;

function TBytecodeVM.AllocRecord(Ctx: TExecutionContext; IntC, FloatC, StrC, TypeId: Integer): Integer;
// Allocate a record instance (heap block of typed slot arrays) in Ctx's per-thread heap and
// return its handle (an index into Ctx.Records).
begin
  if Ctx.RecordCount >= Length(Ctx.Records) then
    SetLength(Ctx.Records, (Ctx.RecordCount + 1) * 2);
  Ctx.Records[Ctx.RecordCount].TypeId := TypeId;
  SetLength(Ctx.Records[Ctx.RecordCount].IntData, IntC);
  SetLength(Ctx.Records[Ctx.RecordCount].FloatData, FloatC);
  SetLength(Ctx.Records[Ctx.RecordCount].StringData, StrC);
  Result := Ctx.RecordCount;
  Inc(Ctx.RecordCount);
end;

function TBytecodeVM.AllocSharedRecord(IntC, FloatC, StrC, TypeId: Integer): Int64;
// M5.2c: allocate a record in the cross-thread shared region and return a SHARED_REC_FLAG-tagged
// handle. Each record is its own heap block (stable pointer), so a handle survives the outer array
// growing. Used for arrays of UDT, whose handles live in the global FArrays and are read by any thread.
var
  R: PRecordStorage;
  Idx: Integer;
begin
  New(R);
  R^.TypeId := TypeId;
  SetLength(R^.IntData, IntC);
  SetLength(R^.FloatData, FloatC);
  SetLength(R^.StringData, StrC);
  EnterCriticalSection(FSharedRecLock);
  try
    Idx := FSharedRecordCount;
    if Idx >= Length(FSharedRecords) then
      SetLength(FSharedRecords, (Idx + 1) * 2);
    FSharedRecords[Idx] := R;
    Inc(FSharedRecordCount);
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
  Result := SHARED_REC_FLAG or Int64(Idx);
end;

function TBytecodeVM.ResolveRec(Ctx: TExecutionContext; Handle: Int64): PRecordStorage;
// M5.2c: route a record handle to its storage. A SHARED_REC_FLAG-tagged handle indexes the shared
// region (looked up under FSharedRecLock; the returned pointer is stable, so field access is then
// lock-free — concurrent writes to the SAME shared record are the programmer's job, via a mutex).
// A plain handle indexes the active context's per-thread heap (only that thread touches it).
begin
  if (Handle and SHARED_REC_FLAG) <> 0 then
  begin
    EnterCriticalSection(FSharedRecLock);
    Result := FSharedRecords[Handle and SHARED_REC_MASK];
    LeaveCriticalSection(FSharedRecLock);
  end
  else
    Result := @Ctx.Records[Handle];
end;

procedure TBytecodeVM.CleanupSharedRecords;
// Destructor helper: free every record in the shared region.
var
  i: Integer;
begin
  for i := 0 to FSharedRecordCount - 1 do
    if FSharedRecords[i] <> nil then Dispose(FSharedRecords[i]);
  SetLength(FSharedRecords, 0);
  FSharedRecordCount := 0;
end;

procedure TBytecodeVM.RecordNewArrayInit(Ctx: TExecutionContext; ArrayId: Integer; PackedCounts: Int64);
// Eager-allocate one record instance per element of the (int handle) array and store the handles.
// PackedCounts = intCount | floatCount<<16 | strCount<<32 | typeId<<48. M5.2c: array-of-UDT records go
// in the shared region (the handle array FArrays[ArrayId] is global, so any thread can reach them).
var
  k, IntC, FloatC, StrC, TypeId: Integer;
begin
  IntC := PackedCounts and $FFFF;
  FloatC := (PackedCounts shr 16) and $FFFF;
  StrC := (PackedCounts shr 32) and $FFFF;
  TypeId := (PackedCounts shr 48) and $FFFF;
  for k := 0 to FArrays[ArrayId].TotalSize - 1 do
    FArrays[ArrayId].IntData[k] := AllocSharedRecord(IntC, FloatC, StrC, TypeId);
end;

procedure TBytecodeVM.SetupWorkerContext(WCtx: TExecutionContext);
// Size a fresh worker context's banks/stacks to the program's needs and zero them — the same
// initial state InitializeRegisters + the constructor give the main context. Register banks are
// sized to the main context's current counts (already grown to the program's full register usage),
// so the worker SUB never indexes past its banks. FB locals/registers start at 0 / 0.0 / ''.
var i: Integer;
begin
  WCtx.IntRegCount := FCtx.IntRegCount;
  WCtx.FloatRegCount := FCtx.FloatRegCount;
  WCtx.StringRegCount := FCtx.StringRegCount;
  SetLength(WCtx.IntRegs, WCtx.IntRegCount);
  SetLength(WCtx.FloatRegs, WCtx.FloatRegCount);
  SetLength(WCtx.StringRegs, WCtx.StringRegCount);
  SetLength(WCtx.TempIntRegs, WCtx.IntRegCount);
  SetLength(WCtx.TempFloatRegs, WCtx.FloatRegCount);
  SetLength(WCtx.TempFStringRegs, WCtx.StringRegCount);
  for i := 0 to WCtx.IntRegCount - 1 do begin WCtx.IntRegs[i] := 0; WCtx.TempIntRegs[i] := 0; end;
  for i := 0 to WCtx.FloatRegCount - 1 do begin WCtx.FloatRegs[i] := 0.0; WCtx.TempFloatRegs[i] := 0.0; end;
  for i := 0 to WCtx.StringRegCount - 1 do begin WCtx.StringRegs[i] := ''; WCtx.TempFStringRegs[i] := ''; end;
  SetLength(WCtx.CallStack, 256);
  // NB: the transfer slots (XferInt/Float/Str) are sized and filled by SpawnWorker with the worker's
  // argument snapshot — do NOT re-init them here, or the arguments would be lost.
  WCtx.CallStackPtr := 0;
  WCtx.FrameSaveIntTop := 0;
  WCtx.FrameSaveFloatTop := 0;
  WCtx.FrameSaveStrTop := 0;
  WCtx.FrameRecBaseTop := 0;
  WCtx.BlockRecMarkTop := 0;
  SetLength(WCtx.Records, 0);
  WCtx.RecordCount := 0;
  WCtx.CursorCol := 0;
  WCtx.TrapLine := 0;
  WCtx.TrapPC := -1;
  WCtx.ResumePC := -1;
  WCtx.InErrorHandler := False;
  WCtx.PC := 0;
end;

function TBytecodeVM.SpawnWorker(EntryPC: Int64; SpawnerCtx: TExecutionContext): Int64;
// bcThreadCreate: register a worker (handle = index+1) and BeginThread it. The worker runs the SUB at
// EntryPC on its own context; the SUB's arguments are snapshotted from SpawnerCtx's transfer slots (the
// caller staged them there). From the first spawn FHasWorkers is True, wiring the M5.3 draw queue.
var
  Spawn: TWorkerSpawn;
  Idx: Integer;
begin
  Spawn := TWorkerSpawn.Create;
  Spawn.VM := Self;
  Spawn.Ctx := TExecutionContext.Create;
  Spawn.EntryPC := EntryPC;
  Spawn.Joined := False;
  Spawn.Detached := False;
  // Snapshot the spawning context's transfer slots into the worker's context: the arguments were just
  // staged there (StageCallArgs, like a normal call), and the worker's SUB prologue loads its parameters
  // from these same slots. Done here on the spawner thread (the worker hasn't started), so it is safe
  // and the args are captured before any later spawn overwrites the spawner's transfer slots.
  SetLength(Spawn.Ctx.XferInt, Length(SpawnerCtx.XferInt));
  SetLength(Spawn.Ctx.XferFloat, Length(SpawnerCtx.XferFloat));
  SetLength(Spawn.Ctx.XferStr, Length(SpawnerCtx.XferStr));
  for Idx := 0 to High(SpawnerCtx.XferInt) do Spawn.Ctx.XferInt[Idx] := SpawnerCtx.XferInt[Idx];
  for Idx := 0 to High(SpawnerCtx.XferFloat) do Spawn.Ctx.XferFloat[Idx] := SpawnerCtx.XferFloat[Idx];
  for Idx := 0 to High(SpawnerCtx.XferStr) do Spawn.Ctx.XferStr[Idx] := SpawnerCtx.XferStr[Idx];
  EnterCriticalSection(FWorkerLock);
  try
    Idx := Length(FWorkerThreads);
    SetLength(FWorkerThreads, Idx + 1);
    FWorkerThreads[Idx] := Spawn;
    FHasWorkers := True;
    Result := Idx + 1;   // handle (0 = invalid)
  finally
    LeaveCriticalSection(FWorkerLock);
  end;
  Spawn.Handle := Result;   // M5.5: so the worker can report its own handle via THREADSELF
  Spawn.ThreadId := BeginThread(@WorkerThreadEntry, Pointer(Spawn));
end;

procedure TBytecodeVM.RunWorker(Spawn: TObject);
// Worker-thread body (called from WorkerThreadEntry, with GActiveCtx already = Sp.Ctx). Initialise the
// context, prime a synthetic call frame identical to bcCallSub (so the SUB's bcReturnSub exits the
// loop) and enter the run loop at the SUB's entry PC.
var
  Sp: TWorkerSpawn;
  WCtx: TExecutionContext;
begin
  Sp := TWorkerSpawn(Spawn);
  WCtx := Sp.Ctx;
  SetupWorkerContext(WCtx);                          // sizes register/stack banks; leaves the xfer snapshot intact
  WCtx.StartPC := Sp.EntryPC;
  FramePush(WCtx);                                  // snapshot the (zeroed) banks for the SUB frame
  WCtx.CallStack[0] := FProgram.GetInstructionCount;  // return-to-stop sentinel (CurPC >= InstrCount → exit)
  WCtx.CallStackPtr := 1;
  // The SUB's arguments are already in WCtx's transfer slots (snapshotted at spawn from the caller);
  // the prologue loads them into the parameter registers — so workers take typed, multi-arg parameters.
  RunFast;                                          // binds Ctx := GActiveCtx, starts at WCtx.StartPC
end;

procedure TBytecodeVM.JoinWorker(Handle: Int64);
// bcThreadWait: wait for the worker named by Handle to terminate (once). Invalid/stale handles and
// already-joined workers are no-ops, matching FB's tolerant Threadwait.
var
  Spawn: TWorkerSpawn;
begin
  Spawn := nil;
  EnterCriticalSection(FWorkerLock);
  try
    if (Handle >= 1) and (Handle <= Length(FWorkerThreads)) then
    begin
      Spawn := TWorkerSpawn(FWorkerThreads[Handle - 1]);
      if (Spawn = nil) or Spawn.Joined or Spawn.Detached then
        Spawn := nil           // nothing to wait on (already joined, or detached)
      else
        Spawn.Joined := True;  // claim the join under the lock so only one thread waits
    end;
  finally
    LeaveCriticalSection(FWorkerLock);
  end;
  if Spawn <> nil then
    WaitForThreadTerminate(Spawn.ThreadId, 0);  // 0 = wait indefinitely
end;

procedure TBytecodeVM.DetachWorker(Handle: Int64);
// bcThreadDetach: mark a worker as not-to-be-explicitly-joined. It runs to completion independently;
// the VM still waits for it at program end (CleanupWorkers) so its context is never freed under it.
var
  Spawn: TWorkerSpawn;
begin
  EnterCriticalSection(FWorkerLock);
  try
    if (Handle >= 1) and (Handle <= Length(FWorkerThreads)) then
    begin
      Spawn := TWorkerSpawn(FWorkerThreads[Handle - 1]);
      if Spawn <> nil then Spawn.Detached := True;
    end;
  finally
    LeaveCriticalSection(FWorkerLock);
  end;
end;

procedure TBytecodeVM.CleanupWorkers;
// Destructor helper: join any worker still running, then free its context and spawn record.
var
  i: Integer;
  Spawn: TWorkerSpawn;
begin
  for i := 0 to Length(FWorkerThreads) - 1 do
  begin
    Spawn := TWorkerSpawn(FWorkerThreads[i]);
    if Spawn = nil then Continue;
    if not Spawn.Joined then
    begin
      WaitForThreadTerminate(Spawn.ThreadId, 0);
      Spawn.Joined := True;
    end;
    Spawn.Ctx.Free;
    Spawn.Free;
  end;
  SetLength(FWorkerThreads, 0);
end;

function TBytecodeVM.CreateMutex: Int64;
// bcMutexCreate: allocate a fresh critical section and register it; return its handle (index + 1).
var
  M: PMutex;
  Idx: Integer;
begin
  New(M);
  InitCriticalSection(M^);
  EnterCriticalSection(FMutexTableLock);
  try
    Idx := Length(FMutexes);
    SetLength(FMutexes, Idx + 1);
    FMutexes[Idx] := M;
    Result := Idx + 1;
  finally
    LeaveCriticalSection(FMutexTableLock);
  end;
end;

procedure TBytecodeVM.LockMutex(Handle: Int64);
// bcMutexLock: look the mutex up under the table lock, then block on the mutex itself (outside the
// table lock, so locking one mutex never serialises others).
var
  M: PMutex;
begin
  M := nil;
  EnterCriticalSection(FMutexTableLock);
  try
    if (Handle >= 1) and (Handle <= Length(FMutexes)) then M := PMutex(FMutexes[Handle - 1]);
  finally
    LeaveCriticalSection(FMutexTableLock);
  end;
  if M <> nil then EnterCriticalSection(M^);
end;

procedure TBytecodeVM.UnlockMutex(Handle: Int64);
// bcMutexUnlock: release the mutex (invalid/destroyed handles are no-ops).
var
  M: PMutex;
begin
  M := nil;
  EnterCriticalSection(FMutexTableLock);
  try
    if (Handle >= 1) and (Handle <= Length(FMutexes)) then M := PMutex(FMutexes[Handle - 1]);
  finally
    LeaveCriticalSection(FMutexTableLock);
  end;
  if M <> nil then LeaveCriticalSection(M^);
end;

procedure TBytecodeVM.DestroyMutex(Handle: Int64);
// bcMutexDestroy: detach the mutex from the table (handle stays invalid) and free it. The caller
// must not hold or use it afterward (FB contract).
var
  M: PMutex;
begin
  M := nil;
  EnterCriticalSection(FMutexTableLock);
  try
    if (Handle >= 1) and (Handle <= Length(FMutexes)) then
    begin
      M := PMutex(FMutexes[Handle - 1]);
      FMutexes[Handle - 1] := nil;
    end;
  finally
    LeaveCriticalSection(FMutexTableLock);
  end;
  if M <> nil then begin DoneCriticalSection(M^); Dispose(M); end;
end;

procedure TBytecodeVM.CleanupMutexes;
// Destructor helper: free any mutex the program left undestroyed.
var
  i: Integer;
  M: PMutex;
begin
  for i := 0 to Length(FMutexes) - 1 do
  begin
    M := PMutex(FMutexes[i]);
    if M <> nil then begin DoneCriticalSection(M^); Dispose(M); end;
  end;
  SetLength(FMutexes, 0);
end;

function TBytecodeVM.CreateCond: Int64;
// bcCondCreate: allocate a condition variable and return its handle (index + 1).
var
  CV: TCondVar;
  Idx: Integer;
begin
  CV := TCondVar.Create;
  InitCriticalSection(CV.ILock);
  SetLength(CV.Waiters, 0);
  EnterCriticalSection(FCondTableLock);
  try
    Idx := Length(FCondVars);
    SetLength(FCondVars, Idx + 1);
    FCondVars[Idx] := CV;
    Result := Idx + 1;
  finally
    LeaveCriticalSection(FCondTableLock);
  end;
end;

procedure TBytecodeVM.CondWaitOp(CondHandle, MutexHandle: Int64);
// bcCondWait: register this thread's event on the cond var, release the user mutex, block until the
// event is set (sticky → a signal that races the wait still wakes us), then reacquire the mutex.
var
  CV: TCondVar;
  Ev: PRTLEvent;
  N: Integer;
begin
  CV := nil;
  EnterCriticalSection(FCondTableLock);
  try
    if (CondHandle >= 1) and (CondHandle <= Length(FCondVars)) then CV := TCondVar(FCondVars[CondHandle - 1]);
  finally
    LeaveCriticalSection(FCondTableLock);
  end;
  if CV = nil then Exit;
  Ev := RTLEventCreate;
  EnterCriticalSection(CV.ILock);
  try
    N := Length(CV.Waiters);
    SetLength(CV.Waiters, N + 1);
    CV.Waiters[N] := Ev;
  finally
    LeaveCriticalSection(CV.ILock);
  end;
  UnlockMutex(MutexHandle);     // release the associated mutex while we wait
  RTLEventWaitFor(Ev);          // block; the signaler removed Ev from the list before setting it
  LockMutex(MutexHandle);       // reacquire before returning (FB/POSIX contract)
  RTLEventDestroy(Ev);          // we own Ev now (the signaler only set it)
end;

procedure TBytecodeVM.CondSignalOp(CondHandle: Int64);
// bcCondSignal: wake the longest-waiting thread (FIFO front), if any.
var
  CV: TCondVar;
  Ev: PRTLEvent;
  i: Integer;
begin
  CV := nil;
  EnterCriticalSection(FCondTableLock);
  try
    if (CondHandle >= 1) and (CondHandle <= Length(FCondVars)) then CV := TCondVar(FCondVars[CondHandle - 1]);
  finally
    LeaveCriticalSection(FCondTableLock);
  end;
  if CV = nil then Exit;
  Ev := nil;
  EnterCriticalSection(CV.ILock);
  try
    if Length(CV.Waiters) > 0 then
    begin
      Ev := CV.Waiters[0];
      for i := 1 to High(CV.Waiters) do CV.Waiters[i - 1] := CV.Waiters[i];
      SetLength(CV.Waiters, Length(CV.Waiters) - 1);
    end;
  finally
    LeaveCriticalSection(CV.ILock);
  end;
  if Ev <> nil then RTLEventSetEvent(Ev);   // set outside ILock; the waiter destroys Ev after waking
end;

procedure TBytecodeVM.CondBroadcastOp(CondHandle: Int64);
// bcCondBroadcast: wake every waiter.
var
  CV: TCondVar;
  Evs: array of PRTLEvent;
  i: Integer;
begin
  CV := nil;
  EnterCriticalSection(FCondTableLock);
  try
    if (CondHandle >= 1) and (CondHandle <= Length(FCondVars)) then CV := TCondVar(FCondVars[CondHandle - 1]);
  finally
    LeaveCriticalSection(FCondTableLock);
  end;
  if CV = nil then Exit;
  Evs := nil;
  EnterCriticalSection(CV.ILock);
  try
    SetLength(Evs, Length(CV.Waiters));
    for i := 0 to High(CV.Waiters) do Evs[i] := CV.Waiters[i];
    SetLength(CV.Waiters, 0);
  finally
    LeaveCriticalSection(CV.ILock);
  end;
  for i := 0 to High(Evs) do RTLEventSetEvent(Evs[i]);
end;

procedure TBytecodeVM.DestroyCond(CondHandle: Int64);
// bcCondDestroy: detach and free the condition variable (FB contract: no waiters remain).
var
  CV: TCondVar;
begin
  CV := nil;
  EnterCriticalSection(FCondTableLock);
  try
    if (CondHandle >= 1) and (CondHandle <= Length(FCondVars)) then
    begin
      CV := TCondVar(FCondVars[CondHandle - 1]);
      FCondVars[CondHandle - 1] := nil;
    end;
  finally
    LeaveCriticalSection(FCondTableLock);
  end;
  if CV <> nil then begin DoneCriticalSection(CV.ILock); CV.Free; end;
end;

procedure TBytecodeVM.CleanupConds;
// Destructor helper: free any condition variable the program left undestroyed.
var
  i: Integer;
  CV: TCondVar;
begin
  for i := 0 to Length(FCondVars) - 1 do
  begin
    CV := TCondVar(FCondVars[i]);
    if CV <> nil then begin DoneCriticalSection(CV.ILock); CV.Free; end;
  end;
  SetLength(FCondVars, 0);
end;

procedure TBytecodeVM.EnsureRegisterCapacity(Ctx: TExecutionContext; RegType: TSSARegisterType; MinIndex: Integer);
var
  OldSize, NewSize, i: Integer;
begin
  case RegType of
    srtInt:
    begin
      if MinIndex >= Ctx.IntRegCount then
      begin
        OldSize := Ctx.IntRegCount;
        // Double the size or use MinIndex + 1, whichever is larger (but cap at MAX)
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for integer registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        // Grow both working and temp register arrays
        SetLength(Ctx.IntRegs, NewSize);
        SetLength(Ctx.TempIntRegs, NewSize);

        // Initialize new slots to zero
        for i := OldSize to NewSize - 1 do
        begin
          Ctx.IntRegs[i] := 0;
          Ctx.TempIntRegs[i] := 0;
        end;

        Ctx.IntRegCount := NewSize;
      end;
    end;

    srtFloat:
    begin
      if MinIndex >= Ctx.FloatRegCount then
      begin
        OldSize := Ctx.FloatRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for float registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(Ctx.FloatRegs, NewSize);
        SetLength(Ctx.TempFloatRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          Ctx.FloatRegs[i] := 0.0;
          Ctx.TempFloatRegs[i] := 0.0;
        end;

        Ctx.FloatRegCount := NewSize;
      end;
    end;

    srtString:
    begin
      if MinIndex >= Ctx.StringRegCount then
      begin
        OldSize := Ctx.StringRegCount;
        NewSize := Max(OldSize * 2, MinIndex + 1);
        if NewSize > MAX_REGISTER_SLOTS then
          NewSize := MAX_REGISTER_SLOTS;

        if MinIndex >= NewSize then
          raise Exception.CreateFmt('Register index %d exceeds maximum %d for string registers',
                                    [MinIndex, MAX_REGISTER_SLOTS - 1]);

        SetLength(Ctx.StringRegs, NewSize);
        SetLength(Ctx.TempFStringRegs, NewSize);

        for i := OldSize to NewSize - 1 do
        begin
          Ctx.StringRegs[i] := '';
          Ctx.TempFStringRegs[i] := '';
        end;

        Ctx.StringRegCount := NewSize;
      end;
    end;
  end;
end;

procedure TBytecodeVM.CheckFloatValid(Ctx: TExecutionContext; RegIndex: Integer; const OpName: string);
begin
  if IsNan(Ctx.FloatRegs[RegIndex]) then
    raise Exception.CreateFmt('NaN detected in R%d after %s', [RegIndex, OpName]);
  if IsInfinite(Ctx.FloatRegs[RegIndex]) then
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
        bcFloatToInt, bcFloatRound:
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

        // LBOUND/UBOUND: Dest = int bound, Src2 = int dim index (Src1 = array id, not a register)
        bcArrayLBound, bcArrayUBound:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // REDIM: Src2 = int new upper bound (Src1 = array id, not a register; no Dest)
        bcArrayRedim:
        begin
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // Int source (Src1) for branch
        bcJumpIfZero, bcJumpIfNotZero:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // Float dest, float sources
        bcLoadConstFloat, bcCopyFloat, bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat,
        bcModFloat, bcPowFloat, bcNegFloat,
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
        bcStrLTrim, bcStrRTrim, bcStrTrim, bcStrUCase, bcStrLCase,  // B1.2
        bcInputString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // PLAY: string Src1 (music string)
        bcSoundPlay:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
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
        bcStrLen, bcStrAsc, bcStrDec, bcStrValInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Int Src1 -> String Dest (CHR$, HEX$, ERR$, SPACE, OCT, BIN)
        bcStrChr, bcStrHex, bcStrErr, bcStrSpace, bcStrOct, bcStrBin:
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

        // INSTR/INSTRREV(haystack$, needle$[, start]) -> int Dest
        bcStrInstr, bcStrInstrRev:
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

        // bcSprSaveFile: Src1=filename string reg
        bcSprSaveFile:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;   // filename
        end;

        // bcSprLoadFile: Src1=filename string reg, Src2=usefilecolors int reg
        bcSprLoadFile:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;   // filename
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;         // flag
        end;

        // bcSprSize: Src1=n, Src2=w, Dest=h (all float regs)
        bcSprSize:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;     // sprite number
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;     // width
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;     // height
        end;

        // bcSprForm: Src1=n, Src2=format (float regs)
        bcSprForm:
        begin
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;     // sprite number
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;     // format
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
    EnsureRegisterCapacity(FCtx, srtInt, MaxIntReg);
  if MaxFloatReg >= 0 then
    EnsureRegisterCapacity(FCtx, srtFloat, MaxFloatReg);
  if MaxStringReg >= 0 then
    EnsureRegisterCapacity(FCtx, srtString, MaxStringReg);

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

procedure TBytecodeVM.SetSpriteManager(Manager: ISpriteManager);
begin
  FSpriteManager := Manager;
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
var
  i: Integer;
begin
  FCtx.PC := 0;
  FCtx.Running := False;
  FCtx.CallStackPtr := 0;
  FCtx.FrameSaveIntTop := 0;
  FCtx.FrameSaveFloatTop := 0;
  FCtx.FrameSaveStrTop := 0;
  FCtx.FrameRecBaseTop := 0;
  FCtx.BlockRecMarkTop := 0;
  SetLength(FCtx.Records, 0);
  FCtx.RecordCount := 0;
  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  FInstructionsExecuted := 0;
  {$ENDIF}
  InitializeRegisters;
  FVarMap.Clear;
  // Reset DATA pool
  SetLength(FDataPool, 0);
  FCtx.DataIndex := 0;
  // Reset PUDEF to defaults
  FPudefFiller := ' ';
  FPudefComma := ',';
  FPudefDecimal := '.';
  FPudefDollar := '$';
  // Reset TRAP/RESUME error handling state
  FCtx.TrapLine := 0;
  FCtx.TrapPC := -1;
  FCtx.ResumePC := -1;
  FCtx.InErrorHandler := False;
  // Reset error state for EL, ER, ERR$
  FCtx.LastErrorLine := 0;
  FCtx.LastErrorCode := 0;
  FCtx.LastErrorMessage := '';
  // Reset sprite state
  if Assigned(FSpriteManager) then
    FSpriteManager.ResetAllSprites;
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
  FCtx.LastErrorLine := ALine;
  FCtx.LastErrorCode := ACode;
  FCtx.LastErrorMessage := AMessage;
end;

procedure TBytecodeVM.ClearErrorState;
begin
  FCtx.LastErrorLine := 0;
  FCtx.LastErrorCode := 0;
  FCtx.LastErrorMessage := '';
end;

procedure TBytecodeVM.SetTrueValue(AValue: Int64);
begin
  FTrueValue := AValue;
end;

procedure TBytecodeVM.ExecuteInstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  Group: Word;
  SleepMs: Integer;
  FrameFPS, FrameTimeMs, WaitMs, ChunkMs: Integer;
  NowTick, TargetTick: QWord;
  KeyNum, KeyIdx, CharIdx: Integer;
  KeyText: string;
  Ch: Char;
  InQuotes: Boolean;
begin
  // Two-level dispatch: extract group from high byte
  Group := Instr.OpCode shr 8;

  case Group of
    0: ; // Core VM - fall through to inline dispatch below for performance
    1: begin ExecuteStringOp(Ctx, Instr); Exit; end;
    2: begin ExecuteMathOp(Ctx, Instr); Exit; end;
    3: begin ExecuteArrayOp(Ctx, Instr); Exit; end;
    4: begin ExecuteIOOp(Ctx, Instr); Exit; end;
    5: begin ExecuteSpecialVarOp(Ctx, Instr); Exit; end;
    6: begin ExecuteFileIOOp(Ctx, Instr); Exit; end;
    7: begin ExecuteSpriteOp(Ctx, Instr); Exit; end;
    {$IFDEF WEB_MODE}
    8: begin ExecuteWebOp(Ctx, Instr); Exit; end;
    {$ENDIF}
    10: begin ExecuteGraphicsOp(Ctx, Instr); Exit; end;
    11: begin ExecuteSoundOp(Ctx, Instr); Exit; end;
    200..255: begin ExecuteSuperinstruction(Ctx, Instr); Exit; end;
  else
    raise Exception.CreateFmt('Unknown opcode group %d at PC=%d', [Group, Ctx.PC]);
  end;

  // Group 0: Core VM operations - inline for performance
  case Instr.OpCode of
    bcLoadConstInt: Ctx.IntRegs[Instr.Dest] := Instr.Immediate;
    bcLoadConstFloat: Ctx.FloatRegs[Instr.Dest] := Double(Pointer(@Instr.Immediate)^);
    bcLoadConstString:
      if (Instr.Immediate >= 0) and (Instr.Immediate < FProgram.StringConstants.Count) then
        Ctx.StringRegs[Instr.Dest] := FProgram.StringConstants[Instr.Immediate];
    bcCopyInt: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
    bcCopyFloat:
      begin
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1];
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace copies to R38 specifically (the problematic register in n-body)
        if Instr.Dest = 38 then
        begin
          WriteLn(StdErr, 'CopyFloat at PC=', Ctx.PC, ': R[', Instr.Dest, '] ← R[', Instr.Src1, ']');
          WriteLn(StdErr, '  Source R[', Instr.Src1, '] = ', Ctx.FloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  Dest   R[', Instr.Dest, '] = ', Ctx.FloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcCopyString: Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1];
    bcAddInt: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] + Ctx.IntRegs[Instr.Src2];
    bcSubInt: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] - Ctx.IntRegs[Instr.Src2];
    bcMulInt: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] * Ctx.IntRegs[Instr.Src2];
    bcDivInt:
      if Ctx.IntRegs[Instr.Src2] <> 0 then
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] div Ctx.IntRegs[Instr.Src2]
      else raise Exception.Create('Division by zero');
    bcModInt:
      if Ctx.IntRegs[Instr.Src2] <> 0 then
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] mod Ctx.IntRegs[Instr.Src2]
      else raise Exception.Create('Modulo by zero');
    bcModFloat:
      if Ctx.FloatRegs[Instr.Src2] <> 0.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] - Floor(Ctx.FloatRegs[Instr.Src1] / Ctx.FloatRegs[Instr.Src2]) * Ctx.FloatRegs[Instr.Src2]
      else raise Exception.Create('Float modulo by zero');
    bcNegInt: Ctx.IntRegs[Instr.Dest] := -Ctx.IntRegs[Instr.Src1];
    bcAddFloat:
      begin
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Ctx, Instr.Dest, 'AddFloat');
        {$ENDIF}
        {$IFDEF DEBUG_REGISTER_DUMP}
        // Trace additions to R41 and R43 (sum of squares in n-body)
        if (Instr.Dest = 41) or (Instr.Dest = 43) then
        begin
          WriteLn(StdErr, 'AddFloat at PC=', Ctx.PC, ': R[', Instr.Dest, '] = R[', Instr.Src1, '] + R[', Instr.Src2, ']');
          WriteLn(StdErr, '  R[', Instr.Src1, '] = ', Ctx.FloatRegs[Instr.Src1]:0:17);
          WriteLn(StdErr, '  R[', Instr.Src2, '] = ', Ctx.FloatRegs[Instr.Src2]:0:17);
          WriteLn(StdErr, '  R[', Instr.Dest, '] = ', Ctx.FloatRegs[Instr.Dest]:0:17);
        end;
        {$ENDIF}
      end;
    bcSubFloat:
      begin
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] - Ctx.FloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Ctx, Instr.Dest, 'SubFloat');
        {$ENDIF}
      end;
    bcMulFloat:
      begin
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
        {$IFDEF DEBUG_FLOAT_CHECKS}
        CheckFloatValid(Ctx, Instr.Dest, 'MulFloat');
        {$ENDIF}
      end;
    bcDivFloat:
      begin
        if Abs(Ctx.FloatRegs[Instr.Src2]) >= 1e-300 then
        begin
          Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] / Ctx.FloatRegs[Instr.Src2];
          {$IFDEF DEBUG_FLOAT_CHECKS}
          CheckFloatValid(Ctx, Instr.Dest, 'DivFloat');
          {$ENDIF}
        end
        else
          raise EZeroDivide.Create('Division by zero');
      end;
    bcPowFloat: Ctx.FloatRegs[Instr.Dest] := Power(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]);
    bcNegFloat: Ctx.FloatRegs[Instr.Dest] := -Ctx.FloatRegs[Instr.Src1];
    bcIntToFloat: Ctx.FloatRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
    bcFloatToInt: Ctx.IntRegs[Instr.Dest] := Trunc(Ctx.FloatRegs[Instr.Src1]);
    bcFloatRound: Ctx.IntRegs[Instr.Dest] := Round(Ctx.FloatRegs[Instr.Src1]);  // CINT (round-to-even)
    // Comparison operators - Int (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqInt: if Ctx.IntRegs[Instr.Src1] = Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpNeInt: if Ctx.IntRegs[Instr.Src1] <> Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLtInt: if Ctx.IntRegs[Instr.Src1] < Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGtInt: if Ctx.IntRegs[Instr.Src1] > Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLeInt: if Ctx.IntRegs[Instr.Src1] <= Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGeInt: if Ctx.IntRegs[Instr.Src1] >= Ctx.IntRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    // Comparison operators - Float (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqFloat: if Ctx.FloatRegs[Instr.Src1] = Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpNeFloat: if Ctx.FloatRegs[Instr.Src1] <> Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLtFloat: if Ctx.FloatRegs[Instr.Src1] < Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGtFloat: if Ctx.FloatRegs[Instr.Src1] > Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLeFloat: if Ctx.FloatRegs[Instr.Src1] <= Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGeFloat: if Ctx.FloatRegs[Instr.Src1] >= Ctx.FloatRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    // Comparison operators - String (use FTrueValue for TRUE, 0 for FALSE)
    bcCmpEqString: if Ctx.StringRegs[Instr.Src1] = Ctx.StringRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpNeString: if Ctx.StringRegs[Instr.Src1] <> Ctx.StringRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLtString: if Ctx.StringRegs[Instr.Src1] < Ctx.StringRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGtString: if Ctx.StringRegs[Instr.Src1] > Ctx.StringRegs[Instr.Src2] then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    // Bitwise operators
    bcBitwiseAnd: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] and Ctx.IntRegs[Instr.Src2];
    bcBitwiseOr: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] or Ctx.IntRegs[Instr.Src2];
    bcBitwiseXor: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] xor Ctx.IntRegs[Instr.Src2];
    bcBitwiseNot: Ctx.IntRegs[Instr.Dest] := not Ctx.IntRegs[Instr.Src1];
    // Control flow
    bcJump: Ctx.PC := Instr.Immediate - 1;
    bcJumpIfZero:
      if Ctx.IntRegs[Instr.Src1] = 0 then Ctx.PC := Instr.Immediate - 1;
    bcJumpIfNotZero:
      if Ctx.IntRegs[Instr.Src1] <> 0 then Ctx.PC := Instr.Immediate - 1;
    bcCall:
      begin
        Ctx.CallStack[Ctx.CallStackPtr] := Ctx.PC;
        Inc(Ctx.CallStackPtr);
        Ctx.PC := Instr.Immediate - 1;
      end;
    bcReturn:
      if Ctx.CallStackPtr > 0 then
      begin
        Dec(Ctx.CallStackPtr);
        Ctx.PC := Ctx.CallStack[Ctx.CallStackPtr];
      end;
    // SUB/FUNCTION call frames (M2): like bcCall/bcReturn but snapshot/restore the
    // register banks so the callee has its own locals and recursion works.
    bcCallSub:
      begin
        FramePush(Ctx);
        Ctx.CallStack[Ctx.CallStackPtr] := Ctx.PC;
        Inc(Ctx.CallStackPtr);
        Ctx.PC := Instr.Immediate - 1;
      end;
    bcReturnSub:
      if Ctx.CallStackPtr > 0 then
      begin
        Dec(Ctx.CallStackPtr);
        Ctx.PC := Ctx.CallStack[Ctx.CallStackPtr];
        FramePop(Ctx);
      end;
    // Block-scoped record reclamation (M8): push the current high-water mark at a loop-body entry,
    // and reclaim to the last mark at the body exit (after the destructors ran).
    bcRecMarkPush:
      begin
        if Ctx.BlockRecMarkTop >= Length(Ctx.BlockRecMark) then
          SetLength(Ctx.BlockRecMark, Ctx.BlockRecMarkTop + 256);
        Ctx.BlockRecMark[Ctx.BlockRecMarkTop] := Ctx.RecordCount;
        Inc(Ctx.BlockRecMarkTop);
      end;
    bcRecMarkPop:
      if Ctx.BlockRecMarkTop > 0 then
      begin
        Dec(Ctx.BlockRecMarkTop);
        if Ctx.BlockRecMark[Ctx.BlockRecMarkTop] < Ctx.RecordCount then
          Ctx.RecordCount := Ctx.BlockRecMark[Ctx.BlockRecMarkTop];
      end;
    // Transfer registers (M2): move a value to/from the non-saved transfer banks.
    bcXferStoreInt:    Ctx.XferInt[Instr.Immediate] := Ctx.IntRegs[Instr.Src1];
    bcXferStoreFloat:  Ctx.XferFloat[Instr.Immediate] := Ctx.FloatRegs[Instr.Src1];
    bcXferStoreString: Ctx.XferStr[Instr.Immediate] := Ctx.StringRegs[Instr.Src1];
    bcXferLoadInt:     Ctx.IntRegs[Instr.Dest] := Ctx.XferInt[Instr.Immediate];
    bcXferLoadFloat:   Ctx.FloatRegs[Instr.Dest] := Ctx.XferFloat[Instr.Immediate];
    bcXferLoadString:  Ctx.StringRegs[Instr.Dest] := Ctx.XferStr[Instr.Immediate];
    // UDT/record heap (M3)
    bcRecordNew:
      // Immediate bit 48: allocate in the shared cross-thread region (e.g. a SHARED UDT scalar).
      if (Instr.Immediate shr 48) and 1 <> 0 then
        Ctx.IntRegs[Instr.Dest] := AllocSharedRecord(Instr.Src1, Instr.Src2,
                                          Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF)
      else
        Ctx.IntRegs[Instr.Dest] := AllocRecord(Ctx, Instr.Src1, Instr.Src2,
                                          Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF);
    bcRecordNewArray:
      RecordNewArrayInit(Ctx, Instr.Src1, Instr.Immediate);  // Src1=array id; Imm=packed slot counts
    // M5.2c: ResolveRec routes the handle to its record (per-thread heap or the shared region).
    bcRecordLoadInt:    Ctx.IntRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.IntData[Instr.Immediate];
    bcRecordLoadFloat:  Ctx.FloatRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.FloatData[Instr.Immediate];
    bcRecordLoadString: Ctx.StringRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate];
    bcRecordStoreInt:   ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.IntData[Instr.Immediate] := Ctx.IntRegs[Instr.Src2];
    bcRecordStoreFloat: ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.FloatData[Instr.Immediate] := Ctx.FloatRegs[Instr.Src2];
    bcRecordStoreString:ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate] := Ctx.StringRegs[Instr.Src2];
    bcRecordTypeId:     Ctx.IntRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.TypeId;
    // System commands
    bcEnd:
      begin
        Ctx.Running := False;
        Ctx.Stopped := False;  // END clears stopped state
      end;
    bcStop:
      begin
        Ctx.Running := False;
        Ctx.Stopped := True;             // Mark as stopped (can CONT)
        Ctx.StoppedPC := Ctx.PC + 1;        // Save PC for resume (next instruction)
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
        else if Instr.Src1 < Ctx.FloatRegCount then
          SleepMs := Trunc(Ctx.FloatRegs[Instr.Src1] * 1000)
        else
          SleepMs := 1000;
        if SleepMs < 0 then SleepMs := 0;
        if SleepMs > 65535000 then SleepMs := 65535000;
        while (SleepMs > 0) and Ctx.Running do
        begin
          if SleepMs > 16 then
          begin
            Sleep(16);
            Dec(SleepMs, 16);
          end
          else
          begin
            Sleep(SleepMs);
            SleepMs := 0;
          end;
          // Process events and render a frame during sleep
          if Assigned(FEventPollCallback) then
            FEventPollCallback()
          else
          begin
            if Assigned(FOutputDevice) then
              PresentFrame;
            if Assigned(FInputDevice) then
            begin
              FInputDevice.ProcessEvents;
              if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
              begin
                Ctx.Running := False;
                FInputDevice.ClearStopRequest;
              end;
            end;
          end;
        end;
      end;
    bcFrame:
      begin
        // FRAME [fps] - wait for frame sync (default 60fps)
        FrameFPS := Ctx.IntRegs[Instr.Src1];
        if FrameFPS < 1 then FrameFPS := 1;
        if FrameFPS > 1000 then FrameFPS := 1000;
        FrameTimeMs := 1000 div FrameFPS;

        NowTick := GetTickCount64;
        if FLastFrameTick = 0 then
          FLastFrameTick := NowTick;

        // Calculate remaining wait time
        TargetTick := FLastFrameTick + QWord(FrameTimeMs);
        if NowTick < TargetTick then
        begin
          WaitMs := Integer(TargetTick - NowTick);
          // Sleep in 16ms chunks, calling EventPollCallback each chunk
          while (WaitMs > 0) and Ctx.Running do
          begin
            ChunkMs := WaitMs;
            if ChunkMs > 16 then ChunkMs := 16;
            Sleep(ChunkMs);
            Dec(WaitMs, ChunkMs);
            if Assigned(FEventPollCallback) then
              FEventPollCallback()
            else begin
              if Assigned(FOutputDevice) then PresentFrame;
              if Assigned(FInputDevice) then begin
                FInputDevice.ProcessEvents;
                if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then begin
                  Ctx.Running := False;
                  FInputDevice.ClearStopRequest;
                end;
              end;
            end;
          end;
        end
        else begin
          // Frame overrun - still call EventPoll once for rendering
          if Assigned(FEventPollCallback) then
            FEventPollCallback()
          else if Assigned(FOutputDevice) then
            PresentFrame;
        end;

        // Use target-based timing to prevent drift
        FLastFrameTick := FLastFrameTick + QWord(FrameTimeMs);
        // Guard against large drift (e.g. after breakpoint)
        NowTick := GetTickCount64;
        if FLastFrameTick + QWord(FrameTimeMs) < NowTick then
          FLastFrameTick := NowTick;
      end;
    bcKey:
      begin
        // KEY n, "text" - define function key
        // KEY n - clear key definition
        // KEY (0) - list all keys (valid keys are 1-12)
        KeyNum := Ctx.IntRegs[Instr.Src1];
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
          if Instr.Src2 < Ctx.StringRegCount then
            FFunctionKeys[KeyNum] := Ctx.StringRegs[Instr.Src2]
          else
            FFunctionKeys[KeyNum] := '';  // Clear key
        end;
      end;
    bcTron:
      begin
        // TRON - Enable trace mode
        Ctx.TraceActive := True;
        Ctx.LastSourceLine := 0;  // Reset last line
      end;
    bcTroff:
      begin
        // TROFF - Disable trace mode
        Ctx.TraceActive := False;
      end;
    bcTrap:
      begin
        // TRAP linenum - Set error handler line
        // If Immediate >= 0, use it directly (constant line number)
        // If Immediate = -1, use register R[Src1] (variable line number)
        if Instr.Immediate >= 0 then
          Ctx.TrapLine := Instr.Immediate
        else
          Ctx.TrapLine := Ctx.IntRegs[Instr.Src1];
        if Ctx.TrapLine > 0 then
        begin
          // Resolve line number to PC
          // For now, we store the line and resolve at error time
          // using the program's line number map
          Ctx.TrapPC := -1;  // Will be resolved when error occurs
        end
        else
        begin
          // Disable trap handler
          Ctx.TrapLine := 0;
          Ctx.TrapPC := -1;
        end;
      end;
    bcResume:
      begin
        // RESUME [line] - Resume at error line or specified line
        if Ctx.InErrorHandler then
        begin
          if Instr.Immediate > 0 then
          begin
            // RESUME <line> with constant line number in Immediate
            Ctx.PC := FindPCForSourceLine(Instr.Immediate);
          end
          else if Instr.Src1 > 0 then
          begin
            // RESUME <line> with line number in register
            Ctx.PC := FindPCForSourceLine(Ctx.IntRegs[Instr.Src1]);
          end
          else if Ctx.ResumePC >= 0 then
          begin
            // Plain RESUME - resume at error line
            Ctx.PC := Ctx.ResumePC;
          end;
          Ctx.InErrorHandler := False;
          Exit;  // Don't increment PC
        end;
        // If not in error handler, just continue
      end;
    bcResumeNext:
      begin
        // RESUME NEXT - Resume at next instruction after error
        if Ctx.InErrorHandler and (Ctx.ResumePC >= 0) then
        begin
          // Jump to the instruction AFTER the one that caused the error
          Ctx.PC := Ctx.ResumePC + 1;
          Ctx.InErrorHandler := False;
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
        if Ctx.DataIndex < Length(FDataPool) then
        begin
          // Use VarAsType for proper Variant to Int64 conversion
          Ctx.IntRegs[Instr.Dest] := VarAsType(FDataPool[Ctx.DataIndex], varInt64);
          Inc(Ctx.DataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataReadFloat:
      begin
        // Read next DATA value into float register
        if Ctx.DataIndex < Length(FDataPool) then
        begin
          // Use VarAsType for proper Variant to Double conversion
          Ctx.FloatRegs[Instr.Dest] := VarAsType(FDataPool[Ctx.DataIndex], varDouble);
          Inc(Ctx.DataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataReadString:
      begin
        // Read next DATA value into string register
        if Ctx.DataIndex < Length(FDataPool) then
        begin
          Ctx.StringRegs[Instr.Dest] := string(FDataPool[Ctx.DataIndex]);
          Inc(Ctx.DataIndex);
        end
        else
          raise Exception.Create('?OUT OF DATA ERROR');
      end;
    bcDataRestore:
      begin
        // Reset DATA pointer
        // Immediate = line number (0 = beginning, ignored for now - line-specific restore not implemented)
        Ctx.DataIndex := 0;
      end;
    // Input commands
    bcGet:
      begin
        // GET A$ - non-blocking character input
        // Returns empty string if no key pressed
        if Assigned(FInputDevice) then
        begin
          FInputDevice.ProcessEvents;
          Ctx.StringRegs[Instr.Dest] := FInputDevice.GetLastChar;
        end
        else
          Ctx.StringRegs[Instr.Dest] := '';
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
                Ctx.Running := False;
                FInputDevice.ClearStopRequest;
                Break;
              end;
              if FInputDevice.HasChar then
                Break;  // Got a character, exit loop
              // Use event poll callback for full rendering (sprites, cursor, etc.)
              if Assigned(FEventPollCallback) then
              begin
                if FEventPollCallback() then
                begin
                  Ctx.Running := False;
                  Break;
                end;
              end
              else if Assigned(FOutputDevice) then
                PresentFrame;
              Sleep(10);  // Prevent busy-wait
            until False;
            // Only read character if we didn't exit due to CTRL+C
            if Ctx.Running then
              Ctx.StringRegs[Instr.Dest] := FInputDevice.GetLastChar
            else
              Ctx.StringRegs[Instr.Dest] := '';
          finally
            FInputDevice.DisableTextInput;
          end;
        end
        else
          Ctx.StringRegs[Instr.Dest] := '';
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
          FOutputDevice.Print(FormatUsingString(Ctx.StringRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]));
        end;
      end;
    bcPudef:
      begin
        // PUDEF " ,.$" - redefine PRINT USING format characters
        // Immediate = string constant index, or Src1 = string register
        if Instr.Src1 <> 0 then
        begin
          // String from register
          if Length(Ctx.StringRegs[Instr.Src1]) >= 1 then
            FPudefFiller := Ctx.StringRegs[Instr.Src1][1];
          if Length(Ctx.StringRegs[Instr.Src1]) >= 2 then
            FPudefComma := Ctx.StringRegs[Instr.Src1][2];
          if Length(Ctx.StringRegs[Instr.Src1]) >= 3 then
            FPudefDecimal := Ctx.StringRegs[Instr.Src1][3];
          if Length(Ctx.StringRegs[Instr.Src1]) >= 4 then
            FPudefDollar := Ctx.StringRegs[Instr.Src1][4];
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
          FOutputDevice.SetCursor(Integer(Ctx.IntRegs[Instr.Src2]), Integer(Ctx.IntRegs[Instr.Dest]));
          FOutputDevice.Print(Ctx.StringRegs[Instr.Immediate and $FFFF]);
        end;
      end;
    bcLoad:
      begin
        // LOAD "filename" - Load and run program from file
        // Src1 = string register with filename
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := False;  // Stop current execution
          FOnFileCommand(Self, 'LOAD', Ctx.StringRegs[Instr.Src1], Ctx.Running);
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
          Ctx.Running := True;  // Default: continue after SAVE
          FOnFileCommand(Self, 'SAVE', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('SAVE command not supported: no handler assigned');
      end;
    bcVerify:
      begin
        // VERIFY "filename" - Verify program against file
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := True;
          FOnFileCommand(Self, 'VERIFY', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('VERIFY command not supported: no handler assigned');
      end;
    bcBload:
      begin
        // BLOAD "filename" - Load bytecode from file
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := False;
          FOnFileCommand(Self, 'BLOAD', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('BLOAD command not supported: no handler assigned');
      end;
    bcBsave:
      begin
        // BSAVE "filename" - Save bytecode to file
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := True;
          FOnFileCommand(Self, 'BSAVE', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('BSAVE command not supported: no handler assigned');
      end;
    bcBoot:
      begin
        // BOOT "filename" - Load and run bytecode
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := False;
          FOnFileCommand(Self, 'BOOT', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('BOOT command not supported: no handler assigned');
      end;
    bcRun:
      begin
        // RUN [linenum] - Run program
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := False;
          FOnFileCommand(Self, 'RUN', IntToStr(Instr.Immediate), Ctx.Running);
        end
        else
          raise Exception.Create('RUN command not supported: no handler assigned');
      end;
    bcList:
      begin
        // LIST [start-end] - List program
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := True;
          FOnFileCommand(Self, 'LIST', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('LIST command not supported: no handler assigned');
      end;
    bcNew:
      begin
        // NEW - Clear program
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := False;
          FOnFileCommand(Self, 'NEW', '', Ctx.Running);
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
          Ctx.Running := True;
          // Format: "start-end" for range, "line" for single line
          if Ctx.IntRegs[Instr.Src1] = Ctx.IntRegs[Instr.Src2] then
            FOnFileCommand(Self, 'DELETE', IntToStr(Ctx.IntRegs[Instr.Src1]), Ctx.Running)
          else
            FOnFileCommand(Self, 'DELETE', IntToStr(Ctx.IntRegs[Instr.Src1]) + '-' + IntToStr(Ctx.IntRegs[Instr.Src2]), Ctx.Running);
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
          Ctx.Running := True;
          // Format: "new,inc,old"
          FOnFileCommand(Self, 'RENUMBER',
            IntToStr(Ctx.IntRegs[Instr.Src1]) + ',' +
            IntToStr(Ctx.IntRegs[Instr.Src2]) + ',' +
            IntToStr(Ctx.IntRegs[Instr.Immediate and $FFFF]), Ctx.Running);
        end
        else
          raise Exception.Create('RENUMBER command not supported: no handler assigned');
      end;
    bcCatalog:
      begin
        // CATALOG/DIR - List directory (requires console callback)
        if Assigned(FOnFileCommand) then
        begin
          Ctx.Running := True;
          FOnFileCommand(Self, 'CATALOG', Ctx.StringRegs[Instr.Src1], Ctx.Running);
        end
        else
          raise Exception.Create('CATALOG command not supported: no handler assigned');
      end;

    // === FILE MANAGEMENT COMMANDS (executed directly in VM) ===
    bcCopyFile:
      begin
        // COPY "src", "dest" [, overwrite]
        // Src1 = source path, Src2 = dest path, Dest = overwrite flag (int reg)
        ExecuteCopyFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2],
                       Ctx.IntRegs[Instr.Dest] <> 0);
      end;

    bcScratch:
      begin
        // SCRATCH "pattern" [, flags]
        // Src1 = pattern, Src2 = flags (int reg): 1 = silent, 2 = force, 3 = both
        ExecuteScratch(Ctx.StringRegs[Instr.Src1],
          (Ctx.IntRegs[Instr.Src2] and 2) <> 0,  // force (bit 1)
          (Ctx.IntRegs[Instr.Src2] and 1) <> 0); // silent (bit 0)
      end;

    bcRenameFile:
      begin
        // RENAME "old", "new"
        // Src1 = old name, Src2 = new name
        ExecuteRenameFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);
      end;

    bcConcat:
      begin
        // CONCAT "src", "dest"
        // Src1 = source, Src2 = dest (append src to dest)
        ExecuteConcat(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);
      end;

    bcMkdir:
      begin
        // MKDIR "path"
        // Src1 = path
        ExecuteMkdir(Ctx.StringRegs[Instr.Src1]);
      end;

    bcChdir:
      begin
        // CHDIR "path"
        // Src1 = path
        ExecuteChdir(Ctx.StringRegs[Instr.Src1]);
      end;

    bcMoveFile:
      begin
        // MOVE "src", "dest"
        // Src1 = source, Src2 = dest
        ExecuteMoveFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);
      end;

  end; // case Op (standard bytecode)
end;

procedure TBytecodeVM.ExecuteSuperinstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
begin
  // Superinstructions use sub-opcode (low byte) for dispatch
  // Full opcode is 0xC800 + SubOp (group 200)
  SubOp := Instr.OpCode and $FF;

  case SubOp of
    // Fused compare-and-branch (Int) - sub-opcodes 0-5
    0: // bcBranchEqInt: if (r[src1] == r[src2]) goto target
      if Ctx.IntRegs[Instr.Src1] = Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    1: // bcBranchNeInt
      if Ctx.IntRegs[Instr.Src1] <> Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    2: // bcBranchLtInt
      if Ctx.IntRegs[Instr.Src1] < Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    3: // bcBranchGtInt
      if Ctx.IntRegs[Instr.Src1] > Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    4: // bcBranchLeInt
      if Ctx.IntRegs[Instr.Src1] <= Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    5: // bcBranchGeInt
      if Ctx.IntRegs[Instr.Src1] >= Ctx.IntRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;

    // Fused compare-and-branch (Float) - sub-opcodes 10-15
    10: // bcBranchEqFloat
      if Ctx.FloatRegs[Instr.Src1] = Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    11: // bcBranchNeFloat
      if Ctx.FloatRegs[Instr.Src1] <> Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    12: // bcBranchLtFloat
      if Ctx.FloatRegs[Instr.Src1] < Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    13: // bcBranchGtFloat
      if Ctx.FloatRegs[Instr.Src1] > Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    14: // bcBranchLeFloat
      if Ctx.FloatRegs[Instr.Src1] <= Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    15: // bcBranchGeFloat
      if Ctx.FloatRegs[Instr.Src1] >= Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;

    // Fused arithmetic-to-dest (Int) - sub-opcodes 20-22
    20: // bcAddIntTo: r[dest] += r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] + Ctx.IntRegs[Instr.Src1];
    21: // bcSubIntTo: r[dest] -= r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] - Ctx.IntRegs[Instr.Src1];
    22: // bcMulIntTo: r[dest] *= r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] * Ctx.IntRegs[Instr.Src1];

    // Fused arithmetic-to-dest (Float) - sub-opcodes 30-33
    30: // bcAddFloatTo: r[dest] += r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] + Ctx.FloatRegs[Instr.Src1];
    31: // bcSubFloatTo: r[dest] -= r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] - Ctx.FloatRegs[Instr.Src1];
    32: // bcMulFloatTo: r[dest] *= r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] * Ctx.FloatRegs[Instr.Src1];
    33: // bcDivFloatTo: r[dest] /= r[src1]
      if Ctx.FloatRegs[Instr.Src1] <> 0.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] / Ctx.FloatRegs[Instr.Src1]
      else
        raise Exception.Create('Division by zero');

    // Fused constant arithmetic (Int) - sub-opcodes 40-42
    40: // bcAddIntConst: r[dest] = r[src1] + immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] + Instr.Immediate;
    41: // bcSubIntConst: r[dest] = r[src1] - immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] - Instr.Immediate;
    42: // bcMulIntConst: r[dest] = r[src1] * immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] * Instr.Immediate;

    // Fused constant arithmetic (Float) - sub-opcodes 50-53
    50: // bcAddFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^);
    51: // bcSubFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^);
    52: // bcMulFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^);
    53: // bcDivFloatConst
      if Double(Pointer(@Instr.Immediate)^) <> 0.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^)
      else
        raise Exception.Create('Division by zero');

    // Fused compare-zero-and-branch (Int) - sub-opcodes 60-61
    60: // bcBranchEqZeroInt
      if Ctx.IntRegs[Instr.Src1] = 0 then
        Ctx.PC := Instr.Immediate - 1;
    61: // bcBranchNeZeroInt
      if Ctx.IntRegs[Instr.Src1] <> 0 then
        Ctx.PC := Instr.Immediate - 1;

    // Fused compare-zero-and-branch (Float) - sub-opcodes 70-71
    70: // bcBranchEqZeroFloat
      if Ctx.FloatRegs[Instr.Src1] = 0.0 then
        Ctx.PC := Instr.Immediate - 1;
    71: // bcBranchNeZeroFloat
      if Ctx.FloatRegs[Instr.Src1] <> 0.0 then
        Ctx.PC := Instr.Immediate - 1;

    // Fused array-store-constant - sub-opcodes 80-82
    80: // bcArrayStoreIntConst
      FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] := Instr.Immediate;
    81: // bcArrayStoreFloatConst
      FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^);
    82: // bcArrayStoreStringConst
      FArrays[Instr.Src1].StringData[Ctx.IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate];

    // Fused loop increment-and-branch (Int) - sub-opcodes 90-93
    90: // bcAddIntToBranchLe: r[dest] += r[src1]; if (r[dest] <= r[src2]) goto target
      begin
        Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] <= Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    91: // bcAddIntToBranchLt: r[dest] += r[src1]; if (r[dest] < r[src2]) goto target
      begin
        Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] < Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    92: // bcSubIntToBranchGe: r[dest] -= r[src1]; if (r[dest] >= r[src2]) goto target
      begin
        Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] >= Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    93: // bcSubIntToBranchGt: r[dest] -= r[src1]; if (r[dest] > r[src2]) goto target
      begin
        Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] > Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;

    // FMA (Fused Multiply-Add) - sub-opcodes 100-103
    100: // bcMulAddFloat: dest = c + a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] + Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    101: // bcMulSubFloat: dest = c - a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] - Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    102: // bcMulAddToFloat: dest += a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] + Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    103: // bcMulSubToFloat: dest -= a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] - Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];

    // Array Load + Arithmetic - sub-opcodes 110-112
    110: // bcArrayLoadAddFloat: dest = acc + arr[idx]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]];
    111: // bcArrayLoadSubFloat: dest = acc - arr[idx]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]];
    112: // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
        FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]] / Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF];

    // Square-Sum patterns - sub-opcodes 120-121
    120: // bcSquareSumFloat: dest = x*x + y*y
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src1] +
                                Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Src2];
    121: // bcAddSquareFloat: dest = sum + x*x
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Src2];

    // Mul-Mul and Add-Sqrt - sub-opcodes 130-131
    130: // bcMulMulFloat: dest = a*b*c
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Immediate];
    131: // bcAddSqrtFloat: dest = sqrt(a+b)
      Ctx.FloatRegs[Instr.Dest] := Sqrt(Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2]);

    // Array Load + Branch - sub-opcodes 140-141
    140: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
      if FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] <> 0 then
        Ctx.PC := Instr.Immediate - 1;
    141: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
      if FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] = 0 then
        Ctx.PC := Instr.Immediate - 1;

    // Array Reverse Range - sub-opcode 156
    156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
      begin
        Ctx.StartIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.EndIdx := Ctx.IntRegs[Instr.Dest] - 1;
        Ctx.ArrIdxTmp := Instr.Src1;
        while Ctx.StartIdx < Ctx.EndIdx do
        begin
          Ctx.SwapTempInt := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.StartIdx];
          FArrays[Ctx.ArrIdxTmp].IntData[Ctx.StartIdx] := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.EndIdx];
          FArrays[Ctx.ArrIdxTmp].IntData[Ctx.EndIdx] := Ctx.SwapTempInt;
          Inc(Ctx.StartIdx);
          Dec(Ctx.EndIdx);
        end;
      end;

    // Array Shift Left - sub-opcode 157
    157: // bcArrayShiftLeft: shift left and rotate first to end+1
      begin
        Ctx.StartIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.EndIdx := Ctx.IntRegs[Instr.Dest];
        Ctx.ArrIdxTmp := Instr.Src1;
        Ctx.FirstVal := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.StartIdx];
        Ctx.LoopIdx := Ctx.StartIdx;
        while Ctx.LoopIdx <= Ctx.EndIdx do
        begin
          FArrays[Ctx.ArrIdxTmp].IntData[Ctx.LoopIdx] := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.LoopIdx + 1];
          Inc(Ctx.LoopIdx);
        end;
        FArrays[Ctx.ArrIdxTmp].IntData[Ctx.EndIdx + 1] := Ctx.FirstVal;
      end;

    // Array Swap (Int) - sub-opcode 250
    250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
      begin
        Ctx.SwapTempInt := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]];
        FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Dest]];
        FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Dest]] := Ctx.SwapTempInt;
      end;

    // Self-increment/decrement (Int) - sub-opcodes 251-252
    251: // bcAddIntSelf: r[dest] += r[src1]
      Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
    252: // bcSubIntSelf: r[dest] -= r[src1]
      Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);

    // Array Load to register (Int) - sub-opcode 253
    253: // bcArrayLoadIntTo: r[dest] = arr[src1][r[src2]]
      Ctx.IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]];

    // Array Copy Element - sub-opcode 254
    254: // bcArrayCopyElement: arr_dest[idx] = arr_src[idx]
      FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]];

    // Array Move Element - sub-opcode 255
    255: // bcArrayMoveElement: arr[dest_idx] = arr[src_idx]
      FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src1]];

  else
    raise Exception.CreateFmt('Unknown superinstruction sub-opcode %d (full: %d) at PC=%d',
      [SubOp, Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.Step;
var
  Instr: TBytecodeInstruction;
  CurrentSourceLine: Integer;
begin
  if (FProgram = nil) or (FCtx.PC >= FProgram.GetInstructionCount) then
  begin
    FCtx.Running := False;
    Exit;
  end;
  Instr := FProgram.GetInstruction(FCtx.PC);

  // TRON trace output: print line number when it changes
  // SourceLine > 0 only when compiled with TRON (debug mode) active
  CurrentSourceLine := FProgram.GetSourceLine(FCtx.PC);
  if (CurrentSourceLine > 0) and (CurrentSourceLine <> FCtx.LastSourceLine) then
  begin
    FCtx.LastSourceLine := CurrentSourceLine;
    if Assigned(FOutputDevice) then
      FOutputDevice.Print('[' + IntToStr(CurrentSourceLine) + ']');
  end;

  {$IFDEF ENABLE_PROFILER}
  // Profiler: BeforeInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
    FProfiler.BeforeInstruction(FCtx.PC, Instr.OpCode);
  {$ENDIF}

  ExecuteInstruction(FCtx, Instr);

  {$IFDEF ENABLE_PROFILER}
  // Profiler: AfterInstruction hook
  if Assigned(FProfiler) and FProfiler.Enabled then
  begin
    FProfiler.AfterInstruction(FCtx.PC, Instr.OpCode);
    // Track superinstructions
    if Instr.OpCode >= bcGroupSuper then
      FProfiler.OnSuperinstruction(Instr.OpCode, 1);
  end;
  {$ENDIF}

  {$IFDEF ENABLE_INSTRUCTION_COUNTING}
  Inc(FInstructionsExecuted);
  {$ENDIF}
  Inc(FCtx.PC);
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

// Parse the leading integer of a string (optional sign + digits), stopping at the
// first non-numeric character - matches FreeBASIC VALINT/VALLNG/VALUINT. Returns 0
// when no digits are present.
function ParseLeadingInt64(const S: string): Int64;
var
  I, Len: Integer;
  Neg: Boolean;
begin
  Result := 0;
  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do Inc(I);  // skip leading whitespace
  Neg := False;
  if (I <= Len) and ((S[I] = '+') or (S[I] = '-')) then
  begin
    Neg := (S[I] = '-');
    Inc(I);
  end;
  while (I <= Len) and (S[I] >= '0') and (S[I] <= '9') do
  begin
    Result := Result * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if Neg then Result := -Result;
end;

// Render an Int64 in an arbitrary base (2..16) as an unsigned bit pattern, no
// leading zeros - mirrors HEX$ semantics for OCT(n)/BIN(n) (FreeBASIC B1.3).
function IntToBaseStr(Value: Int64; Base: Integer): string;
const
  Digits: array[0..15] of Char = '0123456789ABCDEF';
var
  U: QWord;
begin
  U := QWord(Value);
  if U = 0 then
    Exit('0');
  Result := '';
  while U > 0 do
  begin
    Result := Digits[U mod QWord(Base)] + Result;
    U := U div QWord(Base);
  end;
end;

procedure TBytecodeVM.ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  Len, StartPos, Count: Integer;
  S, SubStr: string;
begin
  SubOp := Instr.OpCode and $FF;  // Extract sub-opcode (low byte)
  case SubOp of
    0: // bcStrConcat
      Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1] + Ctx.StringRegs[Instr.Src2];
    1: // bcStrLen
      Ctx.IntRegs[Instr.Dest] := Length(Ctx.StringRegs[Instr.Src1]);
    2: // bcStrLeft
      begin
        Len := Ctx.IntRegs[Instr.Src2];
        if Len < 0 then Len := 0;
        Ctx.StringRegs[Instr.Dest] := Copy(Ctx.StringRegs[Instr.Src1], 1, Len);
      end;
    3: // bcStrRight
      begin
        Len := Ctx.IntRegs[Instr.Src2];
        S := Ctx.StringRegs[Instr.Src1];
        if Len < 0 then Len := 0;
        if Len > Length(S) then Len := Length(S);
        Ctx.StringRegs[Instr.Dest] := Copy(S, Length(S) - Len + 1, Len);
      end;
    4: // bcStrMid - MID$(s, start, len)
      begin
        // Src2 = start position register (int)
        // Immediate = length register index (low 16 bits)
        StartPos := Ctx.IntRegs[Instr.Src2];
        Count := Ctx.IntRegs[Instr.Immediate and $FFFF];
        if StartPos < 1 then StartPos := 1;
        if Count < 0 then Count := 0;
        Ctx.StringRegs[Instr.Dest] := Copy(Ctx.StringRegs[Instr.Src1], StartPos, Count);
      end;
    5: // bcStrAsc
      begin
        S := Ctx.StringRegs[Instr.Src1];
        if Length(S) > 0 then
          Ctx.IntRegs[Instr.Dest] := Ord(S[1])
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    6: // bcStrChr
      Ctx.StringRegs[Instr.Dest] := Chr(Ctx.IntRegs[Instr.Src1] and $FF);
    12: // bcStrLTrim - LTRIM(s)
      Ctx.StringRegs[Instr.Dest] := TrimLeft(Ctx.StringRegs[Instr.Src1]);
    13: // bcStrRTrim - RTRIM(s)
      Ctx.StringRegs[Instr.Dest] := TrimRight(Ctx.StringRegs[Instr.Src1]);
    14: // bcStrTrim - TRIM(s)
      Ctx.StringRegs[Instr.Dest] := Trim(Ctx.StringRegs[Instr.Src1]);
    15: // bcStrUCase - UCASE(s)
      Ctx.StringRegs[Instr.Dest] := UpperCase(Ctx.StringRegs[Instr.Src1]);
    16: // bcStrLCase - LCASE(s)
      Ctx.StringRegs[Instr.Dest] := LowerCase(Ctx.StringRegs[Instr.Src1]);
    17: // bcStrInstrRev - INSTRREV(str, sub) -> position of last occurrence (1-based, 0 if none)
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];
        Len := 0;
        if SubStr <> '' then
          for StartPos := 1 to Length(S) - Length(SubStr) + 1 do
            if Copy(S, StartPos, Length(SubStr)) = SubStr then Len := StartPos;
        Ctx.IntRegs[Instr.Dest] := Len;
      end;
    18: // bcStrSpace - SPACE(n) -> n spaces
      begin
        Count := Ctx.IntRegs[Instr.Src1];
        if Count < 0 then Count := 0;
        Ctx.StringRegs[Instr.Dest] := StringOfChar(' ', Count);
      end;
    7: // bcStrStr - STR$(n)
      Ctx.StringRegs[Instr.Dest] := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]);
    8: // bcStrVal - VAL(s)
      begin
        S := Trim(Ctx.StringRegs[Instr.Src1]);
        if not TryStrToFloat(S, Ctx.FloatRegs[Instr.Dest]) then
          Ctx.FloatRegs[Instr.Dest] := 0.0;
      end;
    9: // bcStrHex - HEX$(n) - full INT64 range, no leading zeros
      begin
        S := IntToHex(Ctx.IntRegs[Instr.Src1], 1);  // Minimum 1 digit
        // IntToHex with digits=1 still pads, so trim leading zeros
        while (Length(S) > 1) and (S[1] = '0') do
          Delete(S, 1, 1);
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    10: // bcStrInstr - INSTR(haystack, needle [,start])
      begin
        // Src1 = haystack, Src2 = needle, Immediate = start position (1-based)
        StartPos := 1;
        if Instr.Immediate > 0 then
          StartPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        if StartPos < 1 then StartPos := 1;
        Ctx.IntRegs[Instr.Dest] := Pos(Ctx.StringRegs[Instr.Src2],
          Copy(Ctx.StringRegs[Instr.Src1], StartPos, MaxInt));
        if Ctx.IntRegs[Instr.Dest] > 0 then
          Inc(Ctx.IntRegs[Instr.Dest], StartPos - 1);
      end;
    11: // bcStrErr - ERR$(n)
      Ctx.StringRegs[Instr.Dest] := SedaiExecutorErrors.GetErrorCodeDescription(Ctx.IntRegs[Instr.Src1]);
    19: // bcStrOct - OCT(n) - octal string, no leading zeros, full INT64 range
      Ctx.StringRegs[Instr.Dest] := IntToBaseStr(Ctx.IntRegs[Instr.Src1], 8);
    20: // bcStrBin - BIN(n) - binary string, no leading zeros, full INT64 range
      Ctx.StringRegs[Instr.Dest] := IntToBaseStr(Ctx.IntRegs[Instr.Src1], 2);
    21: // bcStrValInt - VALINT/VALLNG/VALUINT(s) - parse leading integer (0 if none)
      Ctx.IntRegs[Instr.Dest] := ParseLeadingInt64(Ctx.StringRegs[Instr.Src1]);
  else
    raise Exception.CreateFmt('Unknown string opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcMathSin
      Ctx.FloatRegs[Instr.Dest] := Sin(Ctx.FloatRegs[Instr.Src1]);
    1: // bcMathCos
      Ctx.FloatRegs[Instr.Dest] := Cos(Ctx.FloatRegs[Instr.Src1]);
    2: // bcMathTan
      Ctx.FloatRegs[Instr.Dest] := Tan(Ctx.FloatRegs[Instr.Src1]);
    3: // bcMathAtn
      Ctx.FloatRegs[Instr.Dest] := ArcTan(Ctx.FloatRegs[Instr.Src1]);
    4: // bcMathLog
      if Ctx.FloatRegs[Instr.Src1] > 0 then
        Ctx.FloatRegs[Instr.Dest] := Ln(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('LOG of non-positive number');
    5: // bcMathExp
      Ctx.FloatRegs[Instr.Dest] := Exp(Ctx.FloatRegs[Instr.Src1]);
    6: // bcMathSqr
      begin
        if Ctx.FloatRegs[Instr.Src1] < 0 then
          raise Exception.CreateFmt('Square root of negative number: %.17e', [Ctx.FloatRegs[Instr.Src1]])
        else
          Ctx.FloatRegs[Instr.Dest] := Sqrt(Ctx.FloatRegs[Instr.Src1]);
      end;
    7: // bcMathAbs
      Ctx.FloatRegs[Instr.Dest] := Abs(Ctx.FloatRegs[Instr.Src1]);
    8: // bcMathSgn
      if Ctx.FloatRegs[Instr.Src1] > 0 then
        Ctx.FloatRegs[Instr.Dest] := 1
      else if Ctx.FloatRegs[Instr.Src1] < 0 then
        Ctx.FloatRegs[Instr.Dest] := -1
      else
        Ctx.FloatRegs[Instr.Dest] := 0;
    9: // bcMathInt
      Ctx.FloatRegs[Instr.Dest] := Floor(Ctx.FloatRegs[Instr.Src1]);
    10: // bcMathRnd
      Ctx.FloatRegs[Instr.Dest] := Random;
    11: // bcMathLog10
      if Ctx.FloatRegs[Instr.Src1] > 0 then
        Ctx.FloatRegs[Instr.Dest] := Log10(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOG10 of non-positive number');
    12: // bcMathLog2
      if Ctx.FloatRegs[Instr.Src1] > 0 then
        Ctx.FloatRegs[Instr.Dest] := Log2(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOG2 of non-positive number');
    13: // bcMathLogN
      begin
        // LOGN(base, x) - Src1 = base, Src2 = x
        if (Ctx.FloatRegs[Instr.Src1] > 0) and (Ctx.FloatRegs[Instr.Src1] <> 1) and (Ctx.FloatRegs[Instr.Src2] > 0) then
          Ctx.FloatRegs[Instr.Dest] := LogN(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2])
        else if Ctx.FloatRegs[Instr.Src1] <= 0 then
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN base must be positive')
        else if Ctx.FloatRegs[Instr.Src1] = 1 then
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN base cannot be 1')
        else
          raise Exception.Create('?ILLEGAL QUANTITY ERROR: LOGN of non-positive number');
      end;
    14: // bcStrDec - DEC(hexstring) - convert hex string to decimal integer
      begin
        // Src1 is string register, Dest is int register
        // Ctx.StringRegs is used, result goes to Ctx.IntRegs
        try
          Ctx.IntRegs[Instr.Dest] := StrToInt64('$' + Ctx.StringRegs[Instr.Src1]);
        except
          on E: Exception do
            raise Exception.CreateFmt('?ILLEGAL QUANTITY ERROR: Invalid hex string "%s"', [Ctx.StringRegs[Instr.Src1]]);
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown math opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

// ERASE arr (B1.4): reset every element of an existing array to its default
// (0 / 0.0 / ""), keeping the current dimensions.
procedure TBytecodeVM.EraseArray(ArrayIdx: Integer);
var
  k: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then Exit;
  case FArrays[ArrayIdx].ElementType of
    0: for k := 0 to High(FArrays[ArrayIdx].IntData) do FArrays[ArrayIdx].IntData[k] := 0;
    1: for k := 0 to High(FArrays[ArrayIdx].FloatData) do FArrays[ArrayIdx].FloatData[k] := 0.0;
    2: for k := 0 to High(FArrays[ArrayIdx].StringData) do FArrays[ArrayIdx].StringData[k] := '';
  end;
end;

// REDIM [PRESERVE] arr(ub) (B1.4): re-dimension an existing 1-D array, keeping its
// original lower bound. PRESERVE keeps the overlapping elements; otherwise all are
// reset to default. New element type is unchanged (taken from the existing array).
procedure TBytecodeVM.RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean);
var
  Lb, NewSize, k: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then Exit;
  Lb := 0;
  if Length(FArrays[ArrayIdx].LowerBounds) > 0 then Lb := FArrays[ArrayIdx].LowerBounds[0];
  NewSize := NewUpper - Lb + 1;
  if NewSize < 0 then NewSize := 0;
  case FArrays[ArrayIdx].ElementType of
    0: begin
         SetLength(FArrays[ArrayIdx].IntData, NewSize);
         if not Preserve then
           for k := 0 to NewSize - 1 do FArrays[ArrayIdx].IntData[k] := 0;
       end;
    1: begin
         SetLength(FArrays[ArrayIdx].FloatData, NewSize);
         if not Preserve then
           for k := 0 to NewSize - 1 do FArrays[ArrayIdx].FloatData[k] := 0.0;
       end;
    2: begin
         SetLength(FArrays[ArrayIdx].StringData, NewSize);
         if not Preserve then
           for k := 0 to NewSize - 1 do FArrays[ArrayIdx].StringData[k] := '';
       end;
  end;
  // Collapse to a single dimension with the same lower bound.
  FArrays[ArrayIdx].DimCount := 1;
  SetLength(FArrays[ArrayIdx].Dimensions, 1);
  FArrays[ArrayIdx].Dimensions[0] := NewSize;
  SetLength(FArrays[ArrayIdx].LowerBounds, 1);
  FArrays[ArrayIdx].LowerBounds[0] := Lb;
  FArrays[ArrayIdx].TotalSize := NewSize;
end;

procedure TBytecodeVM.ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ArrayIdx, LinearIdx, i, ProdDims, ArrLowerBound: Integer;
  ArrInfo: TSSAArrayInfo;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcArrayLoad (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        case FArrays[ArrayIdx].ElementType of
          0: Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
          1: Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
          2: Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
        end;
      end;
    1: // bcArrayStore (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        case FArrays[ArrayIdx].ElementType of
          0: FArrays[ArrayIdx].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
          1: FArrays[ArrayIdx].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
          2: FArrays[ArrayIdx].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
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
              // Variable upper bound: size = ub - lb + 1 (lb = the FreeBASIC explicit lower bound, 0 if none).
              ArrLowerBound := 0;
              if i <= High(ArrInfo.LowerBounds) then ArrLowerBound := ArrInfo.LowerBounds[i];
              case ArrInfo.DimRegTypes[i] of
                srtInt: FArrays[ArrayIdx].Dimensions[i] := Ctx.IntRegs[ArrInfo.DimRegisters[i]] - ArrLowerBound + 1;
                srtFloat: FArrays[ArrayIdx].Dimensions[i] := Trunc(Ctx.FloatRegs[ArrInfo.DimRegisters[i]]) - ArrLowerBound + 1;
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
        // Record per-dimension lower bounds for LBOUND/UBOUND (B1.4); default 0.
        SetLength(FArrays[ArrayIdx].LowerBounds, ArrInfo.DimCount);
        for i := 0 to ArrInfo.DimCount - 1 do
          if i <= High(ArrInfo.LowerBounds) then
            FArrays[ArrayIdx].LowerBounds[i] := ArrInfo.LowerBounds[i]
          else
            FArrays[ArrayIdx].LowerBounds[i] := 0;
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
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    4: // bcArrayLoadFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    5: // bcArrayLoadString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
        {$ENDIF}
      end;
    6: // bcArrayStoreInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
    7: // bcArrayStoreFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
    8: // bcArrayStoreString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (LinearIdx < 0) or (LinearIdx >= FArrays[ArrayIdx].TotalSize) then
          raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
        FArrays[ArrayIdx].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
        {$IFDEF ENABLE_PROFILER}
        if Assigned(FProfiler) and FProfiler.Enabled then
          FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
        {$ENDIF}
      end;
    9: // bcArrayLBound - LBOUND(arr[, dim]) - Src2 = 0-based dim index (B1.4)
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx];
      end;
    10: // bcArrayUBound - UBOUND(arr[, dim]) - upper = lower + size - 1 (B1.4)
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx]
                                   + FArrays[ArrayIdx].Dimensions[LinearIdx] - 1;
      end;
    11: // bcArrayErase - ERASE arr - reset all elements to default, keep size (B1.4)
      EraseArray(Instr.Src1);
    12: // bcArrayRedim - REDIM [PRESERVE] arr(ub) (B1.4); Src2=ub reg, Immediate bit0=preserve
      RedimArray(Instr.Src1, Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0);
  else
    raise Exception.CreateFmt('Unknown array opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  PrintStr, InputStr, CmdNewLine: string;
  InputVal: Double;
  NextTabCol, TabIdx: Integer;
  CmdErr: Integer;  // Error code for CMD-redirected output
begin
  CmdErr := 0;
  CmdNewLine := #13;  // CR for file newlines
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcPrint (float)
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          Inc(Ctx.CursorCol, Length(PrintStr));
        end;
      end;
    1: // bcPrintLn (float)
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr);
          FOnFileData(Self, 'PRINT#', FCmdHandle, CmdNewLine, CmdErr);
        end
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          FOutputDevice.NewLine;  // NewLine already calls Present
          Ctx.CursorCol := 0;
        end;
      end;
    2: // bcPrintString
      begin
        PrintStr := FConsoleBehavior.FormatString(Ctx.StringRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          Inc(Ctx.CursorCol, Length(PrintStr));
        end;
      end;
    3: // bcPrintStringLn
      begin
        PrintStr := FConsoleBehavior.FormatString(Ctx.StringRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr);
          FOnFileData(Self, 'PRINT#', FCmdHandle, CmdNewLine, CmdErr);
        end
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          FOutputDevice.NewLine;  // NewLine already calls Present
          Ctx.CursorCol := 0;
        end;
      end;
    4: // bcPrintInt
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.IntRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          Inc(Ctx.CursorCol, Length(PrintStr));
        end;
      end;
    5: // bcPrintIntLn
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.IntRegs[Instr.Src1]);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr);
          FOnFileData(Self, 'PRINT#', FCmdHandle, CmdNewLine, CmdErr);
        end
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          FOutputDevice.NewLine;  // NewLine already calls Present
          Ctx.CursorCol := 0;
        end;
      end;
    6: // bcPrintComma
      if Assigned(FOutputDevice) then
      begin
        NextTabCol := FConsoleBehavior.GetNextTabPosition(Ctx.CursorCol);
        if NextTabCol = 0 then
        begin
          FOutputDevice.NewLine;
          Ctx.CursorCol := 0;
        end
        else if FConsoleBehavior.CommaAction = caTabZone then
        begin
          while Ctx.CursorCol < NextTabCol do
          begin
            FOutputDevice.Print(' ');
            Inc(Ctx.CursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caFixedSpaces then
        begin
          for TabIdx := 1 to FConsoleBehavior.CommaSpaces do
          begin
            FOutputDevice.Print(' ');
            Inc(Ctx.CursorCol);
          end;
        end
        else if FConsoleBehavior.CommaAction = caNewLine then
        begin
          FOutputDevice.NewLine;
          Ctx.CursorCol := 0;
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
              Inc(Ctx.CursorCol);
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
        NextTabCol := Ctx.IntRegs[Instr.Src1];
        if NextTabCol < 0 then NextTabCol := 0;
        // Only move forward if we're before the target column
        while Ctx.CursorCol < NextTabCol do
        begin
          FOutputDevice.Print(' ');
          Inc(Ctx.CursorCol);
        end;
        // If Ctx.CursorCol >= NextTabCol, do nothing (as per C128 behavior)
      end;
    9: // bcPrintSpc
      if Assigned(FOutputDevice) then
      begin
        // Src1 = register containing space count (always a register from SSA)
        TabIdx := Ctx.IntRegs[Instr.Src1];
        while TabIdx > 0 do
        begin
          FOutputDevice.Print(' ');
          Inc(Ctx.CursorCol);
          Dec(TabIdx);
        end;
      end;
    10: // bcPrintNewLine
      begin
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, CmdNewLine, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.NewLine;
          Ctx.CursorCol := 0;
        end;
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
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars
          InputStr := FInputDevice.ReadLine('? ', False, not FC128InputMode, True);
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            Ctx.Running := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            Ctx.FloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
              FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
          end;
        until False;
      end;
    13: // bcInputInt
      if Assigned(FInputDevice) then
      begin
        // Print initial prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars (no decimal for int)
          InputStr := Trim(FInputDevice.ReadLine('? ', False, not FC128InputMode, False));
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            Ctx.Running := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            if (InputVal >= Low(Int64)) and (InputVal <= High(Int64)) then
            begin
              Ctx.IntRegs[Instr.Dest] := Trunc(InputVal);
              Break;
            end
            else if Assigned(FOutputDevice) then
            begin
              FOutputDevice.Print('?REDO FROM START');
              FOutputDevice.NewLine;
              // Reprint prompt for retry
              if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
                FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
            end;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
              FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
          end;
        until False;
      end;
    14: // bcInputFloat
      if Assigned(FInputDevice) then
      begin
        // Print initial prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        repeat
          // C128 mode: accept all, validate after; Mask mode: filter invalid chars
          InputStr := Trim(FInputDevice.ReadLine('? ', False, not FC128InputMode, True));
          // Check for CTRL+END stop request or window close
          if FInputDevice.ShouldStop or FInputDevice.ShouldQuit then
          begin
            Ctx.Running := False;
            FInputDevice.ClearStopRequest;
            Break;
          end;
          if TryStrToFloat(InputStr, InputVal) then
          begin
            Ctx.FloatRegs[Instr.Dest] := InputVal;
            Break;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
              FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
          end;
        until False;
      end;
    15: // bcInputString
      if Assigned(FInputDevice) then
      begin
        // Print prompt (from Src1 register if set) + "? "
        if Assigned(FOutputDevice) then
        begin
          if (Instr.Src1 > 0) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        Ctx.StringRegs[Instr.Dest] := FInputDevice.ReadLine('? ', False, False, False);
        if FInputDevice.ShouldStop then
        begin
          Ctx.Running := False;
          FInputDevice.ClearStopRequest;
        end;
      end;
  else
    raise Exception.CreateFmt('Unknown I/O opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteSpecialVarOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
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
      Ctx.IntRegs[Instr.Dest] := ((GetTickCount64 - FStartTicks) * 60) div 1000;
    1: // bcLoadTIS - return current time as "HHMMSS" string
      begin
        TimeCurrentTime := Now + (FTimeOffset / 86400000);
        DecodeTime(TimeCurrentTime, TimeH, TimeM, TimeS, TimeMS);
        Ctx.StringRegs[Instr.Dest] := Format('%.2d%.2d%.2d', [TimeH, TimeM, TimeS]);
      end;
    2: // bcStoreTIS - set time offset
      begin
        TimeStr := Ctx.StringRegs[Instr.Src1];
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
        Ctx.StringRegs[Instr.Dest] := Format('%.4d%.2d%.2d', [DateY, DateM, DateD]);
      end;
    4: // bcFre - return available memory in bytes
      begin
        {$IFDEF WINDOWS}
        Ctx.IntRegs[Instr.Dest] := GetFPCHeapStatus.CurrHeapFree;
        {$ELSE}
        Ctx.IntRegs[Instr.Dest] := GetFPCHeapStatus.CurrHeapFree;
        {$ENDIF}
      end;
    5: // bcLoadEL - return last error line number
      Ctx.IntRegs[Instr.Dest] := Ctx.LastErrorLine;
    6: // bcLoadER - return last error code
      Ctx.IntRegs[Instr.Dest] := Ctx.LastErrorCode;
    7: // bcLoadERRS - return last error message (variable form)
      Ctx.StringRegs[Instr.Dest] := Ctx.LastErrorMessage;
    8: // bcPeek - read from memory-mapped location
      begin
        if Assigned(FMemoryMapper) then
          Ctx.IntRegs[Instr.Dest] := FMemoryMapper.Peek(Ctx.IntRegs[Instr.Src1])
        else
          Ctx.IntRegs[Instr.Dest] := 0;  // No memory mapper = return 0
      end;
    9: // bcPoke - write to memory-mapped location
      begin
        if Assigned(FMemoryMapper) then
          FMemoryMapper.Poke(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
        // If no memory mapper, silently ignore (like real hardware)
      end;
    10: // bcLoadCWDS - return current working directory
      Ctx.StringRegs[Instr.Dest] := GetCurrentDir;
  else
    raise Exception.CreateFmt('Unknown special variable opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  DrawMode: Integer;
begin
  // M5.3: off the render-owner thread, defer to the queue instead of touching SDL. Dormant on
  // the single-threaded path (FHasWorkers = False short-circuits before any thread-id check).
  if FHasWorkers and not IsRenderOwner then
  begin
    EnqueueDeferredOp(Ctx, dckGraphics, Instr);
    Exit;
  end;
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcGraphicRGBA
      Ctx.IntRegs[Instr.Dest] :=
        ((Ctx.IntRegs[Instr.Immediate and $FFFF] and $FF) shl 24) or
        ((Ctx.IntRegs[Instr.Src1] and $FF) shl 16) or
        ((Ctx.IntRegs[Instr.Src2] and $FF) shl 8) or
        (Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF] and $FF);
    1: // bcGraphicSetMode
      if Assigned(FOutputDevice) then
        FOutputDevice.SetGraphicMode(
          TGraphicMode(Ctx.IntRegs[Instr.Src1] and $F),
          Ctx.IntRegs[Instr.Src2] <> 0,
          Ctx.IntRegs[Instr.Immediate and $FFFF]
        );
    2: // bcGraphicBox
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.DrawBoxWithColor(
          Ctx.IntRegs[Instr.Src2],
          Ctx.IntRegs[Instr.Dest],
          Ctx.IntRegs[(Instr.Immediate) and $FFF],
          Ctx.IntRegs[(Instr.Immediate shr 12) and $FFF],
          UInt32(Ctx.IntRegs[Instr.Src1]),
          Ctx.FloatRegs[(Instr.Immediate shr 24) and $FFF],
          Ctx.IntRegs[(Instr.Immediate shr 36) and $FFF] <> 0
        );
      end;
    3: // bcGraphicCircle
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.DrawCircleWithColor(
          Ctx.IntRegs[Instr.Src2],
          Ctx.IntRegs[Instr.Dest],
          Ctx.IntRegs[(Instr.Immediate) and $3FF],
          Ctx.IntRegs[(Instr.Immediate shr 10) and $3FF],
          UInt32(Ctx.IntRegs[Instr.Src1]),
          Ctx.FloatRegs[(Instr.Immediate shr 20) and $3FF],
          Ctx.FloatRegs[(Instr.Immediate shr 30) and $3FF],
          Ctx.FloatRegs[(Instr.Immediate shr 40) and $3FF],
          Ctx.FloatRegs[(Instr.Immediate shr 50) and $3FF]
        );
      end;
    4: // bcGraphicDraw
      if Assigned(FOutputDevice) then
      begin
        DrawMode := Instr.Immediate and $7FFF;
        case DrawMode of
          0: FOutputDevice.SetPixelCursor(Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest]);
          1:
            begin
              FOutputDevice.DrawLine(
                FOutputDevice.GetPixelCursorX,
                FOutputDevice.GetPixelCursorY,
                Ctx.IntRegs[Instr.Src2],
                Ctx.IntRegs[Instr.Dest],
                UInt32(Ctx.IntRegs[Instr.Src1])
              );
              FOutputDevice.SetPixelCursor(Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest]);
            end;
          2:
            begin
              FOutputDevice.SetPixel(Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest], UInt32(Ctx.IntRegs[Instr.Src1]));
              FOutputDevice.SetPixelCursor(Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest]);
            end;
        end;
      end;
    5: // bcGraphicLocate
      if Assigned(FOutputDevice) then
        FOutputDevice.SetPixelCursor(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
    6: // bcGraphicRdot
      if Assigned(FOutputDevice) then
      begin
        case Ctx.IntRegs[Instr.Src1] of
          0: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorX;
          1: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetPixelCursorY;
          2: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetPixelIndex(
               FOutputDevice.GetPixelCursorX, FOutputDevice.GetPixelCursorY);
        else
          Ctx.IntRegs[Instr.Dest] := 0;
        end;
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    7: // bcGraphicGetMode
      if Assigned(FOutputDevice) then
      begin
        case Ctx.IntRegs[Instr.Src1] of
          0: Ctx.IntRegs[Instr.Dest] := Ord(FOutputDevice.GetGraphicMode);
        else
          Ctx.IntRegs[Instr.Dest] := 0;
        end;
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    8: // bcGraphicColor - COLOR source, color
      if Assigned(FOutputDevice) then
      begin
        // Src1 = source (0-6), Src2 = color
        FOutputDevice.SetColorSource(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
      end;
    9: // bcGraphicWidth - WIDTH n (1 or 2)
      if Assigned(FOutputDevice) then
      begin
        FOutputDevice.SetLineWidth(Ctx.IntRegs[Instr.Src1]);
      end;
    10: // bcGraphicScale - SCALE n [,xmax, ymax]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = enable (0/1), Src2 = xmax, Dest = ymax
        FOutputDevice.SetScale(Ctx.IntRegs[Instr.Src1] <> 0, Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest]);
      end;
    11: // bcGraphicPaint - PAINT source, x, y, mode
      if Assigned(FOutputDevice) then
      begin
        // Src1 = source, Src2 = x, Dest = y, Immediate = mode
        // All parameters are INT registers
        FOutputDevice.FloodFill(Ctx.IntRegs[Instr.Src1],
          Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Immediate and $FFFF]);
      end;
    12: // bcGraphicWindow - WINDOW col1, row1, col2, row2 [,clear]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = col1, Src2 = row1, Dest = col2
        // Immediate bits 0-15 = row2 register, bits 16-31 = clear register
        FOutputDevice.SetWindow(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2],
          Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Immediate and $FFFF],
          Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF] <> 0);
      end;
    13: // bcGraphicSShape - SSHAPE A$, x1, y1 [,x2, y2]
      if Assigned(FOutputDevice) then
      begin
        // Dest = string reg index, Src1 = x1, Src2 = y1 (INT)
        // Immediate bits 0-15 = x2, bits 16-31 = y2 (INT)
        Ctx.StringRegs[Instr.Dest] := FOutputDevice.SaveShape(
          Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2],
          Ctx.IntRegs[Instr.Immediate and $FFFF], Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF]);
      end;
    14: // bcGraphicGShape - GSHAPE A$, x, y [,mode]
      if Assigned(FOutputDevice) then
      begin
        // Src1 = string reg index, Src2 = x, Dest = y (INT), Immediate = mode
        FOutputDevice.LoadShape(Ctx.StringRegs[Instr.Src1],
          Ctx.IntRegs[Instr.Src2], Ctx.IntRegs[Instr.Dest], Instr.Immediate);
      end;
    15: // bcGraphicGList - GLIST
      begin
        // List SDL2 video modes - TODO: Implement actual mode listing via FOutputDevice
      end;
    16: // bcGraphicPos - POS(x)
      begin
        // Return cursor column position (0-indexed, consistent with TAB)
        // Use Ctx.CursorCol which is tracked by the VM during PRINT operations
        Ctx.IntRegs[Instr.Dest] := Ctx.CursorCol;
      end;
    17: // bcGraphicRclr - RCLR(n)
      if Assigned(FOutputDevice) then
      begin
        // Return color of source n
        Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetColorSourceDirect(Ctx.IntRegs[Instr.Src1]);
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    18: // bcGraphicRwindow - RWINDOW(n)
      if Assigned(FOutputDevice) then
      begin
        // Return window info: 0=lines, 1=cols, 2=screen width
        case Ctx.IntRegs[Instr.Src1] of
          0: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetWindowLines;
          1: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetWindowCols;
          2: Ctx.IntRegs[Instr.Dest] := FOutputDevice.GetScreenWidth;
        else
          Ctx.IntRegs[Instr.Dest] := 0;
        end;
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    19: // bcSetColor - SETCOLOR index, R, G, B [, A]
      if Assigned(FOutputDevice) then
      begin
        // Src1=index, Src2=R, Dest=G, Immediate: B(12) | A(12)
        FOutputDevice.SetPaletteColorRGBA(
          Ctx.IntRegs[Instr.Src1],                              // index
          Byte(Ctx.IntRegs[Instr.Src2]),                        // R
          Byte(Ctx.IntRegs[Instr.Dest]),                        // G
          Byte(Ctx.IntRegs[Instr.Immediate and $FFF]),          // B
          Byte(Ctx.IntRegs[(Instr.Immediate shr 12) and $FFF])  // A
        );
      end;
    20: // bcGetColor - GETCOLOR(index)
      // Returns RGBA value from palette at given index (0-255)
      if Assigned(FOutputDevice) then
        Ctx.IntRegs[Instr.Dest] := Int64(FOutputDevice.GetPaletteColor(Ctx.IntRegs[Instr.Src1]))
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    21: // bcScnClr - SCNCLR [mode]
      if Assigned(FOutputDevice) then
        FOutputDevice.ClearScreen(Ctx.IntRegs[Instr.Src1]);
    22: // bcPLoad - PLOAD "filename"
      if Assigned(FOutputDevice) then
      begin
        if not FOutputDevice.LoadPaletteFromJSON(Ctx.StringRegs[Instr.Src1]) then
        begin
          // Set error state for BASIC error handling
          Ctx.LastErrorMessage := FOutputDevice.GetLastPaletteError;
          Ctx.LastErrorCode := 100;  // Palette error code
          Ctx.LastErrorLine := FProgram.GetSourceLine(Ctx.PC);
        end;
      end;
    23: // bcPSave - PSAVE "filename"
      if Assigned(FOutputDevice) then
      begin
        if not FOutputDevice.SavePaletteToJSON(Ctx.StringRegs[Instr.Src1]) then
        begin
          // Set error state for BASIC error handling
          Ctx.LastErrorMessage := FOutputDevice.GetLastPaletteError;
          Ctx.LastErrorCode := 101;  // Palette save error code
          Ctx.LastErrorLine := FProgram.GetSourceLine(Ctx.PC);
        end;
      end;
    24: // bcPRst - PRST (reset palette to C64 default)
      if Assigned(FOutputDevice) then
        FOutputDevice.ResetPalette;
  else
    raise Exception.CreateFmt('Unknown graphics opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteSoundOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  {$IFDEF WITH_SEDAI_AUDIO}
  VoiceIdx: Integer;
  DurationMs: Integer;
  Dir, WaveformIdx, PulseWidthVal: Integer;
  MinFreq, SweepSpeed: Integer;
  CurrentFreq, StartFreq: Integer;
  Remaining, SleepStep: Integer;
  SweepUp: Boolean;
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
          FSIDEvo.SetMasterVolume(Ctx.IntRegs[Instr.Src1] / 15.0);
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
        VoiceIdx := Ctx.IntRegs[Instr.Src1] - 1;
        DurationMs := Ctx.IntRegs[Instr.Dest] * 1000 div 60;

        // Extract optional params from register indices in Immediate
        // Layout: dir(8) | minfreq(12) | sweeptime(12) | waveform(8) | pw(12)
        Dir := Ctx.IntRegs[(Instr.Immediate) and $FF];
        MinFreq := Ctx.IntRegs[(Instr.Immediate shr 8) and $FFF];
        SweepSpeed := Ctx.IntRegs[(Instr.Immediate shr 20) and $FFF];
        WaveformIdx := Ctx.IntRegs[(Instr.Immediate shr 32) and $FF];
        PulseWidthVal := Ctx.IntRegs[(Instr.Immediate shr 40) and $FFF];

        FAudioBackend.Lock;
        try
          // Reset envelope state machine to avoid the ADSR delay bug:
          // When Sustain=15 ($FF), rapid retrigger causes Inc($FF)->$00 wrap
          // which triggers HoldZero, permanently silencing the voice.
          FSIDEvo.ResetVoiceEnvelope(VoiceIdx);

          // Convert SID frequency to Hz: SID_value * PAL_clock / 16777216
          // Simplified: SID_value * 0.0596 (for PAL 985248 Hz clock)
          FSIDEvo.SetFrequencyHz(VoiceIdx, Ctx.IntRegs[Instr.Src2] * 0.0596);
          case WaveformIdx of
            0: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_TRIANGLE);
            1: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_SAWTOOTH);
            2: begin
                 FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_PULSE);
                 if PulseWidthVal > 0 then
                   FSIDEvo.SetPulseWidth(VoiceIdx, PulseWidthVal / 4095.0)
                 else
                   FSIDEvo.SetPulseWidth(VoiceIdx, 0.5);
               end;
            3: FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_NOISE);
          else
            FSIDEvo.SetWaveform(VoiceIdx, SIDEVO_WAVE_SAWTOOTH);
          end;
          // Default ADSR for SOUND: instant on, full sustain, instant off
          FSIDEvo.SetADSR(VoiceIdx, 0.0, 0.0, 1.0, 0.0);
          // Ensure full voice volume (PLAY may have changed it via Un)
          FSIDEvo.SetVoiceVolume(VoiceIdx, 1.0);
          FSIDEvo.GateOn(VoiceIdx);
        finally
          FAudioBackend.Unlock;
        end;

        // Flush pending display output before blocking on sound duration
        if Assigned(FInputDevice) then
          FInputDevice.ProcessEvents;
        if Assigned(FOutputDevice) then
          PresentFrame;

        // Wait for duration (outside lock to allow callback to run)
        if DurationMs > 0 then
        begin
          // Frequency sweep if sweep params are set
          if (SweepSpeed > 0) and (Dir in [0, 1, 2]) then
          begin
            StartFreq := Ctx.IntRegs[Instr.Src2];
            CurrentFreq := StartFreq;
            SweepUp := True;  // For oscillate mode
            Remaining := DurationMs;
            while Remaining > 0 do
            begin
              SleepStep := Remaining;
              if SleepStep > 16 then SleepStep := 16;  // ~1 jiffy per step
              CooperativeSleep(Ctx, SleepStep);
              Dec(Remaining, SleepStep);
              case Dir of
                0: begin // Sweep up
                     CurrentFreq := CurrentFreq + SweepSpeed;
                     if CurrentFreq > 65535 then CurrentFreq := 65535;
                   end;
                1: begin // Sweep down
                     CurrentFreq := CurrentFreq - SweepSpeed;
                     if CurrentFreq < 0 then CurrentFreq := 0;
                   end;
                2: begin // Oscillate between MinFreq and StartFreq
                     if SweepUp then
                     begin
                       CurrentFreq := CurrentFreq + SweepSpeed;
                       if CurrentFreq >= StartFreq then
                       begin
                         CurrentFreq := StartFreq;
                         SweepUp := False;
                       end;
                     end else begin
                       CurrentFreq := CurrentFreq - SweepSpeed;
                       if CurrentFreq <= MinFreq then
                       begin
                         CurrentFreq := MinFreq;
                         SweepUp := True;
                       end;
                     end;
                   end;
              end;
              FAudioBackend.Lock;
              try
                FSIDEvo.SetFrequencyHz(VoiceIdx, CurrentFreq * 0.0596);
              finally
                FAudioBackend.Unlock;
              end;
            end;
          end else
            CooperativeSleep(Ctx, DurationMs);

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
        if (Ctx.IntRegs[Instr.Src1] >= 0) and (Ctx.IntRegs[Instr.Src1] <= 9) then
        begin
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].Attack := Ctx.IntRegs[(Instr.Immediate) and $FF] / 15.0;
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].Decay := Ctx.IntRegs[(Instr.Immediate shr 8) and $FF] / 15.0;
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].Sustain := Ctx.IntRegs[(Instr.Immediate shr 16) and $FF] / 15.0;
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].Release := Ctx.IntRegs[(Instr.Immediate shr 24) and $FF] / 15.0;
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].Waveform := Ctx.IntRegs[(Instr.Immediate shr 32) and $FF];
          FAudioEnvelopes[Ctx.IntRegs[Instr.Src1]].PulseWidth := Ctx.IntRegs[(Instr.Immediate shr 40) and $FFF] / 4095.0;
        end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
    3: // bcSoundTempo
      {$IFDEF WITH_SEDAI_AUDIO}
      if FAudioInitialized then
      begin
        FAudioTempo := Ctx.IntRegs[Instr.Src1];
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
        WriteLn('[DEBUG_AUDIO] PLAY called, AudioInit=', FAudioInitialized, ' String="', Ctx.StringRegs[Instr.Src1], '"');
        {$ENDIF}
        if FAudioInitialized then
          ExecutePlayString(Ctx, Ctx.StringRegs[Instr.Src1]);
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
            Ctx.IntRegs[Instr.Src2] <> 0,                    // lowpass
            Ctx.IntRegs[Instr.Dest] <> 0,                    // bandpass
            Ctx.IntRegs[Instr.Immediate and $FF] <> 0       // highpass
          );
          // Set cutoff: convert Hz (0-20000) to 11-bit value (0-2047)
          FSIDEvo.SetFilterCutoff(Round(Ctx.FloatRegs[Instr.Src1] / 20000.0 * 2047));
          // Set resonance: 0-15 range
          FSIDEvo.SetFilterResonance(Ctx.IntRegs[(Instr.Immediate shr 8) and $FF] and $0F);
        finally
          FAudioBackend.Unlock;
        end;
      end;
      {$ELSE}
      ; // No audio support
      {$ENDIF}
  else
    raise Exception.CreateFmt('Unknown sound opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteSpriteOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  SpriteNum, Enabled, Priority, Mode, SprW, SprH: Integer;
  X, Y, ScaleX, ScaleY, Angle, Speed: Double;
  Color: Integer;
  SprColor, MC1Color, MC2Color: TSpriteColor;
  SaveStr: string;
begin
  { Group 7: Sprite operations (0x07xx) — delegated to ISpriteManager }

  // M5.3: off the render-owner thread, defer to the queue (see ExecuteGraphicsOp). Dormant on
  // the single-threaded path. NOTE for M5.2: sprite *query* ops (RSPRITE/BUMP/RSPPOS) return a
  // value into a register and so must run synchronously, not be deferred — to be split out then.
  if FHasWorkers and not IsRenderOwner then
  begin
    EnqueueDeferredOp(Ctx, dckSprite, Instr);
    Exit;
  end;

  SubOp := Instr.OpCode and $FF;

  case SubOp of
    0: // bcSprite
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;

        Enabled := 1;
        if Instr.Src2 <> 0 then
          Enabled := Round(Ctx.FloatRegs[Instr.Src2]);

        Color := 1;
        if Instr.Dest <> 0 then
          Color := Round(Ctx.FloatRegs[Instr.Dest]);

        Priority := 0;
        ScaleX := 1.0;
        ScaleY := 1.0;
        Mode := 0;

        if (Instr.Immediate and $FFF) <> 0 then
          Priority := Round(Ctx.FloatRegs[Instr.Immediate and $FFF]);
        if ((Instr.Immediate shr 12) and $FFF) <> 0 then
          ScaleX := Ctx.FloatRegs[(Instr.Immediate shr 12) and $FFF];
        if ((Instr.Immediate shr 24) and $FFF) <> 0 then
          ScaleY := Ctx.FloatRegs[(Instr.Immediate shr 24) and $FFF];
        if ((Instr.Immediate shr 36) and $FFF) <> 0 then
          Mode := Round(Ctx.FloatRegs[(Instr.Immediate shr 36) and $FFF]);

        if Assigned(FSpriteManager) then
        begin
          SprColor := MakeIndexedColor(Byte(Color));
          FSpriteManager.SetSprite(SpriteNum, Enabled, SprColor,
            Priority, ScaleX, ScaleY, Mode);
        end;
      end;

    1: // bcMovsprAbs
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        X := Ctx.FloatRegs[Instr.Src2];
        Y := Ctx.FloatRegs[Instr.Dest];
        if Assigned(FSpriteManager) then
          FSpriteManager.MoveSpriteAbs(SpriteNum, X, Y);
      end;

    2: // bcMovsprRel
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        X := Ctx.FloatRegs[Instr.Src2];
        Y := Ctx.FloatRegs[Instr.Dest];
        if Assigned(FSpriteManager) then
          FSpriteManager.MoveSpriteRel(SpriteNum, X, Y);
      end;

    3: // bcMovsprPolar
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        X := Ctx.FloatRegs[Instr.Src2];  // Distance
        Angle := Ctx.FloatRegs[Instr.Dest];
        if Assigned(FSpriteManager) then
          FSpriteManager.MoveSpritePolar(SpriteNum, X, Angle);
      end;

    4: // bcMovsprAuto
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum < 1) or (SpriteNum > 256) then Exit;
        Angle := Ctx.FloatRegs[Instr.Src2];
        Speed := Ctx.FloatRegs[Instr.Dest];
        if Assigned(FSpriteManager) then
          FSpriteManager.MoveSpriteAuto(SpriteNum, Angle, Speed);
      end;

    5: // bcSprcolor
      begin
        if Assigned(FSpriteManager) then
        begin
          if Instr.Src1 <> 0 then
            MC1Color := MakeIndexedColor(Byte(Round(Ctx.FloatRegs[Instr.Src1])))
          else
            MC1Color := MakeIndexedColor(255);  // 255 = keep current
          if Instr.Src2 <> 0 then
            MC2Color := MakeIndexedColor(Byte(Round(Ctx.FloatRegs[Instr.Src2])))
          else
            MC2Color := MakeIndexedColor(255);
          FSpriteManager.SetSpriteMulticolors(MC1Color, MC2Color);
        end;
      end;

    6: // bcSprsav
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if Assigned(FSpriteManager) then
        begin
          if (SpriteNum >= 1) and (SpriteNum <= 256) then
          begin
            FSpriteManager.SaveSpriteToString(SpriteNum, SaveStr);
            Ctx.StringRegs[Instr.Src2] := SaveStr;
          end;
        end;
      end;

    7: // bcCollision
      begin
        if Assigned(FSpriteManager) then
          FSpriteManager.SetCollisionHandler(
            Round(Ctx.FloatRegs[Instr.Src1]),
            Round(Ctx.FloatRegs[Instr.Src2]));
      end;

    8: // bcBump
      begin
        if Assigned(FSpriteManager) then
          Ctx.FloatRegs[Instr.Dest] := FSpriteManager.GetCollisionStatus(
            Ctx.IntRegs[Instr.Src1])
        else
          Ctx.FloatRegs[Instr.Dest] := 0;
      end;

    9: // bcRspcolor
      begin
        if Assigned(FSpriteManager) then
          Ctx.FloatRegs[Instr.Dest] := SpriteColorToInt(
            FSpriteManager.GetMulticolor(Ctx.IntRegs[Instr.Src1]))
        else
          Ctx.FloatRegs[Instr.Dest] := 0;
      end;

    10: // bcRsppos
      begin
        if Assigned(FSpriteManager) then
          Ctx.FloatRegs[Instr.Dest] := FSpriteManager.GetSpritePosition(
            Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2])
        else
          Ctx.FloatRegs[Instr.Dest] := 0;
      end;

    11: // bcRsprite
      begin
        if Assigned(FSpriteManager) then
          Ctx.FloatRegs[Instr.Dest] := FSpriteManager.GetSpriteAttribute(
            Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2])
        else
          Ctx.FloatRegs[Instr.Dest] := 0;
      end;

    12: // bcSpriteDef - SPRDEF [n]: enter the interactive sprite editor (sbv)
      begin
        // The editor is a modal console operation, so it is provided as a callback
        // (set by the SDL console); other front-ends leave it nil = no-op.
        if Assigned(FSpriteEditorCallback) then
          if FSpriteEditorCallback(Round(Ctx.FloatRegs[Instr.Src1])) then
            Ctx.Running := False;  // editor requested quit (window closed)
      end;

    13: // bcSprSaveFile - SPRSAVE "file": save all sprites to a JSON file
      if Assigned(FSpriteManager) then
        FSpriteManager.SaveSpritesToJSON(Ctx.StringRegs[Instr.Src1]);

    14: // bcSprLoadFile - SPRLOAD "file" [,usefilecolors]: load sprites from JSON
      if Assigned(FSpriteManager) then
        // Src2 = "use file colours" flag (int reg, 0 by default).
        FSpriteManager.LoadSpritesFromJSON(Ctx.StringRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2] <> 0);

    15: // bcSprSize - SPRSIZE n, w, h (Src1=n, Src2=w, Dest=h; float regs)
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum >= 1) and (SpriteNum <= 256) and Assigned(FSpriteManager) then
        begin
          SprW := Round(Ctx.FloatRegs[Instr.Src2]);
          SprH := Round(Ctx.FloatRegs[Instr.Dest]);
          if SprW < 1 then SprW := 1 else if SprW > 256 then SprW := 256;
          if SprH < 1 then SprH := 1 else if SprH > 256 then SprH := 256;
          FSpriteManager.SetSpriteSize(SpriteNum, SprW, SprH);
        end;
      end;

    16: // bcSprForm - SPRFORM n, format (Src1=n, Src2=format; float regs)
      begin
        SpriteNum := Round(Ctx.FloatRegs[Instr.Src1]);
        if (SpriteNum >= 1) and (SpriteNum <= 256) and Assigned(FSpriteManager) then
          FSpriteManager.SetSpriteFormat(SpriteNum, Round(Ctx.FloatRegs[Instr.Src2]));
      end;

  else
    raise Exception.CreateFmt('Unknown sprite opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.ExecuteFileIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
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
      8 = APPEND #handle, data
      9 = DCLEAR
      10 = RECORD #handle, position

    Register encoding (handle in Src1, not Dest, to avoid SSA versioning issues):
      DOPEN: Src1 = handle register (int), Src2 = filename register (string),
             Immediate = mode register (string, optional)
      DCLOSE: Src1 = handle register (int)
      APPEND: Src1 = handle register (int), Src2 = data register (string)
      RECORD: Src1 = handle register (int), Src2 = position register (int)
  }

  SubOp := Instr.OpCode and $FF;
  ErrorCode := 0;

  case SubOp of
    0, 2: // bcDopen, bcOpen
      begin
        // DOPEN #handle, "filename" [, mode$]
        // Src1 = handle, Src2 = filename, Immediate = mode register (or 0)
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Filename := Ctx.StringRegs[Instr.Src2];

        // Mode is optional, default to "R" (read)
        if Instr.Immediate > 0 then
          Mode := Ctx.StringRegs[Instr.Immediate]
        else
          Mode := 'R';

        // Named handles not currently used, clear handle name
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
        // Src1 = handle
        HandleNum := Ctx.IntRegs[Instr.Src1];
        HandleName := '';

        if Assigned(FOnDiskFile) then
        begin
          FOnDiskFile(Self, 'DCLOSE', HandleNum, HandleName, '', '', ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('DCLOSE error %d closing handle: %d', [ErrorCode, HandleNum]);
          // Reset CMD redirection if closing the CMD output file
          if FCmdHandle = HandleNum then
            FCmdHandle := 0;
        end
        else
          raise Exception.Create('DCLOSE command not supported: no handler assigned');
      end;

    4: // bcGetFile - GET# file, var
      begin
        { GET# file, var - Read one character from file
          Dest = variable register index to store result (string)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'GET#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('GET# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Store result in string register
          if Instr.Dest >= 0 then
            Ctx.StringRegs[Instr.Dest] := Data;
        end
        else
          raise Exception.Create('GET# command not supported: no handler assigned');
      end;

    5: // bcInputFile - INPUT# file, vars
      begin
        { INPUT# file, var - Read data from file
          Dest = variable register index to store result
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'INPUT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('INPUT# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Store result in string register
          if Instr.Dest >= 0 then
            Ctx.StringRegs[Instr.Dest] := Data;
        end
        else
          raise Exception.Create('INPUT# command not supported: no handler assigned');
      end;

    6: // bcPrintFile - PRINT# file, exprs
      begin
        { PRINT# file, data - Write data to file
          Dest = data register (expression to print)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        // Data can be in Dest (float converted to string, or string directly)
        // Need to handle different register types
        if Instr.Dest >= 0 then
          Data := Ctx.StringRegs[Instr.Dest]
        else
          Data := '';
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
          Src1 = file handle register (int)
          When handle is 0, output returns to screen }
        HandleNum := Ctx.IntRegs[Instr.Src1];

        // Set output redirection
        if HandleNum = 0 then
          FCmdHandle := 0  // Reset to screen
        else
          FCmdHandle := HandleNum;  // Redirect output to this file
      end;

    8: // bcAppend - APPEND #handle, data
      begin
        { APPEND #handle, data - Append string data to open file
          Src1 = file handle register (int)
          Src2 = data string register }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Instr.Src2 >= 0 then
          Data := Ctx.StringRegs[Instr.Src2]
        else
          Data := '';

        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'APPEND', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('APPEND error %d writing to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('APPEND command not supported: no handler assigned');
      end;

    9: // bcDclear - DCLEAR (close all file handles)
      begin
        { DCLEAR - Close all open file handles
          No parameters }
        if Assigned(FOnDiskFile) then
        begin
          // Use handle 0 as signal to close all handles
          FOnDiskFile(Self, 'DCLEAR', 0, '', '', '', ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('DCLEAR error %d', [ErrorCode]);
        end
        else
          raise Exception.Create('DCLEAR command not supported: no handler assigned');
      end;

    10: // bcRecord - RECORD #handle, position
      begin
        { RECORD #handle, position - Seek to byte position in file
          Src1 = file handle register (int)
          Src2 = position register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Instr.Src2 >= 0 then
          Data := IntToStr(Ctx.IntRegs[Instr.Src2])  // Pass position as string
        else
          Data := '0';

        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'RECORD', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('RECORD error %d seeking in file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('RECORD command not supported: no handler assigned');
      end;

    11: // bcPrintFileNewLine - Write CR (newline) to file
      begin
        { PRINT# newline - Write CHR$(13) to file
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := #13;  // Carriage return (C128 BASIC behavior)
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# newline error %d writing to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('PRINT# newline not supported: no handler assigned');
      end;

    12: // bcPrintFileFloat - PRINT# file, float expr
      begin
        { PRINT# file, float - Write float value to file
          Dest = float register (value to print)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Dest]);
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# error %d writing float to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('PRINT# command not supported: no handler assigned');
      end;

    13: // bcPrintFileInt - PRINT# file, int expr
      begin
        { PRINT# file, int - Write integer value to file
          Dest = int register (value to print)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := FConsoleBehavior.FormatNumber(Ctx.IntRegs[Instr.Dest]);
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# error %d writing int to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('PRINT# command not supported: no handler assigned');
      end;

    14: // bcInputFileFloat - INPUT# file, float var
      begin
        { INPUT# file, float - Read float value from file
          Dest = float register (variable to store result)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'INPUT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('INPUT# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Convert string to float and store in float register
          if Instr.Dest >= 0 then
            Ctx.FloatRegs[Instr.Dest] := StrToFloatDef(Trim(Data), 0.0);
        end
        else
          raise Exception.Create('INPUT# command not supported: no handler assigned');
      end;

    15: // bcInputFileInt - INPUT# file, int var
      begin
        { INPUT# file, int - Read integer value from file
          Dest = int register (variable to store result)
          Src1 = file handle register (int) }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'INPUT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('INPUT# error %d reading from file: %d', [ErrorCode, HandleNum]);
          // Convert string to integer and store in int register
          if Instr.Dest >= 0 then
            Ctx.IntRegs[Instr.Dest] := StrToIntDef(Trim(Data), 0);
        end
        else
          raise Exception.Create('INPUT# command not supported: no handler assigned');
      end;

  else
    raise Exception.CreateFmt('Unknown file I/O opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
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

procedure TBytecodeVM.ExecuteScratch(const Pattern: string; Force: Boolean; Silent: Boolean);
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
    // Only raise error if not Silent
    if not Silent then
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
  if not FCtx.Stopped then
    raise Exception.Create('?CAN''T CONTINUE ERROR');
  if FProgram = nil then
    raise Exception.Create('?CAN''T CONTINUE ERROR');
  if FCtx.StoppedPC >= FProgram.GetInstructionCount then
    raise Exception.Create('?CAN''T CONTINUE ERROR');

  // Resume execution from saved position
  FCtx.PC := FCtx.StoppedPC;
  FCtx.Running := True;
  FCtx.Stopped := False;

  // Continue the execution loop
  while FCtx.Running and (FCtx.PC < FProgram.GetInstructionCount) do Step;

  // Reset FAST mode when program ends
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode(False);
end;

{$IFDEF WEB_MODE}
procedure TBytecodeVM.SetWebContext(AContext: TObject);
begin
  FWebContext := AContext;
end;

procedure TBytecodeVM.ExecuteWebOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
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
        ParamName := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := WebCtx.GetParam(ParamName);
      end;

    $02: // bcWebPostParam - POST$("name")
      begin
        ParamName := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := WebCtx.PostParam(ParamName);
      end;

    $03: // bcWebGetRaw - GETRAW$("name")
      begin
        ParamName := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := WebCtx.GetParamRaw(ParamName);
      end;

    $04: // bcWebPostRaw - POSTRAW$("name")
      begin
        ParamName := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := WebCtx.PostParamRaw(ParamName);
      end;

    $05: // bcWebHtmlEncode - HTML$(s)
      begin
        Value := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := HtmlEncode(Value);
      end;

    $06: // bcWebUrlEncode - URL$(s)
      begin
        Value := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := UrlEncode(Value);
      end;

    $07: // bcWebMethod - METHOD$
      begin
        Ctx.StringRegs[Instr.Dest] := WebCtx.Method;
      end;

    $08: // bcWebPath - PATH$
      begin
        Ctx.StringRegs[Instr.Dest] := WebCtx.Path;
      end;

    $09: // bcWebQuery - QUERY$
      begin
        Ctx.StringRegs[Instr.Dest] := WebCtx.QueryString;
      end;

    $0A: // bcWebHeader - HEADER$("name")
      begin
        ParamName := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Dest] := WebCtx.GetHeader(ParamName);
      end;

    $0B: // bcWebSetHeader - SETHEADER name, value
      begin
        ParamName := Ctx.StringRegs[Instr.Src1];
        Value := Ctx.StringRegs[Instr.Src2];
        WebCtx.SetResponseHeader(ParamName, Value);
      end;

    $0C: // bcWebStatus - STATUS code
      begin
        WebCtx.ResponseStatus := Ctx.IntRegs[Instr.Src1];
      end;
  else
    raise Exception.CreateFmt('Unknown web opcode $%x at PC=%d', [SubOp, Ctx.PC]);
  end;
end;
{$ENDIF}

end.
