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
  Classes, SysUtils, Math, Variants, StrUtils, DateUtils,
  SedaiBytecodeTypes, SedaiOutputInterface, SedaiSSATypes,
  SedaiConsoleBehavior, SedaiDebugger, SedaiExecutorErrors,
  SedaiMemoryMapper, SedaiSpriteTypes, SedaiExecutionContext, SedaiDrawQueue,
  SedaiGraphicsBackend, SedaiInputState
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
    FSharedRecFreeList: array of Integer;  // DELETE: indices of freed shared records, reused by NEW
    FSharedRecFreeCount: Integer;
    FSharedRecLock: TRTLCriticalSection;
    // FreeBASIC raw memory (Allocate/Deallocate/...): a VM-internal byte heap. A raw pointer is a byte
    // OFFSET into FRawHeap (tagged with RAWPTR_TAG so it is distinct from managed FArrays/record
    // pointers). Each block carries an 8-byte size header just below the returned offset; freed blocks
    // go on a first-fit free list. VM-managed (not OS addresses) → memory-safe and portable. Guarded by
    // FRawHeapLock for cross-thread Allocate/Free.
    FRawHeap: array of Byte;
    FRawHeapTop: PtrUInt;                       // bump pointer (next free byte)
    FRawFreeOfs: array of PtrUInt;              // free-list block data offsets
    FRawFreeSz: array of PtrUInt;               // matching block payload sizes
    FRawFreeCount: Integer;
    FRawHeapLock: TRTLCriticalSection;
    FProgram: TBytecodeProgram;
    FOutputDevice: IOutputDevice;
    FGraphics: IGraphicsBackend;     // FreeBASIC graphics phase: operation-level drawing backend (SW headless / SDL2 on sbv)
    FOwnedGraphics: TObject;         // concrete backend object the VM owns and frees (e.g. the software backend on sb)
    FGfxForeColor: UInt32;           // current FreeBASIC draw foreground (COLOR fg); omitted-colour default
    FGfxBackColor: UInt32;           // current FreeBASIC draw background (COLOR ,bg)
    FGfxWorkSurface: Integer;        // FreeBASIC page flipping: cached surface all draw ops target (= FGfxPages[FGfxWorkPage])
    FGfxWorkPage: Integer;           // current work page index (drawing target)
    FGfxVisiblePage: Integer;        // current visible page index (shown on screen; sbv)
    FGfxPages: array of Integer;     // page index -> surface id (page 0 = screen surface 0; 1+ = image surfaces)
    // FreeBASIC WINDOW logical coordinate system: physical = A*logical + B (per axis). Identity when off.
    FGfxWinActive: Boolean;
    FGfxWinAx, FGfxWinBx, FGfxWinAy, FGfxWinBy: Double;
    // FreeBASIC VIEW viewport: physical origin added to mapped coords (non-SCREEN form); clip is on the surface.
    FGfxViewOffsetX, FGfxViewOffsetY: Integer;
    // FreeBASIC GETMOUSE snapshot cache: bcGetmouse queries the input provider once and stores the state
    // here; bcMouseAxis(which) then reads the requested component (0=x,1=y,2=wheel,3=buttons,4=clip).
    FMouseX, FMouseY, FMouseWheel, FMouseButtons, FMouseClip: Integer;
    // FreeBASIC GETJOYSTICK snapshot cache: bcGetJoystick queries the provider once and stores the state
    // here; bcJoyBtn reads the button bitmask, bcJoyAxis(which) reads axis `which` (0..7).
    FJoyButtons: Integer;
    FJoyAxes: array[0..7] of Single;
    FProgramArgs: array of string;   // COMMAND$: arguments passed to the BASIC program (arg 1, 2, ...)
    FIOStatus: Integer;   // ST (Commodore): Kernal I/O status byte; bit 6 (64) = EOF on the last GET#
    FInputDevice: IInputDevice;
    FMemoryMapper: IMemoryMapper;  // Memory-mapped PEEK/POKE support
    FConsoleBehavior: TConsoleBehavior;
    FOwnsConsoleBehavior: Boolean;
    // Time tracking for TI and TI$
    FStartTicks: QWord;     // Milliseconds since system start when VM started
    FTimeOffset: Int64;     // TI$ offset in milliseconds from real time
    FClockOffsetDays: Double; // FreeBASIC SETDATE/SETTIME offset (days) applied to NOW/DATE/TIME/TIMER
    FEnvOverrides: TStringList; // SETENVIRON "NAME=value" overrides, consulted by ENVIRON$ before the OS environment
    FDrawPenX, FDrawPenY: Integer; // FreeBASIC DRAW "..." (GML) pen position, in logical (WINDOW) coordinates; read by POINTCOORD
    FLastFrameTick: QWord;  // Last FRAME sync tick for drift-free timing
    // Function key definitions (1-12)
    FFunctionKeys: array[1..12] of string;
    FVarMap: TStringList;
    FArrays: array of TArrayStorage;
    // Array BYREF parameter binding (MODERN): a save-stack for bcArrayBind/bcArrayUnbind. Binding
    // aliases a callee param-array slot to the caller's array (sharing the element data); the saved
    // original is restored on unbind. A stack so recursion / re-entrancy nest correctly. ArgId is the
    // caller's array slot, kept so a REDIM [PRESERVE] inside the callee (which reallocates the param's
    // storage and thereby breaks the shared reference) is propagated back to the caller on unbind.
    FArrayBindStack: array of record SlotId: Integer; ArgId: Integer; Saved: TArrayStorage; Snapshot: TArrayStorage; end;
    FArrayBindTop: Integer;
    FRedimPendingUBs: array of Integer;   // REDIM multi-dim: upper bounds accumulated by bcArrayRedimPush, consumed by bcArrayRedimN
    FIdxPending: array of Int64;          // runtime multi-dim index: indices accumulated by bcArrayIdxPush, consumed by bcArrayIdxResolve

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
    FBoundsCheck: Boolean;        // True = always raise on out-of-bounds array access (even in MODERN). Default False:
                                  // MODERN follows FreeBASIC (no bounds check -> default read / ignored write), CLASSIC always checks.
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
    function GfxMapX(LX: Double): Integer;   // FreeBASIC WINDOW: logical x -> physical x
    function GfxMapY(LY: Double): Integer;   // FreeBASIC WINDOW: logical y -> physical y
    procedure SetupGfxScreen(W, H, NumPages: Integer);  // SCREENRES/SCREEN: resize + (re)build pages
    // Group-specific dispatch handlers
    procedure ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    // Dialect-aware bounds test for a flat element index. Returns True when in range. Out of bounds:
    // CLASSIC (Commodore ?BAD SUBSCRIPT) or an explicit --bounds-check raises; MODERN (FreeBASIC, which
    // does not bounds-check) returns False so the caller yields a default on read / skips the write.
    function ArrayBoundsOK(ArrayIdx, LinearIdx: Integer): Boolean;
    procedure EraseArray(ArrayIdx: Integer);                                   // B1.4: ERASE
    procedure RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean; HasNewLower: Boolean = False; NewLower: Integer = 0);  // B1.4: REDIM (1-D)
    procedure RedimArrayN(ArrayIdx: Integer; const Uppers: array of Integer; Preserve: Boolean); // REDIM multi-dim

    procedure ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpecialVarOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSoundOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpriteOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteFileIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$IFDEF WEB_MODE}
    procedure ExecuteWebOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$ENDIF}
    // Raise a dialect-aware filesystem runtime error: FreeBASIC error number + message in MODERN,
    // Commodore error number + '?...' message in CLASSIC. The code reaches ERR via the except handler.
    procedure RaiseFileError(const FBMsg: string; FBCode: Integer; const CBMMsg: string; CBMCode: Integer);
    // FreeBASIC resets Err/Erl after RESUME / RESUME NEXT; Commodore keeps EL/ER. Reset only in MODERN.
    procedure ResetErrorStateIfModern(Ctx: TExecutionContext);
    // Dialect-aware float division by (near-)zero. FreeBASIC (MODERN) follows IEEE-754: x/0 -> +/-Inf,
    // 0/0 -> NaN. Commodore BASIC (CLASSIC) raises ?DIVISION BY ZERO ERROR. Given the numerator, returns
    // the IEEE result in MODERN or raises EZeroDivide in CLASSIC. Used at every float-div-by-zero site.
    function DivZeroFloat(Numerator: Double): Double;
    // File management operations (executed directly in VM)
    procedure ExecuteCopyFile(const Src, Dest: string; Overwrite: Boolean);
    procedure ExecuteScratch(const Pattern: string; Force: Boolean; Silent: Boolean = False);
    procedure ExecuteRenameFile(const OldName, NewName: string);
    procedure ExecuteConcat(const Src, Dest: string);
    procedure ExecuteMkdir(const Path: string);
    procedure SetEnvOverride(const NameValue: string);   // SETENVIRON: record a "NAME=value" override
    function RunShellCommand(const Cmd: string): Integer; // SHELL: run a command via the platform shell, return exit code
    procedure DrawGML(const S: string);                  // DRAW "...": interpret the FreeBASIC graphics macro language
    procedure ExecuteChdir(const Path: string);
    procedure ExecuteRmdir(const Path: string);
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
    procedure FreeSharedRecord(Handle: Int64);   // DELETE: release a shared record, recycle its slot
    // FreeBASIC raw byte heap (Allocate family). All return/take RAWPTR_TAG-tagged byte offsets.
    function RawAlloc(ByteCount: PtrUInt): Int64;
    function StrSAdd(const S: string): Int64;   // SADD(s) -> raw pointer to a NUL-terminated byte copy
    function FormatNumber(Value: Double; const Mask: string): string;  // FORMAT(num, mask) -> formatted string (numeric masks)
    function FormatDateMask(Value: Double; const Mask: string): string;  // FORMAT(serial, mask) -> date/time formatted string
    function CommandLine(Index: Integer): string;  // COMMAND$(index) -> command-line argument(s)
    function DiskStatusString: string;  // DS$ -> Commodore disk status line "NN, MESSAGE,00,00"
    function FileLength(const Path: string): Int64;   // FILELEN(path) -> file size in bytes (0 if absent)
    function FileDateTimeSerial(const Path: string): Double;  // FILEDATETIME(path) -> last-modified date serial (0 if absent)
    procedure RawFree(RawPtr: Int64);
    function RawRealloc(RawPtr: Int64; ByteCount: PtrUInt): Int64;
    function RawLoadInt(RawPtr: Int64; TypeCode: Integer): Int64;
    function RawLoadFloat(RawPtr: Int64; TypeCode: Integer): Double;
    procedure RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
    procedure RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
    procedure RawMemCopy(DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);  // FB_MEMCOPY/FB_MEMMOVE: copy ByteCount bytes on the raw heap
    procedure RawClear(DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);  // CLEAR: set ByteCount bytes to Value on the raw heap
    function ResolveRec(Ctx: TExecutionContext; Handle: Int64): PRecordStorage; inline;
    function RecPtrTarget(Ctx: TExecutionContext; PtrAddr: Int64; out Slot: Integer): PRecordStorage; inline;  // decode @obj.field pointer
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
    // FreeBASIC graphics backend. OwnedObj (optional) is the concrete object the VM should free on
    // destruction (used for the software backend on sb; pass nil for the SDL2 device owned elsewhere).
    procedure SetGraphicsBackend(Backend: IGraphicsBackend; OwnedObj: TObject = nil);
    procedure UseSoftwareGraphics;  // attach a VM-owned headless software graphics backend (CLI / bare-metal)
    procedure SetInputDevice(Device: IInputDevice);
    // Command-line arguments passed to the BASIC program (for COMMAND$): Args are the arguments only
    // (arg 1, 2, ...), excluding the interpreter/script name. Empty by default.
    procedure SetProgramArgs(const Args: array of string);
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
    // Array bounds checking: when True, every out-of-bounds access raises (a debugging aid, akin to FB's
    // -exx). Default False -> MODERN skips the check like FreeBASIC; CLASSIC always checks regardless.
    property BoundsCheck: Boolean read FBoundsCheck write FBoundsCheck;
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

// B1.5 type-width narrowing: wrap/sign-extend an Int64 to a narrower integer width.
// Width codes: 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32. Anything else is a full-width no-op
// (s64/u64 need no bit change; unsigned-64 semantics live in compare/div/print, not here).
function NarrowInt64(Value: Int64; WidthCode: Int64): Int64;
begin
  case WidthCode of
    1: Result := Int64(ShortInt(Value and $FF));         // s8
    2: Result := Value and $FF;                           // u8
    3: Result := Int64(SmallInt(Value and $FFFF));        // s16
    4: Result := Value and $FFFF;                          // u16
    5: Result := Int64(LongInt(Value and $FFFFFFFF));     // s32
    6: Result := Value and $FFFFFFFF;                      // u32
  else
    Result := Value;
  end;
end;

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

procedure SAFAudioCallback(AOutput: PSingle; AFrameCount: Integer; AUserData: Pointer);
var
  I: Integer;
  Sample: Single;
begin
  Inc(GCallbackCount);

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
    // No extra amplification: SIDEvo output is already at proper level
    // once master volume ($D418) is set; clamp only to guard against overflow.
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
  FEnvOverrides := TStringList.Create;
  FEnvOverrides.CaseSensitive := False;   // environment names are case-insensitive on Windows; harmless elsewhere
  // FreeBASIC draw colours: white foreground, opaque-black background (match the SCREENRES surface clear).
  FGfxForeColor := $FFFFFFFF;
  FGfxBackColor := $000000FF;
  // FreeBASIC page flipping: single page (the screen) until SCREENRES requests more.
  FGfxWorkSurface := GFX_SCREEN_SURFACE;
  FGfxWorkPage := 0;
  FGfxVisiblePage := 0;
  SetLength(FGfxPages, 1);
  FGfxPages[0] := GFX_SCREEN_SURFACE;
  FGfxWinActive := False;   // WINDOW logical coords off -> identity mapping
  // GETMOUSE cache: no snapshot taken yet -> report "no mouse" (-1) until the first bcGetmouse.
  FMouseX := -1; FMouseY := -1; FMouseWheel := 0; FMouseButtons := 0; FMouseClip := 0;
  // GETJOYSTICK cache: no snapshot yet. FJoyAxes is filled wholesale by bcGetJoystick before any bcJoyAxis
  // read (__JOYAXIS is only emitted inside GETJOYSTICK, after the snapshot), so it needs no init here.
  FJoyButtons := 0;
  FIOStatus := 0;   // ST: no I/O yet -> clear (no EOF)
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
  InitCriticalSection(FRawHeapLock);
  FRawHeapTop := 0;
  FRawFreeCount := 0;
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
  FCtx.CursorRow := 0;
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
  FEnvOverrides.Free;
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
  if Assigned(FOwnedGraphics) then
    FreeAndNil(FOwnedGraphics);   // free a VM-owned graphics backend (e.g. the software backend on sb)
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
  SetLength(FRawHeap, 0);
  DoneCriticalSection(FRawHeapLock);
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
    // Reuse a slot freed by DELETE if one is available, else append.
    if FSharedRecFreeCount > 0 then
    begin
      Dec(FSharedRecFreeCount);
      Idx := FSharedRecFreeList[FSharedRecFreeCount];
    end
    else
    begin
      Idx := FSharedRecordCount;
      if Idx >= Length(FSharedRecords) then
        SetLength(FSharedRecords, (Idx + 1) * 2);
      Inc(FSharedRecordCount);
    end;
    FSharedRecords[Idx] := R;
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
  Result := SHARED_REC_FLAG or Int64(Idx);
end;

procedure TBytecodeVM.FreeSharedRecord(Handle: Int64);
// DELETE: release a shared-region record and recycle its slot. A non-shared (per-thread) handle is
// ignored — those records are reclaimed by frame unwinding. Double-free / use-after-free are the
// programmer's responsibility (as in FreeBASIC).
var
  Idx: Integer;
begin
  if (Handle and SHARED_REC_FLAG) = 0 then Exit;
  Idx := Handle and SHARED_REC_MASK;
  EnterCriticalSection(FSharedRecLock);
  try
    if (Idx < 0) or (Idx >= FSharedRecordCount) or (FSharedRecords[Idx] = nil) then Exit;
    Dispose(FSharedRecords[Idx]);   // finalizes the record's managed fields (strings/arrays)
    FSharedRecords[Idx] := nil;
    if FSharedRecFreeCount >= Length(FSharedRecFreeList) then
      SetLength(FSharedRecFreeList, (FSharedRecFreeCount + 1) * 2);
    FSharedRecFreeList[FSharedRecFreeCount] := Idx;
    Inc(FSharedRecFreeCount);
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
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

function TBytecodeVM.RecPtrTarget(Ctx: TExecutionContext; PtrAddr: Int64; out Slot: Integer): PRecordStorage;
// Decode a record-field pointer (RECPTR_TAG set): recover the record handle (index + shared flag) and
// the field slot, then route the handle to its storage. See SedaiSSATypes for the bit layout.
var
  Handle: Int64;
begin
  Slot := PtrAddr and RECPTR_SLOT_MASK;
  Handle := (PtrAddr shr RECPTR_SLOT_BITS) and RECPTR_INDEX_MASK;
  if (PtrAddr and SHARED_REC_FLAG) <> 0 then Handle := Handle or SHARED_REC_FLAG;
  Result := ResolveRec(Ctx, Handle);
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
  SetLength(FSharedRecFreeList, 0);
  FSharedRecFreeCount := 0;
end;

// ===== FreeBASIC raw byte heap =====
// Block layout: [8-byte payload-size header][payload...]. The raw pointer (RAWPTR_TAG | dataOffset)
// points at the payload; the header just below it lets Free/Realloc recover the size. dataOffset is
// always >= 8, so a valid raw pointer is never 0 (NULL). Allocations are 8-byte aligned for safe typed
// access. A first-fit free list recycles exact-or-larger freed blocks; otherwise the bump pointer grows.

function TBytecodeVM.RawAlloc(ByteCount: PtrUInt): Int64;
var
  i, best: Integer;
  dataOfs, need: PtrUInt;
begin
  if ByteCount = 0 then ByteCount := 1;
  ByteCount := (ByteCount + 7) and not PtrUInt(7);   // round payload up to 8
  EnterCriticalSection(FRawHeapLock);
  try
    // first-fit reuse
    best := -1;
    for i := 0 to FRawFreeCount - 1 do
      if FRawFreeSz[i] >= ByteCount then begin best := i; Break; end;
    if best >= 0 then
    begin
      dataOfs := FRawFreeOfs[best];
      // remove from the free list (swap with last)
      FRawFreeOfs[best] := FRawFreeOfs[FRawFreeCount - 1];
      FRawFreeSz[best] := FRawFreeSz[FRawFreeCount - 1];
      Dec(FRawFreeCount);
      // keep the recorded size (block stays its original size); header already holds it
    end
    else
    begin
      if FRawHeapTop = 0 then FRawHeapTop := 8;        // reserve offset 0 region (NULL)
      need := FRawHeapTop + 8 + ByteCount;
      if need > PtrUInt(Length(FRawHeap)) then
        SetLength(FRawHeap, (need + need div 2) + 4096);
      dataOfs := FRawHeapTop + 8;
      PtrUInt((@FRawHeap[dataOfs - 8])^) := ByteCount; // size header
      FRawHeapTop := dataOfs + ByteCount;
    end;
    FillChar(FRawHeap[dataOfs], PtrUInt((@FRawHeap[dataOfs - 8])^), 0);  // zero the payload
  finally
    LeaveCriticalSection(FRawHeapLock);
  end;
  Result := RAWPTR_TAG or Int64(dataOfs);
end;

function TBytecodeVM.StrSAdd(const S: string): Int64;
// FreeBASIC SADD: a raw byte-heap pointer to a NUL-terminated COPY of the string's bytes. A read-only
// snapshot — writes through it do not propagate back to the managed string (the managed string model has
// no stable mutable buffer address). Suitable for reading the bytes / passing a ZSTRING pointer.
var
  ofs: PtrUInt;
  i: Integer;
begin
  Result := RawAlloc(PtrUInt(Length(S)) + 1);
  if (Result and RAWPTR_TAG) = 0 then Exit;
  ofs := PtrUInt(Result and RAWPTR_OFS_MASK);
  for i := 1 to Length(S) do
    FRawHeap[ofs + PtrUInt(i) - 1] := Byte(Ord(S[i]));
  FRawHeap[ofs + PtrUInt(Length(S))] := 0;   // NUL terminator (ZSTRING)
end;

// FreeBASIC FORMAT(number, mask): format a Double per a VB/FB-style picture string. v1 covers NUMERIC
// masks — digit placeholders '0' (required) and '#' (optional), '.' decimal point, ',' thousands
// grouping, '%' percent (x100), scientific 'E+'/'E-'/'e+'/'e-', and literal characters (also '\x' and
// "..."). Date/time masks (d/m/y/h/s) are not yet handled. An empty mask yields a general format.
function TBytecodeVM.FormatNumber(Value: Double; const Mask: string): string;
var
  M: string;
  pctCount, i, ePos: Integer;
  hasNum, hasDate: Boolean;

  function ProcLiteral(const S: string): string;
  var k: Integer; ch: Char;
  begin
    Result := '';
    k := 1;
    while k <= Length(S) do
    begin
      ch := S[k];
      if (ch = '\') and (k < Length(S)) then begin Result := Result + S[k+1]; Inc(k, 2); end
      else if ch = '"' then
      begin
        Inc(k);
        while (k <= Length(S)) and (S[k] <> '"') do begin Result := Result + S[k]; Inc(k); end;
        Inc(k);   // skip closing quote
      end
      else begin Result := Result + ch; Inc(k); end;
    end;
  end;

  function Grouped(const Digits: string): string;
  var k, c: Integer;
  begin
    Result := '';
    c := 0;
    for k := Length(Digits) downto 1 do
    begin
      Result := Digits[k] + Result;
      Inc(c);
      if (c mod 3 = 0) and (k > 1) then Result := ',' + Result;
    end;
  end;

  function FixedPoint(V: Double; const FM: string): string;
  var
    neg, grouping: Boolean;
    dotPos, fracPH, reqZeros, firstCore, lastCore, j: Integer;
    absV: Double;
    Z, intDigits, fracDigits, intMask, fracMask, intOut, fracOut, prefix, suffix: string;
    scaled: Int64;
  begin
    neg := V < 0;
    absV := Abs(V);
    dotPos := Pos('.', FM);
    firstCore := 0; lastCore := 0;
    for j := 1 to Length(FM) do
      if (FM[j] = '0') or (FM[j] = '#') then
      begin
        if firstCore = 0 then firstCore := j;
        lastCore := j;
      end;
    if firstCore = 0 then Exit(ProcLiteral(FM));   // no placeholders: pure literal
    prefix := ProcLiteral(Copy(FM, 1, firstCore - 1));
    suffix := ProcLiteral(Copy(FM, lastCore + 1, MaxInt));
    if (dotPos > firstCore) and (dotPos < lastCore) then
    begin
      intMask := Copy(FM, firstCore, dotPos - firstCore);
      fracMask := Copy(FM, dotPos + 1, lastCore - dotPos);
    end
    else
    begin
      intMask := Copy(FM, firstCore, lastCore - firstCore + 1);
      fracMask := '';
    end;
    fracPH := 0;
    for j := 1 to Length(fracMask) do
      if (fracMask[j] = '0') or (fracMask[j] = '#') then Inc(fracPH);
    grouping := Pos(',', intMask) > 0;
    reqZeros := 0;
    for j := 1 to Length(intMask) do if intMask[j] = '0' then Inc(reqZeros);
    scaled := Round(absV * Power(10, fracPH));
    Z := IntToStr(scaled);
    while Length(Z) < fracPH + 1 do Z := '0' + Z;
    fracDigits := Copy(Z, Length(Z) - fracPH + 1, fracPH);
    intDigits := Copy(Z, 1, Length(Z) - fracPH);
    if intDigits = '' then intDigits := '0';
    while Length(intDigits) < reqZeros do intDigits := '0' + intDigits;
    if reqZeros = 0 then
      while (Length(intDigits) > 1) and (intDigits[1] = '0') do Delete(intDigits, 1, 1);
    if grouping and (Length(intDigits) > 0) then intOut := Grouped(intDigits)
    else intOut := intDigits;
    fracOut := '';
    for j := 1 to Length(fracMask) do
      if (fracMask[j] = '0') or (fracMask[j] = '#') then
      begin
        if j <= Length(fracDigits) then fracOut := fracOut + fracDigits[j]
        else fracOut := fracOut + '0';
      end
      else
        fracOut := fracOut + fracMask[j];
    // trailing '#' placeholders drop trailing zeros
    j := Length(fracMask);
    while (j >= 1) and (Length(fracOut) >= 1) and (fracMask[j] = '#') and (fracOut[Length(fracOut)] = '0') do
    begin
      Delete(fracOut, Length(fracOut), 1);
      Dec(j);
    end;
    Result := intOut;
    if fracOut <> '' then Result := Result + '.' + fracOut;
    if neg and (scaled <> 0) then Result := '-' + Result;
    Result := prefix + Result + suffix;
  end;

  function Scientific(V: Double; const FM: string; EIdx: Integer): string;
  var
    mantMask, expDigitsMask: string;
    plusSign: Boolean;
    expo, j, expDigits: Integer;
    absV, mant: Double;
    mantStr, expStr: string;
  begin
    plusSign := FM[EIdx + 1] = '+';
    mantMask := Copy(FM, 1, EIdx - 1);
    expDigitsMask := Copy(FM, EIdx + 2, MaxInt);
    expDigits := 0;
    for j := 1 to Length(expDigitsMask) do
      if (expDigitsMask[j] = '0') or (expDigitsMask[j] = '#') then Inc(expDigits);
    absV := Abs(V);
    if absV = 0 then begin expo := 0; mant := 0; end
    else begin expo := Floor(Log10(absV)); mant := absV / Power(10, expo); end;
    mantStr := FixedPoint(mant, mantMask);
    expStr := IntToStr(Abs(expo));
    while Length(expStr) < expDigits do expStr := '0' + expStr;
    if expo < 0 then expStr := '-' + expStr
    else if plusSign then expStr := '+' + expStr;
    Result := mantStr + Copy(FM, EIdx, 1) + expStr;
    if (V < 0) and (absV <> 0) then Result := '-' + Result;
  end;

begin
  if Mask = '' then
  begin
    Result := FloatToStrF(Value, ffGeneral, 15, 0);
    if Copy(Result, 1, 2) = '0.' then Delete(Result, 1, 1)
    else if Copy(Result, 1, 3) = '-0.' then Delete(Result, 2, 1);
    Exit;
  end;
  M := Mask;
  // Date/time mask: no numeric placeholders (0/#) but contains date/time letters -> format as a date.
  hasNum := False; hasDate := False;
  for i := 1 to Length(M) do
  begin
    if (M[i] = '0') or (M[i] = '#') then hasNum := True;
    if UpCase(M[i]) in ['D', 'M', 'Y', 'H', 'S', 'N'] then hasDate := True;
  end;
  if (not hasNum) and hasDate then Exit(FormatDateMask(Value, M));
  pctCount := 0;
  for i := 1 to Length(M) do if M[i] = '%' then Inc(pctCount);
  for i := 1 to pctCount do Value := Value * 100;
  ePos := 0;
  for i := 1 to Length(M) - 1 do
    if ((M[i] = 'E') or (M[i] = 'e')) and ((M[i+1] = '+') or (M[i+1] = '-')) then
    begin ePos := i; Break; end;
  if ePos > 0 then Result := Scientific(Value, M, ePos)
  else Result := FixedPoint(Value, M);
end;

// FreeBASIC FORMAT with a date/time mask: the value is a TDateTime serial (FB serial == FPC TDateTime,
// epoch 1899-12-30). Supported tokens: d/dd/ddd/dddd/ddddd, m/mm/mmm/mmmm (month) or minute when in a
// time context, M/MM (always month), n/nn (minute), y/yy/yyyy, h/hh, s/ss, ttttt, AM/PM | A/P (12-hour),
// ':' and '/' separators, "..." and \x literals. (English month/day names, like MONTHNAME/WEEKDAYNAME.)
function TBytecodeVM.FormatDateMask(Value: Double; const Mask: string): string;
var
  y, mo, d, h, mi, s, ms: Word;
  wd, i, runLen, h12: Integer;
  c, cl: Char;
  timeCtx, hasAMPM, pm: Boolean;

  function Pad2(v: Integer): string;
  begin Result := IntToStr(v); if Length(Result) < 2 then Result := '0' + Result; end;

  function MonName(n: Integer; full: Boolean): string;
  begin
    case n of
      1: Result := 'January';  2: Result := 'February'; 3: Result := 'March';     4: Result := 'April';
      5: Result := 'May';      6: Result := 'June';     7: Result := 'July';      8: Result := 'August';
      9: Result := 'September'; 10: Result := 'October'; 11: Result := 'November'; 12: Result := 'December';
    else Result := '';
    end;
    if (not full) and (Length(Result) > 3) then Result := Copy(Result, 1, 3);
  end;

  function DayName(n: Integer; full: Boolean): string;   // n: 1=Sunday .. 7=Saturday
  begin
    case n of
      1: Result := 'Sunday';   2: Result := 'Monday'; 3: Result := 'Tuesday'; 4: Result := 'Wednesday';
      5: Result := 'Thursday'; 6: Result := 'Friday'; 7: Result := 'Saturday';
    else Result := '';
    end;
    if (not full) and (Length(Result) > 3) then Result := Copy(Result, 1, 3);
  end;

begin
  DecodeDate(Value, y, mo, d);
  DecodeTime(Value, h, mi, s, ms);
  wd := DayOfWeek(Value);                       // 1=Sunday .. 7=Saturday
  hasAMPM := (Pos('AM/PM', UpperCase(Mask)) > 0) or (Pos('A/P', UpperCase(Mask)) > 0);
  pm := h >= 12;
  h12 := h mod 12; if h12 = 0 then h12 := 12;
  Result := '';
  timeCtx := False;
  i := 1;
  // NOTE: do NOT use the `Continue` loop keyword here — TBytecodeVM has a method named Continue (CONT)
  // that would shadow it and run instead. The body is an if/else chain that increments i per branch.
  while i <= Length(Mask) do
  begin
    c := Mask[i];
    cl := UpCase(c);
    if (c = '\') and (i < Length(Mask)) then
    begin
      Result := Result + Mask[i+1]; Inc(i, 2);
    end
    else if c = '"' then
    begin
      Inc(i);
      while (i <= Length(Mask)) and (Mask[i] <> '"') do begin Result := Result + Mask[i]; Inc(i); end;
      if i <= Length(Mask) then Inc(i);   // skip the closing quote
    end
    else if hasAMPM and (cl = 'A') and (i + 4 <= Length(Mask)) and (UpperCase(Copy(Mask, i, 5)) = 'AM/PM') then
    begin
      if pm then Result := Result + 'PM' else Result := Result + 'AM'; Inc(i, 5);
    end
    else if hasAMPM and ((cl = 'A') or (cl = 'P')) and (i + 2 <= Length(Mask)) and (UpperCase(Copy(Mask, i, 3)) = 'A/P') then
    begin
      if pm then Result := Result + 'P' else Result := Result + 'A'; Inc(i, 3);
    end
    else
    begin
    // run of the same letter (case-insensitive)
    runLen := 1;
    while (i + runLen <= Length(Mask)) and (UpCase(Mask[i + runLen]) = cl) do Inc(runLen);
    case cl of
      'D':
        begin
          if runLen = 1 then Result := Result + IntToStr(d)
          else if runLen = 2 then Result := Result + Pad2(d)
          else if runLen = 3 then Result := Result + DayName(wd, False)
          else if runLen = 4 then Result := Result + DayName(wd, True)
          else Result := Result + Pad2(mo) + '/' + Pad2(d) + '/' + IntToStr(y);   // ddddd: complete date
          timeCtx := False; Inc(i, runLen);
        end;
      'M':
        begin
          if (c = 'M') or (not timeCtx) then           // 'M' always month; 'm' is month unless in a time context
          begin
            if runLen = 1 then Result := Result + IntToStr(mo)
            else if runLen = 2 then Result := Result + Pad2(mo)
            else if runLen = 3 then Result := Result + MonName(mo, False)
            else Result := Result + MonName(mo, True);
            timeCtx := False;
          end
          else
          begin
            if runLen = 1 then Result := Result + IntToStr(mi) else Result := Result + Pad2(mi);
            timeCtx := True;
          end;
          Inc(i, runLen);
        end;
      'N':
        begin
          if runLen = 1 then Result := Result + IntToStr(mi) else Result := Result + Pad2(mi);
          timeCtx := True; Inc(i, runLen);
        end;
      'Y':
        begin
          if runLen >= 3 then Result := Result + IntToStr(y) else Result := Result + Pad2(y mod 100);
          timeCtx := False; Inc(i, runLen);
        end;
      'H':
        begin
          if hasAMPM then
          begin
            if runLen = 1 then Result := Result + IntToStr(h12) else Result := Result + Pad2(h12);
          end
          else
          begin
            if runLen = 1 then Result := Result + IntToStr(h) else Result := Result + Pad2(h);
          end;
          timeCtx := True; Inc(i, runLen);
        end;
      'S':
        begin
          if runLen = 1 then Result := Result + IntToStr(s) else Result := Result + Pad2(s);
          timeCtx := True; Inc(i, runLen);
        end;
      'T':
        begin
          if runLen >= 5 then Result := Result + Pad2(h) + ':' + Pad2(mi) + ':' + Pad2(s)  // ttttt: complete time
          else Result := Result + Copy(Mask, i, runLen);
          Inc(i, runLen);
        end;
    else
      begin
        Result := Result + c;
        if c = ':' then timeCtx := True
        else if c = '/' then timeCtx := False;
        Inc(i);
      end;
    end;   // case
    end;   // else (non-literal/non-AMPM token branch)
  end;     // while
end;

function TBytecodeVM.FileLength(const Path: string): Int64;
// FreeBASIC FILELEN(path): size of a file in bytes, or 0 if it does not exist / can't be opened.
var
  fs: TFileStream;
begin
  Result := 0;
  if not FileExists(Path) then Exit;
  try
    fs := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try Result := fs.Size; finally fs.Free; end;
  except
    Result := 0;
  end;
end;

function TBytecodeVM.FileDateTimeSerial(const Path: string): Double;
// FreeBASIC FILEDATETIME(path): the file's last-modified timestamp as a Date Serial (Double),
// or 0 if the file does not exist. FPC's FileAge returns a packed DOS datetime (-1 if absent);
// FileDateToDateTime converts it to a TDateTime, which is the same day-serial convention FB uses.
var
  Age: LongInt;
begin
  Result := 0;
  if not FileExists(Path) then Exit;
  try
    Age := FileAge(Path);
    if Age = -1 then Exit;
    Result := FileDateToDateTime(Age);
  except
    Result := 0;
  end;
end;

procedure TBytecodeVM.RawFree(RawPtr: Int64);
var
  dataOfs, sz: PtrUInt;
begin
  if (RawPtr and RAWPTR_TAG) = 0 then Exit;            // not a raw pointer / NULL
  dataOfs := RawPtr and RAWPTR_OFS_MASK;
  if (dataOfs < 8) or (dataOfs > PtrUInt(Length(FRawHeap))) then Exit;
  EnterCriticalSection(FRawHeapLock);
  try
    sz := PtrUInt((@FRawHeap[dataOfs - 8])^);
    if FRawFreeCount >= Length(FRawFreeOfs) then
    begin
      SetLength(FRawFreeOfs, (FRawFreeCount + 1) * 2);
      SetLength(FRawFreeSz, (FRawFreeCount + 1) * 2);
    end;
    FRawFreeOfs[FRawFreeCount] := dataOfs;
    FRawFreeSz[FRawFreeCount] := sz;
    Inc(FRawFreeCount);
  finally
    LeaveCriticalSection(FRawHeapLock);
  end;
end;

function TBytecodeVM.RawRealloc(RawPtr: Int64; ByteCount: PtrUInt): Int64;
var
  oldOfs, oldSz, newOfs, copySz: PtrUInt;
begin
  if (RawPtr and RAWPTR_TAG) = 0 then Exit(RawAlloc(ByteCount));   // realloc(NULL,n) == alloc
  oldOfs := RawPtr and RAWPTR_OFS_MASK;
  oldSz := PtrUInt((@FRawHeap[oldOfs - 8])^);
  Result := RawAlloc(ByteCount);
  newOfs := Result and RAWPTR_OFS_MASK;
  copySz := oldSz;
  if PtrUInt((@FRawHeap[newOfs - 8])^) < copySz then copySz := PtrUInt((@FRawHeap[newOfs - 8])^);
  Move(FRawHeap[oldOfs], FRawHeap[newOfs], copySz);
  RawFree(RawPtr);
end;

function TBytecodeVM.RawLoadInt(RawPtr: Int64; TypeCode: Integer): Int64;
var
  ofs: PtrUInt;
begin
  ofs := RawPtr and RAWPTR_OFS_MASK;
  if (RawPtr and RAWPTR_TAG) = 0 then raise ERangeError.Create('Null or invalid raw pointer dereference');
  case TypeCode of
    RTC_I8:  Result := PShortInt(@FRawHeap[ofs])^;
    RTC_I16: Result := PSmallInt(@FRawHeap[ofs])^;
    RTC_I32: Result := PLongInt(@FRawHeap[ofs])^;
  else
    Result := PInt64(@FRawHeap[ofs])^;
  end;
end;

function TBytecodeVM.RawLoadFloat(RawPtr: Int64; TypeCode: Integer): Double;
var
  ofs: PtrUInt;
begin
  ofs := RawPtr and RAWPTR_OFS_MASK;
  if (RawPtr and RAWPTR_TAG) = 0 then raise ERangeError.Create('Null or invalid raw pointer dereference');
  if TypeCode = RTC_SINGLE then Result := PSingle(@FRawHeap[ofs])^
  else Result := PDouble(@FRawHeap[ofs])^;
end;

procedure TBytecodeVM.RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
var
  ofs: PtrUInt;
begin
  ofs := RawPtr and RAWPTR_OFS_MASK;
  if (RawPtr and RAWPTR_TAG) = 0 then raise ERangeError.Create('Null or invalid raw pointer dereference');
  case TypeCode of
    RTC_I8:  PShortInt(@FRawHeap[ofs])^ := ShortInt(Value);
    RTC_I16: PSmallInt(@FRawHeap[ofs])^ := SmallInt(Value);
    RTC_I32: PLongInt(@FRawHeap[ofs])^ := LongInt(Value);
  else
    PInt64(@FRawHeap[ofs])^ := Value;
  end;
end;

procedure TBytecodeVM.RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
var
  ofs: PtrUInt;
begin
  ofs := RawPtr and RAWPTR_OFS_MASK;
  if (RawPtr and RAWPTR_TAG) = 0 then raise ERangeError.Create('Null or invalid raw pointer dereference');
  if TypeCode = RTC_SINGLE then PSingle(@FRawHeap[ofs])^ := Value
  else PDouble(@FRawHeap[ofs])^ := Value;
end;

// FB_MEMCOPY / FB_MEMMOVE: copy ByteCount bytes from SrcPtr to DstPtr on the raw byte heap. Both raw
// pointers are RAWPTR_TAG-tagged byte offsets. FPC Move is overlap-safe, so this serves both the
// (non-overlapping) memcopy and the (overlap-safe) memmove semantics.
procedure TBytecodeVM.RawMemCopy(DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);
var
  dofs, sofs: PtrUInt;
begin
  if ByteCount = 0 then Exit;
  if ((DstPtr and RAWPTR_TAG) = 0) or ((SrcPtr and RAWPTR_TAG) = 0) then
    raise ERangeError.Create('Null or invalid raw pointer in memory copy');
  dofs := DstPtr and RAWPTR_OFS_MASK;
  sofs := SrcPtr and RAWPTR_OFS_MASK;
  if (dofs + ByteCount > PtrUInt(Length(FRawHeap))) or (sofs + ByteCount > PtrUInt(Length(FRawHeap))) then
    raise ERangeError.Create('Raw memory copy out of bounds');
  Move(FRawHeap[sofs], FRawHeap[dofs], ByteCount);
end;

// CLEAR: set ByteCount bytes at DstPtr to Value on the raw byte heap.
procedure TBytecodeVM.RawClear(DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);
var
  dofs: PtrUInt;
begin
  if ByteCount = 0 then Exit;
  if (DstPtr and RAWPTR_TAG) = 0 then
    raise ERangeError.Create('Null or invalid raw pointer in clear');
  dofs := DstPtr and RAWPTR_OFS_MASK;
  if dofs + ByteCount > PtrUInt(Length(FRawHeap)) then
    raise ERangeError.Create('Raw clear out of bounds');
  FillChar(FRawHeap[dofs], ByteCount, Value);
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
  WCtx.CursorRow := 0;
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
        bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr,
        bcRandomize:  // RANDOMIZE: Src1 = seed reg (Dest unused = 0)
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

        // NarrowInt: int Dest, int Src1 (B1.5)
        bcNarrowInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // NarrowSingle: float Dest, float Src1 (B1.5)
        bcNarrowSingle:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
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
        bcMathExp, bcMathLog, bcMathAtn, bcMathRnd,
        bcMathAcos, bcMathAsin, bcMathAtan2, bcMathFix, bcMathFrac,
        bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // Date/time: NOW/TIMER -> float Dest, no sources.
        bcDateNow:
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;

        // DATESERIAL/TIMESERIAL: float Dest, int Src1/Src2, and an int register in Immediate (3rd arg).
        bcDateSerial, bcTimeSerial:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;
        end;

        // DATEVALUE/TIMEVALUE: float Dest, string Src1.
        bcDateValue:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY: int Dest, float Src1 (serial).
        bcDateDecode:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // ISDATE: int Dest, string Src1.
        bcIsDate:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // DATE/TIME, CURDIR$, EXEPATH: string Dest, no sources.
        bcDateStr, bcCurDir, bcExePath:
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;

        // FORMAT(num, mask): string Dest, string Src1 (mask), float Immediate reg (value).
        bcStrFormat:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;
        end;

        // ENVIRON$(name): string Dest, string Src1.
        bcEnviron:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // COMMAND$(index): string Dest, int Src1 (index).
        bcCommand:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // MONTHNAME/WEEKDAYNAME: string Dest, int Src1.
        bcDateName:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // DATEADD: float Dest, string Src1 (interval), int Src2 (number), float Immediate reg (serial).
        bcDateAdd:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;
        end;

        // DATEDIFF: int Dest, string Src1 (interval), float Src2 (s1), float Immediate reg (s2).
        bcDateDiff:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
          if Instr.Immediate > MaxFloatReg then MaxFloatReg := Instr.Immediate;
        end;

        // DATEPART: int Dest, string Src1 (interval), float Src2 (serial).
        bcDatePart:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;

        // SETDATE/SETTIME: string Src1 only (statement, no result).
        bcSetClock:
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;

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
        bcStrLeftW, bcStrRightW, bcStrMidW,                        // WSTRING codepoint substrings
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
        bcStrLen, bcStrLenW, bcStrAsc, bcStrDec, bcStrValInt, bcStrSAdd, bcStrCvInt, bcFileExists, bcFileLen:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // Int Src1 -> String Dest (CHR$, HEX$, ERR$, SPACE, OCT, BIN, MK*int)
        bcStrChr, bcStrHex, bcStrErr, bcStrSpace, bcStrOct, bcStrBin, bcStrWChr, bcStrMkInt:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;

        // STRING(n,ch) / WSTRING(n,cp): int count (Src1) + int char code/codepoint (Src2) -> String Dest
        bcStrString, bcStrWStringN:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;

        // TRIM/LTRIM/RTRIM(s$, set$) -> String Dest (mode is a constant in Immediate)
        bcStrTrimSet:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // Float Src1 -> String Dest (STR$, MK*float)
        bcStrStr, bcStrMkFloat:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        end;

        // String Src1 -> Float Dest (VAL, CV*float, FILEDATETIME)
        bcStrVal, bcStrCvFloat, bcFileDateTime:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        // INSTR/INSTRREV(haystack$, needle$[, start]) -> int Dest
        bcStrInstr, bcStrInstrRev, bcStrInstrRevAny, bcStrInstrW, bcStrInstrRevW:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;  // haystack
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;  // needle / set
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

        // PrintBool/PrintUInt (B1.5 phase C): int value in Src1
        bcPrintBool, bcPrintUInt:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
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

        // Pointer deref (FreeBASIC). Address (Src1) is always an int register; value bank varies.
        bcRefLoadInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;    // value
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;    // address
        end;
        bcRefLoadFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;  // value
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;      // address
        end;
        bcRefLoadString:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;  // value
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;        // address
        end;
        bcRefStoreInt:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;    // address
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;    // value
        end;
        bcRefStoreFloat:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;      // address
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;  // value
        end;
        bcRefStoreString:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;        // address
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;  // value
        end;
        bcRefAddrField:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;        // packed addr
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;        // record handle
        end;
        // Raw heap (FreeBASIC Allocate family): Src1 is always int (count/pointer).
        bcRawAlloc, bcRawRealloc:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;        // raw pointer
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;        // count / old pointer
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;        // realloc count
        end;
        bcRawFree:
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        bcRawLoadInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        bcRawLoadFloat:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        bcRawStoreInt:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        bcRawStoreFloat:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxFloatReg then MaxFloatReg := Instr.Src2;
        end;
        // FB_MEMCOPY/FB_MEMMOVE: Dest=dst result, Src1=dst, Src2=src, Immediate=byte count — all int.
        bcRawMemCopy, bcRawMemMove:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;
        end;
        // CLEAR: Src1=dst, Src2=value, Immediate=byte count — all int.
        bcRawClear:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;
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

        // FreeBASIC graphics slice: all int registers.
        bcGfxScreenRes:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // w
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // h
        end;
        bcGfxPset:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;  // color reg
        end;
        bcGfxPoint:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // color result
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y
        end;
        bcGfxPaint:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;  // color reg
        end;
        // bcGfxLine: Src1=x1, Src2=y1; Immediate [0-15]=x2, [16-31]=y2, [32-47]=color, [48-49]=flag
        bcGfxLine:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x1
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y1
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;             // x2
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;  // y2
          if ((Instr.Immediate shr 32) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 32) and $FFFF;  // color
        end;
        // bcGfxCircle: Src1=x, Src2=y; Immediate [0-15]=radius, [16-31]=color
        bcGfxCircle:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;             // radius
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;  // color
        end;
        // bcGfxPalette: Src1=index, Src2=packed colour (both int)
        bcGfxPalette:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // index
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // colour
        end;
        // bcGfxPalGet: Dest=result, Src1=index (Immediate = which selector, not a reg)
        bcGfxPalGet:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // result
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // index
        end;
        // bcGfxColor: Src1=fg, Src2=bg (Immediate = present-flags, not regs)
        bcGfxColor:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // fg
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // bg
        end;
        // bcGfxForeColor: Dest=result (current foreground)
        bcGfxForeColor:
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // result
        // bcGfxImageCreate: Dest=handle, Src1=w, Src2=h, Immediate=fill colour reg
        bcGfxImageCreate:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // handle
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // w
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // h
          if Instr.Immediate > MaxIntReg then MaxIntReg := Instr.Immediate;  // fill colour reg
        end;
        // bcGfxImageDestroy: Src1=handle
        bcGfxImageDestroy:
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // handle
        // bcGfxImageInfo: Dest=result, Src1=handle (Immediate = which selector, not a reg)
        bcGfxImageInfo:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // result
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // handle
        end;
        // bcGfxGet: Src1=x1, Src2=y1; Immediate [0-15]=x2, [16-31]=y2, [32-47]=dst handle
        bcGfxGet:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x1
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y1
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;             // x2
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;  // y2
          if ((Instr.Immediate shr 32) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 32) and $FFFF;  // dst handle
        end;
        // bcGfxPut: Src1=x, Src2=y; Immediate [0-15]=src handle (Immediate[16-31]=mode const, not a reg)
        bcGfxPut:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;   // x
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;   // y
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;             // src handle
        end;
        // bcGfxScreenInfo: Dest=result (Immediate = which selector, not a reg)
        bcGfxScreenInfo:
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;   // result
        // bcGfxScreenSet / bcGfxPCopy: Src1, Src2 (Immediate = flags const, not regs)
        bcGfxScreenSet, bcGfxPCopy:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        // bcGfxWindow / bcGfxView: Src1=x1, Src2=y1; Immediate [0-15]=x2, [16-31]=y2 (bits 32-33 = flags)
        bcGfxWindow, bcGfxView:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;
          if ((Instr.Immediate shr 16) and $FFFF) > MaxIntReg then MaxIntReg := (Instr.Immediate shr 16) and $FFFF;
        end;
        // bcGfxPMap: Dest=result, Src1=coord (Immediate = n selector, not a reg)
        bcGfxPMap:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // bcGfxScreen: Src1=mode (Immediate = num_pages const, not a reg)
        bcGfxScreen:
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        // bcMultikey: Dest=result, Src1=scancode
        bcMultikey:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // bcGetmouse: Dest=status; bcMouseAxis: Dest=result (Immediate=which const, not a reg)
        bcGetmouse, bcMouseAxis:
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
        // bcSetmouse: Src1=x, Src2=y, Immediate[0-15]=visibility reg
        bcSetmouse:
        begin
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;
        end;
        // bcGetJoystick: Dest=status, Src1=id; bcStick/bcStrig: Dest=result, Src1=axis/button (all int)
        bcGetJoystick, bcStick, bcStrig:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // bcJoyBtn: Dest=button bitmask (int)
        bcJoyBtn:
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
        // bcJoyAxis: Dest=axis value (FLOAT); Immediate=which const (not a reg)
        bcJoyAxis:
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;

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

procedure TBytecodeVM.SetGraphicsBackend(Backend: IGraphicsBackend; OwnedObj: TObject = nil);
begin
  if Assigned(FOwnedGraphics) and (FOwnedGraphics <> OwnedObj) then
    FreeAndNil(FOwnedGraphics);
  FGraphics := Backend;
  FOwnedGraphics := OwnedObj;
end;

procedure TBytecodeVM.UseSoftwareGraphics;
var
  SW: TSoftwareGraphicsBackend;
begin
  SW := TSoftwareGraphicsBackend.Create;
  SetGraphicsBackend(SW, SW);
end;

procedure TBytecodeVM.SetInputDevice(Device: IInputDevice);
begin
  FInputDevice := Device;
end;

procedure TBytecodeVM.SetProgramArgs(const Args: array of string);
var
  i: Integer;
begin
  SetLength(FProgramArgs, Length(Args));
  for i := 0 to High(Args) do
    FProgramArgs[i] := Args[i];
end;

function TBytecodeVM.CommandLine(Index: Integer): string;
// COMMAND$(index): index < 0 -> the whole command line (program args, space-separated); 0 -> the
// executable name; n >= 1 -> the n-th program argument ('' if out of range). FProgramArgs holds the
// arguments only (arg 1 at FProgramArgs[0]); the interpreter/script name is excluded.
var
  i: Integer;
begin
  if Index < 0 then
  begin
    Result := '';
    for i := 0 to High(FProgramArgs) do
      if i = 0 then Result := FProgramArgs[i]
      else Result := Result + ' ' + FProgramArgs[i];
  end
  else if Index = 0 then
    Result := ParamStr(0)
  else if Index <= Length(FProgramArgs) then
    Result := FProgramArgs[Index - 1]
  else
    Result := '';
end;

function TBytecodeVM.DiskStatusString: string;
// DS$: the Commodore drive status channel, formatted "NN, MESSAGE,TT,SS". We report the last file
// operation's error code (0 = OK) and its message; track/sector are always 00 (no physical geometry).
var
  Code: Integer;
  Msg: string;
begin
  Code := FCtx.LastErrorCode;
  if Code = 0 then
    Msg := 'OK'
  else
  begin
    Msg := FCtx.LastErrorMessage;
    if (Msg <> '') and (Msg[1] = '?') then Delete(Msg, 1, 1);   // strip the leading '?' of CBM messages
    if Msg = '' then Msg := 'ERROR';
  end;
  Result := Format('%.2d, %s,00,00', [Code, UpperCase(Msg)]);
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
          Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Src1]);
      end;
    bcPowFloat: Ctx.FloatRegs[Instr.Dest] := Power(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]);
    bcNegFloat: Ctx.FloatRegs[Instr.Dest] := -Ctx.FloatRegs[Instr.Src1];
    bcIntToFloat: Ctx.FloatRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
    bcFloatToInt: Ctx.IntRegs[Instr.Dest] := Trunc(Ctx.FloatRegs[Instr.Src1]);
    // Numeric -> string (FreeBASIC Str() / "&" concat): no leading sign-space, unlike v7 STR$.
    bcIntToString: Ctx.StringRegs[Instr.Dest] := IntToStr(Ctx.IntRegs[Instr.Src1]);
    bcFloatToString:
      // FreeBASIC Str()/"&" concat of a float: the number with no leading sign-space and no trailing
      // field-space (FormatNumber adds both under the Commodore preset).
      Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]));
    bcFloatRound: Ctx.IntRegs[Instr.Dest] := Round(Ctx.FloatRegs[Instr.Src1]);  // CINT (round-to-even)
    bcNarrowInt: Ctx.IntRegs[Instr.Dest] := NarrowInt64(Ctx.IntRegs[Instr.Src1], Instr.Immediate);  // B1.5
    bcNarrowSingle: Ctx.FloatRegs[Instr.Dest] := Single(Ctx.FloatRegs[Instr.Src1]);                  // B1.5
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
    bcShl: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] shl Ctx.IntRegs[Instr.Src2];  // SHL
    bcShr: Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] shr Ctx.IntRegs[Instr.Src2];  // SHR (logical)
    bcRandomize:  // RANDOMIZE: seed the RNG (Immediate=1 -> explicit seed in Src1; 0 -> time-based)
      if Instr.Immediate <> 0 then RandSeed := Cardinal(Ctx.IntRegs[Instr.Src1]) else Randomize;
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
    bcCallSubIndirect:  // FreeBASIC function pointer: same as bcCallSub but the target entry PC is in Src1 (int reg)
      begin
        FramePush(Ctx);
        Ctx.CallStack[Ctx.CallStackPtr] := Ctx.PC;
        Inc(Ctx.CallStackPtr);
        Ctx.PC := Ctx.IntRegs[Instr.Src1] - 1;
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
    bcRecordFree:
      FreeSharedRecord(Ctx.IntRegs[Instr.Src1]);  // DELETE p: release the heap record (Src1=handle)
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
    bcAssert:
      begin
        // ASSERT/ASSERTWARN: if the condition (Src1) is 0 (false), print the pre-built diagnostic
        // (Src2). For ASSERT (Immediate bit 0 set) the program also halts; ASSERTWARN continues.
        if Ctx.IntRegs[Instr.Src1] = 0 then
        begin
          if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src2]);
            FOutputDevice.NewLine;
          end;
          if (Instr.Immediate and 1) <> 0 then
          begin
            Ctx.Running := False;
            Ctx.Stopped := False;
          end;
        end;
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
          ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME (MODERN only)
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
          ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME NEXT (MODERN only)
          Exit;  // Don't increment PC - we already set it
        end;
        // If not in error handler, just continue
      end;
    bcOnError:
      begin
        // ON ERROR GOTO <label> - install a label-based error handler.
        // Immediate = resolved handler PC; TrapLine = -2 marks "label handler, PC pre-resolved".
        Ctx.TrapPC := Instr.Immediate;
        Ctx.TrapLine := -2;
      end;
    bcResumeLabel:
      begin
        // RESUME <label> - resume at a named label (Immediate = target PC)
        if Ctx.InErrorHandler then
        begin
          Ctx.PC := Instr.Immediate;
          Ctx.InErrorHandler := False;
          ResetErrorStateIfModern(Ctx);  // FreeBASIC clears Err after RESUME <label> (MODERN only)
          Exit;  // Don't increment PC - we already set it
        end;
      end;
    bcRaiseError:
      begin
        // ERROR <n> - raise a user runtime error number n. The run-loop except handler reads the
        // code into ERR and transfers to any active ON ERROR / TRAP handler (or aborts if none).
        raise TExecutorRuntimeException.CreateWithCode(
          'Error ' + IntToStr(Ctx.IntRegs[Instr.Src1]), Ctx.IntRegs[Instr.Src1]);
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

    bcSetEnviron:
      begin
        // SETENVIRON "NAME=value": record a VM-internal override (consulted by ENVIRON$). A bare "NAME"
        // (no '=') clears the value. Portable — no OS-specific setenv needed.
        SetEnvOverride(Ctx.StringRegs[Instr.Src1]);
      end;

    bcShell:
      begin
        // SHELL cmd: run the command through the platform shell. Dest (if used) receives the exit code.
        Ctx.IntRegs[Instr.Dest] := RunShellCommand(Ctx.StringRegs[Instr.Src1]);
      end;

    bcChdir:
      begin
        // CHDIR "path"
        // Src1 = path
        ExecuteChdir(Ctx.StringRegs[Instr.Src1]);
      end;

    bcRmdir:
      begin
        // RMDIR "path" (FreeBASIC/QB) - remove an empty directory
        // Src1 = path
        ExecuteRmdir(Ctx.StringRegs[Instr.Src1]);
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
        Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Dest]);

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
        Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Src1]);

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
      if Abs(Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF]) < 1e-300 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
          DivZeroFloat(FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]])   // MODERN: IEEE; CLASSIC: error
      else
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
// first non-numeric character - matches FreeBASIC VALINT/VALLNG/VALUINT/VALULNG.
// A "&H"/"&O"/"&B" prefix selects hexadecimal/octal/binary parsing. Returns 0 when
// no digits are present.
function ParseLeadingInt64(const S: string): Int64;
var
  I, Len, Base, D: Integer;
  Neg: Boolean;
  C: Char;
  U: QWord;
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
  // FreeBASIC base prefixes: &H hex, &O octal, &B binary.
  if (I < Len) and (S[I] = '&') then
  begin
    C := UpCase(S[I + 1]);
    Base := 0;
    if C = 'H' then Base := 16
    else if C = 'O' then Base := 8
    else if C = 'B' then Base := 2;
    if Base > 0 then
    begin
      Inc(I, 2);  // skip the "&X" prefix
      U := 0;
      while I <= Len do
      begin
        C := UpCase(S[I]);
        if (C >= '0') and (C <= '9') then D := Ord(C) - Ord('0')
        else if (C >= 'A') and (C <= 'F') then D := Ord(C) - Ord('A') + 10
        else Break;
        if D >= Base then Break;
        U := U * QWord(Base) + QWord(D);
        Inc(I);
      end;
      Result := Int64(U);
      if Neg then Result := -Result;
      Exit;
    end;
  end;
  while (I <= Len) and (S[I] >= '0') and (S[I] <= '9') do
  begin
    Result := Result * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if Neg then Result := -Result;
end;

// Parse the leading floating-point number of a string, FreeBASIC VAL style: skip leading whitespace,
// read the longest valid number ([sign] digits [. digits] [ (e|d) [sign] digits ]) and stop at the
// first unsuitable character (so VAL("10abc")=10, VAL("10.5xy")=10.5). A "&H"/"&O"/"&B" base prefix is
// parsed as an integer. Returns 0 when no number is present.
function ParseLeadingFloat(const S: string): Double;
var
  I, J, K, Len, Code: Integer;
  T: string;
  HasDigit, HasDot: Boolean;
begin
  Result := 0.0;
  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do Inc(I);   // skip leading whitespace
  // A base prefix (optionally signed) is an integer value; reuse ParseLeadingInt64.
  J := I;
  if (J <= Len) and ((S[J] = '+') or (S[J] = '-')) then Inc(J);
  if (J <= Len) and (S[J] = '&') then
  begin
    Result := ParseLeadingInt64(Copy(S, I, Len - I + 1));
    Exit;
  end;
  // [sign] digits [. digits]
  J := I;
  if (J <= Len) and ((S[J] = '+') or (S[J] = '-')) then Inc(J);
  HasDigit := False;
  HasDot := False;
  while J <= Len do
  begin
    if (S[J] >= '0') and (S[J] <= '9') then begin HasDigit := True; Inc(J); end
    else if (S[J] = '.') and (not HasDot) then begin HasDot := True; Inc(J); end
    else Break;
  end;
  if not HasDigit then Exit;
  // Optional exponent: (e|E|d|D) [sign] digits — only consumed if at least one exponent digit follows.
  if (J <= Len) and (UpCase(S[J]) in ['E', 'D']) then
  begin
    K := J + 1;
    if (K <= Len) and ((S[K] = '+') or (S[K] = '-')) then Inc(K);
    if (K <= Len) and (S[K] >= '0') and (S[K] <= '9') then
    begin
      while (K <= Len) and (S[K] >= '0') and (S[K] <= '9') do Inc(K);
      J := K;
    end;
  end;
  T := Copy(S, I, J - I);
  // A leading '.' (e.g. ".5" or "-.5") needs a '0' for Pascal's Val; and FB's 'D' exponent -> 'E'.
  if (Length(T) >= 1) and (T[1] = '.') then T := '0' + T
  else if (Length(T) >= 2) and ((T[1] = '+') or (T[1] = '-')) and (T[2] = '.') then
    T := T[1] + '0' + Copy(T, 2, Length(T));
  T := StringReplace(T, 'd', 'e', [rfReplaceAll]);
  T := StringReplace(T, 'D', 'E', [rfReplaceAll]);
  Val(T, Result, Code);
  if Code <> 0 then Result := 0.0;
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

// FreeBASIC WSTRING helpers. Wide strings are stored as UTF-8 bytes in the ordinary string bank; these
// count/extract by Unicode codepoint. A codepoint boundary is any byte that is not a UTF-8 continuation
// byte (10xxxxxx).
function Utf8CPCount(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if (Ord(S[i]) and $C0) <> $80 then Inc(Result);
end;

// Return the substring covering CPCount codepoints starting at the 1-based codepoint CPStart, clamped.
function Utf8SubCP(const S: string; CPStart, CPCount: Integer): string;
var
  i, n, cp, bStart, bEnd: Integer;
begin
  if CPStart < 1 then CPStart := 1;
  if CPCount < 0 then CPCount := 0;
  n := Length(S);
  bStart := n + 1;   // default past end => empty
  bEnd := n + 1;     // default => copy to end of string
  cp := 0;
  for i := 1 to n do
    if (Ord(S[i]) and $C0) <> $80 then
    begin
      Inc(cp);                                  // byte i begins codepoint #cp (1-based)
      if cp = CPStart then bStart := i;
      if cp = CPStart + CPCount then begin bEnd := i; Break; end;
    end;
  if bStart > n then Exit('');
  Result := Copy(S, bStart, bEnd - bStart);
end;

// Encode a single Unicode codepoint as its UTF-8 byte sequence (FreeBASIC WCHR). Invalid codepoints
// (negative or > U+10FFFF) yield the replacement char U+FFFD.
function Utf8EncodeCP(CP: Integer): string;
begin
  if (CP < 0) or (CP > $10FFFF) then CP := $FFFD;
  if CP < $80 then
    Result := Chr(CP)
  else if CP < $800 then
    Result := Chr($C0 or (CP shr 6)) + Chr($80 or (CP and $3F))
  else if CP < $10000 then
    Result := Chr($E0 or (CP shr 12)) + Chr($80 or ((CP shr 6) and $3F)) + Chr($80 or (CP and $3F))
  else
    Result := Chr($F0 or (CP shr 18)) + Chr($80 or ((CP shr 12) and $3F)) +
              Chr($80 or ((CP shr 6) and $3F)) + Chr($80 or (CP and $3F));
end;

// Map a 1-based BYTE position in a UTF-8 string to a 1-based CODEPOINT position (0 stays 0 = not found).
function Utf8BytePosToCP(const S: string; BytePos: Integer): Integer;
var
  i: Integer;
begin
  if BytePos <= 0 then Exit(0);
  Result := 0;
  for i := 1 to BytePos do
    if (i <= Length(S)) and ((Ord(S[i]) and $C0) <> $80) then Inc(Result);
end;

procedure TBytecodeVM.ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  Len, StartPos, Count, EnvIdx: Integer;
  S, SubStr: string;
  PackInt: Int64;        // B3 serialization scratch (MK*/CV* integer pack/unpack)
  PackSingle: Single;
  PackDouble: Double;
begin
  SubOp := Instr.OpCode and $FF;  // Extract sub-opcode (low byte)
  case SubOp of
    0: // bcStrConcat
      Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1] + Ctx.StringRegs[Instr.Src2];
    1: // bcStrLen
      Ctx.IntRegs[Instr.Dest] := Length(Ctx.StringRegs[Instr.Src1]);
    25: // bcStrLenW - LEN(wstring): Unicode codepoint count of the UTF-8 byte storage.
      Ctx.IntRegs[Instr.Dest] := Utf8CPCount(Ctx.StringRegs[Instr.Src1]);
    26: // bcStrLeftW - LEFT$(wstring, n): first n codepoints.
      Ctx.StringRegs[Instr.Dest] := Utf8SubCP(Ctx.StringRegs[Instr.Src1], 1, Ctx.IntRegs[Instr.Src2]);
    27: // bcStrRightW - RIGHT$(wstring, n): last n codepoints.
      begin
        S := Ctx.StringRegs[Instr.Src1];
        Count := Ctx.IntRegs[Instr.Src2];
        Len := Utf8CPCount(S);                    // total codepoints
        if Count < 0 then Count := 0;
        if Count > Len then Count := Len;
        Ctx.StringRegs[Instr.Dest] := Utf8SubCP(S, Len - Count + 1, Count);
      end;
    28: // bcStrMidW - MID$(wstring, start[,len]): codepoint substring. Src2=start, Immediate=len reg.
      begin
        StartPos := Ctx.IntRegs[Instr.Src2];
        Count := Ctx.IntRegs[Instr.Immediate and $FFFF];
        Ctx.StringRegs[Instr.Dest] := Utf8SubCP(Ctx.StringRegs[Instr.Src1], StartPos, Count);
      end;
    29: // bcStrInstrW - INSTR(wstring, sub): codepoint position of first occurrence (0 if none).
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];
        Ctx.IntRegs[Instr.Dest] := Utf8BytePosToCP(S, Pos(SubStr, S));
      end;
    30: // bcStrInstrRevW - INSTRREV(wstring, sub): codepoint position of last occurrence (0 if none).
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];
        Len := 0;
        if SubStr <> '' then
          for StartPos := 1 to Length(S) - Length(SubStr) + 1 do
            if Copy(S, StartPos, Length(SubStr)) = SubStr then Len := StartPos;  // last byte match
        Ctx.IntRegs[Instr.Dest] := Utf8BytePosToCP(S, Len);
      end;
    31: // bcStrWChr - WCHR(n): UTF-8 byte sequence for Unicode codepoint n.
      Ctx.StringRegs[Instr.Dest] := Utf8EncodeCP(Ctx.IntRegs[Instr.Src1]);
    32: // bcStrWStringN - WSTRING(n,cp): n copies of the UTF-8 char for codepoint cp.
      begin
        Count := Ctx.IntRegs[Instr.Src1];
        if Count < 0 then Count := 0;
        SubStr := Utf8EncodeCP(Ctx.IntRegs[Instr.Src2]);
        S := '';
        for StartPos := 1 to Count do S := S + SubStr;
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    33: // bcStrSAdd - SADD(s): raw byte-heap pointer to a NUL-terminated copy of the string
      Ctx.IntRegs[Instr.Dest] := StrSAdd(Ctx.StringRegs[Instr.Src1]);
    40: // bcFileExists - FILEEXISTS(path): -1 if the file exists, else 0 (cross-platform).
      if FileExists(Ctx.StringRegs[Instr.Src1]) then Ctx.IntRegs[Instr.Dest] := -1
      else Ctx.IntRegs[Instr.Dest] := 0;
    41: // bcCurDir - CURDIR$: the current working directory (cross-platform).
      Ctx.StringRegs[Instr.Dest] := GetCurrentDir;
    42: // bcEnviron - ENVIRON$(name): value of an environment variable ('' if unset). A SETENVIRON override
        // (VM-internal) takes precedence over the OS environment.
      begin
        EnvIdx := FEnvOverrides.IndexOfName(Ctx.StringRegs[Instr.Src1]);
        if EnvIdx >= 0 then
          Ctx.StringRegs[Instr.Dest] := FEnvOverrides.ValueFromIndex[EnvIdx]
        else
          Ctx.StringRegs[Instr.Dest] := GetEnvironmentVariable(Ctx.StringRegs[Instr.Src1]);
      end;
    43: // bcFileLen - FILELEN(path): size of the file in bytes (0 if absent).
      Ctx.IntRegs[Instr.Dest] := FileLength(Ctx.StringRegs[Instr.Src1]);
    44: // bcExePath - EXEPATH: directory of the running program (cross-platform).
      Ctx.StringRegs[Instr.Dest] := ExtractFileDir(ParamStr(0));
    45: // bcStrFormat - FORMAT(num, mask): formatted number string. Value is in the Immediate float reg.
      Ctx.StringRegs[Instr.Dest] := FormatNumber(Ctx.FloatRegs[Instr.Immediate], Ctx.StringRegs[Instr.Src1]);
    46: // bcCommand - COMMAND$(index): command-line argument(s) passed to the BASIC program.
      Ctx.StringRegs[Instr.Dest] := CommandLine(Ctx.IntRegs[Instr.Src1]);
    47: // bcFileDateTime - FILEDATETIME(path): last-modified date serial (Double), 0 if absent.
      Ctx.FloatRegs[Instr.Dest] := FileDateTimeSerial(Ctx.StringRegs[Instr.Src1]);
    36: // bcStrMkInt - MKI/MKL/MKSHORT/MKLONGINT: binary copy of an integer into a string.
      begin
        // Immediate = byte width (2/4/8). Write the low `width` bytes, little-endian (two's complement).
        Count := Instr.Immediate;
        PackInt := Ctx.IntRegs[Instr.Src1];
        SetLength(S, Count);
        for StartPos := 1 to Count do
        begin
          S[StartPos] := Chr(PackInt and $FF);
          PackInt := PackInt shr 8;
        end;
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    37: // bcStrMkFloat - MKS (4=single) / MKD (8=double): binary copy of a float into a string.
      begin
        Count := Instr.Immediate;
        if Count = 4 then
        begin
          PackSingle := Ctx.FloatRegs[Instr.Src1];   // narrow Double -> IEEE-754 single
          SetLength(S, 4);
          Move(PackSingle, S[1], 4);
        end
        else
        begin
          PackDouble := Ctx.FloatRegs[Instr.Src1];
          SetLength(S, 8);
          Move(PackDouble, S[1], 8);
        end;
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    38: // bcStrCvInt - CVI/CVL/CVSHORT/CVLONGINT: read `width` little-endian bytes, sign-extend to Int64.
      begin
        Count := Instr.Immediate;                  // byte width (2/4/8)
        S := Ctx.StringRegs[Instr.Src1];
        if Length(S) < Count then
          Ctx.IntRegs[Instr.Dest] := 0             // FreeBASIC: 0 if the string is too short
        else
        begin
          PackInt := 0;
          for StartPos := Count downto 1 do
            PackInt := (PackInt shl 8) or Int64(Ord(S[StartPos]));
          // Sign-extend from the top bit of the `width`-byte value (skip for the full-width 8-byte case).
          if (Count < 8) and ((PackInt and (Int64(1) shl (Count * 8 - 1))) <> 0) then
            PackInt := PackInt or not ((Int64(1) shl (Count * 8)) - 1);
          Ctx.IntRegs[Instr.Dest] := PackInt;
        end;
      end;
    39: // bcStrCvFloat - CVS (4=single) / CVD (8=double): read IEEE-754 bytes, widen to Double.
      begin
        Count := Instr.Immediate;
        S := Ctx.StringRegs[Instr.Src1];
        if Length(S) < Count then
          Ctx.FloatRegs[Instr.Dest] := 0.0         // FreeBASIC: 0 if the string is too short
        else if Count = 4 then
        begin
          Move(S[1], PackSingle, 4);
          Ctx.FloatRegs[Instr.Dest] := PackSingle;
        end
        else
        begin
          Move(S[1], PackDouble, 8);
          Ctx.FloatRegs[Instr.Dest] := PackDouble;
        end;
      end;
    34: // bcDateStr - DATE ("mm-dd-yyyy") / TIME ("hh:mm:ss"). Immediate 0=DATE, 1=TIME.
      begin
        if Instr.Immediate = 1 then
          Ctx.StringRegs[Instr.Dest] := FormatDateTime('hh":"nn":"ss', Now + FClockOffsetDays)
        else
          Ctx.StringRegs[Instr.Dest] := FormatDateTime('mm"-"dd"-"yyyy', Now + FClockOffsetDays);
      end;
    35: // bcDateName - MONTHNAME(n) / WEEKDAYNAME(n). Immediate 0=month (1..12), 1=weekday (1=Sunday..7=Saturday).
      begin
        Count := Ctx.IntRegs[Instr.Src1];   // the 1-based index
        if Instr.Immediate = 1 then
        begin
          case Count of
            1: S := 'Sunday';   2: S := 'Monday';  3: S := 'Tuesday'; 4: S := 'Wednesday';
            5: S := 'Thursday'; 6: S := 'Friday';  7: S := 'Saturday';
          else S := '';
          end;
        end
        else
        begin
          case Count of
            1: S := 'January';  2: S := 'February'; 3: S := 'March';     4: S := 'April';
            5: S := 'May';      6: S := 'June';     7: S := 'July';      8: S := 'August';
            9: S := 'September'; 10: S := 'October'; 11: S := 'November'; 12: S := 'December';
          else S := '';
          end;
        end;
        Ctx.StringRegs[Instr.Dest] := S;
      end;
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
    24: // bcStrInstrRevAny - INSTRREV(str, Any set) -> last position of any char in set (1-based, 0 if none)
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];   // character set
        Len := 0;
        if SubStr <> '' then
          for StartPos := Length(S) downto 1 do
            if Pos(S[StartPos], SubStr) > 0 then
            begin
              Len := StartPos;
              Break;
            end;
        Ctx.IntRegs[Instr.Dest] := Len;
      end;
    22: // bcStrString - STRING(n, ch) -> n copies of the character whose code is Src2
      begin
        Count := Ctx.IntRegs[Instr.Src1];
        if Count < 0 then Count := 0;
        Ctx.StringRegs[Instr.Dest] := StringOfChar(Chr(Ctx.IntRegs[Instr.Src2] and $FF), Count);
      end;
    23: // bcStrTrimSet - LTRIM/RTRIM/TRIM(s, set). Immediate = mode: low 2 bits = side (0=both,
        // 1=left, 2=right); bit 2 (value 4) = FreeBASIC "Any" form (trim any CHARACTER in the set)
        // vs the default which trims the whole `set` substring. Case-sensitive.
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];   // trimset
        Count := Instr.Immediate and 3;         // side
        Len := Length(SubStr);
        if Len > 0 then
        begin
          if (Instr.Immediate and 4) <> 0 then
          begin
            // "Any" form: strip any single character that appears in the set.
            if Count <> 2 then                  // left or both
              while (Length(S) >= 1) and (Pos(S[1], SubStr) > 0) do
                Delete(S, 1, 1);
            if Count <> 1 then                  // right or both
              while (Length(S) >= 1) and (Pos(S[Length(S)], SubStr) > 0) do
                Delete(S, Length(S), 1);
          end
          else
          begin
            // Default form: strip repeated occurrences of the whole `set` substring.
            if Count <> 2 then                  // left or both
              while (Length(S) >= Len) and (Copy(S, 1, Len) = SubStr) do
                Delete(S, 1, Len);
            if Count <> 1 then                  // right or both
              while (Length(S) >= Len) and (Copy(S, Length(S) - Len + 1, Len) = SubStr) do
                Delete(S, Length(S) - Len + 1, Len);
          end;
        end;
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    7: // bcStrStr - STR$(n) / Str(n). FormatNumber applies the console PRINT spacing (a leading sign-
      // space AND a trailing field-space); neither belongs in the string value returned by STR$/Str.
      // MODERN (FreeBASIC Str): no spaces at all. CLASSIC (v7 STR$): keep the leading sign-space for a
      // non-negative value, drop the trailing space. (Without this, e.g. Right(Str(638269696),6) picked
      // up the trailing space and returned "69696 " instead of "269696".)
      if Assigned(FProgram) and FProgram.ModernMode then
        Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]))
      else
        Ctx.StringRegs[Instr.Dest] := TrimRight(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]));
    8: // bcStrVal - VAL(s): leading floating-point number, FreeBASIC style (leading parse + &H/&O/&B).
      begin
        Ctx.FloatRegs[Instr.Dest] := ParseLeadingFloat(Ctx.StringRegs[Instr.Src1]);
      end;
    9: // bcStrHex - HEX$(n) - full INT64 range, no leading zeros
      begin
        S := IntToHex(Ctx.IntRegs[Instr.Src1], 1);  // Minimum 1 digit
        // IntToHex with digits=1 still pads, so trim leading zeros
        while (Length(S) > 1) and (S[1] = '0') do
          Delete(S, 1, 1);
        Ctx.StringRegs[Instr.Dest] := S;
      end;
    10: // bcStrInstr - INSTR([start,] haystack, needle)
      begin
        // Src1 = haystack, Src2 = needle, Immediate = the int register holding the 1-based start position
        // (the 2-arg form passes a register holding 1).
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

function ParseDateSerial(const S: string; out DT: TDateTime): Boolean;
// Parse a date/time string into a TDateTime serial. Accepts ISO-ish forms deterministically across
// platforms/locales: "yyyy-mm-dd", "yyyy/mm/dd", "hh:mm[:ss]", or "<date> <time>". Anything else falls
// back to the locale parser. Used by DATEVALUE/TIMEVALUE/ISDATE.
var
  ds, ts, w: string;
  sp, y, mo, d, hh, mi, ss: Integer;
  dpart, tpart: TDateTime;
  haveD, haveT: Boolean;

  function SplitInts(const Str: string; Sep: Char; out a, b, c: Integer): Integer;
  // Split Str on Sep into up to 3 integer fields; returns the field count (2 or 3), or -1 if a/b are
  // not numeric or there are too many fields.
  var
    f: array[0..2] of string;
    i, n: Integer;
  begin
    n := 0; f[0] := ''; f[1] := ''; f[2] := '';
    for i := 1 to Length(Str) do
      if Str[i] = Sep then
      begin
        Inc(n);
        if n > 2 then begin Result := -1; Exit; end;
      end
      else
        f[n] := f[n] + Str[i];
    a := StrToIntDef(Trim(f[0]), -999999);
    b := StrToIntDef(Trim(f[1]), -999999);
    if n >= 2 then c := StrToIntDef(Trim(f[2]), 0) else c := 0;
    if (a = -999999) or (b = -999999) then Result := -1 else Result := n + 1;
  end;

begin
  Result := False; DT := 0;
  w := Trim(S);
  if w = '' then Exit;
  haveD := False; haveT := False; dpart := 0; tpart := 0;
  sp := Pos(' ', w);
  if sp > 0 then begin ds := Trim(Copy(w, 1, sp - 1)); ts := Trim(Copy(w, sp + 1, Length(w))); end
  else if Pos(':', w) > 0 then begin ds := ''; ts := w; end
  else begin ds := w; ts := ''; end;
  if ds <> '' then
  begin
    if (Pos('-', ds) > 0) and (SplitInts(ds, '-', y, mo, d) >= 3) and TryEncodeDate(y, mo, d, dpart) then
      haveD := True
    else if (Pos('/', ds) > 0) and (SplitInts(ds, '/', y, mo, d) >= 3) and TryEncodeDate(y, mo, d, dpart) then
      haveD := True
    else if TryStrToDate(ds, dpart) then
      haveD := True
    else
      Exit;
  end;
  if ts <> '' then
  begin
    if (Pos(':', ts) > 0) and (SplitInts(ts, ':', hh, mi, ss) >= 2) and TryEncodeTime(hh, mi, ss, 0, tpart) then
      haveT := True
    else if TryStrToTime(ts, tpart) then
      haveT := True
    else
      Exit;
  end;
  if not (haveD or haveT) then Exit;
  DT := dpart + tpart;
  Result := True;
end;

function IntervalCode(const S: string): Integer;
// FreeBASIC/VB date interval string -> internal code (used by DATEADD/DATEDIFF/DATEPART).
// 0=yyyy 1=q 2=m 3=y(dayOfYear) 4=d 5=w(weekday) 6=ww(week) 7=h 8=n(minute) 9=s. Default = day.
var
  u: string;
begin
  u := LowerCase(Trim(S));
  if u = 'yyyy' then Result := 0
  else if u = 'q' then Result := 1
  else if u = 'm' then Result := 2
  else if u = 'y' then Result := 3
  else if u = 'd' then Result := 4
  else if u = 'w' then Result := 5
  else if u = 'ww' then Result := 6
  else if u = 'h' then Result := 7
  else if u = 'n' then Result := 8
  else if u = 's' then Result := 9
  else Result := 4;
end;

procedure TBytecodeVM.ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  dtVal, dt2: TDateTime;
  dY, dMo, dD, dH, dMi, dS, dMs: Word;
  iv, n: Integer;
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
    15: // bcMathAcos - ACOS(x), domain [-1, 1]
      if Abs(Ctx.FloatRegs[Instr.Src1]) <= 1 then
        Ctx.FloatRegs[Instr.Dest] := ArcCos(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: ACOS argument out of [-1,1]');
    16: // bcMathAsin - ASIN(x), domain [-1, 1]
      if Abs(Ctx.FloatRegs[Instr.Src1]) <= 1 then
        Ctx.FloatRegs[Instr.Dest] := ArcSin(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: ASIN argument out of [-1,1]');
    17: // bcMathAtan2 - ATAN2(y, x) - Src1 = y, Src2 = x
      Ctx.FloatRegs[Instr.Dest] := ArcTan2(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]);
    18: // bcMathFix - FIX(x) - truncate toward zero
      Ctx.FloatRegs[Instr.Dest] := Trunc(Ctx.FloatRegs[Instr.Src1]);
    19: // bcMathFrac - FRAC(x) - fractional part (keeps sign)
      Ctx.FloatRegs[Instr.Dest] := Frac(Ctx.FloatRegs[Instr.Src1]);
    30: // bcMathSinh - SINH(x) - hyperbolic sine
      Ctx.FloatRegs[Instr.Dest] := Math.Sinh(Ctx.FloatRegs[Instr.Src1]);
    31: // bcMathCosh - COSH(x) - hyperbolic cosine
      Ctx.FloatRegs[Instr.Dest] := Math.Cosh(Ctx.FloatRegs[Instr.Src1]);
    32: // bcMathTanh - TANH(x) - hyperbolic tangent
      Ctx.FloatRegs[Instr.Dest] := Math.Tanh(Ctx.FloatRegs[Instr.Src1]);
    33: // bcMathAsinh - ASINH(x) - inverse hyperbolic sine
      Ctx.FloatRegs[Instr.Dest] := Math.ArcSinh(Ctx.FloatRegs[Instr.Src1]);
    34: // bcMathAcosh - ACOSH(x), domain x >= 1
      if Ctx.FloatRegs[Instr.Src1] >= 1 then
        Ctx.FloatRegs[Instr.Dest] := Math.ArcCosh(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: ACOSH argument < 1');
    35: // bcMathAtanh - ATANH(x), domain |x| < 1
      if Abs(Ctx.FloatRegs[Instr.Src1]) < 1 then
        Ctx.FloatRegs[Instr.Dest] := Math.ArcTanh(Ctx.FloatRegs[Instr.Src1])
      else
        raise Exception.Create('?ILLEGAL QUANTITY ERROR: ATANH argument out of (-1,1)');
    20: // bcDateNow - Immediate 0=NOW (date+time serial), 1=TIMER (seconds since midnight)
      begin
        dtVal := Now + FClockOffsetDays;
        if Instr.Immediate = 1 then
          Ctx.FloatRegs[Instr.Dest] := Frac(dtVal) * 86400.0   // TIMER
        else
          Ctx.FloatRegs[Instr.Dest] := dtVal;                  // NOW
      end;
    21: // bcDateDecode - YEAR/MONTH/DAY/HOUR/MINUTE/SECOND/WEEKDAY(serial). Immediate selects the field.
      begin
        dtVal := Ctx.FloatRegs[Instr.Src1];
        DecodeDate(dtVal, dY, dMo, dD);
        DecodeTime(dtVal, dH, dMi, dS, dMs);
        case Instr.Immediate of
          0: Ctx.IntRegs[Instr.Dest] := dY;
          1: Ctx.IntRegs[Instr.Dest] := dMo;
          2: Ctx.IntRegs[Instr.Dest] := dD;
          3: Ctx.IntRegs[Instr.Dest] := dH;
          4: Ctx.IntRegs[Instr.Dest] := dMi;
          5: Ctx.IntRegs[Instr.Dest] := dS;
          6: Ctx.IntRegs[Instr.Dest] := DayOfWeek(dtVal);   // 1=Sunday .. 7=Saturday
        else
          Ctx.IntRegs[Instr.Dest] := 0;
        end;
      end;
    22: // bcDateSerial - DATESERIAL(y,m,d) -> serial, with VB-style month/day rollover (Src1=y, Src2=m, Immediate=d reg)
      begin
        dtVal := EncodeDate(Word(Ctx.IntRegs[Instr.Src1]), 1, 1);
        dtVal := IncMonth(dtVal, Ctx.IntRegs[Instr.Src2] - 1);
        dtVal := dtVal + (Ctx.IntRegs[Instr.Immediate] - 1);
        Ctx.FloatRegs[Instr.Dest] := dtVal;
      end;
    23: // bcTimeSerial - TIMESERIAL(h,m,s) -> serial fraction (Src1=h, Src2=m, Immediate=s reg)
      Ctx.FloatRegs[Instr.Dest] :=
        (Ctx.IntRegs[Instr.Src1] * 3600.0 + Ctx.IntRegs[Instr.Src2] * 60.0 + Ctx.IntRegs[Instr.Immediate]) / 86400.0;
    24: // bcDateValue - DATEVALUE/TIMEVALUE(str) -> serial. Immediate 0=date part, 1=time part. 0 on failure.
      begin
        if ParseDateSerial(Ctx.StringRegs[Instr.Src1], dtVal) then
        begin
          if Instr.Immediate = 1 then Ctx.FloatRegs[Instr.Dest] := Frac(dtVal)
          else Ctx.FloatRegs[Instr.Dest] := Trunc(dtVal);
        end
        else
          Ctx.FloatRegs[Instr.Dest] := 0;
      end;
    25: // bcIsDate - ISDATE(str) -> -1 if a valid date/time string, else 0
      if ParseDateSerial(Ctx.StringRegs[Instr.Src1], dtVal) then
        Ctx.IntRegs[Instr.Dest] := -1
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    26: // bcDateAdd - DATEADD(interval$, number, serial) -> serial. Src1=interval, Src2=n, Immediate=serial reg.
      begin
        iv := IntervalCode(Ctx.StringRegs[Instr.Src1]);
        n := Ctx.IntRegs[Instr.Src2];
        dtVal := Ctx.FloatRegs[Instr.Immediate];
        case iv of
          0: dtVal := IncYear(dtVal, n);          // yyyy
          1: dtVal := IncMonth(dtVal, n * 3);     // q (quarter)
          2: dtVal := IncMonth(dtVal, n);         // m
          6: dtVal := dtVal + n * 7;              // ww (week)
          7: dtVal := dtVal + n / 24.0;           // h
          8: dtVal := dtVal + n / 1440.0;         // n (minute)
          9: dtVal := dtVal + n / 86400.0;        // s
        else
          dtVal := dtVal + n;                     // y / d / w (whole days)
        end;
        Ctx.FloatRegs[Instr.Dest] := dtVal;
      end;
    27: // bcDateDiff - DATEDIFF(interval$, s1, s2) -> int count. Src1=interval, Src2=s1, Immediate=s2 reg.
      begin
        iv := IntervalCode(Ctx.StringRegs[Instr.Src1]);
        dtVal := Ctx.FloatRegs[Instr.Src2];                 // s1
        dt2 := Ctx.FloatRegs[Instr.Immediate];              // s2
        case iv of
          0: Ctx.IntRegs[Instr.Dest] := YearOf(dt2) - YearOf(dtVal);
          1: Ctx.IntRegs[Instr.Dest] := (YearOf(dt2) * 4 + (MonthOf(dt2) - 1) div 3) -
                                        (YearOf(dtVal) * 4 + (MonthOf(dtVal) - 1) div 3);
          2: Ctx.IntRegs[Instr.Dest] := (YearOf(dt2) * 12 + MonthOf(dt2)) -
                                        (YearOf(dtVal) * 12 + MonthOf(dtVal));
          6: Ctx.IntRegs[Instr.Dest] := (Trunc(dt2) - Trunc(dtVal)) div 7;
          7: Ctx.IntRegs[Instr.Dest] := Round((dt2 - dtVal) * 24.0);
          8: Ctx.IntRegs[Instr.Dest] := Round((dt2 - dtVal) * 1440.0);
          9: Ctx.IntRegs[Instr.Dest] := Round((dt2 - dtVal) * 86400.0);
        else
          Ctx.IntRegs[Instr.Dest] := Trunc(dt2) - Trunc(dtVal);   // y / d / w (whole days)
        end;
      end;
    28: // bcDatePart - DATEPART(interval$, serial) -> int. Src1=interval, Src2=serial.
      begin
        iv := IntervalCode(Ctx.StringRegs[Instr.Src1]);
        dtVal := Ctx.FloatRegs[Instr.Src2];
        case iv of
          0: Ctx.IntRegs[Instr.Dest] := YearOf(dtVal);
          1: Ctx.IntRegs[Instr.Dest] := (MonthOf(dtVal) - 1) div 3 + 1;
          2: Ctx.IntRegs[Instr.Dest] := MonthOf(dtVal);
          3: Ctx.IntRegs[Instr.Dest] := DayOfTheYear(dtVal);   // y (day of year)
          4: Ctx.IntRegs[Instr.Dest] := DayOf(dtVal);          // d
          5: Ctx.IntRegs[Instr.Dest] := DayOfWeek(dtVal);      // w (1=Sunday)
          6: Ctx.IntRegs[Instr.Dest] := WeekOfTheYear(dtVal);  // ww
          7: Ctx.IntRegs[Instr.Dest] := HourOf(dtVal);
          8: Ctx.IntRegs[Instr.Dest] := MinuteOf(dtVal);
          9: Ctx.IntRegs[Instr.Dest] := SecondOf(dtVal);
        else
          Ctx.IntRegs[Instr.Dest] := 0;
        end;
      end;
    29: // bcSetClock - SETDATE/SETTIME str: adjust the VM clock offset. Immediate 0=SETDATE, 1=SETTIME.
      begin
        if ParseDateSerial(Ctx.StringRegs[Instr.Src1], dtVal) then
        begin
          dt2 := Now + FClockOffsetDays;   // currently-observed VM time
          if Instr.Immediate = 1 then
            // SETTIME: replace the time-of-day, keep the date.
            FClockOffsetDays := FClockOffsetDays + (Frac(dtVal) - Frac(dt2))
          else
            // SETDATE: replace the date, keep the time-of-day.
            FClockOffsetDays := FClockOffsetDays + (Trunc(dtVal) - Trunc(dt2));
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
procedure TBytecodeVM.RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean;
  HasNewLower: Boolean = False; NewLower: Integer = 0);
var
  Lb, NewSize, k: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then Exit;
  Lb := 0;
  if Length(FArrays[ArrayIdx].LowerBounds) > 0 then Lb := FArrays[ArrayIdx].LowerBounds[0];
  // An explicit "REDIM a(lb TO ub)" sets the lower bound too (FreeBASIC); a bare "REDIM a(ub)" keeps the
  // array's current lower bound. A dynamic array's element access reads this run-time bound (bcArrayLBound),
  // so the two stay consistent.
  if HasNewLower then Lb := NewLower;
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

procedure TBytecodeVM.RedimArrayN(ArrayIdx: Integer; const Uppers: array of Integer; Preserve: Boolean);
// REDIM a(u0, u1, ...) — re-dimension a multi-dimensional array. Keeps each dimension's original lower
// bound (only the upper bounds change). PRESERVE keeps the flat element order up to the new size (FB
// preserves the linear layout); otherwise the storage is cleared. Strides stay row-major (computed at
// access from Dimensions[]).
var
  d, NewSize, k, Lb: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) or (Length(Uppers) = 0) then Exit;
  NewSize := 1;
  SetLength(FArrays[ArrayIdx].Dimensions, Length(Uppers));
  if Length(FArrays[ArrayIdx].LowerBounds) < Length(Uppers) then
    SetLength(FArrays[ArrayIdx].LowerBounds, Length(Uppers));
  for d := 0 to High(Uppers) do
  begin
    Lb := FArrays[ArrayIdx].LowerBounds[d];   // keep the original lower bound per dimension
    k := Uppers[d] - Lb + 1;
    if k < 0 then k := 0;
    FArrays[ArrayIdx].Dimensions[d] := k;
    NewSize := NewSize * k;
  end;
  case FArrays[ArrayIdx].ElementType of
    0: begin
         SetLength(FArrays[ArrayIdx].IntData, NewSize);
         if not Preserve then for k := 0 to NewSize - 1 do FArrays[ArrayIdx].IntData[k] := 0;
       end;
    1: begin
         SetLength(FArrays[ArrayIdx].FloatData, NewSize);
         if not Preserve then for k := 0 to NewSize - 1 do FArrays[ArrayIdx].FloatData[k] := 0.0;
       end;
    2: begin
         SetLength(FArrays[ArrayIdx].StringData, NewSize);
         if not Preserve then for k := 0 to NewSize - 1 do FArrays[ArrayIdx].StringData[k] := '';
       end;
  end;
  FArrays[ArrayIdx].DimCount := Length(Uppers);
  FArrays[ArrayIdx].TotalSize := NewSize;
end;

function TBytecodeVM.ArrayBoundsOK(ArrayIdx, LinearIdx: Integer): Boolean;
begin
  if (LinearIdx >= 0) and (LinearIdx < FArrays[ArrayIdx].TotalSize) then
    Exit(True);
  // Out of bounds. CLASSIC keeps Commodore's ?BAD SUBSCRIPT semantics; --bounds-check forces the raise in
  // any dialect. Otherwise MODERN matches FreeBASIC, which performs no bounds check by default: the caller
  // substitutes a default value on a read and drops the store, keeping us memory-safe (FB would touch
  // adjacent heap). Enable BoundsCheck to turn accidental out-of-bounds accesses back into hard errors.
  if FBoundsCheck or (Assigned(FProgram) and not FProgram.ModernMode) then
    raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
  Result := False;
end;

function ArrayDataShared(const A, B: TArrayStorage): Boolean;
// True if A and B still reference the SAME element-data buffer (a dynamic array shares its reference on
// a struct copy; SetLength/REDIM reallocates and breaks the sharing). Used to detect whether a byref
// array parameter was resized during a call. Compared by the array's element bank.
begin
  case A.ElementType of
    1: Result := Pointer(A.FloatData) = Pointer(B.FloatData);
    2: Result := Pointer(A.StringData) = Pointer(B.StringData);
  else
    Result := Pointer(A.IntData) = Pointer(B.IntData);
  end;
end;

procedure TBytecodeVM.ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ArrayIdx, LinearIdx, i, ProdDims, ArrLowerBound: Integer;
  ArrInfo: TSSAArrayInfo;
  PtrAddr: Int64;
  PtrOffset, RecSlot: Integer;
  Rec: PRecordStorage;
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcArrayLoad (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
          case FArrays[ArrayIdx].ElementType of
            0: Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
            1: Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
            2: Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
          end
        else                                  // MODERN out-of-bounds read -> default (FreeBASIC)
          case FArrays[ArrayIdx].ElementType of
            0: Ctx.IntRegs[Instr.Dest] := 0;
            1: Ctx.FloatRegs[Instr.Dest] := 0.0;
            2: Ctx.StringRegs[Instr.Dest] := '';
          end;
      end;
    1: // bcArrayStore (generic, deprecated)
      begin
        ArrayIdx := Instr.Src1;
        if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then
          raise ERangeError.CreateFmt('Array not allocated: %d', [ArrayIdx]);
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then   // MODERN out-of-bounds store is dropped (FreeBASIC)
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
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.IntRegs[Instr.Dest] := 0;   // MODERN out-of-bounds read -> default (FreeBASIC)
      end;
    4: // bcArrayLoadFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.FloatRegs[Instr.Dest] := 0.0;   // MODERN out-of-bounds read -> default
      end;
    5: // bcArrayLoadString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[LinearIdx];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, False, LinearIdx);
          {$ENDIF}
        end
        else
          Ctx.StringRegs[Instr.Dest] := '';   // MODERN out-of-bounds read -> default
      end;
    6: // bcArrayStoreInt
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then     // MODERN out-of-bounds store is dropped (FreeBASIC)
        begin
          FArrays[ArrayIdx].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
      end;
    7: // bcArrayStoreFloat
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          FArrays[ArrayIdx].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
      end;
    8: // bcArrayStoreString
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          FArrays[ArrayIdx].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
          {$IFDEF ENABLE_PROFILER}
          if Assigned(FProfiler) and FProfiler.Enabled then
            FProfiler.OnArrayAccess(ArrayIdx, True, LinearIdx);
          {$ENDIF}
        end;
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
    12: // bcArrayRedim - REDIM [PRESERVE] arr([lb TO] ub) (B1.4); Src2=ub reg. Immediate: bit0=preserve,
        // bit1=has explicit lower bound, bits8+ = that (non-negative) lower bound.
      RedimArray(Instr.Src1, Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0,
                 (Instr.Immediate and 2) <> 0, Instr.Immediate shr 8);
    // FreeBASIC pointer dereference. Two pointer kinds share these ops, discriminated by bit 63: a
    // record-field pointer (RECPTR_TAG set, so PtrAddr < 0) addresses ResolveRec(handle)^.Data[slot];
    // otherwise the packed address holds (arrayId+1) in the high bits (0 = NULL) and the element offset
    // in the low POINTER_ARRAY_SHIFT bits, addressing FArrays[arrayId].Data[offset] (offset 0 for a
    // scalar's 1-element backing). Load: Dest=value, Src1=address. Store: Src1=address, Src2=value.
    13: // bcRefLoadInt
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Ctx.IntRegs[Instr.Dest] := Rec^.IntData[RecSlot];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].IntData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[PtrOffset];
        end;
      end;
    14: // bcRefLoadFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Ctx.FloatRegs[Instr.Dest] := Rec^.FloatData[RecSlot];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].FloatData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[PtrOffset];
        end;
      end;
    15: // bcRefLoadString
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Ctx.StringRegs[Instr.Dest] := Rec^.StringData[RecSlot];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].StringData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          Ctx.StringRegs[Instr.Dest] := FArrays[ArrayIdx].StringData[PtrOffset];
        end;
      end;
    16: // bcRefStoreInt
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Rec^.IntData[RecSlot] := Ctx.IntRegs[Instr.Src2];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].IntData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          FArrays[ArrayIdx].IntData[PtrOffset] := Ctx.IntRegs[Instr.Src2];
        end;
      end;
    17: // bcRefStoreFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Rec^.FloatData[RecSlot] := Ctx.FloatRegs[Instr.Src2];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].FloatData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          FArrays[ArrayIdx].FloatData[PtrOffset] := Ctx.FloatRegs[Instr.Src2];
        end;
      end;
    18: // bcRefStoreString
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Rec^.StringData[RecSlot] := Ctx.StringRegs[Instr.Src2];
        end
        else
        begin
          ArrayIdx := (PtrAddr shr POINTER_ARRAY_SHIFT) - 1;
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) or (PtrOffset > High(FArrays[ArrayIdx].StringData)) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          FArrays[ArrayIdx].StringData[PtrOffset] := Ctx.StringRegs[Instr.Src2];
        end;
      end;
    19: // bcRefAddrField — pack a record-field pointer from a handle (Src1) and slot (Immediate)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];   // record handle (may carry SHARED_REC_FLAG)
        Ctx.IntRegs[Instr.Dest] := RECPTR_TAG or (PtrAddr and SHARED_REC_FLAG) or
          (((PtrAddr and SHARED_REC_MASK) and RECPTR_INDEX_MASK) shl RECPTR_SLOT_BITS) or
          (Int64(Instr.Immediate) and RECPTR_SLOT_MASK);
      end;
    // FreeBASIC raw byte heap (Allocate family).
    20: Ctx.IntRegs[Instr.Dest] := RawAlloc(Ctx.IntRegs[Instr.Src1]);                              // bcRawAlloc
    21: RawFree(Ctx.IntRegs[Instr.Src1]);                                                          // bcRawFree
    22: Ctx.IntRegs[Instr.Dest] := RawRealloc(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);   // bcRawRealloc
    23: Ctx.IntRegs[Instr.Dest] := RawLoadInt(Ctx.IntRegs[Instr.Src1], Instr.Immediate);           // bcRawLoadInt
    24: Ctx.FloatRegs[Instr.Dest] := RawLoadFloat(Ctx.IntRegs[Instr.Src1], Instr.Immediate);       // bcRawLoadFloat
    25: RawStoreInt(Ctx.IntRegs[Instr.Src1], Instr.Immediate, Ctx.IntRegs[Instr.Src2]);            // bcRawStoreInt
    26: RawStoreFloat(Ctx.IntRegs[Instr.Src1], Instr.Immediate, Ctx.FloatRegs[Instr.Src2]);        // bcRawStoreFloat
    31: // bcRawMemCopy - FB_MEMCOPY(dst, src, bytes); Dest receives dst (FB returns the destination)
      begin
        RawMemCopy(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], PtrUInt(Ctx.IntRegs[Instr.Immediate]));
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
      end;
    32: // bcRawMemMove - FB_MEMMOVE(dst, src, bytes); overlap-safe
      begin
        RawMemCopy(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], PtrUInt(Ctx.IntRegs[Instr.Immediate]));
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
      end;
    33: // bcRawClear - CLEAR(dst, value, bytes)
      RawClear(Ctx.IntRegs[Instr.Src1], Byte(Ctx.IntRegs[Instr.Src2]), PtrUInt(Ctx.IntRegs[Instr.Immediate]));
    34: // bcArrayBind - array BYREF param (PHASE 1): save FArrays[Src1] and snapshot the arg FArrays[Immediate],
      begin  // but DEFER the alias to bcArrayBindApply. Two-phase so a batch of binds that swaps arrays
             // (recursive "proc(a(),b())" -> "proc(b(),a())", where param and arg slots coincide) reads every
             // arg from the UNMODIFIED table before any assignment. Src1=param id, Imm=arg id.
        if (Instr.Src1 >= 0) and (Instr.Immediate >= 0) and (Instr.Immediate <= High(FArrays)) then
        begin
          // The param placeholder array is never runtime-DIM'd, so grow FArrays to hold its slot.
          if Instr.Src1 > High(FArrays) then SetLength(FArrays, Instr.Src1 + 1);
          if FArrayBindTop >= Length(FArrayBindStack) then
            SetLength(FArrayBindStack, (FArrayBindTop + 1) * 2);
          FArrayBindStack[FArrayBindTop].SlotId := Instr.Src1;
          FArrayBindStack[FArrayBindTop].ArgId := Instr.Immediate;
          FArrayBindStack[FArrayBindTop].Saved := FArrays[Instr.Src1];        // dyn-array fields share by ref
          FArrayBindStack[FArrayBindTop].Snapshot := FArrays[Instr.Immediate]; // the arg, captured now
          Inc(FArrayBindTop);
        end;
      end;
    36: // bcArrayBindApply - commit the top N pending binds (Immediate=N): alias each param slot to its
      begin  // snapshotted arg. All snapshots were captured (in phase 1) from the unmodified table.
        for I := FArrayBindTop - Instr.Immediate to FArrayBindTop - 1 do
          if (I >= 0) and (FArrayBindStack[I].SlotId <= High(FArrays)) then
            FArrays[FArrayBindStack[I].SlotId] := FArrayBindStack[I].Snapshot;  // alias: share the caller's data
      end;
    35: // bcArrayUnbind - restore the last saved FArrays[Src1] (Src1 = param array id).
      begin
        if (FArrayBindTop > 0) and (FArrayBindStack[FArrayBindTop - 1].SlotId = Instr.Src1) then
        begin
          Dec(FArrayBindTop);
          // Propagate the callee's final array back to the caller's slot ONLY if a REDIM [PRESERVE]
          // reallocated the param's storage — detected by its data no longer sharing the reference we
          // snapshotted from the arg at bind time. Without a resize the caller already sees the writes via
          // the shared reference, and copying would be wrong: in deep recursion the arg slot may have been
          // rebound at an outer level (merge sort's swapped a()/b()), so an unconditional copy corrupts it.
          if (FArrayBindStack[FArrayBindTop].ArgId >= 0) and
             (FArrayBindStack[FArrayBindTop].ArgId <= High(FArrays)) and
             (FArrayBindStack[FArrayBindTop].ArgId <> Instr.Src1) and
             not ArrayDataShared(FArrays[Instr.Src1], FArrayBindStack[FArrayBindTop].Snapshot) then
            FArrays[FArrayBindStack[FArrayBindTop].ArgId] := FArrays[Instr.Src1];
          FArrays[Instr.Src1] := FArrayBindStack[FArrayBindTop].Saved;
          // Release the saved/snapshot copies' references (ownership transferred back to the live slots).
          SetLength(FArrayBindStack[FArrayBindTop].Saved.IntData, 0);
          SetLength(FArrayBindStack[FArrayBindTop].Saved.FloatData, 0);
          SetLength(FArrayBindStack[FArrayBindTop].Saved.StringData, 0);
          SetLength(FArrayBindStack[FArrayBindTop].Snapshot.IntData, 0);
          SetLength(FArrayBindStack[FArrayBindTop].Snapshot.FloatData, 0);
          SetLength(FArrayBindStack[FArrayBindTop].Snapshot.StringData, 0);
        end;
      end;
    27: // bcArrayRedimPush - push one upper bound onto the pending REDIM dimension list
      begin
        SetLength(FRedimPendingUBs, Length(FRedimPendingUBs) + 1);
        FRedimPendingUBs[High(FRedimPendingUBs)] := Ctx.IntRegs[Instr.Src1];
      end;
    28: // bcArrayRedimN - commit a multi-dimensional REDIM using the pushed upper bounds
      begin
        RedimArrayN(Instr.Src1, FRedimPendingUBs, (Instr.Immediate and 1) <> 0);
        SetLength(FRedimPendingUBs, 0);
      end;
    29: // bcArrayIdxPush - push one (already lower-bound-adjusted) index for a runtime multi-dim access
      begin
        SetLength(FIdxPending, Length(FIdxPending) + 1);
        FIdxPending[High(FIdxPending)] := Ctx.IntRegs[Instr.Src1];
      end;
    30: // bcArrayIdxResolve - linear row-major index from the array's CURRENT dimensions: Dest=int, Src1=array id.
        // Matches the compile-time formula Σ idx[d] * (Π Dimensions[d+1..]) but with runtime sizes (REDIM).
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := 0;
        if (ArrayIdx >= 0) and (ArrayIdx < Length(FArrays)) then
          for i := 0 to High(FIdxPending) do
          begin
            ProdDims := 1;
            for ArrLowerBound := i + 1 to High(FArrays[ArrayIdx].Dimensions) do
              ProdDims := ProdDims * FArrays[ArrayIdx].Dimensions[ArrLowerBound];
            LinearIdx := LinearIdx + FIdxPending[i] * ProdDims;
          end;
        Ctx.IntRegs[Instr.Dest] := LinearIdx;
        SetLength(FIdxPending, 0);
      end;
    // --- UDT array members: indirect access, array handle read from a register (Src1). A handle < 1
    //     means the member was never allocated (REDIM not yet run): reads yield the default, stores drop. ---
    37: // bcArrayLoadIndInt
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].IntData[LinearIdx]
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    38: // bcArrayLoadIndFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.FloatRegs[Instr.Dest] := FArrays[PtrAddr].FloatData[LinearIdx]
        else
          Ctx.FloatRegs[Instr.Dest] := 0.0;
      end;
    39: // bcArrayLoadIndString
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.StringRegs[Instr.Dest] := FArrays[PtrAddr].StringData[LinearIdx]
        else
          Ctx.StringRegs[Instr.Dest] := '';
      end;
    40: // bcArrayStoreIndInt (Dest = value register, READ)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
      end;
    41: // bcArrayStoreIndFloat (Dest = value register, READ)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
      end;
    42: // bcArrayStoreIndString (Dest = value register, READ)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
      end;
    43: // bcArrayIdxResolveInd - member multi-dim linear index from the handle array's CURRENT dimensions
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        LinearIdx := 0;
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) then
          for i := 0 to High(FIdxPending) do
          begin
            ProdDims := 1;
            for ArrLowerBound := i + 1 to High(FArrays[PtrAddr].Dimensions) do
              ProdDims := ProdDims * FArrays[PtrAddr].Dimensions[ArrLowerBound];
            LinearIdx := LinearIdx + FIdxPending[i] * ProdDims;
          end;
        Ctx.IntRegs[Instr.Dest] := LinearIdx;
        SetLength(FIdxPending, 0);
      end;
    44: // bcMemberArrayRedim - REDIM obj.field(...): allocate the member's FArrays entry lazily, size it
      begin
        Rec := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1]);
        RecSlot := (Instr.Immediate shr 8) and $FFFF;   // field int-slot within the record
        PtrOffset := (Instr.Immediate shr 4) and $F;    // element type (0=int, 1=float, 2=string)
        if Assigned(Rec) then
        begin
          PtrAddr := Rec^.IntData[RecSlot];
          if (PtrAddr < 1) or (PtrAddr > High(FArrays)) then
          begin
            if Length(FArrays) = 0 then SetLength(FArrays, 1);   // keep id 0 reserved as the "unallocated" sentinel
            PtrAddr := Length(FArrays);
            SetLength(FArrays, PtrAddr + 1);
            FArrays[PtrAddr].ElementType := PtrOffset;
            FArrays[PtrAddr].DimCount := 0;
            FArrays[PtrAddr].TotalSize := 0;
            SetLength(FArrays[PtrAddr].Dimensions, 0);
            SetLength(FArrays[PtrAddr].LowerBounds, 0);
            Rec^.IntData[RecSlot] := PtrAddr;
          end;
          RedimArrayN(PtrAddr, FRedimPendingUBs, (Instr.Immediate and 1) <> 0);
        end;
        SetLength(FRedimPendingUBs, 0);
      end;
    45: // bcArrayLBoundInd - LBOUND of a UDT array member (Src1=handle reg, Src2=dim reg)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and
           (LinearIdx >= 0) and (LinearIdx <= High(FArrays[PtrAddr].LowerBounds)) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].LowerBounds[LinearIdx]
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    46: // bcArrayUBoundInd - UBOUND of a UDT array member (upper = lower + size - 1; -1 if unallocated)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1]; LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and
           (LinearIdx >= 0) and (LinearIdx <= High(FArrays[PtrAddr].Dimensions)) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].LowerBounds[LinearIdx]
                                     + FArrays[PtrAddr].Dimensions[LinearIdx] - 1
        else
          Ctx.IntRegs[Instr.Dest] := -1;
      end;
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
        end;
      end;
    16: // bcPrintBool (B1.5): a BOOLEAN prints as "true"/"false"
      begin
        if Ctx.IntRegs[Instr.Src1] <> 0 then PrintStr := 'true' else PrintStr := 'false';
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          Inc(Ctx.CursorCol, Length(PrintStr));
        end;
      end;
    17: // bcPrintUInt (B1.5): print an Int64 as an unsigned 64-bit value
      begin
        PrintStr := FConsoleBehavior.FormatUInt(QWord(Ctx.IntRegs[Instr.Src1]));
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          Inc(Ctx.CursorCol, Length(PrintStr));
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
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
          Inc(Ctx.CursorRow);  // CSRLIN: advance to next text row on a print newline
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
    11: // bcCsrlin - return current text cursor row (VM-tracked, parallels POS/CursorCol)
      Ctx.IntRegs[Instr.Dest] := Ctx.CursorRow;
    12: // bcLoadDS - Commodore disk status code = last file-operation error code (0 = OK)
      Ctx.IntRegs[Instr.Dest] := Ctx.LastErrorCode;
    13: // bcLoadDSS - Commodore disk status message line "NN, MESSAGE,00,00"
      Ctx.StringRegs[Instr.Dest] := DiskStatusString;
    14: // bcLoadST - Kernal I/O status byte (bit 6 = EOF on the last GET#)
      Ctx.IntRegs[Instr.Dest] := FIOStatus;
  else
    raise Exception.CreateFmt('Unknown special variable opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.SetupGfxScreen(W, H, NumPages: Integer);
// SCREENRES / SCREEN: resize the screen surface and (re)build the page table (page 0 = screen, pages
// 1..n-1 = same-size image surfaces). Resets the work/visible page to 0. Shared by both opcodes.
var
  i: Integer;
begin
  if not Assigned(FGraphics) then Exit;
  FGraphics.ResizeScreen(W, H, 0);
  for i := 1 to High(FGfxPages) do
    if FGfxPages[i] <> GFX_SCREEN_SURFACE then FGraphics.DestroySurface(FGfxPages[i]);
  if NumPages < 1 then NumPages := 1;
  SetLength(FGfxPages, NumPages);
  FGfxPages[0] := GFX_SCREEN_SURFACE;
  for i := 1 to NumPages - 1 do
    FGfxPages[i] := FGraphics.CreateSurface(W, H, $000000FF);
  FGfxWorkPage := 0;
  FGfxVisiblePage := 0;
  FGfxWorkSurface := GFX_SCREEN_SURFACE;
end;

function TBytecodeVM.GfxMapX(LX: Double): Integer;
// Map a logical x to a physical x: WINDOW transform (identity when off) then the VIEW viewport offset.
begin
  if FGfxWinActive then Result := Round(FGfxWinAx * LX + FGfxWinBx) else Result := Round(LX);
  Result := Result + FGfxViewOffsetX;
end;

function TBytecodeVM.GfxMapY(LY: Double): Integer;
begin
  if FGfxWinActive then Result := Round(FGfxWinAy * LY + FGfxWinBy) else Result := Round(LY);
  Result := Result + FGfxViewOffsetY;
end;

procedure TBytecodeVM.ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  DrawMode: Integer;
  PalColor: UInt32;
  GetX1, GetY1, GetX2, GetY2, GetSx, GetSy, SwapTmp: Integer;
  WinX1, WinY1, WinX2, WinY2, WinW, WinH: Integer;
  JoyBtns, JoyDev, JoyLocal, JoyBtnIdx: Integer;
  JoyAx: array[0..7] of Single;
  JoyV: Single;
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
    21: // bcScnClr - SCNCLR [mode] / CLS: clear screen and home the cursor (POS/CSRLIN -> 0)
      begin
        if Assigned(FOutputDevice) then
          FOutputDevice.ClearScreen(Ctx.IntRegs[Instr.Src1]);
        Ctx.CursorCol := 0;
        Ctx.CursorRow := 0;
      end;
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
    // FreeBASIC graphics (phase 1 slice) routed through the IGraphicsBackend abstraction.
    25: // bcGfxScreenRes - SCREENRES w, h [, , numpages]  (Immediate = number of pages, default 1)
      SetupGfxScreen(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], Instr.Immediate);
    26: // bcGfxPset - PSET (x,y), color  (color in Immediate float-free int register; targets the work page)
      if Assigned(FGraphics) then
      begin
        FGraphics.SetPixel(FGfxWorkSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
                           UInt32(Ctx.IntRegs[Instr.Immediate]));
        FDrawPenX := Ctx.IntRegs[Instr.Src1]; FDrawPenY := Ctx.IntRegs[Instr.Src2];  // becomes the current graphics point
      end;
    27: // bcGfxPoint - POINT(x, y) -> color  (reads the work page)
      if Assigned(FGraphics) then
        Ctx.IntRegs[Instr.Dest] := Int64(FGraphics.GetPixel(FGfxWorkSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2])))
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    28: // bcGfxPaint - PAINT (x,y), color  (flood fill; color in the Immediate int register)
      if Assigned(FGraphics) then
        FGraphics.Fill(FGfxWorkSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
                       UInt32(Ctx.IntRegs[Instr.Immediate]));
    29: // bcGfxLine - LINE (x1,y1)-(x2,y2),color[,B|BF]  (endpoints mapped through the WINDOW transform)
      if Assigned(FGraphics) then
      begin
        GetX2 := Ctx.IntRegs[(Instr.Immediate) and $FFFF];          // logical x2
        GetY2 := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];   // logical y2
        // NOSTART (bit 50): "LINE -(x2,y2)" omits the start -> use the current graphics point.
        if (Instr.Immediate shr 50) and 1 <> 0 then
        begin
          GetX1 := FDrawPenX; GetY1 := FDrawPenY;
        end
        else
        begin
          GetX1 := Ctx.IntRegs[Instr.Src1]; GetY1 := Ctx.IntRegs[Instr.Src2];
        end;
        case (Instr.Immediate shr 48) and $3 of
          1: FGraphics.DrawRect(FGfxWorkSurface, GfxMapX(GetX1), GfxMapY(GetY1),
               GfxMapX(GetX2), GfxMapY(GetY2),
               UInt32(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]), False, 1, 0.0);    // B  = box outline
          2: FGraphics.DrawRect(FGfxWorkSurface, GfxMapX(GetX1), GfxMapY(GetY1),
               GfxMapX(GetX2), GfxMapY(GetY2),
               UInt32(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]), True, 1, 0.0);     // BF = filled box
        else
          FGraphics.DrawLine(FGfxWorkSurface, GfxMapX(GetX1), GfxMapY(GetY1),
            GfxMapX(GetX2), GfxMapY(GetY2),
            UInt32(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]), 1);                   // plain line
        end;
        FDrawPenX := GetX2; FDrawPenY := GetY2;   // the end point becomes the current graphics point
      end;
    30: // bcGfxCircle - CIRCLE (x,y),r[,color]  (centre mapped; radius scaled by the x-axis WINDOW scale)
      if Assigned(FGraphics) then
      begin
        if FGfxWinActive then GetX1 := Round(Ctx.IntRegs[Instr.Immediate and $FFFF] * Abs(FGfxWinAx))
        else GetX1 := Ctx.IntRegs[Instr.Immediate and $FFFF];                              // physical radius
        FGraphics.DrawEllipse(FGfxWorkSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
          GetX1, GetX1,
          UInt32(Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF]), 0.0, 360.0, 0.0, 0.0, 1);
        FDrawPenX := Ctx.IntRegs[Instr.Src1]; FDrawPenY := Ctx.IntRegs[Instr.Src2];  // centre becomes the current point
      end;
    31: // bcGfxPalette - PALETTE index, r,g,b  (Src1=index, Src2=packed RGBA colour)
      if Assigned(FGraphics) and (Ctx.IntRegs[Instr.Src1] >= 0) and (Ctx.IntRegs[Instr.Src1] <= 255) then
        FGraphics.SetPaletteColor(TPaletteIndex(Ctx.IntRegs[Instr.Src1]), UInt32(Ctx.IntRegs[Instr.Src2]));
    32: // bcGfxPalGet - __PALGET(index, which) -> 0-255 component (Dest=result, Src1=index, Immediate=which)
      if Assigned(FGraphics) and (Ctx.IntRegs[Instr.Src1] >= 0) and (Ctx.IntRegs[Instr.Src1] <= 255) then
      begin
        // Engine palette is ABGR ($AABBGGRR): red = low byte, blue = bits 16-23.
        PalColor := UInt32(FGraphics.GetPaletteColor(TPaletteIndex(Ctx.IntRegs[Instr.Src1])));
        case Instr.Immediate of
          0: Ctx.IntRegs[Instr.Dest] := PalColor and $FF;           // red
          1: Ctx.IntRegs[Instr.Dest] := (PalColor shr 8) and $FF;   // green
        else Ctx.IntRegs[Instr.Dest] := (PalColor shr 16) and $FF;  // blue
        end;
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    33: // bcGfxPaletteReset - PALETTE (no args)
      if Assigned(FGraphics) then
        FGraphics.ResetPalette;
    34: // bcGfxColor - COLOR [fg][,bg] : set current draw colours (Immediate bit0=hasFg, bit1=hasBg)
      begin
        if (Instr.Immediate and 1) <> 0 then FGfxForeColor := UInt32(Ctx.IntRegs[Instr.Src1]);
        if (Instr.Immediate and 2) <> 0 then FGfxBackColor := UInt32(Ctx.IntRegs[Instr.Src2]);
      end;
    35: // bcGfxForeColor - read the current draw colour (Immediate 0 = foreground, 1 = background). The
        //   omitted-colour default for PSET (foreground) and PRESET (background).
      if Instr.Immediate = 1 then
        Ctx.IntRegs[Instr.Dest] := Int64(FGfxBackColor)
      else
        Ctx.IntRegs[Instr.Dest] := Int64(FGfxForeColor);
    36: // bcGfxImageCreate - IMAGECREATE(w,h[,color]) -> handle (Immediate = fill colour reg)
      if Assigned(FGraphics) then
        Ctx.IntRegs[Instr.Dest] := Int64(FGraphics.CreateSurface(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2],
                                          UInt32(Ctx.IntRegs[Instr.Immediate])))
      else
        Ctx.IntRegs[Instr.Dest] := GFX_INVALID_SURFACE;
    37: // bcGfxImageDestroy - IMAGEDESTROY handle
      if Assigned(FGraphics) then
        FGraphics.DestroySurface(Ctx.IntRegs[Instr.Src1]);
    38: // bcGfxImageInfo - __IMGINFO(handle, which): width (0) / height (1)
      if Assigned(FGraphics) then
      begin
        if Instr.Immediate = 0 then
          Ctx.IntRegs[Instr.Dest] := FGraphics.SurfaceWidth(Ctx.IntRegs[Instr.Src1])
        else
          Ctx.IntRegs[Instr.Dest] := FGraphics.SurfaceHeight(Ctx.IntRegs[Instr.Src1]);
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    39: // bcGfxGet - GET (x1,y1)-(x2,y2),dst : capture a screen rect into image dst (per-pixel copy)
      if Assigned(FGraphics) then
      begin
        GetX1 := Ctx.IntRegs[Instr.Src1];
        GetY1 := Ctx.IntRegs[Instr.Src2];
        GetX2 := Ctx.IntRegs[Instr.Immediate and $FFFF];
        GetY2 := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];
        DrawMode := Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF];   // dst image handle (reuse DrawMode var)
        if GetX2 < GetX1 then begin SwapTmp := GetX1; GetX1 := GetX2; GetX2 := SwapTmp; end;
        if GetY2 < GetY1 then begin SwapTmp := GetY1; GetY1 := GetY2; GetY2 := SwapTmp; end;
        for GetSy := 0 to (GetY2 - GetY1) do
          for GetSx := 0 to (GetX2 - GetX1) do
            FGraphics.SetPixel(DrawMode, GetSx, GetSy,
              FGraphics.GetPixel(FGfxWorkSurface, GetX1 + GetSx, GetY1 + GetSy));
      end;
    40: // bcGfxPut - PUT (x,y),src[,mode] : blit image src onto the work page (Immediate[0-15]=src handle
        //  register, Immediate[16-31]=mode ordinal constant)
      if Assigned(FGraphics) then
        FGraphics.Blit(FGfxWorkSurface, Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2],
                       Ctx.IntRegs[Instr.Immediate and $FFFF], TGfxBlitMode((Instr.Immediate shr 16) and $FFFF));
    41: // bcGfxScreenInfo - __SCRINFO(which): screen w/h/depth/bpp/pitch/rate
      if Assigned(FGraphics) then
        case Instr.Immediate of
          0: Ctx.IntRegs[Instr.Dest] := FGraphics.SurfaceWidth(FGraphics.ScreenSurface);
          1: Ctx.IntRegs[Instr.Dest] := FGraphics.SurfaceHeight(FGraphics.ScreenSurface);
          2: Ctx.IntRegs[Instr.Dest] := 32;                                             // colour depth (bits)
          3: Ctx.IntRegs[Instr.Dest] := 4;                                              // bytes per pixel
          4: Ctx.IntRegs[Instr.Dest] := FGraphics.SurfaceWidth(FGraphics.ScreenSurface) * 4;  // pitch (bytes)
        else Ctx.IntRegs[Instr.Dest] := 0;                                              // refresh rate (unknown)
        end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    42: // bcGfxScreenSet - SCREENSET work[,visible] / FLIP (page selection; Immediate bit0=hasWork,
        //  bit1=hasVisible, bit2=swap). Drawing follows the work page; the visible page is shown on sbv.
      begin
        if (Instr.Immediate and 4) <> 0 then       // FLIP with no args: swap work and visible pages
        begin
          SwapTmp := FGfxWorkPage; FGfxWorkPage := FGfxVisiblePage; FGfxVisiblePage := SwapTmp;
        end
        else
        begin
          if (Instr.Immediate and 1) <> 0 then FGfxWorkPage := Ctx.IntRegs[Instr.Src1];
          if (Instr.Immediate and 2) <> 0 then FGfxVisiblePage := Ctx.IntRegs[Instr.Src2];
        end;
        if (FGfxWorkPage < 0) or (FGfxWorkPage > High(FGfxPages)) then FGfxWorkPage := 0;
        if (FGfxVisiblePage < 0) or (FGfxVisiblePage > High(FGfxPages)) then FGfxVisiblePage := 0;
        FGfxWorkSurface := FGfxPages[FGfxWorkPage];
        // sbv: showing a non-zero visible page on screen is deferred (headless tracks it for SCREENCOPY).
      end;
    43: // bcGfxPCopy - PCOPY src,dst / SCREENCOPY [src][,dst] : copy one page onto another (full-surface
        //  blit). Immediate bit0=hasSrc, bit1=hasDst; omitted src defaults to the work page, dst to visible.
      if Assigned(FGraphics) then
      begin
        if (Instr.Immediate and 1) <> 0 then GetX1 := Ctx.IntRegs[Instr.Src1] else GetX1 := FGfxWorkPage;     // src page
        if (Instr.Immediate and 2) <> 0 then GetY1 := Ctx.IntRegs[Instr.Src2] else GetY1 := FGfxVisiblePage;  // dst page
        if (GetX1 >= 0) and (GetX1 <= High(FGfxPages)) and (GetY1 >= 0) and (GetY1 <= High(FGfxPages)) and (GetX1 <> GetY1) then
          FGraphics.Blit(FGfxPages[GetY1], 0, 0, FGfxPages[GetX1], gbmPSet);
      end;
    44: // bcGfxWindow - WINDOW [SCREEN] (x1,y1)-(x2,y2): set/clear the logical coordinate transform
      if Assigned(FGraphics) then
      begin
        if ((Instr.Immediate shr 32) and 1) = 0 then
          FGfxWinActive := False                                    // no bounds -> identity
        else
        begin
          WinX1 := Ctx.IntRegs[Instr.Src1];
          WinY1 := Ctx.IntRegs[Instr.Src2];
          WinX2 := Ctx.IntRegs[Instr.Immediate and $FFFF];
          WinY2 := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];
          WinW := FGraphics.SurfaceWidth(FGfxWorkSurface);
          WinH := FGraphics.SurfaceHeight(FGfxWorkSurface);
          if (WinX2 <> WinX1) and (WinY2 <> WinY1) and (WinW > 1) and (WinH > 1) then
          begin
            FGfxWinAx := (WinW - 1) / (WinX2 - WinX1);
            FGfxWinBx := -WinX1 * FGfxWinAx;
            if ((Instr.Immediate shr 33) and 1) = 1 then
            begin
              // WINDOW SCREEN: y1 = top, y2 = bottom (no flip)
              FGfxWinAy := (WinH - 1) / (WinY2 - WinY1);
              FGfxWinBy := -WinY1 * FGfxWinAy;
            end
            else
            begin
              // WINDOW (default): y1 = bottom, y2 = top (y flipped)
              FGfxWinAy := -(WinH - 1) / (WinY2 - WinY1);
              FGfxWinBy := (WinH - 1) - WinY1 * FGfxWinAy;
            end;
            FGfxWinActive := True;
          end
          else
            FGfxWinActive := False;
        end;
      end;
    45: // bcGfxPMap - __PMAP(coord, n): map between logical and physical coordinates (incl. VIEW offset)
      case Instr.Immediate of
        0: Ctx.IntRegs[Instr.Dest] := GfxMapX(Ctx.IntRegs[Instr.Src1]);   // logical x -> physical x
        1: Ctx.IntRegs[Instr.Dest] := GfxMapY(Ctx.IntRegs[Instr.Src1]);   // logical y -> physical y
        2: if FGfxWinActive and (FGfxWinAx <> 0) then                      // physical x -> logical x
             Ctx.IntRegs[Instr.Dest] := Round((Ctx.IntRegs[Instr.Src1] - FGfxViewOffsetX - FGfxWinBx) / FGfxWinAx)
           else Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] - FGfxViewOffsetX;
      else
        if FGfxWinActive and (FGfxWinAy <> 0) then                        // physical y -> logical y
          Ctx.IntRegs[Instr.Dest] := Round((Ctx.IntRegs[Instr.Src1] - FGfxViewOffsetY - FGfxWinBy) / FGfxWinAy)
        else Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] - FGfxViewOffsetY;
      end;
    46: // bcGfxView - VIEW [SCREEN] (x1,y1)-(x2,y2): set/clear the viewport (offset + clip on the work page)
      if Assigned(FGraphics) then
      begin
        if ((Instr.Immediate shr 32) and 1) = 0 then
        begin
          FGfxViewOffsetX := 0; FGfxViewOffsetY := 0;          // reset -> full screen, no offset
          FGraphics.SetClip(FGfxWorkSurface, False, 0, 0, 0, 0);
        end
        else
        begin
          WinX1 := Ctx.IntRegs[Instr.Src1];
          WinY1 := Ctx.IntRegs[Instr.Src2];
          WinX2 := Ctx.IntRegs[Instr.Immediate and $FFFF];
          WinY2 := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];
          FGraphics.SetClip(FGfxWorkSurface, True, WinX1, WinY1, WinX2, WinY2);
          if ((Instr.Immediate shr 33) and 1) = 1 then
          begin
            FGfxViewOffsetX := 0; FGfxViewOffsetY := 0;        // VIEW SCREEN: absolute coordinates
          end
          else
          begin
            // VIEW (default): coordinates relative to the viewport's top-left corner
            if WinX1 <= WinX2 then FGfxViewOffsetX := WinX1 else FGfxViewOffsetX := WinX2;
            if WinY1 <= WinY2 then FGfxViewOffsetY := WinY1 else FGfxViewOffsetY := WinY2;
          end;
        end;
      end;
    47: // bcGfxScreen - SCREEN mode [, , num_pages]: numbered graphics mode -> resolution (QB/FB table)
      begin
        case Ctx.IntRegs[Instr.Src1] of
          1, 7:  begin WinW := 320; WinH := 200; end;
          2, 8:  begin WinW := 640; WinH := 200; end;
          9, 10: begin WinW := 640; WinH := 350; end;
          11, 12, 18: begin WinW := 640; WinH := 480; end;
          13:    begin WinW := 320; WinH := 200; end;
          14:    begin WinW := 320; WinH := 240; end;
          15:    begin WinW := 400; WinH := 300; end;
          16:    begin WinW := 512; WinH := 384; end;
          17:    begin WinW := 640; WinH := 400; end;
          19:    begin WinW := 800; WinH := 600; end;
          20:    begin WinW := 1024; WinH := 768; end;
          21:    begin WinW := 1280; WinH := 1024; end;
        else
          WinW := 0; WinH := 0;   // mode 0 / unknown: no graphics mode change (v1)
        end;
        if (WinW > 0) and (WinH > 0) then
          SetupGfxScreen(WinW, WinH, Instr.Immediate);
      end;
    48: // bcMultikey - MULTIKEY(scancode): -1 if held, 0 otherwise (real-time, via the input provider)
      if Assigned(GKeyDownProvider) and GKeyDownProvider(Ctx.IntRegs[Instr.Src1]) then
        Ctx.IntRegs[Instr.Dest] := -1
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    49: // bcGetmouse - snapshot the mouse into the cache; Dest = status (0 ok, 1 no mouse / off-window).
      begin
        if Assigned(GGetMouseProvider) and
           GGetMouseProvider(FMouseX, FMouseY, FMouseWheel, FMouseButtons) then
        begin
          FMouseClip := 0;                 // clip status not tracked in v1
          Ctx.IntRegs[Instr.Dest] := 0;    // success
        end
        else
        begin
          // No provider (headless) or mouse off-window: FB sets every field to -1 and returns 1.
          FMouseX := -1; FMouseY := -1; FMouseWheel := -1; FMouseButtons := -1; FMouseClip := -1;
          Ctx.IntRegs[Instr.Dest] := 1;    // failure
        end;
      end;
    50: // bcMouseAxis - read a cached mouse component (Immediate: 0=x,1=y,2=wheel,3=buttons,4=clip).
      case Instr.Immediate of
        0: Ctx.IntRegs[Instr.Dest] := FMouseX;
        1: Ctx.IntRegs[Instr.Dest] := FMouseY;
        2: Ctx.IntRegs[Instr.Dest] := FMouseWheel;
        3: Ctx.IntRegs[Instr.Dest] := FMouseButtons;
        4: Ctx.IntRegs[Instr.Dest] := FMouseClip;
      else
        Ctx.IntRegs[Instr.Dest] := -1;
      end;
    51: // bcSetmouse - move/show the mouse; Src1=x, Src2=y, Immediate[0-15]=visibility reg (-1 = no change).
      if Assigned(GSetMouseProvider) then
        GSetMouseProvider(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2],
                          Ctx.IntRegs[Instr.Immediate and $FFFF]);
    52: // bcGetJoystick - snapshot gaming device Src1=id into the cache; Dest = status (0 ok, 1 no device).
      begin
        if Assigned(GGetJoystickProvider) and
           GGetJoystickProvider(Ctx.IntRegs[Instr.Src1], FJoyButtons, @FJoyAxes[0], 8) then
          Ctx.IntRegs[Instr.Dest] := 0    // success
        else
        begin
          // No provider (headless) or device absent: FB sets buttons 0, axes -1000, returns 1.
          FJoyButtons := 0;
          for JoyLocal := 0 to 7 do FJoyAxes[JoyLocal] := -1000.0;
          Ctx.IntRegs[Instr.Dest] := 1;   // failure
        end;
      end;
    53: // bcJoyBtn - cached joystick button bitmask (int).
      Ctx.IntRegs[Instr.Dest] := FJoyButtons;
    54: // bcJoyAxis - cached joystick axis value (Immediate = which 0..7); FLOAT result.
      if (Instr.Immediate >= 0) and (Instr.Immediate <= 7) then
        Ctx.FloatRegs[Instr.Dest] := FJoyAxes[Instr.Immediate]
      else
        Ctx.FloatRegs[Instr.Dest] := -1000.0;
    55: // bcStick - STICK(axis): axis 0..3 (X/Y of device A/B) -> 1..200, or 0 if not attached/absent.
      begin
        JoyDev := Ctx.IntRegs[Instr.Src1] div 2;    // 0,1 -> device A (0); 2,3 -> device B (1)
        JoyLocal := Ctx.IntRegs[Instr.Src1] and 1;  // 0 = X, 1 = Y
        if Assigned(GGetJoystickProvider) and GGetJoystickProvider(JoyDev, JoyBtns, @JoyAx[0], 8) and
           (JoyLocal < 8) and (JoyAx[JoyLocal] > -999.0) then
        begin
          JoyV := JoyAx[JoyLocal];                  // -1..1 -> 1..200 (100.5 = centre)
          Ctx.IntRegs[Instr.Dest] := 1 + Round((JoyV + 1.0) * 99.5);
        end
        else
          Ctx.IntRegs[Instr.Dest] := 0;             // not attached
      end;
    56: // bcStrig - STRIG(button): button 0..7 -> -1 (pressed) / 0. v1 reports the current level for both
        //   the "pressed since" (even) and "is pressed" (odd) queries (no edge latch).
      begin
        // 0,1->devA btn0; 2,3->devB btn0; 4,5->devA btn1; 6,7->devB btn1.
        JoyDev := (Ctx.IntRegs[Instr.Src1] shr 1) and 1;
        JoyBtnIdx := (Ctx.IntRegs[Instr.Src1] shr 2) and 1;
        if Assigned(GGetJoystickProvider) and GGetJoystickProvider(JoyDev, JoyBtns, @JoyAx[0], 8) and
           ((JoyBtns and (1 shl JoyBtnIdx)) <> 0) then
          Ctx.IntRegs[Instr.Dest] := -1
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    57: // bcGfxDrawGML - DRAW "..." : interpret the FreeBASIC graphics macro language (Src1 = string).
      DrawGML(Ctx.StringRegs[Instr.Src1]);
    58: // bcGfxPointCoord - POINTCOORD(n): the DRAW pen coordinate (Src1 selector: 0 = x, 1 = y).
      if Ctx.IntRegs[Instr.Src1] = 1 then
        Ctx.IntRegs[Instr.Dest] := FDrawPenY
      else
        Ctx.IntRegs[Instr.Dest] := FDrawPenX;
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
  BinI: Int64;
  BinF: Double;
  BinLen: Longint;
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

        // The mode string always lives in the register named by Immediate: ProcessDopen (the sole emitter
        // of ssaDopen) always allocates a mode register, defaulting to "R". A previous `Immediate > 0`
        // guard silently dropped the mode whenever register allocation placed it in string register 0
        // (low-pressure programs), so OPEN ... FOR OUTPUT fell back to read and failed on a fresh file.
        // Reading register 0 is safe: it holds the mode, and an empty string still means read.
        Mode := Ctx.StringRegs[Instr.Immediate];
        if Mode = '' then Mode := 'R';

        // Named handles not currently used, clear handle name
        HandleName := '';

        FIOStatus := 0;   // ST (Commodore): a fresh file open clears the I/O status (no EOF yet)
        // Commodore OPEN to a device/command channel (no filename, e.g. OPEN 1,8,15) is a no-op here:
        // there is no drive to command, so opening nothing must not raise.
        if Filename = '' then Exit;
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
          // ST (Commodore): a GET# that returns no byte has hit end-of-file -> set the EOF bit (64).
          if Data = '' then FIOStatus := FIOStatus or 64 else FIOStatus := FIOStatus and not 64;
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

    16: // bcFileQuery - EOF/FREEFILE/LOF/LOC/SEEK(n) -> int (non-fatal; Src1=handle, Immediate=query code)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        case Instr.Immediate of
          1: Mode := 'FREEFILE';
          2: Mode := 'LOF';
          3: Mode := 'LOC';
          4: Mode := 'SEEK';
        else
          Mode := 'EOF';
        end;
        Data := '';
        if Assigned(FOnFileData) then
          FOnFileData(Self, Mode, HandleNum, Data, ErrorCode);   // queries don't raise
        if Instr.Dest >= 0 then
          Ctx.IntRegs[Instr.Dest] := StrToIntDef(Trim(Data), 0);
      end;

    17: // bcSeekSet - SEEK #n, pos: set the 1-based file position
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := IntToStr(Ctx.IntRegs[Instr.Src2]);
        if Assigned(FOnFileData) then
          FOnFileData(Self, 'SEEKSET', HandleNum, Data, ErrorCode);
      end;

    25: // bcFileAttr - FILEATTR(filenum, returntype) -> int (non-fatal; Src1=handle, Src2=returntype).
      begin           // The returntype is passed in via Data; the handler writes the result back to Data.
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := IntToStr(Ctx.IntRegs[Instr.Src2]);
        if Assigned(FOnFileData) then
          FOnFileData(Self, 'FILEATTR', HandleNum, Data, ErrorCode);   // queries don't raise
        if Instr.Dest >= 0 then
          Ctx.IntRegs[Instr.Dest] := StrToIntDef(Trim(Data), 0);
      end;

    26: // bcFileSetEof - FILESETEOF filenum: set the file length to the current position (Src1=handle).
      begin           // The handler truncates/extends and writes a status (0 = success) back to Data.
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := '';
        if Assigned(FOnFileData) then
          FOnFileData(Self, 'FILESETEOF', HandleNum, Data, ErrorCode);
        if Instr.Dest >= 0 then
          Ctx.IntRegs[Instr.Dest] := StrToIntDef(Trim(Data), 0);
      end;

    18: // bcInputFileLine - LINE INPUT# file, string var: read a whole line (commas not split)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '';
          FOnFileData(Self, 'LINEINPUT#', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('LINE INPUT# error %d reading from file: %d', [ErrorCode, HandleNum]);
          if Instr.Dest >= 0 then
            Ctx.StringRegs[Instr.Dest] := Data;
        end
        else
          raise Exception.Create('LINE INPUT# command not supported: no handler assigned');
      end;

    19: // bcPutBinInt - PUT #n: write 8 bytes of an integer (Src1=handle, Src2=int value)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinI := Ctx.IntRegs[Instr.Src2];
        SetLength(Data, 8); Move(BinI, Data[1], 8);
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    20: // bcPutBinFloat - PUT #n: write 8 bytes of a double (Src1=handle, Src2=float value)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinF := Ctx.FloatRegs[Instr.Src2];
        SetLength(Data, 8); Move(BinF, Data[1], 8);
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    21: // bcGetBinInt - GET #n: read 8 bytes into an integer (Dest=int value, Src1=handle)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '8'; FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          if Length(Data) >= 8 then Move(Data[1], BinI, 8) else BinI := 0;
          if Instr.Dest >= 0 then Ctx.IntRegs[Instr.Dest] := BinI;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

    22: // bcGetBinFloat - GET #n: read 8 bytes into a double (Dest=float value, Src1=handle)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '8'; FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          if Length(Data) >= 8 then Move(Data[1], BinF, 8) else BinF := 0;
          if Instr.Dest >= 0 then Ctx.FloatRegs[Instr.Dest] := BinF;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

    23: // bcPutBinStr - PUT #n: write a string as [int32 length][bytes] (Src1=handle, Src2=string value)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinLen := Length(Ctx.StringRegs[Instr.Src2]);
        SetLength(HandleName, 4); Move(BinLen, HandleName[1], 4);
        Data := HandleName + Ctx.StringRegs[Instr.Src2];
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    24: // bcGetBinStr - GET #n: read a length-prefixed string (Dest=string value, Src1=handle)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          Data := '4'; FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          if Length(Data) >= 4 then Move(Data[1], BinLen, 4) else BinLen := 0;
          if BinLen < 0 then BinLen := 0;
          if BinLen > 0 then
          begin
            Data := IntToStr(BinLen);
            FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          end
          else Data := '';
          if Instr.Dest >= 0 then Ctx.StringRegs[Instr.Dest] := Data;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

  else
    raise Exception.CreateFmt('Unknown file I/O opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

{ ========== FILE MANAGEMENT COMMANDS (executed directly in VM) ========== }

procedure TBytecodeVM.ResetErrorStateIfModern(Ctx: TExecutionContext);
begin
  // FreeBASIC: "Err is reset by Resume and Resume Next." Commodore BASIC keeps EL/ER until the
  // next error, so only clear in MODERN.
  if Assigned(FProgram) and FProgram.ModernMode then
  begin
    Ctx.LastErrorCode := 0;
    Ctx.LastErrorLine := 0;
    Ctx.LastErrorMessage := '';
  end;
end;

function TBytecodeVM.DivZeroFloat(Numerator: Double): Double;
begin
  // MODERN (FreeBASIC) follows IEEE-754: a positive numerator over zero is +Inf, a negative one is -Inf,
  // and 0/0 is NaN. The result is built from Math-unit constants (a plain assignment, so it never triggers
  // the FP hardware trap that FPC leaves unmasked). CLASSIC (Commodore v7) raises ?DIVISION BY ZERO ERROR.
  if Assigned(FProgram) and FProgram.ModernMode then
  begin
    if Numerator > 0.0 then Result := Infinity
    else if Numerator < 0.0 then Result := NegInfinity
    else Result := NaN;
  end
  else
    raise EZeroDivide.Create('Division by zero');
end;

procedure TBytecodeVM.RaiseFileError(const FBMsg: string; FBCode: Integer; const CBMMsg: string; CBMCode: Integer);
begin
  // Dialect selects the error number and message. The except handler in the run loop reads
  // TExecutorException.ErrorCode into ERR (and the message into ERR$), so a caught filesystem
  // error reports the dialect's native code: FreeBASIC numbers in MODERN, Commodore in CLASSIC.
  if Assigned(FProgram) and FProgram.ModernMode then
    raise TExecutorIOException.CreateWithCode(FBMsg, FBCode)
  else
    raise TExecutorIOException.CreateWithCode(CBMMsg, CBMCode);
end;

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
      RaiseFileError('Illegal function call', FBERR_ILLEGAL_CALL,
                     '?DESTINATION MUST BE A DIRECTORY FOR WILDCARDS', ERR_INVALID_ARGUMENT);

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
      RaiseFileError('File not found', FBERR_FILE_NOT_FOUND, '?FILE NOT FOUND', ERR_FILE_NOT_FOUND);
  end
  else
  begin
    // Single file copy
    if not FileExists(Src) then
      RaiseFileError('File not found', FBERR_FILE_NOT_FOUND,
                     '?FILE NOT FOUND: ' + ExtractFileName(Src), ERR_FILE_NOT_FOUND);

    // Determine destination
    if DirectoryExists(Dest) then
      DstFullPath := IncludeTrailingPathDelimiter(Dest) + ExtractFileName(Src)
    else
      DstFullPath := Dest;

    // Check overwrite
    if FileExists(DstFullPath) and not Overwrite then
      RaiseFileError('File I/O error', FBERR_FILE_IO,
                     '?FILE EXISTS: ' + ExtractFileName(DstFullPath), ERR_FILE_ACCESS);

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
            RaiseFileError('File I/O error', FBERR_FILE_IO,
                           '?CANNOT DELETE: ' + SearchRec.Name, ERR_FILE_ACCESS);
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
      RaiseFileError('File not found', FBERR_FILE_NOT_FOUND, '?FILE NOT FOUND', ERR_FILE_NOT_FOUND);
  end;
end;

procedure TBytecodeVM.ExecuteRenameFile(const OldName, NewName: string);
begin
  if not FileExists(OldName) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND,
                   '?FILE NOT FOUND: ' + ExtractFileName(OldName), ERR_FILE_NOT_FOUND);

  if FileExists(NewName) then
    RaiseFileError('File I/O error', FBERR_FILE_IO,
                   '?FILE EXISTS: ' + ExtractFileName(NewName), ERR_FILE_ACCESS);

  if not SysUtils.RenameFile(OldName, NewName) then
    RaiseFileError('File I/O error', FBERR_FILE_IO, '?CANNOT RENAME FILE', ERR_FILE_ACCESS);
end;

procedure TBytecodeVM.ExecuteConcat(const Src, Dest: string);
var
  SrcStream, DstStream: TFileStream;
begin
  // Source must exist
  if not FileExists(Src) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND,
                   '?FILE NOT FOUND: ' + ExtractFileName(Src), ERR_FILE_NOT_FOUND);

  // Destination must exist (we append to it)
  if not FileExists(Dest) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND,
                   '?FILE NOT FOUND: ' + ExtractFileName(Dest), ERR_FILE_NOT_FOUND);

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
    RaiseFileError('File I/O error', FBERR_FILE_IO, '?DIRECTORY EXISTS: ' + Path, ERR_FILE_ACCESS);

  if not ForceDirectories(Path) then
    RaiseFileError('File I/O error', FBERR_FILE_IO, '?CANNOT CREATE DIRECTORY: ' + Path, ERR_FILE_ACCESS);
end;

procedure TBytecodeVM.SetEnvOverride(const NameValue: string);
// SETENVIRON "NAME=value": store a VM-internal environment override (consulted by ENVIRON$ before the OS
// environment). A bare "NAME" with no '=' clears the value. Portable — avoids OS-specific setenv.
var
  eq: Integer;
  nm: string;
begin
  eq := Pos('=', NameValue);
  if eq > 0 then
    FEnvOverrides.Values[Copy(NameValue, 1, eq - 1)] := Copy(NameValue, eq + 1, MaxInt)
  else
  begin
    nm := NameValue;
    FEnvOverrides.Values[nm] := '';
  end;
end;

function TBytecodeVM.RunShellCommand(const Cmd: string): Integer;
// SHELL cmd: run a command through the platform shell (cmd.exe on Windows, /bin/sh elsewhere) and return
// its exit code (-1 if the shell could not be launched). Uses SysUtils.ExecuteProcess (portable).
{$IFDEF WINDOWS}
var
  ComSpec: string;
{$ENDIF}
begin
  Result := -1;
  try
    {$IFDEF WINDOWS}
    ComSpec := GetEnvironmentVariable('COMSPEC');
    if ComSpec = '' then ComSpec := 'cmd.exe';
    Result := ExecuteProcess(ComSpec, ['/C', Cmd], []);
    {$ELSE}
    Result := ExecuteProcess('/bin/sh', ['-c', Cmd], []);
    {$ENDIF}
  except
    on E: Exception do Result := -1;   // shell not found / launch failure
  end;
end;

procedure TBytecodeVM.DrawGML(const S: string);
// Interpret a FreeBASIC DRAW graphics-macro-language string, drawing on the current work surface and
// tracking the pen position (FDrawPenX/Y, read by POINTCOORD). Supported commands (case-insensitive):
//   C n      set the draw colour (raw, as LINE/PSET take it)
//   S n      set scale (n/4; 4 = 1x) applied to the directional/relative distances
//   A n      set angle in quarter-turns (0..3, clockwise)
//   M x,y    move: absolute if unsigned, relative (pen + scaled delta) if the first coord is signed (+/-)
//   U/D/L/R n  draw up/down/left/right by n (default 1)
//   E/F/G/H n  draw the four diagonals by n
//   B prefix blind move (do not draw); N prefix no-update (draw but keep the pen where it was)
// Distances are scaled by S and rotated by A; an absolute M is neither scaled nor rotated.
var
  i: Integer;
  cmd: Char;
  blindP, noUpdateP, sgn, sgnY: Boolean;
  num, my, sc, ang, nx, ny: Integer;
  penColor: UInt32;

  procedure SkipSep;
  begin
    while (i <= Length(S)) and (S[i] in [' ', ';', #9, #10, #13]) do Inc(i);
  end;

  function ReadNum(out val: Integer; out isSigned: Boolean): Boolean;
  var st, s2: Integer;
  begin
    val := 0; isSigned := False; s2 := 1;
    SkipSep;
    if (i <= Length(S)) and ((S[i] = '+') or (S[i] = '-')) then
    begin isSigned := True; if S[i] = '-' then s2 := -1; Inc(i); end;
    st := i;
    while (i <= Length(S)) and (S[i] >= '0') and (S[i] <= '9') do
    begin val := val * 10 + (Ord(S[i]) - Ord('0')); Inc(i); end;
    Result := i > st;
    val := val * s2;
  end;

  function Scaled(d: Integer): Integer;
  begin Result := (d * sc) div 4; end;

  procedure StepPen(dx, dy: Integer);   // draw a scaled+rotated segment from the pen, honouring B/N
  var t, ex, ey: Integer;
  begin
    dx := Scaled(dx); dy := Scaled(dy);
    case ang and 3 of
      1: begin t := dx; dx := -dy; dy := t; end;    // 90 CW (screen y grows down)
      2: begin dx := -dx; dy := -dy; end;           // 180
      3: begin t := dx; dx := dy; dy := -t; end;    // 270
    end;
    ex := FDrawPenX + dx; ey := FDrawPenY + dy;
    if (not blindP) and Assigned(FGraphics) then
      FGraphics.DrawLine(FGfxWorkSurface, GfxMapX(FDrawPenX), GfxMapY(FDrawPenY),
                         GfxMapX(ex), GfxMapY(ey), penColor, 1);
    if not noUpdateP then begin FDrawPenX := ex; FDrawPenY := ey; end;
  end;

begin
  if S = '' then Exit;
  i := 1;
  penColor := FGfxForeColor;
  sc := 4; ang := 0;
  while i <= Length(S) do
  begin
    SkipSep;
    if i > Length(S) then Break;
    blindP := False; noUpdateP := False;
    while (i <= Length(S)) and (UpCase(S[i]) in ['B', 'N']) do
    begin
      if UpCase(S[i]) = 'B' then blindP := True else noUpdateP := True;
      Inc(i);
    end;
    if i > Length(S) then Break;
    cmd := UpCase(S[i]); Inc(i);
    case cmd of
      'C': if ReadNum(num, sgn) then penColor := UInt32(num);
      'S': if ReadNum(num, sgn) then sc := num;
      'A': if ReadNum(num, sgn) then ang := num and 3;
      'U': begin if not ReadNum(num, sgn) then num := 1; StepPen(0, -num); end;
      'D': begin if not ReadNum(num, sgn) then num := 1; StepPen(0,  num); end;
      'L': begin if not ReadNum(num, sgn) then num := 1; StepPen(-num, 0); end;
      'R': begin if not ReadNum(num, sgn) then num := 1; StepPen( num, 0); end;
      'E': begin if not ReadNum(num, sgn) then num := 1; StepPen( num, -num); end;
      'F': begin if not ReadNum(num, sgn) then num := 1; StepPen( num,  num); end;
      'G': begin if not ReadNum(num, sgn) then num := 1; StepPen(-num,  num); end;
      'H': begin if not ReadNum(num, sgn) then num := 1; StepPen(-num, -num); end;
      'M':
        begin
          ReadNum(num, sgn);
          if (i <= Length(S)) and (S[i] = ',') then Inc(i);
          ReadNum(my, sgnY);
          if sgn then begin nx := FDrawPenX + Scaled(num); ny := FDrawPenY + Scaled(my); end
          else begin nx := num; ny := my; end;
          if (not blindP) and Assigned(FGraphics) then
            FGraphics.DrawLine(FGfxWorkSurface, GfxMapX(FDrawPenX), GfxMapY(FDrawPenY),
                               GfxMapX(nx), GfxMapY(ny), penColor, 1);
          if not noUpdateP then begin FDrawPenX := nx; FDrawPenY := ny; end;
        end;
    end;
  end;
end;

procedure TBytecodeVM.ExecuteChdir(const Path: string);
begin
  if not DirectoryExists(Path) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND, '?DIRECTORY NOT FOUND: ' + Path, ERR_FILE_NOT_FOUND);

  if not SetCurrentDir(Path) then
    RaiseFileError('File I/O error', FBERR_FILE_IO, '?CANNOT CHANGE DIRECTORY: ' + Path, ERR_FILE_ACCESS);
end;

procedure TBytecodeVM.ExecuteRmdir(const Path: string);
begin
  if not DirectoryExists(Path) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND, '?DIRECTORY NOT FOUND: ' + Path, ERR_FILE_NOT_FOUND);

  // RemoveDir fails if the directory is not empty (or on permission error) -> File I/O error.
  if not RemoveDir(Path) then
    RaiseFileError('File I/O error', FBERR_FILE_IO, '?CANNOT REMOVE DIRECTORY: ' + Path, ERR_FILE_ACCESS);
end;

procedure TBytecodeVM.ExecuteMoveFile(const Src, Dest: string);
var
  DstFullPath: string;
begin
  if not FileExists(Src) then
    RaiseFileError('File not found', FBERR_FILE_NOT_FOUND,
                   '?FILE NOT FOUND: ' + ExtractFileName(Src), ERR_FILE_NOT_FOUND);

  // Determine destination
  if DirectoryExists(Dest) then
    DstFullPath := IncludeTrailingPathDelimiter(Dest) + ExtractFileName(Src)
  else
    DstFullPath := Dest;

  if FileExists(DstFullPath) then
    RaiseFileError('File I/O error', FBERR_FILE_IO,
                   '?FILE EXISTS: ' + ExtractFileName(DstFullPath), ERR_FILE_ACCESS);

  // Try rename first (works if same volume)
  if not SysUtils.RenameFile(Src, DstFullPath) then
  begin
    // If rename fails (different volumes), copy then delete
    ExecuteCopyFile(Src, DstFullPath, False);
    if not SysUtils.DeleteFile(Src) then
      RaiseFileError('File I/O error', FBERR_FILE_IO, '?CANNOT DELETE SOURCE AFTER MOVE', ERR_FILE_ACCESS);
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
