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
// The dispatch loop is alignment-fragile: unrelated code growth elsewhere in the binary
// moves n-body several % with no change to what the loop executes (C3: 14%; C4: 5%). Pinning
// the alignment makes timings reflect the code rather than where the linker placed it.
// LOOP=128 was tuned on n-body: the interpreter improves monotonically 32->64->128 (C4 delta
// vs C3 5.3%->3.6%->2.8%) and plateaus at 128 (256 is within noise, more padding). It does NOT
// fully close the gap - RunFast is a genuinely different-sized procedure once the AOT helper
// machinery grows, so its loop lands at different offsets. Closing it for good needs the
// dispatch loop in its own unit; see PIANO_B1_AOT_DESIGN §5.8.
{$CODEALIGN PROC=64,LOOP=128,JUMP=16}
{$interfaces CORBA}
{$codepage UTF8}
{$I ConfigFlags.inc}
{$I DebugFlags.inc}
{$I ProfilerFlags.inc}
{$I JitFlags.inc}

interface

uses
  Classes, SysUtils, Math, Variants, StrUtils, DateUtils, RegExpr,
  SedaiBytecodeTypes, SedaiOutputInterface, SedaiSSATypes,
  SedaiConsoleBehavior, SedaiConsoleState, SedaiDebugger, SedaiExecutorErrors,
  SedaiMemoryMapper, SedaiSpriteTypes, SedaiExecutionContext, SedaiDrawQueue,
  SedaiGraphicsBackend, SedaiInputState, SedaiOpcodeTable, SedaiJit, SedaiAot
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

  { Numeric fast path for the file QUERIES (EOF/FREEFILE/LOF/LOC/SEEK), the bcFileQuery arm.
    Returns False when the handler has no numeric answer, and the caller falls back to the string
    protocol above - so a handler that does not implement this keeps working unchanged.

    ⚠️ Why it exists: asking "am I at end of file?" through TFileDataEvent costs TWO string
    allocations and a parse. The command travels as a string and is matched by a chain of string
    compares; the answer travels back as IntToStr(...) and is decoded with StrToIntDef(Trim(...)).
    Measured at 242 ns per Eof() call, 101 ms on reverse-complement, which reads one line at a time
    and asks before every one. }
  TFileQueryEvent = function(Sender: TBytecodeVM; QueryCode, Handle: Integer;
                             out Value: Int64; out ErrorCode: Integer): Boolean of object;

const
  { bcFileQuery.Immediate - the query codes the SSA builder emits (SedaiSSA ~6057). }
  FQ_EOF      = 0;
  FQ_FREEFILE = 1;
  FQ_LOF      = 2;
  FQ_LOC      = 3;
  FQ_SEEK     = 4;

type

  { Event poll callback for keeping UI responsive during VM execution }
  TEventPollCallback = function: Boolean of object;

  { SPRDEF callback: runs the modal sprite editor for the given sprite number.
    Returns True if the VM should stop (e.g. the window was closed). }
  TSpriteEditorCallback = function(SpriteNum: Integer): Boolean of object;

  { The three bank ranges a procedure entered at this PC can clobber, each packed as
    (width shl 32) or base; Int = -1 means "not analysed", use the program-wide width. }
  TProcWidth = record
    WInt: Int64;
    WFloat: Int64;
    WStr: Int64;
  end;

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

  { A thread's private stock of free shared-region indices.

    WHY. The region's free list used to be one global stack behind one global lock, and every New and
    every Delete pushed or popped it. The lock was never the problem — giving it a spin count is worth
    0.9% — the problem is that four threads writing the same few cache lines pay a coherence transfer
    per write. Measured on binary-trees N=16: the same four worker threads burn 21.7 s of CPU spread
    over the cores and 9.5 s pinned to ONE core, where coherence traffic cannot arise. Twelve seconds
    of CPU were cache-line ping-pong, not work.

    A thread takes indices from its own stack and reaches the region only when it runs dry (refill a
    batch, or reserve a fresh block) or overflows (flush a batch back). Nothing else is shared.

    ⚠️ SINGLE-THREADED PROGRAMS STAY BYTE-IDENTICAL, deliberately: with one thread there is one stack,
    so it IS the global one — same LIFO order, same indices, same handles. Fresh blocks are pushed in
    descending order so they pop ascending, exactly as appending one at a time did.

    ⛔ For MULTI-threaded programs the slot a given thread gets does change, and that is admissible
    only because it was never fixed to begin with: two workers allocating concurrently already raced
    for the lock, so which one got index 0 varied run to run under the old design too. Verified, not
    assumed — job/tests/bas/handle_order.bas prints the first handle each worker received, and the
    pair flips between runs on the global-lock build. }
  TRecCache = record
    Count: Integer;
    Owner: Int64;            // generation of the VM these indices belong to; 0 the cache if it differs
    Idx: array[0 .. 1023] of Integer;
  end;
  PRecCache = ^TRecCache;

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
    // Number of workers spawned and not yet finished. Guarded by FWorkerLock. Backstop against a
    // runaway spawn (see MAX_LIVE_WORKERS): a miscompiled @sub once resolved to entry PC 0, so every
    // worker re-ran the whole main program -- including its own THREADCREATE calls -- and the resulting
    // recursive thread explosion saturated the machine. A compiler bug must not be able to do that.
    FLiveWorkers: Integer;
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
    FSharedRecords: TSharedRecArray;
    FSharedRecordCount: Integer;
    FSharedRecFreeList: array of Integer;  // DELETE: indices of freed shared records, reused by NEW
    FSharedRecFreeCount: Integer;
    // DELETE RETIRES the storage instead of destroying it, so NEW can take it back without touching
    // the heap: New + three SetLength + Dispose per node was the largest single cost in binary-trees
    // (231 ns per New/Delete pair, measured). The storage is parked HERE, at the record's own index,
    // and stays there while the slot is free — FSharedRecords[i] goes nil (a freed handle must keep
    // resolving to nil, loudly) while FSharedRecStore[i] keeps the block. Parking it by index rather
    // than on a stack is what lets the free list be a plain list of INTEGERS, which in turn is what
    // lets each thread hold its own (see TRecCache): a thread-local cache of pointers would have to
    // be walked at teardown, one of integers does not.
    FSharedRecStore: TSharedRecArray;
    FVmGeneration: Int64;          // stamps a thread's TRecCache; see GVmGeneration
    FProgReadsScreen: Boolean;     // program calls SCREEN(row, col)
    FProgPeeks: Boolean;           // program PEEKs/POKEs -- only reaches the screen through a mapper
    FRecBlockTake: Integer;        // how many fresh indices a cache reserves at once (grows to REC_CACHE_BATCH)
    FSharedRecLock: TRTLCriticalSection;
    // Every pointer array this table ever OUTGREW, kept alive until the program is unloaded.
    // Growing used to be SetLength, which reallocates: a reader holding the old base could have it
    // freed underneath, and THAT is the only reason looking a handle up ever needed the lock. Retiring
    // the old array instead of freeing it makes the lookup safe without one — a live handle's entry is
    // present and valid in every array from the one current when it was issued onwards.
    FSharedRetired: array of TSharedRecArray;
    FSharedRecLockFree: Boolean;   // gate: SHAREDREC_LOCK=1 restores the per-access lock (A/B)
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
    // Decode-once dense dispatch (VM perf plan, milestone M2): the 16-bit (group.sub) opcode of each
    // instruction, translated ONCE to its dense linear index (Op16ToDense). The hot loop dispatches on
    // this instead of extracting the group every instruction. Rebuilt when the loaded program changes;
    // the on-file bytecode and TBytecodeInstruction.OpCode are left untouched (format unchanged).
    FDenseOps: array of Word;
    FDenseOpsFor: TBytecodeProgram;   // the program FDenseOps was built for (rebuild guard)
    {$IFDEF JIT_PROFILE}
    // JIT hot-loop profiling (milestone J1): per-instruction count of how often a BACKWARD branch
    // targets that PC. A loop back-edge is a branch to a lower PC, so a high count marks a hot loop
    // header -- the trigger the JIT will use to decide what to compile. Sized with FDenseOps (per
    // program). Compile-time gated (JitFlags.inc) so a normal build's hot path is untouched.
    FBackEdgeCount: array of Integer;
    FJitProfile: Boolean;
    {$ENDIF}
    // JIT (J2/J3): when enabled, eligible hot loops are compiled to native at load. FNativeLoops[PC] is
    // the compiled loop whose HEADER is at PC (nil if none / not compilable). The interpreter, reaching
    // such a PC, calls the native function which runs the whole loop and returns the exit PC.
    FJitEnabled: Boolean;
    FNativeLoops: array of TExecMem;
    // AOT (plan B): whole SSA functions compiled to native, registered under their ENTRY PC.
    // The dispatch check fires on the iteration after bcCallSub has already done FramePush,
    // so the native function needs no frame handling; it returns the resume PC (bcReturnSub
    // or a deopt PC). Populated by RegisterAotFunc from the host after the bytecode passes.
    FAotEnabled: Boolean;
    FNativeFuncs: array of TExecMem;
    // Per-PC: does an AOT region already cover this instruction? Only meaningful in the combined
    // profile; empty when --aot is off.
    FAotCovered: array of Boolean;
    // Per-procedure frame-clobber widths, indexed by procedure entry PC (built by
    // BuildProcFrameWidths in LoadProgram). Length 0 = not built -> program-wide width.
    // Each entry packs BOTH ends of the range the callee can touch, (width shl 32) or base, with -1
    // for "unknown - use the program-wide width". The banks are shared by the whole program and a
    // procedure's registers are numbered above its caller's, so the part worth saving is a RANGE:
    // starting at 0 copies registers the callee provably cannot reach. Packed rather than kept in
    // three more arrays because FramePush is hot enough that three extra loads per call cost more
    // than the copies the narrower range saves.
    // One record per instruction rather than three parallel arrays: FramePush reads all three ends
    // for the SAME index, so parallel arrays meant three field loads, three Length() checks and
    // three cache lines to answer one question. Int stays first: it is the field the "is this entry
    // known?" test reads.
    FProcWidths: array of TProcWidth;
    // Per entry PC: the integer range a RELOCATABLE procedure occupies, packed (hi shl 32) or lo;
    // -1 = this procedure keeps the copying frame. Built by BuildProcFrameBases.
    FProcFrameBase: array of Int64;
    // FAST-RELOCATION table. The common case of a relocated call - a callee that touches neither the
    // float nor the string bank - needs exactly two numbers, and reading them used to cost four
    // lookups across three arrays (FProcFrameBase, then FProcWidths' three width fields, each with
    // its own Length check). Here they are precomputed into ONE Int64 per entry PC: the frame WIDTH
    // in the high half, its lowest register index in the low half; -1 = not eligible for the fast
    // path, take the general one. FramePush/FramePop were 65.7 cycles of a 205-cycle call while
    // copying nothing at all - the cost was the bookkeeping, not the bytes.
    FFrameFast: array of Int64;
    // True only when EVERY call target in the program has a fast frame. The fast call primitive
    // falls back to the general one for a callee that has not, and that fallback costs a second
    // jump per call - measured +5.9% on fibf, whose recursive function copies floats. So the
    // specialised primitive is installed only where nothing will take the fallback: all-or-nothing
    // per program, which keeps fib's -21.5% without making anyone else pay for it.
    FAllCalleesFast: Boolean;
    // Filled only when FRAMEMARK=0, to reproduce the historic three-array lookup on ONE binary.
    FProcWidthInt: array of Int64;
    FProcWidthFloat: array of Int64;
    FProcWidthStr: array of Int64;
    // Per CALL SITE (indexed by the bcCallSub's own PC), the integer registers this caller still
    // needs when the callee returns, packed the same way; -1 = not analysed, no caller-side
    // narrowing. Intersected with the callee footprint above: only registers in BOTH need saving.
    FCallLiveInt: array of Int64;
    // Array descriptor table passed to compiled loops: 3 Int64 per array (IntData ptr, FloatData ptr,
    // Count). Rebuilt from FArrays only when the array set changes (FArraysDirty), so the per-call cost
    // is a single pointer once the arrays are DIM'd.
    FJitArrDesc: array of Int64;
    FArraysDirty: Boolean;
    FOutputDevice: IOutputDevice;
    FGraphics: IGraphicsBackend;     // FreeBASIC graphics phase: operation-level drawing backend (SW headless / SDL2 on sbv)
    FOwnedGraphics: TObject;         // concrete backend object the VM owns and frees (e.g. the software backend on sb)
    FGfxForeColor: UInt32;           // current FreeBASIC draw foreground (COLOR fg); omitted-colour default
    FGfxBackColor: UInt32;           // current FreeBASIC draw background (COLOR ,bg)
    // The same COLOR statement, remembered as the TEXT attribute the console form of FreeBASIC's
    // "Color()" reads back (foreground in the low word, background in the high word). Kept apart from
    // the draw colours because their DEFAULTS differ and both defaults are observable: a fresh fbc
    // console reports 7 on 0, while an untouched draw colour is white on black.
    FConColorFg: Int64;
    FConColorBg: Int64;
    FGfxWorkSurface: Integer;        // FreeBASIC page flipping: cached surface all draw ops target (= FGfxPages[FGfxWorkPage])
    // FreeBASIC "PSET img,(x,y)" etc.: a per-statement drawing target. When active, drawing ops (PSET/LINE/
    // CIRCLE/PAINT/POINT) target this image surface instead of the work page. Set/cleared by bcGfxSetTarget.
    FGfxDrawTargetActive: Boolean;
    FGfxDrawTargetHandle: Integer;
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
    // FreeBASIC DIR: ONE directory walk is open at a time, exactly as in fbc - "Dir(spec, mask)" starts
    // it, "Dir()" steps it, and it ends when the entries run out (fbc has no handle to close). The
    // attributes of the entry just returned are kept here too, because DIR reports them through a byref
    // argument and we read them back with a second opcode instead.
    // FreeBASIC variadic arguments (CVA_*): the SURPLUS arguments of a variadic call, one FRAME per
    // call. The callee walks a frame with an ordinary integer cursor, which is why CVA_LIST lowers to
    // an Integer, CVA_COPY to a copy and CVA_END to nothing. A slot carries its BANK because the
    // caller knows the type and the callee names it again in CVA_ARG - and the two may disagree, as
    // they may in C, so the read converts.
    FVarArgs: array of record IntVal: Int64; FloatVal: Double; StrVal: string; Bank: Byte; end;
    FVarArgFrames: array of Integer;   // stack of frame bases; the top one is what CVA_START answers
    FDirRec: TSearchRec;
    FDirOpen: Boolean;
    FDirMask: Integer;
    FDirAttr: Int64;
    // PRINT# per-handle output column, for the in-file comma zones (fbc pads the FILE to
    // 14-column boundaries). Grown on demand; a newline resets its handle's column.
    FFilePrintCols: array of Integer;
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
    FRedimPendingLBs: array of Integer;   // REDIM "lb TO ub" with a RUNTIME lb: lower bounds accumulated (immediate flag on the push)
    FIdxPending: array of Int64;          // runtime multi-dim index: indices accumulated by bcArrayIdxPush, consumed by bcArrayIdxResolve

    // UDT/record heap is per-context (FCtx.Records / FCtx.RecordCount) since M5.2b.
    // DATA pool for DATA/READ/RESTORE statements (the read cursor FCtx.DataIndex is per-context)
    FDataPool: array of Variant;
    // PUDEF format characters (filler, comma, decimal, dollar)
    FPudefFiller: Char;
    FPudefComma: Char;
    FPudefDecimal: Char;
    FPudefDollar: Char;
    // Staged (already-stringified) values for a runtime-format PRINT USING (bcPrintUsingStage/Run).
    FPUStage: array of string;
    // File command callback (LOAD, SAVE from program)
    FOnFileCommand: TFileCommandEvent;
    // Disk file I/O callback (DOPEN, DCLOSE, etc.)
    FOnDiskFile: TDiskFileEvent;
    // File data I/O callback (GET#, INPUT#, PRINT#, CMD)
    FOnFileData: TFileDataEvent;
    FOnFileQuery: TFileQueryEvent;   // optional numeric fast path for bcFileQuery
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
    FAudioStartTried: Boolean;   // lazy init: device open attempted (failure is not retried per-op)
    FAudioBackend: TSedaiAudioBackend;   // SAF audio backend
    FSIDEvo: TSedaiSIDEvo;       // SID emulator for advanced audio
    FAudioTempo: Integer;        // Current tempo (0-255, default 8)
    FAudioEnvelopes: array[0..9] of record  // 10 envelope slots
      Attack, Decay, Sustain, Release: Single;
      Waveform: Integer;
      PulseWidth: Single;
    end;
    procedure EnsureAudioStarted; // open the device on the FIRST audio op, not at VM creation
    procedure ExecutePlayString(Ctx: TExecutionContext; const MusicStr: string);
    procedure CooperativeSleep(Ctx: TExecutionContext; Milliseconds: Integer);
    {$ENDIF}
    function ComputeBuiltinFP(OpId: Int64; X: Double): Double;   // @Sin etc.: math builtin taken as a funcptr
    procedure ExecuteInstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSuperinstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    function GfxMapX(LX: Double): Integer;   // FreeBASIC WINDOW: logical x -> physical x
    function GfxMapY(LY: Double): Integer;   // FreeBASIC WINDOW: logical y -> physical y
    function DrawSurface: Integer;           // FreeBASIC per-statement image draw target (else the work page)
    procedure SetupGfxScreen(W, H, NumPages: Integer);  // SCREENRES/SCREEN: resize + (re)build pages
    // Group-specific dispatch handlers
    procedure ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    // Dialect-aware bounds test for a flat element index. Returns True when in range. Out of bounds:
    // CLASSIC (Commodore ?BAD SUBSCRIPT) or an explicit --bounds-check raises; MODERN (FreeBASIC, which
    // does not bounds-check) returns False so the caller yields a default on read / skips the write.
    function ArrayBoundsOK(ArrayIdx, LinearIdx: Integer): Boolean; inline;
    procedure EraseArray(ArrayIdx: Integer; Deallocate: Boolean = False);      // B1.4: ERASE (deallocate = dynamic array)
    procedure RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean; HasNewLower: Boolean = False; NewLower: Integer = 0);  // B1.4: REDIM (1-D)
    procedure RedimArrayN(ArrayIdx: Integer; const Uppers: array of Integer; Preserve: Boolean; const Lowers: array of Integer); // REDIM multi-dim

    procedure AdvancePrintCol(Ctx: TExecutionContext; Chars: Integer);   // printed text advances the cursor -- and the cursor WRAPS at the right margin
    procedure ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpecialVarOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSoundOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpriteOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteFileIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$IFDEF WEB_MODE}
    procedure ExecuteWebOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$ENDIF}
    // Build FDenseOps for the current program if it is not already current (VM perf plan M2).
    procedure EnsureDenseOps;
    // JIT (J2/J3): compile every eligible hot loop of the current program to native (called from
    // EnsureDenseOps when FJitEnabled). Loops with an unsupported opcode are left to the interpreter.
    procedure BuildJitLoops;
    // JIT (J3): refresh the array descriptor table from FArrays (base pointers + counts).
    procedure RebuildJitArrDesc;
    // Raise a dialect-aware filesystem runtime error: FreeBASIC error number + message in MODERN,
    // Commodore error number + '?...' message in CLASSIC. The code reaches ERR via the except handler.
    procedure RaiseFileError(const FBMsg: string; FBCode: Integer; const CBMMsg: string; CBMCode: Integer);
    // FreeBASIC resets Err/Erl after RESUME / RESUME NEXT; Commodore keeps EL/ER. Reset only in MODERN.
    procedure ResetErrorStateIfModern(Ctx: TExecutionContext);
    // Dialect-aware float division by (near-)zero. FreeBASIC (MODERN) follows IEEE-754: x/0 -> +/-Inf,
    // 0/0 -> NaN. Commodore BASIC (CLASSIC) raises ?DIVISION BY ZERO ERROR. Given the numerator, returns
    // the IEEE result in MODERN or raises EZeroDivide in CLASSIC. Used at every float-div-by-zero site.
    function DivZeroFloat(Numerator: Double): Double;
    // Dialect-aware square root. FreeBASIC (MODERN) Sqr maps to C sqrt: a negative argument yields NaN
    // (IEEE), it does not trap. Commodore v7 (CLASSIC) raises ?ILLEGAL QUANTITY. Shared by both run
    // loops so the two paths cannot diverge (opt == no-opt).
    function SqrtFloat(X: Double): Double;
    // Dialect-aware natural log. MODERN (FreeBASIC) Log follows C log: Log(0) = -Inf, Log(negative)
    // = NaN, no trap. CLASSIC (Commodore v7) raises ?ILLEGAL QUANTITY. Shared by both run loops.
    function LnFloat(X: Double): Double;
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
    // FreeBASIC function forms of the filesystem commands: same actions as the Execute* statement
    // handlers but NEVER raise - they return the fbc-verified error code instead (Immediate = -1
    // dispatch in ExecuteInstruction).
    function FsChdirCode(const Path: string): Integer;    // 0 ok, -1 failure
    function FsMkdirCode(const Path: string): Integer;    // 0 ok, -1 failure (incl. already exists)
    function FsRmdirCode(const Path: string): Integer;    // 0 ok, -1 failure
    function FsKillCode(const FileName: string): Integer; // 0 ok, 2 not found, 13 delete failed
    function FsCopyFileCode(const Src, Dest: string): Integer; // 0 ok, 1 failure; always overwrites
    procedure InitializeRegisters;
    procedure ClearAllVariables;
    procedure EnsureRegisterCapacity(Ctx: TExecutionContext; RegType: TSSARegisterType; MinIndex: Integer);
    // bcCallSub: snapshot the registers the callee at TargetPC can touch (-1 = unknown target,
    // e.g. an indirect call: falls back to the program-wide width). bcReturnSub restores exactly
    // what was pushed, reading the width back off the frame-width stack.
    procedure FramePush(Ctx: TExecutionContext; TargetPC: Integer = -1; CallPC: Integer = -1);
    procedure FramePop(Ctx: TExecutionContext);
    // Size the integer bank to LogicalCount slots PLUS the relocatable frame region above them,
    // then reset the view to offset 0. Every reallocation of IntRegsMem must go through here, or
    // Ctx.IntRegs is left dangling into the freed block.
    procedure SizeIntBank(Ctx: TExecutionContext; LogicalCount: Integer);
    procedure BuildProcFrameWidths;
    procedure BuildProcFrameBases;
    procedure BuildCallSiteLiveness;
    procedure GrowCallStackIfNeeded(Ctx: TExecutionContext); inline;  // auto-grow return-addr stack (deep recursion)
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
    procedure GrowSharedRecords(NeedLen: Integer);
    function AllocSharedRecord(IntC, FloatC, StrC, TypeId: Integer): Int64;
    function AllocSharedRecordBlock(N, IntC, FloatC, StrC, TypeId: Integer): Int64;  // N consecutive shared records (Callocate block)
    procedure FreeSharedRecord(Handle: Int64);   // DELETE: release a shared record, recycle its slot
    // Resolve a tagged raw pointer to a real address in its region (byte heap or framebuffer), checking
    // that NeedBytes bytes fit. Every raw access goes through it.
    function RawAddr(RawPtr: Int64; NeedBytes: PtrUInt): Pointer;
    // FreeBASIC raw byte heap (Allocate family). All return/take RAWPTR_TAG-tagged byte offsets.
    function RawAlloc(ByteCount: PtrUInt): Int64;
    function StrSAdd(const S: string): Int64;   // SADD(s) -> raw pointer to a NUL-terminated byte copy
    function FormatNumber(Value: Double; const Mask: string): string;  // FORMAT(num, mask) -> formatted string (numeric masks)
    function FormatDateMask(Value: Double; const Mask: string): string;  // FORMAT(serial, mask) -> date/time formatted string
    procedure ImageConvertRowExec(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);  // IMAGECONVERTROW
    function CommandLine(Index: Integer): string;  // COMMAND$(index) -> command-line argument(s)
    function DiskStatusString: string;  // DS$ -> Commodore disk status line "NN, MESSAGE,00,00"
    function FileLength(const Path: string): Int64;   // FILELEN(path) -> file size in bytes (0 if absent)
    function FileDateTimeSerial(const Path: string): Double;  // FILEDATETIME(path) -> last-modified date serial (0 if absent)
    procedure RawFree(RawPtr: Int64);
    function RawRealloc(RawPtr: Int64; ByteCount: PtrUInt): Int64;
    function RawLoadInt(RawPtr: Int64; TypeCode: Integer): Int64;
    function RawLoadFloat(RawPtr: Int64; TypeCode: Integer): Double;
    procedure FilePrintColAdvance(Handle: Integer; const Data: string);
    procedure FilePrintColSet(Handle, Col: Integer);
    function FilePrintColGet(Handle: Integer): Integer;
    function RawLoadZStrVal(RawPtr: Int64; Wide: Boolean): string;      // C string at the raw address, up to NUL
    function RawLoadBytesVal(RawPtr: Int64; Count: Integer): string;    // exactly Count bytes at the raw address
    procedure RawStoreZStrVal(RawPtr: Int64; const S: string; Wide: Boolean);  // chars + NUL at the raw address
    procedure RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
    procedure RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
    procedure RawMemCopy(DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);  // FB_MEMCOPY/FB_MEMMOVE: copy ByteCount bytes on the raw heap
    procedure RawClear(DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);  // CLEAR: set ByteCount bytes to Value on the raw heap
    function ResolveRec(Ctx: TExecutionContext; Handle: Int64): PRecordStorage; inline;
    function RecPtrTarget(Ctx: TExecutionContext; PtrAddr: Int64; out Slot: Integer): PRecordStorage; inline;  // decode @obj.field pointer
    procedure CleanupSharedRecords;   // free the shared region (destructor)
    procedure UpdateScreenModelGate;          // decide whether the modelled screen must be kept
    procedure RecCacheAdopt(C: PRecCache);    // bind this thread's free-index cache to this VM
    procedure RecCacheFlush(C: PRecCache);    // give a batch of free indices back to the region
    procedure RecCacheRefill(C: PRecCache);   // restock a dry cache from the region
    procedure RecordNewArrayInit(Ctx: TExecutionContext; ArrayId: Integer; PackedCounts: Int64);  // M3.1: fill UDT array
    procedure DeepCopyArrayRecords(Ctx: TExecutionContext; DestArr, SrcArr: Int64; PackedCounts: Int64);  // value-copy array-of-UDT member
    procedure CheckFloatValid(Ctx: TExecutionContext; RegIndex: Integer; const OpName: string);
    function FormatUsingString(const FormatStr: string; Value: Double;
      IsInt: Boolean = False; IntValue: Int64 = 0): string;
    function FormatUsingRuntime(const FormatStr: string): string;   // walk a runtime format over FPUStage
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
    {$IFDEF JIT_PROFILE}
    // JIT hot-loop profiling (J1): enable back-edge counting, then dump the hot loops after a run.
    property JitProfile: Boolean read FJitProfile write FJitProfile;
    procedure DumpHotLoops(Threshold: Integer = 1000);
    {$ENDIF}
    // JIT (J2/J3): compile eligible hot loops to native. Set before LoadProgram / run.
    property JitEnabled: Boolean read FJitEnabled write FJitEnabled;
    property AotEnabled: Boolean read FAotEnabled write FAotEnabled;
    // AOT: adopt a compiled function (ownership passes to the VM) under its entry PC.
    procedure RegisterAotFunc(EntryPC: Integer; Mem: TObject; LastPC: Integer = -1);
    { Layout the native back ends need to reach a record field without a helper: the offset of the
      Records dynamic-array FIELD inside TExecutionContext, plus SizeOf(TRecordStorage) and the
      offsets of its IntData/FloatData fields. Only offsets travel, never an address - the emitted
      code loads the current base from the context it is handed, which is what makes one compiled
      function correct for the main context and for a THREADCREATE worker alike (see
      jit-thread-unsafe). The JIT already derives these inline for J13; the AOT gets them here. }
    procedure GetRecordLayout(out RecordsOff, RecSize, RecIntOff, RecFloatOff, SharedRecOff: Integer);
    // AOT: turn a compiled function's return value into the PC to resume at, handling the
    // C3 helper sentinels. Out of line on purpose - see the implementation comment.
    function AotSettle(C: TExecutionContext; R: PtrInt): Integer;
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
    property OnFileQuery: TFileQueryEvent read FOnFileQuery write FOnFileQuery;
    property CmdHandle: Integer read FCmdHandle;  // Current CMD output redirect handle
    {$IFDEF WEB_MODE}
    procedure SetWebContext(AContext: TObject);
    {$ENDIF}
    // Error state for EL, ER, ERR$ system variables
    procedure SetErrorState(ALine, ACode: Integer; const AMessage: string);
    procedure SetErrorProc(const AProcName: string);   // ERFN
    function ReadChars(Count, Handle: Integer; Wide: Boolean): string;  // INPUT() / WINPUT()
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

// DATE LOCALISATION, opt-in and OFF by default.
//
// fbc answers from the SYSTEM LOCALE: on an Italian machine MonthName(11) is "novembre" and
// DateValue accepts "28-11-2005" while rejecting "2005-11-28". We answer DETERMINISTICALLY - English
// names, ISO-ish parsing - because an output that changes with the machine's regional settings cannot
// be diffed, and the test baselines would stop being portable between Windows and Linux.
//
// Both behaviours are legitimate, so this chooses. Deterministic stays the default; locale mode is for
// running unmodified FreeBASIC programs that expect their own regional conventions. It is a RUNTIME
// switch on purpose: an existing FB program contains no SedaiBasic directive, so a source-level option
// could never help it. A source-level OPTION is meant to layer on top of this later - it needs the
// .basc format to carry the setting, or compiling with sbc and running with sb would silently lose it.
//
// Affects: MONTHNAME/WEEKDAYNAME, the "mmm/mmmm/ddd/dddd" masks of FORMAT, and the DATEVALUE/
// TIMEVALUE/ISDATE string parsers (locale first, ISO second).
procedure SetDateLocaleMode(Enabled: Boolean);
function DateLocaleMode: Boolean;

implementation

// Declared here because ExecuteSuperinstruction (bcStrConcatCharAt) calls it well before its
// definition further down, next to AppendString.
procedure AppendChar(var D: AnsiString; C: AnsiChar); forward;

var
  // -1 = not read yet, 0 = deterministic (default), 1 = follow the system locale.
  GDateLocale: Integer = -1;
  // JIT_OVERAOT=1 lets the loop JIT compile loops the AOT already owns (see BuildJitLoops). Default
  // off: the overlap costs a second compilation and buys nothing.
  GJitOverAot: Boolean = False;

procedure SetDateLocaleMode(Enabled: Boolean);
begin
  if Enabled then GDateLocale := 1 else GDateLocale := 0;
end;

function DateLocaleMode: Boolean;
begin
  if GDateLocale < 0 then
    if GetEnvironmentVariable('SB_DATE_LOCALE') = '1' then GDateLocale := 1 else GDateLocale := 0;
  Result := GDateLocale = 1;
end;

function LocaleMonthName(n: Integer; Full: Boolean): string;
// FPC keeps the running locale's names in FormatSettings; index 1..12.
begin
  Result := '';
  if (n < 1) or (n > 12) then Exit;
  if Full then Result := FormatSettings.LongMonthNames[n]
  else Result := FormatSettings.ShortMonthNames[n];
end;

function LocaleDateFields(const S: string; out Y, Mo, D: Integer): Boolean;
// Split a date on '-', '/' or '.' into three integers and assign them in the LOCALE's FIELD ORDER,
// taken from FormatSettings.ShortDateFormat.
//
// TryStrToDate cannot do this job: it insists on the locale's SEPARATOR too, so an Italian machine
// rejects "28-11-2005" while accepting "28/11/2005". fbc accepts both, because what the locale
// decides is the ORDER of the fields, not the punctuation between them.
var
  f: array[0..2] of string;
  n, i, v: array[0..2] of Integer;
  k, p, pd, pm, py: Integer;
  c: Char;
  fmt: string;
begin
  Result := False;
  Y := 0; Mo := 0; D := 0;
  k := 0; f[0] := ''; f[1] := ''; f[2] := '';
  for p := 1 to Length(S) do
  begin
    c := S[p];
    if (c = '-') or (c = '/') or (c = '.') then
    begin
      Inc(k);
      if k > 2 then Exit;
    end
    else
      f[k] := f[k] + c;
  end;
  if k <> 2 then Exit;
  for p := 0 to 2 do
  begin
    v[p] := StrToIntDef(Trim(f[p]), -1);
    if v[p] < 0 then Exit;
  end;
  fmt := LowerCase(FormatSettings.ShortDateFormat);
  pd := Pos('d', fmt); pm := Pos('m', fmt); py := Pos('y', fmt);
  if (pd = 0) or (pm = 0) or (py = 0) then begin pd := 1; pm := 2; py := 3; end;
  // Rank the three positions: n[j] = how many of the others come before field j.
  n[0] := 0; n[1] := 0; n[2] := 0;
  i[0] := pd; i[1] := pm; i[2] := py;
  for p := 0 to 2 do
    for k := 0 to 2 do
      if (k <> p) and (i[k] < i[p]) then Inc(n[p]);
  D := v[n[0]]; Mo := v[n[1]]; Y := v[n[2]];
  Result := True;
end;

function LocaleDayName(n: Integer; Full: Boolean): string;
// n: 1=Sunday..7=Saturday, which is also FPC's indexing.
begin
  Result := '';
  if (n < 1) or (n > 7) then Exit;
  if Full then Result := FormatSettings.LongDayNames[n]
  else Result := FormatSettings.ShortDayNames[n];
end;

function QuietNaN: Double;
// A NaN with the sign bit CLEAR. FPC's NaN constant has it SET (it is the x86 "indefinite" form, which
// is what an invalid operation like 0/0 or Sqr(-1) produces, and what prints as "-1.#IND"). The C
// library's log(-1) returns the sign-clear one, printed "1.#QNAN", and fbc reports it that way too.
begin
  Result := NaN;
  PInt64(@Result)^ := PInt64(@Result)^ and $7FFFFFFFFFFFFFFF;
end;

function FloatToIntConv(V: Double; Modern: Boolean): Int64; inline;
// The IMPLICIT float -> int conversion. FreeBASIC ROUNDS to nearest, ties to even -- verified against
// fbc 1.10.1: 1.5 and 2.5 both convert to 2, 1.7 to 2, -1.5 and -2.5 to -2 -- and it does so wherever
// the conversion is implicit: assignment, argument passing, an array store, an array INDEX, a FOR bound,
// a FUNCTION result. Truncation is what Int() and Fix() are for, and they have their own opcodes.
// Commodore v7 truncates on assignment to an integer variable, so CLASSIC keeps Trunc.
begin
  if Modern then
    Result := Round(V)      // FPC's Round is round-half-to-even, which is what FreeBASIC does
  else
    Result := Trunc(V);
end;

const
  // Ceiling on simultaneously-live THREADCREATE workers. Sized far above any legitimate FreeBASIC
  // program on a desktop core count, and far below what it takes to wedge the host. It exists so that a
  // compiler defect (an @sub whose entry PC resolves wrong, a worker that re-enters the module body)
  // fails the program instead of spawning threads without bound.
  MAX_LIVE_WORKERS = 64;

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

  // Free indices held per thread before anything global is touched (see TRecCache). REC_CACHE_BATCH is
  // how many move at a time when the cache does have to talk to the region, so the global list is
  // touched once per BATCH allocations instead of once per allocation. CAP must match the Idx array.
  REC_CACHE_CAP   = 1024;
  REC_CACHE_BATCH = 512;

var
  // Monotonic VM counter. A cache is stamped with the generation of the VM whose indices it holds,
  // NOT with the VM pointer: a destroyed VM can be replaced by a new one at the same address, and the
  // cache would then hand that VM indices belonging to a region it never had.
  GVmGeneration: Int64 = 0;

threadvar
  // This thread's free-index cache. Take its address ONCE per operation: every mention of a threadvar
  // is a TLS lookup.
  GRecCache: TRecCache;
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
    try
      Spawn.VM.RunWorker(Spawn);
    except
      // A worker must never propagate an exception past the RTL thread boundary (it would abort the
      // process). v1: swallow it — the join still completes. (Proper per-thread error reporting: M5.5.)
    end;
  finally
    // Release this worker's slot against MAX_LIVE_WORKERS even when its body raised.
    EnterCriticalSection(Spawn.VM.FWorkerLock);
    try
      Dec(Spawn.VM.FLiveWorkers);
    finally
      LeaveCriticalSection(Spawn.VM.FWorkerLock);
    end;
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
  FConColorFg := 7;   // fbc's console defaults, which "Color()" reports before any COLOR statement
  FConColorBg := 0;
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
  FLiveWorkers := 0;
  // M5.2: the main context starts at the program EntryPoint (StartPC = -1); workers override it.
  FCtx.StartPC := -1;
  FDrainCtx.StartPC := -1;
  FCtx.ModeSwitchPC := -1;
  FDrainCtx.ModeSwitchPC := -1;
  SetLength(FWorkerThreads, 0);
  InitCriticalSection(FWorkerLock);
  SetLength(FMutexes, 0);
  InitCriticalSection(FMutexTableLock);
  SetLength(FCondVars, 0);
  InitCriticalSection(FCondTableLock);
  SetLength(FSharedRecords, 0);
  SetLength(FSharedRecStore, 0);
  FSharedRecordCount := 0;
  FRecBlockTake := 8;
  Inc(GVmGeneration);
  FVmGeneration := GVmGeneration;
  SetLength(FSharedRetired, 0);
  // Default ON. SHAREDREC_LOCK=1 puts the per-access lock back, so the two can be timed against each
  // other on ONE binary instead of two builds (see ab-needs-a-built-baseline).
  FSharedRecLockFree := GetEnvironmentVariable('SHAREDREC_LOCK') <> '1';
  GJitOverAot := GetEnvironmentVariable('JIT_OVERAOT') = '1';
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
  // -1 = not measured yet, so FramePush falls back to the whole bank. LoadProgram replaces these
  // with the program's real widths, which are legitimately 0 for a bank it never touches -- which
  // is why 0 must NOT read as "unmeasured" (that cost 256 float copies per call in an int-only
  // program, the residual call overhead after the first pass at this).
  FCtx.FrameSaveIntCount := -1;
  FCtx.FrameSaveFloatCount := -1;
  FCtx.FrameSaveStrCount := -1;
  FCtx.FrameMarkTop := 0;
  FCtx.FrameWidthTop := 0;      // FRAMEMARK=0 layout
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
  // Audio device open is LAZY (EnsureAudioStarted, first audio op): opening the backend and
  // starting its callback thread costs tens of ms, paid at every sb launch by programs that
  // never play a note - the whole regression harness included.
  FAudioInitialized := False;
  FAudioStartTried := False;
  FAudioBackend := nil;
  FSIDEvo := nil;

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
var
  JitI: Integer;
begin
  for JitI := 0 to High(FNativeLoops) do FNativeLoops[JitI].Free;   // JIT: release executable pages
  for JitI := 0 to High(FNativeFuncs) do FNativeFuncs[JitI].Free;   // AOT: release executable pages
  FEnvOverrides.Free;
  if FDirOpen then begin FindClose(FDirRec); FDirOpen := False; end;   // a DIR walk the program never finished
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
  // From the VIEW, not the allocation: a relocated frame's operands live at the view's offset.
  SetLength(Cmd.IntRegs, Ctx.IntRegCount);
  if Ctx.IntRegCount > 0 then
    Move(Ctx.IntRegs^, Cmd.IntRegs[0], Ctx.IntRegCount * SizeOf(Int64));
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
    FDrainCtx.IntRegsMem := Items[i].IntRegs;
    if Length(FDrainCtx.IntRegsMem) > 0 then FDrainCtx.IntRegs := @FDrainCtx.IntRegsMem[0]
    else FDrainCtx.IntRegs := nil;
    FDrainCtx.RegDeltaI := 0;                 // a replayed snapshot is already flat
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

{ The relocatable region that sits above the logical integer bank. A frame-base call slides the
  view into it instead of copying the callee's registers out of the way; a call that would run past
  the end falls back to the copying frame, so this is a performance bound, not a correctness one.
  16384 slots = 128 KB, and a small recursive procedure uses a handful of slots per level, so it
  covers recursion far deeper than the AOT's own 1500-level cap. }
const
  FRAME_REGION_SLOTS = 16384;

procedure TBytecodeVM.SizeIntBank(Ctx: TExecutionContext; LogicalCount: Integer);
var i, Total: Integer;
begin
  Ctx.IntRegCount := LogicalCount;
  Total := LogicalCount + FRAME_REGION_SLOTS;
  SetLength(Ctx.IntRegsMem, Total);
  for i := 0 to Total - 1 do Ctx.IntRegsMem[i] := 0;
  Ctx.RegDeltaI := 0;
  Ctx.RegHwI := LogicalCount;      // the first slot no logical register can name
  Ctx.RegFrameCap := Total;
  Ctx.IntRegs := @Ctx.IntRegsMem[0];
end;

procedure TBytecodeVM.InitializeRegisters;
var i: Integer;
begin
  // Initialize with minimum register slots
  FCtx.IntRegCount := MIN_REGISTER_SLOTS;
  FCtx.FloatRegCount := MIN_REGISTER_SLOTS;
  FCtx.StringRegCount := MIN_REGISTER_SLOTS;

  SizeIntBank(FCtx, FCtx.IntRegCount);
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

function TBytecodeVM.FormatUsingString(const FormatStr: string; Value: Double;
  IsInt: Boolean = False; IntValue: Int64 = 0): string;
// When IsInt is set the value is an EXACT 64-bit integer (IntValue), not the Double: a LongInt beyond
// 2^53 (e.g. Pell's 2469645423824185801) prints every digit instead of the Double-rounded 2469645423824185900.
// The Double path is kept for genuine floats and for a "#.##" field that asks for fractional digits.
var
  i, j, TotalWidth, IntDigits, DecDigits, DotPos: Integer;
  HasDollar, FloatDollar, HasCommas, IsNegative: Boolean;
  IntPart, FormattedInt, FormattedDec, RoundedStr: string;
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

  // Round the WHOLE value to DecDigits decimals, then split into integer and fractional parts. Rounding
  // Frac() and Trunc() separately dropped the carry when the fractional part rounded up to all zeros:
  // 11.9999999 gave Frac->"1.000000" (stripped to "000000") + Trunc 11 = "11.000000" instead of
  // "12.000000". Formatting the whole value first carries into the integer part correctly.
  if IsInt then
  begin
    // Exact integer: IntToStr keeps every digit (Int64.Min handled -- take the sign off the string rather
    // than Abs(), which would overflow). A "#.##" field still gets zero fractional digits appended.
    RoundedStr := IntToStr(IntValue);
    IsNegative := (RoundedStr <> '') and (RoundedStr[1] = '-');
    if IsNegative then Delete(RoundedStr, 1, 1);
    if DecDigits > 0 then RoundedStr := RoundedStr + '.' + StringOfChar('0', DecDigits);
  end
  else
  begin
    IsNegative := Value < 0;
    AbsValue := Abs(Value);
    RoundedStr := Format('%.*f', [DecDigits, AbsValue]);   // '.' separator (as the split below assumes)
  end;
  DotPos := Pos('.', RoundedStr);
  if DotPos > 0 then
  begin
    IntPart := Copy(RoundedStr, 1, DotPos - 1);
    FormattedDec := Copy(RoundedStr, DotPos + 1, Length(RoundedStr) - DotPos);
  end
  else
  begin
    IntPart := RoundedStr;
    FormattedDec := '';
  end;
  if DecDigits <= 0 then FormattedDec := '';

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

  // FreeBASIC/QB field-overflow marker: when the value's integer part has MORE digits than the field's "#"
  // positions, it does not fit -- FB prints the number in full, prefixed with '%' ("Print Using ""#""; 10"
  // gives "%10"). The padding below is then a no-op (the result already exceeds the field width).
  if Length(IntPart) > IntDigits then
    Result := '%' + Result;

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

function TBytecodeVM.FormatUsingRuntime(const FormatStr: string): string;
// Interpret a RUNTIME PRINT USING format string over the staged values (FPUStage), mirroring the
// compile-time field engine in ProcessPrintUsing: a "\...\" fixed-width string field, "&" variable-width
// string field, "!" one-character field, a run of #/./,/$/+/-/^ numeric field, and literal text; the
// format is RECYCLED when more values than fields are given. Staged values are strings; a numeric field
// converts via Val(). Consumes and clears FPUStage.
var
  fLen, fi, i, W, vi, nVals, passStart, vcode: Integer;
  FieldStr: string;
  dv: Double;

  function IsNumFieldStart(P: Integer): Boolean;
  var k: Integer; sawHash: Boolean;
  begin
    Result := False;
    if P > fLen then Exit;
    if FormatStr[P] = '#' then Exit(True);
    if FormatStr[P] in ['$', '+', '-'] then
    begin
      sawHash := False; k := P;
      while (k <= fLen) and (FormatStr[k] in ['#', '.', '$', '+', '-', '^', ',']) do
      begin
        if FormatStr[k] = '#' then sawHash := True;
        Inc(k);
      end;
      Result := sawHash;
    end;
  end;

begin
  Result := '';
  fLen := Length(FormatStr);
  nVals := Length(FPUStage);
  vi := 0;
  if fLen = 0 then begin SetLength(FPUStage, 0); Exit; end;
  repeat
    passStart := vi;
    fi := 1;
    while fi <= fLen do
    begin
      // FreeBASIC escape: "_" prints the NEXT character literally, marker or not. MODERN only -- in
      // Commodore v7 PRINT USING there is no escape and "_" prints as itself. (Mirrors ProcessPrintUsing,
      // the compile-time engine: a constant format never reaches this one, so both must agree.)
      if Assigned(FProgram) and FProgram.ModernMode and (FormatStr[fi] = '_') then
      begin
        if fi < fLen then Result := Result + FormatStr[fi + 1];
        Inc(fi, 2);
      end
      else if FormatStr[fi] = '\' then
      begin
        i := fi + 1;
        while (i <= fLen) and (FormatStr[i] <> '\') do Inc(i);
        if i <= fLen then W := i - fi + 1 else W := i - fi;   // include the closing backslash
        fi := i + 1;
        if vi < nVals then
        begin
          Result := Result + Copy(FPUStage[vi] + StringOfChar(' ', W), 1, W);   // left-justify/pad/truncate
          Inc(vi);
        end;
      end
      else if FormatStr[fi] = '&' then
      begin
        Inc(fi);
        if vi < nVals then begin Result := Result + FPUStage[vi]; Inc(vi); end;
      end
      else if FormatStr[fi] = '!' then
      begin
        Inc(fi);
        if vi < nVals then begin Result := Result + Copy(FPUStage[vi], 1, 1); Inc(vi); end;
      end
      else if IsNumFieldStart(fi) then
      begin
        i := fi;
        while (i <= fLen) and (FormatStr[i] in ['#', '.', '$', '+', '-', '^', ',']) do Inc(i);
        FieldStr := Copy(FormatStr, fi, i - fi);
        fi := i;
        if vi < nVals then
        begin
          Val(Trim(FPUStage[vi]), dv, vcode);   // locale-independent ('.' decimal); 0 on bad input
          if vcode <> 0 then dv := 0;
          Result := Result + FormatUsingString(FieldStr, dv);
          Inc(vi);
        end;
      end
      else
      begin
        i := fi;
        while (i <= fLen) and not ((FormatStr[i] in ['\', '&', '!']) or IsNumFieldStart(i) or
                                   (Assigned(FProgram) and FProgram.ModernMode and (FormatStr[i] = '_'))) do Inc(i);
        Result := Result + Copy(FormatStr, fi, i - fi);   // literal text
        fi := i;
      end;
    end;
  until (vi >= nVals) or (vi = passStart);   // all consumed, or a pass with no value-consuming field
  SetLength(FPUStage, 0);
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

procedure TBytecodeVM.GrowCallStackIfNeeded(Ctx: TExecutionContext);
// The return-address stack starts at 256 but deep recursion (e.g. Ackermann) can exceed it. Grow it
// like FramePush grows the register save stacks, so an over-deep call widens the buffer instead of
// writing out of bounds and corrupting adjacent context memory.
begin
  if Ctx.CallStackPtr >= Length(Ctx.CallStack) then
    SetLength(Ctx.CallStack, Length(Ctx.CallStack) * 2 + 16);
end;

{ True for the opcodes PROVEN to read and write nothing but the integer bank.
  Used by the per-procedure frame widths below to stop charging a purely integer procedure the
  float and string widths of the whole program. The polarity is the one OpIsMergeSafe already paid
  for: an opcode missing from this list costs a missed NARROWING, never a missed save - so a new
  opcode is safe by default. Deliberately short: it covers the shapes that dominate call-heavy code
  (integer arithmetic, comparisons, branches, argument transfer and the call itself), and anything
  else keeps the old conservative behaviour.
  bcCallSub is here because the CALLEE's own footprint is folded in separately by the fixpoint
  below - the call instruction itself touches no bank. }
var
  GFrameBankNarrow: Integer = 1;  // FRAMEBANK=0 restores the coarse all-banks footprint
  // One gate for the whole slice: FRAMERANGE=0 restores the historic snapshot exactly - a prefix
  // [0, width) copied element by element. The three parts (write set, range, block move) are not
  // independently useful, so they are not independently switchable: the write set exists to make
  // the range computable, and the block move exists to keep the range's index arithmetic from
  // eating what the range saves.
  GFrameRangeNarrow: Integer = 1;
  // FRAMELIVE=0 drops the caller-side half: the snapshot is then whatever the callee can touch,
  // without asking whether this caller still needs it.
  GFrameLiveNarrow: Integer = 1;
  // FRAMEMARK=0 restores the historic DATA LAYOUT of the frame bookkeeping: five parallel stacks
  // instead of one stack of records, and three parallel per-procedure width arrays instead of one
  // array of records. Semantics are identical either way - this gate exists only so the layout can
  // be A/B'd on ONE binary. It has to be: two separately linked builds differ in code alignment,
  // and measuring this change across two builds moved unrelated benchmarks by 4-12% in the same
  // direction, which is the alignment effect, not the change.
  GFrameMark: Integer = 1;
  // FRAME RELOCATION, per-procedure opt-in: an eligible callee runs on fresh slots above every
  // live frame instead of having its register range copied out of the way. DEFAULT ON;
  // FRAMEBASE=0 restores the copying frame everywhere. See BuildProcFrameBases for the five
  // conditions a procedure has to meet - one that fails any of them simply keeps copying.
  GFrameBase: Integer = 1;
  // FRAMEBASE_DIAG=1 prints, per procedure, whether it is relocatable and if not WHICH condition
  // refused it. The first version of this analysis found nothing at all and looked exactly like a
  // working one from the outside.
  GFrameBaseDiag: Integer = 0;
  // FRAMEBASE_WIDE=0 restores the v1 rule that only a procedure touching NOTHING but the integer
  // bank could be relocated. With it on (default), a procedure that also uses float, string or
  // record opcodes relocates its INTEGER frame and keeps COPYING the other two banks exactly as
  // before - the callee reaches those at absolute addresses, which the existing snapshot already
  // protects. Only the integer view slides, so nothing else about the scheme changes.
  GFrameBaseWide: Integer = 0;
  // FRAMEBANK_SHAPE=0 restores the BINARY float/string width rule (credit both banks for anything
  // not proven integer-only) instead of the per-bank write shapes. For a one-binary A/B.
  GFrameBankShape: Integer = 1;
  // FRAME_FAST=0 disables the precomputed fast-relocation table and its paired FramePop exit, so
  // the same binary runs the general path for every call. For the A/B.
  GFrameFast: Integer = 1;
  // AOT_FASTCALL=0 installs the general native call primitive for every call, as before.
  GAotFastCall: Integer = 1;

function BcTouchesOnlyIntBank(Op: Word): Boolean;
begin
  if GFrameBankNarrow <> 1 then Exit(False);
  case Op of
    bcLoadConstInt, bcCopyInt,
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr,
    bcXferStoreInt, bcXferLoadInt,
    bcJump, bcJumpIfZero, bcJumpIfNotZero, bcNop, bcCallSub, bcReturnSub:
      Result := True;
  else
    Result := False;
  end;
end;

{ True for the opcodes VERIFIED to write no integer register other than Instr.Dest.
  The frame snapshot exists to protect the CALLER's values, so what a callee READS is irrelevant to
  it: a register the callee never writes still holds the caller's value when the callee returns, and
  restoring it would be a copy of itself. (Nor does the callee see anything different: FramePush only
  COPIES the banks aside, it never clears them, so an unwritten register reads the same either way.)
  The width may therefore be the callee's WRITE set instead of every index it mentions.
  Soundness rests on one claim, checked opcode by opcode against the interpreter: none of these
  writes Ctx.IntRegs at an index other than Dest. Same safe polarity as BcTouchesOnlyIntBank above -
  an opcode missing from this list keeps its reads counted, costing a missed narrowing and never a
  missed save. Crediting Dest for the opcodes that do not use it (a jump, a transfer store) costs
  nothing: an absent operand lowers to register 0, which the smallest frame already covers.
  The membership happens to match BcTouchesOnlyIntBank today, but the two answer different questions
  (which BANKS an opcode touches vs. WHERE in the integer bank it writes), so they are kept apart. }
const
  IW_UNKNOWN = 0;   // not audited: credit Dest, Src1 and Src2, as before
  IW_DEST    = 1;   // writes Ctx.IntRegs[Dest], nothing else in the bank
  IW_NONE    = 2;   // writes no integer register at all

function BcIntWriteShapeRaw(Op: Word): Integer;
begin
  case Op of
    bcLoadConstInt, bcCopyInt,
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr,
    bcXferLoadInt,
    // Opcodes whose OTHER operands live in the float or string bank but whose Dest is an integer.
    // A comparison is the shape that matters here: it reads two floats and writes a truth value
    // into the integer bank, so it writes Dest and reads nothing of ours.
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,   // no Le/Ge form exists
    bcFloatToInt, bcFloatRound, bcNarrowInt,
    bcRecordLoadInt, bcRecordTypeId, bcRecordNew, bcRecordNewBlock,
    // Array element and bound reads. The array itself lives in FArrays, a bank of its own that is
    // never relocated, so an array opcode is transparent to the sliding view: only its register
    // operands matter here. Src1 is the array ID (an immediate), Src2 the index register.
    bcArrayLoadInt, bcArrayLBound, bcArrayUBound:
      Result := IW_DEST;
    // Dest is not an operand of these at all. Saying so matters for the LOW end of the range and
    // only there: an absent operand lowers to register 0 ([[absent-operand-lowers-to-r0]]), so
    // crediting it would pin every procedure's base to 0 and undo the narrowing. bcXferStoreInt
    // writes the transfer bank, which is not part of the snapshot; bcCallSub's callee is folded in
    // by the fixpoint below.
    bcXferStoreInt, bcJump, bcJumpIfZero, bcJumpIfNotZero, bcNop, bcCallSub, bcReturnSub,
    // Purely float/string opcodes: their Dest indexes ANOTHER bank, so crediting it here would
    // reserve an integer register that is not one. The banks are numbered in parallel, which is
    // exactly why this has to be said explicitly rather than inferred from the operand fields.
    bcLoadConstFloat, bcLoadConstString, bcCopyFloat, bcCopyString, bcNarrowSingle,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat,
    bcXferLoadFloat, bcXferLoadString, bcXferStoreFloat, bcXferStoreString,
    bcIntToFloat, bcIntToString,
    bcRecordLoadFloat, bcRecordLoadString,
    bcRecordStoreInt, bcRecordStoreFloat, bcRecordStoreString, bcRecordFree,
    bcArrayLoadFloat, bcArrayLoadString,
    bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    // Binding an array BYREF parameter moves entries between FArrays slots and a save stack of its
    // own. bcArrayBind/Unbind/BindApply name their arrays by immediate and touch no register at all;
    // bcArrayBindInd takes the member's runtime handle from Src2.
    bcArrayBind, bcArrayUnbind, bcArrayBindApply, bcArrayBindInd, bcArrayErase,
    // The PRINT family writes no register at all - it formats an operand and sends it to the
    // output device. Classifying it matters more than it looks: an unaudited opcode counts as
    // READING its operands in the INTEGER bank, and the banks are numbered in parallel, so a
    // module-level 'PrintString R4' was making the integer R4 look externally read - which
    // refused relocation for every procedure that writes integer R4 (fibf, and its whole shape).
    // INPUT and SCREEN/LOCATE/VIEW PRINT are deliberately left unaudited: they write registers,
    // and bcConScreen even uses Immediate as a register INDEX.
    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn, bcPrintInt, bcPrintIntLn,
    bcPrintComma, bcPrintSemicolon, bcPrintTab, bcPrintSpc, bcPrintNewLine, bcPrintEnd,
    bcPrintBool, bcPrintUInt:
      Result := IW_NONE;
  else
    Result := IW_UNKNOWN;
  end;
end;

function BcIntWriteShape(Op: Word): Integer;   // gated view, used by the width/base scan
begin
  if GFrameRangeNarrow <> 1 then Result := IW_UNKNOWN else Result := BcIntWriteShapeRaw(Op);
end;

{ The same question as BcIntWriteShapeRaw, asked of the FLOAT and STRING banks: which registers of
  that bank can this opcode write? The frame width is a WRITE-set question - a register the callee
  never writes still holds the caller's value when it returns - so an opcode that writes nothing in
  a bank costs that bank nothing.

  This replaces a BINARY test. Until now the widths asked only "is this opcode integer-only?", and
  anything that was not credited Dest, Src1 and Src2 to the float and string banks TOGETHER. A
  purely float opcode therefore dragged in the STRING bank: a recursive function returning a double
  copied five REFCOUNTED strings per call for strings it never touches, which measured -19.5% on
  fibf(30) when probed away with FRAMESAVE_NOSTR. It is the same defect the per-bank widths fixed
  for integer procedures, left standing for float ones.

  ⚠️ POLARITY - this is the dangerous direction. Saying BW_NONE for an opcode that DOES write the
  bank means the caller's value is not saved and does not come back: a silent miscompile. Every
  entry below is verified against its implementation in RunTemplate.inc. An opcode that is not
  listed answers BW_UNKNOWN and keeps the old, coarse behaviour, which costs a missed narrowing and
  never a missed save. Note what is NOT here: the whole string group ($01xx), all of I/O and
  graphics, and anything else unaudited - those still credit everything, exactly as before. }
const
  BW_UNKNOWN = 0;   // not audited: credit Dest, Src1 and Src2 to this bank, as before
  BW_DEST    = 1;   // writes this bank at Dest, and nowhere else in it
  BW_NONE    = 2;   // writes nothing in this bank at all

function BcFloatWriteShape(Op: Word): Integer;
begin
  // FRAMEBANK_SHAPE=0 restores the binary "integer-only?" test on one binary: everything not
  // proven integer-only answers UNKNOWN and credits all three operands to both banks, as before.
  if (GFrameBankNarrow <> 1) or (GFrameBankShape <> 1) then
  begin
    if BcTouchesOnlyIntBank(Op) then Exit(BW_NONE) else Exit(BW_UNKNOWN);
  end;
  case Op of
    // Dest is a float register.
    bcLoadConstFloat, bcCopyFloat, bcNarrowSingle,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat,
    bcIntToFloat, bcXferLoadFloat, bcRecordLoadFloat, bcArrayLoadFloat:
      Result := BW_DEST;
    // Integer-only work, plus the opcodes that READ a float and write elsewhere: a comparison
    // writes the integer bank, a transfer store writes the transfer bank, an array or record store
    // writes that array or record. None of them leaves a float register changed.
    bcLoadConstInt, bcCopyInt,
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr,
    bcXferStoreInt, bcXferLoadInt, bcJump, bcJumpIfZero, bcJumpIfNotZero, bcNop,
    bcCallSub, bcReturnSub,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    bcFloatToInt, bcFloatRound, bcNarrowInt,
    bcLoadConstString, bcCopyString, bcXferLoadString, bcXferStoreString, bcXferStoreFloat,
    bcIntToString,
    bcRecordNew, bcRecordNewBlock, bcRecordLoadInt, bcRecordLoadString, bcRecordTypeId,
    bcRecordStoreInt, bcRecordStoreFloat, bcRecordStoreString, bcRecordFree,
    bcArrayLoadInt, bcArrayLoadString, bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    bcArrayLBound, bcArrayUBound, bcArrayBind, bcArrayUnbind, bcArrayBindApply,
    bcArrayBindInd, bcArrayErase:
      Result := BW_NONE;
  else
    Result := BW_UNKNOWN;
  end;
end;

function BcStrWriteShape(Op: Word): Integer;
begin
  if (GFrameBankNarrow <> 1) or (GFrameBankShape <> 1) then
  begin
    if BcTouchesOnlyIntBank(Op) then Exit(BW_NONE) else Exit(BW_UNKNOWN);
  end;
  case Op of
    // Dest is a string register. Every one of these is a refcounted assignment, which is why the
    // string bank is the expensive one to get wrong in either direction.
    bcLoadConstString, bcCopyString, bcXferLoadString, bcRecordLoadString, bcArrayLoadString,
    bcIntToString:
      Result := BW_DEST;
    bcLoadConstInt, bcCopyInt,
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt, bcNegInt,
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr,
    bcXferStoreInt, bcXferLoadInt, bcJump, bcJumpIfZero, bcJumpIfNotZero, bcNop,
    bcCallSub, bcReturnSub,
    // The float family, which is the point of this function: none of it touches a string.
    bcLoadConstFloat, bcCopyFloat, bcNarrowSingle,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat,
    bcIntToFloat, bcXferLoadFloat, bcXferStoreFloat,
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,
    bcFloatToInt, bcFloatRound, bcNarrowInt, bcXferStoreString,
    bcRecordNew, bcRecordNewBlock, bcRecordLoadInt, bcRecordLoadFloat, bcRecordTypeId,
    bcRecordStoreInt, bcRecordStoreFloat, bcRecordStoreString, bcRecordFree,
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayStoreInt, bcArrayStoreFloat, bcArrayStoreString,
    bcArrayLBound, bcArrayUBound, bcArrayBind, bcArrayUnbind, bcArrayBindApply,
    bcArrayBindInd, bcArrayErase:
      Result := BW_NONE;
  else
    Result := BW_UNKNOWN;
  end;
end;

{ Which Src fields an opcode READS from the integer bank - the read-side twin of BcIntWriteShape,
  and the one whose polarity is dangerous. Everywhere else in this file an opcode missing from a list
  costs a missed narrowing; here, declaring an operand absent when the opcode really reads it would
  let the liveness below call a register dead while its value is still needed, which is a silent
  miscompile. Every member below is checked against its interpreter implementation, and an opcode
  that is not listed makes its whole procedure ineligible rather than being guessed at. }
const
  US_UNKNOWN = -1;
  US_NONE    = 0;
  US_SRC1    = 1;
  US_SRC2    = 2;
  // Dest is not always a destination. An array store carries the VALUE to be stored in the Dest
  // field and READS it (see bcArrayStoreInt in RunTemplate.inc), so without this axis the only two
  // answers available - "writes Dest" and "touches nothing" - are both wrong, and the second one is
  // wrong in the direction that kills a live register.
  US_DEST    = 4;

function BcIntUseShape(Op: Word): Integer;
begin
  case Op of
    bcLoadConstInt, bcXferLoadInt, bcJump, bcNop, bcCallSub, bcReturnSub,
    // Nothing of these reaches the integer bank: operands and result are all float or string.
    bcLoadConstFloat, bcLoadConstString, bcCopyFloat, bcCopyString, bcNarrowSingle,
    bcAddFloat, bcSubFloat, bcMulFloat, bcDivFloat, bcNegFloat,
    bcXferLoadFloat, bcXferLoadString, bcXferStoreFloat, bcXferStoreString,
    // Read two floats or two strings, write an integer truth value: the reads are not ours.
    bcCmpEqFloat, bcCmpNeFloat, bcCmpLtFloat, bcCmpGtFloat, bcCmpLeFloat, bcCmpGeFloat,
    bcCmpEqString, bcCmpNeString, bcCmpLtString, bcCmpGtString,   // no Le/Ge form exists
    bcFloatToInt, bcFloatRound,
    // Src1/Src2 are the slot COUNTS passed straight to AllocRecord, not register indices.
    bcRecordNew,
    // Arrays named by immediate only.
    bcArrayBind, bcArrayUnbind, bcArrayBindApply, bcArrayErase,
    // PRINT of a float or a string, and the pure layout ops: nothing of ours is read.
    bcPrint, bcPrintLn, bcPrintString, bcPrintStringLn,
    bcPrintComma, bcPrintSemicolon, bcPrintNewLine, bcPrintEnd:
      Result := US_NONE;
    bcCopyInt, bcNegInt, bcBitwiseNot, bcXferStoreInt, bcJumpIfZero, bcJumpIfNotZero,
    bcIntToFloat, bcIntToString, bcNarrowInt,
    // Src1 is the record HANDLE, which lives in the integer bank whatever the field's type is;
    // the slot number is an immediate. Verified one by one against ResolveRec in RunTemplate.inc.
    bcRecordLoadInt, bcRecordLoadFloat, bcRecordLoadString, bcRecordTypeId,
    bcRecordStoreFloat, bcRecordStoreString, bcRecordFree, bcRecordNewBlock,
    // ...and PRINT of an integer value, or a TAB/SPC count, reads it from Src1.
    bcPrintInt, bcPrintIntLn, bcPrintBool, bcPrintUInt, bcPrintTab, bcPrintSpc:
      Result := US_SRC1;
    // Src2 is the element index (or the member handle for BindInd); Src1 is an immediate array id.
    bcArrayLoadInt, bcArrayLoadFloat, bcArrayLoadString,
    bcArrayLBound, bcArrayUBound, bcArrayBindInd,
    // A float or string element store reads its index from Src2 and its VALUE from the other bank.
    bcArrayStoreFloat, bcArrayStoreString:
      Result := US_SRC2;
    // ... but an INTEGER element store reads the value from Dest, in our bank.
    bcArrayStoreInt:
      Result := US_SRC2 or US_DEST;
    bcAddInt, bcSubInt, bcMulInt, bcDivInt, bcModInt,
    bcCmpEqInt, bcCmpNeInt, bcCmpLtInt, bcCmpGtInt, bcCmpLeInt, bcCmpGeInt,
    bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcShl, bcShr,
    bcRecordStoreInt:   // Src1 = handle, Src2 = the integer value being stored
      Result := US_SRC1 or US_SRC2;
  else
    Result := US_UNKNOWN;
  end;
end;

procedure TBytecodeVM.BuildProcFrameWidths;
// A call only has to protect the registers its callee - and everything that callee can reach -
// might touch. Compute that per procedure entry PC: first the highest register index appearing in
// each procedure's own instruction range (reads included, a conservative superset of what it
// writes), then a fixpoint over static bcCallSub targets so a caller covers its callees too. Any
// procedure containing an INDIRECT call gets the program-wide width, since its target is unknown.
// Falls back to the program-wide width everywhere if the proc map is unusable.
var
  NProc, NInstr, i, p, PcStart, PcEnd, Tgt, TgtIdx: Integer;
  Instr: TBytecodeInstruction;
  Op, Grp: Word;
  Entry: array of Integer;          // procedure index -> entry PC
  WI, WF, WS: array of Integer;     // procedure index -> widths (one past the highest index used)
  LI, LF, LS: array of Integer;     // procedure index -> lowest index used (MaxInt = bank untouched)
  Unknown: array of Boolean;        // procedure makes an indirect call -> program-wide
  BaseUnsafe: array of Boolean;     // procedure contains a GOSUB -> its range scan is incomplete
  Changed: Boolean;
  Rounds: Integer;

  function ProcIndexAt(PC: Integer): Integer;   // last entry at or before PC, -1 = module level
  var k: Integer;
  begin
    Result := -1;
    for k := 0 to NProc - 1 do
      if (Entry[k] >= 0) and (Entry[k] <= PC) then Result := k else if Entry[k] > PC then Break;
  end;
  procedure Note(pi: Integer; Bank: Integer; RegIdx: Integer);
  begin
    if (pi < 0) or (RegIdx < 0) then Exit;
    case Bank of
      0: begin
           if RegIdx + 1 > WI[pi] then WI[pi] := RegIdx + 1;
           if RegIdx < LI[pi] then LI[pi] := RegIdx;
         end;
      1: begin
           if RegIdx + 1 > WF[pi] then WF[pi] := RegIdx + 1;
           if RegIdx < LF[pi] then LF[pi] := RegIdx;
         end;
      2: begin
           if RegIdx + 1 > WS[pi] then WS[pi] := RegIdx + 1;
           if RegIdx < LS[pi] then LS[pi] := RegIdx;
         end;
    end;
  end;

begin
  SetLength(FProcWidths, 0);
  SetLength(FProcWidthInt, 0); SetLength(FProcWidthFloat, 0); SetLength(FProcWidthStr, 0);
  if FProgram = nil then Exit;
  NInstr := FProgram.GetInstructionCount;
  NProc := FProgram.GetProcMapCount;
  if (NInstr = 0) or (NProc = 0) then Exit;

  SetLength(Entry, NProc); SetLength(WI, NProc); SetLength(WF, NProc); SetLength(WS, NProc);
  SetLength(LI, NProc); SetLength(LF, NProc); SetLength(LS, NProc);
  SetLength(Unknown, NProc); SetLength(BaseUnsafe, NProc);
  for p := 0 to NProc - 1 do
  begin
    Entry[p] := FProgram.GetProcMapStart(p);
    WI[p] := 0; WF[p] := 0; WS[p] := 0; Unknown[p] := False; BaseUnsafe[p] := False;
    LI[p] := MaxInt; LF[p] := MaxInt; LS[p] := MaxInt;
  end;

  // Pass 1: own footprint per procedure. The register banks an opcode touches are not uniform, so
  // instead of re-deriving per-opcode operand shapes (the trap that would silently under-save), be
  // deliberately coarse: credit Dest/Src1/Src2 to EVERY bank. Widths stay well under the
  // program-wide maximum for small procedures, which is where the win is, and can never under-save.
  for p := 0 to NProc - 1 do
  begin
    PcStart := Entry[p];
    // NB: System.Continue, not Continue -- inside a TBytecodeVM method the bare name resolves to
    // the class's own Continue method (the BASIC CONT command), which raises "CAN'T CONTINUE".
    if PcStart < 0 then begin Unknown[p] := True; System.Continue; end;
    if p + 1 < NProc then PcEnd := FProgram.GetProcMapStart(p + 1) - 1 else PcEnd := NInstr - 1;
    if PcEnd >= NInstr then PcEnd := NInstr - 1;
    for i := PcStart to PcEnd do
    begin
      Instr := FProgram.GetInstruction(i);
      Op := Instr.OpCode;
      Grp := Op shr 8;
      // The integer bank stays fully conservative. The float and string banks are credited only
      // for opcodes NOT proven integer-only: charging a purely integer procedure the program's
      // string width is what made fib copy five REFCOUNTED strings per call for strings it never
      // touches - 54 of its ~214 cycles of frame snapshot, measured with AOT_CALLPROF.
      // Integer bank: the callee's WRITE set where that is proven (see BcIntWriteShape), every
      // index mentioned otherwise.
      case BcIntWriteShape(Op) of
        IW_DEST: Note(p, 0, Instr.Dest);
        IW_NONE: ;
      else
        begin
          Note(p, 0, Instr.Dest);
          Note(p, 0, Instr.Src1);
          Note(p, 0, Instr.Src2);
        end;
      end;
      // Float and string banks: same write-set question as the integer bank above, asked per bank.
      // This used to be one BINARY test - "integer-only?" - that credited both banks together, so a
      // purely float opcode dragged in the STRING width and a recursive function returning a double
      // copied five refcounted strings per call it never touched (-19.5% on fibf when probed away).
      case BcFloatWriteShape(Op) of
        BW_DEST: Note(p, 1, Instr.Dest);
        BW_NONE: ;
      else
        begin Note(p, 1, Instr.Dest); Note(p, 1, Instr.Src1); Note(p, 1, Instr.Src2); end;
      end;
      case BcStrWriteShape(Op) of
        BW_DEST: Note(p, 2, Instr.Dest);
        BW_NONE: ;
      else
        begin Note(p, 2, Instr.Dest); Note(p, 2, Instr.Src1); Note(p, 2, Instr.Src2); end;
      end;
      // An indirect call, a thread spawn or an error jump can land anywhere: give up on this one.
      if (Op = Ord(bcCallSubIndirect)) or (Op = Ord(bcThreadCreate)) or
         (Op = Ord(bcOnError)) or (Op = Ord(bcResumeLabel)) or (Op = Ord(bcResume)) or
         (Op = Ord(bcTrap)) or (Grp = $05) then
        Unknown[p] := True;
      // A GOSUB shares the caller's register frame and its body can sit OUTSIDE this procedure's
      // instruction range, so the range scan never sees what it writes. That is survivable for the
      // widths (the program-wide ceiling still applies below the maximum) but not for the base,
      // which would skip low registers the GOSUB body writes: keep base 0 for such a procedure.
      if (Op = Ord(bcCall)) or (Op = Ord(bcReturn)) then
        BaseUnsafe[p] := True;
    end;
    if BaseUnsafe[p] then begin LI[p] := 0; LF[p] := 0; LS[p] := 0; end;
  end;

  // Pass 2: fixpoint over static call edges - a caller must cover everything its callees touch.
  Rounds := 0;
  repeat
    Changed := False;
    Inc(Rounds);
    for p := 0 to NProc - 1 do
    begin
      PcStart := Entry[p];
      if PcStart < 0 then System.Continue;
      if p + 1 < NProc then PcEnd := FProgram.GetProcMapStart(p + 1) - 1 else PcEnd := NInstr - 1;
      if PcEnd >= NInstr then PcEnd := NInstr - 1;
      for i := PcStart to PcEnd do
      begin
        Instr := FProgram.GetInstruction(i);
        if Instr.OpCode <> Ord(bcCallSub) then System.Continue;
        Tgt := Instr.Immediate;
        TgtIdx := ProcIndexAt(Tgt);
        if (TgtIdx < 0) or (Entry[TgtIdx] <> Tgt) then begin
          if not Unknown[p] then begin Unknown[p] := True; Changed := True; end;
          System.Continue;
        end;
        if Unknown[TgtIdx] and not Unknown[p] then begin Unknown[p] := True; Changed := True; end;
        if WI[TgtIdx] > WI[p] then begin WI[p] := WI[TgtIdx]; Changed := True; end;
        if WF[TgtIdx] > WF[p] then begin WF[p] := WF[TgtIdx]; Changed := True; end;
        if WS[TgtIdx] > WS[p] then begin WS[p] := WS[TgtIdx]; Changed := True; end;
        // The saved range has to COVER the callee's, so the low end moves down as the high end
        // moves up.
        if LI[TgtIdx] < LI[p] then begin LI[p] := LI[TgtIdx]; Changed := True; end;
        if LF[TgtIdx] < LF[p] then begin LF[p] := LF[TgtIdx]; Changed := True; end;
        if LS[TgtIdx] < LS[p] then begin LS[p] := LS[TgtIdx]; Changed := True; end;
      end;
    end;
  until (not Changed) or (Rounds > NProc + 2);

  // Publish, indexed by entry PC. Unknown (or a fixpoint that did not settle) = program-wide.
  SetLength(FProcWidths, NInstr);
  for i := 0 to NInstr - 1 do
    with FProcWidths[i] do begin WInt := -1; WFloat := -1; WStr := -1; end;
  if GFrameMark = 0 then          // gate: the historic three parallel arrays, filled alongside
  begin
    SetLength(FProcWidthInt, NInstr); SetLength(FProcWidthFloat, NInstr); SetLength(FProcWidthStr, NInstr);
    for i := 0 to NInstr - 1 do
    begin
      FProcWidthInt[i] := -1; FProcWidthFloat[i] := -1; FProcWidthStr[i] := -1;
    end;
  end;
  for p := 0 to NProc - 1 do
  begin
    if (Entry[p] < 0) or (Entry[p] >= NInstr) then System.Continue;
    if Unknown[p] or (Rounds > NProc + 2) then System.Continue;    // leave -1 = program-wide
    // A bank left untouched keeps base 0: its width is 0 too, so the range is empty either way.
    if (GFrameRangeNarrow <> 1) or (LI[p] = MaxInt) then LI[p] := 0;
    if (GFrameRangeNarrow <> 1) or (LF[p] = MaxInt) then LF[p] := 0;
    if (GFrameRangeNarrow <> 1) or (LS[p] = MaxInt) then LS[p] := 0;
    with FProcWidths[Entry[p]] do
    begin
      WInt := (Int64(WI[p]) shl 32) or Int64(LI[p]);
      WFloat := (Int64(WF[p]) shl 32) or Int64(LF[p]);
      WStr := (Int64(WS[p]) shl 32) or Int64(LS[p]);
    end;
    if GFrameMark = 0 then
    begin
      FProcWidthInt[Entry[p]] := (Int64(WI[p]) shl 32) or Int64(LI[p]);
      FProcWidthFloat[Entry[p]] := (Int64(WF[p]) shl 32) or Int64(LF[p]);
      FProcWidthStr[Entry[p]] := (Int64(WS[p]) shl 32) or Int64(LS[p]);
    end;
  end;
  BuildProcFrameBases;
end;

{ Which procedures may have their integer frame RELOCATED instead of copied.

  The banks are global and a procedure's registers keep the same indices at every depth, so a second
  activation of the same procedure lands on the first one - which is the only reason a call has ever
  had to snapshot anything. Sliding the bank view up by a delta for the callee's whole activation
  removes the need entirely: its accesses land in fresh slots and nothing is copied or restored.

  That is only sound if EVERY register the procedure touches is one the slide is allowed to move.
  Four conditions, all conservative in the safe direction - a procedure that cannot be proven simply
  keeps the copying frame, which is always correct:

  1. INTEGER-ONLY. Every opcode in the range must satisfy BcTouchesOnlyIntBank, so the float and
     string banks are provably untouched and only the integer view has to slide. (The float bank
     could follow the same scheme; the string bank could not without a second view, because the AOT
     reaches it through a ctx field rather than an argument.)
  2. PRIVATE REGISTERS. Every index the procedure touches must be touched by NO other procedure and
     not by module level. A register shared with anyone else is a global under another name, and
     sliding the view would silently redirect reads of it. This is the condition that rules out
     SHARED scalars, and it is computed over reads AND writes - unlike the frame WIDTH, which is a
     write-set question (an unwritten register keeps the caller's value, so it needs no protection;
     but it very much needs to stay at its absolute address).
  3. NO UPWARD-EXPOSED READ. Every integer register the procedure reads must also be written by it
     somewhere. A register read but never written carries a value from outside this activation -
     the caller's, or a previous activation's - and a relocated frame starts on fresh slots, so
     that value would not be there. This is what protects anything with call-to-call persistence.
  4. NO GOSUB, NO INDIRECT CONTROL FLOW. Already computed as BaseUnsafe/Unknown by the width pass:
     a GOSUB body sits outside the procedure's instruction range, so the scan never sees what it
     touches, and an indirect call or error jump can land anywhere.

  Note what is NOT required: that the callees be relocatable too. A relocated frame sits above every
  logical register index, so a non-relocated callee running at its absolute indices cannot collide
  with it. The two schemes nest freely. }
procedure TBytecodeVM.BuildProcFrameBases;
var
  NInstr, NProc, NUnit, p, i, r, i2, PcStart, PcEnd, Lo, Hi: Integer;
  Instr: TBytecodeInstruction;
  Op: Word;
  IsTarget: array of Boolean;       // PC is the entry of some call: only there can a frame start
  UStart, UEnd: array of Integer;   // unit index -> instruction range
  Owner: array of Integer;          // register index -> owning unit (-1 free, -2 shared)
  WrittenBy: array of Integer;      // register index -> unit that writes it (-1 none, -2 many)
  ExternallyRead: array of Boolean; // register carries a value INTO some unit from outside it
  WStamp: array of Integer;         // register -> unit whose own write set contains it
  MaxReg: Integer;
  UD, UDR, U1, U2, Ok, ChangedU: Boolean;
  Why: string;
  Elig: array of Boolean;           // unit -> relocatable (before and after the callee fixpoint)
  ELo, EHi: array of Integer;       // unit -> its private integer range
  EWhy: array of string;            // unit -> why it was refused (diagnostic only)

  procedure Claim(u, RegIdx: Integer);
  begin
    if (RegIdx < 0) or (RegIdx > MaxReg) then Exit;
    if Owner[RegIdx] = -1 then Owner[RegIdx] := u
    else if Owner[RegIdx] <> u then Owner[RegIdx] := -2;
  end;
  { Which of Dest/Src1/Src2 this opcode really touches in the INTEGER bank. Asking the raw operand
    fields instead would be wrong in the one way that matters here: an absent operand lowers to
    register 0, which is a perfectly valid index, so crediting it makes every procedure containing
    a call "touch" R0 and share it with everyone - which is exactly how the first version of this
    analysis found nothing at all. Unaudited shapes answer YES, which can only cost eligibility. }
  procedure IntOperands(Op: Word; out UD, UDR, U1, U2: Boolean);
  var us: Integer;
  begin
    UD := BcIntWriteShapeRaw(Op) <> IW_NONE;          // Dest WRITTEN (or unaudited)
    us := BcIntUseShape(Op);
    U1 := (us = US_UNKNOWN) or ((us and US_SRC1) <> 0);
    U2 := (us = US_UNKNOWN) or ((us and US_SRC2) <> 0);
    UDR := (us = US_UNKNOWN) or ((us and US_DEST) <> 0);   // Dest READ (an array element store)
  end;

begin
  SetLength(FProcFrameBase, 0);
  if (FProgram = nil) or (GFrameBase <> 1) then Exit;
  NInstr := FProgram.GetInstructionCount;
  NProc := FProgram.GetProcMapCount;
  if (NInstr = 0) or (NProc = 0) then Exit;

  // Widest index any instruction names, so the ownership tables can be flat arrays.
  MaxReg := 0;
  SetLength(IsTarget, NInstr);
  for i := 0 to NInstr - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if Instr.Dest > MaxReg then MaxReg := Instr.Dest;
    if Instr.Src1 > MaxReg then MaxReg := Instr.Src1;
    if Instr.Src2 > MaxReg then MaxReg := Instr.Src2;
    IsTarget[i] := False;
  end;
  // An activation can only begin where something calls, so those PCs - not the procedure map - are
  // what partitions the program here. The map is FINER than the call graph: LICM registers a
  // procedure's preheader as its own entry ("FIB_prehead"), and treating that as a separate
  // procedure makes every hoisted constant look SHARED between it and the body, which refused
  // every candidate there was. bcLoadProcAddr counts too: its target can be reached indirectly.
  for i := 0 to NInstr - 1 do
  begin
    Instr := FProgram.GetInstruction(i);
    if (Instr.OpCode = Ord(bcCallSub)) or (Instr.OpCode = Ord(bcLoadProcAddr)) then
      if (Instr.Immediate >= 0) and (Instr.Immediate < NInstr) then IsTarget[Instr.Immediate] := True;
    // PROGRAM-WIDE veto: an armed error handler jumps straight to its PC without unwinding the
    // frame stack, so a trap taken inside a relocated frame would run the handler on the CALLEE's
    // slid view - reading the handler's own registers at the wrong addresses. Nothing local to a
    // procedure can see this coming, so a program that arms a handler anywhere relocates nothing.
    // (Same veto the register-reuse merge already carries, for the same reason.)
    if (Instr.OpCode = Ord(bcOnError)) or (Instr.OpCode = Ord(bcTrap)) or
       (Instr.OpCode = Ord(bcResume)) or (Instr.OpCode = Ord(bcResumeLabel)) then
    begin
      if GFrameBaseDiag = 1 then
        WriteLn(ErrOutput, '[FRAMEBASE] program arms an error handler: relocation disabled');
      Exit;
    end;
  end;

  // Units: a map entry that nothing calls is absorbed into the one before it. Unit NUnit is module
  // level (everything below the first entry), so a register shared between a SUB and the module
  // body reads as shared rather than as the SUB's private property.
  SetLength(UStart, NProc + 1); SetLength(UEnd, NProc + 1);
  NUnit := 0;
  for p := 0 to NProc - 1 do
  begin
    PcStart := FProgram.GetProcMapStart(p);
    if PcStart < 0 then System.Continue;
    if p + 1 < NProc then PcEnd := FProgram.GetProcMapStart(p + 1) - 1 else PcEnd := NInstr - 1;
    if PcEnd >= NInstr then PcEnd := NInstr - 1;
    // An EMPTY map entry (PcEnd < PcStart) is two entries at the same PC: LICM registers a
    // preheader even when it hoisted nothing into it. Absorbing it would be harmless; starting a
    // unit on it is not - the eligibility loop below never runs, so nothing refuses it and it
    // publishes a ZERO-WIDTH frame whose delta slides the view by the whole bank. That is exactly
    // how m389 and m435 died.
    if PcEnd < PcStart then System.Continue;
    if (NUnit > 0) and ((not IsTarget[PcStart]) or (UEnd[NUnit - 1] < UStart[NUnit - 1])) then
      UEnd[NUnit - 1] := PcEnd                       // absorb: a preheader, or any uncalled tail
    else
    begin
      UStart[NUnit] := PcStart; UEnd[NUnit] := PcEnd; Inc(NUnit);
    end;
  end;
  if NUnit = 0 then Exit;
  UStart[NUnit] := 0; UEnd[NUnit] := UStart[0] - 1;  // module level
  if GFrameBaseDiag = 1 then
    for p := 0 to NProc - 1 do
      WriteLn(ErrOutput, Format('[FRAMEBASE] map %d: pc %d "%s" isTarget=%s',
                                [p, FProgram.GetProcMapStart(p), FProgram.GetProcMapName(p),
                                 BoolToStr(IsTarget[FProgram.GetProcMapStart(p)], True)]));

  // Pass 1: ownership, walked by RANGE rather than by asking "which unit owns this PC?" per
  // instruction - that question answered with a scan is how LICM and Strength Reduction came to be
  // 92% of compile time.
  SetLength(Owner, MaxReg + 1); SetLength(WrittenBy, MaxReg + 1); SetLength(WStamp, MaxReg + 1);
  for i := 0 to MaxReg do begin Owner[i] := -1; WrittenBy[i] := -1; WStamp[i] := -1; end;
  for p := 0 to NUnit do
    for i := UStart[p] to UEnd[p] do
    begin
      Instr := FProgram.GetInstruction(i);
      IntOperands(Instr.OpCode, UD, UDR, U1, U2);
      if UD or UDR then Claim(p, Instr.Dest);
      if U1 then Claim(p, Instr.Src1);
      if U2 then Claim(p, Instr.Src2);
      if BcIntWriteShapeRaw(Instr.OpCode) = IW_DEST then
        if (Instr.Dest >= 0) and (Instr.Dest <= MaxReg) then
        begin
          if WrittenBy[Instr.Dest] = -1 then WrittenBy[Instr.Dest] := p
          else if WrittenBy[Instr.Dest] <> p then WrittenBy[Instr.Dest] := -2;
        end;
    end;

  // Which registers carry a value INTO a unit from outside it - a unit reading a register it never
  // writes is reading someone else's value, so that register is a global under another name. This
  // is the set relocation must not disturb, and it is the RIGHT question: "is this register private
  // to one procedure?" is not, because the register allocator legitimately reuses one index in two
  // procedures whose live ranges do not overlap, and a relocated frame cannot hurt the caller in any
  // case - it never writes the caller's slots at all.
  SetLength(ExternallyRead, MaxReg + 1);
  for i := 0 to MaxReg do ExternallyRead[i] := False;
  for p := 0 to NUnit do
  begin
    // Stamp this unit's own writes first: "does p write r?" cannot be asked of the program-wide
    // WrittenBy, which collapses to -2 the moment two units share an index - and the allocator
    // shares indices routinely, so asking it there marks every reused register as external and
    // refuses everything.
    for i := UStart[p] to UEnd[p] do
    begin
      Instr := FProgram.GetInstruction(i);
      if BcIntWriteShapeRaw(Instr.OpCode) = IW_DEST then
        if (Instr.Dest >= 0) and (Instr.Dest <= MaxReg) then WStamp[Instr.Dest] := p;
    end;
    for i := UStart[p] to UEnd[p] do
    begin
      Instr := FProgram.GetInstruction(i);
      IntOperands(Instr.OpCode, UD, UDR, U1, U2);
      if U1 and (Instr.Src1 >= 0) and (Instr.Src1 <= MaxReg) and (WStamp[Instr.Src1] <> p) then
        ExternallyRead[Instr.Src1] := True;
      if U2 and (Instr.Src2 >= 0) and (Instr.Src2 <= MaxReg) and (WStamp[Instr.Src2] <> p) then
        ExternallyRead[Instr.Src2] := True;
      // A read through the Dest field counts exactly like the other two.
      if UDR and (Instr.Dest >= 0) and (Instr.Dest <= MaxReg) and (WStamp[Instr.Dest] <> p) then
        ExternallyRead[Instr.Dest] := True;
    end;
  end;

  // Pass 2: eligibility, per unit (module level is never a callee, so it is skipped).
  SetLength(FProcFrameBase, NInstr);
  for i := 0 to NInstr - 1 do FProcFrameBase[i] := -1;
  // The fast table is only ever consulted when the frame-mark stack is the bookkeeping in use;
  // leaving it EMPTY under FRAMEMARK=0 or FRAME_FAST=0 makes the length test in FramePush fail and
  // sends every call down the general path, with no second gate to check per call.
  SetLength(FFrameFast, 0);
  if (GFrameMark = 1) and (GFrameFast = 1) then
  begin
    SetLength(FFrameFast, NInstr);
    for i := 0 to NInstr - 1 do FFrameFast[i] := -1;
  end;
  SetLength(Elig, NUnit); SetLength(ELo, NUnit); SetLength(EHi, NUnit); SetLength(EWhy, NUnit);
  for p := 0 to NUnit - 1 do
  begin
    Elig[p] := False; ELo[p] := 0; EHi[p] := 0; EWhy[p] := 'not analysed';
    PcStart := UStart[p]; PcEnd := UEnd[p];
    if (PcStart < 0) or (PcStart >= NInstr) or (PcEnd < PcStart) then System.Continue;
    if (PcStart >= Length(FProcWidths)) or (FProcWidths[PcStart].WInt < 0) then System.Continue;
    Lo := MaxInt; Hi := 0;
    Ok := True; Why := '';
    // This unit's OWN write set, stamped with its index: "does p write r?" is a per-unit question
    // and the program-wide WrittenBy cannot answer it once two units share an index.
    for i := PcStart to PcEnd do
    begin
      Instr := FProgram.GetInstruction(i);
      if BcIntWriteShapeRaw(Instr.OpCode) = IW_DEST then
        if (Instr.Dest >= 0) and (Instr.Dest <= MaxReg) then WStamp[Instr.Dest] := p;
    end;
    for i := PcStart to PcEnd do
    begin
      Instr := FProgram.GetInstruction(i);
      Op := Instr.OpCode;
      // (1) the opcode's effect on the INTEGER bank must be audited, and (4) no GOSUB / indirect
      // control flow. What matters is not that the opcode is integer-only but that we know exactly
      // which integer registers it writes: a float multiply touches none of ours and is as safe to
      // relocate around as a jump. An unaudited opcode still refuses the whole unit.
      if (BcIntWriteShapeRaw(Op) = IW_UNKNOWN) or (Op = Ord(bcCall)) or (Op = Ord(bcReturn)) or
         (Op = Ord(bcCallSubIndirect)) or (Op = Ord(bcThreadCreate)) then
      begin Ok := False; Why := Format('unaudited write shape/indirect: op $%x at pc %d', [Op, i]); Break; end;
      if BcIntUseShape(Op) = US_UNKNOWN then
      begin Ok := False; Why := Format('unaudited read shape: op $%x at pc %d', [Op, i]); Break; end;
      IntOperands(Op, UD, UDR, U1, U2);
      // (3) a register this unit reads but never writes carries a value from outside the
      // activation - the caller's, or an outer activation's - and a relocated frame starts on
      // fresh slots, so that value would not be there.
      if U1 and (Instr.Src1 >= 0) and (Instr.Src1 <= MaxReg) and (WStamp[Instr.Src1] <> p) then
      begin Ok := False;
        Why := Format('reads R%d never written here (op $%x pc %d)', [Instr.Src1, Op, i]); Break; end;
      if U2 and (Instr.Src2 >= 0) and (Instr.Src2 <= MaxReg) and (WStamp[Instr.Src2] <> p) then
      begin Ok := False;
        Why := Format('reads R%d never written here (op $%x pc %d)', [Instr.Src2, Op, i]); Break; end;
      if UDR and (Instr.Dest >= 0) and (Instr.Dest <= MaxReg) and (WStamp[Instr.Dest] <> p) then
      begin Ok := False;
        Why := Format('reads R%d (Dest) never written here (op $%x pc %d)', [Instr.Dest, Op, i]); Break; end;
      // (2) nothing this unit WRITES may be a register somebody else reads for its value: that
      // write has to land at the absolute address the reader will look at, and a relocated frame
      // would put it somewhere else. Reads and writes of the same index by an unrelated unit are
      // fine - the allocator reuses indices across procedures whose live ranges are disjoint.
      for r := 0 to 2 do
      begin
        case r of
          0: begin if not (UD or UDR) then System.Continue; i2 := Instr.Dest; end;
          1: begin if not U1 then System.Continue; i2 := Instr.Src1; end;
        else  begin if not U2 then System.Continue; i2 := Instr.Src2; end;
        end;
        if (i2 < 0) or (i2 > MaxReg) then System.Continue;
        // Only a WRITE through Dest can strand another unit's reader; a read through it is just
        // one more index this unit's range has to cover.
        if (r = 0) and UD and ExternallyRead[i2] then
        begin Ok := False;
          Why := Format('writes R%d which another unit reads (op $%x pc %d)', [i2, Op, i]); Break; end;
        if i2 < Lo then Lo := i2;
        if i2 + 1 > Hi then Hi := i2 + 1;
      end;
      if not Ok then Break;
    end;
    // A frame with no integer registers of its own has nothing to relocate, and its delta would be
    // HighWater - MaxInt. Refuse it rather than publish a zero-width frame.
    if Ok and ((Lo = MaxInt) or (Hi <= Lo)) then
    begin Ok := False; Why := 'no integer registers of its own'; end;
    Elig[p] := Ok; ELo[p] := Lo; EHi[p] := Hi; EWhy[p] := Why;
  end;

  // (5) Every static callee of a relocated unit must be relocatable too - a FIXPOINT, because
  // refusing one unit can refuse its callers. Without this the scheme is unsound in a way no local
  // test can see: the view is per-context, so a non-relocated callee invoked from a relocated
  // caller would run with the CALLER's delta still applied and address its own absolute registers
  // - and any global it touches - at slid addresses. Self-recursion satisfies the condition, which
  // is the case that matters.
  repeat
    ChangedU := False;
    for p := 0 to NUnit - 1 do
    begin
      if not Elig[p] then System.Continue;
      for i := UStart[p] to UEnd[p] do
      begin
        Instr := FProgram.GetInstruction(i);
        if Instr.OpCode <> Ord(bcCallSub) then System.Continue;
        i2 := -1;
        for r := 0 to NUnit - 1 do
          if UStart[r] = Instr.Immediate then begin i2 := r; Break; end;
        if (i2 < 0) or (not Elig[i2]) then
        begin
          Elig[p] := False; ChangedU := True;
          EWhy[p] := Format('calls a non-relocatable unit at pc %d', [Instr.Immediate]);
          Break;
        end;
      end;
    end;
  until not ChangedU;

  // Program-wide: does every unit that something CALLS have a fast frame?
  FAllCalleesFast := True;
  for p := 0 to NUnit - 1 do
    if IsTarget[UStart[p]] and
       not ((UStart[p] < Length(FProcWidths)) and (FProcWidths[UStart[p]].WInt >= 0) and
            ((FProcWidths[UStart[p]].WFloat shr 32) = 0) and
            ((FProcWidths[UStart[p]].WStr shr 32) = 0) and Elig[p]) then
      FAllCalleesFast := False;
  for p := 0 to NUnit - 1 do
  begin
    if GFrameBaseDiag = 1 then
      if Elig[p] then
        WriteLn(ErrOutput, Format('[FRAMEBASE] unit %d @pc %d..%d (%s): RELOCATABLE, int range [%d,%d)',
                                  [p, UStart[p], UEnd[p], FProgram.GetProcMapName(p), ELo[p], EHi[p]]))
      else
        WriteLn(ErrOutput, Format('[FRAMEBASE] unit %d @pc %d..%d: no - %s',
                                  [p, UStart[p], UEnd[p], EWhy[p]]));
    if not Elig[p] then System.Continue;
    // MEASURED: relocating a frame that still copies floats or strings COSTS instead of paying.
    // fibf, made eligible by classifying the PRINT family, came out +3.5% --aot and +2.0%
    // interpreted AGAINST the copying frame - after the call-site liveness its integer range was
    // already half a register, so the slide's bookkeeping bought nothing while the general
    // path's extra work stayed. The whole win is the FAST path: fib, which copies nothing at
    // all, is -27.5%. So publish a frame base ONLY where the fast path applies.
    // FRAMEBASE_WIDE=1 publishes the rest too, to re-measure this decision on one binary.
    if (GFrameBaseWide <> 1) and
       not ((UStart[p] < Length(FProcWidths)) and (FProcWidths[UStart[p]].WInt >= 0) and
            ((FProcWidths[UStart[p]].WFloat shr 32) = 0) and
            ((FProcWidths[UStart[p]].WStr shr 32) = 0)) then
    begin
      if GFrameBaseDiag = 1 then
        WriteLn(ErrOutput, Format('[FRAMEBASE] unit %d @pc %d..%d: relocatable but NOT fast'
                                  + ' (copies float/string) - keeping the copying frame',
                                  [p, UStart[p], UEnd[p]]));
      System.Continue;
    end;
    // Published per entry PC, packed like the widths: one past the highest index used in the high
    // half, the lowest in the low half. A relocated frame starts at the high-water mark and is
    // Hi-Lo slots wide, so the view delta is HighWater - Lo.
    FProcFrameBase[UStart[p]] := (Int64(EHi[p]) shl 32) or Int64(ELo[p]);
    // Fast path eligibility, decided ONCE here instead of per call: a relocatable callee that also
    // leaves the float and string banks alone has nothing to copy, so its whole frame is the pointer
    // slide. Store the WIDTH (not the high end) so FramePush adds instead of subtracting.
    if (Length(FFrameFast) > 0) and (UStart[p] < Length(FProcWidths)) and
       (FProcWidths[UStart[p]].WInt >= 0) and
       ((FProcWidths[UStart[p]].WFloat shr 32) = 0) and ((FProcWidths[UStart[p]].WStr shr 32) = 0) then
      FFrameFast[UStart[p]] := (Int64(EHi[p] - ELo[p]) shl 32) or Int64(ELo[p]);
  end;
end;

procedure TBytecodeVM.BuildCallSiteLiveness;
// What a call has to protect is not what the callee can WRITE - it is what the CALLER still needs
// afterwards. This computes, per call site, the integer registers live ACROSS the call (backward
// liveness over the calling procedure's own instruction range) and publishes the range; FramePush
// intersects it with the callee footprint from BuildProcFrameWidths, and the intersection is what
// gets copied. On naive recursive Fibonacci the callee footprint is ten registers and the live-across
// set is ONE: everything else the callee writes, the caller has already finished with.
//
// The two sets answer genuinely different questions, and only their intersection has to be saved:
// a register the callee never writes keeps the caller's value anyway, and a register the caller
// never reads again may be left in whatever state the callee leaves it.
//
// Deliberately restricted to procedures where EVERY instruction has an audited operand shape and
// known control flow. Unlike the width computation - where an unaudited opcode merely costs a missed
// narrowing - liveness must see every READ to be sound, so an unaudited opcode disqualifies its
// whole procedure rather than being guessed at. That currently means integer procedures; anything
// with a float, string, array or I/O opcode keeps the callee-footprint behaviour.
var
  NProc, NInstr, p, i, k, w, Words, RegCount, Lo, Hi, Tgt, PcStart, PcEnd, Rounds: Integer;
  Instr: TBytecodeInstruction;
  Op: Word;
  Live: array of QWord;      // LiveIn per instruction, (i - PcStart) * Words + w
  Out_: array of QWord;      // scratch: LiveOut of the instruction being processed
  Eligible: Boolean;
  Changed: Boolean;
  Def, Use1, Use2, Use3, Shape, UShape: Integer;
  Bit: QWord;

  procedure SetBit(var A: array of QWord; Base, Reg: Integer);
  begin
    if (Reg < 0) or (Reg >= RegCount) then Exit;
    A[Base + (Reg shr 6)] := A[Base + (Reg shr 6)] or (QWord(1) shl (Reg and 63));
  end;

begin
  SetLength(FCallLiveInt, 0);
  if (FProgram = nil) or (GFrameLiveNarrow <> 1) then Exit;
  NInstr := FProgram.GetInstructionCount;
  NProc := FProgram.GetProcMapCount;
  if (NInstr = 0) or (NProc = 0) then Exit;
  SetLength(FCallLiveInt, NInstr);
  for i := 0 to NInstr - 1 do FCallLiveInt[i] := -1;

  for p := 0 to NProc - 1 do
  begin
    PcStart := FProgram.GetProcMapStart(p);
    if PcStart < 0 then System.Continue;
    if p + 1 < NProc then PcEnd := FProgram.GetProcMapStart(p + 1) - 1 else PcEnd := NInstr - 1;
    if PcEnd >= NInstr then PcEnd := NInstr - 1;
    if PcEnd < PcStart then System.Continue;

    // Eligibility and register bound in one scan: every opcode audited on both sides, every branch
    // target inside this procedure, and no path falling out of its last instruction.
    Eligible := True;
    RegCount := 0;
    for i := PcStart to PcEnd do
    begin
      Instr := FProgram.GetInstruction(i);
      Op := Instr.OpCode;
      if (BcIntWriteShapeRaw(Op) = IW_UNKNOWN) or (BcIntUseShape(Op) = US_UNKNOWN) then
      begin Eligible := False; Break; end;
      if (Op = Ord(bcJump)) or (Op = Ord(bcJumpIfZero)) or (Op = Ord(bcJumpIfNotZero)) then
      begin
        Tgt := Instr.Immediate;
        if (Tgt < PcStart) or (Tgt > PcEnd) then begin Eligible := False; Break; end;
      end;
      if (i = PcEnd) and (Op <> Ord(bcReturnSub)) and (Op <> Ord(bcJump)) then
      begin Eligible := False; Break; end;   // falls out of the range: successors unknown
      if Instr.Dest + 1 > RegCount then RegCount := Instr.Dest + 1;
      if Instr.Src1 + 1 > RegCount then RegCount := Instr.Src1 + 1;
      if Instr.Src2 + 1 > RegCount then RegCount := Instr.Src2 + 1;
    end;
    if not Eligible or (RegCount <= 0) then System.Continue;

    Words := (RegCount + 63) div 64;
    SetLength(Live, (PcEnd - PcStart + 1) * Words);
    for k := 0 to Length(Live) - 1 do Live[k] := 0;
    SetLength(Out_, Words);

    // Backward fixpoint. LiveIn = (LiveOut - Def) + Use; LiveOut = union of the successors' LiveIn.
    // Iterating backwards converges in a couple of rounds on structured code; the cap is a backstop.
    Rounds := 0;
    repeat
      Changed := False;
      Inc(Rounds);
      for i := PcEnd downto PcStart do
      begin
        Instr := FProgram.GetInstruction(i);
        Op := Instr.OpCode;
        for w := 0 to Words - 1 do Out_[w] := 0;
        // Successors: the branch target, the fall-through, or neither for a return.
        if (Op = Ord(bcJump)) then
        begin
          for w := 0 to Words - 1 do Out_[w] := Live[(Instr.Immediate - PcStart) * Words + w];
        end
        else if (Op = Ord(bcJumpIfZero)) or (Op = Ord(bcJumpIfNotZero)) then
        begin
          for w := 0 to Words - 1 do
            Out_[w] := Live[(Instr.Immediate - PcStart) * Words + w] or
                       Live[(i + 1 - PcStart) * Words + w];
        end
        else if Op <> Ord(bcReturnSub) then
          for w := 0 to Words - 1 do Out_[w] := Live[(i + 1 - PcStart) * Words + w];

        // A call site's live-OUT is what this frame still needs when the callee returns: publish it
        // before Def/Use turn it into the instruction's live-IN.
        if Op = Ord(bcCallSub) then
        begin
          Lo := -1; Hi := -1;
          for k := 0 to RegCount - 1 do
          begin
            Bit := Out_[k shr 6] shr (k and 63);
            if (Bit and 1) <> 0 then
            begin
              if Lo < 0 then Lo := k;
              Hi := k;
            end;
          end;
          if Lo < 0 then FCallLiveInt[i] := 0                       // nothing live: save nothing
          else FCallLiveInt[i] := (Int64(Hi + 1) shl 32) or Int64(Lo);
        end;

        Shape := BcIntWriteShapeRaw(Op);
        UShape := BcIntUseShape(Op);
        Def := -1; Use1 := -1; Use2 := -1; Use3 := -1;
        if Shape = IW_DEST then Def := Instr.Dest;
        if (UShape and US_SRC1) <> 0 then Use1 := Instr.Src1;
        if (UShape and US_SRC2) <> 0 then Use2 := Instr.Src2;
        // An array element store READS the value out of its Dest field. Missing this would let the
        // liveness call that register dead across a call and drop it from the snapshot.
        if (UShape and US_DEST) <> 0 then Use3 := Instr.Dest;
        if (Def >= 0) and (Def < RegCount) then
          Out_[Def shr 6] := Out_[Def shr 6] and not (QWord(1) shl (Def and 63));
        SetBit(Out_, 0, Use1);
        SetBit(Out_, 0, Use2);
        SetBit(Out_, 0, Use3);
        for w := 0 to Words - 1 do
          if Live[(i - PcStart) * Words + w] <> Out_[w] then
          begin
            Live[(i - PcStart) * Words + w] := Out_[w];
            Changed := True;
          end;
      end;
    until (not Changed) or (Rounds > 64);
    // A fixpoint that did not settle publishes nothing: the entries stay -1 and the call sites keep
    // the callee-footprint behaviour.
    if Rounds > 64 then
      for i := PcStart to PcEnd do FCallLiveInt[i] := -1;
  end;
end;

var
  GFrameSaveNoStr: Integer = 0;   // measurement probe, set from FRAMESAVE_NOSTR at startup

procedure TBytecodeVM.FramePush(Ctx: TExecutionContext; TargetPC: Integer; CallPC: Integer);
// Snapshot the live part of each register bank onto the flat per-bank save stacks (one frame).
// The saved width is Ctx.FrameSave*Count (the highest register index the program's bytecode
// mentions, +1) rather than the whole 256-slot-floor bank: registers above that are never read
// or written, so not saving them is unobservable - and it removes 256 refcounted string copies
// per call, which dominated the SUB/FUNCTION call cost. The widths are invariant during a run,
// so bcReturnSub pops exactly what was pushed.
var
  i, NI, NF, NS, BI, BF, BS: Integer;
  FBLo, FBHi: Integer;
  PW: Int64;
  PWidth: ^TProcWidth;
  Reloc: Boolean;
  SaveDelta, SaveHw, NewDelta, NewHw: Integer;
begin
  // FAST RELOCATION. One table read decides it, because the answer was precomputed: a callee proven
  // relocatable that touches neither the float nor the string bank has nothing to copy, so its whole
  // frame is a pointer slide plus the marks FramePop needs. This used to be reached through four
  // lookups across three arrays and wrote seven mark fields; measured on fib, FramePush and FramePop
  // together were 65.7 cycles of a 205-cycle call while copying ZERO bytes.
  // WInt = -1 is the sentinel that lets FramePop take its own fast path: a general frame always
  // writes a non-negative packed width there, including a relocated one that still copies floats.
  if (TargetPC >= 0) and (TargetPC < Length(FFrameFast)) then
  begin
    PW := FFrameFast[TargetPC];
    if PW >= 0 then
    begin
      FBHi := PW shr 32;                        // frame WIDTH, precomputed as Hi - Lo
      FBLo := PW and $FFFFFFFF;                 // its lowest register index
      if Ctx.RegHwI + FBHi <= Ctx.RegFrameCap then
      begin
        if Ctx.FrameMarkTop >= Length(Ctx.FrameMarks) then
          SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
        with Ctx.FrameMarks[Ctx.FrameMarkTop] do
        begin
          SaveDeltaI := Ctx.RegDeltaI;
          SaveHwI := Ctx.RegHwI;
          WInt := -1;                           // nothing copied, and nothing to read back
          RecBase := Ctx.RecordCount;
          BlockMark := Ctx.BlockRecMarkTop;
        end;
        Inc(Ctx.FrameMarkTop);
        Ctx.RegDeltaI := Ctx.RegHwI - FBLo;
        Inc(Ctx.RegHwI, FBHi);
        Ctx.IntRegs := @Ctx.IntRegsMem[Ctx.RegDeltaI];
        Exit;
      end;
    end;
  end;
  BI := 0; BF := 0; BS := 0;
  Reloc := False; SaveDelta := -1; SaveHw := 0; NewDelta := 0; NewHw := 0;
  // FRAME RELOCATION. When the callee is one of the procedures BuildProcFrameBases proved
  // relocatable, its INTEGER activation runs on fresh slots above every live frame: slide the view
  // and copy no integers at all. The delta is HighWater - Lo, so the frame lands exactly at the
  // high-water mark whatever its lowest register index happens to be. Falls through to the copying
  // path if the region is full, which is a slowdown and never a wrong answer.
  // The float and string banks are NOT relocated: a relocatable callee may now touch them (see
  // GFrameBaseWide), and it reaches them at absolute addresses, so they keep being snapshotted
  // exactly as they always were. Only the integer half of this frame becomes free.
  // Gated on GFrameMark because a relocated frame has nowhere but the frame-mark stack to record
  // the view it has to slide back to.
  if (GFrameBase = 1) and (GFrameMark = 1) and (TargetPC >= 0) and
     (TargetPC < Length(FProcFrameBase)) and (FProcFrameBase[TargetPC] >= 0) then
  begin
    PW := FProcFrameBase[TargetPC];
    FBHi := PW shr 32; FBLo := PW and $FFFFFFFF;  // separate locals: falling through must not
    if Ctx.RegHwI + (FBHi - FBLo) <= Ctx.RegFrameCap then   // disturb the copying path's BI/NI
    begin
      Reloc := True;
      SaveDelta := Ctx.RegDeltaI;                 // >= 0 marks this frame as RELOCATED
      SaveHw := Ctx.RegHwI;
      NewDelta := Ctx.RegHwI - FBLo;
      NewHw := Ctx.RegHwI + (FBHi - FBLo);
      // FAST PATH, and the only one this scheme had at first: a callee that touches neither the
      // float nor the string bank has nothing left to copy, so its whole frame is a pointer slide.
      // Worth testing for rather than folding into the general path - it is the recursive
      // integer function, which is where relocation earns its keep.
      if (TargetPC < Length(FProcWidths)) and (FProcWidths[TargetPC].WInt >= 0) and
         ((FProcWidths[TargetPC].WFloat shr 32) = 0) and ((FProcWidths[TargetPC].WStr shr 32) = 0) then
      begin
        if Ctx.FrameMarkTop >= Length(Ctx.FrameMarks) then
          SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
        with Ctx.FrameMarks[Ctx.FrameMarkTop] do
        begin
          SaveDeltaI := SaveDelta;
          SaveHwI := SaveHw;
          WInt := 0; WFloat := 0; WStr := 0;      // nothing was copied, so nothing is restored
          RecBase := Ctx.RecordCount;
          BlockMark := Ctx.BlockRecMarkTop;
        end;
        Inc(Ctx.FrameMarkTop);
        Ctx.RegDeltaI := NewDelta;
        Ctx.RegHwI := NewHw;
        Ctx.IntRegs := @Ctx.IntRegsMem[NewDelta];
        Exit;
      end;
    end;
  end;
  // A NEGATIVE width means "not measured for this context" (any path that runs bytecode without
  // going through LoadProgram): fall back to the whole bank, the historical behaviour. Zero is a
  // real width -- a bank the program never touches -- and must save nothing.
  NI := Ctx.FrameSaveIntCount;    if (NI < 0) or (NI > Ctx.IntRegCount) then NI := Ctx.IntRegCount;
  NF := Ctx.FrameSaveFloatCount;  if (NF < 0) or (NF > Ctx.FloatRegCount) then NF := Ctx.FloatRegCount;
  NS := Ctx.FrameSaveStrCount;    if (NS < 0) or (NS > Ctx.StringRegCount) then NS := Ctx.StringRegCount;
  // MEASUREMENT PROBE (FRAMESAVE_NOSTR=1): drop the string half of the frame snapshot. Unsound in
  // general - a callee that writes a string register would corrupt the caller - but SOUND for a
  // program that touches no string, which is how it is used: to price the refcounted copies that
  // the coarse per-procedure width charges to procedures that never see a string. Verify the
  // output of any program measured with it (the LS_NOWB lesson).
  if GFrameSaveNoStr = 1 then NS := 0;
  // Narrow further to what THIS callee can clobber, when that is known (static target, no indirect
  // call anywhere in its reachable set). Never widens: the program-wide width stays the ceiling.
  if GFrameMark = 0 then
  begin
    // Gate: the historic three-array lookup, verbatim - three field loads, three Length() checks.
    if (TargetPC >= 0) and (TargetPC < Length(FProcWidthInt)) and (FProcWidthInt[TargetPC] >= 0) then
    begin
      PW := FProcWidthInt[TargetPC];
      if PW shr 32 < NI then NI := PW shr 32;
      BI := PW and $FFFFFFFF; if BI > NI then BI := NI;
      PW := FProcWidthFloat[TargetPC];
      if PW shr 32 < NF then NF := PW shr 32;
      BF := PW and $FFFFFFFF; if BF > NF then BF := NF;
      PW := FProcWidthStr[TargetPC];
      if PW shr 32 < NS then NS := PW shr 32;
      BS := PW and $FFFFFFFF; if BS > NS then BS := NS;
    end;
  end
  else if (TargetPC >= 0) and (TargetPC < Length(FProcWidths)) and (FProcWidths[TargetPC].WInt >= 0) then
  begin
    // Both ends come out of one load per bank: width in the high half, base in the low half. The
    // base is where the snapshot STARTS - registers below it belong to the caller's own frame and
    // the callee cannot name them. Clamped to the width, so the range is never negative if a probe
    // above (FRAMESAVE_NOSTR) has already zeroed a bank. All three come off ONE cache line.
    PWidth := @FProcWidths[TargetPC];
    PW := PWidth^.WInt;
    if PW shr 32 < NI then NI := PW shr 32;
    BI := PW and $FFFFFFFF; if BI > NI then BI := NI;
    PW := PWidth^.WFloat;
    if PW shr 32 < NF then NF := PW shr 32;
    BF := PW and $FFFFFFFF; if BF > NF then BF := NF;
    PW := PWidth^.WStr;
    if PW shr 32 < NS then NS := PW shr 32;
    BS := PW and $FFFFFFFF; if BS > NS then BS := NS;
  end;
  // Caller side: intersect with what THIS call site still needs afterwards. Sound for an unknown
  // callee too - a register the caller never reads again needs no protection whatever the callee
  // does with it - but only computed for procedures whose every opcode is audited.
  if (CallPC >= 0) and (CallPC < Length(FCallLiveInt)) then
  begin
    PW := FCallLiveInt[CallPC];
    if PW >= 0 then
    begin
      if PW shr 32 < NI then NI := PW shr 32;
      if (PW and $FFFFFFFF) > BI then BI := PW and $FFFFFFFF;
      if BI > NI then BI := NI;              // nothing live across: the range is empty
    end;
  end;
  // A relocated frame copies no integers whatever the widths say - the callee is about to run on
  // slots the caller does not own. The float and string ranges computed above still apply.
  if Reloc then begin NI := 0; BI := 0; end;
  // Grow save stacks if needed (defensive; usually sized once).
  if Ctx.FrameSaveIntTop + (NI - BI) > Length(Ctx.FrameSaveInt) then
    SetLength(Ctx.FrameSaveInt, Ctx.FrameSaveIntTop + (NI - BI) + 256);
  if Ctx.FrameSaveFloatTop + (NF - BF) > Length(Ctx.FrameSaveFloat) then
    SetLength(Ctx.FrameSaveFloat, Ctx.FrameSaveFloatTop + (NF - BF) + 256);
  if Ctx.FrameSaveStrTop + (NS - BS) > Length(Ctx.FrameSaveStr) then
    SetLength(Ctx.FrameSaveStr, Ctx.FrameSaveStrTop + (NS - BS) + 256);
  // Int and float are plain scalars, so the snapshot is a block move - one memcpy per bank instead
  // of an indexed loop. Strings cannot be: each copy is a refcount update.
  if GFrameRangeNarrow = 1 then
  begin
    if NI > BI then
      Move(Ctx.IntRegs[BI], Ctx.FrameSaveInt[Ctx.FrameSaveIntTop], (NI - BI) * SizeOf(Int64));
    if NF > BF then
      Move(Ctx.FloatRegs[BF], Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop], (NF - BF) * SizeOf(Double));
  end
  else
  begin
    for i := 0 to NI - 1 do                    // gate off: the historic prefix copy, verbatim
      Ctx.FrameSaveInt[Ctx.FrameSaveIntTop + i] := Ctx.IntRegs[i];
    for i := 0 to NF - 1 do
      Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop + i] := Ctx.FloatRegs[i];
  end;
  Inc(Ctx.FrameSaveIntTop, NI - BI);
  Inc(Ctx.FrameSaveFloatTop, NF - BF);
  for i := BS to NS - 1 do
    Ctx.FrameSaveStr[Ctx.FrameSaveStrTop + (i - BS)] := Ctx.StringRegs[i];
  Inc(Ctx.FrameSaveStrTop, NS - BS);
  // Remember everything FramePop needs, in ONE entry: the three bank ranges (widths differ per
  // callee, so they cannot be recomputed at pop time), where this frame's record allocations begin
  // (RAII V2) and the block-mark depth on entry (M8). One growth check and one cache line instead
  // of the five arrays this replaced - which is where the cycles of a call actually were.
  if GFrameMark = 1 then
  begin
    if Ctx.FrameMarkTop >= Length(Ctx.FrameMarks) then
      SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
    with Ctx.FrameMarks[Ctx.FrameMarkTop] do
    begin
      WInt := (Int64(NI) shl 32) or Int64(BI);
      WFloat := (Int64(NF) shl 32) or Int64(BF);
      WStr := (Int64(NS) shl 32) or Int64(BS);
      RecBase := Ctx.RecordCount;
      BlockMark := Ctx.BlockRecMarkTop;
      SaveDeltaI := SaveDelta;          // -1 = this frame COPIED: pop must not slide the view back
      SaveHwI := SaveHw;
    end;
    Inc(Ctx.FrameMarkTop);
    // Slide the integer view only now: the copies above had to read the CALLER's float and string
    // banks, and FramePop undoes this from the mark just written.
    if Reloc then
    begin
      Ctx.RegDeltaI := NewDelta;
      Ctx.RegHwI := NewHw;
      Ctx.IntRegs := @Ctx.IntRegsMem[NewDelta];
    end;
  end
  else
  begin
    // FRAMEMARK=0: the historic five-array layout, verbatim, for a one-binary A/B.
    if Ctx.FrameWidthTop >= Length(Ctx.FrameWidthInt) then
    begin
      SetLength(Ctx.FrameWidthInt, Ctx.FrameWidthTop + 256);
      SetLength(Ctx.FrameWidthFloat, Ctx.FrameWidthTop + 256);
      SetLength(Ctx.FrameWidthStr, Ctx.FrameWidthTop + 256);
    end;
    Ctx.FrameWidthInt[Ctx.FrameWidthTop] := (Int64(NI) shl 32) or Int64(BI);
    Ctx.FrameWidthFloat[Ctx.FrameWidthTop] := (Int64(NF) shl 32) or Int64(BF);
    Ctx.FrameWidthStr[Ctx.FrameWidthTop] := (Int64(NS) shl 32) or Int64(BS);
    Inc(Ctx.FrameWidthTop);
    if Ctx.FrameRecBaseTop >= Length(Ctx.FrameRecBase) then
      SetLength(Ctx.FrameRecBase, Ctx.FrameRecBaseTop + 256);
    if Ctx.FrameRecBaseTop >= Length(Ctx.FrameBlockMarkTop) then
      SetLength(Ctx.FrameBlockMarkTop, Ctx.FrameRecBaseTop + 256);
    Ctx.FrameRecBase[Ctx.FrameRecBaseTop] := Ctx.RecordCount;
    Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop] := Ctx.BlockRecMarkTop;
    Inc(Ctx.FrameRecBaseTop);
  end;
end;

procedure TBytecodeVM.FramePop(Ctx: TExecutionContext);
// Restore the live part of each register bank from the top frame (same widths FramePush used).
var
  i, NI, NF, NS, BI, BF, BS: Integer;
  PW: Int64;
  Mark: PFrameMark;
begin
  // Everything comes off the one frame-mark stack: the ranges are per-callee, so they cannot be
  // recomputed here. An empty stack means the frame was pushed before this bookkeeping existed (or
  // the stack was unwound by an error jump): fall back to the context-wide widths, as FramePush
  // would have used, and skip the record reclamation (there is no mark to roll back to).
  BI := 0; BF := 0; BS := 0;
  Mark := nil;
  if (GFrameMark = 1) and (Ctx.FrameMarkTop > 0) then
  begin
    Dec(Ctx.FrameMarkTop);
    Mark := @Ctx.FrameMarks[Ctx.FrameMarkTop];
    // FAST PATH, paired with the one in FramePush: the sentinel says this frame copied nothing at
    // all, so there are no widths to unpack, no save-stack tops to move and no banks to restore -
    // slide the view back, hand the record slots back, done. Everything else falls through to the
    // general path below, including a relocated frame that still copied floats or strings.
    if Mark^.WInt < 0 then
    begin
      Ctx.RegDeltaI := Mark^.SaveDeltaI;
      Ctx.RegHwI := Mark^.SaveHwI;
      Ctx.IntRegs := @Ctx.IntRegsMem[Mark^.SaveDeltaI];
      if Mark^.RecBase < Ctx.RecordCount then
        Ctx.RecordCount := Mark^.RecBase;
      Ctx.BlockRecMarkTop := Mark^.BlockMark;
      Exit;
    end;
    // A RELOCATED frame restored nothing and copied nothing: slide the view back to the caller's
    // offset, hand its slots back to the high-water mark, and skip the bank restore entirely (its
    // widths were written as zero, so the code below is a no-op either way - but the record
    // reclamation still has to run).
    if Mark^.SaveDeltaI >= 0 then
    begin
      Ctx.RegDeltaI := Mark^.SaveDeltaI;
      Ctx.RegHwI := Mark^.SaveHwI;
      Ctx.IntRegs := @Ctx.IntRegsMem[Ctx.RegDeltaI];
    end;
    PW := Mark^.WInt;   NI := PW shr 32; BI := PW and $FFFFFFFF;
    PW := Mark^.WFloat; NF := PW shr 32; BF := PW and $FFFFFFFF;
    PW := Mark^.WStr;   NS := PW shr 32; BS := PW and $FFFFFFFF;
  end
  else if (GFrameMark = 0) and (Ctx.FrameWidthTop > 0) then
  begin
    Dec(Ctx.FrameWidthTop);
    PW := Ctx.FrameWidthInt[Ctx.FrameWidthTop];   NI := PW shr 32; BI := PW and $FFFFFFFF;
    PW := Ctx.FrameWidthFloat[Ctx.FrameWidthTop]; NF := PW shr 32; BF := PW and $FFFFFFFF;
    PW := Ctx.FrameWidthStr[Ctx.FrameWidthTop];   NS := PW shr 32; BS := PW and $FFFFFFFF;
  end
  else
  begin
    NI := Ctx.FrameSaveIntCount;    if (NI < 0) or (NI > Ctx.IntRegCount) then NI := Ctx.IntRegCount;
    NF := Ctx.FrameSaveFloatCount;  if (NF < 0) or (NF > Ctx.FloatRegCount) then NF := Ctx.FloatRegCount;
    NS := Ctx.FrameSaveStrCount;    if (NS < 0) or (NS > Ctx.StringRegCount) then NS := Ctx.StringRegCount;
  end;
  Dec(Ctx.FrameSaveIntTop, NI - BI);
  Dec(Ctx.FrameSaveFloatTop, NF - BF);
  if GFrameRangeNarrow = 1 then
  begin
    if NI > BI then
      Move(Ctx.FrameSaveInt[Ctx.FrameSaveIntTop], Ctx.IntRegs[BI], (NI - BI) * SizeOf(Int64));
    if NF > BF then
      Move(Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop], Ctx.FloatRegs[BF], (NF - BF) * SizeOf(Double));
  end
  else
  begin
    for i := 0 to NI - 1 do                    // gate off: the historic prefix copy, verbatim
      Ctx.IntRegs[i] := Ctx.FrameSaveInt[Ctx.FrameSaveIntTop + i];
    for i := 0 to NF - 1 do
      Ctx.FloatRegs[i] := Ctx.FrameSaveFloat[Ctx.FrameSaveFloatTop + i];
  end;
  Dec(Ctx.FrameSaveStrTop, NS - BS);
  for i := BS to NS - 1 do
    Ctx.StringRegs[i] := Ctx.FrameSaveStr[Ctx.FrameSaveStrTop + (i - BS)];
  // RAII (V2): release the records this frame allocated (locals/temporaries) by rolling the
  // high-water mark back. Slots become reusable by the next AllocRecord. A UDT result has already
  // been copied into the caller-allocated instance (which lives below this frame's mark).
  if Mark <> nil then
  begin
    if Mark^.RecBase < Ctx.RecordCount then
      Ctx.RecordCount := Mark^.RecBase;
    // M8: discard any block marks this frame left dangling (e.g. EXIT SUB from inside a loop).
    Ctx.BlockRecMarkTop := Mark^.BlockMark;
  end
  else if (GFrameMark = 0) and (Ctx.FrameRecBaseTop > 0) then
  begin
    Dec(Ctx.FrameRecBaseTop);
    if Ctx.FrameRecBase[Ctx.FrameRecBaseTop] < Ctx.RecordCount then
      Ctx.RecordCount := Ctx.FrameRecBase[Ctx.FrameRecBaseTop];
    Ctx.BlockRecMarkTop := Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop];
  end;
end;

function TBytecodeVM.AllocRecord(Ctx: TExecutionContext; IntC, FloatC, StrC, TypeId: Integer): Integer;
// Allocate a record instance (heap block of typed slot arrays) in Ctx's per-thread heap and
// return its handle (an index into Ctx.Records).
begin
  // Reserve handle 0 as the null-pointer sentinel: a real record handle must never be 0, or a pointer
  // to the first-allocated record ("Dim As T b : Dim As T Ptr p = @b") would carry the value 0 and a
  // null check ("If p = 0", "While p <> 0") would wrongly see it as null. Index 0 stays an unused empty
  // slot. The shared-record region already keeps 0 free via SHARED_REC_FLAG (see AllocSharedRecord); this
  // mirrors that invariant for the per-thread heap. Matches FreeBASIC, where a valid @obj is never 0.
  // The guard sits here (not at each context reset) so it holds after every frame/block reclaim, which
  // restore RecordCount to a saved high-water mark that can be 0.
  if Ctx.RecordCount = 0 then
    Ctx.RecordCount := 1;
  if Ctx.RecordCount >= Length(Ctx.Records) then
    SetLength(Ctx.Records, (Ctx.RecordCount + 1) * 2);
  Ctx.Records[Ctx.RecordCount].TypeId := TypeId;
  SetLength(Ctx.Records[Ctx.RecordCount].IntData, IntC);
  SetLength(Ctx.Records[Ctx.RecordCount].FloatData, FloatC);
  SetLength(Ctx.Records[Ctx.RecordCount].StringData, StrC);
  Result := Ctx.RecordCount;
  Inc(Ctx.RecordCount);
end;

procedure TBytecodeVM.GrowSharedRecords(NeedLen: Integer);
// Grow the shared region's index -> record table WITHOUT ever freeing the array a reader might be
// holding. SetLength would reallocate and release the old block; a lock-free ResolveRec running on
// another thread could then dereference freed memory. So: allocate a new array, copy, and RETIRE the
// old one (kept alive until the program is unloaded) instead of letting it go.
//
// Geometric growth, so the retired copies total about the same as the final table - a few hundred KB
// for a program allocating millions of records, paid once. Callers hold FSharedRecLock: only the
// LOOKUP is lock-free, growth is still serialised.
var
  NewArr, NewStore: TSharedRecArray;
  i, NewLen, n: Integer;
begin
  NewLen := NeedLen * 2;
  if NewLen < 64 then NewLen := 64;
  SetLength(NewArr, NewLen);
  SetLength(NewStore, NewLen);
  // FSharedRecStore copies verbatim and cannot lose a write: it is only ever assigned under THIS lock,
  // when a block of indices is reserved.
  for i := 0 to Length(FSharedRecords) - 1 do
    NewStore[i] := FSharedRecStore[i];
  if not FHasWorkers then
  begin
    // One thread: nothing can be writing while we copy, so the live/free picture transfers exactly and
    // a freed handle goes on resolving to nil for the whole run.
    for i := 0 to Length(FSharedRecords) - 1 do
      NewArr[i] := FSharedRecords[i];
  end
  else
    // ⚠️ Several threads: FSharedRecords IS written outside the lock (that is the point — a publish
    // that took the lock would put back the cache-line traffic this whole design removes), so a
    // concurrent New's publish can land in the array we are copying FROM, after we have read that
    // entry. Losing it would leave a LIVE handle resolving to nil: a crash on correct code.
    //
    // So the copy is biased towards LIVE — every index that has storage is carried over as live. A
    // lost publish is then covered (the entry already holds the right storage, which for a given index
    // never changes), and what degrades instead is the other direction: an index freed before this
    // growth stops resolving to nil. That is the harmless one, and it is only ever a DIAGNOSTIC —
    // with several threads it was already unreliable, since any of them may reallocate the slot
    // between your Delete and your mistaken read. Single-threaded programs, where the guarantee is
    // real, take the branch above and keep it.
    for i := 0 to Length(FSharedRecords) - 1 do
      NewArr[i] := FSharedRecStore[i];
  if Length(FSharedRecords) > 0 then
  begin
    n := Length(FSharedRetired);
    // Both tables are retired, for the same reason: a thread that popped an index from its own cache
    // reads FSharedRecStore[Idx] without the lock, so the base it holds must stay live.
    SetLength(FSharedRetired, n + 2);
    FSharedRetired[n] := FSharedRecords;   // keep the old block alive; never freed while loaded
    FSharedRetired[n + 1] := FSharedRecStore;
  end;
  FSharedRecords := NewArr;
  FSharedRecStore := NewStore;
end;

function TBytecodeVM.AllocSharedRecord(IntC, FloatC, StrC, TypeId: Integer): Int64;
// M5.2c: allocate a record in the cross-thread shared region and return a SHARED_REC_FLAG-tagged
// handle. Each record is its own heap block (stable pointer), so a handle survives the outer array
// growing. Used for arrays of UDT, whose handles live in the global FArrays and are read by any thread.
var
  R: PRecordStorage;
  C: PRecCache;
  Idx, i: Integer;
begin
  // ⛔ ONE path only, deliberately. The A/B gate this function briefly had was a SECOND copy of the
  // body, and the copy drifted: with the gate off it measured 974 ms against the 325 ms of the code it
  // was supposed to reproduce. A baseline lives in a worktree on the previous commit, not in a
  // duplicated branch here.
  C := @GRecCache;
  if C^.Owner <> FVmGeneration then RecCacheAdopt(C);
  if C^.Count = 0 then
    RecCacheRefill(C);          // the ONLY place a plain New touches the region: dry cache
  Dec(C^.Count);
  Idx := C^.Idx[C^.Count];
  // Storage is parked at the index, so recovering it is a load, not a second shared stack to pop.
  R := FSharedRecStore[Idx];
  if R = nil then
  begin
    New(R);
    FSharedRecStore[Idx] := R;
  end;
  FSharedRecords[Idx] := R;
  // ⛔ Shaping and zeroing are OUTSIDE any lock, on purpose: this benchmark runs four workers that all
  // allocate, and every instruction under a shared lock is paid by everyone. Doing this work inside
  // cost the whole gain -- the single-threaded micro-benchmark improved 68% while binary-trees did not
  // move at all. It is safe here because the handle has not been handed back yet, so no other thread
  // can resolve it.
  R^.TypeId := TypeId;
  // On a recycled record these are almost always no-ops (same shape as the record that was freed),
  // and FPC returns immediately when the length already matches.
  SetLength(R^.IntData, IntC);
  SetLength(R^.FloatData, FloatC);
  SetLength(R^.StringData, StrC);
  // A recycled record must be indistinguishable from a fresh one: a brand-new SetLength zero-fills,
  // so recycling has to zero explicitly. (Strings were already emptied when it was retired.)
  for i := 0 to IntC - 1 do R^.IntData[i] := 0;
  for i := 0 to FloatC - 1 do R^.FloatData[i] := 0;
  Result := SHARED_REC_FLAG or Int64(Idx);
end;

procedure TBytecodeVM.FreeSharedRecord(Handle: Int64);
// DELETE: release a shared-region record and recycle its slot. A non-shared (per-thread) handle is
// ignored — those records are reclaimed by frame unwinding. Double-free / use-after-free are the
// programmer's responsibility (as in FreeBASIC).
var
  Idx, i: Integer;
  R: PRecordStorage;
  C: PRecCache;
begin
  if (Handle and SHARED_REC_FLAG) = 0 then Exit;
  Idx := Handle and SHARED_REC_MASK;
  if (Idx < 0) or (Idx >= FSharedRecordCount) then Exit;
  R := FSharedRecords[Idx];
  if R = nil then Exit;                  // already freed, or never handed out: ignore, as FreeBASIC does
  // ⛔ The slot still becomes nil: reading a freed handle must keep giving a loud failure rather than
  // somebody else's data. What is recycled is the STORAGE, which stays parked at FSharedRecStore[Idx]
  // where nothing can reach it through a handle.
  FSharedRecords[Idx] := nil;
  // Let go of the managed content (a parked record must hold nothing alive) but KEEP the arrays: their
  // blocks are the point of the recycling.
  for i := 0 to High(R^.StringData) do
    if R^.StringData[i] <> '' then R^.StringData[i] := '';
  C := @GRecCache;
  if C^.Owner <> FVmGeneration then RecCacheAdopt(C);
  if C^.Count >= REC_CACHE_CAP then
    RecCacheFlush(C);                    // hand a batch back so another thread can have them
  C^.Idx[C^.Count] := Idx;
  Inc(C^.Count);
end;

procedure TBytecodeVM.RecCacheAdopt(C: PRecCache);
// Bind this thread's cache to this VM. Indices held for a DIFFERENT VM are dropped rather than
// returned: that VM is being torn down (or already is), and its region goes with it.
begin
  C^.Owner := FVmGeneration;
  C^.Count := 0;
end;

procedure TBytecodeVM.RecCacheFlush(C: PRecCache);
// Return REC_CACHE_BATCH indices from the bottom of this thread's cache to the region, so a thread
// that frees far more than it allocates cannot starve the others. Bottom, not top, so the indices
// this thread is actively cycling through stay local and hot.
var
  i: Integer;
begin
  EnterCriticalSection(FSharedRecLock);
  try
    if FSharedRecFreeCount + REC_CACHE_BATCH > Length(FSharedRecFreeList) then
      SetLength(FSharedRecFreeList, (FSharedRecFreeCount + REC_CACHE_BATCH) * 2);
    for i := 0 to REC_CACHE_BATCH - 1 do
      FSharedRecFreeList[FSharedRecFreeCount + i] := C^.Idx[i];
    Inc(FSharedRecFreeCount, REC_CACHE_BATCH);
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
  for i := REC_CACHE_BATCH to C^.Count - 1 do
    C^.Idx[i - REC_CACHE_BATCH] := C^.Idx[i];
  Dec(C^.Count, REC_CACHE_BATCH);
end;

procedure TBytecodeVM.RecCacheRefill(C: PRecCache);
// Restock an empty cache: first from indices other threads gave back, then — if there are none — by
// reserving a fresh consecutive block of the region.
//
// ⚠️ Both branches preserve the order the old single global stack handed indices out in, which is what
// keeps a single-threaded program byte-identical. Returned indices keep their LIFO order (the region's
// top must still be the next one out); a fresh block is stored descending so that popping it yields
// First, First+1, ... exactly as appending one index at a time did.
var
  i, Take, First: Integer;
  R: PRecordStorage;
begin
  EnterCriticalSection(FSharedRecLock);
  try
    Take := FSharedRecFreeCount;
    if Take > REC_CACHE_BATCH then Take := REC_CACHE_BATCH;
    if Take > 0 then
    begin
      // The region's list is a LIFO: its top is the most recently returned index, and it must stay the
      // first one handed out. Copying the top `Take` entries straight across preserves that.
      for i := 0 to Take - 1 do
        C^.Idx[i] := FSharedRecFreeList[FSharedRecFreeCount - Take + i];
      Dec(FSharedRecFreeCount, Take);
      C^.Count := Take;
    end
    else
    begin
      // Reserve a fresh consecutive block. It starts small so a program that allocates a handful of
      // records does not pay for 512, and doubles up to the batch size for one that keeps going.
      Take := FRecBlockTake;
      if FRecBlockTake < REC_CACHE_BATCH then FRecBlockTake := FRecBlockTake * 2;
      First := FSharedRecordCount;
      if First + Take > Length(FSharedRecords) then
        GrowSharedRecords(First + Take);
      Inc(FSharedRecordCount, Take);
      for i := 0 to Take - 1 do
      begin
        C^.Idx[i] := First + (Take - 1 - i);      // descending: pops give First, First+1, ...
        // The storage is created HERE, under the lock, and parked at its index for good. That is what
        // keeps FSharedRecStore free of lock-free writes, which is in turn what lets GrowSharedRecords
        // copy it verbatim (see there). It costs one New per index over the program's life — the same
        // number the old code paid, just taken a block at a time.
        New(R);
        FSharedRecStore[First + i] := R;
      end;
      C^.Count := Take;
    end;
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
    // Lock-free by default. The lock here never protected the RECORD (its pointer is stable and field
    // access was already outside the lock) - only the pointer ARRAY, which SetLength could reallocate
    // and free under a reader. GrowSharedRecords retires the old array instead of freeing it, so this
    // read is always into live memory, and the entry for a live handle is valid in every array from
    // the one current when the handle was issued onwards.
    //
    // It is worth what it costs to reason about: this runs on EVERY field access of a shared record,
    // not just on New/Delete. Walking a tree of N nodes takes the region lock ~8 times per node
    // against 2 for allocating and freeing it, so the per-access lock was ~80% of the traffic, and
    // with several worker threads it was contended traffic.
    if FSharedRecLockFree then
      Result := FSharedRecords[Handle and SHARED_REC_MASK]
    else
    begin
      EnterCriticalSection(FSharedRecLock);
      Result := FSharedRecords[Handle and SHARED_REC_MASK];
      LeaveCriticalSection(FSharedRecLock);
    end;
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
  // FSharedRecStore, not FSharedRecords: it holds the storage of every index ever used, whether the
  // slot is currently live or parked. That is the whole point of parking by index — no record can be
  // stranded in some thread's cache, because a thread's cache holds integers.
  for i := 0 to FSharedRecordCount - 1 do
    if FSharedRecStore[i] <> nil then Dispose(FSharedRecStore[i]);
  SetLength(FSharedRecords, 0);
  SetLength(FSharedRecStore, 0);
  FSharedRecordCount := 0;
  SetLength(FSharedRecFreeList, 0);
  FSharedRecFreeCount := 0;
  // The retired pointer arrays hold no records of their own (every live record was disposed through
  // the current table above) - they were kept alive only so a lock-free lookup could never read freed
  // memory. Nothing can be looking any more, so release them.
  SetLength(FSharedRetired, 0);
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
    if DateLocaleMode then Exit(LocaleMonthName(n, full));
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
    if DateLocaleMode then Exit(LocaleDayName(n, full));
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
  // The framebuffer is not heap memory: SCREENPTR does not allocate, so Deallocate must not pretend to
  // free it. Silently ignoring the call, as FreeBASIC does for a null pointer, is safer than corrupting
  // the free list with an offset that means nothing in this region.
  if (RawPtr and RAWPTR_REGION_FB) <> 0 then Exit;
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
  // Reallocating the framebuffer is meaningless: its size is the screen's, set by SCREENRES.
  if (RawPtr and RAWPTR_REGION_FB) <> 0 then
    raise ERangeError.Create('Reallocate: SCREENPTR does not point to allocated memory');
  oldOfs := RawPtr and RAWPTR_OFS_MASK;
  oldSz := PtrUInt((@FRawHeap[oldOfs - 8])^);
  Result := RawAlloc(ByteCount);
  newOfs := Result and RAWPTR_OFS_MASK;
  copySz := oldSz;
  if PtrUInt((@FRawHeap[newOfs - 8])^) < copySz then copySz := PtrUInt((@FRawHeap[newOfs - 8])^);
  Move(FRawHeap[oldOfs], FRawHeap[newOfs], copySz);
  RawFree(RawPtr);
end;

function TBytecodeVM.RawAddr(RawPtr: Int64; NeedBytes: PtrUInt): Pointer;
// Resolve a tagged raw pointer to a real address inside whichever REGION it names, after checking that
// NeedBytes bytes starting there actually fit. Every raw load, store and block operation goes through
// here, so a raw pointer can never address memory the VM does not own.
//
// The bounds check is not decoration: RawLoadInt/RawStoreInt used to index FRawHeap with an unchecked
// offset, so pointer arithmetic that walked off the end of a block read or wrote past the array -- the
// same memory-unsafety class as the out-of-bounds superinstruction fixed earlier.
var
  ofs: PtrUInt;
  Data: PByte;
  SizeBytes: Integer;
begin
  if (RawPtr and RAWPTR_TAG) = 0 then
    raise ERangeError.Create('Null or invalid raw pointer dereference');
  ofs := PtrUInt(RawPtr and RAWPTR_OFS_MASK);

  if (RawPtr and RAWPTR_REGION_FB) <> 0 then
  begin
    // SCREENPTR region: the working page's framebuffer.
    if not Assigned(FGraphics) or not FGraphics.SurfaceData(FGfxWorkSurface, Data, SizeBytes) then
      raise ERangeError.Create('SCREENPTR dereference: no graphics screen (call SCREENRES first)');
    if (SizeBytes <= 0) or (ofs + NeedBytes > PtrUInt(SizeBytes)) then
      raise ERangeError.CreateFmt('SCREENPTR dereference out of bounds: offset %d + %d > %d bytes',
                                  [Int64(ofs), Int64(NeedBytes), Int64(SizeBytes)]);
    Result := Pointer(Data + ofs);
    Exit;
  end;

  // Byte-heap region. Offset 0..7 is the reserved NULL block (see RawAlloc).
  if (ofs < 8) or (ofs + NeedBytes > PtrUInt(Length(FRawHeap))) then
    raise ERangeError.CreateFmt('Raw pointer dereference out of bounds: offset %d + %d > %d bytes',
                                [Int64(ofs), Int64(NeedBytes), Int64(Length(FRawHeap))]);
  Result := @FRawHeap[ofs];
end;

function TBytecodeVM.RawLoadInt(RawPtr: Int64; TypeCode: Integer): Int64;
begin
  case TypeCode of
    RTC_I8:  Result := PShortInt(RawAddr(RawPtr, 1))^;
    RTC_I16: Result := PSmallInt(RawAddr(RawPtr, 2))^;
    RTC_I32: Result := PLongInt(RawAddr(RawPtr, 4))^;
  else
    Result := PInt64(RawAddr(RawPtr, 8))^;
  end;
end;

function TBytecodeVM.RawLoadFloat(RawPtr: Int64; TypeCode: Integer): Double;
begin
  if TypeCode = RTC_SINGLE then Result := PSingle(RawAddr(RawPtr, 4))^
  else Result := PDouble(RawAddr(RawPtr, 8))^;
end;

procedure TBytecodeVM.FilePrintColSet(Handle, Col: Integer);
begin
  if (Handle < 0) or (Handle > 4095) then Exit;   // defensive cap; real handles are tiny
  if Handle > High(FFilePrintCols) then SetLength(FFilePrintCols, Handle + 1);
  FFilePrintCols[Handle] := Col;
end;

function TBytecodeVM.FilePrintColGet(Handle: Integer): Integer;
begin
  if (Handle < 0) or (Handle > High(FFilePrintCols)) then Exit(0);
  Result := FFilePrintCols[Handle];
end;

procedure TBytecodeVM.FilePrintColAdvance(Handle: Integer; const Data: string);
// Advance the handle's column by the written text; a CR/LF inside resets to the tail length.
var
  i, LastNL: Integer;
begin
  LastNL := 0;
  for i := Length(Data) downto 1 do
    if (Data[i] = #13) or (Data[i] = #10) then begin LastNL := i; Break; end;
  if LastNL > 0 then
    FilePrintColSet(Handle, Length(Data) - LastNL)
  else
    FilePrintColSet(Handle, FilePrintColGet(Handle) + Length(Data));
end;

function DirAllowedAttrs(Mask: Integer): Integer;
// The attribute bits a DIR entry may carry and still be returned, for a given FreeBASIC attrib_mask.
//
// FreeBASIC's rule is not the DOS one and not a plain intersection; it was read off the oracle with a
// directory holding a plain file, a read-only file, a hidden file and a subdirectory, over twelve masks:
//
//   mask 0     -> plain files only          mask fbDirectory       -> DIRECTORIES ONLY, no plain files
//   fbReadOnly -> plain + the read-only     fbDirectory Or fbArchive -> directories AND plain files
//   fbHidden   -> plain + the hidden        all bits               -> everything
//
// So: an entry is returned when every bit it carries is allowed, ARCHIVE being allowed implicitly --
// except when the mask asks for directories, where archive is NOT implied and the plain files (which
// all carry it on Windows) drop out. Both halves of that are observable, which is why it is spelled out
// here rather than folded into FindFirst's own mask.
begin
  Result := Mask;
  if (Mask and faDirectory) = 0 then
    Result := Result or faArchive;
end;

function TBytecodeVM.RawLoadBytesVal(RawPtr: Int64; Count: Integer): string;
// Exactly Count bytes at the raw address, terminator or not: a fixed-length string FIELD of a UDT laid
// over raw memory occupies its declared width whatever it contains.
var
  P: PByte;
begin
  Result := '';
  if Count <= 0 then Exit;
  P := PByte(RawAddr(RawPtr, PtrUInt(Count)));    // validates the whole span
  SetLength(Result, Count);
  Move(P^, Result[1], Count);
end;

function TBytecodeVM.RawLoadZStrVal(RawPtr: Int64; Wide: Boolean): string;
// "*p" where p is a ZSTRING PTR (Wide=False) or WSTRING PTR (Wide=True): the C string AT the
// pointed address, read up to the NUL terminator - never past the end of the byte heap (a block
// missing its NUL yields the bytes to the region end instead of walking off it). WSTRING is
// stored as UCS-2 units and converted to the VM's uniform UTF-8 managed string.
var
  P: PByte;
  ofs, Limit, n: PtrUInt;
  W: UnicodeString;
  PW: PWord;
begin
  P := PByte(RawAddr(RawPtr, 1));                      // validates region + at least one byte
  ofs := PtrUInt(RawPtr and RAWPTR_OFS_MASK);
  if (RawPtr and RAWPTR_REGION_FB) <> 0 then
    Limit := 0                                          // a framebuffer is not text: empty string
  else
    Limit := PtrUInt(Length(FRawHeap)) - ofs;
  if not Wide then
  begin
    n := 0;
    while (n < Limit) and (P[n] <> 0) do Inc(n);
    SetLength(Result, n);
    if n > 0 then Move(P^, Result[1], n);
  end
  else
  begin
    PW := PWord(P);
    n := 0;
    while (n * 2 + 1 < Limit) and (PW[n] <> 0) do Inc(n);
    SetLength(W, n);
    if n > 0 then Move(PW^, W[1], n * 2);
    Result := UTF8Encode(W);
  end;
end;

procedure TBytecodeVM.RawStoreZStrVal(RawPtr: Int64; const S: string; Wide: Boolean);
// "*p = s" where p is a ZSTRING/WSTRING PTR: the string's characters + NUL terminator at the
// pointed address. Bounds-checked as a whole through RawAddr - an overflowing store raises
// instead of corrupting the heap (fbc would silently overrun).
var
  P: PByte;
  W: UnicodeString;
begin
  if not Wide then
  begin
    P := PByte(RawAddr(RawPtr, PtrUInt(Length(S)) + 1));
    if Length(S) > 0 then Move(S[1], P^, Length(S));
    P[Length(S)] := 0;
  end
  else
  begin
    W := UTF8Decode(S);
    P := PByte(RawAddr(RawPtr, PtrUInt(Length(W)) * 2 + 2));
    if Length(W) > 0 then Move(W[1], P^, PtrUInt(Length(W)) * 2);
    PWord(P)[Length(W)] := 0;
  end;
end;

procedure TBytecodeVM.RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
begin
  case TypeCode of
    RTC_I8:  PShortInt(RawAddr(RawPtr, 1))^ := ShortInt(Value);
    RTC_I16: PSmallInt(RawAddr(RawPtr, 2))^ := SmallInt(Value);
    RTC_I32: PLongInt(RawAddr(RawPtr, 4))^ := LongInt(Value);
  else
    PInt64(RawAddr(RawPtr, 8))^ := Value;
  end;
end;

procedure TBytecodeVM.RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
begin
  if TypeCode = RTC_SINGLE then PSingle(RawAddr(RawPtr, 4))^ := Value
  else PDouble(RawAddr(RawPtr, 8))^ := Value;
end;

// FB_MEMCOPY / FB_MEMMOVE: copy ByteCount bytes from SrcPtr to DstPtr. Both pointers are resolved
// through RawAddr, so either may name the byte heap or the framebuffer, and both ends are bounds-checked
// against their own region. FPC Move is overlap-safe, so this serves both the (non-overlapping) memcopy
// and the (overlap-safe) memmove semantics.
procedure TBytecodeVM.RawMemCopy(DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);
begin
  if ByteCount = 0 then Exit;
  Move(RawAddr(SrcPtr, ByteCount)^, RawAddr(DstPtr, ByteCount)^, ByteCount);
end;

// CLEAR: set ByteCount bytes at DstPtr to Value, in whichever region DstPtr names.
procedure TBytecodeVM.RawClear(DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);
begin
  if ByteCount = 0 then Exit;
  FillChar(RawAddr(DstPtr, ByteCount)^, ByteCount, Value);
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
  // Allocate a record only for elements that do not already have one. A valid array-of-UDT element
  // handle is a shared-region record (SHARED_REC_FLAG set), so it is never 0 — a 0 handle marks an
  // uninitialized slot. After a plain DIM every slot is 0, so all are filled; after REDIM [PRESERVE]
  // only the freshly-grown slots are 0, so existing records are kept (no clobber / leak).
  for k := 0 to FArrays[ArrayId].TotalSize - 1 do
    if FArrays[ArrayId].IntData[k] = 0 then
      FArrays[ArrayId].IntData[k] := AllocSharedRecord(IntC, FloatC, StrC, TypeId);
end;

function TBytecodeVM.AllocSharedRecordBlock(N, IntC, FloatC, StrC, TypeId: Integer): Int64;
// Allocate N records at CONSECUTIVE shared-region indices (always append, never reuse a freed slot) and
// return the first's SHARED_REC_FLAG handle. Because the indices are consecutive, "handle + i" (a plain
// pointer add) yields the i-th record's handle — the basis for "p[i]" on a Callocate(n, SizeOf(T)) block.
var
  i, firstIdx: Integer;
  R: PRecordStorage;
begin
  if N < 1 then N := 1;
  EnterCriticalSection(FSharedRecLock);
  try
    firstIdx := FSharedRecordCount;
    for i := 0 to N - 1 do
    begin
      New(R);
      R^.TypeId := TypeId;
      SetLength(R^.IntData, IntC);
      SetLength(R^.FloatData, FloatC);
      SetLength(R^.StringData, StrC);
      if FSharedRecordCount >= Length(FSharedRecords) then
        GrowSharedRecords(FSharedRecordCount + 1);
      FSharedRecords[FSharedRecordCount] := R;
      FSharedRecStore[FSharedRecordCount] := R;
      Inc(FSharedRecordCount);
    end;
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
  Result := SHARED_REC_FLAG or Int64(firstIdx);
end;

procedure TBytecodeVM.DeepCopyArrayRecords(Ctx: TExecutionContext; DestArr, SrcArr: Int64; PackedCounts: Int64);
// FreeBASIC value semantics of an array-of-UDT member: give the destination its OWN element records,
// each holding an independent copy of the corresponding source element's contents (so "Dim b = a" and
// return-by-value do not share element instances). The dest handle array is resized to match src; each
// dest element is reused if present (contents overwritten) or freshly allocated. Record contents are
// copied one level deep (Int/Float/StringData via Copy) — a nested UDT/array inside an element is copied
// as its handle (shallow at that deeper level), matching the SSA EmitRecordCopy depth for arrays.
var
  IntC, FloatC, StrC, TypeId, k: Integer;
  SrcRec, DestRec: PRecordStorage;
begin
  if (DestArr < 1) or (DestArr > High(FArrays)) or (SrcArr < 1) or (SrcArr > High(FArrays)) then Exit;
  IntC := PackedCounts and $FFFF;
  FloatC := (PackedCounts shr 16) and $FFFF;
  StrC := (PackedCounts shr 32) and $FFFF;
  TypeId := (PackedCounts shr 48) and $FFFF;
  // Match the destination's shape to the source. On a size change, release the dest's current element
  // records first (this is a distinct value instance, so they are not aliased) to avoid a leak.
  if FArrays[DestArr].TotalSize <> FArrays[SrcArr].TotalSize then
  begin
    for k := 0 to FArrays[DestArr].TotalSize - 1 do
      if FArrays[DestArr].IntData[k] <> 0 then FreeSharedRecord(FArrays[DestArr].IntData[k]);
    FArrays[DestArr].ElementType := FArrays[SrcArr].ElementType;
    FArrays[DestArr].DimCount    := FArrays[SrcArr].DimCount;
    FArrays[DestArr].TotalSize   := FArrays[SrcArr].TotalSize;
    FArrays[DestArr].Dimensions  := Copy(FArrays[SrcArr].Dimensions);
    FArrays[DestArr].LowerBounds := Copy(FArrays[SrcArr].LowerBounds);
    SetLength(FArrays[DestArr].IntData, FArrays[SrcArr].TotalSize);
    for k := 0 to FArrays[DestArr].TotalSize - 1 do FArrays[DestArr].IntData[k] := 0;
  end;
  for k := 0 to FArrays[SrcArr].TotalSize - 1 do
  begin
    if FArrays[DestArr].IntData[k] = 0 then
      FArrays[DestArr].IntData[k] := AllocSharedRecord(IntC, FloatC, StrC, TypeId);
    SrcRec := ResolveRec(Ctx, FArrays[SrcArr].IntData[k]);
    DestRec := ResolveRec(Ctx, FArrays[DestArr].IntData[k]);
    if (SrcRec <> nil) and (DestRec <> nil) then
    begin
      DestRec^.TypeId := SrcRec^.TypeId;
      DestRec^.IntData := Copy(SrcRec^.IntData);
      DestRec^.FloatData := Copy(SrcRec^.FloatData);
      DestRec^.StringData := Copy(SrcRec^.StringData);
    end;
  end;
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
  // A worker pushes/pops its own call frames, so it needs the same saved widths as the main context.
  WCtx.FrameSaveIntCount := FCtx.FrameSaveIntCount;
  WCtx.FrameSaveFloatCount := FCtx.FrameSaveFloatCount;
  WCtx.FrameSaveStrCount := FCtx.FrameSaveStrCount;
  WCtx.FrameMarkTop := 0;    // the worker's own frame-mark stack starts empty
  WCtx.FrameWidthTop := 0;   // FRAMEMARK=0 layout
  WCtx.FrameRecBaseTop := 0;
  SizeIntBank(WCtx, WCtx.IntRegCount);
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
  Spawn.Ctx.ModeSwitchPC := -1;  // no TRON/TROFF switch pending (0 would read as "resume at PC 0")
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
    // Refuse to spawn past the live-worker ceiling. A well-formed program never approaches it (this
    // machine class runs a handful of workers); blowing past it means a runaway spawn, which without
    // this guard takes the whole host down rather than just failing the program.
    if FLiveWorkers >= MAX_LIVE_WORKERS then
    begin
      Spawn.Ctx.Free;
      Spawn.Free;
      raise Exception.CreateFmt(
        'THREADCREATE: live worker limit (%d) exceeded -- runaway thread creation', [MAX_LIVE_WORKERS]);
    end;
    Idx := Length(FWorkerThreads);
    SetLength(FWorkerThreads, Idx + 1);
    FWorkerThreads[Idx] := Spawn;
    FHasWorkers := True;
    Inc(FLiveWorkers);
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
  OldSize, NewSize, i, Shift: Integer;
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

        // Grow both working and temp register arrays. The integer bank carries the relocatable
        // frame region above the logical slots, so growing the logical part has to SLIDE that
        // region up by the same amount - any frame currently relocated into it is live, and its
        // view offset moves with it. Doing this wrong would corrupt an in-flight recursion rather
        // than fail visibly, so the region is moved before the new logical slots are cleared.
        Shift := NewSize - OldSize;
        SetLength(Ctx.IntRegsMem, NewSize + FRAME_REGION_SLOTS);
        SetLength(Ctx.TempIntRegs, NewSize);
        if Ctx.RegHwI > OldSize then                      // frames are live in the region
          Move(Ctx.IntRegsMem[OldSize], Ctx.IntRegsMem[NewSize], (Ctx.RegHwI - OldSize) * SizeOf(Int64));
        for i := OldSize to NewSize - 1 do
        begin
          Ctx.IntRegsMem[i] := 0;
          Ctx.TempIntRegs[i] := 0;
        end;
        if Ctx.RegDeltaI > 0 then Inc(Ctx.RegDeltaI, Shift);
        if Ctx.RegHwI > OldSize then Inc(Ctx.RegHwI, Shift) else Ctx.RegHwI := NewSize;
        Ctx.RegFrameCap := NewSize + FRAME_REGION_SLOTS;
        Ctx.IntRegs := @Ctx.IntRegsMem[Ctx.RegDeltaI];    // the view must follow the reallocation

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

  // Does anything in this program read the terminal's modelled screen back? Only SCREEN(row, col) and
  // a PEEK/POKE of the C128 screen RAM can, and both are visible right here in the bytecode. When
  // none of them is present the grid is write-only, and keeping it costs a per-character pass over
  // every byte printed. Whole-program and decided once, like the AOT's GNoThreads.
  //
  // ⚠️ Conservative on purpose: PEEK and POKE count whatever address they carry, because the address
  // is a runtime value and the screen is only part of what they can reach. Getting this wrong in the
  // other direction would make SCREEN() return spaces, so the doubt goes to keeping the model.
  FProgReadsScreen := False;
  FProgPeeks := False;
  if Assigned(FProgram) then
    for i := 0 to FProgram.GetInstructionCount - 1 do
    begin
      // ⛔ NOT "OpCode in [...]": an opcode is TWO bytes (bcConScreen = $0414) and a Pascal set holds
      // 0..255, so the membership test silently compares the low byte and matches nothing. It compiled
      // clean and made this scan always answer "no observer" -- caught by m358_view_print.
      if FProgram.GetInstruction(i).OpCode = bcConScreen then FProgReadsScreen := True
      else if (FProgram.GetInstruction(i).OpCode = bcPeek) or
              (FProgram.GetInstruction(i).OpCode = bcPoke) then FProgPeeks := True;
      if FProgReadsScreen and FProgPeeks then Break;
    end;
  UpdateScreenModelGate;

  // AOT: functions compiled for a previous program are keyed by its PCs - drop them.
  // The host re-registers (RegisterAotFunc) after loading the new program.
  for i := 0 to High(FNativeFuncs) do FNativeFuncs[i].Free;
  SetLength(FNativeFuncs, 0);

  // PRINT number spacing is dialect-specific. CLASSIC (Commodore v7) pads a non-negative number with a
  // leading space AND appends a trailing one. FreeBASIC keeps the leading pad but prints NO trailing space
  // -- the FB manual states the trailing space is a -lang qb trait. Switch the preset for a MODERN program
  // so its output matches real FreeBASIC character for character.
  if Assigned(FConsoleBehavior) and Assigned(FProgram) and FProgram.ModernMode then
  begin
    // -lang qb sources keep the FB zone width but ADD the trailing space after numerics
    // ("In the -lang qb dialect, an extra space is printed after numbers" - fbc-verified live: ' 15 '
    // then zone pad). Only after an INTEGER, though: a Single or a Double keeps the leading sign pad
    // and nothing after it, which is what nfQB says and nfCommodore (real v7, where a float gets the
    // trailing space too) does not.
    if FProgram.QBLang then
      FConsoleBehavior.NumberFormat := nfQB
    else
      FConsoleBehavior.NumberFormat := nfFreeBASIC;
    // A comma in PRINT tabs to the next zone, and the zone width is dialect-specific too: Commodore uses
    // 10 columns, FreeBASIC 14 ("A comma indicates printing should take place at the next 14 column
    // boundary" -- the FB manual's Print page).
    FConsoleBehavior.CommaTabSize := 14;
    // A comma zone that would fall past the end of the line wraps to the next one instead, and the
    // behaviour's line width was still the Commodore 40. A FreeBASIC console is 80 wide (which is what
    // the terminal device itself already reports), so a PRINT with several comma zones broke in half
    // around column 40 -- "Print a, "x ="; b, "y ="; c" came out on two lines.
    FConsoleBehavior.ScreenCols := 80;
  end;

  // RESERVE the whole static array-id space up front. Static arrays have compile-time FArrays indices, but
  // a UDT array member gets its handle at RUNTIME by appending at Length(FArrays). Growing FArrays lazily
  // (only as each static array is DIM'd) let a member array claim an id still owed to a static one — most
  // often a param placeholder, which is never DIM'd at all — and the two then ALIAS the same storage.
  if FProgram.GetArrayCount > Length(FArrays) then
    SetLength(FArrays, FProgram.GetArrayCount);

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
        bcDivUInt, bcModUInt, bcCmpLtUInt, bcCmpGtUInt, bcCmpLeUInt, bcCmpGeUInt,
        bcBitwiseAnd, bcBitwiseOr, bcBitwiseXor, bcBitwiseNot, bcShl, bcShr, bcShrUInt,
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

        // Int Src1 -> String Dest (CHR$, HEX$, ERR$, SPACE, OCT, BIN, MK*int).
        // HEX/OCT/BIN also take Src2 = the "digits" width register (0 = natural length).
        bcStrChr, bcStrHex, bcStrErr, bcStrSpace, bcStrOct, bcStrBin, bcStrWChr, bcStrMkInt:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if (Instr.OpCode = bcStrHex) or (Instr.OpCode = bcStrOct) or (Instr.OpCode = bcStrBin) then
            if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
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
        bcStrInstr, bcStrInstrRev, bcStrInstrRevAny, bcStrInstrAny, bcStrInstrW, bcStrInstrRevW:
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

  // Call frames only need to snapshot the registers the program can actually touch: the same
  // scan that sized the banks gives the highest index per bank, and the banks keep their
  // 256-slot floor. Saving the whole floor cost 256 refcounted string copies per call.
  FCtx.FrameSaveIntCount := MaxIntReg + 1;
  FCtx.FrameSaveFloatCount := MaxFloatReg + 1;
  FCtx.FrameSaveStrCount := MaxStringReg + 1;

  // Narrow further per callee: a call only has to protect what its callee subtree can touch.
  // Any failure here is not fatal - the empty tables simply mean "program-wide width everywhere".
  try
    BuildProcFrameWidths;
    BuildCallSiteLiveness;   // ...and again by what each CALL SITE still needs after the call
  except
    SetLength(FProcWidths, 0);
    SetLength(FProcWidthInt, 0); SetLength(FProcWidthFloat, 0); SetLength(FProcWidthStr, 0);
    SetLength(FCallLiveInt, 0);
  end;
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

procedure TBytecodeVM.ImageConvertRowExec(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
// IMAGECONVERTROW(src, src_bpp, dst, dst_bpp, width [, isrgb]): copy one row of pixels from one raw
// address to another, converting the colour information to the destination's depth.
//
// Both ends live on the raw heap (or the framebuffer region), so every access goes through RawLoadInt/
// RawStoreInt, which bounds-check their own region: a width that runs off the row is caught rather
// than silently reading whatever follows.
//
// Depths, per the manual: source 1-8 (paletted), 24 or 32; destination 1-8, 16 or 32. Paletted values
// are palette INDICES and are looked up through the current palette when the destination is a
// full-colour depth; a paletted destination takes the index through unchanged (fbc requires the two
// palettes to match, and we cannot check that either). isrgb = 0 swaps red and blue.
var
  SrcAddr, DstAddr: Int64;
  SrcBpp, DstBpp, Width, IsRgb, i: Integer;
  Px, R, G, B, A: Int64;

  function ReadPixel(Idx: Integer): Int64;
  begin
    case SrcBpp of
      24: Result := RawLoadInt(SrcAddr + Idx * 3, RTC_I32) and $00FFFFFF;
      32: Result := RawLoadInt(SrcAddr + Idx * 4, RTC_I32) and $FFFFFFFF;
    else  Result := RawLoadInt(SrcAddr + Idx, RTC_I8) and $FF;   // 1..8 bpp: one palette index per byte
    end;
  end;

begin
  SrcAddr := Ctx.IntRegs[Instr.Src1];
  DstAddr := Ctx.IntRegs[Instr.Src2];
  SrcBpp  := Ctx.IntRegs[Instr.Immediate and $FFFF];
  DstBpp  := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];
  Width   := Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF];
  IsRgb   := Ctx.IntRegs[(Instr.Immediate shr 48) and $FFFF];
  if (Width <= 0) or (SrcAddr = 0) or (DstAddr = 0) then Exit;

  for i := 0 to Width - 1 do
  begin
    Px := ReadPixel(i);
    if SrcBpp <= 8 then
    begin
      if DstBpp <= 8 then
      begin
        RawStoreInt(DstAddr + i, RTC_I8, Px);        // paletted -> paletted: the index travels as-is
        Continue;
      end;
      if Assigned(FGraphics) then Px := Int64(FGraphics.GetPaletteColor(Integer(Px)))
      else Px := 0;
    end;
    // Engine layout is ABGR ($AABBGGRR), so R is the low byte and B the high one of the colour.
    R := Px and $FF;
    G := (Px shr 8) and $FF;
    B := (Px shr 16) and $FF;
    if SrcBpp = 32 then A := (Px shr 24) and $FF else A := $FF;
    if IsRgb = 0 then begin Px := R; R := B; B := Px; end;   // "the channels are the other way round"
    case DstBpp of
      16: // RGB565: red in the HIGH bits (11-15), green 5-10, blue 0-4. That layout is the standard's,
          // not a choice - fbc packs it the same way, so this half is comparable byte for byte.
        RawStoreInt(DstAddr + i * 2, RTC_I16,
                    ((R shr 3) shl 11) or ((G shr 2) shl 5) or (B shr 3));
      32:
        RawStoreInt(DstAddr + i * 4, RTC_I32, R or (G shl 8) or (B shl 16) or (A shl 24));
    else
      RawStoreInt(DstAddr + i, RTC_I8, R);           // a full-colour source into a paletted row
    end;
  end;
end;

function TBytecodeVM.CommandLine(Index: Integer): string;
// COMMAND$(index): index < 0 -> the whole command line (program args, space-separated); 0 -> the
// executable name; n >= 1 -> the n-th program argument ('' if out of range). FProgramArgs holds the
// arguments only (arg 1 at FProgramArgs[0]); the interpreter/script name is excluded.
//
// Two indices below that are NOT a COMMAND$ the user can write: they carry FreeBASIC's __FB_ARGC__ and
// __FB_ARGV__, which are values of the implicit main and have no spelling of their own. Both are
// RUNTIME facts, so neither can be a preprocessor constant; and neither is worth an opcode of its own
// (see the opcode checklist: compose from what exists unless there is no field left to discriminate
// with). The index field was free below -1, so the macros expand to COMMAND$(-2)/(-3) and convert.
//   -2 -> argc, as decimal text: the argument count INCLUDING the program name, as fbc counts it.
//   -3 -> argv, as decimal text: the raw address of a freshly built vector of pointers to NUL-
//         terminated copies of the arguments, so "*argv[i]" reads the i-th one.
var
  i: Integer;
  Vec, SPtr: Int64;
begin
  if Index = -2 then
    Exit(IntToStr(Length(FProgramArgs) + 1));      // + the program name, which is argv[0]
  if Index = -3 then
  begin
    // Built on demand and owned by the raw heap. One allocation for the vector plus one per argument;
    // they live as long as the program does, which is what a C argv is entitled to assume.
    Vec := RawAlloc(PtrUInt((Length(FProgramArgs) + 1) * SizeOf(Int64)));
    if (Vec and RAWPTR_TAG) = 0 then Exit('0');
    SPtr := StrSAdd(ParamStr(0));
    RawStoreInt(Vec, RTC_I64, SPtr);
    for i := 0 to High(FProgramArgs) do
    begin
      SPtr := StrSAdd(FProgramArgs[i]);
      RawStoreInt(Vec + (i + 1) * SizeOf(Int64), RTC_I64, SPtr);
    end;
    Exit(IntToStr(Vec));
  end;
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

procedure TBytecodeVM.UpdateScreenModelGate;
// Must the terminal keep its modelled screen up to date? Only if something in THIS program can read
// it back. Two things can:
//
//   SCREEN(row, col)        -- present in both dialects, so it counts on its own;
//   PEEK/POKE of screen RAM -- CLASSIC C128 machinery, and it reaches the cells ONLY through an
//                              IMemoryMapper. The headless CLI installs none (the mapper is created
//                              by the windowed console and by sbv), so there PEEK returns 0 and POKE
//                              is dropped: a MODERN program cannot observe the model through them,
//                              and must not be made to pay for it.
//
// Called from LoadProgram and again from SetMemoryMapper, because either can arrive first.
begin
  GScreenModelObservable := FProgReadsScreen or (FProgPeeks and Assigned(FMemoryMapper));
end;

procedure TBytecodeVM.SetMemoryMapper(Mapper: IMemoryMapper);
begin
  FMemoryMapper := Mapper;
  UpdateScreenModelGate;   // a mapper arriving later can make PEEK/POKE observers after all
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
  // -1 = not measured yet, so FramePush falls back to the whole bank. LoadProgram replaces these
  // with the program's real widths, which are legitimately 0 for a bank it never touches -- which
  // is why 0 must NOT read as "unmeasured" (that cost 256 float copies per call in an int-only
  // program, the residual call overhead after the first pass at this).
  FCtx.FrameSaveIntCount := -1;
  FCtx.FrameSaveFloatCount := -1;
  FCtx.FrameSaveStrCount := -1;
  FCtx.FrameMarkTop := 0;
  FCtx.FrameWidthTop := 0;      // FRAMEMARK=0 layout
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

procedure TBytecodeVM.SetErrorProc(const AProcName: string);
begin
  FCtx.LastErrorProc := AProcName;
end;

function TBytecodeVM.ReadChars(Count, Handle: Integer; Wide: Boolean): string;
// Backs both INPUT(n [, [#]f]) and WINPUT(n [, [#]f]).
//
// The two differ only in what they count. INPUT counts bytes. WINPUT counts wide characters, and a
// WSTRING here is UTF-8 with codepoint-aware LEN, so a wide character is one Unicode codepoint and may
// span several bytes: keep pulling until Count lead bytes have been seen, a lead byte being anything
// that is not a UTF-8 continuation byte (10xxxxxx). Either way the bytes read are returned whole.
//
// Handle = 0 reads from the keyboard (unechoed, like FreeBASIC), otherwise from that file number.
// Short reads are not an error: end of file simply returns fewer characters, as FreeBASIC does.
var
  Got, ErrorCode: Integer;
  Data: string;
  Ch: Char;
begin
  Result := '';
  if Count <= 0 then Exit;
  Got := 0;
  while Got < Count do
  begin
    if Handle = 0 then
    begin
      if not Assigned(FInputDevice) then Break;
      Ch := FInputDevice.ReadKey;
      if Ch = #0 then Break;            // no key available / input exhausted
      Data := Ch;
    end
    else
    begin
      if not Assigned(FOnFileData) then
        raise Exception.Create('INPUT/WINPUT: no file handler assigned');
      Data := '';
      ErrorCode := 0;
      FOnFileData(Self, 'GET#', Handle, Data, ErrorCode);
      if ErrorCode <> 0 then
        raise Exception.CreateFmt('INPUT/WINPUT error %d reading from file %d', [ErrorCode, Handle]);
      if Data = '' then Break;          // end of file
    end;
    Result := Result + Data;
    // Bytes count one apiece; wide characters count only on a lead byte, so a multi-byte character is
    // read whole rather than cut in half.
    if (not Wide) or ((Length(Data) > 0) and ((Ord(Data[1]) and $C0) <> $80)) then
      Inc(Got);
  end;
end;

procedure TBytecodeVM.ClearErrorState;
begin
  FCtx.LastErrorLine := 0;
  FCtx.LastErrorCode := 0;
  FCtx.LastErrorMessage := '';
  FCtx.LastErrorProc := '';
end;

procedure TBytecodeVM.SetTrueValue(AValue: Int64);
begin
  FTrueValue := AValue;
end;

function TBytecodeVM.ComputeBuiltinFP(OpId: Int64; X: Double): Double;
// Apply a math builtin taken as a function pointer (@Sin etc.), per the op id in a BUILTIN_FP_TAG value.
begin
  case OpId of
    1:  Result := System.Sin(X);
    2:  Result := System.Cos(X);
    3:  Result := Math.Tan(X);
    4:  Result := System.ArcTan(X);
    5:  Result := SqrtFloat(X);
    6:  Result := System.Exp(X);
    7:  Result := System.Ln(X);
    8:  Result := System.Abs(X);
    9:  Result := Math.ArcSin(X);
    10: Result := Math.ArcCos(X);
    11: Result := Math.Sinh(X);
    12: Result := Math.Cosh(X);
    13: Result := Math.Tanh(X);
    14: Result := System.Int(X);
  else  Result := 0.0;
  end;
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
  HandleNum64: Int64;   // indirect-call target (entry PC, or a BUILTIN_FP_TAG @Sin sentinel)
  VaIdx: Integer;       // CVA_ARG: cursor into the variadic slot stack
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
    bcDivUInt:   // unsigned 64-bit \ : reinterpret both registers as QWord
      if Ctx.IntRegs[Instr.Src2] <> 0 then
        Ctx.IntRegs[Instr.Dest] := Int64(QWord(Ctx.IntRegs[Instr.Src1]) div QWord(Ctx.IntRegs[Instr.Src2]))
      else raise Exception.Create('Division by zero');
    bcModUInt:
      if Ctx.IntRegs[Instr.Src2] <> 0 then
        Ctx.IntRegs[Instr.Dest] := Int64(QWord(Ctx.IntRegs[Instr.Src1]) mod QWord(Ctx.IntRegs[Instr.Src2]))
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
    // The IMPLICIT float -> int conversion: FreeBASIC ROUNDS (to nearest, ties to even), it does not
    // truncate. It rounds everywhere the conversion is implicit -- assignment, argument passing, an array
    // store, an array INDEX, a FOR bound, a FUNCTION result -- so "Dim As Integer i : i = 1.5" is 2, and
    // "a(1.5)" is element 2. Truncation is what Int() and Fix() are for, and they have their own opcodes.
    // CLASSIC keeps truncating: Commodore v7 assigns 1.7 to an integer variable as 1.
    bcFloatToInt: Ctx.IntRegs[Instr.Dest] := FloatToIntConv(Ctx.FloatRegs[Instr.Src1],
                                                            Assigned(FProgram) and FProgram.ModernMode);
    // Numeric -> string (FreeBASIC Str() / "&" concat): no leading sign-space, unlike v7 STR$.
    bcIntToString: Ctx.StringRegs[Instr.Dest] := IntToStr(Ctx.IntRegs[Instr.Src1]);
    bcFloatToString:
      // FreeBASIC Str()/"&" concat of a float: the number with no leading sign-space and no trailing
      // field-space (FormatNumber adds both under the Commodore preset). Immediate = 1 when the value is
      // SINGLE-typed: 7 significant digits, as PRINT gives it.
      Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1],
                                                                       Instr.Immediate = 1));
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
    bcCmpLtUInt: if QWord(Ctx.IntRegs[Instr.Src1]) < QWord(Ctx.IntRegs[Instr.Src2]) then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGtUInt: if QWord(Ctx.IntRegs[Instr.Src1]) > QWord(Ctx.IntRegs[Instr.Src2]) then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpLeUInt: if QWord(Ctx.IntRegs[Instr.Src1]) <= QWord(Ctx.IntRegs[Instr.Src2]) then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
    bcCmpGeUInt: if QWord(Ctx.IntRegs[Instr.Src1]) >= QWord(Ctx.IntRegs[Instr.Src2]) then Ctx.IntRegs[Instr.Dest] := FTrueValue else Ctx.IntRegs[Instr.Dest] := 0;
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
    // SHR is ARITHMETIC on a signed operand in MODERN (FreeBASIC copies the sign bit into the vacated
    // bits: "-5 Shr 2" = -2). FPC's "shr" is logical, so it needs the helper. CLASSIC keeps the logical
    // shift it has always had -- v7 has no SHR of its own, so there is no reason to move it. The unsigned
    // variant, which the SSA selects for a UInteger/ULongInt operand, is zero-filling in both.
    bcShr:     if Assigned(FProgram) and FProgram.ModernMode then
                 Ctx.IntRegs[Instr.Dest] := ArithShr64(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2])
               else
                 Ctx.IntRegs[Instr.Dest] := LogicalShr64(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
    bcShrUInt: Ctx.IntRegs[Instr.Dest] := LogicalShr64(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
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
        GrowCallStackIfNeeded(Ctx);
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
        FramePush(Ctx, Instr.Immediate, Ctx.PC);   // narrowed by callee footprint AND caller liveness
        GrowCallStackIfNeeded(Ctx);
        Ctx.CallStack[Ctx.CallStackPtr] := Ctx.PC;
        Inc(Ctx.CallStackPtr);
        Ctx.PC := Instr.Immediate - 1;
      end;
    bcCallSubIndirect:  // FreeBASIC function pointer: same as bcCallSub but the target entry PC is in Src1 (int reg)
      begin
        HandleNum64 := Ctx.IntRegs[Instr.Src1];
        if (HandleNum64 and BUILTIN_FP_TAG) <> 0 then
          // @Sin etc.: no real PC — compute the math op on the Double arg (float xfer slot 0) and write
          // the Double result slot. No jump; the main loop advances to the next instruction.
          Ctx.XferFloat[255] := ComputeBuiltinFP(HandleNum64 and $FF, Ctx.XferFloat[0])   // 255 = XFER_RESULT_SLOT
        else
        begin
          // A function pointer that was never assigned holds 0, and anything outside the program is not
          // an entry point either: jumping there ran whatever bytes followed and surfaced as an access
          // violation somewhere else entirely. Report it where it happens.
          if (HandleNum64 <= 0) or (HandleNum64 >= FProgram.GetInstructionCount) then
            raise ERangeError.Create('Call through an unset or invalid procedure pointer');
          FramePush(Ctx, HandleNum64, Ctx.PC);   // indirect: the entry PC is the register value
          GrowCallStackIfNeeded(Ctx);
          Ctx.CallStack[Ctx.CallStackPtr] := Ctx.PC;
          Inc(Ctx.CallStackPtr);
          Ctx.PC := HandleNum64 - 1;
        end;
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
    // Threads, mutexes and condition variables (M5.4). These lived ONLY in RunTemplate's inline case,
    // which is why they sat on AotHelperUnsafeOp's deny-list: with no handler here the AOT helper
    // would have run them as a silent no-op, so a region touching one bailed outright. That cost the
    // native path the MAIN of essentially every parallel program plus the WORKER of mandelbrot and
    // spectral-norm - "survey: 0/2 regions eligible" and the whole program interpreted.
    //
    // Each one is the same single call the inline arm makes, and none of them carries call-site
    // semantics (no PC games, no Ctx.Running check), so the two paths cannot drift in behaviour.
    // Routing them through the generic helper is cheap in the way it was NOT cheap for a string
    // primitive: these are OS-level operations, so a register flush around one is noise next to
    // what the operation itself costs.
    //
    // ⚠️ UNDER SUSPICION: the machine froze on 30 Jul with this change in the tree, while running the
    // three thread-heavy benchmarks. Nothing in the Windows logs, which is the signature of resource
    // exhaustion rather than a fault. Never proved. Re-measure ONE program at a time, smallest size
    // first, foreground, with a short timeout, checking for stray processes after each run.
    bcThreadCreate:  Ctx.IntRegs[Instr.Dest] := SpawnWorker(Ctx.IntRegs[Instr.Src1], Ctx);
    bcThreadWait:    JoinWorker(Ctx.IntRegs[Instr.Src1]);
    bcThreadDetach:  DetachWorker(Ctx.IntRegs[Instr.Src1]);
    bcThreadSelf:    Ctx.IntRegs[Instr.Dest] := GSelfHandle;
    bcMutexCreate:   Ctx.IntRegs[Instr.Dest] := CreateMutex;
    bcMutexLock:     LockMutex(Ctx.IntRegs[Instr.Src1]);
    bcMutexUnlock:   UnlockMutex(Ctx.IntRegs[Instr.Src1]);
    bcMutexDestroy:  DestroyMutex(Ctx.IntRegs[Instr.Src1]);
    bcCondCreate:    Ctx.IntRegs[Instr.Dest] := CreateCond;
    bcCondWait:      CondWaitOp(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2]);
    bcCondSignal:    CondSignalOp(Ctx.IntRegs[Instr.Src1]);
    bcCondBroadcast: CondBroadcastOp(Ctx.IntRegs[Instr.Src1]);
    bcCondDestroy:   DestroyCond(Ctx.IntRegs[Instr.Src1]);
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
    bcRecordNewArrayInd:
      // Array-of-UDT MEMBER: the FArrays id is a runtime handle in IntRegs[Src1]. Imm=packed slot counts.
      RecordNewArrayInit(Ctx, Ctx.IntRegs[Instr.Src1], Instr.Immediate);
    bcRecordNewBlock:  // Callocate(n, SizeOf(T)) of a UDT: n consecutive shared records; Dest = first handle
      Ctx.IntRegs[Instr.Dest] := AllocSharedRecordBlock(Ctx.IntRegs[Instr.Src1],
                                   Instr.Immediate and $FFFF, (Instr.Immediate shr 16) and $FFFF,
                                   (Instr.Immediate shr 32) and $FFFF, (Instr.Immediate shr 48) and $FFFF);
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
        // (Src2), prefixed by the module path - fbc -eassert prints "path(line): assertion failed
        // at FUNC: expr" and the path half only exists at run time (the program's ModuleName).
        // For ASSERT (Immediate bit 0 set) the program also halts; ASSERTWARN continues.
        if Ctx.IntRegs[Instr.Src1] = 0 then
        begin
          if Assigned(FOutputDevice) then
          begin
            if Assigned(FProgram) then
              FOutputDevice.Print(FProgram.ModuleName + Ctx.StringRegs[Instr.Src2])
            else
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
        // FreeBASIC "Sleep n" is n MILLISECONDS (not seconds); the argument is a millisecond count. A bare
        // "Sleep" (no argument) waits for a keypress in FB — headless there is none, so approximate with a
        // short fixed wait. (The sleep duration never affects program OUTPUT, which is deterministic, so
        // this only changes wall-clock time — but treating ms as seconds made "Sleep 100" hang ~100 s.)
        if Instr.Immediate > 0 then
          SleepMs := Instr.Immediate
        else if Instr.Src1 < Ctx.FloatRegCount then
          SleepMs := Trunc(Ctx.FloatRegs[Instr.Src1])
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
          // Define or clear function key. Immediate = -1 marks the no-text form ("KEY n"):
          // an absent operand lowers to register 0, which is a valid string register, so
          // "Src2 < StringRegCount" could never tell a clear from a define and copied whatever
          // string lived in R0 into the table.
          if (Instr.Immediate = -1) or (Instr.Src2 >= Ctx.StringRegCount) then
            FFunctionKeys[KeyNum] := ''  // Clear key
          else
            FFunctionKeys[KeyNum] := Ctx.StringRegs[Instr.Src2];
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
          // Immediate = -1 flags "line number in Src1" (register 0 is valid - the old
          // Src1 > 0 test alone silently degraded RESUME <line> to plain RESUME when the
          // line landed in R0). Src1 > 0 is kept as a fallback for older .basc files.
          if (Instr.Immediate = -1) or (Instr.Src1 > 0) then
          begin
            // RESUME <line> with line number in register
            Ctx.PC := FindPCForSourceLine(Ctx.IntRegs[Instr.Src1]);
          end
          else if Instr.Immediate > 0 then
          begin
            // RESUME <line> with constant line number in Immediate
            Ctx.PC := FindPCForSourceLine(Instr.Immediate);
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
        // ERROR <n> - raise a runtime error number n. The run-loop except handler reads the
        // code into ERR and transfers to any active ON ERROR / TRAP handler (or aborts if none).
        // Known codes report their standard BASIC message (e.g. 10 -> NEXT WITHOUT FOR, also
        // reached by the compiler's orphan-NEXT lowering); unknown ones fall back to "ERROR n".
        raise TExecutorRuntimeException.CreateWithCode(
          GetErrorCodeDescription(Ctx.IntRegs[Instr.Src1]), Ctx.IntRegs[Instr.Src1]);
      end;
    // === FreeBASIC variadic arguments (CVA_*) ===
    bcVarArgCtl:
      if Instr.Immediate = 0 then
      begin
        // The CALLER opens a frame just before staging the surplus arguments.
        SetLength(FVarArgFrames, Length(FVarArgFrames) + 1);
        FVarArgFrames[High(FVarArgFrames)] := Length(FVarArgs);
      end
      else if Length(FVarArgFrames) > 0 then
      begin
        // ...and closes it once the call returns, discarding that call's slots.
        SetLength(FVarArgs, FVarArgFrames[High(FVarArgFrames)]);
        SetLength(FVarArgFrames, Length(FVarArgFrames) - 1);
      end;
    bcVarArgPushInt, bcVarArgPushFloat, bcVarArgPushStr:
      begin
        SetLength(FVarArgs, Length(FVarArgs) + 1);
        with FVarArgs[High(FVarArgs)] do
        begin
          IntVal := 0; FloatVal := 0; StrVal := '';
          case Instr.OpCode of
            bcVarArgPushFloat: begin Bank := 1; FloatVal := Ctx.FloatRegs[Instr.Src1]; end;
            bcVarArgPushStr:   begin Bank := 2; StrVal := Ctx.StringRegs[Instr.Src1]; end;
          else                 begin Bank := 0; IntVal := Ctx.IntRegs[Instr.Src1]; end;
          end;
        end;
      end;
    bcVarArgBase:
      // CVA_START: the cursor at the first argument of the frame this call opened.
      if Length(FVarArgFrames) > 0 then
        Ctx.IntRegs[Instr.Dest] := FVarArgFrames[High(FVarArgFrames)]
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    bcVarArgGetInt, bcVarArgGetFloat, bcVarArgGetStr:
      begin
        // CVA_ARG: the slot AT the cursor, converted to the type the callee named. Reading past the
        // end answers a zero/empty value rather than faulting: C would be undefined here, and a
        // diagnosable nothing is the better of the two.
        VaIdx := Integer(Ctx.IntRegs[Instr.Src1]);
        if (VaIdx < 0) or (VaIdx > High(FVarArgs)) then
        begin
          case Instr.OpCode of
            bcVarArgGetFloat: Ctx.FloatRegs[Instr.Dest] := 0;
            bcVarArgGetStr:   Ctx.StringRegs[Instr.Dest] := '';
          else Ctx.IntRegs[Instr.Dest] := 0;
          end;
        end
        else
          case Instr.OpCode of
            bcVarArgGetFloat:
              if FVarArgs[VaIdx].Bank = 1 then Ctx.FloatRegs[Instr.Dest] := FVarArgs[VaIdx].FloatVal
              else if FVarArgs[VaIdx].Bank = 2 then Ctx.FloatRegs[Instr.Dest] := StrToFloatDef(FVarArgs[VaIdx].StrVal, 0)
              else Ctx.FloatRegs[Instr.Dest] := FVarArgs[VaIdx].IntVal;
            bcVarArgGetStr:
              if FVarArgs[VaIdx].Bank = 2 then Ctx.StringRegs[Instr.Dest] := FVarArgs[VaIdx].StrVal
              else if FVarArgs[VaIdx].Bank = 1 then Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(FVarArgs[VaIdx].FloatVal, False))
              else Ctx.StringRegs[Instr.Dest] := IntToStr(FVarArgs[VaIdx].IntVal);
          else
            if FVarArgs[VaIdx].Bank = 1 then Ctx.IntRegs[Instr.Dest] := Trunc(FVarArgs[VaIdx].FloatVal)
            else if FVarArgs[VaIdx].Bank = 2 then Ctx.IntRegs[Instr.Dest] := StrToInt64Def(FVarArgs[VaIdx].StrVal, 0)
            else Ctx.IntRegs[Instr.Dest] := FVarArgs[VaIdx].IntVal;
          end;
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
          // Src2 is a FLOAT value here; the exact-integer form is bcPrintUsingInt (below).
          FOutputDevice.Print(FormatUsingString(Ctx.StringRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]));
      end;
    bcPrintUsingInt:
      // PRINT USING with an EXACT integer value: Src1 = format string, Src2 = int value. A LongInt beyond
      // 2^53 keeps every digit instead of being rounded through a Double (Pell's 2469645423824185801).
      begin
        if Assigned(FOutputDevice) then
          FOutputDevice.Print(FormatUsingString(Ctx.StringRegs[Instr.Src1], 0.0, True, Ctx.IntRegs[Instr.Src2]));
      end;
    bcPrintUsingStage:
      // Stage one already-stringified value for a runtime-format PRINT USING (Src1 = string register).
      begin
        SetLength(FPUStage, Length(FPUStage) + 1);
        FPUStage[High(FPUStage)] := Ctx.StringRegs[Instr.Src1];
      end;
    bcPrintUsingRun:
      // Run a runtime-format PRINT USING over the staged values (Src1 = format string register).
      begin
        if Assigned(FOutputDevice) then
          FOutputDevice.Print(FormatUsingRuntime(Ctx.StringRegs[Instr.Src1]))
        else
          SetLength(FPUStage, 0);
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
        // COPY/FILECOPY "src", "dest" [, overwrite]
        // Src1 = source path, Src2 = dest path, Immediate = overwrite flag INT REGISTER index
        // (that is where the compiler has always put Src3 - the old code read the flag from
        // IntRegs[Dest] with Dest=0, i.e. whatever garbage R0 held). Immediate = -1 is the
        // FreeBASIC function form FileCopy(src, dst): always overwrite, error code into Dest.
        if Instr.Immediate = -1 then
          Ctx.IntRegs[Instr.Dest] := FsCopyFileCode(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2])
        else if FProgram.ModernMode then
          // FreeBASIC's FILECOPY STATEMENT does not stop the program when the copy fails - it answers
          // through the function form, or not at all. Raising here made "FileCopy "source.txt", ..." on
          // a missing file an abort where fbc simply carries on. CLASSIC's COPY keeps its Commodore
          // behaviour: a failed disk copy is a runtime error there.
          FsCopyFileCode(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2])
        else
          ExecuteCopyFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2],
                         Ctx.IntRegs[Instr.Immediate] <> 0);
      end;

    bcScratch:
      if Instr.Immediate = -1 then
        // FreeBASIC function form Kill(file): delete one file, error code into Dest
        // (0 ok, 2 file not found, 13 delete failed) - no v7 prompt/pattern semantics.
        Ctx.IntRegs[Instr.Dest] := FsKillCode(Ctx.StringRegs[Instr.Src1])
      else
      begin
        // SCRATCH "pattern" [, flags]
        // Src1 = pattern, Src2 = flags (int reg): 1 = silent, 2 = force, 3 = both
        ExecuteScratch(Ctx.StringRegs[Instr.Src1],
          (Ctx.IntRegs[Instr.Src2] and 2) <> 0,  // force (bit 1)
          (Ctx.IntRegs[Instr.Src2] and 1) <> 0); // silent (bit 0)
      end;

    bcRenameFile:
      // RENAME "old", "new" - Src1 = old name, Src2 = new name. Immediate = -1 is FreeBASIC's function
      // form Name(old, new), which answers 0 or an error code instead of raising.
      if Instr.Immediate = -1 then
      begin
        if not FileExists(Ctx.StringRegs[Instr.Src1]) then
          Ctx.IntRegs[Instr.Dest] := 2                      // no such file
        else if RenameFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]) then
          Ctx.IntRegs[Instr.Dest] := 0
        else
          Ctx.IntRegs[Instr.Dest] := 1;
      end
      else
        ExecuteRenameFile(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);

    bcConcat:
      begin
        // CONCAT "src", "dest"
        // Src1 = source, Src2 = dest (append src to dest)
        ExecuteConcat(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);
      end;

    bcMkdir:
      // Immediate = -1: FreeBASIC function form MkDir(path) - error code into Dest (0/-1).
      if Instr.Immediate = -1 then
        Ctx.IntRegs[Instr.Dest] := FsMkdirCode(Ctx.StringRegs[Instr.Src1])
      else
        // MKDIR "path" - Src1 = path
        ExecuteMkdir(Ctx.StringRegs[Instr.Src1]);

    bcSetEnviron:
      begin
        // SETENVIRON "NAME=value": record a VM-internal override (consulted by ENVIRON$). A bare "NAME"
        // (no '=') clears the value. Portable — no OS-specific setenv needed.
        SetEnvOverride(Ctx.StringRegs[Instr.Src1]);
      end;

    bcShell:
      // SHELL cmd: run the command through the platform shell. Immediate = -1 is the FreeBASIC
      // function form Shell(cmd): exit code into Dest. The STATEMENT form must NOT touch Dest -
      // it is 0 there, and the old unconditional store clobbered live int register R0.
      //
      // Immediate = -2 is RUN / CHAIN / EXEC: the same launch, but of a PROGRAM rather than a shell
      // command line, and FreeBASIC answers -1 when it cannot be started at all. Going through the
      // shell would answer the shell's own "command not found" code instead, so the file is checked
      // first - which also means a missing program is never handed to a shell to interpret.
      if Instr.Immediate = -2 then
      begin
        if not FileExists(Ctx.StringRegs[Instr.Src1]) then
          Ctx.IntRegs[Instr.Dest] := -1
        else if Ctx.StringRegs[Instr.Src2] <> '' then
          Ctx.IntRegs[Instr.Dest] := RunShellCommand(Ctx.StringRegs[Instr.Src1] + ' ' + Ctx.StringRegs[Instr.Src2])
        else
          Ctx.IntRegs[Instr.Dest] := RunShellCommand(Ctx.StringRegs[Instr.Src1]);
      end
      else if Instr.Immediate = -1 then
        Ctx.IntRegs[Instr.Dest] := RunShellCommand(Ctx.StringRegs[Instr.Src1])
      else
        RunShellCommand(Ctx.StringRegs[Instr.Src1]);

    bcChdir:
      // Immediate = -1: FreeBASIC function form ChDir(path) - error code into Dest (0/-1).
      if Instr.Immediate = -1 then
        Ctx.IntRegs[Instr.Dest] := FsChdirCode(Ctx.StringRegs[Instr.Src1])
      else
        // CHDIR "path" - Src1 = path
        ExecuteChdir(Ctx.StringRegs[Instr.Src1]);

    bcRmdir:
      // Immediate = -1: FreeBASIC function form RmDir(path) - error code into Dest (0/-1).
      if Instr.Immediate = -1 then
        Ctx.IntRegs[Instr.Dest] := FsRmdirCode(Ctx.StringRegs[Instr.Src1])
      else
        // RMDIR "path" (FreeBASIC/QB) - remove an empty directory; Src1 = path
        ExecuteRmdir(Ctx.StringRegs[Instr.Src1]);

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
  ElemVal: Double;   // scratch for bounds-guarded array element reads
  CharPos: Integer;  // bcStrConcatCharAt: the 1-based index into the table
  CharVal2: Integer; // bcStrMidAssign: how many bytes actually get overwritten
  MidRepl: AnsiString; // bcStrMidAssign: the replacement, held across a possible Dest/Src2 collision
  SrcLen: Integer;   // ...length of the accumulator being extended
  CharVal: AnsiChar; // ...and the byte taken from the table
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

    // Fused array-store-constant - sub-opcodes 80-82. Bounds-guarded to match the base ExecuteArrayOp
    // store path: MODERN drops an out-of-bounds store (memory-safe), CLASSIC/--bounds-check raises.
    80: // bcArrayStoreIntConst
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
        FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] := Instr.Immediate;
    81: // bcArrayStoreFloatConst
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
        FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^);
    82: // bcArrayStoreStringConst
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
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

    // Array Load + Arithmetic - sub-opcodes 110-112. Bounds-guarded: an out-of-bounds read yields the
    // element default (0.0) in MODERN, matching the base ExecuteArrayOp load path; CLASSIC raises.
    110: // bcArrayLoadAddFloat: dest = acc + arr[idx]
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] + FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate];
    111: // bcArrayLoadSubFloat: dest = acc - arr[idx]
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] - FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate];
    112: // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
      begin
        if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
          ElemVal := FArrays[Instr.Src1].FloatData[Ctx.IntRegs[Instr.Src2]]
        else
          ElemVal := 0.0;
        if Abs(Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF]) < 1e-300 then
          Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
            DivZeroFloat(ElemVal)   // MODERN: IEEE; CLASSIC: error
        else
          Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
            ElemVal / Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF];
      end;

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
      Ctx.FloatRegs[Instr.Dest] := SqrtFloat(Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2]);

    // Array Load + Branch - sub-opcodes 140-141. Bounds-guarded: an out-of-bounds read is treated as the
    // element default 0 in MODERN (matching the base load path) — NZ does not branch, Z branches; CLASSIC raises.
    140: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
      begin
        if FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] <> 0 then
          Ctx.PC := Instr.Immediate - 1;
      end;
    141: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
      begin
        if FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]] = 0 then
          Ctx.PC := Instr.Immediate - 1;
      end
      else
        Ctx.PC := Instr.Immediate - 1;   // OOB read = 0 -> zero-branch taken

    // Array Reverse Range - sub-opcode 156
    156: // bcArrayReverseRange: reverse arr[start..end-1] in-place
      begin
        Ctx.StartIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.EndIdx := Ctx.IntRegs[Instr.Dest] - 1;
        Ctx.ArrIdxTmp := Instr.Src1;
        // Bounds-guard the whole contiguous range once (endpoints valid => interior valid).
        if (Ctx.StartIdx < Ctx.EndIdx) and
           (not ArrayBoundsOK(Ctx.ArrIdxTmp, Ctx.StartIdx) or not ArrayBoundsOK(Ctx.ArrIdxTmp, Ctx.EndIdx)) then
          Ctx.StartIdx := Ctx.EndIdx;   // MODERN: skip out-of-range reversal (CLASSIC already raised)
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
        // Bounds-guard the touched range [start .. end+1] once; skip the whole rotate if out of range (MODERN).
        if ArrayBoundsOK(Ctx.ArrIdxTmp, Ctx.StartIdx) and ArrayBoundsOK(Ctx.ArrIdxTmp, Ctx.EndIdx + 1) then
        begin
          Ctx.FirstVal := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.StartIdx];
          Ctx.LoopIdx := Ctx.StartIdx;
          while Ctx.LoopIdx <= Ctx.EndIdx do
          begin
            FArrays[Ctx.ArrIdxTmp].IntData[Ctx.LoopIdx] := FArrays[Ctx.ArrIdxTmp].IntData[Ctx.LoopIdx + 1];
            Inc(Ctx.LoopIdx);
          end;
          FArrays[Ctx.ArrIdxTmp].IntData[Ctx.EndIdx + 1] := Ctx.FirstVal;
        end;
      end;

    // "acc + MID$(tab, k, 1)" fused - sub-opcode 158.
    158: // bcStrConcatCharAt: Dest := Src1 + tab[k], with no one-character string ever built.
      begin
        // Which byte (if any) MID$(tab, k, 1) would yield. Every branch mirrors bcStrMid's, in the
        // same order and with the same dialect rules - the two must not drift apart. The length is
        // 1 by construction (the fusion only fires on a literal 1), so the negative-length arm of
        // bcStrMid has no counterpart here.
        // ⛔ The table is read IN PLACE, never copied into a local: "T := Ctx.StringRegs[i]" is a
        // managed assignment, so it costs a reference count up and down on EVERY iteration of a
        // per-character loop. Measured: doing that made this fusion 26% SLOWER than the two
        // instructions it replaces, which is the opposite of the point.
        CharPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        if (CharPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          CharPos := 0                         // FB: a start below 1 is an empty string
        else
        begin
          if CharPos < 1 then CharPos := 1;    // CLASSIC clamps
          if CharPos > Length(Ctx.StringRegs[Instr.Src2]) then CharPos := 0;
        end;
        if CharPos = 0 then
        begin
          // Nothing to append: the result is the accumulator unchanged.
          if Instr.Dest <> Instr.Src1 then
            Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1];
        end
        else if Instr.Dest = Instr.Src1 then
          // The shape this exists for: grow the accumulator in place, no allocation at all.
          AppendChar(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src2][CharPos])
        else
        begin
          // ⛔ NOT "Src1 + Ctx.StringRegs[Src2][CharPos]": in FPC "string + char" converts the char
          // into a temporary AnsiString first, so that form ALLOCATES -- which is the very cost this
          // opcode exists to remove. Measured: the concatenating version made reverse-complement 9%
          // slower under --aot and 24% interpreted, i.e. worse than the two instructions it replaces.
          CharVal := Ctx.StringRegs[Instr.Src2][CharPos];
          SrcLen := Length(Ctx.StringRegs[Instr.Src1]);
          // See ConcatCharTo: release the shared buffer before resizing, or SetLength copies the old
          // contents that the Move below is about to replace anyway.
          Ctx.StringRegs[Instr.Dest] := '';
          SetLength(Ctx.StringRegs[Instr.Dest], SrcLen + 1);
          if SrcLen > 0 then
            Move(Ctx.StringRegs[Instr.Src1][1], Ctx.StringRegs[Instr.Dest][1], SrcLen);
          Ctx.StringRegs[Instr.Dest][SrcLen + 1] := CharVal;
        end;
      end;

    // "acc += tab[Asc(MID$(s, i, 1)) + 1]" fused whole - sub-opcode 159.
    159: // bcStrAppendMapped: Dest += Src2[Ord(Src1[Immediate]) + 1]
      begin
        // The three steps this replaces, in the same order and with the same dialect rules as the
        // opcodes it fuses: bcStrAscMid, then the +1, then bcStrConcatCharAt.
        //
        // ⚠️ An out-of-range source index does NOT mean "append nothing", which is what this arm
        // used to do. bcStrAscMid answers 0 for an empty substring, the +1 turns that into 1, and
        // bcStrConcatCharAt then appends tab[1] - the table's FIRST byte. Skipping the append
        // instead diverges from the very sequence this opcode replaces, silently and only for
        // out-of-range indices, which is why reverse-complement never showed it.
        // ⛔ Read both strings IN PLACE. Assigning either to a local AnsiString is a managed
        // assignment and costs a reference count up and down per CHARACTER - the mistake that made
        // the first version of bcStrConcatCharAt slower than the instructions it replaced.
        CharPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        if (CharPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          CharPos := 0                       // FB: a start below 1 is an empty string
        else
        begin
          if CharPos < 1 then CharPos := 1;  // CLASSIC clamps
          if CharPos > Length(Ctx.StringRegs[Instr.Src1]) then CharPos := 0;
        end;
        // The byte's CODE indexes the table, 1-based as everywhere in BASIC; an empty substring
        // contributes code 0, hence index 1. A table too short for that index appends nothing
        // rather than reading past its end.
        if CharPos > 0 then
          SrcLen := Ord(Ctx.StringRegs[Instr.Src1][CharPos]) + 1
        else
          SrcLen := 1;
        if (SrcLen >= 1) and (SrcLen <= Length(Ctx.StringRegs[Instr.Src2])) then
          AppendChar(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src2][SrcLen]);
      end;

    // "MID$(t, start [, len]) = src" - sub-opcode 160.
    160: // bcStrMidAssign: overwrite Length(Src2) bytes of Dest starting at Immediate, IN PLACE
      begin
        // The FreeBASIC MID STATEMENT never changes Len(t): it overwrites at most what fits. So for a
        // start INSIDE the string this is a bounded Move into t's own buffer, and the old lowering --
        // "Left(t, start-1) + Left(src, avail) + Mid(t, start+n)" -- was rebuilding the whole string
        // to write a few bytes. Filling a buffer character by character was therefore quadratic.
        //
        // Src2 arrives ALREADY capped to the requested length (the ssaStrLeft the lowering emits), so
        // the only clamp left is the room remaining in Dest.
        // ⚠️ Dest, Src1 and Src2 can all be the same register once the allocator has had its say, and
        // "Dest := Src1" would then destroy the replacement before it is read. Take a reference to it
        // FIRST when they collide -- a managed assignment, so a reference count, not a copy.
        if (Instr.Dest <> Instr.Src1) and (Instr.Dest = Instr.Src2) then
        begin
          MidRepl := Ctx.StringRegs[Instr.Src2];
          Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1];
        end
        else
        begin
          if Instr.Dest <> Instr.Src1 then
            Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1];
          MidRepl := Ctx.StringRegs[Instr.Src2];
        end;
        CharPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        SrcLen := Length(Ctx.StringRegs[Instr.Dest]);
        // start past the end writes nothing (the rebuild produced the original string unchanged);
        // start below 1 is outside the statement's definition and keeps the old general behaviour,
        // which the lowering still emits for that case -- see EmitMidAssign.
        if (CharPos >= 1) and (CharPos <= SrcLen) then
        begin
          CharVal2 := Length(MidRepl);
          if CharVal2 > SrcLen - CharPos + 1 then CharVal2 := SrcLen - CharPos + 1;
          if CharVal2 > 0 then
          begin
            // UniqueString, not SetLength: the length does not change, we only need the right to
            // write. On the register that owns the buffer this is a no-op -- and THAT is where the
            // linear behaviour comes from, so Dest and Src1 must be the same register (see the
            // lowering, which emits the variable's canonical register for both).
            UniqueString(Ctx.StringRegs[Instr.Dest]);
            Move(MidRepl[1], Ctx.StringRegs[Instr.Dest][CharPos], CharVal2);
          end;
        end;
      end;

    // Array Swap (Int) - sub-opcode 250. Bounds-guarded: skip the swap if either index is out of range (MODERN); CLASSIC raises.
    250: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) and
         ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Dest]) then
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

    // Array Load to register (Int) - sub-opcode 253. Bounds-guarded: OOB read yields default 0 (MODERN); CLASSIC raises.
    253: // bcArrayLoadIntTo: r[dest] = arr[src1][r[src2]]
      if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
        Ctx.IntRegs[Instr.Dest] := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.IntRegs[Instr.Dest] := 0;

    // Array Copy Element - sub-opcode 254. Bounds-guarded: OOB store dropped, OOB source reads default 0 (MODERN); CLASSIC raises.
    254: // bcArrayCopyElement: arr_dest[idx] = arr_src[idx]
      if ArrayBoundsOK(Instr.Dest, Ctx.IntRegs[Instr.Src2]) then
      begin
        if ArrayBoundsOK(Instr.Src1, Ctx.IntRegs[Instr.Src2]) then
          FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Instr.Src1].IntData[Ctx.IntRegs[Instr.Src2]]
        else
          FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := 0;
      end;

    // Array Move Element - sub-opcode 255. Bounds-guarded like 254.
    255: // bcArrayMoveElement: arr[dest_idx] = arr[src_idx]
      if ArrayBoundsOK(Instr.Dest, Ctx.IntRegs[Instr.Src2]) then
      begin
        if ArrayBoundsOK(Instr.Dest, Ctx.IntRegs[Instr.Src1]) then
          FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src1]]
        else
          FArrays[Instr.Dest].IntData[Ctx.IntRegs[Instr.Src2]] := 0;
      end;

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

{ EnsureDenseOps - decode-once dense dispatch table (VM perf plan, milestone M2).
  Translate every instruction's 16-bit (group.sub) opcode to its dense linear index ONCE, so the hot
  loop dispatches on a single compact case (no per-instruction group extraction / superinstruction
  branch). Rebuilt only when the loaded program changes. The on-file bytecode and the in-memory
  TBytecodeInstruction.OpCode are left untouched -- serialization and disassembly are unaffected. }
procedure TBytecodeVM.EnsureDenseOps;
type
  PBytecodeInstr = ^TBytecodeInstruction;
var
  i, n: Integer;
  Ins: PBytecodeInstr;
begin
  if FProgram = nil then Exit;
  n := FProgram.GetInstructionCount;
  if (FDenseOpsFor = FProgram) and (Length(FDenseOps) = n) then Exit;
  SetLength(FDenseOps, n);
  Ins := PBytecodeInstr(FProgram.GetInstructionsPtr);
  if Ins <> nil then
    for i := 0 to n - 1 do
      FDenseOps[i] := Word(Op16ToDense(Ins[i].OpCode));
  {$IFDEF JIT_PROFILE}
  // J1: (re)size the back-edge counters for this program and clear them.
  SetLength(FBackEdgeCount, n);
  if n > 0 then FillDWord(FBackEdgeCount[0], n, 0);
  {$ENDIF}
  FDenseOpsFor := FProgram;
  if FJitEnabled then BuildJitLoops;
end;

{ BuildJitLoops - compile every eligible hot loop of the current program to native (JIT J2/J3). A loop
  header is the target of a backward branch; its body runs to the LAST branch that jumps back to it.
  CompileLoop returns nil for any loop with an unsupported opcode, so those stay interpreted. }
procedure TBytecodeVM.BuildJitLoops;
type
  PBcInstr = ^TBytecodeInstruction;
var
  i, n, hdr: Integer;
  Ins: PBcInstr;
  Op: Word;
  HeaderEnd: array of Integer;   // header PC -> highest back-edge source (loop end), -1 if not a header
  Mem: TExecMem;
  RecTmp: TRecordStorage;        // to derive TRecordStorage layout for the JIT record accessor (J13)
  RecSz, RIntOff, RFloatOff: Integer;
begin
  RecSz := SizeOf(TRecordStorage);
  RIntOff := Integer(PtrUInt(@RecTmp.IntData) - PtrUInt(@RecTmp));
  RFloatOff := Integer(PtrUInt(@RecTmp.FloatData) - PtrUInt(@RecTmp));
  n := FProgram.GetInstructionCount;
  for i := 0 to High(FNativeLoops) do FNativeLoops[i].Free;
  SetLength(FNativeLoops, 0);
  SetLength(FNativeLoops, n);   // all nil
  if n = 0 then Exit;
  Ins := PBcInstr(FProgram.GetInstructionsPtr);

  SetLength(HeaderEnd, n);
  for i := 0 to n - 1 do HeaderEnd[i] := -1;
  for i := 0 to n - 1 do
  begin
    Op := Ins[i].OpCode;
    if (Op = bcJump) or (Op = bcJumpIfZero) or (Op = bcJumpIfNotZero) then
    begin
      hdr := Integer(Ins[i].Immediate);
      if (hdr >= 0) and (hdr < i) and (i > HeaderEnd[hdr]) then
        HeaderEnd[hdr] := i;
    end;
  end;

  for hdr := 0 to n - 1 do
    if HeaderEnd[hdr] >= 0 then
    begin
      // ⛔ Skip a loop the AOT has already compiled. In the combined profile the AOT owns whole
      // procedures, and the loop JIT was recompiling the loops INSIDE them: the native AOT code runs
      // regardless, so that work bought nothing and its compilation was pure cost -- fannkuch +21%
      // and binary-trees +8,6% against --aot alone. The JIT keeps exactly what it is for: hot loops
      // in code the AOT did NOT take. JIT_OVERAOT=1 restores the overlap for A/B.
      if (Length(FAotCovered) > hdr) and FAotCovered[hdr] and not GJitOverAot then
      begin
        if GetEnvironmentVariable('JIT_DIAG') <> '' then
          WriteLn(ErrOutput, Format('[JIT] loop PC %d..%d: SKIP (already compiled by the AOT)',
                                    [hdr, HeaderEnd[hdr]]));
        // ⛔ System.Continue: inside a VM method a bare Continue binds to TBytecodeVM.Continue -- the
        // BASIC CONT command -- and the program dies with "?CAN'T CONTINUE ERROR" instead of looping.
        System.Continue;
      end;
      // Array/sqrt/div may only be compiled when their MODERN edge semantics match the native SSE forms
      // (no CLASSIC raise, no forced bounds-check).
      // Per-context state (Xfer banks, record heap) is reached through the EXECUTING context, passed
      // as the native function's 4th argument: only the class FIELD OFFSETS are baked, never an
      // address - so the same native loop is correct for the main context AND for THREADCREATE
      // workers (this is what allowed the "main context only" JIT gate to go). Shared-record handles
      // deopt, so no lock is baked in.
      Mem := CompileLoop(Ins, hdr, HeaderEnd[hdr], n, FTrueValue,
                         Assigned(FProgram) and FProgram.ModernMode and (not FBoundsCheck),
                         Assigned(FProgram) and FProgram.ModernMode,
                         Integer(PtrUInt(@FCtx.XferInt) - PtrUInt(Pointer(FCtx))),
                         Integer(PtrUInt(@FCtx.XferFloat) - PtrUInt(Pointer(FCtx))),
                         Integer(PtrUInt(@FCtx.Records) - PtrUInt(Pointer(FCtx))),
                         RecSz, RIntOff, RFloatOff);
      if Mem <> nil then FNativeLoops[hdr] := Mem;
      if GetEnvironmentVariable('JIT_DIAG') <> '' then
      begin
        if Mem <> nil then
          WriteLn(ErrOutput, Format('[JIT] loop PC %d..%d (%d instr, src line %d): NATIVE',
            [hdr, HeaderEnd[hdr], HeaderEnd[hdr] - hdr + 1, FProgram.GetSourceLine(hdr)]))
        else
          WriteLn(ErrOutput, Format('[JIT] loop PC %d..%d (%d instr, src line %d): BAIL at PC %d op %d (src line %d)',
            [hdr, HeaderEnd[hdr], HeaderEnd[hdr] - hdr + 1, FProgram.GetSourceLine(hdr),
             JitDiagCurPC, JitDiagCurOp, FProgram.GetSourceLine(JitDiagCurPC)]));
      end;
    end;
  // Nothing compiled - no loop, or every candidate bailed - so drop the table entirely. The run
  // loop arms its per-instruction native check on `Length(FNativeLoops) > 0`, and that check is a
  // load from a PC-indexed array on EVERY interpreted instruction, sharing cache with the dense
  // opcode table read beside it. Keeping an all-nil table around therefore taxes the interpreter
  // for a JIT that has nothing to offer: measured at ~4% on a program with no loop at all
  // (recursive fib, where a `--jit` run was slower than the same program without the flag).
  hdr := -1;
  for i := 0 to n - 1 do
    if FNativeLoops[i] <> nil then begin hdr := i; Break; end;
  if hdr < 0 then SetLength(FNativeLoops, 0);
  FArraysDirty := True;   // force a descriptor rebuild before the first compiled loop runs
end;

{ ---------------- AOT runtime helper (C3, PIANO_B1_AOT_DESIGN §5.6/§5.7) ----------------
  The lowering that gives the AOT full program coverage: an SSA op with no native form
  becomes a call to this, which runs THAT ONE bytecode instruction on the interpreter's
  existing slow path and hands back the PC to continue at.

  Two things make it safe to call from code we generated ourselves:

  * It catches everything. An FPC exception unwinding through an AOT frame would be
    undefined behaviour - those frames have no unwind info - so the exception stops here,
    is parked on the context (per-worker, never on the VM), and comes back as a sentinel
    the AOT call site in RunTemplate turns into a real `raise` inside the interpreter's
    try..except. ON ERROR / TRAP / Err / RESUME keep working, unchanged and unaware.
  * It reports control flow by PC. Ctx.PC is the interpreter's own channel for "the flow
    moved" (jump, call, return, RESUME), so reading it back after the handler covers every
    case without enumerating opcodes: native code continues only when the next PC is the
    one it statically expected, and otherwise leaves to the interpreter there.

  Emitted code reaches it through TAotCtx.ExecOne - never a baked address, so a worker
  thread running the same compiled function passes its own VM/context pair. }
function AotExecOne(VMSelf, CtxObj: Pointer; PC: PtrInt; AotCtx: PAotCtx): PtrInt; cdecl;
type
  PInstr = ^TBytecodeInstruction;
var
  VM: TBytecodeVM;
  C: TExecutionContext;
begin
  VM := TBytecodeVM(VMSelf);
  C := TExecutionContext(CtxObj);
  try
    C.PC := PC;
    {$IFDEF DEBUG_AOTTRACE}
    // build.ps1 -Target sb -DebugFlags AOTTRACE, then AOT_TRACE=1 at runtime. Compiled out
    // otherwise: this unit's code size is not free, it moves the dispatch loop around
    // (see PIANO_B1_AOT_DESIGN §5.7).
    if GetEnvironmentVariable('AOT_TRACE') <> '' then
      WriteLn(ErrOutput, '[AOT] helper PC=', PC, ' op=$',
              IntToHex(PInstr(VM.FProgram.GetInstructionsPtr)[PC].OpCode, 4));
    {$ENDIF}
    VM.ExecuteInstruction(C, PInstr(VM.FProgram.GetInstructionsPtr)[PC]);
    // Refresh what the instruction may have invalidated underneath the caller. DIM/REDIM/ERASE
    // rebuild the array descriptor table and can move it, and native code caches element base
    // pointers read from it - so the table is rebuilt here, while we are on an interpreter
    // frame, and the pointer is handed back through the context the caller re-reads after the
    // call. (The register banks need no such care: they are sized once by LoadProgram and
    // never reallocated during a run, so rbx/rsi stay valid.)
    if AotCtx <> nil then
    begin
      if VM.FArraysDirty then VM.RebuildJitArrDesc;
      if Length(VM.FJitArrDesc) > 0 then AotCtx^.ArrDesc := @VM.FJitArrDesc[0]
      else AotCtx^.ArrDesc := nil;
    end;
    // A handler that clears Running (CTRL+C, quit, a failed ASSERT) must stop the run loop,
    // not just this instruction - native code cannot do that, so bounce out to the interpreter.
    if not C.Running then
    begin
      C.AotFaultPC := C.PC + 1;
      Exit(AOT_HELPER_HALT);
    end;
    Result := C.PC + 1;
  except
    C.AotPendingExc := TObject(AcquireExceptionObject);
    C.AotFaultPC := PC;
    Result := AOT_HELPER_EXC;
  end;
end;

{ ===== C6: the RECORD family as native leaf calls =====

  These four are the record counterpart of the C5 string primitives, and they exist for the same
  measured reason. Without a native lowering, New/Delete/RecMark went through AotExecOne, and
  EmitHelperCall wraps EVERY such instruction in a full flush of the allocated registers plus a
  full reload afterwards. binary-trees pays that five times per node (two mark pushes, the New,
  the Delete, a mark pop), which is why `--aot` measured 358 ns per New+Delete pair against the
  interpreter's 194: the AOT made calls 3x faster and gave it all back on the allocator.

  Each is an EXACT transcription of its RunTemplate arm - the interpreter stays the definition of
  the behaviour, this is only a cheaper way to reach it. None of them can hand the invocation back
  to the interpreter, so the call sites carry no deopt hazard (unlike a helper call).

  ⚠️ The counts travel PACKED because Win64 has four argument registers and New needs five values:
  Src1/Src2 of the bytecode instruction are compile-time slot counts, not registers (see the
  ssaRecordNew override in SedaiBytecodeCompiler), so the emitter bakes them as one immediate. }

function AotRecordNew(VMSelf, CtxObj: Pointer; Counts, Imm: PtrInt): PtrInt; cdecl;
// Counts = IntSlots or (FloatSlots shl 32); Imm is the bytecode Immediate verbatim:
// string slots in bits 0..15, type id in bits 32..47, "allocate in the shared region" in bit 48.
var
  IntC, FloatC, StrC, TypeId: Integer;
begin
  IntC   := Integer(Counts and $FFFFFFFF);
  FloatC := Integer((Counts shr 32) and $FFFFFFFF);
  StrC   := Integer(Imm and $FFFF);
  TypeId := Integer((Imm shr 32) and $FFFF);
  if (Imm shr 48) and 1 <> 0 then
    Result := PtrInt(TBytecodeVM(VMSelf).AllocSharedRecord(IntC, FloatC, StrC, TypeId))
  else
    Result := PtrInt(TBytecodeVM(VMSelf).AllocRecord(TExecutionContext(CtxObj), IntC, FloatC, StrC, TypeId));
end;

procedure AotRecordFree(VMSelf: Pointer; Handle: PtrInt); cdecl;
begin
  TBytecodeVM(VMSelf).FreeSharedRecord(Handle);
end;

procedure AotRecMarkPush(CtxObj: Pointer); cdecl;
var
  C: TExecutionContext;
begin
  C := TExecutionContext(CtxObj);
  if C.BlockRecMarkTop >= Length(C.BlockRecMark) then
    SetLength(C.BlockRecMark, C.BlockRecMarkTop + 256);
  C.BlockRecMark[C.BlockRecMarkTop] := C.RecordCount;
  Inc(C.BlockRecMarkTop);
end;

procedure AotRecMarkPop(CtxObj: Pointer); cdecl;
var
  C: TExecutionContext;
begin
  C := TExecutionContext(CtxObj);
  if C.BlockRecMarkTop > 0 then
  begin
    Dec(C.BlockRecMarkTop);
    if C.BlockRecMark[C.BlockRecMarkTop] < C.RecordCount then
      C.RecordCount := C.BlockRecMark[C.BlockRecMarkTop];
  end;
end;

const
  // B3: cap on native-to-native call nesting. Each level consumes real machine stack (the
  // callee's native frame plus this Pascal frame); the interpreter's call stack lives on the
  // heap and auto-grows, so past the cap AotCallSub DECLINES the call - the caller falls back
  // to the interpreted bcCallSub, which unwinds the whole native chain to the run loop and
  // re-enters the callee natively from there at depth ~0. Deep recursion therefore costs one
  // full unwind per cap-many levels and stays correct (m449 exercises this).
  AOT_CALLSUB_MAX_DEPTH = 1500;

{ AotCallSub (B3, road A): the native call site for a STATIC bcCallSub whose callee is itself
  compiled. Replicates the interpreter's bcCallSub exactly - FramePush (bank snapshot narrowed
  to the callee's clobber width) + return-PC push - then invokes the callee's compiled function
  on the same banks. When the callee comes back AT its bcReturnSub (which its native code does
  not execute - returns are exit points), the return half is performed here (pop + FramePop,
  the interpreter's order) and AOT_CALL_OK tells the caller to continue natively. Everything
  else hands the rest of the invocation to the interpreter: callee not compiled / depth cap ->
  decline BEFORE any state change (return BcCallSubPC, the interpreter re-runs the call); a
  deopt PC from inside the callee -> pass it through with frame and return address still
  pushed (the callee's eventual interpreted bcReturnSub pops them); the two negative helper
  sentinels -> pass through untouched. The caller's native code exits through its BARE
  epilogue on any non-OK value: its registers were flushed before the call and the callee has
  since written the banks. Reached through TAotCtx.CallSub - never a baked address. }
{ ---- AOT_CALLPROF: where the cycles of a native call ACTUALLY go ----------------------------
  Attribution, not estimation. fib(36) measures ~300 cycles per call while the parts visible in the
  source add up to a fraction of that, so the missing cost has to be located before anything is
  rewritten. Off unless AOT_CALLPROF=1; the only cost when off is one integer test per call.
  Phases, measured with RDTSC and reported net of the instrument's own overhead (calibrated at
  startup by reading the counter twice back to back):
    push   entry -> after FramePush + call-stack push        (bank snapshot + bookkeeping)
    callee the compiled function itself                      (its prologue, body and epilogue)
    pop    after the callee -> exit                          (descriptor refresh + FramePop)
  Plus the EXACT number of bank elements FramePush copied, taken from the save-stack tops rather
  than recomputed, so it cannot disagree with what actually happened. }
{$push}{$asmmode intel}
function AotRdTsc: QWord; assembler; nostackframe;
asm
  rdtsc
  shl rdx, 32
  or  rax, rdx
end;
{$pop}

var
  GCallProf: Integer = -1;         // -1 unread, 0 off, 1 on, 2 on + sub-phases
  GCPCalls: QWord = 0;
  GCPTotal: QWord = 0;
  GCPPush: QWord = 0;
  GCPCallee: QWord = 0;
  GCPPop: QWord = 0;
  GCPBankI: QWord = 0;
  GCPBankF: QWord = 0;
  GCPBankS: QWord = 0;
  GCPTscOverhead: QWord = 0;       // cycles charged by one RdTsc read, measured
  // AOT_CALLPROF=2 only: the push and pop phases split into their parts. Six extra RdTsc reads
  // per call, so with these on the phase totals above are NOT comparable to a =1 run - read the
  // shares, not the absolutes. Off by default for exactly that reason.
  GCPPre: QWord = 0;               // entry -> before FramePush   (dispatch checks, callee lookup)
  GCPFrameP: QWord = 0;            // FramePush alone
  GCPStack: QWord = 0;             // GrowCallStackIfNeeded + return-PC push
  GCPArrIn: QWord = 0;             // descriptor refresh BEFORE the callee
  GCPArrOut: QWord = 0;            // descriptor refresh AFTER the callee
  GCPFrameQ: QWord = 0;            // return-PC pop + FramePop + the bcReturnSub test

function AotCallProfOn: Boolean; inline;
var i: Integer; a, b, lo: QWord;
begin
  if GCallProf < 0 then
  begin
    if GetEnvironmentVariable('AOT_CALLPROF') = '1' then GCallProf := 1
    else if GetEnvironmentVariable('AOT_CALLPROF') = '2' then GCallProf := 2
    else GCallProf := 0;
    if GCallProf > 0 then
    begin
      lo := High(QWord);
      for i := 1 to 1000 do
      begin
        a := AotRdTsc; b := AotRdTsc;
        if (b - a) < lo then lo := b - a;
      end;
      GCPTscOverhead := lo;
    end;
  end;
  Result := GCallProf > 0;
end;

function AotCallSub(AotCtx: PAotCtx; CalleeEntryPC, BcCallSubPC: PtrInt): PtrInt; cdecl; forward;

{ AotCallSubFast: the same call site as AotCallSub, for the case that is worth specialising - a
  callee whose frame is a POINTER SLIDE. Measured on fib, a native call costs ~205 cycles, of which
  FramePush and FramePop were 65.7 while copying nothing at all; the fast frame took that to ~24, and
  what is left of the Pascal side is mostly the calls themselves. So here the frame push and pop are
  INLINED rather than called, and everything that exists for the general case is gone: no profiling
  branches, no save-stack bookkeeping (nothing is saved), no width unpacking (there are no widths).

  Eligibility is decided by the CALLER at compile time - FFrameFast[calleePC] >= 0 - so this routine
  may assume it. The two things it still has to check are the two that are only knowable at run time:
  the callee may not be compiled yet, and the relocation region may be full. Either way it falls back
  to the general primitive, which is always correct.

  DUPLICATED SEMANTICS, deliberately and narrowly: this is the fast half of FramePush/FramePop
  written out. If that fast path changes, this must change with it - the guard is that both are
  driven by the same FFrameFast table and the same sentinel (WInt = -1). }
function AotCallSubFast(AotCtx: PAotCtx; CalleeEntryPC, BcCallSubPC: PtrInt): PtrInt; cdecl;
type
  PInstr = ^TBytecodeInstruction;
var
  VM: TBytecodeVM;
  C: TExecutionContext;
  Fn: TExecMem;
  RetPC: PtrInt;
  PW: Int64;
  FBHi, FBLo, SaveDelta, SaveHw: Integer;
begin
  VM := TBytecodeVM(AotCtx^.VMSelf);
  C := TExecutionContext(AotCtx^.CtxObj);
  if (CalleeEntryPC < 0) or (CalleeEntryPC >= Length(VM.FNativeFuncs)) then
    Exit(BcCallSubPC);
  Fn := VM.FNativeFuncs[CalleeEntryPC];
  if (Fn = nil) or (C.AotCallDepth >= AOT_CALLSUB_MAX_DEPTH) then
    Exit(BcCallSubPC);
  // The frame, inlined. One table read gives both numbers; the region check is also what bounds
  // recursion here, exactly as in FramePush.
  PW := VM.FFrameFast[CalleeEntryPC];
  if PW < 0 then Exit(AotCallSub(AotCtx, CalleeEntryPC, BcCallSubPC));   // not fast after all
  FBHi := PW shr 32; FBLo := PW and $FFFFFFFF;
  if C.RegHwI + FBHi > C.RegFrameCap then
    Exit(AotCallSub(AotCtx, CalleeEntryPC, BcCallSubPC));                // region full: copy instead
  try
    if C.FrameMarkTop >= Length(C.FrameMarks) then
      SetLength(C.FrameMarks, C.FrameMarkTop + 256);
    VM.GrowCallStackIfNeeded(C);
  except
    C.AotPendingExc := TObject(AcquireExceptionObject);
    C.AotFaultPC := BcCallSubPC;
    Exit(AOT_HELPER_EXC);
  end;
  SaveDelta := C.RegDeltaI;
  SaveHw := C.RegHwI;
  with C.FrameMarks[C.FrameMarkTop] do
  begin
    SaveDeltaI := SaveDelta;
    SaveHwI := SaveHw;
    WInt := -1;                       // the sentinel FramePop's fast path answers to
    RecBase := C.RecordCount;
    BlockMark := C.BlockRecMarkTop;
  end;
  Inc(C.FrameMarkTop);
  C.RegDeltaI := SaveHw - FBLo;
  C.RegHwI := SaveHw + FBHi;
  C.IntRegs := @C.IntRegsMem[C.RegDeltaI];
  C.CallStack[C.CallStackPtr] := Integer(BcCallSubPC) + 1;
  Inc(C.CallStackPtr);
  if VM.FArraysDirty then VM.RebuildJitArrDesc;
  if Length(VM.FJitArrDesc) > 0 then AotCtx^.ArrDesc := @VM.FJitArrDesc[0]
  else AotCtx^.ArrDesc := nil;
  Inc(C.AotCallDepth);
  RetPC := TNativeFuncFn(Fn.Ptr)(C.IntRegs, PInt64(@C.FloatRegs[0]), AotCtx);
  Dec(C.AotCallDepth);
  if VM.FArraysDirty then VM.RebuildJitArrDesc;
  if Length(VM.FJitArrDesc) > 0 then AotCtx^.ArrDesc := @VM.FJitArrDesc[0]
  else AotCtx^.ArrDesc := nil;
  if RetPC < 0 then Exit(RetPC);      // helper sentinel: the frame stays pushed, as in the general one
  if (RetPC < VM.FProgram.GetInstructionCount) and
     (PInstr(VM.FProgram.GetInstructionsPtr)[RetPC].OpCode = bcReturnSub) then
  begin
    Dec(C.CallStackPtr);              // pop, then the frame - the interpreter's bcReturnSub order
    Dec(C.FrameMarkTop);
    with C.FrameMarks[C.FrameMarkTop] do
    begin
      C.RegDeltaI := SaveDeltaI;
      C.RegHwI := SaveHwI;
      C.IntRegs := @C.IntRegsMem[SaveDeltaI];
      if RecBase < C.RecordCount then C.RecordCount := RecBase;
      C.BlockRecMarkTop := BlockMark;
    end;
    Exit(AOT_CALL_OK);
  end;
  Result := RetPC;                    // deopt inside the callee: frame + return address stay
end;

function AotCallSub(AotCtx: PAotCtx; CalleeEntryPC, BcCallSubPC: PtrInt): PtrInt; cdecl;
type
  PInstr = ^TBytecodeInstruction;
var
  VM: TBytecodeVM;
  C: TExecutionContext;
  Fn: TExecMem;
  RetPC: PtrInt;
  Prof, Fine: Boolean;
  T0, T1, T2, T3: QWord;
  Ta, Tb, Tc, Td: QWord;
  SI, SF, SS: Integer;
begin
  Prof := AotCallProfOn;
  Fine := GCallProf = 2;
  T0 := 0; T1 := 0; T2 := 0; Ta := 0; Tb := 0; Tc := 0; Td := 0;
  if Prof then T0 := AotRdTsc;
  VM := TBytecodeVM(AotCtx^.VMSelf);
  C := TExecutionContext(AotCtx^.CtxObj);
  if (CalleeEntryPC < 0) or (CalleeEntryPC >= Length(VM.FNativeFuncs)) then
    Exit(BcCallSubPC);
  Fn := VM.FNativeFuncs[CalleeEntryPC];
  if (Fn = nil) or (C.AotCallDepth >= AOT_CALLSUB_MAX_DEPTH) then
    Exit(BcCallSubPC);
  SI := C.FrameSaveIntTop; SF := C.FrameSaveFloatTop; SS := C.FrameSaveStrTop;
  if Fine then Ta := AotRdTsc;
  try
    VM.FramePush(C, Integer(CalleeEntryPC), Integer(BcCallSubPC));
    if Fine then Tb := AotRdTsc;
    VM.GrowCallStackIfNeeded(C);
    C.CallStack[C.CallStackPtr] := Integer(BcCallSubPC) + 1;
    Inc(C.CallStackPtr);
    if Fine then Tc := AotRdTsc;
  except
    // FramePush/Grow can allocate; a raise here must not unwind through native frames.
    C.AotPendingExc := TObject(AcquireExceptionObject);
    C.AotFaultPC := BcCallSubPC;
    Exit(AOT_HELPER_EXC);
  end;
  // Same refresh the run loop performs before invoking a native function.
  if VM.FArraysDirty then VM.RebuildJitArrDesc;
  if Length(VM.FJitArrDesc) > 0 then AotCtx^.ArrDesc := @VM.FJitArrDesc[0]
  else AotCtx^.ArrDesc := nil;
  if Prof then
  begin
    T1 := AotRdTsc;
    Inc(GCPBankI, QWord(C.FrameSaveIntTop - SI));
    Inc(GCPBankF, QWord(C.FrameSaveFloatTop - SF));
    Inc(GCPBankS, QWord(C.FrameSaveStrTop - SS));
    if Fine then
    begin
      Inc(GCPPre,    Ta - T0 - GCPTscOverhead);
      Inc(GCPFrameP, Tb - Ta - GCPTscOverhead);
      Inc(GCPStack,  Tc - Tb - GCPTscOverhead);
      Inc(GCPArrIn,  T1 - Tc - GCPTscOverhead);
    end;
  end;
  Inc(C.AotCallDepth);
  // C.IntRegs is the context's VIEW: if FramePush relocated this frame it already points at the
  // callee's fresh slots, so the compiled code (which takes the bank base as its first argument
  // and addresses everything off it) needs no change at all to run relocated.
  RetPC := TNativeFuncFn(Fn.Ptr)(C.IntRegs, PInt64(@C.FloatRegs[0]), AotCtx);
  Dec(C.AotCallDepth);
  if Prof then T2 := AotRdTsc;
  // A DIM/REDIM/ERASE inside the callee may have rebuilt/moved the descriptor table the
  // caller's cached bases came from; refresh while still on a Pascal frame.
  if VM.FArraysDirty then VM.RebuildJitArrDesc;
  if Length(VM.FJitArrDesc) > 0 then AotCtx^.ArrDesc := @VM.FJitArrDesc[0]
  else AotCtx^.ArrDesc := nil;
  if Fine then Td := AotRdTsc;
  if RetPC < 0 then Exit(RetPC);   // helper sentinel from inside the callee: frame stays pushed
  if (RetPC < VM.FProgram.GetInstructionCount) and
     (PInstr(VM.FProgram.GetInstructionsPtr)[RetPC].OpCode = bcReturnSub) then
  begin
    Dec(C.CallStackPtr);           // the interpreter's bcReturnSub order: pop, then FramePop
    VM.FramePop(C);
    if Prof then
    begin
      T3 := AotRdTsc;
      Inc(GCPCalls);
      Inc(GCPPush,   T1 - T0 - GCPTscOverhead);
      Inc(GCPCallee, T2 - T1 - GCPTscOverhead);
      Inc(GCPPop,    T3 - T2 - GCPTscOverhead);
      Inc(GCPTotal,  T3 - T0 - 3 * GCPTscOverhead);
      if Fine then
      begin
        Inc(GCPArrOut, Td - T2 - GCPTscOverhead);
        Inc(GCPFrameQ, T3 - Td - GCPTscOverhead);
      end;
    end;
    Exit(AOT_CALL_OK);
  end;
  Result := RetPC;                 // deopt inside the callee: frame + return address stay
end;

{ AotStrCmp (C5): the leaf primitive the AOT calls for bcCmp*String instead of routing the whole
  instruction through AotExecOne. a and b are the two StringRegs slot VALUES (AnsiString data
  pointers or nil), read natively from the bank by the emitted code; Kind selects the relation
  (0=Eq 1=Ne 2=Lt 3=Gt - Le/Ge do not exist here, the parser rewrites them to Gt/Lt with the
  operands swapped). Returns 1/0, which the native code turns into TrueVal/0.

  The params are Pointer, not string: a hard AnsiString typecast reinterprets the pointer without
  creating a managed temporary, so nothing is incref'd or decref'd - the comparison only reads the
  bytes, exactly as the interpreter's `StringRegs[i] = StringRegs[j]` does. cdecl to match the
  emitted call site; this is a global function (same address for every worker), reached through
  TAotCtx.StrCmp like the other primitives so no address is ever baked into the code. }
function AotStrCmp(a, b: Pointer; Kind: PtrInt): PtrInt; cdecl;
var r: Boolean;
begin
  case Kind of
    0: r := AnsiString(a) =  AnsiString(b);
    1: r := AnsiString(a) <> AnsiString(b);
    2: r := AnsiString(a) <  AnsiString(b);
    3: r := AnsiString(a) >  AnsiString(b);
  else
    r := False;
  end;
  if r then Result := 1 else Result := 0;
end;

{ C5 string leaf primitives. Each does ONE bank operation the AOT would otherwise route through
  AotExecOne, in Pascal, so refcount/allocation stay correct and codepage-agnostic. dstSlot is
  the ADDRESS of a StringRegs element (&StringRegs[dest]); srcVal/aVal/bVal are element VALUES
  (the AnsiString data pointer or nil), read natively from the bank by the emitted code. The
  managed assignment `PAnsiString(dstSlot)^ := ...` is exactly what the interpreter's
  `StringRegs[Dest] := ...` compiles to (incref new, decref old), so behaviour is bit-identical,
  including dst aliasing a source. cdecl to match the emitted call sites; reached through TAotCtx
  so no address is baked. Global functions in this unit see TBytecodeVM's private fields, like
  AotExecOne does. }
procedure AotStrAssign(dstSlot, srcVal: Pointer); cdecl;
begin
  PAnsiString(dstSlot)^ := AnsiString(srcVal);
end;

procedure AotArrLoadStr(dstSlot, VMSelf: Pointer; ArrIdx, Idx: PtrInt); cdecl;
// StringRegs[dest] := FArrays[ArrIdx].StringData[Idx], the EXACT expression of the interpreter's
// inline bcArrayLoadString arm - out of range yields '', as it does there. The two must agree; the
// differential interp-vs-aot net (aot_validate) is what guards the pair.
//
// A string element is MANAGED, so the assignment goes through the same refcounted path every native
// string op uses. Only ever reached where an out-of-range index cannot RAISE (GArrStrNative), because
// an exception thrown here would unwind through a compiled frame that is not registered for it.
var
  VM: TBytecodeVM;
begin
  VM := TBytecodeVM(VMSelf);
  if (Idx >= 0) and (Idx < VM.FArrays[ArrIdx].TotalSize) then
    PAnsiString(dstSlot)^ := VM.FArrays[ArrIdx].StringData[Idx]
  else
    PAnsiString(dstSlot)^ := '';
end;

procedure AotArrStoreStr(VMSelf: Pointer; ArrIdx: PtrInt; srcVal: Pointer; Idx: PtrInt); cdecl;
// FArrays[ArrIdx].StringData[Idx] := StringRegs[src], mirroring the interpreter's inline
// bcArrayStoreString arm: an out-of-range store is DROPPED there, so it is dropped here.
//
// The INDEX is the last parameter on purpose: it lets the emitter load it into the last ABI
// register before touching anything else, which is what keeps a pooled index from being clobbered
// (see EmitArrStoreStr - getting this order wrong lost one array element and nothing complained).
var
  VM: TBytecodeVM;
begin
  VM := TBytecodeVM(VMSelf);
  if (Idx >= 0) and (Idx < VM.FArrays[ArrIdx].TotalSize) then
    VM.FArrays[ArrIdx].StringData[Idx] := AnsiString(srcVal);
end;

procedure AotStrLoadConst(dstSlot, VMSelf: Pointer; imm: PtrInt); cdecl;
var VM: TBytecodeVM;
begin
  VM := TBytecodeVM(VMSelf);
  // Out of range leaves dst unchanged, exactly as bcLoadConstString does (never happens for a
  // valid program, but the two paths must agree).
  if (imm >= 0) and (imm < VM.FProgram.StringConstants.Count) then
    PAnsiString(dstSlot)^ := VM.FProgram.StringConstants[imm];
end;

procedure AppendString(var D: AnsiString; const S: AnsiString); forward;

{ ===== Appending without calling the allocator on every byte =====

  An in-place append still costs a heap call: SetLength on a unique AnsiString goes to ReallocMem,
  and "acc += one character" therefore asks the allocator to regrow the block once PER CHARACTER.
  Measured on the reverse-complement shape (1M characters, --aot): Asc(Mid(s,i,1)) 15 ns, Mid(tab,k,1)
  48 ns, and the append alone 93 ns -- sixty per cent of the loop, for moving one byte.

  What is missing is CAPACITY. An AnsiString carries a length but no notion of spare room, so nothing
  can tell that the block already has space. The heap block usually does: MemSize reports what was
  really handed out. So: when the block can already hold the new length, write the length field and
  the terminator directly and skip the allocator entirely; when it cannot, grow with geometric slack
  so the next appends find room.

  ⚠️ This reaches into FPC's AnsiString header, so it is verified at startup rather than assumed:
  StrCapacityInit builds a string, checks that the length field sits where TSbAnsiRec says and that
  MemSize covers it, and leaves the fast path OFF unless both hold. A future FPC that changes the
  layout loses the optimisation instead of corrupting memory. }
type
  // Mirrors the RTL's TAnsiRec. Only the SIZE is used, to step back from the data pointer.
  TSbAnsiRec = record
    CodePage: Word;
    ElementSize: Word;
    {$IFDEF CPU64}Dummy: DWord;{$ENDIF}
    Ref: SizeInt;
    Len: SizeInt;
  end;

const
  SB_ANSI_HDR = SizeOf(TSbAnsiRec);

var
  GStrCapacity: Boolean = False;   // set by StrCapacityInit once the layout is confirmed

function StrSpareRoom(const D: AnsiString): SizeInt; inline;
// How many bytes the block behind D can hold beyond its current length. Negative-safe: returns 0
// when anything looks unexpected, which sends the caller back to SetLength.
var
  Blk: Pointer;
  Total: PtrUInt;
begin
  Blk := Pointer(D) - SB_ANSI_HDR;
  Total := MemSize(Blk);
  if Total <= PtrUInt(SB_ANSI_HDR + 1) then Exit(0);
  Result := SizeInt(Total) - SB_ANSI_HDR - 1 - Length(D);   // -1 keeps room for the NUL terminator
  if Result < 0 then Result := 0;
end;

procedure StrSetLenInPlace(var D: AnsiString; NewLen: SizeInt); inline;
// Publish a new length for a block that already has the room. This is the part SetLength would do
// after deciding it need not reallocate.
begin
  PSizeInt(Pointer(D) - SizeOf(SizeInt))^ := NewLen;
  PByte(Pointer(D) + NewLen)^ := 0;
end;

procedure StrGrowWithSlack(var D: AnsiString; NewLen: SizeInt);
// Reallocate to comfortably more than NewLen, then publish NewLen. The slack is what makes the next
// appends free; without it every single one comes back here.
var
  Want: SizeInt;
begin
  Want := NewLen + (NewLen div 2) + 32;
  SetLength(D, Want);
  StrSetLenInPlace(D, NewLen);
end;

procedure StrCapacityInit;
// Confirm the header layout on THIS runtime before letting anything above run. Cheap, once.
var
  T: AnsiString;
begin
  GStrCapacity := False;
  GAotStrHdrOK := False;
  T := 'abcdefgh';
  UniqueString(T);
  if StringRefCount(T) <> 1 then Exit;
  if PSizeInt(Pointer(T) - SizeOf(SizeInt))^ <> 8 then Exit;          // the length field is where we think
  // The AOT's inline Asc(Mid()) reads that length field from emitted code and needs nothing else,
  // so it is cleared for take-off HERE - a separate flag, on purpose: STRCAP=0 is the A/B switch
  // for the capacity work below and must not silently disable the inline as a side effect.
  GAotStrHdrOK := True;
  if MemSize(Pointer(T) - SB_ANSI_HDR) < PtrUInt(SB_ANSI_HDR + 9) then Exit;  // and the block covers it
  GStrCapacity := True;
end;

procedure AotStrConcat(dstSlot, aVal, bVal: Pointer); cdecl;
// "s = s + x" arrives here with dest = src1 once the SSA fusion has removed the temporary copy, and
// then growing the destination in place is the difference between linear and quadratic -- exactly as
// in bcStrConcat. Without this arm the AOT kept rebuilding the whole accumulator on every append
// while the interpreter had already stopped: the same source ran the fast shape only interpreted.
//
// The test is on the BUFFER, not on a register number the primitive cannot see: dest holds the very
// string that was passed as the left operand. Sharing the buffer with the RIGHT operand ("s = s + s")
// must stay on the plain concatenation, which allocates and therefore keeps the two apart.
begin
  if (Pointer(PAnsiString(dstSlot)^) = aVal) and (aVal <> bVal) then
    AppendString(PAnsiString(dstSlot)^, AnsiString(bVal))
  else
    PAnsiString(dstSlot)^ := AnsiString(aVal) + AnsiString(bVal);
end;

function AotStrLen(sVal: Pointer): PtrInt; cdecl;
begin
  Result := Length(AnsiString(sVal));
end;

{ C5 residuals: substring/char/search leaf primitives, each the EXACT expression of the
  corresponding ExecuteStringOp handler (byte-string ops; the W codepoint family stays on
  the runtime helper). Copy() builds a NEW string before the managed assignment, so a dst
  that aliases the source is safe, as everywhere else in this family. StrMid is dialect-
  variant: the run loop installs the Modern or Classic flavor per program (TAotCtx.StrMid). }
procedure AssignSubstr(var D: AnsiString; const S: AnsiString; Start, Cnt: SizeInt);
// D := Copy(S, Start, Cnt), but REUSING D's buffer when nothing else shares it.
//
// The obvious "D := Copy(S, Start, Cnt)" allocates a fresh buffer on every call and frees the old
// one, and measurement says that the allocation - not the copying - is what a string primitive costs
// here: MID$ of ONE character and MID$ of 128 both cost the same 131 ns, and a raw GetMem/FreeMem
// pair alone is 48 ns. SetLength on an unshared string reuses the block instead: 30 ns against
// Copy's 66. Every per-character loop in the string benchmarks pays this on every single character.
//
// The reuse is only taken when D's buffer is UNSHARED and is not S's own buffer. Letting SetLength
// handle a shared buffer instead would be correct but slower than what it replaces: SetLength copies
// the old contents into the new block before we overwrite them, so a shared destination paid for a
// copy of bytes nobody wanted. Reading the refcount first and falling back to Copy avoids that.
begin
  if Cnt <= 0 then
  begin
    D := '';
    Exit;
  end;
  if Start < 1 then Start := 1;
  if Start + Cnt - 1 > Length(S) then
  begin
    Cnt := Length(S) - Start + 1;
    if Cnt <= 0 then
    begin
      D := '';
      Exit;
    end;
  end;
  if (Pointer(D) <> nil) and (Pointer(D) <> Pointer(S)) and (StringRefCount(D) = 1) then
  begin
    SetLength(D, Cnt);
    Move(S[Start], D[1], Cnt);
  end
  else
    D := Copy(S, Start, Cnt);
end;

procedure AppendChar(var D: AnsiString; C: AnsiChar);
// D := D + C, growing D in place when nothing else shares its buffer: the single-character case of
// AppendString, and what "outLine += Mid(tab, k, 1)" lowers to once the fusion has removed the
// one-character temporary. The unshared test is what makes it correct, not merely fast -- growing a
// buffer somebody else holds would rewrite their string too.
var
  OldLen: SizeInt;
begin
  OldLen := Length(D);
  if (OldLen > 0) and (StringRefCount(D) = 1) then
  begin
    if GStrCapacity then
    begin
      // The block usually has room already, and then the whole append is: write the byte, publish
      // the length. No allocator call at all -- which is the point, see StrSpareRoom.
      if StrSpareRoom(D) >= 1 then
        StrSetLenInPlace(D, OldLen + 1)
      else
        StrGrowWithSlack(D, OldLen + 1);
    end
    else
      SetLength(D, OldLen + 1);
    D[OldLen + 1] := C;
  end
  else
    D := D + C;
end;

procedure AppendString(var D: AnsiString; const S: AnsiString);
// D := D + S, GROWING D in place when nothing else shares its buffer.
//
// This is the difference between linear and quadratic. "D := D + S" builds a whole new string of
// Length(D) + Length(S) and copies BOTH parts into it, every time; over n appends that is O(n^2)
// bytes moved. SetLength on an unshared buffer hands the work to the allocator's realloc, which
// usually extends the block in place and moves nothing, so only the new bytes are copied: O(n).
//
// FPC does exactly this for a plain "s := s + x" on a variable, but it cannot see the shape through
// StringRegs[i] where i is a run-time index -- so we spell it out. Reachable only because the
// peephole now writes the concatenation straight into the accumulator's register.
//
// The unshared test is what makes it CORRECT, not just fast: growing a buffer somebody else is
// holding would rewrite their string too. When it is shared we fall back to the plain concatenation,
// which allocates and therefore separates them.
var
  OldLen, AddLen: SizeInt;
begin
  AddLen := Length(S);
  if AddLen = 0 then Exit;
  OldLen := Length(D);
  if OldLen = 0 then
  begin
    D := S;
    Exit;
  end;
  if (Pointer(D) <> Pointer(S)) and (StringRefCount(D) = 1) then
  begin
    if GStrCapacity then
    begin
      if StrSpareRoom(D) >= AddLen then
        StrSetLenInPlace(D, OldLen + AddLen)
      else
        StrGrowWithSlack(D, OldLen + AddLen);
    end
    else
      SetLength(D, OldLen + AddLen);
    Move(S[1], D[OldLen + 1], AddLen);
  end
  else
    D := D + S;
end;

procedure AssignChar(var D: AnsiString; Code: Byte);
// D := Chr(Code), reusing D's buffer. Same reasoning as AssignSubstr: CHR$ measured 182 ms per
// million calls, essentially all of it the allocation of a one-byte string. When the destination
// register already holds an unshared single character - which is exactly what a CHR$ in a loop
// does every time round - this writes one byte and touches the heap not at all.
begin
  if (Pointer(D) <> nil) and (Length(D) = 1) and (StringRefCount(D) = 1) then
    PByte(Pointer(D))^ := Code
  else
    D := Chr(Code);
end;

procedure AotStrLeft(dstSlot, sVal: Pointer; n: PtrInt); cdecl;
begin
  if n < 0 then n := 0;
  AssignSubstr(PAnsiString(dstSlot)^, AnsiString(sVal), 1, n);
end;

procedure AotStrRight(dstSlot, sVal: Pointer; n: PtrInt); cdecl;
var
  L: PtrInt;
begin
  L := Length(AnsiString(sVal));
  if n < 0 then n := 0;
  if n > L then n := L;
  AssignSubstr(PAnsiString(dstSlot)^, AnsiString(sVal), L - n + 1, n);
end;

procedure AotStrMidModern(dstSlot, sVal: Pointer; start, cnt: PtrInt); cdecl;
begin
  // FreeBASIC: start < 1 yields '' (no clamp); a negative length means "rest of string".
  if start < 1 then
  begin
    PAnsiString(dstSlot)^ := '';
    Exit;
  end;
  if cnt < 0 then
  begin
    cnt := Length(AnsiString(sVal)) - start + 1;
    if cnt < 0 then cnt := 0;
  end;
  AssignSubstr(PAnsiString(dstSlot)^, AnsiString(sVal), start, cnt);
end;

procedure AotStrMidClassic(dstSlot, sVal: Pointer; start, cnt: PtrInt); cdecl;
begin
  // Commodore v7 clamps both (see bcStrMid).
  if start < 1 then start := 1;
  if cnt < 0 then cnt := 0;
  AssignSubstr(PAnsiString(dstSlot)^, AnsiString(sVal), start, cnt);
end;

function AotStrAsc(sVal: Pointer): PtrInt; cdecl;
begin
  if Length(AnsiString(sVal)) > 0 then
    Result := Ord(AnsiString(sVal)[1])
  else
    Result := 0;
end;

{ ASC(MID$(s, start, len)) fused, the compiled counterpart of the bcStrAscMid arm. Dialect-variant
  for the same reason StrMid is: MODERN yields '' for a start below 1 and reads "rest of string" from
  a negative length, CLASSIC clamps both. The run loop installs the right one per program, so the
  emitted code stays dialect-blind.

  ⚠️ Without these, the fused opcode fell to the GENERIC per-instruction helper - which flushes and
  reloads the whole register pool - and since it sits in a per-character loop that was SLOWER than
  leaving the region interpreted: reverse-complement's --aot went from faster than the interpreter to
  1.2x slower. A new opcode is not finished until the native backends know it. }
function AotStrAscMidModern(sVal: Pointer; start, ignored, cnt: PtrInt): PtrInt; cdecl;
// ⚠️ 'ignored' exists so that START and LEN land in the 3rd and 4th ABI registers (r8, r9 on
// Win64) instead of the 2nd (rdx). rdx CAN hold a pooled value, so writing it before reading the
// other operand clobbered one - and that is exactly how the first version returned 0 for "s[i]".
// r8 is the context register and therefore never pooled, so it is safe as a destination; r9 is
// written last. Same argument that makes EmitStrMid safe. Do not 'clean up' this parameter.
var
  S: AnsiString;
begin
  Result := 0;
  if start < 1 then Exit;                       // FB: below 1 is an empty substring, not char 1
  S := AnsiString(sVal);
  if cnt < 0 then cnt := Length(S) - start + 1; // negative length = the rest of the string
  if (cnt <= 0) or (start > Length(S)) then Exit;
  Result := Ord(S[start]);
end;

function AotStrAscMidClassic(sVal: Pointer; start, ignored, cnt: PtrInt): PtrInt; cdecl;
var
  S: AnsiString;
begin
  Result := 0;
  if start < 1 then start := 1;                 // CLASSIC clamps the start
  if cnt < 0 then cnt := 0;                     // ...and rejects a negative length
  S := AnsiString(sVal);
  if (cnt <= 0) or (start > Length(S)) then Exit;
  Result := Ord(S[start]);
end;

{ "acc + MID$(tab, k, 1)" fused, the compiled counterpart of the bcStrConcatCharAt arm. Dialect-
  variant for the same reason as StrAscMid, and for the only rule that can differ at length 1: what
  a start below 1 means. The length is 1 by construction, so no negative-length rule applies.

  When the destination slot already holds the accumulator - which is the shape the fusion exists for,
  "outLine += Mid(...)" - the buffer grows in place and the append costs no allocation at all. }
procedure ConcatCharTo(var D: AnsiString; const S: AnsiString; C: AnsiChar);
// D := S + C without ever building a one-character string. "S + C" in FPC converts C into a
// temporary AnsiString first, so it ALLOCATES -- and that allocation is exactly what this opcode
// exists to avoid. Measured with the concatenating form: reverse-complement 9% slower under --aot,
// 24% interpreted, i.e. worse than the two instructions the fusion replaces.
var
  L: SizeInt;
begin
  L := Length(S);
  // ⛔ Drop the old buffer FIRST. SetLength on a string whose buffer is shared (refcount > 1, which
  // is the normal state of a register that a CopyString has aliased) allocates AND copies the old
  // contents -- contents we are about to overwrite completely. Releasing it first makes the new
  // buffer unshared, so SetLength allocates without copying and only the Move below runs.
  D := '';
  SetLength(D, L + 1);
  if L > 0 then Move(S[1], D[1], L);
  D[L + 1] := C;
end;

procedure AotStrConcatCharAtModern(dstSlot, accVal, tabVal: Pointer; k: PtrInt); cdecl;
// ⛔ Never assign the operands to local AnsiString variables: that is a managed assignment and costs
// a reference count up and down per call, in a loop that runs once per character. Casting inside the
// expression, as AotStrLen does, reads them without touching the count. Measured: the local-variable
// version was SLOWER than the two instructions this fusion replaces.
begin
  if (k < 1) or (k > Length(AnsiString(tabVal))) then   // FB: below 1 is an empty substring
  begin
    if Pointer(PAnsiString(dstSlot)^) <> accVal then PAnsiString(dstSlot)^ := AnsiString(accVal);
    Exit;
  end;
  if Pointer(PAnsiString(dstSlot)^) = accVal then
    AppendChar(PAnsiString(dstSlot)^, AnsiString(tabVal)[k])
  else
    ConcatCharTo(PAnsiString(dstSlot)^, AnsiString(accVal), AnsiString(tabVal)[k]);
end;

procedure AotStrConcatCharAtClassic(dstSlot, accVal, tabVal: Pointer; k: PtrInt); cdecl;
begin
  if k < 1 then k := 1;                          // CLASSIC clamps the start
  if k > Length(AnsiString(tabVal)) then
  begin
    if Pointer(PAnsiString(dstSlot)^) <> accVal then PAnsiString(dstSlot)^ := AnsiString(accVal);
    Exit;
  end;
  if Pointer(PAnsiString(dstSlot)^) = accVal then
    AppendChar(PAnsiString(dstSlot)^, AnsiString(tabVal)[k])
  else
    ConcatCharTo(PAnsiString(dstSlot)^, AnsiString(accVal), AnsiString(tabVal)[k]);
end;

{ "acc += tab[Asc(MID$(s, i, 1)) + 1]" fused whole, the compiled counterpart of the
  bcStrAppendMapped arm. Dialect-variant for the same reason as StrAscMid: what a start below 1
  means. The accumulator always grows IN PLACE - that is the shape the opcode exists for.

  ⚠️ An out-of-range index yields code 0, hence table index 1 - NOT "append nothing". See the
  interpreter arm: Asc of an empty substring is 0, the +1 makes it 1, and the concatenation then
  appends the table's first byte. Exiting early instead diverges from the sequence being replaced.

  ⛔ NEGATIVE RESULT (1 Aug 2026, measured): do NOT inline this opcode into the AOT the way
  bcStrAscMid was. The helper CALL is not what it costs. Priced on job/tests/bench/apmap_floor.bas
  by cutting the body down in stages, 5 M characters, one binary:
      spill + call + both byte reads .......  47 ms   (the part an inline would remove)
      + StringRefCount ..................... +16 ms   3,2 ns/char
      + StrSpareRoom (MemSize) ............. +15 ms   3,0 ns/char
      + the write itself ................... +46 ms   9,2 ns/char
  So ~60% of the fused loop is inside AppendChar, and it is spread evenly over asking whether the
  buffer is shared, asking whether it has room, and writing - with no single culprit to remove.
  Inlining the opcode would attack the one part that is already the cheapest. }
procedure AotStrAppendMappedModern(dstSlot, srcVal, tabVal: Pointer; i: PtrInt); cdecl;
// ⛔ Operands read in place, never through local AnsiString variables: a managed assignment costs a
// reference count up and down on EVERY character. Same rule as AotStrConcatCharAt.
var
  code: PtrInt;
begin
  if (i < 1) or (i > Length(AnsiString(srcVal))) then
    code := 1                                           // FB: empty substring -> Asc 0 -> index 1
  else
    code := Ord(AnsiString(srcVal)[i]) + 1;             // the byte's code, 1-based into the table
  if (code < 1) or (code > Length(AnsiString(tabVal))) then Exit;
  AppendChar(PAnsiString(dstSlot)^, AnsiString(tabVal)[code]);
end;

procedure AotStrAppendMappedClassic(dstSlot, srcVal, tabVal: Pointer; i: PtrInt); cdecl;
var
  code: PtrInt;
begin
  if i < 1 then i := 1;                                 // CLASSIC clamps the start
  if i > Length(AnsiString(srcVal)) then
    code := 1                                           // empty substring -> Asc 0 -> index 1
  else
    code := Ord(AnsiString(srcVal)[i]) + 1;
  if (code < 1) or (code > Length(AnsiString(tabVal))) then Exit;
  AppendChar(PAnsiString(dstSlot)^, AnsiString(tabVal)[code]);
end;

procedure AotStrChr(dstSlot: Pointer; code: PtrInt); cdecl;
begin
  AssignChar(PAnsiString(dstSlot)^, code and $FF);
end;

function AotStrInstr(hayVal, needleVal: Pointer; start: PtrInt): PtrInt; cdecl;
begin
  if start < 1 then start := 1;
  Result := Pos(AnsiString(needleVal), Copy(AnsiString(hayVal), start, MaxInt));
  if Result > 0 then Inc(Result, start - 1);
end;

{ Str() of an int and Val(): the bcIntToString/bcStrVal/bcStrValInt handlers are
  dialect-independent one-liners over these library parsers (defined further down;
  forward-declared here so the primitives can sit with their C5 siblings). Float Str()
  stays on the runtime helper: its handler needs the console-behavior object. }
function ParseLeadingInt64(const S: string): Int64; forward;

// REGEXREPL: 1 = build the replacement in one measured allocation (the default), 0 = the library's
// own quadratic Replace. -1 = the environment has not been read yet. Read once, on the first
// substitution of the run.
var
  GRegexReplLinear: Integer = -1;

function RegexCountMatches(const S, Pattern: string): Int64;
// REGEXCOUNT: how many NON-OVERLAPPING matches of Pattern are in S. Backed by FPC's own RegExpr, so a
// program gets a real regex engine rather than something hand-rolled - the point of having it at all.
// A malformed pattern answers 0 rather than aborting the program, matching how the string builtins
// around it treat bad input.
var
  R: TRegExpr;
begin
  Result := 0;
  if (Pattern = '') or (S = '') then Exit;
  R := TRegExpr.Create;
  try
    try
      // '.' must NOT match a newline. TRegExpr defaults ModifierS to TRUE, PCRE and Python default it
      // to false, and patterns are written for the latter: ">.*\n" is meant to eat one description
      // line, and with the dot matching newlines it ate the entire input in one go.
      R.ModifierS := False;
      R.Expression := Pattern;
      if R.Exec(S) then
        repeat
          Inc(Result);
        until not R.ExecNext;
    except
      Result := 0;
    end;
  finally
    R.Free;
  end;
end;

function RegexReplaceAll(const S, Pattern, Repl: string): string;
// REGEXREPLACE: every match of Pattern in S replaced by Repl. The replacement is LITERAL text (no \1
// group references): the substitutions this is built for are plain, and taking the text as-is means a
// replacement containing a backslash cannot silently turn into something else.
//
// ⛔ This deliberately does NOT call TRegExpr.Replace, which is QUADRATIC. Its body is
//
//     Result := '';
//     repeat  Result := Result + Copy(input, prev, matchPos - prev);
//             Result := Result + replacement;  until not ExecNext;
//     Result := Result + Copy(input, prev, MaxInt);
//
// - a growing AnsiString built by repeated concatenation, so with M matches and an output of N bytes
// it copies O(M*N). On regex-redux two of the substitutions have ~70 000 matches per megabyte, which
// is 140 000 concatenations each re-copying the whole accumulated result. Decomposed (probe:
// job/tests/bench/regexredux_phases.bas), the nine REGEXCOUNT calls scale perfectly linearly - 376,
// 736, 1502, 3004 ms across four doublings - while the replacements go 141, 312, 750, 2095: factors
// of 2.21, 2.40 and 2.79, growing. They grow rather than jumping straight to 4x because at small
// sizes the allocator can still extend the buffer in place; that accident stops working as the
// string gets big, which is the same lesson as the append-in-place work.
//
// The matching is untouched - same engine, same expression object, same Exec/ExecNext walk, so the
// same matches in the same order. What changes is that the OUTPUT is measured first and built once:
// one allocation, then Move for each span. The match walk still runs exactly once.
var
  R: TRegExpr;
  MStart, MLen: array of Integer;
  NM, i, OutLen, SrcPos, DstPos, SegLen, RL, SLen: Integer;
begin
  Result := S;
  if Pattern = '' then Exit;
  R := TRegExpr.Create;
  try
    try
      R.ModifierS := False;      // as in RegexCountMatches: '.' stops at end of line
      R.Expression := Pattern;
      // REGEXREPL=0 restores the library's own quadratic Replace. It is the A/B for this work on a
      // single binary, and the differential the guard is run under: the two paths must agree on
      // every output, byte for byte, or the rewrite has changed a semantics rather than a cost.
      if GRegexReplLinear < 0 then
        if GetEnvironmentVariable('REGEXREPL') = '0' then GRegexReplLinear := 0
        else GRegexReplLinear := 1;
      if GRegexReplLinear = 0 then
      begin
        Result := R.Replace(S, Repl, False);
        Exit;
      end;
      NM := 0;
      if R.Exec(S) then
      begin
        SetLength(MStart, 64);
        SetLength(MLen, 64);
        repeat
          if NM = Length(MStart) then
          begin
            SetLength(MStart, NM * 2);
            SetLength(MLen, NM * 2);
          end;
          MStart[NM] := R.MatchPos[0];
          MLen[NM] := R.MatchLen[0];
          Inc(NM);
        until not R.ExecNext;
      end;
      if NM = 0 then Exit;                   // no match: Result is already S, and shares its buffer
      SLen := Length(S);
      RL := Length(Repl);
      OutLen := SLen;
      for i := 0 to NM - 1 do OutLen := OutLen - MLen[i] + RL;
      // Not SetLength on Result while it still aliases S - that would force a copy of S first.
      Result := '';
      SetLength(Result, OutLen);
      DstPos := 1;
      SrcPos := 1;
      for i := 0 to NM - 1 do
      begin
        SegLen := MStart[i] - SrcPos;        // the untouched span before this match
        if SegLen > 0 then
        begin
          Move(S[SrcPos], Result[DstPos], SegLen);
          Inc(DstPos, SegLen);
        end;
        if RL > 0 then
        begin
          Move(Repl[1], Result[DstPos], RL);
          Inc(DstPos, RL);
        end;
        SrcPos := MStart[i] + MLen[i];
      end;
      SegLen := SLen - SrcPos + 1;           // the tail after the last match
      if SegLen > 0 then Move(S[SrcPos], Result[DstPos], SegLen);
    except
      Result := S;
    end;
  finally
    R.Free;
  end;
end;
function ParseLeadingFloat(const S: string): Double; forward;

procedure AotIntToString(dstSlot: Pointer; v: Int64); cdecl;
begin
  PAnsiString(dstSlot)^ := IntToStr(v);
end;

function AotStrVal(sVal: Pointer): Double; cdecl;
begin
  Result := ParseLeadingFloat(AnsiString(sVal));
end;

function AotStrValInt(sVal: Pointer): Int64; cdecl;
begin
  Result := ParseLeadingInt64(AnsiString(sVal));
end;

{ Resolve what a compiled AOT function returned (C3). Normally that is just the resume PC and
  this is a compare and a return; the two negative sentinels mean a runtime helper hit
  something native code cannot finish.

  This lives in its own method rather than at the call site in RunTemplate because
  RunFast/RunDebug are the interpreter's hot loop, and this binary's dispatch speed is
  measurably sensitive to code layout (job/docs/PIANO_B1_AOT_DESIGN.md §5.7). Keeping the
  loop's source as small as it was is cheap insurance; nothing here is on a hot path.

  Raising from here is still correct: the caller invokes it inside its try..except, so the
  exception surfaces exactly where ON ERROR / TRAP / Err / RESUME are handled. }
function TBytecodeVM.AotSettle(C: TExecutionContext; R: PtrInt): Integer;
var
  E: TObject;
begin
  {$IFDEF DEBUG_AOTTRACE}
  if GetEnvironmentVariable('AOT_TRACE') <> '' then
    WriteLn(ErrOutput, '[AOT] native returned ', R);
  {$ENDIF}
  if R >= 0 then Exit(Integer(R));
  Result := C.AotFaultPC;
  C.PC := Result;
  if R = AOT_HELPER_EXC then
  begin
    E := C.AotPendingExc;
    C.AotPendingExc := nil;
    // Tell the run loop's handler which instruction actually failed: from its point of view
    // the raise comes out of the AOT call site, whose CurPC is the region entry.
    C.AotRaisePC := Result;
    if E <> nil then raise E;
    // Sentinel with nothing parked: cannot happen, but resuming beats raising nil.
    Exit;
  end;
  // AOT_HELPER_HALT: the instruction ended the run. Clearing Running exits the loop through
  // its own condition, so the template needs no break of its own.
  C.Running := False;
end;

procedure TBytecodeVM.GetRecordLayout(out RecordsOff, RecSize, RecIntOff, RecFloatOff,
  SharedRecOff: Integer);
var
  RecTmp: TRecordStorage;
begin
  RecordsOff  := Integer(PtrUInt(@FCtx.Records) - PtrUInt(Pointer(FCtx)));
  RecSize     := SizeOf(TRecordStorage);
  RecIntOff   := Integer(PtrUInt(@RecTmp.IntData) - PtrUInt(@RecTmp));
  RecFloatOff := Integer(PtrUInt(@RecTmp.FloatData) - PtrUInt(@RecTmp));
  // The shared region is an array of POINTERS on the VM instance, reached through ctx.VMSelf. Every
  // record of an ARRAY OF UDT lives there (AllocSharedRecord), which is the common case in real
  // BASIC, so a native path that handled only the per-context heap would miss all of it.
  SharedRecOff := Integer(PtrUInt(@FSharedRecords) - PtrUInt(Pointer(Self)));
end;

procedure TBytecodeVM.RegisterAotFunc(EntryPC: Integer; Mem: TObject; LastPC: Integer);
var
  i: Integer;
begin
  // Remember which PCs this region owns, so the loop JIT can leave them alone (see BuildJitLoops).
  if (EntryPC >= 0) and (LastPC >= EntryPC) and (FProgram <> nil) then
  begin
    if Length(FAotCovered) <> FProgram.GetInstructionCount then
    begin
      SetLength(FAotCovered, 0);
      SetLength(FAotCovered, FProgram.GetInstructionCount);
    end;
    for i := EntryPC to LastPC do
      if i < Length(FAotCovered) then FAotCovered[i] := True;
  end;
  if (FProgram = nil) or (Mem = nil) then
  begin
    Mem.Free;
    Exit;
  end;
  if Length(FNativeFuncs) <> FProgram.GetInstructionCount then
  begin
    for i := 0 to High(FNativeFuncs) do FNativeFuncs[i].Free;
    SetLength(FNativeFuncs, 0);
    SetLength(FNativeFuncs, FProgram.GetInstructionCount);   // all nil
  end;
  if (EntryPC >= 0) and (EntryPC < Length(FNativeFuncs)) then
  begin
    FNativeFuncs[EntryPC].Free;
    FNativeFuncs[EntryPC] := TExecMem(Mem);
    // Force a descriptor rebuild before the first native call: with --aot alone (no --jit)
    // nothing else may have primed FJitArrDesc yet.
    FArraysDirty := True;
  end
  else
    Mem.Free;
end;

procedure TBytecodeVM.RebuildJitArrDesc;
var
  a, n: Integer;
begin
  // 4 Int64 per array (32 bytes): IntData ptr, FloatData ptr, Count (TotalSize), lower bound of dim 0.
  // LBound lets the JIT compile LBOUND/UBOUND(arr) for a 1-D array (dim 0); other dims / the rank query
  // deopt to the interpreter.
  n := Length(FArrays);
  SetLength(FJitArrDesc, n * 4 + 4);   // +4 so @FJitArrDesc[0] is always valid even with no arrays
  for a := 0 to n - 1 do
  begin
    if Length(FArrays[a].IntData) > 0 then
      FJitArrDesc[a * 4 + 0] := Int64(PtrUInt(@FArrays[a].IntData[0]))
    else FJitArrDesc[a * 4 + 0] := 0;
    if Length(FArrays[a].FloatData) > 0 then
      FJitArrDesc[a * 4 + 1] := Int64(PtrUInt(@FArrays[a].FloatData[0]))
    else FJitArrDesc[a * 4 + 1] := 0;
    FJitArrDesc[a * 4 + 2] := FArrays[a].TotalSize;
    if Length(FArrays[a].LowerBounds) > 0 then
      FJitArrDesc[a * 4 + 3] := FArrays[a].LowerBounds[0]
    else FJitArrDesc[a * 4 + 3] := 0;
  end;
  FArraysDirty := False;
end;

{$IFDEF JIT_PROFILE}
{ DumpHotLoops - report the hot loops found by back-edge profiling (JIT milestone J1).
  A loop header is a PC that backward branches target often; the loop body runs from the header down to
  the LAST branch that jumps back to it. Prints each hot header, the body extent, the back-edge count and
  the source line -- the candidates the JIT will compile. }
procedure TBytecodeVM.DumpHotLoops(Threshold: Integer);
type
  PBytecodeInstr = ^TBytecodeInstruction;
var
  i, j, n, EndPC: Integer;
  Ins: PBytecodeInstr;
  Op: Word;
begin
  if FProgram = nil then Exit;
  n := FProgram.GetInstructionCount;
  if Length(FBackEdgeCount) < n then Exit;
  Ins := PBytecodeInstr(FProgram.GetInstructionsPtr);
  WriteLn('=== JIT hot-loop profile (back-edge count >= ', Threshold, ') ===');
  for i := 0 to n - 1 do
    if FBackEdgeCount[i] >= Threshold then
    begin
      // Loop body extent: the highest PC whose branch jumps back to this header.
      EndPC := i;
      for j := i + 1 to n - 1 do
      begin
        Op := Ins[j].OpCode;
        if ((Op = bcJump) or (Op = bcJumpIfZero) or (Op = bcJumpIfNotZero)) and
           (Ins[j].Immediate = i) then
          EndPC := j;
      end;
      WriteLn(Format('  hot loop  PC %d..%d  (%d instr)  back-edges=%d  src line %d',
        [i, EndPC, EndPC - i + 1, FBackEdgeCount[i], FProgram.GetSourceLine(i)]));
    end;
  WriteLn('=== end hot-loop profile ===');
end;
{$ENDIF}

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
function FitBaseDigits(const S: string; Digits: Int64): string;
// The optional "digits" width of HEX$/OCT/BIN. FreeBASIC: "if you specify digits > 0, the result string
// will be exactly that length" -- left-padded with zeros when the value is shorter, cut to the RIGHTMOST
// digits when it is longer. 0 (the SSA's default when the argument is absent) means the natural length.
begin
  Result := S;
  if Digits <= 0 then Exit;
  if Length(Result) > Digits then
    Result := Copy(Result, Length(Result) - Digits + 1, Digits)
  else
    while Length(Result) < Digits do
      Result := '0' + Result;
end;

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
  Len, StartPos, Count, EnvIdx, Idx: Integer;
  S, SubStr: string;
  PackInt: Int64;        // B3 serialization scratch (MK*/CV* integer pack/unpack)
  PackSingle: Single;
  PackDouble: Double;
begin
  SubOp := Instr.OpCode and $FF;  // Extract sub-opcode (low byte)
  case SubOp of
    0: // bcStrConcat
      // "s = s + x" is THE string idiom, and written as a plain concatenation it is QUADRATIC: each
      // iteration allocates a buffer the size of the whole accumulator and copies it. Pascal's own
      // "s := s + x" is linear because FPC recognises the self-append and grows in place, but that
      // recognition cannot fire through StringRegs[i] with an index only known at run time.
      // So detect the shape here. It is reachable at all only because the peephole now fuses the
      // temporary away (OptimizeStringTempCopy): before that, Dest was always a fresh temp, never
      // Src1, and the accumulator's buffer was shared besides -- so a grow in place would have been
      // both unreachable and wrong.
      if (Instr.Dest = Instr.Src1) and (Instr.Dest <> Instr.Src2) then
        AppendString(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src2])
      // Immediate = -1: the compiler proved the LEFT operand is dead right after this instruction
      // (RunConcatDeadSourceMark). Then its buffer can be taken over instead of rebuilding the whole
      // string: move it into Dest, drop the source's reference so the buffer becomes unshared, and
      // append in place. This is what makes "s = s + x" linear even when Dest is a DIFFERENT register
      // from Src1 -- the shape the loop-carried PHI copies always produce.
      else if (Instr.Immediate = -1) and (Instr.Dest <> Instr.Src2) and (Instr.Src1 <> Instr.Src2) then
      begin
        Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Src1];
        Ctx.StringRegs[Instr.Src1] := '';
        AppendString(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src2]);
      end
      else
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
        // Negative length = the rest of the string, exactly as for the byte-string MID (see above).
        if (Count < 0) and Assigned(FProgram) and FProgram.ModernMode then
          Count := Utf8CPCount(Ctx.StringRegs[Instr.Src1]) - StartPos + 1;
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
        if DateLocaleMode then
        begin
          if Instr.Immediate = 1 then S := LocaleDayName(Count, True)
          else S := LocaleMonthName(Count, True);
        end
        else if Instr.Immediate = 1 then
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
        AssignSubstr(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src1], 1, Len);
      end;
    3: // bcStrRight
      begin
        Len := Ctx.IntRegs[Instr.Src2];
        S := Ctx.StringRegs[Instr.Src1];
        if Len < 0 then Len := 0;
        if Len > Length(S) then Len := Length(S);
        AssignSubstr(Ctx.StringRegs[Instr.Dest], S, Length(S) - Len + 1, Len);
      end;
    4: // bcStrMid - MID$(s, start, len)
      begin
        // Src2 = start position register (int)
        // Immediate = length register index (low 16 bits)
        StartPos := Ctx.IntRegs[Instr.Src2];
        Count := Ctx.IntRegs[Instr.Immediate and $FFFF];
        // FreeBASIC: a start position below 1 yields an EMPTY string (the position is 1-based, and FB does
        // not clamp it). Clamping to 1 instead returned the first character, so "For n = 0 To Len(s):
        // Mid(s,n,1)" processed character 1 twice and doubled the leading letter (Rosetta "XML/Output").
        // CLASSIC v7 keeps the clamp (its MID$ has no such rule).
        if (StartPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          Ctx.StringRegs[Instr.Dest] := ''
        else
        begin
          if StartPos < 1 then StartPos := 1;
          // A NEGATIVE length returns the rest of the string in FreeBASIC: "if n < 0 or n >= len(str)
          // then all of the remaining characters are returned" (manual, Mid function). Clamping it to 0
          // instead dropped the final field of the common split idiom -- "Mid(s, p + 1, Instr(...) - p - 1)"
          // computes a negative length on the last token, because Instr returns 0 when it runs out.
          // CLASSIC keeps the clamp: Commodore v7 has no such rule (it rejects a negative length outright).
          if Count < 0 then
          begin
            if Assigned(FProgram) and FProgram.ModernMode then
              Count := Length(Ctx.StringRegs[Instr.Src1]) - StartPos + 1
            else
              Count := 0;
            if Count < 0 then Count := 0;
          end;
          AssignSubstr(Ctx.StringRegs[Instr.Dest], Ctx.StringRegs[Instr.Src1], StartPos, Count);
        end;
      end;
    51: // bcStrAscMid - ASC(MID$(s, start, len)) without building the substring.
      begin
        // The answer is the FIRST byte of that substring, so all the substring rules matter only
        // insofar as they decide whether it is EMPTY. Every branch below mirrors bcStrMid's, in the
        // same order, followed by bcStrAsc's "empty yields 0" - the two arms must not drift apart.
        StartPos := Ctx.IntRegs[Instr.Src2];
        Count := Ctx.IntRegs[Instr.Immediate and $FFFF];
        S := Ctx.StringRegs[Instr.Src1];
        if (StartPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          Ctx.IntRegs[Instr.Dest] := 0        // FB: a start below 1 is an empty string, not the first char
        else
        begin
          if StartPos < 1 then StartPos := 1;  // CLASSIC clamps
          if Count < 0 then
          begin
            // FB: a negative length means "the rest of the string"; CLASSIC rejects it (length 0).
            if Assigned(FProgram) and FProgram.ModernMode then
              Count := Length(S) - StartPos + 1
            else
              Count := 0;
            if Count < 0 then Count := 0;
          end;
          if (Count <= 0) or (StartPos > Length(S)) then
            Ctx.IntRegs[Instr.Dest] := 0
          else
            Ctx.IntRegs[Instr.Dest] := Ord(S[StartPos]);
        end;
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
      AssignChar(Ctx.StringRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1] and $FF);
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
      // Immediate = 1 when the argument is SINGLE-typed: 7 significant digits, as PRINT gives it.
      if Assigned(FProgram) and FProgram.ModernMode then
        Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1],
                                                                         Instr.Immediate = 1))
      else
        Ctx.StringRegs[Instr.Dest] := TrimRight(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1]));
    8: // bcStrVal - VAL(s): leading floating-point number, FreeBASIC style (leading parse + &H/&O/&B).
      begin
        Ctx.FloatRegs[Instr.Dest] := ParseLeadingFloat(Ctx.StringRegs[Instr.Src1]);
      end;
    9: // bcStrHex - HEX$(n[, digits]) - full INT64 range. Src2 = digits width (0 = no leading zeros).
      begin
        S := IntToHex(Ctx.IntRegs[Instr.Src1], 1);  // Minimum 1 digit
        // IntToHex with digits=1 still pads, so trim leading zeros
        while (Length(S) > 1) and (S[1] = '0') do
          Delete(S, 1, 1);
        Ctx.StringRegs[Instr.Dest] := FitBaseDigits(S, Ctx.IntRegs[Instr.Src2]);
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
    48: // bcStrInstrAny - INSTR([start,] str, Any set) -> FIRST position of any char in the set (1-based, 0 if none)
      begin
        // The mirror of bcStrInstrRevAny, scanning FORWARD, and it honours a start position exactly as
        // bcStrInstr does: Immediate is the int register holding the 1-based start (the 2-arg form passes a
        // register holding 1). An EMPTY set matches nothing -- that is what FreeBASIC returns.
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];   // character set
        StartPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        if StartPos < 1 then StartPos := 1;
        Len := 0;
        if SubStr <> '' then
          for Idx := StartPos to Length(S) do
            if Pos(S[Idx], SubStr) > 0 then
            begin
              Len := Idx;
              Break;
            end;
        Ctx.IntRegs[Instr.Dest] := Len;
      end;
    11: // bcStrErr - ERR$(n)
      Ctx.StringRegs[Instr.Dest] := SedaiExecutorErrors.GetErrorCodeDescription(Ctx.IntRegs[Instr.Src1]);
    19: // bcStrOct - OCT(n[, digits]) - octal string, full INT64 range. Src2 = digits width (0 = natural).
      Ctx.StringRegs[Instr.Dest] := FitBaseDigits(IntToBaseStr(Ctx.IntRegs[Instr.Src1], 8), Ctx.IntRegs[Instr.Src2]);
    20: // bcStrBin - BIN(n[, digits]) - binary string, full INT64 range. Src2 = digits width (0 = natural).
      Ctx.StringRegs[Instr.Dest] := FitBaseDigits(IntToBaseStr(Ctx.IntRegs[Instr.Src1], 2), Ctx.IntRegs[Instr.Src2]);
    21: // bcStrValInt - VALINT/VALLNG/VALUINT(s) - parse leading integer (0 if none)
      Ctx.IntRegs[Instr.Dest] := ParseLeadingInt64(Ctx.StringRegs[Instr.Src1]);
    49: // bcRegexCount - REGEXCOUNT(s, pattern): non-overlapping matches
      Ctx.IntRegs[Instr.Dest] := RegexCountMatches(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2]);
    50: // bcRegexReplace - REGEXREPLACE(s, pattern, repl): every match replaced (repl in the Immediate string reg)
      Ctx.StringRegs[Instr.Dest] := RegexReplaceAll(Ctx.StringRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2],
                                                    Ctx.StringRegs[Instr.Immediate]);
  else
    raise Exception.CreateFmt('Unknown string opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

function ParseDateSerial(const S: string; out DT: TDateTime): Boolean;
// Parse a date/time string into a TDateTime serial. Accepts ISO-ish forms deterministically across
// platforms/locales: "yyyy-mm-dd", "yyyy/mm/dd", "hh:mm[:ss]", or "<date> <time>". Anything else falls
// back to the locale parser. Used by DATEVALUE/TIMEVALUE/ISDATE.
var
  ds, ts, w, up: string;
  sp, y, mo, d, hh, mi, ss: Integer;
  dpart, tpart: TDateTime;
  haveD, haveT, isAM, isPM: Boolean;

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
  // 12-hour marker, stripped BEFORE anything else looks at the string. Two things went wrong without
  // this: "07:12:28AM" split into 07/12/"28AM", and StrToIntDef gave the seconds as 0; and
  // "07:12:28 AM" split on the SPACE, so "07:12:28" was handed to the date parser, which failed and
  // took the whole value with it. PM was simply ignored.
  isAM := False; isPM := False;
  if Length(w) >= 2 then
  begin
    up := UpperCase(w);
    if Copy(up, Length(up) - 1, 2) = 'PM' then
    begin isPM := True; w := Trim(Copy(w, 1, Length(w) - 2)); end
    else if Copy(up, Length(up) - 1, 2) = 'AM' then
    begin isAM := True; w := Trim(Copy(w, 1, Length(w) - 2)); end;
    if w = '' then Exit;
  end;
  sp := Pos(' ', w);
  if sp > 0 then begin ds := Trim(Copy(w, 1, sp - 1)); ts := Trim(Copy(w, sp + 1, Length(w))); end
  else if Pos(':', w) > 0 then begin ds := ''; ts := w; end
  else begin ds := w; ts := ''; end;
  if ds <> '' then
  begin
    // Locale mode asks the SYSTEM parser first, so "28-11-2005" reads as dd-mm-yyyy where that is the
    // regional convention - which is what fbc does. Deterministic mode keeps ISO-ish first and falls
    // back to the locale only for what it does not recognise, so the same source gives the same answer
    // on every machine.
    if DateLocaleMode and LocaleDateFields(ds, y, mo, d) and TryEncodeDate(y, mo, d, dpart) then
      haveD := True
    else if DateLocaleMode and TryStrToDate(ds, dpart) then
      haveD := True
    else if (Pos('-', ds) > 0) and (SplitInts(ds, '-', y, mo, d) >= 3) and TryEncodeDate(y, mo, d, dpart) then
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
  // Apply the 12-hour marker to the encoded fraction: 0.5 is exactly noon, so "before noon" is the
  // whole test. 12:30 PM stays 12:30 and 12:30 AM becomes 00:30, which is the rule VB and FB follow.
  if haveT then
  begin
    if isPM and (tpart < 0.5) then tpart := tpart + 0.5
    else if isAM and (tpart >= 0.5) then tpart := tpart - 0.5;
  end;
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
      Ctx.FloatRegs[Instr.Dest] := LnFloat(Ctx.FloatRegs[Instr.Src1]);
    5: // bcMathExp
      Ctx.FloatRegs[Instr.Dest] := Exp(Ctx.FloatRegs[Instr.Src1]);
    6: // bcMathSqr
      Ctx.FloatRegs[Instr.Dest] := SqrtFloat(Ctx.FloatRegs[Instr.Src1]);
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
procedure TBytecodeVM.EraseArray(ArrayIdx: Integer; Deallocate: Boolean = False);
// ERASE. A STATIC array (Deallocate=False) keeps its bounds and only zeroes its elements. A DYNAMIC array
// (Deallocate=True) is FREED: its storage is released and EVERY dimension reports LBound 0 / UBound -1
// until a later REDIM grows it again -- exactly as FreeBASIC does (the dimension COUNT is kept, so
// "UBound(a, 2)" of a freed 2-D array still answers -1, not a stale bound).
var
  k, d: Integer;
begin
  if (ArrayIdx < 0) or (ArrayIdx >= Length(FArrays)) then Exit;
  if Deallocate then
  begin
    SetLength(FArrays[ArrayIdx].IntData, 0);
    SetLength(FArrays[ArrayIdx].FloatData, 0);
    SetLength(FArrays[ArrayIdx].StringData, 0);
    for d := 0 to High(FArrays[ArrayIdx].Dimensions) do
      FArrays[ArrayIdx].Dimensions[d] := 0;        // UBound(d) = LowerBound(d) + 0 - 1 = -1
    for d := 0 to High(FArrays[ArrayIdx].LowerBounds) do
      FArrays[ArrayIdx].LowerBounds[d] := 0;       // LBound(d) = 0
    FArrays[ArrayIdx].TotalSize := 0;
    Exit;
  end;
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

procedure TBytecodeVM.RedimArrayN(ArrayIdx: Integer; const Uppers: array of Integer; Preserve: Boolean;
  const Lowers: array of Integer);
// REDIM a(l0 TO u0, ...) — re-dimension a multi-dimensional array. Each dimension's lower bound comes
// from Lowers[d] when supplied (an explicit "lb TO ub" range, possibly a runtime value), else the
// dimension's original lower bound is kept (a bare "ub"). PRESERVE keeps the flat element order up to
// the new size; otherwise the storage is cleared. Strides stay row-major (computed at access).
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
    if d <= High(Lowers) then
      Lb := Lowers[d]                           // explicit "lb TO ub" (runtime lb supported)
    else
      Lb := FArrays[ArrayIdx].LowerBounds[d];   // bare "ub": keep the original lower bound
    FArrays[ArrayIdx].LowerBounds[d] := Lb;
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

procedure ClearArrayStorage(var A: TArrayStorage);
// Reset a storage record to a well-formed EMPTY array (no dimensions, no data). Field-by-field, not
// FillChar: the dynamic-array fields are managed and must be released, not zeroed behind the RTL's back.
begin
  A.ElementType := 0;
  A.DimCount := 0;
  A.TotalSize := 0;
  SetLength(A.Dimensions, 0);
  SetLength(A.LowerBounds, 0);
  SetLength(A.IntData, 0);
  SetLength(A.FloatData, 0);
  SetLength(A.StringData, 0);
end;

procedure TBytecodeVM.ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  ArrayIdx, LinearIdx, i, ProdDims, ArrLowerBound: Integer;
  ArrInfo: TSSAArrayInfo;
  PtrAddr, DestArr: Int64;
  PtrOffset, RecSlot: Integer;
  Rec: PRecordStorage;
begin
  // This is the COLD array path - DIM/REDIM/ERASE/BIND and friends, any of which can resize or
  // move an array's backing store; the hot typed accessors never come through here. So the
  // JIT/AOT descriptor table must be rebuilt before the next compiled code reads it.
  //
  // Marked HERE rather than only at the interpreter's call site, which is where it used to
  // live: with the AOT runtime helper there is now a second caller, and a flag set by one
  // caller is a semantic the other silently loses. It cost a real bug to learn - compiled code
  // kept reading the pre-DIM descriptor and every array element came back 0.
  FArraysDirty := True;
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
        SetLength(FArrays[ArrayIdx].LowerBounds, ArrInfo.DimCount);
        for i := 0 to ArrInfo.DimCount - 1 do
        begin
          // Effective lower bound for this dimension: a RUNTIME lb register (FreeBASIC
          // "Dim a(Lbound(m) To Ubound(m))") overrides the compile-time constant. Recorded for
          // LBOUND/UBOUND and used to adjust the size (ub - lb + 1) and every element index.
          ArrLowerBound := 0;
          if (i < Length(ArrInfo.LowerBoundRegisters)) and (ArrInfo.LowerBoundRegisters[i] >= 0) then
            ArrLowerBound := Ctx.IntRegs[ArrInfo.LowerBoundRegisters[i]]
          else if i <= High(ArrInfo.LowerBounds) then
            ArrLowerBound := ArrInfo.LowerBounds[i];
          FArrays[ArrayIdx].LowerBounds[i] := ArrLowerBound;

          if ArrInfo.Dimensions[i] = 0 then
          begin
            if (i < Length(ArrInfo.DimRegisters)) and (ArrInfo.DimRegisters[i] >= 0) then
            begin
              // Variable upper bound: size = ub - lb + 1.
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
    {$I ArrayHotOps.inc}
    9: // bcArrayLBound - LBOUND(arr[, dim]) - Src2 = 0-based dim index (B1.4). Dim 0 (index -1) is the
       // special FreeBASIC query "how many dimensions": LBOUND(arr, 0) is always 1.
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if LinearIdx < 0 then
          Ctx.IntRegs[Instr.Dest] := 1
        else
          Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx];
      end;
    10: // bcArrayUBound - UBOUND(arr[, dim]) - upper = lower + size - 1 (B1.4). Dim 0 (index -1) is the
        // FreeBASIC "number of dimensions" query: the count of ALLOCATED dimensions -- a fixed array's rank,
        // and 0 for a dynamic array not yet dimensioned (TotalSize 0, which reports UBOUND(arr) = -1).
      begin
        ArrayIdx := Instr.Src1;
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if LinearIdx < 0 then
        begin
          if FArrays[ArrayIdx].TotalSize > 0 then
            Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].DimCount
          else
            Ctx.IntRegs[Instr.Dest] := 0;
        end
        else
          Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].LowerBounds[LinearIdx]
                                     + FArrays[ArrayIdx].Dimensions[LinearIdx] - 1;
      end;
    11: // bcArrayErase - ERASE arr (B1.4). Immediate 1 = dynamic array (free -> LBound 0/UBound -1);
        // 0 = static array (keep bounds, zero the elements).
      EraseArray(Instr.Src1, Instr.Immediate <> 0);
    12: // bcArrayRedim - REDIM [PRESERVE] arr([lb TO] ub) (B1.4); Src2=ub reg. Immediate: bit0=preserve,
        // bit1=has explicit lower bound, bits8+ = that (non-negative) lower bound. A RUNTIME lower bound
        // arrives via a preceding bcArrayRedimPush (LB flag) in FRedimPendingLBs and takes precedence.
      begin
        if Length(FRedimPendingLBs) > 0 then
        begin
          RedimArray(Instr.Src1, Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0,
                     True, FRedimPendingLBs[0]);
          SetLength(FRedimPendingLBs, 0);
        end
        else
          RedimArray(Instr.Src1, Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0,
                     (Instr.Immediate and 2) <> 0, Instr.Immediate shr 8);
      end;
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
    50: // bcRawLoadZStr - Dest(str) = C string at RawAddr(IntRegs[Src1]); Imm 1 = WSTRING (UCS-2).
        // Immediate >= 2 asks for EXACTLY (Immediate - 2) bytes instead of "up to the terminator": that
        // is what a fixed-length string FIELD of a UDT laid over raw memory is - n bytes, terminator or
        // not, which is why "As String*5 sig" over "GIF89a" reads "GIF89" and misses a character.
      if Instr.Immediate >= 2 then
        Ctx.StringRegs[Instr.Dest] := RawLoadBytesVal(Ctx.IntRegs[Instr.Src1], Instr.Immediate - 2)
      else
        Ctx.StringRegs[Instr.Dest] := RawLoadZStrVal(Ctx.IntRegs[Instr.Src1], Instr.Immediate = 1);
    51: // bcRawStoreZStr - StringRegs[Src2] chars + NUL -> RawAddr(IntRegs[Src1]); Imm 1 = WSTRING
      RawStoreZStrVal(Ctx.IntRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2], Instr.Immediate = 1);
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
    49: // bcArrayBindInd - PHASE 1 bind whose arg is a UDT ARRAY MEMBER: its FArrays handle is only known at
      begin  // runtime (per instance), so it arrives in a register instead of an immediate. Src1=param id,
             // Src2=handle reg. Always pushes a save-stack entry — bcArrayBindApply commits a FIXED count and
             // bcArrayUnbind pops LIFO by SlotId, so skipping a push here would desynchronize both.
        PtrAddr := Ctx.IntRegs[Instr.Src2];
        if Instr.Src1 >= 0 then
        begin
          if Instr.Src1 > High(FArrays) then SetLength(FArrays, Instr.Src1 + 1);  // grow AFTER reading the handle
          if FArrayBindTop >= Length(FArrayBindStack) then
            SetLength(FArrayBindStack, (FArrayBindTop + 1) * 2);
          FArrayBindStack[FArrayBindTop].SlotId := Instr.Src1;
          FArrayBindStack[FArrayBindTop].Saved := FArrays[Instr.Src1];
          if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) then
          begin
            FArrayBindStack[FArrayBindTop].ArgId := PtrAddr;
            FArrayBindStack[FArrayBindTop].Snapshot := FArrays[PtrAddr];   // alias the member's storage
          end
          else
          begin  // handle < 1 = member array never allocated: bind an EMPTY array (UBOUND = -1), and set
                 // ArgId = -1 so unbind performs no copy-back (there is no caller slot to write to).
            FArrayBindStack[FArrayBindTop].ArgId := -1;
            ClearArrayStorage(FArrayBindStack[FArrayBindTop].Snapshot);
          end;
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
    27: // bcArrayRedimPush - push one bound onto the pending REDIM list (Immediate bit0 = it is a
        // RUNTIME lower bound -> the parallel LB list; otherwise an upper bound).
      begin
        if (Instr.Immediate and 1) <> 0 then
        begin
          SetLength(FRedimPendingLBs, Length(FRedimPendingLBs) + 1);
          FRedimPendingLBs[High(FRedimPendingLBs)] := Ctx.IntRegs[Instr.Src1];
        end
        else
        begin
          SetLength(FRedimPendingUBs, Length(FRedimPendingUBs) + 1);
          FRedimPendingUBs[High(FRedimPendingUBs)] := Ctx.IntRegs[Instr.Src1];
        end;
      end;
    28: // bcArrayRedimN - commit a multi-dimensional REDIM using the pushed upper (and any lower) bounds
      begin
        RedimArrayN(Instr.Src1, FRedimPendingUBs, (Instr.Immediate and 1) <> 0, FRedimPendingLBs);
        SetLength(FRedimPendingUBs, 0);
        SetLength(FRedimPendingLBs, 0);
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
          RedimArrayN(PtrAddr, FRedimPendingUBs, (Instr.Immediate and 1) <> 0, FRedimPendingLBs);
        end;
        SetLength(FRedimPendingUBs, 0);
        SetLength(FRedimPendingLBs, 0);
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
    47: // bcArrayCopyContents - deep-copy FArrays[Src1] <- FArrays[Src2] (value semantics of an array member)
      begin
        DestArr := Ctx.IntRegs[Instr.Src1]; PtrAddr := Ctx.IntRegs[Instr.Src2];
        if (DestArr >= 1) and (DestArr <= High(FArrays)) and
           (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) then
        begin
          FArrays[DestArr].ElementType := FArrays[PtrAddr].ElementType;
          FArrays[DestArr].DimCount    := FArrays[PtrAddr].DimCount;
          FArrays[DestArr].TotalSize   := FArrays[PtrAddr].TotalSize;
          FArrays[DestArr].Dimensions  := Copy(FArrays[PtrAddr].Dimensions);
          FArrays[DestArr].LowerBounds := Copy(FArrays[PtrAddr].LowerBounds);
          FArrays[DestArr].IntData     := Copy(FArrays[PtrAddr].IntData);
          FArrays[DestArr].FloatData   := Copy(FArrays[PtrAddr].FloatData);
          FArrays[DestArr].StringData  := Copy(FArrays[PtrAddr].StringData);
        end;
      end;
    48: // bcArrayCopyRecords - value-copy an array-of-UDT member element-wise (independent element records)
      DeepCopyArrayRecords(Ctx, Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], Instr.Immediate);
  else
    raise Exception.CreateFmt('Unknown array opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

procedure TBytecodeVM.AdvancePrintCol(Ctx: TExecutionContext; Chars: Integer);
// The tracked cursor column is a SCREEN column, so it WRAPS at the right margin: text that runs past the
// last column continues on the next line, and the column starts over. The counter never did, and the one
// thing that reads it -- the PRINT comma zone -- then computed its next zone from a column that no screen
// ever has, decided it fell off the line, and broke the record in half.
//
// FreeBASIC does exactly this (verified against fbc, output redirected to a file, 80 columns): after 85
// characters it pads the comma to column 14 of the WRAPPED line -- 9 spaces, no newline -- while we
// emitted a newline and restarted at column 0. Note the wrap adds no newline of its own to the stream:
// on a console the line wraps by itself, and a redirected FreeBASIC writes the characters unbroken. Only
// the bookkeeping wraps.
begin
  Inc(Ctx.CursorCol, Chars);
  if Assigned(FConsoleBehavior) and (FConsoleBehavior.ScreenCols > 0) then
    Ctx.CursorCol := Ctx.CursorCol mod FConsoleBehavior.ScreenCols;
end;

procedure TBytecodeVM.ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  PrintStr, InputStr, CmdNewLine: string;
  InputVal: Double;
  NextTabCol, TabIdx: Integer;
  CmdErr: Integer;  // Error code for CMD-redirected output
  ScreenCol, ScreenRow: Integer;  // SCREEN(row, col): the cell, converted to the console's 0-based grid
begin
  CmdErr := 0;
  CmdNewLine := #13;  // CR for file newlines
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcPrint (float). Immediate = 1 when the value is SINGLE-typed: print it with a SINGLE's
       // 7 significant digits, which is what hides its representation error (8.300000190734863 -> "8.3").
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1], Instr.Immediate = 1);
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          AdvancePrintCol(Ctx, Length(PrintStr));
        end;
      end;
    1: // bcPrintLn (float); Immediate = 1 -> SINGLE precision, as for bcPrint above
      begin
        PrintStr := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1], Instr.Immediate = 1);
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
          AdvancePrintCol(Ctx, Length(PrintStr));
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
        PrintStr := FConsoleBehavior.FormatInt(Ctx.IntRegs[Instr.Src1]);  // exact 64-bit (no Double rounding above 2^53)
        if (FCmdHandle > 0) and Assigned(FOnFileData) then
          FOnFileData(Self, 'PRINT#', FCmdHandle, PrintStr, CmdErr)
        else if Assigned(FOutputDevice) then
        begin
          FOutputDevice.Print(PrintStr);
          AdvancePrintCol(Ctx, Length(PrintStr));
        end;
      end;
    5: // bcPrintIntLn
      begin
        PrintStr := FConsoleBehavior.FormatInt(Ctx.IntRegs[Instr.Src1]);  // exact 64-bit (no Double rounding above 2^53)
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
          AdvancePrintCol(Ctx, Length(PrintStr));
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
          AdvancePrintCol(Ctx, Length(PrintStr));
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
          AdvancePrintCol(Ctx, 0);   // wrap once the spaces are out (see the TAB branch)
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
              AdvancePrintCol(Ctx, 1);
            end;
          saSpaceBefore: ;
        end;
      end;
    8: // bcPrintTab
      // TAB/SPC are cursor MOVEMENTS. FreeBASIC (MODERN) emits them only onto a visible screen -- to a
      // redirected stream it writes nothing (there are no cells to skip over). CLASSIC v7 always emits the
      // spaces. So skip the whole thing in MODERN when the device has no visible screen.
      if Assigned(FOutputDevice) and
         ((not (Assigned(FProgram) and FProgram.ModernMode)) or FOutputDevice.IsScreenVisible) then
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
        // Wrap only now that the padding is done: wrapping inside the loop would send the column back to 0
        // and the loop would never reach NextTabCol.
        AdvancePrintCol(Ctx, 0);
      end;
    9: // bcPrintSpc
      if Assigned(FOutputDevice) and
         ((not (Assigned(FProgram) and FProgram.ModernMode)) or FOutputDevice.IsScreenVisible) then
      begin
        // Src1 = register containing space count (always a register from SSA)
        TabIdx := Ctx.IntRegs[Instr.Src1];
        while TabIdx > 0 do
        begin
          FOutputDevice.Print(' ');
          Inc(Ctx.CursorCol);
          Dec(TabIdx);
        end;
        AdvancePrintCol(Ctx, 0);   // wrap once the spaces are out (see the TAB branch)
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
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
            if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
              if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
                FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
            end;
          end
          else if Assigned(FOutputDevice) then
          begin
            FOutputDevice.Print('?REDO FROM START');
            FOutputDevice.NewLine;
            // Reprint prompt for retry
            if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
            if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
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
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        Ctx.StringRegs[Instr.Dest] := FInputDevice.ReadLine('? ', False, False, False);
        if FInputDevice.ShouldStop then
        begin
          Ctx.Running := False;
          FInputDevice.ClearStopRequest;
        end;
      end;
    18: // bcWInputChars - WINPUT(n [, [#]f]): count Unicode codepoints
      Ctx.StringRegs[Instr.Dest] :=
        ReadChars(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], True);
    19: // bcInputChars - INPUT(n [, [#]f]): count bytes
      Ctx.StringRegs[Instr.Dest] :=
        ReadChars(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], False);
    20: // bcConScreen - SCREEN(row, col [, colorflag]): read a console cell
      if Assigned(FOutputDevice) then
      begin
        // FB counts rows and columns from 1; the console addresses cells from 0.
        ScreenCol := Integer(Ctx.IntRegs[Instr.Src2]) - 1;
        ScreenRow := Integer(Ctx.IntRegs[Instr.Src1]) - 1;
        if Ctx.IntRegs[Instr.Immediate] = 0 then
          Ctx.IntRegs[Instr.Dest] := Int64(FOutputDevice.GetCharAt(ScreenCol, ScreenRow))
        else
          // Colour attribute, in FreeBASIC's <=4bpp palette-console packing: background in the high
          // nibble, foreground in the low one.
          Ctx.IntRegs[Instr.Dest] :=
            (Int64(FOutputDevice.GetBackColorAt(ScreenCol, ScreenRow) and $0F) shl 4) or
             Int64(FOutputDevice.GetColorAt(ScreenCol, ScreenRow) and $0F);
      end
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    21: // bcConLocate - MODERN LOCATE [row][, col]: the console text cursor (1-based).
      // Every argument is optional ("Declare Function Locate( row As Long = 0, column As Long = 0, ... )"),
      // and the default 0 means LEAVE THAT COORDINATE ALONE -- "Locate 10" moves to row 10 and keeps the
      // column. We required both and rejected the one-argument form outright.
      if Assigned(FOutputDevice) then
      begin
        ScreenRow := Integer(Ctx.IntRegs[Instr.Src1]);
        ScreenCol := Integer(Ctx.IntRegs[Instr.Src2]);
        if ScreenRow <= 0 then ScreenRow := FOutputDevice.GetCursorY + 1;
        if ScreenCol <= 0 then ScreenCol := FOutputDevice.GetCursorX + 1;
        FOutputDevice.SetCursor(ScreenCol - 1, ScreenRow - 1);
      end;
    22: // bcConViewPrint - VIEW PRINT [first TO last]: the text print area (scroll region)
      if Assigned(FOutputDevice) then
      begin
        ScreenRow := Integer(Ctx.IntRegs[Instr.Src1]);   // first row, 1-based (0 = whole screen)
        ScreenCol := Integer(Ctx.IntRegs[Instr.Src2]);   // last row, 1-based
        if (ScreenRow <= 0) or (ScreenCol <= 0) then
        begin
          ScreenRow := 1;
          ScreenCol := FOutputDevice.GetActualRows;
        end;
        if ScreenCol < ScreenRow then ScreenCol := ScreenRow;
        // The print area spans every column; only the rows are bounded. No clear -- FB leaves the
        // area's contents alone and only moves the cursor to the start of the first row.
        FOutputDevice.SetWindow(0, ScreenRow - 1, FOutputDevice.GetActualCols - 1, ScreenCol - 1, False);
        FOutputDevice.SetCursor(0, ScreenRow - 1);
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
    15: // bcLoadERFN - name of the procedure in which the last error occurred ('' at module level)
      Ctx.StringRegs[Instr.Dest] := Ctx.LastErrorProc;
    16: // bcLoadERMN - name of the module (source file) the error came from
      Ctx.StringRegs[Instr.Dest] := FProgram.ModuleName;
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

function TBytecodeVM.DrawSurface: Integer;
// The surface the FreeBASIC drawing ops (PSET/LINE/CIRCLE/PAINT/POINT) target: the per-statement image
// target when one is active ("PSET img,(x,y)"), otherwise the current work page.
begin
  if FGfxDrawTargetActive then Result := FGfxDrawTargetHandle else Result := FGfxWorkSurface;
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
  ScrData: PByte;      // SCREENPTR: working-page pixel bytes (existence check only)
  ScrSize: Integer;
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
        FGraphics.SetPixel(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
                           UInt32(Ctx.IntRegs[Instr.Immediate]));
        FDrawPenX := Ctx.IntRegs[Instr.Src1]; FDrawPenY := Ctx.IntRegs[Instr.Src2];  // becomes the current graphics point
      end;
    27: // bcGfxPoint - POINT(x, y [, img]) -> color  (reads the work page, or the image target when active)
      if Assigned(FGraphics) then
        Ctx.IntRegs[Instr.Dest] := Int64(FGraphics.GetPixel(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2])))
      else
        Ctx.IntRegs[Instr.Dest] := 0;
    28: // bcGfxPaint - PAINT (x,y), color  (flood fill; color in the Immediate int register)
      if Assigned(FGraphics) then
        FGraphics.Fill(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
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
          1: FGraphics.DrawRect(DrawSurface, GfxMapX(GetX1), GfxMapY(GetY1),
               GfxMapX(GetX2), GfxMapY(GetY2),
               UInt32(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]), False, 1, 0.0);    // B  = box outline
          2: FGraphics.DrawRect(DrawSurface, GfxMapX(GetX1), GfxMapY(GetY1),
               GfxMapX(GetX2), GfxMapY(GetY2),
               UInt32(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]), True, 1, 0.0);     // BF = filled box
        else
          FGraphics.DrawLine(DrawSurface, GfxMapX(GetX1), GfxMapY(GetY1),
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
        FGraphics.DrawEllipse(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
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
        if (Instr.Immediate and 1) <> 0 then
        begin FGfxForeColor := UInt32(Ctx.IntRegs[Instr.Src1]); FConColorFg := Ctx.IntRegs[Instr.Src1]; end;
        if (Instr.Immediate and 2) <> 0 then
        begin FGfxBackColor := UInt32(Ctx.IntRegs[Instr.Src2]); FConColorBg := Ctx.IntRegs[Instr.Src2]; end;
      end;
    35: // bcGfxForeColor - read the current colour. Immediate 0 = draw foreground, 1 = draw background
        //   (the omitted-colour defaults for PSET and PRESET); 2 = console foreground, 3 = console
        //   background, which is what FreeBASIC's "Color()" packs into its result.
      case Instr.Immediate of
        1: Ctx.IntRegs[Instr.Dest] := Int64(FGfxBackColor);
        2: Ctx.IntRegs[Instr.Dest] := FConColorFg;
        3: Ctx.IntRegs[Instr.Dest] := FConColorBg;
      else
        Ctx.IntRegs[Instr.Dest] := Int64(FGfxForeColor);
      end;
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
    63: // bcGfxScreenPtr - SCREENPTR: a raw pointer to the working page's framebuffer.
        //  Offset 0 of the framebuffer REGION of the raw-pointer namespace: dereferencing it goes through
        //  the ordinary raw load/store path, which bounds-checks against the surface's byte size. FB
        //  returns 0 when there is no graphics screen; do the same rather than hand out a pointer that
        //  would only fail later.
      begin
        if Assigned(FGraphics) and FGraphics.SurfaceData(FGfxWorkSurface, ScrData, ScrSize) and (ScrSize > 0) then
          Ctx.IntRegs[Instr.Dest] := RAWPTR_TAG or RAWPTR_REGION_FB
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    64: // bcGfxImageConvertRow - IMAGECONVERTROW(src, src_bpp, dst, dst_bpp, width [, isrgb])
      ImageConvertRowExec(Ctx, Instr);
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
    59: // bcGfxCircleEx - CIRCLE ellipse/arc. Src1=x, Src2=y, Dest=RX; Immediate [0-15]=RY, [16-31]=color,
        // [32-47]=start-angle-degrees, [48-63]=end-angle-degrees (all int regs). Angles are already in
        // degrees; RX/RY already carry the aspect ratio. Centre mapped and radii scaled by the WINDOW axes.
      if Assigned(FGraphics) then
      begin
        if FGfxWinActive then
        begin
          GetX1 := Round(Ctx.IntRegs[Instr.Dest] * Abs(FGfxWinAx));            // RX physical
          GetY1 := Round(Ctx.IntRegs[Instr.Immediate and $FFFF] * Abs(FGfxWinAy));  // RY physical
        end
        else
        begin
          GetX1 := Ctx.IntRegs[Instr.Dest];                                    // RX
          GetY1 := Ctx.IntRegs[Instr.Immediate and $FFFF];                     // RY
        end;
        FGraphics.DrawEllipse(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
          GetX1, GetY1,
          UInt32(Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF]),
          Double(Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]),   // start angle (degrees)
          Double(Ctx.IntRegs[(Instr.Immediate shr 48) and $FFFF]),   // end angle (degrees)
          0.0, 0.0, 1);
        FDrawPenX := Ctx.IntRegs[Instr.Src1]; FDrawPenY := Ctx.IntRegs[Instr.Src2];  // centre becomes the current point
      end;
    60: // bcGfxPaintBorder - PAINT (x,y),color,border : boundary flood fill (stops at the border colour).
        // Src1=x, Src2=y; Immediate [0-15]=color, [16-31]=border (int regs).
      if Assigned(FGraphics) then
        FGraphics.FillBorder(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
          UInt32(Ctx.IntRegs[Instr.Immediate and $FFFF]),
          UInt32(Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF]));
    61: // bcGfxSetTarget - set/clear the per-statement image draw target. Src1=handle; Immediate bit 0 = active.
      if (Instr.Immediate and 1) <> 0 then
      begin
        FGfxDrawTargetActive := True;
        FGfxDrawTargetHandle := Ctx.IntRegs[Instr.Src1];
      end
      else
        FGfxDrawTargetActive := False;
    62: // bcGfxLineStyled - LINE (x1,y1)-(x2,y2),color,[B],style : styled (dashed) line or box outline.
        // Src1=x1, Src2=y1, Dest=x2; Immediate [0-15]=y2, [16-31]=color, [32-47]=style, [48-49]=shape.
      if Assigned(FGraphics) then
      begin
        GetX1 := GfxMapX(Ctx.IntRegs[Instr.Src1]); GetY1 := GfxMapY(Ctx.IntRegs[Instr.Src2]);
        GetX2 := GfxMapX(Ctx.IntRegs[Instr.Dest]); GetY2 := GfxMapY(Ctx.IntRegs[(Instr.Immediate) and $FFFF]);
        GetSx := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];         // colour
        GetSy := Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF] and $FFFF;   // style mask (16-bit)
        if ((Instr.Immediate shr 48) and $3) = 1 then
        begin
          // B: styled box outline = four styled edges (pattern restarts on each edge).
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY1, GetX2, GetY1, UInt32(GetSx), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX2, GetY1, GetX2, GetY2, UInt32(GetSx), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX2, GetY2, GetX1, GetY2, UInt32(GetSx), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY2, GetX1, GetY1, UInt32(GetSx), Word(GetSy));
        end
        else
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY1, GetX2, GetY2, UInt32(GetSx), Word(GetSy));
        FDrawPenX := Ctx.IntRegs[Instr.Dest]; FDrawPenY := Ctx.IntRegs[(Instr.Immediate) and $FFFF];
      end;
  else
    raise Exception.CreateFmt('Unknown graphics opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
end;

{$IFDEF WITH_SEDAI_AUDIO}
procedure TBytecodeVM.EnsureAudioStarted;
begin
  // Lazy device open, moved verbatim from the constructor: the first audio op pays it once;
  // a failed open is not retried (FAudioStartTried), matching the old "failed at startup"
  // behavior where every handler just saw FAudioInitialized = False.
  if FAudioStartTried then Exit;
  FAudioStartTried := True;

  FSIDEvo := TSedaiSIDEvo.Create;
  FSIDEvo.Initialize(1);  // 1 group = 8 voices
  GSIDEvoInstance := FSIDEvo;

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
    if FAudioBackend.Start then
    begin
      FAudioInitialized := True;
      {$IFDEF DEBUG_AUDIO}
      WriteLn('[DEBUG_AUDIO] SAF Audio initialized and started OK');
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
end;
{$ENDIF}

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
  {$IFDEF WITH_SEDAI_AUDIO}
  EnsureAudioStarted;   // lazy device open: only programs that reach an audio op pay it
  {$ENDIF}
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
  QVal: Int64;         // bcFileQuery numeric fast path result (unmanaged: costs nothing to declare)
  BinI: Int64;
  BinF: Double;
  BinS: Single;
  BinLen: Longint;
  BinWidth: Integer;   // binary PUT/GET element byte width (from the variable's declared type)
  BinCount, BinBank, k: Integer;
  BinArr: ^TArrayStorage;
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
    0, 2, 34: // bcDopen, bcOpen, bcOpenFunc
      begin
        // DOPEN #handle, "filename" [, mode$]
        // Src1 = handle, Src2 = filename, Immediate = mode register (or 0)
        //
        // bcOpenFunc is FreeBASIC's FUNCTION form: the SAME open, except that the error code is DELIVERED
        // in Dest instead of raising - "If Open(f For Input As #1) <> 0 Then" is how a FreeBASIC program
        // handles a missing file. It shares this arm so the two forms can never drift apart.
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
        if SubOp = 34 then Ctx.IntRegs[Instr.Dest] := 0;   // function form: 0 until proven otherwise
        // Commodore OPEN to a device/command channel (no filename, e.g. OPEN 1,8,15) is a no-op here:
        // there is no drive to command, so opening nothing must not raise.
        if Filename = '' then Exit;
        if Assigned(FOnDiskFile) then
        begin
          FOnDiskFile(Self, 'DOPEN', HandleNum, HandleName, Filename, Mode, ErrorCode);
          if SubOp = 34 then
            // The FUNCTION form is FreeBASIC's, so it answers with FreeBASIC's code, not the Commodore
            // one the statement form raises: 62 FILE NOT FOUND is fbc's 2, and the two failure codes the
            // file layer can otherwise return are its 3 (file I/O error). The statement form is untouched.
            case ErrorCode of
              0:      Ctx.IntRegs[Instr.Dest] := 0;
              62:     Ctx.IntRegs[Instr.Dest] := 2;
            else      Ctx.IntRegs[Instr.Dest] := 3;
            end
          else if ErrorCode <> 0 then
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
          FilePrintColAdvance(HandleNum, Data);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# error %d writing to file: %d', [ErrorCode, HandleNum]);
        end
        else
          raise Exception.Create('PRINT# command not supported: no handler assigned');
      end;

    27: // bcPrintFileComma - pad spaces in the FILE to the next 14-column zone (fbc-verified:
        // "Print #1, a, b" writes the zone padding INTO the file; it used to leak to stdout).
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := StringOfChar(' ', 14 - (FilePrintColGet(HandleNum) mod 14));
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          FilePrintColAdvance(HandleNum, Data);
          if ErrorCode <> 0 then
            raise Exception.CreateFmt('PRINT# error %d writing to file: %d', [ErrorCode, HandleNum]);
        end;
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

    11: // bcPrintFileNewLine - newline to file (dialect-specific line ending)
      begin
        { PRINT# newline. CLASSIC writes CHR$(13) alone - real C128 DOS files end lines with a
          bare CR. MODERN writes the platform line ending like fbc does (CRLF on Windows, LF
          elsewhere - bleh.dat from the fbc-built fileio/print example ends lines CR LF). }
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FProgram) and FProgram.ModernMode then
          Data := LineEnding
        else
          Data := #13;  // Carriage return (C128 BASIC behavior)
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          FilePrintColSet(HandleNum, 0);
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
          FilePrintColAdvance(HandleNum, Data);
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
        Data := FConsoleBehavior.FormatInt(Ctx.IntRegs[Instr.Dest]);  // exact 64-bit (no Double rounding above 2^53)
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PRINT#', HandleNum, Data, ErrorCode);
          FilePrintColAdvance(HandleNum, Data);
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
        // Numeric fast path: the whole query answers in an Int64, with no string built, matched or
        // parsed anywhere. QVal is an Int64 local - unmanaged, so unlike the AnsiString locals of
        // this method it costs nothing to have. Falls back to the string protocol when the handler
        // does not implement it (or declines a particular query).
        if Assigned(FOnFileQuery) and FOnFileQuery(Self, Instr.Immediate, HandleNum, QVal, ErrorCode) then
        begin
          if Instr.Dest >= 0 then Ctx.IntRegs[Instr.Dest] := QVal;
        end
        else
        begin
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

    19: // bcPutBinInt - PUT #n: write the low Immediate bytes of an integer (Src1=handle, Src2=int value;
        //   Immediate = byte width from the variable's declared type: BYTE=1, SHORT=2, LONG=4, else 8)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinI := Ctx.IntRegs[Instr.Src2];
        BinWidth := Instr.Immediate;
        if (BinWidth < 1) or (BinWidth > 8) then BinWidth := 8;   // default: full 64-bit integer
        SetLength(Data, BinWidth); Move(BinI, Data[1], BinWidth);  // little-endian low bytes
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    20: // bcPutBinFloat - PUT #n: write a double (8 bytes) or, with Immediate = 4, a SINGLE
        //   (Src1=handle, Src2=float value). A "Dim As Single" is 4 bytes on file, like fbc.
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinF := Ctx.FloatRegs[Instr.Src2];
        if Instr.Immediate = 4 then
        begin
          BinS := BinF; SetLength(Data, 4); Move(BinS, Data[1], 4);
        end
        else
        begin
          SetLength(Data, 8); Move(BinF, Data[1], 8);
        end;
        if Assigned(FOnFileData) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    21: // bcGetBinInt - GET #n: read the variable's declared width into an integer (Dest=int value,
        //   Src1=handle; Immediate = byte width: BYTE=1, SHORT=2, LONG=4, else 8). Value is zero-extended;
        //   the destination variable's own width code applies sign-extension on later use if signed.
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinWidth := Instr.Immediate;
        if (BinWidth < 1) or (BinWidth > 8) then BinWidth := 8;
        if Assigned(FOnFileData) then
        begin
          Data := IntToStr(BinWidth); FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          BinI := 0;
          if Length(Data) >= BinWidth then Move(Data[1], BinI, BinWidth);   // little-endian, zero-extended
          if Instr.Dest >= 0 then Ctx.IntRegs[Instr.Dest] := BinI;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

    22: // bcGetBinFloat - GET #n: read a double, or a SINGLE with Immediate = 4 (Dest=float, Src1=handle)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          BinWidth := 8; if Instr.Immediate = 4 then BinWidth := 4;
          Data := IntToStr(BinWidth); FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
          BinF := 0;
          if Length(Data) >= BinWidth then
          begin
            if BinWidth = 4 then begin Move(Data[1], BinS, 4); BinF := BinS; end
            else Move(Data[1], BinF, 8);
          end;
          if Instr.Dest >= 0 then Ctx.FloatRegs[Instr.Dest] := BinF;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

    23: // bcPutBinStr - PUT #n: write the string's RAW bytes, no length prefix (fbc-verified).
        //   Src1=handle, Src2=string value, Immediate=field width (0 = the string's own length;
        //   > 0 = a fixed-length field, NUL-padded or cut — a UDT "String * n" member).
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        Data := Ctx.StringRegs[Instr.Src2];
        BinWidth := Instr.Immediate;
        if BinWidth > 0 then
        begin
          if Length(Data) > BinWidth then SetLength(Data, BinWidth);
          while Length(Data) < BinWidth do Data := Data + #0;
        end;
        if Assigned(FOnFileData) and (Length(Data) > 0) then
        begin
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end
        else if not Assigned(FOnFileData) then
          raise Exception.Create('PUT command not supported: no handler assigned');
      end;

    24: // bcGetBinStr - GET #n: read RAW bytes into a string (Dest=string value, Src1=handle).
        //   Immediate = field width; 0 means "as many bytes as the destination string currently
        //   holds", which is FreeBASIC's rule for a variable-length string (Len(s) bytes read).
        //   A fixed-width field (> 0) is cut at its first NUL, like a "String * n" member.
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        if Assigned(FOnFileData) then
        begin
          BinWidth := Instr.Immediate;
          if BinWidth <= 0 then
          begin
            if Instr.Dest >= 0 then BinWidth := Length(Ctx.StringRegs[Instr.Dest]) else BinWidth := 0;
          end;
          if BinWidth > 0 then
          begin
            Data := IntToStr(BinWidth);
            FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
            while Length(Data) < BinWidth do Data := Data + #0;   // short read at EOF: zero-fill
          end
          else Data := '';
          // A fixed-width field is "n characters + NUL terminator" on file (fbc's C layout for a
          // "String * n" member): keep the n characters, padding included — the destination is a
          // fixed-length buffer, which holds exactly n bytes.
          if (Instr.Immediate > 0) and (Length(Data) > 0) then
            SetLength(Data, Length(Data) - 1);
          if Instr.Dest >= 0 then Ctx.StringRegs[Instr.Dest] := Data;
        end
        else raise Exception.Create('GET command not supported: no handler assigned');
      end;

    28, 29: // bcPutBinMem / bcGetBinMem - counted transfer between the file and RAW memory
            //   ("Put #f, , *p, n"): Src1=handle, Src2=raw pointer reg, Immediate=byte-count REG.
            //   RawAddr validates the region and the whole span, so a bad count raises instead of
            //   walking off the heap.
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinI := Ctx.IntRegs[Instr.Src2];
        BinCount := Ctx.IntRegs[Instr.Immediate];
        if BinCount < 0 then BinCount := 0;
        if not Assigned(FOnFileData) then
          raise Exception.Create('GET/PUT command not supported: no handler assigned');
        if BinCount > 0 then
        begin
          if SubOp = 28 then
          begin
            SetLength(Data, BinCount);
            Move(RawAddr(BinI, PtrUInt(BinCount))^, Data[1], BinCount);
            FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
            if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
          end
          else
          begin
            Data := IntToStr(BinCount);
            FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
            while Length(Data) < BinCount do Data := Data + #0;   // short read at EOF: zero-fill
            Move(Data[1], RawAddr(BinI, PtrUInt(BinCount))^, BinCount);
          end;
        end;
      end;

    30, 31: // bcPutBinArray / bcGetBinArray - whole array ("Put #f, , a()"): every element at its
            //   DECLARED width, not the 8-byte VM slot. Src1=handle, Src2=array id (immediate),
            //   Immediate = width or (bank shl 8), bank 0 = int, 1 = float.
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinWidth := Instr.Immediate and $FF;
        BinBank := (Instr.Immediate shr 8) and $FF;
        if (BinWidth < 1) or (BinWidth > 8) then BinWidth := 8;
        if not Assigned(FOnFileData) then
          raise Exception.Create('GET/PUT command not supported: no handler assigned');
        if (Instr.Src2 < Length(FArrays)) then
        begin
          BinArr := @FArrays[Instr.Src2];
          BinCount := BinArr^.TotalSize;
          if BinCount < 0 then BinCount := 0;
          if BinCount > 0 then
          begin
            if SubOp = 30 then
            begin
              SetLength(Data, BinCount * BinWidth);
              for k := 0 to BinCount - 1 do
                if BinBank = 1 then
                begin
                  if BinWidth = 4 then
                  begin
                    BinS := BinArr^.FloatData[k]; Move(BinS, Data[k * 4 + 1], 4);
                  end
                  else
                  begin
                    BinF := BinArr^.FloatData[k]; Move(BinF, Data[k * 8 + 1], 8);
                  end;
                end
                else
                begin
                  BinI := BinArr^.IntData[k];
                  Move(BinI, Data[k * BinWidth + 1], BinWidth);   // little-endian low bytes
                end;
              FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
              if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
            end
            else
            begin
              Data := IntToStr(BinCount * BinWidth);
              FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
              while Length(Data) < BinCount * BinWidth do Data := Data + #0;   // short read at EOF
              for k := 0 to BinCount - 1 do
                if BinBank = 1 then
                begin
                  if BinWidth = 4 then
                  begin
                    Move(Data[k * 4 + 1], BinS, 4); BinArr^.FloatData[k] := BinS;
                  end
                  else
                  begin
                    Move(Data[k * 8 + 1], BinF, 8); BinArr^.FloatData[k] := BinF;
                  end;
                end
                else
                begin
                  BinI := 0;
                  Move(Data[k * BinWidth + 1], BinI, BinWidth);
                  // Sign-extend a narrow SIGNED element back to the 64-bit slot (Byte/Short/Long).
                  case BinWidth of
                    1: BinI := Int64(ShortInt(Byte(BinI)));
                    2: BinI := Int64(SmallInt(Word(BinI)));
                    4: BinI := Int64(LongInt(LongWord(BinI)));
                  end;
                  BinArr^.IntData[k] := BinI;
                end;
            end;
          end;
        end;
      end;

    32: // bcPutBinPad - write Immediate NUL bytes (UDT record image alignment padding)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinCount := Instr.Immediate;
        if (BinCount > 0) and Assigned(FOnFileData) then
        begin
          Data := StringOfChar(#0, BinCount);
          FOnFileData(Self, 'PUTBIN', HandleNum, Data, ErrorCode);
          if ErrorCode <> 0 then raise Exception.CreateFmt('PUT error %d to file %d', [ErrorCode, HandleNum]);
        end;
      end;

    33: // bcGetBinSkip - skip Immediate bytes (UDT record image alignment padding)
      begin
        HandleNum := Ctx.IntRegs[Instr.Src1];
        BinCount := Instr.Immediate;
        if (BinCount > 0) and Assigned(FOnFileData) then
        begin
          Data := IntToStr(BinCount);
          FOnFileData(Self, 'GETBIN', HandleNum, Data, ErrorCode);
        end;
      end;

    35: // bcDirSearch - DIR(spec, mask) starts a walk (Immediate 0), DIR() steps it (Immediate 1).
      begin
        if Instr.Immediate = 0 then
        begin
          if FDirOpen then begin FindClose(FDirRec); FDirOpen := False; end;   // a new search cancels the old one
          FDirMask := Integer(Ctx.IntRegs[Instr.Src2]);
          FDirOpen := FindFirst(Ctx.StringRegs[Instr.Src1], faAnyFile, FDirRec) = 0;
        end
        else if FDirOpen then
          if FindNext(FDirRec) <> 0 then begin FindClose(FDirRec); FDirOpen := False; end;
        // Filter here rather than through FindFirst's own mask, because FreeBASIC's rule is its own and
        // was read off the oracle: an entry is returned when every attribute bit it carries is one the
        // mask allows, with ARCHIVE allowed implicitly -- EXCEPT when the mask asks for directories, and
        // then archive is not implied and plain files drop out. That is what makes "Dir("*", fbDirectory)"
        // list directories ALONE while "fbDirectory Or fbArchive" lists both, and it fits all twelve
        // mask/entry combinations measured against fbc.
        while FDirOpen and ((FDirRec.Attr and not DirAllowedAttrs(FDirMask)) <> 0) do
          if FindNext(FDirRec) <> 0 then begin FindClose(FDirRec); FDirOpen := False; end;
        if FDirOpen then
        begin
          Ctx.StringRegs[Instr.Dest] := FDirRec.Name;
          FDirAttr := FDirRec.Attr;
        end
        else
        begin
          Ctx.StringRegs[Instr.Dest] := '';
          FDirAttr := 0;
        end;
      end;
    36: // bcDirAttr - the attributes of the entry bcDirSearch last returned (0 once the walk is over)
      Ctx.IntRegs[Instr.Dest] := FDirAttr;

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
    Ctx.LastErrorProc := '';   // ERFN travels with the rest of the error state
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

function TBytecodeVM.SqrtFloat(X: Double): Double;
begin
  // MODERN (FreeBASIC): Sqr of a negative is NaN, taken from a Math-unit constant (a plain assignment,
  // so it never trips the FP hardware trap FPC leaves unmasked). CLASSIC (Commodore v7): ?ILLEGAL
  // QUANTITY. Programs rely on "Sqr(-1)" being NaN, e.g. an is-square test over -1-padded data.
  if X < 0.0 then
  begin
    if Assigned(FProgram) and FProgram.ModernMode then
      Result := NaN
    else
      raise Exception.CreateFmt('Square root of negative number: %.17e', [X]);
  end
  else
    Result := Sqrt(X);
end;

function TBytecodeVM.LnFloat(X: Double): Double;
begin
  // MODERN (FreeBASIC): Log follows C log -- Log(0) = -Inf, Log(negative) = NaN, no trap. CLASSIC
  // (Commodore v7): ?ILLEGAL QUANTITY. IEEE results come from Math-unit constants (plain assignments,
  // no FP hardware trap). Shared by both run loops so bcMathLog cannot diverge (opt == no-opt).
  if X > 0.0 then
    Result := Ln(X)
  else if Assigned(FProgram) and FProgram.ModernMode then
  begin
    // Log of a negative yields a NaN with the sign bit CLEAR, which is how the C library reports it and
    // what fbc 1.10.1 prints ("1.#QNAN"). Sqr of a negative, and 0/0, yield the sign-SET "indefinite"
    // NaN instead ("-1.#IND"), which is FPC's NaN constant as it comes. The distinction is visible only
    // in the printed text, and it is the text FreeBASIC produces.
    if X = 0.0 then Result := NegInfinity else Result := QuietNaN;
  end
  else
    raise Exception.Create('LOG of non-positive number');
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

function TBytecodeVM.FsChdirCode(const Path: string): Integer;
begin
  if SetCurrentDir(Path) then Result := 0 else Result := -1;
end;

function TBytecodeVM.FsMkdirCode(const Path: string): Integer;
begin
  // fbc: creating an existing directory fails with -1 (CreateDir already covers that case).
  if CreateDir(Path) then Result := 0 else Result := -1;
end;

function TBytecodeVM.FsRmdirCode(const Path: string): Integer;
begin
  if RemoveDir(Path) then Result := 0 else Result := -1;
end;

function TBytecodeVM.FsKillCode(const FileName: string): Integer;
begin
  // fbc returns the C runtime's errno: 2 (ENOENT) for a missing file - the case programs
  // actually test for - and an access-class error otherwise (13 = EACCES).
  if not FileExists(FileName) then
    Result := 2
  else if SysUtils.DeleteFile(FileName) then
    Result := 0
  else
    Result := 13;
end;

function TBytecodeVM.FsCopyFileCode(const Src, Dest: string): Integer;
var
  SrcStream, DstStream: TFileStream;
begin
  // fbc: 0 on success, 1 on any error; an existing destination is always overwritten.
  Result := 1;
  if not FileExists(Src) then Exit;
  try
    SrcStream := TFileStream.Create(Src, fmOpenRead or fmShareDenyWrite);
    try
      DstStream := TFileStream.Create(Dest, fmCreate);
      try
        DstStream.CopyFrom(SrcStream, SrcStream.Size);
        Result := 0;
      finally
        DstStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  except
    Result := 1;
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

procedure AotCallProfReport;
var n: QWord;
begin
  if GCallProf <= 0 then Exit;
  n := GCPCalls;
  if n = 0 then Exit;
  WriteLn(ErrOutput, '');
  WriteLn(ErrOutput, '[CALLPROF] native calls through AotCallSub: ', n,
                     '   (RdTsc overhead calibrated at ', GCPTscOverhead, ' cycles/read)');
  WriteLn(ErrOutput, Format('[CALLPROF]   total   %8.1f cycles/call', [GCPTotal / n]));
  WriteLn(ErrOutput, Format('[CALLPROF]     push  %8.1f   (FramePush bank snapshot + call-stack push)', [GCPPush / n]));
  WriteLn(ErrOutput, Format('[CALLPROF]     callee%8.1f   (the compiled function: prologue, body, epilogue)', [GCPCallee / n]));
  WriteLn(ErrOutput, Format('[CALLPROF]     pop   %8.1f   (descriptor refresh + FramePop)', [GCPPop / n]));
  if GCallProf = 2 then
  begin
    WriteLn(ErrOutput, '[CALLPROF]   sub-phases (AOT_CALLPROF=2: six extra reads per call, so the');
    WriteLn(ErrOutput, '[CALLPROF]   absolutes above are inflated - read these as SHARES):');
    WriteLn(ErrOutput, Format('[CALLPROF]       pre       %8.1f   (VM/ctx loads, callee lookup, depth cap)', [GCPPre / n]));
    WriteLn(ErrOutput, Format('[CALLPROF]       FramePush %8.1f', [GCPFrameP / n]));
    WriteLn(ErrOutput, Format('[CALLPROF]       callstack %8.1f   (grow check + return-PC push)', [GCPStack / n]));
    WriteLn(ErrOutput, Format('[CALLPROF]       arrdesc-in%8.1f   (FArraysDirty + ArrDesc rebase)', [GCPArrIn / n]));
    WriteLn(ErrOutput, Format('[CALLPROF]       arrdesc-ou%8.1f   (same again, after the callee)', [GCPArrOut / n]));
    WriteLn(ErrOutput, Format('[CALLPROF]       FramePop  %8.1f   (return-PC pop + FramePop + bcReturnSub test)', [GCPFrameQ / n]));
  end;
  WriteLn(ErrOutput, Format('[CALLPROF]   bank elements copied per call: int=%.2f float=%.2f string=%.2f',
                            [GCPBankI / n, GCPBankF / n, GCPBankS / n]));
  WriteLn(ErrOutput, '[CALLPROF]   NB "total" excludes the CALLER-side flush/reload, which is');
  WriteLn(ErrOutput, '[CALLPROF]   emitted code around the call and is charged to the caller.');
end;

initialization
  if GetEnvironmentVariable('FRAMESAVE_NOSTR') = '1' then GFrameSaveNoStr := 1;
  if GetEnvironmentVariable('FRAMEBANK') = '0' then GFrameBankNarrow := 0;
  if GetEnvironmentVariable('FRAMERANGE') = '0' then GFrameRangeNarrow := 0;
  if GetEnvironmentVariable('FRAMELIVE') = '0' then GFrameLiveNarrow := 0;
  if GetEnvironmentVariable('FRAMEMARK') = '0' then GFrameMark := 0;
  if GetEnvironmentVariable('FRAMEBASE') = '0' then GFrameBase := 0;
  if GetEnvironmentVariable('FRAMEBASE_WIDE') = '1' then GFrameBaseWide := 1;
  if GetEnvironmentVariable('FRAMEBANK_SHAPE') = '0' then GFrameBankShape := 0;
  if GetEnvironmentVariable('FRAME_FAST') = '0' then GFrameFast := 0;
  if GetEnvironmentVariable('AOT_FASTCALL') = '0' then GAotFastCall := 0;
  if GetEnvironmentVariable('FRAMEBASE_DIAG') = '1' then GFrameBaseDiag := 1;
  // Confirm the AnsiString header layout once, before any append can take the capacity path.
  // STRCAP=0 forces the old SetLength-per-append behaviour, so the two can be timed on ONE binary.
  StrCapacityInit;
  if GetEnvironmentVariable('STRCAP') = '0' then GStrCapacity := False;

finalization
  AotCallProfReport;

end.
