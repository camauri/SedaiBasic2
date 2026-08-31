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
  Classes, SysUtils, Math, Variants, StrUtils, DateUtils, RegExpr, SedaiRegexEngine,
  SedaiBytecodeTypes, SedaiOutputInterface, SedaiSSATypes,
  SedaiConsoleBehavior, SedaiConsoleState, SedaiDebugger, SedaiExecutorErrors,
  SedaiMemoryMapper, SedaiSpriteTypes, SedaiExecutionContext, SedaiDrawQueue,
  SedaiGraphicsBackend, SedaiInputState, SedaiOpcodeTable, SedaiOpcodeBanks,
  SedaiJit, SedaiAot, SedaiCpuInfo, SedaiBigInt
  {$IFDEF ENABLE_PROFILER}, SedaiProfiler{$ENDIF}
  {$IFDEF WITH_SEDAI_AUDIO}, SedaiAudioTypes, SedaiAudioBackend, SedaiSIDEvo{$ENDIF}
  {$IFDEF WEB_MODE}, SedaiWebIO{$ENDIF};

type
  { Forward declaration }
  TBytecodeVM = class;

  { One buffer of the JIT/AOT array-descriptor table (see FJitArrDesc / FRetiredArrDesc). Named so a
    retired buffer can be held in a list, which is what keeps it alive under a running worker. }
  TInt64Array = array of Int64;

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
  { TArrayStorage moved to SedaiExecutionContext (21 Aug 2026: the array BIND SAVE-STACK is
    per-context, and the context has to see the type - exactly as TRecordStorage moved for the
    record heap). Aliased here so every existing use of the name still resolves. }
  TArrayStorage = SedaiExecutionContext.TArrayStorage;

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
    // Managed STRING cells: a "String Ptr" points at a 24-byte cell holding an INDEX into this, the
    // way FreeBASIC's String is a descriptor whose characters live elsewhere. Slot 0 is ''.
    FRawStrCells: array of string;
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
    // ⛔ MISURATO E RESPINTO (21 ago 2026). Il profilo diceva che AotCallSub spende il 3,8% in
    // GetInstructionCount + GetInstructionsPtr, due metodi di un'altra unita' chiamati A OGNI
    // RITORNO per decidere se il PC restituito e' un bcReturnSub. Metterli in due CAMPI della VM,
    // riempiti una volta per programma, sembrava gratis.
    // 📊 A/B alternato su un binario solo, macchina fredda, con fannkuch come controllo:
    //     binary-trees N=18   metodi 4050 -> campi 4128 ms   +1,9%
    //                         metodi 4023 -> campi 4214 ms   +4,7%
    //     fannkuch     N=11   2023 / 2026 / 2023             piatto, come deve essere
    // Le due coppie concordano: i campi sono PIU' LENTI. Spiegazione plausibile: i campi nuovi
    // cadevano in una zona fredda dell'oggetto, quindi ogni ritorno toccava una RIGA DI CACHE in
    // piu', mentre le due chiamate a metodo leggono campi gia' caldi. Il controllo non si muove,
    // quindi l'effetto e' reale e sta sul percorso di chiamata.
    // ⛔ Non ritentarlo senza misurare: se un giorno serve, va provato mettendo i campi ACCANTO a
    // quelli che il percorso di ritorno tocca gia', non in fondo all'oggetto.

    {$IFDEF HOT_C}
    FHotOp: array of Word;            // per PC: 1 + the C arm that runs it, 0 = not the C loop's
    FHotOpBase: array of Word;        // ...before the run-wide gate is folded in
    FHotOpEnabled: Boolean;
    {$ENDIF}
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
    // J15: the leaf-primitive table handed to CompileLoop. Filled once per BuildJitLoops by
    // SetAotPrimitives; a compiled loop reads the addresses out of it AT COMPILE TIME and bakes
    // them, so nothing here is consulted at run time and the per-context fields stay unset.
    FJitPrimCtx: TAotCtx;
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
    //
    // ⛔⛔ THREADS. This table is VM-GLOBAL and every worker reads it, so both the dirty flag and the
    // rebuild are shared mutable state and BOTH need FArrDescLock. Two workers entering the first
    // native call together each ran SetLength on this same dynamic array: two allocations, one
    // assignment winning and dropping the other's buffer, and the loser's context left pointing at
    // freed memory - so its array writes went nowhere and its increments vanished in silence.
    // Measured before the fix: of 600 runs of m59_sharedscalar.bas at 32-way parallelism, 73 had two
    // threads inside RebuildJitArrDesc at once, two printed a wrong total and two printed nothing.
    FJitArrDesc: array of Int64;
    // Buffers this table used to live in. A worker that is RUNNING native code holds the old pointer
    // in its own AotCtx and only refreshes it at ITS OWN call boundaries, so a rebuild on another
    // thread must not free what that worker is still reading. While any worker is live the previous
    // buffer is retired here instead of freed; the list is dropped when the last worker exits, so a
    // single-threaded program keeps the old behaviour exactly and pays nothing.
    FRetiredArrDesc: array of TInt64Array;
    FArrDescLock: TRTLCriticalSection;
    FArraysDirty: Boolean;
    // ⭐ L'ultimo puntatore che AcquireArrDesc ha pubblicato, scritto SOTTO IL LOCK. Serve alla
    // corsia veloce di EnsureArrDesc: se il contesto del chiamante ha gia' questo puntatore e
    // nessuno ha sporcato la tabella, non c'e' niente da riacquisire e la sezione critica si salta.
    FCurArrDesc: Pointer;
    // Bumped on every master rebuild. A per-context copy carries the generation it was built from,
    // which is how a context knows its own table has gone stale without comparing pointers it does
    // not own. Read outside the lock on the fast lane, exactly as FArraysDirty is.
    FArrDescGen: Int64;
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
    FGfxWinScreen: Boolean;                            // WINDOW SCREEN: y grows downward (no flip)
    FGfxWinX1, FGfxWinY1, FGfxWinX2, FGfxWinY2: Double;  // the logical bounds, kept so the coefficients
                                                         // can be REBUILT when the viewport changes
    // ⛔ TWO coefficient sets, because fbc HAS two. Drawing divides the viewport into W-1 steps (world
    // x2 lands on the LAST pixel); PMAP divides it into W (world x2 answers W, one past the last).
    // Measured on an 11x11 screen with WINDOW (0,0)-(10,10): PSET (3,7) lands at physical (3,3) while
    // PMAP(7,1) answers 2.3. Using one set for both moves every drawing by up to a pixel per unit.
    FGfxWinAx, FGfxWinBx, FGfxWinAy, FGfxWinBy: Double;       // DRAWING (PSET/LINE/CIRCLE/POINT)
    FGfxPMapAx, FGfxPMapBx, FGfxPMapAy, FGfxPMapBy: Double;   // PMAP only
    // FreeBASIC VIEW viewport: physical origin added to mapped coords (non-SCREEN form); clip is on the surface.
    FGfxViewOffsetX, FGfxViewOffsetY: Integer;
    // ...and the rectangle itself, because CLS clears the VIEWPORT, not the screen. The clip lives on
    // the surface and the backend has no way to hand it back, so the VM keeps its own copy.
    FGfxViewActive: Boolean;
    FGfxViewX1, FGfxViewY1, FGfxViewX2, FGfxViewY2: Integer;
    // True from SCREENRES/SCREEN until the program leaves graphics: a FreeBASIC truecolor screen is up,
    // so CLS means "clear the framebuffer POINT reads", not "clear the text device". The C128 GRAPHIC
    // modes keep their own path, which is why this is a flag and not a test on InGraphics.
    FGfxFBScreen: Boolean;
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
    // SB_FAKE_CLOCK=1: NOW/TIMER advance by a fixed 1 ms per reading instead of following the wall
    // clock, which is what makes a self-timing program's output comparable between two engines.
    FFakeClock: Boolean;
    FFakeClockTicks: Int64;
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
    // --- Private (proc-local) arrays: one storage PER EXECUTION CONTEXT -------------------------
    // ⛔ Defect fixed 21 Aug 2026. A DIM inside a SUB compiles to an immediate array id, i.e. ONE
    // slot for the whole program, so two threads running the same SUB wrote the same elements. Every
    // context now owns a BLOCK of physical slots, one per private array, appended after the static
    // id space; Ctx.ArrMap turns the logical id the bytecode names into the physical slot.
    // The blocks are reserved ONCE at load (never grown afterwards) because a UDT member array
    // appends to FArrays at RUNTIME and a concurrent SetLength would move the table under it.
    FArrPrivSlot: array of Integer;   // per LOGICAL array id: position inside a block, or -1 = shared
    FPrivArrCount: Integer;           // arrays per block (0 = the program has no private array at all)
    FPrivBlockBase: Integer;          // FArrays index of block 0
    FPrivBlockUsed: array of Boolean; // which blocks are handed out (guarded by FWorkerLock)
    FStaticArrCount: Integer;         // size of the compile-time id space (ArrMap covers exactly this)
    // The array BYREF bind save-stack moved to TExecutionContext (per-context since 21 Aug 2026).
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
    // ⛔ TWO CALLBACKS, NOT ONE, and the split is the whole point. The POLL runs on an instruction
    // counter and must stay cheap; PRESENT runs at a frame boundary and may be expensive. Sharing one
    // callback meant the instruction counter decided how often the screen was shown - 158 times per
    // frame on a compute-heavy program, at about 0.7 ms each.
    FPresentCallback: TEventPollCallback;
    // Present cadence for a windowed run (0 = off, which is every target except `sb --window`)
    FPresentCadenceMs: LongWord;
    FLastPresentTick: QWord;
    FFrameBoundarySeen: Boolean;   // the program repaints the whole screen: use that, not the clock
    // SCREENLOCK / SCREENUNLOCK depth. While positive the picture in the buffer is HALF DRAWN and must
    // not be shown: that is the whole definition of the statement, and it is what the clock-driven
    // cadence could never know. Counted rather than flagged so nested locks - a SUB that brackets its
    // own drawing inside a caller that already did - unwind correctly instead of presenting early.
    FScreenLockDepth: Integer;
    // ⛔ THE PROCESS EXIT CODE. A run that ABORTED must not report success: fbc's runtime answers 1
    // when an ASSERT fails, and "End n" / "System n" answer n. We had no channel at all for it - the
    // END parser said so in a comment ("we have no process exit-code channel, so it is parsed and
    // discarded") - so every failure looked like a success to whatever ran us, and every net that
    // read $? was blind by construction.
    FProgramExitCode: Integer;

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
    function PointOutsideView(PX, PY: Integer): Boolean;  // POINT answers -1 outside the VIEW / the surface
    function GfxViewW: Integer;              // the width WINDOW maps onto: the viewport's, or the screen's
    function GfxViewH: Integer;
    procedure RecomputeGfxWindow;            // rebuild the WINDOW coefficients against the current viewport
    function DrawSurface: Integer;           // FreeBASIC per-statement image draw target (else the work page)
    procedure SetupGfxScreen(W, H, NumPages: Integer);  // SCREENRES/SCREEN: resize + (re)build pages
    // Group-specific dispatch handlers
    procedure ExecuteStringOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteMathOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteArrayOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    // Dialect-aware bounds test for a flat element index. Returns True when in range. Out of bounds:
    // CLASSIC (Commodore ?BAD SUBSCRIPT) or an explicit --bounds-check raises; MODERN (FreeBASIC, which
    // does not bounds-check) returns False so the caller yields a default on read / skips the write.
    function ArrayBoundsFail(ArrayIdx, LinearIdx: Integer): Boolean;   // the raise path, out of line
    function ArrayBoundsOK(ArrayIdx, LinearIdx: Integer): Boolean; inline;
    procedure EraseArray(ArrayIdx: Integer; Deallocate: Boolean = False);      // B1.4: ERASE (deallocate = dynamic array)
    procedure RedimArray(ArrayIdx, NewUpper: Integer; Preserve: Boolean; HasNewLower: Boolean = False; NewLower: Integer = 0);  // B1.4: REDIM (1-D)
    procedure RedimArrayN(ArrayIdx: Integer; const Uppers: array of Integer; Preserve: Boolean; const Lowers: array of Integer); // REDIM multi-dim

    procedure AdvancePrintCol(Ctx: TExecutionContext; Chars: Integer);   // printed text advances the cursor -- and the cursor WRAPS at the right margin
    procedure ExecuteIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSpecialVarOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    function PresentNow: Boolean;
    procedure MaybePresentCadence(Ctx: TExecutionContext);   // windowed runs only; see the body
    procedure PresentBeforeFullRepaint(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteSoundOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    { Group 12: BigInt. A value is a handle into Ctx.BigVals. }
    procedure ExecuteBigIntOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    function BigAlloc(Ctx: TExecutionContext): Integer;      // a fresh handle, value 0
    function BigDecimal(Ctx: TExecutionContext; H: Integer): string;
    procedure BigSetDecimal(Ctx: TExecutionContext; H: Integer; const S: string);
    function BigDestOf(Ctx: TExecutionContext; Reg: Integer): Integer;
    function BigSignedCmp(Ctx: TExecutionContext; A, B: Integer): Int64;
    procedure BigSignedAdd(Ctx: TExecutionContext; H, A, B: Integer; NegB: Boolean);
    procedure ExecuteSpriteOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    procedure ExecuteFileIOOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$IFDEF WEB_MODE}
    procedure ExecuteWebOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
    {$ENDIF}
    // Build FDenseOps for the current program if it is not already current (VM perf plan M2).
    procedure EnsureDenseOps;
    {$IFDEF HOT_C}
    procedure SetHotOpEnabled(AEnabled: Boolean);
    {$ENDIF}
    // JIT (J2/J3): compile every eligible hot loop of the current program to native (called from
    // EnsureDenseOps when FJitEnabled). Loops with an unsupported opcode are left to the interpreter.
    procedure BuildJitLoops;
    // JIT (J3): refresh the array descriptor table from FArrays (base pointers + counts).
    procedure SetAotPrimitives(var C: TAotCtx);
    // --- private (proc-local) array plumbing; see FArrPrivSlot ---
    procedure BuildPrivateArrayPlan;                       // census the private ids, reserve every block
    procedure BindArrayMap(Ctx: TExecutionContext);         // hand Ctx a free block and build its ArrMap
    procedure ReleaseArrayMap(Ctx: TExecutionContext);      // give the block back (and clear its storage)
    function MapArrDyn(Ctx: TExecutionContext; Id: Int64): Integer;  // id carried in a register/pointer
    procedure CheckPrivDesc(Ctx: TExecutionContext; Desc: Pointer);  // ARRPRIV_DIAG: descriptor vs storage
    function ActiveCtx: TExecutionContext; inline;   // this thread's context (GActiveCtx, or the main one)
    procedure RebuildJitArrDesc;
    function AcquireArrDesc: Pointer;
    procedure EnsureArrDesc(Ctx: PAotCtx);
    function AcquireArrDescCtx(ECtx: TExecutionContext): Pointer;   // per-context table when arrays are private
    function AcquireArrDescFast(var Cached: Pointer; ECtx: TExecutionContext): Pointer;
    procedure ReleaseRetiredArrDesc;
    // Raise a dialect-aware filesystem runtime error: FreeBASIC error number + message in MODERN,
    // Commodore error number + '?...' message in CLASSIC. The code reaches ERR via the except handler.
    function ErrorText(Code: Integer): string;   // the dialect's own message for an error NUMBER
    procedure RaiseFileError(const FBMsg: string; FBCode: Integer; const CBMMsg: string; CBMCode: Integer);
    // FreeBASIC resets Err/Erl after RESUME / RESUME NEXT; Commodore keeps EL/ER. Reset only in MODERN.
    procedure ResetErrorStateIfModern(Ctx: TExecutionContext);
    // Dialect-aware float division by (near-)zero. FreeBASIC (MODERN) follows IEEE-754: x/0 -> +/-Inf,
    // 0/0 -> NaN. Commodore BASIC (CLASSIC) raises ?DIVISION BY ZERO ERROR. Given the numerator, returns
    // the IEEE result in MODERN or raises EZeroDivide in CLASSIC. Used at every float-div-by-zero site.
    function DivZeroFloat(Numerator, Denominator: Double): Double;
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
    function FramePushIsAllocFree(Ctx: TExecutionContext; TargetPC: Integer): Boolean;
    procedure FramePush(Ctx: TExecutionContext; TargetPC: Integer = -1; CallPC: Integer = -1);
    procedure ArrPrivRestoreSlow(Ctx: TExecutionContext; Base: Integer);
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
    function AllocRecord(Ctx: TExecutionContext; ByteSize, StrC, TypeId: Integer): Integer;  // M3: new record instance -> handle
    // M5.2c: allocate in the shared region (cross-thread); ResolveRec routes a handle to its record.
    procedure GrowSharedRecords(NeedLen: Integer);
    function AllocSharedRecord(ByteSize, StrC, TypeId: Integer): Int64;
    function AllocSharedRecordBlock(N, ByteSize, StrC, TypeId: Integer): Int64;
    function SharedRecordBlockLen(Handle: Int64): Int64;
    function ReallocSharedRecordBlock(OldHandle: Int64; NewN, ByteSize, StrC, TypeId: Integer): Int64;  // N consecutive shared records (Callocate block)
    procedure FreeSharedRecord(Handle: Int64);   // DELETE: release a shared record, recycle its slot
    // Resolve a tagged raw pointer to a real address in its region (byte heap or framebuffer), checking
    // that NeedBytes bytes fit. Every raw access goes through it.
    function PtrDomainLoadZStr(Ctx: TExecutionContext; PtrAddr: Int64; Wide: Boolean;
                               ExactBytes: Integer): AnsiString;
    procedure PtrDomainStoreZStr(Ctx: TExecutionContext; PtrAddr: Int64; const Value: AnsiString;
                                 Wide: Boolean);
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
    function RawStrCellGet(RawPtr: Int64): string;                             // managed String cell at a raw address
    procedure RawStrCellSet(RawPtr: Int64; const S: string);                   // ...and writing one
    procedure RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
    procedure RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
    function BlockAddr(Ctx: TExecutionContext; Ptr: Int64; NeedBytes: PtrUInt): Pointer;  // CLEAR/FB_MEMCOPY: the raw heap, the framebuffer, OR an array's element storage
    procedure RawMemCopy(Ctx: TExecutionContext; DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);  // FB_MEMCOPY/FB_MEMMOVE: copy ByteCount bytes
    procedure RawClear(Ctx: TExecutionContext; DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);  // CLEAR: set ByteCount bytes to Value
    function ResolveRec(Ctx: TExecutionContext; Handle: Int64): PRecordStorage; inline;
    function RecPtrTarget(Ctx: TExecutionContext; PtrAddr: Int64; out Slot: Integer): PRecordStorage; inline;  // decode @obj.field pointer
    // ⭐ The THREE POINTER DOMAINS, answered in one place each. A pointer VALUE is one of: a record-field
    // pointer (RECPTR_TAG, bit 63, so NEGATIVE), a raw heap address (RAWPTR_TAG, bit 62) or a packed
    // array pointer. The bcRefLoad*/bcRefStore* arms spelled all three out inline, and the bcRawLoad*/
    // bcRawStore* arms knew only the raw one - so "@obj.field" kept in a POINTER FIELD of another UDT
    // (which the compiler classifies as raw) died on a dereference that worked from a pointer VARIABLE.
    // ⛔ Extracted rather than copied: this would have been the THIRD written-out copy of the same
    // decode, and every earlier copy of it in this VM has cost a defect.
    function PtrDomainLoadInt(Ctx: TExecutionContext; PtrAddr: Int64): Int64;
    function PtrDomainLoadFloat(Ctx: TExecutionContext; PtrAddr: Int64): Double;
    procedure PtrDomainStoreInt(Ctx: TExecutionContext; PtrAddr, Value: Int64);
    procedure PtrDomainStoreFloat(Ctx: TExecutionContext; PtrAddr: Int64; Value: Double);
    procedure CleanupSharedRecords;   // free the shared region (destructor)
    procedure UpdateScreenModelGate;          // decide whether the modelled screen must be kept
    procedure RecCacheAdopt(C: PRecCache);    // bind this thread's free-index cache to this VM
    procedure RecCacheFlush(C: PRecCache);    // give a batch of free indices back to the region
    procedure RecCacheRefill(C: PRecCache);   // restock a dry cache from the region
    procedure RecordNewArrayInit(Ctx: TExecutionContext; ArrayId: Integer; PackedCounts: Int64);  // M3.1: fill UDT array
    procedure DeepCopyArrayRecords(Ctx: TExecutionContext; DestArr, SrcArr: Int64; PackedCounts: Int64);  // value-copy array-of-UDT member
    procedure CheckFloatValid(Ctx: TExecutionContext; RegIndex: Integer; const OpName: string);
    function FormatUsing(const FormatStr: string; Value: Double;
      IsInt: Boolean; IntValue: Int64): string;   // picks the dialect's rules
    function FormatUsingFB(const FormatStr: string; Value: Double;
      IsInt: Boolean; IntValue: Int64): string;   // MODERN: FreeBASIC's rules
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
    procedure AttachGraphicsToOutput;   // hand the graphics backend to a text device that can mirror
    // FreeBASIC graphics backend. OwnedObj (optional) is the concrete object the VM should free on
    // destruction (used for the software backend on sb; pass nil for the SDL2 device owned elsewhere).
    procedure SetGraphicsBackend(Backend: IGraphicsBackend; OwnedObj: TObject = nil);
    procedure UseSoftwareGraphics;  // attach a VM-owned headless software graphics backend (CLI / bare-metal)
    function  GraphicsBackend: IGraphicsBackend;  // the attached backend (nil if none) - for wiring at the call site
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
    // ⚠️ OUTSIDE the instruction-counting IFDEF on purpose: the exit code is not a statistic, it is the
    // program's ANSWER, and it must exist in every build. Put inside it once by accident, the field
    // vanished from the default build and the compiler said so at the one line that used it.
    property ProgramExitCode: Integer read FProgramExitCode write FProgramExitCode;
    {$IFDEF ENABLE_INSTRUCTION_COUNTING}
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
    property PresentCallback: TEventPollCallback read FPresentCallback write FPresentCallback;
    // Minimum milliseconds between presents driven from the graphics opcodes. 0 disables the
    // mechanism entirely, which is the default and what every target other than `sb --window`
    // leaves it at, so nothing else changes behaviour or pays more than one integer compare.
    property PresentCadenceMs: LongWord read FPresentCadenceMs write FPresentCadenceMs;
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

uses
  // Only for AttachGraphicsToOutput: the headless text device is the one that has to be TOLD about the
  // drawing surface, because unlike sbv's controller it is not the graphics backend itself. In the
  // implementation section so the interface of this unit stays free of it.
  SedaiTerminalIO;

{ Trigonometry comes from the platform C library, NOT from FPC's RTL.
  FPC lowers Sin/Cos/Tan onto the x87's fsin/fcos, whose argument reduction carries pi to 66 bits.
  Past about 1e6 that is not enough: measured 22 Aug 2026, Sin(1e15) came out 0.8582721324763734
  where the true value is 0.85827279317023583 - NINE significant digits lost. glibc reduces
  correctly, and fbc (our oracle) answers exactly what glibc answers, so we were alone and wrong.
  Atn, Log, Exp and Sqr were measured on the same day and do NOT diverge: they need no reduction
  modulo pi, and they stay on the RTL.
  ⚠️ All THREE entry points must use these - ExecuteMathOp, ComputeBuiltinFP and the C hot loop's
  arms in hotdisp.c - or the same program would answer differently depending on which engine ran
  it, which is the silent miscompilation this project has already paid for once.
  ⚠️ On Windows these bind to the CRT's sin/cos/tan, which is what MinGW's hotdisp.o also calls:
  the invariant that matters is that every engine on ONE platform agrees. Whether the CRT reduces
  as well as glibc has NOT been measured here. }
{$IFDEF UNIX}
function c_sin(x: Double): Double; cdecl; external 'm' name 'sin';
function c_cos(x: Double): Double; cdecl; external 'm' name 'cos';
function c_tan(x: Double): Double; cdecl; external 'm' name 'tan';
{$ELSE}
function c_sin(x: Double): Double; cdecl; external 'msvcrt' name 'sin';
function c_cos(x: Double): Double; cdecl; external 'msvcrt' name 'cos';
function c_tan(x: Double): Double; cdecl; external 'msvcrt' name 'tan';
{$ENDIF}

{$IFDEF HOT_C}
{ WINDOWS ONLY, and the gate is not tidiness. On win64 there is no libm to pull and FPC ships no
  msvcrt import library, so the C object cannot name "sin" itself - the link died on "Undefined
  symbol: sin". These forward to the same c_sin/c_cos/c_tan every other engine uses, so one platform
  still has one implementation.
  ⛔ NOT used on Unix, because the forwarding is NOT free: measured on a 3 M-iteration loop saturated
  with Sin/Cos/Tan, the C object calling libc directly runs 0.097 s and calling through here 0.114 s,
  +17.5%. (An earlier version of this comment called that "below the noise, and measured". It was
  neither until it was.) Where the alternative is not linking at all, the call is worth paying. }
{$IFDEF WINDOWS}
function sb_hot_sin(x: Double): Double; cdecl; public name 'sb_hot_sin';
begin Result := c_sin(x); end;
function sb_hot_cos(x: Double): Double; cdecl; public name 'sb_hot_cos';
begin Result := c_cos(x); end;
function sb_hot_tan(x: Double): Double; cdecl; public name 'sb_hot_tan';
begin Result := c_tan(x); end;
{$ENDIF}
{$ENDIF}


// Declared here because ExecuteSuperinstruction (bcStrConcatCharAt) calls it well before its
// definition further down, next to AppendString.
procedure AppendChar(var D: AnsiString; C: AnsiChar); forward;

var
  // -1 = not read yet, 0 = deterministic (default), 1 = follow the system locale.
  GDateLocale: Integer = -1;
  // JIT_OVERAOT=1 lets the loop JIT compile loops the AOT already owns (see BuildJitLoops). Default
  // off: the overlap costs a second compilation and buys nothing.
  GJitOverAot: Boolean = False;
  // AOT_ARRDESC=0 riporta EnsureArrDesc alla sezione critica INCONDIZIONATA (il comportamento del
  // 4a8b8ac). E' il riferimento dell'A/B su un binario solo: quel commit ha corretto tre difetti a
  // thread veri e ha messo un lock globale sul cammino di chiamata, che su binary-trees costava 5,6x.
  GArrDescFast: Boolean = True;
  GArrPrivDiag: Boolean = False;   // ARRPRIV_DIAG=1: trace the private-array mapping
  // AOT_EXCFRAME=1 rimette il frame di eccezione su OGNI chiamata (il comportamento fino al
  // 21 ago 2026): e' l'A/B su un binario solo per la modifica che lo salta quando nulla puo' allocare.
  GNoExcFrame: Boolean = True;
  // HOTC_DIAG=1 counts, per opcode, how many times the C hot loop HANDED THE PC BACK on it - that
  // is, how often each uncovered opcode SPLITS a hot run. It is the census that answers "which
  // opcode should get an arm next" with a measurement instead of a hand-written list, which is the
  // only way that question has ever been answered correctly here: covering the record loads and
  // stores on 21 Aug was worth 12.7% on binary-trees, and nobody had asked since spectral-norm.
  // The count is what matters, not the opcode's presence: an uncovered opcode that never lands in
  // a loop costs nothing at all.
  GHotCDiag: Boolean = False;
  GHotCReported: Boolean = False;
  GHotCCalls: Int64 = 0;    // how many times the C loop was entered
  GHotCBudgetExits: Int64 = 0;  // ...of which returned because the BACK-EDGE BUDGET ran out, which
                                // is NOT an uncovered opcode and must not be ranked as one
  GPairDiag: Boolean = False;       // PAIR_DIAG=1: census of the adjacent opcode pairs executed
  GPairDiagTop: Integer = 20;       // how many to print: 0 = all (PAIR_DIAG=all), else the number
  // ⛔ NOT the raw opcode as an index. A 16-bit opcode would need a 65536x65536 table, and masking
  // it down to 12 bits - which is what the first version of this did - both mislabels the entries
  // (0xC8xx superinstructions come out as group 8/9 "Web_nn") and COLLIDES two real opcodes into
  // one counter. PairSlot compacts group|sub into 0..2047 losslessly for the groups that exist.
  GPairCount: array[0..2047, 0..2047] of LongWord;
  GSuperDiag: Boolean = False;      // SUPER_DIAG=1: census of the NESTED superinstruction dispatch
  GSuperCount: array[0..255] of Int64;
  GHotCExit: array[0..65535] of Int64;

function PairSlot(Op: Word): Integer;
// group -> a small dense id, sub kept whole: slot = gid*256 + sub, so no two opcodes share a slot.
var gid: Integer;
begin
  case Op shr 8 of
    $00: gid := 0; $01: gid := 1; $02: gid := 2; $03: gid := 3;
    $04: gid := 4; $0A: gid := 5; $0B: gid := 6; $C8: gid := 7;
  else   gid := 7;   // anything unforeseen lands with the superinstructions rather than colliding
  end;                // with a core opcode - and the report prints the real name, so it is visible
  Result := gid * 256 + (Op and $FF);
end;

function SlotOpcode(Slot: Integer): Word;
const G: array[0..7] of Word = ($00, $01, $02, $03, $04, $0A, $0B, $C8);
begin
  Result := (G[Slot div 256] shl 8) or Word(Slot mod 256);
end;

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

function FloatToUIntConv(V: Double; Modern: Boolean): Int64;
// The implicit float -> int conversion when the DESTINATION is UNSIGNED 64-bit (UInteger/ULongInt).
// It is a genuinely different conversion, not a reinterpretation: an Int64 cannot hold [2^63, 2^64),
// so converting there and reading the bits back squashed every value in that range onto the single
// value the signed truncation returns when it cannot answer -- "Dim As ULongInt u = 1e19" gave
// 9223372036854775808 where fbc gives 10000000000000000000.
//
// MEASURED against fbc 1.10.1 (its constant folding and its runtime sequence agree):
//     d >= 2^63 :  UInt64(trunc(d - 2^63)) + 2^63      (the add wraps)
//     otherwise :  UInt64(trunc(d))
// with an out-of-range or NaN truncation giving x86's "integer indefinite", $8000000000000000.
// Every measured edge follows from that one rule: 2^64, 1e20, 1e30 and +inf give 0; -inf, a NaN and
// -1e30 give 2^63; -5 gives 18446744073709551611; and the rounding is the dialect's own, so 2.5
// still gives 2 and 3.5 gives 4.
// ⛔ Must stay bit-identical to the SSA generator's ConstFloatToUInt: a folded constant and a
// computed value pass through different code and must not answer differently.
const
  TWO63 = 9223372036854775808.0;
  INDEFINITE = Int64($8000000000000000);
var
  Q: QWord;
begin
  if V <> V then Exit(INDEFINITE);
  if V < -TWO63 then Exit(INDEFINITE);
  if V >= TWO63 then
  begin
    if V - TWO63 >= TWO63 then Q := QWord(INDEFINITE)
    else Q := QWord(FloatToIntConv(V - TWO63, Modern));
    {$PUSH}{$Q-}{$R-}
    // "Take 2^63 off, convert, put it back": the addition wraps THROUGH the sign, which is the whole
    // trick. Deliberate, and silenced here so a debug build stays usable.
    Result := Int64(Q + QWord(INDEFINITE));
    {$POP}
  end
  else
    Result := FloatToIntConv(V, Modern);
end;

const
  // Ceiling on simultaneously-live THREADCREATE workers. Sized far above any legitimate FreeBASIC
  // program on a desktop core count, and far below what it takes to wedge the host. It exists so that a
  // compiler defect (an @sub whose entry PC resolves wrong, a worker that re-enters the module body)
  // fails the program instead of spawning threads without bound.
  MAX_LIVE_WORKERS = 64;

{ ⛔ A TYPED constant, i.e. a variable, and that is the whole point: dividing by a
  literal lets the optimiser turn the division into a MULTIPLICATION BY THE
  RECIPROCAL, and 1/86400 is not exact, so the answer lands one ulp away from the
  IEEE quotient. See bcTimeSerial. }
const
  SECS_PER_DAY_D: Double = 86400.0;
  HOURS_PER_DAY_D: Double = 24.0;
  MINS_PER_DAY_D: Double = 1440.0;

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
  Last: Boolean;
begin
  Spawn := TWorkerSpawn(p);
  Last := False;
  GActiveCtx := Spawn.Ctx;
  GSelfHandle := Spawn.Handle;   // M5.5: THREADSELF inside this worker returns its own handle
  try
    try
      Spawn.VM.RunWorker(Spawn);
    except
      // ⛔ A worker must never propagate an exception past the RTL thread boundary - that aborts the
      // process - but it must not die in SILENCE either, and this used to do exactly that.
      //
      // What silence costs, measured: a worker faulting between MUTEXLOCK and MUTEXUNLOCK exits
      // without unlocking, so every other worker blocks on that mutex forever and the main thread
      // blocks in THREADWAIT. The program hangs, prints NOTHING, and there is no clue anywhere that
      // a thread ever failed. That is how the descriptor-table race above presented: two hung runs
      // in 600, with no output to explain either of them, and the swallow is what made a diagnosable
      // fault into an unexplainable hang.
      // ⭐ So the report is the fix here, not decoration. Full per-thread error reporting (routing
      // it to ON ERROR, failing the program) is still M5.5; this is the floor: the failure is named,
      // on stderr, with the thread that had it.
      on E: Exception do
      begin
        WriteLn(ErrOutput, Format('?thread %d died: %s: %s (mutexes it held stay locked)',
                                  [Spawn.Handle, E.ClassName, E.Message]));
        // ⭐ AND WHERE. A named failure without a location is still a guessing game: this file already
        // documents that swallowing a worker's exception turned a diagnosable fault into an
        // unexplainable hang, and "?thread 14 died: EAccessViolation" on its own is only half a step
        // better. Costs nothing - it runs once, on a thread that is already failing - and needs a
        // build with symbols (./build.sh sb --symbols) to name the frames.
        DumpExceptionBackTrace(ErrOutput);
      end;
      else
        WriteLn(ErrOutput, Format('?thread %d died: non-Exception (mutexes it held stay locked)',
                                  [Spawn.Handle]));
    end;
  finally
    // Give the private-array block back HERE and not when the context is freed. Contexts live until
    // the VM dies, so releasing at destruction would let a program that spawns workers one after
    // another run out of blocks after MAX_LIVE_WORKERS spawns even though only one is ever live.
    Spawn.VM.ReleaseArrayMap(Spawn.Ctx);
    // Release this worker's slot against MAX_LIVE_WORKERS even when its body raised.
    EnterCriticalSection(Spawn.VM.FWorkerLock);
    try
      Dec(Spawn.VM.FLiveWorkers);
      Last := Spawn.VM.FLiveWorkers = 0;
    finally
      LeaveCriticalSection(Spawn.VM.FWorkerLock);
    end;
    // With the last worker gone, nobody can still be holding a retired descriptor buffer: drop them.
    // Taken OUTSIDE FWorkerLock because it takes FArrDescLock, and nothing else nests those two.
    if Last then Spawn.VM.ReleaseRetiredArrDesc;
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
var
  PairDiagEnv: string;
{$IFDEF WITH_SEDAI_AUDIO}
  i: Integer;
{$ENDIF}
begin
  inherited Create;
  FEnvOverrides := TStringList.Create;
  FEnvOverrides.CaseSensitive := False;   // environment names are case-insensitive on Windows; harmless elsewhere
  // FreeBASIC draw colours: white foreground, opaque-black background (match the SCREENRES surface clear).
  FGfxForeColor := $FFFFFFFF;
  FGfxBackColor := $FF000000;   // opaque black, ARGB - what fbc reads back from an untouched screen
  // ⛔ ZERO, and it was 7 on an assumption. The comment claimed these were "fbc's console defaults,
  // which Color() reports before any COLOR statement" - measured, fbc reports 0 there. The example
  // that caught it reads Color() before issuing any COLOR at all, so it measures exactly this value.
  FConColorFg := 0;
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
  InitCriticalSection(FArrDescLock);
  // The deterministic clock (see bcDateNow). Read once per VM: a program cannot turn it on or off.
  FFakeClock := GetEnvironmentVariable('SB_FAKE_CLOCK') = '1';
  FFakeClockTicks := 0;
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
  GArrPrivDiag := GetEnvironmentVariable('ARRPRIV_DIAG') = '1';
  GHotCDiag := GetEnvironmentVariable('HOTC_DIAG') = '1';
  GSuperDiag := GetEnvironmentVariable('SUPER_DIAG') = '1';
  // PAIR_DIAG=1 (the old spelling, top 20) | PAIR_DIAG=all | PAIR_DIAG=<n>
  PairDiagEnv := LowerCase(Trim(GetEnvironmentVariable('PAIR_DIAG')));
  GPairDiag := (PairDiagEnv <> '') and (PairDiagEnv <> '0');
  if PairDiagEnv = 'all' then GPairDiagTop := 0
  else if (PairDiagEnv <> '') and (PairDiagEnv <> '1') then
  begin
    GPairDiagTop := StrToIntDef(PairDiagEnv, 20);
    if GPairDiagTop < 0 then GPairDiagTop := 20;
  end;
  GJitOverAot := GetEnvironmentVariable('JIT_OVERAOT') = '1';
  GArrDescFast := GetEnvironmentVariable('AOT_ARRDESC') <> '0';
  GNoExcFrame := GetEnvironmentVariable('AOT_EXCFRAME') <> '1';
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
  FCtx.RecordHigh := 0;   // the slots are GONE, so nothing below the mark is reused any more
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
  FPresentCadenceMs := 0;      // off unless a windowed front end asks for it
  FLastPresentTick := 0;
  FFrameBoundarySeen := False;
  FScreenLockDepth := 0;
  FSpriteEditorCallback := nil;
  FEventPollInterval := 10000;  // Poll every 10000 instructions by default
  FPresentCallback := nil;
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
  // ⛔⛔⛔ THE WORKERS GO FIRST, BEFORE ANYTHING THEY COULD STILL BE STANDING ON IS FREED.
  // A DETACHED worker is by definition still running when the program ends - THREADDETACH is the
  // statement that says "do not join me" - and this destructor used to unmap the executable pages
  // as its very first act, twenty lines before it joined anybody. A detached worker inside a
  // compiled function then executed memory that had just been unmapped and died of an access
  // violation at a high mmap address. Silently: the thread boundary swallowed the exception, so
  // m56_threadops.bas simply printed its (correct) output and nobody ever knew a thread had been
  // shot. 📊 Measured before the fix: 33 dead workers in 200 runs at 32-way parallelism.
  // ⭐ Order is the whole fix. Nothing above CleanupWorkers may free anything a worker can reach.
  CleanupWorkers;
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
  // M5.2: the workers were joined at the TOP of this destructor (see the note there); only their
  // lock is released here, once nothing can spawn or join any more.
  DoneCriticalSection(FWorkerLock);
  DoneCriticalSection(FArrDescLock);
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
  { ⛔ -1, NOT zero. An object's fields are born zeroed and 0 is a VALID handle: left that
    way, the first use of the scratch would write over somebody else's BigInt.
    A sentinel must live OUTSIDE the domain, not be its minimum. }
  FCtx.BigScratch := -1;
  FCtx.BigCount := 0;

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

function TBytecodeVM.FormatUsing(const FormatStr: string; Value: Double;
  IsInt: Boolean; IntValue: Int64): string;
{ ⭐ ONE PLACE decides which dialect's PRINT USING rules apply, so the two
  formatters cannot drift apart at some call site that forgot to ask. }
begin
  if Assigned(FProgram) and FProgram.ModernMode then
    Result := FormatUsingFB(FormatStr, Value, IsInt, IntValue)
  else
    Result := FormatUsingString(FormatStr, Value, IsInt, IntValue);
end;

function TBytecodeVM.FormatUsingFB(const FormatStr: string; Value: Double;
  IsInt: Boolean; IntValue: Int64): string;
{ PRINT USING for the MODERN dialect, i.e. FreeBASIC's rules - which are NOT the
  ones below. Three directives were simply ignored by the shared formatter and
  are implemented here, each measured against fbc rather than read off a manual:

    +  leading   the sign is ALWAYS printed and occupies a position ("+12.5",
                 " -4.5"); trailing, it follows the digits ("12.5+", "12.5-").
    -  trailing  '-' for a negative, a SPACE for a positive ("12.5 "), and the
                 digits are the absolute value.
    $$ floating  the '$' sits immediately before the first digit rather than at
                 the field's left edge, and a minus sign goes BEFORE it
                 (" -$12.50"). A single '$' stays fixed, which the old code
                 already had right.
    ^^^^ exponent ⭐ the rule that is not guessable: the mantissa carries
                 (number of '#' before the point) MINUS ONE significant integer
                 digits, because the first position belongs to the sign. So
                 "#.##^^^^" gives 0.12E+04, "##.##^^^^" gives 1.23E+03 and
                 "###.#^^^^" gives 12.3E+02 - all the same number. Four carets
                 print a two-digit exponent, five print three.

  ⛔ CLASSIC keeps the formatter below unchanged. Commodore v7 documents these
  same directives, so they are missing there too, but there is no C128 here to
  say what its exact output is and inventing it would be worse than recording
  the gap. }
var
  i, j, IntDigits, DecDigits, Caret, ExpDigits, Sh, CommaCount: Integer;
  LeadSign, TrailSign, TrailMinus, FixedDollar, FloatDollar, HasCommas: Boolean;
  HasDot: Boolean;              // a '.' in the field prints even with no decimals
  Neg, Overflow: Boolean;
  AbsValue, Mant: Double;
  Body, Digits, IntPart, DecPart, Grouped, ExpStr: string;
  Ex, Width: Integer;
begin
  IntDigits := 0; DecDigits := 0; Caret := 0; CommaCount := 0;
  LeadSign := False; TrailSign := False; TrailMinus := False;
  FixedDollar := False; FloatDollar := False; HasCommas := False;
  HasDot := False;

  i := 1;
  while i <= Length(FormatStr) do
  begin
    case FormatStr[i] of
      '#': Inc(IntDigits);
      ',': begin
             HasCommas := True;
             Inc(CommaCount);      // ⚠️ each ',' OCCUPIES a field position
           end;
      '$': if (i < Length(FormatStr)) and (FormatStr[i + 1] = '$') then
           begin
             FloatDollar := True;
             Inc(i);                       // "$$" is one directive, two chars
           end
           else
             FixedDollar := True;
      '+': if i = 1 then LeadSign := True else TrailSign := True;
      { ⚠️ A leading '-' never reaches here in MODERN: IsNumFieldStart refuses to
        open a field on it, so it is emitted as TEXT and the field this function
        receives begins at the '#'. The unconditional TrailMinus is therefore
        right - any '-' that gets here is a trailing one.
        ⛔ It briefly said "if i = 1 then LeadMinus" (a leading SIGN POSITION),
        which was the wrong reading of fbc: measured, "Using ""-##.#""; 1.5"
        gives "- 1.5", the '-' printed as text. See IsNumFieldStart. }
      '-': TrailMinus := True;
      '.': begin
             HasDot := True;
             j := i + 1;
             while (j <= Length(FormatStr)) and (FormatStr[j] = '#') do
             begin
               Inc(DecDigits);
               Inc(j);
             end;
             i := j - 1;
           end;
      '^': begin
             { ⚠️ FIVE carets is the ceiling: fbc prints a three-digit exponent
               for "^^^^^" and leaves any further caret as LITERAL text
               ("##.#^^^^^^" gives " 1.2E+003^"). Counting them all would have
               produced a four-digit exponent nobody asked for. }
             j := i;
             while (j <= Length(FormatStr)) and (FormatStr[j] = '^') and (Caret < 5) do
             begin
               Inc(Caret);
               Inc(j);
             end;
             { ⛔ Only skip what was actually CONSUMED. Once the cap is reached
               the loop consumes nothing, and "i := j - 1" would then move the
               cursor BACKWARDS - the enclosing Inc(i) puts it right back, and
               the parse spins for ever. A hang, on a caret nobody would think
               to test. }
             if j > i then i := j - 1;
           end;
    end;
    Inc(i);
  end;

  if IsInt then
  begin
    Neg := IntValue < 0;
    AbsValue := Abs(Double(IntValue));
  end
  else
  begin
    Neg := Value < 0;
    AbsValue := Abs(Value);
  end;

  { ---- exponential ---- }
  if Caret >= 4 then
  begin
    ExpDigits := Caret - 2;                 // ^^^^ -> 2, ^^^^^ -> 3
    { ⭐ The mantissa carries one FEWER significant integer digit than the field
      has '#', because the first position belongs to the sign - so "#.##^^^^"
      gives 0.12E+04 and "##.##^^^^" gives 1.23E+03 for the same number.
      ⚠️ Unless there is no decimal point at all: "#^^^^" prints 5E+00, i.e. the
      single position IS a digit. }
    { One position always belongs to the sign, so the mantissa carries
      IntDigits-1 significant integer digits. ⚠️ With NO decimal point there has
      to be at least one digit left: "#^^^^" prints 5E+00, while "###^^^^"
      prints 12E+02 - measured, both. }
    Sh := IntDigits - 1;
    if (DecDigits = 0) and not HasDot and (Sh < 1) then Sh := 1;
    if Sh < 0 then Sh := 0;
    Ex := 0;
    Mant := AbsValue;
    if Mant <> 0 then
    begin
      // bring the mantissa into [10^(Sh-1), 10^Sh) - or [0.1, 1) when Sh = 0
      while Mant >= Power(10, Sh) do begin Mant := Mant / 10; Inc(Ex); end;
      while Mant < Power(10, Sh - 1) do begin Mant := Mant * 10; Dec(Ex); end;
    end;
    Body := Format('%.*f', [DecDigits, Mant]);
    { ⚠️ Rounding can push the mantissa back OUT of its window - 9.99 asked for
      two decimals becomes 10.00, and with Sh = 0 it becomes 1.00 where a
      leading zero was required. Either way the answer is to shift once more. }
    j := Pos('.', Body);
    if j = 0 then j := Length(Body) + 1;
    if ((Sh = 0) and (Copy(Body, 1, 1) <> '0')) or ((Sh > 0) and (j - 1 > Sh)) then
    begin
      Mant := Mant / 10; Inc(Ex);
      Body := Format('%.*f', [DecDigits, Mant]);
    end;
    // left-pad the mantissa so its integer digits fill the field's positions
    j := Pos('.', Body);
    if j = 0 then j := Length(Body) + 1;
    while (j - 1) < Sh do
    begin
      Body := ' ' + Body;
      Inc(j);
    end;
    if Sh = 0 then
    begin
      // "0.dd" - and a minus sign REPLACES the leading zero, giving "-.dd"
      if Neg then
      begin
        if (Length(Body) > 0) and (Body[1] = '0') then Delete(Body, 1, 1);
        Body := '-' + Body;
      end;
    end
    else if Sh < IntDigits then
      // a position WAS held back for the sign: a space when there is none
      if Neg then Body := '-' + Body else Body := ' ' + Body
    else
    begin
      { ⚠️ Sh = IntDigits means every position is a DIGIT and none was held back
        - "#^^^^" prints 5E+00 with nothing in front. A negative value then does
        not fit at all, and fbc says so with the overflow marker: "%-5E+00".
        ⭐ The test is on the POSITIONS, not on whether there is a decimal
        point: "###^^^^" holds one back and prints " 12E+02". }
      if Neg then Body := '%-' + Body;
    end;
    if Ex < 0 then ExpStr := '-' else ExpStr := '+';
    ExpStr := 'E' + ExpStr + Format('%.*d', [ExpDigits, Abs(Ex)]);
    Result := Body + ExpStr;
    Exit;
  end;

  { ---- plain numeric field ---- }
  if IsInt then
  begin
    Digits := IntToStr(IntValue);
    if (Digits <> '') and (Digits[1] = '-') then Delete(Digits, 1, 1);
    if DecDigits > 0 then Digits := Digits + '.' + StringOfChar('0', DecDigits);
  end
  else
    Digits := Format('%.*f', [DecDigits, AbsValue]);

  j := Pos('.', Digits);
  if j > 0 then
  begin
    IntPart := Copy(Digits, 1, j - 1);
    DecPart := Copy(Digits, j + 1, MaxInt);
  end
  else
  begin
    IntPart := Digits;
    DecPart := '';
  end;

  { ⭐ THE FIELD'S CAPACITY in integer positions. Computed HERE, above the body,
    because the zero-drop right below is a decision that needs it. }
  Width := IntDigits;
  if FloatDollar then Inc(Width, 2);           // "$$" is TWO field positions
  { A LEADING '+' owns a position of its own and always prints something there,
    so it adds one to BOTH sides and never decides an overflow by itself. The
    sign that DOES compete is the one no position was asked for. }
  if LeadSign then Inc(Width);

  { ⭐ AN INTEGER PART OF EXACTLY "0" IS DROPPABLE - it yields its position to
    the sign rather than overflowing the field, and only when it has to.
    Measured against fbc 1.10.1, and it takes all four rows to pin the rule:
      "##.#" with -0.5  -> "-0.5"    two positions: sign AND zero fit, zero STAYS
      "#.#"  with -0.5  -> "-.5"     one position: the zero yields it
      "#.#"  with -1.5  -> "%-1.5"   the integer part is not "0": nothing to yield
      ".##"  with  0.5  -> ".50"     zero positions, but no sign wants one either
    ⛔ We used to print "%-0.5" for the second row - the marker was raised on a
    capacity the field could have met. It is why "#.#########" on -0.169075164
    came out "%-0.169075164", which in turn is what made n-body's mask look like
    an engine defect on 12 Aug (it was the mask; see IsNumFieldStart in
    SedaiSSA.pas). ⚠️ Keep the ORDER: drop first, THEN measure the overflow. }
  if IntPart = '0' then
  begin
    Sh := 1;
    if FloatDollar then Inc(Sh);
    if LeadSign or (Neg and not (TrailSign or TrailMinus)) then Inc(Sh);
    if Sh > Width then IntPart := '';
  end;

  if HasCommas then
  begin
    Grouped := '';
    j := 0;
    for i := Length(IntPart) downto 1 do
    begin
      if (j > 0) and (j mod 3 = 0) then Grouped := ',' + Grouped;
      Grouped := IntPart[i] + Grouped;
      Inc(j);
    end;
  end
  else
    Grouped := IntPart;

  { ⚠️ A '.' in the field prints even when NO '#' follows it: "#." on 0.5 gives
    "1." in fbc. The point is part of the picture, not a consequence of having
    decimals. }
  if DecDigits > 0 then Body := Grouped + '.' + DecPart
  else if HasDot then Body := Grouped + '.'
  else Body := Grouped;

  // the '$' of "$$" hugs the first digit, so it is part of the body
  if FloatDollar then Body := '$' + Body;

  // sign placement
  if LeadSign then
  begin
    if Neg then Body := '-' + Body else Body := '+' + Body;
  end
  else if Neg and not (TrailSign or TrailMinus) then
    Body := '-' + Body;

  { ⭐ THE OVERFLOW MARKER IS A CAPACITY TEST ON THE INTEGER POSITIONS, and the
    parts that COMPETE for them are the digits, the '$' of "$$", and a sign that
    is printed in FRONT. Measured, not assumed:
      "##.#" with -12.5   -> "%-12.5"   the minus takes a position, 3 > 2
      "##.#" with  -1.5   -> "-1.5"     1 digit + sign fits exactly
      "$$###.##" with 1234.5  -> fits   4 digits + '$' = 5 positions
      "$$###.##" with -1234.5 -> "%"    the sign makes it 6
    ⚠️ A TRAILING sign does not compete: it has its own position at the end.
    ⚠️ Width was computed ABOVE, before the droppable zero was resolved. }
  Sh := Length(IntPart);
  if FloatDollar then Inc(Sh);
  if LeadSign or (Neg and not (TrailSign or TrailMinus)) then Inc(Sh);
  Overflow := Sh > Width;

  { ⭐ THE FIRST SIGN DIRECTIVE WINS; A LATER ONE IS TEXT. It is the same rule as
    the leading '-' (see IsNumFieldStart): a sign character is a DIRECTIVE only
    where a sign is actually placed - everywhere else it prints as itself.
    Measured on fbc 1.10.1:
      "+#-" with  0.5 -> "+1-"   the '+' signs it, the '-' is the CHARACTER '-'
      "+#+" with -0.5 -> "-1+"   likewise, and note it is NOT flipped to '-'
      "#-"  with  0.5 -> "1 "    no leading sign, so the trailing '-' IS the
      "#-"  with -0.5 -> "1-"    directive: '-' for a negative, a BLANK for a
                                 positive. That pair is what proves the rule is
                                 about WHO PLACES THE SIGN, not about position. }
  if LeadSign then
  begin
    if TrailSign then Body := Body + '+'
    else if TrailMinus then Body := Body + '-';
  end
  else if TrailSign then
    if Neg then Body := Body + '-' else Body := Body + '+'
  else if TrailMinus then
    if Neg then Body := Body + '-' else Body := Body + ' ';

  // pad to the field width: the same integer positions counted above, plus the
  // fractional part and whatever owns a position of its own
  if DecDigits > 0 then Inc(Width, DecDigits + 1)
  else if HasDot then Inc(Width);
  if TrailSign or TrailMinus then Inc(Width);
  Inc(Width, CommaCount);
  { ⚠️ A fixed '$' OCCUPIES one of the field's positions rather than adding one,
    so the padding is computed WITHOUT it and the '$' is put in front after. }
  if Length(Body) < Width then
    Body := StringOfChar(' ', Width - Length(Body)) + Body;

  { A single '$' sits at the field's LEFT EDGE - ahead of the padding, not next
    to the digits: "$###.##" with 4.5 is "$  4.50". That is what makes it the
    fixed dollar, as against "$$" which hugs the number.
    ⚠️ And it goes ahead of the OVERFLOW marker too: fbc prints "$%1234.50", not
    "%$1234.50". Measured; there is no reasoning that would have produced it. }
  if Overflow then Body := '%' + Body;
  if FixedDollar then Body := '$' + Body;
  Result := Body;
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
    { ⛔ Must stay identical to the twin in TSSAGenerator.ProcessPrintUsing: a
      CONSTANT format never reaches this one, so the two engines only agree if
      kept in step. MODERN: a leading '-' is ORDINARY TEXT, not a field opener -
      in FreeBASIC '-' is a directive only in TRAILING position. See the long
      note at the twin for the fbc measurements and for why the opposite fix
      (a leading sign position) was wrong. }
    if Assigned(FProgram) and FProgram.ModernMode and (FormatStr[P] = '-') then Exit(False);
    { A '.' opens a field when a '#' follows in the same run - ".##" is ONE field.
      See the twin for the fbc measurements. }
    if FormatStr[P] in ['$', '+', '-', '.'] then
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
          Result := Result + FormatUsing(FieldStr, dv, False, 0);
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
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
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
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
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
    bcArrayLoadInt, bcArrayLBound, bcArrayUBound,
    // ⭐ THE FUSED LOOP COUNTER writes its counter into Dest and nothing else in the bank.
    // Auditing this family is not a micro-narrowing: an UNAUDITED opcode disqualifies its whole
    // procedure from call-site liveness (see BuildCallSiteLiveness), and every superinstruction was
    // unaudited - so a single fused branch inside a recursive SUB pushed every call in it back to
    // the callee-footprint snapshot. Measured on binary-trees: the superinstruction pass and the
    // frame narrowing were cancelling each other out, 3.7 s -> 7.6 s, TWICE as slow with strictly
    // FEWER instructions to execute. The fusion was never the cost; losing the narrowing was.
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt:
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
    bcPrintBool, bcPrintUInt,
    // The fused compare-and-branch family consumes its operands and stores nothing: the truth value
    // that used to occupy a register is exactly what the fusion removes. The float forms read the
    // float bank, so they write no integer register either.
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // The string and unsigned forms likewise consume their operands and store nothing.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
    // Thread primitives: each takes a HANDLE out of the integer bank and returns nothing to it.
    // Verified against RunTemplate.inc - "LockMutex(Ctx.IntRegs[Instr^.Src1])" and its siblings.
    // Leaving them unaudited disqualified every procedure that touches a mutex, which is both
    // multi-threaded benchmarks: binary-trees' WORKER and spectral-norm's whole worker unit.
    bcMutexLock, bcMutexUnlock, bcMutexDestroy,
    bcCondWait, bcCondSignal, bcCondBroadcast, bcCondDestroy,
    // INT(x): "FloatRegs[Dest] := FloorDouble(FloatRegs[Src1])" - float in, float out, nothing of
    // ours on either side. Unaudited, its float Src1 counted as an INTEGER read of that register
    // number, and one such read anywhere in the program refused relocation to a procedure that
    // merely wrote the same number in the integer bank. Measured on a recursive fib whose only
    // Int() was in a PRINT executed once: 137 ms against 86 with it gone.
    bcMathInt:
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
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
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
    bcArrayBindInd, bcArrayErase,
    // ⭐ THE FUSED BRANCH FAMILY WRITES NO REGISTER AT ALL, in any bank - it consumes a comparison
    // and moves the PC - and the loop-counter forms write only the integer counter. Leaving them
    // unaudited is what made the superinstruction pass LOSE on call-heavy programs: BW_UNKNOWN
    // credits Dest, Src1 and Src2 to this bank, so one fused branch in a recursive SUB widened its
    // STRING bank, and the string bank is refcounted. binary-trees paid it once per call.
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqZeroInt, bcBranchNeZeroInt,
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    // The float compare-and-branch READS two floats and writes none of them.
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // ⚠️ The STRING compare-and-branch READS two string registers and writes NEITHER - a branch
    // stores nothing. BW is a WRITE-set question, so BW_NONE is right in both banks; saying
    // otherwise here would widen every frame that contains one.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt:
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
    bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
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
    bcArrayBindInd, bcArrayErase,
    // The fused branch family again - and THIS is the bank where leaving it unaudited was expensive,
    // because every entry here is a refcounted assignment. See the note in BcFloatWriteShape.
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqZeroInt, bcBranchNeZeroInt,
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // ⚠️ The STRING compare-and-branch READS two string registers and writes NEITHER - a branch
    // stores nothing. BW is a WRITE-set question, so BW_NONE is right in both banks; saying
    // otherwise here would widen every frame that contains one.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt:
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

function BcIsFusedCondBranch(Op: Word): Boolean;
// The superinstruction branch family: every one of them carries its target in Immediate and either
// takes it or falls through, so each has TWO successors exactly like bcJumpIfZero.
//
// ⛔ This list and the two shape tables above must move together. Auditing an opcode's operands
// while leaving its control flow unknown is worse than not auditing it at all: the procedure becomes
// ELIGIBLE, and the backward liveness then treats a branch as pure fall-through - so a register live
// only on the taken edge is called dead and gets dropped from the frame snapshot. That is a silent
// miscompile, not a missed narrowing.
begin
  case Op of
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcBranchEqZeroInt, bcBranchNeZeroInt, bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt,
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt:
      Result := True;
  else
    Result := False;
  end;
end;

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
    bcPrintComma, bcPrintSemicolon, bcPrintNewLine, bcPrintEnd,
    // A FLOAT compare-and-branch reads two floats and branches: nothing of ours is read.
    bcBranchEqFloat, bcBranchNeFloat, bcBranchLtFloat, bcBranchGtFloat, bcBranchLeFloat, bcBranchGeFloat,
    bcBranchEqZeroFloat, bcBranchNeZeroFloat,
    // A STRING compare-and-branch reads two strings: nothing of ours.
    bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString, bcBranchLeString, bcBranchGeString,
    // INT(x) is float in, float out: "FloatRegs[Dest] := FloorDouble(FloatRegs[Src1])".
    bcMathInt:
      Result := US_NONE;
    bcCopyInt, bcNegInt, bcBitwiseNot, bcXferStoreInt, bcJumpIfZero, bcJumpIfNotZero,
    bcIntToFloat, bcIntToString, bcNarrowInt,
    // Src1 is the record HANDLE, which lives in the integer bank whatever the field's type is;
    // the slot number is an immediate. Verified one by one against ResolveRec in RunTemplate.inc.
    bcRecordLoadInt, bcRecordLoadFloat, bcRecordLoadString, bcRecordTypeId,
    bcRecordStoreFloat, bcRecordStoreString, bcRecordFree, bcRecordNewBlock,
    // ...and PRINT of an integer value, or a TAB/SPC count, reads it from Src1.
    bcPrintInt, bcPrintIntLn, bcPrintBool, bcPrintUInt, bcPrintTab, bcPrintSpc,
    // The counting bit intrinsics take one operand; the WIDTH is an immediate, not a register.
    bcBitClz, bcBitCtz, bcBitPopcnt,
    // "if r[Src1] <> 0 goto target": one integer operand, the target is an immediate.
    bcBranchEqZeroInt, bcBranchNeZeroInt,
    // Thread primitives taking one HANDLE from the integer bank and writing nothing back to it.
    bcMutexLock, bcMutexUnlock, bcMutexDestroy,
    bcCondSignal, bcCondBroadcast, bcCondDestroy:
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
    bcBitRotl, bcBitRotr,   // Src1 = value, Src2 = rotate count (the width is an immediate)
    bcRecordStoreInt,   // Src1 = handle, Src2 = the integer value being stored
    // The fused compare-and-branch reads the two operands the CmpInt used to read.
    bcBranchEqInt, bcBranchNeInt, bcBranchLtInt, bcBranchGtInt, bcBranchLeInt, bcBranchGeInt,
    // Unsigned reads the same two INT registers; only the comparison differs.
    bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
    // "CondWaitOp(Ctx.IntRegs[Instr^.Src1], Ctx.IntRegs[Instr^.Src2])": the condition and the mutex.
    bcCondWait:
      Result := US_SRC1 or US_SRC2;
    // The fused loop counter reads all three: the counter in Dest (which it also writes), the step
    // in Src1 and the limit in Src2. Dropping US_DEST here would be the silent miscompile this
    // table's header warns about - the counter would look dead across a call.
    bcAddIntToBranchLe, bcAddIntToBranchLt, bcSubIntToBranchGe, bcSubIntToBranchGt:
      Result := US_SRC1 or US_SRC2 or US_DEST;
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
      // ⛔ "copies float/string" NAMES TWO BANKS AND TELLS YOU NEITHER. Which one disqualifies a
      // procedure is the whole question - the float bank could follow the relocation scheme, the
      // string bank cannot - and reconstructing it by grepping a listing for float-looking opcode
      // names is a list by omission that gets it wrong. Print the widths the decision actually read.
      if GFrameBaseDiag = 1 then
        if (UStart[p] < Length(FProcWidths)) and (FProcWidths[UStart[p]].WInt >= 0) then
          WriteLn(ErrOutput, Format('[FRAMEBASE] unit %d @pc %d..%d: relocatable but NOT fast'
                                    + ' - keeping the copying frame (wFloat=%d wStr=%d)',
                                    [p, UStart[p], UEnd[p],
                                     FProcWidths[UStart[p]].WFloat shr 32,
                                     FProcWidths[UStart[p]].WStr shr 32]))
        else
          WriteLn(ErrOutput, Format('[FRAMEBASE] unit %d @pc %d..%d: relocatable but NOT fast'
                                    + ' - keeping the copying frame (widths NOT MEASURED for it)',
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
      if (Op = Ord(bcJump)) or (Op = Ord(bcJumpIfZero)) or (Op = Ord(bcJumpIfNotZero)) or
         BcIsFusedCondBranch(Op) then
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
        else if (Op = Ord(bcJumpIfZero)) or (Op = Ord(bcJumpIfNotZero)) or
                BcIsFusedCondBranch(Op) then
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

function TBytecodeVM.FramePushIsAllocFree(Ctx: TExecutionContext; TargetPC: Integer): Boolean;
// Vero quando FramePush(Ctx, TargetPC, _) seguito da GrowCallStackIfNeeded NON puo' allocare - e
// quindi non puo' sollevare, e quindi il chiamante non ha bisogno di un frame di eccezione.
//
// ⛔ PERCHE' ESISTE. AotCallSub avvolge FramePush + GrowCallStackIfNeeded in un try...except per
// catturare l'allocazione che fallisce. Quel try costa un setjmp e la manipolazione della catena
// delle eccezioni A OGNI CHIAMATA, mentre la crescita che protegge avviene forse una volta ogni
// diecimila. Campionato il 20 ago 2026 su binary-trees-modern-arena sotto --aot, contando i soli
// thread attivi: fpc_pushexceptaddr + fpc_popaddrstack + fpc_setjmp = 11,3% del tempo.
//
// ⛔ QUESTA FUNZIONE RIPRODUCE LE CONDIZIONI DEL RAMO VELOCE DI FramePush, ED E' QUI ACCANTO PER
// QUESTO: se quel ramo cambia, questa cambia con lui. Sono due copie della stessa condizione e
// l'unica difesa e' che siano ADIACENTI e dichiarate tali. Quando il ramo veloce non si applica si
// risponde False e il chiamante tiene il suo try: la risposta prudente e' sempre False.
var
  PW: Int64;
  FBHi: Integer;
begin
  Result := False;
  if (TargetPC < 0) or (TargetPC >= Length(FFrameFast)) then Exit;
  PW := FFrameFast[TargetPC];
  if PW < 0 then Exit;                                     // non e' uno scorrimento di puntatore
  FBHi := PW shr 32;
  if Ctx.RegHwI + FBHi > Ctx.RegFrameCap then Exit;        // regione piena: si copia, e si alloca
  if Ctx.FrameMarkTop >= Length(Ctx.FrameMarks) then Exit; // i marcatori vanno cresciuti
  if Ctx.CallStackPtr >= Length(Ctx.CallStack) then Exit;  // e la pila dei ritorni idem
  Result := True;
end;

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
        begin
          SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
          SetLength(Ctx.FrameMarkArrSave, Ctx.FrameMarkTop + 256);   // cresce IN PASSO: nessun controllo in piu'
        end;
        with Ctx.FrameMarks[Ctx.FrameMarkTop] do
        begin
          SaveDeltaI := Ctx.RegDeltaI;
          SaveHwI := Ctx.RegHwI;
          WInt := -1;                           // nothing copied, and nothing to read back
          RecBase := Ctx.RecordCount;
          BlockMark := Ctx.BlockRecMarkTop;
        end;
        Ctx.FrameMarkArrSave[Ctx.FrameMarkTop] := Ctx.ArrPrivSaveTop;
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
        begin
          SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
          SetLength(Ctx.FrameMarkArrSave, Ctx.FrameMarkTop + 256);   // cresce IN PASSO: nessun controllo in piu'
        end;
        with Ctx.FrameMarks[Ctx.FrameMarkTop] do
        begin
          SaveDeltaI := SaveDelta;
          SaveHwI := SaveHw;
          WInt := 0; WFloat := 0; WStr := 0;      // nothing was copied, so nothing is restored
          RecBase := Ctx.RecordCount;
          BlockMark := Ctx.BlockRecMarkTop;
        end;
        Ctx.FrameMarkArrSave[Ctx.FrameMarkTop] := Ctx.ArrPrivSaveTop;
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
    begin
      SetLength(Ctx.FrameMarks, Ctx.FrameMarkTop + 256);
      SetLength(Ctx.FrameMarkArrSave, Ctx.FrameMarkTop + 256);   // cresce IN PASSO: nessun controllo in piu'
    end;
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
    Ctx.FrameMarkArrSave[Ctx.FrameMarkTop] := Ctx.ArrPrivSaveTop;
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
    if Ctx.FrameRecBaseTop >= Length(Ctx.FrameArrSaveBase) then
      SetLength(Ctx.FrameArrSaveBase, Ctx.FrameRecBaseTop + 256);
    Ctx.FrameRecBase[Ctx.FrameRecBaseTop] := Ctx.RecordCount;
    Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop] := Ctx.BlockRecMarkTop;
    Ctx.FrameArrSaveBase[Ctx.FrameRecBaseTop] := Ctx.ArrPrivSaveTop;
    Inc(Ctx.FrameRecBaseTop);
  end;
end;

procedure TBytecodeVM.ArrPrivRestoreSlow(Ctx: TExecutionContext; Base: Integer);
// The COLD half. It is a separate routine for one reason: it needs a TArrayStorage temporary to
// clear the stack slot, and TArrayStorage is a MANAGED record - so FPC emits fpc_initialize and
// fpc_finalize in this routine's prologue and epilogue, with RTTI, on EVERY call.
// ⛔ Measured 22 Aug 2026: with the loop and the guard in ONE routine called from FramePop, a
// program that never DIMs a private array still paid it once per RETURN. binary-trees went 2980 ->
// 4460 ms, +50%, and `perf` named it in one run - fpc_initialize 9.4%, fpc_finalize 8.2%,
// RECORDRTTI 4.6%, dynarray_clear 4.2%, together 26.5% of a benchmark that has no private array at
// all. Two guesses at the cause (the frame record growing, the extra store) were both wrong.
var
  i: Integer;
begin
  if GArrPrivDiag then
    WriteLn(ErrOutput, Format('[arrpriv] RIPRISTINA da %d a %d', [Ctx.ArrPrivSaveTop, Base]));
  for i := Ctx.ArrPrivSaveTop - 1 downto Base do
  begin
    FArrays[Ctx.ArrPrivSave[i].SlotId] := Ctx.ArrPrivSave[i].Saved;
    Ctx.ArrPrivSave[i].Saved := Default(TArrayStorage);   // drop this stack slot's references
  end;
  Ctx.ArrPrivSaveTop := Base;
  FArraysDirty := True;
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
    if Ctx.ArrPrivSaveTop > Ctx.FrameMarkArrSave[Ctx.FrameMarkTop] then
      ArrPrivRestoreSlow(Ctx, Ctx.FrameMarkArrSave[Ctx.FrameMarkTop]);
  end
  else if (GFrameMark = 0) and (Ctx.FrameRecBaseTop > 0) then
  begin
    Dec(Ctx.FrameRecBaseTop);
    if Ctx.FrameRecBase[Ctx.FrameRecBaseTop] < Ctx.RecordCount then
      Ctx.RecordCount := Ctx.FrameRecBase[Ctx.FrameRecBaseTop];
    Ctx.BlockRecMarkTop := Ctx.FrameBlockMarkTop[Ctx.FrameRecBaseTop];
    if (Ctx.FrameRecBaseTop < Length(Ctx.FrameArrSaveBase))
       and (Ctx.ArrPrivSaveTop > Ctx.FrameArrSaveBase[Ctx.FrameRecBaseTop]) then
      ArrPrivRestoreSlow(Ctx, Ctx.FrameArrSaveBase[Ctx.FrameRecBaseTop]);
  end;
end;

{ A3-i: reading and writing a numeric field of a record's byte image.

  A field arrives packed in the instruction's Immediate, the way SedaiSSA.ComputeUDTLiveLayout
  stamped it into TUDTField.Slot:

      bits 4..31 : the field's byte offset in the record image - fbc's offset
      bits 0..3  : the B1.5 width code (0 = full width, 1=s8 2=u8 3=s16 4=u16 5=s32 6=u32 7=single)

  so nothing here needs the type tables: the width travelled with the instruction. The full-width
  case is tested first and separately, because it is the overwhelming majority of fields and this
  is a two-digit-nanosecond operation where a jump table would show.

  ⭐ A narrow STORE is one instruction of the right width, not a read-modify-write. That answers the
  regression the plan predicted for narrow records ("a UByte needs load/mask/store"): it needs
  nothing of the sort, because the layout never packs two fields into the same byte. The one overlap
  that exists is a UNION, and there overwriting the neighbour IS the semantics being asked for. }

{ FLOOR in the DOUBLE domain, which is what INT() means and what fbc computes.

  🐛 This replaces Math.Floor, whose result type is an INTEGER: on this target that overflowed for
  every |x| >= 2^31, so INT(1e15 + 0.5) answered -1530494976 instead of 1000000000000000 - silent
  garbage, in a primitive that graphics and simulation code calls per pixel. It also lost the sign of
  a negative zero, where fbc prints -0.
  Found by NATIVISING the opcode: the AOT emits one roundsd and its answer disagreed with the
  interpreter's, so the differential had to be settled against fbc - and the interpreter was the one
  that was wrong. ⭐ Making a path faster made an old defect visible, which is the argument for
  keeping two implementations that must agree.

  System.Int truncates toward zero and STAYS a Double, so it is exact at any magnitude; one compare
  turns truncation into floor, and -0.0 falls through untouched because -0.0 > -0.0 is false. }
// FIX() is truncation TOWARD ZERO, the sibling of INT(). System.Int is exactly that and stays a
// Double, so it is exact at any magnitude - unlike Trunc, whose result type is an Int64 and which is
// the same shape of dependency that made INT() wrong past 2^31.
//
// ⚠️ THE SIGN RULE IS NOT THE SAME AS INT'S, and this is fbc's behaviour rather than a tidy one:
//     Int(-0.0)  -> -0        Fix(-0.0)  ->  0
//     Int(-0.5)  -> -1        Fix(-0.5)  -> -0
// so FIX keeps the sign when it TRUNCATED something away, and not when the input was already zero.
// Verified against fbc 1.10.1 line by line; the asymmetry is respected rather than smoothed over,
// because the oracle is the specification here and a tidier rule would be a different language.
function FixDouble(const X: Double): Double; inline;
begin
  Result := System.Int(X);
  // ⚠️ NEGATIVE ZERO, and this is a DELIBERATE divergence from fbc (user decision, 10 Aug 2026).
  // IEEE 754-2019 §5.9 roundToIntegralTowardZero gives the result the sign of the OPERAND, so
  // Fix(-0.0) is -0.0. fbc answers +0 there while answering -0 for Fix(-0.5), and -0 for
  // Int(-0.0) - i.e. it is inconsistent with itself, and we followed it. The guard used to carry
  // an extra `(X <> 0)` that reproduced exactly that hole; removing it makes this the same rule
  // FloorDouble already applies, and makes the whole family agree.
  // ⭐ It also makes the native lowering ONE instruction: roundsd toward zero already preserves
  // the sign of a zero, so the AOT and the JIT need no fix-up at all. Same class of decision as
  // the correctly-rounded PRINT of a float: conform to the standard, declare the divergence.
  //
  // ⛔⛔ It FORCES the sign bit, it does NOT negate - and the difference has been paid for.
  // Negating assumes System.Int has LOST the sign, which is true on Windows and FALSE on Linux,
  // where System.Int(-0.0) already returns -0. There the "cure" made -(-0) = +0, i.e. introduced
  // the very defect it was meant to remove: interpreter and JIT returned +0 while the AOT
  // returned -0, and that is the thing a two-implementation design cannot afford. Measured
  // 13 Aug 2026 with the regression net (bug_int_floor). Forcing the bit is IDEMPOTENT, and so
  // right on both platforms.
  if (Result = 0) and (PInt64(@X)^ < 0) then
    PInt64(@Result)^ := PInt64(@Result)^ or Int64($8000000000000000);
end;

function FracDouble(const X: Double): Double; inline;
// FRAC(x) - the fractional part. ⚠️ A ZERO OPERAND KEEPS ITS OWN SIGN; a zero RESULT
// from a non-zero operand does not. Measured against fbc 1.10.1 on Linux:
//   Frac(-0.0)  -> -0        Frac(-5.0)  ->  0        Frac(-3.75) -> -0.75
// ⛔ The two zero rows look contradictory and are not: fbc computes x - Fix(x) with
// ITS Fix, where Fix(-0.0) is +0, so -0.0 - (+0.0) = -0.0 while -5.0 - (-5.0) = +0.0.
// We cannot borrow that arithmetic, because OUR Fix(-0.0) is -0.0 - a divergence the
// user DECLARED on 10 Aug 2026 (IEEE 754-2019 §5.9 gives the result the sign of the
// operand, and fbc is inconsistent with itself there). Through FPC's Frac that
// declared divergence leaked one step downstream and turned Frac(-0.0) into +0.
// ⇒ State the zero rule directly instead of inheriting it from a Fix we deliberately
// changed. Imposing the bit is idempotent, so this is right on both platforms.
begin
  Result := System.Frac(X);
  if (Result = 0) and (X = 0) and (PInt64(@X)^ < 0) then
    PInt64(@Result)^ := PInt64(@Result)^ or Int64($8000000000000000);
end;

function FloorDouble(const X: Double): Double; inline;
begin
  Result := System.Int(X);
  if Result > X then Result := Result - 1;
  // ⚠️ NEGATIVE ZERO: fbc prints Int(-0.0) as -0, and so does roundsd, which is what the AOT emits.
  // The sign is put back - otherwise WHICH ENGINE RAN would change the answer, and that is the one
  // thing the two-implementation design is not allowed to do.
  // ⛔⛔ It FORCES the bit, it does not negate: see the long note in FixDouble. System.Int LOSES
  // the sign on Windows but KEEPS it on Linux, so negating flipped it and returned +0 where -0
  // is wanted.
  // Only reachable when the result is zero, so no ordinary value pays for the test.
  if (Result = 0) and (PInt64(@X)^ < 0) then
    PInt64(@Result)^ := PInt64(@Result)^ or Int64($8000000000000000);
end;

function RecFieldInt(R: PRecordStorage; Enc: Int64): Int64; inline;
var
  p: PByte;
begin
  p := @R^.Bytes[Enc shr 4];
  if (Enc and $F) = 0 then Exit(PInt64(p)^);
  case Enc and $F of
    1: Result := PShortInt(p)^;      // s8
    2: Result := PByte(p)^;          // u8
    3: Result := PSmallInt(p)^;      // s16
    4: Result := PWord(p)^;          // u16
    5: Result := PLongInt(p)^;       // s32
    6: Result := PLongWord(p)^;      // u32
  else Result := PInt64(p)^;
  end;
end;

procedure RecSetFieldInt(R: PRecordStorage; Enc, Val: Int64); inline;
var
  p: PByte;
begin
  p := @R^.Bytes[Enc shr 4];
  if (Enc and $F) = 0 then begin PInt64(p)^ := Val; Exit; end;
  case Enc and $F of
    1, 2: PByte(p)^ := Byte(Val);
    3, 4: PWord(p)^ := Word(Val);
    5, 6: PLongWord(p)^ := LongWord(Val);
  else PInt64(p)^ := Val;
  end;
end;

function RecFieldFloat(R: PRecordStorage; Enc: Int64): Double; inline;
begin
  // A SINGLE field is FOUR bytes now, where the slot array held it widened to a Double. What a
  // program sees is unchanged - a store already rounded to single precision - but the BYTES are now
  // the ones fbc writes, which is the whole point of the exercise.
  if (Enc and $F) = 7 then
    Result := PSingle(@R^.Bytes[Enc shr 4])^
  else
    Result := PDouble(@R^.Bytes[Enc shr 4])^;
end;

procedure RecSetFieldFloat(R: PRecordStorage; Enc: Int64; Val: Double); inline;
begin
  if (Enc and $F) = 7 then
    PSingle(@R^.Bytes[Enc shr 4])^ := Val
  else
    PDouble(@R^.Bytes[Enc shr 4])^ := Val;
end;

function TBytecodeVM.AllocRecord(Ctx: TExecutionContext; ByteSize, StrC, TypeId: Integer): Integer;
// Allocate a record instance (heap block of typed slot arrays) in Ctx's per-thread heap and
// return its handle (an index into Ctx.Records).
var
  RecClr: Integer;   // clearing a REUSED slot's string vector; see the note below
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
  // A3-i: one byte image plus the string vector. SetLength zero-fills a FRESH block, which is the
  // initial state a record must have.
  // ⛔⛔ ...AND "FRESH" IS THE WHOLE CLAIM, which held only for a slot never used before. A block or a
  // frame reclaim rolls RecordCount BACK (bcRecMarkPop, PopFrame), so the next allocation lands on a
  // slot that already holds the previous occupant's data - and SetLength on an array ALREADY that
  // length does nothing at all. Every UDT local declared in a Scope, in a loop body or in a Sub was
  // therefore handed its PREVIOUS value instead of zero, on all four engines and in silence:
  //     Sub f() : Dim As A a : Print a.x : a.x = 11 : End Sub
  //     f() : f()          ' FreeBASIC prints 0 and 0; we printed 0 and 11
  // Below the high-water mark the slot is reused and is cleared here; at or above it, SetLength has
  // just zero-filled and there is nothing to do - which is what keeps this off the growing path.
  if Ctx.RecordCount < Ctx.RecordHigh then
  begin
    SetLength(Ctx.Records[Ctx.RecordCount].Bytes, ByteSize);
    if ByteSize > 0 then
      FillChar(Ctx.Records[Ctx.RecordCount].Bytes[0], ByteSize, 0);
    SetLength(Ctx.Records[Ctx.RecordCount].StringData, StrC);
    // ⚠️ The strings need their OWN clear: they are managed, so FillChar over them would leak the old
    // reference and hand the next reader a dangling one. Assigning '' releases it properly.
    for RecClr := 0 to StrC - 1 do
      Ctx.Records[Ctx.RecordCount].StringData[RecClr] := '';
  end
  else
  begin
    SetLength(Ctx.Records[Ctx.RecordCount].Bytes, ByteSize);
    SetLength(Ctx.Records[Ctx.RecordCount].StringData, StrC);
  end;
  Result := Ctx.RecordCount;
  Inc(Ctx.RecordCount);
  if Ctx.RecordCount > Ctx.RecordHigh then Ctx.RecordHigh := Ctx.RecordCount;
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

function TBytecodeVM.AllocSharedRecord(ByteSize, StrC, TypeId: Integer): Int64;
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
  // On a recycled record these are almost always no-ops - the shape matches the record that was
  // retired - and FPC does return immediately when the length already matches. ⛔ BUT "returns
  // immediately" IS STILL A CALL. Profiled 21 Aug 2026 on a New/Delete loop, release build with
  // symbols, sampling only running threads: fpc_dynarray_setlength was 10.8% of the whole program,
  // second only to the allocator body itself, while doing nothing on almost every call. Length() on
  // a dynamic array is an inline header read, so asking first turns those calls into a compare.
  if Length(R^.Bytes) <> ByteSize then SetLength(R^.Bytes, ByteSize);
  if Length(R^.StringData) <> StrC then SetLength(R^.StringData, StrC);
  // A recycled record must be indistinguishable from a fresh one: a brand-new SetLength zero-fills,
  // so recycling has to zero explicitly. (Strings were already emptied when it was retired.)
  if ByteSize > 0 then FillChar(R^.Bytes[0], ByteSize, 0);
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
  begin
    if (GetEnvironmentVariable('RECDIAG') = '1') and
       ((Handle < 0) or (Handle > High(Ctx.Records))) then
      WriteLn(ErrOutput, Format('[rec] FUORI RANGE handle=%d alto=%d pc=%d',
              [Handle, High(Ctx.Records), Ctx.PC]));
    Result := @Ctx.Records[Handle];
  end;
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
    // ⛔ A '+' or '-' TOUCHING the placeholders is a SIGN POSITION, not a literal. Everything outside
    // the placeholders used to go through ProcLiteral, so the mask's '+' was printed AND the number's
    // own '-' was added: Format(-42, "+#####") answered "+-42" where fbc answers "-42", and
    // Format(42, "-#####") answered "-42" where fbc answers "42". Measured against fbc 23 Aug 2026.
    //   leading  '+' : the sign is always shown, '+' or '-'
    //   leading  '-' : only a negative shows one, a positive shows nothing
    //   trailing '+' : the sign follows the digits ("42+", "42-")
    //   trailing '-' : only a negative shows one, after the digits
    // ⭐ PRINT USING already knew all of this (FormatUsingFB, measured the same way). FORMAT() goes
    // through a DIFFERENT formatter, and only one of the two had been taught - the same parallel-list
    // shape this project keeps paying for.
    // ⚠️ Declared divergence: fbc answers Format(0, "+#####") with a NUL byte followed by "0". That is
    // an fbc defect, not a rule; we answer "+0".
    if (prefix <> '') and (prefix[Length(prefix)] in ['+', '-']) then
    begin
      if prefix[Length(prefix)] = '+' then
      begin
        // always a sign, and it REPLACES the mask character rather than being dropped
        if neg and (scaled <> 0) then prefix[Length(prefix)] := '-';
      end
      else
        if not (neg and (scaled <> 0)) then SetLength(prefix, Length(prefix) - 1);
      neg := False;                                  // the mask owns the sign now
    end
    else if (suffix <> '') and (suffix[1] in ['+', '-']) then
    begin
      if suffix[1] = '+' then
      begin
        if neg and (scaled <> 0) then suffix[1] := '-';
      end
      else
        if not (neg and (scaled <> 0)) then Delete(suffix, 1, 1);
      neg := False;
    end;
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

function TBytecodeVM.PtrDomainLoadZStr(Ctx: TExecutionContext; PtrAddr: Int64;
  Wide: Boolean; ExactBytes: Integer): AnsiString;
// A C STRING read at a PACKED ARRAY pointer - the third domain, beside the raw heap and the
// record-field one this opcode already told apart.
// ⛔ It was missing, and the shape that wanted it is fbc's own idiom for a byte buffer:
// "Dim As UByte foo(...)" then "*Cast(ZString Ptr, @foo(0))". Reading a single element through the
// same address worked ("*Cast(UByte Ptr, @foo(0))" answers 65), because the SCALAR loads have their
// packed arm; only the string pair went straight to RawAddr and raised "Null or invalid raw pointer
// dereference" on a perfectly good array address. DIVERGENZE 127.
var
  ArrayIdx: Integer;
  PtrOffset, Lim: Int64;
  Ch: Int64;
begin
  Result := '';
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  Lim := High(FArrays[ArrayIdx].IntData);
  while PtrOffset <= Lim do
  begin
    Ch := FArrays[ArrayIdx].IntData[PtrOffset];
    // ⚠️ A WIDE cell is one ELEMENT here, not two bytes: the array holds one code unit per element,
    // which is the same image the scalar loads see through this address.
    if Ch = 0 then Break;
    if Wide then Result := Result + UTF8Encode(WideChar(Word(Ch)))
    else Result := Result + AnsiChar(Byte(Ch));
    Inc(PtrOffset);
    if (ExactBytes > 0) and (Length(Result) >= ExactBytes) then Break;
  end;
  if (ExactBytes > 0) and (Length(Result) < ExactBytes) then
    Result := Result + StringOfChar(#0, ExactBytes - Length(Result));
end;

procedure TBytecodeVM.PtrDomainStoreZStr(Ctx: TExecutionContext; PtrAddr: Int64;
  const Value: AnsiString; Wide: Boolean);
// The write half of PtrDomainLoadZStr: the characters plus the terminator, one per element.
var
  ArrayIdx, i: Integer;
  PtrOffset, Lim: Int64;
  W: WideString;
begin
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if GetEnvironmentVariable('ZPTR_DIAG') = '1' then
    WriteLn(StdErr, '[ZPTR] store addr=', PtrAddr, ' idx=', ArrayIdx, ' off=', PtrOffset,
            ' highArr=', High(FArrays), ' limInt=', High(FArrays[ArrayIdx].IntData));
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  Lim := High(FArrays[ArrayIdx].IntData);
  if Wide then
  begin
    W := UTF8Decode(Value);
    for i := 1 to Length(W) do
    begin
      if PtrOffset > Lim then Exit;
      FArrays[ArrayIdx].IntData[PtrOffset] := Ord(W[i]);
      Inc(PtrOffset);
    end;
  end
  else
    for i := 1 to Length(Value) do
    begin
      if PtrOffset > Lim then Exit;
      FArrays[ArrayIdx].IntData[PtrOffset] := Ord(Value[i]);
      Inc(PtrOffset);
    end;
  if PtrOffset <= Lim then FArrays[ArrayIdx].IntData[PtrOffset] := 0;   // the terminator
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

function TBytecodeVM.PtrDomainLoadInt(Ctx: TExecutionContext; PtrAddr: Int64): Int64;
// A pointer value that is NOT a raw heap address: a record-field pointer or a packed array pointer.
var
  Rec: PRecordStorage;
  RecSlot, ArrayIdx: Integer;
  PtrOffset: Int64;
begin
  if PtrAddr < 0 then
  begin
    Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
    Exit(RecFieldInt(Rec, RecSlot));
  end;
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  // The vector that IS populated is the discriminator - see the note in bcRefLoadInt.
  if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
    Result := FArrays[ArrayIdx].IntData[PtrOffset]
  else if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
    Result := PInt64(@FArrays[ArrayIdx].FloatData[PtrOffset])^
  else
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
end;

function TBytecodeVM.PtrDomainLoadFloat(Ctx: TExecutionContext; PtrAddr: Int64): Double;
var
  Rec: PRecordStorage;
  RecSlot, ArrayIdx: Integer;
  PtrOffset: Int64;
begin
  if PtrAddr < 0 then
  begin
    Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
    Exit(RecFieldFloat(Rec, RecSlot));
  end;
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
    Result := FArrays[ArrayIdx].FloatData[PtrOffset]
  else if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
    Result := PDouble(@FArrays[ArrayIdx].IntData[PtrOffset])^
  else
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
end;

procedure TBytecodeVM.PtrDomainStoreInt(Ctx: TExecutionContext; PtrAddr, Value: Int64);
var
  Rec: PRecordStorage;
  RecSlot, ArrayIdx: Integer;
  PtrOffset: Int64;
begin
  if PtrAddr < 0 then
  begin
    Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
    RecSetFieldInt(Rec, RecSlot, Value);
    Exit;
  end;
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
    FArrays[ArrayIdx].IntData[PtrOffset] := Value
  else if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
    PInt64(@FArrays[ArrayIdx].FloatData[PtrOffset])^ := Value
  else
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
end;

procedure TBytecodeVM.PtrDomainStoreFloat(Ctx: TExecutionContext; PtrAddr: Int64; Value: Double);
var
  Rec: PRecordStorage;
  RecSlot, ArrayIdx: Integer;
  PtrOffset: Int64;
begin
  if PtrAddr < 0 then
  begin
    Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
    RecSetFieldFloat(Rec, RecSlot, Value);
    Exit;
  end;
  ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
  if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
    FArrays[ArrayIdx].FloatData[PtrOffset] := Value
  else if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
    PDouble(@FArrays[ArrayIdx].IntData[PtrOffset])^ := Value
  else
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
end;

function TBytecodeVM.RawLoadInt(RawPtr: Int64; TypeCode: Integer): Int64;
begin
  case TypeCode of
    RTC_I8:  Result := PShortInt(RawAddr(RawPtr, 1))^;
    RTC_I16: Result := PSmallInt(RawAddr(RawPtr, 2))^;
    RTC_I32: Result := PLongInt(RawAddr(RawPtr, 4))^;
    // The unsigned views ZERO-extend: a UByte holding 200 is 200 in the int bank, not -56.
    RTC_U8:  Result := PByte(RawAddr(RawPtr, 1))^;
    RTC_U16: Result := PWord(RawAddr(RawPtr, 2))^;
    RTC_U32: Result := PLongWord(RawAddr(RawPtr, 4))^;
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

function DirTranslateSpec(const Spec: string): string;
// FreeBASIC's DIR keeps the DOS reading of "*.*": it means EVERY entry, dotted or not. On Unix the
// pattern reaches fnmatch, where "*.*" demands a literal dot, so a directory named "sub" was missing
// from a listing fbc includes. Measured: fbc on Linux lists "sub" for "*.*".
begin
  if Spec = '*.*' then Result := '*'
  else if (Length(Spec) >= 4) and (Copy(Spec, Length(Spec) - 3, 4) = '/*.*') then
    Result := Copy(Spec, 1, Length(Spec) - 3) + '*'
  else
    Result := Spec;
end;

function DirEntryAttrs(const Rec: TSearchRec): Integer;
// The attributes of ONE entry, in FreeBASIC's spelling rather than the platform's.
//
// ⛔ A DIRECTORY DOES NOT CARRY THE ARCHIVE BIT. Measured against fbc 1.10.1 on Linux:
//   a plain file -> 32 (archive)        a subdirectory -> 16 (directory), NOT 48.
// FPC's FindFirst on Unix sets faArchive on EVERYTHING, directories included, so the
// raw Attr said 48 - and the rule above then rejected the directory from a mask of
// fbDirectory alone, because 48 carries a bit (archive) the mask does not allow.
// ⇒ "Dir(""*"", fbDirectory)" came back EMPTY while fbc listed the subdirectory. The
// two halves look like separate defects and are one: the entry, not the rule.
//
// ⚠️ Invisible on Windows, which is where the baseline was captured: on NTFS a
// directory genuinely has no archive bit, so the raw value was already right. This is
// the shape of defect the move to Linux exposes - a platform whose spelling we adopted
// without translating it.
begin
  Result := Rec.Attr;
  {$IFDEF UNIX}
  // A leading dot IS the hidden attribute on Unix, and that is how fbc reports it: ".hidden.txt" is
  // 34 (archive|hidden), "." and ".." are 18 (directory|hidden). FPC's FindFirst marks the ordinary
  // dotfiles but not "." and "..", so the two dot entries came back as plain directories - and the
  // MASK then let them through where fbc excludes them, which is the whole reason a fbDirectory-only
  // walk looked as if fbc dropped them.
  if (Rec.Name <> '') and (Rec.Name[1] = '.') then Result := Result or faHidden;
  {$ENDIF}
  if (Result and faDirectory) <> 0 then
    Result := Result and not faArchive;
end;

function DirEntrySkipped(const Rec: TSearchRec): Boolean;
// Nothing is skipped: the MASK decides, and "." and ".." are ordinary entries to it.
//
// ⛔ THIS USED TO DROP "." AND ".." ON UNIX, on a measurement that was real and read the wrong way.
// The observation was that fbc listed only "sub" for a directory holding "sub", "." and ".." - true,
// and it was measured with a mask of fbDirectory ALONE. Re-measured across four masks: a dot entry
// carries DIRECTORY *and* HIDDEN (attrib 18), so fbDirectory alone excludes it by the ordinary rule,
// while "fbDirectory Or fbHidden" lists all three, exactly as fbc does. The skip was a second rule
// saying what the first already said, and where they disagreed the second one won and was wrong -
// the manual's own system/dirfolder walks with every bit set and prints "." and "..".
//
// ⚠️ The dotted names get their HIDDEN bit from the platform layer (a leading dot on Unix), which is
// also why ".hidden.txt" reports 34 - measured, and identical to fbc.
begin
  Result := False;
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
// stored as WIDE_CELL_BYTES-wide cells, one per codepoint, and converted to the VM's uniform UTF-8
// managed string.
var
  P: PByte;
  ofs, Limit, n: PtrUInt;
  i: Integer;
  W: UnicodeString;
  PW: PLongWord;
begin
  // ⛔ A NULL ZSTRING/WSTRING POINTER READS AS THE EMPTY STRING, and that is fbc's rule rather than
  // undefined behaviour it gets away with: its string runtime tests the pointer, so "Len(*pz)" answers
  // 0 and "*pz" answers "" on a null "ZString Ptr". We went through RawAddr, which raises "Null or
  // invalid raw pointer dereference" - correct for every NUMERIC view of a raw pointer and wrong for
  // the STRING one, which is the only view with a defined answer at zero.
  // ⚠️ EXACTLY zero. An invalid non-zero pointer still raises: that check is what keeps a raw pointer
  // from addressing memory the VM does not own, and it is not being relaxed here.
  if RawPtr = 0 then Exit('');
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
    // ⭐ ONE CELL IS WIDE_CELL_BYTES, and the cell is WIDER than the UnicodeString unit it decodes
    // into, so this cannot be a Move: each cell is read whole and narrowed. A codepoint above the BMP
    // fits one cell here and TWO UTF-16 units in W, which is why W is built by appending.
    PW := PLongWord(P);
    n := 0;
    while ((n + 1) * WIDE_CELL_BYTES <= Limit) and (PW[n] <> 0) do Inc(n);
    W := '';
    for i := 0 to Integer(n) - 1 do W := W + UCS4CellToUnicode(PW[i]);
    Result := UTF8Encode(W);
  end;
end;

procedure TBytecodeVM.RawStoreZStrVal(RawPtr: Int64; const S: string; Wide: Boolean);
// "*p = s" where p is a ZSTRING/WSTRING PTR: the string's characters + NUL terminator at the
// pointed address. Bounds-checked as a whole through RawAddr - an overflowing store raises
// instead of corrupting the heap (fbc would silently overrun).
var
  P: PByte;
  i: Integer;
  U: TUCS4Cells;
begin
  if not Wide then
  begin
    P := PByte(RawAddr(RawPtr, PtrUInt(Length(S)) + 1));
    if Length(S) > 0 then Move(S[1], P^, Length(S));
    P[Length(S)] := 0;
  end
  else
  begin
    // The mirror: one CELL per codepoint, a surrogate PAIR folded back into the single cell it came
    // from - so a round trip through the buffer is the identity, which the UCS-2 image could not
    // promise above the BMP.
    U := UnicodeToUCS4Cells(UTF8Decode(S));
    P := PByte(RawAddr(RawPtr, (PtrUInt(Length(U)) + 1) * WIDE_CELL_BYTES));
    for i := 0 to Length(U) - 1 do PLongWord(P)[i] := U[i];
    PLongWord(P)[Length(U)] := 0;
  end;
end;

function TBytecodeVM.RawStrCellGet(RawPtr: Int64): string;
// "*p" where p is a "STRING PTR": a MANAGED string cell. FreeBASIC's String is a DESCRIPTOR - a
// pointer, a length and a capacity, SizeOf(String) = 24 bytes - whose characters live elsewhere, and
// this is that model in our terms: the 24-byte cell holds an INDEX into FRawStrCells, and the text
// itself stays a managed string. Reading a cell that was never written (CAllocate zeroes it) gives
// index 0, which is reserved for the empty string.
//
// ⛔ Deliberately NOT the ZSTRING treatment. Writing the characters into the cell would fit the
// manual's example by luck - 23 characters into 24 bytes - and raise on the next longer string.
var
  Idx: Int64;
begin
  Idx := PInt64(RawAddr(RawPtr, 8))^;
  if (Idx > 0) and (Idx <= High(FRawStrCells)) then Result := FRawStrCells[Idx] else Result := '';
end;

procedure TBytecodeVM.RawStrCellSet(RawPtr: Int64; const S: string);
// Store into a managed string cell: reuse the slot this cell already names, or take a new one.
// ⚠️ A slot is not reclaimed when the block is Deallocate'd - the cell is gone by then and nothing
// names the slot. The leak is one string per DISTINCT cell ever written, which no realistic program
// makes unbounded; reclaiming it would need the raw heap to know which of its bytes are cells.
var
  Idx: Int64;
  P: PInt64;
begin
  P := PInt64(RawAddr(RawPtr, 8));
  Idx := P^;
  if (Idx <= 0) or (Idx > High(FRawStrCells)) then
  begin
    if Length(FRawStrCells) = 0 then
    begin
      SetLength(FRawStrCells, 2);
      FRawStrCells[0] := '';        // slot 0 is the empty string: a zeroed cell reads as ""
      Idx := 1;
    end
    else
    begin
      Idx := Length(FRawStrCells);
      SetLength(FRawStrCells, Idx + 1);
    end;
    P^ := Idx;
  end;
  FRawStrCells[Idx] := S;
end;

procedure TBytecodeVM.RawStoreInt(RawPtr: Int64; TypeCode: Integer; Value: Int64);
begin
  case TypeCode of
    RTC_I8:  PShortInt(RawAddr(RawPtr, 1))^ := ShortInt(Value);
    RTC_I16: PSmallInt(RawAddr(RawPtr, 2))^ := SmallInt(Value);
    RTC_I32: PLongInt(RawAddr(RawPtr, 4))^ := LongInt(Value);
    // Unsigned views: same WIDTH, so the bytes written are the same - they exist for the LOAD.
    RTC_U8:  PByte(RawAddr(RawPtr, 1))^ := Byte(Value);
    RTC_U16: PWord(RawAddr(RawPtr, 2))^ := Word(Value);
    RTC_U32: PLongWord(RawAddr(RawPtr, 4))^ := LongWord(Value);
  else
    PInt64(RawAddr(RawPtr, 8))^ := Value;
  end;
end;

procedure TBytecodeVM.RawStoreFloat(RawPtr: Int64; TypeCode: Integer; Value: Double);
begin
  if TypeCode = RTC_SINGLE then PSingle(RawAddr(RawPtr, 4))^ := Value
  else PDouble(RawAddr(RawPtr, 8))^ := Value;
end;

// The destination of a BLOCK operation (CLEAR, FB_MEMCOPY, FB_MEMMOVE), which is not always the byte
// heap. FreeBASIC's own manual clears an ARRAY with it -
//     Clear array(0), , 100 * SizeOf(Integer)
// - and "array(0)" is not a raw pointer here: it is an FArrays-backed pointer (bit 63 and bit 62 both
// clear), packing arrayId+1 and the element offset, which RawAddr rejects out of hand. The statement
// therefore died on "Null or invalid raw pointer dereference" - and where the array was otherwise
// unused, DCE dropped the whole thing and the program printed fbc's answer for the wrong reason.
//
// An int or float array IS a contiguous byte image: IntData is "array of Int64" and FloatData "array of
// Double", so a block operation over it means what fbc means, element for element, PROVIDED the elements
// are eight bytes wide. A NARROW element type ("As Short") is stored widened here and its byte image is
// not fbc's; the SSA generator refuses those at compile time rather than answering differently in
// silence (see EmitRawMemOp). A STRING array has no byte image at all and is refused here.
//
// The bounds check is the same contract RawAddr keeps: NeedBytes must fit from the offset to the end of
// the storage, so a block operation can never reach memory the VM does not own.
function TBytecodeVM.BlockAddr(Ctx: TExecutionContext; Ptr: Int64; NeedBytes: PtrUInt): Pointer;
var
  ArrayIdx: Integer;
  PtrOffset, Avail: Int64;
begin
  // A raw-heap / framebuffer pointer, or a record-field pointer (bit 63): RawAddr owns both answers -
  // the second one by refusing it, since a record field is not a byte image either.
  if (Ptr and RAWPTR_TAG) <> 0 then Exit(RawAddr(Ptr, NeedBytes));
  if Ptr < 0 then
    raise ERangeError.Create('CLEAR/FB_MEMCOPY: a record-field pointer is not a byte image');
  if Ptr = 0 then
    raise ERangeError.Create('Null or invalid raw pointer dereference');

  ArrayIdx := MapArrDyn(Ctx, (Ptr shr POINTER_ARRAY_SHIFT) - 1);
  PtrOffset := Ptr and POINTER_OFFSET_MASK;
  if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
    raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [Ptr]);
  case FArrays[ArrayIdx].ElementType of
    0: begin
         Avail := (Int64(Length(FArrays[ArrayIdx].IntData)) - PtrOffset) * SizeOf(Int64);
         if (PtrOffset > High(FArrays[ArrayIdx].IntData)) or (Int64(NeedBytes) > Avail) then
           raise ERangeError.CreateFmt('Block operation out of bounds: %d bytes from element %d, %d available',
                                       [Int64(NeedBytes), PtrOffset, Avail]);
         Result := @FArrays[ArrayIdx].IntData[PtrOffset];
       end;
    1: begin
         Avail := (Int64(Length(FArrays[ArrayIdx].FloatData)) - PtrOffset) * SizeOf(Double);
         if (PtrOffset > High(FArrays[ArrayIdx].FloatData)) or (Int64(NeedBytes) > Avail) then
           raise ERangeError.CreateFmt('Block operation out of bounds: %d bytes from element %d, %d available',
                                       [Int64(NeedBytes), PtrOffset, Avail]);
         Result := @FArrays[ArrayIdx].FloatData[PtrOffset];
       end;
  else
    raise ERangeError.Create('CLEAR/FB_MEMCOPY over a STRING array: its elements are managed values, not bytes');
  end;
end;

// FB_MEMCOPY / FB_MEMMOVE: copy ByteCount bytes from SrcPtr to DstPtr. Both pointers are resolved
// through BlockAddr, so either may name the byte heap, the framebuffer or an array's storage, and both
// ends are bounds-checked against their own region. FPC Move is overlap-safe, so this serves both the
// (non-overlapping) memcopy and the (overlap-safe) memmove semantics.
procedure TBytecodeVM.RawMemCopy(Ctx: TExecutionContext; DstPtr, SrcPtr: Int64; ByteCount: PtrUInt);
begin
  if ByteCount = 0 then Exit;
  Move(BlockAddr(Ctx, SrcPtr, ByteCount)^, BlockAddr(Ctx, DstPtr, ByteCount)^, ByteCount);
end;

// CLEAR: set ByteCount bytes at DstPtr to Value, in whichever region DstPtr names.
procedure TBytecodeVM.RawClear(Ctx: TExecutionContext; DstPtr: Int64; Value: Byte; ByteCount: PtrUInt);
begin
  if ByteCount = 0 then Exit;
  FillChar(BlockAddr(Ctx, DstPtr, ByteCount)^, ByteCount, Value);
end;

procedure TBytecodeVM.RecordNewArrayInit(Ctx: TExecutionContext; ArrayId: Integer; PackedCounts: Int64);
// Eager-allocate one record instance per element of the (int handle) array and store the handles.
// PackedCounts = byteSize | 0<<16 | strCount<<32 | typeId<<48. A3-i: bits 16..31 used to hold the
// float slot count and are now always zero - the numeric halves are one byte image, so allocation
// needs a SIZE and a string count. M5.2c: array-of-UDT records go in the shared region (the handle
// array FArrays[ArrayId] is global, so any thread can reach them).
var
  k, ByteSize, StrC, TypeId: Integer;
begin
  ByteSize := PackedCounts and $FFFF;
  StrC := (PackedCounts shr 32) and $FFFF;
  TypeId := (PackedCounts shr 48) and $FFFF;
  // Allocate a record only for elements that do not already have one. A valid array-of-UDT element
  // handle is a shared-region record (SHARED_REC_FLAG set), so it is never 0 — a 0 handle marks an
  // uninitialized slot. After a plain DIM every slot is 0, so all are filled; after REDIM [PRESERVE]
  // only the freshly-grown slots are 0, so existing records are kept (no clobber / leak).
  for k := 0 to FArrays[ArrayId].TotalSize - 1 do
    if FArrays[ArrayId].IntData[k] = 0 then
      FArrays[ArrayId].IntData[k] := AllocSharedRecord(ByteSize, StrC, TypeId);
end;

function TBytecodeVM.AllocSharedRecordBlock(N, ByteSize, StrC, TypeId: Integer): Int64;
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
      R^.BlockLen := 0;
      SetLength(R^.Bytes, ByteSize);
      SetLength(R^.StringData, StrC);
      if FSharedRecordCount >= Length(FSharedRecords) then
        GrowSharedRecords(FSharedRecordCount + 1);
      FSharedRecords[FSharedRecordCount] := R;
      FSharedRecStore[FSharedRecordCount] := R;
      Inc(FSharedRecordCount);
    end;
    FSharedRecords[firstIdx]^.BlockLen := N;   // only the FIRST record carries the block's length
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;
  Result := SHARED_REC_FLAG or Int64(firstIdx);
end;

function TBytecodeVM.SharedRecordBlockLen(Handle: Int64): Int64;
// How many CONSECUTIVE records the block starting at Handle holds. Only the FIRST record of a block
// carries the number (AllocSharedRecordBlock writes it there), so anything else - a lone record, a
// handle into the middle - answers 1, which is what "Delete[] p" on a single object must do.
var
  Idx: Integer;
begin
  Result := 1;
  if (Handle and SHARED_REC_FLAG) = 0 then Exit;
  Idx := Integer(Handle and not SHARED_REC_FLAG);
  if (Idx < 0) or (Idx >= FSharedRecordCount) then Exit;
  if FSharedRecords[Idx]^.BlockLen > 1 then Result := FSharedRecords[Idx]^.BlockLen;
end;

function TBytecodeVM.ReallocSharedRecordBlock(OldHandle: Int64; NewN, ByteSize, StrC, TypeId: Integer): Int64;
// FreeBASIC "p = Reallocate(p, n * SizeOf(T))" where p is a MANAGED block of UDT records: give the block
// NewN records, keeping the contents of the ones already there.
//
// The region only ever APPENDS, so when the block is the last thing in it the extra records go straight
// on the end and the handle does not move - which is what a C realloc usually does too. Otherwise a
// fresh block is allocated and the old records' contents are copied one level deep, exactly as
// DeepCopyArrayRecords does for an array member. The old block is left alone rather than freed: every
// other path here leaks a superseded record the same way, and freeing one a live handle may still name
// is the one thing that must not happen.
//
// ⛔ Reallocate on a UDT pointer used to take the RAW path, which read the managed handle as a byte
// offset: proguide/dynamicmemory printed its first line and died on an access violation.
var
  OldIdx, OldN, i, k: Integer;
  NewHandle: Int64;
  Src, Dst: PRecordStorage;
begin
  Result := OldHandle;
  if (OldHandle and SHARED_REC_FLAG) = 0 then Exit;        // not a managed block: nothing to do here
  OldIdx := Integer(OldHandle and not SHARED_REC_FLAG);
  if (OldIdx < 0) or (OldIdx >= FSharedRecordCount) then Exit;
  OldN := FSharedRecords[OldIdx]^.BlockLen;
  if OldN < 1 then OldN := 1;
  if NewN < 1 then NewN := 1;
  if NewN = OldN then Exit;

  EnterCriticalSection(FSharedRecLock);
  try
    if (NewN > OldN) and (OldIdx + OldN = FSharedRecordCount) then
    begin
      // The block ends the region: extend it in place.
      for i := 0 to NewN - OldN - 1 do
      begin
        New(Dst);
        Dst^.TypeId := TypeId;
        Dst^.BlockLen := 0;
        SetLength(Dst^.Bytes, ByteSize);
        SetLength(Dst^.StringData, StrC);
        if FSharedRecordCount >= Length(FSharedRecords) then
          GrowSharedRecords(FSharedRecordCount + 1);
        FSharedRecords[FSharedRecordCount] := Dst;
        FSharedRecStore[FSharedRecordCount] := Dst;
        Inc(FSharedRecordCount);
      end;
      FSharedRecords[OldIdx]^.BlockLen := NewN;
      Exit(OldHandle);
    end;
  finally
    LeaveCriticalSection(FSharedRecLock);
  end;

  NewHandle := AllocSharedRecordBlock(NewN, ByteSize, StrC, TypeId);
  k := NewN; if OldN < k then k := OldN;
  for i := 0 to k - 1 do
  begin
    Src := FSharedRecords[OldIdx + i];
    Dst := FSharedRecords[Integer(NewHandle and not SHARED_REC_FLAG) + i];
    Dst^.TypeId := Src^.TypeId;
    Dst^.Bytes := Copy(Src^.Bytes, 0, Length(Src^.Bytes));
    Dst^.StringData := Copy(Src^.StringData, 0, Length(Src^.StringData));
  end;
  Result := NewHandle;
end;

procedure TBytecodeVM.DeepCopyArrayRecords(Ctx: TExecutionContext; DestArr, SrcArr: Int64; PackedCounts: Int64);
// FreeBASIC value semantics of an array-of-UDT member: give the destination its OWN element records,
// each holding an independent copy of the corresponding source element's contents (so "Dim b = a" and
// return-by-value do not share element instances). The dest handle array is resized to match src; each
// dest element is reused if present (contents overwritten) or freshly allocated. Record contents are
// copied one level deep (Int/Float/StringData via Copy) — a nested UDT/array inside an element is copied
// as its handle (shallow at that deeper level), matching the SSA EmitRecordCopy depth for arrays.
var
  ByteSize, StrC, TypeId, k: Integer;
  SrcRec, DestRec: PRecordStorage;
begin
  if (DestArr < 1) or (DestArr > High(FArrays)) or (SrcArr < 1) or (SrcArr > High(FArrays)) then Exit;
  ByteSize := PackedCounts and $FFFF;
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
      FArrays[DestArr].IntData[k] := AllocSharedRecord(ByteSize, StrC, TypeId);
    SrcRec := ResolveRec(Ctx, FArrays[SrcArr].IntData[k]);
    DestRec := ResolveRec(Ctx, FArrays[DestArr].IntData[k]);
    if (SrcRec <> nil) and (DestRec <> nil) then
    begin
      DestRec^.TypeId := SrcRec^.TypeId;
      DestRec^.Bytes := Copy(SrcRec^.Bytes);
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
  // The worker's own block of PROC-LOCAL array slots. Without this every worker inside the same SUB
  // indexed the one storage the compile-time id names, and they overwrote each other's elements.
  BindArrayMap(WCtx);
  SetLength(WCtx.Records, 0);
  WCtx.RecordCount := 0;
  WCtx.RecordHigh := 0;   // the slots are GONE, so nothing below the mark is reused any more
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

  procedure CreditField(Bank: TRegBank; Reg: Integer);
  // Credit one operand field to the bank SedaiOpcodeBanks says it names. rbUnknown credits nothing:
  // no list claims the field, and guessing a bank here is what sizes the wrong one.
  begin
    case Bank of
      rbInt:    if Reg > MaxIntReg then MaxIntReg := Reg;
      rbFloat:  if Reg > MaxFloatReg then MaxFloatReg := Reg;
      rbString: if Reg > MaxStringReg then MaxStringReg := Reg;
    end;
  end;

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

  { "OPTION DIGITS n". Applied AFTER the dialect block above, and that order is
    the point: the preset decides the number FORMAT (where the spaces go), the
    option decides the PRECISION, and a program that asks for a precision must
    not have it overwritten by a preset chosen for another reason.
    ⭐ It sets both banks. Asking for n digits and getting 7 on a Single would be
    the surprising reading - the request is about how much of the number to
    show, and a Single simply runs out of true digits sooner (its exact
    expansion terminates, so the rest come out as zeros and are stripped).
    Whatever n is, the digits stay correctly rounded from the exact value:
    the option moves the precision, never the standard. }
  if Assigned(FConsoleBehavior) and Assigned(FProgram) and (FProgram.OptionDigits > 0) then
  begin
    FConsoleBehavior.FloatDigits := FProgram.OptionDigits;
    FConsoleBehavior.SingleDigits := FProgram.OptionDigits;
  end;

  // RESERVE the whole static array-id space up front. Static arrays have compile-time FArrays indices, but
  // a UDT array member gets its handle at RUNTIME by appending at Length(FArrays). Growing FArrays lazily
  // (only as each static array is DIM'd) let a member array claim an id still owed to a static one — most
  // often a param placeholder, which is never DIM'd at all — and the two then ALIAS the same storage.
  if FProgram.GetArrayCount > Length(FArrays) then
    SetLength(FArrays, FProgram.GetArrayCount);

  // Reserve the per-context blocks for the PROC-LOCAL arrays and give the main context its map. Must
  // come after the reservation above (the static id space has to be complete) and before anything
  // executes: from here on every array access goes through Ctx.ArrMap.
  BuildPrivateArrayPlan;
  BindArrayMap(FCtx);

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
        // Fused compare-and-branch (String) - Src1/Src2 index the STRING bank. ⛔ This case has no
        // else branch: an opcode missing here contributes ZERO, the bank is sized too small, and the
        // interpreter writes past the end - a heap corruption that surfaces at program EXIT, far
        // from the cause. It is the fourth of the four unchecked counters an opcode addition touches.
        bcBranchEqString, bcBranchNeString, bcBranchLtString, bcBranchGtString,
        bcBranchLeString, bcBranchGeString:
        begin
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
        end;

        // Fused compare-and-branch (Int, and Unsigned which reads the same bank) - IntRegs
        bcBranchLtUInt, bcBranchLeUInt, bcBranchGtUInt, bcBranchGeUInt,
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

        // The string fusions. ⛔ These three were MISSING here until 19 Aug 2026: they contributed
        // zero to the string bank, and only the fact that their registers are also touched by an
        // ordinary string opcode elsewhere in the same program kept the bank big enough. That is
        // luck, not design - a register used ONLY by one of these would have sized the bank short.
        // Dest/Src1/Src2 are string registers, Immediate is the INT register holding the index.
        bcStrConcatCharAt, bcStrAppendMapped, bcStrMidAssign:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
          if Instr.Src2 > MaxStringReg then MaxStringReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;
        end;
        // MID$ into an array element: Src1 is the ARRAY ID and indexes no bank at all; Dest is the
        // replacement (string), Src2 the linear index and Immediate the start (both int).
        bcStrMidAssignArr:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
          if (Instr.Immediate and $FFFF) > MaxIntReg then MaxIntReg := Instr.Immediate and $FFFF;
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
        // Bit intrinsics: all three operands are integer registers (Src2 unused = 0 for the counts,
        // which register 0 already covers).
        bcBitClz, bcBitCtz, bcBitPopcnt, bcBitRotl, bcBitRotr,
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
        bcMathSinh, bcMathCosh, bcMathTanh, bcMathAsinh, bcMathAcosh, bcMathAtanh,
        bcMathCeil, bcMathRound, bcMathMin, bcMathMax, bcMathCopySign:
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

        // BitsToSingle: float Dest, int Src1 - the same shape as IntToFloat, and it has to be here
        // for the same reason: a bank counted short is an index past the end of an array.
        bcBitsToSingle:
        begin
          if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        // SingleBits: int Dest, float Src1 - the other direction.
        bcSingleBits:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
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
        bcStrLen, bcStrLenW, bcStrAsc, bcStrAscW, bcStrDec, bcStrValInt, bcStrSAdd, bcStrCvInt, bcFileExists, bcFileLen:
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
        bcStrInstr, bcStrInstrRev, bcStrInstrRevAny, bcStrInstrAny, bcStrInstrW, bcStrInstrRevW,
        bcStrInstrAnyW, bcStrInstrRevAnyW:
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

        { ⛔⛔⛔ GROUP 12: BigInt. THIS SCAN IS A FOURTH COUNTER NOBODY CHECKS, and it
          is not in the opcode checklist. It sizes the register banks from the highest
          index each bank is ever given, opcode by opcode, with NO default arm: an
          opcode that is not listed here contributes NOTHING, the bank is created too
          small, and the interpreter then writes PAST THE END of it.
          The symptom is as far from the cause as it gets: values all correct, and a
          SIGSEGV inside FPC's SysFreeMem at teardown, on a program large enough to
          push a bank past its initial size (~21000 int registers here). Adding one
          unrelated variable moved the layout and made it vanish. Found 14 Aug 2026.
          ⚠️ A handle is an INT register; only BigToStr writes a STRING. }
        bcBigNew:
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
        bcBigFromInt, bcBigCopy:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        bcBigAdd, bcBigSub, bcBigMul, bcBigCmp, bcBigMulSmall, bcBigDiv, bcBigMod:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
          if Instr.Src2 > MaxIntReg then MaxIntReg := Instr.Src2;
        end;
        bcBigToStr:
        begin
          if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        bcBigToInt:
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        end;
        bcBigFromStr:   { the inverse: Dest is a handle (int), Src1 the TEXT }
        begin
          if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
          if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
        end;

        { ⛔⛔⛔ THE TRANSFER BANK, AND EVERY PROCEDURE PROLOGUE EMITS ONE. bcXferLoad<bank> writes
          its DEST into a register bank (the Immediate is the transfer SLOT, not a register), and
          bcXferStore<bank> reads its Src1 from one - and none of the six was listed here, so a
          procedure whose only use of a high register was its own parameter load sized the bank
          from nothing and the load wrote past the end. fbc's structs/derived-cast died on exactly
          that: "XferLoadInt Dest=20" as the first instruction of a prologue, in a program whose
          int registers otherwise stopped below 20. }
        bcXferLoadInt:    if Instr.Dest > MaxIntReg then MaxIntReg := Instr.Dest;
        bcXferLoadFloat:  if Instr.Dest > MaxFloatReg then MaxFloatReg := Instr.Dest;
        bcXferLoadString: if Instr.Dest > MaxStringReg then MaxStringReg := Instr.Dest;
        bcXferStoreInt:    if Instr.Src1 > MaxIntReg then MaxIntReg := Instr.Src1;
        bcXferStoreFloat:  if Instr.Src1 > MaxFloatReg then MaxFloatReg := Instr.Src1;
        bcXferStoreString: if Instr.Src1 > MaxStringReg then MaxStringReg := Instr.Src1;
      else
        { ⛔⛔⛔ THE SAFETY NET THE FOURTH COUNTER NEVER HAD, and it is what makes the omission above
          impossible to repeat. This case is hand-written and every arm names its banks; an opcode
          nobody listed used to contribute ZERO, so the bank was created too small and the
          interpreter wrote past the end - a heap corruption that surfaces at teardown, three steps
          from its cause (see the GROUP 12 note above, which paid for it once, and
          structs/derived-cast, which paid for it again with the TRANSFER opcodes).
          ⭐⭐ IT DELEGATES TO SedaiOpcodeBanks, the unit that exists to be the ONE answer to
          "which bank does this opcode use for this field". That unit's own header names this scan
          as the third copy still to be reconciled, and its note asks for the case that proves it
          FIRST: structs/derived-cast is that case. Answering here through the single source means
          an opcode nobody listed above is sized CORRECTLY rather than conservatively - which
          matters, because a census run under REGSCAN_DIAG names over a hundred of them, the whole
          RECORD family included, and crediting a record register to the STRING bank would inflate
          FrameSaveStrCount (refcounted string copies, per call).
          ⚠️ rbUnknown credits nothing, which is the old behaviour: this arm narrows the blind spot
          to the fields NO list claims, and REGSCAN_DIAG names them. }
        begin
          CreditField(BankOfDest(TBytecodeOp(Instr.OpCode)), Instr.Dest);
          CreditField(BankOfSrc1(TBytecodeOp(Instr.OpCode)), Instr.Src1);
          CreditField(BankOfSrc2(TBytecodeOp(Instr.OpCode)), Instr.Src2);
          if GetEnvironmentVariable('REGSCAN_DIAG') = '1' then
            WriteLn(ErrOutput, '[regscan] opcode NON ELENCATO nella scansione dei banchi: ',
                    BytecodeOpToString(TBytecodeOp(Instr.OpCode)),
                    ' (dest=', Instr.Dest, ' src1=', Instr.Src1, ' src2=', Instr.Src2, ')');
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
  AttachGraphicsToOutput;   // order-independent: whichever of the two arrives second wires the pair
end;

procedure TBytecodeVM.SetGraphicsBackend(Backend: IGraphicsBackend; OwnedObj: TObject = nil);
begin
  if Assigned(FOwnedGraphics) and (FOwnedGraphics <> OwnedObj) then
    FreeAndNil(FOwnedGraphics);
  FGraphics := Backend;
  FOwnedGraphics := OwnedObj;
  // Hand it to the output device too, so PRINT inside a graphics mode lands on the same surface as
  // LINE and PSET - which is what FreeBASIC does, and what we did not. The VM is the only thing that
  // holds both, so this is where the two are introduced.
  AttachGraphicsToOutput;
end;

procedure TBytecodeVM.AttachGraphicsToOutput;
// ⛔ There is NO cast from IOutputDevice back to its class here, and the first attempt at one crashed
// outright: under {$interfaces CORBA} an interface reference points at the interface's method table,
// NOT at the object, so "TObject(FOutputDevice)" is a wild pointer and "is" walks it.
// The wiring is done where both sides are known CONCRETELY instead - SedaiBasicVM.lpr already keeps a
// typed GTermCtrl handle for exactly this reason (AttachGraphicsMemory under --window does the same).
// Kept as a method so the intent has a name and one place to come back to.
begin
  // nothing to do here: see AttachGraphicsBackend at the call site in SedaiBasicVM.lpr
end;

function TBytecodeVM.GraphicsBackend: IGraphicsBackend;
begin
  Result := FGraphics;
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
  FCtx.RecordHigh := 0;   // the slots are GONE, so nothing below the mark is reused any more
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
  // ⚡ INPUT counts BYTES, so the whole request is one call. It used to walk the loop below asking
  // the file layer for ONE BYTE at a time and doing `Result := Result + Data` per byte - the same
  // shape as the 712 us-per-line file reader, and it made a block read of stdin pointless even
  // where it worked at all. WINPUT still needs the loop: it counts CODEPOINTS, so it cannot know
  // how many bytes to ask for until it has looked at them.
  if (not Wide) and (Handle <> 0) then
  begin
    if not Assigned(FOnFileData) then
      raise Exception.Create('INPUT: no file handler assigned');
    Data := IntToStr(Count);          // GETN# carries the count in and the bytes out
    ErrorCode := 0;
    FOnFileData(Self, 'GETN#', Handle, Data, ErrorCode);
    if ErrorCode <> 0 then
      raise Exception.CreateFmt('INPUT error %d reading from file %d', [ErrorCode, Handle]);
    Exit(Data);
  end;
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
    1:  Result := c_sin(X);      // NOT System.Sin - see the note by the declaration
    2:  Result := c_cos(X);
    3:  Result := c_tan(X);
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

function DataItemToInt64(const V: Variant): Int64;
// READ of a DATA item into an INTEGER variable. Its own function, not three lines inside
// ExecuteInstruction: that routine is the dispatch loop and its SIZE is a measured cost
// (see the notes on run-loop growth and dispatch alignment). bcDataReadInt is cold; a call is free here.
//
// Two things wrong with the "VarAsType(V, varInt64)" it replaces, and both were invisible until DATA
// could hold a fractional value at all:
//
//  1. It CRASHED on one. A DATA item arrives from the parser as its TEXT, so "1.75" is a string, and
//     FPC's string -> Int64 variant cast rejects a fractional string outright: "READ A%" with
//     "DATA 1.75" died with EVariantError in CLASSIC. Pre-existing, and reproduced on the archived
//     binary before blaming this change for it.
//  2. It ROUNDED. fbc TRUNCATES toward zero here and only here: 435/4 reads as 108, 217.5 as 217,
//     -435/4 as -108, while ORDINARY assignment rounds in fbc exactly as it does for us (a = 108.75
//     gives 109 on both sides). Verified against fbc, one case per row, before changing anything.
//     Truncation is also v7's rule for a "%" variable -- our own "A% = 1.75" already gives 1 -- so
//     this needs no dialect gate: both dialects want the same answer.
//
// The integral cases stay EXACT on purpose. Routing everything through Double would lose precision
// above 2^53, and a DATA item wide enough to notice is a plain integer, never a fraction.
var
  S: string;
  I64: Int64;
  FS: TFormatSettings;
begin
  if VarIsOrdinal(V) then Exit(VarAsType(V, varInt64));
  if VarIsFloat(V) then Exit(Trunc(Double(V)));
  S := Trim(VarToStr(V));
  if TryStrToInt64(S, I64) then Exit(I64);
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';          // the SOURCE's decimal point, not the machine's locale
  Result := Trunc(StrToFloatDef(S, 0.0, FS));
end;

procedure TBytecodeVM.ExecuteInstruction(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  Group: Word;
  SleepMs: Integer;
  FrameFPS, FrameTimeMs, WaitMs, ChunkMs: Integer;
  NowTick, TargetTick: QWord;
  KeyNum, KeyIdx, CharIdx: Integer;
  KeyText: string;
  KeyStr: string;       // GETKEY: the character just read (the FUNCTION form turns it into a code)
  PrintStr: string;     // PRINT USING: the formatted text, kept so the cursor column can be advanced by it
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
    12: begin ExecuteBigIntOp(Ctx, Instr); Exit; end;
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
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] - FloorDouble(Ctx.FloatRegs[Instr.Src1] / Ctx.FloatRegs[Instr.Src2]) * Ctx.FloatRegs[Instr.Src2]
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
          Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]);
      end;
    bcPowFloat: Ctx.FloatRegs[Instr.Dest] := Power(Ctx.FloatRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2]);
    bcNegFloat: Ctx.FloatRegs[Instr.Dest] := -Ctx.FloatRegs[Instr.Src1];
    // Immediate = 1: the source was UNSIGNED, so its bits are a magnitude. ⛔ This arm and the dense
    // dispatch's are TWO implementations of one opcode, and the AOT's helper road reaches THIS one:
    // correcting only the other left --aot answering -21 while the interpreter answered the right
    // number, with the AOT's refusal working perfectly all along.
    bcIntToFloat:
    begin
      // ⚠️ BITS, not a choice of three values: bit 0 = the source is UNSIGNED, bit 1 = the result goes
      // straight to binary32 (ONE rounding). They are independent, and the combination is real:
      // "Dim As Single s = u" on an unsigned u needs both, and used to take the binary32 arm alone -
      // reading the magnitude as a signed number and answering -1 where fbc answers 1.844674e+019.
      if (Instr.Immediate and 2) <> 0 then
      begin
        if (Instr.Immediate and 1) <> 0 then
          Ctx.FloatRegs[Instr.Dest] := Single(QWord(Ctx.IntRegs[Instr.Src1]))
        else
          Ctx.FloatRegs[Instr.Dest] := Single(Ctx.IntRegs[Instr.Src1]);
      end
      else if (Instr.Immediate and 1) <> 0 then
        Ctx.FloatRegs[Instr.Dest] := QWord(Ctx.IntRegs[Instr.Src1])
      else
        Ctx.FloatRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
    end;
    // The IMPLICIT float -> int conversion: FreeBASIC ROUNDS (to nearest, ties to even), it does not
    // truncate. It rounds everywhere the conversion is implicit -- assignment, argument passing, an array
    // store, an array INDEX, a FOR bound, a FUNCTION result -- so "Dim As Integer i : i = 1.5" is 2, and
    // "a(1.5)" is element 2. Truncation is what Int() and Fix() are for, and they have their own opcodes.
    // CLASSIC keeps truncating: Commodore v7 assigns 1.7 to an integer variable as 1.
    // ⛔ Immediate = 1: the DESTINATION is unsigned 64-bit, which is a different conversion above 2^63.
    // This arm and the dense dispatch's are TWO implementations of one opcode, and the AOT's helper
    // road reaches THIS one - correcting only the other is how the same bug survived a green run once.
    bcFloatToInt:
      if Instr.Immediate = 1 then
        Ctx.IntRegs[Instr.Dest] := FloatToUIntConv(Ctx.FloatRegs[Instr.Src1],
                                                   Assigned(FProgram) and FProgram.ModernMode)
      else
        Ctx.IntRegs[Instr.Dest] := FloatToIntConv(Ctx.FloatRegs[Instr.Src1],
                                                  Assigned(FProgram) and FProgram.ModernMode);
    // Numeric -> string (FreeBASIC Str() / "&" concat): no leading sign-space, unlike v7 STR$.
    bcIntToString: Ctx.StringRegs[Instr.Dest] := IntToStr(Ctx.IntRegs[Instr.Src1]);
    bcFloatToString:
      // FreeBASIC Str()/"&" concat of a float: the number with no leading sign-space and no trailing
      // field-space (FormatNumber adds both under the Commodore preset). Immediate = 1 when the value is
      // SINGLE-typed: 7 significant digits, as PRINT gives it.
      Ctx.StringRegs[Instr.Dest] := Trim(FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Src1],
                                                                       Instr.Immediate = 1));
    // CINT (round-to-even). Immediate = 1: the destination is unsigned 64-bit (CUINT/CULNGINT/CUNSG),
    // the same distinction bcFloatToInt carries - this opcode differs only in rounding in BOTH dialects.
    bcFloatRound:
      if Instr.Immediate = 1 then
        Ctx.IntRegs[Instr.Dest] := FloatToUIntConv(Ctx.FloatRegs[Instr.Src1], True)
      else
        Ctx.IntRegs[Instr.Dest] := Round(Ctx.FloatRegs[Instr.Src1]);
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
    // MODERN bit intrinsics. Immediate carries the WIDTH (32 or 64); the helpers are shared with the
    // WASM backend's reference semantics, so the two sides cannot drift on clz(0) or on the rotate's
    // modulo - the two places where an "obvious" implementation would differ.
    bcBitClz:    Ctx.IntRegs[Instr.Dest] := BitClz(Ctx.IntRegs[Instr.Src1], Instr.Immediate);
    bcBitCtz:    Ctx.IntRegs[Instr.Dest] := BitCtz(Ctx.IntRegs[Instr.Src1], Instr.Immediate);
    bcBitPopcnt: Ctx.IntRegs[Instr.Dest] := BitPopcnt(Ctx.IntRegs[Instr.Src1], Instr.Immediate);
    bcBitRotl:   Ctx.IntRegs[Instr.Dest] := BitRotl(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], Instr.Immediate);
    bcBitRotr:   Ctx.IntRegs[Instr.Dest] := BitRotr(Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], Instr.Immediate);
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
        Ctx.IntRegs[Instr.Dest] := AllocSharedRecord(Instr.Src1,
                                          Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF)
      else
        Ctx.IntRegs[Instr.Dest] := AllocRecord(Ctx, Instr.Src1,
                                          Instr.Immediate and $FFFF, (Instr.Immediate shr 32) and $FFFF);
    bcRecordNewArray:
      RecordNewArrayInit(Ctx, Ctx.ArrMap[Instr.Src1], Instr.Immediate);  // Src1=array id; Imm=packed slot counts
    bcRecordNewArrayInd:
      // Array-of-UDT MEMBER: the FArrays id is a runtime handle in IntRegs[Src1]. Imm=packed slot counts.
      RecordNewArrayInit(Ctx, MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]), Instr.Immediate);
    bcRecordNewBlock:  // Callocate(n, SizeOf(T)) of a UDT: n consecutive shared records; Dest = first handle
      Ctx.IntRegs[Instr.Dest] := AllocSharedRecordBlock(Ctx.IntRegs[Instr.Src1],
                                   Instr.Immediate and $FFFF,
                                   (Instr.Immediate shr 32) and $FFFF, (Instr.Immediate shr 48) and $FFFF);
    bcRecordReallocBlock:  // Reallocate a UDT block: Dest = the (possibly moved) first handle
      Ctx.IntRegs[Instr.Dest] := ReallocSharedRecordBlock(Ctx.IntRegs[Instr.Src1],
                                   Integer(Ctx.IntRegs[Instr.Src2]),
                                   Instr.Immediate and $FFFF,
                                   (Instr.Immediate shr 32) and $FFFF, (Instr.Immediate shr 48) and $FFFF);
    bcRecordBlockLen:  // Delete[] p: how many records the block holds (1 when it is a lone record)
      Ctx.IntRegs[Instr.Dest] := SharedRecordBlockLen(Ctx.IntRegs[Instr.Src1]);
    bcRecordFree:
      FreeSharedRecord(Ctx.IntRegs[Instr.Src1]);  // DELETE p: release the heap record (Src1=handle)
    // M5.2c: ResolveRec routes the handle to its record (per-thread heap or the shared region).
    bcRecordLoadInt:    Ctx.IntRegs[Instr.Dest] := RecFieldInt(ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1]), Instr.Immediate);
    bcRecordLoadFloat:  Ctx.FloatRegs[Instr.Dest] := RecFieldFloat(ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1]), Instr.Immediate);
    bcRecordLoadString: Ctx.StringRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate];
    bcRecordStoreInt:   RecSetFieldInt(ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1]), Instr.Immediate, Ctx.IntRegs[Instr.Src2]);
    bcRecordStoreFloat: RecSetFieldFloat(ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1]), Instr.Immediate, Ctx.FloatRegs[Instr.Src2]);
    bcRecordStoreString:ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.StringData[Instr.Immediate] := Ctx.StringRegs[Instr.Src2];
    bcRecordTypeId:     Ctx.IntRegs[Instr.Dest] := ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.TypeId;
    bcRecordSetTypeId:  ResolveRec(Ctx, Ctx.IntRegs[Instr.Src1])^.TypeId := Instr.Immediate;
    // System commands
    bcEnd:
      begin
        Ctx.Running := False;
        Ctx.Stopped := False;  // END clears stopped state
        // "End n": n is what the PROCESS answers with. 0 is both "no code" and "End 0".
        if Instr.Immediate <> 0 then FProgramExitCode := Instr.Immediate;
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
            FProgramExitCode := 1;   // fbc's runtime exits 1 on a failed ASSERT (measured, -g)
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
        //
        // ⭐ Immediate = 1 is "Err = n", which the FreeBASIC manual defines as exactly this minus the
        // raise: "Unlike Error, Err = number sets the error number without invoking an error handler."
        // One arm for both, so the two spellings cannot answer differently.
        if Instr.Immediate = 1 then
        begin
          Ctx.LastErrorCode := Ctx.IntRegs[Instr.Src1];
          Ctx.LastErrorMessage := ErrorText(Ctx.IntRegs[Instr.Src1]);
          if Ctx.LastErrorCode <> 0 then
            Ctx.LastErrorLine := FProgram.GetSourceLine(Ctx.PC);
        end
        else
          raise TExecutorRuntimeException.CreateWithCode(
            ErrorText(Ctx.IntRegs[Instr.Src1]), Ctx.IntRegs[Instr.Src1]);
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
          // fbc TRUNCATES a fractional DATA item here (unlike ordinary assignment, which rounds),
          // and a fractional item used to crash the variant cast outright. See DataItemToInt64.
          Ctx.IntRegs[Instr.Dest] := DataItemToInt64(FDataPool[Ctx.DataIndex]);
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
        // Immediate = the DATA-POOL INDEX to resume from, already resolved from the line number or the
        // label by ProcessRestore (0 = the beginning, which is what a bare RESTORE emits). It used to be
        // the raw line number and was DISCARDED here, so "RESTORE 100" and "Restore label" both reset to
        // the first item and answered from the wrong block without a word.
        Ctx.DataIndex := Instr.Immediate;
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
              // ⛔ ...and a read that can never succeed must END, not spin. With no key source (the
              // headless terminal on Unix) or with the input exhausted, this looped for ever: a
              // program doing GETKEY simply froze. fbc answers -1 in exactly this situation.
              if FInputDevice.InputExhausted then Break;
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
            if Instr.Immediate = 1 then
            begin
              // FreeBASIC's GETKEY is a FUNCTION returning the key CODE, and -1 when no key can be
              // had. The Commodore statement form below answers with a character instead: one arm,
              // two dialects, and the Immediate is which.
              KeyStr := '';
              if Ctx.Running and FInputDevice.HasChar then KeyStr := FInputDevice.GetLastChar;
              if KeyStr = '' then Ctx.IntRegs[Instr.Dest] := -1
              else Ctx.IntRegs[Instr.Dest] := Ord(KeyStr[1]);
            end
            else if Ctx.Running then
              Ctx.StringRegs[Instr.Dest] := FInputDevice.GetLastChar
            else
              Ctx.StringRegs[Instr.Dest] := '';
          finally
            FInputDevice.DisableTextInput;
          end;
        end
        else if Instr.Immediate = 1 then
          Ctx.IntRegs[Instr.Dest] := -1
        else
          Ctx.StringRegs[Instr.Dest] := '';
      end;
    // Formatted output
    // ⛔ PRINT USING ADVANCES THE CURSOR COLUMN like any other output. All four arms wrote straight to
    // the device and left Ctx.CursorCol alone, so a following comma computed its tab zone from a column
    // that did not include what USING had just printed. "Print Using ""###: ""; i;" then "Print s," put
    // the next zone 5 columns too far - the whole width of the USING output - on EVERY item of the row.
    // The tracked column is also what POS() and CSRLIN answer, so it was wrong there too.
    bcPrintUsing:
      begin
        // PRINT USING format$; value
        // Src1 = format string register, Src2 = value register
        if Assigned(FOutputDevice) then
        begin
          // Src2 is a FLOAT value here; the exact-integer form is bcPrintUsingInt (below).
          PrintStr := FormatUsing(Ctx.StringRegs[Instr.Src1], Ctx.FloatRegs[Instr.Src2], False, 0);
          FOutputDevice.Print(PrintStr);
          AdvancePrintCol(Ctx, Length(PrintStr));
        end;
      end;
    bcPrintUsingInt:
      // PRINT USING with an EXACT integer value: Src1 = format string, Src2 = int value. A LongInt beyond
      // 2^53 keeps every digit instead of being rounded through a Double (Pell's 2469645423824185801).
      begin
        if Assigned(FOutputDevice) then
        begin
          PrintStr := FormatUsing(Ctx.StringRegs[Instr.Src1], 0.0, True, Ctx.IntRegs[Instr.Src2]);
          FOutputDevice.Print(PrintStr);
          AdvancePrintCol(Ctx, Length(PrintStr));
        end;
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
        begin
          PrintStr := FormatUsingRuntime(Ctx.StringRegs[Instr.Src1]);
          FOutputDevice.Print(PrintStr);
          AdvancePrintCol(Ctx, Length(PrintStr));
        end
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
  ArrayIdx: Integer;   // bcStrMidAssignArr: the target array...
  LinearIdx: Integer;  // ...and the element inside it
begin
  // Superinstructions use sub-opcode (low byte) for dispatch
  // Full opcode is 0xC800 + SubOp (group 200)
  SubOp := Instr.OpCode and $FF;

  // SUPER_DIAG=1: count how often each sub-opcode is reached HERE, that is, through the SECOND
  // dispatch. Every one of these paid the main dispatch already; the count is what says which of
  // them is worth flattening into it. Naming candidates by reading the list is how the last two
  // attempts at this kind of work produced nothing (see the inliner) - the ranking is the answer.
  if GSuperDiag then Inc(GSuperCount[SubOp]);

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
    6: // bcBranchEqFloat
      if Ctx.FloatRegs[Instr.Src1] = Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    7: // bcBranchNeFloat
      if Ctx.FloatRegs[Instr.Src1] <> Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    8: // bcBranchLtFloat
      if Ctx.FloatRegs[Instr.Src1] < Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    9: // bcBranchGtFloat
      if Ctx.FloatRegs[Instr.Src1] > Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    10: // bcBranchLeFloat
      if Ctx.FloatRegs[Instr.Src1] <= Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;
    11: // bcBranchGeFloat
      if Ctx.FloatRegs[Instr.Src1] >= Ctx.FloatRegs[Instr.Src2] then
        Ctx.PC := Instr.Immediate - 1;

    // Fused arithmetic-to-dest (Int) - sub-opcodes 20-22
    12: // bcAddIntTo: r[dest] += r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] + Ctx.IntRegs[Instr.Src1];
    13: // bcSubIntTo: r[dest] -= r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] - Ctx.IntRegs[Instr.Src1];
    14: // bcMulIntTo: r[dest] *= r[src1]
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Dest] * Ctx.IntRegs[Instr.Src1];

    // Fused arithmetic-to-dest (Float) - sub-opcodes 30-33
    15: // bcAddFloatTo: r[dest] += r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] + Ctx.FloatRegs[Instr.Src1];
    16: // bcSubFloatTo: r[dest] -= r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] - Ctx.FloatRegs[Instr.Src1];
    17: // bcMulFloatTo: r[dest] *= r[src1]
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] * Ctx.FloatRegs[Instr.Src1];
    18: // bcDivFloatTo: r[dest] /= r[src1]
      if Ctx.FloatRegs[Instr.Src1] <> 0.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] / Ctx.FloatRegs[Instr.Src1]
      else
        Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Dest], Ctx.FloatRegs[Instr.Src1]);

    // Fused constant arithmetic (Int) - sub-opcodes 40-42
    19: // bcAddIntConst: r[dest] = r[src1] + immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] + Instr.Immediate;
    20: // bcSubIntConst: r[dest] = r[src1] - immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] - Instr.Immediate;
    21: // bcMulIntConst: r[dest] = r[src1] * immediate
      Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1] * Instr.Immediate;

    // Fused constant arithmetic (Float) - sub-opcodes 50-53
    22: // bcAddFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] + Double(Pointer(@Instr.Immediate)^);
    23: // bcSubFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] - Double(Pointer(@Instr.Immediate)^);
    24: // bcMulFloatConst
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Double(Pointer(@Instr.Immediate)^);
    25: // bcDivFloatConst
      if Double(Pointer(@Instr.Immediate)^) <> 0.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] / Double(Pointer(@Instr.Immediate)^)
      else
        Ctx.FloatRegs[Instr.Dest] := DivZeroFloat(Ctx.FloatRegs[Instr.Src1], Double(Pointer(@Instr.Immediate)^));

    // Fused compare-zero-and-branch (Int) - sub-opcodes 60-61
    26: // bcBranchEqZeroInt
      if Ctx.IntRegs[Instr.Src1] = 0 then
        Ctx.PC := Instr.Immediate - 1;
    27: // bcBranchNeZeroInt
      if Ctx.IntRegs[Instr.Src1] <> 0 then
        Ctx.PC := Instr.Immediate - 1;

    // Fused compare-zero-and-branch (Float) - sub-opcodes 70-71
    28: // bcBranchEqZeroFloat
      if Ctx.FloatRegs[Instr.Src1] = 0.0 then
        Ctx.PC := Instr.Immediate - 1;
    29: // bcBranchNeZeroFloat
      if Ctx.FloatRegs[Instr.Src1] <> 0.0 then
        Ctx.PC := Instr.Immediate - 1;

    // Fused array-store-constant - sub-opcodes 80-82. Bounds-guarded to match the base ExecuteArrayOp
    // store path: MODERN drops an out-of-bounds store (memory-safe), CLASSIC/--bounds-check raises.
    30: // bcArrayStoreIntConst
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]] := Instr.Immediate;
    31: // bcArrayStoreFloatConst
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        FArrays[Ctx.ArrMap[Instr.Src1]].FloatData[Ctx.IntRegs[Instr.Src2]] := Double(Pointer(@Instr.Immediate)^);
    32: // bcArrayStoreStringConst
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        FArrays[Ctx.ArrMap[Instr.Src1]].StringData[Ctx.IntRegs[Instr.Src2]] := FProgram.StringConstants[Instr.Immediate];

    // Fused loop increment-and-branch (Int) - sub-opcodes 90-93
    33: // bcAddIntToBranchLe: r[dest] += r[src1]; if (r[dest] <= r[src2]) goto target
      begin
        Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] <= Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    34: // bcAddIntToBranchLt: r[dest] += r[src1]; if (r[dest] < r[src2]) goto target
      begin
        Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] < Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    35: // bcSubIntToBranchGe: r[dest] -= r[src1]; if (r[dest] >= r[src2]) goto target
      begin
        Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] >= Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;
    36: // bcSubIntToBranchGt: r[dest] -= r[src1]; if (r[dest] > r[src2]) goto target
      begin
        Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
        if Ctx.IntRegs[Instr.Dest] > Ctx.IntRegs[Instr.Src2] then
          Ctx.PC := Instr.Immediate - 1;
      end;

    // FMA (Fused Multiply-Add) - sub-opcodes 100-103
    37: // bcMulAddFloat: dest = c + a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] + Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    38: // bcMulSubFloat: dest = c - a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] - Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    39: // bcMulAddToFloat: dest += a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] + Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];
    40: // bcMulSubToFloat: dest -= a*b
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Dest] - Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2];

    // Array Load + Arithmetic - sub-opcodes 110-112. Bounds-guarded: an out-of-bounds read yields the
    // element default (0.0) in MODERN, matching the base ExecuteArrayOp load path; CLASSIC raises.
    41: // bcArrayLoadAddFloat: dest = acc + arr[idx]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] + FArrays[Ctx.ArrMap[Instr.Src1]].FloatData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate];
    42: // bcArrayLoadSubFloat: dest = acc - arr[idx]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate] - FArrays[Ctx.ArrMap[Instr.Src1]].FloatData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate];
    43: // bcArrayLoadDivAddFloat: dest = acc + arr[idx] / denom
      begin
        if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
          ElemVal := FArrays[Ctx.ArrMap[Instr.Src1]].FloatData[Ctx.IntRegs[Instr.Src2]]
        else
          ElemVal := 0.0;
        if Abs(Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF]) < 1e-300 then
          Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
            DivZeroFloat(ElemVal, Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF])   // MODERN: IEEE; CLASSIC: error
        else
          Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Immediate and $FFFF] +
            ElemVal / Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF];
      end;

    // Square-Sum patterns - sub-opcodes 120-121
    44: // bcSquareSumFloat: dest = x*x + y*y
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src1] +
                                Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Src2];
    45: // bcAddSquareFloat: dest = sum + x*x
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Src2];

    // Mul-Mul and Add-Sqrt - sub-opcodes 130-131
    46: // bcMulMulFloat: dest = a*b*c
      Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1] * Ctx.FloatRegs[Instr.Src2] * Ctx.FloatRegs[Instr.Immediate];
    47: // bcAddSqrtFloat: dest = sqrt(a+b)
      Ctx.FloatRegs[Instr.Dest] := SqrtFloat(Ctx.FloatRegs[Instr.Src1] + Ctx.FloatRegs[Instr.Src2]);

    // Array Load + Branch - sub-opcodes 140-141. Bounds-guarded: an out-of-bounds read is treated as the
    // element default 0 in MODERN (matching the base load path) — NZ does not branch, Z branches; CLASSIC raises.
    48: // bcArrayLoadIntBranchNZ: if arr[idx] <> 0 goto target
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
      begin
        if FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]] <> 0 then
          Ctx.PC := Instr.Immediate - 1;
      end;
    49: // bcArrayLoadIntBranchZ: if arr[idx] = 0 goto target
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
      begin
        if FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]] = 0 then
          Ctx.PC := Instr.Immediate - 1;
      end
      else
        Ctx.PC := Instr.Immediate - 1;   // OOB read = 0 -> zero-branch taken

    // Array Reverse Range - sub-opcode 156
    50: // bcArrayReverseRange: reverse arr[start..end-1] in-place
      begin
        Ctx.StartIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.EndIdx := Ctx.IntRegs[Instr.Dest] - 1;
        Ctx.ArrIdxTmp := Ctx.ArrMap[Instr.Src1];
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
    51: // bcArrayShiftLeft: shift left and rotate first to end+1
      begin
        Ctx.StartIdx := Ctx.IntRegs[Instr.Src2];
        Ctx.EndIdx := Ctx.IntRegs[Instr.Dest];
        Ctx.ArrIdxTmp := Ctx.ArrMap[Instr.Src1];
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
    52: // bcStrConcatCharAt: Dest := Src1 + tab[k], with no one-character string ever built.
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
    53: // bcStrAppendMapped: Dest += Src2[Ord(Src1[Immediate]) + 1]
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
    54: // bcStrMidAssign: overwrite Length(Src2) bytes of Dest starting at Immediate, IN PLACE
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
    71: // bcStrMidAssignArr: the same statement when the target is an ARRAY ELEMENT
      begin
        // ⛔ WHY THIS IS NOT sub-opcode 54 ON A LOADED REGISTER. UniqueString is free at reference
        // count 1 and a FULL COPY at 2, and loading an array element into a register makes it 2 by
        // construction - so the register form copies the whole string on every assignment. Measured
        // 19 Aug 2026: a 400,000-character SHARED string filled one byte at a time took 33.9 s that
        // way against 28 ms for the identical code on a local, and the cost grew with the SQUARE of
        // the length. Writing the slot directly keeps the count at 1 and the write free.
        // Src1 = array id, Src2 = the linear index, Dest = the replacement (READ), Immediate = start.
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
        LinearIdx := Ctx.IntRegs[Instr.Src2];
        if ArrayBoundsOK(ArrayIdx, LinearIdx) then
        begin
          MidRepl := Ctx.StringRegs[Instr.Dest];
          CharPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
          SrcLen := Length(FArrays[ArrayIdx].StringData[LinearIdx]);
          // Same clamping as sub-opcode 54, in the same order: a start past the end writes nothing,
          // and the replacement is already capped to len by the ssaStrLeft the lowering emits.
          if (CharPos >= 1) and (CharPos <= SrcLen) then
          begin
            CharVal2 := Length(MidRepl);
            if CharVal2 > SrcLen - CharPos + 1 then CharVal2 := SrcLen - CharPos + 1;
            if CharVal2 > 0 then
            begin
              UniqueString(FArrays[ArrayIdx].StringData[LinearIdx]);
              Move(MidRepl[1], FArrays[ArrayIdx].StringData[LinearIdx][CharPos], CharVal2);
            end;
          end;
        end;
      end;

    // Array Swap (Int) - sub-opcode 250. Bounds-guarded: skip the swap if either index is out of range (MODERN); CLASSIC raises.
    55: // bcArraySwapInt: swap arr[idx1] and arr[idx2]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) and
         ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Dest]) then
      begin
        Ctx.SwapTempInt := FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]];
        FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Dest]];
        FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Dest]] := Ctx.SwapTempInt;
      end;

    // Self-increment/decrement (Int) - sub-opcodes 251-252
    56: // bcAddIntSelf: r[dest] += r[src1]
      Inc(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);
    57: // bcSubIntSelf: r[dest] -= r[src1]
      Dec(Ctx.IntRegs[Instr.Dest], Ctx.IntRegs[Instr.Src1]);

    // Array Load to register (Int) - sub-opcode 253. Bounds-guarded: OOB read yields default 0 (MODERN); CLASSIC raises.
    58: // bcArrayLoadIntTo: r[dest] = arr[src1][r[src2]]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
        Ctx.IntRegs[Instr.Dest] := FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]]
      else
        Ctx.IntRegs[Instr.Dest] := 0;

    // Array Copy Element - sub-opcode 254. Bounds-guarded: OOB store dropped, OOB source reads default 0 (MODERN); CLASSIC raises.
    59: // bcArrayCopyElement: arr_dest[idx] = arr_src[idx]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Dest], Ctx.IntRegs[Instr.Src2]) then
      begin
        if ArrayBoundsOK(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2]) then
          FArrays[Ctx.ArrMap[Instr.Dest]].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Ctx.ArrMap[Instr.Src1]].IntData[Ctx.IntRegs[Instr.Src2]]
        else
          FArrays[Ctx.ArrMap[Instr.Dest]].IntData[Ctx.IntRegs[Instr.Src2]] := 0;
      end;

    // Array Move Element - sub-opcode 255. Bounds-guarded like 254.
    60: // bcArrayMoveElement: arr[dest_idx] = arr[src_idx]
      if ArrayBoundsOK(Ctx.ArrMap[Instr.Dest], Ctx.IntRegs[Instr.Src2]) then
      begin
        if ArrayBoundsOK(Ctx.ArrMap[Instr.Dest], Ctx.IntRegs[Instr.Src1]) then
          FArrays[Ctx.ArrMap[Instr.Dest]].IntData[Ctx.IntRegs[Instr.Src2]] := FArrays[Ctx.ArrMap[Instr.Dest]].IntData[Ctx.IntRegs[Instr.Src1]]
        else
          FArrays[Ctx.ArrMap[Instr.Dest]].IntData[Ctx.IntRegs[Instr.Src2]] := 0;
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

{$IFDEF HOT_C}
{ The hot arithmetic/branch opcodes, compiled by a C compiler rather than by FPC. The reason is
  measured and is not a preference: the same dispatch loop - same arms, same values live across it -
  runs in 253 ms under gcc -O2 and 443 under FPC on this machine, and no FPC optimisation level
  closes any of it. gcc keeps the hot pointers in registers where FPC spills them. See src/hotdisp.c.

  The record layouts match exactly: TBytecodeInstruction is a packed record of four Words and an
  Int64, which is C's { uint16_t x4; int64_t } with no padding on either side. cdecl is the right
  convention on both platforms - on win64 FPC's cdecl IS the Microsoft x64 ABI that MinGW-w64 emits. }
{$L hotdisp.o}
{ ⛔ AND THE OBJECT IS NO LONGER FREESTANDING. It calls sin/cos/tan since the trigonometry moved to
  the libc on 22 Aug 2026 - "nm -u" on it lists exactly those three. On Linux the Pascal RTL has
  already pulled libm, so nothing was needed and nothing was noticed; on win64 the link failed with
  "Undefined symbol: sin", and it failed for MONTHS unnoticed because build.ps1 does not implement
  HOT_C at all and nobody cross-built for win64 in between. The comment above still claimed the
  object was freestanding and that win64 had been verified - that verification predates the change. }

{ How many BACK EDGES one stay inside the C loop may take before it hands the PC back so the caller
  can pump events. Spent per ITERATION of a BASIC loop, not per instruction: the whole point is that
  a program with nobody to pump for never pays for this. At the couple of nanoseconds an iteration of
  a covered loop costs, 200 000 back edges is a fraction of a millisecond of unresponsiveness - far
  below a frame - while being long enough that the exit is nowhere near a hot path. }
const HOT_BACKEDGE_BUDGET = 200000;

function sedai_hot_run(prog: PBytecodeInstruction; ireg: PInt64; freg: PDouble;
                       pc, count: LongInt; tv: Int64;
                       arrdesc: Pointer; flags: LongInt;
                       xi: PInt64; xf: PDouble; hidx: PWord;
                       recdesc: PInt64; backedge_budget: LongInt;
                       gfxdesc: PInt64): LongInt; cdecl; external;
{ The opcode list in DISPATCH-TABLE order, published by the C file so that nothing here holds a
  second copy of it. Entry j is run by arm j, which is what makes FHotOpBase an index. }
function sedai_hot_ops(out list: PWord): LongInt; cdecl; external;
{$ENDIF}

{ EnsureDenseOps - decode-once dense dispatch table (VM perf plan, milestone M2).
  Translate every instruction's 16-bit (group.sub) opcode to its dense linear index ONCE, so the hot
  loop dispatches on a single compact case (no per-instruction group extraction / superinstruction
  branch). Rebuilt only when the loaded program changes. The on-file bytecode and the in-memory
  TBytecodeInstruction.OpCode are left untouched -- serialization and disassembly are unaffected. }
procedure TBytecodeVM.EnsureDenseOps;
type
  PBytecodeInstr = ^TBytecodeInstruction;
  {$IFDEF HOT_C}
  TWordArr = array[0..High(Word)] of Word;
  PWordArr = ^TWordArr;
  {$ENDIF}
var
  i, n: Integer;
  {$IFDEF HOT_C}
  j, HotOpN: Integer;
  HotOpList: PWord;
  {$ENDIF}
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
  {$IFDEF HOT_C}
  // Per PC: WHICH C arm runs this instruction (1-based, 0 = none), so the C loop dispatches on an
  // index instead of decoding the opcode. Answering it with an array read costs a load; answering
  // it by calling C and being refused costs a call.
  SetLength(FHotOpBase, n);
  SetLength(FHotOp, n);
  HotOpN := sedai_hot_ops(HotOpList);
  if Ins <> nil then
    for i := 0 to n - 1 do
    begin
      FHotOpBase[i] := 0;
      for j := 0 to HotOpN - 1 do
        if PWordArr(HotOpList)^[j] = Ins[i].OpCode then begin FHotOpBase[i] := Word(j + 1); Break; end;
      FHotOp[i] := 0;
    end;
  FHotOpEnabled := False;
  {$ENDIF}
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

// The one-instruction interpreter helper, defined further down next to the rest of the AOT runtime.
// Declared here because the loop JIT is handed its address (the J14 helper route) and is compiled
// before it.
function AotExecOne(VMSelf, CtxObj: Pointer; PC: PtrInt; AotCtx: PAotCtx): PtrInt; cdecl; forward;

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
  // A3-i: ONE data pointer now, the byte image. Both offsets are handed the same field so the two
  // emitters keep their existing signatures while the storage is one array - see GetRecordLayout,
  // where the same thing is said at more length.
  RIntOff := Integer(PtrUInt(@RecTmp.Bytes) - PtrUInt(@RecTmp));
  RFloatOff := RIntOff;
  n := FProgram.GetInstructionCount;
  for i := 0 to High(FNativeLoops) do FNativeLoops[i].Free;
  SetLength(FNativeLoops, 0);
  SetLength(FNativeLoops, n);   // all nil
  if n = 0 then Exit;
  Ins := PBcInstr(FProgram.GetInstructionsPtr);
  // The leaf primitives a compiled loop may bake. Filled here rather than per Run because a loop is
  // compiled once: everything in it is fixed for the loaded program, dialect variants included.
  FillChar(FJitPrimCtx, SizeOf(FJitPrimCtx), 0);
  SetAotPrimitives(FJitPrimCtx);

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
                         RecSz, RIntOff, RFloatOff,
                         // J14: the channel to the interpreter the JIT never had. An instruction with
                         // no native form is now run by AotExecOne - the same helper the AOT uses -
                         // instead of costing the whole loop. Self is the VM that owns this code, so
                         // baking it is safe; the per-thread half (the context) is a call argument.
                         @AotExecOne, Pointer(Self),
                         // J15: the string family as leaf calls. The primitive addresses are baked
                         // from this table at compile time; the string BANK is per-context, so its
                         // field offset is passed instead and read at run time - a worker uses its own.
                         @FJitPrimCtx,
                         Integer(PtrUInt(@FCtx.StringRegs) - PtrUInt(Pointer(FCtx))));
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
  VM.EnsureArrDesc(AotCtx);
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
// Counts = byteSize (A3-i: the record's live image size, where this used to be two slot counts, the
// second of which is now always zero). Imm is the bytecode Immediate verbatim: string slots in bits
// 0..15, type id in bits 32..47, "allocate in the shared region" in bit 48.
var
  ByteSize, StrC, TypeId: Integer;
begin
  ByteSize := Integer(Counts and $FFFFFFFF);
  StrC   := Integer(Imm and $FFFF);
  TypeId := Integer((Imm shr 32) and $FFFF);
  if (Imm shr 48) and 1 <> 0 then
    Result := PtrInt(TBytecodeVM(VMSelf).AllocSharedRecord(ByteSize, StrC, TypeId))
  else
    Result := PtrInt(TBytecodeVM(VMSelf).AllocRecord(TExecutionContext(CtxObj), ByteSize, StrC, TypeId));
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

{ ===== C7: PRINT's two bookkeeping opcodes as leaf calls =====

  ⭐ Why these two and not the print ITEMS. `Print a;` is THREE bytecode instructions -
  bcPrintString, bcPrintSemicolon, bcPrintEnd - and without a native lowering each one is a full
  AotExecOne round trip: flush every homed register, call, reload them, compare the returned PC.
  Measured 17 Aug 2026 on 800 000 statements: an AOT print ITEM costs 145 ns against the
  interpreter's 65, so compiled code was 2.2x SLOWER than interpreted code at printing - and the
  helper call is where the difference lives (the same ~76 ns per helper call that put ABS and SGN
  in IsB1Op). Two of the three instructions do almost nothing: in every dialect preset in the tree
  SemicolonAction is saNoSpace, and PrintEnd is one virtual call that resets C128 reverse mode.

  They are safe as LEAF calls for the three reasons the C5/C6 primitives are: neither moves the PC,
  neither can invalidate the array-descriptor table, and neither can ask to leave for the
  interpreter. The print ITEMS stay on the helper - they carry the CMD-redirection branch, which
  can raise a BASIC error, and an exception must not unwind through native frames.

  ⛔ Each is an EXACT transcription of its ExecuteIOOp arm; the interpreter stays the definition of
  the behaviour. In particular the SemicolonAction case is reproduced in full rather than collapsed
  to "do nothing", because the property is writable at run time. }

procedure AotPrintSemicolon(VMSelf, CtxObj: Pointer); cdecl;
var
  VM: TBytecodeVM;
begin
  VM := TBytecodeVM(VMSelf);
  if not Assigned(VM.FOutputDevice) then Exit;
  case VM.FConsoleBehavior.SemicolonAction of
    saNoSpace: ;
    saSpaceAfter, saSpaceBoth:
      begin
        VM.FOutputDevice.Print(' ');
        VM.AdvancePrintCol(TExecutionContext(CtxObj), 1);
      end;
    saSpaceBefore: ;
  end;
end;

{ C7b: a PRINT ITEM as a CONDITIONAL leaf call.

  The two bookkeeping opcodes above are unconditional leaves; the item is not, and the reason is one
  branch: with CMD redirection active the arm hands the text to the file layer, which RAISES a BASIC
  error on a bad handle - and an exception must not unwind through a compiled frame that carries no
  unwinding information. So the primitive answers "not handled" for that case and the emitted code
  falls through to the ordinary helper call, which runs the same arm on an interpreter frame.

  Everything else is an exact transcription of the bcPrintString / bcPrintStringLn arms. Measured
  worth: the helper round trip costs ~53 ns more than a leaf call, and reverse-complement's write
  phase makes 1.67 million print items.

  ⛔ The operand is a STRING bank slot, never a homed machine register, and the arm writes no int or
  float register either - that is what makes the fast path safe without the helper's flush/reload.

  Result: 0 = done, 1 = not handled (run the helper). }
function AotPrintString(VMSelf, CtxObj: Pointer; SrcSlot, WithNewline: PtrInt): PtrInt; cdecl;
var
  VM: TBytecodeVM;
  C: TExecutionContext;
  S: string;
begin
  VM := TBytecodeVM(VMSelf);
  // CMD redirection: the only reachable raise on this path. Hand it back.
  if (VM.FCmdHandle > 0) and Assigned(VM.FOnFileData) then Exit(1);
  Result := 0;
  if not Assigned(VM.FOutputDevice) then Exit;
  C := TExecutionContext(CtxObj);
  S := VM.FConsoleBehavior.FormatString(C.StringRegs[SrcSlot]);
  VM.FOutputDevice.Print(S);
  if WithNewline <> 0 then
  begin
    VM.FOutputDevice.NewLine;          // NewLine already calls Present
    C.CursorCol := 0;
    Inc(C.CursorRow);                  // CSRLIN: a print newline advances the text row
  end
  else
    VM.AdvancePrintCol(C, Length(S));
end;

// ⛔ THE BATTERY, VERIFIED BACKWARDS. The CMD-redirection fallback above is the one branch nothing
// in the corpus exercises - grep finds no program that redirects PRINT to a file - so a defect in it
// would sit behind a green net. AOT_PRINTSTR=2 installs THIS instead: it always answers "not
// handled", so EVERY print in every program takes the fallback road and aot_validate compares the
// result against the interpreter. Green with it set is what proves the road, not the absence of a
// test that walks it.
function AotPrintStringForceHelper(VMSelf, CtxObj: Pointer; SrcSlot, WithNewline: PtrInt): PtrInt; cdecl;
begin
  Result := 1;
end;

procedure AotPrintEnd(VMSelf: Pointer); cdecl;
var
  VM: TBytecodeVM;
begin
  VM := TBytecodeVM(VMSelf);
  if Assigned(VM.FOutputDevice) then
    VM.FOutputDevice.ResetPrintState;
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
    begin
      SetLength(C.FrameMarks, C.FrameMarkTop + 256);
      SetLength(C.FrameMarkArrSave, C.FrameMarkTop + 256);   // cresce IN PASSO: nessun controllo in piu'
    end;
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
  C.FrameMarkArrSave[C.FrameMarkTop] := C.ArrPrivSaveTop;
  Inc(C.FrameMarkTop);
  C.RegDeltaI := SaveHw - FBLo;
  C.RegHwI := SaveHw + FBHi;
  C.IntRegs := @C.IntRegsMem[C.RegDeltaI];
  C.CallStack[C.CallStackPtr] := Integer(BcCallSubPC) + 1;
  Inc(C.CallStackPtr);
  VM.EnsureArrDesc(AotCtx);
  Inc(C.AotCallDepth);
  RetPC := TNativeFuncFn(Fn.Ptr)(C.IntRegs, PInt64(@C.FloatRegs[0]), AotCtx);
  Dec(C.AotCallDepth);
  VM.EnsureArrDesc(AotCtx);
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
  // ⭐ SENZA FRAME DI ECCEZIONE quando niente puo' allocare. Il try qui sotto esiste per una
  // allocazione che fallisce, ma costa un setjmp e la catena delle eccezioni A OGNI CHIAMATA -
  // 11,3% del tempo AOT su binary-trees, campionato. FramePushIsAllocFree (accanto a FramePush,
  // apposta) risponde True solo se il ramo veloce si applica E nessuna delle tre capacita' va
  // cresciuta: allora questo blocco e' identico a quello sotto, meno la protezione che non serve.
  if VM.FramePushIsAllocFree(C, Integer(CalleeEntryPC)) and GNoExcFrame then
  begin
    VM.FramePush(C, Integer(CalleeEntryPC), Integer(BcCallSubPC));
    if Fine then Tb := AotRdTsc;
    C.CallStack[C.CallStackPtr] := Integer(BcCallSubPC) + 1;
    Inc(C.CallStackPtr);
    if Fine then Tc := AotRdTsc;
  end
  else
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
  VM.EnsureArrDesc(AotCtx);
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
  VM.EnsureArrDesc(AotCtx);
  if Fine then Td := AotRdTsc;
  if RetPC < 0 then Exit(RetPC);   // helper sentinel from inside the callee: frame stays pushed
  if (RetPC < VM.FProgram.GetInstructionCount) and
     (PInstr(VM.FProgram.GetInstructionsPtr)[RetPC].OpCode = bcReturnSub) then
  begin
    Dec(C.CallStackPtr);           // the interpreter's bcReturnSub order: pop, then FramePop
    VM.FramePop(C);
    // ⛔ AND AGAIN AFTER THE POP. The refresh above happens while the callee's arrays are still
    // installed; FramePop then restores this frame's own proc-local arrays, which moves their
    // storage a second time. Refreshing only before the pop left the caller reading the CALLEE's
    // array: rec(3) answered 138 where the interpreter and the JIT both said 198, with the save and
    // restore counts IDENTICAL on all three - the bookkeeping was right and the descriptor was stale.
    VM.EnsureArrDesc(AotCtx);
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
  // Logical id, baked into the emitted code: resolve it against the ACTIVE context, which is this
  // thread's. Unlike the typed accessors these two never see the descriptor table, so the mapping the
  // table would have done for them has to happen here.
  ArrIdx := VM.MapArrDyn(VM.ActiveCtx, ArrIdx);
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
  // Logical id, baked into the emitted code: resolve it against the ACTIVE context, which is this
  // thread's. Unlike the typed accessors these two never see the descriptor table, so the mapping the
  // table would have done for them has to happen here.
  ArrIdx := VM.MapArrDyn(VM.ActiveCtx, ArrIdx);
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
function ParseLeadingInt64(const S: string; DecWidth: Integer): Int64; forward;

// REGEXREPL: 1 = build the replacement in one measured allocation (the default), 0 = the library's
// own quadratic Replace. -1 = the environment has not been read yet. Read once, on the first
// substitution of the run.
var
  GRegexReplLinear: Integer = -1;
  // Our own automaton (SedaiRegexEngine) handles the patterns it can compile and hands the rest to
  // the library. ON by default since 2026-08-03; REGEX_ENGINE=tregexpr is the A/B that puts every
  // call back on FPC's RegExpr.
  //
  // What earned the default, measured on one binary against the library it replaces: regex-redux on
  // real input 4.8x faster and byte-identical; short subjects with a reused pattern 1.6-1.9x faster.
  // The one shape where a DFA cannot win is a pattern built from data and used once - it pays the
  // construction up front to be fast later, and there is no later - and that is now 1.37x rather
  // than 14.7x, because past the cache cap AcquirePattern declines short subjects outright.
  //
  // ⚠️ The engine is leftmost-LONGEST where the library is leftmost-FIRST. CompilePattern refuses
  // any pattern where that could show, so the fallback is a correctness boundary and not a
  // convenience: see SedaiRegexEngine's header for the two sufficient conditions.
  GRegexOwnEngine: Integer = -1;

  // ⛔ DIAGNOSTIC ONLY - REGEX_BISECT makes REGEXCOUNT return WRONG ANSWERS on purpose. It exists
  // because a cache HIT on a 50-byte subject measures 1.49 us per call while the same function
  // exiting at its guard measures 0.08, and neither the cache walk nor the pattern's complexity
  // explains the difference. Timing a whole call cannot say which half it is in, so this cuts the
  // path at two points and lets subtraction answer:
  //   2 = return right after the engine check   -> dispatch + call + guard
  //   1 = + AcquirePattern, but no scan         -> adds the cache lookup
  //   0 = normal                                -> adds RegexEngineCount
  // Never set outside a measurement.
  GRegexBisect: Integer = -1;

function RegexBisectLevel: Integer;
begin
  if GRegexBisect < 0 then
    GRegexBisect := StrToIntDef(GetEnvironmentVariable('REGEX_BISECT'), 0);
  Result := GRegexBisect;
end;

function RegexUseOwnEngine: Boolean;
begin
  if GRegexOwnEngine < 0 then
    if LowerCase(GetEnvironmentVariable('REGEX_ENGINE')) = 'tregexpr' then GRegexOwnEngine := 0
    else GRegexOwnEngine := 1;
  Result := GRegexOwnEngine = 1;
end;

function RegexCountMatches(const S, Pattern: string): Int64;
// REGEXCOUNT: how many NON-OVERLAPPING matches of Pattern are in S. Backed by FPC's own RegExpr, so a
// program gets a real regex engine rather than something hand-rolled - the point of having it at all.
// A malformed pattern answers 0 rather than aborting the program, matching how the string builtins
// around it treat bad input.
var
  R: TRegExpr;
  RX: TCompiledRegex;
  RXOwned: Boolean;
begin
  Result := 0;
  // ⚠️ An empty SUBJECT answers 0, and that is a decision rather than an oversight. PCRE2 answers 1
  // for "a*" against "" (the empty match at position zero), and our own engine can do the same - but
  // FPC's RegExpr returns no match for ANY pattern on an empty subject, and it is the engine that
  // answers every pattern the fast one declines. Letting the fast path be right here would make the
  // ENGINE CHOICE VISIBLE, and "the choice never changes the answer" is the property this whole
  // design rests on. One uniform answer beats one correct answer and one wrong one.
  // 🥅 Recorded, with the divergence, in job/tests/bas/bug_regex_empty_subject.bas and BASIC.md.
  if (Pattern = '') or (S = '') then Exit;
  if RegexUseOwnEngine then
  begin
    if RegexBisectLevel = 2 then Exit;          // ⛔ diagnostic, see GRegexBisect
    // Borrowed from the cache unless Owned: see AcquirePattern for why the
    // cache never evicts and why that is what makes the borrow safe. Length(S)
    // lets it decline a bargain that would not pay off - see there.
    RX := AcquirePattern(Pattern, RXOwned, Length(S));
    if RegexBisectLevel = 1 then
    begin
      if RXOwned then RX.Free;                  // ⛔ diagnostic, see GRegexBisect
      Exit;
    end;
    // ⚡ The BORROWED case takes no try/finally, and that is not a micro-tidy: FPC's try..finally on
    // win64 installs a setjmp-style frame, which measured ~0.7 us per call here - half the cost of a
    // cache hit, on a path whose actual work is a table walk. A borrowed pattern is owned by the
    // cache and has nothing to free, so the frame was protecting a cleanup that does not exist.
    // Only the owned case, which is the rare one, still needs it.
    if RX <> nil then
    begin
      if not RXOwned then Exit(RegexEngineCount(RX, S));
      try
        Exit(RegexEngineCount(RX, S));
      finally
        RX.Free;
      end;
    end;
    // nil = a construct outside the regular subset, or a per-call compile the subject was too short
    // to justify; fall through to the library.
  end;
  // ⛔ Except where the library would take the process with it: see PatternKillsLibrary.
  if PatternKillsLibrary(Pattern) then Exit;
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
  RX: TCompiledRegex;
  RXOwned: Boolean;
begin
  Result := S;
  if Pattern = '' then Exit;
  if RegexUseOwnEngine then
  begin
    RX := AcquirePattern(Pattern, RXOwned, Length(S));
    // No frame on the BORROWED path - the cache owns it and there is nothing to free. See the same
    // spot in RegexCountMatches for why that is worth a branch: a try..finally frame measured ~0.7 us.
    if RX <> nil then
    begin
      if not RXOwned then Exit(RegexEngineReplace(RX, S, Repl));
      try
        Exit(RegexEngineReplace(RX, S, Repl));
      finally
        RX.Free;
      end;
    end;
  end;
  // ⛔ Same guard as RegexCountMatches: the library dies on a lazy quantifier over a nullable
  // operand, so the subject comes back unchanged rather than not at all.
  if PatternKillsLibrary(Pattern) then Exit;
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

function AotStrValInt(sVal: Pointer; DecWidth: PtrInt): Int64; cdecl;
begin
  // DecWidth is the opcode's Immediate: 32 for the Long/ULong spellings of VAL,
  // 0 for the 64-bit ones. Passed rather than assumed, so the compiled path and
  // the interpreted one saturate at the same place.
  Result := ParseLeadingInt64(AnsiString(sVal), Integer(DecWidth));
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
  // A3-i: the numeric halves are one byte image, so both "bank" offsets name the same field. The
  // emitters that read these still take two, and giving them one field twice is what lets the native
  // record path be rewritten in its own step rather than in this one.
  RecIntOff   := Integer(PtrUInt(@RecTmp.Bytes) - PtrUInt(@RecTmp));
  RecFloatOff := RecIntOff;
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

{ The AOT/JIT primitive table, filled in ONE place. Everything here is fixed for the life of a
  loaded program - static function addresses, with the dialect-variant ones resolved from the program
  that is loaded - so a compiled loop may bake them, while the per-CONTEXT fields (CtxObj, StrRegs,
  the Xfer bases, ArrDesc) are supplied by whoever is about to run. Shared by RunTemplate's per-Run
  ctx and by BuildJitLoops: two hand-maintained copies of this list is exactly how the two backends
  would begin calling different functions for the same opcode. }
procedure TBytecodeVM.SetAotPrimitives(var C: TAotCtx);
begin
  // C3: the runtime-helper pair. Compiled code calls back into the interpreter for an op with no
  // native form. Both are stable: the helper is a static function and VMSelf is the instance that
  // owns the compiled code. The per-WORKER half of the triple is CtxObj, which is not set here.
  C.ExecOne := @AotExecOne;
  C.VMSelf := Self;
  // C5: native string lowering - the leaf primitives compiled code calls directly for the hot
  // string ops. (The bank base itself is per-context and is set by the caller.)
  C.StrCmp := @AotStrCmp;
  C.StrAssign := @AotStrAssign;
  C.StrLoadConst := @AotStrLoadConst;
  C.StrConcat := @AotStrConcat;
  C.StrLen := @AotStrLen;
  // B3: native call site primitive. The FAST one specialises the case worth specialising - a
  // callee whose frame is a pointer slide - and falls back to the general one for everything
  // else, so it is correct for any program. AOT_FASTCALL=0 restores the general one always.
  //
  // ⛔ MISURATO E RESPINTO (20 ago 2026). Questa condizione ha una seconda meta' - FAllCalleesFast -
  // che CONTRADDICE la frase qui sopra: e' di programma intero, quindi una sola unita' chiamata che
  // copia float o stringhe spegne il percorso veloce per OGNI chiamata del programma. Su
  // binary-trees-modern-arena quell'unita' e' WORKER, che gira una volta per thread, e squalificava
  // MAKETREE e CHECKTREE, che girano milioni di volte ed erano entrambe RELOCATABLE.
  // La decisione per singolo chiamato esiste gia' ed e' esatta (FFrameFast[pc] = -1 per le non
  // idonee, e AotCallSubFast delega su quel -1), quindi il cancello sembrava puro spreco.
  // 📊 Tolto e misurato, N=18 best-of-5, output confrontato: AOT 2994 -> 3067 ms, AOT+JIT
  // 2978 -> 3102 ms. Nessun guadagno, e le due configurazioni peggiorano nella stessa direzione.
  // Le reti restavano verdi (AOT e JIT 1012 confrontati, 0 MISMATCH): era corretto, non conveniente.
  // ⛔ Non ritentarlo senza una misura: il costo del frame che AotCallSubFast "toglie" non sparisce,
  // viene INLINEATO, e su questo programma l'indirezione in piu' per i chiamati non idonei se lo
  // mangia. Se un giorno serve, va misurato di nuovo - questo verdetto ha una data.
  if (GAotFastCall = 1) and FAllCalleesFast then C.CallSub := @AotCallSubFast
  else C.CallSub := @AotCallSub;
  // C5 residuals. StrMid is dialect-variant: install the flavor once per run.
  C.StrLeft := @AotStrLeft;
  C.StrRight := @AotStrRight;
  if Assigned(FProgram) and FProgram.ModernMode then
    C.StrMid := @AotStrMidModern
  else
    C.StrMid := @AotStrMidClassic;
  C.StrAsc := @AotStrAsc;
  C.StrChr := @AotStrChr;
  C.StrInstr := @AotStrInstr;
  C.StrIntToStr := @AotIntToString;
  C.StrVal := @AotStrVal;
  C.StrValInt := @AotStrValInt;
  // Managed STRING array element: reached through a primitive because the array descriptor has no
  // StringData slot (see TAotCtx). Only ever called where an out-of-range index cannot raise.
  C.ArrLoadStr := @AotArrLoadStr;
  C.ArrStoreStr := @AotArrStoreStr;
  if Assigned(FProgram) and FProgram.ModernMode then
    C.StrAscMid := @AotStrAscMidModern
  else
    C.StrAscMid := @AotStrAscMidClassic;
  if Assigned(FProgram) and FProgram.ModernMode then
    C.StrConcatCharAt := @AotStrConcatCharAtModern
  else
    C.StrConcatCharAt := @AotStrConcatCharAtClassic;
  if Assigned(FProgram) and FProgram.ModernMode then
    C.StrAppendMapped := @AotStrAppendMappedModern
  else
    C.StrAppendMapped := @AotStrAppendMappedClassic;
  // C6: the record family. Dialect-blind (allocation has no dialect), so one flavor each.
  C.RecNew := @AotRecordNew;
  C.RecFree := @AotRecordFree;
  C.RecMarkPush := @AotRecMarkPush;
  C.RecMarkPop := @AotRecMarkPop;
  // C7: PRINT's bookkeeping pair. Dialect-blind - the semicolon primitive reads the console
  // behaviour's own property, so it needs no per-run flavor the way StrMid does.
  C.PrintSemi := @AotPrintSemicolon;
  C.PrintEnd := @AotPrintEnd;
  if GetEnvironmentVariable('AOT_PRINTSTR') = '2' then
    C.PrintStr := @AotPrintStringForceHelper       // injection: see the comment on that function
  else
    C.PrintStr := @AotPrintString;
end;

procedure TBytecodeVM.BuildPrivateArrayPlan;
// Census the arrays the compiler marked PRIVATE (a DIM inside a SUB/FUNCTION, neither SHARED nor
// STATIC) and reserve one block of physical slots per execution context, appended after the whole
// static id space.
//
// ⛔ RESERVED ONCE, NEVER GROWN. A UDT array member allocates its slot at RUNTIME by appending at
// Length(FArrays), so a SetLength here while a worker is indexing the table would move it under that
// worker. Reserving every block up front (one for the main context plus MAX_LIVE_WORKERS) means the
// spawn path never resizes the table - it only claims a block that is already there.
//
// A program with no private array at all pays NOTHING: FPrivArrCount stays 0, the table keeps exactly
// the length it had, and every ArrMap is the identity.
var
  i, n, Blocks: Integer;
begin
  // A second LoadProgram (the REPL) must not stack a new set of blocks on top of the old ones: if the
  // table still ends exactly where the previous reservation left it, take that reservation back first.
  if (FPrivArrCount > 0) and (FPrivBlockBase > 0) and
     (Length(FArrays) = FPrivBlockBase + Length(FPrivBlockUsed) * FPrivArrCount) then
    SetLength(FArrays, FPrivBlockBase);
  FStaticArrCount := Length(FArrays);
  FPrivArrCount := 0;
  FPrivBlockBase := FStaticArrCount;
  SetLength(FArrPrivSlot, FStaticArrCount);
  for i := 0 to FStaticArrCount - 1 do FArrPrivSlot[i] := -1;
  if FProgram = nil then Exit;
  n := FProgram.GetArrayCount;
  if n > FStaticArrCount then n := FStaticArrCount;
  for i := 0 to n - 1 do
    if FProgram.GetArray(i).IsPrivate then
    begin
      FArrPrivSlot[i] := FPrivArrCount;
      Inc(FPrivArrCount);
    end;
  if FPrivArrCount = 0 then
  begin
    SetLength(FPrivBlockUsed, 0);
    Exit;
  end;
  if GArrPrivDiag then
    for i := 0 to FStaticArrCount - 1 do
      WriteLn(ErrOutput, Format('[arrpriv] plan: ARR[%d] "%s" priv=%d', [i,
              FProgram.GetArray(i).Name, FArrPrivSlot[i]]));
  // ⚠️ COST, and it is the MCU target that will care: one TArrayStorage (~64 bytes of descriptor, no
  // elements until a DIM runs) per private array per block. A program with 100 proc-local arrays
  // reserves 65 x 100 slots, about 400 KB of descriptors on a build whose whole budget there is 64 KB.
  // Lowering MAX_LIVE_WORKERS is the lever; reserving lazily is NOT, for the reason in the header.
  Blocks := MAX_LIVE_WORKERS + 1;   // the main context holds one too, so every context takes one path
  SetLength(FPrivBlockUsed, Blocks);
  for i := 0 to Blocks - 1 do FPrivBlockUsed[i] := False;
  SetLength(FArrays, FPrivBlockBase + Blocks * FPrivArrCount);
end;

procedure TBytecodeVM.BindArrayMap(Ctx: TExecutionContext);
// Hand Ctx a free private block and build its logical -> physical vector. Called for the main context
// at load and for every worker before it runs. Claiming is under FWorkerLock: two spawns racing here
// must not be handed the same block.
var
  i, b, Base: Integer;
begin
  Ctx.ArrPrivBlock := -1;
  SetLength(Ctx.ArrMap, FStaticArrCount);
  for i := 0 to FStaticArrCount - 1 do Ctx.ArrMap[i] := i;   // identity: every shared array, and the
  if FPrivArrCount = 0 then Exit;                            // whole map when nothing is private
  Base := -1;
  EnterCriticalSection(FWorkerLock);
  try
    for b := 0 to High(FPrivBlockUsed) do
      if not FPrivBlockUsed[b] then
      begin
        FPrivBlockUsed[b] := True;
        Ctx.ArrPrivBlock := b;
        Base := FPrivBlockBase + b * FPrivArrCount;
        Break;
      end;
  finally
    LeaveCriticalSection(FWorkerLock);
  end;
  // No block left. That needs MAX_LIVE_WORKERS+1 LIVE contexts, which SpawnWorker already refuses to
  // create - so this is unreachable rather than tolerated, and saying so beats corrupting quietly.
  if Base < 0 then
    raise Exception.Create('array contexts exhausted: no private array block available');
  for i := 0 to FStaticArrCount - 1 do
    if FArrPrivSlot[i] >= 0 then Ctx.ArrMap[i] := Base + FArrPrivSlot[i];
  // The block this context just took may have been somebody else's a moment ago, so the entries the
  // descriptor table holds for it are not this context's. Same reason as the release path above.
  FArraysDirty := True;
  if GArrPrivDiag then
    WriteLn(ErrOutput, Format('[arrpriv] bind: ctx=%p block=%d base=%d', [Pointer(Ctx), Ctx.ArrPrivBlock, Base]));
end;

procedure TBytecodeVM.ReleaseArrayMap(Ctx: TExecutionContext);
// Give the block back and drop its element data. A worker context dies at CleanupWorkers; its arrays
// must not keep their storage alive for the rest of the run, and the block must be reusable.
var
  i, Base: Integer;
begin
  if (Ctx = nil) or (Ctx.ArrPrivBlock < 0) then Exit;
  Base := FPrivBlockBase + Ctx.ArrPrivBlock * FPrivArrCount;
  // ⛔ UNDER FArrDescLock. Freeing these buffers changes the very fields RebuildJitArrDesc reads to
  // build the table (@FArrays[a].IntData[0] and TotalSize), and that rebuild runs on ANOTHER thread
  // holding this lock. Doing it unlocked is a data race on the dynamic-array headers, and it showed
  // as parallel fasta answering differently in about one run in twelve - on every engine, which is
  // what told us the descriptor table was no longer the culprit.
  EnterCriticalSection(FArrDescLock);
  try
    for i := Base to Base + FPrivArrCount - 1 do
      if i <= High(FArrays) then
      begin
        SetLength(FArrays[i].IntData, 0);
        SetLength(FArrays[i].FloatData, 0);
        SetLength(FArrays[i].StringData, 0);
        SetLength(FArrays[i].Dimensions, 0);
        SetLength(FArrays[i].LowerBounds, 0);
        FArrays[i].TotalSize := 0;
        FArrays[i].DimCount := 0;
      end;
  finally
    LeaveCriticalSection(FArrDescLock);
  end;
  // ⛔ THE DESCRIPTOR TABLE STILL POINTS AT THE STORAGE JUST FREED. Say so, or the next context handed
  // this block reads and writes through dangling pointers until something else happens to dirty the
  // table - which is a use-after-free that only appears when blocks are REUSED, i.e. when a program
  // spawns more than one wave of workers. 📊 Found on parallel fasta (three waves): threads died with
  // access violations and the output changed run to run, while fannkuch, k-nucleotide and
  // reverse-complement - one wave each, or no local array at all - were stable.
  FArraysDirty := True;
  EnterCriticalSection(FWorkerLock);
  try
    if Ctx.ArrPrivBlock <= High(FPrivBlockUsed) then FPrivBlockUsed[Ctx.ArrPrivBlock] := False;
  finally
    LeaveCriticalSection(FWorkerLock);
  end;
  Ctx.ArrPrivBlock := -1;
  Ctx.ArrDescCur := nil;
  SetLength(Ctx.ArrMap, 0);
  SetLength(Ctx.ArrDescRetired, 0);   // this context is done: nothing can still be reading them
  SetLength(Ctx.ArrDescOwn, 0);
  Ctx.ArrDescGen := 0;
end;

procedure TBytecodeVM.CheckPrivDesc(Ctx: TExecutionContext; Desc: Pointer);
// ARRPRIV_DIAG=1: every PRIVATE entry of the table about to be handed to the C loop must point at the
// storage THIS context owns. Says which id, what the table holds and what it should hold.
var
  i, phys: Integer;
  D: PInt64;
  want: Int64;
begin
  D := PInt64(Desc);
  for i := 0 to Length(FArrPrivSlot) - 1 do
    if FArrPrivSlot[i] >= 0 then
    begin
      phys := Ctx.ArrMap[i];
      if (phys < 0) or (phys > High(FArrays)) then Continue;
      if Length(FArrays[phys].IntData) > 0 then want := Int64(PtrUInt(@FArrays[phys].IntData[0])) else want := 0;
      if D[i * 4] <> want then
        WriteLn(ErrOutput, Format('[arrpriv] ⛔ ARR[%d] phys=%d desc=%x atteso=%x size=%d gen=%d/%d',
                [i, phys, D[i * 4], want, FArrays[phys].TotalSize, Ctx.ArrDescGen, FArrDescGen]));
    end;
end;

function TBytecodeVM.ActiveCtx: TExecutionContext; inline;
// ⛔ GActiveCtx is set only inside a WORKER; on the main thread it is nil. Every reader of it in this
// unit resolves it the same way, and writing that out again at each site is how one of them ends up
// dereferencing nil.
begin
  Result := GActiveCtx;
  if Result = nil then Result := FCtx;
end;

function TBytecodeVM.MapArrDyn(Ctx: TExecutionContext; Id: Int64): Integer;
// The array id arrived in a REGISTER or packed inside a pointer, so it may be a runtime slot (a UDT
// array member appended past the static space) as easily as a compile-time one. Only a compile-time
// id can be private, hence the range test the immediate path does not need.
begin
  Result := Integer(Id);
  if (FPrivArrCount > 0) and (Result >= 0) and (Result < Length(Ctx.ArrMap)) then
    Result := Ctx.ArrMap[Result];
end;

procedure TBytecodeVM.RebuildJitArrDesc;
// ⛔ CALL ONLY WITH FArrDescLock HELD - use EnsureArrDesc, which is the whole public entry point.
var
  a, n: Integer;
begin
  // 4 Int64 per array (32 bytes): IntData ptr, FloatData ptr, Count (TotalSize), lower bound of dim 0.
  // LBound lets the JIT compile LBOUND/UBOUND(arr) for a 1-D array (dim 0); other dims / the rank query
  // deopt to the interpreter.
  //
  // The buffer this table lives in is REPLACED, not resized, whenever a worker might still be reading
  // the old one: a running worker holds the previous pointer in its own AotCtx and refreshes it only
  // at its own call boundaries, so freeing here would hand it a dangling table. Retiring costs one
  // list entry per rebuild for the duration of the threaded phase, and rebuilds are rare (DIM/REDIM/
  // ERASE and native-region installs). With no workers the assignment below just resizes in place,
  // exactly as before.
  if FHasWorkers and (Length(FJitArrDesc) > 0) then
  begin
    SetLength(FRetiredArrDesc, Length(FRetiredArrDesc) + 1);
    FRetiredArrDesc[High(FRetiredArrDesc)] := FJitArrDesc;   // the reference is what keeps it alive
    FJitArrDesc := nil;                                      // so the SetLength below allocates fresh
  end;
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
  Inc(FArrDescGen);   // every per-context copy built from the old one is now stale
end;

function TBytecodeVM.AcquireArrDescCtx(ECtx: TExecutionContext): Pointer;
// The descriptor table THIS context may hand to compiled code.
//
// ⭐ Why it exists. hotdisp.c, the AOT and the loop JIT all index the table with the array id baked
// into their code, which is a LOGICAL id. A proc-local array has one storage PER CONTEXT, so the same
// logical id must resolve to different memory in different threads - and the cheapest place to say so
// is the table itself, not every access. Patching the copy here is what let all three compiled
// engines stay byte-identical while local arrays became per-thread.
//
// ⛔ A program with NO private array gets the master table, unchanged and unlocked on the fast lane,
// exactly as before: this whole path must cost nothing where it buys nothing.
var
  i, n: Integer;
  Src, Dst: Integer;
begin
  if FPrivArrCount = 0 then Exit(AcquireArrDesc);
  EnterCriticalSection(FArrDescLock);
  try
    if FArraysDirty then RebuildJitArrDesc;
    if (ECtx.ArrDescGen <> FArrDescGen) or (Length(ECtx.ArrDescOwn) <> Length(FJitArrDesc)) then
    begin
      // ⛔ UPDATE IN PLACE WHENEVER THE LENGTH ALLOWS IT, and retire only when it does not.
      // Compiled code caches base pointers READ OUT OF this table in machine registers and reloads
      // them only at its own call boundaries, so handing it a different buffer mid-function is handing
      // it stale bases. The master table has always behaved this way - SetLength to the same length
      // keeps the buffer and the loop below overwrites the entries - and the per-context copy has to
      // behave the same. 📊 Retiring unconditionally cost reverse-complement its whole output under
      // --aot: a shared scalar read after a shared string write came back 0 and the program died on a
      // division by zero.
      // A LENGTH CHANGE still retires: the buffer must move, and a native frame may hold the old one.
      if Length(ECtx.ArrDescOwn) <> Length(FJitArrDesc) then
      begin
        if Length(ECtx.ArrDescOwn) > 0 then
        begin
          n := Length(ECtx.ArrDescRetired);
          SetLength(ECtx.ArrDescRetired, n + 1);
          ECtx.ArrDescRetired[n] := ECtx.ArrDescOwn;
          ECtx.ArrDescOwn := nil;
        end;
        SetLength(ECtx.ArrDescOwn, Length(FJitArrDesc));
      end;
      // ⛔ WRITE EACH ENTRY ALREADY MAPPED - do NOT copy the master and then patch it.
      // The two-step version has a WINDOW: after the copy and before the patch, this context's own
      // private entries hold the master's values for the dead compile-time slot, which are zero. The
      // buffer is updated IN PLACE (see above), so a compiled loop of this thread that re-reads the
      // table inside that window reads a null data pointer. 📊 That window made parallel fasta under
      // --jit produce a different answer in roughly one run out of three, while --aot - which only
      // re-reads at call boundaries - was stable.
      // Written this way each entry goes from one correct value straight to the next, and a 64-bit
      // aligned store is atomic, so there is no moment at which the table is wrong.
      for i := 0 to (Length(ECtx.ArrDescOwn) div 4) - 1 do
      begin
        Dst := i * 4;
        if Dst + 3 >= Length(ECtx.ArrDescOwn) then Break;
        if (i < Length(FArrPrivSlot)) and (FArrPrivSlot[i] >= 0) then
        begin
          // ⛔ A PRIVATE ENTRY IS BUILT FROM THE STORAGE, NOT FROM THE MASTER TABLE.
          // The master is only rebuilt when FArraysDirty says so, and that flag is VM-global: another
          // thread can consume it - clearing it - in the window between this context allocating its
          // array and reading the table back, leaving the master's entry for this block empty while
          // the storage exists. Reading FArrays directly cannot be stale: it IS the storage.
          // 📊 That is what killed a worker inside sedai_hot_run on the multi-wave guard, roughly one
          // run in six: `desc=0 atteso=7F7D4975B050 size=64`.
          Src := ECtx.ArrMap[i];
          if (Src < 0) or (Src > High(FArrays)) then Continue;
          if Length(FArrays[Src].IntData) > 0 then
            ECtx.ArrDescOwn[Dst + 0] := Int64(PtrUInt(@FArrays[Src].IntData[0]))
          else ECtx.ArrDescOwn[Dst + 0] := 0;
          if Length(FArrays[Src].FloatData) > 0 then
            ECtx.ArrDescOwn[Dst + 1] := Int64(PtrUInt(@FArrays[Src].FloatData[0]))
          else ECtx.ArrDescOwn[Dst + 1] := 0;
          ECtx.ArrDescOwn[Dst + 2] := FArrays[Src].TotalSize;
          if Length(FArrays[Src].LowerBounds) > 0 then
            ECtx.ArrDescOwn[Dst + 3] := FArrays[Src].LowerBounds[0]
          else ECtx.ArrDescOwn[Dst + 3] := 0;
        end
        else if i * 4 + 3 < Length(FJitArrDesc) then
        begin
          ECtx.ArrDescOwn[Dst + 0] := FJitArrDesc[i * 4 + 0];
          ECtx.ArrDescOwn[Dst + 1] := FJitArrDesc[i * 4 + 1];
          ECtx.ArrDescOwn[Dst + 2] := FJitArrDesc[i * 4 + 2];
          ECtx.ArrDescOwn[Dst + 3] := FJitArrDesc[i * 4 + 3];
        end;
      end;
      ECtx.ArrDescGen := FArrDescGen;
      if GArrPrivDiag then
        for i := 0 to Length(FArrPrivSlot) - 1 do
          if FArrPrivSlot[i] >= 0 then
            WriteLn(ErrOutput, Format('[arrpriv] desc gen=%d ARR[%d]->phys %d data=%x size=%d',
                    [FArrDescGen, i, ECtx.ArrMap[i], ECtx.ArrDescOwn[i*4], ECtx.ArrDescOwn[i*4+2]]));
    end;
    if Length(ECtx.ArrDescOwn) > 0 then Result := @ECtx.ArrDescOwn[0] else Result := nil;
    ECtx.ArrDescCur := Result;   // published: this is what a cached pointer must compare equal to
  finally
    LeaveCriticalSection(FArrDescLock);
  end;
end;

function TBytecodeVM.AcquireArrDesc: Pointer;
// The ONE way compiled code gets its array-descriptor pointer. Test the flag, rebuild if needed and
// TAKE THE ADDRESS - all three under FArrDescLock, because the flag and the table are VM-global and
// every worker reaches them.
//
// ⛔⛔ Splitting those three steps is what the bug was, and it was written out FOUR TIMES: twice in
// the AotCallSub paths here and twice in the run-loop template (the JIT arm and the AOT arm). Each
// copy read the flag, called the rebuild and then took @FJitArrDesc[0] with nothing held. Two
// workers reaching their first native call together therefore each ran SetLength on the same
// dynamic array: two buffers allocated, the second assignment dropping the first, and the thread
// that had already taken the address of the first left holding freed memory.
// 📊 Measured on m59_sharedscalar.bas at 32-way parallelism, before the fix: of 600 runs, 73 had
// two threads inside the rebuild at once; two printed a wrong total (increments written into the
// dead buffer) and two HUNG - the faulting worker died of an access violation inside the compiled
// code, its exception was swallowed at the thread boundary, and it never released the user mutex.
// ⭐ The reason one shared helper replaces four inline copies: a lock taken by one caller is a
// guarantee every other caller silently loses.
begin
  EnterCriticalSection(FArrDescLock);
  try
    if FArraysDirty then RebuildJitArrDesc;
    if Length(FJitArrDesc) > 0 then Result := @FJitArrDesc[0]
    else Result := nil;
    FCurArrDesc := Result;          // pubblicato sotto il lock: e' la chiave della corsia veloce
  finally
    LeaveCriticalSection(FArrDescLock);
  end;
end;

procedure TBytecodeVM.EnsureArrDesc(Ctx: PAotCtx);
// AcquireArrDesc, published into the caller's own context record.
//
// ⭐⭐⭐ E LA CORSIA VELOCE, CHE E' TUTTO IL PUNTO. Questa procedura sta sul cammino piu' caldo del
// motore: AotCallSub la chiama PRIMA e DOPO ogni chiamata nativa, quindi una funzione ricorsiva ne
// paga due per invocazione. Con la sola sezione critica, `binary-trees` sotto --aot e' passato da
// 171 ms (11 ago) a 711 ms - due sezioni critiche globali PER NODO, e l'interprete, che non ne
// prende nessuna, e' finito 5,6 volte piu' veloce del codice compilato.
//
// La corsia veloce salta il lock quando NIENTE puo' essere cambiato, e lo decide con DUE domande,
// non una:
//   - FArraysDirty: qualcuno ha marcato la tabella (DIM/REDIM/ERASE, ogni ExecuteArrayOp,
//     l'installazione di una regione nativa). E' gia' scritto SENZA lock da quei siti, quindi
//     leggerlo senza lock non aggiunge una classe di corse che non ci fosse.
//   - Ctx^.ArrDesc <> FCurArrDesc: la tabella e' stata RICOSTRUITA da qualcun altro dopo l'ultima
//     volta che questo contesto l'ha letta. Questa seconda domanda chiude la finestra in cui il
//     flag e' gia' stato azzerato dal ricostruttore e il lettore lo vedrebbe falso.
// Se entrambe dicono di no, il puntatore in mano al chiamante e' quello corrente e non serve altro.
// ⛔ Ogni percorso che PUO' cambiare la tabella passa comunque dal lock, esattamente come prima.
// AOT_ARRDESC=0 ripristina il lock incondizionato: e' l'A/B su un binario solo.
var
  ECtx: TExecutionContext;
begin
  if FPrivArrCount > 0 then
  begin
    // Private arrays: the table is per-context, so "is it still the current one" is a GENERATION
    // question, not a pointer one - FCurArrDesc names the master, which this context never holds.
    ECtx := TExecutionContext(Ctx^.CtxObj);
    if ECtx = nil then ECtx := ActiveCtx;
    // TWO questions, and both are needed: the generation says the CONTENT is current, the pointer
    // says the caller holds the CURRENT BUFFER. Dropping the second cost an access violation in
    // compiled code - see TExecutionContext.ArrDescCur.
    if GArrDescFast and (not FArraysDirty) and (Ctx^.ArrDesc <> nil) and
       (Ctx^.ArrDesc = ECtx.ArrDescCur) and (ECtx.ArrDescGen = FArrDescGen) then Exit;
    Ctx^.ArrDesc := AcquireArrDescCtx(ECtx);
    Exit;
  end;
  if GArrDescFast and (not FArraysDirty) and (Ctx^.ArrDesc = FCurArrDesc) and
     (FCurArrDesc <> nil) then Exit;
  Ctx^.ArrDesc := AcquireArrDesc;
end;

function TBytecodeVM.AcquireArrDescFast(var Cached: Pointer; ECtx: TExecutionContext): Pointer;
// La stessa corsia veloce di EnsureArrDesc, per un chiamante che tiene il puntatore in una VARIABILE
// invece che in un contesto AOT: e' l'arm del JIT nel ciclo di esecuzione.
//
// ⛔ Perche' esiste invece di due righe scritte li': l'arm del JIT chiamava AcquireArrDesc a OGNI
// ingresso in un ciclo compilato, cioe' una sezione critica globale per ingresso. E' esattamente il
// difetto che EnsureArrDesc documenta di aver tolto dall'AOT (binary-trees 171 -> 711 ms, 4,2x): la
// corsia veloce era stata aggiunta a UN motore e non al gemello. Misurato il 20 ago 2026 su
// binary-trees-modern-arena N=14: --jit 668 ms contro 196 dell'interprete.
//
// ⭐ E il file lo dice gia' sopra: «una corsia scritta in linea da un chiamante e' una garanzia che
// ogni altro chiamante perde in silenzio». Quindi un helper solo, non una quinta copia.
begin
  if FPrivArrCount > 0 then
  begin
    if GArrDescFast and (not FArraysDirty) and (Cached <> nil) and
       (Cached = ECtx.ArrDescCur) and (ECtx.ArrDescGen = FArrDescGen) then Exit(Cached);
    Cached := AcquireArrDescCtx(ECtx);
    Exit(Cached);
  end;
  if GArrDescFast and (not FArraysDirty) and (Cached = FCurArrDesc) and (FCurArrDesc <> nil) then
    Exit(Cached);
  Cached := AcquireArrDesc;
  Result := Cached;
end;

procedure TBytecodeVM.ReleaseRetiredArrDesc;
// Drop the retired descriptor buffers once no worker can still be holding one. Called when the last
// worker exits, so the retention above lasts only as long as the threaded phase does.
begin
  EnterCriticalSection(FArrDescLock);
  try
    SetLength(FRetiredArrDesc, 0);
  finally
    LeaveCriticalSection(FArrDescLock);
  end;
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

{ ⛔ THIS MUST STAY ABOVE THE RUN LOOP. It is declared `inline` and it is called on every typed
  array element access, which is the interpreter's hot array path - but FPC can only inline a body it
  has ALREADY compiled, and this one used to sit 1345 lines BELOW the {$I RunTemplate.inc} that calls
  it. The result was a real function call per array access, and the compiler said so 69 times in one
  build: "Call to subroutine TBytecodeVM.ArrayBoundsOK ... marked as inline is not inlined". An
  `inline` directive is a request, and the compiler answers it in source order. }
{ The out-of-bounds half, deliberately OUT OF LINE. CLASSIC keeps Commodore's ?BAD SUBSCRIPT
  semantics; --bounds-check forces the raise in any dialect. Otherwise MODERN matches FreeBASIC,
  which performs no bounds check by default: the caller substitutes a default value on a read and
  drops the store, keeping us memory-safe (FB would touch adjacent heap). }
function TBytecodeVM.ArrayBoundsFail(ArrayIdx, LinearIdx: Integer): Boolean;
begin
  if FBoundsCheck or (Assigned(FProgram) and not FProgram.ModernMode) then
    raise ERangeError.CreateFmt('Array index out of bounds: %d (size: %d)', [LinearIdx, FArrays[ArrayIdx].TotalSize]);
  Result := False;
end;

function TBytecodeVM.ArrayBoundsOK(ArrayIdx, LinearIdx: Integer): Boolean;
begin
  // ⭐ WHAT GETS INLINED IS THIS AND NOTHING MORE: one compare, one branch. The raise path - with its
  // Format call and its two conditions - lives in ArrayBoundsFail, out of line. Inlining the whole
  // thing into fifty call sites grew the run loop enough to COST on an array-heavy program
  // (spectral-norm +5%, stable over ten runs) while paying on others, which is code growth rather
  // than work: the check itself is two instructions.
  if (LinearIdx >= 0) and (LinearIdx < FArrays[ArrayIdx].TotalSize) then
    Exit(True);
  Result := ArrayBoundsFail(ArrayIdx, LinearIdx);
end;

{$IFDEF HOT_C}
{ Fold the run-wide gate into the per-PC table, so the hot path reads one array and tests nothing
  else. Cheap because it only runs when the gate CHANGES, which is once per run in practice. }
procedure TBytecodeVM.SetHotOpEnabled(AEnabled: Boolean);
var
  i: Integer;
begin
  if (FHotOpEnabled = AEnabled) and (Length(FHotOp) = Length(FHotOpBase)) then Exit;
  FHotOpEnabled := AEnabled;
  SetLength(FHotOp, Length(FHotOpBase));
  if AEnabled then
  begin
    for i := 0 to High(FHotOpBase) do FHotOp[i] := FHotOpBase[i];
    // ...and ZERO every PC that starts a compiled region, so the C loop hands the PC back there
    // instead of running past it - the dispatcher already does exactly that on a zero entry
    // ("if (!h_) return pc"). Without this the C loop can step over a compiled entry and the region
    // is simply skipped: the answer stays right, because the bytecode is equivalent, but the AOT
    // stops being used and a measurement of the two together measures neither.
    for i := 0 to High(FNativeFuncs) do
      if (FNativeFuncs[i] <> nil) and (i <= High(FHotOp)) then FHotOp[i] := 0;
    // ...and the same for the LOOP JIT, for the same reason. FNativeLoops is a per-PC side table
    // exactly like FNativeFuncs - the JIT does not rewrite the instruction stream, it indexes the
    // loop HEADER pc - so zeroing the header makes the C dispatcher hand the pc back there, and the
    // interpreter then enters the compiled loop. Order is safe: BuildJitLoops fills FNativeLoops
    // while the program is being prepared, and this runs at RunFast entry, after it.
    for i := 0 to High(FNativeLoops) do
      if (FNativeLoops[i] <> nil) and (i <= High(FHotOp)) then FHotOp[i] := 0;
  end
  else
    for i := 0 to High(FHotOpBase) do FHotOp[i] := 0;
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
function InputFieldIsFloat(const S: string): Boolean;
// Does this INPUT field spell a FLOATING-POINT number? A '.' or an exponent letter after at least one
// digit. ⛔ A base-prefixed literal is never one: 'd' and 'e' are hex DIGITS there, and "&h1d1" is 465
// and not 1 x 10^1. Used only to choose which grammar INPUT parses the field with - see bcInputFileInt.
var
  I, Len: Integer;
  HasDigit: Boolean;
begin
  Result := False;
  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do Inc(I);
  if (I <= Len) and ((S[I] = '+') or (S[I] = '-')) then Inc(I);
  if (I <= Len) and (S[I] = '&') then Exit;          // a base literal, whatever letters follow
  HasDigit := False;
  while I <= Len do
  begin
    if (S[I] >= '0') and (S[I] <= '9') then begin HasDigit := True; Inc(I); end
    else if S[I] = '.' then Exit(True)
    else if HasDigit and (UpCase(S[I]) in ['E', 'D']) then Exit(True)
    else Break;
  end;
end;

function ParseLeadingInt64(const S: string; DecWidth: Integer): Int64;
var
  I, Len, Base, D: Integer;
  Neg, Signed: Boolean;
  C: Char;
  U: QWord;
begin
  Result := 0;
  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do Inc(I);  // skip leading whitespace
  Neg := False;
  Signed := False;
  if (I <= Len) and ((S[I] = '+') or (S[I] = '-')) then
  begin
    Neg := (S[I] = '-');
    Signed := True;
    Inc(I);
  end;
  // FreeBASIC base prefixes: &H hex, &O octal, &B binary - and ONLY when nothing precedes them.
  // fbc does not accept a SIGN before a base prefix. VALINT("-&HFF") is 0, not -255: the '-' is
  // consumed, the prefix is then not recognised, the decimal scan finds no digits, and negating zero
  // leaves zero. Measured against fbc 1.10.1 for both signs and all three prefixes - see
  // ParseLeadingFloat for the float form, where the same rule shows up as NEGATIVE zero.
  if (not Signed) and (I < Len) and (S[I] = '&') then
  begin
    C := UpCase(S[I + 1]);
    Base := 0;
    if C = 'H' then Base := 16
    else if C = 'O' then Base := 8
    else if C = 'B' then Base := 2;
    if Base > 0 then
      Inc(I, 2)   // skip the "&X" prefix
    // ⭐ A BARE "&" IS OCTAL TOO, and it is the spelling nothing here knew: fbc reads "&77" as 63
    // through VAL, VALINT, VALLNG, VALUINT and INPUT alike (measured on all five), while we answered
    // 0 for every one of them - the prefix was not recognised, the decimal scan then found no digits.
    // Its own line of fbc's file/large_int asserts it beside "&O77", which we already had.
    // ⚠️ Only when an OCTAL DIGIT follows: "&x" stays 0, and "&8" reads as 0 the same way "&o78"
    // stops at the 8 - the digit test below is what does both.
    else if (S[I + 1] >= '0') and (S[I + 1] <= '7') then
    begin
      Base := 8;
      Inc(I);     // skip the "&" alone
    end;
    if Base > 0 then
    begin
      U := 0;
      while I <= Len do
      begin
        C := UpCase(S[I]);
        if (C >= '0') and (C <= '9') then D := Ord(C) - Ord('0')
        else if (C >= 'A') and (C <= 'F') then D := Ord(C) - Ord('A') + 10
        else Break;
        if D >= Base then Break;
        {$PUSH}{$Q-}{$R-}
        // The UNSIGNED accumulation wraps by design - that is what parsing a full-width base literal
        // means. Silenced here so a debug build can be used; see the lexer's twin.
        U := U * QWord(Base) + QWord(D);
        {$POP}
        Inc(I);
      end;
      Result := Int64(U);
      if Neg then Result := -Result;
      Exit;
    end;
  end;
  // ⭐ The DECIMAL magnitude SATURATES; it does not wrap. fbc reads a decimal
  // through the C library, which stops at the type's unsigned maximum, and then
  // applies the sign by two's-complement negation in the target width. Measured
  // against fbc 1.10.1, and it is what makes every one of these agree:
  //     ValLng ("18446744073709551616")   -1   (saturated, read signed)
  //     ValULng("18446744073709551616")   18446744073709551615
  //     ValInt ("-99999999999999999999999")  1   (-(2^64-1) narrowed to 32 bits)
  // ⛔ The BASE-PREFIX branch above deliberately keeps WRAPPING: fbc scans &H/&O/&B
  // itself rather than through the C library, so ValInt("&H100000000") is 0 in both
  // and saturating there would break a case that already agrees.
  U := 0;
  D := 0;                                   // reused as the overflow flag
  while (I <= Len) and (S[I] >= '0') and (S[I] <= '9') do
  begin
    if U > (QWord($FFFFFFFFFFFFFFFF) - QWord(Ord(S[I]) - Ord('0'))) div 10 then
      D := 1
    else
      U := U * 10 + QWord(Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if D <> 0 then U := QWord($FFFFFFFFFFFFFFFF);
  // The 32-bit spellings saturate at THEIR maximum, not at the 64-bit one:
  // ValInt("4294967296") is -1 in fbc (0xFFFFFFFF read signed) and not 0, and
  // ValInt("-4294967296") is 1, because the sign is applied to the saturated
  // magnitude and then narrowed.
  if (DecWidth = 32) and (U > QWord($FFFFFFFF)) then U := QWord($FFFFFFFF);
  Result := Int64(U);
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
  else if Length(Result) < Digits then
    // One allocation. Prepending '0' in a loop reallocates and copies the whole string per zero,
    // which for HEX$(1, 16) is fifteen of them.
    Result := StringOfChar('0', Digits - Length(Result)) + Result;
end;

function ParseLeadingFloat(const S: string): Double;
var
  I, J, K, Len, DPos: Integer;
  T: string;
  HasDigit, HasDot, Neg: Boolean;
begin
  Result := 0.0;
  Len := Length(S);
  I := 1;
  while (I <= Len) and (S[I] = ' ') do Inc(I);   // skip leading whitespace
  // A base prefix is an integer value - but ONLY when it is the first thing after the spaces.
  // FreeBASIC does not accept a sign before one, so this deliberately looks BEFORE the sign scan.
  if (I <= Len) and (S[I] = '&') then
  begin
    Result := ParseLeadingInt64(Copy(S, I, Len - I + 1), 0);
    Exit;
  end;
  // [sign] digits [. digits]
  J := I;
  Neg := False;
  if (J <= Len) and ((S[J] = '+') or (S[J] = '-')) then
  begin
    Neg := (S[J] = '-');
    Inc(J);
  end;
  HasDigit := False;
  HasDot := False;
  while J <= Len do
  begin
    if (S[J] >= '0') and (S[J] <= '9') then begin HasDigit := True; Inc(J); end
    else if (S[J] = '.') and (not HasDot) then begin HasDot := True; Inc(J); end
    else Break;
  end;
  if not HasDigit then
  begin
    // ⚠️ NOT plain zero. fbc applies a minus it has already consumed even when nothing parseable
    // follows, so every string that starts with '-' and then fails to be a number reads as NEGATIVE
    // zero: "-x", "- 12" (fbc does not skip spaces after the sign), "--12", "-e5" and "-&HFF" all
    // answer -0, which PRINTS as "-0" while comparing equal to zero.
    // The single exception is a sign with NOTHING after it: VAL("-") is +0, because fbc never gets
    // as far as looking for a digit. Both halves measured against fbc 1.10.1, not reasoned from the
    // manual; guardian job/tests/bas/bug_basestr_val.bas.
    // The sign bit is set through the bit pattern because a `-0.0` literal is a constant the
    // compiler may fold straight back to +0.0, and the whole point here is the sign bit.
    if Neg and (J <= Len) then PInt64(@Result)^ := Int64($8000000000000000);
    Exit;
  end;
  // Optional exponent: (e|E|d|D) [sign] digits — only consumed if at least one exponent digit follows.
  DPos := 0;
  if (J <= Len) and (UpCase(S[J]) in ['E', 'D']) then
  begin
    K := J + 1;
    if (K <= Len) and ((S[K] = '+') or (S[K] = '-')) then Inc(K);
    if (K <= Len) and (S[K] >= '0') and (S[K] <= '9') then
    begin
      // Remember WHERE the exponent marker is, relative to the slice taken below. It is the only
      // place a 'd'/'D' can occur in a number we scanned ourselves, which is what lets the blanket
      // StringReplace pass go away.
      if (S[J] = 'd') or (S[J] = 'D') then DPos := J - I + 1;
      while (K <= Len) and (S[K] >= '0') and (S[K] <= '9') do Inc(K);
      J := K;
    end;
  end;
  T := Copy(S, I, J - I);
  // A leading '.' (e.g. ".5" or "-.5") needs a '0' for Pascal's Val. Either form inserts exactly one
  // character ahead of any exponent marker, so DPos moves with it - and it is tied to the insertion
  // itself rather than inferred afterwards from T[1], which cannot tell an inserted '0' from the one
  // in "0.5e3".
  if (Length(T) >= 1) and (T[1] = '.') then
  begin
    T := '0' + T;
    if DPos > 0 then Inc(DPos);
  end
  else if (Length(T) >= 2) and ((T[1] = '+') or (T[1] = '-')) and (T[2] = '.') then
  begin
    T := T[1] + '0' + Copy(T, 2, Length(T));
    if DPos > 0 then Inc(DPos);
  end;
  // FreeBASIC's 'D' exponent means what Pascal spells 'E'. This used to be two StringReplace passes
  // over every VAL() ever evaluated - each allocating a fresh string and rescanning it - to fix a
  // character that is almost never there. The scan above already located it, so this is one byte
  // store on the rare call that needs it.
  if (DPos > 0) and (DPos <= Length(T)) then T[DPos] := 'E';
  // ⛔ NOT FPC's Val, which was wrong twice and silently: it gives up entirely on a
  // string longer than 255 characters (fpc_Val_Real_AnsiStr sets code 256, and the
  // caller here turned that into 0.0), and it parses through the 80-bit Extended
  // and rounds a second time into the Double. Both measured against fbc, which is
  // right on both counts. ExactStrToDouble rounds ONCE, half to even, from the
  // exact decimal - and it is written without floating point so the WebAssembly
  // backend can run the same algorithm and agree by construction.
  Result := ExactStrToDouble(T);
end;

// Render an Int64 in an arbitrary base (2..16) as an unsigned bit pattern, no
// leading zeros - mirrors HEX$ semantics for OCT(n)/BIN(n) (FreeBASIC B1.3).
function IntToBaseStr(Value: Int64; Base: Integer): string;
const
  Digits: array[0..15] of Char = '0123456789ABCDEF';
var
  U: QWord;
  Buf: array[0..63] of Char;   // 64 bits in base 2 is the widest this can get
  P, N: Integer;
begin
  U := QWord(Value);
  if U = 0 then
    Exit('0');
  // Digits come out least-significant first, so they are written BACKWARDS into a fixed buffer and
  // copied once. The obvious version - Result := Digit + Result - reallocates and copies the whole
  // accumulated string on every digit, which is the same shape as the quadratic REGEXREPLACE: fine
  // at three digits, sixteen allocations for a 64-bit value in hex and sixty-four in binary.
  P := SizeOf(Buf) div SizeOf(Char);
  while U > 0 do
  begin
    Dec(P);
    Buf[P] := Digits[U mod QWord(Base)];
    U := U div QWord(Base);
  end;
  N := (SizeOf(Buf) div SizeOf(Char)) - P;
  SetLength(Result, N);
  Move(Buf[P], Result[1], N * SizeOf(Char));
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
function Utf8ContainsCP(const CPSet, Ch: string): Boolean;
// Does the UTF-8 string CPSet contain the single codepoint Ch as one of ITS codepoints? A plain Pos()
// would answer yes for a byte sequence that merely OVERLAPS one - the reason the byte "Any" form is
// wrong here - so the set is walked codepoint by codepoint.
var
  i, n, cs: Integer;
begin
  Result := False;
  n := Length(CPSet);
  cs := 0;
  for i := 1 to n do
    if (Ord(CPSet[i]) and $C0) <> $80 then
    begin
      if (cs > 0) and (Copy(CPSet, cs, i - cs) = Ch) then Exit(True);
      cs := i;
    end;
  if (cs > 0) and (Copy(CPSet, cs, n - cs + 1) = Ch) then Exit(True);
end;

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

function Utf8FindAnyCP(const S, CPSet: string; Last: Boolean): Integer;
// The 1-based CODEPOINT position of the first (or last) codepoint of S that also occurs in CPSet, or 0.
// Both strings are UTF-8, and the comparison is per CODEPOINT on both sides: comparing bytes matches a
// CONTINUATION byte and answers nonsense on anything outside ASCII.
var
  i, n, cp, cs, ce: Integer;
  Ch: string;
begin
  Result := 0;
  n := Length(S);
  if (n = 0) or (CPSet = '') then Exit;
  cp := 0;
  cs := 0;
  for i := 1 to n do
    if (Ord(S[i]) and $C0) <> $80 then          // byte i begins a codepoint
    begin
      if cs > 0 then
      begin
        // the codepoint that started at cs ends just before i
        Ch := Copy(S, cs, i - cs);
        if Utf8ContainsCP(CPSet, Ch) then
        begin
          Result := cp;
          if not Last then Exit;
        end;
      end;
      Inc(cp);
      cs := i;
    end;
  if cs > 0 then                                 // the final codepoint
  begin
    Ch := Copy(S, cs, n - cs + 1);
    if Utf8ContainsCP(CPSet, Ch) then Result := cp;
  end;
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

// Decode the FIRST codepoint of a UTF-8 string (FreeBASIC ASC on a WSTRING); 0 for an empty string.
// The mirror of Utf8EncodeCP, and the reason bcStrAsc cannot answer for a wide string: that one takes
// the first BYTE, which for anything above U+007F is only the lead byte of the sequence.
function Utf8FirstCP(const S: string): Integer;
var
  b, n, i, need: Integer;
begin
  if S = '' then Exit(0);
  b := Ord(S[1]);
  if b < $80 then Exit(b);
  if (b and $E0) = $C0 then begin Result := b and $1F; need := 1; end
  else if (b and $F0) = $E0 then begin Result := b and $0F; need := 2; end
  else if (b and $F8) = $F0 then begin Result := b and $07; need := 3; end
  else Exit(b);                                  // a stray continuation byte: report it as it stands
  n := Length(S);
  for i := 2 to need + 1 do
  begin
    if (i > n) or ((Ord(S[i]) and $C0) <> $80) then Exit(b);   // truncated: the lead byte, as bcStrAsc
    Result := (Result shl 6) or (Ord(S[i]) and $3F);
  end;
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
  // ⛔ READ-ONLY ALIAS of a string register. Assigning a register to the local S above is a MANAGED
  // assignment: a reference count up when it is taken and down when it is replaced, i.e. TWO atomic
  // read-modify-writes per executed opcode. In a per-character loop that is two atomics per
  // CHARACTER, and when several threads read the SAME string they are hitting one cache line.
  // 📊 Measured 21 Aug 2026 on a probe (1 M characters x 20 passes of Asc(Mid(s,i,1))): reading a
  // SHARED string took 677 ms on one thread and 12 357 ms on eight - EIGHTEEN TIMES WORSE for
  // eight times the parallelism. The same loop over a shared ARRAY went 90 -> 137 ms.
  // ⭐ The rule was already written in this file, at bcStrAppendMapped: "Read both strings IN
  // PLACE". It was true there and lost everywhere else.
  SP: PString;
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
    52: // bcStrAscW - ASC(wstring): the Unicode CODEPOINT of the first character.
      Ctx.IntRegs[Instr.Dest] := Utf8FirstCP(Ctx.StringRegs[Instr.Src1]);
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
        // ⛔ A START BELOW 1 YIELDS AN EMPTY STRING - the rule the BYTE MID above has carried since it
        // was written, and its wide twin did not. Utf8SubCP CLAMPS the start to 1, so "Mid(w, 0)" and
        // "Mid(w, -1)" answered the WHOLE string where fbc answers "" (measured over start -2..5 on a
        // WString, a String and a ZString: the two byte paths were already right, only the wide one was
        // not). CLASSIC v7 keeps the clamp exactly as the byte path does - its MID$ has no such rule.
        // The test is HERE and not inside Utf8SubCP because that clamp is what LEFT$/RIGHT$ rely on.
        if (StartPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          Ctx.StringRegs[Instr.Dest] := ''
        else
        begin
          // Negative length = the rest of the string, exactly as for the byte-string MID (see above).
          if (Count < 0) and Assigned(FProgram) and FProgram.ModernMode then
            Count := Utf8CPCount(Ctx.StringRegs[Instr.Src1]) - StartPos + 1;
          Ctx.StringRegs[Instr.Dest] := Utf8SubCP(Ctx.StringRegs[Instr.Src1], StartPos, Count);
        end;
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
    53: // bcStrInstrAnyW - INSTR(wstring, Any set): the CODEPOINT position of the first codepoint of
        // Src1 that belongs to the set Src2, or 0.
        //
        // ⛔ THE BYTE TWIN CANNOT ANSWER THIS. bcStrInstrAny compares single BYTES against the set's
        // bytes, so on UTF-8 it matches a CONTINUATION byte of a multi-byte character and answers a
        // BYTE offset: on a three-codepoint Japanese string it said 1 where fbc says 2. The comparison
        // has to be per CODEPOINT on both sides, which is what these two arms are for. (The ASCII case
        // agreed all along, which is exactly why it looked like it worked.)
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];   // the character set, itself UTF-8
        Ctx.IntRegs[Instr.Dest] := Utf8FindAnyCP(S, SubStr, False);
      end;
    54: // bcStrInstrRevAnyW - INSTRREV(wstring, Any set): the LAST such codepoint, or 0.
      begin
        S := Ctx.StringRegs[Instr.Src1];
        SubStr := Ctx.StringRegs[Instr.Src2];
        Ctx.IntRegs[Instr.Dest] := Utf8FindAnyCP(S, SubStr, True);
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
        SP := @Ctx.StringRegs[Instr.Src1];   // IN PLACE: see the note at SP's declaration
        if Len < 0 then Len := 0;
        if Len > Length(SP^) then Len := Length(SP^);
        AssignSubstr(Ctx.StringRegs[Instr.Dest], SP^, Length(SP^) - Len + 1, Len);
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
        SP := @Ctx.StringRegs[Instr.Src1];   // IN PLACE: see the note at SP's declaration
        if (StartPos < 1) and Assigned(FProgram) and FProgram.ModernMode then
          Ctx.IntRegs[Instr.Dest] := 0        // FB: a start below 1 is an empty string, not the first char
        else
        begin
          if StartPos < 1 then StartPos := 1;  // CLASSIC clamps
          if Count < 0 then
          begin
            // FB: a negative length means "the rest of the string"; CLASSIC rejects it (length 0).
            if Assigned(FProgram) and FProgram.ModernMode then
              Count := Length(SP^) - StartPos + 1
            else
              Count := 0;
            if Count < 0 then Count := 0;
          end;
          if (Count <= 0) or (StartPos > Length(SP^)) then
            Ctx.IntRegs[Instr.Dest] := 0
          else
            Ctx.IntRegs[Instr.Dest] := Ord(SP^[StartPos]);
        end;
      end;
    5: // bcStrAsc
      begin
        SP := @Ctx.StringRegs[Instr.Src1];   // IN PLACE: see the note at SP's declaration
        if Length(SP^) > 0 then
          Ctx.IntRegs[Instr.Dest] := Ord(SP^[1])
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
        // IntToHex on an Int64 pads to the TYPE's width - sixteen characters whatever the value - and
        // the leading zeros used to come off one at a time, each Delete being a UniqueString plus a
        // Move of the remainder. For a small number that is a dozen of them to produce three digits.
        // IntToBaseStr emits exactly the digits there are, in one allocation, and is what OCT and BIN
        // already use, so the three now share a single implementation of the same idea.
        Ctx.StringRegs[Instr.Dest] :=
          FitBaseDigits(IntToBaseStr(Ctx.IntRegs[Instr.Src1], 16), Ctx.IntRegs[Instr.Src2]);
      end;
    10: // bcStrInstr - INSTR([start,] haystack, needle)
      begin
        // Src1 = haystack, Src2 = needle, Immediate = the int register holding the 1-based start position
        // (the 2-arg form passes a register holding 1).
        StartPos := Ctx.IntRegs[Instr.Immediate and $FFFF];
        // ⛔ A START BELOW 1 IS AN ERROR, NOT A CLAMP. fbc answers 0 for "Instr( 0, s, sub )" - the
        // position is 1-based and 0 names nothing - while clamping it to 1 SEARCHED THE WHOLE STRING and
        // answered a position the caller had asked not to look at. The 2-argument form passes a register
        // holding 1, so it is unaffected.
        if StartPos < 1 then
          Ctx.IntRegs[Instr.Dest] := 0
        else
        begin
          Ctx.IntRegs[Instr.Dest] := Pos(Ctx.StringRegs[Instr.Src2],
            Copy(Ctx.StringRegs[Instr.Src1], StartPos, MaxInt));
          if Ctx.IntRegs[Instr.Dest] > 0 then
            Inc(Ctx.IntRegs[Instr.Dest], StartPos - 1);
        end;
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
      Ctx.StringRegs[Instr.Dest] := ErrorText(Ctx.IntRegs[Instr.Src1]);
    19: // bcStrOct - OCT(n[, digits]) - octal string, full INT64 range. Src2 = digits width (0 = natural).
      Ctx.StringRegs[Instr.Dest] := FitBaseDigits(IntToBaseStr(Ctx.IntRegs[Instr.Src1], 8), Ctx.IntRegs[Instr.Src2]);
    20: // bcStrBin - BIN(n[, digits]) - binary string, full INT64 range. Src2 = digits width (0 = natural).
      Ctx.StringRegs[Instr.Dest] := FitBaseDigits(IntToBaseStr(Ctx.IntRegs[Instr.Src1], 2), Ctx.IntRegs[Instr.Src2]);
    21: // bcStrValInt - VALINT/VALLNG/VALUINT(s) - parse leading integer (0 if none).
       // Immediate carries the DECIMAL saturation width: 32 for the Long/ULong
       // spellings, 0 for the 64-bit ones. A base prefix always wraps.
      Ctx.IntRegs[Instr.Dest] := ParseLeadingInt64(Ctx.StringRegs[Instr.Src1], Integer(Instr.Immediate));
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
  FloatTmpB: Double;   // the second operand of MIN/MAX
  PackInt: Int64;      // COPYSIGN assembles its answer from bits
  SngTmp: Single;      // the bit-casts work on the binary32 value, not on the double
begin
  SubOp := Instr.OpCode and $FF;
  case SubOp of
    0: // bcMathSin
      Ctx.FloatRegs[Instr.Dest] := c_sin(Ctx.FloatRegs[Instr.Src1]);
    1: // bcMathCos
      Ctx.FloatRegs[Instr.Dest] := c_cos(Ctx.FloatRegs[Instr.Src1]);
    2: // bcMathTan
      Ctx.FloatRegs[Instr.Dest] := c_tan(Ctx.FloatRegs[Instr.Src1]);
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
      Ctx.FloatRegs[Instr.Dest] := FloorDouble(Ctx.FloatRegs[Instr.Src1]);
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
      Ctx.FloatRegs[Instr.Dest] := FixDouble(Ctx.FloatRegs[Instr.Src1]);
    19: // bcMathFrac - FRAC(x) - fractional part (keeps sign, negative zero included)
      Ctx.FloatRegs[Instr.Dest] := FracDouble(Ctx.FloatRegs[Instr.Src1]);
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
    36: // bcMathCeil - CEIL(x): toward +infinity. Written as -floor(-x) so the whole double range
        // works; Math.Ceil answers an integer and would clamp.
      Ctx.FloatRegs[Instr.Dest] := -FloorDouble(-Ctx.FloatRegs[Instr.Src1]);
    37: // bcMathRound - ROUND(x): nearest, ties to EVEN (IEEE roundTiesToEven, and WASM's f64.nearest).
        // Beyond 2^52 every double is already an integer, so the value is its own answer - and that is
        // also the range where the trip through Int64 would stop being exact.
      if Abs(Ctx.FloatRegs[Instr.Src1]) >= 4503599627370496.0 then
        Ctx.FloatRegs[Instr.Dest] := Ctx.FloatRegs[Instr.Src1]
      else
      begin
        Ctx.FloatRegs[Instr.Dest] := Double(Round(Ctx.FloatRegs[Instr.Src1]));
        // ⛔ Rounding to an integral value KEEPS the sign of a zero (IEEE 754
        // roundToIntegralTiesToEven), so Round(-0.3) is -0 and not +0. The trip through Int64
        // loses that - Round gives 0 and Double(0) is +0 - and it showed as the module answering
        // -0 where we answered 0. The module was right; same family as FIX(-0.0).
        if (Ctx.FloatRegs[Instr.Dest] = 0.0) and
           (PInt64(@Ctx.FloatRegs[Instr.Src1])^ < 0) then
          Ctx.FloatRegs[Instr.Dest] := -0.0;
      end;
    38, 39: // bcMathMin / bcMathMax - IEEE minimum / maximum.
        // ⛔ A NaN PROPAGATES and -0 ranks below +0. That is not the obvious "if a < b" reading, and it
        // is the reading chosen on purpose: it is what one machine instruction does (WASM's f64.min /
        // f64.max), so the semantics is decided where it can still be decided rather than transcribed
        // afterwards from whatever the interpreter happened to do.
      begin
        dtVal := Ctx.FloatRegs[Instr.Src1]; FloatTmpB := Ctx.FloatRegs[Instr.Src2];
        if IsNan(dtVal) or IsNan(FloatTmpB) then
          Ctx.FloatRegs[Instr.Dest] := dtVal / 0.0 * 0.0                      // a quiet NaN
        else if (dtVal = 0.0) and (FloatTmpB = 0.0) then
        begin
          // both zeros: the sign decides, and only the bits can tell them apart
          if SubOp = 38 then
          begin
            if (PInt64(@dtVal)^ < 0) then Ctx.FloatRegs[Instr.Dest] := dtVal
            else Ctx.FloatRegs[Instr.Dest] := FloatTmpB;
          end
          else
          begin
            if (PInt64(@dtVal)^ < 0) then Ctx.FloatRegs[Instr.Dest] := FloatTmpB
            else Ctx.FloatRegs[Instr.Dest] := dtVal;
          end;
        end
        else if SubOp = 38 then
        begin
          if dtVal < FloatTmpB then Ctx.FloatRegs[Instr.Dest] := dtVal
          else Ctx.FloatRegs[Instr.Dest] := FloatTmpB;
        end
        else
        begin
          if dtVal > FloatTmpB then Ctx.FloatRegs[Instr.Dest] := dtVal
          else Ctx.FloatRegs[Instr.Dest] := FloatTmpB;
        end;
      end;
    40: // bcMathCopySign - the magnitude of x with the sign of y, bit for bit (WASM's f64.copysign).
      begin
        PackInt := (PInt64(@Ctx.FloatRegs[Instr.Src1])^ and $7FFFFFFFFFFFFFFF) or
                   (PInt64(@Ctx.FloatRegs[Instr.Src2])^ and Int64($8000000000000000));
        Ctx.FloatRegs[Instr.Dest] := PDouble(@PackInt)^;
      end;
    41: // bcSingleBits - the 32 bits of a SINGLE, as an integer. The value is a double that a
        // SINGLE can hold, so narrowing it first is what makes the answer well defined.
      begin
        SngTmp := Ctx.FloatRegs[Instr.Src1];   // narrows to binary32, which is the value in question
        Ctx.IntRegs[Instr.Dest] := Int64(PLongWord(@SngTmp)^);
      end;
    42: // bcBitsToSingle - the SINGLE those 32 bits spell, widened to the float bank.
      begin
        PackInt := Ctx.IntRegs[Instr.Src1] and $FFFFFFFF;
        Ctx.FloatRegs[Instr.Dest] := PSingle(@PackInt)^;
      end;
    20: // bcDateNow - Immediate 0=NOW (date+time serial), 1=TIMER (seconds since midnight)
      begin
        // ⭐ SB_FAKE_CLOCK=1 makes the clock ADVANCE BY A FIXED STEP instead of reading the wall
        // clock, so NOW/TIMER return a deterministic sequence and any program that measures itself
        // prints the same numbers on every run.
        //
        // ⛔ THE REASON IT EXISTS: job/tests/bench holds the programs that drive the AOT hardest,
        // and NOT ONE of them could be checked by a net, because almost every one prints elapsed
        // milliseconds. Measured: of the first 40, most produce five different outputs in five
        // IDENTICAL runs. So the hottest code in the compiler had no output comparison at all, and
        // a divergence there could only ever be found by accident - which is exactly how the
        // string-argument clobber surfaced.
        //
        // The step is 1 ms per reading rather than a constant: several of those programs DIVIDE by
        // the elapsed time (MFLOP/s, ns per cell), and a frozen clock turns the net into a division
        // by zero instead of a comparison.
        if FFakeClock then
        begin
          // ⛔ Computed in the unit the caller asked for, NOT as a day serial run through
          // Frac()*86400: a 1 ms step is 1.16e-8 of a day, and going out to a ~45000 serial and
          // back loses it - the first version of this printed "0 ms" and an MFLOP/s of Int64.MinValue
          // (an overflowed divide by an elapsed time of exactly zero).
          FFakeClockTicks := FFakeClockTicks + 1;
          if Instr.Immediate = 1 then
            Ctx.FloatRegs[Instr.Dest] := FFakeClockTicks * 0.001            // TIMER: seconds
          else
            Ctx.FloatRegs[Instr.Dest] := 45000.0 + FFakeClockTicks * 0.001 / 86400.0;  // NOW: serial
        end
        else
        begin
          dtVal := Now + FClockOffsetDays;
          if Instr.Immediate = 1 then
            Ctx.FloatRegs[Instr.Dest] := Frac(dtVal) * 86400.0   // TIMER
          else
            Ctx.FloatRegs[Instr.Dest] := dtVal;                  // NOW
        end
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
      { ⛔⛔⛔ TWO DEFECTS LIVED IN THIS ONE LINE, and the second is the one
        nobody would look for.

        1. FPC types an unsuffixed real constant by BEST FIT, so 3600.0, 60.0 and
           86400.0 were all SINGLE - and an Int64 times a Single is a SINGLE
           multiplication, so the whole expression was evaluated in 24 bits and
           only then widened into the Double register. TimeSerial(0,0,1) answered
           exactly Single(1/86400): 3EE845C8A0000000 against 3EE845C8A0CE5129.
           ⇒ The numerator is now INTEGER arithmetic, which cannot fall into a
           narrower real type at all.
        2. Dividing by a LITERAL let the optimiser turn the division into a
           MULTIPLICATION BY THE RECIPROCAL, and 1/86400 is not exact - so the
           answer landed one ulp off the IEEE quotient (…F31 against …F32) on a
           value where the first defect was already cured. ⇒ The divisor is a
           TYPED constant, i.e. a variable, which the optimiser cannot fold.

        ⚠️ Both were found by compiling this to WebAssembly, where the module was
        right twice: it has no narrower real type to fall into, and no reciprocal
        rewrite. And neither shows up by reading the code - the signature says
        Double, the destination is Double, and only what happens in between is not.
        ⚠️ Trap 1 needs an INTEGER on the other side (Double * Single promotes to
        Double), which is why the neighbouring DATEDIFF arms were unaffected. }
      Ctx.FloatRegs[Instr.Dest] :=
        (Ctx.IntRegs[Instr.Src1] * 3600 + Ctx.IntRegs[Instr.Src2] * 60 +
         Ctx.IntRegs[Instr.Immediate]) / SECS_PER_DAY_D;
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
          { ⛔ Double() for the reason spelled out at bcTimeSerial: n is an
            INTEGER, so without the cast the division is done in SINGLE and only
            then added to a Double. DateAdd("s", 30, x) was one ulp out. }
          { ⛔ TYPED constants, not literals: dividing by a literal lets the
            optimiser multiply by the reciprocal instead, and 1/24, 1/1440 and
            1/86400 are none of them exact. See bcTimeSerial for the measurement. }
          7: dtVal := dtVal + n / HOURS_PER_DAY_D;        // h
          8: dtVal := dtVal + n / MINS_PER_DAY_D;         // n (minute)
          9: dtVal := dtVal + n / SECS_PER_DAY_D;         // s
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
          // ww - ⛔ NOT WeekOfTheYear. That is ISO 8601: weeks start on MONDAY and week 1 is the one
          // holding the first Thursday. VB and fbc use a different definition entirely - week 1 is the
          // week CONTAINING 1 January and weeks start on SUNDAY - and the two agree most of the time,
          // which is why this survived: measured 23 Aug 2026 over 48 dates, 7 differed and every one
          // of them was a SUNDAY, the day the VB week turns over and the ISO one does not.
          //   offset from the Sunday that opens week 1 = (dayOfYear - 1) + (weekday(Jan 1) - 1)
          6: Ctx.IntRegs[Instr.Dest] :=
               ((DayOfTheYear(dtVal) - 1) + (DayOfWeek(EncodeDate(YearOf(dtVal), 1, 1)) - 1)) div 7 + 1;
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
  FArrays[ArrayIdx].IsDynamic := True;   // a REDIM'd array is DYNAMIC: ERASE frees it (see EraseArray)
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
  if (ArrayIdx >= 0) and (ArrayIdx < Length(FArrays)) then
    FArrays[ArrayIdx].IsDynamic := True;   // as RedimArray: a REDIM'd array is DYNAMIC
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
  InstrHot: PBytecodeInstruction;   // what ArrayHotOps.inc dereferences; see the note at its include
                                    // in RunTemplate.inc - the same text is compiled into two scopes.
  ArrMapP: PInteger;                // ...and so is the array-id map alias, for the same reason.
begin
  InstrHot := @Instr;
  if Length(Ctx.ArrMap) > 0 then ArrMapP := @Ctx.ArrMap[0] else ArrMapP := nil;
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
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
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
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
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
        // ⛔ TWO ids, and they are not interchangeable. The DECLARATION (element type, rank, bounds)
        // is looked up by the LOGICAL id, because the compiler's array table is indexed that way; the
        // STORAGE is written at the PHYSICAL slot this context owns. Mapping before the lookup reads
        // the declaration of a slot that has none - and the first thing that happened was every
        // worker dying on "Invalid array index".
        if (Instr.Src1 < 0) or (Instr.Src1 >= FProgram.GetArrayCount) then
          raise Exception.CreateFmt('Invalid array index: %d', [Instr.Src1]);
        ArrInfo := FProgram.GetArray(Instr.Src1);
        ArrayIdx := Ctx.ArrMap[Instr.Src1];
        if GArrPrivDiag then
          WriteLn(ErrOutput, Format('[arrpriv] DIM ARR[%d] -> phys %d (ctx=%p)',
                  [Instr.Src1, ArrayIdx, Pointer(Ctx)]));
        if ArrayIdx >= Length(FArrays) then
          SetLength(FArrays, ArrayIdx + 1);
        // ⛔ A PRIVATE array is one storage PER CONTEXT, and every recursion level runs in the same
        // context: DIMming it here would destroy the values of the invocation that called us. Push
        // what is in the slot and start clean; FramePop puts it back. Copying the record is O(1) -
        // its dynamic fields share by reference - and clearing them is what makes the new DIM
        // ALLOCATE instead of resizing the storage we just saved.
        if (Instr.Src1 < Length(FArrPrivSlot)) and (FArrPrivSlot[Instr.Src1] >= 0) then
        begin
          if Ctx.ArrPrivSaveTop >= Length(Ctx.ArrPrivSave) then
            SetLength(Ctx.ArrPrivSave, (Ctx.ArrPrivSaveTop + 1) * 2);
          Ctx.ArrPrivSave[Ctx.ArrPrivSaveTop].SlotId := ArrayIdx;
          Ctx.ArrPrivSave[Ctx.ArrPrivSaveTop].Saved := FArrays[ArrayIdx];
          Inc(Ctx.ArrPrivSaveTop);
          if GArrPrivDiag then
            WriteLn(ErrOutput, Format('[arrpriv] SALVA phys %d, pila -> %d', [ArrayIdx, Ctx.ArrPrivSaveTop]));
          FArrays[ArrayIdx] := Default(TArrayStorage);
        end;
        FArrays[ArrayIdx].ElementType := Byte(ArrInfo.ElementType);
        FArrays[ArrayIdx].DimCount := ArrInfo.DimCount;
        // Fixed or dynamic, stamped on the STORAGE: ERASE through an array PARAMETER asks the storage,
        // because the answer is the CALLER's (see EraseArray and the Immediate-2 case).
        FArrays[ArrayIdx].IsDynamic := ArrInfo.IsDynamicShape;
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
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
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
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
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
      // Immediate 2 = "the compiler could not tell": the name is an array PARAMETER, and whether ERASE
      // frees or merely resets is the CALLER's array's property. The bind copied the storage record
      // whole, so the answer travels with it. fbc suite string/string-array-erase-arg.
      if Instr.Immediate = 2 then
        EraseArray(Ctx.ArrMap[Instr.Src1], FArrays[Ctx.ArrMap[Instr.Src1]].IsDynamic)
      else
        EraseArray(Ctx.ArrMap[Instr.Src1], Instr.Immediate <> 0);
    12: // bcArrayRedim - REDIM [PRESERVE] arr([lb TO] ub) (B1.4); Src2=ub reg. Immediate: bit0=preserve,
        // bit1=has explicit lower bound, bits8+ = that (non-negative) lower bound. A RUNTIME lower bound
        // arrives via a preceding bcArrayRedimPush (LB flag) in FRedimPendingLBs and takes precedence.
      begin
        if Length(FRedimPendingLBs) > 0 then
        begin
          RedimArray(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0,
                     True, FRedimPendingLBs[0]);
          SetLength(FRedimPendingLBs, 0);
        end
        else
          RedimArray(Ctx.ArrMap[Instr.Src1], Ctx.IntRegs[Instr.Src2], (Instr.Immediate and 1) <> 0,
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
          Ctx.IntRegs[Instr.Dest] := RecFieldInt(Rec, RecSlot);
        end
        // ⛔ ...AND A RAW ADDRESS IS A THIRD KIND. An @-taken LOCAL is a raw byte slot (RAWPTR_TAG,
        // bit 62), and the deref lowered from a NAME knows that; the one lowered from a VALUE cannot,
        // because there is no name left to ask. So "*p" worked and "**pp" did not: the inner deref
        // handed back p's value - a correctly tagged raw address - and this arm decoded it as a packed
        // array pointer, whose array id is then nonsense ("Null or invalid pointer dereference,
        // address 4611686018427387920"). The tag is IN the value, so the question is answered here,
        // where every path that produces one arrives.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          Ctx.IntRegs[Instr.Dest] := RawLoadInt(PtrAddr, 0)
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          // ⛔ ...AND THE BANK OF THE POINTER NEED NOT BE THE BANK OF THE STORAGE. These six arms chose
          // which vector to read from the OPCODE, while a TArrayStorage populates exactly ONE of
          // IntData / FloatData / StringData - so "*CPtr(ULongInt Ptr, @d)" over a Double reached an
          // INT arm, found IntData empty and reported the FLOAT bank's tag as a bad address. Type
          // punning is the idiom fbc's own suite uses everywhere, and it was impossible by
          // construction. The vector that IS populated is the discriminator, so no extra field is
          // needed: fall through to it and REINTERPRET the eight bytes, which is what fbc does.
          // ⚠️ DECLARED LIMIT: a SINGLE is stored here as an 8-byte Double, so punning one through a
          // ULong Ptr still differs from fbc's 4-byte IEEE754 image (DIVERGENZE 55). Double <-> Int64,
          // which is what numbers/infnan and numbers/limits use, is exact.
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
            Ctx.IntRegs[Instr.Dest] := FArrays[ArrayIdx].IntData[PtrOffset]
          else if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
            Ctx.IntRegs[Instr.Dest] := PInt64(@FArrays[ArrayIdx].FloatData[PtrOffset])^
          else
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
        end;
      end;
    14: // bcRefLoadFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Ctx.FloatRegs[Instr.Dest] := RecFieldFloat(Rec, RecSlot);
        end
        // The raw-address kind - see the note in bcRefLoadInt above.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          Ctx.FloatRegs[Instr.Dest] := RawLoadFloat(PtrAddr, 0)
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          // The bank of the pointer need not be the bank of the storage - see bcRefLoadInt above.
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
            Ctx.FloatRegs[Instr.Dest] := FArrays[ArrayIdx].FloatData[PtrOffset]
          else if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
            Ctx.FloatRegs[Instr.Dest] := PDouble(@FArrays[ArrayIdx].IntData[PtrOffset])^
          else
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
        end;
      end;
    15: // bcRefLoadString - Imm 1 = the pointee is a WSTRING (only consulted for a RAW address)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Ctx.StringRegs[Instr.Dest] := Rec^.StringData[RecSlot];
        end
        // ⛔ THE RAW THIRD KIND, WHICH THIS ARM ALONE DID NOT KNOW. bcRefLoadInt/Float learned it and
        // say so in the note above - "the tag is IN the value, so the question is answered here, where
        // every path that produces one arrives" - and the STRING arm was never visited. A BYREF cast
        // written "Operator = *This.p" over a CAllocate'd ZString hands back a correctly tagged raw
        // address, and this decoded it as a packed array pointer: "Null or invalid pointer
        // dereference, address 4611686018427387944". Text at a raw address is a C string.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          Ctx.StringRegs[Instr.Dest] := RawLoadZStrVal(PtrAddr, Instr.Immediate = 1)
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
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
          RecSetFieldInt(Rec, RecSlot, Ctx.IntRegs[Instr.Src2]);
        end
        // The raw-address kind - see the note in bcRefLoadInt above. The WRITE half must know it too,
        // or "**pp = 5" stores into a nonexistent array while "*p = 5" works.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          RawStoreInt(PtrAddr, 0, Ctx.IntRegs[Instr.Src2])
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          // The bank of the pointer need not be the bank of the storage - see bcRefLoadInt above. The
          // WRITE half needs it too, or "*Cast(ULongInt Ptr, @d) = bits" raises where the read works.
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
            FArrays[ArrayIdx].IntData[PtrOffset] := Ctx.IntRegs[Instr.Src2]
          else if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
            PInt64(@FArrays[ArrayIdx].FloatData[PtrOffset])^ := Ctx.IntRegs[Instr.Src2]
          else
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
        end;
      end;
    17: // bcRefStoreFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          RecSetFieldFloat(Rec, RecSlot, Ctx.FloatRegs[Instr.Src2]);
        end
        // The raw-address kind - see the note in bcRefLoadInt above.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          RawStoreFloat(PtrAddr, 0, Ctx.FloatRegs[Instr.Src2])
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
          PtrOffset := PtrAddr and POINTER_OFFSET_MASK;
          // The bank of the pointer need not be the bank of the storage - see bcRefLoadInt above.
          if (ArrayIdx < 0) or (ArrayIdx > High(FArrays)) or (PtrOffset < 0) then
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
          if PtrOffset <= High(FArrays[ArrayIdx].FloatData) then
            FArrays[ArrayIdx].FloatData[PtrOffset] := Ctx.FloatRegs[Instr.Src2]
          else if PtrOffset <= High(FArrays[ArrayIdx].IntData) then
            PDouble(@FArrays[ArrayIdx].IntData[PtrOffset])^ := Ctx.FloatRegs[Instr.Src2]
          else
            raise ERangeError.CreateFmt('Null or invalid pointer dereference (address %d)', [PtrAddr]);
        end;
      end;
    18: // bcRefStoreString - Imm 1 = the pointee is a WSTRING (only consulted for a RAW address)
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if PtrAddr < 0 then
        begin
          Rec := RecPtrTarget(Ctx, PtrAddr, RecSlot);
          Rec^.StringData[RecSlot] := Ctx.StringRegs[Instr.Src2];
        end
        // The raw-address kind - see the note in bcRefLoadString above. The WRITE half needs it too,
        // or LSET on such a UDT reads its buffer and then stores into a nonexistent array.
        else if (PtrAddr and RAWPTR_TAG) <> 0 then
          RawStoreZStrVal(PtrAddr, Ctx.StringRegs[Instr.Src2], Instr.Immediate = 1)
        else
        begin
          ArrayIdx := MapArrDyn(Ctx, (PtrAddr shr POINTER_ARRAY_SHIFT) - 1);
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
    // ⛔ A NEGATIVE ADDRESS IS NOT RAW MEMORY: it is a RECORD-FIELD pointer (RECPTR_TAG, bit 63), what
    // "@obj.field" yields for a managed record. bcRefLoadInt has told the three domains apart for a
    // while - its own comment says "the tag is IN the value, so the question is answered here, where
    // every path that produces one arrives" - and the RAW arm never learnt the same thing. So
    // "@a.i" put in a pointer VARIABLE worked and the same address put in a pointer FIELD of another
    // UDT died on "Null or invalid raw pointer dereference": the compiler classifies a pointer FIELD as
    // raw and emits this opcode, and only the value knows better. Measured with a 5-variant deck: the
    // combination "record-field pointer inside a record field" was the only one that broke.
    23: // bcRawLoadInt
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if (PtrAddr and RAWPTR_TAG) <> 0 then
          Ctx.IntRegs[Instr.Dest] := RawLoadInt(PtrAddr, Instr.Immediate)   // a real raw address: it carries the WIDTH
        else
          Ctx.IntRegs[Instr.Dest] := PtrDomainLoadInt(Ctx, PtrAddr);
      end;
    24: // bcRawLoadFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if (PtrAddr and RAWPTR_TAG) <> 0 then
          Ctx.FloatRegs[Instr.Dest] := RawLoadFloat(PtrAddr, Instr.Immediate)
        else
          Ctx.FloatRegs[Instr.Dest] := PtrDomainLoadFloat(Ctx, PtrAddr);
      end;
    // The WRITE half of the same rule, and it must be here too - "*g.pi = 33" through a pointer FIELD
    // holding "@obj.field" wrote into a nonexistent raw block while the READ, once fixed, worked.
    25: // bcRawStoreInt
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if (PtrAddr and RAWPTR_TAG) <> 0 then
          RawStoreInt(PtrAddr, Instr.Immediate, Ctx.IntRegs[Instr.Src2])
        else
          PtrDomainStoreInt(Ctx, PtrAddr, Ctx.IntRegs[Instr.Src2]);
      end;
    26: // bcRawStoreFloat
      begin
        PtrAddr := Ctx.IntRegs[Instr.Src1];
        if (PtrAddr and RAWPTR_TAG) <> 0 then
          RawStoreFloat(PtrAddr, Instr.Immediate, Ctx.FloatRegs[Instr.Src2])
        else
          PtrDomainStoreFloat(Ctx, PtrAddr, Ctx.FloatRegs[Instr.Src2]);
      end;
    31: // bcRawMemCopy - FB_MEMCOPY(dst, src, bytes); Dest receives dst (FB returns the destination)
      begin
        RawMemCopy(Ctx, Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], PtrUInt(Ctx.IntRegs[Instr.Immediate]));
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
      end;
    32: // bcRawMemMove - FB_MEMMOVE(dst, src, bytes); overlap-safe
      begin
        RawMemCopy(Ctx, Ctx.IntRegs[Instr.Src1], Ctx.IntRegs[Instr.Src2], PtrUInt(Ctx.IntRegs[Instr.Immediate]));
        Ctx.IntRegs[Instr.Dest] := Ctx.IntRegs[Instr.Src1];
      end;
    33: // bcRawClear - CLEAR(dst, value, bytes)
      RawClear(Ctx, Ctx.IntRegs[Instr.Src1], Byte(Ctx.IntRegs[Instr.Src2]), PtrUInt(Ctx.IntRegs[Instr.Immediate]));
    50: // bcRawLoadZStr - Dest(str) = C string at RawAddr(IntRegs[Src1]); Imm 1 = WSTRING (wide cells).
        // Imm -1 is a MANAGED STRING CELL ("String Ptr"), not text in the heap: see RawStrCellGet.
        // Immediate >= 2 asks for EXACTLY (Immediate - 2) bytes instead of "up to the terminator": that
        // is what a fixed-length string FIELD of a UDT laid over raw memory is - n bytes, terminator or
        // not, which is why "As String*5 sig" over "GIF89a" reads "GIF89" and misses a character.
      // ⭐ A NEGATIVE address is not raw memory at all: it is a RECORD-FIELD pointer (RECPTR_TAG,
      // bit 63), which is what "@obj.field" yields for a MANAGED record. A raw byte address carries
      // RAWPTR_TAG (bit 62) and so is never negative - the two domains are told apart here exactly as
      // bcRefLoad*/bcRefStore* already tell them apart. Without this "*Cast(ZString Ptr, @_data)",
      // which is how fbc's OWN udt-zstring reference implementation reads a fixed-length field, took a
      // field pointer for a heap offset and raised "Null or invalid raw pointer dereference".
      // The exact-byte-count form still means "the field's DECLARED width", so the content is padded
      // with NULs or cut to it; the managed slot holds the content and has no padding of its own.
      if Ctx.IntRegs[Instr.Src1] < 0 then
      begin
        Rec := RecPtrTarget(Ctx, Ctx.IntRegs[Instr.Src1], RecSlot);
        Ctx.StringRegs[Instr.Dest] := Rec^.StringData[RecSlot];
        if Instr.Immediate >= 2 then
        begin
          if Length(Ctx.StringRegs[Instr.Dest]) > Instr.Immediate - 2 then
            Ctx.StringRegs[Instr.Dest] := Copy(Ctx.StringRegs[Instr.Dest], 1, Instr.Immediate - 2)
          else if Length(Ctx.StringRegs[Instr.Dest]) < Instr.Immediate - 2 then
            Ctx.StringRegs[Instr.Dest] := Ctx.StringRegs[Instr.Dest] +
              StringOfChar(#0, Instr.Immediate - 2 - Length(Ctx.StringRegs[Instr.Dest]));
        end;
      end
      else if Instr.Immediate = -1 then
        Ctx.StringRegs[Instr.Dest] := RawStrCellGet(Ctx.IntRegs[Instr.Src1])
      // ⭐ ...and the THIRD domain: a positive address with no RAWPTR_TAG is a PACKED ARRAY pointer,
      // which is what "@foo(0)" yields. See PtrDomainLoadZStr. DIVERGENZE 127.
      // ⚠️ ...and NOT for address 0, which has a DEFINED answer of its own further down (fbc's string
      // runtime tests the pointer, so a null ZSTRING reads as the empty string). Gated on non-zero so
      // that rule keeps its own path, exactly as it had it.
      else if (Ctx.IntRegs[Instr.Src1] <> 0) and ((Ctx.IntRegs[Instr.Src1] and RAWPTR_TAG) = 0) then
        Ctx.StringRegs[Instr.Dest] := PtrDomainLoadZStr(Ctx, Ctx.IntRegs[Instr.Src1],
                                        Instr.Immediate = 1,
                                        Ord(Instr.Immediate >= 2) * (Instr.Immediate - 2))
      else if Instr.Immediate >= 2 then
        Ctx.StringRegs[Instr.Dest] := RawLoadBytesVal(Ctx.IntRegs[Instr.Src1], Instr.Immediate - 2)
      else
        Ctx.StringRegs[Instr.Dest] := RawLoadZStrVal(Ctx.IntRegs[Instr.Src1], Instr.Immediate = 1);
    51: // bcRawStoreZStr - StringRegs[Src2] chars + NUL -> RawAddr(IntRegs[Src1]); Imm 1 = WSTRING,
        // Imm -1 a MANAGED STRING CELL ("String Ptr" - see RawStrCellSet).
      // ...and the write half of the same discrimination: a negative address is the MANAGED field
      // itself, so the characters go into its slot rather than into bytes that do not exist.
      if Ctx.IntRegs[Instr.Src1] < 0 then
      begin
        Rec := RecPtrTarget(Ctx, Ctx.IntRegs[Instr.Src1], RecSlot);
        Rec^.StringData[RecSlot] := Ctx.StringRegs[Instr.Src2];
      end
      else if Instr.Immediate = -1 then
        RawStrCellSet(Ctx.IntRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2])
      else if (Ctx.IntRegs[Instr.Src1] <> 0) and ((Ctx.IntRegs[Instr.Src1] and RAWPTR_TAG) = 0) then
        PtrDomainStoreZStr(Ctx, Ctx.IntRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2],
                           Instr.Immediate = 1)
      else
        RawStoreZStrVal(Ctx.IntRegs[Instr.Src1], Ctx.StringRegs[Instr.Src2], Instr.Immediate = 1);
    34: // bcArrayBind - array BYREF param (PHASE 1): save FArrays[Src1] and snapshot the arg FArrays[Immediate],
      begin  // but DEFER the alias to bcArrayBindApply. Two-phase so a batch of binds that swaps arrays
             // (recursive "proc(a(),b())" -> "proc(b(),a())", where param and arg slots coincide) reads every
             // arg from the UNMODIFIED table before any assignment. Src1=param id, Imm=arg id.
        if (Instr.Src1 >= 0) and (Instr.Immediate >= 0) and (Instr.Immediate <= High(FArrays)) then
        begin
          // Both ids are logical. The ARGUMENT in particular may be a proc-local array being passed
          // on, so it has to name this context's copy and not the dead compile-time slot.
          ArrayIdx := Ctx.ArrMap[Instr.Src1];
          LinearIdx := Ctx.ArrMap[Instr.Immediate];
          // The param placeholder array is never runtime-DIM'd, so grow FArrays to hold its slot.
          if ArrayIdx > High(FArrays) then SetLength(FArrays, ArrayIdx + 1);
          if Ctx.ArrayBindTop >= Length(Ctx.ArrayBindStack) then
            SetLength(Ctx.ArrayBindStack, (Ctx.ArrayBindTop + 1) * 2);
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].SlotId := ArrayIdx;
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId := LinearIdx;
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved := FArrays[ArrayIdx];        // dyn-array fields share by ref
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot := FArrays[LinearIdx];    // the arg, captured now
          Inc(Ctx.ArrayBindTop);
        end;
      end;
    49: // bcArrayBindInd - PHASE 1 bind whose arg is a UDT ARRAY MEMBER: its FArrays handle is only known at
      begin  // runtime (per instance), so it arrives in a register instead of an immediate. Src1=param id,
             // Src2=handle reg. Always pushes a save-stack entry — bcArrayBindApply commits a FIXED count and
             // bcArrayUnbind pops LIFO by SlotId, so skipping a push here would desynchronize both.
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src2]);
        if Instr.Src1 >= 0 then
        begin
          ArrayIdx := Ctx.ArrMap[Instr.Src1];
          if ArrayIdx > High(FArrays) then SetLength(FArrays, ArrayIdx + 1);  // grow AFTER reading the handle
          if Ctx.ArrayBindTop >= Length(Ctx.ArrayBindStack) then
            SetLength(Ctx.ArrayBindStack, (Ctx.ArrayBindTop + 1) * 2);
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].SlotId := ArrayIdx;
          Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved := FArrays[ArrayIdx];
          if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) then
          begin
            Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId := PtrAddr;
            Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot := FArrays[PtrAddr];   // alias the member's storage
          end
          else
          begin  // handle < 1 = member array never allocated: bind an EMPTY array (UBOUND = -1), and set
                 // ArgId = -1 so unbind performs no copy-back (there is no caller slot to write to).
            Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId := -1;
            ClearArrayStorage(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot);
          end;
          Inc(Ctx.ArrayBindTop);
        end;
      end;
    36: // bcArrayBindApply - commit the top N pending binds (Immediate=N): alias each param slot to its
      begin  // snapshotted arg. All snapshots were captured (in phase 1) from the unmodified table.
        for I := Ctx.ArrayBindTop - Instr.Immediate to Ctx.ArrayBindTop - 1 do
          if (I >= 0) and (Ctx.ArrayBindStack[I].SlotId <= High(FArrays)) then
            FArrays[Ctx.ArrayBindStack[I].SlotId] := Ctx.ArrayBindStack[I].Snapshot;  // alias: share the caller's data
      end;
    35: // bcArrayUnbind - restore the last saved FArrays[Src1] (Src1 = param array id).
      begin
        ArrayIdx := Ctx.ArrMap[Instr.Src1];
        if (Ctx.ArrayBindTop > 0) and (Ctx.ArrayBindStack[Ctx.ArrayBindTop - 1].SlotId = ArrayIdx) then
        begin
          Dec(Ctx.ArrayBindTop);
          // Propagate the callee's final array back to the caller's slot ONLY if a REDIM [PRESERVE]
          // reallocated the param's storage — detected by its data no longer sharing the reference we
          // snapshotted from the arg at bind time. Without a resize the caller already sees the writes via
          // the shared reference, and copying would be wrong: in deep recursion the arg slot may have been
          // rebound at an outer level (merge sort's swapped a()/b()), so an unconditional copy corrupts it.
          if (Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId >= 0) and
             (Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId <= High(FArrays)) and
             (Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId <> ArrayIdx) and
             not ArrayDataShared(FArrays[ArrayIdx], Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot) then
            FArrays[Ctx.ArrayBindStack[Ctx.ArrayBindTop].ArgId] := FArrays[ArrayIdx];
          FArrays[ArrayIdx] := Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved;
          // Release the saved/snapshot copies' references (ownership transferred back to the live slots).
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved.IntData, 0);
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved.FloatData, 0);
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Saved.StringData, 0);
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot.IntData, 0);
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot.FloatData, 0);
          SetLength(Ctx.ArrayBindStack[Ctx.ArrayBindTop].Snapshot.StringData, 0);
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
        RedimArrayN(Ctx.ArrMap[Instr.Src1], FRedimPendingUBs, (Instr.Immediate and 1) <> 0, FRedimPendingLBs);
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
        ArrayIdx := Ctx.ArrMap[Instr.Src1];   // logical -> this context's physical slot
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
        // ⛔ AN ELEMENT INDEX IS RELATIVE TO THE ARRAY'S LOWER BOUND, and a member array used to be
        // documented as "0-based (v1), so no lower-bound subtraction is needed" - true only while
        // nothing could give one a non-zero bound. "ReDim obj.field(3 To 5)" now can (the member arm
        // of ProcessRedim threw its lower bound away until 1 Sep 2026), so the subtraction is real.
        // ⭐ It belongs HERE and not in the SSA: this opcode already holds the descriptor, so it costs
        // one read and one subtract instead of two extra instructions per access - and it is the
        // identity for every array whose lower bound is 0, which is all of them today.
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].IntData[LinearIdx]
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    38: // bcArrayLoadIndFloat
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.FloatRegs[Instr.Dest] := FArrays[PtrAddr].FloatData[LinearIdx]
        else
          Ctx.FloatRegs[Instr.Dest] := 0.0;
      end;
    39: // bcArrayLoadIndString
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          Ctx.StringRegs[Instr.Dest] := FArrays[PtrAddr].StringData[LinearIdx]
        else
          Ctx.StringRegs[Instr.Dest] := '';
      end;
    40: // bcArrayStoreIndInt (Dest = value register, READ)
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].IntData[LinearIdx] := Ctx.IntRegs[Instr.Dest];
      end;
    41: // bcArrayStoreIndFloat (Dest = value register, READ)
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].FloatData[LinearIdx] := Ctx.FloatRegs[Instr.Dest];
      end;
    42: // bcArrayStoreIndString (Dest = value register, READ)
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and (Length(FArrays[PtrAddr].LowerBounds) > 0) then
          LinearIdx := LinearIdx - FArrays[PtrAddr].LowerBounds[0];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and ArrayBoundsOK(PtrAddr, LinearIdx) then
          FArrays[PtrAddr].StringData[LinearIdx] := Ctx.StringRegs[Instr.Dest];
      end;
    43: // bcArrayIdxResolveInd - member multi-dim linear index from the handle array's CURRENT dimensions
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]);
        LinearIdx := 0;
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) then
          for i := 0 to High(FIdxPending) do
          begin
            ProdDims := 1;
            for ArrLowerBound := i + 1 to High(FArrays[PtrAddr].Dimensions) do
              ProdDims := ProdDims * FArrays[PtrAddr].Dimensions[ArrLowerBound];
            // ...and the same subtraction per dimension, for the same reason as the element opcodes.
            ArrLowerBound := 0;
            if i <= High(FArrays[PtrAddr].LowerBounds) then
              ArrLowerBound := FArrays[PtrAddr].LowerBounds[i];
            LinearIdx := LinearIdx + (FIdxPending[i] - ArrLowerBound) * ProdDims;
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
          PtrAddr := RecFieldInt(Rec, RecSlot);
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
            RecSetFieldInt(Rec, RecSlot, PtrAddr);
          end;
          RedimArrayN(PtrAddr, FRedimPendingUBs, (Instr.Immediate and 1) <> 0, FRedimPendingLBs);
          if GArrPrivDiag then
            WriteLn(ErrOutput, Format('[arrpriv] MEMBRO rec=%p slot=%d -> phys %d size=%d',
                    [Pointer(Rec), RecSlot, PtrAddr, FArrays[PtrAddr].TotalSize]));
        end
        else if GArrPrivDiag then
          WriteLn(ErrOutput, Format('[arrpriv] MEMBRO ⛔ record NULLO (handle=%d slot=%d)',
                  [Ctx.IntRegs[Instr.Src1], RecSlot]));
        SetLength(FRedimPendingUBs, 0);
        SetLength(FRedimPendingLBs, 0);
      end;
    45: // bcArrayLBoundInd - LBOUND of a UDT array member (Src1=handle reg, Src2=dim reg)
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and
           (LinearIdx >= 0) and (LinearIdx <= High(FArrays[PtrAddr].LowerBounds)) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].LowerBounds[LinearIdx]
        else
          Ctx.IntRegs[Instr.Dest] := 0;
      end;
    46: // bcArrayUBoundInd - UBOUND of a UDT array member (upper = lower + size - 1; -1 if unallocated)
      begin
        PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); LinearIdx := Ctx.IntRegs[Instr.Src2];
        if (PtrAddr >= 1) and (PtrAddr <= High(FArrays)) and
           (LinearIdx >= 0) and (LinearIdx <= High(FArrays[PtrAddr].Dimensions)) then
          Ctx.IntRegs[Instr.Dest] := FArrays[PtrAddr].LowerBounds[LinearIdx]
                                     + FArrays[PtrAddr].Dimensions[LinearIdx] - 1
        else
          Ctx.IntRegs[Instr.Dest] := -1;
      end;
    47: // bcArrayCopyContents - deep-copy FArrays[Src1] <- FArrays[Src2] (value semantics of an array member)
      begin
        DestArr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]); PtrAddr := MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src2]);
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
      DeepCopyArrayRecords(Ctx, MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src1]),
                                MapArrDyn(Ctx, Ctx.IntRegs[Instr.Src2]), Instr.Immediate);
  else
    raise Exception.CreateFmt('Unknown array opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;
  // ⛔ AND AGAIN, AFTER THE WORK. The flag above says "the table is about to change"; this one says
  // "it has changed". Both are needed because the rebuild that CONSUMES the flag also CLEARS it, and
  // it runs on another thread: a worker that set the flag and then allocated its elements could have
  // the flag cleared by a rebuild that ran between the two - publishing a descriptor with a NULL data
  // pointer, which compiled code then dereferences.
  // 📊 Found 21 Aug 2026 while giving proc-local arrays per-thread storage: four workers DIMming four
  // different slots at once, and the guard program either read another thread's data or died in the
  // JIT on `mov (%rdx,%rcx,8)` with rdx = 0. The window existed before - every worker DIMmed the same
  // slot, so a stale entry was overwritten by the next DIM instead of staying null.
  FArraysDirty := True;
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
        // ⛔ ...and in MODERN only when a person is there: see GStdinIsTerminal below.
        if Assigned(FOutputDevice) and
           (GStdinIsTerminal or not (Assigned(FProgram) and FProgram.ModernMode)) then
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
        // ⛔ ...and in MODERN only when a person is there: see GStdinIsTerminal below.
        if Assigned(FOutputDevice) and
           (GStdinIsTerminal or not (Assigned(FProgram) and FProgram.ModernMode)) then
        begin
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        // ⭐⭐ MODERN NEVER REFUSES AN INPUT. fbc parses what it can and leaves 0 for the rest - there
        // is no "?REDO FROM START" and no re-prompt there, and no character filter either: "&77" and
        // "1d1" are legal fields it reads as 63 and 10, and we REFUSED both ("?SYNTAX ERROR - Number
        // expected", then asked again). The Commodore validation below is v7's and stays v7's.
        // The grammar is the one INPUT # uses, for the same reason and through the same helpers.
        // DIVERGENZE 124.
        if Assigned(FProgram) and FProgram.ModernMode then
        begin
          // ⛔ AND NO PROMPT AT ALL WHEN NOBODY IS THERE TO READ IT. fbc writes neither the user's
          // prompt string nor the "? " when standard input is redirected - measured: its own program
          // "Print "start" : Input "enter n"; n" emits just "start" under < /dev/null, while we
          // wrote "enter n? " into the captured output. See GStdinIsTerminal.
          if GStdinIsTerminal then
            InputStr := Trim(FInputDevice.ReadLine('? ', False, False, True))
          else
            InputStr := Trim(FInputDevice.ReadLine('', False, False, True));
          // ⛔ EXHAUSTED INPUT IS A VALUE, NOT AN END. fbc leaves 0 in the variable and runs on -
          // "Dim a As Integer = 7 : Input a : Print a" prints 0 and then the rest of the program -
          // while we STOPPED the program mid-line. Only a STOP request (Ctrl+End, window closed)
          // ends it. ⚠️ A program that LOOPS on INPUT will spin at EOF, exactly as it does under fbc.
          if FInputDevice.ShouldStop then
          begin
            Ctx.Running := False;
            FInputDevice.ClearStopRequest;
          end
          else if InputFieldIsFloat(InputStr) then
            Ctx.IntRegs[Instr.Dest] := FloatToIntConv(ParseLeadingFloat(InputStr), True)
          else
            Ctx.IntRegs[Instr.Dest] := ParseLeadingInt64(InputStr, 64);
        end
        else
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
        // ⛔ ...and in MODERN only when a person is there: see GStdinIsTerminal below.
        if Assigned(FOutputDevice) and
           (GStdinIsTerminal or not (Assigned(FProgram) and FProgram.ModernMode)) then
        begin
          if ((Instr.Immediate = -1) or (Instr.Src1 > 0)) and (Instr.Src1 < Length(Ctx.StringRegs)) then
            FOutputDevice.Print(Ctx.StringRegs[Instr.Src1]);
        end;
        // ⭐ MODERN never refuses - see the integer arm above.
        if Assigned(FProgram) and FProgram.ModernMode then
        begin
          if GStdinIsTerminal then
            InputStr := Trim(FInputDevice.ReadLine('? ', False, False, True))
          else
            InputStr := Trim(FInputDevice.ReadLine('', False, False, True));
          // Exhausted input is a VALUE - see the integer arm.
          if FInputDevice.ShouldStop then
          begin
            Ctx.Running := False;
            FInputDevice.ClearStopRequest;
          end
          else
            Ctx.FloatRegs[Instr.Dest] := ParseLeadingFloat(InputStr);
        end
        else
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
    17: // bcCpuCount - how many of WHAT: the immediate picks the quantity
      { 0 = logical processors (hardware threads), 1 = physical cores, 2 = physical CPUs (sockets).
        Three different numbers - 22 / 16 / 1 on a Core Ultra 9 185H - because only the cores with
        SMT become two logical processors.
        ⛔ Not System.CPUCount: on FPC 3.2.2/Linux it answers 1 whatever the machine has, so a
        worker pool sized from it would silently become single-threaded. }
      case Instr.Immediate of
        0: Ctx.IntRegs[Instr.Dest] := LogicalProcessorCount;
        1: Ctx.IntRegs[Instr.Dest] := PhysicalCoreCount;
      else
        Ctx.IntRegs[Instr.Dest] := PhysicalCpuCount;
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
      // ⛔ ONE-BASED IN MODERN, for the same reason POS is: the manual says "The topmost row is
      // number 1". CLASSIC keeps the Commodore numbering.
      if Assigned(FProgram) and FProgram.ModernMode then
        Ctx.IntRegs[Instr.Dest] := Ctx.CursorRow + 1
      else
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
    FGfxPages[i] := FGraphics.CreateSurface(W, H, $FF000000);   // opaque black (ARGB), as page 0
  FGfxWorkPage := 0;
  FGfxVisiblePage := 0;
  FGfxWorkSurface := GFX_SCREEN_SURFACE;
  FGfxFBScreen := True;
  FGfxViewActive := False;                      // a new mode has no viewport, as in FreeBASIC
  FGfxViewOffsetX := 0; FGfxViewOffsetY := 0;
  RecomputeGfxWindow;                           // the surface changed size under any live WINDOW
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

function TBytecodeVM.GfxViewW: Integer;
// What WINDOW divides up: the VIEWPORT when one is defined, the whole surface otherwise. Measured
// against fbc - with "View Screen (100,50)-(200,150)" the viewport is 101 pixels wide (both edges
// included) and "Window (0,0)-(10,10)" then maps world 10 to 101, not to the screen width.
begin
  if FGfxViewActive then
    Result := Abs(FGfxViewX2 - FGfxViewX1) + 1
  else if Assigned(FGraphics) then
    Result := FGraphics.SurfaceWidth(FGfxWorkSurface)
  else
    Result := 0;
end;

function TBytecodeVM.GfxViewH: Integer;
begin
  if FGfxViewActive then
    Result := Abs(FGfxViewY2 - FGfxViewY1) + 1
  else if Assigned(FGraphics) then
    Result := FGraphics.SurfaceHeight(FGfxWorkSurface)
  else
    Result := 0;
end;

procedure TBytecodeVM.RecomputeGfxWindow;
// The WINDOW transform, rebuilt from the logical bounds and the CURRENT viewport.
//
// ⛔ It has to be rebuilt, not computed once: fbc derives the mapping at use time, so setting a VIEW
// after a WINDOW changes what the WINDOW means (measured - "Window (0,0)-(10,10)" answers PMap(10,0)
// = 320 before a viewport and 101 after it). Keeping the coefficients but recomputing them on every
// statement that can change the viewport keeps the drawing path a multiply-add.
//
// The scale is the viewport size W, NOT W-1: world x2 maps to W, one past the last pixel. The
// vertical form of the default (non-SCREEN) WINDOW flips AND shifts by one, so world y2 maps to -1
// and world y1 to H-1. Both read off fbc, and the asymmetry is fbc's, not a simplification here.
var
  VW, VH: Integer;
begin
  if not FGfxWinActive then Exit;
  VW := GfxViewW; VH := GfxViewH;
  if (FGfxWinX2 = FGfxWinX1) or (FGfxWinY2 = FGfxWinY1) or (VW < 1) or (VH < 1) then
  begin
    FGfxWinActive := False;
    Exit;
  end;
  // Drawing: the last pixel is W-1, so world x2 lands ON it.
  FGfxWinAx := (VW - 1) / (FGfxWinX2 - FGfxWinX1);
  FGfxWinBx := -FGfxWinX1 * FGfxWinAx;
  // PMAP: world x2 answers W, one past the last pixel. Not the same number, and not a rounding of it.
  FGfxPMapAx := VW / (FGfxWinX2 - FGfxWinX1);
  FGfxPMapBx := -FGfxWinX1 * FGfxPMapAx;
  if FGfxWinScreen then
  begin
    FGfxWinAy := (VH - 1) / (FGfxWinY2 - FGfxWinY1);   // y1 = top, y2 = bottom
    FGfxWinBy := -FGfxWinY1 * FGfxWinAy;
    FGfxPMapAy := VH / (FGfxWinY2 - FGfxWinY1);
    FGfxPMapBy := -FGfxWinY1 * FGfxPMapAy;
  end
  else
  begin
    FGfxWinAy := -(VH - 1) / (FGfxWinY2 - FGfxWinY1); // y1 = bottom, y2 = top
    FGfxWinBy := (VH - 1) - FGfxWinY1 * FGfxWinAy;
    FGfxPMapAy := -VH / (FGfxWinY2 - FGfxWinY1);
    FGfxPMapBy := (VH - 1) - FGfxWinY1 * FGfxPMapAy;
  end;
end;

function TBytecodeVM.PointOutsideView(PX, PY: Integer): Boolean;
// Is this PHYSICAL pixel outside what POINT is allowed to see? The VIEW rectangle when one is
// defined, the surface itself otherwise. fbc answers &hFFFFFFFF for both, which is why they are one
// question and not two.
var
  Lo, Hi: Integer;
begin
  Result := True;
  if not Assigned(FGraphics) then Exit;
  if FGfxViewActive then
  begin
    if FGfxViewX1 <= FGfxViewX2 then begin Lo := FGfxViewX1; Hi := FGfxViewX2; end
                                else begin Lo := FGfxViewX2; Hi := FGfxViewX1; end;
    if (PX < Lo) or (PX > Hi) then Exit;
    if FGfxViewY1 <= FGfxViewY2 then begin Lo := FGfxViewY1; Hi := FGfxViewY2; end
                                else begin Lo := FGfxViewY2; Hi := FGfxViewY1; end;
    if (PY < Lo) or (PY > Hi) then Exit;
  end;
  if (PX < 0) or (PY < 0) then Exit;
  if (PX >= FGraphics.SurfaceWidth(DrawSurface)) or (PY >= FGraphics.SurfaceHeight(DrawSurface)) then Exit;
  Result := False;
end;

procedure TBytecodeVM.ExecuteGraphicsOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  SubOp: Word;
  DrawMode: Integer;
  PalColor: UInt32;
  GetX1, GetY1, GetX2, GetY2, GetSx, GetSy, SwapTmp: Integer;
  // ⛔ A COLOUR DOES NOT FIT IN AN Integer. "Rgb(0, 255, 0)" is 4278255360 with its alpha byte set, and
  // reading it into the 32-bit signed GetSx TRUNCATED it - silently in a release build (the bit pattern
  // survives the later UInt32 cast, which is why nothing ever looked wrong) and as a range error in a
  // debug one, where it made every graphics program undebuggable. Found 25 Aug 2026 by running the
  // corpus under the debug build.
  GfxColour: Int64;
  WinX1, WinY1, WinX2, WinY2, WinW, WinH: Integer;
  PMapVal: Double;                        // PMAP's answer before it is narrowed to a SINGLE
  JoyBtns, JoyDev, JoyLocal, JoyBtnIdx: Integer;
  JoyAx: array[0..7] of Single;
  JoyV: Single;
  ScrData: PByte;      // SCREENPTR: working-page pixel bytes (existence check only)
  ScrSize: Integer;
begin
  // ⛔ THE TEST IS INLINE AND THE CALL IS NOT MADE WHILE LOCKED. Both of these run once per GRAPHICS
  // OPERATION - 62 500 times a frame in a demo that plots points - and a call that returns immediately
  // still costs its call. Measured: with the frame bracketed by SCREENLOCK the two of them were 5.8 ms
  // of a 24 ms frame, about 47 ns per call. Inside a lock neither can do anything anyway: the boundary
  // is the UNLOCK, so testing the depth here is strictly better than testing it inside.
  if (FPresentCadenceMs > 0) and (FScreenLockDepth = 0) then PresentBeforeFullRepaint(Ctx, Instr);
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
        // Cursor column, tracked by the VM during PRINT.
        // ⛔ ONE-BASED IN MODERN. FreeBASIC counts the leftmost column as 1 (CSRLIN's page says it
        // outright for rows: "The topmost row is number 1"), and we answered 0 - so every FreeBASIC
        // program reading POS was off by one, silently. Commodore counts from 0 and CLASSIC keeps it:
        // the two dialects number their columns differently and they stay apart.
        if Assigned(FProgram) and FProgram.ModernMode then
          Ctx.IntRegs[Instr.Dest] := Ctx.CursorCol + 1
        else
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
        // ⛔ TWO CLEARS, and only one of them was ever instructed. CLS went to the TEXT device, which
        // clears the framebuffer only when it has been handed one (sb --window does that; headless sb
        // does not) - so under a FreeBASIC SCREENRES the picture survived every CLS, in all four
        // forms, and POINT proved it. A FreeBASIC graphics screen is cleared HERE, on the same surface
        // PSET writes and POINT reads, and to the background colour COLOR set (fbc: measured).
        if FGfxFBScreen and Assigned(FGraphics) then
        begin
          if FGfxViewActive then
            // CLS clears the VIEWPORT when one is defined, and leaves the rest of the screen standing.
            FGraphics.DrawRect(FGfxWorkSurface, FGfxViewX1, FGfxViewY1, FGfxViewX2, FGfxViewY2,
                               FGfxBackColor, True, 1, 0)
          else
            FGraphics.ClearSurface(FGfxWorkSurface, FGfxBackColor);
        end
        else if Assigned(FOutputDevice) then
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
      // ⛔ OUT OF THE VIEWPORT IS -1, NOT ZERO, and "the viewport" is not "the surface": with a VIEW
      // defined, fbc answers &hFFFFFFFF for anything outside the RECTANGLE, even where the screen has
      // a pixel. We answered the pixel there and 0 past the edge of the surface - so the two idioms
      // that use POINT as a bounds test ("is this inside?" and "did the fill reach here?") both read
      // as a legitimate black. Measured against fbc; -1 is the answer for both cases.
      if Assigned(FGraphics) then
      begin
        GetX1 := GfxMapX(Ctx.IntRegs[Instr.Src1]);
        GetY1 := GfxMapY(Ctx.IntRegs[Instr.Src2]);
        if PointOutsideView(GetX1, GetY1) then
          Ctx.IntRegs[Instr.Dest] := $FFFFFFFF
        else
          Ctx.IntRegs[Instr.Dest] := Int64(FGraphics.GetPixel(DrawSurface, GetX1, GetY1));
      end
      else
        Ctx.IntRegs[Instr.Dest] := $FFFFFFFF;
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
        // Leave them on the backend as well: inside a graphics mode PRINT draws with these, and the
        // text device reads them from there. It is the only place both can reach - see SetTextColors.
        if Assigned(FGraphics) then
          FGraphics.SetTextColors(UInt32(FConColorFg), UInt32(FConColorBg));
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
    40: // bcGfxPut - PUT [img,] (x,y),src[,mode] : blit image src onto the DRAW SURFACE (Immediate[0-15]=
        //  src handle register, Immediate[16-31]=mode ordinal constant)
        // ⛔ It used to name FGfxWorkSurface outright, so "Put img,(x,y),src" evaluated its target, set
        //  it, and blitted onto the screen anyway. DrawSurface is the same funnel PSET/LINE/CIRCLE/
        //  PAINT/POINT read, and it is the work page whenever no target is active.
      if Assigned(FGraphics) then
        // Immediate [0-15]=src handle reg, [16-31]=mode ordinal, [32-47]=blend-value reg (-1 = none).
        FGraphics.Blit(DrawSurface, GfxMapX(Ctx.IntRegs[Instr.Src1]), GfxMapY(Ctx.IntRegs[Instr.Src2]),
                       Ctx.IntRegs[Instr.Immediate and $FFFF], TGfxBlitMode((Instr.Immediate shr 16) and $FFFF),
                       Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF]);
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
    66: // bcGfxScreenLock - SCREENLOCK: the frame starts here; suppress every present until it ends.
      begin
        Inc(FScreenLockDepth);
        // A program that locks has TOLD us where its frames end, so the clock-driven guess must stop
        // guessing - permanently, not just while locked. Leaving it on would present between the
        // unlock and the next lock, which is a gap of microseconds and produces a flicker that looks
        // random because it depends on when the 16 ms tick lands.
        FFrameBoundarySeen := True;
      end;
    67: // bcGfxScreenUnlock - SCREENUNLOCK: the picture is finished. Show it, exactly once.
      begin
        if FScreenLockDepth > 0 then Dec(FScreenLockDepth);
        if (FScreenLockDepth = 0) and (FPresentCadenceMs > 0) then
          if PresentNow then Ctx.Running := False;
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
    44: // bcGfxWindow - WINDOW [SCREEN] (x1,y1)-(x2,y2): set/clear the logical coordinate transform.
        //  ⛔ The bounds are FLOAT registers: "Window (-2.5,-2.5)-(2.5,2.5)" is the whole point of the
        //  statement, and integer registers truncated it. Only the BOUNDS are stored here; the
        //  coefficients come from RecomputeGfxWindow, which the VIEW and SCREENRES arms also call.
      if Assigned(FGraphics) then
      begin
        if ((Instr.Immediate shr 32) and 1) = 0 then
          FGfxWinActive := False                                    // no bounds -> identity
        else
        begin
          FGfxWinX1 := Ctx.FloatRegs[Instr.Src1];
          FGfxWinY1 := Ctx.FloatRegs[Instr.Src2];
          FGfxWinX2 := Ctx.FloatRegs[Instr.Immediate and $FFFF];
          FGfxWinY2 := Ctx.FloatRegs[(Instr.Immediate shr 16) and $FFFF];
          FGfxWinScreen := ((Instr.Immediate shr 33) and 1) = 1;
          FGfxWinActive := True;
          RecomputeGfxWindow;
        end;
      end;
    45: // bcGfxPMap - __PMAP(coord, n): map between logical and physical coordinates.
        //  0 = logical x -> physical x   1 = logical y -> physical y
        //  2 = physical x -> logical x   3 = physical y -> logical y
        //  ⛔ FLOAT in and out, all four. fbc's PMAP returns a SINGLE, and both directions are
        //  fractional: PMap(319,2) is 0.99375 under a unit window, and PMap(5,0) is 50.5 under a
        //  101-pixel viewport. Rounding either one to an integer is a different function.
        //  ⚠️ It does NOT round the way GfxMapX does. GfxMapX exists to pick a PIXEL; PMAP exists to
        //  report the mapping, and the answer fbc gives is the unrounded product - narrowed to SINGLE,
        //  which is the type fbc declares and the reason it prints 9.90099 where a double says
        //  9.900990099009901.
      begin
        case Instr.Immediate of
          0: if FGfxWinActive then PMapVal := FGfxPMapAx * Ctx.FloatRegs[Instr.Src1] + FGfxPMapBx
             else PMapVal := Ctx.FloatRegs[Instr.Src1];
          1: if FGfxWinActive then PMapVal := FGfxPMapAy * Ctx.FloatRegs[Instr.Src1] + FGfxPMapBy
             else PMapVal := Ctx.FloatRegs[Instr.Src1];
          2: if FGfxWinActive and (FGfxPMapAx <> 0) then
               PMapVal := (Ctx.FloatRegs[Instr.Src1] - FGfxPMapBx) / FGfxPMapAx
             else PMapVal := Ctx.FloatRegs[Instr.Src1];
        else
          if FGfxWinActive and (FGfxPMapAy <> 0) then
            PMapVal := (Ctx.FloatRegs[Instr.Src1] - FGfxPMapBy) / FGfxPMapAy
          else PMapVal := Ctx.FloatRegs[Instr.Src1];
        end;
        // A flipped window has a NEGATIVE scale, so an exact zero numerator gives -0 - which prints
        // as "-0" and fbc prints " 0". Same value, and the comparison against 0 catches both signs.
        if PMapVal = 0 then PMapVal := 0;
        Ctx.FloatRegs[Instr.Dest] := Double(Single(PMapVal));
      end;
    46: // bcGfxView - VIEW [SCREEN] (x1,y1)-(x2,y2): set/clear the viewport (offset + clip on the work page)
      if Assigned(FGraphics) then
      begin
        if ((Instr.Immediate shr 32) and 1) = 0 then
        begin
          FGfxViewOffsetX := 0; FGfxViewOffsetY := 0;          // reset -> full screen, no offset
          FGfxViewActive := False;
          FGraphics.SetClip(FGfxWorkSurface, False, 0, 0, 0, 0);
          RecomputeGfxWindow;                                  // WINDOW now divides the whole screen
        end
        else
        begin
          WinX1 := Ctx.IntRegs[Instr.Src1];
          WinY1 := Ctx.IntRegs[Instr.Src2];
          WinX2 := Ctx.IntRegs[Instr.Immediate and $FFFF];
          WinY2 := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];
          FGraphics.SetClip(FGfxWorkSurface, True, WinX1, WinY1, WinX2, WinY2);
          FGfxViewActive := True;
          FGfxViewX1 := WinX1; FGfxViewY1 := WinY1; FGfxViewX2 := WinX2; FGfxViewY2 := WinY2;
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
        RecomputeGfxWindow;   // a WINDOW set BEFORE the VIEW now divides the VIEWPORT (fbc-measured)
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
          SetupGfxScreen(WinW, WinH, Instr.Immediate)
        else if Ctx.IntRegs[Instr.Src1] = 0 then
          FGfxFBScreen := False;   // SCREEN 0 asks for text back: CLS belongs to the console again
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
          // No provider (headless) or device absent: fbc answers status 1, axes -1000 and buttons -1.
          // ⛔ The comment here used to say "buttons 0" and the code agreed with it - an assumption
          // written as a fact and never asked of the oracle. Measured: -1, for every device id.
          FJoyButtons := -1;
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
    65: // bcGfxDrawString - DRAW STRING [img,](x,y),text[,colour] : blit text with the built-in 8x8 font.
        // Src1 = text (string reg), Src2 = x, Src3 = y, Immediate[0-15] = colour reg.
        // Coordinates go through the same WINDOW mapping as every other draw op, so text lands where the
        // program's own coordinate system puts it; the image target rides on the bcGfxSetTarget pair.
        // NOT opaque: FreeBASIC's DRAW STRING leaves the background showing through, unlike PRINT.
      if Assigned(FGraphics) then
      begin
        GetX1 := GfxMapX(Ctx.IntRegs[Instr.Src2]);
        GetY1 := GfxMapY(Ctx.IntRegs[Instr.Immediate and $FFFF]);
        GfxColour := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];         // colour: 64-bit, see above
        FGraphics.DrawText(DrawSurface, GetX1, GetY1, Ctx.StringRegs[Instr.Src1],
                           UInt32(GfxColour and $FFFFFFFF), 0, False);
      end;
    58: // bcGfxPointCoord - POINTCOORD(n): the DRAW pen coordinate (Src1 selector: 0 = x, 1 = y).
      if Ctx.IntRegs[Instr.Src1] = 1 then
        Ctx.IntRegs[Instr.Dest] := FDrawPenY
      else
        Ctx.IntRegs[Instr.Dest] := FDrawPenX;
    59, 68: // bcGfxCircleEx / bcGfxCircleExF - CIRCLE ellipse/arc, outline or FILLED (sub-op 68).
        // Src1=x, Src2=y, Dest=RX; Immediate [0-15]=RY, [16-31]=color,
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
          0.0, 0.0, 1, SubOp = 68);                                  // sub-op 68 = the F flag
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
        GfxColour := Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF];         // colour: 64-bit, see above
        GetSy := Ctx.IntRegs[(Instr.Immediate shr 32) and $FFFF] and $FFFF;   // style mask (16-bit)
        if ((Instr.Immediate shr 48) and $3) = 1 then
        begin
          // B: styled box outline = four styled edges (pattern restarts on each edge).
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY1, GetX2, GetY1, UInt32(GfxColour and $FFFFFFFF), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX2, GetY1, GetX2, GetY2, UInt32(GfxColour and $FFFFFFFF), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX2, GetY2, GetX1, GetY2, UInt32(GfxColour and $FFFFFFFF), Word(GetSy));
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY2, GetX1, GetY1, UInt32(GfxColour and $FFFFFFFF), Word(GetSy));
        end
        else
          FGraphics.DrawLineStyled(DrawSurface, GetX1, GetY1, GetX2, GetY2, UInt32(GfxColour and $FFFFFFFF), Word(GetSy));
        FDrawPenX := Ctx.IntRegs[Instr.Dest]; FDrawPenY := Ctx.IntRegs[(Instr.Immediate) and $FFFF];
      end;
  else
    raise Exception.CreateFmt('Unknown graphics opcode %d at PC=%d', [Instr.OpCode, Ctx.PC]);
  end;

  if (FPresentCadenceMs > 0) and (FScreenLockDepth = 0) then MaybePresentCadence(Ctx);
end;

// Present the framebuffer on a wall-clock cadence, driven from the graphics opcodes.
//
// Why it has to be here. The window presenter (`sb --window`) is driven by EventPollCallback, and
// the dispatch loop only reaches that at BLOCKING points: SLEEP, GETKEY, waiting on a note. A
// graphics program whose main loop is pure computation - draw the frame, compute the next one,
// repeat, with no SLEEP anywhere - therefore never presents at all. The window comes up black and
// stops answering events, and nothing in the program is wrong. FreeBASIC has a frame boundary for
// exactly this, SCREENUNLOCK, and we accept it as a no-op, so there is no boundary to hang a
// present on either.
//
// ExecuteGraphicsOp is the single entry point every graphics opcode passes through, so one call
// here covers PSET, LINE, PAINT, blits, text-in-graphics and everything else - every program with
// this shape, not just the one that found it.
//
// ⚠️ THE PRICE, stated plainly: without a frame boundary this can present a half-drawn frame, so a
// slow frame shows a horizontal seam where the cadence caught it. It is a live-preview mechanism,
// not a substitute for double buffering; the fix for the seam is to make SCREENUNLOCK a real
// present, which needs an opcode.
//
// ⚠️ AND IT IS OFF BY DEFAULT, which matters more than it looks. sbv, sbw and headless sb never set
// PresentCadenceMs - only the WITH_WINDOW path in SedaiBasicVM.lpr does - so for every other target
// this whole mechanism costs one compare against a field per graphics opcode and changes nothing.
// A present cadence added unconditionally would have fought with sbv's own rendering.
// A frame ends where the next one starts: at the moment the program is about to repaint the whole
// screen. That is not a guess about intent, it is a fact about the buffer - everything currently in
// it is about to be destroyed, so this instant is the last one at which it is a complete picture.
//
// This exists because the clock alone is not good enough. A time cadence fires wherever it happens
// to land, which for a program that clears and redraws every frame means it regularly catches the
// screen just after the clear and just before the landscape: the window then alternates between the
// finished frame and a flat field of the clear colour, at the cadence rate. Which is to say it
// FLICKERS, in the background colour, and looks far worse than the seam a mid-frame present was
// expected to cost.
//
// Detected here: LINE with the BF flag whose corners cover the whole screen - the idiom every
// FreeBASIC animation uses to clear. Once a program has shown it has a frame boundary, the clock is
// switched off for good and presents happen only here, exactly once per frame, never mid-picture.
// A program that never repaints the whole screen keeps the clock, which is right: it has no frames
// to be caught between.
procedure TBytecodeVM.PresentBeforeFullRepaint(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
var
  X1, Y1, X2, Y2, W, H: Integer;
begin
  if FScreenLockDepth > 0 then Exit;   // inside a lock the boundary is the UNLOCK, not a guess
  if Instr.OpCode <> bcGfxLine then Exit;
  if ((Instr.Immediate shr 48) and $3) <> 2 then Exit;        // not BF: not a filled box
  if not Assigned(FGraphics) then Exit;

  W := FGraphics.SurfaceWidth(FGraphics.ScreenSurface);
  H := FGraphics.SurfaceHeight(FGraphics.ScreenSurface);
  if (W <= 0) or (H <= 0) then Exit;

  X1 := GfxMapX(Ctx.IntRegs[Instr.Src1]);
  Y1 := GfxMapY(Ctx.IntRegs[Instr.Src2]);
  X2 := GfxMapX(Ctx.IntRegs[(Instr.Immediate) and $FFFF]);
  Y2 := GfxMapY(Ctx.IntRegs[(Instr.Immediate shr 16) and $FFFF]);
  if (X1 > 0) or (Y1 > 0) or (X2 < W - 1) or (Y2 < H - 1) then Exit;

  // The picture standing in the buffer is finished. Show it, then let the clear happen.
  FFrameBoundarySeen := True;
  if PresentNow then Ctx.Running := False;
end;

function TBytecodeVM.PresentNow: Boolean;
// Show the picture, through the PRESENT callback and not the poll one. Returns True if the window was
// closed. Falls back to the poll callback for any front end that has not been split yet, and to the
// direct PresentFrame when there is no callback at all.
begin
  Result := False;
  if Assigned(FPresentCallback) then Result := FPresentCallback()
  else if Assigned(FEventPollCallback) then Result := FEventPollCallback()
  else PresentFrame;
end;

procedure TBytecodeVM.MaybePresentCadence(Ctx: TExecutionContext);
var
  Tick: QWord;
begin
  if FScreenLockDepth > 0 then Exit; // ⛔ the picture is half drawn: showing it IS the tearing
  if FFrameBoundarySeen then Exit;   // the program has a real frame boundary; the clock would only
                                     // catch it mid-picture and make it flicker
  // GetTickCount64 reads a shared page on Windows and a monotonic clock on Linux: cheap enough to
  // call per LINE (a few thousand times a frame) without a counter in front of it, and a counter
  // would break the case that needs this most - a program drawing five lines per frame.
  Tick := GetTickCount64;
  if Tick - FLastPresentTick < FPresentCadenceMs then Exit;
  FLastPresentTick := Tick;

  if PresentNow then Ctx.Running := False;
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

function TBytecodeVM.BigAlloc(Ctx: TExecutionContext): Integer;
// A fresh BigInt handle, value 0. Grows the per-context heap geometrically, as the
// record heap does - a BigInt is allocated exactly where a UDT instance would be.
begin
  if Ctx.BigCount >= Length(Ctx.BigVals) then
    SetLength(Ctx.BigVals, (Ctx.BigCount * 2) + 16);
  Result := Ctx.BigCount;
  Inc(Ctx.BigCount);
  SetLength(Ctx.BigVals[Result].Limbs, 1);
  Ctx.BigVals[Result].Limbs[0] := 0;
  Ctx.BigVals[Result].N := 1;         { zero is N=1 with a zero limb: ONE representation }
  Ctx.BigVals[Result].Neg := False;
  Ctx.BigVals[Result].Owner := -1;    { the caller says who owns it }
end;

function TBytecodeVM.BigDecimal(Ctx: TExecutionContext; H: Integer): string;
// The decimal text of a BigInt. ⚠️ In base 2^64 this is NOT free the way base 10^9
// was: it is repeated division of the whole magnitude by 10^19, the largest power of
// ten a limb holds, which costs O(n^2) digits. Acceptable because printing is not on
// any hot path here - pidigits emits its digits from the tap, one at a time, and
// never converts a whole number. If that changes, the answer is a divide-and-conquer
// split, not a faster inner loop.
const
  CHUNK = QWord(10000000000000000000);   { 10^19, the largest power of ten below 2^64 }
var
  W: TLimbs;
  n, i: Integer;
  rem, cur: QWord;
  part: string;
begin
  if (H < 0) or (H >= Ctx.BigCount) then Exit('0');
  n := Ctx.BigVals[H].N;
  if (n = 1) and (Ctx.BigVals[H].Limbs[0] = 0) then Exit('0');
  { Work on a COPY: the conversion destroys the value it walks. }
  SetLength(W, n);
  for i := 0 to n - 1 do W[i] := Ctx.BigVals[H].Limbs[i];
  Result := '';
  while (n > 1) or (W[0] <> 0) do
  begin
    rem := 0;
    for i := n - 1 downto 0 do
    begin
      { 128-bit ÷ 64-bit one limb at a time. FPC has no 128-bit divide, so the
        remainder is carried in the high half by hand: this is the schoolbook step
        rem:W[i] div CHUNK, done with the two 64-bit halves the language does have. }
      cur := W[i];
      W[i] := DivMod128By64(rem, cur, CHUNK, rem);
    end;
    while (n > 1) and (W[n - 1] = 0) do Dec(n);
    part := IntToStr(rem);
    if (n > 1) or (W[0] <> 0) then
      while Length(part) < 19 do part := '0' + part;   { inner chunks keep their zeros }
    Result := part + Result;
  end;
  if Ctx.BigVals[H].Neg then Result := '-' + Result;
end;

function TBytecodeVM.BigDestOf(Ctx: TExecutionContext; Reg: Integer): Integer;
// The handle an arithmetic result goes into: the one the register already holds when
// it is live, a fresh one otherwise. ⚠️ Reusing matters - a loop that accumulates into
// the same BigInt would otherwise allocate a handle per iteration and never free one.
begin
  Result := Integer(Ctx.IntRegs[Reg]);
  { ⭐ Reuse the handle this register already OWNS. The ownership test is what makes it
    safe: a register that finds a handle it does not own (garbage on first execution, or
    another register's value left behind by the compactor) allocates instead of writing
    over somebody else's number. One allocation per register per lifetime rather than
    one per OPERATION - which is the whole difference on pidigits. }
  if (Result >= 0) and (Result < Ctx.BigCount) and (Ctx.BigVals[Result].Owner = Reg) then
    Exit;
  Result := BigAlloc(Ctx);
  Ctx.BigVals[Result].Owner := Reg;
  Ctx.IntRegs[Reg] := Result;
end;

function TBytecodeVM.BigSignedCmp(Ctx: TExecutionContext; A, B: Integer): Int64;
// -1, 0 or 1. Signs first, magnitudes only when they agree - and for two negatives the
// magnitude comparison INVERTS, which is the step that is easy to forget.
begin
  if Ctx.BigVals[A].Neg <> Ctx.BigVals[B].Neg then
  begin
    if Ctx.BigVals[A].Neg then Exit(-1) else Exit(1);
  end;
  Result := BigCmp(Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
                   Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N);
  if Ctx.BigVals[A].Neg then Result := -Result;
end;

procedure TBytecodeVM.BigSignedAdd(Ctx: TExecutionContext; H, A, B: Integer; NegB: Boolean);
// H := A + (B with the sign NegB). The core arithmetic is MAGNITUDE-ONLY on purpose, so
// the sign rules live here, in one place, and BigSub reaches them by flipping NegB.
//
//   same signs      -> add the magnitudes, keep the sign
//   different signs -> subtract the SMALLER magnitude from the LARGER, and take the
//                      sign of the larger. ⛔ BigSub requires a >= b: handing it the
//                      operands the other way round underflows silently.
var
  c: Integer;
  ResNeg: Boolean;
begin
  UniqueLimbs(Ctx.BigVals[H].Limbs);
  if Ctx.BigVals[A].Neg = NegB then
  begin
    BigAdd(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N,
           Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
           Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N);
    ResNeg := Ctx.BigVals[A].Neg;
  end
  else
  begin
    c := BigCmp(Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
                Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N);
    if c >= 0 then
    begin
      BigSub(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N,
             Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
             Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N);
      ResNeg := Ctx.BigVals[A].Neg;
    end
    else
    begin
      BigSub(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N,
             Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N,
             Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N);
      ResNeg := NegB;
    end;
  end;
  { One representation of zero, so a zero result is never negative - otherwise
    "0 = -0" would depend on how the zero was reached. }
  if (Ctx.BigVals[H].N = 1) and (Ctx.BigVals[H].Limbs[0] = 0) then ResNeg := False;
  Ctx.BigVals[H].Neg := ResNeg;
end;

procedure TBytecodeVM.BigSetDecimal(Ctx: TExecutionContext; H: Integer; const S: string);
// The inverse of BigDecimal: a decimal text into limbs. Horner in base 10^19 - the
// largest power of ten a limb holds - so one multiply-and-add per NINETEEN digits
// instead of per digit.
// ⚠️ Anything that is not a digit ENDS the number, and a leading '-' is the sign, so
// this accepts exactly what Str() produces and stops at the first character it cannot
// use. It does not raise: an empty or unparsable text is zero, which is what VAL does
// for the builtin types and keeps one rule in the language rather than two.
var
  i, k, ndig: Integer;
  chunk, scale: QWord;
  neg: Boolean;
begin
  UniqueLimbs(Ctx.BigVals[H].Limbs);
  BigSetSmall(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N, 0);
  Ctx.BigVals[H].Neg := False;
  i := 1;
  while (i <= Length(S)) and (S[i] = ' ') do Inc(i);
  neg := False;
  if (i <= Length(S)) and ((S[i] = '-') or (S[i] = '+')) then
  begin
    neg := S[i] = '-';
    Inc(i);
  end;
  while i <= Length(S) do
  begin
    chunk := 0; scale := 1; ndig := 0;
    while (i <= Length(S)) and (S[i] >= '0') and (S[i] <= '9') and (ndig < 19) do
    begin
      chunk := chunk * 10 + QWord(Ord(S[i]) - Ord('0'));
      scale := scale * 10;
      Inc(ndig); Inc(i);
    end;
    if ndig = 0 then Break;              { primo carattere non-cifra: il numero finisce }
    BigMulSmall(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N, scale);
    k := Ctx.BigVals[H].N;
    BigAddSmall(Ctx.BigVals[H].Limbs, k, chunk);
    Ctx.BigVals[H].N := k;
  end;
  { A zero is never negative: one representation only. }
  if not ((Ctx.BigVals[H].N = 1) and (Ctx.BigVals[H].Limbs[0] = 0)) then
    Ctx.BigVals[H].Neg := neg;
end;

procedure TBytecodeVM.ExecuteBigIntOp(Ctx: TExecutionContext; const Instr: TBytecodeInstruction);
// Group 12. A BigInt value is a HANDLE in the int bank; the limbs live in Ctx.BigVals.
var
  H, S, A, B: Integer;
  v: Int64;
  u: QWord;
  NegB, NegA: Boolean;
  QTmpLimbs: TLimbs;
  QTmpN: Integer;
begin
  case Instr.OpCode of
    bcBigNew:
      begin
        { Same ownership rule: a DIM executes once, but a temporary's BigNew sits inside
          the loop, and re-allocating there is what made the handle heap grow without
          bound. Reusing means resetting to zero, which costs nothing. }
        H := BigDestOf(Ctx, Instr.Dest);
        UniqueLimbs(Ctx.BigVals[H].Limbs);
        BigSetSmall(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N, 0);
        Ctx.BigVals[H].Neg := False;
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigFromInt:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        v := Ctx.IntRegs[Instr.Src1];
        { ⛔ The magnitude of Low(Int64) does not fit in an Int64, so negating it is
          the overflow this project has already been bitten by. Take the two's
          complement in the UNSIGNED domain, where 2^63 is representable. }
        if v < 0 then begin Ctx.BigVals[H].Neg := True;  u := QWord(-(v + 1)) + 1; end
                 else begin Ctx.BigVals[H].Neg := False; u := QWord(v); end;
        UniqueLimbs(Ctx.BigVals[H].Limbs);
        BigSetSmall(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N, u);
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigCopy:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        S := Integer(Ctx.IntRegs[Instr.Src1]);
        if (S < 0) or (S >= Ctx.BigCount) then Exit;
        { ⭐ VALUE semantics without copying the limbs: the assignment shares them and
          the refcount goes up; UniqueLimbs splits them at the first WRITE. Measured
          at the same price as AnsiString's own copy-on-write. }
        Ctx.BigVals[H].Limbs := Ctx.BigVals[S].Limbs;
        Ctx.BigVals[H].N     := Ctx.BigVals[S].N;
        Ctx.BigVals[H].Neg   := Ctx.BigVals[S].Neg;
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigToStr:
      Ctx.StringRegs[Instr.Dest] := BigDecimal(Ctx, Integer(Ctx.IntRegs[Instr.Src1]));

    bcBigAdd, bcBigSub:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        B := Integer(Ctx.IntRegs[Instr.Src2]);
        if (A < 0) or (A >= Ctx.BigCount) or (B < 0) or (B >= Ctx.BigCount) then Exit;
        { ⭐ a - b IS a + (-b), and saying so here is the point: the sign logic below
          is written ONCE and both opcodes reach it. Two bodies would be two chances
          to get the borrow case wrong. }
        NegB := Ctx.BigVals[B].Neg;
        if Instr.OpCode = bcBigSub then NegB := not NegB;
        BigSignedAdd(Ctx, H, A, B, NegB);
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigMul:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        B := Integer(Ctx.IntRegs[Instr.Src2]);
        if (A < 0) or (A >= Ctx.BigCount) or (B < 0) or (B >= Ctx.BigCount) then Exit;
        UniqueLimbs(Ctx.BigVals[H].Limbs);
        BigMul(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N,
               Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
               Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N);
        { The sign of a product is the xor of the signs - EXCEPT that zero has only
          one representation here, so a zero result is never negative. }
        Ctx.BigVals[H].Neg := (Ctx.BigVals[A].Neg <> Ctx.BigVals[B].Neg) and
                              not ((Ctx.BigVals[H].N = 1) and (Ctx.BigVals[H].Limbs[0] = 0));
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigFromStr:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        BigSetDecimal(Ctx, H, Ctx.StringRegs[Instr.Src1]);
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigMulSmall:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        if (A < 0) or (A >= Ctx.BigCount) then Exit;
        v := Ctx.IntRegs[Instr.Src2];
        if v < 0 then begin NegB := True;  u := QWord(-(v + 1)) + 1; end
                 else begin NegB := False; u := QWord(v); end;
        { ⭐ UNA passata, non copia-poi-moltiplica: BigMulSmallTo legge a[i] e scrive
          dst[i] at the same index, so it holds when H = A too. }
        NegA := Ctx.BigVals[A].Neg;
        BigMulSmallTo(Ctx.BigVals[H].Limbs, Ctx.BigVals[H].N,
                      Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N, u);
        Ctx.BigVals[H].Neg := NegA;
        Ctx.BigVals[H].Neg := (Ctx.BigVals[H].Neg <> NegB) and
                              not ((Ctx.BigVals[H].N = 1) and (Ctx.BigVals[H].Limbs[0] = 0));
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigDiv, bcBigMod:
      begin
        H := BigDestOf(Ctx, Instr.Dest);
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        B := Integer(Ctx.IntRegs[Instr.Src2]);
        if (A < 0) or (A >= Ctx.BigCount) or (B < 0) or (B >= Ctx.BigCount) then Exit;
        if (Ctx.BigVals[B].N = 1) and (Ctx.BigVals[B].Limbs[0] = 0) then
          raise EDivByZero.Create('BigInt division by zero');
        { Il quoziente e il resto escono INSIEME dall'algoritmo: si calcolano entrambi e
          only the requested one is kept. Two destinations distinct from H are needed, because H can
          coincidere con A o con B. }
        if (Ctx.BigScratch < 0) or (Ctx.BigScratch >= Ctx.BigCount) then
        begin
          Ctx.BigScratch := BigAlloc(Ctx);
          Ctx.BigVals[Ctx.BigScratch].Owner := -2;
        end;
        S := Ctx.BigScratch;
        BigDivMod(Ctx.BigVals[S].Limbs, Ctx.BigVals[S].N, QTmpLimbs, QTmpN,
                  Ctx.BigVals[A].Limbs, Ctx.BigVals[A].N,
                  Ctx.BigVals[B].Limbs, Ctx.BigVals[B].N,
                  Ctx.BigDivU, Ctx.BigDivV);
        if Instr.OpCode = bcBigDiv then
        begin
          { ⚠️ TRONCATO VERSO ZERO, come la divisione intera del linguaggio: il segno e'
            lo xor dei segni e la magnitudine non si corregge. }
          NegA := Ctx.BigVals[A].Neg <> Ctx.BigVals[B].Neg;
          Ctx.BigVals[H].Limbs := Ctx.BigVals[S].Limbs;
          Ctx.BigVals[H].N := Ctx.BigVals[S].N;
        end
        else
        begin
          { ⚠️ The remainder takes the DIVIDEND's sign, which is Mod's convention here. }
          NegA := Ctx.BigVals[A].Neg;
          Ctx.BigVals[H].Limbs := QTmpLimbs;
          Ctx.BigVals[H].N := QTmpN;
        end;
        Ctx.BigVals[H].Neg := NegA and
                              not ((Ctx.BigVals[H].N = 1) and (Ctx.BigVals[H].Limbs[0] = 0));
        Ctx.IntRegs[Instr.Dest] := H;
      end;

    bcBigToInt:
      begin
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        if (A < 0) or (A >= Ctx.BigCount) then begin Ctx.IntRegs[Instr.Dest] := 0; Exit; end;
        { ⚠️ I 64 bit BASSI col segno, come ogni altra conversione stretta del linguaggio:
          un BigInt piu' grande di un Int64 non ha una risposta giusta, e avvolgere e'
          la stessa regola che vale gia' per Integer <- LongInt. }
        u := Ctx.BigVals[A].Limbs[0];
        if Ctx.BigVals[A].Neg then Ctx.IntRegs[Instr.Dest] := -Int64(u)
        else Ctx.IntRegs[Instr.Dest] := Int64(u);
      end;

    bcBigCmp:
      begin
        A := Integer(Ctx.IntRegs[Instr.Src1]);
        B := Integer(Ctx.IntRegs[Instr.Src2]);
        if (A < 0) or (A >= Ctx.BigCount) or (B < 0) or (B >= Ctx.BigCount) then
          Ctx.IntRegs[Instr.Dest] := 0
        else
          Ctx.IntRegs[Instr.Dest] := BigSignedCmp(Ctx, A, B);
      end;
  else
    raise Exception.CreateFmt('Unknown BigInt opcode $%.4x at PC=%d', [Instr.OpCode, Ctx.PC]);
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
  OpenFbCode: Integer;   // the FreeBASIC status of an OPEN: delivered in Dest AND in Err
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
          // The FreeBASIC code for what the file layer reported: 62 FILE NOT FOUND is fbc's 2, and the
          // other failures the layer can return are its 3 (file I/O error).
          case ErrorCode of
            0:      OpenFbCode := 0;
            62:     OpenFbCode := 2;
          else      OpenFbCode := 3;
          end;
          if SubOp = 34 then
            // The FUNCTION form is FreeBASIC's, so it answers with FreeBASIC's code.
            Ctx.IntRegs[Instr.Dest] := OpenFbCode
          else if not (Assigned(FProgram) and FProgram.ModernMode) and (ErrorCode <> 0) then
            raise Exception.CreateFmt('DOPEN error %d opening file: %s', [ErrorCode, Filename]);
          // ⛔ AND IT REACHES Err, WHICH IS THE WHOLE POINT OF THE STATEMENT FORM IN FreeBASIC.
          // "Open f For Input As #1 : Loop Until Err() = 0" is the manual's own inline idiom, and the
          // statement form lowers to THIS opcode - the function one - with its result discarded, so a
          // missing file simply vanished: Err stayed 0 and the program read an empty file.
          // Every open sets Err to its OWN status, 0 on success included, exactly as fbc does.
          if Assigned(FProgram) and FProgram.ModernMode then
          begin
            Ctx.LastErrorCode := OpenFbCode;
            if OpenFbCode <> 0 then
            begin
              Ctx.LastErrorLine := FProgram.GetSourceLine(Ctx.PC);
              Ctx.LastErrorMessage := 'File not found';
            end;
          end;
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
        // Immediate = 3: the value is SINGLE-typed, so 7 significant digits, exactly as the console
        // arm does it. The kind travels with the value for floats too, not just for integers.
        Data := FConsoleBehavior.FormatNumber(Ctx.FloatRegs[Instr.Dest], Instr.Immediate = 3);
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
        // Immediate carries the PRINT KIND, the same one the console arms use: 0 = signed,
        // 1 = BOOLEAN ("true"/"false"), 2 = unsigned 64-bit. Without it a file got "-1" where the
        // console got "true", and every unsigned value carried a sign space fbc does not write - the
        // console path had known all three since B1.5 and PRINT#/WRITE# had never been told.
        case Instr.Immediate of
          1: if Ctx.IntRegs[Instr.Dest] <> 0 then Data := 'true' else Data := 'false';
          2: Data := FConsoleBehavior.FormatUInt(QWord(Ctx.IntRegs[Instr.Dest]));
        else
          Data := FConsoleBehavior.FormatInt(Ctx.IntRegs[Instr.Dest]);  // exact 64-bit (no Double rounding above 2^53)
        end;
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
          // ⛔ THE SAME TEXT, READ BY TWO DIFFERENT PARSERS. VAL has known FreeBASIC's number
          // grammar - the &H/&O/&B base prefixes, the saturating magnitude, the full 64 bits -
          // since it was written, and INPUT# converted with the RTL's StrToFloatDef/StrToIntDef,
          // which know none of it and follow the locale's decimal separator besides. So
          // "&h1F" read back as 0 and 9223372036854775807 as -1, while VAL("&h1F") was 31.
          // One grammar, one parser: file/large_int.bas alone reads 4116 numbers this way.
          if Instr.Dest >= 0 then
            Ctx.FloatRegs[Instr.Dest] := ParseLeadingFloat(Trim(Data));
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
          // Same grammar as VAL - see the float arm above. StrToIntDef is a 32-BIT conversion
          // (its result is a LongInt), so every value past 2^31 came back as the default 0 even
          // when the register that holds it is 64 bits wide.
          // Immediate carries the READ KIND the SSA worked out from the destination's declared type,
          // the mirror of PRINT#'s: 1 = BOOLEAN. fbc reads the WORDS "true"/"false" (either case)
          // there, and anything else through the numeric grammar with "non-zero" meaning true - so
          // "1.7" is true and "abc" is false. Measured against fbc 1.10.1 for all nine forms.
          if Instr.Dest >= 0 then
          begin
            if Instr.Immediate = 1 then
            begin
              Mode := UpperCase(Trim(Data));
              if Mode = 'TRUE' then Ctx.IntRegs[Instr.Dest] := -1
              else if Mode = 'FALSE' then Ctx.IntRegs[Instr.Dest] := 0
              else if ParseLeadingFloat(Trim(Data)) <> 0.0 then Ctx.IntRegs[Instr.Dest] := -1
              else Ctx.IntRegs[Instr.Dest] := 0;
            end
            // ⭐⭐ AN INTEGER DESTINATION READS THE *FLOAT* GRAMMAR AND ROUNDS, and that is NOT what
            // VALINT does with the same text: fbc answers VALINT("1d1") = 1 and reads the very same
            // field into an Integer as 10. INPUT parses a NUMBER and then converts it, so a fraction
            // and an exponent both count - measured against fbc: "1.9" -> 2, "-1.9" -> -2, "2.5" -> 2,
            // "3.5" -> 4 (ties to even, the implicit conversion everywhere else), "1d1" -> 10,
            // "1.23d+2" -> 123, "1e18" -> 1000000000000000000.
            // ⛔ AND ONLY THEN. A plain integer must NOT go through a Double: fbc reads
            // "9223372036854775807" back exactly, and a Double cannot hold it. So the float path is
            // taken only for a field that actually IS one - a '.' or an exponent letter - and never
            // for a base-prefixed literal, where 'd'/'e' are HEX DIGITS ("&h1d1" is 465).
            // DIVERGENZE 123.
            else if InputFieldIsFloat(Trim(Data)) then
              Ctx.IntRegs[Instr.Dest] := FloatToIntConv(ParseLeadingFloat(Trim(Data)), True)
            else
              Ctx.IntRegs[Instr.Dest] := ParseLeadingInt64(Trim(Data), 64);
          end;
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
          if BinWidth < 0 then BinWidth := -BinWidth        // ZSTRING * n: read |n|, keep all of it
          else if BinWidth = 0 then
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
          // ⛔ A "ZSTRING * n" DESTINATION IS THE OTHER CONVENTION, and it needed saying: fbc reads
          // n-1 bytes there and KEEPS ALL OF THEM ("Dim z6 As ZString * 6" over "1234567890" leaves
          // "12345" and the file position at 6, not 7). A NEGATIVE immediate is that request - read
          // |n| bytes, drop nothing - because the two differ in what they KEEP, not in what they read,
          // so no width can express both. DIVERGENZE 125.
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
          BinArr := @FArrays[Ctx.ArrMap[Instr.Src2]];   // Src2 is a LOGICAL array id
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
        // ⛔ AN EMPTY FILESPEC MEANS "THE NEXT ONE", not a new search. The manual is explicit - "if
        // filespec is omitted or empty, the next matching file is returned" - and Dir("") is how a
        // FreeBASIC loop is actually written, the manual's own fileio and system examples included.
        // Taken as a new search it handed FindFirst an empty pattern, which matches nothing, so every
        // such loop stopped after its FIRST entry: a directory listing that silently listed one file.
        if (Instr.Immediate = 0) and (Ctx.StringRegs[Instr.Src1] <> '') then
        begin
          if FDirOpen then begin FindClose(FDirRec); FDirOpen := False; end;   // a new search cancels the old one
          FDirMask := Integer(Ctx.IntRegs[Instr.Src2]);
          FDirOpen := FindFirst(DirTranslateSpec(Ctx.StringRegs[Instr.Src1]), faAnyFile, FDirRec) = 0;
        end
        else if FDirOpen then
          if FindNext(FDirRec) <> 0 then begin FindClose(FDirRec); FDirOpen := False; end;
        // Filter here rather than through FindFirst's own mask, because FreeBASIC's rule is its own and
        // was read off the oracle: an entry is returned when every attribute bit it carries is one the
        // mask allows, with ARCHIVE allowed implicitly -- EXCEPT when the mask asks for directories, and
        // then archive is not implied and plain files drop out. That is what makes "Dir("*", fbDirectory)"
        // list directories ALONE while "fbDirectory Or fbArchive" lists both, and it fits all twelve
        // mask/entry combinations measured against fbc.
        while FDirOpen and (DirEntrySkipped(FDirRec) or
                            ((DirEntryAttrs(FDirRec) and not DirAllowedAttrs(FDirMask)) <> 0)) do
          if FindNext(FDirRec) <> 0 then begin FindClose(FDirRec); FDirOpen := False; end;
        if FDirOpen then
        begin
          Ctx.StringRegs[Instr.Dest] := FDirRec.Name;
          FDirAttr := DirEntryAttrs(FDirRec);
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

function TBytecodeVM.DivZeroFloat(Numerator, Denominator: Double): Double;
begin
  // MODERN (FreeBASIC) follows IEEE-754: a positive numerator over zero is +Inf, a negative one is -Inf,
  // and 0/0 is NaN. The result is built from Math-unit constants (a plain assignment, so it never triggers
  // the FP hardware trap that FPC leaves unmasked). CLASSIC (Commodore v7) raises ?DIVISION BY ZERO ERROR.
  if Assigned(FProgram) and FProgram.ModernMode then
  begin
    if Numerator = 0.0 then Result := NaN
    // IEEE gives the quotient the XOR of the two SIGN BITS, and a zero has one: 1/-0.0 is -Inf, not
    // +Inf. `Denominator < 0` cannot see it - negative zero compares EQUAL to zero - so the bit is
    // read directly. fbc agrees because it simply lets the hardware divide; this path exists only
    // because CLASSIC has to raise instead.
    else if (Numerator < 0.0) xor (PInt64(@Denominator)^ < 0) then Result := NegInfinity
    else Result := Infinity;
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

function TBytecodeVM.ErrorText(Code: Integer): string;
// The message for an error NUMBER, in the dialect's own table. The two collide: FreeBASIC 5 is
// "Illegal resume" and Commodore 5 is DEVICE NOT PRESENT, so "Error 5" in MODERN used to report the
// Commodore text - the numbers had been separated by dialect and the WORDS had not.
begin
  if Assigned(FProgram) and FProgram.ModernMode then
    Result := SedaiExecutorErrors.GetFBErrorCodeDescription(Code)
  else
    Result := SedaiExecutorErrors.GetErrorCodeDescription(Code);
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
  num, my, sc, nx, ny: Integer;
  angDeg: Double;                 // the current rotation, in DEGREES (A n is TA n*90)
  penFX, penFY: Double;           // the pen, carried at full precision for the whole string
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

  function Scaled(d: Integer): Double;
  begin Result := (d * sc) / 4; end;

  procedure StepPen(dxi, dyi: Integer);   // draw a scaled+rotated segment from the pen, honouring B/N
  // ⛔ THE ROTATION IS COUNTER-CLOCKWISE, and it used to be clockwise: measured against fbc, "A1 R20"
  // from (100,100) lands on (100,80) - UP - and we landed on (100,120). A1 and A3 were each other's.
  // ⭐ And it is one formula for BOTH A and TA now: A n is exactly TA (n*90). "TA" was not a case at
  // all, so "TA45" parsed as an unknown T followed by A45, and "45 and 3" made it a 90-degree turn -
  // an arbitrary angle silently snapped to a right angle.
  var
    dx, dy, ex, ey, cs, sn, r: Double;
  begin
    dx := Scaled(dxi); dy := Scaled(dyi);
    if angDeg <> 0 then
    begin
      r := angDeg * Pi / 180.0;
      cs := Cos(r); sn := Sin(r);
      ex := dx * cs + dy * sn;      // screen y grows DOWN, so a CCW turn is this pair of signs
      ey := -dx * sn + dy * cs;
      dx := ex; dy := ey;
    end;
    ex := penFX + dx; ey := penFY + dy;
    if (not blindP) and Assigned(FGraphics) then
      FGraphics.DrawLine(FGfxWorkSurface, GfxMapX(Round(penFX)), GfxMapY(Round(penFY)),
                         GfxMapX(Round(ex)), GfxMapY(Round(ey)), penColor, 1);
    if not noUpdateP then begin penFX := ex; penFY := ey; end;
  end;

begin
  if S = '' then Exit;
  i := 1;
  penColor := FGfxForeColor;
  sc := 4; angDeg := 0;
  // The pen is carried at full precision for the whole string and rounded back at the end: a rotated
  // step lands between pixels, and rounding every segment accumulates the error over a long figure.
  penFX := FDrawPenX; penFY := FDrawPenY;
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
      'A': if ReadNum(num, sgn) then angDeg := (num and 3) * 90.0;
      // TA n - rotate by n DEGREES, not by quarter turns. It was not a case at all, so "TA45" read as
      // an unknown T plus "A45" and snapped to a right angle.
      'T':
        begin
          if (i <= Length(S)) and (UpCase(S[i]) = 'A') then
          begin
            Inc(i);
            if ReadNum(num, sgn) then angDeg := num;
          end;
        end;
      // P colour,border - flood fill from the pen, bounded by a border colour. Never implemented, so
      // the manual's own DRAW example ("P 1,2" inside a box) drew the box and left it hollow.
      'P':
        begin
          if ReadNum(num, sgn) then
          begin
            if (i <= Length(S)) and (S[i] = ',') then Inc(i);
            if not ReadNum(my, sgnY) then my := num;
            if Assigned(FGraphics) then
              FGraphics.FillBorder(FGfxWorkSurface, GfxMapX(Round(penFX)), GfxMapY(Round(penFY)),
                                   UInt32(num), UInt32(my));
          end;
        end;
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
          if sgn then begin nx := Round(penFX + Scaled(num)); ny := Round(penFY + Scaled(my)); end
          else begin nx := num; ny := my; end;
          if (not blindP) and Assigned(FGraphics) then
            FGraphics.DrawLine(FGfxWorkSurface, GfxMapX(Round(penFX)), GfxMapY(Round(penFY)),
                               GfxMapX(nx), GfxMapY(ny), penColor, 1);
          if not noUpdateP then begin penFX := nx; penFY := ny; end;
        end;
    end;
  end;
  // Hand the pen back to the integer pair POINTCOORD and PSET share - and which the C hot loop writes
  // directly, which is why there is one authoritative INTEGER pen and the fractional one lives only
  // for the duration of a DRAW string.
  FDrawPenX := Round(penFX); FDrawPenY := Round(penFY);
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

procedure ReportPairCounts;
// The top adjacent pairs actually executed. Printed raw: the ranking is the answer, and deciding
// which of them is FUSABLE (same operand slots, temporary dead, no jump target in between) is a
// separate question that belongs to whoever writes the fusion.
var
  a, b, i, j, n: Integer;
  KA, KB: array of Word;
  KC: array of LongWord;
  tc: LongWord; tw: Word;
  Tot: Int64;
begin
  if not GPairDiag then Exit;
  SetLength(KA, 0); SetLength(KB, 0); SetLength(KC, 0); Tot := 0;
  for a := 0 to 2047 do
    for b := 0 to 2047 do
      if GPairCount[a, b] > 0 then
      begin
        Tot := Tot + GPairCount[a, b];
        n := Length(KA); SetLength(KA, n+1); SetLength(KB, n+1); SetLength(KC, n+1);
        KA[n] := a; KB[n] := b; KC[n] := GPairCount[a, b];
      end;
  if Length(KA) = 0 then begin WriteLn(ErrOutput, '[PAIR] nessuna coppia'); Exit; end;
  for i := 0 to High(KA) - 1 do
    for j := i + 1 to High(KA) do
      if KC[j] > KC[i] then
      begin
        tc := KC[i]; KC[i] := KC[j]; KC[j] := tc;
        tw := KA[i]; KA[i] := KA[j]; KA[j] := tw;
        tw := KB[i]; KB[i] := KB[j]; KB[j] := tw;
      end;
  // ⛔ THE CUT-OFF USED TO BE 20, HARD-CODED, and a census that shows only its own top 20 cannot
  // answer "was this opcode executed at all?" - which is the question the rare ones are asked. The
  // rare pairs are exactly the ones a coverage check needs. PAIR_DIAG=1 keeps the old 20;
  // PAIR_DIAG=<n> shows n; PAIR_DIAG=all shows every pair.
  if GPairDiagTop <= 0 then
    WriteLn(ErrOutput, '[PAIR] coppie adiacenti eseguite (totale ', Tot, '), tutte:')
  else
    WriteLn(ErrOutput, '[PAIR] coppie adiacenti eseguite (totale ', Tot, '), le prime ', GPairDiagTop, ':');
  for i := 0 to High(KA) do
  begin
    if (GPairDiagTop > 0) and (i >= GPairDiagTop) then Break;
    WriteLn(ErrOutput, '[PAIR]   ', KC[i]:12, '  ', OpcodeToString(SlotOpcode(KA[i])), ' -> ', OpcodeToString(SlotOpcode(KB[i])));
  end;
end;

procedure ReportSuperCounts;
// The nested-dispatch census, printed at shutdown. Sorted by count: an arm reached rarely costs
// nothing to leave here, one reached in an inner loop pays the second dispatch every iteration.
var
  i, j, n, t: Integer;
  Idx: array of Integer;
  Tot: Int64;
begin
  if not GSuperDiag then Exit;
  SetLength(Idx, 0); Tot := 0;
  for i := 0 to 255 do
    if GSuperCount[i] > 0 then
    begin
      n := Length(Idx); SetLength(Idx, n + 1); Idx[n] := i; Tot := Tot + GSuperCount[i];
    end;
  if Length(Idx) = 0 then begin WriteLn(ErrOutput, '[SUPER] nessun dispatch annidato'); Exit; end;
  for i := 0 to High(Idx) - 1 do
    for j := i + 1 to High(Idx) do
      if GSuperCount[Idx[j]] > GSuperCount[Idx[i]] then
      begin t := Idx[i]; Idx[i] := Idx[j]; Idx[j] := t; end;
  WriteLn(ErrOutput, '[SUPER] dispatch annidati, per sotto-opcode (totale ', Tot, '):');
  for i := 0 to High(Idx) do
    WriteLn(ErrOutput, '[SUPER]   sub=', Idx[i]:3, '  ', GSuperCount[Idx[i]]:14,
            '  (', OpcodeToString(Word($C800 or Idx[i])), ')');
end;

procedure ReportHotCExits;
// The HOTC_DIAG census, printed once at shutdown so it covers every engine and every thread that
// ran. Sorted by count, because the ranking IS the answer: an uncovered opcode that never lands in
// a loop costs nothing, and one that lands in the innermost loop of a benchmark costs it the whole
// C loop. Printed to stderr so it never mixes with a program's own output.
var
  i, j, n: Integer;
  Idx: array of Integer;
  t: Integer;
  Tot: Int64;
begin
  if (not GHotCDiag) or GHotCReported then Exit;
  GHotCReported := True;
  SetLength(Idx, 0);
  Tot := 0;
  for i := 0 to High(GHotCExit) do
    if GHotCExit[i] > 0 then
    begin
      n := Length(Idx); SetLength(Idx, n + 1); Idx[n] := i;
      Tot := Tot + GHotCExit[i];
    end;
  if GHotCBudgetExits > 0 then
    WriteLn(ErrOutput, '[HOTC] uscite per BUDGET sui back edge = ', GHotCBudgetExits,
            ' (non sono opcode scoperti: il ciclo e'' rientrato per far pompare gli eventi)');
  if Length(Idx) = 0 then
  begin
    WriteLn(ErrOutput, '[HOTC] nessuna uscita dal ciclo caldo C (ingressi=', GHotCCalls, ')');
    Exit;
  end;
  for i := 0 to High(Idx) - 1 do
    for j := i + 1 to High(Idx) do
      if GHotCExit[Idx[j]] > GHotCExit[Idx[i]] then
      begin t := Idx[i]; Idx[i] := Idx[j]; Idx[j] := t; end;
  WriteLn(ErrOutput, '[HOTC] ingressi nel ciclo C=', GHotCCalls);
  WriteLn(ErrOutput, '[HOTC] uscite dal ciclo caldo C, per opcode (totale ', Tot, '):');
  for i := 0 to High(Idx) do
    WriteLn(ErrOutput, '[HOTC]   ', GHotCExit[Idx[i]]:12, '  ', OpcodeToString(Word(Idx[i])));
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
  AddExitProc(@ReportHotCExits);
  AddExitProc(@ReportSuperCounts);
  AddExitProc(@ReportPairCounts);

finalization
  // NOT the only place it is called from: see the AddExitProc in the initialization above. A CLASSIC
  // program ends at END, which halts, and a halt does not always reach unit finalization - so the
  // census was silent on exactly the programs it was written to measure, and silence read as
  // "no exits". ReportHotCExits guards itself against running twice.
  AotCallProfReport;

end.
