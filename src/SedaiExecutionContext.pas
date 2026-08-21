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
unit SedaiExecutionContext;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  // Only for TLimbs, the limb vector a BigInt value holds. SedaiBigInt depends on
  // nothing of ours, so this cannot close a cycle.
  SedaiBigInt;

{ TExecutionContext — the per-thread-of-control execution state of the bytecode VM.

  M5.1 (multithreading groundwork, see job/docs/SEDAIBASIC_EVOLUTION.md S16.2/S16.8):
  TBytecodeVM historically bundled *execution state* (one running thread of control) with
  *shared program/runtime state* (the heap, global arrays, I/O devices). This class isolates
  the execution state so that, once OS threads land (M5.2), each worker can run its own
  TExecutionContext over the same shared program + heap + globals (FreeBASIC shared-memory
  model). For now the VM owns exactly one instance (FCtx) and the single-threaded run uses it,
  so this is a pure no-op relocation: language behaviour — both BASIC v7 (CLASSIC) and
  FreeBASIC (MODERN) — is unchanged, guarded by the corpus + --no-opt differential.

  This is a dumb data holder: the VM reads and writes the fields directly; there are no
  methods beyond construction. Object fields are zero-initialised on Create, so scalar
  state starts at 0 / False / '' as the VM expects. }

type
  { UDT/record instance storage (M3): a heap block of typed slot arrays, referenced by a
    handle (its index in the record heap). M5.2b: the heap is per-context (per-thread), so a
    thread's RAII frame-mark rollback reclaims only its own records, never another thread's. }
  { A3-i: the numeric half of a record is ONE BYTE IMAGE, laid out exactly as fbc lays the type out
    (SedaiSSA.ComputeUDTLiveLayout). It used to be two parallel slot arrays - IntData and FloatData -
    one per register bank, which made a byte offset a thing that did not exist: OFFSETOF had to
    fabricate one, a UNION could only ever overlap members of the SAME bank, and "b As UByte" over
    "d As Double" could not alias at all because they lived in different arrays.

    A field is now (offset, width, signedness) into Bytes. Narrow fields cost nothing extra on x86:
    a one-byte store is a one-byte store, not the read-modify-write the plan feared.

    ⚠️ STRINGS STILL LIVE OUTSIDE, in StringData, indexed by slot. That is what makes this A3-i
    rather than A3-ii: a string is owned by the allocator and is not an inert byte. The layout DOES
    reserve the bytes a fixed-length "String * n" field occupies in fbc's image, at fbc's offset, so
    moving the characters in later is a change to that field's accessors and moves no other field. }
  { One BigInt value. The magnitude is normalised - no leading zero limb - because
    a comparison looks at N first and a leading zero would make the smaller number
    come out larger. Zero is N=1, Limbs[0]=0, Neg=False, so there is exactly one
    representation of it and no "negative zero" to reason about.
    ⚠️ Limbs is an FPC dynamic array: refcounted, but WITHOUT copy-on-write. That
    is the whole reason UniqueLimbs exists in SedaiBigInt.pas. }
  TBigValue = record
    Limbs: TLimbs;            // magnitude, least significant limb first
    N: Integer;               // limbs in use (Length(Limbs) may be larger)
    Neg: Boolean;             // sign, kept apart: the limbs are unsigned
    // ⭐ THE REGISTER THAT OWNS THIS HANDLE, or -1. It is what lets an operation
    // REUSE the handle its destination register already holds instead of allocating
    // a new one per operation: a register may only overwrite a value it owns, so a
    // stale or foreign handle can never be scribbled on. Without it every BigInt
    // operation allocated, and pidigits spent its time in the allocator rather than
    // in the arithmetic.
    Owner: Integer;
  end;

  TRecordStorage = record
    TypeId: Integer;          // M4.3: runtime UDT type id (for virtual dispatch); -1 if untyped
    Bytes: array of Byte;     // the record's live C image: numeric fields at their true offsets
    StringData: array of string;
  end;
  PRecordStorage = ^TRecordStorage;   // M5.2c: stable pointer to a record (per-thread or shared region)
  // The shared region's index -> record table. Named so an outgrown copy can be RETIRED (kept alive)
  // rather than freed: that is what lets a handle lookup run without taking the region's lock.
  TSharedRecArray = array of PRecordStorage;

  { One array's storage. Lived in SedaiBytecodeVM until 21 Aug 2026 and moved here for the same reason
    TRecordStorage did: the moment a kind of storage becomes PER-CONTEXT, its type has to be visible to
    the context. The VM still owns the table of these; what is per-context is the BIND SAVE-STACK below
    and the map from a bytecode array id to a slot in that table (ArrMap). }
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

  TArrayBindEntry = record
    SlotId: Integer;
    ArgId: Integer;
    Saved: TArrayStorage;
    Snapshot: TArrayStorage;
  end;

  { One call frame's bookkeeping, pushed by FramePush and read back by FramePop. 32 bytes, so two
    frames share a cache line - see the FrameMarks field for why this is one record and not the
    five parallel arrays it replaced. WInt/WFloat/WStr each pack (width shl 32) or base, the two
    ends of the bank range this frame snapshotted. }
  TFrameMark = record
    WInt: Int64;
    WFloat: Int64;
    WStr: Int64;
    RecBase: Integer;      // Ctx.RecordCount on entry: the frame's records die at pop
    BlockMark: Integer;    // Ctx.BlockRecMarkTop on entry: discards block marks the frame leaked
    // Frame base: the caller's view offset and high-water, restored at pop. DeltaI = -1 marks a
    // frame that did NOT relocate (the copying path ran instead), so pop knows not to slide back.
    SaveDeltaI: Integer;
    SaveHwI: Integer;
  end;
  PFrameMark = ^TFrameMark;

  TExecutionContext = class
  public
    // --- Register banks (one working set per context) ---
    // IntRegsMem OWNS the integer bank; IntRegs is a rebasable VIEW into it, and is what every
    // access site indexes. Splitting the two is what makes a call frame relocatable: instead of
    // COPYING the callee's register range out of the way (which is what a call has always done
    // here, because the banks are global and a second activation of the same procedure reuses the
    // same indices), an eligible call slides the view up by a delta and the callee's accesses land
    // in fresh slots. Every path benefits at once and none of them changes: the run loop caches
    // this pointer, the JIT and the AOT receive it as their first argument, and the interpreter's
    // slow path indexes it through Ctx. Only SetLength/Length touch IntRegsMem - and every
    // reallocation must refresh IntRegs, or the view dangles.
    IntRegsMem: array of Int64;
    IntRegs: PInt64;
    FloatRegs: array of Double;
    StringRegs: array of string;
    TempIntRegs: array of Int64;
    TempFloatRegs: array of Double;
    TempFStringRegs: array of string;
    IntRegCount: Integer;       // Current size of int register arrays
    FloatRegCount: Integer;     // Current size of float register arrays
    StringRegCount: Integer;    // Current size of string register arrays
    // Frame base (integer bank). RegDeltaI is how far the view above is slid from the allocation;
    // RegHwI is the first slot above every live frame, i.e. where the next relocated frame starts.
    // Both are per-context, so a worker keeps its own. RegFrameCap is where the relocatable region
    // ends: a call that would run past it falls back to the copying frame, which is always correct.
    RegDeltaI: Integer;
    RegHwI: Integer;
    RegFrameCap: Integer;

    // --- Program counter / run state ---
    PC: Integer;
    // M5.2: where this context's run loop starts. The main context uses -1 (= program EntryPoint);
    // a worker sets it to the entry PC of the SUB it runs, so Run begins at the worker's procedure.
    StartPC: Integer;
    // One-shot resume PC for a TRON/TROFF mode switch: RunFast/RunDebug hand control to each
    // other mid-program, and the re-entered loop must CONTINUE here rather than restart at
    // StartPC/EntryPoint. Consumed (reset to -1) at Run entry; -1 = no switch pending.
    // (Distinct from ResumePC below, which belongs to TRAP/RESUME error handling.)
    ModeSwitchPC: Integer;
    Running: Boolean;
    Stopped: Boolean;           // True if stopped by STOP command (can CONT)
    StoppedPC: Integer;         // PC to resume from when CONT is called
    TraceActive: Boolean;       // True = trace mode on (TRON), outputs line numbers
    LastSourceLine: Integer;    // Last source line executed (for trace output)

    // --- GOSUB call stack ---
    CallStack: array of Integer;
    CallStackPtr: Integer;

    // --- SUB/FUNCTION call frames (M2): flat per-bank save stacks ---
    FrameSaveInt: array of Int64;
    FrameSaveFloat: array of Double;
    FrameSaveStr: array of string;
    FrameSaveIntTop: Integer;
    FrameSaveFloatTop: Integer;
    FrameSaveStrTop: Integer;
    // How many slots per bank a call frame actually snapshots. The banks are allocated with a
    // 256-slot floor (MIN_REGISTER_SLOTS), but a program only ever touches registers below the
    // maximum index its bytecode mentions, so saving the whole bank is pure cost - and for the
    // STRING bank it is refcounted-copy cost, which dominated the per-call overhead. Set by
    // LoadProgram from the same whole-program scan that sizes the banks (and raised with them if
    // a bank grows); 0 until then, and FramePush clamps to the bank size, so a stale/zero value
    // can only ever save LESS than the bank, never out of range.
    FrameSaveIntCount: Integer;
    FrameSaveFloatCount: Integer;
    FrameSaveStrCount: Integer;
    // Everything one call frame records, in ONE 32-byte entry. This used to be FIVE parallel
    // dynamic arrays (three widths + the record base + the block mark) pushed and popped in
    // lockstep, and that layout - not the copying - was what a call actually cost: five field
    // loads, five Length() checks and five writes landing on five different cache lines to record
    // what is one small struct. AOT_CALLPROF attributed ~93 of the ~127 non-callee cycles of a
    // native call to FramePush/FramePop on a program that copies ONE integer per call, i.e. to
    // pointer chasing, not to bank traffic. One array of records makes it one length check and one
    // cache line per frame; two frames share a line.
    //
    // W* pack BOTH ends of the saved range, (width shl 32) or base. The saved part of a bank is a
    // RANGE, not a prefix: a procedure's registers are numbered above its caller's, so saving from
    // 0 copies registers the callee provably cannot reach. Packing beats a separate base stack -
    // that measured MORE in extra traffic than the copies it saved.
    //
    // A call only has to protect the registers its CALLEE (and everything the callee calls) can
    // touch, which is usually far below the program-wide width: the caller of a two-line SUB should
    // not pay for the biggest procedure in the program. Pushed by FramePush, read by FramePop.
    FrameMarks: array of TFrameMark;
    FrameMarkTop: Integer;
    // The five arrays the entry above replaced, kept ONLY so FRAMEMARK=0 can reproduce the historic
    // layout for an A/B on ONE binary. Two separately linked binaries differ in code alignment, and
    // this project has measured 14% from code that never ran, so a layout change cannot be judged by
    // comparing two builds. Dead unless the gate is off.
    FrameWidthInt: array of Int64;
    FrameWidthFloat: array of Int64;
    FrameWidthStr: array of Int64;
    FrameWidthTop: Integer;
    FrameRecBase: array of Integer;
    FrameBlockMarkTop: array of Integer;
    FrameRecBaseTop: Integer;

    // --- Record reclamation marks (RAII, M8 block) ---
    // The FRAME-level marks moved into TFrameMark above; these are the per-BLOCK ones, which are
    // pushed and popped on a different schedule and stay separate.
    BlockRecMark: array of Integer;
    BlockRecMarkTop: Integer;

    // --- Transfer slots (M2): carry args/result across a call's frame save/restore ---
    XferInt: array of Int64;
    XferFloat: array of Double;
    XferStr: array of string;

    // --- Cursor / print state ---
    CursorCol: Integer;         // Track cursor column for TAB zones
    CursorRow: Integer;         // Track cursor row for CSRLIN (incremented at each print newline)

    // --- TRAP/RESUME error handling state ---
    TrapLine: Integer;          // Line to jump to on error (0 = no trap)
    TrapPC: Integer;            // PC to jump to on error (resolved from TrapLine)
    ResumePC: Integer;          // PC to resume from after RESUME
    InErrorHandler: Boolean;    // True if currently in error handler
    LastErrorLine: Integer;     // EL: Last error line number
    LastErrorCode: Integer;     // ER: Last error code
    LastErrorMessage: string;   // ERR$: Last error message (variable form)
    LastErrorProc: string;      // ERFN: procedure in which the last error occurred ('' = module level)

    // --- Superinstruction scratch ---
    SwapTempInt: Int64;         // Temp variable for ArraySwapInt superinstruction
    StartIdx, EndIdx, ArrIdxTmp, LoopIdx: Integer;  // ArrayReverseRange / ArrayShiftLeft
    FirstVal: Int64;

    // --- DATA / READ / RESTORE cursor (the DATA pool itself stays shared) ---
    DataIndex: Integer;         // Current read position in the DATA pool

    // --- Array BYREF parameter binding (MODERN) ---
    // ⛔ PER-CONTEXT since 21 Aug 2026, and it had to move for the same reason the param slot itself
    // became private: binding aliases a callee param-array slot to the caller's array and pushes the
    // original here, to be restored on unbind. One stack for the whole VM meant two threads inside the
    // same SUB pushed and popped each other's entries - the alias one restored was the other's.
    // ArgId is the caller's array slot, kept so a REDIM [PRESERVE] inside the callee (which reallocates
    // the param's storage and thereby breaks the shared reference) is propagated back on unbind.
    ArrayBindStack: array of TArrayBindEntry;
    ArrayBindTop: Integer;

    // --- Array identity (M?, 21 Aug 2026): logical array id -> PHYSICAL slot in the VM's array table ---
    // ⛔ A proc-local array used to be ONE storage for the whole program: the slot id is an immediate
    // baked into the bytecode, so two threads inside the same SUB addressed the same elements and
    // overwrote each other (job/tests/bas/bug_local_array_shared_across_threads.bas). The bytecode
    // still names a LOGICAL id; this vector turns it into the physical slot THIS context owns.
    // Identity for every global array (ArrMap[i] = i), and each context's own block for the private
    // ones - which is why the lookup is a plain load and not a branch: the common case must not pay
    // for the rare one. Covers the STATIC id space only; a UDT member array allocated at runtime gets
    // a physical id beyond it and is never private (it lives in its record, not in a frame).
    ArrMap: array of Integer;
    ArrPrivBlock: Integer;      // index of the private block this context holds (-1 = none), released at exit
    // The compiled engines (the C hot loop, the AOT, the loop JIT) read an array through a DESCRIPTOR
    // table indexed by the id baked into their code - a LOGICAL id. They therefore cannot be handed the
    // VM's one table once private arrays exist: each context gets its own copy, identical to the master
    // except that the private entries point at the slots THIS context owns. That is the whole reason
    // hotdisp.c, SedaiAot and SedaiJit needed no change at all for per-thread local arrays.
    // Empty (and unused) when the program declares no private array: then the master table is handed
    // out exactly as before.
    ArrDescOwn: array of Int64;
    ArrDescRetired: array of array of Int64;  // superseded buffers; a native frame may still hold one
    ArrDescGen: Int64;          // the master generation this copy was built from (0 = never built)
    // ⛔ The pointer LAST PUBLISHED from ArrDescOwn. A caller that caches the table (the JIT arm)
    // must be able to ask "is what I hold still the current buffer", and the generation alone
    // cannot answer it: another arm of the same thread can rebuild this context's table, which
    // RETIRES the old buffer and allocates a new one, leaving the generation matching while the
    // cached pointer names a buffer that is one DIM out of date.
    ArrDescCur: Pointer;

    // --- UDT/record heap (M3; per-context since M5.2b) ---
    // Instances allocated by bcRecordNew; a handle is an index here. RAII (V2) rolls
    // RecordCount back to a frame's saved mark on exit, freeing that frame's records — sound
    // per-thread because each thread owns its heap (no cross-thread reclamation, S16.4).
    Records: array of TRecordStorage;
    RecordCount: Integer;

    // --- BigInt heap (per-context, exactly as the record heap above) ---
    // A BigInt VALUE is a handle into this array, which is why it is here and not
    // beside the register banks: the language already knows how to scope, pass and
    // reclaim a handle, and reusing that shape means BigInt adds a TYPE, not a new
    // storage concept. Magnitude in Limbs[0..N-1], least significant first; the
    // sign is separate because the limbs are unsigned and the core arithmetic in
    // SedaiBigInt.pas is deliberately magnitude-only.
    // ⚠️ Value semantics come from copy-on-write, NOT from copying on assignment:
    // BigCopy shares the limbs and UniqueLimbs splits them at the first write.
    BigVals: array of TBigValue;
    BigCount: Integer;
    // A service handle for operations that need an intermediate (division produces quotient
    // AND remainder together). ⛔ -1 until it is needed: 0 is a VALID handle, so a zeroed
    // sentinel would write over somebody else's BigInt.
    BigScratch: Integer;
    // Long division's workspace: Knuth's normalization builds a shifted dividend and divisor,
    // and allocating them on every call costs more than the algorithm itself. They grow once
    // and stay.
    BigDivU, BigDivV: TLimbs;

    // --- AOT runtime-helper handoff (C3, PIANO_B1_AOT_DESIGN §5.6) ---
    // An exception must never unwind through AOT-generated frames: they carry no unwind
    // info. The helper that runs one bytecode instruction for native code catches
    // everything, parks the exception object here, and returns a sentinel; the AOT call
    // site in RunTemplate re-raises it inside the interpreter's own try..except, which is
    // what knows about ON ERROR / TRAP / Err / RESUME. Per-context, so a worker thread
    // carries its own pending exception.
    //
    // Declared last: they are touched once per fault, while the fields above are the
    // interpreter's hot state, and appending avoids shifting every following field's offset.
    // (Measured as free either way - but see [[dispatch-alignment-fragility]]: this binary's
    // dispatch loop is sensitive enough to layout that not perturbing it is worth doing.)
    AotPendingExc: TObject;     // acquired exception object, owned until re-raised (nil = none)
    AotFaultPC: Integer;        // bytecode PC to resume/report at when a sentinel comes back
    // Set by AotSettle immediately before it re-raises, consumed (and cleared) by the run
    // loop's exception handler. Needed because that handler otherwise blames the PC the loop
    // variable happens to hold - which, for an exception raised out of the AOT call site, is
    // the region's ENTRY, not the instruction that failed. -1 = no AOT raise pending.
    AotRaisePC: Integer;
    // B3: nesting depth of native-to-native calls (AotCallSub frames on the PASCAL stack).
    // The interpreter's call stack lives on the heap and auto-grows, but each native call
    // level also consumes real machine stack; past the cap AotCallSub declines the call and
    // the caller falls back to the interpreted bcCallSub, which unwinds the whole native
    // chain and re-enters the callee natively from the run loop at depth ~0 - correct, and
    // amortized to one unwind per cap-many levels on deep recursion.
    AotCallDepth: Integer;
  end;

implementation

end.
