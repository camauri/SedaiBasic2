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
  TExecutionContext = class
  public
    // --- Register banks (one working set per context) ---
    IntRegs: array of Int64;
    FloatRegs: array of Double;
    StringRegs: array of string;
    TempIntRegs: array of Int64;
    TempFloatRegs: array of Double;
    TempFStringRegs: array of string;
    IntRegCount: Integer;       // Current size of int register arrays
    FloatRegCount: Integer;     // Current size of float register arrays
    StringRegCount: Integer;    // Current size of string register arrays

    // --- Program counter / run state ---
    PC: Integer;
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

    // --- Record reclamation marks (RAII, V2 frame / M8 block) ---
    FrameRecBase: array of Integer;
    FrameRecBaseTop: Integer;
    BlockRecMark: array of Integer;
    BlockRecMarkTop: Integer;
    FrameBlockMarkTop: array of Integer;   // BlockRecMarkTop saved per frame

    // --- Transfer slots (M2): carry args/result across a call's frame save/restore ---
    XferInt: array of Int64;
    XferFloat: array of Double;
    XferStr: array of string;

    // --- Cursor / print state ---
    CursorCol: Integer;         // Track cursor column for TAB zones

    // --- TRAP/RESUME error handling state ---
    TrapLine: Integer;          // Line to jump to on error (0 = no trap)
    TrapPC: Integer;            // PC to jump to on error (resolved from TrapLine)
    ResumePC: Integer;          // PC to resume from after RESUME
    InErrorHandler: Boolean;    // True if currently in error handler
    LastErrorLine: Integer;     // EL: Last error line number
    LastErrorCode: Integer;     // ER: Last error code
    LastErrorMessage: string;   // ERR$: Last error message (variable form)

    // --- Superinstruction scratch ---
    SwapTempInt: Int64;         // Temp variable for ArraySwapInt superinstruction
    StartIdx, EndIdx, ArrIdxTmp, LoopIdx: Integer;  // ArrayReverseRange / ArrayShiftLeft
    FirstVal: Int64;

    // --- DATA / READ / RESTORE cursor (the DATA pool itself stays shared) ---
    DataIndex: Integer;         // Current read position in the DATA pool
  end;

implementation

end.
