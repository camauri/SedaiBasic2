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
{ ============================================================================
  OptimizationFlags.inc

  Shared optimization flags for the entire SedaiBasic2 compiler.
  Include this file in any unit that needs to respect optimization settings.

  Usage:
    {$I OptimizationFlags.inc}

  Master control is in SedaiBasicVM.lpr - modify DISABLE_ALL_OPTIMIZATIONS
  there to enable/disable all optimizations at once.
  ============================================================================ }

// Master switch: uncomment to disable ALL optimizations
{.$DEFINE DISABLE_ALL_OPTIMIZATIONS}

// ⛔ SUPERINSTRUCTIONS OFF - and this time WHY is measured, not asserted. Two separate problems,
// one fixed and one still open. Do not re-enable until the second is fixed.
//
// PROBLEM 1 - FIXED. The pass rewrites BYTECODE, which is what the JIT consumes; the JIT declines any
// hot loop holding an opcode it cannot lower, and every superinstruction is one. Turned on globally
// it does not slow the JIT, it SWITCHES IT OFF: fannkuch --jit 178 ms -> 4378, a factor of 24, and an
// official benchmark battery ran 85 minutes with half its cells measuring the interpreter under the
// JIT's name. The AOT loses too, through the regions it declines and runs interpreted. The gate now
// lives INSIDE RunSuperinstructions (GAotWillRun / GJitWillRun) because there are FOUR callers -
// SedaiRunner, SedaiBasicVM.lpr, SedaiBasicCompiler.lpr, SedaiWebServer - and gating one left `sb`
// itself still fusing.
//
// PROBLEM 2 - OPEN, AND IT IS THE REAL ONE. Even for the interpreter alone the family LOSES:
// binary-trees +36%, spectral-norm +53%, fannkuch +49%, n-body +12%. The cause is the SUB-OPCODE
// NUMBERING. The 61 sub-opcodes are scattered over 0..255 (0-5, 10-15, 20-22, ... 250-255), so the
// inner "case Op and $FF of" compiles to a DECISION TREE rather than a jump table. Isolated outside
// the VM - the same case with 61 labels, sparse against dense, 20M dispatches:
//
//        sparse (0..255)  80 ms        dense (0..60)  30 ms       2.7x
//
// A fused opcode therefore costs 16.7 ns against a plain one's 4.05, and folding three cheap
// instructions into one expensive one is a loss by construction. THE FIX IS TO NUMBER THEM DENSELY
// (0..60), which makes the inner case a jump table and leaves the outer one alone.
//
// ⛔ What is NOT the fix, both tried on 18 Aug: flattening the inner arms into the outer dense case
// (it damaged the dispatch of EVERY opcode - +12% to +15% on every program with fusion still off,
// measured against a binary built from the previous commit), and hoisting the hot few in front of the
// inner case (micro -28% but fannkuch +81%, because every other superinstruction then pays six
// compares first).
//
// ⚠️ And a warning for whoever measures the renumbering: an A/B against the same binary with a
// runtime switch is NOT enough when the change also alters code the switch does not disable. Build
// the previous commit. That mistake is what made the first attempt look like a win.
//
// Kept from the attempt: the two genuine miscompiles fixed in TryFuseConstantArithmetic, and the
// SUPERINSTR=0 / SUPERMASK=<n,...> switches that found them.
// See job/docs/VM_DISPATCH_E_RIDIMENSIONABILITA.md and the JIT design doc.
{$DEFINE DISABLE_SUPERINSTRUCTIONS}
{$DEFINE DISABLE_ARRAYSTORECONST}

// Individual optimization flags (controlled by master switch)
// Ordered by execution sequence in the optimization pipeline

{$IFDEF DISABLE_ALL_OPTIMIZATIONS}
  {$DEFINE DISABLE_CONSTANT_FOLDING}   // Skip constant folding in SSA generation
  {$DEFINE DISABLE_SUB_INLINING}       // Skip SUB/FUNCTION inlining (unification pass, pre-everything)
  {$DEFINE DISABLE_DBE}                // Skip dead block elimination
  {$DEFINE DISABLE_DOMINATOR_TREE}     // Skip dominator tree construction
  {$DEFINE DISABLE_SSA_CONSTRUCTION}   // Skip Semi-Pruned SSA construction (PHI + versioning)
  {$DEFINE DISABLE_GVN}                // Skip Global Value Numbering (mutually exclusive with CSE)
  {$DEFINE DISABLE_CSE}                // Skip common subexpression elimination (mutually exclusive with GVN)
  {$DEFINE DISABLE_ALGEBRAIC}          // Skip algebraic simplification
  {$DEFINE DISABLE_STRENGTH_RED}       // Skip strength reduction
  {$DEFINE DISABLE_GOSUB_INLINE}       // Skip GOSUB inlining
  {$DEFINE DISABLE_CONST_PROP}         // Skip aggressive constant propagation
  {$DEFINE DISABLE_COPY_PROP}          // Skip copy propagation
  {$DEFINE DISABLE_LICM}               // Skip loop-invariant code motion
  {$DEFINE DISABLE_LOOP_UNROLL}        // Skip loop unrolling optimization
  {$DEFINE DISABLE_DCE}                // Skip dead code elimination (removes dead PHI, LoadConst, etc.)
  {$DEFINE DISABLE_RANGE_ANALYSIS}     // Skip B4 range analysis (bounds-check elimination hints for AOT/JIT)
  {$DEFINE DISABLE_PHI_ELIM}           // Skip PHI Elimination (converts PHI to Copy - MUST run after DCE)
  {$DEFINE DISABLE_COPY_COAL}          // Skip Copy Coalescing (removes redundant Copy instructions)
  {$DEFINE DISABLE_REG_ALLOC}          // Skip Register Allocation (leave SSA form with virtual registers)
  {$DEFINE DISABLE_PEEPHOLE}           // Skip peephole optimizations
  {$DEFINE DISABLE_SUPERINSTRUCTIONS}  // Skip Superinstruction generation
  {$DEFINE DISABLE_ARRAYSTORECONST}    // Skip ArrayStoreConst superinstruction fusion
  {$DEFINE DISABLE_NOP_COMPACTION}     // Skip NOP Compaction (removes NOPs after fusion)
  {$DEFINE DISABLE_PEEPHOLE_PASS2}     // Skip second peephole pass (redundant jump elimination)
  {$DEFINE DISABLE_REG_COMPACTION}     // Skip Register Compaction (post-bytecode)
{$ENDIF}

{.$DEFINE DISABLE_REG_COMPACTION}

// SUB INLINING (unification pass, 2026-07-21): small LEAF SUB/FUNCTIONs are flattened into
// their call sites right after SSA generation, with per-site register renaming - every engine
// wins (measured: interp -9%, --aot -33% on n-body; the AOT nearly reaches the JIT without
// bytecode-level inlining). V1 excludes UDT/record callees and local-array callees (see
// SedaiSubInlining's header). Uncomment to disable.
{.$DEFINE DISABLE_SUB_INLINING}

// B4 RANGE ANALYSIS (bounds-check elimination hints for the AOT/JIT backends).
// The interpreter ignores the hint (it always checks), so this pass cannot
// change interpreted output. Measured: ~-9% on array-bound integer loops
// (both engines), ~-1..3% on the FP-bound n-body. Uncomment to disable.
{.$DEFINE DISABLE_RANGE_ANALYSIS}

// COPY COALESCING IS OFF: IT MISCOMPILES.
//
// SedaiCopyCoalescing rewrites "d := Copy s" by replacing every use of d with s across the WHOLE
// program and NOPping the copy. Its header claims a live-range interference check; there is none. Its
// only guard is "s must not be a user variable", which says nothing about whether s or d is redefined.
//
// That is unsound for exactly the instructions the pass exists to remove. PHI elimination gives a PHI
// target d one copy PER PREDECESSOR: "d := Copy s1" in one, "d := Copy s2" in another. The pass
// coalesces the first, rewrites EVERY use of d to s1, and deletes that copy -- so on the path through
// the second predecessor, d is still assigned s2 while the uses now read s1. Wrong value, no diagnostic.
//
// Found by the Rosetta sweep (Mayan_numerals): the optimized build dropped and reordered printed items
// while --no-opt was correct. Guard m392 keeps a reproducer in the corpus, so the differential
// (opt vs --no-opt) net fails immediately if this is switched back on unchanged.
//
// Re-enabling requires a real implementation: single-definition (or dominance) checks on BOTH the copy's
// destination and its source, or a proper interference graph. Same reasoning that keeps GVN/CSE off.
{$DEFINE DISABLE_COPY_COAL}

// GVN / CSE are mutually exclusive in the pipeline gating: GVN runs only when
// DISABLE_CSE is defined, CSE runs only when DISABLE_GVN is defined. With NEITHER
// defined (the current state) the gating runs NEITHER, so no SSA redundancy pass
// runs. GVN itself is correct (inter-block, dominator-scoped) and verified against
// the regression corpus, but it is left OFF: on the current line-per-block CFG it
// finds little, and the array loop-idiom superinstructions already capture the wins.
//
// Re-evaluated with fresh data 2026-06-20 (DISABLE_CSE on -> GVN runs): the produced
// bytecode was BYTE-IDENTICAL to GVN-off on every benchmark (n-body 485, fannkuch 81,
// spectral-norm 158, sieve 87, sieve_int 77) and the corpus differential stayed 47/47,
// 0 OPTDIFF. So GVN is correct but yields ZERO reduction here -- enabling it only adds
// compile-time work. Left OFF until the CFG/pipeline rework that would make it pay off.
// To toggle:
//   - GVN on:  uncomment {$DEFINE DISABLE_CSE} below.
//   - CSE on:  define DISABLE_GVN instead.
{$DEFINE DISABLE_CSE}
// GVN ENABLED 2026-07-20 (Phase A versioned-SSA pipeline) after making it sound:
//   (a) value numbering is restricted to SINGLE-ASSIGNMENT names, checked directly on a whole-program
//       def count (TGVNPass.BuildDefCounts/IsSingleDef) — covers CLASSIC Version=0 variables (m406),
//       MODERN multi-def temps, and MID-statement string rebuilds (m80) with one criterion;
//   (b) potentially-TRAPPING ops are excluded (their runtime error is observable under ON ERROR, so
//       every occurrence must re-execute — m138): integer div/mod always; float div/pow, FloatToInt,
//       Sqr/Log/Exp only in CLASSIC (MODERN is IEEE and stays pure).
// Verified: corpus 470/470, 0 OPTDIFF, JIT net 0 mismatch; benchmark bytecode byte-identical to
// GVN-off (their redundancies are loop-variant); cross-line scalar dedup works (3x "x*y+1" -> 1 mul).