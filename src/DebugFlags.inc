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
  DebugFlags.inc

  Compile-time debug flags for the SedaiBasic2 compiler.

  USAGE:
    1. Define flags here to ENABLE debug code compilation
    2. Pass command-line parameters to ACTIVATE debug output at runtime

  IMPORTANT: Debug output requires BOTH:
    - Compile-time: Flag defined here (code is compiled)
    - Runtime: Command-line parameter passed (output is produced)

  This two-level system ensures:
    - Release builds have ZERO debug overhead (flags undefined)
    - Debug builds can selectively enable output via command line

  Command-line parameters (only work if corresponding flag is defined):
    --debug-all          Enable ALL debug output
    --debug-ssa          SSA construction debug
    --debug-gvn          Global Value Numbering debug
    --debug-cse          Common Subexpression Elimination debug
    --debug-dce          Dead Code Elimination debug
    --debug-licm         Loop-Invariant Code Motion debug
    --debug-algebraic    Algebraic Simplification debug
    --debug-strength     Strength Reduction debug
    --debug-constprop    Constant Propagation debug
    --debug-copyprop     Copy Propagation debug
    --debug-copycoal     Copy Coalescing debug
    --debug-phielim      PHI Elimination debug
    --debug-regalloc     Register Allocation debug
    --debug-peephole     Peephole Optimization debug
    --debug-superinstr   Superinstruction Fusion debug
    --debug-domtree      Dominator Tree debug
    --debug-dbe          Dead Block Elimination debug
    --debug-bytecode     Bytecode Compilation debug
    --debug-vm           Virtual Machine execution debug
    --debug-cleanup      Memory cleanup/destructor debug
  ============================================================================ }

{ ============================================================================
  MASTER DEBUG SWITCH

  Uncomment to enable ALL debug flags at once (for development)
  ============================================================================ }
{.$DEFINE DEBUG_ALL}

{ ============================================================================
  INDIVIDUAL DEBUG FLAGS

  Uncomment specific flags to enable debug code compilation for that module.
  The debug output will only appear if the corresponding --debug-xxx parameter
  is passed at runtime.

  NOTE: If DEBUG_ALL is defined, all individual flags are automatically enabled.
  You can still define individual flags independently when DEBUG_ALL is not set.
  ============================================================================ }

{ Individual flags - uncomment as needed for targeted debugging }
{.$DEFINE DEBUG_SSA}
{.$DEFINE DEBUG_GVN}
{.$DEFINE DEBUG_CSE}
{.$DEFINE DEBUG_DCE}
{.$DEFINE DEBUG_LICM}
{.$DEFINE DEBUG_ALGEBRAIC}
{.$DEFINE DEBUG_STRENGTH}
{.$DEFINE DEBUG_CONSTPROP}
{.$DEFINE DEBUG_COPYPROP}
{.$DEFINE DEBUG_COPYCOAL}
{.$DEFINE DEBUG_PHIELIM}
{.$DEFINE DEBUG_REGALLOC}
{.$DEFINE DEBUG_PEEPHOLE}
{.$DEFINE DEBUG_SUPERINSTR}
{.$DEFINE DEBUG_DOMTREE}
{.$DEFINE DEBUG_DBE}
{.$DEFINE DEBUG_BYTECODE}
{.$DEFINE DEBUG_VM}
{.$DEFINE DEBUG_CLEANUP}
{.$DEFINE DEBUG_CONSOLE}     // Interactive console (SDL2) debug
{.$DEFINE DEBUG_AUDIO}      // Audio subsystem debug (WITH_SEDAI_AUDIO required)

{ ============================================================================
  AUTO-ENABLE ALL FLAGS WHEN DEBUG_ALL IS SET
  ============================================================================ }
{$IFDEF DEBUG_ALL}
  {$DEFINE DEBUG_SSA}
  {$DEFINE DEBUG_GVN}
  {$DEFINE DEBUG_CSE}
  {$DEFINE DEBUG_DCE}
  {$DEFINE DEBUG_LICM}
  {$DEFINE DEBUG_ALGEBRAIC}
  {$DEFINE DEBUG_STRENGTH}
  {$DEFINE DEBUG_CONSTPROP}
  {$DEFINE DEBUG_COPYPROP}
  {$DEFINE DEBUG_COPYCOAL}
  {$DEFINE DEBUG_PHIELIM}
  {$DEFINE DEBUG_REGALLOC}
  {$DEFINE DEBUG_PEEPHOLE}
  {$DEFINE DEBUG_SUPERINSTR}
  {$DEFINE DEBUG_DOMTREE}
  {$DEFINE DEBUG_DBE}
  {$DEFINE DEBUG_BYTECODE}
  {$DEFINE DEBUG_VM}
  {$DEFINE DEBUG_CLEANUP}
  {$DEFINE DEBUG_CONSOLE}
  {$DEFINE DEBUG_AUDIO}
{$ENDIF}

{ ============================================================================
  ANY_DEBUG_ENABLED - Set if at least one debug flag is active

  Used to conditionally show/hide debug options in help output.
  ============================================================================ }
{$IFDEF DEBUG_ALL}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_SSA}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_GVN}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_CSE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_DCE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_LICM}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_ALGEBRAIC}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_STRENGTH}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_CONSTPROP}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_COPYPROP}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_COPYCOAL}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_PHIELIM}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_REGALLOC}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_PEEPHOLE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_SUPERINSTR}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_DOMTREE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_DBE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_BYTECODE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_VM}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_CLEANUP}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_CONSOLE}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}
{$IFDEF DEBUG_AUDIO}
  {$DEFINE ANY_DEBUG_ENABLED}
{$ENDIF}

{ ============================================================================
  VM INSTRUMENTATION FLAGS

  These flags add instrumentation to the VM for performance analysis.
  Unlike debug flags, these don't require runtime parameters.
  ============================================================================ }

{.$DEFINE ENABLE_INSTRUCTION_COUNTING}  // Count total instructions executed
                                        // Comment out for maximum performance

{.$DEFINE DEBUG_FLOAT_CHECKS}          // Enable NaN/Infinity checks (slow!)
{.$DEFINE DEBUG_REGISTER_DUMP}         // Dump register values on error (very slow!)

{ ============================================================================
  SSA CONSTRUCTION TIMING

  Uncomment to see internal timing breakdown of SSA Construction phases:
  - ComputeDF: Dominance Frontier computation
  - InsertPhi: PHI function insertion
  - Rename: Variable renaming
  ============================================================================ }
{$DEFINE DEBUG_SSA_TIMING}

