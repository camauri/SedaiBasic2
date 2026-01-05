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
  Unit: SedaiDebug

  Runtime debug flag management for SedaiBasic2 compiler.

  This unit provides:
    - Runtime debug flag variables (set via command-line)
    - Initialization from command-line parameters
    - Helper functions for conditional debug output

  USAGE:
    1. Include DebugFlags.inc in your unit
    2. Use SedaiDebug in your uses clause
    3. Call InitDebugFlags at program start
    4. Use DebugXxx variables to check if debug is enabled

  Example:
    {$I DebugFlags.inc}
    uses SedaiDebug;

    {$IFDEF DEBUG_GVN}
    if DebugGVN then
      WriteLn('[GVN] Some debug info...');
    {$ENDIF}
  ============================================================================ }

unit SedaiDebug;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$I DebugFlags.inc}

interface

var
  { Runtime debug flags - set by InitDebugFlags from command line }
  DebugAll: Boolean = False;
  DebugSSA: Boolean = False;
  DebugGVN: Boolean = False;
  DebugCSE: Boolean = False;
  DebugDCE: Boolean = False;
  DebugLICM: Boolean = False;
  DebugAlgebraic: Boolean = False;
  DebugStrength: Boolean = False;
  DebugConstProp: Boolean = False;
  DebugCopyProp: Boolean = False;
  DebugCopyCoal: Boolean = False;
  DebugPhiElim: Boolean = False;
  DebugRegAlloc: Boolean = False;
  DebugPeephole: Boolean = False;
  DebugSuperInstr: Boolean = False;
  DebugDomTree: Boolean = False;
  DebugDBE: Boolean = False;
  DebugBytecode: Boolean = False;
  DebugVM: Boolean = False;
  DebugCleanup: Boolean = False;

{ Initialize debug flags from command-line parameters }
procedure InitDebugFlags;

{ Check if any debug flag is active (for performance-critical code) }
function AnyDebugActive: Boolean;

implementation

uses
  SysUtils;

procedure InitDebugFlags;
var
  i: Integer;
  Param: string;
begin
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i));

    if Param = '--debug-all' then
    begin
      DebugAll := True;
      {$IFDEF DEBUG_SSA}DebugSSA := True;{$ENDIF}
      {$IFDEF DEBUG_GVN}DebugGVN := True;{$ENDIF}
      {$IFDEF DEBUG_CSE}DebugCSE := True;{$ENDIF}
      {$IFDEF DEBUG_DCE}DebugDCE := True;{$ENDIF}
      {$IFDEF DEBUG_LICM}DebugLICM := True;{$ENDIF}
      {$IFDEF DEBUG_ALGEBRAIC}DebugAlgebraic := True;{$ENDIF}
      {$IFDEF DEBUG_STRENGTH}DebugStrength := True;{$ENDIF}
      {$IFDEF DEBUG_CONSTPROP}DebugConstProp := True;{$ENDIF}
      {$IFDEF DEBUG_COPYPROP}DebugCopyProp := True;{$ENDIF}
      {$IFDEF DEBUG_COPYCOAL}DebugCopyCoal := True;{$ENDIF}
      {$IFDEF DEBUG_PHIELIM}DebugPhiElim := True;{$ENDIF}
      {$IFDEF DEBUG_REGALLOC}DebugRegAlloc := True;{$ENDIF}
      {$IFDEF DEBUG_PEEPHOLE}DebugPeephole := True;{$ENDIF}
      {$IFDEF DEBUG_SUPERINSTR}DebugSuperInstr := True;{$ENDIF}
      {$IFDEF DEBUG_DOMTREE}DebugDomTree := True;{$ENDIF}
      {$IFDEF DEBUG_DBE}DebugDBE := True;{$ENDIF}
      {$IFDEF DEBUG_BYTECODE}DebugBytecode := True;{$ENDIF}
      {$IFDEF DEBUG_VM}DebugVM := True;{$ENDIF}
      {$IFDEF DEBUG_CLEANUP}DebugCleanup := True;{$ENDIF}
    end
    {$IFDEF DEBUG_SSA}
    else if Param = '--debug-ssa' then DebugSSA := True
    {$ENDIF}
    {$IFDEF DEBUG_GVN}
    else if Param = '--debug-gvn' then DebugGVN := True
    {$ENDIF}
    {$IFDEF DEBUG_CSE}
    else if Param = '--debug-cse' then DebugCSE := True
    {$ENDIF}
    {$IFDEF DEBUG_DCE}
    else if Param = '--debug-dce' then DebugDCE := True
    {$ENDIF}
    {$IFDEF DEBUG_LICM}
    else if Param = '--debug-licm' then DebugLICM := True
    {$ENDIF}
    {$IFDEF DEBUG_ALGEBRAIC}
    else if Param = '--debug-algebraic' then DebugAlgebraic := True
    {$ENDIF}
    {$IFDEF DEBUG_STRENGTH}
    else if Param = '--debug-strength' then DebugStrength := True
    {$ENDIF}
    {$IFDEF DEBUG_CONSTPROP}
    else if Param = '--debug-constprop' then DebugConstProp := True
    {$ENDIF}
    {$IFDEF DEBUG_COPYPROP}
    else if Param = '--debug-copyprop' then DebugCopyProp := True
    {$ENDIF}
    {$IFDEF DEBUG_COPYCOAL}
    else if Param = '--debug-copycoal' then DebugCopyCoal := True
    {$ENDIF}
    {$IFDEF DEBUG_PHIELIM}
    else if Param = '--debug-phielim' then DebugPhiElim := True
    {$ENDIF}
    {$IFDEF DEBUG_REGALLOC}
    else if Param = '--debug-regalloc' then DebugRegAlloc := True
    {$ENDIF}
    {$IFDEF DEBUG_PEEPHOLE}
    else if Param = '--debug-peephole' then DebugPeephole := True
    {$ENDIF}
    {$IFDEF DEBUG_SUPERINSTR}
    else if Param = '--debug-superinstr' then DebugSuperInstr := True
    {$ENDIF}
    {$IFDEF DEBUG_DOMTREE}
    else if Param = '--debug-domtree' then DebugDomTree := True
    {$ENDIF}
    {$IFDEF DEBUG_DBE}
    else if Param = '--debug-dbe' then DebugDBE := True
    {$ENDIF}
    {$IFDEF DEBUG_BYTECODE}
    else if Param = '--debug-bytecode' then DebugBytecode := True
    {$ENDIF}
    {$IFDEF DEBUG_VM}
    else if Param = '--debug-vm' then DebugVM := True
    {$ENDIF}
    {$IFDEF DEBUG_CLEANUP}
    else if Param = '--debug-cleanup' then DebugCleanup := True
    {$ENDIF}
    ;
  end;
end;

function AnyDebugActive: Boolean;
begin
  Result := DebugAll or DebugSSA or DebugGVN or DebugCSE or DebugDCE or
            DebugLICM or DebugAlgebraic or DebugStrength or DebugConstProp or
            DebugCopyProp or DebugCopyCoal or DebugPhiElim or DebugRegAlloc or
            DebugPeephole or DebugSuperInstr or DebugDomTree or DebugDBE or
            DebugBytecode or DebugVM or DebugCleanup;
end;

end.
