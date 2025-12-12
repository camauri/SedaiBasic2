{*
 * SedaiBasic - Build System
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

{$mode objfpc}{$H+}
program fpmake;

uses
  fpmkunit;

var
  P: TPackage;
  T: TTarget;

begin
  with Installer do
  begin
    P := AddPackage('sedaibasic');
    P.Version := '1.0.0';
    P.Author := 'Maurizio Cammalleri';
    P.License := 'GPL-3.0-only OR Commercial';
    P.Email := 'maurizio.cammalleri@gmail.com';
    P.Description := 'SedaiBasic - A BASIC interpreter with bytecode VM';
    P.OSes := [Win32, Win64, Linux, Darwin];
    P.CPUs := [i386, x86_64, aarch64];

    // Source paths
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    // =========================================================================
    // sb - SedaiBasic VM (main interpreter)
    // =========================================================================
    T := P.Targets.AddProgram('SedaiBasicVM.lpr');
    with T.Dependencies do
    begin
      AddInclude('OptimizationFlags.inc');
      AddInclude('DebugFlags.inc');
      AddInclude('ProfilerFlags.inc');
      AddInclude('Version.inc');
    end;

    // =========================================================================
    // sbc - SedaiBasic Compiler
    // =========================================================================
    T := P.Targets.AddProgram('SedaiBasicCompiler.lpr');
    with T.Dependencies do
    begin
      AddInclude('OptimizationFlags.inc');
      AddInclude('Version.inc');
    end;

    // =========================================================================
    // sbd - SedaiBasic Disassembler
    // =========================================================================
    T := P.Targets.AddProgram('SedaiBasicDisassembler.lpr');
    with T.Dependencies do
    begin
      AddInclude('Version.inc');
    end;

    // =========================================================================
    // sbv - SedaiVision (SDL2 graphical version)
    // =========================================================================
    T := P.Targets.AddProgram('SedaiVision.lpr');
    with T.Dependencies do
    begin
      AddInclude('OptimizationFlags.inc');
      AddInclude('DebugFlags.inc');
      AddInclude('ProfilerFlags.inc');
      AddInclude('Version.inc');
    end;

    Run;
  end;
end.
