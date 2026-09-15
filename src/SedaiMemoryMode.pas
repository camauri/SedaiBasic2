unit SedaiMemoryMode;

// =====================================================================================================
// THE MEMORY MODE of a compiled program - which pointer model it is built for (owner, 15 Sep 2026).
//
//   fb      FreeBASIC-compatible (the default): a pointer is the machine address, over native memory with
//           C layout, unsafe included - what a program written for fbc expects when C keeps, returns or
//           follows its pointers.
//   strict  isolated from the VM: a pointer is a name of the VM, translated at the C boundary for one call
//           (the model every build had until this unit existed).
//
// ⭐ THE MODE BELONGS TO THE COMPILATION, not only to the run: the SSA generator chooses how a pointer is
// represented, so `sb prog.bas` resolves it before generating and a `.basc` carries it in its header.
//
// ⭐ ONE place resolves it, in one order - the most specific wins:
//   1. the command line   --memory=fb | --memory=strict
//   2. sedai.conf         memory = fb | memory = strict   (the same five places as every other key)
//   3. the default        fb - or strict on a build locked with {$DEFINE SEDAI_STRICT_ONLY}
//
// ⛔ A LOCKED BUILD REFUSES, ALOUD. `./build.sh sb --strict-only` exists for contexts where security
// matters: there `--memory=fb`, a sedai.conf asking for fb and a `.basc` compiled for fb are refused with a
// message that names the lock. Accepting and ignoring would be the WITH_WINDOW trap this project has paid
// for already: a flag that did nothing and said nothing.
//
// Phase 0 of job/markdown/MODELLO-PUNTATORI.md: the mode is chosen, carried and checked; both modes still
// BEHAVE the same. MEMMODE_DIAG=1 prints the resolved mode and where it came from.
// =====================================================================================================

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TMemoryMode = (mmFB, mmStrict);

const
  MEMORY_MODE_FLAG = '--memory=';
  MEMORY_MODE_KEY  = 'MEMORY';

var
  GMemoryModeFlag: string = '';          // what followed "--memory=" on the command line ('' = not given)
  GMemoryMode: TMemoryMode = mmFB;       // the mode of the program being compiled or run, once resolved

{ 'fb' / 'strict' }
function MemoryModeName(M: TMemoryMode): string;

{ Case-insensitive; False for anything else. }
function ParseMemoryMode(const S: string; out M: TMemoryMode): Boolean;

{ True on a build compiled with SEDAI_STRICT_ONLY. }
function MemoryModeLocked: Boolean;

{ The mode for a program about to be COMPILED. AFlagValue is what followed "--memory=" (empty when the flag
  was not given). Call it after LoadConfig. False, with AError set, when a value is not a mode or a locked
  build is asked for fb. ASource names where the answer came from. }
function ResolveMemoryMode(const AFlagValue: string; out M: TMemoryMode; out ASource, AError: string): Boolean;

{ The mode for a program that was compiled ALREADY (a .basc): the file decides. False, with AError set, when
  the command line asked explicitly for the other mode, or a locked build meets a file compiled for fb. }
function CheckCompiledMemoryMode(AFileMode: TMemoryMode; const AFlagValue: string; out AError: string): Boolean;

{ MEMMODE_DIAG=1: one line on stderr. }
procedure NoteMemoryMode(M: TMemoryMode; const ASource: string);

{ For a front end about to compile: resolve into GMemoryMode from GMemoryModeFlag and the configuration.
  On a refusal it prints "ERROR: ..." on stderr and answers False; the caller sets its exit code. }
function ResolveSourceMemoryMode: Boolean;

{ For a front end that loaded a compiled program: adopt the mode its header carries into GMemoryMode, or raise
  EMemoryModeError naming why it cannot run here. }
procedure AdoptCompiledMemoryMode(ANativeMemory: Boolean);

type
  EMemoryModeError = class(Exception);

implementation

uses
  SedaiConfig;

function MemoryModeName(M: TMemoryMode): string;
begin
  if M = mmFB then Result := 'fb' else Result := 'strict';
end;

function ParseMemoryMode(const S: string; out M: TMemoryMode): Boolean;
begin
  Result := True;
  M := mmFB;
  if SameText(Trim(S), 'fb') then M := mmFB
  else if SameText(Trim(S), 'strict') then M := mmStrict
  else Result := False;
end;

function MemoryModeLocked: Boolean;
begin
  {$IFDEF SEDAI_STRICT_ONLY}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function ResolveMemoryMode(const AFlagValue: string; out M: TMemoryMode; out ASource, AError: string): Boolean;
var
  V: string;
begin
  AError := '';
  if MemoryModeLocked then M := mmStrict else M := mmFB;
  ASource := 'default';
  if AFlagValue <> '' then
  begin
    if not ParseMemoryMode(AFlagValue, M) then
    begin
      AError := Format('--memory=%s: the memory mode is "fb" or "strict"', [AFlagValue]);
      Exit(False);
    end;
    ASource := 'command line';
  end
  else
  begin
    V := ConfigValue(MEMORY_MODE_KEY, '');
    if V <> '' then
    begin
      if not ParseMemoryMode(V, M) then
      begin
        AError := Format('sedai.conf: memory = %s: the memory mode is "fb" or "strict" (read from %s)',
                         [V, ConfigSources]);
        Exit(False);
      end;
      ASource := 'sedai.conf';
    end;
  end;
  if MemoryModeLocked and (M = mmFB) then
  begin
    AError := Format('memory mode "fb" (from the %s) refused: this build is locked to STRICT (SEDAI_STRICT_ONLY)',
                     [ASource]);
    Exit(False);
  end;
  Result := True;
end;

function CheckCompiledMemoryMode(AFileMode: TMemoryMode; const AFlagValue: string; out AError: string): Boolean;
var
  Asked: TMemoryMode;
begin
  AError := '';
  if MemoryModeLocked and (AFileMode = mmFB) then
  begin
    AError := 'this program was compiled for memory mode "fb", and this build is locked to STRICT (SEDAI_STRICT_ONLY)';
    Exit(False);
  end;
  if AFlagValue <> '' then
  begin
    if not ParseMemoryMode(AFlagValue, Asked) then
    begin
      AError := Format('--memory=%s: the memory mode is "fb" or "strict"', [AFlagValue]);
      Exit(False);
    end;
    // ⛔ The pointer model is decided when the program is COMPILED, so a .basc cannot be switched at run:
    // its instructions and its C call entries were written for one model. Say which, and how to get the other.
    if Asked <> AFileMode then
    begin
      AError := Format('--memory=%s: this program was compiled for memory mode "%s"; recompile it with sbc --memory=%s',
                       [MemoryModeName(Asked), MemoryModeName(AFileMode), MemoryModeName(Asked)]);
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure NoteMemoryMode(M: TMemoryMode; const ASource: string);
begin
  if GetEnvironmentVariable('MEMMODE_DIAG') = '1' then
    WriteLn(ErrOutput, 'MEMMODE: ', MemoryModeName(M), ' (', ASource, ')');
end;

function ResolveSourceMemoryMode: Boolean;
var
  Src, Err: string;
begin
  Result := ResolveMemoryMode(GMemoryModeFlag, GMemoryMode, Src, Err);
  if Result then NoteMemoryMode(GMemoryMode, Src)
  else WriteLn(ErrOutput, 'ERROR: ', Err);
end;

procedure AdoptCompiledMemoryMode(ANativeMemory: Boolean);
var
  FileMode: TMemoryMode;
  Err: string;
begin
  if ANativeMemory then FileMode := mmFB else FileMode := mmStrict;
  if not CheckCompiledMemoryMode(FileMode, GMemoryModeFlag, Err) then
    raise EMemoryModeError.Create(Err);
  GMemoryMode := FileMode;
  NoteMemoryMode(FileMode, '.basc header');
end;

end.
