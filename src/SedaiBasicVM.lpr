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
program SedaiBasicVM;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

// Include shared optimization flags
{$I OptimizationFlags.inc}
// JIT feature flags (hot-loop profiling opt-in)
{$I JitFlags.inc}
// Include debug flags (compile-time control of debug code)
{$I DebugFlags.inc}
// Include profiler flags
{$I ProfilerFlags.inc}

uses
  // M5.2 threading: on Unix the cthreads unit MUST be first so BeginThread uses pthreads.
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Variants, TypInfo, Math,
  // Lexer/Parser
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser, SedaiDateTimeUtils,
  // Bytecode VM
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeTypes, SedaiBytecodeCompiler, SedaiBytecodeVM,
  SedaiBytecodeDisassembler, SedaiOpcodeTable, SedaiJit, SedaiAot,
  // Headless file I/O handler (OPEN/PRINT#/INPUT#/EOF/FREEFILE...) for the CLI VM
  SedaiFileIO,
  // Register Allocation
  SedaiRegAlloc,
  // Peephole and Superinstructions
  SedaiPeephole, SedaiSuperinstructions,
  // NOP Compaction (removes NOPs after superinstruction fusion)
  SedaiNopCompaction,
  // Register Compaction
  SedaiRegisterCompaction,
  // Debug runtime flags
  SedaiDebug,
  // Profiler
  {$IFDEF ENABLE_PROFILER}SedaiProfiler,{$ENDIF}
  // Executor Context
  SedaiExecutorContext, SedaiExecutorTypes, SedaiOutputInterface,
  // I/O Manager
  SedaiIOManager, SedaiTerminalIO, SedaiConsoleState,
  // Optional SDL2 window presenter for `sb --window` (WITH_WINDOW build only; no SDL2 dependency otherwise)
  {$IFDEF WITH_WINDOW}SedaiGraphicsBackend, SedaiWindowPresenter,{$ENDIF}
  // Runner and Serializer (for .basc support)
  SedaiRunner, SedaiBytecodeSerializer, SedaiPreprocessor;

// Include version information (must be after uses, contains const declarations)
{$I Version.inc}

// Console I/O is now in SedaiTerminalIO unit
// TTerminalController and TTerminalInput replace the old TConsoleOutput/TConsoleInput

// === BUILT-IN FUNCTIONS ===

type
  TBuiltinFunctions = class
    function BuiltinSqr(const Args: array of Variant): Variant;
    function BuiltinAbs(const Args: array of Variant): Variant;
    function BuiltinInt(const Args: array of Variant): Variant;
    // String functions
    function BuiltinLen(const Args: array of Variant): Variant;
    function BuiltinLeft(const Args: array of Variant): Variant;
    function BuiltinMid(const Args: array of Variant): Variant;
    function BuiltinRight(const Args: array of Variant): Variant;
    function BuiltinAsc(const Args: array of Variant): Variant;
    function BuiltinChr(const Args: array of Variant): Variant;
    function BuiltinStr(const Args: array of Variant): Variant;
    function BuiltinVal(const Args: array of Variant): Variant;
  end;

function TBuiltinFunctions.BuiltinSqr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('SQR requires exactly one argument');
  Result := Sqrt(Double(Args[0]));
end;

function TBuiltinFunctions.BuiltinAbs(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('ABS requires exactly one argument');
  Result := Abs(Double(Args[0]));
end;

function TBuiltinFunctions.BuiltinInt(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('INT requires exactly one argument');
  Result := Int(Double(Args[0]));
end;

// === STRING FUNCTIONS ===

function TBuiltinFunctions.BuiltinLen(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('LEN requires exactly one argument');
  Result := Length(VarToStr(Args[0]));
end;

function TBuiltinFunctions.BuiltinLeft(const Args: array of Variant): Variant;
var
  S: string;
  N: Integer;
begin
  if Length(Args) <> 2 then
    raise Exception.Create('LEFT$ requires two arguments');
  S := VarToStr(Args[0]);
  N := Integer(Args[1]);
  if N < 0 then N := 0;
  if N > Length(S) then N := Length(S);
  Result := Copy(S, 1, N);
end;

function TBuiltinFunctions.BuiltinMid(const Args: array of Variant): Variant;
var
  S: string;
  Start, Len: Integer;
begin
  if Length(Args) < 2 then
    raise Exception.Create('MID$ requires at least two arguments');
  S := VarToStr(Args[0]);
  Start := Integer(Args[1]);
  if Length(Args) >= 3 then
    Len := Integer(Args[2])
  else
    Len := Length(S);  // Default: to end of string
  Result := Copy(S, Start, Len);
end;

function TBuiltinFunctions.BuiltinRight(const Args: array of Variant): Variant;
var
  S: string;
  N: Integer;
begin
  if Length(Args) <> 2 then
    raise Exception.Create('RIGHT$ requires two arguments');
  S := VarToStr(Args[0]);
  N := Integer(Args[1]);
  if N < 0 then N := 0;
  if N > Length(S) then N := Length(S);
  Result := Copy(S, Length(S) - N + 1, N);
end;

function TBuiltinFunctions.BuiltinAsc(const Args: array of Variant): Variant;
var
  S: string;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('ASC requires exactly one argument');
  S := VarToStr(Args[0]);
  if Length(S) > 0 then
    Result := Ord(S[1])
  else
    Result := 0;
end;

function TBuiltinFunctions.BuiltinChr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('CHR$ requires exactly one argument');
  Result := Chr(Integer(Args[0]) and $FF);
end;

function TBuiltinFunctions.BuiltinStr(const Args: array of Variant): Variant;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('STR$ requires exactly one argument');
  if VarIsFloat(Args[0]) then
    Result := FloatToStr(Double(Args[0]))
  else
    Result := IntToStr(Integer(Args[0]));
end;

function TBuiltinFunctions.BuiltinVal(const Args: array of Variant): Variant;
var
  S: string;
  FloatVal: Double;
begin
  if Length(Args) <> 1 then
    raise Exception.Create('VAL requires exactly one argument');
  S := Trim(VarToStr(Args[0]));
  try
    if Pos('.', S) > 0 then
    begin
      FloatVal := StrToFloat(S);
      Result := FloatVal;
    end
    else
      Result := StrToInt(S);
  except
    Result := 0;  // Return 0 on error
  end;
end;

procedure RegisterBuiltinFunctions(Context: TExecutorContext; Functions: TBuiltinFunctions);
begin
  // Math functions
  Context.RegisterFunction('SQR', @Functions.BuiltinSqr);
  Context.RegisterFunction('ABS', @Functions.BuiltinAbs);
  Context.RegisterFunction('INT', @Functions.BuiltinInt);

  // String functions
  Context.RegisterFunction('LEN', @Functions.BuiltinLen);
  Context.RegisterFunction('LEFT$', @Functions.BuiltinLeft);
  Context.RegisterFunction('MID$', @Functions.BuiltinMid);
  Context.RegisterFunction('RIGHT$', @Functions.BuiltinRight);
  Context.RegisterFunction('ASC', @Functions.BuiltinAsc);
  Context.RegisterFunction('CHR$', @Functions.BuiltinChr);
  Context.RegisterFunction('STR$', @Functions.BuiltinStr);
  Context.RegisterFunction('VAL', @Functions.BuiltinVal);
end;

procedure PrintASTTree(Node: TASTNode; Indent: Integer = 0; TokenList: TTokenList = nil);
var
 i: Integer;
 Prefix: string;
 NodeInfo: string;
begin
 if not Assigned(Node) then Exit;

 Prefix := StringOfChar(' ', Indent * 2);

 // Format node information with type and value
 if not VarIsEmpty(Node.Value) then
   NodeInfo := Format('%s: "%s"', [
     GetEnumName(TypeInfo(TASTNodeType), Ord(Node.NodeType)),
     VarToStr(Node.Value)
   ])
 else
   NodeInfo := GetEnumName(TypeInfo(TASTNodeType), Ord(Node.NodeType));

 WriteLn(Format('%s%s', [Prefix, NodeInfo]));

 // Print all children recursively
 for i := 0 to Node.ChildCount - 1 do
   PrintASTTree(Node.Child[i], Indent + 1, TokenList);
end;

{ Format time: always ms with 3 decimals, plus human-readable conversion in parentheses
  < 1ms:        ms (with µs or ns conversion)
  1-60000ms:    ms only (no parentheses needed)
  > 60000ms:    ms (with hh:mm:ss.nnn conversion)
}
function FormatTimeEx(TimeMs: Double): string;
var
  TimeUs, TimeNs: Double;
  Seconds: Double;
  Hours, Minutes, Secs, Millis: Integer;
begin
  if TimeMs < 0.001 then
  begin
    // Less than 1µs -> show ns conversion
    TimeNs := TimeMs * 1000000.0;
    Result := Format('%.3f ms (%.0f ns)', [TimeMs, TimeNs]);
  end
  else if TimeMs < 1.0 then
  begin
    // 1µs to 1ms -> show µs conversion
    TimeUs := TimeMs * 1000.0;
    Result := Format('%.3f ms (%.2f µs)', [TimeMs, TimeUs]);
  end
  else if TimeMs < 60000.0 then
  begin
    // 1ms to 60s -> milliseconds only (no conversion needed)
    Result := Format('%.3f ms', [TimeMs]);
  end
  else
  begin
    // > 60s -> show hh:mm:ss.nnn conversion
    Seconds := TimeMs / 1000.0;
    Hours := Trunc(Seconds) div 3600;
    Minutes := (Trunc(Seconds) mod 3600) div 60;
    Secs := Trunc(Seconds) mod 60;
    Millis := Round((Seconds - Trunc(Seconds)) * 1000);
    Result := Format('%.3f ms (%s:%s:%s.%s)',
      [TimeMs,
       FormatFloat('00', Hours),
       FormatFloat('00', Minutes),
       FormatFloat('00', Secs),
       FormatFloat('000', Millis)]);
  end;
end;

{ Format time with auto-scaling (ms -> µs -> ns) - legacy function }
function FormatTime(TimeMs: Double): string;
var
  TimeUs, TimeNs: Double;
begin
  if TimeMs >= 1.0 then
    Result := Format('%.2f ms', [TimeMs])
  else
  begin
    TimeUs := TimeMs * 1000.0;  // Convert to microseconds
    if TimeUs >= 1.0 then
      Result := Format('%.2f µs', [TimeUs])
    else
    begin
      TimeNs := TimeUs * 1000.0;  // Convert to nanoseconds
      Result := Format('%.2f ns', [TimeNs]);
    end;
  end;
end;

{ Get system architecture string }
function GetSystemArchitecture: string;
begin
  {$IFDEF CPUX86_64}
    {$IFDEF WINDOWS}
    Result := 'x86_64-win64';
    {$ENDIF}
    {$IFDEF LINUX}
    Result := 'x86_64-linux';
    {$ENDIF}
    {$IFDEF DARWIN}
    Result := 'x86_64-darwin';
    {$ENDIF}
  {$ELSE}
    {$IFDEF CPUI386}
      {$IFDEF WINDOWS}
      Result := 'i386-win32';
      {$ENDIF}
      {$IFDEF LINUX}
      Result := 'i386-linux';
      {$ENDIF}
    {$ELSE}
      {$IFDEF CPUAARCH64}
      Result := 'aarch64';
      {$ELSE}
      Result := 'unknown';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

var
  // Global option for TRUE value in comparisons (-1 = Commodore BASIC, 1 = modern BASIC)
  OptTrueValue: Int64 = -1;
  // --bounds-check: force array bounds checking on in every dialect (default off; MODERN follows FreeBASIC
  // and skips it, CLASSIC always checks). A debugging aid analogous to FreeBASIC's -exx.
  OptBoundsCheck: Boolean = False;
  // --jit: compile eligible hot loops to native code (JIT J2/J3).
  OptJit: Boolean = False;
  OptAot: Boolean = False;
  {$IFDEF JIT_PROFILE}
  // --jit-profile: enable JIT hot-loop back-edge profiling and dump the hot loops after the run (J1).
  OptJitProfile: Boolean = False;
  {$ENDIF}
  // Headless file-I/O handler shared by the CLI VM instances (lazily created, freed at exit).
  GFileHandler: TVMFileHandler = nil;

{ Attach the headless file handler's callbacks to a freshly created VM, so OPEN /
  PRINT# / INPUT# / EOF / FREEFILE perform real file I/O in the CLI. }
procedure WireFileHandler(AVM: TBytecodeVM);
begin
  if GFileHandler = nil then GFileHandler := TVMFileHandler.Create;
  AVM.OnDiskFile := @GFileHandler.DiskFile;
  AVM.OnFileData := @GFileHandler.FileData;
  // Numeric fast path for EOF/LOF/LOC/SEEK/FREEFILE. FILEQUERY=0 leaves it unwired, so the queries
  // go back through the string protocol and the two can be compared - for equality and for time -
  // on ONE binary.
  if GetEnvironmentVariable('FILEQUERY') <> '0' then
    AVM.OnFileQuery := @GFileHandler.FileQuery;
end;

{ Keyword coverage self-check (--kw-check <list>).

  For every name in the list, report what THIS build's front end makes of it. The verdict comes from
  the real lexer, not from a table kept by hand: a name the lexer does not know comes back as
  ttIdentifier, and anything else is a keyword it recognises. That is the whole point -- an inventory
  maintained separately from the compiler drifts the moment a keyword is added, and BASIC.md's table
  had drifted by 22 entries before this existed.

  Two things it deliberately does NOT claim:
    - "recognised" is not "implemented". A keyword accepted and ignored on purpose (EXPORT, the build
      directives) is recognised, and says so; whether the SEMANTICS are right is what the manual's own
      examples answer, which is the companion half of kwcheck.ps1.
    - a MULTI-WORD entry from the manual's index ("Select Case", "Print #", "For...Next") is reported
      per LEADING word, since that is the token the lexer decides on.

  Lines may carry a "# comment" tail and blank lines are skipped, so the manual's extracted index can
  be fed in as-is. Output is one TAB-separated record per name, for a script to aggregate. }
procedure KwCheckReport(const ListFile: string);
var
  Names: TStringList;
  Lexer: TLexerFSM;
  Toks: TTokenList;
  i, HashPos, Known, Unknown: Integer;
  Nm, Lead, Verdict, Expanded: string;
begin
  if not FileExists(ListFile) then
  begin
    WriteLn(ErrOutput, 'kw-check: list file not found: ', ListFile);
    Halt(2);
  end;
  Names := TStringList.Create;
  try
    Names.LoadFromFile(ListFile);
    Known := 0;
    Unknown := 0;
    for i := 0 to Names.Count - 1 do
    begin
      Nm := Names[i];
      HashPos := Pos('#', Nm);
      // A leading '#' is a PREPROCESSOR directive and part of the name; only a later one is a comment.
      if HashPos > 1 then Nm := Copy(Nm, 1, HashPos - 1);
      Nm := Trim(Nm);
      if Nm = '' then Continue;
      // Decide on the LEADING word: "Select Case" is two tokens, and it is the first that tells the
      // lexer a statement is starting.
      Lead := Nm;
      if Pos(' ', Lead) > 0 then Lead := Copy(Lead, 1, Pos(' ', Lead) - 1);
      if Pos('.', Lead) > 0 then Lead := Copy(Lead, 1, Pos('.', Lead) - 1);   // "For...Next"
      if Lead = '' then Continue;

      Verdict := 'UNKNOWN';
      Lexer := TLexerFSM.Create;
      try
        Lexer.SetHasLineNumbers(False);          // MODERN: this inventory is the FreeBASIC keyword set
        Lexer.SetCaseSensitive(False);
        Lexer.Source := Lead;
        Toks := Lexer.ScanAllTokensFast;
        if Assigned(Toks) and (Toks.Count > 0) then
        begin
          if Toks[0].TokenType = ttIdentifier then
            Verdict := 'UNKNOWN'
          else
            Verdict := GetEnumName(TypeInfo(TTokenType), Ord(Toks[0].TokenType));
        end;
      finally
        Lexer.Free;
      end;
      // The lexer is not the whole front end: the __FB_* family are PREPROCESSOR macros and never
      // reach it as keywords at all. Asking only the lexer reported every one of them unknown -- 90
      // false gaps, which is what made the first version of this report unusable. A macro answers by
      // EXPANDING: preprocess the bare name and see whether it comes back as something else.
      if Verdict = 'UNKNOWN' then
      begin
        Expanded := PreprocessSource(Lead, '');
        if Trim(Expanded) <> Lead then Verdict := 'ppMacro'
        else
        begin
          Expanded := PreprocessSource(Lead + '(a,b)', '');    // function-like macro
          if Trim(Expanded) <> Lead + '(a,b)' then Verdict := 'ppMacro';
        end;
      end;
      if Verdict = 'UNKNOWN' then Inc(Unknown) else Inc(Known);
      WriteLn(Nm, #9, Verdict);
    end;
    WriteLn(ErrOutput, Format('kw-check: %d names, %d recognised, %d unknown',
                              [Known + Unknown, Known, Unknown]));
  finally
    Names.Free;
  end;
end;

{ Print version banner }
procedure PrintVersion;
begin
  WriteLn('SedaiBasic ver. ', SEDAIBASIC_VERSION, ' [', SEDAIBASIC_RELEASE_DATE, '] for ', GetSystemArchitecture);
  WriteLn(SEDAIBASIC_COPYRIGHT);
  WriteLn(SEDAIBASIC_LICENSE);
end;

{ Print help information }
procedure PrintHelp;
begin
  PrintVersion;
  WriteLn;
  WriteLn('Usage: sb <source.bas|program.basc> [options]');
  WriteLn;
  WriteLn('Supported file types:');
  WriteLn('  .bas       BASIC source code (compiled at runtime)');
  WriteLn('  .basc      Pre-compiled bytecode (faster startup)');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --help              Show this help message');
  WriteLn('  --verbose           Show loading, lexing, parsing, and VM execution info');
  WriteLn('  --dump-ast          Show AST structure after parsing');
  WriteLn('  --disasm            Show bytecode disassembly (after superinstructions)');
  WriteLn('  --disasm-pre        Show bytecode BEFORE superinstruction fusion');
  WriteLn('  --no-exec           Compile only, do not execute (useful with --disasm)');
  WriteLn('  --no-opt            Skip the SSA/bytecode optimization passes (differential testing)');
  WriteLn('  --bounds-check      Hard-error on out-of-bounds array access (MODERN too; default follows dialect)');
  WriteLn('  --date-locale       Month/day names and date parsing follow the SYSTEM locale (as fbc does).');
  WriteLn('                        Default is deterministic: English names, ISO-ish dates, same everywhere.');
  WriteLn('                        Also settable with SB_DATE_LOCALE=1; the flag wins over it.');
  WriteLn('  --stats             Show execution statistics');
  WriteLn('  --true-value=N      Set TRUE value for comparisons (-1 or 1, default: -1)');
  WriteLn('                        -1 = Commodore BASIC style (default)');
  WriteLn('                         1 = Modern BASIC style');
  WriteLn;
  {$IFDEF ANY_DEBUG_ENABLED}
  WriteLn('Debug options (compile-time flags enabled in DebugFlags.inc):');
  WriteLn('  --debug-all         Enable ALL debug output');
  WriteLn('  --debug-ssa         SSA construction debug');
  WriteLn('  --debug-gvn         Global Value Numbering debug');
  WriteLn('  --debug-cse         Common Subexpression Elimination debug');
  WriteLn('  --debug-dce         Dead Code Elimination debug');
  WriteLn('  --debug-licm        Loop-Invariant Code Motion debug');
  WriteLn('  --debug-algebraic   Algebraic Simplification debug');
  WriteLn('  --debug-strength    Strength Reduction debug');
  WriteLn('  --debug-constprop   Constant Propagation debug');
  WriteLn('  --debug-copyprop    Copy Propagation debug');
  WriteLn('  --debug-copycoal    Copy Coalescing debug');
  WriteLn('  --debug-phielim     PHI Elimination debug');
  WriteLn('  --debug-regalloc    Register Allocation debug');
  WriteLn('  --debug-peephole    Peephole Optimization debug');
  WriteLn('  --debug-superinstr  Superinstruction Fusion debug');
  WriteLn('  --debug-domtree     Dominator Tree debug');
  WriteLn('  --debug-dbe         Dead Block Elimination debug');
  WriteLn('  --debug-bytecode    Bytecode Compilation debug');
  WriteLn('  --debug-vm          Virtual Machine execution debug');
  WriteLn('  --debug-cleanup     Memory cleanup/destructor debug');
  WriteLn;
  {$ELSE}
  WriteLn('Debug options disabled (see DebugFlags.inc for details).');
  WriteLn;
  {$ENDIF}
  {$IFDEF ENABLE_PROFILER}
  WriteLn('Profiler options (compile-time flag enabled in ProfilerFlags.inc):');
  WriteLn('  --profile[=MODE]    Enable profiler. MODE can be:');
  WriteLn('                        sampling        Sample-based profiling, low overhead (default)');
  WriteLn('                        hybrid          Sampling + selective instrumentation');
  WriteLn('                        instrumentation Full instrumentation, accurate but slow');
  WriteLn('  --profile-export=FILE Export profile data (format by extension: .json, .csv, .folded)');
  WriteLn;
  {$ELSE}
  WriteLn('Profiler options disabled (see ProfilerFlags.inc for details).');
  WriteLn;
  {$ENDIF}
  WriteLn('Examples:');
  WriteLn('  sb program.bas              Run program (output only)');
  WriteLn('  sb program.bas arg1 arg2    Run, passing arg1/arg2 to the program (COMMAND$)');
  WriteLn('  sb program.bas --verbose    Run with verbose output');
  WriteLn('  sb program.bas --stats      Run with execution statistics');
  WriteLn('  sb program.bas --disasm     Show disassembly and run');
  {$IFDEF ENABLE_PROFILER}
  WriteLn('  sb program.bas --profile    Run with sampling profiler (default)');
  WriteLn('  sb program.bas --profile=instrumentation');
  WriteLn('                                          Run with full instrumentation');
  WriteLn('  sb program.bas --profile --profile-export=out.json');
  WriteLn('                                          Profile and export to JSON');
  {$ENDIF}
  WriteLn('  sb --help                   Show this help');
end;

var
  OptWindow: Boolean = False;   // sb --window: mirror the software framebuffer into an SDL2 window
  GTermCtrl: TTerminalController = nil;   // concrete terminal output device (for graphics attach under --window)
  GProgramArgs: array of string;   // COMMAND$: everything on the command line after the script file
{$IFDEF WITH_WINDOW}
var
  GPresenter: TWindowPresenter = nil;
{$ENDIF}

// Graphics setup shared by both run paths: a window presenter (sb --window, WITH_WINDOW build) or the
// plain headless software backend (default — the regression target never opens a window).
procedure SetupVMGraphics(AVM: TBytecodeVM);
{$IFDEF WITH_WINDOW}
var SW: TSoftwareGraphicsBackend;
{$ENDIF}
begin
  {$IFDEF WITH_WINDOW}
  if OptWindow then
  begin
    SW := TSoftwareGraphicsBackend.Create;
    AVM.SetGraphicsBackend(SW, SW);
    // Share the backend's framebuffer with the terminal output device so the C128 (BASIC v7) graphics
    // commands (GRAPHIC/BOX/CIRCLE/COLOR/SETCOLOR/PAINT...) render into the same surface the window
    // shows — not just the FreeBASIC graphics. Viewport-only (no text-on-graphics / sprites).
    if Assigned(GTermCtrl) then GTermCtrl.AttachGraphicsMemory(SW.ScreenMemory);
    // ...and the backend itself, so PRINT inside a FreeBASIC graphics mode lands on that same surface.
    if Assigned(GTermCtrl) then GTermCtrl.AttachGraphicsBackend(SW);
    GPresenter := TWindowPresenter.Create(SW, 'SedaiBasic');
    // ⛔ THE SPLIT: the POLL drains events (cheap, on an instruction counter), the PRESENT shows the
    // picture (expensive, at a frame boundary). Wiring Pump to the poll made the instruction counter
    // decide the frame rate - 158 presents per frame on a compute-heavy program.
    AVM.EventPollCallback := @GPresenter.PollEvents;
    AVM.PresentCallback := @GPresenter.Pump;
    // ...but the dispatch loop only reaches that callback at blocking points, so a graphics
    // program with no SLEEP in its loop would never present. Ask the VM for a wall-clock cadence
    // from the graphics opcodes as well. 16 ms = about 60 presents a second. This property is
    // deliberately left at 0 everywhere else (sbv, sbw, headless sb), which is what keeps this
    // from interfering with SedaiVision's own rendering.
    AVM.PresentCadenceMs := 16;
    Exit;
  end;
  {$ENDIF}
  AVM.UseSoftwareGraphics;
  // The headless default path. In FreeBASIC a graphics mode has NO separate text plane - the console IS
  // the framebuffer - so the text device has to be told which surface to mirror PRINT onto. Without
  // this, every gfx example that only PRINTS left the screen blank, and nothing said so.
  // Wired HERE, where both sides are concrete: an IOutputDevice cannot be cast back to its class under
  // CORBA interfaces, which is why AttachGraphicsMemory above is done the same way.
  if Assigned(GTermCtrl) then GTermCtrl.AttachGraphicsBackend(AVM.GraphicsBackend);
end;

// After the program ends: keep the window open until the user closes it, then tear it down.
procedure FinishVMGraphics(AVM: TBytecodeVM);
begin
  {$IFDEF WITH_WINDOW}
  if Assigned(GPresenter) then
  begin
    AVM.EventPollCallback := nil;
    GPresenter.ReportPumpCalls;
    // FreeBASIC convention: the program terminates at END and the window closes with it. To keep the
    // window visible after drawing, the program waits (SLEEP / a GETKEY loop) — those keep the event
    // pump alive and presenting. We do NOT block here, so `END` actually ends the program.
    FreeAndNil(GPresenter);
  end;
  {$ENDIF}
end;

procedure TestBytecodeCompilation(const SourceFile: string;
  OptVerbose, OptDumpAST, OptDisasm, OptDisasmPre, OptStats, OptNoExec: Boolean
  {$IFDEF ENABLE_PROFILER}; OptProfile: Boolean; ProfileMode: string; ProfileExport: string{$ENDIF});
var
  Source: TStringList;
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  BytecodeProgram: TBytecodeProgram;
  VM: TBytecodeVM;
  Disassembler: TBytecodeDisassembler;
  Output: IOutputDevice;
  Input: IInputDevice;
  Timer: THiResTimer;
  SSATime, OptTime, CompileTime, ExecuteTime: Double;
  // Per-pass timing (when OptStats is enabled)
  TimeDBE, TimeDomTree, TimeSSAConstr, TimeGVN, TimeCSE, TimeAlgebraic: Double;
  TimeStrength, TimeGosubInline, TimeConstProp, TimeCopyProp, TimeLICM: Double;
  TimeLoopUnroll, TimeDCE, TimePhiElim, TimeCopyCoal, TimeRegAlloc: Double;
  PassTimer: THiResTimer;
  // Per-pass EFFECT (when OptStats is enabled), collected by PassMark below. The timings alone
  // cannot tell a pass that is working from one that fires on nothing - both cost a fraction of a
  // millisecond and look the same in the breakdown. This records whether the SSA program actually
  // changed, and by how many instructions.
  PassEffect: TStringList;
  PassPrevHash: QWord;
  PassPrevCount: Integer;
  i, removed: Integer;
  ErrorSourceLine: Integer;
  QBLangDetected: Boolean;
  ShowBanners: Boolean;
  AotFuncList: TAotFuncs;
  AotI: Integer;
  AotRecordsOff, AotRecSize, AotRecIntOff, AotRecFloatOff, AotSharedRecOff: Integer;
  {$IFDEF ENABLE_PROFILER}
  Profiler: TProfiler;
  ProfMode: TProfilerMode;
  ExportExt: string;
  {$ENDIF}
  {$IFNDEF DISABLE_REG_ALLOC}
  RegAlloc: TLinearScanAllocator;
  SpillCount: Integer;
  {$ENDIF}

  // Record what a pass actually DID, by comparing the SSA program's content fingerprint before and
  // after it. Called right after each pass's timing line, so the two answer different questions:
  // the timer says what the pass cost, this says whether it earned it. A pass reported as "inert"
  // rewrote nothing at all on this program - which is not automatically a defect (many passes
  // legitimately find no opportunity in a given program), but a pass inert across the whole corpus
  // is the signature of the GVN bug found on 2026-07-25: sound, timed, and doing nothing.
  procedure PassMark(const AName: string);
  var
    H: QWord;
    Cnt, Delta: Integer;
  begin
    if not OptStats then Exit;
    if not Assigned(SSAProgram) then Exit;
    H := SSAProgram.Fingerprint(Cnt);
    Delta := Cnt - PassPrevCount;
    if H = PassPrevHash then
      PassEffect.Add(Format('    %-14s inert     (%5d instrs)', [AName + ':', Cnt]))
    else
      // FPC's Format has no '+' flag - the sign goes in by hand.
      PassEffect.Add(Format('    %-14s CHANGED   (%5d instrs, %s%d)',
        [AName + ':', Cnt, Copy('+', 1, Ord(Delta >= 0)), Delta]));
    PassPrevHash := H;
    PassPrevCount := Cnt;
  end;

begin
  // Determine if we should show banners (any option enabled)
  ShowBanners := OptVerbose or OptDumpAST or OptDisasm or OptStats or AnyDebugActive;
  PassEffect := nil;   // several early-exit paths run before it is created

  if ShowBanners then
  begin
    WriteLn('========================================');
    PrintVersion;
    WriteLn('========================================');
    WriteLn;

    // Show optimization status only if any debug target is active
    if AnyDebugActive then
    begin
      {$IFDEF DISABLE_ALL_OPTIMIZATIONS}
      WriteLn('>>> OPTIMIZATION MODE: DISABLED <<<');
      WriteLn('    All optimization passes are turned off.');
      WriteLn('    Running baseline SSA → Compiler → VM path.');
      {$ELSE}
      WriteLn('>>> OPTIMIZATION MODE: ENABLED <<<');
      WriteLn('    All optimization passes are active.');
      {$ENDIF}
      WriteLn;
    end;
  end;

  // Load source
  if OptVerbose then
    WriteLn('Loading source: ', SourceFile);
  Source := TStringList.Create;
  try
    if FileExists(SourceFile) then
      Source.LoadFromFile(SourceFile)
    else
    begin
      WriteLn('ERROR: File not found: ', SourceFile);
      Exit;
    end;

    if OptVerbose then
      WriteLn('Source loaded (', Source.Count, ' lines)');

    // Pre-filter: remove Markdown fence lines (``` or ```vb) that some test files include
    // We don't modify test files on disk; this only cleans the in-memory source before lexing.
    removed := 0;
    for i := Source.Count - 1 downto 0 do
    begin
      if Pos('```', Trim(Source[i])) = 1 then
      begin
        Source.Delete(i);
        Inc(removed);
      end;
    end;
    if OptVerbose and (removed > 0) then
      WriteLn(Format('Pre-filter: removed %d fence line(s) from source (non-destructive).', [removed]));

    // === PREPROCESSOR === (FreeBASIC #define/#undef/#ifdef/#ifndef/#else/#endif/#include).
    // Pure text->text pass before lexing; #include paths resolve relative to the source file.
    try
      // -lang qb ('#lang "qb"' / the '$lang: "qb" metacommand) takes QB PRINT number spacing
      // (FB zone width + TRAILING space after numerics, fbc-verified; fblite does NOT). Detected
      // on the RAW text: the preprocessor strips both directive forms before the old site ran.
      QBLangDetected := DetectQBLang(Source.Text);
      Source.Text := PreprocessSource(Source.Text, ExtractFilePath(ExpandFileName(SourceFile)), SourceFile);
    except
      on E: EPreprocessorError do
      begin
        WriteLn('ERROR: ', E.Message);
        Exit;
      end;
    end;

    if OptVerbose then
      WriteLn;

    // === LEXING ===
    if OptVerbose then
      WriteLn('=== LEXING ===');
    Lexer := TLexerFSM.Create;
    try
      // Dialect auto-selected at LOAD by content: a program that uses line numbers
      // is classic; otherwise FreeBASIC/Modern (no line numbers).
      Lexer.SetHasLineNumbers(TSedaiRunner.SourceHasLineNumbers(Source.Text));
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;
      Lexer.PreScanOptions;

      try
        Timer := CreateHiResTimer;
        TokenList := Lexer.ScanAllTokensFast;
        if OptVerbose then
          WriteLn(Format('Tokenized %d tokens in %.2f ms',
            [Lexer.TokenCount, Timer.ElapsedMilliseconds]));
      except
        on E: Exception do
        begin
          WriteLn('ERROR during lexing: ', E.ClassName, ': ', E.Message);
          Exit;
        end;
      end;
    finally
      // Don't free lexer yet, TokenList needs it
    end;
    if OptVerbose then
      WriteLn;

    // === PARSING ===
    if OptVerbose then
      WriteLn('=== PARSING ===');
    Parser := CreatePackratParser;
    try
      try
        Timer := CreateHiResTimer;
        ParserResult := Parser.Parse(TokenList);
        if OptVerbose then
          WriteLn(Format('Parsed in %.2f ms', [Timer.ElapsedMilliseconds]));

        if not ParserResult.Success then
        begin
          WriteLn('ERROR: Parsing failed!');
          if ParserResult.Errors.Count > 0 then
            WriteLn('  ', ParserResult.Errors[0].ToString);
          Exit;
        end;
      except
        on E: Exception do
        begin
          WriteLn('ERROR during parsing: ', E.ClassName, ': ', E.Message);
          Exit;
        end;
      end;

      if OptVerbose then
        WriteLn('Parsing successful!');
    finally
      Parser.Free;
    end;
    if OptVerbose then
      WriteLn;

    // === AST DEBUG ===
    if OptDumpAST then
    begin
      WriteLn('=== AST STRUCTURE ===');
      PrintASTTree(ParserResult.AST, 0, TokenList);
      WriteLn;
    end;

    // === SSA GENERATION ===
    {$IFDEF DEBUG_SSA}
    if DebugSSA then
      WriteLn('=== SSA GENERATION ===');
    {$ENDIF}
    SSAGen := TSSAGenerator.Create;
    try
      // Dialect gate for FB lexical scope: MODERN when the source has no line numbers (mirrors the
      // lexer config above), CLASSIC otherwise. CLASSIC keeps BASIC v7 global-by-name semantics.
      SSAGen.ModernMode := not TSedaiRunner.SourceHasLineNumbers(Source.Text);
      try
        Timer := CreateHiResTimer;
        SSAProgram := SSAGen.Generate(ParserResult.AST);
        SSATime := Timer.ElapsedMilliseconds;

        if not Assigned(SSAProgram) then
        begin
          WriteLn('ERROR: SSA generation failed!');
          Exit;
        end;
      except
        on E: Exception do
        begin
          WriteLn('ERROR during SSA generation: ', E.ClassName, ': ', E.Message);
          Exit;
        end;
      end;

      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn(Format('SSA generated in %.2f ms', [SSATime]));
        WriteLn(Format('  Blocks: %d', [SSAProgram.Blocks.Count]));
        WriteLn(Format('  Variables: %d', [SSAProgram.Variables.Count]));
        WriteLn;
        WriteLn('=== SSA DUMP (BEFORE SSA CONSTRUCTION) ===');
        SSAProgram.PrintSSA;
        WriteLn('=== END SSA DUMP ===');
      end;
      {$ENDIF}

      // Start optimization timer
      Timer := CreateHiResTimer;
      // Initialize per-pass timers
      TimeDBE := 0; TimeDomTree := 0; TimeSSAConstr := 0; TimeGVN := 0;
      TimeCSE := 0; TimeAlgebraic := 0; TimeStrength := 0; TimeGosubInline := 0;
      TimeConstProp := 0; TimeCopyProp := 0; TimeLICM := 0; TimeLoopUnroll := 0;
      TimeDCE := 0; TimePhiElim := 0; TimeCopyCoal := 0; TimeRegAlloc := 0;
      // Baseline for the per-pass effect report (see PassMark)
      PassEffect := TStringList.Create;
      if OptStats then PassPrevHash := SSAProgram.Fingerprint(PassPrevCount);

      {$IFNDEF DISABLE_SUB_INLINING}
      // SUB/FUNCTION INLINING (unification) - flatten small leaf calls FIRST, so the
      // clones go through versioning and every later pass like hand-written code.
      try
        SSAProgram.RunSubInlining;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Sub inlining failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      // ARGUMENT-SLOT FORWARDING - immediately after inlining, and only useful there. Inlining
      // splices the callee's body in but leaves the argument protocol around it: the arguments are
      // staged into the transfer bank and read straight back out, with no call in between any more.
      // Measured on a three-statement SUB in a hot loop: five instructions of fourteen, and 52% of
      // the running time against the same statements written inline by hand.
      try
        SSAProgram.RunXferForwarding;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Xfer forwarding failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      {$ENDIF}

      {$IFNDEF DISABLE_DBE}
      // DEAD BLOCK ELIMINATION - Remove unreachable blocks BEFORE dominator tree
      // CRITICAL: Must run BEFORE dominator tree because dominator tree requires
      // exactly ONE entry point (only entry block can have no predecessors)
      {$IFDEF DEBUG_DBE}
      if DebugDBE then
      begin
        WriteLn;
        WriteLn('=== DEAD BLOCK ELIMINATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunDBE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Dead block elimination failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeDBE := PassTimer.ElapsedMilliseconds;
      PassMark('DBE');
      {$ELSE}
      {$IFDEF DEBUG_DBE}
      if DebugDBE then
      begin
        WriteLn;
        WriteLn('=== DEAD BLOCK ELIMINATION ===');
        WriteLn('[DISABLED] Skipping dead block elimination (optimization disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_DOMINATOR_TREE}
      // PHASE 3 TIER 2: Build dominator tree AFTER dead block elimination
      // CRITICAL: DBE may have removed blocks, so we must build dominator tree AFTER DBE
      {$IFDEF DEBUG_DOMTREE}
      if DebugDomTree then
      begin
        WriteLn;
        WriteLn('=== DOMINATOR TREE CONSTRUCTION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.BuildDominatorTree;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Dominator tree construction failed: ', E.Message);
          WriteLn('Compilation aborted.');
          Exit;
        end;
      end;
      if OptStats then TimeDomTree := PassTimer.ElapsedMilliseconds;
      PassMark('Dom Tree');

      // PHASE 3: Semi-Pruned SSA Construction with versioning
      {$IFNDEF DISABLE_SSA_CONSTRUCTION}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunSSAConstruction;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: SSA construction failed: ', E.Message);
          WriteLn('Compilation aborted.');
          Exit;
        end;
      end;
      if OptStats then TimeSSAConstr := PassTimer.ElapsedMilliseconds;
      PassMark('SSA Constr');
      {$ELSE}
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn;
        WriteLn('=== SSA CONSTRUCTION ===');
        WriteLn('[DISABLED] Skipping SSA construction (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}
      {$ELSE}
      {$IFDEF DEBUG_DOMTREE}
      if DebugDomTree then
      begin
        WriteLn;
        WriteLn('=== DOMINATOR TREE CONSTRUCTION ===');
        WriteLn('[DISABLED] Skipping dominator tree construction (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      // GVN and CSE are mutually exclusive - use one or the other
      {$IFNDEF DISABLE_GVN}
      {$IFDEF DISABLE_CSE}
      // PHASE 3 TIER 2: Run GVN optimization (Steps 4-6)
      {$IFDEF DEBUG_GVN}
      if DebugGVN then
      begin
        WriteLn;
        WriteLn('=== GLOBAL VALUE NUMBERING ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunGVN;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: GVN optimization failed: ', E.Message);
          WriteLn('Continuing with unoptimized SSA...');
        end;
      end;
      if OptStats then TimeGVN := PassTimer.ElapsedMilliseconds;
      PassMark('GVN');
      {$ELSE}
      {$IFDEF DEBUG_GVN}
      if DebugGVN then
      begin
        WriteLn;
        WriteLn('=== GLOBAL VALUE NUMBERING ===');
        WriteLn('[DISABLED] Skipping GVN (CSE is enabled - they are mutually exclusive)');
      end;
      {$ENDIF}
      {$ENDIF}
      {$ELSE}
      {$IFDEF DEBUG_GVN}
      if DebugGVN then
      begin
        WriteLn;
        WriteLn('=== GLOBAL VALUE NUMBERING ===');
        WriteLn('[DISABLED] Skipping GVN optimization (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_CSE}
      {$IFDEF DISABLE_GVN}
      // Common Subexpression Elimination (alternative to GVN)
      {$IFDEF DEBUG_CSE}
      if DebugCSE then
      begin
        WriteLn;
        WriteLn('=== COMMON SUBEXPRESSION ELIMINATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunCSE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: CSE failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeCSE := PassTimer.ElapsedMilliseconds;
      PassMark('CSE');
      {$ELSE}
      {$IFDEF DEBUG_CSE}
      if DebugCSE then
      begin
        WriteLn;
        WriteLn('=== COMMON SUBEXPRESSION ELIMINATION ===');
        WriteLn('[DISABLED] Skipping CSE (GVN is enabled - they are mutually exclusive)');
      end;
      {$ENDIF}
      {$ENDIF}
      {$ELSE}
      {$IFDEF DEBUG_CSE}
      if DebugCSE then
      begin
        WriteLn;
        WriteLn('=== COMMON SUBEXPRESSION ELIMINATION ===');
        WriteLn('[DISABLED] Skipping CSE (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_ALGEBRAIC}
      // Algebraic Simplification
      {$IFDEF DEBUG_ALGEBRAIC}
      if DebugAlgebraic then
      begin
        WriteLn;
        WriteLn('=== ALGEBRAIC SIMPLIFICATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunAlgebraic;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Algebraic simplification failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeAlgebraic := PassTimer.ElapsedMilliseconds;
      PassMark('Algebraic');
      {$ELSE}
      {$IFDEF DEBUG_ALGEBRAIC}
      if DebugAlgebraic then
      begin
        WriteLn;
        WriteLn('=== ALGEBRAIC SIMPLIFICATION ===');
        WriteLn('[DISABLED] Skipping algebraic simplification (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_STRENGTH_RED}
      // Strength Reduction
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
      begin
        WriteLn;
        WriteLn('=== STRENGTH REDUCTION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunStrengthReduction;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Strength reduction failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeStrength := PassTimer.ElapsedMilliseconds;
      PassMark('Strength');
      {$ELSE}
      {$IFDEF DEBUG_STRENGTH}
      if DebugStrength then
      begin
        WriteLn;
        WriteLn('=== STRENGTH REDUCTION ===');
        WriteLn('[DISABLED] Skipping strength reduction (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_GOSUB_INLINE}
      // GOSUB Inlining
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn;
        WriteLn('=== GOSUB INLINING ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunGosubInlining;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: GOSUB inlining failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeGosubInline := PassTimer.ElapsedMilliseconds;
      PassMark('GOSUB Inline');
      {$ELSE}
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn;
        WriteLn('=== GOSUB INLINING ===');
        WriteLn('[DISABLED] Skipping GOSUB inlining (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_CONST_PROP}
      // Simple constant propagation pass
      // Propagates only single-assignment variables with immediate constant values
      // Safe: Does not propagate registers (avoids reuse issues)
      {$IFDEF DEBUG_CONSTPROP}
      if DebugConstProp then
      begin
        WriteLn;
        WriteLn('=== CONSTANT PROPAGATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunConstProp;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Constant propagation failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeConstProp := PassTimer.ElapsedMilliseconds;
      PassMark('Const Prop');
      {$ELSE}
      {$IFDEF DEBUG_CONSTPROP}
      if DebugConstProp then
      begin
        WriteLn;
        WriteLn('=== CONSTANT PROPAGATION ===');
        WriteLn('[DISABLED] Skipping constant propagation (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_COPY_PROP}
      // Copy Propagation
      {$IFDEF DEBUG_COPYPROP}
      if DebugCopyProp then
      begin
        WriteLn;
        WriteLn('=== COPY PROPAGATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunCopyProp;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Copy propagation failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeCopyProp := PassTimer.ElapsedMilliseconds;
      PassMark('Copy Prop');
      {$ELSE}
      {$IFDEF DEBUG_COPYPROP}
      if DebugCopyProp then
      begin
        WriteLn;
        WriteLn('=== COPY PROPAGATION ===');
        WriteLn('[DISABLED] Skipping copy propagation (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_LICM}
      // Loop-Invariant Code Motion pass
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
      begin
        WriteLn;
        WriteLn('=== LOOP-INVARIANT CODE MOTION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunLICM;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Loop-invariant code motion failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeLICM := PassTimer.ElapsedMilliseconds;
      PassMark('LICM');
      {$ELSE}
      {$IFDEF DEBUG_LICM}
      if DebugLICM then
      begin
        WriteLn;
        WriteLn('=== LOOP-INVARIANT CODE MOTION ===');
        WriteLn('[DISABLED] Skipping loop-invariant code motion (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      // Index strength reduction: an array index of the form "invariant + counter" becomes a
      // running index advanced by the counter's step. Placed HERE for two reasons that are not
      // interchangeable: after LICM, because it writes the initial value into the preheader LICM
      // creates; and before the range analysis, because the running indices it emits have to be
      // re-proven in bounds by EvalDerivedIV or the loop gets its guards back and comes out SLOWER.
      try
        SSAProgram.RunIndexReduction;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Index reduction failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      PassMark('Index Reduction');

      {$IFNDEF DISABLE_LOOP_UNROLL}
      // Loop Unrolling - duplicates loop bodies for reduced overhead
      // IMPORTANT: Rebuild dominator tree first because LICM may have created new blocks (pre-headers)
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn;
        WriteLn('=== LOOP UNROLLING ===');
        WriteLn('[UNROLL] Rebuilding dominator tree (LICM may have modified CFG)...');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        // Rebuild dominator tree to include any blocks added by LICM
        SSAProgram.ClearDomTree;
        SSAProgram.BuildDominatorTree;
        SSAProgram.RunLoopUnrolling;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Loop unrolling failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeLoopUnroll := PassTimer.ElapsedMilliseconds;
      PassMark('Loop Unroll');
      {$ELSE}
      {$IFDEF DEBUG_SSA}
      if DebugSSA then
      begin
        WriteLn;
        WriteLn('=== LOOP UNROLLING ===');
        WriteLn('[DISABLED] Skipping loop unrolling (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_DCE}
      // Dead code elimination pass - removes unused PHI nodes, LoadConst, etc.
      // CRITICAL: Must run BEFORE PHI Elimination to remove dead PHI nodes
      {$IFDEF DEBUG_DCE}
      if DebugDCE then
      begin
        WriteLn;
        WriteLn('=== DEAD CODE ELIMINATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunDCE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Dead code elimination failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeDCE := PassTimer.ElapsedMilliseconds;
      PassMark('DCE');
      {$ELSE}
      {$IFDEF DEBUG_DCE}
      if DebugDCE then
      begin
        WriteLn;
        WriteLn('=== DEAD CODE ELIMINATION ===');
        WriteLn('[DISABLED] Skipping dead code elimination (optimizations disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      {$IFNDEF DISABLE_RANGE_ANALYSIS}
      // B4 bounds-check elimination: prove array accesses in-bounds and mark them
      // BoundsSafe. After DCE (instruction positions are final), before PHI
      // elimination (the induction-variable proof needs the PHIs). Sets a hint
      // only - the instruction stream is untouched.
      try
        SSAProgram.RunRangeAnalysis;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Range analysis failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      {$ENDIF}

      // PHI ELIMINATION - FINAL PASS BEFORE BYTECODE COMPILATION
      // CRITICAL: Must run AFTER DCE so dead PHI nodes are already removed
      // This converts remaining live PHI functions to Copy instructions
      {$IFNDEF DISABLE_PHI_ELIM}
      {$IFNDEF DISABLE_SSA_CONSTRUCTION}
      {$IFDEF DEBUG_PHIELIM}
      if DebugPhiElim then
      begin
        WriteLn;
        WriteLn('=== PHI ELIMINATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunPhiElimination;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: PHI elimination failed: ', E.Message);
          WriteLn('Compilation aborted.');
          Exit;
        end;
      end;
      if OptStats then TimePhiElim := PassTimer.ElapsedMilliseconds;
      PassMark('PHI Elim');
      {$ELSE}
      {$IFDEF DEBUG_PHIELIM}
      if DebugPhiElim then
      begin
        WriteLn;
        WriteLn('=== PHI ELIMINATION ===');
        WriteLn('[DISABLED] Skipping PHI elimination (SSA construction disabled)');
      end;
      {$ENDIF}
      {$ENDIF}
      {$ELSE}
      {$IFDEF DEBUG_PHIELIM}
      if DebugPhiElim then
      begin
        WriteLn;
        WriteLn('=== PHI ELIMINATION ===');
        WriteLn('[DISABLED] Skipping PHI elimination (optimization disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      // COPY COALESCING - Remove redundant Copy instructions from PHI Elimination
      // CRITICAL: Must run AFTER PHI Elimination (generates Copy instructions)
      //           and BEFORE Register Allocation (reduces register pressure)
      {$IFNDEF DISABLE_COPY_COAL}
      {$IFDEF DEBUG_COPYCOAL}
      if DebugCopyCoal then
      begin
        WriteLn;
        WriteLn('=== COPY COALESCING ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      try
        SSAProgram.RunCopyCoalescing;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Copy coalescing failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
      if OptStats then TimeCopyCoal := PassTimer.ElapsedMilliseconds;
      PassMark('Copy Coal');
      {$ELSE}
      {$IFDEF DEBUG_COPYCOAL}
      if DebugCopyCoal then
      begin
        WriteLn;
        WriteLn('=== COPY COALESCING ===');
        WriteLn('[DISABLED] Skipping copy coalescing (optimization disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      // STRING TEMP FUSION - let a string primitive write straight into its destination register,
      // instead of a temporary that the next instruction copies. See TSSAProgram.RunStringTempFusion:
      // it must run HERE, on the SSA, because the AOT compiles from this form -- doing it on the
      // bytecode instead desynchronised the two and miscompiled Str() under --aot.
      // ⚠️ This program has its OWN SSA pipeline: there are nine such sites in the tree, and a pass
      // added to only some of them silently does nothing on the paths it missed.
      // ⚠️ The three calls need the begin/end: without it the gate covers only the FIRST one and
      // "STRFUSE=0" still fuses, which makes every A/B on this gate read a contaminated baseline.
      if GetEnvironmentVariable('STRFUSE') <> '0' then
      begin
        // ⚠️ "except end" swallows the pass's own defects: a fusion that aborts halfway looks exactly
        // like one that had nothing to do. Under STRFUSE_DIAG the exception is reported.
        try SSAProgram.RunStringTempFusion;
        except on E: Exception do
          if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
            WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunStringTempFusion: ', E.Message);
        end;
        try SSAProgram.RunAscMidFusion;
        except on E: Exception do
          if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
            WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunAscMidFusion: ', E.Message);
        end;
        try SSAProgram.RunStringTempFusion;
        except on E: Exception do
          if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
            WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunStringTempFusion (2nd): ', E.Message);
        end;
        // Last: it consumes a Mid whose temporary the fusions above have already had their chance
        // at. Gated OFF inside the pass itself (STRCHARFUSE=1 to enable) - see the note there.
        try SSAProgram.RunConcatCharFusion;
        except on E: Exception do
          if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
            WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunConcatCharFusion: ', E.Message);
        end;
        // ⚠️ RunAppendMappedFusion is NOT here any more - it runs AFTER register allocation, see the
        // call further down and the reason there.
        // Last of all: needs the final shape of the concatenations, including anything the fusions
        // above rewrote.
        try SSAProgram.RunConcatDeadSourceMark;
        except on E: Exception do
          if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
            WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunConcatDeadSourceMark: ', E.Message);
        end;
      end;

      // REGISTER ALLOCATION - Allocate physical registers to virtual registers
      // Uses Linear Scan algorithm (O(n log n) complexity)
      // CRITICAL: Must run AFTER PHI Elimination (no more PHI nodes)
      //           and BEFORE Bytecode Compilation (needs physical registers)
      {$IFNDEF DISABLE_REG_ALLOC}
      {$IFDEF DEBUG_REGALLOC}
      if DebugRegAlloc then
      begin
        WriteLn;
        WriteLn('=== REGISTER ALLOCATION ===');
      end;
      {$ENDIF}
      if OptStats then PassTimer := CreateHiResTimer;
      RegAlloc := TLinearScanAllocator.Create(SSAProgram);
      try
        try
          SpillCount := RegAlloc.Run;
          {$IFDEF DEBUG_REGALLOC}
          if DebugRegAlloc then
            WriteLn('[RegAlloc] Register allocation completed with ', SpillCount, ' spills');
          {$ENDIF}
        except
          on E: Exception do
          begin
            WriteLn('ERROR: Register allocation failed: ', E.Message);
            WriteLn('Compilation aborted.');
            Exit;
          end;
        end;
      finally
        RegAlloc.Free;
      end;
      if OptStats then TimeRegAlloc := PassTimer.ElapsedMilliseconds;
      PassMark('Reg Alloc');

      // "acc += tab[Asc(Mid(s,i,1))+1]" into ONE instruction - AFTER register allocation, on purpose.
      //
      // ⭐ This is the whole reason the fusion can exist at all. The fused instruction has to name FIVE
      // values - accumulator out, accumulator in, source string, table, index - and a TSSAInstruction
      // has four operand slots, so with the source in Src1 the incoming accumulator is named by no
      // operand. Run BEFORE allocation, that is fatal: PHI elimination and the allocator cannot see
      // the accumulator arriving, the copies closing the loop-carried PHI are dropped, and a reset of
      // the accumulator lands on a different register from the one the append grows - the miscompile
      // that kept this switched off (job/tests/bas/bug_appendmapped_aot.bas).
      //
      // Run HERE the problem does not arise: the registers are already PHYSICAL, so the incoming and
      // outgoing accumulator ARE the same register and Dest names both. Nothing renames anything after
      // this point, and both consumers of the SSA - the bytecode compiler and the AOT, which compiles
      // from SSA and has a native helper for this opcode - see the fused form.
      //
      // ⚠️ It still CONSUMES what RunConcatCharFusion produces, and that pass runs before allocation.
      // That is fine: allocation rewrites the operands of ssaStrConcatCharAt, it does not remove it.
      try SSAProgram.RunAppendMappedFusion;
      except on E: Exception do
        if GetEnvironmentVariable('STRFUSE_DIAG') = '1' then
          WriteLn(ErrOutput, '[STRFUSE] EXCEPTION in RunAppendMappedFusion: ', E.Message);
      end;
      {$ELSE}
      {$IFDEF DEBUG_REGALLOC}
      if DebugRegAlloc then
      begin
        WriteLn;
        WriteLn('=== REGISTER ALLOCATION ===');
        WriteLn('[DISABLED] Skipping register allocation (optimization disabled)');
      end;
      {$ENDIF}
      {$ENDIF}

      // End optimization timer
      OptTime := Timer.ElapsedMilliseconds;

    finally
      SSAGen.Free;
    end;
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn;
    {$ENDIF}

    // === BYTECODE COMPILATION ===
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn('=== BYTECODE COMPILATION ===');
    {$ENDIF}
    Compiler := TBytecodeCompiler.Create;
    try
      try
        Timer := CreateHiResTimer;
        BytecodeProgram := Compiler.Compile(SSAProgram);
        CompileTime := Timer.ElapsedMilliseconds;

        if not Assigned(BytecodeProgram) then
        begin
          WriteLn('ERROR: Bytecode compilation failed!');
          Exit;
        end;
        // Record the source dialect on the program so the VM can pick dialect-aware behaviour
        // (e.g. filesystem error codes: FreeBASIC vs Commodore). Mirrors SSAGen.ModernMode above.
        BytecodeProgram.ModernMode := not TSedaiRunner.SourceHasLineNumbers(Source.Text);
        // "OPTION DIGITS n" rides out on the PARSE RESULT (the parser itself is
        // long gone) and the VM applies it to the console behavior before running.
        BytecodeProgram.OptionDigits := ParserResult.OptionDigits;
        // ERMN reports the module an error came from. fbc bakes the source PATH as passed on
        // its command line (native separators) into the executable and returns that - so do we
        // (an fbc-compiled erfn.bas prints the full compile-time path, verified live). Assert's
        // fbc-style "path(line):" prefix reuses the same value.
        {$IFDEF WINDOWS}
        BytecodeProgram.ModuleName := StringReplace(SourceFile, '/', '\', [rfReplaceAll]);
        {$ELSE}
        BytecodeProgram.ModuleName := SourceFile;
        {$ENDIF}
        BytecodeProgram.QBLang := QBLangDetected;

      except
        on E: Exception do
        begin
          WriteLn('ERROR during bytecode compilation: ', E.ClassName, ': ', E.Message);
          Exit;
        end;
      end;

      {$IFDEF DEBUG_BYTECODE}
      if DebugBytecode then
      begin
        WriteLn(Format('Compiled in %.2f ms', [CompileTime]));
        WriteLn(Format('  Instructions: %d', [BytecodeProgram.GetInstructionCount]));
        WriteLn(Format('  Variables: %d', [BytecodeProgram.GetVariableCount]));
        WriteLn(Format('  String constants: %d', [BytecodeProgram.StringConstants.Count]));
      end;
      {$ENDIF}
    finally
      Compiler.Free;
    end;
    {$IFDEF DEBUG_BYTECODE}
    if DebugBytecode then
      WriteLn;
    {$ENDIF}

    // === PEEPHOLE OPTIMIZATION ===
    // Local bytecode optimizations (redundant copies, jump chains, etc.)
    // Must run AFTER bytecode compilation and BEFORE superinstructions
    {$IFNDEF DISABLE_PEEPHOLE}
    {$IFDEF DEBUG_PEEPHOLE}
    if DebugPeephole then
      WriteLn('=== PEEPHOLE OPTIMIZATION ===');
    {$ENDIF}
    try
      if GSSAOptimizationsEnabled then RunPeephole(BytecodeProgram);
      {$IFDEF DEBUG_PEEPHOLE}
      if DebugPeephole then
        WriteLn(Format('  Instructions after peephole: %d', [BytecodeProgram.GetInstructionCount]));
      {$ENDIF}
    except
      on E: Exception do
      begin
        WriteLn('ERROR: Peephole optimization failed: ', E.Message);
        WriteLn('Continuing without peephole...');
      end;
    end;
    {$IFDEF DEBUG_PEEPHOLE}
    if DebugPeephole then
      WriteLn;
    {$ENDIF}
    {$ELSE}
    {$IFDEF DEBUG_PEEPHOLE}
    if DebugPeephole then
    begin
      WriteLn('=== PEEPHOLE OPTIMIZATION ===');
      WriteLn('[DISABLED] Skipping peephole optimization (optimization disabled)');
      WriteLn;
    end;
    {$ENDIF}
    {$ENDIF}

    // === DISASSEMBLY BEFORE SUPERINSTRUCTIONS (Optional) ===
    if OptDisasmPre then
    begin
      WriteLn('=== BYTECODE DISASSEMBLY (BEFORE SUPERINSTRUCTIONS) ===');
      Disassembler := TBytecodeDisassembler.Create;
      try
        WriteLn(Disassembler.Disassemble(BytecodeProgram));
      finally
        Disassembler.Free;
      end;
      WriteLn;
    end;

    // === SUPERINSTRUCTIONS ===
    // Fuses common instruction sequences into single superinstructions
    // Must run AFTER bytecode compilation and BEFORE VM execution
    {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn('=== SUPERINSTRUCTIONS ===');
    {$ENDIF}
    try
      if GSSAOptimizationsEnabled then RunSuperinstructions(BytecodeProgram);
      {$IFDEF DEBUG_SUPERINSTR}
      if DebugSuperinstr then
        WriteLn(Format('  Instructions after fusion: %d', [BytecodeProgram.GetInstructionCount]));
      {$ENDIF}
    except
      on E: Exception do
      begin
        WriteLn('ERROR: Superinstruction optimization failed: ', E.Message);
        WriteLn('Continuing without superinstructions...');
      end;
    end;
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn;
    {$ENDIF}
    {$ELSE}
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
    begin
      WriteLn('=== SUPERINSTRUCTIONS ===');
      WriteLn('[DISABLED] Skipping superinstructions (optimization disabled)');
      WriteLn;
    end;
    {$ENDIF}
    {$ENDIF}

    // === NOP COMPACTION ===
    // Removes NOP instructions and adjusts jump targets
    // Must run AFTER superinstructions (which generates NOPs)
    {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
    {$IFNDEF DISABLE_NOP_COMPACTION}
    try
      RunNopCompaction(BytecodeProgram);
    except
      on E: Exception do
      begin
        WriteLn('ERROR: NOP compaction failed: ', E.Message);
        WriteLn('Continuing with NOPs in bytecode...');
      end;
    end;
    {$ENDIF}
    {$ENDIF}

    // === PEEPHOLE PASS 2 ===
    // Second peephole pass to catch redundant jumps after NOP compaction
    // Pattern: Jump N followed by instruction N (jump to next instruction)
    {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
    {$IFNDEF DISABLE_PEEPHOLE}
    try
      if GSSAOptimizationsEnabled then RunPeephole(BytecodeProgram);
      // Run NOP compaction again to remove any new NOPs
      {$IFNDEF DISABLE_NOP_COMPACTION}
      RunNopCompaction(BytecodeProgram);
      {$ENDIF}
    except
      on E: Exception do
        ; // Ignore errors in second pass
    end;
    {$ENDIF}
    {$ENDIF}

    // === REGISTER COMPACTION ===
    // Controlled by DISABLE_REG_COMPACTION in OptimizationFlags.inc
    // Currently disabled by default - enable when type tracking is fixed
    {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
    {$IFNDEF DISABLE_REG_COMPACTION}
    {$IFDEF DEBUG_REGALLOC}
    if DebugRegAlloc then
      WriteLn('=== REGISTER COMPACTION ===');
    {$ENDIF}
    try
      if GSSAOptimizationsEnabled then RunRegisterCompaction(BytecodeProgram);
      {$IFDEF DEBUG_REGALLOC}
      if DebugRegAlloc then
        WriteLn;
      {$ENDIF}
    except
      on E: Exception do
      begin
        WriteLn('ERROR: Register compaction failed: ', E.Message);
        WriteLn('Continuing without compaction...');
      end;
    end;
    {$ENDIF}
    {$ENDIF}

    // === DISASSEMBLY (Optional) ===
    if OptDisasm then
    begin
      Disassembler := TBytecodeDisassembler.Create;
      try
        WriteLn(Disassembler.Disassemble(BytecodeProgram));
      finally
        Disassembler.Free;
      end;
      WriteLn;
    end;

    // === VM EXECUTION ===
    if OptNoExec then
    begin
      // Skip execution - just compile and optionally disassemble
      if OptVerbose then
        WriteLn('=== SKIPPING VM EXECUTION (--no-exec) ===');
      WriteLn('Compilation completed successfully.');
      WriteLn('Instructions: ', BytecodeProgram.GetInstructionCount);
      if OptStats then
      begin
        WriteLn;
        WriteLn('=== COMPILATION STATISTICS ===');
        WriteLn(Format('SSA generation:   %s', [FormatTimeEx(SSATime)]));
        WriteLn(Format('Optimizations:    %s', [FormatTimeEx(OptTime)]));
        WriteLn('  Per-pass breakdown:');
        WriteLn(Format('    DBE:           %8.3f ms', [TimeDBE]));
        WriteLn(Format('    Dom Tree:      %8.3f ms', [TimeDomTree]));
        WriteLn(Format('    SSA Constr:    %8.3f ms', [TimeSSAConstr]));
        WriteLn(Format('    GVN:           %8.3f ms', [TimeGVN]));
        WriteLn(Format('    CSE:           %8.3f ms', [TimeCSE]));
        WriteLn(Format('    Algebraic:     %8.3f ms', [TimeAlgebraic]));
        WriteLn(Format('    Strength:      %8.3f ms', [TimeStrength]));
        WriteLn(Format('    GOSUB Inline:  %8.3f ms', [TimeGosubInline]));
        WriteLn(Format('    Const Prop:    %8.3f ms', [TimeConstProp]));
        WriteLn(Format('    Copy Prop:     %8.3f ms', [TimeCopyProp]));
        WriteLn(Format('    LICM:          %8.3f ms', [TimeLICM]));
        WriteLn(Format('    Loop Unroll:   %8.3f ms', [TimeLoopUnroll]));
        WriteLn(Format('    DCE:           %8.3f ms', [TimeDCE]));
        WriteLn(Format('    PHI Elim:      %8.3f ms', [TimePhiElim]));
        WriteLn(Format('    Copy Coal:     %8.3f ms', [TimeCopyCoal]));
        WriteLn(Format('    Reg Alloc:     %8.3f ms', [TimeRegAlloc]));
        // What each pass actually DID. A timing says what a pass cost; this says whether it earned
        // it. "inert" = the pass rewrote nothing at all on this program.
        if Assigned(PassEffect) and (PassEffect.Count > 0) then
        begin
          WriteLn('  Per-pass effect:');
          for i := 0 to PassEffect.Count - 1 do WriteLn(PassEffect[i]);
        end;
        WriteLn(Format('Compilation time: %s', [FormatTimeEx(CompileTime)]));
        WriteLn(Format('Total time:       %s', [FormatTimeEx(SSATime + OptTime + CompileTime)]));
      end;
      // Cleanup without VM
      BytecodeProgram.Free;
      SSAProgram.Free;
      FreeAndNil(PassEffect);
      ParserResult.Free;
      Lexer.Free;
    end
    else
    begin
      // Execute the program
      if OptVerbose then
        WriteLn('=== VIRTUAL MACHINE EXECUTION ===');
      // Use TIOManager to create I/O devices based on mode
      // Default is terminal mode (pure console, no SDL2)
      GTermCtrl := TTerminalController.Create;
      Output := GTermCtrl;   // keep a concrete handle so --window can attach the shared graphics surface
      Input := TTerminalInput.Create;
      Output.Initialize('SedaiBasic', 80, 25);
      VM := TBytecodeVM.Create;
      WireFileHandler(VM);
    {$IFDEF ENABLE_PROFILER}
    Profiler := nil;
    if OptProfile then
    begin
      // Determine profiler mode
      // sampling = sample-based profiling, low overhead (default)
      // hybrid = sampling + selective instrumentation
      // instrumentation = full instrumentation, accurate but slow
      if ProfileMode = 'sampling' then
        ProfMode := pmSampling
      else if ProfileMode = 'hybrid' then
        ProfMode := pmHybrid
      else if ProfileMode = 'instrumentation' then
        ProfMode := pmInstrumentation
      else
      begin
        WriteLn('WARNING: Unknown profiler mode "', ProfileMode, '", using sampling');
        ProfMode := pmSampling;
        ProfileMode := 'sampling';
      end;

      Profiler := TProfiler.Create(ProfMode);
      VM.SetProfiler(Profiler);
      if OptVerbose then
        WriteLn('Profiler enabled (mode: ', ProfileMode, ')');
    end;
    {$ENDIF}
    try
      VM.SetOutputDevice(Output);
      SetupVMGraphics(VM);  // headless SW backend by default; SDL2 window when `sb --window` (WITH_WINDOW)
      VM.SetProgramArgs(GProgramArgs);  // COMMAND$: command-line args after the script
      VM.SetInputDevice(Input);
      VM.TrueValue := OptTrueValue;  // Set TRUE value for comparisons
      VM.BoundsCheck := OptBoundsCheck;  // --bounds-check: hard-error on out-of-bounds array access
      VM.JitEnabled := OptJit;           // --jit: compile eligible hot loops to native
      {$IFDEF JIT_PROFILE}
      VM.JitProfile := OptJitProfile;    // --jit-profile: count loop back-edges (J1)
      {$ENDIF}
      VM.LoadProgram(BytecodeProgram);

      // === AOT SURVEY (B1-S3, diagnostics only) ===
      // Slices the SSA program into function regions and reports which are compilable with the B1
      // scalar set (stderr). ⛔ It runs HERE, after the VM exists and with the record layout already
      // handed over, because it must see the SAME gates the compiler will: run before that, GRecSize
      // was 0, AotRecNative answered False, and every record field access was reported as taking the
      // helper road when the compiled code lowers it natively. A diagnostic that disagrees with
      // reality is worse than none.
      if GetEnvironmentVariable('AOT_DIAG') = '1' then
      begin
        VM.GetRecordLayout(AotRecordsOff, AotRecSize, AotRecIntOff, AotRecFloatOff, AotSharedRecOff);
        AotSetRecordLayout(AotRecordsOff, AotRecSize, AotRecIntOff, AotRecFloatOff, AotSharedRecOff);
        AotSurvey(SSAProgram, BytecodeProgram,
                  BytecodeProgram.ModernMode and not OptBoundsCheck);  // same gate the compiler uses
      end;

      // === AOT (plan B, B1) ===
      // Compile eligible whole SSA functions to native and register them under their
      // entry PCs. Needs both the SSA program and the FINAL bytecode (PC/register maps).
      if OptAot then
      begin
        // Hand the emitter the record-heap layout so a field access lowers natively instead of
        // paying a helper call (which flushes and reloads the whole register pool around itself).
        // Offsets only - the compiled code reads the live base from the context it is given.
        VM.GetRecordLayout(AotRecordsOff, AotRecSize, AotRecIntOff, AotRecFloatOff, AotSharedRecOff);
        AotSetRecordLayout(AotRecordsOff, AotRecSize, AotRecIntOff, AotRecFloatOff, AotSharedRecOff);
        AotFuncList := AotCompileProgram(SSAProgram, BytecodeProgram, OptTrueValue,
                                         BytecodeProgram.ModernMode and not OptBoundsCheck,
                                         GetEnvironmentVariable('AOT_DIAG') = '1',
                                         AotSkipMainDefault(OptJit));  // engine arbitration, AOT_MAIN overrides
        for AotI := 0 to High(AotFuncList) do
          VM.RegisterAotFunc(AotFuncList[AotI].EntryPC, AotFuncList[AotI].Mem,
                             AotFuncList[AotI].LastPC);
        VM.AotEnabled := Length(AotFuncList) > 0;
      end;

      try
        Timer := CreateHiResTimer;
        {$IFDEF ENABLE_PROFILER}
        if OptProfile then
          VM.Run  // Use slow path with profiler support
        else
          VM.RunFast;  // Use optimized execution loop
        {$ELSE}
        VM.RunFast;  // Use optimized execution loop
        {$ENDIF}
        ExecuteTime := Timer.ElapsedMilliseconds;
        {$IFDEF JIT_PROFILE}
        if OptJitProfile then VM.DumpHotLoops;  // JIT J1: report hot loops found by back-edge profiling
        {$ENDIF}
        FinishVMGraphics(VM);  // sb --window: keep the window open until closed
      except
        on E: Exception do
        begin
          // Default report is DETERMINISTIC (BASIC line + message only): bytecode PCs differ
          // between optimized and --no-opt builds of the same program, so printing them by
          // default made every runtime-erroring program a false OPTDIFF. --verbose restores
          // the full dump (PC, failing instruction, surrounding bytecode).
          // The program's own output may still be buffered: drain it FIRST, or this message
          // lands ahead of text the program produced before failing.
          TerminalOutFlush;
          Write('ERROR during VM execution');
          if OptVerbose then
            Write(' at PC=', VM.PC);
          if (VM.PC >= 0) and (VM.PC < BytecodeProgram.GetInstructionCount) then
          begin
            ErrorSourceLine := BytecodeProgram.GetSourceLine(VM.PC);
            with BytecodeProgram.GetInstruction(VM.PC) do
            begin
              if ErrorSourceLine > 0 then
                WriteLn(' (BASIC LINE ', ErrorSourceLine, '): ', E.ClassName, ': ', E.Message)
              else
                WriteLn(': ', E.ClassName, ': ', E.Message);
              if OptVerbose then
                WriteLn('Failing instruction: ', BytecodeOpToString(TBytecodeOp(OpCode)),
                        ' Dest=', Dest, ' Src1=', Src1, ' Src2=', Src2);
            end;
            if OptVerbose then
            begin
              // Dump surrounding instructions to see the bytecode sequence
              WriteLn;
              WriteLn('Bytecode context (PC-7 to PC+2):');
              for i := Max(0, VM.PC - 7) to Min(BytecodeProgram.GetInstructionCount - 1, VM.PC + 2) do
              begin
                with BytecodeProgram.GetInstruction(i) do
                begin
                  Write(Format('%4d: %-15s', [i, BytecodeOpToString(TBytecodeOp(OpCode))]));
                  WriteLn(Format(' Dest=%3d Src1=%3d Src2=%3d Imm=%d', [Dest, Src1, Src2, Immediate]));
                end;
              end;
            end;
          end;
          Exit;
        end;
      end;

      if OptStats then
      begin
        WriteLn;
        WriteLn('=== EXECUTION STATISTICS ===');
        {$IFDEF ENABLE_INSTRUCTION_COUNTING}
        WriteLn(Format('Instructions executed: %d', [VM.InstructionsExecuted]));
        if ExecuteTime > 0 then
          WriteLn(Format('Time per instruction:  %s', [FormatTime(ExecuteTime / VM.InstructionsExecuted)]));
        {$ENDIF}
        WriteLn(Format('SSA generation:   %s', [FormatTimeEx(SSATime)]));
        WriteLn(Format('Optimizations:    %s', [FormatTimeEx(OptTime)]));
        WriteLn('  Per-pass breakdown:');
        WriteLn(Format('    DBE:           %8.3f ms', [TimeDBE]));
        WriteLn(Format('    Dom Tree:      %8.3f ms', [TimeDomTree]));
        WriteLn(Format('    SSA Constr:    %8.3f ms', [TimeSSAConstr]));
        WriteLn(Format('    GVN:           %8.3f ms', [TimeGVN]));
        WriteLn(Format('    CSE:           %8.3f ms', [TimeCSE]));
        WriteLn(Format('    Algebraic:     %8.3f ms', [TimeAlgebraic]));
        WriteLn(Format('    Strength:      %8.3f ms', [TimeStrength]));
        WriteLn(Format('    GOSUB Inline:  %8.3f ms', [TimeGosubInline]));
        WriteLn(Format('    Const Prop:    %8.3f ms', [TimeConstProp]));
        WriteLn(Format('    Copy Prop:     %8.3f ms', [TimeCopyProp]));
        WriteLn(Format('    LICM:          %8.3f ms', [TimeLICM]));
        WriteLn(Format('    Loop Unroll:   %8.3f ms', [TimeLoopUnroll]));
        WriteLn(Format('    DCE:           %8.3f ms', [TimeDCE]));
        WriteLn(Format('    PHI Elim:      %8.3f ms', [TimePhiElim]));
        WriteLn(Format('    Copy Coal:     %8.3f ms', [TimeCopyCoal]));
        WriteLn(Format('    Reg Alloc:     %8.3f ms', [TimeRegAlloc]));
        // What each pass actually DID. A timing says what a pass cost; this says whether it earned
        // it. "inert" = the pass rewrote nothing at all on this program.
        if Assigned(PassEffect) and (PassEffect.Count > 0) then
        begin
          WriteLn('  Per-pass effect:');
          for i := 0 to PassEffect.Count - 1 do WriteLn(PassEffect[i]);
        end;
        WriteLn(Format('Compilation time: %s', [FormatTimeEx(CompileTime)]));
        WriteLn(Format('Execution time:   %s', [FormatTimeEx(ExecuteTime)]));
        WriteLn(Format('Total time:       %s', [FormatTimeEx(SSATime + OptTime + CompileTime + ExecuteTime)]));
      end;

      {$IFDEF ENABLE_PROFILER}
      // Profiler report and export
      if OptProfile and Assigned(Profiler) then
      begin
        WriteLn;
        Profiler.PrintReport;

        // Export if requested
        if ProfileExport <> '' then
        begin
          ExportExt := LowerCase(ExtractFileExt(ProfileExport));
          if ExportExt = '.json' then
            Profiler.ExportJSON(ProfileExport)
          else if ExportExt = '.csv' then
            Profiler.ExportCSV(ProfileExport)
          else if ExportExt = '.folded' then
            Profiler.ExportFoldedFlameGraph(ProfileExport)
          else
          begin
            WriteLn('WARNING: Unknown export format "', ExportExt, '", defaulting to JSON');
            Profiler.ExportJSON(ProfileExport);
          end;
          WriteLn('Profile data exported to: ', ProfileExport);
        end;
      end;
      {$ENDIF}
      finally
        {$IFDEF ENABLE_PROFILER}
        Profiler.Free;
        {$ENDIF}
        VM.Free;
        BytecodeProgram.Free;
        SSAProgram.Free;
        FreeAndNil(PassEffect);
      end;

      ParserResult.Free;
      Lexer.Free;
    end;  // end of else (not OptNoExec)

  finally
    Source.Free;
  end;

  if ShowBanners then
  begin
    WriteLn;
    WriteLn('========================================');
    WriteLn('Test complete!');
    WriteLn('========================================');
  end;
end;

{ Execute a pre-compiled bytecode file (.basc) }
procedure RunFromBytecode(const BytecodeFile: string;
  OptVerbose, OptDisasm, OptStats, OptNoExec: Boolean
  {$IFDEF ENABLE_PROFILER}; OptProfile: Boolean; ProfileMode: string; ProfileExport: string{$ENDIF});
var
  Serializer: TBytecodeSerializer;
  BytecodeProgram: TBytecodeProgram;
  VM: TBytecodeVM;
  Disassembler: TBytecodeDisassembler;
  Output: IOutputDevice;
  Input: IInputDevice;
  Timer: THiResTimer;
  LoadTime, ExecuteTime: Double;
  i: Integer;
  ErrorSourceLine: Integer;
  ShowBanners: Boolean;
  {$IFDEF ENABLE_PROFILER}
  Profiler: TProfiler;
  ProfMode: TProfilerMode;
  ExportExt: string;
  {$ENDIF}
begin
  ShowBanners := OptVerbose or OptDisasm or OptStats;

  if ShowBanners then
  begin
    WriteLn('========================================');
    PrintVersion;
    WriteLn('========================================');
    WriteLn;
    WriteLn('Loading pre-compiled bytecode: ', BytecodeFile);
  end;

  // Load bytecode from file
  Serializer := TBytecodeSerializer.Create;
  try
    Timer := CreateHiResTimer;
    try
      BytecodeProgram := Serializer.LoadFromFile(BytecodeFile);
      LoadTime := Timer.ElapsedMilliseconds;
    except
      on E: Exception do
      begin
        WriteLn('ERROR loading bytecode: ', E.Message);
        Exit;
      end;
    end;

    if OptVerbose then
    begin
      WriteLn(Format('Bytecode loaded in %.2f ms', [LoadTime]));
      WriteLn(Format('  Instructions: %d', [BytecodeProgram.GetInstructionCount]));
      WriteLn(Format('  Variables: %d', [BytecodeProgram.GetVariableCount]));
      WriteLn(Format('  String constants: %d', [BytecodeProgram.StringConstants.Count]));
      WriteLn;
    end;

    // === DISASSEMBLY (Optional) ===
    if OptDisasm then
    begin
      WriteLn('=== BYTECODE DISASSEMBLY ===');
      Disassembler := TBytecodeDisassembler.Create;
      try
        WriteLn(Disassembler.Disassemble(BytecodeProgram));
      finally
        Disassembler.Free;
      end;
      WriteLn;
    end;

    // === VM EXECUTION ===
    if OptNoExec then
    begin
      if OptVerbose then
        WriteLn('=== SKIPPING VM EXECUTION (--no-exec) ===');
      WriteLn('Bytecode loaded successfully.');
      WriteLn('Instructions: ', BytecodeProgram.GetInstructionCount);
      BytecodeProgram.Free;
    end
    else
    begin
      if OptVerbose then
        WriteLn('=== VIRTUAL MACHINE EXECUTION ===');

      GTermCtrl := TTerminalController.Create;
      Output := GTermCtrl;   // keep a concrete handle so --window can attach the shared graphics surface
      Input := TTerminalInput.Create;
      Output.Initialize('SedaiBasic', 80, 25);
      VM := TBytecodeVM.Create;
      WireFileHandler(VM);
      {$IFDEF ENABLE_PROFILER}
      Profiler := nil;
      if OptProfile then
      begin
        if ProfileMode = 'sampling' then
          ProfMode := pmSampling
        else if ProfileMode = 'hybrid' then
          ProfMode := pmHybrid
        else if ProfileMode = 'instrumentation' then
          ProfMode := pmInstrumentation
        else
        begin
          WriteLn('WARNING: Unknown profiler mode "', ProfileMode, '", using sampling');
          ProfMode := pmSampling;
          ProfileMode := 'sampling';
        end;

        Profiler := TProfiler.Create(ProfMode);
        VM.SetProfiler(Profiler);
        if OptVerbose then
          WriteLn('Profiler enabled (mode: ', ProfileMode, ')');
      end;
      {$ENDIF}
      try
        VM.SetOutputDevice(Output);
        SetupVMGraphics(VM);  // headless SW backend by default; SDL2 window when `sb --window` (WITH_WINDOW)
        VM.SetProgramArgs(GProgramArgs);  // COMMAND$: command-line args after the script
        VM.SetInputDevice(Input);
        VM.TrueValue := OptTrueValue;  // Set TRUE value for comparisons
        VM.BoundsCheck := OptBoundsCheck;  // --bounds-check: hard-error on out-of-bounds array access
        // --jit works here exactly as it does for a source run: the loop JIT compiles from the
        // BYTECODE, which is all a .basc has. Without this assignment the flag was accepted and
        // silently ignored, and a .basc ran fully interpreted - 17.6 s against 0.93 s on n-body.
        // (--aot is a different story and stays out: it compiles from the SSA program, which only
        // exists when compiling from source. A .basc carries no SSA.)
        VM.JitEnabled := OptJit;
        {$IFDEF JIT_PROFILE}
        VM.JitProfile := OptJitProfile;
        {$ENDIF}
        VM.LoadProgram(BytecodeProgram);

        try
          Timer := CreateHiResTimer;
          {$IFDEF ENABLE_PROFILER}
          if OptProfile then
            VM.Run
          else
            VM.RunFast;
          {$ELSE}
          VM.RunFast;
          {$ENDIF}
          ExecuteTime := Timer.ElapsedMilliseconds;
          FinishVMGraphics(VM);  // sb --window: keep the window open until closed
        except
          on E: Exception do
          begin
            // Same report as the run-from-source handler: a program must report errors
            // identically whether launched from .bas or .basc — deterministic by default
            // (BASIC line + message), full PC/bytecode dump only with --verbose.
            TerminalOutFlush;   // drain the program's buffered output before this message
            Write('ERROR during VM execution');
            if OptVerbose then
              Write(' at PC=', VM.PC);
            if (VM.PC >= 0) and (VM.PC < BytecodeProgram.GetInstructionCount) then
            begin
              ErrorSourceLine := BytecodeProgram.GetSourceLine(VM.PC);
              with BytecodeProgram.GetInstruction(VM.PC) do
              begin
                if ErrorSourceLine > 0 then
                  WriteLn(' (BASIC LINE ', ErrorSourceLine, '): ', E.ClassName, ': ', E.Message)
                else
                  WriteLn(': ', E.ClassName, ': ', E.Message);
                if OptVerbose then
                  WriteLn('Failing instruction: ', BytecodeOpToString(TBytecodeOp(OpCode)),
                          ' Dest=', Dest, ' Src1=', Src1, ' Src2=', Src2);
              end;
              if OptVerbose then
              begin
                WriteLn;
                WriteLn('Bytecode context (PC-7 to PC+2):');
                for i := Max(0, VM.PC - 7) to Min(BytecodeProgram.GetInstructionCount - 1, VM.PC + 2) do
                begin
                  with BytecodeProgram.GetInstruction(i) do
                  begin
                    Write(Format('%4d: %-15s', [i, BytecodeOpToString(TBytecodeOp(OpCode))]));
                    WriteLn(Format(' Dest=%3d Src1=%3d Src2=%3d Imm=%d', [Dest, Src1, Src2, Immediate]));
                  end;
                end;
              end;
            end;
            Exit;
          end;
        end;

        if OptStats then
        begin
          WriteLn;
          WriteLn('=== EXECUTION STATISTICS ===');
          {$IFDEF ENABLE_INSTRUCTION_COUNTING}
          WriteLn(Format('Instructions executed: %d', [VM.InstructionsExecuted]));
          if ExecuteTime > 0 then
            WriteLn(Format('Time per instruction:  %s', [FormatTime(ExecuteTime / VM.InstructionsExecuted)]));
          {$ENDIF}
          WriteLn(Format('Load time:       %s', [FormatTimeEx(LoadTime)]));
          WriteLn(Format('Execution time:  %s', [FormatTimeEx(ExecuteTime)]));
          WriteLn(Format('Total time:      %s', [FormatTimeEx(LoadTime + ExecuteTime)]));
        end;

        {$IFDEF ENABLE_PROFILER}
        if OptProfile and Assigned(Profiler) then
        begin
          WriteLn;
          Profiler.PrintReport;

          if ProfileExport <> '' then
          begin
            ExportExt := LowerCase(ExtractFileExt(ProfileExport));
            if ExportExt = '.json' then
              Profiler.ExportJSON(ProfileExport)
            else if ExportExt = '.csv' then
              Profiler.ExportCSV(ProfileExport)
            else if ExportExt = '.folded' then
              Profiler.ExportFoldedFlameGraph(ProfileExport)
            else
            begin
              WriteLn('WARNING: Unknown export format "', ExportExt, '", defaulting to JSON');
              Profiler.ExportJSON(ProfileExport);
            end;
            WriteLn('Profile data exported to: ', ProfileExport);
          end;
        end;
        {$ENDIF}
      finally
        {$IFDEF ENABLE_PROFILER}
        Profiler.Free;
        {$ENDIF}
        VM.Free;
        BytecodeProgram.Free;
      end;
    end;

  finally
    Serializer.Free;
  end;

  if ShowBanners then
  begin
    WriteLn;
    WriteLn('========================================');
    WriteLn('Execution complete!');
    WriteLn('========================================');
  end;
end;

var
  TestFile: string;
  FileType: TSedaiFileType;
  OptVerbose, OptDumpAST, OptDisasm, OptDisasmPre, OptStats, OptHelp, OptNoExec: Boolean;
  {$IFDEF ENABLE_PROFILER}
  OptProfile: Boolean;
  ProfileMode: string;
  ProfileExport: string;
  {$ENDIF}
  i: Integer;
  Param: string;
  VerifyMsg: string;
  VerifyI: Integer;

begin
  try
    // Mask the FPU/SSE exceptions so floating-point overflow/invalid/div-by-zero produce IEEE Inf/NaN
    // (FreeBASIC/C semantics) instead of raising a Pascal exception that would abort the program. FPC
    // leaves these unmasked by default. The VM already guards integer and explicit division by zero by
    // value, so this only affects genuine float edge cases (e.g. an escaping Mandelbrot iterate overflowing).
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

    // Set console code page to UTF-8 for proper character encoding. SetupConsoleUTF8 saves the previous
    // code pages (restored from SedaiConsoleState's finalization) and does nothing at all when stdout is
    // redirected or piped: the console belongs to the parent shell, and a run that is killed rather than
    // exiting -- as the regression harness does on timeout -- would never restore it.
    {$IFDEF WINDOWS}
    SetupConsoleUTF8;
    SetTextCodePage(Output, CP_UTF8);
    SetTextCodePage(Input, CP_UTF8);
    SetMultiByteConversionCodePage(CP_UTF8);
    DefaultSystemCodePage := CP_UTF8;
    {$ENDIF}

    // Force BASIC-compatible number formatting process-wide. The lexer's constructor sets the
    // same thing, but a .basc run never creates a lexer -- without this, PRINT of a float
    // followed the OS locale (e.g. a comma decimal separator on an Italian system).
    DefaultFormatSettings.DecimalSeparator := '.';
    DefaultFormatSettings.ThousandSeparator := ',';

    // Initialize random number generator
    Randomize;

    // Initialize debug flags from command-line parameters
    InitDebugFlags;

    // VM dispatch plan, milestone M1: one-shot opcode-table self-check (dense-map bijection/coverage).
    // Diagnostic only; prints the summary and exits without touching the dispatch path.
    if (ParamCount >= 1) and (LowerCase(ParamStr(1)) = '--verify-opcodes') then
    begin
      if VerifyOpcodeTable(VerifyMsg) then
        WriteLn('opcode-table OK: ', VerifyMsg)
      else
        WriteLn('opcode-table FAIL: ', VerifyMsg);
      Halt(0);
    end;

    // Every opcode the compiler can emit, with the name the disassembler would print for it. One line
    // per opcode, sorted by code. Diagnostic only -- but it is also the NET for any change to how
    // names are produced: capture it before, capture it after, and require the diff to contain only
    // the lines you meant to change.
    if (ParamCount >= 1) and (LowerCase(ParamStr(1)) = '--dump-opnames') then
    begin
      for VerifyI := 0 to OPCODE_LIST_COUNT - 1 do
        WriteLn(Format('$%.4X %s', [OPCODES[VerifyI],
                       BytecodeOpToString(TBytecodeOp(OPCODES[VerifyI]))]));
      Halt(0);
    end;

    // Keyword coverage self-check: read a list of names (one per line) and report, for each, whether
    // THIS build's front end knows it. The answer comes from the real lexer -- a name it does not know
    // comes back as ttIdentifier, anything else is a keyword it recognises -- so the check can never
    // drift from the compiler the way a hand-maintained table does. Diagnostic only; prints and exits.
    // Companion tool: job/tests/tools/kwcheck.ps1, which feeds it the FreeBASIC manual's own index.
    if (ParamCount >= 2) and (LowerCase(ParamStr(1)) = '--kw-check') then
    begin
      KwCheckReport(ParamStr(2));
      Halt(0);
    end;

    // JIT J2 foundation self-test: emit a tiny native function and call it (proves the
    // emitter + executable-memory + native-call pipeline works). Prints the result and exits.
    if (ParamCount >= 1) and (LowerCase(ParamStr(1)) = '--jit-selftest') then
    begin
      if JitSelfTest(VerifyMsg) then
        WriteLn('jit-selftest OK: ', VerifyMsg)
      else
        WriteLn('jit-selftest FAIL: ', VerifyMsg);
      Halt(0);
    end;

    // Parse command-line parameters
    TestFile := '';
    OptVerbose := False;
    OptDumpAST := False;
    OptDisasm := False;
    OptDisasmPre := False;
    OptStats := False;
    OptHelp := False;
    OptNoExec := False;
    OptTrueValue := -1;  // Default: Commodore BASIC style (TRUE = -1)
    {$IFDEF ENABLE_PROFILER}
    OptProfile := False;
    ProfileMode := 'sampling';  // Default to low-overhead sampling
    ProfileExport := '';
    {$ENDIF}

    for i := 1 to ParamCount do
    begin
      Param := LowerCase(ParamStr(i));
      if Param = '--verbose' then
        OptVerbose := True
      else if Param = '--dump-ast' then
        OptDumpAST := True
      else if Param = '--disasm' then
        OptDisasm := True
      else if Param = '--disasm-pre' then
        OptDisasmPre := True
      else if Param = '--stats' then
        OptStats := True
      else if Param = '--no-exec' then
        OptNoExec := True
      else if (Param = '--no-opt') or (Param = '--no-optimize') then
        GSSAOptimizationsEnabled := False   // differential-test reference: skip the optimization passes
      else if (Param = '--bounds-check') or (Param = '--boundscheck') then
        OptBoundsCheck := True   // force array bounds checking on (even in MODERN); default follows the dialect
      else if (Param = '--date-locale') or (Param = '--datelocale') then
        SetDateLocaleMode(True)  // month/day names and date parsing follow the SYSTEM locale, as fbc does
      else if (Param = '--date-deterministic') then
        SetDateLocaleMode(False) // explicit default: English names, ISO-ish parsing, same on every machine
      else if (Param = '--jit') then
        begin
          OptJit := True;  // JIT: compile eligible hot loops to native code (J2/J3)
          GJitWillRun := True;   // ...e le superistruzioni devono stare zitte: vedi il flag
        end
      else if (Param = '--aot') then
      begin
        OptAot := True;  // AOT (plan B): compile eligible whole SSA functions to native
        // Let the SSA pipeline know which engine is coming: RunConcatCharFusion emits a shape that
        // pays under the AOT and costs when interpreted (see the note in that pass).
        GAotWillRun := True;
      end
      {$IFDEF JIT_PROFILE}
      else if (Param = '--jit-profile') or (Param = '--jitprofile') then
        OptJitProfile := True   // JIT J1: profile hot loops (back-edge counts) and dump them after the run
      {$ENDIF}
      else if Param = '--window' then
      begin
        OptWindow := True;   // present graphics in an SDL2 window
        {$IFNDEF WITH_WINDOW}
        // ⛔ SAY SO. WITH_WINDOW is a {$DEFINE}, so on a build without it this flag used to be accepted
        // and IGNORED: the program ran headless, printed its frame rate, exited cleanly - and no window
        // ever appeared. Nothing was wrong with the program, nothing was wrong with the command line,
        // and there was no way to tell from the outside. That is the same shape as an exit code of 0
        // meaning "did not complain" rather than "worked".
        WriteLn(ErrOutput, '?--window: this build has no window presenter (rebuild with: ./build.sh sb --window)');
        {$ENDIF}
      end
      else if (Param = '--help') or (Param = '-h') or (Param = '-?') then
        OptHelp := True
      {$IFDEF ENABLE_PROFILER}
      else if (Param = '--profile') then
      begin
        OptProfile := True;
        // Keep default mode (full)
      end
      else if Pos('--profile=', Param) = 1 then
      begin
        OptProfile := True;
        ProfileMode := Copy(Param, 11, Length(Param));
      end
      else if Pos('--profile-export=', Param) = 1 then
        ProfileExport := Copy(ParamStr(i), 18, Length(ParamStr(i)))  // Use original case for filename
      {$ENDIF}
      else if Pos('--true-value=', Param) = 1 then
      begin
        // Parse TRUE value: -1 (Commodore BASIC) or 1 (modern BASIC)
        OptTrueValue := StrToInt64Def(Copy(Param, 14, Length(Param)), -1);
        if (OptTrueValue <> -1) and (OptTrueValue <> 1) then
        begin
          WriteLn('WARNING: --true-value must be -1 or 1, using default -1');
          OptTrueValue := -1;
        end;
      end
      else if (Pos('--', Param) <> 1) and (TestFile = '') then
        TestFile := ParamStr(i)   // first non-flag argument = the script/bytecode file
      else if (Pos('--', Param) <> 1) then
      begin
        // Non-flag arguments after the script are program arguments (for COMMAND$). Known sb flags are
        // still recognised anywhere (see above); flag-looking tokens are not forwarded to the program.
        SetLength(GProgramArgs, Length(GProgramArgs) + 1);
        GProgramArgs[High(GProgramArgs)] := ParamStr(i);
      end;
    end;

    // Show help if requested or no file provided
    if OptHelp or (TestFile = '') then
    begin
      PrintHelp;
      Exit;
    end;

    // Convenience: "sb hello" finds "hello.bas" (then "hello.basc"). Only when the name as typed
    // does not exist, so an explicit "sb hello.bas" still wins and a real path is never shadowed.
    if not FileExists(TestFile) then
    begin
      if FileExists(TestFile + '.bas') then
        TestFile := TestFile + '.bas'
      else if FileExists(TestFile + '.basc') then
        TestFile := TestFile + '.basc';
    end;

    // Check if file exists
    if not FileExists(TestFile) then
    begin
      WriteLn('ERROR: File not found: ', TestFile);
      ExitCode := 1;
      Exit;
    end;

    // Detect file type and run appropriate handler
    FileType := TSedaiRunner.DetectFileType(TestFile);

    case FileType of
      sftSource:
        // Compile and run .bas source file
        TestBytecodeCompilation(TestFile, OptVerbose, OptDumpAST, OptDisasm, OptDisasmPre, OptStats, OptNoExec
          {$IFDEF ENABLE_PROFILER}, OptProfile, ProfileMode, ProfileExport{$ENDIF});

      sftBytecode:
        begin
          // Run pre-compiled .basc bytecode
          // Note: --dump-ast and --disasm-pre are not applicable for bytecode
          if OptDumpAST then
            WriteLn('WARNING: --dump-ast not available for .basc files (no AST)');
          if OptDisasmPre then
            WriteLn('WARNING: --disasm-pre not available for .basc files (already optimized)');

          RunFromBytecode(TestFile, OptVerbose, OptDisasm, OptStats, OptNoExec
            {$IFDEF ENABLE_PROFILER}, OptProfile, ProfileMode, ProfileExport{$ENDIF});
        end;

      else
        begin
          WriteLn('ERROR: Unknown file type: ', TestFile);
          WriteLn('Supported extensions: .bas (source), .basc (bytecode)');
          ExitCode := 1;
        end;
    end;

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      // Hold the window open only when a human is actually there to close it (sb launched by
      // double-click, stdin attached to a console). Under a pipe, a redirect or a test harness nobody
      // can press anything: the wait never ends, sb never exits, and the run becomes a stray process
      // that has to be hunted down and killed.
      if StdInIsConsole then
      begin
        WriteLn;
        WriteLn('Press Enter to exit...');
        ReadLn;
      end;
      ExitCode := 1;
    end;
  end;
end.
