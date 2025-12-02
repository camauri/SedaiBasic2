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
{$DEFINE ENABLE_INSTRUCTION_COUNTING}  // Must match define in SedaiBytecodeVM.pas

// Include shared optimization flags
{$I OptimizationFlags.inc}
// Include debug flags (compile-time control of debug code)
{$I DebugFlags.inc}
// Include profiler flags
{$I ProfilerFlags.inc}

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Variants, TypInfo, Math,
  // Lexer/Parser
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser, SedaiDateTimeUtils,
  // Bytecode VM
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeTypes, SedaiBytecodeCompiler, SedaiBytecodeVM,
  SedaiBytecodeDisassembler,
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
  SedaiExecutorContext, SedaiExecutorTypes, SedaiOutputInterface;

// Include version information (must be after uses, contains const declarations)
{$I Version.inc}

type
  { Simple console input for testing }
  TConsoleInput = class(TInterfacedObject, IInputDevice)
  public
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True;
                     NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function ShouldQuit: Boolean;
    procedure ProcessEvents;
    procedure Reset;
  end;

  { Simple console output for testing }
  TConsoleOutput = class(TInterfacedObject, IOutputDevice)
  public
    function Initialize(const Title: string = ''; Width: Integer = 80; Height: Integer = 25): Boolean;
    procedure Shutdown;
    function IsInitialized: Boolean;
    procedure Print(const Text: string; ClearBackground: Boolean = False);
    procedure PrintLn(const Text: string; ClearBackground: Boolean = False);
    procedure NewLine;
    procedure Clear;
    procedure SetCursor(X, Y: Integer);
    procedure MoveCursor(DeltaX, DeltaY: Integer);
    function GetCursorX: Integer;
    function GetCursorY: Integer;
    procedure ShowCursor(X, Y: Integer);
    procedure HideCursor(X, Y: Integer);
    procedure SetColors(Foreground, Background: TColor);
    procedure Present;
    procedure SetFullscreen(Enabled: Boolean);
    function IsFullscreen: Boolean;
    function ShouldQuit: Boolean;
    function GetActualCols: Integer;
    function GetActualRows: Integer;
    procedure MarkPromptRow;
    procedure OnUserInput;
    function HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
    procedure ProcessScrollInput;
    function GetInScrollMode: Boolean;
    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False; SplitLine: Integer = -1): Boolean;
    function IsInGraphicsMode: Boolean;
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;
  end;

function TConsoleOutput.Initialize(const Title: string; Width: Integer; Height: Integer): Boolean;
begin
  Result := True;
end;

procedure TConsoleOutput.Shutdown;
begin
end;

function TConsoleOutput.IsInitialized: Boolean;
begin
  Result := True;
end;

procedure TConsoleOutput.Print(const Text: string; ClearBackground: Boolean);
begin
  System.Write(Text);
end;

procedure TConsoleOutput.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  WriteLn(Text);
end;

procedure TConsoleOutput.NewLine;
begin
  WriteLn;
end;

procedure TConsoleOutput.Clear;
begin
  // Not implemented for console
end;

procedure TConsoleOutput.SetCursor(X, Y: Integer);
begin
end;

procedure TConsoleOutput.MoveCursor(DeltaX, DeltaY: Integer);
begin
end;

function TConsoleOutput.GetCursorX: Integer;
begin
  Result := 0;
end;

function TConsoleOutput.GetCursorY: Integer;
begin
  Result := 0;
end;

procedure TConsoleOutput.ShowCursor(X, Y: Integer);
begin
end;

procedure TConsoleOutput.HideCursor(X, Y: Integer);
begin
end;

procedure TConsoleOutput.SetColors(Foreground, Background: TColor);
begin
end;

procedure TConsoleOutput.Present;
begin
end;

procedure TConsoleOutput.SetFullscreen(Enabled: Boolean);
begin
end;

function TConsoleOutput.IsFullscreen: Boolean;
begin
  Result := False;
end;

function TConsoleOutput.ShouldQuit: Boolean;
begin
  Result := False;
end;

function TConsoleOutput.GetActualCols: Integer;
begin
  Result := 80;
end;

function TConsoleOutput.GetActualRows: Integer;
begin
  Result := 25;
end;

procedure TConsoleOutput.MarkPromptRow;
begin
end;

procedure TConsoleOutput.OnUserInput;
begin
end;

function TConsoleOutput.HandleScrollKeys(Key: Integer; Modifiers: Integer): Boolean;
begin
  Result := False;
end;

procedure TConsoleOutput.ProcessScrollInput;
begin
end;

function TConsoleOutput.GetInScrollMode: Boolean;
begin
  Result := False;
end;

function TConsoleOutput.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean; SplitLine: Integer): Boolean;
begin
  Result := False;
end;

function TConsoleOutput.IsInGraphicsMode: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutput.SetPixel(X, Y: Integer; RGB: UInt32);
begin
end;

procedure TConsoleOutput.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
end;

function TConsoleOutput.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;
end;

procedure TConsoleOutput.EnablePalette(Enable: Boolean);
begin
end;

function TConsoleOutput.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TConsoleOutput.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
end;

function TConsoleOutput.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;
end;

procedure TConsoleOutput.ResetPalette;
begin
end;

// === CONSOLE INPUT IMPLEMENTATION ===

function TConsoleInput.ReadLine(const Prompt: string; IsCommand: Boolean;
                                NumericOnly: Boolean; AllowDecimal: Boolean): string;
var
  Input: string;
  IsValid: Boolean;
  TempFloat: Double;
begin
  repeat
    IsValid := True;

    // Display prompt
    if Prompt <> '' then
      System.Write(Prompt);

    // Read input
    System.ReadLn(Input);

    // Validate if numeric only
    if NumericOnly and (Input <> '') then
    begin
      // Check if valid number
      if not TryStrToFloat(Input, TempFloat) then
      begin
        WriteLn('?SYNTAX ERROR - Number expected');
        IsValid := False;
      end
      else if not AllowDecimal then
      begin
        // Check if it contains a decimal point
        if Pos('.', Input) > 0 then
        begin
          WriteLn('?SYNTAX ERROR - Integer expected');
          IsValid := False;
        end;
      end;
    end;
  until IsValid;

  Result := Input;
end;

function TConsoleInput.ReadKey: Char;
begin
  Result := #0;
  // Not implemented for simple console
end;

function TConsoleInput.KeyPressed: Boolean;
begin
  Result := False;
  // Not implemented for simple console
end;

function TConsoleInput.ShouldQuit: Boolean;
begin
  Result := False;
end;

procedure TConsoleInput.ProcessEvents;
begin
  // Not needed for simple console
end;

procedure TConsoleInput.Reset;
begin
  // Not needed for simple console
end;

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

{ Format time with smart scaling:
  < 0.001ms (< 1µs):  nanoseconds
  0.001-1ms:          microseconds with 2 decimals
  1-60000ms:          milliseconds with 3 decimals
  60000-120000ms:     milliseconds + (seconds in parentheses)
  120000-300000ms:    seconds + (hh:mm:ss.nnn in parentheses)
  > 300000ms:         hh:mm:ss.nnn directly
}
function FormatTimeEx(TimeMs: Double): string;
var
  TimeUs, TimeNs: Double;
  Seconds: Double;
  Hours, Minutes, Secs, Millis: Integer;
begin
  if TimeMs < 0.001 then
  begin
    // Less than 1µs -> nanoseconds
    TimeNs := TimeMs * 1000000.0;
    Result := Format('%.0f ns', [TimeNs]);
  end
  else if TimeMs < 1.0 then
  begin
    // 1µs to 1ms -> microseconds
    TimeUs := TimeMs * 1000.0;
    Result := Format('%.2f µs', [TimeUs]);
  end
  else if TimeMs < 60000.0 then
  begin
    // 1ms to 60s -> milliseconds with 3 decimals
    Result := Format('%.3f ms', [TimeMs]);
  end
  else if TimeMs < 120000.0 then
  begin
    // 1-2 minutes -> ms + (seconds)
    Seconds := TimeMs / 1000.0;
    Result := Format('%.3f ms (%.2f seconds)', [TimeMs, Seconds]);
  end
  else if TimeMs < 300000.0 then
  begin
    // 2-5 minutes -> seconds + (hh:mm:ss.nnn)
    Seconds := TimeMs / 1000.0;
    Hours := Trunc(Seconds) div 3600;
    Minutes := (Trunc(Seconds) mod 3600) div 60;
    Secs := Trunc(Seconds) mod 60;
    Millis := Round((Seconds - Trunc(Seconds)) * 1000);
    Result := Format('%.2f seconds (%02d:%02d:%02d.%03d)',
      [Seconds, Hours, Minutes, Secs, Millis]);
  end
  else
  begin
    // > 5 minutes -> hh:mm:ss.nnn directly
    Seconds := TimeMs / 1000.0;
    Hours := Trunc(Seconds) div 3600;
    Minutes := (Trunc(Seconds) mod 3600) div 60;
    Secs := Trunc(Seconds) mod 60;
    Millis := Round((Seconds - Trunc(Seconds)) * 1000);
    Result := Format('%02d:%02d:%02d.%03d', [Hours, Minutes, Secs, Millis]);
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
  WriteLn('Usage: sb <source.bas> [options]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --help              Show this help message');
  WriteLn('  --verbose           Show loading, lexing, parsing, and VM execution info');
  WriteLn('  --dump-ast          Show AST structure after parsing');
  WriteLn('  --disasm            Show bytecode disassembly');
  WriteLn('  --no-exec           Compile only, do not execute (useful with --disasm)');
  WriteLn('  --stats             Show execution statistics');
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

procedure TestBytecodeCompilation(const SourceFile: string;
  OptVerbose, OptDumpAST, OptDisasm, OptStats, OptNoExec: Boolean
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
  SSATime, CompileTime, ExecuteTime: Double;
  i, removed: Integer;
  ShowBanners: Boolean;
  {$IFDEF ENABLE_PROFILER}
  Profiler: TProfiler;
  ProfMode: TProfilerMode;
  ExportExt: string;
  {$ENDIF}
  {$IFNDEF DISABLE_REG_ALLOC}
  RegAlloc: TLinearScanAllocator;
  SpillCount: Integer;
  {$ENDIF}
begin
  // Determine if we should show banners (any option enabled)
  ShowBanners := OptVerbose or OptDumpAST or OptDisasm or OptStats or AnyDebugActive;

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

    if OptVerbose then
      WriteLn;

    // === LEXING ===
    if OptVerbose then
      WriteLn('=== LEXING ===');
    Lexer := TLexerFSM.Create;
    try
      Lexer.SetHasLineNumbers(True);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;

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
      try
        SSAProgram.RunDBE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Dead block elimination failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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

      // PHASE 3: Semi-Pruned SSA Construction with versioning
      {$IFNDEF DISABLE_SSA_CONSTRUCTION}
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
      try
        SSAProgram.RunGVN;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: GVN optimization failed: ', E.Message);
          WriteLn('Continuing with unoptimized SSA...');
        end;
      end;
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
      try
        SSAProgram.RunCSE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: CSE failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunAlgebraic;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Algebraic simplification failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunStrengthReduction;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Strength reduction failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunGosubInlining;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: GOSUB inlining failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunConstProp;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Constant propagation failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunCopyProp;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Copy propagation failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunLICM;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Loop-invariant code motion failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunDCE;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Dead code elimination failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      try
        SSAProgram.RunCopyCoalescing;
      except
        on E: Exception do
        begin
          WriteLn('ERROR: Copy coalescing failed: ', E.Message);
          WriteLn('Continuing...');
        end;
      end;
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
      RunPeephole(BytecodeProgram);
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

    // === SUPERINSTRUCTIONS ===
    // Fuses common instruction sequences into single superinstructions
    // Must run AFTER bytecode compilation and BEFORE VM execution
    {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
    {$IFDEF DEBUG_SUPERINSTR}
    if DebugSuperinstr then
      WriteLn('=== SUPERINSTRUCTIONS ===');
    {$ENDIF}
    try
      RunSuperinstructions(BytecodeProgram);
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
      RunPeephole(BytecodeProgram);
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
      RunRegisterCompaction(BytecodeProgram);
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
      // Cleanup without VM
      BytecodeProgram.Free;
      SSAProgram.Free;
      ParserResult.Free;
      Lexer.Free;
    end
    else
    begin
      // Execute the program
      if OptVerbose then
        WriteLn('=== VIRTUAL MACHINE EXECUTION ===');
      Output := TConsoleOutput.Create;
      Input := TConsoleInput.Create;
      VM := TBytecodeVM.Create;
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
      VM.SetInputDevice(Input);
      VM.LoadProgram(BytecodeProgram);

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
      except
        on E: Exception do
        begin
          Write('ERROR during VM execution at PC=', VM.PC);
          if (VM.PC >= 0) and (VM.PC < BytecodeProgram.GetInstructionCount) then
          begin
            with BytecodeProgram.GetInstruction(VM.PC) do
            begin
              if SourceLine > 0 then
                WriteLn(' (BASIC LINE ', SourceLine, '): ', E.ClassName, ': ', E.Message)
              else
                WriteLn(': ', E.ClassName, ': ', E.Message);
              WriteLn('Failing instruction: ', BytecodeOpToString(TBytecodeOp(OpCode)),
                      ' Dest=', Dest, ' Src1=', Src1, ' Src2=', Src2);
            end;
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
        WriteLn(Format('Compilation time: %s', [FormatTimeEx(CompileTime)]));
        WriteLn(Format('Execution time:   %s', [FormatTimeEx(ExecuteTime)]));
        WriteLn(Format('Total time:       %s', [FormatTimeEx(SSATime + CompileTime + ExecuteTime)]));
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

var
  TestFile: string;
  OptVerbose, OptDumpAST, OptDisasm, OptStats, OptHelp, OptNoExec: Boolean;
  {$IFDEF ENABLE_PROFILER}
  OptProfile: Boolean;
  ProfileMode: string;
  ProfileExport: string;
  {$ENDIF}
  i: Integer;
  Param: string;

begin
  try
    // Set console code page to UTF-8 for proper character encoding
    {$IFDEF WINDOWS}
    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);
    SetTextCodePage(Output, CP_UTF8);
    SetTextCodePage(Input, CP_UTF8);
    SetMultiByteConversionCodePage(CP_UTF8);
    DefaultSystemCodePage := CP_UTF8;
    {$ENDIF}

    // Initialize random number generator
    Randomize;

    // Initialize debug flags from command-line parameters
    InitDebugFlags;

    // Parse command-line parameters
    TestFile := '';
    OptVerbose := False;
    OptDumpAST := False;
    OptDisasm := False;
    OptStats := False;
    OptHelp := False;
    OptNoExec := False;
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
      else if Param = '--stats' then
        OptStats := True
      else if Param = '--no-exec' then
        OptNoExec := True
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
      else if (Pos('--', Param) <> 1) and (TestFile = '') then
        TestFile := ParamStr(i);  // Use original case for filename
    end;

    // Show help if requested or no file provided
    if OptHelp or (TestFile = '') then
    begin
      PrintHelp;
      Exit;
    end;

    TestBytecodeCompilation(TestFile, OptVerbose, OptDumpAST, OptDisasm, OptStats, OptNoExec
      {$IFDEF ENABLE_PROFILER}, OptProfile, ProfileMode, ProfileExport{$ENDIF});

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  end;
end.
