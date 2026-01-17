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
program SedaiVision;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiVision (sbv) - Graphical BASIC Console with SDL2

  This is the SDL2-based graphical version of SedaiBasic.
  It provides a retro-style console experience similar to classic home
  computers (C64, VIC-20, Spectrum, etc.)

  Usage: sbv <source.bas|program.basc> [options]

  Modes:
    --mode=retro-text   40x25 retro console (C64-style) [default]
    --mode=retro-gfx    40x25 with graphics support
    --mode=modern-text  80x50 modern console
    --mode=modern-gfx   Full resolution graphics
    --mode=c64          C64 preset (40x25, C64 font)
    --mode=spectrum     ZX Spectrum preset
    --mode=cpc          Amstrad CPC preset

  Exit codes:
    0 = Success
    1 = Error (file not found, compilation error, etc.)
  ============================================================================ }

// Require SDL2 for this program
{$DEFINE HAS_SDL2}

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
  SedaiExecutorContext, SedaiExecutorTypes, SedaiOutputInterface,
  // I/O Manager (for SDL2 modes)
  SedaiIOManager,
  // Runner and Serializer (for .basc support)
  SedaiRunner, SedaiBytecodeSerializer,
  // Interactive console
  SedaiGraphicsModes, SedaiNewConsole;

// Include version information (must be after uses, contains const declarations)
{$I Version.inc}

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
  WriteLn('SedaiBasic Vision (sbv) ver. ', SEDAIBASIC_VERSION, ' [', SEDAIBASIC_RELEASE_DATE, '] for ', GetSystemArchitecture);
  WriteLn(SEDAIBASIC_COPYRIGHT);
  WriteLn('SDL2 Graphical Console');
end;

var
  // Global option for TRUE value in comparisons (-1 = Commodore BASIC, 1 = modern BASIC)
  OptTrueValue: Int64 = -1;

{ Print help information }
procedure PrintHelp;
begin
  PrintVersion;
  WriteLn;
  WriteLn('Usage: sbv [source.bas|program.basc] [options]');
  WriteLn;
  WriteLn('When no file is specified, sbv starts in interactive mode with a');
  WriteLn('splash screen and READY. prompt, like classic home computers.');
  WriteLn;
  WriteLn('Supported file types:');
  WriteLn('  .bas       BASIC source code (compiled at runtime)');
  WriteLn('  .basc      Pre-compiled bytecode (faster startup)');
  WriteLn;
  WriteLn('Display modes:');
  WriteLn('  --mode=retro-text    40x25 retro console (default)');
  WriteLn('  --mode=retro-gfx     40x25 with graphics support');
  WriteLn('  --mode=modern-text   80x50 modern console');
  WriteLn('  --mode=modern-gfx    Full resolution graphics');
  WriteLn;
  WriteLn('Presets (shorthand for display modes):');
  WriteLn('  --mode=c64           Commodore 64 style');
  WriteLn('  --mode=spectrum      ZX Spectrum style');
  WriteLn('  --mode=cpc           Amstrad CPC style');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --help               Show this help message');
  WriteLn('  --autorun            Auto-run program after loading (interactive mode)');
  WriteLn('  --verbose            Show loading, lexing, parsing, and VM execution info');
  WriteLn('  --disasm             Show bytecode disassembly');
  WriteLn('  --no-exec            Compile only, do not execute');
  WriteLn('  --stats              Show execution statistics');
  WriteLn('  --fullscreen         Start in fullscreen mode');
  WriteLn('  --history-file=PATH  Use custom history file (default: ~/.sedai/.sbv_history)');
  WriteLn('  --true-value=N       Set TRUE value for comparisons (-1 or 1, default: -1)');
  WriteLn('                         -1 = Commodore BASIC style (default)');
  WriteLn('                          1 = Modern BASIC style');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  sbv                            Start interactive mode');
  WriteLn('  sbv program.bas                Run program in retro mode');
  WriteLn('  sbv program.bas --mode=c64     Run with C64 style console');
  WriteLn('  sbv program.basc               Run pre-compiled bytecode');
  WriteLn('  sbv program.bas --fullscreen   Run fullscreen');
  WriteLn('  sbv --help                     Show this help');
end;

{ Run BASIC program with SDL2 output }
procedure RunWithSDL2(const FileName: string;
  IOMode: TIOMode; OptVerbose, OptDisasm, OptStats, OptNoExec, OptFullscreen: Boolean
  {$IFDEF ENABLE_PROFILER}; OptProfile: Boolean; ProfileMode: string; ProfileExport: string{$ENDIF});
var
  Runner: TSedaiRunner;
  BytecodeProgram: TBytecodeProgram;
  VM: TBytecodeVM;
  Disassembler: TBytecodeDisassembler;
  IOManager: TIOManager;
  Output: IOutputDevice;
  Input: IInputDevice;
  Timer: THiResTimer;
  LoadTime, ExecuteTime: Double;
  FileType: TSedaiFileType;
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
  end;

  // Detect file type
  FileType := TSedaiRunner.DetectFileType(FileName);

  // Load/compile the program
  Runner := TSedaiRunner.Create;
  try
    Runner.Verbose := OptVerbose;

    Timer := CreateHiResTimer;
    try
      case FileType of
        sftSource:
          begin
            if OptVerbose then
              WriteLn('Compiling source: ', FileName);
            BytecodeProgram := Runner.LoadFromSource(FileName);
          end;
        sftBytecode:
          begin
            if OptVerbose then
              WriteLn('Loading bytecode: ', FileName);
            BytecodeProgram := Runner.LoadFromBytecode(FileName);
          end;
        else
          begin
            WriteLn('ERROR: Unknown file type: ', FileName);
            WriteLn('Supported extensions: .bas (source), .basc (bytecode)');
            Exit;
          end;
      end;
      LoadTime := Timer.ElapsedMilliseconds;
    except
      on E: Exception do
      begin
        WriteLn('ERROR: ', E.Message);
        Exit;
      end;
    end;

    if OptVerbose then
    begin
      WriteLn(Format('Loaded in %.2f ms', [LoadTime]));
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
      WriteLn('Compilation completed successfully.');
      WriteLn('Instructions: ', BytecodeProgram.GetInstructionCount);
      BytecodeProgram.Free;
    end
    else
    begin
      if OptVerbose then
        WriteLn('=== VIRTUAL MACHINE EXECUTION (SDL2) ===');

      // Create I/O manager with SDL2 mode
      IOManager := TIOManager.Create;
      try
        IOManager.SetMode(IOMode);
        IOManager.SetWindowTitle('SedaiBasic Vision');

        if OptVerbose then
          WriteLn('Display mode: ', IOManager.ModeToString(IOMode));

        // Create I/O devices
        Output := IOManager.CreateOutputDevice;
        Input := IOManager.CreateInputDevice;

        // Initialize display
        Output.Initialize('SedaiBasic Vision',
          IOManager.ConsoleWidth, IOManager.ConsoleHeight);

        VM := TBytecodeVM.Create;
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
          VM.SetInputDevice(Input);
          VM.TrueValue := OptTrueValue;  // Set TRUE value for comparisons
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
          except
            on E: Exception do
            begin
              WriteLn('ERROR during VM execution at PC=', VM.PC, ': ', E.Message);
              Exit;
            end;
          end;

          if OptStats then
          begin
            WriteLn;
            WriteLn('=== EXECUTION STATISTICS ===');
            {$IFDEF ENABLE_INSTRUCTION_COUNTING}
            WriteLn(Format('Instructions executed: %d', [VM.InstructionsExecuted]));
            {$ENDIF}
            WriteLn(Format('Load time:       %.3f ms', [LoadTime]));
            WriteLn(Format('Execution time:  %.3f ms', [ExecuteTime]));
            WriteLn(Format('Total time:      %.3f ms', [LoadTime + ExecuteTime]));
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

        // Cleanup I/O
        Output.Shutdown;

      finally
        IOManager.Free;
      end;
    end;

  finally
    Runner.Free;
  end;

  if ShowBanners then
  begin
    WriteLn;
    WriteLn('========================================');
    WriteLn('Execution complete!');
    WriteLn('========================================');
  end;
end;

{ Run interactive mode - splash screen, READY., blinking cursor }
procedure RunInteractiveMode(OptFullscreen: Boolean; const StartupFile: string = '';
  AutoRun: Boolean = False; const HistoryFile: string = '');
var
  Console: TSedaiNewConsole;
begin
  Console := TSedaiNewConsole.Create;
  try
    // Set custom history file before Initialize (which calls LoadHistory)
    if HistoryFile <> '' then
      Console.SetHistoryFile(HistoryFile);

    // Initialize with 80x50 text mode (GRAPHIC 8)
    if not Console.Initialize(gm80x50Text, OptFullscreen) then
    begin
      WriteLn('ERROR: Failed to initialize interactive console');
      ExitCode := 1;
      Exit;
    end;

    // Set startup file if specified
    if StartupFile <> '' then
      Console.SetStartupFile(StartupFile, AutoRun);

    // Run main loop (shows splash, READY., handles input)
    Console.Run;
  finally
    Console.Free;
  end;
end;

var
  TestFile: string;
  IOMode: TIOMode;
  ModeStr: string;
  OptVerbose, OptDisasm, OptStats, OptHelp, OptNoExec, OptFullscreen, OptAutoRun: Boolean;
  OptHistoryFile: string;
  HistoryDir: string;
  HistoryResponse: string;
  HistoryF: TextFile;
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
    IOMode := ioRetroText;  // Default to retro text mode
    OptVerbose := False;
    OptDisasm := False;
    OptStats := False;
    OptHelp := False;
    OptNoExec := False;
    OptFullscreen := False;
    OptAutoRun := False;
    OptHistoryFile := '';  // Use default path
    OptTrueValue := -1;  // Default: Commodore BASIC style (TRUE = -1)
    {$IFDEF ENABLE_PROFILER}
    OptProfile := False;
    ProfileMode := 'sampling';
    ProfileExport := '';
    {$ENDIF}

    i := 1;
    while i <= ParamCount do
    begin
      Param := ParamStr(i);

      if LowerCase(Param) = '--verbose' then
        OptVerbose := True
      else if LowerCase(Param) = '--disasm' then
        OptDisasm := True
      else if LowerCase(Param) = '--stats' then
        OptStats := True
      else if LowerCase(Param) = '--no-exec' then
        OptNoExec := True
      else if LowerCase(Param) = '--fullscreen' then
        OptFullscreen := True
      else if LowerCase(Param) = '--autorun' then
        OptAutoRun := True
      else if (LowerCase(Param) = '--help') or (Param = '-h') or (Param = '-?') then
        OptHelp := True
      else if Pos('--mode=', LowerCase(Param)) = 1 then
      begin
        ModeStr := LowerCase(Copy(Param, 8, Length(Param)));
        // Handle mode aliases
        if (ModeStr = 'c64') or (ModeStr = 'retro') then
          IOMode := ioRetroText
        else if ModeStr = 'retro-text' then
          IOMode := ioRetroText
        else if ModeStr = 'retro-gfx' then
          IOMode := ioRetroGfx
        else if ModeStr = 'modern-text' then
          IOMode := ioModernText
        else if ModeStr = 'modern-gfx' then
          IOMode := ioModernGfx
        else if (ModeStr = 'spectrum') or (ModeStr = 'cpc') then
          IOMode := ioRetroText  // Use retro for these presets
        else
        begin
          WriteLn('WARNING: Unknown mode "', ModeStr, '", using retro-text');
          IOMode := ioRetroText;
        end;
      end
      {$IFDEF ENABLE_PROFILER}
      else if LowerCase(Param) = '--profile' then
      begin
        OptProfile := True;
      end
      else if Pos('--profile=', LowerCase(Param)) = 1 then
      begin
        OptProfile := True;
        ProfileMode := Copy(Param, 11, Length(Param));
      end
      else if Pos('--profile-export=', LowerCase(Param)) = 1 then
        ProfileExport := Copy(Param, 18, Length(Param))
      {$ENDIF}
      else if Pos('--true-value=', LowerCase(Param)) = 1 then
      begin
        // Parse TRUE value: -1 (Commodore BASIC) or 1 (modern BASIC)
        OptTrueValue := StrToInt64Def(Copy(Param, 14, Length(Param)), -1);
        if (OptTrueValue <> -1) and (OptTrueValue <> 1) then
        begin
          WriteLn('WARNING: --true-value must be -1 or 1, using default -1');
          OptTrueValue := -1;
        end;
      end
      else if Pos('--history-file=', LowerCase(Param)) = 1 then
        OptHistoryFile := Copy(Param, 16, Length(Param))
      else if LowerCase(Param) = '--history-file' then
      begin
        // Next parameter is the history file path
        if i < ParamCount then
        begin
          Inc(i);
          OptHistoryFile := ParamStr(i);
        end
        else
          WriteLn('WARNING: --history-file requires a path argument');
      end
      else if (Pos('--', Param) <> 1) and (TestFile = '') then
        TestFile := Param;  // Use original case for filename

      Inc(i);
    end;

    // Show help if requested
    if OptHelp then
    begin
      PrintHelp;
      Exit;
    end;

    // Validate and create history file path if custom path specified
    if OptHistoryFile <> '' then
    begin
      HistoryDir := ExtractFilePath(OptHistoryFile);
      if HistoryDir <> '' then
      begin
        // Check if directory exists
        if not DirectoryExists(HistoryDir) then
        begin
          Write('History directory does not exist: ', HistoryDir);
          WriteLn;
          Write('Create it? (Y/N): ');
          ReadLn(HistoryResponse);
          if (LowerCase(HistoryResponse) = 'y') or (LowerCase(HistoryResponse) = 'yes') then
          begin
            if ForceDirectories(HistoryDir) then
              WriteLn('Directory created: ', HistoryDir)
            else
            begin
              WriteLn('ERROR: Failed to create directory: ', HistoryDir);
              WriteLn('Using default history path.');
              OptHistoryFile := '';  // Fallback to default
            end;
          end
          else
          begin
            WriteLn('Using default history path.');
            OptHistoryFile := '';  // Fallback to default
          end;
        end;
      end;

      // If directory exists (or was created), create empty file if needed
      if (OptHistoryFile <> '') and not FileExists(OptHistoryFile) then
      begin
        try
          AssignFile(HistoryF, OptHistoryFile);
          Rewrite(HistoryF);
          CloseFile(HistoryF);
          WriteLn('Created empty history file: ', OptHistoryFile);
        except
          on E: Exception do
          begin
            WriteLn('WARNING: Could not create history file: ', E.Message);
            WriteLn('Using default history path.');
            OptHistoryFile := '';  // Fallback to default
          end;
        end;
      end;
    end;

    // No file specified -> interactive mode without startup file
    if TestFile = '' then
    begin
      RunInteractiveMode(OptFullscreen, '', False, OptHistoryFile);
      Exit;
    end;

    // Check if file exists
    if not FileExists(TestFile) then
    begin
      WriteLn('ERROR: File not found: ', TestFile);
      ExitCode := 1;
      Exit;
    end;

    // Run interactive mode with startup file
    // This loads the file and optionally auto-runs it
    RunInteractiveMode(OptFullscreen, TestFile, OptAutoRun, OptHistoryFile);

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      WriteLn;
      ExitCode := 1;
    end;
  end;
end.
