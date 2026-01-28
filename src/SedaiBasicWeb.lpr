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
program SedaiBasicWeb;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}
{$DEFINE WEB_MODE}

// Include shared optimization flags
{$I OptimizationFlags.inc}
// Include debug flags (compile-time control of debug code)
{$I DebugFlags.inc}

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Variants, TypInfo, Math,
  // HTTP Server
  fphttpserver,
  // Lexer/Parser
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser, SedaiDateTimeUtils,
  // Bytecode VM
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeTypes, SedaiBytecodeCompiler, SedaiBytecodeVM,
  // Register Allocation
  SedaiRegAlloc,
  // Peephole and Superinstructions
  SedaiPeephole, SedaiSuperinstructions,
  // NOP Compaction
  SedaiNopCompaction,
  // Register Compaction
  SedaiRegisterCompaction,
  // Debug runtime flags
  SedaiDebug,
  // Executor Context
  SedaiExecutorContext, SedaiExecutorTypes, SedaiOutputInterface,
  // Web I/O (to be created)
  SedaiWebIO, SedaiWebServer;

// Include version information
{$I Version.inc}

var
  Server: TSedaiWebServer;
  Port: Word;
  Host: string;
  BaseDir: string;
  Verbose: Boolean;
  UseCache: Boolean;
  AllowDirListing: Boolean;
  i: Integer;
  Param, ParamLower: string;

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
  WriteLn('SedaiBasic Web Server ver. ', SEDAIBASIC_VERSION, ' [', SEDAIBASIC_RELEASE_DATE, '] for ', GetSystemArchitecture);
  WriteLn(SEDAIBASIC_COPYRIGHT);
  WriteLn(SEDAIBASIC_LICENSE);
end;

{ Print help information }
procedure PrintHelp;
begin
  PrintVersion;
  WriteLn;
  WriteLn('Usage: sbw --port <port> --basedir <path> [options]');
  WriteLn;
  WriteLn('Required options:');
  WriteLn('  --basedir <path>    Directory containing .wbas scripts');
  WriteLn;
  WriteLn('Optional:');
  WriteLn('  --port <number>     HTTP port (default: 8080)');
  WriteLn('  --host <address>    Bind address (default: 0.0.0.0)');
  WriteLn('  --cache             Enable bytecode caching');
  WriteLn('  --verbose           Enable verbose logging');
  WriteLn('  --no-dir-listing    Disable directory listing (403 Forbidden)');
  WriteLn('  --help              Show this help message');
  WriteLn;
  WriteLn('File extension:');
  WriteLn('  .wbas               Web BASIC scripts (executed via HTTP)');
  WriteLn;
  WriteLn('URL mapping:');
  WriteLn('  GET /hello.wbas         -> {basedir}/hello.wbas');
  WriteLn('  GET /admin/login.wbas   -> {basedir}/admin/login.wbas');
  WriteLn('  GET /api/users.wbas?id=5 -> {basedir}/api/users.wbas');
  WriteLn('  GET /                   -> directory listing or index.wbas');
  WriteLn('  GET /subdir/            -> directory listing or subdir/index.wbas');
  WriteLn;
  WriteLn('Web BASIC instructions:');
  WriteLn('  GET$("name")        Query parameter (HTML-escaped, safe)');
  WriteLn('  POST$("name")       POST parameter (HTML-escaped, safe)');
  WriteLn('  GETRAW$("name")     Raw query parameter (unsafe)');
  WriteLn('  POSTRAW$("name")    Raw POST parameter (unsafe)');
  WriteLn('  HTML$(s)            Escape HTML entities');
  WriteLn('  URL$(s)             URL encode string');
  WriteLn('  METHOD$             "GET" or "POST"');
  WriteLn('  PATH$               Requested path');
  WriteLn('  QUERY$              Full query string');
  WriteLn('  HEADER$("name")     HTTP request header');
  WriteLn('  SETHEADER n, v      Set response header');
  WriteLn('  STATUS code         Set HTTP status code');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  sbw --port 8080 --basedir C:\www\scripts');
  WriteLn('  sbw --port 3000 --basedir ./scripts --cache --verbose');
end;

begin
  try
    // Set console code page to UTF-8
    {$IFDEF WINDOWS}
    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);
    SetTextCodePage(Output, CP_UTF8);
    SetTextCodePage(Input, CP_UTF8);
    SetMultiByteConversionCodePage(CP_UTF8);
    DefaultSystemCodePage := CP_UTF8;
    {$ENDIF}

    // Initialize debug flags
    InitDebugFlags;

    // Default values
    Port := 8080;
    Host := '0.0.0.0';
    BaseDir := '';
    Verbose := False;
    UseCache := False;
    AllowDirListing := True;

    // Parse command-line parameters
    i := 1;
    while i <= ParamCount do
    begin
      Param := ParamStr(i);
      ParamLower := LowerCase(Param);

      if (ParamLower = '--help') or (ParamLower = '-h') or (ParamLower = '-?') then
      begin
        PrintHelp;
        Exit;
      end
      else if ParamLower = '--port' then
      begin
        Inc(i);
        if i <= ParamCount then
          Port := StrToIntDef(ParamStr(i), 8080);
      end
      else if ParamLower = '--host' then
      begin
        Inc(i);
        if i <= ParamCount then
          Host := ParamStr(i);
      end
      else if ParamLower = '--basedir' then
      begin
        Inc(i);
        if i <= ParamCount then
          BaseDir := ParamStr(i);
      end
      else if ParamLower = '--verbose' then
        Verbose := True
      else if ParamLower = '--cache' then
        UseCache := True
      else if ParamLower = '--no-dir-listing' then
        AllowDirListing := False;

      Inc(i);
    end;

    // Validate required parameters
    if BaseDir = '' then
    begin
      WriteLn('ERROR: --basedir is required');
      WriteLn;
      PrintHelp;
      ExitCode := 1;
      Exit;
    end;

    if not DirectoryExists(BaseDir) then
    begin
      WriteLn('ERROR: Base directory does not exist: ', BaseDir);
      ExitCode := 1;
      Exit;
    end;

    // Print startup banner
    PrintVersion;
    WriteLn;
    WriteLn('Starting HTTP server...');
    WriteLn('  Host:       ', Host);
    WriteLn('  Port:       ', Port);
    WriteLn('  BaseDir:    ', BaseDir);
    WriteLn('  Cache:      ', BoolToStr(UseCache, 'enabled', 'disabled'));
    WriteLn('  Verbose:    ', BoolToStr(Verbose, 'enabled', 'disabled'));
    WriteLn('  DirListing: ', BoolToStr(AllowDirListing, 'enabled', 'disabled'));
    WriteLn;

    // Create and start server
    Server := TSedaiWebServer.Create(Host, Port, BaseDir, UseCache, Verbose, AllowDirListing);
    try
      WriteLn('Server is running. Press Ctrl+C to stop.');
      WriteLn('Access at: http://', Host, ':', Port, '/');
      WriteLn;

      // Run server (blocking)
      Server.Run;
    finally
      Server.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
