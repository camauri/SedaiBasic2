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
unit SedaiWebServer;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fgl,
  // SedaiBasic core
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults,
  SedaiPackratParser,
  SedaiSSATypes, SedaiSSA,
  SedaiBytecodeTypes, SedaiBytecodeCompiler, SedaiBytecodeVM,
  SedaiRegAlloc,
  SedaiPeephole, SedaiSuperinstructions, SedaiNopCompaction, SedaiRegisterCompaction,
  SedaiExecutorContext, SedaiExecutorTypes, SedaiOutputInterface,
  // Web I/O
  SedaiWebIO;

type
  { Bytecode cache entry }
  TBytecodeCache = specialize TFPGMap<string, TBytecodeProgram>;

  { TSedaiWebServer - HTTP server for Web BASIC scripts }
  TSedaiWebServer = class
  private
    FServer: TFPHttpServer;
    FHost: string;
    FPort: Word;
    FBaseDir: string;
    FUseCache: Boolean;
    FVerbose: Boolean;
    FCache: TBytecodeCache;
    FRunning: Boolean;
    FAllowDirListing: Boolean;

    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function CompileScript(const ScriptPath: string): TBytecodeProgram;
    function ExecuteScript(BytecodeProgram: TBytecodeProgram;
      const WebContext: TWebContext): string;
    procedure SendError(var AResponse: TFPHTTPConnectionResponse;
      StatusCode: Integer; const Message: string);
    procedure SendDirectoryListing(var AResponse: TFPHTTPConnectionResponse;
      const DirPath: string; const RequestPath: string);
    procedure Log(const Msg: string);
    function SanitizePath(const RequestPath: string): string;
    function IsValidWbasPath(const Path: string): Boolean;
  public
    constructor Create(const AHost: string; APort: Word; const ABaseDir: string;
      AUseCache: Boolean; AVerbose: Boolean; AAllowDirListing: Boolean = True);
    destructor Destroy; override;

    procedure Run;
    procedure Stop;

    property Host: string read FHost;
    property Port: Word read FPort;
    property BaseDir: string read FBaseDir;
    property UseCache: Boolean read FUseCache;
    property Verbose: Boolean read FVerbose;
    property AllowDirListing: Boolean read FAllowDirListing;
  end;

implementation

uses
  StrUtils, DateUtils;

{ TSedaiWebServer }

constructor TSedaiWebServer.Create(const AHost: string; APort: Word;
  const ABaseDir: string; AUseCache: Boolean; AVerbose: Boolean;
  AAllowDirListing: Boolean = True);
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FBaseDir := IncludeTrailingPathDelimiter(ExpandFileName(ABaseDir));
  FUseCache := AUseCache;
  FVerbose := AVerbose;
  FAllowDirListing := AAllowDirListing;
  FRunning := False;

  // Initialize cache
  FCache := TBytecodeCache.Create;

  // Create HTTP server
  FServer := TFPHttpServer.Create(nil);
  FServer.Port := FPort;
  FServer.OnRequest := @HandleRequest;
  FServer.Threaded := False;  // Single-threaded for now
end;

destructor TSedaiWebServer.Destroy;
var
  i: Integer;
begin
  // Free cached bytecode programs
  for i := 0 to FCache.Count - 1 do
    FCache.Data[i].Free;
  FCache.Free;

  FServer.Free;
  inherited Destroy;
end;

procedure TSedaiWebServer.Log(const Msg: string);
begin
  if FVerbose then
    WriteLn('[', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), '] ', Msg);
end;

function TSedaiWebServer.SanitizePath(const RequestPath: string): string;
var
  Parts: TStringArray;
  i: Integer;
  CleanParts: TStringList;
begin
  // Remove leading slash and split by /
  Result := RequestPath;
  if (Length(Result) > 0) and (Result[1] = '/') then
    Delete(Result, 1, 1);

  // Split path into parts
  Parts := Result.Split(['/']);
  CleanParts := TStringList.Create;
  try
    for i := 0 to High(Parts) do
    begin
      // Skip empty parts and parent directory traversal
      if (Parts[i] = '') or (Parts[i] = '.') then
        Continue;
      if Parts[i] = '..' then
      begin
        // Remove last part if exists (but don't go above root)
        if CleanParts.Count > 0 then
          CleanParts.Delete(CleanParts.Count - 1);
        Continue;
      end;
      // Add valid path component
      CleanParts.Add(Parts[i]);
    end;

    // Rebuild path
    Result := '';
    for i := 0 to CleanParts.Count - 1 do
    begin
      if i > 0 then
        Result := Result + PathDelim;
      Result := Result + CleanParts[i];
    end;
  finally
    CleanParts.Free;
  end;
end;

function TSedaiWebServer.IsValidWbasPath(const Path: string): Boolean;
begin
  // Must end with .wbas
  Result := LowerCase(ExtractFileExt(Path)) = '.wbas';
  // Must not contain null bytes
  if Result then
    Result := Pos(#0, Path) = 0;
end;

procedure TSedaiWebServer.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  RequestPath: string;
  ScriptPath: string;
  SafePath: string;
  FullPath: string;
  BytecodeProgram: TBytecodeProgram;
  WebContext: TWebContext;
  ResponseBody: string;
  CacheIdx: Integer;
  FileTime: TDateTime;
  IndexPath: string;
begin
  // Get request path (URL decoded)
  RequestPath := ARequest.PathInfo;

  Log(Format('%s %s from %s', [ARequest.Method, RequestPath, ARequest.RemoteAddress]));

  // Sanitize path to prevent directory traversal
  SafePath := SanitizePath(RequestPath);

  // Build full path
  FullPath := FBaseDir + SafePath;

  // Check if it's a directory
  if DirectoryExists(FullPath) then
  begin
    // Check for index.wbas in the directory
    IndexPath := IncludeTrailingPathDelimiter(FullPath) + 'index.wbas';
    if FileExists(IndexPath) then
    begin
      // Serve index.wbas
      ScriptPath := IndexPath;
      // Continue to script execution below
    end
    else
    begin
      // Serve directory listing if allowed
      if FAllowDirListing then
      begin
        SendDirectoryListing(AResponse, FullPath, RequestPath);
        Exit;
      end
      else
      begin
        SendError(AResponse, 403, 'Forbidden: Directory listing not allowed');
        Exit;
      end;
    end;
  end
  else
  begin
    // Validate .wbas extension for files
    if not IsValidWbasPath(SafePath) then
    begin
      SendError(AResponse, 404, 'Not Found: Only .wbas files can be executed');
      Exit;
    end;

    // Build full script path
    ScriptPath := FullPath;

    // Check if file exists
    if not FileExists(ScriptPath) then
    begin
      SendError(AResponse, 404, Format('Not Found: %s', [SafePath]));
      Exit;
    end;
  end;

  // Verify the script is within BaseDir (extra security check)
  if Pos(FBaseDir, ExpandFileName(ScriptPath)) <> 1 then
  begin
    SendError(AResponse, 403, 'Forbidden: Access denied');
    Exit;
  end;

  try
    // Compile or get from cache
    if FUseCache then
    begin
      CacheIdx := FCache.IndexOf(ScriptPath);
      if CacheIdx >= 0 then
      begin
        BytecodeProgram := FCache.Data[CacheIdx];
        // TODO: Check file modification time for cache invalidation
        Log('Cache hit: ' + SafePath);
      end
      else
      begin
        BytecodeProgram := CompileScript(ScriptPath);
        if BytecodeProgram = nil then
        begin
          SendError(AResponse, 500, 'Internal Server Error: Compilation failed');
          Exit;
        end;
        FCache.Add(ScriptPath, BytecodeProgram);
        Log('Cache miss, compiled: ' + SafePath);
      end;
    end
    else
    begin
      BytecodeProgram := CompileScript(ScriptPath);
      if BytecodeProgram = nil then
      begin
        SendError(AResponse, 500, 'Internal Server Error: Compilation failed');
        Exit;
      end;
    end;

    // Create web context from request
    WebContext := TWebContext.Create;
    try
      WebContext.Method := ARequest.Method;
      WebContext.Path := RequestPath;
      WebContext.QueryString := ARequest.QueryString;
      WebContext.ParseQueryString(ARequest.QueryString);
      WebContext.ParsePostData(ARequest.Content);
      // Copy request headers
      // Note: fphttpserver provides headers via ARequest.CustomHeaders

      // Execute script
      ResponseBody := ExecuteScript(BytecodeProgram, WebContext);

      // Set response
      AResponse.Code := WebContext.ResponseStatus;
      AResponse.CodeText := GetStatusCodeText(WebContext.ResponseStatus);

      // Set response headers from context
      WebContext.ApplyResponseHeaders(AResponse);

      // Set default Content-Type if not set
      if AResponse.ContentType = '' then
        AResponse.ContentType := 'text/html; charset=utf-8';

      AResponse.Content := ResponseBody;

    finally
      WebContext.Free;
    end;

    // Free bytecode if not cached
    if not FUseCache then
      BytecodeProgram.Free;

  except
    on E: Exception do
    begin
      Log('ERROR: ' + E.Message);
      SendError(AResponse, 500, 'Internal Server Error: ' + E.Message);
    end;
  end;
end;

function TSedaiWebServer.CompileScript(const ScriptPath: string): TBytecodeProgram;
var
  Source: TStringList;
  Lexer: TLexerFSM;
  Parser: TPackratParser;
  TokenList: TTokenList;
  ParserResult: TParsingResult;
  SSAGen: TSSAGenerator;
  SSAProgram: TSSAProgram;
  Compiler: TBytecodeCompiler;
  {$IFNDEF DISABLE_REG_ALLOC}
  RegAlloc: TLinearScanAllocator;
  {$ENDIF}
begin
  Result := nil;

  Source := TStringList.Create;
  try
    Source.LoadFromFile(ScriptPath);

    // Lexing
    Lexer := TLexerFSM.Create;
    try
      Lexer.SetHasLineNumbers(True);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := Source.Text;
      TokenList := Lexer.ScanAllTokensFast;
    except
      on E: Exception do
      begin
        Log('Lexer error: ' + E.Message);
        Lexer.Free;
        Exit;
      end;
    end;

    // Parsing
    Parser := CreatePackratParser;
    try
      ParserResult := Parser.Parse(TokenList);
      if not ParserResult.Success then
      begin
        if ParserResult.Errors.Count > 0 then
          Log('Parser error: ' + ParserResult.Errors[0].ToString);
        Parser.Free;
        Lexer.Free;
        Exit;
      end;
    finally
      Parser.Free;
    end;

    // SSA Generation
    SSAGen := TSSAGenerator.Create;
    try
      SSAProgram := SSAGen.Generate(ParserResult.AST);
      if not Assigned(SSAProgram) then
      begin
        Log('SSA generation failed');
        SSAGen.Free;
        ParserResult.Free;
        Lexer.Free;
        Exit;
      end;

      // Run optimization passes (simplified for web)
      {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
      try
        {$IFNDEF DISABLE_DBE}
        SSAProgram.RunDBE;
        {$ENDIF}
        {$IFNDEF DISABLE_DOMINATOR_TREE}
        SSAProgram.BuildDominatorTree;
        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        SSAProgram.RunSSAConstruction;
        {$ENDIF}
        {$ENDIF}
        {$IFNDEF DISABLE_CONST_PROP}
        SSAProgram.RunConstProp;
        {$ENDIF}
        {$IFNDEF DISABLE_COPY_PROP}
        SSAProgram.RunCopyProp;
        {$ENDIF}
        {$IFNDEF DISABLE_DCE}
        SSAProgram.RunDCE;
        {$ENDIF}
        {$IFNDEF DISABLE_PHI_ELIM}
        {$IFNDEF DISABLE_SSA_CONSTRUCTION}
        SSAProgram.RunPhiElimination;
        {$ENDIF}
        {$ENDIF}
        {$IFNDEF DISABLE_COPY_COAL}
        SSAProgram.RunCopyCoalescing;
        {$ENDIF}
        {$IFNDEF DISABLE_REG_ALLOC}
        RegAlloc := TLinearScanAllocator.Create(SSAProgram);
        try
          RegAlloc.Run;
        finally
          RegAlloc.Free;
        end;
        {$ENDIF}
      except
        on E: Exception do
          Log('Optimization warning: ' + E.Message);
      end;
      {$ENDIF}

      // Bytecode Compilation
      Compiler := TBytecodeCompiler.Create;
      try
        Result := Compiler.Compile(SSAProgram);
        if Assigned(Result) then
        begin
          // Post-compilation optimizations
          {$IFNDEF DISABLE_ALL_OPTIMIZATIONS}
          {$IFNDEF DISABLE_PEEPHOLE}
          try RunPeephole(Result); except end;
          {$ENDIF}
          {$IFNDEF DISABLE_SUPERINSTRUCTIONS}
          try RunSuperinstructions(Result); except end;
          {$ENDIF}
          {$IFNDEF DISABLE_NOP_COMPACTION}
          try RunNopCompaction(Result); except end;
          {$ENDIF}
          {$ENDIF}
        end;
      finally
        Compiler.Free;
      end;

    finally
      SSAGen.Free;
      SSAProgram.Free;
    end;

    ParserResult.Free;
    Lexer.Free;

  finally
    Source.Free;
  end;
end;

function TSedaiWebServer.ExecuteScript(BytecodeProgram: TBytecodeProgram;
  const WebContext: TWebContext): string;
var
  VM: TBytecodeVM;
  Output: TWebOutput;
begin
  Result := '';

  Output := TWebOutput.Create(WebContext);
  try
    VM := TBytecodeVM.Create;
    try
      VM.SetOutputDevice(Output);
      VM.SetInputDevice(nil);  // No input device for web (INPUT is not supported)
      VM.LoadProgram(BytecodeProgram);

      // Set web context for VM to access GET$/POST$/etc.
      VM.SetWebContext(WebContext);

      try
        VM.RunFast;
      except
        on E: Exception do
        begin
          Log('VM execution error: ' + E.Message);
          Result := '<html><body><h1>500 Internal Server Error</h1><p>' +
                    HtmlEncode(E.Message) + '</p></body></html>';
          WebContext.ResponseStatus := 500;
          Exit;
        end;
      end;

      // Get accumulated output
      Result := Output.GetContent;

    finally
      VM.Free;
    end;
  finally
    Output.Free;
  end;
end;

procedure TSedaiWebServer.SendError(var AResponse: TFPHTTPConnectionResponse;
  StatusCode: Integer; const Message: string);
begin
  AResponse.Code := StatusCode;
  AResponse.CodeText := GetStatusCodeText(StatusCode);
  AResponse.ContentType := 'text/html; charset=utf-8';
  AResponse.Content := Format(
    '<html><head><title>%d %s</title></head><body>' +
    '<h1>%d %s</h1><p>%s</p>' +
    '<hr><p><em>SedaiBasic Web Server</em></p>' +
    '</body></html>',
    [StatusCode, GetStatusCodeText(StatusCode),
     StatusCode, GetStatusCodeText(StatusCode),
     HtmlEncode(Message)]);
  Log(Format('Error %d: %s', [StatusCode, Message]));
end;

procedure TSedaiWebServer.SendDirectoryListing(var AResponse: TFPHTTPConnectionResponse;
  const DirPath: string; const RequestPath: string);
var
  SR: TSearchRec;
  Content: TStringBuilder;
  DisplayPath: string;
  ParentPath: string;
  FileName: string;
  FileSize: Int64;
  FileDate: TDateTime;
  FileSizeStr: string;
  IsDir: Boolean;
  Entries: TStringList;
  i: Integer;
begin
  // Build display path (URL path)
  DisplayPath := RequestPath;
  if DisplayPath = '' then
    DisplayPath := '/';
  if (Length(DisplayPath) > 0) and (DisplayPath[Length(DisplayPath)] <> '/') then
    DisplayPath := DisplayPath + '/';

  Content := TStringBuilder.Create;
  Entries := TStringList.Create;
  try
    // HTML header
    Content.Append('<!DOCTYPE html>');
    Content.Append('<html><head>');
    Content.Append('<meta charset="utf-8">');
    Content.AppendFormat('<title>Index of %s</title>', [HtmlEncode(DisplayPath)]);
    Content.Append('<style>');
    Content.Append('body { font-family: monospace; margin: 20px; }');
    Content.Append('h1 { border-bottom: 1px solid #ccc; padding-bottom: 10px; }');
    Content.Append('table { border-collapse: collapse; width: 100%; }');
    Content.Append('th, td { text-align: left; padding: 8px; border-bottom: 1px solid #eee; }');
    Content.Append('th { background: #f5f5f5; }');
    Content.Append('a { text-decoration: none; color: #0066cc; }');
    Content.Append('a:hover { text-decoration: underline; }');
    Content.Append('.dir { font-weight: bold; }');
    Content.Append('.size { text-align: right; }');
    Content.Append('</style>');
    Content.Append('</head><body>');
    Content.AppendFormat('<h1>Index of %s</h1>', [HtmlEncode(DisplayPath)]);

    // Table header
    Content.Append('<table>');
    Content.Append('<tr><th>Name</th><th>Size</th><th>Last Modified</th></tr>');

    // Parent directory link (if not at root)
    if (RequestPath <> '') and (RequestPath <> '/') then
    begin
      ParentPath := ExcludeTrailingPathDelimiter(RequestPath);
      i := LastDelimiter('/', ParentPath);
      if i > 0 then
        ParentPath := Copy(ParentPath, 1, i)
      else
        ParentPath := '/';
      Content.AppendFormat('<tr><td class="dir"><a href="%s">..</a></td><td>-</td><td>-</td></tr>',
        [HtmlEncode(ParentPath)]);
    end;

    // Collect directory entries
    if FindFirst(IncludeTrailingPathDelimiter(DirPath) + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          // Skip . and ..
          if (SR.Name = '.') or (SR.Name = '..') then
            Continue;

          IsDir := (SR.Attr and faDirectory) <> 0;
          FileName := SR.Name;
          FileSize := SR.Size;
          FileDate := FileDateToDateTime(SR.Time);

          // Format file size
          if IsDir then
            FileSizeStr := '-'
          else if FileSize < 1024 then
            FileSizeStr := Format('%d B', [FileSize])
          else if FileSize < 1024 * 1024 then
            FileSizeStr := Format('%.1f KB', [FileSize / 1024])
          else if FileSize < 1024 * 1024 * 1024 then
            FileSizeStr := Format('%.1f MB', [FileSize / (1024 * 1024)])
          else
            FileSizeStr := Format('%.1f GB', [FileSize / (1024 * 1024 * 1024)]);

          // Store entry for sorting (directories first, then files)
          if IsDir then
            Entries.Add(Format('0|%s|%s|%s', [FileName, FileSizeStr, FormatDateTime('yyyy-mm-dd hh:nn:ss', FileDate)]))
          else
            Entries.Add(Format('1|%s|%s|%s', [FileName, FileSizeStr, FormatDateTime('yyyy-mm-dd hh:nn:ss', FileDate)]));
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;

    // Sort entries (directories first, then alphabetically)
    Entries.Sort;

    // Output entries
    for i := 0 to Entries.Count - 1 do
    begin
      // Parse entry: type|name|size|date
      IsDir := Entries[i][1] = '0';
      FileName := Copy(Entries[i], 3, Pos('|', Copy(Entries[i], 3, MaxInt)) - 1);
      FileSizeStr := Copy(Entries[i], 3 + Length(FileName) + 1,
        Pos('|', Copy(Entries[i], 3 + Length(FileName) + 2, MaxInt)) - 1);

      if IsDir then
      begin
        Content.AppendFormat('<tr><td class="dir"><a href="%s%s/">%s/</a></td><td class="size">%s</td><td>%s</td></tr>',
          [HtmlEncode(DisplayPath), HtmlEncode(FileName), HtmlEncode(FileName), FileSizeStr,
           Copy(Entries[i], Length(Entries[i]) - 18, 19)]);
      end
      else
      begin
        Content.AppendFormat('<tr><td><a href="%s%s">%s</a></td><td class="size">%s</td><td>%s</td></tr>',
          [HtmlEncode(DisplayPath), HtmlEncode(FileName), HtmlEncode(FileName), FileSizeStr,
           Copy(Entries[i], Length(Entries[i]) - 18, 19)]);
      end;
    end;

    Content.Append('</table>');
    Content.Append('<hr><p><em>SedaiBasic Web Server</em></p>');
    Content.Append('</body></html>');

    // Send response
    AResponse.Code := 200;
    AResponse.CodeText := 'OK';
    AResponse.ContentType := 'text/html; charset=utf-8';
    AResponse.Content := Content.ToString;

    Log(Format('Directory listing: %s', [RequestPath]));

  finally
    Content.Free;
    Entries.Free;
  end;
end;

procedure TSedaiWebServer.Run;
begin
  FRunning := True;
  try
    FServer.Active := True;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Failed to start server: ', E.Message);
      FRunning := False;
    end;
  end;
end;

procedure TSedaiWebServer.Stop;
begin
  FRunning := False;
  FServer.Active := False;
end;

{ Helper function to get HTTP status code text }
function GetStatusCodeText(Code: Integer): string;
begin
  case Code of
    200: Result := 'OK';
    201: Result := 'Created';
    204: Result := 'No Content';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    304: Result := 'Not Modified';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
  else
    Result := 'Unknown';
  end;
end;

{ HTML encode helper }
function HtmlEncode(const S: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    case S[i] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
    else
      Result := Result + S[i];
    end;
end;

end.
