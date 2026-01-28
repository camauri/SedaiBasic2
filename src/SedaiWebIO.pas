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
unit SedaiWebIO;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, httpdefs, fphttpserver,
  SedaiOutputInterface, SedaiGraphicsTypes;

type
  { Dictionary for query/post parameters }
  TStringMap = specialize TFPGMap<string, string>;

  { TWebContext - Holds HTTP request context for script execution }
  TWebContext = class
  private
    FMethod: string;
    FPath: string;
    FQueryString: string;
    FGetParams: TStringMap;
    FPostParams: TStringMap;
    FRequestHeaders: TStringMap;
    FResponseHeaders: TStringMap;
    FResponseStatus: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Parse query string into GetParams
    procedure ParseQueryString(const QueryStr: string);
    // Parse POST data (application/x-www-form-urlencoded)
    procedure ParsePostData(const PostData: string);
    // Set a request header
    procedure SetRequestHeader(const Name, Value: string);
    // Set a response header
    procedure SetResponseHeader(const Name, Value: string);

    // Get parameter with HTML escaping (safe)
    function GetParam(const Name: string): string;
    function PostParam(const Name: string): string;
    // Get parameter without escaping (raw, unsafe)
    function GetParamRaw(const Name: string): string;
    function PostParamRaw(const Name: string): string;
    // Get request header
    function GetHeader(const Name: string): string;
    // Apply response headers to HTTP response
    procedure ApplyResponseHeaders(var Response: TFPHTTPConnectionResponse);

    property Method: string read FMethod write FMethod;
    property Path: string read FPath write FPath;
    property QueryString: string read FQueryString write FQueryString;
    property ResponseStatus: Integer read FResponseStatus write FResponseStatus;
    property GetParams: TStringMap read FGetParams;
    property PostParams: TStringMap read FPostParams;
    property RequestHeaders: TStringMap read FRequestHeaders;
    property ResponseHeaders: TStringMap read FResponseHeaders;
  end;

  { TWebOutput - Output device that accumulates output for HTTP response }
  TWebOutput = class(TInterfacedObject, IOutputDevice)
  private
    FContent: TStringBuilder;
    FContext: TWebContext;
    FInitialized: Boolean;
    FCursorX, FCursorY: Integer;
    FFastMode: Boolean;
    FFastModeAlpha: Byte;
  public
    constructor Create(AContext: TWebContext);
    destructor Destroy; override;

    // IOutputDevice implementation - Core functionality
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

    // Graphics support - stub implementations (not used in web mode)
    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False;
                           SplitLine: Integer = -1): Boolean;
    function GetGraphicMode: TGraphicMode;
    function IsInGraphicsMode: Boolean;
    procedure ClearScreen(Mode: Integer);

    // Pixel operations - stub implementations
    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload;
    function GetPixel(X, Y: Integer): UInt32;
    function GetPixelIndex(X, Y: Integer): TPaletteIndex;

    // Palette management - stub implementations
    procedure EnablePalette(Enable: Boolean);
    function IsPaletteEnabled: Boolean;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
    procedure SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte = 255);
    function GetPaletteColor(Index: TPaletteIndex): UInt32;
    procedure ResetPalette;

    // Palette file operations - stub implementations
    function LoadPaletteFromJSON(const FileName: string): Boolean;
    function SavePaletteToJSON(const FileName: string): Boolean;
    function GetLastPaletteError: string;

    // Shape drawing - stub implementations
    procedure DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double = 0);
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0);
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False);
    procedure DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                  SA: Double = 0; EA: Double = 360;
                                  Angle: Double = 0; Inc: Double = 2);
    procedure DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);

    // Pixel Cursor - stub implementations
    procedure SetPixelCursor(X, Y: Integer);
    function GetPixelCursorX: Integer;
    function GetPixelCursorY: Integer;

    // Color sources - stub implementations
    procedure SetColorSource(Source, Color: Integer);
    procedure SetColorSourceDirect(Source, Color: Integer);
    function GetColorSourceDirect(Source: Integer): Integer;

    // Line width - stub implementation
    procedure SetLineWidth(Width: Integer);

    // Coordinate scaling - stub implementation
    procedure SetScale(Enabled: Boolean; XMax, YMax: Integer);

    // Flood fill - stub implementation
    procedure FloodFill(Source: Integer; X, Y: Double; Mode: Integer);

    // Text window - stub implementations
    procedure SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
    function GetWindowLines: Integer;
    function GetWindowCols: Integer;
    function GetScreenWidth: Integer;

    // Screen memory access - stub implementations
    function GetCharAt(Col, Row: Integer): Byte;
    procedure SetCharAt(Col, Row: Integer; Ch: Byte);
    function GetColorAt(Col, Row: Integer): Byte;
    procedure SetColorAt(Col, Row: Integer; Color: Byte);

    // Shape save/load - stub implementations
    function SaveShape(X1, Y1, X2, Y2: Double): string;
    procedure LoadShape(const Data: string; X, Y: Double; Mode: Integer);

    // Style state - stub implementations
    procedure SetBorderStyle(const Style: TBorderStyle);
    procedure SetFillStyle(const Style: TFillStyleDef);
    function GetBorderStyle: TBorderStyle;
    function GetFillStyle: TFillStyleDef;

    // FAST mode - stub implementations
    procedure SetFastMode(Enabled: Boolean);
    function GetFastMode: Boolean;
    procedure SetFastModeAlpha(Alpha: Byte);
    function GetFastModeAlpha: Byte;

    // Get accumulated content
    function GetContent: string;

    property Context: TWebContext read FContext;
  end;

{ Helper functions }
function HtmlEncode(const S: string): string;
function UrlEncode(const S: string): string;
function UrlDecode(const S: string): string;
function GetStatusCodeText(Code: Integer): string;

implementation

uses
  StrUtils;

{ HTML encode - escapes < > & " ' }
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

{ URL encode - escapes special characters for URLs }
function UrlEncode(const S: string): string;
var
  i: Integer;
  C: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    C := S[i];
    if C in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + C
    else if C = ' ' then
      Result := Result + '+'
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

{ URL decode - decodes %XX sequences and + to space }
function UrlDecode(const S: string): string;
var
  i: Integer;
  HexStr: string;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] = '%' then
    begin
      if i + 2 <= Length(S) then
      begin
        HexStr := '$' + Copy(S, i + 1, 2);
        try
          Result := Result + Chr(StrToInt(HexStr));
          Inc(i, 3);
          Continue;
        except
          // Invalid hex, keep as-is
        end;
      end;
      Result := Result + S[i];
    end
    else if S[i] = '+' then
      Result := Result + ' '
    else
      Result := Result + S[i];
    Inc(i);
  end;
end;

{ TWebContext }

constructor TWebContext.Create;
begin
  inherited Create;
  FMethod := 'GET';
  FPath := '/';
  FQueryString := '';
  FResponseStatus := 200;
  FGetParams := TStringMap.Create;
  FPostParams := TStringMap.Create;
  FRequestHeaders := TStringMap.Create;
  FResponseHeaders := TStringMap.Create;
end;

destructor TWebContext.Destroy;
begin
  FGetParams.Free;
  FPostParams.Free;
  FRequestHeaders.Free;
  FResponseHeaders.Free;
  inherited Destroy;
end;

procedure TWebContext.ParseQueryString(const QueryStr: string);
var
  Pairs: TStringArray;
  Pair: TStringArray;
  i: Integer;
  Name, Value: string;
begin
  FQueryString := QueryStr;
  FGetParams.Clear;

  if QueryStr = '' then
    Exit;

  Pairs := QueryStr.Split(['&']);
  for i := 0 to High(Pairs) do
  begin
    Pair := Pairs[i].Split(['=']);
    if Length(Pair) >= 1 then
    begin
      Name := UrlDecode(Pair[0]);
      if Length(Pair) >= 2 then
        Value := UrlDecode(Pair[1])
      else
        Value := '';
      FGetParams.Add(Name, Value);
    end;
  end;
end;

procedure TWebContext.ParsePostData(const PostData: string);
var
  Pairs: TStringArray;
  Pair: TStringArray;
  i: Integer;
  Name, Value: string;
begin
  FPostParams.Clear;

  if PostData = '' then
    Exit;

  // Parse as application/x-www-form-urlencoded
  Pairs := PostData.Split(['&']);
  for i := 0 to High(Pairs) do
  begin
    Pair := Pairs[i].Split(['=']);
    if Length(Pair) >= 1 then
    begin
      Name := UrlDecode(Pair[0]);
      if Length(Pair) >= 2 then
        Value := UrlDecode(Pair[1])
      else
        Value := '';
      FPostParams.Add(Name, Value);
    end;
  end;
end;

procedure TWebContext.SetRequestHeader(const Name, Value: string);
begin
  FRequestHeaders.AddOrSetData(LowerCase(Name), Value);
end;

procedure TWebContext.SetResponseHeader(const Name, Value: string);
begin
  FResponseHeaders.AddOrSetData(Name, Value);
end;

function TWebContext.GetParam(const Name: string): string;
var
  Idx: Integer;
begin
  Idx := FGetParams.IndexOf(Name);
  if Idx >= 0 then
    Result := HtmlEncode(FGetParams.Data[Idx])
  else
    Result := '';
end;

function TWebContext.PostParam(const Name: string): string;
var
  Idx: Integer;
begin
  Idx := FPostParams.IndexOf(Name);
  if Idx >= 0 then
    Result := HtmlEncode(FPostParams.Data[Idx])
  else
    Result := '';
end;

function TWebContext.GetParamRaw(const Name: string): string;
var
  Idx: Integer;
begin
  Idx := FGetParams.IndexOf(Name);
  if Idx >= 0 then
    Result := FGetParams.Data[Idx]
  else
    Result := '';
end;

function TWebContext.PostParamRaw(const Name: string): string;
var
  Idx: Integer;
begin
  Idx := FPostParams.IndexOf(Name);
  if Idx >= 0 then
    Result := FPostParams.Data[Idx]
  else
    Result := '';
end;

function TWebContext.GetHeader(const Name: string): string;
var
  Idx: Integer;
begin
  Idx := FRequestHeaders.IndexOf(LowerCase(Name));
  if Idx >= 0 then
    Result := FRequestHeaders.Data[Idx]
  else
    Result := '';
end;

procedure TWebContext.ApplyResponseHeaders(var Response: TFPHTTPConnectionResponse);
var
  i: Integer;
begin
  for i := 0 to FResponseHeaders.Count - 1 do
  begin
    if LowerCase(FResponseHeaders.Keys[i]) = 'content-type' then
      Response.ContentType := FResponseHeaders.Data[i]
    else
      Response.SetCustomHeader(FResponseHeaders.Keys[i], FResponseHeaders.Data[i]);
  end;
end;

{ Get HTTP status code text }
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

{ TWebOutput }

constructor TWebOutput.Create(AContext: TWebContext);
begin
  inherited Create;
  FContent := TStringBuilder.Create;
  FContext := AContext;
  FInitialized := False;
  FCursorX := 0;
  FCursorY := 0;
  FFastMode := False;
  FFastModeAlpha := 128;
end;

destructor TWebOutput.Destroy;
begin
  FContent.Free;
  inherited Destroy;
end;

function TWebOutput.Initialize(const Title: string; Width, Height: Integer): Boolean;
begin
  FInitialized := True;
  Result := True;
end;

procedure TWebOutput.Shutdown;
begin
  FInitialized := False;
end;

function TWebOutput.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

procedure TWebOutput.Print(const Text: string; ClearBackground: Boolean);
begin
  FContent.Append(Text);
end;

procedure TWebOutput.PrintLn(const Text: string; ClearBackground: Boolean);
begin
  FContent.Append(Text);
  FContent.AppendLine;
end;

procedure TWebOutput.NewLine;
begin
  FContent.AppendLine;
end;

procedure TWebOutput.Clear;
begin
  FContent.Clear;
end;

procedure TWebOutput.SetCursor(X, Y: Integer);
begin
  FCursorX := X;
  FCursorY := Y;
end;

procedure TWebOutput.MoveCursor(DeltaX, DeltaY: Integer);
begin
  Inc(FCursorX, DeltaX);
  Inc(FCursorY, DeltaY);
end;

function TWebOutput.GetCursorX: Integer;
begin
  Result := FCursorX;
end;

function TWebOutput.GetCursorY: Integer;
begin
  Result := FCursorY;
end;

procedure TWebOutput.ShowCursor(X, Y: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.HideCursor(X, Y: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetColors(Foreground, Background: TColor);
begin
  // Not applicable for web output
end;

procedure TWebOutput.Present;
begin
  // Nothing to present - content is accumulated
end;

procedure TWebOutput.SetFullscreen(Enabled: Boolean);
begin
  // Not applicable for web output
end;

function TWebOutput.IsFullscreen: Boolean;
begin
  Result := False;
end;

function TWebOutput.ShouldQuit: Boolean;
begin
  Result := False;
end;

function TWebOutput.GetActualCols: Integer;
begin
  Result := 80;
end;

function TWebOutput.GetActualRows: Integer;
begin
  Result := 25;
end;

procedure TWebOutput.MarkPromptRow;
begin
  // Not applicable for web output
end;

procedure TWebOutput.OnUserInput;
begin
  // Not applicable for web output
end;

function TWebOutput.HandleScrollKeys(Key, Modifiers: Integer): Boolean;
begin
  Result := False;
end;

procedure TWebOutput.ProcessScrollInput;
begin
  // Not applicable for web output
end;

function TWebOutput.GetInScrollMode: Boolean;
begin
  Result := False;
end;

function TWebOutput.SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean;
                                   SplitLine: Integer): Boolean;
begin
  // Graphics mode not supported in web mode
  Result := False;
end;

function TWebOutput.GetGraphicMode: TGraphicMode;
begin
  Result := gm40ColText;
end;

function TWebOutput.IsInGraphicsMode: Boolean;
begin
  Result := False;
end;

procedure TWebOutput.ClearScreen(Mode: Integer);
begin
  FContent.Clear;
end;

procedure TWebOutput.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  // Not applicable for web output
end;

function TWebOutput.GetPixel(X, Y: Integer): UInt32;
begin
  Result := 0;
end;

function TWebOutput.GetPixelIndex(X, Y: Integer): TPaletteIndex;
begin
  Result := 0;
end;

procedure TWebOutput.EnablePalette(Enable: Boolean);
begin
  // Not applicable for web output
end;

function TWebOutput.IsPaletteEnabled: Boolean;
begin
  Result := False;
end;

procedure TWebOutput.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetPaletteColorRGBA(Index: TPaletteIndex; R, G, B: Byte; A: Byte);
begin
  // Not applicable for web output
end;

function TWebOutput.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := 0;
end;

procedure TWebOutput.ResetPalette;
begin
  // Not applicable for web output
end;

function TWebOutput.LoadPaletteFromJSON(const FileName: string): Boolean;
begin
  Result := False;
end;

function TWebOutput.SavePaletteToJSON(const FileName: string): Boolean;
begin
  Result := False;
end;

function TWebOutput.GetLastPaletteError: string;
begin
  Result := 'Palette not supported in web mode';
end;

procedure TWebOutput.DrawBox(X1, Y1, X2, Y2: Integer; Angle: Double);
begin
  // Not applicable for web output
end;

procedure TWebOutput.DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double);
begin
  // Not applicable for web output
end;

procedure TWebOutput.DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double; Filled: Boolean);
begin
  // Not applicable for web output
end;

procedure TWebOutput.DrawCircleWithColor(X, Y, XR, YR: Integer; Color: UInt32;
                                         SA, EA, Angle, Inc: Double);
begin
  // Not applicable for web output
end;

procedure TWebOutput.DrawLine(X1, Y1, X2, Y2: Integer; Color: UInt32);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetPixelCursor(X, Y: Integer);
begin
  // Not applicable for web output
end;

function TWebOutput.GetPixelCursorX: Integer;
begin
  Result := 0;
end;

function TWebOutput.GetPixelCursorY: Integer;
begin
  Result := 0;
end;

procedure TWebOutput.SetColorSource(Source, Color: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetColorSourceDirect(Source, Color: Integer);
begin
  // Not applicable for web output
end;

function TWebOutput.GetColorSourceDirect(Source: Integer): Integer;
begin
  Result := 0;
end;

procedure TWebOutput.SetLineWidth(Width: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetScale(Enabled: Boolean; XMax, YMax: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.FloodFill(Source: Integer; X, Y: Double; Mode: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetWindow(Col1, Row1, Col2, Row2: Integer; DoClear: Boolean);
begin
  // Not applicable for web output
end;

function TWebOutput.GetWindowLines: Integer;
begin
  Result := 25;
end;

function TWebOutput.GetWindowCols: Integer;
begin
  Result := 80;
end;

function TWebOutput.GetScreenWidth: Integer;
begin
  Result := 80;
end;

function TWebOutput.GetCharAt(Col, Row: Integer): Byte;
begin
  Result := 0;
end;

procedure TWebOutput.SetCharAt(Col, Row: Integer; Ch: Byte);
begin
  // Not applicable for web output
end;

function TWebOutput.GetColorAt(Col, Row: Integer): Byte;
begin
  Result := 0;
end;

procedure TWebOutput.SetColorAt(Col, Row: Integer; Color: Byte);
begin
  // Not applicable for web output
end;

function TWebOutput.SaveShape(X1, Y1, X2, Y2: Double): string;
begin
  Result := '';
end;

procedure TWebOutput.LoadShape(const Data: string; X, Y: Double; Mode: Integer);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetBorderStyle(const Style: TBorderStyle);
begin
  // Not applicable for web output
end;

procedure TWebOutput.SetFillStyle(const Style: TFillStyleDef);
begin
  // Not applicable for web output
end;

function TWebOutput.GetBorderStyle: TBorderStyle;
begin
  Result := MakeDefaultBorderStyle(0, 1);
end;

function TWebOutput.GetFillStyle: TFillStyleDef;
begin
  Result := MakeDefaultFillStyle(0);
end;

procedure TWebOutput.SetFastMode(Enabled: Boolean);
begin
  FFastMode := Enabled;
end;

function TWebOutput.GetFastMode: Boolean;
begin
  Result := FFastMode;
end;

procedure TWebOutput.SetFastModeAlpha(Alpha: Byte);
begin
  FFastModeAlpha := Alpha;
end;

function TWebOutput.GetFastModeAlpha: Byte;
begin
  Result := FFastModeAlpha;
end;

function TWebOutput.GetContent: string;
begin
  Result := FContent.ToString;
end;

end.
