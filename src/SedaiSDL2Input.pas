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
unit SedaiSDL2Input;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, StrUtils, Math,
  SDL2,
  SedaiOutputInterface;

type
  { UTF-8 Helper Record }
  TUTF8Position = record
    ByteIndex: Integer;     // Position in bytes (for string operations)
    CharIndex: Integer;     // Position in UTF-8 characters (logical)
  end;

  { TSDL2InputDevice - UTF-8 COMPLIANT INPUT SYSTEM }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TSDL2InputDevice = class(TObject, IInputDevice)
  private
    FInputBuffer: string;
    FCharacterPositions: array of Integer; // Maps char index to byte index
    FBufferCharCount: Integer;             // Number of UTF-8 characters
    FCursorCharPosition: Integer;          // Cursor position in characters
    FQuitRequested: Boolean;               // CTRL+ALT+END: exit SedaiVision
    FStopRequested: Boolean;               // CTRL+C: stop BASIC program
    FMaxInputLength: Integer;
    FLastCursorToggle: Cardinal;
    FCursorVisible: Boolean;
    FCurrentPrompt: string;
    FInputActive: Boolean;

    // Visual positioning
    FInputStartX, FInputStartY: Integer;
    FVisualCursorX, FVisualCursorY: Integer;

    // Output device reference
    FOutputDevice: IOutputDevice;

    // Command history
    FCommandHistory: TStringList;
    FHistoryIndex: Integer;
    FMaxHistorySize: Integer;

    // Input mode flags
    FIsInputMode: Boolean;

    // === UTF-8 CORE FUNCTIONS ===
    function UTF8CharLength(const S: string; BytePos: Integer): Integer;
    function UTF8Length(const S: string): Integer;
    function UTF8Copy(const S: string; StartChar, CharCount: Integer): string;
    function UTF8CharAt(const S: string; CharIndex: Integer): string;
    function CharIndexToByteIndex(CharIndex: Integer): Integer;
    function ByteIndexToCharIndex(ByteIndex: Integer): Integer;
    procedure RebuildCharacterMap;
    function ValidateUTF8Buffer: Boolean;

    // === SAFE BUFFER OPERATIONS ===
    procedure InsertUTF8Text(const Text: string; CharPos: Integer);
    procedure DeleteUTF8Char(CharPos: Integer; Direction: Integer); // -1 = backspace, +1 = delete
    procedure ClearInputBuffer;
    procedure SetInputBuffer(const Text: string);

    // === POSITION MANAGEMENT ===
    function GetUTF8Position(CharIndex: Integer): TUTF8Position;
    procedure SetCursorCharPosition(NewCharPos: Integer);
    procedure MoveCursorByChars(Delta: Integer);
    procedure CalculateVisualCursorPosition;

    // === EVENT HANDLING ===
    procedure HandleTextInput(const Text: string);
    procedure HandleKeyDown(Key: Integer; Modifiers: Word);
    procedure ProcessInputEvents;
    procedure ProcessScrollEvents;

    // === DISPLAY MANAGEMENT ===
    procedure UpdateDisplay;
    procedure UpdateCursor;
    procedure RedrawInputLine;
    procedure EnsureCursorHidden;
    procedure EnsureCursorVisible;
    procedure UpdateVisualDisplay;

    // === HISTORY MANAGEMENT ===
    procedure AddToHistory(const Command: string);
    procedure NavigateHistory(Direction: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    // IInputDevice implementation
    function ReadLine(const Prompt: string = ''; IsCommand: Boolean = True; NumericOnly: Boolean = False; AllowDecimal: Boolean = True): string;
    function ReadKey: Char;
    function KeyPressed: Boolean;

    // State management
    function ShouldQuit: Boolean;       // CTRL+ALT+END pressed - exit application
    function ShouldStop: Boolean;       // CTRL+C pressed - stop BASIC program
    procedure ClearStopRequest;         // Clear stop flag after handling
    procedure ProcessEvents;
    procedure Reset;

    // Configuration
    property MaxInputLength: Integer read FMaxInputLength write FMaxInputLength;
    property OutputDevice: IOutputDevice read FOutputDevice write FOutputDevice;
    property MaxHistorySize: Integer read FMaxHistorySize write FMaxHistorySize;
  end;

implementation

{ TSDL2InputDevice }

constructor TSDL2InputDevice.Create;
begin
  inherited Create;

  // Initialize basic state
  FInputBuffer := '';
  SetLength(FCharacterPositions, 256); // Initial capacity
  FBufferCharCount := 0;
  FCursorCharPosition := 0;
  FQuitRequested := False;
  FMaxInputLength := 255;
  FLastCursorToggle := SDL_GetTicks;
  FCursorVisible := False;
  FCurrentPrompt := '';
  FInputActive := False;
  FOutputDevice := nil;

  // Visual positioning
  FInputStartX := 0;
  FInputStartY := 0;
  FVisualCursorX := 0;
  FVisualCursorY := 0;

  // History management
  FCommandHistory := TStringList.Create;
  FHistoryIndex := -1;
  FMaxHistorySize := 100;

  FIsInputMode := False;
end;

destructor TSDL2InputDevice.Destroy;
begin
  FCommandHistory.Free;
  inherited Destroy;
end;

// === UTF-8 CORE FUNCTIONS ===

function TSDL2InputDevice.UTF8CharLength(const S: string; BytePos: Integer): Integer;
begin
  if (BytePos < 1) or (BytePos > Length(S)) then
  begin
    Result := 0;
    Exit;
  end;

  case Ord(S[BytePos]) of
    0..127:   Result := 1;      // ASCII
    192..223: Result := 2;      // 2-byte UTF-8 (à, è, ò, ñ)
    224..239: Result := 3;      // 3-byte UTF-8 (€, 中, ♠)
    240..247: Result := 4;      // 4-byte UTF-8 (emoji, 𝕏)
    else      Result := 1;      // Invalid byte - treat as single
  end;
end;

function TSDL2InputDevice.UTF8Length(const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 1;
  while i <= Length(S) do
  begin
    Inc(Result);
    Inc(i, UTF8CharLength(S, i));
  end;
end;

function TSDL2InputDevice.UTF8Copy(const S: string; StartChar, CharCount: Integer): string;
var
  StartByte, CurrentByte, CharsFound: Integer;
begin
  Result := '';
  if (StartChar < 1) or (CharCount < 1) or (Length(S) = 0) then Exit;

  // Find starting byte position
  StartByte := 1;
  CharsFound := 1;

  while (CharsFound < StartChar) and (StartByte <= Length(S)) do
  begin
    Inc(StartByte, UTF8CharLength(S, StartByte));
    Inc(CharsFound);
  end;

  if StartByte > Length(S) then Exit;

  // Copy the specified number of characters
  CurrentByte := StartByte;
  CharsFound := 0;

  while (CharsFound < CharCount) and (CurrentByte <= Length(S)) do
  begin
    Result := Result + Copy(S, CurrentByte, UTF8CharLength(S, CurrentByte));
    Inc(CurrentByte, UTF8CharLength(S, CurrentByte));
    Inc(CharsFound);
  end;
end;

function TSDL2InputDevice.UTF8CharAt(const S: string; CharIndex: Integer): string;
begin
  Result := UTF8Copy(S, CharIndex, 1);
end;

function TSDL2InputDevice.CharIndexToByteIndex(CharIndex: Integer): Integer;
begin
  if CharIndex <= 0 then
    Result := 1  // Start of string (1-based indexing)
  else if CharIndex >= FBufferCharCount then
    Result := Length(FInputBuffer) + 1  // End of string
  else if (CharIndex - 1) < Length(FCharacterPositions) then
    Result := FCharacterPositions[CharIndex - 1]
  else
    Result := Length(FInputBuffer) + 1;
end;

function TSDL2InputDevice.ByteIndexToCharIndex(ByteIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FBufferCharCount - 1 do
  begin
    if FCharacterPositions[i] >= ByteIndex then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := FBufferCharCount;
end;

procedure TSDL2InputDevice.RebuildCharacterMap;
var
  i, BytePos, CharCount: Integer;
begin
  CharCount := 0;
  BytePos := 1;

  // Ensure capacity
  if Length(FCharacterPositions) < Length(FInputBuffer) then
    SetLength(FCharacterPositions, Length(FInputBuffer) + 64);

  while BytePos <= Length(FInputBuffer) do
  begin
    if CharCount < Length(FCharacterPositions) then
      FCharacterPositions[CharCount] := BytePos;

    Inc(BytePos, UTF8CharLength(FInputBuffer, BytePos));
    Inc(CharCount);
  end;

  FBufferCharCount := CharCount;

  // Ensure cursor is within bounds
  if FCursorCharPosition > FBufferCharCount then
    FCursorCharPosition := FBufferCharCount;
end;

function TSDL2InputDevice.ValidateUTF8Buffer: Boolean;
var
  i, CharLen: Integer;
begin
  Result := True;
  i := 1;

  while i <= Length(FInputBuffer) do
  begin
    CharLen := UTF8CharLength(FInputBuffer, i);

    // Check if we have enough bytes for this character
    if i + CharLen - 1 > Length(FInputBuffer) then
    begin
      Result := False;
      Exit;
    end;

    // Basic UTF-8 sequence validation
    if CharLen > 1 then
    begin
      // Check continuation bytes
      if (CharLen >= 2) and (i + 1 <= Length(FInputBuffer)) then
        if (Ord(FInputBuffer[i + 1]) and $C0) <> $80 then
        begin
          Result := False;
          Exit;
        end;
    end;

    Inc(i, CharLen);
  end;
end;

// === SAFE BUFFER OPERATIONS ===

procedure TSDL2InputDevice.InsertUTF8Text(const Text: string; CharPos: Integer);
var
  BytePos: Integer;
  NewBuffer: string;
  TextCharCount: Integer;
begin
  if Text = '' then Exit;

  TextCharCount := UTF8Length(Text);
  if FBufferCharCount + TextCharCount > FMaxInputLength then Exit;

  // Handle insertion at start, middle, or end
  if CharPos <= 0 then
  begin
    // Insert at beginning
    NewBuffer := Text + FInputBuffer;
  end
  else if CharPos >= FBufferCharCount then
  begin
    // Insert at end
    NewBuffer := FInputBuffer + Text;
  end
  else
  begin
    // Insert in middle
    BytePos := CharIndexToByteIndex(CharPos);
    NewBuffer := Copy(FInputBuffer, 1, BytePos - 1) + Text + Copy(FInputBuffer, BytePos, MaxInt);
  end;

  // Validate the result
  FInputBuffer := NewBuffer;
  if not ValidateUTF8Buffer then
  begin
    // Rollback on validation failure - restore original buffer
    if CharPos <= 0 then
      FInputBuffer := Copy(NewBuffer, Length(Text) + 1, MaxInt)
    else if CharPos >= FBufferCharCount then
      FInputBuffer := Copy(NewBuffer, 1, Length(NewBuffer) - Length(Text))
    else
    begin
      BytePos := CharIndexToByteIndex(CharPos);
      FInputBuffer := Copy(NewBuffer, 1, BytePos - 1) + Copy(NewBuffer, BytePos + Length(Text), MaxInt);
    end;
    Exit;
  end;

  // Rebuild character map
  RebuildCharacterMap;

  // Update cursor position
  FCursorCharPosition := CharPos + TextCharCount;
end;

procedure TSDL2InputDevice.DeleteUTF8Char(CharPos: Integer; Direction: Integer);
var
  DeleteCharPos: Integer;
  StartByte, CharLen: Integer;
  NewBuffer: string;
begin
  if FBufferCharCount = 0 then Exit;

  DeleteCharPos := CharPos;
  if Direction = -1 then // Backspace
  begin
    if DeleteCharPos <= 0 then Exit;
    Dec(DeleteCharPos);
  end
  else // Delete
  begin
    if DeleteCharPos >= FBufferCharCount then Exit;
  end;

  StartByte := CharIndexToByteIndex(DeleteCharPos);
  CharLen := UTF8CharLength(FInputBuffer, StartByte);

  // Create new buffer without the character
  NewBuffer := Copy(FInputBuffer, 1, StartByte - 1) +
               Copy(FInputBuffer, StartByte + CharLen, MaxInt);

  FInputBuffer := NewBuffer;
  RebuildCharacterMap;

  // Update cursor position
  if Direction = -1 then // Backspace
    FCursorCharPosition := DeleteCharPos;
end;

procedure TSDL2InputDevice.ClearInputBuffer;
begin
  FInputBuffer := '';
  FBufferCharCount := 0;
  FCursorCharPosition := 0;
  SetLength(FCharacterPositions, 256);
end;

procedure TSDL2InputDevice.SetInputBuffer(const Text: string);
begin
  FInputBuffer := Text;
  RebuildCharacterMap;
  FCursorCharPosition := FBufferCharCount;
end;

// === POSITION MANAGEMENT ===

function TSDL2InputDevice.GetUTF8Position(CharIndex: Integer): TUTF8Position;
begin
  Result.CharIndex := CharIndex;
  Result.ByteIndex := CharIndexToByteIndex(CharIndex);
end;

procedure TSDL2InputDevice.SetCursorCharPosition(NewCharPos: Integer);
begin
  FCursorCharPosition := Max(0, Min(NewCharPos, FBufferCharCount));
end;

procedure TSDL2InputDevice.MoveCursorByChars(Delta: Integer);
begin
  SetCursorCharPosition(FCursorCharPosition + Delta);
end;

procedure TSDL2InputDevice.CalculateVisualCursorPosition;
var
  Cols: Integer;
  VisibleChars: Integer;
begin
  if not Assigned(FOutputDevice) then Exit;

  Cols := FOutputDevice.GetActualCols;
  VisibleChars := FCursorCharPosition;

  FVisualCursorX := (FInputStartX + VisibleChars) mod Cols;
  FVisualCursorY := FInputStartY + ((FInputStartX + VisibleChars) div Cols);

  if FVisualCursorY >= FOutputDevice.GetActualRows then
    FVisualCursorY := FOutputDevice.GetActualRows - 1;
end;

// === EVENT HANDLING ===

procedure TSDL2InputDevice.HandleTextInput(const Text: string);
begin
  EnsureCursorHidden;

  // Insert UTF-8 text at cursor position
  InsertUTF8Text(Text, FCursorCharPosition);

  // Update display
  UpdateVisualDisplay;
  CalculateVisualCursorPosition;
end;

procedure TSDL2InputDevice.HandleKeyDown(Key: Integer; Modifiers: Word);
var
  i, Cols, MaxDisplayChars: Integer;
  ClearX, ClearY: Integer;
  NumLockActive: Boolean;
begin
  EnsureCursorHidden;

  NumLockActive := ((SDL_GetModState and KMOD_NUM) <> 0);

  case Key of
    SDLK_RETURN, SDLK_KP_ENTER:
    begin
      FInputActive := False;
    end;

    SDLK_DELETE, SDLK_KP_PERIOD:
    begin
      if (Key = SDLK_KP_PERIOD) and NumLockActive then Exit;

      DeleteUTF8Char(FCursorCharPosition, 1); // Delete forward
      UpdateVisualDisplay;
      CalculateVisualCursorPosition;
    end;

    SDLK_BACKSPACE:
    begin
      if FCursorCharPosition > 0 then
      begin
        DeleteUTF8Char(FCursorCharPosition, -1); // Delete backward

        // Clear extended area and redraw
        Cols := FOutputDevice.GetActualCols;
        MaxDisplayChars := FBufferCharCount + 10;

        ClearX := FInputStartX;
        ClearY := FInputStartY;

        for i := 0 to MaxDisplayChars do
        begin
          if ClearX >= Cols then
          begin
            ClearX := 0;
            Inc(ClearY);
          end;
          FOutputDevice.SetCursor(ClearX, ClearY);
          FOutputDevice.Print(' ');
          Inc(ClearX);
        end;

        UpdateVisualDisplay;
        CalculateVisualCursorPosition;
      end;
    end;

    SDLK_LEFT, SDLK_KP_4:
    begin
      if (Key = SDLK_KP_4) and NumLockActive then Exit;
      MoveCursorByChars(-1);
      CalculateVisualCursorPosition;
    end;

    SDLK_RIGHT, SDLK_KP_6:
    begin
      if (Key = SDLK_KP_6) and NumLockActive then Exit;
      MoveCursorByChars(1);
      CalculateVisualCursorPosition;
    end;

    SDLK_UP: NavigateHistory(-1);
    SDLK_DOWN: NavigateHistory(+1);

    SDLK_HOME, SDLK_KP_7:
    begin
      if (Key = SDLK_KP_7) and NumLockActive then Exit;
      SetCursorCharPosition(0);
      CalculateVisualCursorPosition;
    end;

    SDLK_END, SDLK_KP_1:
    begin
      if (Key = SDLK_KP_1) and NumLockActive then Exit;
      SetCursorCharPosition(FBufferCharCount);
      CalculateVisualCursorPosition;
    end;

    SDLK_F11:
    begin
      if (Modifiers and KMOD_CTRL) <> 0 then
        FOutputDevice.SetFullscreen(not FOutputDevice.IsFullscreen);
    end;

    SDLK_ESCAPE:
    begin
      if (Modifiers and KMOD_CTRL) = 0 then
      begin
        ClearInputBuffer;
        UpdateVisualDisplay;
        CalculateVisualCursorPosition;
      end
      else
      begin
        FQuitRequested := True;
        FInputActive := False;
      end;
    end;

    SDLK_c:
    begin
      if (Modifiers and KMOD_CTRL) <> 0 then
      begin
        FQuitRequested := True;
        FInputActive := False;
      end;
    end;
  end;
end;

// === DISPLAY MANAGEMENT ===

procedure TSDL2InputDevice.UpdateVisualDisplay;
var
  i, Cols: Integer;
  VisualX, VisualY: Integer;
  Ch: string;
  ClearLength: Integer;
begin
  if not Assigned(FOutputDevice) then Exit;

  Cols := FOutputDevice.GetActualCols;

  // Clear previous input area
  ClearLength := Min(FBufferCharCount + 10, Cols - FInputStartX);
  VisualX := FInputStartX;
  VisualY := FInputStartY;

  FOutputDevice.SetCursor(VisualX, VisualY);

  // Clear old content
  for i := 0 to ClearLength - 1 do
  begin
    if VisualX >= Cols then
    begin
      VisualX := 0;
      Inc(VisualY);
    end;
    FOutputDevice.Print(' ');
    Inc(VisualX);
  end;

  // Redraw current input using UTF-8 character iteration
  VisualX := FInputStartX;
  VisualY := FInputStartY;
  FOutputDevice.SetCursor(VisualX, VisualY);

  for i := 1 to FBufferCharCount do
  begin
    if VisualX >= Cols then
    begin
      VisualX := 0;
      Inc(VisualY);
    end;

    Ch := UTF8CharAt(FInputBuffer, i);
    FOutputDevice.Print(Ch);
    Inc(VisualX);
  end;

  FOutputDevice.Present;
end;

procedure TSDL2InputDevice.EnsureCursorHidden;
begin
  if FCursorVisible and Assigned(FOutputDevice) then
  begin
    FOutputDevice.HideCursor(FVisualCursorX, FVisualCursorY);
    FCursorVisible := False;
  end;
end;

procedure TSDL2InputDevice.EnsureCursorVisible;
begin
  if not FCursorVisible and Assigned(FOutputDevice) then
  begin
    CalculateVisualCursorPosition;
    FOutputDevice.ShowCursor(FVisualCursorX, FVisualCursorY);
    FCursorVisible := True;
  end;
end;

procedure TSDL2InputDevice.UpdateCursor;
var
  CurrentTime: Cardinal;
begin
  if not FInputActive or not Assigned(FOutputDevice) then Exit;

  CurrentTime := SDL_GetTicks;
  CalculateVisualCursorPosition;

  FOutputDevice.SetCursor(FVisualCursorX, FVisualCursorY);

  if CurrentTime - FLastCursorToggle >= 500 then
  begin
    if FCursorVisible then
    begin
      FOutputDevice.HideCursor(FVisualCursorX, FVisualCursorY);
      FCursorVisible := False;
    end
    else
    begin
      FOutputDevice.ShowCursor(FVisualCursorX, FVisualCursorY);
      FCursorVisible := True;
    end;

    FLastCursorToggle := CurrentTime;
  end
  else
  begin
    if FCursorVisible then
    begin
      FOutputDevice.HideCursor(FVisualCursorX, FVisualCursorY);
      FOutputDevice.ShowCursor(FVisualCursorX, FVisualCursorY);
    end;
  end;
end;

procedure TSDL2InputDevice.UpdateDisplay;
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.Present;
end;

procedure TSDL2InputDevice.RedrawInputLine;
begin
  UpdateVisualDisplay;
end;

// === HISTORY MANAGEMENT ===

procedure TSDL2InputDevice.AddToHistory(const Command: string);
begin
  if Trim(Command) = '' then Exit;

  if (FCommandHistory.Count > 0) and
     (FCommandHistory[FCommandHistory.Count-1] = Command) then Exit;

  FCommandHistory.Add(Command);

  while FCommandHistory.Count > FMaxHistorySize do
    FCommandHistory.Delete(0);

  FHistoryIndex := FCommandHistory.Count;
end;

procedure TSDL2InputDevice.NavigateHistory(Direction: Integer);
var
  NewIndex: Integer;
begin
  if FCommandHistory.Count = 0 then Exit;

  NewIndex := FHistoryIndex + Direction;

  if NewIndex < 0 then
    NewIndex := 0
  else if NewIndex >= FCommandHistory.Count then
    NewIndex := FCommandHistory.Count;

  FHistoryIndex := NewIndex;

  if FHistoryIndex < FCommandHistory.Count then
  begin
    SetInputBuffer(FCommandHistory[FHistoryIndex]);
  end
  else
  begin
    ClearInputBuffer;
  end;

  UpdateVisualDisplay;
  CalculateVisualCursorPosition;
end;

// === SCROLL EVENT HANDLING ===

procedure TSDL2InputDevice.ProcessScrollEvents;
var
  Event: TSDL_Event;
  EventsToReprocess: array of TSDL_Event;
  EventCount: Integer;
  i: Integer;
begin
  EventCount := 0;
  SetLength(EventsToReprocess, 100);

  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_KEYDOWN:
      begin
        if FOutputDevice.HandleScrollKeys(Event.key.keysym.sym, Event.key.keysym.mod_) then
        begin
          // Scroll event handled - don't reprocess
        end
        else
        begin
          if EventCount < Length(EventsToReprocess) then
          begin
            EventsToReprocess[EventCount] := Event;
            Inc(EventCount);
          end;

          if not FIsInputMode then
            FOutputDevice.OnUserInput;
        end;
      end;

      SDL_TEXTINPUT:
      begin
        if not FIsInputMode then
          FOutputDevice.OnUserInput;

        if EventCount < Length(EventsToReprocess) then
        begin
          EventsToReprocess[EventCount] := Event;
          Inc(EventCount);
        end;
      end;

      else
      begin
        if EventCount < Length(EventsToReprocess) then
        begin
          EventsToReprocess[EventCount] := Event;
          Inc(EventCount);
        end;
      end;
    end;
  end;

  // Requeue unprocessed events
  for i := 0 to EventCount - 1 do
    SDL_PushEvent(@EventsToReprocess[i]);
end;

procedure TSDL2InputDevice.ProcessInputEvents;
var
  Event: TSDL_Event;
begin
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        FQuitRequested := True;
        FInputActive := False;
      end;

      SDL_TEXTINPUT:
      begin
        HandleTextInput(Event.text.text);
      end;

      SDL_KEYDOWN:
      begin
        HandleKeyDown(Event.key.keysym.sym, Event.key.keysym.mod_);
      end;
    end;
  end;
end;

// === PUBLIC INTERFACE ===

function TSDL2InputDevice.ReadLine(const Prompt: string; IsCommand: Boolean; NumericOnly: Boolean; AllowDecimal: Boolean): string;
begin
  if not Assigned(FOutputDevice) then
    raise Exception.Create('OutputDevice must be assigned before ReadLine');

  FIsInputMode := not IsCommand;

  FOutputDevice.MarkPromptRow;

  // Initialize input state
  ClearInputBuffer;
  FCurrentPrompt := Prompt;
  FInputActive := True;
  FQuitRequested := False;
  FCursorVisible := False;

  // Display prompt and set initial position
  if Prompt <> '' then
  begin
    FOutputDevice.Print(Prompt);
    FOutputDevice.Present;
  end;

  FInputStartX := FOutputDevice.GetCursorX;
  FInputStartY := FOutputDevice.GetCursorY;
  FVisualCursorX := FInputStartX;
  FVisualCursorY := FInputStartY;

  // Show cursor
  FOutputDevice.SetCursor(FVisualCursorX, FVisualCursorY);
  FOutputDevice.ShowCursor(FVisualCursorX, FVisualCursorY);
  FCursorVisible := True;
  FLastCursorToggle := SDL_GetTicks;
  FOutputDevice.Present;

  SDL_StartTextInput;
  try
    while FInputActive and not FQuitRequested do
    begin
      ProcessScrollEvents;
      ProcessInputEvents;
      UpdateCursor;
      UpdateDisplay;
      SDL_Delay(16);
    end;
  finally
    SDL_StopTextInput;
    FInputActive := False;
    EnsureCursorHidden;
    FIsInputMode := False;
  end;

  Result := FInputBuffer;
  if not FIsInputMode then
    AddToHistory(Result);

  FOutputDevice.NewLine;
  FOutputDevice.Present;
end;

function TSDL2InputDevice.ReadKey: Char;
var
  Event: TSDL_Event;
  Done: Boolean;
begin
  Result := #0;
  Done := False;

  while not Done and not FQuitRequested do
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV:
        begin
          FQuitRequested := True;
          Done := True;
        end;

        SDL_TEXTINPUT:
        begin
          if Length(Event.text.text) > 0 then
          begin
            Result := Event.text.text[1];
            Done := True;
          end;
        end;

        SDL_KEYDOWN:
        begin
          case Event.key.keysym.sym of
            SDLK_RETURN:
            begin
              Result := #13;
              Done := True;
            end;
            SDLK_ESCAPE:
            begin
              Result := #27;
              Done := True;
            end;
            SDLK_BACKSPACE:
            begin
              Result := #8;
              Done := True;
            end;
          end;
        end;
      end;
    end;

    if not Done then
      SDL_Delay(16);
  end;
end;

function TSDL2InputDevice.KeyPressed: Boolean;
var
  Event: TSDL_Event;
begin
  Result := False;

  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
        FQuitRequested := True;
      SDL_TEXTINPUT, SDL_KEYDOWN:
        Result := True;
    end;
  end;
end;

function TSDL2InputDevice.ShouldQuit: Boolean;
begin
  ProcessEvents;
  Result := FQuitRequested;
end;

procedure TSDL2InputDevice.ProcessEvents;
var
  Event: TSDL_Event;
  CtrlPressed, AltPressed: Boolean;
begin
  if FInputActive then
    ProcessInputEvents
  else
  begin
    while SDL_PollEvent(@Event) <> 0 do
    begin
      case Event.type_ of
        SDL_QUITEV:
          FQuitRequested := True;
        SDL_KEYDOWN:
        begin
          CtrlPressed := (Event.key.keysym.mod_ and KMOD_CTRL) <> 0;
          AltPressed := (Event.key.keysym.mod_ and KMOD_ALT) <> 0;

          // CTRL+ALT+END: Exit SedaiVision completely
          if (Event.key.keysym.sym = SDLK_END) and CtrlPressed and AltPressed then
          begin
            FQuitRequested := True;
          end
          // CTRL+C: Stop BASIC program (but don't exit)
          else if (Event.key.keysym.sym = SDLK_c) and CtrlPressed and (not AltPressed) then
          begin
            FStopRequested := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSDL2InputDevice.Reset;
begin
  EnsureCursorHidden;
  ClearInputBuffer;
  FQuitRequested := False;
  FStopRequested := False;
  FInputActive := False;
  FCurrentPrompt := '';
  FCursorVisible := False;
  FLastCursorToggle := SDL_GetTicks;
  FInputStartX := 0;
  FInputStartY := 0;
  FVisualCursorX := 0;
  FVisualCursorY := 0;
end;

function TSDL2InputDevice.ShouldStop: Boolean;
begin
  ProcessEvents;
  Result := FStopRequested;
end;

procedure TSDL2InputDevice.ClearStopRequest;
begin
  FStopRequested := False;
end;

end.
