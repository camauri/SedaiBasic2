{
  SedaiC128MemoryMapper.pas
  =========================
  Commodore 128 memory mapping implementation for PEEK/POKE compatibility.

  Maps C128 memory locations to SedaiBasic properties:
  - VIC-II registers (53248-53296): colors, sprites, screen control
  - SID registers (54272-54300): sound, volume
  - CIA registers (56320-56335, 56576-56591): joystick, timers
  - System variables: ST, TI, keyboard

  This is NOT an emulator - it's a compatibility facade that allows
  legacy BASIC code using PEEK/POKE to work with SedaiBasic.

  NOTE: This is a minimal implementation that uses only the methods
  available in the current ISpriteManager and IOutputDevice interfaces.
  Sprite-related PEEK/POKE operations use GetSpriteInfo/SetSprite methods.

  Copyright (c) 2024-2025 Artiforge
}
unit SedaiC128MemoryMapper;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils,
  SedaiMemoryMapper, SedaiOutputInterface;

type
  { TC128MemoryMapper - Commodore 128 memory mapping }
  TC128MemoryMapper = class(TMemoryMapper)
  private
    FOutputDevice: IOutputDevice;
    FSpriteManager: ISpriteManager;

    // Internal state for color sources (COLOR command)
    FBorderColor: Integer;
    FBackgroundColor: Integer;
    FVolume: Integer;

    // === VIC-II Color Registers (53280-53286) ===
    function ReadBorderColor(Address: Integer): Integer;
    procedure WriteBorderColor(Address: Integer; Value: Integer);
    function ReadBackgroundColor(Address: Integer): Integer;
    procedure WriteBackgroundColor(Address: Integer; Value: Integer);
    // Background colors 1-3 for multicolor modes
    function ReadBackgroundColor1(Address: Integer): Integer;
    procedure WriteBackgroundColor1(Address: Integer; Value: Integer);
    function ReadBackgroundColor2(Address: Integer): Integer;
    procedure WriteBackgroundColor2(Address: Integer; Value: Integer);
    function ReadBackgroundColor3(Address: Integer): Integer;
    procedure WriteBackgroundColor3(Address: Integer; Value: Integer);

    // === VIC-II Sprite Position Registers (53248-53263) ===
    function ReadSpritePosition(Address: Integer): Integer;
    procedure WriteSpritePosition(Address: Integer; Value: Integer);

    // === VIC-II Sprite X MSB (53264) ===
    function ReadSpriteXMSB(Address: Integer): Integer;
    procedure WriteSpriteXMSB(Address: Integer; Value: Integer);

    // === VIC-II Sprite Control Registers ===
    function ReadSpriteEnable(Address: Integer): Integer;
    procedure WriteSpriteEnable(Address: Integer; Value: Integer);
    function ReadSpriteYExpand(Address: Integer): Integer;
    procedure WriteSpriteYExpand(Address: Integer; Value: Integer);
    function ReadSpritePriority(Address: Integer): Integer;
    procedure WriteSpritePriority(Address: Integer; Value: Integer);
    function ReadSpriteMulticolorMode(Address: Integer): Integer;
    procedure WriteSpriteMulticolorMode(Address: Integer; Value: Integer);
    function ReadSpriteXExpand(Address: Integer): Integer;
    procedure WriteSpriteXExpand(Address: Integer; Value: Integer);

    // === VIC-II Sprite Multicolor (53285-53286) ===
    function ReadSpriteMulticolor0(Address: Integer): Integer;
    procedure WriteSpriteMulticolor0(Address: Integer; Value: Integer);
    function ReadSpriteMulticolor1(Address: Integer): Integer;
    procedure WriteSpriteMulticolor1(Address: Integer; Value: Integer);

    // === VIC-II Sprite Colors (53287-53294) ===
    function ReadSpriteColor(Address: Integer): Integer;
    procedure WriteSpriteColor(Address: Integer; Value: Integer);

    // === VIC-II Collision Registers (53278-53279) ===
    function ReadSpriteSpriteCollision(Address: Integer): Integer;
    function ReadSpriteBackgroundCollision(Address: Integer): Integer;

    // === SID Volume (54296) ===
    function ReadSIDVolume(Address: Integer): Integer;
    procedure WriteSIDVolume(Address: Integer; Value: Integer);

    // === Fast Mode (53296) ===
    function ReadFastMode(Address: Integer): Integer;
    procedure WriteFastMode(Address: Integer; Value: Integer);

    // === Zero Page: Cursor, Color, and System State ===
    function ReadKeyBufCount(Address: Integer): Integer;
    function ReadReverseFlag(Address: Integer): Integer;
    procedure WriteReverseFlag(Address: Integer; Value: Integer);
    function ReadQuoteMode(Address: Integer): Integer;
    function ReadCursorCol(Address: Integer): Integer;
    procedure WriteCursorCol(Address: Integer; Value: Integer);
    function ReadInsertMode(Address: Integer): Integer;
    function ReadCursorRow(Address: Integer): Integer;
    procedure WriteCursorRow(Address: Integer; Value: Integer);
    function ReadScreenLineLen(Address: Integer): Integer;
    function ReadCharColor(Address: Integer): Integer;
    procedure WriteCharColor(Address: Integer; Value: Integer);

    // === Screen Memory (1024-2023) - Only for GRAPHIC 0 text mode ===
    function ReadScreenMem(Address: Integer): Integer;
    procedure WriteScreenMem(Address: Integer; Value: Integer);

    // === Color RAM (55296-56295) - Only for GRAPHIC 0 text mode ===
    function ReadColorRAM(Address: Integer): Integer;
    procedure WriteColorRAM(Address: Integer; Value: Integer);

  protected
    procedure InitializeMappings; override;

  public
    constructor Create(OutputDevice: IOutputDevice; SpriteManager: ISpriteManager);
  end;

implementation

uses
  SedaiSpriteTypes;

const
  // VIC-II base address
  VIC_BASE = 53248;  // $D000

  // VIC-II register offsets
  VIC_SPRITE_X_Y = VIC_BASE;        // 53248-53263: Sprite X/Y positions
  VIC_SPRITE_X_MSB = VIC_BASE + 16; // 53264: Sprite X MSB
  VIC_CR1 = VIC_BASE + 17;          // 53265: Control Register 1
  VIC_RASTER = VIC_BASE + 18;       // 53266: Raster
  VIC_LIGHT_PEN_X = VIC_BASE + 19;  // 53267: Light pen X
  VIC_LIGHT_PEN_Y = VIC_BASE + 20;  // 53268: Light pen Y
  VIC_SPRITE_ENABLE = VIC_BASE + 21; // 53269: Sprite enable
  VIC_CR2 = VIC_BASE + 22;          // 53270: Control Register 2
  VIC_SPRITE_Y_EXPAND = VIC_BASE + 23; // 53271: Sprite Y expand
  VIC_MEMORY = VIC_BASE + 24;       // 53272: Memory pointers
  VIC_IRQ_STATUS = VIC_BASE + 25;   // 53273: IRQ status
  VIC_IRQ_ENABLE = VIC_BASE + 26;   // 53274: IRQ enable
  VIC_SPRITE_PRIORITY = VIC_BASE + 27; // 53275: Sprite priority
  VIC_SPRITE_MC_MODE = VIC_BASE + 28;  // 53276: Sprite multicolor mode
  VIC_SPRITE_X_EXPAND = VIC_BASE + 29; // 53277: Sprite X expand
  VIC_SPRITE_COLLISION = VIC_BASE + 30; // 53278: Sprite-sprite collision
  VIC_BG_COLLISION = VIC_BASE + 31;    // 53279: Sprite-background collision
  VIC_BORDER = VIC_BASE + 32;       // 53280: Border color
  VIC_BG0 = VIC_BASE + 33;          // 53281: Background 0
  VIC_BG1 = VIC_BASE + 34;          // 53282: Background 1
  VIC_BG2 = VIC_BASE + 35;          // 53283: Background 2
  VIC_BG3 = VIC_BASE + 36;          // 53284: Background 3
  VIC_SPRITE_MC0 = VIC_BASE + 37;   // 53285: Sprite multicolor 0
  VIC_SPRITE_MC1 = VIC_BASE + 38;   // 53286: Sprite multicolor 1
  VIC_SPRITE_COLOR_BASE = VIC_BASE + 39; // 53287-53294: Sprite colors 0-7
  VIC_KEYBOARD = VIC_BASE + 47;     // 53295: Keyboard interface (C128)
  VIC_FAST = VIC_BASE + 48;         // 53296: Fast mode (C128)

  // SID base address
  SID_BASE = 54272;  // $D400
  SID_VOLUME = SID_BASE + 24;       // 54296: Volume/filter mode

  // Zero page locations (C64/C128 BASIC)
  ZP_KEY_BUF_COUNT = 198;           // Number of characters in keyboard buffer
  ZP_REVERSE_FLAG = 199;            // Reverse mode flag (RVS ON/OFF)
  ZP_QUOTE_MODE = 208;              // Quote mode flag
  ZP_CURSOR_COL = 211;              // Current cursor column
  ZP_INSERT_MODE = 212;             // Insert mode flag / number of inserts
  ZP_CURSOR_ROW = 214;              // Current cursor row
  ZP_SCREEN_LINE_LEN = 216;         // Current screen line length (40 or 80)
  ZP_CHAR_COLOR = 646;              // Current character color

  // Screen memory (C64/C128)
  SCREEN_MEM_START = 1024;          // $0400: Screen memory start
  SCREEN_MEM_END = 2023;            // $07E7: Screen memory end (40x25=1000)

  // Color RAM (C64/C128)
  COLOR_RAM_START = 55296;          // $D800: Color RAM start
  COLOR_RAM_END = 56295;            // $DBE7: Color RAM end (40x25=1000)

{ TC128MemoryMapper }

constructor TC128MemoryMapper.Create(OutputDevice: IOutputDevice; SpriteManager: ISpriteManager);
begin
  FOutputDevice := OutputDevice;
  FSpriteManager := SpriteManager;
  FBorderColor := 14;    // Light blue default
  FBackgroundColor := 6; // Blue default
  FVolume := 15;         // Max volume
  inherited Create;
  SetMachineName('Commodore 128');
end;

procedure TC128MemoryMapper.InitializeMappings;
begin
  inherited;

  // === VIC-II Sprite Positions (53248-53263) ===
  RegisterMapping(VIC_SPRITE_X_Y, VIC_SPRITE_X_Y + 15, mmtReadWrite,
    @ReadSpritePosition, @WriteSpritePosition,
    'VIC-II Sprite X/Y positions');

  // === VIC-II Sprite X MSB (53264) ===
  RegisterSingleMapping(VIC_SPRITE_X_MSB, mmtReadWrite,
    @ReadSpriteXMSB, @WriteSpriteXMSB,
    'VIC-II Sprite X MSB');

  // === VIC-II Sprite Enable (53269) ===
  RegisterSingleMapping(VIC_SPRITE_ENABLE, mmtReadWrite,
    @ReadSpriteEnable, @WriteSpriteEnable,
    'VIC-II Sprite enable');

  // === VIC-II Sprite Y Expand (53271) ===
  RegisterSingleMapping(VIC_SPRITE_Y_EXPAND, mmtReadWrite,
    @ReadSpriteYExpand, @WriteSpriteYExpand,
    'VIC-II Sprite Y expand');

  // === VIC-II Sprite Priority (53275) ===
  RegisterSingleMapping(VIC_SPRITE_PRIORITY, mmtReadWrite,
    @ReadSpritePriority, @WriteSpritePriority,
    'VIC-II Sprite priority');

  // === VIC-II Sprite Multicolor Mode (53276) ===
  RegisterSingleMapping(VIC_SPRITE_MC_MODE, mmtReadWrite,
    @ReadSpriteMulticolorMode, @WriteSpriteMulticolorMode,
    'VIC-II Sprite multicolor mode');

  // === VIC-II Sprite X Expand (53277) ===
  RegisterSingleMapping(VIC_SPRITE_X_EXPAND, mmtReadWrite,
    @ReadSpriteXExpand, @WriteSpriteXExpand,
    'VIC-II Sprite X expand');

  // === VIC-II Collision Registers (53278-53279) - Read-only ===
  RegisterSingleMapping(VIC_SPRITE_COLLISION, mmtReadOnly,
    @ReadSpriteSpriteCollision, nil,
    'VIC-II Sprite-sprite collision');
  RegisterSingleMapping(VIC_BG_COLLISION, mmtReadOnly,
    @ReadSpriteBackgroundCollision, nil,
    'VIC-II Sprite-background collision');

  // === VIC-II Color Registers (53280-53284) ===
  RegisterSingleMapping(VIC_BORDER, mmtReadWrite,
    @ReadBorderColor, @WriteBorderColor,
    'VIC-II Border color');
  RegisterSingleMapping(VIC_BG0, mmtReadWrite,
    @ReadBackgroundColor, @WriteBackgroundColor,
    'VIC-II Background color 0');
  RegisterSingleMapping(VIC_BG1, mmtReadWrite,
    @ReadBackgroundColor1, @WriteBackgroundColor1,
    'VIC-II Background color 1 (multicolor)');
  RegisterSingleMapping(VIC_BG2, mmtReadWrite,
    @ReadBackgroundColor2, @WriteBackgroundColor2,
    'VIC-II Background color 2 (multicolor)');
  RegisterSingleMapping(VIC_BG3, mmtReadWrite,
    @ReadBackgroundColor3, @WriteBackgroundColor3,
    'VIC-II Background color 3 (multicolor)');

  // === VIC-II Sprite Multicolors (53285-53286) ===
  RegisterSingleMapping(VIC_SPRITE_MC0, mmtReadWrite,
    @ReadSpriteMulticolor0, @WriteSpriteMulticolor0,
    'VIC-II Sprite multicolor 0');
  RegisterSingleMapping(VIC_SPRITE_MC1, mmtReadWrite,
    @ReadSpriteMulticolor1, @WriteSpriteMulticolor1,
    'VIC-II Sprite multicolor 1');

  // === VIC-II Sprite Colors (53287-53294) ===
  RegisterMapping(VIC_SPRITE_COLOR_BASE, VIC_SPRITE_COLOR_BASE + 7, mmtReadWrite,
    @ReadSpriteColor, @WriteSpriteColor,
    'VIC-II Sprite colors 0-7');

  // === VIC-II Fast Mode (53296) - C128 only ===
  RegisterSingleMapping(VIC_FAST, mmtReadWrite,
    @ReadFastMode, @WriteFastMode,
    'VIC-II Fast mode (C128)');

  // === SID Volume (54296) ===
  RegisterSingleMapping(SID_VOLUME, mmtReadWrite,
    @ReadSIDVolume, @WriteSIDVolume,
    'SID Volume/filter mode');

  // === Zero Page: Cursor, Color, and System State ===
  RegisterSingleMapping(ZP_KEY_BUF_COUNT, mmtReadOnly,
    @ReadKeyBufCount, nil,
    'Keyboard buffer count');
  RegisterSingleMapping(ZP_REVERSE_FLAG, mmtReadWrite,
    @ReadReverseFlag, @WriteReverseFlag,
    'Reverse mode flag');
  RegisterSingleMapping(ZP_QUOTE_MODE, mmtReadOnly,
    @ReadQuoteMode, nil,
    'Quote mode flag');
  RegisterSingleMapping(ZP_CURSOR_COL, mmtReadWrite,
    @ReadCursorCol, @WriteCursorCol,
    'Cursor column position');
  RegisterSingleMapping(ZP_INSERT_MODE, mmtReadOnly,
    @ReadInsertMode, nil,
    'Insert mode flag');
  RegisterSingleMapping(ZP_CURSOR_ROW, mmtReadWrite,
    @ReadCursorRow, @WriteCursorRow,
    'Cursor row position');
  RegisterSingleMapping(ZP_SCREEN_LINE_LEN, mmtReadOnly,
    @ReadScreenLineLen, nil,
    'Screen line length');
  RegisterSingleMapping(ZP_CHAR_COLOR, mmtReadWrite,
    @ReadCharColor, @WriteCharColor,
    'Current character color');

  // === Screen Memory (1024-2023) - Only for C128 text modes ===
  RegisterMapping(SCREEN_MEM_START, SCREEN_MEM_END, mmtReadWrite,
    @ReadScreenMem, @WriteScreenMem,
    'Screen memory (40x25 text)');

  // === Color RAM (55296-56295) - Only for C128 text modes ===
  RegisterMapping(COLOR_RAM_START, COLOR_RAM_END, mmtReadWrite,
    @ReadColorRAM, @WriteColorRAM,
    'Color RAM (40x25 text)');
end;

// === VIC-II Color Register Implementations ===

function TC128MemoryMapper.ReadBorderColor(Address: Integer): Integer;
begin
  // Use color source 4 (border) if available, otherwise internal state
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(4) and $0F
  else
    Result := FBorderColor and $0F;
end;

procedure TC128MemoryMapper.WriteBorderColor(Address: Integer; Value: Integer);
begin
  FBorderColor := Value and $0F;
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(4, Value and $0F);  // Color source 4 = border (0-based)
end;

function TC128MemoryMapper.ReadBackgroundColor(Address: Integer): Integer;
begin
  // Use color source 0 (background) if available
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(0) and $0F
  else
    Result := FBackgroundColor and $0F;
end;

procedure TC128MemoryMapper.WriteBackgroundColor(Address: Integer; Value: Integer);
begin
  FBackgroundColor := Value and $0F;
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(0, Value and $0F);  // Color source 0 = background (0-based)
end;

function TC128MemoryMapper.ReadBackgroundColor1(Address: Integer): Integer;
begin
  // Color source 2 = multicolor 1
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(2) and $0F
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteBackgroundColor1(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(2, Value and $0F);  // Color source 2 = multicolor 1
end;

function TC128MemoryMapper.ReadBackgroundColor2(Address: Integer): Integer;
begin
  // Color source 3 = multicolor 2
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(3) and $0F
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteBackgroundColor2(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(3, Value and $0F);  // Color source 3 = multicolor 2
end;

function TC128MemoryMapper.ReadBackgroundColor3(Address: Integer): Integer;
begin
  // Color source 1 = foreground (used as BG3 in extended color mode)
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(1) and $0F
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteBackgroundColor3(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(1, Value and $0F);  // Color source 1 = foreground
end;

// === VIC-II Sprite Position Implementations ===

function TC128MemoryMapper.ReadSpritePosition(Address: Integer): Integer;
var
  SpriteNum: Integer;
  IsY: Boolean;
  X, Y: Double;
begin
  // Addresses 53248-53263: alternating X, Y for sprites 0-7
  // Even offset = X, Odd offset = Y
  SpriteNum := ((Address - VIC_SPRITE_X_Y) div 2) + 1;  // 1-based
  IsY := ((Address - VIC_SPRITE_X_Y) mod 2) = 1;

  Result := 0;
  if Assigned(FSpriteManager) and (SpriteNum >= 1) and (SpriteNum <= 8) then
  begin
    // GetSpritePosition: attr 0=X, 1=Y
    if IsY then
    begin
      Y := FSpriteManager.GetSpritePosition(SpriteNum, 1);
      Result := Round(Y) and $FF;  // Y is 8-bit
    end
    else
    begin
      X := FSpriteManager.GetSpritePosition(SpriteNum, 0);
      Result := Round(X) and $FF;  // X low 8 bits
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpritePosition(Address: Integer; Value: Integer);
var
  SpriteNum: Integer;
  IsY: Boolean;
  Info: TSpriteInfo;
  X, Y: Double;
  XMSB: Integer;
begin
  SpriteNum := ((Address - VIC_SPRITE_X_Y) div 2) + 1;
  IsY := ((Address - VIC_SPRITE_X_Y) mod 2) = 1;

  if Assigned(FSpriteManager) and (SpriteNum >= 1) and (SpriteNum <= 8) then
  begin
    Info := FSpriteManager.GetSpriteInfo(SpriteNum);
    if IsY then
    begin
      // Set Y position, keep X
      FSpriteManager.MoveSpriteAbs(SpriteNum, Info.X, Value and $FF);
    end
    else
    begin
      // Set X low byte, preserve MSB
      XMSB := Round(Info.X) and $100;  // Preserve bit 8
      FSpriteManager.MoveSpriteAbs(SpriteNum, XMSB or (Value and $FF), Info.Y);
    end;
  end;
end;

function TC128MemoryMapper.ReadSpriteXMSB(Address: Integer): Integer;
var
  I: Integer;
  X: Double;
begin
  // Bit n = MSB of sprite n X position
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      X := FSpriteManager.GetSpritePosition(I, 0);
      if (Round(X) and $100) <> 0 then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpriteXMSB(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  XLow, NewXMSB: Integer;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      XLow := Round(Info.X) and $FF;  // Low 8 bits
      if (Value and (1 shl (I - 1))) <> 0 then
        NewXMSB := $100
      else
        NewXMSB := 0;
      FSpriteManager.MoveSpriteAbs(I, XLow or NewXMSB, Info.Y);
    end;
  end;
end;

// === VIC-II Sprite Control Implementations ===

function TC128MemoryMapper.ReadSpriteEnable(Address: Integer): Integer;
var
  I: Integer;
  Info: TSpriteInfo;
begin
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if Info.Enabled then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpriteEnable(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  Enabled: Integer;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if (Value and (1 shl (I - 1))) <> 0 then
        Enabled := 1
      else
        Enabled := 0;
      // SetSprite(Num, Enabled, Color, Priority, ScaleX, ScaleY, Mode)
      // Use -1 to keep current values
      FSpriteManager.SetSprite(I, Enabled, Info.Color, -1, -1, -1, -1);
    end;
  end;
end;

function TC128MemoryMapper.ReadSpriteYExpand(Address: Integer): Integer;
var
  I: Integer;
  Info: TSpriteInfo;
begin
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if Info.ScaleY >= 2.0 then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpriteYExpand(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  NewScaleY: Double;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if (Value and (1 shl (I - 1))) <> 0 then
        NewScaleY := 2.0
      else
        NewScaleY := 1.0;
      FSpriteManager.SetSprite(I, -1, Info.Color, -1, Info.ScaleX, NewScaleY, -1);
    end;
  end;
end;

function TC128MemoryMapper.ReadSpritePriority(Address: Integer): Integer;
var
  I: Integer;
  Info: TSpriteInfo;
begin
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      // Bit set = behind background (priority 1)
      if Info.Priority > 0 then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpritePriority(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  NewPriority: Integer;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if (Value and (1 shl (I - 1))) <> 0 then
        NewPriority := 1  // Behind background
      else
        NewPriority := 0;  // In front of background
      FSpriteManager.SetSprite(I, -1, Info.Color, NewPriority, -1, -1, -1);
    end;
  end;
end;

function TC128MemoryMapper.ReadSpriteMulticolorMode(Address: Integer): Integer;
var
  I: Integer;
  Info: TSpriteInfo;
begin
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if Info.MulticolorMode then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpriteMulticolorMode(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  NewMode: Integer;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      // Set multicolor mode: 1=multicolor, 0=standard
      if (Value and (1 shl (I - 1))) <> 0 then
        NewMode := 1  // Multicolor
      else
        NewMode := 0;  // Standard
      FSpriteManager.SetSprite(I, -1, Info.Color, -1, -1, -1, NewMode);
    end;
  end;
end;

function TC128MemoryMapper.ReadSpriteXExpand(Address: Integer): Integer;
var
  I: Integer;
  Info: TSpriteInfo;
begin
  Result := 0;
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if Info.ScaleX >= 2.0 then
        Result := Result or (1 shl (I - 1));
    end;
  end;
end;

procedure TC128MemoryMapper.WriteSpriteXExpand(Address: Integer; Value: Integer);
var
  I: Integer;
  Info: TSpriteInfo;
  NewScaleX: Double;
begin
  if Assigned(FSpriteManager) then
  begin
    for I := 1 to 8 do
    begin
      Info := FSpriteManager.GetSpriteInfo(I);
      if (Value and (1 shl (I - 1))) <> 0 then
        NewScaleX := 2.0
      else
        NewScaleX := 1.0;
      FSpriteManager.SetSprite(I, -1, Info.Color, -1, NewScaleX, Info.ScaleY, -1);
    end;
  end;
end;

// === VIC-II Sprite Multicolor Implementations ===

function TC128MemoryMapper.ReadSpriteMulticolor0(Address: Integer): Integer;
var
  MC: TSpriteColor;
begin
  if Assigned(FSpriteManager) then
  begin
    MC := FSpriteManager.GetMulticolor(1);  // MC1
    if MC.Mode = scmIndexed then
      Result := MC.Index and $0F
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteSpriteMulticolor0(Address: Integer; Value: Integer);
var
  MC1, MC2: TSpriteColor;
begin
  if Assigned(FSpriteManager) then
  begin
    MC1 := MakeIndexedColor(Value and $0F);
    MC2.Mode := scmIndexed;
    MC2.Index := 255;  // Keep current
    FSpriteManager.SetSpriteMulticolors(MC1, MC2);
  end;
end;

function TC128MemoryMapper.ReadSpriteMulticolor1(Address: Integer): Integer;
var
  MC: TSpriteColor;
begin
  if Assigned(FSpriteManager) then
  begin
    MC := FSpriteManager.GetMulticolor(2);  // MC2
    if MC.Mode = scmIndexed then
      Result := MC.Index and $0F
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteSpriteMulticolor1(Address: Integer; Value: Integer);
var
  MC1, MC2: TSpriteColor;
begin
  if Assigned(FSpriteManager) then
  begin
    MC1.Mode := scmIndexed;
    MC1.Index := 255;  // Keep current
    MC2 := MakeIndexedColor(Value and $0F);
    FSpriteManager.SetSpriteMulticolors(MC1, MC2);
  end;
end;

// === VIC-II Sprite Color Implementations ===

function TC128MemoryMapper.ReadSpriteColor(Address: Integer): Integer;
var
  SpriteNum: Integer;
  Info: TSpriteInfo;
begin
  // Address 53287-53294 maps to sprites 1-8 (0-7 in hardware)
  SpriteNum := (Address - VIC_SPRITE_COLOR_BASE) + 1;  // 1-based for SedaiBasic
  Result := 0;
  if Assigned(FSpriteManager) and (SpriteNum >= 1) and (SpriteNum <= 8) then
  begin
    Info := FSpriteManager.GetSpriteInfo(SpriteNum);
    if Info.Color.Mode = scmIndexed then
      Result := Info.Color.Index and $0F
    else
      Result := 0;  // RGBA sprites don't have indexed color
  end;
end;

procedure TC128MemoryMapper.WriteSpriteColor(Address: Integer; Value: Integer);
var
  SpriteNum: Integer;
  NewColor: TSpriteColor;
begin
  SpriteNum := (Address - VIC_SPRITE_COLOR_BASE) + 1;
  if Assigned(FSpriteManager) and (SpriteNum >= 1) and (SpriteNum <= 8) then
  begin
    NewColor := MakeIndexedColor(Value and $0F);
    FSpriteManager.SetSprite(SpriteNum, -1, NewColor, -1, -1, -1, -1);
  end;
end;

// === VIC-II Collision Register Implementations ===

function TC128MemoryMapper.ReadSpriteSpriteCollision(Address: Integer): Integer;
begin
  if Assigned(FSpriteManager) then
    Result := FSpriteManager.GetCollisionStatus(COLLISION_SPRITE_SPRITE) and $FF
  else
    Result := 0;
end;

function TC128MemoryMapper.ReadSpriteBackgroundCollision(Address: Integer): Integer;
begin
  if Assigned(FSpriteManager) then
    Result := FSpriteManager.GetCollisionStatus(COLLISION_SPRITE_DISPLAY) and $FF
  else
    Result := 0;
end;

// === SID Volume Implementation ===

function TC128MemoryMapper.ReadSIDVolume(Address: Integer): Integer;
begin
  Result := FVolume and $0F;
end;

procedure TC128MemoryMapper.WriteSIDVolume(Address: Integer; Value: Integer);
begin
  FVolume := Value and $0F;
  // Note: SedaiBasic doesn't have audio volume control yet
end;

// === Fast Mode Implementation ===

function TC128MemoryMapper.ReadFastMode(Address: Integer): Integer;
begin
  if Assigned(FOutputDevice) then
  begin
    if FOutputDevice.GetFastMode then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteFastMode(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetFastMode((Value and 1) <> 0);
end;

// === Zero Page: Cursor, Color, and System State Implementation ===

function TC128MemoryMapper.ReadKeyBufCount(Address: Integer): Integer;
begin
  // Return 0 - we don't have access to keyboard buffer count from here
  // This would require IInputDevice access which we don't have
  Result := 0;
end;

function TC128MemoryMapper.ReadReverseFlag(Address: Integer): Integer;
begin
  // Return 0 for now - reverse flag not exposed in interface
  // Could be added to IOutputDevice in the future
  Result := 0;
end;

procedure TC128MemoryMapper.WriteReverseFlag(Address: Integer; Value: Integer);
begin
  // No-op for now - reverse flag not exposed in interface
  // Could be added to IOutputDevice in the future
end;

function TC128MemoryMapper.ReadQuoteMode(Address: Integer): Integer;
begin
  // Return 0 - quote mode is internal to input handling
  Result := 0;
end;

function TC128MemoryMapper.ReadCursorCol(Address: Integer): Integer;
begin
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetCursorX
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteCursorCol(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetCursor(Value, FOutputDevice.GetCursorY);
end;

function TC128MemoryMapper.ReadInsertMode(Address: Integer): Integer;
begin
  // Return 0 - insert mode flag not exposed in interface
  Result := 0;
end;

function TC128MemoryMapper.ReadCursorRow(Address: Integer): Integer;
begin
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetCursorY
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteCursorRow(Address: Integer; Value: Integer);
begin
  if Assigned(FOutputDevice) then
    FOutputDevice.SetCursor(FOutputDevice.GetCursorX, Value);
end;

function TC128MemoryMapper.ReadScreenLineLen(Address: Integer): Integer;
begin
  // Return screen width (40 or 80 depending on mode)
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetScreenWidth
  else
    Result := 40;  // Default to 40 columns
end;

function TC128MemoryMapper.ReadCharColor(Address: Integer): Integer;
begin
  // Color source 5 = character/text color
  if Assigned(FOutputDevice) then
    Result := FOutputDevice.GetColorSourceDirect(5) and $0F
  else
    Result := 0;
end;

procedure TC128MemoryMapper.WriteCharColor(Address: Integer; Value: Integer);
begin
  // Color source 5 = character/text color
  if Assigned(FOutputDevice) then
    FOutputDevice.SetColorSourceDirect(5, Value and $0F);
end;

// === Screen Memory Implementation (1024-2023) ===
// Maps to the 40x25 text buffer. Memory is always accessible regardless of
// current graphics mode - the mode only affects what is displayed on screen.
// Address 1024 = position (0,0), 1025 = position (1,0), etc.
// Row = (Address - 1024) div 40, Col = (Address - 1024) mod 40

function TC128MemoryMapper.ReadScreenMem(Address: Integer): Integer;
var
  Offset, Col, Row: Integer;
begin
  if not Assigned(FOutputDevice) then
  begin
    Result := 32; // Space
    Exit;
  end;

  Offset := Address - SCREEN_MEM_START;
  Col := Offset mod 40;
  Row := Offset div 40;

  // Check bounds (40x25 = 1000 characters)
  if (Offset < 0) or (Offset >= 1000) then
  begin
    Result := 32;
    Exit;
  end;

  Result := FOutputDevice.GetCharAt(Col, Row);
end;

procedure TC128MemoryMapper.WriteScreenMem(Address: Integer; Value: Integer);
var
  Offset, Col, Row: Integer;
begin
  if not Assigned(FOutputDevice) then
    Exit;

  Offset := Address - SCREEN_MEM_START;
  Col := Offset mod 40;
  Row := Offset div 40;

  // Check bounds (40x25 = 1000 characters)
  if (Offset < 0) or (Offset >= 1000) then
    Exit;

  FOutputDevice.SetCharAt(Col, Row, Value and $FF);
end;

// === Color RAM Implementation (55296-56295) ===
// Maps to the 40x25 color buffer. Memory is always accessible regardless of
// current graphics mode - the mode only affects what is displayed on screen.
// Address 55296 = color at position (0,0), etc.

function TC128MemoryMapper.ReadColorRAM(Address: Integer): Integer;
var
  Offset, Col, Row: Integer;
begin
  if not Assigned(FOutputDevice) then
  begin
    Result := 0;
    Exit;
  end;

  Offset := Address - COLOR_RAM_START;
  Col := Offset mod 40;
  Row := Offset div 40;

  // Check bounds (40x25 = 1000 characters)
  if (Offset < 0) or (Offset >= 1000) then
  begin
    Result := 0;
    Exit;
  end;

  Result := FOutputDevice.GetColorAt(Col, Row) and $0F;
end;

procedure TC128MemoryMapper.WriteColorRAM(Address: Integer; Value: Integer);
var
  Offset, Col, Row: Integer;
begin
  if not Assigned(FOutputDevice) then
    Exit;

  Offset := Address - COLOR_RAM_START;
  Col := Offset mod 40;
  Row := Offset div 40;

  // Check bounds (40x25 = 1000 characters)
  if (Offset < 0) or (Offset >= 1000) then
    Exit;

  FOutputDevice.SetColorAt(Col, Row, Value and $0F);
end;

end.
