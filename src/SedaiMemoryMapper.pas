{
  SedaiMemoryMapper.pas
  =====================
  Generic memory mapping interface for PEEK/POKE compatibility layer.

  This unit provides a flexible, reusable architecture for mapping
  legacy memory locations to modern properties. It is NOT an emulator -
  it's a facade that translates PEEK/POKE calls to appropriate
  SedaiBasic property access.

  Design Philosophy:
  - No physical memory storage - values exist in class properties
  - PEEK reads from mapped properties
  - POKE writes to mapped properties
  - Unmapped locations return 0 (PEEK) or are ignored (POKE)

  Extensibility:
  - Create subclasses for different computers (C128, C64, ZX Spectrum, etc.)
  - Each subclass registers its own mappings in InitializeMappings

  Copyright (c) 2024-2025 Artiforge
}
unit SedaiMemoryMapper;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils;

type
  // Type of memory mapping
  TMemoryMappingType = (
    mmtReadOnly,      // Only PEEK allowed
    mmtWriteOnly,     // Only POKE allowed
    mmtReadWrite,     // Both PEEK and POKE allowed
    mmtNotMapped      // Location not supported (returns 0, ignores writes)
  );

  // Forward declaration
  TMemoryMapper = class;

  // Callback types for read/write operations
  TMemoryReadFunc = function(Address: Integer): Integer of object;
  TMemoryWriteProc = procedure(Address: Integer; Value: Integer) of object;

  // Memory mapping entry
  TMemoryMapEntry = record
    AddressStart: Integer;        // Start address (inclusive)
    AddressEnd: Integer;          // End address (inclusive, same as start for single)
    MappingType: TMemoryMappingType;
    ReadFunc: TMemoryReadFunc;    // Called for PEEK
    WriteProc: TMemoryWriteProc;  // Called for POKE
    Description: string;          // For debugging/documentation
  end;
  PMemoryMapEntry = ^TMemoryMapEntry;

  // Dynamic array of mappings
  TMemoryMapEntryArray = array of TMemoryMapEntry;

  { IMemoryMapper - Interface for memory mapping operations }
  { NOTE: Using CORBA interface (no reference counting) for consistency with SedaiBasic }
  IMemoryMapper = interface
    function Peek(Address: Integer): Integer;
    procedure Poke(Address: Integer; Value: Integer);
    function GetMappingType(Address: Integer): TMemoryMappingType;
    function GetDescription(Address: Integer): string;
    function GetMachineName: string;
  end;

  { TMemoryMapper - Base class for memory mapping }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TMemoryMapper = class(TObject, IMemoryMapper)
  private
    FMappings: TMemoryMapEntryArray;
    FMappingCount: Integer;
    FDefaultValue: Integer;
    FMachineName: string;

    // Find mapping entry for address (returns nil if not found)
    function FindMapping(Address: Integer): PMemoryMapEntry;

  protected
    // Register a range mapping (AddressStart to AddressEnd inclusive)
    procedure RegisterMapping(
      AddrStart, AddrEnd: Integer;
      MappingType: TMemoryMappingType;
      ReadFunc: TMemoryReadFunc;
      WriteProc: TMemoryWriteProc;
      const Description: string
    );

    // Register a single address mapping
    procedure RegisterSingleMapping(
      Addr: Integer;
      MappingType: TMemoryMappingType;
      ReadFunc: TMemoryReadFunc;
      WriteProc: TMemoryWriteProc;
      const Description: string
    );

    // Override in subclasses to register machine-specific mappings
    procedure InitializeMappings; virtual;

    // Set machine name (call in subclass constructor)
    procedure SetMachineName(const Name: string);

  public
    constructor Create;
    destructor Destroy; override;

    // IMemoryMapper implementation
    function Peek(Address: Integer): Integer; virtual;
    procedure Poke(Address: Integer; Value: Integer); virtual;
    function GetMappingType(Address: Integer): TMemoryMappingType;
    function GetDescription(Address: Integer): string;
    function GetMachineName: string;

    // Default value for unmapped locations (default: 0)
    property DefaultValue: Integer read FDefaultValue write FDefaultValue;
  end;

implementation

{ TMemoryMapper }

constructor TMemoryMapper.Create;
begin
  inherited Create;
  FMappingCount := 0;
  SetLength(FMappings, 0);
  FDefaultValue := 0;
  FMachineName := 'Generic';
  InitializeMappings;
end;

destructor TMemoryMapper.Destroy;
begin
  SetLength(FMappings, 0);
  inherited Destroy;
end;

function TMemoryMapper.FindMapping(Address: Integer): PMemoryMapEntry;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMappingCount - 1 do
  begin
    if (Address >= FMappings[I].AddressStart) and
       (Address <= FMappings[I].AddressEnd) then
    begin
      Result := @FMappings[I];
      Exit;
    end;
  end;
end;

procedure TMemoryMapper.RegisterMapping(
  AddrStart, AddrEnd: Integer;
  MappingType: TMemoryMappingType;
  ReadFunc: TMemoryReadFunc;
  WriteProc: TMemoryWriteProc;
  const Description: string
);
begin
  // Grow array if needed
  if FMappingCount >= Length(FMappings) then
    SetLength(FMappings, FMappingCount + 32);

  FMappings[FMappingCount].AddressStart := AddrStart;
  FMappings[FMappingCount].AddressEnd := AddrEnd;
  FMappings[FMappingCount].MappingType := MappingType;
  FMappings[FMappingCount].ReadFunc := ReadFunc;
  FMappings[FMappingCount].WriteProc := WriteProc;
  FMappings[FMappingCount].Description := Description;
  Inc(FMappingCount);
end;

procedure TMemoryMapper.RegisterSingleMapping(
  Addr: Integer;
  MappingType: TMemoryMappingType;
  ReadFunc: TMemoryReadFunc;
  WriteProc: TMemoryWriteProc;
  const Description: string
);
begin
  RegisterMapping(Addr, Addr, MappingType, ReadFunc, WriteProc, Description);
end;

procedure TMemoryMapper.InitializeMappings;
begin
  // Override in subclasses to register machine-specific mappings
end;

procedure TMemoryMapper.SetMachineName(const Name: string);
begin
  FMachineName := Name;
end;

function TMemoryMapper.Peek(Address: Integer): Integer;
var
  Entry: PMemoryMapEntry;
begin
  Entry := FindMapping(Address);

  if Entry = nil then
  begin
    // Unmapped location - return default value
    Result := FDefaultValue;
    Exit;
  end;

  if Entry^.MappingType = mmtWriteOnly then
  begin
    // Write-only location - return default value
    Result := FDefaultValue;
    Exit;
  end;

  if Assigned(Entry^.ReadFunc) then
    Result := Entry^.ReadFunc(Address)
  else
    Result := FDefaultValue;
end;

procedure TMemoryMapper.Poke(Address: Integer; Value: Integer);
var
  Entry: PMemoryMapEntry;
begin
  Entry := FindMapping(Address);

  if Entry = nil then
  begin
    // Unmapped location - silently ignore (like real hardware)
    Exit;
  end;

  if Entry^.MappingType = mmtReadOnly then
  begin
    // Read-only location - silently ignore
    Exit;
  end;

  if Assigned(Entry^.WriteProc) then
    Entry^.WriteProc(Address, Value);
  // If no write proc, silently ignore
end;

function TMemoryMapper.GetMappingType(Address: Integer): TMemoryMappingType;
var
  Entry: PMemoryMapEntry;
begin
  Entry := FindMapping(Address);
  if Entry = nil then
    Result := mmtNotMapped
  else
    Result := Entry^.MappingType;
end;

function TMemoryMapper.GetDescription(Address: Integer): string;
var
  Entry: PMemoryMapEntry;
begin
  Entry := FindMapping(Address);
  if Entry = nil then
    Result := 'Not mapped'
  else
    Result := Entry^.Description;
end;

function TMemoryMapper.GetMachineName: string;
begin
  Result := FMachineName;
end;

end.
