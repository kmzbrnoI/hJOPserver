unit libPreload;

{
  Allows to preload any dll libraries at start of an apllication
  (before all other dll libraries -- e.g. RCS & Trakce). Unloads loaded
  libraries after all other dll libraries are unloaded.

  This is useful for creating static data, which are required by libraries.
}

interface

uses Generics.Collections, SysUtils, IniFiles, Classes, Windows;

type
  TLibPreload = class
  private
    preloaded: TList<NativeUInt>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Preload(ini: TMemIniFile; section: string); overload;
    procedure Preload(path: string); overload;
    procedure Unload();
  end;

var Preload: TLibPreload;

implementation

uses Logging;

constructor TLibPreload.Create();
begin
  inherited;
  Self.preloaded := TList<NativeUInt>.Create();
end;

destructor TLibPreload.Destroy();
begin
  Self.Unload();
  Self.preloaded.Free();
  inherited;
end;

procedure TLibPreload.Unload();
begin
  for var i: Integer := Self.preloaded.Count - 1 downto 0 do
    FreeLibrary(Self.preloaded[i]);
  Self.preloaded.Clear();
end;

procedure TLibPreload.Preload(path: string);
var handle: NativeUInt;
begin
  handle := LoadLibrary(PChar(path));
  if (handle = 0) then
  begin
    Log('Nelze naèíst preload knihovnu: ' + path, llError);
  end else begin
    Log('Naètena preload knihovna: ' + path, llInfo);
    Self.preloaded.Add(handle);
  end;
end;

procedure TLibPreload.Preload(ini: TMemIniFile; section: string);
var keys: TStrings;
begin
  keys := TStringList.Create();
  try
    ini.ReadSection(section, keys);
    for var key in keys do
    begin
      var path := ini.ReadString(section, key, '');
      Self.Preload(path);
    end;
  finally
    keys.Free();
  end;
end;

initialization

Preload := TLibPreload.Create();

finalization

FreeAndNil(Preload);

end.
