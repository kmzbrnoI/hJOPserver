unit BoosterDb;

{ This unit defines booster database. }

interface

uses Booster, IniFiles, Classes, SysUtils, Windows, Generics.Collections,
  Generics.Defaults;

type
  BoosterExistsException = class(Exception);

  TBoosterDb = class
  private const
    _BEEP_INTERVAL = 1; // in seconds

  private
    db: TDictionary<string, TBooster>;
    sortedKeys: TList<TBooster>;

    Beep: record // beep on overload
      NextBeep: TDateTime; // time of next beep
    end;

    // events from TBooster; those events direct call blk methods
    procedure OnOverloadChange(Sender: TObject; state: TBoosterSignal);
    procedure OnPowerChange(Sender: TObject; state: TBoosterSignal);
    procedure OnDCCChange(Sender: TObject; state: TBoosterSignal);

    procedure ControlBeep();

    procedure Clear();
    function GetCount(): Integer;
    function GetItem(key: string): TBooster;

  public

    constructor Create(inifilename: string = '');
    destructor Destroy(); override;

    procedure Add(new: TBooster);
    procedure Remove(id: string);

    procedure LoadFromFile(inifilename: string);
    procedure SaveToFile(inifilename: string);

    procedure Update();
    procedure SyncStructures();

    function ContainsKey(key: string; ignore: TBooster = nil): Boolean;

    property Items[index: string]: TBooster read GetItem; default;
    function GetEnumerator(): TDictionary<string, TBooster>.TValueEnumerator; overload;
    property Count: Integer read GetCount;
    property sorted: TList<TBooster> read sortedKeys;

  end; // TBoosterDb

var Boosters: TBoosterDb;

implementation

uses BlockDb, fMain, TrakceC, appEv, logging, DataZesilovac, TrakceIFace;

/// /////////////////////////////////////////////////////////////////////////////

// booster ini file format:
// ini file
// [id1] ... [id2] ... [id3] ...

constructor TBoosterDb.Create(inifilename: string = '');
begin
  inherited Create();
  Self.db := TDictionary<string, TBooster>.Create();
  Self.sortedKeys := TList<TBooster>.Create(TBooster.IdComparer());
  if (inifilename <> '') then
    Self.LoadFromFile(inifilename);
end;

destructor TBoosterDb.Destroy();
begin
  Self.Clear();
  Self.db.Free();
  Self.sortedKeys.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

// reads all sections
procedure TBoosterDb.LoadFromFile(inifilename: string);
var ini: TMemIniFile;
  sections: TStrings;
begin
  Log('Načítám zesilovače: ' + inifilename, TLogLevel.llInfo, lsData);

  Self.Clear();

  try
    ini := TMemIniFile.Create(inifilename, TEncoding.UTF8);
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'Načítám zesilovače: nelze otevrit soubor bloku');
      Exit();
    end;
  end;
  sections := TStringList.Create();

  ini.ReadSections(sections);

  for var id in sections do
  begin
    if (id = '') then
    begin
      Log('WARNING: prázdný primární klíč zesilovače - přeskakuji', llError, lsData);
      continue;
    end;
    if (Self.db.ContainsKey(id)) then
    begin
      Log('Duplicita primárního klíče zesilovače (' + id + ') - přeskakuji', llWarning, lsData);
      continue;
    end;

    var Booster: TBooster := nil;

    try
      Booster := TBooster.Create(ini, id);

      Booster.OnPowerChange := Self.OnPowerChange;
      Booster.OnOverloadChange := Self.OnOverloadChange;
      Booster.OnDCCChange := Self.OnDCCChange;

      Self.db.AddOrSetValue(id, Booster);

      Self.sortedKeys.Add(Booster);
    except
      on E: Exception do
      begin
        if (Assigned(Booster)) then
          Booster.Free();
        AppEvents.LogException(E, 'Chyba při zeilovače ' + id);
        continue;
      end;
    end;
  end; // for i

  Self.sortedKeys.Sort();

  ini.Free();
  sections.Free();

  ZesTableData.LoadToTable();

  Log('Načteno ' + IntToStr(Self.Count) + ' zesilovačů', TLogLevel.llInfo, lsData);
end;

procedure TBoosterDb.SaveToFile(inifilename: string);
var ini: TMemIniFile;
begin
  Log('Ukládám zesilovače...', TLogLevel.llInfo, lsData);

  try
    DeleteFile(PChar(inifilename));
    ini := TMemIniFile.Create(inifilename, TEncoding.UTF8);
  except
    on E: Exception do
    begin
      AppEvents.LogException(E, 'Ukladam zesilovace: nelze otevrit vystupni soubor');
      Exit();
    end;
  end;

  for var booster: TBooster in Self.sortedKeys do
    Booster.SaveDataToFile(ini, Booster.id);

  ini.UpdateFile();
  ini.Free();

  Log('Uloženo zesilovačů: ' + IntToStr(Self.Count), TLogLevel.llInfo, lsData);
end;

/// /////////////////////////////////////////////////////////////////////////////
// db operations

procedure TBoosterDb.Add(new: TBooster);
begin
  if (Self.db.ContainsKey(new.id)) then
    raise BoosterExistsException.Create('Zesilovač s ID ' + new.id + ' již existuje');

  Self.db.Add(new.id, new);

  new.OnPowerChange := Self.OnPowerChange;
  new.OnOverloadChange := Self.OnOverloadChange;
  new.OnDCCChange := Self.OnDCCChange;

  Self.sortedKeys.Add(new);
  Self.sortedKeys.Sort();
end;

procedure TBoosterDb.Remove(id: string);
begin
  if (Self.db.ContainsKey(id)) then
  begin
    Self.sortedKeys.Remove(Self.db[id]);
    Self.db.Remove(id);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.Update();
begin
  for var booster: TBooster in Self.db.Values do
    Booster.Update();

  Self.ControlBeep();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.OnOverloadChange(Sender: TObject; state: TBoosterSignal);
begin
  Blocks.OnBoosterChange(TBooster(Sender).id);
  ZesTableData.ZesChange();
end;

procedure TBoosterDb.OnPowerChange(Sender: TObject; state: TBoosterSignal);
begin
  Blocks.OnBoosterChange(TBooster(Sender).id);
  ZesTableData.ZesChange();
end;

procedure TBoosterDb.OnDCCChange(Sender: TObject; state: TBoosterSignal);
begin
  Blocks.OnBoosterChange(TBooster(Sender).id);
  ZesTableData.ZesChange();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.ControlBeep();
var short: Boolean;
begin
  if (trakce.TrackStatusSafe() <> TTrkStatus.tsOn) then
    Exit();

  short := false;
  for var booster: TBooster in Self.db.Values do
  begin
    if (Booster.overload = TBoosterSignal.error) then
    begin
      short := true;
      Break;
    end;
  end;

  if (not short) then
    Exit();

  if (Self.Beep.NextBeep < Now) then
    Self.Beep.NextBeep := Now + EncodeTime(0, 0, Self._BEEP_INTERVAL, 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBoosterDb.GetCount(): Integer;
begin
  Result := Self.db.Count;
end;

function TBoosterDb.GetItem(key: string): TBooster;
begin
  if (Self.db.ContainsKey(key)) then
    Result := Self.db.Items[key]
  else
    Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.SyncStructures();
begin
  for var id: string in Self.db.Keys do
  begin
    if (Self.db[id].id <> id) then
    begin
      Self.db.AddOrSetValue(Self.db[id].id, Self.db[id]);
      Self.db.Remove(id);
    end;
  end;

  Self.sortedKeys.Sort();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBoosterDb.ContainsKey(key: string; ignore: TBooster = nil): Boolean;
begin
  if (Self.db.ContainsKey(key)) then
    Result := (ignore <> Self[key])
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.Clear();
begin
  for var booster: TBooster in Self.db.Values do
    Booster.Free();
  Self.db.Clear();
  Self.sortedKeys.Clear();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBoosterDb.GetEnumerator(): TDictionary<string, TBooster>.TValueEnumerator;
begin
  Result := Self.db.Values.GetEnumerator();
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

Boosters := TBoosterDb.Create();

finalization

FreeAndNil(Boosters);

end.// unit
