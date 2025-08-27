unit TrainDb;

{ Trains database. }

interface

uses SysUtils, Train, IniFiles, Classes, Windows, Forms, TrakceIFace, JsonDataObjects;

const
  _MAX_TRAIN = 128;

type

  TTrainDb = class
  private
    ffilename: string;

    procedure FreeTrains();

    function GetCount(): Integer;
    function GetItem(index: Integer): TTrain;
    function GetEmptySpaceForTrain(): Integer;

  public
    trains: array [0 .. _MAX_TRAIN] of TTrain;

    constructor Create();
    destructor Destroy(); override;

    procedure LoadData(const filename: string);
    procedure SaveData(const filename: string);

    function Add(Train: TStrings; usek: TObject; area: TObject;
      sprUsekIndex: Integer; ok: TCb; err: TCb): TTrain; overload;
    function Add(Train: TJsonObject; ok: TCb; err: TCb): TTrain; overload;
    procedure Remove(index: Integer);
    function Exists(i: Cardinal): Boolean;

    function GetTrainNameByIndex(index: Integer): string;
    function GetTrainIndexByName(name: string): Integer;

    procedure UpdateFront();
    procedure StopAllTrains();
    procedure ClearPOdj();
    procedure UpdateTraveled(msSinceLastUpdate: Cardinal);

    procedure GetPtData(json: TJsonObject);

    property filename: string read ffilename;

    property Items[index: Integer]: TTrain read GetItem; default;
    property count: Integer read GetCount;

  end; // TTrainDb

var
  trains: TTrainDb;

implementation

uses Logging, DataTrains, BlockDb, BlockTrack, DataHV, appEv, Block,
  TCPServerPanel, PTUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTrainDb.Create();
begin
  inherited;

  for var i := 0 to _MAX_TRAIN - 1 do
    Self.trains[i] := nil;
end;

destructor TTrainDb.Destroy();
begin
  Self.FreeTrains();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.FreeTrains();
begin
  for var i := 0 to _MAX_TRAIN - 1 do
    if (Assigned(Self.trains[i])) then
      FreeAndNil(Self.trains[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.LoadData(const filename: string);
var
  ini: TMemIniFile;
  sections: TStrings;
begin
  Log('Načítám soupravy: ' + filename, llInfo, lsData);
  Self.ffilename := filename;

  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  sections := TStringList.Create();

  try
    ini.ReadSections(sections);

    Self.FreeTrains();

    for var i := 0 to sections.count - 1 do
      Self.trains[i] := TTrain.Create(ini, sections[i], i);

    Log('Načteno ' + IntToStr(sections.count) + ' souprav', llInfo, lsData);
  finally
    FreeAndNil(ini);
    FreeAndNil(sections);
  end;

  TrainTableData.LoadToTable();
  HVTableData.LoadToTable();
end;

procedure TTrainDb.SaveData(const filename: string);
var
  ini: TMemIniFile;
begin
  Log('Ukládám soupravy: ' + filename, llInfo, lsData);

  if (FileExists(filename)) then
    DeleteFile(PChar(filename));
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    for var i := 0 to _MAX_TRAIN - 1 do
      if (Assigned(Self.trains[i])) then
        Self.trains[i].SaveToFile(ini, IntToStr(i));
    ini.UpdateFile();
  finally
    FreeAndNil(ini);
  end;

  Log('Uloženo ' + IntToStr(Self.count) + ' souprav', llInfo, lsData);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetCount(): Integer;
begin
  Result := 0;
  for var i := 0 to _MAX_TRAIN do
    if (Assigned(Self.trains[i])) then
      Inc(Result);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainNameByIndex(index: Integer): string;
begin
  if (index < 0) or (index >= _MAX_TRAIN) then
    Exit('-');

  if (Assigned(Self.trains[index])) then
    Result := Self.trains[index].name
  else
    Result := '-';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainIndexByName(name: string): Integer;
begin
  for var i := 0 to _MAX_TRAIN - 1 do
    if ((Assigned(Self.trains[i])) and (Self.trains[i].name = name)) then
      Exit(i);
  Exit(-1);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.Add(Train: TStrings; usek: TObject; area: TObject;
  sprUsekIndex: Integer; ok: TCb; err: TCb): TTrain;
var
  i: Integer;
begin
  i := Self.GetEmptySpaceForTrain();

  try
    Self.trains[i] := TTrain.Create(Train, i, usek, area, ok, err);
    if (Assigned(usek)) then // toto musi byt tady, nikoliv v konstruktoru
    begin
      (usek as TBlkTrack).AddTrain(sprUsekIndex, i);
      (usek as TBlkTrack).Change(); // volano kvuli aktualizaci dat
    end;

    Self.trains[i].OnPredictedSignalChange();
    Self.trains[i].OnExpectedSpeedChange();
    Result := Self.trains[i];
  except
    on E: Exception do
    begin
      if (Assigned(Self.trains[i])) then
        FreeAndNil(Self.trains[i]);
      TrainTableData.reload := true;
      raise;
    end;
  end;
end;

function TTrainDb.Add(Train: TJsonObject; ok: TCb; err: TCb): TTrain;
var
  i: Integer;
begin
  i := Self.GetEmptySpaceForTrain();
  var
  track := Blocks.GetBlkTrackOrRTByID(Train['front']);
  if (Train.Contains('createPos')) then
    Train.i['createPos'] := 0;

  try
    Self.trains[i] := TTrain.Create(Train, i, ok, err);
    if (Assigned(track)) then // toto musi byt tady, nikoliv v konstruktoru
    begin
      track.AddTrain(Train.i['createPos'], i);
      track.Change(); // volano kvuli aktualizaci dat
    end;

    Self.trains[i].OnPredictedSignalChange();
    Self.trains[i].OnExpectedSpeedChange();
    Result := Self.trains[i];
  except
    on E: Exception do
    begin
      if (Assigned(Self.trains[i])) then
        FreeAndNil(Self.trains[i]);
      TrainTableData.reload := true;
      raise;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.Remove(index: Integer);
begin
  if (not Assigned(Self.trains[index])) then
    Exit();

  Blocks.RemoveTrain(Self.trains[index]);
  PanelServer.OnRemoveTrain(Self.trains[index]);
  FreeAndNil(Self.trains[index]);
  TrainTableData.reload := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.Exists(i: Cardinal): Boolean;
begin
  Result := (i < _MAX_TRAIN) and (Self.trains[i] <> nil);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.UpdateFront();
begin
  for var i := 0 to _MAX_TRAIN - 1 do
    if (Self.trains[i] <> nil) then
      Self.trains[i].UpdateFront();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.StopAllTrains();
begin
  for var i := 0 to _MAX_TRAIN - 1 do
    if ((Self.trains[i] <> nil) and (Self.trains[i].wantedSpeed <> 0)) then
      Self.trains[i].speed := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetItem(index: Integer): TTrain;
begin
  Result := Self.trains[index];
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.ClearPOdj();
begin
  for var i := 0 to _MAX_TRAIN - 1 do
    if (Self.trains[i] <> nil) then
      Self.trains[i].ClearPOdj();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetEmptySpaceForTrain(): Integer;
begin
  for var i := 0 to _MAX_TRAIN do
    if (Self.trains[i] = nil) then
      Exit(i);
  raise Exception.Create('Založen maximální počet souprav!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.GetPtData(json: TJsonObject);
begin
  for var train in Self.trains do
  begin
    try
      if (train <> nil) then
        train.GetPtData(json.A['trains'].AddObject);
    except
      on E: Exception do
        PTUtils.PtErrorToJson(json.A['errors'].AddObject, '500',
          'Chyba pri nacitani soupravy ' + Train.name, E.Message);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.UpdateTraveled(msSinceLastUpdate: Cardinal);
begin
  for var train in Self.trains do
  begin
    if (train = nil) then
      continue;

    try
      train.UpdateTraveled(msSinceLastUpdate);
    except
      on E: Exception do
        AppEvents.LogException(E, 'TTrainDb.OnTTraveledUpdate');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

trains := TTrainDb.Create();

finalization

FreeAndNil(trains);

end.
