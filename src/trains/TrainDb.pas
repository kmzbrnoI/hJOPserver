unit TrainDb;

{ Trains database. }

interface

uses SysUtils, Train, IniFiles, Classes, Windows, Forms, Trakce,
     JsonDataObjects;

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
      trains: array [0.._MAX_TRAIN] of TTrain;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename: string);
      procedure SaveData(const filename: string);

      function Add(train: TStrings; usek: TObject; area: TObject; sprUsekIndex: Integer; ok: TCb; err: TCb): TTrain; overload;
      function Add(train: TJsonObject; ok: TCb; err: TCb): TTrain; overload;
      procedure Remove(index: Integer);

      function GetTrainNameByIndex(index: Integer): string;
      function GetTrainIndexByName(name: string): Integer;

      procedure UpdateFront();
      procedure StopAllTrains();
      procedure ClearPOdj();

      procedure GetPtData(json: TJsonObject);

      property filename: string read ffilename;

      property Items[index : integer] : TTrain read GetItem; default;
      property count: Integer read GetCount;

  end;//TTrainDb

var
  Trains: TTrainDb;

implementation

uses Logging, DataSpr, BlockDb, BlockTrack, DataHV, appEv, Block,
     TCPServerPanel, PTUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TTrainDb.Create();
var i: Integer;
begin
 inherited;

 for i := 0 to _MAX_TRAIN-1 do
   Self.trains[i] := nil;
end;//ctor

destructor TTrainDb.Destroy();
begin
 Self.FreeTrains();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.FreeTrains();
var i: Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if (Assigned(Self.trains[i])) then
   FreeAndNil(Self.trains[i]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.LoadData(const filename: string);
var ini: TMemIniFile;
    sections: TStrings;
begin
 Log('Načítám soupravy: '+filename, ltData);
 Self.ffilename := filename;

 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 sections := TStringList.Create();

 try
   ini.ReadSections(sections);

   Self.FreeTrains();

   for var i := 0 to sections.Count-1 do
     Self.trains[i] := TTrain.Create(ini, sections[i], i);

   Log('Načteno '+IntToStr(sections.Count)+' souprav', ltData);
 finally
   FreeAndNil(ini);
   FreeAndNil(sections);
 end;

 TrainTableData.LoadToTable();
 HVTableData.LoadToTable();
end;

procedure TTrainDb.SaveData(const filename: string);
var ini: TMemIniFile;
begin
 Log('Ukládám soupravy: '+filename, ltData);

 if (FileExists(filename)) then
   DeleteFile(PChar(filename));
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   for var i := 0 to _MAX_TRAIN-1 do
     if (Assigned(Self.trains[i])) then
       Self.trains[i].SaveToFile(ini, IntToStr(i));
   ini.UpdateFile();
 finally
   FreeAndNil(ini);
 end;

 Log('Uloženo '+IntToStr(Self.Count)+' souprav', ltData);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetCount(): Integer;
begin
 Result := 0;
 for var i := 0 to _MAX_TRAIN do
   if (Assigned(Self.trains[i])) then
     Inc(Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainNameByIndex(index: Integer): string;
begin
 if (index < 0) or (index >= _MAX_TRAIN) then Exit('-');

 if (Assigned(Self.trains[index])) then
  Result := Self.trains[index].name
 else
  Result := '-';
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainIndexByName(name: string): Integer;
begin
 for var i := 0 to _MAX_TRAIN-1 do
   if ((Assigned(Self.trains[i])) and (Self.trains[i].name = name)) then
     Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.Add(train: TStrings; usek: TObject; area: TObject; sprUsekIndex: Integer; ok: TCb; err: TCb): TTrain;
var i: Integer;
begin
 i := Self.GetEmptySpaceForTrain();

 try
  Self.trains[i] := TTrain.Create(train, i, usek, area, ok, err);
  if (Assigned(usek)) then          // toto musi byt tady, nikoliv v konstruktoru
   begin
    (Usek as TBlkTrack).AddTrain(sprUsekIndex, i);
    (Usek as TBlkTrack).Change();    // volano kvuli aktualizaci dat
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

function TTrainDb.Add(train: TJsonObject; ok: TCb; err: TCb): TTrain;
var i: Integer;
begin
 i := Self.GetEmptySpaceForTrain();
 var track := Blocks.GetBlkTrackOrRTByID(train['front']);
 if (train.Contains('createPos')) then
   train.I['createPos'] := 0;

 try
  Self.trains[i] := TTrain.Create(train, i, ok, err);
  if (Assigned(track)) then          // toto musi byt tady, nikoliv v konstruktoru
   begin
    track.AddTrain(train.I['createPos'], i);
    track.Change();    // volano kvuli aktualizaci dat
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

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.Remove(index: Integer);
begin
 if (not Assigned(Self.trains[index])) then Exit();

 Blocks.RemoveTrain(Self.trains[index]);
 PanelServer.OnRemoveTrain(Self.trains[index]);
 FreeAndNil(Self.trains[index]);
 TrainTableData.reload := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.UpdateFront();
begin
 for var i := 0 to _MAX_TRAIN-1 do
   if (Self.trains[i] <> nil) then
      Self.trains[i].UpdateFront();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.StopAllTrains();
begin
 for var i := 0 to _MAX_TRAIN-1 do
  if ((Self.trains[i] <> nil) and (Self.trains[i].wantedSpeed <> 0)) then
    Self.trains[i].speed := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetItem(index: Integer): TTrain;
begin
 Result := Self.trains[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.ClearPOdj();
begin
  for var i := 0 to _MAX_TRAIN-1 do
    if (Self.trains[i] <> nil) then
      Self.trains[i].ClearPOdj();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetEmptySpaceForTrain(): Integer;
begin
 for var i := 0 to _MAX_TRAIN do
   if (Self.trains[i] = nil) then
     Exit(i);
 raise Exception.Create('Založen maximální počet souprav!');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.GetPtData(json: TJsonObject);
begin
 for var train in Self.trains do
  begin
   try
     if (train <> nil) then
       train.GetPtData(json.A['trains'].AddObject);
   except
     on E: Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani soupravy '+train.name,
        E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Trains := TTrainDb.Create();
finalization
  FreeAndNil(Trains);

end.
