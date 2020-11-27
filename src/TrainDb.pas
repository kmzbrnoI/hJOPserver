﻿unit TrainDb;

{ Trains database. }

interface

uses SysUtils, Train, IniFiles, Classes, Windows, Forms, Trakce,
     JsonDataObjects;

const
  _MAX_TRAIN = 128;

type

  TTrainDb = class
   private
      ffilename:string;

      procedure FreeTrains();

      function GetCount():Integer;
      function GetItem(index:Integer): TTrain;
      function GetEmptySpaceForTrain(): Integer;

   public
      trains:array [0.._MAX_TRAIN] of TTrain;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadData(const filename:string);
      procedure SaveData(const filename:string);

      procedure AddTrainFromPanel(spr:TStrings; usek:TObject; OblR:TObject; sprUsekIndex:Integer; ok: TCb; err: TCb);
      procedure RemoveTrain(index:Integer);

      function GetTrainNameByIndex(index:Integer):string;
      function GetTrainIndexByName(name:string):Integer;

      procedure UpdateFront();
      procedure StopAllTrains();
      procedure ClearPOdj();

      procedure GetPtData(json:TJsonObject);

      property filename:string read ffilename;

      property Items[index : integer] : TTrain read GetItem; default;
      property count:Integer read GetCount;

  end;//TTrainDb

var
  Trains: TTrainDb;

implementation

uses Logging, DataSpr, TBloky, TBlokUsek, DataHV, appEv, TBlok,
     TCPServerOR, PTUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TTrainDb.Create();
var i:Integer;
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
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if (Assigned(Self.trains[i])) then
   FreeAndNil(Self.trains[i]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.LoadData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
begin
 writelog('Načítám soupravy: '+filename, WR_DATA);
 Self.ffilename := filename;

 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 sections := TStringList.Create();

 try
   ini.ReadSections(sections);

   Self.FreeTrains();

   for i := 0 to sections.Count-1 do
     Self.trains[i] := TTrain.Create(ini, sections[i], i);

   writelog('Načteno '+IntToStr(sections.Count)+' souprav', WR_DATA);
 finally
   FreeAndNil(ini);
   FreeAndNil(sections);
 end;

 TrainTableData.LoadToTable();
 HVTableData.LoadToTable();
end;

procedure TTrainDb.SaveData(const filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukládám soupravy: '+filename, WR_DATA);

 if (FileExists(filename)) then
   DeleteFile(PChar(filename));
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   for i := 0 to _MAX_TRAIN-1 do
     if (Assigned(Self.trains[i])) then
       Self.trains[i].SaveToFile(ini, IntToStr(i));
   ini.UpdateFile();
 finally
   FreeAndNil(ini);
 end;

 writelog('Uloženo '+IntToStr(Self.Count)+' souprav', WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetCount():Integer;
var i:Integer;
begin
 Result := 0;
 for i := 0 to _MAX_TRAIN do
  if (Assigned(Self.trains[i])) then
   Inc(Result);
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainNameByIndex(index:Integer):string;
begin
 if (index < 0) or (index >= _MAX_TRAIN) then Exit('-');

 if (Assigned(Self.trains[index])) then
  Result := Self.trains[index].name
 else
  Result := '-';
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetTrainIndexByName(name:string):Integer;
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if ((Assigned(Self.trains[i])) and (Self.trains[i].name = name)) then
   Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.AddTrainFromPanel(spr:TStrings; Usek:TObject; OblR:TObject; sprUsekIndex:Integer; ok: TCb; err: TCb);
var i:Integer;
begin
 i := Self.GetEmptySpaceForTrain();

 try
  Self.trains[i] := TTrain.Create(spr, Usek, i, OblR, ok, err);
  if (Assigned(Usek)) then          // toto musi byt tady, nikoliv v konstruktoru
   begin
    (Usek as TBlkUsek).AddTrain(sprUsekIndex, i);
    (Usek as TBlkUsek).Change();    // volano kvuli aktualizaci dat
   end;

  Self.trains[i].OnPredictedSignalChange();
  Self.trains[i].OnExpectedSpeedChange();
 except
  on E: Exception do
   begin
    FreeAndNil(Self.trains[i]);
    TrainTableData.reload := true;
    raise Exception.Create(E.Message);
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.RemoveTrain(index:Integer);
begin
 if (not Assigned(Self.trains[index])) then Exit();

 Blky.RemoveTrain(Self.trains[index]);
 ORTCPServer.OnRemoveTrain(Self.trains[index]);
 FreeAndNil(Self.trains[index]);
 TrainTableData.reload := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.UpdateFront();
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if (Self.trains[i] <> nil) then
    Self.trains[i].UpdateFront();
end;

////////////////////////////////////////////////////////////////////////////////

// Tato funkce predpoklada vysokou zatez sbernice do centraly ->
// schvalne ceka.
procedure TTrainDb.StopAllTrains();
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if ((Self.trains[i] <> nil) and (Self.trains[i].wantedSpeed <> 0)) then
   begin
    Self.trains[i].speed := 0;
    Sleep(3);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetItem(index:Integer):TTrain;
begin
 Result := Self.trains[index];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.ClearPOdj();
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
  if (Self.trains[i] <> nil) then
    Self.trains[i].ClearPOdj();
end;

////////////////////////////////////////////////////////////////////////////////

function TTrainDb.GetEmptySpaceForTrain():Integer;
var i:Integer;
begin
 for i := 0 to _MAX_TRAIN do
   if (Self.trains[i] = nil) then
     Exit(i);
 raise Exception.Create('Založen maximální počet souprav!');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainDb.GetPtData(json:TJsonObject);
var spr: TTrain;
begin
 for spr in Self.trains do
  begin
   try
     if (spr <> nil) then
       spr.GetPtData(json.A['trains'].AddObject);
   except
     on E:Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani soupravy '+spr.name,
        E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Trains := TTrainDb.Create();
finalization
  FreeAndNil(Trains);

end.//unit