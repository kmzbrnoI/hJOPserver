unit BoosterDb;

//this unit defines booster database as a class

interface

uses Booster, IniFiles, Classes, SysUtils, Windows;

const
  _MAX_B = 32;

type
  TBDatabase = record
    data:array [0.._MAX_B-1] of TBooster;
    cnt:Integer;
  end;

  TBoosterDb = class
   private const
    _BEEP_INTERVAL = 1;                                //in seconds

   private
     data:TBDatabase;

     Beep:record                                        //beep on overload
       NextBeep:TDateTime;                                //time of next beep
     end;

      procedure ClearDb();

      //events from TBooster; those events direct call blk methods
      procedure OnZkratChange(Sender:TObject;state:boolean);
      procedure OnNapajeniChange(Sender:TObject;state:boolean);

      function FindInDb(booster:TBooster):Integer;          //returns index

      procedure ControlBeep();
   public
      constructor Create(inifilename:string = '');
      destructor Destroy(); override;

      //db operatons
      function AddBooster(data:TBooster):Byte;
      function RemoveBooster(index:Integer):Byte;
      function GetBooster(index:Integer):TBooster;

      //files
      procedure LoadFromFile(inifilename:string);
      procedure SaveToFile(inifilename:string);

      procedure Update();                                   //update all boosters

      property BoosterCnt:Integer read data.cnt;
  end;//TBoosterDb

var BoostersDb:TBoosterDb;

implementation

uses TBloky, Main, Trakce;

////////////////////////////////////////////////////////////////////////////////

//booster ini file format:
//  ini file
//  [B0, B1, ... Bn]

//ctor
constructor TBoosterDb.Create(inifilename:string = '');
begin
 inherited Create;

 Self.data.cnt := 0;

 if (inifilename <> '') then Self.LoadFromFile(inifilename);
end;//ctor

//dtor
destructor TBoosterDb.Destroy();
begin
 Self.ClearDb();

 inherited Destroy;
end;//dtor

////////////////////////////////////////////////////////////////////////////////
//files

//reads all sections
procedure TBoosterDb.LoadFromFile(inifilename:string);
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
begin
 Self.ClearDb();
 ini := TMemIniFile.Create(inifilename);
 sections := TStringList.Create();

 ini.ReadSections(sections);

 if (sections.Count > _MAX_B) then Exit;

 for i := 0 to sections.Count-1 do
  begin
   Self.data.data[i] := TBooster.Create(ini,sections[i]);

   Self.data.data[i].OnNapajeniChange := Self.OnNapajeniChange;
   Self.data.data[i].OnZkratChange    := Self.OnZkratChange;

   Self.data.cnt := Self.data.cnt + 1;
  end;//for i

 ini.Free();
 sections.Free();
end;//procedure

procedure TBoosterDb.SaveToFile(inifilename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 DeleteFile(PChar(inifilename));
 ini := TMemIniFile.Create(inifilename);

 for i := 0 to Self.data.cnt-1 do
  begin
   Self.data.data[i].SaveDataToFile(ini,'B'+IntToStr(i));
  end;//for i

 ini.UpdateFile();
 ini.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//clear db
procedure TBoosterDb.ClearDb();
var i:Integer;
begin
 for i := 0 to Self.data.cnt-1 do
   FreeAndNil(Self.data.data[i]);

 Self.data.cnt := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//db operations

function TBoosterDb.AddBooster(data:TBooster):Byte;
begin
 if (Self.data.cnt >= _MAX_B) then Exit(1);

 Self.data.data[Self.data.cnt] := data;

 Self.data.data[Self.data.cnt].OnNapajeniChange := Self.OnNapajeniChange;
 Self.data.data[Self.data.cnt].OnZkratChange    := Self.OnZkratChange;

 Self.data.cnt := Self.data.cnt + 1;

 Result := 0;
end;//function

function TBoosterDb.RemoveBooster(index:Integer):Byte;
var i:Integer;
begin
 if ((index < 0) or (index >= Self.data.cnt)) then Exit(1);

 FreeAndNil(Self.data.data[index]);
 for i := index to Self.data.cnt-2 do
   Self.data.data[i] := Self.data.data[i+1];

 Self.data.cnt := Self.data.cnt - 1;

 Result := 0;
end;//function

function TBoosterDb.GetBooster(index:Integer):TBooster;
begin
 if ((index < 0) or (index >= Self.data.cnt)) then
  begin
   Result := nil;
   Exit;
  end;

 Result := Self.data.data[index];
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.Update();
var i:Integer;
begin
 for i := 0 to Self.data.cnt-1 do
   Self.data.data[i].Update();

 Self.ControlBeep();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.OnZkratChange(Sender:TObject;state:boolean);
var index:Integer;
begin
 index := Self.FindInDb(Sender as TBooster);
 if (index < 0) then Exit;
 Blky.SetZesZkrat(index, state);
end;//procedure

procedure TBoosterDb.OnNapajeniChange(Sender:TObject;state:boolean);
var index:Integer;
begin
 index := Self.FindInDb(Sender as TBooster);
 if (index < 0) then Exit;
 Blky.SetZesNapajeni(index, state);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBoosterDb.FindInDb(booster:TBooster):Integer;
var i:Integer;
begin
 for i := 0 to Self.data.cnt-1 do
  begin
   if (Self.data.data[i] = booster) then
    begin
     Result := i;
     Exit;
    end;
  end;//for i

 Result := -1;
end;//function

////////////////////////////////////////////////////////////////////////////////

//controls beeping
procedure TBoosterDb.ControlBeep();
var i:Integer;
    zkrat:boolean;
begin
 if (TrkSystem.status <> TS_ON) then Exit;

 zkrat := false;
 for i := 0 to Self.data.cnt-1 do
  begin
   if (Self.data.data[i].zkrat) then
    begin
     zkrat := true;
     Break;
    end;//if
  end;//for i

 if (not zkrat) then Exit;

 if (Self.Beep.NextBeep < Now) then
  begin
   Self.Beep.NextBeep := Now+EncodeTime(0,0,Self._BEEP_INTERVAL,0);
//   SoundsPlay.PlayNowSound(10);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
