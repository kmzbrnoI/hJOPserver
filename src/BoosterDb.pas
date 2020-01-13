unit BoosterDb;

//this unit defines booster database as a class

interface

uses Booster, IniFiles, Classes, SysUtils, Windows, Generics.Collections,
      Generics.Defaults;

type
  BoosterExistsException = class(Exception);

  TBoosterDb = class
   private const
    _BEEP_INTERVAL = 1;                                //in seconds

   private
     db:TDictionary<string, TBooster>;
     sortedKeys:TList<TBooster>;

     Beep:record                                        //beep on overload
       NextBeep:TDateTime;                                //time of next beep
     end;

      //events from TBooster; those events direct call blk methods
      procedure OnZkratChange(Sender:TObject; state:TBoosterSignal);
      procedure OnNapajeniChange(Sender:TObject; state:TBoosterSignal);
      procedure OnDCCChange(Sender:TObject; state:TBoosterSignal);

      procedure ControlBeep();

      procedure Clear();
      function GetCount():Integer;
      function GetItem(key:string):TBooster;

   public

      constructor Create(inifilename:string = '');
      destructor Destroy(); override;

      procedure Add(new:TBooster);
      procedure Remove(id:string);

      procedure LoadFromFile(inifilename:string);
      procedure SaveToFile(inifilename:string);

      procedure Update();
      procedure SyncStructures();

      function ContainsKey(key:string; ignore:TBooster = nil):boolean;

      property Items[index : string] : TBooster read GetItem; default;
      property Count : integer read GetCount;
      property sorted : TList<TBooster> read sortedKeys;

  end;//TBoosterDb

var Boosters:TBoosterDb;

implementation

uses TBloky, fMain, TechnologieTrakce, appEv, logging, DataZesilovac, Trakce;

////////////////////////////////////////////////////////////////////////////////

//booster ini file format:
//  ini file
//  [id1] ... [id2] ... [id3] ...

constructor TBoosterDb.Create(inifilename:string = '');
begin
 inherited Create();
 Self.db := TDictionary<string, TBooster>.Create();
 Self.sortedKeys := TList<TBooster>.Create(TBooster.IdComparer());
 if (inifilename <> '') then Self.LoadFromFile(inifilename);
end;

destructor TBoosterDb.Destroy();
begin
 Self.Clear();
 Self.db.Free();
 Self.sortedKeys.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//files

//reads all sections
procedure TBoosterDb.LoadFromFile(inifilename:string);
var ini:TMemIniFile;
    sections:TStrings;
    id:string;
    booster:TBooster;
begin
 writelog('Načítám zesilovače: '+inifilename, WR_DATA);

 Self.Clear();

 try
   ini := TMemIniFile.Create(inifilename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Načítám zesilovače: nelze otevrit soubor bloku');
     Exit();
    end;
 end;
 sections := TStringList.Create();

 ini.ReadSections(sections);

 for id in Sections do
  begin
   if (id = '') then
    begin
     writelog('WARNING: prázdný primární klíč zesilovače - přeskakuji', WR_ERROR);
     continue;
    end;
   if (Self.db.ContainsKey(id)) then
    begin
     writelog('WARNING: duplicita primárního klíče zesilovače ('+id+') - přeskakuji', WR_ERROR);
     continue;
    end;

   booster := nil;

   try
     booster := TBooster.Create(ini, id);

     booster.OnNapajeniChange := Self.OnNapajeniChange;
     booster.OnZkratChange    := Self.OnZkratChange;
     booster.OnDCCChange      := Self.OnDCCChange;

     Self.db.AddOrSetValue(id, booster);

     Self.sortedKeys.Add(booster);
   except
     on e:Exception do
      begin
       if (Assigned(booster)) then booster.Free();
       AppEvents.LogException(E, 'Chyba při zeilovače '+id);
       continue;
      end;
   end;
  end;//for i

 Self.sortedKeys.Sort();

 ini.Free();
 sections.Free();

 ZesTableData.LoadToTable();

 writelog('Načteno '+IntToStr(Self.Count)+' zesilovačů', WR_DATA);
end;

procedure TBoosterDb.SaveToFile(inifilename:string);
var ini:TMemIniFile;
    booster:TBooster;
begin
 writelog('Ukládám zesilovače...', WR_DATA);

 try
   DeleteFile(PChar(inifilename));
   ini := TMemIniFile.Create(inifilename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Ukladam zesilovace: nelze otevrit vystupni soubor');
     Exit();
    end;
 end;

 for booster in Self.db.Values do
   booster.SaveDataToFile(ini, booster.id);

 ini.UpdateFile();
 ini.Free();

 writelog('Uloženo zesilovačů: '+IntToStr(Self.Count), WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////
//db operations

procedure TBoosterDb.Add(new:TBooster);
begin
 if (Self.db.ContainsKey(new.id)) then
   raise BoosterExistsException.Create('Zesilovač s ID '+new.id+' již existuje');

 Self.db.Add(new.id, new);

 new.OnNapajeniChange := Self.OnNapajeniChange;
 new.OnZkratChange    := Self.OnZkratChange;
 new.OnDCCChange      := Self.OnDCCChange;

 Self.sortedKeys.Add(new);
 Self.sortedKeys.Sort();
end;

procedure TBoosterDb.Remove(id:string);
begin
 if (Self.db.ContainsKey(id)) then
  begin
   Self.sortedKeys.Remove(Self.db[id]);
   Self.db.Remove(id);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.Update();
var booster:TBooster;
begin
 for booster in Self.db.Values do booster.Update();

 Self.ControlBeep();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.OnZkratChange(Sender:TObject; state:TBoosterSignal);
begin
 Blky.OnBoosterChange(TBooster(Sender).id);
 ZesTableData.ZesChange();
end;

procedure TBoosterDb.OnNapajeniChange(Sender:TObject; state:TBoosterSignal);
begin
 Blky.OnBoosterChange(TBooster(Sender).id);
 ZesTableData.ZesChange();
end;

procedure TBoosterDb.OnDCCChange(Sender:TObject; state:TBoosterSignal);
begin
 Blky.OnBoosterChange(TBooster(Sender).id);
 ZesTableData.ZesChange();
end;

////////////////////////////////////////////////////////////////////////////////

//controls beeping
procedure TBoosterDb.ControlBeep();
var zkrat:boolean;
    booster:TBooster;
begin
 if (TrakceI.TrackStatusSafe() <> TTrkStatus.tsOn) then Exit;

 zkrat := false;
 for booster in Self.db.Values do
  begin
   if (booster.zkrat = TBoosterSignal.error) then
    begin
     zkrat := true;
     Break;
    end;//if
  end;//for i

 if (not zkrat) then Exit;

 if (Self.Beep.NextBeep < Now) then
   Self.Beep.NextBeep := Now+EncodeTime(0,0,Self._BEEP_INTERVAL,0);
end;

////////////////////////////////////////////////////////////////////////////////

function TBoosterDb.GetCount():Integer;
begin
 Result := Self.db.Count;
end;

function TBoosterDb.GetItem(key:string):TBooster;
begin
 if (Self.db.ContainsKey(key)) then
   Result := Self.db.Items[key]
 else
   Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.SyncStructures();
var id:string;
begin
 for id in Self.db.Keys do
  begin
   if (Self.db[id].id <> id) then
    begin
     Self.db.AddOrSetValue(Self.db[id].id, Self.db[id]);
     Self.db.Remove(id);
    end;
  end;

 Self.sortedKeys.Sort();
end;

////////////////////////////////////////////////////////////////////////////////

function TBoosterDb.ContainsKey(key:string; ignore:TBooster = nil):boolean;
begin
 if (Self.db.ContainsKey(key)) then
   Result := (ignore <> Self[key])
  else
   Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBoosterDb.Clear();
var booster:TBooster;
begin
 for booster in Self.db.Values do booster.Free();
 Self.db.Clear();
 Self.sortedKeys.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Boosters := TBoosterDb.Create();
finalization
  FreeAndNil(Boosters);

end.//unit
