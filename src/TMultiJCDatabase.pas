unit TMultiJCDatabase;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////

interface

uses TechnologieMultiJC, TBlok, IniFiles, RPConst, SysUtils, Windows, IdContext,
      Generics.Collections, Classes;

type
  TMultiJCDb = class
   private const
    _MAX_JC = 255;

   private
    JCs:TList<TMultiJC>;

    ffilename:string;

     procedure Clear();
     function GetJCCnt():Word;

   public

     constructor Create();
     destructor Destroy(); override;

     function LoadData(const filename:string):Byte;
     function SaveData(const filename:string):Byte;

     procedure Update();
     function GetJCByIndex(index:Integer):TMultiJC;

     function StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject):boolean;   // vraci true, pokud nasel prislusnou cestu

     function Add(data:TMultiJCProp):TMultiJC;
     procedure Remove(index:Integer);

     property Count:Word read GetJCCnt;
     property filename:string read ffilename;

  end;

var
  MultiJCDb:TMultiJcDb;


implementation

uses Logging, GetSystems, TBloky, TBlokSCom, TBlokUsek, TOblRizeni, TCPServerOR,
      DataMultiJC, Zasobnik, TOblsRizeni, TechnologieJC, TJCDatabase, appEv;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TMultiJCDb
//  databaze slozenych jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TMultiJCDb.Create();
begin
 inherited Create();
 Self.JCs := TList<TMultiJC>.Create();
end;//ctor

destructor TMultiJCDb.Destroy();
begin
 Self.Clear();
 Self.JCs.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// load data from ini file
function TMultiJCDb.LoadData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
    mJC:TMultiJC;
    sections:TStrings;
begin
 writelog('Naèítám složené JC - '+filename, WR_DATA);

 Self.ffilename := filename;

 try
   ini := TMemIniFile.Create(filename);
 except
   Exit(1);
 end;

 Self.Clear();

 sections := TStringList.Create();
 ini.ReadSections(sections);
 for i := 0 to sections.Count-1 do
  begin
   mJC := TMultiJC.Create();
   try
     mJC.LoadData(ini, sections[i]);
   except
     on e:Exception do
      begin
       mJC.Free();
       AppEvents.LogException(E, 'Chyba pøi naèítání složené JC '+mJC.Nazev);
       continue;
      end;
   end;
   Self.JCs.Add(mJC);
  end;//for i

 ini.Free;
 sections.Free();
 Result := 0;
 writelog('Naèteno '+IntToStr(Self.JCs.Count)+' složených JC', WR_DATA);

 MultiJCTableData.LoadToTable();
end;//function

// save data to ini file:
function TMultiJCDb.SaveData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukládám složené JC - '+filename, WR_DATA);

 try
   DeleteFile(PChar(filename));
   ini := TMemIniFile.Create(filename);
 except
   Exit(1);
 end;

 for i := 0 to Self.JCs.Count-1 do
   Self.JCs[i].SaveData(ini, 'JC'+IntToStr(i));

 ini.UpdateFile();
 ini.Free();
 Result := 0;

 writelog('Složené JC uloženy', WR_DATA);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetJCByIndex(index:Integer):TMultiJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
  begin
   Result := nil;
   Exit;
  end;

 Result := Self.JCs[index];
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.Update();
var i:Integer;
begin
 if (not GetFunctions.GetSystemStart) then Exit;

 for i := 0 to Self.JCs.Count-1 do
  begin
   try
     if (Self.JCs[i].stav.JCIndex > -1) then
       Self.JCs[i].UpdateStaveni();
   except
    on E:Exception do
     begin
      if (not log_err_flag) then
        AppEvents.LogException(E, 'JC '+Self.JCs[i].nazev + ' update error, rusim staveni');
      Self.JCs[i].RusStaveni();
     end;
   end;//except
  end;//for i }
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJCDb.Clear();
var i:Integer;
begin
 for i := Self.JCs.Count-1 downto 0 do
   Self.JCs[i].Free();
 Self.JCs.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.GetJCCnt():Word;
begin
 Result := Self.JCs.Count;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject):boolean;
var i, j:Integer;
    Blk:TBlk;
    JC:TJC;
    flag:boolean;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   if (Self.JCs[i].data.JCs.Count < 2) then continue;

   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[0]);
   if (JC = nil) then continue;

   Blky.GetBlkByID(JC.data.NavestidloBlok, Blk);
   if (Blk <> StartBlk) then continue;

   // posledni blok musi byt poseldni blok posledni jizdni cesty
   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[Self.JCs[i].data.JCs.Count-1]);
   Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Blk);
   if (Blk <> EndBlk) then continue;

   // kontrola variantnich bodu
   flag := true;
   if (Self.JCs[i].data.vb.Count <> (SenderOR as TOR).vb.Count) then continue;
   for j := 0 to Self.JCs[i].data.vb.Count-1 do
     if (Self.JCs[i].data.vb[j] <> ((SenderOR as TOR).vb[j] as TBlk).GetGlobalSettings().id) then flag := false;
   if (not flag) then continue;

   JC := JCDb.GetJCByID(Self.JCs[i].data.JCs[0]);
   if ((Integer((StartBlk as TBlkSCom).ZacatekVolba) = Integer(JC.data.TypCesty)) or
      (((StartBlk as TBlkSCom).ZacatekVolba = TBLkSComVolba.NC) and (JC.data.TypCesty = TJCType.vlak))) then
    begin
     // nasli jsme jizdni cestu, kterou hledame

     // kontrola existence vsech jizdnich cest v dane slozene jizdni ceste
     for j := 0 to Self.JCs[i].data.JCs.Count-1 do
      if (JCDb.GetJCByID(Self.JCs[i].data.JCs[j]) = nil) then Exit(false);

     if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) then
      begin
       // VZ -> pridame do zasobniku postupne vsechny jizdni cesty
       for j := 0 to Self.JCs[i].data.JCs.Count-1 do
         (SenderOR as TOR).stack.AddJC(JCDb.GetJCByID(Self.JCs[i].data.JCs[j]), SenderPnl, false);

       (StartBlk as TBlkSCom).ZacatekVolba := TBlkSComVOlba.none;
       (EndBlk as TBlkUsek).KonecJC        := TJCType.no;
       (SenderOR as TOR).ClearVb();
      end else begin
       Self.JCs[i].StavJC(SenderPnl, SenderOR);
      end;

     Exit(true);
    end;
  end;//for i

 Result := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TMultiJCDb.Add(data:TMultiJCProp):TMultiJC;
var mJC:TMultiJC;
begin
 mJC := TMultiJC.Create(data);
 Self.JCs.Add(mJC);
 Result := mJC;
 MultiJCTableData.AddJC;
end;//procedure

procedure TMultiJCDb.Remove(index:Integer);
begin
 if ((index < Self.JCs.Count) and (not Self.JCs[index].staveni)) then
  begin
   Self.JCs.Delete(index);
   MultiJCTableData.RemoveJC(index);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  MultiJCDb := TMultiJCDb.Create();

finalization
  FreeAndNil(MultiJCDb);

end.//unit
