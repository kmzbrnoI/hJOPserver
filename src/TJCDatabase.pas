unit TJCDatabase;

interface

uses TechnologieJC, TBlok, IniFiles, RPConst, SysUtils, Windows, IdContext,
      Generics.Collections, Classes;

type
  TJCDb = class
   private
    JCs:TList<TJC>;

    ffilename:string;

     procedure Clear();
     function GetCount():Word;

   public

     constructor Create();
     destructor Destroy(); override;

     function LoadData(const filename:string):Byte;
     function SaveData(const filename:string):Byte;

     function GetJCByIndex(index:Integer):TJC;
     procedure Update();
     procedure StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject);
     function SwitchJC(index1:Word; index2:Word):Byte;

     function AddJC(JCdata:TJCprop):Integer;
     function RemoveJC(index:Integer):Integer;

     // najde JC, ktera je postavena, ci prave stavena !
     function FindJC(NavestidloBlokID:Integer;Staveni:Boolean = false):Integer;

     //pouzivano pri vypadku polohy vyhybky postavene jizdni cesty
     function FindPostavenaJCWithVyhybka(vyh_id:Integer):TArSmallI;
     function FindPostavenaJCWithUsek(usek_id:Integer):Integer;
     function FindPostavenaJCWithPrisl(blk_id:Integer):TArSmallI;
     function FindPostavenaJCWithPrj(blk_id:Integer):Integer;
     function FindPostavenaJCWithTrat(trat_id:Integer):Integer;
     function FindPostavenaJCWithZamek(zam_id:Integer):TArSmallI;

     // jakmile dojde k postaveni cesty fJC, muze dojit k ovlivneni nejakeho navestidla pred ni
     // tato fce zajisti, ze k ovlivneni dojde
     procedure CheckNNavaznost(fJC:TJC);

     procedure RusAllJC();
     procedure RusJC(Blk:TBlk);     // rusi cestu, ve ktere je zadany blok

     property Count:Word read GetCount;
     property filename:string read ffilename;

  end;

var
  JCDb:TJcDb;


implementation

uses Logging, GetSystems, TBloky, TBlokSCom, TBlokUsek, TOblRizeni, TCPServerOR,
      DataJC, Zasobnik, TOblsRizeni, TMultiJCDatabase;

////////////////////////////////////////////////////////////////////////////////
// TRIDA TJCDb
//  databaze jizdnich cest
////////////////////////////////////////////////////////////////////////////////


constructor TJCDb.Create();
begin
 inherited Create();
 Self.JCs := TList<TJC>.Create();
end;//ctor

destructor TJCDb.Destroy();
begin
 Self.Clear();
 Self.JCs.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// load data from ini file
function TJCDb.LoadData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
    JC:TJC;
begin
 writelog('Naèítám JC - '+filename, WR_DATA);

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
   JC := TJC.Create();
   JC.LoadData(ini, sections[i]);
   Self.JCs.Add(JC);
  end;//for i

 ini.Free;
 sections.Free();
 Result := 0;
 writelog('Naèteno '+IntToStr(Self.JCs.Count)+' JC', WR_DATA);
end;//function

// save data to ini file:
function TJCDb.SaveData(const filename:string):Byte;
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukládám JC - '+filename, WR_DATA);

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

 writelog('JC uloženy', WR_DATA);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.Update();
var i:Integer;
begin
 if (not GetFunctions.GetSystemStart) then Exit;

 for i := 0 to Self.JCs.Count-1 do
  begin
   try
     if (Self.JCs[i].stav.RozpadBlok > -5) then
       Self.JCs[i].UsekyRusJC();

     if (Self.JCs[i].Staveni) then
      begin
       Self.JCs[i].UpdateStaveni();
       Self.JCs[i].UpdateTimeOut();
      end;
   except
    on E:Exception do
     begin
      if (not log_err_flag) then
       writeLog('JC '+Self.JCs[i].nazev + ' update error : '+E.Message+' ; rusim staveni', WR_ERROR);
      if (Self.JCs[i].staveni) then
        Self.JCs[i].CancelStaveni('Vyjímka', true)
      else
        Self.JCs[i].RusJCWithoutBlk();
     end;
   end;//except
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.Clear();
var i:Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
   Self.JCs[i].Free();
 Self.JCs.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetJCByIndex(index:Integer):TJC;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
  begin
   Result := nil;
   Exit;
  end;

 Result := Self.JCs[index];
end;//function

////////////////////////////////////////////////////////////////////////////////

//toto se vola zvnejsi, kdyz chceme postavit jakoukoliv JC
procedure TJCDb.StavJC(StartBlk,EndBlk:TBlk; SenderPnl:TIdContext; SenderOR:TObject);
var i, j:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   Blky.GetBlkByID(Self.JCs[i].data.NavestidloBlok, Blk);
   if (Blk <> StartBlk) then continue;

   Blky.GetBlkByID(Self.JCs[i].data.Useky[Self.JCs[i].data.Useky.Count-1], Blk);
   if (Blk <> EndBlk) then continue;

   if ((Integer((StartBlk as TBlkSCom).ZacatekVolba) = Integer(Self.JCs[i].data.TypCesty)) or
      (((StartBlk as TBlkSCom).ZacatekVolba = TBLkSComVolba.NC) and (Self.JCs[i].data.TypCesty = TJCType.vlak))) then
    begin
     // nasli jsme jizdni cestu, kterou hledame

     // jeste kontrola variantnich bodu:
     if (Self.JCs[i].data.vb.Count <> (SenderOR as TOR).vb.Count) then continue;
     for j := 0 to Self.JCs[i].data.vb.Count-1 do
      if (Self.JCs[i].data.vb[j] <> ((SenderOR as TOR).vb[j] as TBlk).GetGlobalSettings.id) then continue;

     // v pripade nouzove cesty klik na DK opet prevest na klienta
     if ((StartBlk as TBlkSCom).ZacatekVolba = TBLkSComVolba.NC) then
       for j:= 0 to (StartBlk as TBlkSCom).OblsRizeni.Cnt-1 do
        (StartBlk as TBlkSCom).OblsRizeni.ORs[j].ORDKClickClient();


     if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) then
      begin
       (SenderOR as TOR).stack.AddJC(Self.JCs[i], SenderPnl, ((StartBlk as TBlkSCom).ZacatekVolba = TBLkSComVolba.NC));

       // zrusime zacatek, konec a variantni body
       (StartBlk as TBlkSCom).ZacatekVolba := TBlkSComVOlba.none;
       (EndBlk as TBlkUsek).KonecJC        := TJCType.no;
       (SenderOR as TOR).ClearVb();
      end else begin
       (SenderOR as TOR).vb.Clear();            // variantni body aktualne stavene JC jen smazeme z databaze (zrusime je na konci staveni JC)
       Self.JCs[i].StavJC(SenderPnl, SenderOR, nil, ((StartBlk as TBlkSCom).ZacatekVolba = TBLkSComVolba.NC));
      end;


     Exit;
    end;
  end;//for i

 // kontrola staveni slozene jizdni cesty
 if ((StartBlk as TBlkSCom).ZacatekVolba <> TBLkSComVolba.NC) then
   if (MultiJCDb.StavJC(StartBlk, EndBlk, SenderPnl, SenderOR)) then Exit();

 (EndBlk as TBlkUsek).KonecJC := TJCType.no;
 ORTCPServer.SendInfoMsg(SenderPnl, 'Cesta nenalezena v závìrové tabulce');
 writelog('Nelze postavit JC -  nenalezena v zaverove tabulce',WR_VC);
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJCDb.AddJC(JCdata:TJCprop):Integer;
var JC:TJC;
begin
 JC := TJC.Create(JCData);
 Self.JCs.Add(JC);
 JCTableData.AddJC();
 Result := 0;
end;//function

function TJCDb.RemoveJC(index:Integer):Integer;
var i:Integer;
    OblR:TOR;
begin
 if ((index < 0) or (index >= Self.JCs.Count)) then
   Exit(1);

 if (Self.JCs[index].postaveno) then
   Exit(2);

 for i := 0 to ORs.ORcnt-1 do
  begin
   ORs.GetORByIndex(i, OblR);
   if (OblR.stack.IsJCInStack(Self.JCs[index])) then
    Exit(3);
  end;

 Self.JCs[index].Free();
 Self.JCs.Delete(index);

 JCTableData.RemoveJC(index);
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TJCDb.SwitchJC(index1:Word; index2:Word):Byte;
var tmp:TJC;
begin
 if ((index1 >= Self.JCs.Count) or (index2 >= Self.JCs.Count)) then
   Exit(1);

 tmp := Self.JCs[index1];
 Self.JCs[index1] := Self.JCs[index2];
 Self.JCs[index2] := tmp;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TJCDb.RusAllJC();
var i:Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
   if (Self.JCs[i].postaveno) then
     Self.JCs[i].RusJC();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJCDb.FindJC(NavestidloBlokID:Integer; Staveni:Boolean = false):Integer;
var i:Integer;
 begin
  for i := 0 to Self.JCs.Count-1 do
    if (((Self.JCs[i].postaveno) or ((Staveni) and (Self.JCs[i].staveni))) and (Self.JCs[i].data.NavestidloBlok = NavestidloBlokID)) then
      Exit(i);
  Exit(-1);
 end;//function

////////////////////////////////////////////////////////////////////////////////

//vyuzivani pri vypadku polohy vyhybky ke zruseni jizdni cesty
// muze vracet vic jizdnich cest - jeden odvrat muze byt u vic aktualne postavenych JC
function TJCDB.FindPostavenaJCWithVyhybka(vyh_id:Integer):TArSmallI;
var i,j:Integer;
begin
  SetLength(Result, 0);

  for i := 0 to Self.JCs.Count-1 do
   begin
    if (not Self.JCs[i].postaveno) then continue;

    // prime vyhybky
    for j := 0 to Self.JCs[i].data.Vyhybky.Count-1 do
     begin
      if (Self.JCs[i].data.Vyhybky[j].Blok = vyh_id) then
       begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
       end;//if (JC[cyklus2].VyhybkyZaver[cyklus3].Blok = Bloky.Data[cyklus].ID)
     end;//for j

    // odvraty
    for j := 0 to Self.JCs[i].data.Odvraty.Count-1 do
     begin
      if (Self.JCs[i].data.Odvraty[j].Blok = vyh_id) then
       begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
       end;//if (JC[cyklus2].VyhybkyZaver[cyklus3].Blok = Bloky.Data[cyklus].ID)
     end;//for j

    // kontrolovane vyhybky
    for j := 0 to Self.JCs[i].data.podminky.vyhybky.Count-1 do
     begin
      if (Self.JCs[i].data.podminky.vyhybky[j].Blok = vyh_id) then
       begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
       end;//if (JC[cyklus2].VyhybkyZaver[cyklus3].Blok = Bloky.Data[cyklus].ID)
     end;//for j

   end;//for i
end;//function

//vyuzivani pri vypadku polohy vyhybky ke zruseni jizdni cesty
function TJCDB.FindPostavenaJCWithUsek(usek_id:Integer):Integer;
var i,j:Integer;
begin
  Result := -1;

  for i := 0 to Self.JCs.Count-1 do
   begin
    if (not Self.JCs[i].postaveno) then continue;

    for j := 0 to Self.JCs[i].data.Useky.Count-1 do
      if (Self.JCs[i].data.Useky[j] = usek_id) then
        Exit(i);
   end;//for i
end;//function

function TJCDB.FindPostavenaJCWithPrisl(blk_id:Integer):TArSmallI;
var i,j:Integer;
begin
  SetLength(Result, 0);

  for i := 0 to Self.JCs.Count-1 do
   begin
    if (not Self.JCs[i].postaveno) then continue;

    for j := 0 to Self.JCs[i].data.Prisl.Count-1 do
     begin
      if (Self.JCs[i].data.Prisl[j].Blok = blk_id) then
       begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
       end;//if (JC[cyklus2].VyhybkyZaver[cyklus3].Blok = Bloky.Data[cyklus].ID)
     end;//for j
   end;//for i
end;//function

function TJCDB.FindPostavenaJCWithTrat(trat_id:Integer):Integer;
var i:Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   if (not Self.JCs[i].postaveno) then continue;
   if (Self.JCs[i].data.Trat = trat_id) then Exit(i);
  end;//for i

 Exit(-1);
end;//function

function TJCDB.FindPostavenaJCWithPrj(blk_id:Integer):Integer;
var i,j:Integer;
begin
 for i := 0 to Self.JCs.Count-1 do
  begin
   if (not Self.JCs[i].postaveno) then continue;
   for j := 0 to Self.JCs[i].data.Prejezdy.Count-1 do
     if (Self.JCs[i].data.Prejezdy[j].Prejezd = blk_id) then Exit(i);
  end;//for i

 Exit(-1);
end;//procedure

function TJCDB.FindPostavenaJCWithZamek(zam_id:Integer):TArSmallI;
var i,j:Integer;
begin
  SetLength(Result, 0);

  for i := 0 to Self.JCs.Count-1 do
   begin
    if (not Self.JCs[i].postaveno) then continue;

    // prime vyhybky
    for j := 0 to Self.JCs[i].data.podminky.zamky.Count-1 do
     begin
      if (Self.JCs[i].data.podminky.zamky[j].Blok = zam_id) then
       begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := i;
       end;//if (JC[cyklus2].VyhybkyZaver[cyklus3].Blok = Bloky.Data[cyklus].ID)
     end;//for j
   end;//for i
end;//function

////////////////////////////////////////////////////////////////////////////////

//jakmile dojde k nastaveni navestidla na ceste JC, tady se zkontroluje, zda-li
//se nahodou nema nejake navestidlo pred cestou JC rozsvitit jinak
procedure TJCDb.CheckNNavaznost(fJC:TJC);
var i:Integer;
    nav:TBlk;
    my_navest:Integer;
begin
  if (fJC.data.TypCesty = TJCType.posun) then Exit;

  Blky.GetBlkByID(fJC.data.NavestidloBlok, nav);
  if (nav = nil) then Exit;
  my_navest := (nav as TBlkSCom).Navest;

  for i := 0 to Self.JCs.Count-1 do
   begin
    if ((Self.JCs[i].data.TypCesty = TJCType.posun) or (Self.JCs[i].data.DalsiNNavaznost <> fJC.data.NavestidloBlok)) then continue;

    Blky.GetBlkByID(Self.JCs[i].data.NavestidloBlok, nav);

    if ((nav as TBlkSCom).Navest = 0) then continue;    

    if (my_navest > 0) then
     begin
      if (Self.JCs[i].data.RychlostDalsiN = 4) then
       begin
        (nav as TBlkSCom).Navest := 4;              // 40 km/h a volno
       end else begin
        (nav as TBlkSCom).Navest := 1;              // volno
       end;//else ...Odbocka

     end else begin

      if (Self.JCs[i].data.RychlostNoDalsiN = 4) then
       begin
        (nav as TBlkSCom).Navest := 4;              // 40 km/h a volno
       end else begin
        (nav as TBlkSCom).Navest := 1;              // volno
       end;//else ...Odbocka

     end;
   end;//for i

end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tusi cestu, ve ktere je zadany blok
procedure TJCDb.RusJC(Blk:TBlk);
var tmpblk:TBlk;
    jcs:TArSmallI;
    i, j:Integer;
begin
 case (Blk.GetGlobalSettings().typ) of
  _BLK_VYH     : jcs := JCDb.FindPostavenaJCWithVyhybka(Blk.GetGlobalSettings().id);
  _BLK_PREJEZD : begin
    SetLength(jcs, 1);
    jcs[0] := JCDb.FindPostavenaJCWithPrj(Blk.GetGlobalSettings().id);
  end;
  _BLK_USEK, _BLK_TU: begin
    SetLength(jcs, 1);
    jcs[0] := JCDb.FindPostavenaJCWithUsek(Blk.GetGlobalSettings().id);
  end;

  _BLK_SCOM: begin
    SetLength(jcs, 1);
    jcs[0] := JCDb.FindJC(Blk.GetGlobalSettings().id);
  end;

  _BLK_TRAT: begin
    SetLength(jcs, 1);
    jcs[0] := JCDb.FindPostavenaJCWithTrat(Blk.GetGlobalSettings().id);
  end;

  _BLK_ZAMEK: jcs := JCDb.FindPostavenaJCWithZamek(Blk.GetGlobalSettings().id);
 end;//case


 if (Length(jcs) > 0) then
  begin
   for i := 0 to Length(jcs)-1 do
    begin
     if (jcs[i] < 0) then continue;   // toto tady musi byt, proto napr FindPostavenaJCWithTrat vraci -1, pokud trat nenajde

     Blky.GetBlkByID(JCDb.GetJCByIndex(jcs[i]).data.NavestidloBlok, tmpblk);
     if ((tmpblk as TBlkSCom).Navest = 0) then continue;

     JCDB.GetJCByIndex(jcs[i]).RusJCWithoutBlk();
     for j := 0 to (tmpBlk as TBlkScom).OblsRizeni.Cnt-1 do
       (tmpBlk as TBlkScom).OblsRizeni.ORs[j].BlkWriteError(Self, 'Chyba povolovaci navesti '+tmpblk.GetGlobalSettings().name, 'TECHNOLOGIE');
    end;//for i
  end;//if jcindex <> -1
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJCDb.GetCount():Word;
begin
 Result := Self.JCs.Count;
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
  JCDb := TJCDb.Create();

finalization
  FreeAndNil(JCDb);

end.//unit
