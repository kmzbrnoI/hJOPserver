unit TBloky;

//tato unita definuje tridu, ktera se stara o vsechny bloky
//tedy jsou v ni ulozeny  vsechny bloky

//zakladni principy:
//  - trida TBlky udrzuje databazi existujicich technologickych bloku
//  - tyto bloky jsou odvozeny ze spolecne abstraktni tridy TBlk
//  - tridu TBlky vytvari main
//  - event OnChange je odesilan do prislusnych oblasti rizeni a tam se zpracovava dal (odesila se jednotlivym panelum)

// Update ve verzi 4.3.7:
//  Bloky jsou serazeny podle ID a vyhledava se v nich binarne.

interface

uses IniFiles, TBlok, SysUtils, Windows, TOblsRizeni, TOblRizeni, StdCtrls,
      RPConst, Generics.Collections, Classes, IdContext, IBUtils;

type

 TBlksList = TList<TObject>;

 TBlky = class(TObject)

  private
   data:TList<TBlk>;

   ffstatus:string;
   ffile:string;
   fenabled:boolean;

    procedure DestroyBlocks();
    procedure BlkChange(Sender:TObject);

    function GetCount():Integer;

    function FindPlaceForNewBlk(id:Integer):Integer;
    procedure UpdateBlkIndexes();             // aktualizuje indexy vsech bloku, pouziva se pri nacitani dat

  public
    constructor Create();
    destructor Destroy(); override;

    //data loading/saving
    function LoadFromFile(const tech_filename,rel_filename,stat_filename:string):Integer;
    procedure SaveToFile(const tech_filename:string);
    procedure SaveStatToFile(const stat_filename:string);

    function Add(typ:Integer; glob:TBlkSettings):TBlk;
    procedure Delete(index:Integer);

    function GetBlkByIndex(index:integer;var Blk:TBlk):Integer;
    function SetBlk(index:integer;data:TBlk):Integer;

    //enable/disable all block (on screen)
    procedure Enable();
    procedure Disable();
    procedure Reset();

    //update all blokcs states (should be called in timer every x ms)
    procedure Update();

    function GetBlkIndex(id:Integer):Integer;
    function GetBlkByID(id:integer;var Blk:TBlk):Integer;
    function GetBlkID(index:Integer):Integer;
    function GetBlkName(id:Integer):string;
    function GetBlkIndexName(index:Integer):string;

    function GetBlkSComZacatekVolba(obl:string):TBlk;
    function GetBlkUsekVlakPresun(obl:string):TBlk;

    function GetSComPrivol(oblR:TOR):TBlksList;

    //ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
    procedure GetORBlk(OblRizeni_id:string; conn:TIdContext);

    //kontroluje, zda-li blok s timto ID uz nahadou existuje
    //pri hledani vynechava blok s indexem index
    //true = existuje, false = neexistuje
    function IsBlok(id:Integer; ignore_index:Integer = -1):boolean;

    procedure SetZesZkrat(zesil:Integer;state:boolean);       //pokud je na zesilovaci zmenen zkrat
    procedure SetZesNapajeni(zesil:Integer;state:boolean);    //pokud je na zesilovaci zmeneno napajeni
    procedure SetDCC(state:boolean);                          //vola se pri zmene stavu DCC (zapnuto X vypnuto)

    procedure NUZ(or_id:string; state:boolean = true);        //pokud true, aplikuji NUZ, pokud false, zrusim NUZ vsech bloku v OR

    procedure NactiBlokyDoObjektu(CB:TComboBox; Polozky:PTArSmallI; Vypust:PTArSmallI; OblRizeniID:TArStr; BlokTyp:Integer; BlokID:Integer = -1; BlokTyp2:Integer = -1);

    procedure RemoveSpr(spr:Integer);
    procedure SprPrediction(Nav:TBlk);

    function GetBlkWithSpr(spr:Integer):TBlksList;
    function GetVyhWithZamek(zamekID:integer):TBlksList;

    // zavola change na vsechny useky, ktere obsahuji zadanou soupravu
    // pouziva se napriklad pro oznameni ukradeni LOKO
    procedure ChangeUsekWithSpr(spr:Integer);

    // zavola Change vsech trati, ktere obsahuji danou soupravu
    // pouziva se pri zmene vlastnosti soupravy -> musi se aktualizovat seznam LOKO v trati
    procedure ChangeSprToTrat(spr:Integer);

    // volano pri zmene ID bloku na indexu \index
    // -> je potreba zmenit poradi bloku
    procedure BlkIDChanged(index:Integer);

    class function GetBlksList(first:TObject = nil; second:TObject = nil; third:TObject = nil):TBlksList;

    property Cnt:Integer read GetCount;
    property fstatus:string read ffstatus;
    property blky_file:string read ffile;
    property enabled:boolean read fenabled;
 end;//class TBlky

var
 Blky:TBlky;

implementation

uses TBlokVyhybka, TBlokUsek, TBlokIR, TBlokSCom, fMain, TBlokPrejezd,
      TBlokZamek, TJCDatabase, Logging, TBlokTrat, TBlokUvazka, TechnologieMTB,
      DataBloky, SprDb, TechnologieJC, Zasobnik, GetSystems, TBlokRozp,
      TBlokTratUsek;

////////////////////////////////////////////////////////////////////////////////

constructor TBlky.Create();
begin
 inherited Create();
 Self.Data     := TList<TBlk>.Create();
 Self.fenabled := false;
end;//ctor

destructor TBlky.Destroy();
begin
 Self.DestroyBlocks();
 Self.Data.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.DestroyBlocks();
var i:Integer;
begin
 for i := 0 to Self.data.Count-1 do
  if (Assigned(Self.Data[i])) then Self.data[i].Free;
 Self.data.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//pri zmene stavu jakehokoliv bloku je vyvolana tato metoda
//tady se resi veskere provazanosti bloku a odesilani eventu do oblasti rizeni
procedure TBlky.BlkChange(Sender:TObject);
var blkset:TBlkSettings;
    obl_rizeni:TORsRef;
    i:Integer;
begin
 if ((Sender as TBlk).GetGlobalSettings.typ = _BLK_USEK) then
  begin
   obl_rizeni := (Sender as TBlkUsek).OblsRizeni;

   // pri jakekoliv zmene useku dojde k Change() na vyhybce
   // navaznost: usek -> vyhybka
   blkset := (Sender as TBlk).GetGlobalSettings();
   for i := 0 to Self.Data.Count-1 do
     if (Self.Data[i].GetGlobalSettings().typ = _BLK_VYH) then
       if ((Self.Data[i] as TBlkVyhybka).UsekID = blkset.id) then
         Self.Data[i].Change();
  end;//_BLK_USEK

 //zavolame OnChange vsech OR daneho bloku
 obl_rizeni := (Sender as TBlk).OblsRizeni;
 if (obl_rizeni.Cnt > 0) then
   for i := 0 to obl_rizeni.Cnt-1 do
     obl_rizeni.ORs[i].BlkChange(Sender);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// load all blocks from file
// Pri vytvareni dostavaji vsechny bloky table_index -1, pak je hromadne
//  oindexujeme metodou UpdateBlkIndexes
function TBlky.LoadFromFile(const tech_filename,rel_filename,stat_filename:string):Integer;
var ini_tech,ini_rel,ini_stat:TMemIniFile;
    i:Integer;
    Blk:TBlk;
    str:TStrings;
begin
 writelog('Nacitam bloky: '+tech_filename+ '; '+rel_filename, WR_DATA);
 Self.ffile    := tech_filename;
 Self.ffstatus := stat_filename;

 try
   ini_tech := TMemIniFile.Create(tech_filename);
 except
   writelog('Nacitam bloky: nelze otevrit soubor bloku', WR_ERROR);
   Exit(1);
 end;

 try
   ini_rel  := TMemIniFile.Create(rel_filename);
 except
   ini_rel := nil;
   writelog('Nacitam bloky: nelze otevrit soubor s reliefy', WR_DATA);
   Exit(2);
 end;

 try
   ini_stat  := TMemIniFile.Create(stat_filename);
 except
   ini_stat := nil;
   writelog('Nacitam bloky: nelze otevrit soubor se stavy bloku', WR_DATA);
   Exit(3);
 end;

 //all data will be rewrited
 Self.DestroyBlocks();

 str := TStringList.Create();
 ini_tech.ReadSections(str);

 Blk := nil;
 for i := 0 to str.Count-1 do
  begin
   case (ini_tech.ReadInteger(str[i], 'typ', -1)) of
    _BLK_VYH      : Blk := TBlkVyhybka.Create(-1);
    _BLK_USEK     : Blk := TBlkUsek.Create(-1);
    _BLK_IR       : Blk := TBlkIR.Create(-1);
    _BLK_SCOM     : Blk := TBlkSCom.Create(-1);
    _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(-1);
    _BLK_TRAT     : Blk := TBlkTrat.Create(-1);
    _BLK_UVAZKA   : Blk := TBlkUvazka.Create(-1);
    _BLK_ZAMEK    : Blk := TBlkZamek.Create(-1);
    _BLK_ROZP     : Blk := TBlkRozp.Create(-1);
    _BLK_TU       : Blk := TBlkTU.Create(-1);

   else//case
    continue;
   end;

   Blk.LoadData(ini_tech, str[i], ini_rel, ini_stat);
   Blk.OnChange := Self.BlkChange;

   Self.data.Insert(Self.FindPlaceForNewBlk(Blk.GetGlobalSettings().id), Blk);
  end;//for i

 Self.UpdateBlkIndexes();

 FreeAndNil(ini_tech);
 FreeAndNil(ini_rel);
 FreeAndNil(ini_stat);
 FreeAndNil(str);

 for i := 0 to Self.data.Count-1 do
   Self.data[i].AfterLoad();

 writelog('Nacteno bloku: '+IntToStr(Self.Cnt), WR_DATA);
 Result := 0;
end;//function

//save all blocks to the file
procedure TBlky.SaveToFile(const tech_filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukladam bloky...', WR_DATA);

 try
   DeleteFile(PChar(tech_filename));  //all data will be rewrited
   ini := TMemIniFile.Create(tech_filename);
 except
   writelog('Ukladam bloky: nelze otevrit vystupni soubor', WR_ERROR);
   Exit();
 end;

 for i := 0 to Self.Data.Count-1 do Self.Data[i].SaveData(ini, IntToStr(i));

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Ulozeno bloku: '+IntToStr(Self.Cnt), WR_DATA);

 Self.SaveStatToFile(Self.fstatus);
end;//procedure

procedure TBlky.SaveStatToFile(const stat_filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 writelog('Ukladam stavy bloku...', WR_DATA);

 try
   DeleteFile(PChar(stat_filename));  //all data will be rewrited
   ini := TMemIniFile.Create(stat_filename);
 except
   writelog('Ukladam stavy bloky: nelze otevrit vystupni soubor', WR_ERROR);
   Exit();
 end;

 for i := 0 to Self.Data.Count-1 do Self.Data[i].SaveStatus(ini, IntToStr(i));

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Ulozen status bloku: '+IntToStr(Self.Cnt), WR_DATA);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//add 1 block
function TBlky.Add(typ:Integer; glob:TBlkSettings):TBlk;
var Blk:TBlk;
    i, index:Integer;
begin
 // kontrola existence bloku stejneho ID
 if (Self.IsBlok(glob.id)) then Exit(nil);

 index := Self.FindPlaceForNewBlk(glob.id);

 case (typ) of
  _BLK_VYH      : Blk := TBlkVyhybka.Create(index);
  _BLK_USEK     : Blk := TBlkUsek.Create(index);
  _BLK_IR       : Blk := TBlkIR.Create(index);
  _BLK_SCOM     : Blk := TBlkSCom.Create(index);
  _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(index);
  _BLK_TRAT     : Blk := TBlkTrat.Create(index);
  _BLK_UVAZKA   : Blk := TBlkUvazka.Create(index);
  _BLK_ZAMEK    : Blk := TBlkZamek.Create(index);
  _BLK_ROZP     : Blk := TBlkRozp.Create(index);
  _BLK_TU       : Blk := TBlkTU.Create(index);
 else//case
  Exit(nil);
 end;

 Blk.SetGlobalSettings(glob);
 Blk.OnChange := Self.BlkChange;
 Self.data.Insert(index, blk);
 BlokyTableData.BlkAdd(index);
 Result := Blk;

 // indexy prislusnych bloku na konci seznamu posuneme o 1 nahoru
 for i := index+1 to Self.data.Count-1 do
   Self.data[i].table_index := Self.data[i].table_index + 1;
end;//function

//Smazat blok z databaze
procedure TBlky.Delete(index:Integer);
var tmp, blk:TBlk;
    i:Integer;
begin
 if (index < 0) then raise Exception.Create('Index podtekl seznam bloku');
 if (index >= Self.Data.Count) then raise Exception.Create('Index pretekl seznam bloku');
 tmp := Self.data[index];
 if ((tmp.GetGlobalSettings().typ = _BLK_TU) and ((tmp as TBlkTU).InTrat > -1)) then
   raise Exception.Create('Tento blok je zaveden jako tratovy usek v trati ID '+IntToStr((tmp as TBlkTU).InTrat));

 Self.data.Delete(index);

 // aktulizujeme indexy bloku (dekrementujeme)
 for i := index to Self.data.Count-1 do
   Self.data[i].table_index := Self.data[i].table_index - 1;

 // pokud mazeme trat, je potreba smazat i uvazky
 if (tmp.GetGlobalSettings().typ = _BLK_TRAT) then
  begin
   Self.Delete(Blky.GetBlkIndex((tmp as TBlkTrat).GetSettings().uvazkaA));
   Self.Delete(Blky.GetBlkIndex((tmp as TBlkTrat).GetSettings().uvazkaB));
  end;
 if (tmp.GetGlobalSettings().typ = _BLK_UVAZKA) then
  begin
   Blky.GetBlkByID((tmp as TBlkUvazka).GetSettings.parent, Blk);
   if (blk <> nil) then
     Self.Delete(Blky.GetBlkIndex((tmp as TBlkUvazka).GetSettings.parent));
  end;

 FreeAndNil(tmp);
 BlokyTableData.BlkRemove(index);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkByIndex(index:integer;var Blk:TBlk):Integer;
begin
 Blk := nil;
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);

 Blk := Self.data[index];
 Result := 0;
end;//function

function TBlky.SetBlk(index:integer;data:TBlk):Integer;
begin
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);
 Self.Data[index] := data;
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

//enable all blocks
procedure TBlky.Enable();
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do Self.Data[i].Enable();
 Self.fenabled := true;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;//procedure

//disable all blocks
procedure TBlky.Disable();
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do Self.Data[i].Disable();
 Self.fenabled := false;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;//procedure

// reset all blocks
procedure TBlky.Reset();
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do Self.Data[i].Reset();
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//update all blocks
procedure TBlky.Update();
var i:Integer;
begin
 if ((not (GetFunctions.GetSystemStart())) or (MTB.generalError)) then Exit();

 for i := 0 to Self.Data.Count-1 do
  begin
   try
     Self.Data[i].Update();
   except
    on E:Exception do
     begin
      if (not log_err_flag) then
       writeLog('Blok '+Self.Data[i].GetGlobalSettings().name + ' update error : '+E.Message, WR_ERROR);
     end;
   end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// Hledame blok se zadanym ID v seznamu bloku pomoci binarniho vyhledavani.

function TBlky.GetBlkIndex(id:Integer):Integer;
var left, right, mid:Integer;
begin
 left  := 0;
 right := Self.data.Count-1;

 while (left <= right) do
  begin
   mid := (left + right) div 2;
   if (Self.data[mid].GetGlobalSettings().id = id) then Exit(mid);

   if (Self.data[mid].GetGlobalSettings().id > id) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := -1;
end;//procedure

function TBlky.GetBlkByID(id:integer;var Blk:TBlk):Integer;
var index:Integer;
begin
 Blk := nil;
 index := Self.GetBlkIndex(id);
 if (index < 0) then Exit(-1);
 Self.GetBlkByIndex(index,Blk);
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

//ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
procedure TBlky.GetORBlk(OblRizeni_id:string; conn:TIdContext);
var i,j:Integer;
    obl_rizeni:TORsRef;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   //ziskame vsechny oblasti rizeni prislusnych bloku
   obl_rizeni := Self.Data[i].OblsRizeni;

   //tyto OR porovname na "OblRizeni:PTOR"
   for j := 0 to obl_rizeni.Cnt-1 do
     if (obl_rizeni.ORs[j].id = OblRizeni_id) then
      begin
       obl_rizeni.ORs[j].BlkChange(Self.data[i], conn);
       break;
      end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//kontroluje, zda-li blok s timto ID uz nahodou existuje
//pri hledani vynechava blok s indexem index
//true = existuje, false = neexistuje
function TBlky.IsBlok(id:Integer; ignore_index:Integer = -1):boolean;
var index:Integer;
begin
 index := Self.GetBlkIndex(id);
 Result := ((index <> -1) and (index <> ignore_index));
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkID(index:Integer):Integer;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit(-1);
 Result := Self.Data[index].GetGlobalSettings.id;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkName(id:Integer):string;
var Blk:TBlk;
begin
 Self.GetBlkByID(id,Blk);
 if (not Assigned(Blk)) then Exit('## Blok s timto ID neexistuje ##');
 Result := Blk.GetGlobalSettings().name;
end;//function

function TBlky.GetBlkIndexName(index:Integer):string;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit('## Blok s timto ID neexistuje ##');
 Result := Self.Data[index].GetGlobalSettings().name;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkSComZacatekVolba(obl:string):TBlk;
var i,j:Integer;
    orindex:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if (Self.Data[i].GetGlobalSettings.typ <> _BLK_SCOM) then continue;

   orindex := -1;
   for j := 0 to (Self.Data[i] as TBlkSCom).OblsRizeni.Cnt-1 do
     if ((Self.Data[i] as TBlkSCom).OblsRizeni.ORs[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;

   if ((Integer((Self.Data[i] as TBlkSCom).ZacatekVolba) > 0) and
      ((JCDb.FindJC((Self.Data[i] as TBlkSCom).GetGlobalSettings().id, true) = -1) or ((Self.Data[i] as TBlkSCom).OblsRizeni.ORs[orindex].stack.volba = VZ))) then
     Exit(Self.Data[i]);
  end;//for i

 Result := nil;
end;//function

function TBlky.GetBlkUsekVlakPresun(obl:string):TBlk;
var i,j:Integer;
    orindex:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data[i].GetGlobalSettings.typ <> _BLK_USEK) and (Self.Data[i].GetGlobalSettings.typ <> _BLK_TU)) then continue;

   orindex := -1;
   for j := 0 to (Self.Data[i] as TBlkUsek).OblsRizeni.Cnt-1 do
     if ((Self.Data[i] as TBlkUsek).OblsRizeni.ORs[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;
   if ((Self.Data[i] as TBlkUsek).VlakPresun) then Exit(Self.Data[i]);
  end;//for i

 Result := nil;
end;//function

////////////////////////////////////////////////////////////////////////////////

//pokud je na zesilovaci zmenen zkrat
procedure TBlky.SetZesZkrat(zesil:Integer;state:boolean);
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data[i].GetGlobalSettings().typ <> _BLK_USEK) and (Self.Data[i].GetGlobalSettings().typ <> _BLK_TU)) then continue;

   if ((Self.Data[i] as TBlkUsek).GetSettings().Zesil = zesil) then
     (Self.Data[i] as TBlkUsek).ZesZkrat := state;
  end;//for i
end;//procedure

//pokud je na zesilovaci zmeneno napajeni
procedure TBlky.SetZesNapajeni(zesil:Integer;state:boolean);
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data[i].GetGlobalSettings().typ <> _BLK_USEK) and (Self.Data[i].GetGlobalSettings().typ <> _BLK_TU)) then continue;

   if ((Self.Data[i] as TBlkUsek).GetSettings().Zesil = zesil) then
     (Self.Data[i] as TBlkUsek).ZesNapajeni := state;
  end;//for i
end;//procedure

//vola se pri zmene stavu DCC
procedure TBlky.SetDCC(state:boolean);
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
   if (not state) then
     Self.Data[i].Freeze();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// pozn.: NUZ maze soupravy z bloku
procedure TBlky.NUZ(or_id:string; state:boolean = true);
var cyklus,i:Integer;
 begin
  for cyklus := 0 to Self.Data.Count-1 do
   begin
    if (Self.Data[cyklus].GetGlobalSettings().typ <> _BLK_USEK) then continue;

    for i := 0 to (Self.Data[cyklus] as TBlkUsek).OblsRizeni.Cnt-1 do
      if ((Self.Data[cyklus] as TBlkUsek).OblsRizeni.ORs[i].id = or_id) then
       begin
        if (state) then
         begin
          if ((Self.Data[cyklus] as TBlkUsek).NUZ) then
           begin
            if (Self.GetBlkWithSpr((Self.Data[cyklus] as TBlkUsek).Souprava).Count = 1) then
              Soupravy.RemoveSpr((Self.Data[cyklus] as TBlkUsek).Souprava);
            (Self.Data[cyklus] as TBlkUsek).Zaver    := TJCType.no;
            (Self.Data[cyklus] as TBlkUsek).Souprava := -1;
           end;
         end else
          (Self.Data[cyklus] as TBlkUsek).NUZ := false;
       end;
   end;//for cyklus
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.NactiBlokyDoObjektu(CB:TComboBox; Polozky:PTArSmallI; Vypust:PTArSmallI; OblRizeniID:TArStr; BlokTyp:Integer; BlokID:Integer = -1; BlokTyp2:Integer = -1);
var cyklus,i,j:Integer;
    Priradit:Boolean;
    Pocet:Integer;
    Blk:TBlk;
    Obl_r:TOrsRef;
    glob:TBlkSettings;
 begin
  Pocet := 0;
  if (Polozky <> nil) then SetLength(Polozky^,0);
  CB.Clear;
  CB.Enabled := true;

  for cyklus := 0 to Blky.Cnt-1 do
   begin
    Blky.GetBlkByIndex(cyklus,Blk);
    glob := Blk.GetGlobalSettings();

    if ((glob.typ <> BlokTyp) and (glob.typ <> BlokTyp2)) then continue;

    if (Assigned(OblRizeniID)) then
     begin
      Priradit := false;
      Obl_r := Blk.OblsRizeni;

      if ((glob.typ = _BLK_TRAT) or (glob.typ = _BLK_IR)) then
         priradit := true
      else begin
        for i := 0 to Length(OblRizeniID)-1 do
         begin
          if (Obl_r.Cnt = 0) then priradit := true;
          if (Priradit) then Break;
          for j := 0 to Obl_r.Cnt-1 do
            if (Obl_r.ORs[j].id = OBlRizeniID[i]) then
             begin
              Priradit := true;
              Break;
             end;
         end;//for i
      end;
     end else begin
      Priradit := true;
     end;

    if (not Priradit) then continue;

    if (Vypust <> nil) then
     begin
      for i := 0 to Length(Vypust^)-1 do
       begin
        if (not Priradit) then Break;
        if (glob.id = Vypust^[i]) then Priradit := false;
       end;//for cyklus2
     end;//if Vypust <> nil

    if (not Priradit) then continue;

    if (Polozky <> nil) then
     begin
      SetLength(Polozky^,Length(Polozky^)+1);
      Polozky^[Length(Polozky^)-1] := cyklus;
     end;
    CB.Items.Add(glob.name);
    if (glob.id = BlokID) then CB.ItemIndex := Pocet;
    Pocet := Pocet + 1;
   end;//for cyklus

  if (CB.Items.Count = 0) then
   begin
    CB.Items.Add('Bloky nenalezeny');
    CB.Enabled := false;
   end;//if Length(UsekNaveestidlo) = 0}
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.RemoveSpr(spr:Integer);
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data[i].GetGlobalSettings().typ = _BLK_USEK) or (Self.Data[i].GetGlobalSettings().typ = _BLK_TU)) then
    begin
     if ((Self.Data[i] as TBlkUsek).Souprava = spr)   then (Self.Data[i] as TBlkUsek).Souprava := -1;
     if ((Self.Data[i] as TBlkUsek).SprPredict = spr) then (Self.Data[i] as TBlkUsek).SprPredict := -1;
    end;
   if (Self.Data[i].GetGlobalSettings().typ = _BLK_TRAT) then (Self.Data[i] as TBlkTrat).RemoveSpr(spr);
  end;//for
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkWithSpr(spr:Integer):TBlksList;
var i:Integer;
begin
 Result := TList<TObject>.Create();
 for i := 0 to Self.Data.Count-1 do
   if (((Self.Data[i].GetGlobalSettings().typ = _BLK_USEK) or (Self.Data[i].GetGlobalSettings().typ = _BLK_TU)) and ((Self.Data[i] as TBlkUsek).Souprava = spr)) then
     Result.Add(Self.Data[i]);
end;//function

////////////////////////////////////////////////////////////////////////////////
// predpovidani soupravy na bloky v jizdni ceste

procedure TBlky.SprPrediction(Nav:TBlk);
var Usek:TBlk;
    Trat:TBlk;
    spr:Integer;
    JC:TJC;
begin
 try
   // zjistime soupravu pres navestidlem
   Usek := (Nav as TBlkSCom).UsekPred;
   if ((Nav as TBlkSCom).Navest > 0) then
     if (((Usek as TBlkUsek).Souprava > -1) and (Soupravy.soupravy[(Usek as TBlkUsek).Souprava].smer = (Nav as TBlkSCom).Smer)) then
      spr := (Usek as TBlkUsek).Souprava else spr := (Usek as TBlkUsek).SprPredict
   else
     spr := -1;
   JC := (Nav as TBlkSCom).DNjc;
   if ((JC <> nil) and (JC.stav.RozpadBlok > 0)) then Exit();

   // predpovidame, dokud existuji jizdni cesty
   while ((JC <> nil) and (JC.data.TypCesty = TJCType.vlak)) do
    begin
     // zjistime posledni usek jizdni cesty
     Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Usek);

     if ((Usek.GetGlobalSettings().typ = _BLK_TU) and ((Usek as TBlkTU).InTrat > -1)) then
      begin
       // pokud je usek v trati, zmenime usek na usek na druhem konci trati
       Blky.GetBlkByID((Usek as TBlkTU).InTrat, Trat);
       (Trat as TBlkTrat).SprPredict := spr;

       // v trati jsou jiz soupravy -> konec predpovidani
       if (TBlkTrat(Trat).stav.soupravy.Count > 0) then Exit();
       TBlkTrat(Trat).UpdateSprPredict();

       case ((Trat as TBlkTrat).Smer) of
        TTratSmer.AtoB : Blky.GetBlkByID((Trat as TBlkTrat).GetSettings().Useky[(Trat as TBlkTrat).GetSettings().Useky.Count-1], Usek);
        TTratSmer.BtoA : Blky.GetBlkByID((Trat as TBlkTrat).GetSettings().Useky[0], Usek);
       end;//case

       // souprava nebyla v trati propagovana az na konec (napr kvuli navestidlu autobloku zamknutemu na STUJ) -> konec predpovidani
       if (TBlkUsek(Usek).SprPredict <> spr) then Exit();
      end;

     // do useku vlozime predpovidnou soupravu
     (Usek as TBlkUsek).SprPredict := spr;

     // zjistime, jeslti je nejake nevastidlo u tohoto useku postaveno na volno
     Nav := (Usek as TBlkUsek).SComJCRef;
     if (Nav = nil) then
      JC := nil
     else
      JC := (Nav as TBlkSCom).DNjc;
    end;//while
 except
  if (Usek <> nil) then
    writelog('Vyjímka pøi pøedpovídání soupravy - Usek '+Usek.GetGlobalSettings.name, WR_ERROR)
  else
    writelog('Vyjímka pøi pøedpovídání soupravy', WR_ERROR);
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

class function TBlky.GetBlksList(first:TObject = nil; second:TObject = nil; third:TObject = nil):TBlksList;
begin
 Result := TList<TObject>.Create();
 if (first <> nil) then Result.Add(first);
 if (second <> nil) then Result.Add(second);
 if (third <> nil) then Result.Add(third);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetSComPrivol(oblR:TOR):TBlksList;
var i, j:Integer;
begin
 Result := TList<TObject>.Create();
 for i := 0 to Self.Data.Count-1 do
  begin
   if (self.Data[i].GetGlobalSettings.typ <> _BLK_SCOM) then continue;
   if ((Self.Data[i] as TBlkSCom).Navest <> 8) then continue;

   for j := 0 to (Self.Data[i] as TBlkSCom).OblsRizeni.Cnt-1 do
    if ((Self.Data[i] as TBlkSCom).OblsRizeni.ORs[j] = oblR) then
     begin
      Result.Add(self.Data[i]);
      break;
     end;//if ORs[j] = oblR
  end;//for i

end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetVyhWithZamek(zamekID:integer):TBlksList;
var i:Integer;
begin
 Result := TBlksList.Create();
 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data[i].GetGlobalSettings().typ = _BLK_VYH) and
      ((Self.Data[i] as TBlkVyhybka).GetSettings().zamek = zamekID)) then
    Result.Add(Self.Data[i]);
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ChangeUsekWithSpr(spr:Integer);
var Blks:TBlksList;
    i:Integer;
begin
 Blks := Self.GetBlkWithSpr(spr);
 for i := 0 to Blks.Count-1 do
  (Blks[i] as TBlk).Change();
 Blks.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetCount():Integer;
begin
 Result := Self.data.Count;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ChangeSprToTrat(spr:Integer);
var i:Integer;
begin
 for i := 0 to Self.data.Count-1 do
   if ((Self.data[i].GetGlobalSettings().typ = _BLK_TRAT) and (Self.data[i] as TBlkTrat).IsSpr(spr, true)) then
     Self.data[i].Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// najde index pro novy blok
function TBlky.FindPlaceForNewBlk(id:Integer):Integer;
var left, right, mid:Integer;
begin
 left  := 0;
 mid   := 0;
 right := Self.data.Count-1;

 // je potreba osetrit specialni pripad pridani na konec:
 if ((right >= 0) and (id > Self.data[right].GetGlobalSettings().id)) then Exit(Self.data.Count);

 while (left <= right) do
  begin
   mid := (left + right) div 2;

   // specialni pripad, kdy uz se blok anchazi v databazi, vyuziva se pri kontrole zmeny pozice bloku pri uprave ID:
   if (Self.data[mid].GetGlobalSettings().id = id) then Exit(mid);
   if (Self.data[mid].GetGlobalSettings().id > id) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := mid;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.UpdateBlkIndexes();
var i:Integer;
begin
 for i := 0 to Self.data.Count-1 do
  Self.data[i].table_index := i; 
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.BlkIDChanged(index:Integer);
var new_index, min_index, i:Integer;
begin
 new_index := FindPlaceForNewBlk(Self.data[index].GetGlobalSettings().id);
 if (index = new_index) then Exit();  // pozice bloku se nemeni -> koncime

 // provedeme prehozeni bloku na jinou pozici
 Self.data.Insert(new_index, Self.data[index]);
 if (new_index < index) then
  begin
   Self.data.Delete(index+1)
  end else begin
   Self.data.Delete(index);
  end;

 // od nejmensiho prohazovaneho indexu aktualizujeme indexy
 // aktualizjeme dokud indexy nesedi
 min_index := Min(new_index, index);
 for i := min_index to Self.data.Count-1 do
   if (Self.data[i].table_index = i) then break
   else Self.data[i].table_index := i;

 BlokyTableData.BlkMove(index, new_index);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
