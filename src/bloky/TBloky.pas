﻿unit TBloky;

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
     Generics.Collections, Classes, IdContext, IBUtils, TechnologieRCS,
     JsonDataObjects, Souprava;

type
 TArI  = array of Integer;
 PTArI = ^TArI;
 TArStr = array of string;

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
    function GetItem(i: Integer):TBlk;

  public
    constructor Create();
    destructor Destroy(); override;

    //data loading/saving
    procedure LoadFromFile(const tech_filename,rel_filename,stat_filename:string);
    procedure SaveToFile(const tech_filename:string);
    procedure SaveStatToFile(const stat_filename:string);

    function Add(typ:Integer; glob:TBlkSettings):TBlk;
    procedure Delete(index:Integer);

    function GetBlkByIndex(index:integer; var Blk:TBlk):Integer;
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

    function GetBlkNavZacatekVolba(obl:string):TBlk;
    function GetBlkUsekVlakPresun(obl:string):TBlk;

    function GetNavPrivol(oblR:TOR):TBlksList;

    //ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
    procedure GetORBlk(OblRizeni_id:string; conn:TIdContext);

    //kontroluje, zda-li blok s timto ID uz nahadou existuje
    //pri hledani vynechava blok s indexem index
    //true = existuje, false = neexistuje
    function IsBlok(id:Integer; ignore_index:Integer = -1):boolean;

    procedure OnBoosterChange(booster: string);

    procedure NUZ(or_id:string; state:boolean = true);        //pokud true, aplikuji NUZ, pokud false, zrusim NUZ vsech bloku v OR

    procedure NactiBlokyDoObjektu(CB:TComboBox; Polozky:PTArI; Vypust:PTArI; OblRizeniID:TArStr; BlokTyp:Integer; BlokID:Integer = -1; BlokTyp2:Integer = -1);

    procedure RemoveSpr(spr: TSouprava);
    procedure SprPrediction(Nav:TBlk);

    function GetBlkWithSpr(spr: TSouprava):TBlksList;
    function GetVyhWithZamek(zamekID:integer):TBlksList;

    // zavola change na vsechny useky, ktere obsahuji zadanou soupravu
    // pouziva se napriklad pro oznameni ukradeni LOKO
    procedure ChangeUsekWithSpr(spr: TSouprava);

    // zavola Change vsech trati, ktere obsahuji danou soupravu
    // pouziva se pri zmene vlastnosti soupravy -> musi se aktualizovat seznam LOKO v trati
    procedure ChangeSprToTrat(spr: TSouprava);

    // volano pri zmene ID bloku na indexu \index
    // -> je potreba zmenit poradi bloku
    procedure BlkIDChanged(index:Integer);

    procedure ClearPOdj();

    class function GetBlksList(first:TObject = nil; second:TObject = nil; third:TObject = nil):TBlksList;
    class function SEPortMaxValue(addr: Integer; currentValue: Integer):Integer;

    // vrati vsechny bloky do JSON objektu PTserveru
    procedure GetPtData(json:TJsonObject; includeState:boolean; stanice:TOR = nil; typ:Integer = -1);

    procedure NouzZaverZrusen(Sender:TBlk);
    procedure ZakladniPolohaVyhybek();

    function AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;

    procedure OnClientDisconnect(client: TIdContext);

    function GetEnumerator():TEnumerator<TBlk>;
    property Items[index : integer] : TBlk read GetItem; default;
    property count:Integer read GetCount;

    property fstatus:string read ffstatus;
    property blky_file:string read ffile;
    property enabled:boolean read fenabled;
 end;//class TBlky

var
 Blky:TBlky;

implementation

uses TBlokVyhybka, TBlokUsek, TBlokIR, TBlokNav, fMain, TBlokPrejezd,
      TBlokZamek, TJCDatabase, Logging, TBlokTrat, TBlokUvazka, TBlokAC,
      DataBloky, SprDb, TechnologieJC, Zasobnik, GetSystems, TBlokRozp,
      TBlokTratUsek, appEv, TBlokIO, PTUtils, TBlokSouctovaHlaska,
      TechnologieAB, ACBlocks;

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
  if (Assigned(Self.Data[i])) then Self.data[i].Free();
 Self.data.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

//pri zmene stavu jakehokoliv bloku je vyvolana tato metoda
//tady se resi veskere provazanosti bloku a odesilani eventu do oblasti rizeni
procedure TBlky.BlkChange(Sender:TObject);
var blkset:TBlkSettings;
    obl_rizeni:TList<TOR>;
    blk: TBlk;
    oblr:TOR;
begin
 if ((Sender as TBlk).typ = _BLK_USEK) then
  begin
   // pri jakekoliv zmene useku dojde k Change() na vyhybce
   // navaznost: usek -> vyhybka
   blkset := (Sender as TBlk).GetGlobalSettings();
   for blk in Self.Data do
     if (blk.typ = _BLK_VYH) then
       if ((blk as TBlkVyhybka).UsekID = blkset.id) then
         blk.Change();
  end;//_BLK_USEK

 //zavolame OnChange vsech OR daneho bloku
 obl_rizeni := (Sender as TBlk).OblsRizeni;
 if (obl_rizeni.Count > 0) then
   for oblr in obl_rizeni do
     oblr.BlkChange(Sender);

 ACBlk.OnBlkChange(TBlk(Sender).id);
end;

////////////////////////////////////////////////////////////////////////////////

// load all blocks from file
// Pri vytvareni dostavaji vsechny bloky table_index -1, pak je hromadne
//  oindexujeme metodou UpdateBlkIndexes
procedure TBlky.LoadFromFile(const tech_filename,rel_filename,stat_filename:string);
var ini_tech,ini_rel,ini_stat:TMemIniFile;
    id:Integer;
    Blk:TBlk;
    str:TStrings;
    section:string;
begin
 writelog('Načítám bloky: '+tech_filename+ '; '+rel_filename, WR_DATA);
 Self.ffile    := tech_filename;
 Self.ffstatus := stat_filename;

 ini_tech := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
 ini_rel := TMemIniFile.Create(rel_filename, TEncoding.UTF8);
 ini_stat := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
 str := TStringList.Create();

 try
   Self.DestroyBlocks();
   ini_tech.ReadSections(str);

   Blk := nil;
   for section in str do
    begin
     try
       id := StrToIntDef(section, -1);
       if (id < 0) then
        begin
         writelog('Nenacitam blok ' + section + ' - id neni validni', WR_ERROR);
         continue;
        end;

       if (Self.IsBlok(id)) then
        begin
         writelog('Nenacitam blok ' + section + ' - blok s timto id jiz existuje', WR_ERROR);
         continue;
        end;

       case (ini_tech.ReadInteger(section, 'typ', -1)) of
        _BLK_VYH      : Blk := TBlkVyhybka.Create(-1);
        _BLK_USEK     : Blk := TBlkUsek.Create(-1);
        _BLK_IR       : Blk := TBlkIR.Create(-1);
        _BLK_NAV      : Blk := TBlkNav.Create(-1);
        _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(-1);
        _BLK_TRAT     : Blk := TBlkTrat.Create(-1);
        _BLK_UVAZKA   : Blk := TBlkUvazka.Create(-1);
        _BLK_ZAMEK    : Blk := TBlkZamek.Create(-1);
        _BLK_ROZP     : Blk := TBlkRozp.Create(-1);
        _BLK_TU       : Blk := TBlkTU.Create(-1);
        _BLK_IO       : Blk := TBlkIO.Create(-1);
        _BLK_SH       : Blk := TBlkSH.Create(-1);
        _BLK_AC       : Blk := TBlkAC.Create(-1);

       else//case
         continue;
       end;

       Blk.LoadData(ini_tech, section, ini_rel, ini_stat);
       Blk.OnChange := Self.BlkChange;

       Self.data.Insert(Self.FindPlaceForNewBlk(Blk.id), Blk);
       Blk := nil;
     except
      on E:Exception do
       begin
        if (Assigned(Blk)) then Blk.Free();
        AppEvents.LogException(E, 'Nacitani bloku ' + section);
       end;
     end;
    end;//for i

   Self.UpdateBlkIndexes();
 finally
   FreeAndNil(ini_tech);
   FreeAndNil(ini_rel);
   FreeAndNil(ini_stat);
   FreeAndNil(str);
 end;

 for Blk in Self.data do
   Blk.AfterLoad();

 writelog('Načteno bloků: '+IntToStr(Self.count), WR_DATA);
end;

//save all blocks to the file
procedure TBlky.SaveToFile(const tech_filename:string);
var ini: TMemIniFile;
    blk: TBlk;
begin
 writelog('Ukladam bloky...', WR_DATA);

 try
   DeleteFile(PChar(tech_filename));  //all data will be rewrited
   ini := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Ukladam bloky: nelze otevrit vystupni soubor');
     Exit();
    end;
 end;

 for blk in Self.Data do
   blk.SaveData(ini, IntToStr(blk.id));

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Uloženo bloků: '+IntToStr(Self.count), WR_DATA);

 Self.SaveStatToFile(Self.fstatus);
end;

procedure TBlky.SaveStatToFile(const stat_filename:string);
var ini: TMemIniFile;
    blk: TBlk;
begin
 writelog('Ukládám stavy bloků...', WR_DATA);

 try
   DeleteFile(PChar(stat_filename));
   ini := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Ukladam stavy bloku: nelze otevrit vystupni soubor');
     Exit();
    end;
 end;

 for blk in Self.data do
  begin
   try
     blk.SaveStatus(ini, IntToStr(blk.id));
   except
     on E:Exception do
       AppEvents.LogException(E, 'Save blok '+blk.name);
   end;
  end;

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Uložen stav bloků: '+IntToStr(Self.count), WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////

//add 1 block
function TBlky.Add(typ:Integer; glob:TBlkSettings):TBlk;
var Blk:TBlk;
    i, index:Integer;
begin
 // kontrola existence bloku stejneho ID
 if (Self.IsBlok(glob.id)) then
   raise Exception.Create('Blok tohoto ID již existuje!');

 index := Self.FindPlaceForNewBlk(glob.id);

 case (typ) of
  _BLK_VYH      : Blk := TBlkVyhybka.Create(index);
  _BLK_USEK     : Blk := TBlkUsek.Create(index);
  _BLK_IR       : Blk := TBlkIR.Create(index);
  _BLK_NAV      : Blk := TBlkNav.Create(index);
  _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(index);
  _BLK_TRAT     : Blk := TBlkTrat.Create(index);
  _BLK_UVAZKA   : Blk := TBlkUvazka.Create(index);
  _BLK_ZAMEK    : Blk := TBlkZamek.Create(index);
  _BLK_ROZP     : Blk := TBlkRozp.Create(index);
  _BLK_TU       : Blk := TBlkTU.Create(index);
  _BLK_IO       : Blk := TBlkIO.Create(index);
  _BLK_SH       : Blk := TBlkSH.Create(index);
  _BLK_AC       : Blk := TBlkAC.Create(index);
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
end;

//Smazat blok z databaze
procedure TBlky.Delete(index:Integer);
var tmp, blk:TBlk;
    i:Integer;
begin
 if (index < 0) then raise Exception.Create('Index podtekl seznam bloku');
 if (index >= Self.Data.Count) then raise Exception.Create('Index pretekl seznam bloku');
 tmp := Self.data[index];
 if ((tmp.typ = _BLK_TU) and ((tmp as TBlkTU).InTrat > -1)) then
   raise Exception.Create('Tento blok je zaveden jako tratovy usek v trati ID '+IntToStr((tmp as TBlkTU).InTrat));

 Self.data.Delete(index);

 // aktulizujeme indexy bloku (dekrementujeme)
 for i := index to Self.data.Count-1 do
   Self.data[i].table_index := Self.data[i].table_index - 1;

 // pokud mazeme trat, je potreba smazat i uvazky
 if (tmp.typ = _BLK_TRAT) then
  begin
   Self.Delete(Blky.GetBlkIndex((tmp as TBlkTrat).GetSettings().uvazkaA));
   Self.Delete(Blky.GetBlkIndex((tmp as TBlkTrat).GetSettings().uvazkaB));
  end;
 if (tmp.typ = _BLK_UVAZKA) then
  begin
   Blky.GetBlkByID((tmp as TBlkUvazka).GetSettings.parent, Blk);
   if (blk <> nil) then
     Self.Delete(Blky.GetBlkIndex((tmp as TBlkUvazka).GetSettings.parent));
  end;

 FreeAndNil(tmp);
 BlokyTableData.BlkRemove(index);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkByIndex(index:integer;var Blk:TBlk):Integer;
begin
 Blk := nil;
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);

 Blk := Self.data[index];
 Result := 0;
end;

function TBlky.SetBlk(index:integer;data:TBlk):Integer;
begin
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);
 Self.Data[index] := data;
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.Enable();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Enable();
 Self.fenabled := true;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

procedure TBlky.Disable();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Disable();
 Self.fenabled := false;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

procedure TBlky.Reset();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Reset();
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.Update();
var blk: TBlk;
begin
 if (not Self.enabled) then Exit();

 for blk in Self.Data do
  begin
   try
     blk.Update();
   except
    on E:Exception do
     begin
      if (not log_err_flag) then
       AppEvents.LogException(E, 'Blok '+blk.name + ' update error');
     end;
   end;
  end;
end;

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
   if (Self.data[mid].id = id) then Exit(mid);

   if (Self.data[mid].id > id) then
     right := mid - 1
   else
     left := mid + 1;
  end;
 Result := -1;
end;

function TBlky.GetBlkByID(id:integer;var Blk:TBlk):Integer;
var index:Integer;
begin
 Blk := nil;
 index := Self.GetBlkIndex(id);
 if (index < 0) then Exit(-1);
 Self.GetBlkByIndex(index,Blk);
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

//ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
procedure TBlky.GetORBlk(OblRizeni_id:string; conn:TIdContext);
var blk: TBlk;
    obl_rizeni:TList<TOR>;
    oblr:TOR;
begin
 for blk in Self.Data do
  begin
   //ziskame vsechny oblasti rizeni prislusnych bloku
   obl_rizeni := blk.OblsRizeni;

   //tyto OR porovname na "OblRizeni:PTOR"
   for oblr in obl_rizeni do
    begin
     if (oblr.id = OblRizeni_id) then
      begin
       oblr.BlkChange(blk, conn);
       Break;
      end;
    end;
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

//kontroluje, zda-li blok s timto ID uz nahodou existuje
//pri hledani vynechava blok s indexem index
//true = existuje, false = neexistuje
function TBlky.IsBlok(id:Integer; ignore_index:Integer = -1):boolean;
var index:Integer;
begin
 index := Self.GetBlkIndex(id);
 Result := ((index <> -1) and (index <> ignore_index));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkID(index:Integer):Integer;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit(-1);
 Result := Self.Data[index].id;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkName(id:Integer):string;
var Blk:TBlk;
begin
 Self.GetBlkByID(id,Blk);
 if (not Assigned(Blk)) then Exit('## Blok s timto ID neexistuje ##');
 Result := Blk.name;
end;

function TBlky.GetBlkIndexName(index:Integer):string;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit('## Blok s timto ID neexistuje ##');
 Result := Self.Data[index].name;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkNavZacatekVolba(obl:string):TBlk;
var j:Integer;
    orindex:Integer;
    blk: TBlk;
begin
 for blk in Self.Data do
  begin
   if (blk.typ <> _BLK_NAV) then continue;

   orindex := -1;
   for j := 0 to (blk as TBlkNav).OblsRizeni.Count-1 do
     if ((blk as TBlkNav).OblsRizeni[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;

   if (((blk as TBlkNav).ZacatekVolba > TBlkNavVolba.none) and
      ((JCDb.FindOnlyStaveniJC((blk as TBlkNav).id) = nil) or
        ((blk as TBlkNav).OblsRizeni[orindex].stack.volba = VZ))) then
     Exit(blk);
  end;

 Result := nil;
end;

function TBlky.GetBlkUsekVlakPresun(obl:string):TBlk;
var j:Integer;
    orindex:Integer;
    blk: TBlk;
begin
 for blk in Self.Data do
  begin
   if ((blk.typ <> _BLK_USEK) and (blk.typ <> _BLK_TU)) then continue;

   orindex := -1;
   for j := 0 to (blk as TBlkUsek).OblsRizeni.Count-1 do
     if ((blk as TBlkUsek).OblsRizeni[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;
   if ((blk as TBlkUsek).IsVlakPresun()) then Exit(blk);
  end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.OnBoosterChange(booster: string);
var blk:TBlk;
begin
 for blk in Self.data do
   if ((blk.typ = _BLK_USEK) or (blk.typ = _BLK_TU)) then
     if ((booster = '') or (TBlkUsek(blk).GetSettings().Zesil = booster)) then
       TBlkUsek(blk).OnBoosterChange();
end;

////////////////////////////////////////////////////////////////////////////////

// pozn.: NUZ maze soupravy z bloku
procedure TBlky.NUZ(or_id:string; state:boolean = true);
var spri: Integer;
    blk: TBlk;
    usek: TBlkUsek;
    oblr: TOR;
 begin
  for blk in Self.Data do
   begin
    if (blk.typ <> _BLK_USEK) then continue;
    usek := (blk as TBlkUsek);
    if (not usek.NUZ) then continue;

    for oblr in usek.OblsRizeni do
     begin
      if (oblr.id = or_id) then
       begin
        if (state) then
         begin
          for spri in usek.Soupravs do
            if (Self.GetBlkWithSpr(Soupravy[spri]).Count = 1) then
              Soupravy.RemoveSpr(spri);

          if (ABlist.IsUsekInAnyABJC(usek.id)) then
            usek.Zaver := TZaver.ab
          else
            usek.Zaver := TZaver.no;

          usek.RemoveSoupravy();
         end else
          usek.NUZ := false;
       end;
     end;
   end;//for usek
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.NactiBlokyDoObjektu(CB:TComboBox; Polozky:PTArI; Vypust:PTArI; OblRizeniID:TArStr; BlokTyp:Integer; BlokID:Integer = -1; BlokTyp2:Integer = -1);
var bloki,i:Integer;
    Priradit:Boolean;
    Pocet:Integer;
    Blk:TBlk;
    Obl_r:TList<TOR>;
    oblr:TOR;
    glob:TBlkSettings;
 begin
  Pocet := 0;
  if (Polozky <> nil) then SetLength(Polozky^,0);
  CB.Clear;
  CB.Enabled := true;

  for bloki := 0 to Blky.count-1 do
   begin
    blk := Blky[bloki];
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
          if (Obl_r.Count = 0) then priradit := true;
          if (Priradit) then Break;
          for oblr in Obl_r do
            if (oblr.id = OBlRizeniID[i]) then
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
      Polozky^[Length(Polozky^)-1] := bloki;
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
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.RemoveSpr(spr: TSouprava);
var blk: TBlk;
begin
 for blk in Self.data do
  begin
   if ((blk.typ = _BLK_USEK) or (blk.typ = _BLK_TU)) then
    begin
     if ((blk as TBlkUsek).IsSouprava(spr)) then
       (blk as TBlkUsek).RemoveSouprava(spr);

     if ((blk as TBlkUsek).SprPredict = spr) then
       (blk as TBlkUsek).SprPredict := nil;
    end;
   if (blk.typ = _BLK_TRAT) then (blk as TBlkTrat).RemoveSpr(spr);
  end;//for
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetBlkWithSpr(spr: TSouprava):TBlksList;
var blk: TBlk;
begin
 Result := TList<TObject>.Create();
 for blk in Self.data do
   if (((blk.typ = _BLK_USEK) or (blk.typ = _BLK_TU)) and
       ((blk as TBlkUsek).IsSouprava(spr))) then
     Result.Add(blk);
end;

////////////////////////////////////////////////////////////////////////////////
// predpovidani soupravy na bloky v jizdni ceste

procedure TBlky.SprPrediction(nav:TBlk);
var usek, startUsek: TBlkUsek;
    trat: TBlkTrat;
    spr: TSouprava;
    JC: TJC;
begin
 try
   // zjistime soupravu pred navestidlem
   usek := TBlkUsek(TBlkNav(nav).UsekPred);
   startUsek := usek;
   spr := TBlkNav(nav).GetSouprava(usek);

   if (TBlkNav(nav).IsPovolovaciNavest()) then begin
     if ((not usek.IsSouprava()) or
         (spr.direction <> TBlkNav(nav).Smer)) then
      spr := usek.SprPredict
   end else
     spr := nil;
   JC := TBlkNav(nav).DNjc;

   // predpovidame, dokud existuji jizdni cesty
   while ((JC <> nil) and (JC.data.TypCesty = TJCType.vlak) and (JC.stav.RozpadBlok <= 0)) do
    begin
     // kontrola povolujici navesti
     Blky.GetBlkByID(JC.data.NavestidloBlok, Nav);
     if ((nav = nil) or (nav.typ <> _BLK_NAV) or (not TBlkNav(nav).IsPovolovaciNavest())) then
       spr := nil;

     // zjistime posledni usek jizdni cesty
     Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], TBlk(usek));

     if (usek = startUsek) then
       break; // ochrana proti JC na ovalu

     if ((Usek.typ = _BLK_TU) and (TBlkTU(Usek).InTrat > -1)) then
      begin
       // pokud je usek v trati, zmenime usek na usek na druhem konci trati
       Blky.GetBlkByID(TBlkTU(Usek).InTrat, TBlk(trat));
       if (spr <> nil) then begin
         if ((trat.SprPredict = nil) or (trat.SprPredict.souprava <> spr)) then
           trat.SprPredict := TBlkTratSouprava.Create(spr.index);
       end else begin
         if (trat.SprPredict <> nil) then
           trat.SprPredict := nil;
       end;

       // v trati jsou jiz soupravy -> konec predpovidani
       if (trat.stav.soupravy.Count > 0) then Exit();
       trat.UpdateSprPredict();

       case (trat.Smer) of
        TTratSmer.AtoB : Blky.GetBlkByID(trat.GetSettings().Useky[trat.GetSettings().Useky.Count-1], TBlk(usek));
        TTratSmer.BtoA : Blky.GetBlkByID(trat.GetSettings().Useky[0], TBlk(usek));
       end;//case

       // souprava nebyla v trati propagovana az na konec (napr kvuli navestidlu autobloku zamknutemu na STUJ) -> konec predpovidani
       if (usek.SprPredict <> spr) then Exit();
      end;

     // do useku vlozime predpovidnou soupravu
     usek.SprPredict := spr;

     // zjistime, jeslti je nejake navestidlo u tohoto useku postaveno na volno
     if (usek.NavJCRef.Count = 0) then
      JC := nil
     else
      JC := TBlkNav(usek.NavJCRef[0]).DNjc;
    end;//while
 except
  on E:Exception do
   begin
    if (Usek <> nil) then
      AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy - Usek '+Usek.name)
    else
      AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy');
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlky.GetBlksList(first:TObject = nil; second:TObject = nil; third:TObject = nil):TBlksList;
begin
 Result := TList<TObject>.Create();
 if (first <> nil) then Result.Add(first);
 if (second <> nil) then Result.Add(second);
 if (third <> nil) then Result.Add(third);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetNavPrivol(oblR:TOR):TBlksList;
var blk: TBlk;
    moblr: TOR;
begin
 Result := TList<TObject>.Create();
 for blk in Self.data do
  begin
   if (blk.typ <> _BLK_NAV) then continue;
   if ((blk as TBlkNav).Navest <> ncPrivol) then continue;

   for moblr in (blk as TBlkNav).OblsRizeni do
    if (moblr = oblR) then
     begin
      Result.Add(blk);
      break;
     end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetVyhWithZamek(zamekID:integer):TBlksList;
var blk: TBlk;
begin
 Result := TBlksList.Create();
 for blk in Self.data do
  begin
   if ((blk.typ = _BLK_VYH) and
      ((blk as TBlkVyhybka).GetSettings().zamek = zamekID)) then
    Result.Add(blk);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ChangeUsekWithSpr(spr: TSouprava);
var Blks:TBlksList;
    i:Integer;
begin
 Blks := Self.GetBlkWithSpr(spr);
 for i := 0 to Blks.Count-1 do
  (Blks[i] as TBlk).Change();
 Blks.Free();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetCount():Integer;
begin
 Result := Self.data.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ChangeSprToTrat(spr: TSouprava);
var blk: TBlk;
begin
 for blk in Self.data do
   if ((blk.typ = _BLK_TRAT) and (blk as TBlkTrat).IsSpr(spr, true)) then
     blk.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// najde index pro novy blok
// casova narocnost: linearni
function TBlky.FindPlaceForNewBlk(id:Integer):Integer;
var i:Integer;
begin
 i := Self.data.Count-1;
 while ((i >= 0) and (Self.data[i].id > id)) do
   i := i - 1;
 Result := i+1;
end;

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
    tmp:TBlk;
begin
 tmp := Self.data[index];
 Self.data.Delete(index);
 new_index := FindPlaceForNewBlk(tmp.id);

 Self.data.Insert(new_index, tmp);
 if (index = new_index) then Exit();  // pozice bloku se nemeni -> koncime

 // od nejmensiho prohazovaneho indexu aktualizujeme indexy
 // aktualizjeme dokud indexy nesedi
 min_index := Min(new_index, index);
 for i := min_index to Self.data.Count-1 do
   if (Self.data[i].table_index = i) then break
   else Self.data[i].table_index := i;

 BlokyTableData.BlkMove(index, new_index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.GetPtData(json:TJsonObject; includeState:boolean; stanice:TOR = nil; typ:Integer = -1);
var Blk:TBlk;
begin
 for Blk in Self.data do
  begin
   try
     if ((stanice <> nil) and (not Blk.IsInOR(stanice))) then continue;
     if ((typ <> -1) and (Blk.typ <> typ)) then continue;

     Blk.GetPtData(json.A['bloky'].AddObject, includeState);
   except
     on E:Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani bloku '+IntToStr(Blk.id)+' : '+Blk.name,
        E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.NouzZaverZrusen(Sender:TBlk);
var Blk:TBlk;
begin
 for Blk in Self.data do
   if (Blk.typ = _BLK_NAV) then
     TBlkNav(Blk).RemoveBlkFromRnz(Sender.id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ClearPOdj();
var Blk:TBlk;
begin
 for Blk in Self.data do
   if ((Blk.typ = _BLK_USEK) or (Blk.typ = _BLK_TU)) then
     TBlkUsek(Blk).ClearPOdj();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.GetItem(i: Integer):TBlk;
begin
 Result := Self.data[i];
end;

function TBlky.GetEnumerator():TEnumerator<TBlk>;
begin
 Result := Self.data.GetEnumerator();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.ZakladniPolohaVyhybek();
var blk:TBlk;
 begin
  for blk in Self.data do
    if ((Blk.typ = _BLK_VYH) and (TBlkVyhybka(Blk).Poloha <> TVyhPoloha.plus) and (not TBlkVyhybka(Blk).outputLocked) and
        (TBlkVyhybka(Blk).Obsazeno <> TUsekStav.obsazeno) and
        ((TBlkVyhybka(Blk).spojka = nil) or (TBlkVyhybka(Blk).spojka.Obsazeno <> TUsekStav.obsazeno))) then
      TBlkVyhybka(Blk).SetPoloha(plus);
 end;

////////////////////////////////////////////////////////////////////////////////

class function TBlky.SEPortMaxValue(addr: Integer; currentValue: Integer): Integer;
var tmpMax: Integer;
begin
 tmpMax := Max(Integer(RCSi.GetModuleInputsCountSafe(addr))-1, 0);
 if (currentValue > tmpMax) then
   Result := 255 // max value defined in TechnologieRCS.TRCSAddr.port
 else
   Result := tmpMax;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlky.AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;
var blk: TBlk;
begin
 for blk in Self.data do
   if (blk <> me) and (blk.UsesRCS(addr, typ)) then
     Exit(blk);
 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlky.OnClientDisconnect(client: TIdContext);
var blk: TBlk;
begin
 for blk in Self.data do
   if (blk.typ = _BLK_AC) then
     TBlkAC(blk).OnClientDisconnect(client);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Blky := TBlky.Create();
finalization
  FreeAndNil(Blky);

end.//unit
