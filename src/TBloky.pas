unit TBloky;

//tato unita definuje tridu, ktera se stara o vsechny bloky
//tedy jsou v ni ulozeny  vsechny bloky

//zakladni principy:
//  - trida TBlky udrzuje databazi existujicich technologickych bloku
//  - tyto bloky jsou odvozeny ze spolecne abstraktni tridy TBlk
//  - tridu TBlky vytvari main
//  - event OnChange je odesilan do prislusnych oblasti rizeni a tam se zpracovava dal (odesila se jednotlivym panelum)

interface

uses IniFiles, TBlok, SysUtils, Windows, TOblsRizeni, TOblRizeni, StdCtrls,
      RPConst, Generics.Collections, Classes, IdContext;

type

 TBlksList = TList<TObject>;

 TBlky = class(TObject)

  private
   data:TList<TBlk>;

   ffstatus:string;
   ffile:string;

    procedure DestroyBlocks();
    procedure BlkChange(Sender:TObject);

    function GetCount():Integer;

  public
    constructor Create();
    destructor Destroy(); override;

    //data loading/saving
    function LoadFromFile(const tech_filename,rel_filename,stat_filename:string):Integer;
    procedure SaveToFile(const tech_filename:string);
    procedure SaveStatToFile(const stat_filename:string);

    //moving in array
    function MoveUp(index:Integer):Integer;
    function MoveDown(index:Integer):Integer;

    function Add(typ:Integer; glob:TBlkSettings):TBlk;
    function Delete(index:Integer):Integer;

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
    function CheckID(id,index:Integer):boolean;

    procedure SetZesZkrat(zesil:Integer;state:boolean);       //pokud je na zesilovaci zmenen zkrat
    procedure SetZesNapajeni(zesil:Integer;state:boolean);    //pokud je na zesilovaci zmeneno napajeni
    procedure SetDCC(state:boolean);                          //vola se pri zmene stavu DCC (zapnuto X vypnuto)

    procedure NUZ(or_id:string; state:boolean = true);        //pokud true, aplikuji NUZ, pokud false, zrusim NUZ vsech bloku v OR

    procedure NactiBlokyDoObjektu(CB:TComboBox;Polozky:PTArSmallI;Vypust:PTArSmallI;OblRizeniID:TArStr;BlokTyp:byte;BlokID:Integer);

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

    class function GetBlksList(first:TObject = nil; second:TObject = nil; third:TObject = nil):TBlksList;

    property Cnt:Integer read GetCount;
    property fstatus:string read ffstatus;
    property blky_file:string read ffile;
 end;//class TBlky

var
 Blky:TBlky;

implementation

uses TBlokVyhybka, TBlokUsek, TBlokIR, TBlokSCom, Main, TBlokPrejezd,
      TBlokZamek,
      TJCDatabase, Logging, TBlokTrat, TBlokUvazka, TechnologieMTB, DataBloky,
      SprDb, TechnologieJC, Zasobnik, GetSystems, TBlokRozp;

////////////////////////////////////////////////////////////////////////////////

constructor TBlky.Create();
begin
 inherited Create();
 Self.Data := TList<TBlk>.Create();
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
 case ((Sender as TBlk).GetGlobalSettings.typ) of
  _BLK_VYH:begin
   obl_rizeni := (Sender as TBlkVyhybka).OblsRizeni;
  end;//_BLK_VYH

  _BLK_USEK:begin
   obl_rizeni := (Sender as TBlkUsek).OblsRizeni;

   // pri jakekoliv zmene useku dojde k Change() na vyhybce
   // navaznost: usek -> vyhybka
   blkset := (Sender as TBlk).GetGlobalSettings();
   for i := 0 to Self.Data.Count-1 do
     if (Self.Data[i].GetGlobalSettings().typ = _BLK_VYH) then
       if ((Self.Data[i] as TBlkVyhybka).UsekID = blkset.id) then
         Self.Data[i].Change();
  end;//_BLK_USEK

  _BLK_IR:begin
   obl_rizeni.Cnt := 0;
  end;//_BLK_IR

  _BLK_SCOM:begin
   obl_rizeni := (Sender as TBlkSCom).OblsRizeni;
  end;//_BLK_VYH

  _BLK_PREJEZD:begin
   obl_rizeni := (Sender as TBlkPrejezd).OblsRizeni;
  end;//_BLK_VYH

  _BLK_TRAT:begin
    obl_rizeni.Cnt := 0;
  end;

  _BLK_UVAZKA:begin
   obl_rizeni := (Sender as TBlkUvazka).OblsRizeni;
  end;

  _BLK_ZAMEK:begin
   obl_rizeni := (Sender as TBlkZamek).OblsRizeni;
  end;

  _BLK_ROZP:begin
   obl_rizeni := (Sender as TBlkRozp).OblsRizeni;
  end;

 else
  Exit();
 end;

 //zavolame OnChange vsech OR daneho bloku
 for i := 0 to obl_rizeni.Cnt-1 do obl_rizeni.ORs[i].BlkChange(Sender);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//load all blocks from file
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
 end;

 try
   ini_stat  := TMemIniFile.Create(stat_filename);
 except
   ini_stat := nil;
   writelog('Nacitam bloky: nelze otevrit soubor se stavy bloku', WR_DATA);
 end;

 //all data will be rewrited
 Self.DestroyBlocks();

 str := TStringList.Create();
 ini_tech.ReadSections(str);

 for i := 0 to str.Count-1 do
  begin
   case (ini_tech.ReadInteger(str[i], 'typ', -1)) of
    _BLK_VYH      : Blk := TBlkVyhybka.Create(i);
    _BLK_USEK     : Blk := TBlkUsek.Create(i);
    _BLK_IR       : Blk := TBlkIR.Create(i);
    _BLK_SCOM     : Blk := TBlkSCom.Create(i);
    _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(i);
    _BLK_TRAT     : Blk := TBlkTrat.Create(i);
    _BLK_UVAZKA   : Blk := TBlkUvazka.Create(i);
    _BLK_ZAMEK    : Blk := TBlkZamek.Create(i);
    _BLK_ROZP     : Blk := TBlkRozp.Create(i);
   else//case
    continue;
   end;

   Blk.LoadData(ini_tech, str[i], ini_rel, ini_stat);
   Blk.OnChange := Self.BlkChange;
   Self.data.Add(Blk);
  end;//for i

 FreeAndNil(ini_tech);
 FreeAndNil(ini_rel);
 FreeAndNil(ini_stat);
 FreeAndNil(str);

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
 end;

 for i := 0 to Self.Data.Count-1 do Self.Data[i].SaveStatus(ini, IntToStr(i));

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Ulozen status bloku: '+IntToStr(Self.Cnt), WR_DATA);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//move block 1 element up in array (fro higher index to lower)
function TBlky.MoveUp(index:Integer):Integer;
var tmp:TBlk;
begin
 if (index < 1) then Exit(1);
 if (index >= Self.Data.Count) then Exit(2);

 tmp := Self.data[index-1];
 Self.data[index-1] := Self.data[index];
 Self.data[index] := tmp;

 BlokyTableData.BlkChange(index-1);
 BlokyTableData.BlkChange(index);
 BlokyTableData.UpdateTable();

 Result := 0;
end;//function

//move block 1 element down in array (from lower index to higer index)
function TBlky.MoveDown(index:Integer):Integer;
var tmp:TBlk;
begin
 if (index < 0) then Exit(1);
 if (index >= (Self.Data.Count-1)) then Exit(2);

 tmp := Self.data[index+1];
 Self.data[index+1] := Self.data[index];
 Self.data[index] := tmp;

 BlokyTableData.BlkChange(index+1);
 BlokyTableData.BlkChange(index);
 BlokyTableData.UpdateTable();

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

//add 1 block
function TBlky.Add(typ:Integer; glob:TBlkSettings):TBlk;
var Blk:TBlk;
begin
 case (typ) of
  _BLK_VYH      : Blk := TBlkVyhybka.Create(Self.Data.Count);
  _BLK_USEK     : Blk := TBlkUsek.Create(Self.Data.Count);
  _BLK_IR       : Blk := TBlkIR.Create(Self.Data.Count);
  _BLK_SCOM     : Blk := TBlkSCom.Create(Self.Data.Count);
  _BLK_PREJEZD  : Blk := TBlkPrejezd.Create(Self.Data.Count);
  _BLK_TRAT     : Blk := TBlkTrat.Create(Self.Data.Count);
  _BLK_UVAZKA   : Blk := TBlkUvazka.Create(Self.Data.Count);
  _BLK_ZAMEK    : Blk := TBlkZamek.Create(Self.Data.Count);
  _BLK_ROZP     : Blk := TBlkRozp.Create(Self.Data.Count);
 else//case
  Exit(nil);
 end;

 Blk.SetGlobalSettings(glob);

 Blk.OnChange := Self.BlkChange;
 Self.data.Add(blk);
 BlokyTableData.BlkAdd();
 Result := Blk;
end;//function

//delete 1 block
function TBlky.Delete(index:Integer):Integer;
var tmp, blk:TBlk;
begin
 if (index < 0) then Exit(2); 
 if (index >= Self.Data.Count) then Exit(1);

 tmp := Self.data[index];
 Self.data.Delete(index);

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

 Result := 0;
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
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;//procedure

//disable all blocks
procedure TBlky.Disable();
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do Self.Data[i].Disable();
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

function TBlky.GetBlkIndex(id:Integer):Integer;
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
   if (Self.Data[i].GetGlobalSettings().id = id) then
     Exit(i);
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
   case (Self.Data[i].GetGlobalSettings.typ) of
    _BLK_VYH      : obl_rizeni := (Self.Data[i] as TBlkVyhybka).OblsRizeni;    // bloky vyhybka preskakujeme: odeslou se automaticky v ramci useku
    _BLK_USEK     : obl_rizeni := (Self.Data[i] as TBlkUsek).OblsRizeni;
    _BLK_IR       : obl_rizeni.Cnt := 0;
    _BLK_SCOM     : obl_rizeni := (Self.Data[i] as TBlkSCom).OblsRizeni;
    _BLK_PREJEZD  : obl_rizeni := (Self.Data[i] as TBlkPrejezd).OblsRizeni;
    _BLK_TRAT     : obl_rizeni.Cnt := 0;
    _BLK_UVAZKA   : obl_rizeni := (Self.Data[i] as TBlkUvazka).OblsRizeni;
    _BLK_ZAMEK    : obl_rizeni := (Self.Data[i] as TBlkZamek).OblsRizeni;
    _BLK_ROZP     : obl_rizeni := (Self.Data[i] as TBlkRozp).OblsRizeni;
   else continue; end;

   //tyto OR porovname na "OblRizeni:PTOR"
   for j := 0 to obl_rizeni.Cnt-1 do
     if (obl_rizeni.ORs[j].id = OblRizeni_id) then
      begin
       obl_rizeni.ORs[j].BlkChange(Self.data[i], conn);
       break;
      end;

   // tady resime zapnuti zvukoveho signalu zadosti o tratovy souhlas
   if (Self.Data[i].GetGlobalSettings.typ = _BLK_UVAZKA) then
    begin
     if ((not (Self.Data[i] as TBlkUvazka).zadost) and (((Self.Data[i] as TBlkUvazka).parent as TBlkTrat).Zadost)) then
       for j := 0 to obl_rizeni.Cnt-1 do
         if (obl_rizeni.ORs[j].id = OblRizeni_id) then
          begin
           obl_rizeni.ORs[j].BlkPlaySound(Self.Data[i], TORControlRights.write, _SND_TRAT_ZADOST, 500);
           break;
          end;
    end;//if (Self.Data[i].GetGlobalSettings.typ = _BLK_UVAZKA)

  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//kontroluje, zda-li blok s timto ID uz nahadou existuje
//pri hledani vynechava blok s indexem index
//true = existuje, false = neexistuje
function TBlky.CheckID(id,index:Integer):boolean;
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
   if ((i <> index) and (Self.Data[i].GetGlobalSettings.id = id)) then
     Exit(true);
 Result := false;
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
   if (Self.Data[i].GetGlobalSettings.typ <> _BLK_USEK) then continue;

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
   if (Self.Data[i].GetGlobalSettings().typ <> _BLK_USEK) then continue;

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
   if (Self.Data[i].GetGlobalSettings().typ <> _BLK_USEK) then continue;

   if ((Self.Data[i] as TBlkUsek).GetSettings().Zesil = zesil) then
     (Self.Data[i] as TBlkUsek).ZesNapajeni := state;
  end;//for i
end;//procedure

//vola se pri zmene stavu DCC
procedure TBlky.SetDCC(state:boolean);
var i:Integer;
begin
 for i := 0 to Self.Data.Count-1 do
  begin
   if (state) then
     Self.Data[i].UnFreeze()
   else
     Self.Data[i].Freeze();
  end;
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

procedure TBlky.NactiBlokyDoObjektu(CB:TComboBox;Polozky:PTArSmallI;Vypust:PTArSmallI;OblRizeniID:TArStr;BlokTyp:byte;BlokID:Integer);
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

    if (glob.typ <> BlokTyp) then continue;

    if (Assigned(OblRizeniID)) then
     begin
      Priradit := false;

      case (glob.typ) of
       _BLK_VYH  : Obl_r := (Blk as TBlkVyhybka).OblsRizeni;
       _BLK_USEK : Obl_r := (Blk as TBlkUsek).OblsRizeni;
       _BLK_SCOM : Obl_r := (Blk as TBlkSCom).OblsRizeni;
       _BLK_IR   : begin
                    Obl_r.Cnt := 0;
                    Priradit := true;
                   end;
       _BLK_PREJEZD : Obl_r := (Blk as TBlkPrejezd).OblsRizeni;
       _BLK_ZAMEK   : Obl_r := (Blk as TBlkZamek).OblsRizeni;
       _BLK_ROZP    : Obl_r := (Blk as TBlkRozp).OblsRizeni;
      end;//case typ

      if ((glob.typ = _BLK_TRAT) or (glob.typ = _BLK_IR)) then
         priradit := true
      else  begin
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
   if (Self.Data[i].GetGlobalSettings().typ = _BLK_USEK) then
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
   if ((Self.Data[i].GetGlobalSettings().typ = _BLK_USEK) and ((Self.Data[i] as TBlkUsek).Souprava = spr)) then
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
   Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Usek);
   if ((Nav as TBlkSCom).Navest > 0) then
     if (((Usek as TBlkUsek).Souprava > -1) and (Soupravy.soupravy[(Usek as TBlkUsek).Souprava].smer = (Nav as TBlkSCom).Smer)) then
      spr := (Usek as TBlkUsek).Souprava else spr := (Usek as TBlkUsek).SprPredict
   else
     spr := -1;
   JC := (Nav as TBlkSCom).DNjc;

   // predpovidame, dokud existuji jizdni cesty
   while ((JC <> nil) and (JC.data.TypCesty = TJCType.vlak)) do
    begin
     // zjistime posledni usek jizdni cesty
     Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Usek);

     if ((Usek as TBlkUsek).InTrat > -1) then
      begin
       // pokud je usek v trati, zmenime usek na usek na druhem konci trati
       Blky.GetBlkByID((Usek as TBlkUsek).InTrat, Trat);
       (Trat as TBlkTrat).SprPredict := spr;

       case ((Trat as TBlkTrat).Smer) of
        TTratSmer.AtoB : Blky.GetBlkByID((Trat as TBlkTrat).GetSettings().Useky[Length((Trat as TBlkTrat).GetSettings().Useky)-1], Usek);
        TTratSmer.BtoA : Blky.GetBlkByID((Trat as TBlkTrat).GetSettings().Useky[0], Usek);
       end;//case
      end;

     // do useku vloziem predpovidnou soupravu
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

end.//unit
