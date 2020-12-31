unit BlockDb;

{ Database of all technological blocks. }

// Zakladni principy:
//  - trida TBlocks udrzuje databazi existujicich technologickych bloku
//  - tyto bloky jsou odvozeny ze spolecne abstraktni tridy TBlk
//  - tridu TBlocks vytvari main
//  - event OnChange je odesilan do prislusnych oblasti rizeni a tam se zpracovava dal (odesila se jednotlivym panelum)

// Update ve verzi 4.3.7:
//  Bloky jsou serazeny podle ID a vyhledava se v nich binarne.

interface

uses IniFiles, Block, SysUtils, Windows, TOblsRizeni, TOblRizeni, StdCtrls,
     Generics.Collections, Classes, IdContext, IBUtils, TechnologieRCS,
     JsonDataObjects, Train;

type
 TArI  = array of Integer;
 PTArI = ^TArI;
 TArStr = array of string;

 TBlksList = TList<TObject>;

 TBlocks = class(TObject)

  private
   data: TList<TBlk>;

   ffstatus: string;
   ffile: string;
   fenabled: Boolean;

    procedure DestroyBlocks();
    procedure BlkChange(Sender: TObject);

    function GetCount(): Integer;

    function FindPlaceForNewBlk(id: Integer): Integer;
    procedure UpdateBlkIndexes();             // aktualizuje indexy vsech bloku, pouziva se pri nacitani dat
    function GetItem(i: Integer): TBlk;

  public
    constructor Create();
    destructor Destroy(); override;

    //data loading/saving
    procedure LoadFromFile(const tech_filename, rel_filename, stat_filename: string);
    procedure SaveToFile(const tech_filename: string);
    procedure SaveStatToFile(const stat_filename: string);

    function Add(glob: TBlkSettings): TBlk;
    procedure Delete(index: Integer);

    function GetBlkByIndex(index: integer; var Blk: TBlk): Integer;
    function SetBlk(index: integer; data: TBlk): Integer;

    //enable/disable all block (on screen)
    procedure Enable();
    procedure Disable();
    procedure Reset();

    //update all blokcs states (should be called in timer every x ms)
    procedure Update();

    function GetBlkIndex(id: Integer): Integer;
    function GetBlkByID(id: integer; var Blk: TBlk): Integer;
    function GetBlkID(index: Integer): Integer;
    function GetBlkName(id: Integer): string;
    function GetBlkIndexName(index: Integer): string;

    function GetBlkSignalSelected(obl: string): TBlk;
    function GetBlkUsekVlakPresun(obl: string): TBlk;

    function GetNavPrivol(oblR: TOR): TBlksList;

    //ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
    procedure GetORBlk(OblRizeni_id: string; conn: TIdContext);

    //kontroluje, zda-li blok s timto ID uz nahadou existuje
    //pri hledani vynechava blok s indexem index
    //true = existuje, false = neexistuje
    function IsBlok(id: Integer; ignore_index: Integer = -1): Boolean;

    procedure OnBoosterChange(booster: string);

    procedure NUZ(or_id: string; state: Boolean = true);        //pokud true, aplikuji NUZ, pokud false, zrusim NUZ vsech bloku v OR

    procedure NactiBlokyDoObjektu(CB: TComboBox; Polozky: PTArI; Vypust: PTArI; OblRizeniID: TArStr; BlokTyp: TBlkType; BlokID: Integer = -1; BlokTyp2: TBlkType = btAny);

    procedure RemoveTrain(train: TTrain);
    procedure TrainPrediction(signal: TBlk);

    function GetBlkWithTrain(train: TTrain): TBlksList;
    function GetVyhWithZamek(zamekID: integer): TBlksList;

    // zavola change na vsechny useky, ktere obsahuji zadanou soupravu
    // pouziva se napriklad pro oznameni ukradeni LOKO
    procedure ChangeUsekWithTrain(train: TTrain);

    // zavola Change vsech trati, ktere obsahuji danou soupravu
    // pouziva se pri zmene vlastnosti soupravy -> musi se aktualizovat seznam LOKO v trati
    procedure ChangeTrainToTrat(train: TTrain);

    // volano pri zmene ID bloku na indexu \index
    // -> je potreba zmenit poradi bloku
    procedure BlkIDChanged(index: Integer);

    procedure ClearPOdj();

    class function GetBlksList(first: TObject = nil; second: TObject = nil; third: TObject = nil): TBlksList;
    class function SEPortMaxValue(addr: Integer; currentValue: Integer): Integer;

    // vrati vsechny bloky do JSON objektu PTserveru
    procedure GetPtData(json: TJsonObject; includeState: Boolean; stanice: TOR = nil; typ: TBlkType = btAny);

    procedure NouzZaverZrusen(Sender: TBlk);
    procedure ZakladniPolohaVyhybek();

    function AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;

    procedure OnClientDisconnect(client: TIdContext);

    function GetEnumerator(): TEnumerator<TBlk>;
    property Items[index : integer] : TBlk read GetItem; default;
    property count: Integer read GetCount;

    property fstatus: string read ffstatus;
    property blky_file: string read ffile;
    property enabled: Boolean read fenabled;
 end;

var
 Blocks: TBlocks;

implementation

uses BlockTurnout, BlockTrack, BlockIR, BlockSignal, fMain, BlockCrossing,
     BlockLock, TJCDatabase, Logging, BlockRailway, BlockLinker, BlockAC,
     DataBloky, TrainDb, TechnologieJC, Stack, GetSystems, BlockDisconnector,
     BlockRailwayTrack, appEv, BlockIO, PTUtils, BlockSummary,
     TechnologieAB, ACBlocks, BlockGroupSignal;

////////////////////////////////////////////////////////////////////////////////

constructor TBlocks.Create();
begin
 inherited Create();
 Self.Data     := TList<TBlk>.Create();
 Self.fenabled := false;
end;//ctor

destructor TBlocks.Destroy();
begin
 Self.DestroyBlocks();
 Self.Data.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.DestroyBlocks();
var i: Integer;
begin
 for i := 0 to Self.data.Count-1 do
  if (Assigned(Self.Data[i])) then Self.data[i].Free();
 Self.data.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

//pri zmene stavu jakehokoliv bloku je vyvolana tato metoda
//tady se resi veskere provazanosti bloku a odesilani eventu do oblasti rizeni
procedure TBlocks.BlkChange(Sender: TObject);
var blkset: TBlkSettings;
    obl_rizeni: TList<TOR>;
    blk: TBlk;
    oblr: TOR;
begin
 if (((Sender as TBlk).typ = btTrack) or ((Sender as TBlk).typ = btRT)) then
  begin
   // pri jakekoliv zmene useku dojde k Change() na vyhybce
   // navaznost: usek -> vyhybka
   blkset := (Sender as TBlk).GetGlobalSettings();
   for blk in Self.Data do
     if (blk.typ = btTurnout) then
       if ((blk as TBlkTurnout).trackID = blkset.id) then
         blk.Change();
  end;//btUsek

 //zavolame OnChange vsech OR daneho bloku
 obl_rizeni := (Sender as TBlk).stations;
 if (obl_rizeni.Count > 0) then
   for oblr in obl_rizeni do
     oblr.BlkChange(Sender);

 ACBlk.OnBlkChange(TBlk(Sender).id);
end;

////////////////////////////////////////////////////////////////////////////////

// load all blocks from file
// Pri vytvareni dostavaji vsechny bloky table_index -1, pak je hromadne
//  oindexujeme metodou UpdateBlkIndexes
procedure TBlocks.LoadFromFile(const tech_filename, rel_filename, stat_filename: string);
var ini_tech, ini_rel, ini_stat: TMemIniFile;
    id: Integer;
    Blk: TBlk;
    str: TStrings;
    section: string;
    typei: Integer;
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

       typei := ini_tech.ReadInteger(section, 'typ', -1);

       case (typei) of
         Integer(btTurnout): Blk := TBlkTurnout.Create(-1);
         Integer(btTrack): Blk := TBlkTrack.Create(-1);
         Integer(btIR): Blk := TBlkIR.Create(-1);
         Integer(btSignal): Blk := TBlkSignal.Create(-1);
         Integer(btCrossing): Blk := TBlkCrossing.Create(-1);
         Integer(btRailway): Blk := TBlkRailway.Create(-1);
         Integer(btLinker): Blk := TBlkLinker.Create(-1);
         Integer(btLock): Blk := TBlkLock.Create(-1);
         Integer(btDisconnector): Blk := TBlkDisconnector.Create(-1);
         Integer(btRT): Blk := TBlkRT.Create(-1);
         Integer(btIO): Blk := TBlkIO.Create(-1);
         Integer(btSummary): Blk := TBlkSummary.Create(-1);
         Integer(btAC): Blk := TBlkAC.Create(-1);
         Integer(btGroupSignal): Blk := TBlkGroupSignal.Create(-1);
       else
         writelog('Nenacitam blok ' + section + ' - neznamy typ', WR_ERROR);
         continue;
       end;

       Blk.LoadData(ini_tech, section, ini_rel, ini_stat);
       Blk.OnChange := Self.BlkChange;

       Self.data.Insert(Self.FindPlaceForNewBlk(Blk.id), Blk);
       Blk := nil;
     except
      on E: Exception do
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
procedure TBlocks.SaveToFile(const tech_filename: string);
var ini: TMemIniFile;
    blk: TBlk;
begin
 writelog('Ukladam bloky...', WR_DATA);

 try
   DeleteFile(PChar(tech_filename));  //all data will be rewrited
   ini := TMemIniFile.Create(tech_filename, TEncoding.UTF8);
 except
   on E: Exception do
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

procedure TBlocks.SaveStatToFile(const stat_filename: string);
var ini: TMemIniFile;
    blk: TBlk;
begin
 writelog('Ukládám stavy bloků...', WR_DATA);

 try
   DeleteFile(PChar(stat_filename));
   ini := TMemIniFile.Create(stat_filename, TEncoding.UTF8);
 except
   on E: Exception do
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
     on E: Exception do
       AppEvents.LogException(E, 'Save blok '+blk.name);
   end;
  end;

 ini.UpdateFile();
 FreeAndNil(ini);

 writelog('Uložen stav bloků: '+IntToStr(Self.count), WR_DATA);
end;

////////////////////////////////////////////////////////////////////////////////

//add 1 block
function TBlocks.Add(glob: TBlkSettings): TBlk;
var Blk: TBlk;
    i, index: Integer;
begin
 // kontrola existence bloku stejneho ID
 if (Self.IsBlok(glob.id)) then
   raise Exception.Create('Blok tohoto ID již existuje!');

 index := Self.FindPlaceForNewBlk(glob.id);

 case (glob.typ) of
  btTurnout: Blk := TBlkTurnout.Create(index);
  btTrack: Blk := TBlkTrack.Create(index);
  btIR: Blk := TBlkIR.Create(index);
  btSignal: Blk := TBlkSignal.Create(index);
  btCrossing: Blk := TBlkCrossing.Create(index);
  btRailway: Blk := TBlkRailway.Create(index);
  btLinker: Blk := TBlkLinker.Create(index);
  btLock: Blk := TBlkLock.Create(index);
  btDisconnector: Blk := TBlkDisconnector.Create(index);
  btRT: Blk := TBlkRT.Create(index);
  btIO: Blk := TBlkIO.Create(index);
  btSummary: Blk := TBlkSummary.Create(index);
  btAC: Blk := TBlkAC.Create(index);
  btGroupSignal: Blk := TBlkGroupSignal.Create(index);
 else
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
procedure TBlocks.Delete(index: Integer);
var tmp, blk: TBlk;
    i: Integer;
begin
 if (index < 0) then raise Exception.Create('Index podtekl seznam bloků');
 if (index >= Self.Data.Count) then raise Exception.Create('Index přetekl seznam bloků');
 tmp := Self.data[index];
 if ((tmp.typ = btRT) and (TBlkRT(tmp).inRailway > -1)) then
   raise Exception.Create('Tento blok je zaveden jako traťový úsek v trati ID '+IntToStr((tmp as TBlkRT).inRailway));
 if ((tmp.typ = btSignal) and (TBlkSignal(tmp).groupMaster <> nil)) then
   raise Exception.Create('Toto návěstidlo je zavedeno ve skupinovém návěstidle ' + TBlkSignal(tmp).groupMaster.name);

 Self.data.Delete(index);

 // aktulizujeme indexy bloku (dekrementujeme)
 for i := index to Self.data.Count-1 do
   Self.data[i].table_index := Self.data[i].table_index - 1;

 // pokud mazeme trat, je potreba smazat i uvazky
 if (tmp.typ = btRailway) then
  begin
   Self.Delete(Blocks.GetBlkIndex((tmp as TBlkRailway).GetSettings().linkerA));
   Self.Delete(Blocks.GetBlkIndex((tmp as TBlkRailway).GetSettings().linkerB));
  end;
 if (tmp.typ = btLinker) then
  begin
   Blocks.GetBlkByID((tmp as TBlkLinker).GetSettings.parent, Blk);
   if (blk <> nil) then
     Self.Delete(Blocks.GetBlkIndex((tmp as TBlkLinker).GetSettings.parent));
  end;

 FreeAndNil(tmp);
 BlokyTableData.BlkRemove(index);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkByIndex(index: integer; var Blk: TBlk): Integer;
begin
 Blk := nil;
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);

 Blk := Self.data[index];
 Result := 0;
end;

function TBlocks.SetBlk(index: integer; data: TBlk): Integer;
begin
 if ((index < 0) or (index >= Self.Data.Count)) then Exit(1);
 Self.Data[index] := data;
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.Enable();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Enable();
 Self.fenabled := true;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

procedure TBlocks.Disable();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Disable();
 Self.fenabled := false;
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

procedure TBlocks.Reset();
var blk: TBlk;
begin
 for blk in Self.data do
   blk.Reset();
 BlokyTableData.reload := true;
 BlokyTableData.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.Update();
var blk: TBlk;
begin
 if (not Self.enabled) then Exit();

 for blk in Self.Data do
  begin
   try
     blk.Update();
   except
    on E: Exception do
     begin
      if (not log_err_flag) then
       AppEvents.LogException(E, 'Blok '+blk.name + ' update error');
     end;
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Hledame blok se zadanym ID v seznamu bloku pomoci binarniho vyhledavani.

function TBlocks.GetBlkIndex(id: Integer): Integer;
var left, right, mid: Integer;
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

function TBlocks.GetBlkByID(id: integer; var Blk: TBlk): Integer;
var index: Integer;
begin
 Blk := nil;
 index := Self.GetBlkIndex(id);
 if (index < 0) then Exit(-1);
 Self.GetBlkByIndex(index, Blk);
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

//ziskani stavu vsech bloku na danem OR, slouzi k ziskani dat pri prvnim pripojeni OR
procedure TBlocks.GetORBlk(OblRizeni_id: string; conn: TIdContext);
var blk: TBlk;
    obl_rizeni: TList<TOR>;
    oblr: TOR;
begin
 for blk in Self.Data do
  begin
   //ziskame vsechny oblasti rizeni prislusnych bloku
   obl_rizeni := blk.stations;

   //tyto OR porovname na "OblRizeni: PTOR"
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
function TBlocks.IsBlok(id: Integer; ignore_index: Integer = -1): Boolean;
var index: Integer;
begin
 index := Self.GetBlkIndex(id);
 Result := ((index <> -1) and (index <> ignore_index));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkID(index: Integer): Integer;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit(-1);
 Result := Self.Data[index].id;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkName(id: Integer): string;
var Blk: TBlk;
begin
 Self.GetBlkByID(id, Blk);
 if (not Assigned(Blk)) then Exit('## Blok s timto ID neexistuje ##');
 Result := Blk.name;
end;

function TBlocks.GetBlkIndexName(index: Integer): string;
begin
 if (index < 0) or (index >= Self.Data.Count) then Exit('## Blok s timto ID neexistuje ##');
 Result := Self.Data[index].name;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkSignalSelected(obl: string): TBlk;
var j: Integer;
    orindex: Integer;
    blk: TBlk;
begin
 for blk in Self.Data do
  begin
   if (blk.typ <> btSignal) then continue;

   orindex := -1;
   for j := 0 to (blk as TBlkSignal).stations.Count-1 do
     if ((blk as TBlkSignal).stations[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;

   if (((blk as TBlkSignal).selected > TBlkSignalSelection.none) and
      ((JCDb.FindJCActivating((blk as TBlkSignal).id) = nil) or
        ((blk as TBlkSignal).stations[orindex].stack.mode = VZ))) then
     Exit(blk);
  end;

 Result := nil;
end;

function TBlocks.GetBlkUsekVlakPresun(obl: string): TBlk;
var j: Integer;
    orindex: Integer;
    blk: TBlk;
begin
 for blk in Self.Data do
  begin
   if ((blk.typ <> btTrack) and (blk.typ <> btRT)) then continue;

   orindex := -1;
   for j := 0 to (blk as TBlkTrack).stations.Count-1 do
     if ((blk as TBlkTrack).stations[j].id = obl) then orindex := j;

   if (orindex = -1) then continue;
   if ((blk as TBlkTrack).IsTrainMoving()) then Exit(blk);
  end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.OnBoosterChange(booster: string);
var blk: TBlk;
begin
 if (not Self.enabled) then Exit();

 for blk in Self.data do
   if ((blk.typ = btTrack) or (blk.typ = btRT)) then
     if ((booster = '') or (TBlkTrack(blk).GetSettings().boosterId = booster)) then
       TBlkTrack(blk).OnBoosterChange();
end;

////////////////////////////////////////////////////////////////////////////////

// pozn.: NUZ maze soupravy z bloku
procedure TBlocks.NUZ(or_id: string; state: Boolean = true);
var traini: Integer;
    blk: TBlk;
    usek: TBlkTrack;
    oblr: TOR;
 begin
  for blk in Self.Data do
   begin
    if (blk.typ <> btTrack) then continue;
    usek := (blk as TBlkTrack);
    if (not usek.NUZ) then continue;

    for oblr in usek.stations do
     begin
      if (oblr.id = or_id) then
       begin
        if (state) then
         begin
          for traini in usek.trains do
            if (Self.GetBlkWithTrain(Trains[traini]).Count = 1) then
              Trains.Remove(traini);

          if (ABlist.IsUsekInAnyABJC(usek.id)) then
            usek.Zaver := TZaver.ab
          else
            usek.Zaver := TZaver.no;

          usek.RemoveTrains();
         end else
          usek.NUZ := false;
       end;
     end;
   end;//for usek
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.NactiBlokyDoObjektu(CB: TComboBox; Polozky: PTArI; Vypust: PTArI; OblRizeniID: TArStr;
                                    BlokTyp: TBlkType; BlokID: Integer = -1; BlokTyp2: TBlkType = btAny);
var bloki, i: Integer;
    Priradit: Boolean;
    Pocet: Integer;
    Blk: TBlk;
    Obl_r: TList<TOR>;
    oblr: TOR;
    glob: TBlkSettings;
 begin
  Pocet := 0;
  if (Polozky <> nil) then SetLength(Polozky^,0);
  CB.Clear;
  CB.Enabled := true;

  for bloki := 0 to Blocks.count-1 do
   begin
    blk := Blocks[bloki];
    glob := Blk.GetGlobalSettings();

    if ((glob.typ <> BlokTyp) and (glob.typ <> BlokTyp2)) then continue;

    if (Assigned(OblRizeniID)) then
     begin
      Priradit := false;
      Obl_r := Blk.stations;

      if ((glob.typ = btRailway) or (glob.typ = btIR)) then
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
      SetLength(Polozky^, Length(Polozky^)+1);
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

procedure TBlocks.RemoveTrain(train: TTrain);
var blk: TBlk;
begin
 for blk in Self.data do
  begin
   if ((blk.typ = btTrack) or (blk.typ = btRT)) then
    begin
     if ((blk as TBlkTrack).IsTrain(train)) then
       (blk as TBlkTrack).RemoveTrain(train);

     if ((blk as TBlkTrack).trainPredict = train) then
       (blk as TBlkTrack).trainPredict := nil;
    end;
   if (blk.typ = btRailway) then (blk as TBlkRailway).RemoveTrain(train);
  end;//for
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetBlkWithTrain(train: TTrain): TBlksList;
var blk: TBlk;
begin
 Result := TList<TObject>.Create();
 for blk in Self.data do
   if (((blk.typ = btTrack) or (blk.typ = btRT)) and
       ((blk as TBlkTrack).IsTrain(train))) then
     Result.Add(blk);
end;

////////////////////////////////////////////////////////////////////////////////
// predpovidani soupravy na bloky v jizdni ceste

procedure TBlocks.TrainPrediction(signal: TBlk);
var usek, startUsek: TBlkTrack;
    trat: TBlkRailway;
    train: TTrain;
    JC: TJC;
begin
 try
   // zjistime soupravu pred navestidlem
   usek := TBlkTrack(TBlkSignal(signal).track);
   startUsek := usek;
   train := TBlkSignal(signal).GeTTrain(usek);

   if (TBlkSignal(signal).IsGoSignal()) then begin
     if ((not usek.IsTrain()) or
         (train.direction <> TBlkSignal(signal).direction)) then
      train := usek.trainPredict
   end else
     train := nil;
   JC := TBlkSignal(signal).DNjc;

   // predpovidame, dokud existuji jizdni cesty
   while ((JC <> nil) and (JC.typ = TJCType.train) and (JC.state.destroyBlock <= 0)) do
    begin
     // kontrola povolujici navesti
     Blocks.GetBlkByID(JC.data.signalId, signal);
     if ((signal = nil) or (signal.typ <> btSignal) or (not TBlkSignal(signal).IsGoSignal())) then
       train := nil;

     // zjistime posledni usek jizdni cesty
     Blocks.GetBlkByID(JC.data.tracks[JC.data.tracks.Count-1], TBlk(usek));

     if (usek = startUsek) then Exit();

     if ((Usek.typ = btRT) and (TBlkRT(Usek).inRailway > -1)) then
      begin
       // pokud je usek v trati, zmenime usek na usek na druhem konci trati
       Blocks.GetBlkByID(TBlkRT(Usek).inRailway, TBlk(trat));
       if (train <> nil) then begin
         if ((trat.trainPredict = nil) or (trat.trainPredict.train <> train)) then
           trat.trainPredict := TBlkRailwayTrain.Create(train.index);
       end else begin
         if (trat.trainPredict <> nil) then
           trat.trainPredict := nil;
       end;

       // v trati jsou jiz soupravy -> konec predpovidani
       if (trat.state.trains.Count > 0) then Exit();
       trat.UpdateTrainPredict(false);

       case (trat.direction) of
        TRailwayDirection.AtoB : Blocks.GetBlkByID(trat.GetSettings().trackIds[trat.GetSettings().trackIds.Count-1], TBlk(usek));
        TRailwayDirection.BtoA : Blocks.GetBlkByID(trat.GetSettings().trackIds[0], TBlk(usek));
       end;//case

       // souprava nebyla v trati propagovana az na konec (napr kvuli navestidlu autobloku zamknutemu na STUJ) -> konec predpovidani
       if ((usek.trainPredict <> train) or (usek = startUsek)) then Exit();
      end;

     // do useku vlozime predpovidnou soupravu
     usek.trainPredict := train;

     // zjistime, jeslti je nejake navestidlo u tohoto useku postaveno na volno
     if (usek.signalJCRef.Count = 0) then
      JC := nil
     else
      JC := TBlkSignal(usek.signalJCRef[0]).DNjc;
    end;//while
 except
  on E: Exception do
   begin
    if (Usek <> nil) then
      AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy - Usek '+Usek.name)
    else
      AppEvents.LogException(E, 'Vyjímka při předpovídání soupravy');
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlocks.GetBlksList(first: TObject = nil; second: TObject = nil; third: TObject = nil): TBlksList;
begin
 Result := TList<TObject>.Create();
 if (first <> nil) then Result.Add(first);
 if (second <> nil) then Result.Add(second);
 if (third <> nil) then Result.Add(third);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetNavPrivol(oblR: TOR): TBlksList;
var blk: TBlk;
    moblr: TOR;
begin
 Result := TList<TObject>.Create();
 for blk in Self.data do
  begin
   if (blk.typ <> btSignal) then continue;
   if ((blk as TBlkSignal).signal <> ncPrivol) then continue;

   for moblr in (blk as TBlkSignal).stations do
    if (moblr = oblR) then
     begin
      Result.Add(blk);
      break;
     end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetVyhWithZamek(zamekID: integer): TBlksList;
var blk: TBlk;
begin
 Result := TBlksList.Create();
 for blk in Self.data do
  begin
   if ((blk.typ = btTurnout) and
      ((blk as TBlkTurnout).GetSettings().lock = zamekID)) then
    Result.Add(blk);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ChangeUsekWithTrain(train: TTrain);
var Blks: TBlksList;
    i: Integer;
begin
 Blks := Self.GetBlkWithTrain(train);
 for i := 0 to Blks.Count-1 do
  (Blks[i] as TBlk).Change();
 Blks.Free();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetCount(): Integer;
begin
 Result := Self.data.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ChangeTrainToTrat(train: TTrain);
var blk: TBlk;
begin
 for blk in Self.data do
   if ((blk.typ = btRailway) and (blk as TBlkRailway).IsTrain(train, true)) then
     blk.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// najde index pro novy blok
// casova narocnost: linearni
function TBlocks.FindPlaceForNewBlk(id: Integer): Integer;
var i: Integer;
begin
 i := Self.data.Count-1;
 while ((i >= 0) and (Self.data[i].id > id)) do
   i := i - 1;
 Result := i+1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.UpdateBlkIndexes();
var i: Integer;
begin
 for i := 0 to Self.data.Count-1 do
  Self.data[i].table_index := i; 
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.BlkIDChanged(index: Integer);
var new_index, min_index, i: Integer;
    tmp: TBlk;
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

procedure TBlocks.GetPtData(json: TJsonObject; includeState: Boolean; stanice: TOR = nil; typ: TBlkType = btAny);
var Blk: TBlk;
begin
 for Blk in Self.data do
  begin
   try
     if ((stanice <> nil) and (not Blk.IsInOR(stanice))) then continue;
     if ((typ <> btAny) and (Blk.typ <> typ)) then continue;

     Blk.GetPtData(json.A['blocks'].AddObject, includeState);
   except
     on E: Exception do
       PTUtils.PtErrorToJson(json.A['errors'].AddObject,
        '500', 'Chyba pri nacitani bloku '+IntToStr(Blk.id)+' : '+Blk.name,
        E.Message);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.NouzZaverZrusen(Sender: TBlk);
var Blk: TBlk;
begin
 for Blk in Self.data do
   if (Blk.typ = btSignal) then
     TBlkSignal(Blk).RemoveBlkFromRnz(Sender.id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ClearPOdj();
var Blk: TBlk;
begin
 for Blk in Self.data do
   if ((Blk.typ = btTrack) or (Blk.typ = btRT)) then
     TBlkTrack(Blk).ClearPOdj();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.GetItem(i: Integer): TBlk;
begin
 Result := Self.data[i];
end;

function TBlocks.GetEnumerator(): TEnumerator<TBlk>;
begin
 Result := Self.data.GetEnumerator();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.ZakladniPolohaVyhybek();
var blk: TBlk;
 begin
  for blk in Self.data do
    if ((Blk.typ = btTurnout) and (TBlkTurnout(Blk).position <> TTurnoutPosition.plus) and (not TBlkTurnout(Blk).outputLocked) and
        (TBlkTurnout(Blk).occupied <> TTrackState.occupied) and
        ((TBlkTurnout(Blk).coupling = nil) or (TBlkTurnout(Blk).coupling.occupied <> TTrackState.occupied))) then
      TBlkTurnout(Blk).SetPosition(plus);
 end;

////////////////////////////////////////////////////////////////////////////////

class function TBlocks.SEPortMaxValue(addr: Integer; currentValue: Integer): Integer;
var tmpMax: Integer;
begin
 tmpMax := Max(Integer(RCSi.GetModuleInputsCountSafe(addr))-1, 0);
 if (currentValue > tmpMax) then
   Result := 255 // max value defined in TechnologieRCS.TRCSAddr.port
 else
   Result := tmpMax;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlocks.AnotherBlockUsesRCS(addr: TRCSAddr; me: TBlk; typ: TRCSIOType): TBlk;
var blk: TBlk;
begin
 for blk in Self.data do
   if (blk <> me) and (blk.UsesRCS(addr, typ)) then
     Exit(blk);
 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlocks.OnClientDisconnect(client: TIdContext);
var blk: TBlk;
begin
 for blk in Self.data do
   if (blk.typ = btAC) then
     TBlkAC(blk).OnClientDisconnect(client);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  Blocks := TBlocks.Create();
finalization
  FreeAndNil(Blocks);

end.//unit
