unit TBlokTrat;

{
 Definice a obsluha technologickeho bloku Trat
 tento blok by se ve skutecnosti na panelu nemel vyskytnout - slouzi pouze jako
 rodic dvou uvazek.

 U bloku trati je zajisteno, ze existuji a jsou typu TBlkTU
 Bloky, ktere tomuto nevyhovuji, jsou po startu odstraneny.
}

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, JsonDataObjects,
     Generics.Collections, Train;

const
  _MAX_TRAT_TRAINS = 6;

type
 TTratZZ  = (souhlas = 0, nabidka = 2);                                         // typ tratoveho zabezpecovaciho zarizeni
 TTratSmer = (disabled = -1, zadny = 0, AtoB = 1, BtoA = 2);                    // mozne smery trati; disabled = cely blok trati je disabled
 TTratNavestidla = (autoblok = 0, hradlo = 1);

 //technologicka nastaveni trati
 TBlkTratSettings = record
  uvazkaA, uvazkaB: Integer;                                                     // reference na ID bloku trati
  zabzar: TTratZZ;                                                               // typ tratoveho zabezpecovaciho zarizeni
  navestidla: TTratNavestidla;                                                   // typ navestidel v trati
  Useky: TList<integer>;                                                         // reference na ID bloku v trati
 end;

 TBlkTratEFull = class(Exception);
 TBlkTratETimeNotDefined = class(Exception);

 // Jedna souprava v trati.
 TBlkTraTTrain = class
  private
    mTime: TTime;
    mTimeDefined: Boolean;

     function GetTime(): TTime;
     procedure SetTime(time: TTime);
     function GeTTrain(): TTrain;

  public
    traini: Integer;                                                          // index soupravy
    predict: Boolean;

     constructor Create(train: Integer); overload;
     constructor Create(train: Integer; time: TTime; predict: Boolean = false); overload;
     function IsTimeDefined(): Boolean;
     procedure UndefTime();
     property time: TTime read GetTime write SetTime;
     property train: TTrain read GeTTrain;

     function SerializeForPanel(trat: TBlk; trainPredict: Boolean = false): string;
 end;

 //aktualni stav trati
 TBlkTratStav = record
  zaver: Boolean;                                                                // aktualni stav zaveru na trati
  smer: TTratSmer;                                                               // aktualni smer trati, reprezentuje i enabled bloku
  zadost: Boolean;                                                               // flag probihajici zadosti, zadost vzdy probiha proti aktualnimu smeru trati
  trains: TObjectList<TBlkTraTTrain>;                                            // seznam souprav v trati
  trainPredict: TBlkTraTTrain;                                                   // predpovidana souprava
  BP: Boolean;                                                                   // jestli je v trati zavedena blokova podminka
 end;

 TTrainUsek = record
  trat_index: Integer;
  usek: TBlk;
 end;

 TBlkTrat = class(TBlk)
  const
   //defaultni stav
   _def_trat_stav: TBlkTratStav = (
    zaver: false;
    smer: disabled;
    zadost: false;
    trainPredict: nil;
   );

  private
   TratSettings: TBlkTratSettings;
   TratStav: TBlkTratStav;

   file_smer: TTratSmer;

   fuvazkaA, fuvazkaB: TBlk;                                                    // tady si ukladame reference na skutecne bloky, ktere si vytvarime az pri prvnim pristupu k uvazce pres \uvazkaA a \uvazkaB
   fNavLichy, fNavSudy: TBlk;                                                   // analogicky funguji krajni navestidla trati, viz \navLichy a \navSudy
                                                                                // fNavLichy je navestidlo u stanice blize pocatku trati, fNavSudy navestidlo u stanice blize konce trati

    function GetUvazkaA(): TBlk;
    function GetUvazkaB(): TBlk;

    function GetObsazeno(): Boolean;
    function GetZAK(): Boolean;
    function GetRBP(): Boolean;
    function GetNouzZaver(): Boolean;

    procedure SetTratSmer(smer: TTratSmer);
    procedure SetTratZaver(zaver: Boolean);
    procedure SetTratZadost(zadost: Boolean);
    procedure SetTrainPredict(train: TBlkTraTTrain);

    procedure SetBP(state: Boolean);

    function GetNavLichy(): TBlk;
    function GetNavSudy(): TBlk;

    procedure CheckTUExist();                                                   // zkontroluje existenci vsech bloku, ktere maji v trati byt; nevalidni bloky z trati smaze a provede o tom zapis do LOGu
    procedure InitTUs();                                                        // inicializuje tratove useky - oznami jim, ze jsou v trati a provede mnoho dalsich veci, viz telo metody
    procedure ResetTUs();                                                       // resetuje stav tratoveho useku; tratovy usek zapomene, ze je v nejake trati a stane se neutralnim tratovym usekem, ktery nic nevi

    function GetReady(): Boolean;                                                // vrati, jestli jsou vsechny tratove useky pripraveny pro vjezd soupravy, pouziva se pri zjistovani toho, jestli je mozne obratit smer trati
    function GetTrainIndex(train: TTrain): Integer;
    function TrainTUsCount(train: TTrain): Integer;

    function mGetLastUsek(): TBlk;
    function GetVyluka(): Boolean;

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    procedure AfterLoad(); override;

    //update states
    procedure Change(now: Boolean = false); override;
    procedure ChangeFromUv(Sender: TBlk);
    procedure ChangeUseky();

    //----- trat own functions -----

    function GetSettings(): TBlkTratSettings;
    procedure SetSettings(data: TBlkTratSettings);

    function IsFirstUvazka(uv: TBlk): Boolean;
    procedure TrainChangeOR(train: TTrain); overload;
    procedure TrainChangeOR(train: TTrain; smer: TTratSmer); overload;

    procedure AddTrain(train: TTrain); overload;
    procedure AddTrain(train: TBlkTraTTrain); overload;
    function GetTrainsList(separator: Char): string;
    procedure RemoveTrain(train: TTrain);

    function IsTrain(train: TTrain; predict: Boolean = true): Boolean;
    function IsTrainInAnyTU(train: TTrain): Boolean;
    function IsTrainInMoreTUs(train: TTrain): Boolean;

    procedure CallChangeToTU();
    procedure UpdateTrainPredict(call_prediction: Boolean = true);
    function NavestProtismer(): Integer;

    function SameUserControlsBothUvazka(): Boolean;                              // vraci true prave tehdy, kdyz obe uvazky kontrlu stejny uzivatel
                                                                                // kdyz je true, do trati neni potreba zadat

    function ChangesTrainDir(): Boolean;                                         // vraci true prave tehdy, kdyz se v trati meni smer soupravy
    function GetTrainUsek(train: TTrain): TBlk;
    function GetLastUsek(smer: TTratSmer): TBlk;
    function HasAutoblokNav(blk: TBlk): Boolean;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    property uvazkaA: TBlk read GetUvazkaA;                                      // blok uvazky blize zacatku trati
    property uvazkaB: TBlk read GetUvazkaB;                                      // blok uvazky blize konci trati
    property RBPCan: Boolean read GetRBP;                                        // vraci, jestli v trati doslo k poruse uplne blokove podminky, resp. jesli je mozno ji zrusit

    property stav: TBlkTratStav read TratStav;                                   // kompletni stav trati
    property Obsazeno: Boolean read GetObsazeno;                                 // flag obsazenosti trati
    property Smer: TTratSmer read TratStav.smer write SetTratSmer;               // aktualni smer trati, reprezentuje i enabled celeho bloku
    property Zaver: Boolean read TratStav.zaver write SetTratZaver;              // flag klasickeho zaveru trati (typicky od vlakove cesty)
    property ZAK: Boolean read GetZAK;                                           // flag zakazu odjezdu do trati
    property nouzZaver: Boolean read GetNouzZaver;                               // flag nouzoveho zaveru trati
    property Zadost: Boolean read TratStav.zadost write SetTratZadost;           // flag probihajici zadosti o tratovy souhlas
    property BP: Boolean read TratStav.BP write SetBP;                           // blokova podminka - zavedeni a zruseni; blokova podminka se zavadi obsazenim prvniho useku trati z jizdni cesty, rusi se pri uvolneni posledni soupravy z trati
    property trainPredict: TBlkTraTTrain read TratStav.trainPredict write SetTrainPredict; // predpovidana souprava do trati
    property lastUsek: TBlk read mGetLastUsek;                                   // posledni usek trati (smerove zavisle)
    property vyluka: Boolean read GetVyluka;

    // vrati hranicni navestidla
    property navLichy: TBlk read GetNavLichy;                                    // hranicni navestidlo trati blize zacatku trati
    property navSudy: TBlk read GetNavSudy;                                      // hranicni navestidlo trati blize konci trati

    property ready: Boolean read GetReady;                                       // jsou vsechny tratove useky "ready"? typicky se pouziva jako flag moznosti zmeny smeru trati
    property zabzar: TTratZZ read TratSettings.zabzar;
    property navestidla: TTratNavestidla read TratSettings.navestidla;

 end;//class TBlkTrat

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, TOblRizeni, TBlokNav, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlokUsek, TBlokUvazka, TrainDb, THVDatabase,
    TBlokTratUsek, appEv, timeHelper, ownConvert, Graphics;

constructor TBlkTrat.Create(index: Integer);
begin
 inherited;

 Self.GlobalSettings.typ := btTrat;
 Self.TratStav := _def_trat_stav;

 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.fNavLichy := nil;
 Self.fNavSudy  := nil;

 Self.TratSettings.Useky := TList<Integer>.Create();
 Self.TratStav.trains := TObjectList<TBlkTraTTrain>.Create();
end;//ctor

destructor TBlkTrat.Destroy();
begin
 Self.TratStav.trains.Free();
 Self.TratSettings.Useky.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var str: TStrings;
    i: Integer;
    data: TStrings;
    index: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TratSettings.uvazkaA  := ini_tech.ReadInteger(section, 'uvazkaA', -1);
 Self.TratSettings.uvazkaB  := ini_tech.ReadInteger(section, 'uvazkaB', -1);
 i := ini_tech.ReadInteger(section, 'zabzar', 0);
 if (i = 1) then i := 2;
 Self.TratSettings.zabzar := TTratZZ(i);
 Self.TratSettings.navestidla := TTratNavestidla(ini_tech.ReadInteger(section, 'navestidla', 0));

 Self.file_smer := TTratSmer(ini_stat.ReadInteger(section, 'smer', 1));

 Self.TratStav.BP := ini_stat.ReadBool(section, 'BP', false);

 data := TStringList.Create();
 ExtractStrings([',', ';'], [], PChar(ini_stat.ReadString(section, 'spr', '')), data);
 Self.TratStav.trains.Clear();
 for i := 0 to data.Count-1 do
  begin
   index := Trains.GetTrainIndexByName(data[i]);
   if (index > -1) then Self.TratStav.trains.Add(TBlkTraTTrain.Create(index));
  end;
 data.Free();

 str := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini_tech.ReadString(section, 'useky', '')), str);
 Self.TratSettings.Useky.Clear();
 for i := 0 to str.Count-1 do
  begin
   try
    Self.TratSettings.Useky.Add(StrToInt(str[i]));
   except

   end;
  end;//for i
 str.Free();
end;

procedure TBlkTrat.SaveData(ini_tech: TMemIniFile; const section: string);
var str: string;
    i: Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'uvazkaA', Self.TratSettings.uvazkaA);
 ini_tech.WriteInteger(section, 'uvazkaB', Self.TratSettings.uvazkaB);
 ini_tech.WriteInteger(section, 'zabzar', Integer(Self.TratSettings.zabzar));
 ini_tech.WriteInteger(section, 'navestidla', Integer(Self.TratSettings.navestidla));

 str := '';
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  str := str + IntToStr(Self.TratSettings.Useky[i]) + ',';
 ini_tech.WriteString(section, 'useky', str)
end;

procedure TBlkTrat.SaveStatus(ini_stat: TMemIniFile; const section: string);
var i: Integer;
    str: string;
begin
 ini_stat.WriteInteger(section, 'smer', Integer(Self.file_smer));

 if (Self.TratStav.BP) then
   ini_stat.WriteBool(section, 'BP', Self.TratStav.BP);

 str := '';
 for i := 0 to Self.TratStav.trains.Count-1 do
   str := str + Self.TratStav.trains[i].train.name + ';';

 if (str <> '') then
   ini_stat.WriteString(section, 'spr', str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.Enable();
begin
 Self.TratStav.smer := Self.file_smer;
 Self.Change();
end;

procedure TBlkTrat.Disable();
begin
 if (Self.Smer <> TTratSmer.disabled) then
   Self.file_smer := Self.Smer;
 Self.TrainPredict := nil;
 Self.TratStav.Smer := TTratSmer.disabled;
 Self.Change(true);
end;

procedure TBlkTrat.Reset();
begin
 Self.Zaver  := false;
 Self.Zadost := false;
 Self.TrainPredict := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AfterLoad();
begin
 Self.CheckTUExist();
 Self.InitTUs();
end;

// change je vyvolano i pri zmene obsazenosti jakehokoliv useku v trati
procedure TBlkTrat.Change(now: Boolean = false);
begin
 inherited Change(now);

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;

 (Self.uvazkaA as TBlkUvazka).ChangeFromTrat();
 (Self.uvazkaB as TBlkUvazka).ChangeFromTrat();

 inherited Update();
end;

procedure TBlkTrat.ChangeFromUv(Sender: TBlk);
begin
 if (Sender = Self.uvazkaA) then
  (Self.uvazkaB as TBlkUvazka).ChangeFromTrat();
 if (Sender = Self.uvazkaB) then
  (Self.uvazkaA as TBlkUvazka).ChangeFromTrat();

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ChangeUseky();
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk <> nil) and (Blk.typ = btTU)) then
    Blk.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetObsazeno(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btTU)) then continue;
   if ((Blk as TBlkTU).Obsazeno = TUsekStav.obsazeno) then
    Exit(true);
  end;

 Result := false;
end;

function TBlkTrat.GetZAK(): Boolean;
begin
 if ((Self.uvazkaA = nil) or (Self.uvazkaB = nil)) then Exit(false); 
 if (((Self.uvazkaA as TBlkUvazka).ZAK) or ((Self.uvazkaB as TBlkUvazka).ZAK)) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.SetTratSmer(smer: TTratSmer);
begin
 Self.TratStav.smer := smer;

 // zrusime blokovou podminku
 Self.BP := false;

 Self.Change();
 Self.CallChangeToTU();
end;

procedure TBlkTrat.SetTratZaver(Zaver: Boolean);
begin
 if (Self.TratStav.zaver <> Zaver) then
  begin
   Self.TratStav.zaver := zaver;
   Self.TrainPredict := nil;
   Self.Change();
  end else begin
   Self.TrainPredict := nil;
  end;
end;

procedure TBlkTrat.SetTratZadost(Zadost: Boolean);
var uvazka: TBlkUvazka;
    oblr: TOR;
begin
 if (Self.Zadost = Zadost) then Exit(); 

 // tady se resi prehravani zvuku
 try
   uvazka := nil;
   if ((Self.fuvazkaA as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaB as TBlkUvazka)
   else if ((Self.fuvazkaB as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaA as TBlkUvazka);

   if ((uvazka <> nil) and (Zadost <> Self.TratStav.zadost)) then
    begin
     if (Zadost) then
      begin
       for oblr in uvazka.OblsRizeni do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt + 1;
      end else begin
       for oblr in uvazka.OblsRizeni do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt - 1;
      end;
    end;
 except
   on E: Exception do
     AppEvents.LogException(E, 'SetTratZadost');
 end;

 Self.TratStav.zadost := zadost;
 Self.Change();
end;

procedure TBlkTrat.SetTrainPredict(train: TBlkTraTTrain);
begin
 if (Self.TratStav.TrainPredict = train) then Exit();

 if (Self.TratStav.TrainPredict <> nil) then
   FreeAndNil(Self.TratStav.TrainPredict);

 if (train <> nil) then
   Self.TratStav.trainPredict := train;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSettings(): TBlkTratSettings;
begin
 Result := Self.TratSettings;
end;

procedure TBlkTrat.SetSettings(data: TBlkTratSettings);
begin
 Self.ResetTUs();
 if (data.Useky <> Self.TratSettings.Useky) then
   Self.TratSettings.Useky.Free();

 Self.TratSettings := data;

 if (not Assigned(data.Useky)) then data.Useky := TList<Integer>.Create();
 Self.CheckTUExist();
 Self.InitTUs();

 // zrusim uvazku, aby se prepocitala
 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsFirstUvazka(uv: TBlk): Boolean;
begin
 if (uv = Self.uvazkaA) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetUvazkaA(): TBlk;
begin
 if (Self.fuvazkaA = nil) then
  Blky.GetBlkByID(Self.TratSettings.uvazkaA, Self.fuvazkaA);
 Result := Self.fuvazkaA;
end;

function TBlkTrat.GetUvazkaB(): TBlk;
begin
 if (Self.fuvazkaB = nil) then
  Blky.GetBlkByID(Self.TratSettings.uvazkaB, Self.fuvazkaB);
 Result := Self.fuvazkaB;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetRBP(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (TBlkTU(Blk).poruchaBP) then Exit(true);
  end;//for i
 Exit(false);
end;

////////////////////////////////////////////////////////////////////////////////
// zavedeni / zruseni blokove podminky
// zavest blokovou podminky lze vzdy, zrusit ji lze jen tehdy, kdyz
//  na zadnem tratovem useku neni blokova podminka

procedure TBlkTrat.SetBP(state: Boolean);
var i: Integer;
    Blk: TBlk;
begin
 if (Self.BP = state) then Exit();

 if (state) then
  begin
   Self.TratStav.BP := true;
  end else begin
   for i := 0 to Self.TratSettings.Useky.Count-1 do
    begin
     Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
     if (TBlkTU(Blk).bpInBlk) then Exit();
    end;
   Self.TratStav.BP := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AddTrain(train: TTrain);
begin
 Self.AddTrain(TBlkTraTTrain.Create(train.index, timeHelper.hJOPnow()));
end;

procedure TBlkTrat.AddTrain(train: TBlkTraTTrain);
begin
 if (Self.TratStav.trains.Count >= _MAX_TRAT_TRAINS) then
   raise TBlkTratEFull.Create('Trat je plna!');

 Self.TratStav.trains.Add(train);
 if (train <> Self.trainPredict) then
   Self.trainPredict := nil // will also Free
 else
   Self.TratStav.trainPredict := nil; // will not Free

 if (not train.IsTimeDefined()) then
   train.time := timeHelper.hJOPnow();

 writelog('Trať '+Self.GlobalSettings.name+ ' : přidána souprava '+train.train.name, WR_SPRPREDAT);

 Self.Change();
end;

procedure TBlkTrat.RemoveTrain(train: TTrain);
var toChange: Boolean;
begin
 toChange := false;

 if ((Self.trainPredict <> nil) and (Self.trainPredict.train = train)) then
  begin
   Self.trainPredict := nil;
   toChange := true;
  end;

 if (Self.IsTrain(train)) then
  begin
   Self.TratStav.trains.Delete(Self.GetTrainIndex(train));
   writelog('Trať '+Self.GlobalSettings.name+ ' : smazána souprava '+train.name, WR_SPRPREDAT);
   toChange := true;
  end;

 if (toChange) then
   Self.Change(); 
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetTrainsList(separator: Char): string;
var train: TBlkTraTTrain;
begin
 Result := '';

 for train in Self.TratStav.trains do
   Result := Result + train.SerializeForPanel(Self) + separator;

 if (Self.trainPredict <> nil) then
   Result := Result + Self.trainPredict.SerializeForPanel(Self, true) + separator;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.TrainChangeOR(train: TTrain);
begin
 Self.TrainChangeOR(train, Self.Smer);
end;

procedure TBlkTrat.TrainChangeOR(train: TTrain; smer: TTratSmer);
begin
 case (smer) of
   TTratSmer.AtoB: begin
      if ((Self.uvazkaB as TBlkUvazka).OblsRizeni.Count > 0) then
        train.station := (Self.uvazkaB as TBlkUvazka).OblsRizeni[0]
      else
        train.station := nil;
   end;//AtoB
   TTratSmer.BtoA: begin
      if ((Self.uvazkaA as TBlkUvazka).OblsRizeni.Count > 0) then
        train.station := (Self.uvazkaA as TBlkUvazka).OblsRizeni[0]
      else
        train.station := nil;
   end;//BtoA
 end;//case

 writelog('Trať '+Self.GlobalSettings.name+ ' : souprava '+train.name+
          ' : stanice změněna na '+(train.station as TOR).Name, WR_SPRPREDAT);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetNouzZaver(): Boolean;
begin
 Result := (Self.uvazkaA as TBlkUvazka).nouzZaver or (Self.uvazkaB as TBlkUvazka).nouzZaver;
end;

////////////////////////////////////////////////////////////////////////////////

// vrati hranicni navestidlo trati na jejim zacatku
// hranicni navestidlo musi
//  - byt ve stejne OR, jako uvazka
//  - mit blok_pred_id hranicni blok trati
//  - nebyl navestidlo autobloku druheho useku trati
function TBlkTrat.GetNavLichy(): TBlk;
var Blk: TBlk;
    BlkTU: TBlkTU;
begin
 if (Self.TratSettings.Useky.Count = 0) then Exit(nil);

 if ((Self.fNavLichy = nil) or ((Self.fNavLichy as TBlkNav).UsekID <> Self.TratSettings.Useky[0])) then
  begin
   if (Self.TratSettings.Useky.Count > 1) then
     Blky.GetBlkByID(Self.TratSettings.Useky[1], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> btNav) then continue;
     if ((TBlkNav(Blk).UsekID = Self.TratSettings.Useky[0]) and
         (Blk.OblsRizeni[0] = Self.uvazkaA.OblsRizeni[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.navLid))) then
      begin
       Self.fNavLichy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavLichy;
end;

// vrati hranicni navestidlo trati na jejim konci
function TBlkTrat.GetNavSudy(): TBlk;
var Blk: TBlk;
    BlkTU: TBlkTU;
begin
 if (Self.TratSettings.Useky.Count = 0) then Exit(nil);

 if ((Self.fNavSudy = nil) or ((Self.fNavSudy as TBlkNav).UsekID <> Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1])) then
  begin
   if (Self.TratSettings.Useky.Count > 1) then
     Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-2], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> btNav) then continue;
     if ((TBlkNav(Blk).UsekID = Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1]) and
         (Blk.OblsRizeni[0] = Self.uvazkaB.OblsRizeni[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.navSid))) then
      begin
       Self.fNavSudy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavSudy;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsTrain(train: TTrain; predict: Boolean = true): Boolean;
begin
 Result := ((Self.GetTrainIndex(train) > -1) or
            ((predict) and (Self.trainPredict <> nil) and (Self.trainPredict.train = train)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetTrainIndex(train: TTrain): Integer;
var i: Integer;
begin
 for i := 0 to Self.TratStav.trains.Count-1 do
   if (Self.TratStav.trains[i].train = train) then
     Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CheckTUExist();
var i: Integer;
    Blk: TBlk;
begin
 for i := Self.TratSettings.Useky.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btTU)) then
    begin
     writelog('Trat '+Self.name+' obsahuje referenci na TU ID '+IntToStr(Self.TratSettings.Useky[i])+', tento blok ale bud neexistuje, nebo neni typu TU, odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
     continue;
    end;
   if (((Blk as TBlkTU).InTrat <> -1) and ((Blk as TBlkTU).InTrat <> Self.id)) then
    begin
     writelog('Trat '+Self.name+': TU ID '+IntToStr(Self.TratSettings.Useky[i])+' jiz referuje na trat ID '+IntToStr((Blk as TBlkTU).InTrat)+', odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.TrainTUsCount(train: TTrain): Integer;
var usek: Integer;
    Blk: TBlk;
begin
 Result := 0;
 for usek in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usek, Blk);
   if (TBlkTU(Blk).IsTrain(train)) then
     Inc(Result);
  end;
end;

function TBlkTrat.IsTrainInAnyTU(train: TTrain): Boolean;
begin
 Result := (Self.TrainTUsCount(train) > 0);
end;

function TBlkTrat.IsTrainInMoreTUs(train: TTrain): Boolean;
begin
 Result := (Self.TrainTUsCount(train) > 1);
end;

////////////////////////////////////////////////////////////////////////////////
// vytvoreni navaznosti mezi tratovymi useky, sekcemi tratovych useku a
// navestidly autobloku

procedure TBlkTrat.InitTUs();
var i: Integer;
    lTU, sTU: TBlkTU;
    useky: TList<TBlkTU>;
    blk, sMaster: TBlkTU;
begin
 // 1) nejprve vytvorime navaznosti mezi tratovymi useky:
 //    Kazdemu TU rekneme, jaky TU je vedle neho v lichem smeru (bliz zacatku trati)
 //    a jaky TU je vedle enho v sudem smeru (bliz konci trati).
 //    Krajni TU maji referenci na dalsi TU "nil".

 if (Self.TratSettings.Useky.Count = 0) then Exit(); 

 lTU := nil;
 Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(blk));
 if (Self.TratSettings.Useky.Count > 1) then
   Blky.GetBlkByID(Self.TratSettings.Useky[1], TBlk(sTU))
 else
   sTU := nil;

 for i := 0 to Self.TratSettings.Useky.Count-2 do
  begin
   Blk.lTU := lTU;
   Blk.sTU := sTU;
   lTU := Blk;
   Blk := sTU;
   if (i < Self.TratSettings.Useky.Count-2) then
     Blky.GetBlkByID(Self.TratSettings.Useky[i+2], TBlk(sTU));
  end;

 // posledni TU:
 Blk.lTU := lTU;
 Blk.sTU := nil;

 /////////////////////////////////////////////////////////////////////////////
 // 2) Kazdemu TU priradime jeho Section Master a Section Masteru priradime
 //    jeho useky.

 //  a) v lichem smeru: jdeme od zacatku trati ke konci
 Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(sMaster));
 useky := sMaster.lsectUseky;
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));

   // useku take priradime, ze je v nasi trati
   (Blk as TBlkTU).InTrat := Self.id;

   if (blk.GetSettings().navLid <> -1) then
    begin
     sMaster := blk;
     useky := sMaster.lsectUseky;
    end;
   blk.lsectMaster := sMaster;
   useky.Add(blk);
  end;

 //  b) v sudem smeru: jdeme od konce trati k zacatku
 Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], TBlk(sMaster));
 useky := sMaster.ssectUseky;
 for i := Self.TratSettings.Useky.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   if (blk.GetSettings().navSid <> -1) then
    begin
     sMaster := blk;
     useky := sMaster.ssectUseky;
    end;
   blk.ssectMaster := sMaster;
   useky.Add(blk);
  end;

 /////////////////////////////////////////////////////////////////////////////
 // 3) inicializujeme navaznosti navestidel

 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.CreateNavRefs();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ResetTUs();
var i: Integer;
    blk: TBlkTU;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.RemoveTURefs();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CallChangeToTU();
var i: Integer;
    blk: TBlkTU;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.ChangeFromTrat();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace predpovidane soupravy na posledni usek trati
// volano pri uvolneni posledniho useku trati, nebo RBP

procedure TBlkTrat.UpdateTrainPredict(call_prediction: Boolean = true);
var Blk, last: TBlkTU;
    i: Integer;
begin
 if ((Self.Smer <> TTratSmer.AtoB) and (Self.Smer <> TTratSmer.BtoA)) then Exit();
 if (Self.TratSettings.Useky.Count = 0) then Exit();

 case (Self.Smer) of
  TTratSmer.AtoB: begin
       Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], TBlk(last));
       last.trainPredict := nil;
       if (last.IsTrain()) then Exit();
       for i := Self.TratSettings.Useky.Count-2 downto 0 do
        begin
         Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(Blk));
         if (Blk.IsTrain()) then
          begin
           last.trainPredict := Blk.train;
           break;
          end;
         if (Blk.trainPredict <> nil) then
          begin
           last.trainPredict := Blk.trainPredict;
           break;
          end;
         if ((Blk.navKryci <> nil) and (TBlkNav(Blk.navKryci).Navest = ncStuj)) then
          begin
           Blky.TrainPrediction(Self.navSudy);
           Exit();
          end;
        end;

       if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
         last.trainPredict := Self.trainPredict.train;
       if ((call_prediction) and (Self.navSudy <> nil)) then
         Blky.trainPrediction(Self.navSudy);
  end;

  TTratSmer.BtoA: begin
       Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(last));
       last.trainPredict := nil;
       if (last.IsTrain()) then Exit();
       for i := 1 to Self.TratSettings.Useky.Count-1 do
        begin
         Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(Blk));
         if (Blk.IsTrain()) then
          begin
           last.trainPredict := Blk.train;
           break;
          end;
         if (Blk.trainPredict <> nil) then
          begin
           last.trainPredict := Blk.trainPredict;
           break;
          end;
         if ((Blk.navKryci <> nil) and (TBlkNav(Blk.navKryci).Navest = ncStuj)) then
          begin
           Blky.TrainPrediction(Self.navLichy);
           Exit();
          end;
        end;

       if ((last.trainPredict = nil) and (Self.trainPredict <> nil)) then
         last.trainPredict := Self.trainPredict.train;
       if ((call_prediction) and (Self.navLichy <> nil)) then
         Blky.TrainPrediction(Self.navLichy);
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetReady(): Boolean;
var i: Integer;
    Blk: TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> btTU)) then Exit(false);
   if (not TBlkTU(Blk).ready) then Exit(false);
  end;
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci true prave tehdy, pokud je trat na obou koncich rizena stejnym uzivatelem.
// Mazerne nenavazujeme rizeni obou koncu na konkretniho uzivatele, napriklad
// kvuli staveni ze zasobniku.

function TBlkTrat.SameUserControlsBothUvazka(): Boolean;
var first, second: TORPanel;
begin
 if ((not Assigned(Self.uvazkaA)) or (not Assigned(Self.uvazkaB))) then Exit(false);
 if ((TBlkUvazka(Self.uvazkaA).OblsRizeni.Count <> 1) or (TBlkUvazka(Self.uvazkaB).OblsRizeni.Count <> 1)) then Exit(false);

 for first in TBlkUvazka(Self.uvazkaA).OblsRizeni[0].Connected do
   if (first.Rights >= TORControlRights.write) then
     for second in TBlkUvazka(Self.uvazkaB).OblsRizeni[0].Connected do
       if ((first.user = second.user) and (second.Rights >= TORControlRights.write)) then
         Exit(true);

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.ChangesTrainDir(): Boolean;
begin
 Result := (Assigned(Self.navLichy)) and (Assigned(Self.navSudy)) and
    (TBlkNav(Self.navLichy).Smer = TBlkNav(Self.navSudy).Smer);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetTrainUsek(train: TTrain): TBlk;
var usekid: Integer;
    blk: TBlk;
begin
 for usekid in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usekid, blk);
   if ((blk <> nil) and (blk.typ = btTU) and (TBlkUsek(blk).train = train)) then
     Exit(blk);
  end;

 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.mGetLastUsek(): TBlk;
begin
 Result := Self.GetLastUsek(Self.smer);
end;

function TBlkTrat.GetLastUsek(smer: TTratSmer): TBlk;
begin
 if (Self.TratSettings.Useky.Count < 1) then
   raise Exception.Create('Trať nemá žádný úsek!');

 if (smer = TTratSmer.AtoB) then
   Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], Result)
 else if (smer = TTratSmer.BtoA) then
   Blky.GetBlkByID(Self.TratSettings.Useky[0], Result)
 else
   raise Exception.Create('Trať nemá žádný směr!');
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetVyluka(): Boolean;
var blkUsek: TBlkUsek;
    usek: Integer;
begin
 for usek in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usek, TBlk(blkUsek));
   if (blkUsek <> nil) then
     if (blkUsek.Vyluka <> '') then
       Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.NavestProtismer(): Integer;
begin
 case (Self.navestidla) of
  TTratNavestidla.autoblok: Result := Integer(ncZhasnuto);
  TTratNavestidla.hradlo: Result := Integer(ncStuj);
 else
  Result := Integer(ncZhasnuto);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.HasAutoblokNav(blk: TBlk): Boolean;
var usekid: Integer;
    usek: TBlkTU;
begin
 for usekid in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usekid, TBlk(usek));
   if (usek.typ <> btTU) then continue;
   if ((blk = usek.navKryciL) or (blk = usek.navKryciS)) then
     Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.GetPtData(json: TJsonObject; includeState: Boolean);
var usekid: Integer;
begin
 inherited;

 json['harnessA'] := Self.TratSettings.uvazkaA;
 json['harnessB'] := Self.TratSettings.uvazkaB;
 json['zabzar'] := Integer(Self.TratSettings.zabzar);
 json['signals'] := Integer(Self.TratSettings.navestidla);
 for usekid in Self.TratSettings.Useky do
   json.A['tracks'].Add(usekid);

 if (includeState) then
   Self.GetPtState(json['blockState']);
end;

procedure TBlkTrat.GetPtState(json: TJsonObject);
var train: TBlkTraTTrain;
begin
 json['lock'] := Self.TratStav.zaver;
 json['direction'] := Integer(Self.TratStav.smer);
 json['request'] := Self.TratStav.zadost;

 for train in Self.TratStav.trains do
   if (not train.predict) then
     json.A['trains'].Add(train.train.name);
 if (Self.TratStav.trainPredict <> nil) then
   json['trainPredict'] := Self.TratStav.trainPredict.train;
 json['BP'] := Self.TratStav.BP;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// TBlkTraTTrain

constructor TBlkTraTTrain.Create(train: Integer);
begin
 inherited Create();
 Self.traini := train;
 Self.mTimeDefined := false;
 Self.predict := false;
end;

constructor TBlkTraTTrain.Create(train: Integer; time: TTime; predict: Boolean = false);
begin
 inherited Create();
 Self.traini := train;
 Self.time := time;
 Self.predict := predict;
end;

function TBlkTraTTrain.GetTime(): TTime;
begin
 if (not Self.IsTimeDefined()) then
   raise TBlkTratETimeNotDefined.Create('Time not defined!');
 Result := Self.mTime;
end;

procedure TBlkTraTTrain.SetTime(time: TTime);
begin
 Self.mTimeDefined := true;
 Self.mTime := time;
end;

function TBlkTraTTrain.IsTimeDefined(): Boolean;
begin
 Result := Self.mTimeDefined;
end;

procedure TBlkTraTTrain.UndefTime();
begin
 Self.mTimeDefined := false;
end;

function TBlkTraTTrain.SerializeForPanel(trat: TBlk; trainPredict: Boolean = false): string;
var addr, usek: Integer;
    porucha_bp: Boolean;
    blk: TBlk;
    stopsInHalt: Boolean;
begin
 // Pozor, souprava muze byt ve vice usecich a mit poruchu BP jen v jednom z nich
 porucha_bp := false;
 for usek in TBlkTrat(trat).GetSettings().Useky do
  begin
   Blky.GetBlkByID(usek, blk);
   if ((blk <> nil) and (blk.typ = btTU)) then
     if (TBlkUsek(blk).train = Self.train) and (TBlkTU(blk).poruchaBP) then
       porucha_bp := true;
  end;

 blk := Self.train.front as TBlk;
 stopsInHalt := ((blk <> nil) and (blk.typ = btTU) and (TBlkTU(blk).TUStav.zast_stopped));

 Result := Self.train.name + '|';
 if (trainPredict) then
   Result := Result + ownConvert.ColorToStr(clYellow) + '|'
 else if (porucha_bp) then
   Result := Result + ownConvert.ColorToStr(clAqua) + '|'
 else if ((Self.train.speed = 0) and (not stopsInHalt)) then
   Result := Result + ownConvert.ColorToStr(clRed) + '|'
 else
   Result := Result + ownConvert.ColorToStr(clWhite) + '|';

 if (Self.mTimeDefined) then
   Result := Result + FormatDateTime('nn', Self.mTime);
 Result := Result + '|';
 if (Self.predict) then
   Result := Result + ownConvert.ColorToStr(clYellow) + '|'
 else
   Result := Result + ownConvert.ColorToStr(clAqua) + '|';

 for addr in Self.train.HVs do
   Result := Result + HVDb[addr].name + '|';
end;

function TBlkTraTTrain.GeTTrain(): TTrain;
begin
 if (Self.traini = -1) then
   Exit(nil);
 Result := Trains[Self.trainI];
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

end.//unit

