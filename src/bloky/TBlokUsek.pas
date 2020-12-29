unit TBlokUsek;

//definice a obsluha technologickeho bloku Usek

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, Booster, houkEvent,
     IdContext, Generics.Collections, JsonDataObjects, TOblRizeni, Train,
     stanicniHlaseni, changeEvent, predvidanyOdjezd, TechnologieRCS;

type
 TUsekStav  = (disabled = -5, none = -1, uvolneno = 0, obsazeno = 1);

 ETrainFull = class(Exception);
 ETrainNotExists = class(Exception);
 EMultipleTrains = class(Exception);
 EDuplicitTrains = class(Exception);
 ERunningTrain   = class(Exception);

 //technologicka nastaveni useku (delka, RCS moduly, ...)
 TBlkUsekSettings = record
  RCSAddrs: TRCSAddrs;
  Lenght: double;          //delka useku v metrech
  SmcUsek: Boolean;        //specialni pripad: usek ve smycce
  Zesil: string;           //id zesilovace
  houkEvL: TObjectList<THoukEv>;  //seznam houkacich udalosti pro lichy smer
  houkEvS: TObjectList<THoukEv>;  //seznam houkacich udalosti pro sudy smer
  maxTrains: Cardinal;        // maximalni pocet souprav v bloku
 end;

 TSekceStav = TList<TUsekStav>;

 //aktualni stav useku (obsazeno, ...)
 TBlkUsekStav = record
  stav, stavOld: TUsekStav;  // hlavni stav useku a jeho predchozi hodnota (pouzivano v Update())
  sekce: TSekceStav;         // obsazenost jednotlivych casti useku
  zaver: TZaver;             // zaver na bloku
  NUZ: Boolean;              // nouzove uvolneni zaveru
  konecJC: TZaver;           // jaka jizdni cesta (vlakova, posunova, nouzova) konci na tomto bloku - vyuzivano pro zobrazeni
  stit, vyl: string;         // stitek a vyluka
  signalJCRef: TList<TBlk>;  // navestidla, ze kterych je z tohoto bloku postavena JC
  trainPredict: Integer;     // souprava, ktera je na tomto bloku predpovidana
  zkrat: TBoosterSignal;     // zkrat zesilovace
  napajeni: TBoosterSignal;  // napajeni zesilovace
  DCC: Boolean;              // stav DCC na useku: kdyz je kontrola na SPAXu, beru SPAX, jinak se bere stav z centraly
  stanicni_kolej: Boolean;   // pokud je blok stanicni koleji, je zde true, jinak false
  cislo_koleje: string;      // cislo koleje, pokud je stanicni
  train_pos: Boolean;          // jestli je v panelu alespon jeden specialni symbol pro cislo koleje
  vlakPresun: Integer;       // index soupravy, ktera se presouva, v ramci lokalniho seznamu souprav na useku; zadny presun = -1

  zpomalovani_ready: Boolean;          // pri predani soupravy do tohoto useku z trati, ci z jizdni cesty, je tento flag nastaven na true
                                      // to znamena, ze je souprava pripravena ke zpomalovani; navetidlo umozni zpomaleni jen, pokud je tento flag na true
                                      // po zpomaleni si navestidlo flag zrusi

  train_vypadek: Boolean;      // ztrata soupravy na useku se pozna tak, ze blok po urcity cas obsahuje soupravu, ale neni obsazen
  train_vypadek_time: Integer; //    to je nutne pro predavani souprav mezi bloky v ramci JC (usek se uvolni, ale souprava se jeste nestihne predat
                            // pro reseni timeoutu jsou tyto 2 promenne
  zkratSenseTime: TDateTime; // cas, kdy ma dojit k detekci zkratu
  currentHoukEv: Integer;    // index aktualni houkaci udalosti
  neprofilJCcheck: TList<Integer>; // seznam id jizdnich cest, ktere na usek uvalily podminku neprofiloveho styku
  trains: TList<Integer>;        // seznam souprav na useku ve smeru L --> S
 end;

 TBlkUsek = class(TBlk)
  const
   //defaultni stav
   _def_usek_stav: TBlkUsekStav = (
    stav : disabled;
    stavOld : disabled;
    zaver : no;
    NUZ : false;
    konecJC : no;
    stit : '';
    vyl : '';
    trainPredict : -1;
    zkrat : TBoosterSignal.undef;
    napajeni : TBoosterSignal.undef;
    DCC : false;
    stanicni_kolej : false;
    cislo_koleje : '';
    vlakPresun: -1;
    zpomalovani_ready : false;
    currentHoukEv : -1;
   );

   _DEFAULT_MAX_TRAINS = 1;

  private
   UsekStav: TBlkUsekStav;
   last_zes_zkrat: TBoosterSignal;  //poziva se na pamatovani posledniho stavu zkratu zesilovace pri vypnuti DCC

    procedure SetUsekNUZ(nuz: Boolean);
    procedure SetUsekZaver(Zaver: TZaver);
    procedure SetUsekStit(stit: string);
    procedure mSetUsekVyl(vyl: string);
    function GetTrainPredict(): TTrain;
    procedure SetTrainPredict(train: TTrain);
    procedure SetKonecJC(konecjc: TZaver);
    procedure SetVlakPresun(presun: Integer);

    procedure SetNapajeni(state: TBoosterSignal);
    procedure SetZkrat(state: TBoosterSignal);
    procedure SetDCC(state: Boolean);

    procedure XVezmiVlakOk(Sender: TObject; Data: Pointer);
    procedure XVezmiVlakErr(Sender: TObject; Data: Pointer);

    procedure MenuNewLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuEditLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDeleteLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUVOLLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuXVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRegVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRUCLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuMAUSLokClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPRESUNLokClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
    procedure MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVLOZLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
    procedure MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PotvrDeleteLok(Sender: TIdContext; success: Boolean);
    procedure PotvrUvolLok(Sender: TIdContext; success: Boolean);
    procedure PotvrRegVezmiLok(Sender: TIdContext; success: Boolean);

    procedure MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure ORVylukaNull(Sender: TIdContext; success: Boolean);

    procedure LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
    procedure CheckHoukEv();

    procedure CheckPOdjChanged();

    function GetHoukList(): TList<THoukEv>;
    function GetHoukEvEnabled(): Boolean;
    procedure SetHoukEvEnabled(state: Boolean);

    function GetSHTrain(trainLocalIndex: Integer): TSHTrain;

    procedure NeprofilObsaz();

    function GeTTrainI(): Integer;
    function GeTTrain(): TTrain;
    function GeTTrainIL(): Integer;
    function GeTTrainL(): TTrain;
    function GeTTrainIS(): Integer;
    function GeTTrainS(): TTrain;

    procedure ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORControlRights; params: string);
    function CanTrainSpeedInsert(index: Integer): Boolean;
    function IsStujForTrain(train: TTrain): Boolean;
    function RealZesZkrat(): TBoosterSignal;
    function CanStandTrain(): Boolean;
    function CanBeNextVB(vbs: TList<TObject>; start: TBlk): Boolean;
    function CanBeKC(vbs: TList<TObject>; start: TBlk): Boolean;

    function GetNavL(): TBlk;
    function GetNavS(): TBlk;

  protected
   UsekSettings: TBlkUsekSettings;

    procedure MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject);
    function MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
    function PresunLok(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;

  public

    EventsOnObsaz: TChangeEvents;
    EventsOnUvol: TChangeEvents;
    EventsOnZaverReleaseOrAB: TChangeEvents;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;

    procedure Freeze(); override;
    procedure UnFreeze(); override;

    //----- usek own functions -----

    function GetSettings(): TBlkUsekSettings;
    procedure SetSettings(data: TBlkUsekSettings);

    procedure OnBoosterChange();

    procedure SetUsekVyl(Sender: TIDCOntext; vyl: string);

    procedure AddNeprofilJC(id: Integer);
    procedure RemoveNeprofilJC(id: Integer);
    function IsNeprofilJC(): Boolean;

    function IsTrain(): Boolean; overload;
    function IsTrain(index: Integer): Boolean; overload;
    function IsTrain(train: TTrain): Boolean; overload;
    procedure AddTrainL(index: Integer); overload; virtual;
    procedure AddTrainL(train: TTrain); overload; virtual;
    procedure AddTrainS(index: Integer); overload; virtual;
    procedure AddTrainS(train: TTrain); overload; virtual;
    procedure AddTrain(localTrainIndex: Integer; train: Integer); overload;
    procedure AddTrain(localTrainIndex: Integer; train: TTrain); overload;
    procedure RemoveTrains(); virtual;
    procedure RemoveTrain(index: Integer); overload; virtual;
    procedure RemoveTrain(train: TTrain); overload; virtual;
    function TrainsFull(): Boolean;

    function IsVlakPresun(): Boolean;
    procedure ClearPOdj();
    procedure PropagatePOdjToTrat();

    procedure MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);

    property Stav: TBlkUsekStav read UsekStav;

    property obsazeno: TUsekStav read UsekStav.Stav;
    property NUZ: Boolean read UsekStav.NUZ write SetUsekNUZ;
    property zaver: TZaver read UsekStav.Zaver write SetUsekZaver;
    property stitek: string read UsekStav.Stit write SetUsekStit;
    property vyluka: string read UsekStav.Vyl write mSetUsekVyl;
    property trainPredict: TTrain read GetTrainPredict write SetTrainPredict;
    property konecJC: TZaver read UsekStav.KonecJC write SetKonecJC;
    property signalJCRef: TList<TBlk> read UsekStav.signalJCRef write UsekStav.signalJCRef;
    property sekceStav: TList<TUsekStav> read UsekStav.sekce;
    property navL: TBlk read GetNavL;  // warning: slow getter!
    property navS: TBLk read GetNavS;  // warning: slow getter!

    property trainI: Integer read GeTTrainI;
    property train: TTrain read GeTTrain;
    property trainIL: Integer read GeTTrainIL;
    property trainL: TTrain read GeTTrainL;
    property trainIS: Integer read GeTTrainIS;
    property trainSudy: TTrain read GeTTrainS;
    property trains: TList<Integer> read UsekStav.trains;

    property zkrat: TBoosterSignal read UsekStav.Zkrat write SetZkrat;
    property napajeni: TBoosterSignal read UsekStav.Napajeni write SetNapajeni;
    property DCC: Boolean read UsekStav.DCC write SetDCC;

    property vlakPresun: Integer read UsekStav.vlakPresun write SetVlakPresun;
    property zpomalovani_ready: Boolean read UsekStav.zpomalovani_ready write UsekStav.zpomalovani_ready;
    property houk_ev_enabled: Boolean read GetHoukEvEnabled write SetHoukEvEnabled;

    //GUI:

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TORCOntrolRights; params: string = ''); override;
    procedure POdjChanged(trainId: Integer; var podj: TPOdj);
    function GetTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer): string;
    function PanelStateString(): string; override;

    //PT:

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TBloky, TBlokNav, Logging, RCS, ownStrUtils, Diagnostics,
    TJCDatabase, fMain, TCPServerOR, TBlokTrat, TrainDb, THVDatabase, Math,
    Trakce, THnaciVozidlo, TBlokTratUsek, BoosterDb, appEv,
    stanicniHlaseniHelper, TechnologieJC, PTUtils, RegulatorTCP, TCPORsRef,
    Graphics, ownConvert, TechnologieTrakce, TMultiJCDatabase;

constructor TBlkUsek.Create(index: Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := btUsek;
 Self.UsekStav := _def_usek_stav;

 Self.EventsOnObsaz := TChangeEvents.Create();
 Self.EventsOnUvol  := TChangeEvents.Create();
 Self.EventsOnZaverReleaseOrAB := TChangeEvents.Create();

 Self.UsekSettings.houkEvL := TObjectList<THoukEv>.Create();
 Self.UsekSettings.houkEvS := TObjectList<THoukEv>.Create();

 Self.UsekSettings.maxTrains := _DEFAULT_MAX_TRAINS;

 Self.UsekStav.neprofilJCcheck := TList<Integer>.Create();
 Self.UsekStav.trains := TList<Integer>.Create();
 Self.UsekStav.signalJCRef := TList<TBlk>.Create();
 Self.UsekStav.sekce := TList<TUsekStav>.Create();
end;//ctor

destructor TBlkUsek.Destroy();
begin
 if (Assigned(Self.UsekSettings.houkEvL)) then
   Self.UsekSettings.houkEvL.Free();

 if (Assigned(Self.UsekSettings.houkEvS)) then
   Self.UsekSettings.houkEvS.Free();

 Self.EventsOnObsaz.Free();
 Self.EventsOnUvol.Free();
 Self.EventsOnZaverReleaseOrAB.Free();

 Self.UsekStav.neprofilJCcheck.Free();
 Self.UsekStav.trains.Free();
 Self.UsekStav.signalJCRef.Free();
 Self.UsekStav.sekce.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var strs: TStrings;
    s: string;
    trainIndex: Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.UsekSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
 Self.UsekSettings.Lenght := ini_tech.ReadFloat(section,'delka',0);
 Self.UsekSettings.Zesil := ini_tech.ReadString(section,'zesil','');
 Self.UsekSettings.SmcUsek := ini_tech.ReadBool(section, 'smc', false);
 Self.UsekSettings.maxTrains := ini_tech.ReadInteger(section, 'maxSpr', _DEFAULT_MAX_TRAINS);

 if (Boosters[Self.UsekSettings.Zesil] = nil) then
   writelog('WARNING: Blok '+Self.name + ' ('+IntToStr(Self.id)+
            ') nemá návaznost na validní zesilovač', WR_ERROR);

 Self.UsekStav.Stit := ini_stat.ReadString(section, 'stit', '');
 Self.UsekStav.Vyl := ini_stat.ReadString(section, 'vyl' , '');

 strs := TStringList.Create();
 try
   Self.UsekStav.trains.Clear();
   ExtractStringsEx([','], [], ini_stat.ReadString(section, 'spr' , ''), strs);
   for s in strs do
    begin
     trainIndex := TrainDb.Trains.GetTrainIndexByName(s);
     if (trainIndex > -1) then
       Self.UsekStav.trains.Add(trainIndex)
     else
       writelog('WARNING: souprava '+s+' na bloku '+Self.name+' neexistuje, mažu soupravu', WR_DATA);
    end;

   // houkaci udalosti
   try
     Self.LoadHoukEventToList(Self.UsekSettings.houkEvL, ini_tech, section, 'houkL');
   except
     writelog('Nepodařilo se načíst houkací události L bloku ' + Self.name, WR_ERROR);
   end;

   try
     Self.LoadHoukEventToList(Self.UsekSettings.houkEvS, ini_tech, section, 'houkS');
   except
     writelog('Nepodařilo se načíst houkací události S bloku ' + Self.name, WR_ERROR);
   end;
 finally
   strs.Free();
 end;

 strs := Self.LoadORs(ini_rel, 'U');
 try
   if (strs.Count >= 2) then
    begin
     Self.UsekStav.stanicni_kolej := (strs[1] = '1');
     if (strs.Count >= 3) then
       Self.UsekStav.cislo_koleje := strs[2]
     else
       Self.UsekStav.cislo_koleje := '';
    end;

   if (strs.Count >= 4) then
     Self.UsekStav.train_pos := (strs[3] = '1');

   if ((not Self.UsekStav.stanicni_kolej) and (Self.UsekSettings.maxTrains <> 1)) then
     Self.UsekSettings.maxTrains := 1;
 finally
   strs.Free();
 end;

 PushRCSToOR(Self.ORsRef, Self.UsekSettings.RCSAddrs);
end;

procedure TBlkUsek.SaveData(ini_tech: TMemIniFile; const section: string);
var i: Integer;
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.UsekSettings.RCSAddrs);
 ini_tech.WriteFloat(section,'delka', Self.UsekSettings.Lenght);
 ini_tech.WriteString(section,'zesil', Self.UsekSettings.Zesil);

 if (Self.UsekSettings.maxTrains <> _DEFAULT_MAX_TRAINS) then
   ini_tech.WriteInteger(section, 'maxSpr', Self.UsekSettings.maxTrains);

 if (Self.UsekSettings.SmcUsek) then
   ini_tech.WriteBool(section, 'smc', Self.UsekSettings.SmcUsek);

 if (Assigned(Self.UsekSettings.houkEvL)) then
  begin
   for i := 0 to Self.UsekSettings.houkEvL.Count-1 do
    begin
     try
       ini_tech.WriteString(section, 'houkL'+IntToStr(i), Self.UsekSettings.houkEvL[i].GetDefString());
     except
       on E: Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
     end;
    end;
  end;

 if (Assigned(Self.UsekSettings.houkEvS)) then
  begin
   for i := 0 to Self.UsekSettings.houkEvS.Count-1 do
    begin
     try
       ini_tech.WriteString(section, 'houkS'+IntToStr(i), Self.UsekSettings.houkEvS[i].GetDefString());
     except
       on E: Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.name);
     end;
    end;
  end;
end;

procedure TBlkUsek.SaveStatus(ini_stat: TMemIniFile; const section: string);
var str: string;
    traini: Integer;
begin
 if (Self.UsekStav.Stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.UsekStav.Stit);

 if (Self.UsekStav.Vyl <> '') then
   ini_stat.WriteString(section, 'vyl' , Self.UsekStav.Vyl);

 if (Self.IsTrain()) then
  begin
   str := '';
   for traini in Self.trains do
     str := str + TrainDb.Trains[traini].name + ',';
   ini_stat.WriteString(section, 'spr' , str);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.Enable();
var rcsaddr: TRCSAddr;
    enable: Boolean;
begin
 enable := true;
 try
   for rcsaddr in Self.UsekSettings.RCSAddrs do
     if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
       enable := false;
 except
   enable := false;
 end;

 if (enable) then
  begin
   Self.UsekStav.Stav := none;
   Self.UsekStav.StavOld := none;
  end;

 Self.OnBoosterChange();
 Self.UsekStav.sekce.Clear();
 Self.UsekStav.neprofilJCcheck.Clear();

 Self.Update();
 //change event will be called in Update();
end;

procedure TBlkUsek.Disable();
var oblr: TOR;
begin
 inherited;

 if (Self.UsekStav.zkrat = TBoosterSignal.error) then
   for oblr in Self.ORsRef do
     oblr.ZKratBlkCnt := oblr.ZKratBlkCnt - 1;

 Self.UsekStav.Stav       := disabled;
 Self.UsekStav.StavOld    := disabled;
 Self.UsekStav.NUZ        := false;
 Self.UsekStav.TrainPredict := -1;
 Self.UsekStav.KonecJC    := TZaver.no;
 Self.UsekStav.zpomalovani_ready := false;
 Self.houk_ev_enabled     := false;
 Self.UsekStav.zkrat      := TBoosterSignal.undef;
 Self.UsekStav.napajeni   := TBoosterSignal.undef;
 Self.UsekStav.DCC        := false;
 Self.UsekStav.sekce.Clear();

 Self.UsekStav.neprofilJCcheck.Clear();

 Self.Change(true);
end;

procedure TBlkUsek.Reset();
begin
 Self.EventsOnObsaz.Clear();
 Self.EventsOnUvol.Clear();
 Self.EventsOnZaverReleaseOrAB.Clear();
 Self.Zaver := TZaver.no;
end;

function TBlkUsek.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.input) and (Self.UsekSettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkUsek.Update();
var i, train: Integer;
    state: TRCSInputState;
    usekStav: TUsekStav;
    oblr: TOR;
begin
 inherited Update();

 if (((Self.zkrat = TBoosterSignal.error) or (Self.napajeni = TBoosterSignal.error)) and (not Self.frozen)) then
  begin
   Self.Freeze();
   Exit();
  end;

 if (Self.frozen) then
  begin
   if ((Self.zkrat <> TBoosterSignal.error) and (Self.RealZesZkrat() = TBoosterSignal.error) and
       (Self.napajeni = TBoosterSignal.ok) and (Now > Self.Stav.zkratSenseTime) and (Self.DCC)) then
     Self.zkrat := TBoosterSignal.error;

   if (((not Self.DCC) or (Self.napajeni = TBoosterSignal.error)
        or (Now < Self.Stav.zkratSenseTime))
       and (Self.zkrat = TBoosterSignal.error)) then
     Self.zkrat := TBoosterSignal.ok;

   if ((Self.DCC) and (Self.zkrat <> TBoosterSignal.error) and (Self.napajeni <> TBoosterSignal.error) and
       (Now > Self.Stav.zkratSenseTime)) then
     Self.UnFreeze();
   Exit();
  end;

 if (Self.UsekStav.sekce.Count <> Self.UsekSettings.RCSAddrs.Count) then
  begin
   Self.UsekStav.sekce.Clear();
   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
     Self.UsekStav.sekce.Add(TUsekStav.none);
  end;

 Self.UsekStav.Stav := uvolneno; // must be here to update booster state

 for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
  begin
   try
     state := RCSi.GetInput(Self.UsekSettings.RCSAddrs[i]);
   except
     state := failure;
   end;

   case (state) of
    isOn  : Self.UsekStav.sekce[i] := TUsekStav.obsazeno;
    isOff : Self.UsekStav.sekce[i] := TUsekStav.uvolneno;
    failure, notYetScanned, unavailableModule, unavailablePort: begin
        Self.UsekStav.sekce[i] := TUsekStav.disabled;
        Self.UsekStav.Stav := TUsekStav.disabled;
      end;
   end;
  end;

 if (Self.UsekStav.Stav <> TUsekStav.disabled) then
   for usekStav in Self.UsekStav.sekce do
     if (usekStav = TUsekStav.obsazeno) then
        Self.UsekStav.Stav := TUsekStav.obsazeno;

 // reseni vypadku soupravy
 // pad soupravy z bloku az po urcitem case - aby se jizdni ceste nechal cas na zpracovani pohybu soupravy
 if (Self.UsekStav.train_vypadek) then
  begin
   if (not Self.IsTrain()) then
    begin
     Self.UsekStav.train_vypadek := false;
     Exit();
    end;

   Inc(Self.UsekStav.train_vypadek_time);
   if (Self.UsekStav.train_vypadek_time > 3) then
    begin
     Self.UsekStav.train_vypadek := false;

     // informace o vypadku soupravy probiha jen ve stanicnich kolejich a v trati
     if ((Self.typ = btTU) or (Self.UsekStav.stanicni_kolej)) then
       for oblr in Self.stations do
         oblr.BlkWriteError(Self, 'Ztráta soupravy v úseku '+Self.name, 'TECHNOLOGIE');
     if (Self.UsekStav.Zaver <> TZaver.no) then Self.UsekStav.Zaver := TZaver.nouz;
    end;//if train_vypadek_time > 3
  end;//if train_vypadek

 // OnChange
 if (Self.UsekStav.Stav <> Self.UsekStav.StavOld) then
  begin
   if (Self.UsekStav.Stav = TUsekStav.disabled) then
    begin
     Self.UsekStav.Stav := disabled;
     Self.UsekStav.StavOld := Self.UsekStav.Stav;
     JCDb.RusJC(Self);

     // zastavime soupravy na useku
     for train in Self.trains do
       TrainDb.Trains[train].speed := 0;

     Self.Change(true);
    end;

   if (Self.UsekStav.StavOld = TUsekStav.disabled) then
    begin
     // Wake-up from disabled
     Self.OnBoosterChange();
    end;

   // kontrola udalosti obsazeni
   if (Self.UsekStav.Stav = TUsekStav.obsazeno) then begin
     Self.NeprofilObsaz();
     Self.CallChangeEvents(Self.EventsOnObsaz);
   end else if (Self.UsekStav.Stav = TUsekStav.uvolneno) then
     Self.CallChangeEvents(Self.EventsOnUvol);

   if (Self.IsTrain()) then
    begin
     // souprava
     if ((Self.UsekStav.Stav = TUsekStav.uvolneno) and (Self.UsekStav.StavOld = TUsekStav.obsazeno)) then
      begin
       Self.UsekStav.train_vypadek      := true;
       Self.UsekStav.train_vypadek_time := 0;
      end;
    end;//if Train <> -1

   Self.UsekStav.StavOld := Self.UsekStav.Stav;
   Self.Change();
  end;

 // reseni zruseni PRESUN soupravy, ktera jede
 if ((Self.IsVlakPresun()) and ((not Self.IsTrain(Self.trains[Self.VlakPresun])) or
     (TrainDb.Trains[Self.trains[Self.VlakPresun]].wantedSpeed > 0))) then
   Self.VlakPresun := -1;

 // pousteni houkani na houkaci udalosti
 if (Self.UsekStav.currentHoukEv > -1) then
   Self.CheckHoukEv();

 // kontrola zmeny barev vlivem uplynuti casu predvidaneho odjezdu
 Self.CheckPOdjChanged();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.SetUsekNUZ(nuz: Boolean);
var oblr: TOR;
begin
 if (Self.UsekStav.NUZ = nuz) then Exit(); 

 if (Self.UsekStav.NUZ) and (not nuz) then
  begin
   for oblr in Self.ORsRef do
     if (oblr.NUZblkCnt > 0) then
       oblr.NUZblkCnt := oblr.NUZblkCnt - 1;
  end else begin
    if ((not Self.UsekStav.NUZ) and (nuz)) then
      for oblr in Self.ORsRef do
        oblr.NUZblkCnt := oblr.NUZblkCnt + 1;
  end;

 Self.UsekStav.NUZ := nuz;
 Self.Change();
end;

procedure TBlkUsek.SetUsekZaver(Zaver: TZaver);
var old: TZaver;
begin
 if (Zaver = Self.Zaver) then Exit();

 if ((Self.UsekStav.Zaver > TZaver.no) and ((Zaver = TZaver.no) or (Zaver = TZaver.ab))) then
   Self.NUZ := false;

 old := Self.Zaver;
 Self.UsekStav.Zaver := Zaver;
 Self.UsekStav.TrainPredict := -1;

 if ((old > TZaver.no) and (zaver = TZaver.no)) then
   Self.CallChangeEvents(Self.EventsOnZaverReleaseOrAb)
 else if ((old <> TZaver.no) and (old <> TZaver.ab) and (zaver = TZaver.ab)) then
   Self.CallChangeEvents(Self.EventsOnZaverReleaseOrAb);

 // staveci zavery se do panelu neposilaji, protoze jsou mi k nicemu
 if ((Self.Zaver <> TZaver.staveni) or (old <> TZaver.no) or (diag.showZaver)) then
   Self.Change();
end;

procedure TBlkUsek.SetUsekStit(stit: string);
begin
 Self.UsekStav.Stit := stit;
 Self.Change();
end;

procedure TBlkUsek.mSetUsekVyl(vyl: string);
begin
 Self.UsekStav.Vyl := vyl;
 Self.Change();
end;

procedure TBlkUsek.ORVylukaNull(Sender: TIdContext; success: Boolean);
begin
 if (success) then
  Self.Vyluka := '';
end;

procedure TBlkUsek.SetUsekVyl(Sender: TIDCOntext; vyl: string);
begin
 if ((self.UsekStav.Vyl <> '') and (vyl = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil);
  end else begin
   Self.Vyluka := vyl;
  end;
end;

function TBlkUsek.GetTrainPredict(): TTrain;
begin
 if (Self.UsekStav.TrainPredict = -1) then
   Exit(nil);
 Result := TrainDb.Trains[Self.UsekStav.TrainPredict];
end;

procedure TBlkUsek.SetTrainPredict(train: TTrain);
var old: Integer;
begin
 old := Self.UsekStav.TrainPredict;
 if (train = nil) then
   Self.UsekStav.TrainPredict := -1
 else
   Self.UsekStav.TrainPredict := train.index;

 if ((train = nil) and (old > -1)) then
  begin
   // odstranit predvidany odjezd mazane predpovidane soupravy
   if (TrainDb.Trains[old].IsPOdj(Self)) then
     TrainDb.Trains[old].RemovePOdj(Self);
  end;

 Self.Change();
end;

procedure TBlkUsek.SetKonecJC(konecjc: TZaver);
begin
 Self.UsekStav.KonecJC := konecjc;
 Self.Change();
end;

procedure TBlkUsek.OnBoosterChange();
begin
 if (Boosters.ContainsKey(Self.UsekSettings.Zesil)) then
  begin
   Self.DCC := (Boosters[Self.UsekSettings.Zesil].DCC = TBoosterSignal.ok);
   Self.napajeni := Boosters[Self.UsekSettings.Zesil].napajeni;
   Self.zkrat := Boosters[Self.UsekSettings.Zesil].zkrat;
  end else begin
   Self.DCC := (TrakceI.TrackStatusSafe() = TTrkStatus.tsOn);
   Self.napajeni := TBoosterSignal.undef;
   Self.zkrat := TBoosterSignal.undef;
  end;
end;

procedure TBlkUsek.SetZkrat(state: TBoosterSignal);
var oblr: TOR;
begin
 if (Self.frozen) then
   Self.last_zes_zkrat := state;

 if (Self.UsekStav.zkrat = state) then
   Exit();

 if ((state = TBoosterSignal.error) and (not Self.frozen) and
     ((not Self.DCC) or (Self.napajeni = TBoosterSignal.error) or
      (Now < Self.Stav.zkratSenseTime))) then
  begin
   if (Self.zkrat <> TBoosterSignal.ok) then
    begin
     Self.UsekStav.zkrat := TBoosterSignal.ok;
     Self.Change();
    end;
   Self.Freeze();
   Exit();
  end;

 if (state = TBoosterSignal.error) then
  begin
   // do OR oznamime, ze nastal zkrat, pak se prehraje zvuk v klientech...
   for oblr in Self.ORsRef do
    oblr.ZKratBlkCnt := oblr.ZkratBlkCnt + 1;
  end else begin
   if (Self.UsekStav.zkrat = TBoosterSignal.error) then
     for oblr in Self.ORsRef do
       oblr.ZKratBlkCnt := oblr.ZkratBlkCnt - 1;
  end;

 Self.UsekStav.zkrat := state;
 Self.Change();
end;

procedure TBlkUsek.SetNapajeni(state: TBoosterSignal);
begin
 if (Self.UsekStav.napajeni = state) then
   Exit();

 Self.UsekStav.napajeni := state;

 if (state = TBoosterSignal.ok) then
   Self.UsekStav.zkratSenseTime := Now+EncodeTime(0, 0, 1, 0);

 Self.Change();
end;

procedure TBlkUsek.SetDCC(state: Boolean);
begin
 if (state = Self.Stav.DCC) then
   Exit();

 // doslo ke zmene DCC
 Self.UsekStav.DCC := state;
 if (state) then
   Self.UsekStav.zkratSenseTime := Now+EncodeTime(0, 0, 1, 0)
 else
   Self.Freeze();

 Self.Change();
end;

procedure TBlkUsek.SetVlakPresun(presun: Integer);
begin
 if (Self.UsekStav.vlakPresun <> presun) then
  begin
   Self.UsekStav.vlakPresun := presun;
   Self.Change();
  end;
end;

procedure TBlkUsek.Freeze();
begin
 if (Self.frozen) then Exit();

 inherited;
 Self.last_zes_zkrat := Self.zkrat;
end;

procedure TBlkUsek.UnFreeze();
begin
 if (not Self.frozen) then Exit();

 inherited;
 if (Self.zkrat <> Self.last_zes_zkrat) then
  begin
   Self.zkrat := Self.last_zes_zkrat;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSettings(): TBlkUsekSettings;
begin
 Result := Self.UsekSettings;
end;

procedure TBlkUsek.SetSettings(data: TBlkUsekSettings);
begin
 if (Self.UsekSettings.houkEvL <> data.houkEvL) then
   Self.UsekSettings.houkEvL.Free();

 if (Self.UsekSettings.houkEvS <> data.houkEvS) then
   Self.UsekSettings.houkEvS.Free();

 if (Self.UsekSettings.RCSAddrs <> data.RCSAddrs) then
   Self.UsekSettings.RCSAddrs.Free();

 Self.UsekSettings := data;

 if (not Self.UsekStav.stanicni_kolej) then
   Self.UsekSettings.maxTrains := 1;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//dynamicke funkce:

procedure TBlkUsek.MenuNewLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
 // nejdrive posleme aktualni seznam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkNewTrain(Self, SenderPnl, (itemindex-2) div 2);
end;

procedure TBlkUsek.MenuVLOZLokClick(SenderPnl: TIdContext; SenderOR: TObject; itemindex: Integer);
begin
 Self.PresunLok(SenderPnl, SenderOR, (itemindex-2) div 2);
end;

procedure TBlkUsek.MenuEditLokClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 // nejdrive posleme aktualni senam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkEditTrain(Self, SenderPnl, TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]]);
end;

procedure TBlkUsek.MenuDeleteLokClick(SenderPnl: TIdContext; SenderOR: TObject);
var podm: TPSPodminky;
    blk: TObject;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 podm := TPSPodminky.Create();
 for blk in Blky.GetBlkWithTrain(TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]]) do
   podm.Add(TOR.GetPSPodminka(blk, 'Smazání soupravy z úseku'));
 ORTCPServer.Potvr(SenderPnl, Self.PotvrDeleteLok, SenderOR as TOR,
   'Smazání soupravy '+TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].name,
   TBlky.GetBlksList(Self), podm);
end;

procedure TBlkUsek.PotvrDeleteLok(Sender: TIdContext; success: Boolean);
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (success) then
  begin
   if (Self.UsekStav.vlakPresun = TTCPORsRef(Sender.Data).train_menu_index) then
     Self.UsekStav.VlakPresun := -1;
   TrainDb.Trains.Remove(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkUsek.PotvrUvolLok(Sender: TIdContext; success: Boolean);
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (not success) then Exit();

 if (Blky.GetBlkWithTrain(TrainDb.Trains[Self.trains[TTCPORsRef(Sender.Data).train_menu_index]]).Count = 1) then
  begin
   TrainDb.Trains.Remove(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava odstraněna');
  end else begin
   Self.RemoveTrain(Self.trains[TTCPORsRef(Sender.Data).train_menu_index]);
  end;
end;

procedure TBlkUsek.MenuUVOLLokClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 ORTCPServer.Potvr(SenderPnl, Self.PotvrUvolLok, SenderOR as TOR,
  'Uvolnění soupravy '+TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].name+' z bloku',
  TBlky.GetBlksList(Self), nil);
end;

procedure TBlkUsek.MenuVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];

 if (train.stolen) then
  begin
   // Prevzit soupravu, ktera byla ukradena.
   Self.MenuXVEZMILokClick(SenderPnl, SenderOR);
  end else begin
   if (train.IsAnyLokoInRegulator()) then
    begin
     // Nasilim prevzit lokomotivy z regulatoru.
     Self.MenuRegVEZMILokClick(SenderPnl, SenderOR);
    end;
  end;
end;

procedure TBlkUsek.XVezmiVlakOk(Sender: TObject; Data: Pointer);
begin
 ORTCPServer.SendInfoMsg(TIdContext(Data), 'Vlak převzat');
end;

procedure TBlkUsek.XVezmiVlakErr(Sender: TObject; Data: Pointer);
begin
 ORTCPServer.BottomError(TIdContext(Data), 'Vlak se nepodařilo převzít', '', 'TECHNOLOGIE');
end;

procedure TBlkUsek.MenuXVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];
 train.Acquire(TTrakce.Callback(Self.XVezmiVlakOk, SenderPnl), TTrakce.Callback(Self.XVezmiVlakErr, SenderPnl));
end;

procedure TBlkUsek.MenuRegVEZMILokClick(SenderPnl: TIdContext; SenderOR: TObject);
var podm: TPSPodminky;
    train: TTrain;
    hvaddr: Integer;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();
 train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];

 podm := TPSPodminky.Create();
 for hvaddr in train.HVs do
   if (HVDb[hvaddr] <> nil) then
     podm.Add(TOR.GetPSPodminka(HVDb[hvaddr].NiceName(), 'Násilné převzetí řízení'));

 ORTCPServer.Potvr(SenderPnl, Self.PotvrRegVezmiLok, SenderOR as TOR,
  'Nouzové převzetí hnacích vozidel do automatického řízení',
  TBlky.GetBlksList(Self), podm);
end;

procedure TBlkUsek.PotvrRegVezmiLok(Sender: TIdContext; success: Boolean);
var train: TTrain;
begin
 if ((TTCPORsRef(Sender.Data).train_menu_index < 0) or
     (TTCPORsRef(Sender.Data).train_menu_index >= Self.trains.Count) or (not success)) then Exit();
 train := TrainDb.Trains[Self.trains[TTCPORsRef(Sender.Data).train_menu_index]];

 try
   train.ForceRemoveAllRegulators();
   ORTCPServer.SendInfoMsg(Sender, 'Vlak převzat');
 except
  on E: Exception do
    ORTCPServer.BottomError(Sender, 'Vlak se nepodařilo převzít', TOR(Sender).ShortName, 'TECHNOLOGIE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;

procedure TBlkUsek.MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.Stav.Vyl);
end;

// pokud volba nebyla uspesna, vraci false a v tom pripade je vyvolano menu
function TBlkUsek.MenuKCClick(SenderPnl: TIdContext; SenderOR: TObject): Boolean;
var signal: TBlkSignal;
begin
 if ((Self.UsekStav.KonecJC <> TZaver.no) and (not (SenderOR as TOR).vb.Contains(Self))) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit(true);
  end;

 if ((SenderOR as TOR).vb.Contains(Self)) then (SenderOR as TOR).vb.Remove(self);

 signal := Blky.GeTBlkSignalSelected((SenderOR as TOR).id) as TBlkSignal;
 if (signal = nil) then Exit(false);

 case (signal.selected) of
  TBlkSignalSelection.VC : Self.UsekStav.KonecJC := TZaver.vlak;
  TBlkSignalSelection.PC : Self.UsekStav.KonecJC := TZaver.posun;
  TBlkSignalSelection.NC, TBlkSignalSelection.PP
                   : Self.UsekStav.KonecJC := TZaver.nouz;
 end;//case

 JCDb.StavJC(signal, Self, SenderPnl, SenderOR, signal.beginAB);

 Self.Change();
 Result := true;
end;

procedure TBlkUsek.MenuVBClick(SenderPnl: TIdContext; SenderOR: TObject);
var Blk: TBlk;
begin
 if (Self.UsekStav.KonecJC <> TZaver.no) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit();
  end;

 Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk = nil) then Exit();

 if (not Self.CanBeNextVB((SenderOR as TOR).vb, blk)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Není variantním bodem žádné JC');
   Exit();
  end;

 case ((Blk as TBlkSignal).selected) of
  TBlkSignalSelection.VC : Self.UsekStav.KonecJC := TZaver.vlak;
  TBlkSignalSelection.PC : Self.UsekStav.KonecJC := TZaver.posun;
  TBlkSignalSelection.NC : Self.UsekStav.KonecJC := TZaver.nouz;
  TBlkSignalSelection.PP : Self.UsekStav.KonecJC := TZaver.nouz;
 end;

 (SenderOR as TOR).vb.Add(Self);

 Self.Change();
end;

procedure TBlkUsek.MenuNUZStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.NUZ := true;
end;

procedure TBlkUsek.MenuNUZStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.NUZ := false;
end;

procedure TBlkUsek.MenuPRESUNLokClick(SenderPnl: TIdContext; SenderOR: TObject; new_state: Boolean);
var Blk: TBlk;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 if (new_state) then
  begin
   if (TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].station <> SenderOR) then
    begin
     ORTCPServer.SendInfoMsg(SenderPnl, 'Loko se nenachází ve vaši oblasti řízení');
     Exit();
    end;

   Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
   if (Blk <> nil) then (Blk as TBlkUsek).VlakPresun := -1;
   Self.VlakPresun := TTCPORsRef(SenderPnl.Data).train_menu_index;
  end else begin
   Self.VlakPresun := -1;
  end;
end;

procedure TBlkUsek.MenuRUClokClick(SenderPnl: TIdContext; SenderOR: TObject);
var addr: Integer;
    str: string;
    HV: THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 str := (SenderOR as TOR).id + ';LOK-TOKEN;OK;';
 for addr in TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].HVs do
  begin
   HV := HVDb[addr];
   str := str + '[' + IntToStr(HV.addr) + '|' + HV.GetToken() + ']';
  end;//for i

 ORTCPServer.SendLn(SenderPnl, str);
end;

procedure TBlkUsek.MenuMAUSlokClick(SenderPnl: TIdContext; SenderOR: TObject);
var addr: Integer;
    str: string;
    HV: THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 str := (SenderOR as TOR).id + ';MAUS;{';
 for addr in TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]].HVs do
  begin
   HV := HVDb[addr];
   str := str + IntToStr(HV.addr) + '|';
  end;//for i
 str := str + '}';

 ORTCPServer.SendLn(SenderPnl, str);
end;

procedure TBlkUsek.MenuObsazClick(SenderPnl: TIdContext; SenderOR: TObject);
var rcsaddr: TRCSAddr;
begin
 try
   for rcsaddr in Self.UsekSettings.RCSAddrs do
     RCSi.SetInput(rcsaddr.board, rcsaddr.port, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkUsek.MenuUvolClick(SenderPnl: TIdContext; SenderOR: TObject);
var rcsaddr: TRCSAddr;
begin
 try
   for rcsaddr in Self.UsekSettings.RCSAddrs do
     RCSi.SetInput(rcsaddr.board, rcsaddr.port, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkUsek.MenuHLASENIOdjezdClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();
   TOR(SenderOR).hlaseni.Odjede(Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index));
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuHLASENIPrijezdClick(SenderPnl: TIdContext; SenderOR: TObject);
var shTrain: TSHTrain;
    blk: TBlkUsek;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();

   shTrain := Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]],
      TOR(SenderOR)).stanicniKolej;
   if (blk = nil) then Exit();

   shTrain.kolej := blk.Stav.cislo_koleje;
   TOR(SenderOR).hlaseni.Prijede(shTrain);
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuHLASENIPrujezdClick(SenderPnl: TIdContext; SenderOR: TObject);
var shTrain: TSHTrain;
    blk: TBlkUsek;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).train_menu_index >= Self.trains.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();

   shTrain := Self.GetSHTrain(TTCPORsRef(SenderPnl.Data).train_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]],
      TOR(SenderOR)).stanicniKolej;

   if (blk <> nil) then
     shTrain.kolej := blk.Stav.cislo_koleje;
   TOR(SenderOR).hlaseni.Projede(shTrain);
 except
   on E: Exception do
    begin
     writelog('Nepodařilo se spustit staniční hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodařilo se spustit staniční hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuPOdjClick(SenderPnl: TIdContext; SenderOR: TObject);
var train: TTrain;
begin
 if ((TTCPORsRef(SenderPnl.Data).train_menu_index >= 0) and
     (TTCPORsRef(SenderPnl.Data).train_menu_index < Self.trains.Count)) then
  begin
   train := TrainDb.Trains[Self.trains[TTCPORsRef(SenderPnl.Data).train_menu_index]];
  end else begin
   if (Self.TrainPredict = nil) then Exit();
   train := Self.TrainPredict;
  end;

 if (train.IsPOdj(Self)) then
   ORTCPServer.POdj(SenderPnl, Self, train.index, train.GetPOdj(Self))
 else
   ORTCPServer.POdj(SenderPnl, Self, train.index, nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.MenuSOUPRAVA(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer);
var menu: string;
begin
 TTCPORsRef(SenderPnl.Data).train_menu_index := trainLocalI;

 menu := '$'+Self.GlobalSettings.name+',';
 menu := menu + '$Souprava ' + TrainDb.Trains[Self.trains[trainLocalI]].name + ',-,';
 menu := menu + Self.GetTrainMenu(SenderPnl, SenderOr, trainLocalI);

 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), menu);
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkUsek.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string;
var Blk: TBlk;
    train: Integer;
    canAdd: Boolean;
    addStr: string;
    usekStav: TUsekStav;
begin
 Result := inherited;

 if (Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id) <> nil) then
   addStr := 'VLOŽ vlak,'
 else
   addStr := 'NOVÝ vlak,';

 if (Self.TrainsFull() and (Self.trains.Count = 1)) then begin
   TTCPORsRef(SenderPnl.Data).train_menu_index := 0;
   Result := Result + Self.GetTrainMenu(SenderPnl, SenderOR, 0) + '-,';
 end else begin
   canAdd := ((Self.CanStandTrain()) and
              (( (not Self.TrainsFull()) and ((Self.UsekStav.Stav = TUsekStav.obsazeno) or (Self.UsekSettings.RCSAddrs.Count = 0)) ) or // novy vlak
               ( addStr = 'VLOŽ vlak,' ) // presun vlaku
              ));

   if (canAdd) then
     Result := Result + addStr;

   for train in Self.trains do
    begin
     Result := Result + TrainDb.Trains[train].name + ',';
     if (canAdd) then Result := Result + addStr;
    end;

   if ((canAdd) or (Self.trains.Count > 0)) then
     Result := Result + '-,';
 end;

 if ((Self.TrainPredict <> nil) and (Self.UsekStav.stanicni_kolej)) then
   Result := Result + 'PODJ,-,';

 Result := Result + 'STIT,VYL,';

 if (Self.UsekStav.NUZ) then
   Result := Result + '-,NUZ<,';

 if ((((not (SenderOR as TOR).NUZtimer) and (Integer(Self.UsekStav.Zaver) > 0) and (Self.UsekStav.Zaver <> TZaver.ab) and
      (Self.UsekStav.Zaver <> TZaver.staveni) and (Self.typ = btUsek) and
      (not Self.UsekStav.stanicni_kolej)) or (rights >= superuser)) and
      (not Self.UsekStav.NUZ)) then
   Result := Result + '-,NUZ>,';

 //11 = KC
 Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   if ((Self.CanBeKC((SenderOR as TOR).vb, blk)) or Self.CanBeNextVB((SenderOR as TOR).vb, blk)) then
     Result := Result + '-,';
   if (Self.CanBeKC((SenderOR as TOR).vb, blk)) then
     Result := Result + 'KC,';
   if (Self.CanBeNextVB((SenderOR as TOR).vb, blk)) then
     Result := Result + 'VB,';
  end;

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (RCSi.simulation) then
  begin
   Result := Result + '-,';

   for usekStav in Self.SekceStav do
    if (usekStav = TUsekStav.uvolneno) then
     begin
      Result := Result + '*OBSAZ,';
      break;
     end;

   for usekStav in Self.SekceStav do
    if (usekStav = TUsekStav.obsazeno) then
     begin
      Result := Result + '*UVOL,';
      break;
     end;
  end;//if RCSi.lib = 2
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; trainLocalI: Integer): string;
var train: TTrain;
    shPlay: stanicniHlaseniHelper.TSHToPlay;
    train_count: Integer;
begin
 train := TrainDb.Trains[Self.trains[trainLocalI]];
 train_count := Blky.GetBlkWithTrain(TrainDb.Trains[Self.trains[trainLocalI]]).Count;

 if (Self.CanStandTrain()) then
   Result := Result + 'EDIT vlak,';
 if ((Self.CanStandTrain()) or (train_count <= 1)) then
   Result := Result + '!ZRUŠ vlak,';
 if (train_count > 1) then
   Result := Result + '!UVOL vlak,';

 if (train.HVs.Count > 0) then
  begin
   Result := Result + 'RUČ vlak,';
   if (TTCPORsRef(SenderPnl.Data).maus) then Result := Result + 'MAUS vlak,';
  end;

 if (Self.VlakPresun = trainLocalI) then
  Result := Result + 'PŘESUŇ vlak<,'
 else if ((not Self.IsVlakPresun()) and (train.wantedSpeed = 0) and (train.station = SenderOR)) then
   Result := Result + 'PŘESUŇ vlak>,';

 if (train.stolen) then
   Result := Result + 'VEZMI vlak,'
 else begin
   if (train.IsAnyLokoInRegulator()) then
     Result := Result + '!VEZMI vlak,';
 end;

 if (Self.CanStandTrain()) then
   Result := Result + 'PODJ,';

 if ((Assigned(TOR(SenderOR).hlaseni)) and (TOR(SenderOR).hlaseni.available) and
     (train.stationFrom <> nil) and (train.stationTo <> nil) and (train.typ <> '')) then
  begin
   if ((Self.UsekStav.stanicni_kolej) and (train.announcement)) then
     Result := Result + 'HLÁŠENÍ odjezd,';

   try
     shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(train, TOR(SenderOR));
   except
     on E: Exception do
       AppEvents.LogException(E, 'CanPlayPrijezdSH');
   end;

   if ((shPlay.stanicniKolej <> nil) and ((shPlay.trat = nil) or (train.IsPOdj(shPlay.stanicniKolej)))) then
     Result := Result + 'HLÁŠENÍ příjezd,'
   else if (shPlay.trat <> nil) then
     Result := Result + 'HLÁŠENÍ průjezd,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.PanelClick(SenderPnl: TIdContext; SenderOR: TObject ; Button: TPanelButton; rights: TORCOntrolRights; params: string = '');
var Blk: TBlk;
begin
 case (Button) of
  F2: Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params);

  ENTER: begin
    if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
      if (((Self.UsekSettings.maxTrains <> 1) and (Self.trains.Count > 0)) or (not Self.PresunLok(SenderPnl, SenderOR, 0))) then
        Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params);
  end;

  F1: begin
    Blk := Blky.GeTBlkSignalSelected((SenderOR as TOR).id);
    if (Blk = nil) then
      Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params)
    else
      Self.MenuVBClick(SenderPnl, SenderOR);
  end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkUsek.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
var i: Integer;
begin
 if (item = 'NOVÝ vlak')           then Self.MenuNewLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'VLOŽ vlak')      then Self.MenuVLOZLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'EDIT vlak')      then Self.MenuEditLokClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ vlak')      then Self.MenuDeleteLokClick(SenderPnl, SenderOR)
 else if (item = 'UVOL vlak')      then Self.MenuUVOLLokClick(SenderPnl, SenderOR)
 else if (item = 'VEZMI vlak')     then Self.MenuVEZMILokClick(SenderPnl, SenderOR)
 else if (item = 'PŘESUŇ vlak>')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, true)
 else if (item = 'PŘESUŇ vlak<')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, false)
 else if (item = 'RUČ vlak')       then Self.MenuRUCLokClick(SenderPnl, SenderOR)
 else if (item = 'MAUS vlak')      then Self.MenuMAUSLokClick(SenderPnl, SenderOR)
 else if (item = 'STIT')           then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'VYL')            then Self.MenuVylClick(SenderPnl, SenderOR)
 else if (item = 'KC')             then Self.MenuKCClick(SenderPnl, SenderOR)
 else if (item = 'VB')             then Self.MenuVBClick(SenderPnl, SenderOR)
 else if (item = 'NUZ>')           then Self.MenuNUZStartClick(SenderPnl, SenderOR)
 else if (item = 'NUZ<')           then Self.MenuNUZStopClick(SenderPnl, SenderOR)
 else if (item = 'OBSAZ')          then Self.MenuObsazClick(SenderPnl, SenderOR)
 else if (item = 'UVOL')           then Self.MenuUvolClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ odjezd') then Self.MenuHLASENIOdjezdClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ příjezd')then Self.MenuHLASENIPrijezdClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ průjezd')then Self.MenuHLASENIPrujezdClick(SenderPnl, SenderOR)
 else if (item = 'PODJ')           then Self.MenuPOdjClick(SenderPnl, SenderOR)
 else begin
  // cislo soupravy
  for i := 0 to Self.trains.Count-1 do
    if (item = TrainDb.Trains[Self.trains[i]].name) then
     begin
      Self.MenuSOUPRAVA(SenderPnl, SenderOR, i);
      break;
     end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud volba vyvolala nejaky efekt (false pokud se ma zobrazit menu)
function TBlkUsek.PresunLok(SenderPnl: TIdContext; SenderOR: TObject; trainLocalIndex: Integer): Boolean;
var Blk, signal: TBlk;
    train: TTrain;
begin
 Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
 if (Blk = nil) then Exit(false);

 if (not Self.CanStandTrain()) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Loko lze přesunout pouze na staniční kolej!');
   Exit(true);
  end;

 if ((Self.TrainsFull()) and (Blk <> Self)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Do úseku se již nevejde další souprava!');
   Exit(true);
  end;

 if ((Self.Zaver > TZaver.no) and (Self.Zaver <> TZaver.posun)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze přesunout na úsek se závěrem!');
   Exit(true);
  end;

 if (not Self.CanTrainSpeedInsert(trainLocalIndex)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze vložit soupravu před jedoucí soupravu!');
   Exit(true);
  end;

 train := TrainDb.Trains[TBlkUsek(Blk).trains[TBlkUsek(Blk).VlakPresun]];

 if (Blk = Self) then
  begin
   Self.UsekStav.trains.Insert(trainLocalIndex, train.index);

   if (trainLocalIndex <= Self.VlakPresun) then
     Self.UsekStav.trains.Delete(Self.VlakPresun+1)
   else
     Self.UsekStav.trains.Delete(Self.VlakPresun);

   Self.UsekStav.vlakPresun := -1;
   Self.Change();
  end else begin

   try
     Self.AddTrain(trainLocalIndex, train);
     (Blk as TBlkUsek).RemoveTrain(train);
   except
     on E: Exception do
      begin
       ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
       Exit(true);
      end;
   end;
  end;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Souprava '+train.name+' přesunuta na '+Self.GlobalSettings.name+'.');

 if (Blky.GetBlkWithTrain(train).Count = 1) then
   train.front := Self;

 for signal in (Blk as TBlkUsek).signalJCRef do
   Blky.TrainPrediction(signal);

 if (Blk <> Self) then
   for signal in Self.signalJCRef do
     Blky.TrainPrediction(signal);

 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.GetPtData(json: TJsonObject; includeState: Boolean);
begin
 inherited;

 TBlk.RCSstoJSON(Self.UsekSettings.RCSAddrs, json.A['rcs']);

 json['length'] := Self.UsekSettings.Lenght;
 if (Self.UsekSettings.SmcUsek) then json['loop'] := Self.UsekSettings.SmcUsek;
 json['booster'] := Self.UsekSettings.Zesil;
 json['maxTrains'] := Self.UsekSettings.maxTrains;
 json['stationTrack'] := Self.UsekStav.stanicni_kolej;
 if (Self.UsekStav.cislo_koleje <> '') then
   json['trackNumber'] := Self.UsekStav.cislo_koleje;

 if (includeState) then
   Self.GetPtState(json['blockState']);
end;

procedure TBlkUsek.GetPtState(json: TJsonObject);
var train: Integer;
    usekStav: TUsekStav;
begin
 case (Self.Obsazeno) of
  TUsekStav.disabled : json['state'] := 'off';
  TUsekStav.none     : json['state'] := 'none';
  TUsekStav.uvolneno : json['state'] := 'free';
  TUsekStav.obsazeno : json['state'] := 'occupied';
 end;

 for usekStav in Self.SekceStav do
  begin
   case (usekStav) of
    TUsekStav.disabled : json.A['sections'].Add('off');
    TUsekStav.none     : json.A['sections'].Add('none');
    TUsekStav.uvolneno : json.A['sections'].Add('free');
    TUsekStav.obsazeno : json.A['sections'].Add('occupied');
   end;
  end;

 json['power'] := (Self.napajeni = TBoosterSignal.ok);
 json['shortCircuit'] := (Self.zkrat = TBoosterSignal.error);
 json['dcc'] := Self.DCC;

 if (Self.Stitek <> '') then json['note'] := Self.Stitek;
 if (Self.Vyluka <> '') then json['lockout'] := Self.Vyluka;

 for train in Self.trains do
   json.A['trains'].Add(TrainDb.Trains[train].name);

 json['lock'] := Integer(Self.Stav.Zaver);
 if (Self.UsekStav.TrainPredict > -1) then
   json['trainPredict'] := TrainDb.Trains[Self.UsekStav.TrainPredict].name;
 if (Self.UsekStav.NUZ) then
   json['nuz'] := true;
 if (Self.UsekStav.KonecJC <> TZaver.no) then
   json['endJC'] := Integer(Self.UsekStav.KonecJC);
end;

procedure TBlkUsek.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
var trainStr: string;
    train: Integer;
begin
 if (reqJson.Contains('trains')) then
  begin
   if (Cardinal(reqJson.A['trains'].Count) > Self.UsekSettings.maxTrains) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                           'Nelze pridat vice souprav, nez je limit useku');
     inherited;
     Exit();
    end;

   Self.RemoveTrains();
   for trainStr in reqJson.A['trains'] do
    begin
     train := TrainDb.Trains.GetTrainIndexByName(trainStr);
     if (train > -1) then
      begin
       try
         Self.AddTrainS(train)
       except
        on E: Exception do
         begin
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                                E.Message);
         end;

       end;
      end else
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                             'Souprava ' + trainStr + ' neexistuje, ignoruji.');
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.LoadHoukEventToList(list: TList<THoukEv>; ini_tech: TMemIniFile; section: string; prefix: string);
var i: Integer;
    data: string;
begin
 try
   i := 0;
   list.Clear();
   data := ini_tech.ReadString(section, prefix+IntToStr(i), '');
   while (data <> '') do
    begin
     try
       list.Add(THoukEv.Create(data));
     except
       writelog('Nepodarilo se nacist houkaci udalost ' + data + ' bloku ' +
                  Self.name, WR_ERROR);
       Exit();
     end;

     Inc(i);
     data := ini_tech.ReadString(section, prefix+IntToStr(i), '');
    end;
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.CheckHoukEv();
var list: TList<THoukEv>;
begin
 if (Self.UsekStav.currentHoukEv < 0) then Exit();

 if (not Self.IsTrain()) then
  begin
   Self.houk_ev_enabled := false;
   Exit();
  end;

 list := Self.GetHoukList();
 if (list = nil) then
  begin
   Self.houk_ev_enabled := false;
   Exit();
  end;

 if (Self.UsekStav.currentHoukEv >= list.Count) then
  begin
   Self.houk_ev_enabled := false;
   Exit();
  end;

 // kontrola udalosti
 if (list[Self.UsekStav.currentHoukEv].CheckTriggerred(Self)) then
  begin
   // udalost aktivovana, zpracovana a automaticky odregistrovana -> presun na dalsi udalost
   Inc(Self.UsekStav.currentHoukEv);
   if (Self.UsekStav.currentHoukEv >= list.Count) then
     Self.houk_ev_enabled := false
   else
     list[Self.UsekStav.currentHoukEv].Register();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetHoukEvEnabled(): Boolean;
begin
 Result := (Self.UsekStav.currentHoukEv > -1);
end;

procedure TBlkUsek.SetHoukEvEnabled(state: Boolean);
var houkEv: THoukEv;
begin
 if (state) then
  begin
   if (not Self.IsTrain()) then Exit();
   if (Self.GetHoukList().Count = 0) then Exit();

   // aktivace prvni houkaci udalosti
   Self.UsekStav.currentHoukEv := 0;
   Self.GetHoukList()[0].Register();
  end else begin
   Self.UsekStav.currentHoukEv := -1;

   for houkEv in Self.UsekSettings.houkEvL do
     houkEv.Unregister();

   for houkEv in Self.UsekSettings.houkEvS do
     houkEv.Unregister();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetHoukList(): TList<THoukEv>;
begin
 if (not Self.IsTrain()) then Exit(nil);

 if (Self.TrainL.direction = THVStanoviste.lichy) then
   Result := Self.UsekSettings.houkEvL
 else
   Result := Self.UsekSettings.houkEvS;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSHTrain(trainLocalIndex: Integer): TSHTrain;
begin
 if ((trainLocalIndex < 0) or (trainLocalIndex >= Self.trains.Count)) then Exit();

 Result.cislo    := TrainDb.Trains[Self.trains[trainLocalIndex]].name;
 Result.typ      := TrainDb.Trains[Self.trains[trainLocalIndex]].typ;
 Result.kolej    := Self.UsekStav.cislo_koleje;
 Result.fromORid := TOR(TrainDb.Trains[Self.trains[trainLocalIndex]].stationFrom).id;
 Result.toORid   := TOR(TrainDb.Trains[Self.trains[trainLocalIndex]].stationTo).id;

 Result.timeArrive := 0;

 if ((TrainDb.Trains[Self.trains[trainLocalIndex]].IsPOdj(Self)) and
     (TrainDb.Trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs_enabled)) then
   Result.timeDepart := TrainDb.Trains[Self.trains[trainLocalIndex]].GetPOdj(Self).abs
 else
   Result.timeDepart := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.AddNeprofilJC(id: Integer);
begin
 if (Self.UsekStav.neprofilJCcheck.Contains(id)) then Exit();

 Self.UsekStav.neprofilJCcheck.Add(id);
 if (Self.UsekStav.neprofilJCcheck.Count = 1) then
   Self.Change();
end;

procedure TBlkUsek.RemoveNeprofilJC(id: Integer);
begin
 if (not Self.UsekStav.neprofilJCcheck.Contains(id)) then Exit();

 Self.UsekStav.neprofilJCcheck.Remove(id);
 if (Self.UsekStav.neprofilJCcheck.Count = 0) then
   Self.Change();
end;

function TBlkUsek.IsNeprofilJC(): Boolean;
begin
 Result := (Self.UsekStav.neprofilJCcheck.Count > 0);
end;

procedure TBlkUsek.NeprofilObsaz();
var jcid: Integer;
    jc: TJC;
begin
 for jcid in Self.UsekStav.neprofilJCcheck do
  begin
   jc := JCDb.GetJCByID(jcid);
   if (jc <> nil) then
     jc.NeprofilObsaz();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GeTTrainIL(): Integer;
begin
 if (Self.UsekStav.trains.Count < 1) then
   Result := -1
 else
   Result := Self.UsekStav.trains[0];
end;

function TBlkUsek.GeTTrainL(): TTrain;
begin
 if (Self.GeTTrainIL() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainIL()];
end;

function TBlkUsek.GeTTrainIS(): Integer;
begin
 if (Self.UsekStav.trains.Count < 1) then
   Result := -1
 else
   Result := Self.UsekStav.trains[Self.UsekStav.trains.Count-1];
end;

function TBlkUsek.GeTTrainS(): TTrain;
begin
 if (Self.GeTTrainIS() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainIS()];
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsTrain(): Boolean;
begin
 Result := (Self.UsekStav.trains.Count > 0);
end;

function TBlkUsek.IsTrain(index: Integer): Boolean;
begin
 Result := Self.UsekStav.trains.Contains(index);
end;

function TBlkUsek.IsTrain(train: TTrain): Boolean;
begin
 Result := Self.IsTrain(train.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.AddTrainL(index: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(index)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.UsekStav.trains.Insert(0, index);
 Self.UsekStav.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkUsek.AddTrainS(index: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(index)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.UsekStav.trains.Add(index);
 Self.UsekStav.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkUsek.AddTrainL(train: TTrain);
begin
 Self.AddTrainL(train.index);
end;

procedure TBlkUsek.AddTrainS(train: TTrain);
begin
 Self.AddTrainS(train.index);
end;

procedure TBlkUsek.AddTrain(localTrainIndex: Integer; train: Integer);
begin
 if (Self.TrainsFull()) then
   raise ETrainFull.Create('Do bloku ' + Self.name + ' se uz nevejde dalsi souprava!');
 if (Self.trains.Contains(train)) then
   raise EDuplicitTrains.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');
 if (not Self.CanTrainSpeedInsert(localTrainIndex)) then
   raise ERunningTrain.Create('Nelze vložit soupravu před jedoucí soupravu!');

 Self.UsekStav.trains.Insert(localTrainIndex, train);
 Self.UsekStav.TrainPredict := -1;
 Self.Change();
end;

procedure TBlkUsek.AddTrain(localTrainIndex: Integer; train: TTrain);
begin
 Self.AddTrain(localTrainIndex, train.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.RemoveTrains();
var train: Integer;
begin
 for train in Self.trains do
   if (TrainDb.Trains[train].IsPOdj(Self)) then
     TrainDb.Trains[train].RemovePOdj(Self);

 Self.UsekStav.trains.Clear();
 Self.UsekStav.vlakPresun := -1;
 Self.UsekStav.zpomalovani_ready := false;
 Self.houk_ev_enabled := false;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.RemoveTrain(index: Integer);
begin
 if (Self.UsekStav.trains.Contains(index)) then
  begin
   if ((Self.IsVlakPresun) and (Self.trains[Self.VlakPresun] = index)) then
     Self.UsekStav.vlakPresun := -1;

   Self.UsekStav.trains.Remove(index);

   // odstranit predvidany odjezd z aktualniho useku
   if (TrainDb.Trains[index].IsPOdj(Self)) then
     TrainDb.Trains[index].RemovePOdj(Self);

   Self.Change();
  end else begin
   raise ETrainNotExists.Create('Souprava ' + IntToStr(index) +
     ' neexistuje na useku ' + Self.name);
  end;

 if (not Self.IsTrain()) then
  begin
   Self.UsekStav.zpomalovani_ready := false;
   Self.houk_ev_enabled := false;
  end;
end;

procedure TBlkUsek.RemoveTrain(train: TTrain);
begin
 Self.RemoveTrain(train.index);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.TrainsFull(): Boolean;
begin
 Result := (Cardinal(Self.UsekStav.trains.Count) >= Self.UsekSettings.maxTrains);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsVlakPresun(): Boolean;
begin
 Result := (Self.Stav.vlakPresun > -1);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GeTTrainI(): Integer;
begin
 if (Self.trains.Count = 0) then Exit(-1)
 else if (Self.trains.Count = 1) then Exit(Self.trains[0])
 else raise EMultipleTrains.Create('Usek ' + Self.name +
   ' obsahuje vice souprav, nelze se proto ptat jen na jednu soupravu!');
end;

function TBlkUsek.GeTTrain(): TTrain;
begin
 if (Self.GeTTrainI() = -1) then
   Result := nil
 else
   Result := TrainDb.Trains[Self.GeTTrainI()];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.ShowProperMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORControlRights; params: string);
var i: Integer;
begin
 if (params <> '') then begin
   i := StrToIntDef(params, -1);
   if ((i <> -1) and (i >= 0) and (i < Self.trains.Count)) then
     Self.MenuSOUPRAVA(SenderPnl, (SenderOR as TOR), i)
   else
     ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end else
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true prave tehdy, kdyz lze vlozit soupravu na pozici index
// kontroluje, zda-li se nenazime vlozit pred soupravu v pohybu
function TBlkUsek.CanTrainSpeedInsert(index: Integer): Boolean;
begin
 Result := not ((Self.trains.Count > 0) and
                (((index = 0) and (TrainDb.Trains[Self.trains[index]].wantedSpeed > 0) and
                  (TrainDb.Trains[Self.trains[index]].direction = THVStanoviste.sudy)) or
                 ((index = Self.trains.Count) and (TrainDb.Trains[Self.trains[index-1]].wantedSpeed > 0) and
                  (TrainDb.Trains[Self.trains[index-1]].direction = THVStanoviste.lichy))));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.POdjChanged(trainId: Integer; var podj: TPOdj);
var train: Integer;
    was: Boolean;
    nav: TBlk;
    jc: TJC;
begin
 if ((not Self.trains.Contains(trainId)) and (trainId <> Self.UsekStav.TrainPredict)) then
   raise Exception.Create('Souprava již není na úseku!');

 was := TrainDb.Trains[trainId].IsPOdj(Self);

 for train in Self.trains do
   if (train = trainId) then
     podj.RecordOriginNow();

 TrainDb.Trains[trainId].AddOrUpdatePOdj(Self, podj);

 if ((was) and (not TrainDb.Trains[trainId].IsPOdj(Self))) then
  begin
   // PODJ bylo odstraneno -> rozjet soupravu pred navestidlem i kdyz neni na zastavovaci udalosti
   // aktualizaci rychlosti pro vsechny signalJCRef bychom nemeli nic pokazit
   for nav in Self.signalJCRef do
     TBlkSignal(nav).UpdateRychlostTrain(true);
  end;

 // Pri zruseni / zavedei PODJ aktualizovat rychlsot loko, ktera prijizdi,
 // protoze muze dojit ke zmene rychlosti
 jc := JCDb.FindPostavenaJCWithUsek(Self.id);
  if (jc <> nil) then
    TBlkSignal(jc.navestidlo).UpdateRychlostTrain(true);

 Self.PropagatePOdjToTrat();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.CheckPOdjChanged();
var traini: Integer;
    shouldChange: Boolean;
    podj: TPOdj;
    signal: TBlk;
    oblr: TOR;
    train: TTrain;
begin
 shouldChange := false;

 for traini in Self.trains do
  begin
   train := TrainDb.Trains[traini];

   if ((train.IsPOdj(Self)) and (train.GetPOdj(Self).changed)) then
    begin
     podj := train.GetPOdj(Self);

     // prehravani zvukove vystrahy
     if ((not shouldChange) and (Self.IsStujForTrain(train))) then
      begin
        if ((podj.phase_old = ppPreparing) and (podj.GetPhase() = ppGoingToLeave)) then
          for oblr in Self.ORsRef do
            oblr.BlkPlaySound(Self, TORControlRights.write, _SND_STAVENI_VYZVA)

        else if ((podj.phase_old = ppGoingToLeave) and (podj.GetPhase() = ppSoundLeave)) then
          for oblr in Self.ORsRef do
            oblr.BlkPlaySound(Self, TORControlRights.write, _SND_NENI_JC);
      end;

     if (train.GetPOdj(Self).DepRealDelta() < 0) then
      begin
       train.RemovePOdj(Self);

       // tvrda aktualizace rychlosti soupravy
       if ((train = Self.trainL) or (train = Self.trainSudy)) then
        begin
         for signal in Self.signalJCRef do
           if (((TBlkSignal(signal).direction = THVStanoviste.sudy) and (train = Self.trainL)) or
               ((TBlkSignal(signal).direction = THVStanoviste.lichy) and (train = Self.trainSudy))) then
             TBlkSignal(signal).UpdateRychlostTrain(true);
        end;
      end else
       train.GetPOdj(Self).changed := false;

     shouldChange := true;
    end;
  end;

 if (Self.TrainPredict <> nil) then
  begin
   train := Self.TrainPredict;
   if ((train.IsPOdj(Self)) and (train.GetPOdj(Self).changed)) then
    begin
     if (train.GetPOdj(Self).DepRealDelta() < 0) then
       train.RemovePOdj(Self)
     else
       train.GetPOdj(Self).changed := false;

     shouldChange := true;
    end;
  end;

 if (shouldChange) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// predvidane odjezdy se mazou pres bloky, aby bloky zavolaly Change().
procedure TBlkUsek.ClearPOdj();
var train: Integer;
    change: Boolean;
begin
 change := false;

 for train in Self.trains do
  begin
   if (TrainDb.Trains[train].IsPOdj(Self)) then
    begin
     TrainDb.Trains[train].RemovePOdj(Self);
     change := true;
    end;
  end;

 if ((Self.TrainPredict <> nil) and (Self.TrainPredict.IsPOdj(Self))) then
  begin
   Self.TrainPredict.RemovePOdj(Self);
   change := true;
  end;

 if (change) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.PropagatePOdjToTrat();
var signal: TBlk;
begin
 for signal in Self.signalJCRef do
   TBlkSignal(signal).PropagatePOdjToTrat();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsStujForTrain(train: TTrain): Boolean;
begin
 if (not Self.trains.Contains(train.index)) then Exit(false);
 if (Self.signalJCRef.Count = 0) then Exit(true);

 if (Self.signalJCRef.Count = 1) then
  begin
   if (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) then Exit(true);
   if (TBlkSignal(Self.signalJCRef[0]).direction = THvStanoviste.lichy) then
     Exit(train <> Self.trainSudy)
   else
     Exit(train <> Self.trainL);
  end;

 if (Self.signalJCRef.Count = 2) then
  begin
   if ((Self.trains.Count = 1) and (not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) and
        (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal())) then Exit(true);
   if ((Self.trains.Count >= 2) and ((not TBlkSignal(Self.signalJCRef[0]).IsGoSignal()) or
        (not TBlkSignal(Self.signalJCRef[1]).IsGoSignal()))) then Exit(true);
   if ((Self.trains.Count > 2) and (Self.trainL <> train) and (Self.trainSudy <> train)) then Exit(true);
 end;

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.PanelStateString(): string;
var fg, bg, nebarVetve, sfg, sbg: TColor;
    Blk: TBlk;
    traini, i: Integer;
begin
 // Pro blok trati se vola take
 Result := IntToStr(Integer(btUsek))+';'+IntToStr(Self.id)+';';;

 nebarVetve := $A0A0A0;

 // --- Popredi ---

 case (Self.Obsazeno) of
  TUsekStav.disabled : fg := clFuchsia;
  TUsekStav.none     : fg := $A0A0A0;
  TUsekStav.uvolneno : fg := $A0A0A0;
  TUsekStav.obsazeno : fg := clRed;
 else
  fg := clFuchsia;
 end;

 // zobrazeni zakazu odjezdu do trati
 if ((fg = $A0A0A0) and (Self.typ = btTU) and (TBlkTU(Self).InTrat > -1)) then
  begin
   Blky.GetBlkByID(TBlkTU(Self).InTrat, Blk);
   if ((Blk <> nil) and (Blk.typ = btTrat)) then
     if ((Blk as TBlkTrat).ZAK) then
       fg := clBlue;
  end;

 // neprofilove deleni v useku
 if ((fg = $A0A0A0) and (Self.IsNeprofilJC())) then
   fg := clYellow;

 // zaver
 if (((Self.Obsazeno) = TUsekStav.uvolneno) and (Self.typ = btUsek) and
     (Self.GetSettings().RCSAddrs.Count > 0)) then
  begin
   case (Self.Zaver) of
    TZaver.vlak   : fg := clLime;
    TZaver.posun  : fg := clWhite;
    TZaver.nouz   : fg := clAqua;
    TZaver.ab     : if (diag.showZaver) then fg := $707070 else fg := $A0A0A0;
    TZaver.staveni: if (diag.showZaver) then fg := clBlue;
   end;//case
  end;

 // porucha BP v trati
 if ((Self.typ = btTU) and (TBlkTU(Self).poruchaBP)) then fg := clAqua;

 if (fg = clYellow) then
   nebarVetve := clYellow;

 Result := Result + ownConvert.ColorToStr(fg) + ';';

 // --- Pozadi ---

 bg := clBlack;
 if (Self.Stitek <> '') then bg := clTeal;
 if (Self.Vyluka <> '') then bg := clOlive;

 if (not Self.DCC) then bg := clMaroon;
 if (Self.zkrat = TBoosterSignal.error) then bg := clFuchsia;
 if ((Self.napajeni <> TBoosterSignal.ok) or
    (Self.zkrat = TBoosterSignal.undef)) then bg := clBlue;

 Result := Result + ownConvert.ColorToStr(bg) + ';';

 Result := Result + IntToStr(ownConvert.BoolToInt(Self.NUZ)) + ';' +
                    IntToStr(Integer(Self.KonecJC)) + ';' +
                    ownConvert.ColorToStr(nebarVetve) + ';';

 // seznam souprav
 Result := Result + '{';
 for i := 0 to Self.trains.Count-1 do
  begin
   traini := Self.trains[i];
   sfg := fg;
   sbg := bg;

   if (Self.Obsazeno = uvolneno) then
     sfg := clAqua;

   Result := Result + '(' + TrainDb.Trains[traini].name + ';' +
                            IntToStr(ownConvert.BoolToInt(TrainDb.Trains[traini].sdata.dir_L)) +
                            IntToStr(ownConvert.BoolToInt(TrainDb.Trains[traini].sdata.dir_S)) + ';';

   if ((TrainDb.Trains[traini].stationTo = TrainDb.Trains[traini].station) and (sbg = clBlack)) then
     sbg := clSilver;

   // predvidany odjezd
   if (TrainDb.Trains[traini].IsPOdj(Self)) then
     predvidanyOdjezd.GetPOdjColors(TrainDb.Trains[traini].GetPOdj(Self), sfg, sbg);

   Result := Result + ownConvert.ColorToStr(sfg) + ';' +
                      ownConvert.ColorToStr(sbg) + ';';

   if (Self.vlakPresun = i) then
    Result := Result + ownConvert.ColorToStr(clYellow) + ';';

   Result := Result + ')';
  end;

 // predpovidana souprava
 if (Self.TrainPredict <> nil) then
  begin
   // predvidany odjezd
   sfg := fg;
   sbg := bg;

   if (Self.TrainPredict.IsPOdj(Self)) then
     predvidanyOdjezd.GetPOdjColors(Self.TrainPredict.GetPOdj(Self), sfg, sbg);

   Result := Result + '(' + Self.TrainPredict.name + ';' +
              '00;' +
              ownConvert.ColorToStr(sfg) + ';' +
              ownConvert.ColorToStr(sbg) + ';)';
  end;

 Result := Result + '}';
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.RealZesZkrat(): TBoosterSignal;
begin
 if (Boosters.ContainsKey(Self.UsekSettings.Zesil)) then
  begin
   Result := Boosters[Self.UsekSettings.Zesil].zkrat;
  end else begin
   Result := TBoosterSignal.ok;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.CanStandTrain(): Boolean;
begin
 Result := (Self.UsekStav.stanicni_kolej or Self.UsekStav.train_pos);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.CanBeNextVB(vbs: TList<TObject>; start: TBlk): Boolean;
var vbs_plus_me: TList<TObject>;
begin
 if (vbs.Contains(Self)) then
   Exit(false);

 vbs_plus_me := TList<TObject>.Create();

 try
   vbs_plus_me.AddRange(vbs);
   vbs_plus_me.Add(Self);

   Result := ((JCDb.IsAnyJCWithPrefix(start as TBlkSignal, vbs_plus_me)) or
              (MultiJCDb.IsAnyMJCWithPrefix(start as TBlkSignal, vbs_plus_me)));
 finally
   vbs_plus_me.Free();
 end;
end;

function TBlkUsek.CanBeKC(vbs: TList<TObject>; start: TBlk): Boolean;
begin
 Result := ((JCDb.FindJC(start as TBlkSignal, vbs, Self) <> nil) or
            (MultiJCDb.FindMJC(start as TBlkSignal, vbs, Self) <> nil));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetNavL(): TBlk;
var blk: TBlk;
begin
 for blk in Blky do
   if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVStanoviste.lichy)) then
     Exit(blk);
 Result := nil;
end;

function TBlkUsek.GetNavS(): TBlk;
var blk: TBlk;
begin
 for blk in Blky do
   if ((blk.typ = btSignal) and (TBlkSignal(blk).trackId = Self.id) and (TBlkSignal(blk).direction = THVStanoviste.sudy)) then
     Exit(blk);
 Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

