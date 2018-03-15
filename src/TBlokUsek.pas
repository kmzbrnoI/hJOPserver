unit TBlokUsek;

//definice a obsluha technologickeho bloku Usek

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, Booster, houkEvent,
     IdContext, Generics.Collections, JsonDataObjects, TOblRizeni, rrEvent,
     stanicniHlaseni, changeEvent, predvidanyOdjezd;

type
 TUsekStav  = (disabled = -5, none = -1, uvolneno = 0, obsazeno = 1);

 ESprFull = class(Exception);
 ESprNotExists = class(Exception);
 EMultipleSprs = class(Exception);
 EDuplicitSprs = class(Exception);
 ERunningSpr   = class(Exception);

 //technologicka nastaveni useku (delka, MTB, ...)
 TBlkUsekSettings = record
  RCSAddrs:TRCSAddrs;
  Lenght:double;          //delka useku v metrech
  SmcUsek:boolean;        //specialni pripad: usek ve smycce
  Zesil:string;           //id zesilovace
  houkEvL:TList<THoukEv>;  //seznam houkacich udalosti pro lichy smer
  houkEvS:TList<THoukEv>;  //seznam houkacich udalosti pro sudy smer
  maxSpr:Cardinal;        // maximalni pocet souprav v bloku
 end;

 TUsekStavAr = array[0..3] of TUsekStav;

 //aktualni stav useku (obsazeno, ...)
 TBlkUsekStav = record
  Stav,StavOld:TUsekStav;   // hlavni stav useku a jeho predchozi hodnota (pouzivano v Update())
  StavAr:TUsekStavAr;       // obsazenost jednotlivych casti useku
  Zaver:TZaver;             // zaver na bloku
  NUZ:boolean;              // nouzove uvolneni zaveru
  KonecJC:TZaver;           // jaka jizdni cesta (vlakova, posunova, nouzova) konci na tomto bloku - vyuzivano pro zobrazeni
  Stit,Vyl:string;          // stitek a vyluka
  SComJCRef:TList<TBlk>;    // navestidla, ze kterych je z tohoto bloku postavena JC
  SprPredict:Integer;       // souprava, ktera je na tomto bloku predpovidana
  zkrat:TBoosterSignal;     // zkrat zesilovace
  napajeni:TBoosterSignal;  // napajeni zesilovace
  DCC:boolean;              // stav DCC na useku: kdyz je kontrola na SPAXu, beru SPAX, jinak se bere stav z centraly
  stanicni_kolej:boolean;   // pokud je blok stanicni koleji, je zde true, jinak false
  cislo_koleje:string;      // cislo koleje, pokud je stanicni
  vlakPresun:Integer;       // index soupravy, ktera se presouva, v ramci lokalniho seznamu souprav na useku; zadny presun = -1

  zpomalovani_ready:boolean;          // pri predani soupravy do tohoto useku z trati, ci z jizdni cesty, je tento flag nastaven na true
                                      // to znamena, ze je souprava pripravena ke zpomalovani; navetidlo umozni zpomaleni jen, pokud je tento flag na true
                                      // po zpomaleni si navestidlo flag zrusi

  spr_vypadek:boolean;      // ztrata soupravy na useku se pozna tak, ze blok po urcity cas obsahuje soupravu, ale neni obsazen
  spr_vypadek_time:Integer; //    to je nutne pro predavani souprav mezi bloky v ramci JC (usek se uvolni, ale souprava se jeste nestihne predat
                            // pro reseni timeoutu jsou tyto 2 promenne
  DCCGoTime:TDateTime;      // cas kdy doslo k zpanuti DCC
  currentHoukEv:Integer;    // index aktualni houkaci udalosti
  neprofilJCcheck:TList<Integer>; // seznam id jizdnich cest, ktere na usek uvalily podminku neprofiloveho styku
  soupravy:TList<Integer>;        // seznam souprav na useku ve smeru L --> S
 end;

 TBlkUsek = class(TBlk)
  const
   //defaultni stav
   _def_usek_stav:TBlkUsekStav = (
    Stav : disabled;
    StavOld : disabled;
    StavAr : (disabled, disabled, disabled, disabled);
    Zaver : no;
    NUZ : false;
    KonecJC : no;
    Stit : '';
    Vyl : '';
    SprPredict : -1;
    zkrat : TBoosterSignal.undef;
    napajeni : TBoosterSignal.undef;
    DCC : false;
    stanicni_kolej : false;
    cislo_koleje : '';
    vlakPresun: -1;
    zpomalovani_ready : false;
    currentHoukEv : -1;
   );

   _DEFAULT_MAX_SPR = 1;

  private
   UsekStav:TBlkUsekStav;
   last_zes_zkrat:TBoosterSignal;  //poziva se na pamatovani posledniho stavu zkratu zesilovace pri vypnuti DCC

    procedure SetUsekNUZ(nuz:boolean);
    procedure SetUsekZaver(Zaver:TZaver);
    procedure SetUsekStit(stit:string);
    procedure SetUsekVyl(vyl:string); overload;
    procedure SetSprPredict(sprcesta:Integer);
    procedure SetKonecJC(konecjc:TZaver);
    procedure SetVlakPresun(presun:Integer);

    procedure SetZesZkrat(state:TBoosterSignal);
    procedure SetZesNap(state:TBoosterSignal);
    procedure SetZesDCC(state:TBoosterSignal);
    procedure SetCentralaDCC(state:boolean);
    procedure SetDCC(state:boolean);

    procedure MenuNewLokClick(SenderPnl:TIdContext; SenderOR:TObject; itemindex:Integer);
    procedure MenuEditLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuDeleteLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUVOLLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVEZMILokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRUCLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuMAUSLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNUZStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNUZStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPRESUNLokClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
    procedure MenuHLASENIOdjezdClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuHLASENIPrijezdClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuHLASENIPrujezdClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSOUPRAVA(SenderPnl:TIdContext; SenderOR:TObject; sprLocalI:Integer);
    procedure MenuVLOZLokClick(SenderPnl:TIdContext; SenderOR:TObject; itemindex:Integer);
    procedure MenuPOdjClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure PotvrDeleteLok(Sender:TIdContext; success:boolean);
    procedure PotvrUvolLok(Sender:TIdContext; success:boolean);

    procedure MenuObsazClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUvolClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure ORVylukaNull(Sender:TIdContext; success:boolean);

    procedure LoadHoukEventToList(list:TList<THoukEv>; ini_tech:TMemIniFile; section:string; prefix:string);
    procedure CheckHoukEv();

    function GetHoukList():TList<THoukEv>;
    function GetHoukEvEnabled():boolean;
    procedure SetHoukEvEnabled(state:boolean);

    function GetSHSpr(sprLocalIndex:Integer):TSHSpr;

    procedure NeprofilObsaz();

    function GetSoupravaL():Integer;
    function GetSoupravaS():Integer;
    function GetUsekSpr():Integer;

    function GetSprMenu(SenderPnl:TIdContext; SenderOR:TObject; sprLocalI:Integer):string;
    procedure ShowProperMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORControlRights; params:string);

    function CanSprSpeedInsert(index:Integer):boolean;

    procedure CheckPOdjChanged();

    function IsStujForSpr(spr:Integer):boolean;

  protected
   UsekSettings:TBlkUsekSettings;

    procedure MenuVBClick(SenderPnl:TIdContext; SenderOR:TObject);
    function MenuKCClick(SenderPnl:TIdContext; SenderOR:TObject):boolean;
    function PresunLok(SenderPnl:TIdContext; SenderOR:TObject; sprLocalIndex:Integer):boolean;

  public

    EventsOnObsaz:TChangeEvents;
    EventsOnUvol:TChangeEvents;
    EventsOnZaverRelease:TChangeEvents;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;

    //update states
    procedure Update(); override;

    procedure Freeze(); override;
    procedure UnFreeze(); override;

    //----- usek own functions -----

    function GetSettings():TBlkUsekSettings;
    procedure SetSettings(data:TBlkUsekSettings);

    procedure GetObsazeno(var ar:TUsekStavAr);

    procedure SetUsekVyl(Sender:TIDCOntext; vyl:string); overload;

    procedure AddNeprofilJC(id:Integer);
    procedure RemoveNeprofilJC(id:Integer);
    function IsNeprofilJC():boolean;

    function IsSouprava():boolean; overload;
    function IsSouprava(index:Integer):boolean; overload;
    procedure AddSoupravaL(index:Integer); virtual;
    procedure AddSoupravaS(index:Integer); virtual;
    procedure AddSouprava(localSprIndex:Integer; souprava:Integer);
    procedure RemoveSoupravy(); virtual;
    procedure RemoveSouprava(index:Integer); virtual;
    function SoupravyFull():boolean;

    function IsVlakPresun():boolean;
    procedure ClearPOdj();

    property Stav:TBlkUsekStav read UsekStav;

    property Obsazeno:TUsekStav read UsekStav.Stav;
    property NUZ:boolean read UsekStav.NUZ write SetUsekNUZ;
    property Zaver:TZaver read UsekStav.Zaver write SetUsekZaver;
    property Stitek:string read UsekStav.Stit write SetUsekStit;
    property Vyluka:string read UsekStav.Vyl write SetUsekVyl;
    property SprPredict:Integer read UsekStav.SprPredict write SetSprPredict;
    property KonecJC:TZaver read UsekStav.KonecJC write SetKonecJC;
    property SComJCRef:TList<TBlk> read UsekStav.SComJCRef write UsekStav.SComJCRef;

    property Souprava:Integer read GetUsekSpr;
    property SoupravaL:Integer read GetSoupravaL;
    property SoupravaS:Integer read GetSoupravaS;
    property Soupravs:TList<Integer> read UsekStav.Soupravy;

    property ZesZkrat:TBoosterSignal read UsekStav.Zkrat write SetZesZkrat;
    property ZesNapajeni:TBoosterSignal read UsekStav.Napajeni write SetZesNap;
    property ZesDCC:TBoosterSignal write SetZesDCC;
    property CentralaDCC:boolean write SetCentralaDCC;
    property DCC:boolean read UsekStav.DCC;

    property VlakPresun:Integer read UsekStav.vlakPresun write SetVlakPresun;
    property zpomalovani_ready:boolean read UsekStav.zpomalovani_ready write UsekStav.zpomalovani_ready;
    property houk_ev_enabled:boolean read GetHoukEvEnabled write SetHoukEvEnabled;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    procedure PanelPOdj(SenderPnl:TIdContext; sprId:Integer; var podj:TPOdj);

    //PT:

    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;
    procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject); override;

 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, TBlokSCom, Logging, RCS, ownStrUtils,
    TJCDatabase, fMain, TCPServerOR, TBlokTrat, SprDb, THVDatabase, Zasobnik,
    TBlokIR, Trakce, THnaciVozidlo, TBlokTratUsek, BoosterDb, appEv, Souprava,
    stanicniHlaseniHelper, TechnologieJC, PTUtils;

constructor TBlkUsek.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_USEK;
 Self.UsekStav := _def_usek_stav;

 Self.EventsOnObsaz := TChangeEvents.Create();
 Self.EventsOnUvol  := TChangeEvents.Create();
 Self.EventsOnZaverRelease := TChangeEvents.Create();

 Self.UsekSettings.houkEvL := TList<THoukEv>.Create();
 Self.UsekSettings.houkEvS := TList<THoukEv>.Create();

 Self.UsekSettings.maxSpr := _DEFAULT_MAX_SPR;

 Self.UsekStav.neprofilJCcheck := TList<Integer>.Create();
 Self.UsekStav.soupravy := TList<Integer>.Create();
 Self.UsekStav.SComJCRef := TList<TBlk>.Create();
end;//ctor

destructor TBlkUsek.Destroy();
var houkEv:THoukEv;
begin
 if (Assigned(Self.UsekSettings.houkEvL)) then
  begin
   for houkEv in Self.UsekSettings.houkEvL do
     houkEv.Free();
   Self.UsekSettings.houkEvL.Free();
  end;

 if (Assigned(Self.UsekSettings.houkEvS)) then
  begin
   for houkEv in Self.UsekSettings.houkEvS do
     houkEv.Free();
   Self.UsekSettings.houkEvS.Free();
  end;

 Self.EventsOnObsaz.Free();
 Self.EventsOnUvol.Free();
 Self.EventsOnZaverRelease.Free();

 Self.UsekStav.neprofilJCcheck.Free();
 Self.UsekStav.soupravy.Free();
 Self.UsekStav.SComJCRef.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
    s:string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.UsekSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
 Self.UsekSettings.Lenght   := ini_tech.ReadFloat(section,'delka',0);
 Self.UsekSettings.Zesil    := ini_tech.ReadString(section,'zesil','');
 Self.UsekSettings.SmcUsek  := ini_tech.ReadBool(section, 'smc', false);
 Self.UsekSettings.maxSpr   := ini_tech.ReadInteger(section, 'maxSpr', _DEFAULT_MAX_SPR);

 if (Boosters[Self.UsekSettings.Zesil] = nil) then
   writelog('WARNING: Blok '+Self.GetGlobalSettings.name + ' ('+IntToStr(Self.GetGlobalSettings.id)+
            ') nemá návaznost na validní zesilovaè', WR_ERROR);

 Self.UsekStav.Stit         := ini_stat.ReadString(section, 'stit', '');
 Self.UsekStav.Vyl          := ini_stat.ReadString(section, 'vyl' , '');

 str := TStringList.Create();

 Self.UsekStav.soupravy.Clear();
 ExtractStringsEx([','], [], ini_stat.ReadString(section, 'spr' , ''), str);
 for s in str do
   Self.UsekStav.soupravy.Add(Soupravy.GetSprIndexByName(s));

 // houkaci udalosti
 try
   Self.LoadHoukEventToList(Self.UsekSettings.houkEvL, ini_tech, section, 'houkL');
 except
   writelog('Nepodaøilo se naèíst houkací události L bloku ' + Self.GetGlobalSettings.name, WR_ERROR);
 end;

 try
   Self.LoadHoukEventToList(Self.UsekSettings.houkEvS, ini_tech, section, 'houkS');
 except
   writelog('Nepodaøilo se naèíst houkací události S bloku ' + Self.GetGlobalSettings.name, WR_ERROR);
 end;


 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str.Clear();
   ExtractStrings([';'],[],PChar(ini_rel.ReadString('U',IntToStr(Self.GlobalSettings.id),'')),str);
   if (str.Count < 1) then Exit;

   Self.ORsRef := ORs.ParseORs(str[0]);

   if (str.Count >= 2) then
    begin
     Self.UsekStav.stanicni_kolej := (str[1] = '1');
     if (str.Count >= 3) then
       Self.UsekStav.cislo_koleje := str[2]
     else
       Self.UsekStav.cislo_koleje := '';
    end;

   if ((not Self.UsekStav.stanicni_kolej) and (Self.UsekSettings.maxSpr <> 1)) then
     Self.UsekSettings.maxSpr := 1;
  end else begin
   Self.ORsRef.Cnt := 0;
  end;

 PushRCSToOR(Self.ORsRef, Self.UsekSettings.RCSAddrs);

 str.Free();
end;//procedure

procedure TBlkUsek.SaveData(ini_tech:TMemIniFile;const section:string);
var i:Integer;
begin
 inherited SaveData(ini_tech,section);

 Self.SaveRCS(ini_tech, section, Self.UsekSettings.RCSAddrs);
 ini_tech.WriteFloat(section,'delka', Self.UsekSettings.Lenght);
 ini_tech.WriteString(section,'zesil', Self.UsekSettings.Zesil);

 if (Self.UsekSettings.maxSpr <> _DEFAULT_MAX_SPR) then
   ini_tech.WriteInteger(section, 'maxSpr', Self.UsekSettings.maxSpr);

 if (Self.UsekSettings.SmcUsek) then
   ini_tech.WriteBool(section, 'smc', Self.UsekSettings.SmcUsek);

 if (Assigned(Self.UsekSettings.houkEvL)) then
  begin
   for i := 0 to Self.UsekSettings.houkEvL.Count-1 do
    begin
     try
       ini_tech.WriteString(section, 'houkL'+IntToStr(i), Self.UsekSettings.houkEvL[i].GetDefString());
     except
       on E:Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.GetGlobalSettings().name);
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
       on E:Exception do
         AppEvents.LogException(E, 'Ukladani houkaci udalosti bloku ' + Self.GetGlobalSettings().name);
     end;
    end;
  end;
end;//procedure

procedure TBlkUsek.SaveStatus(ini_stat:TMemIniFile;const section:string);
var str:string;
    spri:Integer;
begin
 if (Self.UsekStav.Stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.UsekStav.Stit);

 if (Self.UsekStav.Vyl <> '') then
   ini_stat.WriteString(section, 'vyl' , Self.UsekStav.Vyl);

 if (Self.IsSouprava()) then
  begin
   str := '';
   for spri in Self.Soupravs do
     str := str + Soupravy[spri].nazev + ',';
   ini_stat.WriteString(section, 'spr' , str);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.Enable();
var i:Integer;
begin
 try
   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
     if (not RCSi.IsModule(Self.UsekSettings.RCSAddrs.data[i].board)) then
      Exit();
 except
   Exit();
 end;

 Self.UsekStav.Stav    := none;
 Self.UsekStav.StavOld := none;
 for i := 0 to 3 do Self.UsekStav.StavAr[i] := none;
 Self.UsekStav.neprofilJCcheck.Clear();
 Self.Update();
 //change event will be called in Update();
end;//procedure

procedure TBlkUsek.Disable();
var i:Integer;
begin
 inherited;

 Self.UsekStav.Stav       := disabled;
 Self.UsekStav.StavOld    := disabled;
 Self.UsekStav.NUZ        := false;
 Self.UsekStav.SprPredict := -1;
 Self.UsekStav.KonecJC    := TZaver.no;
 Self.UsekStav.zpomalovani_ready := false;
 Self.houk_ev_enabled     := false;
 Self.UsekStav.zkrat      := TBoosterSignal.undef;
 Self.UsekStav.napajeni   := TBoosterSignal.undef;
 Self.UsekStav.DCC        := false;
 for i := 0 to 3 do Self.UsekStav.StavAr[i] := disabled;

 Self.EventsOnObsaz.Clear();
 Self.EventsOnUvol.Clear();
 Self.EventsOnZaverRelease.Clear();

 Self.UsekStav.neprofilJCcheck.Clear();

 Self.Change();
end;//procedure

procedure TBlkUsek.Reset();
begin
 Self.Zaver := TZaver.no;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkUsek.Update();
var i, spr:Integer;
    state:TRCSInputState;
begin
 if (((Self.ZesZkrat = TBoosterSignal.error) or (Self.ZesNapajeni = TBoosterSignal.error)) and (not Self.frozen)) then
  begin
   Self.Freeze();
   Exit();
  end;

 if (Self.frozen) then
  begin
   if (((not Self.DCC) or (Self.ZesNapajeni = TBoosterSignal.error)
        or (Now < Self.Stav.DCCGoTime+EncodeTime(0, 0, 1, 0)))
       and (Self.ZesZkrat = TBoosterSignal.error)) then
    begin
     Self.UsekStav.zkrat := TBoosterSignal.ok;
     for i := 0 to Self.ORsRef.Cnt-1 do
       Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt - 1;
     Self.Change(true);
    end;
   if ((Self.DCC) and (Self.ZesZkrat <> TBoosterSignal.error) and (Self.ZesNapajeni <> TBoosterSignal.error) and
       (Now > Self.Stav.DCCGoTime+EncodeTime(0, 0, 1, 0))) then Self.UnFreeze();
   Exit();
  end;

 for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
  begin
   try
     state := RCSi.GetInput(Self.UsekSettings.RCSAddrs.data[i].board, Self.UsekSettings.RCSAddrs.data[i].port);
   except
     state := failure;
   end;

   case (state) of
    isOn  : Self.UsekStav.StavAr[i] := TUsekStav.obsazeno;
    isOff : Self.UsekStav.StavAr[i] := TUsekStav.uvolneno;
    failure, notYetScanned, unavailable:begin
      // vypadek MTB, ci nespravny argument -> disable blok
      if (Self.UsekStav.Stav <> disabled) then
       begin
        Self.UsekStav.Stav    := disabled;
        Self.UsekStav.StavOld := Self.UsekStav.Stav;
        JCDb.RusJC(Self);

        // zastavime soupravy na useku
        for spr in Self.Soupravs do
          Soupravy.soupravy[spr].rychlost := 0;

        Self.Change(true);
       end;
      Exit();
    end;
   end;//case
  end;//for i

 //get current state
 Self.UsekStav.Stav := uvolneno;
 for i := 0 to 3 do
  if (Self.UsekStav.StavAr[i] = TUsekStav.obsazeno) then
   begin
    Self.UsekStav.Stav := TUsekStav.obsazeno;
    break;
   end;

 // reseni vypadku soupravy
 // pad soupravy z bloku az po urcitem case - aby se jizdni ceste nechal cas na zpracovani pohybu soupravy
 if (Self.UsekStav.spr_vypadek) then
  begin
   if (not Self.IsSouprava()) then
    begin
     Self.UsekStav.spr_vypadek := false;
     Exit();
    end;

   Inc(Self.UsekStav.spr_vypadek_time);
   if (Self.UsekStav.spr_vypadek_time > 3) then
    begin
     Self.UsekStav.spr_vypadek := false;

     // informace o vypadku soupravy probiha jen ve stanicnich kolejich a v trati
     if ((Self.GetGlobalSettings().typ = _BLK_TU) or (Self.UsekStav.stanicni_kolej)) then
       for i := 0 to Self.OblsRizeni.Cnt-1 do
         Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Ztráta soupravy v úseku '+Self.GetGlobalSettings().name, 'TECHNOLOGIE');
     if (Self.UsekStav.Zaver <> TZaver.no) then Self.UsekStav.Zaver := TZaver.nouz;
    end;//if spr_vypadek_time > 3
  end;//if spr_vypadek

 //OnChange
 if (Self.UsekStav.Stav <> Self.UsekStav.StavOld) then
  begin
   // kontrola udalosti obsazeni
   if (Self.UsekStav.Stav = TUsekStav.obsazeno) then begin
     Self.NeprofilObsaz();
     Self.CallChangeEvents(Self.EventsOnObsaz);
   end else if (Self.UsekStav.Stav = TUsekStav.uvolneno) then
     Self.CallChangeEvents(Self.EventsOnUvol);

   if (Self.IsSouprava()) then
    begin
     // souprava
     if ((Self.UsekStav.Stav = TUsekStav.uvolneno) and (Self.UsekStav.StavOld = TUsekStav.obsazeno)) then
      begin
       Self.UsekStav.spr_vypadek      := true;
       Self.UsekStav.spr_vypadek_time := 0;
      end;
    end;//if Spr <> -1

   Self.UsekStav.StavOld := Self.UsekStav.Stav;
   Self.Change();
  end;

 // reseni zruseni PRESUN soupravy, ktera jede
 if ((Self.IsVlakPresun()) and ((not Self.IsSouprava(Self.Soupravs[Self.VlakPresun])) or
     (Soupravy.soupravy[Self.VlakPresun].rychlost > 0))) then
   Self.VlakPresun := -1;

 // pousteni houkani na houkaci udalosti
 if (Self.UsekStav.currentHoukEv > -1) then
   Self.CheckHoukEv();

 // kontrola zmeny barev vlivem uplynuti casu predvidaneho odjezdu
 Self.CheckPOdjChanged();

 inherited Update();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.SetUsekNUZ(nuz:boolean);
var i:Integer;
begin
 if (Self.UsekStav.NUZ = nuz) then Exit(); 

 if (Self.UsekStav.NUZ) and (not nuz) then
  begin
   for i := 0 to Self.ORsRef.Cnt-1 do
    begin
     Self.ORsRef.ORs[i].NUZblkCnt := Self.ORsRef.ORs[i].NUZblkCnt - 1;
     if (Self.ORsRef.ORs[i].NUZblkCnt < 0) then Self.ORsRef.ORs[i].NUZblkCnt := 0;
    end;
  end else begin
    if ((not Self.UsekStav.NUZ) and (nuz)) then
     for i := 0 to Self.ORsRef.Cnt-1 do
       Self.ORsRef.ORs[i].NUZblkCnt := Self.ORsRef.ORs[i].NUZblkCnt + 1;

  end;

 Self.UsekStav.NUZ := nuz;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekZaver(Zaver:TZaver);
var old:TZaver;
begin
 if (Zaver = Self.Zaver) then Exit();

 if ((Integer(Self.UsekStav.Zaver) > 0) and (Zaver = TZaver.no)) then
   Self.NUZ := false;

 old := Self.Zaver;
 Self.UsekStav.Zaver      := Zaver;
 Self.UsekStav.SprPredict := -1;

 if ((old > TZaver.no) and (zaver = TZaver.no)) then
   Self.CallChangeEvents(Self.EventsOnZaverRelease);

 // staveci zavery se do panelu neposilaji, protoze jsou mi k nicemu
 if (Self.Zaver <> TZaver.staveni) or (old <> TZaver.no) then
   Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekStit(stit:string);
begin
 Self.UsekStav.Stit := stit;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekVyl(vyl:string);
begin
 Self.UsekStav.Vyl := vyl;
 Self.Change();
end;//procedure

procedure TBlkUsek.ORVylukaNull(Sender:TIdContext; success:boolean);
begin
 if (success) then
  Self.Vyluka := '';
end;//procedure

procedure TBlkUsek.SetUsekVyl(Sender:TIDCOntext; vyl:string);
begin
 if ((self.UsekStav.Vyl <> '') and (vyl = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef.ORs[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil);
  end else begin
   Self.Vyluka := vyl;
  end;
end;//procedure

procedure TBlkUsek.SetSprPredict(sprcesta:Integer);
var old:Integer;
begin
 old := Self.UsekStav.SprPredict;
 Self.UsekStav.SprPredict := sprcesta;

 if ((sprcesta = -1) and (old > -1)) then
  begin
   // odstranit predvidany odjezd mazane predpovidane soupravy
   if (Soupravy[old].IsPOdj(Self)) then
     Soupravy[old].RemovePOdj(Self);
  end;

 Self.Change();
end;//procedure

procedure TBlkUsek.SetKonecJC(konecjc:TZaver);
begin
 Self.UsekStav.KonecJC := konecjc;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetZesZkrat(state:TBoosterSignal);
var i:Integer;
begin
 if (Self.frozen) then
   Self.last_zes_zkrat := state;

 if ((state = TBoosterSignal.error) and (not Self.frozen) and
     ((not Self.DCC) or (Self.ZesNapajeni = TBoosterSignal.error) or
      (Now < Self.Stav.DCCGoTime+EncodeTime(0, 0, 1, 0)))) then
  begin
   if (Self.ZesZkrat <> TBoosterSignal.ok) then
    begin
     Self.UsekStav.zkrat := TBoosterSignal.ok;
     Self.Change(true);
    end;
   Self.Freeze();
   Exit();
  end;

 if (state = TBoosterSignal.error) then
  begin
   // do OR oznamime, ze nastal zkrat, pak se prehraje zvuk v klientech...
   if (not Self.frozen) then
     for i := 0 to Self.ORsRef.Cnt-1 do
      Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt + 1;
  end else begin
   if (Self.UsekStav.zkrat = TBoosterSignal.error) then
     for i := 0 to Self.ORsRef.Cnt-1 do
      Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt - 1;
  end;

 if (Self.UsekStav.zkrat <> state) then
  begin
   Self.UsekStav.zkrat := state;
   Self.Change(true);
  end;
end;//procedure

procedure TBlkUsek.SetZesNap(state:TBoosterSignal);
begin
 Self.UsekStav.napajeni := state;
 Self.Change(true);
end;//procedure

procedure TBlkUsek.SetZesDCC(state:TBoosterSignal);
begin
 if (state = TBoosterSignal.undef) then
   Self.SetCentralaDCC(TrkSystem.status = Ttrk_status.TS_ON)
 else
   Self.SetDCC(state = TBoosterSignal.ok);
end;

procedure TBlkUsek.SetCentralaDCC(state:boolean);
var booster:TBooster;
begin
 booster := Boosters[Self.UsekSettings.Zesil];
 if ((booster = nil) or (not booster.isDCCdetection) or (booster.DCC = TBoosterSignal.undef)) then
   Self.SetDCC(TrkSystem.status = Ttrk_status.TS_ON);
end;

procedure TBlkUsek.SetDCC(state:boolean);
begin
 // tady probiha kontrola old a new
 if (state = Self.Stav.DCC) then Exit();

 // doslo ke zmene DCC
 Self.UsekStav.DCC := state;
 if (state) then
   Self.UsekStav.DCCGoTime := Now
 else
   Self.Freeze();

 Self.Change(true);
end;

procedure TBlkUsek.SetVlakPresun(presun:Integer);
begin
 if (Self.UsekStav.vlakPresun <> presun) then
  begin
   Self.UsekStav.vlakPresun := presun;
   Self.Change();
  end;
end;//procedure

procedure TBlkUsek.Freeze();
begin
 if (Self.frozen) then Exit();

 inherited;
 Self.last_zes_zkrat := Self.ZesZkrat;
end;//procedure

procedure TBlkUsek.UnFreeze();
begin
 if (not Self.frozen) then Exit();

 inherited;
 if (Self.ZesZkrat <> Self.last_zes_zkrat) then
  begin
   Self.ZesZkrat := Self.last_zes_zkrat;
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSettings():TBlkUsekSettings;
begin
 Result := Self.UsekSettings;
end;//function

procedure TBlkUsek.SetSettings(data:TBlkUsekSettings);
var houkEv:THoukEv;
begin
 if (Self.UsekSettings.houkEvL <> data.houkEvL) then
  begin
   for houkEv in Self.UsekSettings.houkEvL do
     houkEv.Free();
   Self.UsekSettings.houkEvL.Free();
  end;

 if (Self.UsekSettings.houkEvS <> data.houkEvS) then
  begin
   for houkEv in Self.UsekSettings.houkEvS do
     houkEv.Free();
   Self.UsekSettings.houkEvS.Free();
  end;

 Self.UsekSettings := data;

 if (not Self.UsekStav.stanicni_kolej) then
   Self.UsekSettings.maxSpr := 1;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.GetObsazeno(var ar:TUsekStavAr);
begin
 ar := Self.UsekStav.StavAr;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//dynamicke funkce:

procedure TBlkUsek.MenuNewLokClick(SenderPnl:TIdContext; SenderOR:TObject; itemindex:Integer);
begin
 // nejdrive posleme aktualni seznam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkNewSpr(Self, SenderPnl, (itemindex-2) div 2);
end;//procedure

procedure TBlkUsek.MenuVLOZLokClick(SenderPnl:TIdContext; SenderOR:TObject; itemindex:Integer);
begin
 Self.PresunLok(SenderPnl, SenderOR, (itemindex-2) div 2);
end;

procedure TBlkUsek.MenuEditLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 // nejdrive posleme aktualni senam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkEditSpr(Self, SenderPnl, Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]]);
end;//procedure

procedure TBlkUsek.MenuDeleteLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 ORTCPServer.Potvr(SenderPnl, Self.PotvrDeleteLok, SenderOR as TOR,
   'Smazání soupravy '+Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].nazev,
   TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUsek.PotvrDeleteLok(Sender:TIdContext; success:boolean);
begin
 if ((TTCPORsRef(Sender.Data).spr_menu_index < 0) or
     (TTCPORsRef(Sender.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 if (success) then
  begin
   if (Self.UsekStav.vlakPresun = TTCPORsRef(Sender.Data).spr_menu_index) then
     Self.UsekStav.VlakPresun := -1;
   Soupravy.RemoveSpr(Self.Soupravs[TTCPORsRef(Sender.Data).spr_menu_index]);
  end;
end;//procedure

procedure TBlkUsek.PotvrUvolLok(Sender:TIdContext; success:boolean);
begin
 if ((TTCPORsRef(Sender.Data).spr_menu_index < 0) or
     (TTCPORsRef(Sender.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 if (not success) then Exit();

 if (Blky.GetBlkWithSpr(Self.Soupravs[TTCPORsRef(Sender.Data).spr_menu_index]).Count = 1) then
  begin
   Soupravy.RemoveSpr(Self.Soupravs[TTCPORsRef(Sender.Data).spr_menu_index]);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava odstranìna');
  end else begin
   Self.RemoveSouprava(Self.Soupravs[TTCPORsRef(Sender.Data).spr_menu_index]);
  end;
end;//procedure

procedure TBlkUsek.MenuUVOLLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 ORTCPServer.Potvr(SenderPnl, Self.PotvrUvolLok, SenderOR as TOR,
  'Uvolnìní soupravy '+Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].nazev+' z bloku',
  TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUsek.MenuVEZMILokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 try
   Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].VezmiVlak();
 except
  on E: Exception do
   begin
    ORTCPServer.BottomError(SenderPnl, 'Vlak se nepodaøilo pøevzít', (SenderOR as TOR).ShortName, 'TECHNOLOGIE');
    Exit();
   end;
 end;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Vlak pøevzat');
end;//procedure

procedure TBlkUsek.MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;//procedure

procedure TBlkUsek.MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.Stav.Vyl);
end;//procedure

// pokud volba nebyla uspesna, vraci false a v tom pripade je vyvolano menu
function TBlkUsek.MenuKCClick(SenderPnl:TIdContext; SenderOR:TObject):boolean;
var Blk:TBlk;
begin
 if ((Self.UsekStav.KonecJC <> TZaver.no) and (not (SenderOR as TOR).vb.Contains(Self))) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit(true);
  end;

 if ((SenderOR as TOR).vb.Contains(Self)) then (SenderOR as TOR).vb.Remove(self);

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk = nil) then Exit(false);

 case ((Blk as TBlkSCom).ZacatekVolba) of
  TBLkSComVolba.VC : Self.UsekStav.KonecJC := TZaver.vlak;
  TBLkSComVolba.PC : Self.UsekStav.KonecJC := TZaver.posun;
  TBLkSComVolba.NC, TBLkSComVolba.PP
                   : Self.UsekStav.KonecJC := TZaver.nouz;
 end;//case

 JCDb.StavJC(Blk, Self, SenderPnl, SenderOR);

 Self.Change();
 Result := true;
end;//procedure

procedure TBlkUsek.MenuVBClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if (Self.UsekStav.KonecJC <> TZaver.no) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit();
  end;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk = nil) then Exit();

 case ((Blk as TBlkSCom).ZacatekVolba) of
  TBLkSComVolba.VC : Self.UsekStav.KonecJC := TZaver.vlak;
  TBLkSComVolba.PC : Self.UsekStav.KonecJC := TZaver.posun;
 else
  Exit();     // nouzova cesta nemuze mit variantni body
 end;

 (SenderOR as TOR).vb.Add(Self);

 Self.Change();
end;//procedure

procedure TBlkUsek.MenuNUZStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.NUZ := true;
end;//procedure

procedure TBlkUsek.MenuNUZStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.NUZ := false;
end;//procedure

procedure TBlkUsek.MenuPRESUNLokClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
var Blk:TBlk;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 if (new_state) then
  begin
   if (Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].stanice <> SenderOR) then
    begin
     ORTCPServer.SendInfoMsg(SenderPnl, 'Loko se nenachází ve vaši oblasti øízení');
     Exit();
    end;

   Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
   if (Blk <> nil) then (Blk as TBlkUsek).VlakPresun := -1;
   Self.VlakPresun := TTCPORsRef(SenderPnl.Data).spr_menu_index;
  end else begin
   Self.VlakPresun := -1;
  end;
end;//procedure

procedure TBlkUsek.MenuRUClokClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
    str:string;
    HV:THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 str := (SenderOR as TOR).id + ';LOK-TOKEN;OK;';
 for i := 0 to Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].sdata.HV.cnt-1 do
  begin
   HV := HVDb.HVozidla[Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].sdata.HV.HVs[i]];
   str := str + '[' + IntToStr(HV.adresa) + '|' + HV.GetToken() + ']';
  end;//for i

 ORTCPServer.SendLn(SenderPnl, str);
end;//procedure

procedure TBlkUsek.MenuMAUSlokClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
    str:string;
    HV:THV;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 str := (SenderOR as TOR).id + ';MAUS;{';
 for i := 0 to Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].sdata.HV.cnt-1 do
  begin
   HV := HVDb.HVozidla[Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]].sdata.HV.HVs[i]];
   str := str + IntToStr(HV.adresa) + '|';
  end;//for i
 str := str + '}';

 ORTCPServer.SendLn(SenderPnl, str);
end;//procedure

procedure TBlkUsek.MenuObsazClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 try
   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
     RCSi.SetInput(Self.UsekSettings.RCSAddrs.data[i].board, Self.UsekSettings.RCSAddrs.data[i].port, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení MTB vstupù!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;//procedure

procedure TBlkUsek.MenuUvolClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 try
   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
     RCSi.SetInput(Self.UsekSettings.RCSAddrs.data[i].board, Self.UsekSettings.RCSAddrs.data[i].port, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení MTB vstupù!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;//procedure

procedure TBlkUsek.MenuHLASENIOdjezdClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();
   TOR(SenderOR).hlaseni.Odjede(Self.GetSHSpr(TTCPORsRef(SenderPnl.Data).spr_menu_index));
 except
   on E:Exception do
    begin
     writelog('Nepodaøilo se spustit stanièní hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se spustit stanièní hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuHLASENIPrijezdClick(SenderPnl:TIdContext; SenderOR:TObject);
var shSpr:TSHSpr;
    blk:TBlkUsek;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();

   shSpr := Self.GetSHSpr(TTCPORsRef(SenderPnl.Data).spr_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]],
      TOR(SenderOR)).stanicniKolej;
   if (blk = nil) then Exit();

   shSpr.kolej := blk.Stav.cislo_koleje;
   TOR(SenderOR).hlaseni.Prijede(shSpr);
 except
   on E:Exception do
    begin
     writelog('Nepodaøilo se spustit stanièní hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se spustit stanièní hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuHLASENIPrujezdClick(SenderPnl:TIdContext; SenderOR:TObject);
var shSpr:TSHSpr;
    blk:TBlkUsek;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index < 0) or
     (TTCPORsRef(SenderPnl.Data).spr_menu_index >= Self.Soupravs.Count)) then Exit();

 try
   if (not Assigned(TOR(SenderOR).hlaseni)) then Exit();

   shSpr := Self.GetSHSpr(TTCPORsRef(SenderPnl.Data).spr_menu_index);
   blk := stanicniHlaseniHelper.CanPlayPrijezdSH(
      Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]],
      TOR(SenderOR)).stanicniKolej;

   if (blk <> nil) then
     shSpr.kolej := blk.Stav.cislo_koleje;
   TOR(SenderOR).hlaseni.Projede(shSpr);
 except
   on E:Exception do
    begin
     writelog('Nepodaøilo se spustit stanièní hlášení : ' + E.Message, WR_ERROR);
     ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se spustit stanièní hlášení!', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
    end;
 end;
end;

procedure TBlkUsek.MenuPOdjClick(SenderPnl:TIdContext; SenderOR:TObject);
var spr:TSouprava;
begin
 if ((TTCPORsRef(SenderPnl.Data).spr_menu_index >= 0) and
     (TTCPORsRef(SenderPnl.Data).spr_menu_index < Self.Soupravs.Count)) then
  begin
   spr := Soupravy[Self.Soupravs[TTCPORsRef(SenderPnl.Data).spr_menu_index]];
  end else begin
   if (Self.SprPredict = -1) then Exit();
   spr := Soupravy[Self.SprPredict];
  end;

 if (spr.IsPOdj(Self)) then
   ORTCPServer.POdj(SenderPnl, Self, spr.index, spr.GetPOdj(Self))
 else
   ORTCPServer.POdj(SenderPnl, Self, spr.index, nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.MenuSOUPRAVA(SenderPnl:TIdContext; SenderOR:TObject; sprLocalI:Integer);
var menu:string;
begin
 TTCPORsRef(SenderPnl.Data).spr_menu_index := sprLocalI;

 menu := '$'+Self.GlobalSettings.name+',';
 menu := menu + '$Souprava ' + Soupravy[Self.Soupravs[sprLocalI]].nazev + ',-,';
 menu := menu + Self.GetSprMenu(SenderPnl, SenderOr, sprLocalI);

 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), menu);
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkUsek.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk:TBlk;
    i, spr:Integer;
    canAdd:boolean;
    addStr:string;
begin
 Result := inherited;

 if (Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id) <> nil) then
   addStr := 'VLOŽ vlak,'
 else
   addStr := 'NOVÝ vlak,';

 if (Self.SoupravyFull() and (Self.Soupravs.Count = 1)) then begin
   Result := Result + Self.GetSprMenu(SenderPnl, SenderOR, 0) + '-,';
   TTCPORsRef(SenderPnl.Data).spr_menu_index := 0;
 end else begin
   canAdd := ((Self.UsekStav.stanicni_kolej) and
              (( (not Self.SoupravyFull()) and ((Self.UsekStav.Stav = TUsekStav.obsazeno) or (Self.UsekSettings.RCSAddrs.Count = 0)) ) or // novy vlak
               ( addStr = 'VLOŽ vlak,' ) // presun vlaku
              ));

   if (canAdd) then
     Result := Result + addStr;

   for spr in Self.Soupravs do
    begin
     Result := Result + Soupravy[spr].nazev + ',';
     if (canAdd) then Result := Result + addStr;
    end;

   if ((canAdd) or (Self.Soupravs.Count > 0)) then
     Result := Result + '-,';
 end;

 if ((Self.SprPredict > -1) and (Self.UsekStav.stanicni_kolej)) then
   Result := Result + 'PODJ,-,';

 Result := Result + 'STIT,VYL,';

 if (((not (SenderOR as TOR).NUZtimer) and (Integer(Self.UsekStav.Zaver) > 0) and (Self.UsekStav.Zaver <> TZaver.staveni)
    and (Self.GetGlobalSettings().typ = _BLK_USEK) and (not Self.UsekStav.stanicni_kolej)) or (rights >= superuser)) then
  begin
   if (Self.UsekStav.NUZ) then
     Result := Result + '-,NUZ<,'
    else
     Result := Result + '-,NUZ>,';
  end;

 //11 = KC
 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   Result := Result + '-,KC,';
   if (not (SenderOR as TOR).vb.Contains(Self)) then
    Result := Result + 'VB,';
  end;

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (RCSi.IsSimulatorMode()) then
  begin
   Result := Result + '-,';

   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
    if (Self.UsekStav.StavAr[i] = TUsekStav.uvolneno) then
     begin
      Result := Result + '*OBSAZ,';
      break;
     end;

   for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
    if (Self.UsekStav.StavAr[i] = TUsekStav.obsazeno) then
     begin
      Result := Result + '*UVOL,';
      break;
     end;
  end;//if RCSi.lib = 2
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSprMenu(SenderPnl:TIdContext; SenderOR:TObject; sprLocalI:Integer):string;
var spr:TSouprava;
    ok:boolean;
    i:Integer;
    shPlay:stanicniHlaseniHelper.TSHToPlay;
begin
 spr := Soupravy[Self.Soupravs[sprLocalI]];

 if (Self.UsekStav.stanicni_kolej) then
  Result := Result + 'EDIT vlak,ZRUŠ vlak,';
 Result := Result + 'UVOL vlak,';

 if (spr.sdata.HV.cnt > 0) then
  begin
   Result := Result + 'RUÈ vlak,';
   if (TTCPORsRef(SenderPnl.Data).maus) then Result := Result + 'MAUS vlak,';
  end;

 if (Self.VlakPresun = sprLocalI) then
  Result := Result + 'PØESUÒ vlak<,'
 else if ((not Self.IsVlakPresun()) and (spr.rychlost = 0) and (spr.stanice = SenderOR)) then
   Result := Result + 'PØESUÒ vlak>,';

 if (spr.ukradeno) then
   Result := Result + 'VEZMI vlak,';

 if (Self.UsekStav.stanicni_kolej) then
   Result := Result + 'PODJ,';

 if ((Assigned(TOR(SenderOR).hlaseni)) and (TOR(SenderOR).hlaseni.available) and
     (spr.vychoziOR <> nil) and (spr.cilovaOR <> nil) and (spr.typ <> '')) then
  begin
   ok := true;
   for i := 0 to Length(stanicniHlaseni._HLASENI_SPRTYP_FORBIDDEN)-1 do
    begin
     if (spr.typ = stanicniHlaseni._HLASENI_SPRTYP_FORBIDDEN[i]) then
      begin
       ok := false;
       break;
      end;
    end;

   if ((Self.UsekStav.stanicni_kolej) and (ok)) then
     Result := Result + 'HLÁŠENÍ odjezd,';

   try
     shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(spr, TOR(SenderOR));
   except
     on E:Exception do
       AppEvents.LogException(E, 'CanPlayPrijezdSH');
   end;

   if ((shPlay.trat = nil) and (shPlay.stanicniKolej <> nil)) then
     Result := Result + 'HLÁŠENÍ pøíjezd,'
   else if (shPlay.trat <> nil) then
     Result := Result + 'HLÁŠENÍ prùjezd,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
var Blk:TBlk;
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 case (Button) of
  F2: Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params);

  ENTER: begin
    if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
      if (((Self.UsekSettings.maxSpr <> 1) and (Self.Soupravs.Count > 0)) or (not Self.PresunLok(SenderPnl, SenderOR, 0))) then
        Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params);
  end;

  F1: begin
    Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
    if (Blk = nil) then
      Self.ShowProperMenu(SenderPnl, (SenderOR as TOR), rights, params)
    else
      Self.MenuVBClick(SenderPnl, SenderOR);
  end;
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkUsek.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
var i:Integer;
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 if (item = 'NOVÝ vlak')           then Self.MenuNewLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'VLOŽ vlak')      then Self.MenuVLOZLokClick(SenderPnl, SenderOR, itemindex)
 else if (item = 'EDIT vlak')      then Self.MenuEditLokClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ vlak')      then Self.MenuDeleteLokClick(SenderPnl, SenderOR)
 else if (item = 'UVOL vlak')      then Self.MenuUVOLLokClick(SenderPnl, SenderOR)
 else if (item = 'VEZMI vlak')     then Self.MenuVEZMILokClick(SenderPnl, SenderOR)
 else if (item = 'PØESUÒ vlak>')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, true)
 else if (item = 'PØESUÒ vlak<')   then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, false)
 else if (item = 'RUÈ vlak')       then Self.MenuRUCLokClick(SenderPnl, SenderOR)
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
 else if (item = 'HLÁŠENÍ pøíjezd')then Self.MenuHLASENIPrijezdClick(SenderPnl, SenderOR)
 else if (item = 'HLÁŠENÍ prùjezd')then Self.MenuHLASENIPrujezdClick(SenderPnl, SenderOR)
 else if (item = 'PODJ')           then Self.MenuPOdjClick(SenderPnl, SenderOR)
 else begin
  // cislo soupravy
  for i := 0 to Self.Soupravs.Count-1 do
    if (item = Soupravy[Self.Soupravs[i]].nazev) then
     begin
      Self.MenuSOUPRAVA(SenderPnl, SenderOR, i);
      break;
     end;
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud volba vyvolala nejaky efekt (false pokud se ma zobrazit menu)
function TBlkUsek.PresunLok(SenderPnl:TIdContext; SenderOR:TObject; sprLocalIndex:Integer):boolean;
var Blk, Nav:TBlk;
    spri:Integer;
begin
 Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
 if (Blk = nil) then Exit(false);

 if (not Self.UsekStav.stanicni_kolej) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Loko lze pøesunout pouze na stanièní kolej!');
   Exit(true);
  end;

 if ((Self.SoupravyFull()) and (Blk <> Self)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Do úseku se již nevejde další souprava!');
   Exit(true);
  end;

 if ((Self.Zaver > TZaver.no) and (Self.Zaver <> TZaver.posun)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze pøesunout na úsek se závìrem!');
   Exit(true);
  end;

 if (not Self.CanSprSpeedInsert(sprLocalIndex)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze vložit soupravu pøed jedoucí soupravu!');
   Exit(true);
  end;

 spri := TBlkUsek(Blk).Soupravs[TBlkUsek(Blk).VlakPresun];

 if (Blk = Self) then
  begin
   Self.UsekStav.soupravy.Insert(sprLocalIndex, spri);

   if (sprLocalIndex <= Self.VlakPresun) then
     Self.UsekStav.soupravy.Delete(Self.VlakPresun+1)
   else
     Self.UsekStav.soupravy.Delete(Self.VlakPresun);

   Self.UsekStav.vlakPresun := -1;
   Self.Change();
  end else begin

   try
     Self.AddSouprava(sprLocalIndex, spri);
     (Blk as TBlkUsek).RemoveSouprava(spri);
   except
     on E:Exception do
      begin
       ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
       Exit(true);
      end;
   end;
  end;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Souprava '+Soupravy.GetSprNameByIndex(spri)+' pøesunuta na '+Self.GlobalSettings.name+'.');

 if (Blky.GetBlkWithSpr(spri).Count = 1) then
   Soupravy[spri].front := Self;

 for nav in (Blk as TBlkUsek).SComJCRef do
   Blky.SprPrediction(Nav);

 if (Blk <> Self) then
   for nav in Self.SComJCRef do
     Blky.SprPrediction(Nav);

 Result := true;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;

 TBlk.RCSstoJSON(Self.UsekSettings.RCSAddrs, json.A['mtb']);

 json['delka'] := Self.UsekSettings.Lenght;
 if (Self.UsekSettings.SmcUsek) then json['smyckaUsek'] := Self.UsekSettings.SmcUsek;
 json['zesilovac'] := Self.UsekSettings.Zesil;

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkUsek.GetPtState(json:TJsonObject);
var i, spr:Integer;
begin
 case (Self.Obsazeno) of
  TUsekStav.disabled : json['stav'] := 'vypnuto';
  TUsekStav.none     : json['stav'] := 'zadny';
  TUsekStav.uvolneno : json['stav'] := 'uvolneno';
  TUsekStav.obsazeno : json['stav'] := 'obsazeno';
 end;

 for i := 0 to Self.UsekSettings.RCSAddrs.Count-1 do
  begin
   case (Self.UsekStav.StavAr[i]) of
    TUsekStav.disabled : json.A['sekce'].Add('vypnuto');
    TUsekStav.none     : json.A['sekce'].Add('zadny');
    TUsekStav.uvolneno : json.A['sekce'].Add('uvolneno');
    TUsekStav.obsazeno : json.A['sekce'].Add('obsazeno');
   end;
  end;

 json['napajeni'] := (Self.ZesNapajeni = TBoosterSignal.ok);
 json['zkrat']    := (Self.ZesZkrat = TBoosterSignal.error);
 json['dcc']      := Self.DCC;

 if (Self.Stitek <> '') then json['stitek'] := Self.Stitek;
 if (Self.Vyluka <> '') then json['vyluka'] := Self.Vyluka;

 for spr in Self.Soupravs do
   json.A['soupravy'].Add(Soupravy[spr].nazev);
end;

procedure TBlkUsek.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
var sprStr:string;
    spr:Integer;
begin
 if (reqJson.Contains('soupravy')) then
  begin
   if (Cardinal(reqJson.A['soupravy'].Count) > Self.UsekSettings.maxSpr) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                           'Nelze pridat vice souprav, nez je limit useku');
     inherited;
     Exit();
    end;

   Self.RemoveSoupravy();
   for sprStr in reqJson.A['soupravy'] do
    begin
     spr := Soupravy.GetSprIndexByName(sprStr);
     if (spr > -1) then
      begin
       try
         Self.AddSoupravaS(spr)
       except
        on E: Exception do
         begin
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                                E.Message);
         end;

       end;
      end else
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request',
                             'Souprava ' + sprStr + ' neexistuje, ignoruji.');
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.LoadHoukEventToList(list:TList<THoukEv>; ini_tech:TMemIniFile; section:string; prefix:string);
var i:Integer;
    data:string;
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
                  Self.GetGlobalSettings.name, WR_ERROR);
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
var list:TList<THoukEv>;
begin
 if (Self.UsekStav.currentHoukEv < 0) then Exit();

 if (not Self.IsSouprava()) then
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

function TBlkUsek.GetHoukEvEnabled():boolean;
begin
 Result := (Self.UsekStav.currentHoukEv > -1);
end;

procedure TBlkUsek.SetHoukEvEnabled(state:boolean);
var houkEv:THoukEv;
begin
 if (state) then
  begin
   if (not Self.IsSouprava()) then Exit();
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

function TBlkUsek.GetHoukList():TList<THoukEv>;
begin
 if (not Self.IsSouprava()) then Exit(nil);

 if (Soupravy[Self.SoupravaL].smer = THVStanoviste.lichy) then
   Result := Self.UsekSettings.houkEvL
 else
   Result := Self.UsekSettings.houkEvS;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSHSpr(sprLocalIndex:Integer):TSHSpr;
begin
 if ((sprLocalIndex < 0) or (sprLocalIndex >= Self.Soupravs.Count)) then Exit();

 Result.cislo    := Soupravy[Self.Soupravs[sprLocalIndex]].nazev;
 Result.typ      := Soupravy[Self.Soupravs[sprLocalIndex]].typ;
 Result.kolej    := Self.UsekStav.cislo_koleje;
 Result.fromORid := TOR(Soupravy[Self.Soupravs[sprLocalIndex]].vychoziOR).id;
 Result.toORid   := TOR(Soupravy[Self.Soupravs[sprLocalIndex]].cilovaOR).id;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.AddNeprofilJC(id:Integer);
begin
 if (Self.UsekStav.neprofilJCcheck.Contains(id)) then Exit();

 Self.UsekStav.neprofilJCcheck.Add(id);
 if (Self.UsekStav.neprofilJCcheck.Count = 1) then
   Self.Change();
end;

procedure TBlkUsek.RemoveNeprofilJC(id:Integer);
begin
 if (not Self.UsekStav.neprofilJCcheck.Contains(id)) then Exit();

 Self.UsekStav.neprofilJCcheck.Remove(id);
 if (Self.UsekStav.neprofilJCcheck.Count = 0) then
   Self.Change();
end;

function TBlkUsek.IsNeprofilJC():boolean;
begin
 Result := (Self.UsekStav.neprofilJCcheck.Count > 0);
end;

procedure TBlkUsek.NeprofilObsaz();
var jcid:Integer;
    jc:TJC;
begin
 for jcid in Self.UsekStav.neprofilJCcheck do
  begin
   jc := JCDb.GetJCByID(jcid);
   if (jc <> nil) then
     jc.NeprofilObsaz();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSoupravaL():Integer;
begin
 if (Self.UsekStav.soupravy.Count < 1) then
   Result := -1
 else
   Result := Self.UsekStav.soupravy[0];
end;

function TBlkUsek.GetSoupravaS():Integer;
begin
 if (Self.UsekStav.soupravy.Count < 1) then
   Result := -1
 else
   Result := Self.UsekStav.soupravy[Self.UsekStav.soupravy.Count-1];
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsSouprava():boolean;
begin
 Result := (Self.UsekStav.soupravy.Count > 0);
end;

function TBlkUsek.IsSouprava(index:Integer):boolean;
begin
 Result := Self.UsekStav.soupravy.Contains(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.AddSoupravaL(index:Integer);
begin
 if (Self.SoupravyFull()) then
   raise ESprFull.Create('Do bloku ' + Self.GetGlobalSettings.name + ' se uz nevejde dalsi souprava!');
 if (Self.Soupravs.Contains(index)) then
   raise EDuplicitSprs.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.UsekStav.soupravy.Insert(0, index);
 Self.UsekStav.SprPredict := -1;
 Self.Change();
end;

procedure TBlkUsek.AddSoupravaS(index:Integer);
begin
 if (Self.SoupravyFull()) then
   raise ESprFull.Create('Do bloku ' + Self.GetGlobalSettings.name + ' se uz nevejde dalsi souprava!');
 if (Self.Soupravs.Contains(index)) then
   raise EDuplicitSprs.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');

 Self.UsekStav.soupravy.Add(index);
 Self.UsekStav.SprPredict := -1;
 Self.Change();
end;

procedure TBlkUsek.AddSouprava(localSprIndex:Integer; souprava:Integer);
begin
 if (Self.SoupravyFull()) then
   raise ESprFull.Create('Do bloku ' + Self.GetGlobalSettings.name + ' se uz nevejde dalsi souprava!');
 if (Self.Soupravs.Contains(souprava)) then
   raise EDuplicitSprs.Create('Nelze pridat jednu soupravu na jeden blok vicekrat!');
 if (not Self.CanSprSpeedInsert(localSprIndex)) then
   raise ERunningSpr.Create('Nelze vložit soupravu pøed jedoucí soupravu!');

 Self.UsekStav.soupravy.Insert(localSprIndex, souprava);
 Self.UsekStav.SprPredict := -1;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.RemoveSoupravy();
var spr:Integer;
begin
 for spr in Self.Soupravs do
   if (Soupravy[spr].IsPOdj(Self)) then
     Soupravy[spr].RemovePOdj(Self);

 Self.UsekStav.soupravy.Clear();
 Self.UsekStav.vlakPresun := -1;
 Self.UsekStav.zpomalovani_ready := false;
 Self.houk_ev_enabled := false;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.RemoveSouprava(index:Integer);
begin
 if (Self.UsekStav.soupravy.Contains(index)) then
  begin
   if ((Self.IsVlakPresun) and (Self.Soupravs[Self.VlakPresun] = index)) then
     Self.UsekStav.vlakPresun := -1;

   Self.UsekStav.soupravy.Remove(index);

   // odstranit predvidany odjezd z aktualniho useku
   if (Soupravy[index].IsPOdj(Self)) then
     Soupravy[index].RemovePOdj(Self);

   Self.Change();
  end else begin
   raise ESprNotExists.Create('Souprava ' + IntToStr(index) +
     ' neexistuje na useku ' + Self.GetGlobalSettings.name);
  end;

 if (not Self.IsSouprava()) then
  begin
   Self.UsekStav.zpomalovani_ready := false;
   Self.houk_ev_enabled := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.SoupravyFull():boolean;
begin
 Result := (Cardinal(Self.UsekStav.soupravy.Count) >= Self.UsekSettings.maxSpr);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsVlakPresun():boolean;
begin
 Result := (Self.Stav.vlakPresun > -1);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetUsekSpr():Integer;
begin
 if (Self.Soupravs.Count = 0) then Exit(-1)
 else if (Self.Soupravs.Count = 1) then Exit(Self.Soupravs[0])
 else raise EMultipleSprs.Create('Usek ' + Self.GetGlobalSettings.name +
   ' obsahuje vice souprav, nelze se proto ptat jen na jednu soupravu!');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.ShowProperMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORControlRights; params:string);
var i:Integer;
begin
 if (params <> '') then begin
   i := StrToIntDef(params, -1);
   if ((i <> -1) and (i >= 0) and (i < Self.Soupravs.Count)) then
     Self.MenuSOUPRAVA(SenderPnl, (SenderOR as TOR), i)
   else
     ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end else
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

// vraci true prave tehdy, kdyz lze vlozit soupravu na pozici index
// kontroluje, zda-li se nenazime vlozit pred soupravu v pohybu
function TBlkUsek.CanSprSpeedInsert(index:Integer):boolean;
begin
 Result := not ((Self.Soupravs.Count > 0) and
                (((index = 0) and (Soupravy[Self.Soupravs[index]].rychlost > 0) and (Soupravy[Self.Soupravs[index]].smer = THVStanoviste.sudy)) or
                 ((index = Self.Soupravs.Count) and (Soupravy[Self.Soupravs[index-1]].rychlost > 0) and (Soupravy[Self.Soupravs[index-1]].smer = THVStanoviste.lichy))));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.PanelPOdj(SenderPnl:TIdContext; sprId:Integer; var podj:TPOdj);
var spr:Integer;
    was:boolean;
    nav:TBlk;
begin
 if ((not Self.Soupravs.Contains(sprId)) and (sprId <> Self.SprPredict)) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Souprava již není na úseku!');
   Exit();
  end;

 was := Soupravy[sprId].IsPOdj(Self);

 for spr in Self.Soupravs do
   if (spr = sprId) then
     podj.RecordOriginNow();

 Soupravy[sprId].AddOrUpdatePOdj(Self, podj);

 if ((was) and (not Soupravy[sprId].IsPOdj(Self))) then
  begin
   // PODJ bylo odstraneno -> rozjet soupravu pred navestidlem i kdyz neni na zastavovaci udalosti
   // aktuiualziaci rychlosti pro vsechny SComJCReg bychom nemeli nic pokazit
   for nav in Self.SComJCRef do
     TBlkSCom(nav).UpdateRychlostSpr(true);
  end;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.CheckPOdjChanged();
var spr:Integer;
    shouldChange:boolean;
    podj:TPOdj;
    i:Integer;
    Nav:TBlk;
begin
 shouldChange := false;

 for spr in Self.Soupravs do
  begin
   if ((Soupravy[spr].IsPOdj(Self)) and (Soupravy[spr].GetPOdj(Self).changed)) then
    begin
     podj := Soupravy[spr].GetPOdj(Self);

     // prehravani zvukove vystrahy
     if ((not shouldChange) and (Self.IsStujForSpr(spr))) then
      begin
        if ((podj.phase_old = ppPreparing) and (podj.GetPhase() = ppGoingToLeave)) then
          for i := 0 to Self.ORsRef.Cnt-1 do
            Self.ORsRef.ORs[i].BlkPlaySound(Self, TORControlRights.write, _SND_STAVENI_VYZVA)

        else if ((podj.phase_old = ppGoingToLeave) and (podj.GetPhase() = ppSoundLeave)) then
          for i := 0 to Self.ORsRef.Cnt-1 do
            Self.ORsRef.ORs[i].BlkPlaySound(Self, TORControlRights.write, _SND_NENI_JC);
      end;

     if (Soupravy[spr].GetPOdj(Self).DepRealDelta() < 0) then
      begin
       Soupravy[spr].RemovePOdj(Self);

       // tvrda aktualizace rychlosti soupravy
       if ((spr = Self.SoupravaL) or (spr = Self.SoupravaS)) then
        begin
         for nav in Self.SComJCRef do
           if (((TBlkSCom(nav).Smer = THVStanoviste.sudy) and (spr = Self.SoupravaL)) or
               ((TBlkSCom(nav).Smer = THVStanoviste.lichy) and (spr = Self.SoupravaS))) then
             TBlkSCom(nav).UpdateRychlostSpr(true);
        end;
      end else
       Soupravy[spr].GetPOdj(Self).changed := false;

     shouldChange := true;
    end;
  end;

 if (Self.SprPredict > -1) then
  begin
   spr := Self.SprPredict;
   if ((Soupravy[spr].IsPOdj(Self)) and (Soupravy[spr].GetPOdj(Self).changed)) then
    begin
     if (Soupravy[spr].GetPOdj(Self).DepRealDelta() < 0) then
       Soupravy[spr].RemovePOdj(Self)
     else
       Soupravy[spr].GetPOdj(Self).changed := false;

     shouldChange := true;
    end;
  end;

 if (shouldChange) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// predvidane odjezdy se mazou pres bloky, aby bloky zavolaly Change().
procedure TBlkUsek.ClearPOdj();
var spr:Integer;
    change:boolean;
begin
 change := false;

 for spr in Self.Soupravs do
  begin
   if (Soupravy[spr].IsPOdj(Self)) then
    begin
     Soupravy[spr].RemovePOdj(Self);
     change := true;
    end;
  end;

 if ((Self.SprPredict > -1) and (Soupravy[Self.SprPredict].IsPOdj(Self))) then
  begin
   Soupravy[Self.SprPredict].RemovePOdj(Self);
   change := true;
  end;

 if (change) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.IsStujForSpr(spr:Integer):boolean;
begin
 if (not Self.Soupravs.Contains(spr)) then Exit(false);
 if (Self.SComJCRef.Count = 0) then Exit(true);

 if (Self.SComJCRef.Count = 1) then
  begin
   if (not TBlkSCom(Self.SComJCRef[0]).IsPovolovaciNavest()) then Exit(true);
   if (TBlkSCom(Self.SComJCRef[0]).Smer = THvStanoviste.lichy) then
     Exit(spr <> Self.SoupravaS)
   else
     Exit(spr <> Self.SoupravaL);
  end;

 if (Self.SComJCRef.Count = 2) then
  begin
   if ((Self.Soupravs.Count = 1) and (not TBlkSCom(Self.SComJCRef[0]).IsPovolovaciNavest()) and
        (not TBlkSCom(Self.SComJCRef[1]).IsPovolovaciNavest())) then Exit(true);
   if ((Self.Soupravs.Count >= 2) and ((not TBlkSCom(Self.SComJCRef[0]).IsPovolovaciNavest()) or
        (not TBlkSCom(Self.SComJCRef[1]).IsPovolovaciNavest()))) then Exit(true);
   if ((Self.Soupravs.Count > 2) and (Self.SoupravaL <> spr) and (Self.SoupravaS <> spr)) then Exit(true);
 end;

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

