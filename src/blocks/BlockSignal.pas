﻿unit BlockSignal;

{ SIGNAL technological block definition. }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, rrEvent, System.Math,
  TechnologieJC, IdContext, Generics.Collections, THnaciVozidlo, JCBarriers,
  Area, StrUtils, JsonDataObjects, TechnologieRCS, Train, RegularExpressions;

type
  TBlkSignalSelection = (none = 0, VC = 1, PC = 2, NC = 3, PP = 4);
  TBlkSignalOutputType = (scom = 0, binary = 1);
  TBlkSignalSymbol = (unknown = -1, main = 0, shunting = 1);
  TBlkSignalCode = (
    ncChanging = -2,
    ncDisabled = -1,
    ncStuj = 0,
    ncVolno = 1,
    ncVystraha = 2,
    ncOcek40 = 3,
    ncVolno40 = 4,
    ncVse = 5,
    ncVystraha40 = 6,
    nc40Ocek40 = 7,
    ncPrivol = 8,
    ncPosunZaj = 9,
    ncPosunNezaj = 10,
    ncOpakVolno = 11,
    ncOpakVystraha = 12,
    ncZhasnuto = 13,
    ncOpakOcek40 = 14,
    ncOpakVystraha40 = 15,
    ncOpak40Ocek40 = 16
  );
  TBlkSignalInRailway = (
    rwNone = 0,
    rwAutoblok = 1,
    rwHradlo = 2
  );

  ENoEvents = class(Exception);

  // zastavovaci a zpomalovaci udalost pro jeden typ soupravy a jeden rozsah delek soupravy
  TBlkSignalTrainEvent = class
    train_type_re: string;

    length: record // tato udalost je brana v potaz, pokud ma souprava delku > min && < max
      min: Integer;
      max: Integer;
    end;

    stop: TRREv; // zastavovaci udalost

    slow: record // zpomalovaci udalost
      enabled: Boolean; // povolena zpomalovaci udalost?
      speed: Integer; // rychlost z km/h (40, 50, 60...)
      ev: TRREv; // udalost
    end;

    constructor Create(); overload;
    constructor Create(str: string; old: Boolean); overload;
    destructor Destroy(); override;

    procedure Parse(str: string; old: Boolean);
    class function ParseTrainTypes(types: string): string;
    function ToFileStr(short: Boolean = false): string;
    class function ParseOldRychEvent(trackAllowed: Boolean; str: string): TRREv;

    function Match(train: TTrain): Boolean;
  end;

  TBlkSignalSettings = record
    RCSAddrs: TRCSAddrs;
    outputType: TBlkSignalOutputType;
    events: TObjectList<TBlkSignalTrainEvent>;
    // tady jsou ulozena veskera zastavovani a zpomalovani; zastaveni na indexu 0 je vzdy primarni
    // program si pamatuje vice zastavovacich a zpomalovaich udalosti pro ruzne typy a delky soupravy
    fallDelay: Integer; // zpozdeni padu navestidla v sekundach (standartne 0)
    changeTime: TTime; // cas zmeny navesti
    locked: Boolean; // jestli je navestidlo trvale zamknuto na STUJ (hodi se napr. u navestidel a konci kusych koleji)
    PSt: record
      enabled: Boolean;
      rcsIndicationShunt: TRCSAddr;
      rcsControllerShunt: TRCSAddr;
    end;
  end;

  TBlkSignalState = record
    signal, signalOld: TBlkSignalCode;
    selected: TBlkSignalSelection; // zacatek volby jidni cesty
    beginAB: Boolean; // jestli je zacatek volby JC v rezimu AB
    targetSignal: TBlkSignalCode; // navest, ktera ma byt nastavena
    ABJC: TJC; // odkaz na automaticky stavenou JC
    ZAM: Boolean; // navestidlo zamkle z panelu
    dnJC, privolJC: TJC; // reference na aktualni JC na navestidle (resp. NC)

    falling: Boolean; // zde je true, pokud navestidlo pada do STUJ (ma zpozdeny pad) a jeste nespadlo
    fallingStart: TDateTime;

    privolStart: TDateTime; // start privolavaci navesti (privolavacka sviti pouze omezeny cas a pak se vypne)
    privolTimerId: Integer; // id timeru ukonceni privolavacky v panelu, ze ktreho byla JC postavena
    inRailway: TBlkSignalInRailway;

    toRnz: TDictionary<Integer, Cardinal>; // seznam bloku k RNZ spolu s pocty ruseni, ktere je treba udelat
    changeCallbackOk, changeCallbackErr: TNotifyEvent; // notifikace o nastaveni polohy navestidla
    changeEnd: TTime;
    psts: TList<TBlk>;
    dnArea: TObject;
  end;

  TBlkSignalSpnl = record
    symbolType: TBlkSignalSymbol;
    trackId: Integer; // ID useku pred navestidlem
    direction: THVSite;
  end;

  TBlkSignal = class(TBlk)
  const
    _def_signal_state: TBlkSignalState = (signal: ncDisabled; selected: none; beginAB: false; targetSignal: ncDisabled;
      ABJC: nil; ZAM: false; dnJC: nil; privolJC: nil; privolTimerId: 0; inRailway: rwNone;
      changeCallbackOk: nil; changeCallbackErr: nil; dnArea: nil; );

    // doba sviceni privolavaci navesti
    _PRIVOL_SEC = 90;

    _SIG_DEFAULT_DELAY = 2;
    _SIG_DEFAULT_CHANGE_DELAY_OUTPUT = '1.0'; // seconds, same format as in ini file
    _SIG_DEFAULT_CHANGE_DELAY_NO_OUTPUT = '0.2'; // seconds, same format as in ini file

  private
    m_trackId: TBlk;
    m_lastEvIndex: Integer;
    m_groupMaster: TBlk;

    function IsEnabled(): Boolean;

    procedure mSetSignal(signal: TBlkSignalCode);

    function GetAB(): Boolean;
    procedure SetAB(ab: Boolean);
    procedure SetABJC(ab: TJC);

    procedure SetSelected(typ: TBlkSignalSelection);
    procedure SetZAM(ZAM: Boolean);

    procedure MenuVCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTUJClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuDNClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRCClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuABStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuABStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuLockClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUnlockClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPNStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPNStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPStartClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPStopClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPPNClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuRNZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuKCDKClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure MenuAdminStopIR(SenderPnl: TIdContext; SenderOR: TObject; enabled: Boolean);
    procedure MenuAdminRadOnClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminRadOffClick(SenderPnl: TIDContext; SenderOR: TObject);

    procedure DNCSCallback(Sender: TIDContext; success: Boolean);
    procedure PNStartMain(SenderPnl: TIdContext; SenderOR: TObject);
    procedure PNStartHradlo(SenderPnl: TIdContext; SenderOR: TObject);
    procedure PNHradloConfSeq(Sender: TIdContext; success: Boolean);

    procedure UpdateFalling();
    procedure UpdatePrivol();
    procedure UpdateSignalSet();
    procedure OnSignalSetOk();
    procedure OnSignalSetError();

    procedure PNDKClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton);
    procedure PNDKConfSeq(Sender: TIdContext; success: Boolean);
    procedure PNBarriersConfirmed(Sender: TObject);
    procedure PNBarriersRejected(Sender: TObject);
    procedure RNZPotvrSekv(Sender: TIdContext; success: Boolean);

    function GetTrackId(): TBlk;
    procedure SetTrackId(new_id: Integer);

    function CurrentEventIndex(): Integer;
    function CanIDoRNZ(): Boolean;
    function CanSTUJ(): Boolean;

    procedure UnregisterAllEvents();
    function IsChanging(): Boolean;
    function GetTargetSignal(): TBlkSignalCode;

    procedure PstCheckActive();
    procedure ShowIndication();
    procedure ReadContollers();
    function DefaultChangeTime(): string; overload;

  protected
    m_settings: TBlkSignalSettings;
    m_state: TBlkSignalState;
    m_spnl: TBlkSignalSpnl;

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    function IsGoSignal(jctype: TJCType = TJCType.Train): Boolean; overload;
    function IsTargetGoSignal(jctype: TJCType = TJCType.Train): Boolean;
    function IsOpakVystraha(): Boolean;
    class function IsGoSignal(Navest: TBlkSignalCode; jctype: TJCType = TJCType.Train): Boolean; overload;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    procedure JCCancelSignal(); // zahrnuje cas na pad navesti
    procedure SetSignal(code: TBlkSignalCode; changeCallbackOk: TNotifyEvent = nil;
      changeCallbackErr: TNotifyEvent = nil);

    // ----- Signal specific functions -----

    function GetSettings(): TBlkSignalSettings;
    procedure SetSettings(data: TBlkSignalSettings);

    procedure UpdateTrainSpeed(force: Boolean = false);
    procedure AddBlkToRnz(blkId: Integer; Change: Boolean = true);
    procedure RemoveBlkFromRnz(blkId: Integer);
    function FourtyKmph(): Boolean;
    class function AddOpak(code: TBlkSignalCode): TBlkSignalCode;

    function GetTrain(track: TBlk = nil): TTrain;
    procedure PropagatePOdjToRailway();
    function IsSpecificChangeTime(): Boolean;

    class function SignalToString(code: TBlkSignalCode): string;
    class function DefaultChangeTime(hasRCSoutput: Boolean): string; overload;

    procedure PstAdd(pst: TBlk);
    procedure PstRemove(pst: TBlk);
    function PstIsActive(): Boolean;
    function PstIs(): Boolean;
    function ControllerInBasicPosition(): Boolean;

    property symbolType: TBlkSignalSymbol read m_spnl.symbolType;
    property trackId: Integer read m_spnl.trackId write SetTrackId;
    property direction: THVSite read m_spnl.direction write m_spnl.direction;

    property state: TBlkSignalState read m_state;
    property signal: TBlkSignalCode read m_state.signal write mSetSignal;
    property targetSignal: TBlkSignalCode read GetTargetSignal;
    property selected: TBlkSignalSelection read m_state.selected write SetSelected;
    property beginAB: Boolean read m_state.beginAB;
    property ab: Boolean read GetAB write SetAB;
    property ABJC: TJC read m_state.ABJC write SetABJC;
    property ZAM: Boolean read m_state.ZAM write SetZAM;
    property lichy: THVSite read m_spnl.direction;
    property dnJC: TJC read m_state.dnJC write m_state.dnJC;
    property privol: TJC read m_state.privolJC write m_state.privolJC;
    property track: TBlk read GetTrackId;
    property inRailway: TBlkSignalInRailway read m_state.inRailway write m_state.inRailway;
    property canRNZ: Boolean read CanIDoRNZ;
    property changing: Boolean read IsChanging;
    property enabled: Boolean read IsEnabled;
    property groupMaster: TBlk read m_groupMaster write m_groupMaster;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

  // format dat Navu v souboru *.ini:
  // ev=udalosti zastavovani a zpomalovani
  // OutType=typ vystupu (scom, binarni)
  // zamknuti=zamknuti navestidla trvale do STUJ

  // format ev: (ev1)(ev2)(ev3)
  // format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|re: train_typ_regexp|min_delka|max_delka
  // train_typ, min_delka a max_delka jsou u eventu 0 (globalniho eventu) vynechany
  // vsechny dalsi eventy jsou specificke -> vyse zminene informace v nich jsou ulozeny

  // format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
  // typ_zastaveni(0=usek;1=ir);
  // pro usek nasleduje: usekid;usekpart;speed;
  // pro ir nasleduje: irid;speed;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses BlockDb, BlockTrack, TJCDatabase, TCPServerPanel, Graphics, BlockGroupSignal,
  GetSystems, Logging, TrainDb, BlockIR, AreaStack, ownStrUtils, BlockPst,
  BlockRailwayTrack, BlockRailway, BlockTurnout, BlockLock, TechnologieAB,
  predvidanyOdjezd, ownConvert, RCS, IfThenElse, RCSErrors, UPO, PanelConnData,
  TrainSpeed, ConfSeq, PTUtils, timeHelper, Config, colorHelper;

constructor TBlkSignal.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btSignal;
  Self.m_state := Self._def_signal_state;
  Self.m_state.toRnz := TDictionary<Integer, Cardinal>.Create();
  Self.m_settings.events := TObjectList<TBlkSignalTrainEvent>.Create();
  Self.m_trackId := nil;
  Self.m_groupMaster := nil;
  Self.m_state.psts := TList<TBlk>.Create();
end;

destructor TBlkSignal.Destroy();
begin
  Self.m_state.toRnz.Free();
  Self.m_settings.events.Free();
  Self.m_state.psts.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);

  Self.m_settings.locked := ini_tech.ReadBool(section, 'zamknuti', false);

  Self.m_settings.outputType := TBlkSignalOutputType(ini_tech.ReadInteger(section, 'OutType', 0));
  Self.m_settings.fallDelay := ini_tech.ReadInteger(section, 'zpoz', _SIG_DEFAULT_DELAY);
  Self.m_settings.changeTime := ownConvert.SecTenthsToTime(ini_tech.ReadString(section, 'changeTime', Self.DefaultChangeTime()));

  var strs: TStrings := Self.LoadAreas(ini_rel, 'N');
  try
    if (strs.Count >= 3) then
    begin
      Self.m_spnl.symbolType := TBlkSignalSymbol(StrToInt(strs[1]));

      // 0 = navestidlo v lichem smeru. 1 = navestidlo v sudem smeru
      if (strs[2] = '0') then
        Self.m_spnl.direction := THVSite.odd
      else
        Self.m_spnl.direction := THVSite.even;
      Self.m_spnl.trackId := StrToInt(strs[3]);
    end else begin
      Self.m_spnl.symbolType := TBlkSignalSymbol.unknown;
      Self.m_spnl.direction := THVSite.odd;
      Self.m_spnl.trackId := -1;
    end;
  finally
    strs.Free();
  end;

  // Nacitani zastavovacich a zpomaovacich udalosti
  // toto musi byt az po nacteni spnl
  Self.m_settings.events.Clear();

  var str: string := ini_tech.ReadString(section, 'ev', '');
  if (str <> '') then
  begin
    // 1) stary zpusob nacitani zastavovacich udalosti (vsechny udalosti ve starem
    // formatu na jednom radku). Tohle je tady hlavne kvuli zpetne kompatibilite.
    strs := TStringList.Create();
    try
      ExtractStrings(['(', ')'], [], PChar(str), strs);
      for str in strs do
      begin
        try
          Self.m_settings.events.Add(TBlkSignalTrainEvent.Create(str, true));
        except

        end;
      end;
    finally
      strs.Free();
    end;

  end else begin
    // 2) novy zpusob nacitani zastavovacich udalosti
    // kazda udalost na samostatnem radku ev1=... ev2=... ...
    var i: Integer := 0;
    str := ini_tech.ReadString(section, 'ev' + IntToStr(i), '');
    while (str <> '') do
    begin
      try
        Self.m_settings.events.Add(TBlkSignalTrainEvent.Create(str, false));
      except

      end;
      Inc(i);
      str := ini_tech.ReadString(section, 'ev' + IntToStr(i), '');
    end;
  end;

  Self.m_settings.PSt.enabled := (ini_tech.ReadString(section, 'indShunt', '') <> '');
  if (Self.m_settings.PSt.enabled) then
  begin
    Self.m_settings.PSt.rcsIndicationShunt.Load(ini_tech.ReadString(section, 'indShunt', ''));
    Self.m_settings.PSt.rcsControllerShunt.Load(ini_tech.ReadString(section, 'contShunt', ''));
    Self.RCSRegister(Self.m_settings.PSt.rcsIndicationShunt);
    Self.RCSRegister(Self.m_settings.PSt.rcsControllerShunt);
  end;

  Self.RCSRegister(Self.m_settings.RCSAddrs);
end;

procedure TBlkSignal.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);

  for var i: Integer := 0 to Self.m_settings.events.Count - 1 do
    ini_tech.WriteString(section, 'ev' + IntToStr(i), Self.m_settings.events[i].ToFileStr(i = 0));

  if (Self.m_settings.RCSAddrs.Count > 0) then
    ini_tech.WriteInteger(section, 'OutType', Integer(Self.m_settings.outputType));

  if (Self.m_settings.fallDelay <> _SIG_DEFAULT_DELAY) then
    ini_tech.WriteInteger(section, 'zpoz', Self.m_settings.fallDelay);

  if (Self.IsSpecificChangeTime()) then
    ini_tech.WriteString(section, 'changeTime', ownConvert.TimeToSecTenths(Self.m_settings.changeTime));

  if (Self.m_settings.locked) then
    ini_tech.WriteBool(section, 'zamknuti', Self.m_settings.locked);

  if (Self.m_settings.PSt.enabled) then
   begin
    ini_tech.WriteString(section, 'indShunt', Self.m_settings.PSt.rcsIndicationShunt.ToString());
    ini_tech.WriteString(section, 'contShunt', Self.m_settings.Pst.rcsControllerShunt.ToString());
   end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Enable();
var enable: Boolean;
begin
  if (Self.signal <> ncDisabled) then
    Exit(); // skip already enabled block

  enable := true;
  try
    for var rcsaddr: TRCSAddr in Self.m_settings.RCSAddrs do
      if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
        enable := false;
  except
    enable := false;
  end;

  if (enable) then
  begin
    Self.m_state.signal := ncStuj;
    Self.m_state.signalOld := ncStuj;
  end;

  Self.m_state.toRnz.Clear();
  Self.UnregisterAllEvents();
  Self.m_state.psts.Clear();
  Self.Change();
end;

procedure TBlkSignal.Disable();
begin
  Self.m_state.signal := ncDisabled;
  Self.m_state.signalOld := ncDisabled;
  Self.m_state.selected := TBlkSignalSelection.none;
  Self.ab := false;
  Self.m_state.ZAM := false;
  Self.m_state.toRnz.Clear();
  Self.UnregisterAllEvents();
  Self.m_state.psts.Clear();
  Self.Change(true);
end;

function TBlkSignal.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Contains(addr)));

  if ((portType = TRCSIOType.input) and (Self.m_settings.PSt.enabled) and
      (addr = Self.m_settings.PSt.rcsControllerShunt)) then
    Exit(true);

  if ((portType = TRCSIOType.output) and (Self.m_settings.PSt.enabled) and
      (addr = Self.m_settings.PSt.rcsIndicationShunt)) then
    Exit(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Update();
begin
  Self.UpdateSignalSet();
  Self.UpdateFalling();
  Self.UpdateTrainSpeed();

  if (Self.signal = ncPrivol) then
    Self.UpdatePrivol();

  if (Self.m_settings.RCSAddrs.Count > 0) then
  begin
    if (RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].board)) then
    begin
      if (Self.m_state.signal = ncDisabled) then
      begin
        Self.m_state.signal := ncStuj;
        Self.Change();
      end;
    end else begin
      if (Self.changing) then
        Self.OnSignalSetError();

      if (Self.m_state.signal >= ncStuj) then
      begin
        Self.m_state.signal := ncDisabled;
        JCDb.Cancel(Self);
        Self.Change();
      end;
    end;
  end;

  Self.ReadContollers();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.Change(now: Boolean = false);
begin
  // zmenu navesti propagujeme do prilehle trati, kde by mohlo dojit ke zmene
  // navesti autobloku
  if ((Self.track <> nil) and (Self.track.typ = btRT) and (TBlkRT(Self.track).inRailway > -1)) then
    Self.track.Change();

  Self.ShowIndication();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetSettings(): TBlkSignalSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkSignal.SetSettings(data: TBlkSignalSettings);
begin
  if (Self.m_settings.events <> data.events) then
    Self.m_settings.events.Free();

  if (Self.m_settings.RCSAddrs <> data.RCSAddrs) then
    Self.m_settings.RCSAddrs.Free();

  Self.m_settings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.SetSignal(code: TBlkSignalCode; changeCallbackOk: TNotifyEvent = nil;
  changeCallbackErr: TNotifyEvent = nil);
begin
  if ((Self.m_state.signal = ncDisabled) or (Self.m_settings.locked)) then
  begin
    if (Assigned(changeCallbackErr)) then
      changeCallbackErr(Self);
    Exit();
  end;

  if (code = ncPrivol) then
    Self.m_state.privolStart := now;

  if ((code = ncPrivol) or (code = ncStuj)) then
  begin
    // prodlouzeni nebo zruseni privolavaci navesti -> zrusit odpocet v panelu
    if (Self.m_state.privolTimerId > 0) then
      for var area: TArea in Self.m_areas do
      begin
        Area.BroadcastGlobalData('INFO-TIMER-RM;' + IntToStr(Self.m_state.privolTimerId));
        Area.TimerCnt := Area.TimerCnt - 1;
      end;
    Self.m_state.privolTimerId := 0;
  end;

  if ((Self.m_state.signal = code) or ((Self.changing) and (Self.m_state.targetSignal = code))) then
  begin
    if (Assigned(changeCallbackOk)) then
      changeCallbackOk(Self);
    Exit();
  end;

  var prevTargetSignal := Self.targetSignal;
  if (not Self.changing) then
    Self.m_state.signalOld := Self.signal;
  Self.m_state.changeCallbackOk := changeCallbackOk;
  Self.m_state.changeCallbackErr := changeCallbackErr;
  Self.m_state.signal := ncChanging;
  Self.m_state.targetSignal := code;

  // nastaveni vystupu
  try
    case (Self.m_settings.outputType) of
      TBlkSignalOutputType.scom:
        RCSi.SetOutputs(Self.m_settings.RCSAddrs, Integer(code));
      TBlkSignalOutputType.binary:
        RCSi.SetOutputs(Self.m_settings.RCSAddrs, Integer(TBlkSignal.IsGoSignal(code, TJCType.train)));
    else
      RCSi.SetOutputs(Self.m_settings.RCSAddrs, Integer(TBlkSignalCode.ncStuj));
    end;
  except
    if (Assigned(changeCallbackErr)) then
      changeCallbackErr(Self);
    Self.m_state.changeCallbackOk := nil;
    Self.m_state.changeCallbackErr := nil;
    Self.m_state.signal := prevTargetSignal;
    Self.m_state.targetSignal := prevTargetSignal;
    Exit();
  end;

  // propagace do skupinoveho navestidla
  if (Self.groupMaster <> nil) then
    TBlkGroupSignal(Self.groupMaster).SetSignal(code);

  // ruseni nouzove jizdni cesty pri padu navestidla do STUJ
  if (code = ncStuj) then
  begin
    if ((Self.track <> nil) and ((Self.track.typ = btTrack) or (Self.track.typ = btRT)) and
      ((Self.track as TBlkTrack).signalJCRef.Contains(Self))) then
      (Self.track as TBlkTrack).signalJCRef.Remove(Self);

    if (Assigned(Self.privol)) then
    begin
      var tmpjc: TJC := Self.privol;
      Self.privol := nil; // reset first to allow potential reetrability
      tmpjc.CancelWithoutTrackRelease();
    end;
  end;

  if ((prevTargetSignal = ncPrivol) and (code = ncStuj)) then
  begin
    // STUJ po privolavacce -> vypnout zvukovou vyzvu
    Self.Log('Zhasnuta PN', TLogLevel.llInfo);
    for var area: TArea in Self.m_areas do
      Area.pnBlkCnt := area.pnBlkCnt - 1;
  end;

  Self.m_state.changeEnd := now + Self.m_settings.changeTime;

  if (not TBlkSignal.IsGoSignal(Self.m_state.targetSignal)) then // zastavujeme ihned
    Self.UpdateTrainSpeed(true);

  if (Self.track <> nil) then
  begin
    for var traini: Integer in TBlkTrack(Self.track).trains do
      trains[traini].OnPredictedSignalChange();
    if (TBlkTrack(Self.track).trainPredict <> nil) then
      TBlkTrack(Self.track).trainPredict.OnPredictedSignalChange();
  end;

  Self.Change();
end;

procedure TBlkSignal.mSetSignal(signal: TBlkSignalCode);
begin
  Self.SetSignal(signal, TNotifyEvent(nil), TNotifyEvent(nil));
end;

procedure TBlkSignal.OnSignalSetOk();
begin
  Self.m_state.signal := Self.m_state.targetSignal;

  if (Self.m_state.targetSignal = ncPrivol) then
  begin
    // nova navest je privolavacka -> zapnout zvukovou vyzvu
    for var area: TArea in Self.m_areas do
      Area.pnBlkCnt := Area.pnBlkCnt + 1;
  end;

  if (Self.inRailway <> rwNone) then
  begin
    if (TBlkRT(Self.track).nextRT <> nil) then
      TBlkRT(Self.track).nextRT.Change();
    if (TBlkRT(Self.track).railway <> nil) then
      TBlkRailway(TBlkRT(Self.track).railway).UpdateTrainPredict();
  end;

  if (Assigned(Self.m_state.changeCallbackOk)) then
  begin
    var tmp: TNotifyEvent := Self.m_state.changeCallbackOk; // may set new callback in event
    Self.m_state.changeCallbackOk := nil;
    tmp(Self);
  end;

  Self.UpdateTrainSpeed(true);
  JCDb.UpdatePrevSignal(Self);
  Self.Change();
end;

procedure TBlkSignal.OnSignalSetError();
begin
  // Tato funkce zatim neni moc vyuzivana, jedna se o pripravu do budoucna, kdy
  // by melo navestidlo kontrolu navesti (vstup do hJOP).

  if (Assigned(Self.m_state.changeCallbackErr)) then
  begin
    var tmp: TNotifyEvent := Self.m_state.changeCallbackErr; // may set new callback in event
    Self.m_state.changeCallbackErr := nil;
    tmp(Self);
  end;

  // Jak nastavit aktualni navest?
  Self.m_state.signal := ncStuj;

  Self.Change();
end;

procedure TBlkSignal.UpdateSignalSet();
begin
  if (not Self.changing) then
    Exit();
  if (Self.m_state.changeEnd <= now) then
    Self.OnSignalSetOk();
end;

function TBlkSignal.GetAB(): Boolean;
begin
  Result := (Self.m_state.ABJC <> nil);
end;

procedure TBlkSignal.SetABJC(ab: TJC);
begin
  if ((ab <> nil) and (Self.ABJC <> nil)) then
    raise EInvalidOperation.Create('Cannot change AB JC, can only enable/disable AB JC!');

  if ((Self.ABJC <> nil) and (ab = nil)) then
  begin
    try
      if (Assigned(ABlist)) then
        ABlist.Remove(Self.ABJC);
    except
      on E: EABJCNotInList do
      begin
      end; // ignore exception
    end;
    Self.m_state.ABJC := nil;
    Self.Change();
  end else if ((Self.ABJC = nil) and (ab <> nil)) then
  begin
    try
      ABlist.Add(ab);
    except
      on E: EABJCAlreadyInList do
      begin
      end; // ignore exception
    end;
    Self.m_state.ABJC := ab;
    Self.Change();
  end;
end;

procedure TBlkSignal.SetAB(ab: Boolean);
begin
  if (ab) then
    raise EInvalidOperation.Create('You can only enable AB via SetABJC!');

  if (Self.ab and (not ab)) then
    Self.ABJC := nil;
end;

procedure TBlkSignal.SetSelected(typ: TBlkSignalSelection);
begin
  if (Self.m_state.selected = typ) then
    Exit();
  Self.m_state.selected := typ;
  if (typ = TBlkSignalSelection.none) then
    Self.m_state.beginAB := false;
  Self.Change();
end;

procedure TBlkSignal.SetZAM(ZAM: Boolean);
begin
  if (Self.m_state.ZAM = ZAM) then
    Exit();
  Self.m_state.ZAM := ZAM;

  if (Self.dnJC <> nil) then
  begin
    if (ZAM) then
    begin
      if (Self.dnJC.destroyBlock <= 0) then
      begin
        Self.dnJC.destroyBlock := -2;
        Self.dnJC.STUJ();
      end;
    end else begin
      if ((Self.dnJC.destroyBlock = -2) and (not Self.dnJC.cancelling)) then
      begin
        Self.dnJC.destroyBlock := -1;
        Self.dnJC.DN(nil, nil);
      end;
    end;
    Blocks.TrainPrediction(Self);
  end;

  if ((Self.inRailway <> rwNone) and (not ZAM) and (Self.track <> nil) and (TBlkRT(Self.track).railway <> nil)) then
    TBlkRailway(TBlkRT(Self.track).railway).ChangeTracks();

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////
// gui: menu
// dynamicke funkce

procedure TBlkSignal.MenuVCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) then
    Exit();
  if ((SenderOR as TArea).stack.mode = PV) then
    if (((Self.dnJC <> nil) and (Self.dnJC.destroyEndBlock < 1)) or (JCDb.FindJCActivating(Self.id) <> nil)) then
      Exit();

  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);
  Self.selected := TBlkSignalSelection.VC;
  Self.m_state.beginAB := false;
end;

procedure TBlkSignal.MenuVCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.selected := TBlkSignalSelection.none;
  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
end;

procedure TBlkSignal.MenuPCStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((SenderOR as TArea).stack.mode = PV) then
    if (((Self.dnJC <> nil) and (Self.dnJC.destroyEndBlock < 1)) or (JCDb.FindJCActivating(Self.id) <> nil)) then
      Exit();

  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);

  Self.selected := TBlkSignalSelection.PC;
  Self.m_state.beginAB := false;
end;

procedure TBlkSignal.MenuPCStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.selected := TBlkSignalSelection.none;
  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
end;

procedure TBlkSignal.MenuSTUJClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  // poradi musi byt zachovano !
  Self.signal := ncStuj;
  if (Self.dnJC = nil) then
    Exit();

  Self.dnJC.STUJ();
  Blocks.TrainPrediction(Self);
end;

procedure TBlkSignal.MenuDNClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.dnJC = nil) then
    Exit();

  if (Self.dnJC.cancelling) then
    Self.dnJC.StopCancelling();

  Self.m_state.dnArea := SenderOR;

  var barriers: TJCBarriers := Self.dnJC.barriers();
  var csItems: TList<TConfSeqItem> := TList<TConfSeqItem>.Create();
  try
    for var barrier in barriers do
      if (JCBarriers.IsCSBarrier(barrier.typ)) then
        csItems.Add(CSItem(barrier.Block, JCBarriers.BarrierGetCSNote(barrier.typ)));

    if (csItems.Count > 0) then
      PanelServer.ConfirmationSequence(SenderPnl, Self.DNCSCallback, TArea(SenderOR),
        'Dodatečná návěst s potvrzením', GetObjsList(Self), csItems, True, False)
    else
      Self.DNCSCallback(SenderPnl, True);
  finally
    barriers.Free();
    csItems.Free();
  end;
end;

procedure TBlkSignal.DNCSCallback(Sender: TIDContext; success: Boolean);
begin
  if (success) then
  begin
    Self.dnJC.DN(Sender, Self.m_state.dnArea);
    Blocks.TrainPrediction(Self);
  end;

  Self.m_state.dnArea := nil;
end;

procedure TBlkSignal.MenuRCClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((Self.dnJC = nil) or (Self.dnJC.cancelling)) then
    Exit();
  Self.dnJC.StartCancelling(TArea(SenderOR));
end;

procedure TBlkSignal.MenuABStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.dnJC <> nil) then
    Self.ABJC := Self.dnJC
  else
  begin
    Self.MenuVCStartClick(SenderPnl, SenderOR);
    Self.m_state.beginAB := true;
  end;
end;

procedure TBlkSignal.MenuABStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.ABJC <> nil) then
    Self.ABJC := nil
  else
    Self.MenuVCStopClick(SenderPnl, SenderOR);
end;

procedure TBlkSignal.MenuLockClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.ZAM := true;
  Self.signal := ncStuj;
end;

procedure TBlkSignal.MenuUnlockClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.ZAM := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPNStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) or (Self.IsTargetGoSignal())) then
    Exit();

  if (Self.inRailway = rwHradlo) then
    Self.PNStartHradlo(SenderPnl, SenderOR)
  else if (Self.inRailway = rwNone) then
    Self.PNStartMain(SenderPnl, SenderOR);
end;

procedure TBlkSignal.MenuPNStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.selected := TBlkSignalSelection.none;
  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
end;

procedure TBlkSignal.PNStartMain(SenderPnl: TIdContext; SenderOR: TObject);
begin
  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);
  Self.selected := TBlkSignalSelection.NC;

  for var area: TArea in Self.areas do
    Area.ORDKClickServer(Self.PNDKClick);
end;

procedure TBlkSignal.PNStartHradlo(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.ConfirmationSequence(SenderPnl, Self.PNHradloConfSeq, SenderOR as TArea,
    'Zapnutí přivolávací návěsti', GetObjsList(Self), nil);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPPStartClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((SenderOR as TArea).stack.mode = PV) then
    if (JCDb.FindJC(Self.id, false) <> nil) then
      Exit();

  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
  TPanelConnData(SenderPnl.Data).pathBlocks.Add(Self);
  Self.selected := TBlkSignalSelection.PP;
end;

procedure TBlkSignal.MenuPPStopClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.selected := TBlkSignalSelection.none;
  TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.MenuPPNClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.ConfirmationSequence(SenderPnl, Self.PNDKConfSeq, SenderOR as TArea,
    'Prodloužení doby přivolávací návěsti', GetObjsList(Self), nil);
end;

procedure TBlkSignal.MenuRNZClick(SenderPnl: TIdContext; SenderOR: TObject);
var csItems: TList<TConfSeqItem>;
begin
  csItems := TList<TConfSeqItem>.Create();
  try
    for var blkId: Integer in Self.m_state.toRnz.Keys do
    begin
      var blk: TBlk := Blocks.GetBlkByID(blkId);

      if (blk <> nil) then
      begin
        var justOne: Boolean := True; // print only blocks with just one em lock remaining (but decrease on all toRnz)
        case (blk.typ) of
          btTurnout: begin
              var turnout: TBlkTurnout := TBlkTurnout(blk);
              justOne := (turnout.state.locks = 1) or ((turnout.state.locks = 2) and (turnout.coupling <> nil) and (Self.m_state.toRnz.ContainsKey(turnout.coupling.id)));
          end;
          btLock:
              justOne := (TBlkLock(blk).state.emLock = 1);
          btPst:
              justOne := (TBlkPst(blk).state.emLock = 1);
        end;

        if (justOne) then
          csItems.Add(CSItem(blk, 'Rušení NZ'));
      end;
    end;

    PanelServer.ConfirmationSequence(SenderPnl, Self.RNZPotvrSekv, SenderOR as TArea,
      'Zrušení nouzových závěrů po nouzové cestě', GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkSignal.MenuKCDKClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.selected = TBlkSignalSelection.NC) then
  begin
    for var area: TArea in Self.areas do
      Area.ORDKClickClient();
    PanelServer.ConfirmationSequence(SenderPnl, Self.PNDKConfSeq, SenderOR as TArea,
      'Zapnutí přivolávací návěsti', GetObjsList(Self), nil);
  end;
end;

procedure TBlkSignal.MenuAdminStopIR(SenderPnl: TIdContext; SenderOR: TObject; enabled: Boolean);
begin
  try
    if (Self.m_settings.events[0].stop.typ = TRREvType.rrtIR) then
    begin
      var ir: TBlkIR := Blocks.GetBlkIrByID(Self.m_settings.events[0].stop.data.irId);
      if (ir = nil) then
        Exit();
      RCSi.SetInput(ir.GetSettings().RCSAddr, ite(enabled, 1, 0));
    end;
  except
    PanelServer.BottomError(SenderPnl, 'Nepodařilo se nastavit stav IR čidla!', TArea(SenderOR).ShortName, 'SIMULACE');
  end;
end;

procedure TBlkSignal.MenuAdminRadOnClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.PSt.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.PSt.rcsControllerShunt, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkSignal.MenuAdminRadOffClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.PSt.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.PSt.rcsControllerShunt, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  case (Button) of
    F2:
      PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

    ENTER:
      begin
        if ((IsWritable(rights)) and ((((Self.dnJC = nil) or (Self.dnJC.destroyEndBlock >= 1)) and (JCDb.FindJCActivating(Self.id) = nil) and
          (Self.signal <> ncPrivol) and (JCDb.IsAnyVCAvailable(Self) and (Self.enabled)) and (not Self.PstIs())) or
          (TArea(SenderOR).stack.mode = VZ)) and (JCDb.IsAnyVC(Self))) then
        begin
          if ((not Self.m_settings.locked) and (Self.inRailway = rwNone)) then
            Self.MenuVCStartClick(SenderPnl, SenderOR);
        end
        else
          PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
      end;

    F1:
      begin
        if ((IsWritable(rights)) and ((((Self.dnJC = nil) or (Self.dnJC.destroyEndBlock >= 1)) and (JCDb.FindJCActivating(Self.id) = nil) and
          (Self.signal <> ncPrivol) and (JCDb.IsAnyPCAvailable(Self)) and (Self.enabled) and (not Self.PstIs())) or
          ((SenderOR as TArea).stack.mode = VZ)) and (JCDb.IsAnyPC(Self))) then
        begin
          if ((not Self.m_settings.locked) and (Self.inRailway = rwNone)) then
            Self.MenuPCStartClick(SenderPnl, SenderOR);
        end
        else
          PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSignal.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights);
begin
  if (item = 'VC>') then
    Self.MenuVCStartClick(SenderPnl, SenderOR)
  else if (item = 'VC<') then
    Self.MenuVCStopClick(SenderPnl, SenderOR)
  else if (item = 'PC>') then
    Self.MenuPCStartClick(SenderPnl, SenderOR)
  else if (item = 'PC<') then
    Self.MenuPCStopClick(SenderPnl, SenderOR)
  else if (item = 'STUJ') then
    Self.MenuSTUJClick(SenderPnl, SenderOR)
  else if (item = 'DN') then
    Self.MenuDNClick(SenderPnl, SenderOR)
  else if (item = 'RC') then
    Self.MenuRCClick(SenderPnl, SenderOR)
  else if (item = 'AB>') then
    Self.MenuABStartClick(SenderPnl, SenderOR)
  else if (item = 'AB<') then
    Self.MenuABStopClick(SenderPnl, SenderOR)
  else if (item = 'ZAM>') then
    Self.MenuLockClick(SenderPnl, SenderOR)
  else if (item = 'ZAM<') then
    Self.MenuUnlockClick(SenderPnl, SenderOR)
  else if (item = 'PN>') then
    Self.MenuPNStartClick(SenderPnl, SenderOR)
  else if (item = 'PN<') then
    Self.MenuPNStopClick(SenderPnl, SenderOR)
  else if (item = 'PP>') then
    Self.MenuPPStartClick(SenderPnl, SenderOR)
  else if (item = 'PP<') then
    Self.MenuPPStopClick(SenderPnl, SenderOR)
  else if (item = 'PPN') then
    Self.MenuPPNClick(SenderPnl, SenderOR)
  else if (item = 'RNZ') then
    Self.MenuRNZClick(SenderPnl, SenderOR)
  else if (item = 'IR>') then
    Self.MenuAdminStopIR(SenderPnl, SenderOR, true)
  else if (item = 'IR<') then
    Self.MenuAdminStopIR(SenderPnl, SenderOR, false)
  else if (item = 'RAD>') then
    Self.MenuAdminRadOnClick(SenderPnl, SenderOR)
  else if (item = 'RAD<') then
    Self.MenuAdminRadOffClick(SenderPnl, SenderOR)
  else if (item = 'KC') then
    Self.MenuKCDKClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro konkretni s-com:
function TBlkSignal.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  // pokud je navestidlo trvale zamkle, neumoznime zadne volby
  if ((Self.m_settings.locked) or (not IsWritable(rights))) then
    Exit();

  if (((((Self.dnJC = nil) or (Self.dnJC.destroyEndBlock >= 1)) and (JCDb.FindJCActivating(Self.id) = nil) and
        (Self.signal <> ncPrivol) and (not Self.ab)) or ((SenderOR as TArea).stack.mode = VZ)) and (Self.inRailway <> rwAutoblok)) then
  begin
    case (Self.m_state.selected) of
      TBlkSignalSelection.VC:
        if (Self.beginAB) then
          Result := Result + 'AB<,'
        else
          Result := Result + 'VC<,';
      TBlkSignalSelection.PC:
        Result := Result + 'PC<,';
      TBlkSignalSelection.NC:
        Result := Result + 'PN<,';
      TBlkSignalSelection.PP:
        Result := Result + 'PP<,';
    else
      // 2 = VC, 3= PC
      if (Self.m_spnl.symbolType = TBlkSignalSymbol.main) then
      begin
        if ((JCDb.IsAnyVC(Self)) and (((JCDb.IsAnyVCAvailable(Self)) and (Self.enabled) and (not Self.PstIs())) or
            ((SenderOR as TArea).stack.mode = VZ))) then
        // i kdyz neni zadna VC, schvalne umoznime PN
        begin
          Result := Result + 'VC>,';
          if (Self.dnJC = nil) then
            Result := Result + 'AB>,';
        end;
        if (not Self.IsTargetGoSignal()) then // hradlo
          Result := Result + '!PN>,';
      end;
      if (JCDb.IsAnyPC(Self)) then
      begin
        if ((JCDb.IsAnyPC(Self)) and (((JCDb.IsAnyPCAvailable(Self)) and (Self.enabled) and (not Self.PstIs())) or
            ((SenderOR as TArea).stack.mode = VZ))) then
          Result := Result + 'PC>,';
        Result := Result + 'PP>,';
      end;
    end;

    if (not Result.EndsWith('-,')) then
      Result := Result + '-,';
  end;

  if (Self.CanSTUJ()) then
    Result := Result + 'STUJ,';

  if (Self.signal = ncPrivol) then
    Result := Result + '!PPN,';

  if (Self.dnJC <> nil) then
  begin
    // bud je cesta primo postavena, nebo je zrusena, ale podminky jsou vyhovujici pro DN
    // plati jen pro postavenou JC
    if ((not Self.ZAM) and (Self.signal = ncStuj) and (Self.dnJC.CanDN())) then
    begin
      if (JCBarriers.IsAnyCSBarrier(Self.dnJC.barriers())) then
        Result := Result + '!DN,'
      else
        Result := Result + 'DN,';
    end;

    if (((Self.signal > ncStuj) or (Self.dnJC.CanDN()) or (Self.dnJC.destroyBlock < 1)) and (not Self.dnJC.cancelling))
    then
    begin
      Result := Result + 'RC,';

      // AB lze jen u vlakove cesty
      if ((Self.dnJC.typ = TJCType.Train) and (not Self.ab)) then
        Result := Result + 'AB>,';
    end;
  end;

  // AB lze jen u vlakove cesty
  if (Self.ab) then
    Result := Result + 'AB<,';

  if (Self.m_state.ZAM) then
    Result := Result + 'ZAM<,'
  else
    Result := Result + 'ZAM>,';

  if ((Self.signal <> ncPrivol) and (Self.CanIDoRNZ)) then
    Result := Result + '!RNZ,';

  // DEBUG: jednoduche nastaveni IR pri knihovne simulator
  if (RCSi.simulation) then
  begin
    Result := Result + '-,';

    if ((Self.m_settings.events.Count > 0) and (Self.m_settings.events[0].stop.typ = TRREvType.rrtIR)) then
    begin
      var ir := Blocks.GetBlkIrByID(Self.m_settings.events[0].stop.data.irId);
      if (ir <> nil) then
      begin
        case (ir.occupied) of
          TIROccupationState.Free:
            Result := Result + '*IR>,';
          TIROccupationState.occupied:
            Result := Result + '*IR<,';
        end; // case
      end;
    end;

    if (Self.m_settings.PSt.enabled) then
    begin
      try
        if (RCSi.GetInput(Self.m_settings.PSt.rcsControllerShunt) = isOn) then
          Result := Result + '*RAD<,'
        else
          Result := Result + '*RAD>,';
      except
        on E: RCSException do begin end;
        on E: Exception do raise;
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlkSignal.SignalToString(code: TBlkSignalCode): string;
begin
  case (code) of
    ncChanging:
      Result := 'stavění...';
    ncDisabled:
      Result := 'disabled';
    ncStuj:
      Result := 'stůj/posun zakázán';
    ncVolno:
      Result := 'volno';
    ncVystraha:
      Result := 'výstraha';
    ncOcek40:
      Result := 'očekávejte 40 km/h';
    ncVolno40:
      Result := '40 km/h a volno';
    ncVse:
      Result := 'svítí vše (Rezerva)';
    ncVystraha40:
      Result := '40 km/h a výstraha';
    nc40Ocek40:
      Result := '40 km/h a očekávejte 40 km/h';
    ncPrivol:
      Result := 'přivolávací návěst';
    ncPosunZaj:
      Result := 'dovolen zajištěný posun';
    ncPosunNezaj:
      Result := 'dovolen nezajištěný posun';
    ncOpakVolno:
      Result := 'opakování návěsti volno';
    ncOpakVystraha:
      Result := 'opakování návěsti výstraha';
    ncZhasnuto:
      Result := 'návěstidlo zhaslé';
    ncOpakOcek40:
      Result := 'opakování návěsti očekávejte 40 km/h';
    ncOpakVystraha40:
      Result := 'opakování návěsti výstraha a 40 km/h';
    ncOpak40Ocek40:
      Result := '40 km/h a opakování návěsti očekávejte 40 km/h';
  else
    Result := 'Jiná návěst';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.JCCancelSignal();
begin
  if (Self.m_settings.fallDelay > 0) then
  begin
    Self.m_state.falling := true;
    Self.m_state.fallingStart := now;
    Log('Návěstidlo ' + Self.m_globSettings.name + ': spoždění pádu ' + IntToStr(Self.m_settings.fallDelay) +
      ' s', TLogLevel.llInfo, lsJC);
  end else begin
    Self.signal := ncStuj;
  end;

  Self.UpdateTrainSpeed(true);
end;

procedure TBlkSignal.UpdateFalling();
begin
  if (not Self.m_state.falling) then
    Exit();

  if (Self.m_state.fallingStart + EncodeTimeSec(Self.m_settings.fallDelay)
    < now) then
  begin
    Self.signal := ncStuj;
    Self.m_state.falling := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// aktualizace rychlosti souprav před návěstidlem
// pozor na padání !
// force nucene zastavi vlak, resp. nastavi jeho rychlost
// metoda je volana s force v pripade, kdy dochazi k prime zmene navesti od uzivatele (STUJ, DN, RC)
procedure TBlkSignal.UpdateTrainSpeed(force: Boolean = false);
begin
  if (Self.m_settings.events.Count = 0) then
    Exit();
  var track := TBlkTrack(Self.track);
  if (Self.m_spnl.symbolType = TBlkSignalSymbol.shunting) then
    Exit(); // pokud jsem posunove navestidlo, koncim funkci
  if (track = nil) then
    Exit(); // pokud pred navestidlem neni usek, koncim funkci

  if ((track.typ = btRT) and (TBlkRT(track).railway <> nil)) then
  begin
    var railway: TBlkRailway := TBlkRailway(TBlkRT(track).railway);

    // Ignoruji krajni navestidla trati, ktera jsou proti smeru trati
    if ((railway.direction = TRailwayDirection.AtoB) and (Self = railway.signalA)) then
      Exit();
    if ((railway.direction = TRailwayDirection.BtoA) and (Self = railway.signalB)) then
      Exit();

    // Vsechna navestidla autobloku proti smeru trati se ignoruji (zejmena v kontextu zmeny smeru soupravy)
    if ((Self.inRailway <> rwNone) and (TBlkRailway(TBlkRT(track).railway).direction = TRailwayDirection.AtoB) and
      (Self.direction = THVSite.even)) then
      Exit();
    if ((Self.inRailway <> rwNone) and (TBlkRailway(TBlkRT(track).railway).direction = TRailwayDirection.BtoA) and
      (Self.direction = THVSite.odd)) then
      Exit();
  end;

  // zjisteni aktualni udalosti podle typu a delky soupravy
  var i: Integer := Self.CurrentEventIndex();
  var signalEv: TBlkSignalTrainEvent := Self.m_settings.events[i];

  if (i <> Self.m_lastEvIndex) then
  begin
    // Z nejakeho duvodu reagujeme na novou udalost -> vypnout starou udalost
    if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
    begin
      if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
        Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();

      if ((Self.m_settings.events[Self.m_lastEvIndex].slow.enabled) and
        (Self.m_settings.events[Self.m_lastEvIndex].slow.ev.enabled)) then
        Self.m_settings.events[Self.m_lastEvIndex].slow.ev.Unregister();
    end;

    Self.m_lastEvIndex := i;
  end;

  /// ////////////////////////////////////////////////

  // ZPOMALOVANI
  // Zpomalni je mozne i na jinem useku nez je usek pred navestidlem
  // Proto kontroly pritmnosti vlaku na useku pred navestidlem jsou az nize

  if (signalEv.slow.enabled) then
  begin
    var slowTrack := TBlkTrack(signalEv.slow.ev.Track(track));
    var slowTrain := Self.GetTrain(slowTrack);

    if ((slowTrain <> nil) and (slowTrain.front = slowTrack) and (slowTrain.wantedSpeed > signalEv.slow.speed) and (slowTrack.slowingReady) and
        ((not Self.IsGoSignal()) or (slowTrain.IsPOdj(slowTrack))) and (slowTrain.direction = Self.m_spnl.direction)) then
    begin
      if (not signalEv.slow.ev.enabled) then
        signalEv.slow.ev.Register(slowTrain.index);

      if (signalEv.slow.ev.IsTriggerred(slowTrack, true)) then
      begin
        signalEv.slow.ev.Unregister();
        slowTrain.speed := signalEv.slow.speed;
        slowTrack.slowingReady := false;
      end;
    end else begin
      if (signalEv.slow.ev.enabled) then
        signalEv.slow.ev.Unregister();
    end;
  end;

  /// ////////////////////////////////////////////////
  // ZASTAVOVANI, resp. nastavovani rychlosti prislusne JC

  // pokud na useku prede mnou neni souprava, koncim funkci
  if (not track.IsTrain()) then
  begin
    // tady musi dojit ke zruseni registrace eventu, kdyby nedoslo, muze se stat,
    // ze za nejakou dobu budou splneny podminky, pro overovani eventu, ale
    // event bude porad bezet -> pokud je casovy, okamzite byse spustil
    if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
      if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
        Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();
    Exit();
  end;

  // pokud souprava svym predkem neni na bloku pred navestidlem, koncim funkci
  var train: TTrain := Self.GetTrain(track);
  if (train.front <> track) then
  begin
    // tady musime zrusit registraci eventu, viz vyse
    if ((Self.m_lastEvIndex >= 0) and (Self.m_lastEvIndex < Self.m_settings.events.Count)) then
      if (Self.m_settings.events[Self.m_lastEvIndex].stop.enabled) then
        Self.m_settings.events[Self.m_lastEvIndex].stop.Unregister();
    Exit();
  end;

  if (not signalEv.stop.enabled) then
    signalEv.stop.Register(train.index);

  if ((signalEv.stop.IsTriggerred(track, true)) or (force)) then
  // podminka IsTriggerred take resi to, ze usek musi byt obsazeny (tudiz resi vypadek useku)
  begin
    // event se odregistruje automaticky pri zmene

    if ((Train.IsPOdj(track)) and (Train.direction = Self.m_spnl.direction)) then
    begin
      // predvidany odjezd neuplynul -> zastavit soupravu
      if (Train.wantedSpeed <> 0) then
        Train.speed := 0;

      // souprava je na zastavovaci udalosti -> zacit pocitat cas
      if (not Train.GetPOdj(track).origin_set) then
      begin
        Train.GetPOdj(track).RecordOriginNow();
        track.PropagatePOdjToRailway();
        track.Change();
      end;

      Exit();
    end;

    if ((Assigned(Self.dnJC)) and (Self.dnJC.typ = TJCType.Train)) then
    begin
      // je JC -> je postaveno?
      if ((Self.IsGoSignal()) and (not Self.m_state.falling)) then
      begin
        // je postaveno -> zkontrolujeme, jestli budeme na konci zastavovat
        if ((Train.wantedSpeed > 0) and (Train.direction <> Self.m_spnl.direction)) then
          Exit(); // pokud jede souprava opacnym smerem, kaslu na ni

        // Aby se nezrychlilo napr. pri uvolneni predchoziho useku v trati
        track.slowingReady := false;

        case (Self.dnJC.data.nextSignalType) of
          TJCNextSignalType.signal:
            begin
              var signal := Blocks.GetBlkSignalByID(Self.dnJC.data.nextSignalId);

              if ((signal <> nil) and (signal.IsGoSignal()) and (not Train.IsPOdj(Self.dnJC.lastTrack))) then
              begin
                // na konci JC jedeme dal
                var speed: Cardinal;
                var success: Boolean;
                if (Self.dnJC.data.speedsGo.Count > 0) then // if go speeds empty, use stop speeds
                  success := TTrainSpeed.Pick(Train, Self.dnJC.data.speedsGo, speed)
                else
                  success := TTrainSpeed.Pick(Train, Self.dnJC.data.speedsStop, speed);
                if ((success) and (Train.wantedSpeed <> Integer(speed)) or (Train.direction <> Self.m_spnl.direction)) then
                  Train.SetSpeedDirection(speed, Self.m_spnl.direction);
              end else begin
                // na konci JC stojime
                var speed: Cardinal;
                if (TTrainSpeed.Pick(Train, Self.dnJC.data.speedsStop, speed)) then // if success
                  if ((Train.wantedSpeed <> Integer(speed)) or (Train.direction <> Self.m_spnl.direction)) then
                    Train.SetSpeedDirection(speed, Self.m_spnl.direction);
              end;
            end;

          TJCNextSignalType.railway:
            begin
              var speed: Cardinal;
              if (TTrainSpeed.Pick(Train, Self.dnJC.data.speedsGo, speed)) then // if success
                if ((Train.wantedSpeed <> Integer(speed)) or (Train.direction <> Self.m_spnl.direction)) then
                  Train.SetSpeedDirection(speed, Self.m_spnl.direction);
            end;

          TJCNextSignalType.no:
            begin
              var speed: Cardinal;
              if (TTrainSpeed.Pick(Train, Self.dnJC.data.speedsStop, speed)) then // if success
                if ((Train.wantedSpeed <> Integer(speed)) or (Train.direction <> Self.m_spnl.direction)) then
                  Train.SetSpeedDirection(speed, Self.m_spnl.direction);
            end;
        end;

        // kontrola prehravani stanicniho hlaseni
        Train.CheckAnnouncement(Self);
      end else begin
        // neni povolovaci navest -> zastavit LOKO
        if ((Train.direction = Self.m_spnl.direction) and (Train.wantedSpeed <> 0)) then
          Train.SetSpeedDirection(0, Self.m_spnl.direction);
      end;
    end else begin
      // nenalezena jizdni cesta -> muze se jednat o navestidlo v autobloku
      if (Train.direction = Self.m_spnl.direction) then
      begin
        if ((Self.IsGoSignal()) and (not Self.m_state.falling) and (Self.track.typ = btRT) and
          (TBlkRT(Self.track).inRailway > -1)) then
        begin
          var speed: Cardinal;
          var success := Train.GetRailwaySpeed(speed);
          if ((success) and (Cardinal(Train.wantedSpeed) <> speed) and (not Train.IsSpeedOverride())) then
            Train.SetSpeedDirection(speed, Self.m_spnl.direction)
        end else begin
          // neni povolovaci navest -> zastavit
          if (Train.wantedSpeed <> 0) then
            Train.SetSpeedDirection(0, Self.m_spnl.direction);
        end;
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Vraci udalost, na kterou by se melo reagovat podle aktualniho stavu kolejiste.

function TBlkSignal.CurrentEventIndex(): Integer;
begin
  if (Self.m_settings.events.Count = 0) then
    raise ENoEvents.Create('No current events!');

  var track := TBlkTrack(Self.track);
  if (track.IsTrain()) then
  begin
    var train: TTrain := Self.GetTrain(track);

    // hledame takovy event, ktery odpovida nasi souprave
    if (Self.m_settings.events.Count >= 2) then
      for var i: Integer := 1 to Self.m_settings.events.Count - 1 do
        if (Self.m_settings.events[i].Match(train)) then
          Exit(i);

    // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
    Result := 0;
  end else begin
    // na bloku neni zadna souprava -> muzeme chtit zpomaleni na jinem bloku, musime se divat na soupravu na tomto bloku

    if (Self.m_settings.events.Count >= 2) then
    begin
      for var i: Integer := 1 to Self.m_settings.events.Count - 1 do
      begin
        var event: TBlkSignalTrainEvent := Self.m_settings.events[i];
        if ((event.slow.enabled) and (event.slow.ev.trackDefined) and (TBlkTrack(event.slow.ev.Track()).IsTrain())
            and (event.Match(Self.GetTrain(TBlkTrack(event.slow.ev.Track()))))) then
          Exit(i);
      end;
    end;

    Result := 0;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.IsGoSignal(jctype: TJCType = TJCType.Train): Boolean;
begin
  if ((Self.signal = ncChanging) and (TBlkSignal.IsGoSignal(Self.m_state.targetSignal, jctype))) then
    // navest se meni na nejakou povolovaci -> ridim se jeste tou starou
    Result := TBlkSignal.IsGoSignal(Self.m_state.signalOld, jctype)
  else
    Result := TBlkSignal.IsGoSignal(Self.signal, jctype);
end;

function TBlkSignal.IsTargetGoSignal(jctype: TJCType = TJCType.Train): Boolean;
begin
  if (Self.changing) then
    Result := TBlkSignal.IsGoSignal(Self.targetSignal, jctype)
  else
    Result := TBlkSignal.IsGoSignal(Self.signal, jctype);
end;

function TBlkSignal.IsOpakVystraha(): Boolean;
begin
  Result := (Self.targetSignal = ncOpakVystraha) or (Self.targetSignal = ncOpakVystraha40);
end;

class function TBlkSignal.IsGoSignal(Navest: TBlkSignalCode; jctype: TJCType = TJCType.Train): Boolean;
begin
  if (jctype = TJCType.Train) then
  begin
    case (Navest) of
      ncVolno, ncVystraha, ncOcek40, ncVolno40, ncVystraha40, nc40Ocek40, ncOpakVolno, ncOpakVystraha, ncOpakOcek40,
        ncOpakVystraha40, ncOpak40Ocek40:
        Result := true;
    else
      Result := false;
    end;
  end else if (jctype = TJCType.shunt) then
    Result := (Navest = ncPosunZaj) or (Navest = ncPosunNezaj)
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.UpdatePrivol();
begin
  if ((Self.m_state.privolStart + EncodeTimeSec(_PRIVOL_SEC) < now + EncodeTime(0, 0, 30, 0)) and
    (Self.m_state.privolTimerId = 0)) then
  begin
    // oznameni o brzkem ukonceni privolavaci navesti
    Self.m_state.privolTimerId := Random(65536) + 1;
    for var area: TArea in Self.m_areas do
    begin
      Area.BroadcastGlobalData('INFO-TIMER;' + IntToStr(Self.m_state.privolTimerId) + ';0;30; PN ' +
        Self.m_globSettings.name);
      Area.TimerCnt := Area.TimerCnt + 1;
    end;
  end;

  if (Self.m_state.privolStart + EncodeTimeSec(_PRIVOL_SEC) < now) then
  begin
    // pad privolavaci navesti
    Self.signal := ncStuj;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// privolavaci navest bez podpory zabezpecovaciho zarizeni
procedure TBlkSignal.PNDKClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton);
begin
  if (Button = ENTER) then
  begin
    for var area: TArea in Self.areas do
      Area.ORDKClickClient();

    var upos := TList<TUPOItem>.Create();
    try
      TArea(SenderOR).AddPNsUPOs(upos);
      TArea(SenderOR).AddNCsUPOs(upos);
      PanelServer.UPO(SenderPnl, upos, false, Self.PNBarriersConfirmed, Self.PNBarriersRejected, SenderOR);
    finally
      upos.Free();
    end;

  end else begin
    if (Button = TPanelButton.F2) then
      PanelServer.Menu(SenderPnl, Self, TArea(SenderOR), '$' + TArea(SenderOR).name + ',-,' + 'KC');
  end;
end;

procedure TBlkSignal.PNBarriersConfirmed(Sender: TObject);
begin
  PanelServer.ConfirmationSequence(TIdContext(Sender), Self.PNDKConfSeq, (TPanelConnData(TIdContext(Sender).data).UPO_ref as TArea),
    'Zapnutí přivolávací návěsti', GetObjsList(Self), nil);
end;

procedure TBlkSignal.PNBarriersRejected(Sender: TObject);
begin
  Self.selected := TBlkSignalSelection.none;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PNDKConfSeq(Sender: TIdContext; success: Boolean);
begin
  if (success) then
  begin
    Self.m_state.selected := TBlkSignalSelection.none;
    Self.signal := ncPrivol;
    Self.Log('Rozsvícena PN bez podpory zab. zař. / prodloužena PN', TLogLevel.llInfo, lsJC);
  end else begin
    Self.selected := TBlkSignalSelection.none;
  end;
end;

procedure TBlkSignal.PNHradloConfSeq(Sender: TIdContext; success: Boolean);
begin
  if (success) then
  begin
    Self.signal := ncPrivol;
    Self.Log('Rozsvícena PN na hradle', TLogLevel.llInfo, lsJC);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.RNZPotvrSekv(Sender: TIdContext; success: Boolean);
var
  toRnz: TDictionary<Integer, Cardinal>;
begin
  if (not success) then
    Exit();

  // nejdriv uvolnime toRNZ -- abychom jej nemazali v DecreaseNouzZaver
  toRnz := Self.m_state.toRnz;
  Self.m_state.toRnz := TDictionary<Integer, Cardinal>.Create();

  for var blkId: Integer in toRnz.Keys do
  begin
    var blk := Blocks.GetBlkByID(blkId);
    if (blk = nil) then
      continue;

    case (Blk.typ) of
      btTurnout:
        begin
          if (TBlkTurnout(blk).emLock) then
            TBlkTurnout(blk).DecreaseEmergencyLock(toRnz[blkId]);
        end;

      btLock:
        begin
          if (TBlkLock(blk).emLock) then
            TBlkLock(blk).DecreaseEmLock(toRnz[blkId]);
        end;

      btPst:
        begin
          if (TBlkPst(blk).emLock) then
            TBlkPst(blk).DecreaseEmLock(toRnz[blkId]);
        end;
    end;
  end;

  toRnz.Free();
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.SetTrackId(new_id: Integer);
begin
  if (Self.m_spnl.trackId = new_id) then
    Exit();
  Self.m_spnl.trackId := new_id;
  if (new_id = -1) then
    Self.inRailway := rwNone;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetTrackId(): TBlk;
begin
  if (((Self.m_trackId = nil) and (Self.trackId <> -1)) or
    ((Self.m_trackId <> nil) and (Self.trackId <> Self.m_trackId.id))) then
    Self.m_trackId := Blocks.GetBlkTrackorRTByID(Self.trackId);
  Result := Self.m_trackId;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.AddBlkToRnz(blkId: Integer; Change: Boolean = true);
begin
  if (Self.m_state.toRnz.ContainsKey(blkId)) then
    Self.m_state.toRnz[blkId] := Self.m_state.toRnz[blkId] + 1
  else
    Self.m_state.toRnz.Add(blkId, 1);

  if ((Self.m_state.toRnz.Count = 1) and (Self.m_state.toRnz[blkId] = 1) and (Change)) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.CanIDoRNZ(): Boolean;
begin
  Result := Self.m_state.toRnz.Count > 0;
end;

function TBlkSignal.CanSTUJ(): Boolean;
begin
  Result := (Self.targetSignal <> ncStuj) and (Self.targetSignal <> ncZhasnuto) and
    ((Self.inRailway = rwNone) or ((Self.inRailway = rwHradlo) and (Self.targetSignal = ncPrivol)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.RemoveBlkFromRnz(blkId: Integer);
begin
  if (Self.m_state.toRnz.ContainsKey(blkId)) then
  begin
    Self.m_state.toRnz.Remove(blkId);
    if (Self.m_state.toRnz.Count = 0) then
      Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.UnregisterAllEvents();
begin
  for var ev: TBlkSignalTrainEvent in Self.m_settings.events do
  begin
    ev.stop.Unregister();
    if ((ev.slow.enabled) and (Assigned(ev.slow.ev))) then
      ev.slow.ev.Unregister();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  json['symbolType'] := Integer(Self.m_spnl.symbolType);
  json['track'] := Self.m_spnl.trackId;
  json['direction'] := Integer(Self.m_spnl.direction);

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkSignal.GetPtState(json: TJsonObject);
begin
  json['signal'] := Self.signal;
  json['targetSignal'] := Self.targetSignal;
  json['selected'] := Self.selected;
  json['ab'] := Self.ab;
  json['zam'] := Self.ZAM;
  if (Self.ABJC <> nil) then
    json['abjc'] := Self.ABJC.id;
  if (Self.privol <> nil) then
    json['pnjc'] := Self.privol.id;
end;

procedure TBlkSignal.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('signal')) then
  begin
    if (not Self.enabled) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Neaktivni blok');
      inherited;
      Exit();
    end;

    if (reqJson.I['signal'] = Integer(TBlkSignalCode.ncStuj)) then
    begin
      if (Self.targetSignal <> ncStuj) then
      begin
        if (Self.CanSTUJ()) then
          Self.signal := ncStuj
        else
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request', 'Nelze prestavit do STUJ');
      end;
    end else if (reqJson.I['signal'] = Integer(TBlkSignalCode.ncPrivol)) then
    begin
      if (Self.targetSignal <> ncPrivol) then
      begin
        if ((Self.m_spnl.symbolType = TBlkSignalSymbol.main) and (Self.targetSignal = ncStuj) and
            (JCDb.FindJCActivating(Self.id) = nil) and (Self.inRailway <> rwAutoblok) and ((Self.dnJC = nil) or (Self.dnJC.destroyEndBlock >= 1))) then
        begin
          Self.signal := ncPrivol;
        end else
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request', 'Nelze rozsvitit PN');
      end;
    end else
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request', 'Nepodorovana navest');
  end;

  if (reqJson.Contains('rcsControllerShunt')) then
  begin
    try
      if (Self.m_settings.PSt.enabled) then
        RCSi.SetInput(Self.m_settings.PSt.rcsControllerShunt, ownConvert.BoolToInt(reqJson.B['rcsControllerShunt']))
      else
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad Request', 'Navestidlo nema kontrolery PSt');
    except
      on e: RCSException do
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '500', 'Simulace nepovolila nastaveni RCS vstupu', e.Message);
    end;

    Self.Update(); // to propagate new state into response
  end;

  inherited;
end;


/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetTrain(track: TBlk = nil): TTrain;
begin
  if (track = nil) then
    track := Blocks.GetBlkTrackOrRTByID(Self.trackId);

  if (Self.direction = THVSite.odd) then
    Result := TBlkTrack(track).trainSudy
  else
    Result := TBlkTrack(track).trainL;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PropagatePOdjToRailway();
var train: TTrain;
begin
  train := Self.GetTrain();
  if (train = nil) then
  begin
    train := TBlkTrack(Self.track).trainPredict;
    if (train = nil) then
      Exit();
  end;

  if ((Self.dnJC = nil) or (Self.dnJC.data.railwayId = -1)) then
    Exit();
  var railway := Blocks.GetBlkRailwayByID(Self.dnJC.data.railwayId);
  if ((railway = nil) or (railway.trainPredict = nil) or (railway.trainPredict.Train <> Train)) then
    Exit();

  if (Train.IsPOdj(Self.track)) then
  begin
    var podj: TPOdj := Train.GetPOdj(Self.track);
    if (not podj.IsDepSet) then
      Exit();
    if ((TBlkRailway(railway).trainPredict.IsTimeDefined) and (TBlkRailway(railway).trainPredict.time = podj.DepTime())) then
      Exit();

    TBlkRailway(railway).trainPredict.predict := true;
    TBlkRailway(railway).trainPredict.time := podj.DepTime();
    TBlkRailway(railway).Change();
  end else if ((TBlkRailway(railway).trainPredict.predict) and ((not Train.IsPOdj(Self.track)) or
    (not Train.GetPOdj(Self.track).IsDepSet()))) then
  begin
    TBlkRailway(railway).trainPredict.predict := false;
    TBlkRailway(railway).trainPredict.UndefTime();
    TBlkRailway(railway).Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.IsChanging(): Boolean;
begin
  Result := (Self.signal = ncChanging);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.PanelStateString(): string;
var fg, bg, okoli: TColor;
begin
  Result := inherited;

  case (Self.selected) of
    TBlkSignalSelection.none:
      okoli := TJopColor.black;
    TBlkSignalSelection.VC:
      okoli := TJopColor.greenDark;
    TBlkSignalSelection.PC:
      okoli := TJopColor.white;
    TBlkSignalSelection.NC, TBlkSignalSelection.PP:
      okoli := TJopColor.turqDark;
  else
    okoli := TJopColor.black;
  end;

  bg := okoli;
  fg := TJopColor.grayDark;

  if (Self.ZAM) then
    case (Self.symbolType) of
      TBlkSignalSymbol.main:
        fg := TJopColor.red;
      TBlkSignalSymbol.shunting:
        fg := TJopColor.blue;
    end;
  if (Self.canRNZ) then
    fg := TJopColor.turqDark;

  case (Self.signal) of
    ncStuj:
      begin
      end;
    ncChanging, ncZhasnuto:
      begin
        fg := TJopColor.black;
        bg := TJopColor.grayDark;
      end;
    ncVolno, ncVystraha, ncOcek40, ncVolno40, ncVystraha40, nc40Ocek40, ncOpakVolno, ncOpakVystraha, ncOpakOcek40,
      ncOpakVystraha40, ncOpak40Ocek40:
      fg := TJopColor.green;
    ncPrivol, ncPosunZaj, ncPosunNezaj:
      fg := TJopColor.white;
    ncVse:
      fg := TJopColor.yellow;
  else
    if (fg = TJopColor.grayDark) then
      fg := TJopColor.black;
    bg := TJopColor.purple;
  end;

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) + ';' +
    ownConvert.BoolToStr10(Self.signal = ncPrivol) + ';' + ownConvert.BoolToStr10(Self.ab) + ';' +
    ownConvert.ColorToStr(okoli);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.FourtyKmph(): Boolean;
begin
  Result := (Self.targetSignal = ncVolno40) or (Self.targetSignal = ncVystraha40) or (Self.targetSignal = nc40Ocek40) or
    (Self.targetSignal = ncOpakVystraha40) or (Self.targetSignal = ncOpak40Ocek40);
end;

class function TBlkSignal.AddOpak(code: TBlkSignalCode): TBlkSignalCode;
begin
  case (code) of
    ncVolno:
      Result := ncOpakVolno;
    ncVystraha:
      Result := ncOpakVystraha;
    ncOcek40:
      Result := ncOpakOcek40;
    ncVystraha40:
      Result := ncOpakVystraha40;
    nc40Ocek40:
      Result := ncOpak40Ocek40;
  else
    Result := code;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.GetTargetSignal(): TBlkSignalCode;
begin
  if (Self.changing) then
    Result := Self.m_state.targetSignal
  else
    Result := Self.m_state.signal;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkSignalTrainEvent.Create();
begin
  inherited;

  Self.stop := nil;
  Self.slow.ev := nil;
end;

constructor TBlkSignalTrainEvent.Create(str: string; old: Boolean);
begin
  Self.Create();
  Self.Parse(str, old);
end;

destructor TBlkSignalTrainEvent.Destroy();
begin
  if (Assigned(Self.stop)) then
    Self.stop.Free();
  if (Assigned(Self.slow.ev)) then
    Self.slow.ev.Free();

  inherited;
end;

// format ev1: RychEvent.stop|RychEvent-slow|re:train_typ_regexp|min_delka|max_delka
procedure TBlkSignalTrainEvent.Parse(str: string; old: Boolean);
var sl, sl2: TStrings;
begin
  sl := TStringList.Create();
  sl2 := TStringList.Create();
  ExtractStringsEx(['|'], [], PChar(str), sl);

  try
    Self.length.min := -1;
    Self.length.max := -1;

    if (old) then
      Self.stop := Self.ParseOldRychEvent(false, sl[0])
    else
      Self.stop := TRREv.Create(false, sl[0]);

    if ((sl.Count > 1) and (sl[1] <> '') and (LeftStr(sl[1], 2) <> '-1')) then
    begin
      ExtractStringsEx([';', ','], [], sl[1], sl2);

      Self.slow.enabled := true;
      if (old) then
        Self.slow.ev := Self.ParseOldRychEvent(true, sl[1])
      else
        Self.slow.ev := TRREv.Create(true, sl2[0]);

      Self.slow.speed := StrToInt(sl2[sl2.Count - 1]);
    end else begin
      Self.slow.enabled := false;
      Self.slow.ev := nil;
    end;

    if (sl.Count > 2) then
    begin
      Self.train_type_re := ParseTrainTypes(sl[2]);
      Self.length.min := StrToIntDef(sl[3], -1);
      Self.length.max := StrToIntDef(sl[4], -1);
    end;
  finally
    sl.Free();
    sl2.Free();
  end;
end;

class function TBlkSignalTrainEvent.ParseTrainTypes(types: string): string;
begin
  if ((types = '') or (types = ';')) then
    Result := '^.*$'
  else if (StartsText('re:', types)) then
  begin
    if (types = 're:') then
      Result := '^.*$'
    else
      Result := RightStr(types, System.length(types) - 3);
  end else begin
    // backward-compatibility: manually create regexp
    Result := '^(';
    var strs: TStrings := TStringList.Create();
    try
      ExtractStringsEx([';'], [' '], types, strs);
      for var str: string in strs do
        Result := Result + str + '|';
    finally
      strs.Free();
    end;
    Result[System.length(Result)] := ')';
    Result := Result + '$';
  end;
end;

function TBlkSignalTrainEvent.ToFileStr(short: Boolean = false): string;
begin
  Result := '{' + Self.stop.GetDefStr() + '}|';

  if (Self.slow.enabled) then
    Result := Result + '{{' + Self.slow.ev.GetDefStr() + '},' + IntToStr(Self.slow.speed) + '}';
  Result := Result + '|';

  if (not short) then
  begin
    Result := Result + '{re:' + Self.train_type_re + '}|' + IntToStr(Self.length.min) + '|' +
      IntToStr(Self.length.max);
  end;
end;

// ziskavani zpomalovacich a zastavovaich dat ze souboru (parsing dat)
// format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
// : typ.stop(0=usek;1=ir);
// pro usek nasleduje: usekid;usekpart;speed;
// pro ir nasleduje: irid;speed;
class function TBlkSignalTrainEvent.ParseOldRychEvent(trackAllowed: Boolean; str: string): TRREv;
var data: TStrings;
  rrData: TRREvData;
begin
  data := TStringList.Create();

  try
    ExtractStringsEx([';'], [], str, data);

    case (data[0][1]) of
      '0':
        begin
          // usek
          rrData.typ := TRREvType.rrtTrack;
          rrData.trackState := true;
          rrData.trackPart := StrToInt(data[2]);
        end; // case 0

      '1':
        begin
          // ir
          rrData.typ := TRREvType.rrtIR;
          rrData.irId := StrToInt(data[1]);
          rrData.irState := true;
        end; // case 1
    end; // case

    Result := TRREv.Create(trackAllowed, rrData);
  finally
    data.Free();
  end;
end;

function TBlkSignalTrainEvent.Match(train: TTrain): Boolean;
begin
  Result := ((train.length >= Self.length.min) and (train.length <= Self.length.max) and
    (TRegEx.IsMatch(train.typ, Self.train_type_re)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSignal.IsEnabled(): Boolean;
begin
  Result := (Self.signal <> TBlkSignalCode.ncDisabled);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.PstAdd(pst: TBlk);
begin
  if (Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Add(pst);
  Self.Change();
end;

procedure TBlkSignal.PstRemove(pst: TBlk);
begin
  if (not Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Remove(pst);
  if (Self.m_state.psts.Count = 0) then
  begin
    if (Self.signal <> ncStuj) then
      Self.signal := ncStuj;
  end;
  Self.Change();
end;

function TBlkSignal.PstIsActive(): Boolean;
begin
  Self.PstCheckActive();
  for var blk: TBlk in Self.m_state.psts do
    if (TBlkPst(blk).status = pstActive) then
      Exit(true);
  Result := false;
end;

function TBlkSignal.PstIs(): Boolean;
begin
  Self.PstCheckActive();
  Result := (Self.m_state.psts.Count > 0);
end;

procedure TBlkSignal.PstCheckActive();
begin
  for var i := Self.m_state.psts.Count-1 downto 0 do
    if (TBlkPst(Self.m_state.psts[i]).status <= pstOff) then
      Self.PstRemove(self.m_state.psts[i]);
end;

function TBlkSignal.ControllerInBasicPosition(): Boolean;
begin
  if (not Self.m_settings.PSt.enabled) then
    Exit(true);

  try
    Result := (RCSi.GetInput(Self.m_settings.PSt.rcsControllerShunt) <> isOn);
  except
    Result := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSignal.ShowIndication();
begin
  if ((not Self.m_settings.PSt.enabled) or (not RCSi.Started)) then
    Exit();

  try
    if (not Self.PstIsActive()) then
    begin
      RCSi.SetOutput(Self.m_settings.PSt.rcsIndicationShunt, 0);
      Exit();
    end;

    if (Self.changing) then
      RCSi.SetOutput(Self.m_settings.PSt.rcsIndicationShunt, TRCSOutputState.osf240)
    else
      RCSi.SetOutput(Self.m_settings.PSt.rcsIndicationShunt,
        ite((Self.signal = ncPosunZaj) or (Self.signal = ncPosunNezaj), 1, 0));
  except

  end;
end;

procedure TBlkSignal.ReadContollers();
begin
  if ((not Self.m_settings.PSt.enabled) or (not RCSi.Started) or (not Self.PstIsActive()) or (Self.changing)) then
    Exit();

  try
    var state := RCSi.GetInput(Self.m_settings.PSt.rcsControllerShunt);
    if ((state = TRCSInputState.isOn) and (Self.signal <> ncPosunZaj)) then
      Self.signal := ncPosunZaj;
    if ((state = TRCSInputState.isOff) and (Self.signal <> ncStuj)) then
      Self.signal := ncStuj;
  except
    on E: RCSException do Exit();
    on E: Exception do raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlkSignal.DefaultChangeTime(hasRCSoutput: Boolean): string;
begin
  Result := ite(hasRCSoutput, _SIG_DEFAULT_CHANGE_DELAY_OUTPUT, _SIG_DEFAULT_CHANGE_DELAY_NO_OUTPUT);
end;

function TBlkSignal.DefaultChangeTime(): string;
begin
  Result := TBlkSignal.DefaultChangeTime(Self.m_settings.RCSAddrs.Count > 0);
end;

function TBlkSignal.IsSpecificChangeTime(): Boolean;
begin
  Result := (ownConvert.TimeToSecTenths(Self.m_settings.changeTime) <> Self.DefaultChangeTime());
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
