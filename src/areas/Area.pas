﻿unit Area;

{
  TArea class definition.

  Area (station) is a section of railway designed to be controlled
  from one place. Panel can contain multiple areas. Are is identified by
  "house" in the panel.

  TArea provides communication between technological blocks and panels
  (panel server).
}

interface

uses IniFiles, SysUtils, Classes, Graphics, Menus, announcement,
  IdContext, TechnologieRCS, StrUtils, ComCtrls, Forms, AreaLighting,
  Generics.Collections, AreaStack, Windows, Generics.Defaults,
  JsonDataObjects, Logging, UPO;

const
  _MAX_CON_PNL = 16; // max number of panels connected to single area
  _MAX_ORREF = 16;

  // these constants must match constantd defined in clients
  _SND_RAILWAY_REQUEST = 4;
  _SND_PRIVOLAVACKA = 5;
  _SND_TIMEOUT = 6;
  _SND_OVERLOAD = 7;
  _SND_CONF_SEQ = 8;
  _SND_MESSAGE = 9;
  _SND_ERROR = 10;
  _SND_JC_CALL = 11;
  _SND_JC_NO = 12;

type
  TAreaRights = (null = 0, read = 1, write = 2, superuser = 3, other = 4); // 'other' is never saved, just transmitted to panel
  TPanelButton = (F1, F2, ENTER, ESCAPE);

  EMaxClients = class(Exception);
  ENoClientConnected = class(Exception);

  TAreaData = record
    name: string;
    nameShort: string;
    id: string;
    lights: TObjectList<TAreaLighting>;
  end;

  TCSCallback = procedure(Relief: TObject; Panel: TObject; success: Boolean) of object;
  TBlkCallback = procedure(SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton) of object;

  TAreaCountdown = record
    start: TDateTime;
    duration: TDateTime;
    callback: TNotifyEvent;
  end;

  TAreaPanel = record
    Panel: TIDContext;
    rights: TAreaRights;
    user: string;
  end;

  TAreaState = record
    NUZing: Boolean;
    NUZblkCnt: Integer;
    NUZtimerId: Integer;
    shortCircBlkCnt: Integer;
    railwayReqBlkCnt: Integer;
    pnBlkCnt: Integer;
    timerCnt: Integer;
    dkClickCallback: TBlkCallback;
    regPlease: TIDContext; // regulator requesting area for loco
  end;

  TAreaRCSModule = class
    failed: Boolean;
    constructor Create(failed: Boolean = false);
  end;

  TAreaRCSs = class
    modules: TObjectDictionary<Cardinal, TAreaRCSModule>;
    failure: Boolean; // any RCS module failed in area?
    lastFailureTime: TDateTime;

    constructor Create();
    destructor Destroy(); override;
  end;

  /// //////////////////////////////////////////////////////////////////////////

  TArea = class
  public const
    _COM_ACCESS_DENIED = 'Přístup odepřen';

  private
    m_index: Integer; // index in all areas
    m_data: TAreaData;
    m_state: TAreaState;
    m_next_countdown_id: Byte;
    countdowns: TDictionary<Byte, TAreaCountdown>;
    RCSs: TAreaRCSs; // list of RCS addresses in area (based on blocks)

    procedure PanelDbAdd(Panel: TIDContext; rights: TAreaRights; user: string);
    procedure PanelDbRemove(Panel: TIDContext; contextDestroyed: Boolean = false);
    function PanelDbIndex(Panel: TIDContext): Integer;

    procedure NUZTimeOut(Sender: TObject); // NUZ timeout callback
    procedure NUZ_PS(Sender: TIDContext; success: Boolean); // NUZ confirm/deny callback

    procedure ORAuthoriseResponse(Panel: TIDContext; rights: TAreaRights; msg: string; username: string);

    procedure SetNUZBlkCnt(new: Integer);
    procedure SetShortCircBlkCnt(new: Integer);
    procedure SetRailwayReqBlkCnt(new: Integer);
    procedure SetPnBlkCnt(new: Integer);
    procedure SetTimerCnt(new: Integer);

    // Send sumary of faliled modules (module failed in <500 ms are reported at once)
    procedure RCSUpdate();

    procedure SendState(Panel: TIDContext);
    procedure SendLn(Panel: TIDContext; str: string);

    // These function should be called when user rights are changed
    // Main goal = turn on/off appropriate sounds in panel
    procedure AuthReadToWrite(Panel: TIDContext);
    procedure AuthWriteToRead(Panel: TIDContext);
    procedure LastWriteDisconnected();

    procedure OnAnncmntAvailable(Sender: TObject; available: Boolean);
    procedure NUZPrematureZaverRelease(Sender: TObject; data: Integer);
    procedure NUZcancelPrematureEvents();

    procedure SetIndex(newIndex: Integer);

    procedure PanelTrainChangeOk(Sender: TObject; data: Pointer);
    procedure PanelTrainChangeErr(Sender: TObject; data: Pointer);
    procedure PanelTrainCreateErr(Sender: TObject; data: Pointer);

    procedure SetLights(id: string; state: Boolean);

    procedure DkNUZStart(Sender: TIDContext);
    procedure DkNUZStop(Sender: TIDContext);

    procedure DkHvFuncsSetOk(Sender: TObject; data: Pointer);
    procedure DkHvFuncsSetErr(Sender: TObject; data: Pointer);

    procedure DkMenuShowOsv(Sender: TIDContext);
    procedure DkMenuShowLok(Sender: TIDContext);
    procedure ShowDkMenu(Panel: TIDContext; root: string; menustr: string);

    procedure AuthReadersTo(rights: TAreaRights; exception: TIdContext = nil);
    procedure UpdateReadersAuth(exception: TIdContext = nil);

    procedure Log(text: string; level: TLogLevel; source: TLogSource = lsAny);

  public
    stack: TORStack;
    changed: Boolean;
    connected: TList<TAreaPanel>;
    announcement: TStationAnnouncement;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(str: string);
    procedure LoadStat(ini: TMemIniFile; section: string);
    procedure SaveStat(ini: TMemIniFile; section: string);

    function PanelDbRights(Panel: TIDContext): TAreaRights;
    procedure RemoveClient(Panel: TIDContext; contextDestroyed: Boolean = false);

    procedure Update();
    procedure DisconnectPanels();

    function AddCountdown(callback: TNotifyEvent; len: TDateTime): Byte;
    procedure RemoveCountdown(id: Integer);
    function IsCountdown(id: Integer): Boolean;

    procedure RCSAdd(addr: Integer);
    procedure RCSFail(addr: Integer);

    procedure UpdateLine(LI: TListItem);

    // Sends data to all panels with rights >= min_rights
    // Uses area id as message prefix
    procedure BroadcastData(data: string; min_rights: TAreaRights = read);

    // Sends data to all panels with rights >= min_rights
    // Uses '-' id as message prefix
    procedure BroadcastGlobalData(data: string; min_rights: TAreaRights = read);
    procedure BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read; stanice: string = '');

    function AnySuperuserConnected(): Boolean;
    function AnyotherWriteConnected(Sender: TIdContext): Integer;
    function AnyotherWriteOrMoreConnected(Sender: TIdContext): Boolean;

    // --- called from technological blocks ---

    procedure BlkChange(Sender: TObject; specificClient: TIDContext = nil);
    procedure BlkPlaySound(Sender: TObject; min_rights: TAreaRights; sound: Integer; loop: Boolean = false);
    procedure BlkRemoveSound(Sender: TObject; sound: Integer);
    procedure BlkWriteError(Sender: TObject; error: string; system: string; minRights: TAreaRights = TAreaRights.write);
    procedure BlkNewTrain(Sender: TObject; Panel: TIDContext; trainUsekIndex: Integer);
    procedure BlkEditTrain(Sender: TObject; Panel: TIDContext; train: TObject);

    // --- called from areas ---
    procedure SendMessage(Sender: TArea; msg: string);
    procedure ORDKClickServer(callback: TBlkCallback);
    procedure ORDKClickClient();

    function LokoPlease(Sender: TIDContext; user: TObject; comment: string): Integer;
    procedure LokoCancel(Sender: TIDContext);

    procedure OsvInit();

    // --- Communication with panels ---
    procedure PanelAuthorise(Sender: TIDContext; rights: TAreaRights; username: string; password: string);
    procedure PanelFirstGet(Sender: TIDContext);
    procedure PanelClick(Sender: TIDContext; blokid: Integer; Button: TPanelButton; params: string = '');
    procedure PanelEscape(Sender: TIDContext);
    procedure PanelMessage(Sender: TIDContext; recepient: string; msg: string);
    procedure PanelHVList(Sender: TIDContext);
    procedure PanelTrainChange(Sender: TIDContext; trainstr: TStrings);
    procedure PanelMoveLok(Sender: TIDContext; lok_addr: word; new_or: string);
    procedure PanelZAS(Sender: TIDContext; str: TStrings);
    procedure PanelDKClick(SenderPnl: TIDContext; Button: TPanelButton);
    procedure PanelLokoReq(Sender: TIDContext; str: TStrings);
    procedure PanelHlaseni(Sender: TIDContext; str: TStrings);
    procedure PanelDkMenuClick(Sender: TIDContext; rootItem, subItem: string);

    procedure PanelHVAdd(Sender: TIDContext; str: string);
    procedure PanelHVRemove(Sender: TIDContext; addr: Integer);
    procedure PanelHVEdit(Sender: TIDContext; str: string);

    function PanelGetTrains(Sender: TIDContext): string;
    procedure PanelRemoveTrain(Sender: TIDContext; train_index: Integer);

    function GetORPanel(conn: TIDContext; var ORPanel: TAreaPanel): Integer;
    class function GetRightsString(rights: TAreaRights): string;

    procedure UserUpdateRights(user: TObject);
    procedure UserDelete(userid: string);

    procedure GetPtData(json: TJsonObject);

    function IsReadable(panel: TIdContext): Boolean;
    function IsWritable(panel: TIdContext): Boolean;

    procedure AddPNsUPOs(var upos: TUPOItems);
    procedure AddNCsUPOs(var upos: TUPOItems);

    class function ORRightsToString(rights: TAreaRights): string;

    class function NameComparer(): IComparer<TArea>;
    class function IdComparer(): IComparer<TArea>;

    property NUZtimer: Boolean read m_state.NUZing write m_state.NUZing;
    property NUZblkCnt: Integer read m_state.NUZblkCnt write SetNUZBlkCnt;
    property shortCircBlkCnt: Integer read m_state.shortCircBlkCnt write SetShortCircBlkCnt;
    property railwayReqBlkCnt: Integer read m_state.railwayReqBlkCnt write SetRailwayReqBlkCnt;
    property pnBlkCnt: Integer read m_state.pnBlkCnt write SetPnBlkCnt;
    property timerCnt: Integer read m_state.timerCnt write SetTimerCnt;
    property regPlease: TIDContext read m_state.regPlease;

    property name: string read m_data.name;
    property shortName: string read m_data.nameShort;
    property id: string read m_data.id;
    property index: Integer read m_index write SetIndex;
  end;

  function IsReadable(right: TAreaRights): Boolean; overload;
  function IsWritable(right: TAreaRights): Boolean; overload;
  function IsReadable(panel: TAreaPanel): Boolean; overload;
  function IsWritable(panel: TAreaPanel): Boolean; overload;

implementation

/// /////////////////////////////////////////////////////////////////////////////

uses BlockDb, GetSystems, BlockTrack, BlockSignal, fMain, TechnologieJC,
  TJCDatabase, ownConvert, TCPServerPanel, AreaDb, block, THVDatabase, TrainDb,
  UserDb, THnaciVozidlo, Trakce, user, PanelConnData, fRegulator, RegulatorTCP,
  ownStrUtils, train, changeEvent, TechnologieTrakce, ConfSeq, Config, timeHelper,
  BlockCrossing;

function IsReadable(right: TAreaRights): Boolean;
begin
  Result := (right > TAreaRights.null);
end;

function IsWritable(right: TAreaRights): Boolean;
begin
  Result := (right = TAreaRights.write) or (right = TAreaRights.superuser);
end;

function IsReadable(panel: TAreaPanel): Boolean;
begin
  Result := IsReadable(panel.rights);
end;

function IsWritable(panel: TAreaPanel): Boolean;
begin
  Result := IsWritable(panel.rights);
end;

constructor TArea.Create(index: Integer);
begin
  inherited Create();

  Self.m_index := index;

  Self.m_data.lights := TObjectList<TAreaLighting>.Create();
  Self.connected := TList<TAreaPanel>.Create();
  Self.RCSs := TAreaRCSs.Create();

  Self.m_state.dkClickCallback := nil;
  Self.m_state.regPlease := nil;

  Self.stack := TORStack.Create(index, Self);
  Self.changed := false;

  Self.countdowns := TDictionary<Byte, TAreaCountdown>.Create();
  Self.announcement := nil;

  Self.m_next_countdown_id := 0;
end;

destructor TArea.Destroy();
begin
  if (Assigned(Self.announcement)) then
    Self.announcement.Free();

  Self.stack.Free();
  Self.m_data.lights.Free();
  Self.countdowns.Free();
  Self.connected.Free();
  Self.RCSs.Free();

  inherited;
end;

function TArea.IsReadable(panel: TIdContext): Boolean;
begin
  Result := Area.IsReadable(Self.PanelDbRights(panel));
end;

function TArea.IsWritable(panel: TIdContext): Boolean;
begin
  Result := Area.IsWritable(Self.PanelDbRights(panel));
end;

constructor TAreaRCSs.Create();
begin
  inherited;
  Self.modules := TObjectDictionary<Cardinal, TAreaRCSModule>.Create();
end;

destructor TAreaRCSs.Destroy();
begin
  Self.modules.Free();
  inherited;
end;

constructor TAreaRCSModule.Create(failed: Boolean = false);
begin
  inherited Create();
  Self.failed := failed;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.LoadData(str: string);
var
  data_main, lights: TStrings;
  lightsStr: string;
begin
  data_main := TStringList.Create();
  lights := TStringList.Create();

  try
    ExtractStringsEx([';', ','], [], str, data_main);

    if (data_main.Count < 3) then
      raise Exception.Create('Mene nez 3 parametry v popisu oblasti rizeni');

    Self.m_data.name := data_main[0];
    Self.m_data.nameShort := data_main[1];
    Self.m_data.id := data_main[2];

    Self.m_data.lights.Clear();

    lights.Clear();
    if (data_main.Count > 3) then
    begin
      ExtractStringsEx([')'], ['('], data_main[3], lights);
      for lightsStr in lights do
      begin
        try
          Self.m_data.lights.Add(TAreaLighting.Create(lightsStr));
        except
          on E:Exception do
            Self.Log('Načítání osvětlení: '+E.Message, llError, lsData);
        end;
      end;
    end;

    Self.announcement := TStationAnnouncement.Create(Self.id);
    Self.announcement.OnAvailable := Self.OnAnncmntAvailable;
  finally
    FreeAndNil(data_main);
    FreeAndNil(lights);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.LoadStat(ini: TMemIniFile; section: string);
begin
  for var light: TAreaLighting in Self.m_data.lights do
    light.default_state := ini.ReadBool(section, light.name, false);
end;

procedure TArea.SaveStat(ini: TMemIniFile; section: string);
begin
  for var light: TAreaLighting in Self.m_data.lights do
    ini.WriteBool(section, light.name, light.default_state);
end;

/// /////////////////////////////////////////////////////////////////////////////

// tato funkce je vyvolana pri zmene stavu jakehokoliv bloku
procedure TArea.BlkChange(Sender: TObject; specificClient: TIDContext = nil);
var
  msg: string;
begin
  if (Self.connected.Count = 0) then
    Exit();

  msg := 'CHANGE;' + TBlk(Sender).PanelStateString();

  for var areaPanel: TAreaPanel in Self.connected do
  begin
    if (not Area.IsReadable(areaPanel)) then
      continue;
    if ((specificClient <> nil) and (areaPanel.Panel <> specificClient)) then
      continue;

    Self.SendLn(areaPanel.Panel, msg);

    // update menu
    if ((areaPanel.Panel.data as TPanelConnData).menu = Sender) then
      PanelServer.menu(areaPanel.Panel, (Sender as TBlk), Self, (Sender as TBlk).ShowPanelMenu(areaPanel.Panel, Self,
        areaPanel.rights));
  end;
end;

procedure TArea.BlkWriteError(Sender: TObject; error: string; system: string; minRights: TAreaRights = TAreaRights.write);
begin
  for var areaPanel: TAreaPanel in Self.connected do
    if (areaPanel.rights >= minRights) then
      PanelServer.BottomError(areaPanel.Panel, error, Self.shortName, system);
end;

procedure TArea.BlkPlaySound(Sender: TObject; min_rights: TAreaRights; sound: Integer; loop: Boolean = false);
begin
  for var areaPanel: TAreaPanel in Self.connected do
    if (areaPanel.rights >= min_rights) then
      PanelServer.PlaySound(areaPanel.Panel, sound, loop);
end;

procedure TArea.BlkRemoveSound(Sender: TObject; sound: Integer);
begin
  for var areaPanel: TAreaPanel in Self.connected do
    PanelServer.DeleteSound(areaPanel.Panel, sound);
end;

procedure TArea.BlkNewTrain(Sender: TObject; Panel: TIDContext; trainUsekIndex: Integer);
begin
  TPanelConnData(Panel.data).train_new_usek_index := trainUsekIndex;
  TPanelConnData(Panel.data).train_usek := Sender;
  Self.SendLn(Panel, 'SPR-NEW;');
end;

procedure TArea.BlkEditTrain(Sender: TObject; Panel: TIDContext; train: TObject);
begin
  TPanelConnData(Panel.data).train_new_usek_index := -1;
  TPanelConnData(Panel.data).train_edit := TTrain(train);
  TPanelConnData(Panel.data).train_usek := Sender;

  Self.SendLn(Panel, 'SPR-EDIT;' + TTrain(train).GetPanelString());
end;

/// /////////////////////////////////////////////////////////////////////////////
// Panel database maintainance

procedure TArea.PanelDbAdd(panel: TIDContext; rights: TAreaRights; user: string);
begin
  if (rights = TAreaRights.other) then
    rights := TAreaRights.read;

  var panelI: Integer := Self.PanelDbIndex(panel);
  if (panelI > -1) then
  begin
    // pokud uz je zaznam v databazi, pouze upravime tento zaznam
    var pnl: TAreaPanel;
    pnl := Self.connected[panelI];
    var wrToRe: Boolean := ((Area.IsWritable(pnl.rights)) and (not Area.IsWritable(rights)));
    pnl.rights := rights;
    pnl.user := user;
    Self.connected[panelI] := pnl;

    if (wrToRe) then
      Self.LastWriteDisconnected();
    authLog('or', 'reauth', user, Self.id + ' :: ' + Self.GetRightsString(rights));
    Exit();
  end;

  if (Self.connected.Count >= _MAX_CON_PNL) then
    raise EMaxClients.Create('Připojen maximální počet klientů');
  if ((Panel.data as TPanelConnData).areas.Count >= _MAX_ORREF) then
    raise EMaxClients.Create('Připojen maximální OR k jedné stanici');

  var pnl: TAreaPanel;
  pnl.Panel := Panel;
  pnl.rights := rights;
  pnl.user := user;
  Self.connected.Add(pnl);

  authLog('or', 'login', user, Self.id + ' :: ' + Self.GetRightsString(rights));

  // pridame referenci na sami sebe do TIDContext
  (Panel.data as TPanelConnData).areas.Add(Self);

  // odesleme incializacni udaje
  if (Area.IsReadable(rights)) then
  begin
    Self.SendState(Panel);
    Self.stack.NewConnection(Panel);
  end;
end;

procedure TArea.PanelDbRemove(Panel: TIDContext; contextDestroyed: Boolean = false);
begin
  var panelI: Integer := Self.PanelDbIndex(Panel);
  if (panelI > -1) then
  begin
    var wrToRe: Boolean := ((Area.IsWritable(Self.connected[panelI].rights)) and (not Self.AnyotherWriteOrMoreConnected(Panel)));
    authLog('or', 'logout', Self.connected[panelI].user, Self.id);
    Self.connected.Delete(panelI);
    if (wrToRe) then
      Self.LastWriteDisconnected();
    Exit();
  end;

  if (not contextDestroyed) then
    if ((Panel.data as TPanelConnData).areas.Contains(Self)) then
      (Panel.data as TPanelConnData).areas.Remove(Self);
end;

function TArea.PanelDbRights(Panel: TIDContext): TAreaRights;
var
  index: Integer;
begin
  index := Self.PanelDbIndex(Panel);
  if (index < 0) then
    Exit(TAreaRights.null);
  Result := Self.connected[index].rights;
end;

function TArea.PanelDbIndex(Panel: TIDContext): Integer;
begin
  for var i: Integer := 0 to Self.connected.Count - 1 do
    if (Self.connected[i].Panel = Panel) then
      Exit(i);
  Result := -1;
end;

/// /////////////////////////////////////////////////////////////////////////////

// touto funkci panel zada o opravneni
procedure TArea.PanelAuthorise(Sender: TIDContext; rights: TAreaRights; username: string; password: string);
var
  userRights: TAreaRights;
  msg: string;
begin
  // panel se chce odpojit -> vyradit z databaze
  if (rights = TAreaRights.null) then
  begin
    Self.ORAuthoriseResponse(Sender, TAreaRights.null, 'Úspěšně autorizováno - odpojen', '');
    PanelServer.GUIQueueLineToRefresh((Sender.data as TPanelConnData).index);
    if (Self.PanelDbRights(Sender) >= write) then
      Self.AuthWriteToRead(Sender);
    Self.PanelDbRemove(Sender);
    Exit();
  end;

  if (rights = TAreaRights.other) then // just for sure
    rights := TAreaRights.read;

  // tady mame zaruceno, ze panel chce zadat o neco vic, nez null

  var user := UsrDb.GetUser(username);

  if (not Assigned(user)) then
  begin
    userRights := TAreaRights.null;
    msg := 'Uživatel ' + username + ' neexistuje !';
  end else if (user.ban) then
  begin
    userRights := TAreaRights.null;
    msg := 'Uživatel ' + user.username + ' má BAN !';
  end else if (not user.ComparePasswd(password)) then
  begin
    userRights := TAreaRights.null;
    msg := 'Neplatné heslo !';
  end else begin
    userRights := user.GetRights(Self.id);
    if (userRights < rights) then
      msg := 'K této OŘ nemáte oprávnění';
  end;

  var lastRights := Self.PanelDbRights(Sender);

  try
    if (userRights = TAreaRights.null) then
    begin
      Self.PanelDbRemove(Sender);
      Self.ORAuthoriseResponse(Sender, userRights, msg, '');
      PanelServer.GUIQueueLineToRefresh((Sender.data as TPanelConnData).index);
      Exit();
    end;
    if (rights > userRights) then
      rights := userRights;

    if ((not GetFunctions.GetSystemStart) and (rights > TAreaRights.read) and (rights < TAreaRights.superuser)) then
    begin
      // superuser can authorize write even with systems turned off
      Self.PanelDbAdd(Sender, TAreaRights.read, username);
      Self.ORAuthoriseResponse(Sender, TAreaRights.read, 'Nelze autorizovat zápis při vyplých systémech !',
        user.fullName);
      PanelServer.GUIQueueLineToRefresh((Sender.data as TPanelConnData).index);
      Exit();
    end;

    // check if other panels are connected
    // only single panel could be connected with write rights
    var anotherWriteI: Integer := Self.AnyotherWriteConnected(Sender);
    var anotherPanel: TAreaPanel;
    if (anotherWriteI > -1) then
      anotherPanel := Self.connected[anotherWriteI];

    if ((rights = TAreaRights.write) and (anotherWriteI > -1) and (anotherPanel.user = username)) then
    begin
      // same user authorizes -> change his other panel to read
      anotherPanel.rights := TAreaRights.read;
      Self.connected[anotherWriteI] := anotherPanel;
      PanelServer.GUIQueueLineToRefresh(anotherWriteI);
      // ORAuthoriseResponse is sent in UpdateReadersAuth
    end else if ((rights < TAreaRights.superuser) and (anotherWriteI > -1)) then begin
      rights := TAreaRights.other; // this value is never saved to db ('read' if saved, see PanelDbAdd)
      msg := 'Panel již připojen: ';
      var _user: TUser := UsrDB.GetUser(anotherPanel.user);
      if (_user <> nil) then
        msg := msg + _user.fullName + ', ';
      msg := msg + anotherPanel.Panel.Connection.Socket.Binding.PeerIP + '!';
    end else begin
      msg := 'Úspěšně autorizováno';
    end;

    Self.PanelDbAdd(Sender, rights, username);
  except
    on E: EMaxClients do
    begin
      PanelServer.GUIQueueLineToRefresh((Sender.data as TPanelConnData).index);
      Self.ORAuthoriseResponse(Sender, TAreaRights.null, E.Message, user.fullName);
      Exit();
    end;
  end;

  UsrDb.LoginUser(username);
  PanelServer.GUIQueueLineToRefresh((Sender.data as TPanelConnData).index);

  var sendRights: TAreaRights := rights;
  if (rights = TAreaRights.read) and (Self.AnySuperuserConnected()) then
    sendRights := TAreaRights.other;

  Self.ORAuthoriseResponse(Sender, sendRights, msg, user.fullName);

  if ((Area.IsWritable(rights)) and (not Area.IsWritable(lastRights))) then
    Self.AuthReadToWrite(Sender);
  if ((not Area.IsWritable(rights)) and (Area.IsWritable(lastRights))) then
    Self.AuthWriteToRead(Sender);

  Self.UpdateReadersAuth(Sender);
end;

procedure TArea.UpdateReadersAuth(exception: TIdContext = nil);
begin
  var isSuperuser: Boolean := false;
  var isWrite: Boolean := false;
  for var areaPanel in Self.connected do
  begin
    if (areaPanel.rights = TAreaRights.write) then
      isWrite := true;
    if (areaPanel.rights = TAreaRights.superuser) then
      isSuperuser := true;
  end;

  var rights := TAreaRights.read;
  if ((isWrite) or (isSuperuser)) then
    rights := TAreaRights.other;

  Self.AuthReadersTo(rights, exception);
end;

// ziskani stavu vsech bloku v panelu
procedure TArea.PanelFirstGet(Sender: TIDContext);
var
  rights: TAreaRights;
begin
  rights := Self.PanelDbRights(Sender);
  if (rights < read) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  Blocks.GetAreaBlk(Self.id, Sender);

  // zjistime RUC u vsech hnacich vozidel
  for var addr: Integer := 0 to _MAX_ADDR - 1 do
    if ((HVDb[addr] <> nil) and (HVDb[addr].state.Area = Self)) then
      HVDb[addr].UpdatePanelRuc(false);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelClick(Sender: TIDContext; blokid: Integer; Button: TPanelButton; params: string = '');
begin
  if (not IsReadable(Sender)) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  var blk := Blocks.GetBlkByID(blokid);
  if (blk = nil) then
    Exit();

  // musime provest kontrolu, jestli OR ma povoleno menit blok
  // tj. jestli ma technologicky blok toto OR

  for var area: TArea in blk.areas do
  begin
    if (area = Self) then
    begin
      blk.PanelClick(Sender, Self, Button, Self.PanelDbRights(Sender), params);
      Exit();
    end;
  end;

  PanelServer.SendInfoMsg(Sender, 'Nemáte oprávnění měnit tento blok');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelEscape(Sender: TIDContext);
begin
  if (not IsWritable(Sender)) then
  begin
    // ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    // tady se schvalne neposila informace o chybe - aby klienta nespamovala chyba v momente, kdy provadi escape a nema autorizovana vsechna OR na panelu
    Exit();
  end;

  Self.ORDKClickClient();

  var blk := Blocks.GetBlkTrackTrainMoving(Self.id);
  if (blk <> nil) then
    (blk as TBlkTrack).trainMoving := -1;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.DkNUZStart(Sender: TIDContext);
var csItems: TList<TConfSeqItem>;
begin
  csItems := TList<TConfSeqItem>.Create();
  try
    for var blk: TBlk in Blocks do
    begin
      if ((blk.typ = btTrack) and (TBlkTrack(blk).NUZ) and ((blk as TBlkTrack).areas.Contains(Self))) then
      begin
        csItems.Add(CSItem(blk, 'Nouzové vybavování'));
        if (TBlkTrack(blk).occupied = TTrackState.occupied) then
          csItems.Add(CSItem(blk, 'Kolejový úsek obsazen'));
      end;
    end;

    PanelServer.ConfirmationSequence(Sender, Self.NUZ_PS, Self, 'Nouzové uvolnění závěrů úseků',
      GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TArea.DkNUZStop(Sender: TIDContext);
begin
  Self.NUZcancelPrematureEvents();
  Blocks.NUZ(Self.id, false);
  Self.NUZblkCnt := 0; // zastavi mereni casu (melo by zastavit uz volani vyse)
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelMessage(Sender: TIDContext; recepient: string; msg: string);
begin
  if (not IsWritable(Sender)) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  var area: TArea := areas.Get(recepient);
  if (area = nil) then
  begin
    Self.SendLn(Sender, 'MSG-ERR;' + recepient + ';Tato OŘ neexistuje');
    Exit();
  end;

  try
    Area.SendMessage(Self, msg);
  except
    on E: ENoClientConnected do
      Self.SendLn(Sender, 'MSG-ERR;' + recepient + ';K této OŘ aktuálně není připojen žádný panel');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// pozadavek na ziskani sezmu hnacich vozidel
procedure TArea.PanelHVList(Sender: TIDContext);
begin
  // kontrola opravneni klienta
  if (not IsReadable(Sender)) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  var str: string := 'HV;LIST;{';
  for var addr: Integer := 0 to _MAX_ADDR - 1 do
    if ((Assigned(HVDb[addr])) and (HVDb[addr].state.Area = Self)) then
      str := str + '[{' + HVDb[addr].GetPanelLokString(full) + '}]';
  str := str + '}';
  Self.SendLn(Sender, str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelTrainChange(Sender: TIDContext; trainstr: TStrings);
begin
  if (not IsWritable(Sender)) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  if ((TPanelConnData(Sender.data).train_new_usek_index = -1) and (TPanelConnData(Sender.data).train_edit = nil)) then
  begin
    Self.SendLn(Sender, 'SPR-EDIT-ERR;Žádná souprava k editaci / neplatný úsek pro vytvoření soupravy');
    Exit();
  end;

  try
    if (TPanelConnData(Sender.data).train_new_usek_index > -1) then
    begin
      // nova souprava
      Trains.Add(trainstr, TPanelConnData(Sender.data).train_usek, Self,
        (TPanelConnData(Sender.data).train_new_usek_index), TTrakce.callback(Self.PanelTrainChangeOk, Sender),
        TTrakce.callback(Self.PanelTrainCreateErr, Sender));
    end else begin

      // uprava soupravy
      var track: TBlkTrack := (TPanelConnData(Sender.data).train_usek as TBlkTrack);
      var train: TTrain := TPanelConnData(Sender.data).train_edit;

      if (not track.IsTrain(TPanelConnData(Sender.data).train_edit.index)) then
      begin
        Self.SendLn(Sender, 'SPR-EDIT-ERR;Souprava již není na úseku');
        Exit();
      end;

      if ((train.front <> track) and (train.wantedSpeed > 0)) then
      begin
        Self.SendLn(Sender, 'SPR-EDIT-ERR;Nelze upravit soupravu, která odjela a je v pohybu');
        Exit();
      end;

      TPanelConnData(Sender.data).train_edit.UpdateTrainFromPanel(trainstr, track, Self,
        TTrakce.callback(Self.PanelTrainChangeOk, Sender), TTrakce.callback(Self.PanelTrainChangeErr, Sender));
    end;
  except
    on E: Exception do
      Self.SendLn(Sender, 'SPR-EDIT-ERR;' + E.Message);
  end;
end;

procedure TArea.PanelTrainChangeOk(Sender: TObject; data: Pointer);
var
  tcpSender: TIDContext;
begin
  tcpSender := data;
  TPanelConnData(tcpSender.data).ResetTrains();
  Self.SendLn(tcpSender, 'SPR-EDIT-ACK;');
end;

procedure TArea.PanelTrainChangeErr(Sender: TObject; data: Pointer);
var
  tcpSender: TIDContext;
begin
  tcpSender := data;
  Self.SendLn(tcpSender, 'SPR-EDIT-ERR;Nepodařilo se převzít lokomotivy z centrály!');
end;

procedure TArea.PanelTrainCreateErr(Sender: TObject; data: Pointer);
var
  tcpSender: TIDContext;
begin
  tcpSender := data;
  Self.SendLn(tcpSender, 'SPR-EDIT-ERR;Souprava založena, ale nepodařilo se převízt lokomotivy z centrály!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelMoveLok(Sender: TIDContext; lok_addr: word; new_or: string);
begin
  if (not IsWritable(Sender)) then
  begin
    Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';ERR;Přístup odepřen');
    Exit();
  end;

  var new: TArea := areas.Get(new_or);
  if (new = nil) then
  begin
    Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';ERR;Tato OR neexistuje!');
    Exit();
  end;
  if (not Assigned(HVDb[lok_addr])) then
  begin
    Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';ERR;HV neexistuje!');
    Exit();
  end;
  if (HVDb[lok_addr].state.train > -1) then
  begin
    Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';ERR;HV přiřazeno soupravě ' +
      Trains.GetTrainNameByIndex(HVDb[lok_addr].state.train) + '!');
    Exit();
  end;
  if (HVDb[lok_addr].state.Area <> Self) then
  begin
    Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';ERR;HV nepatří této stanici!');
    Exit();
  end;

  HVDb[lok_addr].MoveToArea(new);
  Self.SendLn(Sender, 'HV;MOVE;' + IntToStr(lok_addr) + ';OK');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.Update();
begin
  Self.RCSUpdate();
  Self.stack.Update();

  // aktualizace mereni casu:
  for var id: Byte in Self.countdowns.Keys do
  begin
    var countdown := Self.countdowns[id];
    if (Now >= (countdown.start + countdown.duration)) then
    begin
      var callback: TNotifyEvent := countdown.callback;
      Self.countdowns.Remove(id);
      if (Assigned(callback)) then
        callback(Self);
    end;
  end;
end;

// vraci id pridaneho mereni
function TArea.AddCountdown(callback: TNotifyEvent; len: TDateTime): Byte;
begin
  var id := Self.m_next_countdown_id;

  Self.BroadcastData('CAS;START;' + IntToStr(id) + ';' + FormatDateTime('s', len) + ';');

  var mc: TAreaCountdown;
  mc.start := Now;
  mc.duration := len;
  mc.callback := callback;
  Self.countdowns.Add(id, mc);

  Result := id;
  Inc(Self.m_next_countdown_id); // Inc ignores overflows
end;

procedure TArea.RemoveCountdown(id: Integer);
begin
  if (Self.countdowns.ContainsKey(id)) then
  begin
    Self.countdowns.Remove(id);
    Self.BroadcastData('CAS;STOP;' + IntToStr(id) + ';');
  end;
end;

function TArea.IsCountdown(id: Integer): Boolean;
begin
  Result := Self.countdowns.ContainsKey(id);
end;

/// /////////////////////////////////////////////////////////////////////////////

// zavola se, az probehne meerni casu:
procedure TArea.NUZTimeOut(Sender: TObject);
begin
  Self.NUZcancelPrematureEvents();
  Self.NUZtimer := false;
  Blocks.NUZ(Self.id);
  Self.NUZblkCnt := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.NUZ_PS(Sender: TIDContext; success: Boolean);
begin
  if (not success) then
    Exit();

  Self.m_state.NUZing := true;

  // ruseni pripadnych jiznich cest:
  for var blk: TBlk in Blocks do
  begin
    if (blk.typ <> btTrack) then
      continue;
    var track: TBlKTrack := (blk as TBlkTrack);
    if ((not track.NUZ) or (not track.IsInArea(Self))) then
      continue;

    track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEventInt(Self.NUZPrematureZaverRelease, 0));
    var jc: TJC := JCDb.FindActiveJCWithTrack(blk.id);
    if (jc <> nil) then
    begin
      jc.EmergencyCancelActivePath();
      var signal: TBlkSignal := Blocks.GetBlkSignalByID(jc.data.signalId);
      if (signal.DNjc = jc) then
        signal.DNjc := nil;
    end;
  end;

  Self.BroadcastData('NUZ;2;');
  Self.m_state.NUZtimerId := Self.AddCountdown(Self.NUZTimeOut, EncodeTimeSec(GlobalConfig.times.nuz));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.ORAuthoriseResponse(Panel: TIDContext; rights: TAreaRights; msg: string; username: string);
begin
  Self.SendLn(Panel, 'AUTH;' + IntToStr(Integer(rights)) + ';' + msg + ';' + username);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.RemoveClient(Panel: TIDContext; contextDestroyed: Boolean = false);
begin
  Self.PanelDbRemove(Panel, contextDestroyed);
  Self.stack.OnDisconnect(Panel);
  Self.UpdateReadersAuth(Panel);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetNUZBlkCnt(new: Integer);
begin
  if ((Self.m_state.NUZblkCnt = 0) and (new > 0)) then
  begin
    // zacina NUZ, informovat oblasti rizeni
    Self.BroadcastData('NUZ;1;');
  end;

  if ((Self.m_state.NUZblkCnt > 0) and (new = 0)) then
  begin
    // nekdo si rekl, ze bloky nechce nuzovat
    Self.BroadcastData('NUZ;0;');
    if (Self.NUZtimer) then
    begin
      Self.NUZtimer := false;
      Self.RemoveCountdown(Self.m_state.NUZtimerId);
    end;
  end;

  Self.m_state.NUZblkCnt := new;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetShortCircBlkCnt(new: Integer);
begin
  if (new < 0) then
    Exit();

  if ((new > 2) and (Self.m_state.shortCircBlkCnt = 2)) then
  begin
    // V OR nastal zkrat -> prehrat zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      if (Area.IsWritable(Self.connected[i])) then
        PanelServer.PlaySound(Self.connected[i].Panel, _SND_OVERLOAD, true);
  end;

  if ((new <= 2) and (Self.m_state.shortCircBlkCnt = 2)) then
  begin
    // zkrat skoncil -> vypnout zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      PanelServer.DeleteSound(Self.connected[i].Panel, _SND_OVERLOAD);
  end;

  Self.m_state.shortCircBlkCnt := new;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetRailwayReqBlkCnt(new: Integer);
begin
  if (new < 0) then
    Exit();

  if ((new > 0) and (Self.railwayReqBlkCnt = 0)) then
  begin
    // nastala zadost -> prehrat zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      if (Area.IsWritable(Self.connected[i])) then
        PanelServer.PlaySound(Self.connected[i].Panel, _SND_RAILWAY_REQUEST, true);
  end;

  if ((new = 0) and (Self.railwayReqBlkCnt > 0)) then
  begin
    // skocnila zadost -> vypnout zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      PanelServer.DeleteSound(Self.connected[i].Panel, _SND_RAILWAY_REQUEST);
  end;

  Self.m_state.railwayReqBlkCnt := new;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetPnBlkCnt(new: Integer);
begin
  if (new < 0) then
    Exit();

  if ((new > 0) and (Self.pnBlkCnt = 0)) then
  begin
    // aktivace prvni privolavaci navesti -> prehrat zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      if (Area.IsWritable(Self.connected[i])) then
        PanelServer.PlaySound(Self.connected[i].Panel, _SND_PRIVOLAVACKA, true);
  end;

  if ((new = 0) and (Self.pnBlkCnt > 0)) then
  begin
    // skocnila posledni privolavaci navest -> vypnout zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      PanelServer.DeleteSound(Self.connected[i].Panel, _SND_PRIVOLAVACKA);
  end;

  Self.m_state.pnBlkCnt := new;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetTimerCnt(new: Integer);
begin
  if (new < 0) then
    Exit();

  if ((new > 0) and (Self.timerCnt = 0)) then
  begin
    // aktivace prvniho timeru -> prehrat zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      if (Area.IsWritable(Self.connected[i])) then
        PanelServer.PlaySound(Self.connected[i].Panel, _SND_TIMEOUT, true);
  end;

  if ((new = 0) and (Self.timerCnt > 0)) then
  begin
    // skocnil posledni timer -> vypnout zvuk
    for var i: Integer := 0 to Self.connected.Count - 1 do
      PanelServer.DeleteSound(Self.connected[i].Panel, _SND_TIMEOUT);
  end;

  Self.m_state.timerCnt := new;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.DisconnectPanels();
begin
  for var i: Integer := Self.connected.Count - 1 downto 0 do
  begin
    Self.ORAuthoriseResponse(Self.connected[i].Panel, TAreaRights.null, 'Odpojení systémů', '');
    var index: Integer := (Self.connected[i].Panel.data as TPanelConnData).index;
    Self.PanelDbRemove(Self.connected[i].Panel);
    PanelServer.GUIQueueLineToRefresh(index);
  end;

  Self.stack.Clear();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SendMessage(Sender: TArea; msg: string);
begin
  for var areaPanel: TAreaPanel in Self.connected do
  begin
    if (Area.IsWritable(areaPanel)) then
    begin
      Self.SendLn(areaPanel.Panel, 'MSG;' + Sender.id + ';{' + msg + '}');
      Exit();
    end;
  end;

  raise ENoClientConnected.Create('Nepřipojen žádný klient!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.RCSAdd(addr: Integer);
begin
  try
    if (not Self.RCSs.modules.ContainsKey(addr)) then
      Self.RCSs.modules.Add(addr, TAreaRCSModule.Create(false));
  except

  end;
end;

procedure TArea.RCSFail(addr: Integer);
begin
  try
    if (not Self.RCSs.modules.ContainsKey(addr)) then
      Exit();
    Self.RCSs.modules[addr].failed := true;
    Self.RCSs.failure := true;
    Self.RCSs.lastFailureTime := Now;
  except

  end;
end;

procedure TArea.RCSUpdate();
begin
  if (not Self.RCSs.failure) then
    Exit();

  if ((Self.RCSs.lastFailureTime + EncodeTime(0, 0, 0, 500)) < Now) then
  begin
    var str: string := 'Výpadek RCS modulu ';
    for var addr: Integer in Self.RCSs.modules.Keys do
      if (Self.RCSs.modules[addr].failed) then
      begin
        str := str + IntToStr(addr) + ', ';
        Self.RCSs.modules[addr].failed := false;
      end;

    str := LeftStr(str, Length(str) - 2);
    Self.RCSs.failure := false;

    for var panel: TAreaPanel in Self.connected do
      if (panel.rights >= read) then
        PanelServer.BottomError(panel.Panel, str, Self.shortName, 'TECHNOLOGIE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.UpdateLine(LI: TListItem);
begin
  LI.Caption := IntToStr(Self.index);
  LI.SubItems.Strings[0] := Self.name;
  LI.SubItems.Strings[1] := Self.shortName;
  LI.SubItems.Strings[2] := Self.id;
  var str: string := Self.stack.GetList();
  LI.SubItems.Strings[3] := RightStr(str, Length(str) - 2);

  case (Self.stack.mode) of
    TORStackMode.PV:
      LI.SubItems.Strings[4] := 'PV';
    TORStackMode.VZ:
      LI.SubItems.Strings[4] := 'VZ';
  end;

  str := '';
  for var i: Integer := 0 to Self.m_data.lights.Count - 1 do
    str := str + '(' + Self.m_data.lights[i].name + ' - ' + IntToStr(Self.m_data.lights[i].rcsAddr.board) + ':' +
      IntToStr(Self.m_data.lights[i].rcsAddr.port) + ')';
  LI.SubItems.Strings[5] := str;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelZAS(Sender: TIDContext; str: TStrings);
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  Self.stack.ParseCommand(Sender, str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.SetLights(id: string; state: Boolean);
begin
  for var light: TAreaLighting in Self.m_data.lights do
    if (light.name = id) then
    begin
      try
        RCSi.SetOutput(light.rcsAddr, ownConvert.BoolToInt(state));
        light.default_state := state;
      except

      end;

      Exit();
    end;
end;

procedure TArea.OsvInit();
begin
  try
    for var light: TAreaLighting in Self.m_data.lights do
      if (RCSi.IsModule(light.rcsAddr.board)) then
        RCSi.SetOutput(light.rcsAddr, ownConvert.BoolToInt(light.default_state));
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TArea.PanelGetTrains(Sender: TIDContext): string;
begin
  if (Self.PanelDbRights(Sender) < read) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit('');
  end;

  Result := '{';
  for var i: Integer := 0 to _MAX_TRAIN - 1 do
    if ((Assigned(Trains[i])) and (Trains[i].station = Self)) then
      Result := Result + '[{' + Trains[i].GetPanelString() + '}]';
  Result := Result + '}';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelRemoveTrain(Sender: TIDContext; train_index: Integer);
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  if ((Trains[train_index] <> nil) and (Trains[train_index].station = Self)) then
  begin
    Trains.Remove(train_index);
    PanelServer.SendInfoMsg(Sender, 'Souprava smazána');
    Exit();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelHVAdd(Sender: TIDContext; str: string);
var
  HV: THV;
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    Self.SendLn(Sender, 'HV;ADD;-;ERR;Přístup odepřen');
    Exit();
  end;

  try
    HV := HVDb.Add(str, Self);
  except
    on E: Exception do
    begin
      Self.SendLn(Sender, 'HV;ADD;-;ERR;' + E.Message);
      Exit();
    end;
  end;

  Self.SendLn(Sender, 'HV;ADD;' + HV.addrStr + ';OK');
end;

procedure TArea.PanelHVRemove(Sender: TIDContext; addr: Integer);
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    Self.SendLn(Sender, 'HV;REMOVE;' + IntToStr(addr) + ';ERR;Přístup odepřen');
    Exit();
  end;
  if (HVDb[addr] = nil) then
  begin
    Self.SendLn(Sender, 'HV;REMOVE;' + IntToStr(addr) + ';ERR;Loko neexsituje');
    Exit();
  end;
  if (HVDb[addr].state.Area <> Self) then
  begin
    Self.SendLn(Sender, 'HV;REMOVE;' + IntToStr(addr) + ';ERR;Loko se nenachází ve stanici ' + Self.name);
    Exit();
  end;

  try
    HVDb.Remove(addr);
  except
    on E: Exception do
      Self.SendLn(Sender, 'HV;REMOVE;' + IntToStr(addr) + ';ERR;' + E.Message);
  end;

  Self.SendLn(Sender, 'HV;REMOVE;' + IntToStr(addr) + ';OK');
end;

procedure TArea.PanelHVEdit(Sender: TIDContext; str: string);
var addr: Integer;
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    Self.SendLn(Sender, 'HV;EDIT;-;ERR;Přístup odepřen');
    Exit();
  end;

  var data: TStrings := TStringList.Create();
  addr := 0;
  try
    ExtractStringsEx(['|'], [], str, data);
    addr := StrToInt(data[4]);
    data.Free();
    if (HVDb[addr] = nil) then
    begin
      Self.SendLn(Sender, 'HV;EDIT;' + IntToStr(addr) + ';ERR;Loko neexistuje');
      Exit();
    end;
    if (HVDb[addr].state.Area <> Self) then
    begin
      Self.SendLn(Sender, 'HV;EDIT;' + IntToStr(addr) + ';ERR;Loko se nenachází ve stanici ' + Self.name);
      Exit();
    end;

    HVDb[addr].UpdateFromPanelString(str);

    if (HVDb[addr].acquired) then
      HVDb[addr].StateFunctionsToSlotFunctions(TTrakce.callback(), TTrakce.callback());

    Self.SendLn(Sender, 'HV;EDIT;' + IntToStr(addr) + ';OK');
  except
    on E: Exception do
    begin
      Self.SendLn(Sender, 'HV;EDIT;' + IntToStr(addr) + ';ERR;' + E.Message);
      if (Assigned(data)) then
        data.Free();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.BroadcastData(data: string; min_rights: TAreaRights = read);
begin
  for var panel: TAreaPanel in Self.connected do
    if (panel.rights >= min_rights) then
      Self.SendLn(panel.Panel, data);
end;

procedure TArea.BroadcastGlobalData(data: string; min_rights: TAreaRights = read);
begin
  for var panel: TAreaPanel in Self.connected do
    if (panel.rights >= min_rights) then
      PanelServer.SendLn(panel.Panel, '-;' + data);
end;

procedure TArea.BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read; stanice: string = '');
begin
  if (stanice = '') then
    stanice := Self.shortName;

  for var panel: TAreaPanel in Self.connected do
    if (panel.rights >= min_rights) then
      PanelServer.BottomError(panel.Panel, err, stanice, tech);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.ORDKClickServer(callback: TBlkCallback);
begin
  Self.m_state.dkClickCallback := callback;
  Self.BroadcastData('DK-CLICK;1', TAreaRights.write);
end;

procedure TArea.ORDKClickClient();
begin
  if (not Assigned(Self.m_state.dkClickCallback)) then
    Exit();

  Self.m_state.dkClickCallback := nil;
  Self.BroadcastData('DK-CLICK;0', TAreaRights.write);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelDKClick(SenderPnl: TIDContext; Button: TPanelButton);
begin
  if (Assigned(Self.m_state.dkClickCallback)) then
    Self.m_state.dkClickCallback(SenderPnl, Self, Button);
end;

// Tato procedura parsuje "LOK-REQ" z panelu.
procedure TArea.PanelLokoReq(Sender: TIDContext; str: TStrings);
begin
  // kontrola opravneni klienta
  var rights: TAreaRights := Self.PanelDbRights(Sender);
  if (not Area.IsWritable(rights)) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  str[2] := UpperCase(str[2]);

  // zadost o vydani tokenu
  // odpovedi:
  // or;LOK-TOKEN;OK;[addr|token][addr|token] - odpověď na žádost o token, je posílano také při RUČ loko
  // or;LOK-TOKEN;ERR;addr1|addr2...;comment  - chybova odpoved na zadost o token
  if (str[2] = 'PLEASE') then
  begin
    // parsing loko
    try
      var data: TStrings := TStringList.Create();
      ExtractStringsEx(['|'], [], str[3], data);

      // zkontrolujeme vsechna LOKO
      for var i: Integer := 0 to data.Count - 1 do
      begin
        var HV: THV := HVDb[StrToInt(data[i])];
        if (HV = nil) then
        begin
          Self.SendLn(Sender, 'LOK-TOKEN;ERR;' + str[3] + ';Loko ' + data[i] + ' neexistuje');
          Exit();
        end;

        // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
        if ((HV.state.Area <> Self) and (rights <> TAreaRights.superuser)) then
        begin
          Self.SendLn(Sender, 'LOK-TOKEN;ERR;' + str[3] + ';Loko ' + data[i] + ' se nenachází ve stanici');
          Exit();
        end;

        // nelze vygenerovat token pro loko, ktere je uz v regulatoru
        if ((HV.state.regulators.Count > 0) and (rights <> TAreaRights.superuser)) then
        begin
          Self.SendLn(Sender, 'LOK-TOKEN;ERR;' + str[3] + ';Loko ' + data[i] + ' již otevřeno v regulátoru');
          Exit();
        end;
      end; // for i

      // kontrola OK -> generujeme zpravu z tokeny a zpravu odesleme
      var line: string := 'LOK-TOKEN;OK;';
      for var i: Integer := 0 to data.Count - 1 do
      begin
        var HV: THV := HVDb[StrToInt(data[i])];
        line := line + '[' + IntToStr(HV.addr) + '|' + HV.GetToken() + ']';
      end;
      Self.SendLn(Sender, line);

      data.Free();
    except
      Self.SendLn(Sender, 'LOK-TOKEN;ERR;Neplatný formát argumentů');
    end;
  end

  // klient vybral lokomotivy pro rucni rizeni
  // odpovedi, ktere muzu poslat panelu:
  // or;LOK-REQ;OK                           - seznam loko na rucni rizeni schvalen serverem
  // or;LOK-REQ;ERR;comment                  - seznam loko na rucni rizeni odmitnut serverem
  else if (str[2] = 'LOK') then
  begin
    try
      // nejdriv musi probihat zadost o loko
      if (Self.m_state.regPlease = nil) then
      begin
        Self.SendLn(Sender, 'LOK-REQ;ERR;Neprobíhá žádná žádost z regulátoru');
        Exit();
      end;

      var data: TStrings := TStringList.Create();
      ExtractStringsEx(['|'], [], str[3], data);

      // zkontrolujeme vsechna LOKO
      for var i: Integer := 0 to data.Count - 1 do
      begin
        var HV: THV := HVDb[StrToInt(data[i])];
        if (HV = nil) then
        begin
          Self.SendLn(Sender, 'LOK-REQ;ERR;Loko ' + data[i] + ' neexistuje');
          Exit();
        end;

        // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
        if ((HV.state.Area <> Self) and (rights <> TAreaRights.superuser)) then
        begin
          Self.SendLn(Sender, 'LOK-REQ;ERR;Loko ' + data[i] + ' se nenachází ve stanici');
          Exit();
        end;

        // nelze vygenerovat token pro loko, ktere je uz v regulatoru
        if ((HV.state.regulators.Count > 0) and (rights <> TAreaRights.superuser)) then
        begin
          Self.SendLn(Sender, 'LOK-REQ;ERR;Loko ' + data[i] + ' již otevřeno v regulátoru');
          Exit();
        end;
      end; // for i

      // kontrola OK -> odesleme panelu zpravu o tom, ze je vse OK
      Self.SendLn(Sender, 'LOK-REQ;OK;');

      // vsem ostatnim panelum jeste posleme, ze doslo ke zruseni zadosti
      for var i: Integer := 0 to Self.connected.Count - 1 do
        if ((Area.IsReadable(Self.connected[i])) and (Self.connected[i].Panel <> Sender)) then
          Self.SendLn(Self.connected[i].Panel, 'LOK-REQ;CANCEL;');

      // lokomotivy priradime regulatoru
      for var i: Integer := 0 to data.Count - 1 do
      begin
        var HV: THV := HVDb[StrToInt(data[i])];
        TCPRegulator.LokToRegulator(Self.m_state.regPlease, HV);
      end;

      // zrusit zadost regulatoru
      (Self.m_state.regPlease.data as TPanelConnData).regulator_zadost := nil;
      Self.m_state.regPlease := nil;

      data.Free();
    except
      Self.SendLn(Sender, 'LOK-REQ;ERR;Neplatný formát argumentů');
    end;
  end

  // relief odmitl zadost regulatoru o lokomotivu
  else if (str[2] = 'DENY') then
  begin
    PanelServer.SendLn(Self.m_state.regPlease, '-;LOK;G;PLEASE-RESP;err;Dispečer odmítl žádost');
    Self.BroadcastData('LOK-REQ;CANCEL;');
    (Self.m_state.regPlease.data as TPanelConnData).regulator_zadost := nil;
    Self.m_state.regPlease := nil;
  end

  // or;LOK-REQ;U-PLEASE;blk_id;train_index      - zadost o vydani seznamu hnacich vozidel na danem useku
  // mozne odpovedi:
  // or;LOK-REQ;U-OK;[hv1][hv2]...           - seznamu hnacich vozidel v danem useku
  // or;LOK-REQ;U-ERR;info                   - chyba odpoved na pozadavek na seznam loko v danem useku

  else if (str[2] = 'U-PLEASE') then
  begin
    try
      var track: TBlkTrack := Blocks.GetBlkTrackOrRTByID(StrToInt(str[3]));
      if (track = nil) then
      begin
        Self.SendLn(Sender, 'LOK-REQ;U-ERR;Neplatný blok');
        Exit();
      end;

      if (not track.IsTrain()) then
      begin
        Self.SendLn(Sender, 'LOK-REQ;U-ERR;Žádná souprava na bloku');
        Exit();
      end;

      var traini: Integer := -1;
      if (str.Count > 4) then
      begin
        traini := StrToIntDef(str[4], -1);
        if ((traini < -1) or (traini >= track.Trains.Count)) then
        begin
          Self.SendLn(Sender, 'LOK-REQ;U-ERR;Tato souprava na úseku neexistuje');
          Exit();
        end;
      end;

      // generujeme zpravu s tokeny
      var line: string := 'LOK-REQ;U-OK;{';
      if (traini = -1) then
      begin
        // vsechny soupravy na useku
        for var j: Integer := 0 to track.Trains.Count - 1 do
          for var addr: Integer in Trains[track.Trains[j]].HVs do
            line := line + '[{' + HVDb[addr].GetPanelLokString() + '}]';
      end else begin
        // konkretni souprava
        for var addr: Integer in Trains[track.Trains[traini]].HVs do
          line := line + '[{' + HVDb[addr].GetPanelLokString() + '}]';
      end;

      line := line + '}';
      Self.SendLn(Sender, line);

    except
      Self.SendLn(Sender, 'LOK-REQ;U-ERR;Neplatný formát argumentů');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// odesle status oblasti rizeni po prihlaseni klienta
procedure TArea.SendState(Panel: TIDContext);
begin
  // kliknuti na dopravni kancelar
  if (Assigned(Self.m_state.dkClickCallback)) then
    Self.SendLn(Panel, 'DK-CLICK;1')
  else
    Self.SendLn(Panel, 'DK-CLICK;0');

  // pripradna zadost o lokomotivu
  if (Self.regPlease <> nil) then
  begin
    var user: TUser := (Self.regPlease.data as TPanelConnData).regulator_user;
    if (user <> nil) then
      Self.SendLn(Panel, 'LOK-REQ;REQ;' + user.username + ';' + user.firstname + ';' + user.lastname + ';');
  end;

  if ((Assigned(Self.announcement)) and (Self.announcement.available)) then
    Self.SendLn(Panel, 'SHP;AVAILABLE;1');

  if ((Self.NUZblkCnt > 0) and (not Self.NUZtimer)) then
    Self.SendLn(Panel, 'NUZ;1;');
end;

procedure TArea.SendLn(Panel: TIDContext; str: string);
begin
  PanelServer.SendLn(Panel, Self.id + ';' + str);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TArea.GetORPanel(conn: TIDContext; var ORPanel: TAreaPanel): Integer;
begin
  for var i: Integer := 0 to Self.connected.Count - 1 do
  begin
    if (Self.connected[i].Panel = conn) then
    begin
      ORPanel := Self.connected[i];
      Exit(0);
    end;
  end;
  Result := 1;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TArea.GetRightsString(rights: TAreaRights): string;
begin
  case (rights) of
    TAreaRights.null:
      Result := 'null';
    TAreaRights.read:
      Result := 'read';
    TAreaRights.write:
      Result := 'write';
    TAreaRights.superuser:
      Result := 'superuser';
    TAreaRights.other:
      Result := 'other';
  else
    Result := '';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// je volano v pripade, ze dojde ke zmene opravenni za behu programu
procedure TArea.UserUpdateRights(user: TObject);
begin
  for var areaPanel: TAreaPanel in Self.connected do
  begin
    // je pripojeny uzivatel s vyssimi opravevnimi, nez jsou mu pridelena?
    var rights: TAreaRights := TUser(user).GetRights(Self.id);
    if ((areaPanel.user = TUser(user).username) and ((areaPanel.rights > rights) or (TUser(user).ban))) then
    begin
      if (TUser(user).ban) then
        rights := TAreaRights.null;
      Self.PanelDbAdd(areaPanel.Panel, rights, TUser(user).username);
      Self.ORAuthoriseResponse(areaPanel.Panel, rights, 'Snížena oprávnění uživatele', '');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.UserDelete(userid: string);
begin
  for var i: Integer := Self.connected.Count - 1 downto 0 do
  begin
    if (Self.connected[i].user = userid) then
    begin
      Self.ORAuthoriseResponse(Self.connected[i].Panel, TAreaRights.null, 'Uživatel smazán', '');
      Self.PanelDbRemove(Self.connected[i].Panel);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vraci 1 pokud zadost jiz probiha
// vraci 0 pokud prikaz probehl vporadku
function TArea.LokoPlease(Sender: TIDContext; user: TObject; comment: string): Integer;
begin
  if (Self.m_state.regPlease <> nil) then
    Exit(1);
  Self.m_state.regPlease := Sender;

  var str: string := 'LOK-REQ;REQ;' + TUser(user).username + ';';
  if (TUser(user).firstname <> '') then
    str := str + TUser(user).firstname + ';'
  else
    str := str + '-;';
  if (TUser(user).lastname <> '') then
    str := str + TUser(user).lastname + ';'
  else
    str := str + '-;';
  if (comment <> '') then
    str := str + comment + ';'
  else
    str := str + '-;';

  Self.BroadcastData(str);

  Result := 0;
end;

procedure TArea.LokoCancel(Sender: TIDContext);
begin
  if (Self.m_state.regPlease = nil) then
    Exit();
  Self.m_state.regPlease := nil;
  Self.BroadcastData('LOK-REQ;CANCEL;');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.AuthReadToWrite(Panel: TIDContext);
begin
  if (Self.shortCircBlkCnt > 2) then
    PanelServer.PlaySound(Panel, _SND_OVERLOAD, true);
  if (Self.railwayReqBlkCnt > 0) then
    PanelServer.PlaySound(Panel, _SND_RAILWAY_REQUEST, true);
  if (Self.pnBlkCnt > 0) then
    PanelServer.PlaySound(Panel, _SND_PRIVOLAVACKA, true);
  if (Self.timerCnt > 0) then
    PanelServer.PlaySound(Panel, _SND_TIMEOUT, true);
end;

procedure TArea.AuthWriteToRead(Panel: TIDContext);
begin
  if (Self.shortCircBlkCnt > 2) then
    PanelServer.DeleteSound(Panel, _SND_OVERLOAD);
  if (Self.railwayReqBlkCnt > 0) then
    PanelServer.DeleteSound(Panel, _SND_RAILWAY_REQUEST);
  if (Self.pnBlkCnt > 0) then
    PanelServer.DeleteSound(Panel, _SND_PRIVOLAVACKA);
  if (Self.timerCnt > 0) then
    PanelServer.DeleteSound(Panel, _SND_TIMEOUT);
  Self.stack.OnWriteToRead(Panel);

  // This will hide path blocks in other panel areas, but it's still better than not hiding anything
  // (we don't know which blocks to hide)
  TPanelConnData(Panel.Data).ClearAndHidePathBlocks();
end;

procedure TArea.LastWriteDisconnected();
begin
  for var block in Blocks do
  begin
    if ((not block.IsInArea(Self)) or (block.AnyWritableClientConnected())) then
      continue;

    if ((block.typ = TBlkType.btSignal) and (TBlkSignal(block).targetSignal = TBlkSignalCode.ncPrivol)) then
      TBlkSignal(block).signal := ncStuj; // cancel PN

    if ((block.typ = TBlkType.btTrack) and (TBlkTrack(block).NUZ)) then
      TBlkTrack(block).NUZ := False; // cancel NUZ

    if ((block.typ = TBlkType.btCrossing) and (TBlkCrossing(block).pcEmOpen)) then
      TBlkCrossing(block).pcEmOpen := False; // cancel NOT
  end;
end;

procedure TArea.AuthReadersTo(rights: TAreaRights; exception: TIdContext = nil);
begin
  for var areaPanel in Self.connected do
  begin
    if (areaPanel.Panel = exception) then
      continue;
    if (areaPanel.rights = TAreaRights.read) then // intentionally omitting superuser (superuser stays)
    begin
      var user := UsrDb.GetUser(areaPanel.user);
      var name: string := '';
      if (Assigned(user)) then
        name := user.fullName;
      Self.ORAuthoriseResponse(areaPanel.Panel, rights, '', name);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TArea.ORRightsToString(rights: TAreaRights): string;
begin
  case (rights) of
    TAreaRights.null:
      Result := 'žádné oprávnění';
    TAreaRights.read, TAreaRights.other:
      Result := 'oprávnění k pozorování';
    TAreaRights.write:
      Result := 'oprávnění k řízení';
    TAreaRights.superuser:
      Result := 'superuživatel';
  else
    Result := '';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TArea.OnAnncmntAvailable(Sender: TObject; available: Boolean);
begin
  if (available) then
    Self.BroadcastData('SHP;AVAILABLE;1')
  else
    Self.BroadcastData('SHP;AVAILABLE;0');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelHlaseni(Sender: TIDContext; str: TStrings);
begin
  if (not Assigned(Self.announcement)) then
    Exit();
  if (str.Count < 3) then
    Exit();

  // kontrola opravneni klienta
  if (Self.PanelDbRights(Sender) < write) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  str[2] := UpperCase(str[2]);

  if (str[2] = 'SPEC') then
  begin
    Self.announcement.Special(str[3]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.NUZPrematureZaverRelease(Sender: TObject; data: Integer);
begin
  if (Self.NUZblkCnt > 0) then
    Self.NUZblkCnt := Self.NUZblkCnt - 1;
end;

procedure TArea.NUZcancelPrematureEvents();
begin
  for var blk: TBlk in Blocks do
  begin
    if (blk.typ <> btTrack) then
      continue;
    var track: TBlkTrack := blk as TBlkTrack;
    if (not track.NUZ) then
      continue;
    for var area: TArea in track.areas do
      if (area = Self) then
        track.RemoveChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEventInt(Self.NUZPrematureZaverRelease, 0));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetIndex(newIndex: Integer);
begin
  if (Self.index = newIndex) then
    Exit();
  Self.stack.index := newIndex;
  Self.m_index := newIndex;
end;

////////////////////////////////////////////////////////////////////////////////

class function TArea.NameComparer(): IComparer<TArea>;
begin
  Result := TComparer<TArea>.Construct(
    function(const Left, Right: TArea): Integer
    begin
      Result := CompareStr(Left.name, Right.name, loUserLocale);
    end);
end;

class function TArea.IdComparer(): IComparer<TArea>;
begin
  Result := TComparer<TArea>.Construct(
    function(const Left, Right: TArea): Integer
    begin
      Result := CompareStr(Left.id, Right.id, loUserLocale);
    end);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelDkMenuClick(Sender: TIDContext; rootItem, subItem: string);
begin
  if (Self.PanelDbRights(Sender) < write) then
  begin
    PanelServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    Exit();
  end;

  if (subItem = '') then
  begin
    // Root item
    if (rootItem = 'OSV') then
      Self.DkMenuShowOsv(Sender)
    else if (rootItem = 'LOKO') then
      Self.DkMenuShowLok(Sender)
    else if (rootItem = 'NUZ>') then
      Self.DkNUZStart(Sender)
    else if (rootItem = 'NUZ<') then
      Self.DkNUZStop(Sender);
  end else begin
    // Non-root item
    if (rootItem = 'OSV') then
      Self.SetLights(LeftStr(subItem, Length(subItem) - 1), (subItem[Length(subItem)] = '>'))
    else if (rootItem = 'LOKO') then
    begin
      if ((subItem = 'ZVUK>') or (subItem = 'ZVUK<')) then
      begin
        PanelServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
        TrakceI.LoksSetFunc(_SOUND_FUNC, (subItem = 'ZVUK>'), TTrakce.callback(Self.DkHvFuncsSetOk, Sender),
          TTrakce.callback(Self.DkHvFuncsSetErr, Sender));
      end else if (subItem = 'ZVUK ztlum') then
      begin
        PanelServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
        TrakceI.TurnOffSound(TTrakce.callback(Self.DkHvFuncsSetOk, Sender),
          TTrakce.callback(Self.DkHvFuncsSetErr, Sender))
      end else if (subItem = 'ZVUK obnov') then
      begin
        PanelServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
        TrakceI.RestoreSound(TTrakce.callback(Self.DkHvFuncsSetOk, Sender),
          TTrakce.callback(Self.DkHvFuncsSetErr, Sender));
      end;
    end;
  end;
end;

procedure TArea.DkMenuShowOsv(Sender: TIDContext);
var
  menustr: string;
begin
  menustr := '$' + Self.name + ',$Osvětlení,-,';
  for var light: TAreaLighting in Self.m_data.lights do
  begin
    menustr := menustr + light.name;
    if (light.active) then
      menustr := menustr + '<,'
    else
      menustr := menustr + '>,';
  end;
  Self.ShowDkMenu(Sender, 'OSV', menustr);
end;

procedure TArea.DkMenuShowLok(Sender: TIDContext);
var
  menustr: string;
begin
  menustr := '-,';

  if (not HVDb.AllAcquiredHVsHaveActiveFunc(_SOUND_FUNC)) then
    menustr := menustr + 'ZVUK>,';
  if (HVDb.AnyAcquiredHVHasActiveFunc(_SOUND_FUNC)) then
    menustr := menustr + 'ZVUK<,ZVUK ztlum,';
  if (HVDb.AnyHvToRestoreFunc(_SOUND_FUNC)) then
    menustr := menustr + 'ZVUK obnov,';

  if (menustr = '-,') then
    menustr := '';
  Self.ShowDkMenu(Sender, 'LOKO', menustr);
end;

procedure TArea.ShowDkMenu(Panel: TIDContext; root: string; menustr: string);
begin
  Self.SendLn(Panel, 'MENU;' + root + ';{' + menustr + '}');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.DkHvFuncsSetOk(Sender: TObject; data: Pointer);
var
  Panel: TIDContext;
begin
  Panel := TIDContext(data);
  PanelServer.SendInfoMsg(Panel, 'Funkce nastaveny.');
end;

procedure TArea.DkHvFuncsSetErr(Sender: TObject; data: Pointer);
var
  Panel: TIDContext;
begin
  Panel := TIDContext(data);
  PanelServer.BottomError(Panel, 'Nepodařilo se nastavit zvuky hnacích vozidel!', Self.shortName, 'Trakce');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.GetPtData(json: TJsonObject);
begin
  json['id'] := Self.id;
  json['name'] := Self.name;
  json['shortName'] := Self.shortName;
end;

////////////////////////////////////////////////////////////////////////////////

function TArea.AnySuperuserConnected(): Boolean;
begin
  Result := false;
  for var areaPanel: TAreaPanel in Self.connected do
    if (areaPanel.rights = TAreaRights.superuser) then
      Exit(true);
end;

function TArea.AnyotherWriteConnected(Sender: TIdContext): Integer;
begin
  // Intentionally omitting superuser
  Result := -1;
  for var i: Integer := 0 to Self.connected.Count-1 do
    if ((Self.connected[i].Panel <> Sender) and (Self.connected[i].rights = TAreaRights.write)) then
      Exit(i);
end;

function TArea.AnyotherWriteOrMoreConnected(Sender: TIdContext): Boolean;
begin
  for var i: Integer := 0 to Self.connected.Count-1 do
    if ((Self.connected[i].Panel <> Sender) and (Area.IsWritable(Self.connected[i].rights))) then
      Exit(True);
  Result := False;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.Log(text: string; level: TLogLevel; source: TLogSource);
begin
  Logging.log(Self.id + ': ' + text, level, source);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.AddPNsUPOs(var upos: TUPOItems);
begin
  var blocks: TList<TBlk> := Blocks.PNSignals(Self);
  try
    for var blk: TBlk in blocks do
      upos.Add(UPO.PNUPO(blk.name));
  finally
    blocks.Free();
  end;
end;

procedure TArea.AddNCsUPOs(var upos: TUPOItems);
begin
  for var path: TJC in JCDb do
    if ((path.ncActive) and (path.signal <> nil) and (path.signal.areas.Contains(Self))) then
      upos.Add(UPO.NCUPO(path.name));
end;

////////////////////////////////////////////////////////////////////////////////

end.
