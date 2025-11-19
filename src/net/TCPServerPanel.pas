unit TCPServerPanel;

{ TPanelServer is hJOPserver's main TCP server.
  Is communicates with papnels, regulators, station announcement etc.

  Full specification of TCP protocol is available in czech at:
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer. }

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal, SyncObjs, IniFiles,
  Classes, StrUtils, Graphics, Windows, Area, ExtCtrls, ConfSeq,
  IdContext, Block, ComCtrls, IdSync, UPO, PanelConnData, IdSocketHandle,
  User, Train, Generics.Collections, TRailVehicle, predvidanyOdjezd;

const
  _DEFAULT_PORT = 5896;
  _MAX_CLIENTS = 64;
  _PING_TIMER_PERIOD_MS = 250;
  _RECEIVE_CHECK_PERIOD_MS = 15;
  _CONFIG_SECTION = 'PanelServer';

  // tady jsou vyjmenovane vsechny verze protokolu, ktere akceptuje server od klientu
  _PROTO_V_ACCEPT: array [0 .. 1] of string = ('1.0', '1.1');

type
  TPanelConnectionState = (closed, opening, handshake, opened);
  TTCPEvent = (evMessage, evDisconnect);

  EInvalidButton = class(Exception);

  TPanelClient = class
    connection: TIdContext;
    state: TPanelConnectionState;
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane

    constructor Create(conn: TIdContext; status: TPanelConnectionState = handshake);
  end;

  TPanelReceived = class
    AContext: TIdContext;
    orsRef: TPanelConnData;
    parsed: TStrings;
    event: TTCPEvent;

    constructor Create();
    destructor Destroy(); override;
  end;

  TPanelServer = class
  private const
    _PROTOCOL_VERSION = '1.1';

  private
    receiveTimer: TTimer; // must be executed in main thread synchronously!
    received: TObjectQueue<TPanelReceived>; // locked by receivedLock!
    receivedLock: TCriticalSection;

    clients: array [0 .. _MAX_CLIENTS - 1] of TPanelClient;
    tcpServer: TIdTCPServer;
    data: string; // prijata data v plain-text forme
    m_DCCStopped: TIdContext; // tady je ulozeno ID spojeni, ktere zazadalo o CentralStop
    // vsechny panely maji standartne moznost vypnout DCC
    // pokud to udela nejaky panel, ma moznost DCC zapnout jen tento panel
    // pokud vypne DCC nekdo ze serveru, nebo z ovladace, zadny klient nema moznost ho zapnout
    refreshQueue: array [0 .. _MAX_CLIENTS - 1] of Boolean;
    pingTimer: TTimer;

    procedure OnTcpServerConnect(AContext: TIdContext); // event pripojeni klienta z TIdTCPServer
    procedure OnTcpServerDisconnect(AContext: TIdContext); // event odpojeni klienta z TIdTCPServer
    procedure OnTcpServerDisconnectMainThread(AContext: TIdContext; orsRef: TPanelConnData);
    procedure OnTcpServerExecute(AContext: TIdContext); // event akce klienta z TIdTCPServer

    procedure ParseGlobal(AContext: TIdContext; parsed: TStrings); // parsinag dat s globalnim prefixem: "-;"
    procedure ParseOR(AContext: TIdContext; parsed: TStrings); // parsing dat s prefixem konkretni oblasti rizeni
    procedure Auth(AContext: TIdContext; parsed: TStrings); // pozadavek na autorizaci OR, data se ziskavaji z \parsed

    function IsOpenned(): Boolean; // je server zapnut?
    function GetBind(): string; overload;

    procedure OnDCCCmdErr(Sender: TObject; data: Pointer); // event chyby komunikace s lokomotivou v automatu
    procedure CheckPing(Sender: TObject);
    procedure ProcessReceivedMessages();
    procedure OnReceiveTimerTick(Sender: TObject);
    procedure SetDCCStopped(who: TIdContext);

    property DCCStopped: TIdContext read m_DCCStopped write SetDCCStopped;

  public

    constructor Create();
    destructor Destroy(); override;

    procedure LoadConfig(ini: TMemIniFile);
    procedure SaveConfig(ini: TMemIniFile);

    procedure Start();
    procedure Stop();
    procedure DisconnectClient(conn: TIdContext);

    // volani funkci do panelu, ktere neprislusi OR, ale jednotlivym panelum
    procedure SendInfoMsg(AContext: TIdContext; msg: string);
    procedure Note(AContext: TIdContext; blk: TBlk; note: string); overload;
    procedure Note(AContext: TIdContext; blk: TBlk; note: string; rights: TAreaRights); overload;
    procedure Lockout(AContext: TIdContext; blk: TBlk; lockout: string); overload;
    procedure Lockout(AContext: TIdContext; blk: TBlk; lockout: string; rights: TAreaRights); overload;
    procedure Menu(AContext: TIdContext; Blk: TBlk; Area: TArea; Menu: string);

    procedure ConfirmationSequence(AContext: TIdContext; callback: TCSCallback; Area: TArea; event: string;
      senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
    procedure InfoWindow(AContext: TIdContext; callback: TCSCallback; Area: TArea; event: string;
      senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
    procedure CSWindow(AContext: TIdContext; mode: string; callback: TCSCallback; Area: TArea; event: string;
      senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
    procedure CSClose(AContext: TIdContext; msg: string = '');
    procedure CSWindowClose(AContext: TIdContext; mode: string; msg: string = '');

    procedure UPO(AContext: TIdContext; items: TUPOItems; critical: Boolean; callbackOK: TNotifyEvent;
      callbackEsc: TNotifyEvent; ref: TObject);
    procedure CancelUPO(AContext: TIdContext; ref: TObject);
    procedure POdj(AContext: TIdContext; SenderBlk: TBlk; SenderTrainId: Integer; POdj: TPOdj = nil);

    // Tyto funkce take muzou byt volany z oblasti rizeni, protoze nemusi byt
    // primou reakci na akci uzivatele - chceme je odeslat vsem.

    // Prehravani zvuku neprehrava a nezacina prehravat zvuky primo, pamatuje si pocitadlo prehravani jednotlivych zvuku
    // a prehravani meni jen pokud se pocitadlo mezi z 0 na 1 resp. z 1 na 0.
    // To je k tomu, aby ruzne OR mohly volat tyto funkce bez dali kontroly nezavisle.
    // Priority prehravani zvuku resi klient.
    procedure PlaySound(AContext: TIdContext; code: Integer; loop: Boolean = false);
    procedure DeleteSound(AContext: TIdContext; code: Integer);

    procedure BottomError(AContext: TIdContext; err: string; stanice: string; tech: string);

    procedure BroadcastBottomError(err: string; tech: string);
    procedure BroadcastData(data: string);
    procedure BroadcastFuncsDescription();

    procedure SendLn(AContext: TIdContext; str: string);

    procedure GUIInitTable();
    procedure GUIRefreshLine(index: Integer; repaint: Boolean = true);
    procedure GUIQueueLineToRefresh(lineindex: Integer);
    procedure GUIRefreshTable();
    procedure GUIRefreshFromQueue();

    procedure DCCStart();
    procedure DCCStop();

    function GetClient(index: Integer): TPanelClient;
    procedure DisconnectRegulatorUser(User: TUser);
    function StrToPanelButton(button: string): TPanelButton;
    procedure OnRemoveTrain(Train: TTrain);
    function GetBindings(): TIdSocketHandles;
    function IsBind(ip: string): Boolean;
    function GetBind(ip: string): TIdSocketHandle; overload;
    function GetBindOrZeroBind(ip: string): TIdSocketHandle;
    procedure BlockRemoved(blk: TBlk);

    property openned: Boolean read IsOpenned;
    property bindingsStr: string read GetBind;
  end;

var
  PanelServer: TPanelServer;

implementation

uses fMain, BlockTrack, BlockTurnout, BlockSignal, AreaDb, BlockLinker,
  BlockCrossing, Logging, TimeModel, TrainDb, Config, TrakceC, TrakceIFace,
  BlockLock, RegulatorTCP, ownStrUtils, FunkceVyznam, RCSdebugger,
  UDPDiscover, TJCDatabase, TechnologieJC, BlockAC, ACBlocks, BlockDb,
  BlockDisconnector, BlockIO, ownConvert, TRVDatabase, BlockPst, TCPServerPT;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelReceived.Create();
begin
  inherited;
  Self.parsed := TStringList.Create();
end;

destructor TPanelReceived.Destroy();
begin
  Self.parsed.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelServer.Create();
begin
  inherited Create();

  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    Self.clients[i] := nil;

  Self.received := TObjectQueue<TPanelReceived>.Create();
  Self.receivedLock := TCriticalSection.Create();

  Self.pingTimer := TTimer.Create(nil);
  Self.pingTimer.Enabled := false;
  Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
  Self.pingTimer.OnTimer := Self.CheckPing;

  Self.tcpServer := TIdTCPServer.Create(nil);
  Self.tcpServer.OnConnect := Self.OnTcpServerConnect;
  Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
  Self.tcpServer.OnExecute := Self.OnTcpServerExecute;

  Self.receiveTimer := TTimer.Create(nil);
  Self.receiveTimer.Enabled := false;
  Self.receiveTimer.Interval := _RECEIVE_CHECK_PERIOD_MS;
  Self.receiveTimer.OnTimer := Self.OnReceiveTimerTick;

  Self.m_DCCStopped := nil;
end;

destructor TPanelServer.Destroy();
begin
  try
    if (Self.tcpServer.Active) then
      Self.tcpServer.Active := false;

    if (Assigned(Self.tcpServer)) then
      FreeAndNil(Self.tcpServer);

    Self.receivedLock.Acquire(); // wait for everything to end
    FreeAndNil(Self.received);
    Self.receivedLock.Release();
    Self.receivedLock.Free();

    Self.receiveTimer.Free();
    Self.pingTimer.Free();
  finally
    inherited;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.LoadConfig(ini: TMemIniFile);
begin
  var strBinds: string := '';
  if (ini.ValueExists(_CONFIG_SECTION, 'bind')) then
    strBinds := ini.ReadString(_CONFIG_SECTION, 'bind', '0.0.0.0:'+IntToStr(_DEFAULT_PORT))
  else
    strBinds := '0.0.0.0:' + IntToStr(ini.ReadInteger(_CONFIG_SECTION, 'port', _DEFAULT_PORT));

  Self.tcpServer.Bindings.Clear();
  var binds: TStrings := TStringList.Create();
  var bind: TStrings := TStringList.Create();
  try
    ExtractStringsEx([' '], [','], strBinds, binds);
    for var strBind: string in binds do
    begin
      bind.Clear();
      ExtractStringsEx([':'], [], strBind, bind);
      if (bind.Count = 2) then
      begin
        var ip: string := bind[0];
        var port: Word := StrToIntDef(bind[1], 0);
        if (port <> 0) then
        begin
          var binding: TIdSocketHandle := Self.tcpServer.Bindings.Add();
          binding.IP := ip;
          binding.Port := port;
        end;
      end;
    end;

  finally
    bind.Free();
    binds.Free();
  end;
end;

procedure TPanelServer.SaveConfig(ini: TMemIniFile);
begin
  ini.WriteString(_CONFIG_SECTION, 'bind', Self.bindingsStr);
  ini.DeleteKey(_CONFIG_SECTION, 'port'); // old definition; new = bind
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.Start();
begin
  if (Self.tcpServer.Active) then
    Exit();

  F_Main.S_Server.Brush.Color := clGray;
  logging.Log('Panel server: spouštění '+Self.bindingsStr+' ...', TLogLevel.llInfo, TLogSource.lsPanelServer, True);

  try
    Self.tcpServer.Active := true;
  except
    on E: Exception do
    begin
      F_Main.S_Server.Brush.Color := clRed;
      logging.Log('Panel server: chyba při startování serveru : ' + E.Message, TLogLevel.llError, TLogSource.lsPanelServer, True);
      raise;
    end;
  end;

  Self.receiveTimer.Enabled := true;
  Self.pingTimer.Enabled := true;

  F_Main.S_Server.Brush.Color := clLime;
  logging.Log('Panel server: spuštěn', TLogLevel.llInfo, TLogSource.lsPanelServer, True);

  UDPdisc.SendDiscover();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.Stop();
var iA: Integer;
  Context: TIdContext;
begin
  if ((SystemData.status = stopping) and (not Self.openned)) then
  begin
    F_Main.A_Turnoff_FunctionsExecute(Self);
    Exit();
  end;

  if (not Self.tcpServer.Active) then
    Exit();

  logging.Log('Panel server: vypínám...', TLogLevel.llInfo, TLogSource.lsPanelServer, True);
  F_Main.S_Server.Brush.Color := clGray;

  Self.pingTimer.Enabled := false;
  Self.receiveTimer.Enabled := false;

  with Self.tcpServer.Contexts.LockList do
    try
      for iA := Count - 1 downto 0 do
      begin
        Context := items[iA];
        if Context = nil then
          Continue;
        Context.connection.IOHandler.WriteBufferClear;
        Context.connection.IOHandler.InputBuffer.Clear;
        Context.connection.IOHandler.Close;
        if Context.connection.Connected then
          Context.connection.Disconnect;
      end;
    finally
      Self.tcpServer.Contexts.UnlockList;
    end;

  Self.tcpServer.Active := false;

  Self.ProcessReceivedMessages();
  PanelServer.GUIRefreshTable();
  F_Main.S_Server.Brush.Color := clRed;
  UDPdisc.SendDiscover();
  ACBlk.RemoveAllClients();

  if (SystemData.status = stopping) then
    F_Main.A_Turnoff_FunctionsExecute(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TPanelServer.OnTcpServerConnect(AContext: TIdContext);
begin
  Self.tcpServer.Contexts.LockList();
  try
    AContext.connection.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8;

    var i: Integer;
    for i := 0 to _MAX_CLIENTS - 1 do
      if (Self.clients[i] = nil) then
        break;

    // na serveru neni misto -> odpojit klienta
    if (i = _MAX_CLIENTS) then
    begin
      // tady bych mohl napsat chybovou hlasku
      Self.SendInfoMsg(AContext, 'Připojeno maximum klientů');
      AContext.connection.Disconnect();
      Exit();
    end;

    AContext.data := TPanelConnData.Create(i);
    Self.clients[i] := TPanelClient.Create(AContext);
    Self.GUIQueueLineToRefresh(i);
  finally
    Self.tcpServer.Contexts.UnlockList();
  end;
end;

procedure TPanelServer.OnTcpServerDisconnect(AContext: TIdContext);
begin
  Self.tcpServer.Contexts.LockList();
  try
    for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    begin
      if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].connection)) then
      begin
        FreeAndNil(Self.clients[i]);
        break;
      end;
    end;
  finally
    Self.tcpServer.Contexts.UnlockList();
  end;

  receivedLock.Acquire();
  try
    var received: TPanelReceived := TPanelReceived.Create();
    received.AContext := AContext;
    received.event := evDisconnect;
    received.orsRef := TPanelConnData(AContext.data);
    Self.received.Enqueue(received);
    AContext.data := nil;
  finally
    receivedLock.Release();
  end;
end;

procedure TPanelServer.OnTcpServerDisconnectMainThread(AContext: TIdContext; orsRef: TPanelConnData);
begin
  // Warning: AContext is destroyed, only address is left.
  // vymazeme klienta ze vsech oblasti rizeni
  for var area: TArea in orsRef.areas do
    Area.RemoveClient(AContext, true);
  orsRef.areas.Clear();

  // ukoncime probihajici potvrzovaci sekvenci
  if (Assigned(orsRef.potvr)) then
  begin
    orsRef.potvr(AContext, false);
    orsRef.potvr := nil;
  end;

  // ukoncime pripadne UPO
  if (Assigned(orsRef.UPO_Esc)) then
  begin
    orsRef.UPO_Esc(Self);
    orsRef.UPO_Esc := nil;
    orsRef.UPO_OK := nil;
    orsRef.UPO_ref := nil;
  end;

  orsRef.ClearAndHidePathBlocks();

  // zrusime pripadnou zadost o lokomotivu
  if (orsRef.regulator_zadost <> nil) then
    orsRef.regulator_zadost.LokoCancel(AContext);

  // odpojeni vsech pripadne neodpojenych regulatoru
  if (orsRef.regulator) then
    TCPRegulator.RegDisconnect(AContext, true);

  // vymazeme klienta z RCS debuggeru
  RCSd.RemoveClient(AContext);

  Blocks.OnClientDisconnect(AContext);
  ACBlk.OnClientDisconnect(AContext);

  // odpojil se klient, ktery zpusobil stop dcc -> dcc muze zapnout kdokoliv
  if (Self.DCCStopped = AContext) then
  begin
    Self.m_DCCStopped := nil; // do not assign to DCCStopped, because AContext is already destroyed
    Self.BroadcastData('-;DCC;STOP');
  end;

  for var area: TArea in orsRef.st_hlaseni do
    if (Assigned(area.announcement)) then
      area.announcement.ClientDisconnect(AContext);

  for var jc: TJC in JCdb do
    if (jc.state.SenderPnl = AContext) then
      jc.ClientDisconnect(AContext);

  if (orsRef.pkey_block <> nil) then
  begin
    orsRef.pkey_block.PanelKey(AContext, '', False);
    orsRef.pkey_block := nil;
  end;

  orsRef.Free();

  // aktualizujeme radek v tabulce klientu ve F_Main
  if (Self.tcpServer.Active) then
    PanelServer.GUIRefreshTable();
end;

/// /////////////////////////////////////////////////////////////////////////////
// This function is executed in separate thread for each client!

procedure TPanelServer.OnTcpServerExecute(AContext: TIdContext);
begin
  if (not AContext.connection.Connected) then
    Exit();

  if (AContext.connection.IOHandler.InputBufferIsEmpty) then
  begin
    IndySleep(1);
    Exit();
  end;

  receivedLock.Acquire();
  if (not Assigned(Self.received)) then
    Exit(); // everything is shutting down

  try
    var received: TPanelReceived := TPanelReceived.Create();
    received.AContext := AContext;
    received.event := evMessage;
    received.orsRef := TPanelConnData(AContext.data);

    data := AContext.connection.IOHandler.ReadLn();
    ExtractStringsEx([';'], [#13, #10], data, received.parsed);

    if (received.parsed.Count = 0) then
    begin
      received.Free();
      Exit();
    end else if (received.parsed.Count > 1) then
      received.parsed[1] := UpperCase(received.parsed[1]);

    Self.received.Enqueue(received);
  finally
    receivedLock.Release();
  end;
end;

procedure TPanelServer.OnReceiveTimerTick(Sender: TObject);
begin
  Self.ProcessReceivedMessages();
end;

procedure TPanelServer.ProcessReceivedMessages();
begin
  if (not Assigned(Self.received)) then
    Exit(); // everything is shutting down

  receivedLock.Acquire();

  try
    while (Self.received.Count > 0) do
    begin
      var received: TPanelReceived := Self.received.Extract();
      try
        case (received.event) of
          evMessage:
            begin
              if (received.parsed[0] = '-') then
                Self.ParseGlobal(received.AContext, received.parsed)
              else
                Self.ParseOR(received.AContext, received.parsed);
            end;

          evDisconnect:
            Self.OnTcpServerDisconnectMainThread(received.AContext, received.orsRef);
        end;
      except

      end;
    end;
  finally
    receivedLock.Release();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.ParseGlobal(AContext: TIdContext; parsed: TStrings);
var connData: TPanelConnData;
begin
  connData := (AContext.data as TPanelConnData);

  // najdeme klienta v databazi
  var clientIndex: Integer;
  for clientIndex := 0 to _MAX_CLIENTS - 1 do
    if ((Assigned(Self.clients[clientIndex])) and (Self.clients[clientIndex].connection = AContext)) then
      break;

  // pokud pripojeni neni v databazi, je neco sakra spatne
  if (clientIndex = _MAX_CLIENTS) then
    Exit();

  // parse handhake
  if (parsed[1] = 'HELLO') then
  begin
    // kontrola verze protokolu
    var found: Boolean := false;
    for var j: Integer := 0 to Length(_PROTO_V_ACCEPT) - 1 do
    begin
      if (parsed[2] = _PROTO_V_ACCEPT[j]) then
      begin
        found := true;
        break;
      end;
    end;

    if (not found) then
    begin
      Self.BottomError(AContext, 'Nepodporovaná verze protokolu.', '-', 'PROTOKOL');
      Self.DisconnectClient(AContext);
      Exit();
    end;

    Self.clients[clientIndex].state := TPanelConnectionState.opened;
    Self.SendLn(AContext, '-;HELLO;' + _PROTOCOL_VERSION);

    // oznamime verzi komunikacniho protokolu
    connData.protocol_version := parsed[2];
    F_Main.LV_Clients.items[connData.index].SubItems[F_Main._LV_CLIENTS_COL_PROTOCOL] := parsed[2];

    // jmeno klienta
    if (parsed.Count >= 4) then
      connData.client_name := parsed[3]
    else
      connData.client_name := '';
    F_main.LV_Clients.Items[connData.index].SubItems[F_Main._LV_CLIENTS_COL_APP] := connData.client_name;

    PanelServer.GUIQueueLineToRefresh(connData.index);
    modelTime.SendTimeToPanel(AContext);

    if (trakce.TrackStatusSafe() = tsOn) then
      Self.SendLn(AContext, '-;DCC;GO')
    else if ((Self.DCCStopped <> nil) or (trakce.TrackStatusSafe() <> tsOff)) then
      Self.SendLn(AContext, '-;DCC;DISABLED')
    else
      Self.SendLn(AContext, '-;DCC;STOP');

    Exit();
  end;

  // vsechny nasledujici prikazy jsou podminene tim, ze probehl handshake
  if (Self.clients[clientIndex].state < TPanelConnectionState.opened) then
    Exit();

  if (parsed[1] = 'PONG') then
  begin
    if (parsed.Count >= 3) then
      connData.PongReceived(StrToInt(parsed[2]));

  end else if (parsed[1] = 'STIT') then
  begin
    var note: string := '';
    if (parsed.Count >= 3) then
      note := parsed[2];

    if (connData.Note = nil) then
      Exit();
    case (connData.Note.typ) of
      btTrack, btRT:
        (connData.Note as TBlkTrack).Note := note;
      btTurnout:
        (connData.Note as TBlkTurnout).Note := note;
      btLinker:
        (connData.Note as TBlkLinker).Note := note;
      btCrossing:
        (connData.Note as TBlkCrossing).Note := note;
      btLock:
        (connData.Note as TBlkLock).Note := note;
      btDisconnector:
        (connData.Note as TBlkDisconnector).Note := note;
      btIO:
        (connData.Note as TBlkIO).Note := note;
      btPst:
        (connData.note as TBlkPst).Note := note;
    end; // case
    connData.Note := nil;
  end

  else if (parsed[1] = 'VYL') then
  begin
    var lockout: string := '';
    if (parsed.Count >= 3) then
      lockout := parsed[2];

    if (connData.lockout = nil) then
      Exit();
    case (connData.lockout.typ) of
      btTrack, btRT:
        (connData.lockout as TBlkTrack).SetLockout(AContext, lockout);
      btTurnout:
        (connData.lockout as TBlkTurnout).SetLockout(AContext, lockout);
    end;
    connData.lockout := nil;
  end

  else if ((parsed[1] = 'PS') or (parsed[1] = 'IS')) then
  begin
    if (not Assigned(connData.potvr)) then
      Exit();

    connData.potvr(AContext, (parsed[2] = '2'));
    connData.potvr := nil;
  end

  else if (parsed[1] = 'MENUCLICK') then
  begin
    if (connData.Menu = nil) then
      Exit();
    var blk: TBlk := connData.Menu;
    connData.Menu := nil; // musi byt v tomto poradi - pri volani menu do bloku uz musi byt menu = nil

    var rights := connData.menu_or.PanelDbRights(AContext);
    if (not IsReadable(rights)) then
    begin
      PanelServer.SendInfoMsg(AContext, TArea._COM_ACCESS_DENIED);
      Exit();
    end;

    if (not blk.AcceptsMenuClick(AContext, connData.menu_or, rights, parsed[2])) then
    begin
      PanelServer.SendInfoMsg(AContext, 'Neplatná volba');
      Exit();
    end;

    if (parsed.Count > 2) then
      blk.PanelMenuClick(AContext, connData.menu_or, parsed[2], StrToIntDef(parsed[3], -1), rights)
    else
      blk.PanelMenuClick(AContext, connData.menu_or, parsed[2], -1, rights);
  end

  else if (parsed[1] = 'CLICK') then
  begin
    try
      const btn = Self.StrToPanelButton(parsed[2]);
      if (btn = TPanelButton.ESCAPE) then
        connData.Escape(AContext);
    except
      on E: EInvalidButton do
        Exit();
    end;

  end else if (parsed[1] = 'OR-LIST') then
    areas.SendORList(AContext)

  else if (parsed[1] = 'UPO') then
  begin
    if (parsed[2] = 'OK') then
    begin
      if (Assigned(connData.UPO_OK)) then
        connData.UPO_OK(AContext);
    end else if (parsed[2] = 'ESC') then
      if (Assigned(connData.UPO_Esc)) then
        connData.UPO_Esc(AContext);

    connData.UPO_OK := nil;
    connData.UPO_Esc := nil;
    connData.UPO_ref := nil;
  end // if parsed[2] = 'UPO'

  else if (parsed[1] = 'MOD-CAS') then
    modelTime.Parse(parsed)

  else if (parsed[1] = 'DCC') then
  begin
    if ((parsed[2] = 'GO') and (trakce.TrackStatusSafe() <> tsOn)) then
    begin
      try
        trakce.SetTrackStatus(tsOn, TTrakce.callback(), TTrakce.callback(Self.OnDCCCmdErr, AContext));
      except
        on E: Exception do
          Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
      end;
    end else if ((parsed[2] = 'STOP') and (trakce.TrackStatusSafe() = tsOn)) then
    begin
      Self.DCCStopped := AContext;
      try
        trakce.SetTrackStatus(tsOff, TTrakce.callback(), TTrakce.callback(Self.OnDCCCmdErr, AContext));
      except
        on E: Exception do
          Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
      end;
    end;
  end

  else if (parsed[1] = 'SPR-LIST') then
  begin
    var trains: string := '';
    for var area: TArea in connData.areas do
      trains := trains + area.PanelGetTrains(AContext);
    Self.SendLn(AContext, '-;SPR-LIST;' + trains);
  end

  else if (parsed[1] = 'SPR-REMOVE') then
  begin
    var i: Integer := Trains.GetTrainIndexByName(parsed[2]);
    if (i >= 0) then
      (Trains[i].station as TArea).PanelRemoveTrain(AContext, i);

    var trains: string := '';
    for var area: TArea in connData.areas do
      trains := trains + area.PanelGetTrains(AContext);
    Self.SendLn(AContext, '-;SPR-LIST;' + trains);
  end

  else if (parsed[1] = 'LOK') then
    TCPRegulator.Parse(AContext, parsed)

  else if (parsed[1] = 'F-VYZN-GET') then
  begin
    connData.funcsVyznamReq := true;
    Self.SendLn(AContext, '-;F-VYZN-LIST;{' + FuncNames.PanelStr(';') + '}');
  end

  else if (parsed[1] = 'F-VYZN-ADD') then
  begin
    FuncNames.Add(parsed[2]);
    Self.BroadcastFuncsDescription();
  end

  else if (parsed[1] = 'RCSD') then
    RCSd.Parse(AContext, parsed)

  else if (parsed[1] = 'MAUS') then
  begin
    TPanelConnData(AContext.data).maus := (parsed[2] = '1');
    if ((Assigned(TPanelConnData(AContext.data).Menu)) and ((TPanelConnData(AContext.data).Menu.typ = btTrack) or
      (TPanelConnData(AContext.data).Menu.typ = btRT))) then
      TPanelConnData(AContext.data).Menu.Change();
  end

  else if (parsed[1] = 'PODJ') then
  begin
    if (TPanelConnData(AContext.data).podj_track <> nil) then
    begin
      var POdj: TPOdj := nil;
      try
        POdj := TPOdj.Create(parsed[2], parsed[3]);
        (TPanelConnData(AContext.data).podj_track as TBlkTrack).POdjChanged(TPanelConnData(AContext.data).podj_trainid,
          POdj); // sets podj to nil if takes ownership
      except
        on E: Exception do
          Self.SendInfoMsg(AContext, 'Nepodařilo se nastavit předvídaný odjezd: ' + E.Message);
      end;
      if (POdj <> nil) then
        POdj.Free();

      TPanelConnData(AContext.data).podj_track := nil;
      TPanelConnData(AContext.data).podj_trainid := -1;
    end;

  end else if (parsed[1] = 'AC') then
  begin
    if (parsed.Count < 3) then
      Exit();
    if (parsed[2] = '-') then
    begin
      if ((parsed.Count >= 4) and (UpperCase(parsed[3]) = 'BLOCKS')) then
        ACBlk.ParseBlocksMessage(AContext, parsed);
    end else begin
      var i: Integer := StrToInt(parsed[2]);
      var blk: TBlk := Blocks.GetBlkByID(i);
      if ((Blk <> nil) and (Blk.typ = btAC)) then
        TBlkAC(Blk).ClientParse(AContext, parsed)
      else
        Self.SendLn(AContext, '-;AC;' + parsed[2] + ';ERR;Neplatné id AC');
    end;
  end else if (parsed[1] = 'HV') and (parsed[2] = 'ASK') then
  begin
    var i: Integer := StrToInt(parsed[3]);
    if (RVDb[i] <> nil) then
      PanelServer.SendLn(AContext, '-;HV;ASK;' + IntToStr(i) + ';FOUND;{' + RVDb[i].GetPanelLokString() + '}')
    else
      PanelServer.SendLn(AContext, '-;HV;ASK;' + IntToStr(i) + ';NOT-FOUND');

  end else if (parsed[1] = 'SPEED-STEPS-REQ') then begin
    PanelServer.SendLn(AContext, '-;SPEED-STEPS;{'+trakce.SpeedTableToStr()+'}');

  end else if ((parsed[1] = 'PKEY') and (parsed.Count >= 4)) then begin
    const pkey_block = TPanelConnData(AContext.data).pkey_block;
    if (pkey_block <> nil) then
      pkey_block.PanelKey(AContext, LowerCase(parsed[2]), ownConvert.StrToBool(parsed[3]));

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.ParseOR(AContext: TIdContext; parsed: TStrings);
var
  connData: TPanelConnData;
begin
  connData := (AContext.data as TPanelConnData);
  if (parsed.Count < 2) then
    Exit();

  // nejdriv se podivame, jestli nahodou nechce nekdo autorizaci
  if (parsed[1] = 'AUTH') then
  begin
    Self.Auth(AContext, parsed);
    Exit();
  end else if (parsed[1] = 'SH') then
  begin
    try
      var area: TArea := areas.Get(parsed[0]);
      if (Assigned(Area)) then
      begin
        if (Assigned(area.announcement)) then
          area.announcement.Parse(AContext, Area, parsed)
        else
          Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2] + '-RESPONSE;ERR;INTERNAL_ERROR');
      end else if (parsed.Count > 2) then
        Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2] + '-RESPONSE;ERR;NONEXISTING_OR');
    except
      if (parsed.Count > 2) then
        Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2] + '-RESPONSE;ERR;INTERNAL_ERROR')
    end;

    Exit();
  end;

  // vsechna ostatni data pak podlehaji znalosti OR, ktere mam autorizovane, tak z toho vyjdeme

  var area: TArea := areas.Get(parsed[0]);
  if (Area = nil) then
  begin
    Self.SendInfoMsg(AContext, 'Neautorizováno');
    Exit();
  end;

  if (parsed[1] = 'GET-ALL') then
    area.PanelFirstGet(AContext)

  else if (parsed[1] = 'CLICK') then
  begin
    try
      const btn = Self.StrToPanelButton(parsed[2]);

      if (parsed.Count > 4) then
        area.PanelClick(AContext, StrToInt(parsed[3]), btn, parsed[4])
      else
        area.PanelClick(AContext, StrToInt(parsed[3]), btn);

      if (btn = ESCAPE) then
        connData.Escape(AContext);
    except
      on E: EInvalidButton do
        Exit();
    end;

  end else if (parsed[1] = 'MSG') then
    area.PanelMessage(AContext, parsed[2], parsed[3])

  else if (parsed[1] = 'SPR-CHANGE') then
  begin
    parsed.Delete(0);
    parsed.Delete(0);
    area.PanelTrainChange(AContext, parsed);
  end

  else if (parsed[1] = 'MENUCLICK') then
  begin
    if (parsed.Count > 3) then
      area.PanelDkMenuClick(AContext, parsed[2], parsed[3])
    else
      area.PanelDkMenuClick(AContext, parsed[2], '');
  end

  else if (parsed[1] = 'HV') then
  begin
    if (parsed[2] = 'ADD') then
      area.PanelRVAdd(AContext, parsed[3])
    else if (parsed[2] = 'REMOVE') then
      area.PanelRVRemove(AContext, StrToInt(parsed[3]))
    else if (parsed[2] = 'EDIT') then
      area.PanelRVEdit(AContext, parsed[3])
    else if (parsed[2] = 'MOVE') then
      area.PanelRVMove(AContext, StrToInt(parsed[3]), parsed[4])
    else if (parsed[2] = 'LIST') then
      area.PanelRVList(AContext)
  end

  else if (parsed[1] = 'ZAS') then
    area.PanelZAS(AContext, parsed)

  else if (parsed[1] = 'DK-CLICK') then
    area.PanelDKClick(AContext, TPanelButton(StrToInt(parsed[2])))

  else if (parsed[1] = 'LOK-REQ') then
    area.PanelLokoReq(AContext, parsed)

  else if (parsed[1] = 'SHP') then
    area.PanelHlaseni(AContext, parsed);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.SendInfoMsg(AContext: TIdContext; msg: string);
begin
  Self.SendLn(AContext, '-;INFOMSG;{' + msg + '};');
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.IsOpenned(): Boolean;
begin
  Result := Self.tcpServer.Active;
end;

/// /////////////////////////////////////////////////////////////////////////////
// volani funkci ke klientovi

procedure TPanelServer.Note(AContext: TIdContext; blk: TBlk; note: string);
begin
  try
    (AContext.data as TPanelConnData).Note := Blk;
    Self.SendLn(AContext, '-;STIT;{' + Blk.name + '};{' + note + '};');
  except

  end;
end;

procedure TPanelServer.Note(AContext: TIdContext; blk: TBlk; note: string; rights: TAreaRights);
begin
  if (IsWritable(rights)) then
  begin
    Self.Note(AContext, Blk, note);
  end else if (note <> '') then
  begin
    var upos := TList<TUPOItem>.Create();
    try
      upos.Add(NoteUPO(blk.name, note));
      PanelServer.UPO(AContext, upos, True, nil, nil, nil);
    finally
      upos.Free();
    end;
  end;
end;

procedure TPanelServer.Lockout(AContext: TIdContext; blk: TBlk; lockout: string);
begin
  try
    (AContext.data as TPanelConnData).lockout := Blk;
    Self.SendLn(AContext, '-;VYL;{' + Blk.name + '};{' + lockout + '};');
  except

  end;
end;

procedure TPanelServer.Lockout(AContext: TIdContext; blk: TBlk; lockout: string; rights: TAreaRights);
begin
  if (IsWritable(rights)) then
  begin
    Self.Lockout(AContext, Blk, lockout);
  end else if (lockout <> '') then
  begin
    var upos := TList<TUPOItem>.Create();
    try
      upos.Add(LockoutUPO(blk.name, lockout));
      PanelServer.UPO(AContext, upos, True, nil, nil, nil);
    finally
      upos.Free();
    end;
  end;
end;

procedure TPanelServer.Menu(AContext: TIdContext; Blk: TBlk; area: TArea; Menu: string);
begin
  try
    (AContext.data as TPanelConnData).Menu := Blk;
    (AContext.data as TPanelConnData).menu_or := Area;
    Self.SendLn(AContext, '-;MENU;{' + Menu + '};');
  except

  end;
end;

procedure TPanelServer.ConfirmationSequence(AContext: TIdContext; callback: TCSCallback; Area: TArea; event: string;
  senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
begin
  Self.CSWindow(AContext, 'PS', callback, Area, event, senders, lines, free_senders, free_lines);
end;

procedure TPanelServer.InfoWindow(AContext: TIdContext; callback: TCSCallback; Area: TArea; event: string;
  senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
begin
  Self.CSWindow(AContext, 'IS', callback, Area, event, senders, lines, free_senders, free_lines);
end;

procedure TPanelServer.CSWindow(AContext: TIdContext; mode: string; callback: TCSCallback; Area: TArea; event: string;
  senders: TList<TObject>; lines: TConfSeqItems; free_senders: Boolean = true; free_lines: Boolean = true);
var str, areaName: string;
begin
  str := '';
  if (Assigned(senders)) then
  begin
    for var sender: TObject in senders do
    begin
      if (Assigned(sender)) then
      begin
        if ((sender.ClassType.InheritsFrom(TBlk))) then
          str := str + TBlk(sender).name + '|'
        else if (sender.ClassType = TArea) then
          str := str + 'Stanoviště výpravčího ' + TArea(sender).name + '|';
      end;
    end;
  end;

  str := str + ';';

  if (lines <> nil) then
    for var line: TConfSeqItem in lines do
      str := str + '[{' + line.target + '}|{' + line.note + '}]';

  if (Area <> nil) then
    areaName := Area.name
  else
    areaName := '-';

  try
    (AContext.data as TPanelConnData).potvr := callback;
    Self.SendLn(AContext, '-;' + UpperCase(mode) + ';{' + areaName + '};{' + event + '};' + str);
  except

  end;

  if ((free_senders) and (Assigned(senders))) then
    senders.Free();
  if ((free_lines) and (Assigned(lines))) then
    lines.Free();
end;

procedure TPanelServer.CSClose(AContext: TIdContext; msg: string = '');
begin
  Self.CSWindowClose(AContext, 'PS', msg);
end;

procedure TPanelServer.CSWindowClose(AContext: TIdContext; mode: string; msg: string = '');
begin
  try
    (AContext.data as TPanelConnData).potvr := nil;

    if (msg <> '') then
      Self.SendLn(AContext, '-;' + UpperCase(mode) + '-CLOSE;' + msg)
    else
      Self.SendLn(AContext, '-;' + UpperCase(mode) + '-CLOSE;');
  except

  end;
end;

procedure TPanelServer.PlaySound(AContext: TIdContext; code: Integer; loop: Boolean = false);
begin
  if ((not TPanelConnData(AContext.data).soundDict.ContainsKey(code)) or
    (TPanelConnData(AContext.data).soundDict[code] = 0)) then
  begin
    if (loop) then
      Self.SendLn(AContext, '-;SND;PLAY;' + IntToStr(code) + ';L')
    else
      Self.SendLn(AContext, '-;SND;PLAY;' + IntToStr(code) + ';');
  end;

  if (loop) then
    if (not TPanelConnData(AContext.data).soundDict.ContainsKey(code)) then
      TPanelConnData(AContext.data).soundDict.Add(code, 1)
    else
      TPanelConnData(AContext.data).soundDict[code] := TPanelConnData(AContext.data).soundDict[code] + 1;
end;

procedure TPanelServer.DeleteSound(AContext: TIdContext; code: Integer);
begin
  if (not TPanelConnData(AContext.data).soundDict.ContainsKey(code)) then
    Exit();

  if (TPanelConnData(AContext.data).soundDict[code] > 0) then
  begin
    if (TPanelConnData(AContext.data).soundDict[code] = 1) then
      Self.SendLn(AContext, '-;SND;STOP;' + IntToStr(code) + ';');
    TPanelConnData(AContext.data).soundDict[code] := TPanelConnData(AContext.data).soundDict[code] - 1;
  end;
end;

procedure TPanelServer.BottomError(AContext: TIdContext; err: string; stanice: string; tech: string);
begin
  Self.SendLn(AContext, '-;BOTTOMERR;{' + err + '};{' + stanice + '};{' + tech + '};');
  Log(tech + ': ' + stanice + ': ' + err, llWarning);
end;

procedure TPanelServer.UPO(AContext: TIdContext; items: TUPOItems; critical: Boolean; callbackOK: TNotifyEvent;
  callbackEsc: TNotifyEvent; ref: TObject);
var str: string;
begin
  TPanelConnData(AContext.data).UPO_OK := callbackOK;
  TPanelConnData(AContext.data).UPO_Esc := callbackEsc;
  TPanelConnData(AContext.data).UPO_ref := ref;

  if (items.Count = 0) then
  begin
    if (Assigned(callbackOK)) then
      callbackOK(AContext);

    TPanelConnData(AContext.data).UPO_OK := nil;
    TPanelConnData(AContext.data).UPO_Esc := nil;
    TPanelConnData(AContext.data).UPO_ref := nil;      
    Exit();
  end;

  if (critical) then
    str := '-;UPO-CRIT;{'
  else
    str := '-;UPO;{';

  for var i: Integer := 0 to items.Count - 1 do
  begin
    str := str + '[{';

    for var j: Integer := 0 to 2 do
    begin
      if (items[i][j].str = '') then
        break;

      str := str + '[{';

      case (items[i][j].align) of
        taLeftJustify:
          str := str + 'L|';
        taRightJustify:
          str := str + 'R|';
        taCenter:
          str := str + 'M|';
      end; // acse align

      if (items[i][j].fg <> clNone) then
        str := str + ownConvert.ColorToStr(items[i][j].fg) + '|';
      if (items[i][j].bg <> clNone) then
        str := str + ownConvert.ColorToStr(items[i][j].bg) + '|';
      str := str + items[i][j].str + '}]';
    end; // for j
    str := str + '}]';
  end; // for i
  str := str + '}';

  Self.SendLn(AContext, str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.POdj(AContext: TIdContext; SenderBlk: TBlk; SenderTrainId: Integer; POdj: TPOdj = nil);
var str: string;
begin
  str := '-;PODJ;';

  if ((POdj <> nil) and (POdj.abs_enabled)) then
    str := str + FormatDateTime('hh:nn:ss', POdj.abs);
  str := str + ';';

  if ((POdj <> nil) and (POdj.rel_enabled)) then
    str := str + FormatDateTime('nn:ss', POdj.rel);
  str := str + ';';

  (AContext.data as TPanelConnData).podj_track := SenderBlk;
  (AContext.data as TPanelConnData).podj_trainid := SenderTrainId;

  Self.SendLn(AContext, str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.Auth(AContext: TIdContext; parsed: TStrings);
var area: TArea;
begin
  area := areas.Get(parsed[0]);
  if (area = nil) then
  begin
    Self.SendInfoMsg(AContext, 'Tato OR neexistuje');
    Exit();
  end;

  if (parsed.Count < 4) then
    area.PanelAuthorise(AContext, TAreaRights(StrToInt(parsed[2])), '', '')
  else if (parsed.Count < 5) then
    area.PanelAuthorise(AContext, TAreaRights(StrToInt(parsed[2])), parsed[3], '')
  else
    area.PanelAuthorise(AContext, TAreaRights(StrToInt(parsed[2])), parsed[3], parsed[4]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.BroadcastBottomError(err: string; tech: string);
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    if (Assigned(Self.clients[i])) then
      Self.BottomError(Self.clients[i].connection, err, '-', tech);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.SendLn(AContext: TIdContext; str: string);
begin
  // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
  try
    AContext.connection.IOHandler.WriteLn(str);
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// gui metody
// zajistuji komunikaci s F_PanelsStatus

procedure TPanelServer.GUIInitTable();
var MI: TListItem;
begin
  F_Main.LV_Clients.Clear();
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
  begin
    MI := F_Main.LV_Clients.items.Add;
    MI.Caption := IntToStr(i);
    MI.SubItems.Add('odpojen');
    for var j: Integer := 1 to F_Main.LV_Clients.Columns.Count - 1 do
      MI.SubItems.Add('');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.GUIRefreshLine(index: Integer; repaint: Boolean = true);
begin
  if (not Assigned(F_Main.LV_Clients.items[index])) then
    Exit();

  if (not Assigned(Self.clients[index])) then
  begin
    // klient neexistuje
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'odpojen';
    for var i: Integer := 1 to F_Main.LV_Clients.Columns.Count - 1 do
      F_Main.LV_Clients.items[index].SubItems[i] := '';

    Exit();
  end;

  if (not Assigned(Self.clients[index].connection)) then
  begin
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'soket nenalezen';
    for var i: Integer := 1 to F_Main.LV_Clients.Columns.Count - 1 do
      F_Main.LV_Clients.items[index].SubItems[i] := '';
  end;

  var connData: TPanelConnData := (Self.clients[index].connection.data as TPanelConnData);

  case (Self.clients[index].state) of
    TPanelConnectionState.closed:
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'uzavřeno';
    TPanelConnectionState.opening:
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'otevírání';
    TPanelConnectionState.handshake:
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'handshake';
    TPanelConnectionState.opened:
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_STATE] := 'otevřeno';
  end;

  F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_CLIENT] :=
    Self.clients[index].connection.connection.Socket.Binding.PeerIP;
  if (connData.ping_unreachable) then
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_PING] := 'unreachable'
  else if (connData.PingComputed()) then
  begin
    var Hour, Min, Sec, MSec: Word;
    DecodeTime(connData.ping, Hour, Min, Sec, MSec);
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_PING] := IntToStr(MSec);
  end
  else
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_PING] := '?';

  for var i: Integer := 0 to 2 do
  begin
    if (i < connData.areas.Count) then
    begin
      // klient existuje
      var ORpanel: TAreaPanel;
      connData.areas[i].GetORPanel(Self.clients[index].connection, ORPanel);
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_OR1 + i] := connData.areas[i].ShortName + ' (' + ORPanel.User
        + ' :: ' + TArea.GetRightsString(ORPanel.Rights) + ')';
    end else begin
      // klient neexistuje
      F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_OR1 + i] := '';
    end;
  end;

  if (connData.areas.Count > 3) then
  begin
    var str: string := '';
    for var i: Integer := 3 to connData.areas.Count - 1 do
    begin
      var ORpanel: TAreaPanel;
      connData.areas[i].GetORPanel(Self.clients[index].connection, ORPanel);
      str := str + connData.areas[i].ShortName + ' (' + ORPanel.User + ' :: ' + TArea.GetRightsString(ORPanel.Rights) +
        ')' + ', ';
    end;
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_OR_NEXT] := LeftStr(str, Length(str) - 2);
  end;

  if (connData.regulator) then
  begin
    var str: string;
    if (Assigned(connData.regulator_user)) then
      str := connData.regulator_user.username
    else
      str := 'ano';

    if (connData.regulator_loks.Count > 0) then
    begin
      str := str + ': ';
      for var vehicle: TRV in connData.regulator_loks do
        str := str + IntToStr(vehicle.addr) + ', ';
      str := LeftStr(str, Length(str) - 2);
    end;

    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_REGULATOR] := str;
  end
  else
    F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_REGULATOR] := '';

  var announcement: string := '';
  for var area: TArea in TPanelConnData(Self.clients[index].connection.data).st_hlaseni do
    announcement := announcement + area.ShortName + ', ';
  F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_SH] := LeftStr(announcement, Length(announcement) - 2);

  F_Main.LV_Clients.items[index].SubItems[TF_Main._LV_CLIENTS_COL_DCC] := IfThen(Self.DCCStopped = Self.clients[index].connection, 'ano', '');

  F_Main.LV_Clients.UpdateItems(index, index);
end;

procedure TPanelServer.GUIRefreshTable();
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    Self.GUIRefreshLine(i, false);
  F_Main.LV_Clients.repaint();
end;

procedure TPanelServer.GUIRefreshFromQueue();
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
  begin
    if (Self.refreshQueue[i]) then
    begin
      Self.GUIRefreshLine(i);
      Self.refreshQueue[i] := false;
    end;
  end;
end;

procedure TPanelServer.GUIQueueLineToRefresh(lineindex: Integer);
begin
  Self.refreshQueue[lineindex] := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.CancelUPO(AContext: TIdContext; ref: TObject);
begin
  try
    if ((AContext.data as TPanelConnData).UPO_ref = ref) then
    begin
      Self.SendLn(AContext, '-;UPO-CLOSE');
      (AContext.data as TPanelConnData).UPO_ref := nil;
      (AContext.data as TPanelConnData).UPO_OK := nil;
      (AContext.data as TPanelConnData).UPO_Esc := nil;
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.BroadcastData(data: string);
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    if (Assigned(Self.clients[i])) then
      Self.SendLn(Self.clients[i].connection, data);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.BroadcastFuncsDescription();
var data: string;
begin
  data := '-;F-VYZN-LIST;{' + FuncNames.PanelStr() + '}';
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    if ((Assigned(Self.clients[i])) and ((Self.clients[i].connection.data as TPanelConnData).funcsVyznamReq)) then
      Self.SendLn(Self.clients[i].connection, data);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.DCCStart();
begin
  Self.BroadcastData('-;DCC;GO;');
  Self.DCCStopped := nil;
end;

procedure TPanelServer.DCCStop();
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
  begin
    if (Assigned(Self.clients[i])) then
    begin
      if ((Self.DCCStopped = Self.clients[i].connection) and (trakce.TrackStatusSafe() = tsOff)) then
        Self.SendLn(Self.clients[i].connection, '-;DCC;STOP')
      else
        Self.SendLn(Self.clients[i].connection, '-;DCC;DISABLED');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.OnDCCCmdErr(Sender: TObject; data: Pointer);
begin
  Self.BottomError(TIdContext(data), 'Centrála neodpověděla na příkaz', '-', 'CENTRÁLA');
  trakce.emergency := True;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.DisconnectClient(conn: TIdContext);
begin
  conn.connection.Disconnect();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.GetClient(index: Integer): TPanelClient;
begin
  if (index < _MAX_CLIENTS) then
    Result := Self.clients[index]
  else
    Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.DisconnectRegulatorUser(User: TUser);
begin
  for var i: Integer := _MAX_CLIENTS - 1 downto 0 do
    if ((Self.clients[i] <> nil) and (TPanelConnData(Self.clients[i].connection.data).regulator) and
      (TPanelConnData(Self.clients[i].connection.data).regulator_user = User)) then
    begin
      Self.SendLn(Self.clients[i].connection, '-;LOK;G;AUTH;not;Zrušeno oprávnění regulátor');
      TCPRegulator.RegDisconnect(Self.clients[i].connection);
    end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.CheckPing(Sender: TObject);
begin
  for var i: Integer := _MAX_CLIENTS - 1 downto 0 do
  begin
    if (Self.clients[i] <> nil) then
    begin
      try
        var connData: TPanelConnData := TPanelConnData(Self.clients[i].connection.data);
        connData.PingUpdate(Self.clients[i].connection);
      except

      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.StrToPanelButton(button: string): TPanelButton;
begin
  if (button = 'F1') then
    Result := TPanelButton.F1
  else if (button = 'F2') then
    Result := TPanelButton.F2
  else if (button = 'ENTER') then
    Result := TPanelButton.ENTER
  else if (button = 'ESCAPE') then
    Result := TPanelButton.ESCAPE
  else
    raise EInvalidButton.Create('Invalid button!');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.OnRemoveTrain(Train: TTrain);
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    if ((Self.clients[i] <> nil) and (TPanelConnData(Self.clients[i].connection.data).train_edit = Train)) then
      TPanelConnData(Self.clients[i].connection.data).ResetTrains();
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelClient.Create(conn: TIdContext; status: TPanelConnectionState = handshake);
begin
  inherited Create();

  Self.connection := conn;
  Self.state := status;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.GetBind(): string;
begin
  Result := '';
  for var i: Integer := 0 to Self.tcpServer.Bindings.Count-1 do
  begin
    var handle: TIdSocketHandle := Self.tcpServer.Bindings[i];
    Result := Result + handle.IP + ':' + handle.Port.ToString() + ', ';
  end;
  Result := LeftStr(Result, Length(Result)-2);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.GetBindings(): TIdSocketHandles;
begin
  Result := Self.tcpServer.Bindings;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelServer.IsBind(ip: string): Boolean;
begin
  Result := False;
  for var i: Integer := 0 to Self.tcpServer.Bindings.Count-1 do
    if (Self.tcpServer.Bindings[i].IP = ip) then
      Exit(True);
end;

function TPanelServer.GetBind(ip: string): TIdSocketHandle;
begin
  Result := nil;
  for var i: Integer := 0 to Self.tcpServer.Bindings.Count-1 do
    if (Self.tcpServer.Bindings[i].IP = ip) then
      Exit(Self.tcpServer.Bindings[i]);
end;

function TPanelServer.GetBindOrZeroBind(ip: string): TIdSocketHandle;
begin
  Result := Self.GetBind(ip);
  if (Result = nil) then
    Result := Self.GetBind('0.0.0.0');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.SetDCCStopped(who: TIdContext);
begin
  if (Self.m_DCCStopped = who) then
    Exit();

  if (Self.m_DCCStopped <> nil) then
  begin
    const i = TPanelConnData(Self.m_DCCStopped.data).index;
    F_Main.LV_Clients.items[i].SubItems[TF_Main._LV_CLIENTS_COL_DCC] := '';
  end;
  if (who <> nil) then
  begin
    const i = TPanelConnData(who.data).index;
    F_Main.LV_Clients.items[i].SubItems[TF_Main._LV_CLIENTS_COL_DCC] := 'ano';
  end;

  Self.m_DCCStopped := who;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServer.BlockRemoved(blk: TBlk);
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    if (Assigned(Self.clients[i])) then
      TPanelConnData(Self.clients[i].connection.Data).BlockRemoved(blk);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

PanelServer := TPanelServer.Create;

finalization

FreeAndNil(PanelServer);

end.// unit

