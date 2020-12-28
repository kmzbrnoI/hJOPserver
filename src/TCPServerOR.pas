unit TCPServerOR;

{
  TCP server pro komunikaci s panely, regulatory a obecne se vsemi klienty.

  Kompletni specifikace komunikacnho protkolu je popsana na
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer.
}

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal, SyncObjs,
     Classes, StrUtils, Graphics, Windows, TOblRizeni, ExtCtrls,
     IdContext, TBlok, ComCtrls, IdSync, TBloky, UPO, TCPORsRef,
     User, Train, Generics.Collections, THnaciVozidlo, predvidanyOdjezd;

const
  _PANEL_DEFAULT_PORT = 5896;                                                   // default port, na ktere bezi server
  _MAX_OR_CLIENTS = 64;                                                         // maximalni pocet klientu
  _PING_TIMER_PERIOD_MS = 250;
  _RECEIVE_CHECK_PERIOD_MS = 15;

  // tady jsou vyjmenovane vsechny verze protokolu, ktere akceptuje server od klientu
  _PROTO_V_ACCEPT : array[0..1] of string =
    (
      '1.0', '1.1'
    );

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);
  TTCPEvent = (evMessage, evDisconnect);

  EInvalidButton = class(Exception);

  // jeden klient:
  TORTCPClient = class
    conn: TIdContext;                                                            // fyzicke spojeni
    status: TPanelConnectionStatus;                                              // stav spojeni
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane

    constructor Create(conn: TIDContext; status: TPanelConnectionStatus = handshake);
  end;

  TTCPReceived = class
    AContext: TIdContext;
    orsRef: TTCPORsRef;
    parsed: TStrings;
    event: TTCPEvent;

    constructor Create();
    destructor Destroy(); override;
  end;

  TORTCPServer = class
   private const
    _PROTOCOL_VERSION = '1.1';

   private
    receiveTimer: TTimer;                                                        // must be executed in main thread synchronously!
    received: TObjectQueue<TTCPReceived>;                                        // locked by receivedLock!
    receivedLock: TCriticalSection;

    clients: array[0.._MAX_OR_CLIENTS-1] of TORTCPClient;                        // databaze klientu
    tcpServer: TIdTCPServer;                                                    // object serveru
    data: string;                                                                // prijata data v plain-text forme
    fport: Word;                                                                 // aktualni port serveru
    DCCStopped: TIdContext;                                                      // tady je ulozeno ID spojeni, ktere zazadalo o CentralStop
                                                                                // vsechny panely maji standartne moznost vypnout DCC
                                                                                // pokud to udela nejaky panel, ma moznost DCC zapnout jen tento panel
                                                                                // pokud vypne DCC nekdo ze serveru, nebo z ovladace, zadny klient nema moznost ho zapnout
    refreshQueue: array [0.._MAX_OR_CLIENTS-1] of Boolean;
    pingTimer: TTimer;

     procedure OnTcpServerConnect(AContext: TIdContext);                        // event pripojeni klienta z TIdTCPServer
     procedure OnTcpServerDisconnect(AContext: TIdContext);                     // event odpojeni klienta z TIdTCPServer
     procedure OnTcpServerDisconnectMainThread(AContext: TIdContext; ORsRef: TTCPORsRef);
     procedure OnTcpServerExecute(AContext: TIdContext);                        // event akce klienta z TIdTCPServer

     procedure ParseGlobal(AContext: TIdContext; parsed: TStrings);             // parsinag dat s globalnim prefixem: "-;"
     procedure ParseOR(AContext: TIdContext; parsed: TStrings);                 // parsing dat s prefixem konkretni oblasti rizeni
     procedure Auth(AContext: TIdContext; parsed: TStrings);                    // pozadavek na autorizaci OR, data se ziskavaji z \parsed

     function IsOpenned(): Boolean;                                              // je server zapnut?

     procedure OnDCCCmdErr(Sender: TObject; Data: Pointer);                       // event chyby komunikace s lokomotivou v automatu
     procedure CheckPing(Sender: TObject);
     procedure ProcessReceivedMessages();
     procedure OnReceiveTimerTick(Sender: TObject);

   public

     constructor Create();
     destructor Destroy(); override;

     procedure Start(port: Word); overload;
     procedure Start(); overload;
     procedure Stop();
     procedure DisconnectClient(conn: TIdContext);

     // volani funkci do panelu, ktere neprislusi OR, ale jednotlivym panelum
     procedure SendInfoMsg(AContext: TIdContext; msg: string);
     procedure Stitek(AContext: TIdContext; Blk: TBlk; stit: string);
     procedure Vyluka(AContext: TIdContext; Blk: TBlk; vyl: string);
     procedure Menu(AContext: TIdContext; Blk: TBlk; OblR: TOR; menu: string);
     procedure Potvr(AContext: TIdContext; callback: TPSCallback; stanice: TOR;
                     udalost: string; senders: TBlksList; podminky: TPSPodminky;
                     free_senders: Boolean = true; free_podm: Boolean = true);
     procedure PotvrOrInfo(AContext: TIdContext; mode: string; callback: TPSCallback; stanice: TOR;
                           udalost: string; senders: TBlksList; podminky: TPSPodminky;
                           free_senders: Boolean = true; free_podm: Boolean = true);
     procedure PotvrClose(AContext: TIdContext; msg: string = '');
     procedure PotvrOrInfoClose(AContext: TIdContext; mode: string; msg: string = '');
     procedure UPO(AContext: TIdContext; items: TUPOItems; critical: Boolean; callbackOK: TNotifyEvent; callbackEsc: TNotifyEvent; ref: TObject);
     procedure CancelUPO(AContext: TIdContext; ref: TObject);
     procedure POdj(AContext: TIdContext; SenderBlk: TBlk; SenderTrainId: Integer;
                    podj: TPOdj = nil);

     // Tyto funkce take muzou byt volany z oblasti rizeni, protoze nemusi byt
     // primou reakci na akci uzivatele - chceme je odeslat vsem.

     // Prehravani zvuku neprehrava a nezacina prehravat zvuky primo, pamatuje si pocitadlo prehravani jednotlivych zvuku
     // a prehravani meni jen pokud se pocitadlo mezi z 0 na 1 resp. z 1 na 0.
     //  To je k tomu, aby ruzne OR mohly volat tyto funkce bez dali kontroly nezavisle.
     //  Priority prehravani zvuku resi klient.
     procedure PlaySound(AContext: TIdContext; code: Integer; loop: Boolean = false);
     procedure DeleteSound(AContext: TIdContext; code: Integer);

     procedure BottomError(AContext: TIdContext; err: string; stanice: string; tech: string);

     procedure BroadcastBottomError(err: string; tech: string);
     procedure BroadcastData(data: string);
     procedure BroadcastFuncsVyznam();

     procedure SendLn(AContext: TIDContext; str: string);

     procedure GUIInitTable();
     procedure GUIRefreshLine(index: Integer; repaint: Boolean = true);
     procedure GUIQueueLineToRefresh(lineindex: Integer);
     procedure GUIRefreshTable();
     procedure GUIRefreshFromQueue();

     procedure DCCStart();
     procedure DCCStop();

     function GetClient(index: Integer): TORTCPClient;
     procedure DisconnectRegulatorUser(user: TUser);
     function StrToPanelButton(button: string): TPanelButton;
     procedure OnRemoveTrain(train: TTrain);

      property openned: Boolean read IsOpenned;
      property port: Word read fport write fport;
  end;//TPanelTCPClient

var
  ORTCPServer : TORTCPServer;

implementation

uses fMain, TBlokUsek, TBlokVyhybka, TBlokNav, TOblsRizeni, TBlokUvazka,
      TBlokPrejezd, Logging, ModelovyCas, TrainDb, TechnologieTrakce, FileSystem,
      TBlokZamek, Trakce, RegulatorTCP, ownStrUtils, FunkceVyznam, RCSdebugger,
      UDPDiscover, TJCDatabase, TechnologieJC, TBlokAC, ACBlocks,
      TBlokRozp, TBlokIO, ownConvert, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor TTCPReceived.Create();
begin
 inherited;
 Self.parsed := TStringList.Create();
end;

destructor TTCPReceived.Destroy();
begin
 Self.parsed.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TORTCPServer.Create();
var i: Integer;
begin
 inherited Create();

 Self.fport := _PANEL_DEFAULT_PORT;

 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.clients[i] := nil;

 Self.received := TObjectQueue<TTCPReceived>.Create();
 Self.receivedLock := TCriticalSection.Create();

 Self.pingTimer := TTimer.Create(nil);
 Self.pingTimer.Enabled := false;
 Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
 Self.pingTimer.OnTimer := Self.CheckPing;

 Self.tcpServer := TIdTCPServer.Create(nil);
 Self.tcpServer.OnConnect    := Self.OnTcpServerConnect;
 Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
 Self.tcpServer.OnExecute    := Self.OnTcpServerExecute;

 Self.receiveTimer := TTimer.Create(nil);
 Self.receiveTimer.Enabled := false;
 Self.receiveTimer.Interval := _RECEIVE_CHECK_PERIOD_MS;
 Self.receiveTimer.OnTimer := Self.OnReceiveTimerTick;

 Self.DCCStopped := nil;
end;//ctor

destructor TORTCPServer.Destroy();
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
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.Start(port: Word);
begin
 if ((SystemData.Status = starting) and (Self.openned)) then
  begin
   F_Main.LogStatus('System: start OK');
   SystemData.Status := null;
   F_Main.UpdateSystemButtons();
   Exit();
  end;

 if (Self.tcpServer.Active) then Exit();

 F_Main.S_Server.Brush.Color := clGray;
 F_Main.LogStatus('Panel server: spouštění...');

 Self.tcpServer.DefaultPort := port;
 Self.fport := port;

 try
  Self.tcpServer.Active := true;
 except
  on E: Exception do
   begin
    F_Main.S_Server.Brush.Color := clRed;
    F_Main.LogStatus('ERR: Panel server: chyba při startování serveru : '+E.Message);
    raise;
   end;
 end;

 Self.receiveTimer.Enabled := true;
 Self.pingTimer.Enabled := true;

 F_Main.S_Server.Brush.Color := clLime;
 F_Main.LogStatus('Panel server: spuštěn');

 UDPdisc.SendDiscover();

 if (SystemData.Status = starting) then
  begin
   if (GlobalConfig.ptAutoStart) then
     F_Main.A_PT_StartExecute(Self)
   else begin
     F_Main.LogStatus('System: start OK');
     SystemData.Status := null;
     F_Main.UpdateSystemButtons();
   end;
  end;
end;

procedure TORTCPServer.Start();
begin
 Self.Start(Self.port);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.Stop();
var iA: integer;
    Context: TidContext;
begin
 if ((SystemData.Status = stopping) and (not Self.openned)) then
  begin
   F_Main.A_Turnoff_FunctionsExecute(Self);
   Exit();
  end;

 if (not Self.tcpServer.Active) then Exit();

 F_Main.LogStatus('Panel server: vypínám...');
 F_Main.S_Server.Brush.Color := clGray;

 Self.pingTimer.Enabled := false;
 Self.receiveTimer.Enabled := false;

 with Self.tcpServer.Contexts.LockList do
    try
       for iA := Count - 1 downto 0 do
       begin
          Context := Items[iA];
          if Context = nil then
             Continue;
          Context.Connection.IOHandler.WriteBufferClear;
          Context.Connection.IOHandler.InputBuffer.Clear;
          Context.Connection.IOHandler.Close;
          if Context.Connection.Connected then
             Context.Connection.Disconnect;
       end;
    finally
       Self.tcpServer.Contexts.UnlockList;
    end;

 Self.tcpServer.Active := false;

 Self.ProcessReceivedMessages();
 ORTCPServer.GUIRefreshTable();
 F_Main.S_Server.Brush.Color := clRed;
 UDPdisc.SendDiscover();
 ACBlk.RemoveAllClients();

 if (SystemData.Status = stopping) then
   F_Main.A_Turnoff_FunctionsExecute(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TORTCPServer.OnTcpServerConnect(AContext: TIdContext);
var i: Integer;
begin
 Self.tcpServer.Contexts.LockList();
 try
   AContext.Connection.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;

   for i := 0 to _MAX_OR_CLIENTS-1 do
     if (Self.clients[i] = nil) then
       break;

   // na serveru neni misto -> odpojit klienta
   if (i = _MAX_OR_CLIENTS) then
    begin
     // tady bych mohl napsat chybovou hlasku
     Self.SendInfoMsg(AContext, 'Připojeno maximum klientů');
     AContext.Connection.Disconnect();
     Exit();
    end;

   AContext.Data := TTCPORsRef.Create(i);
   Self.clients[i] := TORTCPClient.Create(AContext);
   Self.GUIQueueLineToRefresh(i);
 finally
   Self.tcpServer.Contexts.UnlockList();
 end;
end;

procedure TORTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
var received: TTCPReceived;
    i: Integer;
begin
 Self.tcpServer.Contexts.LockList();
 try
   for i := 0 to _MAX_OR_CLIENTS-1 do
    begin
     if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].conn)) then
      begin
       FreeAndNil(Self.clients[i]);
       Break;
      end;
    end;
 finally
   Self.tcpServer.Contexts.UnlockList();
 end;

 receivedLock.Acquire();
 try
   received := TTCPReceived.Create();
   received.AContext := AContext;
   received.event := evDisconnect;
   received.orsRef := TTCPORsRef(AContext.Data);
   Self.received.Enqueue(received);
   AContext.data := nil;
 finally
   receivedLock.Release();
 end;
end;

procedure TORTCPServer.OnTcpServerDisconnectMainThread(AContext: TIdContext; ORsRef: TTCPORsRef);
var oblr: TOR;
    jc: TJC;
begin
 // Warning: AContext is destroyed, only address is left.
 // vymazeme klienta ze vsech oblasti rizeni
 for oblr in ORsRef.ORs do
   oblr.RemoveClient(AContext, true);
 ORsRef.ORs.Clear();

 // ukoncime probihajici potvrzovaci sekvenci
 if (Assigned(ORsRef.potvr)) then
  begin
   ORsRef.potvr(AContext, false);
   ORsRef.potvr := nil;
  end;

 // ukoncime pripadne UPO
 if (Assigned(ORsRef.UPO_Esc)) then
  begin
   ORsRef.UPO_Esc(Self);
   ORsRef.UPO_Esc := nil;
   ORsRef.UPO_OK  := nil;
   ORsRef.UPO_ref := nil;
  end;

 // zrusime pripadnou zadost o lokomotivu
 if (ORsRef.regulator_zadost <> nil) then
   ORsRef.regulator_zadost.LokoCancel(AContext);

 // odpojeni vsech pripadne neodpojenych regulatoru
 if (ORsRef.regulator) then
   TCPRegulator.RegDisconnect(AContext, true);

 // vymazeme klienta z RCS debuggeru
 RCSd.RemoveClient(AContext);

 Blky.OnClientDisconnect(AContext);
 ACBlk.OnClientDisconnect(AContext);

 // odpojil se klient, ktery zpusobil stop dcc -> dcc muze zapnout kdokoliv
 if (Self.DCCStopped = AContext) then
  begin
   Self.DCCStopped := nil;
   Self.BroadcastData('-;DCC;STOP');
  end;

 for oblr in ORsRef.st_hlaseni do
   if (Assigned(oblr.hlaseni)) then
     oblr.hlaseni.ClientDisconnect(AContext);

 for jc in JCdb do
   if (jc.stav.SenderPnl = AContext) then
     jc.ClientDisconnect(AContext);

 ORsRef.Free();

 // aktualizujeme radek v tabulce klientu ve F_Main
 if (Self.tcpServer.Active) then
   ORTCPServer.GUIRefreshTable();
end;

////////////////////////////////////////////////////////////////////////////////
// This function is executed in separate thread for each client!

procedure TORTCPServer.OnTcpServerExecute(AContext: TIdContext);
var received: TTCPReceived;
begin
 if (not AContext.Connection.Connected) then Exit;

 if (AContext.Connection.IOHandler.InputBufferIsEmpty) then
  begin
   IndySleep(1);
   Exit();
  end;

 receivedLock.Acquire();
 if (not Assigned(Self.received)) then
   Exit(); // everything is shutting down

 try
   received := TTCPReceived.Create();
   received.AContext := AContext;
   received.event := evMessage;
   received.orsRef := TTCPOrsRef(AContext.Data);

   data := AContext.Connection.IOHandler.ReadLn();
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

procedure TORTCPServer.OnReceiveTimerTick(Sender: TObject);
begin
 Self.ProcessReceivedMessages();
end;

procedure TORTCPServer.ProcessReceivedMessages();
var received: TTCPReceived;
begin
 if (not Assigned(Self.received)) then
   Exit(); // everything is shutting down

 receivedLock.Acquire();

 try
   while (Self.received.Count > 0) do
    begin
     received := Self.received.Extract();
     try
      case (received.event) of
        evMessage: begin
          if (received.parsed[0] = '-') then
            Self.ParseGlobal(received.AContext, received.parsed)
          else
            Self.ParseOR(received.AContext, received.parsed);
        end;

        evDisconnect: Self.OnTcpServerDisconnectMainThread(received.AContext, received.orsRef);
      end;
     except

     end;
    end;
 finally
   receivedLock.Release();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseGlobal(AContext: TIdContext; parsed: TStrings);
var i, j: Integer;
    tmp: string;
    blk: TBlk;
    found: Boolean;
    btn: TPanelButton;
    podj: TPOdj;
    oblr: TOR;
    orRef: TTCPORsRef;
begin
 orRef := (AContext.Data as TTCPORsRef);

 // najdeme klienta v databazi
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and (Self.clients[i].conn = AContext)) then
   break;

 //pokud pripojeni neni v databazi, je neco sakra spatne
 if (i = _MAX_OR_CLIENTS) then Exit();

 // parse handhake
 if (parsed[1] = 'HELLO') then
  begin
   // kontrola verze protokolu
   found := false;
   for j := 0 to Length(_PROTO_V_ACCEPT)-1 do
    begin
     if (parsed[2] = _PROTO_V_ACCEPT[j]) then
      begin
       found := true;
       break;
      end;
    end;//for i

   if (not found) then
    begin
     Self.BottomError(AContext, 'Nepodporovaná verze protokolu.', '-', 'PROTOKOL');
     Self.DisconnectClient(AContext);
     Exit();
    end;

   Self.clients[i].status := TPanelConnectionStatus.opened;
   Self.SendLn(AContext, '-;HELLO;' + _PROTOCOL_VERSION);

   // oznamime verzi komunikacniho protokolu
   orRef.protocol_version := parsed[2];
   F_Main.LV_Clients.Items[orRef.index].SubItems[_LV_CLIENTS_COL_PROTOCOL] := parsed[2];

   ORTCPServer.GUIQueueLineToRefresh(orRef.index);
   ModCas.SendTimeToPanel(AContext);

   if (TrakceI.TrackStatusSafe() = tsOn) then
     Self.SendLn(AContext, '-;DCC;GO')
   else if ((Self.DCCStopped <> nil) or (TrakceI.TrackStatusSafe() <> tsOff)) then
     Self.SendLn(AContext, '-;DCC;DISABLED')
   else
     Self.SendLn(AContext, '-;DCC;STOP');

   Exit();
  end;

 // vsechny nasledujici prikazy jsou podminene tim, ze probehl handshake
 if (Self.clients[i].status < TPanelConnectionStatus.opened) then Exit();

 if (parsed[1] = 'PONG') then begin
   if (parsed.Count >= 3) then
     orRef.PongReceived(StrToInt(parsed[2]));

 end else if (parsed[1] = 'STIT') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items[orRef.index].SubItems[_LV_CLIENTS_COL_STIT] := '';

   if (orRef.stitek = nil) then Exit();
   case (orRef.stitek.typ) of
    btUsek, btTU : (orRef.stitek as TBlkUsek).Stitek := tmp;
    btTurnout    : (orRef.stitek as TBlkTurnout).note := tmp;
    btUvazka     : (orRef.stitek as TBlkUvazka).Stitek := tmp;
    btPrejezd    : (orRef.stitek as TBlkPrejezd).Stitek := tmp;
    btZamek      : (orRef.stitek as TBlkZamek).Stitek := tmp;
    btRozp       : (orRef.stitek as TBlkRozp).stit := tmp;
    btIO         : (orRef.stitek as TBlkIO).stit := tmp;
   end;//case
   orRef.stitek := nil;
  end

 else if (parsed[1] = 'VYL') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items[orRef.index].SubItems[_LV_CLIENTS_COL_STIT] := '';

   if (orRef.vyluka = nil) then Exit();
   case (orRef.vyluka.typ) of
    btUsek, btTU : (orRef.vyluka as TBlkUsek).SetUsekVyl(AContext, tmp);
    btTurnout    : (orRef.vyluka as TBlkTurnout).SetLockout(AContext, tmp);
   end;//case
   orRef.vyluka := nil;
  end

 else if ((parsed[1] = 'PS') or (parsed[1] = 'IS')) then
  begin
   if (not Assigned(orRef.potvr)) then Exit();

   F_Main.LV_Clients.Items[orRef.index].SubItems[_LV_CLIENTS_COL_RIZ] := '';
   orRef.potvr(AContext, (parsed[2] = '2'));
   orRef.potvr := nil;
  end

 else if (parsed[1] = 'MENUCLICK') then
  begin
   if (orRef.menu = nil) then Exit();
   blk := orRef.menu;
   orRef.menu := nil;       // musi byt v tomto poradi - pri volani menu do bloku uz musi byt menu = nil
   F_Main.LV_Clients.Items[orRef.index].SubItems[_LV_CLIENTS_COL_MENU] := '';

   if (parsed.Count > 2) then
     blk.PanelMenuClick(AContext, orRef.menu_or, parsed[2], StrToIntDef(parsed[3], -1))
   else
     blk.PanelMenuClick(AContext, orRef.menu_or, parsed[2], -1);
  end

 else if (parsed[1] = 'CLICK') then begin
  try
   btn := Self.StrToPanelButton(parsed[2]);
   if (btn = TPanelButton.ESCAPE) then
     orRef.Escape(AContext);
  except
   on E: EInvalidButton do
     Exit();
  end;

 end else if (parsed[1] = 'OR-LIST') then
   ORs.SendORList(AContext)

 else if (parsed[1] = 'UPO') then
  begin
   if (parsed[2] = 'OK') then
    begin
      if (Assigned(orRef.UPO_OK)) then
        orRef.UPO_OK(AContext);
    end else
      if (parsed[2] = 'ESC') then
        if (Assigned(orRef.UPO_Esc)) then
          orRef.UPO_Esc(AContext);

   orRef.UPO_OK  := nil;
   orRef.UPO_Esc := nil;
   orRef.UPO_ref := nil;
  end//if parsed[2] = 'UPO'

 else if (parsed[1] = 'MOD-CAS') then
  ModCas.Parse(parsed)

 else if (parsed[1] = 'DCC') then
  begin
   if ((parsed[2] = 'GO') and (TrakceI.TrackStatusSafe() <> tsOn)) then begin
    try
      TrakceI.SetTrackStatus(tsOn, TTrakce.Callback(), TTrakce.Callback(Self.OnDCCCmdErr, AContext));
    except
      on E: Exception do
        Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
    end;
   end else if ((parsed[2] = 'STOP') and (TrakceI.TrackStatusSafe() = tsOn)) then
    begin
     Self.DCCStopped := AContext;
     try
       TrakceI.SetTrackStatus(tsOff, TTrakce.Callback(), TTrakce.Callback(Self.OnDCCCmdErr, AContext));
     except
       on E: Exception do
         Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
     end;
    end;
  end

 else if (parsed[1] = 'SPR-LIST') then
  begin
   tmp := '';
   for oblr in orRef.ORs do
     tmp := tmp + oblr.PanelGetTrains(AContext);
   Self.SendLn(AContext, '-;SPR-LIST;'+tmp);
  end

 else if (parsed[1] = 'SPR-REMOVE') then
  begin
   i := Trains.GetTrainIndexByName(parsed[2]);
   if (i >= 0) then (Trains[i].station as TOR).PanelRemoveTrain(AContext, i);

   tmp := '';
   for oblr in orRef.ORs do
    tmp := tmp + oblr.PanelGetTrains(AContext);
   Self.SendLn(AContext, '-;SPR-LIST;'+tmp);
  end

 else if (parsed[1] = 'LOK') then
   TCPRegulator.Parse(AContext, parsed)

 else if (parsed[1] = 'F-VYZN-GET') then
  begin
   orRef.funcsVyznamReq := true;
   Self.SendLn(AContext, '-;F-VYZN-LIST;{'+ FuncsFyznam.GetFuncsVyznam() +'}');
  end

 else if (parsed[1] = 'F-VYZN-ADD') then
  begin
   FuncsFyznam.ParseNewItems(parsed[2]);
   Self.BroadcastFuncsVyznam();
  end

 else if (parsed[1] = 'RCSD') then
   RCSd.Parse(AContext, parsed)

 else if (parsed[1] = 'MAUS') then
  begin
   TTCPORsRef(AContext.Data).maus := (parsed[2] = '1');
   if ((Assigned(TTCPORsRef(AContext.Data).menu)) and
       ((TTCPORsRef(AContext.Data).menu.typ = btUsek) or
        (TTCPORsRef(AContext.Data).menu.typ = btTU))) then
     TTCPORsRef(AContext.Data).menu.Change();
  end

 else if (parsed[1] = 'PODJ') then
  begin
   if (TTCPORsRef(AContext.Data).podj_usek <> nil) then
    begin
     podj := nil;
     try
       podj := TPodj.Create(parsed[2], parsed[3]);
       (TTCPORsRef(AContext.Data).podj_usek as TBlkUsek).POdjChanged(
         TTCPORsRef(AContext.Data).podj_trainid, podj
       ); // sets podj to nil if takes ownership
     except
       on E:Exception do
         Self.SendInfoMsg(AContext, 'Nepodařilo se nastavit předvídaný odjezd: '+E.Message);
     end;
     if (podj <> nil) then
       podj.Free();

     TTCPORsRef(AContext.Data).podj_usek := nil;
     TTCPORsRef(AContext.Data).podj_trainid := -1;
    end;

 end else if (parsed[1] = 'AC') then
  begin
   if (parsed.Count < 3) then Exit();
   if (parsed[2] = '-') then begin
    if ((parsed.Count >= 4) and (UpperCase(parsed[3]) = 'BLOCKS')) then
      ACBlk.ParseBlocksMessage(AContext, parsed);
   end else begin
    i := StrToInt(parsed[2]);
    Blky.GetBlkByID(i, blk);
    if ((blk <> nil) and (blk.typ = btAC)) then
      TBlkAC(blk).ClientParse(AContext, parsed)
    else
      Self.SendLn(AContext, '-;AC;'+parsed[2]+';ERR;Neplatné id AC');
   end;
 end else if (parsed[1] = 'HV') and (parsed[2] = 'ASK') then begin
   i := StrToInt(parsed[3]);
   if (HVDb[i] <> nil) then
     ORTCPServer.SendLn(AContext, '-;HV;ASK;'+IntToStr(i)+';FOUND;{'+HVDb[i].GetPanelLokString()+'}')
   else
     ORTCPServer.SendLn(AContext, '-;HV;ASK;'+IntToStr(i)+';NOT-FOUND');
 end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseOR(AContext: TIdContext; parsed: TStrings);
var oblr: TOR;
    btn: TPanelButton;
    orRef: TTCPORsRef;
begin
 orRef := (AContext.Data as TTCPORsRef);
 if (parsed.Count < 2) then Exit();

 // nejdriv se podivame, jestli nahodou nechce nekdo autorizaci
 if (parsed[1] = 'AUTH') then begin
   Self.Auth(AContext, parsed);
   Exit;
 end else if (parsed[1] = 'SH') then begin
   try
     oblr := ORs.Get(parsed[0]);
     if (Assigned(oblr)) then begin
       if (Assigned(oblr.hlaseni)) then
         oblr.hlaseni.Parse(AContext, oblr, parsed)
       else
         Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2]+'-RESPONSE;ERR;INTERNAL_ERROR');
     end else if (parsed.Count > 2) then
      Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2]+'-RESPONSE;ERR;NONEXISTING_OR');
   except
     if (parsed.Count > 2) then
       Self.SendLn(AContext, parsed[0] + ';SH;' + parsed[2]+'-RESPONSE;ERR;INTERNAL_ERROR')
   end;

   Exit();
 end;

 // vsechna ostatni data pak podlehaji znalosti OR, ktere mam autorizovane, tak z toho vyjdeme

 oblr := ORs.Get(parsed[0]);
 if (oblr = nil) then
  begin
   Self.SendInfoMsg(AContext, 'Neautorizováno');
   Exit();
  end;

 if (parsed[1] = 'GET-ALL') then
  oblr.PanelFirstGet(AContext)

 else if (parsed[1] = 'CLICK') then begin
  try
   btn := Self.StrToPanelButton(parsed[2]);

   if (parsed.Count > 4) then
     oblr.PanelClick(AContext, StrToInt(parsed[3]), btn, parsed[4])
   else
     oblr.PanelClick(AContext, StrToInt(parsed[3]), btn);

   if (btn = ESCAPE) then
     orRef.Escape(AContext);
  except
   on E: EInvalidButton do
     Exit();
  end;

 end else if (parsed[1] = 'MSG') then
   oblr.PanelMessage(ACOntext, parsed[2], parsed[3])

 else if (parsed[1] = 'SPR-CHANGE') then
  begin
   parsed.Delete(0);
   parsed.Delete(0);
   oblr.PanelTrainChange(AContext, parsed);
  end

 else if (parsed[1] = 'MENUCLICK') then
  begin
   if (parsed.Count > 3) then
     oblr.PanelDkMenuClick(AContext, parsed[2], parsed[3])
   else
     oblr.PanelDkMenuClick(AContext, parsed[2], '');
  end

 else if (parsed[1] = 'HV') then
  begin
   if (parsed[2] = 'ADD') then
     oblr.PanelHVAdd(AContext, parsed[3])
   else if (parsed[2] = 'REMOVE') then
     oblr.PanelHVRemove(AContext, StrToInt(parsed[3]))
   else if (parsed[2] = 'EDIT') then
     oblr.PanelHVEdit(AContext, parsed[3])
   else if (parsed[2] = 'MOVE') then
     oblr.PanelMoveLok(AContext, StrToInt(parsed[3]), parsed[4])
   else if (parsed[2] = 'LIST') then
     oblr.PanelHVList(AContext)
  end

 else if (parsed[1] = 'ZAS') then
  oblr.PanelZAS(AContext, parsed)

 else if (parsed[1] = 'DK-CLICK') then
  oblr.PanelDKClick(AContext, TPanelButton(StrToInt(parsed[2])))

 else if (parsed[1] = 'LOK-REQ') then
  oblr.PanelLokoReq(AContext, parsed)

 else if (parsed[1] = 'SHP') then
  oblr.PanelHlaseni(AContext, parsed);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendInfoMsg(AContext: TIdContext; msg: string);
begin
 Self.SendLn(AContext, '-;INFOMSG;'+msg+';');
end;

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.IsOpenned(): Boolean;
begin
 Result := Self.tcpServer.Active;
end;


////////////////////////////////////////////////////////////////////////////////
// volani funkci ke klientovi

procedure TORTCPServer.Stitek(AContext: TIdContext; Blk: TBlk; stit: string);
begin
 try
   (AContext.Data as TTCPORsRef).stitek := Blk;
   Self.SendLn(AContext, '-;STIT;'+Blk.name+';'+stit+';');
   F_Main.LV_Clients.Items[(AContext.Data as TTCPORsRef).index].SubItems[_LV_CLIENTS_COL_STIT] := Blk.name;
 except

 end;
end;

procedure TORTCPServer.Vyluka(AContext: TIdContext; Blk: TBlk; vyl: string);
begin
 try
   (AContext.Data as TTCPORsRef).vyluka := Blk;
   Self.SendLn(AContext, '-;VYL;'+Blk.name+';'+vyl+';');
   F_Main.LV_Clients.Items[(AContext.Data as TTCPORsRef).index].SubItems[_LV_CLIENTS_COL_STIT] := Blk.name;
 except

 end;
end;

procedure TORTCPServer.Menu(AContext: TIdContext; Blk: TBlk; OblR: TOR; menu: string);
begin
 try
   (AContext.Data as TTCPORsRef).menu    := Blk;
   (AContext.Data as TTCPORsRef).menu_or := OblR;
   Self.SendLn(AContext, '-;MENU;'+menu+';');
   F_Main.LV_Clients.Items[(AContext.Data as TTCPORsRef).index].SubItems[_LV_CLIENTS_COL_MENU] := Blk.name;
 except

 end;
end;

procedure TORTCPServer.Potvr(AContext: TIdContext;
  callback: TPSCallback; stanice: TOR; udalost: string; senders: TBlksList;
  podminky: TPSPodminky; free_senders: Boolean = true; free_podm: Boolean = true);
begin
 Self.PotvrOrInfo(AContext, 'PS', callback, stanice, udalost, senders, podminky, free_senders, free_podm);
end;

procedure TORTCPServer.PotvrOrInfo(AContext: TIdContext; mode: string;
  callback: TPSCallback; stanice: TOR; udalost: string; senders: TBlksList;
  podminky: TPSPodminky; free_senders: Boolean = true; free_podm: Boolean = true);
var str, station_name: string;
    i: Integer;
begin
 str := '';
 if (Assigned(senders)) then
   for i := 0 to senders.Count-1 do
    if (Assigned(senders[i])) then
     begin
      if ((senders[i].ClassType.InheritsFrom(TBlk))) then
        str := str + (senders[i] as TBlk).name + '|'
      else if (senders[i].ClassType = TOR) then
        str := str + 'Stanoviště výpravčího '+(senders[i] as TOR).Name + '|';
     end;

 str := str + ';';

 if (podminky <> nil) then
   for i := 0 to podminky.Count-1 do
     str := str + '[' + podminky[i].cil + '|' + podminky[i].podminka + ']';

 if (stanice <> nil) then
   station_name := stanice.Name
 else
   station_name := '-';

 try
   (AContext.Data as TTCPORsRef).potvr := callback;
   Self.SendLn(AContext, '-;'+UpperCase(mode)+';'+station_name+';'+udalost+';'+str);
   F_Main.LV_Clients.Items[(AContext.Data as TTCPORsRef).index].SubItems[_LV_CLIENTS_COL_RIZ] := udalost;
 except

 end;

 if ((free_senders) and (Assigned(senders))) then senders.Free();
 if ((free_podm) and (Assigned(podminky))) then podminky.Free();
end;

procedure TORTCPServer.PotvrClose(AContext: TIdContext; msg: string = '');
begin
 Self.PotvrOrInfoClose(AContext, 'PS', msg);
end;

procedure TORTCPServer.PotvrOrInfoClose(AContext: TIdContext; mode: string; msg: string = '');
begin
 try
   (AContext.Data as TTCPORsRef).potvr := nil;
   F_Main.LV_Clients.Items[(AContext.Data as TTCPORsRef).index].SubItems[_LV_CLIENTS_COL_RIZ] := '';

   if (msg <> '') then
    Self.SendLn(AContext, '-;'+UpperCase(mode)+'-CLOSE;'+msg)
   else
    Self.SendLn(AContext, '-;'+UpperCase(mode)+'-CLOSE;');
 except

 end;
end;

procedure TORTCPServer.PlaySound(AContext: TIdContext; code: Integer; loop: Boolean = false);
begin
 if ((not TTCPORsRef(AContext.Data).soundDict.ContainsKey(code)) or (TTCPORsRef(AContext.Data).soundDict[code] = 0)) then
  begin
   if (loop) then
     Self.SendLn(AContext, '-;SND;PLAY;'+IntToStr(code)+';L')
   else
     Self.SendLn(AContext, '-;SND;PLAY;'+IntToStr(code)+';');
  end;

 if (loop) then
   if (not TTCPORsRef(AContext.Data).soundDict.ContainsKey(code)) then
     TTCPORsRef(AContext.Data).soundDict.Add(code, 1)
   else
     TTCPORsRef(AContext.Data).soundDict[code] := TTCPORsRef(AContext.Data).soundDict[code] + 1;
end;

procedure TORTCPServer.DeleteSound(AContext: TIdContext; code: Integer);
begin
 if (not TTCPORsRef(AContext.Data).soundDict.ContainsKey(code)) then Exit();

 if (TTCPORsRef(AContext.Data).soundDict[code] > 0) then
  begin
   if (TTCPORsRef(AContext.Data).soundDict[code] = 1) then
     Self.SendLn(AContext, '-;SND;STOP;'+IntToStr(code)+';');
   TTCPORsRef(AContext.Data).soundDict[code] := TTCPORsRef(AContext.Data).soundDict[code] - 1;
  end;
end;

procedure TORTCPServer.BottomError(AContext: TIdContext; err: string; stanice: string; tech: string);
begin
 Self.SendLn(AContext, '-;BOTTOMERR;'+err+';'+stanice+';'+tech+';');
 writelog(tech + ': ' + stanice + ': ' + err, WR_ERROR);
end;

procedure TORTCPServer.UPO(AContext: TIdContext; items: TUPOItems; critical: Boolean; callbackOK: TNotifyEvent; callbackEsc: TNotifyEvent; ref: TObject);
var str: string;
    i, j: Integer;
begin
 if (critical) then
  str := '-;UPO-CRIT;{'
 else
  str := '-;UPO;{';

 for i := 0 to items.Count-1 do
  begin
   str := str + '[{';

   for j := 0 to 2 do
    begin
     if (items[i][j].str = '') then break;

     str := str + '[{';

     case (items[i][j].align) of
      taLeftJustify  : str := str + 'L|';
      taRightJustify : str := str + 'R|';
      taCenter       : str := str + 'M|';
     end;//acse align

     if (items[i][j].fg <> clNone) then
      str := str + ownConvert.ColorToStr(items[i][j].fg) + '|';
     if (items[i][j].bg <> clNone) then
      str := str + ownConvert.ColorToStr(items[i][j].bg) + '|';
     str := str + items[i][j].str + '}]';
    end;//for j
   str := str + '}]';
  end;//for i
 str := str + '}';

 try
   (AContext.Data as TTCPORsRef).UPO_OK  := callbackOK;
   (AContext.Data as TTCPORsRef).UPO_Esc := callbackEsc;
   (AContext.Data as TTCPORsRef).UPO_ref := ref;
   Self.SendLn(AContext, str);
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.POdj(AContext: TIdContext; SenderBlk: TBlk;
                            SenderTrainId: Integer; podj: TPOdj = nil);
var str: string;
begin
 str := '-;PODJ;';

 if ((podj <> nil) and (podj.abs_enabled)) then
   str := str + FormatDateTime('hh:nn:ss', podj.abs);
 str := str + ';';

 if ((podj <> nil) and (podj.rel_enabled)) then
   str := str + FormatDateTime('nn:ss', podj.rel);
 str := str + ';';

 (AContext.Data as TTCPORsRef).podj_usek := SenderBlk;
 (AContext.Data as TTCPORsRef).podj_trainid := SenderTrainId;

 Self.SendLn(AContext, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.Auth(AContext: TIdContext; parsed: TStrings);
var OblR: TOR;
begin
 OblR := ORs.Get(parsed[0]);
 if (OblR = nil) then
  begin
   Self.SendInfoMsg(AContext, 'Tato OR neexistuje');
   Exit();
  end;

 if (parsed.Count < 4) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), '', '')
 else if (parsed.Count < 5) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], '')
 else
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], parsed[4]);

end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastBottomError(err: string; tech: string);
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
   if (Assigned(Self.clients[i])) then
     Self.BottomError(Self.clients[i].conn, err, '-', tech);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendLn(AContext: TIDContext; str: string);
begin
 // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
 try
   AContext.Connection.IOHandler.WriteLn(str);
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////
// gui metody
// zajistuji komunikaci s F_PanelsStatus

procedure TORTCPServer.GUIInitTable();
var i, j: Integer;
    MI: TListItem;
begin
 F_Main.LV_Clients.Clear();
 for i := 0 to _MAX_OR_CLIENTS-1 do
  begin
   MI := F_Main.LV_Clients.Items.Add;
   MI.Caption := IntToStr(i);
   MI.SubItems.Add('odpojen');
   for j := 1 to F_Main.LV_Clients.Columns.Count-1 do
    MI.SubItems.Add('');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.GUIRefreshLine(index: Integer; repaint: Boolean = true);
var i: Integer;
    str: string;
    ORPanel: TORPanel;
    HV: THV;
    oblr: TOR;
    orRef: TTCPORsRef;
    Hour, Min, Sec, MSec: Word;
begin
 if (not Assigned(F_Main.LV_Clients.Items[index])) then
   Exit();

 if (not Assigned(Self.clients[index])) then
  begin
   // klient neexistuje
   F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'odpojen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
     F_Main.LV_Clients.Items[index].SubItems[i] := '';

   Exit();
  end;

 if (not Assigned(Self.clients[index].conn)) then
  begin
   F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'soket nenalezen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
    F_Main.LV_Clients.Items[index].SubItems[i] := '';
  end;

 orRef := (Self.clients[index].conn.Data as TTCPORsRef);

 case (Self.clients[index].status) of
  TPanelConnectionStatus.closed    : F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'uzavřeno';
  TPanelConnectionStatus.opening   : F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'otevírání';
  TPanelConnectionStatus.handshake : F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'handshake';
  TPanelConnectionStatus.opened    : F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'otevřeno';
 end;

 F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_CLIENT] := Self.clients[index].conn.Connection.Socket.Binding.PeerIP;
 if (orRef.ping_unreachable) then
   F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_PING] := 'unreachable'
 else
   if (orRef.PingComputed()) then
    begin
     DecodeTime(orRef.ping, Hour, Min, Sec, MSec);
     F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_PING] := IntToStr(MSec);
    end else
     F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_PING] := '?';

 for i := 0 to 2 do
  begin
   if (i < orRef.ORs.Count) then
    begin
     // klient existuje
     orRef.ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
     F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_OR1+i] :=
       orRef.ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')';
    end else begin
     // klient neexistuje
     F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_OR1+i] := '';
    end;
  end;//for i

 if (orRef.ORs.Count > 3) then
  begin
   str := '';
   for i := 3 to orRef.ORs.Count-1 do
    begin
     orRef.ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
     str := str + orRef.ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')' + ', ';
    end;
   F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_OR_NEXT] := LeftStr(str, Length(str)-2);
  end;

 if (orRef.menu <> nil) then
  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_MENU] := orRef.menu.name
 else begin
  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_MENU] := '';
 end;

 if (orRef.vyluka <> nil) then
  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STIT] := orRef.vyluka.name
 else begin
   if (orRef.stitek <> nil) then
    F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STIT] := orRef.stitek.name
   else
    F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_STIT] := '';
 end;

 if (not Assigned(orRef.potvr)) then
  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_RIZ] := '';

 if (orRef.regulator) then begin
  if (Assigned(orRef.regulator_user)) then
    str := orRef.regulator_user.username
  else
    str := 'ano';

  if (orRef.regulator_loks.Count > 0) then
   begin
    str := str + ': ';
    for HV in orRef.regulator_loks do
      str := str + IntToStr(HV.addr) + ', ';
    str := LeftStr(str, Length(str)-2);
   end;

  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_REGULATOR] := str;
 end else
  F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_REGULATOR] := '';

 str := '';
 for oblr in TTCPORsRef(Self.clients[index].conn.Data).st_hlaseni do
   str := str + oblr.ShortName + ', ';
 F_Main.LV_Clients.Items[index].SubItems[_LV_CLIENTS_COL_SH] := LeftStr(str, Length(str)-2);

 F_Main.LV_Clients.UpdateItems(index, index);
end;

procedure TORTCPServer.GUIRefreshTable();
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.GUIRefreshLine(i, false);
 F_Main.LV_Clients.Repaint();
end;

procedure TORTCPServer.GUIRefreshFromQueue();
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  begin
   if (Self.refreshQueue[i]) then
    begin
     Self.GUIRefreshLine(i);
     Self.refreshQueue[i] := false;
    end;
  end;
end;

procedure TORTCPServer.GUIQueueLineToRefresh(lineindex: Integer);
begin
 Self.refreshQueue[lineindex] := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.CancelUPO(AContext: TIdContext; ref: TObject);
begin
 try
   if ((AContext.Data as TTCPORsRef).UPO_ref = ref) then
    begin
     Self.SendLn(AContext, '-;UPO-CLOSE');
     (AContext.Data as TTCPORsRef).UPO_ref := nil;
     (AContext.Data as TTCPORsRef).UPO_OK  := nil;
     (AContext.Data as TTCPORsRef).UPO_Esc := nil;
    end;
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastData(data: string);
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.SendLn(Self.clients[i].conn, data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastFuncsVyznam();
var i: Integer;
    data: string;
begin
 data := '-;F-VYZN-LIST;{'+FuncsFyznam.GetFuncsVyznam()+'}';
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and ((Self.clients[i].conn.Data as TTCPORsRef).funcsVyznamReq)) then
    Self.SendLn(Self.clients[i].conn, data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DCCStart();
begin
 Self.BroadcastData('-;DCC;GO;');
 Self.DCCStopped := nil;
end;

procedure TORTCPServer.DCCStop();
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
   begin
    if ((Self.DCCStopped = Self.clients[i].conn) and (TrakceI.TrackStatusSafe() = tsOff)) then
      Self.SendLn(Self.clients[i].conn, '-;DCC;STOP')
    else
      Self.SendLn(Self.clients[i].conn, '-;DCC;DISABLED');
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnDCCCmdErr(Sender: TObject; Data: Pointer);
begin
 Self.BottomError(TIdContext(Data), 'Centrála neodpověděla na příkaz', '-', 'CENTRÁLA');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DisconnectClient(conn: TIdContext);
begin
 conn.Connection.Disconnect();
end;

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.GetClient(index: Integer): TORTCPClient;
begin
 if (index < _MAX_OR_CLIENTS) then
   Result := Self.clients[index]
 else
   Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DisconnectRegulatorUser(user: TUser);
var i: Integer;
begin
 for i := _MAX_OR_CLIENTS-1 downto 0 do
   if ((Self.clients[i] <> nil) and (TTCPORsRef(Self.clients[i].conn.Data).regulator) and
       (TTCPORsRef(Self.clients[i].conn.Data).regulator_user = user)) then
    begin
     Self.SendLn(Self.clients[i].conn, '-;LOK;G;AUTH;not;Zrušeno oprávnění regulátor');
     TCPRegulator.RegDisconnect(Self.clients[i].conn);
    end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.CheckPing(Sender: TObject);
var i: Integer;
    orRef: TTCPORsRef;
begin
 for i := _MAX_OR_CLIENTS-1 downto 0 do
  begin
   if (Self.clients[i] <> nil) then
    begin
     try
      orRef := TTCPORsRef(Self.clients[i].conn.Data);
      orRef.PingUpdate(Self.clients[i].conn);
     except

     end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.StrToPanelButton(button: string): TPanelButton;
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

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnRemoveTrain(train: TTrain);
var i: Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
   if ((Self.clients[i] <> nil) and (TTCPORsRef(Self.clients[i].conn.Data).train_edit = train)) then
     TTCPORsRef(Self.clients[i].conn.Data).ResetTrains();
end;

////////////////////////////////////////////////////////////////////////////////

constructor TORTCPClient.Create(conn: TIDContext; status: TPanelConnectionStatus = handshake);
begin
 inherited Create();

 Self.conn := conn;
 Self.status := status;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 ORTCPServer := TORTCPServer.Create;

finalization
 FreeAndNil(ORTCPServer);

end.//unit
