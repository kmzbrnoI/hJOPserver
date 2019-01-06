unit TCPServerOR;

{
  TCP server pro komunikaci s panely, regulatory a obecne se vsemi klienty.

  Kompletni specifikace komunikacnho protkolu je popsana na
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer.
}

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal, SyncObjs,
     Classes, StrUtils, Graphics, Windows, TOblRizeni, ExtCtrls,
     IdContext, TBlok, Prevody, ComCtrls, IdSync, TBloky, UPO,
     User, Souprava, Generics.Collections, THnaciVozidlo, predvidanyOdjezd;

const
  _PANEL_DEFAULT_PORT = 5896;                                                   // default port, na ktere bezi server
  _MAX_OR_CLIENTS = 32;                                                         // maximalni pocet klientu
  _PING_TIMER_PERIOD_MS = 20000;

  // tady jsou vyjmenovane vsechny verze protokolu, ktere akceptuje server od klientu
  _PROTO_V_ACCEPT : array[0..1] of string =
    (
      '1.0', '1.1'
    );


type
  TPSCallback = procedure (Sender:TIdContext; success:boolean) of object;

  TPanelConnectionStatus = (closed, opening, handshake, opened);

  EInvalidButton = class(Exception);

  // tady je ulozeno jedno fyzicke spojeni s panelem (obsahuje oblasti rizeni, otevrene okynko stitku, menu, ...)
  TTCPORsRef = class
    ORs:TList<TOR>;                                                             // reference na OR
    protocol_version:string;

    stitek:TBlk;                                                                // blok, na kterema kutalne probiha zmena stitku
    vyluka:TBlk;                                                                // blok, na kterem aktualne probiha zmena vyluky
    potvr:TPSCallback;                                                          // callback probihajici potvrzovaci sekvence
    menu:TBlk;                                                                  // blok, kteremu odeslat callback kliku na polozku v menu
    menu_or:TOR;                                                                // OR, ze ktere bylo vyvolano menu
    UPO_OK, UPO_Esc:TNotifyEvent;                                               // callbacky manipulace s upozornenim vlevo dole
    UPO_ref:TObject;                                                            //
    index:Integer;                                                              // index spojeni v tabulce ve F_Main
    funcsVyznamReq:boolean;                                                     // jestli mame panelu odesilat zmeny vyznamu funkci; zmeny se odesilaji jen, pokud panel alespon jednou zazadal o seznam vyznamu funkci
    maus:boolean;                                                               // jestli je k panelu pripojeny uLI-daemon pripraveny prijimat adresy

    spr_new_usek_index:Integer;                                                 // index nove vytvarene soupravy na useku (-1 pokud neni vytvarena)
    spr_edit:TSouprava;                                                         // souprava, kterou panel edituje
    spr_usek:TObject;                                                           // usek, na kterem panel edituje soupravu (TBlkUsek)

    regulator:boolean;                                                          // true pokud klient autorizoval rizeni pres regulator
    regulator_user:TUser;                                                       // uzivatel, ktery autorizoval regulator
    regulator_zadost:TOR;                                                       // oblast rizeni, do ktere probiha zadost o hnaci vozidlo
    regulator_loks:TList<THV>;                                                  // seznam lokomotiv v regulatoru

    st_hlaseni:TList<TOR>;                                                      // stanice, do kterych je autorizovano stanicni hlaseni
    spr_menu_index:Integer;                                                     // index sopuravy, ktere se aktualne zorbazuje menu (viz blok usek)

    soundDict:TDictionary<Integer, Cardinal>;                                   // pro kazdy zvuk obsahuje pocet jeho prehravani
                                                                                // predpoklada se, ze kazda OR si resi zvuku samostatne, az tady se to spojuje

    podj_usek: TBlk;                                                            // data pro editaci predvidaneho odjezdu
    podj_sprid: Integer;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    procedure Escape(AContext: TIdContext);                                     // volano pri stisku Escape v panelu
    procedure Reset();

  end;

  // jeden klient:
  TORTCPClient = class
    conn:TIdContext;                                                            // fyzicke spojeni
    status:TPanelConnectionStatus;                                              // stav spojeni
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane

    constructor Create(conn:TIDContext; status:TPanelConnectionStatus = handshake);
  end;

  TORTCPServer = class
   private const
    _PROTOCOL_VERSION = '1.0';

   private
    clients:array[0.._MAX_OR_CLIENTS-1] of TORTCPClient;                        // databaze klientu
    tcpServer: TIdTCPServer;                                                    // object serveru
    parsed: TStrings;                                                           // naparsovana data, implementovano jako globalni promenna pro zrychleni
    data:string;                                                                // prijata data v plain-text forme
    fport:Word;                                                                 // aktualni port serveru
    DCCStopped:TIdContext;                                                      // tady je ulozeno ID spojeni, ktere zazadalo o CentralStop
                                                                                // vsechny panely maji standartne moznost vypnout DCC
                                                                                // pokud to udela nejaky panel, ma moznost DCC zapnout jen tento panel
                                                                                // pokud vypne DCC nekdo ze serveru, nebo z ovladace, zadny klient nema moznost ho zapnout
    refreshQueue:array [0.._MAX_OR_CLIENTS-1] of Boolean;
    readLock:TCriticalSection;
    pingTimer:TTimer;

     procedure OnTcpServerConnect(AContext: TIdContext);                        // event pripojeni klienta z TIdTCPServer
     procedure OnTcpServerDisconnect(AContext: TIdContext);                     // event odpojeni klienta z TIdTCPServer
     procedure OnTcpServerExecute(AContext: TIdContext);                        // event akce klienta z TIdTCPServer

     procedure ParseGlobal(AContext: TIdContext);                               // parsinag dat s globalnim prefixem: "-;"
     procedure ParseOR(AContext: TIdContext);                                   // parsing dat s prefixem konkretni oblasti rizeni
     procedure Auth(AContext: TIdContext);                                      // pozadavek na autorizaci OR, data se ziskavaji z \parsed

     function IsOpenned():boolean;                                              // je server zapnut?

     procedure OnDCCCmdErr(Sender:TObject; Data:Pointer);                       // event chyby komunikace s lokomotivou v automatu
     procedure BroadcastPing(Sedner:TObject);

   public

     constructor Create();
     destructor Destroy(); override;

     function Start(port:Word):Integer; overload;                               // spustit server
     function Start():Integer; overload;                                        // spustit server
     function Stop():Integer;                                                   // zastavit server
     procedure DisconnectClient(conn:TIdContext);                               // odpojit konkretniho klienta

     // volani funkci do panelu, ktere neprislusi OR, ale jednotlivym panelum
     procedure SendInfoMsg(AContext:TIdContext; msg:string);
     procedure Stitek(AContext: TIdContext; Blk:TBlk; stit:string);
     procedure Vyluka(AContext: TIdContext; Blk:TBlk; vyl:string);
     procedure Menu(AContext: TIdContext; Blk:TBlk; OblR:TOR; menu:string);
     procedure Potvr(AContext: TIdContext; callback:TPSCallback; stanice:TOR; udalost:string; senders:TBlksList; podminky:TPSPodminky; free_senders:boolean = true; free_podm:boolean = true);
     procedure PotvrClose(AContext: TIdContext; msg:string = '');
     procedure UPO(AContext: TIdContext; items:TUPOItems; critical:boolean; callbackOK:TNotifyEvent; callbackEsc:TNotifyEvent; ref:TObject);
     procedure CancelUPO(AContext: TIdContext; ref:TObject);
     procedure POdj(AContext: TIdContext; SenderBlk:TBlk; SenderSprId:Integer;
                    podj:TPOdj = nil);

     // Tyto funkce take muzou byt volany z oblasti rizeni, protoze nemusi byt
     // primou reakci na akci uzivatele - chceme je odeslat vsem.

     // Prehravani zvuku neprehrava a nezacina prehravat zvuky primo, pamatuje si pocitadlo prehravani jednotlivych zvuku
     // a prehravani meni jen pokud se pocitadlo mezi z 0 na 1 resp. z 1 na 0.
     //  To je k tomu, aby ruzne OR mohly volat tyto funkce bez dali kontroly nezavisle.
     //  Priority prehravani zvuku resi klient.
     procedure PlaySound(AContext: TIdContext; code:Integer; loop:boolean = false);
     procedure DeleteSound(AContext: TIdContext; code:Integer);

     procedure BottomError(AContext: TIdContext; err:string; stanice:string; tech:string);

     procedure BroadcastBottomError(err:string; tech:string);
     procedure BroadcastData(data:string);
     procedure BroadcastFuncsVyznam();

     procedure SendLn(AContext:TIDContext; str:string);

     procedure GUIInitTable();
     procedure GUIRefreshLine(index:Integer; repaint:boolean = true);
     procedure GUIQueueLineToRefresh(lineindex:Integer);
     procedure GUIRefreshTable();
     procedure GUIRefreshFromQueue();

     procedure DCCStart();
     procedure DCCStop();

     function GetClient(index:Integer):TORTCPClient;

     procedure DisconnectRegulatorUser(user:TUser);

     function StrToPanelButton(button:string):TPanelButton;

      property openned:boolean read IsOpenned;
      property port:Word read fport write fport;
  end;//TPanelTCPClient

var
  ORTCPServer : TORTCPServer;

implementation

uses fMain, TBlokUsek, TBlokVyhybka, TBlokSCom, TOblsRizeni, TBlokUvazka,
      TBlokPrejezd, Logging, ModelovyCas, SprDb,
      TBlokZamek, Trakce, RegulatorTCP, ownStrUtils, FunkceVyznam, RCSdebugger,
      UDPDiscover, DateUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TORTCPServer.Create();
var i:Integer;
begin
 inherited Create();

 Self.fport := _PANEL_DEFAULT_PORT;

 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.clients[i] := nil;

 Self.parsed := TStringList.Create;
 Self.readLock := TCriticalSection.Create();

 Self.pingTimer := TTimer.Create(nil);
 Self.pingTimer.Enabled := false;
 Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
 Self.pingTimer.OnTimer := Self.BroadcastPing;

 Self.tcpServer := TIdTCPServer.Create(nil);
 Self.tcpServer.OnConnect    := Self.OnTcpServerConnect;
 Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
 Self.tcpServer.OnExecute    := Self.OnTcpServerExecute;

 Self.DCCStopped := nil;
end;//ctor

destructor TORTCPServer.Destroy();
begin
 try
   if (Self.tcpServer.Active) then
    Self.tcpServer.Active := false;

   if (Assigned(Self.tcpServer)) then
     FreeAndNil(Self.tcpServer);

   if (Assigned(Self.parsed)) then
     FreeAndNil(Self.parsed);

   Self.pingTimer.Free();
   Self.readLock.Free();
 finally
   inherited;
 end;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.Start(port:Word):Integer;
begin
 if ((SystemData.Status = starting) and (Self.openned)) then
  begin
   F_Main.LogStatus('System: start OK');
   SystemData.Status := null;
   F_Main.UpdateSystemButtons();
   Exit(0);
  end;

 if (Self.tcpServer.Active) then Exit(1);

 F_Main.S_Server.Brush.Color := clGray;
 F_Main.LogStatus('Panel server: spouštìní...');

 Self.tcpServer.DefaultPort := port;
 Self.fport := port;

 try
  Self.tcpServer.Active := true;
 except
  on E:Exception do
   begin
    F_Main.S_Server.Brush.Color := clRed;
    F_Main.LogStatus('ERR: Panel server: chyba pøi startování serveru : '+E.Message);
    raise;
   end;
 end;

 Self.pingTimer.Enabled := true;

 F_Main.S_Server.Brush.Color := clLime;
 F_Main.LogStatus('Panel server: spuštìn');

 UDPdisc.SendDiscover();

 if (SystemData.Status = starting) then
  begin
   F_Main.LogStatus('System: start OK');
   SystemData.Status := null;
   F_Main.UpdateSystemButtons();
  end;

 Result := 0;
end;//function

function TORTCPServer.Start():Integer;
begin
 Result := Self.Start(Self.port);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.Stop():Integer;
var iA:integer;
    Context: TidContext;
begin
 if ((SystemData.Status = stopping) and (not Self.openned)) then
  begin
   TrkSystem.TurnOffFunctions(F_Main.OnSoundDisabled);
   Exit(0);
  end;

 if (not Self.tcpServer.Active) then Exit(1);

 F_Main.LogStatus('Panel server: vypínám...');
 F_Main.S_Server.Brush.Color := clGray;

 Self.pingTimer.Enabled := false;

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

 ORTCPServer.GUIRefreshTable();

 F_Main.S_Server.Brush.Color := clRed;

 UDPdisc.SendDiscover();

 if (SystemData.Status = stopping) then
   TrkSystem.TurnOffFunctions(F_Main.OnSoundDisabled);

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TORTCPServer.OnTcpServerConnect(AContext: TIdContext);
var i:Integer;
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
     Self.SendInfoMsg(AContext, 'Pøipojeno maximum klientù');
     AContext.Connection.Disconnect();
     Exit();
    end;

   AContext.Data := TTCPORsRef.Create(i);
   Self.clients[i] := TORTCPClient.Create(AContext);
   Self.GUIQueueLineToRefresh(i);
 finally
   Self.tcpServer.Contexts.UnlockList();
 end;
end;//procedure

// Udalost vyvolana pri odpojeni klienta
procedure TORTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
var i:Integer;
    oblr:TOR;
begin
 Self.tcpServer.Contexts.LockList();

 try
   // vymazeme klienta ze vsech oblasti rizeni
   for oblr in (AContext.Data as TTCPORsRef).ORs do
     oblr.RemoveClient(AContext);

   // ukoncime probihajici potvrzovaci sekvenci
   if (Assigned(TTCPORsRef(AContext.Data).potvr)) then
    begin
     TTCPORsRef(AContext.Data).potvr(AContext, false);
     TTCPORsRef(AContext.Data).potvr := nil;
    end;

   // ukoncime pripadne UPO
   if (Assigned(TTCPORsRef(AContext.Data).UPO_Esc)) then
    begin
     TTCPORsRef(AContext.Data).UPO_Esc(Self);
     TTCPORsRef(AContext.Data).UPO_Esc := nil;
     TTCPORsRef(AContext.Data).UPO_OK  := nil;
     TTCPORsRef(AContext.Data).UPO_ref := nil;
    end;

   // vymazeme klienta z databaze klientu
   for i := 0 to _MAX_OR_CLIENTS-1 do
    if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].conn)) then
     begin
      // zrusime pripadnou zadost o lokomotivu
      if ((Self.clients[i].conn.Data as TTCPORsRef).regulator_zadost <> nil) then
        (Self.clients[i].conn.Data as TTCPORsRef).regulator_zadost.LokoCancel(Self.clients[i].conn);

      // odpojeni vsech pripadne neodpojenych regulatoru
      if ((Self.clients[i].conn.Data as TTCPORsRef).regulator) then
        TCPRegulator.RegDisconnect(Self.clients[i].conn);

      FreeAndNil(Self.clients[i]);
      break;
     end;

    // vymazeme klienta z RCS debuggeru
    RCSd.RemoveClient(AContext);

    // odpoji se klient, ktery zpusobil stop dcc -> dcc muze zapnout kdokoliv
    if (Self.DCCStopped = AContext) then
     begin
      Self.DCCStopped := nil;
      Self.BroadcastData('-;DCC;STOP');
     end;

    for oblr in TTCPORsRef(AContext.Data).st_hlaseni do
      if (Assigned(oblr.hlaseni)) then
        oblr.hlaseni.ClientDisconnect(AContext);

    // aktualizujeme radek v tabulce klientu ve F_Main
    if (Self.tcpServer.Active) then
      ORTCPServer.GUIQueueLineToRefresh(i);
 finally
   Self.tcpServer.Contexts.UnlockList();
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnTcpServerExecute(AContext: TIdContext);
begin
 if (not AContext.Connection.Connected) then Exit;

 if (AContext.Connection.IOHandler.InputBufferIsEmpty) then
  begin
   IndySleep(1);
   Exit();
  end;

 readLock.Acquire();

 try
   //read data
   // data jsou schvalne globalni, aby se porad nevytvarela a nenicila dokola
   data := AContext.Connection.IOHandler.ReadLn();

   Self.parsed.Clear();
   ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

   if (Self.parsed.Count = 0) then Exit();
   if (Self.parsed.Count > 1) then Self.parsed[1] := UpperCase(Self.parsed[1]);

   try
     // zakladni rozdeleni parsovani - na data, ktera jsou obecna a na data pro konkretni oblast rizeni
     if (Self.parsed[0] = '-') then
      Self.ParseGlobal(AContext)
     else
      Self.ParseOR(AContext);
   except

   end;
 finally
   readLock.Release();
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseGlobal(AContext: TIdContext);
var i, j:Integer;
    tmp:string;
    blk:TBlk;
    found:boolean;
    btn:TPanelButton;
    podj: TPOdj;
    dt: TDateTime;
    oblr:TOR;
begin
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
     if (Self.parsed[2] = _PROTO_V_ACCEPT[j]) then
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
   (AContext.Data as TTCPORsRef).protocol_version := Self.parsed[2];
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[9] := Self.parsed[2];

   ORTCPServer.GUIQueueLineToRefresh((AContext.Data as TTCPORsRef).index);
   ModCas.SendTimeToPanel(AContext);

   if (TrkSystem.status = Ttrk_status.TS_ON) then
     Self.SendLn(AContext, '-;DCC;GO')
   else if ((Self.DCCStopped <> nil) or (TrkSystem.status <> Ttrk_status.TS_OFF)) then
     Self.SendLn(AContext, '-;DCC;DISABLED')
   else
     Self.SendLn(AContext, '-;DCC;STOP');

   Exit();
  end;

 // vsechny nasledujici prikazy jsou podminene tim, ze probehl handshake
 if (Self.clients[i].status < TPanelConnectionStatus.opened) then Exit();

 if (parsed[1] = 'STIT') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := '';

   if ((AContext.Data as TTCPORsRef).stitek = nil) then Exit();
   case ((AContext.Data as TTCPORsRef).stitek.typ) of
    _BLK_USEK, _BLK_TU : ((AContext.Data as TTCPORsRef).stitek as TBlkUsek).Stitek := tmp;
    _BLK_VYH           : ((AContext.Data as TTCPORsRef).stitek as TBlkVyhybka).Stitek := tmp;
    _BLK_UVAZKA        : ((AContext.Data as TTCPORsRef).stitek as TBlkUvazka).Stitek := tmp;
    _BLK_PREJEZD       : ((AContext.Data as TTCPORsRef).stitek as TBlkPrejezd).Stitek := tmp;
    _BLK_ZAMEK         : ((AContext.Data as TTCPORsRef).stitek as TBlkZamek).Stitek := tmp;
   end;//case
   (AContext.Data as TTCPORsRef).stitek := nil;
  end

 else if (parsed[1] = 'VYL') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := '';

   if ((AContext.Data as TTCPORsRef).vyluka = nil) then Exit();
   case ((AContext.Data as TTCPORsRef).vyluka.typ) of
    _BLK_USEK, _BLK_TU : ((AContext.Data as TTCPORsRef).vyluka as TBlkUsek).SetUsekVyl(AContext, tmp);
    _BLK_VYH           : ((AContext.Data as TTCPORsRef).vyluka as TBlkVyhybka).SetVyhVyl(AContext, tmp);
   end;//case
   (AContext.Data as TTCPORsRef).vyluka := nil;
  end

 else if (parsed[1] = 'PS') then
  begin
   if (not Assigned((AContext.Data as TTCPORsRef).potvr)) then Exit();

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := '';

   if (parsed[2] = '2') then
     (AContext.Data as TTCPORsRef).potvr(AContext, true)
   else
     (AContext.Data as TTCPORsRef).potvr(AContext, false);

   (AContext.Data as TTCPORsRef).potvr := nil;
  end

 else if (parsed[1] = 'MENUCLICK') then
  begin
   if ((AContext.Data as TTCPORsRef).menu = nil) then Exit();
   blk := (AContext.Data as TTCPORsRef).menu;
   (AContext.Data as TTCPORsRef).menu := nil;       // musi byt v tomto poradi - pri volani menu do bloku uz musi byt menu = nil
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[6] := '';

   if (parsed.Count > 2) then
     blk.PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2], StrToIntDef(parsed[3], -1))
   else
     blk.PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2], -1);
  end

 else if (parsed[1] = 'CLICK') then begin
  try
   btn := Self.StrToPanelButton(parsed[2]);
   if (btn = TPanelButton.ESCAPE) then
     (AContext.Data as TTCPORsRef).Escape(AContext);
  except
   on E:EInvalidButton do
     Exit();
  end;

 end else if (parsed[1] = 'OR-LIST') then
   ORs.SendORList(AContext)

 else if (parsed[1] = 'UPO') then
  begin
   if (parsed[2] = 'OK') then
    begin
      if (Assigned((AContext.Data as TTCPORsRef).UPO_OK)) then
        (AContext.Data as TTCPORsRef).UPO_OK(AContext);
    end else
      if (parsed[2] = 'ESC') then
        if (Assigned((AContext.Data as TTCPORsRef).UPO_Esc)) then
          (AContext.Data as TTCPORsRef).UPO_Esc(AContext);

   (AContext.Data as TTCPORsRef).UPO_OK  := nil;
   (AContext.Data as TTCPORsRef).UPO_Esc := nil;
   (AContext.Data as TTCPORsRef).UPO_ref := nil;
  end//if parsed[2] = 'UPO'

 else if (parsed[1] = 'MOD-CAS') then
  ModCas.Parse(parsed)

 else if (parsed[1] = 'DCC') then
  begin
   if ((parsed[2] = 'GO') and (TrkSystem.status <> TS_ON)) then begin
    TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCCmdErr, AContext);
    try
      TrkSystem.CentralStart();
    except
      on E:Exception do
        Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
    end;
   end else if ((parsed[2] = 'STOP') and (TrkSystem.status = TS_ON)) then
    begin
     Self.DCCStopped := AContext;
     TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCCmdErr, AContext);
     try
       TrkSystem.CentralStop();
     except
       on E:Exception do
         Self.BottomError(AContext, E.Message, '-', 'CENTRÁLA');
     end;
    end;
  end

 else if (parsed[1] = 'SPR-LIST') then
  begin
   tmp := '';
   for oblr in (AContext.Data as TTCPORsRef).ORs do
     tmp := tmp + oblr.PanelGetSprs(AContext);
   Self.SendLn(AContext, '-;SPR-LIST;'+tmp);
  end

 else if (parsed[1] = 'SPR-REMOVE') then
  begin
   i := Soupravy.GetSprIndexByName(parsed[2]);
   if (i >= 0) then (Soupravy.soupravy[i].stanice as TOR).PanelRemoveSpr(AContext, i);

   tmp := '';
   for oblr in (AContext.Data as TTCPORsRef).ORs do
    tmp := tmp + oblr.PanelGetSprs(AContext);
   Self.SendLn(AContext, '-;SPR-LIST;'+tmp);
  end

 else if (parsed[1] = 'LOK') then
   TCPRegulator.Parse(AContext, parsed)

 else if (parsed[1] = 'F-VYZN-GET') then
  begin
   (AContext.Data as TTCPORsRef).funcsVyznamReq := true;
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
       ((TTCPORsRef(AContext.Data).menu.typ = _BLK_USEK) or
        (TTCPORsRef(AContext.Data).menu.typ = _BLK_TU))) then
     TTCPORsRef(AContext.Data).menu.Change();
  end

 else if (parsed[1] = 'PODJ') then
  begin
   if (TTCPORsRef(AContext.Data).podj_usek <> nil) then
    begin
     try
       podj := TPodj.Create();
       try
         if (parsed[2] <> '') then
          begin
           if (ModCas.used) then
            begin
             podj.abs := ModCas.date + StrToTime(parsed[2]);
             if (podj.abs < ModCas.time) then
               podj.abs := IncDay(podj.abs);
            end else begin
             podj.abs := Date() + StrToTime(parsed[2]);
             dt := Now;
             ReplaceDate(dt, 0);
             if (podj.abs < dt) then
               podj.abs := IncDay(podj.abs);
            end;
          end;

         if (parsed[3] <> '') then
           podj.rel := StrToTime('00:'+parsed[3]);

         (TTCPORsRef(AContext.Data).podj_usek as TBlkUsek).PanelPOdj(
           AContext, TTCPORsRef(AContext.Data).podj_sprid, podj
         );
       finally
         if (podj <> nil) then
           podj.Free();
       end;
     except
       Self.SendInfoMsg(AContext, 'Nepodaøilo se nastavit pøedvídaný odjezd!');
     end;

     TTCPORsRef(AContext.Data).podj_usek := nil;
     TTCPORsRef(AContext.Data).podj_sprid := -1;
    end;
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseOR(AContext: TIdContext);
var i:Integer;
    oblr:TOR;
    btn:TPanelButton;
begin
 if (parsed.Count < 2) then Exit();

 // nejdriv se podivame, jestli nahodou nechce nekdo autorizaci
 if (parsed[1] = 'AUTH') then begin
   Self.Auth(AContext);
   Exit;
 end else if (parsed[1] = 'SH') then begin
   try
     oblr := ORs.GetORById(parsed[0]);
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
 for i := 0 to (AContext.Data as TTCPORsRef).ORs.Count-1 do
   if (parsed[0] = (AContext.Data as TTCPORsRef).ORs[i].id) then
     break;

 if (i = (AContext.Data as TTCPORsRef).ORs.Count) then
  begin
   Self.SendInfoMsg(AContext, 'Neautorizováno');
   Exit();
  end;

 if (parsed[1] = 'NUZ') then
  begin
   if (parsed[2] = '1') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelNUZ(AContext);
   if (parsed[2] = '0') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelNUZCancel(AContext);
   Exit();
  end

 else if (parsed[1] = 'GET-ALL') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelFirstGet(AContext)

 else if (parsed[1] = 'CLICK') then begin
  try
   btn := Self.StrToPanelButton(parsed[2]);

   if (parsed.Count > 4) then
     (AContext.Data as TTCPORsRef).ORs[i].PanelClick(AContext, StrToInt(parsed[3]), btn, parsed[4])
   else
     (AContext.Data as TTCPORsRef).ORs[i].PanelClick(AContext, StrToInt(parsed[3]), btn);

   if (btn = ESCAPE) then
     (AContext.Data as TTCPORsRef).Escape(AContext);
  except
   on E:EInvalidButton do
     Exit();
  end;

 end else if (parsed[1] = 'MSG') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelMessage(ACOntext, parsed[2], parsed[3])

 else if (parsed[1] = 'HV-LIST') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelHVList(AContext)

 else if (parsed[1] = 'SPR-CHANGE') then
  begin
   parsed.Delete(0);
   parsed.Delete(0);
   (AContext.Data as TTCPORsRef).ORs[i].PanelSprChange(AContext, parsed);
  end

 else if (parsed[1] = 'LOK-MOVE-OR') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelMoveLok(AContext, StrToInt(parsed[2]), parsed[3])

 else if (parsed[1] = 'OSV') then
  begin
   if (parsed[2] = 'GET') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelSendOsv(AContext)
   else if (parsed[2] = 'SET') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelSetOsv(AContext, parsed[3], PrevodySoustav.StrToBool(parsed[4]));
  end

 else if (parsed[1] = 'HV') then
  begin
   if (parsed[2] = 'ADD') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVAdd(AContext, parsed[3]);
   if (parsed[2] = 'REMOVE') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVRemove(AContext, StrToInt(parsed[3]));
   if (parsed[2] = 'EDIT') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVEdit(AContext, parsed[3]);
  end

 else if (parsed[1] = 'ZAS') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelZAS(AContext, parsed)

 else if (parsed[1] = 'DK-CLICK') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelDKClick(AContext, TPanelButton(StrToInt(parsed[2])))

 else if (parsed[1] = 'LOK-REQ') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelLokoReq(AContext, parsed)

 else if (parsed[1] = 'SHP') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelHlaseni(AContext, parsed);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendInfoMsg(AContext:TIdContext; msg:string);
begin
 Self.SendLn(AContext, '-;INFOMSG;'+msg+';');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.IsOpenned():boolean;
begin
 Result := Self.tcpServer.Active;
end;//function


////////////////////////////////////////////////////////////////////////////////
// volani funkci ke klientovi

procedure TORTCPServer.Stitek(AContext: TIdContext; Blk:TBlk; stit:string);
begin
 try
   (AContext.Data as TTCPORsRef).stitek := Blk;
   Self.SendLn(AContext, '-;STIT;'+Blk.name+';'+stit+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := Blk.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Vyluka(AContext: TIdContext; Blk:TBlk; vyl:string);
begin
 try
   (AContext.Data as TTCPORsRef).vyluka := Blk;
   Self.SendLn(AContext, '-;VYL;'+Blk.name+';'+vyl+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := Blk.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Menu(AContext: TIdContext; Blk:TBlk; OblR:TOR; menu:string);
begin
 try
   (AContext.Data as TTCPORsRef).menu    := Blk;
   (AContext.Data as TTCPORsRef).menu_or := OblR;
   Self.SendLn(AContext, '-;MENU;'+menu+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[6] := Blk.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Potvr(AContext: TIdContext; callback:TPSCallback; stanice:TOR; udalost:string; senders:TBlksList; podminky:TPSPodminky; free_senders:boolean = true; free_podm:boolean = true);
var str:string;
    i:Integer;
begin
 str := '';
 if (Assigned(senders)) then
   for i := 0 to senders.Count-1 do
    if (Assigned(senders[i])) then
     begin
      if ((senders[i].ClassType = TBlk) or
          (senders[i].ClassType = TBlkVyhybka) or (senders[i].ClassType = TBlkUsek) or (senders[i].ClassType = TBlkSCom) or (senders[i].ClassType = TBlkUvazka)) then
       str := str + (senders[i] as TBlk).name + '|'
      else if (senders[i].ClassType = TOR) then
       str := str + 'Stanovištì výpravèího '+(senders[i] as TOR).Name + '|';
     end;

 str := str + ';';

 if (podminky <> nil) then
   for i := 0 to podminky.Count-1 do
     if (Assigned(podminky[i].blok)) then
      begin
       try
         str := str + '['+(podminky[i].blok as TBlk).name + '|' + podminky[i].podminka + ']';
       except

       end;
      end;

 try
   (AContext.Data as TTCPORsRef).potvr := callback;
    Self.SendLn(AContext, '-;PS;'+stanice.Name+';'+udalost+';'+str);
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := udalost;
 except

 end;

 if ((free_senders) and (Assigned(senders))) then senders.Free();
 if ((free_podm) and (Assigned(podminky))) then podminky.Free();
end;//procedure

procedure TORTCPServer.PotvrClose(AContext: TIdContext; msg:string = '');
begin
 try
   (AContext.Data as TTCPORsRef).potvr := nil;
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := '';

   if (msg <> '') then
    Self.SendLn(AContext, '-;PS-CLOSE;'+msg)
   else
    Self.SendLn(AContext, '-;PS-CLOSE;');
 except

 end;
end;//procedure

procedure TORTCPServer.PlaySound(AContext: TIdContext; code:Integer; loop:boolean = false);
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
end;//procedure

procedure TORTCPServer.DeleteSound(AContext: TIdContext; code:Integer);
begin
 if (not TTCPORsRef(AContext.Data).soundDict.ContainsKey(code)) then Exit();

 if (TTCPORsRef(AContext.Data).soundDict[code] > 0) then
  begin
   if (TTCPORsRef(AContext.Data).soundDict[code] = 1) then
     Self.SendLn(AContext, '-;SND;STOP;'+IntToStr(code)+';');
   TTCPORsRef(AContext.Data).soundDict[code] := TTCPORsRef(AContext.Data).soundDict[code] - 1;
  end;
end;//procedure

procedure TORTCPServer.BottomError(AContext: TIdContext; err:string; stanice:string; tech:string);
begin
 Self.SendLn(AContext, '-;BOTTOMERR;'+err+';'+stanice+';'+tech+';');
 writelog(tech + ' : ' + stanice + ' : ' + err, WR_ERROR);
end;//procedure

procedure TORTCPServer.UPO(AContext: TIdContext; items:TUPOItems; critical:boolean; callbackOK:TNotifyEvent; callbackEsc:TNotifyEvent; ref:TObject);
var str:string;
    i, j:Integer;
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
      str := str + PrevodySoustav.ColorToStr(items[i][j].fg) + '|';
     if (items[i][j].bg <> clNone) then
      str := str + PrevodySoustav.ColorToStr(items[i][j].bg) + '|';
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.POdj(AContext: TIdContext; SenderBlk:TBlk;
                            SenderSprId:Integer; podj:TPOdj = nil);
var str:string;
begin
 str := '-;PODJ;';

 if ((podj <> nil) and (podj.abs_enabled)) then
   str := str + FormatDateTime('hh:nn:ss', podj.abs);
 str := str + ';';

 if ((podj <> nil) and (podj.rel_enabled)) then
   str := str + FormatDateTime('nn:ss', podj.rel);
 str := str + ';';

 (AContext.Data as TTCPORsRef).podj_usek := SenderBlk;
 (AContext.Data as TTCPORsRef).podj_sprid := SenderSprId;

 Self.SendLn(AContext, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.Auth(AContext: TIdContext);
var OblR:TOR;
    i:Integer;
begin
 i := ORs.GetORIndex(parsed[0]);
 if (i = -1) then
  begin
   Self.SendInfoMsg(AContext, 'Tato OR neexistuje');
   Exit;
  end;

 ORs.GetORByIndex(i, OblR);
 if (parsed.Count < 4) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), '', '')
 else if (parsed.Count < 5) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], '')
 else
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], parsed[4]);

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastBottomError(err:string; tech:string);
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
   if (Assigned(Self.clients[i])) then
     Self.BottomError(Self.clients[i].conn, err, '-', tech);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendLn(AContext:TIDContext; str:string);
begin
 // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
 try
   AContext.Connection.IOHandler.WriteLn(str);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// gui metody
// zajistuji komunikaci s F_PanelsStatus

procedure TORTCPServer.GUIInitTable();
var i, j:Integer;
    MI:TListItem;
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.GUIRefreshLine(index:Integer; repaint:boolean = true);
var i:Integer;
    str:string;
    ORPanel:TORPanel;
    HV:THV;
    oblr:TOR;
begin
 if (not Assigned(F_Main.LV_Clients.Items.Item[index])) then
  Exit;

 if (not Assigned(Self.clients[index])) then
  begin
   // klient neexistuje
   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'odpojen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[i] := '';

   Exit();
  end;

 if (not Assigned(Self.clients[index].conn)) then
  begin
   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'soket nenalezen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[i] := '';
  end;

 case (Self.clients[index].status) of
  TPanelConnectionStatus.closed    : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'uzavøeno';
  TPanelConnectionStatus.opening   : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'otevírání';
  TPanelConnectionStatus.handshake : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'handshake';
  TPanelConnectionStatus.opened    : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'otevøeno';
 end;

 F_Main.LV_Clients.Items.Item[index].SubItems.Strings[1] := Self.clients[index].conn.Connection.Socket.Binding.PeerIP;

 for i := 0 to 2 do
  begin
   if (i < (Self.clients[index].conn.Data as TTCPORsRef).ORs.Count) then
    begin
     // klient existuje
     (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
     F_Main.LV_Clients.Items.Item[index].SubItems.Strings[2+i] :=
      (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')';
    end else begin
     // klient neexistuje
     F_Main.LV_Clients.Items.Item[index].SubItems.Strings[2+i] := '';
    end;
  end;//for i

 if ((Self.clients[index].conn.Data as TTCPORsRef).ORs.Count > 3) then
  begin
   str := '';
   for i := 3 to (Self.clients[index].conn.Data as TTCPORsRef).ORs.Count-1 do
    begin
     (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
     str := str + (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')' + ', ';
    end;
   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[5] := LeftStr(str, Length(str)-2);
  end;

 if ((Self.clients[index].conn.Data as TTCPORsRef).menu <> nil) then
  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[6] := (Self.clients[index].conn.Data as TTCPORsRef).menu.name
 else begin
  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[6] := '';
 end;

 if ((Self.clients[index].conn.Data as TTCPORsRef).vyluka <> nil) then
  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := (Self.clients[index].conn.Data as TTCPORsRef).vyluka.name
 else begin
   if ((Self.clients[index].conn.Data as TTCPORsRef).stitek <> nil) then
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := (Self.clients[index].conn.Data as TTCPORsRef).stitek.name
   else
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := '';
 end;

 if (not Assigned((Self.clients[index].conn.Data as TTCPORsRef).potvr)) then
  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[8] := '';

 if ((Self.clients[index].conn.Data as TTCPORsRef).regulator) then begin
  if (Assigned((Self.clients[index].conn.Data as TTCPORsRef).regulator_user)) then
    str := (Self.clients[index].conn.Data as TTCPORsRef).regulator_user.id
  else
    str := 'ano';

  if ((Self.clients[index].conn.Data as TTCPORsRef).regulator_loks.Count > 0) then
   begin
    str := str + ': ';
    for HV in (Self.clients[index].conn.Data as TTCPORsRef).regulator_loks do
      str := str + IntToStr(HV.adresa) + ', ';
    str := LeftStr(str, Length(str)-2);
   end;

  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[10] := str;
 end else
  F_Main.LV_Clients.Items.Item[index].SubItems.Strings[10] := '';

 str := '';
 for oblr in TTCPORsRef(Self.clients[index].conn.Data).st_hlaseni do
   str := str + oblr.ShortName + ', ';
 F_Main.LV_Clients.Items.Item[index].SubItems.Strings[11] := LeftStr(str, Length(str)-2);

 F_Main.LV_Clients.UpdateItems(index, index);
end;//procedure

procedure TORTCPServer.GUIRefreshTable();
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.GUIRefreshLine(i, false);
 F_Main.LV_Clients.Repaint();
end;//procedure

procedure TORTCPServer.GUIRefreshFromQueue();
var i:Integer;
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

procedure TORTCPServer.GUIQueueLineToRefresh(lineindex:Integer);
begin
 Self.refreshQueue[lineindex] := true;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TTCPORsRef.Create(index:Integer);
begin
 inherited Create();
 Self.regulator_loks := TList<THV>.Create();
 Self.st_hlaseni := TList<TOR>.Create();
 Self.soundDict := TDictionary<Integer, Cardinal>.Create();
 Self.ORs := TList<TOR>.Create();
 Self.regulator_zadost := nil;
 Self.index := index;
 Self.Reset();
end;

destructor TTCPORsRef.Destroy();
begin
 Self.ORs.Free();
 Self.st_hlaseni.Free();
 Self.regulator_loks.Free();
 Self.soundDict.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPORsRef.Escape(AContext: TIdContext);
var oblr:TOR;
begin
 if ((Self.stitek = nil) and (Self.vyluka = nil) and (not Assigned(Self.potvr)) and (Self.menu = nil) and (not Assigned(Self.UPO_OK))) then
   for oblr in Self.ORs do
     oblr.PanelEscape(AContext);

 Self.Reset();
end;

procedure TTCPOrsRef.Reset();
begin
 Self.stitek      := nil;
 Self.vyluka      := nil;
 Self.potvr       := nil;
 Self.menu        := nil;
 Self.menu_or     := nil;
 Self.UPO_OK      := nil;
 Self.UPO_Esc     := nil;
 Self.UPO_ref     := nil;

 Self.spr_new_usek_index := -1;
 Self.spr_edit    := nil;
 Self.spr_usek    := nil;

 Self.podj_usek   := nil;
 Self.podj_sprid  := -1;

 Self.funcsVyznamReq := false;
 Self.spr_menu_index := -1;

 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[6] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[7] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[8] := '';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.CancelUPO(AContext: TIdContext; ref:TObject);
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastData(data:string);
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.SendLn(Self.clients[i].conn, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastFuncsVyznam();
var i:Integer;
    data:string;
begin
 data := '-;F-VYZN-LIST;{'+FuncsFyznam.GetFuncsVyznam()+'}';
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and ((Self.clients[i].conn.Data as TTCPORsRef).funcsVyznamReq)) then
    Self.SendLn(Self.clients[i].conn, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DCCStart();
begin
 Self.BroadcastData('-;DCC;GO;');
 Self.DCCStopped := nil;
end;//procedure

procedure TORTCPServer.DCCStop();
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
   begin
    if ((Self.DCCStopped = Self.clients[i].conn) and (TrkSystem.status = Ttrk_status.TS_OFF)) then
      Self.SendLn(Self.clients[i].conn, '-;DCC;STOP')
    else
      Self.SendLn(Self.clients[i].conn, '-;DCC;DISABLED');
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnDCCCmdErr(Sender:TObject; Data:Pointer);
begin
 Self.BottomError(TIdContext(Data), 'Centrála neodpovìdìla na pøíkaz', '-', 'CENTRÁLA');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DisconnectClient(conn:TIdContext);
begin
 conn.Connection.Disconnect();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.GetClient(index:Integer):TORTCPClient;
begin
 if (index < _MAX_OR_CLIENTS) then
   Result := Self.clients[index]
 else
   Result := nil;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DisconnectRegulatorUser(user:TUser);
var i:Integer;
begin
 for i := _MAX_OR_CLIENTS-1 downto 0 do
   if ((Self.clients[i] <> nil) and (TTCPORsRef(Self.clients[i].conn.Data).regulator) and
       (TTCPORsRef(Self.clients[i].conn.Data).regulator_user = user)) then
    begin
     Self.SendLn(Self.clients[i].conn, '-;LOK;G;AUTH;not;Zrušeno oprávnìní regulátor');
     TCPRegulator.RegDisconnect(Self.clients[i].conn);
    end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastPing(Sedner:TObject);
begin
 try
   Self.BroadcastData('-;PING');
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.StrToPanelButton(button:string):TPanelButton;
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

constructor TORTCPClient.Create(conn:TIDContext; status:TPanelConnectionStatus = handshake);
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
