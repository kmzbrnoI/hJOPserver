unit TCPAreasRef;

interface

uses Generics.Collections, Area, Block, Classes, Train, User,
     THnaciVozidlo, IdContext, SysUtils;

type
  TCSCallback = procedure (Sender: TIdContext; success: Boolean) of object;

  TAreaPing = record
    id: Cardinal;
    sent: TDateTime;
  end;

  EPingNotYetComputed = class(Exception);

  // tady je ulozeno jedno fyzicke spojeni s panelem (obsahuje oblasti rizeni, otevrene okynko stitku, menu, ...)
  TPanelConnData = class
   const
    _PING_WINDOW_SIZE = 3;
    _PING_PERIOD_NOREG = 5;
    _PING_PERIOD_REG = 1;
    _PING_TRYOUT = 3;

   private
     procedure CheckSendPing(AContext: TIdContext);
     procedure CheckUnreachable(AContext: TIdContext);

     function GetPing(): TTime;
     procedure OnUnreachable(AContext: TIdContext);
     procedure PingNewReceived(time: TTime);

   public
    areas: TList<TArea>;
    protocol_version: string;

    note: TBlk;                                                                 // blok, na kterema kutalne probiha zmena stitku
    lockout: TBlk;                                                              // blok, na kterem aktualne probiha zmena vyluky
    potvr: TCSCallback;                                                         // callback probihajici potvrzovaci sekvence
    menu: TBlk;                                                                 // blok, kteremu odeslat callback kliku na polozku v menu
    menu_or: TArea;                                                             // OR, ze ktere bylo vyvolano menu
    UPO_OK, UPO_Esc: TNotifyEvent;                                              // callbacky manipulace s upozornenim vlevo dole
    UPO_ref: TObject;                                                           //
    index: Integer;                                                             // index spojeni v tabulce ve F_Main
    funcsVyznamReq: Boolean;                                                    // jestli mame panelu odesilat zmeny vyznamu funkci; zmeny se odesilaji jen, pokud panel alespon jednou zazadal o seznam vyznamu funkci
    maus: Boolean;                                                              // jestli je k panelu pripojeny uLI-daemon pripraveny prijimat adresy

    train_new_usek_index: Integer;                                              // index nove vytvarene soupravy na useku (-1 pokud neni vytvarena)
    train_edit: TTrain;                                                         // souprava, kterou panel edituje
    train_usek: TObject;                                                        // usek, na kterem panel edituje soupravu (TBlkUsek)

    regulator: Boolean;                                                         // true pokud klient autorizoval rizeni pres regulator
    regulator_user: TUser;                                                      // uzivatel, ktery autorizoval regulator
    regulator_zadost: TArea;                                                    // oblast rizeni, do ktere probiha zadost o hnaci vozidlo
    regulator_loks: TList<THV>;                                                 // seznam lokomotiv v regulatoru

    st_hlaseni: TList<TArea>;                                                   // stanice, do kterych je autorizovano stanicni hlaseni
    train_menu_index: Integer;                                                  // index sopuravy, ktere se aktualne zorbazuje menu (viz blok usek)

    soundDict: TDictionary<Integer, Cardinal>;                                  // pro kazdy zvuk obsahuje pocet jeho prehravani
                                                                                // predpoklada se, ze kazda OR si resi zvuku samostatne, az tady se to spojuje

    podj_track: TBlk;                                                           // data pro editaci predvidaneho odjezdu
    podj_trainid: Integer;

    ping_next_id: Cardinal;
    ping_next_send: TDateTime;
    ping_sent: TQueue<TAreaPing>;
    ping_received: TList<TTime>;
    ping_received_next_index: Integer;
    ping_unreachable: Boolean;

     constructor Create(index: Integer);
     destructor Destroy(); override;

     procedure Escape(AContext: TIdContext);                                     // volano pri stisku Escape v panelu
     procedure Reset();
     procedure ResetTrains();

     class function ORPing(id: Cardinal; sent: TDateTime): TAreaPing;
     function PingComputed(): Boolean;
     procedure PingUpdate(AContext: TIdContext);
     procedure PongReceived(id: Cardinal);

     property ping: TTime read GetPing;
  end;

implementation

uses fMain, TCPServerPanel, RegulatorTCP;

////////////////////////////////////////////////////////////////////////////////

constructor TPanelConnData.Create(index: Integer);
begin
 inherited Create();
 Self.regulator_loks := TList<THV>.Create();
 Self.st_hlaseni := TList<TArea>.Create();
 Self.soundDict := TDictionary<Integer, Cardinal>.Create();
 Self.areas := TList<TArea>.Create();
 Self.regulator_zadost := nil;
 Self.index := index;
 Self.ping_sent := TQueue<TAreaPing>.Create();
 Self.ping_received := TList<TTime>.Create();
 Self.ping_next_send := Now+EncodeTime(0, 0, 0, 500); // do not disturb device for first half a sec
 Self.ping_received_next_index := 0;
 Self.ping_unreachable := false;
 Self.Reset();
end;

destructor TPanelConnData.Destroy();
begin
 Self.areas.Free();
 Self.st_hlaseni.Free();
 Self.regulator_loks.Free();
 Self.soundDict.Free();
 Self.ping_sent.Free();
 Self.ping_received.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.Escape(AContext: TIdContext);
var area: TArea;
begin
 if ((Self.note = nil) and (Self.lockout = nil) and (not Assigned(Self.potvr)) and (Self.menu = nil) and (not Assigned(Self.UPO_OK))) then
   for area in Self.areas do
     area.PanelEscape(AContext);

 Self.Reset();
end;

procedure TPanelConnData.Reset();
begin
 Self.note := nil;
 Self.lockout := nil;
 Self.potvr := nil;
 Self.menu := nil;
 Self.menu_or := nil;
 Self.UPO_OK := nil;
 Self.UPO_Esc := nil;
 Self.UPO_ref := nil;
 Self.ResetTrains();

 Self.podj_track := nil;
 Self.podj_trainid := -1;

 Self.funcsVyznamReq := false;
 Self.train_menu_index := -1;

 F_Main.LV_Clients.Items[Self.index].SubItems[_LV_CLIENTS_COL_MENU] := '';
 F_Main.LV_Clients.Items[Self.index].SubItems[_LV_CLIENTS_COL_STIT] := '';
 F_Main.LV_Clients.Items[Self.index].SubItems[_LV_CLIENTS_COL_RIZ] := '';
end;

procedure TPanelConnData.ResetTrains();
begin
 Self.train_new_usek_index := -1;
 Self.train_edit := nil;
 Self.train_usek := nil;
end;

////////////////////////////////////////////////////////////////////////////////

class function TPanelConnData.ORPing(id: Cardinal; sent: TDateTime): TAreaPing;
begin
 Result.id := id;
 Result.sent := sent;
end;

////////////////////////////////////////////////////////////////////////////////

function TPanelConnData.PingComputed(): Boolean;
begin
 Result := (Self.ping_received.Count > 0);
end;

function TPanelConnData.GetPing(): TTime;
var sum, ping: TTime;
begin
 if (Self.ping_received.Count = 0) then
   raise EPingNotYetComputed.Create('Ping not yet computed!');

 sum := 0;
 for ping in Self.ping_received do
   sum := sum + ping;
 Result := sum / Self.ping_received.Count;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.OnUnreachable(AContext: TIdContext);
var i: Integer;
begin
 // clear loco always (even when called after several attempts)
 for i := Self.regulator_loks.Count-1 downto 0 do
   TCPRegulator.RemoveLok(AContext, Self.regulator_loks[i], 'Zařízení neodpovídá na ping!');

 if (not Self.ping_unreachable) then
  begin
   Self.ping_received.Clear();
   PanelServer.GUIQueueLineToRefresh(Self.index);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.CheckSendPing(AContext: TIdContext);
begin
 if (Now >= Self.ping_next_send) then
  begin
   PanelServer.SendLn(AContext, '-;PING;REQ-RESP;'+IntToStr(Self.ping_next_id));
   Self.ping_sent.Enqueue(ORPing(Self.ping_next_id, Now));

   if (Self.regulator_loks.Count > 0) then
     Self.ping_next_send := Now+EncodeTime(0, 0, _PING_PERIOD_REG, 0)
   else
     Self.ping_next_send := Now+EncodeTime(0, 0, _PING_PERIOD_NOREG, 0);

   if (Self.ping_next_id < High(Cardinal)) then
     Self.ping_next_id := Self.ping_next_id + 1
   else
     Self.ping_next_id := 0;

   if (Self.ping_unreachable) then
     Self.ping_sent.Dequeue(); // do not fill queue with inifinite amount of data
  end;
end;

procedure TPanelConnData.PongReceived(id: Cardinal);
var orPing: TAreaPing;
begin
 while (Self.ping_sent.Count > 0) do
  begin
   orPing := Self.ping_sent.Dequeue();
   if (orPing.id = id) then
    begin
     Self.PingNewReceived(Now-orPing.sent);
     Exit();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.CheckUnreachable(AContext: TIdContext);
begin
 if (Self.ping_sent.Count > _PING_TRYOUT) then
  begin
   Self.OnUnreachable(AContext);
   Self.ping_unreachable := true; // set flag
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.PingUpdate(AContext: TIdContext);
begin
 Self.CheckSendPing(AContext);
 Self.CheckUnreachable(AContext);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.PingNewReceived(time: TTime);
begin
 if (Self.ping_unreachable) then
  begin
   Self.ping_unreachable := false; // client restored
   PanelServer.GUIQueueLineToRefresh(Self.index);
  end;

 if (Self.ping_received.Count <= Self.ping_received_next_index) then
   Self.ping_received.Add(time)
 else
   Self.ping_received[Self.ping_received_next_index] := time;

 Self.ping_received_next_index := (Self.ping_received_next_index+1) mod _PING_WINDOW_SIZE;

 PanelServer.GUIQueueLineToRefresh(Self.index);
end;

////////////////////////////////////////////////////////////////////////////////

// TODO: kontrola co se bude dit kdyz dlouho neodpovida (neplnit frontu)

end.
