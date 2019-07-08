unit TCPORsRef;

interface

uses Generics.Collections, TOblRizeni, TBlok, Classes, Souprava, User,
     THnaciVozidlo, IdContext, SysUtils;

type
  TPSCallback = procedure (Sender:TIdContext; success:boolean) of object;

  TORPing = record
    id: Cardinal;
    sent: TDateTime;
  end;

  EPingNotYetComputed = class(Exception);

  // tady je ulozeno jedno fyzicke spojeni s panelem (obsahuje oblasti rizeni, otevrene okynko stitku, menu, ...)
  TTCPORsRef = class
   const
    _PING_WINDOW_SIZE = 5;
    _PING_PERIOD_NOREG = 5;
    _PING_PERIOD_REG = 1;
    _PING_TRYOUT = 3;

   private
    procedure CheckSendPing(AContext:TIdContext);
    procedure CheckUnreachable(AContext:TIdContext);

    function GetPing():TTime;
    procedure OnUnreachable(AContext:TIdContext);
    procedure PingNewReceived(time:TTime);

   public
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

    ping_next_id: Cardinal;
    ping_next_send: TDateTime;
    ping_sent: TQueue<TORPing>;
    ping_received: TList<TTime>;
    ping_received_next_index: Integer;
    ping_unreachable: Boolean;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    procedure Escape(AContext: TIdContext);                                     // volano pri stisku Escape v panelu
    procedure Reset();
    procedure ResetSpr();

    class function ORPing(id: Cardinal; sent: TDateTime):TORPing;
    function PingComputed():boolean;
    procedure PingUpdate(AContext:TIdContext);
    procedure PongReceived(id:Cardinal);

    property ping: TTime read GetPing;
  end;

implementation

uses fMain, TCPServerOR;

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
 Self.ping_sent := TQueue<TORPing>.Create();
 Self.ping_received := TList<TTime>.Create();
 Self.ping_next_send := 0;
 Self.ping_received_next_index := 0;
 Self.ping_unreachable := false;
 Self.Reset();
end;

destructor TTCPORsRef.Destroy();
begin
 Self.ORs.Free();
 Self.st_hlaseni.Free();
 Self.regulator_loks.Free();
 Self.soundDict.Free();
 Self.ping_sent.Free();
 Self.ping_received.Free();
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
 Self.ResetSpr();

 Self.podj_usek   := nil;
 Self.podj_sprid  := -1;

 Self.funcsVyznamReq := false;
 Self.spr_menu_index := -1;

 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[6] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[7] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[8] := '';
end;

procedure TTCPOrsRef.ResetSpr();
begin
 Self.spr_new_usek_index := -1;
 Self.spr_edit    := nil;
 Self.spr_usek    := nil;
end;

////////////////////////////////////////////////////////////////////////////////

class function TTCPOrsRef.ORPing(id: Cardinal; sent: TDateTime):TORPing;
begin
 Result.id := id;
 Result.sent := sent;
end;

////////////////////////////////////////////////////////////////////////////////

function TTCPOrsRef.PingComputed():boolean;
begin
 Result := (Self.ping_received.Count > 0);
end;

function TTCPOrsRef.GetPing():TTime;
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

procedure TTCPORsRef.OnUnreachable(AContext:TIdContext);
begin
 Self.ping_received.Clear();
 ORTCPServer.GUIQueueLineToRefresh(Self.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPORsRef.CheckSendPing(AContext:TIdContext);
begin
 if (Now >= Self.ping_next_send) then
  begin
   ORTCPServer.SendLn(AContext, '-;PING;REQ-RESP;'+IntToStr(Self.ping_next_id));
   Self.ping_sent.Enqueue(ORPing(Self.ping_next_id, Now));
   if (Self.regulator_loks.Count > 0) then
     Self.ping_next_send := Now+EncodeTime(0, 0, _PING_PERIOD_REG, 0)
   else
     Self.ping_next_send := Now+EncodeTime(0, 0, _PING_PERIOD_NOREG, 0);
   Self.ping_next_id := Self.ping_next_id + 1;
   if (Self.ping_unreachable) then
     Self.ping_sent.Dequeue(); // do not fill queue with inifinite amount of data
  end;
end;

procedure TTCPORsRef.PongReceived(id:Cardinal);
var orPing: TORPing;
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

procedure TTCPORsRef.CheckUnreachable(AContext:TIdContext);
begin
 if ((Self.ping_sent.Count > _PING_TRYOUT) and (not Self.ping_unreachable)) then
  begin
   Self.OnUnreachable(AContext);
   Self.ping_unreachable := true; // set flag
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPORsRef.PingUpdate(AContext:TIdContext);
begin
 Self.CheckSendPing(AContext);
 Self.CheckUnreachable(AContext);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPORsRef.PingNewReceived(time:TTime);
begin
 if (Self.ping_unreachable) then
  begin
   Self.ping_unreachable := false; // client restored
   ORTCPServer.GUIQueueLineToRefresh(Self.index);
  end;

 if (Self.ping_received.Count <= Self.ping_received_next_index) then
   Self.ping_received.Add(time)
 else
   Self.ping_received[Self.ping_received_next_index] := time;

 Self.ping_received_next_index := (Self.ping_received_next_index+1) mod _PING_WINDOW_SIZE;

 ORTCPServer.GUIQueueLineToRefresh(Self.index);
end;

////////////////////////////////////////////////////////////////////////////////

// TODO: kontrola co se bude dit kdyz dlouho neodpovida (neplnit frontu)

end.
