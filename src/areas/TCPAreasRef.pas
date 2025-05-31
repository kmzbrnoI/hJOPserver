unit TCPAreasRef;

interface

uses Generics.Collections, Area, Block, Classes, Train, User,
  THnaciVozidlo, IdContext, SysUtils, TechnologieJC;

type
  TCSCallback = procedure(Sender: TIdContext; success: Boolean) of object;

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

    class procedure HidePathBlock(blk: TBlk);

  public
    areas: TList<TArea>;
    protocol_version: string;
    client_name: string;

    note: TBlk; // blok, na kterema kutalne probiha zmena stitku
    lockout: TBlk; // blok, na kterem aktualne probiha zmena vyluky
    potvr: TCSCallback; // callback probihajici potvrzovaci sekvence
    menu: TBlk; // blok, kteremu odeslat callback kliku na polozku v menu
    menu_or: TArea; // OR, ze ktere bylo vyvolano menu
    UPO_OK, UPO_Esc: TNotifyEvent; // callbacky manipulace s upozornenim vlevo dole
    UPO_ref: TObject; //
    index: Integer; // index spojeni v tabulce ve F_Main
    funcsVyznamReq: Boolean;
    // jestli mame panelu odesilat zmeny vyznamu funkci; zmeny se odesilaji jen, pokud panel alespon jednou zazadal o seznam vyznamu funkci
    maus: Boolean; // jestli je k panelu pripojeny uLI-daemon pripraveny prijimat adresy

    // bloky aktualne zadavane JC (se zobrazenym podbarvenim)
    // 0. blok je vzdy navestidlo, nasleduji variantni body, posledni blok je posledni usek JC
    pathBlocks: TList<TBlk>;

    lastActivatedPath: TJC;

    train_new_usek_index: Integer; // index nove vytvarene soupravy na useku (-1 pokud neni vytvarena)
    train_edit: TTrain; // souprava, kterou panel edituje
    train_usek: TObject; // usek, na kterem panel edituje soupravu (TBlkUsek)

    regulator: Boolean; // true pokud klient autorizoval rizeni pres regulator
    regulator_user: TUser; // uzivatel, ktery autorizoval regulator
    regulator_zadost: TArea; // oblast rizeni, do ktere probiha zadost o hnaci vozidlo
    regulator_loks: TList<THV>; // seznam lokomotiv v regulatoru

    st_hlaseni: TList<TArea>; // stanice, do kterych je autorizovano stanicni hlaseni
    train_menu_index: Integer; // index sopuravy, ktere se aktualne zorbazuje menu (viz blok usek)

    soundDict: TDictionary<Integer, Cardinal>; // pro kazdy zvuk obsahuje pocet jeho prehravani
    // predpoklada se, ze kazda OR si resi zvuku samostatne, az tady se to spojuje

    podj_track: TBlk; // data pro editaci predvidaneho odjezdu
    podj_trainid: Integer;

    ping_next_id: Cardinal;
    ping_next_send: TDateTime;
    ping_sent: TQueue<TAreaPing>;
    ping_received: TList<TTime>;
    ping_received_next_index: Integer;
    ping_unreachable: Boolean;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    function Escape(AContext: TIdContext): Boolean; // volano pri stisku Escape v panelu
    procedure ResetTrains();

    class function ORPing(id: Cardinal; sent: TDateTime): TAreaPing;
    function PingComputed(): Boolean;
    procedure PingUpdate(AContext: TIdContext);
    procedure PongReceived(id: Cardinal);

    procedure ClearAndHidePathBlocks();
    procedure DeleteLastPathBlock();
    procedure DeleteAndHideLastPathBlock();
    function PathIsStartSignal(): Boolean;

    property ping: TTime read GetPing;
  end;

implementation

uses fMain, TCPServerPanel, RegulatorTCP, BlockSignal, BlockTrack;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelConnData.Create(index: Integer);
begin
  inherited Create();

  Self.areas := TList<TArea>.Create();
  Self.protocol_version := '';
  Self.client_name := '';
  Self.note := nil;
  Self.lockout := nil;
  Self.potvr := nil;
  Self.menu := nil;
  Self.menu_or := nil;
  Self.UPO_OK := nil;
  Self.UPO_Esc := nil;
  Self.UPO_ref := nil;
  Self.index := index;
  Self.funcsVyznamReq := False;
  Self.maus := False;
  Self.pathBlocks := TList<TBlk>.Create();
  Self.lastActivatedPath := nil;
  Self.train_new_usek_index := -1;
  Self.train_edit := nil;
  Self.train_usek := nil;
  Self.regulator := False;
  Self.regulator_user := nil;
  Self.regulator_zadost := nil;
  Self.regulator_loks := TList<THV>.Create();
  Self.st_hlaseni := TList<TArea>.Create();
  Self.train_menu_index := -1;
  Self.soundDict := TDictionary<Integer, Cardinal>.Create();
  Self.podj_track := nil;
  Self.podj_trainid := -1;

  Self.ping_next_id := 0;
  Self.ping_next_send := Now + EncodeTime(0, 0, 0, 500); // do not disturb device for first half a sec
  Self.ping_sent := TQueue<TAreaPing>.Create();
  Self.ping_received := TList<TTime>.Create();
  Self.ping_received_next_index := 0;
  Self.ping_unreachable := false;
end;

destructor TPanelConnData.Destroy();
begin
  Self.pathBlocks.Free();
  Self.areas.Free();
  Self.st_hlaseni.Free();
  Self.regulator_loks.Free();
  Self.soundDict.Free();
  Self.ping_sent.Free();
  Self.ping_received.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelConnData.Escape(AContext: TIdContext): Boolean;
begin
  Result := false; // Result = handled

  if (Self.note <> nil) then
  begin
    Self.note := nil;
    Result := true;
  end;

  if (Self.lockout <> nil) then
  begin
    Self.lockout := nil;
    Result := true;
  end;

  if (Assigned(Self.potvr)) then
  begin
    Self.potvr := nil;
    Result := true;
  end;

  if (Self.menu <> nil) then
  begin
    Self.menu := nil;
    Self.menu_or := nil;
    Self.train_menu_index := -1;
    Result := true;
  end;

  if (Self.UPO_ref <> nil) then
  begin
    Self.UPO_OK := nil;
    Self.UPO_Esc := nil;
    Self.UPO_ref := nil;
    Result := true;
  end;

  if ((Self.train_edit <> nil) or (Self.train_usek <> nil)) then
  begin
    Self.ResetTrains();
    Result := true;
  end;

  if (Self.podj_track <> nil) then
  begin
    Self.podj_track := nil;
    Self.podj_trainid := -1;
    Result := true;
  end;

  if ((not Result) and (Self.pathBlocks.Count > 0)) then
  begin
    Self.DeleteAndHideLastPathBlock();
    Result := true;
  end;

  Self.funcsVyznamReq := false;

  if (not Result) then
    for var area: TArea in Self.areas do
      area.PanelEscape(AContext);
end;

procedure TPanelConnData.ResetTrains();
begin
  Self.train_new_usek_index := -1;
  Self.train_edit := nil;
  Self.train_usek := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TPanelConnData.ORPing(id: Cardinal; sent: TDateTime): TAreaPing;
begin
  Result.id := id;
  Result.sent := sent;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelConnData.PingComputed(): Boolean;
begin
  Result := (Self.ping_received.Count > 0);
end;

function TPanelConnData.GetPing(): TTime;
begin
  if (Self.ping_received.Count = 0) then
    raise EPingNotYetComputed.Create('Ping not yet computed!');

  var sum: TTime := 0;
  for var ping: TTime in Self.ping_received do
    sum := sum + ping;
  Result := sum / Self.ping_received.Count;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.OnUnreachable(AContext: TIdContext);
begin
  // clear loco always (even when called after several attempts)
  for var i: Integer := Self.regulator_loks.Count - 1 downto 0 do
    TCPRegulator.RemoveLok(AContext, Self.regulator_loks[i], 'Zařízení neodpovídá na ping!');

  if (not Self.ping_unreachable) then
  begin
    Self.ping_received.Clear();
    PanelServer.GUIQueueLineToRefresh(Self.index);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.CheckSendPing(AContext: TIdContext);
begin
  if (Now >= Self.ping_next_send) then
  begin
    PanelServer.SendLn(AContext, '-;PING;REQ-RESP;' + IntToStr(Self.ping_next_id));
    Self.ping_sent.Enqueue(ORPing(Self.ping_next_id, Now));

    if (Self.regulator_loks.Count > 0) then
      Self.ping_next_send := Now + EncodeTime(0, 0, _PING_PERIOD_REG, 0)
    else
      Self.ping_next_send := Now + EncodeTime(0, 0, _PING_PERIOD_NOREG, 0);

    if (Self.ping_next_id < High(Cardinal)) then
      Self.ping_next_id := Self.ping_next_id + 1
    else
      Self.ping_next_id := 0;

    if (Self.ping_unreachable) then
      Self.ping_sent.Dequeue(); // do not fill queue with inifinite amount of data
  end;
end;

procedure TPanelConnData.PongReceived(id: Cardinal);
begin
  while (Self.ping_sent.Count > 0) do
  begin
    var ORPing: TAreaPing := Self.ping_sent.Dequeue();
    if (ORPing.id = id) then
    begin
      Self.PingNewReceived(Now - ORPing.sent);
      Exit();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.CheckUnreachable(AContext: TIdContext);
begin
  if (Self.ping_sent.Count > _PING_TRYOUT) then
  begin
    Self.OnUnreachable(AContext);
    Self.ping_unreachable := true; // set flag
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.PingUpdate(AContext: TIdContext);
begin
  Self.CheckSendPing(AContext);
  Self.CheckUnreachable(AContext);
end;

/// /////////////////////////////////////////////////////////////////////////////

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

  Self.ping_received_next_index := (Self.ping_received_next_index + 1) mod _PING_WINDOW_SIZE;

  PanelServer.GUIQueueLineToRefresh(Self.index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelConnData.ClearAndHidePathBlocks();
begin
  for var blk: TBlk in Self.pathBlocks do
    Self.HidePathBlock(blk);

  Self.pathBlocks.Clear();
end;

procedure TPanelConnData.DeleteLastPathBlock();
begin
  if (Self.pathBlocks.Count = 0) then
    raise Exception.Create('pathBlocks empty');
  Self.pathBlocks.Delete(Self.pathBlocks.Count-1);
end;

procedure TPanelConnData.DeleteAndHideLastPathBlock();
begin
  if (Self.pathBlocks.Count = 0) then
    raise Exception.Create('pathBlocks empty');
  Self.HidePathBlock(Self.pathBlocks[Self.pathBlocks.Count-1]);
  Self.DeleteLastPathBlock();
end;

function TPanelConnData.PathIsStartSignal(): Boolean;
begin
  Result := ((Self.pathBlocks.Count > 0) and (Self.pathBlocks[0].typ = btSignal));
end;

class procedure TPanelConnData.HidePathBlock(blk: TBlk);
begin
  case (blk.typ) of
    btSignal:
      TBlkSignal(blk).selected := TBlkSignalSelection.none;
    btTrack, btRT:
      TBlkTrack(blk).jcEnd := TZaver.no;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// TODO: kontrola co se bude dit kdyz dlouho neodpovida (neplnit frontu)

end.
