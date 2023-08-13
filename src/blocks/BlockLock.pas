unit BlockLock;

{ LOCK technological block definition. }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, IdContext,
  Generics.Collections, Area, JsonDataObjects;

type

  TBlkLockState = record
    enabled: Boolean;
    keyReleased: Boolean;
    emLock: Cardinal; // n.o. blocks who gave emergency lock
    zaver: Integer; // n.o. blocks who game me zaver
    note: string;
    error: Boolean;
  end;

  // zamek ma zaver, pokud jakakoliv vyhybka, kterou obsluhuje, ma zaver

  TBlkLock = class(TBlk)
  const
    _def_lock_state: TBlkLockState = ( // default state
      enabled: false;
      keyReleased: false;
      emLock: 0;
      zaver: 0;
      note: '';
      error: false;
    );

  private
    m_state: TBlkLockState;
    last_zaver: Boolean; // tady je ulozena posledni hodnota zaveru (aby mohlo byt rozponano, kdy volat Change)

    procedure MenuUKClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZUKClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);

    function GetZaver(): Boolean;
    function GetEmLock(): Boolean;
    function IsRightPosition(): Boolean; // vraci true, pokud jsou vyhybky s timto zamkem v poloze pro zamknuti

    procedure SetEmLock(new: Boolean);
    procedure SetNote(note: string);
    procedure SetError(new: Boolean);
    procedure SetZaver(new: Boolean);

    procedure SetKeyRelesed(new: Boolean);
    procedure PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);

    procedure CallChangeToTurnout();

  public

    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;
    // change do zamku je volano pri zmene zaveru jakekoliv vyhybky, kterou zaver obsluhuje

    // ----- lock own functions -----

    procedure DecreaseEmLock(amount: Cardinal);

    property state: TBlkLockState read m_state;

    property zaver: Boolean read GetZaver write SetZaver;
    property emLock: Boolean read GetEmLock write SetEmLock;
    property note: string read m_state.note write SetNote;
    property keyReleased: Boolean read m_state.keyReleased write SetKeyRelesed;
    property error: Boolean read m_state.error write SetError;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, BlockDb, Graphics, Diagnostics, ownConvert, ConfSeq, TechnologieJC,
  TJCDatabase, fMain, TCPServerPanel, TrainDb, THVDatabase, BlockTurnout, colorHelper;

constructor TBlkLock.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btLock;
  Self.m_state := _def_lock_state;
  Self.last_zaver := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);
  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.LoadAreas(ini_rel, 'Z').Free();
end;

procedure TBlkLock.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);
end;

procedure TBlkLock.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.Enable();
begin
  Self.m_state.enabled := true;

  // nezamykat zamek tady
  // zamek se zamyka v Disable(), tady se totiz muze stat, ze uz ho nejaka vyhybka
  // nouzove odemkla (vyhybka, ktere Enable() se vola driv)

  inherited Change();
end;

procedure TBlkLock.Disable();
begin
  Self.m_state.enabled := false;
  Self.m_state.keyReleased := false;
  Self.m_state.emLock := 0;
  Self.m_state.error := false;

  Self.Change(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.Update();
begin
  inherited Update();
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkLock.Change(now: Boolean = false);
begin
  // porucha zamku -> zrusit postavenou JC
  if ((Self.zaver) and ((Self.keyReleased) or (Self.error))) then
    JCDb.Cancel(Self);

  inherited Change(now);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.MenuUKClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((not Self.zaver) and (not Self.emLock)) then
    Self.keyReleased := true;
end;

procedure TBlkLock.MenuZUKClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.keyReleased := false;
end;

procedure TBlkLock.MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.note);
end;

procedure TBlkLock.MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.emLock := true;
end;

procedure TBlkLock.MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var csItems := TList<TConfSeqItem>.Create();
  var paths: TList<TJC> := nil;
  try
    paths := JCDb.FindActiveNCsWithLock(Self.id);
    if (paths.Count > 0) then
      csItems.Add(CSItem(Self, 'Nouzový závěr je v nouzové cestě'));
    PanelServer.ConfirmationSequence(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TArea), 'Zrušení nouzového závěru',
      GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
    if (paths <> nil) then
      paths.Free();
  end;
end;

procedure TBlkLock.PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);
begin
  if (success) then
  begin
    Self.m_state.emLock := 0;
    Self.SetEmLock(false);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkLock.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  if ((Self.keyReleased) and (Self.IsRightPosition())) then
    Result := Result + 'ZUK,';

  if ((not Self.zaver) and (not Self.emLock)) then
    if (not Self.keyReleased) then
      Result := Result + 'UK,';

  if (Self.emLock) then
    Result := Result + '!ZAV<,'
  else
    Result := Result + 'ZAV>,';

  Result := Result + 'STIT,';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (Self.m_state.enabled) then
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkLock.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (not Self.m_state.enabled) then
    Exit();

  if (item = 'UK') then
    Self.MenuUKClick(SenderPnl, SenderOR)
  else if (item = 'ZUK') then
    Self.MenuZUKClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuSTITClick(SenderPnl, SenderOR)
  else if (item = 'ZAV>') then
    Self.MenuZAVEnableClick(SenderPnl, SenderOR)
  else if (item = 'ZAV<') then
    Self.MenuZAVDisableClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkLock.GetZaver(): Boolean;
begin
  Result := (Self.m_state.zaver > 0);
end;

procedure TBlkLock.SetZaver(new: Boolean);
begin
  if (new) then
  begin
    Inc(Self.m_state.zaver);
    if (Self.m_state.zaver = 1) then
      Self.Change();
  end else begin
    if (Self.m_state.zaver > 0) then
    begin
      Dec(Self.m_state.zaver);
      if (Self.m_state.zaver = 0) then
        Self.Change();
    end;
  end;
end;

function TBlkLock.GetEmLock(): Boolean;
begin
  Result := (Self.m_state.emLock > 0);
end;

procedure TBlkLock.SetEmLock(new: Boolean);
begin
  if (new) then
  begin
    Inc(Self.m_state.emLock);
    if (Self.m_state.emLock = 1) then
    begin
      if (Self.keyReleased) then
      begin
        Self.m_state.keyReleased := false;
        Self.CallChangeToTurnout();
      end;
      inherited Change();
    end;
  end else begin
    if (Self.m_state.emLock > 0) then
      Dec(Self.m_state.emLock);
    if (Self.m_state.emLock = 0) then
    begin
      // pokud vyhybky nejsou ve spravne poloze, automaticky uvolnujeme klic
      if (not Self.IsRightPosition()) then
        Self.keyReleased := true
      else
      begin
        Self.CallChangeToTurnout();
        inherited Change();
      end;
      Blocks.NouzZaverZrusen(Self);
    end;
  end;
end;

procedure TBlkLock.SetNote(note: string);
begin
  if (note <> Self.m_state.note) then
  begin
    Self.m_state.note := note;
    inherited Change();
  end;
end;

procedure TBlkLock.SetKeyRelesed(new: Boolean);
begin
  if (new <> Self.keyReleased) then
  begin
    Self.m_state.keyReleased := new;
    if (not new) then
      Self.m_state.error := false;
    Self.CallChangeToTurnout();
    inherited Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.CallChangeToTurnout();
var list: TBlksList;
  i: Integer;
begin
  list := Blocks.GetTurnoutWithLock(Self.id);

  for i := 0 to list.Count - 1 do
    (list[i] as TBlkTurnout).Change();

  list.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.SetError(new: Boolean);
begin
  if (Self.m_state.error <> new) then
  begin
    Self.m_state.error := new;
    if (new) then // vyvolani poruchy vlivem odpadeni vyhybky zpusobi uvolneni klice; VZDYCKY !!
      Self.m_state.keyReleased := true;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkLock.IsRightPosition(): Boolean;
var list: TBlksList;
begin
  list := Blocks.GetTurnoutWithLock(Self.id);

  for var i: Integer := 0 to list.Count - 1 do
    if (TBlkTurnout(list[i]).position <> (list[i] as TBlkTurnout).GetSettings().lockPosition) then
    begin
      list.Free();
      Exit(false);
    end;

  list.Free();
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.DecreaseEmLock(amount: Cardinal);
begin
  if (Self.m_state.emLock = 0) then
    Exit();

  if (amount > Self.m_state.emLock) then
    Self.m_state.emLock := 0
  else
    Self.m_state.emLock := Self.m_state.emLock - amount;

  if (not Self.emLock) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkLock.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  if (Self.note <> '') then
    bg := TJopColor.turqDark
  else
    bg := TJopColor.black;

  if ((diag.showZaver) and (Self.zaver)) then
    bg := TJopColor.greenDark;

  if (Self.error) then
  begin
    fg := bg;
    bg := TJopColor.blue;
  end else if (Self.keyReleased) then
    fg := TJopColor.blue
  else if (Self.emLock) then
    fg := TJopColor.turq
  else
    fg := TJopColor.grayDark;

  Result := Result + ownConvert.ColorToStr(fg) + ';';
  Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLock.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;
  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkLock.GetPtState(json: TJsonObject);
begin
  json['enabled'] := Self.m_state.enabled;
  json['keyReleased'] := Self.m_state.keyReleased;
  json['emLock'] := Self.m_state.emLock;
  json['zaver'] := Self.m_state.zaver;
  json['note'] := Self.m_state.note;
  json['error'] := Self.m_state.error;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
