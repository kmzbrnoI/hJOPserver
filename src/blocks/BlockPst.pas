unit BlockPst;

{ Pst technological block definition. }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, IdContext,
  Generics.Collections, Area, JsonDataObjects, TechnologieRCS;

type
  TBlkPstStatus = (pstDisabled, pstOff, pstTakeReady, pstRefuging, pstTaken);

  TBlkPstSettings = record
    tracks: TList<Integer>;
    turnouts: TList<Integer>;
    signals: TList<Integer>;
    refugees: TList<Integer>;
    rcsInTake: TRCSAddr;
    rcsInRelease: TRCSAddr;
    rcsOutTaken: TRCSAddr;
    rcsOutHorn: TRCSAddr;
  end;

  TBlkPstState = record
    status: TBlkPstStatus;
    emLock: Cardinal; // n.o. blocks who gave emergency lock
    note: string;
  end;

  // Pst has zaver if any track in pst has zaver

  TBlkPst = class(TBlk)
  const
    _def_pst_state: TBlkPstState = ( // default state
      status: pstDisabled;
      emLock: 0;
      note: '';
    );

  private
    m_settings: TBlkPstSettings;
    m_state: TBlkPstState;

    procedure MenuPstEnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPstDisClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNPstClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);

    function GetZaver(): Boolean;
    function GetEmLock(): Boolean;
    function GetEnabled(): Boolean;

    procedure SetEmLock(new: Boolean);
    procedure SetNote(note: string);

  public

    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    // ----- pst own functions -----

    procedure DecreaseEmLock(amount: Cardinal);

    property state: TBlkPstState read m_state;
    property status: TBLKPstStatus read m_state.status;

    property zaver: Boolean read GetZaver;
    property emLock: Boolean read GetEmLock write SetEmLock;
    property note: string read m_state.note write SetNote;
    property enabled: Boolean read GetEnabled;

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

uses GetSystems, BlockDb, Graphics, Diagnostics, ownConvert,
  TJCDatabase, fMain, TCPServerPanel, TrainDb, THVDatabase, BlockTurnout;

constructor TBlkPst.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btPst;
  Self.m_state := _def_pst_state;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);
  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.LoadAreas(ini_rel, 'Pst').Free();
end;

procedure TBlkPst.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);
end;

procedure TBlkPst.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
  ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.Enable();
begin
  Self.m_state.status := pstOff;
  Self.Change();
end;

procedure TBlkPst.Disable();
begin
  Self.m_state.status := pstDisabled;
  Self.m_state.emLock := 0;
  Self.Change(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.Update();
begin
  inherited Update();
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkPst.Change(now: Boolean = false);
begin
  inherited Change(now);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.MenuPstEnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuPstDisClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuNPstClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.note);
end;

procedure TBlkPst.MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkPst.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  if ((Self.status = pstOff) and (not Self.zaver)) then
    Result := Result + 'PST>,'
  else if (Self.status = pstTakeReady) then
    Result := Result + 'PST<,'
  else if (Self.status = pstTaken) then
    Result := Result + '!NPST,';

  Result := Result + 'STIT,';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (Self.enabled) then
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkPst.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (not Self.enabled) then
    Exit();

  if (item = 'PST>') then
    Self.MenuPstEnClick(SenderPnl, SenderOR)
  else if (item = 'PST<') then
    Self.MenuPstDisClick(SenderPnl, SenderOR)
  else if (item = 'NPST') then
    Self.MenuNPstClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuSTITClick(SenderPnl, SenderOR)
  else if (item = 'ZAV>') then
    Self.MenuZAVEnableClick(SenderPnl, SenderOR)
  else if (item = 'ZAV<') then
    Self.MenuZAVDisableClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkPst.GetEnabled(): Boolean;
begin
  Result := (Self.status > pstDisabled);
end;

function TBlkPst.GetZaver(): Boolean;
begin
//  Result := (Self.m_state.zaver > 0);
// TODO
end;

function TBlkPst.GetEmLock(): Boolean;
begin
  Result := (Self.m_state.emLock > 0);
end;

procedure TBlkPst.SetEmLock(new: Boolean);
begin
  if (new) then
  begin
    Inc(Self.m_state.emLock);
    if (Self.m_state.emLock = 1) then
    begin
      {if (Self.keyReleased) then
      begin
        Self.m_state.keyReleased := false;
        Self.CallChangeToTurnout();
      end;}
      inherited Change();
    end;
  end else begin
    if (Self.m_state.emLock > 0) then
      Dec(Self.m_state.emLock);
    if (Self.m_state.emLock = 0) then
    begin
      // pokud vyhybky nejsou ve spravne poloze, automaticky uvolnujeme klic
      {if (not Self.IsRightPoloha()) then
        Self.keyReleased := true
      else
      begin
        Self.CallChangeToTurnout();
        inherited Change();
      end;
      Blocks.NouzZaverZrusen(Self);}
    end;
  end;
end;

procedure TBlkPst.SetNote(note: string);
begin
  if (note <> Self.m_state.note) then
  begin
    Self.m_state.note := note;
    inherited Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.DecreaseEmLock(amount: Cardinal);
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

function TBlkPst.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  if (Self.note <> '') then
    bg := clTeal
  else
    bg := clBlack;

  if ((diag.showZaver) and (Self.zaver)) then
    bg := clGreen;

  case (Self.status) of
    pstOff: fg := $A0A0A0;
    pstTakeReady: fg := clWhite;
    pstTaken: fg := clBlue;
  else
    fg := clFuchsia;
  end;

  if (Self.emLock) then
    fg := clAqua;

  Result := Result + ownConvert.ColorToStr(fg) + ';';
  Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;
  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkPst.GetPtState(json: TJsonObject);
begin
  // TODO
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
