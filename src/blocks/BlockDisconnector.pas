unit BlockDisconnector;

{ DISCONNECTOR technological blokc definition }

interface

uses IniFiles, Block, Classes, AreaDb, SysUtils, JsonDataObjects,
  IdContext, Area, TechnologieRCS;

type
  TBlkDiscBasicState = (disabled = -5, not_selected = 0, mounting = 1, active = 2);

  TBlkDiscSettings = record
    RCSAddrs: TRCSAddrs; // only 1 address
  end;

  TBlkDiscState = record
    state: TBlkDiscBasicState;
    finish: TDateTime;
    rcsFailed: Boolean;
    note: string;
  end;

  TBlkDisconnector = class(TBlk)
  const
    _def_rozp_stav: TBlkDiscState = (state: disabled; rcsFailed: false; note: '';);

  private const
    _MOUNT_TO_ACTIVE_TIME_SEC = 3;
    _ACTIVE_TO_DISABLE_TIME_SEC = 30;

  private
    RozpSettings: TBlkDiscSettings;
    RozpStav: TBlkDiscState;

    procedure SetState(status: TBlkDiscBasicState);
    procedure UpdateOutput();

    procedure SetNote(note: string);

    procedure Mount();
    procedure Activate();
    procedure Prolong();

    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);

  public
    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;

    // ----- Disconnector specific functions -----

    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    function GetSettings(): TBlkDiscSettings;
    procedure SetSettings(data: TBlkDiscSettings);

    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    property fullState: TBlkDiscState read RozpStav;
    property state: TBlkDiscBasicState read RozpStav.state write SetState;
    property note: string read RozpStav.note write SetNote;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses TCPServerPanel, ownConvert, Graphics, PTUtils;

constructor TBlkDisconnector.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btDisconnector;
  Self.RozpStav := Self._def_rozp_stav;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.RozpSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
  Self.LoadORs(ini_rel, 'R').Free();
  PushRCSToArea(Self.m_areas, Self.RozpSettings.RCSAddrs);

  Self.RozpStav.note := ini_stat.ReadString(section, 'stit', '');
end;

procedure TBlkDisconnector.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.RozpSettings.RCSAddrs);
end;

procedure TBlkDisconnector.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.RozpStav.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.RozpStav.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Enable();
var rcsaddr: TRCSAddr;
  Enable: Boolean;
begin
  Enable := true;
  try
    for rcsaddr in Self.RozpSettings.RCSAddrs do
      if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
        Enable := false;
  except
    Enable := false;
  end;

  Self.RozpStav.rcsFailed := not Enable;

  if (Enable) then
    Self.state := TBlkDiscBasicState.not_selected;
end;

procedure TBlkDisconnector.Disable();
begin
  Self.state := TBlkDiscBasicState.disabled;
  Self.RozpStav.rcsFailed := false;
  Self.Change(true);
end;

function TBlkDisconnector.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := ((portType = TRCSIOType.output) and (Self.RozpSettings.RCSAddrs.Contains(addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Update();
begin
  if ((Self.state <> TBlkDiscBasicState.disabled) and (not RCSi.IsNonFailedModule(Self.RozpSettings.RCSAddrs[0].board)))
  then
  begin
    Self.state := TBlkDiscBasicState.disabled;
    Self.RozpStav.rcsFailed := true;
  end;

  case (Self.state) of
    TBlkDiscBasicState.disabled:
      begin
        if ((Self.RozpStav.rcsFailed) and (RCSi.IsNonFailedModule(Self.RozpSettings.RCSAddrs[0].board))) then
        begin
          Self.RozpStav.rcsFailed := false;
          Self.state := TBlkDiscBasicState.not_selected;
        end;
      end;
    TBlkDiscBasicState.mounting:
      begin
        if (Now > Self.RozpStav.finish) then
        begin
          Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
          Self.state := TBlkDiscBasicState.active;
        end;
      end;
    TBlkDiscBasicState.active:
      if (Now > Self.RozpStav.finish) then
        Self.state := TBlkDiscBasicState.not_selected;
  end; // case

  inherited Update();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.GetSettings(): TBlkDiscSettings;
begin
  Result := Self.RozpSettings;
end;

procedure TBlkDisconnector.SetSettings(data: TBlkDiscSettings);
begin
  if (Self.RozpSettings.RCSAddrs <> data.RCSAddrs) then
    Self.RozpSettings.RCSAddrs.Free();

  Self.RozpSettings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton;
  rights: TAreaRights; params: string = '');
begin
  case (Button) of
    F2:
      PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

    ENTER:
      begin
        case (Self.state) of
          TBlkDiscBasicState.disabled:
            PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
          TBlkDiscBasicState.not_selected:
            Self.Mount();
          TBlkDiscBasicState.mounting:
            Self.Activate();
          TBlkDiscBasicState.active:
            Self.Prolong();
        end;
      end;

    ESCAPE:
      begin
        case (Self.state) of
          TBlkDiscBasicState.mounting:
            Self.state := TBlkDiscBasicState.not_selected;
          TBlkDiscBasicState.active:
            Self.state := TBlkDiscBasicState.not_selected;
        end;
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;
  if (Self.state = TBlkDiscBasicState.active) then
    Result := Result + 'AKTIV<,'
  else if (Self.state <> TBlkDiscBasicState.disabled) then
    Result := Result + 'AKTIV>,';
  Result := Result + 'STIT,';
end;

procedure TBlkDisconnector.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV>') then
    Self.MenuAktivOnClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV<') then
    Self.MenuAktivOffClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetState(status: TBlkDiscBasicState);
begin
  if (Self.state <> status) then
  begin
    Self.RozpStav.state := status;
    Self.UpdateOutput();
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.UpdateOutput();
begin
  try
    if (Self.state = TBlkDiscBasicState.active) then
      RCSi.SetOutputs(Self.RozpSettings.RCSAddrs, 1)
    else
      RCSi.SetOutputs(Self.RozpSettings.RCSAddrs, 0);
  except
    Self.RozpStav.rcsFailed := true;
    Self.state := TBlkDiscBasicState.disabled;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  bg := clBlack;
  case (Self.state) of
    TBlkDiscBasicState.disabled:
      fg := clFuchsia;
    TBlkDiscBasicState.not_selected:
      fg := $A0A0A0;
    TBlkDiscBasicState.mounting:
      fg := clYellow;
    TBlkDiscBasicState.active:
      fg := clLime;
  else
    fg := clFuchsia;
  end;

  if (Self.note <> '') then
    bg := clTeal;

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) + ';0;';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Mount();
begin
  Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._MOUNT_TO_ACTIVE_TIME_SEC, 0);
  Self.state := TBlkDiscBasicState.mounting;
end;

procedure TBlkDisconnector.Activate();
begin
  Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
  Self.state := TBlkDiscBasicState.active;
end;

procedure TBlkDisconnector.Prolong();
begin
  Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;
  TBlk.RCStoJSON(Self.RozpSettings.RCSAddrs[0], json['rcs']);
  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkDisconnector.GetPtState(json: TJsonObject);
begin
  case (Self.state) of
    TBlkDiscBasicState.disabled:
      json['state'] := 'off';
    TBlkDiscBasicState.not_selected:
      json['state'] := 'notSelected';
    TBlkDiscBasicState.mounting:
      json['state'] := 'mounting';
    TBlkDiscBasicState.active:
      json['state'] := 'active';
  end;
end;

procedure TBlkDisconnector.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('state')) then
  begin
    if (Self.state = TBlkDiscBasicState.disabled) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze nastavit neaktivni rozpojovac');
      inherited;
      Exit();
    end;

    if (reqJson.S['state'] = 'mounting') then
      Self.Mount()
    else if (reqJson.S['state'] = 'active') then
      Self.Activate()
    else if (reqJson.S['state'] = 'notSelected') then
      Self.state := TBlkDiscBasicState.not_selected;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetNote(note: string);
begin
  Self.RozpStav.note := note;
  Self.Change();
end;

procedure TBlkDisconnector.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.fullState.note);
end;

procedure TBlkDisconnector.MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.Activate();
end;

procedure TBlkDisconnector.MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.state := TBlkDiscBasicState.not_selected;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
