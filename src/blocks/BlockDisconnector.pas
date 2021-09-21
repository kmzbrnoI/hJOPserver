unit BlockDisconnector;

{ DISCONNECTOR technological block definition }

interface

uses IniFiles, Block, Classes, AreaDb, SysUtils, JsonDataObjects,
  IdContext, Area, TechnologieRCS, RCS, Generics.Collections;

type
  TBlkDiscBasicState = (
    disabled = -5,
    notSelected = 0,
    mounting = 1,
    active = 2,
    shortTimeRemaining = 3,
    activeInfinite = 4
  );

  TBlkDiscSettings = record
    RCSAddrs: TRCSAddrs; // only 1 address
    outputType: TRCSOutputState;
    rcsController: record
      enabled: Boolean;
      addr: TRCSAddr;
      pstOnly: Boolean;
    end;
  end;

  TBlkDiscState = record
    state: TBlkDiscBasicState;
    finish: TDateTime;
    warning: TDateTime;
    rcsFailed: Boolean;
    note: string;
    psts: TList<TBlk>;
  end;

  TBlkDisconnector = class(TBlk)
  const
    _def_disc_stav: TBlkDiscState = (state: disabled; rcsFailed: false; note: '';);

  private const
    _MOUNT_TO_ACTIVE_TIME_SEC = 3;
    _ACTIVE_TO_DISABLE_TIME_SEC = 60;
    _WARNING_TIME_SEC = 15;

  private
    m_settings: TBlkDiscSettings;
    m_state: TBlkDiscState;

    procedure SetState(state: TBlkDiscBasicState);
    procedure UpdateOutput();

    procedure SetNote(note: string);

    procedure Prolong();

    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);

    function IsActive(): Boolean;
    function IsActiveByController(): Boolean;

    procedure PstCheckActive();
    procedure ReadControllers();

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

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

    procedure PstAdd(pst: TBlk);
    procedure PstRemove(pst: TBlk);
    function PstIsActive(): Boolean;
    function PstIs(): Boolean;

    property fullState: TBlkDiscState read m_state;
    property state: TBlkDiscBasicState read m_state.state write SetState;
    property note: string read m_state.note write SetNote;
    property active: Boolean read IsActive;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TCPServerPanel, ownConvert, Graphics, PTUtils, IfThenElse, BlockPst, RCSErrors;

constructor TBlkDisconnector.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btDisconnector;
  Self.m_state := Self._def_disc_stav;
  Self.m_state.psts := TList<TBlk>.Create();
end;

destructor TBlkDisconnector.Destroy();
begin
  Self.m_state.psts.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);
  Self.LoadAreas(ini_rel, 'R').Free();
  Self.PushRCSToAreas(Self.m_settings.RCSAddrs);
  Self.m_settings.outputType := TRCSOutputState(ini_tech.ReadInteger(section, 'outputType', 1));

  Self.m_settings.rcsController.enabled := (ini_tech.ReadString(section, 'contRcsAddr', '') <> '');
  if (Self.m_settings.rcsController.enabled) then
   begin
    Self.m_settings.rcsController.addr.Load(ini_tech.ReadString(section, 'contRcsAddr', '0:0'));
    Self.m_settings.rcsController.pstOnly := ini_tech.ReadBool(section, 'contPstOnly', false);
    Self.PushRCSToAreas(Self.m_settings.rcsController.addr);
   end;

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.PushRCSToAreas(Self.m_settings.RCSAddrs);
end;

procedure TBlkDisconnector.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);

  if (Self.m_settings.outputType <> osEnabled) then
    ini_tech.WriteInteger(section, 'outputType', Integer(Self.m_settings.outputType));

  if (Self.m_settings.rcsController.enabled) then
   begin
    ini_tech.WriteString(section, 'contRcsAddr', Self.m_settings.rcsController.addr.ToString());
    ini_tech.WriteBool(section, 'contPstOnly', Self.m_settings.rcsController.pstOnly);
   end;
end;

procedure TBlkDisconnector.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Enable();
var enable: Boolean;
begin
  enable := true;
  try
    for var rcsaddr in Self.m_settings.RCSAddrs do
      if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
        enable := false;
  except
    enable := false;
  end;

  Self.m_state.rcsFailed := not Enable;

  if (enable) then
    Self.state := TBlkDiscBasicState.notSelected;

  Self.m_state.psts.Clear();
end;

procedure TBlkDisconnector.Disable();
begin
  Self.state := TBlkDiscBasicState.disabled;
  Self.m_state.rcsFailed := false;
  Self.m_state.psts.Clear();
  Self.Change(true);
end;

function TBlkDisconnector.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Update();
begin
  if ((Self.state <> TBlkDiscBasicState.disabled) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].board)))
  then
  begin
    Self.state := TBlkDiscBasicState.disabled;
    Self.m_state.rcsFailed := true;
  end;

  case (Self.state) of
    TBlkDiscBasicState.disabled:
      begin
        if ((Self.m_state.rcsFailed) and (RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].board))) then
        begin
          Self.m_state.rcsFailed := false;
          Self.state := TBlkDiscBasicState.notSelected;
        end;
      end;
    TBlkDiscBasicState.mounting:
      begin
        if (Now > Self.m_state.finish) then
          Self.state := TBlkDiscBasicState.active;
      end;
    TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining:
      begin
        if (Now > Self.m_state.finish) then
          Self.state := TBlkDiscBasicState.notSelected
        else if ((Now > Self.m_state.warning) and (Self.state = TBlkDiscBasicState.active)) then
          Self.state := TBlkDiscBasicState.shortTimeRemaining;
      end;
  end; // case

  Self.ReadControllers();
  inherited Update();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.GetSettings(): TBlkDiscSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkDisconnector.SetSettings(data: TBlkDiscSettings);
begin
  if (Self.m_settings.RCSAddrs <> data.RCSAddrs) then
    Self.m_settings.RCSAddrs.Free();

  Self.m_settings := data;
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
          TBlkDiscBasicState.disabled, TBlkDiscBasicState.activeInfinite:
            PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
          TBlkDiscBasicState.notSelected:
            Self.state := TBlkDiscBasicState.mounting;
          TBlkDiscBasicState.mounting:
            Self.state := TBlkDiscBasicState.active;
          TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining:
            Self.Prolong();
        end;
      end;

    ESCAPE:
      begin
        case (Self.state) of
          TBlkDiscBasicState.mounting, TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining,
          TBlkDiscBasicState.activeInfinite:
            if (not Self.IsActiveByController()) then
              Self.state := TBlkDiscBasicState.notSelected;
        end;
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;
  if (not Self.IsActiveByController()) then
  begin
    if (Self.active) then
      Result := Result + 'AKTIV<,'
    else if (Self.state <> TBlkDiscBasicState.disabled) then
      Result := Result + 'AKTIV>,';
  end;
  Result := Result + 'STIT,';
end;

procedure TBlkDisconnector.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR);

  if (Self.IsActiveByController()) then
    Exit();

  if (item = 'AKTIV>') then
    Self.MenuAktivOnClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV<') then
    Self.MenuAktivOffClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetState(state: TBlkDiscBasicState);
begin
  if (Self.state = state) then
    Exit();

  Self.m_state.state := state;

  if (state = TBlkDiscBasicState.mounting) then
  begin
    Self.m_state.finish := Now + EncodeTime(0, 0, Self._MOUNT_TO_ACTIVE_TIME_SEC, 0);

  end else if (state = TBlkDiscBasicState.active) then
  begin
    Self.m_state.finish := Now + EncodeTime(0, Self._ACTIVE_TO_DISABLE_TIME_SEC div 60,
                                             Self._ACTIVE_TO_DISABLE_TIME_SEC mod 60, 0);
    Self.m_state.warning := Now + EncodeTime(0, (Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC) div 60,
                                             (Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC) mod 60, 0);
  end;

  Self.UpdateOutput();
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.UpdateOutput();
begin
  try
    if (Self.active) then
      RCSi.SetOutputs(Self.m_settings.RCSAddrs, Self.m_settings.outputType)
    else
      RCSi.SetOutputs(Self.m_settings.RCSAddrs, 0);
  except
    Self.m_state.rcsFailed := true;
    Self.state := TBlkDiscBasicState.disabled;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.PanelStateString(): string;
var fg, bg: TColor;
    flicker: Boolean;
begin
  Result := inherited;

  bg := clBlack;
  case (Self.state) of
    TBlkDiscBasicState.disabled:
      fg := clFuchsia;
    TBlkDiscBasicState.notSelected:
      fg := $A0A0A0;
    TBlkDiscBasicState.mounting:
      fg := clYellow;
    TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining, TBlkDiscBasicState.activeInfinite:
      fg := clLime;
  else
    fg := clFuchsia;
  end;

  if ((fg = $A0A0A0) and (Self.PstIs())) then
    fg := clBlue;

  if (Self.note <> '') then
    bg := clTeal;

  flicker := (Self.state = TBlkDiscBasicState.shortTimeRemaining);

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) +
            ';' + IntToStr(BoolToInt(flicker)) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Prolong();
begin
  Self.m_state.finish := Now + EncodeTime(0, Self._ACTIVE_TO_DISABLE_TIME_SEC div 60,
                                           Self._ACTIVE_TO_DISABLE_TIME_SEC mod 60, 0);
  Self.m_state.warning := Now + EncodeTime(0, (Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC) div 60,
                                           (Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC) mod 60, 0);
  Self.state := TBlkDiscBasicState.active;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;
  TBlk.RCStoJSON(Self.m_settings.RCSAddrs[0], json['rcs']);
  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkDisconnector.GetPtState(json: TJsonObject);
begin
  case (Self.state) of
    TBlkDiscBasicState.disabled:
      json['state'] := 'off';
    TBlkDiscBasicState.notSelected:
      json['state'] := 'notSelected';
    TBlkDiscBasicState.mounting:
      json['state'] := 'mounting';
    TBlkDiscBasicState.active:
      json['state'] := 'active';
    TBlkDiscBasicState.shortTimeRemaining:
      json['state'] := 'shortTimeRemaining';
    TBlkDiscBasicState.activeInfinite:
      json['state'] := 'activeInfinite';
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
      Self.state := TBlkDiscBasicState.mounting
    else if (reqJson.S['state'] = 'active') then
      Self.state := TBlkDiscBasicState.active
    else if (reqJson.S['state'] = 'activeInfinite') then
      Self.state := TBlkDiscBasicState.activeInfinite
    else if (reqJson.S['state'] = 'notSelected') then
      Self.state := TBlkDiscBasicState.notSelected;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkDisconnector.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.fullState.note);
end;

procedure TBlkDisconnector.MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.state := TBlkDiscBasicState.active;
end;

procedure TBlkDisconnector.MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.state := TBlkDiscBasicState.notSelected;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.IsActive(): Boolean;
begin
  Result := ((Self.state = TBlkDiscBasicState.active) or (Self.state = TBlkDiscBasicState.shortTimeRemaining)
          or (Self.state = TBlkDiscBasicState.activeInfinite));
end;

function TBlkDisconnector.IsActiveByController(): Boolean;
begin
  if (not Self.m_settings.rcsController.enabled) then
    Exit(false);

  var controller: TRCSInputState;
  try
    controller := RCSi.GetInput(Self.m_settings.rcsController.addr);
  except
    controller := TRCSInputState.isOff;
  end;

  Result := ((Self.active) and (controller = TRCSInputState.isOn));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.PstAdd(pst: TBlk);
begin
  if (Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Add(pst);
  Self.Change();
end;

procedure TBlkDisconnector.PstRemove(pst: TBlk);
begin
  if (not Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Remove(pst);
  if (Self.m_state.psts.Count = 0) then
  begin
    if (Self.active) then
      Self.state := TBlkDiscBasicState.notSelected;
  end;
  Self.Change();
end;

function TBlkDisconnector.PstIsActive(): Boolean;
begin
  Self.PstCheckActive();
  for var blk: TBlk in Self.m_state.psts do
    if (TBlkPst(blk).status = pstActive) then
      Exit(true);
  Result := false;
end;

function TBlkDisconnector.PstIs(): Boolean;
begin
  Self.PstCheckActive();
  Result := (Self.m_state.psts.Count > 0);
end;

procedure TBlkDisconnector.PstCheckActive();
begin
  for var i := Self.m_state.psts.Count-1 downto 0 do
    if (TBlkPst(Self.m_state.psts[i]).status <= pstOff) then
      Self.PstRemove(self.m_state.psts[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.ReadControllers();
begin
  if ((not Self.m_settings.rcsController.enabled) or (not RCSi.Started)) then
    Exit();
  if ((Self.m_settings.rcsController.pstOnly) and (not Self.PstIsActive())) then
    Exit();

  try
    var state := RCSi.GetInput(Self.m_settings.rcsController.addr);
    if ((state = TRCSInputState.isOn) and (not Self.active)) then
      Self.state := TBlkDiscBasicState.activeInfinite;
    if ((state = TRCSInputState.isOff) and (Self.active)) then
      Self.state := TBlkDiscBasicState.notSelected;
  except
    on E: RCSException do Exit();
    on E: Exception do raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
