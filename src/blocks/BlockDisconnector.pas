unit BlockDisconnector;

{ DISCONNECTOR technological block definition }

interface

uses IniFiles, Block, Classes, AreaDb, SysUtils, JsonDataObjects,
  IdContext, Area, RCSc, RCSIFace, Generics.Collections;

type
  TBlkDiscBasicState = (
    disabled = -5,
    inactive = 0,
    selected = 1, // only in dmSpace mode
    active = 2,
    shortTimeRemaining = 3,
    activeInfinite = 4
  );

  TBlkDiscMode = (
    dmAuto = 0,
    dmSpace = 1
  );

  TBlkDiscSettings = record
    RCSAddrs: TRCSAddrs; // only 1 address
    outputType: TRCSOutputState;
    dscMode: TBlkDiscMode;
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
    _ACTIVE_TO_DISABLE_TIME_SEC = 60;
    _WARNING_TIME_SEC = 15;

  private
    m_settings: TBlkDiscSettings;
    m_state: TBlkDiscState;

    procedure SetState(state: TBlkDiscBasicState);
    procedure UpdateOutput();

    procedure SetNote(note: string);

    procedure Prolong();

    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights);
    procedure MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure MenuAdminRadOnClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminRadOffClick(SenderPnl: TIDContext; SenderOR: TObject);

    procedure ActivateWithPossibleUPO(SenderPnl: TIdContext; SenderOR: TObject);
    procedure UPOActivDone(Sender: TObject);
    procedure NoteUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
      UPO_EscCallback: TNotifyEvent);

    function IsActive(): Boolean;
    function IsSelected(): Boolean;

    procedure PstCheckActive();
    procedure ReadControllers();

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

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
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights); override;
    procedure PanelKey(senderPnl: TIdContext; key: string; state: Boolean); override;

    function IsActiveByController(): Boolean;

    procedure PstAdd(pst: TBlk);
    procedure PstRemove(pst: TBlk);
    function PstIsActive(): Boolean;
    function PstIs(): Boolean;
    function ControllerInBasicPosition(): Boolean;

    property fullState: TBlkDiscState read m_state;
    property state: TBlkDiscBasicState read m_state.state write SetState;
    property note: string read m_state.note write SetNote;
    property active: Boolean read IsActive;
    property selected: Boolean read IsSelected;
    property mode: TBlkDiscMode read m_settings.dscMode;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TCPServerPanel, ownConvert, Graphics, PTUtils, IfThenElse, BlockPst,
    RCSErrors, UPO, timeHelper, colorHelper, PanelConnData;

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
  Self.m_settings.outputType := TRCSOutputState(ini_tech.ReadInteger(section, 'outputType', 1));
  Self.m_settings.dscMode := TBlkDiscMode(ini_tech.ReadInteger(section, 'mode', 0));

  Self.m_settings.rcsController.enabled := (ini_tech.ReadString(section, 'contRcsAddr', '') <> '');
  if (Self.m_settings.rcsController.enabled) then
   begin
    Self.m_settings.rcsController.addr.Load(ini_tech.ReadString(section, 'contRcsAddr', '0:0'));
    Self.m_settings.rcsController.pstOnly := ini_tech.ReadBool(section, 'contPstOnly', false);
    Self.RCSRegister(Self.m_settings.rcsController.addr);
   end;

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.RCSRegister(Self.m_settings.RCSAddrs);
end;

procedure TBlkDisconnector.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);

  if (Self.m_settings.outputType <> osEnabled) then
    ini_tech.WriteInteger(section, 'outputType', Integer(Self.m_settings.outputType));

  ini_tech.WriteInteger(section, 'mode', Integer(Self.m_settings.dscMode));

  if (Self.m_settings.rcsController.enabled) then
   begin
    ini_tech.WriteString(section, 'contRcsAddr', Self.m_settings.rcsController.addr.ToString());
    ini_tech.WriteBool(section, 'contPstOnly', Self.m_settings.rcsController.pstOnly);
   end;
end;

procedure TBlkDisconnector.SaveState(ini_stat: TMemIniFile; const section: string);
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
      if (not RCSi.IsNonFailedModule(rcsaddr.module)) then
        enable := false;
  except
    enable := false;
  end;

  Self.m_state.rcsFailed := not Enable;

  if (enable) then
    Self.state := TBlkDiscBasicState.inactive;

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
  if ((portType = TRCSIOType.input) and (Self.m_settings.rcsController.enabled) and (addr = Self.m_settings.rcsController.addr)) then
    Exit(true);

  Result := ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Update();
begin
  if ((Self.state <> TBlkDiscBasicState.disabled) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].module)))
  then
  begin
    Self.state := TBlkDiscBasicState.disabled;
    Self.m_state.rcsFailed := true;
  end;

  case (Self.state) of
    TBlkDiscBasicState.disabled:
      begin
        if ((Self.m_state.rcsFailed) and (RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].module))) then
        begin
          Self.m_state.rcsFailed := false;
          Self.state := TBlkDiscBasicState.inactive;
        end;
      end;
    TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining:
      begin
        if (Now > Self.m_state.finish) then
          Self.state := TBlkDiscBasicState.inactive
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
          TBlkDiscBasicState.inactive:
            if ((IsWritable(rights)) and (not Self.PstIsActive())) then
              Self.ActivateWithPossibleUPO(SenderPnl, SenderOR)
            else
              PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
          TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining:
            if ((IsWritable(rights)) and (not Self.PstIsActive())) then
              Self.Prolong()
            else
              PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
        end;
      end;

    ESCAPE:
      begin
        case (Self.state) of
          TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining, TBlkDiscBasicState.activeInfinite, TBlkDiscBasicState.selected:
            if ((IsWritable(rights)) and (not Self.IsActiveByController())) then
              Self.state := TBlkDiscBasicState.inactive;
        end;
      end;
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;
  if ((IsWritable(rights)) and (not Self.IsActiveByController())) then
  begin
    if ((Self.active) or (Self.selected)) then
      Result := Result + 'AKTIV<,'
    else if ((Self.state <> TBlkDiscBasicState.disabled) and (not Self.PstIsActive())) then
      Result := Result + 'AKTIV>,';
  end;

  if ((IsWritable(rights)) or (Self.note <> '')) then
    Result := Result + 'STIT,';

  if ((IsWritable(rights)) and (RCSi.simulation) and (Self.m_settings.rcsController.enabled)) then
  begin
    try
      if (RCSi.GetInput(Self.m_settings.rcsController.addr) = isOn) then
        Result := Result + '-,*RAD<,'
      else
        Result := Result + '-,*RAD>,';
    except
      on E: RCSException do begin end;
      on E: Exception do raise;
    end;
  end;
end;

procedure TBlkDisconnector.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights);
begin
  if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR, rights)
  else if (item = 'RAD>') then
    Self.MenuAdminRadOnClick(SenderPnl, SenderOR)
  else if (item = 'RAD<') then
    Self.MenuAdminRadOffClick(SenderPnl, SenderOR);

  if (Self.IsActiveByController()) then
    Exit();

  if (item = 'AKTIV>') then
    Self.MenuAktivOnClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV<') then
    Self.MenuAktivOffClick(SenderPnl, SenderOR);
end;

procedure TBlkDisconnector.PanelKey(senderPnl: TIdContext; key: string; state: Boolean);
begin
  if ((key = 'space') and (Self.state = TBlkDiscBasicState.selected) and (state)) then
  begin
    Self.state := TBlkDiscBasicState.activeInfinite;
  end else if ((key = 'space') and (Self.active) and (not state)) then
  begin
    Self.state := TBlkDiscBasicState.selected;
  end else if ((key = '') and ((Self.active) or (Self.selected)) and (not state)) then
  begin
    Self.state := TBlkDiscBasicState.inactive;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetState(state: TBlkDiscBasicState);
begin
  if (Self.state = state) then
    Exit();

  Self.m_state.state := state;

  if (state = TBlkDiscBasicState.active) then
  begin
    Self.m_state.finish := Now + EncodeTimeSec(Self._ACTIVE_TO_DISABLE_TIME_SEC);
    Self.m_state.warning := Now + EncodeTimeSec(Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC);
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
      fg := TJopColor.purple;
    TBlkDiscBasicState.inactive:
      fg := TJopColor.grayDark;
    TBlkDiscBasicState.active, TBlkDiscBasicState.shortTimeRemaining, TBlkDiscBasicState.activeInfinite, TBlkDiscBasicState.selected:
      fg := TJopColor.green;
  else
    fg := TJopColor.purple;
  end;

  if ((fg = TJopColor.grayDark) and (Self.PstIs())) then
    fg := TJopColor.blue;

  if (Self.note <> '') then
    bg := TJopColor.turqDark;

  flicker := (Self.state = TBlkDiscBasicState.shortTimeRemaining);

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) +
            ';' + IntToStr(BoolToInt(flicker)) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.Prolong();
begin
  Self.m_state.finish := Now + EncodeTimeSec(Self._ACTIVE_TO_DISABLE_TIME_SEC);
  Self.m_state.warning := Now + EncodeTimeSec(Self._ACTIVE_TO_DISABLE_TIME_SEC-Self._WARNING_TIME_SEC);
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
    TBlkDiscBasicState.inactive:
      json['state'] := 'inactive';
    TBlkDiscBasicState.active:
      json['state'] := 'active';
    TBlkDiscBasicState.selected:
      json['state'] := 'selected';
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

    if (reqJson.S['state'] = 'active') then
      Self.state := TBlkDiscBasicState.active
    else if (reqJson.S['state'] = 'activeInfinite') then
      Self.state := TBlkDiscBasicState.activeInfinite
    else if (reqJson.S['state'] = 'inactive') then
      Self.state := TBlkDiscBasicState.inactive;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkDisconnector.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights);
begin
  PanelServer.Note(SenderPnl, Self, Self.fullState.note, rights);
end;

procedure TBlkDisconnector.MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.ActivateWithPossibleUPO(SenderPnl, SenderOR);
end;

procedure TBlkDisconnector.MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.state := TBlkDiscBasicState.inactive;
end;

procedure TBlkDisconnector.MenuAdminRadOnClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.rcsController.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.rcsController.addr, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupù!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkDisconnector.MenuAdminRadOffClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.rcsController.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.rcsController.addr, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupù!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkDisconnector.IsActive(): Boolean;
begin
  Result := ((Self.state = TBlkDiscBasicState.active) or (Self.state = TBlkDiscBasicState.shortTimeRemaining)
          or (Self.state = TBlkDiscBasicState.activeInfinite));
end;

function TBlkDisconnector.IsSelected(): Boolean;
begin
  Result := (Self.state = TBlkDiscBasicState.selected);
end;

function TBlkDisconnector.IsActiveByController(): Boolean;
begin
  if (not Self.m_settings.rcsController.enabled) then
    Exit(false);
  if (not Self.PstIs()) or (not Self.PstIsActive()) then
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
    if (Self.m_settings.rcsController.pstOnly) then
      Self.state := TBlkDiscBasicState.inactive;
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

function TBlkDisconnector.ControllerInBasicPosition(): Boolean;
begin
  if (not Self.m_settings.rcsController.enabled) then
    Exit(true);

  try
    Result := (RCSi.GetInput(Self.m_settings.rcsController.addr) <> isOn);
  except
    Result := false;
  end;
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
    if ((state = TRCSInputState.isOff) and (Self.state = TBlkDiscBasicState.activeInfinite)) then
      Self.state := TBlkDiscBasicState.inactive;
  except
    on E: RCSException do Exit();
    on E: Exception do raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkDisconnector.ActivateWithPossibleUPO(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOActivDone, nil)
  else
    Self.UPOActivDone(SenderPnl);
end;

procedure TBlkDisconnector.UPOActivDone(Sender: TObject);
begin
  const senderPnl = TIdContext(Sender);

  if (Self.mode = TBlkDiscMode.dmAuto) then
  begin
    Self.state := TBlkDiscBasicState.active;
  end else begin
    var pkeyBlock: TBlk := TPanelConnData(senderPnl.Data).pkey_block;
    if ((pkeyBlock <> nil) and (pkeyBlock.typ = TBlkType.btDisconnector)) then
      TBlkDisconnector(pkeyBlock).state := TBlkDiscBasicState.inactive;
    Self.state := TBlkDiscBasicState.selected;
    TPanelConnData(senderPnl.Data).pkey_block := Self;
  end;
end;

procedure TBlkDisconnector.NoteUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
  UPO_EscCallback: TNotifyEvent);
var upos: TUPOItems;
begin
  upos := TList<TUPOItem>.Create();
  try
    upos.Add(UPO.NoteUPO(Self.name, Self.note));
    PanelServer.UPO(SenderPnl, upos, false, UPO_OKCallback, UPO_EscCallback, SenderPnl);
  finally
    upos.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
