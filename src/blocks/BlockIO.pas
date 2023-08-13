unit BlockIO;

{
  IO technological block definition.

  IO block represents block with binary state which could be propagated to any
  RCS output. It can also display state of any (other) RCS input. This block is
  usually shown as "dot" in the panel.
}

interface

uses IniFiles, Block, TechnologieRCS, Classes, SysUtils, IdContext, Area,
  Graphics, JsonDataObjects, RCSErrors, RCS;

type

  TBlkIOsettings = record
    isRCSinput: Boolean;
    RCSinputNeeded: Boolean;
    RCSinput: TRCSAddr;

    isRCSOutput: Boolean;
    RCSoutputNeeded: Boolean;
    RCSoutput: TRCSAddr;
    allowOutChange: Boolean;

    setOutputOnStart: Boolean;
    nullAfterSec: Integer;
  end;

  TBlkIOstate = record
    enabled: Boolean;
    inputState: TRCSInputState;
    outputState: TRCSOutputState;
    nullTime: TTime;
    note: string;
  end;

  TBlkIO = class(TBlk)
  const
    _def_io_state: TBlkIOstate = (
      enabled: false;
      inputState: TRCSInputState.isOff;
      outputState: TRCSOutputState.osDisabled;
      nullTime: 0;
    );

  private
    m_settings: TBlkIOsettings;
    m_state: TBlkIOstate;

    function IsNullable(): Boolean;
    function IsActiveInput(): Boolean;
    function IsActiveOutput(): Boolean;

    procedure SetNote(note: string);

    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuInClick(SenderPnl: TIdContext; SenderOR: TObject; target: Boolean);

  public
    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Activate();
    procedure Deactivate();

    procedure Update(); override;

    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    // ----- IO own functions -----

    function GetSettings(): TBlkIOsettings;
    procedure SetSettings(data: TBlkIOsettings);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

    property isRCSOutput: Boolean read m_settings.isRCSOutput;
    property isRCSinput: Boolean read m_settings.isRCSinput;
    property RCSoutputNeeded: Boolean read m_settings.RCSoutputNeeded;
    property RCSinputNeeded: Boolean read m_settings.RCSinputNeeded;
    property enabled: Boolean read m_state.enabled;
    property allowOutChange: Boolean read m_settings.allowOutChange;
    property activeOutput: Boolean read IsActiveOutput;
    property activeInput: Boolean read IsActiveInput;
    property nullable: Boolean read IsNullable;
    property note: string read m_state.note write SetNote;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses AreaDb, TCPServerPanel, ownConvert, Config, timeHelper, colorHelper;

constructor TBlkIO.Create(index: Integer);
begin
  inherited;
  Self.m_state := _def_io_state;
  Self.m_globSettings.typ := btIO;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.isRCSOutput := false;
  Self.m_settings.isRCSinput := false;

  Self.m_settings.isRCSOutput := (ini_tech.ReadString(section, 'RCSout', '') <> '') or (ini_tech.ReadInteger(section, 'RCSb0', -1) <> -1);
  Self.m_settings.allowOutChange := ini_tech.ReadBool(section, 'allowOutChange', Self.m_settings.isRCSOutput);
  if (Self.m_settings.isRCSOutput) then
  begin
    Self.m_settings.RCSoutput := RCSFromIni(ini_tech, section, 'RCSout', 'RCSb0', 'RCSp0');
    Self.m_settings.RCSoutputNeeded := ini_tech.ReadBool(section, 'RCSno', true) or ini_tech.ReadBool(section, 'RCSn0', true);
  end;

  Self.m_settings.isRCSinput := (ini_tech.ReadString(section, 'RCSin', '') <> '') or (ini_tech.ReadInteger(section, 'RCSbi', -1) <> -1); // old format
  if (Self.m_settings.isRCSinput) then
  begin
    Self.m_settings.RCSinput := RCSFromIni(ini_tech, section, 'RCSin', 'RCSbi', 'RCSpi');
    Self.m_settings.RCSinputNeeded := ini_tech.ReadBool(section, 'RCSni', true);
  end;

  Self.LoadAreas(ini_rel, 'POM').Free();
  Self.m_settings.setOutputOnStart := ini_tech.ReadBool(section, 'activateOnStart', false);
  Self.m_settings.nullAfterSec := ini_tech.ReadInteger(section, 'nullTime', 0);
  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');

  if (Self.isRCSinput) then
  begin
    Self.RCSRegister(Self.m_settings.RCSinput);
    RCSi.SetNeeded(Self.m_settings.RCSinput.board);
  end;
  if (Self.isRCSOutput) then
  begin
    Self.RCSRegister(Self.m_settings.RCSoutput);
    RCSi.SetNeeded(Self.m_settings.RCSoutput.board);
  end;
end;

procedure TBlkIO.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  if (Self.isRCSOutput) then
  begin
    ini_tech.WriteString(section, 'RCSout', Self.m_settings.RCSoutput.ToString());
    ini_tech.WriteBool(section, 'RCSno', Self.m_settings.RCSoutputNeeded);
  end;
  if ((Self.isRCSOutput <> Self.allowOutChange)) then
     ini_tech.WriteBool(section, 'allowOutChange', Self.m_settings.allowOutChange);

  if (Self.isRCSinput) then
  begin
    ini_tech.WriteString(section, 'RCSin', Self.m_settings.RCSinput.ToString());
    ini_tech.WriteBool(section, 'RCSni', Self.m_settings.RCSinputNeeded);
  end;

  if (Self.m_settings.setOutputOnStart) then
    ini_tech.WriteBool(section, 'activateOnStart', true);
  if (Self.nullable) then
    ini_tech.WriteInteger(section, 'nullTime', Self.m_settings.nullAfterSec);
end;

procedure TBlkIO.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Enable();
begin
  if ((Self.isRCSOutput) and (Self.RCSoutputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board)))
  then
    Exit();
  if ((Self.isRCSinput) and (Self.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))) then
    Exit();

  if (Self.isRCSinput) then
    Self.m_state.inputState := TRCSInputState.notYetScanned;

  Self.m_state.enabled := true;
  if (Self.m_settings.setOutputOnStart) then
    Self.Activate();
end;

procedure TBlkIO.Disable();
begin
  Self.m_state.enabled := false;
  Self.m_state.outputState := TRCSOutputState.osDisabled;
  Self.m_state.inputState := TRCSInputState.isOff;
end;

function TBlkIO.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := (((Self.isRCSOutput) and (portType = TRCSIOType.output) and (Self.m_settings.RCSoutput = addr)) or
    ((Self.isRCSinput) and (portType = TRCSIOType.input) and (Self.m_settings.RCSinput = addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkIO.GetSettings(): TBlkIOsettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkIO.SetSettings(data: TBlkIOsettings);
begin
  Self.m_settings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Update();
var inputState: TRCSInputState;
    outputState: TRCSOutputState;
begin
  inherited;

  if ((not Self.enabled) and (((not Self.isRCSOutput) or (RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board))) and
    ((not Self.isRCSinput) or (RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))))) then
  begin
    Self.Enable();
    Self.Change();
  end;
  if ((Self.enabled) and (((Self.isRCSOutput) and (Self.m_settings.RCSoutputNeeded) and
    (not RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board))) or ((Self.isRCSinput) and
    (Self.m_settings.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))))) then
  begin
    Self.Disable();
    Self.Change(true);
  end;

  if ((Self.enabled) and (Self.isRCSinput)) then
  begin
    try
      inputState := RCSi.GetInput(Self.m_settings.RCSinput);
    except
      on E: RCSException do
        inputState := TRCSInputState.failure;
    end;

    if (inputState <> Self.m_state.inputState) then
    begin
      Self.m_state.inputState := inputState;
      Self.Change();
    end;
  end;

  if ((Self.enabled) and (Self.isRCSOutput)) then
  begin
    try
      outputState := RCSi.GetOutputState(Self.m_settings.RCSoutput);
    except
      on E: RCSException do
        outputState := TRCSOutputState.osFailure;
    end;

    if (outputState <> Self.m_state.outputState) then
    begin
      Self.m_state.outputState := outputState;
      Self.Change();
    end;
  end;

  if ((Self.enabled) and (Self.nullable) and (Self.activeOutput) and (Now > Self.m_state.nullTime)) then
    Self.Deactivate();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Activate();
begin
  if (Self.activeOutput) then
    Exit();

  if (Self.isRCSOutput) then
  begin
    try
      RCSi.SetOutput(Self.m_settings.RCSoutput, 1);
    except

    end;
  end else if (Self.allowOutChange) then
  begin
    Self.m_state.outputState := TRCSOutputState.osEnabled;
    Self.Change();
  end;

  if (Self.nullable) then
    Self.m_state.nullTime := Now + EncodeTimeSec(Self.m_settings.nullAfterSec);

  Self.Update();
end;

procedure TBlkIO.Deactivate();
begin
  if (not Self.activeOutput) then
    Exit();

  if (Self.isRCSOutput) then
  begin
    try
      RCSi.SetOutput(Self.m_settings.RCSoutput, 0);
    except

    end;
  end else if (Self.allowOutChange) then
  begin
    Self.m_state.outputState := TRCSOutputState.osDisabled;
    Self.Change();
  end;

  Self.Update();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (Button = TPanelButton.F2) then
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  if (Button = TPanelButton.ENTER) then
  begin
    if ((Self.enabled) and (Self.allowOutChange)) then
    begin
      try
        Self.Activate();
      except
        PanelServer.BottomError(SenderPnl, 'Nepodaøilo se aktivovat blok', TArea(SenderOR).shortName, 'TECHNOLOGIE');
      end
    end
    else
      PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end else if (Button = TPanelButton.ESCAPE) then
  begin
    if (Self.enabled) then
    begin
      try
        Self.Deactivate();
      except
        PanelServer.BottomError(SenderPnl, 'Nepodaøilo se deaktivovat blok', TArea(SenderOR).shortName, 'TECHNOLOGIE');
      end
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkIO.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  bg := TJopColor.black;
  if (Self.note <> '') then
    bg := TJopColor.turqDark;

  if (not Self.enabled) then
    fg := TJopColor.purple
  else if (Self.activeOutput) then
    fg := TJopColor.yellow
  else if (Self.isRCSinput) then
  begin
    case (Self.m_state.inputState) of
      TRCSInputState.isOff:
        fg := TJopColor.grayDark;
      TRCSInputState.isOn:
        fg := TJopColor.green;
    else
      fg := TJopColor.purple;
    end;
  end
  else
    fg := TJopColor.grayDark;

  Result := Result + ownConvert.ColorToStr(fg) + ';';
  Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  if (Self.isRCSOutput) then
    TBlk.RCStoJSON(Self.m_settings.RCSoutput, json['rcs']['output']);
  if (Self.isRCSinput) then
    TBlk.RCStoJSON(Self.m_settings.RCSinput, json['rcs']['input']);

  json['setOutputOnStart'] := Self.m_settings.setOutputOnStart;
  if (includeState) then
    Self.GetPtState(json['blockState']);
  json['nullable'] := Self.nullable;
  if (Self.nullable) then
    json['nullTime'] := Self.m_settings.nullAfterSec;
  json['allowOutChange'] := Self.allowOutChange;
end;

procedure TBlkIO.GetPtState(json: TJsonObject);
begin
  json['enabled'] := Self.m_state.enabled;
  json['activeOutput'] := Self.activeOutput;
  json['activeInput'] := Self.activeInput;
end;

procedure TBlkIO.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (not Self.enabled) then
    Exit();

  if ((reqJson.Contains('activeOutput')) and (Self.allowOutChange)) then
  begin
    if ((reqJson.B['activeOutput']) and (not Self.activeOutput)) then
      Self.Activate()
    else if ((not reqJson.B['activeOutput']) and (Self.activeOutput)) then
      Self.Deactivate();
  end;

  if (reqJson.Contains('activeInput') and (not Self.isRCSinput)) then
  begin
    if ((reqJson.B['activeInput']) and (not Self.activeInput)) then
    begin
      Self.m_state.inputState := TRCSInputState.isOn;
      Self.Change();
    end else if ((not reqJson.B['activeInput']) and (Self.activeInput)) then
    begin
      Self.m_state.inputState := TRCSInputState.isOff;
      Self.Change();
    end;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkIO.IsNullable(): Boolean;
begin
  Result := (Self.m_settings.nullAfterSec > 0);
end;

function TBlkIO.IsActiveInput(): Boolean;
begin
  Result := (Self.m_state.inputState = TRCSInputState.isOn);
end;

function TBlkIO.IsActiveOutput(): Boolean;
begin
  Result := (Self.m_state.outputState >= TRCSOutputState.osEnabled) and (Self.m_state.outputState <= TRCSOutputState.osf66);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkIO.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;
  if ((Self.enabled) and (Self.allowOutChange)) then
  begin
    if (Self.activeOutput) then
      Result := Result + 'AKTIV<,'
    else
      Result := Result + 'AKTIV>,';
  end;
  Result := Result + 'STIT,';

  if ((RCSi.simulation) and (Self.isRCSinput)) then
  begin
    if (Self.IsActiveInput) then
      Result := Result + '-,*IN<,'
    else
      Result := Result + '-,*IN>,'
  end;
end;

procedure TBlkIO.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV>') then
    Self.MenuAktivOnClick(SenderPnl, SenderOR)
  else if (item = 'AKTIV<') then
    Self.MenuAktivOffClick(SenderPnl, SenderOR)
  else if (item = 'IN<') then
    Self.MenuInClick(SenderPnl, SenderOR, false)
  else if (item = 'IN>') then
    Self.MenuInClick(SenderPnl, SenderOR, true);
end;

procedure TBlkIO.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkIO.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.note);
end;

procedure TBlkIO.MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.Activate();
end;

procedure TBlkIO.MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.Deactivate();
end;

procedure TBlkIO.MenuInClick(SenderPnl: TIdContext; SenderOR: TObject; target: Boolean);
begin
  try
    RCSi.SetInput(Self.m_settings.RCSinput, ownConvert.BoolToInt(target));
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupù!', TArea(SenderOR).shortName,
      'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
