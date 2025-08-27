unit BlockCrossing;

// definice a obsluha technologickeho bloku Prejezd

interface

uses IniFiles, Block, SysUtils, Menus, AreaDb, Classes, RCSc,
  IdContext, Area, Generics.Collections, JsonDataObjects, ConfSeq,
  BlockCrossingLogic, BlockCrossingPositive;

type
  TBlkCrossingRCSInputs = record
    closed: TRCSAddrOptional; // or barriers closed
    open: TRCSAddrOptional; // or barriers open
    caution: TRCSAddrOptional;
    annulation: TRCSAddrOptional;
  end;

  TBlkCrossingRCSOutputs = record
    close: TRCSAddrOptional;
    lights: TRCSAddrOptional;
    emOpen: TRCSAddrOptional;
    positive: TRCSAddrOptional;
    positiveInvert: Boolean;
    positiveFlick: Boolean;
    barriersDown: TRCSAddrOptional;
    barriersUp: TRCSAddrOptional;
    bell: TRCSAddrOptional;
    bellActiveDown: Boolean;
  end;

  TBlkCrossingSettings = record
    RCSInputs: TBlkCrossingRCSInputs;
    RCSOutputs: TBlkCrossingRCSOutputs;
    preringTime: Cardinal; // in seconds
    closedRequired: Boolean;
  end;

  TBlkCrossingPanelState = (
    psDisabled = -5,
    psError = -1,
    psOpen = 0,
    psCaution = 1,
    psClosed = 2,
    psAnnulation = 3
  );

  TBlkCrossingBasicState = (
    disabled = -5,
    unknown = -2,
    error = -1,
    open = 0,
    caution = 1,
    closed = 2
  );

  TBlkCrossingState = record
    state: TBlkCrossingBasicState;
    annulationOld: Boolean;
    note, lockout: string;
    pcEmOpen, pcClosed: Boolean;
    // uzavreni prejezdu z pocitace (tj z technologie), prejezd muze byt uzavren taky z pultu
    zaver: Integer; // pocet bloku, ktere mi daly zaver (pokud > 0, mam zaver; jinak zaver nemam)
    cautionStart: TDateTime;
    warningTimeout: TDateTime;
    barriersClosed: Boolean;
    shs: TList<TBlk>; // seznam souctovych hlasek, kam hlasi prejezd stav
    rcsModules: TList<Cardinal>; // seznam RCS modulu, ktere vyuziva prejezd
    lastInputValid: TDateTime;
    lastWantClose: Boolean;
    lastPositive: Boolean;
  end;

  EPrjNOT = class(Exception);

  TBlkCrossing = class(TBlk)
  const
    _def_crossing_state: TBlkCrossingState = (state: disabled; note: ''; lockout: ''; pcEmOpen: false; pcClosed: false;
      zaver: 0;);

    _SECT_RCSICLOSED = 'RCSIclosed';
    _SECT_RCSIOPEN = 'RCSIopen';
    _SECT_RCSICAUTION = 'RCSIcaution';
    _SECT_RCSIANNULATION = 'RCSIannulation';
    _SECT_RCSOCLOSE = 'RCSOclose';
    _SECT_RCSOLIGHTS = 'RCSOlights';
    _SECT_RCSOEMOPEN = 'RCSOemOpen';
    _SECT_RCSOPOSITIVE = 'RCSOpositive';
    _SECT_RCSOPOSITIVE_INVERT = 'RCSOpositiveInv';
    _SECT_RCSOPOSITIVE_FLICK = 'RCSOpositiveFlick';
    _SECT_RCSOBLOCKPOSITIVE = 'RCSOblockPositive';
    _SECT_RCSOBARRIERS_DOWN = 'RCSObarriersDown';
    _SECT_RCSOBARRIERS_UP = 'RCSObarriersUp';
    _SECT_RCSOBELL = 'RCSObell';
    _SECT_RCSOBELL_ACTIVE_DOWN = 'RCSObellActiveDown';
    _SECT_PRERING_TIME = 'preringTime';
    _SECT_TRACKS = 'tracks';
    _SECT_POSITIVE = 'positive'; // actually positiveX, where X \in [0..n-1] (multiple lines)
    _SECT_CLOSED_REQUIRED = 'closedRequired';

    _UZ_UPOZ_MIN = 4; // po 4 minutach uzavreneho prejezdu zobrazim upozorneni na uzavreni prilis dlouho
    _INVALID_INPUT_TOLER_SEC = 1;

  private
    m_settings: TBlkCrossingSettings;
    m_state: TBlkCrossingState;

    procedure SetNote(note: string);
    procedure SetLockout(lockout: string);

    procedure UpdateOutputs();
    procedure UpdateTracks();
    procedure UpdatePositive();
    function UpdateState(): TBlkCrossingBasicState;

    procedure SetEmOpen(state: Boolean);
    procedure SetClosed(state: Boolean);

    procedure SetZaver(zaver: Boolean);
    function GetZaver(): Boolean;

    function IsSignalClosed(): Boolean;
    function IsSignalCaution(): Boolean;
    function IsSignalOpen(): Boolean;

    function TrackClosed(): Boolean;
    function TrackPositiveLight(): Boolean;
    function IsAnnulation(): Boolean;
    function IsPositive(): Boolean;
    function IsEnabled(): Boolean;
    function IsPreringElapsed(): Boolean;
    function IsInputValid(): Boolean;
    function RCSModulesAvailable(): Boolean;
    function IsSafelyClosed(): Boolean;

    procedure MenuUZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZUZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights);
    procedure MenuSTAVClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure UPOUZClick(Sender: TObject);
    procedure UPOZUZClick(Sender: TObject);
    procedure UPONOTClick(Sender: TObject);
    procedure UPOZNOTClick(Sender: TObject);

    procedure MenuAdminZavreno(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminOtevreno(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminVystraha(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminAnulaceStart(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminAnulaceStop(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminNUZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure SetSimInputs(closed, caution, open: Boolean);
    procedure SetPanelSimInputs(closed, caution, open: Boolean; SenderPnl: TIdContext; SenderOR: TObject);

    procedure FillRCSModules();
    procedure SetState(new: TBlkCrossingBasicState);

    function OccupiedTrackIds(): TList<Integer>;
    procedure CSAddOccupiedTracks(var items: TConfSeqItems);

    procedure PanelShowState(pnl: TIdContext; area: TArea);

  public
    tracks: TObjectList<TBlkCrossingTrack>;
    positiveRules: TPositiveRules;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    // ----- Crossing specific functions -----

    function GetSettings(): TBlkCrossingSettings;
    procedure SetSettings(data: TBlkCrossingSettings);

    procedure AddSH(Sender: TBlk);
    procedure RemoveSH(Sender: TBlk);

    function WantClose(): Boolean;

    property state: TBlkCrossingBasicState read m_state.state;
    property fullState: TBlkCrossingState read m_state;
    property pcEmOpen: Boolean read m_state.pcEmOpen write SetEmOpen;
    property pcClosed: Boolean read m_state.pcClosed write SetClosed;
    property note: string read m_state.note write SetNote;
    property lockout: string read m_state.lockout write SetLockout;
    property zaver: Boolean read GetZaver write SetZaver;
    property annulation: Boolean read IsAnnulation;
    property positive: Boolean read IsPositive;
    property enabled: Boolean read IsEnabled;
    property safelyClosed: Boolean read IsSafelyClosed;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    procedure PanelZUZCallBack(Sender: TIdContext; success: Boolean);
    procedure PanelZNOTCallBack(Sender: TIdContext; success: Boolean);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses BlockDb, GetSystems, ownStrUtils, TJCDatabase, TCPServerPanel, RCSIFace, UPO,
  Graphics, PanelConnData, Diagnostics, appEv, ownConvert, Config, timeHelper,
  BlockTrack, BlockTrackRef, colorHelper, RCSErrors, PTUtils;

constructor TBlkCrossing.Create(index: Integer);
begin
  inherited;

  Self.m_globSettings.typ := btCrossing;
  Self.m_state := Self._def_crossing_state;
  Self.m_state.shs := TList<TBlk>.Create();
  Self.m_state.rcsModules := TList<Cardinal>.Create();
  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();
  Self.positiveRules := TPositiveRules.Create();
end;

destructor TBlkCrossing.Destroy();
begin
  Self.m_state.shs.Free();
  Self.m_state.rcsModules.Free();
  Self.tracks.Free();
  Self.positiveRules.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_state.note := '';
  Self.m_state.lockout := '';

  Self.m_settings.RCSInputs.closed := RCSOptionalFromIni(ini_tech, section, _SECT_RCSICLOSED, 'RCSIzm', 'RCSIz');
  Self.m_settings.RCSInputs.open := RCSOptionalFromIni(ini_tech, section, _SECT_RCSIOPEN, 'RCSIom', 'RCSIo');
  Self.m_settings.RCSInputs.caution := RCSOptionalFromIni(ini_tech, section, _SECT_RCSICAUTION, 'RCSIvm', 'RCSIv');
  Self.m_settings.RCSInputs.annulation := RCSOptionalFromIni(ini_tech, section, _SECT_RCSIANNULATION, 'RCSam', 'RCSa');

  Self.m_settings.RCSOutputs.close := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOCLOSE, 'RCSOzm', 'RCSOz');
  Self.m_settings.RCSOutputs.emOpen := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOEMOPEN, 'RCSOnotm', 'RCSOnot');
  Self.m_settings.RCSOutputs.lights := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOLIGHTS);
  Self.m_settings.RCSOutputs.positive := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOPOSITIVE);
  if (not Self.m_settings.RCSOutputs.positive.enabled) then // backward compatibility
    Self.m_settings.RCSOutputs.positive := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOBLOCKPOSITIVE, 'RCSObpm', 'RCSObp');
  Self.m_settings.RCSOutputs.positiveInvert := ini_tech.ReadBool(section, _SECT_RCSOPOSITIVE_INVERT, False);
  Self.m_settings.RCSOutputs.positiveFlick := ini_tech.ReadBool(section, _SECT_RCSOPOSITIVE_FLICK, False);

  Self.m_settings.RCSOutputs.barriersDown := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOBARRIERS_DOWN);
  Self.m_settings.RCSOutputs.barriersUp := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOBARRIERS_UP);
  Self.m_settings.RCSOutputs.bell := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOBELL);
  Self.m_settings.RCSOutputs.bellActiveDown := ini_tech.ReadBool(section, _SECT_RCSOBELL_ACTIVE_DOWN, False);

  Self.m_settings.preringTime := ini_tech.ReadInteger(section, _SECT_PRERING_TIME, 0);
  Self.m_settings.closedRequired := ini_tech.ReadBool(section, _SECT_CLOSED_REQUIRED, False);

  Self.tracks.Clear();
  var notracks: Integer := ini_tech.ReadInteger(section, _SECT_TRACKS, 0);
  for var i: Integer := 0 to notracks - 1 do
  begin
    var track: TBlkCrossingTrack := TBlkCrossingTrack.Create();
    try
      track.Load(ini_tech, section, 'T' + IntToStr(i));
      Self.tracks.Add(track);
    except
      on E: Exception do
        AppEvents.LogException(E, 'LoadTracks');
    end;
  end;

  Self.positiveRules.Clear();
  var positiveLine: string := ini_tech.ReadString(section, _SECT_POSITIVE + '0', '');
  var count: Integer := 0;
  while (positiveLine <> '') do
  begin
    try
      Self.positiveRules.Add(TPositiveRule.Create(positiveLine));
    except
      on E: Exception do
        AppEvents.LogException(E, 'TBlkCrossing.positiveRules.Add '+IntToStr(count));
    end;
    Inc(count);
    positiveLine := ini_tech.ReadString(section, _SECT_POSITIVE + IntToStr(count), '');
  end;

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');

  Self.LoadAreas(ini_rel, 'PRJ').Free();

  Self.FillRCSModules();
  for var module: Cardinal in Self.m_state.rcsModules do
    RCSi.SetNeeded(module);
  for var area: TArea in Self.m_areas do
    for var module: Cardinal in Self.m_state.rcsModules do
      area.RCSAdd(module);
end;

procedure TBlkCrossing.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  if (Self.m_settings.RCSInputs.closed.enabled) then
    ini_tech.WriteString(section, _SECT_RCSICLOSED, Self.m_settings.RCSInputs.closed.addr.ToString());
  if (Self.m_settings.RCSInputs.open.enabled) then
    ini_tech.WriteString(section, _SECT_RCSIOPEN, Self.m_settings.RCSInputs.open.addr.ToString());
  if (Self.m_settings.RCSInputs.caution.enabled) then
    ini_tech.WriteString(section, _SECT_RCSICAUTION, Self.m_settings.RCSInputs.caution.addr.ToString());
  if (Self.m_settings.RCSInputs.annulation.enabled) then
    ini_tech.WriteString(section, _SECT_RCSIANNULATION, Self.m_settings.RCSInputs.annulation.addr.ToString());

  if (Self.m_settings.RCSOutputs.close.enabled) then
    ini_tech.WriteString(section, _SECT_RCSOCLOSE, Self.m_settings.RCSOutputs.close.addr.ToString());
  if (Self.m_settings.RCSOutputs.emOpen.enabled) then
    ini_tech.WriteString(section, _SECT_RCSOEMOPEN, Self.m_settings.RCSOutputs.emOpen.addr.ToString());
  if (Self.m_settings.RCSOutputs.lights.enabled) then
    ini_tech.WriteString(section, _SECT_RCSOLIGHTS, Self.m_settings.RCSOutputs.lights.addr.ToString());
  if (Self.m_settings.RCSOutputs.positive.enabled) then
  begin
    ini_tech.WriteString(section, _SECT_RCSOPOSITIVE, Self.m_settings.RCSOutputs.positive.addr.ToString());
    if (Self.m_settings.RCSOutputs.positiveInvert) then
      ini_tech.WriteBool(section, _SECT_RCSOPOSITIVE_INVERT, Self.m_settings.RCSOutputs.positiveInvert);
    if (Self.m_settings.RCSOutputs.positiveFlick) then
      ini_tech.WriteBool(section, _SECT_RCSOPOSITIVE_FLICK, Self.m_settings.RCSOutputs.positiveFlick);
  end;

  if (Self.m_settings.RCSOutputs.barriersDown.enabled) then
    ini_tech.WriteString(section, _SECT_RCSOBARRIERS_DOWN, Self.m_settings.RCSOutputs.barriersDown.addr.ToString());
  if (Self.m_settings.RCSOutputs.barriersUp.enabled) then
    ini_tech.WriteString(section, _SECT_RCSOBARRIERS_UP, Self.m_settings.RCSOutputs.barriersUp.addr.ToString());
  if (Self.m_settings.RCSOutputs.bell.enabled) then
  begin
    ini_tech.WriteString(section, _SECT_RCSOBELL, Self.m_settings.RCSOutputs.bell.addr.ToString());
    ini_tech.WriteBool(section, _SECT_RCSOBELL_ACTIVE_DOWN, Self.m_settings.RCSOutputs.bellActiveDown);
  end;

  if (Self.m_settings.preringTime > 0) then
    ini_tech.WriteInteger(section, _SECT_PRERING_TIME, Self.m_settings.preringTime);
  ini_tech.WriteBool(section, _SECT_CLOSED_REQUIRED, Self.m_settings.closedRequired);

  if (Self.tracks.Count > 0) then
    ini_tech.WriteInteger(section, _SECT_TRACKS, Self.tracks.Count);
  for var i: Integer := 0 to Self.tracks.Count - 1 do
    Self.tracks[i].Save(ini_tech, section, 'T' + IntToStr(i));

  for var i: Integer := 0 to Self.positiveRules.Count - 1 do
    ini_tech.WriteString(section, _SECT_POSITIVE + IntToStr(i), Self.positiveRules[i].IdStr());
end;

procedure TBlkCrossing.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.Enable();
begin
  try
    for var module: Cardinal in Self.m_state.rcsModules do
      if (not RCSi.IsNonFailedModule(module)) then
        Exit();
  except
    Exit();
  end;

  Self.m_state.state := TBlkCrossingBasicState.unknown;
  Self.m_state.annulationOld := Self.annulation;
  if (Self.IsInputValid()) then
    Self.m_state.lastInputValid := Now;
  Self.Change();
end;

procedure TBlkCrossing.Disable();
begin
  Self.m_state.state := TBlkCrossingBasicState.disabled;
  Self.m_state.shs.Clear();
  Self.Change(true);
end;

function TBlkCrossing.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := False;

  if (Self.m_settings.RCSInputs.closed.enabled) then
    Result := Result or (portType = TRCSIOType.input) and (addr = Self.m_settings.RCSInputs.closed.addr);
  if (Self.m_settings.RCSInputs.open.enabled) then
    Result := Result or (portType = TRCSIOType.input) and (addr = Self.m_settings.RCSInputs.open.addr);
  if (Self.m_settings.RCSInputs.caution.enabled) then
    Result := Result or (portType = TRCSIOType.input) and (addr = Self.m_settings.RCSInputs.caution.addr);
  if (Self.m_settings.RCSInputs.annulation.enabled) then
    Result := Result or (portType = TRCSIOType.input) and (addr = Self.m_settings.RCSInputs.annulation.addr);

  if (Self.m_settings.RCSOutputs.close.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.close.addr);
  if (Self.m_settings.RCSOutputs.emOpen.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.emOpen.addr);
  if (Self.m_settings.RCSOutputs.positive.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.positive.addr);

  if (Self.m_settings.RCSOutputs.barriersDown.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.barriersDown.addr);
  if (Self.m_settings.RCSOutputs.barriersUp.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.barriersUp.addr);
  if (Self.m_settings.RCSOutputs.bell.enabled) then
    Result := Result or (portType = TRCSIOType.output) and (addr = Self.m_settings.RCSOutputs.bell.addr);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.Update();
begin
  var available := Self.RCSModulesAvailable();

  if ((not available) and (Self.m_state.state <> TBlkCrossingBasicState.disabled)) then
  begin
    Self.m_state.state := TBlkCrossingBasicState.disabled;
    JCDb.Cancel(Self);
    Self.Change(true);
  end;

  if ((available) and (Self.m_state.state = TBlkCrossingBasicState.disabled)) then
  begin
    Self.m_state.state := TBlkCrossingBasicState.unknown;
    Self.Change();
  end;

  if (Self.m_state.state = TBlkCrossingBasicState.disabled) then
    Exit();

  var new_state: TBlkCrossingBasicState := Self.m_state.state;

  if (Self.IsInputValid()) then
  begin
    Self.m_state.lastInputValid := Now;
    new_state := Self.UpdateState();
  end;
  if (Now >= Self.m_state.lastInputValid+EncodeTimeSec(_INVALID_INPUT_TOLER_SEC)) then
    new_state := TBlkCrossingBasicState.error;

  if ((Self.state = TBlkCrossingBasicState.caution) and (Self.IsPreringElapsed()) and (not Self.m_state.barriersClosed)) then
  begin
    Self.m_state.barriersClosed := True;
    Self.Change();
  end;

  Self.SetState(new_state);

  if (Self.m_state.annulationOld <> Self.annulation) then
  begin
    Self.m_state.annulationOld := Self.annulation;
    Self.Change();
  end;

  if (Self.WantClose() <> Self.m_state.lastWantClose) then
  begin
    Self.m_state.lastWantClose := Self.WantClose();
    if (Self.WantClose()) then
    begin
      // closing started by hJOP
      Self.m_state.warningTimeout := Now + EncodeTime(0, _UZ_UPOZ_MIN, 0, 0);
      if ((RCSi.simulation) and (Self.m_settings.RCSInputs.caution.enabled)) then
        RCSi.SetInput(Self.m_settings.RCSInputs.caution.addr, 1); // so error state is prevented
    end;
  end;

  // kontrola prilis dlouho uzavreneho prejezdu
  if (Self.WantClose()) then
  begin
    if (now > Self.m_state.warningTimeout) then
    begin
      Self.BottomErrorBroadcast(Self.m_globSettings.name + ' uzavřen déle, jak ' + IntToStr(_UZ_UPOZ_MIN) + ' min',
          'VAROVÁNÍ');
      Self.m_state.warningTimeout := Now + EncodeTime(0, _UZ_UPOZ_MIN, 0, 0);
    end;
  end;

  Self.UpdateTracks();
  Self.UpdatePositive();

  if (Self.changed) then
    Self.UpdateOutputs();
  inherited Update();
end;

function TBlkCrossing.UpdateState(): TBlkCrossingBasicState;
begin
  Result := Self.m_state.state;

  case (Self.m_state.state) of
    TBlkCrossingBasicState.unknown, TBlkCrossingBasicState.error: begin
      if (Self.IsSignalClosed()) then
        Result := TBlkCrossingBasicState.closed
      else if (Self.IsSignalOpen()) then
        Result := TBlkCrossingBasicState.open
      else if (Self.IsSignalCaution()) then
        Result := TBlkCrossingBasicState.caution;
    end;

    TBlkCrossingBasicState.open: begin
      Self.m_state.barriersClosed := False;
      if ((Self.WantClose()) or (Self.IsSignalCaution())) then
        Result := TBlkCrossingBasicState.caution
      else if ((Self.IsSignalClosed()) and (Self.IsSignalCaution())) then
        Result := TBlkCrossingBasicState.closed
    end;

    TBlkCrossingBasicState.caution: begin
      if ((Self.IsSignalClosed()) and (Self.IsSignalCaution())) then
        Result := TBlkCrossingBasicState.closed
      else if ((not Self.IsSignalCaution()) and (Self.IsSignalOpen())) then
        Result := TBlkCrossingBasicState.open;
    end;

    TBlkCrossingBasicState.closed: begin
      if ((not Self.IsSignalClosed()) and (Self.WantClose)) then
      begin
        Result := TBlkCrossingBasicState.caution;
        if (Self.m_settings.closedRequired) then
        begin
          Self.BottomErrorBroadcast('Ztráta dohledu na přejezdu ' + Self.m_globSettings.name, 'TECHNOLOGIE');
          JCDb.Cancel(Self);
        end else begin
          Self.BottomErrorBroadcast('Nouzový stav ' + Self.m_globSettings.name, 'TECHNOLOGIE');
        end;
      end else if (not Self.IsSignalClosed()) then
        Result := TBlkCrossingBasicState.caution;
    end;
  end;
end;

procedure TBlkCrossing.UpdateTracks();
var changed: Boolean;
begin
  changed := false;
  for var track: TBlkCrossingTrack in Self.tracks do
  begin
    track.Update();
    if (track.stateChanged) then
    begin
      changed := true;
      track.stateChanged := false;
    end;
  end;

  if (changed) then
    Self.Change();
end;

procedure TBlkCrossing.UpdatePositive();
begin
  var positive: Boolean := Self.positive;
  if (Self.m_state.lastPositive <> positive) then
  begin
    Self.m_state.lastPositive := positive;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.Change(now: Boolean = false);
begin
  inherited;

  try
    for var sh: TBlk in Self.m_state.shs do
      sh.Change();
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.UpdateOutputs();
begin
  try
    if (Self.m_settings.RCSOutputs.close.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.close.addr, ownConvert.BoolToInt(Self.WantClose()));
    if (Self.m_settings.RCSOutputs.emOpen.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.emOpen.addr, ownConvert.BoolToInt(Self.m_state.pcEmOpen));

    if (Self.m_settings.RCSOutputs.lights.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.lights.addr, ownConvert.BoolToInt(
        (Self.state = TBlkCrossingBasicState.caution) or (Self.state = TBlkCrossingBasicState.closed)
      ));

    var barriersToDown: Boolean := (Self.WantClose() and Self.IsPreringElapsed());
    if (Self.m_settings.RCSOutputs.barriersDown.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.barriersDown.addr, ownConvert.BoolToInt(barriersToDown));
    if (Self.m_settings.RCSOutputs.barriersUp.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.barriersUp.addr, ownConvert.BoolToInt(not barriersToDown));

    if (Self.m_settings.RCSOutputs.bell.enabled) then
    begin
      RCSi.SetOutput(Self.m_settings.RCSOutputs.bell.addr, ownConvert.BoolToInt(
        (Self.state = TBlkCrossingBasicState.caution) or ((Self.state = TBlkCrossingBasicState.closed) and (Self.m_settings.RCSOutputs.bellActiveDown))
      ));
    end;

    if (Self.m_settings.RCSOutputs.positive.enabled) then
    begin
      if (Self.m_settings.RCSOutputs.positiveFlick) then
      begin
        if (Self.positive) then
          RCSi.SetOutput(Self.m_settings.RCSOutputs.positive.addr, TRCSOutputState.osf33)
        else
          RCSi.SetOutput(Self.m_settings.RCSOutputs.positive.addr, TRCSOutputState.osDisabled);
      end else begin
        RCSi.SetOutput(Self.m_settings.RCSOutputs.positive.addr, ownConvert.BoolToInt(Self.positive xor Self.m_settings.RCSOutputs.positiveInvert));
      end;
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.GetSettings(): TBlkCrossingSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkCrossing.SetSettings(data: TBlkCrossingSettings);
begin
  Self.m_settings := data;
  Self.FillRCSModules();
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkCrossing.SetLockout(lockout: string);
begin
  Self.m_state.lockout := lockout;
  Self.Change();
end;

procedure TBlkCrossing.SetEmOpen(state: Boolean);
begin
  if (Self.m_state.pcEmOpen = state) then
    Exit();
  if ((Self.zaver) and (state)) then
    Exit();

  if (state) then
  begin
    // NOT rusi jizdni cesty vedouci pres prejezd
    JCDb.Cancel(Self);
  end;

  Self.m_state.pcEmOpen := state;
  Self.Change();
end;

procedure TBlkCrossing.SetClosed(state: Boolean);
begin
  if (Self.m_state.pcClosed = state) then
    Exit();

  if ((state) and (Self.pcEmOpen)) then
    raise EPrjNOT.Create('Prejezd nouzove otevren, nelze uzavrit!');

  Self.m_state.pcClosed := state;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.MenuUZClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var upos := TList<TUPOItem>.Create();
  try
    if (Self.note <> '') then
      upos.Add(UPO.NoteUPO(Self.name, Self.note));
    PanelServer.UPO(SenderPnl, upos, false, Self.UPOUZClick, nil, SenderOR);
  finally
    upos.Free();
  end;
end;

procedure TBlkCrossing.MenuZUZClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var upos := TList<TUPOItem>.Create();
  try
    if (Self.note <> '') then
      upos.Add(UPO.NoteUPO(Self.name, Self.note));
    TArea(SenderOR).AddPNsUPOs(upos);
    TArea(SenderOR).AddNCsUPOs(upos);
    PanelServer.UPO(SenderPnl, upos, false, Self.UPOZUZClick, nil, SenderOR);
  finally
    upos.Free();
  end;
end;

procedure TBlkCrossing.MenuNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var upos := TList<TUPOItem>.Create();
  try
    if (Self.note <> '') then
      upos.Add(UPO.NoteUPO(Self.name, Self.note));
    TArea(SenderOR).AddPNsUPOs(upos);
    TArea(SenderOR).AddNCsUPOs(upos);
    PanelServer.UPO(SenderPnl, upos, false, Self.UPONOTClick, nil, SenderOR);
  finally
    upos.Free();
  end;
end;

procedure TBlkCrossing.MenuZNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var upos := TList<TUPOItem>.Create();
  try
    if (Self.note <> '') then
      upos.Add(UPO.NoteUPO(Self.name, Self.note));
    PanelServer.UPO(SenderPnl, upos, false, Self.UPOZNOTClick, nil, SenderOR);
  finally
    upos.Free();
  end;
end;

procedure TBlkCrossing.UPOUZClick(Sender: TObject);
begin
  Self.pcClosed := true;
end;

procedure TBlkCrossing.UPOZUZClick(Sender: TObject);
begin
  var csItems := TList<TConfSeqItem>.Create();
  try
    Self.CSAddOccupiedTracks(csItems);

    PanelServer.ConfirmationSequence(TIdContext(Sender), Self.PanelZUZCallBack,
      (TPanelConnData(TIdContext(Sender).data).UPO_ref as TArea), 'Zrušení uzavření přejezdu',
      GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkCrossing.UPONOTClick(Sender: TObject);
begin
  var csItems := TList<TConfSeqItem>.Create();
  try
    Self.CSAddOccupiedTracks(csItems);

    PanelServer.ConfirmationSequence(TIdContext(Sender), Self.PanelZNOTCallBack,
      (TPanelConnData(TIdContext(Sender).data).UPO_ref as TArea), 'Nouzové otevření přejezdu',
      GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

procedure TBlkCrossing.UPOZNOTClick(Sender: TObject);
begin
  Self.pcEmOpen := false;
end;

procedure TBlkCrossing.MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights);
begin
  PanelServer.note(SenderPnl, Self, Self.m_state.note, rights);
end;

procedure TBlkCrossing.MenuSTAVClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.PanelShowState(SenderPnl, SenderOR as TArea);
end;

procedure TBlkCrossing.MenuAdminZavreno(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetPanelSimInputs(true, true, false, SenderPnl, SenderOR);
end;

procedure TBlkCrossing.MenuAdminOtevreno(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetPanelSimInputs(false, false, true, SenderPnl, SenderOR);
end;

procedure TBlkCrossing.MenuAdminVystraha(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetPanelSimInputs(false, true, false, SenderPnl, SenderOR);
end;

procedure TBlkCrossing.MenuAdminAnulaceStart(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (not Self.m_settings.RCSInputs.annulation.enabled) then
  begin
    PanelServer.BottomError(SenderPnl, 'Vstup anulace nezadán!', TArea(SenderOR).shortName, 'SIMULACE');
    Exit();
  end;

  try
    RCSi.SetInput(Self.m_settings.RCSInputs.annulation.addr, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).shortName,
      'SIMULACE');
  end;
end;

procedure TBlkCrossing.MenuAdminAnulaceStop(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (not Self.m_settings.RCSInputs.annulation.enabled) then
  begin
    PanelServer.BottomError(SenderPnl, 'Vstup anulace nezadán!', TArea(SenderOR).shortName, 'SIMULACE');
    Exit();
  end;

  try
    RCSi.SetInput(Self.m_settings.RCSInputs.annulation.addr, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).shortName,
      'SIMULACE');
  end;
end;

procedure TBlkCrossing.MenuAdminNUZClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.m_state.zaver := 0;
  Self.Change();
end;

procedure TBlkCrossing.SetSimInputs(closed, caution, open: Boolean);
begin
  if (Self.m_settings.RCSInputs.closed.enabled) then
    RCSi.SetInput(Self.m_settings.RCSInputs.closed.addr, ownConvert.BoolToInt(closed));
  if (Self.m_settings.RCSInputs.caution.enabled) then
    RCSi.SetInput(Self.m_settings.RCSInputs.caution.addr, ownConvert.BoolToInt(caution));
  if (Self.m_settings.RCSInputs.open.enabled) then
    RCSi.SetInput(Self.m_settings.RCSInputs.open.addr, ownConvert.BoolToInt(open));
end;

procedure TBlkCrossing.SetPanelSimInputs(closed, caution, open: Boolean; SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    Self.SetSimInputs(closed, caution, open);
  except
    if ((SenderPnl <> nil) and (SenderOR <> nil)) then
      PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).shortName,
        'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkCrossing.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  if ((IsWritable(rights)) and (Self.m_state.state <> TBlkCrossingBasicState.disabled)) then
  begin
    if (not Self.m_state.pcEmOpen) then
    begin
      if (Self.m_state.pcClosed) then
        Result := Result + '!ZUZ,'
      else
        Result := Result + 'UZ,';
    end;

    if (not Self.zaver) then
    begin
      if (not Self.m_state.pcClosed) then
      begin
        if (Self.m_state.pcEmOpen) then
          Result := Result + 'NOT<,'
        else
          Result := Result + '!NOT>,';
      end;
    end;
  end;

  if ((IsWritable(rights)) or (Self.note <> '')) then
    Result := Result + 'STIT,';

  Result := Result + 'STAV?,';

  // pokud mame knihovnu simulator, muzeme ridit stav useku
  // DEBUG nastroj
  if ((IsWritable(rights)) and (RCSi.simulation)) then
  begin
    Result := Result + '-,*ZAVŘENO,*OTEVŘENO,*VÝSTRAHA,';
    if (Self.m_settings.RCSInputs.annulation.enabled) then
    begin
      if (RCSi.GetInput(Self.m_settings.RCSInputs.annulation.addr) = TRCSInputState.isOn) then
        Result := Result + '*ANULACE<'
      else
        Result := Result + '*ANULACE>';
    end;
  end;

  if (rights = TAreaRights.superuser) then
  begin
    Result := Result + '-,';
    if (Self.zaver) then
      Result := Result + '*NUZ>,';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (Button <> TPanelButton.ESCAPE) then
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkCrossing.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer; rights: TAreaRights);
begin
  if (item = 'UZ') then
    Self.MenuUZClick(SenderPnl, SenderOR)
  else if (item = 'ZUZ') then
    Self.MenuZUZClick(SenderPnl, SenderOR)
  else if (item = 'NOT>') then
    Self.MenuNOTClick(SenderPnl, SenderOR)
  else if (item = 'NOT<') then
    Self.MenuZNOTClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuSTITClick(SenderPnl, SenderOR, rights)
  else if (item = 'STAV?') then
    Self.MenuSTAVClick(SenderPnl, SenderOR)
  else if (item = 'NUZ>') then
    Self.MenuAdminNUZClick(SenderPnl, SenderOR)
  else if (item = 'ZAVŘENO') then
    Self.MenuAdminZavreno(SenderPnl, SenderOR)
  else if (item = 'OTEVŘENO') then
    Self.MenuAdminOtevreno(SenderPnl, SenderOR)
  else if (item = 'VÝSTRAHA') then
    Self.MenuAdminVystraha(SenderPnl, SenderOR)
  else if (item = 'ANULACE>') then
    Self.MenuAdminAnulaceStart(SenderPnl, SenderOR)
  else if (item = 'ANULACE<') then
    Self.MenuAdminAnulaceStop(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

// zavola se pri uspesnem zvladnuti potvrzovaci sekvence
procedure TBlkCrossing.PanelZNOTCallBack(Sender: TIdContext; success: Boolean);
begin
  if (not success) then
    Exit();
  Self.pcEmOpen := true;
end;

procedure TBlkCrossing.PanelZUZCallBack(Sender: TIdContext; success: Boolean);
begin
  if (success) then
    Self.pcClosed := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.SetZaver(zaver: Boolean);
begin
  if (zaver) then
  begin
    Inc(Self.m_state.zaver);

    if (Self.pcEmOpen) then
      raise EPrjNOT.Create('Prejezd nouzove otevren, nelze udelit zaver!');

    if (Self.m_state.zaver = 1) then
    begin
      // prvni udeleni zaveru
      Self.SetEmOpen(False);
      Self.Change();
    end;
  end else begin
    Dec(Self.m_state.zaver);

    if (Self.m_state.zaver <= 0) then
    begin
      // posledni odstraneni zaveru
      Self.Change();
    end;
  end;
end;

function TBlkCrossing.GetZaver(): Boolean;
begin
  Result := (Self.m_state.zaver > 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.AddSH(Sender: TBlk);
begin
  if (not Self.m_state.shs.Contains(Sender)) then
    Self.m_state.shs.Add(Sender);
end;

procedure TBlkCrossing.RemoveSH(Sender: TBlk);
begin
  if (Self.m_state.shs.Contains(Sender)) then
    Self.m_state.shs.Remove(Sender);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.FillRCSModules();
begin
  Self.m_state.rcsModules.Clear();

  if (Self.m_settings.RCSInputs.closed.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.closed.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.closed.addr.module);

  if (Self.m_settings.RCSInputs.open.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.open.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.open.addr.module);

  if (Self.m_settings.RCSInputs.caution.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.caution.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.caution.addr.module);

  if (Self.m_settings.RCSInputs.annulation.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.annulation.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.annulation.addr.module);

  if (Self.m_settings.RCSOutputs.close.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.close.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.close.addr.module);

  if (Self.m_settings.RCSOutputs.lights.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.lights.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.lights.addr.module);

  if (Self.m_settings.RCSOutputs.emOpen.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.emOpen.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.emOpen.addr.module);

  if (Self.m_settings.RCSOutputs.positive.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.positive.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.positive.addr.module);

  if (Self.m_settings.RCSOutputs.barriersDown.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.barriersDown.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.barriersDown.addr.module);

  if (Self.m_settings.RCSOutputs.barriersUp.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.barriersUp.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.barriersUp.addr.module);

  if (Self.m_settings.RCSOutputs.bell.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.bell.addr.module)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.bell.addr.module);

  Self.m_state.rcsModules.Sort();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  bg := TJopColor.black;
  if (Self.note <> '') then
    bg := TJopColor.turqDark;

  if (diag.showZaver) then
  begin
    if (Self.zaver) then
      bg := TJopColor.greenDark
    else if (Self.TrackClosed()) then
      bg := TJopColor.blue;
  end;

  if (Self.pcEmOpen) then
    fg := TJopColor.red
  else if (Self.pcClosed) then
    fg := TJopColor.white
  else
    fg := TJopColor.grayDark;

  case (Self.state) of
    TBlkCrossingBasicState.disabled:
      begin
        fg := bg;
        bg := TJopColor.purple;
      end;

    TBlkCrossingBasicState.error:
      begin
        fg := bg;
        bg := TJopColor.red;
      end;
  end;

  var panelState: TBlkCrossingPanelState := TBlkCrossingPanelState(Self.state);
  if (Self.state = TBlkCrossingBasicState.unknown) then
    panelState := TBlkCrossingPanelState.psCaution; // unknown is shown as caution
  if ((Self.state = TBlkCrossingBasicState.open) and (Self.annulation)) then
     panelState := TBlkCrossingPanelState.psAnnulation;

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) + ';0;' +
    IntToStr(Integer(panelState)) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  if (Self.m_settings.RCSInputs.closed.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSInputs.closed.addr, json['rcs']['closed']);
  if (Self.m_settings.RCSInputs.open.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSInputs.open.addr, json['rcs']['opened']);
  if (Self.m_settings.RCSInputs.caution.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSInputs.caution.addr, json['rcs']['warning']);
  if (Self.m_settings.RCSInputs.annulation.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSInputs.annulation.addr, json['rcs']['annulation']);
  if (Self.m_settings.RCSOutputs.close.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.close.addr, json['rcs']['close']);
  if (Self.m_settings.RCSOutputs.emOpen.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.emOpen.addr, json['rcs']['emOpen']);
  if (Self.m_settings.RCSOutputs.positive.enabled) then
  begin
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.positive.addr, json['rcs']['positive']);
    json['rcs'].B['positiveInvert'] := Self.m_settings.RCSOutputs.positiveInvert;
    json['rcs'].B['positiveFlick'] := Self.m_settings.RCSOutputs.positiveFlick;
  end;
  if (Self.m_settings.RCSOutputs.barriersDown.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.barriersDown.addr, json['rcs']['barriersDown']);
  if (Self.m_settings.RCSOutputs.barriersUp.enabled) then
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.barriersUp.addr, json['rcs']['barriersUp']);
  if (Self.m_settings.RCSOutputs.bell.enabled) then
  begin
    TBlk.RCStoJSON(Self.m_settings.RCSOutputs.bell.addr, json['rcs']['bell']);
    json['rcs'].B['bellActiveDown'] := Self.m_settings.RCSOutputs.bellActiveDown;
  end;

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkCrossing.GetPtState(json: TJsonObject);
begin
  case (Self.m_state.state) of
    TBlkCrossingBasicState.disabled:
      json['state'] := 'disabled';
    TBlkCrossingBasicState.unknown:
      json['state'] := 'unknown';
    TBlkCrossingBasicState.error:
      json['state'] := 'error';
    TBlkCrossingBasicState.open:
      json['state'] := 'opened';
    TBlkCrossingBasicState.caution:
      json['state'] := 'caution';
    TBlkCrossingBasicState.closed:
      json['state'] := 'closed';
  end;

  if (Self.note <> '') then
    json['note'] := Self.m_state.note;
  if (Self.lockout <> '') then
    json['lockout'] := Self.m_state.lockout;

  json['annulation'] := Self.annulation;
  json['pcEmOpen'] := Self.m_state.pcEmOpen;
  json['pcClosed'] := Self.m_state.pcClosed;
  json['locks'] := Self.m_state.zaver;
end;

procedure TBlkCrossing.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('pcClosed')) then
    Self.pcClosed := reqJson.B['pcClosed'];

  if (reqJson.Contains('note')) then
    Self.note := reqJson.S['note'];
  if (reqJson.Contains('lockout')) then
    Self.lockout := reqJson.S['lockout'];

  if (reqJson.Contains('state')) then
  begin
    try
      if (reqJson.S['state'] = 'opened') then
        Self.SetSimInputs(False, False, True)
      else if (reqJson.S['state'] = 'closed') then
        Self.SetSimInputs(True, True, False)
      else if (reqJson.S['state'] = 'caution') then
        Self.SetSimInputs(False, True, False);
    except
      on e: RCSException do
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '500', 'Simulace nepovolila nastaveni RCS vstupu', e.Message);
    end;

    Self.Update(); // to propagate new state into response
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.TrackClosed(): Boolean;
var track: TBlkCrossingTrack;
begin
  for track in Self.tracks do
    if (track.shouldBeClosed) then
      Exit(true);
  Result := false;
end;

function TBlkCrossing.TrackPositiveLight(): Boolean;
var track: TBlkCrossingTrack;
begin
  for track in Self.tracks do
    if (not track.positiveLight) then
      Exit(false);
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.IsAnnulation(): Boolean;
begin
  Result := false;
  if (Self.m_settings.RCSInputs.annulation.enabled and RCSi.Started) then
  begin
    try
      Result := (RCSi.GetInput(Self.m_settings.RCSInputs.annulation.addr) = isOn);
    except

    end;
  end;

  if (not Result) then
    for var track: TBlkCrossingTrack in Self.tracks do
      if (track.anullation) then
        Exit(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.IsSignalClosed(): Boolean;
begin
  Result := Self.IsPreringElapsed();

  if (Self.m_settings.RCSInputs.closed.enabled) then
  begin
    try
      Result := Result and (RCSi.GetInput(Self.m_settings.RCSInputs.closed.addr) = isOn);
    except
      Exit(False);
    end;
  end else begin
    Result := Result and Self.WantClose();
  end;
end;

function TBlkCrossing.IsSignalCaution(): Boolean;
begin
  Result := (Self.WantClose());

  if (Self.m_settings.RCSInputs.caution.enabled) then
  begin
    try
      Result := Result or (RCSi.GetInput(Self.m_settings.RCSInputs.caution.addr) = isOn);
    except
      Exit();
    end;
  end;
end;

function TBlkCrossing.IsSignalOpen(): Boolean;
begin
  Result := (not Self.WantClose());

  if ((Result) and (Self.m_settings.RCSInputs.open.enabled)) then
  begin
    try
      Result := Result and (RCSi.GetInput(Self.m_settings.RCSInputs.open.addr) = isOn);
    except
      Exit(False);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.IsPositive(): Boolean;
begin
  Result := (Self.state = TBlkCrossingBasicState.open) and (Self.TrackPositiveLight()) and (BlockCrossingPositive.PositiveOn(Self.positiveRules))
     and (not Self.pcClosed) and (not Self.pcEmOpen);
end;

function TBlkCrossing.IsEnabled(): Boolean;
begin
  Result := (Self.m_state.state > TBlkCrossingBasicState.disabled);
end;

function TBlkCrossing.IsPreringElapsed(): Boolean;
begin
  Result := (Now >= Self.m_state.cautionStart+EncodeTimeSec(Self.m_settings.preringTime));
end;

function TBlkCrossing.WantClose(): Boolean;
begin
  Result := ((Self.pcClosed) or (Self.zaver) or (Self.TrackClosed())) and (not Self.m_state.pcEmOpen);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.IsInputValid(): Boolean;
begin
  try
    if (((Self.m_settings.RCSInputs.open.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.open.addr) = isOn)) and
        ((Self.m_settings.RCSInputs.closed.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.closed.addr) = isOn))) then
      Exit(False);
    if (((Self.m_settings.RCSInputs.open.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.open.addr) = isOff)) and
        ((Self.m_settings.RCSInputs.closed.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.closed.addr) = isOff)) and
        ((Self.m_settings.RCSInputs.caution.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.caution.addr) = isOff))) then
      Exit(False);
    if ((Self.m_settings.RCSInputs.caution.enabled) and (Self.WantClose()) and (RCSi.GetInput(Self.m_settings.RCSInputs.caution.addr) = isOff)) then
      Exit(False);
  except
    Exit(False);
  end;

  Result := True;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.RCSModulesAvailable(): Boolean;
begin
  Result := True;
  try
    for var module: Cardinal in Self.m_state.rcsModules do
      Result := Result and RCSi.IsNonFailedModule(module);
  except
    Result := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.SetState(new: TBlkCrossingBasicState);
begin
  if (new = Self.state) then
    Exit();

  if ((new = TBlkCrossingBasicState.error) and (Self.state <> TBlkCrossingBasicState.disabled)) then
  begin
    Self.BottomErrorBroadcast('Porucha přejezdu ' + Self.m_globSettings.name, 'TECHNOLOGIE');
    JCDb.Cancel(Self);
  end;

  if (new = TBlkCrossingBasicState.caution) then // going to 'closed'
  begin
    Self.m_state.cautionStart := Now;
    Self.m_state.warningTimeout := Now + EncodeTime(0, _UZ_UPOZ_MIN, 0, 0);
    Self.m_state.barriersClosed := False;
  end;

  Self.m_state.state := new;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.IsSafelyClosed(): Boolean;
begin
  if (Self.m_settings.closedRequired) then
    Result := (Self.state = TBlkCrossingBasicState.closed) and (Self.IsSignalClosed())
  else
    Result := ((Self.state = TBlkCrossingBasicState.closed) or (Self.state = TBlkCrossingBasicState.caution)) and (Self.IsPreringElapsed());
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.OccupiedTrackIds(): TList<Integer>;
begin
  Result := TList<Integer>.Create();
  try
    for var track: TBlkCrossingTrack in Self.tracks do
      track.AddOccupiedLMRTracksIds(Result);
  except
    Result.Free();
    raise;
  end;
end;

procedure TBlkCrossing.CSAddOccupiedTracks(var items: TConfSeqItems);
begin
  var occupiedIds: TList<Integer> := Self.OccupiedTrackIds();
  try
    for var id: Integer in occupiedIds do
    begin
      var blk := Blocks.GetBlkByID(id);
      if (blk <> nil) then
        items.Add(CSItem(blk, 'Obsazený kolejový úsek'));
    end;
  finally
    occupiedIds.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.PanelShowState(pnl: TIdContext; area: TArea);
var lines: TList<TConfSeqItem>;
begin
  lines := TList<TConfSeqItem>.Create();
  try
    case (Self.state) of
      TBlkCrossingBasicState.disabled:
        lines.Add(CSItem(Self, 'Blok přejezdu neaktivní'));
      TBlkCrossingBasicState.unknown:
        lines.Add(CSItem(Self, 'Neznámý stav přejezdu'));
      TBlkCrossingBasicState.error:
        lines.Add(CSItem(Self, 'Poruchový stav přejezdu'));
      TBlkCrossingBasicState.open:
        lines.Add(CSItem(Self, 'Přejezd otevřen'));
      TBlkCrossingBasicState.caution, TBlkCrossingBasicState.closed:
        lines.Add(CSItem(Self, 'Výstražný stav přejezdu'));
    end;

    if (Self.IsSafelyClosed()) then
      lines.Add(CSItem(Self, 'Přejezd uzavřen'));
    if (Self.annulation) then
      lines.Add(CSItem(Self, 'Přejezd v anulaci'));
    if (Self.pcEmOpen) then
      lines.Add(CSItem(Self, 'Vydán pokyn k nouzovému otevření'));
    if ((Self.pcClosed) or (Self.zaver)) then
      lines.Add(CSItem(Self, 'Vydán pokyn k uzavření'));

    PanelServer.InfoWindow(pnl, nil, area, 'Zobrazení stavu přejezdu', GetObjsList(Self), lines, True, False);
  finally
    lines.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
