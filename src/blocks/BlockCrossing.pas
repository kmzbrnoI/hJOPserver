unit BlockCrossing;

// definice a obsluha technologickeho bloku Prejezd

interface

uses IniFiles, Block, SysUtils, Menus, AreaDb, Classes, TechnologieRCS,
  IdContext, Area, Generics.Collections, JsonDataObjects,
  BlockCrossingLogic;

type
  TBlkCrossingRCSInputs = record
    closed: TRCSAddrOptional; // or barriers closed
    open: TRCSAddrOptional; // or barriers open
    caution: TRCSAddrOptional;
    annulation: TRCSAddrOptional;
  end;

  TBlkCrossingRCSOutputs = record
    close: TRCSAddrOptional;
    emOpen: TRCSAddrOptional;
    positive: TRCSAddrOptional;
    positiveInvert: Boolean;
    positiveFlick: Boolean;
  end;

  TBlkCrossingSettings = record
    RCSInputs: TBlkCrossingRCSInputs;
    RCSOutputs: TBlkCrossingRCSOutputs;
  end;

  TBlkCrossingBasicState = (disabled = -5, none = -1, open = 0, caution = 1, closed = 2);

  TBlkCrossingState = record
    basicState: TBlkCrossingBasicState;
    annulationOld: Boolean;
    note, lockout: string;
    pcEmOpen, pcClosed: Boolean;
    // uzavreni prejezdu z pocitace (tj z technologie), prejezd muze byt uzavren taky z pultu
    zaver: Integer; // pocet bloku, ktere mi daly zaver (pokud > 0, mam zaver; jinak zaver nemam)
    closeStart: TDateTime;
    shs: TList<TBlk>; // seznam souctovych hlasek, kam hlasi prejezd stav
    rcsModules: TList<Cardinal>; // seznam RCS modulu, ktere vyuziva prejezd
  end;

  EPrjNOT = class(Exception);

  TBlkCrossing = class(TBlk)
  const
    _def_crossing_state: TBlkCrossingState = (basicState: disabled; note: ''; lockout: ''; pcEmOpen: false; pcClosed: false;
      zaver: 0;);

    _SECT_RCSICLOSED = 'RCSIclosed';
    _SECT_RCSIOPEN = 'RCSIopen';
    _SECT_RCSICAUTION = 'RCSIcaution';
    _SECT_RCSIANNULATION = 'RCSIannulation';
    _SECT_RCSOCLOSE = 'RCSOclose';
    _SECT_RCSOEMOPEN = 'RCSOemOpen';
    _SECT_RCSOPOSITIVE = 'RCSOpositive';
    _SECT_RCSOPOSITIVE_INVERT = 'RCSOpositiveInv';
    _SECT_RCSOPOSITIVE_FLICK = 'RCSOpositiveFlick';
    _SECT_RCSOBLOCKPOSITIVE = 'RCSOblockPositive';
    _SECT_TRACKS = 'tracks';

    _UZ_UPOZ_MIN = 4; // po 4 minutach uzavreneho prejezdu zobrazim upozorneni na uzavreni prilis dlouho

  private
    m_settings: TBlkCrossingSettings;
    m_state: TBlkCrossingState;

    procedure SetNote(note: string);
    procedure SetLockout(lockout: string);

    function UpdateInputs(): TBlkCrossingBasicState;
    procedure UpdateOutputs();
    procedure UpdateTracks();

    procedure SetEmOpen(state: Boolean);
    procedure SetClosed(state: Boolean);

    procedure SetZaver(zaver: Boolean);
    function GetZaver(): Boolean;

    function TrackClosed(): Boolean;
    function TrackPositiveLight(): Boolean;
    function GetAnnulation(): Boolean;
    function GetPositive(): Boolean;

    procedure MenuUZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZUZClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);

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
    procedure SetSimInputs(uzavreno, vystraha, otevreno: Boolean; SenderPnl: TIdContext; SenderOR: TObject);

    procedure NoteUPO(SenderPnl: TIdContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
      UPO_EscCallback: TNotifyEvent);
    procedure FillRCSModules();

  public
    tracks: TObjectList<TBlkCrossingTrack>;

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

    property state: TBlkCrossingBasicState read m_state.basicState;
    property fullState: TBlkCrossingState read m_state;
    property pcEmOpen: Boolean read m_state.pcEmOpen write SetEmOpen;
    property pcClosed: Boolean read m_state.pcClosed write SetClosed;
    property note: string read m_state.note write SetNote;
    property lockout: string read m_state.lockout write SetLockout;
    property zaver: Boolean read GetZaver write SetZaver;
    property annulation: Boolean read GetAnnulation;
    property positive: Boolean read GetPositive;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    procedure PanelZUZCallBack(Sender: TIdContext; success: Boolean);
    procedure PanelZNOTCallBack(Sender: TIdContext; success: Boolean);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses BlockDb, GetSystems, ownStrUtils, TJCDatabase, TCPServerPanel, RCS, UPO,
  Graphics, TCPAreasRef, Diagnostics, appEv, ownConvert, Config;

constructor TBlkCrossing.Create(index: Integer);
begin
  inherited;

  Self.m_globSettings.typ := btCrossing;
  Self.m_state := Self._def_crossing_state;
  Self.m_state.shs := TList<TBlk>.Create();
  Self.m_state.rcsModules := TList<Cardinal>.Create();
  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();
end;

destructor TBlkCrossing.Destroy();
begin
  Self.m_state.shs.Free();
  Self.m_state.rcsModules.Free();
  Self.tracks.Free();
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
  Self.m_settings.RCSOutputs.positive := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOPOSITIVE);
  if (not Self.m_settings.RCSOutputs.positive.enabled) then // backward compatibility
    Self.m_settings.RCSOutputs.positive := RCSOptionalFromIni(ini_tech, section, _SECT_RCSOBLOCKPOSITIVE, 'RCSObpm', 'RCSObp');
  Self.m_settings.RCSOutputs.positiveInvert := ini_tech.ReadBool(section, _SECT_RCSOPOSITIVE_INVERT, False);
  Self.m_settings.RCSOutputs.positiveFlick := ini_tech.ReadBool(section, _SECT_RCSOPOSITIVE_FLICK, False);

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
  if (Self.m_settings.RCSOutputs.positive.enabled) then
  begin
    ini_tech.WriteString(section, _SECT_RCSOPOSITIVE, Self.m_settings.RCSOutputs.positive.addr.ToString());
    if (Self.m_settings.RCSOutputs.positiveInvert) then
      ini_tech.WriteBool(section, _SECT_RCSOPOSITIVE_INVERT, Self.m_settings.RCSOutputs.positiveInvert);
    if (Self.m_settings.RCSOutputs.positiveFlick) then
      ini_tech.WriteBool(section, _SECT_RCSOPOSITIVE_FLICK, Self.m_settings.RCSOutputs.positiveFlick);
  end;

  if (Self.tracks.Count > 0) then
    ini_tech.WriteInteger(section, _SECT_TRACKS, Self.tracks.Count);
  for var i: Integer := 0 to Self.tracks.Count - 1 do
    Self.tracks[i].Save(ini_tech, section, 'T' + IntToStr(i));
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

  Self.m_state.basicState := TBlkCrossingBasicState.none;
  Self.m_state.annulationOld := Self.annulation;
  Self.Change();
end;

procedure TBlkCrossing.Disable();
begin
  Self.m_state.basicState := disabled;
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
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.Update();
var available: Boolean;
begin
  available := true;
  try
    for var module: Cardinal in Self.m_state.rcsModules do
      available := available and RCSi.IsNonFailedModule(module);
  except
    available := false;
  end;

  if ((not available) and (Self.m_state.basicState <> TBlkCrossingBasicState.disabled)) then
  begin
    Self.m_state.basicState := TBlkCrossingBasicState.disabled;
    JCDb.Cancel(Self);
    Self.Change(true);
  end;

  var new_state: TBlkCrossingBasicState := Self.UpdateInputs();

  if ((Self.m_state.basicState <> new_state) and ((Self.m_state.basicState <> TBlkCrossingBasicState.disabled) or
    (new_state <> TBlkCrossingBasicState.none))) then
  begin
    // kontrola necekaneho otevreni prejezdu, pokud je v JC
    // necekaniy stav = prejezd je pod zaverem a na vstupu se objevi cokoliv jineho, nez "uzavreno"
    if ((Self.zaver) and (Self.m_state.basicState = TBlkCrossingBasicState.closed)) then
    begin
      Self.BottomErrorBroadcast('Ztráta dohledu na přejezdu : ' + Self.m_globSettings.name, 'TECHNOLOGIE');
      JCDb.Cancel(Self);
    end;

    if ((new_state = TBlkCrossingBasicState.none) and (Self.m_state.basicState <> TBlkCrossingBasicState.disabled)) then
    begin
      Self.BottomErrorBroadcast('Porucha přejezdu : ' + Self.m_globSettings.name, 'TECHNOLOGIE');
      JCDb.Cancel(Self);
    end;

    Self.m_state.basicState := new_state;
    Self.Change();
  end;

  if (Self.m_state.annulationOld <> Self.annulation) then
  begin
    Self.m_state.annulationOld := Self.annulation;
    Self.Change();
  end;

  // kontrola prilis dlouho uzavreneho prejezdu
  if ((Self.zaver) or (Self.m_state.pcClosed)) then
  begin
    if (now > Self.m_state.closeStart + EncodeTime(0, _UZ_UPOZ_MIN, 0, 0)) then
    begin
      Self.BottomErrorBroadcast(Self.m_globSettings.name + ' uzavřen déle, jak ' + IntToStr(_UZ_UPOZ_MIN) + ' min',
          'VAROVÁNÍ');
      Self.m_state.closeStart := now;
    end;
  end;

  Self.UpdateTracks();
  inherited Update();
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

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.Change(now: Boolean = false);
begin
  inherited;
  Self.UpdateOutputs();

  try
    for var sh: TBlk in Self.m_state.shs do
      sh.Change();
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.UpdateInputs(): TBlkCrossingBasicState;
var tmpInputs: record
      closed: Boolean;
      open: Boolean;
      caution: Boolean;
    end;
begin
  // get data from RCS
  try
    tmpInputs.closed := (Self.m_settings.RCSInputs.closed.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.closed.addr) = isOn);
    tmpInputs.open := (Self.m_settings.RCSInputs.open.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.open.addr) = isOn);
    tmpInputs.caution := (Self.m_settings.RCSInputs.caution.enabled) and (RCSi.GetInput(Self.m_settings.RCSInputs.caution.addr) = isOn);
  except
    // prejezd prejde do poruchoveho stavu
    tmpInputs.closed := false;
    tmpInputs.open := false;
    tmpInputs.caution := false;
  end;

  if (tmpInputs.closed) then
    Exit(TBlkCrossingBasicState.closed);
  if (tmpInputs.caution) then
    Exit(TBlkCrossingBasicState.caution);
  if (tmpInputs.open) then
    Exit(TBlkCrossingBasicState.open);

  // without data
  Result := none;
end;

procedure TBlkCrossing.UpdateOutputs();
begin
  try
    if (Self.m_settings.RCSOutputs.close.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.close.addr,
        ownConvert.BoolToInt(((Self.m_state.pcClosed) or (Self.zaver) or (Self.TrackClosed())) and
                             (not Self.m_state.pcEmOpen)));
    if (Self.m_settings.RCSOutputs.emOpen.enabled) then
      RCSi.SetOutput(Self.m_settings.RCSOutputs.emOpen.addr, ownConvert.BoolToInt(Self.m_state.pcEmOpen));
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
  if (state) then
  begin
    if (Self.pcEmOpen) then
      raise EPrjNOT.Create('Prejezd nouzove otevren, nelze uzavrit!');
    Self.m_state.closeStart := now;
  end;

  Self.m_state.pcClosed := state;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossing.MenuUZClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOUZClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPOUZClick(SenderPnl);
  end;
end;

procedure TBlkCrossing.MenuZUZClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOZUZClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPOZUZClick(SenderPnl);
  end;
end;

procedure TBlkCrossing.MenuNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPONOTClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPONOTClick(SenderPnl);
  end;
end;

procedure TBlkCrossing.MenuZNOTClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOZNOTClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPOZNOTClick(SenderPnl);
  end;
end;

procedure TBlkCrossing.UPOUZClick(Sender: TObject);
begin
  Self.pcClosed := true;
end;

procedure TBlkCrossing.UPOZUZClick(Sender: TObject);
begin
  PanelServer.ConfirmationSequence(TIdContext(Sender), Self.PanelZUZCallBack,
    (TPanelConnData(TIdContext(Sender).data).UPO_ref as TArea), 'Zrušení uzavření přejezdu',
    GetObjsList(Self), nil);
end;

procedure TBlkCrossing.UPONOTClick(Sender: TObject);
begin
  PanelServer.ConfirmationSequence(TIdContext(Sender), Self.PanelZNOTCallBack,
    (TPanelConnData(TIdContext(Sender).data).UPO_ref as TArea), 'Nouzové otevření přejezdu',
    GetObjsList(Self), nil);
end;

procedure TBlkCrossing.UPOZNOTClick(Sender: TObject);
begin
  Self.pcEmOpen := false;
end;

procedure TBlkCrossing.MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.m_state.note);
end;

procedure TBlkCrossing.MenuAdminZavreno(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetSimInputs(true, false, false, SenderPnl, SenderOR);
end;

procedure TBlkCrossing.MenuAdminOtevreno(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetSimInputs(false, false, true, SenderPnl, SenderOR);
end;

procedure TBlkCrossing.MenuAdminVystraha(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.SetSimInputs(false, true, false, SenderPnl, SenderOR);
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

procedure TBlkCrossing.SetSimInputs(uzavreno, vystraha, otevreno: Boolean; SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    if (Self.m_settings.RCSInputs.closed.enabled) then
      RCSi.SetInput(Self.m_settings.RCSInputs.closed.addr, ownConvert.BoolToInt(uzavreno));
    if (Self.m_settings.RCSInputs.caution.enabled) then
      RCSi.SetInput(Self.m_settings.RCSInputs.caution.addr, ownConvert.BoolToInt(vystraha));
    if (Self.m_settings.RCSInputs.open.enabled) then
      RCSi.SetInput(Self.m_settings.RCSInputs.open.addr, ownConvert.BoolToInt(otevreno));
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).shortName,
      'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkCrossing.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  if (Self.m_state.basicState <> TBlkCrossingBasicState.disabled) then
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

  Result := Result + 'STIT,';

  // pokud mame knihovnu simulator, muzeme ridit stav useku
  // DEBUG nastroj
  if (RCSi.simulation) then
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
procedure TBlkCrossing.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
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
    Self.MenuSTITClick(SenderPnl, SenderOR)
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
      Self.m_state.closeStart := now;
      Self.SetEmOpen(false);

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

procedure TBlkCrossing.NoteUPO(SenderPnl: TIdContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
  UPO_EscCallback: TNotifyEvent);
var UPO: TUPOItems;
  item: TUPOItem;
  lines: TStrings;
begin
  UPO := TList<TUPOItem>.Create;
  try
    if (Self.note <> '') then
    begin
      item[0] := GetUPOLine('ŠTÍTEK ' + Self.m_globSettings.name, taCenter, clBlack, clTeal);
      lines := GetLines(Self.note, _UPO_LINE_LEN);

      try
        item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
        if (lines.Count > 1) then
          item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
      finally
        lines.Free();
      end;

      UPO.Add(item);
    end;

    PanelServer.UPO(SenderPnl, UPO, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
  finally
    UPO.Free();
  end;
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
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.closed.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.closed.addr.board);

  if (Self.m_settings.RCSInputs.open.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.open.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.open.addr.board);

  if (Self.m_settings.RCSInputs.caution.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.caution.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.caution.addr.board);

  if (Self.m_settings.RCSInputs.annulation.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSInputs.annulation.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSInputs.annulation.addr.board);

  if (Self.m_settings.RCSInputs.closed.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.close.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.close.addr.board);

  if (Self.m_settings.RCSOutputs.emOpen.enabled) then
    if (not Self.m_state.rcsModules.Contains(Self.m_settings.RCSOutputs.emOpen.addr.board)) then
      Self.m_state.rcsModules.Add(Self.m_settings.RCSOutputs.emOpen.addr.board);

  Self.m_state.rcsModules.Sort();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossing.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  bg := clBlack;
  if (Self.note <> '') then
    bg := clTeal;

  if (diag.showZaver) then
  begin
    if (Self.zaver) then
      bg := clGreen
    else if (Self.TrackClosed()) then
      bg := clBlue;
  end;

  if (Self.pcEmOpen) then
    fg := clRed
  else if (Self.pcClosed) then
    fg := clWhite
  else
    fg := $A0A0A0;

  case (Self.state) of
    TBlkCrossingBasicState.disabled:
      begin
        fg := bg;
        bg := clFuchsia;
      end;

    TBlkCrossingBasicState.none:
      begin
        fg := bg;
        bg := clRed;
      end;
  end;

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) + ';0;' +
    IntToStr(Integer(Self.state)) + ';';
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

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkCrossing.GetPtState(json: TJsonObject);
begin
  case (Self.m_state.basicState) of
    TBlkCrossingBasicState.disabled:
      json['state'] := 'disabled';
    TBlkCrossingBasicState.none:
      json['state'] := 'none';
    TBlkCrossingBasicState.open:
      json['state'] := 'opened';
    TBlkCrossingBasicState.caution:
      json['state'] := 'caution';
    TBlkCrossingBasicState.closed:
      json['state'] := 'closed';
  end;

  json['annulation'] := Self.annulation;
  json['note'] := Self.m_state.note;
  json['lockout'] := Self.m_state.lockout;
  json['pcEmOpen'] := Self.m_state.pcEmOpen;
  json['pcClosed'] := Self.m_state.pcClosed;
  json['locks'] := Self.m_state.zaver;
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

function TBlkCrossing.GetAnnulation(): Boolean;
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

function TBlkCrossing.GetPositive(): Boolean;
begin
  Result := (Self.state = TBlkCrossingBasicState.open) and (Self.TrackPositiveLight()) and (not Self.pcClosed) and (not Self.pcEmOpen);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
