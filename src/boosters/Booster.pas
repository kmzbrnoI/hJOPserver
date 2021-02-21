unit Booster;

{
  Definice a obsluha technologie zesilovace (napr. SPAX).
}

interface

uses IniFiles, TechnologieRCS, SysUtils, Generics.Defaults;

type
  TBoosterSignal = (undef = -1, error = 0, ok = 1);
  TBoosterChangeEvent = procedure(Sender: TObject; state: TBoosterSignal) of object;

  TBoosterSettings = record
    name: string;

    RCS: record
      overload: TRCSAddr;
      power: TRCSAddr;
      DCC: TRCSAddr; // DCC input; DCC nemusi byt detekovano, to se pozna tak, ze .board = 0
    end;

    id: string;
  end;

  TBooster = class
  private
    m_settings: TBoosterSettings;

    overloadOld, powerOld, DCCOld: TBoosterSignal; // old states (used to call events)

    m_OnPowerChange: TBoosterChangeEvent;
    m_OnOverloadChange: TBoosterChangeEvent;
    m_OnDCCChange: TBoosterChangeEvent;

    function GetOverload(): TBoosterSignal;
    function GetPower(): TBoosterSignal;
    function GetDCC(): TBoosterSignal;

    function GetRCSPresent(): Boolean;
    function GetDCCDetection(): Boolean;
    function GetOverloadDetection(): Boolean;
    function GetPowerDetection(): Boolean;

  public

    constructor Create(); overload;
    constructor Create(var ini: TMemIniFile; const section: string); overload;

    procedure Update(); // update data; events are controlled only here

    procedure LoadDataFromFile(var ini: TMemIniFile; const section: string);
    procedure SaveDataToFile(var ini: TMemIniFile; const section: string);

    property overload: TBoosterSignal read GetOverload;
    property power: TBoosterSignal read GetPower;
    property DCC: TBoosterSignal read GetDCC;

    property rcsPresent: Boolean read GetRCSPresent;
    property isOverloadDetection: Boolean read GetOverloadDetection;
    property isPowerDetection: Boolean read GetPowerDetection;
    property isDCCdetection: Boolean read GetDCCDetection;

    property settings: TBoosterSettings read m_settings write m_settings;

    property id: string read m_settings.id;
    property name: string read m_settings.name;

    property OnPowerChange: TBoosterChangeEvent read m_OnPowerChange write m_OnPowerChange;
    property OnOverloadChange: TBoosterChangeEvent read m_OnOverloadChange write m_OnOverloadChange;
    property OnDCCChange: TBoosterChangeEvent read m_OnDCCChange write m_OnDCCChange;

    class function IdComparer(): IComparer<TBooster>;
  end;

implementation

uses GetSystems, fMain, RCS, TechnologieTrakce, Trakce;

{
  Format datoveho souboru: .ini soubor, kazdy SPAX ma svou sekci
  [id]
  name
  class
  zkr_RCS
  zkr_port
  nap_RCS
  nap_port
  dcc_RCS
  dcc_port
}

/// /////////////////////////////////////////////////////////////////////////////

constructor TBooster.Create();
begin
  inherited;

  Self.overloadOld := TBoosterSignal.undef;
  Self.powerOld := TBoosterSignal.undef;
  Self.DCCOld := TBoosterSignal.undef;
end;

constructor TBooster.Create(var ini: TMemIniFile; const section: string);
begin
  Self.Create();
  Self.LoadDataFromFile(ini, section);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBooster.Update();
var state: TBoosterSignal;
begin
  // update DCC
  state := Self.GetDCC();
  if (state <> Self.DCCOld) then
  begin
    if (Assigned(Self.OnDCCChange)) then
      Self.OnDCCChange(Self, state);
    Self.DCCOld := state;
  end;

  state := Self.GetPower();
  if (state <> Self.powerOld) then
  begin
    if (Assigned(Self.OnPowerChange)) then
      Self.OnPowerChange(Self, state);
    Self.powerOld := state;
  end;

  state := Self.GetOverload();
  if (state <> Self.overloadOld) then
  begin
    if (Assigned(Self.OnOverloadChange)) then
      Self.OnOverloadChange(Self, state);
    Self.overloadOld := state;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBooster.LoadDataFromFile(var ini: TMemIniFile; const section: string);
begin
  Self.m_settings.id := section;
  Self.m_settings.name := ini.ReadString(section, 'name', 'booster');

  Self.m_settings.RCS.overload.board := ini.ReadInteger(section, 'zkr_module', 0);
  if (Self.m_settings.RCS.overload.board = 0) then
    Self.m_settings.RCS.overload.board := ini.ReadInteger(section, 'zkr_mtb', 0);
  Self.m_settings.RCS.overload.port := ini.ReadInteger(section, 'zkr_port', 0);

  Self.m_settings.RCS.power.board := ini.ReadInteger(section, 'nap_module', 0);
  if (Self.m_settings.RCS.power.board = 0) then
    Self.m_settings.RCS.power.board := ini.ReadInteger(section, 'nap_mtb', 0);
  Self.m_settings.RCS.power.port := ini.ReadInteger(section, 'nap_port', 0);

  Self.m_settings.RCS.DCC.board := ini.ReadInteger(section, 'dcc_module', 0);
  if (Self.m_settings.RCS.DCC.board = 0) then
    Self.m_settings.RCS.DCC.board := ini.ReadInteger(section, 'dcc_mtb', 0);
  Self.m_settings.RCS.DCC.port := ini.ReadInteger(section, 'dcc_port', 0);

  if (Self.isPowerDetection) then
    RCSi.SetNeeded(Self.m_settings.RCS.power.board);
  if (Self.isOverloadDetection) then
    RCSi.SetNeeded(Self.m_settings.RCS.overload.board);
end;

procedure TBooster.SaveDataToFile(var ini: TMemIniFile; const section: string);
begin
  ini.WriteString(section, 'name', Self.m_settings.name);

  if (Self.isOverloadDetection) then
  begin
    ini.WriteInteger(section, 'zkr_module', Self.m_settings.RCS.overload.board);
    ini.WriteInteger(section, 'zkr_port', Self.m_settings.RCS.overload.port);
  end;

  if (Self.isPowerDetection) then
  begin
    ini.WriteInteger(section, 'nap_module', Self.m_settings.RCS.power.board);
    ini.WriteInteger(section, 'nap_port', Self.m_settings.RCS.power.port);
  end;

  if (Self.isDCCdetection) then
  begin
    ini.WriteInteger(section, 'dcc_module', Self.m_settings.RCS.DCC.board);
    ini.WriteInteger(section, 'dcc_port', Self.m_settings.RCS.DCC.port);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBooster.GetOverload(): TBoosterSignal;
var val: TRCSInputState;
begin
  if ((not RCSi.ready) or (not RCSi.Started)) then
    Exit(TBoosterSignal.undef);

  // if not a power, not a overload
  if (Self.power = TBoosterSignal.error) then
    Exit(TBoosterSignal.undef);

  if (not Self.isOverloadDetection) then
    Exit(TBoosterSignal.ok);

  try
    val := RCSi.GetInput(Self.m_settings.RCS.overload);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if (val = isOn) then
    Result := TBoosterSignal.error
  else
    Result := TBoosterSignal.ok;
end;

function TBooster.GetPower(): TBoosterSignal;
var val: TRCSInputState;
begin
  if ((not RCSi.ready) or (not RCSi.Started)) then
    Exit(TBoosterSignal.undef);

  if (not Self.isPowerDetection) then
    Exit(TBoosterSignal.ok);

  try
    val := RCSi.GetInput(Self.m_settings.RCS.power);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if (val = isOn) then
    Result := TBoosterSignal.error
  else
    Result := TBoosterSignal.ok;
end;

function TBooster.GetDCC(): TBoosterSignal;
var val: TRCSInputState;
begin
  if ((not RCSi.ready) or (not RCSi.Started)) then
    Exit(TBoosterSignal.undef);

  if (not Self.isDCCdetection) then
  begin
    case (TrakceI.TrackStatusSafe()) of
      TTrkStatus.tsUnknown:
        Exit(TBoosterSignal.undef);
      TTrkStatus.tsOff:
        Exit(TBoosterSignal.error);
      TTrkStatus.tsOn:
        Exit(TBoosterSignal.ok);
      TTrkStatus.tsProgramming:
        Exit(TBoosterSignal.error);
    else
      Exit(TBoosterSignal.undef);
    end;
  end;

  try
    val := RCSi.GetInput(Self.m_settings.RCS.DCC);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if (val = isOn) then
    Result := TBoosterSignal.error
  else
    Result := TBoosterSignal.ok;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBooster.GetRCSPresent(): Boolean;
begin
  Result := (((not Self.isOverloadDetection) or RCSi.IsModule(Self.m_settings.RCS.overload.board)) and
    ((not Self.isPowerDetection) or RCSi.IsModule(Self.m_settings.RCS.power.board)) and
    ((not Self.isDCCdetection) or RCSi.IsModule(Self.m_settings.RCS.DCC.board)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBooster.GetDCCDetection(): Boolean;
begin
  Result := (Self.m_settings.RCS.DCC.board > 0);
end;

function TBooster.GetOverloadDetection(): Boolean;
begin
  Result := (Self.m_settings.RCS.overload.board > 0);
end;

function TBooster.GetPowerDetection(): Boolean;
begin
  Result := (Self.m_settings.RCS.power.board > 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBooster.IdComparer(): IComparer<TBooster>;
begin
  Result := TComparer<TBooster>.Construct(
    function(const Left, Right: TBooster): Integer
    begin
      Result := CompareStr(Left.id, Right.id, loUserLocale);
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
