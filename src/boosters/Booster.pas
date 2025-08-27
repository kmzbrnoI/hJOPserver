unit Booster;

{
  Definice a obsluha technologie zesilovace (napr. SPAX).
}

interface

uses IniFiles, RCSc, SysUtils, Generics.Defaults;

type
  TBoosterSignal = (undef = -1, error = 0, ok = 1);
  TBoosterChangeEvent = procedure(Sender: TObject; state: TBoosterSignal) of object;

  TBoosterRCSSignal = record
    // disabled input = (addr.module = 0)
    addr: TRCSAddr;
    reversed: Boolean;
  end;

  TBoosterSettings = record
    id: string;
    name: string;

    rcs: record
      overload: TBoosterRCSSignal;
      power: TBoosterRCSSignal;
      DCC: TBoosterRCSSignal;
    end;
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

uses GetSystems, fMain, RCSIFace, TrakceC, TrakceIFace, Config;

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

  Self.m_settings.rcs.overload.addr := RCSFromIni(ini, section, 'short', 'zkr_module', 'zkr_port');
  Self.m_settings.rcs.overload.reversed := ini.ReadBool(section, 'shortReversed', false);
  Self.m_settings.rcs.power.addr := RCSFromIni(ini, section, 'power', 'nap_module', 'nap_port');
  Self.m_settings.rcs.power.reversed := ini.ReadBool(section, 'powerReversed', false);
  Self.m_settings.rcs.DCC.addr := RCSFromIni(ini, section, 'dcc', 'dcc_module', 'dcc_port');
  Self.m_settings.rcs.DCC.reversed := ini.ReadBool(section, 'dccReversed', false);

  if (Self.isPowerDetection) then
    RCSi.SetNeeded(Self.m_settings.rcs.power.addr.module);
  if (Self.isOverloadDetection) then
    RCSi.SetNeeded(Self.m_settings.rcs.overload.addr.module);
  if (Self.isDCCdetection) then
    RCSi.SetNeeded(Self.m_settings.rcs.DCC.addr.module);
end;

procedure TBooster.SaveDataToFile(var ini: TMemIniFile; const section: string);
begin
  ini.WriteString(section, 'name', Self.m_settings.name);

  if (Self.isOverloadDetection) then
  begin
    ini.WriteString(section, 'short', Self.m_settings.rcs.overload.addr.ToString());
    if (Self.m_settings.rcs.overload.reversed) then
      ini.WriteBool(section, 'shortReversed', true);
  end;
  if (Self.isPowerDetection) then
  begin
    ini.WriteString(section, 'power', Self.m_settings.rcs.power.addr.ToString());
    if (Self.m_settings.rcs.power.reversed) then
      ini.WriteBool(section, 'powerReversed', true);
  end;
  if (Self.isDCCdetection) then
  begin
    ini.WriteString(section, 'dcc', Self.m_settings.rcs.DCC.addr.ToString());
    if (Self.m_settings.rcs.DCC.reversed) then
      ini.WriteBool(section, 'dccReversed', true);
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
    val := RCSi.GetInput(Self.m_settings.rcs.overload.addr);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if ((val = isOn) xor (Self.m_settings.rcs.overload.reversed)) then
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
    val := RCSi.GetInput(Self.m_settings.rcs.power.addr);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if ((val = isOn) xor (Self.m_settings.rcs.power.reversed)) then
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
    case (trakce.TrackStatusSafe()) of
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
    val := RCSi.GetInput(Self.m_settings.rcs.DCC.addr);
  except
    Exit(TBoosterSignal.undef);
  end;

  if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
    Result := TBoosterSignal.undef
  else if ((val = isOn) xor (Self.m_settings.rcs.DCC.reversed)) then
    Result := TBoosterSignal.error
  else
    Result := TBoosterSignal.ok;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBooster.GetRCSPresent(): Boolean;
begin
  Result := (((not Self.isOverloadDetection) or RCSi.IsModule(Self.m_settings.rcs.overload.addr.module)) and
    ((not Self.isPowerDetection) or RCSi.IsModule(Self.m_settings.rcs.power.addr.module)) and
    ((not Self.isDCCdetection) or RCSi.IsModule(Self.m_settings.rcs.DCC.addr.module)));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBooster.GetDCCDetection(): Boolean;
begin
  Result := (Self.m_settings.rcs.DCC.addr.module > 0);
end;

function TBooster.GetOverloadDetection(): Boolean;
begin
  Result := (Self.m_settings.rcs.overload.addr.module > 0);
end;

function TBooster.GetPowerDetection(): Boolean;
begin
  Result := (Self.m_settings.rcs.power.addr.module > 0);
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
