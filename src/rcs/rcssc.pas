unit RCSsc;

interface

uses Generics.Defaults, RCSc, RCSIFace, IniFiles, Generics.Collections, Classes,
  RCSErrors, Logging;

type
  // Single RCS address with system too
  TRCSsAddr = record
    system: Cardinal;
    module: Cardinal;
    port: Byte;
    class operator Equal(a, b: TRCSsAddr): Boolean;
    procedure Load(str: string);
    function ToString(): string;
  end;

  TRCSsAddrOptional = record
    addr: TRCSsAddr;
    enabled: Boolean;
  end;

  EInvalidSystem = class(RCSException)
  public
    constructor Create(system: Cardinal);
  end;

  // Access to multiple Railroad Control Systems
  TRCSs = class
  private const
    _INIFILE_SECTNAME = 'RCS';
    _DEFAULT_CONFIG_PATH = 'lib-conf';

  public const
    _RCSS_COUNT = 4;
    _RCSS_MAX = _RCSS_COUNT-1;

  private
    m_rcss: array[0.._RCSS_MAX] of TRCS;
    mLibDir: string;
    mConfigDir: string;

    function IsGeneralError(): Boolean;
    procedure Log(msg: string; level: TLogLevel);
    function GetItem(i: Integer): TRCS;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure LoadLib(system: Cardinal; libFn: string);

    function NoExStarted(): Boolean;
    function NoExOpened(): Boolean;

    procedure SetNeeded(system: Cardinal; module: Cardinal; state: Boolean = true); overload;
    procedure SetNeeded(addr: TRCSsAddr; state: Boolean = true); overload;
    function GetNeeded(system: Cardinal; module: Cardinal): Boolean;

    procedure LoadFromFile(ini: TMemIniFile);
    procedure SaveToFile(ini: TMemIniFile);

    procedure SetOutput(addr: TRCSsAddr; state: Integer); overload;
    procedure SetOutput(addr: TRCSsAddr; state: TRCSOutputState); overload;
    procedure SetOutputs(addrs: TList<TRCSsAddr>; state: Integer); overload;
    procedure SetOutputs(addrs: TList<TRCSsAddr>; state: TRCSOutputState); overload;

    function GetInput(addr: TRCSsAddr): TRCSInputState; overload;

    procedure SetInput(addr: TRCSsAddr; state: Integer); overload;
    procedure SetInputs(addrs: TList<TRCSsAddr>; state: Integer); overload;

    function GetOutput(addr: TRCSsAddr): Integer; overload;
    function GetOutputState(addr: TRCSsAddr): TRCSOutputState; overload;

    procedure AddInputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
    procedure RemoveInputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);

    procedure AddOutputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
    procedure RemoveOutputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);

    function GetModuleInputsCountSafe(system: Cardinal; module: Cardinal): Cardinal;
    function GetModuleOutputsCountSafe(system: Cardinal; module: Cardinal): Cardinal;

    procedure InputSim(); // nastavit simulovane vstupy (koncove polohy vyhybek atp.)
    procedure TrainOccupySim(); // nastavit RCS vstupy tak, aby useky, ve kterych je souprava, byly obsazene

    property generalError: Boolean read IsGeneralError;

    class function RCSsAddr(system: Cardinal; module: Cardinal; port: Byte): TRCSsAddr;
    class function RCSsOptionalAddr(system: Cardinal; module: Cardinal; port: Byte): TRCSsAddrOptional; overload;
    class function RCSsOptionalAddrDisabled(): TRCSsAddrOptional; overload;

    property items[index: Integer]: TRCS read GetItem; default;

    // events
//    property AfterClose: TNotifyEvent read fAfterClose write fAfterClose;

//    property OnReady: TRCSReadyEvent read fOnReady write fOnReady;
//    property ready: Boolean read aReady;
    property libDir: string read mLibDir;
    property configDir: string read mConfigDir;
//    property maxModuleAddr: Cardinal read GetMaxModuleAddr;
//    property maxModuleAddrSafe: Cardinal read GetMaxModuleAddrSafe;
  end;

var
  RCSs: TRCSs;

function RCSAddrComparer(): IComparer<TRCSsAddr>;

////////////////////////////////////////////////////////////////////////////////

implementation

uses SysUtils, fMain;

constructor EInvalidSystem.Create(system: Cardinal);
begin
  inherited Create('Invalid RCS system: '+IntToStr(system));
end;

constructor TRCSs.Create();
begin
  inherited;

  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i] := TRCS.Create(i);
end;

destructor TRCSs.Destroy();
begin
  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i].Free();

  inherited;
end;

procedure TRCSs.Log(msg: string; level: TLogLevel);
begin
  Logging.Log(msg, level, lsRCS);
end;

////////////////////////////////////////////////////////////////////////////////

class operator TRCSsAddr.Equal(a, b: TRCSsAddr): Boolean;
begin
  Result := ((a.system = b.system) and (a.module = b.module) and (a.port = b.port));
end;

function TRCSsAddr.ToString(): string;
begin
  Result := IntToStr(system) + ':' + IntToStr(module) + ':' + IntToStr(port);
end;

procedure TRCSsAddr.Load(str: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStrings([':'], [], PChar(str), strs);
    if (strs.Count = 2) then
    begin
      system := 0;
      module := StrToInt(strs[0]);
      port := StrToInt(strs[1]);
    end else if (strs.Count = 3) then begin
      system := StrToInt(strs[0]);
      module := StrToInt(strs[1]);
      port := StrToInt(strs[2]);
    end else begin
      raise Exception.Create('Unable to load RCS: '+str);
    end;
  finally
    strs.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function RCSAddrComparer(): IComparer<TRCsSAddr>;
begin
  Result := TComparer<TRCsSAddr>.Construct(
    function(const Left, Right: TRCsSAddr): Integer
    begin
      if (Left.system < Right.system) then
        Exit(-1);
      if (Left.system > Right.system) then
        Exit(1);
      if (Left.module < Right.module) then
        Exit(-1);
      if (Left.module > Right.module) then
        Exit(1);
      if (Left.port < Right.port) then
        Exit(-1);
      if (Left.port > Right.port) then
        Exit(1);
      Result := 0;
    end);
end;

////////////////////////////////////////////////////////////////////////////////

function TRCSs.GetItem(i: Integer): TRCS;
begin
  if (i > _RCSS_MAX) then
    raise EInvalidSystem.Create(i);
  Result := Self.m_rcss[i];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSs.LoadLib(system: Cardinal; libFn: string);
begin
  if (system > _RCSS_MAX) then
    raise EInvalidSystem.Create(system);

  Self.m_rcss[system].LoadLib(Self.libDir + '\' + libFn, Self.configDir + '\rcs'+IntToStr(system) + '_' + ChangeFileExt(ExtractFileName(libFn), '.ini'));
end;

function TRCSs.IsGeneralError(): Boolean;
begin
  for var i: Integer := 0 to _RCSS_MAX do
    if (Self.m_rcss[i].generalError) then
      Exit(True);
  Result := False;
end;

function TRCSs.NoExStarted(): Boolean;
begin
  for var i: Integer := 0 to _RCSS_MAX do
    if ((Self.m_rcss[i].libLoaded) and (not Self.m_rcss[i].NoExStarted())) then
      Exit(False);
  Result := True;
end;

function TRCSs.NoExOpened(): Boolean;
begin
  for var i: Integer := 0 to _RCSS_MAX do
    if ((Self.m_rcss[i].libLoaded) and (not Self.m_rcss[i].NoExOpened())) then
      Exit(False);
  Result := True;
end;

procedure TRCSs.SetNeeded(system: Cardinal; module: Cardinal; state: Boolean = true);
begin
  if (system > _RCSS_MAX) then
    raise EInvalidSystem.Create(system);
  Self.m_rcss[system].SetNeeded(module, state);
end;

procedure TRCSs.SetNeeded(addr: TRCSsAddr; state: Boolean = true);
begin
  Self.SetNeeded(addr.system, addr.module, state);
end;

function TRCSs.GetNeeded(system: Cardinal; module: Cardinal): Boolean;
begin
  if (system > _RCSS_MAX) then
    raise EInvalidSystem.Create(system);
  Result := Self.m_rcss[system].GetNeeded(module);
end;

procedure TRCSs.LoadFromFile(ini: TMemIniFile);
begin
  Self.mLibDir := ini.ReadString(_INIFILE_SECTNAME, 'dir', '.');
  Self.mConfigDir := ini.ReadString(_INIFILE_SECTNAME, 'configDir', _DEFAULT_CONFIG_PATH);

  for var i: Integer := 0 to _RCSS_MAX do
  begin
    var dllFile: string := ini.ReadString(_INIFILE_SECTNAME, 'lib'+IntToStr(i), '');
    if ((i = 0) and (dllFile = '')) then
      dllFile := ini.ReadString(_INIFILE_SECTNAME, 'lib', ''); // backward-compatibility

    if (dllFile <> '') then
    begin
      try
        Self.LoadLib(i, dllFile);
      except
        on E: Exception do
        begin
          Self.m_rcss[i].LogFMainStatusError('Nelze naèíst knihovnu ' + Self.mLibDir + '\' + dllFile + ': ' + E.Message);
          Self.m_rcss[i].Log('Nelze naèíst knihovnu ' + Self.mLibDir + '\' + dllFile + ': ' + E.Message, llError);
        end;
      end;
    end;
  end;
end;

procedure TRCSs.SaveToFile(ini: TMemIniFile);
begin
  ini.DeleteKey(_INIFILE_SECTNAME, 'lib'); // old name
  for var i: Integer := 0 to _RCSS_MAX do
    ini.WriteString(_INIFILE_SECTNAME, 'lib'+IntToStr(i), ExtractFileName(Self.m_rcss[i].lib));
end;

procedure TRCSs.SetOutput(addr: TRCSsAddr; state: Integer);
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Self.m_rcss[addr.system].SetOutput(addr.module, addr.port, state);
end;

procedure TRCSs.SetOutput(addr: TRCSsAddr; state: TRCSOutputState);
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Self.m_rcss[addr.system].SetOutput(addr.module, addr.port, state);
end;

procedure TRCSs.SetOutputs(addrs: TList<TRCSsAddr>; state: Integer);
begin
  for var addr: TRCSsAddr in addrs do
    if (addr.system > _RCSS_MAX) then
      raise EInvalidSystem.Create(addr.system);

  for var addr: TRCSsAddr in addrs do
    Self.m_rcss[addr.system].SetOutput(addr.module, addr.port, state);
end;

procedure TRCSs.SetOutputs(addrs: TList<TRCSsAddr>; state: TRCSOutputState);
begin
  for var addr: TRCSsAddr in addrs do
    if (addr.system > _RCSS_MAX) then
      raise EInvalidSystem.Create(addr.system);

  for var addr: TRCSsAddr in addrs do
    Self.m_rcss[addr.system].SetOutput(addr.module, addr.port, state);
end;

function TRCSs.GetInput(addr: TRCSsAddr): TRCSInputState;
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Result := Self.m_rcss[addr.system].GetInput(addr.module, addr.port);
end;

procedure TRCSs.SetInput(addr: TRCSsAddr; state: Integer);
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Self.m_rcss[addr.system].SetInput(addr.module, addr.port, state);
end;

procedure TRCSs.SetInputs(addrs: TList<TRCSsAddr>; state: Integer);
begin
  for var addr: TRCSsAddr in addrs do
    if (addr.system > _RCSS_MAX) then
      raise EInvalidSystem.Create(addr.system);

  for var addr: TRCSsAddr in addrs do
    Self.m_rcss[addr.system].SetInput(addr.module, addr.port, state);
end;

function TRCSs.GetOutput(addr: TRCSsAddr): Integer;
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Result := Self.m_rcss[addr.system].GetOutput(addr.module, addr.port);
end;

function TRCSs.GetOutputState(addr: TRCSsAddr): TRCSOutputState;
begin
  if (addr.system > _RCSS_MAX) then
    raise EInvalidSystem.Create(addr.system);
  Result := Self.m_rcss[addr.system].GetOutputState(addr.module, addr.port);
end;

procedure TRCSs.AddInputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
begin
  // TODO
end;

procedure TRCSs.RemoveInputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);
begin
  // TODO
end;

procedure TRCSs.AddOutputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
begin
  // TODO
end;

procedure TRCSs.RemoveOutputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);
begin
  // TODO
end;

function TRCSs.GetModuleInputsCountSafe(system: Cardinal; module: Cardinal): Cardinal;
begin
  if (system > _RCSS_MAX) then
    Exit(TRCS._MODULE_DEFAULT_IO);
  Result := Self.m_rcss[system].GetModuleInputsCountSafe(module);
end;

function TRCSs.GetModuleOutputsCountSafe(system: Cardinal; module: Cardinal): Cardinal;
begin
  if (system > _RCSS_MAX) then
    Exit(TRCS._MODULE_DEFAULT_IO);
  Result := Self.m_rcss[system].GetModuleOutputsCountSafe(module);
end;

procedure TRCSs.InputSim();
begin
  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i].InputSim();
end;

procedure TRCSs.TrainOccupySim();
begin
  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i].TrainOccupySim();
end;

class function TRCSs.RCSsAddr(system: Cardinal; module: Cardinal; port: Byte): TRCSsAddr;
begin
  Result.system := system;
  Result.module := module;
  Result.port := port;
end;

class function TRCSs.RCSsOptionalAddr(system: Cardinal; module: Cardinal; port: Byte): TRCSsAddrOptional;
begin
  Result.enabled := True;
  Result.addr := RCSsAddr(system, module, port);
end;

class function TRCSs.RCSsOptionalAddrDisabled(): TRCSsAddrOptional;
begin
  Result.enabled := False;
  Result.addr := RCSsAddr(0, 0, 0);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  RCSs := TRCSs.Create();

finalization
  // Free in hJOPserver.dpr, because we must gurantee preload gets destructed after all shared libraries

end.

