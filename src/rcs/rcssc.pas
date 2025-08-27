unit RCSsc;

interface

uses Generics.Defaults, RCSc, RCSIFace, IniFiles, Generics.Collections, Classes,
  RCSErrors;

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
  public const
    _RCSS_COUNT = 4;
    _RCSS_MAX = _RCSS_COUNT-1;

  private
    m_rcss: array[0.._RCSS_MAX] of TRCS;

    function IsGeneralError(): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;

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

    // events
//    property AfterClose: TNotifyEvent read fAfterClose write fAfterClose;

//    property OnReady: TRCSReadyEvent read fOnReady write fOnReady;
//    property ready: Boolean read aReady;
//    property libDir: string read fLibDir;
//    property configDir: string read mConfigDir;
//    property maxModuleAddr: Cardinal read GetMaxModuleAddr;
//    property maxModuleAddrSafe: Cardinal read GetMaxModuleAddrSafe;
  end;

var
  RCSs: TRCSs;

function RCSAddrComparer(): IComparer<TRCSsAddr>;

////////////////////////////////////////////////////////////////////////////////

implementation

uses SysUtils;

constructor EInvalidSystem.Create(system: Cardinal);
begin
  inherited Create('Invalid RCS system: '+IntToStr(system));
end;

constructor TRCSs.Create();
begin
  inherited;

  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i] := TRCS.Create();
end;

destructor TRCSs.Destroy();
begin
  for var i: Integer := 0 to _RCSS_MAX do
    Self.m_rcss[i].Free();

  inherited;
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

function TRCSs.IsGeneralError(): Boolean;
begin
  for var i: Integer := 0 to _RCSS_MAX do
    if (Self.m_rcss[i].generalError) then
      Exit(True);
  Result := False;
end;

function TRCSs.NoExStarted(): Boolean;
begin
  // TODO
end;

function TRCSs.NoExOpened(): Boolean;
begin
  // TODO
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

end;

procedure TRCSs.SaveToFile(ini: TMemIniFile);
begin

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
q
