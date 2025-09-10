unit RCSc;

{
  Technologie RCS: rozhrani pro pouzivani Railroad Control System.

  RCS je obecny nazev pro sbernici resici rizeni prislusenstvi, napriklad MTB,
  touto sbernici ale muze byt klidne i XpressNET.

  Vsechny ostatni casti programu by mely volat metody tridy TRCS, ktera interaguje
  s RCS. Trida TRCS v sobe skryva interakci s TCSIFace.

  Pricip:
  - na zacatku vytvorime tridy pro vsechny existujici moduly RCS
  - po otevreni RCS zjistime, ktere desky jsou skutecne dostupne a ktere ne
}

interface

uses SysUtils, Classes, IniFiles, Generics.Collections, RCSIFace, Generics.Defaults,
  Logging, RCSErrors;

type
  TRCSReadyEvent = procedure(Sender: TObject; ready: Boolean) of object;
  TRCSModuleChangeEvent = procedure(Sender: TObject; module: Cardinal) of object;
  TRCSIOType = (input = 0, output = 1);

  /// ///////////////////////////////////////////////////////////

  // toto se pouziva pro identifikaci desky a portu VSUDE v technologii
  TRCSAddr = record // jedno fyzicke RCS spojeni
    module: Cardinal; // cislo desky
    port: Byte; // cislo portu
    class operator Equal(a, b: TRCSAddr): Boolean;
    procedure Load(str: string);
    function ToString(): string;
  end;

  TRCSAddrOptional = record
    addr: TRCSAddr;
    enabled: Boolean;
  end;

  TRCSModule = class // jedna RCS deska
    needed: Boolean; // jestli jed eska potrebna pro technologii (tj. jeslti na ni referuji nejake bloky atp.)
    inputChangedEv: TList<TRCSModuleChangeEvent>;
    outputChangedEv: TList<TRCSModuleChangeEvent>;

    constructor Create();
    destructor Destroy(); override;
  end;

  /// ///////////////////////////////////////////////////////////

  // Technologie RCS
  TRCS = class(TRCSIFace)
  public const
    _MODULE_DEFAULT_IO = 256; // cannot be more than Max(TRCSAddr.port)+1

  private
    mSystemI: Cardinal;
    mLibDir: string;
    modules: TObjectDictionary<Cardinal, TRCSModule>;
    mLoadedOk: Boolean; // jestli je nactena knihovna vporadku a tudiz jestli lze zapnout systemy
    fGeneralError: Boolean; // flag oznamujici nastani "RCS general IO error" -- te nejhorsi veci na svete

    // events to the main program
    fOnReady: TRCSReadyEvent;
    fAfterClose: TNotifyEvent;

    // events from library
    procedure DllAfterClose(Sender: TObject);

    procedure DllOnLog(Sender: TObject; logLevel: TRCSLogLevel; msg: string);
    procedure DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg: PChar);
    procedure DllOnModuleChanged(Sender: TObject; module: Cardinal);
    procedure DllOnInputChanged(Sender: TObject; module: Cardinal);
    procedure DllOnOutputChanged(Sender: TObject; module: Cardinal);
    function GetMaxModuleAddrSafe(): Cardinal;
    function IsReady(): Boolean;

  public
    logEnabled: Boolean;

    constructor Create(systemI: Cardinal);
    destructor Destroy(); override;

    procedure LoadLib(libFn: string; configFn: string);
    procedure UnloadLib();

    procedure SetNeeded(module: Cardinal; state: Boolean = true);
    function GetNeeded(module: Cardinal): Boolean;

    procedure SetOutput(addr: TRCSAddr; state: Integer); overload;
    procedure SetOutput(addr: TRCSAddr; state: TRCSOutputState); overload;
    procedure SetOutputs(addrs: TList<TRCSAddr>; state: Integer); overload;
    procedure SetOutputs(addrs: TList<TRCSAddr>; state: TRCSOutputState); overload;

    function GetInput(addr: TRCSAddr): TRCSInputState; overload;

    procedure SetInput(addr: TRCSAddr; state: Integer); overload;
    procedure SetInputs(addrs: TList<TRCSAddr>; state: Integer); overload;

    function GetOutput(addr: TRCSAddr): Integer; overload;
    function GetOutputState(addr: TRCSAddr): TRCSOutputState; overload;

    procedure AddInputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
    procedure RemoveInputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);

    procedure AddOutputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
    procedure RemoveOutputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);

    function GetModuleInputsCountSafe(module: Cardinal): Cardinal;
    function GetModuleOutputsCountSafe(module: Cardinal): Cardinal;

    property generalError: Boolean read fGeneralError;
    class function RCSAddr(module: Cardinal; port: Byte): TRCSAddr;
    class function RCSOptionalAddr(module: Cardinal; port: Byte): TRCSAddrOptional; overload;
    class function RCSOptionalAddrDisabled(): TRCSAddrOptional; overload;

    procedure Log(msg: string; level: TLogLevel; brief: Boolean = False);
    procedure LogFMainStatus(msg: string; level: TLogLevel);

    // events
    property AfterClose: TNotifyEvent read fAfterClose write fAfterClose;

    property OnReady: TRCSReadyEvent read fOnReady write fOnReady;
    property ready: Boolean read IsReady;
    property maxModuleAddr: Cardinal read GetMaxModuleAddr;
    property maxModuleAddrSafe: Cardinal read GetMaxModuleAddrSafe;
    property systemI: Cardinal read mSystemI;
    property libDir: string read mLibDir write mLibDir;
  end;

function RCSAddrComparer(): IComparer<TRCSAddr>;

implementation

uses fMain, diagnostics, GetSystems, BlockDb, Block, BlockTurnout, BlockTrack,
  BoosterDb, BlockCrossing, AreaDb, IfThenElse, RCSsc,
  TCPServerPanel, TrainDb, DataRCS, appEv, Booster, StrUtils, fTester;

constructor TRCS.Create(systemI: Cardinal);
begin
  inherited Create();
  Self.mSystemI := systemI;
  Self.mLibDir := '';

  Self.modules := TObjectDictionary<Cardinal, TRCSModule>.Create();

  Self.logEnabled := False;
  Self.mLoadedOk := False;
  Self.fGeneralError := False;

  // assign events
  TRCSIFace(Self).AfterClose := Self.DllAfterClose;
  TRCSIFace(Self).OnError := Self.DllOnError;
  TRCSIFace(Self).OnLog := Self.DllOnLog;
  TRCSIFace(Self).OnModuleChanged := Self.DllOnModuleChanged;
  TRCSIFace(Self).OnInputChanged := Self.DllOnInputChanged;
  TRCSIFace(Self).OnOutputChanged := Self.DllOnOutputChanged;
end;

destructor TRCS.Destroy();
begin
  Self.modules.Free();
  inherited;
end;

procedure TRCS.Log(msg: string; level: TLogLevel; brief: Boolean = False);
begin
  Logging.Log('RCS'+IntToStr(Self.mSystemI) + ': ' + msg, level, lsRCS, brief);
end;

procedure TRCS.LogFMainStatus(msg: string; level: TLogLevel);
begin
  F_Main.LogBrief('RCS'+IntToStr(Self.mSystemI) + ': ' + msg, level);
end;

procedure TRCS.LoadLib(libFn: string; configFn: string);
begin
  var fullPath := Self.mLibDir + '\' + libFn;

  if (not FileExists(fullPath)) then
    raise RCSException.Create('Library file not found, not loading');

  if (Self.ready) then
  begin
    Self.mLoadedOk := false;
    if (Assigned(Self.OnReady)) then
      Self.OnReady(Self, Self.ready);
  end;

  const configDir: string = ExtractFileDir(configFn);
  if (not DirectoryExists(configDir)) then
    CreateDir(configDir);

  TRCSIFace(Self).LoadLib(fullPath, configFn);

  Self.Log('Načtena knihovna ' + fullPath + ', RCS API v'+Self.apiVersionStr() + ', '+configFn, llInfo);

  // kontrola bindnuti vsech eventu

  // bind SetInput neni striktne vyzadovan
  if (Self.unbound.Contains('SetInput')) then
    Self.unbound.Remove('SetInput');

  if (Self.unbound.Count = 0) then
  begin
    Self.mLoadedOk := true;
    if (Assigned(Self.OnReady)) then
      Self.OnReady(Self, Self.ready);
  end else begin
    var str := '';
    for var tmp in Self.unbound do
      str := str + tmp + ', ';
    str := LeftStr(str, Length(str) - 2);
    Self.Log('Nepodařilo se svázat následující funkce: ' + str, llError, True);
  end;
end;

procedure TRCS.UnloadLib();
begin
  Self.mLoadedOk := False;
  Self.fGeneralError := False;
  TRCSIFace(Self).UnloadLib();
  Self.Log('Knihovna odnačtena', llInfo);
end;

procedure TRCS.DllAfterClose(Sender: TObject);
begin
  Self.fGeneralError := false;
  if (Assigned(Self.fAfterClose)) then
    Self.fAfterClose(Self);
end;

procedure TRCS.DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg: PChar);
begin
  Self.Log('RCS ERR: ' + errMsg + ' (' + IntToStr(errValue) + ':' + IntToStr(errAddr) + ')', llError);

  if (SystemData.Status = TSystemStatus.starting) then
    SystemData.Status := TSystemStatus.null;

  if (Self.IsStateActionInProgress()) then
    Self.LogFMainStatus(errMsg, TLogLevel.llError);

  case (errValue) of
    RCS_MODULE_FAILED:
      Areas.RCSFail(TRCSs.RCSsSystemModule(Self.mSystemI, errAddr)); // communication with module failed
  end;
end;

procedure TRCS.DllOnLog(Sender: TObject; logLevel: TRCSLogLevel; msg: string);
begin
  if (not Self.logEnabled) then
    Exit();

  var systemLogLevel: TLogLevel;
  case (logLevel) of
    TRCSLogLevel.llErrors:
      systemLogLevel := TLogLevel.llError;
    TRCSLogLevel.llWarnings:
      systemLogLevel := TLogLevel.llWarning;
    TRCSLogLevel.llRawCommands:
      systemLogLevel := TLogLevel.llDetail;
    TRCSLogLevel.llDebug:
      systemLogLevel := TLogLevel.llDebug;
  else
    systemLogLevel := TLogLevel.llInfo;
  end;

  Self.Log(UpperCase(Self.LogLevelToString(logLevel)) + ': ' + msg, systemLogLevel);
end;

procedure TRCS.DllOnModuleChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.modules.ContainsKey(module)) then
  begin
    for var i: Integer := Self.modules[module].outputChangedEv.Count - 1 downto 0 do
    begin
      if (Assigned(Self.modules[module].outputChangedEv[i])) then
        Self.modules[module].outputChangedEv[i](Self, module)
      else
        Self.modules[module].outputChangedEv.Delete(i);
    end;
  end;
  RCSTableData[Self.mSystemI].UpdateBoard(module);
  F_Tester.RCSModuleChanged(Self.mSystemI, module);
end;

procedure TRCS.DllOnInputChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.modules.ContainsKey(module)) then
  begin
    for var i: Integer := Self.modules[module].inputChangedEv.Count - 1 downto 0 do
    begin
      if (Assigned(Self.modules[module].inputChangedEv[i])) then
        Self.modules[module].inputChangedEv[i](Self, module)
      else
        Self.modules[module].inputChangedEv.Delete(i);
    end;
  end;
  RCSTableData[Self.mSystemI].UpdateBoardInputs(module);
  F_Tester.RCSModuleInputsChanged(Self.mSystemI, module);
end;

procedure TRCS.DllOnOutputChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.modules.ContainsKey(module)) then
  begin
    for var i: Integer := Self.modules[module].outputChangedEv.Count - 1 downto 0 do
    begin
      if (Assigned(Self.modules[module].outputChangedEv[i])) then
        Self.modules[module].outputChangedEv[i](Self, module)
      else
        Self.modules[module].outputChangedEv.Delete(i);
    end;
  end;
  RCSTableData[Self.mSystemI].UpdateBoardOutputs(module);
  F_Tester.RCSModuleOutputsChanged(Self.mSystemI, module);
end;

// ----- events from dll end -----
/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.SetNeeded(module: Cardinal; state: Boolean = true);
begin
  if (not Self.modules.ContainsKey(module)) then
    Self.modules.Add(module, TRCSModule.Create());
  Self.modules[module].needed := state
end;

function TRCS.GetNeeded(module: Cardinal): Boolean;
begin
  if (Self.modules.ContainsKey(module)) then
    Result := Self.modules[module].needed
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRCSModule.Create();
begin
  inherited;
  Self.inputChangedEv := TList<TRCSModuleChangeEvent>.Create();
  Self.outputChangedEv := TList<TRCSModuleChangeEvent>.Create();
end;

destructor TRCSModule.Destroy();
begin
  Self.inputChangedEv.Free();
  Self.outputChangedEv.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddInputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
begin
  if (not Self.modules.ContainsKey(module)) then
    Self.modules.Add(module, TRCSModule.Create());
  if (Self.modules[module].inputChangedEv.IndexOf(event) = -1) then
    Self.modules[module].inputChangedEv.Add(event);
end;

procedure TRCS.RemoveInputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);
var rcsBoard: TRCSModule;
begin
  if (module = -1) then
  begin
    for rcsBoard in Self.modules.Values do
      rcsBoard.inputChangedEv.Remove(event);
  end else begin
    if (Self.modules.ContainsKey(module)) then
      Self.modules[module].inputChangedEv.Remove(event);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddOutputChangeEvent(module: Cardinal; event: TRCSModuleChangeEvent);
begin
  if (not Self.modules.ContainsKey(module)) then
    Self.modules.Add(module, TRCSModule.Create());
  if (Self.modules[module].outputChangedEv.IndexOf(event) = -1) then
    Self.modules[module].outputChangedEv.Add(event);
end;

procedure TRCS.RemoveOutputChangeEvent(event: TRCSModuleChangeEvent; module: Integer = -1);
begin
  if (module = -1) then
  begin
    for var rcsBoard: TRCSModule in Self.modules.Values do
      rcsBoard.outputChangedEv.Remove(event);
  end else begin
    if (Self.modules.ContainsKey(module)) then
      Self.modules[module].outputChangedEv.Remove(event);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRCS.RCSAddr(module: Cardinal; port: Byte): TRCSAddr;
begin
  Result.module := module;
  Result.port := port;
end;

class function TRCS.RCSOptionalAddr(module: Cardinal; port: Byte): TRCSAddrOptional;
begin
  Result.enabled := True;
  Result.addr := RCSAddr(module, port);
end;

class function TRCS.RCSOptionalAddrDisabled(): TRCSAddrOptional;
begin
  Result.enabled := False;
  Result.addr := RCSAddr(0, 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCS.GetMaxModuleAddrSafe(): Cardinal;
begin
  if (not Self.ready) then
    Exit(0);
  try
    Result := Self.GetMaxModuleAddr();
  except
    Result := 0;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCS.GetModuleInputsCountSafe(module: Cardinal): Cardinal;
begin
  if (not Self.ready) then
    Exit(_MODULE_DEFAULT_IO);
  try
    Result := Self.GetModuleInputsCount(module);
  except
    Result := _MODULE_DEFAULT_IO;
  end;
end;

function TRCS.GetModuleOutputsCountSafe(module: Cardinal): Cardinal;
begin
  if (not Self.ready) then
    Exit(_MODULE_DEFAULT_IO);
  try
    Result := Self.GetModuleOutputsCount(module);
  except
    Result := _MODULE_DEFAULT_IO;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.SetInput(addr: TRCSAddr; state: Integer);
begin
  Self.SetInput(addr.module, addr.port, state);
end;

procedure TRCS.SetInputs(addrs: TList<TRCSAddr>; state: Integer);
begin
  for var addr: TRCSAddr in addrs do
    Self.SetInput(addr, state);
end;

function TRCS.GetInput(addr: TRCSAddr): TRCSInputState;
begin
  Result := Self.GetInput(addr.module, addr.port);
end;

procedure TRCS.SetOutput(addr: TRCSAddr; state: Integer);
begin
  Self.SetOutput(addr.module, addr.port, state);
end;

procedure TRCS.SetOutput(addr: TRCSAddr; state: TRCSOutputState);
begin
  Self.SetOutput(addr.module, addr.port, state);
end;

procedure TRCS.SetOutputs(addrs: TList<TRCSAddr>; state: Integer);
begin
  for var addr: TRCSAddr in addrs do
    Self.SetOutput(addr, state);
end;

procedure TRCS.SetOutputs(addrs: TList<TRCSAddr>; state: TRCSOutputState);
begin
  for var addr: TRCSAddr in addrs do
    Self.SetOutput(addr, state);
end;

function TRCS.GetOutput(addr: TRCSAddr): Integer;
begin
  Result := Self.GetOutput(addr.module, addr.port);
end;

function TRCS.GetOutputState(addr: TRCSAddr): TRCSOutputState;
begin
  Result := Self.GetOutputState(addr.module, addr.port);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCS.IsReady(): Boolean;
begin
  Result := (Self.libLoaded) and (Self.mLoadedOk);
end;

/// /////////////////////////////////////////////////////////////////////////////

class operator TRCSAddr.Equal(a, b: TRCSAddr): Boolean;
begin
  Result := ((a.module = b.module) and (a.port = b.port));
end;

function TRCSAddr.ToString(): string;
begin
  Result := IntToStr(module) + ':' + IntToStr(port);
end;

procedure TRCSAddr.Load(str: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStrings([':'], [], PChar(str), strs);
    if (strs.Count <> 2) then
      raise Exception.Create('Unable to load RCS: '+str);
    module := StrToInt(strs[0]);
    port := StrToInt(strs[1]);
  finally
    strs.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function RCSAddrComparer(): IComparer<TRCSAddr>;
begin
  Result := TComparer<TRCSAddr>.Construct(
    function(const Left, Right: TRCSAddr): Integer
    begin
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

/// /////////////////////////////////////////////////////////////////////////////

end.// unit

