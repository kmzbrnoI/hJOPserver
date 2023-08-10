unit TechnologieRCS;

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

uses SysUtils, Classes, IniFiles, Generics.Collections, RCS, Generics.Defaults;

type
  TRCSReadyEvent = procedure(Sender: TObject; ready: Boolean) of object;
  TRCSBoardChangeEvent = procedure(Sender: TObject; board: Cardinal) of object;
  TRCSIOType = (input = 0, output = 1);

  /// ///////////////////////////////////////////////////////////

  // toto se pouziva pro identifikaci desky a portu VSUDE v technologii
  TRCSAddr = record // jedno fyzicke RCS spojeni
    board: Cardinal; // cislo desky
    port: Byte; // cislo portu
    class operator Equal(a, b: TRCSAddr): Boolean;
    procedure Load(str: string);
    function ToString(): string;
  end;

  TRCSAddrOptional = record
    addr: TRCSAddr;
    enabled: Boolean;
  end;

  TRCSBoard = class // jedna RCS deska
    needed: Boolean; // jestli jed eska potrebna pro technologii (tj. jeslti na ni referuji nejake bloky atp.)
    inputChangedEv: TList<TRCSBoardChangeEvent>;
    outputChangedEv: TList<TRCSBoardChangeEvent>;

    constructor Create();
    destructor Destroy(); override;
  end;

  /// ///////////////////////////////////////////////////////////

  // Technologie RCS
  TRCS = class(TRCSIFace)
  public const
    _MODULE_DEFAULT_IO = 256; // cannot be more than Max(TRCSAddr.port)+1

  private const
    _DEFAULT_LIB = 'simulator.dll';
    _INIFILE_SECTNAME = 'RCS';
    _DEFAULT_CONFIG_PATH = 'lib-conf';

  private
    boards: TObjectDictionary<Cardinal, TRCSBoard>;
    aReady: Boolean; // jestli je nactena knihovna vporadku a tudiz jestli lze zapnout systemy
    fGeneralError: Boolean; // flag oznamujici nastani "RCS general IO error" -- te nejhorsi veci na svete
    fLibDir: string;
    mConfigDir: string;

    // events to the main program
    fOnReady: TRCSReadyEvent;
    fAfterClose: TNotifyEvent;

    // events from libraly
    procedure DllAfterClose(Sender: TObject);

    procedure DllOnLog(Sender: TObject; logLevel: TRCSLogLevel; msg: string);
    procedure DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg: PChar);
    procedure DllOnModuleChanged(Sender: TObject; module: Cardinal);
    procedure DllOnInputChanged(Sender: TObject; module: Cardinal);
    procedure DllOnOutputChanged(Sender: TObject; module: Cardinal);
    function GetMaxModuleAddrSafe(): Cardinal;

  public
    log: Boolean;
    logActionInProgress: Boolean;

    constructor Create();
    destructor Destroy; override;

    procedure LoadLib(filename: string); // nacte knihovnu

    procedure InputSim(); // pokud je nactena knihovna Simulator.dll, simuluje vstupy (koncove polohy vyhybek atp.)
    procedure SoupravaUsekSim(); // nastavit RCS vstupy tak, aby useky, n akterych existuje souprava, byly obsazene

    function NoExStarted(): Boolean;
    function NoExOpened(): Boolean;

    procedure SetNeeded(RCSAdr: Cardinal; state: Boolean = true);
    function GetNeeded(RCSAdr: Cardinal): Boolean;

    procedure LoadFromFile(ini: TMemIniFile);
    procedure SaveToFile(ini: TMemIniFile);

    procedure SetOutput(addr: TRCSAddr; state: Integer); overload;
    procedure SetOutput(addr: TRCSAddr; state: TRCSOutputState); overload;
    procedure SetOutputs(addrs: TList<TRCSAddr>; state: Integer); overload;
    procedure SetOutputs(addrs: TList<TRCSAddr>; state: TRCSOutputState); overload;
    function GetInput(addr: TRCSAddr): TRCSInputState; overload;
    procedure SetInput(addr: TRCSAddr; state: Integer); overload;
    procedure SetInputs(addrs: TList<TRCSAddr>; state: Integer); overload;
    function GetOutput(addr: TRCSAddr): Integer; overload;
    function GetOutputState(addr: TRCSAddr): TRCSOutputState; overload;

    procedure AddInputChangeEvent(board: Cardinal; event: TRCSBoardChangeEvent);
    procedure RemoveInputChangeEvent(event: TRCSBoardChangeEvent; board: Integer = -1);

    procedure AddOutputChangeEvent(board: Cardinal; event: TRCSBoardChangeEvent);
    procedure RemoveOutputChangeEvent(event: TRCSBoardChangeEvent; board: Integer = -1);

    function GetModuleInputsCountSafe(module: Cardinal): Cardinal;
    function GetModuleOutputsCountSafe(module: Cardinal): Cardinal;

    property generalError: Boolean read fGeneralError;
    class function RCSAddr(board: Cardinal; port: Byte): TRCSAddr;
    class function RCSOptionalAddr(board: Cardinal; port: Byte): TRCSAddrOptional; overload;
    class function RCSOptionalAddrDisabled(): TRCSAddrOptional; overload;

    // events
    property AfterClose: TNotifyEvent read fAfterClose write fAfterClose;

    property OnReady: TRCSReadyEvent read fOnReady write fOnReady;
    property ready: Boolean read aReady;
    property libDir: string read fLibDir;
    property configDir: string read mConfigDir;
    property maxModuleAddr: Cardinal read GetMaxModuleAddr;
    property maxModuleAddrSafe: Cardinal read GetMaxModuleAddrSafe;
  end;

function RCSAddrComparer(): IComparer<TRCSAddr>;

var
  RCSi: TRCS;

implementation

uses fMain, diagnostics, GetSystems, BlockDb, Block, BlockTurnout, BlockTrack,
  BoosterDb, BlockCrossing, RCSErrors, AreaDb, IfThenElse,
  Logging, TCPServerPanel, TrainDb, DataRCS, appEv, Booster, StrUtils, fTester;

constructor TRCS.Create();
begin
  inherited;

  Self.boards := TObjectDictionary<Cardinal, TRCSBoard>.Create();

  Self.logActionInProgress := false;
  Self.log := false;
  Self.aReady := false;
  Self.fGeneralError := false;

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
  Self.boards.Free();
  inherited;
end;

procedure TRCS.LoadLib(filename: string);
var libName: string;
begin
  libName := ExtractFileName(filename);

  if (not FileExists(filename)) then
    raise Exception.Create('Library file not found, not loading');

  if (Self.ready) then
  begin
    Self.aReady := false;
    if (Assigned(Self.OnReady)) then
      Self.OnReady(Self, Self.aReady);
  end;

  if not DirectoryExists(Self.configDir) then
    CreateDir(Self.configDir);

  TRCSIFace(Self).LoadLib(filename, Self.configDir + '\' + ChangeFileExt(libName, '.ini'));

  Logging.Log('Načtena knihovna ' + libName + ', RCS API v'+Self.apiVersionStr(), llInfo, lsRCS);

  // kontrola bindnuti vsech eventu

  // bind SetInput neni striktne vyzadovan
  if (Self.unbound.Contains('SetInput')) then
    Self.unbound.Remove('SetInput');

  if (Self.unbound.Count = 0) then
  begin
    Self.aReady := true;
    if (Assigned(Self.OnReady)) then
      Self.OnReady(Self, Self.aReady);
  end else begin
    var str := '';
    for var tmp in Self.unbound do
      str := str + tmp + ', ';
    str := LeftStr(str, Length(str) - 2);
    F_Main.LogStatus('ERR: RCS: nepodařilo se svázat následující funkce : ' + str);
  end;
end;

procedure TRCS.InputSim();
begin
  // vychozi stav bloku
  for var blk: TBlk in Blocks do
  begin
    try
      if ((Blk.GetGlobalSettings.typ = btTurnout) and ((Blk as TBlkTurnout).posDetection)) then
        Self.SetInput(TBlkTurnout(Blk).rcsInPlus, 1);
      if ((Blk.typ = btCrossing) and (TBlkCrossing(Blk).GetSettings().RCSInputs.open.enabled)) then
        Self.SetInput(TBlkCrossing(Blk).GetSettings().RCSInputs.open.addr, 1);
      if ((diag.simSoupravaObsaz) and ((Blk.typ = btTrack) or (Blk.typ = btRT)) and ((Blk as TBlkTrack).IsTrain()) and
        ((Blk as TBlkTrack).occupAvailable)) then
        Self.SetInput(TBlkTrack(Blk).GetSettings().RCSAddrs[0], 1);
    except

    end;
  end;

  // defaultni stav zesilovacu
  for var booster: TBooster in Boosters.sorted do
  begin
    try
      if (Booster.isPowerDetection) then
        Self.SetInput(Booster.settings.rcs.power.addr, ite(Booster.settings.rcs.power.reversed, 1, 0));
      if (Booster.isOverloadDetection) then
        Self.SetInput(Booster.settings.rcs.overload.addr, ite(Booster.settings.rcs.overload.reversed, 1, 0));
      if (Booster.isDCCdetection) then
        Self.SetInput(Booster.settings.rcs.DCC.addr, ite(Booster.settings.rcs.DCC.reversed, 1, 0));
    except

    end;
  end;
end;

// simulace obaszeni useku, na kterem je souprava
procedure TRCS.SoupravaUsekSim;
begin
  for var blk: TBlk in Blocks do
  begin
    if ((Blk.typ <> btTrack) and (Blk.typ <> btRT)) then
      continue;
    if (((Blk as TBlkTrack).IsTrain()) and ((Blk as TBlkTrack).occupAvailable)) then
      Self.SetInput((Blk as TBlkTrack).GetSettings().RCSAddrs[0], 1);
  end;
end;

procedure TRCS.LoadFromFile(ini: TMemIniFile);
var lib: string;
begin
  Self.fLibDir := ini.ReadString(_INIFILE_SECTNAME, 'dir', '.');
  lib := ini.ReadString(_INIFILE_SECTNAME, 'lib', _DEFAULT_LIB);
  Self.mConfigDir := ini.ReadString(_INIFILE_SECTNAME, 'configDir', _DEFAULT_CONFIG_PATH);

  try
    Self.LoadLib(fLibDir + '\' + lib);
  except
    on E: Exception do
    begin
      F_Main.LogStatus('ERR: RCS: Nelze načíst knihovnu ' + fLibDir + '\' + lib + ': ' + E.Message);
      Logging.Log('Nelze načíst knihovnu ' + fLibDir + '\' + lib + ': ' + E.Message, llError, lsRCS);
    end;
  end;
end;

procedure TRCS.SaveToFile(ini: TMemIniFile);
begin
  if (Self.lib <> '') then
    ini.WriteString(_INIFILE_SECTNAME, 'lib', ExtractFileName(Self.lib));
end;

procedure TRCS.DllAfterClose(Sender: TObject);
begin
  Self.fGeneralError := false;
  if (Assigned(Self.fAfterClose)) then
    Self.fAfterClose(Self);
end;

procedure TRCS.DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg: PChar);
begin
  Logging.Log('RCS ERR: ' + errMsg + ' (' + IntToStr(errValue) + ':' + IntToStr(errAddr) + ')', llError, lsRCS);

  if (SystemData.Status = TSystemStatus.starting) then
    SystemData.Status := TSystemStatus.null;

  if (Self.logActionInProgress) then
    F_Main.LogStatus('ERR: ' + errMsg);

  case (errValue) of
    RCS_MODULE_FAILED:
      Areas.RCSFail(errAddr); // communication with module failed
  end;
end;

procedure TRCS.DllOnLog(Sender: TObject; logLevel: TRCSLogLevel; msg: string);
begin
  if (not Self.log) then
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

  Logging.Log(UpperCase(Self.LogLevelToString(logLevel)) + ': ' + msg, systemLogLevel, lsRCS);
end;

procedure TRCS.DllOnModuleChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.boards.ContainsKey(module)) then
    for var i: Integer := Self.boards[module].outputChangedEv.Count - 1 downto 0 do
      if (Assigned(Self.boards[module].outputChangedEv[i])) then
        Self.boards[module].outputChangedEv[i](Self, module)
      else
        Self.boards[module].outputChangedEv.Delete(i);
  RCSTableData.UpdateBoard(module);
  F_Tester.RCSModuleChanged(module);
end;

procedure TRCS.DllOnInputChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.boards.ContainsKey(module)) then
    for var i: Integer := Self.boards[module].inputChangedEv.Count - 1 downto 0 do
      if (Assigned(Self.boards[module].inputChangedEv[i])) then
        Self.boards[module].inputChangedEv[i](Self, module)
      else
        Self.boards[module].inputChangedEv.Delete(i);
  RCSTableData.UpdateBoardInputs(module);
  F_Tester.RCSModuleInputsChanged(module);
end;

procedure TRCS.DllOnOutputChanged(Sender: TObject; module: Cardinal);
begin
  if (Self.boards.ContainsKey(module)) then
    for var i: Integer := Self.boards[module].outputChangedEv.Count - 1 downto 0 do
      if (Assigned(Self.boards[module].outputChangedEv[i])) then
        Self.boards[module].outputChangedEv[i](Self, module)
      else
        Self.boards[module].outputChangedEv.Delete(i);
  RCSTableData.UpdateBoardOutputs(module);
  F_Tester.RCSModuleOutputsChanged(module);
end;

// ----- events from dll end -----
/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.SetNeeded(RCSAdr: Cardinal; state: Boolean = true);
begin
  if (not Self.boards.ContainsKey(RCSAdr)) then
    Self.boards.Add(RCSAdr, TRCSBoard.Create());
  Self.boards[RCSAdr].needed := state
end;

function TRCS.GetNeeded(RCSAdr: Cardinal): Boolean;
begin
  if (Self.boards.ContainsKey(RCSAdr)) then
    Result := Self.boards[RCSAdr].needed
  else
    Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRCSBoard.Create();
begin
  inherited;
  Self.inputChangedEv := TList<TRCSBoardChangeEvent>.Create();
  Self.outputChangedEv := TList<TRCSBoardChangeEvent>.Create();
end;

destructor TRCSBoard.Destroy();
begin
  Self.inputChangedEv.Free();
  Self.outputChangedEv.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddInputChangeEvent(board: Cardinal; event: TRCSBoardChangeEvent);
begin
  if (not Self.boards.ContainsKey(board)) then
    Self.boards.Add(board, TRCSBoard.Create());
  if (Self.boards[board].inputChangedEv.IndexOf(event) = -1) then
    Self.boards[board].inputChangedEv.Add(event);
end;

procedure TRCS.RemoveInputChangeEvent(event: TRCSBoardChangeEvent; board: Integer = -1);
var rcsBoard: TRCSBoard;
begin
  if (board = -1) then
  begin
    for rcsBoard in Self.boards.Values do
      rcsBoard.inputChangedEv.Remove(event);
  end else begin
    if (Self.boards.ContainsKey(board)) then
      Self.boards[board].inputChangedEv.Remove(event);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddOutputChangeEvent(board: Cardinal; event: TRCSBoardChangeEvent);
begin
  if (not Self.boards.ContainsKey(board)) then
    Self.boards.Add(board, TRCSBoard.Create());
  if (Self.boards[board].outputChangedEv.IndexOf(event) = -1) then
    Self.boards[board].outputChangedEv.Add(event);
end;

procedure TRCS.RemoveOutputChangeEvent(event: TRCSBoardChangeEvent; board: Integer = -1);
begin
  if (board = -1) then
  begin
    for var rcsBoard: TRCSBoard in Self.boards.Values do
      rcsBoard.outputChangedEv.Remove(event);
  end else begin
    if (Self.boards.ContainsKey(board)) then
      Self.boards[board].outputChangedEv.Remove(event);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCS.NoExStarted(): Boolean;
begin
  try
    Result := Self.Started();
  except
    Result := false;
  end;
end;

function TRCS.NoExOpened(): Boolean;
begin
  try
    Result := Self.Opened();
  except
    Result := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRCS.RCSAddr(board: Cardinal; port: Byte): TRCSAddr;
begin
  Result.board := board;
  Result.port := port;
end;

class function TRCS.RCSOptionalAddr(board: Cardinal; port: Byte): TRCSAddrOptional;
begin
  Result.enabled := True;
  Result.addr := RCSAddr(board, port);
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
  Self.SetInput(addr.board, addr.port, state);
end;

procedure TRCS.SetInputs(addrs: TList<TRCSAddr>; state: Integer);
begin
  for var addr: TRCSAddr in addrs do
    Self.SetInput(addr, state);
end;

function TRCS.GetInput(addr: TRCSAddr): TRCSInputState;
begin
  Result := Self.GetInput(addr.board, addr.port);
end;

procedure TRCS.SetOutput(addr: TRCSAddr; state: Integer);
begin
  Self.SetOutput(addr.board, addr.port, state);
end;

procedure TRCS.SetOutput(addr: TRCSAddr; state: TRCSOutputState);
begin
  Self.SetOutput(addr.board, addr.port, state);
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
  Result := Self.GetOutput(addr.board, addr.port);
end;

function TRCS.GetOutputState(addr: TRCSAddr): TRCSOutputState;
begin
  Result := Self.GetOutputState(addr.board, addr.port);
end;

/// /////////////////////////////////////////////////////////////////////////////

class operator TRCSAddr.Equal(a, b: TRCSAddr): Boolean;
begin
  Result := ((a.board = b.board) and (a.port = b.port));
end;

function TRCSAddr.ToString(): string;
begin
  Result := IntToStr(board) + ':' + IntToStr(port);
end;

procedure TRCSAddr.Load(str: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStrings([':'], [], PChar(str), strs);
    if (strs.Count <> 2) then
      raise Exception.Create('Unable to load RCS: '+str);
    board := StrToInt(strs[0]);
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
      if (Left.board < Right.board) then
        Exit(-1);
      if (Left.board > Right.board) then
        Exit(1);
      if (Left.port < Right.port) then
        Exit(-1);
      if (Left.port > Right.port) then
        Exit(1);
      Result := 0;
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

RCSi := TRCS.Create();

finalization

// Free in hJOPserver.dpr, because we must gurantee preload gets destructed after all shared libraries

end.// unit
