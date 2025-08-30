unit TrakceC;

{
  TTrakce class (and its singleton instance TrakceI) accesses Trakce function
  to the rest of the appliaction. This class is designed to be minimal. It works
  with loco addresses, not with instances of THV. This class is supposed to
  be used as accessor and helper. All important functions (like set speed or
  functions) should be called on instance of THV directly.

  This class is very simillar to TRCS.
}

interface

uses
  SysUtils, Classes, StrUtils, TrakceIFace, ComCtrls, THnaciVozidlo,
  Generics.Collections, IniFiles;

const
  // max speed step for speed table
  _MAX_STEP = 28;
  _SOUND_FUNC: string = 'zvuk';

  _DEFAULT_SPEED_TABLE: array [0 .. _MAX_STEP] of Cardinal = (
    0,
    1,
    2,
    4,
    6,
    8,
    10,
    12,
    15,
    17,
    20,
    22,
    25,
    30,
    35,
    40,
    45,
    50,
    55,
    60,
    65,
    70,
    75,
    80,
    85,
    90,
    100,
    110,
    120
  );

  _MAX_LOGTABLE_ITEMS = 500;
  _LOG_PATH = 'log\trakce';

type
  TReadyEvent = procedure(Sender: TObject; ready: Boolean) of object;
  TErrorEvent = procedure(Sender: TObject; errMsg: string) of object;

  // passed as a parameter to callback when programming POM
  TPOMCallback = record
    locoAddr: Word;
    toProgram: TList<THVPomCV>;
    index: Integer; // index of currently programmed CV in list 'toProgram'
    callback_ok, callback_err: TCommandCallback;
  end;

  // Set functions of all locomotives based on description
  TSetDescFuncsCallback = record
    addr: Word;
    description: string;
    state: Boolean;
    callback_ok, callback_err: TCommandCallback;
  end;

  // toggleQueue record
  THVToggleFunc = record
    HV: THV;
    fIndex: Cardinal;
    time: TDateTime;
  end;

  TFuncCallback = record
    addr: Word;
    callback_ok, callback_err: TCommandCallback;
  end;

  TTrakce = class(TTrakceIFace)
  private const
    _DEF_LOGLEVEL = TTrkLogLevel.llInfo;
    _DEF_LIB = 'xpressnet.dll';
    _INIFILE_SECTNAME = 'Trakce';
    _DEF_CONFIG_PATH = 'lib-conf';

  private
    fLibDir: string;
    aReady: Boolean;
    SpeedTable: array [0 .. _MAX_STEP] of Cardinal;
    turnoff_callback: TNotifyEvent;

    eOnReady: TReadyEvent;

    mLogLevelFile: TTrkLogLevel;
    mLogLevelTable: TTrkLogLevel;
    mConfigDir: string;

    procedure SetLoglevelFile(ll: TTrkLogLevel);
    procedure SetLoglevelTable(ll: TTrkLogLevel);

    procedure TrkLog(Sender: TObject; lvl: TTrkLogLevel; msg: string);
    procedure TrkLocoStolen(Sender: TObject; addr: Word);

    procedure TurnedOffSound(Sender: TObject; Data: Pointer);
    procedure RestoredSound(Sender: TObject; Data: Pointer);

    procedure POMCvWroteOK(Sender: TObject; Data: Pointer);
    procedure POMCvWroteErr(Sender: TObject; Data: Pointer);

    procedure LoksSetFuncOK(Sender: TObject; Data: Pointer);
    procedure LoksSetFuncErr(Sender: TObject; Data: Pointer);

    procedure LoadSpeedTableToTable(var LVRych: TListView);

    procedure CheckToggleQueue();
    procedure FlushToggleQueue();
    procedure ProcessToggleFunc(hvFunc: THVToggleFunc);
    class function hvFunc(HV: THV; fIndex: Cardinal; time: TDateTime): THVToggleFunc;

    procedure CallCb(cb: TCommandCallback);

  public

    DCCGoTime: TDateTime;
    toggleQueue: TQueue<THVToggleFunc>;
    LogObj: TListView;

    constructor Create();
    destructor Destroy(); override;

    procedure LoadLib(filename: string);

    procedure Log(loglevel: TTrkLogLevel; msg: string);

    procedure LoadFromFile(ini: TMemIniFile);
    procedure SaveToFile(ini: TMemIniFile);

    procedure LokFuncToggle(Sender: TObject; HV: THV; fIndex: Cardinal);

    procedure LoadSpeedTable(filename: string; var LVRych: TListView);
    procedure SaveSpeedTable(filename: string);
    function SpeedTableToStr(): string;

    function Step(kmph: Cardinal): Cardinal;
    function Speed(Step: Cardinal): Cardinal;
    procedure SetStepSpeed(Step: byte; Speed: Integer);

    procedure LoksSetFunc(description: string; state: Boolean; ok: TCb; err: TCb);
    procedure POMWriteCVs(addr: Word; toProgram: TList<THVPomCV>; ok: TCb; err: TCb);

    procedure TurnOffSound(ok: TCb; err: TCb);
    procedure RestoreSound(ok: TCb; err: TCb);
    procedure Update();

    function NearestLowerSpeed(Speed: Cardinal): Cardinal;

    property OnReady: TReadyEvent read eOnReady write eOnReady;
    
    property logLevelFile: TTrkLogLevel read mLogLevelFile write SetLoglevelFile;
    property logLevelTable: TTrkLogLevel read mLogLevelTable write SetLoglevelTable;

    property ready: Boolean read aReady;
    property libDir: string read fLibDir;
    property configDir: string read mConfigDir;

  end;

var
  trakce: TTrakce;

implementation

uses fMain, RCSc, fRegulator, TrainDb, GetSystems, THVDatabase, DataHV,
  BlockDb, RegulatorTCP, TCPServerPanel, appEv;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTrakce.Create();
begin
  inherited;

  Self.mConfigDir := _DEF_CONFIG_PATH;
  Self.mLogLevelFile := _DEF_LOGLEVEL;
  Self.mLogLevelTable := _DEF_LOGLEVEL;
  Self.LogObj := nil;
  Self.aReady := false;

  Self.turnoff_callback := nil;
  Self.DCCGoTime := Now;

  Self.toggleQueue := TQueue<THVToggleFunc>.Create();

  TTrakceIFace(Self).OnLog := Self.TrkLog;
  TTrakceIFace(Self).OnLocoStolen := Self.TrkLocoStolen;
end;

destructor TTrakce.Destroy();
begin
  try
    Self.Log(llInfo, 'END');
  except

  end;

  Self.FlushToggleQueue();
  FreeAndNil(Self.toggleQueue);

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoadLib(filename: string);
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

  TTrakceIFace(Self).LoadLib(filename, Self.configDir + '\' + ChangeFileExt(libName, '.ini'));

  Log(llInfo, 'Načtena knihovna ' + libName + ', Trakce API v'+Self.apiVersionStr());

  if (Self.unbound.Count = 0) then
  begin
    Self.aReady := true;
    if (Assigned(Self.OnReady)) then
      Self.OnReady(Self, Self.aReady);
  end else begin
    var str: string := '';
    for var tmp: string in Self.unbound do
      str := str + tmp + ', ';
    str := LeftStr(str, Length(str) - 2);
    F_Main.LogStatus('ERR: Trakce: nepodařilo se svázat následující funkce : ' + str);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Logging
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.Log(loglevel: TTrkLogLevel; msg: string);
var LV_Log: TListItem;
  f: TextFile;
  xDate, xTime: string;
begin
  if ((loglevel > Self.logLevelFile) and (loglevel > Self.logLevelTable)) then
    Exit();

  DateTimeToString(xDate, 'yyyy-mm-dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);

  if ((loglevel <= Self.logLevelTable) and (Self.LogObj <> nil)) then
  begin
    if (Self.LogObj.Items.Count > _MAX_LOGTABLE_ITEMS) then
      Self.LogObj.Clear();

    try
      LV_Log := Self.LogObj.Items.Insert(0);
      LV_Log.Caption := xTime;
      LV_Log.SubItems.Add(IntToStr(Integer(loglevel)));
      LV_Log.SubItems.Add(msg);
    except

    end;
  end;

  if (loglevel <= Self.logLevelFile) then
  begin
    try
      AssignFile(f, _LOG_PATH + '\' + xDate + '.log');
      if (FileExists(_LOG_PATH + '\' + xDate + '.log')) then
        Append(f)
      else
        Rewrite(f);

      var output: string := xTime + ' [' + IntToStr(Integer(loglevel)) + '] ' + msg + #13#10;
      for var b: Byte in TEncoding.UTF8.GetBytes(output) do
        Write(f, AnsiChar(b));

      CloseFile(f);
    except

    end;
  end;
end;

procedure TTrakce.SetLoglevelFile(ll: TTrkLogLevel);
begin
  Self.mLogLevelFile := ll;
  Log(llCommands, 'NEW loglevel_file = ' + LogLevelToString(ll));

  if ((ll > llNo) and (not DirectoryExists(_LOG_PATH))) then
    if (not SysUtils.ForceDirectories(ExpandFileName(_LOG_PATH))) then
      Log(llErrors, 'Nelze vytvořit složku ' + _LOG_PATH);
end;

procedure TTrakce.SetLoglevelTable(ll: TTrkLogLevel);
begin
  Self.mLogLevelTable := ll;
  Log(llCommands, 'NEW loglevel_table = ' + LogLevelToString(ll));
end;

/// /////////////////////////////////////////////////////////////////////////////
// Trakce events
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.TrkLog(Sender: TObject; lvl: TTrkLogLevel; msg: string);
begin
  Self.Log(lvl, msg);
  if ((Self.opening) and (lvl = llErrors) and (Assigned(Self.OnOpenError)) and (Self.apiVersion < $0101)) then
  begin
    Self.opening := false;
    Self.OnOpenError(Self, msg);
  end;
end;

procedure TTrakce.TrkLocoStolen(Sender: TObject; addr: Word);
begin
  if (HVDb[addr] <> nil) then
    HVDb[addr].TrakceStolen();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Load/save
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoadFromFile(ini: TMemIniFile);
var lib: string;
begin
  fLibDir := ini.ReadString(_INIFILE_SECTNAME, 'dir', '.');
  lib := ini.ReadString(_INIFILE_SECTNAME, 'lib', _DEF_LIB);
  Self.mLogLevelFile := TTrkLogLevel(ini.ReadInteger(_INIFILE_SECTNAME, 'loglevelFile', Integer(_DEF_LOGLEVEL)));
  Self.mLogLevelTable := TTrkLogLevel(ini.ReadInteger(_INIFILE_SECTNAME, 'loglevelTable', Integer(_DEF_LOGLEVEL)));
  Self.mConfigDir := ini.ReadString(_INIFILE_SECTNAME, 'configDir', _DEF_CONFIG_PATH);

  try
    Self.LoadLib(fLibDir + '\' + lib);
  except
    on E: Exception do
    begin
      Self.Log(llErrors, 'Nelze načíst knihovnu ' + fLibDir + '\' + lib + ', ' + E.Message);
      F_Main.LogStatus('ERR: Trakce: Nelze načíst knihovnu ' + fLibDir + '\' + lib + ': ' + E.Message);
    end;
  end;
end;

procedure TTrakce.SaveToFile(ini: TMemIniFile);
begin
  if (Self.lib <> '') then
    ini.WriteString(_INIFILE_SECTNAME, 'lib', ExtractFileName(Self.lib));
  ini.WriteInteger(_INIFILE_SECTNAME, 'loglevelFile', Integer(Self.logLevelFile));
  ini.WriteInteger(_INIFILE_SECTNAME, 'loglevelTable', Integer(Self.logLevelTable));
end;

/// /////////////////////////////////////////////////////////////////////////////
// LoksSetFunc
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoksSetFunc(description: string; state: Boolean; ok: TCb; err: TCb);
var cb: ^TSetDescFuncsCallback;
begin
  GetMem(cb, sizeof(TSetDescFuncsCallback));
  FillChar(cb^, sizeof(TSetDescFuncsCallback), 0); // for string to avoid access violation when assigning
  cb^.callback_ok := ok;
  cb^.callback_err := err;
  cb^.addr := 0;
  cb^.description := description;
  cb^.state := state;

  Self.LoksSetFuncOK(Self, cb);
end;

procedure TTrakce.LoksSetFuncOK(Sender: TObject; Data: Pointer);
var addr: Word;
  cb: ^TSetDescFuncsCallback;
begin
  cb := Data;
  addr := cb^.addr;

  if (not Self.ConnectedSafe()) then
    Exit();

  while ((addr < _MAX_ADDR) and ((HVDb[addr] = nil) or (not HVDb[addr].acquired) or
    (not HVDb[addr].funcDict.ContainsKey(cb^.description)) or
    (HVDb[addr].slotFunctions[HVDb[addr].funcDict[cb^.description]] = cb^.state))) do
    Inc(addr);

  if (addr = _MAX_ADDR) then
  begin
    Self.CallCb(cb^.callback_ok);
    FreeMem(cb);
    Exit();
  end;

  cb^.addr := addr + 1;
  try
    HVDb[addr].SetSingleFunc(HVDb[addr].funcDict[cb^.description], cb^.state, TTrakce.callback(Self.LoksSetFuncOK, cb),
      TTrakce.callback(Self.LoksSetFuncErr, cb));
  except
    Self.LoksSetFuncErr(Self, cb);
  end;
end;

procedure TTrakce.LoksSetFuncErr(Sender: TObject; Data: Pointer);
var cb: ^TSetDescFuncsCallback;
begin
  cb := Data;
  Self.CallCb(cb^.callback_err);
  FreeMem(cb);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Speed table
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LoadSpeedTable(filename: string; var LVRych: TListView);
var j: Integer;
  myFile: TextFile;
  Speed: Integer;
begin
  try
    AssignFile(myFile, filename);
    Reset(myFile);
  except
    Log(llErrors,
      'Chyba při načítání souboru s rychlostmi: nepodařilo se přistoupit k souboru! Používám výchozi rychlostní tabulku.');

    // nacteme vychozi rychlostni tabulku
    for var i: Integer := 0 to _MAX_STEP do
      Self.SpeedTable[i] := _DEFAULT_SPEED_TABLE[i];
    Self.LoadSpeedTableToTable(LVRych);

    Exit();
  end;

  for var i: Integer := 0 to _MAX_STEP do
  begin
    if (Eof(myFile)) then
    begin
      Log(llErrors,
        'Chyba při načítání souboru s rychlostmi: příliš málo řádků! Doplňuji výchozí rychlostní tabulkou.');
      CloseFile(myFile);
      for j := i to _MAX_STEP do
        Self.SpeedTable[j] := _DEFAULT_SPEED_TABLE[j];
      Self.LoadSpeedTableToTable(LVRych);
      Exit();
    end else begin
      try
        ReadLn(myFile, Speed);
        Self.SpeedTable[i] := Speed;
      except
        on E: Exception do
        begin
          Log(llErrors, 'Soubor s rychlostmi, řádek ' + IntToStr(i + 1) + ': ' + E.Message);
          Self.SpeedTable[i] := _DEFAULT_SPEED_TABLE[i];
        end;
      end;
    end;
  end;

  CloseFile(myFile);
  Self.LoadSpeedTableToTable(LVRych);
end;

procedure TTrakce.LoadSpeedTableToTable(var LVRych: TListView);
begin
  LVRych.Clear();

  for var i: Integer := 0 to _MAX_STEP do
  begin
    var LI: TListItem := LVRych.Items.Add;
    LI.Caption := IntToStr(i);
    LI.SubItems.Add(IntToStr(Self.SpeedTable[i]) + ' km/h');
  end;
end;

procedure TTrakce.SaveSpeedTable(filename: string);
var myFile: TextFile;
begin
  try
    AssignFile(myFile, filename);
    Rewrite(myFile);
  except
    Log(llErrors, 'Chyba pri ukladani souboru s rychlostmi - nepodarilo se pristoupit k souboru !');
    Exit();
  end;

  for var i: Integer := 0 to _MAX_STEP do
    WriteLn(myFile, IntToStr(Self.SpeedTable[i]));

  CloseFile(myFile);
end;

function TTrakce.SpeedTableToStr(): string;
begin
  Result := '';
  for var i: Integer := 1 to _MAX_STEP do
    Result := Result + IntToStr(Self.SpeedTable[i]) + ',';
end;

function TTrakce.Step(kmph: Cardinal): Cardinal;
begin
  if (kmph = 0) then
    Exit(0);

  for var i: Integer := 0 to _MAX_STEP do
  begin
    if (Self.SpeedTable[i] = kmph) then
      Exit(i)
    else if (Self.SpeedTable[i] > kmph) then
      Exit(i - 1);
  end;

  Exit(_MAX_STEP);
end;

function TTrakce.Speed(Step: Cardinal): Cardinal;
begin
  if (Step > _MAX_STEP) then
    Exit(Self.SpeedTable[_MAX_STEP]);
  Result := Self.SpeedTable[Step];
end;

procedure TTrakce.SetStepSpeed(Step: byte; Speed: Integer);
begin
  if (Step > _MAX_STEP) then
    raise Exception.Create('Invalid speed step: ' + IntToStr(Step));
  Self.SpeedTable[Step] := Speed;
end;

/// /////////////////////////////////////////////////////////////////////////////
// TurnOffSound & RestoreSound
/// /////////////////////////////////////////////////////////////////////////////

// This function is called when hJOPserver is turning systems off
// It stops sound on all locos, howveer sound remaing saved as 'on' so it is
// automatically turned on on the next start.
procedure TTrakce.TurnOffSound(ok: TCb; err: TCb);
var Data: ^TFuncCallback;
begin
  GetMem(Data, sizeof(TFuncCallback));
  Data^.callback_ok := ok;
  Data^.callback_err := err;
  Data^.addr := 0;

  Self.TurnedOffSound(Self, Data);
end;

procedure TTrakce.TurnedOffSound(Sender: TObject; Data: Pointer);
var cb: ^TFuncCallback;
begin
  cb := Data;

  if (not Self.ConnectedSafe()) then
  begin
    Self.CallCb(cb^.callback_err);
    FreeMem(Data);
    Exit();
  end;

  while ((cb^.addr < _MAX_ADDR) and ((HVDb[cb^.addr] = nil) or (not HVDb[cb^.addr].acquired) or
    (not HVDb[cb^.addr].funcDict.ContainsKey(_SOUND_FUNC)) or
    (HVDb[cb^.addr].slotFunctions[HVDb[cb^.addr].funcDict[_SOUND_FUNC]] = false))) do
    Inc(cb^.addr);

  if (cb^.addr = _MAX_ADDR) then
  begin
    Self.CallCb(cb^.callback_ok);
    FreeMem(Data);
    Exit();
  end;

  try
    HVDb[cb^.addr].SetSingleFunc(HVDb[cb^.addr].funcDict[_SOUND_FUNC], false,
      TTrakce.callback(Self.TurnedOffSound, Data), TTrakce.callback(Self.TurnedOffSound, Data));
    HVDb[cb^.addr].state.functions[HVDb[cb^.addr].funcDict[_SOUND_FUNC]] := true;
  except
    Self.CallCb(cb^.callback_err);
    FreeMem(Data);
  end;
end;

procedure TTrakce.RestoreSound(ok: TCb; err: TCb);
var Data: ^TFuncCallback;
begin
  GetMem(Data, sizeof(TFuncCallback));
  Data^.callback_ok := ok;
  Data^.callback_err := err;
  Data^.addr := 0;

  Self.RestoredSound(Self, Data);
end;

procedure TTrakce.RestoredSound(Sender: TObject; Data: Pointer);
var cb: ^TFuncCallback;
begin
  cb := Data;

  if (not Self.ConnectedSafe()) then
  begin
    Self.CallCb(cb^.callback_err);
    FreeMem(Data);
    Exit();
  end;

  while ((cb^.addr < _MAX_ADDR) and ((HVDb[cb^.addr] = nil) or (not HVDb[cb^.addr].acquired) or
    (not HVDb[cb^.addr].funcDict.ContainsKey(_SOUND_FUNC)) or
    (HVDb[cb^.addr].slotFunctions[HVDb[cb^.addr].funcDict[_SOUND_FUNC]] = true) or
    (HVDb[cb^.addr].stateFunctions[HVDb[cb^.addr].funcDict[_SOUND_FUNC]] = false))) do
    Inc(cb^.addr);

  if (cb^.addr = _MAX_ADDR) then
  begin
    Self.CallCb(cb^.callback_ok);
    FreeMem(Data);
    Exit();
  end;

  try
    HVDb[cb^.addr].SetSingleFunc(HVDb[cb^.addr].funcDict[_SOUND_FUNC], true, TTrakce.callback(Self.RestoredSound, Data),
      TTrakce.callback(Self.RestoredSound, Data));
  except
    Self.CallCb(cb^.callback_err);
    FreeMem(Data);
  end;
end;
/// /////////////////////////////////////////////////////////////////////////////
// POM
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.POMWriteCVs(addr: Word; toProgram: TList<THVPomCV>; ok: TCb; err: TCb);
var Data: ^TPOMCallback;
begin
  if (toProgram.Count = 0) then
  begin
    Self.CallCb(ok);
    Exit();
  end;

  GetMem(Data, sizeof(TPOMCallback));
  Data^.locoAddr := addr;
  Data^.toProgram := toProgram;
  Data^.callback_ok := ok;
  Data^.callback_err := err;
  Data^.index := 0;

  try
    Self.POMWriteCV(addr, toProgram[0].cv, toProgram[0].value, TTrakce.callback(Self.POMCvWroteOK, Data),
      TTrakce.callback(Self.POMCvWroteErr, Data));
  except
    Self.POMCvWroteErr(Self, Data);
  end;
end;

procedure TTrakce.POMCvWroteOK(Sender: TObject; Data: Pointer);
var pomData: ^TPOMCallback;
begin
  pomData := Data;

  if (pomData.index >= (pomData.toProgram.Count - 1)) then
  begin
    // last POM
    Self.CallCb(pomData.callback_ok);
    FreeMem(Data);
  end else begin
    // send next data
    pomData.index := pomData.index + 1;

    Self.POMWriteCV(pomData.locoAddr, pomData.toProgram[pomData.index].cv, pomData.toProgram[pomData.index].value,
      TTrakce.callback(Self.POMCvWroteOK, Data), TTrakce.callback(Self.POMCvWroteErr, Data));
  end;
end;

procedure TTrakce.POMCvWroteErr(Sender: TObject; Data: Pointer);
var pomData: ^TPOMCallback;
begin
  pomData := Data;
  Self.CallCb(pomData.callback_err);
  FreeMem(Data);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Function toggling
/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.LokFuncToggle(Sender: TObject; HV: THV; fIndex: Cardinal);
begin
  HV.SetSingleFunc(fIndex, true, trakce.callback(), trakce.callback(), Sender);
  Self.toggleQueue.Enqueue(hvFunc(HV, fIndex, Now + EncodeTime(0, 0, 0, 500)));
end;

procedure TTrakce.CheckToggleQueue();
begin
  if (Self.toggleQueue.Count = 0) then
    Exit();

  if (Now >= Self.toggleQueue.Peek.time) then
  begin
    try
      Self.ProcessToggleFunc(Self.toggleQueue.Dequeue());
    except

    end;
  end;
end;

procedure TTrakce.FlushToggleQueue();
begin
  while (Self.toggleQueue.Count > 0) do
  begin
    try
      Self.ProcessToggleFunc(Self.toggleQueue.Dequeue());
    except

    end;
  end;
end;

procedure TTrakce.ProcessToggleFunc(hvFunc: THVToggleFunc);
begin
  if (hvFunc.HV = nil) then
    Exit();
  hvFunc.HV.SetSingleFunc(hvFunc.fIndex, false, TTrakce.callback(), TTrakce.callback());
end;

class function TTrakce.hvFunc(HV: THV; fIndex: Cardinal; time: TDateTime): THVToggleFunc;
begin
  Result.HV := HV;
  Result.fIndex := fIndex;
  Result.time := time;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.Update();
begin
  Self.CheckToggleQueue();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TTrakce.NearestLowerSpeed(speed: Cardinal): Cardinal;
begin
  for var step: Integer := _MAX_STEP downto 0 do
    if (Self.SpeedTable[step] <= speed) then
      Exit(Self.SpeedTable[step]);
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrakce.CallCb(cb: TCommandCallback);
begin
  try
    if (Assigned(cb.callback)) then
      cb.callback(Self, cb.Data);
  except
    on e: Exception do
      AppEvents.LogException(e, 'TTrakce.CallCb');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

trakce := TTrakce.Create();

finalization

// Free in hJOPserver.dpr, because we must gurantee preload gets destructed after all shared libraries

end.
