unit Logging;

// This unit provides functions for logging

interface

uses ComCtrls, SysUtils, Graphics, Windows, Classes;

const
  _MAX_LOGTABLE_ITEMS = 500;
  _MAIN_LOG_PATH = 'log\program';
  _AUTH_LOG_PATH = 'log\auth';

type
  TLogLevel = (
    llNone = 0,
    llError = 1,
    llWarning = 2,
    llInfo = 3,
    llDetail = 4,
    llDebug = 5
  );

  TLogSource = (
    lsAny,
    lsSystem,
    lsJC,
    lsData,
    lsRCS,
    lsTrakce,
    lsConsole,
    lsStack,
    lsPanelServer,
    lsPTServer,
    lsUDPDiscover
  );

procedure logInit();

procedure log(text: string; level: TLogLevel; source: TLogSource = lsAny; brief: Boolean = False); overload;
procedure log(text: TStrings; level: TLogLevel; source: TLogSource = lsAny; brief: Boolean = False); overload;

procedure authLog(system, operation, user, Text: string);

var
  log_last_error: Boolean; // true iff last log is an error
  auth_logging: Boolean; // if logging auth log

function logColor(level: TLogLevel; source: TLogSource): TColor;
function logLevelStr(level: TLogLevel): string;
function logSourceStr(source: TLogSource): string;
procedure intLog(text: string; level: TLogLevel; source: TLogSource; multiline: Boolean = false; brief: Boolean = False);

implementation

uses fMain, GetSystems, appEv, version;

/// /////////////////////////////////////////////////////////////////////////////

procedure logInit();
begin
  try
    if not DirectoryExists(_MAIN_LOG_PATH) then
      if not SysUtils.ForceDirectories(ExpandFileName(_MAIN_LOG_PATH)) then
        Log('ERR: Nelze vytvořit složku ' + _MAIN_LOG_PATH, llError);
    if not DirectoryExists(_AUTH_LOG_PATH) then
      if not SysUtils.ForceDirectories(ExpandFileName(_AUTH_LOG_PATH)) then
        Log('ERR: Nelze vytvořit složku ' + _AUTH_LOG_PATH, llError);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  Log('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$', llInfo);
  Log('Spouštím hJOPserver '+version.StandardVersionBuildStr()+', datum: '+FormatDateTime('dd.mm.yyyy', Now), llInfo);
end;

/// /////////////////////////////////////////////////////////////////////////////

function logColor(level: TLogLevel; source: TLogSource): TColor;
begin
  case (level) of
    llError:
      Exit(fMain._TABLE_COLOR_RED);
    llWarning:
      Exit(fMain._TABLE_COLOR_YELLOW);
  end;

  case (source) of
    lsSystem, lsJC:
      Result := RGB($C0, $FD, $C3);
    lsData:
      Result := fMain._TABLE_COLOR_BLUE;
    lsRCS:
      Result := fMain._TABLE_COLOR_GREEN;
    lsTrakce:
      Result := RGB($FF, $F0, $FF);
    lsConsole:
      Result := RGB($C0, $C0, $FF);
    lsPTServer, lsPanelServer:
      Result := RGB($F0, $FF, $F0);
  else
    Result := clWhite;
  end;
end;

function logLevelStr(level: TLogLevel): string;
begin
  case (level) of
    llNone:
      Result := '-';
    llError:
      Result := 'error';
    llWarning:
      Result := 'warning';
    llInfo:
      Result := 'info';
    llDetail:
      Result := 'detail';
    llDebug:
      Result := 'debug';
  else
    Result := '?';
  end;
end;

function logSourceStr(source: TLogSource): string;
begin
  case (source) of
    lsAny:
      Result := '';
    lsSystem:
      Result := 'SYSTEM';
    lsJC:
      Result := 'JC';
    lsData:
      Result := 'Data';
    lsRCS:
      Result := 'RCS';
    lsTrakce:
      Result := 'Trakce';
    lsConsole:
      Result := 'Konzole';
    lsStack:
      Result := 'Zásobník';
    lsPTServer:
      Result := 'PTServer';
    lsPanelServer:
      Result := 'PanelServer';
    lsUDPDiscover:
      Result := 'UDP Discover';
  else
    Result := '?';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure intLog(text: string; level: TLogLevel; source: TLogSource; multiline: Boolean = false; brief: Boolean = False);
var f: TextFile;
  xTime, xDate: string;
begin
  if (F_Main = nil) then
    Exit();

  DateTimeToString(xDate, 'yyyy-mm-dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);
  if (multiline) then
    xTime := '';
  log_last_error := (level = llError);

  if (F_Main.LV_log.Items.Count > _MAX_LOGTABLE_ITEMS) then
    F_Main.LV_log.Clear();

  try
    if (Integer(level) <= F_Main.CB_global_loglevel_table.ItemIndex) then
    begin
      var LI: TListItem := F_Main.LV_log.Items.Insert(0);
      LI.Data := Pointer(logColor(level, source));
      LI.Caption := xTime;
      LI.SubItems.Add(logLevelStr(level));
      LI.SubItems.Add(logSourceStr(source));
      LI.SubItems.Add(text);
      if (not multiline) then
        F_main.SB1Log(text);
    end;
  except

  end;

  try
    if (Integer(level) <= F_Main.CB_global_loglevel_file.ItemIndex) then
    begin
      AssignFile(f, _MAIN_LOG_PATH + '\' + xDate + '.log');
      if (FileExists(_MAIN_LOG_PATH + '\' + xDate + '.log')) then
        Append(f)
      else
        Rewrite(f);

      var output := xTime + ' [' + logLevelStr(level) + '] (' + logSourceStr(source) + ') ' + text + #13#10;
      for var b in TEncoding.UTF8.GetBytes(output) do
        Write(f, AnsiChar(b));

      CloseFile(f);
    end;
  except

  end;


  try
    if (brief) then
      F_Main.LogBrief(text, level);
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure Log(text: string; level: TLogLevel; source: TLogSource; brief: Boolean); overload;
begin
  intLog(text, level, source, false, brief);
end;

procedure Log(text: TStrings; level: TLogLevel; source: TLogSource; brief: Boolean); overload;
begin
  if (Text.Count = 0) then
    Exit();

  intLog(text[0], level, source, false);
  for var i := 1 to Text.Count - 1 do
    intLog(' -> ' + text[i], level, source, true, brief);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure authLog(system, operation, user, text: string);
var f: TextFile;
  time, date: string;
begin
  if (not auth_logging) then
    Exit();

  DateTimeToString(date, 'yyyy-mm-dd', Now);
  DateTimeToString(time, 'hh:mm:ss', Now);

  try
    AssignFile(f, _AUTH_LOG_PATH + '\' + date + '.log');
    if (FileExists(_AUTH_LOG_PATH + '\' + date + '.log')) then
      Append(f)
    else
      Rewrite(f);

    var line := time + ' {' + UpperCase(system) + '} [' + UpperCase(operation) + '] ';
    if (user <> '') then
      line := line + '<' + user + '> ';
    line := line + Text + #13#10;
    for var b in TEncoding.UTF8.GetBytes(line) do
      Write(f, AnsiChar(b));

    CloseFile(f);
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization
  auth_logging := false;

end.
