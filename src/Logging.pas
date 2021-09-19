unit Logging;

// This unit provides functions for logging

interface

uses ComCtrls, SysUtils, Graphics, Windows, Classes;

const
  _MAX_LOGTABLE_ITEMS = 500;
  _MAIN_LOG_PATH = 'log\program';
  _AUTH_LOG_PATH = 'log\auth';

type
  LogType = (
    ltMessage,
    ltError,
    ltJC,
    ltData,
    ltRCS,
    ltSystem,
    ltConsole,
    ltTrainMove,
    ltUsers,
    ltStack,
    ltRailway,
    ltPT
  );

procedure logInit();

procedure Log(text: string; typ: LogType); overload;
procedure Log(text: TStrings; typ: LogType); overload;

procedure authLog(system, operation, user, Text: string);

var
  log_err_flag: Boolean; // true iff last log is an error
  auth_logging: Boolean; // if logging auth log

implementation

uses fMain, GetSystems, appEv;

/// /////////////////////////////////////////////////////////////////////////////

procedure logInit();
begin
  try
    if not DirectoryExists(_MAIN_LOG_PATH) then
      if not SysUtils.ForceDirectories(ExpandFileName(_MAIN_LOG_PATH)) then
        Log('ERR: Nelze vytvořit složku ' + _MAIN_LOG_PATH, ltError);
    if not DirectoryExists(_AUTH_LOG_PATH) then
      if not SysUtils.ForceDirectories(ExpandFileName(_AUTH_LOG_PATH)) then
        Log('ERR: Nelze vytvořit složku ' + _AUTH_LOG_PATH, ltError);
  except
    on e: Exception do
      AppEvents.LogException(e);
  end;

  Log('$$$$$$$$$$ Spouštím hJOPserver $$$$$$$$$$', ltMessage);
  Log('Datum ' + FormatDateTime('dd.mm.yyyy', Now), ltMessage);
  Log('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$', ltMessage);
end;

/// /////////////////////////////////////////////////////////////////////////////

function GetLogColor(typ: LogType): TColor;
begin
  case (typ) of
    ltMessage:
      Result := clWhite;
    ltError:
      Result := fMain._TABLE_COLOR_RED;
    ltJC:
      Result := RGB($FF, $FF, $D0);
    ltData:
      Result := fMain._TABLE_COLOR_BLUE;
    ltRCS:
      Result := fMain._TABLE_COLOR_GREEN;
    ltSystem:
      Result := RGB($FF, $FF, $D0);
    ltConsole:
      Result := RGB($C0, $C0, $FF);
    ltTrainMove:
      Result := RGB($CA, $FF, $CA);
    ltUsers:
      Result := RGB($F0, $F0, $D0);
    ltRailway:
      Result := clHotLight;
    ltPT:
      Result := RGB($F0, $FF, $F0);
  else
    Result := clWhite;
  end;
end;

function GetLogTyp(Typ: LogType): string;
begin
  case Typ of
    ltMessage:
      Result := 'Zpráva';
    ltError:
      Result := 'Chyba';
    ltJC:
      Result := 'Jízdní cesty';
    ltData:
      Result := 'Data';
    ltRCS:
      Result := 'RCS';
    ltSystem:
      Result := 'SYSTEM';
    ltConsole:
      Result := 'Konzole';
    ltTrainMove:
      Result := 'Předávání souprav';
    ltUsers:
      Result := 'Uživatelé';
    ltRailway:
      Result := 'Trať';
    ltStack:
      Result := 'Zásobník JC';
    ltPT:
      Result := 'PT server';
  else
    Result := 'Neznámý typ';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure intLog(text: string; typ: LogType; multiline: Boolean = false);
var f: TextFile;
  xTime, xDate: string;
begin
  if (F_Main = nil) then
    Exit();

  DateTimeToString(xDate, 'yyyy-mm-dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);
  if (multiline) then
    xTime := '';
  log_err_flag := (Typ = ltError);

  if (F_Main.LV_log.Items.Count > _MAX_LOGTABLE_ITEMS) then
    F_Main.LV_log.Clear();

  try
    if (F_Main.CHB_mainlog_table.Checked) then
    begin
      var LI: TListItem := F_Main.LV_log.Items.Insert(0);
      LI.Data := Pointer(GetLogColor(Typ));
      LI.Caption := xTime;
      LI.SubItems.Add(GetLogTyp(Typ));
      LI.SubItems.Add(Text);
    end;
    if (not multiline) then
    begin
      F_Main.SB1.Panels.Items[_SB_LOG].Text := Text;
      F_Main.sb1Log := true;
    end;
  except

  end;

  try
    if (F_Main.CHB_Mainlog_File.Checked) then
    begin
      AssignFile(f, _MAIN_LOG_PATH + '\' + xDate + '.log');
      if (FileExists(_MAIN_LOG_PATH + '\' + xDate + '.log')) then
        Append(f)
      else
        Rewrite(f);

      var output := xTime + ' [' + GetLogTyp(Typ) + '] ' + Text + #13#10;
      for var b in TEncoding.UTF8.GetBytes(output) do
        Write(f, AnsiChar(b));

      CloseFile(f);
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure Log(text: string; typ: LogType); overload;
begin
  intLog(text, typ, false);
end;

procedure Log(text: TStrings; typ: LogType); overload;
begin
  if (Text.Count = 0) then
    Exit();

  intLog(text[0], typ, false);
  for var i := 1 to Text.Count - 1 do
    intLog(' -> ' + text[i], typ, true);
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
