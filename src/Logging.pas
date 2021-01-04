unit Logging;

// This unit provides functions for logging

interface

uses ComCtrls, SysUtils, Graphics, Windows, Classes;

const
   // General log types
   WR_MESSAGE = 0;
   WR_ERROR = 1;
   WR_VC = 4;
   WR_DATA = 6;
   WR_RCS = 7;
   WR_SYSTEM = 8;
   WR_CONSOLE = 10;
   WR_SPRPREDAT = 11;
   WR_USERS = 12;
   WR_STACK = 13;
   WR_TRAT = 16;
   WR_PT = 17;

   _MAX_LOGTABLE_ITEMS = 500;
   _MAIN_LOG_PATH = 'log\program';
   _AUTH_LOG_PATH = 'log\auth';


procedure logInit();

procedure writeLog(Text: string; Typ: Integer); overload;
procedure writeLog(Text: TStrings; Typ: Integer); overload;

procedure authLog(system, operation, user, text: string);

var
  log_err_flag: Boolean; // true iff last log is an error
  auth_logging: Boolean; // if logging auth log

implementation

uses fMain, GetSystems, appEv;

////////////////////////////////////////////////////////////////////////////////

procedure logInit();
begin
 try
   if not DirectoryExists(_MAIN_LOG_PATH) then
     if not SysUtils.ForceDirectories(ExpandFileName(_MAIN_LOG_PATH)) then
       writelog('ERR: Nelze vytvořit složku '+_MAIN_LOG_PATH, WR_ERROR);
   if not DirectoryExists(_AUTH_LOG_PATH) then
     if not SysUtils.ForceDirectories(ExpandFileName(_AUTH_LOG_PATH)) then
       writelog('ERR: Nelze vytvořit složku '+_AUTH_LOG_PATH, WR_ERROR);
 except
   on e: Exception do
     AppEvents.LogException(E);
 end;

 WriteLog('$$$$$$$$$$ Spouštím hJOPserver $$$$$$$$$$', WR_MESSAGE);
 WriteLog('Datum ' + FormatDateTime('dd.mm.yyyy', Now), WR_MESSAGE);
 WriteLog('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$', WR_MESSAGE);
end;

////////////////////////////////////////////////////////////////////////////////

function GetLogColor(LogTyp: Integer): TColor;
 begin
  case (LogTyp) of
   WR_MESSAGE    : Result := clWhite;
   WR_ERROR      : Result := fMain._TABLE_COLOR_RED;
   WR_VC         : Result := RGB($FF,$FF,$D0);
   WR_DATA       : Result := fMain._TABLE_COLOR_BLUE;
   WR_RCS        : Result := fMain._TABLE_COLOR_GREEN;
   WR_SYSTEM     : Result := RGB($FF,$FF,$D0);
   WR_CONSOLE    : Result := RGB($C0,$C0,$FF);
   WR_SPRPREDAT  : Result := RGB($CA,$FF,$CA);
   WR_USERS      : Result := RGB($F0,$F0,$D0);
   WR_TRAT       : Result := clHotLight;
   WR_PT         : Result := RGB($F0,$FF,$F0);
  else
   Result := clWhite;
  end;
 end;

function GetWriteLogTyp(Typ: Integer): string;
 begin
  case Typ of
   WR_MESSAGE:   Result := 'Zpráva';
   WR_ERROR:     Result := 'Chyba';
   WR_VC:        Result := 'Jízdní cesty';
   WR_DATA:      Result := 'Data';
   WR_RCS:       Result := 'RCS';
   WR_SYSTEM:    Result := 'SYSTEM';
   WR_CONSOLE:   Result := 'Konzole';
   WR_SPRPREDAT: Result := 'Předávání souprav';
   WR_USERS:     Result := 'Uživatelé';
   WR_TRAT:      Result := 'Trať';
   WR_STACK:     Result := 'Zásobník JC';
   WR_PT:        Result := 'PT server';
  else//case
   Result := 'Neznámý typ';
  end;//else case
 end;

////////////////////////////////////////////////////////////////////////////////

procedure intWriteLog(Text: string; Typ: integer; multiline: Boolean = false);
var LV: TListItem;
    f: TextFile;
    xTime, xDate: string;
    b: Byte;
    output: string;
 begin
  if (F_Main = nil) then Exit();

  DateTimeToString(xDate, 'yyyy-mm-dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss,zzz', Now);
  if (multiline) then
    xTime := '';
  log_err_flag := (typ = WR_ERROR);

  if (F_Main.LV_log.Items.Count > _MAX_LOGTABLE_ITEMS) then
    F_Main.LV_log.Clear();

  try
    if (F_Main.CHB_mainlog_table.Checked) then
     begin
      LV := F_Main.LV_log.Items.Insert(0);
      LV.Data := Pointer(GetLogColor(Typ));
      LV.Caption := xtime;
      LV.SubItems.Add(GetWriteLogTyp(Typ));
      LV.SubItems.Add(Text);
     end;
    if (not multiline) then
     begin
      F_Main.SB1.Panels.Items[_SB_LOG].Text := text;
      F_Main.sb1Log := true;
     end;
  except

  end;

  try
    if (F_Main.CHB_Mainlog_File.Checked) then
     begin
      AssignFile(f, _MAIN_LOG_PATH+'\'+xDate+'.log');
      if (FileExists(_MAIN_LOG_PATH+'\'+xDate+'.log')) then
        Append(f)
      else
        Rewrite(f);

      output := xtime + ' ['+GetWriteLogTyp(Typ) + '] ' + Text + #13#10;
      for b in TEncoding.UTF8.GetBytes(output) do
        Write(f, AnsiChar(b));

      CloseFile(f);
     end;
  except

  end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure writeLog(Text: string; Typ: integer); overload;
begin
 intWriteLog(Text, Typ, false);
end;

procedure writeLog(Text: TStrings; Typ: integer); overload;
var i: Integer;
begin
 if (Text.Count = 0) then Exit();

 intWriteLog(Text[0], Typ, false);
 for i := 1 to Text.Count-1 do
   intWriteLog(' -> '+Text[i], Typ, true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure authLog(system, operation, user, text: string);
var f: TextFile;
    time, date: string;
    line: string;
    b: Byte;
 begin
  if (not auth_logging) then Exit();

  DateTimeToString(date, 'yyyy-mm-dd', Now);
  DateTimeToString(time, 'hh:mm:ss', Now);

  try
    AssignFile(f, _AUTH_LOG_PATH+'\'+date+'.log');
    if (FileExists(_AUTH_LOG_PATH+'\'+date+'.log')) then
      Append(f)
    else
      Rewrite(f);

    line := time + ' {' + UpperCase(system) + '} ['+UpperCase(operation)+'] ';
    if (user <> '') then
      line := line + '<' + user + '> ';
    line := line + text + #13#10;
    for b in TEncoding.UTF8.GetBytes(line) do
      Write(f, AnsiChar(b));

    CloseFile(f);
  except

  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  auth_logging := false;

end.//unit
