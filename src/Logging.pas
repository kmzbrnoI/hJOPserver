unit Logging;

// tato unita zjiatuje logovani

interface

uses ComCtrls, SysUtils, Graphics, Windows;

const
   //konstanty pro writelog
   WR_MESSAGE     = 0;
   WR_ERROR       = 1;
   WR_VYHYBKY     = 2;
   WR_AUTREZ      = 3;
   WR_VC          = 4;
   WR_INTELLIBOX  = 5;
   WR_DATA        = 6;
   WR_MTB         = 7;
   WR_SYSTEM      = 8;
   WR_CONSOLE     = 10;
   WR_SPRPREDAT   = 11;
   WR_USERS       = 12;
   WR_STACK       = 13;
   WR_TRAT        = 16;


procedure writeLog(Text:string;Typ:integer;ErrorID:integer = 0);//zapis do logu i do nabidky v nastaveni, log soubor...

var
  log_err_flag:boolean;              // pokud je posledni log chyba, je zde true, jinak je zde false

implementation

uses fSettings, fMain, GetSystems, RPConst;

////////////////////////////////////////////////////////////////////////////////

function GetLogColor(LogTyp:Integer; ErrorID:Integer):TColor;
 begin
  if (ErrorID > 0) then Exit(RGB($FF,$A0,$A0));

  case (LogTyp) of
   WR_MESSAGE    : Result := clWhite;
   WR_ERROR      : Result := RGB($FF,$A0,$A0);
   WR_VYHYBKY    : Result := RGB($A0,$A0,$80);
   WR_AUTREZ     : Result := RGB($A0,$FF,$A0);
   WR_VC         : Result := RGB($FF,$FF,$A0);
   WR_DATA       : Result := RGB($A0,$FF,$FF);
   WR_MTB        : Result := RGB($80,$FF,$80);
   WR_SYSTEM     : Result := RGB($E0,$E0,$FF);
   WR_CONSOLE    : Result := RGB($A0,$A0,$FF);
   WR_SPRPREDAT  : Result := RGB($AA,$FF,$AA);
   WR_USERS      : Result := RGB($F0,$F0,$C0);
   WR_TRAT       : Result := clHotLight;
  else//case
   Result := clWhite;
  end;//else case
 end;//function

function GetWriteLogTyp(Typ:Integer):string;
 begin
  case Typ of
   WR_MESSAGE:   Result := 'Zpráva';
   WR_ERROR:     Result := 'Chyba';
   WR_VYHYBKY:   Result := 'Vyhýbky';
   WR_AUTREZ:    Result := 'Automatický režim';
   WR_VC:        Result := 'Jízdní cesty';
   WR_INTELLIBOX:Result := 'Intellibox';
   WR_DATA:      Result := 'Data';
   WR_MTB:       Result := 'MTB';
   WR_SYSTEM:    Result := 'SYSTEM';
   WR_CONSOLE:   Result := 'Konzole';
   WR_SPRPREDAT: Result := 'Pøedávání souprav';
   WR_USERS:     Result := 'Uživatelé';
   WR_TRAT:      Result := 'Tra';
   WR_STACK:     Result := 'Zásobník JC';
  else//case
   Result := 'Neznámý typ';
  end;//else case
 end;//function

////////////////////////////////////////////////////////////////////////////////

procedure WriteLog(Text:string;Typ:integer;ErrorID:integer = 0);   //writelog
var LV:TListItem;
    f:TextFile;
    xTime,xDate:string;
 begin
  DateTimeToString(xDate, 'yy_mm_dd', Now);
  DateTimeToString(xTime, 'hh:mm:ss', Now);

  if (typ = WR_ERROR) then
   log_err_flag := true
  else
   log_err_flag := false;

  if (F_Main.LV_log.Items.Count > 500) then
    F_Main.LV_log.Clear();

  try
    if (F_Main.CHB_mainlog_table.Checked) then
     begin
      LV := F_Main.LV_log.Items.Insert(0);
      LV.Data    := Pointer(GetLogColor(Typ, ErrorID));
      LV.Caption := xtime;
      LV.SubItems.Add(text);
      LV.SubItems.Add(GetWriteLogTyp(Typ));
      if (ErrorID > 0) then
       begin
        LV.SubItems.Add(Inttostr(ErrorID));
       end else begin
        LV.SubItems.Add('X');
       end;
     end;//if mainlog.Checked
    F_Main.SB1.Panels.Items[_SB_LOG].Text:=text;
    Log := true;
  except

  end;

  try
    if (F_Main.CHB_Mainlog_File.Checked) then
     begin
      AssignFile(f, 'log\program\'+xDate+'.log');
      if FileExists('log\program\'+xDate+'.log') then
        Append(f)
       else
        Rewrite(f);

      writeln(f, xtime+';'+GetWriteLogTyp(Typ)+';'+InttoStr(ErrorID)+';'+Text+';'+ColorToString(GetLogColor(Typ, ErrorID))+';');

      CloseFile(f);
     end;
  except

  end;

 end;//procedure

end.//unit
