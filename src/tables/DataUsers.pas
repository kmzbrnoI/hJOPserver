unit DataUsers;

// TUsersTableData - trida starajici se o vyplneni tabulky uzivatelu

interface

uses ComCtrls, SysUtils;

type
  TUsersTableData=class
    private
      LV: TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable();

      procedure UpdateLine(line: Integer);

      procedure AddUser();
      procedure RemoveUser(index: Integer);

      constructor Create(LV: TListView);
  end;

var
  UsersTableData : TUsersTableData;

implementation

uses fMain, UserDb, User, ownStrUtils, StrUtils, TOblRizeni, TOblsRizeni,
      ownConvert;

////////////////////////////////////////////////////////////////////////////////

constructor TUsersTableData.Create(LV: TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.LoadToTable();
var i, j: Integer;
    LI: TListItem;
begin
 F_Main.E_Dataload_Users.text := UsrDB.filenameData;
 F_Main.E_dataload_users_stat.text := UsrDB.filenameStat;
 Self.LV.Clear();

 for i := 0 to UsrDB.Count-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);
   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('');
 end;

 Self.UpdateTable();
end;

procedure TUsersTableData.UpdateTable();
var i: Integer;
begin
 for i := 0 to UsrDB.Count-1 do
   Self.UpdateLine(i);
end;

procedure TUsersTableData.UpdateLine(line: Integer);
var user: TUser;
    str: string;
    oblr: TOR;
begin
 user := UsrDb.GetUser(line);

 Self.LV.Items[line].Caption := IntToStr(line);
 Self.LV.Items[line].SubItems[0] := user.username;
 Self.LV.Items[line].SubItems[1] := user.fullName;
 Self.LV.Items[line].SubItems[3] := ownConvert.BoolToTick(user.regulator);
 Self.LV.Items[line].SubItems[4] := ownConvert.BoolToTick(user.root);
 Self.LV.Items[line].Subitems[5] := EscapeNewline(user.note);
 Self.LV.Items[line].SubItems[6] := FormatDateTime('yyyy-mm-dd hh:nn:ss', user.lastlogin);

 str := '';
 for oblr in ORs do
  begin
   if ((user.OblR.ContainsKey(oblr.id)) and (user.OblR[oblr.id] > TORControlRights.null)) then
    begin
     str := str + oblr.ShortName + ':';
     case user.OblR[oblr.id] of
       TORControlRights.read: str := str + 'R';
       TORControlRights.write: str := str + 'W';
       TORControlRights.superuser: str := str + 'S';
     end;
     str := str + ', ';
    end;
  end;

 Self.LV.Items[line].Subitems[2] := LeftStr(str, Length(str)-2);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.AddUser();
var LI: TListItem;
    j: Integer;
begin
 LI := Self.LV.Items.Add;
 LI.Caption := IntToStr(Self.LV.Items.Count);
 for j := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');

 Self.UpdateLine(Self.LV.Items.Count-1);
end;

procedure TUsersTableData.RemoveUser(index: Integer);
begin
 Self.LV.Items.Delete(index);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 UsersTableData.Free();

end.//unit
