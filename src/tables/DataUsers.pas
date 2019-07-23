unit DataUsers;

// TUsersTableData - trida starajici se o vyplneni tabulky uzivatelu

interface

uses ComCtrls, SysUtils;

type
  TUsersTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable();

      procedure UpdateLine(line:Integer);

      procedure AddUser();
      procedure RemoveUser(index:Integer);

      constructor Create(LV:TListView);
  end;

var
  UsersTableData : TUsersTableData;

implementation

uses fMain, UserDb, User, Prevody;

////////////////////////////////////////////////////////////////////////////////

constructor TUsersTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.LoadToTable();
var i, j:Integer;
    LI:TListItem;
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
 end;//for i

 Self.UpdateTable();
end;

procedure TUsersTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to UsrDB.Count-1 do
   Self.UpdateLine(i);
end;

procedure TUsersTableData.UpdateLine(line:Integer);
var User:TUser;
begin
 User   := UsrDb.GetUser(line);

 Self.LV.Items[line].Caption := IntToStr(line);
 Self.LV.Items[line].SubItems[0] := User.id;
 Self.LV.Items[line].SubItems[1] := User.firstname + ' ' + User.lastname;
 Self.LV.Items[line].SubItems[2] := PrevodySoustav.BoolToStr(User.root);
 Self.LV.Items[line].SubItems[3] := FormatDateTime('yyyy-mm-dd hh:nn:ss', User.lastlogin);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.AddUser();
var LI:TListItem;
    j:Integer;
begin
 LI := Self.LV.Items.Add;
 LI.Caption := IntToStr(Self.LV.Items.Count);
 for j := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');

 Self.UpdateLine(Self.LV.Items.Count-1);
end;

procedure TUsersTableData.RemoveUser(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 UsersTableData.Free();

end.//unit
