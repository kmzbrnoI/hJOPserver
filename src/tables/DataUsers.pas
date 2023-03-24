unit DataUsers;

// TUsersTableData - trida starajici se o vyplneni tabulky uzivatelu

interface

uses ComCtrls, SysUtils, Classes;

type
  TUsersTableData = class
  private
    LV: TListView;

  public

    procedure LoadToTable();
    procedure UpdateTable();

    procedure UpdateLine(linei: Integer);

    procedure AddUser();
    procedure RemoveUser(index: Integer);

    constructor Create(LV: TListView);
  end;

var
  UsersTableData: TUsersTableData;

implementation

uses fMain, UserDb, User, ownStrUtils, StrUtils, Area, AreaDb,
  ownConvert;

/// /////////////////////////////////////////////////////////////////////////////

constructor TUsersTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.LoadToTable();
begin
  F_Main.E_Dataload_Users.text := UsrDB.filenameData;
  F_Main.E_dataload_users_stat.text := UsrDB.filenameStat;
  Self.LV.Clear();

  for var i := 0 to UsrDB.Count - 1 do
  begin
    var LI: TListItem := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for var j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('');
  end;

  Self.UpdateTable();
end;

procedure TUsersTableData.UpdateTable();
begin
  for var i := 0 to UsrDB.Count - 1 do
    Self.UpdateLine(i);
end;

procedure TUsersTableData.UpdateLine(linei: Integer);
var user: TUser;
    line: TListItem;
begin
  user := UsrDB.GetUser(linei);
  line := Self.LV.Items[linei];

  line.Caption := IntToStr(linei);
  line.SubItems[0] := User.username;
  line.SubItems[1] := User.fullName;
  line.SubItems[2] := ''; // read
  line.SubItems[3] := ''; // write
  line.SubItems[4] := ''; // superuser
  line.SubItems[5] := ownConvert.BoolToTick(User.regulator);
  line.SubItems[6] := ownConvert.BoolToTick(User.root);
  line.SubItems[7] := EscapeNewline(User.note);
  line.SubItems[8] := FormatDateTime('yyyy-mm-dd hh:nn:ss', User.lastlogin);

  for var area: TArea in Areas do
  begin
    if (user.Areas.ContainsKey(Area.id)) then
    begin
      case (user.Areas[area.id]) of
        TAreaRights.read:
          line.SubItems[2] := line.SubItems[2] + area.id + ', ';
        TAreaRights.write:
          line.SubItems[3] := line.SubItems[3] + area.id + ', ';
        TAreaRights.superuser:
          line.SubItems[4] := line.SubItems[4] + area.id + ', ';
      end;
    end;
  end;

  for var i := 2 to 4 do
    line.SubItems[i] := LeftStr(line.SubItems[i], Length(line.SubItems[i])-2);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUsersTableData.AddUser();
begin
  var LI: TListItem := Self.LV.Items.Add();
  LI.Caption := IntToStr(Self.LV.Items.Count);
  for var j := 0 to Self.LV.Columns.Count - 2 do
    LI.SubItems.Add('');
  Self.UpdateLine(Self.LV.Items.Count - 1);
end;

procedure TUsersTableData.RemoveUser(index: Integer);
begin
  Self.LV.Items.Delete(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization
  UsersTableData.Free();

end.// unit
