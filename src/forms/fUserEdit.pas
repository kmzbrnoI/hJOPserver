unit fUserEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, User, ComCtrls;

type
  TF_UserEdit = class(TForm)
    Label1: TLabel;
    E_UserName: TEdit;
    Label2: TLabel;
    E_FirstName: TEdit;
    Label3: TLabel;
    E_LastName: TEdit;
    GB_Password: TGroupBox;
    Label4: TLabel;
    E_Password1: TEdit;
    Label5: TLabel;
    E_Password2: TEdit;
    CHB_root: TCheckBox;
    B_Apply: TButton;
    B_Cancel: TButton;
    GroupBox1: TGroupBox;
    LV_ORs: TListView;
    CB_Rights: TComboBox;
    CHB_Ban: TCheckBox;
    CHB_Reg: TCheckBox;
    M_Note: TMemo;
    Label6: TLabel;
    procedure B_CancelClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure LV_ORsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CB_RightsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_ORsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    openUser: TUser;
    new: Boolean;

    procedure FillORs();

  public
    procedure OpenForm(User: TUser);
    procedure NewUser();
  end;

var
  F_UserEdit: TF_UserEdit;

implementation

{$R *.dfm}

uses UserDb, DataUsers, AreaDb, Area, fMain;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_UserEdit.B_CancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_UserEdit.CB_RightsChange(Sender: TObject);
var i: Integer;
begin
  for i := 0 to Self.LV_ORs.Items.Count - 1 do
  begin
    if (Self.LV_ORs.Items[i].Selected) then
    begin
      Self.openUser.SetRights(Self.LV_ORs.Items[i].Caption, TAreaRights(Self.CB_Rights.ItemIndex));
      Self.LV_ORs.Items[i].SubItems.Strings[1] := TArea.ORRightsToString(TAreaRights(Self.CB_Rights.ItemIndex));
      TAreaRights(Self.LV_ORs.Items[i].Data^) := TAreaRights(Self.CB_Rights.ItemIndex);
    end;
  end;

  Self.LV_ORs.Repaint();
end;

procedure TF_UserEdit.FormClose(Sender: TObject; var Action: TCloseAction);
var i: Integer;
begin
  Self.new := false;

  for i := 0 to Self.LV_ORs.Items.Count - 1 do
    FreeMem(Self.LV_ORs.Items.Item[i].Data);
  Self.LV_ORs.Clear();
end;

procedure TF_UserEdit.LV_ORsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Self.LV_ORs.SelCount = 0) then
  begin
    // 0 vybranych polozek
    Self.CB_Rights.ItemIndex := -1;
    Self.CB_Rights.Enabled := false;
  end else begin
    Self.CB_Rights.Enabled := true;
    if (Self.LV_ORs.SelCount = 1) then
    begin
      // 1 vybrana polozka
      var rights: TAreaRights;
      if (Self.openUser.areas.TryGetValue(Self.LV_ORs.Selected.Caption, rights)) then
        Self.CB_Rights.ItemIndex := Integer(rights)
      else
        Self.CB_Rights.ItemIndex := 0;
    end else begin
      // vic vybranych polozek -> pokud jsou opravenni stejna, vyplnime, jinak -1

      var rights: TAreaRights;
      for var i := 0 to Self.LV_ORs.Items.Count - 1 do
        if (Self.LV_ORs.Items[i].Selected) then
          Self.openUser.areas.TryGetValue(Self.LV_ORs.Items[i].Caption, rights);

      for var i := 0 to Self.LV_ORs.Items.Count - 1 do
      begin
        if (Self.LV_ORs.Items[i].Selected) then
        begin
          var rights2: TAreaRights;
          Self.openUser.areas.TryGetValue(Self.LV_ORs.Items[i].Caption, rights2);
          if (rights2 <> rights) then
          begin
            Self.CB_Rights.ItemIndex := -1;
            Exit();
          end;
        end;
      end;

      Self.CB_Rights.ItemHeight := Integer(rights);
    end; // else SelCount > 1
  end; // else Selected = nil
end;

procedure TF_UserEdit.LV_ORsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  case TAreaRights(Item.Data^) of
    TAreaRights.null:
      Self.LV_ORs.Canvas.Brush.Color := clWhite;
    TAreaRights.read, TAreaRights.other:
      Self.LV_ORs.Canvas.Brush.Color := _TABLE_COLOR_BLUE;
    TAreaRights.write:
      Self.LV_ORs.Canvas.Brush.Color := _TABLE_COLOR_YELLOW;
    TAreaRights.superuser:
      Self.LV_ORs.Canvas.Brush.Color := _TABLE_COLOR_RED;
  end; // case
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TF_UserEdit.OpenForm(User: TUser);
begin
  Self.openUser := User;
  Self.FillORs();

  Self.E_UserName.Text := User.username;
  Self.E_FirstName.Text := User.firstname;
  Self.E_LastName.Text := User.lastname;
  Self.CHB_root.Checked := User.root;
  Self.CHB_Ban.Checked := User.ban;
  Self.CHB_Reg.Checked := User.regulator;
  Self.M_Note.Text := User.note;

  Self.E_Password1.Text := 'heslo';
  Self.E_Password2.Text := 'heslo';

  Self.ActiveControl := Self.E_UserName;
  Self.Caption := 'Upravit uživatele ' + User.username;
  Self.ShowModal();
end;

procedure TF_UserEdit.NewUser();
begin
  Self.new := true;
  Self.openUser := TUser.Create();
  Self.FillORs();

  Self.E_UserName.Text := '';
  Self.E_FirstName.Text := '';
  Self.E_LastName.Text := '';
  Self.CHB_root.Checked := false;
  Self.CHB_Ban.Checked := false;
  Self.CHB_Reg.Checked := true;
  Self.M_Note.Text := '';

  Self.E_Password1.Text := '';
  Self.E_Password2.Text := '';

  Self.ActiveControl := Self.E_UserName;
  Self.Caption := 'Vytvořit nového uživatele';
  Self.ShowModal();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_UserEdit.B_ApplyClick(Sender: TObject);
var index: Integer;
begin
  if (Length(Self.E_UserName.Text) < 3) then
  begin
    Application.MessageBox('Uživatelské jméno musí mít alespoň 3 znaky!', 'nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.openUser <> nil) then
  begin
    if (Self.E_Password1.Text = '') then
    begin
      Application.MessageBox('Heslo nemůže být prázdné!', 'nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  if (Self.E_Password1.Text <> Self.E_Password2.Text) then
  begin
    Application.MessageBox('Zadaná hesla se neshodují!', 'nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Length(Self.E_Password1.Text) < 3) then
  begin
    Application.MessageBox('Heslo musí mít alespoň 3 znaky!', 'nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.new) then
  begin
    Self.openUser.password := Self.E_Password1.Text;
    Self.openUser.lastlogin := Now;
  end else begin
    if (Self.E_Password1.Text <> 'heslo') then
      Self.openUser.password := Self.E_Password1.Text;
  end;

  Self.openUser.username := Self.E_UserName.Text;
  Self.openUser.firstname := Self.E_FirstName.Text;
  Self.openUser.lastname := Self.E_LastName.Text;
  Self.openUser.root := Self.CHB_root.Checked;
  Self.openUser.ban := Self.CHB_Ban.Checked;
  Self.openUser.regulator := Self.CHB_Reg.Checked;
  Self.openUser.note := Self.M_Note.Text;

  if (new) then
  begin
    try
      UsrDB.AddUser(Self.openUser);
    except
      on e: Exception do
      begin
        Application.MessageBox(PChar(e.Message), 'Varování', MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    UsersTableData.UpdateTable();
  end;

  index := UsrDB.IndexOf(Self.openUser.username);
  if (index > -1) then
  begin
    F_Main.LV_Users.Items[index].Selected := true;
    F_Main.LV_Users.Items[index].Focused := true;
  end;

  Self.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_UserEdit.FillORs();
var LI: TListItem;
  rights: TAreaRights;
  Data: Pointer;
  Area: TArea;
begin
  Self.LV_ORs.Clear();

  for Area in areas do
  begin
    LI := Self.LV_ORs.Items.Add;
    LI.Caption := Area.id;
    LI.SubItems.Add(Area.name);

    if (not Self.openUser.areas.TryGetValue(LI.Caption, rights)) then
      rights := TAreaRights.null;
    LI.SubItems.Add(TArea.ORRightsToString(rights));

    GetMem(Data, 3);
    TAreaRights(Data^) := rights;

    LI.Data := Data;
  end;

  Self.CB_Rights.ItemIndex := -1;
  Self.CB_Rights.Enabled := false;

  Self.LV_ORs.Repaint();
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
