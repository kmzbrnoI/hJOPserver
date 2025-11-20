unit fVehicleEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, TRailVehicle, ComCtrls, Generics.Collections,
  Buttons;

type
  TF_RVEdit = class(TForm)
    E_Name: TEdit;
    E_Designation: TEdit;
    E_Owner: TEdit;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    M_Note: TMemo;
    B_Save: TButton;
    B_Cancel: TButton;
    Label12: TLabel;
    CB_CabA: TComboBox;
    CB_Class: TComboBox;
    Label7: TLabel;
    E_Addr: TEdit;
    CB_Area: TComboBox;
    Label1: TLabel;
    LV_Pom_Automat: TListView;
    Label2: TLabel;
    Label3: TLabel;
    LV_Pom_Manual: TListView;
    SB_Rel_Remove: TSpeedButton;
    SB_Rel_Add: TSpeedButton;
    SB_Take_Add: TSpeedButton;
    SB_Take_Remove: TSpeedButton;
    Label4: TLabel;
    CB_POM_Release: TComboBox;
    Label5: TLabel;
    CHB_Multitrack: TCheckBox;
    procedure B_SaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_CancelClick(Sender: TObject);
    procedure LV_Pom_AutomatDblClick(Sender: TObject);
    procedure SB_Take_AddClick(Sender: TObject);
    procedure SB_Take_RemoveClick(Sender: TObject);
    procedure LV_Pom_AutomatChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure SB_Rel_AddClick(Sender: TObject);
    procedure SB_Rel_RemoveClick(Sender: TObject);
    procedure LV_Pom_ManualChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_Pom_ManualDblClick(Sender: TObject);
    procedure LV_Pom_AutomatKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_Pom_ManualKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    vehicle: TRV;

    procedure EditOpenForm();
    procedure NewOpenForm();
    procedure CommonOpenForm();

  public
    procedure EditVehicle(vehicle: TRV);
    procedure NewVehicle();

  end;

var
  F_RVEdit: TF_RVEdit;

implementation

uses fMain, TRVDatabase, DataRV, AreaDb, Area, fVehiclePomEdit, BlockDb, TrainDb,
  ownGuiUtils;

{$R *.dfm}

procedure TF_RVEdit.EditVehicle(vehicle: TRV);
begin
  Self.vehicle := vehicle;
  Self.ActiveControl := Self.E_Name;
  Self.CommonOpenForm();

  if (vehicle = nil) then
    Self.NewOpenForm()
  else
    Self.EditOpenForm();

  Self.ShowModal();
end;

procedure TF_RVEdit.SB_Rel_AddClick(Sender: TObject);
begin
  F_RV_Pom.OpenForm(-1, 0);
  if (F_RV_Pom.saved) then
  begin
    var i: Integer := 0;
    while ((i < Self.LV_Pom_Manual.Items.Count) and (StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption) <
      F_RV_Pom.SE_CV.Value)) do
      Inc(i);

    if ((Assigned(Self.LV_Pom_Manual.Items.Item[i])) and (StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption)
      = F_RV_Pom.SE_CV.Value)) then
    begin
      Self.LV_Pom_Manual.Items.Item[i].SubItems.Strings[0] := IntToStr(F_RV_Pom.SE_Value.Value);
    end else begin
      var LI: TListItem := Self.LV_Pom_Manual.Items.Insert(i);
      LI.Caption := IntToStr(F_RV_Pom.SE_CV.Value);
      LI.SubItems.Add(IntToStr(F_RV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_RVEdit.SB_Rel_RemoveClick(Sender: TObject);
begin
  if (Self.LV_Pom_Manual.Selected <> nil) then
    Self.LV_Pom_Manual.Items.Delete(Self.LV_Pom_Manual.ItemIndex);
end;

procedure TF_RVEdit.SB_Take_AddClick(Sender: TObject);
begin
  F_RV_Pom.OpenForm(-1, 0);
  if (F_RV_Pom.saved) then
  begin
    var i: Integer := 0;
    while ((i < Self.LV_Pom_Automat.Items.Count) and (StrToInt(Self.LV_Pom_Automat.Items.Item[i].Caption) <
      F_RV_Pom.SE_CV.Value)) do
      Inc(i);

    if ((Assigned(Self.LV_Pom_Automat.Items.Item[i])) and (StrToInt(Self.LV_Pom_Automat.Items.Item[i].Caption)
      = F_RV_Pom.SE_CV.Value)) then
    begin
      Self.LV_Pom_Automat.Items.Item[i].SubItems.Strings[0] := IntToStr(F_RV_Pom.SE_Value.Value);
    end else begin
      var LI: TListItem := Self.LV_Pom_Automat.Items.Insert(i);
      LI.Caption := IntToStr(F_RV_Pom.SE_CV.Value);
      LI.SubItems.Add(IntToStr(F_RV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_RVEdit.SB_Take_RemoveClick(Sender: TObject);
begin
  if (Self.LV_Pom_Automat.Selected <> nil) then
    Self.LV_Pom_Automat.Items.Delete(Self.LV_Pom_Automat.ItemIndex);
end;

procedure TF_RVEdit.B_SaveClick(Sender: TObject);
var data: TRVData;
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vypište název vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Class.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte typ vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_CabA.ItemIndex = -1) and (Self.CB_CabA.Visible) then
  begin
    StrMessageBox('Vyberte směr stanoviště A vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Area.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte stanici vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((not Self.E_Addr.ReadOnly) and (Self.E_Addr.Text = '')) then
  begin
    StrMessageBox('Vyplňte DCC adresu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_POM_Release.ItemIndex < 0) then
  begin
    StrMessageBox('Vyberte POM při uvolnění!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var area: TArea := Areas[Self.CB_Area.ItemIndex];
  if ((Self.vehicle <> nil) and (Self.vehicle.state.train > -1) and (Self.vehicle.state.Area <> Area)) then
    if (StrMessageBox('Měníte stanici vozidla, které je na vlaku, opravdu pokračovat?', 'Opravdu?',
      MB_YESNO OR MB_ICONWARNING) = mrNo) then
      Exit();

  data.name := Self.E_Name.Text;
  data.owner := Self.E_Owner.Text;
  data.designation := Self.E_Designation.Text;
  data.note := Self.M_Note.Text;
  if (Self.CB_Class.ItemIndex = Self.CB_Class.Items.Count - 1) then
    data.typ := TRVType.other
  else
    data.typ := TRVType(Self.CB_Class.ItemIndex);

  data.POMrelease := TPomStatus(Self.CB_POM_Release.ItemIndex);
  data.multitrackCapable := Self.CHB_Multitrack.Checked;

  if (Self.vehicle = nil) then
  begin
    data.POMautomat := TList<TRVPomCV>.Create();
    data.POMmanual := TList<TRVPomCV>.Create();
  end else begin
    data.POMautomat := Self.vehicle.data.POMautomat;
    data.POMmanual := Self.vehicle.data.POMmanual;
  end;

  data.POMautomat.Clear();
  data.POMmanual.Clear();

  // parse POM take
  for var i: Integer := 0 to Self.LV_Pom_Automat.Items.Count - 1 do
  begin
    try
      var pomCV: TRVPomCV;
      pomCV.cv := StrToInt(Self.LV_Pom_Automat.Items.Item[i].Caption);
      pomCV.value := StrToInt(Self.LV_Pom_Automat.Items.Item[i].SubItems.Strings[0]);
      data.POMautomat.Add(pomCV);
    except

    end;
  end;

  // parse POM release
  for var i: Integer := 0 to Self.LV_Pom_Manual.Items.Count - 1 do
  begin
    try
      var pomCV: TRVPomCV;
      pomCV.cv := StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption);
      pomCV.value := StrToInt(Self.LV_Pom_Manual.Items.Item[i].SubItems.Strings[0]);
      data.POMmanual.Add(pomCV);
    except

    end;
  end;

  if (Self.vehicle = nil) then
  begin
    // vytvoreni noveho vozidla
    data.maxSpeed := _DEFAUT_MAX_SPEED;
    data.transience := 0;
    Area := Areas[Self.CB_Area.ItemIndex];
    try
      RVDb.Add(data, StrToInt(Self.E_Addr.Text), TRVSite(Self.CB_CabA.ItemIndex), Area);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nelze přidat', E);
        Exit();
      end;
    end;
  end else begin
    // neupravovane veci jednoduse zkopirujeme
    data.funcDescription := Self.vehicle.data.funcDescription;
    data.maxSpeed := Self.vehicle.data.maxSpeed;
    data.funcType := Self.vehicle.data.funcType;
    data.transience := Self.vehicle.data.transience;

    // update vehicle
    Self.vehicle.data := data;
    Self.vehicle.SaveData();

    Area := Areas[Self.CB_Area.ItemIndex];

    var stav: TRVState := Self.vehicle.state;
    stav.siteA := TRVSite(Self.CB_CabA.ItemIndex);
    Self.vehicle.state := stav;
    Self.vehicle.MoveToArea(Area);
    Self.vehicle.UpdateAllRegulators();

    RVTableData.UpdateLine(vehicle);

    if (stav.train > -1) then
      Blocks.ChangeTrainToAllRailways(Trains[stav.train]);
  end;

  Self.Close();
end;

procedure TF_RVEdit.NewVehicle();
begin
  Self.EditVehicle(nil);
end;

procedure TF_RVEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.vehicle := nil;
end;

procedure TF_RVEdit.B_CancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_RVEdit.CommonOpenForm();
begin
  Self.SB_Rel_Remove.Enabled := false;
  Self.SB_Take_Remove.Enabled := false;
end;

procedure TF_RVEdit.LV_Pom_AutomatChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.SB_Take_Remove.Enabled := (Self.LV_Pom_Automat.Selected <> nil);
end;

procedure TF_RVEdit.LV_Pom_AutomatDblClick(Sender: TObject);
begin
  if (Self.LV_Pom_Automat.Selected <> nil) then
  begin
    F_RV_Pom.OpenForm(StrToInt(Self.LV_Pom_Automat.Selected.Caption),
      StrToInt(Self.LV_Pom_Automat.Selected.SubItems.Strings[0]));
    if (F_RV_Pom.saved) then
      Self.LV_Pom_Automat.Selected.SubItems.Strings[0] := IntToStr(F_RV_Pom.SE_Value.Value);
  end else begin
    Self.SB_Take_AddClick(Self);
  end;
end;

procedure TF_RVEdit.LV_Pom_AutomatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.SB_Take_Remove.Enabled)) then
    Self.SB_Take_RemoveClick(Self);
end;

procedure TF_RVEdit.LV_Pom_ManualChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.SB_Rel_Remove.Enabled := (Self.LV_Pom_Manual.Selected <> nil);
end;

procedure TF_RVEdit.LV_Pom_ManualDblClick(Sender: TObject);
begin
  if (Self.LV_Pom_Manual.Selected <> nil) then
  begin
    F_RV_Pom.OpenForm(StrToInt(Self.LV_Pom_Manual.Selected.Caption),
      StrToInt(Self.LV_Pom_Manual.Selected.SubItems.Strings[0]));
    if (F_RV_Pom.saved) then
      Self.LV_Pom_Manual.Selected.SubItems.Strings[0] := IntToStr(F_RV_Pom.SE_Value.Value);
  end else begin
    Self.SB_Rel_AddClick(Self);
  end;
end;

procedure TF_RVEdit.LV_Pom_ManualKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.SB_Rel_Remove.Enabled)) then
    Self.SB_Rel_RemoveClick(Self);
end;

procedure TF_RVEdit.EditOpenForm();
var data: TRVData;
  stav: TRVState;
begin
  Self.E_Addr.ReadOnly := true;

  data := Self.vehicle.data;
  stav := Self.vehicle.state;

  Self.E_Name.Text := data.name;
  Self.E_Designation.Text := data.designation;
  Self.E_Owner.Text := data.owner;
  Self.E_Addr.Text := IntToStr(Self.vehicle.addr);
  Self.M_Note.Text := data.note;
  if (data.typ = TRVType.other) then
    Self.CB_Class.ItemIndex := Self.CB_Class.Items.Count - 1
  else
    Self.CB_Class.ItemIndex := Integer(data.typ);
  Self.CB_CabA.ItemIndex := Integer(stav.siteA);
  Self.CB_POM_Release.ItemIndex := Integer(data.POMrelease);
  Self.CHB_Multitrack.Checked := data.multitrackCapable;

  Self.LV_Pom_Automat.Clear();
  for var i: Integer := 0 to data.POMautomat.Count - 1 do
  begin
    var LI: TListItem := Self.LV_Pom_Automat.Items.Add;
    LI.Caption := IntToStr(data.POMautomat[i].cv);
    LI.SubItems.Add(IntToStr(data.POMautomat[i].value));
  end;

  Self.LV_Pom_Manual.Clear();
  for var i: Integer := 0 to data.POMmanual.Count - 1 do
  begin
    var LI: TListItem := Self.LV_Pom_Manual.Items.Add;
    LI.Caption := IntToStr(data.POMmanual[i].cv);
    LI.SubItems.Add(IntToStr(data.POMmanual[i].value));
  end;

  Areas.FillCB(Self.CB_Area, stav.Area);

  Self.Caption := 'Vozidlo ' + IntToStr(Self.vehicle.addr);
end;

procedure TF_RVEdit.NewOpenForm();
begin
  Self.E_Addr.ReadOnly := false;

  Self.E_Name.Text := '';
  Self.E_Designation.Text := '';
  Self.E_Owner.Text := '';
  Self.E_Addr.Text := '';
  Self.M_Note.Text := '';
  Self.CB_Class.ItemIndex := -1;
  Self.CB_CabA.ItemIndex := -1;
  Self.CB_POM_Release.ItemIndex := -1;
  Self.CHB_Multitrack.Checked := True;

  Self.LV_Pom_Automat.Clear();
  Self.LV_Pom_Manual.Clear();

  Areas.FillCB(Self.CB_Area, nil);

  Self.Caption := 'Nové vozidlo';
end;

end.// unit
