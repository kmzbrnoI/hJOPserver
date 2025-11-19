unit fHVEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, THnaciVozidlo, ComCtrls, Generics.Collections,
  Buttons;

type
  TF_HVEdit = class(TForm)
    E_Name: TEdit;
    E_Designation: TEdit;
    E_Owner: TEdit;
    L_HV1: TLabel;
    L_HV2: TLabel;
    L_HV3: TLabel;
    L_HV4: TLabel;
    L_HV5: TLabel;
    M_Note: TMemo;
    B_Save: TButton;
    B_Cancel: TButton;
    L_HV7: TLabel;
    CB_CabA: TComboBox;
    CB_Class: TComboBox;
    L_HV10: TLabel;
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
    OpenHV: THV;

    procedure EditOpenForm();
    procedure NewOpenForm();
    procedure CommonOpenForm();

  public
    procedure EditHV(HV: THV);
    procedure NewHV();

  end;

var
  F_HVEdit: TF_HVEdit;

implementation

uses fMain, THVDatabase, DataHV, AreaDb, Area, fHVPomEdit, BlockDb, TrainDb,
  ownGuiUtils;

{$R *.dfm}

procedure TF_HVEdit.EditHV(HV: THV);
begin
  Self.OpenHV := HV;
  Self.ActiveControl := Self.E_Name;
  Self.CommonOpenForm();

  if (HV = nil) then
    Self.NewOpenForm()
  else
    Self.EditOpenForm();

  Self.ShowModal();
end;

procedure TF_HVEdit.SB_Rel_AddClick(Sender: TObject);
begin
  F_HV_Pom.OpenForm(-1, 0);
  if (F_HV_Pom.saved) then
  begin
    var i: Integer := 0;
    while ((i < Self.LV_Pom_Manual.Items.Count) and (StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption) <
      F_HV_Pom.SE_CV.Value)) do
      Inc(i);

    if ((Assigned(Self.LV_Pom_Manual.Items.Item[i])) and (StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption)
      = F_HV_Pom.SE_CV.Value)) then
    begin
      Self.LV_Pom_Manual.Items.Item[i].SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
    end else begin
      var LI: TListItem := Self.LV_Pom_Manual.Items.Insert(i);
      LI.Caption := IntToStr(F_HV_Pom.SE_CV.Value);
      LI.SubItems.Add(IntToStr(F_HV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_HVEdit.SB_Rel_RemoveClick(Sender: TObject);
begin
  if (Self.LV_Pom_Manual.Selected <> nil) then
    Self.LV_Pom_Manual.Items.Delete(Self.LV_Pom_Manual.ItemIndex);
end;

procedure TF_HVEdit.SB_Take_AddClick(Sender: TObject);
begin
  F_HV_Pom.OpenForm(-1, 0);
  if (F_HV_Pom.saved) then
  begin
    var i: Integer := 0;
    while ((i < Self.LV_Pom_Automat.Items.Count) and (StrToInt(Self.LV_Pom_Automat.Items.Item[i].Caption) <
      F_HV_Pom.SE_CV.Value)) do
      Inc(i);

    if ((Assigned(Self.LV_Pom_Automat.Items.Item[i])) and (StrToInt(Self.LV_Pom_Automat.Items.Item[i].Caption)
      = F_HV_Pom.SE_CV.Value)) then
    begin
      Self.LV_Pom_Automat.Items.Item[i].SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
    end else begin
      var LI: TListItem := Self.LV_Pom_Automat.Items.Insert(i);
      LI.Caption := IntToStr(F_HV_Pom.SE_CV.Value);
      LI.SubItems.Add(IntToStr(F_HV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_HVEdit.SB_Take_RemoveClick(Sender: TObject);
begin
  if (Self.LV_Pom_Automat.Selected <> nil) then
    Self.LV_Pom_Automat.Items.Delete(Self.LV_Pom_Automat.ItemIndex);
end;

procedure TF_HVEdit.B_SaveClick(Sender: TObject);
var data: THVData;
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vypište název hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Class.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte typ hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_CabA.ItemIndex = -1) and (Self.CB_CabA.Visible) then
  begin
    StrMessageBox('Vyberte směr stanoviště A hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Area.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte stanici hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
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
  if ((Self.OpenHV <> nil) and (Self.OpenHV.state.train > -1) and (Self.OpenHV.state.Area <> Area)) then
    if (StrMessageBox('Měníte stanici HV, které je na vlaku, opravdu pokračovat?', 'Opravdu?',
      MB_YESNO OR MB_ICONWARNING) = mrNo) then
      Exit();

  data.name := Self.E_Name.Text;
  data.owner := Self.E_Owner.Text;
  data.designation := Self.E_Designation.Text;
  data.note := Self.M_Note.Text;
  if (Self.CB_Class.ItemIndex = Self.CB_Class.Items.Count - 1) then
    data.typ := THVType.other
  else
    data.typ := THVType(Self.CB_Class.ItemIndex);

  data.POMrelease := TPomStatus(Self.CB_POM_Release.ItemIndex);
  data.multitrackCapable := Self.CHB_Multitrack.Checked;

  if (Self.OpenHV = nil) then
  begin
    data.POMautomat := TList<THVPomCV>.Create();
    data.POMmanual := TList<THVPomCV>.Create();
  end else begin
    data.POMautomat := Self.OpenHV.data.POMautomat;
    data.POMmanual := Self.OpenHV.data.POMmanual;
  end;

  data.POMautomat.Clear();
  data.POMmanual.Clear();

  // parse POM take
  for var i: Integer := 0 to Self.LV_Pom_Automat.Items.Count - 1 do
  begin
    try
      var pomCV: THVPomCV;
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
      var pomCV: THVPomCV;
      pomCV.cv := StrToInt(Self.LV_Pom_Manual.Items.Item[i].Caption);
      pomCV.value := StrToInt(Self.LV_Pom_Manual.Items.Item[i].SubItems.Strings[0]);
      data.POMmanual.Add(pomCV);
    except

    end;
  end;

  if (Self.OpenHV = nil) then
  begin
    // vytvoreni noveho HV
    data.maxSpeed := _DEFAUT_MAX_SPEED;
    data.transience := 0;
    Area := Areas[Self.CB_Area.ItemIndex];
    try
      HVDb.Add(data, StrToInt(Self.E_Addr.Text), THVSite(Self.CB_CabA.ItemIndex), Area);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nelze přidat', E);
        Exit();
      end;
    end;
  end else begin
    // neupravovane veci jednoduse zkopirujeme
    data.funcDescription := Self.OpenHV.data.funcDescription;
    data.maxSpeed := Self.OpenHV.data.maxSpeed;
    data.funcType := Self.OpenHV.data.funcType;
    data.transience := Self.OpenHV.data.transience;

    // update HV
    Self.OpenHV.data := data;
    Self.OpenHV.SaveData();

    Area := Areas[Self.CB_Area.ItemIndex];

    var stav: THVState := Self.OpenHV.state;
    stav.siteA := THVSite(Self.CB_CabA.ItemIndex);
    Self.OpenHV.state := stav;
    Self.OpenHV.MoveToArea(Area);
    Self.OpenHV.UpdateAllRegulators();

    HVTableData.UpdateLine(OpenHV);

    if (stav.train > -1) then
      Blocks.ChangeTrainToAllRailways(Trains[stav.train]);
  end;

  Self.Close();
end;

procedure TF_HVEdit.NewHV();
begin
  Self.EditHV(nil);
end;

procedure TF_HVEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.OpenHV := nil;
end;

procedure TF_HVEdit.B_CancelClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_HVEdit.CommonOpenForm();
begin
  Self.SB_Rel_Remove.Enabled := false;
  Self.SB_Take_Remove.Enabled := false;
end;

procedure TF_HVEdit.LV_Pom_AutomatChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.SB_Take_Remove.Enabled := (Self.LV_Pom_Automat.Selected <> nil);
end;

procedure TF_HVEdit.LV_Pom_AutomatDblClick(Sender: TObject);
begin
  if (Self.LV_Pom_Automat.Selected <> nil) then
  begin
    F_HV_Pom.OpenForm(StrToInt(Self.LV_Pom_Automat.Selected.Caption),
      StrToInt(Self.LV_Pom_Automat.Selected.SubItems.Strings[0]));
    if (F_HV_Pom.saved) then
      Self.LV_Pom_Automat.Selected.SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
  end else begin
    Self.SB_Take_AddClick(Self);
  end;
end;

procedure TF_HVEdit.LV_Pom_AutomatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.SB_Take_Remove.Enabled)) then
    Self.SB_Take_RemoveClick(Self);
end;

procedure TF_HVEdit.LV_Pom_ManualChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.SB_Rel_Remove.Enabled := (Self.LV_Pom_Manual.Selected <> nil);
end;

procedure TF_HVEdit.LV_Pom_ManualDblClick(Sender: TObject);
begin
  if (Self.LV_Pom_Manual.Selected <> nil) then
  begin
    F_HV_Pom.OpenForm(StrToInt(Self.LV_Pom_Manual.Selected.Caption),
      StrToInt(Self.LV_Pom_Manual.Selected.SubItems.Strings[0]));
    if (F_HV_Pom.saved) then
      Self.LV_Pom_Manual.Selected.SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
  end else begin
    Self.SB_Rel_AddClick(Self);
  end;
end;

procedure TF_HVEdit.LV_Pom_ManualKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.SB_Rel_Remove.Enabled)) then
    Self.SB_Rel_RemoveClick(Self);
end;

procedure TF_HVEdit.EditOpenForm();
var data: THVData;
  stav: THVState;
begin
  Self.E_Addr.ReadOnly := true;

  data := Self.OpenHV.data;
  stav := Self.OpenHV.state;

  Self.E_Name.Text := data.name;
  Self.E_Designation.Text := data.designation;
  Self.E_Owner.Text := data.owner;
  Self.E_Addr.Text := IntToStr(Self.OpenHV.addr);
  Self.M_Note.Text := data.note;
  if (data.typ = THVType.other) then
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

  F_HVEdit.Caption := 'Vozidlo ' + IntToStr(Self.OpenHV.addr);
end;

procedure TF_HVEdit.NewOpenForm();
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

  F_HVEdit.Caption := 'Nové vozidlo';
end;

end.// unit
