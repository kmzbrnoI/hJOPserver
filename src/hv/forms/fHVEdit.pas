unit fHVEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, THnaciVozidlo, ComCtrls, Generics.Collections,
  Buttons;

type
  TF_HVEdit = class(TForm)
    E_Nazev: TEdit;
    E_Oznaceni: TEdit;
    E_Majitel: TEdit;
    L_HV1: TLabel;
    L_HV2: TLabel;
    L_HV3: TLabel;
    L_HV4: TLabel;
    L_HV5: TLabel;
    M_Poznamky: TMemo;
    B_Save: TButton;
    B_Storno: TButton;
    B_TachoClear: TButton;
    L_HV7: TLabel;
    CB_Orientace: TComboBox;
    CB_Trida: TComboBox;
    L_HV10: TLabel;
    E_Addr: TEdit;
    CB_OR: TComboBox;
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
    procedure B_SaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_StornoClick(Sender: TObject);
    procedure B_TachoClearClick(Sender: TObject);
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

    procedure NormalOpenForm;
    procedure NewHVOpenForm;
    procedure HlavniOpenForm;

  public

    procedure OpenForm(HV: THV);
    procedure NewHV();
  end;

var
  F_HVEdit: TF_HVEdit;

implementation

uses fMain, THVDatabase, DataHV, AreaDb, Area, fHVPomEdit, BlockDb, TrainDb,
  ownGuiUtils;

{$R *.dfm}

procedure TF_HVEdit.OpenForm(HV: THV);
begin
  Self.OpenHV := HV;
  Self.ActiveControl := Self.E_Nazev;
  Self.HlavniOpenForm();

  if (HV = nil) then
    Self.NewHVOpenForm()
  else
    Self.NormalOpenForm();

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
  if (Self.E_Nazev.Text = '') then
  begin
    StrMessageBox('Vypište název hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Trida.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte typ hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Orientace.ItemIndex = -1) and (CB_Orientace.Visible) then
  begin
    StrMessageBox('Vyberte směr stanoviště A hnacího vozidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_OR.ItemIndex = -1) then
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

  var area: TArea := Areas[Self.CB_OR.ItemIndex];
  if ((Self.OpenHV <> nil) and (Self.OpenHV.state.train > -1) and (Self.OpenHV.state.Area <> Area)) then
    if (StrMessageBox('Měníte stanici HV, které je na soupravě, opravdu pokračovat?', 'Opravdu?',
      MB_YESNO OR MB_ICONWARNING) = mrNo) then
      Exit();

  data.name := E_Nazev.Text;
  data.owner := E_Majitel.Text;
  data.designation := E_Oznaceni.Text;
  data.note := M_Poznamky.Text;
  if (CB_Trida.ItemIndex = CB_Trida.Items.Count - 1) then
    data.typ := THVType.other
  else
    data.typ := THVType(CB_Trida.ItemIndex);

  data.POMrelease := TPomStatus(Self.CB_POM_Release.ItemIndex);

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
      pomCV.data := StrToInt(Self.LV_Pom_Automat.Items.Item[i].SubItems.Strings[0]);
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
      pomCV.data := StrToInt(Self.LV_Pom_Manual.Items.Item[i].SubItems.Strings[0]);
      data.POMmanual.Add(pomCV);
    except

    end;
  end;

  if (Self.OpenHV = nil) then
  begin
    // vytvoreni noveho HV
    data.maxSpeed := _DEFAUT_MAX_SPEED;
    data.transience := 0;
    Area := Areas[Self.CB_OR.ItemIndex];
    try
      HVDb.Add(data, StrToInt(Self.E_Addr.Text), THVSite(CB_Orientace.ItemIndex), Area);
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

    Area := Areas[Self.CB_OR.ItemIndex];

    var stav: THVState := Self.OpenHV.state;
    stav.siteA := THVSite(CB_Orientace.ItemIndex);
    Self.OpenHV.state := stav;
    Self.OpenHV.MoveToArea(Area);
    Self.OpenHV.UpdateAllRegulators();

    HVTableData.UpdateLine(OpenHV);

    if (stav.train > -1) then
      Blocks.ChangeTrainToRailway(Trains[stav.train]);
  end;

  Self.Close();
end;

procedure TF_HVEdit.NewHV();
begin
  Self.OpenForm(nil);
end;

procedure TF_HVEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.OpenHV := nil;
end;

procedure TF_HVEdit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_HVEdit.B_TachoClearClick(Sender: TObject);
begin
  if (Self.OpenHV = nil) then
    Exit();

  OpenHV.ResetStats();
  HVTableData.UpdateLine(Self.OpenHV);

  StrMessageBox('Operace proběhla úspěšně.', 'OK', MB_OK OR MB_ICONINFORMATION);
end;

procedure TF_HVEdit.HlavniOpenForm;
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

procedure TF_HVEdit.NormalOpenForm();
var data: THVData;
  stav: THVState;
begin
  Self.B_TachoClear.Enabled := true;
  Self.E_Addr.ReadOnly := true;

  data := Self.OpenHV.data;
  stav := Self.OpenHV.state;

  Self.E_Nazev.Text := data.name;
  Self.E_Oznaceni.Text := data.designation;
  Self.E_Majitel.Text := data.owner;
  Self.E_Addr.Text := IntToStr(Self.OpenHV.addr);
  Self.M_Poznamky.Text := data.note;
  if (data.typ = THVType.other) then
    Self.CB_Trida.ItemIndex := CB_Trida.Items.Count - 1
  else
    Self.CB_Trida.ItemIndex := Integer(data.typ);
  Self.CB_Orientace.ItemIndex := Integer(stav.siteA);
  Self.CB_POM_Release.ItemIndex := Integer(data.POMrelease);

  Self.LV_Pom_Automat.Clear();
  for var i: Integer := 0 to data.POMautomat.Count - 1 do
  begin
    var LI: TListItem := Self.LV_Pom_Automat.Items.Add;
    LI.Caption := IntToStr(data.POMautomat[i].cv);
    LI.SubItems.Add(IntToStr(data.POMautomat[i].data));
  end;

  Self.LV_Pom_Manual.Clear();
  for var i: Integer := 0 to data.POMmanual.Count - 1 do
  begin
    var LI: TListItem := Self.LV_Pom_Manual.Items.Add;
    LI.Caption := IntToStr(data.POMmanual[i].cv);
    LI.SubItems.Add(IntToStr(data.POMmanual[i].data));
  end;

  Areas.FillCB(Self.CB_OR, stav.Area);

  F_HVEdit.Caption := 'HV ' + IntToStr(Self.OpenHV.addr);
end;

procedure TF_HVEdit.NewHVOpenForm();
begin
  Self.B_TachoClear.Enabled := false;
  Self.E_Addr.ReadOnly := false;

  Self.E_Nazev.Text := '';
  Self.E_Oznaceni.Text := '';
  Self.E_Majitel.Text := '';
  Self.E_Addr.Text := '';
  Self.M_Poznamky.Text := '';
  Self.CB_Trida.ItemIndex := -1;
  Self.CB_Orientace.ItemIndex := -1;
  Self.CB_POM_Release.ItemIndex := -1;

  Self.LV_Pom_Automat.Clear();
  Self.LV_Pom_Manual.Clear();

  Areas.FillCB(Self.CB_OR, nil);

  F_HVEdit.Caption := 'Nové hnací vozidlo';
end;

end.// unit
