unit HVEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Gauges, THnaciVozidlo, ComCtrls,
  Generics.Collections, Buttons;

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
    B_NajetoDelete: TButton;
    L_HV7: TLabel;
    CB_Orientace: TComboBox;
    CB_Trida: TComboBox;
    L_HV10: TLabel;
    E_Addr: TEdit;
    CB_OR: TComboBox;
    Label1: TLabel;
    LV_Pom_Load: TListView;
    Label2: TLabel;
    Label3: TLabel;
    LV_Pom_Release: TListView;
    SB_Rel_Remove: TSpeedButton;
    SB_Rel_Add: TSpeedButton;
    SB_Take_Add: TSpeedButton;
    SB_Take_Remove: TSpeedButton;
    procedure B_SaveClick(Sender: TObject);
    procedure E_AdresaKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_StornoClick(Sender: TObject);
    procedure B_NajetoDeleteClick(Sender: TObject);
    procedure LV_Pom_LoadDblClick(Sender: TObject);
    procedure SB_Take_AddClick(Sender: TObject);
    procedure SB_Take_RemoveClick(Sender: TObject);
    procedure LV_Pom_LoadChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure SB_Rel_AddClick(Sender: TObject);
    procedure SB_Rel_RemoveClick(Sender: TObject);
    procedure LV_Pom_ReleaseChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_Pom_ReleaseDblClick(Sender: TObject);
  private
    OpenHV: THV;

    procedure NormalOpenForm;
    procedure NewHVOpenForm;
    procedure HlavniOpenForm;

  public

    procedure OpenForm(HV:THV);
    procedure NewHV();
  end;

var
  F_HVEdit: TF_HVEdit;

implementation

uses Main, FileSystem, Settings, THVDatabase, DataHV, TOblsRizeni, TOblRizeni,
      RPConst, HVPomEdit, TBloky;

{$R *.dfm}

procedure TF_HVEdit.OpenForm(HV:THV);
 begin
  OpenHV                 := HV;
  F_HVEdit.ActiveControl := Self.E_Nazev;
  HlavniOpenForm();

  if (HV = nil) then
   begin
    // nove HV
    NewHVOpenForm();
   end else begin
    //editace HV
    NormalOpenForm();
   end;//else NewHV

  F_HVEdit.ShowModal;
 end;

procedure TF_HVEdit.SB_Rel_AddClick(Sender: TObject);
var LI:TListItem;
    i:Integer;
begin
 F_HV_Pom.OpenForm(-1, 0);
 if (F_HV_Pom.saved) then
  begin
   i := 0;
   while ((i < Self.LV_Pom_Release.Items.Count) and (StrToInt(Self.LV_Pom_Release.Items.Item[i].Caption) < F_HV_Pom.SE_CV.Value)) do Inc(i);

   if ((Assigned(Self.LV_Pom_Release.Items.Item[i])) and (StrToInt(Self.LV_Pom_Release.Items.Item[i].Caption) = F_HV_Pom.SE_CV.Value)) then
    begin
     Self.LV_Pom_Release.Items.Item[i].SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
    end else begin
     LI := Self.LV_Pom_Release.Items.Insert(i);
     LI.Caption := IntToStr(F_HV_Pom.SE_CV.Value);
     LI.SubItems.Add(IntToStr(F_HV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_HVEdit.SB_Rel_RemoveClick(Sender: TObject);
begin
 if (Self.LV_Pom_Release.Selected <> nil) then
  Self.LV_Pom_Release.Items.Delete(Self.LV_Pom_Release.ItemIndex);
end;

procedure TF_HVEdit.SB_Take_AddClick(Sender: TObject);
var LI:TListItem;
    i:Integer;
begin
 F_HV_Pom.OpenForm(-1, 0);
 if (F_HV_Pom.saved) then
  begin
   i := 0;
   while ((i < Self.LV_Pom_Load.Items.Count) and (StrToInt(Self.LV_Pom_Load.Items.Item[i].Caption) < F_HV_Pom.SE_CV.Value)) do Inc(i);

   if ((Assigned(Self.LV_Pom_Load.Items.Item[i])) and (StrToInt(Self.LV_Pom_Load.Items.Item[i].Caption) = F_HV_Pom.SE_CV.Value)) then
    begin
     Self.LV_Pom_Load.Items.Item[i].SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
    end else begin
     LI := Self.LV_Pom_Load.Items.Insert(i);
     LI.Caption := IntToStr(F_HV_Pom.SE_CV.Value);
     LI.SubItems.Add(IntToStr(F_HV_Pom.SE_Value.Value));
    end;
  end;
end;

procedure TF_HVEdit.SB_Take_RemoveClick(Sender: TObject);
begin
 if (Self.LV_Pom_Load.Selected <> nil) then
  Self.LV_Pom_Load.Items.Delete(Self.LV_Pom_Load.ItemIndex);
end;

procedure TF_HVEdit.B_SaveClick(Sender: TObject);
var data:THVData;
    stav:THVStav;
    return:Byte;
    OblR:TOR;
    i:Integer;
    pomCV:THVPomCV;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vypiste nazev hnaciho vozidla','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_trida.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte typ hnaciho vozidla','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Orientace.ItemIndex = -1) and (CB_Orientace.Visible) then
   begin
    Application.MessageBox('Vyberte orientaci hnaciho vozidla !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_OR.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte stanici hnaciho vozidla !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if ((not Self.E_Addr.ReadOnly) and (Self.E_Addr.text = '')) then
   begin
    Application.MessageBox('Vyplòte adresu !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  ORs.GetORByIndex(Self.CB_OR.ItemIndex, OblR);
  if ((Self.OpenHV <> nil) and (Self.OpenHV.Stav.souprava > -1) and (Self.OpenHV.Stav.stanice <> OblR)) then
    if (Application.MessageBox('Mìníte stanici HV, které je na soupravì, opravdu pokraèovat?', 'Opravdu?', MB_YESNO OR MB_ICONWARNING) = mrNo) then
      Exit();


  // samotne ukladani dat
  data.Nazev      := E_Nazev.Text;
  data.Majitel    := E_Majitel.Text;
  data.Oznaceni   := E_Oznaceni.Text;
  data.Poznamka   := M_Poznamky.Text;
  data.Trida      := THVClass(CB_trida.ItemIndex);

  if (Self.OpenHV = nil) then
   begin
    data.POMtake    := TList<THVPomCV>.Create();
    data.POMrelease := TList<THVPomCV>.Create();
   end else begin
    data.POMtake    := Self.OpenHV.Data.POMtake;
    data.POMrelease := Self.OpenHV.Data.POMrelease;
   end;

  data.POMtake.Clear();
  data.POMrelease.Clear();

  // parse POM take
  for i := 0 to Self.LV_Pom_Load.Items.Count-1 do
   begin
    try
      pomCV.cv   := StrToInt(Self.LV_Pom_Load.Items.Item[i].Caption);
      pomCV.data := StrToInt(Self.LV_Pom_Load.Items.Item[i].SubItems.Strings[0]);
      data.POMtake.Add(pomCV);
    except

    end;
   end;

  // parse POM release
  for i := 0 to Self.LV_Pom_Release.Items.Count-1 do
   begin
    try
      pomCV.cv   := StrToInt(Self.LV_Pom_Release.Items.Item[i].Caption);
      pomCV.data := StrToInt(Self.LV_Pom_Release.Items.Item[i].SubItems.Strings[0]);
      data.POMrelease.Add(pomCV);
    except

    end;
   end;


  if (Self.OpenHV = nil) then
   begin
     // vytvoreni noveho HV
     ORs.GetORByIndex(Self.CB_OR.ItemIndex, OblR);
     return := HVDb.Add(data, StrToInt(Self.E_Addr.Text), THVStanoviste(CB_Orientace.ItemIndex), OblR);

     if (return = 1) then
      begin
       Application.MessageBox(PChar('Chyba '+IntToStr(return)+#13#10+'Neplatná adresa !'), 'Nelze pøidat', MB_OK OR MB_ICONWARNING);
       Exit;
      end;
     if (return = 2) then
      begin
       Application.MessageBox(PChar('Chyba '+IntToStr(return)+#13#10+'HV s touto adresou již existuje !'), 'Nelze pøidat', MB_OK OR MB_ICONWARNING);
       Exit;
      end;
   end else begin
     // update HV
     Self.OpenHV.data := data;

     ORs.GetORByIndex(Self.CB_OR.ItemIndex, OblR);

     stav := Self.OpenHV.stav;
     stav.StanovisteA := THVStanoviste(CB_Orientace.ItemIndex);
     Self.OpenHV.Stav := stav;
     Self.OpenHV.PredejStanici(OblR);

     HVTableData.UpdateLine(OpenHV);

     if (stav.souprava > -1) then
       Blky.ChangeSprToTrat(stav.souprava);
   end;

  Self.Close;
 end;//procedure

procedure TF_HVEdit.E_AdresaKeyPress(Sender: TObject; var Key: Char);
begin
 Key := Key;
 case Key of
  '0'..'9',#9,#8:begin
              end else begin
               Key := #0;
              end;
  end;//case
end;

procedure TF_HVEdit.NewHV();
begin
 Self.OpenForm(nil);
end;//procedure

procedure TF_HVEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
 begin
  Self.OpenHV := nil;
 end;//procedure

procedure TF_HVEdit.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;//procedure

procedure TF_HVEdit.B_NajetoDeleteClick(Sender: TObject);
 begin
  if (Self.OpenHV = nil) then Exit;

  OpenHV.RemoveStats();
  HVTableData.UpdateLine(Self.OpenHV);

  Application.MessageBox('Operace probìhla úspìšne', 'OK', MB_OK OR MB_ICONINFORMATION);
 end;//procedure

procedure TF_HVEdit.HlavniOpenForm;
 begin
  Self.SB_Rel_Remove.Enabled  := false;
  Self.SB_Take_Remove.Enabled := false;
 end;

procedure TF_HVEdit.LV_Pom_LoadChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.SB_Take_Remove.Enabled := (Self.LV_Pom_Load.Selected <> nil);
end;

procedure TF_HVEdit.LV_Pom_LoadDblClick(Sender: TObject);
begin
 if (Self.LV_Pom_Load.Selected <> nil) then
  begin
   F_HV_Pom.OpenForm(StrToInt(Self.LV_Pom_Load.Selected.Caption), StrToInt(Self.LV_Pom_Load.Selected.SubItems.Strings[0]));
   if (F_HV_Pom.saved) then
     Self.LV_Pom_Load.Selected.SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
  end else begin
   Self.SB_Take_AddClick(Self);
  end;
end;

procedure TF_HVEdit.LV_Pom_ReleaseChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.SB_Rel_Remove.Enabled := (Self.LV_Pom_Release.Selected <> nil);
end;

procedure TF_HVEdit.LV_Pom_ReleaseDblClick(Sender: TObject);
begin
 if (Self.LV_Pom_Release.Selected <> nil) then
  begin
   F_HV_Pom.OpenForm(StrToInt(Self.LV_Pom_Release.Selected.Caption), StrToInt(Self.LV_Pom_Release.Selected.SubItems.Strings[0]));
   if (F_HV_Pom.saved) then
     Self.LV_Pom_Release.Selected.SubItems.Strings[0] := IntToStr(F_HV_Pom.SE_Value.Value);
  end else begin
   Self.SB_Rel_AddClick(Self);
  end;
end;

procedure TF_HVEdit.NormalOpenForm;
var data:THVData;
    stav:THVStav;
    i:Integer;
    LI:TListItem;
 begin
  B_NajetoDelete.Visible := true;
  E_Addr.ReadOnly        := true;

  data := Self.OpenHV.data;
  stav := Self.OpenHV.stav;

  E_Nazev.Text           := data.Nazev;
  E_Oznaceni.Text        := data.Oznaceni;
  E_Majitel.Text         := data.Majitel;
  E_Addr.Text            := IntToStr(Self.OpenHV.adresa);
  M_Poznamky.Text        := data.Poznamka;
  CB_trida.ItemIndex     := Integer(data.Trida);
  CB_Orientace.ItemIndex := Integer(stav.StanovisteA);

  Self.LV_Pom_Load.Clear();
  for i := 0 to data.POMtake.Count-1 do
   begin
    LI := Self.LV_Pom_Load.Items.Add;
    LI.Caption := IntToStr(data.POMtake[i].cv);
    LI.SubItems.Add(IntToStr(data.POMtake[i].data));
   end;

  Self.LV_Pom_Release.Clear();
  for i := 0 to data.POMrelease.Count-1 do
   begin
    LI := Self.LV_Pom_Release.Items.Add;
    LI.Caption := IntToStr(data.POMrelease[i].cv);
    LI.SubItems.Add(IntToStr(data.POMrelease[i].data));
   end;

  ORs.FillCB(Self.CB_OR, stav.stanice);

  F_HVEdit.Caption := 'HV : '+IntToStr(Self.OpenHV.adresa);
 end;//procedure

procedure TF_HVEdit.NewHVOpenForm;
 begin
  B_NajetoDelete.Visible := false;
  E_Addr.ReadOnly        := false;

  E_Nazev.Text           := '';
  E_Oznaceni.Text        := '';
  E_Majitel.Text         := '';
  E_Addr.Text            := '';
  M_Poznamky.Text        := '';
  CB_trida.ItemIndex     := -1;
  CB_Orientace.ItemIndex := -1;

  Self.LV_Pom_Load.Clear();
  Self.LV_Pom_Release.Clear();

  ORs.FillCB(Self.CB_OR, nil);

  F_HVEdit.Caption       := 'Nové HV';
 end;//procedure

end.//unit
