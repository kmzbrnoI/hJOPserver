unit fZesilovacEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, fMain, Booster;

type
  TF_ZesilovacEdit = class(TForm)
    L_Nazev: TLabel;
    E_Nazev: TEdit;
    RG_Typ: TRadioGroup;
    B_Save: TButton;
    GB_Zkrat: TGroupBox;
    L_Zkrat_Port: TLabel;
    L_Zkrat_MTB: TLabel;
    SE_Zkrat_Port: TSpinEdit;
    GB_Napajeni: TGroupBox;
    L_Napajeni_Port: TLabel;
    L_Napajeni_MTB: TLabel;
    SE_Napajeni_Port: TSpinEdit;
    B_Storno: TButton;
    SE_Zkrat_MTB: TSpinEdit;
    SE_Napajeni_MTB: TSpinEdit;
    GB_DCC: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SE_DCC_port: TSpinEdit;
    SE_DCC_MTB: TSpinEdit;
    CHB_DCC: TCheckBox;
    E_ID: TEdit;
    Label3: TLabel;
    procedure B_SaveClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CHB_DCCClick(Sender: TObject);
  private
    open_booster:TBooster;

     procedure HlavniOpenForm;
     procedure NormalOpenForm;
     procedure NewOpenForm;
  public
    procedure NewZes;
    procedure OpenForm(Zesilovac:TBooster);
  end;

var
  F_ZesilovacEdit: TF_ZesilovacEdit;

implementation

uses GetSystems, fSettings, TechnologieRCS, BoosterDb, FileSystem, DataZesilovac,
    TBloky;

{$R *.dfm}

procedure TF_ZesilovacEdit.OpenForm(Zesilovac:TBooster);
 begin
  Self.open_booster := Zesilovac;
  Self.HlavniOpenForm;
  if (zesilovac = nil) then
   begin
    Self.NewOpenForm;
   end else begin
    Self.NormalOpenForm;
   end;

  Self.ShowModal;
 end;

procedure TF_ZesilovacEdit.B_SaveClick(Sender: TObject);
var settings:TBoosterSettings;
begin
 if (E_Nazev.Text = '') then
  begin
   Application.MessageBox('Vyplnte nazev zesilovace','Nelze ulozit data',MB_OK OR MB_ICONSTOP);
   Exit;
  end;
 if (E_ID.Text = '') then
  begin
   Application.MessageBox('Vyplnte id zesilovace','Nelze ulozit data',MB_OK OR MB_ICONSTOP);
   Exit;
  end;
 if (Self.RG_Typ.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte typ zesilovace','Nelze ulozit data',MB_OK OR MB_ICONSTOP);
   Exit;
  end;
 if (Boosters.ContainsKey(E_ID.Text, Self.open_booster)) then
  begin
   Application.MessageBox('Zesilovaè s tímto ID již existuje!','Nelze ulozit data',MB_OK OR MB_ICONSTOP);
   Exit;
  end;

 if (Self.open_booster = nil) then
  begin
   Self.open_booster := TBooster.Create;
   try
     Boosters.Add(Self.open_booster);
   except
     on E:Exception do
      begin
       Application.MessageBox(PChar(e.Message), 'Chyba pøi vytváøení zesilovaèe', MB_OK OR MB_ICONERROR);
       Exit();
      end;
   end;
  end;

 settings.Name   := E_Nazev.Text;
 settings.bclass := TBoosterClass(RG_Typ.ItemIndex+1);
 settings.id     := E_ID.Text;

 settings.MTB.Zkrat.board     := Self.SE_Zkrat_MTB.Value;
 settings.MTB.Napajeni.board  := Self.SE_Napajeni_MTB.Value;
 settings.MTB.Zkrat.port      := SE_Zkrat_Port.Value;
 settings.MTB.Napajeni.port   := SE_Napajeni_Port.Value;

 if (Self.CHB_DCC.Checked) then
  begin
   Settings.MTB.DCC.board := Self.SE_DCC_MTB.Value;
   Settings.MTB.DCC.port  := Self.SE_DCC_port.Value;
  end else begin
   Settings.MTB.DCC.board := 0;
   Settings.MTB.DCC.port  := 0;
  end;

 Self.open_booster.bSettings := settings;

 Boosters.SyncStructures();
 ZesTableData.LoadToTable();

 Self.Close;
end;//procedure

procedure TF_ZesilovacEdit.B_StornoClick(Sender: TObject);
 begin
  F_ZesilovacEdit.Close;
 end;

procedure TF_ZesilovacEdit.CHB_DCCClick(Sender: TObject);
begin
 Self.SE_DCC_MTB.Enabled  := Self.CHB_DCC.Checked;
 Self.SE_DCC_port.Enabled := Self.CHB_DCC.Checked;
end;

procedure TF_ZesilovacEdit.NewZes;
 begin
  OpenForm(nil);
 end;//procedure

procedure TF_ZesilovacEdit.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
 begin
  Self.open_booster := nil;
  CanClose := true;
 end;//procedure

procedure TF_ZesilovacEdit.HlavniOpenForm;
 begin
  F_ZesilovacEdit.ActiveControl := B_Save;
 end;//procedure

procedure TF_ZesilovacEdit.NormalOpenForm;
var bSettings:TBoosterSettings;
    IgnoraceMTB:TArI;
 begin
  bSettings := open_booster.bSettings;

  E_ID.Text        := bSettings.id;
  E_Nazev.Text     := bSettings.Name;
  RG_Typ.ItemIndex := Integer(bSettings.bclass)-1;

  SE_Zkrat_Port.Value       := bSettings.MTB.Zkrat.port;
  SE_Napajeni_Port.Value    := bSettings.MTB.Napajeni.port;

  SetLength(IgnoraceMTB,2);
  IgnoraceMTB[0] := 3;
  IgnoraceMTB[1] := 4;

  Self.SE_Napajeni_MTB.Value := bSettings.MTB.Napajeni.board;
  Self.SE_Zkrat_MTB.Value    := bSettings.MTB.Zkrat.board;

  Self.SE_DCC_MTB.Value      := bSettings.MTB.DCC.board;
  Self.SE_DCC_port.Value     := bSettings.MTB.DCC.port;

  Self.CHB_DCC.Checked := open_booster.isDCCdetection;
  Self.CHB_DCCClick(Self.CHB_DCC);

  F_ZesilovacEdit.Caption := 'Zesilovaè : '+bSettings.Name;
 end;//procedure

procedure TF_ZesilovacEdit.NewOpenForm;
var IgnoraceMTB:TArI;
 begin
  E_ID.Text         := '';
  E_Nazev.Text      := '';
  RG_Typ.ItemIndex  := -1;

  SE_Zkrat_Port.Value    := 0;
  SE_Napajeni_Port.Value := 0;

  SetLength(IgnoraceMTB,2);
  IgnoraceMTB[0] := 3;
  IgnoraceMTB[1] := 4;

  Self.SE_Napajeni_MTB.Value := 1;
  Self.SE_Zkrat_MTB.Value    := 1;

  Self.SE_DCC_MTB.Value      := 1;
  Self.SE_DCC_port.Value     := 0;

  Self.CHB_DCC.Checked := false;
  Self.CHB_DCCClick(Self.CHB_DCC);

  F_ZesilovacEdit.Caption := 'Nový zesilovaè';
 end;//procedure

end.//unit
