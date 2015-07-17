unit fZesilovacEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, fMain, Booster, RPConst;

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
    procedure B_SaveClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

uses GetSystems, fSettings, TechnologieMTB, BoosterDb, FileSystem, DataZesilovac;

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
 if (Self.RG_Typ.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte typ zesilovace','Nelze ulozit data',MB_OK OR MB_ICONSTOP);
   Exit;
  end;


 if (Self.open_booster = nil) then
  begin
   Self.open_booster := TBooster.Create;
   BoostersDb.AddBooster(Self.open_booster);
  end;

 settings.Name   := E_Nazev.Text;
 settings.bclass := TBoosterClass(RG_Typ.ItemIndex+1);

 settings.MTB.Zkrat.board     := Self.SE_Zkrat_MTB.Value;
 settings.MTB.Napajeni.board  := Self.SE_Napajeni_MTB.Value;
 settings.MTB.Zkrat.port      := SE_Zkrat_Port.Value;
 settings.MTB.Napajeni.port   := SE_Napajeni_Port.Value;

 Self.open_booster.bSettings := settings;

 ZesTableData.LoadToTable();

 Self.Close;
end;//procedure

procedure TF_ZesilovacEdit.B_StornoClick(Sender: TObject);
 begin
  F_ZesilovacEdit.Close;
 end;//procedure

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
    IgnoraceMTB:TArSmallI;
 begin
  bSettings := open_booster.bSettings;

  E_Nazev.Text     := bSettings.Name;
  RG_Typ.ItemIndex := Integer(bSettings.bclass)-1;

  SE_Zkrat_Port.Value       := bSettings.MTB.Zkrat.port;
  SE_Napajeni_Port.Value    := bSettings.MTB.Napajeni.port;

  SetLength(IgnoraceMTB,2);
  IgnoraceMTB[0] := 3;
  IgnoraceMTB[1] := 4;

  Self.SE_Napajeni_MTB.Value := bSettings.MTB.Napajeni.board;
  Self.SE_Zkrat_MTB.Value    := bSettings.MTB.Zkrat.board;

  F_ZesilovacEdit.Caption := 'Zesilovaè : '+bSettings.Name;
 end;//procedure

procedure TF_ZesilovacEdit.NewOpenForm;
var IgnoraceMTB:TArSmallI;
 begin
  E_Nazev.Text      := '';
  RG_Typ.ItemIndex  := -1;

  SE_Zkrat_Port.Value    := 0;
  SE_Napajeni_Port.Value := 0;

  SetLength(IgnoraceMTB,2);
  IgnoraceMTB[0] := 3;
  IgnoraceMTB[1] := 4;

  Self.SE_Napajeni_MTB.Value := 1;
  Self.SE_Zkrat_MTB.Value    := 1;

  F_ZesilovacEdit.Caption := 'Nový zesilovaè';
 end;//procedure

end.//unit
