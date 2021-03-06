﻿unit fZesilovacEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, fMain, Booster, Math;

type
  TF_ZesilovacEdit = class(TForm)
    L_Nazev: TLabel;
    E_Nazev: TEdit;
    B_Save: TButton;
    GB_Zkrat: TGroupBox;
    L_Zkrat_Port: TLabel;
    L_Zkrat_module: TLabel;
    SE_Zkrat_Port: TSpinEdit;
    GB_Napajeni: TGroupBox;
    L_Napajeni_Port: TLabel;
    L_Napajeni_module: TLabel;
    SE_Napajeni_port: TSpinEdit;
    B_Storno: TButton;
    SE_Zkrat_module: TSpinEdit;
    SE_Napajeni_module: TSpinEdit;
    GB_DCC: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SE_DCC_port: TSpinEdit;
    SE_DCC_module: TSpinEdit;
    CHB_DCC: TCheckBox;
    E_ID: TEdit;
    Label3: TLabel;
    CHB_Napajeni: TCheckBox;
    CHB_Zkrat: TCheckBox;
    procedure B_SaveClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CHB_DCCClick(Sender: TObject);
    procedure SE_RCS_moduleExit(Sender: TObject);
    procedure CHB_ZkratClick(Sender: TObject);
    procedure CHB_NapajeniClick(Sender: TObject);
  private
    open_booster: TBooster;

    procedure HlavniOpenForm;
    procedure NormalOpenForm;
    procedure NewOpenForm;
  public
    procedure NewZes;
    procedure OpenForm(Zesilovac: TBooster);
  end;

var
  F_ZesilovacEdit: TF_ZesilovacEdit;

implementation

uses GetSystems, TechnologieRCS, BoosterDb, FileSystem, DataZesilovac,
  BlockDb;

{$R *.dfm}

procedure TF_ZesilovacEdit.OpenForm(Zesilovac: TBooster);
begin
  Self.open_booster := Zesilovac;
  Self.HlavniOpenForm;
  if (Zesilovac = nil) then
  begin
    Self.NewOpenForm();
  end else begin
    Self.NormalOpenForm();
  end;

  Self.ShowModal();
end;

procedure TF_ZesilovacEdit.SE_RCS_moduleExit(Sender: TObject);
begin
  Self.SE_Napajeni_port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Napajeni_module.Value)) - 1, 0);
  Self.SE_DCC_port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_DCC_module.Value)) - 1, 0);
  Self.SE_Zkrat_Port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Zkrat_module.Value)) - 1, 0);
end;

procedure TF_ZesilovacEdit.B_SaveClick(Sender: TObject);
var settings: TBoosterSettings;
begin
  if (E_Nazev.Text = '') then
  begin
    Application.MessageBox('Vyplňte název zesilovače!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;
  if (E_ID.Text = '') then
  begin
    Application.MessageBox('Vyplňte id zesilovače!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;
  if (Boosters.ContainsKey(E_ID.Text, Self.open_booster)) then
  begin
    Application.MessageBox('Zesilovač s tímto ID již existuje!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;

  if (Self.open_booster = nil) then
  begin
    Self.open_booster := TBooster.Create;
    try
      Boosters.Add(Self.open_booster);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar(E.Message), 'Chyba při vytváření zesilovače', MB_OK OR MB_ICONERROR);
        Exit();
      end;
    end;
  end;

  settings.name := E_Nazev.Text;
  settings.id := E_ID.Text;

  if (Self.CHB_Zkrat.Checked) then
  begin
    settings.RCS.overload.board := Self.SE_Zkrat_module.Value;
    settings.RCS.overload.port := SE_Zkrat_Port.Value;
  end else begin
    settings.RCS.overload.board := 0;
    settings.RCS.overload.port := 0;
  end;

  if (Self.CHB_Napajeni.Checked) then
  begin
    settings.RCS.power.board := Self.SE_Napajeni_module.Value;
    settings.RCS.power.port := SE_Napajeni_port.Value;
  end else begin
    settings.RCS.power.board := 0;
    settings.RCS.power.port := 0;
  end;

  if (Self.CHB_DCC.Checked) then
  begin
    settings.RCS.DCC.board := Self.SE_DCC_module.Value;
    settings.RCS.DCC.port := Self.SE_DCC_port.Value;
  end else begin
    settings.RCS.DCC.board := 0;
    settings.RCS.DCC.port := 0;
  end;

  Self.open_booster.settings := settings;

  Boosters.SyncStructures();
  ZesTableData.LoadToTable();

  Self.Close();
end;

procedure TF_ZesilovacEdit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_ZesilovacEdit.CHB_DCCClick(Sender: TObject);
begin
  Self.SE_DCC_module.Enabled := Self.CHB_DCC.Checked;
  Self.SE_DCC_port.Enabled := Self.CHB_DCC.Checked;
end;

procedure TF_ZesilovacEdit.CHB_NapajeniClick(Sender: TObject);
begin
  Self.SE_Napajeni_module.Enabled := Self.CHB_Napajeni.Checked;
  Self.SE_Napajeni_port.Enabled := Self.CHB_Napajeni.Checked;
end;

procedure TF_ZesilovacEdit.CHB_ZkratClick(Sender: TObject);
begin
  Self.SE_Zkrat_module.Enabled := Self.CHB_Zkrat.Checked;
  Self.SE_Zkrat_Port.Enabled := Self.CHB_Zkrat.Checked;
end;

procedure TF_ZesilovacEdit.NewZes;
begin
  OpenForm(nil);
end;

procedure TF_ZesilovacEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.open_booster := nil;
  CanClose := true;
end;

procedure TF_ZesilovacEdit.HlavniOpenForm;
begin
  F_ZesilovacEdit.ActiveControl := B_Save;

  Self.SE_Napajeni_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Zkrat_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_DCC_module.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_ZesilovacEdit.NormalOpenForm;
var bSettings: TBoosterSettings;
  IgnoraceRCS: TArI;
begin
  bSettings := open_booster.settings;

  E_ID.Text := bSettings.id;
  E_Nazev.Text := bSettings.name;

  SE_Zkrat_Port.Value := bSettings.RCS.overload.port;
  SE_Napajeni_port.Value := bSettings.RCS.power.port;

  SetLength(IgnoraceRCS, 2);
  IgnoraceRCS[0] := 3;
  IgnoraceRCS[1] := 4;

  Self.SE_Napajeni_module.Value := bSettings.RCS.power.board;
  Self.SE_Zkrat_module.Value := bSettings.RCS.overload.board;

  Self.SE_DCC_module.Value := bSettings.RCS.DCC.board;
  Self.SE_DCC_port.Value := bSettings.RCS.DCC.port;

  Self.CHB_Zkrat.Checked := open_booster.isOverloadDetection;
  Self.CHB_ZkratClick(Self.CHB_Zkrat);

  Self.CHB_Napajeni.Checked := open_booster.isPowerDetection;
  Self.CHB_NapajeniClick(Self.CHB_Napajeni);

  Self.CHB_DCC.Checked := open_booster.isDCCdetection;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.SE_RCS_moduleExit(Self);

  F_ZesilovacEdit.Caption := 'Zesilovač: ' + bSettings.name;
end;

procedure TF_ZesilovacEdit.NewOpenForm;
var IgnoraceRCS: TArI;
begin
  E_ID.Text := '';
  E_Nazev.Text := '';

  SE_Zkrat_Port.Value := 0;
  SE_Napajeni_port.Value := 0;

  SetLength(IgnoraceRCS, 2);
  IgnoraceRCS[0] := 3;
  IgnoraceRCS[1] := 4;

  Self.SE_Napajeni_module.Value := 1;
  Self.SE_Zkrat_module.Value := 1;

  Self.SE_DCC_module.Value := 1;
  Self.SE_DCC_port.Value := 0;

  Self.CHB_Zkrat.Checked := true;
  Self.CHB_ZkratClick(Self.CHB_Zkrat);

  Self.CHB_Napajeni.Checked := true;
  Self.CHB_NapajeniClick(Self.CHB_Napajeni);

  Self.CHB_DCC.Checked := false;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.SE_RCS_moduleExit(Self);

  F_ZesilovacEdit.Caption := 'Nový zesilovač';
end;

end.// unit
