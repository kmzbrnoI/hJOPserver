unit fZesilovacEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, fMain, Booster, Math;

type
  TF_Booster_Edit = class(TForm)
    Label4: TLabel;
    E_Name: TEdit;
    B_Save: TButton;
    GB_Short: TGroupBox;
    Label8: TLabel;
    Label7: TLabel;
    SE_Short_Port: TSpinEdit;
    GB_Power: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    SE_Power_Port: TSpinEdit;
    B_Storno: TButton;
    SE_Short_Module: TSpinEdit;
    SE_Power_Module: TSpinEdit;
    GB_DCC: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SE_DCC_port: TSpinEdit;
    SE_DCC_module: TSpinEdit;
    CHB_DCC: TCheckBox;
    E_ID: TEdit;
    Label3: TLabel;
    CHB_Power: TCheckBox;
    CHB_Short: TCheckBox;
    CHB_short_reversed: TCheckBox;
    CHB_power_reversed: TCheckBox;
    CHB_dcc_reversed: TCheckBox;
    procedure B_SaveClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CHB_DCCClick(Sender: TObject);
    procedure SE_RCS_moduleExit(Sender: TObject);
    procedure CHB_ShortClick(Sender: TObject);
    procedure CHB_PowerClick(Sender: TObject);
  private
    open_booster: TBooster;

    procedure CommonOpenForm();
    procedure NormalOpenForm();
    procedure NewOpenForm();
  public
    procedure NewBooster();
    procedure EditBooster(booster: TBooster);
  end;

var
  F_Booster_Edit: TF_Booster_Edit;

implementation

uses GetSystems, RCSc, BoosterDb, DataZesilovac, BlockDb, ownGuiUtils;

{$R *.dfm}

procedure TF_Booster_Edit.EditBooster(booster: TBooster);
begin
  Self.open_booster := booster;

  Self.CommonOpenForm();
  if (booster = nil) then
    Self.NewOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_Booster_Edit.SE_RCS_moduleExit(Sender: TObject);
begin
  Self.SE_Power_Port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Power_Module.Value)) - 1, 0);
  Self.SE_DCC_port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_DCC_module.Value)) - 1, 0);
  Self.SE_Short_Port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Short_Module.Value)) - 1, 0);
end;

procedure TF_Booster_Edit.B_SaveClick(Sender: TObject);
var settings: TBoosterSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplňte název zesilovače!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;
  if (Self.E_ID.Text = '') then
  begin
    StrMessageBox('Vyplňte id zesilovače!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;
  if (Boosters.ContainsKey(E_ID.Text, Self.open_booster)) then
  begin
    StrMessageBox('Zesilovač s tímto ID již existuje!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
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
        ExceptionMessageBox('Chyba při vytváření zesilovače', E);
        Exit();
      end;
    end;
  end;

  settings.name := E_Name.Text;
  settings.id := E_ID.Text;

  if (Self.CHB_Short.Checked) then
  begin
    settings.rcs.overload.addr.module := Self.SE_Short_Module.Value;
    settings.rcs.overload.addr.port := Self.SE_Short_Port.Value;
    settings.rcs.overload.reversed := Self.CHB_short_reversed.Checked;
  end else begin
    settings.rcs.overload.addr.module := 0;
    settings.rcs.overload.addr.port := 0;
    settings.rcs.overload.reversed := false;
  end;

  if (Self.CHB_Power.Checked) then
  begin
    settings.rcs.power.addr.module := Self.SE_Power_Module.Value;
    settings.rcs.power.addr.port := Self.SE_Power_Port.Value;
    settings.rcs.power.reversed := Self.CHB_power_reversed.Checked;
  end else begin
    settings.rcs.power.addr.module := 0;
    settings.rcs.power.addr.port := 0;
    settings.rcs.power.reversed := false;
  end;

  if (Self.CHB_DCC.Checked) then
  begin
    settings.rcs.DCC.addr.module := Self.SE_DCC_module.Value;
    settings.rcs.DCC.addr.port := Self.SE_DCC_port.Value;
    settings.rcs.DCC.reversed := Self.CHB_dcc_reversed.Checked;
  end else begin
    settings.rcs.DCC.addr.module := 0;
    settings.rcs.DCC.addr.port := 0;
    settings.rcs.DCC.reversed := false;
  end;

  Self.open_booster.settings := settings;

  Boosters.SyncStructures();
  ZesTableData.LoadToTable();

  Self.Close();
end;

procedure TF_Booster_Edit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Booster_Edit.CHB_DCCClick(Sender: TObject);
begin
  Self.SE_DCC_module.Enabled := Self.CHB_DCC.Checked;
  Self.SE_DCC_port.Enabled := Self.CHB_DCC.Checked;
  Self.CHB_dcc_reversed.Enabled := Self.CHB_DCC.Checked;
end;

procedure TF_Booster_Edit.CHB_PowerClick(Sender: TObject);
begin
  Self.SE_Power_Module.Enabled := Self.CHB_Power.Checked;
  Self.SE_Power_Port.Enabled := Self.CHB_Power.Checked;
  Self.CHB_power_reversed.Enabled := Self.CHB_Power.Checked;
end;

procedure TF_Booster_Edit.CHB_ShortClick(Sender: TObject);
begin
  Self.SE_Short_Module.Enabled := Self.CHB_Short.Checked;
  Self.SE_Short_Port.Enabled := Self.CHB_Short.Checked;
  Self.CHB_short_reversed.Enabled := Self.CHB_Short.Checked;
end;

procedure TF_Booster_Edit.NewBooster;
begin
  Self.EditBooster(nil);
end;

procedure TF_Booster_Edit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.open_booster := nil;
  CanClose := true;
end;

procedure TF_Booster_Edit.CommonOpenForm();
begin
  Self.ActiveControl := Self.B_Save;

  Self.SE_Power_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Short_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_DCC_module.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_Booster_Edit.NormalOpenForm();
var bSettings: TBoosterSettings;
begin
  bSettings := open_booster.settings;

  Self.E_ID.Text := bSettings.id;
  Self.E_Name.Text := bSettings.name;

  Self.SE_Short_Module.Value := bSettings.rcs.overload.addr.module;
  Self.SE_Short_Port.Value := bSettings.rcs.overload.addr.port;
  Self.CHB_short_reversed.Checked := bSettings.rcs.overload.reversed;

  Self.SE_Power_Module.Value := bSettings.rcs.power.addr.module;
  Self.SE_Power_Port.Value := bSettings.rcs.power.addr.port;
  Self.CHB_power_reversed.Checked := bSettings.rcs.power.reversed;

  Self.SE_DCC_module.Value := bSettings.rcs.DCC.addr.module;
  Self.SE_DCC_port.Value := bSettings.rcs.DCC.addr.port;
  Self.CHB_dcc_reversed.Checked := bSettings.rcs.DCC.reversed;

  Self.CHB_Short.Checked := open_booster.isOverloadDetection;
  Self.CHB_ShortClick(Self.CHB_Short);

  Self.CHB_Power.Checked := open_booster.isPowerDetection;
  Self.CHB_PowerClick(Self.CHB_Power);

  Self.CHB_DCC.Checked := open_booster.isDCCdetection;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.SE_RCS_moduleExit(Self);

  Self.Caption := 'Zesilovač: ' + bSettings.name;
end;

procedure TF_Booster_Edit.NewOpenForm();
begin
  Self.E_ID.Text := '';
  Self.E_Name.Text := '';

  Self.SE_Short_Module.Value := 1;
  Self.SE_Short_Port.Value := 0;
  Self.CHB_short_reversed.Checked := false;

  Self.SE_Power_Module.Value := 1;
  Self.SE_Power_Port.Value := 0;
  Self.CHB_power_reversed.Checked := false;

  Self.SE_DCC_module.Value := 1;
  Self.SE_DCC_port.Value := 0;
  Self.CHB_dcc_reversed.Checked := false;

  Self.CHB_Short.Checked := true;
  Self.CHB_ShortClick(Self.CHB_Short);

  Self.CHB_Power.Checked := true;
  Self.CHB_PowerClick(Self.CHB_Power);

  Self.CHB_DCC.Checked := false;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.SE_RCS_moduleExit(Self);

  Self.Caption := 'Nový zesilovač';
end;

end.
