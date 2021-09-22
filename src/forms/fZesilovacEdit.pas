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

uses GetSystems, TechnologieRCS, BoosterDb, FileSystem, DataZesilovac,
  BlockDb;

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
    Application.MessageBox('Vyplňte název zesilovače!', 'Nelze uložit data', MB_OK OR MB_ICONSTOP);
    Exit();
  end;
  if (Self.E_ID.Text = '') then
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

  settings.name := E_Name.Text;
  settings.id := E_ID.Text;

  if (Self.CHB_Short.Checked) then
  begin
    settings.RCS.overload.board := Self.SE_Short_Module.Value;
    settings.RCS.overload.port := SE_Short_Port.Value;
  end else begin
    settings.RCS.overload.board := 0;
    settings.RCS.overload.port := 0;
  end;

  if (Self.CHB_Power.Checked) then
  begin
    settings.RCS.power.board := Self.SE_Power_Module.Value;
    settings.RCS.power.port := SE_Power_Port.Value;
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

procedure TF_Booster_Edit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Booster_Edit.CHB_DCCClick(Sender: TObject);
begin
  Self.SE_DCC_module.Enabled := Self.CHB_DCC.Checked;
  Self.SE_DCC_port.Enabled := Self.CHB_DCC.Checked;
end;

procedure TF_Booster_Edit.CHB_PowerClick(Sender: TObject);
begin
  Self.SE_Power_Module.Enabled := Self.CHB_Power.Checked;
  Self.SE_Power_Port.Enabled := Self.CHB_Power.Checked;
end;

procedure TF_Booster_Edit.CHB_ShortClick(Sender: TObject);
begin
  Self.SE_Short_Module.Enabled := Self.CHB_Short.Checked;
  Self.SE_Short_Port.Enabled := Self.CHB_Short.Checked;
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

procedure TF_Booster_Edit.NormalOpenForm;
var bSettings: TBoosterSettings;
begin
  bSettings := open_booster.settings;

  Self.E_ID.Text := bSettings.id;
  Self.E_Name.Text := bSettings.name;

  Self.SE_Short_Port.Value := bSettings.RCS.overload.port;
  Self.SE_Power_Port.Value := bSettings.RCS.power.port;

  Self.SE_Power_Module.Value := bSettings.RCS.power.board;
  Self.SE_Short_Module.Value := bSettings.RCS.overload.board;

  Self.SE_DCC_module.Value := bSettings.RCS.DCC.board;
  Self.SE_DCC_port.Value := bSettings.RCS.DCC.port;

  Self.CHB_Short.Checked := open_booster.isOverloadDetection;
  Self.CHB_ShortClick(Self.CHB_Short);

  Self.CHB_Power.Checked := open_booster.isPowerDetection;
  Self.CHB_PowerClick(Self.CHB_Power);

  Self.CHB_DCC.Checked := open_booster.isDCCdetection;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.SE_RCS_moduleExit(Self);

  Self.Caption := 'Zesilovač: ' + bSettings.name;
end;

procedure TF_Booster_Edit.NewOpenForm;
begin
  Self.E_ID.Text := '';
  Self.E_Name.Text := '';

  Self.SE_Short_Port.Value := 0;
  Self.SE_Power_Port.Value := 0;

  Self.SE_Power_Module.Value := 1;
  Self.SE_Short_Module.Value := 1;

  Self.SE_DCC_module.Value := 1;
  Self.SE_DCC_port.Value := 0;

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
