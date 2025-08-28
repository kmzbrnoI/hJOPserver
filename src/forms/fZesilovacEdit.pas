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
    SE_Short_Port: TSpinEdit;
    GB_Power: TGroupBox;
    SE_Power_Port: TSpinEdit;
    B_Storno: TButton;
    SE_Short_Module: TSpinEdit;
    SE_Power_Module: TSpinEdit;
    GB_DCC: TGroupBox;
    SE_DCC_Port: TSpinEdit;
    SE_DCC_Module: TSpinEdit;
    CHB_DCC: TCheckBox;
    E_ID: TEdit;
    Label3: TLabel;
    CHB_Power: TCheckBox;
    CHB_Short: TCheckBox;
    CHB_Short_Reversed: TCheckBox;
    CHB_Power_Reversed: TCheckBox;
    CHB_DCC_Reversed: TCheckBox;
    SE_DCC_System: TSpinEdit;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    SE_Short_System: TSpinEdit;
    SE_Power_System: TSpinEdit;
    procedure B_SaveClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CHB_DCCClick(Sender: TObject);
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

uses GetSystems, RCSsc, BoosterDb, DataZesilovac, BlockDb, ownGuiUtils, Config;

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

  settings.rcs.overload.addr := RCSsOptionalFromUI(Self.CHB_Short, Self.SE_Short_System, Self.SE_Short_Module, Self.SE_Short_Port);
  settings.rcs.overload.reversed := (Self.CHB_Short.Checked) and (Self.CHB_Short_Reversed.Checked);

  settings.rcs.power.addr := RCSsOptionalFromUI(Self.CHB_Power, Self.SE_Power_System, Self.SE_Power_Module, Self.SE_Power_Port);
  settings.rcs.power.reversed := (Self.CHB_Power.Checked) and (Self.CHB_Power_Reversed.Checked);

  settings.rcs.DCC.addr := RCSsOptionalFromUI(Self.CHB_DCC, Self.SE_DCC_System, Self.SE_DCC_Module, Self.SE_DCC_Port);
  settings.rcs.DCC.reversed := (Self.CHB_DCC.Checked) and (Self.CHB_DCC_Reversed.Checked);

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
  Self.SE_DCC_System.Enabled := Self.CHB_DCC.Checked;
  Self.SE_DCC_Module.Enabled := Self.CHB_DCC.Checked;
  Self.SE_DCC_Port.Enabled := Self.CHB_DCC.Checked;
  Self.CHB_DCC_Reversed.Enabled := Self.CHB_DCC.Checked;
end;

procedure TF_Booster_Edit.CHB_PowerClick(Sender: TObject);
begin
  Self.SE_Power_System.Enabled := Self.CHB_Power.Checked;
  Self.SE_Power_Module.Enabled := Self.CHB_Power.Checked;
  Self.SE_Power_Port.Enabled := Self.CHB_Power.Checked;
  Self.CHB_Power_Reversed.Enabled := Self.CHB_Power.Checked;
end;

procedure TF_Booster_Edit.CHB_ShortClick(Sender: TObject);
begin
  Self.SE_Short_System.Enabled := Self.CHB_Short.Checked;
  Self.SE_Short_Module.Enabled := Self.CHB_Short.Checked;
  Self.SE_Short_Port.Enabled := Self.CHB_Short.Checked;
  Self.CHB_Short_Reversed.Enabled := Self.CHB_Short.Checked;
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

  Self.SE_Power_System.MaxValue := RCSs._RCSS_MAX;
  Self.SE_Short_System.MaxValue := RCSs._RCSS_MAX;
  Self.SE_DCC_System.MaxValue := RCSs._RCSS_MAX;
end;

procedure TF_Booster_Edit.NormalOpenForm();
var bSettings: TBoosterSettings;
begin
  bSettings := open_booster.settings;

  Self.E_ID.Text := bSettings.id;
  Self.E_Name.Text := bSettings.name;

  RCSsOptionalToUI(bSettings.rcs.overload.addr, Self.CHB_Short, Self.SE_Short_System, Self.SE_Short_Module, Self.SE_Short_Port);
  Self.CHB_Short_Reversed.Checked := bSettings.rcs.overload.reversed;

  RCSsOptionalToUI(bSettings.rcs.power.addr, Self.CHB_Power, Self.SE_Power_System, Self.SE_Power_Module, Self.SE_Power_Port);
  Self.CHB_Power_Reversed.Checked := bSettings.rcs.power.reversed;

  RCSsOptionalToUI(bSettings.rcs.DCC.addr, Self.CHB_DCC, Self.SE_DCC_System, Self.SE_DCC_Module, Self.SE_DCC_Port);
  Self.CHB_DCC_Reversed.Checked := bSettings.rcs.DCC.reversed;

  Self.Caption := 'Zesilovač: ' + bSettings.name;
end;

procedure TF_Booster_Edit.NewOpenForm();
begin
  Self.E_ID.Text := '';
  Self.E_Name.Text := '';

  Self.SE_Short_System.Value := 0;
  Self.SE_Short_Module.Value := 1;
  Self.SE_Short_Port.Value := 0;
  Self.CHB_Short_Reversed.Checked := False;

  Self.SE_Power_System.Value := 0;
  Self.SE_Power_Module.Value := 1;
  Self.SE_Power_Port.Value := 0;
  Self.CHB_Power_Reversed.Checked := False;

  Self.SE_DCC_System.Value := 0;
  Self.SE_DCC_Module.Value := 1;
  Self.SE_DCC_Port.Value := 0;
  Self.CHB_DCC_Reversed.Checked := False;

  Self.CHB_Short.Checked := true;
  Self.CHB_ShortClick(Self.CHB_Short);

  Self.CHB_Power.Checked := true;
  Self.CHB_PowerClick(Self.CHB_Power);

  Self.CHB_DCC.Checked := false;
  Self.CHB_DCCClick(Self.CHB_DCC);

  Self.Caption := 'Nový zesilovač';
end;

end.
