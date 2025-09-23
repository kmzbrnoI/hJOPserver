unit fAdminForm;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, ExtCtrls, TechnologieJC, BlockRailway;

type
  TF_Admin = class(TForm)
    B_InputSim: TButton;
    B_Save: TButton;
    CHB_SimInput: TCheckBox;
    CHB_SimSoupravaUsek: TCheckBox;
    CHB_JC_Simulator: TCheckBox;
    CHB_Trat_Sim: TCheckBox;
    CHB_SimVyhybky: TCheckBox;
    CHB_Zaver: TCheckBox;
    CHB_Show_Block_Id: TCheckBox;
    B_JC_Single_Iter: TButton;
    B_Trat_Single_Iter: TButton;
    procedure B_SaveClick(Sender: TObject);
    procedure B_InputSimClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_JC_SimulatorClick(Sender: TObject);
    procedure CHB_Trat_SimClick(Sender: TObject);
    procedure CHB_SimVyhybkyClick(Sender: TObject);
    procedure CHB_SimSoupravaUsekClick(Sender: TObject);
    procedure CHB_SimInputClick(Sender: TObject);
    procedure CHB_ZaverClick(Sender: TObject);
    procedure CHB_Show_Block_IdClick(Sender: TObject);
    procedure B_JC_Single_IterClick(Sender: TObject);
    procedure B_Trat_Single_IterClick(Sender: TObject);

  public
    procedure LoadData(ini: TMemIniFile);

  end;

var
  F_Admin: TF_Admin;

implementation

uses Diagnostics, Simulation, fMain, RCSsc, Logging, ownGuiUtils;

{$R *.dfm}

procedure TF_Admin.LoadData(ini: TMemIniFile);
begin
  Self.CHB_SimSoupravaUsek.Checked := diag.simSoupravaObsaz;
  Self.CHB_JC_Simulator.Checked := JCSimulator.timer.Enabled;
  Self.CHB_Trat_Sim.Checked := RailwaySimulator.timer.Enabled;
  Self.CHB_SimVyhybky.Checked := TurnoutSimulator.timer.Enabled;
  Self.CHB_SimInput.Checked := diag.simInputs;
  Self.CHB_Zaver.Checked := diag.showZaver;
  Self.CHB_Show_Block_Id.Checked := diag.showBlockId;
end;

procedure TF_Admin.B_JC_Single_IterClick(Sender: TObject);
begin
  JCSimulator.OnTimer(Self);
end;

procedure TF_Admin.B_SaveClick(Sender: TObject);
var ini: TMemIniFile;
const SECTION = 'AdminData';
begin
  ini := TMemIniFile.Create(F_Main.E_configFilename.Text, TEncoding.UTF8);
  try
    ini.EraseSection(SECTION);
    diag.SaveData(ini, SECTION);
    ini.UpdateFile();
  finally
    ini.Free();
  end;
end;

procedure TF_Admin.B_Trat_Single_IterClick(Sender: TObject);
begin
  RailwaySimulator.OnTimer(Self);
end;

procedure TF_Admin.CHB_JC_SimulatorClick(Sender: TObject);
begin
  JCSimulator.timer.Enabled := Self.CHB_JC_Simulator.Checked;
end;

procedure TF_Admin.CHB_Show_Block_IdClick(Sender: TObject);
begin
  diag.showBlockId := Self.CHB_Show_Block_Id.Checked;
end;

procedure TF_Admin.CHB_SimInputClick(Sender: TObject);
begin
  diag.simInputs := Self.CHB_SimInput.Checked;
end;

procedure TF_Admin.CHB_SimSoupravaUsekClick(Sender: TObject);
begin
  diag.simSoupravaObsaz := Self.CHB_SimSoupravaUsek.Checked;
end;

procedure TF_Admin.CHB_SimVyhybkyClick(Sender: TObject);
begin
  TurnoutSimulator.timer.Enabled := Self.CHB_SimVyhybky.Checked;
end;

procedure TF_Admin.CHB_Trat_SimClick(Sender: TObject);
begin
  RailwaySimulator.timer.Enabled := Self.CHB_Trat_Sim.Checked;
end;

procedure TF_Admin.CHB_ZaverClick(Sender: TObject);
begin
  diag.showZaver := Self.CHB_Zaver.Checked;
end;

procedure TF_Admin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Self.Visible) then
  begin
    Self.FormStyle := fsNormal;
    Self.Visible := false;
  end;
end;

procedure TF_Admin.B_InputSimClick(Sender: TObject);
begin
  try
    Simulation.InputSim();
    Log('Proveden InputSim', llInfo, lsRCS);
  except
    on E: Exception do
      ExceptionMessageBox(E);
  end;
end;

end.// unit
