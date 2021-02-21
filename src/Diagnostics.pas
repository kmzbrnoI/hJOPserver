unit Diagnostics;

interface

uses IniFiles, SysUtils;

type
  TDiagnostics = class
  public
    showZaver: Boolean;
    simSoupravaObsaz: Boolean;
    simInputs: Boolean;
    showBlockId: Boolean;

    constructor Create();
    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile; section: string);
  end;

var
  diag: TDiagnostics;

implementation

uses Simulation, TechnologieRCS, Logging;

/// /////////////////////////////////////////////////////////////////////////////

constructor TDiagnostics.Create();
begin
  inherited;
  Self.showZaver := false;
  Self.simSoupravaObsaz := false;
  Self.simInputs := false;
  Self.showBlockId := false;
end;

procedure TDiagnostics.LoadData(ini: TMemIniFile; section: string);
begin
  Self.showZaver := ini.ReadBool(section, 'showZaver', false);
  Self.simSoupravaObsaz := ini.ReadBool(section, 'SoupravaUsekSim', false);
  Self.simInputs := ini.ReadBool(section, 'InputSim', false);
  Self.showBlockId := ini.ReadBool(section, 'showBlockId', false);

  JCSimulator.timer.Enabled := ini.ReadBool(section, 'JCsim', false);
  TurnoutSimulator.timer.Enabled := ini.ReadBool(section, 'VYHsim', false);
  RailwaySimulator.timer.Enabled := ini.ReadBool(section, 'TRATsim', false);
end;

procedure TDiagnostics.SaveData(ini: TMemIniFile; section: string);
begin
  if (Self.showZaver) then
    ini.WriteBool(section, 'showZaver', Self.showZaver);
  if (Self.simSoupravaObsaz) then
    ini.WriteBool('AdminData', 'SoupravaUsekSim', Self.simSoupravaObsaz);
  if (JCSimulator.timer.Enabled) then
    ini.WriteBool(section, 'JCsim', JCSimulator.timer.Enabled);
  if (RailwaySimulator.timer.Enabled) then
    ini.WriteBool(section, 'TRATsim', RailwaySimulator.timer.Enabled);
  if (TurnoutSimulator.timer.Enabled) then
    ini.WriteBool(section, 'VYHsim', TurnoutSimulator.timer.Enabled);
  if (Self.simInputs) then
    ini.WriteBool(section, 'InputSim', Self.simInputs);
  if (Self.showBlockId) then
    ini.WriteBool(section, 'showBlockId', Self.showBlockId);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

diag := TDiagnostics.Create();

finalization

diag.Free();

end.
