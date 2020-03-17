unit Diagnostics;

interface

uses IniFiles, SysUtils;

type
  TDiagnostics = class
  public
    showZaver: boolean;
    simSoupravaObsaz: boolean;
    simInputs: boolean;

    constructor Create();
    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile; section: string);
  end;

var
  diag: TDiagnostics;

implementation

uses Simulation, TechnologieRCS, Logging;

////////////////////////////////////////////////////////////////////////////////

constructor TDiagnostics.Create();
begin
 inherited;
 Self.showZaver := false;
end;

procedure TDiagnostics.LoadData(ini: TMemIniFile; section: string);
begin
 Self.showZaver := ini.ReadBool(section, 'showZaver', false);
 Self.simSoupravaObsaz := ini.ReadBool(section, 'SoupravaUsekSim', false);
 Self.simInputs := ini.ReadBool(section, 'InputSim', false);
 JCSimulator.timer.Enabled := ini.ReadBool(section, 'JCsim', false);
 VyhSimulator.timer.Enabled := ini.ReadBool(section, 'VYHsim', false);
 TratSimulator.timer.Enabled := ini.ReadBool(section, 'TRATsim', false);
end;

procedure TDiagnostics.SaveData(ini: TMemIniFile; section: string);
begin
 if (Self.showZaver) then
   ini.WriteBool(section, 'showZaver', Self.showZaver);
 if (Self.simSoupravaObsaz) then
   ini.WriteBool('AdminData', 'SoupravaUsekSim', Self.simSoupravaObsaz);
 if (JCSimulator.timer.Enabled) then
   ini.WriteBool(section, 'JCsim', JCSimulator.timer.Enabled);
 if (TratSimulator.timer.Enabled) then
   ini.WriteBool(section, 'TRATsim', TratSimulator.timer.Enabled);
 if (VyhSimulator.timer.Enabled) then
   ini.WriteBool(section, 'VYHsim', VyhSimulator.timer.Enabled);
 if (Self.simInputs) then
   ini.WriteBool(section, 'InputSim', Self.simInputs);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  diag := TDiagnostics.Create();
finalization
  diag.Free();

end.
