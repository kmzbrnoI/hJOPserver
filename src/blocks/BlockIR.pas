unit BlockIR;

{ IR technological block definition. }

interface

uses IniFiles, Block, JsonDataObjects, RCSc;

type
  TIROccupationState = (disabled = -5, none = -1, free = 0, occupied = 1);

  TBlkIRSettings = record
    RCSAddr: TRCSAddr;
  end;

  TBlkIRState = record
    occupied, occupiedOld: TIROccupationState;
  end;

  TBlkIR = class(TBlk)
  const
    _def_ir_state: TBlkIRState = (occupied: disabled; occupiedOld: disabled;);

  private
    m_settings: TBlkIRSettings;
    m_state: TBlkIRState;

  public
    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;

    // ----- IR own functions -----

    function GetSettings(): TBlkIRSettings;
    procedure SetSettings(data: TBlkIRSettings);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

    property occupied: TIROccupationState read m_state.occupied;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses RCSIFace, RCSErrors, PTUtils, Config;

constructor TBlkIR.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btIR;
  Self.m_state := Self._def_ir_state;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.RCSAddr := RCSFromIni(ini_tech, section, 'RCS0', 'RCSb0', 'RCSp0');
  RCSi.SetNeeded(Self.m_settings.RCSAddr.board);
end;

procedure TBlkIR.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  ini_tech.WriteString(section, 'RCS0', Self.m_settings.RCSAddr.ToString());
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Enable();
var enable: Boolean;
begin
  try
    enable := RCSi.IsNonFailedModule(Self.m_settings.RCSAddr.board);
  except
    enable := false;
  end;

  if (enable) then
    Self.m_state.occupied := none;
  Self.Update(); // will call change
end;

procedure TBlkIR.Disable();
begin
  Self.m_state.occupied := disabled;
  Self.m_state.occupied := disabled;
  Self.Change(true);
end;

function TBlkIR.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  Result := ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddr = addr));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Update();
var state: TRCSInputState;
begin
  try
    state := RCSi.GetInput(Self.m_settings.RCSAddr)
  except
    state := failure;
  end;

  case (state) of
    isOff:
      Self.m_state.occupied := TIROccupationState.free;
    isOn:
      Self.m_state.occupied := TIROccupationState.occupied;
  else
    Self.m_state.occupied := TIROccupationState.disabled;
  end;

  if (Self.m_state.occupied <> Self.m_state.occupiedOld) then
  begin
    Self.Change();
    Self.m_state.occupiedOld := Self.m_state.occupied;
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkIR.GetSettings(): TBlkIRSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkIR.SetSettings(data: TBlkIRSettings);
begin
  Self.m_settings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  TBlk.RCStoJSON(Self.m_settings.RCSAddr, json['rcs']);

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkIR.GetPtState(json: TJsonObject);
begin
  case (Self.occupied) of
    TIROccupationState.disabled:
      json['state'] := 'off';
    TIROccupationState.none:
      json['state'] := 'none';
    TIROccupationState.free:
      json['state'] := 'free';
    TIROccupationState.occupied:
      json['state'] := 'occupied';
  end;
end;

procedure TBlkIR.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('state')) then
  begin
    try
      if (reqJson.S['state'] = 'free') then
        RCSi.SetInput(Self.m_settings.RCSAddr, 0)
      else if (reqJson.S['state'] = 'occupied') then
        RCSi.SetInput(Self.m_settings.RCSAddr, 1)
    except
      on e: RCSException do
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '500', 'Simulace nepovolila nastaveni RCS vstupu', e.Message);
    end;

    Self.Update(); // to propagate new state into response
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
