unit BlockIR;

{ IR technological block definition. }

interface

uses IniFiles, Block, JsonDataObjects, TechnologieRCS;

type
  TIROccupationState = (disabled = -5, none = -1, free = 0, occupied = 1);

  TBlkIRSettings = record
    RCSAddrs: TRCSAddrs; // only 1 address
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

    property occupied: TIROccupationState read m_state.occupied;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses RCS;

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

  Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);

  for var rcsAddr: TRCSAddr in Self.m_settings.RCSAddrs do
    RCSi.SetNeeded(rcsAddr.board);
end;

procedure TBlkIR.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Enable();
var enable: Boolean;
begin
  try
    enable := (Self.m_settings.RCSAddrs.Count > 0) and (RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[0].board));
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
  Result := ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddrs.Contains(addr)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Update();
var state: TRCSInputState;
begin
  try
    state := RCSi.GetInput(Self.m_settings.RCSAddrs[0])
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
  if (Self.m_settings.RCSAddrs <> data.RCSAddrs) then
    Self.m_settings.RCSAddrs.free();

  Self.m_settings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  TBlk.RCStoJSON(Self.m_settings.RCSAddrs[0], json['rcs']);

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

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
