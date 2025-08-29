unit AreaLighting;

interface

uses RCSc, RCSsc, RCSIFace, Classes, ownStrUtils, SysUtils;

type
  TAreaLighting = class
    name: string;
    rcsAddr: TRCSsAddr;
    default_state: Boolean;

    constructor Create(spnlitem: string);
    procedure Parse(spnlitem: string);
    function GetActive(): Boolean;

    property active: Boolean read GetActive;
  end;

implementation

constructor TAreaLighting.Create(spnlitem: string);
begin
  inherited Create();

  Self.default_state := false;
  Self.Parse(spnlitem);
end;

procedure TAreaLighting.Parse(spnlitem: string);
begin
  Self.rcsAddr.Load(spnlitem);
end;

function TAreaLighting.GetActive(): Boolean;
begin
  try
    Result := (RCSs.GetOutputState(Self.rcsAddr) = TRCSOutputState.osEnabled);
  except
    Result := false;
  end;
end;

end.
