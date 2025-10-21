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
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStringsEx(['|', ','], [], spnlitem, strs);
    if (strs.Count = 3) then
    begin
      Self.name := strs[2];
      Self.rcsAddr := TRCSs.RCSsAddr(0, StrToInt(strs[0]), StrToInt(strs[1]));
    end else if (strs.Count = 4) then
    begin
      Self.name := strs[3];
      Self.rcsAddr := TRCSs.RCSsAddr(StrToInt(strs[0]), StrToInt(strs[1]), StrToInt(strs[2]));
    end else
      raise Exception.Create('Light number of items is not 3 or 4');
  finally
    strs.Free();
  end;
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
