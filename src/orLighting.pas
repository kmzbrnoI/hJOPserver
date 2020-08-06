unit orLighting;

interface

uses TechnologieRCS, Classes, ownStrUtils, SysUtils;

type
  TOrLighting = class
   name: string;
   rcsAddr: TRCSAddr;
   default_state: boolean;

    constructor Create(spnlitem: string);
    procedure Parse(spnlitem: string);
    function GetActive(): Boolean;

    property active: Boolean read GetActive;
  end;


implementation

constructor TOrLighting.Create(spnlitem: string);
begin
 inherited Create();

 Self.default_state := false;
 Self.Parse(spnlitem);
end;

procedure TOrLighting.Parse(spnlitem: string);
var strs: TStrings;
begin
 strs := TStringList.Create();
 try
   ExtractStringsEx(['|'], [], spnlitem, strs);
   Self.name := strs[2];
   Self.rcsAddr := TRCS.RCSAddr(StrToInt(strs[0]), StrToInt(strs[1]));
 finally
   strs.Free();
 end;
end;

function TOrLighting.GetActive(): Boolean;
begin
 try
   Result := (RCSi.GetOutput(Self.rcsAddr) = 1);
 except
   Result := false;
 end;
end;

end.
