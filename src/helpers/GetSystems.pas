unit GetSystems;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdGlobal;

type
 TCloseInfo = ( ci_yes = 0,
                ci_system_changing = 1,
                ci_system_started = 2,
                ci_rcs = 3,
                ci_server = 4,
                ci_trakce = 5);

 TGetFunctions=class
    function GetSystemStart: Boolean;
    function CanClose(): TCloseInfo;
 end;

var
 GetFunctions: TGetFunctions;

implementation

uses fMain, TechnologieRCS, TBLoky, TOblsRizeni, TCPServerOR, TechnologieTrakce;


function TGetFunctions.CanClose(): TCloseInfo;
begin
  if (SystemData.Status <> TSystemStatus.null) then Exit(TCloseInfo.ci_system_changing);
  if (GetFunctions.GetSystemStart) then Exit(TCloseInfo.ci_system_started);
  if (TrakceI.ConnectedSafe()) then Exit(TCloseInfo.ci_trakce);
  if (ORTCPServer.openned) then Exit(TCloseInfo.ci_server);

  try
    if ((RCSi.ready) and (RCSi.Opened)) then Exit(TCloseInfo.ci_rcs);
  except

  end;

  Result := TCloseInfo.ci_yes;
end;

////////////////////////////////////////////////////////////////////////////////

function TGetFunctions.GetSystemStart: Boolean;
 begin
  try
    Result := ((TrakceI.ConnectedSafe()) and (RCSi.ready) and (ORTCPServer.openned) and (RCSi.NoExStarted));
  except
    Result := false;
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

initialization
  GetFunctions := TGetFunctions.Create();
finalization
  FreeAndNil(GetFunctions);

end.//uses
