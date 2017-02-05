unit GetSystems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdGlobal, IdHash, IdHashMessageDigest;

type
 TCloseInfo = ( ci_yes = 0,
                ci_system_changing = 1,
                ci_system_started = 2,
                ci_mtb = 3,
                ci_server = 4,
                ci_trakce = 5);

 TGetFunctions=class
    function GetSystemStart:Boolean;
    function CanClose():TCloseInfo;
 end;

var
 GetFunctions:TGetFunctions;

implementation

uses fMain,
     fAdminForm, TechnologieMTB, fSettings, TBLoky, TOblsRizeni, Logging,
     TCPServerOR;


function TGetFunctions.CanClose():TCloseInfo;
begin
  if (SystemData.Status <> TSystemStatus.null) then Exit(TCloseInfo.ci_system_changing);
  if (GetFunctions.GetSystemStart) then Exit(TCloseInfo.ci_system_started);
  if (TrkSystem.openned) then Exit(TCloseInfo.ci_trakce);
  if (ORTCPServer.openned) then Exit(TCloseInfo.ci_server);

  try
    if ((MTB.ready) and (MTB.Opened)) then Exit(TCloseInfo.ci_mtb);
  except

  end;

  Result := TCloseInfo.ci_yes;
end;

////////////////////////////////////////////////////////////////////////////////

function TGetFunctions.GetSystemStart:Boolean;
 begin
  try
    Result := (((TrkSystem.openned) and (MTB.ready) and (ORTCPServer.openned) and (MTB.NoExStarted))
              or (MTB.ready and MTB.NoExStarted and F_Admin.CHB_SystemStart.Checked));
  except
    Result := false;
  end;
 end;//function

end.//uses
