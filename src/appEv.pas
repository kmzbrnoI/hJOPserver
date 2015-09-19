unit appEv;

interface

uses SysUtils;

type
  TAppEvents = class
    private

    public
      procedure OnAppException(Sender: TObject; E: Exception);
  end;

var
  AppEvents: TAppEvents;

implementation

uses Logging, fMain;

////////////////////////////////////////////////////////////////////////////////

procedure TAppEvents.OnAppException(Sender: TObject; E: Exception);
begin
 try
   writeLog('APP MAIN EXCEPTION : '+E.ToString, WR_ERROR);
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
  AppEvents := TAppEvents.Create();
finalization
  AppEvents.Free();

end.//unit
