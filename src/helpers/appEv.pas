unit appEv;

interface

uses SysUtils, Classes;

type
  TAppEvents = class
    private
     str: TStrings;
    public
      constructor Create();
      destructor Destroy(); override;

      procedure OnAppException(Sender: TObject; E: Exception);
      procedure LogException(E: Exception; prefix: string = '');
  end;

var
  AppEvents: TAppEvents;

implementation

uses Logging, fMain, JclDebug;

////////////////////////////////////////////////////////////////////////////////

constructor TAppEvents.Create();
begin
 inherited;
 Self.str := TStringList.Create();
end;

destructor TAppEvents.Destroy();
begin
 Self.str.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAppEvents.OnAppException(Sender: TObject; E: Exception);
begin
 Self.LogException(E, 'APP MAIN EXCEPTION');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAppEvents.LogException(E: Exception; prefix: string = '');
var i: Integer;
begin
 try
  Self.str.Clear();
  if (prefix <> '') then
    Self.str.Add(prefix + ' ' + E.Message)
  else
    Self.str.Add(E.Message);

  for i := 0 to JclLastExceptStackList.Count-1 do
    if (GetLocationInfo(JclLastExceptStackList.Items[i].CallerAddr).LineNumber <> 0) then
      Self.str.Add(GetLocationInfoStr(JclLastExceptStackList.Items[i].CallerAddr));
  Log(Self.str, WR_ERROR);
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  AppEvents := TAppEvents.Create();

  // Enable raw mode (default mode uses stack frames which aren't always generated by the compiler)
  Include(JclStackTrackingOptions, stRawMode);
  // Disable stack tracking in dynamically loaded modules (it makes stack tracking code a bit faster)
  Include(JclStackTrackingOptions, stStaticModuleList);

  // Initialize Exception tracking
  JclStartExceptionTracking;

finalization
  // Uninitialize Exception tracking
  JclStopExceptionTracking;

  AppEvents.Free();

end.//unit
