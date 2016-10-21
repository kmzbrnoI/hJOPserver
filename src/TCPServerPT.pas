unit TCPServerPT;

{
  PTserver slouzi k administraci hJOPserveru.

  PTserver implementuje rozhrani definovane v
  https://github.com/kmzbrnoI/hJOPserver/wiki/ptServer.
}

interface

uses SysUtils, Classes, Graphics,
     IdHttpServer, IdSocketHandle, IdContext, IdCustomHTTPServer, IdComponent;

const
  _PT_DEFAULT_PORT = 5823;

type

  EPTActive = class(Exception);

  TPtServer = class
   private
    httpServer: TIdHTTPServer;

    // http server events
    procedure httpGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure httpError(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo; AException: Exception);
    procedure httpException(AContext: TIdContext; AException: Exception);

    procedure httpAfterBind(Sender:TObject);
    procedure httpStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);

    function IsOpenned():boolean;

    function GetPort():Word;
    procedure SetPort(new:Word);

   public

     constructor Create();
     destructor Destroy(); override;

     procedure Start();
     procedure Stop();

      property openned:boolean read IsOpenned;
      property port:Word read GetPort write SetPort;
  end;

var
  PtServer : TPTServer;

implementation

uses Logging, appEv, fMain;

////////////////////////////////////////////////////////////////////////////////

constructor TPtServer.Create();
var bindings:TIdSocketHandles;
    binding:TIdSocketHandle;
begin
 inherited;

 // bind all addresses
 bindings := TIdSocketHandles.Create(nil);
 binding := bindings.Add();
 binding.SetBinding('0.0.0.0', _PT_DEFAULT_PORT);

 Self.httpServer := TIdHTTPServer.Create(nil);
 Self.httpServer.Bindings := bindings;

 // bind events
 Self.httpServer.OnCommandGet   := Self.httpGet;
 Self.httpServer.OnCommandOther := Self.httpGet;
 Self.httpServer.OnCommandError := Self.httpError;
 Self.httpServer.OnException    := Self.httpException;

 Self.httpServer.OnAfterBind    := Self.httpAfterBind;
 Self.httpServer.OnStatus       := Self.httpStatus;
end;//ctor

destructor TPtServer.Destroy();
begin
 try
   if (Self.httpServer.Active) then
    Self.httpServer.Active := false;
   FreeAndNil(Self.httpServer);
 except

 end;

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TPtServer.Start();
begin
 with (F_Main) do
  begin
   S_PTServer.Visible := true;
   L_PTServer.Visible := true;
   S_PTServer.Brush.Color := clBlue;
  end;

 try
  Self.httpServer.Active := true;
 except
  on E:Exception do
   begin
    writeLog('ERR: Cannot start server : '+E.Message, WR_PT, 1);
    F_Main.S_PTServer.Brush.Color := clRed;
    raise;
   end;
 end;
end;

procedure TPtServer.Stop();
begin
 Self.httpServer.Active := false;

 writeLog('PT server zastaven', WR_PT);

 with (F_Main) do
  begin
   A_PT_Start.Enabled := true;
   A_PT_Stop.Enabled  := false;
   S_PTServer.Brush.Color := clRed;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPtServer.httpGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
 WriteLog('GET:'+ARequestInfo.Document, WR_PT);
end;

procedure TPtServer.httpError(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; AException: Exception);
begin
 WriteLog('ERROR:'+ARequestInfo.Document, WR_PT);
end;

procedure TPtServer.httpException(AContext: TIdContext; AException: Exception);
begin
 AppEvents.LogException(AException, 'PTServer :: httpException');
end;

////////////////////////////////////////////////////////////////////////////////

function TPtServer.IsOpenned():boolean;
begin
 Result := Self.httpServer.Active;
end;

function TPtServer.GetPort():Word;
begin
 Result := Self.httpServer.Bindings[0].Port;
end;

procedure TPtServer.SetPort(new:Word);
begin
 if (Self.httpServer.Active) then
   raise EPTActive.Create('PT server je aktivni, nelze zmenit cislo portu!');
 Self.httpServer.Bindings[0].Port := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPtServer.httpAfterBind(Sender:TObject);
begin
 writeLog('PT server spuštìn', WR_PT);

 with (F_Main) do
  begin
   A_PT_Start.Enabled := false;
   A_PT_Stop.Enabled  := true;
   S_PTServer.Brush.Color := clLime;
  end;
end;

procedure TPtServer.httpStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
 writelog('PT status: '+AStatusText, WR_PT);
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
 PtServer := TPtServer.Create;

finalization
 FreeAndNil(PtServer);

end.//unit

