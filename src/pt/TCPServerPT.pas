unit TCPServerPT;

{
  PTserver slouzi k administraci hJOPserveru.

  PTserver implementuje rozhrani definovane v
  https://github.com/kmzbrnoI/hJOPserver/wiki/ptServer.

  Jak to funguje?
   - Pokud chces vytvorit vlastni endpoint, instanciuj TPTEndpoint do sve
     odvizene tridy a zarad svou tridu do seznamu endpointu v konstruktoru tridy
     TPtServer.
   - Pri prichodu http pozadavku se postupne prochazi ednpointy, endpoint, ktery
     odpovi na EndpointMatch true, je vybran, pruchod je zastaven a endpoint
     zacne zpracovavat data.
   - POST pozadavky se VZDY parsuji jako json objekt.
   - POST content-type by mel byt application/json, pokud neni, je pozadavek
     odmitnut.
}

interface

uses SysUtils, Classes, Graphics, PTEndpoint, Generics.Collections, SyncObjs,
     IdHttpServer, IdSocketHandle, IdContext, IdCustomHTTPServer, IdComponent,
     JsonDataObjects, PTUtils, ExtCtrls;

const
  _PT_DEFAULT_PORT = 5823;
  _PT_MAX_CONNECTIONS = 10;
  _PT_COMPACT_RESPONSE = false;
  _PT_CONTENT_TYPE = 'application/json';
  _PT_DESCRIPTION = 'ptServer v2.1.0';
  _RECEIVE_CHECK_PERIOD_MS = 15;

type

  EPTActive = class(Exception);

  TPtReceived = class
    AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo;
    reqJson, respJson:TJsonObject;
    endpoint: TPTEndpoint;
    processed: boolean;

    constructor Create();
    destructor Destroy(); override;
  end;

  TPtServer = class
   private
    // seznam endpointu PTserveru, PTserver instanciuje vsechny endpointy v konstruktoru
    endpoints: TObjectList<TPTEndpoint>;
    accessTokens: TDictionary<string, string>;

    httpServer: TIdHTTPServer;

    receiveTimer:TTimer;                                                        // must be executed in main thread synchronously!
    received:TObjectQueue<TPtReceived>;                                         // locked by receivedLock!
    receivedLock:TCriticalSection;

    Fcompact:boolean;

    // http server events
    procedure httpGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure httpError(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo; AException: Exception);
    procedure httpException(AContext: TIdContext; AException: Exception);

    procedure httpSinkEndpoint(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      var respJson:TJsonObject);

    procedure ProcessReceivedMessages();
    procedure OnReceiveTimerTick(Sender: TObject);

    procedure httpAfterBind(Sender:TObject);
    procedure httpStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);

    function IsOpenned():boolean;

    function GetPort():Word;
    procedure SetPort(new:Word);

    function GetEndpoint(path:string):TPTEndpoint;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure Start();
     procedure Stop();

     procedure AccessTokenAdd(login: string; token:string);
     procedure AccessTokenRemove(login: string);
     function HasAccess(login:string):boolean;

      property openned:boolean read IsOpenned;
      property port:Word read GetPort write SetPort;
      property compact:boolean read Fcompact write Fcompact;
  end;

var
  PtServer : TPTServer;

implementation

uses Logging, appEv, fMain,
      PTEndpointBlok, PTEndpointBloky, PTEndpointBlokStav, PTEndpointJC,
      PTEndpointLok, PTEndpointLoks, PTEndpointLokStav, PTEndpointJCs,
      PTEndpointJCStav, PTEndpointTrains, PTEndpointTrain, PTEndpointUsers,
      PTEndpointUser;

////////////////////////////////////////////////////////////////////////////////

constructor TPtReceived.Create();
begin
 inherited;
 Self.processed := false;
 Self.reqJson := nil;
 Self.respJson := TJsonObject.Create();
end;

destructor TPtReceived.Destroy();
begin
 if (Self.reqJson <> nil) then
   Self.reqJson.Free();
 Self.respJson.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TPtServer.Create();
var bindings:TIdSocketHandles;
    binding:TIdSocketHandle;
begin
 inherited;

 // initialize variables
 Self.Fcompact := _PT_COMPACT_RESPONSE;

 // bind all addresses
 bindings := TIdSocketHandles.Create(nil);
 binding := bindings.Add();
 binding.SetBinding('0.0.0.0', _PT_DEFAULT_PORT);

 Self.received := TObjectQueue<TPtReceived>.Create();
 Self.receivedLock := TCriticalSection.Create();
 Self.accessTokens := TDictionary<string, string>.Create();

 Self.httpServer := TIdHTTPServer.Create(nil);
 Self.httpServer.Bindings := bindings;
 Self.httpServer.MaxConnections := _PT_MAX_CONNECTIONS;
 Self.httpServer.KeepAlive := true;
 Self.httpServer.ServerSoftware := _PT_DESCRIPTION;

 // bind events
 Self.httpServer.OnCommandGet   := Self.httpGet;
 Self.httpServer.OnCommandOther := Self.httpGet;
 Self.httpServer.OnCommandError := Self.httpError;
 Self.httpServer.OnException    := Self.httpException;

 Self.httpServer.OnAfterBind    := Self.httpAfterBind;
 Self.httpServer.OnStatus       := Self.httpStatus;

 // endpoints
 Self.endpoints := TObjectList<TPTEndpoint>.Create();

 Self.receiveTimer := TTimer.Create(nil);
 Self.receiveTimer.Enabled := false;
 Self.receiveTimer.Interval := _RECEIVE_CHECK_PERIOD_MS;
 Self.receiveTimer.OnTimer := Self.OnReceiveTimerTick;

 // sem doplnit seznam vsech endpointu:
 Self.endpoints.Add(TPTEndpointBlok.Create());
 Self.endpoints.Add(TPTEndpointBloky.Create());
 Self.endpoints.Add(TPTEndpointBlokStav.Create());
 Self.endpoints.Add(TPTEndpointLok.Create());
 Self.endpoints.Add(TPTEndpointLoks.Create());
 Self.endpoints.Add(TPTEndpointLokStav.Create());
 Self.endpoints.Add(TPTEndpointJCs.Create());
 Self.endpoints.Add(TPTEndpointJC.Create());
 Self.endpoints.Add(TPTEndpointJCStav.Create());
 Self.endpoints.Add(TPTEndpointTrains.Create());
 Self.endpoints.Add(TPTEndpointTrain.Create());
 Self.endpoints.Add(TPTEndpointUsers.Create());
 Self.endpoints.Add(TPTEndpointUser.Create());
 Self.endpoints.Add(TPTEndpointUserAuth.Create());
end;//ctor

destructor TPtServer.Destroy();
begin
 try
   Self.endpoints.Free();
   if (Self.httpServer.Active) then
    Self.httpServer.Active := false;
   Self.httpServer.Free();

   Self.receivedLock.Acquire(); // wait for everything to end
   FreeAndNil(Self.received);
   Self.receivedLock.Release();
   FreeAndNil(Self.receivedLock);
   Self.accessTokens.Free();
 finally
   inherited;
 end;
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
  Self.receiveTimer.Enabled := true;
 except
  on E:Exception do
   begin
    writeLog('ERR: Cannot start server : '+E.Message, WR_PT);
    F_Main.S_PTServer.Brush.Color := clRed;
    raise;
   end;
 end;
end;

procedure TPtServer.Stop();
begin
 Self.httpServer.Active := false;
 Self.receiveTimer.Enabled := false;

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
var received:TPtReceived;
begin
 received := TPtReceived.Create();
 try
   AResponseInfo.ContentType := 'application/json; charset=utf-8';

   received.AContext := AContext;
   received.ARequestInfo := ARequestInfo;
   received.endpoint := Self.GetEndpoint(ARequestInfo.Document);

   if (received.endpoint = nil) then
    begin
     Self.httpSinkEndpoint(AContext, ARequestInfo, received.respJson);
     received.processed := true;
    end else begin

     if ((received.endpoint.AuthRequired(ARequestInfo.CommandType)) and
         ((not ARequestInfo.AuthExists) or (not Self.accessTokens.ContainsKey(ARequestInfo.AuthUsername)) or
         (Self.accessTokens[ARequestInfo.AuthUsername] <> ARequestInfo.AuthPassword))) then
      begin
       AResponseInfo.ResponseNo := 401;
       PTUtils.PtErrorToJson(received.respJson.A['errors'].AddObject, '401', 'Unauthorized',
                             'Neexitující/neplatný autorizační token');
       received.processed := true;
      end;

     case (ARequestInfo.CommandType) of
      hcGET: ;
      hcPOST, hcPUT, hcDELETE: begin
         if ((not received.processed) and (ARequestInfo.ContentType <> _PT_CONTENT_TYPE)) then
          begin
           PTUtils.PtErrorToJson(received.respJson.A['errors'].AddObject, '406', 'Not acceptable',
                                 'S timto content-type si neumim poradit');
           received.processed := true;
          end;
         if (not received.processed) then
          begin
           received.reqJson := TJsonObject.ParseFromStream(ARequestInfo.PostStream, TEncoding.UTF8) as TJsonObject;

           if (received.reqJson = nil) then
            begin
             PTUtils.PtErrorToJson(received.respJson.A['errors'].AddObject, '400', 'Bad request',
                                   'Navalidni JSON objekt v pozadavku');
             received.processed := true;
            end;
          end;
      end;
     else
      PTUtils.PtErrorToJson(received.respJson.A['errors'].AddObject, '405', 'Method not allowed',
                            'S touto HTTP metodou si neumim poradit');
      received.processed := true;
     end;

     if (not received.processed) then
      begin
       Self.receivedLock.Acquire();
       try
         Self.received.Enqueue(received);
       finally
         Self.receivedLock.Release();
       end;
      end;
    end;
 except
   received.Free();
   AResponseInfo.ContentText := '{"errors":[{"title":"Server general error"}]}';
   Exit();
 end;

 // Wait for finishing of the command to send response
 while ((not received.processed) and (Self.received <> nil)) do
   Sleep(_RECEIVE_CHECK_PERIOD_MS); // sleep in its own thread

 if (Self.receivedLock <> nil) then
   Self.receivedLock.Acquire();
 try
   AResponseInfo.ContentStream := TStringStream.Create();
   AResponseInfo.FreeContentStream := true;
   received.respJson.SaveToStream(AResponseInfo.ContentStream, Self.compact, TEncoding.UTF8);
 finally
   received.Free();
   if (Self.receivedLock <> nil) then
     Self.receivedLock.Release();
 end;
end;

function TPtServer.GetEndpoint(path:string):TPTEndpoint;
var endpoint:TPTEndpoint;
begin
 Result := nil;
 for endpoint in Self.endpoints do
   if (endpoint.EndpointMatch(path)) then
     Exit(endpoint);
end;

procedure TPtServer.OnReceiveTimerTick(Sender: TObject);
begin
 Self.ProcessReceivedMessages();
end;

procedure TPtServer.ProcessReceivedMessages();
var received:TPtReceived;
begin
 if (not Assigned(Self.received)) then
   Exit(); // everything is shutting down

 receivedLock.Acquire();

 try
   while (Self.received.Count > 0) do
    begin
     received := Self.received.Extract();
     try
       case (received.ARequestInfo.CommandType) of
         hcGet: received.endpoint.OnGET(received.AContext, received.ARequestInfo, received.respJson);
         hcPOST: received.endpoint.OnPOST(received.AContext, received.ARequestInfo,
                                          received.respJson, received.reqJson);
         hcPUT : received.endpoint.OnPUT(received.AContext, received.ARequestInfo,
                                         received.respJson, received.reqJson);
         hcDELETE: received.endpoint.OnDELETE(received.AContext, received.ARequestInfo,
                                              received.respJson, received.reqJson);
       end;
     except
      on Eorig:Exception do
        PTUtils.PtErrorToJson(received.respJson.A['errors'].AddObject, '500', 'Request exception', Eorig.Message);
     end;

     received.processed := true;
    end;
 finally
   receivedLock.Release();
 end;
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
 writeLog('PT server spuštěn', WR_PT);

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
// Tento virtualni endpoint je volan pokud nenajdeme zadny jiny vhodny endpoint.

procedure TPtServer.httpSinkEndpoint(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Neznamy endpoint');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPtServer.AccessTokenAdd(login: string; token:string);
begin
 Self.accessTokens.Add(login, token);
end;

procedure TPtServer.AccessTokenRemove(login: string);
begin
 Self.accessTokens.Remove(login);
end;

function TPtServer.HasAccess(login:string):boolean;
begin
 Result := Self.accessTokens.ContainsKey(login);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 PtServer := TPtServer.Create;

finalization
 FreeAndNil(PtServer);

end.//unit

