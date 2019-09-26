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
     JsonDataObjects, PTUtils;

const
  _PT_DEFAULT_PORT = 5823;
  _PT_MAX_CONNECTIONS = 10;
  _PT_COMPACT_RESPONSE = false;
  _PT_CONTENT_TYPE = 'application/json';
  _PT_DESCRIPTION = 'ptServer v1.1.0';

type

  EPTActive = class(Exception);

  TPtServer = class
   private
    // seznam endpointu PTserveru, PTserver instanciuje vsechny endpointy v konstruktoru
    endpoints: TObjectList<TPTEndpoint>;

    httpServer: TIdHTTPServer;
    readLock:TCriticalSection;

    Fcompact:boolean;

    // http server events
    procedure httpGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure httpError(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo; AException: Exception);
    procedure httpException(AContext: TIdContext; AException: Exception);

    procedure httpSinkEndpoint(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      var respJson:TJsonObject);

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
      property compact:boolean read Fcompact write Fcompact;
  end;

var
  PtServer : TPTServer;

implementation

uses Logging, appEv, fMain,
      PTEndpointBlok, PTEndpointBloky, PTEndpointBlokStav,
      PTEndpointLok, PTEndpointLoks, PTEndpointLokStav;

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

 Self.readLock := TCriticalSection.Create();

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

 // sem doplnit seznam vsech endpointu:
 Self.endpoints.Add(TPTEndpointBlok.Create());
 Self.endpoints.Add(TPTEndpointBloky.Create());
 Self.endpoints.Add(TPTEndpointBlokStav.Create());
 Self.endpoints.Add(TPTEndpointLok.Create());
 Self.endpoints.Add(TPTEndpointLoks.Create());
 Self.endpoints.Add(TPTEndpointLokStav.Create());
end;//ctor

destructor TPtServer.Destroy();
begin
 try
   Self.endpoints.Free();
   if (Self.httpServer.Active) then
    Self.httpServer.Active := false;
   Self.httpServer.Free();
   Self.readLock.Free();
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
    writeLog('ERR: Cannot start server : '+E.Message, WR_PT);
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
var reqJson, respJson:TJsonObject;
    endpoint:TPTEndpoint;
    found:boolean;
begin
 Self.readLock.Acquire();

 try
   respJson := TJsonObject.Create();

   try
     // vybereme vhodny endpoint
     found := false;
     for endpoint in Self.endpoints do
      begin
       if (endpoint.EndpointMatch(ARequestInfo.Document)) then
        begin
         found := true;
         case (ARequestInfo.CommandType) of
          hcGET: endpoint.OnGET(AContext, ARequestInfo, respJson);
          hcPOST, hcPUT: begin
             if (ARequestInfo.ContentType <> _PT_CONTENT_TYPE) then
              begin
               PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '406', 'Not acceptable', 'S timto content-type si neumim poradit');
               found := true;
               break;
              end;

             reqJson := TJsonObject.ParseFromStream(ARequestInfo.PostStream, TEncoding.UTF8) as TJsonObject;

             if (reqJson = nil) then
              begin
               PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Bad request', 'Navalidni JSON objekt v pozadavku');
               found := true;
               break;
              end;

             case (ARequestInfo.CommandType) of
               hcPOST: endpoint.OnPOST(AContext, ARequestInfo, respJson, reqJson);
               hcPUT : endpoint.OnPUT(AContext, ARequestInfo, respJson, reqJson);
             end;
             reqJson.Free();
          end;
         else
          PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
          found := true;
         end;
         break;
        end;
      end;

     // zadny endpoint nenzalezen -> sink
     if (not found) then
       Self.httpSinkEndpoint(AContext, ARequestInfo, respJson);

     // vytvoreni odpovedi
     AResponseInfo.ContentType := 'application/json; charset=utf-8';
     AResponseInfo.ContentStream := TStringStream.Create();
     AResponseInfo.FreeContentStream := true;
     respJson.SaveToStream(AResponseInfo.ContentStream, Self.compact, TEncoding.UTF8);

   except
    on Eorig:Exception do
     begin
      try
        AResponseInfo.ContentType := 'application/json; charset=utf-8';
        AResponseInfo.ContentStream := TStringStream.Create();
        AResponseInfo.FreeContentStream := true;
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '500', 'Request exception', Eorig.Message);
        respJson.SaveToStream(AResponseInfo.ContentStream, _PT_COMPACT_RESPONSE, TEncoding.UTF8);
      except
        AResponseInfo.ContentText := '{"errors":["title":"Server general error"]}';
      end;
     end;
   end;
 finally
   respJson.Free();
   Self.readLock.Release();
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
// Tento virtualni endpoint je volan pokud nenajdeme zadny jiny vhodny endpoint.

procedure TPtServer.httpSinkEndpoint(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Neznamy endpoint');
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
 PtServer := TPtServer.Create;

finalization
 FreeAndNil(PtServer);

end.//unit

