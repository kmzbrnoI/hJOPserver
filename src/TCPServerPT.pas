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

uses SysUtils, Classes, Graphics, PTEndpoint, Generics.Collections,
     IdHttpServer, IdSocketHandle, IdContext, IdCustomHTTPServer, IdComponent,
     JsonDataObjects, PTUtils;

const
  _PT_DEFAULT_PORT = 5823;
  _PT_MAX_CONNECTIONS = 10;
  _PT_COMPACT_RESPONSE = false;

type

  EPTActive = class(Exception);

  TPtServer = class
   private
    // seznam endpointu PTserveru, PTserver instanciuje vsechny endpointy v konstruktoru
    enpoints: TList<TPTEndpoint>;

    httpServer: TIdHTTPServer;

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
  end;

var
  PtServer : TPTServer;

implementation

uses Logging, appEv, fMain,
      PTEndpointBlok, PTEndpointBloky, PTEndpointLok;

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
 Self.httpServer.MaxConnections := _PT_MAX_CONNECTIONS;
 Self.httpServer.KeepAlive := true;
 Self.httpServer.ServerSoftware := 'PTserver';

 // bind events
 Self.httpServer.OnCommandGet   := Self.httpGet;
 Self.httpServer.OnCommandOther := Self.httpGet;
 Self.httpServer.OnCommandError := Self.httpError;
 Self.httpServer.OnException    := Self.httpException;

 Self.httpServer.OnAfterBind    := Self.httpAfterBind;
 Self.httpServer.OnStatus       := Self.httpStatus;

 // endpoints
 Self.enpoints := TList<TPTEndpoint>.Create();

 // sem doplnit seznam vsech endpointu:
 Self.enpoints.Add(TPTEndpointBlok.Create());
 Self.enpoints.Add(TPTEndpointBloky.Create());
 Self.enpoints.Add(TPTEndpointLok.Create());
end;//ctor

destructor TPtServer.Destroy();
var i:Integer;
begin
 try
   for i := 0 to Self.enpoints.Count-1 do
     Self.enpoints[i].Free();
   Self.enpoints.Free();
   if (Self.httpServer.Active) then
    Self.httpServer.Active := false;
   Self.httpServer.Free();
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
var reqJson, respJson:TJsonObject;
    endpoint:TPTEndpoint;
    found:boolean;
begin
 try
   respJson := TJsonObject.Create();

   try
     // vybereme vhodny endpoint
     found := false;
     for endpoint in Self.enpoints do
      begin
       if (endpoint.EndpointMatch(ARequestInfo.Document)) then
        begin
         found := true;
         case (ARequestInfo.CommandType) of
          hcGET: endpoint.OnGET(AContext, ARequestInfo, respJson);
          hcPOST: begin
             reqJson := TJsonObject.ParseFromStream(ARequestInfo.PostStream, TEncoding.UTF8) as TJsonObject;
             endpoint.OnPOST(AContext, ARequestInfo, respJson, reqJson);
             reqJson.Free();
          end;
         end;//end
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
     respJson.SaveToStream(AResponseInfo.ContentStream, _PT_COMPACT_RESPONSE, TEncoding.UTF8);

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

