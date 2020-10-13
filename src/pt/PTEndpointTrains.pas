unit PTEndpointTrains;

{
  Endpoint PTserveru /trains/.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointTrains = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/trains/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses JclPCRE, PTUtils, SprDb;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrains.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
 try
   Soupravy.GetPtData(respJson);
 finally

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrains.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 re.Compile(_ENDPOINT_MATCH_REGEX, false);
 Result := re.Match(path);
 re.Free();
end;

////////////////////////////////////////////////////////////////////////////////

end.

