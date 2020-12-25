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

      function EndpointMatch(path: string): boolean; override;

  end;

implementation

uses JclPCRE, PTUtils, TrainDb;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrains.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
 try
   Trains.GetPtData(respJson);
 finally

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrains.EndpointMatch(path:string):boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

