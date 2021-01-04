unit PTEndpointTrains;

{ PTserver endpoint /trains/. }

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

      function EndpointMatch(path: string): Boolean; override;

  end;

implementation

uses TrainDb;

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

function TPTEndpointTrains.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

