unit PTEndpointUsers;

{
  Endpoint PTserveru /users/.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointUsers = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/users/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses UserDb;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointUsers.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
 try
   UsrDB.GetPtData(respJson);
 finally

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointUsers.EndpointMatch(path:string):boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

