unit PTEndpoint;

{
  Trida TPTEndpoint je bazovou tridou pro kazdy endpoint PTserveru.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects;

type
  TPTEndpoint = class
    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); virtual;
      procedure OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); virtual;

      function EndpointMatch(path:string):boolean; virtual; abstract;
  end;

implementation

procedure TPTEndpoint.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
  // This function should be empty.
end;

procedure TPTEndpoint.OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject);
begin
  // This function should be empty.
end;

end.
