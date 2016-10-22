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
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); virtual;

      function EndpointMatch(path:string):boolean; virtual; abstract;
  end;

implementation

uses PTUtils;

procedure TPTEndpoint.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
end;

procedure TPTEndpoint.OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
end;

procedure TPTEndpoint.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
end;

end.
