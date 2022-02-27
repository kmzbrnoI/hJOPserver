unit PTEndpoint;

{
  Trida TPTEndpoint je bazovou tridou pro kazdy endpoint PTserveru.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, RegularExpressions;

type
  TPTEndpoint = class
    private const
      _ENDPOINT_MATCH_REGEX = '';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); virtual;
      procedure OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); virtual;
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); virtual;
      procedure OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); virtual;

      function EndpointMatch(path: string): Boolean; virtual; abstract;
      function AuthRequired(cmdType: THTTPCommandType): Boolean; virtual;
      class function PatternMatch(path: string; pattern: string): Boolean;
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

procedure TPTEndpoint.OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject);
begin
 PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
end;

function TPTEndpoint.AuthRequired(cmdType: THTTPCommandType): Boolean;
begin
 Result := (cmdType = hcPOST) or (cmdType = hcPUT) or (cmdType = hcDELETE);
end;

class function TPTEndpoint.PatternMatch(path: string; pattern: string): Boolean;
begin
 Result := TRegEx.Match(path, pattern).Success;
end;

end.
