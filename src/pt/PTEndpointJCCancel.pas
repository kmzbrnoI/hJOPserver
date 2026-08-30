unit PTEndpointJCCancel;

{ PTserver endpoint /jc/id/cancel. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

type
  TPTEndpointJCCancel = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/jc/(\d+)/cancel/?$';

    public
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path: string): Boolean; override;

  end;

implementation

uses PTUtils, TJCDatabase, TechnologieJC;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJCCancel.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var jcId: Integer;
begin
 var match := TRegEx.Match(ARequestInfo.Document, _ENDPOINT_MATCH_REGEX);

 try
   if (not match.Success) then
     raise EConvertError.Create('Unable to parse jc id');
   jcId := StrToInt(match.Groups[1].Value);
 except
   on EConvertError do
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Nevalidni id JC');
     Exit();
    end;
 end;

 var JC := JCDb.GetJCByID(jcId);
 if (JC = nil) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'JC neexistuje', 'JC s id '+IntToStr(jcId)+' neexistuje');
   Exit();
  end;

 if (JC.active) then
 begin
   JC.StartCancelling(JC.signal.areas[0]);
 end else begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'JC neni aktivni', 'Nelze zrusit nepostavenou cestu');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJCCancel.EndpointMatch(path: string): Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
