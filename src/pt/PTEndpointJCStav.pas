unit PTEndpointJCStav;

{ PTserver endpoint /jc/id/activate. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

type
  TPTEndpointJCStav = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/jc/(\d+)/activate/?$';

    public
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, TJCDatabase, TechnologieJC;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJCStav.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
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

 JC.PostPtActivate(reqJson, respJson);
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJCStav.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
