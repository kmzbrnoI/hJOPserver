unit PTEndpointJCStav;

{
  Endpoint PTserveru /jc/id/stav.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

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

uses PTUtils, JclPCRE, TJCDatabase, TechnologieJC;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJCStav.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var re: TJclRegEx;
    jcId:Integer;
    JC:TJC;
begin
 re := TJclRegEx.Create();
 try
   re.Compile('\d+', false);
   re.Match(ARequestInfo.Document);

   try
     jcId := StrToInt(re.Captures[0]);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni id JC', re.Captures[0] + ' neni validni id JC');
       Exit();
      end;
   end;

   JC := JCDb.GetJCByID(jcId);
   if (JC = nil) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'JC neexistuje', 'JC s id '+IntToStr(jcId)+' neexistuje');
     Exit();
    end;

   JC.PostPtStav(reqJson, respJson);
 finally
   re.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJCStav.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
