unit PTEndpointJC;

{ PTserver endpoint /jc/jc. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointJC = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/jc/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, TechnologieJC, TJCDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJC.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var re: TJclRegEx;
    jcId:Integer;
    JC:TJC;
    params:TDictionary<string, string>;
begin
 re := TJclRegEx.Create();
 params := TDictionary<string, string>.Create();

 try
   // Toto parsovani cisla neni vubec hezke, ale tyto regexpy neumi skupiny :(
   // S prechodem na novejsi Delphi XE doporucuji vyuzivat regexpy vestavene v Delphi XE.
   re.Compile('\d+', false);
   re.Match(ARequestInfo.Document);

   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

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

   JC.GetPtData(respJson.O['jc'], params.ContainsKey('state') and (PTUtils.HttpParamToBool(params['state'])));
 finally
   re.Free();
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJC.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
