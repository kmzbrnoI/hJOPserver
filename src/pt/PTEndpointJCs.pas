unit PTEndpointJCs;

{
  Endpoint PTserveru /jc/.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointJCs = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/jc/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses JclPCRE, PTUtils, TechnologieJC, TJCDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJCs.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var params:TDictionary<string, string>;
    JC:TJC;
begin
 params := TDictionary<string, string>.Create();

 try
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);
   for JC in JCDb do
    begin
     try
       JC.GetPtData(respJson.A['jc'].AddObject, params.ContainsKey('stav') and PTUtils.HttpParamToBool(params['stav']));
     except
       on E:Exception do
         PTUtils.PtErrorToJson(respJson.A['errors'].AddObject,
          '500', 'Chyba pri nacitani JC '+IntToStr(JC.id)+' : '+JC.name,
          E.Message);
     end;

    end;
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJCs.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 re.Compile(_ENDPOINT_MATCH_REGEX, false);
 Result := re.Match(path);
 re.Free();
end;

////////////////////////////////////////////////////////////////////////////////

end.
