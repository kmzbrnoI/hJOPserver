unit PTEndpointJCs;

{ PTserver endpoint /jc/. }

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

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, TechnologieJC, TJCDatabase;

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
       JC.GetPtData(respJson.A['jc'].AddObject, params.ContainsKey('state') and PTUtils.HttpParamToBool(params['state']));
     except
       on E:Exception do
         PTUtils.PtErrorToJson(respJson.A['errors'].AddObject,
          500, 'Chyba pri nacitani JC '+IntToStr(JC.id)+' : '+JC.name,
          E.Message);
     end;

    end;
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointJCs.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
