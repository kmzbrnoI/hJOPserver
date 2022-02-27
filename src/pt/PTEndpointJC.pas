unit PTEndpointJC;

{ PTserver endpoint /jc/jc. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

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

uses PTUtils, TechnologieJC, TJCDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointJC.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var jcId: Integer;
    params: TDictionary<string, string>;
begin
 params := TDictionary<string, string>.Create();

 try
   var match := TRegEx.Match(ARequestInfo.Document, _ENDPOINT_MATCH_REGEX);
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   try
     if (not match.Success) then
       raise EConvertError.Create('Unable to parse name');
     jcId := StrToInt(match.Groups[1].Value);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni id JC');
       Exit();
      end;
   end;

   var JC := JCDb.GetJCByID(jcId);
   if (JC = nil) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'JC neexistuje', 'JC s id '+IntToStr(jcId)+' neexistuje');
     Exit();
    end;

   JC.GetPtData(respJson.O['jc'], params.ContainsKey('state') and (PTUtils.HttpParamToBool(params['state'])));
 finally
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
