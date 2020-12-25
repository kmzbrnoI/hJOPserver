unit PTEndpointLok;

{
  Endpoint PTserveru /loks/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointLok = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/loks/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLok.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var re: TJclRegEx;
    lokoAddr:Word;
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
     lokoAddr := StrToInt(re.Captures[0]);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni adresa lokmotivy', re.Captures[0] + ' neni validni adresa lokmotivy');
       Exit();
      end;
   end;

   if ((lokoAddr > 9999) or (HVDb[lokoAddr] = nil)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Lokomotiva neexistuje', 'Lokomotiva s adresou '+IntToStr(lokoAddr)+' neexistuje');
     Exit();
    end;

   HVDb[lokoAddr].GetPtData(respJson.O['lok'], params.ContainsKey('stav') and (PTUtils.HttpParamToBool(params['stav'])));
 finally
   re.Free();
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLok.EndpointMatch(path:string):boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

