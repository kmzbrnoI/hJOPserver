unit PTEndpointLok;

{ PTserver endpoint /loks/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

type
  TPTEndpointLok = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/loks/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLok.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var lokoAddr: Word;
    params: TDictionary<string, string>;
begin
 params := TDictionary<string, string>.Create();

 try
   var match := TRegEx.Match(ARequestInfo.Document, _ENDPOINT_MATCH_REGEX);
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   try
     if (not match.Success) then
       raise EConvertError.Create('Unable to parse loco addr');
     lokoAddr := StrToInt(match.Groups[1].Value);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Nevalidni adresa lokmotivy', 'Neni validni adresa lokmotivy');
       Exit();
      end;
   end;

   if ((lokoAddr > 9999) or (HVDb[lokoAddr] = nil)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'Lokomotiva neexistuje', 'Lokomotiva s adresou '+IntToStr(lokoAddr)+' neexistuje');
     Exit();
    end;

   HVDb[lokoAddr].GetPtData(respJson.O['lok'], params.ContainsKey('state') and (PTUtils.HttpParamToBool(params['state'])));
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLok.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

