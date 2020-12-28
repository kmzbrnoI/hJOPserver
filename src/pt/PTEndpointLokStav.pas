unit PTEndpointLokStav;

{
  Endpoint PTserveru /lokStav/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointLokStav = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/lokState/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLokStav.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
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
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni adresa lokmotivy',
          re.Captures[0] + ' neni validni adresa lokmotivy');
       Exit();
      end;
   end;

   if ((lokoAddr > 9999) or (HVDb[lokoAddr] = nil)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Lokomotiva neexistuje',
        'Lokomotiva s adresou '+IntToStr(lokoAddr)+' neexistuje');
     Exit();
    end;

   HVDb[lokoAddr].GetPtState(respJson.O['lokState']);
 finally
   re.Free();
   params.Free();
 end;
end;
////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLokStav.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var re: TJclRegEx;
    lokoAddr:Word;
    params:TDictionary<string, string>;
begin
 re := TJclRegEx.Create();
 params := TDictionary<string, string>.Create();

 try
   re.Compile('\d+', false);
   re.Match(ARequestInfo.Document);

   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   try
     lokoAddr := StrToInt(re.Captures[0]);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400',
          'Nevalidni adresa lokmotivy', re.Captures[0] + ' neni validni adresa lokmotivy');
       Exit();
      end;
   end;

   if ((lokoAddr > 9999) or (HVDb[lokoAddr] = nil)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Lokomotiva neexistuje',
        'Lokomotiva s adresou '+IntToStr(lokoAddr)+' neexistuje');
     Exit();
    end;

   if (not reqJson.Contains('lokStav')) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Chybi json sekce lokStav');
     Exit();
    end;

   HVDb[lokoAddr].PostPtState(reqJson['lokStav'], respJson);
 finally
   re.Free();
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLokStav.EndpointMatch(path:string):boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

