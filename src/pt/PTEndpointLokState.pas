unit PTEndpointLokState;

{ PTserver endpoint /lokState/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

type
  TPTEndpointLokStav = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/lokState/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLokStav.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
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
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni adresa lokomotivy');
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
   params.Free();
 end;
end;
////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLokStav.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
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
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni adresa lokmotivy');
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
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLokStav.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

