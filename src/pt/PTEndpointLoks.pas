unit PTEndpointLoks;

{
  Endpoint PTserveru /loks.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointLoks = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/loks/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses JclPCRE, PTUtils, THvDatabase, THnaciVozidlo, ownConvert;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLoks.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var params:TDictionary<string, string>;
    addr:Word;
    aktivni:Integer;
begin
 aktivni := 0;
 params := TDictionary<string, string>.Create();

 try
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   if (params.ContainsKey('aktivni')) then
     aktivni := ownConvert.BoolToInt(PTUtils.HttpParamToBool(params['aktivni'])) + 1;

   for addr := 0 to _MAX_ADDR-1 do
    begin
     if (HVDb[addr] <> nil) then
      begin
       if ((aktivni = 0) or
          ((aktivni = 1) and (not HVdb[addr].acquired)) or
          ((aktivni = 2) and (HVdb[addr].acquired))) then
         HVDb[addr].GetPtData(respJson.A['loks'].AddObject, params.ContainsKey('stav') and PTUtils.HttpParamToBool(params['stav']));
      end;
    end;
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLoks.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 re.Compile(_ENDPOINT_MATCH_REGEX, false);
 Result := re.Match(path);
 re.Free();
end;

////////////////////////////////////////////////////////////////////////////////

end.

