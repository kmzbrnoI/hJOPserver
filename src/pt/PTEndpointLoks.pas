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

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, THvDatabase, THnaciVozidlo, ownConvert;

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

   if (params.ContainsKey('active')) then
     aktivni := ownConvert.BoolToInt(PTUtils.HttpParamToBool(params['active'])) + 1;

   for addr := 0 to _MAX_ADDR-1 do
    begin
     if (HVDb[addr] <> nil) then
      begin
       if ((aktivni = 0) or
          ((aktivni = 1) and (not HVdb[addr].acquired)) or
          ((aktivni = 2) and (HVdb[addr].acquired))) then
         HVDb[addr].GetPtData(respJson.A['loks'].AddObject, params.ContainsKey('state') and PTUtils.HttpParamToBool(params['state']));
      end;
    end;
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointLoks.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

