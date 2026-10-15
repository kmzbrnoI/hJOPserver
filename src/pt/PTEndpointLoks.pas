unit PTEndpointLoks;

{ PTserver endpoint /loks/. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointLoks = class(TPTEndpoint)
  private const
    _ENDPOINT_MATCH_REGEX = '^/loks/?$';

  public
    procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject); override;

    function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, TRVDatabase, TRailVehicle, ownConvert;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointLoks.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var params:TDictionary<string, string>;
begin
  var active: Integer := 0;
  params := TDictionary<string, string>.Create();

  try
    PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

    if (params.ContainsKey('active')) then
      active := ownConvert.BoolToInt(PTUtils.HttpParamToBool(params['active'])) + 1;

    for var addr: Word := 0 to _MAX_ADDR-1 do
    begin
      if (RVDb[addr] <> nil) then
      begin
        if ((active = 0) or
           ((active = 1) and (not RVDb[addr].acquired)) or
           ((active = 2) and (RVDb[addr].acquired))) then
          RVDb[addr].GetPtData(respJson.A['loks'].AddObject, params.ContainsKey('state') and PTUtils.HttpParamToBool(params['state']));
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

