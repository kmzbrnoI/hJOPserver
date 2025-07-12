unit PTEndpointTime;

{ PTserver endpoint /time/. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointTime = class(TPTEndpoint)
  private const
    _ENDPOINT_MATCH_REGEX = '^/time/?$';

  public
    procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      var respJson: TJsonObject); override;
    procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      var respJson:TJsonObject; const reqJson:TJsonObject); override;

    function EndpointMatch(path: string): Boolean; override;

  end;

implementation

uses PTUtils, TimeModel;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTime.OnGET(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; var respJson: TJsonObject);
begin
  respJson.B['used'] := modelTime.used;
  respJson.B['started'] := modelTime.started;
  respJson.F['speed'] := modelTime.speed;
  respJson.S['time'] := FormatDateTime('hh:nn:ss', modelTime.time);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPTEndpointTime.EndpointMatch(path: string): Boolean;
begin
  Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTime.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
begin
  if ((reqJson.Contains('used')) and (reqJson.Contains('used')) and (not reqJson.B['used']) and (reqJson.B['started'])) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nelze used=false a zaroven started=true');
    Exit();
  end;

  if (reqJson.Contains('used')) then
    modelTime.used := reqJson.B['used'];
  if (reqJson.Contains('started')) then
    modelTime.started := reqJson.B['started'];
  if (reqJson.Contains('speed')) then
    modelTime.speed := reqJson.F['speed'];
  if (reqJson.Contains('time')) then
    modelTime.time := StrToTime(reqJson.S['time']);

  Self.OnGET(AContext, ARequestInfo, respJson);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
