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

/// /////////////////////////////////////////////////////////////////////////////

end.
