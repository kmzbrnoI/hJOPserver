unit PTEndpointStatus;

{ PTserver endpoint /status/. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointStatus = class(TPTEndpoint)
  private const
    _ENDPOINT_MATCH_REGEX = '^/status/?$';

  public
    procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      var respJson: TJsonObject); override;

    function EndpointMatch(path: string): Boolean; override;

  end;

implementation

uses PTUtils, RCSc, TrakceC, TrakceIFace, TCPServerPanel;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointStatus.OnGET(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; var respJson: TJsonObject);
begin
  var rcso: TJsonObject := respJSON.O['rcs'];
  rcso.B['opened'] := RCSi.NoExOpened();
  rcso.B['started'] := RCSi.NoExStarted();

  var trakceo: TJsonObject := respJSON.O['trakce'];
  trakceo.B['connected'] := trakce.Connected();
  case (trakce.TrackStatusSafe()) of
    TTrkStatus.tsOff: trakceo.S['status'] := 'off';
    TTrkStatus.tsOn: trakceo.S['status'] := 'on';
    TTrkStatus.tsProgramming: trakceo.S['status'] := 'programming';
  else
    trakceo.S['status'] := 'unknown';
  end;
  trakceo.B['emergency'] := trakce.emergency;

  respJSON.O['panelserver'].B['running'] := PanelServer.openned;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPTEndpointStatus.EndpointMatch(path: string): Boolean;
begin
  Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
