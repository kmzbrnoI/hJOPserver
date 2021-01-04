unit PTEndpointArea;

{ PTserver endpoint /areas/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointArea = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/areas/[^/]*/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;


implementation

uses PTUtils, StrUtils, ownStrUtils, AreaDb, Area;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointArea.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var urlSuffix, id: string;
    area: TArea;
begin
 urlSuffix := RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/areas/'));
 id := strTillChar(urlSuffix, '/');
 area := Areas.Get(id);

 if (area = nil) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404',
      'Oblasti rizeni neexistuje', 'Oblast rizeni '+id+' neexistuje');
   Exit();
  end;

 area.GetPtData(respJson.O['area']);
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointArea.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

