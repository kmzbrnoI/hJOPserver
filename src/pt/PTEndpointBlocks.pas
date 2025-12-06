unit PTEndpointBlocks;

{ PTserver endpoint /blocks/. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointBloky = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/blocks/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses BlockDb, PTUtils, Area, AreaDb, Block;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointBloky.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var params:TDictionary<string, string>;
    area: TArea;
    typ: TBlkType;
begin
 area := nil;
 typ := btAny;
 params := TDictionary<string, string>.Create();

 try
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   if (params.ContainsKey('station')) then
    begin
     area := Areas.Get(params['station']);
     if (area = nil) then
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'Oblast rizeni neexistuje',
          'Oblast rizeni '+params['statino']+' neexistuje');
       Exit();
      end;
    end;

   if (params.ContainsKey('type')) then
     typ := TBlk.BlkTypeFromStr(params['type']);

   Blocks.GetPtData(respJson, params.ContainsKey('state') and PTUtils.HttpParamToBool(params['state']), area, typ);
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointBloky.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.

