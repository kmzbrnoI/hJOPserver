unit PTEndpointBloky;

{
  Endpoint PTserveru /blok/.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
     Generics.Collections;

type
  TPTEndpointBloky = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/bloky/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses JclPCRE, TBloky, PTUtils, TOblRizeni, TOblsRizeni, TBlok;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointBloky.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var params:TDictionary<string, string>;
    stanice:TOR;
    typ:Integer;
begin
 stanice := nil;
 typ := -1;
 params := TDictionary<string, string>.Create();

 try
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   if (params.ContainsKey('stanice')) then
    begin
     stanice := ORs.GetORById(params['stanice']);
     if (stanice = nil) then
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Oblast rizeni neexistuje', 'Oblast rizeni '+params['stanice']+' neexistuje');
       Exit();
      end;
    end;

   if (params.ContainsKey('typ')) then
     typ := TBlk.BlkTypeFromStr(params['typ']);

   Blky.GetPtData(respJson, params.ContainsKey('stav') and PTUtils.HttpParamToBool(params['stav']), stanice, typ);
 finally
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointBloky.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 re.Compile(_ENDPOINT_MATCH_REGEX, false);
 Result := re.Match(path);
 re.Free();
end;

////////////////////////////////////////////////////////////////////////////////

end.

