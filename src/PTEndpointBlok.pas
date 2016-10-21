unit PTEndpointBlok;

{
  Endpoint PTserveru /blok/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils;

type
  TPTEndpointBlok = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/bloky/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses TCPServerPT, JclPCRE, TBloky, TBlok;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointBlok.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var re: TJclRegEx;
    blokId:Integer;
    Blk:TBlk;
begin
 re := TJclRegEx.Create();
 re.Compile('\d+', false);
 re.Match(ARequestInfo.Document);

 try
   blokId := StrToInt(re.Captures[0]);
 except
   on EConvertError do
    begin
     respJson.A['errors'].Add(TPtServer.ErrorToJson('400', 'Nevalidni id bloku'));
     Exit();
    end;
 end;
 re.Free();

 if (not Blky.IsBlok(blokId)) then
  begin
   respJson.A['errors'].Add(TPtServer.ErrorToJson('404', 'Blok s timto id neexistuje'));
   Exit();
  end;

 Blky.GetBlkByID(blokId, Blk);
 Blk.GetPtData(respJson.O['blok'], (ARequestInfo.Params.Count > 0) and (ARequestInfo.Params[0] = 'stav=True'));
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointBlok.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 re.Compile(_ENDPOINT_MATCH_REGEX, false);
 Result := re.Match(path);
 re.Free();
end;

////////////////////////////////////////////////////////////////////////////////

end.
