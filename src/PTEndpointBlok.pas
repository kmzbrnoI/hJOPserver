unit PTEndpointBlok;

{
  Endpoint PTserveru /blok/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

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

uses PTUtils, JclPCRE, TBloky, TBlok;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointBlok.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var re: TJclRegEx;
    blokId:Integer;
    Blk:TBlk;
    params:TDictionary<string, string>;
begin
 re := TJclRegEx.Create();
 params := TDictionary<string, string>.Create();

 try
   // Toto parsovani cisla neni vubec hezke, ale tyto regexpy neumi skupiny :(
   // S prechodem na novejsi Delphi XE doporucuji vyuzivat regexpy vestavene v Delphi XE.
   re.Compile('\d+', false);
   re.Match(ARequestInfo.Document);

   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   try
     blokId := StrToInt(re.Captures[0]);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni id bloku', re.Captures[0] + ' neni validni id bloku');
       Exit();
      end;
   end;

   if (not Blky.IsBlok(blokId)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Blok neexistuje', 'Blok s id '+IntToStr(blokId)+' neexistuje');
     Exit();
    end;

   Blky.GetBlkByID(blokId, Blk);
   Blk.GetPtData(respJson.O['blok'], params.ContainsKey('stav') and (PTUtils.HttpParamToBool(params['stav'])));
 finally
   re.Free();
   params.Free();
 end;
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
