unit PTEndpointBlock;

{ PTserver endpoint /block/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointBlok = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/blocks/(\d+)/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, BlockDb, Block;

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

   if (not Blocks.IsBlock(blokId)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Blok neexistuje', 'Blok s id '+IntToStr(blokId)+' neexistuje');
     Exit();
    end;

   Blocks.GetBlkByID(blokId, Blk);
   Blk.GetPtData(respJson.O['block'], params.ContainsKey('state') and (PTUtils.HttpParamToBool(params['state'])));
 finally
   re.Free();
   params.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointBlok.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
