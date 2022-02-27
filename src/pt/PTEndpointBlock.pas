unit PTEndpointBlock;

{ PTserver endpoint /block/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, RegularExpressions;

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

uses PTUtils, BlockDb, Block;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointBlok.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var blokId: Integer;
    params: TDictionary<string, string>;
begin
 params := TDictionary<string, string>.Create();

 try
   var match := TRegEx.Match(ARequestInfo.Document, _ENDPOINT_MATCH_REGEX);
   PTUtils.HttpParametersToDict(ARequestInfo.Params, params);

   try
     if (not match.Success) then
       raise EConvertError.Create('Unable to parse block id');
     blokId := StrToInt(match.Groups[1].Value);
   except
     on EConvertError do
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Nevalidni id bloku');
       Exit();
      end;
   end;

   if (not Blocks.IsBlock(blokId)) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Blok neexistuje', 'Blok s id '+IntToStr(blokId)+' neexistuje');
     Exit();
    end;

   var blk := Blocks.GetBlkByID(blokId);
   blk.GetPtData(respJson.O['block'], params.ContainsKey('state') and (PTUtils.HttpParamToBool(params['state'])));
 finally
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
