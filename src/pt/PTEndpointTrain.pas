unit PTEndpointTrain;

{
  Endpoint PTserveru /train/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointTrain = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/trains/.+/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, SprDb, StrUtils;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var re: TJclRegEx;
    trainName: string;
    trainIndex: Integer;
begin
 re := TJclRegEx.Create();

 try
   // Toto parsovani cisla neni vubec hezke, ale tyto regexpy neumi skupiny :(
   // S prechodem na novejsi Delphi XE doporucuji vyuzivat regexpy vestavene v Delphi XE.
   re.Compile(_ENDPOINT_MATCH_REGEX, false);
   re.Match(ARequestInfo.Document);
   trainName := RightStr(re.Captures[0], Length(re.Captures[0])-8);
   if (trainName[Length(trainName)] = '/') then
     trainName := LeftStr(trainName, Length(trainName)-1);

   trainIndex := Soupravy.GetSprIndexByName(trainName);
   if (trainIndex = -1) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Souprava neexistuje', 'Souprava s id '+trainName+' neexistuje');
     Exit();
    end;

   Soupravy[trainIndex].GetPtData(respJson.O['train']);
 finally
   re.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrain.EndpointMatch(path:string):boolean;
var re: TJclRegEx;
begin
 re := TJclRegEx.Create();
 try
   re.Compile(_ENDPOINT_MATCH_REGEX, false);
   Result := re.Match(path);
 finally
   re.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
