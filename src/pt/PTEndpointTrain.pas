unit PTEndpointTrain;

{
  Endpoint PTserveru /train/id.
}

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, Train;


type
  TPTEndpointTrain = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/trains/.+?/?';
      _ET_NONE = 0;
      _ET_TRAIN = 1;
      _ET_PODJ = 2;

    private

      function CommonGetPut(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject): TDictionary<string, Integer>;

      procedure OnGETTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; train: TTrain);
      procedure OnPUTTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
      procedure OnGETPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; train: TTrain; podjId: Integer);
      procedure OnPUTPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain; podjId: Integer);

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;
      procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;
      procedure OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;
      procedure OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, JclPCRE, TrainDb, StrUtils, ownStrUtils, predvidanyOdjezd, BlockDb,
      TBlock, TBlockTrack, TechnologieTrakce;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrain.CommonGetPut(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject): TDictionary<string, Integer>;
var re: TJclRegEx;
    urlSuffix: string;
    trainName: string;
    podjId: string;
    trainIndex: Integer;
begin
 Result := TDictionary<string, Integer>.Create();
 urlSuffix := RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/trains/'));
 trainName := strTillChar(urlSuffix, '/');

 trainIndex := Trains.GetTrainIndexByName(trainName);
 Result.AddOrSetValue('type', _ET_NONE);
 if (trainIndex = -1) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'Souprava neexistuje', 'Souprava s id '+trainName+' neexistuje');
   Exit();
  end;

 Result.AddOrSetValue('train', trainIndex);
 re := TJclRegEx.Create();
 try
   re.Compile('podj/\d+/?$', false);
   if (re.Match(ARequestInfo.Document)) then begin
     podjId := RightStr(re.Captures[0], Length(re.Captures[0])-Length('podj/'));
     if (podjId[Length(podjId)] = '/') then
       podjId := LeftStr(podjId, Length(podjId)-1);
     Result.AddOrSetValue('type', _ET_PODJ);
     Result.AddOrSetValue('podj', StrToInt(podjId));
   end else
     Result.AddOrSetValue('type', _ET_TRAIN);
 finally
   re.Free();
 end;
end;

procedure TPTEndpointTrain.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var dict: TDictionary<string, Integer>;
begin
 dict := Self.CommonGetPut(AContext, ARequestInfo, respJson);
 try
   if (dict['type'] = _ET_TRAIN) then
     Self.OnGETTrain(AContext, ARequestInfo, respJson, Trains[dict['train']])
   else if (dict['type'] = _ET_PODJ) then
     Self.OnGETPodj(AContext, ARequestInfo, respJson, Trains[dict['train']], dict['podj']);
 finally
   dict.Free();
 end;
end;

procedure TPTEndpointTrain.OnGETTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; train: TTrain);
begin
 train.GetPtData(respJson.O['train']);
end;

procedure TPTEndpointTrain.OnGETPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; train: TTrain; podjId: Integer);
begin
 if (not train.IsPOdj(podjId)) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404', 'PODJ neexistuje', 'PODJ na blok '+IntToStr(podjId)+' neexistuje');
   Exit();
  end;

 train.GetPOdj(podjId).GetPtData(respJson.O['podj']);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var dict: TDictionary<string, Integer>;
begin
 dict := Self.CommonGetPut(AContext, ARequestInfo, respJson);
 try
   if (dict['type'] = _ET_TRAIN) then
     Self.OnPUTTrain(AContext, ARequestInfo, respJson, reqJson, Trains[dict['train']])
   else if (dict['type'] = _ET_PODJ) then
     Self.OnPUTPodj(AContext, ARequestInfo, respJson, reqJson, Trains[dict['train']], dict['podj']);
 finally
   dict.Free();
 end;
end;

procedure TPTEndpointTrain.OnPUTTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
begin
 if (not reqJson.Contains('train')) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Chybi json sekce train');
   Exit();
  end;

 train.PutPtData(reqJson.O['train'], respJson);
end;

procedure TPTEndpointTrain.OnPUTPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain; podjId: Integer);
var podj: TPodj;
    blk: TBlk;
begin
 if (not reqJson.Contains('podj')) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Chybi json sekce podj');
   Exit();
  end;

 podj := nil;
 try
   try
     podj := TPodj.Create(reqJson.O['podj']);
     Blocks.GetBlkByID(podjId, blk);
     if ((blk = nil) or (blk.typ <> TBlkType.btTrack)) then
      begin
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Neplatny typ bloku', 'Blok neexistuje nebo neni usek');
       Exit();
      end;

     TBlkTrack(blk).POdjChanged(train.index, podj); // sets podj to nil if takes ownership
     if (train.IsPOdj(podjId)) then
       train.GetPOdj(podjId).GetPtData(respJson.O['podj'])
     else
       respJson.O['podj']; // empty podj
   except
     on E:Exception do
       PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '500', 'Vnitrni chyba serveru', E.Message);
   end;
 finally
   if (podj <> nil) then
     podj.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var train: TTrain;
begin
 if (not reqJson.Contains('train')) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Chybi json sekce train');
   Exit();
  end;

 train := Trains.Add(reqJson['train'], TTrakce.Callback(), TTrakce.Callback());
 train.GetPtData(respJson.O['train']);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
var dict: TDictionary<string, Integer>;
begin
 dict := Self.CommonGetPut(AContext, ARequestInfo, respJson);
 try
   if (dict['type'] = _ET_TRAIN) then
     Trains.Remove(Trains[dict['train']].index)
   else
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '405', 'Method not allowed', 'S touto HTTP metodou si neumim poradit');
 finally
   dict.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrain.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

end.
