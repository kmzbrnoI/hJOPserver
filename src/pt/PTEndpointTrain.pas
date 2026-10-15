unit PTEndpointTrain;

{ PTserver endpoint /train/id. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections, Train, RegularExpressions;


type
  TPTEndpointTrain = class(TPTEndpoint)
  private const
    _ENDPOINT_MATCH_REGEX = '^/trains/.+?/?';
    _ET_NONE = 0;
    _ET_TRAIN = 1;
    _ET_PODJ = 2;

  private

    procedure OnGETTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; train: TTrain);
    procedure OnPUTTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
    procedure OnGETPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; train: TTrain);
    procedure OnPUTPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
    procedure OnPUTStop(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
    procedure OnPUTUnstop(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);

  public
    procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject); override;
    procedure OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject); override;
    procedure OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject); override;
    procedure OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; var respJson:TJsonObject; const reqJson:TJsonObject); override;

    function EndpointMatch(path:string):Boolean; override;

  end;

implementation

uses PTUtils, TrainDb, StrUtils, ownStrUtils, predvidanyOdjezd, BlockDb,
      Block, BlockTrack, TrakceC;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
begin
  var trainName: string := strTillChar(RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/trains/')), '/');
  var trainIndex: Integer := Trains.GetTrainIndexByName(trainName);
  if (trainIndex = -1) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'Vlak neexistuje', 'Vlak cisla '+trainName+' neexistuje');
    Exit();
  end;

  if (TRegEx.Match(ARequestInfo.Document, '^/trains/.+?/podj/(\d+)/?$').Success) then
    Self.OnGETPodj(AContext, ARequestInfo, respJson, Trains[trainIndex])
  else
    Self.OnGETTrain(AContext, ARequestInfo, respJson, Trains[trainIndex]);
end;

procedure TPTEndpointTrain.OnGETTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; train: TTrain);
begin
  train.GetPtData(respJson.O['train']);
end;

procedure TPTEndpointTrain.OnGETPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; train: TTrain);
begin
  var match := TRegEx.Match(ARequestInfo.Document, 'podj/(\d+)/?$');
  if (not match.Success) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'PODJ neplatné id bloku', 'Neplatné ID bloku PODJ v URL');
    Exit();
  end;

  var podjId := StrToInt(match.Groups[1].Value);
  if (not train.IsPOdj(podjId)) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'PODJ neexistuje', 'PODJ na blok '+IntToStr(podjId)+' neexistuje');
    Exit();
  end;

  train.GetPOdj(podjId).GetPtData(respJson.O['podj']);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
begin
  var trainName: string := strTillChar(RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/trains/')), '/');
  var trainIndex: Integer := Trains.GetTrainIndexByName(trainName);
  if (trainIndex = -1) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'Vlak neexistuje', 'Vlak cisla '+trainName+' neexistuje');
    Exit();
  end;

  if (TRegEx.Match(ARequestInfo.Document, '^/trains/.+?/podj/(\d+)/?$').Success) then
    Self.OnPUTPodj(AContext, ARequestInfo, respJson, reqJson, Trains[trainIndex])
  else if (TRegEx.Match(ARequestInfo.Document, '^/trains/.+?/stop/?$').Success) then
    Self.OnPUTStop(AContext, ARequestInfo, respJson, reqJson, Trains[trainIndex])
  else if (TRegEx.Match(ARequestInfo.Document, '^/trains/.+?/unstop/?$').Success) then
    Self.OnPUTUnstop(AContext, ARequestInfo, respJson, reqJson, Trains[trainIndex])
  else
    Self.OnPUTTrain(AContext, ARequestInfo, respJson, reqJson, Trains[trainIndex]);
end;

procedure TPTEndpointTrain.OnPUTTrain(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
begin
  if (not reqJson.Contains('train')) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Chybi json sekce train');
    Exit();
  end;

  train.PutPtData(reqJson.O['train'], respJson);
end;

procedure TPTEndpointTrain.OnPUTPodj(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
begin
  var match := TRegEx.Match(ARequestInfo.Document, 'podj/(\d+)/?$');
  if (not match.Success) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'PODJ neplatné id bloku', 'Neplatné ID bloku PODJ v URL');
    Exit();
  end;

  var podjId := StrToInt(match.Groups[1].Value);

  if (not reqJson.Contains('podj')) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Chybi json sekce podj');
    Exit();
  end;

  var podj: TPodj := nil;
  try
    try
      podj := TPodj.Create(reqJson.O['podj']);
      var blk: TBlkTrack := Blocks.GetBlkTrackOrRTByID(podjId);
      if ((blk = nil) or (blk.typ <> TBlkType.btTrack)) then
      begin
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Neplatny typ bloku', 'Blok neexistuje nebo neni usek');
        Exit();
      end;

      TBlkTrack(blk).POdjChanged(train.index, podj); // sets podj to nil if takes ownership
      if (train.IsPOdj(podjId)) then
        train.GetPOdj(podjId).GetPtData(respJson.O['podj'])
      else
        respJson.O['podj']; // empty podj
    except
      on E:Exception do
        PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 500, 'Vnitrni chyba serveru', E.Message);
    end;
  finally
    if (podj <> nil) then
      podj.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
begin
  if (not reqJson.Contains('train')) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Chybi json sekce train');
    Exit();
  end;

  const urlSuffix: string = RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/trains/'));
  const trainName: string = strTillChar(urlSuffix, '/');

  reqJson['train']['name'] := trainName;
  var train: TTrain := Trains.Add(reqJson['train'], TTrakce.Callback(), TTrakce.Callback());
  train.GetPtData(respJson.O['train']);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject);
begin
  var trainName: string := strTillChar(RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/trains/')), '/');
  var trainIndex: Integer := Trains.GetTrainIndexByName(trainName);
  if (trainIndex = -1) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 404, 'Vlak neexistuje', 'Vlak cisla '+trainName+' neexistuje');
    Exit();
  end;

  Trains.Remove(Trains[trainIndex].index);
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointTrain.EndpointMatch(path:string):Boolean;
begin
  Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointTrain.OnPUTStop(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
begin
  train.EmergencyStop();
  train.GetPtData(respJson.O['train']);
end;

procedure TPTEndpointTrain.OnPUTUnstop(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  var respJson:TJsonObject; const reqJson:TJsonObject; train: TTrain);
begin
  if (not train.emergencyStopped) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 400, 'Vlak nezastaven', 'Vlak neni nouzove zastaven, nelze odzastavit');
    Exit();
  end;

  train.EmergencyStopRelease();
  train.GetPtData(respJson.O['train']);
end;

////////////////////////////////////////////////////////////////////////////////

end.
