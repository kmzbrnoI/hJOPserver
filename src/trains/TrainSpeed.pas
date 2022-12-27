unit TrainSpeed;

interface

uses Train, SysUtils, StrUtils, Classes, Generics.Collections, JsonDataObjects;

type
  EInvalidData = class(Exception);

  TTrainSpeed = class
  public
   speed: Cardinal;
   trainTypeRe: string;
   hvTransienceRe: string;

    constructor Create(speed: Cardinal; trainTypeRe: string; hvTransienceRe: string); overload;
    constructor Create(fileEntry: string); overload;

    procedure LoadFromFileStr(entry: string);
    function FileStr(): string;
    function IsDefault(): Boolean;

    function Match(train: TTrain): Boolean;
    procedure GetPtData(json: TJsonObject); overload;

    class function DefaultSpeed(speeds: TList<TTrainSpeed>; var speed: Cardinal): Boolean;
    class function DefaultSpeedStr(speeds: TList<TTrainSpeed>): string;
    class function Pick(train: TTrain; speeds: TList<TTrainSpeed>; var speed: Cardinal): Boolean;
    class function IniLoad(line: string): TList<TTrainSpeed>;
    class function IniStr(speeds: TList<TTrainSpeed>): string;
    class procedure GetPtData(speeds: TList<TTrainSpeed>; json: TJsonArray); overload;

  end;

implementation

uses RegularExpressions, THVDatabase, ownStrUtils;

constructor TTrainSpeed.Create(speed: Cardinal; trainTypeRe: string; hvTransienceRe: string);
begin
  inherited Create();
  Self.speed := speed;
  Self.trainTypeRe := trainTypeRe;
  Self.hvTransienceRe := hvTransienceRe;
end;

constructor TTrainSpeed.Create(fileEntry: string);
begin
  inherited Create();
  Self.LoadFromFileStr(fileEntry);
end;

procedure TTrainSpeed.LoadFromFileStr(entry: string);
begin
  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([','], [], entry, strs);
    if (strs.Count < 3) then
      raise EInvalidData.Create('Insufficient number of TTrainSpeed data');

    Self.speed := StrToInt(strs[0]);
    Self.trainTypeRe := strs[1];
    Self.hvTransienceRe := strs[2];
  finally
    strs.Free();
  end;
end;

function TTrainSpeed.FileStr(): string;
begin
  Result := IntToStr(Self.speed) + ',{' + Self.trainTypeRe + '},{' + Self.hvTransienceRe + '}';
end;

function TTrainSpeed.Match(train: TTrain): Boolean;
begin
  // The 2nd part of the condition is because an empty string is not matched by '.*'
  if ((not TRegEx.IsMatch(train.typ, '^'+Self.trainTypeRe+'$')) and
      ((train.typ <> '') or (not TRegEx.IsMatch(' ', '^'+Self.trainTypeRe+'$')))) then
    Exit(false);

  if (train.HVs.Count = 0) then
    Exit(false);

  for var hvAddr in train.HVs do
    if ((HVDb[hvAddr] <> nil) and (not TRegEx.IsMatch(IntToStr(HVDb[hvAddr].data.transience), Self.hvTransienceRe))) then
      Exit(false);

  Result := true;
end;

function TTrainSpeed.IsDefault(): Boolean;
begin
  Result := ((Self.trainTypeRe = '.*') and (Self.hvTransienceRe = '.*'));
end;

procedure TTrainSpeed.GetPtData(json: TJsonObject);
begin
  json['speed'] := Self.speed;
  json['trainTypeRe'] := Self.trainTypeRe;
  json['hvTransienceRe'] := Self.hvTransienceRe;
end;

class function TTrainSpeed.DefaultSpeed(speeds: TList<TTrainSpeed>; var speed: Cardinal): boolean;
begin
  for var ts: TTrainSpeed in speeds do
  begin
    if (ts.IsDefault()) then
    begin
      speed := ts.speed;
      Exit(true);
    end;
  end;

  Result := false;
end;

class function TTrainSpeed.DefaultSpeedStr(speeds: TList<TTrainSpeed>): string;
begin
  var speed: Cardinal;
  if (TTrainSpeed.DefaultSpeed(speeds, speed)) then
  begin
    Result := IntToStr(speed) + 'km/h';
    if (speeds.Count > 1) then
      Result := Result + ' *';
  end else
    Result := '-';
end;

class function TTrainSpeed.Pick(train: TTrain; speeds: TList<TTrainSpeed>; var speed: Cardinal): Boolean;
begin
  for var ts: TTrainSpeed in speeds do
  begin
    if (ts.Match(train)) then
    begin
      speed := ts.speed;
      Exit(true);
    end;
  end;

  Result := false;
end;

class function TTrainSpeed.IniLoad(line: string): TList<TTrainSpeed>;
begin
  Result := TList<TTrainSpeed>.Create();

  if (line = '') then
    Exit();

  if (line.Contains(':')) then
  begin
    // backward-compatible mode for railway tracks
    var strs: TStrings := TStringList.Create();
    var strs2: TStrings := TStringList.Create();
    try
      try
        ExtractStringsEx([','], [], line, strs);
        for var str: string in strs do
        begin
          strs2.Clear();
          ExtractStringsEx([':'], [], str, strs2);
          if (strs2.Count = 2) then
            Result.Add(TTrainSpeed.Create(StrToInt(strs2[1]), '.*', strs2[0]));
        end;
      except
        for var ts: TTrainSpeed in Result do
          ts.Free();
        Result.Free();
        raise;
      end;
    finally
      strs.Free();
      strs2.Free();
    end;

    Exit();
  end;

  if (not line.Contains('(')) then
  begin
    // backward-compatible mode for paths: only single speed
    var speed := StrToInt(line);
    if (speed < 10) then
      speed := speed*10; //  backward compatibility
    Result.Add(TTrainSpeed.Create(speed, '.*', '.*'));
    Exit();
  end;

  var strs: TStrings := TStringList.Create();
  try
    try
      ExtractStringsEx(['(', ')'], [], line, strs);
      for var str in strs do
        if (str <> '') then
          Result.Add(TTrainSpeed.Create(str));
    except
      for var ts: TTrainSpeed in Result do
        ts.Free();
      Result.Free();
      raise;
    end;
  finally
    strs.Free();
  end;
end;

class function TTrainSpeed.IniStr(speeds: TList<TTrainSpeed>): string;
begin
  Result := '';
  for var speed: TTrainSpeed in speeds do
    Result := Result + '(' + speed.FileStr() + ')';
end;

class procedure TTrainSpeed.GetPtData(speeds: TList<TTrainSpeed>; json: TJsonArray);
begin
  for var speed: TTrainSpeed in speeds do
    speed.GetPtData(json.AddObject);
end;

end.
