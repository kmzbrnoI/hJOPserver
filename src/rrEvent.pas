unit rrEvent;

{
  Trida TRREv definuje udalost na kolejisti, na kterou je mozne reagovat.

  Typy udalosti:
  - obsazeni/uvolneni casti useku
  - zmena stavu IR cidla
  - uplynuti casu
  - ujeti vzdalenosti (od obsazeni casti useku)

  Definicni string udalosti:
  - usek: "typ,state,part[,track]"
  - IR: "typ,state,irid[,track]"
  - cas: "typ,timeSec[,track]"
  - vzdalenost: "typ,distCm[,track]"

  Libovolna 'part' = "*" (v definicnim stringu)
}

interface

uses Classes, SysUtils, StrUtils;

type
  TRREvType = (rrtTrack = 1, rrtIR = 2, rrtTime = 3, rrtDist = 4);

  TRREvData = record
    trackId: Integer;

    case typ: TRREvType of
      rrtTrack:
        (trackState: Boolean;
         trackPart: Integer; // -1 = any part
        );

      rrtIR:
        (irId: Cardinal;
          irState: Boolean;
        );

      rrtTime:
        (time: TTime;
        );

      rrtDist:
        (distanceCm: Cardinal;
        );
  end;

  TRREvState = record
    triggerTime: TDateTime;
    triggedTrainI: Integer;
    registerDist: Real;
    triggerDist: Real;
    enabled: Boolean;
  end;

  TRREv = class
  const
    _RR_ANY_TRACK: Integer = -1;

  private
    m_data: TRREvData;
    m_state: TRREvState;
    m_trackAllowed: Boolean;

    procedure LoadFromDefStr(data: string);

    class function TrackPartFromFile(part: string): Integer;
    class function TrackPartToFile(trackPart: Integer): string;

  public

    constructor Create(trackAllowed: Boolean; data: string); overload;
    constructor Create(trackAllowed: Boolean; data: TRREvData); overload;

    function GetDefStr(): string;

    procedure Register(traini: Cardinal);
    procedure Unregister();

    // Sender must be a valid "Usek" blok.
    function IsTriggerred(Sender: TObject; safeState: Boolean): Boolean;
    function Track(Sender: TObject): TObject;

    property enabled: Boolean read m_state.enabled;
    property data: TRREvData read m_data;
    property typ: TRREvType read m_data.typ;
    property trackAllowed: Boolean read m_trackAllowed;
    property trackId: Integer read m_data.trackId;

  end;

implementation

uses BlockDb, Block, BlockIR, BlockTrack, ownConvert, ownStrUtils, IfThenElse,
  TrainDb, Train, timeHelper;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRREv.Create(trackAllowed: Boolean; data: string);
begin
  inherited Create();
  Self.m_trackAllowed := trackAllowed;
  Self.m_state.enabled := false;
  Self.LoadFromDefStr(data);
end;

constructor TRREv.Create(trackAllowed: Boolean; data: TRREvData);
begin
  inherited Create();
  Self.m_trackAllowed := trackAllowed;
  Self.m_state.enabled := false;
  Self.m_data := data;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRREv.LoadFromDefStr(data: string);
var strs: TStrings;
begin
  strs := TStringList.Create();

  try
    ExtractStringsEx([';', ','], [], data, strs);

    Self.m_data.typ := TRREvType(StrToInt(strs[0]));

    case (Self.m_data.typ) of
      rrtTrack:
        begin
          Self.m_data.trackState := ownConvert.StrToBool(strs[1]);
          Self.m_data.trackPart := Self.TrackPartFromFile(strs[2]);
          if (strs.Count > 3) then
            Self.m_data.trackId := StrToInt(strs[3])
          else
            Self.m_data.trackId := -1;
        end;

      rrtIR:
        begin
          Self.m_data.irState := ownConvert.StrToBool(strs[1]);
          Self.m_data.irId := StrToInt(strs[2]);
          if (strs.Count > 3) then
            Self.m_data.trackId := StrToInt(strs[3])
          else
            Self.m_data.trackId := -1;
        end;

      rrtTime:
        begin
          if (StrToIntDef(strs[1], -1) <> -1) then
          begin
            var tmpTime: Integer := StrToInt(strs[1]);
            Self.m_data.time := EncodeTimeSec(tmpTime);
          end else begin
            Self.m_data.time := EncodeTime(0, StrToInt(LeftStr(strs[1], 2)), StrToInt(Copy(strs[1], 4, 2)),
              StrToInt(RightStr(strs[1], 1)));
          end;

          if (strs.Count > 2) then
            Self.m_data.trackId := StrToInt(strs[2])
          else
            Self.m_data.trackId := -1;
        end;

      rrtDist:
        begin
          Self.m_data.distanceCm := StrToInt(strs[1]);
          Self.m_data.trackPart := Self.TrackPartFromFile(strs[2]);
          if (strs.Count > 3) then
            Self.m_data.trackId := StrToInt(strs[3])
          else
            Self.m_data.trackId := -1;
        end;
    end; // m_data.typ
  finally
    strs.Free();
  end;
end;

function TRREv.GetDefStr(): string;
begin
  Result := IntToStr(Integer(Self.m_data.typ)) + ',';

  case (Self.m_data.typ) of
    rrtTrack:
      Result := Result + IntToStr(ownConvert.BoolToInt(Self.m_data.trackState)) + ',' + Self.TrackPartToFile(Self.m_data.trackPart);

    rrtIR:
      Result := Result + IntToStr(ownConvert.BoolToInt(Self.m_data.irState)) + ',' + IntToStr(Self.m_data.irId);

    rrtTime:
      Result := Result + FormatDateTime('nn:ss.z', Self.m_data.time);

    rrtDist:
      Result := Result + IntToStr(Self.m_data.distanceCm) + ',' + Self.TrackPartToFile(Self.m_data.trackPart);
  end;

  if ((Self.trackAllowed) and (Self.m_data.trackId > -1)) then
    Result := Result + ',' + IntToStr(Self.m_data.trackId);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRREv.Register(traini: Cardinal);
begin
  Self.m_state.triggedTrainI := traini;

  if (Self.m_data.typ = rrtTime) then
    Self.m_state.triggerTime := Now + m_data.time
  else if (Self.m_data.typ = rrtDist) then
  begin
    if (trains.Exists(traini)) then
    begin
      Self.m_state.registerDist := trains[traini].traveled;
      Self.m_state.triggerDist := trains[traini].traveled + (Self.m_data.distanceCm / 100)
    end else begin
      Self.m_state.registerDist := 0;
      Self.m_state.triggerDist := 0;
    end;
  end;

  Self.m_state.enabled := true;
end;

procedure TRREv.Unregister();
begin
  Self.m_state.enabled := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TrackPartState(track: TBlkTrack; trackPart: Integer): TTrackState;
begin
  if (trackPart = TRREv._RR_ANY_TRACK) then
  begin
    Result := track.occupied;
  end else begin
    if ((trackPart >= 0) and (trackPart < track.sectionsState.Count)) then
      Result := track.sectionsState[trackPart]
    else
      Result := TTrackState.none;
  end;
end;

function TRREv.IsTriggerred(Sender: TObject; safeState: Boolean): Boolean;
begin
  if (not Self.enabled) then
    Exit(false);

  try
    case (Self.m_data.typ) of
      rrtTrack:
        begin
          var track: TBlkTrack := nil;
          if (Self.trackAllowed) then
            track := Blocks.GetBlkTrackOrRTByID(Self.m_data.trackId);
          if (track = nil) then
            track := TBlkTrack(Sender);

          if (track = nil) then
          begin
            Result := safeState;
            Exit();
          end;

          case (TrackPartState(track, Self.m_data.trackPart)) of
            TTrackState.occupied:
              Result := Self.m_data.trackState;
            TTrackState.Free:
              Result := not Self.m_data.trackState;
          else
            Result := safeState;
          end;
        end;

      rrtIR:
        begin
          var ir: TBlkIR := Blocks.GetBlkIrByID(Self.m_data.irId);
          if (ir = nil) then
            Exit(safeState);
          case (ir.occupied) of
            TIROccupationState.occupied:
              Result := Self.m_data.irState;
            TIROccupationState.Free:
              Result := not Self.m_data.irState;
          else
            Result := safeState;
          end;
        end;

      rrtTime:
        begin
          Result := (Now >= Self.m_state.triggerTime);
        end;

      rrtDist:
        begin
          if (not trains.Exists(Self.m_state.triggedTrainI)) then
            Exit(safeState);

          var train: TTrain := trains[Self.m_state.triggedTrainI];
          Result := ((train.traveled >= Self.m_state.triggerDist) or (train.traveled < Self.m_state.registerDist));
        end
    else
      Result := safeState;
    end;

  except
    Result := safeState;
  end;
end;

function TRREv.Track(Sender: TObject): TObject;
begin
  if ((Self.trackAllowed) and (Self.m_data.trackId > -1)) then
    Result := Blocks.GetBlkTrackOrRTByID(Self.m_data.trackId)
  else
    Result := Sender;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRREv.TrackPartFromFile(part: string): Integer;
begin
  if (part = '*') then
    Result := _RR_ANY_TRACK
  else
    Result := StrToInt(part);
end;

class function TRREv.TrackPartToFile(trackPart: Integer): string;
begin
  if (trackPart = _RR_ANY_TRACK) then
    Result := '*'
  else
    Result := IntToStr(trackPart);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
