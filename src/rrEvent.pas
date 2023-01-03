unit rrEvent;

{
  Trida TRREv definuje udalost na kolejisti, na kterou je mozne reagovat.

  Typy udalosti:
  - obsazeni/uvolneni casti useku
  - zmena stavu IR cidla
  - uplynuti urcite doby

  Definicni string udalosti:
  - usek: "typ,state,part[,track]"
  - IR: "typ,state,irid[,track]"
  - cas: "typ,timeSec[,track]"
}

interface

uses Classes, SysUtils, StrUtils;

type
  TRREvType = (rrtTrack = 1, rrtIR = 2, rrtTime = 3);

  TRREvData = record
    trackId: Integer;

    case typ: TRREvType of
      rrtTrack:
        (trackPart: Cardinal;
          trackState: Boolean;
        );

      rrtIR:
        (irId: Cardinal;
          irState: Boolean;
        );

      rrtTime:
        (time: TTime;
        );
  end;

  TRREvState = record
    triggerTime: TDateTime;
    enabled: Boolean;
  end;

  TRREv = class
  private
    m_data: TRREvData;
    m_state: TRREvState;
    m_trackAllowed: Boolean;

    procedure LoadFromDefStr(data: string);

  public

    constructor Create(trackAllowed: Boolean; data: string); overload;
    constructor Create(trackAllowed: Boolean; data: TRREvData); overload;

    function GetDefStr(): string;

    procedure Register();
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

uses BlockDb, Block, BlockIR, BlockTrack, ownConvert, ownStrUtils, IfThenElse;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRREv.Create(trackAllowed: Boolean; data: string);
begin
  inherited Create();
  Self.m_trackAllowed := trackAllowed;
  Self.m_state.enabled := false;
  LoadFromDefStr(data);
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
  tmpTime: Cardinal;
begin
  strs := TStringList.Create();

  try
    ExtractStringsEx([';', ','], [], data, strs);

    Self.m_data.typ := TRREvType(StrToInt(strs[0]));

    case (Self.m_data.typ) of
      rrtTrack:
        begin
          Self.m_data.trackState := ownConvert.StrToBool(strs[1]);
          Self.m_data.trackPart := StrToInt(strs[2]);
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
            tmpTime := StrToInt(strs[1]);
            Self.m_data.time := EncodeTime(0, tmpTime div 60, tmpTime mod 60, 0);
          end else begin
            Self.m_data.time := EncodeTime(0, StrToInt(LeftStr(strs[1], 2)), StrToInt(Copy(strs[1], 4, 2)),
              StrToInt(RightStr(strs[1], 1)));
          end;

          if (strs.Count > 2) then
            Self.m_data.trackId := StrToInt(strs[2])
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
      Result := Result + IntToStr(ownConvert.BoolToInt(m_data.trackState)) + ',' + IntToStr(m_data.trackPart);

    rrtIR:
      Result := Result + IntToStr(ownConvert.BoolToInt(m_data.irState)) + ',' + IntToStr(m_data.irId);

    rrtTime:
      Result := Result + FormatDateTime('nn:ss.z', m_data.time);
  end;

  if ((Self.trackAllowed) and (Self.m_data.trackId > -1)) then
    Result := Result + ',' + IntToStr(Self.m_data.trackId);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRREv.Register();
begin
  if (Self.m_data.typ = rrtTime) then
    Self.m_state.triggerTime := Now + m_data.time;

  Self.m_state.enabled := true;
end;

procedure TRREv.Unregister();
begin
  Self.m_state.enabled := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

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

          if (Integer(m_data.trackPart) < track.sectionsState.Count) then
          begin
            case (track.sectionsState[m_data.trackPart]) of
              TTrackState.occupied:
                Result := m_data.trackState;
              TTrackState.Free:
                Result := not m_data.trackState;
            else
              Result := safeState;
            end;
          end
          else
            Result := safeState;
        end;

      rrtIR:
        begin
          var ir: TBlkIR := Blocks.GetBlkIrByID(m_data.irId);
          if (ir = nil) then
            Exit(safeState);
          case (ir.occupied) of
            TIROccupationState.occupied:
              Result := m_data.irState;
            TIROccupationState.Free:
              Result := not m_data.irState;
          else
            Result := safeState;
          end;
        end;

      rrtTime:
        begin
          Result := (Now >= Self.m_state.triggerTime);
        end;
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

end.
