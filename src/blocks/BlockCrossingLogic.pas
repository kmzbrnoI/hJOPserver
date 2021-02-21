unit BlockCrossingLogic;

interface

uses Generics.Collections, BlockTrackRefs, IniFiles, Classes, SysUtils, StrUtils;

type
  TBlkCrossingTrackState = (
    tsFree = 0,
    tsException = 1,
    tsUnexpectedOccupation = 2,
    tsLRLeftOccupied,
    tsLRLeftMidOccupied,
    tsLRMidOccupied,
    tsLROnlyRightOccupied,
    tsLRRightOutOccupied,
    tsRLRightOccupied,
    tsRLRightMidOccupied,
    tsRLMidOccupied,
    tsRLOnlyLeftOccupied,
    tsRLLeftOutOccupied
  );

  TBlkCrossingTrackOpening = (
    toMiddleFree = 0,
    toLastOccupied = 1,
    toOutFree = 2
  );

  TBlkCrossingTrack = class
  const
    _SECT_COUNT = 5;
    _SECT_LEFT_OUT = 0;
    _SECT_LEFT = 1;
    _SECT_MID = 2;
    _SECT_RIGHT = 3;
    _SECT_RIGHT_OUT = 4;

  private
    mState: TBlkCrossingTrackState;
    anulEnd: TTime;

    function mShouldBeClosed(): Boolean;
    function mPositiveLight(): Boolean;
    function AllFree(): Boolean;
    function GetAnullation(): Boolean;

    procedure UpdateState();
    procedure SetState(new: TBlkCrossingTrackState);

  public
    sections: array [0 .. 4] of TBlkTrackRefs;
    opening: TBlkCrossingTrackOpening;
    anulTime: TTime;

    onChanged: TNotifyEvent;
    stateChanged: Boolean;

    constructor Create();
    destructor Destroy(); override;

    procedure Load(ini: TMemIniFile; section: string; prefix: string);
    procedure Save(ini: TMemIniFile; section: string; prefix: string);

    procedure Update();

    property state: TBlkCrossingTrackState read mState write SetState;
    property shouldBeClosed: Boolean read mShouldBeClosed;
    property positiveLight: Boolean read mPositiveLight;
    property anullation: Boolean read GetAnullation;

    property leftOut: TBlkTrackRefs read sections[_SECT_LEFT_OUT] write sections[_SECT_LEFT_OUT];
    property left: TBlkTrackRefs read sections[_SECT_LEFT] write sections[_SECT_LEFT];
    property middle: TBlkTrackRefs read sections[_SECT_MID] write sections[_SECT_MID];
    property right: TBlkTrackRefs read sections[_SECT_RIGHT] write sections[_SECT_RIGHT];
    property rightOut: TBlkTrackRefs read sections[_SECT_RIGHT_OUT] write sections[_SECT_RIGHT_OUT];

  end;

implementation

uses BlockTrack;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkCrossingTrack.Create();
var i: Integer;
begin
  inherited;
  Self.onChanged := nil;
  Self.stateChanged := false;
  for i := 0 to _SECT_COUNT - 1 do
    Self.sections[i] := TBlkTrackRefs.Create();
end;

destructor TBlkCrossingTrack.Destroy();
var i: Integer;
begin
  for i := 0 to _SECT_COUNT - 1 do
    Self.sections[i].Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossingTrack.Load(ini: TMemIniFile; section: string; prefix: string);
var str: string;
begin
  Self.leftOut.Parse(ini.ReadString(section, prefix + '-leftOut', ''));
  Self.left.Parse(ini.ReadString(section, prefix + '-left', ''));
  Self.middle.Parse(ini.ReadString(section, prefix + '-middle', ''));
  Self.right.Parse(ini.ReadString(section, prefix + '-right', ''));
  Self.rightOut.Parse(ini.ReadString(section, prefix + '-rightOut', ''));
  Self.opening := TBlkCrossingTrackOpening(ini.ReadInteger(section, prefix + '-opening', 0));

  str := ini.ReadString(section, prefix + '-anulTime', '01:00');
  Self.anulTime := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
end;

procedure TBlkCrossingTrack.Save(ini: TMemIniFile; section: string; prefix: string);
begin
  if (Self.leftOut.ToStr() <> '') then
    ini.WriteString(section, prefix + '-leftOut', Self.leftOut.ToStr());
  if (Self.left.ToStr() <> '') then
    ini.WriteString(section, prefix + '-left', Self.left.ToStr());
  if (Self.middle.ToStr() <> '') then
    ini.WriteString(section, prefix + '-middle', Self.middle.ToStr());
  if (Self.right.ToStr() <> '') then
    ini.WriteString(section, prefix + '-right', Self.right.ToStr());
  if (Self.rightOut.ToStr() <> '') then
    ini.WriteString(section, prefix + '-rightOut', Self.rightOut.ToStr());
  ini.WriteInteger(section, prefix + '-opening', Integer(Self.opening));
  ini.WriteString(section, prefix + '-anulTime', FormatDateTime('nn:ss', Self.anulTime));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossingTrack.SetState(new: TBlkCrossingTrackState);
begin
  if (new <> Self.mState) then
  begin
    Self.mState := new;
    Self.stateChanged := true;
    if (Assigned(Self.onChanged)) then
      Self.onChanged(Self);
  end;
end;

procedure TBlkCrossingTrack.Update();
begin
  try
    Self.UpdateState();
  except
    Self.state := tsException;
  end;
end;

procedure TBlkCrossingTrack.UpdateState();
begin
  if (Self.AllFree()) then
  begin
    if (Self.state <> tsFree) then
      Self.state := tsFree;
    Exit();
  end;

  case (Self.state) of
    tsFree:
      begin
        if ((Self.left.state = TTrackState.occupied) and (Self.middle.state = TTrackState.Free) and
          (Self.right.state = TTrackState.Free)) then
          Self.state := tsLRLeftOccupied
        else if ((Self.right.state = TTrackState.occupied) and (Self.middle.state = TTrackState.Free) and
          (Self.left.state = TTrackState.Free)) then
          Self.state := tsRLRightOccupied
        else if ((Self.left.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free) or
          (Self.right.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation;
      end;

    tsUnexpectedOccupation:
      begin
        if ((Self.left.state = TTrackState.Free) and (Self.middle.state = TTrackState.Free) and
          (Self.right.state = TTrackState.Free)) then
          Self.state := tsFree;
      end;

    tsLRLeftOccupied:
      begin
        if ((Self.left.state <> TTrackState.occupied) or (Self.right.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.occupied) then
          Self.state := tsLRLeftMidOccupied
      end;

    tsRLRightOccupied:
      begin
        if ((Self.right.state <> TTrackState.occupied) or (Self.left.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.occupied) then
          Self.state := tsRLRightMidOccupied
      end;

    tsLRLeftMidOccupied:
      begin
        if (Self.middle.state <> TTrackState.occupied) then
          Self.state := tsUnexpectedOccupation
        else if (Self.left.state = TTrackState.Free) then
          Self.state := tsLRMidOccupied
        else if ((Self.right.changed) and (Self.opening = toLastOccupied)) then
          Self.stateChanged := true;
      end;

    tsRLRightMidOccupied:
      begin
        if (Self.middle.state <> TTrackState.occupied) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.Free) then
          Self.state := tsRLMidOccupied
        else if ((Self.left.changed) and (Self.opening = toLastOccupied)) then
          Self.stateChanged := true;
      end;

    tsLRMidOccupied:
      begin
        if (Self.left.state <> TTrackState.Free) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.Free) then
        begin
          if (Self.right.state = TTrackState.occupied) then
          begin
            Self.anulEnd := Now + Self.anulTime;
            Self.state := tsLROnlyRightOccupied;
          end
          else
            Self.state := tsFree;
        end else if ((Self.right.changed) and (Self.opening = toLastOccupied)) then
          Self.stateChanged := true;
      end;

    tsRLMidOccupied:
      begin
        if (Self.right.state <> TTrackState.Free) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.Free) then
        begin
          if (Self.left.state = TTrackState.occupied) then
          begin
            Self.anulEnd := Now + Self.anulTime;
            Self.state := tsRLOnlyLeftOccupied
          end
          else
            Self.state := tsFree;
        end else if ((Self.left.changed) and (Self.opening = toLastOccupied)) then
          Self.stateChanged := true;
      end;

    tsLROnlyRightOccupied:
      begin
        if ((Self.left.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.Free) then
        begin
          if (Self.rightOut.state = TTrackState.occupied) then
            Self.state := tsLRRightOutOccupied
          else
            Self.state := tsFree;
        end else if ((Now > Self.anulEnd) and (Self.opening = toMiddleFree)) then
          Self.state := tsRLRightOccupied;
      end;

    tsRLOnlyLeftOccupied:
      begin
        if ((Self.right.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.left.state = TTrackState.Free) then
        begin
          if (Self.leftOut.state = TTrackState.occupied) then
            Self.state := tsRLLeftOutOccupied
          else
            Self.state := tsFree;
        end else if ((Now > Self.anulEnd) and (Self.opening = toMiddleFree)) then
          Self.state := tsLRLeftOccupied;
      end;

    tsLRRightOutOccupied:
      begin
        if ((Self.left.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.occupied) then
          Self.state := tsRLRightOccupied
      end;

    tsRLLeftOutOccupied:
      begin
        if ((Self.right.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.left.state = TTrackState.occupied) then
          Self.state := tsLRLeftOccupied
      end;

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossingTrack.mShouldBeClosed(): Boolean;
begin
  if (Self.state = tsUnexpectedOccupation) then
    Exit((Self.left.state <> TTrackState.Free) or (Self.middle.state <> TTrackState.Free) or
      (Self.right.state <> TTrackState.Free));

  case (Self.opening) of
    toMiddleFree:
      begin
        Result := (Self.state = tsLRLeftOccupied) or (Self.state = tsRLRightOccupied) or
          (Self.state = tsLRLeftMidOccupied) or (Self.state = tsRLRightMidOccupied) or (Self.state = tsLRMidOccupied) or
          (Self.state = tsRLMidOccupied);
      end;

    toLastOccupied:
      begin
        Result := (Self.state = tsLRLeftOccupied) or (Self.state = tsRLRightOccupied) or
          (((Self.state = tsLRLeftMidOccupied) or (Self.state = tsLRMidOccupied)) and
          (Self.right.state <> TTrackState.occupied)) or
          (((Self.state = tsRLRightMidOccupied) or (Self.state = tsRLMidOccupied)) and
          (Self.left.state <> TTrackState.occupied));
      end;

    toOutFree:
      begin
        Result := (Self.state >= tsLRLeftOccupied);
      end;
  else
    Result := true;
  end;
end;

function TBlkCrossingTrack.mPositiveLight(): Boolean;
begin
  Result := (Self.left.state = TTrackState.Free) and (Self.middle.state = TTrackState.Free) and
    (Self.right.state = TTrackState.Free);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossingTrack.AllFree(): Boolean;
begin
  for var i: Integer := 0 to _SECT_COUNT - 1 do
  begin
    try
      if (Self.sections[i].state <> TTrackState.Free) then
        Exit(false);
    except
      Exit(false);
    end;
  end;
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossingTrack.GetAnullation(): Boolean;
begin
  Result := (((Self.state = tsRLOnlyLeftOccupied) or (Self.state = tsLROnlyRightOccupied)) and
    (Self.opening = toMiddleFree));
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
