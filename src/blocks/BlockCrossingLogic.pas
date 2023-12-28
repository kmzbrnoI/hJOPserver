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
    toOutFree = 1
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
    openingLR, openingRL: TBlkCrossingTrackOpening;
    infiniteAnul: Boolean;
    anulTime: TTime;

    onChanged: TNotifyEvent;
    stateChanged: Boolean;

    constructor Create();
    destructor Destroy(); override;

    procedure Load(ini: TMemIniFile; section: string; prefix: string);
    procedure Save(ini: TMemIniFile; section: string; prefix: string);

    procedure Update();
    procedure AddOccupiedLMRTracksIds(var occupied: TList<Integer>);

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

  begin
    var opening: Integer := ini.ReadInteger(section, prefix + '-openingLR', -1);
    if (opening = -1) then
      opening := ini.ReadInteger(section, prefix + '-opening', 0); // backward-compatibility
    if (opening > 1) then // backward-compatibility
      opening := 1;
    Self.openingLR := TBlkCrossingTrackOpening(opening);
  end;

  begin
    var opening: Integer := ini.ReadInteger(section, prefix + '-openingRL', 0);
    if (opening > 1) then // backward-compatibility
      opening := 1;
    Self.openingRL := TBlkCrossingTrackOpening(opening);
  end;

  str := ini.ReadString(section, prefix + '-anulTime', '');
  Self.infiniteAnul := (str = 'inf');
  if (Self.infiniteAnul) then
    Self.anulTime := EncodeTime(0, 0, 0, 0)
  else
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
  ini.WriteInteger(section, prefix + '-openingLR', Integer(Self.openingLR));
  ini.WriteInteger(section, prefix + '-openingRL', Integer(Self.openingRL));

  if (Self.infiniteAnul) then
    ini.WriteString(section, prefix + '-anulTime', 'inf')
  else
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
        if ((Self.left.state = TTrackState.occupied) and (Self.middle.state = TTrackState.free) and
          (Self.right.state = TTrackState.free)) then
          Self.state := tsLRLeftOccupied
        else if ((Self.right.state = TTrackState.occupied) and (Self.middle.state = TTrackState.free) and
          (Self.left.state = TTrackState.free)) then
          Self.state := tsRLRightOccupied
        else if ((Self.leftOut.state = TTrackState.free) and (Self.middle.state = TTrackState.occupied) and
          (Self.right.parts.Count = 0)) then
          Self.state := tsRLMidOccupied
        else if ((Self.leftOut.parts.Count = 0) and (Self.middle.state = TTrackState.occupied) and
          (Self.right.state = TTrackState.free)) then
          Self.state := tsLRMidOccupied
        else if ((Self.left.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free) or
          (Self.right.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation;
      end;

    tsUnexpectedOccupation:
      begin
        if ((Self.left.state = TTrackState.free) and (Self.middle.state = TTrackState.free) and
          (Self.right.state = TTrackState.free)) then
          Self.state := tsFree;
      end;

    tsLRLeftOccupied:
      begin
        if ((Self.left.state <> TTrackState.occupied) or (Self.right.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.occupied) then
          Self.state := tsLRLeftMidOccupied
      end;

    tsRLRightOccupied:
      begin
        if ((Self.right.state <> TTrackState.occupied) or (Self.left.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.occupied) then
          Self.state := tsRLRightMidOccupied
      end;

    tsLRLeftMidOccupied:
      begin
        if (Self.middle.state <> TTrackState.occupied) then
          Self.state := tsUnexpectedOccupation
        else if (Self.left.state = TTrackState.free) then
          Self.state := tsLRMidOccupied;
      end;

    tsRLRightMidOccupied:
      begin
        if (Self.middle.state <> TTrackState.occupied) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.free) then
          Self.state := tsRLMidOccupied;
      end;

    tsLRMidOccupied:
      begin
        if (Self.left.state <> TTrackState.free) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.free) then
        begin
          if (Self.right.state = TTrackState.occupied) then
          begin
            Self.anulEnd := Now + Self.anulTime;
            Self.state := tsLROnlyRightOccupied;
          end
          else
            Self.state := tsFree;
        end;
      end;

    tsRLMidOccupied:
      begin
        if (Self.right.state <> TTrackState.free) then
          Self.state := tsUnexpectedOccupation
        else if (Self.middle.state = TTrackState.free) then
        begin
          if (Self.left.state = TTrackState.occupied) then
          begin
            Self.anulEnd := Now + Self.anulTime;
            Self.state := tsRLOnlyLeftOccupied
          end
          else
            Self.state := tsFree;
        end;
      end;

    tsLROnlyRightOccupied:
      begin
        if ((Self.left.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.free) then
        begin
          if (Self.rightOut.state = TTrackState.occupied) then
            Self.state := tsLRRightOutOccupied
          else
            Self.state := tsFree;
        end else if ((not Self.infiniteAnul) and (Now > Self.anulEnd) and (Self.openingLR = toMiddleFree)) then
          Self.state := tsRLRightOccupied;
      end;

    tsRLOnlyLeftOccupied:
      begin
        if ((Self.right.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.left.state = TTrackState.free) then
        begin
          if (Self.leftOut.state = TTrackState.occupied) then
            Self.state := tsRLLeftOutOccupied
          else
            Self.state := tsFree;
        end else if ((not Self.infiniteAnul) and (Now > Self.anulEnd) and (Self.openingRL = toMiddleFree)) then
          Self.state := tsLRLeftOccupied;
      end;

    tsLRRightOutOccupied:
      begin
        if ((Self.left.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free)) then
          Self.state := tsUnexpectedOccupation
        else if (Self.right.state = TTrackState.occupied) then
          Self.state := tsRLRightOccupied
      end;

    tsRLLeftOutOccupied:
      begin
        if ((Self.right.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free)) then
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
    Exit((Self.left.state <> TTrackState.free) or (Self.middle.state <> TTrackState.free) or
      (Self.right.state <> TTrackState.free));

  case (Self.state) of
    tsLRLeftOccupied, tsLRLeftMidOccupied, tsLRMidOccupied, tsLROnlyRightOccupied, tsLRRightOutOccupied: begin
      case (Self.openingLR) of
        toMiddleFree:
            Result := (Self.state = tsLRLeftOccupied) or (Self.state = tsLRLeftMidOccupied) or (Self.state = tsLRMidOccupied);
        toOutFree:
            Result := (Self.state >= tsLRLeftOccupied);
      else
        Result := true; // never happens
      end;
    end;

    tsRLRightOccupied, tsRLRightMidOccupied, tsRLMidOccupied, tsRLOnlyLeftOccupied, tsRLLeftOutOccupied: begin
      case (Self.openingRL) of
        toMiddleFree:
            Result := (Self.state = tsRLRightOccupied) or (Self.state = tsRLRightMidOccupied) or (Self.state = tsRLMidOccupied);
        toOutFree:
            Result := (Self.state >= tsLRLeftOccupied);
      else
        Result := true; // never happens
      end;
    end;

    tsUnexpectedOccupation, tsException: Result := true;
    tsFree: Result := false;
  else
    Result := true; // never happens
  end;
end;

function TBlkCrossingTrack.mPositiveLight(): Boolean;
begin
  Result := (Self.left.state = TTrackState.free) and (Self.middle.state = TTrackState.free) and
    (Self.right.state = TTrackState.free);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkCrossingTrack.AllFree(): Boolean;
begin
  for var i: Integer := 0 to _SECT_COUNT - 1 do
  begin
    try
      if (Self.sections[i].state <> TTrackState.free) then
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
  Result := (((Self.state = tsRLOnlyLeftOccupied) and (Self.openingRL = toMiddleFree)) or
             ((Self.state = tsLROnlyRightOccupied) and (Self.openingLR = toMiddleFree)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkCrossingTrack.AddOccupiedLMRTracksIds(var occupied: TList<Integer>);
begin
  Self.left.AddOccupiedTracksIds(occupied);
  Self.middle.AddOccupiedTracksIds(occupied);
  Self.right.AddOccupiedTracksIds(occupied);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
