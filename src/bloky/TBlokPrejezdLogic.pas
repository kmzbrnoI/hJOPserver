unit TBlokPrejezdLogic;

interface

uses Generics.Collections, TBlokUsekRefs, IniFiles, Classes, SysUtils, StrUtils;

type
 TBlkPrjTrackState = (
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

 TBlkPrjTrackOpening = (
    toMiddleFree = 0,
    toLastOccupied = 1,
    toOutFree = 2
 );

 TBlkPrjTrack = class
  const
   _SECT_COUNT = 5;
   _SECT_LEFT_OUT = 0;
   _SECT_LEFT = 1;
   _SECT_MID = 2;
   _SECT_RIGHT = 3;
   _SECT_RIGHT_OUT = 4;

  private
   mState: TBlkPrjTrackState;
   anulEnd: TTime;

    function mShouldBeClosed(): Boolean;
    function mPozitiva(): Boolean;
    function AllFree(): Boolean;
    function GetAnullation(): Boolean;

    procedure UpdateState();
    procedure SetState(new: TBlkPrjTrackState);

  public
   sections: array[0..4] of TBlkUsekRefs;
   opening: TBlkPrjTrackOpening;
   anulTime: TTime;

   onChanged: TNotifyEvent;
   stateChanged: boolean;

    constructor Create();
    destructor Destroy(); override;

    procedure Load(ini: TMemIniFile; section: string; prefix: string);
    procedure Save(ini: TMemIniFile; section: string; prefix: string);

    procedure Update();

    property state: TBlkPrjTrackState read mState write SetState;
    property shouldBeClosed: boolean read mShouldBeClosed;
    property pozitiva: boolean read mPozitiva;
    property anullation: boolean read GetAnullation;

    property leftOut: TBlkUsekRefs read sections[_SECT_LEFT_OUT] write sections[_SECT_LEFT_OUT];
    property left: TBlkUsekRefs read sections[_SECT_LEFT] write sections[_SECT_LEFT];
    property middle: TBlkUsekRefs read sections[_SECT_MID] write sections[_SECT_MID];
    property right: TBlkUsekRefs read sections[_SECT_RIGHT] write sections[_SECT_RIGHT];
    property rightOut: TBlkUsekRefs read sections[_SECT_RIGHT_OUT] write sections[_SECT_RIGHT_OUT];

 end;

implementation

uses TBlokUsek;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkPrjTrack.Create();
var i: Integer;
begin
 inherited;
 Self.onChanged := nil;
 Self.stateChanged := false;
 for i := 0 to _SECT_COUNT-1 do
   Self.sections[i] := TBlkUsekRefs.Create();
end;

destructor TBlkPrjTrack.Destroy();
var i: Integer;
begin
 for i := 0 to _SECT_COUNT-1 do
   Self.sections[i].Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrjTrack.Load(ini: TMemIniFile; section: string; prefix: string);
var str: string;
begin
 Self.leftOut.Parse(ini.ReadString(section, prefix+'-leftOut', ''));
 Self.left.Parse(ini.ReadString(section, prefix+'-left', ''));
 Self.middle.Parse(ini.ReadString(section, prefix+'-middle', ''));
 Self.right.Parse(ini.ReadString(section, prefix+'-right', ''));
 Self.rightOut.Parse(ini.ReadString(section, prefix+'-rightOut', ''));
 Self.opening := TBlkPrjTrackOpening(ini.ReadInteger(section, prefix+'-opening', 0));

 str := ini.ReadString(section, prefix+'-anulTime', '01:00');
 Self.anulTime := EncodeTime(0, StrToIntDef(LeftStr(str, 2), 1), StrToIntDef(Copy(str, 4, 2), 0), 0);
end;

procedure TBlkPrjTrack.Save(ini: TMemIniFile; section: string; prefix: string);
begin
 if (Self.leftOut.ToStr() <> '') then
   ini.WriteString(section, prefix+'-leftOut', Self.leftOut.ToStr());
 if (Self.left.ToStr() <> '') then
   ini.WriteString(section, prefix+'-left', Self.left.ToStr());
 if (Self.middle.ToStr() <> '') then
   ini.WriteString(section, prefix+'-middle', Self.middle.ToStr());
 if (Self.right.ToStr() <> '') then
   ini.WriteString(section, prefix+'-right', Self.right.ToStr());
 if (Self.rightOut.ToStr() <> '') then
   ini.WriteString(section, prefix+'-rightOut', Self.rightOut.ToStr());
 ini.WriteInteger(section, prefix+'-opening', Integer(Self.opening));
 ini.WriteString(section, prefix+'-anulTime', FormatDateTime('nn:ss', Self.anulTime));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrjTrack.SetState(new: TBlkPrjTrackState);
begin
 if (new <> Self.mState) then
  begin
   Self.mState := new;
   Self.stateChanged := true;
   if (Assigned(Self.onChanged)) then
     Self.onChanged(Self);
  end;
end;

procedure TBlkPrjTrack.Update();
begin
 try
  Self.UpdateState();
 except
  Self.state := tsException;
 end;
end;

procedure TBlkPrjTrack.UpdateState();
begin
 if (Self.AllFree()) then
  begin
   if (Self.state <> tsFree) then
     Self.state := tsFree;
   Exit();
  end;

 case (Self.state) of
  tsFree: begin
   if ((Self.left.state = TUsekStav.obsazeno) and (Self.middle.state = TUsekStav.uvolneno) and
       (Self.right.state = TUsekStav.uvolneno)) then
     Self.state := tsLRLeftOccupied
   else if ((Self.right.state = TUsekStav.obsazeno) and (Self.middle.state = TUsekStav.uvolneno) and
            (Self.left.state = TUsekStav.uvolneno)) then
     Self.state := tsRLRightOccupied
   else
     Self.state := tsUnexpectedOccupation;
  end;

  tsUnexpectedOccupation: begin
    if ((Self.left.state = TUsekStav.uvolneno) and (Self.middle.state = TUsekStav.uvolneno) and
        (Self.right.state = TUsekStav.uvolneno)) then
      Self.state := tsFree;
  end;

  tsLRLeftOccupied: begin
    if ((Self.left.state <> TUsekStav.obsazeno) or (Self.right.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.middle.state = TUsekStav.obsazeno) then
      Self.state := tsLRLeftMidOccupied
  end;

  tsRLRightOccupied: begin
    if ((Self.right.state <> TUsekStav.obsazeno) or (Self.left.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.middle.state = TUsekStav.obsazeno) then
      Self.state := tsRLRightMidOccupied
  end;

  tsLRLeftMidOccupied: begin
    if (Self.middle.state <> TUsekStav.obsazeno) then
      Self.state := tsUnexpectedOccupation
    else if (Self.left.state = TUsekStav.uvolneno) then
      Self.state := tsLRMidOccupied
    else if ((Self.right.changed) and (Self.opening = toLastOccupied)) then
      Self.stateChanged := true;
  end;

  tsRLRightMidOccupied: begin
    if (Self.middle.state <> TUsekStav.obsazeno) then
      Self.state := tsUnexpectedOccupation
    else if (Self.right.state = TUsekStav.uvolneno) then
      Self.state := tsRLMidOccupied
    else if ((Self.left.changed) and (Self.opening = toLastOccupied)) then
      Self.stateChanged := true;
  end;

  tsLRMidOccupied: begin
    if (Self.left.state <> TUsekStav.uvolneno) then
      Self.state := tsUnexpectedOccupation
    else if (Self.middle.state = TUsekStav.uvolneno) then
     begin
      if (Self.right.state = TUsekStav.obsazeno) then
       begin
        Self.anulEnd := Now + Self.anulTime;
        Self.state := tsLROnlyRightOccupied;
       end else
        Self.state := tsFree;
     end else if ((Self.right.changed) and (Self.opening = toLastOccupied)) then
      Self.stateChanged := true;
  end;

  tsRLMidOccupied: begin
    if (Self.right.state <> TUsekStav.uvolneno) then
      Self.state := tsUnexpectedOccupation
    else if (Self.middle.state = TUsekStav.uvolneno) then
     begin
      if (Self.left.state = TUsekStav.obsazeno) then
       begin
        Self.anulEnd := Now + Self.anulTime;
        Self.state := tsRLOnlyLeftOccupied
       end else
        Self.state := tsFree;
     end else if ((Self.left.changed) and (Self.opening = toLastOccupied)) then
      Self.stateChanged := true;
  end;

  tsLROnlyRightOccupied: begin
    if ((Self.left.state <> TUsekStav.uvolneno) or (Self.middle.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.right.state = TUsekStav.uvolneno) then
     begin
      if (Self.rightOut.state = TUsekStav.obsazeno) then
        Self.state := tsLRRightOutOccupied
      else
        Self.state := tsFree;
     end else if ((Now > Self.anulEnd) and (Self.opening = toMiddleFree)) then
      Self.state := tsRLRightOccupied;
  end;

  tsRLOnlyLeftOccupied: begin
    if ((Self.right.state <> TUsekStav.uvolneno) or (Self.middle.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.left.state = TUsekStav.uvolneno) then
     begin
      if (Self.leftOut.state = TUsekStav.obsazeno) then
        Self.state := tsRLLeftOutOccupied
      else
        Self.state := tsFree;
     end else if ((Now > Self.anulEnd) and (Self.opening = toMiddleFree)) then
      Self.state := tsLRLeftOccupied;
  end;

  tsLRRightOutOccupied: begin
    if ((Self.left.state <> TUsekStav.uvolneno) or (Self.middle.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.right.state = TUsekStav.obsazeno) then
      Self.state := tsRLRightOccupied
  end;

  tsRLLeftOutOccupied: begin
    if ((Self.right.state <> TUsekStav.uvolneno) or (Self.middle.state <> TUsekStav.uvolneno)) then
      Self.state := tsUnexpectedOccupation
    else if (Self.left.state = TUsekStav.obsazeno) then
      Self.state := tsLRLeftOccupied
  end;

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrjTrack.mShouldBeClosed(): Boolean;
begin
 if (Self.state = tsUnexpectedOccupation) then
   Exit((Self.left.state <> TUsekStav.uvolneno) or (Self.middle.state <> TUsekStav.uvolneno) or
        (Self.right.state <> TUsekStav.uvolneno));

 case (Self.opening) of
  toMiddleFree: begin
    Result := (Self.state = tsLRLeftOccupied) or (Self.state = tsRLRightOccupied) or
              (Self.state = tsLRLeftMidOccupied) or (Self.state = tsRLRightMidOccupied) or
              (Self.state = tsLRMidOccupied) or (Self.state = tsRLMidOccupied);
  end;

  toLastOccupied: begin
    Result := (Self.state = tsLRLeftOccupied) or (Self.state = tsRLRightOccupied) or
              (((Self.state = tsLRLeftMidOccupied) or (Self.state = tsLRMidOccupied)) and (Self.right.state <> TUsekStav.obsazeno)) or
              (((Self.state = tsRLRightMidOccupied) or (Self.state = tsRLMidOccupied)) and (Self.left.state <> TUsekStav.obsazeno));
  end;

  toOutFree: begin
    Result := (Self.state >= tsLRLeftOccupied);
  end;
 else
  Result := true;
 end;
end;

function TBlkPrjTrack.mPozitiva(): Boolean;
begin
 Result := (Self.left.state = TUsekStav.uvolneno) and (Self.middle.state = TUsekStav.uvolneno) and
            (Self.right.state = TUsekStav.uvolneno);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrjTrack.AllFree():boolean;
var i: Integer;
begin
 for i := 0 to _SECT_COUNT-1 do
  begin
   try
     if (Self.sections[i].state <> TUsekStav.uvolneno) then
       Exit(false);
   except
     Exit(false);
   end;
  end;
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrjTrack.GetAnullation(): Boolean;
begin
 Result := (((Self.state = tsRLOnlyLeftOccupied) or (Self.state = tsLROnlyRightOccupied)) and
            (Self.opening = toMiddleFree));
end;

////////////////////////////////////////////////////////////////////////////////

end.
