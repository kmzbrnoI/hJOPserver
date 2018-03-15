unit predvidanyOdjezd;

{
  Trida TPOdj reprezentuje predvidany odjezd.
}

interface

uses SysUtils, Graphics;

type
  ENoTimeDefined = class(Exception);
  EOriginNotSet = class(Exception);

  TPobjPhase = (ppGone, ppSoundLeave, ppGoingToLeave, ppPreparing, ppLongTime,  // faze, ve kterych muze byt cas odjezdu
                ppTimeNotSet);
                                                                                // muze odpovidat barvam v panelu, ale tohle
                                                                                // jsou spis logicke faze

  TPOdj = class
  private
   prel: TTime;
   pabs: TTime;
   porigin: TTime;
   prel_enabled: boolean;
   pabs_enabled: boolean;
   porigin_set: boolean;
   pphase_old: TPobjPhase;

    function GetRel():TTime;
    procedure SetRel(new:TTime);

    function GetAbs():TTime;
    procedure SetAbs(new:TTime);

    function GetOrigin():TTime;
    procedure SetOrigin(new:TTime);

    function IsChanged():boolean;
    procedure SetChanged(new:boolean);

  public

    constructor Create();
    destructor Destroy(); override;

    property rel_enabled: boolean read prel_enabled write prel_enabled;
    property abs_enabled: boolean read pabs_enabled write pabs_enabled;
    property origin_set: boolean read porigin_set;
    property phase_old: TPobjPhase read pphase_old;

    property changed: boolean read IsChanged write SetChanged;                  // vraci true pokud je zmena faze

    property rel: TTime read GetRel write SetRel;
    property abs: TTime read GetAbs write SetAbs;
    property origin: TTime read GetOrigin write SetOrigin;

    function IsDepSet():boolean;                                                // vraci jestli je mozno spocitat cas do odjezdu
    function DepRealDelta():TTime;                                              // vraci realny cas do odjezdu
    procedure RecordOriginNow();
    function GetPhase():TPobjPhase;

  end;

procedure GetPOdjColors(podj:TPOdj; var fg:TColor; var bg:TColor);

implementation

uses ModelovyCas, timeHelper;

////////////////////////////////////////////////////////////////////////////////

constructor TPOdj.Create();
begin
 inherited;
 Self.prel_enabled := false;
 Self.pabs_enabled := false;
 Self.porigin_set  := false;
end;

destructor TPOdj.Destroy();
begin
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TPOdj.GetRel():TTime;
begin
 if (not Self.prel_enabled) then
   raise ENoTimeDefined.Create('No relative time defined!');
 Result := Self.prel;
end;

procedure TPOdj.SetRel(new:TTime);
begin
 Self.prel := new;
 Self.prel_enabled := true;
 Self.pphase_old := Self.GetPhase();
end;

function TPOdj.GetAbs():TTime;
begin
 if (not Self.pabs_enabled) then
   raise ENoTimeDefined.Create('No absolute time defined!');
 Result := Self.pabs;
end;

procedure TPOdj.SetAbs(new:TTime);
begin
 Self.pabs := new;
 Self.pabs_enabled := true;
 Self.pphase_old := Self.GetPhase();
end;

////////////////////////////////////////////////////////////////////////////////

function TPOdj.IsDepSet():boolean;
begin
 Result := (not Self.prel_enabled) or (Self.origin_set);
end;

function TPOdj.DepRealDelta():TTime;
var rel_elapse, abs_elapse: TTime;
begin
 if (not Self.IsDepSet()) then
   raise EOriginNotSet.Create('Departure not set!');

 if (Self.pabs_enabled and Self.prel_enabled) then
  begin
   abs_elapse := timeHelper.RealDelta(Self.pabs);
   rel_elapse := timeHelper.RealDelta(Self.prel + Self.porigin);
   if (abs_elapse > rel_elapse) then
     Exit(abs_elapse)
   else
     Exit(rel_elapse);
  end else

 if (Self.pabs_enabled) then
   Exit(timeHelper.RealDelta(Self.pabs))

 else if (Self.prel_enabled) then
   Exit(timeHelper.RealDelta(Self.prel + Self.porigin))

 else
   Result := 0;
end;

procedure TPOdj.RecordOriginNow();
begin
 if (ModCas.used) then
   Self.origin := ModCas.dateTime
 else
   Self.origin := Now;
end;

////////////////////////////////////////////////////////////////////////////////

function TPOdj.GetOrigin():TTime;
begin
 if (not Self.porigin_set) then
   raise EOriginNotSet.Create('No origin set!');
 Result := Self.porigin;
end;

procedure TPOdj.SetOrigin(new:TTime);
begin
 Self.porigin := new;
 Self.porigin_set := true;
 Self.pphase_old := Self.GetPhase();
end;

////////////////////////////////////////////////////////////////////////////////

function TPOdj.GetPhase():TPobjPhase;
var time: TTime;
begin
 if (Self.IsDepSet()) then
  begin
   time := Self.DepRealDelta();
   if (time <= 0) then
     Result := ppGone
   else if (time <= EncodeTime(0, 0, 3, 0)) then
     Result := ppSoundLeave
   else if (time <= EncodeTime(0, 0, 30, 0)) then
     Result := ppGoingToLeave
   else if (time <= EncodeTime(0, 3, 0, 0)) then
     Result := ppPreparing
   else
     Result := ppLongTime;
  end else begin
   Result := ppTimeNotSet;
  end;
end;

function TPOdj.IsChanged():boolean;
begin
 Result := (Self.pphase_old <> Self.GetPhase());
end;

procedure TPOdj.SetChanged(new:boolean);
begin
 if (not new) then
   Self.pphase_old := Self.GetPhase();
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure GetPOdjColors(podj:TPOdj; var fg:TColor; var bg:TColor);
var tmp:TColor;
begin
 case (podj.GetPhase()) of
   ppPreparing: bg := clBlue;

   ppGoingToLeave, ppGone, ppSoundLeave: bg := clYellow;

   ppLongTime, ppTimeNotSet: begin
     tmp := fg;
     fg := bg;
     bg := tmp;
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
