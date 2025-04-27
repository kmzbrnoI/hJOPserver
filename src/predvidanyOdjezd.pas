unit predvidanyOdjezd;

{
  Trida TPOdj reprezentuje predvidany odjezd.
}

interface

uses SysUtils, Graphics, JsonDataObjects;

type
  ENoTimeDefined = class(Exception);
  EOriginNotSet = class(Exception);
  EInvalidTime = class(Exception);

  TPobjPhase = (ppGone, ppSoundLeave, ppGoingToLeave, ppPreparing, ppLongTime, // faze, ve kterych muze byt cas odjezdu
    ppTimeNotSet);
  // muze odpovidat barvam v panelu, ale tohle
  // jsou spis logicke faze

  TPOdj = class
  private
    prel: TTime;
    pabs: TTime;
    porigin: TTime;
    prel_enabled: Boolean;
    pabs_enabled: Boolean;
    porigin_set: Boolean;
    pphase_old: TPobjPhase;

    function GetRel(): TTime;
    procedure SetRel(new: TTime);

    function GetAbs(): TTime;
    procedure SetAbs(new: TTime);

    function GetOrigin(): TTime;
    procedure SetOrigin(new: TTime);

    function IsChanged(): Boolean;
    procedure SetChanged(new: Boolean);

  public

    constructor Create(); overload;
    constructor Create(abs: string; rel: string); overload;
    constructor Create(json: TJsonObject); overload;
    destructor Destroy(); override;

    property rel_enabled: Boolean read prel_enabled write prel_enabled;
    property abs_enabled: Boolean read pabs_enabled write pabs_enabled;
    property origin_set: Boolean read porigin_set;
    property phase_old: TPobjPhase read pphase_old;

    property changed: Boolean read IsChanged write SetChanged; // vraci true pokud je zmena faze

    property rel: TTime read GetRel write SetRel;
    property abs: TTime read GetAbs write SetAbs;
    property origin: TTime read GetOrigin write SetOrigin;

    function IsDepSet(): Boolean; // vraci jestli je mozno spocitat cas do odjezdu
    function DepRealDelta(): TTime; // vraci realny cas do odjezdu
    function DepTime(): TTime; // vraci cas (modelovy nebo realny) odjezdu
    procedure RecordOriginNow();
    function GetPhase(): TPobjPhase;

    procedure GetPtData(json: TJsonObject);

  end;

procedure GetPOdjColors(podj: TPOdj; var fg: TColor; var bg: TColor);

implementation

uses TimeModel, timeHelper, DateUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPOdj.Create();
begin
  inherited;
  Self.prel_enabled := false;
  Self.pabs_enabled := false;
  Self.porigin_set := false;
end;

constructor TPOdj.Create(abs: string; rel: string);
var dt: TDateTime;
begin
  Self.Create();

  if (abs <> '') then
  begin
    if (modelTime.used) then
      Self.abs := modelTime.date + StrToTime(abs)
    else
      Self.abs := date() + StrToTime(abs);

    if (Self.abs < timeHelper.hJOPNow()) then
      Self.abs := IncDay(Self.abs);
  end;

  if (rel <> '') then
  begin
    dt := StrToTime('00:' + rel);
    if (dt = 0) then
      raise EInvalidTime.Create('PODJ nemùže být 0s!');
    Self.rel := dt;
  end;
end;

constructor TPOdj.Create(json: TJsonObject);
var dt: TDateTime;
begin
  Self.Create();

  if (json.Contains('absolute')) then
  begin
    dt := json.D['absolute'];
    if (dt < timeHelper.hJOPNow()) then
      raise EInvalidTime.Create('Nelze vytvoøit PODJ do minulosti!');
    Self.abs := dt;
  end;

  if (json.Contains('relative')) then
  begin
    dt := StrToTime('00:' + json.S['relative']);
    if (dt = 0) then
      raise EInvalidTime.Create('PODJ nemùže být 0s!');
    Self.rel := dt;
  end;
end;

destructor TPOdj.Destroy();
begin
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPOdj.GetRel(): TTime;
begin
  if (not Self.prel_enabled) then
    raise ENoTimeDefined.Create('No relative time defined!');
  Result := Self.prel;
end;

procedure TPOdj.SetRel(new: TTime);
begin
  Self.prel := new;
  Self.prel_enabled := true;
  Self.pphase_old := Self.GetPhase();
end;

function TPOdj.GetAbs(): TTime;
begin
  if (not Self.pabs_enabled) then
    raise ENoTimeDefined.Create('No absolute time defined!');
  Result := Self.pabs;
end;

procedure TPOdj.SetAbs(new: TTime);
begin
  Self.pabs := new;
  Self.pabs_enabled := true;
  Self.pphase_old := Self.GetPhase();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPOdj.IsDepSet(): Boolean;
begin
  Result := (not Self.prel_enabled) or (Self.origin_set);
end;

function TPOdj.DepRealDelta(): TTime;
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

function TPOdj.DepTime(): TTime;
var rel_elapse, abs_elapse: TTime;
begin
  if (not Self.IsDepSet()) then
    raise EOriginNotSet.Create('Departure not set!');

  if (Self.pabs_enabled and Self.prel_enabled) then
  begin
    abs_elapse := timeHelper.RealDelta(Self.pabs);
    rel_elapse := timeHelper.RealDelta(Self.prel + Self.porigin);
    if (abs_elapse > rel_elapse) then
      Exit(Self.pabs)
    else
      Exit(Self.prel + Self.porigin);
  end else

    if (Self.pabs_enabled) then
    Exit(Self.pabs)

  else if (Self.prel_enabled) then
    Exit(Self.prel + Self.porigin)

  else
    Result := 0;
end;

procedure TPOdj.RecordOriginNow();
begin
  Self.origin := hJOPNow();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPOdj.GetOrigin(): TTime;
begin
  if (not Self.porigin_set) then
    raise EOriginNotSet.Create('No origin set!');
  Result := Self.porigin;
end;

procedure TPOdj.SetOrigin(new: TTime);
begin
  Self.porigin := new;
  Self.porigin_set := true;
  Self.pphase_old := Self.GetPhase();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPOdj.GetPhase(): TPobjPhase;
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

function TPOdj.IsChanged(): Boolean;
begin
  Result := (Self.pphase_old <> Self.GetPhase());
end;

procedure TPOdj.SetChanged(new: Boolean);
begin
  if (not new) then
    Self.pphase_old := Self.GetPhase();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPOdj.GetPtData(json: TJsonObject);
begin
  if (Self.rel_enabled) then
    json.S['relative'] := FormatDateTime('nn:ss', Self.rel);
  if (Self.abs_enabled) then
    json.D['absolute'] := Self.abs;
end;

/// /////////////////////////////////////////////////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////

procedure GetPOdjColors(podj: TPOdj; var fg: TColor; var bg: TColor);
var tmp: TColor;
begin
  case (podj.GetPhase()) of
    ppPreparing:
      bg := clBlue;

    ppGoingToLeave, ppGone, ppSoundLeave:
      bg := clYellow;

    ppLongTime, ppTimeNotSet:
      begin
        tmp := fg;
        fg := bg;
        bg := tmp;
      end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
