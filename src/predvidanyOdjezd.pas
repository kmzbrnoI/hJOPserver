unit predvidanyOdjezd;

{
  Trida TPOdj reprezentuje predvidany odjezd.
}

interface

uses SysUtils;

type
  ENoTimeDefined = class(Exception);

  TPOdj = class
  private
   prel: TTime;
   pabs: TTime;
   prel_enabled: boolean;
   pabs_enabled: boolean;

    function GetRel():TTime;
    procedure SetRel(new:TTime);

    function GetAbs():TTime;
    procedure SetAbs(new:TTime);

  public
    constructor Create();
    destructor Destroy(); override;

    property rel_enabled: boolean read prel_enabled write prel_enabled;
    property abs_enabled: boolean read pabs_enabled write pabs_enabled;

    property rel:TTime read GetRel write SetRel;
    property abs:TTime read GetAbs write SetAbs;

  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TPOdj.Create();
begin
 inherited;
 Self.prel_enabled := false;
 Self.pabs_enabled := false;
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
end;

////////////////////////////////////////////////////////////////////////////////

end.
