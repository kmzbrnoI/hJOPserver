unit TBlokIR;

//definice a obsluha technologickeho bloku IR

interface

uses IniFiles, TBlok, JsonDataObjects, TechnologieRCS;

type
 TIRStav = (disabled = -5, none = -1, uvolneno = 0, obsazeno = 1);

 TBlkIRSettings = record
  RCSAddrs:TRCSAddrs;     //only 1 address
 end;

 TBlkIRStav = record
  Stav,StavOld:TIRStav;
 end;


 TBlkIR = class(TBlk)
  const
   //defaultni stav
   _def_ir_stav:TBlkIRStav = (
     Stav : disabled;
     StavOld : disabled;
   );

  private
   IRSettings:TBlkIRSettings;
   IRStav:TBlkIRStav;

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;

    //----- IR own functions -----

    function GetSettings():TBlkIRSettings;
    procedure SetSettings(data:TBlkIRSettings);

    //PT:

    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;

    property Stav:TIRStav read IRStav.Stav;
 end;//class TBlkIR

////////////////////////////////////////////////////////////////////////////////

implementation

uses RCS;

constructor TBlkIR.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_IR;
 Self.IRStav := Self._def_ir_stav;
end;//ctor

destructor TBlkIR.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.IRSettings.RCSAddrs := Self.LoadRCS(ini_tech,section);
end;

procedure TBlkIR.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 Self.SaveRCS(ini_tech,section,Self.IRSettings.RCSAddrs);
end;

procedure TBlkIR.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 //
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Enable();
begin
 Self.IRStav.Stav := none;
 Self.Update();   //will call change
end;

procedure TBlkIR.Disable();
begin
 Self.IRStav.Stav    := disabled;
 Self.IRStav.StavOld := disabled;
 Self.Change(true);
end;

function TBlkIR.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.input) and (Self.IRSettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.Update();
var state:TRCSInputState;
begin
 try
   state := RCSi.GetInput(Self.IRSettings.RCSAddrs[0])
 except
   state := failure;
 end;

 case (state) of
  isOff : Self.IRStav.Stav := TIRStav.uvolneno;
  isOn : Self.IRStav.Stav := TIRStav.obsazeno;
 else
  Self.IRStav.Stav := TIRStav.disabled;
 end;

 if (Self.IRStav.Stav <> Self.IRStav.StavOld) then
  begin
   Self.Change();
   Self.IRStav.StavOld := Self.IRStav.Stav;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIR.GetSettings():TBlkIRSettings;
begin
 Result := Self.IRSettings;
end;

procedure TBlkIR.SetSettings(data:TBlkIRSettings);
begin
 if (Self.IRSettings.RCSAddrs <> data.RCSAddrs) then
   Self.IRSettings.RCSAddrs.Free();

 Self.IRSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIR.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;

 TBlk.RCStoJSON(Self.IRSettings.RCSAddrs[0], json['rcs']);

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkIR.GetPtState(json:TJsonObject);
begin
 case (Self.Stav) of
  TIRStav.disabled : json['stav'] := 'vypnuto';
  TIRStav.none     : json['stav'] := 'zadny';
  TIRStav.uvolneno : json['stav'] := 'uvolneno';
  TIRStav.obsazeno : json['stav'] := 'obsazeno';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
