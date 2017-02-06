unit TBlokVystup;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup nastavi po spusteni systemu konkretni MTB vystup
  do logicke hodnoty "1", pri vypinani systemu ho vrati do logicke hodnoty
  "0".
}

interface

uses IniFiles,TBlok;

type

 TBlkVystupSettings = record
  MTBAddrs:TMTBAddrs;
 end;

 TBlkVystupStav = record
  enabled:boolean;
 end;

 TBlkVystup = class(TBlk)
  const
   _def_vystup_stav:TBlkVystupStav = (
     enabled: false;
   );

  private
   VystupSettings:TBlkVystupSettings;
   VystupStav:TBlkVystupStav;

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

    //----- Vystup own functions -----

    function GetSettings():TBlkVystupSettings;
    procedure SetSettings(data:TBlkVystupSettings);

    property enabled : boolean read VystupStav.enabled;

 end;//class TBlkVystup

////////////////////////////////////////////////////////////////////////////////

implementation

uses TechnologieMTB;

constructor TBlkVystup.Create(index:Integer);
begin
 inherited;
 Self.VystupStav := _def_vystup_stav;
 Self.GlobalSettings.typ := _BLK_VYSTUP;
end;//ctor

destructor TBlkVystup.Destroy();
begin
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.VystupSettings.MTBAddrs := Self.LoadMTB(ini_tech, section);
end;//procedure

procedure TBlkVystup.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);
 Self.SaveMTB(ini_tech,section, Self.VystupSettings.MTBAddrs);
end;//procedure

procedure TBlkVystup.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 //
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.Enable();
var i:Integer;
begin
 Self.VystupStav.enabled := true;
 for i := 0 to Self.VystupSettings.MTBAddrs.Count-1 do
  begin
   try
     if ((MTB.Started) and (MTB.IsModule(Self.VystupSettings.MTBAddrs.data[i].board)) and (not MTB.IsModuleFailure(Self.VystupSettings.MTBAddrs.data[i].board))) then
       MTB.SetOutput(Self.VystupSettings.MTBAddrs.data[i].board, Self.VystupSettings.MTBAddrs.data[i].port, 1);
   except

   end;
  end;
end;//procedure

procedure TBlkVystup.Disable();
var i:Integer;
begin
 Self.VystupStav.enabled := false;
 for i := 0 to Self.VystupSettings.MTBAddrs.Count-1 do
  begin
   try
     if ((MTB.Started) and (MTB.IsModule(Self.VystupSettings.MTBAddrs.data[i].board)) and (not MTB.IsModuleFailure(Self.VystupSettings.MTBAddrs.data[i].board))) then
       MTB.SetOutput(Self.VystupSettings.MTBAddrs.data[i].board, Self.VystupSettings.MTBAddrs.data[i].port, 0);
   except

   end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.GetSettings():TBlkVystupSettings;
begin
 Result := Self.VystupSettings;
end;//function

procedure TBlkVystup.SetSettings(data:TBlkVystupSettings);
begin
 Self.VystupSettings := data;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
