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
  RCSAddrs:TRCSAddrs;
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

uses TechnologieRCS;

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
 Self.VystupSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
end;//procedure

procedure TBlkVystup.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);
 Self.SaveRCS(ini_tech,section, Self.VystupSettings.RCSAddrs);
end;//procedure

procedure TBlkVystup.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 //
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.Enable();
var RCSaddr:TRCSAddr;
begin
 Self.VystupStav.enabled := true;
 for RCSaddr in Self.VystupSettings.RCSAddrs do
  begin
   try
     if ((RCSi.Started) and (RCSi.IsModule(RCSaddr.board)) and (not RCSi.IsModuleFailure(RCSaddr.board))) then
       RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 1);
   except

   end;
  end;
end;//procedure

procedure TBlkVystup.Disable();
var RCSaddr:TRCSAddr;
begin
 Self.VystupStav.enabled := false;
 for RCSaddr in Self.VystupSettings.RCSAddrs do
  begin
   try
     if ((RCSi.Started) and (RCSi.IsModule(RCSaddr.board)) and (not RCSi.IsModuleFailure(RCSaddr.board))) then
       RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 0);
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
 if (Self.VystupSettings.RCSAddrs <> data.RCSAddrs) then
   Self.VystupSettings.RCSAddrs.Free();

 Self.VystupSettings := data;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
