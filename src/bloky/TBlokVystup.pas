unit TBlokVystup;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup reprezentuje blok s binarnim stavem s moznym
  vystupem na sbernici RCS.
}

interface

uses IniFiles, TBlok, TechnologieRCS, Classes, SysUtils;

type

 TBlkVystupSettings = record
  RCSAddrs:TRCSAddrs;
  setOutputOnStart:boolean;
 end;

 TBlkVystupStav = record
  enabled: boolean;
  active: boolean;
 end;

 TBlkVystup = class(TBlk)
  const
   _def_vystup_stav:TBlkVystupStav = (
     enabled: false;
     active: false;
   );

  private
   VystupSettings: TBlkVystupSettings;
   VystupStav: TBlkVystupStav;

    function GetRCSUsed():boolean;

  public
    constructor Create(index:Integer);

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile; const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Activate();
    procedure Deactivate();

    //----- Vystup own functions -----

    function GetSettings(): TBlkVystupSettings;
    procedure SetSettings(data: TBlkVystupSettings);

    property enabled: boolean read VystupStav.enabled;
    property rcsUsed: boolean read GetRCSUsed;
    property active: boolean read VystupStav.active;

 end;//class TBlkVystup

////////////////////////////////////////////////////////////////////////////////

implementation

uses TOblsRizeni;

constructor TBlkVystup.Create(index:Integer);
begin
 inherited;
 Self.VystupStav := _def_vystup_stav;
 Self.GlobalSettings.typ := _BLK_VYSTUP;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
var str: TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.VystupSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();
   try
     ExtractStrings([';'], [], PChar(ini_rel.ReadString('SW', IntToStr(Self.id), '')), str);
     if (str.Count < 1) then Exit;
     if (Self.ORsRef <> nil) then
       Self.ORsRef.Free();
     Self.ORsRef := ORs.ParseORs(str[0]);
   finally
     str.Free();
   end;
  end else begin
   Self.ORsRef.Clear();
  end;

 PushRCStoOR(Self.ORsRef, Self.VystupSettings.RCSAddrs);
end;

procedure TBlkVystup.SaveData(ini_tech:TMemIniFile; const section:string);
begin
 inherited SaveData(ini_tech, section);
 Self.SaveRCS(ini_tech,section, Self.VystupSettings.RCSAddrs);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.Enable();
begin
 Self.VystupStav.enabled := true;
 if (Self.VystupSettings.setOutputOnStart) then
   Self.Activate();
end;

procedure TBlkVystup.Disable();
begin
 Self.VystupStav.enabled := false;
 Self.VystupStav.active := false;
end;

function TBlkVystup.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.output) and (Self.VystupSettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.GetSettings():TBlkVystupSettings;
begin
 Result := Self.VystupSettings;
end;

procedure TBlkVystup.SetSettings(data:TBlkVystupSettings);
begin
 if (Self.VystupSettings.RCSAddrs <> data.RCSAddrs) then
   Self.VystupSettings.RCSAddrs.Free();

 Self.VystupSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.GetRCSUsed():boolean;
begin
 Result := (Self.VystupSettings.RCSAddrs.Count > 0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.Activate();
var RCSaddr:TRCSAddr;
begin
 if (Self.active) then Exit();
 Self.VystupStav.active := true;

 for RCSaddr in Self.VystupSettings.RCSAddrs do
  begin
   try
     RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 1);
   except

   end;
  end;

 Self.Change();
end;

procedure TBlkVystup.Deactivate();
var RCSaddr:TRCSAddr;
begin
 if (not Self.active) then Exit();
 Self.VystupStav.active := false;

 for RCSaddr in Self.VystupSettings.RCSAddrs do
  begin
   try
     RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 0);
   except

   end;
  end;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
