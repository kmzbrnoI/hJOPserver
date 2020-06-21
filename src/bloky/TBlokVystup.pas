unit TBlokVystup;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup reprezentuje blok s binarnim stavem s moznym
  vystupem na sbernici RCS.
}

interface

uses IniFiles, TBlok, TechnologieRCS, Classes, SysUtils, IdContext, TOblRizeni,
     Graphics, JsonDataObjects;

type

 TBlkVystupSettings = record
  RCSAddrs: TRCSAddrs;
  setOutputOnStart: boolean;
  nullAfterSec: Integer;
 end;

 TBlkVystupStav = record
  enabled: boolean;
  active: boolean;
  nullTime: TTime;
  stit: string;
 end;

 TBlkVystup = class(TBlk)
  const
   _def_vystup_stav:TBlkVystupStav = (
     enabled: false;
     active: false;
     nullTime: 0;
   );

  private
   VystupSettings: TBlkVystupSettings;
   VystupStav: TBlkVystupStav;

    function GetRCSUsed():boolean;
    function IsNullable():boolean;

    procedure SetStit(stit:string);

    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAktivOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAktivOffClick(SenderPnl:TIdContext; SenderOR:TObject);

  public
    constructor Create(index:Integer);

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile; const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile; const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Activate();
    procedure Deactivate();

    procedure Update(); override;

    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;

    //----- Vystup own functions -----

    function GetSettings(): TBlkVystupSettings;
    procedure SetSettings(data: TBlkVystupSettings);

    procedure GetPtData(json: TJsonObject; includeState: boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    property enabled: boolean read VystupStav.enabled;
    property rcsUsed: boolean read GetRCSUsed;
    property active: boolean read VystupStav.active;
    property nullable: boolean read IsNullable;
    property stit:string read VystupStav.stit write SetStit;

 end;//class TBlkVystup

////////////////////////////////////////////////////////////////////////////////

implementation

uses TOblsRizeni, TCPServerOR, Prevody;

constructor TBlkVystup.Create(index:Integer);
begin
 inherited;
 Self.VystupStav := _def_vystup_stav;
 Self.GlobalSettings.typ := _BLK_VYSTUP;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.VystupSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
 Self.LoadORs(ini_rel, 'POM').Free();
 Self.VystupSettings.setOutputOnStart := ini_tech.ReadBool(section, 'activateOnStart', false);
 Self.VystupSettings.nullAfterSec := ini_tech.ReadInteger(section, 'nullTime', 0);
 Self.VystupStav.Stit := ini_stat.ReadString(section, 'stit', '');
 PushRCStoOR(Self.ORsRef, Self.VystupSettings.RCSAddrs);
end;

procedure TBlkVystup.SaveData(ini_tech:TMemIniFile; const section:string);
begin
 inherited SaveData(ini_tech, section);
 Self.SaveRCS(ini_tech,section, Self.VystupSettings.RCSAddrs);
 ini_tech.WriteBool(section, 'activateOnStart', Self.VystupSettings.setOutputOnStart);
 if (Self.nullable) then
   ini_tech.WriteInteger(section, 'nullTime', Self.VystupSettings.nullAfterSec);
end;

procedure TBlkVystup.SaveStatus(ini_stat:TMemIniFile; const section:string);
begin
 if (Self.VystupStav.Stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.VystupStav.Stit);
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

procedure TBlkVystup.Update();
begin
 inherited;

 if ((Self.enabled) and (Self.nullable) and (Self.active) and (Now > Self.VystupStav.nullTime)) then
   Self.Deactivate();
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

 if (Self.nullable) then
   Self.VystupStav.nullTime := Now +
      EncodeTime(0, Self.VystupSettings.nullAfterSec div 60, Self.VystupSettings.nullAfterSec mod 60, 0);

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

procedure TBlkVystup.PanelClick(SenderPnl:TIdContext; SenderOR:TObject;
                                Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (not Self.enabled) then Exit();

 if (Button = TPanelButton.F2) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights))
 else if (Button = TPanelButton.ENTER) then begin
   try
     Self.Activate();
   except
     ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se aktivovat blok', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
   end
 end else if (Button = TPanelButton.ESCAPE) then begin
   try
     Self.Deactivate();
   except
     ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se deaktivovat blok', TOR(SenderOR).ShortName, 'TECHNOLOGIE');
   end
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.PanelStateString():string;
var fg, bg: TColor;
begin
 Result := inherited;

 bg := clBlack;
 if (Self.stit <> '') then bg := clTeal;

 if (not Self.enabled) then
   fg := clFuchsia
 else if (Self.active) then
   fg := clYellow
 else
   fg := $A0A0A0;

 Result := Result + PrevodySoustav.ColorToStr(fg) + ';';
 Result := Result + PrevodySoustav.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVystup.GetPtData(json: TJsonObject; includeState: boolean);
begin
 inherited;

 TBlk.RCSstoJSON(Self.VystupSettings.RCSAddrs, json.A['rcs']);
 json['setOutputOnStart'] := Self.VystupSettings.setOutputOnStart;
 if (includeState) then
   Self.GetPtState(json['blokStav']);
 json['nullable'] := Self.nullable;
 if (Self.nullable) then
   json['nullTime'] := Self.VystupSettings.nullAfterSec;
end;

procedure TBlkVystup.GetPtState(json: TJsonObject);
begin
 json['enabled'] := Self.VystupStav.enabled;
 json['active'] := Self.VystupStav.active;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.IsNullable():boolean;
begin
 Result := (Self.VystupSettings.nullAfterSec > 0);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVystup.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;
 if (Self.active) then
   Result := Result + 'AKTIV<,'
 else
   Result := Result + 'AKTIV>,';
 Result := Result + 'STIT,';
end;

procedure TBlkVystup.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (not Self.enabled) then Exit();

 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV>') then Self.MenuAktivOnClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV<') then Self.MenuAktivOffClick(SenderPnl, SenderOR);
end;

procedure TBlkVystup.SetStit(stit:string);
begin
 Self.VystupStav.Stit := stit;
 Self.Change();
end;

procedure TBlkVystup.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.stit);
end;

procedure TBlkVystup.MenuAktivOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.Activate();
end;

procedure TBlkVystup.MenuAktivOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.Deactivate();
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
