unit TBlokIO;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup reprezentuje blok s binarnim stavem s moznym
  vystupem na sbernici RCS.
}

interface

uses IniFiles, TBlok, TechnologieRCS, Classes, SysUtils, IdContext, TOblRizeni,
     Graphics, JsonDataObjects;

type

 TBlkIOsettings = record
  RCSAddrs: TRCSAddrs;
  setOutputOnStart: boolean;
  nullAfterSec: Integer;
 end;

 TBlkIOstate = record
  enabled: boolean;
  active: boolean;
  nullTime: TTime;
  stit: string;
 end;

 TBlkIO = class(TBlk)
  const
   _def_IO_stav:TBlkIOstate = (
     enabled: false;
     active: false;
     nullTime: 0;
   );

  private
   IOsettings: TBlkIOsettings;
   IOstate: TBlkIOstate;

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

    //----- IO own functions -----

    function GetSettings(): TBlkIOsettings;
    procedure SetSettings(data: TBlkIOsettings);

    procedure GetPtData(json: TJsonObject; includeState: boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    property enabled: boolean read IOstate.enabled;
    property rcsUsed: boolean read GetRCSUsed;
    property active: boolean read IOstate.active;
    property nullable: boolean read IsNullable;
    property stit:string read IOstate.stit write SetStit;

 end;//class TBlkIO

////////////////////////////////////////////////////////////////////////////////

implementation

uses TOblsRizeni, TCPServerOR, Prevody;

constructor TBlkIO.Create(index:Integer);
begin
 inherited;
 Self.IOstate := _def_IO_stav;
 Self.GlobalSettings.typ := _BLK_IO;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.IOsettings.RCSAddrs := Self.LoadRCS(ini_tech, section);
 Self.LoadORs(ini_rel, 'POM').Free();
 Self.IOsettings.setOutputOnStart := ini_tech.ReadBool(section, 'activateOnStart', false);
 Self.IOsettings.nullAfterSec := ini_tech.ReadInteger(section, 'nullTime', 0);
 Self.IOstate.Stit := ini_stat.ReadString(section, 'stit', '');
 PushRCStoOR(Self.ORsRef, Self.IOsettings.RCSAddrs);
end;

procedure TBlkIO.SaveData(ini_tech:TMemIniFile; const section:string);
begin
 inherited SaveData(ini_tech, section);
 Self.SaveRCS(ini_tech,section, Self.IOsettings.RCSAddrs);
 ini_tech.WriteBool(section, 'activateOnStart', Self.IOsettings.setOutputOnStart);
 if (Self.nullable) then
   ini_tech.WriteInteger(section, 'nullTime', Self.IOsettings.nullAfterSec);
end;

procedure TBlkIO.SaveStatus(ini_stat:TMemIniFile; const section:string);
begin
 if (Self.IOstate.Stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.IOstate.Stit);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Enable();
begin
 Self.IOstate.enabled := true;
 if (Self.IOsettings.setOutputOnStart) then
   Self.Activate();
end;

procedure TBlkIO.Disable();
begin
 Self.IOstate.enabled := false;
 Self.IOstate.active := false;
end;

function TBlkIO.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.output) and (Self.IOsettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.GetSettings():TBlkIOsettings;
begin
 Result := Self.IOsettings;
end;

procedure TBlkIO.SetSettings(data:TBlkIOsettings);
begin
 if (Self.IOsettings.RCSAddrs <> data.RCSAddrs) then
   Self.IOsettings.RCSAddrs.Free();

 Self.IOsettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.GetRCSUsed():boolean;
begin
 Result := (Self.IOsettings.RCSAddrs.Count > 0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Update();
begin
 inherited;

 if ((Self.enabled) and (Self.nullable) and (Self.active) and (Now > Self.IOstate.nullTime)) then
   Self.Deactivate();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Activate();
var RCSaddr:TRCSAddr;
begin
 if (Self.active) then Exit();
 Self.IOstate.active := true;

 for RCSaddr in Self.IOsettings.RCSAddrs do
  begin
   try
     RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 1);
   except

   end;
  end;

 if (Self.nullable) then
   Self.IOstate.nullTime := Now +
      EncodeTime(0, Self.IOsettings.nullAfterSec div 60, Self.IOsettings.nullAfterSec mod 60, 0);

 Self.Change();
end;

procedure TBlkIO.Deactivate();
var RCSaddr:TRCSAddr;
begin
 if (not Self.active) then Exit();
 Self.IOstate.active := false;

 for RCSaddr in Self.IOsettings.RCSAddrs do
  begin
   try
     RCSi.SetOutput(RCSaddr.board, RCSaddr.port, 0);
   except

   end;
  end;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.PanelClick(SenderPnl:TIdContext; SenderOR:TObject;
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

function TBlkIO.PanelStateString():string;
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

procedure TBlkIO.GetPtData(json: TJsonObject; includeState: boolean);
begin
 inherited;

 TBlk.RCSstoJSON(Self.IOsettings.RCSAddrs, json.A['rcs']);
 json['setOutputOnStart'] := Self.IOsettings.setOutputOnStart;
 if (includeState) then
   Self.GetPtState(json['blokStav']);
 json['nullable'] := Self.nullable;
 if (Self.nullable) then
   json['nullTime'] := Self.IOsettings.nullAfterSec;
end;

procedure TBlkIO.GetPtState(json: TJsonObject);
begin
 json['enabled'] := Self.IOstate.enabled;
 json['active'] := Self.IOstate.active;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.IsNullable():boolean;
begin
 Result := (Self.IOsettings.nullAfterSec > 0);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;
 if (Self.active) then
   Result := Result + 'AKTIV<,'
 else
   Result := Result + 'AKTIV>,';
 Result := Result + 'STIT,';
end;

procedure TBlkIO.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (not Self.enabled) then Exit();

 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV>') then Self.MenuAktivOnClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV<') then Self.MenuAktivOffClick(SenderPnl, SenderOR);
end;

procedure TBlkIO.SetStit(stit:string);
begin
 Self.IOstate.Stit := stit;
 Self.Change();
end;

procedure TBlkIO.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.stit);
end;

procedure TBlkIO.MenuAktivOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.Activate();
end;

procedure TBlkIO.MenuAktivOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.Deactivate();
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
