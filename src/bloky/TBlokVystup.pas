unit TBlokVystup;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup reprezentuje blok s binarnim stavem s moznym
  vystupem na sbernici RCS.
}

interface

uses IniFiles, TBlok, TechnologieRCS, Classes, SysUtils, IdContext, TOblRizeni,
     Graphics;

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

    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    //----- Vystup own functions -----

    function GetSettings(): TBlkVystupSettings;
    procedure SetSettings(data: TBlkVystupSettings);

    property enabled: boolean read VystupStav.enabled;
    property rcsUsed: boolean read GetRCSUsed;
    property active: boolean read VystupStav.active;

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

end.//unit
