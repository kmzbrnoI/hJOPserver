unit TBlokIO;

{
  Definice a obsluha technologickeho bloku Vystup.

  Technologicky blok Vystup reprezentuje blok s binarnim stavem s moznym
  vystupem na sbernici RCS.
}

interface

uses IniFiles, TBlok, TechnologieRCS, Classes, SysUtils, IdContext, TOblRizeni,
     Graphics, JsonDataObjects, RCSErrors, RCS;

type

 TBlkIOsettings = record
  isRCSinput: Boolean;
  RCSinputNeeded: Boolean;
  RCSinput: TRCSAddr;

  isRCSOutput: Boolean;
  RCSoutputNeeded: Boolean;
  RCSoutput: TRCSAddr;

  setOutputOnStart: Boolean;
  nullAfterSec: Integer;
 end;

 TBlkIOstate = record
  enabled: Boolean;
  activeOutput: Boolean;
  inputState: TRCSInputState;
  nullTime: TTime;
  stit: string;
 end;

 TBlkIO = class(TBlk)
  const
   _def_IO_stav:TBlkIOstate = (
     enabled: false;
     activeOutput: false;
     inputState: TRCSInputState.isOff;
     nullTime: 0;
   );

  private
   IOsettings: TBlkIOsettings;
   IOstate: TBlkIOstate;

    function IsNullable(): Boolean;
    function IsActiveInput(): Boolean;

    procedure SetStit(stit: string);

    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAktivOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAktivOffClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuInClick(SenderPnl:TIdContext; SenderOR:TObject; target: Boolean);

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
    procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject); override;

    property isRCSoutput: Boolean read IOsettings.isRCSoutput;
    property isRCSinput: Boolean read IOsettings.isRCSinput;
    property RCSoutputNeeded: Boolean read IOsettings.RCSoutputNeeded;
    property RCSinputNeeded: Boolean read IOsettings.RCSinputNeeded;
    property enabled: Boolean read IOstate.enabled;
    property activeOutput: Boolean read IOstate.activeOutput;
    property activeInput: Boolean read IsActiveInput;
    property nullable: Boolean read IsNullable;
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
var strs: TStrings;
    str: string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.IOsettings.isRCSoutput := false;
 Self.IOsettings.isRCSinput := false;

 strs := TStringList.Create();
 try
   ini_tech.ReadSection(section, strs);
   for str in strs do
    begin
     if (str = 'RCSb0') then
      begin
       Self.IOsettings.isRCSoutput := true;
       Self.IOsettings.RCSoutput := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSb0', 0),
                                                 ini_tech.ReadInteger(section, 'RCSp0', 0));
       Self.IOsettings.RCSoutputNeeded := ini_tech.ReadBool(section, 'RCSn0', true);
      end else if (str = 'RCSbi') then begin
       Self.IOsettings.isRCSinput := true;
       Self.IOsettings.RCSinput := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSbi', 0),
                                                ini_tech.ReadInteger(section, 'RCSpi', 0));
       Self.IOsettings.RCSinputNeeded := ini_tech.ReadBool(section, 'RCSni', true);
      end;
    end;
 finally
   strs.Free();
 end;

 Self.LoadORs(ini_rel, 'POM').Free();
 Self.IOsettings.setOutputOnStart := ini_tech.ReadBool(section, 'activateOnStart', false);
 Self.IOsettings.nullAfterSec := ini_tech.ReadInteger(section, 'nullTime', 0);
 Self.IOstate.stit := ini_stat.ReadString(section, 'stit', '');

 if (Self.isRCSinput) then
  begin
   PushRCStoOR(Self.ORsRef, Self.IOsettings.RCSinput);
   RCSi.SetNeeded(Self.IOsettings.RCSinput.board);
  end;
 if (Self.isRCSoutput) then
  begin
   PushRCStoOR(Self.ORsRef, Self.IOsettings.RCSoutput);
   RCSi.SetNeeded(Self.IOsettings.RCSoutput.board);
  end;
end;

procedure TBlkIO.SaveData(ini_tech:TMemIniFile; const section:string);
begin
 inherited SaveData(ini_tech, section);

 if (Self.isRCSoutput) then
  begin
   ini_tech.WriteInteger(section, 'RCSb0', Self.IOsettings.RCSoutput.board);
   ini_tech.WriteInteger(section, 'RCSp0', Self.IOsettings.RCSoutput.port);
   ini_tech.WriteBool(section, 'RCSn0', Self.IOsettings.RCSoutputNeeded);
  end;
 if (Self.isRCSinput) then
  begin
   ini_tech.WriteInteger(section, 'RCSbi', Self.IOsettings.RCSinput.board);
   ini_tech.WriteInteger(section, 'RCSpi', Self.IOsettings.RCSinput.port);
   ini_tech.WriteBool(section, 'RCSni', Self.IOsettings.RCSinputNeeded);
  end;

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
 if ((Self.isRCSoutput) and (Self.RCSoutputNeeded) and (not RCSi.IsNonFailedModule(Self.IOsettings.RCSoutput.board))) then
   Exit();
 if ((Self.isRCSinput) and (Self.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.IOsettings.RCSinput.board))) then
   Exit();

 if (Self.isRCSinput) then
   Self.IOstate.inputState := TRCSInputState.notYetScanned;

 Self.IOstate.enabled := true;
 if (Self.IOsettings.setOutputOnStart) then
   Self.Activate();
end;

procedure TBlkIO.Disable();
begin
 Self.IOstate.enabled := false;
 Self.IOstate.activeOutput := false;
 Self.IOstate.inputState := TRCSInputState.isOff;
end;

function TBlkIO.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := (((Self.isRCSoutput) and (portType = TRCSIOType.output) and (Self.IOsettings.RCSoutput = addr)) or
            ((Self.isRCSinput) and (portType = TRCSIOType.input) and (Self.IOsettings.RCSinput = addr)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.GetSettings():TBlkIOsettings;
begin
 Result := Self.IOsettings;
end;

procedure TBlkIO.SetSettings(data:TBlkIOsettings);
begin
 Self.IOsettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Update();
var inputState: TRCSInputState;
begin
 inherited;

 if ((not Self.enabled) and
     (((not Self.isRCSoutput) or (RCSi.IsNonFailedModule(Self.IOsettings.RCSoutput.board))) or
      ((not Self.isRCSinput) or (RCSi.IsNonFailedModule(Self.IOsettings.RCSinput.board))))) then
  begin
   Self.Enable();
   Self.Change();
  end;
 if ((Self.enabled) and
     (((Self.isRCSoutput) and (Self.IOsettings.RCSoutputNeeded) and (not RCSi.IsNonFailedModule(Self.IOsettings.RCSoutput.board))) or
      ((Self.isRCSinput) and (Self.IOsettings.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.IOsettings.RCSinput.board))))) then
  begin
   Self.Disable();
   Self.Change(true);
  end;

 if ((Self.enabled) and (Self.isRCSinput)) then
  begin
   try
     inputState := RCSi.GetInput(Self.IOsettings.RCSinput);
   except
     on E: RCSException do
       inputState := TRCSInputState.failure;
   end;

   if (inputState <> Self.IOstate.inputState) then
    begin
     Self.IOstate.inputState := inputState;
     Self.Change();
    end;
  end;

 if ((Self.enabled) and (Self.nullable) and (Self.activeOutput) and (Now > Self.IOstate.nullTime)) then
   Self.Deactivate();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Activate();
begin
 if (Self.activeOutput) then Exit();
 Self.IOstate.activeOutput := true;

 if (Self.isRCSoutput) then
  begin
   try
     RCSi.SetOutput(Self.IOsettings.RCSoutput, 1);
   except

   end;
  end;

 if (Self.nullable) then
   Self.IOstate.nullTime := Now +
      EncodeTime(0, Self.IOsettings.nullAfterSec div 60, Self.IOsettings.nullAfterSec mod 60, 0);

 Self.Change();
end;

procedure TBlkIO.Deactivate();
begin
 if (not Self.activeOutput) then Exit();
 Self.IOstate.activeOutput := false;

 if (Self.isRCSoutput) then
  begin
   try
     RCSi.SetOutput(Self.IOsettings.RCSoutput, 0);
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
 else if (Self.activeOutput) then
   fg := clYellow
 else if (Self.isRCSinput) then begin
   case (Self.IOstate.inputState) of
     TRCSInputState.isOff: fg := $A0A0A0;
     TRCSInputState.isOn: fg := clLime;
   else
     fg := clFuchsia;
   end;
 end else
   fg := $A0A0A0;

 Result := Result + PrevodySoustav.ColorToStr(fg) + ';';
 Result := Result + PrevodySoustav.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.GetPtData(json: TJsonObject; includeState: boolean);
begin
 inherited;

 if (Self.isRCSoutput) then
   TBlk.RCStoJSON(Self.IOsettings.RCSoutput, json['rcs']['output']);
 if (Self.isRCSinput) then
   TBlk.RCStoJSON(Self.IOsettings.RCSinput, json['rcs']['input']);

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
 json['activeOutput'] := Self.activeOutput;
 json['activeInput'] := Self.activeInput;
end;

procedure TBlkIO.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
begin
 if (not Self.enabled) then Exit();

 if (reqJson.Contains('activeOutput')) then
  begin
   if ((reqJson.B['activeOutput']) and (not Self.activeOutput)) then
     Self.Activate()
   else if ((not reqJson.B['activeOutput']) and (Self.activeOutput)) then
     Self.Deactivate();
  end;

 if (reqJson.Contains('activeInput') and (not Self.isRCSinput)) then
  begin
   if ((reqJson.B['activeInput']) and (not Self.activeInput)) then
    begin
     Self.IOstate.inputState := TRCSInputState.isOn;
     Self.Change();
    end else if ((not reqJson.B['activeInput']) and (Self.activeInput)) then begin
     Self.IOstate.inputState := TRCSInputState.isOff;
     Self.Change();
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.IsNullable():boolean;
begin
 Result := (Self.IOsettings.nullAfterSec > 0);
end;

function TBlkIO.IsActiveInput(): Boolean;
begin
 Result := (Self.IOstate.inputState = TRCSInputState.isOn);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;
 if (Self.activeOutput) then
   Result := Result + 'AKTIV<,'
 else
   Result := Result + 'AKTIV>,';
 Result := Result + 'STIT,';

 if ((RCSi.simulation) and (Self.isRCSinput)) then
  begin
   if (Self.IsActiveInput) then
     Result := Result + '-,*IN<,'
   else
     Result := Result + '-,*IN>,'
  end;
end;

procedure TBlkIO.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (not Self.enabled) then Exit();

 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV>') then Self.MenuAktivOnClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV<') then Self.MenuAktivOffClick(SenderPnl, SenderOR)
 else if (item = 'IN<') then Self.MenuInClick(SenderPnl, SenderOR, false)
 else if (item = 'IN>') then Self.MenuInClick(SenderPnl, SenderOR, true);
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

procedure TBlkIO.MenuInClick(SenderPnl:TIdContext; SenderOR:TObject; target: Boolean);
begin
 try
   RCSi.SetInput(Self.IOsettings.RCSinput, PrevodySoustav.BoolToInt(target));
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupù!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
