unit BlockIO;

{
  IO technological block definition.

  IO block represents block with binary state which could be propagated to any
  RCS output. It can also display state of any (other) RCS input. This block is
  usually shown as "dot" in the panel.
}

interface

uses IniFiles, Block, TechnologieRCS, Classes, SysUtils, IdContext, Area,
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
  note: string;
 end;

 TBlkIO = class(TBlk)
  const
   _def_IO_stav: TBlkIOstate = (
     enabled: false;
     activeOutput: false;
     inputState: TRCSInputState.isOff;
     nullTime: 0;
   );

  private
   m_settings: TBlkIOsettings;
   m_state: TBlkIOstate;

    function IsNullable(): Boolean;
    function IsActiveInput(): Boolean;

    procedure SetNote(note: string);

    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuInClick(SenderPnl: TIdContext; SenderOR: TObject; target: Boolean);

  public
    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Activate();
    procedure Deactivate();

    procedure Update(); override;

    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights; params: string = ''); override;
    function PanelStateString(): string; override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;

    //----- IO own functions -----

    function GetSettings(): TBlkIOsettings;
    procedure SetSettings(data: TBlkIOsettings);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

    property isRCSoutput: Boolean read m_settings.isRCSoutput;
    property isRCSinput: Boolean read m_settings.isRCSinput;
    property RCSoutputNeeded: Boolean read m_settings.RCSoutputNeeded;
    property RCSinputNeeded: Boolean read m_settings.RCSinputNeeded;
    property enabled: Boolean read m_state.enabled;
    property activeOutput: Boolean read m_state.activeOutput;
    property activeInput: Boolean read IsActiveInput;
    property nullable: Boolean read IsNullable;
    property note: string read m_state.note write SetNote;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses AreaDb, TCPServerPanel, ownConvert;

constructor TBlkIO.Create(index: Integer);
begin
 inherited;
 Self.m_state := _def_IO_stav;
 Self.m_globSettings.typ := btIO;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var strs: TStrings;
    str: string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.m_settings.isRCSoutput := false;
 Self.m_settings.isRCSinput := false;

 strs := TStringList.Create();
 try
   ini_tech.ReadSection(section, strs);
   for str in strs do
    begin
     if (str = 'RCSb0') then
      begin
       Self.m_settings.isRCSoutput := true;
       Self.m_settings.RCSoutput := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSb0', 0),
                                                 ini_tech.ReadInteger(section, 'RCSp0', 0));
       Self.m_settings.RCSoutputNeeded := ini_tech.ReadBool(section, 'RCSn0', true);
      end else if (str = 'RCSbi') then begin
       Self.m_settings.isRCSinput := true;
       Self.m_settings.RCSinput := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSbi', 0),
                                                ini_tech.ReadInteger(section, 'RCSpi', 0));
       Self.m_settings.RCSinputNeeded := ini_tech.ReadBool(section, 'RCSni', true);
      end;
    end;
 finally
   strs.Free();
 end;

 Self.LoadORs(ini_rel, 'POM').Free();
 Self.m_settings.setOutputOnStart := ini_tech.ReadBool(section, 'activateOnStart', false);
 Self.m_settings.nullAfterSec := ini_tech.ReadInteger(section, 'nullTime', 0);
 Self.m_state.note := ini_stat.ReadString(section, 'stit', '');

 if (Self.isRCSinput) then
  begin
   PushRCSToArea(Self.m_areas, Self.m_settings.RCSinput);
   RCSi.SetNeeded(Self.m_settings.RCSinput.board);
  end;
 if (Self.isRCSoutput) then
  begin
   PushRCSToArea(Self.m_areas, Self.m_settings.RCSoutput);
   RCSi.SetNeeded(Self.m_settings.RCSoutput.board);
  end;
end;

procedure TBlkIO.SaveData(ini_tech: TMemIniFile; const section: string);
begin
 inherited SaveData(ini_tech, section);

 if (Self.isRCSoutput) then
  begin
   ini_tech.WriteInteger(section, 'RCSb0', Self.m_settings.RCSoutput.board);
   ini_tech.WriteInteger(section, 'RCSp0', Self.m_settings.RCSoutput.port);
   ini_tech.WriteBool(section, 'RCSn0', Self.m_settings.RCSoutputNeeded);
  end;
 if (Self.isRCSinput) then
  begin
   ini_tech.WriteInteger(section, 'RCSbi', Self.m_settings.RCSinput.board);
   ini_tech.WriteInteger(section, 'RCSpi', Self.m_settings.RCSinput.port);
   ini_tech.WriteBool(section, 'RCSni', Self.m_settings.RCSinputNeeded);
  end;

 ini_tech.WriteBool(section, 'activateOnStart', Self.m_settings.setOutputOnStart);
 if (Self.nullable) then
   ini_tech.WriteInteger(section, 'nullTime', Self.m_settings.nullAfterSec);
end;

procedure TBlkIO.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
 if (Self.m_state.note <> '') then
   ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Enable();
begin
 if ((Self.isRCSoutput) and (Self.RCSoutputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board))) then
   Exit();
 if ((Self.isRCSinput) and (Self.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))) then
   Exit();

 if (Self.isRCSinput) then
   Self.m_state.inputState := TRCSInputState.notYetScanned;

 Self.m_state.enabled := true;
 if (Self.m_settings.setOutputOnStart) then
   Self.Activate();
end;

procedure TBlkIO.Disable();
begin
 Self.m_state.enabled := false;
 Self.m_state.activeOutput := false;
 Self.m_state.inputState := TRCSInputState.isOff;
end;

function TBlkIO.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := (((Self.isRCSoutput) and (portType = TRCSIOType.output) and (Self.m_settings.RCSoutput = addr)) or
            ((Self.isRCSinput) and (portType = TRCSIOType.input) and (Self.m_settings.RCSinput = addr)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.GetSettings(): TBlkIOSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkIO.SetSettings(data: TBlkIOSettings);
begin
 Self.m_settings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Update();
var inputState: TRCSInputState;
begin
 inherited;

 if ((not Self.enabled) and
     (((not Self.isRCSoutput) or (RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board))) and
      ((not Self.isRCSinput) or (RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))))) then
  begin
   Self.Enable();
   Self.Change();
  end;
 if ((Self.enabled) and
     (((Self.isRCSoutput) and (Self.m_settings.RCSoutputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSoutput.board))) or
      ((Self.isRCSinput) and (Self.m_settings.RCSinputNeeded) and (not RCSi.IsNonFailedModule(Self.m_settings.RCSinput.board))))) then
  begin
   Self.Disable();
   Self.Change(true);
  end;

 if ((Self.enabled) and (Self.isRCSinput)) then
  begin
   try
     inputState := RCSi.GetInput(Self.m_settings.RCSinput);
   except
     on E: RCSException do
       inputState := TRCSInputState.failure;
   end;

   if (inputState <> Self.m_state.inputState) then
    begin
     Self.m_state.inputState := inputState;
     Self.Change();
    end;
  end;

 if ((Self.enabled) and (Self.nullable) and (Self.activeOutput) and (Now > Self.m_state.nullTime)) then
   Self.Deactivate();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.Activate();
begin
 if (Self.activeOutput) then Exit();
 Self.m_state.activeOutput := true;

 if (Self.isRCSoutput) then
  begin
   try
     RCSi.SetOutput(Self.m_settings.RCSoutput, 1);
   except

   end;
  end;

 if (Self.nullable) then
   Self.m_state.nullTime := Now +
      EncodeTime(0, Self.m_settings.nullAfterSec div 60, Self.m_settings.nullAfterSec mod 60, 0);

 Self.Change();
end;

procedure TBlkIO.Deactivate();
begin
 if (not Self.activeOutput) then Exit();
 Self.m_state.activeOutput := false;

 if (Self.isRCSoutput) then
  begin
   try
     RCSi.SetOutput(Self.m_settings.RCSoutput, 0);
   except

   end;
  end;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.PanelClick(SenderPnl: TIdContext; SenderOR: TObject;
    Button: TPanelButton; rights: TAreaRights; params: string = '');
begin
 if (Button = TPanelButton.F2) then
   PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

 if (Button = TPanelButton.ENTER) then begin
   if (Self.enabled) then
    begin
     try
       Self.Activate();
     except
       PanelServer.BottomError(SenderPnl, 'Nepodaøilo se aktivovat blok', TArea(SenderOR).shortName, 'TECHNOLOGIE');
     end
    end else
     PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end else if (Button = TPanelButton.ESCAPE) then begin
   if (Self.enabled) then
    begin
     try
       Self.Deactivate();
     except
       PanelServer.BottomError(SenderPnl, 'Nepodaøilo se deaktivovat blok', TArea(SenderOR).shortName, 'TECHNOLOGIE');
     end
    end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.PanelStateString(): string;
var fg, bg: TColor;
begin
 Result := inherited;

 bg := clBlack;
 if (Self.note <> '') then bg := clTeal;

 if (not Self.enabled) then
   fg := clFuchsia
 else if (Self.activeOutput) then
   fg := clYellow
 else if (Self.isRCSinput) then begin
   case (Self.m_state.inputState) of
     TRCSInputState.isOff: fg := $A0A0A0;
     TRCSInputState.isOn: fg := clLime;
   else
     fg := clFuchsia;
   end;
 end else
   fg := $A0A0A0;

 Result := Result + ownConvert.ColorToStr(fg) + ';';
 Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkIO.GetPtData(json: TJsonObject; includeState: Boolean);
begin
 inherited;

 if (Self.isRCSoutput) then
   TBlk.RCStoJSON(Self.m_settings.RCSoutput, json['rcs']['output']);
 if (Self.isRCSinput) then
   TBlk.RCStoJSON(Self.m_settings.RCSinput, json['rcs']['input']);

 json['setOutputOnStart'] := Self.m_settings.setOutputOnStart;
 if (includeState) then
   Self.GetPtState(json['blockState']);
 json['nullable'] := Self.nullable;
 if (Self.nullable) then
   json['nullTime'] := Self.m_settings.nullAfterSec;
end;

procedure TBlkIO.GetPtState(json: TJsonObject);
begin
 json['enabled'] := Self.m_state.enabled;
 json['activeOutput'] := Self.activeOutput;
 json['activeInput'] := Self.activeInput;
end;

procedure TBlkIO.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
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
     Self.m_state.inputState := TRCSInputState.isOn;
     Self.Change();
    end else if ((not reqJson.B['activeInput']) and (Self.activeInput)) then begin
     Self.m_state.inputState := TRCSInputState.isOff;
     Self.Change();
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.IsNullable(): Boolean;
begin
 Result := (Self.m_settings.nullAfterSec > 0);
end;

function TBlkIO.IsActiveInput(): Boolean;
begin
 Result := (Self.m_state.inputState = TRCSInputState.isOn);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkIO.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
 Result := inherited;
 if (Self.enabled) then
  begin
   if (Self.activeOutput) then
     Result := Result + 'AKTIV<,'
   else
     Result := Result + 'AKTIV>,';
  end;
 Result := Result + 'STIT,';

 if ((RCSi.simulation) and (Self.isRCSinput)) then
  begin
   if (Self.IsActiveInput) then
     Result := Result + '-,*IN<,'
   else
     Result := Result + '-,*IN>,'
  end;
end;

procedure TBlkIO.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV>') then Self.MenuAktivOnClick(SenderPnl, SenderOR)
 else if (item = 'AKTIV<') then Self.MenuAktivOffClick(SenderPnl, SenderOR)
 else if (item = 'IN<') then Self.MenuInClick(SenderPnl, SenderOR, false)
 else if (item = 'IN>') then Self.MenuInClick(SenderPnl, SenderOR, true);
end;

procedure TBlkIO.SetNote(note: string);
begin
 Self.m_state.note := note;
 Self.Change();
end;

procedure TBlkIO.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 PanelServer.Note(SenderPnl, Self, Self.note);
end;

procedure TBlkIO.MenuAktivOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.Activate();
end;

procedure TBlkIO.MenuAktivOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.Deactivate();
end;

procedure TBlkIO.MenuInClick(SenderPnl: TIdContext; SenderOR: TObject; target: Boolean);
begin
 try
   RCSi.SetInput(Self.m_settings.RCSinput, ownConvert.BoolToInt(target));
 except
   PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupù!', TArea(SenderOR).shortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
