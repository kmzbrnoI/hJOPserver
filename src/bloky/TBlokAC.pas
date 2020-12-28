unit TBlokAC;

// definice a obsluha technologickeho bloku AC

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, IdContext,
     Generics.Collections, TOblRizeni, Graphics;

type

 TACState = (stopped = 0, running = 1, paused = 2);

 TBlkACSettings = record
   accessToken: string;
 end;

 TBlkACState = record
  enabled: boolean;
  client: TIdContext;
  state: TACState;
  lines: TStrings;
  fg: TColor;
  panelsShowingState: TList<TIdContext>;
 end;

 TBlkACException = class(Exception);

 // zamek ma zaver, pokud jakakoliv vyhybka, kterou obsluhuje, ma zaver

 TBlkAC = class(TBlk)
  const
   //defaultni stav
   _def_ac_state:TBlkACState = (
     enabled: false;
     client: nil;
     state: TACState.stopped;
     fg: clFuchsia;
   );

  private
   m_state: TBlkACState;
   m_settings: TBlkACSettings;

    function GetStopped(): Boolean;
    function GetRunning(): Boolean;
    function GetPaused(): Boolean;
    function GetClientConnected(): Boolean;
    function PtUsername(): string;

    procedure SetFgColor(new: TColor);

    procedure SendLn(text: string); overload;
    procedure SendLn(recipient: TIdContext; text: string); overload;

    procedure MenuSTARTClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTOPClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPAUZAClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPOKRACClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTAVClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PanelShowState(pnl: TIdContext; OblR: TOR);
    procedure PanelSTAVClosed(Sender: TIdContext; success: Boolean);

  public

    constructor Create(index: Integer);
    destructor Destroy(); override;

    // load/save data
    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel,ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    // enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;

    function GetSettings(): TBlkACSettings;
    procedure SetSettings(data: TBlkACSettings);

    // ----- AC own functions -----

    procedure Start();
    procedure Stop();
    procedure Pause();

    procedure SendClientControl();
    procedure OnClientDisconnect(client: TIdContext);
    procedure ClientParse(Sender: TIdContext; parsed: TStrings);

    property state: TBlkACState read m_state;
    property enabled: Boolean read m_state.enabled;
    property stopped: Boolean read GetStopped;
    property running: Boolean read GetRunning;
    property paused: Boolean read GetPaused;
    property acState: TACState read m_state.state;
    property clientConnected: Boolean read GetClientConnected;
    property client: TIdContext read m_state.client;

    // GUI:
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton;
                         rights: TORCOntrolRights; params: string = ''); override;
    function PanelStateString():string; override;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, ownConvert, Diagnostics,
    TJCDatabase, fMain, TCPServerOR, TrainDb, THVDatabase, TBlokVyhybka,
    TCPServerPT, ownStrUtils;

constructor TBlkAC.Create(index: Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := btAC;
 Self.m_state := _def_ac_state;
 Self.m_state.lines := TStringList.Create();
 Self.m_state.panelsShowingState := TList<TIdContext>.Create();
end;

destructor TBlkAC.Destroy();
begin
 Self.m_state.lines.Free();
 Self.m_state.panelsShowingState.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.m_settings.accessToken := ini_tech.ReadString(section, 'accessToken', '');
 Self.LoadORs(ini_rel, 'POM').Free();
end;

procedure TBlkAC.SaveData(ini_tech: TMemIniFile; const section: string);
begin
 inherited SaveData(ini_tech, section);
 ini_tech.WriteString(section, 'accessToken', Self.m_settings.accessToken);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Enable();
begin
 Self.m_state.enabled := true;
 inherited Change();
end;

procedure TBlkAC.Disable();
begin
 try
   if (Self.running) then
     Self.Stop();
 except

 end;

 Self.m_state.enabled := false;
 Self.m_state.lines.Clear();
 Self.m_state.panelsShowingState.Clear();
 Self.Change(true);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.GetSettings(): TBlkACSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkAC.SetSettings(data: TBlkACSettings);
begin
 Self.m_settings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.MenuSTARTClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   Self.Start();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuSTOPClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   Self.Stop();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuPAUZAClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   Self.Pause();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuPOKRACClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if (not Self.paused) then
  begin
   ORTCPServer.BottomError(SenderPnl, 'AC není v režimu pauza!', TOR(SenderOR).ShortName, 'AC');
   Exit();
  end;

 try
   Self.Start();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuSTAVClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.PanelShowState(SenderPnl, SenderOR as TOR);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights):string;
begin
 Result := inherited;

 if ((Self.m_state.client <> nil) and (Self.stopped)) then
   Result := Result + 'START,';

 if (Self.paused) then
   Result := Result + 'POKRAÈ,';
 if (Self.running) then
   Result := Result + 'PAUZA,';
 if (not Self.stopped) then
   Result := Result + 'STOP,';

 Result := Result + 'STAV?,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton;
                            rights: TORCOntrolRights; params: string = '');
begin
 if (Self.enabled) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkAC.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
 if (not Self.enabled) then Exit();

 if      (item = 'START')  then Self.MenuSTARTClick(SenderPnl, SenderOR)
 else if (item = 'STOP')   then Self.MenuSTOPClick(SenderPnl, SenderOR)
 else if (item = 'PAUZA')  then Self.MenuPAUZAClick(SenderPnl, SenderOR)
 else if (item = 'POKRAÈ') then Self.MenuPOKRACClick(SenderPnl, SenderOR)
 else if (item = 'STAV?')  then Self.MenuSTAVClick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.PanelStateString(): string;
var fg, bg: TColor;
    flash: boolean;
begin
 Result := inherited;

 bg := clBlack;
 fg := $A0A0A0;

 case (Self.m_state.state) of
  TACState.running, TACState.paused: fg := Self.m_state.fg;
 end;

 if (Self.stopped) then
  begin
   if (Self.clientConnected) then
     fg := $A0A0A0
   else
     fg := clFuchsia;
  end;

 flash := Self.running;

 Result := Result + ownConvert.ColorToStr(fg) + ';'
                  + ownConvert.ColorToStr(bg) + ';'
                  + IntToStr(ownConvert.BoolToInt(flash)) + ';';
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.GetStopped(): Boolean;
begin
 Result := (Self.m_state.state = TACState.stopped);
end;

function TBlkAC.GetRunning(): Boolean;
begin
 Result := (Self.m_state.state = TACState.running);
end;

function TBlkAC.GetPaused(): Boolean;
begin
 Result := (Self.m_state.state = TACState.paused);
end;

function TBlkAC.GetClientConnected(): Boolean;
begin
 Result := (Self.m_state.client <> nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Start();
begin
 if ((Self.m_state.state <> TACState.stopped) and (Self.m_state.state <> TACState.paused)) then
   TBlkACException.Create('Nelze spustit bìžící AC!');
 if (Self.m_state.client = nil) then
   TBlkACException.Create('AC nelze spustit bez pøipojeného klienta!');

 if (not Self.paused) then
  begin
   Self.m_state.fg := clYellow;
   Self.m_state.lines.Clear();
  end;

 try
   PtServer.AccessTokenAdd(Self.PtUsername(), Self.m_settings.accessToken);
 except
   raise TBlkACException.Create('Nepodaøilo se pøidat pøístupový token!');
 end;

 if (Self.paused) then
   ORTCPServer.SendLn(Self.client, '-;AC;'+IntToStr(Self.id)+';CONTROL;RESUME')
 else
   ORTCPServer.SendLn(Self.client, '-;AC;'+IntToStr(Self.id)+';CONTROL;START');

 Self.m_state.state := TACState.running;
 Self.Change();
end;

procedure TBlkAC.Stop();
begin
 if (Self.m_state.state <> TACState.running) then
   TBlkACException.Create('Nelze zastavit nespuštìné AC!');

 try
   PtServer.AccessTokenRemove(Self.PtUsername());
 except

 end;

 Self.m_state.state := TACState.stopped;
 Self.SendClientControl();
 Self.Change();
end;

procedure TBlkAC.Pause();
begin
 if (Self.m_state.state <> TACState.running) then
   TBlkACException.Create('Nelze pozastavit nespuštìné AC!');

 try
   PtServer.AccessTokenRemove(Self.PtUsername());
 except

 end;

 Self.m_state.state := TACState.paused;
 Self.SendClientControl();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.ClientParse(Sender: TIdContext; parsed: TStrings);
var color: TColor;
    oblr: TOR;
    panel: TIdContext;
begin
 if (parsed.Count < 4) then Exit();

 if ((parsed.Count >= 5) and (UpperCase(parsed[3]) = 'LOGIN')) then
  begin
   if (Self.client <> nil) then
    begin
     Self.SendLn(Sender, 'AUTH;nok;1;Klient již pøihlášen');
     Exit();
    end else if (parsed[4] <> Self.m_settings.accessToken) then
     Self.SendLn(Sender, 'AUTH;nok;2;Neplatný pøístupový token')
    else begin
     Self.m_state.client := Sender;
     Self.SendLn(Sender, 'AUTH;ok;');
     Self.Change();
    end;
  end;

 if ((Self.client = nil) or (Self.client <> Sender)) then Exit();

 if (UpperCase(parsed[3]) = 'LOGOUT') then begin
   if (not Self.stopped) then
     Self.Stop();
   Self.SendLn(Sender, 'AUTH;logout;');
   Self.m_state.client := nil;
   Self.Change();
 end else if ((UpperCase(parsed[3]) = 'CONTROL') and (parsed.Count >= 5) and (UpperCase(parsed[4]) = 'DONE')) then begin
   if (Self.running) then
     Self.Stop();
 end else if ((UpperCase(parsed[3]) = 'CONTROL') and (parsed.Count >= 6) and (UpperCase(parsed[4]) = 'STATE')) then begin
   Self.m_state.lines.Clear();
   ExtractStringsEx([','], [], parsed[5], Self.m_state.lines);
   for panel in Self.m_state.panelsShowingState do
     Self.PanelShowState(panel, nil);
 end else if ((UpperCase(parsed[3]) = 'CONTROL') and (parsed.Count >= 6) and (UpperCase(parsed[4]) = 'FG-COLOR')) then begin
   color := ownConvert.StrToColor(parsed[5]);
   if ((color <> clBlack) and (color <> $A0A0A0) and (color <> clFuchsia)) then
     Self.SetFgColor(color);
 end else if ((UpperCase(parsed[3]) = 'CONTROL') and (parsed.Count >= 7) and (UpperCase(parsed[4]) = 'ERROR')) then begin
   if (UpperCase(parsed[5]) = 'DISPBOTTOM') then
     for oblr in Self.ORsRef do
       oblr.BroadcastBottomError(parsed[6], 'AC', TORControlRights.write, Self.name);
 end;
end;

procedure TBlkAC.OnClientDisconnect(client: TIdContext);
begin
 if (Self.client = client) then
  begin
   Self.m_state.client := nil;
   Self.m_state.lines.Clear();
   if (not Self.stopped) then
     Self.Stop();
   Self.Change();
  end;
 if (Self.m_state.panelsShowingState.Contains(client)) then
   Self.m_state.panelsShowingState.Remove(client);
end;

procedure TBlkAC.SendClientControl();
var state: string;
begin
 if (Self.client = nil) then Exit();

 case (Self.m_state.state) of
  TACState.stopped: state := 'STOP';
  TACState.running: state := 'START';
  TACState.paused: state := 'PAUSE';
 else
  state := 'UNKNOWN';
 end;

 ORTCPServer.SendLn(Self.client, '-;AC;'+IntToStr(Self.id)+';CONTROL;'+state);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.SendLn(text: string);
begin
 if (Self.client = nil) then
   raise TBlkACException.Create('Nezle odeslat data neexistujícímu klientovi!');
 Self.SendLn(Self.client, text);
end;

procedure TBlkAC.SendLn(recipient: TIdContext; text: string);
begin
 ORTCPServer.SendLn(recipient, '-;AC;'+IntToStr(Self.id)+';'+text);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.PtUsername(): string;
begin
 Result := IntToStr(Self.id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.PanelShowState(pnl: TIdContext; OblR: TOR);
var podm: TList<TPSPodminka>;
    str: string;
begin
 podm := TList<TPSPodminka>.Create();
 if (Self.clientConnected) then
   podm.Add(TOR.GetPSPodminka('Klient: pøipojen', ''))
 else
   podm.Add(TOR.GetPSPodminka('Klient: nepøipojen', ''));
 case (Self.acState) of
   TACState.stopped: podm.Add(TOR.GetPSPodminka('AC: zastaven', ''));
   TACState.running: podm.Add(TOR.GetPSPodminka('AC: bìží', ''));
   TACState.paused: podm.Add(TOR.GetPSPodminka('AC: pozastaven', ''))
 end;

 for str in Self.m_state.lines do
   podm.Add(TOR.GetPSPodminka(str, ''));

 if (not Self.m_state.panelsShowingState.Contains(pnl)) then
   Self.m_state.panelsShowingState.Add(pnl);
 ORTCPServer.PotvrOrInfo(pnl, 'IS', Self.PanelSTAVClosed, OblR, 'Zobrazení stavu AC',
                         TBlky.GetBlksList(Self), podm)
end;

procedure TBlkAC.PanelSTAVClosed(Sender: TIdContext; success: Boolean);
begin
 Self.m_state.panelsShowingState.Remove(Sender);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.SetFgColor(new: TColor);
begin
 if (Self.m_state.fg = new) then Exit();
 Self.m_state.fg := new;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

