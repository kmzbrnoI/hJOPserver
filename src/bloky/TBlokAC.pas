unit TBlokAC;

// definice a obsluha technologickeho bloku AC

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, IdContext,
     Generics.Collections, TOblRizeni;

type

 TACState = (stopped = 0, running = 1, paused = 2);

 TBlkACSettings = record
   accessToken: string;
 end;

 TBlkACState = record
  enabled: boolean;
  client: TIdContext;
  state: TACState;
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
   );

  private
   m_state: TBlkACState;
   m_settings: TBlkACSettings;

    function GetStopped():boolean;
    function GetRunning():boolean;
    function GetPaused():boolean;
    function GetClientConnected():boolean;
    function PtUsername():string;

    procedure SendLn(text: string); overload;
    procedure SendLn(recipient: TIdContext; text: string); overload;

    procedure MenuSTARTClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTOPClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPAUZAClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPOKRACClick(SenderPnl:TIdContext; SenderOR:TObject);

  public

    constructor Create(index:Integer);
    destructor Destroy(); override;

    // load/save data
    procedure LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile; const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile; const section:string); override;

    // enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;

    function GetSettings():TBlkACSettings;
    procedure SetSettings(data: TBlkACSettings);

    // update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    // ----- AC own functions -----

    procedure Start();
    procedure Stop();
    procedure Pause();

    procedure SendClientControl();
    procedure OnClientDisconnect();
    procedure ClientParse(Sender: TIdContext; parsed: TStrings);

    property state:TBlkACState read m_state;
    property enabled:boolean read m_state.enabled;
    property stopped:boolean read GetStopped;
    property running:boolean read GetRunning;
    property paused:boolean read GetPaused;
    property acState:TACState read m_state.state;
    property clientConnected:boolean read GetClientConnected;
    property client:TIdContext read m_state.client;

    // GUI:
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton;
                         rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

 end;//class TBlkUsek

// TODO: remove just overloaded functions

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, Graphics, Prevody, Diagnostics,
    TJCDatabase, fMain, TCPServerOR, SprDb, THVDatabase, TBlokVyhybka,
    TCPServerPT;

constructor TBlkAC.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_AC;
 Self.m_state := _def_ac_state;
end;//ctor

destructor TBlkAC.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.m_settings.accessToken := ini_tech.ReadString(section, 'accessToken', '');

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();
   try
     ExtractStrings([';'],[],PChar(ini_rel.ReadString('AC', IntToStr(Self.GlobalSettings.id), '')),str);
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
end;

procedure TBlkAC.SaveData(ini_tech:TMemIniFile; const section:string);
begin
 inherited SaveData(ini_tech, section);
 ini_tech.WriteString(section, 'accessToken', Self.m_settings.accessToken);
end;

procedure TBlkAC.SaveStatus(ini_stat:TMemIniFile; const section:string);
begin
 // TODO
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Enable();
begin
 inherited Change();
 Self.m_state.enabled := true;
end;

procedure TBlkAC.Disable();
begin
 try
   if (Self.running) then
     Self.Stop();
 except

 end;

 Self.m_state.enabled := false;
 Self.Change(true);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.GetSettings():TBlkACSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkAC.SetSettings(data: TBlkACSettings);
begin
 Self.m_settings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Update();
begin
 inherited Update();
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkAC.Change(now:boolean = false);
begin
 inherited Change(now);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.MenuSTARTClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   Self.Start();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuSTOPClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   Self.Stop();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuPAUZAClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   Self.Pause();
 except
   on E:Exception do
     ORTCPServer.BottomError(SenderPnl, E.Message, TOR(SenderOR).ShortName, 'AC');
 end;
end;

procedure TBlkAC.MenuPOKRACClick(SenderPnl:TIdContext; SenderOR:TObject);
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

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton;
                            rights:TORCOntrolRights; params:string = '');
begin
 if (Self.enabled) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkAC.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (not Self.enabled) then Exit();

 if      (item = 'START')  then Self.MenuSTARTClick(SenderPnl, SenderOR)
 else if (item = 'STOP')   then Self.MenuSTOPClick(SenderPnl, SenderOR)
 else if (item = 'PAUZA')  then Self.MenuPAUZAClick(SenderPnl, SenderOR)
 else if (item = 'POKRAÈ') then Self.MenuPOKRACClick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.PanelStateString():string;
var fg, bg: TColor;
    flash: boolean;
begin
 Result := inherited;

 bg := clBlack;
 fg := $A0A0A0;

 case (Self.m_state.state) of
  TACState.running: fg := clYellow;
  TACState.paused: fg := clYellow;
 end;

 if (Self.stopped) then
  begin
   if (Self.clientConnected) then
     fg := $A0A0A0
   else
     fg := clFuchsia;
  end;

 flash := Self.running;

 Result := Result + PrevodySoustav.ColorToStr(fg) + ';'
                  + PrevodySoustav.ColorToStr(bg) + ';'
                  + IntToStr(PrevodySoustav.BoolToInt(flash)) + ';';
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.GetStopped():boolean;
begin
 Result := (Self.m_state.state = TACState.stopped);
end;

function TBlkAC.GetRunning():boolean;
begin
 Result := (Self.m_state.state = TACState.running);
end;

function TBlkAC.GetPaused():boolean;
begin
 Result := (Self.m_state.state = TACState.paused);
end;

function TBlkAC.GetClientConnected():boolean;
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
 end else if ((UpperCase(parsed[3]) = 'CONTROL') and (parsed.Count >= 7) and (UpperCase(parsed[4]) = 'ERROR')) then begin
   // TODO
 end;
end;

procedure TBlkAC.OnClientDisconnect();
begin
 Self.m_state.client := nil;
 if (not Self.stopped) then
   Self.Stop();

 Self.Change();
end;

procedure TBlkAC.SendClientControl();
var state:string;
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

function TBlkAC.PtUsername():string;
begin
 Result := IntToStr(Self.id);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

