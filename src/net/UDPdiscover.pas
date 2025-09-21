unit UDPdiscover;

{
  TUDPDiscover odpovida na UDP discover pozadavky a informauje zarizeni v siti
  o existenci hJOPserveru. TUDPDiscover posila data na broadcast, predpoklada
  se, ze prijmuta budou taky z broadcastu, ale to neni nutnou podminkou.
}

{
  Format informacni zpravy je totozny pro pozadavek i pro odpoved:
    textovy retezec ukonceny znaky #13#10 (novy radek) obsahujici informace
    oddelene znakem ";":

  Verze protokolu 1.0:
    "hJOP";verze_protokolu;typ_zarizeni;server_nazev;server_ip;server_port;
    server_status;server_popis

  \typ_zarizeni:
    a) "server"
    b) "panel"
    c) "regulator"

  \server_status
    a) off
    b) on
}

{
  hJOPserver odpovida na pozadavky, ktere \typ_zarizeni != "server",
  tedy napriklad na pozadavek "hJOP;1.0;regulator;;192.168.5.13;" od regualtoru.
}

interface

uses IdUDPServer, Classes, IdSocketHandle, SysUtils, Generics.Collections,
  ExtCtrls, IdGlobal, IniFiles;

const
  _DISC_DEFAULT_PORT = 5880;
  _DISC_PROTOCOL_VERSION = '1.0';
  _DISC_REPEAT = 2;
  _DISC_PORT_RANGE = 2;
  _CONFIG_SECTION = 'PanelServer';

type
  TDiscBind = record
    broadcastAddr: string;
    panelServerPort: Word;
  end;

  TUDPDiscover = class
  private
    UDPserver: TIdUDPServer;
    mPort: Word;
    mName, mDescription: string;
    mEnabled: Boolean;
    parsed: TStrings;
    binds: TDictionary<string, TDiscBind>;
    updateBindTimer: TTimer;

    procedure OnUDPServerRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure SendDisc(ABinding: TIdSocketHandle; port: Word);

    procedure SetName(name: string);
    procedure SetDescription(desc: string);
    procedure UpdateBindings();
    procedure OnUpdateBindTimer(Sender: TObject);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure LoadConfig(ini: TMemIniFile);
    procedure SaveConfig(ini: TMemIniFile);
    procedure SendDiscover();

    property enabled: Boolean read mEnabled write mEnabled;
    property port: Word read mPort write mPort;
    property name: string read mName write SetName;
    property description: string read mDescription write SetDescription;

  end;

var
  UDPdisc: TUDPDiscover;

implementation

uses TCPServerPanel, ownStrUtils, Logging, USock, IfThenElse, StrUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TUDPDiscover.Create();
begin
  inherited Create();

  Self.updateBindTimer := TTimer.Create(nil);
  Self.updateBindTimer.Interval := 60000;
  Self.updateBindTimer.OnTimer := Self.OnUpdateBindTimer;
  Self.updateBindTimer.Enabled := true;

  Self.binds := TDictionary<string, TDiscBind>.Create();
  Self.parsed := TStringList.Create();

  Self.mPort := _DISC_DEFAULT_PORT;

  try
    Self.UDPserver := TIdUDPServer.Create(nil);
    Self.UDPserver.OnUDPRead := Self.OnUDPServerRead;
  except
    on E: Exception do
      Log('Nelze vytvorit discover UDPserver : ' + E.Message, llError, lsUDPDiscover);
  end;
end;

destructor TUDPDiscover.Destroy();
begin
  Self.UDPserver.Free();
  Self.parsed.Free();
  Self.binds.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.LoadConfig(ini: TMemIniFile);
begin
  Self.mEnabled := ini.ReadBool(_CONFIG_SECTION, 'UDPDiscover', true);
  Self.mName := ini.ReadString(_CONFIG_SECTION, 'nazev', 'hJOPserver');
  Self.mDescription := ini.ReadString(_CONFIG_SECTION, 'popis', 'Moje kolejištì');

  if (Self.enabled) then
    Self.UpdateBindings(); // will also start server
end;

procedure TUDPDiscover.SaveConfig(ini: TMemIniFile);
begin
  ini.WriteBool(_CONFIG_SECTION, 'UDPDiscover', Self.enabled);
  ini.WriteString(_CONFIG_SECTION, 'nazev', Self.name);
  ini.WriteString(_CONFIG_SECTION, 'popis', Self.description);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.OnUDPServerRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  Msg: String;
begin
  try
    Msg := TEncoding.UTF8.GetString(AData);
    Self.parsed.Clear();
    ExtractStringsEx([';'], [], Msg, parsed);

    if (parsed[2] <> 'server') then
    begin
      for var i: Integer := 0 to _DISC_PORT_RANGE - 1 do
        for var j: Integer := 0 to _DISC_REPEAT - 1 do
          Self.SendDisc(ABinding, Self.port + i);
    end;
  except
    on E: Exception do
      Log('Vyjimka TUDPDiscover.OnUDPServerRead : ' + E.Message, llError, lsUDPDiscover);
  end;
end;
/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SendDisc(ABinding: TIdSocketHandle; port: Word);
var msg: string;
begin
  if (ABinding.IP = '0.0.0.0') then
    Exit();
  if (not Self.binds.ContainsKey(ABinding.IP)) then
    Self.UpdateBindings();

  msg :=
    'hJOP;' +
    _DISC_PROTOCOL_VERSION + ';'+
    'server;' +
    Self.name + ';' +
    ABinding.IP + ';' +
    IntToStr(Self.binds[ABinding.IP].panelServerPort) + ';' +
    ite(PanelServer.openned, 'on', 'off') + ';' +
    Self.description + ';';

  try
    var data: TIdBytes := TIdBytes(TEncoding.UTF8.GetBytes(Msg));
    ABinding.Broadcast(data, port, Self.binds[ABinding.IP].broadcastAddr);
  except
    on E: Exception do
      Log('Vyjimka TUDPDiscover.SendDisc : ' + E.Message, llError, lsUDPDiscover);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SetName(name: string);
begin
  if (Self.mName = name) then
    Exit();
  Self.mName := name;
  if (Self.enabled) then
    Self.SendDiscover();
end;

procedure TUDPDiscover.SetDescription(desc: string);
begin
  if (Self.mDescription = desc) then
    Exit();
  Self.mDescription := desc;
  if (Self.enabled) then
    Self.SendDiscover();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.UpdateBindings();
var
  ifaces: tNetworkInterfaceList;
begin
  try
    GetNetworkInterfaces(ifaces);

    var wasActive: Boolean := Self.UDPserver.Active;
    Self.UDPserver.Active := false;
    Self.UDPserver.Bindings.Clear();
    Self.binds.Clear();

    var bindStr: string := '';
    for var i: Integer := 0 to Length(ifaces) - 1 do
    begin
      if ((ifaces[i].IsLoopback) or (not ifaces[i].IsInterfaceUp) or (not ifaces[i].BroadcastSupport)) then
        continue;
      var panelServerBind: TIdSocketHandle := PanelServer.GetBindOrZeroBind(ifaces[i].AddrIP);
      if (panelServerBind = nil) then // do not try to bind everything if interfaces are marked explicitly
        continue;

      var binding: TIdSocketHandle := Self.UDPserver.Bindings.Add();
      binding.Port := port;
      binding.IP := ifaces[i].AddrIP;

      var discBind: TDiscBind;
      discBind.broadcastAddr := ifaces[i].AddrDirectedBroadcast;
      discBind.panelServerPort := panelServerBind.Port;
      Self.binds.Add(binding.IP, discBind);

      bindStr := bindStr + binding.IP + ':' + IntToStr(port) +
        ' (' + panelServerBind.IP + ':' + IntToStr(discBind.panelServerPort) + ' b:' + discBind.broadcastAddr + '), ';
    end;
    bindStr := LeftStr(bindStr, Length(bindStr)-2);

    if (not wasActive) then
      Log('Zapínám UDP Discover server '+bindStr+' - '+Self.name+', '+Self.description+' ...', llInfo, lsUDPDiscover);
    Self.UDPserver.Active := true;
    if (not wasActive) then
      log('UDP Discover server zapnut.', llInfo, lsUDPDiscover);
  except
    on E: Exception do
      Log('Vyjimka TUDPDiscover.UpdateBindings : ' + E.Message, llError, lsUDPDiscover);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SendDiscover();
begin
  if (not Self.enabled) then
    Exit();

  Self.UpdateBindings();

  for var i: Integer := 0 to Self.UDPserver.Bindings.Count - 1 do
    for var j: Integer := 0 to _DISC_PORT_RANGE - 1 do
      for var k: Integer := 0 to _DISC_REPEAT - 1 do
        Self.SendDisc(Self.UDPserver.Bindings.Items[i], Self.port + j);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.OnUpdateBindTimer(Sender: TObject);
begin
  if (Self.enabled) then
    Self.UpdateBindings();
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization
  UDPdisc := TUDPDiscover.Create();

finalization
  FreeAndNIl(UDPdisc);

end.
