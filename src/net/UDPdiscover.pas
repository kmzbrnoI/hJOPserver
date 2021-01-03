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
     ExtCtrls;

const
  _DISC_DEFAULT_PORT = 5880;
  _DISC_PROTOCOL_VERSION = '1.0';
  _DISC_REPEAT = 2;
  _DISC_PORT_RANGE = 2;

type
  TUDPDiscover = class
    private
      UDPserver : TIdUDPServer;
      fPort: Word;
      fName, fDescription: string;
      parsed: TStrings;
      broadcasts: TDictionary<string, string>;
      updateBindTimer: TTimer;

       procedure OnUDPServerRead(AThread: TIdUDPListenerThread;
        AData: TBytes; ABinding: TIdSocketHandle);
       procedure SendDisc(ABinding: TIdSocketHandle; port: Word);

       procedure SetName(name: string);
       procedure SetDescription(desc: string);
       procedure UpdateBindings();
       procedure OnUpdateBindTimer(Sender: TObject);

    public
       constructor Create(port: Word; name: string; description: string);
       destructor Destroy(); override;

       procedure SendDiscover();

       property port: Word read fPort write fPort;
       property name: string read fName write SetName;
       property description: string read fDescription write SetDescription;

  end;

var
  UDPdisc : TUDPDiscover;

implementation

uses TCPServerOR, ownStrUtils, Logging, USock;

////////////////////////////////////////////////////////////////////////////////

constructor TUDPDiscover.Create(port: Word; name: string; description: string);
begin
 inherited Create();

 Self.updateBindTimer := TTimer.Create(nil);
 Self.updateBindTimer.Interval := 60000;
 Self.updateBindTimer.OnTimer := Self.OnUpdateBindTimer;
 Self.updateBindTimer.Enabled := true;

 Self.broadcasts := TDictionary<string, string>.Create();
 Self.parsed := TStringList.Create();
 Self.fPort := port;
 Self.fName := name;
 Self.fDescription := description;

 try
   Self.UDPserver := TIdUDPServer.Create(nil);
   Self.UDPserver.OnUDPRead := Self.OnUDPServerRead;
   Self.SendDiscover();
 except
   on E: Exception do
    begin
     writelog('Nelze vytvorit discover UDPserver : '+E.Message, WR_ERROR);
    end;
 end;

end;

destructor TUDPDiscover.Destroy();
begin
 Self.UDPserver.Free();
 Self.parsed.Free();
 Self.broadcasts.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.OnUDPServerRead(AThread: TIdUDPListenerThread;
  AData: TBytes; ABinding: TIdSocketHandle);
var
  Msg: String;
  i, j: Integer;
begin
 try
   msg := TEncoding.UTF8.GetString(AData);
   Self.parsed.Clear();
   ExtractStringsEx([';'], [], msg, parsed);

   if (parsed[2] <> 'server') then
    begin
     for i := 0 to _DISC_PORT_RANGE-1 do
       for j := 0 to _DISC_REPEAT-1 do
         Self.SendDisc(ABinding, Self.port+i);
    end;
 except
  on E: Exception do
    writelog('Vyjimka TUDPDiscover.OnUDPServerRead : '+E.Message, WR_ERROR);
 end;
end;
////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SendDisc(ABinding: TIdSocketHandle; port: Word);
var msg: string;
    data: TBytes;
begin
 if (ABinding.IP = '0.0.0.0') then Exit();

 msg := 'hJOP;' + _DISC_PROTOCOL_VERSION + ';server;' + Self.name + ';' +
        ABinding.IP + ';' + IntToStr(ORTCPServer.port) + ';';

 case (ORTCPServer.openned) of
   false : msg := msg + 'off;';
   true  : msg := msg + 'on;';
 end;

 msg := msg + Self.description + ';';

 if (not broadcasts.ContainsKey(ABinding.IP)) then Self.UpdateBindings();

 try
   data := TEncoding.UTF8.GetBytes(msg);
   ABinding.Broadcast(data, port, broadcasts[ABinding.IP]);
 except
   on E: Exception do
     writelog('Vyjimka TUDPDiscover.SendDisc : '+E.Message, WR_ERROR);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SetName(name: string);
begin
 if (Self.fName = name) then Exit();
 Self.fName := name;
 Self.SendDiscover();
end;

procedure TUDPDiscover.SetDescription(desc: string);
begin
 if (Self.fDescription = desc) then Exit();
 Self.fDescription := desc;
 Self.SendDiscover();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.UpdateBindings();
var i: Integer;
    ifaces: tNetworkInterfaceList;
    binding: TIdSocketHandle;
begin
 try
   GetNetworkInterfaces(ifaces);

   Self.UDPserver.Active := false;
   Self.UDPserver.Bindings.Clear();
   Self.broadcasts.Clear();

   for i := 0 to Length(ifaces)-1 do
    begin
     if ((ifaces[i].IsLoopback) or (not ifaces[i].IsInterfaceUp) or (not ifaces[i].BroadcastSupport)) then continue;

     binding      := Self.UDPserver.Bindings.Add;
     binding.Port := port;
     binding.IP   := ifaces[i].AddrIP;
     Self.broadcasts.Add(binding.IP, ifaces[i].AddrDirectedBroadcast);
    end;

   Self.UDPserver.Active := true;
 except
  on E: Exception do
    writelog('Vyjimka TUDPDiscover.UpdateBindings : '+E.Message, WR_ERROR);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.SendDiscover();
var i, j, k: Integer;
begin
 Self.UpdateBindings();

 for i := 0 to Self.UDPserver.Bindings.Count-1 do
   for j := 0 to _DISC_PORT_RANGE-1 do
     for k := 0 to _DISC_REPEAT-1 do
       Self.SendDisc(Self.UDPserver.Bindings.Items[i], Self.port+j);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUDPDiscover.OnUpdateBindTimer(Sender: TObject);
begin
 Self.UpdateBindings();
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
  UDPdisc.Free();

end.
