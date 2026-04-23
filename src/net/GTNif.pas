unit GTNif;

// Panel server's interface with GTN
// GTN = Graficko-technologicka nadstavba
// https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer-gtn

interface

uses SysUtils, IdContext, Classes, Generics.Collections;

type

  TGtnClients = class
  private
    m_clients: THashSet<TIdContext>;

    procedure SendLn(AContext: TIdContext; str: string; area: string = '-');

  public
    constructor Create();
    destructor Destroy(); override;

    procedure ParseGTNMsg(conn: TIdContext; parsed: TStrings);
    procedure ClientDisconnected(conn: TIDContext; contextDestroyed: Boolean = false);
    procedure Reset();

    procedure Broadcast(msg: string; area: string = '-');

  end;

var
  gtn: TGtnClients;

implementation

uses PanelConnData, TCPServerPanel, GUIPanelServerClients;

////////////////////////////////////////////////////////////////////////////////

constructor TGtnClients.Create();
begin
  inherited;
  m_clients := THashSet<TIdContext>.Create();
end;

destructor TGtnClients.Destroy();
begin
  m_clients.Free();
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TGtnClients.SendLn(AContext: TIdContext; str: string; area: string);
begin
  PanelServer.SendLn(AContext, area+';GTN;'+str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TGtnClients.ParseGTNMsg(conn: TIdContext; parsed: TStrings);
begin
  if (parsed.Count < 3) then
    Exit();

  if (UpperCase(parsed[2]) = 'REGISTER') then
  begin
    TPanelConnData(conn.Data).gtn := True;
    if (not Self.m_clients.Contains(conn)) then
    begin
      Self.m_clients.Add(conn);
      Self.SendLn(conn, 'REGISTER;OK');
    end else begin
      Self.SendLn(conn, 'REGISTER;OK;GTN klient již zaregistrován!');
    end;
    PanelServerClientsGUI.GUIRefreshSpecificApps((conn.Data as TPanelConnData).index);

  end else if (UpperCase(parsed[2]) = 'UNREGISTER') then
  begin
    TPanelConnData(conn.Data).gtn := False;
    if (Self.m_clients.Contains(conn)) then
    begin
      Self.m_clients.Remove(conn);
      Self.SendLn(conn, 'UNREGISTER;OK');
    end else begin
      Self.SendLn(conn, 'UNREGISTER;ERR;GTN klient není registrován!');
    end;
    PanelServerClientsGUI.GUIRefreshSpecificApps((conn.Data as TPanelConnData).index);

  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TGtnClients.Reset();
begin
  Self.m_clients.Clear();
end;

procedure TGtnClients.ClientDisconnected(conn: TIDContext; contextDestroyed: Boolean);
begin
  if (Self.m_clients.Contains(conn)) then
    Self.m_clients.Remove(conn);
  if (not contextDestroyed) then
    TPanelConnData(conn.Data).gtn := False;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TGtnClients.Broadcast(msg: string; area: string);
begin
  for var client: TIdContext in Self.m_clients do
    Self.SendLn(client, msg, area);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

gtn := TGtnClients.Create();

finalization

FreeAndNil(gtn);

end.
