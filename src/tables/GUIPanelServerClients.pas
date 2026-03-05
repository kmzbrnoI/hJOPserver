unit GUIPanelServerClients;

// Table of PanelServer clients in F_Main.

interface

uses ComCtrls, SysUtils, StrUtils, Classes, TCPServerPanel;

type
  TPanelServerClientsGUI = class
  public const
    _LV_CLIENTS_COL_STATE = 0;
    _LV_CLIENTS_COL_CLIENT = 1;
    _LV_CLIENTS_COL_PROTOCOL = 2;
    _LV_CLIENTS_COL_APP = 3;
    _LV_CLIENTS_COL_PING = 4;
    _LV_CLIENTS_COL_OR1 = 5;
    _LV_CLIENTS_COL_OR2 = 6;
    _LV_CLIENTS_COL_OR3 = 7;
    _LV_CLIENTS_COL_OR_NEXT = 8;
    _LV_CLIENTS_COL_DCC = 9;
    _LV_CLIENTS_COL_SPECIFIC_APPS = 10;

  private
    LV: TListView;
    refreshQueue: array [0 .. _MAX_CLIENTS - 1] of Boolean;

  public
    constructor Create(LV: TListView);

    procedure GUIInitTable();
    procedure GUIRefreshLine(index: Integer; repaint: Boolean = true);
    procedure GUIRefreshSpecificApps(line: Integer);
    procedure GUIQueueLineToRefresh(lineindex: Integer);
    procedure GUIRefreshTable();
    procedure GUIRefreshFromQueue();

  end;

var
  PanelServerClientsGUI: TPanelServerClientsGUI;

implementation

uses PanelConnData, Area, User, TRailVehicle;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelServerClientsGUI.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServerClientsGUI.GUIInitTable();
begin
  LV.Clear();
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
  begin
    var li: TListItem := LV.Items.Add();
    li.Caption := IntToStr(i);
    li.SubItems.Add('odpojen');
    for var j: Integer := 1 to LV.Columns.Count - 1 do
      li.SubItems.Add('');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelServerClientsGUI.GUIRefreshLine(index: Integer; repaint: Boolean = true);
begin
  if (not Assigned(LV.items[index])) then
    Exit();

  if (not Assigned(PanelServer.clients[index])) then
  begin
    // klient neexistuje
    LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'odpojen';
    for var i: Integer := 1 to LV.Columns.Count - 1 do
      LV.items[index].SubItems[i] := '';

    Exit();
  end;

  if (not Assigned(PanelServer.clients[index].connection)) then
  begin
    LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'soket nenalezen';
    for var i: Integer := 1 to LV.Columns.Count - 1 do
      LV.items[index].SubItems[i] := '';
  end;

  var connData: TPanelConnData := (PanelServer.clients[index].connection.data as TPanelConnData);

  case (PanelServer.clients[index].state) of
    TPanelConnectionState.closed:
      LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'uzavøeno';
    TPanelConnectionState.opening:
      LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'otevírání';
    TPanelConnectionState.handshake:
      LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'handshake';
    TPanelConnectionState.opened:
      LV.items[index].SubItems[_LV_CLIENTS_COL_STATE] := 'otevøeno';
  end;

  LV.items[index].SubItems[_LV_CLIENTS_COL_CLIENT] :=
    PanelServer.clients[index].connection.connection.Socket.Binding.PeerIP;
  if (connData.ping_unreachable) then
    LV.items[index].SubItems[_LV_CLIENTS_COL_PING] := 'unreachable'
  else if (connData.PingComputed()) then
  begin
    var Hour, Min, Sec, MSec: Word;
    DecodeTime(connData.ping, Hour, Min, Sec, MSec);
    LV.items[index].SubItems[_LV_CLIENTS_COL_PING] := IntToStr(MSec);
  end
  else
    LV.items[index].SubItems[_LV_CLIENTS_COL_PING] := '?';

  for var i: Integer := 0 to 2 do
  begin
    if (i < connData.areas.Count) then
    begin
      // klient existuje
      var ORpanel: TAreaPanel;
      connData.areas[i].GetORPanel(PanelServer.clients[index].connection, ORPanel);
      LV.items[index].SubItems[_LV_CLIENTS_COL_OR1 + i] := connData.areas[i].ShortName + ' (' + ORPanel.User
        + ' :: ' + TArea.GetRightsString(ORPanel.Rights) + ')';
    end else begin
      // klient neexistuje
      LV.items[index].SubItems[_LV_CLIENTS_COL_OR1 + i] := '';
    end;
  end;

  if (connData.areas.Count > 3) then
  begin
    var str: string := '';
    for var i: Integer := 3 to connData.areas.Count - 1 do
    begin
      var ORpanel: TAreaPanel;
      connData.areas[i].GetORPanel(PanelServer.clients[index].connection, ORPanel);
      str := str + connData.areas[i].ShortName + ' (' + ORPanel.User + ' :: ' + TArea.GetRightsString(ORPanel.Rights) +
        ')' + ', ';
    end;
    LV.items[index].SubItems[_LV_CLIENTS_COL_OR_NEXT] := LeftStr(str, Length(str) - 2);
  end;

  LV.items[index].SubItems[_LV_CLIENTS_COL_DCC] := IfThen(PanelServer.DCCStopped = PanelServer.clients[index].connection, 'ano', '');

  Self.GUIRefreshSpecificApps(index);
  LV.UpdateItems(index, index);
end;

procedure TPanelServerClientsGUI.GUIRefreshSpecificApps(line: Integer);
begin
  var content: string := '';
  var panelConnData: TPanelConnData := TPanelConnData(PanelServer.clients[line].connection.Data);

  if (panelConnData.regulator) then
  begin
    var str: string := 'reg: ';
    if (Assigned(panelConnData.regulator_user)) then
      str := str + TUser(panelConnData.regulator_user).username
    else
      str := str + 'ano';

    if (panelConnData.regulator_vehicles.Count > 0) then
    begin
      str := str + ': ';
      for var vehicle: TRV in panelConnData.regulator_vehicles do
        str := str + IntToStr(vehicle.addr) + ', ';
      str := LeftStr(str, Length(str) - 2);
    end;

    content := content + str + ' ';
  end;

  if (panelConnData.st_hlaseni.Count > 0) then
  begin
    content := content + 'sh: ';
    for var area: TArea in panelConnData.st_hlaseni do
      content := content + area.ShortName + ', ';
    content := LeftStr(content, Length(content) - 2);
  end;

  LV.items[line].SubItems[_LV_CLIENTS_COL_SPECIFIC_APPS] := content;
end;

procedure TPanelServerClientsGUI.GUIRefreshTable();
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
    Self.GUIRefreshLine(i, false);
  LV.Repaint();
end;

procedure TPanelServerClientsGUI.GUIRefreshFromQueue();
begin
  for var i: Integer := 0 to _MAX_CLIENTS - 1 do
  begin
    if (Self.refreshQueue[i]) then
    begin
      Self.GUIRefreshLine(i);
      Self.refreshQueue[i] := false;
    end;
  end;
end;

procedure TPanelServerClientsGUI.GUIQueueLineToRefresh(lineindex: Integer);
begin
  Self.refreshQueue[lineindex] := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

PanelServerClientsGUI.Free();

end.
