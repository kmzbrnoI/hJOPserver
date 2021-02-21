unit announcement;

{ TStationAnnouncement class implements logic of a station announcement in an
  area. }

interface

uses Generics.Collections, IdContext, Classes, SysUtils;

const
  _ANN_TRAINTYPE_FORBIDDEN: array [0 .. 5] of string = ('Pn', 'Mn', 'Vn', 'Lv', 'Vle', 'Slu');

type
  TAvailableEvent = procedure(Sender: TObject; available: Boolean) of object;

  TAnnTrain = record
    name: string;
    typ: string;
    track: string;
    fromAreaId: string;
    toAreaId: string;
    timeArrive: TTime;
    timeDepart: TTime;
  end;

  TStationAnnouncement = class
  private
    m_clients: TList<TIdContext>;
    m_orid: string;

    procedure BroadcastData(data: string);
    function TrainToStr(train: TAnnTrain): string;
    function GetAvailable(): Boolean;

  public
    OnAvailable: TAvailableEvent;

    constructor Create(orid: string);
    destructor Destroy(); override;

    procedure Parse(Sender: TIdContext; SenderOR: TObject; parsed: TStrings);
    procedure Reset();
    procedure ClientDisconnect(Client: TIdContext);

    procedure Arrival(train: TAnnTrain);
    procedure Departure(train: TAnnTrain);
    procedure Transit(train: TAnnTrain);
    procedure Special(id: string);

    class function AnnounceTrainType(typ: string): Boolean;

    property available: Boolean read GetAvailable;

  end;

implementation

uses TCPServerPanel, Area, TCPAreasRef;

/// /////////////////////////////////////////////////////////////////////////////

constructor TStationAnnouncement.Create(orid: string);
begin
  inherited Create();
  Self.m_orid := orid;
  Self.m_clients := TList<TIdContext>.Create();
  Self.OnAvailable := nil;
end;

destructor TStationAnnouncement.Destroy();
begin
  Self.m_clients.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TStationAnnouncement.Parse(Sender: TIdContext; SenderOR: TObject; parsed: TStrings);
begin
  if (parsed.Count < 2) then
    Exit();
  parsed[2] := UpperCase(parsed[2]);

  if (parsed[2] = 'REGISTER') then
  begin
    if (not TPanelConnData(Sender.data).st_hlaseni.Contains(TArea(SenderOR))) then
    begin
      TPanelConnData(Sender.data).st_hlaseni.Add(TArea(SenderOR));
      PanelServer.GUIQueueLineToRefresh(TPanelConnData(Sender.data).index);
    end;

    if (Self.m_clients.Contains(Sender)) then
    begin
      PanelServer.SendLn(Sender, parsed[0] + ';SH;REGISTER-RESPONSE;ERR;ALREADY_REGISTERED');
      Exit();
    end;

    Self.m_clients.Add(Sender);
    if ((Self.m_clients.Count = 1) and (Assigned(Self.OnAvailable))) then
      Self.OnAvailable(Self, true);

    PanelServer.SendLn(Sender, parsed[0] + ';SH;REGISTER-RESPONSE;OK');

  end else if (parsed[2] = 'UNREGISTER') then
  begin
    if (TPanelConnData(Sender.data).st_hlaseni.Contains(TArea(SenderOR))) then
    begin
      TPanelConnData(Sender.data).st_hlaseni.Remove(TArea(SenderOR));
      PanelServer.GUIQueueLineToRefresh(TPanelConnData(Sender.data).index);
    end;

    if (not Self.m_clients.Contains(Sender)) then
    begin
      PanelServer.SendLn(Sender, parsed[0] + ';SH;UNREGISTER-RESPONSE;ERR;NOT_REGISTERED');
      Exit();
    end;

    Self.m_clients.Remove(Sender);
    if ((Self.m_clients.Count = 0) and (Assigned(Self.OnAvailable))) then
      Self.OnAvailable(Self, false);

    PanelServer.SendLn(Sender, parsed[0] + ';SH;UNREGISTER-RESPONSE;OK');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStationAnnouncement.Reset();
begin
  if (Self.m_clients.Count > 0) then
  begin
    Self.m_clients.Clear();
    if (Assigned(Self.OnAvailable)) then
      Self.OnAvailable(Self, false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStationAnnouncement.ClientDisconnect(Client: TIdContext);
begin
  if (Self.m_clients.Contains(Client)) then
  begin
    Self.m_clients.Remove(Client);
    if (Self.m_clients.Count = 0) then
      Self.OnAvailable(Self, false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStationAnnouncement.Arrival(train: TAnnTrain);
begin
  Self.BroadcastData('PRIJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStationAnnouncement.Departure(train: TAnnTrain);
begin
  Self.BroadcastData('ODJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStationAnnouncement.Transit(train: TAnnTrain);
begin
  Self.BroadcastData('PROJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStationAnnouncement.Special(id: string);
begin
  Self.BroadcastData('SPEC;' + id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStationAnnouncement.BroadcastData(data: string);
begin
  for var client: TIdContext in Self.m_clients do
    PanelServer.SendLn(client, Self.m_orid + ';SH;' + data);
end;

////////////////////////////////////////////////////////////////////////////////

function TStationAnnouncement.TrainToStr(train: TAnnTrain): string;
begin
  Result := train.name + ';' + train.typ + ';' + train.track + ';' + train.fromAreaId + ';' + train.toAreaId + ';';

  if (train.timeArrive <> 0) then
    Result := Result + FormatDateTime('hh:nn', train.timeArrive);

  Result := Result + ';';
  if (train.timeDepart <> 0) then
    Result := Result + FormatDateTime('hh:nn', train.timeDepart);
end;

////////////////////////////////////////////////////////////////////////////////

function TStationAnnouncement.GetAvailable(): Boolean;
begin
  Result := (Self.m_clients.Count > 0);
end;

////////////////////////////////////////////////////////////////////////////////

class function TStationAnnouncement.AnnounceTrainType(typ: string): Boolean;
begin
  for var s: string in announcement._ANN_TRAINTYPE_FORBIDDEN do
    if (typ = s) then
      Exit(false);
  Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

end.
