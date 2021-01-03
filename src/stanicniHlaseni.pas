unit stanicniHlaseni;

{
  Trida TStanicniHlaseni se stara o prehravani stanicniho hlaseni jedne oblasti
  rizeni.
}

interface

uses Generics.Collections, IdContext, Classes, SysUtils;

const
  _HLASENI_TRAINTYP_FORBIDDEN: array [0..5] of string = (
    'Pn', 'Mn', 'Vn', 'Lv', 'Vle', 'Slu'
  );

type
  TAvailableEvent = procedure (Sender: TObject; available: Boolean) of object;

  TSHTrain = record
    cislo: string;
    typ: string;
    kolej: string;
    fromORid: string;
    toORid: string;
    timeArrive: TTime;
    timeDepart: TTime;
  end;

  TStanicniHlaseni = class
   private
    m_clients: TList<TIdContext>;
    m_orid: string;

     procedure BroadcastData(data: string);
     function TrainToStr(train: TSHTrain): string;
     function GetAvailable(): Boolean;

   public
    OnAvailable: TAvailableEvent;

     constructor Create(orid: string);
     destructor Destroy(); override;

     procedure Parse(Sender: TIdContext; SenderOR: TObject; parsed: TStrings);
     procedure Reset();
     procedure ClientDisconnect(Client: TIdContext);

     procedure Prijede(train: TSHTrain);
     procedure Odjede(train: TSHTrain);
     procedure Projede(train: TSHTrain);
     procedure Spec(id: string);

     class function HlasitTrainTyp(typ: string): Boolean;

     property available: Boolean read GetAvailable;

end;

implementation

uses TCPServerOR, Area, TCPAreasRef;

////////////////////////////////////////////////////////////////////////////////

constructor TStanicniHlaseni.Create(orid: string);
begin
 inherited Create();
 Self.m_orid := orid;
 Self.m_clients := TList<TIdContext>.Create();
 Self.OnAvailable := nil;
end;

destructor TStanicniHlaseni.Destroy();
begin
 Self.m_clients.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.Parse(Sender: TIdContext; SenderOR: TObject; parsed: TStrings);
begin
 if (parsed.Count < 2) then Exit();
 parsed[2] := UpperCase(parsed[2]);

 if (parsed[2] = 'REGISTER') then
  begin
   if (not TTCPORsRef(Sender.Data).st_hlaseni.Contains(TArea(SenderOR))) then
    begin
     TTCPORsRef(Sender.Data).st_hlaseni.Add(TArea(SenderOR));
     ORTCPServer.GUIQueueLineToRefresh(TTCPORsRef(Sender.Data).index);
    end;

   if (Self.m_clients.Contains(Sender)) then
    begin
     ORTCPServer.SendLn(Sender, parsed[0] + ';SH;REGISTER-RESPONSE;ERR;ALREADY_REGISTERED');
     Exit();
    end;

   Self.m_clients.Add(Sender);
   if ((self.m_clients.Count = 1) and (Assigned(Self.OnAvailable))) then
     Self.OnAvailable(Self, true);

   ORTCPServer.SendLn(Sender, parsed[0] + ';SH;REGISTER-RESPONSE;OK');

  end else if (parsed[2] = 'UNREGISTER') then begin
   if (TTCPORsRef(Sender.Data).st_hlaseni.Contains(TArea(SenderOR))) then
    begin
     TTCPORsRef(Sender.Data).st_hlaseni.Remove(TArea(SenderOR));
     ORTCPServer.GUIQueueLineToRefresh(TTCPORsRef(Sender.Data).index);
    end;

   if (not Self.m_clients.Contains(Sender)) then
    begin
     ORTCPServer.SendLn(Sender, parsed[0] + ';SH;UNREGISTER-RESPONSE;ERR;NOT_REGISTERED');
     Exit();
    end;

   Self.m_clients.Remove(Sender);
   if ((Self.m_clients.Count = 0) and (Assigned(Self.OnAvailable))) then
     Self.OnAvailable(Self, false);

   ORTCPServer.SendLn(Sender, parsed[0] + ';SH;UNREGISTER-RESPONSE;OK');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.Reset();
begin
 if (Self.m_clients.Count > 0) then
  begin
   Self.m_clients.Clear();
   if (Assigned(Self.OnAvailable)) then
     Self.OnAvailable(Self, false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.ClientDisconnect(Client: TIdContext);
begin
 if (Self.m_clients.Contains(Client)) then
  begin
   Self.m_clients.Remove(Client);
   if (Self.m_clients.Count = 0) then
     Self.OnAvailable(Self, false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.Prijede(train: TSHTrain);
begin
 Self.BroadcastData('PRIJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStanicniHlaseni.Odjede(train: TSHTrain);
begin
 Self.BroadcastData('ODJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStanicniHlaseni.Projede(train: TSHTrain);
begin
 Self.BroadcastData('PROJEDE;{' + Self.TrainToStr(train) + '}');
end;

procedure TStanicniHlaseni.Spec(id: string);
begin
 Self.BroadcastData('SPEC;' + id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.BroadcastData(data: string);
var client: TIdContext;
begin
 for client in Self.m_clients do
   ORTCPServer.SendLn(client, Self.m_orid + ';SH;' + data);
end;

////////////////////////////////////////////////////////////////////////////////

function TStanicniHlaseni.TrainToStr(train: TSHTrain): string;
begin
 Result := train.cislo + ';' + train.typ + ';' + train.kolej + ';' + train.fromORid + ';' +
             train.toORid + ';';

 if (train.timeArrive <> 0) then
   Result := Result + FormatDateTime('hh:nn', train.timeArrive);

 Result := Result + ';';
 if (train.timeDepart <> 0) then
   Result := Result + FormatDateTime('hh:nn', train.timeDepart);
end;

////////////////////////////////////////////////////////////////////////////////

function TStanicniHlaseni.GetAvailable(): Boolean;
begin
 Result := (Self.m_clients.Count > 0);
end;

////////////////////////////////////////////////////////////////////////////////

class function TStanicniHlaseni.HlasitTrainTyp(typ: string): Boolean;
var s: string;
begin
 for s in stanicniHlaseni._HLASENI_TRAINTYP_FORBIDDEN do
   if (typ = s) then
     Exit(False);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

end.
