unit stanicniHlaseni;

{
  Trida TStanicniHlaseni se stara o prehravani stanicniho hlaseni jedne oblasti
  rizeni.
}

interface

uses Generics.Collections, IdContext, Classes, SysUtils;

const
  _HLASENI_SPRTYP_FORBIDDEN: array [0..5] of string = (
    'Pn', 'Mn', 'Vn', 'Lv', 'Vle', 'Slu'
  );

type
  TAvailableEvent = procedure (Sender:TObject; available:boolean) of object;

  TSHSpr = record
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

     procedure BroadcastData(data:string);
     function SprToStr(spr:TSHSpr):string;
     function GetAvailable():boolean;

   public
    OnAvailable: TAvailableEvent;

     constructor Create(orid:string);
     destructor Destroy(); override;

     procedure Parse(Sender:TIdContext; SenderOR:TObject; parsed:TStrings);
     procedure Reset();
     procedure ClientDisconnect(Client:TIdContext);

     procedure Prijede(spr:TSHSpr);
     procedure Odjede(spr:TSHSpr);
     procedure Projede(spr:TSHSpr);
     procedure Spec(id:string);

     class function HlasitSprTyp(typ:string):boolean;

     property available: boolean read GetAvailable;

end;

implementation

uses TCPServerOR, TOblRizeni, TCPORsRef;

////////////////////////////////////////////////////////////////////////////////

constructor TStanicniHlaseni.Create(orid:string);
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

procedure TStanicniHlaseni.Parse(Sender:TIdContext; SenderOR:TObject; parsed:TStrings);
begin
 if (parsed.Count < 2) then Exit();
 parsed[2] := UpperCase(parsed[2]);

 if (parsed[2] = 'REGISTER') then
  begin
   if (not TTCPORsRef(Sender.Data).st_hlaseni.Contains(TOR(SenderOR))) then
    begin
     TTCPORsRef(Sender.Data).st_hlaseni.Add(TOR(SenderOR));
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
   if (TTCPORsRef(Sender.Data).st_hlaseni.Contains(TOR(SenderOR))) then
    begin
     TTCPORsRef(Sender.Data).st_hlaseni.Remove(TOR(SenderOR));
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

procedure TStanicniHlaseni.ClientDisconnect(Client:TIdContext);
begin
 if (Self.m_clients.Contains(Client)) then
  begin
   Self.m_clients.Remove(Client);
   if (Self.m_clients.Count = 0) then
     Self.OnAvailable(Self, false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.Prijede(spr:TSHSpr);
begin
 Self.BroadcastData('PRIJEDE;{' + Self.SprToStr(spr) + '}');
end;

procedure TStanicniHlaseni.Odjede(spr:TSHSpr);
begin
 Self.BroadcastData('ODJEDE;{' + Self.SprToStr(spr) + '}');
end;

procedure TStanicniHlaseni.Projede(spr:TSHSpr);
begin
 Self.BroadcastData('PROJEDE;{' + Self.SprToStr(spr) + '}');
end;

procedure TStanicniHlaseni.Spec(id:string);
begin
 Self.BroadcastData('SPEC;' + id);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TStanicniHlaseni.BroadcastData(data:string);
var client:TIdContext;
begin
 for client in Self.m_clients do
   ORTCPServer.SendLn(client, Self.m_orid + ';SH;' + data);
end;

////////////////////////////////////////////////////////////////////////////////

function TStanicniHlaseni.SprToStr(spr:TSHSpr):string;
begin
 Result := spr.cislo + ';' + spr.typ + ';' + spr.kolej + ';' + spr.fromORid + ';' +
             spr.toORid + ';';

 if (spr.timeArrive <> 0) then
   Result := Result + FormatDateTime('hh:nn', spr.timeArrive);

 Result := Result + ';';
 if (spr.timeDepart <> 0) then
   Result := Result + FormatDateTime('hh:nn', spr.timeDepart);
end;

////////////////////////////////////////////////////////////////////////////////

function TStanicniHlaseni.GetAvailable():boolean;
begin
 Result := (Self.m_clients.Count > 0);
end;

////////////////////////////////////////////////////////////////////////////////

class function TStanicniHlaseni.HlasitSprTyp(typ:string):boolean;
var s:string;
begin
 for s in stanicniHlaseni._HLASENI_SPRTYP_FORBIDDEN do
   if (typ = s) then
     Exit(False);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

end.
