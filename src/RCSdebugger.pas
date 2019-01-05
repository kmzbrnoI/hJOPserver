unit RCSdebugger;

{
  Trida TRCSd se stara o komunikaci s RCS debugger klienty po standardnim
  panelovem TCP spojeni.

  RCS debugger je klient, ktery ma pravo zobrazovat a nastavovat konkretni RCS
  vstupy/vystupy. Jedna se primarne o DEBUG funkci, predpokladanym klientem
  jsou predevsim mobilni zarizeni. Pro pouziti RCS debuggeru je nutne se
  autorizovat uzivatelem s opravnenim "root".
}

{
  Princip:
  TRCS vola do jednotlivych klientu eventy pri zmene stavu RCS portu,
  TRCSdClient si ulozi, ze doslo ke zmene stavu RCS desky a pri Update()
  posle klientovi novy stav vsech vstupnich anebo vystupnich pinu.

  - TRCSdClient reprezentuje vzdy prave jednoho autorizovaneho RCSd klienta.
  - Kazdy klient muze ovladat 0..n RCS desek.
  - O desku se zada prikazem "PLEASE", uvoluje se prikazem "RELEASE".
  - Pri odpojeni klienta je vymazan jeho TRCSdClient zaznam.
  - INFO kazde desky jsou nasledujici informace:
      adresa|nazev|typ|existence|verze_fw
      (veskera data jsou stringy, existence je [0,1])
}

interface

uses SysUtils, TechnologieRCS, Generics.Collections, Classes, IdContext;

type
  TRCSdModule = record
    addr:Integer;
    output_changed, input_changed: boolean;
  end;

  // jeden RCSd klient
  TRCSdClient = class
    private
      modules:TList<TRCSdModule>;                                               // seznam autorizovanych modulu klienta
      conn:TIdContext;                                                          // spojeni ke klientovi

       procedure SendOutput(addr:Integer);                                      // odesle stav vystupnich portu RCS \addr
       procedure SendInput(addr:Integer);                                       // odesle stav vstupnich portu RCS \addr

       function ModuleIndexOf(addr:Integer):Integer;                            // vrati index \addr adresy v seznamu modulu \modules

       procedure OnRCSInputChange(Sender:TObject; board:byte);                  // event z TRCS volany pri zmene RCS vstupu
       procedure OnRCSOutputChange(Sender:TObject; board:byte);                 // event z TRCS volany pri zmene RCS vystupu

    public
       constructor Create(conn:TIdContext);
       destructor Destroy(); override;

       procedure Update();                                                      // aktualizace odesilani zmeny stavu RCS desky
       procedure Parse(parsed:TStrings);                                        // parse prijatych dat od klienta

       property connection:TIdContext read conn;                                // reference na spojeni pro rodice

  end;

  //////////////////////////////////////////////////////////////////////////

  // TRCSd sdruzuje jednotlive RCSd klienty
  TRCSd = class
    private
      clients:TList<TRCSdClient>;                                               // seznam autorizovanych klientu

       procedure ParseAuth(Sender:TIdContext; parsed:TStrings);                 // parsovani autorizacniho prikazu "AUTH"

    public
       constructor Create();
       destructor Destroy(); override;

       procedure Parse(Sender:TIdContext; parsed:TStrings);                     // parsovani dat pro RCS debugger -- prefix "-;RCSd;"
       procedure RemoveClient(conn:TIDContext);                                 // smazani RCSd klienta z databaze
       procedure RemoveAllClients();                                            // smazani vsech RCSd klientu
       procedure Update();                                                      // propagace stavu RCS k RCSd klientum

       class function GetRCSInfo(board:byte):string;                            // vraci INFO string

  end;

var
  RCSd: tRCSd;

implementation

{
  Popis TCP protokolu RCS debuggeru:

  @ server -> klient
    -;RCSd;AUTH;[ok,not];message                                                odpoved na autorizaci klienta
    -;RCSd;MOD-AUTH;addr;[ok,not]                                               odpoved na autorizaci RCS modulu
    -;RCSd;MODULE;addr;CHANGE;I;stav_vstupu                                     zmena stavu vstupu RCS modulu \addr
    -;RCSd;MODULE;addr;CHANGE;O;stav_vystupu                                    zmena stavu vystupu RCS modulu \addr
    -;RCSd;ERR;error_message                                                    chybova zprava
    -;RCSd;INFO;board1, board2, ...                                             INFO o modulu (modulech)

  @ klient -> server
    -;RCSd;AUTH;username;hashed_password                                        zadost o povoleni RCS debugger, je nutne se prihlasit rootem
    -;RCSd;PLEASE;addr                                                          zadost o RCS modul \addr
    -;RCSd;RELEASE;addr                                                         uvolneni RCS modulu \addr
    -;RCSd;SETOUT;addr;port;stav                                                nastaveni vystupu \port RCS modulu \addr na stav \stav
    -;RCSd;UPDATE;addr;                                                         pozadavek na zaslani aktualniho stavu vsech portu RCS \addr
    -;RCSd;LIST;                                                                zadost o info vsech existujicich RCS
    -;RCSd;INFO;addr                                                            zadost o info konkretniho RCS

  # stav_vstupu, stav_vystupu: az 15 cisel oddelenych "|"
    napr. 0|0|0|0|0|0|0|0|0|0|0|0|0|0|0 je RCS se vsemi vystupy v logicke nule
    0    = vystup v logicke 0
    1    = vystup v logicke 1
    -2   = RCS neexistuje / vypadek RCS
    0..n = kod SCom navesti
}

uses TCPServerOR, UserDb, User, RCSErrors;

////////////////////////////////////////////////////////////////////////////////

constructor TRCSd.Create();
begin
 inherited;
 Self.clients := TList<TRCSdClient>.Create();
end;

destructor TRCSd.Destroy();
begin
 Self.RemoveAllClients();
 Self.clients.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSd.RemoveClient(conn:TIDContext);
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do
   if (Self.clients[i].connection = conn) then
    begin
     Self.clients[i].Free();
     Self.clients.Delete(i);
    end;
end;

procedure TRCSd.RemoveAllClients();
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do Self.clients[i].Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSd.Parse(Sender:TIdContext; parsed:TStrings);
var i:Integer;
begin
 parsed[2] := UpperCase(parsed[2]);

 if (parsed[2] = 'AUTH') then
  begin
   Self.ParseAuth(Sender, parsed);
  end else begin
    for i := 0 to Self.clients.Count-1 do
      if (Self.clients[i].connection = Sender) then
        Self.clients[i].Parse(parsed);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSd.ParseAuth(Sender:TIdContext; parsed:TStrings);
var user:TUser;
    client:TRCSdClient;
begin
 // -> zjistime uzivatele
 user := UsrDb.GetUser(parsed[3]);

 // kontrola existence uzivatele
 if (not Assigned(user)) then
  begin
   ORTCPServer.SendLn(Sender, '-;RCSd;AUTH;not;Neexistující uživatel');
   Exit();
  end;

 // kontrola BANu uzivatele
 if (user.ban) then
  begin
   ORTCPServer.SendLn(Sender, '-;RCSd;AUTH;not;Uživatel '+user.id+' má BAN !');
   Exit();
  end;

 // kontrola hesla uzivatele
 if (not TUser.ComparePasswd(parsed[4], user.password, user.salt)) then
  begin
   ORTCPServer.SendLn(Sender, '-;RCSd;AUTH;not;Neplatné heslo');
   Exit();
  end;

 // kontrola opravneni root
 if (not user.root) then
  begin
   ORTCPServer.SendLn(Sender, '-;RCSd;AUTH;not;Uživatel nemá oprávnìní root');
   Exit();
  end;

 client := TRCSdClient.Create(Sender);
 Self.clients.Add(client);
 ORTCPServer.SendLn(Sender, '-;RCSd;AUTH;ok;Autorizováno');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSd.Update();
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do
   Self.clients[i].Update();
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////////////  TRIDA TRCSdClient /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TRCSdClient.Create(conn:TIdContext);
begin
 inherited Create();
 Self.modules := TList<TRCSdModule>.Create();
 Self.conn := conn;
end;

destructor TRCSdClient.Destroy();
begin
 RCSi.RemoveInputChangeEvent(Self.OnRCSInputChange);
 RCSi.RemoveInputChangeEvent(Self.OnRCSOutputChange);
 FreeAndNil(Self.modules);
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.SendOutput(addr:Integer);
var i:Integer;
    str:string;
    max:Cardinal;
begin
 str := '';

 try
   max := RCSi.GetModuleOutputsCount(addr);
 except
   max := 16;
 end;

 for i := 0 to max-1 do
  begin
   try
     str := str + IntToStr(RCSi.GetOutput(addr, i)) + '|';
   except
     on E:Exception do
      begin
       ORTCPServer.SendLn(Self.conn, '-;RCSd;ERR;{' + E.Message+'}');
       Exit();
      end;
   end;
  end;
 ORTCPServer.SendLn(Self.conn, '-;RCSd;MODULE;'+IntToStr(addr)+';CHANGE;O;{'+str+'}');
end;

procedure TRCSdClient.SendInput(addr:Integer);
var i:Integer;
    str:string;
    max:Cardinal;
begin
 str := '';

 try
   max := RCSi.GetModuleInputsCount(addr);
 except
   max := 16;
 end;

 for i := 0 to max-1 do
  begin
   try
     str := str + IntToStr(Integer(RCSi.GetInput(addr, i))) + '|';
   except
     on E:Exception do
      begin
       ORTCPServer.SendLn(Self.conn, '-;RCSd;ERR;{' + E.Message+'}');
       Exit();
      end;
   end;
  end;
 ORTCPServer.SendLn(Self.conn, '-;RCSd;MODULE;'+IntToStr(addr)+';CHANGE;I;{'+str+'}');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.OnRCSInputChange(Sender:TObject; board:byte);
var index:Integer;
    module:TRCSdModule;
begin
 index := Self.ModuleIndexOf(board);
 if (index > -1) then
  begin
   module := Self.modules[index];
   module.input_changed := true;
   Self.modules[index] := module;
  end;
end;

procedure TRCSdClient.OnRCSOutputChange(Sender:TObject; board:byte);
var index:Integer;
    module:TRCSdModule;
begin
 index := Self.ModuleIndexOf(board);
 if (index > -1) then
  begin
   module := Self.modules[index];
   module.output_changed := true;
   Self.modules[index] := module;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TRCSdClient.ModuleIndexOf(addr:Integer):Integer;
var i:Integer;
begin
 Result := -1;
 for i := 0 to Self.modules.Count-1 do
   if (Self.modules[i].addr = addr) then Exit(i);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.Update();
var i:Integer;
    module:TRCSdModule;
begin
 for i := 0 to Self.modules.Count-1 do
  begin
   module := Self.modules[i];
   if (module.output_changed) then
    begin
     module.output_changed := false;
     Self.SendOutput(module.addr);
     Self.modules[i] := module;
    end;
   if (module.input_changed) then
    begin
     module.input_changed := false;
     Self.SendInput(module.addr);
     Self.modules[i] := module;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.Parse(parsed:TStrings);
var addr, index, i:Integer;
    module:TRCSdModule;
    str:string;
begin
 if (parsed[2] = 'PLEASE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;MOD-AUTH;'+parsed[2]+';not;Neplatná adresa modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index = -1) then
   begin
    module.addr := addr;
    module.output_changed := false;
    module.input_changed  := false;
    Self.modules.Add(module);
    RCSi.AddInputChangeEvent(addr, Self.OnRCSInputChange);
    RCSi.AddOutputChangeEvent(addr, Self.OnRCSOutputChange);
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;MOD-AUTH;'+IntToStr(addr)+';ok');
    Self.SendInput(addr);
    Self.SendOutput(addr);
   end;

 end else if (parsed[2] = 'RELEASE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index > -1) then
   begin
    RCSi.RemoveInputChangeEvent(Self.OnRCSInputChange, addr);
    RCSi.RemoveOutputChangeEvent(Self.OnRCSOutputChange, addr);
    Self.modules.Delete(index);
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;MOD-AUTH;'+IntToStr(addr)+';not');
   end;

 end else if (parsed[2] = 'SETOUT') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
    Exit();
  end;

  RCSi.SetOutput(addr, StrToInt(parsed[4]), StrToInt(parsed[5]));

 end else if (parsed[2] = 'UPDATE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index > -1) then
   begin
    Self.SendInput(addr);
    Self.SendOutput(addr);
   end else begin
    ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;ERR;Modul není autorizován');
   end;

 end else if (parsed[2] = 'INFO') then begin
   ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;INFO;{{'+TRCSd.GetRCSInfo(StrToInt(parsed[3]))+'}}');

 end else if (parsed[2] = 'LIST') then begin
   str := '';
   for i := 0 to TRCS._MAX_RCS-1 do
     if ((RCSi.IsModule(i)) or (RCSi.GetNeeded(i))) then
        str := str + '{' + TRCSd.GetRCSInfo(i) + '}';
   ORTCPServer.SendInfoMsg(Self.conn, '-;RCSd;INFO;{'+str+'}');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TRCSd.GetRCSInfo(board:byte):string;
begin
 Result := IntToStr(board) + '|';

 try
   Result := Result + RCSi.GetModuleName(board) + '|';
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat nazev - vyjimka|';
 end;

 try
   Result := Result + RCSi.GetModuleType(board) + '|';
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat typ - vyjimka|';
 end;

 try
   case (RCSi.IsModule(board)) of
    false : Result := Result + '0|';
    true  : Result := Result + '1|';
   end;
 except
   Result := Result + '0|';
 end;

 try
   Result := Result + RCSi.GetModuleFW(board);
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat FW - vyjimka|';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  RCSd := TRCSd.Create();
finalization
  FreeAndNil(RCSd);

end.
