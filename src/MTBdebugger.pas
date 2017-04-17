unit MTBdebugger;

{
  Trida TMTBd se stara o komunikaci s MTB debugger klienty po standardnim
  panelovem TCP spojeni.

  MTB debugger je klient, ktery ma pravo zobrazovat a nastavovat konkretni MTB
  vstupy/vystupy. Jedna se primarne o DEBUG funkci, predpokladanym klientem
  jsou predevsim mobilni zarizeni. Pro pouziti MTB debuggeru je nutne se
  autorizovat uzivatelem s opravnenim "root".
}

{
  Princip:
  TMTB vola do jednotlivych klientu eventy pri zmene stavu MTB portu,
  TMTBdClient si ulozi, ze doslo ke zmene stavu MTB desky a pri Update()
  posle klientovi novy stav vsech vstupnich anebo vystupnich pinu.

  - TMTBdClient reprezentuje vzdy prave jednoho autorizovaneho MTBd klienta.
  - Kazdy klient muze ovladat 0..n MTB desek.
  - O desku se zada prikazem "PLEASE", uvoluje se prikazem "RELEASE".
  - Pri odpojeni klienta je vymazan jeho TMTBdClient zaznam.
  - INFO kazde desky jsou nasledujici informace:
      adresa|nazev|typ|existence|verze_fw
      (veskera data jsou stringy, existence je [0,1])
}

interface

uses SysUtils, TechnologieMTB, Generics.Collections, Classes, IdContext;

type
  TMTBdModule = record
    addr:Integer;
    output_changed, input_changed: boolean;
  end;

  // jeden MTBd klient
  TMTBdClient = class
    private
      modules:TList<TMTBdModule>;                                               // seznam autorizovanych modulu klienta
      conn:TIdContext;                                                          // spojeni ke klientovi

       procedure SendOutput(addr:Integer);                                      // odesle stav vystupnich portu MTB \addr
       procedure SendInput(addr:Integer);                                       // odesle stav vstupnich portu MTB \addr

       function ModuleIndexOf(addr:Integer):Integer;                            // vrati index \addr adresy v seznamu modulu \modules

       procedure OnMTBInputChange(Sender:TObject; board:byte);                  // event z TMTB volany pri zmene MTB vstupu
       procedure OnMTBOutputChange(Sender:TObject; board:byte);                 // event z TMTB volany pri zmene MTB vystupu

    public
       constructor Create(conn:TIdContext);
       destructor Destroy(); override;

       procedure Update();                                                      // aktualizace odesilani zmeny stavu MTB desky
       procedure Parse(parsed:TStrings);                                        // parse prijatych dat od klienta

       property connection:TIdContext read conn;                                // reference na spojeni pro rodice

  end;

  //////////////////////////////////////////////////////////////////////////

  // TMTBd sdruzuje jednotlive MTBd klienty
  TMTBd = class
    private
      clients:TList<TMTBdClient>;                                               // seznam autorizovanych klientu

       procedure ParseAuth(Sender:TIdContext; parsed:TStrings);                 // parsovani autorizacniho prikazu "AUTH"

    public
       constructor Create();
       destructor Destroy(); override;

       procedure Parse(Sender:TIdContext; parsed:TStrings);                     // parsovani dat pro MTB debugger -- prefix "-;MTBd;"
       procedure RemoveClient(conn:TIDContext);                                 // smazani MTBd klienta z databaze
       procedure RemoveAllClients();                                            // smazani vsech MTBd klientu
       procedure Update();                                                      // propagace stavu MTB k MTBd klientum

       class function GetMTBInfo(board:byte):string;                            // vraci INFO string

  end;

var
  MTBd: tMTBd;

implementation

{
  Popis TCP protokolu MTB debuggeru:

  @ server -> klient
    -;MTBd;AUTH;[ok,not];message                                                odpoved na autorizaci klienta
    -;MTBd;MOD-AUTH;addr;[ok,not]                                               odpoved na autorizaci MTB modulu
    -;MTBd;MODULE;addr;CHANGE;I;stav_vstupu                                     zmena stavu vstupu MTB modulu \addr
    -;MTBd;MODULE;addr;CHANGE;O;stav_vystupu                                    zmena stavu vystupu MTB modulu \addr
    -;MTBd;ERR;error_message                                                    chybova zprava
    -;MTBd;INFO;board1, board2, ...                                             INFO o modulu (modulech)

  @ klient -> server
    -;MTBd;AUTH;username;hashed_password                                        zadost o povoleni MTB debugger, je nutne se prihlasit rootem
    -;MTBd;PLEASE;addr                                                          zadost o MTB modul \addr
    -;MTBd;RELEASE;addr                                                         uvolneni MTB modulu \addr
    -;MTBd;SETOUT;addr;port;stav                                                nastaveni vystupu \port MTB modulu \addr na stav \stav
    -;MTBd;UPDATE;addr;                                                         pozadavek na zaslani aktualniho stavu vsech portu MTB \addr
    -;MTBd;LIST;                                                                zadost o info vsech existujicich MTB
    -;MTBd;INFO;addr                                                            zadost o info konkretniho MTB

  # stav_vstupu, stav_vystupu: 15 cisel oddelenych "|"
    napr. 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0 je MTB se vsemi vystupy v logicke nule
    0    = vystup v logicke 0
    1    = vystup v logicke 1
    -2   = MTB neexistuje / vypadek MTB
    0..n = kod SCom navesti
}

uses TCPServerOR, UserDb, User, RCSErrors;

////////////////////////////////////////////////////////////////////////////////

constructor TMTBd.Create();
begin
 inherited;
 Self.clients := TList<TMTBdClient>.Create();
end;

destructor TMTBd.Destroy();
begin
 Self.RemoveAllClients();
 Self.clients.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBd.RemoveClient(conn:TIDContext);
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do
   if (Self.clients[i].connection = conn) then
    begin
     Self.clients[i].Free();
     Self.clients.Delete(i);
    end;
end;

procedure TMTBd.RemoveAllClients();
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do Self.clients[i].Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBd.Parse(Sender:TIdContext; parsed:TStrings);
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

procedure TMTBd.ParseAuth(Sender:TIdContext; parsed:TStrings);
var user:TUser;
    client:TMTBdClient;
begin
 // -> zjistime uzivatele
 user := UsrDb.GetUser(parsed[3]);

 // kontrola existence uzivatele
 if (not Assigned(user)) then
  begin
   ORTCPServer.SendLn(Sender, '-;MTBd;AUTH;not;Neexistující uživatel');
   Exit();
  end;

 // kontrola BANu uzivatele
 if (user.ban) then
  begin
   ORTCPServer.SendLn(Sender, '-;MTBd;AUTH;not;Uživatel '+user.id+' má BAN !');
   Exit();
  end;

 // kontrola hesla uzivatele
 if (not TUser.ComparePasswd(parsed[4], user.password, user.salt)) then
  begin
   ORTCPServer.SendLn(Sender, '-;MTBd;AUTH;not;Neplatné heslo');
   Exit();
  end;

 // kontrola opravneni root
 if (not user.root) then
  begin
   ORTCPServer.SendLn(Sender, '-;MTBd;AUTH;not;Uživatel nemá oprávnìní root');
   Exit();
  end;

 client := TMTBdClient.Create(Sender);
 Self.clients.Add(client);
 ORTCPServer.SendLn(Sender, '-;MTBd;AUTH;ok;Autorizováno');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBd.Update();
var i:Integer;
begin
 for i := 0 to Self.clients.Count-1 do
   Self.clients[i].Update();
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////////////  TRIDA TMTBdClient /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TMTBdClient.Create(conn:TIdContext);
begin
 inherited Create();
 Self.modules := TList<TMTBdModule>.Create();
 Self.conn := conn;
end;

destructor TMTBdClient.Destroy();
begin
 MTB.RemoveInputChangeEvent(Self.OnMTBInputChange);
 MTB.RemoveInputChangeEvent(Self.OnMTBOutputChange);
 FreeAndNil(Self.modules);
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBdClient.SendOutput(addr:Integer);
var i:Integer;
    str:string;
begin
 str := '';
 for i := 0 to 15 do
  begin
   try
     str := str + IntToStr(MTB.GetOutput(addr, i)) + '|';
   except
     on E:Exception do
      begin
       ORTCPServer.SendLn(Self.conn, '-;MTBd;ERR;{' + E.Message+'}');
       Exit();
      end;
   end;
  end;
 ORTCPServer.SendLn(Self.conn, '-;MTBd;MODULE;'+IntToStr(addr)+';CHANGE;O;{'+str+'}');
end;

procedure TMTBdClient.SendInput(addr:Integer);
var i:Integer;
    str:string;
begin
 str := '';
 for i := 0 to 15 do
  begin
   try
     str := str + IntToStr(Integer(MTB.GetInput(addr, i))) + '|';
   except
     on E:Exception do
      begin
       ORTCPServer.SendLn(Self.conn, '-;MTBd;ERR;{' + E.Message+'}');
       Exit();
      end;
   end;
  end;
 ORTCPServer.SendLn(Self.conn, '-;MTBd;MODULE;'+IntToStr(addr)+';CHANGE;I;{'+str+'}');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBdClient.OnMTBInputChange(Sender:TObject; board:byte);
var index:Integer;
    module:TMTBdModule;
begin
 index := Self.ModuleIndexOf(board);
 if (index > -1) then
  begin
   module := Self.modules[index];
   module.input_changed := true;
   Self.modules[index] := module;
  end;
end;

procedure TMTBdClient.OnMTBOutputChange(Sender:TObject; board:byte);
var index:Integer;
    module:TMTBdModule;
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

function TMTBdClient.ModuleIndexOf(addr:Integer):Integer;
var i:Integer;
begin
 Result := -1;
 for i := 0 to Self.modules.Count-1 do
   if (Self.modules[i].addr = addr) then Exit(i);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBdClient.Update();
var i:Integer;
    module:TMTBdModule;
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

procedure TMTBdClient.Parse(parsed:TStrings);
var addr, index, i:Integer;
    module:TMTBdModule;
    str:string;
begin
 if (parsed[2] = 'PLEASE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;MOD-AUTH;'+parsed[2]+';not;Neplatná adresa modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index = -1) then
   begin
    module.addr := addr;
    module.output_changed := false;
    module.input_changed  := false;
    Self.modules.Add(module);
    MTB.AddInputChangeEvent(addr, Self.OnMTBInputChange);
    MTB.AddOutputChangeEvent(addr, Self.OnMTBOutputChange);
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;MOD-AUTH;'+IntToStr(addr)+';ok');
    Self.SendInput(addr);
    Self.SendOutput(addr);
   end;

 end else if (parsed[2] = 'RELEASE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;ERR;Neplatná adresa MTB modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index > -1) then
   begin
    MTB.RemoveInputChangeEvent(Self.OnMTBInputChange, addr);
    MTB.RemoveOutputChangeEvent(Self.OnMTBOutputChange, addr);
    Self.modules.Delete(index);
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;MOD-AUTH;'+IntToStr(addr)+';not');
   end;

 end else if (parsed[2] = 'SETOUT') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;ERR;Neplatná adresa MTB modulu');
    Exit();
  end;

  MTB.SetOutput(addr, StrToInt(parsed[4]), StrToInt(parsed[5]));

 end else if (parsed[2] = 'UPDATE') then begin
  try
    addr := StrToInt(parsed[3]);
  except
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;ERR;Neplatná adresa MTB modulu');
    Exit();
  end;

  index := Self.ModuleIndexOf(addr);
  if (index > -1) then
   begin
    Self.SendInput(addr);
    Self.SendOutput(addr);
   end else begin
    ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;ERR;Modul není autorizován');
   end;

 end else if (parsed[2] = 'INFO') then begin
   ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;INFO;{{'+TMTBd.GetMTBInfo(StrToInt(parsed[3]))+'}}');

 end else if (parsed[2] = 'LIST') then begin
   str := '';
   for i := 0 to TMTB._MAX_MTB-1 do
     if ((MTB.IsModule(i)) or (MTB.GetNeeded(i))) then
        str := str + '{' + TMTBd.GetMTBInfo(i) + '}';
   ORTCPServer.SendInfoMsg(Self.conn, '-;MTBd;INFO;{'+str+'}');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TMTBd.GetMTBInfo(board:byte):string;
begin
 Result := IntToStr(board) + '|';

 try
   Result := Result + MTB.GetModuleName(board) + '|';
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat nazev - vyjimka|';
 end;

 try
   Result := Result + IntToStr(MTB.GetModuleType(board)) + '|';
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat typ - vyjimka|';
 end;

 try
   case (MTB.IsModule(board)) of
    false : Result := Result + '0|';
    true  : Result := Result + '1|';
   end;
 except
   Result := Result + '0|';
 end;

 try
   Result := Result + MTB.GetModuleFW(board);
 except
   on E:ERCSInvalidModuleAddr do
     Result := Result + '-|';
   on E:Exception do
     Result := Result + 'Nelze ziskat FW - vyjimka|';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  MTBd := TMTBd.Create();
finalization
  FreeAndNil(MTBd);

end.
