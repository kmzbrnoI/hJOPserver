﻿unit RCSdebugger;

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
}

interface

uses SysUtils, TechnologieRCS, Generics.Collections, Classes, IdContext;

type
  TRCSdModule = record
    addr: Integer;
    output_changed, input_changed: Boolean;
  end;

  // jeden RCSd klient
  TRCSdClient = class
  private
    modules: TList<TRCSdModule>; // seznam autorizovanych modulu klienta
    conn: TIdContext; // spojeni ke klientovi

    procedure SendOutput(addr: Integer); // odesle stav vystupnich portu RCS \addr
    procedure SendInput(addr: Integer); // odesle stav vstupnich portu RCS \addr

    function ModuleIndexOf(addr: Integer): Integer; // vrati index \addr adresy v seznamu modulu \modules

    procedure OnRCSInputChange(Sender: TObject; board: Cardinal); // event z TRCS volany pri zmene RCS vstupu
    procedure OnRCSOutputChange(Sender: TObject; board: Cardinal); // event z TRCS volany pri zmene RCS vystupu

  public
    constructor Create(conn: TIdContext);
    destructor Destroy(); override;

    procedure Update(); // aktualizace odesilani zmeny stavu RCS desky
    procedure Parse(parsed: TStrings); // parse prijatych dat od klienta

    property connection: TIdContext read conn; // reference na spojeni pro rodice

  end;

  /// ///////////////////////////////////////////////////////////////////////

  // TRCSd sdruzuje jednotlive RCSd klienty
  TRCSd = class
  private
    clients: TList<TRCSdClient>; // seznam autorizovanych klientu

    procedure ParseAuth(Sender: TIdContext; parsed: TStrings); // parsovani autorizacniho prikazu "AUTH"

  public
    constructor Create();
    destructor Destroy(); override;

    procedure Parse(Sender: TIdContext; parsed: TStrings); // parsovani dat pro RCS debugger -- prefix "-; RCSd;"
    procedure RemoveClient(conn: TIdContext); // smazani RCSd klienta z databaze
    procedure RemoveAllClients(); // smazani vsech RCSd klientu
    procedure Update(); // propagace stavu RCS k RCSd klientum

    class function GetRCSInfo(board: Cardinal): string; // vraci INFO string

  end;

var
  RCSd: TRCSd;

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

  # stav_vstupu, stav_vystupu: az 16 cisel oddelenych "|"
  napr. 0|0|0|0|0|0|0|0|0|0|0|0|0|0|0 je RCS se vsemi vystupy v logicke nule
  vstupy: stavy odpovidaji enumu TRCSInputState
  vystupy:
  0    = vystup v logicke 0
  1    = vystup v logicke 1
  -2   = RCS neexistuje / vypadek RCS
  0..n = kod S-COM navesti

  # INFO RCS modulu: texty oddelene znakem "|"
  napr. 1|Skuhrov město tunel Skuhrov|MTB-UNI|1|4.1|BBBBBBBBBBBBBBBB|SSSSBBBBBBBBBBBB
  Polozky:
  * adresa modulu
  * nazev modulu
  * typ modulu
  * jestli je modul fyzicky na sbernici (0 v pripade vypadku modulu)
  * firmware modulu
  * typy vstupu (B=binarni vstup, I=IR vstup, -=nedostupny port)
  * typy vystupu (B=binarni vystup, S=S-COM, -=nedostupny port)
}

uses TCPServerPanel, UserDb, User, RCSErrors, RCS;

/// /////////////////////////////////////////////////////////////////////////////

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

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSd.RemoveClient(conn: TIdContext);
begin
  for var i := 0 to Self.clients.Count - 1 do
    if (Self.clients[i].connection = conn) then
    begin
      Self.clients[i].Free();
      Self.clients.Delete(i);
    end;
end;

procedure TRCSd.RemoveAllClients();
begin
  for var i := 0 to Self.clients.Count - 1 do
    Self.clients[i].Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSd.Parse(Sender: TIdContext; parsed: TStrings);
begin
  parsed[2] := UpperCase(parsed[2]);

  if (parsed[2] = 'AUTH') then
  begin
    Self.ParseAuth(Sender, parsed);
  end else begin
    for var i := 0 to Self.clients.Count - 1 do
      if (Self.clients[i].connection = Sender) then
        Self.clients[i].Parse(parsed);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSd.ParseAuth(Sender: TIdContext; parsed: TStrings);
var user: TUser;
begin
  // -> zjistime uzivatele
  user := UsrDb.GetUser(parsed[3]);

  // kontrola existence uzivatele
  if (not Assigned(user)) then
  begin
    PanelServer.SendLn(Sender, '-;RCSd;AUTH;not;Neexistující uživatel');
    Exit();
  end;

  // kontrola BANu uzivatele
  if (user.ban) then
  begin
    PanelServer.SendLn(Sender, '-;RCSd;AUTH;not;Uživatel ' + user.username + ' má BAN !');
    Exit();
  end;

  // kontrola hesla uzivatele
  if (not TUser.ComparePasswd(parsed[4], user.password, user.salt)) then
  begin
    PanelServer.SendLn(Sender, '-;RCSd;AUTH;not;Neplatné heslo');
    Exit();
  end;

  // kontrola opravneni root
  if (not user.root) then
  begin
    PanelServer.SendLn(Sender, '-;RCSd;AUTH;not;Uživatel nemá oprávnění root');
    Exit();
  end;

  var client := TRCSdClient.Create(Sender);
  Self.clients.Add(client);
  PanelServer.SendLn(Sender, '-;RCSd;AUTH;ok;Autorizováno');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSd.Update();
begin
  for var i := 0 to Self.clients.Count - 1 do
    Self.clients[i].Update();
end;

/// /////////////////////////////////////////////////////////////////////////////
/// ////////////////////////  TRIDA TRCSdClient /////////////////////////////////
/// /////////////////////////////////////////////////////////////////////////////

constructor TRCSdClient.Create(conn: TIdContext);
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

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.SendOutput(addr: Integer);
var str: string;
  max: Cardinal;
begin
  str := '';
  max := RCSi.GetModuleOutputsCountSafe(addr);

  for var i := 0 to max - 1 do
  begin
    try
      str := str + IntToStr(RCSi.GetOutput(addr, i)) + '|';
    except
      on E: Exception do
      begin
        PanelServer.SendLn(Self.conn, '-;RCSd;ERR;{' + E.Message + '}');
        Exit();
      end;
    end;
  end;
  PanelServer.SendLn(Self.conn, '-;RCSd;MODULE;' + IntToStr(addr) + ';CHANGE;O;{' + str + '}');
end;

procedure TRCSdClient.SendInput(addr: Integer);
var str: string;
  max: Cardinal;
begin
  str := '';
  max := RCSi.GetModuleInputsCountSafe(addr);

  for var i := 0 to max - 1 do
  begin
    try
      str := str + IntToStr(Integer(RCSi.GetInput(addr, i))) + '|';
    except
      on E: Exception do
      begin
        PanelServer.SendLn(Self.conn, '-;RCSd;ERR;{' + E.Message + '}');
        Exit();
      end;
    end;
  end;
  PanelServer.SendLn(Self.conn, '-;RCSd;MODULE;' + IntToStr(addr) + ';CHANGE;I;{' + str + '}');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.OnRCSInputChange(Sender: TObject; board: Cardinal);
var index: Integer;
  module: TRCSdModule;
begin
  index := Self.ModuleIndexOf(board);
  if (index > -1) then
  begin
    module := Self.modules[index];
    module.input_changed := true;
    Self.modules[index] := module;
  end;
end;

procedure TRCSdClient.OnRCSOutputChange(Sender: TObject; board: Cardinal);
var index: Integer;
  module: TRCSdModule;
begin
  index := Self.ModuleIndexOf(board);
  if (index > -1) then
  begin
    module := Self.modules[index];
    module.output_changed := true;
    Self.modules[index] := module;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCSdClient.ModuleIndexOf(addr: Integer): Integer;
begin
  Result := -1;
  for var i := 0 to Self.modules.Count - 1 do
    if (Self.modules[i].addr = addr) then
      Exit(i);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.Update();
begin
  for var i := 0 to Self.modules.Count - 1 do
  begin
    var module := Self.modules[i];
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

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSdClient.Parse(parsed: TStrings);
begin
  if (parsed[2] = 'PLEASE') then
  begin
    var addr: Integer;
    try
      addr := StrToInt(parsed[3]);
    except
      PanelServer.SendLn(Self.conn, '-;RCSd;MOD-AUTH;' + parsed[2] + ';not;Neplatná adresa modulu');
      Exit();
    end;

    var index := Self.ModuleIndexOf(addr);
    if (index = -1) then
    begin
      var module: TRCSdModule;
      module.addr := addr;
      module.output_changed := false;
      module.input_changed := false;
      Self.modules.Add(module);
      RCSi.AddInputChangeEvent(addr, Self.OnRCSInputChange);
      RCSi.AddOutputChangeEvent(addr, Self.OnRCSOutputChange);
      PanelServer.SendLn(Self.conn, '-;RCSd;MOD-AUTH;' + IntToStr(addr) + ';ok');
      Self.SendInput(addr);
      Self.SendOutput(addr);
    end;

  end else if (parsed[2] = 'RELEASE') then
  begin
    var addr: Integer;
    try
      addr := StrToInt(parsed[3]);
    except
      PanelServer.SendLn(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
      Exit();
    end;

    var index := Self.ModuleIndexOf(addr);
    if (index > -1) then
    begin
      RCSi.RemoveInputChangeEvent(Self.OnRCSInputChange, addr);
      RCSi.RemoveOutputChangeEvent(Self.OnRCSOutputChange, addr);
      Self.modules.Delete(index);
      PanelServer.SendLn(Self.conn, '-;RCSd;MOD-AUTH;' + IntToStr(addr) + ';not');
    end;

  end else if (parsed[2] = 'SETOUT') then
  begin
    var addr: Integer;
    try
      addr := StrToInt(parsed[3]);
    except
      PanelServer.SendLn(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
      Exit();
    end;

    RCSi.SetOutput(addr, StrToInt(parsed[4]), StrToInt(parsed[5]));

  end else if (parsed[2] = 'UPDATE') then
  begin
    var addr: Integer;
    try
      addr := StrToInt(parsed[3]);
    except
      PanelServer.SendLn(Self.conn, '-;RCSd;ERR;Neplatná adresa RCS modulu');
      Exit();
    end;

    Self.SendInput(addr);
    Self.SendOutput(addr);

  end else if (parsed[2] = 'INFO') then
  begin
    PanelServer.SendLn(Self.conn, '-;RCSd;INFO;{{' + TRCSd.GetRCSInfo(StrToInt(parsed[3])) + '}}');

  end else if (parsed[2] = 'LIST') then
  begin
    var str := '';
    for var i := 0 to RCSi.maxModuleAddr do
      if (RCSi.IsModule(i)) then
        str := str + '{' + TRCSd.GetRCSInfo(i) + '}';
    PanelServer.SendLn(Self.conn, '-;RCSd;INFO;{' + str + '}');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRCSd.GetRCSInfo(board: Cardinal): string;
begin
  Result := IntToStr(board) + '|';

  try
    Result := Result + RCSi.GetModuleName(board) + '|';
  except
    on E: ERCSInvalidModuleAddr do
      Result := Result + '-|';
    on E: Exception do
      Result := Result + 'Nelze ziskat nazev - vyjimka|';
  end;

  try
    Result := Result + RCSi.GetModuleType(board) + '|';
  except
    on E: ERCSInvalidModuleAddr do
      Result := Result + '-|';
    on E: Exception do
      Result := Result + 'Nelze ziskat typ - vyjimka|';
  end;

  try
    case (RCSi.IsModule(board)) of
      false:
        Result := Result + '0|';
      true:
        Result := Result + '1|';
    end;
  except
    Result := Result + '0|';
  end;

  try
    Result := Result + RCSi.GetModuleFW(board) + '|';
  except
    on E: ERCSInvalidModuleAddr do
      Result := Result + '-|';
    on E: Exception do
      Result := Result + 'Nelze ziskat FW - vyjimka|';
  end;

  for var port := 0 to RCSi.GetModuleInputsCountSafe(board) - 1 do
  begin
    try
      case (RCSi.GetInputType(board, port)) of
        TRCSIPortType.iptPlain:
          Result := Result + 'B';
        TRCSIPortType.iptIR:
          Result := Result + 'I';
      end;
    except
      Result := Result + '-';
    end;
  end;
  Result := Result + '|';

  for var port := 0 to RCSi.GetModuleOutputsCountSafe(board) - 1 do
  begin
    try
      case (RCSi.GetOutputType(board, port)) of
        TRCSOPortType.optPlain:
          Result := Result + 'B';
        TRCSOPortType.optSCom:
          Result := Result + 'S';
      end;
    except
      Result := Result + '-';
    end;
  end;
  Result := Result + '|';

end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

RCSd := TRCSd.Create();

finalization

FreeAndNil(RCSd);

end.
