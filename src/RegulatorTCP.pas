unit RegulatorTCP;

// Trida TTCPRegulator se stara o komunikaci s regulatory -- klienty.
// napr. regulator Jerry.

interface

uses Classes, IdContext, AnsiStrings, SysUtils, Forms, THnaciVozidlo;

type

  TLokResponseData = record
    addr: Word;
    conn: TIDContext;
  end;

  TTCPRegulator = class
  private
    procedure ParseGlobal(Sender: TIDContext; parsed: TStrings);
    procedure ParseLoko(Sender: TIDContext; parsed: TStrings);

    procedure ClientAuthorise(conn: TIDContext; state: Boolean; user: TObject; comment: string = '');
    procedure ClientError(conn: TIDContext; error: string);

    procedure PanelLOKResponseOK(Sender: TObject; Data: Pointer);
    procedure PanelLOKResponseErr(Sender: TObject; Data: Pointer);

  public

    procedure Parse(Sender: TIDContext; parsed: TStrings);

    procedure LokUpdateFunc(HV: THV; exclude: TObject = nil);
    procedure LokUpdateSpeed(HV: THV; exclude: TObject = nil);
    procedure LokStolen(HV: THV; exclude: TObject = nil);
    procedure LokUpdateRuc(HV: THV);

    procedure LokToRegulator(Regulator: TIDContext; HV: THV);
    procedure RegDisconnect(reg: TIDContext; contextDestroyed: Boolean = false);
    procedure RemoveLok(Regulator: TIDContext; HV: THV; info: string);

    procedure SendExpectedSpeed(reg: TIDContext; HV: THV);
    procedure SendPredictedSignal(reg: TIDContext; HV: THV);

  end;

var
  TCPRegulator: TTCPRegulator;

implementation

uses UserDb, user, TCPServerPanel, Trakce, THVDatabase, TrainDb, TCPAreasRef, Logging,
  fRegulator, fMain, Area, AreaDb, TechnologieTrakce, ownConvert;

/// /////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;"

procedure TTCPRegulator.Parse(Sender: TIDContext; parsed: TStrings);
begin
  parsed[2] := UpperCase(parsed[2]);
  if (parsed[2] = 'G') then
    Self.ParseGlobal(Sender, parsed)
  else
    Self.ParseLoko(Sender, parsed);
end;

/// /////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;G;"

procedure TTCPRegulator.ParseGlobal(Sender: TIDContext; parsed: TStrings);
var user: TUser;
  Area: TArea;
begin
  parsed[3] := UpperCase(parsed[3]);

  if (parsed[3] = 'AUTH') then
  begin
    // pozadavek na autorizaci klienta
    try
      if (parsed[4] = '') then
      begin
        Self.ClientAuthorise(Sender, false, nil, 'Uživatel odhlášen');
        Exit();
      end;

      user := UsrDb.GetUser(parsed[4]);

      // kontrola existence uzivatele
      if (not Assigned(user)) then
      begin
        Self.ClientAuthorise(Sender, false, nil, 'Uživatel ' + parsed[4] + ' neexistuje !');
        Exit();
      end;

      // kontrola BANu uzivatele
      if (user.ban) then
      begin
        Self.ClientAuthorise(Sender, false, user, 'Uživatel ' + user.username + ' má BAN !');
        Exit();
      end;

      if (not user.Regulator) then
      begin
        Self.ClientAuthorise(Sender, false, user, 'Uživatel ' + user.username + ' nemá právo k řízení lokomotiv !');
        Exit();
      end;

      // kontrola hesla
      if (TUser.ComparePasswd(parsed[5], user.password, user.salt)) then
      begin
        Self.ClientAuthorise(Sender, true, user);
      end else begin
        Self.ClientAuthorise(Sender, false, user, 'Špatné heslo');
      end;
    except
      // error pri parsovani -> oznamime chybu
      Self.ClientAuthorise(Sender, false, nil, 'Neplatné argumenty');
    end;
  end;

  // kontrola autorizace (dalsi prikazy jsou podmineny existujicim pristupem)
  if (not(Sender.Data as TPanelConnData).Regulator) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Uživatel neuatorizován!');
    Exit();
  end;

  // regulator zacina zadost o lokomotivu ze stanice
  // -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
  // odpoved od serveru:
  // -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
  if (parsed[3] = 'PLEASE') then
  begin
    try
      if ((Sender.Data as TPanelConnData).regulator_zadost <> nil) then
      begin
        PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Žádost již probíhá');
        Exit();
      end;
      Area := Areas.Get(parsed[4]);
      if (Area = nil) then
      begin
        PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Tato oblast řízení neexistuje');
        Exit();
      end;
      if (Area.regPlease <> nil) then
      begin
        PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Do oblasti řízení již probíhá žádost');
        Exit();
      end;
      if (((Sender.Data as TPanelConnData).ping_unreachable) or (not(Sender.Data as TPanelConnData).PingComputed()))
      then
      begin
        PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Zařízení neodpovídá na ping');
        Exit();
      end;

      (Sender.Data as TPanelConnData).regulator_zadost := Area;
      if (parsed.Count > 5) then
        Area.LokoPlease(Sender, (Sender.Data as TPanelConnData).regulator_user, parsed[5])
      else
        Area.LokoPlease(Sender, (Sender.Data as TPanelConnData).regulator_user, '');

      PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok');
    except
      PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Nesprávný formát příkazu');
    end;
  end

  // regulator rusi zadost o lokomotivu ze stanice
  else if (parsed[3] = 'CANCEL') then
  begin
    if ((Sender.Data as TPanelConnData).regulator_zadost = nil) then
      Exit();

    (Sender.Data as TPanelConnData).regulator_zadost.LokoCancel(Sender);
    (Sender.Data as TPanelConnData).regulator_zadost := nil;
    PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok;Žádost zrušena');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;addr;"

procedure TTCPRegulator.ParseLoko(Sender: TIDContext; parsed: TStrings);
var HV: THV;
begin
  parsed[3] := UpperCase(parsed[3]);

  try
    HV := HVDb[StrToInt(parsed[2])];
  except
    Self.ClientError(Sender, 'Nesprávný formát adresy loko');
    Exit();
  end;

  if (not Assigned(HV)) then
  begin
    Self.ClientError(Sender, 'Loko neexistuje');
    Exit();
  end;

  // kontrola opravneni
  if (not(Sender.Data as TPanelConnData).Regulator) then
    Exit();

  if (parsed[3] = 'RELEASE') then
  begin
    // regulator ukoncuje rizeni LOKO
    Self.RemoveLok(Sender, HV, 'Loko odhlášeno');
    Exit();
  end;

  if (parsed[3] = 'PLEASE') then
  begin
    // zadost o prime prevzeti loko -> kontrola nalezitosti

    try
      // autorizovany regulator, ci regulator uzivatele root se nemusi prokazovat tokenem
      if ((not(Sender.Data as TPanelConnData).regulator_user.root) and (not HV.IsReg(Sender))) then
      begin
        // je loko uz na nejakem nerootovskem (!) ovladaci -> odmitnout
        for var i := 0 to HV.state.regulators.Count - 1 do
        begin
          if (not HV.state.regulators[i].root) then
          begin
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Loko je otevřené v jiném regulátoru');
            Exit();
          end;
        end;

        if (parsed.Count < 5) then
        begin
          if (not HV.IsReg(Sender)) then
          begin
            // Uzivatel nema na hnaci vozidlo narok
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Na toto hnací vozidlo nemáte nárok');
            Exit();
          end;
        end else begin
          // kontrola tokenu
          if ((not HV.IsToken(parsed[4]))) then
          begin
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Špatný token');
            Exit();
          end;
        end;
      end; // if regulator_user_root

      if (parsed.Count > 4) then
        HV.RemoveToken(parsed[4]);

      Self.LokToRegulator(Sender, HV);

      Exit();

    except
      PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Nesprávný formát příkazu');
    end;
  end; // PLEASE

  // ---- Authorized loco is required for commands below ----

  // je tento regulator uz v seznamu regulatoru?
  if (not HV.IsReg(Sender)) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Loko ' + parsed[2] + ' neautorizováno');
    Exit();
  end;

  if (parsed[3] = 'EXPECTED-SPEED') then
  begin
    Self.SendExpectedSpeed(Sender, HV);
    Exit();
  end;

  if (parsed[3] = 'NAV') then
  begin
    Self.SendPredictedSignal(Sender, HV);
    Exit();
  end;

  if (HV.stolen) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Loko ' + parsed[2] + ' ukradeno, nenastavuji');
    Exit();
  end;

  // ---- Unstolen loco is required for commands below ----

  if (parsed[3] = 'F') then
  begin
    var left: Integer := 0;
    var right: Integer := 0;

    begin
      var strings: TStrings := TStringList.Create();
      try
        ExtractStrings(['-'], [], PChar(parsed[4]), strings);
        left := StrToInt(strings[0]);
        if (strings.Count > 1) then
          right := StrToInt(strings[1])
        else
          right := left;
      finally
        strings.Free();
      end;
    end;

    begin
      var func: TFunctions := HV.slotFunctions;
      for var i := left to right do
        func[i] := ownConvert.StrToBool(parsed[5][i - left + 1]);
      HV.state.functions := func;
    end;

    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;
    HV.StateFunctionsToSlotFunctions(TTrakce.Callback(PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'STOP') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.EmergencyStop(TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'TOTAL') then
  begin
    HV.ruc := (parsed[4] = '1');
    Exit();
  end;

  // ---- Total control is required for commands below ----

  if (not HV.ruc) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Loko ' + parsed[2] + ' není v ručním řízení');
    Exit();
  end;

  if (parsed[3] = 'SP') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.SetSpeed(StrToInt(parsed[4]), TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SPD') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.SetSpeedDir(StrToInt(parsed[4]), parsed[5] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SP-S') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.SetSpeedStepDir(StrToInt(parsed[4]), HV.direction, TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SPD-S') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.SetSpeedStepDir(StrToInt(parsed[4]), parsed[5] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'D') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := HV.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    HV.SetDirection(parsed[4] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// je volano, pokud chceme rict klientovi, ze jsme mu zmenili stav autorizace
procedure TTCPRegulator.ClientAuthorise(conn: TIDContext; state: Boolean; user: TObject; comment: string = '');
var str: string;
begin
  (conn.Data as TPanelConnData).regulator := state;
  (conn.Data as TPanelConnData).regulator_user := TUser(user);

  if (state) then
  begin
    if (Assigned(user)) then
      str := TUser(user).username
    else
      str := 'ano';

    F_Main.LV_Clients.Items[(conn.Data as TPanelConnData).index].SubItems[_LV_CLIENTS_COL_REGULATOR] := str;
    authLog('reg', 'login', str, 'Login to regulator');
    PanelServer.SendLn(conn, '-;LOK;G;AUTH;ok;' + comment);
  end else begin
    (conn.Data as TPanelConnData).regulator_user := nil;
    F_Main.LV_Clients.Items[(conn.Data as TPanelConnData).index].SubItems[_LV_CLIENTS_COL_REGULATOR] := '';
    authLog('reg', 'deny', str, comment);
    PanelServer.SendLn(conn, '-;LOK;G;AUTH;not;' + comment);

    // odhlasime vsechny prihlasene regulatory
    TPanelConnData(conn.Data).regulator_loks.Clear();
    HVDb.RemoveRegulator(conn);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.ClientError(conn: TIDContext; error: string);
begin
  try
    PanelServer.SendLn(conn, '-;LOK;G;ERR;' + error)
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.PanelLOKResponseOK(Sender: TObject; Data: Pointer);
var speed: Cardinal;
  HV: THV;
begin
  try
    HV := HVDb[TLokResponseData(Data^).addr];
    speed := HV.realSpeed;
    if (speed > HV.Data.maxSpeed) then
      speed := HV.Data.maxSpeed;

    PanelServer.SendLn(TLokResponseData(Data^).conn, '-;LOK;' + IntToStr(TLokResponseData(Data^).addr) + ';RESP;ok;;' +
      IntToStr(speed));
    FreeMem(Data);
  except

  end;
end;

procedure TTCPRegulator.PanelLOKResponseErr(Sender: TObject; Data: Pointer);
begin
  try
    PanelServer.SendLn(TLokResponseData(Data^).conn, '-;LOK;' + IntToStr(TLokResponseData(Data^).addr) +
      ';RESP;err;Command error;');
    FreeMem(Data);
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// or;LOK;ADDR;F;F_left-F_right;states          - informace o stavu funkci lokomotivy
// napr.; or;LOK;ADDR;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
procedure TTCPRegulator.LokUpdateFunc(HV: THV; exclude: TObject = nil);
var i: Integer;
  Func: string;
begin
  Func := '';
  for i := 0 to _HV_FUNC_MAX do
    case (HV.slotFunctions[i]) of
      false:
        Func := Func + '0';
      true:
        Func := Func + '1';
    end; // case

  for i := 0 to HV.state.regulators.Count - 1 do
    if (HV.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(HV.state.regulators[i].conn, '-;LOK;' + IntToStr(HV.addr) + ';F;0-' + IntToStr(_HV_FUNC_MAX) +
        ';' + Func + ';');
end;

// or;LOK;ADDR;SPD;sp_km/h;sp_stupne;dir
procedure TTCPRegulator.LokUpdateSpeed(HV: THV; exclude: TObject = nil);
var i: Integer;
begin
  for i := 0 to HV.state.regulators.Count - 1 do
    if (HV.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(HV.state.regulators[i].conn, '-;LOK;' + IntToStr(HV.addr) + ';SPD;' + IntToStr(HV.realSpeed) +
        ';' + IntToStr(HV.speedStep) + ';' + IntToStr(ownConvert.BoolToInt(HV.direction)) + ';');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.LokStolen(HV: THV; exclude: TObject = nil);
var i: Integer;
begin
  for i := 0 to HV.state.regulators.Count - 1 do
    if (HV.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(HV.state.regulators[i].conn, '-;LOK;' + IntToStr(HV.addr) +
        ';AUTH;stolen;Loko ukradeno ovladačem');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.LokUpdateRuc(HV: THV);
var state: string;
begin
  if (HV.ruc) then
    state := '1'
  else
    state := '0';

  for var i := 0 to HV.state.regulators.Count - 1 do
    PanelServer.SendLn(HV.state.regulators[i].conn, '-;LOK;' + IntToStr(HV.addr) + ';TOTAL;' + state);
end;

/// /////////////////////////////////////////////////////////////////////////////

// prirazeni lokomotivy regulatoru
procedure TTCPRegulator.LokToRegulator(Regulator: TIDContext; HV: THV);
var found: Boolean;
    reg: THVRegulator;
begin
  // je tento regulator uz v seznamu regulatoru?
  found := false;
  for var i := 0 to HV.state.regulators.Count - 1 do
    if (HV.state.regulators[i].conn = Regulator) then
    begin
      found := true;
      reg := HV.state.regulators[i];
      break;
    end;

  // ne -> pridat do seznamu regulatoru
  if (not found) then
  begin
    reg.conn := Regulator;
    HV.ruc := HV.ruc or (HV.state.train = -1);
    reg.root := (Regulator.Data as TPanelConnData).regulator_user.root;
    HV.state.regulators.Add(reg);
  end;

  // Je loko prevzato?
  if (not HV.acquired) then
  begin
    // ne -> prevzit loko
    try
      HV.TrakceAcquire(TTrakce.Callback(), TTrakce.Callback());
    except
      on E: Exception do
      begin
        PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(HV.addr) + ';AUTH;not;Převzetí z centrály se nezdařilo :' +
          E.Message);
        HV.state.regulators.Remove(reg);
      end;
    end;

    // timeout 3000ms = 3s
    var timeout: Integer := 0;
    while ((not HV.acquired) or (HV.pom = TPomStatus.progr) or (HV.pom = TPomStatus.error)) do
    begin
      Sleep(1);
      timeout := timeout + 1;
      Application.ProcessMessages;

      if (timeout > 3000) then
      begin
        PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(HV.addr) + ';AUTH;not;Převzetí z centrály se nezdařilo');
        HV.state.regulators.Remove(reg);
        Exit();
      end;
    end; // while
  end else begin
    // odpoved na pozadavek o autorizaci rizeni hnaciho vozidla
    // kdyz loko prebirame, je odesilana automaticky
    if (HV.ruc) then
      PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(HV.addr) + ';AUTH;total;{' + HV.GetPanelLokString() + '}')
    else
      PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(HV.addr) + ';AUTH;ok;{' + HV.GetPanelLokString() + '}');
  end;

  // pridani loko do seznamu autorizovanych loko klientem

  found := false;
  for var tmpHV in TPanelConnData(Regulator.Data).regulator_loks do
  begin
    if (tmpHV = HV) then
    begin
      found := true;
      break;
    end;
  end;

  if (not found) then
  begin
    // pridani nove loko do seznamu
    TPanelConnData(Regulator.Data).regulator_loks.Add(HV);
    PanelServer.GUIQueueLineToRefresh(TPanelConnData(Regulator.Data).index);
  end;

  authLog('reg', 'loco-acquire', TPanelConnData(Regulator.Data).regulator_user.username,
    'Acquire loco ' + IntToStr(HV.addr));
  Self.SendExpectedSpeed(Regulator, HV);
  Self.SendPredictedSignal(Regulator, HV);
end;

/// /////////////////////////////////////////////////////////////////////////////
// odhlasit vsechna hnaci vozidla regulatoru

procedure TTCPRegulator.RegDisconnect(reg: TIDContext; contextDestroyed: Boolean = false);
var addr: Integer;
begin
  for addr := 0 to _MAX_ADDR - 1 do
  begin
    if ((HVDb[addr] <> nil) and (HVDb[addr].state.regulators.Count > 0)) then
    begin
      authLog('reg', 'loco-release', '', 'Release loco ' + IntToStr(HVDb[addr].addr));
      HVDb[addr].RemoveRegulator(reg);
    end;
  end;

  if (not contextDestroyed) then
  begin
    authLog('reg', 'logout', TPanelConnData(reg.Data).regulator_user.username, 'Logout from regulator');
    TPanelConnData(reg.Data).Regulator := false;
    TPanelConnData(reg.Data).regulator_user := nil;
    TPanelConnData(reg.Data).regulator_loks.Clear();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.RemoveLok(Regulator: TIDContext; HV: THV; info: string);
begin
  HV.RemoveRegulator(Regulator);
  TPanelConnData(Regulator.Data).regulator_loks.Remove(HV);
  PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(HV.addr) + ';AUTH;release;' + info);
  PanelServer.GUIQueueLineToRefresh(TPanelConnData(Regulator.Data).index);
  authLog('reg', 'loco-release', TPanelConnData(Regulator.Data).regulator_user.username,
    'Release loco ' + IntToStr(HV.addr));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.SendExpectedSpeed(reg: TIDContext; HV: THV);
begin
  PanelServer.SendLn(reg, '-;LOK;' + IntToStr(HV.addr) + ';EXPECTED-SPEED;' + HV.ExpectedSpeedStr());
end;

procedure TTCPRegulator.SendPredictedSignal(reg: TIDContext; HV: THV);
begin
  PanelServer.SendLn(reg, '-;LOK;' + IntToStr(HV.addr) + ';NAV;' + HV.PredictedSignalStr());
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

TCPRegulator := TTCPRegulator.Create();

finalization

TCPRegulator.Free();

end.// unit
