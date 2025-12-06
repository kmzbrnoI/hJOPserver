unit RegulatorTCP;

// Trida TTCPRegulator se stara o komunikaci s regulatory -- klienty.
// napr. regulator Jerry.

interface

uses Classes, IdContext, AnsiStrings, SysUtils, Forms, TRailVehicle;

type

  TLokResponseData = record
    addr: Word;
    conn: TIDContext;
  end;

  TTCPRegulator = class
  private
    procedure ParseGlobal(Sender: TIDContext; parsed: TStrings);
    procedure ParseVehicle(Sender: TIDContext; parsed: TStrings);

    procedure ClientAuthorise(conn: TIDContext; state: Boolean; user: TObject; comment: string = '');
    procedure ClientError(conn: TIDContext; error: string);

    procedure PanelLOKResponseOK(Sender: TObject; Data: Pointer);
    procedure PanelLOKResponseErr(Sender: TObject; Data: Pointer);

  public

    procedure Parse(Sender: TIDContext; parsed: TStrings);

    procedure VehicleUpdateFunc(vehicle: TRV; exclude: TObject = nil);
    procedure VehicleUpdateSpeed(vehicle: TRV; exclude: TObject = nil);
    procedure VehicleStolen(vehicle: TRV; exclude: TObject = nil);
    procedure VehicleUpdateRuc(vehicle: TRV);

    procedure VehicleToRegulator(Regulator: TIDContext; vehicle: TRV);
    procedure RegDisconnect(reg: TIDContext; contextDestroyed: Boolean = false);
    procedure RemoveVehicle(Regulator: TIDContext; vehicle: TRV; info: string);

    procedure SendExpectedSpeed(reg: TIDContext; vehicle: TRV);
    procedure SendPredictedSignal(reg: TIDContext; vehicle: TRV);

  end;

var
  TCPRegulator: TTCPRegulator;

implementation

uses UserDb, user, TCPServerPanel, TrakceC, TRVDatabase, TrainDb, PanelConnData, Logging,
  fRegulator, fMain, Area, AreaDb, ownConvert, IfThenElse;

/// /////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;"

procedure TTCPRegulator.Parse(Sender: TIDContext; parsed: TStrings);
begin
  parsed[2] := UpperCase(parsed[2]);
  if (parsed[2] = 'G') then
    Self.ParseGlobal(Sender, parsed)
  else
    Self.ParseVehicle(Sender, parsed);
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
        Self.ClientAuthorise(Sender, false, user, 'Uživatel ' + user.username + ' nemá právo k řízení vozidel !');
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

  // regulator zacina zadost o vozidlo z dopravny
  // -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni vozidla z dane oblasti rizeni
  // odpoved od serveru:
  // -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o vozidlo z reliefu; v info je pripadna chybova zprava
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
        Area.VehiclePlease(Sender, (Sender.Data as TPanelConnData).regulator_user, parsed[5])
      else
        Area.VehiclePlease(Sender, (Sender.Data as TPanelConnData).regulator_user, '');

      PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok');
    except
      PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Nesprávný formát příkazu');
    end;
  end

  // regulator rusi zadost o vozidlo z dopravny
  else if (parsed[3] = 'CANCEL') then
  begin
    if ((Sender.Data as TPanelConnData).regulator_zadost = nil) then
      Exit();

    (Sender.Data as TPanelConnData).regulator_zadost.VehicleCancel(Sender);
    (Sender.Data as TPanelConnData).regulator_zadost := nil;
    PanelServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok;Žádost zrušena');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;addr;"

procedure TTCPRegulator.ParseVehicle(Sender: TIDContext; parsed: TStrings);
var vehicle: TRV;
begin
  parsed[3] := UpperCase(parsed[3]);

  try
    vehicle := RVDb[StrToInt(parsed[2])];
  except
    Self.ClientError(Sender, 'Nesprávný formát adresy vozidla');
    Exit();
  end;

  if (not Assigned(vehicle)) then
  begin
    Self.ClientError(Sender, 'Vozidlo neexistuje');
    Exit();
  end;

  // kontrola opravneni
  if (not(Sender.Data as TPanelConnData).Regulator) then
    Exit();

  if (parsed[3] = 'RELEASE') then
  begin
    // regulator ukoncuje rizeni vozidla
    Self.RemoveVehicle(Sender, vehicle, 'Vozidlo odhlášeno');
    Exit();
  end;

  if (parsed[3] = 'PLEASE') then
  begin
    // zadost o prime prevzeti vozidla -> kontrola nalezitosti

    try
      // autorizovany regulator, ci regulator uzivatele root se nemusi prokazovat tokenem
      if ((not(Sender.Data as TPanelConnData).regulator_user.root) and (not vehicle.IsReg(Sender))) then
      begin
        // je vozidlo uz na nejakem nerootovskem (!) ovladaci -> odmitnout
        for var i := 0 to vehicle.state.regulators.Count - 1 do
        begin
          if (not vehicle.state.regulators[i].root) then
          begin
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Vozidlo je otevřené v jiném regulátoru');
            Exit();
          end;
        end;

        if (parsed.Count < 5) then
        begin
          if (not vehicle.IsReg(Sender)) then
          begin
            // Uzivatel nema na vozidlo narok
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Na toto vozidlo nemáte nárok');
            Exit();
          end;
        end else begin
          // kontrola tokenu
          if ((not vehicle.IsToken(parsed[4]))) then
          begin
            PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Špatný token');
            Exit();
          end;
        end;
      end; // if regulator_user_root

      if (parsed.Count > 4) then
        vehicle.RemoveToken(parsed[4]);

      Self.VehicleToRegulator(Sender, vehicle);

      Exit();

    except
      PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';AUTH;not;Nesprávný formát příkazu');
    end;
  end; // PLEASE

  // ---- Authorized loco is required for commands below ----

  // je tento regulator uz v seznamu regulatoru?
  if (not vehicle.IsReg(Sender)) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Vozidlo ' + parsed[2] + ' neautorizováno');
    Exit();
  end;

  if (parsed[3] = 'EXPECTED-SPEED') then
  begin
    Self.SendExpectedSpeed(Sender, vehicle);
    Exit();
  end;

  if (parsed[3] = 'NAV') then
  begin
    Self.SendPredictedSignal(Sender, vehicle);
    Exit();
  end;

  if (vehicle.stolen) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Vozidlo ' + parsed[2] + ' ukradeno, nenastavuji');
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
      var func: TFunctions := vehicle.slotFunctions;
      for var i := left to right do
        func[i] := ownConvert.StrToBool(parsed[5][i - left + 1]);
      vehicle.state.functions := func;
    end;

    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;
    vehicle.StateFunctionsToSlotFunctions(TTrakce.Callback(PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'STOP') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.EmergencyStop(TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'TOTAL') then
  begin
    vehicle.manual := (parsed[4] = '1');
    Exit();
  end;

  // ---- Total control is required for commands below ----

  if (not vehicle.manual) then
  begin
    PanelServer.SendLn(Sender, '-;LOK;' + parsed[2] + ';RESP;err;Vozidlo ' + parsed[2] + ' není v ručním řízení');
    Exit();
  end;

  if (parsed[3] = 'SP') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.SetSpeed(StrToInt(parsed[4]), TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SPD') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.SetSpeedDir(StrToInt(parsed[4]), parsed[5] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SP-S') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.SetSpeedStepDir(StrToInt(parsed[4]), vehicle.direction, TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'SPD-S') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.SetSpeedStepDir(StrToInt(parsed[4]), parsed[5] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
      TTrakce.Callback(Self.PanelLOKResponseErr, LokResponseData), Sender);
    Exit();
  end;

  if (parsed[3] = 'D') then
  begin
    var LokResponseData: Pointer;
    GetMem(LokResponseData, SizeOf(TLokResponseData));
    TLokResponseData(LokResponseData^).addr := vehicle.addr;
    TLokResponseData(LokResponseData^).conn := Sender;

    vehicle.SetDirection(parsed[4] = '1', TTrakce.Callback(Self.PanelLOKResponseOK, LokResponseData),
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

    F_Main.LV_Clients.Items[(conn.Data as TPanelConnData).index].SubItems[F_Main._LV_CLIENTS_COL_REGULATOR] := str;
    authLog('reg', 'login', str, 'Login to regulator');
    PanelServer.SendLn(conn, '-;LOK;G;AUTH;ok;' + comment);
  end else begin
    (conn.Data as TPanelConnData).regulator_user := nil;
    F_Main.LV_Clients.Items[(conn.Data as TPanelConnData).index].SubItems[F_Main._LV_CLIENTS_COL_REGULATOR] := '';
    authLog('reg', 'deny', str, comment);
    PanelServer.SendLn(conn, '-;LOK;G;AUTH;not;' + comment);

    // odhlasime vsechny prihlasene regulatory
    TPanelConnData(conn.Data).regulator_vehicles.Clear();
    RVDb.RemoveRegulator(conn);
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
  vehicle: TRV;
begin
  try
    vehicle := RVDb[TLokResponseData(Data^).addr];
    speed := vehicle.realSpeed;
    if (speed > vehicle.Data.maxSpeed) then
      speed := vehicle.Data.maxSpeed;

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

// or;LOK;ADDR;F;F_left-F_right;states          - informace o stavu funkci vozidla
// napr.; or;LOK;ADDR;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
procedure TTCPRegulator.VehicleUpdateFunc(vehicle: TRV; exclude: TObject = nil);
var func: string;
begin
  func := '';
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    case (vehicle.slotFunctions[i]) of
      false:
        func := Func + '0';
      true:
        func := Func + '1';
    end; // case
  end;

  for var i: Integer := 0 to vehicle.state.regulators.Count - 1 do
    if (vehicle.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(vehicle.state.regulators[i].conn, '-;LOK;' + IntToStr(vehicle.addr) + ';F;0-' + IntToStr(_RV_FUNC_MAX) +
        ';' + Func + ';');
end;

// or;LOK;ADDR;SPD;sp_km/h;sp_stupne;dir
procedure TTCPRegulator.VehicleUpdateSpeed(vehicle: TRV; exclude: TObject = nil);
begin
  for var i: Integer := 0 to vehicle.state.regulators.Count - 1 do
    if (vehicle.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(vehicle.state.regulators[i].conn, '-;LOK;' + IntToStr(vehicle.addr) + ';SPD;' + IntToStr(vehicle.realSpeed) +
        ';' + IntToStr(vehicle.speedStep) + ';' + ownConvert.BoolToStr10(vehicle.direction) + ';');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.VehicleStolen(vehicle: TRV; exclude: TObject = nil);
begin
  for var i: Integer := 0 to vehicle.state.regulators.Count - 1 do
    if (vehicle.state.regulators[i].conn <> exclude) then
      PanelServer.SendLn(vehicle.state.regulators[i].conn, '-;LOK;' + IntToStr(vehicle.addr) +
        ';AUTH;stolen;Vozidlo ukradeno ovladačem');
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.VehicleUpdateRuc(vehicle: TRV);
begin
  for var i := 0 to vehicle.state.regulators.Count - 1 do
    PanelServer.SendLn(vehicle.state.regulators[i].conn, '-;LOK;' + IntToStr(vehicle.addr) + ';TOTAL;' + ite(vehicle.manual, '1', '0'));
end;

/// /////////////////////////////////////////////////////////////////////////////

// prirazeni vozidla regulatoru
procedure TTCPRegulator.VehicleToRegulator(Regulator: TIDContext; vehicle: TRV);
var found: Boolean;
    reg: TRVRegulator;
begin
  // je tento regulator uz v seznamu regulatoru?
  found := false;
  for var i := 0 to vehicle.state.regulators.Count - 1 do
  begin
    if (vehicle.state.regulators[i].conn = Regulator) then
    begin
      found := true;
      reg := vehicle.state.regulators[i];
      break;
    end;
  end;

  // ne -> pridat do seznamu regulatoru
  if (not found) then
  begin
    reg.conn := Regulator;
    vehicle.manual := vehicle.manual or (vehicle.state.train = -1);
    reg.root := (Regulator.Data as TPanelConnData).regulator_user.root;
    vehicle.RegulatorAdd(reg);
  end;

  // Je vozidlo prevzato?
  if (not vehicle.acquired) then
  begin
    // ne -> prevzit vozidlo
    try
      vehicle.TrakceAcquire(TTrakce.Callback(), TTrakce.Callback());
    except
      on E: Exception do
      begin
        PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(vehicle.addr) + ';AUTH;not;Převzetí z centrály se nezdařilo :' +
          E.Message);
        vehicle.RegulatorRemove(reg);
      end;
    end;

    // timeout 3000ms = 3s
    var timeout: Integer := 0;
    while ((not vehicle.acquired) or (vehicle.pom = TPomStatus.progr) or (vehicle.pom = TPomStatus.error)) do
    begin
      Sleep(1);
      timeout := timeout + 1;
      Application.ProcessMessages();

      if (timeout > 3000) then
      begin
        PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(vehicle.addr) + ';AUTH;not;Převzetí z centrály se nezdařilo');
        vehicle.RegulatorRemove(reg);
        Exit();
      end;
    end; // while
  end else begin
    // odpoved na pozadavek o autorizaci rizeni vozidla
    // kdyz vozidlo prebirame, je odesilana automaticky
    var typ: string := ite(vehicle.manual, 'total', 'ok');
    PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(vehicle.addr) + ';AUTH;' + typ + ';{' + vehicle.GetPanelVehicleString() + '}')
  end;

  // pridani vozidla do seznamu autorizovanych vozidel klientem

  found := false;
  for var tmpRV in TPanelConnData(Regulator.Data).regulator_vehicles do
  begin
    if (tmpRV = vehicle) then
    begin
      found := true;
      break;
    end;
  end;

  if (not found) then
  begin
    // pridani noveho vozidla do seznamu
    TPanelConnData(Regulator.Data).regulator_vehicles.Add(vehicle);
    PanelServer.GUIQueueLineToRefresh(TPanelConnData(Regulator.Data).index);
  end;

  authLog('reg', 'loco-acquire', TPanelConnData(Regulator.Data).regulator_user.username,
    'Acquire loco ' + IntToStr(vehicle.addr));
  Self.SendExpectedSpeed(Regulator, vehicle);
  Self.SendPredictedSignal(Regulator, vehicle);
end;

/// /////////////////////////////////////////////////////////////////////////////
// odhlasit vsechna vozidla regulatoru

procedure TTCPRegulator.RegDisconnect(reg: TIDContext; contextDestroyed: Boolean = false);
var addr: Integer;
begin
  for addr := 0 to _MAX_ADDR - 1 do
  begin
    if ((RVDb[addr] <> nil) and (RVDb[addr].state.regulators.Count > 0)) then
    begin
      authLog('reg', 'loco-release', '', 'Release loco ' + IntToStr(RVDb[addr].addr));
      RVDb[addr].RemoveRegulator(reg);
    end;
  end;

  if (not contextDestroyed) then
  begin
    authLog('reg', 'logout', TPanelConnData(reg.Data).regulator_user.username, 'Logout from regulator');
    TPanelConnData(reg.Data).regulator := false;
    TPanelConnData(reg.Data).regulator_user := nil;
    TPanelConnData(reg.Data).regulator_vehicles.Clear();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.RemoveVehicle(Regulator: TIDContext; vehicle: TRV; info: string);
begin
  vehicle.RemoveRegulator(Regulator);
  TPanelConnData(Regulator.Data).regulator_vehicles.Remove(vehicle);
  PanelServer.SendLn(Regulator, '-;LOK;' + IntToStr(vehicle.addr) + ';AUTH;release;' + info);
  PanelServer.GUIQueueLineToRefresh(TPanelConnData(Regulator.Data).index);
  authLog('reg', 'loco-release', TPanelConnData(Regulator.Data).regulator_user.username,
    'Release loco ' + IntToStr(vehicle.addr));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.SendExpectedSpeed(reg: TIDContext; vehicle: TRV);
begin
  PanelServer.SendLn(reg, '-;LOK;' + IntToStr(vehicle.addr) + ';EXPECTED-SPEED;' + vehicle.ExpectedSpeedStr());
end;

procedure TTCPRegulator.SendPredictedSignal(reg: TIDContext; vehicle: TRV);
begin
  PanelServer.SendLn(reg, '-;LOK;' + IntToStr(vehicle.addr) + ';NAV;' + vehicle.PredictedSignalStr());
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

TCPRegulator := TTCPRegulator.Create();

finalization

TCPRegulator.Free();

end.// unit
