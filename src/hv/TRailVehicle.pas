unit TRailVehicle;

{
  -------------------------- implementace ----------------------------------
  ----------------------------  VOZIDLA ------------------------------------
  Vozidlo = Zeleznicni vozidlo = Rail Vehicle.

  Digitalni adresa je jen jen pro cteni a lze ji nastavit jen v ctoru !
  toto je schvalne kvuli zpusobu, kterym jsou RV ulozena v databazi
  pri zmene adresy je potreba RV smazat a znovu vytvorit

  Jak to funguje:
  Vozidlo muze byt ve dvou rezimech:
  a) rizeni automatem
  b) rucni rizeni
  Rezim rizeni je jeden jediny pro cele vozidlo. Vozidlo v rucnim rezimu ma rucni
  POM, vozidlo v automatu ma POM automatu.

  Jak se ziskava rizeni vozidla do klienta? Skrze autorizacni token.
  Jak to funguje: technologie si nemuze dovolit vydat jen tak nekomu rizeni
  vozidla. Kontrolu nad vozidly v kazde stanici ma jeji vypravci, ktery
  dane vozidlo muze pridelit strojvedoucim. Strojvedouci se tedy pripoji k
  serveru, autorizuje a pozada prislsneho dispecera (panel) o vozidlo. Dispecer
  mu prideli vozidlo a strojvedouci ji muze ridit.

  NEBO: dispecer si vyzada od serveru tzv autorizacni token, coz je unikatni
  string vytvoreny pro konkretni vozidlo v konkretni cas. Tento string je
  odeslan dispecerovi. Tento token opravnuje k rizeni vozidla bez zadosti
  dispecera. Pokud ho tedy naprikald dispecer vyda strojvedoucimu, strojvedouci
  na jeho zaklade muze prevzit rizeni vozidla. Token patri vzdy ke konkretnimu
  vozidlu a ma omezenou casovou platnost na nekolik minut.

  Pozn.
  Vsechny funkce spojene s nastavovanim dat RV maji parametr Sender
  kam je vhodne dat bud konkretni regulator, nebo OR v pripade
  regulatoru klienta.
  Informace o zmene dat RV je pak volana do vsech systemu mimo Senderu.
  Tedy napriklad, pokud je vozidlo otevrene v 5 regulatorech a jeste na serveru
  a dojde ke zmene rychlosti v OR1, je informace o zmene rychlosti
  odeslana do OR2, OR3, OR4, OR5 a regulatoru na serveru, nikoliv
  vsak do OR1 (tomu prijde napriklad OK, ci error callback)

  Prebirani vozidla:
  1) Zavolat locoAcquire do Trakce (zjisti vsechny informace o vozidlu)
  2) Nastavit spravny smer a rychlost vozidla (vzhledem k vlaku nebo aktualni)
  3) Nastavit funkce na pozadovane hodnoty
  4) Naprogramovat POM
  Pokud v libovolne casti procesu nastane chyba, je vyvolan Error callback.
}

interface

uses TrakceIFace, Classes, SysUtils, Area, Generics.Collections, IdContext,
  IniFiles, JsonDataObjects, Math;

const
  _RV_FUNC_MAX = 28; // maximalni funkcni cislo; funkce zacinaji na cisle 0
  _TOKEN_TIMEOUT_MIN = 3; // delka platnosti tokenu pro autorizaci vozidla v minutach
  _TOKEN_LEN = 24; // delka autorizacniho tokenu pro prevzeti vozidla
  _DEFAUT_MAX_SPEED = 120; // [km/h]

  _LOK_VERSION_SAVE = '2.1';

type
  // v jakem smeru se nachazi stanoviste A
  TRVSite = (odd = 0, even = 1);
  TRVOptionalSite = (osNo = -1, osOdd = 0, osEven = 1);
  TFunctions = array [0 .. _RV_FUNC_MAX] of Boolean;
  TPomStatus = (unknown = -1, manual = 0, automat = 1, progr = 2, error = 3);

  // typ vozidla
  TRVType = (other = -1, steam = 0, diesel = 1, motor = 2, electro = 3, car = 4);

  // mod posilani dat vozidla klientovi
  // full: s POM
  TRVStringMode = (normal = 0, full = 1);

  TRVPomCV = record // jeden zaznam POM se sklada z
    cv: Word; // oznaceni CV a
    value: Byte; // dat, ktera se maji do CV zapsat.
  end;

  TRVFuncType = (permanent = 0, momentary = 1);

  TRVData = record
    name: string;
    owner: string;
    designation: string;
    note: string;
    typ: TRVType;
    maxSpeed: Cardinal;
    transience: Cardinal;
    multitrackCapable: Boolean;

    POMautomat: TList<TRVPomCV>; // seznam POM pri prevzeti do automatu
    POMmanual: TList<TRVPomCV>; // seznam POM pri uvolneni do rucniho rizeni
    POMrelease: TPomStatus; // [automat, manual] (other values are invalid) - POM to set when releasing engine

    funcDescription: array [0 .. _RV_FUNC_MAX] of string; // seznam popisu funkci vozidla
    funcType: array [0 .. _RV_FUNC_MAX] of TRVFuncType; // typy funkci vozidla
  end;

  TRVRegulator = record // jeden regulator -- klient -- je z pohledu vozidla reprezentovan
    conn: TIdContext; // fyzickym spojenim k tomu regulatoru -- klientu a
    root: Boolean; // tages, jestli je uzivatel za timto regulatorem root
  end;

  TRVToken = record // jeden token opravnujici prevzeti rizeni vozidla
    timeout: TDateTime; // cas expirace tokenu (obvykle 3 minuty od zadosti)
    token: string; // samotny token
  end;

  TRVState = record
    siteA: TRVSite;
    traveled_forward: Real; // in meters
    traveled_backward: Real; // in meters
    functions: TFunctions; // stav funkci tak, jak je chceme; uklada se do souboru
    train: Integer; // index vlaku; -1 pokud neni na vlaku
    area: TArea;
    regulators: TList<TRVRegulator>; // seznam regulatoru -- klientu
    tokens: TList<TRVToken>;
    manual: Boolean;
    last_used: TDateTime;
    acquired: Boolean;
    stolen: Boolean; // is false if stolen
    pom: TPomStatus;
    trakceError: Boolean;
    acquiring: Boolean;
    updating: Boolean;
    lastUpdated: TTime;
    speedPendingCmds: Cardinal; // number of pending set speed commands
  end;

  TRV = class
  private
    faddr: Word; // read-only!
    m_funcDict: TDictionary<string, Integer>; // function description to function number map
    acquiredOk: TCommandCallback; // also used for Update callback
    acquiredErr: TCommandCallback; // also used for Update callback
    releasedOk: TCommandCallback;
    pomOk: TCommandCallback;
    pomErr: TCommandCallback;
    pomTarget: TPomStatus;

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure LoadState(ini: TMemIniFile; section: string);

    procedure SetManual(state: Boolean);
    procedure UpdateFuncDict();
    procedure SetTrain(new: Integer);

    function GetSlotFunctions(): TFunctions;
    function GetRealSpeed(): Cardinal;
    function GetStACurrentDirection(): Boolean;

    procedure TrakceCallbackOk(Sender: TObject; data: Pointer);
    procedure TrakceCallbackErr(Sender: TObject; data: Pointer);
    procedure TrakceCallbackErrSpeed(Sender: TObject; data: Pointer);
    procedure TrakceCallbackErrEmergency(Sender: TObject; data: Pointer);
    procedure TrakceCallbackCallEv(cb: PTCb);
    procedure SlotChanged(Sender: TObject; speedChanged: Boolean; dirChanged: Boolean; funcChanged: Boolean);
    procedure TrakceAcquired(Sender: TObject; LocoInfo: TTrkLocoInfo);
    procedure TrakceAcquiredDirection(Sender: TObject; data: Pointer);
    procedure TrakceAcquiredFunctionsSet(Sender: TObject; data: Pointer);
    procedure TrakceAcquiredPOMSet(Sender: TObject; data: Pointer);
    procedure TrakceAcquiredErr(Sender: TObject; data: Pointer);

    procedure TrakceUpdated(Sender: TObject; LocoInfo: TTrkLocoInfo);
    procedure TrakceUpdatedErr(Sender: TObject; data: Pointer);

    procedure TrakceReleased(Sender: TObject; data: Pointer);
    procedure TrakceReleasedPOM(Sender: TObject; data: Pointer);

    procedure TrakcePOMOK(Sender: TObject; data: Pointer);
    procedure TrakcePOMErr(Sender: TObject; data: Pointer);

    procedure BroadcastRegulators(msg: string);
    procedure SendExpectedSpeed();
    procedure SendPredictedSignal();

    function GetAddrStr(): String;

    function Poms(str: string): TList<TRVPomCV>;
    procedure LoadRVFuncs(str: string);
    class function PomToJson(pom: TRVPomCV): TJsonObject;

    procedure CallCb(cb: TCommandCallback);

  public
    index: Word; // index v seznamu vsech vozidel
    data: TRVData;
    state: TRVState;
    slot: TTrkLocoInfo;
    changed: Boolean; // jestli se zmenil stav RV tak, ze je potraba aktualizaovat tabulku ve F_Main

    constructor Create(data_ini: TMemIniFile; state_ini: TMemIniFile; section: string); overload;
    constructor Create(adresa: Word; data: TRVData; stav: TRVState); overload;
    constructor Create(panel_str: string; Sender: TArea); overload;
    destructor Destroy(); override;

    procedure SaveData(); overload;
    procedure SaveData(const filename: string); overload;
    procedure SaveState(ini: TMemIniFile);

    procedure UpdateFromPanelString(data: string); // nacteni informaci o RV z klienta

    procedure ResetStats();
    function ExportStats(): string;

    function MoveToArea(area: TArea): Integer;
    function GetPanelVehicleString(mode: TRVStringMode = normal): string; // vrati RV ve standardnim formatu pro klienta
    procedure UpdatePanelRuc(send_remove: Boolean = true);
    // aktualizuje informaci o rucnim rizeni do panelu (cerny text na bilem pozadi dole na panelu)

    procedure RemoveRegulator(conn: TIdContext); // smaze regulator -- klienta; je volano jen jako callback regulatoru!
    function IsReg(conn: TIdContext): Boolean; // je na tomto RV tento regulator ?
    procedure UpdateAllRegulators();
    procedure ForceRemoveAllRegulators();

    function GetToken(): string;
    function IsToken(str: string): Boolean;
    procedure RemoveToken(token: string);
    procedure UpdateTokenTimeout(); // aktualizace vyprseni platnosti tokenu, melo by byt volano periodicky

    function CanPlayHouk(sound: string): Boolean; // vraci true pokud je povoleno prehravani zvuku
    procedure CheckRelease();
    procedure RecordUseNow();
    function NiceName(): string;
    function ShouldAcquire(): Boolean;
    procedure UpdateTraveled(period: Cardinal);

    procedure SetSpeed(speed: Integer; ok: TCb; err: TCb; Sender: TObject = nil); overload;
    procedure SetSpeed(speed: Integer; Sender: TObject = nil); overload;
    procedure SetDirection(dir: Boolean; ok: TCb; err: TCb; Sender: TObject = nil); overload;
    procedure SetDirection(dir: Boolean; Sender: TObject = nil); overload;
    procedure SetSpeedDir(speed: Integer; direction: Boolean; ok: TCb; err: TCb; Sender: TObject = nil); overload;
    procedure SetSpeedDir(speed: Integer; direction: Boolean; Sender: TObject = nil); overload;
    procedure SetSpeedStepDir(speedStep: Integer; direction: Boolean; ok: TCb; err: TCb;
      Sender: TObject = nil); overload;
    procedure SetSpeedStepDir(speedStep: Integer; direction: Boolean; Sender: TObject = nil); overload;
    procedure SetSingleFunc(func: Integer; state: Boolean; ok: TCb; err: TCb; Sender: TObject = nil);
    procedure EmergencyStop(ok: TCb; err: TCb; Sender: TObject = nil);
    procedure CSReset();

    procedure TrakceAcquire(ok: TCb; err: TCb);
    procedure TrakceRelease(ok: TCb);
    procedure TrakceStolen();
    procedure TrakceUpdateState(ok: TCb; err: TCb);

    procedure StateFunctionsToSlotFunctions(ok: TCb; err: TCb; Sender: TObject = nil);

    procedure SetPom(pom: TPomStatus; ok: TCb; err: TCb);

    function IsTrain(): Boolean;

    procedure OnExpectedSpeedChange();
    function ExpectedSpeedStr(): string;
    procedure OnPredictedSignalChange();
    function PredictedSignalStr(): string;

    procedure RegulatorAdd(reg: TRVRegulator);
    procedure RegulatorRemove(reg: TRVRegulator);
    function DriverFullNames(): string;

    class function CharToRVFuncType(c: char): TRVFuncType;
    class function RVFuncTypeToChar(t: TRVFuncType): char;

    // PT:
    procedure GetPtData(json: TJsonObject; includeState: Boolean);
    procedure GetPtState(json: TJsonObject);
    procedure PostPtState(reqJson: TJsonObject; respJson: TJsonObject);

    property addr: Word read faddr;
    property addrStr: String read GetAddrStr;
    property name: string read data.name;
    property manual: Boolean read state.manual write SetManual;
    property funcDict: TDictionary<string, Integer> read m_funcDict;
    property train: Integer read state.train write SetTrain;
    property speedStep: Byte read slot.step;
    property realSpeed: Cardinal read GetRealSpeed;
    property direction: Boolean read slot.direction;
    property stACurrentDirection: Boolean read GetStACurrentDirection;
    property acquired: Boolean read state.acquired;
    property stolen: Boolean read state.stolen;
    property pom: TPomStatus read state.pom;
    property trakceError: Boolean read state.trakceError;
    property slotFunctions: TFunctions read GetSlotFunctions;
    property stateFunctions: TFunctions read state.functions;
    property acquiring: Boolean read state.acquiring;
    property updating: Boolean read state.updating;
    property lastUpdated: TTime read state.lastUpdated;
  end;

implementation

uses ownStrUtils, AreaDb, TRVDatabase, TrainDb, DataRV, fRegulator, BlockDb,
  RegulatorTCP, fMain, PTUtils, TCPServerPanel, appEv, Logging, TrakceC,
  ownConvert, BlockSignal, IfThenElse, PanelConnData, Config;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRV.Create(data_ini: TMemIniFile; state_ini: TMemIniFile; section: string);
begin
  inherited Create();

  Self.state.regulators := TList<TRVRegulator>.Create();
  Self.state.tokens := TList<TRVToken>.Create();

  Self.state.train := -1;
  Self.state.area := nil;
  Self.CSReset();

  Self.data.name := '';
  Self.data.owner := '';
  Self.data.designation := '';
  Self.data.note := '';
  Self.data.typ := TRVType.other;
  Self.data.maxSpeed := _DEFAUT_MAX_SPEED;
  Self.data.transience := 0;
  Self.data.multitrackCapable := True;
  Self.data.POMautomat := TList<TRVPomCV>.Create();
  Self.data.POMmanual := TList<TRVPomCV>.Create();
  Self.data.POMrelease := TPomStatus.manual;

  for var i := 0 to _RV_FUNC_MAX do
  begin
    Self.data.funcDescription[i] := '';
    Self.data.funcType[i] := TRVFuncType.permanent;
  end;

  Self.acquiredOk := TTrakce.Callback();
  Self.acquiredErr := TTrakce.Callback();

  Self.m_funcDict := TDictionary<string, Integer>.Create();

  try
    Self.LoadData(data_ini, section);
    if (state_ini.SectionExists(section)) then // backward compatibility
      Self.LoadState(state_ini, section)
    else
      Self.LoadState(data_ini, section)
  except
    on E: Exception do
      raise Exception.Create('Chyba při načítání sekce ' + section + ' - ' + E.Message);
  end;
end;

constructor TRV.Create(adresa: Word; data: TRVData; stav: TRVState);
begin
  inherited Create();

  Self.faddr := adresa;
  Self.data := data;
  Self.state := stav;

  Self.acquiredOk := TTrakce.Callback();
  Self.acquiredErr := TTrakce.Callback();

  Self.m_funcDict := TDictionary<string, Integer>.Create();
  Self.UpdateFuncDict();

  if (not Assigned(Self.data.POMautomat)) then
    Self.data.POMautomat := TList<TRVPomCV>.Create();
  if (not Assigned(Self.data.POMmanual)) then
    Self.data.POMmanual := TList<TRVPomCV>.Create();
  if (not Assigned(Self.state.regulators)) then
    Self.state.regulators := TList<TRVRegulator>.Create();
  if (not Assigned(Self.state.tokens)) then
    Self.state.tokens := TList<TRVToken>.Create();

  // Save file explicitly - in panel constructor saving is done in UpdateFromPanelString
  Self.SaveData();
end;

constructor TRV.Create(panel_str: string; Sender: TArea);
begin
  inherited Create();

  Self.ResetStats();

  Self.state.regulators := TList<TRVRegulator>.Create();
  Self.state.tokens := TList<TRVToken>.Create();

  Self.state.train := -1;
  Self.state.area := Sender;
  Self.state.last_used := Now;

  Self.data.POMautomat := TList<TRVPomCV>.Create();
  Self.data.POMmanual := TList<TRVPomCV>.Create();

  Self.m_funcDict := TDictionary<string, Integer>.Create();

  // Also saves to file
  Self.UpdateFromPanelString(panel_str);
end;

destructor TRV.Destroy();
begin
  Self.state.regulators.Free();
  Self.state.tokens.Free();
  Self.data.POMautomat.Free();
  Self.data.POMmanual.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.LoadData(ini: TMemIniFile; section: string);
var addr: Integer;
begin
  addr := StrToInt(section);
  if ((addr < 0) or (addr > 9999)) then
    raise Exception.Create('Adresa vozidla mimo rozsah');
  Self.faddr := addr;

  Self.data.name := ini.ReadString(section, 'nazev', section);
  Self.data.owner := ini.ReadString(section, 'majitel', '');
  Self.data.designation := ini.ReadString(section, 'oznaceni', section);
  Self.data.note := ini.ReadString(section, 'poznamka', '');
  Self.data.typ := TRVType(ini.ReadInteger(section, 'trida', 0));
  Self.data.maxSpeed := ini.ReadInteger(section, 'max_rychlost', _DEFAUT_MAX_SPEED);
  Self.data.transience := ini.ReadInteger(section, 'prechodnost', 0);
  Self.data.multitrackCapable := ini.ReadBool(section, 'multitrakce', True);

  Self.data.POMautomat.Free();
  try
    var sectAut: string := ite(ini.ValueExists(section, 'pom_automat'), 'pom_automat', 'pom_take'); // backward compatibility
    Self.data.POMautomat := Poms(ini.ReadString(section, sectAut, ''));
  except
    Self.data.POMautomat := TList<TRVPomCV>.Create();
    raise;
  end;

  Self.data.POMmanual.Free();
  try
    var sectMan: string := ite(ini.ValueExists(section, 'pom_manual'), 'pom_manual', 'pom_release'); // backward compatibility
    Self.data.POMmanual := Poms(ini.ReadString(section, sectMan, ''));
  except
    Self.data.POMmanual := TList<TRVPomCV>.Create();
    raise;
  end;

  var pomRelease: string := ini.ReadString(section, 'pom_release', 'manual');
  if (pomRelease = 'automat') then
    Self.data.POMrelease := TPomStatus.automat
  else
    Self.data.POMrelease := TPomStatus.manual;


  Self.LoadRVFuncs(ini.ReadString(section, 'func_vyznam', ''));
  Self.UpdateFuncDict();

  // typy funkci:
  var funcTypes: string := ini.ReadString(section, 'func_type', '');
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    if (i < Length(funcTypes)) then
      Self.data.funcType[i] := CharToRVFuncType(funcTypes[i + 1])
    else
      Self.data.funcType[i] := TRVFuncType.permanent;
  end;
end;

procedure TRV.LoadState(ini: TMemIniFile; section: string);
begin
  Self.state.area := Areas.Get(ini.ReadString(section, 'stanice', ''));
  if ((Self.state.area = nil) and (Areas.Count > 0)) then
    Self.state.area := Areas[RVDb.default_or];

  Self.state.traveled_forward := ini.ReadFloat(section, 'najeto_vpred_metru', 0);
  Self.state.traveled_backward := ini.ReadFloat(section, 'najeto_vzad_metru', 0);

  Self.state.siteA := TRVSite(ini.ReadInteger(section, 'stanoviste_a', 0));

  try
    var lastUsed: string := ini.ReadString(section, 'last_used', '');
    if (lastUsed <> '') then
      Self.state.last_used := StrToDateTime(lastUsed)
    else
      Self.state.last_used := 0;
  except
    Self.state.last_used := 0;
  end;

  var funcState: string := ini.ReadString(section, 'stav_funkci', '');
  for var i: Integer := 0 to _RV_FUNC_MAX do
    Self.state.functions[i] := ((i < Length(funcState)) and ownConvert.StrToBool(funcState[i + 1]));
end;

procedure TRV.SaveData(const filename: string);
var ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename, TEncoding.UTF8);

  try
    ini.WriteString('global', 'version', _LOK_VERSION_SAVE);

    var addr: string := IntToStr(Self.addr);
    ini.EraseSection(addr);
    ini.WriteString(addr, 'nazev', Self.data.name);
    ini.WriteString(addr, 'majitel', Self.data.owner);
    ini.WriteString(addr, 'oznaceni', Self.data.designation);
    ini.WriteString(addr, 'poznamka', Self.data.note);
    ini.WriteInteger(addr, 'trida', Integer(Self.data.typ));
    ini.WriteInteger(addr, 'max_rychlost', Self.data.maxSpeed);
    ini.WriteInteger(addr, 'prechodnost', Self.data.transience);
    ini.WriteBool(addr, 'multitrakce', Self.data.multitrackCapable);

    // POM to program for automatic-controlled engines
    var POMautomat: string := '';
    for var pom: TRVPomCV in Self.data.POMautomat do
      POMautomat := POMautomat + '(' + IntToStr(pom.cv) + ',' + IntToStr(pom.value) + ')';
    ini.WriteString(addr, 'pom_automat', POMautomat);

    // POM to program for manually-controlled engines
    var POMmanual: string := '';
    for var pom: TRVPomCV in Self.data.POMmanual do
      POMmanual := POMmanual + '(' + IntToStr(pom.cv) + ',' + IntToStr(pom.value) + ')';
    ini.WriteString(addr, 'pom_manual', POMmanual);

    ini.WriteString(addr, 'pom_release', ite(Self.data.POMrelease = TPomStatus.automat, 'automat', 'manual'));

    // vyznam funkci
    var funcDesc: string := '';
    for var i: Integer := 0 to _RV_FUNC_MAX do
    begin
      if (Self.data.funcDescription[i] <> '') then
        funcDesc := funcDesc + '{' + Self.data.funcDescription[i] + '};'
      else
        funcDesc := funcDesc + ';';
    end;
    ini.WriteString(addr, 'func_vyznam', funcDesc);

    // typ funkci
    var funcType: string := '';
    for var i: Integer := 0 to _RV_FUNC_MAX do
      funcType := funcType + RVFuncTypeToChar(Self.data.funcType[i]);
    ini.WriteString(addr, 'func_type', funcType);

  except
    ini.UpdateFile();
    ini.Free();
    raise;
  end;

  ini.UpdateFile();
  ini.Free();
end;

procedure TRV.SaveData();
begin
  Self.SaveData(RVDb.FilenameForVehicle(Self));
end;

procedure TRV.SaveState(ini: TMemIniFile);
var addr: string;
begin
  if (Self.state.train > -1) then
    Self.RecordUseNow();

  addr := IntToStr(Self.addr);

  if (Self.state.area <> nil) then
    ini.WriteString(addr, 'stanice', Self.state.area.id)
  else
    ini.WriteString(addr, 'stanice', '');

  var fmt := TFormatSettings.Create();
  fmt.DecimalSeparator := '.';
  ini.WriteString(addr, 'najeto_vpred_metru', Format('%.2f', [Self.state.traveled_forward], fmt));
  ini.WriteString(addr, 'najeto_vzad_metru', Format('%.2f', [Self.state.traveled_backward], fmt));

  ini.WriteInteger(addr, 'stanoviste_a', Integer(Self.state.siteA));

  if (Self.state.last_used > 0) then
    ini.WriteString(addr, 'last_used', DateTimeToStr(Self.state.last_used));

  // stav funkci
  var funcState: string := '';
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    if ((Self.state.functions[i]) and (Self.data.funcType[i] <> TRVFuncType.momentary)) then
      funcState := funcState + '1'
    else
      funcState := funcState + '0';
  end;
  ini.WriteString(addr, 'stav_funkci', funcState);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.ResetStats();
begin
  Self.state.traveled_forward := 0;
  Self.state.traveled_backward := 0;
end;

// format vystupnich dat: adresa,nazev,majitel,najeto_metru_vpred,najeto_metru_vzad
function TRV.ExportStats(): string;
begin
  Result := IntToStr(Self.addr) + ',' + Self.data.name + ',' + Self.data.owner + ',' +
    Format('%5.2f', [Self.state.traveled_forward]) + ',' + Format('%5.2f', [Self.state.traveled_backward]) + ',';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.GetPanelVehicleString(mode: TRVStringMode = normal): string;
var
  func: TFunctions;
begin
  Result := Self.data.name + '|' + Self.data.owner + '|' + Self.data.designation + '|{' + Self.data.note + '}|' +
    IntToStr(Self.addr) + '|' + IntToStr(Integer(Self.data.typ)) + '|';

  if (Self.state.train > -1) then
    Result := Result + Trains.GetTrainNameByIndex(Self.state.train) + '|'
  else
    Result := Result + '-|';

  Result := Result + IntToStr(Integer(Self.state.siteA)) + '|';

  if (Self.acquired) then
    func := Self.slotFunctions
  else
    func := Self.state.functions;

  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    if (func[i]) then
      Result := Result + '1'
    else
      Result := Result + '0';
  end;

  Result := Result + '|' + IntToStr(Self.slot.step) + '|' + IntToStr(Self.realSpeed) + '|' +
    ownConvert.BoolToStr10(Self.direction) + '|';
  if (Self.state.area <> nil) then
    Result := Result + Self.state.area.id;
  Result := Result + '|';

  if (mode = TRVStringMode.full) then
  begin
    // cv-automat
    Result := Result + '{';
    for var pomCV: TRVPomCV in Self.data.POMautomat do
      Result := Result + '[{' + IntToStr(pomCV.cv) + '|' + IntToStr(pomCV.value) + '}]';
    Result := Result + '}|{';

    // cv-manual
    for var pomCV: TRVPomCV in Self.data.POMmanual do
      Result := Result + '[{' + IntToStr(pomCV.cv) + '|' + IntToStr(pomCV.value) + '}]';
    Result := Result + '}';
  end else begin
    Result := Result + '|';
  end;

  // vyznamy funkci
  Result := Result + '|{';
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    if (Self.data.funcDescription[i] <> '') then
      Result := Result + '{' + Self.data.funcDescription[i] + '};'
    else
      Result := Result + ';';
  end;
  Result := Result + '}|';

  // typy funkci
  for var i: Integer := 0 to _RV_FUNC_MAX do
    Result := Result + RVFuncTypeToChar(Self.data.funcType[i]);
  Result := Result + '|';

  Result := Result + IntToStr(Self.data.maxSpeed) + '|';
  Result := Result + IntToStr(Self.data.transience) + '|';
  Result := Result + ite(Self.data.POMrelease = TPomStatus.manual, 'manual', 'automat') + '|';
  Result := Result + ownConvert.BoolToStr10(Self.data.multitrackCapable) + '|';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.MoveToArea(area: TArea): Integer;
begin
  // zruseni RUC u stare stanice
  if (Self.state.area <> nil) then
    Self.state.area.BroadcastData('RUC-RM;' + IntToStr(Self.addr));

  // zmena stanice
  Self.state.area := area;

  // RUC do nove stanice
  Self.UpdatePanelRuc(False);

  Self.changed := true;
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.Poms(str: string): TList<TRVPomCV>;
begin
  Result := TList<TRVPomCV>.Create();
  var pomsStrs: TStrings := TStringList.Create();
  var pomStrs: TStrings := TStringList.Create();

  try
    ExtractStringsEx([']', ')'], ['[', '('], str, pomsStrs);
    for var pomStr: string in pomsStrs do
    begin
      pomStrs.Clear();
      ExtractStringsEx(['|', ','], [], pomStr, pomStrs);
      var pomCV: TRVPomCV;
      pomCV.cv := StrToInt(pomStrs[0]);
      if ((pomCV.cv < 1) or (pomCV.cv > 1023)) then
        continue;
      pomCV.value := StrToInt(pomStrs[1]);
      Result.Add(pomCV);
    end;
  except
    Result.Free();
  end;

  pomsStrs.Free();
  pomStrs.Free();
end;

procedure TRV.LoadRVFuncs(str: string);
begin
  var funcDescStrs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([';'], [], str, funcDescStrs);
    for var i: Integer := 0 to _RV_FUNC_MAX do
      if (i < funcDescStrs.Count) then
        Self.data.funcDescription[i] := funcDescStrs[i]
      else
        Self.data.funcDescription[i] := '';
  finally
    funcDescStrs.Free();
  end;
end;

procedure TRV.UpdateFromPanelString(data: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  ExtractStringsEx(['|'], [], data, strs);

  try
    Self.data.name := strs[0];
    Self.data.owner := strs[1];
    Self.data.designation := strs[2];
    Self.data.note := strs[3];
    Self.faddr := StrToInt(strs[4]);
    Self.data.typ := TRVType(StrToInt(strs[5]));
    Self.state.siteA := TRVSite(StrToInt(strs[7]));

    var maxFunc: Integer := Min(Length(strs[8]) - 1, _RV_FUNC_MAX);
    if (maxFunc >= 0) then
      for var i: Integer := 0 to maxFunc do
        Self.state.functions[i] := (strs[8][i + 1] = '1');

    if (strs.Count > 13) then
    begin
      Self.data.POMautomat.Free();
      try
        Self.data.POMautomat := Poms(strs[13]);
      except
        Self.data.POMautomat := TList<TRVPomCV>.Create();
        raise;
      end;
    end;

    if (strs.Count > 14) then
    begin
      Self.data.POMmanual.Free();
      try
        Self.data.POMmanual := Poms(strs[14]);
      except
        Self.data.POMmanual := TList<TRVPomCV>.Create();
        raise;
      end;
    end;

    if (strs.Count > 15) then
      Self.LoadRVFuncs(strs[15])
    else begin
      for var i: Integer := 0 to _RV_FUNC_MAX do
        Self.data.funcDescription[i] := '';
    end;
    Self.UpdateFuncDict();

    if (strs.Count > 16) then
    begin
      // typy funkci
      for var i: Integer := 0 to _RV_FUNC_MAX do
        if (i < Length(strs[16])) then
          Self.data.funcType[i] := CharToRVFuncType(strs[16][i + 1])
        else
          Self.data.funcType[i] := TRVFuncType.permanent;
    end else begin
      for var i: Integer := 0 to _RV_FUNC_MAX do
        Self.data.funcType[i] := TRVFuncType.permanent;
    end;

    if (strs.Count > 17) then
      Self.data.maxSpeed := StrToInt(strs[17]);

    if (strs.Count > 18) then
      Self.data.transience := StrToInt(strs[18]);

    if (strs.Count > 19) then
    begin
      if (strs[19] = 'automat') then
        Self.data.POMrelease := TPomStatus.automat
      else
        Self.data.POMrelease := TPomStatus.manual;
    end;

    if (strs.Count > 20) then
      Self.data.multitrackCapable := ownConvert.StrToBool(strs[20]);

  except
    on E: Exception do
    begin
      raise Exception.Create('Chyba při parsování dat vozidla - ' + E.Message);
      Exit();
    end;
  end;

  Self.changed := true;

  if (Self.state.train > -1) then
    Blocks.ChangeTrainToAllRailways(Trains[Self.state.train]);

  strs.Free();

  // aktulizace vozidel v regulatorech
  Self.UpdateAllRegulators();
  Self.SaveData();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.UpdatePanelRuc(send_remove: Boolean = true);
var train: string;
begin
  if ((Self.data.typ = TRVType.car) or (Self.state.area = nil)) then
    Exit(); // do not report cars

  if (Self.state.train > -1) then
    train := Trains[Self.state.train].name
  else
    train := '-';

  if (Self.stolen) then
  begin
    // vozidlo ukradeno ovladacem
    Self.state.area.BroadcastData('RUC;' + IntToStr(Self.addr) + ';MM. ' + IntToStr(Self.addr) + ' (' + train + ')');
    Exit();
  end else begin
    if (Self.manual) then
    begin
      var msg := 'RUC;' + IntToStr(Self.addr) + ';RUČ. ' + IntToStr(Self.addr) + ' (' + train + ')';
      var drivers: string := Self.DriverFullNames();
      if (drivers <> '') then
         msg := msg + ' - ' + drivers;

      Self.state.area.BroadcastData(msg);
    end else begin
      // vozidlo neni v rucnim rizeni -> oznamit klientovi
      if (send_remove) then
        Self.state.area.BroadcastData('RUC-RM;' + IntToStr(Self.addr));
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// smazani regulatoru
procedure TRV.RemoveRegulator(conn: TIdContext);
begin
  for var i: Integer := 0 to Self.state.regulators.Count - 1 do
  begin
    if (Self.state.regulators[i].conn = conn) then
    begin
      Self.state.regulators.Delete(i);

      // aktualizace rychlosti v pripade, kdy bylo vozidlo rizena rucne (force = true)
      if (Self.state.regulators.Count = 0) then
      begin
        Self.manual := false;
        Self.CheckRelease();
      end;

      Self.UpdatePanelRuc(true);
      if (Self.train > -1) then
      begin
        Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
        Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
      end;
      Exit();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.GetToken(): string;
var token: TRVToken;
begin
  token.token := RandomToken(_TOKEN_LEN);
  token.timeout := Now + EncodeTime(0, _TOKEN_TIMEOUT_MIN, 0, 0);
  Result := token.token;
  Self.state.tokens.Add(token);
end;

function TRV.IsToken(str: string): Boolean;
begin
  for var token: TRVToken in Self.state.tokens do
    if (token.token = str) then
      Exit(true);
  Result := false;
end;

procedure TRV.RemoveToken(token: string);
begin
  for var i: Integer := 0 to Self.state.tokens.Count - 1 do
    if (Self.state.tokens[i].token = token) then
    begin
      Self.state.tokens.Delete(i);
      Exit();
    end;
end;

procedure TRV.UpdateTokenTimeout();
begin
  for var i: Integer := Self.state.tokens.Count - 1 downto 0 do
    if (Now > Self.state.tokens[i].timeout) then
      Self.state.tokens.Delete(i);
end;

/// /////////////////////////////////////////////////////////////////////////////

// timto prikazem je vozidlu zapinano / vypinano rucni rizeni
procedure TRV.SetManual(state: Boolean);
begin
  if (Self.state.manual = state) then
    Exit();
  Self.state.manual := state;

  if (state) then
  begin
    // vozidlo je uvedeno do rucniho rizeni

    // nastavit POM rucniho rizeni
    // neprevzatym RV je POM nastaven pri prebirani; prebirani vozidel ale neni nase starost, to si resi volajici fuknce
    if ((Self.acquired) and (Self.pom <> TPomStatus.manual)) then
      Self.SetPom(TPomStatus.manual, TTrakce.Callback(), TTrakce.Callback());
  end else begin
    // vozidlo je vyjmuto z rucniho rizeni

    if (Self.state.train > -1) then
    begin
      // POM automatu
      if (Self.pom <> TPomStatus.automat) then
        Self.SetPom(TPomStatus.automat, TTrakce.Callback(), TTrakce.Callback());

      Trains[Self.train].speed := Trains[Self.train].speed; // tento prikaz nastavi rychlost
    end else begin
      // vozidlo neni na vlaku -> zkusit odhlasit
      Self.CheckRelease();
    end;
  end;

  if (RegCollector.IsVehicle(Self)) then
    RegCollector.VehicleChanged(Self, Self.addr);
  TCPRegulator.VehicleUpdateRuc(Self);

  // aktualizace informaci do panelu
  Self.UpdatePanelRuc();
  if (Self.train > -1) then
  begin
    Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
    Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.IsReg(conn: TIdContext): Boolean;
begin
  for var reg: TRVRegulator in Self.state.regulators do
    if (reg.conn = conn) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.UpdateAllRegulators();
begin
  for var regulator: TRVRegulator in Self.state.regulators do
    TCPRegulator.VehicleToRegulator(regulator.conn, Self);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  json['addr'] := Self.addr;
  json['name'] := Self.data.name;
  json['owner'] := Self.data.owner;
  json['designation'] := Self.data.designation;
  json['maxSpeed'] := Self.data.maxSpeed;
  if (Self.data.note <> '') then
    json['note'] := Self.data.note;
  json['multitrackCapable'] := Self.data.multitrackCapable;

  case (Self.data.typ) of
    TRVType.other:
      json['type'] := 'other';
    TRVType.steam:
      json['type'] := 'steam';
    TRVType.diesel:
      json['type'] := 'diesel';
    TRVType.motor:
      json['type'] := 'motor';
    TRVType.electro:
      json['type'] := 'electro';
    TRVType.car:
      json['type'] := 'car';
  end;

  var lastFunction: Integer := _RV_FUNC_MAX;
  while ((lastFunction >= 0) and (Self.data.funcDescription[lastFunction] = '')) do
    Dec(lastFunction);

  var types: string := '';
  for var i: Integer := 0 to lastFunction do
  begin
    json.A['funcDescription'].Add(Self.data.funcDescription[i]);
    types := types + RVFuncTypeToChar(Self.data.funcType[i]);
  end;
  json['funcTypes'] := types;

  for var pom: TRVPomCV in Self.data.POMautomat do
    json.A['POMautomat'].Add(Self.PomToJson(pom));
  for var pom: TRVPomCV in Self.data.POMmanual do
    json.A['POMmanual'].Add(Self.PomToJson(pom));
  if ((Self.data.POMautomat.Count > 0) or (Self.data.POMmanual.Count > 0)) then
    json['POMrelease'] := ite(Self.data.POMrelease = TPomStatus.automat, 'automat', 'manual');

  if (includeState) then
    Self.GetPtState(json['lokState']);
end;

procedure TRV.GetPtState(json: TJsonObject);
begin
  json['speedStep'] := Self.speedStep;
  json['realSpeed'] := Self.realSpeed;
  json['direction'] := ite(Self.direction, 'backward', 'forward');

  var funcState: string := '';
  for var i: Integer := 0 to _RV_FUNC_MAX do
    funcState := funcState + ownConvert.BoolToStr10(Self.slotFunctions[i]);
  json['funcState'] := funcState;

  case (Self.state.siteA) of
    TRVSite.odd:
      json['siteA'] := 'L';
    TRVSite.even:
      json['siteA'] := 'S';
  end;

  if (Self.IsTrain()) then
    json['train'] := TrainDb.Trains[Self.train].name;
  if (Self.state.area <> nil) then
    json['area'] := Self.state.area.id;
  json['ruc'] := Self.manual;
  json['lastUsed'] := Self.state.last_used;
  json['acquired'] := Self.acquired;
  json['acquiring'] := Self.acquiring;
  json['stolen'] := Self.stolen;

  json.F['traveledForward'] := Self.state.traveled_forward;
  json.F['traveledBackward'] := Self.state.traveled_backward;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.PostPtState(reqJson: TJsonObject; respJson: TJsonObject);
var speed: Integer;
  newFunctions: TFunctions;
  dir: Boolean;
begin
  if (not Self.acquired) then
  begin
    PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, 403, 'Vozidlo neprevzato');
    Exit();
  end;

  if (reqJson.Contains('direction')) then
    dir := (reqJson['direction'] = 'backward')
  else
    dir := Self.direction;

  if (reqJson.Contains('speedStep')) then
  begin
    speed := StrToInt(reqJson['speedStep']);
    if (speed > 28) then
      speed := 28;
    if (speed < 0) then
      speed := 0;
    Self.SetSpeedStepDir(speed, dir);
  end else if (reqJson.Contains('realSpeed')) then
  begin
    speed := StrToInt(reqJson['realSpeed']);
    Self.SetSpeedDir(speed, dir);
  end else if (reqJson.Contains('direction')) then
    Self.SetSpeedStepDir(Self.speedStep, dir);

  if (reqJson.Contains('funcState')) then
  begin
    for var i: Integer := 0 to _RV_FUNC_MAX do
    begin
      if (i < Length(reqJson.s['funcState'])) then
        newFunctions[i] := ownConvert.StrToBool(reqJson.s['funcState'][i + 1])
      else
        newFunctions[i] := Self.state.functions[i];
    end;
    Self.state.functions := newFunctions;
    Self.StateFunctionsToSlotFunctions(TTrakce.Callback(), TTrakce.Callback());
  end;

  Self.GetPtState(respJson.O['lokState']);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.UpdateFuncDict();
begin
  Self.funcDict.Clear();
  for var i: Integer := 0 to _RV_FUNC_MAX do
    if (Self.data.funcDescription[i] <> '') then
      Self.funcDict.AddOrSetValue(Self.data.funcDescription[i], i);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.CanPlayHouk(sound: string): Boolean;
begin
  Result := ((Self.state.regulators.Count = 0) and (not Self.stolen) and
    ((not Self.funcDict.ContainsKey(_SOUND_FUNC)) or (Self.state.functions[Self.funcDict[_SOUND_FUNC]])) and
    (Self.funcDict.ContainsKey(sound)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.CheckRelease();
begin
  if ((Self.state.train = -1) and (not Self.manual) and (Self.state.regulators.Count = 0) and
    (not RegCollector.IsVehicle(Self)) and (Self.acquired)) then
  begin
    Self.SetSpeed(0);
    Self.TrakceRelease(trakce.Callback());
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.RecordUseNow();
begin
  Self.state.last_used := Now;
  Self.changed := true;
end;

procedure TRV.SetTrain(new: Integer);
begin
  if (new = Self.train) then
    Exit();

  Self.state.train := new;

  if (new = -1) then
  begin
    Self.CheckRelease();
    Self.RecordUseNow();
  end;

  Self.OnExpectedSpeedChange();
  Self.OnPredictedSignalChange();
  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.NiceName(): string;
begin
  Result := IntToStr(Self.addr) + ' : ' + Self.data.name + ' (' + Self.data.designation + ')';
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.ShouldAcquire(): Boolean;
begin
  Result := ((Self.train > -1) and ((not Self.acquired) or (Self.pom = TPomStatus.error)));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.ForceRemoveAllRegulators();
begin
  for var i: Integer := Self.state.regulators.Count - 1 downto 0 do
  begin
    try
      TCPRegulator.RemoveVehicle(Self.state.regulators[i].conn, Self, 'Násilné odhlášení dispečerem');
    except
      on E: Exception do
        AppEvents.LogException(E, 'TRV.ForceRemoveAllRegulators');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRV.CharToRVFuncType(c: char): TRVFuncType;
begin
  if (UpperCase(c) = 'M') then
    Result := TRVFuncType.momentary
  else
    Result := TRVFuncType.permanent;
end;

class function TRV.RVFuncTypeToChar(t: TRVFuncType): char;
begin
  if (t = TRVFuncType.momentary) then
    Result := 'M'
  else
    Result := 'P';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.SetSpeed(speed: Integer; ok: TCb; err: TCb; Sender: TObject = nil);
begin
  Self.SetSpeedDir(speed, Self.slot.direction, ok, err, Sender);
end;

procedure TRV.SetSpeed(speed: Integer; Sender: TObject = nil);
begin
  Self.SetSpeed(speed, trakce.Callback(), trakce.Callback(), Sender);
end;

procedure TRV.SetDirection(dir: Boolean; ok: TCb; err: TCb; Sender: TObject = nil);
begin
  Self.SetSpeedStepDir(Self.slot.step, dir, ok, err, Sender);
end;

procedure TRV.SetDirection(dir: Boolean; Sender: TObject = nil);
begin
  Self.SetDirection(dir, trakce.Callback(), trakce.Callback(), Sender);
end;

procedure TRV.SetSpeedDir(speed: Integer; direction: Boolean; ok: TCb; err: TCb; Sender: TObject = nil);
begin
  Self.SetSpeedStepDir(trakce.step(speed), direction, ok, err, Sender);
end;

procedure TRV.SetSpeedDir(speed: Integer; direction: Boolean; Sender: TObject = nil);
begin
  Self.SetSpeedDir(speed, direction, trakce.Callback(), trakce.Callback(), Sender);
end;

procedure TRV.SetSpeedStepDir(speedStep: Integer; direction: Boolean; ok: TCb; err: TCb; Sender: TObject = nil);
var cbOk, cbErr: PTCb;
begin
  if ((not Self.acquired) and (not Self.acquiring)) then
  begin
    Self.CallCb(err);
    Exit();
  end;
  if ((Self.direction = direction) and (Self.speedStep = speedStep) and (not Self.acquiring)) then
  begin
    Self.CallCb(ok);
    Exit();
  end;
  if ((Self.stolen) and (not Self.acquiring)) then
  begin
    Log('Vozidlo ' + Self.name + ' ukradeno, nenastavuji rychlost', llInfo);
    Self.CallCb(err);
    Exit();
  end;

  var dirOld: Boolean := Self.direction;
  var stepsOld: Byte := Self.speedStep;

  Self.slot.direction := direction;
  Self.slot.step := speedStep;

  trakce.Callbacks(ok, err, cbOk, cbErr);
  trakce.Log(llCommands, 'Vozidlo ' + Self.name + ': rychlostní stupeň: ' + IntToStr(speedStep) + ', směr: ' +
    ownConvert.BoolToStr10(direction));

  Inc(Self.state.speedPendingCmds);

  try
    trakce.LocoSetSpeed(Self.addr, Self.slot.step, Self.direction, TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
      TTrakce.Callback(Self.TrakceCallbackErrSpeed, cbErr));
  except
    on E: Exception do
    begin
      Self.TrakceCallbackErrSpeed(Self, cbErr);
      AppEvents.LogException(E, 'TRV.SetSpeedStepDir');
    end;
  end;

  Self.SlotChanged(Sender, stepsOld <> speedStep, dirOld <> direction, false);
end;

procedure TRV.SetSpeedStepDir(speedStep: Integer; direction: Boolean; Sender: TObject = nil);
begin
  Self.SetSpeedStepDir(speedStep, direction, trakce.Callback(), trakce.Callback(), Sender);
end;

procedure TRV.SetSingleFunc(func: Integer; state: Boolean; ok: TCb; err: TCb; Sender: TObject = nil);
var cbOk, cbErr: PTCb;
begin
  if ((not Self.acquired) and (not Self.acquiring)) then
  begin
    Self.CallCb(err);
    Exit();
  end;
  if (Self.slotFunctions[func] = state) then
  begin
    Self.CallCb(ok);
    Exit();
  end;
  if ((Self.stolen) and (not Self.acquiring)) then
  begin
    Log('Vozidlo ' + Self.name + ' ukradeno, nenastavuji funkce', llInfo);
    Self.CallCb(err);
    Exit();
  end;

  if (state) then
    Self.slot.functions := Self.slot.functions or (1 shl func)
  else
    Self.slot.functions := Self.slot.functions and (not(1 shl func));

  Self.state.functions[func] := state;
  trakce.Callbacks(ok, err, cbOk, cbErr);
  trakce.Log(llCommands, 'Vozidlo ' + Self.name + ': F' + IntToStr(func) + ': ' + ownConvert.BoolToStr10(state));

  try
    trakce.LocoSetSingleFunc(Self.addr, func, Self.slot.functions, TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
      TTrakce.Callback(Self.TrakceCallbackErr, cbErr));
  except
    on E: Exception do
    begin
      Self.TrakceCallbackErr(Self, cbErr);
      AppEvents.LogException(E, 'TRV.SetSingleFunc');
    end;
  end;

  TCPRegulator.VehicleUpdateFunc(Self, Sender);
  RegCollector.VehicleChanged(Sender, Self.addr);
  Self.changed := true;
end;

procedure TRV.StateFunctionsToSlotFunctions(ok: TCb; err: TCb; Sender: TObject = nil);
begin
  if ((not Self.acquired) and (not Self.acquiring)) then
  begin
    Self.CallCb(err);
    Exit();
  end;
  if ((Self.stolen) and (not Self.acquiring)) then
  begin
    Log('Vozidlo ' + Self.name + ' ukradeno, nenastavuji funkce', llInfo);
    Self.CallCb(err);
    Exit();
  end;

  var funcMask: Cardinal := 0;
  var funcState: Cardinal := 0;
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    if (Self.state.functions[i]) then
      funcState := funcState or (1 shl i);
    if (Self.state.functions[i] <> Self.slotFunctions[i]) then
      funcMask := funcMask or (1 shl i);
  end;

  if (funcMask = 0) then
  begin
    Self.CallCb(ok);
    Exit();
  end;

  trakce.Log(llCommands, 'Vozidlo ' + Self.name + ': změna více funkcí');
  Self.slot.functions := funcState;

  try
    trakce.LocoSetFunc(Self.addr, funcMask, funcState, ok, err);
  except
    Self.CallCb(err);
  end;

  TCPRegulator.VehicleUpdateFunc(Self, Sender);
  RegCollector.VehicleChanged(Sender, Self.addr);
  Self.changed := true;
end;

procedure TRV.EmergencyStop(ok: TCb; err: TCb; Sender: TObject = nil);
var cbOk, cbErr: PTCb;
begin
  if (not Self.acquired) then
  begin
    Self.CallCb(err);
    Exit();
  end;

  Self.slot.step := 0;
  trakce.Callbacks(ok, err, cbOk, cbErr);

  try
    trakce.LocoEmergencyStop(Self.addr, TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
      TTrakce.Callback(Self.TrakceCallbackErrEmergency, cbErr));
  except
    on E: Exception do
    begin
      Self.TrakceCallbackErrEmergency(Self, cbErr);
      AppEvents.LogException(E, 'TRV.EmergencyStop');
    end;
  end;

  Self.SlotChanged(Sender, true, false, false);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.CSReset();
begin
  Self.state.acquiring := false;
  Self.state.updating := false;
  Self.state.lastUpdated := 0;
  Self.state.acquired := false;
  Self.state.stolen := false;
  Self.state.pom := TPomStatus.unknown;
  Self.state.trakceError := false;
  Self.state.speedPendingCmds := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.TrakceCallbackOk(Sender: TObject; data: Pointer);
begin
  if (Self.state.speedPendingCmds > 0) then
    Dec(Self.state.speedPendingCmds);

  Self.TrakceCallbackCallEv(data);

  if (not Self.state.trakceError) then
    Exit();
  Self.state.trakceError := false;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
end;

// General error handler: call error event directly
procedure TRV.TrakceCallbackErr(Sender: TObject; data: Pointer);
begin
  Self.TrakceCallbackCallEv(data);
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
end;

// Specific error handler: call error event only if Self.state.speedPendingCmds = 0,
// call ok event if Self.state.speedPendingCmds > 0.
// This implementation calls ok event in case old speed command is dismissed due to
// arrival of new command (required for e.g. Jerry application).
procedure TRV.TrakceCallbackErrSpeed(Sender: TObject; data: Pointer);
begin
  if (Self.state.speedPendingCmds > 0) then
    Dec(Self.state.speedPendingCmds);

  if (Self.state.speedPendingCmds = 0) then
    Self.TrakceCallbackCallEv(data) // call error callback
  else if (TCb(data^).other <> nil) then // the command was not sent due to other speed pending command
    Self.TrakceCallbackCallEv(PTCb(TCb(data^).other)); // call ok callback

  if (Self.state.trakceError) then
    Exit();

  if (Self.state.speedPendingCmds = 0) then
  begin
    Self.state.trakceError := true;
    Self.changed := true;
    RegCollector.VehicleChanged(Self, Self.addr);
  end;
end;

// Specific error handler for "Emergency Stop" command - set trakce.emergency := True,
// IMMEDIATELY STOP WHOLE RAILWAY.
procedure TRV.TrakceCallbackErrEmergency(Sender: TObject; data: Pointer);
begin
  trakce.emergency := True;
  Self.TrakceCallbackErr(Sender, data);
end;

procedure TRV.TrakceCallbackCallEv(cb: PTCb);
begin
  if (cb <> nil) then
  begin
    Self.CallCb(cb^);
    if (Assigned(cb.other)) then
      FreeMem(cb.other);
    FreeMem(cb);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.GetSlotFunctions(): TFunctions;
var functions: Cardinal;
begin
  functions := Self.slot.functions;
  for var i: Integer := 0 to _RV_FUNC_MAX do
  begin
    Result[i] := (functions AND $1 > 0);
    functions := (functions shr 1);
  end;
end;

function TRV.GetRealSpeed(): Cardinal;
begin
  Result := trakce.speed(Self.slot.step);
  if (Result > Self.data.maxSpeed) then
    Result := Self.data.maxSpeed;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.SlotChanged(Sender: TObject; speedChanged: Boolean; dirChanged: Boolean; funcChanged: Boolean);
begin
  if (speedChanged or dirChanged) then
    TCPRegulator.VehicleUpdateSpeed(Self, Sender);
  if (funcChanged) then
    TCPRegulator.VehicleUpdateFunc(Self, Sender);

  RegCollector.VehicleChanged(Sender, Self.addr);
  Self.changed := true;

  if ((dirChanged) and (Self.train > -1)) then
    if ((Sender <> Trains[Self.train]) and (Trains[Self.train] <> nil)) then
      Trains[Self.train].LokDirChanged();
  // Trains[RV.Stav.train] <> nil muze nastat pri aktualizaci RV na vlaku
  // coz se dede prave tady
end;

/// /////////////////////////////////////////////////////////////////////////////
// ACQUIRING
/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.TrakceAcquire(ok: TCb; err: TCb);
begin
  trakce.Log(llCommands, 'PUT: Loco Acquire: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.RecordUseNow();
  Self.state.acquiring := true;
  Self.acquiredOk := ok;
  Self.acquiredErr := err;
  Self.changed := true;

  try
    trakce.LocoAcquire(Self.addr, Self.TrakceAcquired, TTrakce.Callback(Self.TrakceAcquiredErr));
  except
    Self.TrakceAcquiredErr(Self, nil);
  end;
end;

procedure TRV.TrakceAcquired(Sender: TObject; LocoInfo: TTrkLocoInfo);
var direction: Boolean;
  speedStep: Integer;
begin
  Self.slot := LocoInfo;
  Self.changed := true;

  // pokud ma vlak jasne dany smer, nastavime ho
  // podminka na sipky je tu kvuli prebirani z RUCniho rizeni z XpressNETu
  if ((Self.train > -1) and (not Self.manual) and (Trains[Self.train].sdata.dir_L xor Trains[Self.train].sdata.dir_S)) then
  begin
    // vlak ma zadany prave jeden smer
    direction := ((Trains[Self.train].direction = TRVSite.even) xor (Self.state.siteA = TRVSite.even));
    speedStep := trakce.step(Trains[Self.train].speed);
  end else begin
    direction := Self.slot.direction;
    if (Self.stolen) then
      speedStep := Self.slot.step
    else
      speedStep := 0;
  end;

  // Vzdy nastavit smer, protoze tim prevezmeme vozidlo z rizeni jineho ovladace
  Self.SetSpeedStepDir(speedStep, direction, TTrakce.Callback(Self.TrakceAcquiredDirection),
    TTrakce.Callback(Self.TrakceAcquiredErr));
end;

procedure TRV.TrakceAcquiredDirection(Sender: TObject; data: Pointer);
begin
  // Set functions as we wish
  Self.state.stolen := false;
  Self.StateFunctionsToSlotFunctions(TTrakce.Callback(Self.TrakceAcquiredFunctionsSet),
    TTrakce.Callback(Self.TrakceAcquiredErr));
end;

procedure TRV.TrakceAcquiredFunctionsSet(Sender: TObject; data: Pointer);
begin
  Self.state.manual := (RegCollector.IsVehicle(Self)) or (Self.manual);
  Self.changed := true;

  if (Self.state.manual) then
  begin
    // manual control
    Self.SetPom(TPomStatus.manual, TTrakce.Callback(Self.TrakceAcquiredPOMSet),
      TTrakce.Callback(Self.TrakceAcquiredErr));
  end else begin
    // automatic control
    Self.SetPom(TPomStatus.automat, TTrakce.Callback(Self.TrakceAcquiredPOMSet), TTrakce.Callback(Self.TrakceAcquiredErr));
  end;
end;

procedure TRV.TrakceAcquiredPOMSet(Sender: TObject; data: Pointer);
begin
  // Everything done
  trakce.Log(llCommands, 'Loco Fully Acquired: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.state.acquired := true;
  Self.state.acquiring := false;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);

  Self.UpdatePanelRuc();
  if (Self.train > -1) then
  begin
    Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
    Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
  end;

  // odesleme do regulatoru info o uspesne autorizaci
  // to je dobre tehdy, kdyz je vozidlo prebirano z centraly
  var state: string := ite(Self.manual, 'total', 'ok');
  Self.BroadcastRegulators('AUTH;' + state + ';{' + Self.GetPanelVehicleString() + '}');

  Self.CallCb(Self.acquiredOk);
end;

procedure TRV.TrakceAcquiredErr(Sender: TObject; data: Pointer);
begin
  trakce.Log(llCommands, 'ERR: Loco Not Acquired: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.state.acquiring := false;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
  Self.CallCb(Self.acquiredErr);
end;

/// /////////////////////////////////////////////////////////////////////////////
// RELEASING
/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.TrakceRelease(ok: TCb);
begin
  trakce.Log(llCommands, 'PUT: Loco Release: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.releasedOk := ok;
  Self.state.manual := false;
  Self.RecordUseNow();
  Self.changed := true;

  if (Self.pom <> Self.data.POMrelease) then
    Self.SetPom(Self.data.POMrelease, TTrakce.Callback(Self.TrakceReleasedPOM), TTrakce.Callback(Self.TrakceReleasedPOM))
  else
    Self.TrakceReleasedPOM(Self, nil);
end;

procedure TRV.TrakceReleasedPOM(Sender: TObject; data: Pointer);
begin
  // POM done (we do not care is successfully or unsuccessfully)
  try
    trakce.LocoRelease(Self.addr, TTrakce.Callback(Self.TrakceReleased));
  except
    Self.TrakceReleased(Self, nil);
  end;
end;

procedure TRV.TrakceReleased(Sender: TObject; data: Pointer);
begin
  trakce.Log(llCommands, 'Loco Successfully Released: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.state.acquired := false;
  Self.state.pom := TPomStatus.unknown;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
  Self.CallCb(Self.releasedOk);
end;

/// /////////////////////////////////////////////////////////////////////////////
// POM

procedure TRV.SetPom(pom: TPomStatus; ok: TCb; err: TCb);
var toProgram: TList<TRVPomCV>;
begin
  Self.pomOk := ok;
  Self.pomErr := err;
  Self.pomTarget := pom;
  Self.state.pom := TPomStatus.progr;
  Self.changed := true;

  if (pom = TPomStatus.automat) then
    toProgram := Self.data.POMautomat
  else if (pom = TPomStatus.manual) then
    toProgram := Self.data.POMmanual
  else
    raise Exception.Create('Invalid POM!');

  trakce.POMWriteCVs(Self.addr, toProgram, TTrakce.Callback(Self.TrakcePOMOK), TTrakce.Callback(Self.TrakcePOMErr));
end;

procedure TRV.TrakcePOMOK(Sender: TObject; data: Pointer);
begin
  if (Self.state.trakceError) then
    Self.state.trakceError := false;
  Self.state.pom := Self.pomTarget;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
  Self.CallCb(Self.pomOk);
end;

procedure TRV.TrakcePOMErr(Sender: TObject; data: Pointer);
begin
  Self.state.pom := TPomStatus.error;
  Self.state.trakceError := true;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
  Self.CallCb(Self.pomErr);
end;

/// /////////////////////////////////////////////////////////////////////////////
// UPDATE STATE

procedure TRV.TrakceUpdateState(ok: TCb; err: TCb);
begin
  if (Self.acquiring) then
    raise Exception.Create('Cannot update locoinfo when acquiring!');
  if (Self.updating) then
    raise Exception.Create('Update already in progress!');

  trakce.Log(llCommands, 'PUT: Loco Update Info: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.acquiredOk := ok;
  Self.acquiredErr := err;
  Self.state.updating := true;

  try
    trakce.LocoAcquire(Self.addr, Self.TrakceUpdated, TTrakce.Callback(Self.TrakceUpdatedErr));
  except
    Self.TrakceUpdatedErr(Self, nil);
  end;
end;

procedure TRV.TrakceUpdated(Sender: TObject; LocoInfo: TTrkLocoInfo);
var slotOld: TTrkLocoInfo;
begin
  trakce.Log(llCommands, 'Loco Updated: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');

  slotOld := Self.slot;
  Self.slot := LocoInfo;
  Self.state.updating := false;
  Self.state.trakceError := false;
  Self.state.lastUpdated := Now;

  if (slotOld <> Self.slot) then
  begin
    Self.SlotChanged(Sender, slotOld.step <> Self.slot.step, slotOld.direction <> Self.slot.direction,
      slotOld.functions <> Self.slot.functions);
  end;

  Self.CallCb(Self.acquiredOk);
end;

procedure TRV.TrakceUpdatedErr(Sender: TObject; data: Pointer);
begin
  trakce.Log(llCommands, 'ERR: Loco Not Updated: ' + Self.name + ' (' + IntToStr(Self.addr) + ')');
  Self.state.updating := false;
  Self.state.trakceError := true;
  Self.state.lastUpdated := Now;
  Self.changed := true;
  RegCollector.VehicleChanged(Self, Self.addr);
  Self.CallCb(Self.acquiredErr);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.TrakceStolen();
begin
  // tato situace muze nastat, kdyz odhlasime RV a pak si ho vezme Rocomouse
  if (not Self.acquired) then
    Exit();

  Self.state.acquired := false;
  Self.state.stolen := true;
  RegCollector.VehicleChanged(Self, Self.addr);

  TCPRegulator.VehicleStolen(Self);
  Self.UpdatePanelRuc();
  if (Self.train > -1) then
  begin
    Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
    Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
  end;

  if (Self.state.pom <> Self.data.POMrelease) then
    Self.SetPom(Self.data.POMrelease, TTrakce.Callback(), TTrakce.Callback());
  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.GetStACurrentDirection(): Boolean;
begin
  Result := Self.direction xor ownConvert.IntToBool(Integer(Self.state.siteA));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.UpdateTraveled(period: Cardinal);
begin
  if (Self.speedStep = 0) then
    Exit();

  if (Self.direction = _LOCO_DIR_FORWARD) then
    Self.state.traveled_forward := Self.state.traveled_forward + (Self.realSpeed * period / (3.6 * GlobalConfig.scale))
  else
    Self.state.traveled_backward := Self.state.traveled_backward +
      (Self.realSpeed * period / (3.6 * GlobalConfig.scale));

  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.IsTrain(): Boolean;
begin
  Result := (Self.train > -1) and (Trains[Self.train] <> nil);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.BroadcastRegulators(msg: string);
var reg: TRVRegulator;
begin
  for reg in Self.state.regulators do
    PanelServer.SendLn(reg.conn, '-;LOK;' + IntToStr(Self.addr) + ';' + msg);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.OnExpectedSpeedChange();
begin
  if (Self.state.regulators.Count > 0) then
    Self.SendExpectedSpeed();
end;

function TRV.ExpectedSpeedStr(): string;
begin
  if (Self.IsTrain()) then
  begin
    var direction: Integer := Integer(Trains[Self.train].direction) xor Integer(Self.state.siteA);
    Result := IntToStr(Trains[Self.train].speed) + ';' + IntToStr(direction);
  end else begin
    Result := '-;-';
  end;
end;

procedure TRV.SendExpectedSpeed();
begin
  Self.BroadcastRegulators('EXPECTED-SPEED;' + Self.ExpectedSpeedStr());
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.OnPredictedSignalChange();
begin
  if (Self.state.regulators.Count > 0) then
    Self.SendPredictedSignal();
end;

function TRV.PredictedSignalStr(): string;
var signal: TBlkSignal;
begin
  if (Self.IsTrain()) then
  begin
    signal := TBlkSignal(Trains[Self.train].PredictedSignal());
    if (signal <> nil) then
    begin
      var code: TBlkSignalCode := signal.targetSignal;
      if ((code = ncPosunZaj) or (code = ncPosunNezaj)) then
        code := ncStuj; // do not transmit shunting allowed signals
      Result := signal.name + ';' + IntToStr(Integer(code));
    end
    else
      Result := '-;-';
  end
  else
    Result := '-;-';
end;

procedure TRV.SendPredictedSignal();
begin
  Self.BroadcastRegulators('NAV;' + Self.PredictedSignalStr());
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.GetAddrStr(): String;
begin
  Result := IntToStr(Self.addr);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.RegulatorAdd(reg: TRVRegulator);
begin
  Self.state.regulators.Add(reg);
  Self.UpdatePanelRuc(false);
  if (Self.train > -1) then
  begin
    Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
    Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
  end;
end;

procedure TRV.RegulatorRemove(reg: TRVRegulator);
begin
  Self.state.regulators.Remove(reg);
  Self.UpdatePanelRuc(true);
  if (Self.train > -1) then
  begin
    Blocks.ChangeAllTracksWithTrain(Trains[Self.train]);
    Blocks.ChangeTrainToAllRailways(Trains[Self.train]);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRV.DriverFullNames(): string;
begin
  var userfullnames := TList<string>.Create();
  try
    for var reg in Self.state.regulators do
      if (TPanelConnData(reg.conn.Data).regulator_user <> nil) then
        userfullnames.Add(TPanelConnData(reg.conn.Data).regulator_user.fullName);
    Result := SerializeStrList(userfullnames, true);
  finally
    userfullnames.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TRV.PomToJson(pom: TRVPomCV): TJsonObject;
begin
  Result := TJsonObject.Create();
  Result['cv'] := pom.cv;
  Result['value'] := pom.value;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRV.CallCb(cb: TCommandCallback);
begin
  try
    if (Assigned(cb.callback)) then
      cb.callback(Self, cb.Data);
  except
    on e: Exception do
      AppEvents.LogException(e, 'TRV.CallCb');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
