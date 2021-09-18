unit BlockTurnout;

{ TURNOUT technological block definition. }

interface

uses IniFiles, Block, SysUtils, BlockTrack, Menus, AreaDb,
  Classes, IdContext, Generics.Collections, JsonDataObjects, RCS,
  Area, TechnologieRCS;

{
  intentionalLock:
  intentionalLock is used only on refugees. These turnouts do not have to be directly
  in path of a train so they don't know they should be locked (based on zaver
  of tracks). So we must lock them manually - intentionalLock.
  One turnout can be locked from multiple train paths thus turnout remebers
  number of paths which lock the turnout. When value decrements to 0, turnout
  is unlocked. While unlocking the turnout beware that turnout in coupling can
  have different (> 0) intentionalLock!
}

type
  TTurnoutPosition = (disabled = -5, none = -1, plus = 0, minus = 1, both = 2);
  TTurnoutSetError = (vseInvalidPos, vseInvalidRCSConfig, vseLocked, vseOccupied, vseRCS, vseTimeout);
  ECoupling = class(Exception);
  TTurnoutSetPosErrCb = procedure(Sender: TObject; error: TTurnoutSetError) of object;

  TBlkTurnoutIndication = record
    // Pst / control panel indication
    enabled: Boolean;
    rcsPlus: TRCSAddr;
    rcsMinus: TRCSAddr;
    pstOnly: Boolean;
  end;

  TBlkTurnoutControllers = record
    // Pst / control panel buttons
    enabled: Boolean;
    rcsPlus: TRCSAddr;
    rcsMinus: TRCSAddr;
    pstOnly: Boolean;
  end;

  TBlkTurnoutSettings = record
    RCSAddrs: TRCSAddrs; // order: in+, in-, out+, out-
    coupling: Integer; // coupling turnout id (-1 in case of none); Both coupling turnouts reference each other.
    lock: Integer; // lock id in case of turnout refering to lock, -1 in case of none
    lockPosition: TTurnoutPosition;
    npPlus: Integer; // id of non-profile block for position +
    npMinus: Integer; // id of non-profile block for position -
    posDetection: Boolean;
    indication: TBlkTurnoutIndication;
    controllers: TBlkTurnoutControllers;
  end;

  TBlkTurnoutState = record
    position, positionOld, positionReal, positionSave, positionLock: TTurnoutPosition;
    {
       position = current reporting
       old = last position
       real = state based only on current state of RCS inputs
       save = position to save to file in case of posDetection = false
       lock = in which position to lock turnout in case of lock
    }
    note, lockout: string;
    movingPlus, movingMinus: Boolean;
    intentionalLocks: Integer;
    locks: Cardinal; // number of blocks which gave emergency lock

    movingOKCallback: TNotifyEvent;
    movingErrCallback: TTurnoutSetPosErrCb;
    movingStart: TDateTime;
    movingPanel: TIDContext;
    movingOR: TObject;
    psts: TList<TBlk>;
  end;

  TBlkTurnoutSpnl = record
    track: Integer;
  end;

  TBlkTurnoutInputs = record
    plus: TRCSInputState;
    minus: TRCSInputState;
    constructor Create(plus, minus: TRCSInputState);
  end;

  TBlkTurnout = class(TBlk)
  const
    _def_vyh_stav: TBlkTurnoutState = ( // default state
      position: disabled;
      positionOld: disabled;
      positionReal: disabled;
      positionSave: none;
      positionLock: none;
      note: '';
      lockout: '';
      movingPlus: false;
      movingMinus: false;
      intentionalLocks: 0;
      locks: 0;
      movingOKCallback: nil;
      movingErrCallback: nil;
      movingStart: 0;
      movingPanel: nil;
      movingOR: nil;
    );

    _T_MOVING_TIMEOUT_SEC = 10;
    _T_MOVING_MOCK_SEC = 2;

    _TI_INPLUS = 0;
    _TI_INMINUS = 1;
    _TI_OUTPLUS = 2;
    _TI_OUTMINUS = 3;

  private
    m_settings: TBlkTurnoutSettings;
    m_state: TBlkTurnoutState;
    m_spnl: TBlkTurnoutSpnl;

    m_nullOutput: record
      enabled: Boolean;
      NullOutputTime: System.TDateTime; // 500ms to null outputs
    end;

    m_lock: TBlk;
    m_parent: TBlk;
    m_npPlus: TBlk;
    m_npMinus: TBlk;

    function GetZaver(): TZaver;
    function GetNUZ(): Boolean;
    function GetOccupied(): TTrackState;
    function GetOutputLocked(): Boolean;

    procedure SetNote(note: string);
    procedure SetLockout(lockout: string); overload;

    function GetIntentionalLock(): Boolean;

    procedure UpdatePosition();
    procedure UpdateMovingTimeout();
    procedure UpdateLock();
    procedure Unlock();
    function LockLocked(): Boolean;

    procedure CheckNullOutput();

    procedure PanelMovingErr(Sender: TObject; error: TTurnoutSetError);

    procedure MenuPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuNSPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuNSMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuVylClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuZAVEnableClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuZAVDisableClick(SenderPnl: TIDContext; SenderOR: TObject);

    procedure UPOPlusClick(Sender: TObject);
    procedure UPOMinusClick(Sender: TObject);
    procedure UPONSPlusClick(Sender: TObject);
    procedure UPONSMinusClick(Sender: TObject);

    procedure MenuAdminREDUKClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminPolPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminPolMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminNepolClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminRadPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminRadMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
    procedure MenuAdminRadNepolClick(SenderPnl: TIDContext; SenderOR: TObject);

    procedure PanelPotvrSekvNSPlus(Sender: TIDContext; success: Boolean);
    procedure PanelPotvrSekvNSMinus(Sender: TIDContext; success: Boolean);
    procedure PanelPotvrSekvZAV(Sender: TIDContext; success: Boolean);

    procedure ORVylukaNull(Sender: TIDContext; success: Boolean);

    function IsEmergencyLock(): Boolean;
    procedure SetEmergencyLock(zaver: Boolean);
    function MockInputs(): TBlkTurnoutInputs;

    function GetLock(): TBlk;
    function GetNpPlus(): TBlk;
    function GetNpMinus(): TBlk;
    function IsPositionDetection(): Boolean;
    function GetCoupling(): TBlkTurnout;
    function ShouldBeLocked(withZamek: Boolean = true): Boolean;
    function ShouldBeLockedIgnoreStaveni(): Boolean;

    procedure NpObsazChange(Sender: TObject; data: Integer);
    procedure MapNpEvents();

    procedure StitVylUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
      UPO_EscCallback: TNotifyEvent);

    class function CombineCouplingInputs(first: TRCSInputState; second: TRCSInputState): TRCSInputState;

    function GetRCSInPlus(): TRCSAddr;
    function GetRCSInMinus(): TRCSAddr;
    function GetRCSOutPlus(): TRCSAddr;
    function GetRCSOutMinus(): TRCSAddr;

    procedure ShowIndication();
    procedure ReadContollers();

    procedure PstCheckActive();

  public
    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    // ----- turnout-specific functions -----

    function GetSettings(): TBlkTurnoutSettings;
    procedure SetSettings(data: TBlkTurnoutSettings);

    procedure SetPosition(new: TTurnoutPosition; lock: Boolean = false; nouz: Boolean = false;
      callback_ok: TNotifyEvent = nil; callback_err: TTurnoutSetPosErrCb = nil);
    procedure SetLockout(Sender: TIDContext; lockout: string); overload;
    procedure SetCouplingNoPropag(coupling: Integer);

    procedure IntentionalLock();
    procedure IntentionalUnlock();

    procedure ResetEmLocks();
    procedure DecreaseEmergencyLock(amount: Cardinal);
    function GetInputs(): TBlkTurnoutInputs;

    class function SetErrorToMsg(error: TTurnoutSetError): string;

    procedure PstAdd(pst: TBlk);
    procedure PstRemove(pst: TBlk);
    function PstIsActive(): Boolean;
    function PstIs(): Boolean;

    property state: TBlkTurnoutState read m_state;

    property position: TTurnoutPosition read m_state.position;
    property NUZ: Boolean read GetNUZ;
    property zaver: TZaver read GetZaver;
    property occupied: TTrackState read GetOccupied;
    property note: string read m_state.note write SetNote;
    property lockout: string read m_state.lockout write SetLockout;
    property intentionalLocked: Boolean read GetIntentionalLock;
    property trackID: Integer read m_spnl.track;
    property emLock: Boolean read IsEmergencyLock write SetEmergencyLock;
    property lock: TBlk read GetLock;
    property npBlokPlus: TBlk read GetNpPlus;
    property npBlokMinus: TBlk read GetNpMinus;
    property posDetection: Boolean read IsPositionDetection;
    property outputLocked: Boolean read GetOutputLocked;
    property coupling: TBlkTurnout read GetCoupling;

    property movingPlus: Boolean read m_state.movingPlus write m_state.movingPlus;
    property movingMinus: Boolean read m_state.movingMinus write m_state.movingMinus;

    property rcsInPlus: TRCSAddr read GetRCSInPlus;
    property rcsInMinus: TRCSAddr read GetRCSInMinus;
    property rcsOutPlus: TRCSAddr read GetRCSOutPlus;
    property rcsOutMinus: TRCSAddr read GetRCSOutMinus;

    // Panel:
    procedure PanelMenuClick(SenderPnl: TIDContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIDContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    // PT:
    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

    class function PositionToStr(position: TTurnoutPosition): string;
    class function StrToPosition(c: string): TTurnoutPosition;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses BlockDb, GetSystems, fMain, TJCDatabase, UPO, Graphics, Diagnostics, Math,
  TCPServerPanel, BlockLock, PTUtils, changeEvent, TCPAreasRef, ownConvert,
  IfThenElse, RCSErrors, BlockPst;

constructor TBlkTurnout.Create(index: Integer);
begin
  inherited Create(index);
  Self.m_globSettings.typ := btTurnout;
  Self.m_state := Self._def_vyh_stav;
  Self.m_lock := nil;
  Self.m_parent := nil;
  Self.m_npPlus := nil;
  Self.m_npMinus := nil;
  Self.m_state.psts := TList<TBlk>.Create();
end;

destructor TBlkTurnout.Destroy();
begin
  Self.m_state.psts.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.RCSAddrs := Self.LoadRCS(ini_tech, section);
  Self.m_settings.coupling := ini_tech.ReadInteger(section, 'spojka', -1);
  Self.m_settings.lock := ini_tech.ReadInteger(section, 'zamek', -1);
  Self.m_settings.lockPosition := TTurnoutPosition(ini_tech.ReadInteger(section, 'zamek-pol', 0));
  Self.m_settings.posDetection := ini_tech.ReadBool(section, 'detekcePolohy', true);

  Self.m_settings.npPlus := ini_tech.ReadInteger(section, 'npPlus', -1);
  Self.m_settings.npMinus := ini_tech.ReadInteger(section, 'npMinus', -1);

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.m_state.lockout := ini_stat.ReadString(section, 'vyl', '');

  Self.m_state.positionSave := Self.StrToPosition(ini_stat.ReadString(section, 'poloha', '+'));
  if ((Self.m_state.positionSave <> TTurnoutPosition.plus) and (Self.m_state.positionSave <> TTurnoutPosition.minus))
  then
    Self.m_state.positionSave := TTurnoutPosition.plus;

  var strs: TStrings := Self.LoadAreas(ini_rel, 'V');
  try
    Self.m_spnl.track := ite(strs.Count >= 2, StrToInt(strs[1]), -1);
  finally
    strs.Free();
  end;

  Self.m_settings.indication.enabled := (ini_tech.ReadString(section, 'indRcsPlus', '') <> '');
  if (Self.m_settings.indication.enabled) then
   begin
    Self.m_settings.indication.rcsPlus.Load(ini_tech.ReadString(section, 'indRcsPlus', '0:0'));
    Self.m_settings.indication.rcsMinus.Load(ini_tech.ReadString(section, 'indRcsMinus', '0:0'));
    Self.m_settings.indication.pstOnly := ini_tech.ReadBool(section, 'indPstOnly', false);
   end;

  Self.m_settings.controllers.enabled := (ini_tech.ReadString(section, 'contRcsPlus', '') <> '');
  if (Self.m_settings.controllers.enabled) then
   begin
    Self.m_settings.controllers.rcsPlus.Load(ini_tech.ReadString(section, 'contRcsPlus', '0:0'));
    Self.m_settings.controllers.rcsMinus.Load(ini_tech.ReadString(section, 'contRcsMinus', '0:0'));
    Self.m_settings.controllers.pstOnly := ini_tech.ReadBool(section, 'contPstOnly', false);
   end;

  PushRCSToArea(Self.m_areas, Self.m_settings.RCSAddrs);
end;

procedure TBlkTurnout.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  Self.SaveRCS(ini_tech, section, Self.m_settings.RCSAddrs);

  if (Self.m_settings.coupling > -1) then
    ini_tech.WriteInteger(section, 'spojka', Self.m_settings.coupling);

  if (Self.m_settings.npPlus > -1) then
    ini_tech.WriteInteger(section, 'npPlus', Self.m_settings.npPlus);

  if (Self.m_settings.npMinus > -1) then
    ini_tech.WriteInteger(section, 'npMinus', Self.m_settings.npMinus);

  if (Self.m_settings.lock > -1) then
  begin
    ini_tech.WriteInteger(section, 'zamek', Self.m_settings.lock);
    ini_tech.WriteInteger(section, 'zamek-pol', Integer(Self.m_settings.lockPosition));
  end;

  if (not Self.posDetection) then
    ini_tech.WriteBool(section, 'detekcePolohy', false);

  if (Self.m_settings.indication.enabled) then
   begin
    ini_tech.WriteString(section, 'indRcsPlus', Self.m_settings.indication.rcsPlus.ToString());
    ini_tech.WriteString(section, 'indRcsMinus', Self.m_settings.indication.rcsMinus.ToString());
    ini_tech.WriteBool(section, 'indPstOnly', Self.m_settings.indication.pstOnly);
   end;

  if (Self.m_settings.controllers.enabled) then
   begin
    ini_tech.WriteString(section, 'contRcsPlus', Self.m_settings.controllers.rcsPlus.ToString());
    ini_tech.WriteString(section, 'contRcsMinus', Self.m_settings.controllers.rcsMinus.ToString());
    ini_tech.WriteBool(section, 'contPstOnly', Self.m_settings.controllers.pstOnly);
   end;
end;

procedure TBlkTurnout.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);

  if (Self.m_state.lockout <> '') then
    ini_stat.WriteString(section, 'vyl', Self.m_state.lockout);

  if (not Self.posDetection) then
  begin
    var position: TTurnoutPosition;
    if (Self.position > TTurnoutPosition.disabled) then
      position := Self.position
    else
      position := Self.m_state.positionSave;

    ini_stat.WriteString(section, 'poloha', ite((Self.movingMinus) or (position = TTurnoutPosition.minus), '-', '+'));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Enable();
var enable: Boolean;
begin
  enable := (Self.m_settings.RCSAddrs.Count >= 4);

  if (Self.posDetection) then
  begin
    for var rcsaddr: TRCSAddr in Self.m_settings.RCSAddrs do
      if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
        enable := false;
  end else begin
    for var i: Integer := 2 to Self.m_settings.RCSAddrs.Count - 1 do
      if (not RCSi.IsNonFailedModule(Self.m_settings.RCSAddrs[i].board)) then
        enable := false;
  end;

  if (enable) then
  begin
    if (Self.posDetection) then
      Self.m_state.position := none
    else
      Self.m_state.position := Self.m_state.positionSave;
  end;

  Self.m_state.psts.Clear();

  Self.MapNpEvents();
  Self.Update(); // update will call Change()
end;

procedure TBlkTurnout.Disable();
begin
  if (Self.movingPlus) then
    Self.m_state.positionSave := TTurnoutPosition.plus
  else if (Self.movingMinus) then
    Self.m_state.positionSave := TTurnoutPosition.minus
  else
    Self.m_state.positionSave := Self.m_state.position;

  Self.m_state.position := disabled;
  Self.m_state.psts.Clear();
  Self.Change(true);
end;

procedure TBlkTurnout.Reset();
begin
  Self.m_state.intentionalLocks := 0;
  Self.m_state.movingPlus := false;
  Self.m_state.movingMinus := false;
  Self.m_state.positionLock := TTurnoutPosition.none;
  Self.m_state.locks := 0;
  Self.m_state.psts.Clear();
  Self.ShowIndication();
end;

function TBlkTurnout.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
  if ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddrs.Count >= 2) and
    ((Self.rcsInPlus = addr) or (Self.rcsInMinus = addr))) then
    Exit(true);

  if ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Count >= 4) and
    ((Self.rcsOutPlus = addr) or (Self.rcsOutMinus = addr))) then
    Exit(true);

  if ((portType = TRCSIOType.input) and (Self.m_settings.controllers.enabled) and
      ((addr = Self.m_settings.controllers.rcsPlus) or (addr = Self.m_settings.controllers.rcsMinus))) then
    Exit(true);

  if ((portType = TRCSIOType.output) and (Self.m_settings.indication.enabled) and
      ((addr = Self.m_settings.indication.rcsPlus) or (addr = Self.m_settings.indication.rcsMinus))) then
    Exit(true);

  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Update();
begin
  Self.CheckNullOutput();
  Self.UpdatePosition();
  Self.UpdateMovingTimeout();
  Self.UpdateLock();
  Self.ReadContollers();

  if (Self.m_state.position <> Self.m_state.positionOld) then
  begin
    if ((Self.m_state.positionOld = TTurnoutPosition.disabled) and (Self.outputLocked)) then // apply lock
      Self.SetPosition(Self.m_state.positionLock, true);
    Self.m_state.positionOld := Self.m_state.position;
    Self.Change();
  end;

  inherited Update();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetZaver(): TZaver;
begin
  if (Self.m_spnl.track = -1) then
    Exit(TZaver.no);

  if (((Self.m_parent = nil) and (Self.m_spnl.track <> -1)) or ((Self.m_parent.id <> Self.m_spnl.track))) then
    Blocks.GetBlkByID(Self.m_spnl.track, Self.m_parent);
  if (Self.m_parent <> nil) then
    Result := (Self.m_parent as TBlkTrack).zaver
  else
    Result := TZaver.no;
end;

function TBlkTurnout.GetNUZ(): Boolean;
var tmpBlk: TBlk;
  return: Integer;
begin
  return := Blocks.GetBlkByID(Self.m_spnl.track, tmpBlk);
  if (return < 0) then
    Exit(false);
  if (tmpBlk.typ <> btTrack) then
    Exit(false);

  Result := (TBlkTrack(tmpBlk)).NUZ;
end;

function TBlkTurnout.GetOccupied(): TTrackState;
var tmpBlk: TBlk;
  return: Integer;
begin
  return := Blocks.GetBlkByID(Self.m_spnl.track, tmpBlk);
  if (return < 0) then
    Exit(TTrackState.none);
  if ((tmpBlk.typ <> btTrack) and (tmpBlk.typ <> btRT)) then
    Exit(TTrackState.none);

  Result := (tmpBlk as TBlkTrack).occupied;
end;

function TBlkTurnout.GetOutputLocked(): Boolean;
begin
  Result := (Self.m_state.positionLock > TTurnoutPosition.none);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetSettings(): TBlkTurnoutSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkTurnout.SetSettings(data: TBlkTurnoutSettings);
var Blk: TBlk;
  coupling_old: Integer;
begin
  if (data.coupling = Self.id) then
    raise ECoupling.Create('Nelze mít spojku sám se sebou!');
  coupling_old := Self.m_settings.coupling;

  Self.m_settings.coupling := data.coupling;

  // kontrola navaznosti spojky
  if (data.coupling > -1) then
  begin
    // zkontrolujeme, pokud spojka uz neexistovala a pokud ano, tak ji smazeme
    if (coupling_old > -1) then
    begin
      Blocks.GetBlkByID(coupling_old, Blk);
      if ((Blk <> nil) and (Blk.typ = btTurnout)) then
        (Blk as TBlkTurnout).SetCouplingNoPropag(-1);
    end;

    // pridame spojku do druhe vyhybky
    Blocks.GetBlkByID(data.coupling, Blk);
    if ((Blk = nil) or (Blk.typ <> btTurnout)) then
    begin
      Self.m_settings.coupling := -1;
    end else begin
      var coupling_settings: TBlkTurnoutSettings := (Blk as TBlkTurnout).GetSettings();
      if (coupling_settings.coupling <> Self.id) then
      begin
        if (coupling_settings.coupling <> -1) then
        begin
          Self.m_settings.coupling := -1;
          raise ECoupling.Create('Na výhybce je již jiná spojka!');
        end;

        coupling_settings.coupling := Self.id;
        (Blk as TBlkTurnout).SetSettings(coupling_settings);
      end;
    end;
  end else begin
    // odebereme spojku z druhe vyhybky
    if (coupling_old <> -1) then
    begin
      Blocks.GetBlkByID(coupling_old, Blk);
      if ((Blk <> nil) and (Blk.typ = btTurnout)) then
        (Blk as TBlkTurnout).SetCouplingNoPropag(-1);
    end;
  end;

  if (data.RCSAddrs <> Self.m_settings.RCSAddrs) then
    Self.m_settings.RCSAddrs.Free();
  Self.m_settings := data;

  Self.MapNpEvents();
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.SetNote(note: string);
begin
  Self.m_state.note := note;
  Self.Change();
end;

procedure TBlkTurnout.SetLockout(lockout: string);
begin
  Self.m_state.lockout := lockout;
  Self.Change();
end;

procedure TBlkTurnout.ORVylukaNull(Sender: TIDContext; success: Boolean);
begin
  if (success) then
    Self.lockout := '';
end;

procedure TBlkTurnout.SetLockout(Sender: TIDContext; lockout: string);
begin
  if ((Self.m_state.lockout <> '') and (lockout = '')) then
    PanelServer.ConfirmationSequence(Sender, Self.ORVylukaNull, Self.m_areas[0], 'Zrušení výluky',
      TBlocks.GetBlksList(Self), nil)
  else
    Self.lockout := lockout;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetIntentionalLock(): Boolean;
begin
  Result := (Self.m_state.intentionalLocks > 0);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetInputs(): TBlkTurnoutInputs;
begin
  if (Self.posDetection) then
  begin
    Result.plus := RCSi.GetInput(Self.rcsInPlus);
    Result.minus := RCSi.GetInput(Self.rcsInMinus);
  end else begin
    Result := Self.MockInputs();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.UpdatePosition();
var inp, couplingInp: TBlkTurnoutInputs;
  coupling: TBlkTurnout;
begin
  if (Self.m_settings.RCSAddrs.Count < 4) then
    Exit();

  Blocks.GetBlkByID(Self.m_settings.coupling, TBlk(coupling));
  if ((coupling <> nil) and (coupling.typ <> btTurnout)) then
    Exit();

  // RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-
  try
    inp := Self.GetInputs();
  except
    inp.plus := TRCSInputState.failure;
    inp.minus := TRCSInputState.failure;
  end;

  if ((coupling <> nil) and (inp.plus <> TRCSInputState.failure) and (inp.minus <> TRCSInputState.failure)) then
  begin
    try
      couplingInp := coupling.GetInputs();
    except
      couplingInp.plus := TRCSInputState.failure;
      couplingInp.minus := TRCSInputState.failure;
    end;

    inp.plus := CombineCouplingInputs(inp.plus, couplingInp.plus);
    inp.minus := CombineCouplingInputs(inp.minus, couplingInp.minus);
  end;

  try
    if ((inp.plus = failure) or (inp.minus = failure) or (not RCSi.IsModule(Self.rcsOutPlus.board)) or
      (not RCSi.IsModule(Self.rcsOutMinus.board)) or
      ((coupling <> nil) and ((not RCSi.IsModule(coupling.rcsOutPlus.board)) or
      (not RCSi.IsModule(coupling.rcsOutMinus.board))))) then
    begin
      if (Self.state.position <> TTurnoutPosition.disabled) then
      begin
        Self.m_state.position := TTurnoutPosition.disabled;
        JCDb.Cancel(Self);
      end;
      Exit();
    end;
  except
    if (Self.state.position <> TTurnoutPosition.disabled) then
    begin
      Self.m_state.position := TTurnoutPosition.disabled;
      JCDb.Cancel(Self);
    end;
    Exit();
  end;

  if ((inp.plus = isOff) and (inp.minus = isOff)) then
  begin
    Self.m_state.position := none;

    if ((Self.m_state.position <> Self.m_state.positionReal) and ((Self.zaver > TZaver.no) or (Self.emLock) or
      ((Self.intentionalLocked) and (not Self.m_state.movingPlus) and (not Self.m_state.movingMinus)) or
      (Self.LockLocked())) and (Self.zaver <> TZaver.staveni)) then
    begin
      for var area: TArea in Self.areas do
        Area.BlkWriteError(Self, 'Není koncová poloha ' + Self.m_globSettings.name, 'TECHNOLOGIE');
      JCDb.Cancel(Self);
    end; // if Blokovani

    Self.m_state.positionReal := none;
  end;

  if ((inp.plus = isOn) and (inp.minus = isOff)) then
  begin
    // je-li plus vstup 1
    Self.m_state.positionReal := plus;
    if (Self.m_state.movingMinus) then
      Exit();

    if (Self.m_state.movingPlus) then
    begin
      Self.m_state.position := plus;
      Self.m_state.movingPlus := false;

      // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
      if (Self.coupling <> nil) then
        Self.coupling.Update();

      if (Assigned(Self.m_state.movingOKCallback)) then
      begin
        Self.m_state.movingOKCallback(Self);
        Self.m_state.movingOKCallback := nil;
      end;
      Self.m_state.movingErrCallback := nil;
    end else begin
      if (Self.m_state.position <> Self.m_state.positionReal) then
      begin
        // sem se dostaneme, pokud se vyhybka poprve nalezne neocekavane v poloze +
        Self.m_state.position := plus;

        if ((Self.ShouldBeLocked(false)) or (Self.LockLocked() and (Self.m_settings.lockPosition <> plus))) then
        begin
          for var area: TArea in Self.areas do
            Area.BlkWriteError(Self, 'Ztráta dohledu na výhybce ' + Self.m_globSettings.name, 'TECHNOLOGIE');
          JCDb.Cancel(Self);
        end;
      end;
    end;
  end;

  if ((inp.minus = isOn) and (inp.plus = isOff)) then
  begin
    // je-li minus vstup 1
    Self.m_state.positionReal := minus;
    if (Self.m_state.movingPlus) then
      Exit();

    if (Self.m_state.movingMinus) then
    begin
      Self.m_state.position := minus;
      Self.m_state.movingMinus := false;

      // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
      if (Self.coupling <> nil) then
        Self.coupling.Update();

      if (Assigned(Self.m_state.movingOKCallback)) then
      begin
        Self.m_state.movingOKCallback(Self);
        Self.m_state.movingOKCallback := nil;
      end;
      Self.m_state.movingErrCallback := nil;
    end else begin
      if (Self.m_state.position <> Self.m_state.positionReal) then
      begin
        // sem se dostaneme, pokud se vyhybka nalezne neocekavane v poloze -
        Self.m_state.position := minus;

        if ((Self.ShouldBeLocked(false)) or (Self.LockLocked() and (Self.m_settings.lockPosition <> minus))) then
        begin
          for var area: TArea in Self.areas do
            Area.BlkWriteError(Self, 'Ztráta dohledu na výhybce ' + Self.m_globSettings.name, 'TECHNOLOGIE');
          JCDb.Cancel(Self);
        end;
      end;
    end;
  end;

  // 2 polohy zaroven = deje se neco divneho
  if ((inp.plus = isOn) and (inp.minus = isOn)) then
  begin
    Self.m_state.position := both;

    if ((((Self.ShouldBeLocked()) and (Self.zaver <> TZaver.staveni)) or (Self.LockLocked())) and
      (Self.m_state.positionOld <> both)) then
    begin
      for var area: TArea in Self.areas do
        Area.BlkWriteError(Self, 'Není koncová poloha ' + Self.m_globSettings.name, 'TECHNOLOGIE');
      JCDb.Cancel(Self);
    end; // if Blokovani

    Self.m_state.positionReal := both;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.UpdateMovingTimeout();
begin
  if ((not Self.movingPlus) and (not Self.movingMinus)) then
    Exit();

  // timeout
  if (now > Self.m_state.movingStart + EncodeTime(0, 0, _T_MOVING_TIMEOUT_SEC, 0)) then
  begin
    Self.movingPlus := false;
    Self.movingMinus := false;

    // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
    if (Self.coupling <> nil) then
      Self.coupling.Update();

    if (Assigned(Self.m_state.movingErrCallback)) then
    begin
      Self.m_state.movingErrCallback(Self, vseTimeout);
      Self.m_state.movingErrCallback := nil;
    end;
    Self.m_state.movingOKCallback := nil;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.SetPosition(new: TTurnoutPosition; lock: Boolean = false; nouz: Boolean = false;
  callback_ok: TNotifyEvent = nil; callback_err: TTurnoutSetPosErrCb = nil);
begin
  if (Self.m_settings.RCSAddrs.Count < 4) then
  begin
    if (Assigned(callback_err)) then
      callback_err(Self, vseInvalidRCSConfig);
    Exit();
  end;
  if ((new <> plus) and (new <> minus)) then
  begin
    if (Assigned(callback_err)) then
      callback_err(Self, vseInvalidPos);
    Exit();
  end;

  // V tomto momente je klicove ziskat aktualni polohu vyhybky, jinak by mohlo dojit
  // k zacykleni pri staveni spojek.
  Self.UpdatePosition();
  var coupling: TBlkTurnout := Self.coupling;

  if (new <> Self.m_state.position) then
  begin
    // vstupni podminky se kontroluji jen pro pripad, kdy chceme vyhybku opravdu prestavit
    // zamknout ji muzeme kdykoliv

    // pokud se nerovna moje poloha, nerovna se i poloha spojky -> obsazenost na spojce apod. je problem
    if (Self.ShouldBeLockedIgnoreStaveni()) then
    begin
      if (Assigned(callback_err)) then
        callback_err(Self, vseLocked);
      Exit();
    end;
    if (((Self.occupied = TTrackState.occupied) or ((coupling <> nil) and (coupling.occupied = TTrackState.occupied)))
      and (not nouz)) then
    begin
      if (Assigned(callback_err)) then
        callback_err(Self, vseOccupied);
      Exit();
    end;
  end else begin
    // pokud polohu uz mame, zavolame ok callback
    if (Assigned(callback_ok)) then
      callback_ok(Self);
  end;

  // RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-

  if (lock) then
    Self.m_state.positionLock := new; // before SetOutput to ensure locking of failed RCS modules after restoration

  if (new = plus) then
  begin
    try
      RCSi.SetOutput(Self.rcsOutPlus, 1);
      RCSi.SetOutput(Self.rcsOutMinus, 0);
    except
      if (Assigned(callback_err)) then
        callback_err(Self, vseRCS);
      Exit();
    end;

    if (Self.m_state.position <> plus) then
      Self.m_state.movingPlus := true;
    Self.m_state.movingMinus := false;

    if (Self.m_state.position = minus) then
      Self.m_state.position := none;
  end;

  if (new = minus) then
  begin
    try
      RCSi.SetOutput(Self.rcsOutPlus, 0);
      RCSi.SetOutput(Self.rcsOutMinus, 1);
    except
      if (Assigned(callback_err)) then
        callback_err(Self, vseRCS);
      Exit();
    end;

    Self.m_state.movingPlus := false;
    if (Self.m_state.position <> minus) then
      Self.m_state.movingMinus := true;

    if (Self.m_state.position = plus) then
      Self.m_state.position := none;
  end;

  Self.m_state.movingErrCallback := callback_err;
  Self.m_state.movingOKCallback := callback_ok;
  Self.m_state.movingStart := now;

  if (not lock) then
  begin
    Self.m_nullOutput.enabled := true;
    Self.m_nullOutput.NullOutputTime := now + EncodeTime(0, 0, 0, 500);
  end;

  if (coupling <> nil) then
  begin
    // pokud se jedna o spojku, volame SetPosition i na spojku
    if ((coupling.state.movingPlus <> Self.m_state.movingPlus) or
      (coupling.state.movingMinus <> Self.m_state.movingMinus) or ((lock) and (not coupling.outputLocked))) then
      coupling.SetPosition(new, lock, nouz);
  end;

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Unlock();
begin
  try
    if ((Self.outputLocked) and (RCSi.Started)) then
    begin
      RCSi.SetOutput(Self.rcsOutPlus, 0);
      RCSi.SetOutput(Self.rcsOutMinus, 0);
    end;
  except

  end;

  Self.m_state.positionLock := TTurnoutPosition.none;

  var coupling: TBlkTurnout := Self.coupling;
  if ((coupling <> nil) and (coupling.outputLocked)) then
    coupling.Unlock();

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.CheckNullOutput();
begin
  if (not Self.m_nullOutput.enabled) then
    Exit();

  if (now >= Self.m_nullOutput.NullOutputTime) then
  begin
    try
      RCSi.SetOutput(Self.rcsOutPlus, 0);
      RCSi.SetOutput(Self.rcsOutMinus, 0);
    except

    end;

    Self.m_nullOutput.enabled := false;
    Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.MenuPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if ((Self.note <> '') or (Self.lockout <> '')) then
    Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOPlusClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPOPlusClick(SenderPnl);
  end;
end;

procedure TBlkTurnout.MenuMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if ((Self.note <> '') or (Self.lockout <> '')) then
    Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOMinusClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPOMinusClick(SenderPnl);
  end;
end;

procedure TBlkTurnout.MenuNSPlusClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if ((Self.note <> '') or (Self.lockout <> '')) then
    Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSPlusClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPONSPlusClick(SenderPnl);
  end;
end;

procedure TBlkTurnout.MenuNSMinusClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if ((Self.note <> '') or (Self.lockout <> '')) then
    Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSMinusClick, nil)
  else
  begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPONSMinusClick(SenderPnl);
  end;
end;

procedure TBlkTurnout.UPOPlusClick(Sender: TObject);
begin
  Self.m_state.movingPanel := TIDContext(Sender);
  Self.m_state.movingOR := TPanelConnData(TIDContext(Sender).data).UPO_ref;

  Self.SetPosition(TTurnoutPosition.plus, false, false, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.UPOMinusClick(Sender: TObject);
begin
  Self.m_state.movingPanel := TIDContext(Sender);
  Self.m_state.movingOR := TPanelConnData(TIDContext(Sender).data).UPO_ref;

  Self.SetPosition(TTurnoutPosition.minus, false, false, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.UPONSPlusClick(Sender: TObject);
var Blk: TBlk;
begin
  Self.m_state.movingPanel := TIDContext(Sender);
  Self.m_state.movingOR := TPanelConnData(TIDContext(Sender).data).UPO_ref;

  Blocks.GetBlkByID(Self.trackID, Blk);
  PanelServer.ConfirmationSequence(TIDContext(Sender), Self.PanelPotvrSekvNSPlus,
    (TPanelConnData(TIDContext(Sender).data).UPO_ref as TArea), 'Nouzové stavění do polohy plus',
    TBlocks.GetBlksList(Self), TArea.GetPSPodminky(TArea.GetCSCondition(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkTurnout.UPONSMinusClick(Sender: TObject);
var Blk: TBlk;
begin
  Self.m_state.movingPanel := TIDContext(Sender);
  Self.m_state.movingOR := TPanelConnData(TIDContext(Sender).data).UPO_ref;

  Blocks.GetBlkByID(Self.trackID, Blk);
  PanelServer.ConfirmationSequence(TIDContext(Sender), Self.PanelPotvrSekvNSMinus,
    (TPanelConnData(TIDContext(Sender).data).UPO_ref as TArea), 'Nouzové stavění do polohy mínus',
    TBlocks.GetBlksList(Self), TArea.GetPSPodminky(TArea.GetCSCondition(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkTurnout.MenuStitClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.state.note);
end;

procedure TBlkTurnout.MenuVylClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  PanelServer.Lockut(SenderPnl, Self, Self.state.lockout);
end;

procedure TBlkTurnout.MenuZAVEnableClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  Self.emLock := true;
  if (Self.coupling <> nil) then
    Self.coupling.emLock := true;
end;

procedure TBlkTurnout.MenuZAVDisableClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  PanelServer.ConfirmationSequence(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TArea), 'Zrušení nouzového závěru',
    TBlocks.GetBlksList(Self), nil);
end;

procedure TBlkTurnout.PanelPotvrSekvZAV(Sender: TIDContext; success: Boolean);
begin
  if (success) then
    Self.ResetEmLocks();
end;

procedure TBlkTurnout.MenuAdminREDUKClick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  Self.m_state.intentionalLocks := 0;
  Self.Change();
end;

procedure TBlkTurnout.MenuAdminPolPlusCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  try
    RCSi.SetInput(Self.rcsInPlus, 1);
    RCSi.SetInput(Self.rcsInMinus, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTurnout.MenuAdminPolMinusCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  try
    RCSi.SetInput(Self.rcsInPlus, 0);
    RCSi.SetInput(Self.rcsInMinus, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTurnout.MenuAdminNepolCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  try
    RCSi.SetInput(Self.rcsInPlus, 0);
    RCSi.SetInput(Self.rcsInMinus, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTurnout.MenuAdminRadPlusCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.controllers.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.controllers.rcsPlus, 1);
    RCSi.SetInput(Self.m_settings.controllers.rcsMinus, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTurnout.MenuAdminRadMinusCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.controllers.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.controllers.rcsPlus, 0);
    RCSi.SetInput(Self.m_settings.controllers.rcsMinus, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkTurnout.MenuAdminRadNepolCLick(SenderPnl: TIDContext; SenderOR: TObject);
begin
  if (not Self.m_settings.controllers.enabled) then
    Exit();

  try
    RCSi.SetInput(Self.m_settings.controllers.rcsPlus, 0);
    RCSi.SetInput(Self.m_settings.controllers.rcsMinus, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.ShowPanelMenu(SenderPnl: TIDContext; SenderOR: TObject; rights: TAreaRights): string;
var coupling: TBlkTurnout;
begin
  Result := inherited;

  Blocks.GetBlkByID(Self.m_settings.coupling, TBlk(coupling));

  if (not Self.ShouldBeLocked()) then
  begin
    // na vyhybce neni zaver a menu neni redukovane

    if ((Self.occupied = TTrackState.occupied) or ((coupling <> nil) and (coupling.occupied = TTrackState.occupied)))
    then
    begin
      if (Self.m_state.position = plus) then
        Result := Result + '!NS-,';
      if (Self.m_state.position = minus) then
        Result := Result + '!NS+,';
      if ((Self.m_state.position = both) or (Self.m_state.position = none)) then
        Result := Result + '!NS+,!NS-,-,';
    end else begin
      if (Self.m_state.position = plus) then
        Result := Result + 'S-,';
      if (Self.m_state.position = minus) then
        Result := Result + 'S+,';
      if ((Self.m_state.position = both) or (Self.m_state.position = none)) then
        Result := Result + 'S+,S-,-,';
    end;
  end;

  Result := Result + 'STIT,VYL,';

  if (Self.emLock) then
    Result := Result + '!ZAV<,'
  else if ((Self.position = TTurnoutPosition.plus) or (Self.position = TTurnoutPosition.minus)) then
    Result := Result + 'ZAV>,';

  if (rights >= TAreaRights.superuser) then
  begin
    Result := Result + '-,';
    if (Self.intentionalLocked) then
      Result := Result + '*ZRUŠ REDUKCI,';
  end;

  if (RCSi.simulation) then
  begin
    Result := Result + '-,';

    if (Self.posDetection) then
    begin
      if (Self.position <> TTurnoutPosition.plus) then
        Result := Result + '*POL+,';
      if (Self.position <> TTurnoutPosition.minus) then
        Result := Result + '*POL-,';
      if (Self.position <> TTurnoutPosition.none) then
        Result := Result + '*NEPOL,';
    end;

    if (Self.m_settings.controllers.enabled) then
    begin
      try
        var plus := (RCSi.GetInput(Self.m_settings.controllers.rcsPlus) = isOn);
        var minus := (RCSi.GetInput(Self.m_settings.controllers.rcsMinus) = isOn);

        if (not plus) then
          Result := Result + '*RAD+,';
        if (not minus) then
          Result := Result + '*RAD-,';
        if (plus or minus) then
          Result := Result + '*RAD?,';
      except
        on E: RCSException do begin end;
      end;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.PanelClick(SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  case (Button) of
    F1, F2, ENTER:
      PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end; // case
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkTurnout.PanelMenuClick(SenderPnl: TIDContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'S+') then
    Self.MenuPlusClick(SenderPnl, SenderOR)
  else if (item = 'S-') then
    Self.MenuMinusClick(SenderPnl, SenderOR)
  else if (item = 'NS+') then
    Self.MenuNSPlusClick(SenderPnl, SenderOR)
  else if (item = 'NS-') then
    Self.MenuNSMinusClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR)
  else if (item = 'VYL') then
    Self.MenuVylClick(SenderPnl, SenderOR)
  else if (item = 'ZAV>') then
    Self.MenuZAVEnableClick(SenderPnl, SenderOR)
  else if (item = 'ZAV<') then
    Self.MenuZAVDisableClick(SenderPnl, SenderOR)
  else if (item = 'ZRUŠ REDUKCI') then
    Self.MenuAdminREDUKClick(SenderPnl, SenderOR)
  else if (item = 'POL+') then
    Self.MenuAdminPolPlusCLick(SenderPnl, SenderOR)
  else if (item = 'POL-') then
    Self.MenuAdminPolMinusCLick(SenderPnl, SenderOR)
  else if (item = 'NEPOL') then
    Self.MenuAdminNepolCLick(SenderPnl, SenderOR)
  else if (item = 'RAD+') then
    Self.MenuAdminRadPlusClick(SenderPnl, SenderOR)
  else if (item = 'RAD-') then
    Self.MenuAdminRadMinusClick(SenderPnl, SenderOR)
  else if (item = 'RAD?') then
    Self.MenuAdminRadNepolClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.PanelPotvrSekvNSPlus(Sender: TIDContext; success: Boolean);
begin
  if (not success) then
    Exit();
  Self.SetPosition(plus, false, true, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.PanelPotvrSekvNSMinus(Sender: TIDContext; success: Boolean);
begin
  if (not success) then
    Exit();
  Self.SetPosition(minus, false, true, nil, Self.PanelMovingErr);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Change(now: Boolean = false);
var changed: Boolean;
begin
  changed := false;

  if (not Self.outputLocked) and (Self.ShouldBeLocked()) then
  begin
    // pokud je vyhybka redukovana, nebo je na ni zaver a je v koncove poloze a neni zamkla, zamkneme ji
    if (Self.m_state.position = TTurnoutPosition.plus) then
    begin
      Self.SetPosition(TTurnoutPosition.plus, true);
      changed := true;
    end;
    if (Self.m_state.position = TTurnoutPosition.minus) then
    begin
      Self.SetPosition(TTurnoutPosition.minus, true);
      changed := true;
    end;
  end;

  if ((not Self.ShouldBeLocked()) and (Self.outputLocked)) then
  begin
    Self.Unlock();
    changed := true;
  end;

  Self.ShowIndication();

  if (not changed) then
    inherited Change(now);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.IntentionalLock();
begin
  Inc(Self.m_state.intentionalLocks);

  if (Self.m_state.intentionalLocks = 1) then
    Self.Change();
end;

procedure TBlkTurnout.IntentionalUnlock();
begin
  if (Self.m_state.intentionalLocks > 0) then
    Dec(Self.m_state.intentionalLocks);

  if (Self.m_state.intentionalLocks = 0) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.IsEmergencyLock(): Boolean;
begin
  Result := (Self.state.locks > 0);
end;

procedure TBlkTurnout.SetEmergencyLock(zaver: Boolean);
begin
  if (zaver) then
  begin
    Inc(Self.m_state.locks);
    if (Self.m_state.locks = 1) then
      Self.Change();
  end else begin
    if (Self.state.locks > 0) then
      Dec(Self.m_state.locks);
    if (Self.state.locks = 0) then
    begin
      Self.Change();
      Blocks.NouzZaverZrusen(Self);
    end;
  end;
end;

procedure TBlkTurnout.ResetEmLocks();
begin
  Self.m_state.locks := 0;
  Blocks.NouzZaverZrusen(Self);
  Self.Change();
  if (Self.coupling <> nil) then
    Self.coupling.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

// tato metoda je volana, pokud dojde k timeoutu pri staveni vyhybky z paneli
procedure TBlkTurnout.PanelMovingErr(Sender: TObject; error: TTurnoutSetError);
begin
  if ((Assigned(Self.m_state.movingPanel)) and (Assigned(Self.m_state.movingOR))) then
  begin
    PanelServer.BottomError(Self.m_state.movingPanel, 'Nepřestavena ' + Self.m_globSettings.name + ': ' +
      Self.SetErrorToMsg(error), (Self.m_state.movingOR as TArea).ShortName, 'TECHNOLOGIE');
    Self.m_state.movingPanel := nil;
    Self.m_state.movingOR := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetLock(): TBlk;
begin
  if (((Self.m_lock = nil) and (Self.m_settings.lock <> -1)) or
    ((Self.m_lock <> nil) and (Self.m_lock.id <> Self.m_settings.lock))) then
    Blocks.GetBlkByID(Self.m_settings.lock, Self.m_lock);
  Result := Self.m_lock;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetNpPlus(): TBlk;
begin
  if (((Self.m_npPlus = nil) and (Self.m_settings.npPlus <> -1)) or
    ((Self.m_npPlus <> nil) and (Self.m_npPlus.id <> Self.m_settings.npPlus))) then
    Blocks.GetBlkByID(Self.m_settings.npPlus, Self.m_npPlus);
  Result := Self.m_npPlus;
end;

function TBlkTurnout.GetNpMinus(): TBlk;
begin
  if (((Self.m_npMinus = nil) and (Self.m_settings.npMinus <> -1)) or
    ((Self.m_npMinus <> nil) and (Self.m_npMinus.id <> Self.m_settings.npMinus))) then
    Blocks.GetBlkByID(Self.m_settings.npMinus, Self.m_npMinus);
  Result := Self.m_npMinus;
end;

function TBlkTurnout.IsPositionDetection(): Boolean;
begin
  Result := (Self.m_settings.posDetection) and (Self.m_settings.RCSAddrs.Count >= 2);
end;

/// /////////////////////////////////////////////////////////////////////////////

// pokud je na vyhybku zamek, vyhybka ma nespravnou polohu a klic je v zamku, vyhlasime poruchu zamku
procedure TBlkTurnout.UpdateLock();
begin
  if (Self.LockLocked() and (not(Self.lock as TBlkLock).emLock) and (Self.position <> Self.m_settings.lockPosition) and
    (not(Self.lock as TBlkLock).error)) then
    (Self.lock as TBlkLock).error := true;
end;

/// /////////////////////////////////////////////////////////////////////////////
// PT:

procedure TBlkTurnout.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  if (Self.posDetection) then
  begin
    TBlk.RCStoJSON(Self.rcsInPlus, json['rcs'].O['in+']);
    TBlk.RCStoJSON(Self.rcsInMinus, json['rcs'].O['in-']);
  end;

  TBlk.RCStoJSON(Self.rcsOutPlus, json['rcs'].O['out+']);
  TBlk.RCStoJSON(Self.rcsOutMinus, json['rcs'].O['out-']);

  json['track'] := Self.m_spnl.track;

  if (Self.m_settings.coupling > -1) then
    json['coupling'] := Self.m_settings.coupling;
  if (Self.m_settings.lock > -1) then
  begin
    json['lock'] := Self.m_settings.lock;
    json['lockPosition'] := PositionToStr(Self.m_settings.lockPosition);
  end;

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkTurnout.GetPtState(json: TJsonObject);
begin
  json['position'] := PositionToStr(Self.position);
  if (Self.note <> '') then
    json['note'] := Self.note;
  if (Self.lockout <> '') then
    json['lockout'] := Self.lockout;
end;

procedure TBlkTurnout.PutPtState(reqJson: TJsonObject; respJson: TJsonObject);
begin
  if (reqJson.Contains('position')) then
  begin
    if (Self.position = TTurnoutPosition.disabled) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit neaktivni vyhybku');
      inherited;
      Exit();
    end;
    if (Self.outputLocked) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit zamcenou vyhybku');
      inherited;
      Exit();
    end;
    if (Self.occupied = TTrackState.occupied) then
    begin
      PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit obsazenou vyhybku');
      inherited;
      Exit();
    end;

    // nastaveni polohy vyhybky
    if (reqJson.S['position'] = '+') then
      Self.SetPosition(TTurnoutPosition.plus)
    else if (reqJson.S['position'] = '-') then
      Self.SetPosition(TTurnoutPosition.minus);
  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlkTurnout.PositionToStr(position: TTurnoutPosition): string;
begin
  case (position) of
    TTurnoutPosition.plus:
      Result := '+';
    TTurnoutPosition.minus:
      Result := '-';
    TTurnoutPosition.disabled:
      Result := 'off';
    TTurnoutPosition.none:
      Result := 'none';
    TTurnoutPosition.both:
      Result := 'both';
  else
    Result := '';
  end;
end;

class function TBlkTurnout.StrToPosition(c: string): TTurnoutPosition;
begin
  if (c = '+') then
    Result := TTurnoutPosition.plus
  else if (c = '-') then
    Result := TTurnoutPosition.minus
  else
    Result := TTurnoutPosition.none;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.DecreaseEmergencyLock(amount: Cardinal);
begin
  if (Self.m_state.locks = 0) then
    Exit();

  if (amount > Self.m_state.locks) then
    Self.m_state.locks := 0
  else
    Self.m_state.locks := Self.m_state.locks - amount;

  if (not Self.emLock) then
  begin
    Blocks.NouzZaverZrusen(Self);
    Self.Change();
    if (Self.coupling <> nil) then
      Self.coupling.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.NpObsazChange(Sender: TObject; data: Integer);
begin
  if ((data = 0) and (Sender = Self.npBlokPlus)) then
  begin
    // zmena bloku pro polohu +
    if (TBlkTrack(Self.npBlokPlus).occupied = TTrackState.occupied) then
      TBlkTrack(Self.npBlokPlus).AddChangeEvent(TBlkTrack(Self.npBlokPlus).EventsOnFree,
        CreateChangeEvent(Self.NpObsazChange, 0))
    else
      TBlkTrack(Self.npBlokPlus).AddChangeEvent(TBlkTrack(Self.npBlokPlus).eventsOnOccupy,
        CreateChangeEvent(Self.NpObsazChange, 0));

    if (Self.position = TTurnoutPosition.plus) then
      Self.Change();

  end else if ((data = 1) and (Sender = Self.npBlokMinus)) then
  begin
    // zmena bloku pro polohu -
    if (TBlkTrack(Self.npBlokMinus).occupied = TTrackState.occupied) then
      TBlkTrack(Self.npBlokMinus).AddChangeEvent(TBlkTrack(Self.npBlokMinus).EventsOnFree,
        CreateChangeEvent(Self.NpObsazChange, 1))
    else
      TBlkTrack(Self.npBlokMinus).AddChangeEvent(TBlkTrack(Self.npBlokMinus).eventsOnOccupy,
        CreateChangeEvent(Self.NpObsazChange, 1));

    if (Self.position = TTurnoutPosition.minus) then
      Self.Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.MapNpEvents();
begin
  // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu +
  if (Self.npBlokPlus <> nil) then
  begin
    if (TBlkTrack(Self.npBlokPlus).occupied = TTrackState.occupied) then
      TBlkTrack(Self.npBlokPlus).AddChangeEvent(TBlkTrack(Self.npBlokPlus).EventsOnFree,
        CreateChangeEvent(Self.NpObsazChange, 0))
    else
      TBlkTrack(Self.npBlokPlus).AddChangeEvent(TBlkTrack(Self.npBlokPlus).eventsOnOccupy,
        CreateChangeEvent(Self.NpObsazChange, 0));
  end;

  // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu -
  if (Self.npBlokMinus <> nil) then
  begin
    if (TBlkTrack(Self.npBlokMinus).occupied = TTrackState.occupied) then
      TBlkTrack(Self.npBlokMinus).AddChangeEvent(TBlkTrack(Self.npBlokMinus).EventsOnFree,
        CreateChangeEvent(Self.NpObsazChange, 1))
    else
      TBlkTrack(Self.npBlokMinus).AddChangeEvent(TBlkTrack(Self.npBlokMinus).eventsOnOccupy,
        CreateChangeEvent(Self.NpObsazChange, 1));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.StitVylUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
  UPO_EscCallback: TNotifyEvent);
var UPO: TUPOItems;
  item: TUPOItem;
begin
  UPO := TList<TUPOItem>.Create;
  try
    if (Self.note <> '') then
    begin
      item[0] := GetUPOLine('ŠTÍTEK ' + Self.m_globSettings.name, taCenter, clBlack, clTeal);
      var lines: TStrings := GetLines(Self.note, _UPO_LINE_LEN);

      try
        item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
        if (lines.Count > 1) then
          item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
      finally
        lines.Free();
      end;

      UPO.Add(item);
    end;

    if (Self.lockout <> '') then
    begin
      item[0] := GetUPOLine('VÝLUKA ' + Self.m_globSettings.name, taCenter, clBlack, clOlive);
      var lines: TStrings := GetLines(Self.lockout, _UPO_LINE_LEN);

      try
        item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
        if (lines.Count > 1) then
          item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
      finally
        lines.Free();
      end;

      UPO.Add(item);
    end;

    PanelServer.UPO(SenderPnl, UPO, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
  finally
    UPO.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.SetCouplingNoPropag(coupling: Integer);
begin
  Self.m_settings.coupling := coupling;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.PanelStateString(): string;
var fg, bg: TColor;
  Blk: TBlk;
begin
  Result := inherited;

  // Popredi
  if (not Self.emLock) then
  begin
    case (Self.occupied) of
      TTrackState.disabled:
        fg := clFuchsia;
      TTrackState.none:
        fg := $A0A0A0;
      TTrackState.Free:
        fg := $A0A0A0;
      TTrackState.occupied:
        fg := clRed;
    else
      fg := clFuchsia;
    end;

    if (Self.occupied = TTrackState.Free) then
    begin
      case (Self.zaver) of
        vlak:
          fg := clLime;
        posun:
          fg := clWhite;
        nouz:
          fg := clAqua;
        ab:
          fg := $707070;
      end; // case

      // je soucasti vybarveneho neprofiloveho useku / pst
      Blocks.GetBlkByID(Self.trackID, Blk);
      if ((Blk <> nil) and ((Blk.typ = btTrack) or (Blk.typ = btRT)) and (fg = $A0A0A0)) then
      begin
        if (TBlkTrack(Blk).IsNeprofilJC) then
          fg := clYellow;
        if (TBlkTrack(Blk).PstIsActive()) then
          fg := clBlue;
      end;
    end;

    // do profilu vyhybky zasahuje obsazeny usek
    if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokPlus <> nil) and (Self.position = TTurnoutPosition.plus) and
      (TBlkTrack(Self.npBlokPlus).occupied <> TTrackState.Free)) then
      fg := clYellow;

    // do profilu vyhybky zasahuje obsazeny usek
    if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokMinus <> nil) and (Self.position = TTurnoutPosition.minus) and
      (TBlkTrack(Self.npBlokMinus).occupied <> TTrackState.Free)) then
      fg := clYellow;

  end else begin
    // nouzovy zaver vyhybky ma prioritu i nad obsazenim useku
    fg := clAqua;
  end;

  if (Self.position = TTurnoutPosition.disabled) then
    fg := clFuchsia;

  Result := Result + ownConvert.ColorToStr(fg) + ';';

  // Pozadi
  bg := clBlack;
  if (Self.note <> '') then
    bg := clTeal;
  if (Self.lockout <> '') then
    bg := clOlive;
  if (diag.showZaver) then
  begin
    if (Self.zaver > TZaver.no) then
      bg := clGreen
    else if (Self.outputLocked) then
      bg := clBlue
    else if (Self.ShouldBeLocked()) then
      bg := clOlive;
  end;

  Result := Result + ownConvert.ColorToStr(bg) + ';' + IntToStr(ownConvert.BoolToInt(Self.NUZ)) + ';' +
    IntToStr(Integer(Self.position)) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlkTurnout.CombineCouplingInputs(first: TRCSInputState; second: TRCSInputState): TRCSInputState;
begin
  if ((first = TRCSInputState.failure) or (second = TRCSInputState.failure)) then
    Exit(TRCSInputState.failure);
  if ((first = TRCSInputState.unavailableModule) or (second = TRCSInputState.unavailableModule)) then
    Exit(TRCSInputState.unavailableModule);
  if ((first = TRCSInputState.unavailablePort) or (second = TRCSInputState.unavailablePort)) then
    Exit(TRCSInputState.unavailablePort);
  if ((first = TRCSInputState.isOn) and (second = TRCSInputState.isOn)) then
    Exit(TRCSInputState.isOn);
  Result := TRCSInputState.isOff;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.MockInputs(): TBlkTurnoutInputs;
begin
  if (Self.position = TTurnoutPosition.plus) then
    Exit(TBlkTurnoutInputs.Create(isOn, isOff))
  else if (Self.position = TTurnoutPosition.minus) then
    Exit(TBlkTurnoutInputs.Create(isOff, isOn))
  else if ((Self.movingPlus) and (now > Self.m_state.movingStart + EncodeTime(0, 0, _T_MOVING_MOCK_SEC, 0))) then
    Exit(TBlkTurnoutInputs.Create(isOn, isOff))
  else if ((Self.movingMinus) and (now > Self.m_state.movingStart + EncodeTime(0, 0, _T_MOVING_MOCK_SEC, 0))) then
    Exit(TBlkTurnoutInputs.Create(isOff, isOn))
  else if (Self.position = TTurnoutPosition.disabled) then
  begin
    // proper booting of coupling
    if (Self.m_state.positionSave = TTurnoutPosition.plus) then
      Exit(TBlkTurnoutInputs.Create(isOn, isOff))
    else if (Self.m_state.positionSave = TTurnoutPosition.minus) then
      Exit(TBlkTurnoutInputs.Create(isOff, isOn))
    else
      Exit(TBlkTurnoutInputs.Create(isOff, isOff));
  end
  else
    Exit(TBlkTurnoutInputs.Create(isOff, isOff));
end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlkTurnoutInputs.Create(plus, minus: TRCSInputState);
begin
  Self.plus := plus;
  Self.minus := minus;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetCoupling(): TBlkTurnout;
begin
  Blocks.GetBlkByID(Self.m_settings.coupling, TBlk(Result));
  if ((Result <> nil) and (Result.typ <> btTurnout)) then
    Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.ShouldBeLocked(withZamek: Boolean): Boolean;
begin
  Result := (Self.zaver > TZaver.no) or (Self.emLock) or (Self.intentionalLocked) or
    ((withZamek) and (Self.LockLocked()));

  if (Self.coupling <> nil) then
    Result := Result or (Self.coupling.zaver > TZaver.no) or (Self.coupling.emLock) or (Self.coupling.intentionalLocked)
      or ((withZamek) and (Self.coupling.LockLocked()));
end;

function TBlkTurnout.ShouldBeLockedIgnoreStaveni(): Boolean;
begin
  Result := ((Self.zaver > TZaver.no) and (Self.zaver <> TZaver.staveni)) or (Self.emLock) or (Self.LockLocked());

  if (Self.coupling <> nil) then
    Result := Result or ((Self.coupling.zaver > TZaver.no) and (Self.coupling.zaver <> TZaver.staveni)) or
      (Self.coupling.emLock) or (Self.coupling.LockLocked());
end;

function TBlkTurnout.LockLocked(): Boolean;
begin
  Result := (Self.lock <> nil) and (not(Self.lock as TBlkLock).keyReleased);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TBlkTurnout.SetErrorToMsg(error: TTurnoutSetError): string;
begin
  case (error) of
    vseInvalidPos:
      Result := 'neplatná poloha';
    vseInvalidRCSConfig:
      Result := 'neplatná konfigurace bloku';
    vseLocked:
      Result := 'zamčena';
    vseOccupied:
      Result := 'obsazena';
    vseRCS:
      Result := 'vyjímka RCS SetOutput';
    vseTimeout:
      Result := 'timeout';
  else
    Result := 'neznámá chyba';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetRCSInPlus(): TRCSAddr;
begin
  Result := Self.m_settings.RCSAddrs[_TI_INPLUS];
end;

function TBlkTurnout.GetRCSInMinus(): TRCSAddr;
begin
  Result := Self.m_settings.RCSAddrs[_TI_INMINUS];
end;

function TBlkTurnout.GetRCSOutPlus(): TRCSAddr;
begin
  Result := Self.m_settings.RCSAddrs[_TI_OUTPLUS];
end;

function TBlkTurnout.GetRCSOutMinus(): TRCSAddr;
begin
  Result := Self.m_settings.RCSAddrs[_TI_OUTMINUS];
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.ShowIndication();
begin
  if ((not Self.m_settings.indication.enabled) or (not RCSi.Started)) then
    Exit();

  try
    if ((Self.m_settings.indication.pstOnly) and (not Self.PstIsActive())) then
    begin
      RCSi.SetOutput(Self.m_settings.indication.rcsPlus, 0);
      RCSi.SetOutput(Self.m_settings.indication.rcsMinus, 0);
      Exit();
    end;

    if (Self.movingPlus) then
      RCSi.SetOutput(Self.m_settings.indication.rcsPlus, osf180)
    else
      RCSi.SetOutput(Self.m_settings.indication.rcsPlus, ite(Self.position = TTurnoutPosition.plus, 1, 0));

    if (Self.movingMinus) then
      RCSi.SetOutput(Self.m_settings.indication.rcsMinus, osf180)
    else
      RCSi.SetOutput(Self.m_settings.indication.rcsMinus, ite(Self.position = TTurnoutPosition.minus, 1, 0));
  except

  end;
end;

procedure TBlkTurnout.ReadContollers();
begin
  if ((not Self.m_settings.controllers.enabled) or (not RCSi.Started)) then
    Exit();
  if ((Self.m_settings.controllers.pstOnly) and (not Self.PstIsActive())) then
    Exit();

  try
    if ((RCSi.GetInput(Self.m_settings.controllers.rcsPlus) = TRCSInputState.isOn) and
        (RCSi.GetInput(Self.m_settings.controllers.rcsMinus) = TRCSInputState.isOn)) then
      Exit();

    if (RCSi.GetInput(Self.m_settings.controllers.rcsPlus) = TRCSInputState.isOn) then
      if ((not Self.outputLocked) and (Self.position <> TTurnoutPosition.plus)) then
        Self.SetPosition(TTurnoutPosition.plus, false, true);

    if (RCSi.GetInput(Self.m_settings.controllers.rcsMinus) = TRCSInputState.isOn) then
      if ((not Self.outputLocked) and (Self.position <> TTurnoutPosition.minus)) then
        Self.SetPosition(TTurnoutPosition.minus, false, true);

  except
    on E: RCSException do Exit();
    on E: Exception do raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.PstAdd(pst: TBlk);
begin
  if (Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Add(pst);
  Self.Change();
end;

procedure TBlkTurnout.PstRemove(pst: TBlk);
begin
  if (not Self.m_state.psts.Contains(pst)) then
    Exit();

  Self.m_state.psts.Remove(pst);
  Self.Change();
end;

function TBlkTurnout.PstIsActive(): Boolean;
begin
  Self.PstCheckActive();
  for var blk: TBlk in Self.m_state.psts do
    if (TBlkPst(blk).status = pstActive) then
      Exit(true);
  Result := false;
end;

function TBlkTurnout.PstIs(): Boolean;
begin
  Self.PstCheckActive();
  Result := (Self.m_state.psts.Count > 0);
end;

procedure TBlkTurnout.PstCheckActive();
begin
  for var i := Self.m_state.psts.Count-1 downto 0 do
    if (TBlkPst(Self.m_state.psts[i]).status <= pstOff) then
      Self.PstRemove(self.m_state.psts[i]);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit

