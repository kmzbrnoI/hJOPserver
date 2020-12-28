unit TBlokVyhybka;

{ TURNOUT technological block definition. }

interface

uses IniFiles, TBlok, SysUtils, TBlokUsek, Menus, TOblsRizeni,
     Classes, IdContext, Generics.Collections, JsonDataObjects, RCS,
     TOblRizeni, TechnologieRCS;

{
 Jak funguje intentionalLock:
 Tohle se uplatnuje jen u vyhybek odvratu. Tyto vyhybky nemusi byt v usecich JC,
 takze nemaji zadnou informaci o tom, kdy maji byt drzeny a kdy se ma drzeni
 zrusit. Jedna vyhybka muze byt v odvratech vice postavenych JC, proto se pocita,
 kolikrat je vyhybka drzena (VyhStav.intentionalLocks), jakmile je hodnota 0,
 dojde k odblokovaniv vyhybky. Pri odblokovani pozor na to, ze muse byt nenulove
 intentionalLocks vyhybky ve spojce, v takovem pripade vyhybku neodblokovavat.
}

type
 TTurnoutPosition = (disabled = -5, none = -1, plus = 0, minus = 1, both = 2);
 TTurnoutSetError = (vseInvalidPos, vseInvalidRCSConfig, vseLocked, vseOccupied, vseRCS, vseTimeout);
 ECoupling = class(Exception);
 TTurnoutSetPosErrCb = procedure (Sender: TObject; error: TTurnoutSetError) of object;

 TBlkTurnoutSettings = record
  RCSAddrs: TRCSAddrs;     // order: in+, in-, out+, out-
  coupling: Integer;       // coupling turnout id (-1 in case of none); Both coupling turnouts reference each other.
  lock: Integer;           // lock id in case of turnout refering to lock, -1 in case of none
  lockPosition: TTurnoutPosition;
  npPlus: Integer;         // id of non-profile block for position +
  npMinus: Integer;        // id of non-profile block for position -
  posDetection: Boolean;
 end;

 TBlkTurnoutState = record
  position, positionOld, positionReal, positionSave, positionLock: TTurnoutPosition;
                                             // position = current reporting
                                             // old = last position
                                             // real = state based only on current state of RCS inputs
                                             // save = position to save to file in case of posDetection = false
                                             // lock = in which position to lock turnout in case of lock
  note, lockout: string;
  movingPlus, movingMinus: Boolean;
  intentionalLocks: Integer;
  locks: Cardinal;                           // n.o. blocks who gave emergency lock

  movingOKCallback: TNotifyEvent;
  movingErrCallback: TTurnoutSetPosErrCb;
  movingStart: TDateTime;
  movingPanel: TIDContext;
  movingOR: TObject;
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
    position : disabled;
    positionOld : disabled;
    positionReal : disabled;
    positionSave: none;
    positionLock: none;
    note : '';
    lockout : '';
    movingPlus : false;
    movingMinus : false;
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
    function GetOccupied(): TUsekStav;
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

    procedure MenuPlusClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuMinusClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNSPlusClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNSMinusClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure UPOPlusClick(Sender: TObject);
    procedure UPOMinusClick(Sender: TObject);
    procedure UPONSPlusClick(Sender: TObject);
    procedure UPONSMinusClick(Sender: TObject);

    procedure MenuAdminREDUKClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminPolPlusCLick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminPolMinusCLick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminNepolCLick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PanelPotvrSekvNSPlus(Sender: TIdContext; success: Boolean);
    procedure PanelPotvrSekvNSMinus(Sender: TIdContext; success: Boolean);
    procedure PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);

    procedure ORVylukaNull(Sender: TIdContext; success: Boolean);

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

    procedure StitVylUPO(SenderPnl: TIdContext; SenderOR: TObject;
        UPO_OKCallback: TNotifyEvent; UPO_EscCallback: TNotifyEvent);

    class function CombineCouplingInputs(first: TRCSInputState; second: TRCSInputState): TRCSInputState;

    function GetRCSInPlus(): TRCSAddr;
    function GetRCSInMinus(): TRCSAddr;
    function GetRCSOutPlus(): TRCSAddr;
    function GetRCSOutMinus(): TRCSAddr;

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

    //----- turnout-specific functions -----

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

    property state: TBlkTurnoutState read m_state;

    property position: TTurnoutPosition read m_state.position;
    property NUZ: Boolean read GetNUZ;
    property zaver: TZaver read GetZaver;
    property occupied: TUsekStav read GetOccupied;
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
    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject;
        Button: TPanelButton; rights: TORCOntrolRights; params: string = ''); override;
    function PanelStateString(): string; override;

    // PT:
    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;
    procedure PutPtState(reqJson: TJsonObject; respJson: TJsonObject); override;

    class function PositionToStr(position: TTurnoutPosition): string;
    class function StrToPosition(c: string): TTurnoutPosition;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, GetSystems, fMain, TJCDatabase, UPO, Graphics, Diagnostics, Math,
      TCPServerOR, TBlokZamek, PTUtils, changeEvent, TCPORsRef, ownConvert,
      IfThenElse;

constructor TBlkTurnout.Create(index: Integer);
begin
 inherited Create(index);
 Self.GlobalSettings.typ := btTurnout;
 Self.m_state := Self._def_vyh_stav;
 Self.m_lock := nil;
 Self.m_parent := nil;
 Self.m_npPlus := nil;
 Self.m_npMinus := nil;
end;//ctor

destructor TBlkTurnout.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var strs: TStrings;
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
 if ((Self.m_state.positionSave <> TTurnoutPosition.plus) and (Self.m_state.positionSave <> TTurnoutPosition.minus)) then
   Self.m_state.positionSave := TTurnoutPosition.plus;

 strs := Self.LoadORs(ini_rel, 'V');
 try
  Self.m_spnl.track := ite(strs.Count >= 2, StrToInt(strs[1]), -1);
 finally
   strs.Free();
 end;

 PushRCStoOR(Self.ORsRef, Self.m_settings.RCSAddrs);
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
end;

procedure TBlkTurnout.SaveStatus(ini_stat: TMemIniFile; const section: string);
var position: TTurnoutPosition;
begin
 if (Self.m_state.note <> '') then
   ini_stat.WriteString(section, 'stit', Self.m_state.note);

 if (Self.m_state.lockout <> '') then
   ini_stat.WriteString(section, 'vyl', Self.m_state.lockout);

 if (not Self.posDetection) then
  begin
   if (Self.position > TTurnoutPosition.disabled) then
     position := Self.position
   else
     position := Self.m_state.positionSave;

   ini_stat.WriteString(section, 'poloha',
    ite((Self.movingMinus) or (position = TTurnoutPosition.minus), '-', '+'));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Enable();
var rcsaddr: TRCSAddr;
    i: Integer;
    enable: Boolean;
begin
 enable := (Self.m_settings.RCSAddrs.Count >= 4);

 if (Self.posDetection) then
  begin
   for rcsaddr in Self.m_settings.RCSAddrs do
     if (not RCSi.IsNonFailedModule(rcsaddr.board)) then
       enable := false;
  end else begin
   for i := 2 to Self.m_settings.RCSAddrs.Count-1 do
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

 Self.MapNpEvents();
 Self.Update(); //update will call Change()
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
 Self.Change(true);
end;

procedure TBlkTurnout.Reset();
begin
 Self.m_state.intentionalLocks := 0;
 Self.m_state.movingPlus := false;
 Self.m_state.movingMinus := false;
 Self.m_state.positionLock := TTurnoutPosition.none;
 Self.m_state.locks := 0;
end;

function TBlkTurnout.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 if ((portType = TRCSIOType.input) and (Self.m_settings.RCSAddrs.Count >= 2) and
     ((Self.rcsInPlus = addr) or (Self.rcsInMinus = addr))) then
   Exit(True);

 if ((portType = TRCSIOType.output) and (Self.m_settings.RCSAddrs.Count >= 4) and
     ((Self.rcsOutPlus = addr) or (Self.rcsOutMinus = addr))) then
   Exit(True);

 Result := False;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Update();
begin
 Self.CheckNullOutput();
 Self.UpdatePosition();
 Self.UpdateMovingTimeout();
 Self.UpdateLock();

 if (Self.m_state.position <> Self.m_state.positionOld) then
  begin
   if ((Self.m_state.positionOld = TTurnoutPosition.disabled) and (Self.outputLocked)) then // apply lock
     Self.SetPosition(Self.m_state.positionLock, true);
   Self.m_state.positionOld := Self.m_state.position;
   Self.Change();
  end;

 inherited Update();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetZaver(): TZaver;
begin
 if (Self.m_spnl.track = -1) then
   Exit(TZaver.no);

 if (((Self.m_parent = nil) and (Self.m_spnl.track <> -1)) or ((Self.m_parent.id <> Self.m_spnl.track))) then
   Blky.GetBlkByID(Self.m_spnl.track, Self.m_parent);
 if (Self.m_parent <> nil) then
   Result := (Self.m_parent as TBlkUsek).Zaver
 else
   Result := TZaver.no;
end;

function TBlkTurnout.GetNUZ(): Boolean;
var tmpBlk: TBlk;
    return: Integer;
begin
 return := Blky.GetBlkByID(Self.m_spnl.track, tmpBlk);
 if (return < 0) then Exit(false);
 if (tmpBlk.typ <> btUsek) then Exit(false);

 Result := (TBlkUsek(tmpBlk)).NUZ;
end;

function TBlkTurnout.GetOccupied(): TUsekStav;
var tmpBlk: TBlk;
    return: Integer;
begin
 return := Blky.GetBlkByID(Self.m_spnl.track, tmpBlk);
 if (return < 0) then Exit(TUsekStav.none);
 if ((tmpBlk.typ <> btUsek) and (tmpBlk.typ <> btTU)) then Exit(TUsekStav.none);

 Result := (tmpBlk as TBlkUsek).Obsazeno;
end;

function TBlkTurnout.GetOutputLocked(): Boolean;
begin
 Result := (Self.m_state.positionLock > TTurnoutPosition.none);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetSettings(): TBlkTurnoutSettings;
begin
 Result := Self.m_settings;
end;

procedure TBlkTurnout.SetSettings(data: TBlkTurnoutSettings);
var Blk: TBlk;
    coupling_settings: TBlkTurnoutSettings;
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
     Blky.GetBlkByID(coupling_old, Blk);
     if ((Blk <> nil) and (Blk.typ = btTurnout)) then
       (Blk as TBlkTurnout).SetCouplingNoPropag(-1);
    end;

   // pridame spojku do druhe vyhybky
   Blky.GetBlkByID(data.coupling, Blk);
   if ((Blk = nil) or (Blk.typ <> btTurnout)) then
    begin
     Self.m_settings.coupling := -1;
    end else begin
     coupling_settings := (Blk as TBlkTurnout).GetSettings();
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
     Blky.GetBlkByID(coupling_old, Blk);
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

////////////////////////////////////////////////////////////////////////////////

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

procedure TBlkTurnout.ORVylukaNull(Sender: TIdContext; success: Boolean);
begin
 if (success) then
   Self.lockout := '';
end;

procedure TBlkTurnout.SetLockout(Sender: TIDCOntext; lockout: string);
begin
 if ((self.m_state.lockout <> '') and (lockout = '')) then
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil)
 else
   Self.lockout := lockout;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetIntentionalLock(): Boolean;
begin
 Result := (Self.m_state.intentionalLocks > 0);
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.UpdatePosition();
var inp, couplingInp: TBlkTurnoutInputs;
    oblr: TOR;
    coupling: TBlkTurnout;
 begin
  if (Self.m_settings.RCSAddrs.Count < 4) then Exit();

  Blky.GetBlkByID(Self.m_settings.coupling, TBlk(coupling));
  if ((coupling <> nil) and (coupling.typ <> btTurnout)) then
    Exit();

  //RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-
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
    if ((inp.plus = failure) or (inp.minus = failure) or
        (not RCSi.IsModule(Self.rcsOutPlus.board)) or (not RCSi.IsModule(Self.rcsOutMinus.board)) or
        ((coupling <> nil) and ((not RCSi.IsModule(coupling.rcsOutPlus.board)) or
                              (not RCSi.IsModule(coupling.rcsOutMinus.board))))) then
     begin
      if (Self.state.position <> TTurnoutPosition.disabled) then
       begin
        Self.m_state.position := TTurnoutPosition.disabled;
        JCDb.RusJC(Self);
       end;
      Exit();
     end;
  except
    if (Self.state.position <> TTurnoutPosition.disabled) then
     begin
      Self.m_state.position := TTurnoutPosition.disabled;
      JCDb.RusJC(Self);
     end;
    Exit();
  end;


  if ((inp.plus = isOff) and (inp.minus = isOff)) then
   begin
    Self.m_state.position := none;

    if ((Self.m_state.position <> Self.m_state.positionReal) and ((Self.Zaver > TZaver.no) or (Self.emLock) or
      ((Self.intentionalLocked) and (not Self.m_state.movingPlus) and (not Self.m_state.movingMinus)) or
      (Self.LockLocked()))
     and (Self.Zaver <> TZaver.staveni)) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Není koncová poloha '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.m_state.positionReal := none;
   end;

  if ((inp.plus = isOn) and (inp.minus = isOff)) then
   begin
    //je-li plus vstup 1
    Self.m_state.positionReal := plus;
    if (Self.m_state.movingMinus) then Exit();

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
          for oblr in Self.OblsRizeni do
            oblr.BlkWriteError(Self, 'Ztráta dohledu na výhybce '+Self.GlobalSettings.name, 'TECHNOLOGIE');
          JCDb.RusJC(Self);
         end;
       end;
     end;
   end;

  if ((inp.minus = isOn) and (inp.plus = isOff)) then
   begin
    //je-li minus vstup 1
    Self.m_state.positionReal := minus;
    if (Self.m_state.movingPlus) then Exit();

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
        //sem se dostaneme, pokud se vyhybka nalezne neocekavane v poloze -
        Self.m_state.position := minus;

        if ((Self.ShouldBeLocked(false)) or (Self.LockLocked() and (Self.m_settings.lockPosition <> minus))) then
         begin
          for oblr in Self.OblsRizeni do
            oblr.BlkWriteError(Self, 'Ztráta dohledu na výhybce '+Self.GlobalSettings.name, 'TECHNOLOGIE');
          JCDb.RusJC(Self);
         end;
       end;
     end;
   end;

  //2 polohy zaroven = deje se neco divneho
  if ((inp.plus = isOn) and (inp.minus = isOn)) then
   begin
    Self.m_state.position := both;

    if ((((Self.ShouldBeLocked()) and (Self.Zaver <> TZaver.staveni)) or (Self.LockLocked()))
        and (Self.m_state.positionOld <> both)) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Není koncová poloha '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.m_state.positionReal := both;
   end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.UpdateMovingTimeout();
begin
 if ((not Self.movingPlus) and (not Self.movingMinus)) then Exit();

 // timeout
 if (Now > Self.m_state.movingStart+EncodeTime(0, 0, _T_MOVING_TIMEOUT_SEC, 0)) then
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
   Self.m_state.movingOKCallback  := nil;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.SetPosition(new: TTurnoutPosition; lock: Boolean = false; nouz: Boolean = false;
      callback_ok: TNotifyEvent = nil; callback_err: TTurnoutSetPosErrCb = nil);
var coupling: TBlkTurnout;
begin
  if (Self.m_settings.RCSAddrs.Count < 4) then
   begin
    if (Assigned(callback_err)) then callback_err(self, vseInvalidRCSConfig);
    Exit();
   end;
  if ((new <> plus) and (new <> minus)) then
   begin
    if (Assigned(callback_err)) then callback_err(self, vseInvalidPos);
    Exit();
   end;

  // V tomto momente je klicove ziskat aktualni polohu vyhybky, jinak by mohlo dojit
  // k zacykleni pri staveni spojek.
  Self.UpdatePosition();
  coupling := Self.coupling;

  if (new <> Self.m_state.position) then
   begin
    // vstupni podminky se kontroluji jen pro pripad, kdy chceme vyhybku opravdu prestavit
    // zamknout ji muzeme kdykoliv

    // pokud se nerovna moje poloha, nerovna se i poloha spojky -> obsazenost na spojce apod. je problem
    if (Self.ShouldBeLockedIgnoreStaveni()) then
     begin
      if (Assigned(callback_err)) then callback_err(self, vseLocked);
      Exit();
     end;
    if (((Self.occupied = TUsekStav.obsazeno) or ((coupling <> nil) and (coupling.occupied = TUsekStav.obsazeno))) and (not nouz)) then
     begin
      if (Assigned(callback_err)) then callback_err(self, vseOccupied);
      Exit();
     end;
   end else begin
    // pokud polohu uz mame, zavolame ok callback
    if (Assigned(callback_ok)) then callback_ok(Self);
   end;

 //RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-

 if (lock) then
   Self.m_state.positionLock := new; // before SetOutput to ensure locking of failed RCS modules after restoration

 if (new = plus) then
  begin
   try
     RCSi.SetOutput(Self.rcsOutPlus, 1);
     RCSi.SetOutput(Self.rcsOutMinus, 0);
   except
     if (Assigned(callback_err)) then callback_err(self, vseRCS);
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
     if (Assigned(callback_err)) then callback_err(self, vseRCS);
     Exit();
   end;

   Self.m_state.movingPlus  := false;
   if (Self.m_state.position <> minus) then
     Self.m_state.movingMinus := true;

   if (Self.m_state.position = plus) then
     Self.m_state.position := none;
  end;

 Self.m_state.movingErrCallback := callback_err;
 Self.m_state.movingOKCallback := callback_ok;
 Self.m_state.movingStart := Now;

 if (not lock) then
  begin
   Self.m_nullOutput.enabled := true;
   Self.m_nullOutput.NullOutputTime := Now+EncodeTime(0, 0, 0, 500);
  end;

 if (coupling <> nil) then
  begin
   // pokud se jedna o spojku, volame SetPosition i na spojku
   if ((coupling.state.movingPlus <> Self.m_state.movingPlus) or
       (coupling.state.movingMinus <> Self.m_state.movingMinus) or
       ((lock) and (not coupling.outputLocked))) then
     coupling.SetPosition(new, lock, nouz);
  end;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Unlock();
var coupling: TBlkTurnout;
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

 coupling := Self.coupling;
 if ((coupling <> nil) and (coupling.outputLocked)) then
   coupling.Unlock();

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.CheckNullOutput();
begin
 if (not Self.m_nullOutput.enabled) then Exit;

 if (Now >= Self.m_nullOutput.NullOutputTime) then
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

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.MenuPlusClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((Self.note <> '') or (Self.lockout <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOPlusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOPlusClick(SenderPnl);
 end;
end;

procedure TBlkTurnout.MenuMinusClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((Self.note <> '') or (Self.lockout <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOMinusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOMinusClick(SenderPnl);
 end;
end;

procedure TBlkTurnout.MenuNSPlusClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((Self.note <> '') or (Self.lockout <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSPlusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPONSPlusClick(SenderPnl);
 end;
end;

procedure TBlkTurnout.MenuNSMinusClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 if ((Self.note <> '') or (Self.lockout <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSMinusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPONSMinusClick(SenderPnl);
 end;
end;

procedure TBlkTurnout.UPOPlusClick(Sender: TObject);
begin
 Self.m_state.movingPanel := TIdContext(Sender);
 Self.m_state.movingOR := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Self.SetPosition(TTurnoutPosition.plus, false, false, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.UPOMinusClick(Sender: TObject);
begin
 Self.m_state.movingPanel := TIdContext(Sender);
 Self.m_state.movingOR := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Self.SetPosition(TTurnoutPosition.minus, false, false, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.UPONSPlusClick(Sender: TObject);
var Blk: TBlk;
begin
 Self.m_state.movingPanel := TIdContext(Sender);
 Self.m_state.movingOR := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Blky.GetBlkByID(Self.trackID, Blk);
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelPotvrSekvNSPlus, (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
                    'Nouzové stavění do polohy plus', TBlky.GetBlksList(Self),
                    TOR.GetPSPodminky(TOR.GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkTurnout.UPONSMinusClick(Sender: TObject);
var Blk: TBlk;
begin
 Self.m_state.movingPanel := TIdContext(Sender);
 Self.m_state.movingOR := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Blky.GetBlkByID(Self.trackID, Blk);
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelPotvrSekvNSMinus, (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
                    'Nouzové stavění do polohy mínus', TBlky.GetBlksList(Self),
                    TOR.GetPSPodminky(TOR.GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkTurnout.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.state.note);
end;

procedure TBlkTurnout.MenuVylClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.state.lockout);
end;

procedure TBlkTurnout.MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.emLock := true;
 if (Self.coupling <> nil) then
   Self.coupling.emLock := true;
end;

procedure TBlkTurnout.MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TOR),
    'Zrušení nouzového závěru', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkTurnout.PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);
begin
 if (success) then
   Self.ResetEmLocks();
end;

procedure TBlkTurnout.MenuAdminREDUKClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 Self.m_state.intentionalLocks := 0;
 Self.Change();
end;

procedure TBlkTurnout.MenuAdminPolPlusCLick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   RCSi.SetInput(Self.rcsInPlus, 1);
   RCSi.SetInput(Self.rcsInMinus, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkTurnout.MenuAdminPolMinusCLick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   RCSi.SetInput(Self.rcsInPlus, 0);
   RCSi.SetInput(Self.rcsInMinus, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkTurnout.MenuAdminNepolCLick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 try
   RCSi.SetInput(Self.rcsInPlus, 0);
   RCSi.SetInput(Self.rcsInMinus, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TORCOntrolRights): string;
var coupling: TBlkTurnout;
begin
 Result := inherited;

 Blky.GetBlkByID(Self.m_settings.coupling, TBlk(coupling));

 if (not Self.ShouldBeLocked()) then
  begin
   // na vyhybce neni zaver a menu neni redukovane

   if ((Self.occupied = TUsekStav.obsazeno) or ((coupling <> nil) and (coupling.occupied = TUsekStav.obsazeno))) then
    begin
     if (Self.m_state.position = plus) then Result := Result + '!NS-,';
     if (Self.m_state.position = minus) then Result := Result + '!NS+,';
     if ((Self.m_state.position = both) or (Self.m_state.position = none)) then
      Result := Result + '!NS+,!NS-,-,';
    end else begin
     if (Self.m_state.position = plus) then Result := Result + 'S-,';
     if (Self.m_state.position = minus) then Result := Result + 'S+,';
     if ((Self.m_state.position = both) or (Self.m_state.position = none)) then
      Result := Result + 'S+,S-,-,';
    end;
  end;

 Result := Result + 'STIT,VYL,';

 if (Self.emLock) then
  Result := Result + '!ZAV<,'
 else
  if ((Self.position = TTurnoutPosition.plus) or (Self.position = TTurnoutPosition.minus)) then
    Result := Result + 'ZAV>,';

 if (rights >= TORControlRights.superuser) then
  begin
   Result := Result + '-,';
   if (Self.intentionalLocked) then Result := Result + '*ZRUŠ REDUKCI,';
  end;

 if ((RCSi.simulation) and (Self.posDetection)) then
  begin
   Result := Result + '-,';
   if (Self.position <> TTurnoutPosition.plus) then
     Result := Result + '*POL+,';
   if (Self.position <> TTurnoutPosition.minus) then
     Result := Result + '*POL-,';
   if (Self.position <> TTurnoutPosition.none) then
     Result := Result + '*NEPOL,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TORCOntrolRights; params: string = '');
begin
 case (Button) of
  F1, F2, ENTER: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkTurnout.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
 if (item = 'S+')        then Self.MenuPlusClick(SenderPnl, SenderOR)
 else if (item = 'S-')   then Self.MenuMinusClick(SenderPnl, SenderOR)
 else if (item = 'NS+')  then Self.MenuNSPlusClick(SenderPnl, SenderOR)
 else if (item = 'NS-')  then Self.MenuNSMinusClick(SenderPnl, SenderOR)
 else if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'VYL')  then Self.MenuVylClick(SenderPnl, SenderOR)
 else if (item = 'ZAV>') then Self.MenuZAVEnableClick(SenderPnl, SenderOR)
 else if (item = 'ZAV<') then Self.MenuZAVDisableClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ REDUKCI') then Self.MenuAdminREDUKClick(SenderPnl, SenderOR)
 else if (item = 'POL+') then Self.MenuAdminPolPlusCLick(SenderPnl, SenderOR)
 else if (item = 'POL-') then Self.MenuAdminPolMinusCLick(SenderPnl, SenderOR)
 else if (item = 'NEPOL') then Self.MenuAdminNepolCLick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.PanelPotvrSekvNSPlus(Sender: TIdContext; success: Boolean);
begin
 if (not success) then Exit();
 Self.SetPosition(plus, false, true, nil, Self.PanelMovingErr);
end;

procedure TBlkTurnout.PanelPotvrSekvNSMinus(Sender: TIdContext; success: Boolean);
begin
 if (not success) then Exit();
 Self.SetPosition(minus, false, true, nil, Self.PanelMovingErr);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.Change(now: Boolean = false);
var changed: Boolean;
begin
 changed := false;

 if (not Self.outputLocked) and (Self.ShouldBeLocked()) then
  begin
   // pokud je vyhybka redukovana, nebo je na ni zaver a je v koncove poloze a neni zamkla, zamkneme ji
   if (Self.m_state.position = TTurnoutPosition.plus) then begin
    Self.SetPosition(TTurnoutPosition.plus, true);
    changed := true;
   end;
   if (Self.m_state.position = TTurnoutPosition.minus) then begin
    Self.SetPosition(TTurnoutPosition.minus, true);
    changed := true;
   end;
  end;

 if ((not Self.ShouldBeLocked()) and (Self.outputLocked)) then
  begin
   Self.Unlock();
   changed := true;
  end;

 if (not changed) then inherited Change(now);
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.IsEmergencyLock(): Boolean;
begin
 Result := (Self.state.locks > 0);
end;

procedure TBlkTurnout.SetEmergencyLock(zaver: Boolean);
begin
 if (zaver) then
  begin
   Inc(Self.m_state.locks);
   if (Self.m_state.locks = 1) then Self.Change();
  end else begin
   if (Self.state.locks > 0) then Dec(Self.m_state.locks);
   if (Self.state.locks = 0) then
    begin
     Self.Change();
     Blky.NouzZaverZrusen(Self);
    end;
  end;
end;

procedure TBlkTurnout.ResetEmLocks();
begin
 Self.m_state.locks := 0;
 Blky.NouzZaverZrusen(Self);
 Self.Change();
 if (Self.coupling <> nil) then
   Self.coupling.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// tato metoda je volana, pokud dojde k timeoutu pri staveni vyhybky z paneli
procedure TBlkTurnout.PanelMovingErr(Sender: TObject; error: TTurnoutSetError);
begin
  if ((Assigned(Self.m_state.movingPanel)) and (Assigned(Self.m_state.movingOR))) then
   begin
    ORTCPServer.BottomError(Self.m_state.movingPanel, 'Nepřestavena '+Self.GlobalSettings.name + ': ' + Self.SetErrorToMsg(error),
      (Self.m_state.movingOR as TOR).ShortName, 'TECHNOLOGIE');
    Self.m_state.movingPanel := nil;
    Self.m_state.movingOR := nil;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetLock(): TBlk;
begin
 if (((Self.m_lock = nil) and (Self.m_settings.lock <> -1)) or
     ((Self.m_lock <> nil) and (Self.m_lock.id <> Self.m_settings.lock))) then
   Blky.GetBlkByID(Self.m_settings.lock, Self.m_lock);
 Result := Self.m_lock;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetNpPlus(): TBlk;
begin
 if (((Self.m_npPlus = nil) and (Self.m_settings.npPlus <> -1)) or
     ((Self.m_npPlus <> nil) and (Self.m_npPlus.id <> Self.m_settings.npPlus))) then
   Blky.GetBlkByID(Self.m_settings.npPlus, Self.m_npPlus);
 Result := Self.m_npPlus;
end;

function TBlkTurnout.GetNpMinus(): TBlk;
begin
 if (((Self.m_npMinus = nil) and (Self.m_settings.npMinus <> -1)) or
     ((Self.m_npMinus <> nil) and (Self.m_npMinus.id <> Self.m_settings.npMinus))) then
   Blky.GetBlkByID(Self.m_settings.npMinus, Self.m_npMinus);
 Result := Self.m_npMinus;
end;

function TBlkTurnout.IsPositionDetection(): Boolean;
begin
 Result := (Self.m_settings.posDetection) and (Self.m_settings.RCSAddrs.Count >= 2);
end;

////////////////////////////////////////////////////////////////////////////////

// pokud je na vyhybku zamek, vyhybka ma nespravnou polohu a klic je v zamku, vyhlasime poruchu zamku
procedure TBlkTurnout.UpdateLock();
begin
 if (Self.LockLocked() and (not (Self.lock as TBlkZamek).nouzZaver) and
     (Self.position <> Self.m_settings.lockPosition) and (not (Self.lock as TBlkZamek).porucha)) then
   (Self.lock as TBlkZamek).porucha := true;
end;

////////////////////////////////////////////////////////////////////////////////
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
 if (Self.note <> '') then json['note'] := Self.note;
 if (Self.lockout <> '') then json['lockout'] := Self.lockout;
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
   if (Self.occupied = TUsekStav.obsazeno) then
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

////////////////////////////////////////////////////////////////////////////////

class function TBlkTurnout.PositionToStr(position: TTurnoutPosition): string;
begin
 case (position) of
  TTurnoutPosition.plus     : Result := '+';
  TTurnoutPosition.minus    : Result := '-';
  TTurnoutPosition.disabled : Result := 'off';
  TTurnoutPosition.none     : Result := 'none';
  TTurnoutPosition.both     : Result := 'both';
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

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.DecreaseEmergencyLock(amount: Cardinal);
begin
 if (Self.m_state.locks = 0) then Exit();

 if (amount > Self.m_state.locks) then
   Self.m_state.locks := 0
 else
   Self.m_state.locks := Self.m_state.locks - amount;

 if (not Self.emLock) then
  begin
   Blky.NouzZaverZrusen(Self);
   Self.Change();
   if (Self.coupling <> nil) then
     Self.coupling.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.NpObsazChange(Sender: TObject; data: Integer);
begin
 if ((data = 0) and (Sender = Self.npBlokPlus)) then
  begin
   // zmena bloku pro polohu +
   if (TBlkUsek(Self.npBlokPlus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 0))
   else
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 0));

   if (Self.position = TTurnoutPosition.plus) then Self.Change();

  end else if ((data = 1) and (Sender = Self.npBlokMinus)) then begin
   // zmena bloku pro polohu -
   if (TBlkUsek(Self.npBlokMinus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 1))
   else
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 1));

   if (Self.position = TTurnoutPosition.minus) then Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.MapNpEvents();
begin
 // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu +
 if (Self.npBlokPlus <> nil) then
  begin
   if (TBlkUsek(Self.npBlokPlus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 0))
   else
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 0));
  end;

 // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu -
 if (Self.npBlokMinus <> nil) then
  begin
   if (TBlkUsek(Self.npBlokMinus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 1))
   else
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 1));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.StitVylUPO(SenderPnl: TIdContext; SenderOR: TObject;
      UPO_OKCallback: TNotifyEvent; UPO_EscCallback: TNotifyEvent);
var upo: TUPOItems;
    item: TUPOItem;
    lines: TStrings;
begin
 upo := TList<TUPOItem>.Create;
 try
  if (Self.note <> '') then
   begin
    item[0] := GetUPOLine('ŠTÍTEK '+Self.GlobalSettings.name, taCenter, clBlack, clTeal);
    lines := GetLines(Self.note, _UPO_LINE_LEN);

    try
      item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;

   upo.Add(item);
  end;

  if (Self.lockout <> '') then
   begin
    item[0] := GetUPOLine('VÝLUKA '+Self.GlobalSettings.name, taCenter, clBlack, clOlive);
    lines := GetLines(Self.lockout, _UPO_LINE_LEN);

    try
      item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;

   upo.Add(item);
  end;

  ORTCPServer.UPO(SenderPnl, upo, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
 finally
   upo.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTurnout.SetCouplingNoPropag(coupling: Integer);
begin
 Self.m_settings.coupling := coupling;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.PanelStateString(): string;
var fg, bg: TColor;
    Blk: TBlk;
begin
 Result := inherited;

 // Popredi
 if (not Self.emLock) then
  begin
   case (Self.occupied) of
    TUsekStav.disabled: fg := clFuchsia;
    TUsekStav.none    : fg := $A0A0A0;
    TUsekStav.uvolneno: fg := $A0A0A0;
    TUsekStav.obsazeno: fg := clRed;
   else
    fg := clFuchsia;
   end;

   if (Self.occupied = TUsekStav.uvolneno) then
    begin
     case (Self.Zaver) of
      vlak   : fg := clLime;
      posun  : fg := clWhite;
      nouz   : fg := clAqua;
      ab     : fg := $707070;
     end;//case

     // je soucasti vybarveneho neprofiloveho useku
     Blky.GetBlkByID(Self.trackID, Blk);
     if ((Blk <> nil) and ((Blk.typ = btUsek) or (Blk.typ = btTU))
         and (fg = $A0A0A0) and (TBlkUsek(Blk).IsNeprofilJC)) then
       fg := clYellow;
    end;

   // do profilu vyhybky zasahuje obsazeny usek
   if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokPlus <> nil) and (Self.position = TTurnoutPosition.plus) and
       (TBlkUsek(Self.npBlokPlus).Obsazeno <> TUsekStav.uvolneno)) then
     fg := clYellow;

   // do profilu vyhybky zasahuje obsazeny usek
   if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokMinus <> nil) and (Self.position = TTurnoutPosition.minus) and
       (TBlkUsek(Self.npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
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
 if (Self.note <> '') then bg := clTeal;
 if (Self.lockout <> '') then bg := clOlive;
 if (diag.showZaver) then
  begin
   if (Self.Zaver > TZaver.no) then
     bg := clGreen
   else if (Self.outputLocked) then
     bg := clBlue
   else if (Self.ShouldBeLocked()) then
     bg := clOlive;
  end;

 Result := Result + ownConvert.ColorToStr(bg) + ';' +
                    IntToStr(ownConvert.BoolToInt(Self.NUZ)) + ';' +
                    IntToStr(Integer(Self.position))+';';
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.MockInputs(): TBlkTurnoutInputs;
begin
 if (Self.position = TTurnoutPosition.plus) then
   Exit(TBlkTurnoutInputs.Create(isOn, isOff))
 else if (Self.position = TTurnoutPosition.minus) then
   Exit(TBlkTurnoutInputs.Create(isOff, isOn))
 else if ((Self.movingPlus) and (Now > Self.m_state.movingStart+EncodeTime(0, 0, _T_MOVING_MOCK_SEC, 0))) then
   Exit(TBlkTurnoutInputs.Create(isOn, isOff))
 else if ((Self.movingMinus) and (Now > Self.m_state.movingStart+EncodeTime(0, 0, _T_MOVING_MOCK_SEC, 0))) then
   Exit(TBlkTurnoutInputs.Create(isOff, isOn))
 else if (Self.position = TTurnoutPosition.disabled) then begin
   // proper booting of coupling
   if (Self.m_state.positionSave = TTurnoutPosition.plus) then
     Exit(TBlkTurnoutInputs.Create(isOn, isOff))
   else if (Self.m_state.positionSave = TTurnoutPosition.minus) then
     Exit(TBlkTurnoutInputs.Create(isOff, isOn))
   else
     Exit(TBlkTurnoutInputs.Create(isOff, isOff));
 end else
   Exit(TBlkTurnoutInputs.Create(isOff, isOff));
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTurnoutInputs.Create(plus, minus: TRCSInputState);
begin
 Self.plus := plus;
 Self.minus := minus;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.GetCoupling(): TBlkTurnout;
begin
 Blky.GetBlkByID(Self.m_settings.coupling, TBlk(Result));
 if ((Result <> nil) and (Result.typ <> btTurnout)) then
   Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTurnout.ShouldBeLocked(withZamek: Boolean): Boolean;
begin
 Result := (Self.Zaver > TZaver.no) or (Self.emLock) or (Self.intentionalLocked) or
           ((withZamek) and (Self.LockLocked()));

 if (Self.coupling <> nil) then
   Result := Result or (Self.coupling.Zaver > TZaver.no) or (Self.coupling.emLock) or
                       (Self.coupling.intentionalLocked) or ((withZamek) and (Self.coupling.LockLocked()));
end;

function TBlkTurnout.ShouldBeLockedIgnoreStaveni(): Boolean;
begin
 Result := ((Self.Zaver > TZaver.no) and (Self.Zaver <> TZaver.staveni)) or
           (Self.emLock) or (Self.LockLocked());

 if (Self.coupling <> nil) then
   Result := Result or ((Self.coupling.Zaver > TZaver.no) and (Self.coupling.Zaver <> TZaver.staveni)) or
                        (Self.coupling.emLock) or (Self.coupling.LockLocked());
end;

function TBlkTurnout.LockLocked(): Boolean;
begin
 Result := (Self.lock <> nil) and (not (Self.lock as TBlkZamek).klicUvolnen);
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkTurnout.SetErrorToMsg(error: TTurnoutSetError): string;
begin
 case (error) of
   vseInvalidPos: Result := 'neplatná poloha';
   vseInvalidRCSConfig: Result := 'nepklatní konfigurace bloku';
   vseLocked: Result := 'zamčena';
   vseOccupied: Result := 'obsazena';
   vseRCS: Result := 'vyjímka RCS SetOutput';
   vseTimeout: Result := 'timeout';
 else
  Result := 'neznámá chyba';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

end.//unit

