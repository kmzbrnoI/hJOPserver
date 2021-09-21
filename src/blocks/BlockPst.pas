unit BlockPst;

{ Pst technological block definition. }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, IdContext,
  Generics.Collections, Area, JsonDataObjects, TechnologieRCS, BlockTurnout,
  TechnologieJC, JCBarriers, UPO;

type
  TBlkPstStatus = (pstDisabled, pstOff, pstRefuging, pstTakeReady, pstActive);

  TPstRefugeeZav = record
    block: Integer;
    position: TTurnoutPosition;
  end;

  TBlkPstSettings = record
    tracks: TList<Integer>;
    turnouts: TList<Integer>;
    signals: TList<Integer>;
    refugees: TList<TPstRefugeeZav>;
    rcsInTake: TRCSAddr;
    rcsInRelease: TRCSAddr;
    rcsOutTaken: TRCSAddr;
    rcsOutHorn: TRCSAddr;
  end;

  TBlkPstState = record
    status: TBlkPstStatus;
    emLock: Cardinal; // n.o. blocks who gave emergency lock
    note: string;
    senderOR: TObject;
    senderPnl: TIdContext;
  end;

  // Pst has zaver if any track in pst has zaver

  TBlkPst = class(TBlk)
  const
    _def_pst_state: TBlkPstState = ( // default state
      status: pstDisabled;
      emLock: 0;
      note: '';
    );

  private
    m_settings: TBlkPstSettings;
    m_state: TBlkPstState;

    procedure MenuPstEnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuPstDisClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuNPstClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHoukEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuHoukDisableClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure MenuAdminPoOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuAdminPoOffClick(SenderPnl: TIdContext; SenderOR: TObject);

    function GetZaver(): Boolean;
    function GetEmLock(): Boolean;
    function GetEnabled(): Boolean;

    procedure SetEmLock(new: Boolean);
    procedure SetNote(note: string);

    procedure CheckInputs();
    procedure ShowIndication();

    procedure NoteUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
        UPO_EscCallback: TNotifyEvent);

    procedure UPOPstDisDone(Sender: TObject);
    procedure UPONPStDone(Sender: TObject);
    procedure CSNPStDone(Sender: TIDContext; success: Boolean);

    procedure ActivationBarriers(var barriers: TList<TJCBarrier>);
    procedure Activate(senderPnl: TIdContext; senderOR: TObject);
    class function WarningBarrier(typ: TJCBarType): Boolean;
    class function BarrierToMessage(barrier: TJCBarrier): TUPOItem;
    procedure BarriersUPOOKCallback(Sender: TObject);
    procedure BarriersCSCallback(Sender: TIdContext; success: Boolean);

    procedure MoveRefugees();
    procedure RefugeeMoved(Sender: TObject);
    procedure RefugeeNotMoved(Sender: TObject; error: TTurnoutSetError);
    function AllRefugeesInPosition(): Boolean;

  public

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveStatus(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    procedure SetStatus(new: TBlkPstStatus);

    procedure Update(); override;
    procedure Change(now: Boolean = false); override;

    // ----- pst own functions -----

    function GetSettings(): TBlkPstSettings;
    procedure SetSettings(settings: TBlkPstSettings);

    procedure DecreaseEmLock(amount: Cardinal);

    property state: TBlkPstState read m_state;
    property status: TBlkPstStatus read m_state.status write SetStatus;

    property zaver: Boolean read GetZaver;
    property emLock: Boolean read GetEmLock write SetEmLock;
    property note: string read m_state.note write SetNote;
    property enabled: Boolean read GetEnabled;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, BlockDb, Graphics, Diagnostics, ownConvert, ownStrUtils,
  TJCDatabase, fMain, TCPServerPanel, TrainDb, THVDatabase, BlockTrack,
  RCSErrors, RCS, TCPAreasRef, BlockSignal, Logging;

constructor TBlkPst.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btPst;
  Self.m_state := _def_pst_state;

  Self.m_settings.tracks := TList<Integer>.Create();
  Self.m_settings.turnouts := TList<Integer>.Create();
  Self.m_settings.signals := TList<Integer>.Create();
  Self.m_settings.refugees := TList<TPstRefugeeZav>.Create();
end;

destructor TBlkPst.Destroy();
begin
  Self.m_settings.tracks.Free();
  Self.m_settings.turnouts.Free();
  Self.m_settings.signals.Free();
  Self.m_settings.refugees.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  begin
    var strs: TStrings := TStringList.Create();
    try
      ExtractStringsEx([','], [], ini_tech.ReadString(section, 'tracks', ''), strs);
      Self.m_settings.tracks.Clear();
      for var str: string in strs do
        Self.m_settings.tracks.Add(StrToInt(str));
    finally
      strs.Free();
    end;
  end;

  begin
    var strs: TStrings := TStringList.Create();
    try
      ExtractStringsEx([','], [], ini_tech.ReadString(section, 'turnouts', ''), strs);
      Self.m_settings.turnouts.Clear();
      for var str in strs do
        Self.m_settings.turnouts.Add(StrToInt(str));
    finally
      strs.Free();
    end;
  end;

  begin
    var strs: TStrings := TStringList.Create();
    try
      ExtractStringsEx([','], [], ini_tech.ReadString(section, 'signals', ''), strs);
      Self.m_settings.signals.Clear();
      for var str in strs do
        Self.m_settings.signals.Add(StrToInt(str));
    finally
      strs.Free();
    end;
  end;

  begin
    var strs: TStrings := TStringList.Create();
    var refugee: TStrings := TStringList.Create();
    try
      ExtractStringsEx([')'], ['('], ini_tech.ReadString(section, 'refugees', ''), strs);
      Self.m_settings.refugees.Clear();
      for var str in strs do
      begin
        refugee.Clear();
        ExtractStringsEx([','], [], str, refugee);
        var zav: TPstRefugeeZav;
        zav.block := StrToInt(refugee[0]);
        if (refugee[1] = '-') then
          zav.position := TTurnoutPosition.minus
        else
          zav.position := TTurnoutPosition.plus;
        Self.m_settings.refugees.Add(zav);
      end;
    finally
      strs.Free();
      refugee.Free();
    end;
  end;

  Self.m_settings.rcsInTake.Load(ini_tech.ReadString(section, 'rcsInTake', '0:0'));
  Self.m_settings.rcsInRelease.Load(ini_tech.ReadString(section, 'rcsInRelease', '0:0'));
  Self.m_settings.rcsOutTaken.Load(ini_tech.ReadString(section, 'rcsOutTaken', '0:0'));
  Self.m_settings.rcsOutHorn.Load(ini_tech.ReadString(section, 'rcsOutHorn', '0:0'));

  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');
  Self.LoadAreas(ini_rel, 'Pst').Free();
end;

procedure TBlkPst.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  ini_tech.WriteString(section, 'tracks', SerializeIntList(Self.m_settings.tracks));
  ini_tech.WriteString(section, 'turnouts', SerializeIntList(Self.m_settings.turnouts));
  ini_tech.WriteString(section, 'signals', SerializeIntList(Self.m_settings.signals));

  begin
    var str := '';
    for var zav: TPstRefugeeZav in Self.m_settings.refugees do
      str := str + '(' + IntToStr(zav.block) + ',' + TBlkTurnout.PositionToStr(zav.position) + ')';
    ini_tech.WriteString(section, 'refugees', str);
  end;

  ini_tech.WriteString(section, 'rcsInTake', Self.m_settings.rcsInTake.ToString());
  ini_tech.WriteString(section, 'rcsInRelease', Self.m_settings.rcsInRelease.ToString());
  ini_tech.WriteString(section, 'rcsOutTaken', Self.m_settings.rcsOutTaken.ToString());
  ini_tech.WriteString(section, 'rcsOutHorn', Self.m_settings.rcsOutHorn.ToString());
end;

procedure TBlkPst.SaveStatus(ini_stat: TMemIniFile; const section: string);
begin
  ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.Enable();
begin
  Self.m_state.status := pstOff;
  Self.Change();
end;

procedure TBlkPst.Disable();
begin
  Self.m_state.status := pstDisabled;
  Self.m_state.emLock := 0;
  Self.Change(true);
end;

procedure TBlkPst.Reset();
begin
  Self.m_state.senderOR := nil;
  Self.m_state.senderPnl := nil;
end;

procedure TBlkPst.SetStatus(new: TBlkPstStatus);
begin
  if (new = Self.m_state.status) then
    Exit();

  var old := Self.m_state.status;
  Self.m_state.status := new;
  Self.Change();

  if ((old = pstOff) and (new > pstOff)) then
  begin
    // add pst to all blocks
    for var trackId in Self.m_settings.tracks do
    begin
      var blk := Blocks.GetBlkByID(trackId);
      if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
        TBlkTrack(blk).PstAdd(Self);
    end;
    for var turnoutId in Self.m_settings.turnouts do
    begin
      var blk := Blocks.GetBlkByID(turnoutId);
      if ((blk <> nil) and (blk.typ = btTurnout)) then
        TBlkTurnout(blk).PstAdd(Self);
    end;

  end else if ((old > pstOff) and (new = pstOff)) then
  begin
    // remove pst from all blocks
    for var trackId in Self.m_settings.tracks do
    begin
      var blk := Blocks.GetBlkByID(trackId);
      if ((blk <> nil) and ((blk.typ = btTrack) or (blk.typ = btRT))) then
        TBlkTrack(blk).PstRemove(Self);
    end;
    for var turnoutId in Self.m_settings.turnouts do
    begin
      var blk := Blocks.GetBlkByID(turnoutId);
      if ((blk <> nil) and (blk.typ = btTurnout)) then
        TBlkTurnout(blk).PstRemove(Self);
    end;
    // unlock all refugees
    for var refugeeZav in Self.m_settings.refugees do
    begin
      var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));
      refugee.IntentionalUnlock();
    end;

  end else begin
    // call change to all blocks
    for var trackId in Self.m_settings.tracks do
    begin
      var blk := Blocks.GetBlkByID(trackId);
      if (blk <> nil) then
        blk.Change();
    end;
    for var turnoutId in Self.m_settings.turnouts do
    begin
      var blk := Blocks.GetBlkByID(turnoutId);
      if (blk <> nil) then
        blk.Change();
    end;

  end;

  Self.ShowIndication();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.Update();
begin
  inherited Update();
  Self.CheckInputs();
end;

procedure TBlkPst.CheckInputs();
var take, release: Boolean;
begin
  if (not RCSi.Started) then
    Exit();

  take := false;
  release := false;
  try
    take := (RCSi.GetInput(Self.m_settings.rcsInTake) = TRCSInputState.isOn);
    release := (RCSi.GetInput(Self.m_settings.rcsInRelease) = TRCSInputState.isOn);
  except
    on E: RCSException do begin end;
  end;

  if ((Self.status = pstTakeReady) and (take) and (not release)) then
  begin
    // take pst
    Self.status := pstActive;
  end;

  if ((Self.status = pstActive) and (release) and (not take)) then
  begin
    // release pst
    Self.status := pstTakeReady;
  end;
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkPst.Change(now: Boolean = false);
begin
  inherited Change(now);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.MenuPstEnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.Activate(SenderPnl, SenderOR);
end;

procedure TBlkPst.MenuPstDisClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOPstDisDone, nil)
  else
    Self.UPOPstDisDone(SenderPnl);
end;

procedure TBlkPst.MenuNPstClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPONPstDone, nil)
  else begin
    TPanelConnData(SenderPnl.data).UPO_ref := SenderOR;
    Self.UPONPstDone(SenderPnl);
  end;
end;

procedure TBlkPst.UPOPstDisDone(Sender: TObject);
begin
  Self.status := pstOff;
end;

procedure TBlkPst.UPONPstDone(Sender: TObject);
begin
  PanelServer.ConfirmationSequence(TIDContext(Sender), Self.CSNPStDone,
    (TPanelConnData(TIDContext(Sender).data).UPO_ref as TArea), 'Nouzové zrušení obsluhy PSt',
    TBlocks.GetBlksList(Self), TArea.GetCSConditions(TArea.GetCSCondition(Self, 'Není základní poloha prvku PSt')));
end;

procedure TBlkPst.CSNPStDone(Sender: TIDContext; success: Boolean);
begin
  if (success) then
    Self.status := pstOff;
end;

procedure TBlkPst.MenuSTITClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.note);
end;

procedure TBlkPst.MenuZAVEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuZAVDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
end;

procedure TBlkPst.MenuHoukEnableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    RCSi.SetOutput(Self.m_settings.rcsOutHorn, osf60);
  except
    PanelServer.BottomError(SenderPnl, 'Nelze nastavit RCS výstup!', TArea(SenderOR).ShortName, 'TECHNOLOGIE');
  end;
end;

procedure TBlkPst.MenuHoukDisableClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    RCSi.SetOutput(Self.m_settings.rcsOutHorn, osDisabled);
  except
    PanelServer.BottomError(SenderPnl, 'Nelze nastavit RCS výstup!', TArea(SenderOR).ShortName, 'TECHNOLOGIE');
  end;
end;

procedure TBlkPst.MenuAdminPoOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    RCSi.SetInput(Self.m_settings.rcsInRelease, 0);
    RCSi.SetInput(Self.m_settings.rcsInTake, 1);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

procedure TBlkPst.MenuAdminPoOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  try
    RCSi.SetInput(Self.m_settings.rcsInRelease, 1);
    RCSi.SetInput(Self.m_settings.rcsInTake, 0);
  except
    PanelServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TArea(SenderOR).ShortName,
      'SIMULACE');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkPst.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := inherited;

  if ((Self.status = pstOff) and (not Self.zaver)) then
    Result := Result + 'PST>,'
  else if (Self.status = pstTakeReady) then
    Result := Result + 'PST<,'
  else if (Self.status = pstActive) then
    Result := Result + '!NPST,';

  try
    if (RCSi.GetOutput(Self.m_settings.rcsOutHorn) > 0) then
      Result := Result + 'HOUK<,'
    else
      Result := Result + 'HOUK>,';
  except
    on E: RCSException do begin end;
  end;

  Result := Result + 'STIT,';

  if (RCSi.simulation) then
  begin
    Result := Result + '-,';

    if (Self.status = pstTakeReady) then
      Result := Result + '*PO>,'
    else if (Self.status = pstActive) then
      Result := Result + '*PO<,';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (Self.enabled) then
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkPst.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (not Self.enabled) then
    Exit();

  if (item = 'PST>') then
    Self.MenuPstEnClick(SenderPnl, SenderOR)
  else if (item = 'PST<') then
    Self.MenuPstDisClick(SenderPnl, SenderOR)
  else if (item = 'NPST') then
    Self.MenuNPstClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuSTITClick(SenderPnl, SenderOR)
  else if (item = 'ZAV>') then
    Self.MenuZAVEnableClick(SenderPnl, SenderOR)
  else if (item = 'ZAV<') then
    Self.MenuZAVDisableClick(SenderPnl, SenderOR)
  else if (item = 'HOUK>') then
    Self.MenuHoukEnableClick(SenderPnl, SenderOR)
  else if (item = 'HOUK<') then
    Self.MenuHoukDisableClick(SenderPnl, SenderOR)
  else if (item = 'PO>') then
    Self.MenuAdminPoOnClick(SenderPnl, SenderOR)
  else if (item = 'PO<') then
    Self.MenuAdminPoOffClick(SenderPnl, SenderOR);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkPst.GetEnabled(): Boolean;
begin
  Result := (Self.status > pstDisabled);
end;

function TBlkPst.GetZaver(): Boolean;
begin
//  Result := (Self.m_state.zaver > 0);
// TODO
  Result := false;
end;

function TBlkPst.GetEmLock(): Boolean;
begin
  Result := (Self.m_state.emLock > 0);
end;

procedure TBlkPst.SetEmLock(new: Boolean);
begin
  if (new) then
  begin
    Inc(Self.m_state.emLock);
    if (Self.m_state.emLock = 1) then
    begin
      {if (Self.keyReleased) then
      begin
        Self.m_state.keyReleased := false;
        Self.CallChangeToTurnout();
      end;}
      inherited Change();
    end;
  end else begin
    if (Self.m_state.emLock > 0) then
      Dec(Self.m_state.emLock);
    if (Self.m_state.emLock = 0) then
    begin
      // pokud vyhybky nejsou ve spravne poloze, automaticky uvolnujeme klic
      {if (not Self.IsRightPoloha()) then
        Self.keyReleased := true
      else
      begin
        Self.CallChangeToTurnout();
        inherited Change();
      end;
      Blocks.NouzZaverZrusen(Self);}
    end;
  end;
end;

procedure TBlkPst.SetNote(note: string);
begin
  if (note <> Self.m_state.note) then
  begin
    Self.m_state.note := note;
    inherited Change();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.DecreaseEmLock(amount: Cardinal);
begin
  if (Self.m_state.emLock = 0) then
    Exit();

  if (amount > Self.m_state.emLock) then
    Self.m_state.emLock := 0
  else
    Self.m_state.emLock := Self.m_state.emLock - amount;

  if (not Self.emLock) then
    Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkPst.PanelStateString(): string;
var fg, bg: TColor;
begin
  Result := inherited;

  if (Self.note <> '') then
    bg := clTeal
  else
    bg := clBlack;

  if ((diag.showZaver) and (Self.zaver)) then
    bg := clGreen;

  case (Self.status) of
    pstOff, pstRefuging: fg := $A0A0A0;
    pstTakeReady: fg := clWhite;
    pstActive: fg := clBlue;
  else
    fg := clFuchsia;
  end;

  if (Self.emLock) then
    fg := clAqua;

  Result := Result + ownConvert.ColorToStr(fg) + ';';
  Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;
  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkPst.GetPtState(json: TJsonObject);
begin
  // TODO
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkPst.GetSettings(): TBlkPstSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkPst.SetSettings(settings: TBlkPstSettings);
begin
  if (Self.m_settings.tracks <> settings.tracks) then
    Self.m_settings.tracks.Free();
  if (Self.m_settings.turnouts <> settings.turnouts) then
    Self.m_settings.turnouts.Free();
  if (Self.m_settings.signals <> settings.signals) then
    Self.m_settings.signals.Free();
  if (Self.m_settings.refugees <> settings.refugees) then
    Self.m_settings.refugees.Free();

  Self.m_settings := settings;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.ShowIndication();
begin
  if (not RCSi.Started) then
    Exit();

  try
    case (Self.status) of
      pstDisabled, pstOff, pstRefuging: RCSi.SetOutput(Self.m_settings.rcsOutTaken, 0);
      pstTakeReady: RCSi.SetOutput(Self.m_settings.rcsOutTaken, TRCSOutputState.osf180);
      pstActive: RCSi.SetOutput(Self.m_settings.rcsOutTaken, 1);
    end;
  except
    on E: RCSException do begin end;
  end
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.NoteUPO(SenderPnl: TIDContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
  UPO_EscCallback: TNotifyEvent);
var UPO: TUPOItems;
  item: TUPOItem;
begin
  UPO := TList<TUPOItem>.Create();
  try
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
    PanelServer.UPO(SenderPnl, UPO, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
  finally
    UPO.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.ActivationBarriers(var barriers: TList<TJCBarrier>);
begin
  if (Self.note <> '') then
    barriers.Add(JCBarrier(barBlockNote, Self));

  // tracks
  for var trackId in Self.m_settings.tracks do
  begin
    var track: TBlkTrack := TBlkTrack(Blocks.GetBlkByID(trackId));

    if (track = nil) then
    begin
      barriers.Add(JCBarrier(barBlockNotExists, nil, trackId));
      Exit();
    end;

    if ((track.typ <> btTrack) and (track.typ <> btRT)) then
    begin
      barriers.Add(JCBarrier(barBlockWrongType, track, trackId));
      Exit();
    end;

    if (track.occupied = TTrackState.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, track));

    if (track.occupied <> TTrackState.Free) then
      barriers.Add(JCBarrier(barTrackOccupied, track));

    if (track.Zaver <> TZaver.no) then
    begin
      if (track.Zaver = TZaver.ab) then
        barriers.Add(JCBarrier(barTrackAB, track))
      else
        barriers.Add(JCBarrier(barTrackZaver, track));
    end;

    if (track.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, track));

    if (track.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, track));
  end;

  // turnouts
  for var turnoutId in Self.m_settings.turnouts do
  begin
    var turnout: TBlkTurnout := TBlkturnout(Blocks.GetBlkByID(turnoutId));

    if (turnout = nil) then
    begin
      barriers.Add(JCBarrier(barBlockNotExists, nil, turnoutId));
      Exit();
    end;

    if (turnout.typ <> btTurnout) then
    begin
      barriers.Add(JCBarrier(barBlockWrongType, turnout, turnoutId));
      Exit();
    end;

    if (turnout.position = TTurnoutPosition.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, turnout));

    if ((turnout.position = TTurnoutPosition.none) or (turnout.position = TTurnoutPosition.both)) then
      barriers.Add(JCBarrier(barTurnoutNoPos, turnout));

    if (turnout.zaver <> TZaver.no) then
      barriers.Add(JCBarrier(barTrackZaver, turnout));

    if (turnout.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, turnout));

    if (turnout.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, turnout));

    if (turnout.PstIs()) then
      barriers.Add(JCBarrier(barTurnoutPst, turnout));

    if (turnout.emLock) then
      barriers.Add(JCBarrier(barTurnoutEmLock, turnout))
    else if (turnout.outputLocked) then
      barriers.Add(JCBarrier(barTurnoutLocked, turnout));

    // coupling
    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(turnout.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
      else if (coupling.outputLocked) then
        barriers.Add(JCBarrier(barTurnoutLocked, coupling));

      if (coupling.occupied = TTrackState.occupied) then
        barriers.Add(JCBarrier(barTrackOccupied, coupling));
    end;

    if ((coupling <> nil) and (coupling.PstIs())) then
      barriers.Add(JCBarrier(barTurnoutPst, coupling));
  end;

  // refugees
  for var refugeeZav: TPstRefugeeZav in Self.m_settings.refugees do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.Block));

    if (refugee = nil) then
    begin
      barriers.Add(JCBarrier(barBlockNotExists, nil, refugeeZav.block));
      Exit();
    end;

    if (refugee.typ <> btTurnout) then
    begin
      barriers.Add(JCBarrier(barBlockWrongType, refugee, refugeeZav.block));
      Exit();
    end;

    if (refugee.position = TTurnoutPosition.disabled) then
      barriers.Add(JCBarrier(barBlockDisabled, refugee));

    if ((refugee.position = TTurnoutPosition.none) or (refugee.position = TTurnoutPosition.both)) then
      barriers.Add(JCBarrier(barTurnoutNoPos, refugee));

    if (refugee.lockout <> '') then
      barriers.Add(JCBarrier(barBlockLockout, refugee));

    if (refugee.note <> '') then
      barriers.Add(JCBarrier(barBlockNote, refugee));

    if (refugee.position <> refugeeZav.position) then
    begin
      if (refugee.emLock) then
        barriers.Add(JCBarrier(barTurnoutEmLock, refugee))

      else if (refugee.outputLocked) then
        barriers.Add(JCBarrier(barRefugeeLocked, refugee));

      if (refugee.occupied = TTrackState.occupied) then
        barriers.Add(JCBarrier(barRefugeeOccupied, refugee));
    end;

    if (refugee.PstIs()) then
      barriers.Add(JCBarrier(barRefugeePst, refugee));

    var coupling: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugee.GetSettings.coupling));
    if (coupling <> nil) then
    begin
      if (coupling.lockout <> '') then
        barriers.Add(JCBarrier(barBlockLockout, coupling));

      if (coupling.note <> '') then
        barriers.Add(JCBarrier(barBlockNote, coupling));

      if (coupling.PstIs()) then
        barriers.Add(JCBarrier(barRefugeePst, coupling));

      if (refugee.position <> refugeeZav.position) then
      begin
        if (TBlkTurnout(coupling).Zaver > TZaver.no) then
        begin
          if (TBlkTurnout(coupling).Zaver = TZaver.ab) then
            barriers.Add(JCBarrier(barTrackAB, coupling))
          else
            barriers.Add(JCBarrier(barTrackZaver, coupling));
        end;

        if (TBlkTurnout(coupling).emLock) then
          barriers.Add(JCBarrier(barTurnoutEmLock, coupling))
        else if (TBlkTurnout(coupling).outputLocked) then
          barriers.Add(JCBarrier(barTurnoutLocked, coupling));

        if (TBlkTurnout(coupling).occupied = TTrackState.occupied) then
          barriers.Add(JCBarrier(barTrackOccupied, coupling));
      end;
    end;
  end;

  // signals
  for var signalId in Self.m_settings.signals do
  begin
    var signal: TBlkSignal := TBlkSignal(Blocks.GetBlkByID(signalId));

    if (signal = nil) then
    begin
      barriers.Add(JCBarrier(barBlockNotExists, nil, signalId));
      Exit();
    end;

    if (signal.typ <> btSignal) then
    begin
      barriers.Add(JCBarrier(barBlockWrongType, signal, signalId));
      Exit();
    end;

    if (signal.signal <> ncStuj) then
      barriers.Add(JCBarrier(barSignalActive, signal));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkPst.Activate(senderPnl: TIdContext; senderOR: TObject);
begin
  Self.m_state.senderOR := senderOR;
  Self.m_state.senderPnl := senderPnl;

  Self.Log('Požadavek na aktivaci Pst, kontroluji podmínky...', ltMessage);

  var barriers: TJCBarriers := TJCBarriers.Create();
  var UPO: TUPOItems := TList<TUPOItem>.Create;
  try
    Self.ActivationBarriers(barriers);

    var critical: Boolean := false;
    for var barrier: TJCBarrier in barriers do
    begin
      if ((JCBarriers.CriticalBarrier(barrier.typ)) or (not Self.WarningBarrier(barrier.typ))) then
      begin
        critical := true;
        UPO.Add(Self.BarrierToMessage(barrier));
      end;
    end;

    if (critical) then
    begin
      Self.Log('Celkem ' + IntToStr(barriers.Count) + ' bariér, ukončuji přebírání PSt', ltMessage);
      if (senderPnl <> nil) then
        PanelServer.UPO(Self.m_state.senderPnl, UPO, true, nil, nil, Self);
      Exit();

    end else begin
      // barriers to confirm
      if ((barriers.Count > 0) and (senderPnl <> nil)) then
      begin
        Self.Log('Celkem ' + IntToStr(barriers.Count) + ' warning bariér, žádám potvrzení...', ltMessage);
        for var i: Integer := 0 to barriers.Count - 1 do
          UPO.Add(Self.BarrierToMessage(barriers[i]));

        PanelServer.UPO(Self.m_state.senderPnl, UPO, false, Self.BarriersUPOOKCallback, nil, Self);
        Exit();
      end;
    end;

    Self.MoveRefugees();
  finally
    barriers.Free();
    UPO.Free();
  end;
end;

class function TBlkPst.WarningBarrier(typ: TJCBarType): Boolean;
begin
  if (typ = barTrackOccupied) then
    Exit(true);
  Result := JCBarriers.JCWarningBarrier(typ);
end;

class function TBlkPst.BarrierToMessage(barrier: TJCBarrier): TUPOItem;
begin
  if (barrier.typ = barTrackOccupied) then
    barrier.typ := barTrackLastOccupied;
  Result := JCBarriers.JCBarrierToMessage(barrier);
  if (barrier.typ = barTrackLastOccupied) then
    Result[0] := GetUPOLine('POZOR !', taCenter, clBlack, clYellow);
end;

procedure TBlkPst.BarriersUPOOKCallback(Sender: TObject);
begin
  Self.Log('Upozornění schválena, kontroluji znovu bariéry...', ltMessage);

  var critical: Boolean := false;
  var barriers: TJCBarriers := TJCBarriers.Create();

  try
    Self.ActivationBarriers(barriers);

    for var barrier in barriers do
      if ((barrier.typ <> barProcessing) and (JCBarriers.CriticalBarrier(barrier.typ))) then
      begin
        critical := true;
        break;
      end;

    if (critical) then
    begin
      Self.Log('Nelze převít PSt - objevily se kritické bariéry', ltMessage);
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      Exit();
    end;

    // Confirmation sequence barriers?
    var conditions: TList<TConfSeqItem> := TList<TConfSeqItem>.Create();
    for var barrier in barriers do
    begin
      if (JCBarriers.IsCSBarrier(barrier.typ)) then
        conditions.Add(TArea.GetCSCondition(barrier.block, JCBarriers.BarrierGetCSNote(barrier.typ)));
    end;

    if (conditions.Count > 0) then
    begin
      Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...', ltMessage);

      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.BarriersCSCallback, (Self.m_state.senderOR as TArea),
          'Předání obsluhy na PSt', TBlocks.GetBlksList(Self), conditions);
    end else begin
      Self.MoveRefugees();
    end;

  finally
    barriers.Free();
  end;
end;

procedure TBlkPst.BarriersCSCallback(Sender: TIdContext; success: Boolean);
begin
  if (not success) then
  begin
    Self.Log('Potvrzovací sekvence nepotvrzena, zrušeno předávání PSt', ltMessage);
    Exit();
  end;

  var barriers: TJCBarriers := TJCBarriers.Create();

  try
    Self.ActivationBarriers(barriers);

    // existuji kriticke bariery?
    var critical: Boolean := false;
    for var barrier in barriers do
      if ((barrier.typ <> barProcessing) and (JCBarriers.CriticalBarrier(barrier.typ))) then
      begin
        critical := true;
        break;
      end;

    // behem potvrzovani se mohly vyskytnout
    if (critical) then
    begin
      Self.Log('Nelze postavit - kritické bariéry', ltMessage);
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      barriers.Free();
      Exit();
    end;

    Self.MoveRefugees();
  finally
    barriers.Free();
  end;
end;

procedure TBlkPst.MoveRefugees();
begin
  Self.status := pstRefuging;
  Self.Log('Pst připraveno, nastavuji odvraty...', ltMessage);

  // Move all refugees at once, we suppose there is not many of them...
  // In case of many refugees one could implement window refuging in future...
  for var refugeeZav in Self.m_settings.refugees do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));
    refugee.IntentionalLock();
    // Warning: this may call callback directly
    refugee.SetPosition(TTurnoutPosition(refugeeZav.position), true, false, Self.RefugeeMoved, Self.RefugeeNotMoved);
  end;
end;

procedure TBlkPst.RefugeeMoved(Sender: TObject);
begin
  if (Self.AllRefugeesInPosition()) then
  begin
    Self.Log('Všechny odvraty nastaveny, PSt připraveno na převzetí.', ltMessage);
    Self.status := pstTakeReady;
  end;
end;

procedure TBlkPst.RefugeeNotMoved(Sender: TObject; error: TTurnoutSetError);
begin
  Self.Log('Nepřestavena '+(Sender as TBlkTurnout).name, ltMessage);
  if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
    PanelServer.BottomError(Self.m_state.senderPnl, 'Nepřestavena ' + (Sender as TBlkTurnout).name + ': ' +
      TBlkTurnout.SetErrorToMsg(error), (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');

  Self.status := pstOff;
end;

function TBlkPst.AllRefugeesInPosition(): Boolean;
begin
  for var refugeeZav in Self.m_settings.refugees do
  begin
    var refugee: TBlkTurnout := TBlkTurnout(Blocks.GetBlkByID(refugeeZav.block));
    if (refugee.position <> refugeeZav.position) then
      Exit(false);
  end;
  Result := true;
end;

end.
