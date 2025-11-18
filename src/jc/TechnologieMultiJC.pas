unit TechnologieMultiJC;

{
  Technologie slozenych jizdnich cest.
  Slozena jizdni cesta nikdy nemuze byt stavena jako nouzova cesta.
}

interface

uses
  IniFiles, TechnologieJC, Generics.Collections, BlockDb, IdContext, SysUtils,
  Classes, Generics.Defaults, Math, Block, BlockSignal, JCBarriers, Logging,
  BlockTrack;

type

  TMultiJCState = record
    SenderOR: TObject;
    SenderPnl: TIdContext;
    activatingJCs: TList<Integer>; // JCs in stack=VZ are activated all at once at the beginning, here is the rest
    abAfter: Boolean;
    step: JCStep;
    timeOut: TDateTime; // cas, pri jehoz prekroceni dojde k timeoutu staveni JC
  end;

  TMultiJCData = record
    name: string;
    JCs: TList<Integer>;
    vb: TList<Integer>;
    id: Integer;
  end;

  EInvalidID = class(Exception);

  TMultiJC = class

  private const
    _def_mutiJC_staveni: TMultiJCState = (SenderOR: nil; SenderPnl: nil; activatingJCs: nil; step: JCStep.stepDefault);

  private
    m_data: TMultiJCData;
    m_state: TMultiJCState;
    activatingJC: TJC; // zde je ulozena JC, ktera se aktualne stavi

    function IsActivating(): Boolean;
    procedure Log(msg: string; level: TLogLevel = llInfo; source: TLogSource = lsJC);

    procedure UpdateTimeOut();
    procedure SetStep(step: JCStep);

    procedure CritBarieraEsc(Sender: TObject);
    procedure UPO_OKCallback(Sender: TObject); // callback potvrzeni upozorneni
    procedure UPO_EscCallback(Sender: TObject); // callback zamitnuti upozorneni
    procedure CS_Callback(Sender: TIdContext; success: Boolean); // callback potvrzovaci sekvence staveni JC

    function GetFirstSignal(): TBlk;
    function GetLastTrack(): TBlkTrack;

    procedure ActivatePaths();

  public

    changed: Boolean;

    constructor Create(); overload;
    constructor Create(data: TMultiJCData); overload;
    destructor Destroy(); override;

    procedure UpdateActivating();

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile);

    procedure Activate(SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
    procedure CancelActivating(reason: string = '');

    function Match(blocks: TList<TBlk>): Boolean;
    function StartSignal(): TBlkSignal;
    function BarriersActivatingJCs(): TJCBarriers;

    property data: TMultiJCData read m_data write m_data;
    property state: TMultiJCState read m_state;
    property name: string read m_data.name;
    property activating: Boolean read IsActivating;
    property id: Integer read m_data.id;
    property step: JCStep read m_state.step write SetStep;
    property firstSignal: TBlk read GetFirstSignal;
    property lastTrack: TBlkTrack read GetLastTrack;

    class function IdComparer(): IComparer<TMultiJC>;
  end;

implementation

uses TJCDatabase, Area, ownConvert, AreaStack, UPO, PanelConnData,
  TCPServerPanel, ConfSeq, appEv, timeHelper;

/// /////////////////////////////////////////////////////////////////////////////

constructor TMultiJC.Create();
begin
  inherited Create();

  Self.changed := true;
  Self.m_state := _def_mutiJC_staveni;

  Self.m_data.JCs := TList<Integer>.Create();
  Self.m_data.vb := TList<Integer>.Create();
  Self.m_state.activatingJCs := TList<Integer>.Create();

  Self.activatingJC := nil;
end;

constructor TMultiJC.Create(data: TMultiJCData);
begin
  inherited Create();

  Self.changed := true;
  Self.m_state := _def_mutiJC_staveni;

  Self.m_data := data;

  if (not Assigned(data.JCs)) then
    Self.m_data.JCs := TList<Integer>.Create();
  if (not Assigned(data.vb)) then
    Self.m_data.vb := TList<Integer>.Create();

  Self.m_state.activatingJCs := TList<Integer>.Create();
end;

destructor TMultiJC.Destroy();
begin
  if (Assigned(Self.m_data.JCs)) then
    FreeAndNil(Self.m_data.JCs);
  if (Assigned(Self.m_data.vb)) then
    FreeAndNil(Self.m_data.vb);
  if (Assigned(Self.m_state.activatingJCs)) then
    Self.m_state.activatingJCs.Free();

  inherited Destroy();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.LoadData(ini: TMemIniFile; section: string);
begin
  try
    Self.m_data.id := StrToInt(section);
  except
    on E: EConvertError do
      raise EInvalidID.Create('Neplatné id mJC : ' + section);
  end;

  Self.m_data.name := ini.ReadString(section, 'nazev', section);

  Self.m_data.JCs.Clear();
  Self.m_data.vb.Clear();

  // nacteni jizdnich cest ve slozene jizdni ceste:
  begin
    var sl: TStrings := TStringList.Create();
    try
      ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'JCs', '')), sl);
      for var i: Integer := 0 to sl.Count - 1 do
        Self.m_data.JCs.Add(StrToInt(sl[i]));
    finally
      sl.Free();
    end;
  end;

  // nacteni variantnich bodu
  begin
    var sl: TStrings := TStringList.Create();
    try
      ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
      for var i: Integer := 0 to sl.Count - 1 do
        Self.m_data.vb.Add(StrToInt(sl[i]));
    finally
      sl.Free();
    end;
  end;
end;

procedure TMultiJC.SaveData(ini: TMemIniFile);
var section: string;
begin
  section := IntToStr(Self.id);

  ini.WriteString(section, 'nazev', Self.m_data.name);

  begin
    var str: string := SerializeIntList(Self.data.JCs);
    if (str <> '') then
      ini.WriteString(section, 'JCs', str);
  end;

  begin
    var str: string := SerializeIntList(Self.data.vb);
    if (str <> '') then
      ini.WriteString(section, 'vb', str);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.UpdateActivating();
begin
  Self.UpdateTimeOut();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.Activate(SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
begin
  Self.m_state.timeOut := Now + EncodeTimeSec(_JC_INITPOTVR_TIMEOUT_SEC);

  Self.m_state.SenderOR := SenderOR;
  Self.m_state.SenderPnl := SenderPnl;
  Self.m_state.abAfter := abAfter;

  Self.Log('Požadavek na stavění, kontroluji podmínky ...');

  for var jcId in Self.m_data.JCs do
  begin
    if (JCDb.GetJCByID(jcId) = nil) then
    begin
      Self.CancelActivating('Cesta ve složené JC neexistuje');
      Exit();
    end;
  end;

  Self.m_state.activatingJCs.Clear();
  for var jcId in Self.m_data.JCs do
  begin
    var jc: TJC := JCDb.GetJCByID(jcId);
    if ((jc.signal.areas.Count > 0) and (jc.signal.areas[0].stack.mode = TORStackMode.VZ)) then
    begin
      jc.signal.areas[0].stack.AddJC(jc, SenderPnl, false, abAfter);
    end else
      Self.m_state.activatingJCs.Add(jcId);
  end;

  if (Self.m_state.activatingJCs.Count <> Self.m_data.JCs.Count) then
    TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks()
  else
    TPanelConnData(SenderPnl.Data).pathBlocks.Clear();

  // No paths to activate in PV mode -> exit
  if (Self.m_state.activatingJCs.Count = 0) then
  begin
    Self.Log('Zásobník OŘ všech JC v režimu VZ -> nestavím žádnou cestu rovnou');
    Exit();
  end;

  var barriers: TJCBarriers := Self.BarriersActivatingJCs();
  var UPO: TUPOItems := TList<TUPOItem>.Create();
  try
    // Are critical barriers present?
    var critical: Boolean := false;
    for var barrier: TJCBarrier in barriers do
    begin
      if (not barrier.CanContinueByConfirm()) then
      begin
        critical := true;
        UPO.Add(barrier.ToUPO());
      end;
    end;

    if (critical) then
    begin
      // yes -> report
      Self.Log('Celkem ' + IntToStr(barriers.Count) + ' bariér, ukončuji stavění');
      if (senderPnl <> nil) then
      begin
        Self.step := stepCritBarriers;
        PanelServer.UPO(Self.m_state.senderPnl, UPO, true, nil, Self.CritBarieraEsc, Self);
      end;
    end else begin
      // non-critical barriers -> confirmation request
      // when there is no one to ask, just activate the path right now
      if ((barriers.Count > 0) and (senderPnl <> nil)) then
      begin
        Self.Log('Celkem ' + IntToStr(barriers.Count) + ' warning bariér, žádám potvrzení...');
        for var i: Integer := 0 to barriers.Count - 1 do
          UPO.Add(barriers[i].ToUPO());

        PanelServer.UPO(Self.m_state.senderPnl, UPO, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
        Self.step := stepConfBarriers;
      end else begin
        // no barriers -> activate child paths
        Self.Log('Žádné bariéry, stavím');
        Self.ActivatePaths();
      end;
    end;
  except
    on E:Exception do
    begin
      Self.CancelActivating('Výjimka při stavění');
      AppEvents.LogException(E, 'TMultiJC.Activate');
    end;
  end;

  barriers.Free();
  UPO.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.CancelActivating(reason: string = '');
begin
  if (reason <> '') then
  begin
    if (Self.m_state.senderPnl <> nil) then
      PanelServer.SendInfoMsg(Self.m_state.senderPnl, reason);
    Self.Log('Nelze postavit - ' + reason);
  end;

  case (Self.step) of
    stepNcBarrierUpdate:
      begin
        if (Self.m_state.senderPnl <> nil) then
          PanelServer.CSClose(Self.m_state.senderPnl, reason);
      end;

    stepConfBarriers, stepCritBarriers:
      begin
        if (Self.m_state.senderPnl <> nil) then
          PanelServer.CancelUPO(Self.m_state.senderPnl, Self);
      end;
  end;

  Self.step := stepDefault;

  // zrusime zacatek staveni na navestidle
  if (Self.data.JCs.Count > 0) then
  begin
    var firstJC: TJC := JCDb.GetJCByID(Self.data.JCs[0]);
    if (firstJC <> nil) then
      firstJC.CancelSignalBegin();
  end;

  // zrusime konec staveni na poslednim useku posledni JC
  begin
    var lastJC := JCDb.GetJCByID(Self.m_data.JCs[Self.m_data.JCs.Count - 1]);
    if (lastJC <> nil) then
      lastJC.CancelTrackEnd();
  end;

  // zrusime konec staveni na vsech variantnich bodech
  for var i: Integer := 0 to Self.m_data.vb.Count - 1 do
  begin
    var track := Blocks.GetBlkTrackOrRTByID(Self.m_data.vb[i]);
    if (track <> nil) then
      track.jcEnd := TZaver.no;
  end;

  Self.m_state.SenderPnl := nil;
  Self.m_state.SenderOR := nil;
  Self.m_state.abAfter := False;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.IsActivating(): Boolean;
begin
  Result := (Self.m_state.step > JCStep.stepDefault);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TMultiJC.IdComparer(): IComparer<TMultiJC>;
begin
  Result := TComparer<TMultiJC>.Construct(
    function(const mJC1, mJC2: TMultiJC): Integer
    begin
      Result := CompareValue(mJC1.id, mJC2.id);
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.Match(blocks: TList<TBlk>): Boolean;
begin
  if ((Self.data.JCs.Count < 2) or (blocks.Count < 2)) then
    Exit(false);

  var JC := JCDb.GetJCByID(Self.data.JCs[0]);
  if (JC = nil) then
    Exit(false);

  if (blocks[0].typ <> btSignal) then
    Exit(false);
  var startSignal: TBlkSignal := TBlkSignal(blocks[0]);

  if (JC.data.signalId <> blocks[0].id) then
    Exit(false);
  if (Integer(startSignal.selected) <> Integer(JC.typ)) then
    Exit(false);

  // posledni blok musi byt posledni blok posledni jizdni cesty
  JC := JCDb.GetJCByID(Self.data.JCs[Self.data.JCs.Count - 1]);
  if (JC = nil) then
    Exit(false);
  if (JC.data.tracks[JC.data.tracks.Count - 1] <> blocks[blocks.Count-1].id) then
    Exit(false);

  // kontrola variantnich bodu
  if (blocks.Count <> Self.data.vb.Count+2) then
    Exit(false);

  for var j: Integer := 0 to Self.data.vb.Count - 1 do
    if (Self.data.vb[j] <> blocks[j+1].id) then
      Exit(false);

  for var j: Integer := 0 to Self.data.JCs.Count - 1 do
    if (JCDb.GetJCByID(Self.data.JCs[j]) = nil) then
      Exit(false);

  Exit(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.StartSignal(): TBlkSignal;
var JC: TJC;
begin
  if (Self.data.JCs.Count > 0) then
  begin
    JC := JCDb.GetJCByID(Self.data.JCs[0]);
    if (JC = nil) then
      Result := nil
    else
      Result := JC.signal as TBlkSignal;
  end
  else
    Result := nil
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.BarriersActivatingJCs(): TJCBarriers;
begin
  Result := TJCBarriers.Create();
  try
    for var jcId in Self.m_state.activatingJCs do
    begin
      var jc: TJC := JCDb.GetJCByID(jcId);
      var jcbarriers: TJCBarriers := jc.Barriers();
      jcbarriers.OwnsObjects := False; // objects will be moved!
      try
        if (jcId <> Self.m_data.JCs[Self.m_data.JCs.Count-1]) then
        begin
          for var barrier: TJCBarrier in jcbarriers do
          begin
            var newBar: TJCBarrier := barrier;
            if ((barrier.ClassType = TJCBarTrackLastOccupied) or (barrier.ClassType = TJCBarRailwayOccupied)) then
            begin
              Result.Add(TJCBarTrackOccupied.Create((barrier as TJCBlockBarrier).block));
              barrier.Free();
            end else begin
              Result.Add(barrier);
            end;
          end;
        end else begin
          Result.AddRange(jcbarriers);
        end;
      finally
        jcbarriers.Free();
      end;
    end;
  except
    Result.Free();
    raise;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.Log(msg: string; level: TLogLevel; source: TLogSource);
begin
  Logging.Log('Složená JC ' + Self.name + ': ' + msg, level, source);
end;

/// /////////////////////////////////////////////////////////////////////////////

// timeout staveni JC
procedure TMultiJC.UpdateTimeOut();
begin
  // na nouzovou cestu se nevztahuje timeout
  if (not Self.activating) then
    Exit();

  if (Now > Self.m_state.timeOut) then
  begin
    case (Self.step) of
      stepConfSeq:
        begin
          if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
            PanelServer.CSClose(Self.m_state.senderPnl, 'Timeout');
        end;

    else
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Timeout ' + Self.name,
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
    end; // else case

    Self.CancelActivating('Překročení času stavění JC');
  end; // if timeout
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.SetStep(step: JCStep);
begin
  Self.m_state.step := step;
  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.CritBarieraEsc(Sender: TObject);
begin
  Self.CancelActivating();
end;

procedure TMultiJC.UPO_OKCallback(Sender: TObject);
begin
  if (Self.step <> stepConfBarriers) then
    Exit();

  Self.Log('Krok 1 : upozornění schválena, kontroluji znovu bariéry');

  // Pripadne vyvolani jizdni cesty s potvrzenim ...
  // (radsi nejdriv zkontrolujeme, jestli se neobjevily kriticke bariery)
  var barriers: TJCBarriers := Self.BarriersActivatingJCs();
  try
    if (not JCBarriers.CanContinueByConfirmButProcessing(barriers)) then
    begin
      Self.CancelActivating('Nelze postavit - kritické bariéry');
      if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
        PanelServer.BottomError(Self.m_state.senderPnl, 'Nelze postavit ' + Self.name + ' - kritické bariéry',
          (Self.m_state.senderOR as TArea).ShortName, 'TECHNOLOGIE');
      Exit();
    end;

    // existuji bariery na potvrzeni potvrzovaci sekvenci?
    var csItems: TList<TConfSeqItem> := TList<TConfSeqItem>.Create();
    try
      for var barrier in barriers do
        if ((barrier.IsRisky()) and (barrier.InheritsFrom(TJCBlockBarrier))) then
          csItems.Add(CSItem((barrier as TJCBlockBarrier).block, barrier.RiskyNote()));

      if (csItems.Count > 0) then
      begin
        // ano, takoveto bariery existuji -> potvrzovaci sekvence
        Self.Log('Bariéry s potvrzovací sekvencí, žádám potvrzení...');

        if (Self.m_state.senderPnl <> nil) and (Self.m_state.senderOR <> nil) then
          PanelServer.ConfirmationSequence(Self.m_state.senderPnl, Self.CS_Callback, (Self.m_state.senderOR as TArea),
            'Jízdní cesta s potvrzením', GetObjsList(Self.firstSignal, Self.lastTrack), csItems, True, False);

        Self.step := stepConfSeq;
      end else begin
        // ne, takoveto bariery neexistuji -> stavim jizdni cestu
        Self.ActivatePaths();
      end;
    finally
      csItems.Free();
    end;

  finally
    barriers.Free();
  end;
end;

procedure TMultiJC.UPO_EscCallback(Sender: TObject);
begin
  if (Self.step = stepConfBarriers) then
  begin
    Self.Log('UPO odmítnuto');
    Self.CancelActivating();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.CS_Callback(Sender: TIdContext; success: Boolean);
begin
  if (Self.step <> stepConfSeq) then
    Exit();

  // Checking of the barriers again is not required
  // If any critical barrier occurs, it is found in child-paths activation

  if (success) then
  begin
    Self.Log('Povrzovací sekvence OK');
    Self.ActivatePaths();
  end else begin
    Self.Log('Povrzovací sekvence NOK - ruším stavění');
    Self.CancelActivating();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.GetFirstSignal(): TBlk;
begin
  if (Self.data.JCs.Count = 0) then
    Exit(nil);
  var jc: TJC := JCDb.GetJCByID(Self.data.JCs[0]);
  if (jc = nil) then
    Exit(nil);
  Result := jc.signal;
end;

function TMultiJC.GetLastTrack(): TBlkTrack;
begin
  if (Self.data.JCs.Count = 0) then
    Exit(nil);
  var jc: TJC := JCDb.GetJCByID(Self.data.JCs[Self.data.JCs.Count-1]);
  if (jc = nil) then
    Exit(nil);
  Result := jc.lastTrack;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.ActivatePaths();
begin
  // Activate all paths at once, all barriers should be already confirmed.
  // In case some critical barriers occurs, jc.Activate will show them to the dispatcher.

  Self.Log('Stavím jízdní cesty ...');
  Self.step := stepDefault;

  for var jcId: Integer in Self.m_state.activatingJCs do
  begin
    var jc: TJC := JCDb.GetJCByID(jcId);
    if (jc = nil) then
    begin
      Self.CancelActivating('Jízdní cesta neexistuje');
      Exit();
    end;

    jc.Activate(
      Self.m_state.SenderPnl,
      Self.m_state.SenderOR,
      nil,
      false,
      false,
      Self.m_state.abAfter,
      True
    );
  end;

  Self.m_state.activatingJCs.Clear();
  Self.m_state.SenderPnl := nil;
  Self.m_state.SenderOR := nil;
  Self.m_state.abAfter := False;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
