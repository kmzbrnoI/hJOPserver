unit Simulation;

interface

uses TechnologieJC, ExtCtrls, BlockRailway, SysUtils;

type
  TJCSimulator = class
  private
    procedure UpdateJC(JC: TJC);

  public
    timer: TTimer;

    constructor Create();
    destructor Destroy(); override;
    procedure OnTimer(Sender: TObject);
  end;

  TRailwaySimulator = class
  private
    procedure Update(railway: TBlkRailway);

  public
    timer: TTimer;

    constructor Create();
    destructor Destroy(); override;
    procedure OnTimer(Sender: TObject);
  end;

  TTurnoutSimulator = class
  private
    procedure OnTimer(Sender: TObject);

  public
    timer: TTimer;

    constructor Create();
    destructor Destroy(); override;
  end;

var
  JCSimulator: TJCSimulator;
  RailwaySimulator: TRailwaySimulator;
  TurnoutSimulator: TTurnoutSimulator;

  procedure InputSim();
  procedure TrainOccupySim();
  procedure DccChangedBoosterSim(dcc: Boolean);

implementation

uses GetSystems, RCSc, TJCDatabase, Block, BlockTrack, BlockDb, BlockSignal,
  BlockRailwayTrack, BlockTurnout, RCSsc, Booster, BoosterDb, BlockCrossing,
  Diagnostics, IfThenElse;

/// /////////////////////////////////////////////////////////////////////////////
// simulator obsazovani useku v jizdni ceste
/// /////////////////////////////////////////////////////////////////////////////

constructor TJCSimulator.Create();
begin
  inherited Create();

  Self.timer := TTimer.Create(nil);
  Self.timer.Interval := 2000;
  Self.timer.Enabled := false;
  Self.timer.OnTimer := Self.OnTimer;
end;

destructor TJCSimulator.Destroy();
begin
  if (Assigned(Self.timer)) then
    FreeAndNil(Self.timer);
  inherited Destroy();
end;

procedure TJCSimulator.OnTimer(Sender: TObject);
begin
  if ((not GetFunctions.GetSystemStart()) or (not RCSs.IsAnySimulation())) then
    Exit();

  for var jc: TJC in JCDb do
    if (jc.state.destroyBlock > -1) then
      Self.UpdateJC(jc);
end;

procedure TJCSimulator.UpdateJC(JC: TJC);
begin
  try
    if (JC.state.destroyBlock < 0) then
      Exit();

    if (((JC.state.destroyBlock = 1) or (JC.state.destroyBlock >= JC.data.tracks.Count)) and
      (JC.state.destroyEndBlock = -1)) then
    begin
      var signal := Blocks.GetBlkSignalByID(JC.data.signalId);
      var track := Blocks.GetBlkTrackOrRTByID(signal.trackId);
      var trackSettings := track.GetSettings();

      if ((track.occupied = TTrackState.occupied) and (RCSs.IsSimulationAll(trackSettings.RCSAddrs))) then
      begin
        RCSs.SetInputs(trackSettings.RCSAddrs, 0);
        Exit();
      end;
    end; // uvolnit usek pred navestidlem

    if (((JC.state.destroyBlock - JC.state.destroyEndBlock >= 2) or (JC.state.destroyBlock >= JC.data.tracks.Count)) and
      (JC.state.destroyEndBlock >= 0)) then
    begin
      if (JC.state.destroyEndBlock >= JC.data.tracks.Count) then
        Exit();

      // uvolnit RozpadRuseniBlok
      var track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.state.destroyEndBlock]);
      var trackSettings := track.GetSettings();
      if (RCSs.IsSimulationAll(trackSettings.RCSAddrs)) then
        RCSs.SetInputs(trackSettings.RCSAddrs, 0);
    end else begin
      // obsadit RozpadBlok
      if (JC.state.destroyBlock >= JC.data.tracks.Count) then
        Exit();

      var track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.state.destroyBlock]);
      var trackSettings := track.GetSettings();
      if ((trackSettings.RCSAddrs.Count > 0) and (RCSs.IsSimulationAll(trackSettings.RCSAddrs))) then
        RCSs.SetInput(trackSettings.RCSAddrs[0], 1);
    end; // else
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// simulator obsazovani trati
/// /////////////////////////////////////////////////////////////////////////////

constructor TRailwaySimulator.Create();
begin
  inherited Create();

  Self.timer := TTimer.Create(nil);
  Self.timer.Interval := 2000;
  Self.timer.Enabled := false;
  Self.timer.OnTimer := Self.OnTimer;
end;

destructor TRailwaySimulator.Destroy();
begin
  if (Assigned(Self.timer)) then
    FreeAndNil(Self.timer);
  inherited Destroy();
end;

procedure TRailwaySimulator.OnTimer(Sender: TObject);
begin
  if ((not GetFunctions.GetSystemStart()) or (not RCSs.IsAnySimulation())) then
    Exit();

  for var blk: TBlk in Blocks do
  begin
    if (blk.typ <> btRailway) then
      continue;
    if (((blk as TBlkRailway).BP) and ((blk as TBlkRailway).occupied) and
      ((TBlkRailway(blk).direction = TRailwayDirection.AtoB) or (TBlkRailway(blk).direction = TRailwayDirection.BtoA)))
    then
      Self.Update(blk as TBlkRailway);
  end;
end;

procedure TRailwaySimulator.Update(railway: TBlkRailway);
var railwaySettings: TBlkRailwaySettings;
begin
  try
    railwaySettings := railway.GetSettings();

    // mazani soupravy vzadu
    for var i := 0 to railwaySettings.trackIds.Count - 1 do
    begin
      var rt := TBlkRT(Blocks.GetBlkByID(railwaySettings.trackIds[i]));
      if ((rt.bpInBlk) and (rt.prevRT <> nil) and (rt.prevRT.occupied = TTrackState.occupied) and
          (rt.prevRT.train = rt.train) and (RCSs.IsSimulation(TBlkTrack(rt.prevRT).GetSettings().RCSAddrs[0]))) then
      begin
        RCSs.SetInput(TBlkTrack(rt.prevRT).GetSettings().RCSAddrs[0], 0);
        Exit();
      end;
    end;

    // predavani soupravy dopredu
    for var i := 0 to railwaySettings.trackIds.Count - 1 do
    begin
      var rt := TBlkRT(Blocks.GetBlkByID(railwaySettings.trackIds[i]));
      if ((rt.occupied = TTrackState.occupied) and (rt.bpInBlk) and (rt.nextRT <> nil) and
          (rt.nextRT.occupied = TTrackState.free) and ((rt.nextRT.signalCover = nil) or
          (TBlkSignal(rt.nextRT.signalCover).signal > ncStuj)) and (RCSs.IsSimulation(TBlkTrack(rt.nextRT).GetSettings().RCSAddrs[0]))) then
      begin
        RCSs.SetInput(TBlkTrack(rt.nextRT).GetSettings().RCSAddrs[0], 1);
        Exit();
      end;
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// simulator staveni vyhybek
/// /////////////////////////////////////////////////////////////////////////////

constructor TTurnoutSimulator.Create();
begin
  inherited Create();

  Self.timer := TTimer.Create(nil);
  Self.timer.Interval := 500;
  Self.timer.Enabled := false;
  Self.timer.OnTimer := Self.OnTimer;
end;

destructor TTurnoutSimulator.Destroy();
begin
  if (Assigned(Self.timer)) then
    FreeAndNil(Self.timer);
  inherited Destroy();
end;

procedure TTurnoutSimulator.OnTimer(Sender: TObject);
var Blk: TBlk;
  turnout: TBlkTurnout;
begin
  try
    if ((not GetFunctions.GetSystemStart()) or (not RCSs.IsAnySimulation())) then
      Exit();

    for Blk in Blocks do
    begin
      if (Blk.typ <> btTurnout) then
        continue;
      turnout := TBlkTurnout(Blk);

      if (((turnout.movingPlus) or (turnout.movingMinus)) and (turnout.posDetection) and
          (RCSs.IsSimulation(turnout.rcsInPlus)) and (RCSs.IsSimulation(turnout.rcsInMinus))) then
      begin
        // po 1 sekunde nastavime vstup aktualni polohy na 0
        if ((turnout.state.positionReal <> TTurnoutPosition.none) and (turnout.state.movingStart + EncodeTime(0, 0, 1, 0) < Now)) then
        begin
          if (turnout.movingPlus) then
            RCSs.SetInput(turnout.rcsInMinus, 0)
          else
            RCSs.SetInput(turnout.rcsInPlus, 0);
        end;

        // po 3 sekundach oznamime koncovou polohu
        if (turnout.state.movingStart + EncodeTime(0, 0, 3, 0) < Now) then
        begin
          if (turnout.movingMinus) then
            RCSs.SetInput(turnout.rcsInMinus, 1)
          else
            RCSs.SetInput(turnout.rcsInPlus, 1);
        end;
      end;
    end;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure InputSim();
begin
  // vychozi stav bloku
  for var blk: TBlk in Blocks do
  begin
    try
      if ((Blk.GetGlobalSettings.typ = btTurnout) and ((Blk as TBlkTurnout).posDetection)) then
        RCSs.SetInput(TBlkTurnout(Blk).rcsInPlus, 1);
      if ((Blk.typ = btCrossing) and (TBlkCrossing(Blk).GetSettings().RCSInputs.open.enabled)) then
        RCSs.SetInput(TBlkCrossing(Blk).GetSettings().RCSInputs.open.addr, 1);
      if ((diag.simSoupravaObsaz) and ((Blk.typ = btTrack) or (Blk.typ = btRT)) and ((Blk as TBlkTrack).IsTrain()) and
        ((Blk as TBlkTrack).occupAvailable)) then
        RCSs.SetInput(TBlkTrack(Blk).GetSettings().RCSAddrs[0], 1);
    except

    end;
  end;

  // vychozi stav zesilovacu
  for var booster: TBooster in Boosters do
  begin
    try
      if ((booster.isPowerDetection) and (RCSs.IsSimulation(booster.settings.rcs.power.addr.addr.system))) then
        RCSs.SetInput(Booster.settings.rcs.power.addr.addr, ite(booster.settings.rcs.power.reversed, 1, 0));
      if ((booster.isOverloadDetection) and (RCSs.IsSimulation(booster.settings.rcs.overload.addr.addr.system))) then
        RCSs.SetInput(Booster.settings.rcs.overload.addr.addr, ite(booster.settings.rcs.overload.reversed, 1, 0));
      if ((booster.isDCCdetection) and (RCSs.IsSimulation(booster.settings.rcs.DCC.addr.addr.system))) then
        RCSs.SetInput(Booster.settings.rcs.DCC.addr.addr, ite(booster.settings.rcs.DCC.reversed, 1, 0));
    except

    end;
  end;
end;

procedure TrainOccupySim();
begin
  for var blk: TBlk in Blocks do
  begin
    if ((Blk.typ <> btTrack) and (Blk.typ <> btRT)) then
      continue;
    if (((Blk as TBlkTrack).IsTrain()) and ((Blk as TBlkTrack).occupAvailable)) then
      RCSs.SetInput((Blk as TBlkTrack).GetSettings().RCSAddrs[0], 1);
  end;
end;

procedure DccChangedBoosterSim(dcc: Boolean);
begin

end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

JCSimulator := TJCSimulator.Create();
RailwaySimulator := TRailwaySimulator.Create();
TurnoutSimulator := TTurnoutSimulator.Create();

finalization

FreeAndNil(JCSimulator);
FreeAndNil(RailwaySimulator);
FreeAndNil(TurnoutSimulator);

end.
