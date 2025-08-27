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

implementation

uses GetSystems, RCSc, TJCDatabase, Block, BlockTrack, BlockDb, BlockSignal,
  BlockRailwayTrack, BlockTurnout;

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
var JC: TJC;
begin
  if ((not GetFunctions.GetSystemStart()) or (not RCSi.Simulation)) then
    Exit();

  for JC in JCDb do
    if (JC.state.destroyBlock > -1) then
      Self.UpdateJC(JC);
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

      if (track.occupied = TTrackState.occupied) then
      begin
        var trackSettings := track.GetSettings();
        RCSi.SetInputs(trackSettings.RCSAddrs, 0);
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
      RCSi.SetInputs(trackSettings.RCSAddrs, 0);
    end else begin
      // obsadit RozpadBlok
      if (JC.state.destroyBlock >= JC.data.tracks.Count) then
        Exit();

      var track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.state.destroyBlock]);
      var trackSettings := track.GetSettings();
      if (trackSettings.RCSAddrs.Count > 0) then
        RCSi.SetInput(trackSettings.RCSAddrs[0].module, trackSettings.RCSAddrs[0].port, 1);
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
  if ((not GetFunctions.GetSystemStart()) or (not RCSi.Simulation)) then
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
        (rt.prevRT.train = rt.train)) then
      begin
        RCSi.SetInput(TBlkTrack(rt.prevRT).GetSettings().RCSAddrs[0], 0);
        Exit();
      end;
    end;

    // predavani soupravy dopredu
    for var i := 0 to railwaySettings.trackIds.Count - 1 do
    begin
      var rt := TBlkRT(Blocks.GetBlkByID(railwaySettings.trackIds[i]));
      if ((rt.occupied = TTrackState.occupied) and (rt.bpInBlk) and (rt.nextRT <> nil) and
        (rt.nextRT.occupied = TTrackState.free) and ((rt.nextRT.signalCover = nil) or
        (TBlkSignal(rt.nextRT.signalCover).signal > ncStuj))) then
      begin
        RCSi.SetInput(TBlkTrack(rt.nextRT).GetSettings().RCSAddrs[0], 1);
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
    if ((not GetFunctions.GetSystemStart()) or (not RCSi.Simulation)) then
      Exit();

    for Blk in Blocks do
    begin
      if (Blk.typ <> btTurnout) then
        continue;
      turnout := TBlkTurnout(Blk);

      if (((turnout.movingPlus) or (turnout.movingMinus)) and (turnout.posDetection)) then
      begin
        // po 1 sekunde nastavime vstup aktualni polohy na 0
        if ((turnout.state.positionReal <> TTurnoutPosition.none) and (turnout.state.movingStart + EncodeTime(0, 0, 1,
          0) < Now)) then
        begin
          if (turnout.movingPlus) then
            RCSi.SetInput(turnout.rcsInMinus, 0)
          else
            RCSi.SetInput(turnout.rcsInPlus, 0);
        end;

        // po 3 sekundach oznamime koncovou polohu
        if (turnout.state.movingStart + EncodeTime(0, 0, 3, 0) < Now) then
        begin
          if (turnout.movingMinus) then
            RCSi.SetInput(turnout.rcsInMinus, 1)
          else
            RCSi.SetInput(turnout.rcsInPlus, 1);
        end;
      end;
    end;
  except

  end;
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
