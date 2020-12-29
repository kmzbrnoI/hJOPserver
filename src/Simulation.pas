unit Simulation;

interface

uses TechnologieJC, ExtCtrls, BlockRailway, SysUtils;

type
  TJCSimulator = class
    private

     procedure OnTimer(Sender: TObject);
     procedure UpdateJC(JC: TJC);

    public
     timer: TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TTratSimulator = class
    private

     procedure OnTimer(Sender: TObject);
     procedure UpdateTrat(Trat: TBlkRailway);

    public
     timer: TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TVyhSimulator = class
    private

     procedure OnTimer(Sender: TObject);

    public
     timer: TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

var
  JCSimulator : TJCSimulator;
  TratSimulator : TTratSimulator;
  VyhSimulator : TVyhSimulator;

implementation

uses GetSystems, TechnologieRCS, TJCDatabase, Block, BlockTrack, BlockDb, BlockSignal,
     BlockRailwayTrack, BlockTurnout;

////////////////////////////////////////////////////////////////////////////////
// simulator obsazovani useku v jizdni ceste
////////////////////////////////////////////////////////////////////////////////

constructor TJCSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 2000;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
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
 if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

 for JC in JCDb do
   if (JC.stav.RozpadBlok > -1) then
     Self.UpdateJC(JC);
end;

procedure TJCSimulator.UpdateJC(JC: TJC);
var Blk, Nav: TBlk;
    UsekSet: TBlkTrackSettings;
begin
 try
   if (JC.stav.RozpadBlok < 0) then Exit();

   if (((JC.stav.RozpadBlok = 1) or (JC.stav.RozpadBlok >= JC.data.Useky.Count)) and (JC.stav.RozpadRuseniBlok = -1)) then
    begin
     Blocks.GetBlkByID(JC.data.NavestidloBlok, Nav);
     Blocks.GetBlkByID((Nav as TBlkSignal).trackId, Blk);

     if ((Blk as TBlkTrack).occupied = TTrackState.occupied) then
      begin
       UsekSet := (Blk as TBlkTrack).GetSettings();
       RCSi.SetInputs(UsekSet.RCSAddrs, 0);
       Exit();
      end;
    end;//uvolnit usek pred navestidlem

   if (((JC.stav.RozpadBlok-JC.stav.RozpadRuseniBlok >= 2) or (JC.stav.RozpadBlok >= JC.data.Useky.Count)) and
       (JC.stav.RozpadRuseniBlok >= 0)) then
    begin
     if (JC.stav.RozpadRuseniBlok >= JC.data.Useky.Count) then Exit();

     // uvolnit RozpadRuseniBlok
     Blocks.GetBlkByID(JC.data.Useky[JC.stav.RozpadRuseniBlok], Blk);
     UsekSet := (Blk as TBlkTrack).GetSettings();
     RCSi.SetInputs(UsekSet.RCSAddrs, 0);
    end else begin
     // obsadit RozpadBlok
     if (JC.stav.RozpadBlok >= JC.data.Useky.Count) then Exit();

     Blocks.GetBlkByID(JC.data.Useky[JC.stav.RozpadBlok], Blk);
     UsekSet := (Blk as TBlkTrack).GetSettings();
     if (UsekSet.RCSAddrs.Count > 0) then
       RCSi.SetInput(UsekSet.RCSAddrs[0].board, UsekSet.RCSAddrs[0].port, 1);
    end;//else
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////
// simulator obsazovani trati
////////////////////////////////////////////////////////////////////////////////

constructor TTratSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 2000;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
end;

destructor TTratSimulator.Destroy();
begin
 if (Assigned(Self.timer)) then
   FreeAndNil(Self.timer);
 inherited Destroy();
end;

procedure TTratSimulator.OnTimer(Sender: TObject);
var Blk: TBlk;
begin
 if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

 for Blk in Blocks do
  begin
   if (Blk.typ <> btRailway) then continue;
   if (((Blk as TBlkRailway).BP) and ((Blk as TBlkRailway).occupied) and
       ((TBlkRailway(Blk).direction = TRailwayDirection.AtoB) or (TBlkRailway(Blk).direction = TRailwayDirection.BtoA))) then
     Self.UpdateTrat(Blk as TBlkRailway);
  end;
end;

procedure TTratSimulator.UpdateTrat(Trat: TBlkRailway);
var TU: TBlkRT;
    TratSet: TBlkRailwaySettings;
    i: Integer;
begin
 try
   TratSet := Trat.GetSettings();

   // mazani soupravy vzadu
   for i := 0 to TratSet.trackIds.Count-1 do
    begin
     Blocks.GetBlkByID(TratSet.trackIds[i], TBlk(TU));
     if ((TU.bpInBlk) and (TU.prevRT <> nil) and (TU.prevRT.occupied = TTrackState.occupied) and
         (TU.prevRT.train = TU.train)) then
      begin
       RCSi.SetInput(TBlkTrack(TU.prevRT).GetSettings().RCSAddrs[0], 0);
       Exit();
      end;
    end;//for i

   // predavani soupravy dopredu
   for i := 0 to TratSet.trackIds.Count-1 do
    begin
     Blocks.GetBlkByID(TratSet.trackIds[i], TBlk(TU));
     if ((TU.occupied = TTrackState.occupied) and (TU.bpInBlk) and (TU.nextRT <> nil) and
         (TU.nextRT.occupied = TTrackState.free) and
        ((TU.nextRT.signalCover = nil) or (TBlkSignal(TU.nextRT.signalCover).signal > ncStuj))) then
      begin
       RCSi.SetInput(TBlkTrack(TU.nextRT).GetSettings().RCSAddrs[0], 1);
       Exit();
      end;
    end;//for i
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////
// simulator staveni vyhybek
////////////////////////////////////////////////////////////////////////////////

constructor TVyhSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 500;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
end;

destructor TVyhSimulator.Destroy();
begin
 if (Assigned(Self.timer)) then
   FreeAndNil(Self.timer);
 inherited Destroy();
end;

procedure TVyhSimulator.OnTimer(Sender: TObject);
var blk: TBlk;
    vyh: TBlkTurnout;
begin
 try
   if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

   for blk in Blocks do
    begin
     if (blk.typ <> btTurnout) then continue;
     vyh := TBlkTurnout(blk);

     if (((vyh.movingPlus) or (vyh.movingMinus)) and (vyh.posDetection)) then
      begin
       // po 1 sekunde nastavime vstup aktualni polohy na 0
       if ((vyh.state.positionReal <> TTurnoutPosition.none) and (vyh.state.movingStart+EncodeTime(0, 0, 1, 0) < Now)) then
        begin
         if (vyh.movingPlus) then
          RCSi.SetInput(vyh.rcsInMinus, 0)
         else
          RCSi.SetInput(vyh.rcsInPlus, 0);
        end;//if koncova poloha

       // po 3 sekundach oznamime koncovou polohu
       if (vyh.state.movingStart+EncodeTime(0, 0, 3, 0) < Now) then
        begin
         if (vyh.movingMinus) then
          RCSi.SetInput(vyh.rcsInPlus, 1)
         else
          RCSi.SetInput(vyh.rcsInMinus, 1);
        end;//if koncova poloha
      end;
    end;
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  JCSimulator := TJCSimulator.Create();
  TratSimulator := TTratSimulator.Create();
  VyhSimulator := TVYhSimulator.Create();

finalization
  FreeAndNil(JCSimulator);
  FreeAndNil(TratSimulator);
  FreeAndNil(VyhSimulator);

end.
