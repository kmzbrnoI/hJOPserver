unit announcementHelper;

{ This unit implements helper function for station announcement. }

interface

uses Train, Area, Generics.Collections, BlockTrack, BlockRailway;

type
  //jaka stanicni hlaseni prehrat
  TAnnToPlay = record
    railway: TBlkRailway;
    stationTrack: TBlkTrack;
  end;

  { Mozne stavy:
    - railway = nil, stationTrack = nil: neprehravat hlaseni
    - railway = nil, stationTrack != nil: prehrat prijezd na kolej
    - railway != nil, stationTrack = nil: prehrat prujezd
    - railway != nil, stationTrack != nil: prehrat prujezd po koleji
  }

function CanPlayArrival(Train: TTrain; Area: TArea): TAnnToPlay;
//vraci jaka prijezdova stanicni hlaseni lze prehrat
//tato funkce predpoklada, ze jsou spravne vypocitany predpovidane vlaky
//funkce overuje jen pritomnost na usecich, overeni dostupnosti fyzickeho
//modulu SH ve stanici je potreba provest zvlast

implementation

uses BlockDb, Block, BlockRailwayTrack;

/// /////////////////////////////////////////////////////////////////////////////

function CanPlayArrival(Train: TTrain; Area: TArea): TAnnToPlay;
var blksWithTrain: TList<TBlkTrack>;
  blk: TBlk;
  blkTrack: TBlkTrack;
  inRailway: TBlkRailway;
  inStation: Boolean;
begin
  blksWithTrain := TList<TBlkTrack>.Create();

  Result.railway := nil;
  Result.stationTrack := nil;

  try
    inRailway := nil;

    //ziskame seznam bloku na kterych je vlak predpovidany v dane stanici
    //ziskame trat, ve ktere se aktualne vlak nachazi
    for blk in Blocks do
    begin
      inStation := blk.areas.Contains(Area);

      //trate z aktualni dopravny kontrolujeme cele
      if ((not inStation) and (blk.typ = btRT) and (TBlkTrack(blk).trainPredict = Train) and
        (TBlkRT(blk).railway <> nil) and (((TBlkRailway(TBlkRT(blk).railway)).linkerA.areas[0] = Area) or
        ((TBlkRailway(TBlkRT(blk).railway)).linkerB.areas[0] = Area))) then
        blksWithTrain.Add(TBlkTrack(blk));

      if (not inStation) then
        continue;

      if (((blk.typ = btTrack) or (blk.typ = btRT)) and (TBlkTrack(blk).trainPredict = Train)) then
        blksWithTrain.Add(TBlkTrack(blk));

      if ((blk.typ = btRT) and (TBlkTrack(blk).Train = Train)) then
        if (TBlkRT(blk).railway <> nil) then
          inRailway := TBlkRailway(TBlkRT(blk).railway);
    end;

    //zjistime, na ktere stanicni a na ktere tratove koleje je vlak predpovidany
    Result.railway := nil;
    for blkTrack in blksWithTrain do
    begin
      // vlak je predpovidany do jine trati nez ve ktere je -> prujezd
      if ((blkTrack.typ = btRT) and (TBlkRT(blkTrack).railway <> inRailway) and (TBlkRT(blkTrack).railway <> nil)) then
        Result.railway := TBlkRailway(TBlkRT(blkTrack).railway);

      // vlak je predpovidan na stanicni kolej -> vybrat tu s nejkratsim nazvem
      if ((blkTrack.spnl.trackName <> '') and ((Result.stationTrack = nil) or
        (Length(blkTrack.spnl.trackName) < Length(Result.stationTrack.spnl.trackName)))) then
        Result.stationTrack := blkTrack;
    end;

    //odjezd z koleje neni povazovan za prujezd
    if ((inRailway = nil) and (Result.railway <> nil) and (Result.stationTrack = nil)) then
      Result.railway := nil;

  finally
    blksWithTrain.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
