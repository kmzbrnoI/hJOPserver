unit stanicniHlaseniHelper;

{
 Tato unit implementuje pomocne funkce pro prehravani stanicho hlaseni.
}

interface

uses Train, TOblRizeni, Generics.Collections, BlockTrack, BlockRailway;

type
  // jaka stanicni hlaseni prehrat
  TSHToPlay = record
    trat: TBlkRailway;
    stanicniKolej: TBlkTrack;
  end;

  { Mozne stavy:
     - trat = nil, stanicniKolej = nil: neprehravat hlaseni
     - trat = nil, stanicniKolej != nil: prehrat prijezd na kolej
     - trat != nil, stanicniKolej = nil: prehrat prujezd
     - trat != nil, stanicniKolej != nil: prehrat prujezd po koleji
  }

function CanPlayPrijezdSH(train: TTrain; OblR: TOR): TSHToPlay;
  // vraci jaka prijezdova stanicni hlaseni lze prehrat
  // tato funkce predpoklada, ze jsou spravne vypocitany predpovidani souprav
  // funkce overuje jen pritomnost na usecich, overeni dostupnosti fyzickeho
  // modulu SH ve stanici je potreba provest zvlast

implementation

uses BlockDb, Block, BlockRailwayTrack;

////////////////////////////////////////////////////////////////////////////////

function CanPlayPrijezdSH(train: TTrain; OblR: TOR): TSHToPlay;
var blksWithTrain: TList<TBlkTrack>;
    blk: TBlk;
    blkUsek: TBlkTrack;
    inTrat: TBlkRailway;
    inOR: Boolean;
begin
 blksWithTrain := TList<TBlkTrack>.Create();

 Result.trat := nil;
 Result.stanicniKolej := nil;

 try
   inTrat := nil;

   // ziskame seznam bloku na kterych je souprava predpovidana v dane stanici
   // ziskame trat, ve ktere se aktualne souprava nachazi
   for blk in Blocks do
    begin
     inOR := Blk.stations.Contains(OblR);

     // trate z aktualni stanice kontrolujeme cele
     if ((not inOR) and (blk.typ = btRT) and (TBlkTrack(blk).trainPredict = train) and
         (TBlkRT(blk).railway <> nil) and
         (((TBlkRailway(TBlkRT(blk).railway)).linkerA.stations[0] = OblR) or
          ((TBlkRailway(TBlkRT(blk).railway)).linkerB.stations[0] = OblR))) then
       blksWithTrain.Add(TBlkTrack(blk));

     if (not inOR) then continue;

     if (((blk.typ = btTrack) or (blk.typ = btRT)) and
         (TBlkTrack(blk).trainPredict = train)) then
       blksWithTrain.Add(TBlkTrack(blk));

     if ((blk.typ = btRT) and (TBlkTrack(blk).train = train)) then
       if (TBlkRT(blk).railway <> nil) then
         inTrat := TBlkRailway(TBlkRT(blk).railway);
    end;

   // zjistime, na ktere stanicni a na ktere tratove koleje je souprava predpovidana
   Result.trat := nil;
   for blkUsek in blksWithTrain do
    begin
     // souprava je predpovidana do jine trati nez ve ktere je -> prujezd
     if ((blkUsek.typ = btRT) and (TBlkRT(blkUsek).railway <> inTrat)
         and (TBlkRT(blkUsek).railway <> nil)) then
       Result.trat := TBlkRailway(TBlkRT(blkUsek).railway);

     // souprava je predpovidana na stanicni kolej -> vybrat tu s nejkratsim nazvem
     if ((blkUsek.spnl.trackName <> '') and ((Result.stanicniKolej = nil) or
          (Length(blkUsek.spnl.trackName) < Length(Result.stanicniKolej.spnl.trackName)))) then
       Result.stanicniKolej := blkUsek;
    end;

   // odjezd z koleje neni povazovan za prujezd
   if ((inTrat = nil) and (Result.trat <> nil) and (Result.stanicniKolej = nil)) then
     Result.trat := nil;

 finally
   blksWithTrain.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
