unit stanicniHlaseniHelper;

{
 Tato unit implementuje pomocne funkce pro prehravani stanicho hlaseni.
}

interface

uses Souprava, TOblRizeni, Generics.Collections, TBlokUsek, TBlokTrat;

type
  // jaka stanicni hlaseni prehrat
  TSHToPlay = record
    trat:TBlkTrat;
    stanicniKolej:TBlkUsek;
  end;

  { Mozne stavy:
     - trat = nil, stanicniKolej = nil: neprehravat hlaseni
     - trat = nil, stanicniKolej != nil: prehrat prijezd na kolej
     - trat != nil, stanicniKolej = nil: prehrat prujezd
     - trat != nil, stanicniKolej != nil: prehrat prujezd po koleji
  }

function CanPlayPrijezdSH(spr:TSouprava; OblR:TOR):TSHToPlay;
  // vraci jaka prijezdova stanicni hlaseni lze prehrat
  // tato funkce predpoklada, ze jsou spravne vypocitany predpovidani souprav
  // funkce overuje jen pritomnost na usecich, overeni dostupnosti fyzickeho
  // modulu SH ve stanici je potreba provest zvlast

implementation

uses TBloky, TBlok, TBlokTratUsek;

////////////////////////////////////////////////////////////////////////////////

function CanPlayPrijezdSH(spr:TSouprava; OblR:TOR):TSHToPlay;
var blksWithSpr:TList<TBlkUsek>;
    i, j:Integer;
    blk:TBlk;
    blkUsek:TBlkUsek;
    inTrat:TBlkTrat;
    inOR:boolean;
begin
 blksWithSpr := TList<TBlkUsek>.Create();

 Result.trat := nil;
 Result.stanicniKolej := nil;

 try
   inTrat := nil;

   // ziskame seznam bloku na kterych je souprava predpovidana v dane stanici
   // ziskame trat, ve ktere se aktualne souprava nachazi
   for i := 0 to Blky.Cnt-1 do
    begin
     Blky.GetBlkByIndex(i, blk);

     inOR := false;
     for j := 0 to Blk.OblsRizeni.Cnt-1 do
      begin
       if (Blk.OblsRizeni.ORs[j] = OblR) then
        begin
         inOR := true;
         break;
        end;
      end;

     // trate z aktualni stanice kontrolujeme cele
     if ((not inOR) and (blk.typ = _BLK_TU) and (TBlkUsek(blk).SprPredict = spr.index) and
         (TBlkTU(blk).Trat <> nil) and
         (((TBlkTrat(TBlkTU(blk).Trat)).uvazkaA.OblsRizeni.ORs[0] = OblR) or
          ((TBlkTrat(TBlkTU(blk).Trat)).uvazkaB.OblsRizeni.ORs[0] = OblR))) then
       blksWithSpr.Add(TBlkUsek(blk));

     if (not inOR) then continue;

     if (((blk.typ = _BLK_USEK) or (blk.typ = _BLK_TU)) and
         (TBlkUsek(blk).SprPredict = spr.index)) then
       blksWithSpr.Add(TBlkUsek(blk));

     if ((blk.typ = _BLK_TU) and (TBlkUsek(blk).Souprava = spr.index)) then
       if (TBlkTU(blk).Trat <> nil) then
         inTrat := TBlkTrat(TBlkTU(blk).Trat);
    end;

   // zjistime, na ktere stanicni a na ktere tratove koleje je souprava predpovidana
   Result.trat := nil;
   for blkUsek in blksWithSpr do
    begin
     // souprava je predpovidana do jine trati nez ve ktere je -> prujezd
     if ((blkUsek.typ = _BLK_TU) and (TBlkTU(blkUsek).Trat <> inTrat)
         and (TBlkTU(blkUsek).Trat <> nil)) then
       Result.trat := TBlkTrat(TBlkTU(blkUsek).Trat);

     // souprava je predpovidana na stanicni kolej -> vybrat tu s nejkratsim nazvem
     if ((blkUsek.Stav.cislo_koleje <> '') and ((Result.stanicniKolej = nil) or
          (Length(blkUsek.Stav.cislo_koleje) < Length(Result.stanicniKolej.Stav.cislo_koleje)))) then
       Result.stanicniKolej := blkUsek;
    end;

   // odjezd z koleje neni povazovan za prujezd
   if ((inTrat = nil) and (Result.trat <> nil) and (Result.stanicniKolej = nil)) then
     Result.trat := nil;

 finally
   blksWithSpr.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
