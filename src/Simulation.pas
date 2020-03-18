unit Simulation;

interface

uses TechnologieJC, ExtCtrls, TBlokTrat, SysUtils;

type
  TJCSimulator = class
    private

     procedure OnTimer(Sender:TObject);
     procedure UpdateJC(JC:TJC);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TTratSimulator = class
    private

     procedure OnTimer(Sender:TObject);
     procedure UpdateTrat(Trat:TBlkTrat);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TVyhSimulator = class
    private

     procedure OnTimer(Sender:TObject);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

var
  JCSimulator : TJCSimulator;
  TratSimulator : TTratSimulator;
  VyhSimulator : TVyhSimulator;

implementation

uses GetSystems, TechnologieRCS, TJCDatabase, TBlok, TBlokUsek, TBloky, TBlokNav,
     TBlokTratUsek, TBlokVyhybka;

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

procedure TJCSimulator.OnTimer(Sender:TObject);
var JC:TJC;
begin
 if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

 for JC in JCDb do
   if (JC.stav.RozpadBlok > -1) then
     Self.UpdateJC(JC);
end;

procedure TJCSimulator.UpdateJC(JC:TJC);
var i:Integer;
    Blk, Nav:TBlk;
    UsekSet:TBlkUsekSettings;
begin
 try
   if (JC.stav.RozpadBlok < 0) then Exit();

   if (((JC.stav.RozpadBlok = 1) or (JC.stav.RozpadBlok >= JC.data.Useky.Count)) and (JC.stav.RozpadRuseniBlok = -1)) then
    begin
     Blky.GetBlkByID(JC.data.NavestidloBlok, Nav);
     Blky.GetBlkByID((Nav as TBlkNav).UsekID, Blk);

     if ((Blk as TBlkUsek).Stav.Stav = TUsekStav.obsazeno) then
      begin
       UsekSet := (Blk as TBlkUsek).GetSettings();
       for i := 0 to UsekSet.RCSAddrs.Count-1 do
        RCSi.SetInput(UsekSet.RCSAddrs[i].board, UsekSet.RCSAddrs[i].port, 0);
       Exit();
      end;
    end;//uvolnit usek pred navestidlem

   if (((JC.stav.RozpadBlok-JC.stav.RozpadRuseniBlok >= 2) or (JC.stav.RozpadBlok >= JC.data.Useky.Count)) and
       (JC.stav.RozpadRuseniBlok >= 0)) then
    begin
     if (JC.stav.RozpadRuseniBlok >= JC.data.Useky.Count) then Exit();

     // uvolnit RozpadRuseniBlok
     Blky.GetBlkByID(JC.data.Useky[JC.stav.RozpadRuseniBlok], Blk);
     UsekSet := (Blk as TBlkUsek).GetSettings();
     for i := 0 to UsekSet.RCSAddrs.Count-1 do
       RCSi.SetInput(UsekSet.RCSAddrs[i].board, UsekSet.RCSAddrs[i].port, 0);
    end else begin
     // obsadit RozpadBlok
     if (JC.stav.RozpadBlok >= JC.data.Useky.Count) then Exit();

     Blky.GetBlkByID(JC.data.Useky[JC.stav.RozpadBlok], Blk);
     UsekSet := (Blk as TBlkUsek).GetSettings();
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

procedure TTratSimulator.OnTimer(Sender:TObject);
var Blk:TBlk;
begin
 if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

 for Blk in Blky do
  begin
   if (Blk.typ <> _BLK_TRAT) then continue;
   if (((Blk as TBlkTrat).BP) and ((Blk as TBlkTrat).Obsazeno) and
       ((TBlkTrat(Blk).Smer = TTratSmer.AtoB) or (TBlkTrat(Blk).Smer = TTratSmer.BtoA))) then
     Self.UpdateTrat(Blk as TBlkTrat);
  end;
end;

procedure TTratSimulator.UpdateTrat(Trat:TBlkTrat);
var TU:TBlkTU;
    TratSet:TBlkTratSettings;
    i:Integer;
begin
 try
   TratSet := Trat.GetSettings();

   // mazani soupravy vzadu
   for i := 0 to TratSet.Useky.Count-1 do
    begin
     Blky.GetBlkByID(TratSet.Useky[i], TBlk(TU));
     if ((TU.bpInBlk) and (TU.prevTU <> nil) and (TU.prevTU.Obsazeno = TUsekStav.obsazeno) and
         (TU.prevTU.Souprava = TU.Souprava)) then
      begin
       RCSi.SetInput(TBlkUsek(TU.prevTU).GetSettings().RCSAddrs[0].board, TBlkUsek(TU.prevTU).GetSettings().RCSAddrs[0].port, 0);
       Exit();
      end;
    end;//for i

   // predavani soupravy dopredu
   for i := 0 to TratSet.Useky.Count-1 do
    begin
     Blky.GetBlkByID(TratSet.Useky[i], TBlk(TU));
     if ((TU.Obsazeno = TUsekStav.obsazeno) and (TU.bpInBlk) and (TU.nextTU <> nil) and
         (TU.nextTU.Obsazeno = TUsekStav.uvolneno) and
        ((TU.nextTU.navKryci = nil) or (TBlkNav(TU.nextTU.navKryci).Navest > 0))) then
      begin
       RCSi.SetInput(TBlkUsek(TU.nextTU).GetSettings().RCSAddrs[0].board, TBlkUsek(TU.nextTU).GetSettings().RCSAddrs[0].port, 1);
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

procedure TVyhSimulator.OnTimer(Sender:TObject);
var blk:TBlk;
    vyh:TBlkVyhybka;
begin
 try
   if ((not GetFunctions.GetSystemStart()) or (not RCSi.simulation)) then Exit;

   for blk in Blky do
    begin
     if (blk.typ <> _BLK_VYH) then continue;
     vyh := TBlkVyhybka(blk);

     if (((vyh.StaveniPlus) or (vyh.StaveniMinus)) and (vyh.detekcePolohy)) then
      begin
       // po 1 sekunde nastavime vstup aktualni polohy na 0
       if ((vyh.Stav.polohaReal <> TVyhPoloha.none) and (vyh.Stav.staveniStart+EncodeTime(0, 0, 1, 0) < Now)) then
        begin
         if (vyh.StaveniPlus) then
          RCSi.SetInput(vyh.GetSettings.RCSAddrs[1].board, vyh.GetSettings.RCSAddrs[1].port, 0)
         else
          RCSi.SetInput(vyh.GetSettings.RCSAddrs[0].board, vyh.GetSettings.RCSAddrs[0].port, 0);
        end;//if koncova poloha

       // po 3 sekundach oznamime koncovou polohu
       if (vyh.Stav.staveniStart+EncodeTime(0, 0, 3, 0) < Now) then
        begin
         if (vyh.StaveniPlus) then
          RCSi.SetInput(vyh.GetSettings.RCSAddrs[0].board, vyh.GetSettings.RCSAddrs[0].port, 1)
         else
          RCSi.SetInput(vyh.GetSettings.RCSAddrs[1].board, vyh.GetSettings.RCSAddrs[1].port, 1);
        end;//if koncova poloha
      end;
    end;
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  JCSimulator   := TJCSimulator.Create();
  TratSimulator := TTratSimulator.Create();
  VyhSimulator  := TVYhSimulator.Create();

finalization
  FreeAndNil(JCSimulator);
  FreeAndNil(TratSimulator);
  FreeAndNil(VyhSimulator);

end.
