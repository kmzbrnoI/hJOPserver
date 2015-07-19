unit TBlokTratUsek;

// Definice a obsluha technologickeho bloku Tratovy usek
// Tratovy usek dedi z Useku
// TU = Tratovy usek

{
Jak to funguje:
 - Trat je rozdelena na sekce, kazda sekce obsahuje 1..n useku.
 - Kazda sekce je v jednom smeru kryta prave jednim navestidlem,
   sekce tedy nabyva vyznamu useku autobloku.
 - Pokud mam napriklad trat bez autobloku o 4 usecich, jedna se o jednu
   sekci se 4-mi useky.
 - Sekce jsou smerove zavisle.
 - Sekce jakozto takova neni reprezntovana zadnou specialni tridou,
   je reprezentovana TU, ktery je kryty navestidlem. Takovy TU se nazyva
   "Section Master"
 - Prvni TU trati v danem smeru je pro tyto ucely vzdy povazovan za Section
   Master (tedy je povazovan za kryty navestidlem).
 - Kazdy TU ma odkaz na sveho Section Master.
 - Kady Section Master ma odkaz na vsechny TU, ktere spadaji do jeho sekce.
 - Nastavovani kryciho navestidla sekce je zodpovednost pouze Section Master.
 - Kazdy TU ma odkaz na vedlejsi TU (sTU, lTU), lTU vzdy bliz zacatku trati.

 - Nastavovani vazeb TU probiha pri Enable() trati, ktera vsechny zavisloti
   nastavi. Behem provozu neni mozne menit useky v trati.
 - Pri Disable() dojde ke zruseni vsech vazeb a vymazani navaznosti TU.
 - prevTU a nextTU jsou zavisle podle smeru trati
 - navPrev, navNext
}

interface

uses TBlokUsek, Classes, TBlok, IniFiles, SysUtils, IdContext, RPConst,
      Generics.Collections;

type
 TBlkTUZastavka = record  // zastavka na useku
  enabled:boolean;          // existuje v useku zastavka?
  IR_lichy:Integer;         // odkaz na zastavovaci IR v lichem smeru
  IR_sudy:Integer;          // odkaz na zastavovaci IR v sudem smeru
  soupravy:TStrings;        // typy souprav, pro ktere je zastavka
  max_delka:Integer;        // maximalni delka soupravy pro zastaveni v zastavce
  delay:TTime;              // cas cekani v zastavce
 end;

 TBlkTUSettings = record
   zastavka:TBlkTUZastavka;
   navLid,navSid:Integer;
   rychlost:Integer;
 end;

 TBlkTUStav = record      // stav tratoveho useku
  inTrat:Integer;           // tady je ulozeno id bloku trati, v jake se blok nachazi; pokud se nenachazi v trati -> -1

  zast_stopped:boolean;     // jakmile zastavim soupravu v zastavce, nastavim sem true; pokud souprava jede, je zde false
  zast_run_time:TDateTime;  // tady je ulozen cas, kdy se ma souprava ze zastavky rozjet
  zast_rych:Integer;        // tady si pamatuji, jakou rychlost mela souprava puvodne (mela by to byt tratova, toto je tu pro rozsireni zastavek nejen do trati)
  zast_enabled:boolean;     // zastavku lze z panelu zapnout a vypnout (v zakladnim stavu je zapla)
  zast_passed:boolean;      // atdy je ulozeno true, pokud souprava zastavku jiz projela

  bpInBlk:boolean;
 end;


 // technologicky blok Tratoveho useku
 TBlkTU = class(TBlkUsek)
  private const
   _def_tu_stav:TBlkTUStav = (
    inTrat: -1;
    zast_stopped : false;
    zast_enabled : true;
   );

   _def_tu_zastavka:TBlkTUZastavka = (
    enabled : false;
    IR_lichy : -1;
    IR_sudy : -1;
    soupravy : nil;
    max_delka : 0;
   );

  private
   TUSettings:TBlkTUSettings;
   fTUStav:TBlkTUStav;

   fZastIRLichy, fZastIRSudy,
   fNavPrev, fNavNext, fTrat : TBlk;

    function GetZastIRLichy():TBlk;
    function GetZastIRSudy():TBlk;

    procedure ZastUpdate();
    procedure ZastRunTrain();
    procedure ZastStopTrain();

    procedure MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
    procedure MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure SetUsekSpr(spr:Integer); override;
    function GetUsekSpr:Integer;

    procedure UpdateBP();

    function GetTrat():TBlk;
    function GetNavPrev():TBlk;
    function GetNavNext():TBlk;
    function GetTratReady():boolean;
    function GetPrevTU():TBlkTU;
    function GetNextTU():TBlkTU;

    procedure PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);

    property zastIRlichy:TBlk read GetZastIRLichy;
    property zastIRsudy:TBlk read GetZastIRSudy;

    property Trat:TBlk read GetTrat;
    property navPrev:TBlk read GetNavPrev;
    property navNext:TBlk read GetNavNext;
    property tratReady:boolean read GetTratReady;

    property prevTU:TBlkTU read GetPrevTU;
    property nextTU:TBlkTU read GetNextTU;

  public
   lTU, sTU, sectMaster: TBlkTU;
   sectUseky:TList<TBlkTU>;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    function GetSettings():TBlkTUSettings; overload;
    procedure SetSettings(data:TBlkTUSettings); overload;

    procedure Disable(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;

    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights); override;
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); override;

    property TUStav:TBlkTUStav read fTUStav;
    property Souprava:Integer read GetUsekSpr write SetUsekSpr;
    property InTrat:Integer read fTUStav.InTrat write fTUStav.InTrat;
    property bpInBlk:boolean read fTUStav.bpInBlk write fTUStav.bpInBlk;

 end;//TBlkUsek


implementation

uses SprDb, TBloky, TBlokIR, TCPServerOR, TOblRizeni, TBlokTrat, TBlokSCom;

// format dat zastavky v souboru bloku: zast=IR_lichy|IR_sudy|max_delka_soupravy|delay_time|spr1;spr2;...
//  pokud je zast prazdny string, zastavka je disabled

////////////////////////////////////////////////////////////////////////////////

constructor TBlkTU.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_TU;
 Self.fTUStav := _def_tu_stav;

 Self.fZastIRLichy := nil;
 Self.fZastIRSUdy  := nil;
 Self.fTrat        := nil;
 Self.fNavPrev     := nil;
 Self.fNavNext     := nil;
 Self.sectMaster   := nil;
 Self.sectUseky    := TList<TBlkTU>.Create();
 Self.bpInBlk      := false;
end;//ctor

destructor TBlkTU.Destroy();
begin
 Self.sectUseky.Free();
 Self.TUSettings.Zastavka.soupravy.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TUSettings.navLid := ini_tech.ReadInteger(section, 'navL', -1);
 Self.TUSettings.navSid := ini_tech.ReadInteger(section, 'navS', -1);

 Self.TUSettings.rychlost := ini_tech.ReadInteger(section, 'rychlost', -1);

 str := TStringList.Create();
 ExtractStrings(['|'],[], PChar(ini_tech.ReadString(section, 'zast', '')), str);

 // nacitani zastavky
 if (Assigned(Self.TUSettings.Zastavka.soupravy)) then Self.TUSettings.Zastavka.soupravy.Free();
 Self.TUSettings.Zastavka := _def_tu_zastavka;
 Self.TUSettings.Zastavka.soupravy := TStringList.Create();

 if (str.Count > 0) then
  begin
   try
    Self.TUsettings.Zastavka.enabled   := true;
    Self.TUsettings.Zastavka.IR_lichy  := StrToInt(str[0]);
    Self.TUsettings.Zastavka.IR_sudy   := StrToInt(str[1]);
    Self.TUsettings.Zastavka.max_delka := StrToInt(str[2]);
    Self.TUsettings.Zastavka.delay     := StrToTime(str[3]);
    Self.TUsettings.Zastavka.soupravy.Clear();
    ExtractStrings([';'],[],PChar(str[4]), Self.TUsettings.Zastavka.soupravy);
   except
    Self.TUsettings.Zastavka := _def_tu_zastavka;
   end;
  end;

 str.Free();
end;//procedure

procedure TBlkTU.SaveData(ini_tech:TMemIniFile;const section:string);
var str:string;
    i:Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'navL', Self.TUSettings.navLid);
 ini_tech.WriteInteger(section, 'navS', Self.TUSettings.navSid);

 ini_tech.WriteInteger(section, 'rychlost', Self.TUSettings.rychlost);

 // ukladani zastavky
 if (Self.TUsettings.Zastavka.enabled) then
  begin
   with (Self.TUsettings.Zastavka) do
    begin
     str := IntToStr(IR_lichy) + '|' + IntToStr(IR_sudy) + '|' + IntToStr(max_delka) + '|' + TimeToStr(delay) + '|';
     for i := 0 to soupravy.Count-1 do
      str := str + soupravy[i] + ';';
    end;

   ini_tech.WriteString(section, 'zast', str);
  end else begin
   ini_tech.WriteString(section, 'zast', '');
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetSettings():TBlkTUSettings;
begin
 Result := Self.TUSettings;
end;//function

procedure TBlkTU.SetSettings(data:TBlkTUSettings);
begin
 if (Self.TUSettings.Zastavka.soupravy <> data.Zastavka.soupravy) then
  Self.TUSettings.Zastavka.soupravy.Free();

 Self.TUSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace stavu bloku

procedure TBlkTU.Update();
begin
 inherited;

 if ((Self.InTrat > -1) and (Self.Stav.Stav = TUsekStav.obsazeno) and (Self.Souprava > -1) and (Self.TUSettings.Zastavka.enabled)) then
   Self.ZastUpdate();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.Change(now:boolean = false);
begin
 inherited;
 Self.UpdateBP();
 if (Self.Trat <> nil) then Self.Trat.Change(); 
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace stavu zastavky

procedure TBlkTU.ZastUpdate();
var i:Integer;
    found:boolean;
begin
 if (not Self.TUStav.zast_stopped) then
  begin
   // cekam na obsazeni IR
   if ((not Self.TUStav.zast_enabled) or (Self.TUStav.zast_passed) or
      (Soupravy.soupravy[Self.Souprava].delka > Self.TUSettings.Zastavka.max_delka) or (Soupravy.soupravy[Self.Souprava].front <> self)) then Exit();

   // kontrola typu soupravy:
   found := false;
   for i := 0 to Self.TUSettings.Zastavka.soupravy.Count-1 do
    begin
     if (Self.TUSettings.Zastavka.soupravy[i] = Soupravy.soupravy[Self.Souprava].typ) then
      begin
       found := true;
       break;
      end;
    end;

   if (not found) then Exit();

   case (Soupravy.soupravy[Self.Souprava].smer) of
    THVSTanoviste.lichy : if ((Assigned(Self.zastIRlichy)) and ((Self.zastIRlichy as TBlkIR).Stav = TIRStav.obsazeno)) then
                              Self.ZastStopTrain();
    THVSTanoviste.sudy  : if ((Assigned(Self.zastIRsudy)) and ((Self.zastIRsudy as TBlkIR).Stav = TIRStav.obsazeno)) then
                              Self.ZastStopTrain();
   end;//case
  end else begin
   // osetreni rozjeti vlaku z nejakeho pochybneho duvodu
   //  pokud se souprava rozjede, koncim zastavku
   if (Soupravy.soupravy[Self.Souprava].rychlost <> 0) then
    begin
     Self.fTUStav.zast_stopped := false;
     Self.Change();  // change je dulezite volat kvuli menu
    end;

   // cekam na timeout na rozjeti vlaku
   if (Now > Self.TUStav.zast_run_time) then
    Self.ZastRunTrain();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// zastavky:

function TBlkTU.GetZastIRLichy():TBlk;
begin
 if (((Self.fZastIRLichy = nil) and (Self.TUSettings.Zastavka.IR_lichy <> -1)) or ((Self.fZastIRLichy <> nil) and (Self.fZastIRLichy.GetGlobalSettings.id <> Self.TUSettings.Zastavka.IR_lichy))) then
   Blky.GetBlkByID(Self.TUSettings.Zastavka.IR_lichy, Self.fZastIRLichy);
 Result := Self.fZastIRLichy;
end;//function

function TBlkTU.GetZastIRSudy():TBlk;
begin
 if (((Self.fZastIRSudy = nil) and (Self.TUSettings.Zastavka.IR_sudy <> -1)) or ((Self.fZastIRSudy <> nil) and (Self.fZastIRSudy.GetGlobalSettings.id <> Self.TUSettings.Zastavka.IR_sudy))) then
   Blky.GetBlkByID(Self.TUSettings.Zastavka.IR_sudy, Self.fZastIRSudy);
 Result := Self.fZastIRSudy;
end;//function


////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.ZastStopTrain();
begin
 Self.fTUStav.zast_stopped  := true;
 Self.fTUStav.zast_run_time := Now+Self.TUSettings.Zastavka.delay;

 try
   Self.fTUStav.zast_rych := Soupravy.soupravy[Self.Souprava].rychlost;
   Soupravy.soupravy[Self.Souprava].rychlost := 0;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;//procedure

procedure TBlkTU.ZastRunTrain();
begin
 Self.fTUStav.zast_stopped := false;
 Self.fTUStav.zast_passed  := true;

 try
   Soupravy.soupravy[Self.Souprava].rychlost := Self.TUStav.zast_rych;
 except

 end;

 Self.Change();     // change je dulezite volat kvuli menu
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;

 // zastavka
 if ((Self.TUSettings.Zastavka.enabled) and (Self.InTrat > -1)) then
  begin
   Result := Result + '-,';
   if (not Self.TUStav.zast_stopped) then
    begin
     // pokud neni v zastavce zastavena souprava, lze zastavku vypinat a zapinat
     case (Self.TUStav.zast_enabled) of
      false : Result := Result + 'ZAST>,';
      true  : Result := Result + 'ZAST<,';
     end;//case
    end else begin
     // pokud v zastavce osuprava stoji, lze ji rozjet
     Result := Result + 'JEÏ vlak,';
    end;
  end;

 if (Self.Zaver = TJCType.nouz) then
   Result := Result + '!RBP,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 case (Button) of
  right,F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  left    : if (not Self.MenuKCClick(SenderPnl, SenderOR)) then
              if (not Self.PresunLok(SenderPnl, SenderOR)) then
                ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  middle  : Self.MenuVBClick(SenderPnl, SenderOR);
  F3: Self.ShowPanelSpr(SenderPnl, SenderOR, rights);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.SetUsekSpr(spr:Integer);
var old_spr:Integer;
    trat:TBlkTrat;
begin
 if (spr = Self.Souprava) then Exit();
 old_spr := Self.Souprava;
 inherited;

 if (spr = -1) then
  begin
   Self.fTUStav.zast_stopped := false;
   Self.fTUStav.zast_passed  := false;

   // souprava uvolnena z useku, mozna bude nutne ji uvolnit z cele trati
   if ((Self.Trat <> nil) and (not TBlkTrat(Self.Trat).IsSprInAnyTU(old_spr))) then
    begin
     trat := TBlkTrat(Self.Trat);

     // souprava uz neni v trati -> uvolnit
     TBlkTrat(Self.Trat).RemoveSpr(old_spr);

     // zrusime potencialni nouzovy zaver
     Self.bpInBlk := false;
     Self.Zaver   := TJCType.no;

     // pokud je trat uplne volna, zrusime blokovou podminku
     if (not trat.Obsazeno) then trat.BP := false;
     trat.Change();
    end;
  end;
end;

function TBlkTU.GetUsekSpr:Integer;
begin
 Result := Self.Stav.Spr;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.MenuZastClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
begin
 if (not Self.TUStav.zast_stopped) then
   Self.fTUStav.zast_enabled := new_state;
end;//procedure

procedure TBlkTU.MenuJEDLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.TUStav.zast_stopped) then
   Self.ZastRunTrain();
end;//procedure

procedure TBlkTU.MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TOR, 'Zrušení poruchy blokové podmínky', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkTU.PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);
var old_spr:Integer;
begin
 if (success) then
  begin
   Self.bpInBlk := false;
   Self.Zaver   := TJCType.no;
   if (Self.Souprava > -1) then
    begin
     old_spr := Self.Souprava;
     Self.Souprava := -1;
     if ((Self.Trat <> nil) and (not TBlkTrat(Self.Trat).IsSprInAnyTU(old_spr))) then
        TBlkTrat(Self.Trat).RemoveSpr(old_spr);
    end;
   if (Self.Trat <> nil) then Self.Trat.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 if (item = 'JEÏ vlak')   then Self.MenuJEDLokClick(SenderPnl, SenderOR)
 else if (item = 'ZAST>') then Self.MenuZastClick(SenderPnl, SenderOR, true)
 else if (item = 'ZAST<') then Self.MenuZastClick(SenderPnl, SenderOR, false)
 else if (item = 'RBP') then Self.MenuRBPClick(SenderPnl, SenderOR)
 else inherited;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTrat():TBlk;
begin
 if (((Self.fTrat = nil) and (Self.TUStav.inTrat <> -1)) or ((Self.fTrat <> nil) and (Self.fTrat.GetGlobalSettings.id <> Self.TUStav.inTrat))) then
   Blky.GetBlkByID(Self.TUStav.inTrat, Self.fTrat);
 Result := Self.fTrat;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetNavPrev():TBlk;
var navPrevID:Integer;
begin
 if ((Self.Trat = nil) or ((TBlkTrat(Self.Trat).Smer <> TTratSmer.AtoB) and (TBlkTrat(Self.Trat).Smer <> TTratSmer.BtoA))) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : navPrevID := Self.TUSettings.navLid;
   TTratSmer.BtoA : navPrevID := Self.TUSettings.navSid;
 else
  navPrevID := -1;
 end;

 if (((Self.fNavPrev = nil) and (navPrevID <> -1)) or ((Self.fNavPrev <> nil) and (Self.fNavPrev.GetGlobalSettings.id <> navPrevID))) then
   Blky.GetBlkByID(navPrevID, Self.fNavPrev);
 Result := Self.fNavPrev;
end;

function TBlkTU.GetNavNext():TBlk;
var navNextID:Integer;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : navNextID := Self.TUSettings.navSid;
   TTratSmer.BtoA : navNextID := Self.TUSettings.navLid;
 else
  navNextID := -1;
 end;

 if (((Self.fNavNext = nil) and (navNextID <> -1)) or ((Self.fNavNext <> nil) and (Self.fNavNext.GetGlobalSettings.id <> navNextID))) then
   Blky.GetBlkByID(navNextID, Self.fNavNext);
 Result := Self.fNavNext;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetTratReady():boolean;
begin
 Result := ((Self.Trat <> nil) and ((TBlkTrat(Self.Trat).Smer = TTratSmer.AtoB) or (TBlkTrat(Self.Trat).Smer = TTratSmer.BtoA)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTU.GetPrevTU():TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : Result := Self.lTU;
   TTratSmer.BtoA : Result := Self.sTU;
 else
  Result := nil;
 end;
end;

function TBlkTU.GetNextTU():TBlkTU;
begin
 if (not Self.tratReady) then Exit(nil);

 case (TBlkTrat(Self.Trat).Smer) of
   TTratSmer.AtoB : Result := Self.sTU;
   TTratSmer.BtoA : Result := Self.lTU;
 else
  Result := nil;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.Disable();
begin
 inherited;

 Self.lTU        := nil;
 Self.sTU        := nil;
 Self.sectMaster := nil;
 Self.sectUseky.Clear();
 Self.bpInBlk    := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTU.UpdateBP();
begin
 if ((not Self.tratReady) or (not TBlkTrat(Self.Trat).BP)) then Exit();

 if ((Self.prevTU = nil) and (Self.Obsazeno = TUsekStav.obsazeno)) then
   // nastala aktivace blokove podminky prvniho bloku trati
   Self.bpInBlk := true;

 // predavani soupravy z predchoziho TU do meho TU
 if ((Self.prevTU <> nil) and (Self.Obsazeno = TUsekStav.obsazeno) and
     (Self.prevTU.Obsazeno = TUsekStav.obsazeno)) then
  begin
   // nastala aktivace blokove podminky
   Self.bpInBlk := true;

   if ((Self.Souprava = -1) and (Self.prevTU.Souprava > -1)) then
    begin
     // mezi useky je potreba predat soupravu
     Self.Souprava := Self.prevTU.Souprava;
     Self.zpomalovani_ready := true;
     Soupravy.soupravy[Self.Souprava].front := Self;

     if (Self.nextTU = nil) then
      begin
       // souprava vstoupila do posledniho bloku trati
       // zmena stanic soupravy a hnacich vozidel v ni
       TBlkTrat(Self.Trat).SprChangeOR(Self.Souprava);

       // je nutne zmenit smer soupravy?
       if (((Assigned(TBlkTrat(Self.Trat).navLichy)) and (Assigned(TBlkTrat(Self.Trat).navSudy))) and
           (TBlkSCom(TBlkTrat(Self.Trat).navLichy).Smer = TBlkSCom(TBlkTrat(Self.Trat).navSudy).Smer)) then
        begin
         // navestidla na koncich trati jsou ve stejnem smeru -> zmenit smer soupravy, hnacich vozidel v ni a sipek
         Soupravy.soupravy[Self.Souprava].ChangeSmer();
        end;
      end;
    end;//if predavam soupravu
  end;

 // uvolnovani soupravy z TU (pokud je jiz predana do dalsiho TU)
 if ((Self.bpInBlk) and (Self.nextTU <> nil) and (Self.nextTU.Souprava = Self.Souprava) and
     (Self.Obsazeno = TusekStav.Uvolneno) and (Self.nextTU.Obsazeno = TUsekStav.obsazeno)) then
  begin
   Self.bpInBlk  := false;
   Self.Souprava := -1;
   if (not TBlkTrat(Self.Trat).Obsazeno) then TBlkTrat(Self.Trat).BP := false;   
  end;

 // kontrola poruchy blokove podminky
 if ((Self.bpInBlk)) then
  begin
   if ((Self.Obsazeno = TUsekStav.uvolneno) and (Self.Zaver = TJCType.no)) then
     Self.Zaver := TJCType.nouz;
   if ((Self.Obsazeno = TUsekStav.obsazeno) and (Self.Zaver = TJCType.nouz)) then
     Self.Zaver := TJCType.no;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

end.//unit
