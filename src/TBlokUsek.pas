unit TBlokUsek;

//definice a obsluha technologickeho bloku Usek

interface

uses IniFiles, TBlok, TechnologieJC, Menus, TOblsRizeni, SysUtils, Classes,
     RPConst, IdContext, Generics.Collections;

type
 TUsekStav  = (disabled = -5, none = -1, uvolneno = 0, obsazeno = 1);

 //technologicka nastaveni useku (delka, MTB, ...)
 TBlkUsekSettings = record
  MTBAddrs:TMTBAddrs;
  Lenght:double;          //delka useku v metrech
  SmcUsek:boolean;        //specialni pripad: usek ve smycce
  Zesil:Integer;          //index v poli zesilovacu
 end;

 TUsekStavAr = array[0..3] of TUsekStav;

 //aktualni stav useku (obsazeno, ...)
 TBlkUsekStav = record
  Stav,StavOld:TUsekStav;   // hlavnis tav useku a jeho predchozi hodnota (pouzivano v Update())
  StavAr:TUsekStavAr;       // obsazenost jednotlivych casti useku
  Zaver:TJCType;            // zaver na bloku
  NUZ:boolean;              // nouzove uvolneni zaveru
  KonecJC:TJCType;          // jaka jizdni cesta (vlakova, posunova, nouzova) konci na tomto bloku - vyuzivano pro zobrazeni
  Stit,Vyl:string;          // stitek a vyluka
  SComJCRef:TBlk;           // zde je ulozeno, podle jakeho navestidla se odjizdi z tohoto bloku; pokud je postabeno vice navetidel, zde je ulozeno posledni, na kterem probehla volba
  Spr:Integer;              // default -1, jinak array index
  SprPredict:Integer;       // souprava, ktera je na tomto bloku predpovidana
  zkrat:boolean;            // zkrat zesilovace
  napajeni:boolean;         // napajeni zesilovace
  redukuji:TReduction;      // zde si ukladam, koho redukuji; ukladaji se id; jakmile se z meho bloku uvolni zaver, vse odredukuji
  stanicni_kolej:boolean;   // pokud je blok stanicni koleji, je zde true, jinak false
  vlakPresun:boolean;       // pokud je vlak na teto koleji oznacen pro presun do jine koelje, je zde true

  zpomalovani_ready:boolean;          // pri predani soupravy do tohoto useku z trati, ci z jizdni cesty, je tento flag nastaven na true
                                      // to znamena, ze je souprava pripravena ke zpomalovani; navetidlo umozni zpomaleni jen, pokud je tento flag na true
                                      // po zpomaleni si navestidlo flag zrusi

  spr_vypadek:boolean;      // ztrata soupravy na useku se pozna tak, ze blok po urcity cas obsahuje soupravu, ale neni obsazen
  spr_vypadek_time:Integer; //    to je nutne pro predavani souprav mezi bloky v ramci JC (usek se uvolni, ale souprava se jeste nestihne predat
                            // pro reseni timeoutu jsou tyto 2 promenne
 end;

 TBlkUsek = class(TBlk)
  const
   //defaultni stav
   _def_usek_stav:TBlkUsekStav = (
    Stav : disabled;
    StavOld : disabled;
    Zaver : no;
    NUZ : false;
    KonecJC : no;
    Stit : '';
    Vyl : '';
    SComJCRef : nil;
    Spr : -1;
    SprPredict : -1;
    zkrat : false;
    napajeni : true;
    stanicni_kolej : false;
    zpomalovani_ready : false;
   );

  private
   UsekStav:TBlkUsekStav;
   last_zes_zkrat:boolean;  //poziva se na pamatovani posledniho stavu zkratu zesilovace pri vypnuti DCC

    procedure SetUsekNUZ(nuz:boolean);
    procedure SetUsekZaver(Zaver:TJCType);
    procedure SetUsekStit(stit:string);
    procedure SetUsekVyl(vyl:string); overload;
    procedure SetSprPredict(sprcesta:Integer);
    procedure SetKonecJC(konecjc:TJCType);
    procedure SetZesZkrat(state:boolean);
    procedure SetZesNap(state:boolean);
    procedure SetVlakPresun(presun:boolean);

    procedure MenuNewLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuEditLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuDeleteLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUVOLLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVEZMILokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRUCLokClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNUZStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNUZStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPRESUNLokClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);

    procedure PotvrDeleteLok(Sender:TIdContext; success:boolean);
    procedure PotvrUvolLok(Sender:TIdContext; success:boolean);

    procedure MenuObsazClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUvolClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure ORVylukaNull(Sender:TIdContext; success:boolean);

  protected
   UsekSettings:TBlkUsekSettings;

    procedure MenuVBClick(SenderPnl:TIdContext; SenderOR:TObject);
    function MenuKCClick(SenderPnl:TIdContext; SenderOR:TObject):boolean;
    function PresunLok(SenderPnl:TIdContext; SenderOR:TObject):boolean;
    procedure SetUsekSpr(spr:Integer); virtual;

  public

    EventsOnObsaz:TChangeEvents;
    EventsOnUvol:TChangeEvents;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    procedure Freeze(); override;
    procedure UnFreeze(); override;

    //----- usek own functions -----

    function GetSettings():TBlkUsekSettings;
    procedure SetSettings(data:TBlkUsekSettings);

    procedure GetObsazeno(var ar:TUsekStavAr);

    procedure AddToReductionDB(blk_id:Integer);

    procedure SetUsekVyl(Sender:TIDCOntext; vyl:string); overload;

    property Stav:TBlkUsekStav read UsekStav;

    property Obsazeno:TUsekStav read UsekStav.Stav;
    property NUZ:boolean read UsekStav.NUZ write SetUsekNUZ;
    property Zaver:TJCType read UsekStav.Zaver write SetUsekZaver;
    property Stitek:string read UsekStav.Stit write SetUsekStit;
    property Vyluka:string read UsekStav.Vyl write SetUsekVyl;
    property Souprava:Integer read UsekStav.Spr write SetUsekSpr;
    property SprPredict:Integer read UsekStav.SprPredict write SetSprPredict;
    property KonecJC:TJCType read UsekStav.KonecJC write SetKonecJC;
    property SComJCRef:TBlk read UsekStav.SComJCRef write UsekStav.SComJCRef;
    property ZesZkrat:boolean read UsekStav.Zkrat write SetZesZkrat;
    property ZesNapajeni:boolean read UsekStav.Napajeni write SetZesNap;
    property VlakPresun:boolean read UsekStav.vlakPresun write SetVlakPresun;
    property zpomalovani_ready:boolean read UsekStav.zpomalovani_ready write UsekStav.zpomalovani_ready;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); override;

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure ShowPanelSpr(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights); override;
 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieMTB, TBloky, TOblRizeni, TBlokSCom, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlokTrat, SprDb, THVDatabase, Zasobnik,
    TBlokIR, Trakce, THnaciVozidlo, TBlokTratUsek;

constructor TBlkUsek.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_USEK;
 Self.UsekStav := _def_usek_stav;
 Self.InitReduction(Self.UsekStav.redukuji);

 Self.EventsOnObsaz := TChangeEvents.Create();
 Self.EventsOnUvol  := TChangeEvents.Create();
end;//ctor

destructor TBlkUsek.Destroy();
begin
 Self.EventsOnObsaz.Free();
 Self.EventsOnUvol.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.UsekSettings.MTBAddrs := Self.LoadMTB(ini_tech, section);
 Self.UsekSettings.Lenght   := ini_tech.ReadFloat(section,'delka',0);
 Self.UsekSettings.Zesil    := ini_tech.ReadInteger(section,'zesil',-1);
 Self.UsekSettings.SmcUsek  := ini_tech.ReadBool(section, 'smc', false);

 Self.UsekStav.Stit         := ini_stat.ReadString(section, 'stit', '');
 Self.UsekStav.Vyl          := ini_stat.ReadString(section, 'vyl' , '');
 Self.UsekStav.Spr          := Soupravy.GetSprIndexByName(ini_stat.ReadString(section, 'spr' , ''));

 str := TStringList.Create();
 ExtractStrings(['|'],[],PChar(ini_tech.ReadString(section, 'zast', '')), str);

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str.Clear();
   ExtractStrings([';'],[],PChar(ini_rel.ReadString('U',IntToStr(Self.GlobalSettings.id),'')),str);
   if (str.Count < 1) then Exit;

   Self.ORsRef := ORs.ParseORs(str[0]);

   if (str.Count >= 2) then
    begin
     if (str[1] = '1') then
      Self.UsekStav.stanicni_kolej := true
     else
      Self.UsekStav.stanicni_kolej := false;
    end;
  end else begin
   Self.ORsRef.Cnt := 0;
  end;

 PushMTBToOR(Self.ORsRef, Self.UsekSettings.MTBAddrs);

 str.Free();
end;//procedure

procedure TBlkUsek.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 Self.SaveMTB(ini_tech, section, Self.UsekSettings.MTBAddrs);
 ini_tech.WriteFloat(section,'delka', Self.UsekSettings.Lenght);
 ini_tech.WriteInteger(section,'zesil', Self.UsekSettings.Zesil);
 ini_tech.WriteBool(section, 'smc', Self.UsekSettings.SmcUsek);
end;//procedure

procedure TBlkUsek.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 ini_stat.WriteString (section, 'stit', Self.UsekStav.Stit);
 ini_stat.WriteString (section, 'vyl' , Self.UsekStav.Vyl);
 ini_stat.WriteString(section, 'spr' , Soupravy.GetSprNameByIndex(Self.UsekStav.Spr));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.Enable();
var i:Integer;
begin
 for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
   if (not MTB.IsModule(Self.UsekSettings.MTBAddrs.data[i].board)) then
    Exit();

 Self.UsekStav.Stav    := none;
 Self.UsekStav.StavOld := none;
 for i := 0 to 3 do Self.UsekStav.StavAr[i] := none;
 Self.Update();
 //change event will be called in Update();
end;//procedure

procedure TBlkUsek.Disable();
var i:Integer;
begin
 inherited;

 Self.UsekStav.Stav       := disabled;
 Self.UsekStav.StavOld    := disabled;
 Self.UsekStav.NUZ        := false;
 Self.UsekStav.SprPredict := -1;
 Self.UsekStav.KonecJC    := TJCType.no;
 Self.UsekStav.zpomalovani_ready := false;
 Self.ZesZkrat            := false;
 for i := 0 to 3 do Self.UsekStav.StavAr[i] := disabled;
 Self.RemoveAllReduction(Self.UsekStav.redukuji);
 Self.Change();
end;//procedure

procedure TBlkUsek.Reset();
begin
 Self.Zaver := TJCType.no;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkUsek.Update();
var i:Integer;
    Blk:TBlk;
begin
 if (((Self.ZesZkrat) or (not Self.ZesNapajeni)) and (not Self.frozen)) then
  begin
//   if (TrkSystem.status <> Ttrk_status.TS_ON) then Self.ZesZkrat := false;
   Self.Freeze();
   Exit();
  end;

 if (Self.frozen) then
  begin
   if (((TrkSystem.status <> Ttrk_status.TS_ON) or (not Self.ZesNapajeni) or (Now < TrkSystem.DCCGoTime+EncodeTime(0, 0, 1, 0))) and (Self.ZesZkrat)) then
    begin
     Self.UsekStav.zkrat := false;
     for i := 0 to Self.ORsRef.Cnt-1 do
       Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt - 1;
     Self.Change(true);
    end;
   if ((TrkSystem.status = Ttrk_status.TS_ON) and (not Self.ZesZkrat) and (Self.ZesNapajeni) and
       (Now > TrkSystem.DCCGoTime+EncodeTime(0, 0, 1, 0))) then Self.UnFreeze();
   Exit();
  end;

 for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
  begin
   case (MTB.GetInput(Self.UsekSettings.MTBAddrs.data[i].board, Self.UsekSettings.MTBAddrs.data[i].port)) of
    0 : Self.UsekStav.StavAr[i] := TUsekStav.uvolneno;
    1 : Self.UsekStav.StavAr[i] := TUsekStav.obsazeno;
    -2,-1:begin
      // vypadek MTB, ci nespravny argument -> disable blok
      if (Self.UsekStav.Stav <> disabled) then
       begin
        Self.UsekStav.Stav    := disabled;
        Self.UsekStav.StavOld := Self.UsekStav.Stav;
        JCDb.RusJC(Self);

        // zastavime soupravu na useku
        if (Self.Souprava > -1) then
         Soupravy.soupravy[Self.Souprava].rychlost := 0;

        Self.Change(true);
       end;
      Exit();
    end;
   end;//case
  end;//for i

 //get current state
 Self.UsekStav.Stav := uvolneno;
 for i := 0 to 3 do
  if (Self.UsekStav.StavAr[i] = TUsekStav.obsazeno) then
   begin
    Self.UsekStav.Stav := TUsekStav.obsazeno;
    break;
   end;

 // reseni vypadku soupravy
 // pad soupravy z bloku az po urcitem case - aby se jizdni ceste nechal cas na zpracovani pohybu soupravy
 if (Self.UsekStav.spr_vypadek) then
  begin
   if (Self.Souprava < 0) then
    begin
     Self.UsekStav.spr_vypadek := false;
     Exit();
    end;

   Inc(Self.UsekStav.spr_vypadek_time);
   if (Self.UsekStav.spr_vypadek_time > 3) then
    begin
     Self.UsekStav.spr_vypadek := false;

     // informace o vypadku soupravy probiha jen ve stanicnich kolejich a v trati
     if ((Self.GetGlobalSettings().typ = _BLK_TU) or (Self.UsekStav.stanicni_kolej)) then
       for i := 0 to Self.OblsRizeni.Cnt-1 do
         Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Ztráta soupravy v úseku '+Self.GetGlobalSettings().name, 'TECHNOLOGIE');
     if (Self.UsekStav.Zaver <> TJCType.no) then Self.UsekStav.Zaver := nouz;
    end;//if spr_vypadek_time > 3
  end;//if spr_vypadek

 //OnChange
 if (Self.UsekStav.Stav <> Self.UsekStav.StavOld) then
  begin
   // kontrola udalosti obsazeni
   if (Self.UsekStav.Stav = TUsekStav.obsazeno) then
     Self.CallChangeEvents(Self.EventsOnObsaz)
   else if (Self.UsekStav.Stav = TUsekStav.uvolneno) then
     Self.CallChangeEvents(Self.EventsOnUvol);

   if (Self.UsekStav.Spr <> -1) then
    begin
     // souprava
     if ((Self.UsekStav.Stav = uvolneno) and (Self.UsekStav.StavOld = obsazeno)) then
      begin
       Self.UsekStav.spr_vypadek      := true;
       Self.UsekStav.spr_vypadek_time := 0;
      end;
    end;//if Spr <> -1

   // pokud je navaznost na trat, chceme ji informaovat o tom, ze se neco zmenilo
   if (Self.GetGlobalSettings().typ = _BLK_TU) then
    begin
     Blky.GetBlkByID(TBlkTU(Self).InTrat, Blk);
     if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_TRAT)) then
       (Blk as TBlkTrat).Change();
    end;

   Self.UsekStav.StavOld := Self.UsekStav.Stav;
   Self.Change();
  end;

 // reseni zruseni PRESUN soupravy, ktera jede
 if ((Self.VlakPresun) and ((Self.Souprava = -1) or ((Self.Souprava > -1) and (Soupravy.soupravy[Self.Souprava].rychlost > 0)))) then
   Self.VlakPresun := false;

 inherited Update();
end;//procedure

procedure TBlkUsek.Change(now:boolean = false);
begin
 // pri zruseni zaveru zrusim redukci na vsech blocich, ktere mam ulozene
 if (Self.Zaver = TJCType.no) then
   Self.RemoveAllReduction(Self.UsekStav.redukuji);

 inherited Change(now);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.SetUsekNUZ(nuz:boolean);
var i:Integer;
begin
 if (Self.UsekStav.NUZ = nuz) then Exit(); 

 if (Self.UsekStav.NUZ) and (not nuz) then
  begin
   for i := 0 to Self.ORsRef.Cnt-1 do
    begin
     Self.ORsRef.ORs[i].NUZblkCnt := Self.ORsRef.ORs[i].NUZblkCnt - 1;
     if (Self.ORsRef.ORs[i].NUZblkCnt < 0) then Self.ORsRef.ORs[i].NUZblkCnt := 0;
    end;
  end else begin
    if ((not Self.UsekStav.NUZ) and (nuz)) then
     for i := 0 to Self.ORsRef.Cnt-1 do
       Self.ORsRef.ORs[i].NUZblkCnt := Self.ORsRef.ORs[i].NUZblkCnt + 1;

  end;

 Self.UsekStav.NUZ := nuz;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekZaver(Zaver:TJCType);
var old:TJCType;
begin
 if ((Integer(Self.UsekStav.Zaver) > 0) and (Zaver = TJCType.no)) then
   Self.NUZ := false;

 old := Self.Zaver;
 Self.UsekStav.Zaver      := Zaver;
 Self.UsekStav.SprPredict := -1;

 // staveci zavery se do panelu neposilaji, protoze jsou mi k nicemu
 if (Self.Zaver <> TJCType.staveni) or (old <> TJCType.no) then
   Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekStit(stit:string);
begin
 Self.UsekStav.Stit := stit;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetUsekVyl(vyl:string);
begin
 Self.UsekStav.Vyl := vyl;
 Self.Change();
end;//procedure

procedure TBlkUsek.ORVylukaNull(Sender:TIdContext; success:boolean);
begin
 if (success) then
  Self.Vyluka := '';
end;//procedure

procedure TBlkUsek.SetUsekVyl(Sender:TIDCOntext; vyl:string);
begin
 if ((self.UsekStav.Vyl <> '') and (vyl = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef.ORs[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil);
  end else begin
   Self.Vyluka := vyl;
  end;
end;//procedure

procedure TBlkUsek.SetUsekSpr(spr:Integer);
begin
 Self.UsekStav.Spr := spr;
 if (spr > -1) then
  Self.UsekStav.SprPredict := -1
 else begin
  Self.UsekStav.vlakPresun        := false;
  Self.UsekStav.zpomalovani_ready := false;
 end;

 Self.Change();
end;//procedure

procedure TBlkUsek.SetSprPredict(sprcesta:Integer);
begin
 Self.UsekStav.SprPredict := sprcesta;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetKonecJC(konecjc:TJCType);
begin
 Self.UsekStav.KonecJC := konecjc;
 Self.Change();
end;//procedure

procedure TBlkUsek.SetZesZkrat(state:boolean);
var i:Integer;
begin
 if (Self.frozen) then
   Self.last_zes_zkrat := state;

 if ((state) and (not Self.frozen) and ((TrkSystem.status <> Ttrk_status.TS_ON) or (not Self.ZesNapajeni) or (Now < TrkSystem.DCCGoTime+EncodeTime(0, 0, 1, 0)))) then
  begin
   Self.Freeze();
   Exit();
  end;

 if (state) then
  begin
   // do OR oznamime, ze nastal zkrat, pak se prehraje zvuk v klientech...
   if (Self.frozen) then Exit();
   for i := 0 to Self.ORsRef.Cnt-1 do
    Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt + 1;
  end else begin
   for i := 0 to Self.ORsRef.Cnt-1 do
    Self.ORsRef.ORs[i].ZKratBlkCnt := Self.ORsRef.ORs[i].ZKratBlkCnt - 1;
  end;

 Self.UsekStav.zkrat := state;
 Self.Change(true);
end;//procedure

procedure TBlkUsek.SetZesNap(state:boolean);
begin
 Self.UsekStav.napajeni := state;
 Self.Change(true);
end;//procedure

procedure TBlkUsek.SetVlakPresun(presun:boolean);
begin
 if (Self.UsekStav.vlakPresun <> presun) then
  begin
   Self.UsekStav.vlakPresun := presun;
   Self.Change();
  end;
end;//procedure

procedure TBlkUsek.Freeze();
begin
 if (Self.frozen) then Exit();

 inherited;
 Self.last_zes_zkrat := Self.ZesZkrat;
end;//procedure

procedure TBlkUsek.UnFreeze();
begin
 if (not Self.frozen) then Exit();

 inherited;
 if (Self.ZesZkrat <> Self.last_zes_zkrat) then
  begin
   Self.ZesZkrat := Self.last_zes_zkrat;
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkUsek.GetSettings():TBlkUsekSettings;
begin
 Result := Self.UsekSettings;
end;//function

procedure TBlkUsek.SetSettings(data:TBlkUsekSettings);
begin
 Self.UsekSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.GetObsazeno(var ar:TUsekStavAr);
begin
 ar := Self.UsekStav.StavAr;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//dynamicke funkce:

procedure TBlkUsek.MenuNewLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 // nejdrive posleme aktualni senam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkNewSpr(Self, SenderPnl);
end;//procedure

procedure TBlkUsek.MenuEditLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 // nejdrive posleme aktualni senam hnacich vozidel
 (SenderOR as TOR).PanelHVList(SenderPnl);

 // pak posleme pozadavek na editaci hnaciho vozidla
 (SenderOR as TOR).BlkEditSpr(Self, SenderPnl, Soupravy.soupravy[Self.Souprava]);
end;//procedure

procedure TBlkUsek.MenuDeleteLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Souprava > -1) then
   ORTCPServer.Potvr(SenderPnl, Self.PotvrDeleteLok, SenderOR as TOR, 'Smazání soupravy '+Soupravy.soupravy[Self.Souprava].nazev, TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUsek.PotvrDeleteLok(Sender:TIdContext; success:boolean);
begin
 if ((success) and (Self.Souprava > -1)) then
  begin
   Self.UsekStav.VlakPresun := false;
   Soupravy.RemoveSpr(Self.Souprava);
  end;
end;//procedure

procedure TBlkUsek.PotvrUvolLok(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();

 if (Blky.GetBlkWithSpr(Self.Souprava).Count = 1) then
  begin
   Soupravy.RemoveSpr(Self.Souprava);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava odstranìna');
  end else begin
   Self.Souprava := -1;
  end;
end;//procedure

procedure TBlkUsek.MenuUVOLLokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Souprava > -1) then
   ORTCPServer.Potvr(SenderPnl, Self.PotvrUvolLok, SenderOR as TOR, 'Uvolnìní soupravy '+Soupravy.soupravy[Self.Souprava].nazev+' z bloku', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUsek.MenuVEZMILokClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   Soupravy.soupravy[Self.Souprava].VezmiVlak();
 except
  on E: Exception do
   begin
    ORTCPServer.BottomError(SenderPnl, 'Vlak se nepodaøilo pøevzít', (SenderOR as TOR).ShortName, 'TECHNOLOGIE');
    Exit();
   end;
 end;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Vlak pøevzat');
end;//procedure

procedure TBlkUsek.MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;//procedure

procedure TBlkUsek.MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.Stav.Vyl);
end;//procedure

// pokud volba nebyla uspesna, vraci false a v tom pripade je vyvolano menu
function TBlkUsek.MenuKCClick(SenderPnl:TIdContext; SenderOR:TObject):boolean;
var Blk:TBlk;
begin
 if ((Self.UsekStav.KonecJC <> TJCType.no) and (not (SenderOR as TOR).vb.Contains(Self))) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit(true);
  end;

 if ((SenderOR as TOR).vb.Contains(Self)) then (SenderOR as TOR).vb.Remove(self);

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk = nil) then Exit(false);

 case ((Blk as TBlkSCom).ZacatekVolba) of
  TBLkSComVolba.VC : Self.UsekStav.KonecJC := TJCType.vlak;
  TBLkSComVolba.PC : Self.UsekStav.KonecJC := TJCType.posun;
  TBLkSComVolba.NC : Self.UsekStav.KonecJC := TJCType.nouz;
 end;//case

 JCDb.StavJC(Blk, Self, SenderPnl, SenderOR);

 Self.Change();
 Result := true;
end;//procedure

procedure TBlkUsek.MenuVBClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if (Self.UsekStav.KonecJC <> TJCType.no) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Probíhá volba');
   Exit();
  end;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk = nil) then Exit();

 case ((Blk as TBlkSCom).ZacatekVolba) of
  TBLkSComVolba.VC : Self.UsekStav.KonecJC := TJCType.vlak;
  TBLkSComVolba.PC : Self.UsekStav.KonecJC := TJCType.posun;
 else
  Exit();     // nouzova cesta nemuze mit variantni body
 end;

 (SenderOR as TOR).vb.Add(Self);

 Self.Change();
end;//procedure

procedure TBlkUsek.MenuNUZStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.NUZ := true;
end;//procedure

procedure TBlkUsek.MenuNUZStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.NUZ := false;
end;//procedure

procedure TBlkUsek.MenuPRESUNLokClick(SenderPnl:TIdContext; SenderOR:TObject; new_state:boolean);
var Blk:TBlk;
begin
 if (Self.Souprava = -1) then Exit();

 if (new_state) then
  begin
   if (Soupravy.soupravy[Self.Souprava].stanice <> SenderOR) then
    begin
     ORTCPServer.SendInfoMsg(SenderPnl, 'Loko se nenachází ve vaši oblasti øízení');
     Exit();
    end;

   Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
   if (Blk <> nil) then (Blk as TBlkUsek).VlakPresun := false;   
   Self.VlakPresun := true;
  end else begin
   Self.VlakPresun := false;
  end;
end;//procedure

procedure TBlkUsek.MenuRUClokClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
    str:string;
    HV:THV;
begin
 str := (SenderOR as TOR).id + ';LOK-TOKEN;OK;';
 for i := 0 to Soupravy.soupravy[Self.Souprava].sdata.HV.cnt-1 do
  begin
   HV := HVDb.HVozidla[Soupravy.soupravy[Self.Souprava].sdata.HV.HVs[i]];
   str := str + '[' + IntToStr(HV.adresa) + '|' + HV.GetToken() + ']';
  end;//for i

 ORTCPServer.SendLn(SenderPnl, str);
end;//procedure

procedure TBlkUsek.MenuObsazClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
   MTB.SetInput(Self.UsekSettings.MTBAddrs.data[i].board, Self.UsekSettings.MTBAddrs.data[i].port, 1);
end;//procedure

procedure TBlkUsek.MenuUvolClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
   MTB.SetInput(Self.UsekSettings.MTBAddrs.data[i].board, Self.UsekSettings.MTBAddrs.data[i].port, 0);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkUsek.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk:TBlk;
    i:Integer;
begin
 Result := inherited;

 if (Self.UsekStav.Spr > -1) then
  begin
   if (Self.UsekStav.stanicni_kolej) then
    Result := Result + 'EDIT vlak,ZRUŠ vlak,';
   Result := Result + 'UVOL vlak,';

   if (Soupravy.soupravy[Self.Souprava].sdata.HV.cnt > 0) then
    Result := Result + 'RUÈ vlak,';

   if (Self.VlakPresun) then
    Result := Result + 'PØESUÒ vlak<,'
   else if ((Soupravy.soupravy[Self.UsekStav.Spr].rychlost = 0) and (Soupravy.soupravy[Self.UsekStav.Spr].stanice = SenderOR)) then
     Result := Result + 'PØESUÒ vlak>,';

   if (Soupravy.soupravy[Self.UsekStav.Spr].ukradeno) then
     Result := Result + 'VEZMI vlak,';

   Result := Result + '-,';
  end else begin
   if ((Self.UsekStav.stanicni_kolej) and (Self.UsekStav.Stav = TUsekStav.obsazeno)) then
     Result := Result + 'NOVÝ vlak,-,';
  end;

 Result := Result + 'STIT,VYL,';

 if (((not (SenderOR as TOR).NUZtimer) and (Integer(Self.UsekStav.Zaver) > 0) and (Self.UsekStav.Zaver <> TJCType.staveni)
    and (Self.GetGlobalSettings().typ = _BLK_USEK) and (not Self.UsekStav.stanicni_kolej)) or (rights >= superuser)) then
  begin
   if (Self.UsekStav.NUZ) then
     Result := Result + '-,NUZ<,'
    else
     Result := Result + '-,NUZ>,';
  end;

 //11 = KC
 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   Result := Result + '-,KC,';
   if (not (SenderOR as TOR).vb.Contains(Self)) then
    Result := Result + 'VB,';
  end;

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (MTB.lib = 2) then
  begin
   Result := Result + '-,';

   for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
    if (Self.UsekStav.StavAr[i] = TUsekStav.uvolneno) then
     begin
      Result := Result + '*OBSAZ,';
      break;
     end;

   for i := 0 to Self.UsekSettings.MTBAddrs.Count-1 do
    if (Self.UsekStav.StavAr[i] = TUsekStav.obsazeno) then
     begin
      Result := Result + '*UVOL,';
      break;
     end;
  end;//if MTB.lib = 2
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.ShowPanelSpr(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
var menu:string;
    i:Integer;
begin
 menu := '$'+Self.GlobalSettings.name+',$Lokomotivy,-,';

 if (Self.UsekStav.Spr > -1) then
  begin
   for i := 0 to Soupravy.soupravy[Self.UsekStav.Spr].sdata.HV.cnt-1 do
    begin
     menu := menu + IntToStr(Soupravy.soupravy[Self.UsekStav.Spr].sdata.HV.HVs[i]) + ':' +
      HVDb.HVozidla[Soupravy.soupravy[Self.UsekStav.Spr].sdata.HV.HVs[i]].Data.Nazev + ',';
    end;
  end;

 ORTCPServer.Menu(SenderPnl, Self, SenderOR as TOR, menu);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkUsek.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if (Self.Stav.Stav <= TUsekStav.none) then Exit();

 if (item = 'NOVÝ vlak')         then Self.MenuNewLokClick(SenderPnl, SenderOR)
 else if (item = 'EDIT vlak')    then Self.MenuEditLokClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ vlak')    then Self.MenuDeleteLokClick(SenderPnl, SenderOR)
 else if (item = 'UVOL vlak')    then Self.MenuUVOLLokClick(SenderPnl, SenderOR)
 else if (item = 'VEZMI vlak')   then Self.MenuVEZMILokClick(SenderPnl, SenderOR)
 else if (item = 'PØESUÒ vlak>') then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, true)
 else if (item = 'PØESUÒ vlak<') then Self.MenuPRESUNLokClick(SenderPnl, SenderOR, false)
 else if (item = 'RUÈ vlak')     then Self.MenuRUCLokClick(SenderPnl, SenderOR)
 else if (item = 'STIT')         then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'VYL')          then Self.MenuVylClick(SenderPnl, SenderOR)
 else if (item = 'KC')           then Self.MenuKCClick(SenderPnl, SenderOR)
 else if (item = 'VB')           then Self.MenuVBClick(SenderPnl, SenderOR)
 else if (item = 'NUZ>')         then Self.MenuNUZStartClick(SenderPnl, SenderOR)
 else if (item = 'NUZ<')         then Self.MenuNUZStopClick(SenderPnl, SenderOR)
 else if (item = 'OBSAZ')        then Self.MenuObsazClick(SenderPnl, SenderOR)
 else if (item = 'UVOL')         then Self.MenuUvolClick(SenderPnl, SenderOR);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUsek.AddToReductionDB(blk_id:Integer);
begin
 Self.AddReduction(Self.UsekStav.redukuji, blk_id);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud loko opravdu presunuto
function TBlkUsek.PresunLok(SenderPnl:TIdContext; SenderOR:TObject):boolean;
var Blk, Trat:TBlk;
begin
 Blk := Blky.GetBlkUsekVlakPresun((SenderOR as TOR).id);
 if (Blk = nil) then Exit(false);
 if (Blk = Self) then
  begin
   Self.VlakPresun := false;
   Exit(true);
  end;

 if (not Self.UsekStav.stanicni_kolej) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Loko lze pøesunout pouze na stanièní kolej');
   Exit(true);
  end;

 if (Self.Souprava > -1) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Úsek již obsahuje soupravu');
   Exit(true);
  end;

 Self.Souprava := (Blk as TBlkUsek).Souprava;

 // mazani supravy z trati
 if ((Blk.GetGlobalSettings().typ = _BLK_TU) and ((Blk as TBlkTU).InTrat > -1)
     and ((Self.GetGlobalSettings().typ = _BLK_USEK) or ((Blk as TBlkTU).InTrat <> TBlkTU(Self).InTrat))) then
  begin
   Blky.GetBlkByID((Blk as TBlkTU).InTrat, Trat);
   if ((Trat <> nil) and (Trat.GetGlobalSettings().typ = _BLK_TRAT)) then
    (Trat as TBLkTrat).RemoveSpr((Blk as TBlkUsek).Souprava);
  end;

 (Blk as TBlkUsek).Souprava := -1;

 ORTCPServer.SendInfoMsg(SenderPnl, 'Loko '+Soupravy.GetSprNameByIndex(Self.Souprava)+' pøesunuta na '+Self.GlobalSettings.name);

 if (Blky.GetBlkWithSpr(Self.Souprava).Count = 1) then
  Soupravy.soupravy[Self.Souprava].front := Self;

 if ((Blk as TBlkUsek).SComJCRef <> nil) then
  Blky.SprPrediction((Blk as TBlkUsek).SComJCRef);

 if (Self.SComJCRef <> nil) then
  Blky.SprPrediction(Self.SComJCRef);

 Result := true;
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit

