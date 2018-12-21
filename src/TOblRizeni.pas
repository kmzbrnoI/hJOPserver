unit TOblRizeni;

{
  Tato unita se stara o rizeni Oblasti rizeni (OR, tedy stanic).
  OR slouzi jako prostrednik mezi technologickymi bloky (technologii) a
  panely (zobrazenim).

  Blok vola metodu primo do prislusne oblasti rizeni, kde jsou data v pripade,
  ze patri konkretni OR, rovnou odeslane do soketu
  neni tedy vyuzivano ORTCPServer
}

interface

uses Types, IniFiles, SysUtils, Classes, Graphics, Menus, stanicniHlaseni,
      IdContext, TechnologieRCS, StrUtils, ComCtrls, Forms,
      Generics.Collections, Zasobnik, Messages, Windows;

const
  _MAX_CON_PNL = 16;                                                            // maximalni poce tpripojenych panelu k jedne oblasti rizeni
  _MAX_ORREF = 16;

  // zvuky - musi korespondovat se zvuky klienta
  _SND_TRAT_ZADOST   = 4;
  _SND_PRIVOLAVACKA  = 5;
  _SND_TIMEOUT       = 6;
  _SND_PRETIZENI     = 7;
  _SND_POTVR_SEKV    = 8;
  _SND_ZPRAVA        = 9;
  _SND_CHYBA         = 10;
  _SND_STAVENI_VYZVA = 11;
  _SND_NENI_JC       = 12;

type
  TORControlRights = (null = 0, read = 1, write = 2, superuser = 3);
  TPanelButton = (F1, F2, ENTER, ESCAPE);

  // podminky potvrzovaci sekvence
  TPSPodminka = record
   blok:TObject;
   podminka:string;
  end;

  TPSPodminky = TList<TPSPodminka>;

  //1 osvetleni
  TOsv = record
   board:Byte;                                                                  // MTB deska osvetleni
   port:Byte;                                                                   // MTB port osvetleni
   name:string;                                                                 // popis osvetleni, max 5 znaku

   default_state:boolean;                                                       // vychozi stav osvetleni - toto je ukladano do souboru a nacitano ze souboru
  end;

 //primarni vlastnosti kazde OR
 TORProp = record
  Name:string;                                                                  // plne jemno oblati rizeni (napr. "Klobouky u Brna")
  ShortName:string;                                                             // zkratka oblasti rizeni (napr. "Klb"), nemusi byt unikatni
  id:string;                                                                    // unikatni ID oblasti rizeni (napr. "Klb")
  Osvetleni:TList<TOsv>;                                                        // seznam osvetleni OR
 end;

 TPSCallback = procedure (Relief:TObject; Panel:TObject;                        // callback potvrzovaci sekvence
                          success:boolean) of object;
 TBlkCallback = procedure (SenderPnl:TIDContext; SenderOR:TObject;              // callback kliku na dopravni kacelar
                           Button:TPanelButton) of object;

 // jedno mereni casu
 TMereniCasu = record
  Start:TDateTime;                                                              // cas startu mereni
  Length:TDateTime;                                                             // delka mereni
  callback:TNotifyEvent;                                                        // callback pri uplynuti mereni casu
  id:Integer;                                                                   // id mereni casu (vyuzivano pro komunikaci s pamely)
 end;

 //databaze pripojenych panelu
 //kazda OR si pamatuje, jake panely jsou k ni pripojeny a s temito panely komunikuje
 // toto je 1 prvek databaze pripojenych panelu
 TORPanel = record
  Panel:TIdContext;                                                             // spojeni na klienta
  Rights:TORControlRights;                                                      // pristupova prava k OR
  user:string;                                                                  // id uzivatele, ktery se k OR logoval
 end;

 // stav OR
 TORStav = record
  NUZtimer:boolean;                                                             // probiha ruseni useku NUZ?
  NUZblkCnt:Integer;                                                            // kolik bloku ma zaplych NUZ (ruseni jeste neprobiha)
  NUZmerCasuID:Integer;                                                         // ID mereni casu NUZ
  ZkratBlkCnt:Integer;                                                          // kolik bloku je ve zkratu (vyuzivano pro prehravani zvuku)
  ZadostBlkCnt:Integer;                                                         // pocet uvazek, na ktere je zadost o tratovy souhlas (kvuli prehravani zvuku)
  PrivolavackaBlkCnt:Integer;                                                   // pocet aktivnich privolavacich navesti
  timerCnt:Integer;                                                             // pocet bezicich timeru
  dk_click_callback:TBlkCallback;                                               // callback kliku na dopravni kancelar
  reg_please:TIdCOntext;                                                        // zde je ulozen regulator, ktery danou oblast rizeni zada o prideleni lokomotivy
 end;

 // jedno MTB oblasti rizeni
 TORMTB = record
  present:boolean;                                                              // jestli je MTB v OR pritomno
  failed:boolean;                                                               // jestli MTB v OR selhalo (nekomunikuje)
 end;

 // seznam MTB v OR
 TORMTBs = record
  MTBs: array [0..TRCS._MAX_RCS] of TORMTB;                                     // seznam MTB v OR, staticky mapovano kde index je adresa MTB
  failture:boolean;                                                             // jestli doslo k selhani jakohokoliv MTB v OR
  last_failture_time:TDateTime;                                                 // cas posledniho selhani (pouziva se pro vytvareni souhrnnych zprav o selhani MTB pro dispecera)
 end;

 /////////////////////////////////////////////////////////////////////////////

  TOR = class
    private const
      //chybove hlasky komunikace
      _COM_ACCESS_DENIED = 'Pøístup odepøen';

      //levely opravneni
      _R_no        = 0;                                                         // zadne opravneni
      _R_read      = 1;                                                         // opravneni ke cteni stavu bloku
      _R_write     = 2;                                                         // opravneni k nastavovani bloku
      _R_superuser = 3;                                                         // opravneni superuser, neboli "root"

    private
      findex:Integer;                                                           // index OR v tabulce oblasti rizeni
      ORProp:TORProp;                                                           // vlastnosti OR
      ORStav:TORStav;                                                           // stav OR

      MereniCasu:TList<TMereniCasu>;                                            // seznam mereni casu bezicich v OR

      OR_MTB:TORMTBs;                                                           // seznam MTB pritomnych v OR, seznam vsech MTB asociovanych s bloky pritomnych v teto OR

      // prace s databazi pripojenych panelu:
      function PnlDAdd(Panel:TIdContext; rights:TORControlRights; user:string):Byte;
      function PnlDRemove(Panel:TIdContext):Byte;
      function PnlDGetRights(Panel:TIdContext):TORControlRights;
      function PnlDGetIndex(Panel:TIdContext):Integer;

      procedure NUZTimeOut(Sender:TObject);                                     // callback ubehnuti mereni casu pro ruseni nouzovych zaveru bloku
      procedure NUZ_PS(Sender:TIdContext; success:boolean);                     // callback potvrzovaci sekvence NUZ

      procedure ORAuthoriseResponse(Panel:TIdContext; Rights:TORControlRights; msg:string; username:string);

      procedure SetNUZBlkCnt(new:Integer);
      procedure SetZkratBlkCnt(new:Integer);
      procedure SetZadostBlkCnt(new:Integer);
      procedure SetPrivolavackaBlkCnt(new:Integer);
      procedure SetTimerCnt(new:Integer);

      procedure MTBClear();                                                     // nastavi vsem MTB, ze nejsou v OR
      procedure MTBUpdate();                                                    // posila souhrnne zpravy panelu o vypadku MTB modulu (moduly, ktere vypadly hned za sebou - do 500 ms, jsou nahlaseny v jedne chybe)

      procedure SendStatus(panel:TIdContext);                                   // odeslani stavu IR do daneho panelu, napr. kam se ma posilat klik na DK, jaky je stav zasobniku atp.; je ovlano pri pripojeni panelu, aby se nastavila OR do spravneho stavu

      function PanelGetOsv(index:Integer):boolean;                              // vrati stav osvetleni

      // tyto funkce jsou volany pri zmene opravenni mezi cteni a zapisem
      // primarni cil = v techto funkcich resit zapinani a vypinani zvuku v panelu
      procedure AuthReadToWrite(panel:TIdContext);
      procedure AuthWriteToRead(panel:TIdContext);

      procedure OnHlaseniAvailable(Sender:TObject; available:boolean);

    public

      stack:TORStack;                                                           // zasobnik povelu
      changed:boolean;                                                          // jestli doslo ke zmene OR - true znamena aktualizaci tabulky
      vb:TList<TObject>;                                                        // seznam variantnich bodu, ktere jsou aktualne "naklikle"; zde je ulozen seznam bloku
      Connected:TList<TORPanel>;                                                // seznam pripojenych panelu
      hlaseni:TStanicniHlaseni;                                                 // technologie stanicnich hlaseni

      constructor Create(index:Integer);
      destructor Destroy(); override;

      procedure LoadData(str:string);
      procedure LoadStat(ini:TMemIniFile; section:string);
      procedure SaveStat(ini:TMemIniFile; section:string);

      procedure RemoveClient(Panel:TIdContext);                                 // smaze klienta \Panel z databze pripojenych panelu, typicky volano pri odpojeni klienta

      procedure Update();                                                       // pravidelna aktualizace stavu OR - napr. mereni casu
      procedure DisconnectPanels();                                             // vyhodi vsechny autorizovane panely z teto OR

      function AddMereniCasu(callback:TNotifyEvent; len:TDateTime):Byte;        // prida mereni casu; vrati ID mereni
      procedure StopMereniCasu(id:Integer);                                     // zastavi mereni casu s danym ID

      procedure MTBAdd(addr:integer);                                           // prida MTB do OR
      procedure MTBFail(addr:integer);                                          // informuje OR o vypadku MTB

      procedure UpdateLine(LI:TListItem);                                       // aktualizuje zaznam v tabulce oblasti rizeni ve F_Main

      procedure BroadcastData(data:string; min_rights:TORControlRights = read); // posle zpravu \data vsem pripojenym panelum s minimalnim opravnenim \min_rights s prefixem oblaati rizeni
      procedure BroadcastGlobalData(data:string; min_rights:TORControlRights = read); // posle zpravu \data vsem pripojenym panelum s minimalnim opravnenim \min_rights s prefixem "-"

      procedure ClearVb();                                                      // smaze aktualni varientni body

      //--- komunikace s technologickymi bloky ---
      procedure BlkChange(Sender:TObject; specificClient:TIDContext = nil);     // doslo ke zmene bloku v OR, je potreba propagovat zmenu do panelu
      procedure BlkPlaySound(Sender:TObject; min_rights:TORCOntrolRights;       // prehraje zvuk
          sound:Integer; loop:boolean = false);
      procedure BlkRemoveSound(Sender:TObject; sound:Integer);                  // zrusi prehravani zvuku
      procedure BlkWriteError(Sender:TObject; error:string; system:string);     // posle chybovou hlasku do vsech stanic, ktere maji autorizovany zapis
      procedure BlkNewSpr(Sender:TObject; Panel:TIdContext; sprUsekIndex:Integer); // posle do panelu pozadavek na otevreni dialogu pro novou soupravu
      procedure BlkEditSpr(Sender:TObject; Panel:TIdContext; Souprava:TObject);// posle do panelu pozadavek na otevreni dialogu editace soupravy

      function ORSendMsg(Sender:TOR; msg:string):Byte;                          // odesle zpravu OR (od jine OR)

      procedure ORDKClickServer(callback:TBlkCallback);                         // klik na DK probehne na server
      procedure ORDKClickClient();                                              // klik na DK probehne na klieta

      // volany pri zadosti o poskytnuti loko pro regulator:
      function LokoPlease(Sender:TIDContext; user:TObject; comment:string):Integer;
      procedure LokoCancel(Sender:TIdContext);

      procedure InitOsv();

      //--- komunikace s panely zacatek: ---
      procedure PanelAuthorise(Sender:TIdContext; rights:TORControlRights; username:string; password:string);
      procedure PanelFirstGet(Sender:TIdContext);
      procedure PanelClick(Sender:TIdContext; blokid:Integer; Button:TPanelButton; params:string = '');
      procedure PanelEscape(Sender:TIdContext);
      procedure PanelNUZ(Sender:TIdContext);
      procedure PanelNUZCancel(Sender:TIdContext);
      procedure PanelMessage(Sender:TIdContext; recepient:string; msg:string);
      procedure PanelHVList(Sender:TIdContext);
      procedure PanelSprChange(Sender:TIdContext; spr:TStrings);
      procedure PanelMoveLok(Sender:TIdContext; lok_addr:word; new_or:string);
      procedure PanelZAS(Sender:TIdContext; str:TStrings);
      procedure PanelDKClick(SenderPnl:TIdContext; Button:TPanelButton);
      procedure PanelLokoReq(Sender:TIdContext; str:TStrings);
      procedure PanelHlaseni(Sender:TIDContext; str:TStrings);

      procedure PanelHVAdd(Sender:TIDContext; str:string);
      procedure PanelHVRemove(Sender:TIDContext; addr:Integer);
      procedure PanelHVEdit(Sender:TIDContext; str:string);

      procedure PanelSendOsv(Sender:TIdContext);
      procedure PanelSetOsv(Sender:TIdCOntext; id:string; state:boolean);

      function PanelGetSprs(Sender:TIdCOntext):string;
      procedure PanelRemoveSpr(Sender:TIDContext; spr_index:Integer);

      function GetORPanel(conn:TIdContext; var ORPanel:TORPanel):Integer;
      class function GetRightsString(rights:TORControlRights):string;

      procedure UserUpdateRights(user:TObject);
      procedure UserDelete(userid:string);

      class function ORRightsToString(rights:TORControlRights):string;
      class function GetPSPodminka(blok:TObject; podminka:string):TPSPodminka;
      class function GetPSPodminky(podm:TPSPodminka):TPSPodminky;

      property NUZtimer:Boolean read ORStav.NUZtimer write ORStav.NUZtimer;
      property NUZblkCnt:Integer read ORStav.NUZblkCnt write SetNUZBlkCnt;
      property ZKratBlkCnt:Integer read ORStav.ZKratBlkCnt write SetZkratBlkCnt;
      property ZadostBlkCnt:Integer read ORStav.ZadostBlkCnt write SetZadostBlkCnt;
      property PrivolavackaBlkCnt:Integer read ORStav.PrivolavackaBlkCnt write SetPrivolavackaBlkCnt;
      property TimerCnt:Integer read ORStav.TimerCnt write SetTimerCnt;
      property reg_please:TIdContext read ORStav.reg_please;

      //--- komunikace s panely konec ---

      property Name:string read ORProp.Name;
      property ShortName:string read ORProp.ShortName;
      property id:string read ORProp.id;
  end;//TOR

implementation

////////////////////////////////////////////////////////////////////////////////

uses TBloky, GetSystems, TBlokVyhybka, TBlokUsek, TBlokSCOm, fMain, Booster,
     TechnologieJC, TBlokPrejezd, TJCDatabase, Prevody, TCPServerOR,
     TBlokUvazka, TBlokTrat, TOblsRizeni, TBlok, THVDatabase, SprDb,
     Logging, UserDb, THnaciVozidlo, Trakce, TBlokZamek, User,
     fRegulator, TBlokRozp, RegulatorTCP, ownStrUtils, TBlokTratUsek, Souprava,
     TBlokSouctovaHlaska, predvidanyOdjezd;

constructor TOR.Create(index:Integer);
begin
 inherited Create();

 Self.findex := index;

 Self.ORProp.Osvetleni := TList<TOsv>.Create();
 Self.Connected        := TList<TORPanel>.Create();
 Self.MTBClear();

 Self.ORStav.dk_click_callback := nil;
 Self.ORStav.reg_please := nil;

 Self.stack   := TORStack.Create(index+1, Self);
 Self.vb      := TList<TObject>.Create();
 Self.changed := false;

 Self.MereniCasu := TList<TMereniCasu>.Create();
 Self.hlaseni := nil;
end;//ctor

destructor TOR.Destroy();
begin
 if (Assigned(Self.hlaseni)) then
   Self.hlaseni.Free();

 Self.stack.Free();
 Self.ORProp.Osvetleni.Free();
 Self.vb.Free();
 Self.MereniCasu.Free();
 Self.Connected.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

//nacitani dat OR
//na kazdem radku je ulozena jedna oblast rizeni ve formatu:
//  nazev;nazev_zkratka;id;(osv_mtb|osv_port|osv_name)(osv_mtb|...)...;;
procedure TOR.LoadData(str:string);
var data_main,data_osv,data_osv2:TStrings;
    j:Integer;
    Osv:TOsv;
begin
 data_main := TStringList.Create();
 data_osv  := TStringList.Create();
 data_osv2 := TStringList.Create();

 try
   ExtractStrings([';'],[],PChar(str), data_main);

   if (data_main.Count < 3) then
     raise Exception.Create('Mene nez 3 parametry v popisu oblasti rizeni');

   Self.ORProp.Name       := data_main[0];
   Self.ORProp.ShortName  := data_main[1];
   Self.ORProp.id         := data_main[2];

   Self.ORProp.Osvetleni.Clear();

   data_osv.Clear();
   if (data_main.Count > 3) then
    begin
     ExtractStrings(['(', ')'], [], PChar(data_main[3]), data_osv);
     for j := 0 to data_osv.Count-1 do
      begin
       data_osv2.Clear();
       ExtractStrings(['|'], [], PChar(data_osv[j]), data_osv2);

       try
         Osv.default_state := false;
         Osv.board := StrToInt(data_osv2[0]);
         Osv.port  := StrToInt(data_osv2[1]);
         Osv.name  := data_osv2[2];
         Self.ORProp.Osvetleni.Add(Osv);
       except

       end;
      end;//for j
    end;

   Self.hlaseni := TStanicniHlaseni.Create(Self.id);
   Self.hlaseni.OnAvailable := Self.OnHlaseniAvailable;
 finally
   FreeAndNil(data_main);
   FreeAndNil(data_osv);
   FreeAndNil(data_osv2);
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// nacitani or_stat.ini souboru
// musi byt volano po LoadData
procedure TOR.LoadStat(ini:TMemIniFile; section:string);
var osv:TOsv;
    i:Integer;
begin
 // nacteme stav osvetleni
 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
  begin
   osv := Self.ORProp.Osvetleni[i];
   osv.default_state := ini.ReadBool(section, osv.name, false);
   Self.ORProp.Osvetleni[i] := osv;
  end;
end;

// ukladani or_stat.ini souboru
procedure TOR.SaveStat(ini:TMemIniFile; section:string);
var i:Integer;
begin
 // zapiseme stav osvetleni
 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
   ini.WriteBool(section, Self.ORProp.Osvetleni[i].name, Self.ORProp.Osvetleni[i].default_state);
end;

////////////////////////////////////////////////////////////////////////////////

//  or;CHANGE;typ_blk;tech_blk_id;barva_popredi;barva_pozadi;blikani; dalsi argumenty u konkretnich typu bloku:
//    typ_blk = cislo podle typu bloku na serveru
//      usek : konec_jc;[souprava;barva_soupravy;sipkaLsipkaS;barva_pozadi] -  posledni 3 argumenty jsou nepovinne
//      vyhybka : poloha (cislo odpovidajici poloze na serveru - [disabled = -5, none = -1, plus = 0, minus = 1, both = 2])
//      navestidlo: ab (false = 0, true = 1)
//      pjejezd: stav (otevreno = 0, vystraha = 1, uzavreno = 2, anulace = 3)
//      uvazka: smer (0 = zakladni, 1 = opacny); soupravy - cisla souprav oddelena carkou

//komunikace mezi technologickymi bloky a oblastmi rizeni
//tato funkce je vyvolana pri zmene stavu jakehokoliv bloku
procedure TOR.BlkChange(Sender:TObject; specificClient:TIDContext = nil);
var blk_glob:TBlkSettings;
    i:Integer;
    msg:string;
    fg, bg, nebarVetve, sfg, sbg:TColor;
    Blk:TBlk;
begin
 if (Self.Connected.Count = 0) then Exit();

 blk_glob := (Sender as TBlk).GetGlobalSettings;

 fg := clFuchsia;
 bg := clBlack;
 if (blk_glob.typ = _BLK_TU) then blk_glob.typ := _BLK_USEK; 
 msg := Self.id+';CHANGE;'+IntToStr(blk_glob.typ)+';'+IntToStr(blk_glob.id)+';';

 case (blk_glob.typ) of
  _BLK_VYH:begin
   //vytvoreni dat
   if (not (Sender as TBlkVyhybka).vyhZaver) then
    begin
     case ((Sender as TBlkVyhybka).Obsazeno) of
      TUsekStav.disabled: fg := clFuchsia;
      TUsekStav.none    : fg := $A0A0A0;
      TUsekStav.uvolneno: fg := $A0A0A0;
      TUsekStav.obsazeno: fg := clRed;
     end;//case

     if ((Sender as TBlkVyhybka).Obsazeno = TUsekStav.uvolneno) then
      begin
       case ((Sender as TBlkVyhybka).Zaver) of
        vlak   : fg := clLime;
        posun  : fg := clWhite;
        nouz   : fg := clAqua;
        ab     : fg := $707070;
       end;//case

       // je soucasti vybarveneho neprofiloveho useku
       Blky.GetBlkByID(TBlkVyhybka(Sender).UsekID, Blk);
       if ((Blk <> nil) and ((Blk.typ = _BLK_USEK) or (Blk.typ = _BLK_TU))
           and (fg = $A0A0A0) and (TBlkUsek(Blk).IsNeprofilJC)) then
         fg := clYellow;
      end;

     // do profilu vyhybky zasahuje obsazeny usek
     if (((fg = $A0A0A0) or (fg = clRed)) and (TBlkVyhybka(Sender).npBlokPlus <> nil) and
         (TBlkVyhybka(Sender).Poloha = TVyhPoloha.plus) and
         (TBlkUsek(TBlkVyhybka(Sender).npBlokPlus).Obsazeno <> TUsekStav.uvolneno)) then
       fg := clYellow;

     // do profilu vyhybky zasahuje obsazeny usek
     if (((fg = $A0A0A0) or (fg = clRed)) and (TBlkVyhybka(Sender).npBlokMinus <> nil) and
         (TBlkVyhybka(Sender).Poloha = TVyhPoloha.minus) and
         (TBlkUsek(TBlkVyhybka(Sender).npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
       fg := clYellow;

    end else begin
     // nouzovy zaver vyhybky ma prioritu i nad obsazenim useku
     fg := clAqua;
    end;

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';

   bg := clBlack;
   if ((Sender as TBlkVyhybka).Stitek <> '') then bg := clTeal;
   if ((Sender as TBlkVyhybka).Vyluka <> '') then bg := clOlive;

   msg := msg + PrevodySoustav.ColorToStr(bg) + ';';

   if ((Sender as TBlkVyhybka).NUZ) then
    msg := msg + '1;'
   else
    msg := msg + '0;';

   msg := msg + IntToStr(Integer((Sender as TBlkVyhybka).Poloha))+';';
  end;//_BLK_VYH

  /////////////////////////////////////////////////

  _BLK_USEK:begin
   //vytvoreni dat
   nebarVetve := $A0A0A0;

   if ((Sender as TBlkUsek).Obsazeno = TUsekStav.disabled) then
    begin
     msg := msg + PrevodySoustav.ColorToStr(clFuchsia) + ';' + PrevodySoustav.ColorToStr(clBlack) +
       ';0;0;' + PrevodySoustav.ColorToStr(clBlack);
    end else begin
     case ((Sender as TBlkUsek).Obsazeno) of
      TUsekStav.disabled : fg := clFuchsia;
      TUsekStav.none     : fg := $A0A0A0;
      TUsekStav.uvolneno : fg := $A0A0A0;
      TUsekStav.obsazeno : fg := clRed;
     end;//case

     // zobrazeni zakazu odjezdu do trati
     if ((fg = $A0A0A0) and ((Sender as TBlk).typ = _BLK_TU) and ((Sender as TBlkTU).InTrat > -1)) then
      begin
       Blky.GetBlkByID((Sender as TBlkTU).InTrat, Blk);
       if ((Blk <> nil) and (Blk.typ = _BLK_TRAT)) then
         if ((Blk as TBlkTrat).ZAK) then
          fg := clBlue;
      end;

     // neprofilove deleni v useku
     if ((fg = $A0A0A0) and (TBlkUsek(Sender).IsNeprofilJC())) then
       fg := clYellow;

     // zobrazeni zaveru
     if ((((Sender as TBlkUsek).Obsazeno) = TUsekStav.uvolneno) and
         ((Sender as TBlk).typ = _BLK_USEK) and
         ((Sender as TBlkUsek).GetSettings().RCSAddrs.Count > 0)) then
      begin
       case ((Sender as TBlkUsek).Zaver) of
        vlak   : fg := clLime;
        posun  : fg := clWhite;
        nouz   : fg := clAqua;
        ab     : fg := $707070;
       end;//case
      end;

     // zobrazeni poruchy BP v trati
     if (((Sender as TBlk).typ = _BLK_TU) and (TBlkTU(Sender).poruchaBP)) then fg := clAqua;

     // neprofilove deleni v useku
     if (fg = clYellow) then
       nebarVetve := clYellow;

     msg := msg + PrevodySoustav.ColorToStr(fg) + ';';

     if ((Sender as TBlkUsek).Stitek <> '') then bg := clTeal;
     if ((Sender as TBlkUsek).Vyluka <> '') then bg := clOlive;

     if (not TBlkUsek(Sender).DCC) then bg := clMaroon;
     if ((Sender as TBlkUsek).ZesZkrat = TBoosterSignal.error) then bg := clFuchsia;
     if (((Sender as TBlkUsek).ZesNapajeni <> TBoosterSignal.ok) or
        ((Sender as TBlkUsek).ZesZkrat = TBoosterSignal.undef)) then bg := clBlue;

     msg := msg + PrevodySoustav.ColorToStr(bg) + ';';

     if ((Sender as TBlkUsek).NUZ) then
      msg := msg + '1;'
     else
      msg := msg + '0;';

     msg := msg + IntToStr(Integer((Sender as TBlkUsek).KonecJC)) + ';';
     msg := msg + PrevodySoustav.ColorToStr(nebarVetve) + ';';

     // odeslani seznamu souprav
     msg := msg + '{';
     for i := 0 to (Sender as TBlkUsek).Soupravs.Count-1 do
      begin
       sfg := fg;
       sbg := bg;

       if ((Sender as TBlkUsek).Obsazeno = uvolneno) then
         sfg := clAqua;

       msg := msg + '(' + Soupravy[(Sender as TBlkUsek).Soupravs[i]].nazev + ';';

       if (Soupravy[(Sender as TBlkUsek).Soupravs[i]].sdata.smer_L) then
         msg := msg + '1'
       else
         msg := msg + '0';
       if (Soupravy[(Sender as TBlkUsek).Soupravs[i]].sdata.smer_S) then
         msg := msg + '1;'
       else
         msg := msg + '0;';

       if ((Soupravy[(Sender as TBlkUsek).Soupravs[i]].cilovaOR = Self) and (sbg = clBlack)) then
         sbg := clSilver;

       // predvidany odjezd
       if (Soupravy[(Sender as TBlkUsek).Soupravs[i]].IsPOdj(Sender as TBlk)) then
         if (Soupravy[(Sender as TBlkUsek).Soupravs[i]].IsPOdj(Sender as TBlkUsek)) then
           predvidanyOdjezd.GetPOdjColors(Soupravy[(Sender as TBlkUsek).Soupravs[i]].GetPOdj(Sender as TBlkUsek), sfg, sbg);

       msg := msg + PrevodySoustav.ColorToStr(sfg) + ';';
       msg := msg + PrevodySoustav.ColorToStr(sbg) + ';';

       if ((Sender as TBlkUsek).vlakPresun = i) then
        msg := msg + PrevodySoustav.ColorToStr(clYellow) + ';';

       msg := msg + ')';
      end;

     // predpovidana souprava
     if ((Sender as TBlkUsek).SprPredict > -1) then
      begin
       // predvidany odjezd
       sfg := fg;
       sbg := bg;

       if (Soupravy[(Sender as TBlkUsek).SprPredict].IsPOdj(Sender as TBlkUsek)) then
         predvidanyOdjezd.GetPOdjColors(Soupravy[(Sender as TBlkUsek).SprPredict].GetPOdj(Sender as TBlkUsek), sfg, sbg);

       msg := msg + '(' + Soupravy.GetSprNameByIndex((Sender as TBlkUsek).SprPredict) + ';' +
                  '00;' +
                  PrevodySoustav.ColorToStr(sfg) + ';' +
                  PrevodySoustav.ColorToStr(sbg) + ';)';
      end;

     msg := msg + '}';
    end;

  end;//_BLK_USEK

  /////////////////////////////////////////////////

  _BLK_SCOM:begin
   //vytvoreni dat
   if ((Sender as TBlkSCom).Navest = -1) then
    begin
     fg := clBlack;
     bg := clFuchsia;
    end else begin
     // privolavaci navest
     if ((Sender as TBlkSCom).Navest = 8) then
      begin
       fg := clWhite;
      end else begin
       if (((Sender as TBlkSCom).DNjc <> nil) and ((Sender as TBlkSCom).Navest > 0)) then
        begin
          case ((Sender as TBlkSCom).DNjc.data.TypCesty) of
           TJCType.vlak  : fg := clLime;
           TJCType.posun : fg := clWhite;
          else
           fg := clAqua;
          end;
        end else begin
         if (((Sender as TBlkSCom).Navest <> 8) and ((Sender as TBlkSCom).canRNZ)) then
           fg := clTeal
         else
           fg := $A0A0A0;
        end;
      end;// else privolavacka

     if ((Sender as TBlkSCom).ZAM) then
      begin
       case ((Sender as TBlkSCom).SymbolType) of
        0 : fg := clRed;
        1 : fg := clBlue;
       end;
      end;

     case ((Sender as TBlkSCom).ZacatekVolba) of
      TBlkSComVolba.none : bg := clBlack;
      TBlkSComVolba.VC   : bg := clGreen;
      TBlkSComVolba.PC   : bg := clWhite;
      TBlkSComVolba.NC,
      TBlkSComVolba.PP   : bg := clTeal;
     end;//case
    end;//else Navest = -1

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';';

   // blikani privolavacky
   if ((Sender as TBlkSCom).Navest = 8) then
    msg := msg + '1;'
   else
    msg := msg + '0;';

   msg := msg + IntToStr(PrevodySoustav.BoolToInt((Sender as TBlkSCom).AB)) + ';';
  end;//_BLK_SCOM

  /////////////////////////////////////////////////

  _BLK_PREJEZD:begin
   //vytvoreni dat

   if ((Sender as TBlkPrejezd).Stitek <> '') then bg := clTeal;

   if ((Sender as TBlkPrejezd).NOtevreni) then fg := clRed
   else if ((Sender as TBlkPrejezd).UZ) then fg := clWhite
   else fg := $A0A0A0;

   case ((Sender as TBlkPrejezd).Stav.basicStav) of
     TBlkPrjBasicStav.disabled : begin
       fg := clBlack;
       bg := clFuchsia;
     end;

     TBlkPrjBasicStav.none : begin
       fg := clBlack;
       bg := clRed;
     end;
   end;

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';0;';
   msg := msg + IntToStr(Integer((Sender as TBlkPrejezd).Stav.basicStav)) + ';';
  end;//_BLK_SCOM

  /////////////////////////////////////////////////

  _BLK_UVAZKA:begin
   //vytvoreni dat

   if (((Sender as TBlkUvazka).parent as TBlkTrat).Smer = TTratSmer.disabled) then
    begin
     fg := clBlack;
     bg := clFuchsia;
    end else begin
     if ((Sender as TBlkUvazka).Stitek <> '') then bg := clTeal
     else bg := clBlack;

     if (((Sender as TBlkUvazka).parent as TBlkTrat).RBPCan) then fg := clRed
     else if (((Sender as TBlkUvazka).parent as TBlkTrat).Zaver) then fg := clBlue
     else if (((Sender as TBlkUvazka).parent as TBlkTrat).nouzZaver) then fg := clAqua
     else if (((Sender as TBlkUvazka).parent as TBlkTrat).Obsazeno) then fg := clBlue
     else fg := $A0A0A0;
    end;

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';';
   if (((Sender as TBlkUvazka).parent as TBlkTrat).Zadost) then
    msg := msg + '1;'
   else
    msg := msg + '0;';

   // smer trati
   case (((Sender as TBlkUvazka).parent as TBlkTrat).Smer) of
    TTratSmer.disabled, TTratSmer.zadny
                       : msg := msg + '0;';
    TTratSmer.AtoB     : msg := msg + '1;';
    TTratSmer.BtoA     : msg := msg + '2;';
   end;//case

   // soupravy
   msg := msg + ((Sender as TBlkUvazka).parent as TBlkTrat).GetSprList(',') + ';';
  end;//_BLK_UVAZKA

  /////////////////////////////////////////////////

  _BLK_ZAMEK:begin
   //vytvoreni dat

   if ((Sender as TBlkZamek).stitek <> '') then bg := clTeal
   else bg := clBlack;

   if ((Sender as TBlkZamek).porucha) then begin
    fg := bg;
    bg := clBlue;
   end
   else if ((Sender as TBlkZamek).klicUvolnen) then fg := clBlue
   else if ((Sender as TBlkZamek).nouzZaver) then fg := clAqua
   else fg := $A0A0A0;

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';0;';
  end;//_BLK_ZAMEK

  /////////////////////////////////////////////////

  _BLK_ROZP:begin
   //vytvoreni dat

   bg := clBlack;
   case ((Sender as TBlkRozp).status) of
     TRozpStatus.disabled     : fg := clFuchsia;
     TRozpStatus.not_selected : fg := $A0A0A0;
     TRozpStatus.mounting     : fg := clYellow;
     TRozpStatus.active       : fg := clLime;
   end;//case

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';0;';
  end;//_BLK_ROZP

  /////////////////////////////////////////////////

  _BLK_SH:begin
   //vytvoreni dat

   // n.o. rail
   fg := clTeal;
   if (TBlkSH(Sender).anulace) then
     fg := clWhite;
   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(clBlack) + ';0;';

   // left rectangle
   fg := clGreen;
   if ((TBlkSH(Sender).porucha) or (TBlkSH(Sender).nouzoveOT)) then
     fg := clRed;
   if (not TBlkSH(Sender).komunikace) then
     fg := clFuchsia;
   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';

   // right rectangle
   fg := clBlack;
   if (TBlkSH(Sender).uzavreno) then
     fg := $A0A0A0;
   if (TBlkSH(Sender).UZ) then
     fg := clWhite;
   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
  end;//_BLK_SH

 else Exit; end;

 //odeslani do OR
 for i := 0 to Self.Connected.Count-1 do
  begin
   if (Self.Connected[i].Rights < TORControlRights.read) then continue;
   if ((specificClient <> nil) and (Self.Connected[i].Panel <> specificClient)) then continue;

   ORTCPServer.SendLn(Self.Connected[i].Panel, msg);

   // aktualizace menu
   if ((Self.Connected[i].Panel.Data as TTCPORsRef).menu = Sender) then
     ORTCPServer.Menu(Self.Connected[i].Panel, (Sender as TBlk), Self, (Sender as TBlk).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights));

  end;//for i

end;//procedure

procedure TOR.BlkWriteError(Sender:TObject; error:string; system:string);
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
  if (Self.Connected[i].Rights >= TORControlRights.write) then
    ORTCPServer.BottomError(Self.Connected[i].Panel, error, Self.ShortName, system);
end;//procedure

procedure TOR.BlkPlaySound(Sender:TObject; min_rights:TORCOntrolRights; sound:Integer; loop:boolean = false);
var i:Integer;
begin
 for i := 0 to Self.Connected.COunt-1 do
  if (Self.Connected[i].Rights >= min_rights) then
   ORTCPServer.PlaySound(Self.Connected[i].Panel, sound, loop);
end;//procedure

procedure TOR.BlkRemoveSound(Sender:TObject; sound:Integer);
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
  ORTCPServer.DeleteSound(Self.Connected[i].Panel, sound);
end;//procedure

procedure TOR.BlkNewSpr(Sender:TObject; Panel:TIdContext; sprUsekIndex:Integer);
begin
 TTCPORsRef(Panel.Data).spr_new_usek_index := sprUsekIndex;
 TTCPORsRef(Panel.Data).spr_usek := Sender;
 ORTCPServer.SendLn(Panel, Self.id+';SPR-NEW;');
end;//procedure

procedure TOR.BlkEditSpr(Sender:TObject; Panel:TIdContext; Souprava:TObject);
begin
 TTCPORsRef(Panel.Data).spr_new_usek_index := -1;
 TTCPORsRef(Panel.Data).spr_edit := TSouprava(Souprava);
 TTCPORsRef(Panel.Data).spr_usek := Sender;

 ORTCPServer.SendLn(Panel, Self.id+';'+'SPR-EDIT;'+TSouprava(Souprava).GetPanelString());
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//funkce pro praci s databazi pripojenych panelu

//pridani 1 prvku do databaze
//v pripade existence jen zvysime prava
function TOR.PnlDAdd(Panel:TIdContext; rights:TORControlRights; user:string):Byte;
var i:Integer;
    pnl:TORPanel;
begin
 Result := 0;

 for i := 0 to Self.Connected.Count-1 do
  begin
   if (Self.Connected[i].Panel = Panel) then
    begin
     // pokud uz je zaznam v databazi, pouze upravime tento zaznam
     pnl := Self.Connected[i];
     pnl.Rights := rights;
     pnl.user   := user;
     Self.Connected[i] := pnl;
     Exit;
    end;
  end;//for i

 if (Self.Connected.Count >= _MAX_CON_PNL) then Exit(1);
 if ((Panel.Data as TTCPORsRef).ORsCnt >= _MAX_ORREF) then Exit(2);

 //pridani 1 panelu
 pnl.Panel  := Panel;
 pnl.Rights := rights;
 pnl.user   := user;
 Self.Connected.Add(pnl);

 // pridame referenci na sami sebe do TIDContext
 (Panel.Data as TTCPORsRef).ORsCnt := (Panel.Data as TTCPORsRef).ORsCnt + 1;
 (Panel.Data as TTCPORsRef).ORs[(Panel.Data as TTCPORsRef).ORsCnt-1] := Self;

 // odesleme incializacni udaje
 if (rights > TORCOntrolRights.null) then
  begin
   Self.SendStatus(Panel);
   Self.stack.NewConnection(Panel);
  end;
end;//function

//mazani 1 panelu z databaze
function TOR.PnlDRemove(Panel:TIdContext):Byte;
var i, found:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
  begin
   if (Self.Connected[i].Panel = Panel) then
    begin
     Self.Connected.Delete(i);
     Break;
    end;
  end;//for i

 // a samozrejme se musime smazat z oblasti rizeni
 found := -1;
 for i := 0 to (Panel.Data as TTCPORsRef).ORsCnt-1 do
  if ((Panel.Data as TTCPORsRef).ORs[i] = Self) then
   begin
    found := i;
    break;
   end;

 if (found <> -1) then
  begin
   for i := found to (Panel.Data as TTCPORsRef).ORsCnt-2 do
     (Panel.Data as TTCPORsRef).ORs[i] := (Panel.Data as TTCPORsRef).ORs[i+1];
   (Panel.Data as TTCPORsRef).ORsCnt := (Panel.Data as TTCPORsRef).ORsCnt - 1;
  end;

 Result := 0;
end;//function

//ziskani prav daneho panelu z databaze
//v pripade nenalezeni panelu v datbazi vracime prava 'no' = zadna
function TOR.PnlDGetRights(Panel:TIdContext):TORControlRights;
var index:Integer;
begin
 index := Self.PnlDGetIndex(Panel);
 if (index < 0) then
  begin
   Result := TORCOntrolRights.null;
   Exit;
  end;

 Result := Self.Connected[index].Rights;
end;//function

function TOR.PnlDGetIndex(Panel:TIdContext):Integer;
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
   if (Self.Connected[i].Panel = Panel) then
     Exit(i);

 Result := -1;
end;//function

////////////////////////////////////////////////////////////////////////////////
//komunikace s panely:

//touto funkci panel zada o opravneni
procedure TOR.PanelAuthorise(Sender:TIdContext; rights:TORControlRights; username:string; password:string);
var i:Integer;
    UserRights:TORControlRights;
    msg:string;
    panel:TORPanel;
    user:TUser;
    last_rights:TORControlRights;
begin
 // panel se chce odpojit -> vyradit z databaze
 if (rights = TORControlRights.null) then
  begin
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Úspìšnì autorizováno - odpojen', '');
   ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
   if (Self.PnlDGetRights(Sender) >= write) then Self.AuthWriteToRead(Sender);
   if (Self.PnlDGetIndex(Sender) > -1) then Self.PnlDRemove(Sender);
   Exit();
  end;

 // tady mame zaruceno, ze panel chce zadat o neco vic, nez null

 // -> zjistime uzivatele
 user := UsrDb.GetUser(username);

 // kontrola existence uzivatele
 if (not Assigned(user)) then
  begin
   UserRights := TORControlRights.null;
   msg := 'Uživatel '+username+' neexistuje !';
  end else

 // kontrola BANu uzivatele
 if (user.ban) then
  begin
   UserRights := TORControlRights.null;
   msg := 'Uživatel '+user.id+' má BAN !';
  end else

 // kontrola opravneni uzivatele pro tento panel
 if (not TUser.ComparePasswd(password, user.password, user.salt)) then
  begin
   UserRights := TORControlRights.null;
   msg := 'Neplatné heslo !';
  end else begin
   UserRights := user.GetRights(Self.id);
   if (UserRights < rights) then
     msg := 'K této OØ nemáte oprávnìní';
  end;

 // do last_rights si ulozime posledni opravneni panelu
 last_rights := Self.PnlDGetRights(Sender);

 if (UserRights < rights) then
  begin
   if (UserRights > TORControlRights.null) then
    begin
     Self.PnlDAdd(Sender, UserRights, username);
     Self.ORAuthoriseResponse(Sender, UserRights, msg, user.fullName)
    end else begin
     Self.PnlDRemove(Sender);
     Self.ORAuthoriseResponse(Sender, UserRights, msg, '')
    end;

   ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
   Exit;
  end;

 // kontrola vyplych systemu
 if ((not GetFunctions.GetSystemStart) and (rights > read) and (rights < superuser)) then
  begin
   // superuser muze autorizovat zapis i pri vyplych systemech
   Self.PnlDAdd(Sender, TORControlRights.read, username);
   Self.ORAuthoriseResponse(Sender, TORControlRights.read, 'Nelze autorizovat zápis pøi vyplých systémech !', user.fullName);
   ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
   Exit;
  end;

 msg := 'Úspìšnì autorizováno !';

 // kontrola pripojeni dalsich panelu
 // pokud chce panel zapisovat, musime zkontrolovat, jestli uz nahodou neni nejaky panel s pravy zapisovat, pripojeny
 if (UserRights < TORCOntrolRights.superuser) then
  begin
   // pokud jsme superuser, pripojenost dalsich panelu nekontrolujeme
   for i := 0 to Self.Connected.Count-1 do
    begin
     if ((Self.Connected[i].Rights > read) and (Self.Connected[i].Panel <> Sender) and (Self.Connected[i].Rights < superuser)) then
      begin
       // pokud se pripojuje stejny uzivatel, prevezme rizeni z jiz pripojene OR
       //  jiny uzivatel rizeni prevzit nemuze
       // -> technologie pripojovani zarucuje, ze pripojeny dispecer muze byt jen jeden
       if (Self.Connected[i].user = username) then
        begin
         panel := Self.Connected[i];
         panel.Rights := TORCOntrolRights.read;
         Self.Connected[i] := panel;
         Self.ORAuthoriseResponse(panel.Panel, panel.Rights, 'Pøevzetí øízení', user.fullName);
         ORTCPServer.GUIQueueLineToRefresh(i);
        end else begin
         rights := TORControlRights.read;
         msg := 'Panel již pøipojen !';
         break;
        end;
      end;
    end;//for i
  end;

 if (Self.PnlDAdd(Sender, rights, username) <> 0) then
  begin
   ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Pøipojeno maximum OØ, nelze autorizovat další !', '');
   Exit;
  end;

 UsrDb.LoginUser(username);
 Self.ORAuthoriseResponse(Sender, rights, msg, user.fullName);
 ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);

 if ((rights > read) and (last_rights <= read)) then Self.AuthReadToWrite(Sender);
 if ((rights < write) and (last_rights >= write)) then Self.AuthWriteToRead(Sender);
end;//procedure

//ziskani stavu vsech bloku v panelu
procedure TOR.PanelFirstGet(Sender:TIdContext);
var addr:Integer;
    rights:TORControlRights;
begin
 rights := Self.PnlDGetRights(Sender);
 if (rights < read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 Blky.GetORBlk(Self.id, Sender);

 // zjistime RUC u vsech hnacich vozidel
 for addr := 0 to _MAX_ADDR-1 do
  if ((HVDb.HVozidla[addr] <> nil) and (HVDb.HVozidla[addr].Stav.stanice = Self)) then
    HVDb.HVozidla[addr].UpdateRuc(false);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//v panelu je kliknuto na urcity blok
procedure TOR.PanelClick(Sender:TIdContext; blokid:Integer; Button:TPanelButton; params:string = '');
var Blk:TBlk;
    i:Integer;
    rights:TORCOntrolRights;
begin
 //kontrola opravneni
 rights := Self.PnlDGetRights(Sender);
 if (rights < TORCOntrolRights.write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 if (Blky.GetBlkByID(blokid, Blk) <> 0) then Exit;

 // musime provest kontrolu, jestli OR ma povoleno menit blok
 // tj. jestli ma technologicky blok toto OR

 for i := 0 to Blk.OblsRizeni.Cnt-1 do
   if (Blk.OblsRizeni.ORs[i] = Self) then
    begin
     Blk.PanelClick(Sender, Self, Button, rights, params);
     Exit;
    end;

 ORTCPServer.SendInfoMsg(Sender, 'Nemáte oprávnìní mìnit tento blok');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelEscape(Sender:TIdContext);
var Blk:TBlk;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
//   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    // tady se schvalne neposila informace o chybe - aby klienta nespamovala chyba v momente, kdy provadi escape a nema autorizovana vsechna OR na panelu
   Exit;
  end;

 Self.ORDKClickClient();

 if (Self.vb.Count > 0) then
  begin
   (Self.vb[Self.vb.Count-1] as TBlkUsek).KonecJC := TZaver.no;
   Self.vb.Delete(Self.vb.Count-1);
  end else begin
   Blk := Blky.GetBlkSComZacatekVolba(Self.id);
   if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkScomVolba.none;
  end;

 Blk := Blky.GetBlkUsekVlakPresun(Self.id);
 if (Blk <> nil) then (Blk as TBlkUsek).VlakPresun := -1;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelNUZ(Sender:TIdContext);
var i,j:Integer;
    Blk:TBlk;
    podminky:TList<TPSPodminka>;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 podminky := TList<TPSPodminka>.Create();
 // zjisteni jmen bloku:
 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, Blk);
   if (Blk.typ <> _BLK_USEK) then continue;
   if (not (Blk as TBlkUsek).NUZ) then continue;

   for j := 0 to (Blk as TBlkUsek).OblsRizeni.Cnt-1 do
     if ((Blk as TBlkUsek).OblsRizeni.ORs[j] = Self) then
       podminky.Add(GetPSPodminka(Blk, 'Nouzové vybavování'));
  end;//for i

 ORTCPServer.Potvr(Sender, Self.NUZ_PS, Self, 'Nouzové uvolnìní závìrù úsekù', TBlky.GetBlksList(Self), podminky);
end;//procedure

procedure TOR.PanelNUZCancel(Sender:TIdContext);
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 Blky.NUZ(Self.id, false);
 Self.StopMereniCasu(Self.ORStav.NUZmerCasuID);
 Self.ORStav.NUZtimer := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelMessage(Sender:TIdContext; recepient:string; msg:string);
var orindex, return:Integer;
    tmp:TOR;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 orindex := ORs.GetORIndex(recepient);
 if (orindex < 0) then
  begin
   ORTCPServer.SendLn(Sender, Self.id + ';MSG-ERR;' + recepient + ';Tato OØ neexistuje');
   Exit();
  end;

 ORs.GetORByIndex(orindex, tmp);
 return := tmp.ORSendMsg(Self, msg);

 if (return = 1) then
   ORTCPServer.SendLn(Sender, Self.id + ';MSG-ERR;' + recepient + ';K této OØ aktuálnì není pøipojen žádný panel');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// pozadavek na ziskani sezmu hnacich vozidel
procedure TOR.PanelHVList(Sender:TIdContext);
var addr:Integer;
    str:string;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 str := Self.id + ';HV-LIST;{';
 for addr := 0 to _MAX_ADDR-1 do
   if ((Assigned(HVDb.HVozidla[addr])) and (HVDb.HVozidla[addr].Stav.stanice = Self)) then
    str := str + '[{' + HVDb.HVozidla[addr].GetPanelLokString(full) + '}]';
 str := str + '}';
 ORTCPServer.SendLn(Sender, str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// format dat soupravy: nazev;pocet_vozu;poznamka;smer_Lsmer_S;hnaci vozidla
procedure TOR.PanelSprChange(Sender:TIdContext; spr:TStrings);
begin
 if ((TTCPORsRef(Sender.Data).spr_new_usek_index = -1) and (TTCPORsRef(Sender.Data).spr_edit = nil)) then Exit();

 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 try
  if (TTCPORsRef(Sender.Data).spr_new_usek_index > -1) then
   Soupravy.AddSprFromPanel(spr, TTCPORsRef(Sender.Data).spr_usek, Self,
     (TTCPORsRef(Sender.Data).spr_new_usek_index))
  else begin
   // kontrola jestli je souprava porad na useku
   if ((TTCPORsRef(Sender.Data).spr_usek as TBlkUsek).IsSouprava(TTCPORsRef(Sender.Data).spr_edit.index)) then
     TTCPORsRef(Sender.Data).spr_edit.UpdateSprFromPanel(spr, TTCPORsRef(Sender.Data).spr_usek, Self)
   else begin
     ORTCPServer.SendLn(Sender, Self.id+';SPR-EDIT-ERR;Souprava již není na úseku');
     Exit();
   end;
  end;
 except
  on E: Exception do
   begin
    ORTCPServer.SendLn(Sender, Self.id+';SPR-EDIT-ERR;'+E.Message);
    Exit();
   end;
 end;

 TTCPORsRef(Sender.Data).spr_new_usek_index := -1;
 TTCPORsRef(Sender.Data).spr_edit := nil;
 TTCPORsRef(Sender.Data).spr_usek := nil;

 ORTCPServer.SendLn(Sender, Self.id+';SPR-EDIT-ACK;');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelMoveLok(Sender:TIdContext; lok_addr:word; new_or:string);
var n_or:Integer;
    new:TOR;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 n_or := ORs.GetORIndex(new_or);
 if (n_or < 0) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'Tato OR neexistuje!');
   Exit;
  end;
 if (not Assigned(HVDb.HVozidla[lok_addr])) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'HV '+IntToStr(lok_addr)+' neexistuje!');
   Exit;
  end;
 if (HVDb.HVozidla[lok_addr].Stav.souprava > -1) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'HV '+IntToStr(lok_addr)+' pøiøazeno soupravì '+Soupravy.GetSprNameByIndex(HVDb.HVozidla[lok_addr].Stav.souprava)+'!');
   Exit;
  end;
 if (HVDb.HVozidla[lok_addr].Stav.stanice <> Self) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'HV '+IntToStr(lok_addr)+' nepatøí této stanici!');
   Exit;
  end;

 ORs.GetORByIndex(n_or, new);
 HVDb.HVozidla[lok_addr].PredejStanici(new);
 ORTCPServer.SendInfoMsg(Sender, 'HV '+IntToStr(lok_addr)+' pøedáno stanici '+new.Name);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.Update();
var i:Integer;
begin
 Self.MTBUpdate();
 Self.stack.Update();

 //aktualizace mereni casu:
 for i := 0 to Self.MereniCasu.Count-1 do
  begin
   if (Now >= (Self.MereniCasu[i].Start + Self.MereniCasu[i].Length)) then
    begin
     if (Assigned(Self.MereniCasu[i].callback)) then
       Self.MereniCasu[i].callback(Self);
     Self.MereniCasu.Delete(i);
     break;
    end;
  end;//for i
end;//procedure

// vraci id pridaneho mereni
function TOR.AddMereniCasu(callback:TNotifyEvent; len:TDateTime):Byte;
var id:Integer;
    mc:TMereniCasu;
begin
 if (Self.MereniCasu.Count > 0) then
  id := Self.MereniCasu[Self.MereniCasu.Count-1].id+1
 else
  id := 0;

 // pridat mereni casu do vsech OR:
 Self.BroadcastData('CAS;START;'+IntToStr(id)+';'+FormatDateTime('s', len)+';');

 mc.Start    := Now;
 mc.Length   := len;
 mc.callback := callback;
 mc.id       := id;
 Self.MereniCasu.Add(mc);

 Result := id;
end;//function

procedure TOR.StopMereniCasu(id:Integer);
var i:Integer;
begin
 // pridat mereni casu do vsech OR:
 for i := 0 to Self.MereniCasu.Count-1 do
   if (Self.MereniCasu[i].id = id) then
     Self.MereniCasu.Delete(i);

 Self.BroadcastData('CAS;STOP;'+IntToStr(id)+';');
end;//function

////////////////////////////////////////////////////////////////////////////////

// zavola se, az probehne meerni casu:
procedure TOR.NUZTimeOut(Sender:TObject);
begin
 Blky.NUZ(Self.id);
 Self.ORStav.NUZtimer := false;
 Self.NUZblkCnt := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.NUZ_PS(Sender:TIdContext; success:boolean);
var i, j:Integer;
    JC:TJC;
    Blk, Nav:TBlk;
begin
 if (not success) then Exit;

 Self.ORStav.NUZtimer := true;

 // ruseni pripadnych jiznich cest:
 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, Blk);
   if (Blk.typ <> _BLK_USEK) then continue;
   if (not TBlkUsek(Blk).NUZ) then continue;

   for j := 0 to (Blk as TBlkUsek).OblsRizeni.Cnt-1 do
    begin
     if ((Blk as TBlkUsek).OblsRizeni.ORs[j] = Self) then
      begin
       JC := JcDb.GetJCByIndex(JCDb.FindPostavenaJCWithUsek(Blk.id));

       if (JC <> nil) then
        begin
         Blky.GetBlkByID(JC.data.NavestidloBlok, Nav);
         if (((Nav as TBlkSCom).Navest > 0) and ((Nav as TBlkSCom).DNjc = JC)) then
           ORTCPServer.BottomError(JC.stav.SenderPnl, 'Chyba povolovací návìsti '+Blky.GetBlkName(JC.data.NavestidloBlok),
                                   Self.ShortName, 'TECHNOLOGIE');
         JC.RusJCWithoutBlk();
         if ((Nav as TBlkSCom).DNjc = JC) then
           (Nav as TBlkSCom).DNjc := nil;
        end;
      end;
    end;//for j

  end;//for i

 Self.BroadcastData('NUZ;2;');

 Self.ORStav.NUZmerCasuID := Self.AddMereniCasu(Self.NUZTimeOut, EncodeTime(0, 0, 20, 0));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.ORAuthoriseResponse(Panel:TIdContext; Rights:TORControlRights; msg:string; username:string);
begin
 ORTCPServer.SendLn(Panel, Self.id+';AUTH;'+IntToStr(Integer(Rights))+';'+msg+';'+username);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.RemoveClient(Panel:TIdContext);
begin
 Self.PnlDRemove(Panel);
 Self.stack.OnDisconnect(Panel);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.SetNUZBlkCnt(new:Integer);
begin
 if ((Self.ORStav.NUZblkCnt = 0) and (new > 0)) then
  begin
   // zacina NUZ, informovat oblasti rizeni
   Self.BroadcastData('NUZ;1;');
  end;

 if ((Self.ORStav.NUZblkCnt > 0) and (new = 0)) then
  begin
   // nekdo si rekl, ze bloky nechce nuzovat
   Self.BroadcastData('NUZ;0;');
  end;

 Self.ORStav.NUZblkCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.SetZkratBlkCnt(new:Integer);
var i:Integer;
begin
 if (new < 0) then Exit(); 

 if ((new > 2) and (Self.ORStav.ZkratBlkCnt = 2)) then
  begin
   // V OR nastal zkrat -> prehrat zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_PRETIZENI, true);
  end;

 if ((new <= 2) and (Self.ORStav.ZkratBlkCnt = 2)) then
  begin
   // zkrat skoncil -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
     ORTCPServer.DeleteSound(Self.Connected[i].Panel, _SND_PRETIZENI);
  end;

 Self.ORStav.ZkratBlkCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.SetZadostBlkCnt(new:Integer);
var i:Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.ZadostBlkCnt = 0)) then
  begin
   // nastala zadost -> prehrat zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_TRAT_ZADOST, true);
  end;

 if ((new = 0) and (Self.ZadostBlkCnt > 0)) then
  begin
   // skocnila zadost -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
     ORTCPServer.DeleteSound(Self.Connected[i].Panel, _SND_TRAT_ZADOST);
  end;

 Self.ORStav.ZadostBlkCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.SetPrivolavackaBlkCnt(new:Integer);
var i:Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.PrivolavackaBlkCnt = 0)) then
  begin
   // aktivace prvni privolavaci navesti -> prehrat zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_PRIVOLAVACKA, true);
  end;

 if ((new = 0) and (Self.PrivolavackaBlkCnt > 0)) then
  begin
   // skocnila posledni privolavaci navest -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
     ORTCPServer.DeleteSound(Self.Connected[i].Panel, _SND_PRIVOLAVACKA);
  end;

 Self.ORStav.PrivolavackaBlkCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.SetTimerCnt(new:Integer);
var i:Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.TimerCnt = 0)) then
  begin
   // aktivace prvniho timeru -> prehrat zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_TIMEOUT, true);
  end;

 if ((new = 0) and (Self.TimerCnt > 0)) then
  begin
   // skocnil posledni timer -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
     ORTCPServer.DeleteSound(Self.Connected[i].Panel, _SND_TIMEOUT);
  end;

 Self.ORStav.timerCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.DisconnectPanels();
var i:Integer;
    index:Integer;
begin
 for i := Self.Connected.Count-1 downto 0 do
  begin
   Self.ORAuthoriseResponse(Self.Connected[i].Panel, TORControlRights.null, 'Odpojení systémù', '');
   index := (Self.Connected[i].Panel.Data as TTCPORsRef).index;
   Self.PnlDRemove(Self.Connected[i].Panel);
   ORTCPServer.GUIQueueLineToRefresh(index);
 end;

 Self.stack.ClearStack();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TOR.ORSendMsg(Sender:TOR; msg:string):Byte;
var i:Integer;
begin
 Result := 1;             // defaultne vracime chybu nedorucitelnosti
 for i := 0 to Self.Connected.Count-1 do
   if (Self.Connected[i].Rights >= TORControlRights.write) then
    begin
     ORTCPServer.SendLn(Self.Connected[i].Panel, Self.id + ';MSG;' + Sender.id + ';{'+msg+'}');
     Result := 0;
    end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.MTBClear();
var i:Integer;
begin
 for i := 0 to TRCS._MAX_RCS do
  Self.OR_MTB.MTBs[i].present := false;
end;//procedure

procedure TOR.MTBAdd(addr:integer);
begin
 try
   Self.OR_MTB.MTBs[addr].present := true;
 except

 end;
end;//procedure

procedure TOR.MTBFail(addr:integer);
begin
 try
   if (not Self.OR_MTB.MTBs[addr].present) then Exit();
   Self.OR_MTB.MTBs[addr].failed := true;
   Self.OR_MTB.failture := true;
   Self.OR_MTB.last_failture_time := Now;
 except

 end;
end;//procedure

procedure TOR.MTBUpdate();
var i:Integer;
    str:string;
begin
 if (not Self.OR_MTB.failture) then Exit();

 if ((Self.OR_MTB.last_failture_time + EncodeTime(0, 0, 0, 500)) < Now) then
  begin
   str := 'Výpadek MTB modulu ';
   for i := 0 to TRCS._MAX_RCS do
    if (Self.OR_MTB.MTBs[i].failed) then
     begin
      str := str + IntToStr(i) + ', ';
      Self.OR_MTB.MTBs[i].failed := false;
     end;

   str := LeftStr(str, Length(str)-2);
   Self.OR_MTB.failture := false;

   for i := 0 to Self.Connected.Count-1 do
     if (Self.Connected[i].Rights >= read) then
       ORTCPServer.BottomError(Self.Connected[i].Panel, str, Self.ShortName, 'TECHNOLOGIE');
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.UpdateLine(LI:TListItem);
var str:string;
  i: Integer;
begin
 LI.SubItems.Strings[0] := Self.Name;
 LI.SubItems.Strings[1] := Self.ShortName;
 LI.SubItems.Strings[2] := Self.id;
 str := Self.stack.GetList();
 LI.SubItems.Strings[3] := RightStr(str, Length(str)-2);

 case (Self.stack.volba) of
  TORStackVolba.PV : LI.SubItems.Strings[4] := 'PV';
  TORStackVolba.VZ : LI.SubItems.Strings[4] := 'VZ';
 end;

 str := '';
 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
   str := str + '(' + Self.ORProp.Osvetleni[i].name + ' - ' + IntToStr(Self.ORProp.Osvetleni[i].board) + ':' + IntToStr(Self.ORProp.Osvetleni[i].port) + ')';
 LI.SubItems.Strings[5] := str;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelZAS(Sender:TIdContext; str:TStrings);
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 Self.stack.ParseCommand(Sender, str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vrati stav jednoho osvetleni
function TOR.PanelGetOsv(index:Integer):boolean;
begin
 if ((index < 0) or (index >= Self.ORProp.Osvetleni.Count)) then Exit(false);

 try
   if (RCSi.GetOutput(Self.ORProp.Osvetleni[index].board, Self.ORProp.Osvetleni[index].port) = 1) then
     Result := true
   else
     Result := false;
 except
   Result := false;
 end;
end;

//  or;OSV;(code;stav)(code;srav) ...        - informace o stavu osvetleni (stav = [0,1])
procedure TOR.PanelSendOsv(Sender:TIdContext);
var i:Integer;
    str:string;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 str := Self.id + ';OSV;';
 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
   str := str + '[' + Self.ORProp.Osvetleni[i].name + '|' + IntToStr(PrevodySoustav.BoolToInt(Self.PanelGetOsv(i))) + ']';
 ORTCPServer.SendLn(Sender, str);
end;//procedure

procedure TOR.PanelSetOsv(Sender:TIdCOntext; id:string; state:boolean);
var i:Integer;
    osv:TOsv;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
  if (Self.ORProp.Osvetleni[i].name = id) then
   begin
    try
      RCSi.SetOutput(Self.ORProp.Osvetleni[i].board, Self.ORProp.Osvetleni[i].port, PrevodySoustav.BoolToInt(state));
      osv := Self.ORProp.Osvetleni[i];
      osv.default_state := state;
      Self.ORProp.Osvetleni[i] := osv;
    except

    end;

    Exit();
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.InitOsv();
var osv:TOsv;
begin
 try
   for osv in Self.ORProp.Osvetleni do
     if (RCSi.IsModule(osv.board)) then
       RCSi.SetOutput(osv.board, osv.port, PrevodySoustav.BoolToInt(osv.default_state));
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TOR.PanelGetSprs(Sender:TIdCOntext):string;
var i:Integer;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit('');
  end;

 Result := '{';
 for i := 0 to _MAX_SPR-1 do
   if ((Assigned(Soupravy.soupravy[i])) and (Soupravy.soupravy[i].stanice = Self)) then
    Result := Result + '[{' + Soupravy.soupravy[i].GetPanelString() + '}]';
 Result := Result + '}';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelRemoveSpr(Sender:TIDContext; spr_index:integer);
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 if ((Soupravy.soupravy[spr_index] <> nil) and (Soupravy.soupravy[spr_index].stanice = Self)) then
  begin
   Soupravy.RemoveSpr(spr_index);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava smazána');
   Exit();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelHVAdd(Sender:TIDContext; str:string);
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 try
   HVDb.Add(str, Self);
 except
   on e:Exception do
    begin
     ORTCPServer.SendInfoMsg(Sender, e.Message);
     Exit();
    end;
 end;

 ORTCPServer.SendInfoMsg(Sender, 'Loko pøidáno');
end;//procedure

procedure TOR.PanelHVRemove(Sender:TIDContext; addr:Integer);
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;
 if (HVDb.HVozidla[addr] = nil) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'Loko neexsituje');
   Exit();
  end;
 if (HVDb.HVozidla[addr].Stav.stanice <> self) then
  begin
   ORTCPServer.SendInfoMsg(Sender, 'Loko se nenachází ve stanici '+Self.Name);
   Exit();
  end;

 try
   HVDb.Remove(addr);
 except
   on E:Exception do
     ORTCPServer.SendInfoMsg(Sender, 'Nelze smazat loko - '+E.Message)
 end;

 ORTCPServer.SendInfoMsg(Sender, 'Loko '+IntToStr(addr)+' smazáno');
end;//procedure

procedure TOR.PanelHVEdit(Sender:TIDContext; str:string);
var data:TStrings;
    addr:Integer;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 data := nil;
 try
   data := TStringList.Create();
   ExtractStringsEx(['|'], [], str, data);
   addr := StrToInt(data[4]);
   data.Free();
   if (HVDb.HVozidla[addr] = nil) then
    begin
     ORTCPServer.SendInfoMsg(Sender, 'Loko neexistuje');
     Exit();
    end;
   if (HVDb.HVozidla[addr].Stav.stanice <> self) then
    begin
     ORTCPServer.SendInfoMsg(Sender, 'Loko se nenachází ve stanici '+Self.Name);
     Exit();
    end;

   HVDb.HVozidla[addr].UpdateFromPanelString(str);

   if ((HVDb.HVozidla[addr].Slot.prevzato) and (not HVDb.HVozidla[addr].Slot.stolen)) then
     TrkSystem.LokSetFunc(Self, HVDb.HVozidla[addr], HVDb.HVozidla[addr].Stav.funkce);
 except
   on e:Exception do
    begin
     ORTCPServer.SendInfoMsg(Sender, e.Message);
     if (Assigned(data)) then data.Free();
     Exit();
    end;
 end;

 ORTCPServer.SendInfoMsg(Sender, 'Loko '+IntToStr(addr)+' aktualizováno');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.BroadcastData(data:string; min_rights:TORControlRights = read);
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
   if (Self.Connected[i].Rights >= min_rights) then
    ORTCPServer.SendLn(Self.Connected[i].Panel, Self.id+';'+data);
end;//procedure

procedure TOR.BroadcastGlobalData(data:string; min_rights:TORControlRights = read);
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
   if (Self.Connected[i].Rights >= min_rights) then
    ORTCPServer.SendLn(Self.Connected[i].Panel, '-;'+data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.ORDKClickServer(callback:TBlkCallback);
begin
 Self.ORStav.dk_click_callback := callback;
 Self.BroadcastData('DK-CLICK;1', TORControlRights.write);
end;//procedure

procedure TOR.ORDKClickClient();
begin
 if (not Assigned(Self.ORStav.dk_click_callback)) then Exit();

 Self.ORStav.dk_click_callback := nil;
 Self.BroadcastData('DK-CLICK;0', TORControlRights.write);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelDKClick(SenderPnl:TIdContext; Button:TPanelButton);
begin
 if (Assigned(Self.ORStav.dk_click_callback)) then
   Self.ORStav.dk_click_callback(SenderPnl, Self, Button);
end;//procedure

// Tato procedura parsuje "LOK-REQ" z panelu.
procedure TOR.PanelLokoReq(Sender:TIdContext; str:TStrings);
var data:TStrings;
    i, j, addr:Integer;
    HV:THV;
    rights:TORControlRights;
    line:string;
    Blk:TBlk;
    spri:Integer;
begin
//  or;LOK-REQ;PLEASE;addr1|addr2|...       - zadost o vydani tokenu
//  or;LOK-REQ;PLEASE-U;blk_id              - zadost o vydani tokenu pro vozidla soupravy na danem techologickem bloku
//  or;LOK-REQ;LOK;addr1|addr2|...          - lokomotivy pro rucni rizeni na zaklade PLEASE regulatoru vybrany
//  or;LOK-REQ;DENY;                        - odmitnuti pozadavku na rucni rizeni

 //kontrola opravneni klienta
 rights := Self.PnlDGetRights(Sender);
 if (rights < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 str[2] := UpperCase(str[2]);

 // zadost o vydani tokenu
 // odpovedi:
  //  or;LOK-TOKEN;OK;[addr|token][addr|token] - odpovìï na žádost o token, je posílano také pøi RUÈ loko
  //  or;LOK-TOKEN;ERR;addr1|addr2...;comment  - chybova odpoved na zadost o token
 if (str[2] = 'PLEASE') then
  begin
   // parsing loko
   try
     data := TStringList.Create();
     ExtractStringsEx(['|'], [], str[3], data);

     // zkontrolujeme vsechna LOKO
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb.HVozidla[StrToInt(data[i])];
       // kontrola existence loko
       if (HV = nil) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' neexistuje');
         Exit();
        end;

       // kontrola, zda se loko nachazi u me ve stanici
       // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
       if ((HV.Stav.stanice <> Self) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' se nenachází ve stanici');
         Exit();
        end;

       // nelze vygenerovat token pro loko, ktere je uz v regulatoru
       if ((HV.Stav.regulators.Count > 0) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' již otevøeno v regulátoru');
         Exit();
        end;
      end;//for i

     // kontrola OK -> generujeme zpravu z tokeny a zpravu odesleme
     line := Self.id+';LOK-TOKEN;OK;';
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb.HVozidla[StrToInt(data[i])];
       line := line + '[' + IntToStr(HV.adresa) + '|' + HV.GetToken() + ']';
      end;//for i
     ORTCPServer.SendLn(Sender, line);

     data.Free();
   except
     ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;Neplatný formát argumentù');
   end;
  end

 // klient vybral lokomotivy pro rucni rizeni
 // odpovedi, ktere muzu poslat panelu:
 //  or;LOK-REQ;OK                           - seznam loko na rucni rizeni schvalen serverem
 //  or;LOK-REQ;ERR;comment                  - seznam loko na rucni rizeni odmitnut serverem
 else if (str[2] = 'LOK') then
  begin
   try
     // nejdriv musi probihat zadost o loko
     if (Self.ORStav.reg_please = nil) then
      begin
       ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;ERR;Neprobíhá žádná žádost z regulátoru');
       Exit();
      end;

     data := TStringList.Create();
     ExtractStringsEx(['|'], [], str[3], data);

     // zkontrolujeme vsechna LOKO
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb.HVozidla[StrToInt(data[i])];
       // kontrola existence loko
       if (HV = nil) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;ERR;Loko '+data[i]+' neexistuje');
         Exit();
        end;

       // kontrola, zda se loko nachazi u me ve stanici
       // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
       if ((HV.Stav.stanice <> Self) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;ERR;Loko '+data[i]+' se nenachází ve stanici');
         Exit();
        end;

       // nelze vygenerovat token pro loko, ktere je uz v regulatoru
       if ((HV.Stav.regulators.Count > 0) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;ERR;Loko '+data[i]+' již otevøeno v regulátoru');
         Exit();
        end;
      end;//for i


     // kontrola OK -> odesleme panelu zpravu o tom, ze je vse OK
     ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;OK;');

     // vsem ostatnim panelum jeste posleme, ze doslo ke zruseni zadosti
     for i := 0 to Self.Connected.Count-1 do
       if ((Self.Connected[i].Rights >= TORControlRights.read) and (Self.Connected[i].Panel <> Sender)) then
        ORTCPServer.SendLn(Self.Connected[i].Panel, Self.id+';LOK-REQ;CANCEL;');

     // lokomotivy priradime regulatoru
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb.HVozidla[StrToInt(data[i])];
       TCPRegulator.LokToRegulator(Self.ORStav.reg_please, HV);
      end;//for i

     // zrusit zadost regulatoru
     (Self.ORStav.reg_please.Data as TTCPORsRef).regulator_zadost := nil;
     Self.ORStav.reg_please := nil;

     data.Free();
   except
     ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;ERR;Neplatný formát argumentù');
   end;
  end

 // relief odmitl zadost regulatoru o lokomotivu
 else if (str[2] = 'DENY') then
  begin
   ORTCPServer.SendLn(Self.ORStav.reg_please, '-;LOK;G;PLEASE-RESP;err;Dispeèer odmítl žádost');
   Self.BroadcastData('LOK-REQ;CANCEL;');
   (Self.ORStav.reg_please.Data as TTCPORsRef).regulator_zadost := nil;
   Self.ORStav.reg_please := nil;
  end

//  or;LOK-REQ;U-PLEASE;blk_id;spr_index      - zadost o vydani seznamu hnacich vozidel na danem useku
//  mozne odpovedi:
//    or;LOK-REQ;U-OK;[hv1][hv2]...           - seznamu hnacich vozidel v danem useku
//    or;LOK-REQ;U-ERR;info                   - chyba odpoved na pozadavek na seznam loko v danem useku

 else if (str[2] = 'U-PLEASE') then
  begin
   try
     Blky.GetBlkByID(StrToInt(str[3]), Blk);
     if ((Blk = nil) or ((Blk.typ <> _BLK_USEK) and (Blk.typ <> _BLK_TU))) then
      begin
       ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Neplatný blok');
       Exit();
      end;

     if (not (Blk as TBlkUsek).IsSouprava()) then
      begin
       ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Žádná souprava na bloku');
       Exit();
      end;

     spri := -1;
     if (str.Count > 4) then
      begin
       spri := StrToIntDef(str[4], -1);
       if ((spri < -1) or (spri >= (Blk as TBlkUsek).Soupravs.Count)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Tato souprava na úseku neexistuje');
         Exit();
        end;
      end;

     // generujeme zpravu s tokeny
     line := Self.id+';LOK-REQ;U-OK;{';
     if (spri = -1) then
      begin
       // vsechny soupravy na useku
       for j := 0 to (Blk as TBlkUsek).Soupravs.Count-1 do
         for addr in Soupravy.soupravy[(Blk as TBlkUsek).Soupravs[j]].HVs do
           line := line + '[{' + HVDb.HVozidla[addr].GetPanelLokString() + '}]';
      end else begin
       // konkretni souprava
       for addr in Soupravy.soupravy[(Blk as TBlkUsek).Soupravs[spri]].HVs do
         line := line + '[{' + HVDb.HVozidla[addr].GetPanelLokString() + '}]';
      end;

     line := line + '}';
     ORTCPServer.SendLn(Sender, line);

   except
     ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Neplatný formát argumentù');
   end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// odesle status oblasti rizeni po prihlaseni klienta
procedure TOR.SendStatus(panel:TIdContext);
var user:TUser;
begin
 // kliknuti na dopravni kancelar
 if (Assigned(Self.ORStav.dk_click_callback)) then
   ORTCPServer.SendLn(panel, Self.id+';DK-CLICK;1')
 else
   ORTCPServer.SendLn(panel, Self.id+';DK-CLICK;0');

 // pripradna zadost o lokomotivu
 if (Self.reg_please <> nil) then
  begin
   user := (Self.reg_please.Data as TTCPORsRef).regulator_user;
   if (user <> nil) then
     ORTCPServer.SendLn(panel, Self.id+';LOK-REQ;REQ;'+user.id+';'+user.firstname+';'+user.lastname+';');
  end;

 if ((Assigned(Self.hlaseni)) and (Self.hlaseni.available)) then
   ORTCPServer.SendLn(panel, Self.id+';SHP;AVAILABLE;1');

 if ((Self.NUZblkCnt > 0) and (not Self.NUZtimer)) then
   ORTCPServer.SendLn(panel, Self.id + ';NUZ;1;');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.ClearVb();
var i:Integer;
begin
 for i := 0 to Self.vb.Count-1 do
  (Self.vb[i] as TBlkUsek).KonecJC := TZaver.no;
 Self.vb.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TOR.GetORPanel(conn:TIdContext; var ORPanel:TORPanel):Integer;
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
   if (Self.Connected[i].Panel = conn) then
    begin
     ORPanel := Self.Connected[i];
     Exit(0);
    end;
 Result := 1;
end;//fucction

////////////////////////////////////////////////////////////////////////////////

class function TOR.GetRightsString(rights:TORControlRights):string;
begin
 case (rights) of
  TORControlRights.null      : Result := 'null';
  TORControlRights.read      : Result := 'read';
  TORControlRights.write     : Result := 'write';
  TORControlRights.superuser : Result := 'superuser';
 else
  Result := '';
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

// je volano v pripade, ze dojde ke zmene opravenni za behu programu
procedure TOR.UserUpdateRights(user:TObject);
var i:Integer;
    rights:TORControlRights;
begin
 for i := 0 to Self.Connected.Count-1 do
  begin
   // je pripojeny uzivatel s vyssimi opravevnimi, nez jsou mu pridelena?
   rights := TUser(user).GetRights(Self.id);
   if ((Self.Connected[i].user = TUser(user).id) and ((Self.Connected[i].Rights > rights) or (TUser(user).ban))) then
    begin
     if (TUser(user).ban) then rights := TORControlRights.null;
     Self.PnlDAdd(Self.Connected[i].Panel, rights, TUser(user).id);
     Self.ORAuthoriseResponse(Self.Connected[i].Panel, rights, 'Snížena oprávnìní uživatele', '');
    end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.UserDelete(userid:string);
var i:Integer;
begin
 for i := Self.Connected.Count-1 downto 0 do
  begin
   if (Self.Connected[i].user = userid) then
    begin
     Self.ORAuthoriseResponse(Self.Connected[i].Panel, TORControlRights.null, 'Uživatel smazán', '');
     Self.PnlDRemove(Self.Connected[i].Panel);
    end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vraci 1 pokud zadost jiz probiha
// vraci 0 pokud prikaz probehl vporadku
function TOR.LokoPlease(Sender:TIDContext; user:TObject; comment:string):Integer;
var str:string;
begin
 if (Self.ORStav.reg_please <> nil) then Exit(1);
 Self.ORStav.reg_please := Sender;

 //format: or;LOK-REQ;REQ;username;firstname;lastname;comment
 str := 'LOK-REQ;REQ;'+TUser(user).id+';';
 if (TUser(user).firstname <> '') then str := str + TUser(user).firstname + ';' else str := str + '-;';
 if (TUser(user).lastname <> '') then str := str + TUser(user).lastname + ';' else str := str + '-;';
 if (comment <> '') then str := str + comment + ';' else str := str + '-;';

 Self.BroadcastData(str);

 Result := 0;
end;//procedure

procedure TOR.LokoCancel(Sender:TIdContext);
begin
 if (Self.ORStav.reg_please = nil) then Exit();
 Self.ORStav.reg_please := nil;
 //format: or;LOK-REQ;CANCEL;
 Self.BroadcastData('LOK-REQ;CANCEL;');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.AuthReadToWrite(panel:TIdContext);
begin
 if (Self.ZkratBlkCnt > 2) then ORTCPServer.PlaySound(panel, _SND_PRETIZENI, true);
 if (Self.ZadostBlkCnt > 0) then ORTCPServer.PlaySound(panel, _SND_TRAT_ZADOST, true);
 if (Self.PrivolavackaBlkCnt > 0) then ORTCPServer.PlaySound(panel, _SND_PRIVOLAVACKA, true);
 if (Self.TimerCnt > 0) then ORTCPServer.PlaySound(panel, _SND_TIMEOUT, true);
end;//procedure

procedure TOR.AuthWriteToRead(panel:TIdContext);
begin
 if (Self.ZkratBlkCnt > 2) then ORTCPServer.DeleteSound(panel, _SND_PRETIZENI);
 if (Self.ZadostBlkCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_TRAT_ZADOST);
 if (Self.PrivolavackaBlkCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_PRIVOLAVACKA);
 if (Self.TimerCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_TIMEOUT);
 Self.stack.OnWriteToRead(panel);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

class function TOR.ORRightsToString(rights:TORControlRights):string;
begin
 case (rights) of
  null      : Result := 'žádná oprávnìní';
  read      : Result := 'oprávnìní ke ètení';
  write     : Result := 'oprávnìní k zápisu';
  superuser : Result := 'superuser';
 else
  Result := '';
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TOR.GetPSPodminka(blok:TObject; podminka:string):TPSPodminka;
begin
 Result.blok     := blok;
 Result.podminka := podminka;
end;//function

class function TOR.GetPSPodminky(podm:TPSPodminka):TPSPodminky;
begin
 Result := TList<TPSPodminka>.Create();
 Result.Add(podm);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TOR.OnHlaseniAvailable(Sender:TObject; available:boolean);
begin
 if (available) then
   Self.BroadcastData('SHP;AVAILABLE;1')
 else
   Self.BroadcastData('SHP;AVAILABLE;0');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelHlaseni(Sender:TIDContext; str:TStrings);
begin
 if (not Assigned(Self.hlaseni)) then Exit();
 if (str.Count < 3) then Exit();

 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 str[2] := UpperCase(str[2]);

 if (str[2] = 'SPEC') then begin
   Self.hlaseni.Spec(str[3]);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
