unit TBlokSCom;

//definice a obsluha technologickeho bloku SCom
// Pritomnost redukce menu u bloku navestidla znamena, ze z tohoto bloku nelze zacit jizdni cestu.

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, rrEvent,
      TechnologieJC, IdContext, Generics.Collections, THnaciVozidlo,
      TOblRizeni, StrUtils, JsonDataObjects;

type
 TBlkSComVolba = (none = 0, VC = 1, PC = 2, NC = 3, PP = 4);
 TBlkSComOutputType = (scom = 0, binary = 1);

 ENoEvents = class(Exception);

 // zastaovoaci a zpomalovaci udalost pro jeden typ soupravy a jeden rozsah delek soupravy
 TBlkSComSprEvent = record
  spr_typ:TStrings;                             // tato udalost je brana v potaz, pokud ma souprava jeden ze zde definovancyh typu (MOs, Os, Mn, Pn, ...)
  delka:record                                  // tato udalost je brana v potaz, pokud ma souprava delku > min && < max
    min:Integer;
    max:Integer;
  end;

  zastaveni: TRREv;                             // zastavovaci udalost
  zpomaleni: record                             // zpomalovaci udalost
    enabled: boolean;                             // povolena zpomalovaci udalost?
    speed: Integer;                               // rychlost z km/h (40, 50, 60...)
    ev: TRREv;                                    // udalost
  end;
 end;

 // vlastnosti bloku SCom, ktere se ukladaji do databaze bloku
 TBlkSComSettings = record
  RCSAddrs:TRCSAddrs;                            // ve skutecnosti je signifikantni jen jedna adresa - na indexu [0], coz je vystup pro rizeni navestidla
  OutputType:TBlkSComOutputType;                 // typ vystupu: binarni/SCom
  events:TList<TBlkSComSprEvent>;                // tady jsou ulozena veskera zastavovani a zpomalovani; zastaveni na indexu 0 je vzdy primarni
                                                 // program si pamatuje vice zastavovacich a zpomalovaich udalosti pro ruzne typy a delky soupravy
  ZpozdeniPadu:Integer;                          // zpozdeni padu navestidla v sekundach (standartne 0)
  zamknuto:boolean;                              // jestli je navestidlo trvale zamknuto na STUJ (hodi se napr. u navestidel a konci kusych koleji)
 end;

 // stav bloku SCOm
 TBlkSComStav = record
  ZacatekVolba:TBlkSComVolba;                    // nazatek volby jidni cesty
  Navest:Integer;                                // aktualni navest dle kodu SCom; pokud je vypla komunikace, -1
  cilova_navest:Integer;                         // navest, ktera ma byt nastavena
  navest_old:Integer;                            // behem staveni obsahuje byvalou navest
  ABJC:TJC;                                      // odkaz na automaticky stavenou JC
  ZAM:Boolean;                                   // navestidlo zamkle z panelu
  redukce_menu:Integer;                          // kolik blokù mì redukuje
  dn_jc_ref,privol_ref:TJC;                      // reference na aktualni JC na navestidle (resp. NC)

  padani:boolean;                                // zde je true, pokud navestidlo pada do STUJ (ma zpozdeny pad) a jeste nespadlo
  padani_start:TDateTime;                        // start padani

  privol_start:TDateTime;                        // start privolavaci navesti (privolavacka sviti pouze omezeny cas a pak se vypne)
  privol_timer_id:Integer;                       // id timeru ukonceni privolavacky v panelu, ze ktreho byla JC postavena
  autoblok:boolean;

  toRnz:TDictionary<Integer, Cardinal>;          // seznam bloku k RNZ spolu s pocty ruseni, ktere je treba udelat
  RCtimer:Integer;                               // id timeru, ktery se prave ted pouziva pro ruseni JC
  changeCallbackOk, changeCallbackErr: TNotifyEvent; // notifikace o nastaveni polohy navestidla
  changeEnd: TTime;
 end;

 // vlastnosti navestidla ziskane ze souboru .spnl (od reliefu, resp. z Mergeru)
 TBlkSComRel = record
  SymbolType:Byte;                               // typ navestidla: 0 = VC&PC, 1=PC
  UsekID:Integer;                                // ID useku pred navestidlem
  smer:THVStanoviste;                            // smer navetidla (lichy X sudy)
 end;

 // Blok SCOm (Navestidlo)
 TBlkSCom = class(TBlk)
  const
   //defaultni stav
   _def_scom_stav:TBlkSComStav = (
     ZacatekVolba : none;
     Navest : -1;
     ABJC : nil;
     ZAM : false;
     dn_jc_ref : nil;
     privol_ref : nil;
     privol_timer_id : 0;
     autoblok : false;
     RCtimer : -1;
     changeCallbackOk: nil;
     changeCallbackErr: nil;
   );

   // privolavaci navest sviti jednu minitu a 30 sekund
   _PRIVOL_MIN = 1;
   _PRIVOL_SEC = 30;

   // Kody navesti
   _NAV_CHANGING = -2;
   _NAV_DISABLED = -1;
   _NAV_STUJ = 0;
   _NAV_VOLNO = 1;
   _NAV_VYSTRAHA = 2;
   _NAV_OCEK_40 = 3;
   _NAV_VOLNO_40 = 4;
   _NAV_VSE = 5;
   _NAV_VYSTRAHA_40 = 6;
   _NAV_40_OCEK_40 = 7;
   _NAV_PRIVOL = 8;
   _NAV_POSUN_ZAJ = 9;
   _NAV_POSUN_NEZAJ = 10;
   _NAV_OPAK_VOLNO = 11;
   _NAV_OPAK_VYSTRAHA = 12;
   _NAV_ZHASNUTO = 13;
   _NAV_OPAK_OCEK_40 = 14;
   _NAV_OPAK_VYSTRAHA_40 = 15;

   _NAV_DEFAULT_DELAY = 2;
   _NAV_CHANGE_DELAY_MSEC = 1000;
   _NAV_CHANGE_SHORT_DELAY_MSEC = 200;

  private
   SComSettings:TBlkSComSettings;
   SComStav:TBlkSComStav;
   SComRel:TBlkSComRel;

   fUsekPred:TBlk;
   lastEvIndex:Integer;

    function ParseEvent(str:string; old:boolean):TBlkSComSprEvent;
    function ParseSprTypes(str:string):TStrings;
    function ParseOldRychEvent(str:string):TRREv;

    function GetSprTypes(sl:TStrings):string;
    function GetEvent(ev:TBlkSComSprEvent; short:boolean = false):string;
    function RCinProgress():boolean;

    procedure SetNavest(navest:Integer); overload;

    function GetAB():boolean;
    procedure SetAB(ab:boolean);
    procedure SetABJC(ab:TJC);

    procedure SetZacatekVolba(typ:TBlkSComVolba);
    procedure SetZAM(zam:boolean);

    // obsluha polozek v menu panelu
    procedure MenuVCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTUJClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuDNClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRCClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuABStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuABStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuLockClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUnlockClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPNStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPNStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);

    // DEBUG menu:
    procedure MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);
    procedure MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure UpdatePadani();
    procedure UpdatePrivol();
    procedure UpdateNavestSet();
    procedure OnNavestSetOk();
    procedure OnNavestSetError();

    procedure PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
    procedure PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
    procedure RNZPotvrSekv(Sender:TIdContext; success:boolean);

    procedure SetUsekPredID(new_id:Integer);

    function CurrentEventIndex():Integer;

    function GetUsekPred():TBlk;
    function CanIDoRNZ():boolean;

    procedure UnregisterAllEvents();
    function IsChanging():boolean;

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    function IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean; overload;
    class function IsPovolovaciNavest(Navest:Integer; jctype:TJCType = TJCType.vlak):boolean; overload;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    procedure JCZrusNavest();   // zahrnuje cas na pad navesti
    procedure SetNavest(navest:Integer; changeCallbackOk, changeCallbackErr: TNotifyEvent); overload;

    //----- SCom own functions -----

    function GetSettings():TBlkSComSettings;
    procedure SetSettings(data:TBlkSComSettings);

    procedure RedukujMenu();
    procedure ZrusRedukciMenu();

    procedure UpdateRychlostSpr(force:boolean = false);
    procedure AddBlkToRnz(blkId:Integer; change:boolean = true);
    procedure RemoveBlkFromRnz(blkId:Integer);
    procedure RCtimerTimeout();

    function GetSoupravaIndex(usek:TBlk = nil):Integer;
    procedure PropagatePOdjToTrat();

    class function NavestToString(navest:Integer):string;

    property SymbolType:Byte read SComRel.SymbolType;
    property UsekID:Integer read SComRel.UsekID write SetUsekPredID;
    property Smer:THVStanoviste read SComRel.smer write SComRel.smer;

    //stavove promenne
    property Navest:Integer read SComStav.Navest write SetNavest;
    property ZacatekVolba:TBlkSComVolba read SComStav.ZacatekVolba write SetZacatekVolba;
    property AB:boolean read GetAB write SetAB;
    property ABJC:TJC read SComStav.ABJC write SetABJC;
    property ZAM:boolean read SComStav.ZAM write SetZAM;
    property Lichy:THVStanoviste read SComRel.Smer;
    property DNjc:TJC read SComStav.dn_jc_ref write SComStav.dn_jc_ref;
    property privol:TJC read SComStav.privol_ref write SComStav.privol_ref;
    property UsekPred:TBlk read GetUsekPred;
    property autoblok:boolean read SComStav.autoblok write SComStav.autoblok;
    property canRNZ:boolean read CanIDoRNZ;
    property RCtimer:Integer read SComStav.RCtimer write SComStav.RCtimer;
    property changing:Boolean read IsChanging;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;

    //PT:

    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;

 end;//class TBlkSCom

//format dat S-Scomu v souboru *.ini:
//  ev=udalosti zastavovani a zpomalovani
//  OutType=typ vystupu (scom, binarni)
//  zamknuti=zamknuti navestidla trvale do STUJ

// format ev: (ev1)(ev2)(ev3)
// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|spr_typ1;spr_typ2;spr_typ3;...|min_delka|max_delka
//      spr_typ[1..n], min_delka a max_delka jsou u eventu 0 (globalniho eventu) vynechany
//      vsechny dalsi eventy jsou specificke -> vyse zminene informace v nich jsou ulozeny

//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
//   typ_zastaveni(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TechnologieRCS, TBloky, TBlokUsek, TJCDatabase, TCPServerOR,
      GetSystems, Logging, SprDb, Souprava, TBlokIR, Zasobnik, ownStrUtils,
      TBlokTratUsek, TBlokTrat, TBlokVyhybka, TBlokZamek, TechnologieAB,
      predvidanyOdjezd;

constructor TBlkSCom.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_SCOM;
 Self.SComStav := Self._def_scom_stav;
 Self.SComStav.toRnz := TDictionary<Integer, Cardinal>.Create();
 Self.SComSettings.events := TList<TBlkSComSprEvent>.Create();
 Self.fUsekPred := nil;
end;//ctor

destructor TBlkSCom.Destroy();
var i:Integer;
begin
 Self.SComStav.toRnz.Free();
 for i := 0 to Self.SComSettings.events.Count-1 do
  begin
   Self.SComSettings.events[i].zastaveni.Free();
   if (Assigned(Self.SComSettings.events[i].zpomaleni.ev)) then
     Self.SComSettings.events[i].zpomaleni.ev.Free();
  end;
 Self.SComSettings.events.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
    i:Integer;
    s:string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.SComSettings.RCSAddrs := Self.LoadRCS(ini_tech, section);

 Self.SComSettings.zamknuto     := ini_tech.ReadBool(section, 'zamknuti', false);

 Self.SComSettings.OutputType   := TBlkSComOutputType(ini_tech.ReadInteger(section, 'OutType', 0));
 Self.SComSettings.ZpozdeniPadu := ini_tech.ReadInteger(section, 'zpoz', _NAV_DEFAULT_DELAY);

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   try
     ExtractStrings([';'],[],PChar(ini_rel.ReadString('N',IntToStr(Self.GlobalSettings.id),'')),str);
     if (str.Count >= 3) then
      begin
       if (Self.ORsRef <> nil) then
         Self.ORsRef.Free();
       Self.ORsRef := ORs.ParseORs(str[0]);
       Self.SComRel.SymbolType  := StrToInt(str[1]);

       // 0 = navestidlo v lichem smeru. 1 = navestidlo v sudem smeru
       if (str[2] = '0') then
         Self.SComRel.smer := THVStanoviste.lichy
       else
         Self.SComRel.smer := THVStanoviste.sudy;

       Self.SComRel.UsekID      := StrToInt(str[3]);
      end else begin
       Self.SComRel.SymbolType := 0;
       Self.SComRel.smer       := THVStanoviste.lichy;
       Self.SComRel.UsekID     := -1;
      end;
   finally
     str.Free();
   end;
  end else begin
    Self.ORsRef.Clear();
  end;

 // Nacitani zastavovacich a zpomaovacich udalosti
 // toto musi byt az po nacteni spnl
 Self.SComSettings.events.Clear();

 s := ini_tech.ReadString(section, 'ev', '');
 if (s <> '') then
  begin
   // 1) stary zpusob nacitani zastavovacich udalosti (vsechny udalosti ve starem
   //    formatu na jednom radku). Tohle je tady hlavne kvuli zpetne kompatibilite.
   str := TStringList.Create();
   try
     ExtractStrings(['(', ')'], [], PChar(s), str);
     for i := 0 to str.Count-1 do
       Self.SComSettings.events.Add(Self.ParseEvent(str[i], true));
   finally
     str.Free();
   end;

  end else begin
   // 2) novy zpusob nacitani zastavovacich udalosti
   //    kazda udalost na samostatnem radku ev1=... ev2=... ...
   i := 0;
   s := ini_tech.ReadString(section, 'ev'+IntToStr(i), '');
   while (s <> '') do
    begin
     Self.SComSettings.events.Add(Self.ParseEvent(s, false));
     Inc(i);
     s := ini_tech.ReadString(section, 'ev'+IntToStr(i), '');
    end;
  end;

 PushRCSToOR(Self.ORsRef, Self.SComSettings.RCSAddrs);
end;

procedure TBlkSCom.SaveData(ini_tech:TMemIniFile;const section:string);
var i:Integer;
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.SComSettings.RCSAddrs);

 for i := 0 to Self.SComSettings.events.Count-1 do
   ini_tech.WriteString(section, 'ev'+IntToStr(i),
      Self.GetEvent(Self.SComSettings.events[i], (i = 0)));

 if (Self.SComSettings.RCSAddrs.Count > 0) then
   ini_tech.WriteInteger(section, 'OutType', Integer(Self.SComSettings.OutputType));

 if (Self.SComSettings.ZpozdeniPadu <> _NAV_DEFAULT_DELAY) then
   ini_tech.WriteInteger(section, 'zpoz', Self.SComSettings.ZpozdeniPadu);

 if (Self.SComSettings.zamknuto) then
   ini_tech.WriteBool(section, 'zamknuti', Self.SComSettings.zamknuto);
end;

procedure TBlkSCom.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Enable();
begin
 try
   if ((Self.SComSettings.RCSAddrs.Count > 0) and
       (not RCSi.IsModule(Self.SComSettings.RCSAddrs[0].board))) then
     Exit();
 except
   Exit();
 end;

 Self.SComStav.Navest := _NAV_STUJ;
 Self.SComStav.navest_old := _NAV_STUJ;
 Self.SComStav.toRnz.Clear();
 Self.UnregisterAllEvents();
 Self.Change();
end;

procedure TBlkSCom.Disable();
begin
 Self.SComStav.Navest := _NAV_DISABLED;
 Self.SComStav.navest_old := _NAV_DISABLED;
 Self.SComStav.ZacatekVolba := TBlkSComVolba.none;
 Self.AB := false;
 Self.SComStav.ZAM  := false;
 Self.SComStav.toRnz.Clear();
 Self.SComStav.RCtimer := -1;
 Self.UnregisterAllEvents();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Update();
begin
 Self.UpdateNavestSet();
 Self.UpdatePadani();
 Self.UpdateRychlostSpr();

 if (Self.Navest = 8) then
   Self.UpdatePrivol();

 if (Self.SComSettings.RCSAddrs.Count > 0) then
  begin
   if ((RCSi.IsModule(Self.SComSettings.RCSAddrs[0].board)) and
       (not RCSi.IsModuleFailure(Self.SComSettings.RCSAddrs[0].board))) then
    begin
     if (Self.SComStav.Navest = _NAV_DISABLED) then
      begin
       Self.SComStav.Navest := _NAV_STUJ;
       Self.Change();
      end;
    end else begin
     if (Self.changing) then
       Self.OnNavestSetError();

     if (Self.SComStav.Navest >= _NAV_STUJ) then
      begin
       Self.SComStav.Navest := _NAV_DISABLED;
       JCDb.RusJC(Self);
       Self.Change();
      end;
    end;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Change(now:boolean = false);
begin
 // zmenu navesti propagujeme do prilehle trati, kde by mohlo dojit ke zmene
 // navesti autobloku
 if ((Self.UsekPred <> nil) and (Self.UsekPred.typ = _BLK_TU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
   Self.UsekPred.Change();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.GetSettings():TBlkSComSettings;
begin
 Result := Self.SComSettings;
end;

procedure TBlkSCom.SetSettings(data:TBlkSComSettings);
var ev:TBlkSComSprEvent;
begin
 if (Self.SComSettings.events <> data.events) then
  begin
   // destrukce starych dat
   for ev in Self.SComSettings.events do
    begin
     ev.zastaveni.Free();
     if (Assigned(ev.zpomaleni.ev)) then
       ev.zpomaleni.ev.Free();
    end;
   Self.SComSettings.events.Free();
  end;

 if (Self.SComSettings.RCSAddrs <> data.RCSAddrs) then
   Self.SComSettings.RCSAddrs.Free();

 Self.SComSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|spr_typ1;spr_typ2;spr_typ3;...|min_delka|max_delka
function TBlkSCom.ParseEvent(str:string; old:boolean):TBlkSComSprEvent;
var sl, sl2:TStrings;
begin
 sl := TStringList.Create();
 sl2 := TStringList.Create();
 ExtractStringsEx(['|'], [], PChar(str), sl);

 Result.spr_typ := TStringList.Create();
 try
   Result.delka.min := -1;
   Result.delka.max := -1;

   if (old) then
     Result.zastaveni := Self.ParseOldRychEvent(sl[0])
   else
     Result.zastaveni := TRREv.Create(sl[0]);

   if ((sl.Count > 1) and (sl[1] <> '') and (LeftStr(sl[1], 2) <> '-1')) then
    begin
     ExtractStringsEx([';'], [], sl[1], sl2);

     Result.zpomaleni.enabled := true;
     if (old) then
       Result.zpomaleni.ev := Self.ParseOldRychEvent(sl[1])
     else
       Result.zpomaleni.ev := TRREv.Create(sl2[0]);

     Result.zpomaleni.speed := StrToInt(sl2[sl2.Count-1]);
    end else begin
     Result.zpomaleni.enabled := false;
     Result.zpomaleni.ev := nil;
    end;

   if (sl.Count > 2) then
    begin
     Result.spr_typ   := ParseSprTypes(sl[2]);
     Result.delka.min := StrToIntDef(sl[3], -1);
     Result.delka.max := StrToIntDef(sl[4], -1);
    end;
 finally
   sl.Free();
   sl2.Free();
 end;
end;

function TBlkSCom.ParseSprTypes(str:string):TStrings;
begin
 Result := TStringList.Create();
 ExtractStrings([';'], [' '], PChar(str), Result);
end;

////////////////////////////////////////////////////////////////////////////////

//ziskavani zpomalovacich a zastavovaich dat ze souboru (parsing dat)
//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
// : typ_zastaveni(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;
function TBlkSCom.ParseOldRychEvent(str:string):TRREv;
var data:TStrings;
    rrData:TRREvData;
begin
 data := TStringList.Create();

 try
   ExtractStringsEx([';'], [], str, data);

   case (data[0][1]) of
    '0':begin
      // usek
      rrData.typ := TRREvType.rrtUsek;
      rrData.usekState := true;
      rrData.usekPart := StrToInt(data[2]);
    end;//case 0

    '1':begin
      // ir
      rrData.typ := TRREvType.rrtIR;
      rrData.irId := StrToInt(data[1]);
      rrData.irState := true;
    end;//case 1
   end;//case

   Result := TRREv.Create(rrData);
 finally
   data.Free();
 end;
end;

function TBlkSCom.GetSprTypes(sl:TStrings):string;
var i:Integer;
begin
 Result := '';
 for i := 0 to sl.Count-1 do
  Result := Result + sl[i] + ';';
end;

function TBlkSCom.GetEvent(ev:TBlkSComSprEvent; short:boolean = false):string;
begin
 Result := '{' + ev.zastaveni.GetDefStr() + '}|';

 if (ev.zpomaleni.enabled) then
   Result := Result + '{{' + ev.zpomaleni.ev.GetDefStr() + '};' +
              IntToStr(ev.zpomaleni.speed) + '}';
 Result := Result + '|';

 if (not short) then
  begin
   Result := Result + Self.GetSprTypes(ev.spr_typ) + '|' +
             IntToStr(ev.delka.min) + '|' +
             IntToStr(ev.delka.max);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//nastavovani stavovych promennych:

procedure TBlkSCom.SetNavest(navest:Integer; changeCallbackOk, changeCallbackErr: TNotifyEvent);
var oblr:TOR;
begin
 if ((Self.SComStav.Navest = _NAV_DISABLED) or (Self.SComSettings.zamknuto)) then
  begin
   if (Assigned(changeCallbackErr)) then
     changeCallbackErr(Self);
   Exit();
  end;

 if ((navest = _NAV_PRIVOL) or (navest = _NAV_STUJ)) then
  begin
   // prodlouzeni nebo zruseni privolavaci navesti -> zrusit odpocet v panelu
   if (Self.SComStav.privol_timer_id > 0) then
     for oblr in Self.ORsRef do
      begin
       oblr.BroadcastGlobalData('INFO-TIMER-RM;'+IntToStr(Self.SComStav.privol_timer_id));
       oblr.TimerCnt := oblr.TimerCnt - 1;
      end;
   Self.SComStav.privol_timer_id := 0;
  end;

 if (navest = _NAV_PRIVOL) then
   Self.SComStav.privol_start := Now;

 if (Self.SComStav.Navest = navest) then
  begin
   if (Assigned(changeCallbackOk)) then
     changeCallbackOk(Self);
   Exit();
  end;

 // nastaveni vystupu
 try
   if (Self.SComSettings.RCSAddrs.Count > 0) then
    begin
     if (Self.SComSettings.OutputType = scom) then
      begin
       //scom
       RCSi.SetOutput(Self.SComSettings.RCSAddrs[0].board,
          Self.SComSettings.RCSAddrs[0].port, navest);
      end else begin
       //binary
       case (navest) of
        0,5,13,16..127:RCSi.SetOutput(Self.SComSettings.RCSAddrs[0].board,
            Self.SComSettings.RCSAddrs[0].port, 0);
       else//case (navest)
        RCSi.SetOutput(Self.SComSettings.RCSAddrs[0].board,
            Self.SComSettings.RCSAddrs[0].port, 1);
       end;//else case 0,5,13,16..127
      end;//else
    end;
 except
   if (Assigned(changeCallbackErr)) then
     changeCallbackErr(Self);
   Exit();
 end;

 // ruseni nouzove jizdni cesty pri padu navestidla do STUJ
 if (Self.SComStav.Navest = _NAV_STUJ) then
  begin
   if ((Self.UsekPred <> nil) and ((Self.UsekPred.typ = _BLK_USEK) or
       (Self.UsekPred.typ = _BLK_TU)) and ((Self.UsekPred as TBlkUsek).SComJCRef.Contains(Self))) then
    (Self.UsekPred as TBlkUsek).SComJCRef.Remove(Self);

   if (Assigned(Self.privol)) then
    begin
     Self.privol.RusJCWithoutBlk();
     Self.privol := nil;
    end;
  end;

 if ((Self.Navest = _NAV_PRIVOL) and (navest = _NAV_STUJ)) then
  begin
   // STUJ po privolavacce -> vypnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt - 1;
  end;

 if (not Self.changing) then
   Self.SComStav.navest_old := Self.Navest;
 Self.SComStav.changeCallbackOk := changeCallbackOk;
 Self.SComStav.changeCallbackErr := changeCallbackErr;
 Self.SComStav.Navest := _NAV_CHANGING;
 Self.SComStav.cilova_navest := navest;

 if (Self.SComSettings.RCSAddrs.Count > 0) then
   Self.SComStav.changeEnd := Now + EncodeTime(0, 0, _NAV_CHANGE_DELAY_MSEC div 1000, _NAV_CHANGE_DELAY_MSEC mod 1000)
 else
   Self.SComStav.changeEnd := Now + EncodeTime(0, 0, _NAV_CHANGE_SHORT_DELAY_MSEC div 1000, _NAV_CHANGE_SHORT_DELAY_MSEC mod 1000);

 if (not TBlkSCom.IsPovolovaciNavest(Self.SComStav.cilova_navest)) then // zastavujeme ihned
   Self.UpdateRychlostSpr(true);
 Self.Change();
end;

procedure TBlkSCom.SetNavest(navest:Integer);
begin
 Self.SetNavest(navest, TNotifyEvent(nil), TNotifyEvent(nil));
end;

procedure TBlkSCom.OnNavestSetOk();
var tmp:TNotifyEvent;
    oblr:TOR;
begin
 Self.SComStav.Navest := Self.SComStav.cilova_navest;

 if (Self.SComStav.cilova_navest = TBlkSCom._NAV_PRIVOL) then
  begin
   // nova navest je privolavacka -> zapnout zvukovou vyzvu
   for oblr in Self.ORsRef do
     oblr.PrivolavackaBlkCnt := oblr.PrivolavackaBlkCnt + 1;
  end;

 if (Self.autoblok) then
  begin
   if (TBlkTU(Self.UsekPred).nextTU <> nil) then TBlkTU(Self.UsekPred).nextTU.Change();
   if (TBlkTU(Self.UsekPred).Trat <> nil) then TBlkTrat(TBlkTU(Self.UsekPred).Trat).UpdateSprPredict();
  end;

 if (Assigned(Self.SComStav.changeCallbackOk)) then
  begin
   tmp := Self.SComStav.changeCallbackOk; // may set new callback in event
   Self.SComStav.changeCallbackOk := nil;
   tmp(Self);
  end;

 Self.UpdateRychlostSpr(true);
 if (Self.DNjc <> nil) then
   JCDb.CheckNNavaznost(Self.DNjc);
 Self.Change();
end;

procedure TBlkSCom.OnNavestSetError();
var tmp:TNotifyEvent;
begin
 // Tato funkce zatim neni moc vyuzivana, jedna se o pripravu do budoucna, kdy
 // by melo navestidlo kontrolu navesti (vstup do hJOP).

 if (Assigned(Self.SComStav.changeCallbackErr)) then
  begin
   tmp := Self.SComStav.changeCallbackErr; // may set new callback in event
   Self.SComStav.changeCallbackErr := nil;
   tmp(Self);
  end;

 // Jak nastavit aktualni navest?
 Self.SComStav.Navest := _NAV_STUJ;

 Self.Change();
end;

procedure TBlkSCom.UpdateNavestSet();
begin
 if (not Self.changing) then Exit();
 if (Self.SComStav.changeEnd <= Now) then
   Self.OnNavestSetOk();
end;

function TBlkSCom.GetAB():boolean;
begin
 Result := (Self.SComStav.ABJC <> nil);
end;

procedure TBlkSCom.SetABJC(ab:TJC);
begin
 if ((ab <> nil) and (Self.ABJC <> nil)) then
   raise EInvalidOperation.Create('Cannot change AB JC, can only enable/disable AB JC!');

 if ((Self.ABJC <> nil) and (ab = nil)) then begin
   try
     if (Assigned(ABlist)) then
       ABlist.Remove(Self.ABJC);
   except
     on E:EABJCNotInList do
       asm nop; end; // ignore exception
   end;
   Self.SComStav.ABJC := nil;
   Self.Change();
 end else if ((Self.ABJC = nil) and (ab <> nil)) then begin
   try
     ABlist.Add(ab);
   except
     on E:EABJCAlreadyInList do
       asm nop; end; // ignore exception
   end;
   Self.SComStav.ABJC := ab;
   Self.Change();
 end;
end;

procedure TBlkSCom.SetAB(ab:boolean);
begin
 if (ab) then
   raise EInvalidOperation.Create('You can only disable AB via SetAB!');

 if (Self.AB and (not ab)) then
   Self.ABJC := nil;
end;

procedure TBlkSCom.SetZacatekVolba(typ:TBlkSComVolba);
begin
 if (Self.SComStav.ZacatekVolba = typ) then Exit();
 Self.SComStav.ZacatekVolba := typ;
 Self.Change();
end;

procedure TBlkSCom.SetZAM(zam:boolean);
begin
 if (Self.SComStav.ZAM = zam) then Exit();
 Self.SComStav.ZAM := zam;

 if (Self.DNjc <> nil) then
  begin
   if (zam) then
    begin
     if (Self.DNjc.RozpadBlok <= 0) then
      begin
       Self.DNjc.RozpadBlok := -2;
       Self.DNjc.STUJ();
       JCDb.CheckNNavaznost(Self.DNjc);
      end;
    end else begin
     if ((Self.DNjc.RozpadBlok = -2) and (not Self.RCinProgress())) then
      begin
       Self.DNjc.RozpadBlok := -1;
       Self.DNjc.DN();
      end;
    end;
   Blky.SprPrediction(Self);
  end;//if Self.DNjc <> nil

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkSCom.MenuVCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if (Self.SComRel.SymbolType = 1) then Exit;
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) > -1)) then Exit;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then
  begin
   (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
   TOR(SenderOR).ClearVb(); // smazeme dosavadni seznam variantnich bodu
  end;
 Self.ZacatekVolba := TBlkSComVolba.VC;
end;

procedure TBlkSCom.MenuVCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkSComVolba.none;
end;

procedure TBlkSCom.MenuPCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if (((Self.DNjc <> nil) and (Self.DNjc.RozpadRuseniBlok < 1)) or
       (JCDb.FindOnlyStaveniJC(Self.id) > -1)) then Exit;

 Blk := BLky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.PC;
end;

procedure TBlkSCom.MenuPCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkSComVolba.none;
end;

procedure TBlkSCom.MenuSTUJClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 // poradi musi byt zachovano !
 Self.Navest := _NAV_STUJ;
 if (Self.DNjc = nil) then Exit();

 Self.DNjc.STUJ();
 JCDb.CheckNNavaznost(Self.DNjc);
 Blky.SprPrediction(Self);
end;

procedure TBlkSCom.MenuDNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.DNjc = nil) then Exit();

 if (Self.RCinProgress()) then
  begin
   TOR(SenderOR).StopMereniCasu(Self.SComStav.RCtimer);
   Self.SComStav.RCtimer := -1;
  end;

 Self.DNjc.DN();
 Blky.SprPrediction(Self);
end;

procedure TBlkSCom.MenuRCClick(SenderPnl:TIdContext; SenderOR:TObject);
var JC:TJC;
    Blk:TBlk;
begin
 if ((Self.DNjc = nil) or (Self.RCinProgress())) then Exit;

 JC := Self.DNjc;

 Blk := Self.UsekPred;
 if ((Blk = nil) or ((Blk.typ <> _BLK_USEK) and (Blk.typ <> _BLK_TU))) then
  begin
   // pokud blok pred JC neni -> 30 sekund
   Self.SComStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 30, 0));
  end else begin
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.uvolneno) then
    begin
     // pokud neni blok pred JC obsazen -> 2 sekundy
     Self.SComStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 2, 0));
    end else begin
     // pokud je obsazen, zalezi na typu jizdni cesty
     case (JC.data.TypCesty) of
      TJCType.vlak  : Self.SComStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 15, 0));   // vlakova cesta : 20 sekund
      TJCType.posun : Self.SComStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0,  5, 0));   // posunova cesta: 10 sekund
     else
      Self.SComStav.RCtimer := (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 1, 0, 0));                   // nejaka divna cesta: 1 minuta
     end;
    end;
  end;

 Self.AB := false;
 JC.RusJCWithoutBlk();
 Blky.SprPrediction(Self);
end;

procedure TBlkSCom.MenuABStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ABJC := Self.DNjc;
end;

procedure TBlkSCom.MenuABStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ABJC := nil;
end;

procedure TBlkSCom.MenuLockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := true;
 Self.Navest := _NAV_STUJ;
end;

procedure TBlkSCom.MenuUnlockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.MenuPNStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
    oblr:TOR;
begin
 if (Self.SComRel.SymbolType = 1) then Exit;
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) then Exit;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.NC;

 for oblr in Self.OblsRizeni do
   oblr.ORDKClickServer(Self.PrivolDKClick);
end;

procedure TBlkSCom.MenuPNStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBLkSComVolba.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.MenuPPStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) then Exit;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.PP;
end;

procedure TBlkSCom.MenuPPStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBLkSComVolba.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Prodloužení doby pøivolávací návìsti', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkSCom.MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
var podminky:TList<TPSPodminka>;
    blkId:Integer;
    blk:TBlk;
begin
 podminky := TList<TPSPodminka>.Create();

 for blkId in Self.SComStav.toRnz.Keys do
  begin
   Blky.GetBlkByID(blkId, Blk);
   if (blk <> nil) then
     podminky.Add(TOR.GetPSPodminka(blk, 'Rušení NZ'));
  end;

 ORTCPServer.Potvr(SenderPnl, Self.RNZPotvrSekv, SenderOR as TOR, 'Zrušení nouzových závìrù po nouzové cestì', TBlky.GetBlksList(Self), podminky);
end;

procedure TBlkSCom.MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);
var oblr:TOR;
begin
 if (Self.ZacatekVolba = TBlkSComVolba.NC) then
  begin
   for oblr in Self.OblsRizeni do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí pøivolávací návìsti',
                     TBlky.GetBlksList(Self), nil);
  end;
end;

procedure TBlkSCom.MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.SComStav.redukce_menu := 0;
 Self.Change();
end;

procedure TBlkSCom.MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);
var Blk:TBlk;
begin
 try
   if (Self.SComSettings.events[0].zastaveni.typ = TRREvType.rrtIR) then
    begin
     Blky.GetBlkByID(Self.SComSettings.events[0].zastaveni.data.irId, Blk);
     if ((Blk = nil) or (Blk.typ <> _BLK_IR)) then Exit();
     if (enabled) then
       RCSi.SetInput(TBlkIR(Blk).GetSettings().RCSAddrs[0].board, TBlkIR(Blk).GetSettings().RCSAddrs[0].port, 1)
     else
       RCSi.SetInput(TBlkIR(Blk).GetSettings().RCSAddrs[0].board, TBlkIR(Blk).GetSettings().RCSAddrs[0].port, 0);
    end;
 except
   ORTCPServer.BottomError(SenderPnl, 'Nepodaøilo se nastavit stav IR èidla!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Self.SComStav.Navest = -1) then Exit();

 case (Button) of
  F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

  ENTER: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = -1) and (Self.Navest <> 8))
         or (TOR(SenderOR).stack.volba = VZ)) and (JCDb.IsAnyVCAvailable(Self))) then begin
      if ((not Self.SComSettings.zamknuto) and (not Self.autoblok)) then Self.MenuVCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;

  F1: begin
    if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
           (JCDb.FindOnlyStaveniJC(Self.id) = -1) and (Self.Navest <> 8))
         or ((SenderOR as TOR).stack.volba = VZ)) and (JCDb.IsAnyPCAvailable(Self))) then begin
      if ((not Self.SComSettings.zamknuto) and (not Self.autoblok)) then Self.MenuPCStartClick(SenderPnl, SenderOR);
    end else
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSCom.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.SComStav.Navest = -1) then Exit();

 if      (item = 'VC>')  then Self.MenuVCStartClick(SenderPnl, SenderOR)
 else if (item = 'VC<')  then Self.MenuVCStopClick (SenderPnl, SenderOR)
 else if (item = 'PC>')  then Self.MenuPCStartClick(SenderPnl, SenderOR)
 else if (item = 'PC<')  then Self.MenuPCStopClick (SenderPnl, SenderOR)
 else if (item = 'STUJ') then Self.MenuSTUJClick   (SenderPnl, SenderOR)
 else if (item = 'DN')   then Self.MenuDNClick     (SenderPnl, SenderOR)
 else if (item = 'RC')   then Self.MenuRCClick     (SenderPnl, SenderOR)
 else if (item = 'AB>')  then Self.MenuABStartClick(SenderPnl, SenderOR)
 else if (item = 'AB<')  then Self.MenuABStopClick (SenderPnl, SenderOR)
 else if (item = 'ZAM>') then Self.MenuLockClick   (SenderPnl, SenderOR)
 else if (item = 'ZAM<') then Self.MenuUnlockClick (SenderPnl, SenderOR)
 else if (item = 'PN>')  then Self.MenuPNStartClick(SenderPnl, SenderOR)
 else if (item = 'PN<')  then Self.MenuPNStopClick (SenderPnl, SenderOR)
 else if (item = 'PP>')  then Self.MenuPPStartClick(SenderPnl, SenderOR)
 else if (item = 'PP<')  then Self.MenuPPStopClick (SenderPnl, SenderOR)
 else if (item = 'PPN')  then Self.MenuPPNClick    (SenderPnl, SenderOR)
 else if (item = 'RNZ')  then Self.MenuRNZClick    (SenderPnl, SenderOR)
 else if (item = 'IR>')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, true)
 else if (item = 'IR<')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, false)
 else if (item = 'KC')   then Self.MenuKCDKClick   (SenderPnl, SenderOR);

 if (item = 'ZRUŠ REDUKCI') then Self.MenuAdminREDUKClick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro konkretni s-com:
function TBlkSCom.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk:TBlk;
begin
 Result := inherited;

 // pokud je navestidlo trvale zamkle, neumoznime zadne volby
 if (Self.SComSettings.zamknuto) then Exit();

 if (((((Self.DNjc = nil) or (Self.DNjc.RozpadRuseniBlok >= 1)) and
        (JCDb.FindOnlyStaveniJC(Self.id) = -1) and (Self.Navest <> 8) and (not Self.AB))
      or ((SenderOR as TOR).stack.volba = VZ)) and
     (not Self.autoblok)) then
  begin
    case (Self.SComStav.ZacatekVolba) of
     TBlkSCOmVolba.VC : Result := Result + 'VC<,';
     TBlkSCOmVolba.PC : Result := Result + 'PC<,';
     TBlkSCOmVolba.NC : Result := Result + 'PN<,';
     TBlkSCOmVolba.PP : Result := Result + 'PP<,';
    else
      //2 = VC, 3= PC
      if (Self.SComRel.SymbolType <> 1) then
       begin
        if (JCDb.IsAnyVCAvailable(Self)) then // i kdyz neni zadna VC, schvalne umoznime PN
          Result := Result + 'VC>,';
        Result := Result + '!PN>,';
       end;
      if (JCDb.IsAnyPC(Self)) then
       begin
        if (JCDb.IsAnyPCAvailable(Self)) then
          Result := Result + 'PC>,';
        Result := Result + 'PP>,';
       end;
    end;// else ZacatekVolba <> none ...

    Result := Result + '-,';
  end;

 if ((Self.Navest > 0) and (not Self.autoblok)) then
   Result := Result + 'STUJ,';

 if (Self.Navest = 8) then
   Result := Result + '!PPN,';

 if (Self.DNjc <> nil) then
  begin
   // bud je cesta primo postavena, nebo je zrusena, ale podminky jsou vyhovujici pro DN
   // plati jen pro postavenou JC
   if ((not Self.ZAM) and (Self.Navest = 0) and (Self.DNjc.CanDN())) then
     Result := Result + 'DN,';

   if (((Self.Navest > 0) or (Self.DNjc.CanDN()) or (Self.DNjc.RozpadBlok < 1))
       and (not Self.RCinProgress())) then
    begin
     Result := Result + 'RC,';

     // AB lze jen u vlakove cesty
     if ((Self.DNjc.data.TypCesty = TJCType.vlak) and (not Self.AB)) then
       Result := Result + 'AB>,';
    end;
 end;

 // AB lze jen u vlakove cesty
 if (Self.AB) then
   Result := Result + 'AB<,';

 //7=ZAM
 if (Self.SComStav.ZAM) then
   Result := Result + 'ZAM<,'
  else
   Result := Result + 'ZAM>,';

 if ((Self.Navest <> 8) and (Self.CanIDoRNZ)) then
  Result := Result + '!RNZ,';

 if (rights >= TORControlRights.superuser) then
   if (Self.SComStav.redukce_menu > 0) then
     Result := Result + '-,*ZRUŠ REDUKCI,';

 // DEBUG: jednoduche nastaveni IR pri knihovne simulator
 if (RCSi.IsSimulatorMode()) then
  begin
   if ((Self.SComSettings.events.Count > 0) and (Self.SComSettings.events[0].zastaveni.typ = TRREvType.rrtIR)) then
    begin
     Blky.GetBlkByID(Self.SComSettings.events[0].zastaveni.data.irId, Blk);
     if ((Blk <> nil) and (Blk.typ = _BLK_IR)) then
      begin
       case (TBlkIR(Blk).Stav) of
         TIRStav.uvolneno : Result := Result + '-,*IR>,';
         TIRStav.obsazeno : Result := Result + '-,*IR<,';
       end;//case
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkSCom.NavestToString(Navest:Integer):string;
 begin
  case (Navest) of
   -2:Result  := 'Stavìní...';
   -1:Result  := 'Disabled';
    0:Result  := 'Stùj/Posun zakázán';
    1:Result  := 'Volno';
    2:Result  := 'Výstraha';
    3:Result  := 'Oèekávejte 40 km/h';
    4:Result  := '40 km/h a volno';
    5:Result  := 'Svítí vše (Rezerva)';
    6:Result  := '40 km/h a výstraha';
    7:Result  := '40 km/h a oèekávejte 40 km/h';
    8:Result  := 'Pøivolávací návìst';
    9:Result  := 'Dovolen zajištìný posun';
    10:Result := 'Dovolen nezajištìný posun';
    11:Result := 'Opakování návìsti volno';
    12:Result := 'Opakování návìsti výstraha';
    13:Result := 'Návìstidlo zhaslé';
    14:Result := 'Opakování návìsti oèekávejte 40 km/h';
    15:Result := 'Opakování návìsti výstraha a 40 km/h';
   else//case
    Result := 'Jiná návìst';
   end;//else case
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RedukujMenu();
begin
 Self.SComStav.redukce_menu := Self.SComStav.redukce_menu + 1;

 // prave zacala redukce
 if (Self.SComStav.redukce_menu = 1) then
  Self.Change();
end;

procedure TBlkSCom.ZrusRedukciMenu();
begin
 if (Self.SComStav.redukce_menu > 0) then
  Self.SComStav.redukce_menu := Self.SComStav.redukce_menu - 1;

 // prave skoncila redukce
 if (Self.SComStav.redukce_menu = 0) then
  Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.JCZrusNavest();
begin
 if (Self.SComSettings.ZpozdeniPadu > 0) then
  begin
   Self.SComStav.padani       := true;
   Self.SComStav.padani_start := Now;
   writelog('Návìstidlo '+Self.GlobalSettings.name+': spoždìní pádu '+IntToStr(Self.SComSettings.ZpozdeniPadu)+' s', WR_VC, 0);
  end else begin
   Self.Navest := _NAV_STUJ;
  end;

 Self.UpdateRychlostSpr(true);
end;

procedure TBlkSCom.UpdatePadani();
begin
 if (not Self.SComStav.padani) then Exit();

 if (Self.SComStav.padani_start + EncodeTime(0, Self.SComSettings.ZpozdeniPadu div 60, Self.SComSettings.ZpozdeniPadu mod 60, 0) < Now) then
  begin
   Self.Navest := _NAV_STUJ;
   Self.SComStav.padani := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// aktualizace rychlosti souprav pøed návìstidlem
// pozor na padání !
// force nucene zastavi vlak, resp. nastavi jeho rychlost
//  metoda je volana s force v pripade, kdy dochazi k prime zmene navesti od uzivatele (STUJ, DN, RC)
procedure TBlkSCom.UpdateRychlostSpr(force:boolean = false);
var Usek, SCom:TBlk;
    spr:TSouprava;
    scomEv:TBlkScomSprEvent;
    i:Integer;
begin
 if (Self.SComSettings.events.Count = 0) then Exit();
 Usek := Self.UsekPred;
 if (Self.SComRel.SymbolType = 1) then Exit();          // pokud jsem posunove navestidlo, koncim funkci
 if ((Usek = nil) or ((Usek.typ <> _BLK_USEK) and (Usek.typ <> _BLK_TU))) then Exit();    // pokud pred navestidlem neni usek, koncim funkci

 // pokud na useku prede mnou neni souprava, koncim funkci
 if (not (Usek as TBlkUsek).IsSouprava()) then
  begin
   // tady musi dojit ke zruseni registrace eventu, kdyby nedoslo, muze se stat,
   // ze za nejakou dobu budou splneny podminky, pro overovani eventu, ale
   // event bude porad bezet -> pokud je casovy, okamzite byse spustil
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.SComSettings.events.Count)) then
     if (Self.SComSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.SComSettings.events[Self.lastEvIndex].zastaveni.Unregister();
   Exit();
  end;

 // pokud souprava svym predkem neni na bloku pred navestidlem, koncim funkci
 spr := Soupravy[Self.GetSoupravaIndex(Usek)];
 if (spr.front <> Usek) then
  begin
   // tady musime zrusit registraci eventu, viz vyse
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.SComSettings.events.Count)) then
     if (Self.SComSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.SComSettings.events[Self.lastEvIndex].zastaveni.Unregister();
   Exit();
  end;

 if ((Usek.typ = _BLK_TU) and (TBlkTU(Usek).Trat <> nil) and
     ((TBlkTrat(TBlkTU(Usek).Trat)).ChangesSprDir())) then
  begin
   // pokud se jedna o navestidlo, u ktereho se meni smer trati, a vlak jede v
   // trati ve smeru A --> B, navestidlo neni aktivni (tj. koncim funkci)
   if ((Self = TBlkTrat(TBlkTU(Usek).Trat).navLichy) and
       (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.AtoB)) then
     Exit();

   // podobne pokud se jedna o prvni navestidlo autobloku, ignoruji jej ve smeru B --> A
   if ((Self.autoblok) and (TBlkTrat(TBlkTU(Usek).Trat).Smer = TTratSmer.BtoA) and
       (TBlkTU(Usek).id = TBlkTrat(TBlkTU(Usek).Trat).GetSettings().Useky[0])) then
     Exit();
  end;

 // zjisteni aktualni udalosti podle typu a delky soupravy
 i := Self.CurrentEventIndex();
 scomEv := Self.SComSettings.events[i];

 if (i <> Self.lastEvIndex) then
  begin
   // Z nejakeho duvodu reagujeme na novou udalost -> vypnout starou udalost
   if ((Self.lastEvIndex >= 0) and (Self.lastEvIndex < Self.SComSettings.events.Count)) then
    begin
     if (Self.SComSettings.events[Self.lastEvIndex].zastaveni.enabled) then
       Self.SComSettings.events[Self.lastEvIndex].zastaveni.Unregister();

     if ((Self.SComSettings.events[Self.lastEvIndex].zpomaleni.enabled) and
         (Self.SComSettings.events[Self.lastEvIndex].zpomaleni.ev.enabled)) then
       Self.SComSettings.events[Self.lastEvIndex].zpomaleni.ev.Unregister();
    end;

   Self.lastEvIndex := i;
  end;

 ///////////////////////////////////////////////////

 // ZPOMALOVANI
 if ((scomEv.zpomaleni.enabled) and (spr.rychlost > scomEv.zpomaleni.speed) and
     ((Usek as TBlkUsek).zpomalovani_ready) and
     ((not Assigned(Self.DNjc)) or (not Self.IsPovolovaciNavest()) or (spr.IsPOdj(Usek))) and
     (spr.smer = Self.SComRel.smer)) then
  begin
   if (not scomEv.zpomaleni.ev.enabled) then
     scomEv.zpomaleni.ev.Register();

   if (scomEv.zpomaleni.ev.IsTriggerred(Usek, true)) then
    begin
     scomEv.zpomaleni.ev.Unregister();
     spr.rychlost := scomEv.zpomaleni.speed;
     (Usek as TBlkUsek).zpomalovani_ready := false;
    end;
  end else begin
   if ((scomEv.zpomaleni.enabled) and (scomEv.zpomaleni.ev.enabled)) then
     scomEv.zpomaleni.ev.Unregister();
  end;

 ///////////////////////////////////////////////////

 // ZASTAVOVANI, resp. nastavovani rychlosti prislusne JC
 if (not scomEv.zastaveni.enabled) then
   scomEv.zastaveni.Register();

 if ((scomEv.zastaveni.IsTriggerred(Usek, true)) or (force)) then       // podminka IsRychEvent take resi to, ze usek musi byt obsazeny (tudiz resi vypadek useku)
  begin
   // event se odregistruje automaticky pri zmene

   if ((spr.IsPOdj(Usek)) and (spr.smer = Self.SComRel.smer)) then
    begin
     // predvidany odjezd neuplynul -> zastavit soupravu
     if (spr.rychlost <> 0) then
       spr.SetRychlostSmer(0, Self.SComRel.smer);

     // souprava je na zastavovaci udalosti -> zacit pocitat cas
     if (not spr.GetPOdj(Usek).origin_set) then
      begin
       spr.GetPOdj(Usek).RecordOriginNow();
       TBlkUsek(Usek).PropagatePOdjToTrat();
       Usek.Change();
      end;

     Exit();
    end;

   if ((Assigned(Self.DNjc)) and (Self.DNjc.data.TypCesty = TJCType.vlak)) then
    begin
     // je JC -> je postaveno?
     if ((Self.IsPovolovaciNavest()) and (not Self.SComStav.padani)) then
      begin
       // je postaveno -> zkontrlolujeme, jestli je postaveno dalsi navestidlo
       if ((spr.rychlost > 0) and (spr.smer <> Self.SComRel.smer)) then Exit(); // pokud jede souprava opacnym smerem, kaslu na ni

       if (Self.DNjc.data.DalsiNNavaznostTyp = 2) then
        begin
         // zjistime dalsi navestidlo
         Blky.GetBlkByID(Self.DNjc.data.DalsiNNavaznost, SCom);

         if ((SCom <> nil) and (SCom.typ = _BLK_SCOM) and ((SCom as TBlkSCom).IsPovolovaciNavest())) then
          begin
            // dalsi navestilo je na VOLNO
            if ((spr.rychlost <> Self.DNjc.data.RychlostDalsiN*10) or (spr.smer <> Self.SComRel.smer)) then
              spr.SetRychlostSmer(Self.DNjc.data.RychlostDalsiN*10, Self.SComRel.smer);
          end else begin
            // dalsi navestidlo je na STUJ
            if ((spr.rychlost <> Self.DNjc.data.RychlostNoDalsiN*10) or (spr.smer <> Self.SComRel.smer)) then
              spr.SetRychlostSmer(Self.DNjc.data.RychlostNoDalsiN*10, Self.SComRel.smer);
          end;
        end else begin
         if (Self.DNjc.data.DalsiNNavaznostTyp = 1) then
          begin
           // navaznost na trat
           if ((spr.rychlost <> Self.DNjc.data.RychlostDalsiN*10) or (spr.smer <> Self.SComRel.smer)) then
             spr.SetRychlostSmer(Self.DNjc.data.RychlostDalsiN*10, Self.SComRel.smer);
          end else begin
           // navaznost nikam
           if ((spr.rychlost <> Self.DNjc.data.RychlostNoDalsiN*10) or (spr.smer <> Self.SComRel.smer)) then
             spr.SetRychlostSmer(Self.DNjc.data.RychlostNoDalsiN*10, Self.SComRel.smer);
          end;
        end;

       // kontorla prehravani stanicniho hlaseni
       spr.CheckSh(Self);
      end else begin
       // neni povolovaci navest -> zastavit LOKO
       if ((spr.smer = Self.SComRel.smer) and (spr.rychlost <> 0)) then
         spr.SetRychlostSmer(0, Self.SComRel.smer);
      end;
    end else begin
     // nenalezena jizdni cesta -> muze se jednat o navestidlo v autobloku
     if (spr.smer = Self.SComRel.smer) then
      begin
       if ((Self.IsPovolovaciNavest()) and (not Self.SComStav.padani) and
           (Self.UsekPred.typ = _BLK_TU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
        begin
         // je povolavaci navest -> je zelena, nebo zluta?
         if (Self.Navest = 1) then
          begin
           // zelena -> rychlost dalsiho useku
           if (spr.rychlost <> TBlkTU(Self.UsekPred).GetSettings.rychlost) then
             spr.SetRychlostSmer(TBlkTU(Self.UsekPred).GetSettings.rychlost, Self.SComRel.smer)
          end else begin
           // vystraha -> 40 km/h
           if (spr.rychlost <> 40) then
             spr.SetRychlostSmer(40, Self.SComRel.smer)
          end;
        end else begin
         //  neni povolovaci navest -> zastavit
         if (spr.rychlost <> 0) then
           spr.SetRychlostSmer(0, Self.SComRel.smer);
        end;
      end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci udalost, na kterou by se melo reagovat podle aktualniho stavu kolejiste.

function TBlkSCom.CurrentEventIndex():Integer;
var i, j:Integer;
    spr:TSouprava;
    Usek:TBlk;
begin
 if (Self.SComSettings.events.Count = 0) then
   raise ENoEvents.Create('No current events!');

 Usek := Self.UsekPred;
 if (not (Usek as TBlkUsek).IsSouprava()) then
  begin
   // na bloku neni zadna souprava
   Result := 0;
  end else begin
   spr := Soupravy[Self.GetSoupravaIndex(Usek)];

   // hledame takovy event, ktery odpovida nasi souprave
   for i := 0 to Self.SComSettings.events.Count-1 do
     if ((spr.delka >= Self.SComSettings.events[i].delka.min) and (spr.delka <= Self.SComSettings.events[i].delka.max)) then
       for j := 0 to Self.SComSettings.events[i].spr_typ.Count-1 do
         if (spr.typ = Self.SComSettings.events[i].spr_typ[j]) then
           Exit(i);

   // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
   Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean;
begin
 if ((Self.Navest = _NAV_CHANGING) and (TBlkSCom.IsPovolovaciNavest(Self.SComStav.cilova_navest, jctype))) then
   // navest se meni na nejakou povolovaci -> ridim se jeste tou starou
   Result := TBlkSCom.IsPovolovaciNavest(Self.SComStav.navest_old, jctype)
 else
   Result := TBlkSCom.IsPovolovaciNavest(Self.Navest, jctype);
end;

class function TBlkSCom.IsPovolovaciNavest(Navest:Integer; jctype:TJCType = TJCType.vlak):boolean;
begin
 if (jcType = TJCType.vlak) then
  begin
   case (navest) of
    1..4, 6, 7, 11, 12, 14, 15: Result := true;
   else
    Result := false;
   end;
  end else if (jcType = TJCType.posun) then
    Result := (navest = _NAV_POSUN_ZAJ) or (navest = _NAV_POSUN_NEZAJ)
  else
    Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.UpdatePrivol();
var oblr:TOR;
begin
 if ((Self.SComStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now+EncodeTime(0, 0, 30, 0)) and
     (Self.SComStav.privol_timer_id = 0)) then
  begin
   // oznameni o brzkem ukonceni privolavaci navesti
   Self.SComStav.privol_timer_id := Random(65536)+1;
   for oblr in Self.ORsRef do
    begin
     oblr.BroadcastGlobalData('INFO-TIMER;'+IntToStr(Self.SComStav.privol_timer_id)+
                              ';0;30;PN '+Self.GlobalSettings.name);
     oblr.TimerCnt := oblr.TimerCnt + 1;
    end;
  end;

 if (Self.SComStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now) then
  begin
   // pad privolavaci navesti
   Self.Navest := _NAV_STUJ;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// privolavaci navest bez podpory zabezpecovaciho zarizeni
procedure TBlkSCom.PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
var oblr:TOR;
begin
 if (Button = ENTER) then
  begin
   for oblr in Self.OblsRizeni do
     oblr.ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí pøivolávací návìsti',
                     TBlky.GetBlksList(Self), nil);
  end else begin
   if (Button = TPanelButton.F2) then
     ORTCPServer.Menu(SenderPnl, Self, TOR(SenderOR), '$'+TOR(SenderOR).Name + ',-,' + 'KC');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
begin
 if (success) then
  begin
   Self.SComStav.ZacatekVolba := TBlkSComVolba.none;
   Self.Navest := _NAV_PRIVOL;
  end else begin
   self.ZacatekVolba := TBlkSComVolba.none;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RNZPotvrSekv(Sender:TIdContext; success:boolean);
var blkId:Integer;
    blk:TBlk;
    toRNZ:TDictionary<Integer, Cardinal>;
begin
 if (not success) then Exit();

 // nejdriv uvolnime toRNZ -- abychom jej nemazali v DecreaseNouzZaver
 toRNZ := Self.SComStav.toRnz;
 Self.SComStav.toRnz := TDictionary<Integer, Cardinal>.Create();

 for blkId in toRNZ.Keys do
  begin
   Blky.GetBlkByID(blkId, blk);
   if (blk = nil) then continue;   

   case (blk.typ) of
    _BLK_VYH: begin
       if (TBlkVyhybka(blk).vyhZaver) then
         TBlkVyhybka(blk).DecreaseNouzZaver(toRnz[blkId]);
    end;

    _BLK_ZAMEK: begin
       if (TBlkZamek(blk).nouzZaver) then
         TBlkZamek(blk).DecreaseNouzZaver(toRnz[blkId]);
    end;
   end;
  end;

 toRNZ.Free();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.SetUsekPredID(new_id:Integer);
begin
 if (Self.SComRel.UsekID = new_id) then Exit();
 Self.SComRel.UsekID := new_id;
 if (new_id = -1) then Self.autoblok := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.GetUsekPred():TBlk;
begin
 if (((Self.fUsekPred = nil) and (Self.UsekID <> -1)) or ((Self.fUsekPred <> nil) and (Self.UsekID <> Self.fUsekPred.id))) then
   Blky.GetBlkByID(Self.UsekID, Self.fUsekPred);
 Result := Self.fUsekPred;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.AddBlkToRnz(blkId:Integer; change:boolean = true);
begin
 if (Self.SComStav.toRnz.ContainsKey(blkId)) then
   Self.SComStav.toRnz[blkId] := Self.SComStav.toRnz[blkId] + 1
 else
   Self.SComStav.toRnz.Add(blkId, 1);

 if ((Self.SComStav.toRnz.Count = 1) and (Self.SComStav.toRnz[blkId] = 1) and
     (change)) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.CanIDoRNZ():boolean;
begin
 Result := Self.SComStav.toRnz.Count > 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RemoveBlkFromRnz(blkId:Integer);
begin
 if (Self.SComStav.toRnz.ContainsKey(blkId)) then
  begin
   Self.SComStav.toRnz.Remove(blkId);
   if (Self.SComStav.toRnz.Count = 0) then
     Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.UnregisterAllEvents();
var ev:TBlkScomSprEvent;
begin
 for ev in Self.SComSettings.events do
  begin
   ev.zastaveni.Unregister();
   if ((ev.zpomaleni.enabled) and (Assigned(ev.zpomaleni.ev))) then
     ev.zpomaleni.ev.Unregister();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkSCom.GetPtState(json:TJsonObject);
begin
 json['navest'] := Self.Navest;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.GetSoupravaIndex(usek:TBlk = nil):Integer;
begin
 if (usek = nil) then
   Blky.GetBlkByID(Self.UsekID, usek);

 if (Self.Smer = THVStanoviste.lichy) then
   Result := TBlkUsek(usek).SoupravaS
 else
   Result := TBlkUsek(usek).SoupravaL;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.RCinProgress():boolean;
begin
 Result := (Self.SComStav.RCtimer > -1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RCtimerTimeout();
begin
 Self.SComStav.RCtimer := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.PropagatePOdjToTrat();
var spr:Integer;
    trat:TBlk;
    podj:TPOdj;
begin
 spr := Self.GetSoupravaIndex();
 if (spr = -1) then
  begin
   spr := TBlkUsek(Self.UsekPred).SprPredict;
   if (spr = -1) then Exit();
  end;

 if (Self.DNjc = nil) then Exit();
 if (Self.DNjc.data.Trat = -1) then Exit();
 Blky.GetBlkByID(Self.DNjc.data.Trat, trat);
 if (TBlkTrat(trat).SprPredict = nil) then Exit();
 if (TBlkTrat(trat).SprPredict.souprava <> spr) then Exit();

 if (Soupravy[spr].IsPOdj(Self.UsekPred)) then
  begin
   podj := Soupravy[spr].GetPOdj(Self.UsekPred);
   if (not podj.IsDepSet) then Exit();
   if ((TBlkTrat(trat).SprPredict.IsTimeDefined) and (TBlkTrat(trat).SprPredict.time = podj.DepTime())) then Exit();

   TBlkTrat(trat).SprPredict.predict := true;
   TBlkTrat(trat).SprPredict.time := podj.DepTime();
   TBlkTrat(trat).Change();
  end else if ((TBlkTrat(trat).SprPredict.predict) and
               ((not Soupravy[spr].IsPOdj(Self.UsekPred)) or (not Soupravy[spr].GetPOdj(Self.UsekPred).IsDepSet()))) then begin
   TBlkTrat(trat).SprPredict.predict := false;
   TBlkTrat(trat).SprPredict.UndefTime();
   TBlkTrat(trat).Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.IsChanging():boolean;
begin
 Result := (Self.Navest = _NAV_CHANGING);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
