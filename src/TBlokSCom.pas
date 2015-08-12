unit TBlokSCom;

//definice a obsluha technologickeho bloku SCom
// Pritomnost redukce menu u bloku navestidla znamena, ze z tohoto bloku nelze zacit jizdni cestu.

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, RPConst,
      TechnologieJC, IdContext, Generics.Collections;

type
 TBlkSComVolba = (none = 0, VC = 1, PC = 2, NC = 3);
 TBlkSComOutputType = (scom = 0, binary = 1);
 TBlkSComSignal = (disabled = -1, usek = 0, ir = 1);

 // event zastaveni, ci zpomaleni
 TBlkSComRychEvent = record
  signal:TBlkSComSignal;                        // rozhoduje, jeslti uvazovat IR, ci usek
  usekid, usekpart:Integer;                     // id useku a index jeho casti
  irid:Integer;                                 // id bloku IR
  speed:Integer;                                // zpomalovaci rychlost z km/h (40, 50, 60...)
 end;

 // zastaovoaci a zpomalovaci udalost pro jeden typ soupravy a jeden rozsah delek soupravy
 TBlkSComSprEvent = record
  spr_typ:TStrings;                             // tato udalost je brana v potaz, pokud ma souprava jeden ze zde definovancyh typu (MOs, Os, Mn, Pn, ...)
  delka:record                                  // tato udalost je brana v potaz, pokud ma souprava delku > min && < max
    min:Integer;
    max:Integer;
  end;

  zastaveni: TBlkSComRychEvent;                 // zastavovoaci udalost
  zpomaleni: TBlkSComRychEvent;                 // zpomalovaci udalost
 end;

 // vlastnosti bloku SCom, ktere se ukladaji do databaze bloku
 TBlkSComSettings = record
  MTBAddrs:TMTBAddrs;                            // ve skutecnosti je signifikantni jen jedna adresa - na indexu [0], coz je vystup pro rizeni navestidla
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
  AB:Boolean;                                    // automaticke staveni
  ZAM:Boolean;                                   // navestidlo zamkle z panelu
  redukce_menu:Integer;                          // kolik blokù mì redukuje
  dn_jc_ref,privol_ref:TJC;                      // reference na aktualni JC na navestidle (resp. NC)

  padani:boolean;                                // zde je true, pokud navestidlo pada do STUJ (ma zpozdeny pad) a jeste nespadlo
  padani_start:TDateTime;                        // start padani

  privol_start:TDateTime;                        // start privolavaci navesti (privolavacka sviti pouze omezeny cas a pak se vypne)
  privol_timer_id:Integer;                       // id timeru ukonceni privolavacky v panelu, ze ktreho byla JC postavena
  autoblok:boolean;
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
     AB : false;
     ZAM : false;
     dn_jc_ref : nil;
     privol_ref : nil;
     privol_timer_id : 0;
     autoblok : false;
   );

   // privolavaci navest sviti jednu minitu a 30 sekund
   _PRIVOL_MIN = 1;
   _PRIVOL_SEC = 30;

  private
   SComSettings:TBlkSComSettings;
   SComStav:TBlkSComStav;
   SComRel:TBlkSComRel;

   fUsekPred:TBlk;

    procedure ParseEvents(str:string);
    function ParseEvent(str:string):TBlkSComSprEvent;
    function ParseSprTypes(str:string):TStrings;
    function ParseRychEvent(str:string):TBlkSComRychEvent;

    function GetRychEvent(data:TBlkSComRychEvent):string;
    function GetSprTypes(sl:TStrings):string;
    function GetEvents():string;

    procedure SetNavest(navest:Integer);
    procedure SetAB(ab:boolean);
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
    procedure MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);

    // DEBUG menu:
    procedure MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);
    procedure MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure UpdatePadani();
    procedure UpdatePrivol();

    procedure PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
    procedure PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
    procedure RNZPotvrSekv(Sender:TIdContext; success:boolean);

    procedure SetUsekPredID(new_id:Integer);

    class function IsRychEvent(data:TBlkSComRychEvent):boolean;
    function IsZastavEvent():Integer;
    function IsZpomalEvent():Integer;

    function GetUsekPred():TBlk;

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    function IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean;

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

    //----- SCom own functions -----

    function GetSettings():TBlkSComSettings;
    procedure SetSettings(data:TBlkSComSettings);

    procedure RedukujMenu();
    procedure ZrusRedukciMenu();

    procedure UpdateRychlostSpr(force:boolean = false);

    class function NavestToString(navest:Integer):string;

    property SymbolType:Byte read SComRel.SymbolType;
    property UsekID:Integer read SComRel.UsekID write SetUsekPredID;
    property Smer:THVStanoviste read SComRel.smer write SComRel.smer;

    //stavovae promenne
    property Navest:Integer read SComStav.Navest write SetNavest;
    property ZacatekVolba:TBlkSComVolba read SComStav.ZacatekVolba write SetZacatekVolba;
    property AB:boolean read SComStav.AB write SetAB;
    property ZAM:boolean read SComStav.ZAM write SetZAM;
    property Lichy:THVStanoviste read SComRel.Smer;
    property DNjc:TJC read SComStav.dn_jc_ref write SComStav.dn_jc_ref;
    property privol:TJC read SComStav.privol_ref write SComStav.privol_ref;
    property UsekPred:TBlk read GetUsekPred;
    property autoblok:boolean read SComStav.autoblok write SComStav.autoblok;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights); override;

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

uses TechnologieMTB, TBloky, TOblRizeni, TBlokUsek, TJCDatabase, TCPServerOR,
      GetSystems, Logging, SprDb, Souprava, TBlokIR, Zasobnik, ownStrUtils,
      TBlokTratUsek, TBlokTrat;

constructor TBlkSCom.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_SCOM;
 Self.SComStav := Self._def_scom_stav;
 Self.SComSettings.events := TList<TBlkSComSprEvent>.Create();
 Self.fUsekPred := nil;
end;//ctor

destructor TBlkSCom.Destroy();
var i:Integer;
begin
 for i := 0 to Self.SComSettings.events.Count-1 do
   Self.SComSettings.events[i].spr_typ.Free();
 Self.SComSettings.events.Free();

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.SComSettings.MTBAddrs := Self.LoadMTB(ini_tech,section);

 Self.SComSettings.zamknuto     := ini_tech.ReadBool(section, 'zamknuti', false);

 Self.SComSettings.OutputType   := TBlkSComOutputType(ini_tech.ReadInteger(section,'OutType',0));
 Self.SComSettings.ZpozdeniPadu := ini_tech.ReadInteger(section, 'zpoz', 0);

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   ExtractStrings([';'],[],PChar(ini_rel.ReadString('N',IntToStr(Self.GlobalSettings.id),'')),str);
   if (str.Count >= 3) then
    begin
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

   str.Free();
  end else begin
    Self.ORsRef.Cnt := 0;
  end;

 // toto musi byt az po nacteni spnl
 Self.ParseEvents(ini_tech.ReadString(section, 'ev', ''));

 PushMTBToOR(Self.ORsRef, Self.SComSettings.MTBAddrs);
end;//procedure

procedure TBlkSCom.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech, section);

 Self.SaveMTB(ini_tech,section, Self.SComSettings.MTBAddrs);
 ini_tech.WriteString(section, 'ev', Self.GetEvents());
 ini_tech.WriteInteger(section, 'OutType', Integer(Self.SComSettings.OutputType));
 ini_tech.WriteInteger(section, 'zpoz', Self.SComSettings.ZpozdeniPadu);
 ini_tech.WriteBool(section, 'zamknuti', Self.SComSettings.zamknuto);
end;//procedure

procedure TBlkSCom.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Enable();
begin
 if (not MTB.IsModule(Self.SComSettings.MTBAddrs.data[0].board)) then
  Exit();

 Self.SComStav.Navest := 0;
 Self.Change();
end;//procedure

procedure TBlkSCom.Disable();
begin
 Self.SComStav.Navest := -1;
 Self.SComStav.ZacatekVolba := TBlkSComVolba.none;
 Self.SComStav.AB  := false;
 Self.SComStav.ZAM := false;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Update();
begin
 Self.UpdatePadani();
 Self.UpdateRychlostSpr();

 if (Self.Navest = 8) then
   Self.UpdatePrivol();

 if (MTB.IsModule(Self.SComSettings.MTBAddrs.data[0].board)) then
  begin
   if (Self.SComStav.Navest < 0) then
    begin
     Self.SComStav.Navest := 0;
     Self.Change();
    end;
  end else begin
   if (Self.SComStav.Navest >= 0) then
    begin
     Self.SComStav.Navest := -1;
     JCDb.RusJC(Self);
     Self.Change();
    end;
  end;

 inherited;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.Change(now:boolean = false);
begin
 // zmenu navesti propagujeme do prilehle trati, kde by mohlo dojit ke zmene
 // navesti autobloku
 if ((Self.UsekPred <> nil) and (Self.UsekPred.GetGlobalSettings().typ = _BLK_TU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
   Self.UsekPred.Change();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.GetSettings():TBlkSComSettings;
begin
 Result := Self.SComSettings;
end;//function

procedure TBlkSCom.SetSettings(data:TBlkSComSettings);
var i:Integer;
    ev:TBlkSComSprEvent;
begin
 Self.SComSettings := data;

 for i := 0 to Self.SComSettings.events.Count-1 do
  begin
   ev := Self.SComSettings.events[i];
   ev.zastaveni.usekid := Self.SComRel.UsekID;
   ev.zpomaleni.usekid := Self.SComRel.UsekID;
   Self.SComSettings.events[i] := ev;
  end;

 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// format ev: (ev1)(ev2)(ev3)
procedure TBlkSCom.ParseEvents(str:string);
var sl:TStrings;
    i:Integer;
begin
 sl := TStringList.Create();
 ExtractStrings(['(', ')'], [], PChar(str), sl);

 Self.SComSettings.events := TList<TBLkSComSprEvent>.Create();
 for i := 0 to sl.Count-1 do
   Self.SComSettings.events.Add(Self.ParseEvent(sl[i]));

 sl.Free();
end;//function

// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|spr_typ1;spr_typ2;spr_typ3;...|min_delka|max_delka
function TBlkSCom.ParseEvent(str:string):TBlkSComSprEvent;
var sl:TStrings;
begin
 sl := TStringList.Create();
 ExtractStringsEx(['|'], [], str, sl);

 Result.spr_typ := TStringList.Create();
 Result.delka.min := -1;
 Result.delka.max := -1;

 Result.zastaveni := Self.ParseRychEvent(sl[0]);
 if (sl.Count > 1) then
  begin
   Result.zpomaleni := Self.ParseRychEvent(sl[1]);

   if (sl.Count > 2) then
    begin
     Result.spr_typ   := ParseSprTypes(sl[2]);
     Result.delka.min := StrToIntDef(sl[3], -1);
     Result.delka.max := StrToIntDef(sl[4], -1);
    end;
  end else
   Result.zpomaleni.signal := disabled;

 Result.zastaveni.usekid := Self.SComRel.UsekID;

 sl.Free();
end;//function

function TBlkSCom.ParseSprTypes(str:string):TStrings;
begin
 Result := TStringList.Create();
 ExtractStrings([';'], [' '], PChar(str), Result);
end;//function

////////////////////////////////////////////////////////////////////////////////

//ziskavani zpomalovacich a zastavovaich dat ze souboru (parsing dat)
//format RychEvent data: textove ulozeny 1 radek, kde jsou data oddelena ";"
// : typ_zastaveni(0=usek;1=ir);
//    pro usek nasleduje: usekid;usekpart;speed;
//    pro ir nasleduje: irid;speed;
function TBlkSCom.ParseRychEvent(str:string):TBlkSComRychEvent;
var data:TStrings;
begin
 data := TStringList.Create();

 ExtractStrings([';'],[],PChar(str),data);

 Result.usekid   := -1;
 Result.usekpart := -1;
 Result.irid     := -1;
 Result.speed    := 0;

 Result.signal := TBlkSComSignal(StrToInt(data[0]));
 case (Result.signal) of
  disabled:;
  usek:begin
    Result.usekid   := StrToInt(data[1]);
    Result.usekpart := StrToInt(data[2]);
    if (data.Count > 3) then
      Result.speed := StrToInt(data[3])
    else
      Result.speed := 0;
  end;//case 0
  ir:begin
    Result.irid  := StrToInt(data[1]);
    if (data.Count > 2) then
      Result.speed := StrToInt(data[2])
    else
      Result.speed := 0;
  end;//case 1
 end;//case

 data.Free();
end;//function

//ziskavani zpomalovacich a zastavovaich dat pro export do souboru
function TBlkSCom.GetRychEvent(data:TBlkSComRychEvent):string;
begin
 if (data.speed = 0) then
  begin
   case (data.signal) of
    disabled : Result := '-1;';
    usek     : Result := '0;'+IntToStr(data.usekid)+';'+IntToStr(data.usekpart)+';';
    ir       : Result := '1;'+IntToStr(data.irid)+';';
   end;//case
  end else begin
   case (data.signal) of
    disabled : Result := '-1;';
    usek     : Result := '0;'+IntToStr(data.usekid)+';'+IntToStr(data.usekpart)+';'+IntToStr(data.speed)+';';
    ir       : Result := '1;'+IntToStr(data.irid)+';'+IntToStr(data.speed)+';';
   end;//case
  end;
end;//function

function TBlkSCom.GetSprTypes(sl:TStrings):string;
var i:Integer;
begin
 Result := '';
 for i := 0 to sl.Count-1 do
  Result := Result + sl[i] + ';';
end;//function

// format ev: (ev1)(ev2)(ev3)
// format ev1: RychEvent-zastaveni|RychEvent-zpomaleni|spr_typ1;spr_typ2;spr_typ3;...|min_delka|max_delka
function TBlkSCom.GetEvents():string;
var i:Integer;
begin
 Result := '';

 for i := 0 to Self.SComSettings.events.Count-1 do
  begin
   if (i > 0) then
    begin
     Result := Result + '(' +
                Self.GetRychEvent(Self.SComSettings.events[i].zastaveni) + '|' +
                Self.GetRychEvent(Self.SComSettings.events[i].zpomaleni) + '|' +
                Self.GetSprTypes(Self.SComSettings.events[i].spr_typ) + '|' +
                IntToStr(Self.SComSettings.events[i].delka.min) + '|' +
                IntToStr(Self.SComSettings.events[i].delka.max) + ')';

    end else begin
     Result := Result + '(' +
                Self.GetRychEvent(Self.SComSettings.events[i].zastaveni) + '|' +
                Self.GetRychEvent(Self.SComSettings.events[i].zpomaleni) + ')';
    end;//e sel i > 0
  end;//for i
end;//function

////////////////////////////////////////////////////////////////////////////////
//nastavovani stavovych promennych:

procedure TBlkSCom.SetNavest(navest:Integer);
var i:Integer;
begin
 if ((Self.SComStav.Navest < 0) or (Self.SComSettings.zamknuto)) then Exit();

 if (Navest = 8) then
  begin
   if (Self.SComStav.privol_timer_id > 0) then
     for i := 0 to Self.ORsRef.Cnt-1 do
       Self.ORsRef.ORs[i].BroadcastGlobalData('INFO-TIMER-RM;'+IntToStr(Self.SComStav.privol_timer_id));
   Self.SComStav.privol_timer_id := 0;
   Self.SComStav.privol_start := Now;
  end;

 if (Self.SComStav.Navest = navest) then Exit();

 Self.SComStav.Navest := navest;

 //reseni typu navestidla (scom/binary)
 if (Self.SComSettings.OutputType = scom) then
  begin
   //scom
   MTB.SetOutput(Self.SComSettings.MTBAddrs.data[0].board,Self.SComSettings.MTBAddrs.data[0].port,navest);
  end else begin
   //binary
   case (navest) of
    0,5,13,16..127:MTB.SetOutput(Self.SComSettings.MTBAddrs.data[0].board,Self.SComSettings.MTBAddrs.data[0].port,0);
   else//case (navest)
    MTB.SetOutput(Self.SComSettings.MTBAddrs.data[0].board,Self.SComSettings.MTBAddrs.data[0].port,1);
   end;//else case 0,5,13,16..127
  end;//else

 if (Self.SComStav.Navest = 0) then
  begin
   if ((Self.UsekPred <> nil) and ((Self.UsekPred.GetGlobalSettings().typ = _BLK_USEK) or (Self.UsekPred.GetGlobalSettings().typ = _BLK_TU))) then
    (Self.UsekPred as TBlkUsek).SComJCRef := nil;
  end;

 if (Self.autoblok) then
  begin
   if (TBlkTU(Self.UsekPred).nextTU <> nil) then TBlkTU(Self.UsekPred).nextTU.Change();
   if (TBlkTU(Self.UsekPred).Trat <> nil) then TBlkTrat(TBlkTU(Self.UsekPred).Trat).UpdateSprPredict();
  end;

 Self.UpdateRychlostSpr(true);
 Self.Change();
end;//procedure

procedure TBlkSCom.SetAB(ab:boolean);
begin
 if (Self.SComStav.AB = ab) then Exit();
 Self.SComStav.AB := ab;
 Self.Change();
end;//procedure

procedure TBlkSCom.SetZacatekVolba(typ:TBlkSComVolba);
begin
 if (Self.SComStav.ZacatekVolba = typ) then Exit();
 Self.SComStav.ZacatekVolba := typ;
 Self.Change();
end;//procedure

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
     if (Self.DNjc.RozpadBlok = -2) then
      begin
       Self.DNjc.RozpadBlok := -1;
       Self.DNjc.DN();
      end;
    end;
   Blky.SprPrediction(Self);
  end;//if Self.DNjc <> nil

 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkSCom.MenuVCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if (Self.SComRel.SymbolType = 1) then Exit;
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) then Exit;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.VC;
end;//procedure

procedure TBlkSCom.MenuVCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkSComVolba.none;
end;//procedure

procedure TBlkSCom.MenuPCStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) then Exit;

 Blk := BLky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.PC;
end;//procedure

procedure TBlkSCom.MenuPCStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBlkSComVolba.none;
end;//procedure

procedure TBlkSCom.MenuSTUJClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 // poradi musi byt zachovano !
 Self.Navest := 0;
 if (Self.DNjc = nil) then Exit();

 Self.DNjc.STUJ();
 JCDb.CheckNNavaznost(Self.DNjc);
 Blky.SprPrediction(Self);
end;//procedure

procedure TBlkSCom.MenuDNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.DNjc = nil) then Exit();

 Self.DNjc.DN();
 Blky.SprPrediction(Self);
end;//procedure

procedure TBlkSCom.MenuRCClick(SenderPnl:TIdContext; SenderOR:TObject);
var JC:TJC;
    Blk:TBlk;
begin
 if (Self.DNjc = nil) then Exit;

 JC := Self.DNjc;

 Blk := Self.UsekPred;
 if ((Blk = nil) or ((Blk.GetGlobalSettings().typ <> _BLK_USEK) and (Blk.GetGlobalSettings().typ <> _BLK_TU))) then
  begin
   // pokud blok pred JC neni -> 30 sekund
   (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 30, 0));
  end else begin
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.uvolneno) then
    begin
     // pokud neni blok pred JC obsazen -> 2 sekundy
     (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 2, 0));
    end else begin
     // polud je obsazen, zalezi na typu jizdni cesty
     case (JC.data.TypCesty) of
      TJCType.vlak  : (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0, 15, 0));   // vlakova cesta : 20 sekund
      TJCType.posun : (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 0,  5, 0));   // posunova cesta: 10 sekund
     else
      (SenderOR as TOR).AddMereniCasu(JC.RusJC, EncodeTime(0, 1, 0, 0));                   // nejaka divna cesta: 1 minuta
     end;
    end;
  end;

 JC.RusJCWithoutBlk();
 Blky.SprPrediction(Self);
 Self.DNjc := nil;
end;//procedure

procedure TBlkSCom.MenuABStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.AB := true;
end;//procedure

procedure TBlkSCom.MenuABStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.AB := false;
end;//procedure

procedure TBlkSCom.MenuLockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := true;
 Self.Navest := 0;
end;//procedure

procedure TBlkSCom.MenuUnlockClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZAM := false;
end;//procedure

procedure TBlkSCom.MenuPNStartClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
    i:Integer;
begin
 if (Self.SComRel.SymbolType = 1) then Exit;
 if ((SenderOR as TOR).stack.volba = PV) then
   if ((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) then Exit;

 Blk := Blky.GetBlkSComZacatekVolba((SenderOR as TOR).id);
 if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
 Self.ZacatekVolba := TBlkSComVolba.NC;

 for i := 0 to Self.OblsRizeni.Cnt-1 do
  Self.OblsRizeni.ORs[i].ORDKClickServer(Self.PrivolDKClick);
end;//procedure

procedure TBlkSCom.MenuPNStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.ZacatekVolba := TBLkSComVolba.none;
end;//procedure

procedure TBlkSCom.MenuPPNClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Prodloužení doby pøivolávací návìsti', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkSCom.MenuRNZClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Assigned(Self.privol)) then
   ORTCPServer.Potvr(SenderPnl, Self.RNZPotvrSekv, SenderOR as TOR, 'Zrušení nouzových závìrù po nouzoé cestì', TBlky.GetBlksList(Self), Self.privol.GetRNZ());
end;//procedure

procedure TBlkSCom.MenuKCDKClick(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 if (Self.ZacatekVolba = TBlkSComVolba.NC) then
  begin
   for i := 0 to Self.OblsRizeni.Cnt-1 do
    Self.OblsRizeni.ORs[i].ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí pøivolávací návìsti', TBLky.GetBlksList(Self), nil);
  end;
end;

procedure TBlkSCom.MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.SComStav.redukce_menu := 0;
 Self.Change();
end;//procedure

procedure TBlkSCom.MenuAdminStopIR(SenderPnl:TIdContext; SenderOR:TObject; enabled:boolean);
var Blk:TBlk;
begin
 if (Self.SComSettings.events[0].zastaveni.signal = TBlkSComSignal.ir) then
  begin
   Blky.GetBlkByID(Self.SComSettings.events[0].zastaveni.irid, Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_IR)) then Exit();
   if (enabled) then
     MTB.SetInput(TBlkIR(Blk).GetSettings().MTBAddrs.data[0].board, TBlkIR(Blk).GetSettings().MTBAddrs.data[0].port, 1)
   else
     MTB.SetInput(TBlkIR(Blk).GetSettings().MTBAddrs.data[0].board, TBlkIR(Blk).GetSettings().MTBAddrs.data[0].port, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights);
begin
 if (Self.SComStav.Navest = -1) then Exit();

 case (Button) of
  right,F2 : ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
  left     : begin
    if (((Self.Navest > 0) or (JCDb.FindJC(Self.GlobalSettings.id, false) > -1)) and ((SenderOR as TOR).stack.volba = TORStackVOlba.PV)) then
      ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights))
    else
      if ((not Self.SComSettings.zamknuto) and (not Self.autoblok)) then Self.MenuVCStartClick(SenderPnl, SenderOR);
  end;
  middle   : if ((not Self.SComSettings.zamknuto) and (not Self.autoblok)) then Self.MenuPCStartClick(SenderPnl, SenderOR);
 end;//case
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSCom.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
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
 else if (item = 'PPN')  then Self.MenuPPNClick    (SenderPnl, SenderOR)
 else if (item = 'RNZ')  then Self.MenuRNZClick    (SenderPnl, SenderOR)
 else if (item = 'IR>')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, true)
 else if (item = 'IR<')  then Self.MenuAdminStopIR (SenderPnl, SenderOR, false)
 else if (item = 'KC')   then Self.MenuKCDKClick   (SenderPnl, SenderOR);

 if (item = 'ZRUŠ REDUKCI') then Self.MenuAdminREDUKClick(SenderPnl, SenderOR);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro konkretni s-com:
function TBlkSCom.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk:TBlk;
begin
 Result := inherited;

 // pokud je navestidlo trvale zamkle, neumoznime zadne volby
 if (Self.SComSettings.zamknuto) then Exit();

 if ((((((Self.DNjc = nil) and (JCDb.FindJC(Self.GetGlobalSettings().id, true) = -1)) or (Self.DNjc.RozpadBlok > 0)) and (Self.Navest <> 8))
    or ((SenderOR as TOR).stack.volba = VZ) or (Self.SComStav.ZacatekVolba <> TBlkSCOmVolba.none)) and
    (not Self.autoblok)) then
  begin
    case (Self.SComStav.ZacatekVolba) of
     TBlkSCOmVolba.VC : Result := Result + 'VC<,';
     TBlkSCOmVolba.PC : Result := Result + 'PC<,';
     TBlkSCOmVolba.NC : Result := Result + 'PN<,';
    else
      //2 = VC, 3= PC
      if (Self.SComRel.SymbolType <> 1) then
        Result := Result + 'VC>,!PN>,';
       Result := Result + 'PC>,';
    end;// else ZacatekVOlba <> none ...

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

   if ((Self.Navest > 0) or (Self.DNjc.CanDN()) or (Self.DNjc.RozpadBlok < 1)) then
    begin
     Result := Result + 'RC,';

     // AB lze jen u vlakove cesty
     if (Self.DNjc.data.TypCesty = TJCType.vlak) then
      begin
       if (Self.SComStav.AB) then
         Result := Result + 'AB<,'
        else
         Result := Result + 'AB>,'
      end;
    end;
 end;

 //7=ZAM
 if (Self.SComStav.ZAM) then
   Result := Result + 'ZAM<,'
  else
   Result := Result + 'ZAM>,';

 if ((Self.Navest <> 8) and (Self.privol <> nil)) then
  Result := Result + '!RNZ,';

 if (rights >= TORControlRights.superuser) then
   if (Self.SComStav.redukce_menu > 0) then
     Result := Result + '-,*ZRUŠ REDUKCI,';

 // DEBUG: jednoduche nastaveni IR pri simulator.dll
 if (MTB.lib = 'simulator.dll') then
  begin
   if (Self.SComSettings.events[0].zastaveni.signal = TBlkSComSignal.ir) then
    begin
     Blky.GetBlkByID(Self.SComSettings.events[0].zastaveni.irid, Blk);
     if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_IR)) then
      begin
       case (TBlkIR(Blk).Stav) of
         TIRStav.uvolneno : Result := Result + '-,*IR>,';
         TIRStav.obsazeno : Result := Result + '-,*IR<,';
       end;//case
      end;
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

class function TBlkSCom.NavestToString(Navest:Integer):string;
 begin
  case (Navest) of
   -1:Result  := 'disabled';
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
 end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RedukujMenu();
begin
 Self.SComStav.redukce_menu := Self.SComStav.redukce_menu + 1;

 // prave zacala redukce
 if (Self.SComStav.redukce_menu = 1) then
  Self.Change();
end;//procedure

procedure TBlkSCom.ZrusRedukciMenu();
begin
 if (Self.SComStav.redukce_menu > 0) then
  Self.SComStav.redukce_menu := Self.SComStav.redukce_menu - 1;

 // prave skoncila redukce
 if (Self.SComStav.redukce_menu = 0) then
  Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.JCZrusNavest();
begin
 if (Self.SComSettings.ZpozdeniPadu > 0) then
  begin
   Self.SComStav.padani       := true;
   Self.SComStav.padani_start := Now;
   writelog('Návìstidlo '+Self.GlobalSettings.name+': spoždìní pádu '+IntToStr(Self.SComSettings.ZpozdeniPadu)+' s', WR_VC, 0);
  end else begin
   Self.Navest := 0;
  end;

 Self.UpdateRychlostSpr(true);
end;//procedure

procedure TBlkSCom.UpdatePadani();
begin
 if (not Self.SComStav.padani) then Exit();

 if (Self.SComStav.padani_start + EncodeTime(0, Self.SComSettings.ZpozdeniPadu div 60, Self.SComSettings.ZpozdeniPadu mod 60, 0) < Now) then
  begin
   Self.Navest := 0;
   Self.SComStav.padani := false;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// aktualizace rychlosti souprav pøed návìstidlem
// pozor na padání !
// force nucene zastavi vlak, resp. nastavi jeho rychlost
//  metoda je volana s force v pripade, kdy dochazi k prime zmene navesti od uzivatele (STUJ, DN, RC)
procedure TBlkSCom.UpdateRychlostSpr(force:boolean = false);
var Usek, SCom:TBlk;
    spr:TSouprava;
    zpomal:Integer;
begin
 Usek := Self.UsekPred;
 if (Self.SComRel.SymbolType = 1) then Exit();          // pokud jsem posunove navestidlo, koncim funkci
 if ((Usek = nil) or ((Usek.GetGlobalSettings().typ <> _BLK_USEK) and (Usek.GetGlobalSettings().typ <> _BLK_TU))) then Exit();    // pokud pred navestidlem neni usek, koncim funkci
 if ((Usek as TBlkUsek).Souprava = -1) then Exit();     // pokud na useku prede mnou neni souprava, koncim funkci
 spr := Soupravy.soupravy[(Usek as TBlkUsek).Souprava];
 if (spr.front <> Usek) then Exit();                    // pokud souprava svym predkem neni na bloku pred navestidlem, koncim funkci

 ///////////////////////////////////////////////////

 // ZPOMALOVANI
 zpomal := Self.IsZpomalEvent();
 if ((zpomal > -1) and (spr.rychlost > Self.SComSettings.events[zpomal].zpomaleni.speed) and ((Usek as TBLkUsek).zpomalovani_ready) and
    ((not Assigned(Self.DNjc)) or (not Self.IsPovolovaciNavest())) and (spr.smer = Self.SComRel.smer)) then
  begin
   spr.rychlost := Self.SComSettings.events[zpomal].zpomaleni.speed;
   (Usek as TBLkUsek).zpomalovani_ready := false;
  end;

 ///////////////////////////////////////////////////

 // ZASTAVOVANI, resp. nastavovani rychlosti prislusne JC
 if ((Self.IsZastavEvent() > -1) or (force)) then       // podminka IsRychEvent take resi to, ze usek musi byt obsazeny (tudiz resi vypadek useku)
  begin
   if ((Assigned(Self.DNjc)) and (Self.DNjc.data.TypCesty = TJCType.vlak)) then
    begin
     // je JC -> je postaveno?
     if ((Self.IsPovolovaciNavest()) and (not Self.SComStav.padani)) then
      begin
       // je postaveno -> zkontrlolujeme, jestli je postaveno dalsi navestidlo
       if ((spr.rychlost > 0) and (spr.smer <> Self.SComRel.smer)) then   Exit(); // pokud jede souprava opacnym smerem, kaslu na ni

       if (Self.DNjc.data.DalsiNNavaznostTyp = 2) then
        begin
         // zjistime dalsi navestidlo
         Blky.GetBlkByID(Self.DNjc.data.DalsiNNavaznost, SCom);

         if ((SCom <> nil) and (SCom.GetGlobalSettings().typ = _BLK_SCOM) and ((SCom as TBlkSCom).IsPovolovaciNavest())) then
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
      end else begin
       // neni povolovaci navest -> zastavit LOKO
       if ((spr.smer = Self.SComRel.smer) and (spr.rychlost <> 0)) then
         spr.SetRychlostSmer(0, Self.SComRel.smer);
      end;
    end else begin
     // nenalezena jizdni cesta -> muze se jedna o navestidlo v autobloku
     if (spr.smer = Self.SComRel.smer) then
      begin
       if ((Self.IsPovolovaciNavest()) and (not Self.SComStav.padani) and
           (Self.UsekPred.GetGlobalSettings.typ = _BLK_TU) and (TBlkTU(Self.UsekPred).InTrat > -1)) then
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

// writelog()
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.IsZastavEvent():Integer;
var i, j:Integer;
    spr:TSouprava;
    Usek:TBlk;
begin
 if (Self.SComSettings.events.Count = 0) then Exit(-1);

 Usek := Self.UsekPred;
 if ((Usek as TBlkUsek).Souprava < 0) then
  begin
   // na bloku neni zadana souprava
   if (Self.IsRychEvent(Self.SComSettings.events[0].zastaveni)) then Result := 0 else Result := -1;
  end else begin
   spr := Soupravy.soupravy[(Usek as TBlkUsek).Souprava];

   // hledame takovy event, ktery odpovida nasi souprave
   for i := 0 to Self.SComSettings.events.Count-1 do
    begin
     if ((spr.delka >= Self.SComSettings.events[i].delka.min) and (spr.delka <= Self.SComSettings.events[i].delka.max)) then
      begin
       for j := 0 to Self.SComSettings.events[i].spr_typ.Count-1 do
        begin
         if (spr.typ = Self.SComSettings.events[i].spr_typ[j]) then
          begin
           if (Self.IsRychEvent(Self.SComSettings.events[i].zastaveni)) then Result := i else Result := -1;
           Exit();
          end;
        end;
      end;
    end;//for i

   // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
   if (Self.IsRychEvent(Self.SComSettings.events[0].zastaveni)) then Result := 0 else Result := -1;
  end;
end;//function

function TBlkSCom.IsZpomalEvent():Integer;
var i, j:Integer;
    spr:TSouprava;
    Usek:TBlk;
begin
 if (Self.SComSettings.events.Count = 0) then Exit(-1);

 Usek := Self.UsekPred;
 if ((Usek as TBlkUsek).Souprava < 0) then
  begin
   // na bloku neni zadana souprava
   if (Self.IsRychEvent(Self.SComSettings.events[0].zpomaleni)) then Result := 0 else Result := -1;
  end else begin
   spr := Soupravy.soupravy[(Usek as TBlkUsek).Souprava];

   // hledame takovy event, ktery odpovida nasi souprave
   for i := 0 to Self.SComSettings.events.Count-1 do
    begin
     if ((spr.delka >= Self.SComSettings.events[i].delka.min) and (spr.delka <= Self.SComSettings.events[i].delka.max)) then
      begin
       for j := 0 to Self.SComSettings.events[i].spr_typ.Count-1 do
        begin
         if (spr.typ = Self.SComSettings.events[i].spr_typ[j]) then
          begin
           if (Self.IsRychEvent(Self.SComSettings.events[i].zpomaleni)) then Result := i else Result := -1;
           Exit();
          end;
        end;
      end;
    end;//for i

   // pokud jsme event odpovidajici parametrum soupravy nenasli, vyhodnocujeme globalni event
   if (Self.IsRychEvent(Self.SComSettings.events[0].zpomaleni)) then Result := 0 else Result := -1;
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////

// zjistuje, jestli nastala udalost zastaveni, ci zpomaleni
class function TBlkSCom.IsRychEvent(data:TBlkSComRychEvent):boolean;
var Blk:TBlk;
    obsz:TUsekStavAr;
begin
 case (data.signal) of
  TBlkSComSignal.disabled: Exit(false);
  TBlkSComSignal.usek: begin
    Blky.GetBlkByID(data.usekid, Blk);
    if ((Blk = nil) or ((Blk.GetGlobalSettings().typ <> _BLK_USEK) and (Blk.GetGlobalSettings().typ <> _BLK_TU))) then Exit(true);
    (Blk as TBlkUsek).GetObsazeno(obsz);
    if (obsz[data.usekpart] = TUsekStav.obsazeno) then
     Exit(true);
  end;
  TBlkSComSignal.IR:begin
    Blky.GetBlkByID(data.irid, Blk);
    if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_IR)) then Exit(true);
    if ((Blk as TBlkIR).Stav = TIrStav.obsazeno) then
     Exit(true);
  end;
 end;//case

 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.IsPovolovaciNavest(jctype:TJCType = TJCType.vlak):boolean;
begin
 // zatim jen pro vlakovou cestu
 case (Self.Navest) of
  1..4, 6, 7, 11, 12, 14, 15: Result := true;
 else
  Result := false;
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.UpdatePrivol();
var i:Integer;
begin
 if ((Self.SComStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now+EncodeTime(0, 0, 30, 0)) and (Self.SComStav.privol_timer_id = 0)) then
  begin
   // oznameni o brzkem ukonceni privolavaci navesti
   Self.SComStav.privol_timer_id := Random(65536)+1;
   for i := 0 to Self.ORsRef.Cnt-1 do
     Self.ORsRef.ORs[i].BroadcastGlobalData('INFO-TIMER;'+IntToStr(Self.SComStav.privol_timer_id)+';0;30;PN '+Self.GlobalSettings.name);
  end;

 if (Self.SComStav.privol_start+EncodeTime(0, _PRIVOL_MIN, _PRIVOL_SEC, 0) < Now) then
  begin
   // pad privolavaci navesti
   Self.Navest := 0;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// privolavaci navest bez podpory zabezpecovaciho zarizeni
procedure TBlkSCom.PrivolDKClick(SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton);
var i:Integer;
begin
 if (Button = TPanelButton.left) then
  begin
   for i := 0 to Self.OblsRizeni.Cnt-1 do
    Self.OblsRizeni.ORs[i].ORDKClickClient();
   ORTCPServer.Potvr(SenderPnl, Self.PrivokDKPotvrSekv, SenderOR as TOR, 'Zapnutí pøivolávací návìsti', TBLky.GetBlksList(Self), nil);
  end else begin
   if ((Button = TPanelButton.F2) or (Button = TPanelButton.right)) then
     ORTCPServer.Menu(SenderPnl, Self, TOR(SenderOR), '$'+TOR(SenderOR).Name + ',-,' + 'KC');
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.PrivokDKPotvrSekv(Sender:TIdContext; success:boolean);
begin
 if (success) then
  begin
   Self.SComStav.ZacatekVolba := TBlkSComVolba.none;
   Self.Navest := 8;
  end else begin
   self.ZacatekVolba := TBlkSComVolba.none;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.RNZPotvrSekv(Sender:TIdContext; success:boolean);
begin
 if ((success) and (Assigned(Self.privol))) then
  begin
   Self.privol.RNZ();
   Self.privol := nil;
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSCom.SetUsekPredID(new_id:Integer);
var i:Integer;
    ev:TBlkSComSprEvent;
begin
 if (Self.SComRel.UsekID = new_id) then Exit();
 Self.SComRel.UsekID := new_id;

 for i := 0 to Self.SComSettings.events.Count-1 do
  begin
   ev := Self.SComSettings.events[i];
   ev.zastaveni.usekid := Self.SComRel.UsekID;
   ev.zpomaleni.usekid := Self.SComRel.UsekID;
   Self.SComSettings.events[i] := ev;
  end;

 if (new_id = -1) then Self.autoblok := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSCom.GetUsekPred():TBlk;
begin
 if (((Self.fUsekPred = nil) and (Self.UsekID <> -1)) or ((Self.fUsekPred <> nil) and (Self.UsekID <> Self.fUsekPred.GetGlobalSettings.id))) then
   Blky.GetBlkByID(Self.UsekID, Self.fUsekPred);
 Result := Self.fUsekPred;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
