unit TOblRizeni;

//tato unita se stara o rizeni Oblasti rizeni (tedy stanic)
//OR slouzi jako prostrednik mezi technologickymi bloky (technologii) a panely (zobrazenim)

// blok vola metodu primo do prislusne oblasti rizeni, kde jsou data v pripade,
//  ze patri konkretni OR, rovnou odeslane do soketu
//   neni tedy vyuzivano ORTCPServer

interface

uses Types, IniFiles, SysUtils, Classes, RPConst, Graphics, Menus,
      IdContext, TechnologieMTB, StrUtils, Souprava, ComCtrls, Forms,
      Generics.Collections, Zasobnik, User, Messages, Windows;

const
  _MAX_OSV = 8;
  _MAX_CON_PNL = 16;

  _POTVR_SEKV_BLOK     = 0;
  _POTVR_SEKV_CALLBACK = 3;

type
  //1 osvetleni
  TOsv = record
   board:Byte;
   port:Byte;
   name:string;  //max 5 znaku
  end;

 //primarni vlastnosti kazde OR
 TORProp = record
  Name:string;
  ShortName:string;
  id:string;
  Osvetleni:TList<TOsv>;
 end;

 TMericCallback = procedure () of object;
 TPSCallback = procedure (Relief:TObject; Panel:TObject; success:boolean) of object;
 TBlkCallback = procedure (SenderPnl:TIDContext; SenderOR:TObject; Button:TPanelButton) of object;

 TMereniCasu = record
  Start:TDateTime;
  Length:TDateTime;
  callback:TMericCallback;
  id:Integer;
 end;

 //databaze pripojenych panelu
 //kazda OR si pamatuje, jake panely jsou k ni pripojeny a s temito panely komunikuje
 // toto je 1 prvek databaze pripojenych panelu
 TORPanel = record
  Panel:TIdContext;                                 // zde je ulozeno spojeni na klienta
  Rights:TORControlRights;
  user:string;
 end;

 TORStav = record
  NUZtimer:boolean;         // rika, jestli uz probiha potvrzovaci sekvence NUZ
  NUZblkCnt:Integer;        // kolik bloku ma zaplych NUZ
  NUZmerCasuID:Integer;
  ZkratBlkCnt:Integer;
  ZadostBlkCnt:Integer;     // pocet uvazek, na ktere je zadost o tratovy souhlas (kvuli prehravani zvuku)

  spr_new:boolean;
  spr_edit:TSouprava;
  spr_usek:TObject;         // TBlkUsek
  dk_click_callback:TBlkCallback;


  reg_please:TIdCOntext;    // zde je ulozen regulator, ktery danou oblast rizeni zada o prideleni lokomotivy
 end;

 TORMTB = record
  present:boolean;
  failed:boolean;
 end;

 TORMTBs = record
  MTBs: array [0..TMTB._MAX_MTB] of TORMTB;
  failture:boolean;
  last_failture_time:TDateTime;
 end;

  PTOR = ^TOR;
  TOR = class
    private const
      //chybove hlasky komunikace
      _COM_ACCESS_DENIED = 'Pøístup odepøen';

      //levely opravneni
      _R_no        = 0;
      _R_read      = 1;
      _R_write     = 2;
      _R_superuser = 3;

    private
      findex:Integer;
      ORProp:TORProp;
      ORStav:TORStav;

      MereniCasu:TList<TMereniCasu>;

      OR_MTB:TORMTBs;

      //funkce pro praci s databazi pripojenych panelu
      function PnlDAdd(Panel:TIdContext; rights:TORControlRights; user:string):Byte;
      function PnlDRemove(Panel:TIdContext):Byte;
      function PnlDGetRights(Panel:TIdContext):TORControlRights;
      function PnlDGetIndex(Panel:TIdContext):Integer;

      procedure NUZTimeOut();
      procedure NUZ_PS(Sender:TIdContext; success:boolean); // callback z NUZ pro DK

      procedure ORAuthoriseResponse(Panel:TIdContext; Rights:TORControlRights; msg:string);

      procedure SetNUZBlkCnt(new:Integer);
      procedure SetZkratBlkCnt(new:Integer);
      procedure SetZadostBlkCnt(new:Integer);

      procedure MTBClear();
      procedure MTBUpdate();

      procedure SendStatus(panel:TIdContext);

      // tyto funkce jsou volany pri zmene opravenni mezi cteni a zapisem
      // primarni cil = v techto funkcich resit zapinani a vypinani zvuku v panelu
      procedure AuthReadToWrite(panel:TIdContext);
      procedure AuthWriteToRead(panel:TIdContext);

    public

      stack:TORStack;
      changed:boolean;
      vb:TList<TObject>;        // seznam variantnich bodu, ktere jsou aktualne "naklikle"; zde je ulozen seznam bloku
      Connected:TList<TORPanel>;

      constructor Create(index:Integer);
      destructor Destroy(); override;

      function LoadData(str:string):Byte;

      procedure RemoveClient(Panel:TIdContext);

      procedure Update();     // pravidelna aktualizace - zatim na mereni casu
      procedure DisconnectPanels();

      function AddMereniCasu(callback:TMericCallback; len:TDateTime):Byte;      // vraci id mereni
      procedure StopMereniCasu(id:Integer);

      procedure MTBAdd(addr:integer);
      procedure MTBFail(addr:integer);

      procedure UpdateLine(LI:TListItem);

      procedure BroadcastData(data:string; min_rights:TORControlRights = read);
      procedure BroadcastGlobalData(data:string; min_rights:TORControlRights = read);

      procedure ClearVb();

      //--- komunikace s technologickymi bloky ---
      procedure BlkChange(Sender:TObject; specificClient:TIDContext = nil);   // pokdu odesilame data pouze jednomu specifickemu klientovi -- pri GET-ALL
      procedure BlkPlaySound(Sender:TObject; min_rights:TORCOntrolRights; sound:Integer; delay_ms:Integer = -1);
      procedure BlkRemoveSound(Sender:TObject; sound:Integer);
      procedure BlkWriteError(Sender:TObject; error:string; system:string);      // posle chybovou hlasku do vsech stanic, ktere maji autorizovany zapis
      procedure BlkNewSpr(Sender:TObject; Panel:TIdContext);
      procedure BlkEditSpr(Sender:TObject; Panel:TIdContext; Souprava:TSouprava);

      function ORSendMsg(Sender:TOR; msg:string):Byte;

      procedure ORDKClickServer(callback:TBlkCallback);
      procedure ORDKClickClient();

      // volany pri zadosti o poskytnuti loko pro regulator:
      function LokoPlease(Sender:TIDContext; user:TUser; comment:string):Integer;
      procedure LokoCancel(Sender:TIdContext);

      //--- komunikace s panely zacatek: ---
      procedure PanelAuthorise(Sender:TIdContext; rights:TORControlRights; username:string; password:string);
      procedure PanelFirstGet(Sender:TIdContext);
      procedure PanelClick(Sender:TIdContext; blokid:Integer; Button:TPanelButton);
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

      procedure PanelHVAdd(Sender:TIDContext; str:string);
      procedure PanelHVRemove(Sender:TIDContext; addr:Integer);
      procedure PanelHVEdit(Sender:TIDContext; str:string);

      procedure PanelSendOsv(Sender:TIdContext);
      procedure PanelSetOsv(Sender:TIdCOntext; id:string; state:boolean);

      function PanelGetSprs(Sender:TIdCOntext):string;
      procedure PanelRemoveSpr(Sender:TIDContext; spr_index:Integer);

      function GetORPanel(conn:TIdContext; var ORPanel:TORPanel):Integer;
      class function GetRightsString(rights:TORControlRights):string;

      procedure UserUpdateRights(user:TUser);
      procedure UserDelete(userid:string);

      property NUZtimer:Boolean read ORStav.NUZtimer write ORStav.NUZtimer;
      property NUZblkCnt:Integer read ORStav.NUZblkCnt write SetNUZBlkCnt;
      property ZKratBlkCnt:Integer read ORStav.ZKratBlkCnt write SetZkratBlkCnt;
      property ZadostBlkCnt:Integer read ORStav.ZadostBlkCnt write SetZadostBlkCnt;
      property reg_please:TIdContext read ORStav.reg_please;

      //--- komunikace s panely konec ---

      property Name:string read ORProp.Name;
      property ShortName:string read ORProp.ShortName;
      property id:string read ORProp.id;
  end;//TOR

implementation

////////////////////////////////////////////////////////////////////////////////

uses TBloky, GetSystems, TBlokVyhybka, TBlokUsek, TBlokSCOm, Main,
     TechnologieJC, TBlokPrejezd, TJCDatabase, Prevody, TCPServerOR,
     TBlokUvazka, TBlokTrat, TOblsRizeni, TBlok, THVDatabase, SprDb,
     Logging, UserDb, THnaciVozidlo, Trakce, TBlokZamek,
     Regulator, TBlokRozp, RegulatorTCP;

constructor TOR.Create(index:Integer);
begin
 inherited Create();

 Self.findex := index;

 Self.ORProp.Osvetleni := TList<TOsv>.Create();
 Self.Connected        := TList<TORPanel>.Create();
 Self.MTBClear();

 Self.ORStav.spr_new  := false;
 Self.ORStav.spr_edit := nil;
 Self.ORStav.spr_usek := nil;
 Self.ORStav.dk_click_callback := nil;
 Self.ORStav.reg_please := nil;

 Self.stack   := TORStack.Create(index+1, Self);
 Self.vb      := TList<TObject>.Create();
 Self.changed := false;

 Self.MereniCasu := TList<TMereniCasu>.Create();
end;//ctor

destructor TOR.Destroy();
begin
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
function TOR.LoadData(str:string):Byte;
var data_main,data_osv,data_osv2:TStrings;
    j:Integer;
    Osv:TOsv;
begin
 data_main := TStringList.Create();
 data_osv  := TStringList.Create();
 data_osv2 := TStringList.Create();

 ExtractStrings([';'],[],PChar(str), data_main);

 if (data_main.Count < 3) then
  begin
   Result := 2;
   Exit;
  end;

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
       Osv.board := StrToInt(data_osv2[0]);
       Osv.port  := StrToInt(data_osv2[1]);
       Osv.name  := data_osv2[2];
       Self.ORProp.Osvetleni.Add(Osv);
     except

     end;
    end;//for j
  end;

 FreeAndNil(data_main);
 FreeAndNil(data_osv);
 FreeAndNil(data_osv2);
 Result := 0;
end;//procedure

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
    fg, bg:TColor;
    Blk:TBlk;
begin
 if (Self.Connected.Count = 0) then Exit();

 blk_glob := (Sender as TBlk).GetGlobalSettings;

 fg := clFuchsia;
 bg := clBlack;
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
       end;//case
      end;
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
//      usek : konec_jc;[souprava;barva_soupravy;sipkaLsipkaS] -  posledni 3 argumenty jsou nepovinne

   if ((Sender as TBlkUsek).Obsazeno = TUsekStav.disabled) then
    begin
     msg := msg + PrevodySoustav.ColorToStr(clFuchsia) + ';' + PrevodySoustav.ColorToStr(clBlack) + ';0;0;-;';
    end else begin
     case ((Sender as TBlkUsek).Obsazeno) of
      TUsekStav.disabled : fg := clFuchsia;
      TUsekStav.none     : fg := $A0A0A0;
      TUsekStav.uvolneno : fg := $A0A0A0;
      TUsekStav.obsazeno : fg := clRed;
     end;//case

     if ((fg = $A0A0A0) and ((Sender as TBlkUsek).InTrat > -1)) then
      begin
       Blky.GetBlkByID((Sender as TBlkUsek).InTrat, Blk);
       if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_TRAT)) then
         if ((Blk as TBlkTrat).ZAK) then
          fg := clBlue;
      end;

     // usekum v trati se nezobrazuje zaver
     if ((((Sender as TBlkUsek).Obsazeno) = TUsekStav.uvolneno) and (((Sender as TBlkUsek).InTrat = -1) or ((Sender as TBlkUsek).Zaver = nouz))) then
      begin
       case ((Sender as TBlkUsek).Zaver) of
        vlak   : fg := clLime;
        posun  : fg := clWhite;
        nouz   : fg := clAqua;
       end;//case
      end;

     msg := msg + PrevodySoustav.ColorToStr(fg) + ';';

     if ((Sender as TBlkUsek).Stitek <> '') then bg := clTeal;
     if ((Sender as TBlkUsek).Vyluka <> '') then bg := clOlive;

     if ((Sender as TBlkUsek).ZesZkrat)        then bg := clFuchsia;
     if (not (Sender as TBlkUsek).ZesNapajeni) then bg := clBlue;

     msg := msg + PrevodySoustav.ColorToStr(bg) + ';';

     if ((Sender as TBlkUsek).NUZ) then
      msg := msg + '1;'
     else
      msg := msg + '0;';

     msg := msg + IntToStr(Integer((Sender as TBlkUsek).KonecJC)) + ';';

     if ((Sender as TBlkUsek).VlakPresun) then
      msg := msg + PrevodySoustav.ColorToStr(clYellow) + ';'
     else
      msg := msg + '-;';

     if ((Sender as TBlkUsek).Souprava > -1) then
      begin
       if (Assigned(Soupravy.soupravy[(Sender as TBlkUsek).Souprava])) then
        begin
         msg := msg + Soupravy.GetSprNameByIndex((Sender as TBlkUsek).Souprava)+';';
         if ((Sender as TBlkUsek).Obsazeno = uvolneno) then fg := clAqua;
         msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
         if (Soupravy.soupravy[(Sender as TBlkUsek).Souprava].sdata.smer_L) then
           msg := msg + '1'
         else
           msg := msg + '0';
         if (Soupravy.soupravy[(Sender as TBlkUsek).Souprava].sdata.smer_S) then
           msg := msg + '1;'
         else
           msg := msg + '0;';
        end;

//        if (Soupravy.soupravy[(Sender as TBlkUsek).Souprava].ukradeno) then
//         msg := msg + PrevodySoustav.ColorToStr(clYellow)
//        else
         msg := msg + PrevodySoustav.ColorToStr(bg);

      end else begin

       if ((Sender as TBlkUsek).SprPredict > -1) then
         msg := msg + Soupravy.GetSprNameByIndex((Sender as TBlkUsek).SprPredict)+';'+PrevodySoustav.ColorToStr(fg)+';00;';
      end;// else Souprava > -1
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
         if (((Sender as TBlkSCom).Navest <> 8) and ((Sender as TBlkSCom).privol <> nil)) then
           fg := clYellow
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
      TBlkSComVolba.NC   : bg := clTeal;
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

   if ((Sender as TBlkPrejezd).Vyluka <> '') then bg := clOlive
   else if ((Sender as TBlkPrejezd).Stitek <> '') then bg := clTeal;

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

   if ((Sender as TBlkUvazka).Stitek <> '') then bg := clTeal
   else bg := clBlack;

   if (((Sender as TBlkUvazka).parent as TBlkTrat).ZAK) then fg := clRed
   else if (((Sender as TBlkUvazka).parent as TBlkTrat).RBPCan) then fg := clRed
   else if (((Sender as TBlkUvazka).parent as TBlkTrat).Zaver) then fg := clBlue
   else if (((Sender as TBlkUvazka).parent as TBlkTrat).nouzZaver) then fg := clAqua
   else if (((Sender as TBlkUvazka).parent as TBlkTrat).Obsazeno) then fg := clBlue
   else fg := $A0A0A0;

   msg := msg + PrevodySoustav.ColorToStr(fg) + ';';
   msg := msg + PrevodySoustav.ColorToStr(bg) + ';';
   if (((Sender as TBlkUvazka).parent as TBlkTrat).Zadost) then
    msg := msg + '1;'
   else
    msg := msg + '0;';

   // smer trati
   case (((Sender as TBlkUvazka).parent as TBlkTrat).Smer) of
    TTratSmer.disabled : msg := msg + '-5;';
    TTratSmer.zadny    : msg := msg + '0;';
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
  end;//_BLK_ZAMEK

 else Exit; end;

 //odeslani do OR
 for i := 0 to Self.Connected.Count-1 do
  begin
   if (Self.Connected[i].Rights < TORControlRights.read) then continue;
   if ((specificClient <> nil) and (Self.Connected[i].Panel <> specificClient)) then continue;

   ORTCPServer.SendLn(Self.Connected[i].Panel, msg);

   // aktualizace menu
   if ((Self.Connected[i].Panel.Data as TTCPORsRef).menu = Sender) then
    begin
     case (blk_glob.typ) of
      _BLK_VYH     : (Sender as TBlkVyhybka).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
      _BLK_USEK    : (Sender as TBlkUsek   ).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
      _BLK_SCOM    : (Sender as TBlkSCom   ).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
      _BLK_PREJEZD : (Sender as TBlkPrejezd).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
      _BLK_UVAZKA  : (Sender as TBlkUvazka ).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
      _BLK_ZAMEK   : (Sender as TBlkZamek  ).ShowPanelMenu(Self.Connected[i].Panel, Self, Self.Connected[i].Rights);
     end;//case
    end;

  end;//for i

end;//procedure

procedure TOR.BlkWriteError(Sender:TObject; error:string; system:string);
var i:Integer;
begin
 Writelog(Self.Name + ' : ' + system + ' : ' + error, WR_ERROR, 100);

 for i := 0 to Self.Connected.Count-1 do
  if (Self.Connected[i].Rights >= TORControlRights.write) then
    ORTCPServer.BottomError(Self.Connected[i].Panel, error, Self.ShortName, system);
end;//procedure

procedure TOR.BlkPlaySound(Sender:TObject; min_rights:TORCOntrolRights; sound:Integer; delay_ms:Integer = -1);
var i:Integer;
begin
 for i := 0 to Self.Connected.COunt-1 do
  if (Self.Connected[i].Rights >= min_rights) then
   ORTCPServer.PlaySound(Self.Connected[i].Panel, sound, delay_ms);
end;//procedure

procedure TOR.BlkRemoveSound(Sender:TObject; sound:Integer);
var i:Integer;
begin
 for i := 0 to Self.Connected.Count-1 do
  ORTCPServer.DeleteSound(Self.Connected[i].Panel, sound);
end;//procedure

procedure TOR.BlkNewSpr(Sender:TObject; Panel:TIdContext);
begin
 Self.ORStav.spr_new  := true;
 Self.ORStav.spr_usek := Sender;
 ORTCPServer.SendLn(Panel, Self.id+';SPR-NEW;');
end;//procedure

procedure TOR.BlkEditSpr(Sender:TObject; Panel:TIdContext; Souprava:TSouprava);
begin
 Self.ORStav.spr_new  := false;
 Self.ORStav.spr_edit := Souprava;
 Self.ORStav.spr_usek := Sender;

 ORTCPServer.SendLn(Panel, Self.id+';'+'SPR-EDIT;'+Souprava.GetPanelString());
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
     //pokud uz je zaznam v databazi, dame do databaze atualni nastavovana prava (vyuzivano pri DP)
     pnl := Self.Connected[i];
     pnl.Rights := rights;
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
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Úspìšnì autorizováno - odpojen');
   ORTCPServer.GUIRefreshLine((Sender.Data as TTCPORsRef).index);
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
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Uživatel '+username+' neexistuje !');
   Exit();
  end;

 // kontrola BANu uzivatele
 if (user.ban) then
  begin
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Uživatel '+user.id+' má BAN !');
   Exit();
  end;

 // kontrola opravneni uzivatele pro tento panel
 if (not TUser.ComparePasswd(password, user.password)) then
  begin
   UserRights := TORControlRights.null;
  end else begin
   UserRights := user.GetRights(Self.id);
  end;

 // do last_rights si ulozime posledni opravneni panelu
 last_rights := Self.PnlDGetRights(Sender);

 if (UserRights < rights) then
  begin
   Self.ORAuthoriseResponse(Sender, UserRights, 'Pøipojení OØ '+Self.id+' neuatorizováno !');
   if (UserRights > TORControlRights.null) then
     Self.PnlDAdd(Sender, UserRights, username)
   else
     Self.PnlDRemove(Sender);

   ORTCPServer.GUIRefreshLine((Sender.Data as TTCPORsRef).index);
   Exit;
  end;

 // kontrola vyplych systemu
 if ((not GetFunctions.GetSystemStart) and (rights > read) and (rights < superuser)) then
  begin
   // superuser muze autorizovat zapis i pri vyplych systemech
   Self.PnlDAdd(Sender, TORControlRights.read, username);
   Self.ORAuthoriseResponse(Sender, TORControlRights.read, 'Nelze autorizovat zápis pøi vyplých systémech !');
   ORTCPServer.GUIRefreshLine((Sender.Data as TTCPORsRef).index);
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
       // technologie pripojovani zarucuje, ze pripojeny dispecer muze byt jen jeden
       if (Self.Connected[i].user = username) then
        begin
         panel := Self.Connected[i];
         panel.Rights := TORCOntrolRights.read;
         Self.Connected[i] := panel;
         Self.ORAuthoriseResponse(panel.Panel, panel.Rights, 'Pøevzetí øízení');
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
   ORTCPServer.GUIRefreshLine((Sender.Data as TTCPORsRef).index);
   Self.ORAuthoriseResponse(Sender, TORControlRights.null, 'Pøipojeno maximum OØ, nelze autorizovat další !');
   Exit;
  end;

 UsrDb.LoginUser(username);
 Self.ORAuthoriseResponse(Sender, rights, msg);
 ORTCPServer.GUIRefreshLine((Sender.Data as TTCPORsRef).index);

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
procedure TOR.PanelClick(Sender:TIdContext; blokid:Integer; Button:TPanelButton);
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

 case (Blk.GetGlobalSettings().typ) of
  _BLK_VYH     : begin
      for i := 0 to (Blk as TBlkVyhybka).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkVyhybka).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkVyhybka).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
    end;

  _BLK_USEK    : begin
      for i := 0 to (Blk as TBlkUsek).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkUsek).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkUsek).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
   end;

  _BLK_SCOM    : begin
      for i := 0 to (Blk as TBlkSCom).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkSCom).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkSCom).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
   end;

  _BLK_PREJEZD : begin
      for i := 0 to (Blk as TBlkPrejezd).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkPrejezd).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkPrejezd).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
  end;

  _BLK_UVAZKA : begin
      for i := 0 to (Blk as TBlkUvazka).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkUvazka).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkUvazka).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;

  end;

  _BLK_ZAMEK : begin
      for i := 0 to (Blk as TBlkZamek).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkZamek).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkZamek).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
  end;

  _BLK_ROZP : begin
      for i := 0 to (Blk as TBlkRozp).OblsRizeni.Cnt-1 do
        if ((Blk as TBlkRozp).OblsRizeni.ORs[i] = Self) then
         begin
          (Blk as TBlkRozp).PanelClick(Sender, Self, Button, rights);
          Exit;
         end;
  end;

 end;//case

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

 Self.ORStav.spr_new  := false;
 Self.ORStav.spr_edit := nil;
 Self.ORDKClickClient();

 if (Self.vb.Count > 0) then
  begin
   (Self.vb[Self.vb.Count-1] as TBlkUsek).KonecJC := TJCType.no;
   Self.vb.Delete(Self.vb.Count-1);
  end else begin
   Blk := Blky.GetBlkSComZacatekVolba(Self.id);
   if (Blk <> nil) then (Blk as TBlkSCom).ZacatekVolba := TBlkScomVolba.none;
  end;

 Blk := Blky.GetBlkUsekVlakPresun(Self.id);
 if (Blk <> nil) then (Blk as TBlkUsek).VlakPresun := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.PanelNUZ(Sender:TIdContext);
var i,j:Integer;
    Blk:TBlk;
    podminky:TList<RPConst.TPSPodminka>;
begin
 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 podminky := TList<RPConst.TPSPodminka>.Create();
 // zjisteni jmen bloku:
 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, Blk);
   if (Blk.GetGlobalSettings().typ <> _BLK_USEK) then continue;
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
 if ((not Self.ORStav.spr_new) and (Self.ORStav.spr_edit = nil)) then Exit(); 

 //kontrola opravneni klienta
 if (Integer(Self.PnlDGetRights(Sender)) < _R_write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 try
  if (Self.ORStav.spr_new) then
   Soupravy.AddSprFromPanel(spr, Self.ORStav.spr_usek, Self)
  else begin
   // kontrola jestli je souparva porad na useku
   if ((Self.ORStav.spr_usek as TBlkUsek).Souprava = Self.ORStav.spr_edit.index) then
     Self.ORStav.spr_edit.UpdateSprFromPanel(spr, Self.ORStav.spr_usek, Self)
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

 Self.ORStav.spr_new  := false;
 Self.ORStav.spr_edit := nil;
 Self.ORStav.spr_usek := nil;

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
       Self.MereniCasu[i].callback();
     Self.MereniCasu.Delete(i);
     break;
    end;
  end;//for i
end;//procedure

// vraci id pridaneho mereni
function TOR.AddMereniCasu(callback:TMericCallback; len:TDateTime):Byte;
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
procedure TOR.NUZTimeOut();
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
   if (Blk.GetGlobalSettings().typ <> _BLK_USEK) then continue;

   for j := 0 to (Blk as TBlkUsek).OblsRizeni.Cnt-1 do
    begin
     if ((Blk as TBlkUsek).OblsRizeni.ORs[j] = Self) then
      begin
       JC := JcDb.GetJCByIndex(JCDb.FindPostavenaJCWithUsek(Blk.GetGlobalSettings().id));

       if (JC <> nil) then
        begin
         Blky.GetBlkByID(JC.data.NavestidloBlok, Nav);
         if ((Nav as TBlkSCom).Navest > 0) then
           ORTCPServer.BottomError(JC.stav.SenderPnl, 'Chyba povolovací návìsti '+Blky.GetBlkName(JC.data.NavestidloBlok), Self.ShortName, 'TECHNOLOGIE');
         JC.RusJCWithoutBlk();
        end;
      end;
    end;//for j

  end;//for i

 Self.BroadcastData('NUZ;2;');

 Self.ORStav.NUZmerCasuID := Self.AddMereniCasu(Self.NUZTimeOut, EncodeTime(0, 0, 20, 0));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.ORAuthoriseResponse(Panel:TIdContext; Rights:TORControlRights; msg:string);
begin
 ORTCPServer.SendLn(Panel, Self.id+';AUTH;'+IntToStr(Integer(Rights))+';'+msg+';');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.RemoveClient(Panel:TIdContext);
begin
 Self.PnlDRemove(Panel);
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
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_PRETIZENI, 1000);
  end;

 if ((new <= 2) and (Self.ORStav.ZkratBlkCnt = 2)) then
  begin
   // zkrat skoncil -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
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
     ORTCPServer.PlaySound(Self.Connected[i].Panel, _SND_TRAT_ZADOST, 500);
  end;

 if ((new = 0) and (Self.ZadostBlkCnt > 0)) then
  begin
   // skocnila zadost -> vypnout zvuk
   for i := 0 to Self.Connected.Count-1 do
    if (Self.Connected[i].Rights > TORCOntrolRights.read) then
     ORTCPServer.DeleteSound(Self.Connected[i].Panel, _SND_TRAT_ZADOST);
  end;

 Self.ORStav.ZadostBlkCnt := new;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.DisconnectPanels();
var i:Integer;
    index:Integer;
begin
 for i := Self.Connected.Count-1 downto 0 do
  begin
   Self.ORAuthoriseResponse(Self.Connected[i].Panel, TORControlRights.null, 'Odpojení systémù');
   index := (Self.Connected[i].Panel.Data as TTCPORsRef).index;
   Self.PnlDRemove(Self.Connected[i].Panel);
   ORTCPServer.GUIRefreshLine(index);
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
 for i := 0 to MTB._MAX_MTB do
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
   str := 'Výpadek MTB modulù ';
   for i := 0 to MTB._MAX_MTB do
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
 LI.SubItems.Strings[3] := Self.stack.GetList();

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

////////////////////////////////////////////////////////////////////////////////

//  or;OSV;(code;stav)(code;srav) ...        - informace o stavu osvetleni (stav = [0,1])
procedure TOR.PanelSendOsv(Sender:TIdContext);
var i:Integer;
    str:string;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 str := Self.id + ';OSV;';
 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
   str := str + '[' + Self.ORProp.Osvetleni[i].name + '|' + IntToStr(MTB.GetOutputs(Self.ORProp.Osvetleni[i].board)[Self.ORProp.Osvetleni[i].port]) + ']';
 ORTCPServer.SendLn(Sender, str);
end;//procedure

procedure TOR.PanelSetOsv(Sender:TIdCOntext; id:string; state:boolean);
var i:Integer;
begin
 //kontrola opravneni klienta
 if (Self.PnlDGetRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit;
  end;

 for i := 0 to Self.ORProp.Osvetleni.Count-1 do
  if (Self.ORProp.Osvetleni.Items[i].name = id) then
   begin
    MTB.SetOutput(Self.ORProp.Osvetleni.Items[i].board, Self.ORProp.Osvetleni.Items[i].port, PrevodySoustav.BoolToInt(state));
    Exit();
   end;
end;//procedure

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
   ORTCPServer.SendLn(Sender, '-;SPR-LIST;'+Self.PanelGetSprs(Sender));
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
var ret:Integer;
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

 ret := HVDb.Remove(addr);
 if (ret > 0) then
   ORTCPServer.SendInfoMsg(Sender, 'Nelze smazat loko - chyba '+IntToStr(ret))
  else
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
  begin
   Self.ORStav.dk_click_callback(SenderPnl, Self, Button);
   Self.ORDKClickClient();
  end;
end;//procedure

// Tato procedura parsuje "LOK-REQ" z panelu.
procedure TOR.PanelLokoReq(Sender:TIdContext; str:TStrings);
var data:TStrings;
    i:Integer;
    HV:THV;
    rights:TORControlRights;
    line:string;
    Blk:TBlk;
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
  //  or;LOK-TOKEN;OK;[addr|token][addr|token]- odpovìï na žádost o token, je posílano také pøi RUÈ loko
  //  or;LOK-TOKEN;ERR;comment                - chybova odpoved na zadost o token
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
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;Loko '+data[i]+' neexistuje');
         Exit();
        end;

       // kontrola, zda se loko nachazi u me ve stanici
       // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
       if ((HV.Stav.stanice <> Self) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;Loko '+data[i]+' se nenachází ve stanici');
         Exit();
        end;

       // nelze vygenerovat token pro loko, ktere je uz v regulatoru
       if ((HV.Stav.regulators.Count > 0) and (rights < TORControlRights.superuser)) then
        begin
         ORTCPServer.SendLn(Sender, Self.id+';LOK-TOKEN;ERR;Loko '+data[i]+' již otevøeno v regulátoru');
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
   ORTCPServer.SendLn(Self.ORStav.reg_please, '-;LOK;G;PLEASE-RESP;ERR;Dispeèer odmítl žádost');
   Self.BroadcastData('LOK-REQ;CANCEL;');
   (Self.ORStav.reg_please.Data as TTCPORsRef).regulator_zadost := nil;
   Self.ORStav.reg_please := nil;
  end

//  or;LOK-REQ;U-PLEASE;blk_id              - zadost o vydani seznamu hnacich vozidel na danem useku
//  mozne odpovedi:
//    or;LOK-REQ;U-OK;[hv1][hv2]...           - seznamu hnacich vozidel v danem useku
//    or;LOK-REQ;U-ERR;info                   - chyba odpoved na pozadavek na seznam loko v danem useku

 else if (str[2] = 'U-PLEASE') then
  begin
   try
     Blky.GetBlkByID(StrToInt(str[3]), Blk);
     if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_USEK)) then
      begin
       ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Neplatný blok');
       Exit();
      end;

     if ((Blk as TBlkUsek).Souprava = -1) then
      begin
       ORTCPServer.SendLn(Sender, Self.id+';LOK-REQ;U-ERR;Žádná souprava na bloku');
       Exit();
      end;

     // generujeme zpravu s tokeny
     line := Self.id+';LOK-REQ;U-OK;{';
     for i := 0 to Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.cnt-1 do
      begin
       HV := HVDb.HVozidla[Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.HVs[i]];
       line := line + '[{' + HV.GetPanelLokString() + '}]';
      end;//for i
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

 if ((Self.NUZblkCnt > 0) and (not Self.NUZtimer)) then
   ORTCPServer.SendLn(panel, Self.id + ';NUZ;1;');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TOR.ClearVb();
var i:Integer;
begin
 for i := 0 to Self.vb.Count-1 do
  (Self.vb[i] as TBlkUsek).KonecJC := TJCType.no;
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
procedure TOR.UserUpdateRights(user:TUser);
var i:Integer;
    rights:TORControlRights;
begin
 for i := 0 to Self.Connected.Count-1 do
  begin
   // je pripojeny uzivatel s vyssimi opravevnimi, nez jsou mu pridelena?
   rights := user.GetRights(Self.id);
   if ((Self.Connected[i].user = user.id) and ((Self.Connected[i].Rights > rights) or (user.ban))) then
    begin
     if (user.ban) then rights := TORControlRights.null;
     Self.PnlDAdd(Self.Connected[i].Panel, rights, user.id);
     Self.ORAuthoriseResponse(Self.Connected[i].Panel, rights, 'Snížena oprávnìní uživatele');
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
     Self.ORAuthoriseResponse(Self.Connected[i].Panel, TORControlRights.null, 'Uživatel smazán');
     Self.PnlDRemove(Self.Connected[i].Panel);
    end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vraci 1 pokud zadost jiz probiha
// vraci 0 pokud prikaz probehl vporadku
function TOR.LokoPlease(Sender:TIDContext; user:TUser; comment:string):Integer;
var str:string;
begin
 if (Self.ORStav.reg_please <> nil) then Exit(1);
 Self.ORStav.reg_please := Sender;

 //format: or;LOK-REQ;REQ;username;firstname;lastname;comment
 str := 'LOK-REQ;REQ;'+user.id+';';
 if (user.firstname <> '') then str := str + user.firstname + ';' else str := str + '-;';
 if (user.lastname <> '') then str := str + user.lastname + ';' else str := str + '-;';
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
 if (Self.ZkratBlkCnt > 2) then ORTCPServer.PlaySound(panel, _SND_PRETIZENI, 1000);
 if (Self.ZadostBlkCnt > 0) then ORTCPServer.PlaySound(panel, _SND_TRAT_ZADOST, 500);
end;//procedure

procedure TOR.AuthWriteToRead(panel:TIdContext);
begin
 if (Self.ZkratBlkCnt > 2) then ORTCPServer.DeleteSound(panel, _SND_PRETIZENI);
 if (Self.ZadostBlkCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_TRAT_ZADOST);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
