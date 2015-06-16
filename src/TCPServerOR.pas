unit TCPServerOR;

interface

uses SysUtils, IdTCPServer, IdTCPConnection, IdGlobal,
     Classes, StrUtils, RPConst, Graphics, Windows, TOblRizeni,
     IdContext, TBlok, Prevody, ComCtrls, IdSync, TBloky, UPO,
     User;

const
  _PANEL_DEFAULT_PORT = 5896;
  _MAX_OR_CLIENTS = 32;

type
  TPSCallback = procedure (Sender:TIdContext; success:boolean) of object;

  TPanelConnectionStatus = (closed, opening, handshake, opened);

  // tady je ulozeno jedno fyzicke spojeni s panelem (obsahuje oblasti rizeni, otevrene okynko stitku, menu, ...)
  TTCPORsRef = class
    ORs:array [0.._MAX_ORREF-1] of TOR;
    ORsCnt:Integer;

    stitek:TBlk;
    vyluka:TBlk;
    potvr:TPSCallback;
    menu:TBlk;
    menu_or:TOR;
    UPO_OK, UPO_Esc:TNotifyEvent;
    UPO_ref:TObject;
    regulator:boolean;      // true pokud klient autorizoval rizeni pres regulator
    regulator_user:TUser;
    regulator_zadost:TOR;

    index:Integer;

    procedure Escape();

  end;

  TORTCPClient = class
    conn:TIdContext;
    status:TPanelConnectionStatus;
    // v conn.data je ulozen objekt typu TTCPORsRef, kde jsou ulozeny oblasti rizeni, ktere dany panel ma autorizovane
  end;

  TORTCPServer = class
   private const
    _PROTOCOL_VERSION = '1.0';

   private
    clients:array[0.._MAX_OR_CLIENTS] of TORTCPClient;      // databaze klientu
    tcpServer: TIdTCPServer;
    parsed: TStrings;
    data:string;
    fport:Word;
    DCCStopped:TIdContext;                   // tady je ulozeno ID spojeni, ktere zazadalo o CentralStop
                                             // vsechny panely maji standartne moznost vypnout DCC
                                             // pokud to udela nejaky panel, ma moznost DCC zapnout jen tento panel
                                             // pokud vypne DCC nekdo ze serveru, nebo z ovladace, zadny klient nema moznost ho zapnout

     procedure OnTcpServerConnect(AContext: TIdContext);
     procedure OnTcpServerDisconnect(AContext: TIdContext);
     procedure OnTcpServerExecute(AContext: TIdContext);

     procedure ParseGlobal(AContext: TIdContext);
     procedure ParseOR(AContext: TIdContext);
     procedure Auth(AContext: TIdContext);

     function IsOpenned():boolean;

     procedure OnDCCCmdErr(Sender:TObject; Data:Pointer);

   public

     constructor Create();
     destructor Destroy(); override;

     function Start(port:Word):Integer; overload;
     function Start():Integer; overload;
     function Stop():Integer;
     procedure DisconnectClient(conn:TIdContext);

     // volani funkci do panelu, ktere neprislusi OR, ale jednotlivym panelum
     procedure SendInfoMsg(AContext:TIdContext; msg:string);
     procedure Stitek(AContext: TIdContext; Blk:TBlk; stit:string);
     procedure Vyluka(AContext: TIdContext; Blk:TBlk; vyl:string);
     procedure Menu(AContext: TIdContext; Blk:TBlk; OblR:TOR; menu:string);
     procedure Potvr(AContext: TIdContext; callback:TPSCallback; stanice:TOR; udalost:string; senders:TBlksList; podminky:TPSPodminky; free_senders:boolean = true; free_podm:boolean = true);
     procedure PotvrClose(AContext: TIdContext; msg:string = '');
     procedure UPO(AContext: TIdContext; items:TUPOItems; critical:boolean; callbackOK:TNotifyEvent; callbackEsc:TNotifyEvent; ref:TObject);
     procedure CancelUPO(AContext: TIdContext; ref:TObject);

     // tyto funkce take muzou byt volany z oblasti rizeni, protoze nemusi byt primou reakci na akci uzivatele - chceme je odeslat vsem
     procedure PlaySound(AContext: TIdContext; code:Integer; delay_ms:Integer = -1);
     procedure DeleteSound(AContext: TIdContext; code:Integer);
     procedure BottomError(AContext: TIdContext; err:string; stanice:string; tech:string);

     procedure BroadcastBottomError(err:string; tech:string);
     procedure BroadcastData(data:string);

     procedure SendLn(AContext:TIDContext; str:string);

     procedure GUIInitTable();
     procedure GUIRefreshLine(index:Integer);
     procedure GUIRefreshTable();

     procedure DCCStart();
     procedure DCCStop();

     function GetClient(index:Integer):TORTCPClient;

      property openned:boolean read IsOpenned;
      property port:Word read fport write fport;
  end;//TPanelTCPClient

var
  ORTCPServer : TORTCPServer;

implementation

// jak funguje komunikace ze strany serveru:
//  1) klient se pripoji, vyvola se event OnConnect
//  2) klient se zaradi do databaze clients[], kde se mu vybere prvni volne misto a vytvori objekt TORTCPClient
//    do conn se prida connection a AContext a do AContext.data se vytvori prazdny TTCPORsRef (neobsahuje zadne oblasti rizeni)
//  3) event AExecute prijima data
//  4) probehne handshake, AExecute vzdy najde spojeni a ulozi do nej status = opened
//  5) klient posle zadost o autorizaci oblasti rizeni, program najde prislusne OR a pozada je o autorizaci
//     server OR zaroven preda TIdTCPConnection, s kterym si OR uz budou nakladat samy (posilat tam data)
//  6) veskera data urcena pro konkretni oblasti rizeni budou sice prijimana a AExecute, ale na zakldade ACotetext.data jim bude vyhledana oblast rizeni a tato data poslana na zpracovani tam
//  7) globalni data - nejsou urceny pro konkretni oblast rizeni - ale budou zpracovavana zde - server si tedy musi napriklat pamatovat, jakemu bloku klient prave edituje STITEK
//     tohleto si pamatuje SERVER, nikoliv oblast rizeni

//  10) pri uzavreni spojeni od klienta je do or v AContext.data zavolan Disconnect; OR si klienta smazou z databaze
//  11) co se tyce odpojeni klientu, tak server podporuje jen DisconnectAll(), kde odpoji vsechny na zaklade databaze, coz vyvola krok 10

// specifikace komunikacnho protkolu:
//  jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem
//  prvni parametr je vzdy id oblasti rizeni, popr. '-' pokud se jedna o rezijni prikaz
// prikazy:

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// KLIENT -> SERVER ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  -;HELLO;verze;                          - handshake a specifikace komunikacniho protokolu
//  -;ESCAPE;                               - stisknuti tlacitka ESC
//  -;STIT;stitek                           - nastaveni stitku
//  -;VYL;vyluka                            - nastaveni vyluky
//  -;PS;stav                               - odhlaska na potvrzovaci sekvenci
//  -;MENUCLICK;text                        - uzivatel kliknul na polozku v menu s textem text
//  -;UPO;OK                                - vsechna upozorneni schvalena
//  -;UPO;ESC                               - upozorneni neschvalena
//  -;MOD-CAS;START;                        - zapnuti modeloveho casu
//  -;MOD-CAS;STOP;                         - vypnuti modleoveho casu
//  -;MOD-CAS;TIME;time;nasobic             - nastaveni modeloveho casu
//  -;DCC;GO                                - central start
//  -;DCC;STOP                              - central stop
//  -;SPR-LIST;                             - pozadavek na zsikani seznamu souprav v mych oblastech rizeni
//  -;SPR-REMOVE;spr_name                   - pozadavek na smazani soupravy s cislem spr_name
//  or;NUZ;stav                             - 1 = zapnout NUZ, 0 = vypnout NUZ
//  or;GET-ALL;                             - pozadavek na zjisteni stavu vsech bloku v prislusne OR
//  or;CLICK;block_id;button                - klik na blok na panelu
//                                            stav = ['ok', 'cancel']
//  or;AUTH;opravneni;username;password     - pozadavek o autorizaci
//  or;MSG:recepient;msg                    - zprava pro recepient od or
//  -;OR-LIST;                              - pozadavek na ziskani seznamu OR
//  or;HV-LIST;                             - pozadavek na ziskani seznamu hnacich vozidel v dane stanici
//  or;SPR-CHANGE;vlastosti soupravy dle definice zpravy v TSouprava (format: nazev;pocet_vozu;poznamka;smer_Lsmer_S;hnaci vozidla)
//  or;LOK-MOVE-OR;lok_addr;or_id           - presun soupravy do jine oblasti rizeni
//
//  -;LOK;G;AUTH;username;passwd            - pozadavek na autorizaci uzivatele
//  -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
//  -;LOK;G:CANCEL;                         - zruseni pozadavku o loko

//  -:LOK;addr;PLEASE;token                 - zadost o rizeni konkretni lokomotivy; token neni potreba pripojovat v pripade, kdy loko uz mame autorizovane a bylo nam ukradeno napriklad mysi
//  -;LOK;addr;RELEASE                      - uvolneni lokomotivy z rizeni regulatoru
//  -;LOK;addr;SP;sp_km/h                   - nastaveni rychlosti lokomotivy
//  -;LOK;addr;SPD;sp_km/h;dir ()           - nastaveni rychlosti a smeru lokomotivy
//  -;LOK;addr;D;dir ()                     - nastaveni smeru lokomotivy
//  -;LOK;addr;F;F_left-F_right;states      - nastaveni funkci lokomotivy
//    napr.; or;LOK;F;0-4;00010 nastavi F3 a ostatni F vypne
//  -;LOK;addr;STOP;                        - nouzove zastaveni
//  -;LOK;addr;TOTAL;[0,1]                  - nastaveni totalniho rizeni hnaciho vozidla

//  or;OSV;SET;code;stav [0,1]              - nastaveni stavu osvetleni
//  or;OSV;GET;                             - ziskani stavu vsech osvetleni

//  or;HV;ADD;data                          - pridani hnaciho vozidla
//  or;HV;REMOVE;addr                       - smazani hnaciho vozdila
//  or;HV;EDIT;data                         - editace hnaciho vozidla

//  or;ZAS;VZ                               - volba do zasobniku
//  or;ZAS;PV                               - prima volba
//  or;ZAS;EZ;[0,1]                         - zapnuti/vypnuti editace zasobniku
//  or;ZAS;RM;id                            - smazani cesty [id] ze zasobniku
//  or;ZAS;UPO                              - uzivatel klikl na UPO

//  or;DK-CLICK;[L,M,R]                     - klik na dopravni kancelar prislusnym tlacitkem mysi

//  or;LOK-REQ;PLEASE;addr1|addr2|...       - zadost o vydani tokenu
//  or;LOK-REQ;U-PLEASE;blk_id              - zadost o vydani seznamu hnacich vozidel na danem useku
//  or;LOK-REQ;LOK;addr1|addr2|...          - lokomotivy pro rucni rizeni na zaklde PLEASE regulatoru vybrany
//  or;LOK-REQ;DENY;                        - odmitnuti pozadavku na rucni rizeni


////////////////////////////////////////////////////////////////////////////////
/////////////////////////// SERVER -> KLIENT ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  -;HELLO;verze;                          - handshake a specifikace komunikacniho protokolu
//  -;STIT;blk_name;stitek;                 - pozadavek na zobrazeni vstupu pro stitek
//  -;VYL;blk_name;vyluka;                  - pozadavek na zobrazeni vstupu pro vyluku
//  -;PS;stanice;udalost;sender1|sender2|...;(blok1_name|blok1_podminka)(blok2_name|blok2_podminka)(...)...
//                                          - pozadavek na zobrazeni potvrzovaci sekvence
//  -;PS-CLOSE;[duvod]                      - zruseni potvrzovaci sekvence; duvod je nepovinny
//  -;MENU;prikaz1,prikaz2,...              - pozadavek na zobrazeni MENU
//  -;INFOMSG;msg                           - zobrazeni informacni zpravy
//  -;BOTTOMERR;err;stanice;technologie     - zobrazeni spodni chyby
//  -;SND;PLAY;code;[delay (ms)]            - prehravani zvuku, delay je nepovinny, pokud neni uveden, je zvuk prehran jen jednou
//  -;SND;STOP;code                         - zastaveni prehravani zvuku
//  -;OR-LIST;(or1id,or1name)(or2id, ...    - zaslani seznamu vsech oblasti rizeni
//  -;UPO-CLOSE;                            - zavrit upozorneni
//  -;UPO;[item1][item2]                    - upozorneni
//  -;UPO-CRIT;[item1][item2]               - kriticke upozorneni - nelze porkacovat dale
//      format [item_x]:
//          (radek1)(radek2)(radek3)          LRM je zarovnani, fg je barva popredi radku, bg je barva pozadi radku a text je text na radku
//        radek_x: [L,R,M]|fg|bg|text         barvy na dalsich radcich nemusi byt vyplnene, pak prebiraji tu barvu, jako radek predchozi
//  -;INFO-TIMER;id;time_min;time_sec;message  - zobrazit odpocet
//  -;INFO-TIMER-RM;id;                     - smazat odpocet
//  -;MOD-CAS;running;nasobic;cas;                  - oznameni o stavu modeloveho casu - aktualni modelovy cas a jestli bezi
//  -;DCC;GO                                - DCC zapnuto
//  -;DCC;STOP                              - DCC vypnuto
//  -;DCC;DISABLED                          - neni mozno zmenit stav DCC z tohoto klienta
//  -;SPR-LIST;(spr1)(spr2)(...)            - odeslani seznamu souprav ve vsech oblastech rizeni
//  or;AUTH;rights;comment;                 - odpoved na pozadavek o autorizaci
//  or;MENU;items
//                                             pokud je prikaz '-', vypisuje se oddelovac
//                                             pokud je prvni znak #, pole je disabled
//                                             pokud je prvni znak $, pole je vycentrovane a povazovane na nadpis
//  or;CAS;START;id;delka_sekundy;          - pridani mereni casu, id je vzdy unikatni unsigned int, ktery obvykle zacina od 0
//  or;CAS;STOP;id;                         - smazani mereni casu
//  or;CHANGE;typ_blk;tech_blk_id;barva_popredi;barva_pozadi;blikani; dalsi argumenty u konkretnich typu bloku:
//    typ_blk = cislo podle typu bloku na serveru
//      usek : konec_jc;ramecek_color;[souprava;barva_soupravy;sipkaLsipkaS] -  posledni 3 argumenty jsou nepovinne, pokud je ramecek_color string '-', ramecek se nezobrazuje
//      vyhybka : poloha (cislo odpovidajici poloze na serveru - [disabled = -5, none = -1, plus = 0, minus = 1, both = 2])
//      navestidlo: ab (false = 0, true = 1)
//      prejezd: stav (otevreno = 0, vystraha = 1, uzavreno = 2, anulace = 3)
//      uvazka: smer (-5 = disabled, 0 = bez smeru, 1 = zakladni, 2 = opacny); soupravy - cisla souprav oddelene carkou (pokid cislo zacina znakem $, ma byt barevne odliseno barvou - predpovidana souprava)
//         prvni souprava je vzdy ta, ktere do trati prisla prvni
//      zamek: zadne dalsi argumenty
//  or;NUZ;stav                             - stav in [0, 1, 2] - jestli probiha NUZ na DK - fakticky rika, jeslti ma DK blikat
//                                            0 = zadne bloky v NUZ, neblikat
//                                            1 = bloky v NUZ, blikat, nabidnout NUZ>
//                                            2 = probiha NUZ, neblika, nabidnout NUZ<
//  or;MSG:sender;msg                       - zprava pro or od sender
//  or;MSG-ERR;sender;err                   - chyba pri odesilani zpravy
//  or;HV-LIST;[HV1][HV2]                   - odeslani seznamu hnacich vozidel dane stanice
//  or;SPR-NEW;
//  or;SPR-EDIT;vlastosti soupravy dle definice zpravy v TSouprava (format: nazev;pocet_vozu;poznamka;smer_Lsmer_S;hnaci vozidla)
//  or;SPR-EDIT-ERR;err                     - chyba pri ukladani supravy po editaci
//  or;SPR-EDIT-ACK;                        - editace soupravy probehla uspesne

//  -;OR-LIST;(or1id,or1name)(or2id, ...    - zaslani seznamu vsech oblasti rizeni

//  -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
//  -;LOK;G;AUTH;[ok,not,total];info        - navratove hlaseni o autorizaci
//  -;LOK;addr;AUTH;[ok,not,stolen,release];info;hv_data  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
//                                        info je string
//                                        hv_data jsou pripojovana k prikazu v pripade, ze doslo uspesne k autorizaci; jedna se o PanelString hnaciho vozdila obsahujici vsechny informace
//  -;LOK;addr;F;F_left-F_right;states      - informace o stavu funkci lokomotivy
//    napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
//  -;LOK;addr;SPD;sp_km/h;sp_stupne;dir    - informace o zmene rychlosti (ci smeru) lokomotivy
//  -;LOK;addr;RESP;[ok, err];info;speed_kmph
//                                          - odpoved na prikaz;  speed_kmph je nepovinny argument; info zpravidla obsahuje rozepsani chyby, pokud je odpoved "ok", info je prazdne
//  -;LOK;addr;TOTAL;[0,1]                  - zmena rucniho rizeni lokomotivy

//  or;OSV;(code;stav)(code;srav) ...       - informace o stavu osvetleni (stav = [0,1])

//  or;ZAS;VZ                               - volba do zasobniku
//  or;ZAS;PV                               - prima volba
//  or;ZAS;LIST;first-enabled[0,1];(id1|name1)(id2|name2) ... - seznam jizdnich cest v zasobniku
//  or;ZAS;FIRST;first-enabled[0,1]         - jestli je mozno prvni prvek mazat
//  or;ZAS;INDEX;index                      - oznameni indexu zasobniku
//  or;ZAS;RM;id                            - smazani cesty [id] ze zasobniku
//  or;ZAS;ADD;id|name                      - pridani cesty [id, name] do zasobniku
//  or;ZAS;RM;id                            - oznameni o smazani cesty ze zasobniku
//  or;ZAS;HINT;hint                        - zmena informacni zpravy vedle zasobniku
//  or;ZAS;UPO;[0,1]                        - 1 pokud je UPO klikaci, 0 pokud ne

//  or;DK-CLICK;[0, 1]                      - informuje server o kliku na DK misto toho, aby zobrazil menu

//  or;RUC;addr;text                        - informace o rucnim rizeni hnaciho vozdila addr; text se zobrazi jako string v panelu
//  or;RUC-RM;addr                          - smazani informace o rucnim rizeni hnaciho vozidla

//  or;LOK-TOKEN;OK;[addr|token][addr|token]- odpovìï na žádost o token, je posílano také pøi RUÈ loko
//  or;LOK-TOKEN;ERR;comment                - chybova odpoved na zadost o token
//  or;LOK-REQ;REQ;username;firstname;lastname;comment
//                                          - pozadavek na prevzeti loko na rucni rizeni
//  or;LOK-REQ;U-OK;[hv1][hv2]...           - seznamu hnacich vozidel v danem useku
//  or;LOK-REQ;U-ERR;info                   - chyba odpoved na pozadavek na seznam loko v danem useku
//  or;LOK-REQ;OK                           - seznam loko na rucni rizeni schvalen serverem
//  or;LOK-REQ;ERR;comment                  - seznam loko na rucni rizeni odmitnut serverem
//  or;LOK-REQ;CANCEL;                      - zruseni pozadavku na prevzeti loko na rucni rizeni

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// navazani komunikace:
//  1) klient se pripoji
//  2) klient posle hanshake
//  3) klient vycka na odpoved na handshake
//  4) klient vysila pozadavek na autorizaci oblasti rizeni
//  5) po klientovi muze byt vyzadovano heslo
//  5) klient bud dostane, nebo nedostae pristup k prislusnym OR

//  username a heslo se zadava vzdy jen jedno
//   autorizace pro OR ale prichazi samostatne - pokud jsou na panelu 2 OR, tak uzivatel klidne muze dostat autorizaci jen pro jedno z nich

// rights:
//  (null = 0, read = 1, write = 2, superuser = 3); prenasi se pouze cislo

// barva se prenasi jako 3 text, ktery obsahuje 3 sestnactkova cisla = napr. FFAAFF
//  poradi barev: RED, GREEN, BLUE

uses Main, TBlokUsek, TBlokVyhybka, TBlokSCom, TOblsRizeni, TBlokUvazka,
      TBlokPrejezd, Logging, ModelovyCas, SprDb, Souprava,
      TBlokZamek, Trakce, RegulatorTCP;

////////////////////////////////////////////////////////////////////////////////

constructor TORTCPServer.Create();
var i:Integer;
begin
 inherited Create();

 Self.fport := _PANEL_DEFAULT_PORT;

 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.clients[i] := nil;

 Self.parsed := TStringList.Create;

 Self.tcpServer := TIdTCPServer.Create(nil);
 Self.tcpServer.OnConnect    := Self.OnTcpServerConnect;
 Self.tcpServer.OnDisconnect := Self.OnTcpServerDisconnect;
 Self.tcpServer.OnExecute    := Self.OnTcpServerExecute;

 Self.DCCStopped := nil;
end;//ctor

destructor TORTCPServer.Destroy();
begin
 if (Self.tcpServer.Active) then
  Self.tcpServer.Active := false;

 if (Assigned(Self.tcpServer)) then
   FreeAndNil(Self.tcpServer);

 if (Assigned(Self.parsed)) then
   FreeAndNil(Self.parsed);

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.Start(port:Word):Integer;
begin
 if ((SystemData.Status = starting) and (Self.openned)) then
  begin
   F_Main.LogStatus('System: start OK');
   SystemData.Status := null;
   F_Main.A_System_Stop.Enabled := true;
   Exit(0);
  end;

 if (Self.tcpServer.Active) then Exit(1);

 F_Main.S_Server.Brush.Color := clGray;
 F_Main.LogStatus('Panel server: spouštìní...');

 Self.tcpServer.DefaultPort := port;
 Self.fport := port;

 try
  Self.tcpServer.Active := true;
 except
  F_Main.S_Server.Brush.Color := clRed;
  F_Main.LogStatus('Panel server: chyba pøi inicializaci komponenty');
  Exit(2);
 end;

 F_Main.S_Server.Brush.Color := clLime;
 F_Main.LogStatus('Panel server: spuštìn');

 if (SystemData.Status = starting) then
  begin
   F_Main.LogStatus('System: start OK');
   SystemData.Status := null;
   F_Main.A_System_Stop.Enabled := true;
  end;

 Result := 0;
end;//function

function TORTCPServer.Start():Integer;
begin
 Result := Self.Start(Self.port);
end;//function

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.Stop():Integer;
begin
 if ((SystemData.Status = stopping) and (not Self.openned)) then
  begin
   TrkSystem.TurnOffFunctions(F_Main.A_All_Loko_OdhlasitExecute);
   Exit(0);
  end;

 if (not Self.tcpServer.Active) then Exit(1);

 F_Main.LogStatus('Panel server: zastavování...');
 F_Main.S_Server.Brush.Color := clGray;

 Self.tcpServer.Active := false;

 ORTCPServer.GUIRefreshTable();

 F_Main.LogStatus('Panel server: zastaven');
 F_Main.S_Server.Brush.Color := clRed;

 if (SystemData.Status = stopping) then
   TrkSystem.TurnOffFunctions(F_Main.A_All_Loko_OdhlasitExecute);

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TORTCPServer.OnTcpServerConnect(AContext: TIdContext);
var i:Integer;
    ORsref:TTCPORsRef;
begin
 AContext.Connection.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;

 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Self.clients[i] = nil) then
   break;

 // na serveru neni misto -> odpojit klienta
 if (i = _MAX_OR_CLIENTS) then
  begin
   // tady bych mohl napsat chybovou hlasku
   Self.SendInfoMsg(AContext, 'Pøipojeno maximum klientù');
   AContext.Connection.Disconnect();
   Exit();
  end;

 ORsref             := TTCPORsRef.Create;
 ORsref.ORsCnt      := 0;
 ORsref.index  := i;
 ORsref.regulator_zadost := nil;
 ORsref.Escape();

 Self.clients[i]           := TORTCPClient.Create();
 Self.clients[i].conn      := AContext;
 Self.clients[i].status    := TPanelConnectionStatus.handshake;
 Self.clients[i].conn.Data := ORsref;

 Self.GUIRefreshLine(i);
end;//procedure

// Udalost vyvolana pri odpojeni klienta
procedure TORTCPServer.OnTcpServerDisconnect(AContext: TIdContext);
var i:Integer;
begin
 // vymazeme klienta ze vsech oblasti rizeni
 for i := (AContext.Data as TTCPORsRef).ORsCnt-1 downto 0 do
  (AContext.Data as TTCPORsRef).ORs[i].RemoveClient(AContext);

 // vymazeme klienta z databaze klientu
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and (AContext = Self.clients[i].conn)) then
   begin
    // zrusime pripadnou zadost o lokomotivu
    if ((Self.clients[i].conn.Data as TTCPORsRef).regulator_zadost <> nil) then
      (Self.clients[i].conn.Data as TTCPORsRef).regulator_zadost.LokoCancel(Self.clients[i].conn);

    // odpojeni vsech pripadne neodpojenych regulatoru
    if ((Self.clients[i].conn.Data as TTCPORsRef).regulator) then
      TCPRegulator.RegDisconnect(Self.clients[i].conn);

    FreeAndNil(Self.clients[i]);
    break;
   end;

  // aktualizujeme radek v tabulce klientu ve F_Main
  if (Self.tcpServer.Active) then
    ORTCPServer.GUIRefreshLine(i);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnTcpServerExecute(AContext: TIdContext);
begin
 if (not AContext.Connection.Connected) then Exit;

 if (AContext.Connection.IOHandler.InputBufferIsEmpty) then
  begin
   IndySleep(1);
   Exit();
  end;

 //read data
 // data jsou schvalne globalni, aby se porad nevytvarela a nenicila dokola
 data := AContext.Connection.IOHandler.ReadLn();

 Self.parsed.Clear();
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 try
   // zakladni rozdeleni parsovani - na data, ktera jsou obecna a na data pro konkretni oblast rizeni
   if (Self.parsed[0] = '-') then
    Self.ParseGlobal(AContext)
   else
    Self.ParseOR(AContext);
 except

 end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseGlobal(AContext: TIdContext);
var i:Integer;
    tmp:string;
    blk:TBlk;
begin
 // najdeme klienta v databazi
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if ((Assigned(Self.clients[i])) and (Self.clients[i].conn = AContext)) then
   break;

 //pokud pripojeni neni v databazi, je neco sakra spatne
 if (i = _MAX_OR_CLIENTS) then Exit();

 // parse handhake
 if (Self.parsed[1] = 'HELLO') then
  begin
   // tady muze prijit kontrola verze protokolu - je to ulozeno v Self.parsed[2]
   Self.clients[i].status := TPanelConnectionStatus.opened;
   Self.SendLn(AContext, '-;HELLO;1.0');

   // oznamime verzi komunikacniho protokolu
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[9] := Self.parsed[2];

   ORTCPServer.GUIRefreshLine((AContext.Data as TTCPORsRef).index);
   ModCas.SendTimeToPanel(AContext);

   if (TrkSystem.status = Ttrk_status.TS_ON) then
     Self.SendLn(AContext, '-;DCC;GO')
   else
     Self.SendLn(AContext, '-;DCC;DISABLED');

   Exit();
  end;

//  -;STIT;stitek                 - nastaveni stitku
//  -;VYL;vyluka                  - nastaveni vyluky

 // vsechny nasledujici prikazy jsou podminene tim, ze probehl handshake
 if (Self.clients[i].status < TPanelConnectionStatus.opened) then Exit();

 if (parsed[1] = 'STIT') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := '';

   if ((AContext.Data as TTCPORsRef).stitek = nil) then Exit();
   case ((AContext.Data as TTCPORsRef).stitek.GetGlobalSettings().typ) of
    _BLK_USEK   : ((AContext.Data as TTCPORsRef).stitek as TBlkUsek).Stitek := tmp;
    _BLK_VYH    : ((AContext.Data as TTCPORsRef).stitek as TBlkVyhybka).Stitek := tmp;
    _BLK_UVAZKA :((AContext.Data as TTCPORsRef).stitek as TBlkUvazka).Stitek := tmp;
    _BLK_PREJEZD:((AContext.Data as TTCPORsRef).stitek as TBlkPrejezd).Stitek := tmp;
    _BLK_ZAMEK  :((AContext.Data as TTCPORsRef).stitek as TBlkZamek).Stitek := tmp;
   end;//case
   (AContext.Data as TTCPORsRef).stitek := nil;
  end

 else if (parsed[1] = 'VYL') then
  begin
   if (parsed.Count < 3) then
    tmp := ''
   else
    tmp := parsed[2];

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := '';

   if ((AContext.Data as TTCPORsRef).vyluka = nil) then Exit();
   case ((AContext.Data as TTCPORsRef).vyluka.GetGlobalSettings().typ) of
    _BLK_USEK : ((AContext.Data as TTCPORsRef).vyluka as TBlkUsek).SetUsekVyl(AContext, tmp);
    _BLK_VYH  : ((AContext.Data as TTCPORsRef).vyluka as TBlkVyhybka).SetVyhVyl(AContext, tmp);
   end;//case
   (AContext.Data as TTCPORsRef).vyluka := nil;
  end

//  -;PS;stav                     - odhlaska na potvrzovaci sekvenci
 else if (parsed[1] = 'PS') then
  begin
   if (not Assigned((AContext.Data as TTCPORsRef).potvr)) then Exit();

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := '';

   if (parsed[2] = '2') then
     (AContext.Data as TTCPORsRef).potvr(AContext, true)
   else
     (AContext.Data as TTCPORsRef).potvr(AContext, false);

   (AContext.Data as TTCPORsRef).potvr := nil;
  end

 else if (parsed[1] = 'MENUCLICK') then
  begin
   if ((AContext.Data as TTCPORsRef).menu = nil) then Exit();
   blk := (AContext.Data as TTCPORsRef).menu;
   (AContext.Data as TTCPORsRef).menu := nil;       // musi byt v tomto poradi - pri volani menu do bloku uz musi byt menu = nil

   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[6] := '';

   case (blk.GetGlobalSettings().typ) of
    _BLK_USEK    : (blk as TBlkUsek).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
    _BLK_VYH     : (blk as TBlkVyhybka).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
    _BLK_SCOM    : (blk as TBlkSCom).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
    _BLK_UVAZKA  : (blk as TBlkUvazka).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
    _BLK_PREJEZD : (blk as TBlkPrejezd).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
    _BLK_ZAMEK   : (blk as TBlkZamek).PanelMenuClick(AContext, (AContext.Data as TTCPORsRef).menu_or, parsed[2]);
   end;//case
  end

 else if (parsed[1] = 'ESCAPE') then
  begin
   (AContext.Data as TTCPORsRef).Escape();

   for i := 0 to (AContext.Data as TTCPORsRef).ORsCnt-1 do
    (AContext.Data as TTCPORsRef).ORs[i].PanelEscape(AContext);
  end

 else if (parsed[1] = 'OR-LIST') then
   ORs.SendORList(AContext)

 else if (parsed[1] = 'UPO') then
  begin
   if (parsed[2] = 'OK') then
    begin
      if (Assigned((AContext.Data as TTCPORsRef).UPO_OK)) then
        (AContext.Data as TTCPORsRef).UPO_OK(AContext);
    end else
      if (parsed[2] = 'ESC') then
        if (Assigned((AContext.Data as TTCPORsRef).UPO_Esc)) then
          (AContext.Data as TTCPORsRef).UPO_Esc(AContext);

   (AContext.Data as TTCPORsRef).UPO_OK  := nil;
   (AContext.Data as TTCPORsRef).UPO_Esc := nil;
   (AContext.Data as TTCPORsRef).UPO_ref := nil;
  end//if parsed[2] = 'UPO'

//  -;MOD-CAS;START;              - zapnuti modeloveho casu
//  -;MOD-CAS;STOP;               - vypnuti modleoveho casu
//  -;MOD-CAS;TIME;time;nasobic   - nastaveni modeloveho casu
 else if (parsed[1] = 'MOD-CAS') then
  begin
   if (parsed[2] = 'START') then ModCas.started := true;
   if (parsed[2] = 'STOP')  then ModCas.started := false;
   if (parsed[2] = 'TIME')  then ModCas.SetTime(StrToTime(parsed[3]), StrToInt(parsed[4]));
  end

 else if (parsed[1] = 'DCC') then
  begin
   if ((parsed[2] = 'GO') and (TrkSystem.status <> TS_ON)) then begin
    TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCCmdErr, AContext);
    TrkSystem.CentralStart()
   end else if ((parsed[2] = 'STOP') and (TrkSystem.status = TS_ON)) then
    begin
     TrkSystem.callback_err := TTrakce.GenerateCallback(Self.OnDCCCmdErr, AContext);
     TrkSystem.CentralStop();
     Self.DCCStopped := AContext;
    end;
  end

 else if (parsed[1] = 'SPR-LIST') then
  begin
   tmp := '';
   for i := 0 to (AContext.Data as TTCPORsRef).ORsCnt-1 do
    tmp := tmp + (AContext.Data as TTCPORsRef).ORs[i].PanelGetSprs(AContext);
   Self.SendLn(AContext, '-;SPR-LIST;'+tmp);
  end

 else if (parsed[1] = 'SPR-REMOVE') then
  begin
   i := Soupravy.GetSprIndexByName(parsed[2]);
   if (i < 0) then Exit();
   (Soupravy.soupravy[i].stanice as TOR).PanelRemoveSpr(AContext, i);
  end

 else if (parsed[1] = 'LOK') then
   TCPRegulator.Parse(AContext, parsed);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.ParseOR(AContext: TIdContext);
var i:Integer;
begin
 // nejdriv se podivame, jestli nahodou nechce nekdo autorizaci

 if (parsed[1] = 'AUTH') then
  begin
   Self.Auth(AContext);
   Exit;
  end;

 // vsechna ostatni data pak podlehaji znalosti OR, ktere mam autorizovane, tak z toho vyjdeme
 for i := 0 to (AContext.Data as TTCPORsRef).ORsCnt-1 do
  if (parsed[0] = (AContext.Data as TTCPORsRef).ORs[i].id) then
   break;

 if (i = (AContext.Data as TTCPORsRef).ORsCnt) then
  begin
   Self.SendInfoMsg(AContext, 'Neautorizováno');
   Exit();
  end;

//  or;NUZ;stav                   - 1 = zapnout NUZ, 2 = vypnout NUZ
//  or;GET-ALL;                   - pozadavek na zjisteni stavu vsech bloku v prislusne OR
//  or;CLICK;block_id;button      - klik na blok na panelu
//                                      stav = ['ok', 'cancel']
//  or;AUTH;opravneni             - pozadavek o autorizaci
//  or;MSG:recepient;msg          - zprava pro recepient od or
//  or;LOK-MOVE-OR;lok_addr;or_id - presun soupravy do jine oblasti rizeni

 if (parsed[1] = 'NUZ') then
  begin
   if (parsed[2] = '1') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelNUZ(AContext);
   if (parsed[2] = '0') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelNUZCancel(AContext);
   Exit();
  end

 else if (parsed[1] = 'GET-ALL') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelFirstGet(AContext)

 else if (parsed[1] = 'CLICK') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelClick(AContext, StrToInt(parsed[2]), TPanelButton(StrToInt(parsed[3])))

 else if (parsed[1] = 'MSG') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelMessage(ACOntext, parsed[2], parsed[3])

 else if (parsed[1] = 'HV-LIST') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelHVList(AContext)

 else if (parsed[1] = 'SPR-CHANGE') then
  begin
   parsed.Delete(0);
   parsed.Delete(0);
   (AContext.Data as TTCPORsRef).ORs[i].PanelSprChange(AContext, parsed);
  end

 else if (parsed[1] = 'LOK-MOVE-OR') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelMoveLok(AContext, StrToInt(parsed[2]), parsed[3])

 else if (parsed[1] = 'OSV') then
  begin
   if (parsed[2] = 'GET') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelSendOsv(AContext)
   else if (parsed[2] = 'SET') then
   (AContext.Data as TTCPORsRef).ORs[i].PanelSetOsv(AContext, parsed[3], PrevodySoustav.StrToBool(parsed[4]));
  end

 else if (parsed[1] = 'HV') then
  begin
   if (parsed[2] = 'ADD') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVAdd(AContext, parsed[3]);
   if (parsed[2] = 'REMOVE') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVRemove(AContext, StrToInt(parsed[3]));
   if (parsed[2] = 'EDIT') then
     (AContext.Data as TTCPORsRef).ORs[i].PanelHVEdit(AContext, parsed[3]);
  end

 else if (parsed[1] = 'ZAS') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelZAS(AContext, parsed)

 else if (parsed[1] = 'DK-CLICK') then
  begin
   case (parsed[2][1]) of
     'L' : (AContext.Data as TTCPORsRef).ORs[i].PanelDKClick(AContext, TPanelButton.left);
     'M' : (AContext.Data as TTCPORsRef).ORs[i].PanelDKClick(AContext, TPanelButton.middle);
     'R' : (AContext.Data as TTCPORsRef).ORs[i].PanelDKClick(AContext, TPanelButton.right);
   end;
  end

 else if (parsed[1] = 'LOK-REQ') then
  (AContext.Data as TTCPORsRef).ORs[i].PanelLokoReq(AContext, parsed);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendInfoMsg(AContext:TIdContext; msg:string);
begin
 Self.SendLn(AContext, '-;INFOMSG;'+msg+';');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.IsOpenned():boolean;
begin
 Result := Self.tcpServer.Active;
end;//function


////////////////////////////////////////////////////////////////////////////////
// volani funkci ke klientovi

//  -;STIT;blk_name;stitek;                 - pozadavek na zobrazeni vstupu pro stitek
//  -;VYL;blk_name;vyluka;                  - pozadavek na zobrazeni vstupu pro vyluku
//  -;PS;stanice;udalost;sender1|sender2|...;(blok1_name;blok1_podminka)(blok2_name;blok2_podminka)(...)...
//                                          - pozadavek na zobrazeni potvrzovaci sekvence
//  -;MENU;prikaz1,prikaz2,...              - pozadavek na zobrazeni MENU
//  -;INFOMSG;msg                           - zobrazeni informacni zpravy
//  -;BOTTOMERR;err;stanice;technologie     - zobrazeni spodni chyby
//  -;SND;PLAY;code;[delay (ms)]            - prehravani zvuku, delay je nepovinny, pokud neni uveden, je zvuk prehran jen jednou
//  -;SND;STOP;code                         - zastaveni prehravani zvuku

procedure TORTCPServer.Stitek(AContext: TIdContext; Blk:TBlk; stit:string);
begin
 try
   (AContext.Data as TTCPORsRef).stitek := Blk;
   Self.SendLn(AContext, '-;STIT;'+Blk.GetGlobalSettings().name+';'+stit+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := Blk.GetGlobalSettings.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Vyluka(AContext: TIdContext; Blk:TBlk; vyl:string);
begin
 try
   (AContext.Data as TTCPORsRef).vyluka := Blk;
   Self.SendLn(AContext, '-;VYL;'+Blk.GetGlobalSettings().name+';'+vyl+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[7] := Blk.GetGlobalSettings.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Menu(AContext: TIdContext; Blk:TBlk; OblR:TOR; menu:string);
begin
 try
   (AContext.Data as TTCPORsRef).menu    := Blk;
   (AContext.Data as TTCPORsRef).menu_or := OblR;
   Self.SendLn(AContext, '-;MENU;'+menu+';');
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[6] := Blk.GetGlobalSettings.name;
 except

 end;
end;//procedure

procedure TORTCPServer.Potvr(AContext: TIdContext; callback:TPSCallback; stanice:TOR; udalost:string; senders:TBlksList; podminky:TPSPodminky; free_senders:boolean = true; free_podm:boolean = true);
var str:string;
    i:Integer;
begin
 str := '';
 if (Assigned(senders)) then
   for i := 0 to senders.Count-1 do
    if (Assigned(senders[i])) then
     begin
      if ((senders[i].ClassType = TBlk) or
          (senders[i].ClassType = TBlkVyhybka) or (senders[i].ClassType = TBlkUsek) or (senders[i].ClassType = TBlkSCom) or (senders[i].ClassType = TBlkUvazka)) then
       str := str + (senders[i] as TBlk).GetGlobalSettings.name + '|'
      else if (senders[i].ClassType = TOR) then
       str := str + 'Stanovištì výpravèího '+(senders[i] as TOR).Name + '|';
     end;

 str := str + ';';

 if (podminky <> nil) then
   for i := 0 to podminky.Count-1 do
     if (Assigned(podminky[i].blok)) then
      begin
       try
         str := str + '['+(podminky[i].blok as TBlk).GetGlobalSettings.name + '|' + podminky[i].podminka + ']';
       except

       end;
      end;

 try
   (AContext.Data as TTCPORsRef).potvr := callback;
    Self.SendLn(AContext, '-;PS;'+stanice.Name+';'+udalost+';'+str);
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := udalost;
 except

 end;

 if ((free_senders) and (Assigned(senders))) then senders.Free();
 if ((free_podm) and (Assigned(podminky))) then podminky.Free();
end;//procedure

procedure TORTCPServer.PotvrClose(AContext: TIdContext; msg:string = '');
begin
 try
   (AContext.Data as TTCPORsRef).potvr := nil;
   F_Main.LV_Clients.Items.Item[(AContext.Data as TTCPORsRef).index].SubItems.Strings[8] := '';

   if (msg <> '') then
    Self.SendLn(AContext, '-;PS-CLOSE;'+msg)
   else
    Self.SendLn(AContext, '-;PS-CLOSE;');
 except

 end;
end;//procedure

procedure TORTCPServer.PlaySound(AContext: TIdContext; code:Integer; delay_ms:Integer = -1);
begin
 Self.SendLn(AContext, '-;SND;PLAY;'+IntToStr(code)+';'+IntToStr(delay_ms)+';');
end;//procedure

procedure TORTCPServer.DeleteSound(AContext: TIdContext; code:Integer);
begin
 Self.SendLn(AContext, '-;SND;STOP;'+IntToStr(code)+';');
end;//procedure

procedure TORTCPServer.BottomError(AContext: TIdContext; err:string; stanice:string; tech:string);
begin
 Self.SendLn(AContext, '-;BOTTOMERR;'+err+';'+stanice+';'+tech+';');
 writelog('OR ERR: ' + tech + ' : ' + stanice + ' : ' + err, WR_ERROR);
end;//procedure

//  -;UPO;[item1][item2]                    - upozorneni
//  -;UPO-CRIT;[item1][item2]               - kriticke upozorneni - nelze porkacovat dale
//      format [item_x]:
//          (radek1)(radek2)(radek3)
//        radek_x: fg|bg|text         barvy na dalsich radcich nemusi byt vyplnene, pak prebiraji tu barvu, jako radek predchozi
procedure TORTCPServer.UPO(AContext: TIdContext; items:TUPOItems; critical:boolean; callbackOK:TNotifyEvent; callbackEsc:TNotifyEvent; ref:TObject);
var str:string;
    i, j:Integer;
begin
 if (critical) then
  str := '-;UPO-CRIT;'
 else
  str := '-;UPO;';

 for i := 0 to items.Count-1 do
  begin
   str := str + '[';

   for j := 0 to 2 do
    begin
     if (items[i][j].str = '') then break;

     str := str + '[';

     case (items[i][j].align) of
      taLeftJustify  : str := str + 'L|';
      taRightJustify : str := str + 'R|';
      taCenter       : str := str + 'M|';
     end;//acse align

     if (items[i][j].fg <> clNone) then
      str := str + PrevodySoustav.ColorToStr(items[i][j].fg) + '|';
     if (items[i][j].bg <> clNone) then
      str := str + PrevodySoustav.ColorToStr(items[i][j].bg) + '|';
     str := str + items[i][j].str + ']';
    end;//for j
   str := str + ']';
  end;//for i

 try
   (AContext.Data as TTCPORsRef).UPO_OK  := callbackOK;
   (AContext.Data as TTCPORsRef).UPO_Esc := callbackEsc;
   (AContext.Data as TTCPORsRef).UPO_ref := ref;
   Self.SendLn(AContext, str);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.Auth(AContext: TIdContext);
var OblR:TOR;
    i:Integer;
begin
 i := ORs.GetORIndex(parsed[0]);
 if (i = -1) then
  begin
   Self.SendInfoMsg(AContext, 'Tato OR neexistuje');
   Exit;
  end;

 ORs.GetORByIndex(i, OblR);
 if (parsed.Count < 4) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), '', '')
 else if (parsed.Count < 5) then
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], '')
 else
  OblR.PanelAuthorise(AContext, TORControlRights(StrToInt(parsed[2])), parsed[3], parsed[4]);

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastBottomError(err:string; tech:string);
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.BottomError(Self.clients[i].conn, err, '-', tech);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.SendLn(AContext:TIDContext; str:string);
begin
 // vyvolani vyjimky -> spojeni neocekavane preruseno -> melo by zavolat OnDisconnect (automaticky)
 try
   AContext.Connection.IOHandler.WriteLn(str);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// gui metody
// zajistuji komunikaci s F_PanelsStatus

procedure TORTCPServer.GUIInitTable();
var i, j:Integer;
    MI:TListItem;
begin
 F_Main.LV_Clients.Clear();
 for i := 0 to _MAX_OR_CLIENTS-1 do
  begin
   MI := F_Main.LV_Clients.Items.Add;
   MI.Caption := IntToStr(i);
   MI.SubItems.Add('odpojen');
   for j := 1 to F_Main.LV_Clients.Columns.Count-1 do
    MI.SubItems.Add('');
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.GUIRefreshLine(index:Integer);
var i:Integer;
    str:string;
    ORPanel:TORPanel;
begin
 if (not Assigned(F_Main.LV_Clients.Items.Item[index])) then
  Exit;

 if (not Assigned(Self.clients[index])) then
  begin
   // klient neexistuje
   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'odpojen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[i] := '';

   Exit();
  end;

 if (not Assigned(Self.clients[index].conn)) then
  begin
   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'soket nenalezen';
   for i := 1 to F_Main.LV_Clients.Columns.Count-1 do
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[i] := '';
  end;

   case (Self.clients[index].status) of
    TPanelConnectionStatus.closed    : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'uzavøeno';
    TPanelConnectionStatus.opening   : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'otevírání';
    TPanelConnectionStatus.handshake : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'handshake';
    TPanelConnectionStatus.opened    : F_Main.LV_Clients.Items.Item[index].SubItems.Strings[0] := 'otevøeno';
   end;

   F_Main.LV_Clients.Items.Item[index].SubItems.Strings[1] := Self.clients[index].conn.Connection.Socket.Binding.PeerIP;

   for i := 0 to 2 do
    begin
     if (i < (Self.clients[index].conn.Data as TTCPORsRef).ORsCnt) then
      begin
       // klient existuje
       (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
       F_Main.LV_Clients.Items.Item[index].SubItems.Strings[2+i] :=
        (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')';
      end else begin
       // klient neexistuje
       F_Main.LV_Clients.Items.Item[index].SubItems.Strings[2+i] := '';
      end;
    end;//for i

   if ((Self.clients[index].conn.Data as TTCPORsRef).ORsCnt > 3) then
    begin
     str := '';
     for i := 3 to (Self.clients[index].conn.Data as TTCPORsRef).ORsCnt-1 do
      begin
       (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].GetORPanel(Self.clients[index].conn, ORPanel);
       str := str + (Self.clients[index].conn.Data as TTCPORsRef).ORs[i].ShortName + ' (' + ORPanel.user + ' :: ' + TOR.GetRightsString(ORPanel.Rights) +')' + ', ';
      end;
     F_Main.LV_Clients.Items.Item[index].SubItems.Strings[5] := '';
    end;

   if ((Self.clients[index].conn.Data as TTCPORsRef).menu <> nil) then
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[6] := (Self.clients[index].conn.Data as TTCPORsRef).menu.GetGlobalSettings.name
   else begin
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[6] := '';
   end;

   if ((Self.clients[index].conn.Data as TTCPORsRef).vyluka <> nil) then
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := (Self.clients[index].conn.Data as TTCPORsRef).vyluka.GetGlobalSettings.name
   else begin
     if ((Self.clients[index].conn.Data as TTCPORsRef).stitek <> nil) then
      F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := (Self.clients[index].conn.Data as TTCPORsRef).stitek.GetGlobalSettings.name
     else
      F_Main.LV_Clients.Items.Item[index].SubItems.Strings[7] := '';
   end;

   if (not Assigned((Self.clients[index].conn.Data as TTCPORsRef).potvr)) then
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[8] := '';

   if ((Self.clients[index].conn.Data as TTCPORsRef).regulator) then
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[10] := 'ano'
   else
    F_Main.LV_Clients.Items.Item[index].SubItems.Strings[10] := 'ne';

 F_Main.LV_Clients.Repaint();
end;//procedure

procedure TORTCPServer.GUIRefreshTable();
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  Self.GUIRefreshLine(i); 
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPORsRef.Escape();
begin
 Self.stitek      := nil;
 Self.vyluka      := nil;
 Self.potvr       := nil;
 Self.menu        := nil;
 Self.menu_or     := nil;
 Self.UPO_OK      := nil;
 Self.UPO_Esc     := nil;
 Self.UPO_ref     := nil;

 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[6] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[7] := '';
 F_Main.LV_Clients.Items.Item[Self.index].SubItems.Strings[8] := '';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.CancelUPO(AContext: TIdContext; ref:TObject);
begin
 try
   if ((AContext.Data as TTCPORsRef).UPO_ref = ref) then
    begin
     Self.SendLn(AContext, '-;UPO-CLOSE');
     (AContext.Data as TTCPORsRef).UPO_ref := nil;
     (AContext.Data as TTCPORsRef).UPO_OK  := nil;
     (AContext.Data as TTCPORsRef).UPO_Esc := nil;
    end;
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.BroadcastData(data:string);
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
    Self.SendLn(Self.clients[i].conn, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DCCStart();
begin
 Self.BroadcastData('-;DCC;GO;');
 Self.DCCStopped := nil;
end;//procedure

procedure TORTCPServer.DCCStop();
var i:Integer;
begin
 for i := 0 to _MAX_OR_CLIENTS-1 do
  if (Assigned(Self.clients[i])) then
   begin
    if (Self.DCCStopped = Self.clients[i].conn) then
      Self.SendLn(Self.clients[i].conn, '-;DCC;STOP')
    else
      Self.SendLn(Self.clients[i].conn, '-;DCC;DISABLED');
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.OnDCCCmdErr(Sender:TObject; Data:Pointer);
begin
 Self.BottomError(TIdContext(Data), 'Centrála neodpovìdìla na pøíkaz', '-', 'CENTRÁLA');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORTCPServer.DisconnectClient(conn:TIdContext);
begin
 conn.Connection.Disconnect();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORTCPServer.GetClient(index:Integer):TORTCPClient;
begin
 if (index < _MAX_OR_CLIENTS) then
   Result := Self.clients[index]
 else
   Result := nil;
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
 ORTCPServer := TORTCPServer.Create;

finalization
 FreeAndNil(ORTCPServer);

end.//unit
