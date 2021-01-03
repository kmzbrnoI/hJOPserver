unit Area;

{
  TArea class definition.

  Area (station) is a section of railway designed to be controlled
  from one place. Panel can contain multiple areas. Are is identified by
  "house" in the panel.

  TArea provides communication between technological blocks and panels
  (panel server).
}

interface

uses IniFiles, SysUtils, Classes, Graphics, Menus, stanicniHlaseni,
      IdContext, TechnologieRCS, StrUtils, ComCtrls, Forms, orLighting,
      Generics.Collections, Stack, Windows, Generics.Defaults;

const
  _MAX_CON_PNL = 16;  // max number of panels connected to an area
  _MAX_ORREF = 16;

  // these constants must match constantd defined in clients
  _SND_TRAT_ZADOST = 4;
  _SND_PRIVOLAVACKA = 5;
  _SND_TIMEOUT = 6;
  _SND_PRETIZENI = 7;
  _SND_POTVR_SEKV = 8;
  _SND_ZPRAVA = 9;
  _SND_CHYBA = 10;
  _SND_STAVENI_VYZVA = 11;
  _SND_NENI_JC = 12;

type
  TAreaRights = (null = 0, read = 1, write = 2, superuser = 3);
  TPanelButton = (F1, F2, ENTER, ESCAPE);

  EMaxClients = class(Exception);
  ENoClientConnected = class(Exception);

  // confirmation sequence condition
  TConfSeqItem = record
   block: string;
   note: string;
  end;

  TConfSeqItems = TList<TConfSeqItem>;

  TAreaData = record
   name: string;
   nameShort: string;
   id: string;
   lights: TObjectList<TORLighting>;
  end;

  TCSCallback = procedure(Relief: TObject; Panel: TObject; success: Boolean) of object;
  TBlkCallback = procedure (SenderPnl: TIDContext; SenderOR: TObject; Button: TPanelButton) of object;

  TAreaCountdown = record
   start: TDateTime;
   duration: TDateTime;
   callback: TNotifyEvent;
   id: Integer;
  end;

  TAreaPanel = record
   panel: TIdContext;
   rights: TAreaRights;
   user: string;
  end;

  TORState = record
   NUZtimer: Boolean; // probiha ruseni useku NUZ?
   NUZblkCnt: Integer; // kolik bloku ma zaplych NUZ
   NUZmerCasuID: Integer; // ID mereni casu NUZ
   shortCircBlkCnt: Integer; // kolik bloku je ve zkratu (vyuzivano pro prehravani zvuku)
   railwayReqBlkCnt: Integer; // pocet uvazek, na ktere je zadost o tratovy souhlas (kvuli prehravani zvuku)
   pnBlkCnt: Integer; // pocet aktivnich privolavacich navesti
   timerCnt: Integer; // pocet bezicich timeru
   dkClickCallback: TBlkCallback; // callback kliku na dopravni kancelar
   regPlease: TIdCOntext; // zde je ulozen regulator, ktery danou oblast rizeni zada o prideleni lokomotivy
  end;

  TAreaRCSModule = class
   failed: Boolean;
    constructor Create(failed: Boolean = false);
  end;

  TAreaRCSs = class
   modules: TObjectDictionary<Cardinal, TAreaRCSModule>;
   failure: Boolean; // jestli doslo k selhani jakohokoliv RCS modulu v OR
   lastFailureTime: TDateTime; // cas posledniho selhani (pouziva se pro vytvareni souhrnnych zprav o selhani RCS modulu pro dispecera)

    constructor Create();
    destructor Destroy(); override;
  end;

  /////////////////////////////////////////////////////////////////////////////

  TArea = class
    private const
     _COM_ACCESS_DENIED = 'Přístup odepřen';

    private
     m_index: Integer;  // index in all areas
     m_data: TAreaData;
     m_state: TORState;
     countdowns: TList<TAreaCountdown>;
     RCSs: TAreaRCSs; // list of RCS addresses in area (based on blocks)

      procedure PanelDbAdd(panel: TIdContext; rights: TAreaRights; user: string);
      procedure PanelDbRemove(panel: TIdContext; contextDestroyed: Boolean = false);
      function PanelDbRights(panel: TIdContext): TAreaRights;
      function PanelDbIndex(panel: TIdContext): Integer;

      procedure NUZTimeOut(Sender: TObject); // callback ubehnuti mereni casu pro ruseni nouzovych zaveru bloku
      procedure NUZ_PS(Sender: TIdContext; success: Boolean); // callback potvrzovaci sekvence NUZ

      procedure ORAuthoriseResponse(Panel: TIdContext; Rights: TAreaRights; msg: string; username: string);

      procedure SetNUZBlkCnt(new: Integer);
      procedure SetShortCircBlkCnt(new: Integer);
      procedure SetRailwayReqBlkCnt(new: Integer);
      procedure SetPnBlkCnt(new: Integer);
      procedure SetTimerCnt(new: Integer);

      procedure RCSUpdate(); // posila souhrnne zpravy panelu o vypadku RCS modulu (moduly, ktere vypadly hned za sebou - do 500 ms, jsou nahlaseny v jedne chybe)

      procedure SendState(panel: TIdContext); // odeslani stavu IR do daneho panelu, napr. kam se ma posilat klik na DK, jaky je stav zasobniku atp.; je ovlano pri pripojeni panelu, aby se nastavila OR do spravneho stavu
      procedure SendLn(panel: TIdContext; str: string);

      // tyto funkce jsou volany pri zmene opravenni mezi cteni a zapisem
      // primarni cil = v techto funkcich resit zapinani a vypinani zvuku v panelu
      procedure AuthReadToWrite(panel: TIdContext);
      procedure AuthWriteToRead(panel: TIdContext);

      procedure OnAnncmntAvailable(Sender: TObject; available: Boolean);
      procedure NUZPrematureZaverRelease(Sender: TObject; data: Integer);
      procedure NUZcancelPrematureEvents();

      procedure SetIndex(newIndex: Integer);

      procedure PanelTrainChangeOk(Sender: TObject; Data: Pointer);
      procedure PanelTrainChangeErr(Sender: TObject; Data: Pointer);
      procedure PanelTrainCreateErr(Sender: TObject; Data: Pointer);

      procedure SetLights(id: string; state: Boolean);

      procedure DkNUZStart(Sender: TIdContext);
      procedure DkNUZStop(Sender: TIdContext);

      procedure DkHvFuncsSetOk(Sender: TObject; Data: Pointer);
      procedure DkHvFuncsSetErr(Sender: TObject; Data: Pointer);

      procedure DkMenuShowOsv(Sender: TIdContext);
      procedure DkMenuShowLok(Sender: TIdContext);
      procedure ShowDkMenu(panel: TIdContext; root: string; menustr: string);

    public
     stack: TORStack; // zasobnik povelu
     changed: Boolean; // jestli doslo ke zmene OR - true znamena aktualizaci tabulky
     vb: TList<TObject>; // seznam variantnich bodu, ktere jsou aktualne "naklikle"; zde je ulozen seznam bloku
     connected: TList<TAreaPanel>;
     announcement: TStanicniHlaseni;

      constructor Create(index: Integer);
      destructor Destroy(); override;

      procedure LoadData(str: string);
      procedure LoadStat(ini: TMemIniFile; section: string);
      procedure SaveStat(ini: TMemIniFile; section: string);

      // smaze klienta \Panel z databze pripojenych panelu, typicky volano pri odpojeni klienta
      procedure RemoveClient(Panel: TIdContext; contextDestroyed: Boolean = false);

      procedure Update();
      procedure DisconnectPanels();

      function AddCountdown(callback: TNotifyEvent; len: TDateTime): Byte;
      procedure RemoveCountdown(id: Integer);

      procedure RCSAdd(addr: integer);
      procedure RCSFail(addr: integer);

      procedure UpdateLine(LI: TListItem); // aktualizuje zaznam v tabulce oblasti rizeni ve F_Main

      procedure BroadcastData(data: string; min_rights: TAreaRights = read); // posle zpravu \data vsem pripojenym panelum s minimalnim opravnenim \min_rights s prefixem oblaati rizeni
      procedure BroadcastGlobalData(data: string; min_rights: TAreaRights = read); // posle zpravu \data vsem pripojenym panelum s minimalnim opravnenim \min_rights s prefixem "-"
      procedure BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read; stanice: string = '');

      procedure ClearVb();                                                      // smaze aktualni varientni body

      //--- called from technological blocks ---
      procedure BlkChange(Sender: TObject; specificClient: TIDContext = nil);     // doslo ke zmene bloku v OR, je potreba propagovat zmenu do panelu
      procedure BlkPlaySound(Sender: TObject; min_rights: TAreaRights;       // prehraje zvuk
          sound: Integer; loop: Boolean = false);
      procedure BlkRemoveSound(Sender: TObject; sound: Integer);                  // zrusi prehravani zvuku
      procedure BlkWriteError(Sender: TObject; error: string; system: string);     // posle chybovou hlasku do vsech stanic, ktere maji autorizovany zapis
      procedure BlkNewTrain(Sender: TObject; Panel: TIdContext; trainUsekIndex: Integer); // posle do panelu pozadavek na otevreni dialogu pro novou soupravu
      procedure BlkEditTrain(Sender: TObject; Panel: TIdContext; train: TObject);// posle do panelu pozadavek na otevreni dialogu editace soupravy

      //--- called from areas ---
      procedure ORSendMessage(Sender: TArea; msg: string);
      procedure ORDKClickServer(callback: TBlkCallback);
      procedure ORDKClickClient();

      // volany pri zadosti o poskytnuti loko pro regulator:
      function LokoPlease(Sender: TIDContext; user: TObject; comment: string): Integer;
      procedure LokoCancel(Sender: TIdContext);

      procedure OsvInit();

      //--- komunikace s panely zacatek: ---
      procedure PanelAuthorise(Sender: TIdContext; rights: TAreaRights; username: string; password: string);
      procedure PanelFirstGet(Sender: TIdContext);
      procedure PanelClick(Sender: TIdContext; blokid: Integer; Button: TPanelButton; params: string = '');
      procedure PanelEscape(Sender: TIdContext);
      procedure PanelMessage(Sender: TIdContext; recepient: string; msg: string);
      procedure PanelHVList(Sender: TIdContext);
      procedure PanelTrainChange(Sender: TIdContext; trainstr: TStrings);
      procedure PanelMoveLok(Sender: TIdContext; lok_addr: word; new_or: string);
      procedure PanelZAS(Sender: TIdContext; str: TStrings);
      procedure PanelDKClick(SenderPnl: TIdContext; Button: TPanelButton);
      procedure PanelLokoReq(Sender: TIdContext; str: TStrings);
      procedure PanelHlaseni(Sender: TIDContext; str: TStrings);
      procedure PanelDkMenuClick(Sender: TIdContext; rootItem, subItem: string);

      procedure PanelHVAdd(Sender: TIDContext; str: string);
      procedure PanelHVRemove(Sender: TIDContext; addr: Integer);
      procedure PanelHVEdit(Sender: TIDContext; str: string);

      function PanelGetTrains(Sender: TIdCOntext): string;
      procedure PanelRemoveTrain(Sender: TIDContext; train_index: Integer);

      function GetORPanel(conn: TIdContext; var ORPanel: TAreaPanel): Integer;
      class function GetRightsString(rights: TAreaRights): string;

      procedure UserUpdateRights(user: TObject);
      procedure UserDelete(userid: string);

      class function ORRightsToString(rights: TAreaRights): string;
      class function GetPSPodminka(blok: TObject; podminka: string): TConfSeqItem; overload;
      class function GetPSPodminka(cil: string; podminka: string): TConfSeqItem; overload;
      class function GetPSPodminky(podm: TConfSeqItem): TConfSeqItems;

      class function NameComparer(): IComparer<TArea>;
      class function IdComparer(): IComparer<TArea>;

      property NUZtimer: Boolean read m_state.NUZtimer write m_state.NUZtimer;
      property NUZblkCnt: Integer read m_state.NUZblkCnt write SetNUZBlkCnt;
      property shortCircBlkCnt: Integer read m_state.shortCircBlkCnt write SetShortCircBlkCnt;
      property railwayReqBlkCnt: Integer read m_state.railwayReqBlkCnt write SetRailwayReqBlkCnt;
      property pnBlkCnt: Integer read m_state.pnBlkCnt write SetPnBlkCnt;
      property timerCnt: Integer read m_state.TimerCnt write SetTimerCnt;
      property regPlease: TIdContext read m_state.regPlease;

      property name: string read m_data.name;
      property shortName: string read m_data.nameShort;
      property id: string read m_data.id;
      property index: Integer read m_index write SetIndex;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////

uses BlockDb, GetSystems, BlockTrack, BlockSignal, fMain, Logging, TechnologieJC,
     TJCDatabase, ownConvert, TCPServerOR, TOblsRizeni, Block, THVDatabase, TrainDb,
     UserDb, THnaciVozidlo, Trakce, User, TCPORsRef, fRegulator, RegulatorTCP,
     ownStrUtils, Train, changeEvent, TechnologieTrakce;

constructor TArea.Create(index: Integer);
begin
 inherited Create();

 Self.m_index := index;

 Self.m_data.lights := TObjectList<TOrLighting>.Create();
 Self.connected := TList<TAreaPanel>.Create();
 Self.RCSs := TAreaRCSs.Create();

 Self.m_state.dkClickCallback := nil;
 Self.m_state.regPlease := nil;

 Self.stack := TORStack.Create(index, Self);
 Self.vb := TList<TObject>.Create();
 Self.changed := false;

 Self.countdowns := TList<TAreaCountdown>.Create();
 Self.announcement := nil;
end;

destructor TArea.Destroy();
begin
 if (Assigned(Self.announcement)) then
   Self.announcement.Free();

 Self.stack.Free();
 Self.m_data.lights.Free();
 Self.vb.Free();
 Self.countdowns.Free();
 Self.connected.Free();
 Self.RCSs.Free();

 inherited;
end;

constructor TAreaRCSs.Create();
begin
 inherited;
 Self.modules := TObjectDictionary<Cardinal, TAreaRCSModule>.Create();
end;

destructor TAreaRCSs.Destroy();
begin
 Self.modules.Free();
 inherited;
end;

constructor TAreaRCSModule.Create(failed: Boolean = false);
begin
 inherited Create();
 Self.failed := failed;
end;

////////////////////////////////////////////////////////////////////////////////

//nacitani dat OR
//na kazdem radku je ulozena jedna oblast rizeni ve formatu:
//  nazev;nazev_zkratka;id;(osv_RCS|osv_port|osv_name)(osv_RCS|...)...;;
procedure TArea.LoadData(str:string);
var data_main, lights: TStrings;
    lightsStr: string;
begin
 data_main := TStringList.Create();
 lights := TStringList.Create();

 try
   ExtractStrings([';'],[],PChar(str), data_main);

   if (data_main.Count < 3) then
     raise Exception.Create('Mene nez 3 parametry v popisu oblasti rizeni');

   Self.m_data.name := data_main[0];
   Self.m_data.nameShort := data_main[1];
   Self.m_data.id := data_main[2];

   Self.m_data.lights.Clear();

   lights.Clear();
   if (data_main.Count > 3) then
    begin
     ExtractStringsEx([')'], ['('], data_main[3], lights);
     for lightsStr in lights do
      begin
       try
         Self.m_data.lights.Add(TOrLighting.Create(lightsStr));
       except

       end;
      end;
    end;

   Self.announcement := TStanicniHlaseni.Create(Self.id);
   Self.announcement.OnAvailable := Self.OnAnncmntAvailable;
 finally
   FreeAndNil(data_main);
   FreeAndNil(lights);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// nacitani or_stat.ini souboru
// musi byt volano po LoadData
procedure TArea.LoadStat(ini: TMemIniFile; section: string);
var light: TOrLighting;
begin
 for light in Self.m_data.lights do
   light.default_state := ini.ReadBool(section, light.name, false);
end;

// ukladani or_stat.ini souboru
procedure TArea.SaveStat(ini: TMemIniFile; section: string);
var osv: TOrLighting;
begin
 for osv in Self.m_data.lights do
   ini.WriteBool(section, osv.name, osv.default_state);
end;

////////////////////////////////////////////////////////////////////////////////

//tato funkce je vyvolana pri zmene stavu jakehokoliv bloku
procedure TArea.BlkChange(Sender: TObject; specificClient: TIDContext = nil);
var msg: string;
    areaPanel: TAreaPanel;
begin
 if (Self.connected.Count = 0) then Exit();

 msg := 'CHANGE;' + TBlk(Sender).PanelStateString();

 for areaPanel in Self.connected do
  begin
   if (areaPanel.rights < TAreaRights.read) then continue;
   if ((specificClient <> nil) and (areaPanel.panel <> specificClient)) then continue;

   Self.SendLn(areaPanel.panel, msg);

   // aktualizace menu
   if ((areaPanel.panel.Data as TTCPORsRef).menu = Sender) then
     ORTCPServer.Menu(areaPanel.panel, (Sender as TBlk), Self, (Sender as TBlk).ShowPanelMenu(areaPanel.panel,
                      Self, areaPanel.rights));
  end;
end;

procedure TArea.BlkWriteError(Sender: TObject; error: string; system: string);
var areaPanel: TAreaPanel;
begin
 for areaPanel in Self.connected do
  if (areaPanel.Rights >= TAreaRights.write) then
    ORTCPServer.BottomError(areaPanel.Panel, error, Self.shortName, system);
end;

procedure TArea.BlkPlaySound(Sender: TObject; min_rights: TAreaRights; sound: Integer; loop: Boolean = false);
var areaPanel: TAreaPanel;
begin
 for areaPanel in Self.connected do
   if (areaPanel.Rights >= min_rights) then
     ORTCPServer.PlaySound(areaPanel.Panel, sound, loop);
end;

procedure TArea.BlkRemoveSound(Sender: TObject; sound: Integer);
var areaPanel: TAreaPanel;
begin
 for areaPanel in Self.connected do
   ORTCPServer.DeleteSound(areaPanel.Panel, sound);
end;

procedure TArea.BlkNewTrain(Sender: TObject; Panel: TIdContext; trainUsekIndex: Integer);
begin
 TTCPORsRef(Panel.Data).train_new_usek_index := trainUsekIndex;
 TTCPORsRef(Panel.Data).train_usek := Sender;
 Self.SendLn(Panel, 'SPR-NEW;');
end;

procedure TArea.BlkEditTrain(Sender: TObject; Panel: TIdContext; train: TObject);
begin
 TTCPORsRef(Panel.Data).train_new_usek_index := -1;
 TTCPORsRef(Panel.Data).train_edit := TTrain(train);
 TTCPORsRef(Panel.Data).train_usek := Sender;

 Self.SendLn(Panel, 'SPR-EDIT;'+TTrain(train).GetPanelString());
end;

////////////////////////////////////////////////////////////////////////////////
//funkce pro praci s databazi pripojenych panelu

//pridani 1 prvku do databaze
//v pripade existence jen zvysime prava
procedure TArea.PanelDbAdd(panel: TIdContext; rights: TAreaRights; user: string);
var i: Integer;
    pnl: TAreaPanel;
begin
 for i := 0 to Self.connected.Count-1 do
  begin
   if (Self.connected[i].Panel = panel) then
    begin
     // pokud uz je zaznam v databazi, pouze upravime tento zaznam
     pnl := Self.connected[i];
     pnl.rights := rights;
     pnl.user := user;
     Self.connected[i] := pnl;
     authLog('or', 'reauth', user, Self.id + ' :: ' + Self.GetRightsString(rights));
     Exit();
    end;
  end;

 if (Self.connected.Count >= _MAX_CON_PNL) then
   raise EMaxClients.Create('Připojen maximální počet klientů');
 if ((panel.Data as TTCPORsRef).ORs.Count >= _MAX_ORREF) then
   raise EMaxClients.Create('Připojen maximální OR k jedné stanici');

 pnl.panel := panel;
 pnl.rights := rights;
 pnl.user := user;
 Self.connected.Add(pnl);

 authLog('or', 'login', user, Self.id + ' :: ' + Self.GetRightsString(rights));

 // pridame referenci na sami sebe do TIDContext
 (panel.Data as TTCPORsRef).ORs.Add(Self);

 // odesleme incializacni udaje
 if (rights > TAreaRights.null) then
  begin
   Self.SendState(panel);
   Self.stack.NewConnection(panel);
  end;
end;

procedure TArea.PanelDbRemove(panel: TIdContext; contextDestroyed: Boolean = false);
var i: Integer;
begin
 for i := 0 to Self.connected.Count-1 do
  begin
   if (Self.connected[i].Panel = panel) then
    begin
     authLog('or', 'logout', Self.connected[i].user, Self.id);
     Self.connected.Delete(i);
     Break;
    end;
  end;

 if (not contextDestroyed) then
   if ((panel.Data as TTCPORsRef).ORs.Contains(Self)) then
     (panel.Data as TTCPORsRef).ORs.Remove(Self);
end;

function TArea.PanelDbRights(panel: TIdContext): TAreaRights;
var index: Integer;
begin
 index := Self.PanelDbIndex(panel);
 if (index < 0) then
  begin
   Result := TAreaRights.null;
   Exit();
  end;

 Result := Self.connected[index].Rights;
end;

function TArea.PanelDbIndex(panel: TIdContext): Integer;
var i: Integer;
begin
 for i := 0 to Self.connected.Count-1 do
   if (Self.connected[i].Panel = panel) then
     Exit(i);
 Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

//touto funkci panel zada o opravneni
procedure TArea.PanelAuthorise(Sender: TIdContext; rights: TAreaRights; username: string; password: string);
var i: Integer;
    userRights: TAreaRights;
    msg: string;
    panel: TAreaPanel;
    user: TUser;
    lastRights: TAreaRights;
begin
 // panel se chce odpojit -> vyradit z databaze
 if (rights = TAreaRights.null) then
  begin
   Self.ORAuthoriseResponse(Sender, TAreaRights.null, 'Úspěšně autorizováno - odpojen', '');
   ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
   if (Self.PanelDbRights(Sender) >= write) then Self.AuthWriteToRead(Sender);
   Self.PanelDbRemove(Sender);
   Exit();
  end;

 // tady mame zaruceno, ze panel chce zadat o neco vic, nez null

 user := UsrDb.GetUser(username);

 if (not Assigned(user)) then
  begin
   userRights := TAreaRights.null;
   msg := 'Uživatel '+username+' neexistuje !';
  end else

 if (user.ban) then
  begin
   userRights := TAreaRights.null;
   msg := 'Uživatel '+user.username+' má BAN !';
  end else

 if (not TUser.ComparePasswd(password, user.password, user.salt)) then
  begin
   userRights := TAreaRights.null;
   msg := 'Neplatné heslo !';
  end else begin
   userRights := user.GetRights(Self.id);
   if (userRights < rights) then
     msg := 'K této OŘ nemáte oprávnění';
  end;

 // do last_rights si ulozime posledni opravneni panelu
 lastRights := Self.PanelDbRights(Sender);

 try
   if (userRights = TAreaRights.null) then
    begin
     Self.PanelDbRemove(Sender);
     Self.ORAuthoriseResponse(Sender, userRights, msg, '');
     ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
     Exit();
    end;
   if (rights > userRights) then
     rights := userRights;

   if ((not GetFunctions.GetSystemStart) and (rights > read) and (rights < superuser)) then
    begin
     // superuser muze autorizovat zapis i pri vyplych systemech
     Self.PanelDbAdd(Sender, TAreaRights.read, username);
     Self.ORAuthoriseResponse(Sender, TAreaRights.read, 'Nelze autorizovat zápis při vyplých systémech !', user.fullName);
     ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
     Exit();
    end;

   msg := 'Úspěšně autorizováno !';

   // kontrola pripojeni dalsich panelu
   // pokud chce panel zapisovat, musime zkontrolovat, jestli uz nahodou neni nejaky panel s pravy zapisovat, pripojeny
   if (rights = TAreaRights.write) then
    begin
     // pokud jsme superuser, pripojenost dalsich panelu nekontrolujeme
     for i := 0 to Self.connected.Count-1 do
      begin
       if ((Self.connected[i].Rights = write) and (Self.connected[i].Panel <> Sender)) then
        begin
         // pokud se pripojuje stejny uzivatel, prevezme rizeni z jiz pripojene OR
         //  jiny uzivatel rizeni prevzit nemuze
         // -> technologie pripojovani zarucuje, ze pripojeny dispecer muze byt jen jeden
         if (Self.connected[i].user = username) then
          begin
           panel := Self.connected[i];
           panel.rights := TAreaRights.read;
           Self.connected[i] := panel;
           Self.ORAuthoriseResponse(panel.panel, panel.rights, 'Převzetí řízení', user.fullName);
           ORTCPServer.GUIQueueLineToRefresh(i);
          end else begin
           rights := TAreaRights.read;
           msg := 'Panel již připojen!';
           break;
          end;
        end;
      end;//for i
    end;

   Self.PanelDbAdd(Sender, rights, username);
  except
    on E: EMaxClients do
     begin
      ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);
      Self.ORAuthoriseResponse(Sender, TAreaRights.null, E.Message, user.fullName);
      Exit();
     end;
  end;

 UsrDb.LoginUser(username);
 Self.ORAuthoriseResponse(Sender, rights, msg, user.fullName);
 ORTCPServer.GUIQueueLineToRefresh((Sender.Data as TTCPORsRef).index);

 if ((rights > read) and (lastRights <= read)) then Self.AuthReadToWrite(Sender);
 if ((rights < write) and (lastRights >= write)) then Self.AuthWriteToRead(Sender);
end;

//ziskani stavu vsech bloku v panelu
procedure TArea.PanelFirstGet(Sender: TIdContext);
var addr: Integer;
    rights: TAreaRights;
begin
 rights := Self.PanelDbRights(Sender);
 if (rights < read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 Blocks.GetAreaBlk(Self.id, Sender);

 // zjistime RUC u vsech hnacich vozidel
 for addr := 0 to _MAX_ADDR-1 do
   if ((HVDb[addr] <> nil) and (HVDb[addr].Stav.stanice = Self)) then
     HVDb[addr].UpdateRuc(false);
end;

////////////////////////////////////////////////////////////////////////////////

//v panelu je kliknuto na urcity blok
procedure TArea.PanelClick(Sender: TIdContext; blokid: Integer; Button: TPanelButton; params: string = '');
var Blk: TBlk;
    rights: TAreaRights;
    area: TArea;
begin
 rights := Self.PanelDbRights(Sender);
 if (rights < TAreaRights.write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 if (Blocks.GetBlkByID(blokid, Blk) <> 0) then Exit();

 // musime provest kontrolu, jestli OR ma povoleno menit blok
 // tj. jestli ma technologicky blok toto OR

 for area in Blk.areas do
  begin
   if (area = Self) then
    begin
     Blk.PanelClick(Sender, Self, Button, rights, params);
     Exit();
    end;
  end;

 ORTCPServer.SendInfoMsg(Sender, 'Nemáte oprávnění měnit tento blok');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelEscape(Sender: TIdContext);
var Blk: TBlk;
begin
 //kontrola opravneni klienta
 if (Self.PanelDbRights(Sender) < TAreaRights.write) then
  begin
    // ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
    // tady se schvalne neposila informace o chybe - aby klienta nespamovala chyba v momente, kdy provadi escape a nema autorizovana vsechna OR na panelu
   Exit();
  end;

 Self.ORDKClickClient();

 if (Self.vb.Count > 0) then
  begin
   (Self.vb[Self.vb.Count-1] as TBlkTrack).jcEnd := TZaver.no;
   Self.vb.Delete(Self.vb.Count-1);
  end else begin
   Blk := Blocks.GetBlkSignalSelected(Self.id);
   if (Blk <> nil) then (Blk as TBlkSignal).selected := TBlkSignalSelection.none;
  end;

 Blk := Blocks.GetBlkUsekVlakPresun(Self.id);
 if (Blk <> nil) then (Blk as TBlkTrack).trainMoving := -1;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.DkNUZStart(Sender: TIdContext);
var Blk: TBlk;
    podminky: TList<TConfSeqItem>;
    area: TArea;
begin
 podminky := TList<TConfSeqItem>.Create();
 // zjisteni jmen bloku:
 for blk in Blocks do
  begin
   if (Blk.typ <> btTrack) then continue;
   if (not (Blk as TBlkTrack).NUZ) then continue;

   for area in (Blk as TBlkTrack).areas do
     if (area = Self) then
       podminky.Add(GetPSPodminka(Blk, 'Nouzové vybavování'));
  end;//for i

 ORTCPServer.Potvr(Sender, Self.NUZ_PS, Self, 'Nouzové uvolnění závěrů úseků', TBlocks.GetBlksList(Self), podminky);
end;

procedure TArea.DkNUZStop(Sender: TIdContext);
begin
 Self.NUZcancelPrematureEvents();
 Blocks.NUZ(Self.id, false);
 Self.NUZblkCnt := 0; // zastavi mereni casu (melo by zastavit uz volani vyse)
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelMessage(Sender: TIdContext; recepient: string; msg: string);
var area: TArea;
begin
 if (Self.PanelDbRights(Sender) < TAreaRights.write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 area := ORs.Get(recepient);
 if (area = nil) then
  begin
   Self.SendLn(Sender, 'MSG-ERR;' + recepient + ';Tato OŘ neexistuje');
   Exit();
  end;

 try
   area.ORSendMessage(Self, msg);
 except
   on E: ENoClientConnected do
     Self.SendLn(Sender, 'MSG-ERR;' + recepient + ';K této OŘ aktuálně není připojen žádný panel');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// pozadavek na ziskani sezmu hnacich vozidel
procedure TArea.PanelHVList(Sender: TIdContext);
var addr: Integer;
    str: string;
begin
 //kontrola opravneni klienta
 if (Self.PanelDbRights(Sender) < TAreaRights.read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 str := 'HV;LIST;{';
 for addr := 0 to _MAX_ADDR-1 do
   if ((Assigned(HVDb[addr])) and (HVDb[addr].Stav.stanice = Self)) then
    str := str + '[{' + HVDb[addr].GetPanelLokString(full) + '}]';
 str := str + '}';
 Self.SendLn(Sender, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelTrainChange(Sender: TIdContext; trainstr: TStrings);
var track: TBlkTrack;
    train: TTrain;
begin
 if (Self.PanelDbRights(Sender) < TAreaRights.write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 if ((TTCPORsRef(Sender.Data).train_new_usek_index = -1) and (TTCPORsRef(Sender.Data).train_edit = nil)) then
  begin
   Self.SendLn(Sender, 'SPR-EDIT-ERR;Žádná souprava k editaci / neplatný úsek pro vytvoření soupravy');
   Exit();
  end;

 try
  if (TTCPORsRef(Sender.Data).train_new_usek_index > -1) then begin
    // nova souprava
    Trains.Add(
      trainstr, TTCPORsRef(Sender.Data).train_usek, Self,
      (TTCPORsRef(Sender.Data).train_new_usek_index),
      TTrakce.Callback(Self.PanelTrainChangeOk, Sender),
      TTrakce.Callback(Self.PanelTrainCreateErr, Sender)
    );
  end else begin

   // uprava soupravy
   track := (TTCPORsRef(Sender.Data).train_usek as TBlkTrack);
   train := TTCPORsRef(Sender.Data).train_edit;

   if (not track.IsTrain(TTCPORsRef(Sender.Data).train_edit.index)) then
    begin
     Self.SendLn(Sender, 'SPR-EDIT-ERR;Souprava již není na úseku');
     Exit();
    end;

   if ((train.front <> track) and (train.wantedSpeed > 0)) then
    begin
     Self.SendLn(Sender, 'SPR-EDIT-ERR;Nelze editovat soupravu, která odjela a je v pohybu');
     Exit();
    end;

   TTCPORsRef(Sender.Data).train_edit.UpdateTrainFromPanel(
      trainstr, track, Self,
      TTrakce.Callback(Self.PanelTrainChangeOk, Sender),
      TTrakce.Callback(Self.PanelTrainChangeErr, Sender)
   );
  end;
 except
  on E: Exception do
    Self.SendLn(Sender, 'SPR-EDIT-ERR;'+E.Message);
 end;
end;

procedure TArea.PanelTrainChangeOk(Sender: TObject; Data: Pointer);
var tcpSender: TIdContext;
begin
 tcpSender := Data;
 TTCPORsRef(tcpSender.data).ResetTrains();
 Self.SendLn(tcpSender, 'SPR-EDIT-ACK;');
end;

procedure TArea.PanelTrainChangeErr(Sender: TObject; Data: Pointer);
var tcpSender: TIdContext;
begin
 tcpSender := Data;
 Self.SendLn(tcpSender, 'SPR-EDIT-ERR;Nepodařilo se převzít lokomotivy z centrály!');
end;

procedure TArea.PanelTrainCreateErr(Sender: TObject; Data: Pointer);
var tcpSender: TIdContext;
begin
 tcpSender := Data;
 Self.SendLn(tcpSender, 'SPR-EDIT-ERR;Souprava založena, ale nepodařilo se převízt lokomotivy z centrály!');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelMoveLok(Sender: TIdContext; lok_addr: word; new_or: string);
var new: TArea;
begin
 if (Self.PanelDbRights(Sender) < TAreaRights.write) then
  begin
   Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';ERR;Přístup odepřen');
   Exit();
  end;

 new := ORs.Get(new_or);
 if (new = nil) then
  begin
   Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';ERR;Tato OR neexistuje!');
   Exit();
  end;
 if (not Assigned(HVDb[lok_addr])) then
  begin
   Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';ERR;HV neexistuje!');
   Exit();
  end;
 if (HVDb[lok_addr].Stav.train > -1) then
  begin
   Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';ERR;HV přiřazeno soupravě '+
               Trains.GetTrainNameByIndex(HVDb[lok_addr].Stav.train)+'!');
   Exit();
  end;
 if (HVDb[lok_addr].Stav.stanice <> Self) then
  begin
   Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';ERR;HV nepatří této stanici!');
   Exit();
  end;

 HVDb[lok_addr].MoveToArea(new);
 Self.SendLn(Sender, 'HV;MOVE;'+IntToStr(lok_addr)+';OK');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.Update();
var i: Integer;
begin
 Self.RCSUpdate();
 Self.stack.Update();

 //aktualizace mereni casu:
 for i := 0 to Self.countdowns.Count-1 do
  begin
   if (Now >= (Self.countdowns[i].Start + Self.countdowns[i].duration)) then
    begin
     if (Assigned(Self.countdowns[i].callback)) then
       Self.countdowns[i].callback(Self);
     Self.countdowns.Delete(i);
     break;
    end;
  end;
end;

// vraci id pridaneho mereni
function TArea.AddCountdown(callback: TNotifyEvent; len: TDateTime): Byte;
var id: Integer;
    mc: TAreaCountdown;
begin
 if (Self.countdowns.Count > 0) then
  id := Self.countdowns[Self.countdowns.Count-1].id+1
 else
  id := 0;

 // pridat mereni casu do vsech OR:
 Self.BroadcastData('CAS;START;'+IntToStr(id)+';'+FormatDateTime('s', len)+';');

 mc.start    := Now;
 mc.duration   := len;
 mc.callback := callback;
 mc.id       := id;
 Self.countdowns.Add(mc);

 Result := id;
end;

procedure TArea.RemoveCountdown(id: Integer);
var i: Integer;
begin
 for i := Self.countdowns.Count-1 downto 0 do
   if (Self.countdowns[i].id = id) then
     Self.countdowns.Delete(i);

 Self.BroadcastData('CAS;STOP;'+IntToStr(id)+';');
end;

////////////////////////////////////////////////////////////////////////////////

// zavola se, az probehne meerni casu:
procedure TArea.NUZTimeOut(Sender: TObject);
begin
 Self.NUZcancelPrematureEvents();
 Self.NUZtimer := false;
 Blocks.NUZ(Self.id);
 Self.NUZblkCnt := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.NUZ_PS(Sender: TIdContext; success: Boolean);
var JC: TJC;
    blk: TBlk;
    track: TBlkTrack;
    signal: TBlkSignal;
    area: TArea;
begin
 if (not success) then Exit();

 Self.m_state.NUZtimer := true;

 // ruseni pripadnych jiznich cest:
 for blk in Blocks do
  begin
   if (blk.typ <> btTrack) then continue;
   track := blk as TBlkTrack;
   if (not track.NUZ) then continue;

   for area in track.areas do
    begin
     if (area = Self) then
      begin
       track.AddChangeEvent(track.eventsOnZaverReleaseOrAB, CreateChangeEvent(Self.NUZPrematureZaverRelease, 0));
       JC := JCDb.FindActiveJCWithTrack(blk.id);

       if (JC <> nil) then
        begin
         Blocks.GetBlkByID(JC.data.signalId, TBlk(signal));
         if ((signal.signal > ncStuj) and (signal.DNjc = JC)) then
           ORTCPServer.BottomError(JC.state.SenderPnl, 'Chyba povolovací návěsti '+signal.name,
                                   Self.shortName, 'TECHNOLOGIE');
         JC.CancelWithoutTrackRelease();
         if (signal.DNjc = JC) then
           signal.DNjc := nil;
        end;
      end;
    end;
  end;

 Self.BroadcastData('NUZ;2;');
 Self.m_state.NUZmerCasuID := Self.AddCountdown(Self.NUZTimeOut, EncodeTime(0, 0, 20, 0));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.ORAuthoriseResponse(Panel: TIdContext; Rights: TAreaRights; msg: string; username: string);
begin
 Self.SendLn(Panel, 'AUTH;'+IntToStr(Integer(Rights))+';'+msg+';'+username);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.RemoveClient(Panel: TIdContext; contextDestroyed: Boolean = false);
begin
 Self.PanelDbRemove(Panel, contextDestroyed);
 Self.stack.OnDisconnect(Panel);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetNUZBlkCnt(new: Integer);
begin
 if ((Self.m_state.NUZblkCnt = 0) and (new > 0)) then
  begin
   // zacina NUZ, informovat oblasti rizeni
   Self.BroadcastData('NUZ;1;');
  end;

 if ((Self.m_state.NUZblkCnt > 0) and (new = 0)) then
  begin
   // nekdo si rekl, ze bloky nechce nuzovat
   Self.BroadcastData('NUZ;0;');
   if (Self.NUZtimer) then
    begin
     Self.NUZtimer := false;
     Self.RemoveCountdown(Self.m_state.NUZmerCasuID);
    end;
  end;

 Self.m_state.NUZblkCnt := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetShortCircBlkCnt(new: Integer);
var i: Integer;
begin
 if (new < 0) then Exit(); 

 if ((new > 2) and (Self.m_state.shortCircBlkCnt = 2)) then
  begin
   // V OR nastal zkrat -> prehrat zvuk
   for i := 0 to Self.connected.Count-1 do
    if (Self.connected[i].Rights > TAreaRights.read) then
     ORTCPServer.PlaySound(Self.connected[i].Panel, _SND_PRETIZENI, true);
  end;

 if ((new <= 2) and (Self.m_state.shortCircBlkCnt = 2)) then
  begin
   // zkrat skoncil -> vypnout zvuk
   for i := 0 to Self.connected.Count-1 do
     ORTCPServer.DeleteSound(Self.connected[i].Panel, _SND_PRETIZENI);
  end;

 Self.m_state.shortCircBlkCnt := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetRailwayReqBlkCnt(new: Integer);
var i: Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.railwayReqBlkCnt = 0)) then
  begin
   // nastala zadost -> prehrat zvuk
   for i := 0 to Self.connected.Count-1 do
    if (Self.connected[i].Rights > TAreaRights.read) then
     ORTCPServer.PlaySound(Self.connected[i].Panel, _SND_TRAT_ZADOST, true);
  end;

 if ((new = 0) and (Self.railwayReqBlkCnt > 0)) then
  begin
   // skocnila zadost -> vypnout zvuk
   for i := 0 to Self.connected.Count-1 do
     ORTCPServer.DeleteSound(Self.connected[i].Panel, _SND_TRAT_ZADOST);
  end;

 Self.m_state.railwayReqBlkCnt := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetPnBlkCnt(new: Integer);
var i: Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.pnBlkCnt = 0)) then
  begin
   // aktivace prvni privolavaci navesti -> prehrat zvuk
   for i := 0 to Self.connected.Count-1 do
    if (Self.connected[i].Rights > TAreaRights.read) then
     ORTCPServer.PlaySound(Self.connected[i].Panel, _SND_PRIVOLAVACKA, true);
  end;

 if ((new = 0) and (Self.pnBlkCnt > 0)) then
  begin
   // skocnila posledni privolavaci navest -> vypnout zvuk
   for i := 0 to Self.connected.Count-1 do
     ORTCPServer.DeleteSound(Self.connected[i].Panel, _SND_PRIVOLAVACKA);
  end;

 Self.m_state.pnBlkCnt := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetTimerCnt(new: Integer);
var i: Integer;
begin
 if (new < 0) then Exit();

 if ((new > 0) and (Self.timerCnt = 0)) then
  begin
   // aktivace prvniho timeru -> prehrat zvuk
   for i := 0 to Self.connected.Count-1 do
    if (Self.connected[i].Rights > TAreaRights.read) then
     ORTCPServer.PlaySound(Self.connected[i].Panel, _SND_TIMEOUT, true);
  end;

 if ((new = 0) and (Self.timerCnt > 0)) then
  begin
   // skocnil posledni timer -> vypnout zvuk
   for i := 0 to Self.connected.Count-1 do
     ORTCPServer.DeleteSound(Self.connected[i].Panel, _SND_TIMEOUT);
  end;

 Self.m_state.timerCnt := new;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.DisconnectPanels();
var i: Integer;
    index: Integer;
begin
 for i := Self.connected.Count-1 downto 0 do
  begin
   Self.ORAuthoriseResponse(Self.connected[i].Panel, TAreaRights.null, 'Odpojení systémů', '');
   index := (Self.connected[i].Panel.Data as TTCPORsRef).index;
   Self.PanelDbRemove(Self.connected[i].Panel);
   ORTCPServer.GUIQueueLineToRefresh(index);
 end;

 Self.stack.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.ORSendMessage(Sender: TArea; msg: string);
var areaPanel: TAreaPanel;
begin
 for areaPanel in Self.connected do
  begin
   if (areaPanel.Rights >= TAreaRights.write) then
    begin
     Self.SendLn(areaPanel.Panel, 'MSG;' + Sender.id + ';{'+msg+'}');
     Exit();
    end;
  end;

 raise ENoClientConnected.Create('Nepřipojen žádný klient!');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.RCSAdd(addr: integer);
begin
 try
   if (not Self.RCSs.modules.ContainsKey(addr)) then
     Self.RCSs.modules.Add(addr, TAreaRCSModule.Create(false));
 except

 end;
end;

procedure TArea.RCSFail(addr: integer);
begin
 try
   if (not Self.RCSs.modules.ContainsKey(addr)) then Exit();
   Self.RCSs.modules[addr].failed := true;
   Self.RCSs.failure := true;
   Self.RCSs.lastFailureTime := Now;
 except

 end;
end;

procedure TArea.RCSUpdate();
var addr: Integer;
    str: string;
    panel: TAreaPanel;
begin
 if (not Self.RCSs.failure) then Exit();

 if ((Self.RCSs.lastFailureTime + EncodeTime(0, 0, 0, 500)) < Now) then
  begin
   str := 'Výpadek RCS modulu ';
   for addr in Self.RCSs.modules.Keys do
    if (Self.RCSs.modules[addr].failed) then
     begin
      str := str + IntToStr(addr) + ', ';
      Self.RCSs.modules[addr].failed := false;
     end;

   str := LeftStr(str, Length(str)-2);
   Self.RCSs.failure := false;

   for panel in Self.connected do
     if (panel.rights >= read) then
       ORTCPServer.BottomError(panel.panel, str, Self.shortName, 'TECHNOLOGIE');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.UpdateLine(LI: TListItem);
var str: string;
    i: Integer;
begin
 LI.Caption := IntToStr(Self.index);
 LI.SubItems.Strings[0] := Self.name;
 LI.SubItems.Strings[1] := Self.shortName;
 LI.SubItems.Strings[2] := Self.id;
 str := Self.stack.GetList();
 LI.SubItems.Strings[3] := RightStr(str, Length(str)-2);

 case (Self.stack.mode) of
  TORStackMode.PV : LI.SubItems.Strings[4] := 'PV';
  TORStackMode.VZ : LI.SubItems.Strings[4] := 'VZ';
 end;

 str := '';
 for i := 0 to Self.m_data.lights.Count-1 do
   str := str + '(' + Self.m_data.lights[i].name + ' - ' + IntToStr(Self.m_data.lights[i].rcsAddr.board) + ':' +
          IntToStr(Self.m_data.lights[i].rcsAddr.port) + ')';
 LI.SubItems.Strings[5] := str;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelZAS(Sender: TIdContext; str: TStrings);
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 Self.stack.ParseCommand(Sender, str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetLights(id: string; state: Boolean);
var osv: TOrLighting;
begin
 for osv in Self.m_data.lights do
  if (osv.name = id) then
   begin
    try
      RCSi.SetOutput(osv.rcsAddr, ownConvert.BoolToInt(state));
      osv.default_state := state;
    except

    end;

    Exit();
   end;
end;

procedure TArea.OsvInit();
var osv: TOrLighting;
begin
 try
   for osv in Self.m_data.lights do
     if (RCSi.IsModule(osv.rcsAddr.board)) then
       RCSi.SetOutput(osv.rcsAddr, ownConvert.BoolToInt(osv.default_state));
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TArea.PanelGetTrains(Sender: TIdCOntext): string;
var i: Integer;
begin
 if (Self.PanelDbRights(Sender) < read) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit('');
  end;

 Result := '{';
 for i := 0 to _MAX_TRAIN-1 do
   if ((Assigned(Trains[i])) and (Trains[i].station = Self)) then
    Result := Result + '[{' + Trains[i].GetPanelString() + '}]';
 Result := Result + '}';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelRemoveTrain(Sender: TIDContext; train_index: integer);
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 if ((Trains[train_index] <> nil) and (Trains[train_index].station = Self)) then
  begin
   Trains.Remove(train_index);
   ORTCPServer.SendInfoMsg(Sender, 'Souprava smazána');
   Exit();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelHVAdd(Sender: TIDContext; str: string);
var HV: THV;
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   Self.SendLn(Sender, 'HV;ADD;-;ERR;Přístup odepřen');
   Exit();
  end;

 try
   HV := HVDb.Add(str, Self);
 except
   on e: Exception do
    begin
     Self.SendLn(Sender, 'HV;ADD;-;ERR;'+e.Message);
     Exit();
    end;
 end;

 Self.SendLn(Sender, 'HV;ADD;'+HV.addrStr+';OK');
end;

procedure TArea.PanelHVRemove(Sender: TIDContext; addr: Integer);
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   Self.SendLn(Sender, 'HV;REMOVE;'+IntToStr(addr)+';ERR;Přístup odepřen');
   Exit();
  end;
 if (HVDb[addr] = nil) then
  begin
   Self.SendLn(Sender, 'HV;REMOVE;'+IntToStr(addr)+';ERR;Loko neexsituje');
   Exit();
  end;
 if (HVDb[addr].Stav.stanice <> self) then
  begin
   Self.SendLn(Sender, 'HV;REMOVE;'+IntToStr(addr)+';ERR;Loko se nenachází ve stanici '+Self.name);
   Exit();
  end;

 try
   HVDb.Remove(addr);
 except
   on E: Exception do
     Self.SendLn(Sender, 'HV;REMOVE;'+IntToStr(addr)+';ERR;'+E.Message);
 end;

 Self.SendLn(Sender, 'HV;REMOVE;'+IntToStr(addr)+';OK');
end;

procedure TArea.PanelHVEdit(Sender: TIDContext; str: string);
var data: TStrings;
    addr: Integer;
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   Self.SendLn(Sender, 'HV;EDIT;-;ERR;Přístup odepřen');
   Exit();
  end;

 data := nil;
 data := TStringList.Create();
 addr := 0;
 try
   ExtractStringsEx(['|'], [], str, data);
   addr := StrToInt(data[4]);
   data.Free();
   if (HVDb[addr] = nil) then
    begin
     Self.SendLn(Sender, 'HV;EDIT;'+IntToStr(addr)+';ERR;Loko neexistuje');
     Exit();
    end;
   if (HVDb[addr].Stav.stanice <> self) then
    begin
     Self.SendLn(Sender, 'HV;EDIT;'+IntToStr(addr)+';ERR;Loko se nenachází ve stanici '+Self.name);
     Exit();
    end;

   HVDb[addr].UpdateFromPanelString(str);

   if (HVDb[addr].acquired) then
     HVDb[addr].StavFunctionsToSlotFunctions(TTrakce.Callback(), TTrakce.Callback());

   Self.SendLn(Sender, 'HV;EDIT;'+IntToStr(addr)+';OK');
 except
   on e: Exception do
    begin
     Self.SendLn(Sender, 'HV;EDIT;'+IntToStr(addr)+';ERR;'+E.Message);
     if (Assigned(data)) then data.Free();
    end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.BroadcastData(data: string; min_rights: TAreaRights = read);
var panel: TAreaPanel;
begin
 for panel in Self.connected do
   if (panel.rights >= min_rights) then
     Self.SendLn(panel.panel, data);
end;

procedure TArea.BroadcastGlobalData(data: string; min_rights: TAreaRights = read);
var panel: TAreaPanel;
begin
 for panel in Self.connected do
   if (panel.rights >= min_rights) then
     ORTCPServer.SendLn(panel.panel, '-;'+data);
end;

procedure TArea.BroadcastBottomError(err: string; tech: string; min_rights: TAreaRights = read; stanice: string = '');
var panel: TAreaPanel;
begin
 if (stanice = '') then
   stanice := Self.shortName;

 for panel in Self.connected do
   if (panel.rights >= min_rights) then
     ORTCPServer.BottomError(panel.panel, err, stanice, tech);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.ORDKClickServer(callback: TBlkCallback);
begin
 Self.m_state.dkClickCallback := callback;
 Self.BroadcastData('DK-CLICK;1', TAreaRights.write);
end;

procedure TArea.ORDKClickClient();
begin
 if (not Assigned(Self.m_state.dkClickCallback)) then Exit();

 Self.m_state.dkClickCallback := nil;
 Self.BroadcastData('DK-CLICK;0', TAreaRights.write);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelDKClick(SenderPnl: TIdContext; Button: TPanelButton);
begin
 if (Assigned(Self.m_state.dkClickCallback)) then
   Self.m_state.dkClickCallback(SenderPnl, Self, Button);
end;

// Tato procedura parsuje "LOK-REQ" z panelu.
procedure TArea.PanelLokoReq(Sender: TIdContext; str: TStrings);
var data: TStrings;
    i, j, addr: Integer;
    HV: THV;
    rights: TAreaRights;
    line: string;
    Blk: TBlk;
    traini: Integer;
begin
//  or;LOK-REQ;PLEASE;addr1|addr2|...       - zadost o vydani tokenu
//  or;LOK-REQ;PLEASE-U;blk_id              - zadost o vydani tokenu pro vozidla soupravy na danem techologickem bloku
//  or;LOK-REQ;LOK;addr1|addr2|...          - lokomotivy pro rucni rizeni na zaklade PLEASE regulatoru vybrany
//  or;LOK-REQ;DENY;                        - odmitnuti pozadavku na rucni rizeni

 //kontrola opravneni klienta
 rights := Self.PanelDbRights(Sender);
 if (rights < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 str[2] := UpperCase(str[2]);

 // zadost o vydani tokenu
 // odpovedi:
  //  or;LOK-TOKEN;OK;[addr|token][addr|token] - odpověď na žádost o token, je posílano také při RUČ loko
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
       HV := HVDb[StrToInt(data[i])];
       if (HV = nil) then
        begin
         Self.SendLn(Sender, 'LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' neexistuje');
         Exit();
        end;

       // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
       if ((HV.Stav.stanice <> Self) and (rights < TAreaRights.superuser)) then
        begin
         Self.SendLn(Sender, 'LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' se nenachází ve stanici');
         Exit();
        end;

       // nelze vygenerovat token pro loko, ktere je uz v regulatoru
       if ((HV.Stav.regulators.Count > 0) and (rights < TAreaRights.superuser)) then
        begin
         Self.SendLn(Sender, 'LOK-TOKEN;ERR;'+str[3]+';Loko '+data[i]+' již otevřeno v regulátoru');
         Exit();
        end;
      end;//for i

     // kontrola OK -> generujeme zpravu z tokeny a zpravu odesleme
     line := 'LOK-TOKEN;OK;';
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb[StrToInt(data[i])];
       line := line + '[' + IntToStr(HV.addr) + '|' + HV.GetToken() + ']';
      end;//for i
     Self.SendLn(Sender, line);

     data.Free();
   except
     Self.SendLn(Sender, 'LOK-TOKEN;ERR;Neplatný formát argumentů');
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
     if (Self.m_state.regPlease = nil) then
      begin
       Self.SendLn(Sender, 'LOK-REQ;ERR;Neprobíhá žádná žádost z regulátoru');
       Exit();
      end;

     data := TStringList.Create();
     ExtractStringsEx(['|'], [], str[3], data);

     // zkontrolujeme vsechna LOKO
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb[StrToInt(data[i])];
       if (HV = nil) then
        begin
         Self.SendLn(Sender, 'LOK-REQ;ERR;Loko '+data[i]+' neexistuje');
         Exit();
        end;

       // pokud je uzvatel pripojen jako superuser, muze prevzit i loko, ktere se nenachazi ve stanici
       if ((HV.Stav.stanice <> Self) and (rights < TAreaRights.superuser)) then
        begin
         Self.SendLn(Sender, 'LOK-REQ;ERR;Loko '+data[i]+' se nenachází ve stanici');
         Exit();
        end;

       // nelze vygenerovat token pro loko, ktere je uz v regulatoru
       if ((HV.Stav.regulators.Count > 0) and (rights < TAreaRights.superuser)) then
        begin
         Self.SendLn(Sender, 'LOK-REQ;ERR;Loko '+data[i]+' již otevřeno v regulátoru');
         Exit();
        end;
      end;//for i


     // kontrola OK -> odesleme panelu zpravu o tom, ze je vse OK
     Self.SendLn(Sender, 'LOK-REQ;OK;');

     // vsem ostatnim panelum jeste posleme, ze doslo ke zruseni zadosti
     for i := 0 to Self.connected.Count-1 do
       if ((Self.connected[i].Rights >= TAreaRights.read) and (Self.connected[i].Panel <> Sender)) then
        Self.SendLn(Self.connected[i].Panel, 'LOK-REQ;CANCEL;');

     // lokomotivy priradime regulatoru
     for i := 0 to data.Count-1 do
      begin
       HV := HVDb[StrToInt(data[i])];
       TCPRegulator.LokToRegulator(Self.m_state.regPlease, HV);
      end;//for i

     // zrusit zadost regulatoru
     (Self.m_state.regPlease.Data as TTCPORsRef).regulator_zadost := nil;
     Self.m_state.regPlease := nil;

     data.Free();
   except
     Self.SendLn(Sender, 'LOK-REQ;ERR;Neplatný formát argumentů');
   end;
  end

 // relief odmitl zadost regulatoru o lokomotivu
 else if (str[2] = 'DENY') then
  begin
   ORTCPServer.SendLn(Self.m_state.regPlease, '-;LOK;G;PLEASE-RESP;err;Dispečer odmítl žádost');
   Self.BroadcastData('LOK-REQ;CANCEL;');
   (Self.m_state.regPlease.Data as TTCPORsRef).regulator_zadost := nil;
   Self.m_state.regPlease := nil;
  end

//  or;LOK-REQ;U-PLEASE;blk_id;train_index      - zadost o vydani seznamu hnacich vozidel na danem useku
//  mozne odpovedi:
//    or;LOK-REQ;U-OK;[hv1][hv2]...           - seznamu hnacich vozidel v danem useku
//    or;LOK-REQ;U-ERR;info                   - chyba odpoved na pozadavek na seznam loko v danem useku

 else if (str[2] = 'U-PLEASE') then
  begin
   try
     Blocks.GetBlkByID(StrToInt(str[3]), Blk);
     if ((Blk = nil) or ((Blk.typ <> btTrack) and (Blk.typ <> btRT))) then
      begin
       Self.SendLn(Sender, 'LOK-REQ;U-ERR;Neplatný blok');
       Exit();
      end;

     if (not (Blk as TBlkTrack).IsTrain()) then
      begin
       Self.SendLn(Sender, 'LOK-REQ;U-ERR;Žádná souprava na bloku');
       Exit();
      end;

     traini := -1;
     if (str.Count > 4) then
      begin
       traini := StrToIntDef(str[4], -1);
       if ((traini < -1) or (traini >= (Blk as TBlkTrack).trains.Count)) then
        begin
         Self.SendLn(Sender, 'LOK-REQ;U-ERR;Tato souprava na úseku neexistuje');
         Exit();
        end;
      end;

     // generujeme zpravu s tokeny
     line := 'LOK-REQ;U-OK;{';
     if (traini = -1) then
      begin
       // vsechny soupravy na useku
       for j := 0 to (Blk as TBlkTrack).trains.Count-1 do
         for addr in Trains[(Blk as TBlkTrack).trains[j]].HVs do
           line := line + '[{' + HVDb[addr].GetPanelLokString() + '}]';
      end else begin
       // konkretni souprava
       for addr in Trains[(Blk as TBlkTrack).trains[traini]].HVs do
         line := line + '[{' + HVDb[addr].GetPanelLokString() + '}]';
      end;

     line := line + '}';
     Self.SendLn(Sender, line);

   except
     Self.SendLn(Sender, 'LOK-REQ;U-ERR;Neplatný formát argumentů');
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// odesle status oblasti rizeni po prihlaseni klienta
procedure TArea.SendState(panel: TIdContext);
var user: TUser;
begin
 // kliknuti na dopravni kancelar
 if (Assigned(Self.m_state.dkClickCallback)) then
   Self.SendLn(panel, 'DK-CLICK;1')
 else
   Self.SendLn(panel, 'DK-CLICK;0');

 // pripradna zadost o lokomotivu
 if (Self.regPlease <> nil) then
  begin
   user := (Self.regPlease.Data as TTCPORsRef).regulator_user;
   if (user <> nil) then
     Self.SendLn(panel, 'LOK-REQ;REQ;'+user.username+';'+user.firstname+';'+user.lastname+';');
  end;

 if ((Assigned(Self.announcement)) and (Self.announcement.available)) then
   Self.SendLn(panel, 'SHP;AVAILABLE;1');

 if ((Self.NUZblkCnt > 0) and (not Self.NUZtimer)) then
   Self.SendLn(panel, 'NUZ;1;');
end;

procedure TArea.SendLn(panel: TIdContext; str: string);
begin
 ORTCPServer.SendLn(panel, Self.id + ';' + str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.ClearVb();
var i: Integer;
begin
 for i := 0 to Self.vb.Count-1 do
  (Self.vb[i] as TBlkTrack).jcEnd := TZaver.no;
 Self.vb.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

function TArea.GetORPanel(conn: TIdContext; var ORPanel: TAreaPanel): Integer;
var i: Integer;
begin
 for i := 0 to Self.connected.Count-1 do
   if (Self.connected[i].Panel = conn) then
    begin
     ORPanel := Self.connected[i];
     Exit(0);
    end;
 Result := 1;
end;

////////////////////////////////////////////////////////////////////////////////

class function TArea.GetRightsString(rights: TAreaRights): string;
begin
 case (rights) of
  TAreaRights.null: Result := 'null';
  TAreaRights.read: Result := 'read';
  TAreaRights.write: Result := 'write';
  TAreaRights.superuser: Result := 'superuser';
 else
  Result := '';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// je volano v pripade, ze dojde ke zmene opravenni za behu programu
procedure TArea.UserUpdateRights(user: TObject);
var rights: TAreaRights;
    areaPanel: TAreaPanel;
begin
 for areaPanel in Self.connected do
  begin
   // je pripojeny uzivatel s vyssimi opravevnimi, nez jsou mu pridelena?
   rights := TUser(user).GetRights(Self.id);
   if ((areaPanel.user = TUser(user).username) and ((areaPanel.Rights > rights) or (TUser(user).ban))) then
    begin
     if (TUser(user).ban) then rights := TAreaRights.null;
     Self.PanelDbAdd(areaPanel.Panel, rights, TUser(user).username);
     Self.ORAuthoriseResponse(areaPanel.Panel, rights, 'Snížena oprávnění uživatele', '');
    end;
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.UserDelete(userid: string);
var i: Integer;
begin
 for i := Self.connected.Count-1 downto 0 do
  begin
   if (Self.connected[i].user = userid) then
    begin
     Self.ORAuthoriseResponse(Self.connected[i].Panel, TAreaRights.null, 'Uživatel smazán', '');
     Self.PanelDbRemove(Self.connected[i].Panel);
    end;
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

// vraci 1 pokud zadost jiz probiha
// vraci 0 pokud prikaz probehl vporadku
function TArea.LokoPlease(Sender: TIDContext; user: TObject; comment: string): Integer;
var str: string;
begin
 if (Self.m_state.regPlease <> nil) then Exit(1);
 Self.m_state.regPlease := Sender;

 str := 'LOK-REQ;REQ;'+TUser(user).username+';';
 if (TUser(user).firstname <> '') then str := str + TUser(user).firstname + ';' else str := str + '-;';
 if (TUser(user).lastname <> '') then str := str + TUser(user).lastname + ';' else str := str + '-;';
 if (comment <> '') then str := str + comment + ';' else str := str + '-;';

 Self.BroadcastData(str);

 Result := 0;
end;

procedure TArea.LokoCancel(Sender: TIdContext);
begin
 if (Self.m_state.regPlease = nil) then Exit();
 Self.m_state.regPlease := nil;
 Self.BroadcastData('LOK-REQ;CANCEL;');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.AuthReadToWrite(panel: TIdContext);
begin
 if (Self.shortCircBlkCnt > 2) then ORTCPServer.PlaySound(panel, _SND_PRETIZENI, true);
 if (Self.railwayReqBlkCnt > 0) then ORTCPServer.PlaySound(panel, _SND_TRAT_ZADOST, true);
 if (Self.pnBlkCnt > 0) then ORTCPServer.PlaySound(panel, _SND_PRIVOLAVACKA, true);
 if (Self.timerCnt > 0) then ORTCPServer.PlaySound(panel, _SND_TIMEOUT, true);
end;

procedure TArea.AuthWriteToRead(panel: TIdContext);
begin
 if (Self.shortCircBlkCnt > 2) then ORTCPServer.DeleteSound(panel, _SND_PRETIZENI);
 if (Self.railwayReqBlkCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_TRAT_ZADOST);
 if (Self.pnBlkCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_PRIVOLAVACKA);
 if (Self.timerCnt > 0) then ORTCPServer.DeleteSound(panel, _SND_TIMEOUT);
 Self.stack.OnWriteToRead(panel);
end;

////////////////////////////////////////////////////////////////////////////////

class function TArea.ORRightsToString(rights: TAreaRights): string;
begin
 case (rights) of
  null: Result := 'žádná oprávnění';
  read: Result := 'oprávnění ke čtení';
  write: Result := 'oprávnění k zápisu';
  superuser: Result := 'superuser';
 else
  Result := '';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TArea.GetPSPodminka(blok: TObject; podminka: string): TConfSeqItem;
begin
 Result.block := TBlk(blok).name;
 Result.note := podminka;
end;

class function TArea.GetPSPodminka(cil: string; podminka: string): TConfSeqItem;
begin
 Result.block := cil;
 Result.note := podminka;
end;

class function TArea.GetPSPodminky(podm: TConfSeqItem): TConfSeqItems;
begin
 Result := TList<TConfSeqItem>.Create();
 Result.Add(podm);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.OnAnncmntAvailable(Sender: TObject; available: Boolean);
begin
 if (available) then
   Self.BroadcastData('SHP;AVAILABLE;1')
 else
   Self.BroadcastData('SHP;AVAILABLE;0');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelHlaseni(Sender: TIDContext; str: TStrings);
begin
 if (not Assigned(Self.announcement)) then Exit();
 if (str.Count < 3) then Exit();

 //kontrola opravneni klienta
 if (Self.PanelDbRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 str[2] := UpperCase(str[2]);

 if (str[2] = 'SPEC') then begin
   Self.announcement.Spec(str[3]);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.NUZPrematureZaverRelease(Sender: TObject; data: Integer);
begin
 if (Self.NUZblkCnt > 0) then
   Self.NUZblkCnt := Self.NUZblkCnt - 1;
end;

procedure TArea.NUZcancelPrematureEvents();
var blk: TBlk;
    usek: TBlkTrack;
    area: TArea;
begin
 for blk in Blocks do
  begin
   if (Blk.typ <> btTrack) then continue;
   usek := Blk as TBlkTrack;
   if (not usek.NUZ) then continue;
   for area in usek.areas do
     if (area = Self) then
       usek.RemoveChangeEvent(usek.eventsOnZaverReleaseOrAB, CreateChangeEvent(Self.NUZPrematureZaverRelease, 0));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.SetIndex(newIndex: Integer);
begin
 if (Self.index = newIndex) then
   Exit();
 Self.stack.index := newIndex;
 Self.m_index := newIndex;
end;

////////////////////////////////////////////////////////////////////////////////

class function TArea.NameComparer(): IComparer<TArea>;
begin
 Result := TComparer<TArea>.Construct(
  function(const Left, Right: TArea): Integer
   begin
    Result := CompareStr(Left.name, Right.name, loUserLocale);
   end
 );
end;

class function TArea.IdComparer(): IComparer<TArea>;
begin
 Result := TComparer<TArea>.Construct(
  function(const Left, Right: TArea): Integer
   begin
    Result := CompareStr(Left.id, Right.id, loUserLocale);
   end
 );
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.PanelDkMenuClick(Sender: TIdContext; rootItem, subItem: string);
begin
 if (Self.PanelDbRights(Sender) < write) then
  begin
   ORTCPServer.SendInfoMsg(Sender, _COM_ACCESS_DENIED);
   Exit();
  end;

 if (subItem = '') then
  begin
   // Root item
   if (rootItem = 'OSV') then
     Self.DkMenuShowOsv(Sender)
   else if (rootItem = 'LOKO') then
     Self.DkMenuShowLok(Sender)
   else if (rootItem = 'NUZ>') then
     Self.DkNUZStart(Sender)
   else if (rootItem = 'NUZ<') then
     Self.DkNUZStop(Sender);
  end else begin
   // Non-root item
   if (rootItem = 'OSV') then
     Self.SetLights(LeftStr(subItem, Length(subItem)-1), (subItem[Length(subItem)] = '>'))
   else if (rootItem = 'LOKO') then begin
     if ((subItem = 'ZVUK>') or (subItem = 'ZVUK<')) then
      begin
       ORTCPServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
       TrakceI.LoksSetFunc(_SOUND_FUNC, (subItem = 'ZVUK>'), TTrakce.Callback(Self.DkHvFuncsSetOk, Sender),
                           TTrakce.Callback(Self.DkHvFuncsSetErr, Sender));
      end else if (subItem = 'ZVUK ztlum') then begin
        ORTCPServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
        TrakceI.TurnOffSound(TTrakce.Callback(Self.DkHvFuncsSetOk, Sender), TTrakce.Callback(Self.DkHvFuncsSetErr, Sender))
      end else if (subItem = 'ZVUK obnov') then begin
        ORTCPServer.SendInfoMsg(Sender, 'Nastavuji funkce...');
        TrakceI.RestoreSound(TTrakce.Callback(Self.DkHvFuncsSetOk, Sender), TTrakce.Callback(Self.DkHvFuncsSetErr, Sender));
      end;
   end;
  end;
end;

procedure TArea.DkMenuShowOsv(Sender: TIdContext);
var menustr: string;
    light: TOrLighting;
begin
 menustr := '$'+Self.name + ',$Osvětlení,-,';
 for light in Self.m_data.lights do
  begin
   menustr := menustr + light.name;
   if (light.active) then
     menustr := menustr + '<,'
   else
     menustr := menustr + '>,';
  end;
 Self.ShowDkMenu(Sender, 'OSV', menustr);
end;

procedure TArea.DkMenuShowLok(Sender: TIdContext);
var menustr: string;
begin
 menustr := '-,';

 if (not HVDb.AllAcquiredHVsHaveActiveFunc(_SOUND_FUNC)) then
   menustr := menustr + 'ZVUK>,';
 if (HVDb.AnyAcquiredHVHasActiveFunc(_SOUND_FUNC)) then
   menustr := menustr + 'ZVUK<,ZVUK ztlum';
 if (HVDb.AnyHvToRestoreFunc(_SOUND_FUNC)) then
   menustr := menustr + 'ZVUK obnov,';

 if (menustr = '-,') then
   menustr := '';
 Self.ShowDkMenu(Sender, 'LOKO', menustr);
end;

procedure TArea.ShowDkMenu(panel: TIdContext; root: string; menustr: string);
begin
 Self.SendLn(panel, 'MENU;'+root+';{'+menustr+'}');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TArea.DkHvFuncsSetOk(Sender: TObject; Data: Pointer);
var panel: TIdContext;
begin
 panel := TIdContext(Data);
 ORTCPServer.SendInfoMsg(panel, 'Funkce nastaveny.');
end;

procedure TArea.DkHvFuncsSetErr(Sender: TObject; Data: Pointer);
var panel: TIdContext;
begin
 panel := TIdContext(Data);
 ORTCPServer.BottomError(panel, 'Nepodařilo se nastavit zvuky hnacích vozidel!', Self.shortName, 'Trakce');
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
