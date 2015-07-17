unit TBlokVyhybka;

//definice a obsluha technologickeho bloku Vyhybka

// redukce menu u bloku vyhybka = vyhybku nelze stavit (ani nouzove)

interface

uses IniFiles, TBlok, TechnologieJC, SysUtils, TBlokUsek, Menus, TOblsRizeni,
     Classes, RPConst, IdContext, Generics.Collections;

type
 TBlkVyhSettings = record
  MTBAddrs:TMTBAddrs;     // poradi(0..3): vst+,vst-,vyst+,vyst-
  spojka:Integer;         // reference na id vyhybky ve spojce
                          // pokud jsou obe vyhybky ve spojce, maji reference na sebe navzajem
                          // zmena MTB vstupu a vystupu v nastaveni jedne vyhybky ovlivnuje druhou
                          // POZOR: jedna data ulozena na dvou mistech, pri nacitani se nekontroluje koherence; SOUBOR MUSI BYT KOHERENTNI (tj. obe vyhybky musi mit navaznosti vzdy na tu druhou)
  zamek:Integer;          // pokud ma vyhybka navaznost na zamek, je zde id bloku zamku; jinak -1
  zamekPoloha:TVyhPoloha; // poloha, v jake se vyhybka musi nachazet pro uzamceni zamku
 end;

 TBlkVyhStav = record
  poloha,polohaOld,poloha_real:TVyhPoloha;    // poloha_real je skutecna poloha, kterou aktualne zobrazuji MTB vstupy
  stit,vyl:string;                            // stitek a vyluka vyhybky
  staveni_minus,staveni_plus:Boolean;         // stavi se zrovna vyhybka do polohy plus, ci minus?
  locked: boolean;                            // skutecny zamek na vystupu - jestli je MTB vystup zamknut
  redukce_menu:Integer;                       // redukovane menu = zamcena vyhybka; 0 = neredukovano, jinak pocet bloku, kolik redukuje
  redukuji:TReduction;                        // zde si ukladam, koho redukuji; ukladaji se id
  vyhZaver:Integer;                           // pocet bloku, ktere na vyhybku udelily nouzovy zaver

  staveniErrCallback, staveniOKCallback:TNotifyEvent;     // callback eventy pro koncovou polohu vyhybky (resp. timeout prestavovani)
  staveniStart:TDateTime;                                 // cas zacatku prestavovani
  staveniPanel:TIDContext;                                // panel, ktery chtel vyhybku prestavit
  staveniOR:TObject;                                      // oblast rizeni, ktera vyzadala staveni vyhybky
 end;

 TBlkVyhybkaRel = record
  UsekID:Integer;
 end;

 TBlkVyhybka = class(TBlk)
  const
   //defaultni stav
   _def_vyh_stav:TBlkVyhStav = (
    poloha : disabled;
    polohaOld : disabled;
    poloha_real : disabled;
    stit : '';
    vyl : '';
    staveni_minus : false;
    staveni_plus : false;
    locked : false;
    redukce_menu: 0;
    vyhZaver: 0;
    staveniErrCallback: nil;
    staveniOKCallback: nil;
    staveniStart: 0;
    staveniPanel: nil;
    staveniOR: nil;
   );

   _VYH_STAVENI_TIMEOUT_SEC = 10;            // timeout na staveni vyhybky je 10 sekund

  private
   VyhSettings:TBlkVyhSettings;
   VyhStav:TBlkVyhStav;
   VyhRel:TBlkVyhybkaRel;
   ORsRef:TORsRef;    //ve kterych OR se blok nachazi

   NullOutput:record
     enabled:boolean;
     NullOutputTime:System.TDateTime;      //500ms to null outputs
   end;

   fzamek:TBlk;
   fparent:TBlk;

    function GetZaver():TJCType;
    function GetNUZ():boolean;
    function GetObsazeno():TUsekStav;

    procedure SetVyhStit(stit:string);
    procedure SetVyhVyl(vyl:string); overload;

    function GetRedukceMenu():boolean;

    procedure UpdatePoloha();
    procedure UpdateStaveniTimeout();
    procedure UpdateZamek();

    procedure Unlock();

    procedure CheckNullOutput();

    procedure PanelStaveniErr(Sender:TObject);

    procedure MenuPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNSPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNSMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVEnableClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVDisableClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure PanelPotvrSekvNSPlus(Sender:TIdContext; success:boolean);
    procedure PanelPotvrSekvNSMinus(Sender:TIdContext; success:boolean);
    procedure PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);

    procedure ORVylukaNull(Sender:TIdContext; success:boolean);

    function GetVyhZaver():boolean;
    procedure SetVyhZaver(zaver:boolean);

    function GetZamek():TBlk;


  public
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

    //----- vyhybka own functions -----

    function GetSettings():TBlkVyhSettings;
    function SetSettings(data:TBlkVyhSettings):Byte;

    function SetPoloha(new:TVyhPoloha; zamek:boolean = false; nouz:boolean = false; callback_ok:TNotifyEvent = nil; callback_err:TNotifyEvent = nil):Integer;
    procedure SetVyhVyl(Sender:TIDCOntext; vyl:string); overload;

    procedure RedukujMenu();
    procedure ZrusRedukciMenu();

    procedure NullVyhZaver();

    property Stav:TBlkVyhStav read VyhStav;

    property Poloha:TVyhPoloha read VyhStav.poloha;
    property NUZ:boolean read GetNUZ;
    property Zaver:TJCType read GetZaver;
    property Obsazeno:TUsekStav read GetObsazeno;
    property Stitek:string read VyhStav.Stit write SetVyhStit;
    property Vyluka:string read VyhStav.Vyl write SetVyhVyl;
    property redukce_menu:boolean read GetRedukceMenu;
    property UsekID:Integer read VyhRel.UsekID;
    property vyhZaver:boolean read GetVyhZaver write SetVyhZaver;
    property zamek:TBlk read GetZamek;

    property StaveniPlus:Boolean read VyhStav.staveni_plus write VyhStav.staveni_plus;
    property StaveniMinus:Boolean read VyhStav.staveni_minus write VyhStav.staveni_minus;

    property OblsRizeni:TORsRef read ORsRef;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
    procedure ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, GetSystems, TechnologieMTB, fMain, TOblRizeni, TJCDatabase,
      TCPServerOR, TBlokZamek;

constructor TBlkVyhybka.Create(index:Integer);
begin
 inherited Create(index);
 Self.GlobalSettings.typ := _BLK_VYH;
 Self.VyhStav := Self._def_vyh_stav;
 Self.fzamek  := nil;
 Self.fparent := nil;
 Self.InitReduction(Self.VyhStav.redukuji);
end;//ctor

destructor TBlkVyhybka.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.VyhSettings.MTBAddrs    := Self.LoadMTB(ini_tech,section);
 Self.VyhSettings.spojka      := ini_tech.ReadInteger(section, 'spojka', -1);
 Self.VyhSettings.zamek       := ini_tech.ReadInteger(section, 'zamek', -1);
 Self.VyhSettings.zamekPoloha := TVyhPoloha(ini_tech.ReadInteger(section, 'zamek-pol', 0));

 Self.VyhStav.Stit := ini_stat.ReadString(section, 'stit', '');
 Self.VyhStav.Vyl  := ini_stat.ReadString(section, 'vyl', '');

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   ExtractStrings([';'],[],PChar(ini_rel.ReadString('V',IntToStr(Self.GlobalSettings.id),'')),str);
   if (str.Count < 2) then Exit;

   Self.ORsRef := ORs.ParseORs(str[0]);
   Self.VyhRel.UsekID := StrToInt(str[1]);

   str.Free();
  end else begin
   Self.ORsRef.Cnt := 0;
   Self.VyhRel.UsekID := -1;
  end;

 PushMTBToOR(Self.ORsRef, Self.VyhSettings.MTBAddrs);
end;//procedure

procedure TBlkVyhybka.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 Self.SaveMTB(ini_tech,section,Self.VyhSettings.MTBAddrs);
 ini_tech.WriteInteger(section, 'spojka', Self.VyhSettings.spojka);
 ini_tech.WriteInteger(section, 'zamek', Self.VyhSettings.zamek);
 ini_tech.WriteInteger(section, 'zamek-pol', Integer(Self.VyhSettings.zamekPoloha));
end;//procedure

procedure TBlkVyhybka.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 ini_stat.WriteString(section, 'stit', Self.VyhStav.Stit);
 ini_stat.WriteString(section, 'vyl', Self.VyhStav.Vyl);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Enable();
var i:Integer;
begin
 if (Self.VyhSettings.MTBAddrs.Count < 4) then Exit; 
 for i := 0 to Self.VyhSettings.MTBAddrs.Count-1 do
   if (not MTB.IsModule(Self.VyhSettings.MTBAddrs.data[i].board)) then
    Exit();

 Self.VyhStav.poloha := none;
 Self.Update();       //update will call Change()
end;//procedure

procedure TBlkVyhybka.Disable();
begin
 Self.VyhStav.poloha := disabled;
 Self.RemoveAllReduction(Self.VyhStav.redukuji);
 Self.Change();
end;//procedure

procedure TBlkVyhybka.Reset();
begin
 Self.RemoveAllReduction(Self.VyhStav.redukuji);
 Self.VyhStav.redukce_menu := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Update();
begin
 Self.CheckNullOutput();
 Self.UpdatePoloha();
 Self.UpdateStaveniTimeout();
 Self.UpdateZamek();

 if (Self.VyhStav.poloha <> Self.VyhStav.polohaOld) then
  begin
   Self.VyhStav.polohaOld := Self.VyhStav.poloha;
   Self.Change();
  end;

 inherited Update();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetZaver():TJCType;
begin
 if (((Self.fparent = nil) and (Self.VyhRel.UsekID <> -1)) or ((Self.fparent.GetGlobalSettings.id <> Self.VyhRel.UsekID))) then
   Blky.GetBlkByID(Self.VyhRel.UsekID, Self.fparent);
 if (Self.fparent <> nil) then
   Result := (Self.fparent as TBlkUsek).Zaver
 else
   Result := TJCType.no;
end;//function

function TBlkVyhybka.GetNUZ():boolean;
var tmpBlk:TBlk;
    return:Integer;
begin
 return := Blky.GetBlkByID(Self.VyhRel.UsekID,tmpBlk);
 if (return < 0) then Exit(false);
 if (tmpBlk.GetGlobalSettings().typ <> _BLK_USEK) then Exit(false);

 Result := (TBlkUsek(tmpBlk)).NUZ;
end;//function

function TBlkVyhybka.GetObsazeno():TUsekStav;
var tmpBlk:TBlk;
    return:Integer;
begin
 return := Blky.GetBlkByID(Self.VyhRel.UsekID,tmpBlk);
 if (return < 0) then Exit(TUsekStav.none);
 if (tmpBlk.GetGlobalSettings().typ <> _BLK_USEK) then Exit(TUsekStav.none);

 Result := (tmpBlk as TBlkUsek).Obsazeno;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetSettings():TBlkVyhSettings;
begin
 Result := Self.VyhSettings;
end;//function

function TBlkVyhybka.SetSettings(data:TBlkVyhSettings):Byte;
var Blk:TBlk;
    spojka_settings:TBlkVyhSettings;
    spojka_old:Integer;
begin
 spojka_old := Self.VyhSettings.spojka;
 Self.VyhSettings := data;

 // kontrola navaznosti spojky
 if (data.spojka > -1) then
  begin
   // zkontrolujeme, pokud spojka uz neexistovala a pokud ano, tak ji smazeme
   if (spojka_old > -1) then
    begin
     Blky.GetBlkByID(spojka_old, Blk);
     if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_VYH)) then Exit(3);

     spojka_settings := (Blk as TBlkVyhybka).GetSettings();
     spojka_settings.spojka  := -1;
     (Blk as TBlkVyhybka).SetSettings(spojka_settings);
    end;

   // pridame spojku do druhe vyhybky
   Blky.GetBlkByID(data.spojka, Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_VYH)) then
    begin
     Self.VyhSettings.spojka := -1;
     Exit(1);
    end;

   spojka_settings := (Blk as TBlkVyhybka).GetSettings();

   // zabraneni cykleni
   if (spojka_settings.spojka = Self.GlobalSettings.id) then Exit(0);

   // na vyhybce je jina spojka - neumoznime tedy spojku vytvorit
   if (spojka_settings.spojka <> -1) then
    begin
     Self.VyhSettings.spojka := -1;
     Exit(2);
    end;

   spojka_settings.spojka   := self.GlobalSettings.id;
   spojka_settings.MTBAddrs := Self.VyhSettings.MTBAddrs;
   (Blk as TBlkVyhybka).SetSettings(spojka_settings);
  end else begin
   // odebereme spojku z druhe vyhybky

   // pokud uz nebyla, neni co odebirat
   if (spojka_old = -1) then Exit(0);

   Blky.GetBlkByID(spojka_old, Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_VYH)) then Exit(3);

   spojka_settings := (Blk as TBlkVyhybka).GetSettings();
   spojka_settings.spojka  := -1;
   (Blk as TBlkVyhybka).SetSettings(spojka_settings);
  end;

 Self.Change();
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.SetVyhStit(stit:string);
begin
 Self.VyhStav.stit := Stit;
 Self.Change();
end;//procedure

procedure TBlkVyhybka.SetVyhVyl(vyl:string);
begin
 Self.VyhStav.vyl := vyl;
 Self.Change();
end;//procedure

procedure TBlkVyhybka.ORVylukaNull(Sender:TIdContext; success:boolean);
begin
 if (success) then
  Self.Vyluka := '';
end;//procedure

procedure TBlkVyhybka.SetVyhVyl(Sender:TIDCOntext; vyl:string);
begin
 if ((self.VyhStav.Vyl <> '') and (vyl = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef.ORs[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil);
  end else begin
   Self.Vyluka := vyl;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetRedukceMenu():boolean;
begin
 if (Self.VyhStav.redukce_menu > 0) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.UpdatePoloha();
var iplus,iminus, i:Integer;           //state of inputs
 begin
  if (Self.VyhSettings.MTBAddrs.Count < 4) then Exit();

  //MTBAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-
  iplus  := MTB.GetInput(Self.VyhSettings.MTBAddrs.data[0].board,Self.VyhSettings.MTBAddrs.data[0].port);
  iminus := MTB.GetInput(Self.VyhSettings.MTBAddrs.data[1].board,Self.VyhSettings.MTBAddrs.data[1].port);

  if ((iplus < 0) or (iminus < 0) or (not MTB.IsModule(Self.VyhSettings.MTBAddrs.data[2].board)) or (not MTB.IsModule(Self.VyhSettings.MTBAddrs.data[3].board))) then
   begin
    if (Self.Stav.poloha <> TVyhPoloha.disabled) then
      Self.VyhStav.poloha := TVyhPoloha.disabled;
    Exit();
   end;


  if ((iplus < 1) and (iminus < 1)) then
   begin
    Self.VyhStav.poloha := none;

    if ((Self.VyhStav.poloha <> Self.VyhStav.poloha_real) and ((Integer(Self.Zaver) > 0) or (Self.vyhZaver) or
      ((Self.redukce_menu) and (not Self.VyhStav.staveni_plus) and (not Self.VyhStav.staveni_minus)) or
      ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen)))
     and (Self.Zaver <> TJCType.staveni)) then
     begin
      for i := 0 to Self.OblsRizeni.Cnt-1 do
        Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Není koncová poloha : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.VyhStav.poloha_real := none;
   end;

  if ((iplus = 1) and (iminus = 0)) then
   begin
    //je-li plus vstup 1
    if (Self.VyhStav.staveni_minus) then Exit;
    
    if (Self.VyhStav.staveni_plus) and (not Self.VyhStav.staveni_minus) then
     begin
      Self.VyhStav.poloha := plus;
      Self.VyhStav.staveni_plus := false;

      if (Assigned(Self.VyhStav.staveniOKCallback)) then
       begin
        Self.VyhStav.staveniOKCallback(Self);
        Self.VyhStav.staveniOKCallback := nil;
       end;
      Self.VyhStav.staveniErrCallback := nil;
     end;

    if ((not Self.VyhStav.staveni_plus) and (Self.VyhStav.poloha <> Self.VyhStav.poloha_real) and (iminus <> 1)) then
     begin
      // sem se dostaneme, pokud se vyhybka nalezne neocekavane v poloze +
      // TJCType.staveni je specialni druh zaveru, ktery neumoznuje zmenu stavu vyhybky uzivatelem, ale zaroven nekrici, pokud se zmeni skutecny stav
      // pouziva se pri staveni JC: vyhybky nechame prestavit, usekum (resp. vyhybkam) ukamzite delime tento zaver a cekame na koncove polohy

      if ((((Integer(Self.Zaver) > 0) or (Self.vyhZaver)) and (Self.Zaver <> TJCType.staveni)) or
          ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen) and (Self.VyhSettings.zamekPoloha <> plus))) then
       begin
        for i := 0 to Self.OblsRizeni.Cnt-1 do
          Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Ztráta dohledu na výhybce : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
        JCDb.RusJC(Self);
       end;//if Blokovani
      Self.VyhStav.poloha := plus;
     end;

    Self.VyhStav.poloha_real := plus;
   end;

  if ((iminus = 1) and (iplus = 0)) then
   begin
    //je-li minus vstup 1
    if (Self.VyhStav.staveni_plus) then Exit;

    if (Self.VyhStav.staveni_minus) and (not Self.VyhStav.staveni_plus) then
     begin
      Self.VyhStav.poloha := minus;
      Self.VyhStav.staveni_minus := false;

      if (Assigned(Self.VyhStav.staveniOKCallback)) then
       begin
        Self.VyhStav.staveniOKCallback(Self);
        Self.VyhStav.staveniOKCallback := nil;
       end;
      Self.VyhStav.staveniErrCallback := nil;
     end;

    if ((not Self.VyhStav.staveni_minus) and (Self.VyhStav.poloha <> Self.VyhStav.poloha_real) and (iplus <> 1)) then
     begin
      //sem se dostaneme, pokud se vyhybka nalezne neocekavane v poloze -
      // redukce menu se tady nekontroluje, protoze vyhybka se z koncove polohy musi vzdy dostat do nepolohy
      if ((((Integer(Self.Zaver) > 0) or (Self.vyhZaver)) and (Self.Zaver <> TJCType.staveni)) or
          ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen) and (Self.VyhSettings.zamekPoloha <> minus))) then
       begin
        for i := 0 to Self.OblsRizeni.Cnt-1 do
          Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Ztráta dohledu na výhybce : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
        JCDb.RusJC(Self);
       end;//if Blokovani
      Self.VyhStav.poloha := minus;
     end;

    Self.VyhStav.poloha_real := minus;
   end;

  //2 polohy zaroven = deje se neco divneho
  if ((iplus = 1) and (iminus = 1)) then
   begin
    Self.VyhStav.poloha := both;

    if (((((Integer(Self.Zaver) > 0) or (Self.vyhZaver) or ((Self.redukce_menu) and (not Self.VyhStav.staveni_plus) and (not Self.VyhStav.staveni_minus)))
      and (Self.Zaver <> TJCType.staveni)) or ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen)))
         and (Self.VyhStav.polohaOld <> both)) then
     begin
      for i := 0 to Self.OblsRizeni.Cnt-1 do
        Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Není koncová poloha : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.VyhStav.poloha_real := both;
   end;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.UpdateStaveniTimeout();
begin
 if ((not Self.StaveniPlus) and (not Self.StaveniMinus)) then Exit();

 // timeout
 if (Now > Self.VyhStav.staveniStart+EncodeTime(0, 0, _VYH_STAVENI_TIMEOUT_SEC, 0)) then
  begin
   Self.StaveniPlus  := false;
   Self.StaveniMinus := false;
   if (Assigned(Self.VyhStav.staveniErrCallback)) then
    begin
     Self.VyhStav.staveniErrCallback(Self);
     Self.VyhStav.staveniErrCallback := nil;
    end;
   Self.VyhStav.staveniOKCallback  := nil;
   Self.Change();
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.SetPoloha(new:TVyhPoloha; zamek:boolean = false; nouz:boolean = false; callback_ok:TNotifyEvent = nil; callback_err:TNotifyEvent = nil):Integer;
var Blk:TBlk;
    i:Integer;
begin
  if (not GetFunctions.GetSystemStart) then Exit(1);
  if (Self.VyhSettings.MTBAddrs.Count < 4) then Exit(2);
  if ((new <> plus) and (new <> minus)) then Exit(3);

  if (new <> Self.VyhStav.poloha) then
   begin
    // vstupni podminky se kontroluji jen pro pripad, kdy chceme vyhybku opravdu prestavit
    // zamknout ji muzeme kdykoliv

    // pokud se nerovna moje poloha, nerovna se i poloha spojky -> obsazenost na spojce apod. je problem
    Blky.GetBlkByID(Self.VyhSettings.spojka, Blk);
    if ((Integer(Self.Zaver) > 0) and (Self.Zaver <> TJCType.staveni) or (Self.vyhZaver) or
        ((Blk <> nil) and ((Integer((Blk as TBlkVyhybka).Zaver) > 0) and ((Blk as TBlkVyhybka).Zaver <> TJCType.staveni) or ((Blk as TBlkVyhybka).vyhZaver)))) then
     begin
      for i := 0 to Self.OblsRizeni.Cnt-1 do
        Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Nelze pøestavit '+Self.GlobalSettings.name+' - pod závìrem', 'TECHNOLOGIE');
      if (Assigned(callback_err)) then callback_err(self);
      Exit(4);
     end;
    if (((Self.Obsazeno = TUsekStav.obsazeno) or ((Blk <> nil) and ((Blk as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno))) and (not nouz)) then
     begin
      for i := 0 to Self.OblsRizeni.Cnt-1 do
        Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Nelze pøestavit '+Self.GlobalSettings.name+' - obsazeno', 'TECHNOLOGIE');
      if (Assigned(callback_err)) then callback_err(self);
      Exit(5);
     end;
   end else begin
    // pokud polohu uz mame, zavolame ok callback
    if (Assigned(callback_ok)) then callback_ok(Self);
   end;

 //MTBAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-

 if (new = plus) then
  begin
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[2].board,Self.VyhSettings.MTBAddrs.data[2].port, 1);
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[3].board,Self.VyhSettings.MTBAddrs.data[3].port, 0);

   if (Self.VyhStav.poloha <> plus) then Self.VyhStav.staveni_plus := true;
   Self.VyhStav.staveni_minus := false;

   if (Self.VyhStav.Poloha = minus) then Self.VyhStav.poloha := none;
  end;

 if (new = minus) then
  begin
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[2].board, Self.VyhSettings.MTBAddrs.data[2].port, 0);
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[3].board, Self.VyhSettings.MTBAddrs.data[3].port, 1);

   Self.VyhStav.staveni_plus  := false;
   if (Self.VyhStav.poloha <> minus) then Self.VyhStav.staveni_minus := true;

   if (Self.VyhStav.Poloha = plus) then Self.VyhStav.poloha := none;
  end;

 Self.VyhStav.staveniErrCallback := callback_Err;
 Self.VyhStav.staveniOKCallback  := callback_OK;
 Self.VyhStav.staveniStart       := Now;

 if (not zamek) then
  begin
   Self.NullOutput.enabled := true;
   Self.NullOutput.NullOutputTime := Now+EncodeTime(0, 0, 0, 500);
  end else begin
   Self.VyhStav.locked := true;
  end;

 if (Self.VyhSettings.spojka > -1) then
  begin
   Blky.GetBlkByID(Self.VyhSettings.spojka, Blk);
   if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_VYH) and
   (((Blk as TBlkVyhybka).Stav.staveni_plus <> Self.VyhStav.staveni_plus) or ((Blk as TBlkVyhybka).Stav.staveni_minus <> Self.VyhStav.staveni_minus))) then
     (Blk as TBlkVyhybka).SetPoloha(new, zamek, nouz);
  end;

 Result := 0;
 Self.Change();
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Unlock();
var spojka:TBlk;
begin
 Blky.GetBlkByID(Self.VyhSettings.spojka, spojka);
 if ((spojka = nil) or ((((spojka as TBlkVyhybka).Zaver = TJCType.no) or ((spojka as TBlkVyhybka).Zaver = TJCType.staveni)) and (not (spojka as TBlkVyhybka).vyhZaver) and (not (spojka as TBlkVyhybka).Stav.locked))) then
  begin
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[2].board, Self.VyhSettings.MTBAddrs.data[2].port, 0);
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[3].board, Self.VyhSettings.MTBAddrs.data[3].port, 0);
  end;

 Self.VyhStav.locked := false;

 Self.Change();
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.CheckNullOutput();
begin
 if (not Self.NullOutput.enabled) then Exit;

 if (Now >= Self.NullOutput.NullOutputTime) then
  begin
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[2].board,Self.VyhSettings.MTBAddrs.data[2].port,0);
   MTB.SetOutput(Self.VyhSettings.MTBAddrs.data[3].board,Self.VyhSettings.MTBAddrs.data[3].port,0);
   Self.NullOutput.enabled := false;
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkVyhybka.MenuPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.VyhStav.staveniPanel := SenderPnl;
 Self.VyhStav.staveniOR    := SenderOR;

 Self.SetPoloha(TVyhPoloha.plus, false, false, nil, Self.PanelStaveniErr);
end;

procedure TBlkVyhybka.MenuMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.VyhStav.staveniPanel := SenderPnl;
 Self.VyhStav.staveniOR    := SenderOR;

 Self.SetPoloha(TVyhPoloha.minus, false, false, nil, Self.PanelStaveniErr);
end;

procedure TBlkVyhybka.MenuNSPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 Self.VyhStav.staveniPanel := SenderPnl;
 Self.VyhStav.staveniOR    := SenderOR;

 Blky.GetBlkByID(Self.UsekID, Blk);
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvNSPlus, (SenderOR as TOR), 'Nouzové stavìní do polohy plus',
                    TBlky.GetBlksList(Self), GetPSPodminky(GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkVyhybka.MenuNSMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 Self.VyhStav.staveniPanel := SenderPnl;
 Self.VyhStav.staveniOR    := SenderOR;

 Blky.GetBlkByID(Self.UsekID, Blk);
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvNSMinus, (SenderOR as TOR), 'Nouzové stavìní do polohy mínus',
                    TBlky.GetBlksList(Self), GetPSPodminky(GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkVyhybka.MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.stit);
end;

procedure TBlkVyhybka.MenuVylClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Vyluka(SenderPnl, Self, Self.Stav.vyl);
end;

procedure TBlkVyhybka.MenuZAVEnableClick(SenderPnl:TIdContext; SenderOR:TObject);
var Blk:TBlk;
begin
 Self.vyhZaver := true;

 if (Self.VyhSettings.spojka > -1) then
  begin
   Blky.GetBlkByID(Self.VyhSettings.spojka, Blk);
   if ((Assigned(Blk)) and (Blk.GetGlobalSettings().typ = _BLK_VYH)) then
    (Blk as TBlkVyhybka).vyhZaver := true;
  end;
end;//procedure

procedure TBlkVyhybka.MenuZAVDisableClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TOR), 'Zrušení nouzového závìru', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkVyhybka.PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
begin
 if (success) then
   Self.NullVyhZaver();
end;//procedure

procedure TBlkVyhybka.MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.VyhStav.redukce_menu := 0;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
procedure TBlkVyhybka.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
var menu:string;
    spojka:TBlk;
begin
 menu := '$'+Self.GlobalSettings.name+',-,';

 Blky.GetBlkByID(Self.VyhSettings.spojka, spojka);

 if ((Self.Zaver = TJCType.no) and (not Self.vyhZaver) and (not Self.redukce_menu) and
    ((Self.zamek = nil) or ((Self.zamek as TBlkZamek).klicUvolnen)) and
  ((spojka = nil) or (((spojka as TBlkVyhybka).Zaver = TJCType.no) and (not (spojka as TBlkVyhybka).vyhZaver)))) then
  begin
   // na vyhybce neni zaver a menu neni redukovane

   if ((Self.Obsazeno = TUsekStav.obsazeno) or ((spojka <> nil) and ((spojka as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno))) then
    begin
     if (Self.VyhStav.poloha = plus) then menu := menu + '!NS-,';
     if (Self.VyhStav.poloha = minus) then menu := menu + '!NS+,';
     if ((Self.VyhStav.poloha = both) or (Self.VyhStav.poloha = none)) then
      menu := menu + '!NS+,!NS-,-,';
    end else begin
     if (Self.VyhStav.poloha = plus) then menu := menu + 'S-,';
     if (Self.VyhStav.poloha = minus) then menu := menu + 'S+,';
     if ((Self.VyhStav.poloha = both) or (Self.VyhStav.poloha = none)) then
      menu := menu + 'S+,S-,-,';
    end;
  end;

 menu := menu + 'STIT,VYL,';

 if (Self.vyhZaver) then
  menu := menu + '!ZAV<,'
 else
  if ((Self.Poloha = TVyhPoloha.plus) or (Self.Poloha = TVyhPoloha.minus)) then
    menu := menu + 'ZAV>,';

 if (rights >= TORControlRights.superuser) then
  begin
   menu := menu + '-,';
   if (Self.redukce_menu) then menu := menu + '*ZRUŠ REDUKCI,';
  end;

 ORTCPServer.Menu(SenderPnl, Self, SenderOR as TOR, menu);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights);
begin
 if (Self.Stav.poloha <= TVyhPoloha.disabled) then Exit();

 case (Button) of
  right, left, F2: Self.ShowPanelMenu(SenderPnl, SenderOR, rights);
 end;//case
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkVyhybka.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if (Self.Stav.poloha <= TVyhPoloha.disabled) then Exit();

 if (item = 'S+')   then Self.MenuPlusClick(SenderPnl, SenderOR);
 if (item = 'S-')   then Self.MenuMinusClick(SenderPnl, SenderOR);
 if (item = 'NS+')  then Self.MenuNSPlusClick(SenderPnl, SenderOR);
 if (item = 'NS-')  then Self.MenuNSMinusClick(SenderPnl, SenderOR);
 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR);
 if (item = 'VYL')  then Self.MenuVylClick(SenderPnl, SenderOR);
 if (item = 'ZAV>') then Self.MenuZAVEnableClick(SenderPnl, SenderOR);
 if (item = 'ZAV<') then Self.MenuZAVDisableClick(SenderPnl, SenderOR);
 if (item = 'ZRUŠ REDUKCI') then Self.MenuAdminREDUKClick(SenderPnl, SenderOR);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.PanelPotvrSekvNSPlus(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.SetPoloha(plus, false, true, nil, Self.PanelStaveniErr);
end;//procedure

procedure TBlkVyhybka.PanelPotvrSekvNSMinus(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.SetPoloha(minus, false, true, nil, Self.PanelStaveniErr);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Change(now:boolean = false);
var changed:boolean;
begin
 changed := false;

 if (not Self.VyhStav.locked) and ((Self.VyhStav.redukce_menu > 0) or (Self.Zaver <> TJCType.no) or (Self.vyhZaver) or
  ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen))) then
  begin
   // pokud je vyhybka redukovana, nebo je na ni zaver a je v koncove poloze a neni zamkla, zamkneme ji
   if (Self.VyhStav.poloha = TVyhPoloha.plus) then begin
    Self.SetPoloha(TVyhPoloha.plus, true);
    changed := true;
   end;
   if (Self.VyhStav.poloha = TVyhPoloha.minus) then begin
    Self.SetPoloha(TVyhPoloha.minus, true);
    changed := true;
   end;
  end;

  // kontrola spojky:
  //  pokud je na me vyhybce zaver, redukuji menu vyhybky ve spojce
  if (Self.VyhSettings.spojka > -1) then
  begin
   // pokud je na vyhybce spojka
   if ((Self.Zaver <> TJCType.no) or (Self.vyhZaver)) then
    begin
     // zaver
     if (not Self.IsReduction(Self.VyhStav.redukuji, Self.VyhSettings.spojka)) then
      Self.AddReduction(Self.VyhStav.redukuji, Self.VyhSettings.spojka);
    end else begin
     // zaver neni
     if (Self.IsReduction(Self.VyhStav.redukuji, Self.VyhSettings.spojka)) then
      Self.RemoveReduction(Self.VyhStav.redukuji, Self.VyhSettings.spojka);
    end;
  end;

 if ((Self.Zaver = TJCType.no) and (not Self.vyhZaver) and (Self.VyhStav.locked) and (Self.VyhStav.redukce_menu = 0) and
   ((Self.zamek = nil) or ((Self.zamek as TBlkZamek).klicUvolnen))) then
  begin
   Self.Unlock();
   changed := true;
  end;

 if (not changed) then inherited Change(now);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.RedukujMenu();
begin
 Self.VyhStav.redukce_menu := Self.VyhStav.redukce_menu + 1;

 // prave zacala redukce
 if (Self.VyhStav.redukce_menu = 1) then
  Self.Change();
end;//procedure

procedure TBlkVyhybka.ZrusRedukciMenu();
begin
 if (Self.VyhStav.redukce_menu > 0) then
  Self.VyhStav.redukce_menu := Self.VyhStav.redukce_menu - 1;

 // prave skoncila redukce
 if (Self.VyhStav.redukce_menu = 0) then
  Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetVyhZaver():boolean;
begin
 if (Self.Stav.vyhZaver > 0) then
   Result := true
 else
   Result := false;
end;//function

procedure TBlkVyhybka.SetVyhZaver(zaver:boolean);
begin
 if (zaver) then
  begin
   Inc(Self.VyhStav.vyhZaver);
   if (Self.VyhStav.vyhZaver = 1) then Self.Change();
  end else begin
   if (Self.Stav.vyhZaver > 0) then Dec(Self.VyhStav.vyhZaver);
   if (Self.Stav.vyhZaver = 0) then Self.Change();   
  end;
end;//procedure

procedure TBlkVyhybka.NullVyhZaver();
begin
 Self.VyhStav.vyhZaver := 0;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tato metoda je volana, pokud dojde k timeoutu pri staveni vyhybky z paneli
procedure TBlkVyhybka.PanelStaveniErr(Sender:TObject);
begin
  if ((Assigned(Self.VyhStav.staveniPanel)) and (Assigned(Self.VyhStav.staveniOR))) then
   begin
    ORTCPServer.BottomError(Self.VyhStav.staveniPanel, 'Nepøestavena '+Self.GlobalSettings.name, (Self.VyhStav.staveniOR as TOR).ShortName, 'TECHNOLOGIE');
    Self.VyhStav.staveniPanel := nil;
    Self.VyhStav.staveniOR    := nil;
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetZamek():TBlk;
begin
 if (((Self.fzamek = nil) and (Self.VyhSettings.zamek <> -1)) or ((Self.fzamek <> nil) and (Self.fzamek.GetGlobalSettings.id <> Self.VyhSettings.zamek))) then
   Blky.GetBlkByID(Self.VyhSettings.zamek, Self.fzamek);
 Result := Self.fzamek;
end;//function

////////////////////////////////////////////////////////////////////////////////

// pokud je na vyhybku zamek, vyhybka ma nespravnou polohu a klic je v zamku, vyhlasime poruchu zamku
procedure TBlkVyhybka.UpdateZamek();
begin
 if ((Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen) and (not (Self.zamek as TBlkZamek).nouzZaver) and
  (Self.Poloha <> Self.VyhSettings.zamekPoloha) and (not (Self.zamek as TBlkZamek).porucha)) then
   (Self.zamek as TBlkZamek).porucha := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit

