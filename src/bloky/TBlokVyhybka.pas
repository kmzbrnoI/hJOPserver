unit TBlokVyhybka;

{
 Definice a obsluha technologickeho bloku vyhybka.
}

interface

uses IniFiles, TBlok, SysUtils, TBlokUsek, Menus, TOblsRizeni,
     Classes, IdContext, Generics.Collections, JsonDataObjects, RCS,
     TOblRizeni, TechnologieRCS;

{
 Jak funguje interntionalLock:
 Tohle se uplatnuje jen u vyhybek odvratu. Tyto vyhybky nemusi byt v usecich JC,
 takze nemaji zadnou informaci o tom, kdy maji byt drzeny a kdy se ma drzeni
 zrusit. Jedna vyhybka muze byt v odvratech vice postavenych JC, proto se pocita,
 kolikrat je vyhybka drzena (VyhStav.intentionalLocks), jakmile je hodnota 0,
 dojde k odblokovaniv vyhybky. Pri odblokovani pozor na to, ze muse byt nenulove
 intentionalLocks vyhybky ve spojce, v takovem pripade vyhybku neodblokovavat.
}

type
 TVyhPoloha = (disabled = -5, none = -1, plus = 0, minus = 1, both = 2);
 ESpojka = class(Exception);

 TBlkVyhSettings = record
  RCSAddrs:TRCSAddrs;     // vstup+, vstup-, vystup+, vystup-
  spojka:Integer;         // reference na id vyhybky ve spojce
                          // pokud jsou obe vyhybky ve spojce, maji reference na sebe navzajem
                          // zmena RCS vstupu a vystupu v nastaveni jedne vyhybky ovlivnuje druhou
                          // POZOR: jedna data ulozena na dvou mistech, pri nacitani se nekontroluje konzistence
                          // SOUBOR MUSI BYT KONZISTENTNI (tj. kazda vyhybka musi mit navaznosti na tu druhou)
  zamek:Integer;          // pokud ma vyhybka navaznost na zamek, je zde id bloku zamku; jinak -1
  zamekPoloha:TVyhPoloha; // poloha, v jake se vyhybka musi nachazet pro uzamceni zamku
  npPlus:Integer;         // id neprofiloveho useku pro polohu plus (-1 pokud neni)
  npMinus:Integer;        // id neprofiloveho useku pro polohu minus (-1 pokud neni)
  detekcePolohy:Boolean;
 end;

 TBlkVyhStav = record
  poloha,polohaOld,polohaReal,polohaSave:TVyhPoloha; // polohaReal je skutecna poloha, kterou aktualne zobrazuji RCS vstupy
  stit,vyl:string;                            // stitek a vyluka vyhybky
  staveni_minus,staveni_plus:Boolean;         // stavi se zrovna vyhybka do polohy plus, ci minus?
  outputLocked: boolean;                      // skutecny zamek na vystupu - jestli je RCS vystup zamknut
  intentionalLocks:Integer;
  vyhZaver:Cardinal;                          // pocet bloku, ktere na vyhybku udelily nouzovy zaver

  staveniErrCallback, staveniOKCallback:TNotifyEvent;     // callback eventy pro koncovou polohu vyhybky (resp. timeout prestavovani)
  staveniStart:TDateTime;                                 // cas zacatku prestavovani
  staveniPanel:TIDContext;                                // panel, ktery chtel vyhybku prestavit
  staveniOR:TObject;                                      // oblast rizeni, ktera vyzadala staveni vyhybky
 end;

 TBlkVyhybkaRel = record
  UsekID:Integer;
 end;

 TBlkVyhInputs = record
  plus: TRCSInputState;
  minus: TRCSInputState;
  constructor Create(plus, minus: TRCSInputState);
 end;

 TBlkVyhybka = class(TBlk)
  const
   //defaultni stav
   _def_vyh_stav:TBlkVyhStav = (
    poloha : disabled;
    polohaOld : disabled;
    polohaReal : disabled;
    stit : '';
    vyl : '';
    staveni_minus : false;
    staveni_plus : false;
    outputLocked : false;
    intentionalLocks: 0;
    vyhZaver: 0;
    staveniErrCallback: nil;
    staveniOKCallback: nil;
    staveniStart: 0;
    staveniPanel: nil;
    staveniOR: nil;
   );

   _VYH_STAVENI_TIMEOUT_SEC = 10; // timeout na staveni vyhybky je 10 sekund
   _VYH_STAVENI_MOCK_SEC = 2;

  private
   VyhSettings:TBlkVyhSettings;
   VyhStav:TBlkVyhStav;
   VyhRel:TBlkVyhybkaRel;

   NullOutput:record
     enabled:boolean;
     NullOutputTime:System.TDateTime; // 500ms to null outputs
   end;

   fzamek:TBlk;
   fparent:TBlk;
   fnpPlus:TBlk;
   fnpMinus:TBlk;

    function GetZaver():TZaver;
    function GetNUZ():boolean;
    function GetObsazeno():TUsekStav;

    procedure SetVyhStit(stit:string);
    procedure mSetVyhVyl(vyl:string);

    function GetIntentionalLock():boolean;

    procedure UpdatePoloha();
    procedure UpdateStaveniTimeout();
    procedure UpdateZamek();
    procedure Unlock();
    function ZamekLocked():Boolean;

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

    procedure UPOPlusClick(Sender:TObject);
    procedure UPOMinusClick(Sender:TObject);
    procedure UPONSPlusClick(Sender:TObject);
    procedure UPONSMinusClick(Sender:TObject);

    procedure MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminPolPlusCLick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminPolMinusCLick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminNepolCLick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure PanelPotvrSekvNSPlus(Sender:TIdContext; success:boolean);
    procedure PanelPotvrSekvNSMinus(Sender:TIdContext; success:boolean);
    procedure PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);

    procedure ORVylukaNull(Sender:TIdContext; success:boolean);

    function GetVyhZaver():boolean;
    procedure SetVyhZaver(zaver:boolean);
    function MockInputs():TBlkVyhInputs;

    function GetZamek():TBlk;
    function GetNpPlus():TBlk;
    function GetNpMinus():TBlk;
    function GetDetekcePolohy():Boolean;
    function GetSpojka():TBlkVyhybka;
    function ShouldBeLocked(withZamek: boolean = true):boolean;
    function MeOrSpojkaZaverStaveni():boolean;

    procedure NpObsazChange(Sender:TObject; data:Integer);
    procedure MapNpEvents();

    procedure StitVylUPO(SenderPnl:TIdContext; SenderOR:TObject;
        UPO_OKCallback: TNotifyEvent; UPO_EscCallback:TNotifyEvent);

    class function CombineSpojkaInputs(first: TRCSInputState; second: TRCSInputState):TRCSInputState;

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
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    //----- vyhybka own functions -----

    function GetSettings():TBlkVyhSettings;
    procedure SetSettings(data:TBlkVyhSettings);

    function SetPoloha(new:TVyhPoloha; zamek:boolean = false; nouz:boolean = false; callback_ok:TNotifyEvent = nil; callback_err:TNotifyEvent = nil):Integer;
    procedure SetVyhVyl(Sender:TIDContext; vyl:string);
    procedure SetSpojkaNoPropag(spojka:Integer);

    procedure IntentionalLock();
    procedure IntentionalUnlock();

    procedure NullVyhZaver();
    procedure DecreaseNouzZaver(amount:Cardinal);
    function GetInputs():TBlkVyhInputs;

    property Stav:TBlkVyhStav read VyhStav;

    property Poloha:TVyhPoloha read VyhStav.poloha;
    property NUZ:boolean read GetNUZ;
    property Zaver:TZaver read GetZaver;
    property Obsazeno:TUsekStav read GetObsazeno;
    property Stitek:string read VyhStav.Stit write SetVyhStit;
    property Vyluka:string read VyhStav.Vyl write mSetVyhVyl;
    property intentionalLocked:boolean read GetIntentionalLock;
    property UsekID:Integer read VyhRel.UsekID;
    property vyhZaver:boolean read GetVyhZaver write SetVyhZaver;
    property zamek:TBlk read GetZamek;
    property npBlokPlus:TBlk read GetNpPlus;
    property npBlokMinus:TBlk read GetNpMinus;
    property detekcePolohy:Boolean read GetDetekcePolohy;
    property outputLocked:Boolean read VyhStav.outputLocked;
    property spojka: TBlkVyhybka read GetSpojka;

    property StaveniPlus:Boolean read VyhStav.staveni_plus write VyhStav.staveni_plus;
    property StaveniMinus:Boolean read VyhStav.staveni_minus write VyhStav.staveni_minus;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    //PT:

    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;
    procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject); override;

    class function PolohaToStr(poloha:TVyhPoloha):string;
    class function StrToPoloha(c: string):TVyhPoloha;

 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, GetSystems, fMain, TJCDatabase, UPO, Graphics, Diagnostics,
      TCPServerOR, TBlokZamek, PTUtils, changeEvent, TCPORsRef, Prevody;

constructor TBlkVyhybka.Create(index:Integer);
begin
 inherited Create(index);
 Self.GlobalSettings.typ := _BLK_VYH;
 Self.VyhStav := Self._def_vyh_stav;
 Self.fzamek  := nil;
 Self.fparent := nil;
 Self.fnpPlus := nil;
 Self.fnpMinus := nil;
end;//ctor

destructor TBlkVyhybka.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.VyhSettings.RCSAddrs := Self.LoadRCS(ini_tech,section);
 Self.VyhSettings.spojka := ini_tech.ReadInteger(section, 'spojka', -1);
 Self.VyhSettings.zamek := ini_tech.ReadInteger(section, 'zamek', -1);
 Self.VyhSettings.zamekPoloha := TVyhPoloha(ini_tech.ReadInteger(section, 'zamek-pol', 0));
 Self.VyhSettings.detekcePolohy := ini_tech.ReadBool(section, 'detekcePolohy', true);

 Self.VyhSettings.npPlus := ini_tech.ReadInteger(section, 'npPlus', -1);
 Self.VyhSettings.npMinus := ini_tech.ReadInteger(section, 'npMinus', -1);

 Self.VyhStav.Stit := ini_stat.ReadString(section, 'stit', '');
 Self.VyhStav.Vyl  := ini_stat.ReadString(section, 'vyl', '');

 Self.VyhStav.polohaSave := Self.StrToPoloha(ini_stat.ReadString(section, 'poloha', '+'));
 if ((Self.VyhStav.polohaSave <> TVyhPoloha.plus) and (Self.VyhStav.polohaSave <> TVyhPoloha.minus)) then
   Self.VyhStav.polohaSave := TVyhPoloha.plus;

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();
   try
     ExtractStrings([';'],[],PChar(ini_rel.ReadString('V',IntToStr(Self.GlobalSettings.id),'')),str);
     if (str.Count < 2) then Exit;

     if (Self.ORsRef <> nil) then
       Self.ORsRef.Free();
     Self.ORsRef := ORs.ParseORs(str[0]);
     Self.VyhRel.UsekID := StrToInt(str[1]);
   finally
     str.Free();
   end;
  end else begin
   Self.ORsRef.Clear();
   Self.VyhRel.UsekID := -1;
  end;

 PushRCStoOR(Self.ORsRef, Self.VyhSettings.RCSAddrs);
end;

procedure TBlkVyhybka.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech, section);

 Self.SaveRCS(ini_tech, section, Self.VyhSettings.RCSAddrs);

 if (Self.VyhSettings.spojka > -1) then
   ini_tech.WriteInteger(section, 'spojka', Self.VyhSettings.spojka);

 if (Self.VyhSettings.npPlus > -1) then
   ini_tech.WriteInteger(section, 'npPlus', Self.VyhSettings.npPlus);

 if (Self.VyhSettings.npMinus > -1) then
   ini_tech.WriteInteger(section, 'npMinus', Self.VyhSettings.npMinus);

 if (Self.VyhSettings.zamek > -1) then
  begin
   ini_tech.WriteInteger(section, 'zamek', Self.VyhSettings.zamek);
   ini_tech.WriteInteger(section, 'zamek-pol', Integer(Self.VyhSettings.zamekPoloha));
  end;

 if (not Self.detekcePolohy) then
   ini_tech.WriteBool(section, 'detekcePolohy', false);
end;

procedure TBlkVyhybka.SaveStatus(ini_stat:TMemIniFile;const section:string);
var poloha: TVyhPoloha;
begin
 if (Self.VyhStav.stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.VyhStav.Stit);

 if (Self.VyhStav.vyl <> '') then
   ini_stat.WriteString(section, 'vyl', Self.VyhStav.Vyl);

 if (not Self.detekcePolohy) then
  begin
   if (Self.Poloha > TVyhPoloha.disabled) then
     poloha := Self.Poloha
   else
     poloha := Self.VyhStav.polohaSave;

   if ((Self.StaveniMinus) or (poloha = TVyhPoloha.minus)) then
     ini_stat.WriteString(section, 'poloha', '-')
   else
     ini_stat.WriteString(section, 'poloha', '+')
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Enable();
var rcsaddr:TRCSAddr;
    i:Integer;
begin
 if (Self.VyhSettings.RCSAddrs.Count < 4) then
   Exit();
 if (Self.detekcePolohy) then
  begin
   for rcsaddr in Self.VyhSettings.RCSAddrs do
     if (not RCSi.IsModule(rcsaddr.board)) then
       Exit();
  end else begin
   for i := 2 to Self.VyhSettings.RCSAddrs.Count-1 do
     if (not RCSi.IsModule(Self.VyhSettings.RCSAddrs[i].board)) then
       Exit();
  end;

 if (Self.detekcePolohy) then
   Self.VyhStav.poloha := none
 else
   Self.VyhStav.poloha := Self.VyhStav.polohaSave;

 Self.MapNpEvents();
 Self.Update(); //update will call Change()
end;

procedure TBlkVyhybka.Disable();
begin
 if (Self.StaveniPlus) then
   Self.VyhStav.polohaSave := TVyhPoloha.plus
 else if (Self.StaveniMinus) then
   Self.VyhStav.polohaSave := TVyhPoloha.minus
 else
   Self.VyhStav.polohaSave := Self.VyhStav.poloha;

 Self.VyhStav.poloha := disabled;
 Self.Change(true);
end;

procedure TBlkVyhybka.Reset();
begin
 Self.VyhStav.intentionalLocks := 0;
 Self.VyhStav.staveni_plus := false;
 Self.VyhStav.staveni_minus := false;
 Self.VyhStav.outputLocked := false;
 Self.VyhStav.vyhZaver := 0;
end;

function TBlkVyhybka.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 if ((portType = TRCSIOType.input) and (Self.VyhSettings.RCSAddrs.Count >= 2) and
     ((Self.VyhSettings.RCSAddrs[0] = addr) or (Self.VyhSettings.RCSAddrs[1] = addr))) then
   Exit(True);

 if ((portType = TRCSIOType.output) and (Self.VyhSettings.RCSAddrs.Count >= 4) and
     ((Self.VyhSettings.RCSAddrs[2] = addr) or (Self.VyhSettings.RCSAddrs[3] = addr))) then
   Exit(True);

 Result := False;
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetZaver():TZaver;
begin
 if (Self.VyhRel.UsekID = -1) then
   Exit(TZaver.no);

 if (((Self.fparent = nil) and (Self.VyhRel.UsekID <> -1)) or ((Self.fparent.id <> Self.VyhRel.UsekID))) then
   Blky.GetBlkByID(Self.VyhRel.UsekID, Self.fparent);
 if (Self.fparent <> nil) then
   Result := (Self.fparent as TBlkUsek).Zaver
 else
   Result := TZaver.no;
end;

function TBlkVyhybka.GetNUZ():boolean;
var tmpBlk:TBlk;
    return:Integer;
begin
 return := Blky.GetBlkByID(Self.VyhRel.UsekID,tmpBlk);
 if (return < 0) then Exit(false);
 if (tmpBlk.typ <> _BLK_USEK) then Exit(false);

 Result := (TBlkUsek(tmpBlk)).NUZ;
end;

function TBlkVyhybka.GetObsazeno():TUsekStav;
var tmpBlk:TBlk;
    return:Integer;
begin
 return := Blky.GetBlkByID(Self.VyhRel.UsekID,tmpBlk);
 if (return < 0) then Exit(TUsekStav.none);
 if ((tmpBlk.typ <> _BLK_USEK) and (tmpBlk.typ <> _BLK_TU)) then Exit(TUsekStav.none);

 Result := (tmpBlk as TBlkUsek).Obsazeno;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetSettings():TBlkVyhSettings;
begin
 Result := Self.VyhSettings;
end;

procedure TBlkVyhybka.SetSettings(data:TBlkVyhSettings);
var Blk:TBlk;
    spojka_settings:TBlkVyhSettings;
    spojka_old:Integer;
begin
 if (data.spojka = Self.GlobalSettings.id) then
   raise ESpojka.Create('Nelze mít spojku sám se sebou!');
 spojka_old := Self.VyhSettings.spojka;

 Self.VyhSettings.spojka := data.spojka;

 // kontrola navaznosti spojky
 if (data.spojka > -1) then
  begin
   // zkontrolujeme, pokud spojka uz neexistovala a pokud ano, tak ji smazeme
   if (spojka_old > -1) then
    begin
     Blky.GetBlkByID(spojka_old, Blk);
     if ((Blk <> nil) and (Blk.typ = _BLK_VYH)) then
       (Blk as TBlkVyhybka).SetSpojkaNoPropag(-1);
    end;

   // pridame spojku do druhe vyhybky
   Blky.GetBlkByID(data.spojka, Blk);
   if ((Blk = nil) or (Blk.typ <> _BLK_VYH)) then
    begin
     Self.VyhSettings.spojka := -1;
    end else begin
     spojka_settings := (Blk as TBlkVyhybka).GetSettings();
     if (spojka_settings.spojka <> Self.GlobalSettings.id) then
      begin
       if (spojka_settings.spojka <> -1) then
        begin
         Self.VyhSettings.spojka := -1;
         raise ESpojka.Create('Na výhybce je již jiná spojka!');
        end;

       spojka_settings.spojka := self.GlobalSettings.id;
       (Blk as TBlkVyhybka).SetSettings(spojka_settings);
      end;
    end;
  end else begin
   // odebereme spojku z druhe vyhybky
   if (spojka_old <> -1) then
    begin
     Blky.GetBlkByID(spojka_old, Blk);
     if ((Blk <> nil) and (Blk.typ = _BLK_VYH)) then
       (Blk as TBlkVyhybka).SetSpojkaNoPropag(-1);
    end;
  end;

 if (data.RCSAddrs <> Self.VyhSettings.RCSAddrs) then
   Self.VyhSettings.RCSAddrs.Free();
 Self.VyhSettings := data;

 Self.MapNpEvents();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.SetVyhStit(stit:string);
begin
 Self.VyhStav.stit := Stit;
 Self.Change();
end;

procedure TBlkVyhybka.mSetVyhVyl(vyl:string);
begin
 Self.VyhStav.vyl := vyl;
 Self.Change();
end;

procedure TBlkVyhybka.ORVylukaNull(Sender:TIdContext; success:boolean);
begin
 if (success) then
  Self.Vyluka := '';
end;

procedure TBlkVyhybka.SetVyhVyl(Sender:TIDCOntext; vyl:string);
begin
 if ((self.VyhStav.Vyl <> '') and (vyl = '')) then
  begin
   ORTCPServer.Potvr(Sender, Self.ORVylukaNull, Self.ORsRef[0], 'Zrušení výluky', TBlky.GetBlksList(Self), nil);
  end else begin
   Self.Vyluka := vyl;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetIntentionalLock():boolean;
begin
 Result := (Self.VyhStav.intentionalLocks > 0);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetInputs():TBlkVyhInputs;
begin
 if (Self.detekcePolohy) then
  begin
   Result.plus := RCSi.GetInput(Self.VyhSettings.RCSAddrs[0]);
   Result.minus := RCSi.GetInput(Self.VyhSettings.RCSAddrs[1]);
  end else begin
   Result := Self.MockInputs();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.UpdatePoloha();
var inp, spojkaInp: TBlkVyhInputs;
    oblr:TOR;
    spojka: TBlkVyhybka;
 begin
  if (Self.VyhSettings.RCSAddrs.Count < 4) then Exit();

  Blky.GetBlkByID(Self.VyhSettings.spojka, TBlk(spojka));
  if ((spojka <> nil) and (spojka.typ <> _BLK_VYH)) then
    Exit();

  //RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-
  try
    inp := Self.GetInputs();
  except
    inp.plus := TRCSInputState.failure;
    inp.minus := TRCSInputState.failure;
  end;

  if ((spojka <> nil) and (inp.plus <> TRCSInputState.failure) and (inp.minus <> TRCSInputState.failure)) then
   begin
    try
      spojkaInp := spojka.GetInputs();
    except
      spojkaInp.plus := TRCSInputState.failure;
      spojkaInp.minus := TRCSInputState.failure;
    end;

    inp.plus := CombineSpojkaInputs(inp.plus, spojkaInp.plus);
    inp.minus := CombineSpojkaInputs(inp.minus, spojkaInp.minus);
   end;

  try
    if ((inp.plus = failure) or (inp.minus = failure) or
        (not RCSi.IsModule(Self.VyhSettings.RCSAddrs[2].board)) or
        (not RCSi.IsModule(Self.VyhSettings.RCSAddrs[3].board)) or
        ((spojka <> nil) and ((not RCSi.IsModule(spojka.VyhSettings.RCSAddrs[2].board)) or
                              (not RCSi.IsModule(spojka.VyhSettings.RCSAddrs[2].board))))) then
     begin
      if (Self.Stav.poloha <> TVyhPoloha.disabled) then
       begin
        Self.VyhStav.poloha := TVyhPoloha.disabled;
        JCDb.RusJC(Self);
       end;
      Exit();
     end;
  except
    if (Self.Stav.poloha <> TVyhPoloha.disabled) then
     begin
      Self.VyhStav.poloha := TVyhPoloha.disabled;
      JCDb.RusJC(Self);
     end;
    Exit();
  end;


  if ((inp.plus = isOff) and (inp.minus = isOff)) then
   begin
    Self.VyhStav.poloha := none;

    if ((Self.VyhStav.poloha <> Self.VyhStav.polohaReal) and ((Self.Zaver > TZaver.no) or (Self.vyhZaver) or
      ((Self.intentionalLocked) and (not Self.VyhStav.staveni_plus) and (not Self.VyhStav.staveni_minus)) or
      (Self.ZamekLocked()))
     and (Self.Zaver <> TZaver.staveni)) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Není koncová poloha '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.VyhStav.polohaReal := none;
   end;

  if ((inp.plus = isOn) and (inp.minus = isOff)) then
   begin
    //je-li plus vstup 1
    Self.VyhStav.polohaReal := plus;
    if (Self.VyhStav.staveni_minus) then Exit();

    if (Self.VyhStav.staveni_plus) then
     begin
      Self.VyhStav.poloha := plus;
      Self.VyhStav.staveni_plus := false;

      // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
      if (Self.spojka <> nil) then
        Self.spojka.Update();

      if (Assigned(Self.VyhStav.staveniOKCallback)) then
       begin
        Self.VyhStav.staveniOKCallback(Self);
        Self.VyhStav.staveniOKCallback := nil;
       end;
      Self.VyhStav.staveniErrCallback := nil;
     end else begin
      if (Self.VyhStav.poloha <> Self.VyhStav.polohaReal) then
       begin
        // sem se dostaneme, pokud se vyhybka poprve nalezne neocekavane v poloze +
        Self.VyhStav.poloha := plus;

        if ((Self.ShouldBeLocked(false)) or (Self.ZamekLocked() and (Self.VyhSettings.zamekPoloha <> plus))) then
         begin
          for oblr in Self.OblsRizeni do
            oblr.BlkWriteError(Self, 'Ztráta dohledu na výhybce '+Self.GlobalSettings.name, 'TECHNOLOGIE');
          JCDb.RusJC(Self);
         end;
       end;
     end;
   end;

  if ((inp.minus = isOn) and (inp.plus = isOff)) then
   begin
    //je-li minus vstup 1
    Self.VyhStav.polohaReal := minus;
    if (Self.VyhStav.staveni_plus) then Exit();

    if (Self.VyhStav.staveni_minus) then
     begin
      Self.VyhStav.poloha := minus;
      Self.VyhStav.staveni_minus := false;

      // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
      if (Self.spojka <> nil) then
        Self.spojka.Update();

      if (Assigned(Self.VyhStav.staveniOKCallback)) then
       begin
        Self.VyhStav.staveniOKCallback(Self);
        Self.VyhStav.staveniOKCallback := nil;
       end;
      Self.VyhStav.staveniErrCallback := nil;
     end else begin
      if (Self.VyhStav.poloha <> Self.VyhStav.polohaReal) then
       begin
        //sem se dostaneme, pokud se vyhybka nalezne neocekavane v poloze -
        Self.VyhStav.poloha := minus;

        if ((Self.ShouldBeLocked(false)) or (Self.ZamekLocked() and (Self.VyhSettings.zamekPoloha <> minus))) then
         begin
          for oblr in Self.OblsRizeni do
            oblr.BlkWriteError(Self, 'Ztráta dohledu na výhybce '+Self.GlobalSettings.name, 'TECHNOLOGIE');
          JCDb.RusJC(Self);
         end;
       end;
     end;
   end;

  //2 polohy zaroven = deje se neco divneho
  if ((inp.plus = isOn) and (inp.minus = isOn)) then
   begin
    Self.VyhStav.poloha := both;

    if ((((Self.ShouldBeLocked()) and (Self.Zaver <> TZaver.staveni)) or (Self.ZamekLocked()))
        and (Self.VyhStav.polohaOld <> both)) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Není koncová poloha '+Self.GlobalSettings.name, 'TECHNOLOGIE');
      JCDb.RusJC(Self);
     end;//if Blokovani

    Self.VyhStav.polohaReal := both;
   end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.UpdateStaveniTimeout();
var Blk:TBlk;
begin
 if ((not Self.StaveniPlus) and (not Self.StaveniMinus)) then Exit();

 // timeout
 if (Now > Self.VyhStav.staveniStart+EncodeTime(0, 0, _VYH_STAVENI_TIMEOUT_SEC, 0)) then
  begin
   Self.StaveniPlus  := false;
   Self.StaveniMinus := false;

   // aktualizujeme spojku, aby pri volani udalosti byla v konzistentnim stavu
   if (Self.VyhSettings.spojka > -1) then
    begin
     Blky.GetBlkByID(Self.VyhSettings.spojka, Blk);
     if (Blk <> nil) then Blk.Update();
    end;

   if (Assigned(Self.VyhStav.staveniErrCallback)) then
    begin
     Self.VyhStav.staveniErrCallback(Self);
     Self.VyhStav.staveniErrCallback := nil;
    end;
   Self.VyhStav.staveniOKCallback  := nil;
   Self.Change();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.SetPoloha(new:TVyhPoloha; zamek:boolean = false; nouz:boolean = false; callback_ok:TNotifyEvent = nil; callback_err:TNotifyEvent = nil):Integer;
var spojka:TBlkVyhybka;
    oblr:TOR;
begin
  if (Self.VyhSettings.RCSAddrs.Count < 4) then
   begin
    if (Assigned(callback_err)) then callback_err(self);
    Exit(2);
   end;
  if ((new <> plus) and (new <> minus)) then
   begin
    if (Assigned(callback_err)) then callback_err(self);
    Exit(3);
   end;

  // V tomto momente je klicove ziskat aktualni polohu vyhybky, jinak by mohlo dojit
  // k zacykleni pri staveni spojek.
  Self.UpdatePoloha();
  spojka := Self.spojka;

  if (new <> Self.VyhStav.poloha) then
   begin
    // vstupni podminky se kontroluji jen pro pripad, kdy chceme vyhybku opravdu prestavit
    // zamknout ji muzeme kdykoliv

    // pokud se nerovna moje poloha, nerovna se i poloha spojky -> obsazenost na spojce apod. je problem
    if ((Integer(Self.Zaver) > 0) and (Self.Zaver <> TZaver.staveni) or (Self.vyhZaver) or
        ((spojka <> nil) and ((Integer(spojka.Zaver) > 0) and (spojka.Zaver <> TZaver.staveni) or (spojka.vyhZaver)))) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Nelze přestavit '+Self.GlobalSettings.name+' - pod závěrem', 'TECHNOLOGIE');
      if (Assigned(callback_err)) then callback_err(self);
      Exit(4);
     end;
    if (((Self.Obsazeno = TUsekStav.obsazeno) or ((spojka <> nil) and (spojka.Obsazeno = TUsekStav.obsazeno))) and (not nouz)) then
     begin
      for oblr in Self.OblsRizeni do
        oblr.BlkWriteError(Self, 'Nelze přestavit '+Self.GlobalSettings.name+' - obsazeno', 'TECHNOLOGIE');
      if (Assigned(callback_err)) then callback_err(self);
      Exit(5);
     end;
   end else begin
    // pokud polohu uz mame, zavolame ok callback
    if (Assigned(callback_ok)) then callback_ok(Self);
   end;

 //RCSAddrs: poradi(0..3): vst+,vst-,vyst+,vyst-

 if (new = plus) then
  begin
   try
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[2].board,Self.VyhSettings.RCSAddrs[2].port, 1);
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[3].board,Self.VyhSettings.RCSAddrs[3].port, 0);
   except
     for oblr in Self.OblsRizeni do
       oblr.BlkWriteError(Self, 'Nelze přestavit '+Self.GlobalSettings.name+' - výjimka RCS SetOutput', 'TECHNOLOGIE');
     if (Assigned(callback_err)) then callback_err(self);
     Exit(6);
   end;

   if (Self.VyhStav.poloha <> plus) then
     Self.VyhStav.staveni_plus := true;
   Self.VyhStav.staveni_minus := false;

   if (Self.VyhStav.Poloha = minus) then
     Self.VyhStav.poloha := none;
  end;

 if (new = minus) then
  begin
   try
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[2].board, Self.VyhSettings.RCSAddrs[2].port, 0);
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[3].board, Self.VyhSettings.RCSAddrs[3].port, 1);
   except
     for oblr in Self.OblsRizeni do
       oblr.BlkWriteError(Self, 'Nelze přestavit '+Self.GlobalSettings.name+' - výjimka RCS SetOutput', 'TECHNOLOGIE');
     if (Assigned(callback_err)) then callback_err(self);
     Exit(6);
   end;

   Self.VyhStav.staveni_plus  := false;
   if (Self.VyhStav.poloha <> minus) then
     Self.VyhStav.staveni_minus := true;

   if (Self.VyhStav.Poloha = plus) then
     Self.VyhStav.poloha := none;
  end;

 Self.VyhStav.staveniErrCallback := callback_Err;
 Self.VyhStav.staveniOKCallback  := callback_OK;
 Self.VyhStav.staveniStart       := Now;

 if (not zamek) then
  begin
   Self.NullOutput.enabled := true;
   Self.NullOutput.NullOutputTime := Now+EncodeTime(0, 0, 0, 500);
  end else begin
   Self.VyhStav.outputLocked := true;
  end;

 if (spojka <> nil) then
  begin
   // pokud se jedna o spojku, volame SetPoloha i na spojku
   if ((spojka.Stav.staveni_plus <> Self.VyhStav.staveni_plus) or
       (spojka.Stav.staveni_minus <> Self.VyhStav.staveni_minus) or
       ((zamek) and (not spojka.outputLocked))) then
     spojka.SetPoloha(new, zamek, nouz);
  end;

 Result := 0;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Unlock();
var spojka:TBlkVyhybka;
begin
 try
   if ((Self.outputLocked) and (RCSi.Started)) then
    begin
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[2].board, Self.VyhSettings.RCSAddrs[2].port, 0);
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[3].board, Self.VyhSettings.RCSAddrs[3].port, 0);
    end;
 except

 end;

 Self.VyhStav.outputLocked := false;

 spojka := Self.spojka;
 if ((spojka <> nil) and (spojka.outputLocked)) then
   spojka.Unlock();

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.CheckNullOutput();
begin
 if (not Self.NullOutput.enabled) then Exit;

 if (Now >= Self.NullOutput.NullOutputTime) then
  begin
   try
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[2].board,Self.VyhSettings.RCSAddrs[2].port,0);
     RCSi.SetOutput(Self.VyhSettings.RCSAddrs[3].board,Self.VyhSettings.RCSAddrs[3].port,0);
   except

   end;

   Self.NullOutput.enabled := false;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkVyhybka.MenuPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.Stitek <> '') or (Self.Vyluka <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOPlusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOPlusClick(SenderPnl);
 end;
end;

procedure TBlkVyhybka.MenuMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.Stitek <> '') or (Self.Vyluka <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPOMinusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOMinusClick(SenderPnl);
 end;
end;

procedure TBlkVyhybka.MenuNSPlusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.Stitek <> '') or (Self.Vyluka <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSPlusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPONSPlusClick(SenderPnl);
 end;
end;

procedure TBlkVyhybka.MenuNSMinusClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.Stitek <> '') or (Self.Vyluka <> '')) then
   Self.StitVylUPO(SenderPnl, SenderOR, Self.UPONSMinusClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPONSMinusClick(SenderPnl);
 end;
end;

procedure TBlkVyhybka.UPOPlusClick(Sender:TObject);
begin
 Self.VyhStav.staveniPanel := TIdContext(Sender);
 Self.VyhStav.staveniOR    := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Self.SetPoloha(TVyhPoloha.plus, false, false, nil, Self.PanelStaveniErr);
end;

procedure TBlkVyhybka.UPOMinusClick(Sender:TObject);
begin
 Self.VyhStav.staveniPanel := TIdContext(Sender);
 Self.VyhStav.staveniOR    := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Self.SetPoloha(TVyhPoloha.minus, false, false, nil, Self.PanelStaveniErr);
end;

procedure TBlkVyhybka.UPONSPlusClick(Sender:TObject);
var Blk:TBlk;
begin
 Self.VyhStav.staveniPanel := TIdContext(Sender);
 Self.VyhStav.staveniOR    := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Blky.GetBlkByID(Self.UsekID, Blk);
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelPotvrSekvNSPlus, (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
                    'Nouzové stavění do polohy plus', TBlky.GetBlksList(Self),
                    TOR.GetPSPodminky(TOR.GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
end;

procedure TBlkVyhybka.UPONSMinusClick(Sender:TObject);
var Blk:TBlk;
begin
 Self.VyhStav.staveniPanel := TIdContext(Sender);
 Self.VyhStav.staveniOR    := TTCPORsRef(TIdContext(Sender).Data).UPO_ref;

 Blky.GetBlkByID(Self.UsekID, Blk);
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelPotvrSekvNSMinus, (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
                    'Nouzové stavění do polohy mínus', TBlky.GetBlksList(Self),
                    TOR.GetPSPodminky(TOR.GetPSPodminka(Blk, 'Obsazený kolejový úsek')));
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
   if ((Assigned(Blk)) and (Blk.typ = _BLK_VYH)) then
    (Blk as TBlkVyhybka).vyhZaver := true;
  end;
end;

procedure TBlkVyhybka.MenuZAVDisableClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TOR), 'Zrušení nouzového závěru', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkVyhybka.PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
begin
 if (success) then
   Self.NullVyhZaver();
end;

procedure TBlkVyhybka.MenuAdminREDUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.VyhStav.intentionalLocks := 0;
 Self.Change();
end;

procedure TBlkVyhybka.MenuAdminPolPlusCLick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[0], 1);
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[1], 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkVyhybka.MenuAdminPolMinusCLick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[0], 0);
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[1], 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkVyhybka.MenuAdminNepolCLick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[0], 0);
   RCSi.SetInput(Self.VyhSettings.RCSAddrs[1], 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkVyhybka.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var spojka:TBlkVyhybka;
begin
 Result := inherited;

 Blky.GetBlkByID(Self.VyhSettings.spojka, TBlk(spojka));

 if (not Self.ShouldBeLocked()) then
  begin
   // na vyhybce neni zaver a menu neni redukovane

   if ((Self.Obsazeno = TUsekStav.obsazeno) or ((spojka <> nil) and (spojka.Obsazeno = TUsekStav.obsazeno))) then
    begin
     if (Self.VyhStav.poloha = plus) then Result := Result + '!NS-,';
     if (Self.VyhStav.poloha = minus) then Result := Result + '!NS+,';
     if ((Self.VyhStav.poloha = both) or (Self.VyhStav.poloha = none)) then
      Result := Result + '!NS+,!NS-,-,';
    end else begin
     if (Self.VyhStav.poloha = plus) then Result := Result + 'S-,';
     if (Self.VyhStav.poloha = minus) then Result := Result + 'S+,';
     if ((Self.VyhStav.poloha = both) or (Self.VyhStav.poloha = none)) then
      Result := Result + 'S+,S-,-,';
    end;
  end;

 Result := Result + 'STIT,VYL,';

 if (Self.vyhZaver) then
  Result := Result + '!ZAV<,'
 else
  if ((Self.Poloha = TVyhPoloha.plus) or (Self.Poloha = TVyhPoloha.minus)) then
    Result := Result + 'ZAV>,';

 if (rights >= TORControlRights.superuser) then
  begin
   Result := Result + '-,';
   if (Self.intentionalLocked) then Result := Result + '*ZRUŠ REDUKCI,';
  end;

 if ((RCSi.simulation) and (Self.detekcePolohy)) then
  begin
   Result := Result + '-,';
   if (Self.Poloha <> TVyhPoloha.plus) then
     Result := Result + '*POL+,';
   if (Self.Poloha <> TVyhPoloha.minus) then
     Result := Result + '*POL-,';
   if (Self.Poloha <> TVyhPoloha.none) then
     Result := Result + '*NEPOL,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Self.Stav.poloha <= TVyhPoloha.disabled) then Exit();

 case (Button) of
  F1, F2, ENTER: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkVyhybka.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.Stav.poloha <= TVyhPoloha.disabled) then Exit();

 if (item = 'S+')        then Self.MenuPlusClick(SenderPnl, SenderOR)
 else if (item = 'S-')   then Self.MenuMinusClick(SenderPnl, SenderOR)
 else if (item = 'NS+')  then Self.MenuNSPlusClick(SenderPnl, SenderOR)
 else if (item = 'NS-')  then Self.MenuNSMinusClick(SenderPnl, SenderOR)
 else if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'VYL')  then Self.MenuVylClick(SenderPnl, SenderOR)
 else if (item = 'ZAV>') then Self.MenuZAVEnableClick(SenderPnl, SenderOR)
 else if (item = 'ZAV<') then Self.MenuZAVDisableClick(SenderPnl, SenderOR)
 else if (item = 'ZRUŠ REDUKCI') then Self.MenuAdminREDUKClick(SenderPnl, SenderOR)
 else if (item = 'POL+') then Self.MenuAdminPolPlusCLick(SenderPnl, SenderOR)
 else if (item = 'POL-') then Self.MenuAdminPolMinusCLick(SenderPnl, SenderOR)
 else if (item = 'NEPOL') then Self.MenuAdminNepolCLick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.PanelPotvrSekvNSPlus(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.SetPoloha(plus, false, true, nil, Self.PanelStaveniErr);
end;

procedure TBlkVyhybka.PanelPotvrSekvNSMinus(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.SetPoloha(minus, false, true, nil, Self.PanelStaveniErr);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.Change(now:boolean = false);
var changed:boolean;
begin
 changed := false;

 if (not Self.VyhStav.outputLocked) and (Self.ShouldBeLocked()) then
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

 if ((not Self.ShouldBeLocked()) and (Self.VyhStav.outputLocked)) then
  begin
   Self.Unlock();
   changed := true;
  end;

 if (not changed) then inherited Change(now);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.IntentionalLock();
begin
 Inc(Self.VyhStav.intentionalLocks);

 if (Self.VyhStav.intentionalLocks = 1) then
  Self.Change();
end;

procedure TBlkVyhybka.IntentionalUnlock();
begin
 if (Self.VyhStav.intentionalLocks > 0) then
  Dec(Self.VyhStav.intentionalLocks);

 if (Self.VyhStav.intentionalLocks = 0) then
  Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetVyhZaver():boolean;
begin
 Result := (Self.Stav.vyhZaver > 0);
end;

procedure TBlkVyhybka.SetVyhZaver(zaver:boolean);
begin
 if (zaver) then
  begin
   Inc(Self.VyhStav.vyhZaver);
   if (Self.VyhStav.vyhZaver = 1) then Self.Change();
  end else begin
   if (Self.Stav.vyhZaver > 0) then Dec(Self.VyhStav.vyhZaver);
   if (Self.Stav.vyhZaver = 0) then
    begin
     Self.Change();
     Blky.NouzZaverZrusen(Self);
    end;
  end;
end;

procedure TBlkVyhybka.NullVyhZaver();
begin
 Self.VyhStav.vyhZaver := 0;
 Blky.NouzZaverZrusen(Self);
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// tato metoda je volana, pokud dojde k timeoutu pri staveni vyhybky z paneli
procedure TBlkVyhybka.PanelStaveniErr(Sender:TObject);
begin
  if ((Assigned(Self.VyhStav.staveniPanel)) and (Assigned(Self.VyhStav.staveniOR))) then
   begin
    ORTCPServer.BottomError(Self.VyhStav.staveniPanel, 'Nepřestavena '+Self.GlobalSettings.name, (Self.VyhStav.staveniOR as TOR).ShortName, 'TECHNOLOGIE');
    Self.VyhStav.staveniPanel := nil;
    Self.VyhStav.staveniOR    := nil;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetZamek():TBlk;
begin
 if (((Self.fzamek = nil) and (Self.VyhSettings.zamek <> -1)) or ((Self.fzamek <> nil) and (Self.fzamek.id <> Self.VyhSettings.zamek))) then
   Blky.GetBlkByID(Self.VyhSettings.zamek, Self.fzamek);
 Result := Self.fzamek;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetNpPlus():TBlk;
begin
 if (((Self.fnpPlus = nil) and (Self.VyhSettings.npPlus <> -1)) or
     ((Self.fnpPlus <> nil) and (Self.fnpPlus.id <> Self.VyhSettings.npPlus))) then
   Blky.GetBlkByID(Self.VyhSettings.npPlus, Self.fnpPlus);
 Result := Self.fnpPlus;
end;

function TBlkVyhybka.GetNpMinus():TBlk;
begin
 if (((Self.fnpMinus = nil) and (Self.VyhSettings.npMinus <> -1)) or
     ((Self.fnpMinus <> nil) and (Self.fnpMinus.id <> Self.VyhSettings.npMinus))) then
   Blky.GetBlkByID(Self.VyhSettings.npMinus, Self.fnpMinus);
 Result := Self.fnpMinus;
end;

function TBlkVyhybka.GetDetekcePolohy():Boolean;
begin
 Result := (Self.VyhSettings.detekcePolohy) and (Self.VyhSettings.RCSAddrs.Count >= 2);
end;

////////////////////////////////////////////////////////////////////////////////

// pokud je na vyhybku zamek, vyhybka ma nespravnou polohu a klic je v zamku, vyhlasime poruchu zamku
procedure TBlkVyhybka.UpdateZamek();
begin
 if (Self.ZamekLocked() and (not (Self.zamek as TBlkZamek).nouzZaver) and
     (Self.Poloha <> Self.VyhSettings.zamekPoloha) and (not (Self.zamek as TBlkZamek).porucha)) then
   (Self.zamek as TBlkZamek).porucha := true;
end;

////////////////////////////////////////////////////////////////////////////////
// PT:

procedure TBlkVyhybka.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;

 TBlk.RCStoJSON(Self.VyhSettings.RCSAddrs[0], json['rcs'].O['vstup+']);
 TBlk.RCStoJSON(Self.VyhSettings.RCSAddrs[1], json['rcs'].O['vstup-']);
 TBlk.RCStoJSON(Self.VyhSettings.RCSAddrs[2], json['rcs'].O['vystup+']);
 TBlk.RCStoJSON(Self.VyhSettings.RCSAddrs[3], json['rcs'].O['vystup-']);

 json['usek'] := Self.VyhRel.UsekID;

 if (Self.VyhSettings.spojka > -1) then
   json['spojka'] := Self.VyhSettings.spojka;
 if (Self.VyhSettings.zamek > -1) then
  begin
   json['zamek'] := Self.VyhSettings.zamek;
   json['zamekPoloha'] := PolohaToStr(Self.VyhSettings.zamekPoloha);
  end;

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkVyhybka.GetPtState(json:TJsonObject);
begin
 json['poloha'] := PolohaToStr(Self.Poloha);
 if (Self.Stitek <> '') then json['stitek'] := Self.Stitek;
 if (Self.Vyluka <> '') then json['vyluka'] := Self.Vyluka;
end;

procedure TBlkVyhybka.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
begin
 if (reqJson.Contains('poloha')) then
  begin
   if (Self.Poloha = TVyhPoloha.disabled) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit neaktivni vyhybku');
     inherited;
     Exit();
    end;
   if (Self.outputLocked) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit zamcenou vyhybku');
     inherited;
     Exit();
    end;
   if (Self.Obsazeno = TUsekStav.obsazeno) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze prestavit obsazenou vyhybku');
     inherited;
     Exit();
    end;

   // nastaveni polohy vyhybky
   if (reqJson.S['poloha'] = '+') then
     Self.SetPoloha(TVyhPoloha.plus)
   else if (reqJson.S['poloha'] = '-') then
     Self.SetPoloha(TVyhPoloha.minus);
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkVyhybka.PolohaToStr(poloha:TVyhPoloha):string;
begin
 case (poloha) of
  TVyhPoloha.plus     : Result := '+';
  TVyhPoloha.minus    : Result := '-';
  TVyhPoloha.disabled : Result := 'vypnuto';
  TVyhPoloha.none     : Result := 'zadna';
  TVyhPoloha.both     : Result := 'obe';
 else
  Result := '';
 end;
end;

class function TBlkVyhybka.StrToPoloha(c: string):TVyhPoloha;
begin
 if (c = '+') then
   Result := TVyhPoloha.plus
 else if (c = '-') then
   Result := TVyhPoloha.minus
 else
   Result := TVyhPoloha.none;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.DecreaseNouzZaver(amount:Cardinal);
begin
 if (Self.VyhStav.vyhZaver = 0) then Exit();

 if (amount > Self.VyhStav.vyhZaver) then
   Self.VyhStav.vyhZaver := 0
 else
   Self.VyhStav.vyhZaver := Self.VyhStav.vyhZaver - amount;

 if (not Self.vyhZaver) then
  begin
   Blky.NouzZaverZrusen(Self);
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.NpObsazChange(Sender:TObject; data:Integer);
begin
 if ((data = 0) and (Sender = Self.npBlokPlus)) then
  begin
   // zmena bloku pro polohu +
   if (TBlkUsek(Self.npBlokPlus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 0))
   else
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 0));

   if (Self.Poloha = TVyhPoloha.plus) then Self.Change();

  end else if ((data = 1) and (Sender = Self.npBlokMinus)) then begin
   // zmena bloku pro polohu -
   if (TBlkUsek(Self.npBlokMinus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 1))
   else
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 1));

   if (Self.Poloha = TVyhPoloha.minus) then Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.MapNpEvents();
begin
 // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu +
 if (Self.npBlokPlus <> nil) then
  begin
   if (TBlkUsek(Self.npBlokPlus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 0))
   else
     TBlkUsek(Self.npBlokPlus).AddChangeEvent(TBlkUsek(Self.npBlokPlus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 0));
  end;

 // namapovani udalosti obsazeni a uvolneni neprofiloveho useku pro polohu -
 if (Self.npBlokMinus <> nil) then
  begin
   if (TBlkUsek(Self.npBlokMinus).Obsazeno = TUsekStav.obsazeno) then
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnUvol,
       CreateChangeEvent(Self.NpObsazChange, 1))
   else
     TBlkUsek(Self.npBlokMinus).AddChangeEvent(TBlkUsek(Self.npBlokMinus).EventsOnObsaz,
       CreateChangeEvent(Self.NpObsazChange, 1));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.StitVylUPO(SenderPnl:TIdContext; SenderOR:TObject;
      UPO_OKCallback: TNotifyEvent; UPO_EscCallback:TNotifyEvent);
var upo:TUPOItems;
    item:TUPOItem;
    lines:TStrings;
begin
 upo := TList<TUPOItem>.Create;
 try
  if (Self.Stitek <> '') then
   begin
    item[0] := GetUPOLine('ŠTÍTEK '+Self.GlobalSettings.name, taCenter, clBlack, clTeal);
    lines := GetLines(Self.Stitek, _UPO_LINE_LEN);

    try
      item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;

   upo.Add(item);
  end;

  if (Self.Vyluka <> '') then
   begin
    item[0] := GetUPOLine('VÝLUKA '+Self.GlobalSettings.name, taCenter, clBlack, clOlive);
    lines := GetLines(Self.Vyluka, _UPO_LINE_LEN);

    try
      item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;

   upo.Add(item);
  end;

  ORTCPServer.UPO(SenderPnl, upo, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
 finally
   upo.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkVyhybka.SetSpojkaNoPropag(spojka:Integer);
begin
 Self.VyhSettings.spojka := spojka;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.PanelStateString():string;
var fg, bg: TColor;
    Blk:TBlk;
begin
 Result := inherited;

 // Popredi
 if (not Self.vyhZaver) then
  begin
   case (Self.Obsazeno) of
    TUsekStav.disabled: fg := clFuchsia;
    TUsekStav.none    : fg := $A0A0A0;
    TUsekStav.uvolneno: fg := $A0A0A0;
    TUsekStav.obsazeno: fg := clRed;
   else
    fg := clFuchsia;
   end;

   if (Self.Obsazeno = TUsekStav.uvolneno) then
    begin
     case (Self.Zaver) of
      vlak   : fg := clLime;
      posun  : fg := clWhite;
      nouz   : fg := clAqua;
      ab     : fg := $707070;
     end;//case

     // je soucasti vybarveneho neprofiloveho useku
     Blky.GetBlkByID(Self.UsekID, Blk);
     if ((Blk <> nil) and ((Blk.typ = _BLK_USEK) or (Blk.typ = _BLK_TU))
         and (fg = $A0A0A0) and (TBlkUsek(Blk).IsNeprofilJC)) then
       fg := clYellow;
    end;

   // do profilu vyhybky zasahuje obsazeny usek
   if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokPlus <> nil) and (Self.Poloha = TVyhPoloha.plus) and
       (TBlkUsek(Self.npBlokPlus).Obsazeno <> TUsekStav.uvolneno)) then
     fg := clYellow;

   // do profilu vyhybky zasahuje obsazeny usek
   if (((fg = $A0A0A0) or (fg = clRed)) and (Self.npBlokMinus <> nil) and (Self.Poloha = TVyhPoloha.minus) and
       (TBlkUsek(Self.npBlokMinus).Obsazeno <> TUsekStav.uvolneno)) then
     fg := clYellow;

  end else begin
   // nouzovy zaver vyhybky ma prioritu i nad obsazenim useku
   fg := clAqua;
  end;
 Result := Result + PrevodySoustav.ColorToStr(fg) + ';';

 // Pozadi
 bg := clBlack;
 if (Self.Stitek <> '') then bg := clTeal;
 if (Self.Vyluka <> '') then bg := clOlive;
 if (diag.showZaver) then
  begin
   if (Self.Zaver > TZaver.no) then
     bg := clGreen
   else if (Self.VyhStav.outputLocked) then
     bg := clBlue
   else if (Self.ShouldBeLocked()) then
     bg := clOlive;
  end;

 Result := Result + PrevodySoustav.ColorToStr(bg) + ';' +
                    IntToStr(PrevodySoustav.BoolToInt(Self.NUZ)) + ';' +
                    IntToStr(Integer(Self.Poloha))+';';
end;

////////////////////////////////////////////////////////////////////////////////

class function TBlkVyhybka.CombineSpojkaInputs(first: TRCSInputState; second: TRCSInputState):TRCSInputState;
begin
 if ((first = TRCSInputState.failure) or (second = TRCSInputState.failure)) then
   Exit(TRCSInputState.failure);
 if ((first = TRCSInputState.unavailableModule) or (second = TRCSInputState.unavailableModule)) then
   Exit(TRCSInputState.unavailableModule);
 if ((first = TRCSInputState.unavailablePort) or (second = TRCSInputState.unavailablePort)) then
   Exit(TRCSInputState.unavailablePort);
 if ((first = TRCSInputState.isOn) and (second = TRCSInputState.isOn)) then
   Exit(TRCSInputState.isOn);
 Result := TRCSInputState.isOff;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.MockInputs():TBlkVyhInputs;
begin
 if (Self.Poloha = TVyhPoloha.plus) then
   Exit(TBlkVyhInputs.Create(isOn, isOff))
 else if (Self.Poloha = TVyhPoloha.minus) then
   Exit(TBlkVyhInputs.Create(isOff, isOn))
 else if ((Self.StaveniPlus) and (Now > Self.VyhStav.staveniStart+EncodeTime(0, 0, _VYH_STAVENI_MOCK_SEC, 0))) then
   Exit(TBlkVyhInputs.Create(isOn, isOff))
 else if ((Self.StaveniMinus) and (Now > Self.VyhStav.staveniStart+EncodeTime(0, 0, _VYH_STAVENI_MOCK_SEC, 0))) then
   Exit(TBlkVyhInputs.Create(isOff, isOn))
 else if (Self.Poloha = TVyhPoloha.disabled) then begin
   // proper booting of spojka
   if (Self.VyhStav.polohaSave = TVyhPoloha.plus) then
     Exit(TBlkVyhInputs.Create(isOn, isOff))
   else if (Self.VyhStav.polohaSave = TVyhPoloha.minus) then
     Exit(TBlkVyhInputs.Create(isOff, isOn))
   else
     Exit(TBlkVyhInputs.Create(isOff, isOff));
 end else
   Exit(TBlkVyhInputs.Create(isOff, isOff));
end;

////////////////////////////////////////////////////////////////////////////////

constructor TBlkVyhInputs.Create(plus, minus: TRCSInputState);
begin
 Self.plus := plus;
 Self.minus := minus;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.GetSpojka():TBlkVyhybka;
begin
 Blky.GetBlkByID(Self.VyhSettings.spojka, TBlk(Result));
 if ((Result <> nil) and (Result.typ <> _BLK_VYH)) then
   Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkVyhybka.ShouldBeLocked(withZamek: boolean):boolean;
begin
 Result := (Self.Zaver > TZaver.no) or (Self.vyhZaver) or (Self.intentionalLocked) or
           ((withZamek) and (Self.ZamekLocked()));

 if (Self.spojka <> nil) then
   Result := Result or (Self.spojka.Zaver > TZaver.no) or (Self.spojka.vyhZaver) or
                       (Self.spojka.intentionalLocked) or ((withZamek) and (Self.spojka.ZamekLocked()));
end;

function TBlkVyhybka.ZamekLocked():Boolean;
begin
 Result := (Self.zamek <> nil) and (not (Self.zamek as TBlkZamek).klicUvolnen);
end;

function TBlkVyhybka.MeOrSpojkaZaverStaveni():boolean;
begin
 Result := (Self.Zaver = TZaver.staveni) or ((Self.spojka <> nil) and (Self.spojka.Zaver = TZaver.staveni));
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

