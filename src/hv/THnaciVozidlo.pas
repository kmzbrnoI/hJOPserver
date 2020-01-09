unit THnaciVozidlo;

{
  -------------------------- implementace ----------------------------------
  ------------------------- HNACIHO VOZIDLA --------------------------------
 Digitalni adresa je jen jen pro cteni a lze ji nastavit jen v ctoru !
 toto je schvalne kvuli zpusobu, kterym jsou HV ulozena v databazi
 pri zmene adresy je potreba HV smazat a znovu vytvorit

 Jak to funguje:
    Loko muze byt ve dvou rezimech:
      a) rizeni automatem
      b) rucni rizeni
      Rezim rizeni je jeden jediny pro cele loko. Loko v rucnim rezimu ma rucni
      POM, loko v automatu ma POM automatu.

   Jak se ziskava rizeni loko do klienta? Skrze autorizacni token.
      Jak to funguje: technologie si nemuze dovolit vydat jen tak nekomu rizeni
      loko. Kontrolu nad lokomotivami v kazde stanici ma jeji dispecer, ktery
      dane loko muze pridelit strojvedoucim. Strojvedouci se tedy pripoji k
      serveru, autorizuje a pozada prislsneho dispecera (panel) o loko. Dispecer
      mu prideli loko a strojvedouci ji muze ridit.

      NEBO: dispecer si vyzada od serveru tzv autorizacni token, coz je unikatni
      string vytvoreny pro konkretni loko v konkretni cas. Tento string je
      odeslan dispecerovi. Tento token opravnuje k rizeni loko bez zadosti
      dispecera. Pokud ho tedy naprikald dispecer vyda strojvedoucimu, strojvedouci
      na jeho zaklade muze prevzit rizeni loko. Token patri vzdy ke konkretni
      loko a ma omezenou casovou platnost na nekolik minut.

 Pozn.
   Vsechny funkce spojene s nastavovanim dat HV maji parametr Sender
   kam je vhodne dat bud konkretni regulator, nebo OR v pripade
     regulatoru klienta.
   Informace o zmene dat HV je pak volana do vsech systemu mimo Senderu.
   Tedy napriklad, pokud je loko otevrene v 5 regulatorech a jeste na serveru
     a dojde ke zmene rychlosti v OR1, je informace o zmene rychlosti
     odeslana do OR2, OR3, OR4, OR5 a regulatoru na serveru, nikoliv
     vsak do OR1 (tomu prijde napriklad OK, ci error callback)

 Prebirani lokomotivy:
  1) Zavolat locoAcquire do Trakce (zjisti vsechny informace o lokomotive)
  2) Nastavit spravny smer loko a rychlost loko (vzhledem k souprave nebo aktualni)
  3) Nastavit funkce na pzadovane hodnoty
  4) Naprogramovat POM
  Pokud v libovolne casti procesu nastane chyba, je vyvolan Error callback.
}

interface

uses Trakce, TBlok, Classes, StrUtils, SysUtils, TOblRizeni,
      Generics.Collections, IdContext, IniFiles, IBUtils, JsonDataObjects;

const
  _HV_FUNC_MAX       = 28;   // maximalni funkcni cislo; funkce zacinaji na cisle 0
  _TOKEN_TIMEOUT_MIN = 3;    // delka platnosti tokenu pro autorizaci hnaciho vozidla v minutach
  _TOKEN_LEN         = 24;   // delka autorizacniho tokenu pro prevzeti hnaciho vozidla
  _DEFAUT_MAX_SPEED  = 120;  // 120 km/h

  _LOK_VERSION_SAVE = '2.0';

type
  // v jakem smeru se nachazi stanoviste A
  THVStanoviste = (lichy = 0, sudy = 1);
  TFunkce = array[0.._HV_FUNC_MAX] of boolean;
  TPomStatus = (released = 0, pc = 1, progr = 2, error = 3);

  // trida hnaciho vozidla
  THVClass = (parni = 0, diesel = 1, motor = 2, elektro = 3);

  // mod posilani dat hnaciho vozidla klientovi
  // full: s POM
  TLokStringMode = (normal = 0, full = 1);

  THVPomCV = record                                 // jeden zaznam POM se sklada z
    cv:Word;                                           // oznaceni CV a
    data:Byte;                                         // dat, ktera se maji do CV zapsat.
  end;

  THVFuncType = (permanent = 0, momentary = 1);

  THVData = record                                  // data hnaciho vozidla (nacitaji se ze souboru, program sam je vicemene nemeni)
   Nazev:string;                                       // nazev HV
   Majitel:string;                                     // majitel HV
   Oznaceni:string;                                    // oznaceni HV
   Poznamka:String;                                    // poznamka k HV
   Trida:THVClass;                                     // trida hnaciho vozidla - parni, diesel, motor, elektro
   maxRychlost:Cardinal;                               // max. rychlsot hnaciho vozidla

   POMtake:TList<THVPomCV>;                            // seznam POM pri prevzeti do automatu
   POMrelease:TList<THVPomCV>;                         // seznam POM pri uvolneni to rucniho rizeni

   funcVyznam:array[0.._HV_FUNC_MAX] of string;        // seznam popisu funkci hnaciho vozidla
   funcType:array[0.._HV_FUNC_MAX] of THVFuncType;     // typy funkci hnaciho vozidla
  end;

  THVNajeto = record                                // kolik bloku a metru hnaciho vozidlo najelo (vedeme si statistiky)
   Metru:Real;
   Bloku:Cardinal;
  end;

  THVRegulator = record                             // jeden regulator -- klient -- je z pohledu hnaciho vozidla reprezentovan
    conn:TIdContext;                                   // fyzickym spojenim k tomu regulatoru -- klientu a
    root:boolean;                                      // tages, jestli je uzivatel za timto regulatorem root
  end;

  THVToken = record                                 // jeden token opravnujici prevzeti rizeni hnaciho vozidla
    timeout:TDateTime;                                 // cas expirace tokenu (obvykle 3 minuty od zadosti)
    token:string;                                      // samotny token
  end;

  THVStav = record                                  // stav hanciho vozidla -- je menen za behu programu
   StanovisteA:THVStanoviste;                          // umisteni stanoviste A: 0 = lichy; 1 = sudy

   najeto_vpred:THVNajeto;                             // statistiky najeti smerem vpred
   najeto_vzad:THVNajeto;                              // statistiky najeti smerem vzad
   funkce:TFunkce;                                     // stav funkci tak, jak je chceme; uklada se do souboru
   souprava:Integer;                                   // index soupravy, na ktere je hnaci vozidlo
   stanice:TOR;                                        // oblast rizeni, ve ktere se nachazi HV
   regulators:TList<THVRegulator>;                     // seznam regulatoru -- klientu
   tokens:TList<THVToken>;                             // aktualni seznam tokenu -- jedno HV muze mit prideleno vice tokenu
   ruc:boolean;                                        // jestli je hnaciho vozidlo v rucnim rizeni
   last_used:TDateTime;                                // cas posledniho pouzivani loko
   acquired:Boolean;
   stolen:Boolean;
   pom:TPomStatus;
   trakceError:Boolean;
   acquiring:Boolean;
  end;

  THV = class                                       // HNACI VOZIDLO
   private const

   private

     fadresa:Word;                                     // adresa je read-only !
     m_funcDict:TDictionary<string, Integer>;          // mapovani vyznamu funkci na cisla funkci
     acquiredOk:TCommandCallback;
     acquiredErr:TCommandCallback;
     releasedOk:TCommandCallback;
     pomOk:TCommandCallback;
     pomErr:TCommandCallback;
     pomTarget:TPomStatus;

     procedure LoadData(ini:TMemIniFile; section:string);
     procedure LoadState(ini:TMemIniFile; section:string);
                                                       // nacte data ze souboru
     procedure SetRuc(state:boolean);                  // nastavi rucni rizeni
     procedure UpdateFuncDict();
     procedure SetSouprava(new:Integer);

     function GetSlotFunkce():TFunkce;
     function GetRealSpeed():Integer;
     function GetStACurrentDirection():Boolean;

     procedure TrakceCallbackOk(Sender:TObject; data:Pointer);
     procedure TrakceCallbackErr(Sender:TObject; data:Pointer);
     procedure TrakceCallbackCallEv(cb:PTCb);
     procedure SlotChanged(Sender:TObject; speedChanged:boolean; dirChanged:boolean);
     procedure TrakceAcquired(Sender: TObject; LocoInfo:TTrkLocoInfo);
     procedure TrakceAcquiredDirection(Sender: TObject; data:Pointer);
     procedure TrakceAcquiredFunctionsSet(Sender:TObject; Data:Pointer);
     procedure TrakceAcquiredPOMSet(Sender:TObject; Data:Pointer);
     procedure TrakceAcquiredErr(Sender:TObject; data:Pointer);

     procedure TrakceReleased(Sender:TObject; data:Pointer);
     procedure TrakceReleasedPOM(Sender:TObject; data:Pointer);

     procedure TrakcePOMOK(Sender:TObject; data:Pointer);
     procedure TrakcePOMErr(Sender:TObject; data:Pointer);

   public

    index:Word; // index v seznamu vsech hnacich vozidel
    data:THVData;
    stav:THVStav;
    slot:TTrkLocoInfo;
    changed:boolean; // jestli se zmenil stav HV tak, ze je potraba aktualizaovat tabulku ve F_Main

     constructor Create(data_ini:TMemIniFile; state_ini:TMemIniFile; section:string); overload;
     constructor Create(adresa:Word; data:THVData; stav:THVStav); overload;
     constructor Create(panel_str:string; Sender:TOR); overload;
     destructor Destroy(); override;

     procedure SaveData(); overload;
     procedure SaveData(const filename:string); overload;
     procedure SaveState(ini:TMemIniFile);

     procedure UpdateFromPanelString(data:string);     // nacteni informaci o HV z klienta

     procedure RemoveStats();                          // smaz statistiky najeto
     function ExportStats():string;                    // export najetych statistik hnaciho vozidla

     function PredejStanici(st:TOR):Integer;           // predej HV jine stanici
     function GetPanelLokString(mode:TLokStringMode = normal):string; // vrati HV ve standardnim formatu pro klienta
     procedure UpdateRuc(send_remove:boolean = true);  // aktualizuje informaci o rucnim rizeni do panelu (cerny text na bilem pozadi dole na panelu)

     procedure RemoveRegulator(conn:TIDContext);       // smaze regulator -- klienta; je volano jen jako callback regulatoru!
     function IsReg(conn:TIdContext):boolean;          // je na tomto HV tento regulator ?
     procedure UpdateAllRegulators();
     procedure ForceRemoveAllRegulators();

     function GetToken():string;
     function IsToken(str:string):boolean;
     procedure RemoveToken(token:string);
     procedure UpdateTokenTimeout(); // aktualizace vyprseni platnosti tokenu, melo by byt volano periodicky

     function CanPlayHouk(sound:string):boolean;       // vraci true pokud je povoleno prehravani zvuku
     procedure CheckRelease();
     procedure RecordUseNow();
     function NiceName():string;
     function ShouldAcquire():boolean;

     procedure SetSpeed(speed:Integer; ok: TCb; err: TCb; Sender: TObject = nil); overload;
     procedure SetSpeed(speed:Integer; Sender: TObject = nil); overload;
     procedure SetDirection(dir:boolean; ok: TCb; err: TCb; Sender: TObject = nil); overload;
     procedure SetDirection(dir:boolean; Sender: TObject = nil); overload;
     procedure SetSpeedDir(speed: Integer; direction: Boolean; ok: TCb; err: TCb; Sender:TObject = nil); overload;
     procedure SetSpeedDir(speed: Integer; direction: Boolean; Sender:TObject = nil); overload;
     procedure SetSpeedStepDir(speedStep: Integer; direction: Boolean; ok: TCb; err: TCb; Sender:TObject = nil); overload;
     procedure SetSpeedStepDir(speedStep: Integer; direction: Boolean; Sender:TObject = nil); overload;
     procedure SetSingleFunc(func:Integer; state:Boolean; ok: TCb; err: TCb; Sender:TObject = nil);
     procedure EmergencyStop(ok: TCb; err: TCb; Sender:TObject = nil);
     procedure CSReset();

     procedure TrakceAcquire(ok: TCb; err: TCb);
     procedure TrakceRelease(ok: TCb);
     procedure TrakceStolen();

     procedure StavFunctionsToSlotFunctions(ok: TCb; err: TCb; Sender: TObject = nil);

     procedure SetPom(pom:TPomStatus; ok: TCb; err: TCb);

     class function CharToHVFuncType(c:char):THVFuncType;
     class function HVFuncTypeToChar(t:THVFuncType):char;

     //PT:
     procedure GetPtData(json:TJsonObject; includeState:boolean);
     procedure GetPtState(json:TJsonObject);
     procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject);

     property adresa:Word read fadresa;
     property nazev:string read data.nazev;
     property ruc:boolean read stav.ruc write SetRuc;
     property funcDict:TDictionary<string, Integer> read m_funcDict;
     property souprava:Integer read stav.souprava write SetSouprava;
     property speedStep:Byte read slot.speed;
     property realSpeed:Integer read GetRealSpeed;
     property direction:boolean read slot.direction;
     property stACurrentDirection:boolean read GetStACurrentDirection;
     property acquired:boolean read stav.acquired;
     property stolen:boolean read stav.stolen;
     property pom:TPomStatus read stav.pom;
     property trakceError:Boolean read stav.trakceError;
     property slotFunkce:TFunkce read GetSlotFunkce;
     property stavFunkce:TFunkce read stav.funkce;
     property acquiring:boolean read stav.acquiring;
  end;//THV


implementation

uses ownStrUtils, Prevody, TOblsRizeni, THVDatabase, SprDb, DataHV, fRegulator, TBloky,
      RegulatorTCP, fMain, PTUtils, TCPServerOR, appEv, Logging, TechnologieTrakce;

////////////////////////////////////////////////////////////////////////////////

constructor THV.Create(data_ini:TMemIniFile; state_ini:TMemIniFile; section:string);
begin
 inherited Create();

 Self.Stav.regulators := TList<THVRegulator>.Create();
 Self.Stav.tokens     := TList<THVToken>.Create();

 Self.stav.souprava := -1;
 Self.stav.stanice := nil;
 Self.CSReset();

 Self.data.POMtake    := TList<THVPomCV>.Create();
 Self.data.POMrelease := TList<THVPomCV>.Create();

 Self.acquiredOk := TTrakce.Callback();
 Self.acquiredErr := TTrakce.Callback();

 Self.m_funcDict := TDictionary<string, Integer>.Create();

 try
   Self.LoadData(data_ini, section);
   if (state_ini.SectionExists(section)) then // backward compatibility
     Self.LoadState(state_ini, section)
   else
     Self.LoadState(data_ini, section)
 except
   on E:Exception do
     raise Exception.Create('Chyba při načítání sekce '+section+' - '+E.Message);
 end;
end;//ctor

constructor THV.Create(adresa:Word; data:THVData; stav:THVStav);
begin
 inherited Create();

 Self.fadresa := adresa;
 Self.Data    := data;
 Self.Stav    := stav;

 Self.acquiredOk := TTrakce.Callback();
 Self.acquiredErr := TTrakce.Callback();

 Self.m_funcDict := TDictionary<string, Integer>.Create();
 Self.UpdateFuncDict();

 if (not Assigned(Self.Data.POMtake))    then Self.data.POMtake    := TList<THVPomCV>.Create;
 if (not Assigned(Self.Data.POMrelease)) then Self.data.POMrelease := TList<THVPomCV>.Create;
 if (not Assigned(Self.Stav.regulators)) then Self.Stav.regulators := TList<THVRegulator>.Create();
 if (not Assigned(Self.Stav.tokens))     then Self.Stav.tokens     := TList<THVToken>.Create();
end;//ctor

constructor THV.Create(panel_str:string; Sender:TOR);
begin
 inherited Create();

 Self.Stav.najeto_vpred.Metru := 0;
 Self.Stav.najeto_vpred.Bloku := 0;
 Self.Stav.najeto_vzad.Metru  := 0;
 Self.Stav.najeto_vzad.Bloku  := 0;

 Self.Stav.regulators := TList<THVRegulator>.Create();
 Self.Stav.tokens     := TList<THVToken>.Create();

 Self.Stav.souprava := -1;
 Self.Stav.stanice  := Sender;
 Self.Stav.last_used := Now;

 Self.data.POMtake    := TList<THVPomCV>.Create;
 Self.data.POMrelease := TList<THVPomCV>.Create;

 Self.m_funcDict := TDictionary<string, Integer>.Create();

 Self.UpdateFromPanelString(panel_str);
end;//ctor

destructor THV.Destroy();
begin
 Self.Stav.regulators.Free();
 Self.Stav.tokens.Free();
 Self.data.POMtake.Free();
 Self.data.POMrelease.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure THV.LoadData(ini:TMemIniFile; section:string);
var strs, strs2:TStrings;
    i:Integer;
    pomCV:THVPomCV;
    str, s:string;
    addr:Integer;
begin
  strs  := TStringList.Create();
  strs2 := TStringList.Create();

  try
   addr := StrToInt(section);
   if ((addr < 0) or (addr > 9999)) then raise Exception.Create('Adresa loko mimo rozsah');
   Self.fadresa := addr;

   Self.Data.Nazev    := ini.ReadString(section, 'nazev', section);
   Self.Data.Majitel  := ini.ReadString(section, 'majitel', '');
   Self.Data.Oznaceni := ini.ReadString(section, 'oznaceni', section);
   Self.Data.Poznamka := ini.ReadString(section, 'poznamka', '');
   Self.Data.Trida    := THVClass(ini.ReadInteger(section, 'trida', 0));
   Self.Data.maxRychlost := ini.ReadInteger(section, 'max_rychlost', _DEFAUT_MAX_SPEED);

   // POM pri prebirani : (cv,data)(cv,data)(...)...
   str := ini.ReadString(section, 'pom_take', '');
   Self.Data.POMtake.Clear();
   strs.Clear();
   ExtractStringsEx([')'], ['('], str, strs);
   for s in strs do
    begin
     strs2.Clear();
     ExtractStringsEx([','], [], s, strs2);
     if (strs2.Count >= 2) then
      begin
       try
         pomCV.cv   := StrToInt(strs2[0]);
         if ((pomCV.cv < 1) or (pomCV.cv > 1023)) then continue;
         pomCV.data := StrToInt(strs2[1]);
         Self.Data.POMtake.Add(pomCV);
       except
         continue;
       end;// except
      end;//if data2.Count >= 2
    end;

   // POM pri uvolneni : (cv,data)(cv,data)(...)...
   str := ini.ReadString(section, 'pom_release', '');
   Self.Data.POMrelease.Clear();
   strs.Clear();
   ExtractStringsEx([')'], ['('], str, strs);
   for s in strs do
    begin
     strs2.Clear();
     ExtractStringsEx([','], [], s, strs2);
     if (strs2.Count >= 2) then
      begin
       try
         pomCV.cv   := StrToInt(strs2[0]);
         if ((pomCV.cv < 1) or (pomCV.cv > 1023)) then continue;
         pomCV.data := StrToInt(strs2[1]);
         Self.Data.POMrelease.Add(pomCV);
       except
         continue;
       end;// except
      end;//if data2.Count >= 2
    end;

   // vyznamy funkci:
   str := ini.ReadString(section, 'func_vyznam', '');
   strs.Clear();
   ExtractStringsEx([';'], [], str, strs);
   for i := 0 to _HV_FUNC_MAX do
    begin
     if (i < strs.Count) then
       Self.Data.funcVyznam[i] := strs[i]
      else
       Self.Data.funcVyznam[i] := '';
    end;
   Self.UpdateFuncDict();

   // typy funkci:
   str := ini.ReadString(section, 'func_type', '');
   for i := 0 to _HV_FUNC_MAX do
    begin
     if (i < Length(str)) then
      begin
       Self.Data.funcType[i] := CharToHVFuncType(str[i+1]);
      end else
       Self.Data.funcType[i] := THVFuncType.permanent;
    end;

  except
   strs.Free();
   strs2.Free();
   raise;
  end;

 strs.Free();
 strs2.Free();
end;

procedure THV.LoadState(ini:TMemIniFile; section:string);
var i:Integer;
    stanice:Integer;
    str:string;
begin
 stanice := ORs.GetORIndex(ini.ReadString(section, 'stanice', ''));
 if (stanice <> -1) then
  ORs.GetORByIndex(stanice, Self.Stav.stanice)
 else
  ORs.GetORByIndex(HVDb.default_or, Self.Stav.stanice);

 Self.Stav.najeto_vpred.Metru := ini.ReadFloat(section, 'najeto_vpred_metru', 0);
 Self.Stav.najeto_vpred.Bloku := ini.ReadInteger(section, 'najeto_vpred_bloku', 0);

 Self.Stav.najeto_vzad.Metru := ini.ReadFloat(section, 'najeto_vzad_metru', 0);
 Self.Stav.najeto_vzad.Bloku := ini.ReadInteger(section, 'najeto_vzad_bloku', 0);

 Self.Stav.StanovisteA := THVStanoviste(ini.ReadInteger(section, 'stanoviste_a', 0));

 try
   Self.Stav.last_used := StrToDateTime(ini.ReadString(section, 'last_used', ''));
 except
   Self.Stav.last_used := 0;
 end;

 // stav funkci
 str := ini.ReadString(section, 'stav_funkci', '');
 for i := 0 to _HV_FUNC_MAX do
   Self.Stav.funkce[i] := ((i < Length(str)) and PrevodySoustav.StrToBool(str[i+1]));
end;

procedure THV.SaveData(const filename:string);
var ini:TMemIniFile;
    addr, str:string;
    i:Integer;
    pom:THVPomCV;
begin
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 try
   ini.WriteString('global', 'version', _LOK_VERSION_SAVE);

   addr := IntToStr(Self.adresa);
   ini.EraseSection(addr);
   ini.WriteString(addr, 'nazev', Self.Data.Nazev);
   ini.WriteString(addr, 'majitel', Self.Data.Majitel);
   ini.WriteString(addr, 'oznaceni', Self.Data.Oznaceni);
   ini.WriteString(addr, 'poznamka', Self.Data.Poznamka);
   ini.WriteInteger(addr, 'trida', Integer(Self.Data.Trida));
   ini.WriteInteger(addr, 'max_rychlost', Self.Data.maxRychlost);

   // POM pri prebirani
   str := '';
   for pom in Self.Data.POMtake do
     str := str + '(' + IntToStr(pom.cv) + ',' + IntToStr(pom.data) + ')';
   ini.WriteString(addr, 'pom_take', str);

   // POM pri uvolneni
   str := '';
   for pom in Self.Data.POMrelease do
     str := str + '(' + IntToStr(pom.cv) + ',' + IntToStr(pom.data) + ')';
   ini.WriteString(addr, 'pom_release', str);

   // vyznam funkci
   str := '';
   for i := 0 to _HV_FUNC_MAX do
    begin
     if (Self.Data.funcVyznam[i] <> '') then
       str := str + '{' + Self.Data.funcVyznam[i] + '};'
     else
       str := str + ';';
    end;
   ini.WriteString(addr, 'func_vyznam', str);

   // typ funkci
   str := '';
   for i := 0 to _HV_FUNC_MAX do
     str := str + HVFuncTypeToChar(Self.Data.funcType[i]);
   ini.WriteString(addr, 'func_type', str);

 except
   ini.UpdateFile();
   ini.Free();
   raise;
 end;

 ini.UpdateFile();
 ini.Free();
end;

procedure THV.SaveData();
begin
 Self.SaveData(HVdb.FilenameForLok(Self));
end;

procedure THV.SaveState(ini:TMemIniFile);
var i:Integer;
    addr, str:string;
begin
 if (Self.Stav.souprava > -1) then
   Self.RecordUseNow();

 addr := IntToStr(Self.adresa);

 if (Self.Stav.stanice <> nil) then
   ini.WriteString(addr, 'stanice', Self.Stav.stanice.id)
 else
   ini.WriteString(addr, 'stanice', '');

 ini.WriteFloat(addr, 'najeto_vpred_metru', Self.Stav.najeto_vpred.Metru);
 ini.WriteInteger(addr, 'najeto_vpred_bloku', Self.Stav.najeto_vpred.Bloku);

 ini.WriteFloat(addr, 'najeto_vzad_metru', Self.Stav.najeto_vzad.Metru);
 ini.WriteInteger(addr, 'najeto_vzad_bloku', Self.Stav.najeto_vzad.Bloku);

 ini.WriteInteger(addr, 'stanoviste_a', Integer(Self.Stav.StanovisteA));

 if (Self.Stav.last_used > 0) then
   ini.WriteString(addr, 'last_used', DateTimeToStr(Self.Stav.last_used));

 // stav funkci
 str := '';
 for i := 0 to _HV_FUNC_MAX do
  begin
   if ((Self.Stav.funkce[i]) and (Self.Data.funcType[i] <> THVFuncType.momentary)) then
     str := str + '1'
   else
     str := str + '0';
  end;
 ini.WriteString(addr, 'stav_funkci', str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.RemoveStats();
begin
 Self.Stav.najeto_vpred.Metru := 0;
 Self.Stav.najeto_vpred.Bloku := 0;
 Self.Stav.najeto_vzad.Metru  := 0;
 Self.Stav.najeto_vzad.Bloku  := 0;
end;

// format vystupnich dat: adresa;nazev;majitel;najeto_metru_vpres;majeto_bloku_vpred;najeto_metru_vzad;najeto_bloku_vzad
function THV.ExportStats():string;
begin
 Result := IntToStr(Self.adresa) + ';' + Self.Data.Nazev + ';' +
           Self.Data.Majitel + ';' + Format('%5.2f',[Self.Stav.najeto_vpred.Metru]) + ';' +
           IntToStr(Self.Stav.najeto_vpred.Bloku) + ';' + Format('%5.2f',[Self.Stav.najeto_vzad.Metru]) + ';' +
           IntToStr(Self.Stav.najeto_vzad.Bloku);
end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetPanelLokString(mode:TLokStringMode = normal):string;
var i:Integer;
    func:TFunkce;
    pomCV:THVPOMCv;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce|rychlost_stupne|
 //   rychlost_kmph|smer|or_id|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
 //   {vyznam-F0;vyznam-F1;...}|typ_funkci|maximalni rychlost

 // souprava je bud cislo soupravy, nebo znak '-'
 Result := Self.Data.Nazev + '|' + Self.Data.Majitel + '|' + Self.Data.Oznaceni + '|{' + Self.Data.Poznamka + '}|' +
           IntToStr(Self.adresa) + '|' + IntToStr(Integer(Self.Data.Trida)) + '|';

 if (Self.Stav.souprava > -1) then
  Result := Result + Soupravy.GetSprNameByIndex(Self.Stav.souprava) + '|'
 else
  Result := Result + '-|';

 Result := Result + IntToStr(Integer(Self.Stav.StanovisteA)) + '|';

 if (Self.acquired) then
   func := Self.slotFunkce
 else
   func := Self.Stav.funkce;

 for i := 0 to _HV_FUNC_MAX do
  begin
   if (func[i]) then
     Result := Result + '1'
   else
     Result := Result + '0';
  end;

 Result := Result + '|' + IntToStr(Self.Slot.speed) + '|' +
           IntToStr(TrakceI.GetStepSpeed(Self.Slot.speed)) + '|' +
           IntToStr(PrevodySoustav.BoolToInt(Self.direction)) + '|' + Self.Stav.stanice.id + '|';

 if (mode = TLokStringMode.full) then
  begin
   // cv-take
   Result := Result + '{';
   for pomCV in Self.Data.POMtake do
     Result := Result + '[{' + IntToStr(POMcv.cv) + '|' + IntToStr(POMcv.data) + '}]';
   Result := Result + '}|{';

   // cv-release
   for pomCV in Self.Data.POMrelease do
     Result := Result + '[{' + IntToStr(POMcv.cv) + '|' + IntToStr(POMcv.data) + '}]';
   Result := Result + '}';
  end else begin
   Result := Result + '|';
  end;// else pom

 // vyznamy funkci
 Result := Result + '|{';
 for i := 0 to _HV_FUNC_MAX do
  begin
   if (Self.Data.funcVyznam[i] <> '') then
     Result := Result + '{' + Self.Data.funcVyznam[i] + '};'
   else
     Result := Result + ';';
  end;
 Result := Result + '}|';

 // typy funkci
 for i := 0 to _HV_FUNC_MAX do
   Result := Result + HVFuncTypeToChar(Self.Data.funcType[i]);
 Result := Result + '|';

 Result := Result + IntToStr(Self.Data.maxRychlost) + '|';
end;

////////////////////////////////////////////////////////////////////////////////

function THV.PredejStanici(st:TOR):Integer;
begin
 // zruseni RUC u stare stanice
 Self.Stav.stanice.BroadcastData('RUC-RM;'+IntToStr(Self.adresa));

 // zmena stanice
 Self.Stav.stanice := st;

 // RUC do nove stanice
 Self.UpdateRuc(false);

 Self.changed := true;
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.UpdateFromPanelString(data:string);
var str, str2, str3:TStrings;
    i:Integer;
    pomCv:THVPomCv;
    tmp:string;
    maxFunc:Integer;
begin
 str := TStringList.Create();
 str2 := TStringList.Create();
 str3 := TStringList.Create();
 ExtractStringsEx(['|'], [], data, str);

 try
  Self.Data.Nazev       := str[0];
  Self.Data.Majitel     := str[1];
  Self.Data.Oznaceni    := str[2];
  Self.Data.Poznamka    := str[3];
  Self.fadresa          := StrToInt(str[4]);
  Self.Data.Trida       := THVClass(StrToInt(str[5]));
  Self.Stav.StanovisteA := THVStanoviste(StrToInt(str[7]));

  maxFunc := Min(Length(str[8])-1, _HV_FUNC_MAX);
  if (maxFunc >= 0) then
    for i := 0 to maxFunc do
      Self.Stav.funkce[i] := (str[8][i+1] = '1');

  if (str.Count > 13) then
   begin
     // pom-take
     str2.Clear();
     Self.Data.POMtake.Clear();
     ExtractStringsEx([']'] , ['['], str[13], str2);
     for tmp in str2 do
      begin
       str3.Clear();
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv   := StrToInt(str3[0]);
       pomCV.data := StrToInt(str3[1]);
       Self.Data.POMtake.Add(pomCV);
      end;
   end;

  if (str.Count > 14) then
   begin
     // pom-release
     str2.Clear();
     Self.Data.POMrelease.Clear();
     ExtractStringsEx([']'] , ['['], str[14], str2);
     for tmp in str2 do
      begin
       str3.Clear();
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv   := StrToInt(str3[0]);
       pomCV.data := StrToInt(str3[1]);
       Self.Data.POMrelease.Add(pomCV);
      end;
   end;

  if (str.Count > 15) then
   begin
    // vyznam funkci
    str2.Clear();
    ExtractStringsEx([';'], [], str[15], str2);
    for i := 0 to _HV_FUNC_MAX do
      if (i < str2.Count) then
       Self.Data.funcVyznam[i] := str2[i]
      else
       Self.Data.funcVyznam[i] := '';
   end else begin
    for i := 0 to _HV_FUNC_MAX do
      Self.Data.funcVyznam[i] := '';
   end;
   Self.UpdateFuncDict();

  if (str.Count > 16) then
   begin
    // typy funkci
    for i := 0 to _HV_FUNC_MAX do
      if (i < Length(str[16])) then
       Self.Data.funcType[i] := CharToHVFuncType(str[16][i+1])
      else
       Self.Data.funcType[i] := THVFuncType.permanent;
   end else begin
    for i := 0 to _HV_FUNC_MAX do
      Self.Data.funcType[i] := THVFuncType.permanent;
   end;

  if (str.Count > 17) then
    Self.Data.maxRychlost := StrToInt(str[17]);

 except
  on e:Exception do
   begin
    raise Exception.Create('Chyba při parsování dat hnacího vozidla - '+e.Message);
    Exit();
   end;
 end;

 Self.changed := true;

 if (Self.Stav.souprava > -1) then
   Blky.ChangeSprToTrat(Self.Stav.souprava);

 str.Free();
 str2.Free();
 str3.Free();

 // aktulizace LOKO v regulatorech
 Self.UpdateAllRegulators();
 Self.SaveData();
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.UpdateRuc(send_remove:boolean = true);
var spr:string;
begin
 if (Self.Stav.souprava > -1) then
  spr := Soupravy[Self.Stav.souprava].nazev
 else
  spr := '-';

 if (Self.stolen) then
  begin
   // loko ukradeno ovladacem
   Self.Stav.stanice.BroadcastData('RUC;'+IntToStr(Self.adresa)+';MM. '+IntToStr(Self.adresa)+' ('+spr+')');
   Exit();
  end else begin
   if (Self.ruc) then
     Self.Stav.stanice.BroadcastData('RUC;'+IntToStr(Self.adresa)+';RUČ. '+IntToStr(Self.adresa)+' ('+spr+')')
   else
     // loko neni v rucnim rizeni -> oznamit klientovi
     if (send_remove) then Self.Stav.stanice.BroadcastData('RUC-RM;'+IntToStr(Self.adresa));
  end;

end;

////////////////////////////////////////////////////////////////////////////////

// smazani regulatoru
procedure THV.RemoveRegulator(conn:TIDContext);
var i:Integer;
begin
 for i := 0 to Self.Stav.regulators.Count-1 do
   if (Self.Stav.regulators[i].conn = conn) then
    begin
     Self.Stav.regulators.Delete(i);

     // aktualizace rychlosti v pripade, kdy byla loko rizena rucne (force = true)
     if (Self.Stav.regulators.Count = 0) then
      begin
       Self.ruc := false;
       Self.CheckRelease();
      end;

     Exit();
    end;
end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetToken():string;
var token:THVToken;
    all:string;
    i:Integer;
    len:Integer;
begin
 // generace tokenu
 all := 'abcdefghijklmnopqrstuvwxyz0123456789';
 len := Length(all);

 token.token := '';
 for i := 0 to _TOKEN_LEN-1 do
   token.token := token.token + all[Random(len)+1];

 token.timeout := Now + EncodeTime(0, _TOKEN_TIMEOUT_MIN, 0, 0);
 Result := token.token;
 Self.Stav.tokens.Add(token);
end;

function THV.IsToken(str:string):boolean;
var token:THVToken;
begin
 for token in Self.Stav.tokens do
  if (token.token = str) then
   Exit(true);
 Result := false;
end;

procedure THV.RemoveToken(token:string);
var i:Integer;
begin
 for i := 0 to Self.Stav.tokens.Count-1 do
  if (Self.Stav.tokens[i].token = token) then
   begin
    Self.Stav.tokens.Delete(i);
    Exit();
   end;
end;

procedure THV.UpdateTokenTimeout();
var i:Integer;
begin
 for i := Self.Stav.tokens.Count-1 downto 0 do
   if (Now > Self.Stav.tokens[i].timeout) then
     Self.Stav.tokens.Delete(i);
end;

////////////////////////////////////////////////////////////////////////////////

// timto prikazem je lokomotive zapinano / vypinano rucni rizeni
procedure THV.SetRuc(state:boolean);
begin
 if (Self.Stav.ruc = state) then Exit();
 Self.Stav.ruc := state;

 if (state) then
  begin
   // loko je uvedeno do rucniho rizeni

   // nastavit POM rucniho rizeni
   // neprevzatym HV je POM nastaven pri prebirani; prebirni vozidel ale neni nase starost, to si resi volajici fuknce
   if ((Self.acquired) and (Self.pom <> TPomStatus.released)) then
     Self.SetPom(TPomStatus.released, TTrakce.Callback(), TTrakce.Callback());
  end else begin
   // loko je vyjmuto z rucniho rizeni

   if (Self.Stav.souprava > -1) then
    begin
     // POM automatu
     if (Self.pom <> TPomStatus.pc) then
       Self.SetPom(TPomStatus.pc, TTrakce.Callback(), TTrakce.Callback());

     Soupravy[Self.souprava].rychlost := Soupravy[Self.souprava].rychlost; // tento prikaz nastavi rychlost
    end else begin
     // loko neni na souprave -> zkusit odhlasit
     Self.CheckRelease();
    end;
  end;

 if (RegCollector.IsLoko(Self)) then
   RegCollector.LocoChanged(Self, Self.adresa);
 TCPRegulator.LokUpdateRuc(Self);

 // aktualizace informaci do panelu
 Self.UpdateRuc();
end;

////////////////////////////////////////////////////////////////////////////////

function THV.IsReg(conn:TIdContext):boolean;
var reg:THVRegulator;
begin
 for reg in Self.Stav.regulators do
   if (reg.conn = conn) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.UpdateAllRegulators();
var regulator:THVRegulator;
begin
 for regulator in Self.Stav.regulators do
   TCPRegulator.LokToRegulator(regulator.conn, Self);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.GetPtData(json:TJsonObject; includeState:boolean);
var i, lastFunction:Integer;
    types:string;
begin
 json['adresa']   := Self.adresa;
 json['nazev']    := Self.Data.Nazev;
 json['majitel']  := Self.Data.Majitel;
 json['oznaceni'] := Self.Data.Oznaceni;
 json['maxRychlost'] := Self.Data.maxRychlost;
 if (Self.Data.Poznamka <> '') then json['poznamka'] := Self.Data.Poznamka;

 case (Self.Data.Trida) of
  THVClass.parni   : json['trida'] := 'parni';
  THVClass.diesel  : json['trida'] := 'diesel';
  THVClass.motor   : json['trida'] := 'motor';
  THVClass.elektro : json['trida'] := 'elektro';
 end;

 lastFunction := _HV_FUNC_MAX;
 while ((lastFunction >= 0) and (Self.Data.funcVyznam[lastFunction] = '')) do
   Dec(lastFunction);

 types := '';
 for i := 0 to lastFunction do
  begin
   json.A['vyznamFunkci'].Add(Self.Data.funcVyznam[i]);
   types := types + HVFuncTypeToChar(Self.Data.funcType[i]);
  end;
 json['typFunkci'] := types;

 if (includeState) then
   Self.GetPtState(json['lokStav']);
end;

procedure THV.GetPtState(json:TJsonObject);
var i:Integer;
    stavFunkci:string;
begin
 json['rychlostStupne'] := Self.speedStep;
 json['rychlostKmph']   := Self.realSpeed;
 json['smer']           := Self.direction;

 stavFunkci := '';
 for i := 0 to _HV_FUNC_MAX do
   stavFunkci := stavFunkci + IntToStr(PrevodySoustav.BoolToInt(Self.slotFunkce[i]));
 json['stavFunkci'] := stavFunkci;

 case (Self.Stav.StanovisteA) of
  THVStanoviste.lichy : json['stanovisteA'] := 'L';
  THVStanoviste.sudy  : json['stanovisteA'] := 'S';
 end;

 json.O['najetoVpred'].F['metru'] := Self.Stav.najeto_vpred.Metru;
 json.O['najetoVpred'].I['bloku'] := Self.Stav.najeto_vpred.Bloku;

 json.O['najetoVzad'].F['metru'] := Self.Stav.najeto_vzad.Metru;
 json.O['najetoVzad'].I['bloku'] := Self.Stav.najeto_vzad.Bloku;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
var speed, dir, i:Integer;
    noveFunkce:TFunkce;
begin
 dir := 0;

 if (not Self.acquired) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Loko neprevzato');
   Exit();
  end;

 if (reqJson.Contains('smer')) then
  begin
   dir := StrToInt(reqJson['smer']);
   if (dir < 0) then dir := 0;
   if (dir > 1) then dir := 1;   
  end;

 if (reqJson.Contains('rychlostStupne')) then
  begin
   speed := StrToInt(reqJson['rychlostStupne']);
   if (speed > 28) then speed := 28;
   if (speed < 0) then speed := 0;

   if (reqJson.Contains('smer')) then
    begin
     Self.SetSpeedStepDir(speed, PrevodySoustav.IntToBool(dir));
    end else
     Self.SetSpeedStepDir(speed, Self.direction);
  end else if (reqJson.Contains('rychlostKmph')) then
  begin
   speed := StrToInt(reqJson['rychlostKmph']);

   if (reqJson.Contains('smer')) then
    begin
     Self.SetSpeedDir(speed, PrevodySoustav.IntToBool(dir));
    end else
     Self.SetSpeedDir(speed, Self.direction);
  end else if (reqJson.Contains('smer')) then
  begin
   Self.SetSpeedStepDir(Self.speedStep, PrevodySoustav.IntToBool(dir));
  end;

 if (reqJson.Contains('stavFunkci')) then
  begin
   for i := 0 to _HV_FUNC_MAX do
    begin
     if (i < Length(reqJson.S['stavFunkci'])) then
       noveFunkce[i] := PrevodySoustav.StrToBool(reqJson.S['stavFunkci'][i+1])
     else
       noveFunkce[i] := Self.Stav.funkce[i];
    end;
   Self.Stav.funkce := noveFunkce;
   Self.StavFunctionsToSlotFunctions(TTrakce.Callback(), TTrakce.Callback());
  end;

 Self.GetPtState(respJson.O['lokStav']);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.UpdateFuncDict();
var i:Integer;
begin
 Self.funcDict.Clear();
 for i := 0 to _HV_FUNC_MAX do
   if (Self.Data.funcVyznam[i] <> '') then
     Self.funcDict.AddOrSetValue(Self.Data.funcVyznam[i], i);
end;

////////////////////////////////////////////////////////////////////////////////

function THV.CanPlayHouk(sound:string):boolean;
begin
 Result := ((Self.Stav.regulators.Count = 0) and (not Self.stolen) and
           ((not Self.funcDict.ContainsKey('zvuk')) or (Self.Stav.funkce[Self.funcDict['zvuk']])) and
           (Self.funcDict.ContainsKey(sound)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.CheckRelease();
begin
 if ((Self.Stav.souprava = -1) and (not Self.ruc) and (Self.Stav.regulators.Count = 0) and
     (not RegCollector.IsLoko(Self)) and (Self.acquired)) then
  begin
   Self.SetSpeed(0);
   Self.TrakceRelease(TrakceI.Callback());
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.RecordUseNow();
begin
 Self.Stav.last_used := Now;
 Self.changed := true;
end;

procedure THV.SetSouprava(new:Integer);
begin
 if (new = Self.Souprava) then
   Exit();

 Self.Stav.souprava := new;

 if (new = -1) then
  begin
   Self.CheckRelease();
   Self.RecordUseNow();
  end;

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

function THV.NiceName():string;
begin
 Result := IntToStr(Self.adresa) + ' : ' + Self.Data.Nazev + '(' + Self.Data.Oznaceni + ')';
end;

////////////////////////////////////////////////////////////////////////////////

function THV.ShouldAcquire():boolean;
begin
 Result := ((Self.souprava > -1) and ((not Self.acquired) or (Self.pom = TPomStatus.error)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.ForceRemoveAllRegulators();
var i:Integer;
begin
 for i := Self.Stav.regulators.Count-1 downto 0 do
  begin
   try
     TCPRegulator.RemoveLok(Self.Stav.regulators[i].conn, Self, 'Násilné odhlášení dispečerem');
   except
     on E:Exception do
       AppEvents.LogException(E, 'THV.ForceRemoveAllRegulators');
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

class function THV.CharToHVFuncType(c:char):THVFuncType;
begin
 if (UpperCase(c) = 'M') then
   Result := THVFuncType.momentary
 else
   Result := THVFuncType.permanent;
end;

class function THV.HVFuncTypeToChar(t:THVFuncType):char;
begin
 if (t = THVFuncType.momentary) then
   Result := 'M'
 else
   Result := 'P';
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.SetSpeed(speed:Integer; ok: TCb; err: TCb; Sender: TObject = nil);
begin
 Self.SetSpeedDir(speed, Self.slot.direction, ok, err, Sender);
end;

procedure THV.SetSpeed(speed:Integer; Sender: TObject = nil);
begin
 Self.SetSpeed(speed, TrakceI.Callback(), TrakceI.Callback(), Sender);
end;

procedure THV.SetDirection(dir:boolean; ok: TCb; err: TCb; Sender: TObject = nil);
begin
 Self.SetSpeedDir(Self.slot.speed, dir, ok, err, Sender);
end;

procedure THV.SetDirection(dir:boolean; Sender: TObject = nil);
begin
 Self.SetDirection(dir, TrakceI.Callback(), TrakceI.Callback(), Sender);
end;

procedure THV.SetSpeedDir(speed: Integer; direction: Boolean; ok: TCb; err: TCb; Sender:TObject = nil);
begin
 Self.SetSpeedStepDir(TrakceI.SpeedStep(speed), direction, ok, err, Sender);
end;

procedure THV.SetSpeedDir(speed: Integer; direction: Boolean; Sender:TObject = nil);
begin
 Self.SetSpeedDir(speed, direction, TrakceI.Callback(), TrakceI.Callback(), Sender);
end;

procedure THV.SetSpeedStepDir(speedStep: Integer; direction: Boolean; ok: TCb; err: TCb; Sender:TObject = nil);
var dirOld:Boolean;
    stepsOld:Byte;
    cbOk, cbErr: PTCB;
begin
 if ((Self.direction = direction) and (Self.speedStep = speedStep) and (not Self.acquiring)) then
  begin
   if (Assigned(ok.callback)) then
     ok.callback(Self, ok.data);
   Exit();
  end;
 if ((Self.stolen) and (not Self.acquiring)) then
  begin
   writelog('LOKO '+Self.nazev+' ukradena, nenastavuji rychlost', WR_MESSAGE);
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;
 if ((not Self.acquired) and (not Self.acquiring)) then
  begin
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;

 dirOld := Self.direction;
 stepsOld := Self.speedStep;

 Self.slot.direction := direction;
 Self.slot.speed := speedStep;

 TrakceI.Callbacks(ok, err, cbOk, cbErr);
 TrakceI.Log(llCommands, 'Loko ' + Self.nazev + ': rychlostní stupeň: ' + IntToStr(speedStep) +
             ', směr: ' + IntToStr(PrevodySoustav.BoolToInt(direction)));

 try
   TrakceI.LocoSetSpeed(Self.adresa, Self.slot.speed, Self.direction,
                        TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
                        TTrakce.Callback(Self.TrakceCallbackErr, cbErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, cbErr);
     AppEvents.LogException(E, 'THV.SetSpeedDir');
    end;
 end;

 Self.SlotChanged(Sender, stepsOld <> speedStep, dirOld <> direction);
end;

procedure THV.SetSpeedStepDir(speedStep: Integer; direction: Boolean; Sender:TObject = nil);
begin
 Self.SetSpeedStepDir(speedStep, direction, TrakceI.Callback(), TrakceI.Callback(), Sender);
end;

procedure THV.SetSingleFunc(func:Integer; state:Boolean; ok: TCb; err: TCb; Sender:TObject = nil);
var cbOk, cbErr: PTCb;
begin
 if (Self.slotFunkce[func] = state) then
  begin
   if (Assigned(ok.callback)) then
     ok.callback(Self, ok.data);
   Exit();
  end;
 if ((Self.stolen) and (not Self.acquiring)) then
  begin
   writelog('LOKO ' + Self.nazev + ' ukradena, nenastavuji funkce', WR_MESSAGE);
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;
 if ((not Self.acquired) and (not Self.acquiring)) then
  begin
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;

 if (state) then
   Self.slot.functions := Self.slot.functions or (1 shl func)
 else
   Self.slot.functions := Self.slot.functions and (not (1 shl func));

 Self.stav.funkce[func] := state;
 TrakceI.Callbacks(ok, err, cbOk, cbErr);
 TrakceI.Log(llCommands, 'Loko ' + Self.nazev + ': F' + IntToStr(func) +
             ': ' + IntToStr(PrevodySoustav.BoolToInt(state)));

 try
   TrakceI.LocoSetSingleFunc(Self.adresa, func, Self.slot.functions,
                             TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
                             TTrakce.Callback(Self.TrakceCallbackErr, cbErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, cbErr);
     AppEvents.LogException(E, 'THV.SetSingleFunc');
    end;
 end;

 TCPRegulator.LokUpdateFunc(Self, Sender);
 RegCollector.LocoChanged(Sender, Self.adresa);
 Self.changed := true;
end;

procedure THV.StavFunctionsToSlotFunctions(ok: TCb; err: TCb; Sender: TObject = nil);
var i:Integer;
    funcMask:Cardinal;
    funcState:Cardinal;
begin
 if ((Self.stolen) and (not Self.acquiring)) then
  begin
   writelog('LOKO ' + Self.nazev + ' ukradena, nenastavuji funkce', WR_MESSAGE);
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;
 if ((not Self.acquired) and (not Self.acquiring)) then
  begin
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;

 funcMask := 0;
 funcState := 0;
 for i := 0 to _HV_FUNC_MAX do
  begin
   if (Self.stav.funkce[i]) then
     funcState := funcState or (1 shl i);
   if (Self.stav.funkce[i] <> Self.slotFunkce[i]) then
     funcMask := funcMask or (1 shl i);
  end;

 if (funcMask = 0) then
  begin
   if (Assigned(ok.callback)) then
     ok.callback(Self, ok.data);
   Exit();
  end;

 TrakceI.Log(llCommands, 'Loko ' + Self.nazev + ': změna více funkcí');
 Self.slot.functions := funcState;

 try
   TrakceI.LocoSetFunc(Self.adresa, funcMask, funcState, ok, err);
 except
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
 end;

 TCPRegulator.LokUpdateFunc(Self, Sender);
 RegCollector.LocoChanged(Sender, Self.adresa);
 Self.changed := true;
end;

procedure THV.EmergencyStop(ok: TCb; err: TCb; Sender:TObject = nil);
var cbOk, cbErr: PTCb;
begin
 if (not Self.acquired) then
  begin
   if (Assigned(err.callback)) then
     err.callback(Self, err.data);
   Exit();
  end;

 Self.Slot.speed := 0;
 TrakceI.Callbacks(ok, err, cbOk, cbErr);

 try
   TrakceI.LocoEmergencyStop(Self.adresa,
                             TTrakce.Callback(Self.TrakceCallbackOk, cbOk),
                             TTrakce.Callback(Self.TrakceCallbackErr, cbErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, cbErr);
     AppEvents.LogException(E, 'THV.EmergencyStop');
    end;
 end;

 Self.SlotChanged(Sender, true, false);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.CSReset();
begin
 Self.stav.acquiring := false;
 Self.stav.acquired := false;
 Self.stav.stolen := false;
 Self.stav.pom := TPomStatus.released;
 Self.stav.trakceError := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.TrakceCallbackOk(Sender:TObject; data:Pointer);
begin
 Self.TrakceCallbackCallEv(data);

 if (not Self.stav.trakceError) then Exit();
 Self.stav.trakceError := false;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
end;

procedure THV.TrakceCallbackErr(Sender:TObject; data:Pointer);
begin
 Self.TrakceCallbackCallEv(data);

 if (Self.stav.trakceError) then Exit();
 Self.stav.trakceError := true;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
end;

procedure THV.TrakceCallbackCallEv(cb:PTCb);
begin
 if (cb <> nil) then
  begin
   if (Assigned(cb.callback)) then
     cb.callback(Self, cb.data);
   if (Assigned(cb.other)) then
     FreeMem(cb.other);
   FreeMem(cb);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetSlotFunkce():TFunkce;
var i: Integer;
    functions: Cardinal;
begin
 functions := Self.Slot.functions;
 for i := 0 to _HV_FUNC_MAX do
  begin
   Result[i] := (functions AND $1 > 0);
   functions := (functions shr 1);
  end;
end;

function THV.GetRealSpeed():Integer;
begin
 Result := TrakceI.GetStepSpeed(Self.slot.speed);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.SlotChanged(Sender:TObject; speedChanged:boolean; dirChanged:boolean);
begin
 TCPRegulator.LokUpdateSpeed(Self, Sender);
 RegCollector.LocoChanged(Sender, Self.adresa);
 Self.changed := true;

 if ((dirChanged) and (Self.souprava > -1)) then
   if ((Sender <> Soupravy[Self.souprava]) and (Soupravy[Self.souprava] <> nil)) then
     Soupravy[Self.souprava].LokDirChanged();
     // Soupravy[HV.Stav.souprava] <> nil muze nastat pri aktualizaci HV na souprave,
     // coz se dede prave tady
end;

////////////////////////////////////////////////////////////////////////////////
// ACQUIRING
////////////////////////////////////////////////////////////////////////////////

procedure THV.TrakceAcquire(ok: TCb; err: TCb);
begin
 TrakceI.Log(llCommands, 'PUT: Loco Acquire: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.RecordUseNow();
 Self.stav.acquiring := true;
 Self.acquiredOk := ok;
 Self.acquiredErr := err;
 Self.changed := true;

 try
   TrakceI.LocoAcquire(Self.adresa, Self.TrakceAcquired, TTrakce.Callback(Self.TrakceAcquiredErr));
 except
   Self.TrakceAcquiredErr(Self, nil);
 end;
end;

procedure THV.TrakceAcquired(Sender: TObject; LocoInfo: TTrkLocoInfo);
var direction:Boolean;
    speedStep:Integer;
begin
 Self.slot := LocoInfo;
 Self.changed := true;

 // pokud ma souprava jasne dany smer, nastavime ho
 // podminka na sipky je tu kvuli prebirani z RUCniho rizeni z XpressNETu
 if ((Self.souprava > -1) and (not Self.ruc) and
     (Soupravy[Self.souprava].sdata.smer_L xor Soupravy[Self.souprava].sdata.smer_S)) then
  begin
   // souprava ma zadany prave jeden smer
   direction := ((Soupravy[Self.souprava].smer = THVStanoviste.sudy) xor (Self.stav.StanovisteA = THVStanoviste.sudy));
   speedStep := TrakceI.SpeedStep(Soupravy[Self.souprava].rychlost);
  end else begin
   direction := Self.slot.direction;
   if (Self.stolen) then
     speedStep := Self.slot.speed
   else
     speedStep := 0;
  end;

 // Vzdy nastavit smer, protoze tim prevezmeme loko z rizeni jineho ovladace
 Self.SetSpeedStepDir(speedStep, direction,
                      TTrakce.Callback(Self.TrakceAcquiredDirection),
                      TTrakce.Callback(Self.TrakceAcquiredErr));
end;

procedure THV.TrakceAcquiredDirection(Sender: TObject; data: Pointer);
begin
 // Set functions as we wish
 Self.stav.stolen := false;
 Self.StavFunctionsToSlotFunctions(TTrakce.Callback(Self.TrakceAcquiredFunctionsSet),
                                   TTrakce.Callback(Self.TrakceAcquiredErr));
end;

procedure THV.TrakceAcquiredFunctionsSet(Sender:TObject; Data:Pointer);
begin
 Self.Stav.ruc := (RegCollector.IsLoko(Self)) or (Self.ruc);
 Self.changed := true;

 if (Self.Stav.ruc) then
  begin
   // manual control
   Self.SetPom(TPomStatus.released, TTrakce.Callback(Self.TrakceAcquiredPOMSet), TTrakce.Callback(Self.TrakceAcquiredErr));
  end else begin
   // automatic control
   Self.SetPom(TPomStatus.pc, TTrakce.Callback(Self.TrakceAcquiredPOMSet), TTrakce.Callback(Self.TrakceAcquiredErr));
  end;
end;

procedure THV.TrakceAcquiredPOMSet(Sender:TObject; Data:Pointer);
var reg:THVRegulator;
    state:string;
begin
 // Everything done
 TrakceI.Log(llCommands, 'Loco Fully Acquired: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.stav.acquired := true;
 Self.stav.acquiring := false;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);

 if (Self.souprava > -1) then
   Blky.ChangeUsekWithSpr(Self.souprava);

 Self.UpdateRuc();

 // odesleme do regulatoru info o uspesne autorizaci
 // to je dobre tehdy, kdyz je loko prebirano z centraly
 if (Self.ruc) then
   state := 'total'
 else
   state := 'ok';
 for reg in Self.Stav.regulators do
   ORTCPServer.SendLn(reg.conn, '-;LOK;'+IntToStr(Self.adresa)+';AUTH;'+state+';{'+Self.GetPanelLokString()+'}');

 if (Assigned(Self.acquiredOk.callback)) then
   Self.acquiredOk.callback(Self, Self.acquiredOk.data);
end;

procedure THV.TrakceAcquiredErr(Sender:TObject; data:Pointer);
begin
 TrakceI.Log(llCommands, 'ERR: Loco Not Acquired: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.stav.acquiring := false;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
 if (Assigned(Self.acquiredErr.callback)) then
   Self.acquiredErr.callback(Self, Self.acquiredErr.data);
end;

////////////////////////////////////////////////////////////////////////////////
// RELEASING
////////////////////////////////////////////////////////////////////////////////

procedure THV.TrakceRelease(ok: TCb);
begin
 TrakceI.Log(llCommands, 'PUT: Loco Release: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.releasedOk := ok;
 Self.Stav.ruc := false;
 Self.RecordUseNow();
 Self.changed := true;

 if (Self.pom <> TPomStatus.released) then
   Self.SetPom(TPomStatus.released, TTrakce.Callback(Self.TrakceReleasedPOM), TTrakce.Callback(Self.TrakceReleasedPOM))
 else
   Self.TrakceReleasedPOM(Self, nil);
end;

procedure THV.TrakceReleasedPOM(Sender:TObject; data:Pointer);
begin
 // POM done (we do not care is successfully or unsuccessfully)
 try
   TrakceI.LocoRelease(Self.adresa, TTrakce.Callback(Self.TrakceReleased));
 except
   Self.TrakceReleased(Self, nil);
 end;
end;

procedure THV.TrakceReleased(Sender:TObject; data:Pointer);
begin
 TrakceI.Log(llCommands, 'Loco Successfully Released: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.stav.acquired := false;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
 if (Assigned(Self.releasedOk.callback)) then
   Self.releasedOk.callback(Self, Self.releasedOk.data);
end;

////////////////////////////////////////////////////////////////////////////////
// POM

procedure THV.SetPom(pom:TPomStatus; ok: TCb; err: TCb);
var toProgram:TList<THVPomCV>;
begin
 Self.pomOk := ok;
 Self.pomErr := err;
 Self.pomTarget := pom;
 Self.stav.pom := TPomStatus.progr;
 Self.changed := true;

 if (pom = TPomStatus.pc) then
   toProgram := Self.data.POMtake
 else if (pom = TPomStatus.released) then
   toProgram := Self.data.POMrelease
 else
   raise Exception.Create('Invalid POM!');

 TrakceI.POMWriteCVs(Self.adresa, toProgram,
                     TTrakce.Callback(Self.TrakcePOMOK), TTrakce.Callback(Self.TrakcePOMErr));
end;

procedure THV.TrakcePOMOK(Sender:TObject; data:Pointer);
begin
 if (Self.stav.trakceError) then
   Self.stav.trakceError := false;
 Self.stav.pom := Self.pomTarget;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
 if (Assigned(Self.pomOk.callback)) then
   Self.pomOk.callback(Self, Self.pomOk.data);
end;

procedure THV.TrakcePOMErr(Sender:TObject; data:Pointer);
begin
 Self.stav.pom := TPomStatus.error;
 Self.stav.trakceError := true;
 Self.changed := true;
 RegCollector.LocoChanged(Self, Self.adresa);
 if (Assigned(Self.pomErr.callback)) then
   Self.pomOk.callback(Self, Self.pomErr.data);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.TrakceStolen();
begin
 // tato situace muze nastat, kdyz odhlasime HV a pak si ho vezme Rocomouse
 if (not Self.acquired) then
   Exit();

 Self.stav.acquired := false;
 Self.stav.stolen := true;
 RegCollector.LocoChanged(Self, Self.adresa);

 TCPRegulator.LokStolen(Self);
 if (Self.souprava > -1) then
   Blky.ChangeUsekWithSpr(Self.souprava);
 Self.UpdateRuc();

 Self.SetPom(TPomStatus.released, TTrakce.Callback(), TTrakce.Callback());
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetStACurrentDirection():Boolean;
begin
 Result := Self.direction xor PrevodySoustav.IntToBool(Integer(Self.Stav.StanovisteA));
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
