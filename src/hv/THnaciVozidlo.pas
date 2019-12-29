﻿unit THnaciVozidlo;

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
  end;

  THV = class                                       // HNACI VOZIDLO
   private const

   private

     fadresa:Word;                                     // adresa je read-only !
     m_funcDict:TDictionary<string, Integer>;          // mapovani vyznamu funkci na cisla funkci

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
     procedure SlotChanged(Sender:TObject; speedChanged:boolean; dirChanged:boolean);

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

     procedure SetFunction(func: Integer; state: Boolean; Sender: TObject = nil);
     procedure SetSlotFunction(func: Integer; state: Boolean; Sender: TObject = nil);

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

 Self.data.POMtake    := TList<THVPomCV>.Create;
 Self.data.POMrelease := TList<THVPomCV>.Create;

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

// format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|-|stanovisteA|funkce|rychlost_stupne|
//   rychlost_kmph|smer|orid|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
//   {vyznam-F0;vyznam-F1;...}|typy_funkci
// na miste znaku - obvykle byva souprava, pri nacitani hnaciho vozidla tuto polozku nemenime -> je tam pomlcka
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
  spr := Soupravy.soupravy[Self.Stav.souprava].nazev
 else
  spr := '-';

 if (Self.acquired) then
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
   if ((Self.acquired) and (Self.pom <> TPomStatus.released)) then // neprevzatym vozidlum je POM nastaven pri prebirani; prebirni vozidel ale neni nase starost, to si resi volajici fuknce
     TrakceI.POMWriteCVs(Self, Self, Self.Data.POMrelease, TPomStatus.released);
  end else begin
   // loko je vyjmuto z rucniho rizeni

   if (Self.Stav.souprava > -1) then
    begin
     // POM automatu
     if (Self.pom <> TPomStatus.pc) then
       TrakceI.POMWriteCVs(Self, Self, Self.Data.POMtake, TPomStatus.pc);

     Soupravy.soupravy[Self.Stav.souprava].rychlost := Soupravy.soupravy[Self.Stav.souprava].rychlost;    // tento prikaz nastavi rychlost
    end else begin
     // loko neni na souprave -> zkusit odhlasit
     Self.CheckRelease();
    end;
  end;

 if (RegCollector.IsLoko(Self)) then
   RegCollector.UpdateElements(Self, Self.adresa);
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

 if ((not Self.acquired) or (Self.stolen)) then
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
       noveFunkce[i] := Self.slotFunkce[i];
    end;
//   TrkSystem.LokSetFunc(Self, Self, noveFunkce); TODO
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
begin
 // TODO: use callbacks
 if ((Self.direction = direction) and (Self.speedStep = speedStep)) then Exit();
 if (not Self.acquired) then Exit();

 dirOld := Self.direction;
 stepsOld := Self.speedStep;

 Self.slot.direction := direction;
 Self.slot.speed := speedStep;

 if (Self.stolen) then
  begin
   writelog('LOKO '+Self.nazev+' ukradena, nenastavuji rychlost', WR_MESSAGE);
   Exit();
  end;

 try
   // TODO: handle ok and err callbacks and call it in custom callbacks

   TrakceI.LocoSetSpeed(Self.adresa, Self.slot.speed, Self.direction,
                        TTrakce.Callback(Self.TrakceCallbackOk),
                        TTrakce.Callback(Self.TrakceCallbackErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, nil);
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
begin
 if (Self.slotFunkce[func] = state) then Exit();
 if (not Self.acquired) then Exit();

 if (Self.stolen) then
  begin
   writelog('LOKO ' + Self.nazev + ' ukradena, nenastavuji funkce', WR_MESSAGE);
   Exit();
  end;

 if (state) then
  Self.slot.functions := Self.slot.functions or (1 shl func)
 else
  Self.slot.functions := Self.slot.functions and (not (1 shl func));

 try
   TrakceI.LocoSetSingleFunc(Self.adresa, func, Self.slot.functions,
                             TTrakce.Callback(Self.TrakceCallbackOk),
                             TTrakce.Callback(Self.TrakceCallbackErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, nil);
     AppEvents.LogException(E, 'THV.SetSingleFunc');
    end;
 end;

 TCPRegulator.LokUpdateFunc(Self, Sender);
 RegCollector.UpdateElements(Sender, Self.adresa);
 Self.changed := true;
end;

procedure THV.EmergencyStop(ok: TCb; err: TCb; Sender:TObject = nil);
begin
 if (not Self.acquired) then Exit();

 Self.Slot.speed := 0;

 try
   TrakceI.LocoEmergencyStop(Self.adresa,
                             TTrakce.Callback(Self.TrakceCallbackOk),
                             TTrakce.Callback(Self.TrakceCallbackErr));
 except
   on E:Exception do
    begin
     Self.TrakceCallbackErr(Self, nil);
     AppEvents.LogException(E, 'THV.EmergencyStop');
    end;
 end;

 Self.SlotChanged(Sender, true, false);
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.CSReset();
begin
 Self.stav.acquired := false;
 Self.stav.stolen := false;
 Self.stav.pom := TPomStatus.released;
 Self.stav.trakceError := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THV.TrakceCallbackOk(Sender:TObject; data:Pointer);
begin
 if (not Self.stav.trakceError) then Exit();
 Self.stav.trakceError := false;
 Self.changed := true;
end;

procedure THV.TrakceCallbackErr(Sender:TObject; data:Pointer);
begin
 if (Self.stav.trakceError) then Exit();
 Self.stav.trakceError := true;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetSlotFunkce():TFunkce;
var i: Integer;
    functions: Cardinal;
begin
 functions := Self.Slot.functions;
 for i := 0 to 31 do
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
 RegCollector.UpdateElements(Sender, Self.adresa);
 Self.changed := true;

 if ((dirChanged) and (Self.souprava > -1)) then
   if ((Sender <> Soupravy[Self.souprava]) and (Soupravy[Self.souprava] <> nil)) then
     Soupravy[Self.souprava].LokDirChanged();
     // Soupravy[HV.Stav.souprava] <> nil muze nastat pri aktualizaci HV na souprave,
     // coz se dede prave tady
end;

procedure THV.TrakceAcquire(ok: TCb; err: TCb);
begin
 TrakceI.Log(llCommands, 'PUT: Loco Acquire: '+Self.Nazev+' ('+IntToStr(Self.Adresa)+')');
 Self.RecordUseNow();

 try
//   TrakceI.LocoAcquire(Self.adresa);
 except
   on E:Exception do
    begin

    end;
 end;
end;

procedure THV.TrakceRelease(ok: TCb);
begin

end;

procedure THV.SetFunction(func: Integer; state: Boolean; Sender: TObject = nil);
begin

end;

procedure THV.SetSlotFunction(func: Integer; state: Boolean; Sender: TObject = nil);
begin

end;

////////////////////////////////////////////////////////////////////////////////

function THV.GetStACurrentDirection():Boolean;
begin
 Result := Self.direction xor PrevodySoustav.IntToBool(Integer(Self.Stav.StanovisteA));
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
