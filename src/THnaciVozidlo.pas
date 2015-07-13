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
}

interface

uses Trakce, TBlok, Classes, StrUtils, SysUtils, TOblRizeni, RPConst,
      Generics.Collections, IdContext, IniFiles;

const
  _HV_FUNC_MAX       = 12;   // maximalni funkcni cislo; funkce zacinaji na cisle 0
  _TOKEN_TIMEOUT_MIN = 3;    // delka platnosti tokenu pro autorizaci hnaciho vozidla v minutach
  _TOKEN_LEN         = 24;   // delka autorizacniho tokenu pro prevzeti hnaciho vozidla

  _LOK_VERSION_SAVE = '2.0';

type
  // trida hnaciho vozidla
  THVClass = (parni = 0, diesel = 1, motor = 2, elektro = 3);

  // mod posilani dat hnaciho vozidla klientovi
  // full: s POM
  TLokStringMode = (normal = 0, full = 1);

  THVPomCV = record                                 // jeden zaznam POM se sklada z
    cv:Word;                                           // oznaceni CV a
    data:Byte;                                         // dat, ktera se maji do CV zapsat.
  end;

  THVData = record                                  // data hnaciho vozidla (nacitaji se ze souboru, program sam je vicemene nemeni)
   Nazev:string;                                       // nazev HV
   Majitel:string;                                     // majitel HV
   Oznaceni:string;                                    // oznaceni HV
   Poznamka:String;                                    // poznamka k HV
   Trida:THVClass;                                     // trida hnaciho vozidla - parni, diesel, motor, elektro

   POMtake:TList<THVPomCV>;                            // seznam POM pri prevzeti do automatu
   POMrelease:TList<THVPomCV>;                         // seznam POM pri uvolneni to rucniho rizeni

   funcVyznam:array[0.._HV_FUNC_MAX] of string;        // seznam popisu funkci hnaciho vozidla
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
   regulators:TList<THVRegulator>;                     // seznam regulatoru -- kleintu
   tokens:TList<THVToken>;                             // aktualni seznam tokenu -- jedno HV muze mit prideleno vice tokenu
   ruc:boolean;                                        // jestli je hnaciho vozidlo v rucnim rizeni
  end;

  THV = class                                       // HNACI VOZIDLO
   private const

   private

     fadresa:Word;                                     // adresa je read-only !

     procedure LoadFromIni(ini:TMemIniFile; section:string);
                                                       // nacte data ze souboru
     procedure SetRuc(state:boolean);                  // nastavi rucni rizeni

   public

    index:Word;                                        // index v seznamu vsech hnacich vozidel
    Data:THVData;                                      // data HV (viz vyse)
    Stav:THVStav;                                      // stav HV (viz vyse)
    Slot:TSlot;                                        // slot HV (viz trakcni tridy)
    changed:boolean;                                   // jestli se zmenil stav HV tak, ze je potraba aktualizaovat tabulku ve F_Main

     constructor Create(ini:TMemIniFile; section:string); overload;
     constructor Create(adresa:Word; data:THVData; stav:THVStav); overload;
     constructor Create(panel_str:string; Sender:TOR); overload;
     destructor Destroy(); override;

     procedure SaveToFile(const filename:string);

     procedure UpdateFromPanelString(data:string);     // nacteni informaci o HV z klienta

     procedure RemoveStats();                          // smaz statistiky najeto

     function PredejStanici(st:TOR):Integer;           // predej HV jine stanici
     function GetPanelLokString(mode:TLokStringMode = normal):string; // vrati HV ve standardnim formatu pro klienta
     procedure UpdateRuc(send_remove:boolean = true);  // aktualizuje informaci o rucnim rizeni do panelu (cerny text na bilem pozadi dole na panelu)
     procedure RemoveRegulator(conn:TIDContext);       // smaze regulator -- klienta
     function IsReg(conn:TIdContext):boolean;          // je na tomto HV tento regulator ?

     function GetToken():string;                       // ziskani tokenu
     function IsToken(str:string):boolean;             // overeni tokenu
     procedure RemoveToken(token:string);              // smazani tokenu
     procedure UpdateTokenTimeout();                   // aktualizace vyprseni platnosti tokenu, melo by byt volano periodicky

     property adresa:Word read fadresa;                // adresa HV
     property ruc:boolean read Stav.ruc write SetRuc;  // rucni rizeni HV

  end;//THV


implementation

uses Main, Prevody, TOblsRizeni, THVDatabase, SprDb, DataHV, Regulator, TBloky,
      RegulatorTCP;

////////////////////////////////////////////////////////////////////////////////

constructor THV.Create(ini:TMemIniFile; section:string);
begin
 inherited Create();

 Self.Stav.regulators := TList<THVRegulator>.Create();
 Self.Stav.tokens     := TList<THVToken>.Create();

 Self.Stav.souprava := -1;
 Self.Stav.stanice  := nil;

 Self.data.POMtake    := TList<THVPomCV>.Create;
 Self.data.POMrelease := TList<THVPomCV>.Create;

 try
   Self.LoadFromIni(ini, section);
 except
   on E:Exception do
     raise Exception.Create('Chyba pri nacitani sekce '+section+' - '+E.Message);
 end;
end;//ctor

constructor THV.Create(adresa:Word; data:THVData; stav:THVStav);
begin
 inherited Create();

 Self.fadresa := adresa;
 Self.Data    := data;
 Self.Stav    := stav;

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

 Self.data.POMtake    := TList<THVPomCV>.Create;
 Self.data.POMrelease := TList<THVPomCV>.Create;

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

procedure THV.LoadFromIni(ini:TMemIniFile; section:string);
var strs, strs2:TStrings;
    i:Integer;
    stanice:Integer;
    pomCV:THVPomCV;
    str:string;
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

   // stav funkci
   str := ini.ReadString(section, 'stav_funkci', '');
   for i := 0 to _HV_FUNC_MAX do
     Self.Stav.funkce[i] := ((i < Length(str)) and PrevodySoustav.StrToBool(str[i+1]));

   // POM pri prebirani : (cv,data)(cv,data)(...)...
   str := ini.ReadString(section, 'pom_take', '');
   Self.Data.POMtake.Clear();
   strs.Clear();
   ExtractStringsEx([')'], ['('], str, strs);
   for i := 0 to strs.Count-1 do
    begin
     strs2.Clear();
     ExtractStringsEx([','], [], strs[i], strs2);
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
    end;//for i

   // POM pri uvolneni : (cv,data)(cv,data)(...)...
   str := ini.ReadString(section, 'pom_release', '');
   Self.Data.POMrelease.Clear();
   strs.Clear();
   ExtractStringsEx([')'], ['('], str, strs);
   for i := 0 to strs.Count-1 do
    begin
     strs2.Clear();
     ExtractStringsEx([','], [], strs[i], strs2);
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
    end;//for i

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
    end;//for i

  except
   strs.Free();
   strs2.Free();
   raise;
  end;

 strs.Free();
 strs2.Free();
end;//procedure

procedure THV.SaveToFile(const filename:string);
var ini:TMemIniFile;
    addr, str:string;
    i:Integer;
begin
 ini := TMemIniFile.Create(filename);

 try
   ini.WriteString('global', 'version', _LOK_VERSION_SAVE);

   addr := IntToStr(Self.adresa);
   ini.WriteString(addr, 'nazev', Self.Data.Nazev);
   ini.WriteString(addr, 'majitel', Self.Data.Majitel);
   ini.WriteString(addr, 'oznaceni', Self.Data.Oznaceni);
   ini.WriteString(addr, 'poznamka', Self.Data.Poznamka);
   ini.WriteInteger(addr, 'trida', Integer(Self.Data.Trida));

   if (Self.Stav.stanice <> nil) then
     ini.WriteString(addr, 'stanice', Self.Stav.stanice.id)
   else
     ini.WriteString(addr, 'stanice', '');

   ini.WriteFloat(addr, 'najeto_vpred_metru', Self.Stav.najeto_vpred.Metru);
   ini.WriteInteger(addr, 'najeto_vpred_bloku', Self.Stav.najeto_vpred.Bloku);

   ini.WriteFloat(addr, 'najeto_vzad_metru', Self.Stav.najeto_vzad.Metru);
   ini.WriteInteger(addr, 'najeto_vzad_bloku', Self.Stav.najeto_vzad.Bloku);

   ini.WriteInteger(addr, 'stanoviste_a', Integer(Self.Stav.StanovisteA));

   // stav funkci
   str := '';
   for i := 0 to _HV_FUNC_MAX do
    begin
     if (Self.Stav.funkce[i]) then
       str := str + '1'
     else
       str := str + '0';
    end;
   ini.WriteString(addr, 'stav_funkci', str);

   // POM pri prebirani
   str := '';
   for i := 0 to Self.Data.POMtake.Count-1 do
    str := str + '(' + IntToStr(Self.Data.POMtake[i].cv) + ',' + IntToStr(Self.Data.POMtake[i].data) + ')';
   ini.WriteString(addr, 'pom_take', str);

   // POM pri uvolneni
   str := '';
   for i := 0 to Self.Data.POMrelease.Count-1 do
    str := str + '(' + IntToStr(Self.Data.POMrelease[i].cv) + ',' + IntToStr(Self.Data.POMrelease[i].data) + ')';
   ini.WriteString(addr, 'pom_release', str);

   // vyznam funkci
   str := '';
   for i := 0 to _HV_FUNC_MAX do
     str := str + Self.Data.funcVyznam[i] + ';';
   ini.WriteString(addr, 'func_vyznam', str);

 except
   ini.UpdateFile();
   ini.Free();
   raise;
 end;

 ini.UpdateFile();
 ini.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THV.RemoveStats();
begin
 Self.Stav.najeto_vpred.Metru := 0;
 Self.Stav.najeto_vpred.Bloku := 0;
 Self.Stav.najeto_vzad.Metru  := 0;
 Self.Stav.najeto_vzad.Bloku  := 0;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function THV.GetPanelLokString(mode:TLokStringMode = normal):string;
var i:Integer;
    func:TFunkce;
    pomCV:THVPOMCv;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce|rychlost_stupne|
 //   rychlost_kmph|smer|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
 //   {vyznam-F0;vyznam-F1;...}|

 // souprava je bud cislo soupravy, nebo znak '-'
 Result := Self.Data.Nazev + '|' + Self.Data.Majitel + '|' + Self.Data.Oznaceni + '|{' + Self.Data.Poznamka + '}|' +
           IntToStr(Self.adresa) + '|' + IntToStr(Integer(Self.Data.Trida)) + '|';

 if (Self.Stav.souprava > -1) then
  Result := Result + Soupravy.GetSprNameByIndex(Self.Stav.souprava) + '|'
 else
  Result := Result + '-|';

 Result := Result + IntToStr(Integer(Self.Stav.StanovisteA)) + '|';

 if (Self.Slot.prevzato) then func := Self.Slot.funkce else func := Self.Stav.funkce;

 for i := 0 to _HV_FUNC_MAX do
  begin
   if (func[i]) then
     Result := Result + '1'
   else
     Result := Result + '0';
  end;

 Result := Result + '|' + IntToStr(Self.Slot.speed) + '|' +
           IntToStr(TrkSystem.GetStepSpeed(Self.Slot.speed)) + '|' + IntToStr(Self.Slot.smer) + '|';

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

 Result := Result + '|{';

 // vyznamy funkci
 for i := 0 to _HV_FUNC_MAX do
  begin
   if (Self.Data.funcVyznam[i] <> '') then
     Result := Result + '{' + Self.Data.funcVyznam[i] + '};'
   else
     Result := Result + ';';
  end;

 Result := Result + '}|';
end;//function

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
end;//function

////////////////////////////////////////////////////////////////////////////////

// format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|-|stanovisteA|funkce|rychlost_stupne|
//   rychlost_kmph|smer|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
//   {vyznam-F0;vyznam-F1;...}|
// na miste znaku - obvykle byva souprava, pri nacitani hnaciho vozidla tuto polozku nemenime -> je tam pomlcka
procedure THV.UpdateFromPanelString(data:string);
var str, str2, str3:TStrings;
    i:Integer;
    pomCv:THVPomCv;
    tmp:string;
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

  for i := 0 to Length(str[8])-1 do
   begin
    case (str[8][i+1]) of
     '0' : Self.Stav.funkce[i] := false;
     '1' : Self.Stav.funkce[i] := true;
    end;
   end;

  if (str.Count > 12) then
   begin
     // pom-take
     str2.Clear();
     Self.Data.POMtake.Clear();
     ExtractStringsEx([']'] , ['['], str[12], str2);
     for tmp in str2 do
      begin
       str3.Clear();
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv   := StrToInt(str3[0]);
       pomCV.data := StrToInt(str3[1]);
       Self.Data.POMtake.Add(pomCV);
      end;
   end;

  if (str.Count > 13) then
   begin
     // pom-release
     str2.Clear();
     Self.Data.POMrelease.Clear();
     ExtractStringsEx([']'] , ['['], str[13], str2);
     for tmp in str2 do
      begin
       str3.Clear();
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv   := StrToInt(str3[0]);
       pomCV.data := StrToInt(str3[1]);
       Self.Data.POMrelease.Add(pomCV);
      end;
   end;

  if (str.Count > 14) then
   begin
    // vyznam funkci
    str2.Clear();
    ExtractStringsEx([';'], [], str[14], str2);
    for i := 0 to _HV_FUNC_MAX do
      if (i < str2.Count) then
       Self.Data.funcVyznam[i] := str2[i]
      else
       Self.Data.funcVyznam[i] := '';
   end else begin
    for i := 0 to _HV_FUNC_MAX do
      Self.Data.funcVyznam[i] := '';
   end;

 except
  on e:Exception do
   begin
    raise Exception.Create('Chyba pøi parsování dat hnacího vozidla - '+e.Message);
    Exit();
   end;
 end;

 Self.changed := true;

 if (Self.Stav.souprava > -1) then
   Blky.ChangeSprToTrat(Self.Stav.souprava);

 str.Free();
 str2.Free();
 str3.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THV.UpdateRuc(send_remove:boolean = true);
var spr:string;
begin
 if (Self.Stav.souprava > -1) then
  spr := Soupravy.soupravy[Self.Stav.souprava].nazev
 else
  spr := '-';

 if (Self.Slot.stolen) then
  begin
   // loko ukradeno Rocomysi
   Self.Stav.stanice.BroadcastData('RUC;'+IntToStr(Self.adresa)+';MM. '+IntToStr(Self.adresa)+' ('+spr+')');
   Exit();
  end else begin
   if (Self.ruc) then
     Self.Stav.stanice.BroadcastData('RUC;'+IntToStr(Self.adresa)+';RUÈ. '+IntToStr(Self.adresa)+' ('+spr+')')
   else
     // loko neni v rucnim rizeni -> oznamit klientovi
     if (send_remove) then Self.Stav.stanice.BroadcastData('RUC-RM;'+IntToStr(Self.adresa));
  end;

end;//procedure

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
     if (Self.Stav.regulators.Count = 0) then Self.ruc := false;

     Exit();
    end;
end;//procedure

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
end;//function

function THV.IsToken(str:string):boolean;
var i:Integer;
begin
 for i := 0 to Self.Stav.tokens.Count-1 do
  if (Self.Stav.tokens[i].token = str) then
   Exit(true);
 Result := false;
end;//function

procedure THV.RemoveToken(token:string);
var i:Integer;
begin
 for i := 0 to Self.Stav.tokens.Count-1 do
  if (Self.Stav.tokens[i].token = token) then
   begin
    Self.Stav.tokens.Delete(i);
    Exit();
   end;
end;//procedure

procedure THV.UpdateTokenTimeout();
var i:Integer;
begin
 for i := Self.Stav.tokens.Count-1 downto 0 do
   if (Now > Self.Stav.tokens[i].timeout) then
     Self.Stav.tokens.Delete(i);
end;//procedure

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
   if ((Self.Slot.prevzato) and (Self.Slot.pom <> TPomStatus.released)) then // neprevzatym vozidlum je POM nastaven pri prebirani; prebirni vozidel ale neni nase starost, to si resi volajici fuknce
     TrkSystem.POMWriteCVs(Self, Self, Self.Data.POMrelease, TPomStatus.released);
  end else begin
   // loko je vyjmuto z rucniho rizeni

   if (Self.Stav.souprava > -1) then
    Soupravy.soupravy[Self.Stav.souprava].rychlost := Soupravy.soupravy[Self.Stav.souprava].rychlost;    // tento prikaz nastavi rychlost

   if (Self.Stav.souprava < 0) then
    begin
     // loko neni na souprave, ani v rucnim regulatoru -> odhlasit
     if ((Self.Stav.regulators.Count = 0) and (not RegCollector.IsLoko(Self))) then
       TrkSystem.OdhlasitLoko(Self);
    end;

   // POM automatu
   if (Self.Slot.pom <> TPomStatus.pc) then
     TrkSystem.POMWriteCVs(Self, Self, Self.Data.POMtake, TPomStatus.pc);
  end;

 if (RegCollector.IsLoko(Self)) then
  RegCollector.UpdateElements(Self, Self.Slot.adresa);
 TCPRegulator.LokUpdateRuc(Self);

 // aktualizace informaci do panelu
 Self.UpdateRuc();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function THV.IsReg(conn:TIdContext):boolean;
var i:Integer;
begin
 for i := 0 to Self.Stav.regulators.Count-1 do
   if (Self.Stav.regulators[i].conn = conn) then
     Exit(true);
 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
