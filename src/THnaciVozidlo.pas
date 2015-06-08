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
      Generics.Collections, IdContext;

const
  _HV_FUNC_MAX       = 12;   // maximalni funkcni cislo; funkce zacinaji na cisle 0
  _TOKEN_TIMEOUT_MIN = 3;    // delka platnosti tokenu pro autorizaci hnaciho vozidla v minutach
  _TOKEN_LEN         = 24;   // delka autorizacniho tokenu pro prevzeti hnaciho vozidla

type
  // trida hnaciho vozidla
  THVClass = (parni = 0, diesel = 1, motor = 2, elektro = 3);

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

   POMtake : TList<THVPomCV>;                          // seznam POM pri prevzeti do automatu
   POMrelease : TList<THVPomCV>;                       // seznam POM pri uvolneni to rucniho rizeni
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

     function LoadFromFile(const filename:string):Byte;// nacte data ze souboru
     procedure SetRuc(state:boolean);                  // nastavi rucni rizeni

   public

    index:Word;                                        // index v seznamu vsech hnacich vozidel
    Data:THVData;                                      // data HV (viz vyse)
    Stav:THVStav;                                      // stav HV (viz vyse)
    Slot:TSlot;                                        // slot HV (viz trakcni tridy)
    changed:boolean;                                   // jestli se zmenil stav HV tak, ze je potraba aktualizaovat tabulku ve F_Main

     constructor Create(const filename:string); overload;
     constructor Create(adresa:Word; data:THVData; stav:THVStav); overload;
     constructor Create(panel_str:string; Sender:TOR); overload;
     destructor Destroy(); override;

     function SaveToFile(const filename:string):Byte;

     procedure UpdateFromPanelString(data:string);     // nacteni informaci o HV z klienta

     procedure RemoveStats();                          // smaz statistiky najeto

     function PredejStanici(st:TOR):Integer;           // predej HV jine stanici
     function GetPanelLokString():string;              // vrati HV ve standardnim formatu pro klienta
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

constructor THV.Create(const filename:string);
begin
 inherited Create();

 Self.Stav.regulators := TList<THVRegulator>.Create();
 Self.Stav.tokens     := TList<THVToken>.Create();

 Self.Stav.souprava := -1;
 Self.Stav.stanice  := nil;

 Self.data.POMtake    := TList<THVPomCV>.Create;
 Self.data.POMrelease := TList<THVPomCV>.Create;

 if (Self.LoadFromFile(filename) <> 0) then
   raise Exception.Create('Chyba pri nacitani souboru');
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

function THV.LoadFromFile(const filename:string):Byte;
const
    _RADEK_CNT = 4;
var Soubor:TextFile;
    radek:array [0.._RADEK_CNT-1] of string;
    Pole_dat, data2:TStrings;
    i:Integer;
    stanice:Integer;
    pomCV:THVPomCV;
begin
  try
    AssignFile(Soubor, filename);
    Reset(Soubor);
    for i := 0 to _RADEK_CNT-1 do radek[i] := '';
    i := 0;
    while ((not Eof(Soubor)) and (i < _RADEK_CNT)) do
     begin
      Readln(Soubor, Radek[i]);
      Inc(i);
     end;
  except
    Result := 3;
    try
      CloseFile(Soubor);
    except

    end;
    Exit;
  end;

  CloseFile(Soubor);

  Pole_dat := TStringList.Create;
  data2    := TStringList.Create;

  try
    // na prvnim radku jsou obecna data
    ExtractStringsEx([';'], Radek[0], pole_dat);

    if (Pole_dat.Count < 16) then Exit(1);

    Self.Data.Nazev    := Pole_dat[0];
    Self.Data.Majitel  := Pole_dat[1];
    Self.Data.Oznaceni := Pole_dat[2];
    Self.Data.Poznamka := Pole_dat[3];

    Self.fadresa       := StrToInt(Pole_dat[4]);
    Self.Data.Trida    := THVClass(StrToInt(Pole_dat[5]));

    stanice := ORs.GetORIndex(Pole_dat[6]);
    if (stanice <> -1) then
     ORs.GetORByIndex(stanice, Self.Stav.stanice)
    else
     ORs.GetORByIndex(HVDb.default_or, Self.Stav.stanice);

//    if (Pole_dat[7] = '1') then
//      HV.Provoz := true else HV.Provoz := false;

//    HV.Paliva.Nafta := StrToInt(Pole_dat[8]);
//    HV.Paliva.Uhli  := StrToInt(Pole_dat[9]);
//    HV.Paliva.Voda  := StrToInt(Pole_dat[10]);

    Self.Stav.najeto_vpred.Metru  := StrToFloatDef(Pole_Dat[11], 0);
    Self.Stav.najeto_vpred.Bloku  := StrToInt(Pole_dat[12]);
    Self.Stav.najeto_vzad.Metru   := StrToFloatDef(Pole_Dat[13], 0);
    Self.Stav.najeto_vzad.Bloku   := StrToInt(Pole_dat[14]);

    Self.Stav.StanovisteA := THVStanoviste(StrToInt(Pole_Dat[15]));

    // na 2. radku jsou stavy funkci
    if (radek[1] <> '') then
     begin
      pole_dat.Clear();
      ExtractStringsEx([';'], Radek[1], pole_dat);
      for i := 0 to _HV_FUNC_MAX do
       if (i < pole_dat.Count) then
        Self.Stav.funkce[i] := PrevodySoustav.StrToBool(pole_dat[i])
       else
        Self.Stav.funkce[i] := false;
     end else begin
      for i := 0 to _HV_FUNC_MAX do Self.Stav.funkce[i] := false;
     end;

     // na 3. radku je POM pri prebirani : (cv,data)(cv,data)(...)...
     Self.Data.POMtake.Clear();
     if (radek[2] <> '') then
      begin
       pole_dat.Clear();
       ExtractStrings(['(', ')'], [], PChar(Radek[2]), pole_dat);

       for i := 0 to pole_dat.Count-1 do
        begin
         data2.Clear();
         ExtractStrings([','], [], PChar(pole_dat[i]), data2);
         if (data2.Count >= 2) then
          begin
           try
             pomCV.cv   := StrToInt(data2[0]);
             if ((pomCV.cv < 1) or (pomCV.cv > 1023)) then continue;             
             pomCV.data := StrToInt(data2[1]);
             Self.Data.POMtake.Add(pomCV);
           except
             continue;
           end;// except
          end;//if data2.Count >= 2
        end;//for i
      end;// if radek[2] <> ''

     // na 4. radku je POM pri uvolneni : (cv,data)(cv,data)(...)...
     Self.Data.POMrelease.Clear();
     if (radek[3] <> '') then
      begin
       pole_dat.Clear();
       ExtractStrings(['(', ')'], [], PChar(Radek[3]), pole_dat);

       for i := 0 to pole_dat.Count-1 do
        begin
         data2.Clear();
         ExtractStrings([','], [], PChar(pole_dat[i]), data2);
         if (data2.Count >= 2) then
          begin
           try
             pomCV.cv   := StrToInt(data2[0]);
             if ((pomCV.cv < 1) or (pomCV.cv > 1023)) then continue;
             pomCV.data := StrToInt(data2[1]);
             Self.Data.POMrelease.Add(pomCV);
           except
             continue;
           end;// except
          end;//if data2.Count >= 2
        end;//for i
      end;// if radek[2] <> ''

  except
   Pole_dat.Free();
   data2.Free();
   Exit(2);
  end;

 Pole_dat.Free();
 data2.Free();
 Result := 0;
end;//procedure

function THV.SaveToFile(const filename:string):Byte;
var Soubor:TextFile;
    line:string;
    i:Integer;
begin
  try
    AssignFile(Soubor, filename);
    Rewrite(Soubor);
  except
    try
      CloseFile(Soubor);
    except

    end;
    Exit(1);
  end;

  // prvni radek = obecna data
  line :=
    Self.Data.Nazev+';'+
    Self.Data.Majitel+';'+
    Self.Data.Oznaceni+';'+
    Self.Data.Poznamka+';'+
    IntToStr(Self.adresa)+';'+
    IntToStr(Integer(Self.Data.Trida))+';';

  if (Self.Stav.stanice <> nil) then
    line := line + Self.Stav.stanice.id+';'
  else
    line := line + ';';

  line := line +
    {prov+}';'+
    {IntToStr(HV.Paliva.Nafta)+}';'+
    {IntToStr(HV.Paliva.Uhli)+}';'+
    {IntToStr(HV.Paliva.Voda)+}';'+
    FloatToStr(Self.Stav.najeto_vpred.Metru)+';'+
    IntToStr(Self.Stav.najeto_vpred.Bloku)+';'+
    FloatToStr(Self.Stav.najeto_vzad.Metru)+';'+
    IntToStr(Self.Stav.najeto_vzad.Bloku)+';'+
    IntToStr(Integer(Self.Stav.StanovisteA))+';';

 WriteLn(Soubor, line);

 // druhy radek = stavy funkci
 line := '';
 for i := 0 to _HV_FUNC_MAX do
  begin
   if (Self.Stav.funkce[i]) then
     line := line + '1;'
   else
     line := line + '0;';
  end;
 WriteLn(Soubor, line);

 // 3. radek: POM pri prebirani
 line := '';
 for i := 0 to Self.Data.POMtake.Count-1 do
  line := line + '(' + IntToStr(Self.Data.POMtake[i].cv) + ',' + IntToStr(Self.Data.POMtake[i].data) + ')';
 WriteLn(Soubor, line);

 // 4. radek: POM pri uvolneni
 line := '';
 for i := 0 to Self.Data.POMrelease.Count-1 do
  line := line + '(' + IntToStr(Self.Data.POMrelease[i].cv) + ',' + IntToStr(Self.Data.POMrelease[i].data) + ')';
 WriteLn(Soubor, line);

 CloseFile(Soubor);
 Result := 0;
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

function THV.GetPanelLokString():string;
var i:Integer;
    func:TFunkce;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce|rychlost_stupne|rychlost_kmph|smer|
 // souprava je bud cislo soupravy, nebo znak '-'
 Result := Self.Data.Nazev + '|' + Self.Data.Majitel + '|' + Self.Data.Oznaceni + '|' + Self.Data.Poznamka + '|' +
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
           IntToStr(TrkSystem.GetStepSpeed(Self.Slot.speed)) + '|' + IntToStr(Self.Slot.smer);
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

// format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|-|stanovisteA|funkce
// na miste znaku - obvykle byva souprava, pri nacitani hnaciho vozidla tuto polozku nemenime -> je tam pomlcka
procedure THV.UpdateFromPanelString(data:string);
var str:TStrings;
    i:Integer;
begin
 str := TStringList.Create();
 ExtractStringsEx(['|'], data, str);

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
