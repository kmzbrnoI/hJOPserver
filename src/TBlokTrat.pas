unit TBlokTrat;

// definice a obsluha technologickeho bloku Trat
// tento blok by se ve skutecnosti na panelu nemel vyskytnout - slouzi pouze jako rodic dvou uvazek

// U bloku trati je zajisteno, ze existuji a jsou typu TBlkTU
// Bloky, ktere tomuto nevyhovuji, jsou po startu odstraneny.

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes,
     RPConst, IdContext, Generics.Collections;

const
  _MAX_TRAT_SPR = 4;

type
 TTratZZ  = (souhlas = 0, bezsouhas = 1, nabidka = 2);   // typ tratoveho zabezpecovaciho zarizeni
 TTratSmer = (disabled = -1, zadny = 0, AtoB = 1, BtoA = 2);

 TTratSpr = record
  data:array [0.._MAX_TRAT_SPR] of Integer;
  cnt:Integer;
 end;

 //technologicka nastaveni trati
 TBlkTratSettings = record
  uvazkaA, uvazkaB:Integer; // reference na ID bloku
  zabzar:TTratZZ;
  Useky:TList<integer>;     // reference na ID bloku v trati
  rychlost:Integer;         // v km/h
 end;

 //aktualni stav trati
 TBlkTratStav = record
  zaver:boolean;
  smer:TTratSmer;
  zadost:boolean;
  soupravy:TTratSpr;
  SprPredict:Integer;

  BP:boolean;
 end;

 TBlkTrat = class(TBlk)
  const
   //defaultni stav
   _def_trat_stav:TBlkTratStav = (
    zaver: false;
    smer: disabled;
    zadost: false;
    SprPredict: -1;
   );

  private
   TratSettings:TBlkTratSettings;
   TratStav:TBlkTratStav;

   file_smer:TTratSmer;

   fuvazkaA, fuvazkaB: TBlk;    // tady si ukladam reference na skutecne bloky, ktere si vytvarim az pri prvnim pristu k uvazce z Settings
   fNavLichy, fNavSudy: TBlk;

    procedure CreateTratFlag();
    procedure RemoveTratFlag();

    function GetUvazkaA():TBlk;
    function GetUvazkaB():TBlk;

    function GetObsazeno():boolean;
    function GetZAK():boolean;
    function GetRBP():boolean;
    function GetNouzZaver():boolean;

    procedure SetTratSmer(smer:TTratSmer);
    procedure SetTratZaver(Zaver:boolean);
    procedure SetTratZadost(Zadost:boolean);
    procedure SetSprPredict(Spr:Integer);

    procedure UpdateBezsouhlasSmer();
    procedure CheckJC();

    procedure SetBP(state:boolean);

    function GetNavLichy():TBlk;
    function GetNavSudy():TBlk;

    procedure CheckTUExist();
    procedure InitTUs();

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure Reset(); override;
    procedure AfterLoad(); override;

    //update states
    procedure Change(now:boolean = false); override;
    procedure ChangeFromUv(Sender:TBlk);
    procedure ChangeUseky();

    //----- trat own functions -----

    function GetSettings():TBlkTratSettings;
    procedure SetSettings(data:TBlkTratSettings);

    function IsFirstUvazka(uv:TBlk):boolean;
//    procedure RBP();
    procedure SprChangeOR(spr:Integer);

    procedure AddSpr(spr:Integer);
    function GetSprList(separator:Char; hvs:boolean = true):string;
    procedure RemoveSpr(spr:Integer);

    function IsSpr(spr:Integer; predict:boolean = true):boolean;    // je souprava v trati? (vcetne predict)
    function IsSprInAnyTU(spr:Integer):boolean;

    property uvazkaA:TBlk read GetUvazkaA;
    property uvazkaB:TBlk read GetUvazkaB;
    property RBPCan:boolean read GetRBP;

    property stav:TBlkTratStav read TratStav;
    property Obsazeno:boolean read GetObsazeno;
    property Smer:TTratSmer read TratStav.smer write SetTratSmer;
    property Zaver:boolean read TratStav.zaver write SetTratZaver;
    property ZAK:boolean read GetZAK;
    property nouzZaver:boolean read GetNouzZaver;
    property Zadost:boolean read TratStav.zadost write SetTratZadost;
    property BP:boolean read TratStav.BP write SetBP;   // blokova podminka - zavedeni a zruseni; blokova podminka se zavadi obsazenim prvniho useku trati z jizdni cesty
    property SprPredict:Integer read TratStav.SprPredict write SetSprPredict;

    // vrati hranicni navestidla
    property navLichy:TBlk read GetNavLichy;
    property navSudy:TBlk read GetNavSudy;

 end;//class TBlkTrat

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieMTB, TBloky, TOblRizeni, TBlokSCom, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlokUsek, TBlokUvazka, SprDb, THVDatabase,
    TBlokTratUsek;

constructor TBlkTrat.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_TRAT;
 Self.TratStav := _def_trat_stav;

 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.fNavLichy := nil;
 Self.fNavSudy  := nil;

 Self.TratSettings.Useky := TList<Integer>.Create();
end;//ctor

destructor TBlkTrat.Destroy();
begin
 Self.TratSettings.Useky.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
    i:Integer;
    data:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TratSettings.uvazkaA  := ini_tech.ReadInteger(section, 'uvazkaA', -1);
 Self.TratSettings.uvazkaB  := ini_tech.ReadInteger(section, 'uvazkaB', -1);
 Self.TratSettings.zabzar   := TTratZZ(ini_tech.ReadInteger(section, 'zabzar', 0));
 Self.TratSettings.rychlost := ini_tech.ReadInteger(section, 'rychlost', 40);

 Self.file_smer := TTratSmer(ini_stat.ReadInteger(section, 'smer', 1));

 Self.TratStav.BP := ini_stat.ReadBool(section, 'BP', false);

 data := TStringList.Create();
 ExtractStrings([',', ';'], [], PChar(ini_stat.ReadString(section, 'spr', '')), data);
 Self.TratStav.soupravy.cnt := data.Count;
 for i := 0 to data.Count-1 do
  begin
   Self.TratStav.soupravy.data[i] := Soupravy.GetSprIndexByName(data[i]);
   if (Self.TratStav.soupravy.data[i] < 0) then
    begin
     Self.TratStav.soupravy.cnt := 0;
     break;
    end;
  end;
 data.Free();

 str := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini_tech.ReadString(section, 'useky', '')), str);
 Self.TratSettings.Useky.Clear();
 for i := 0 to str.Count-1 do
  begin
   try
    Self.TratSettings.Useky.Add(StrToInt(str[i]));
   except

   end;
  end;//for i
 str.Free();
end;//procedure

procedure TBlkTrat.SaveData(ini_tech:TMemIniFile;const section:string);
var str:string;
    i:Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'uvazkaA', Self.TratSettings.uvazkaA);
 ini_tech.WriteInteger(section, 'uvazkaB', Self.TratSettings.uvazkaB);
 ini_tech.WriteInteger(section, 'zabzar', Integer(Self.TratSettings.zabzar));
 ini_tech.WriteInteger(section, 'rychlost', Self.TratSettings.rychlost);

 str := '';
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  str := str + IntToStr(Self.TratSettings.Useky[i]) + ',';
 ini_tech.WriteString(section, 'useky', str)
end;//procedure

procedure TBlkTrat.SaveStatus(ini_stat:TMemIniFile;const section:string);
var i:Integer;
    str:string;
begin
 ini_stat.WriteInteger(section, 'smer', Integer(Self.file_smer));
 ini_stat.WriteBool(section, 'BP', Self.TratStav.BP);

 str := '';
 for i := 0 to Self.TratStav.soupravy.cnt-1 do
   str := str + Soupravy.GetSprNameByIndex(Self.TratStav.soupravy.data[i]) + ';';
 ini_stat.WriteString(section, 'spr', str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.Enable();
begin
 Self.InitTUs();
 Self.TratStav.smer := Self.file_smer;
 Self.Change();
end;//procedure

procedure TBlkTrat.Disable();
begin
 if (Self.Smer <> TTratSmer.disabled) then
   Self.file_smer := Self.Smer;
 Self.SprPredict    := -1;
 Self.TratStav.Smer := TTratSmer.disabled;
 Self.Change();
end;//procedure

procedure TBlkTrat.Reset();
begin
 Self.Zaver  := false;
 Self.Zadost := false;
 Self.SprPredict := -1;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AfterLoad();
begin
 Self.CheckTUExist();
 Self.CreateTratFlag();
end;//procedure

// change je vyvolano i pri zmene obsazenosti jakehokoliv useku v trati
procedure TBlkTrat.Change(now:boolean = false);
begin
 inherited Change(now);

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;

 // zachovat poradi volani techto dvou funkci !
 Self.UpdateBezsouhlasSmer();
 Self.CheckJC();

 (Self.uvazkaA as TBlkUvazka).ChangeFromTrat();
 (Self.uvazkaB as TBlkUvazka).ChangeFromTrat();

 inherited Update();
end;//procedure

procedure TBlkTrat.ChangeFromUv(Sender:TBlk);
begin
 if (Sender = Self.uvazkaA) then
  (Self.uvazkaB as TBlkUvazka).ChangeFromTrat();
 if (Sender = Self.uvazkaB) then
  (Self.uvazkaA as TBlkUvazka).ChangeFromTrat();

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;

 // zachovat poradi volani techto dvou funkci !
 Self.UpdateBezsouhlasSmer();
 Self.CheckJC();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ChangeUseky();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_TU)) then
    Blk.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetObsazeno():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_TU)) then continue;
   if ((Blk as TBlkTU).Obsazeno = TUsekStav.obsazeno) then
    Exit(true);
  end;

 Result := false;
end;//function

function TBlkTrat.GetZAK():boolean;
begin
 if ((Self.uvazkaA = nil) or (Self.uvazkaB = nil)) then Exit(false); 
 if (((Self.uvazkaA as TBlkUvazka).ZAK) or ((Self.uvazkaB as TBlkUvazka).ZAK)) then
  Result := true
 else
  Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.SetTratSmer(smer:TTratSmer);
begin
 Self.TratStav.smer := smer;

 // zrusime blokovou podminku
 Self.BP := false;

 // DEBUG: zavedem blokovou podminku
// Self.BP := ((smer = TTratSmer.AtoB) or (smer = TTratSmer.BtoA)); // DEBUG

 Self.Change();
end;//procedure

procedure TBlkTrat.SetTratZaver(Zaver:boolean);
begin
 if (Self.TratStav.zaver <> Zaver) then
  begin
   Self.TratStav.zaver := zaver;
   Self.TratStav.SprPredict := -1;
   Self.Change();
  end else begin
   Self.SprPredict := -1;
  end;
end;//procedure

procedure TBlkTrat.SetTratZadost(Zadost:boolean);
var uvazka:TBlkUvazka;
    i:Integer;
begin
 if (Self.Zadost = Zadost) then Exit(); 

 // tady se resi prehravani zvuku
 uvazka := nil;
 if ((Self.fuvazkaA as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaB as TBlkUvazka)
 else if ((Self.fuvazkaB as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaA as TBlkUvazka);

 if ((uvazka <> nil) and (Zadost <> Self.TratStav.zadost)) then
  begin
   if (Zadost) then
    begin
     for i := 0 to uvazka.OblsRizeni.Cnt-1 do
      uvazka.OblsRizeni.ORs[i].ZadostBlkCnt := uvazka.OblsRizeni.ORs[i].ZadostBlkCnt + 1;
    end else begin
     for i := 0 to uvazka.OblsRizeni.Cnt-1 do
      uvazka.OblsRizeni.ORs[i].ZadostBlkCnt := uvazka.OblsRizeni.ORs[i].ZadostBlkCnt - 1;
    end;
  end;

 Self.TratStav.zadost := zadost;
 Self.Change();
end;//procedure

procedure TBlkTrat.SetSprPredict(Spr:Integer);
begin
 if (Self.TratStav.SprPredict <> Spr) then
  begin
   Self.TratStav.SprPredict := Spr;
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSettings():TBlkTratSettings;
begin
 Result := Self.TratSettings;
end;//function

procedure TBlkTrat.SetSettings(data:TBlkTratSettings);
begin
 Self.RemoveTratFlag();
 if (data.Useky <> Self.TratSettings.Useky) then
   Self.TratSettings.Useky.Free();

 Self.TratSettings := data;

 if (not Assigned(data.Useky)) then data.Useky := TList<Integer>.Create();
 Self.CheckTUExist();
 Self.CreateTratFlag();

 // zrusim uvazku, aby se prepocitala
 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// vsem blokum, ktere jsou u me v trati, priradim, ze jsou skutecne v trati
procedure TBlkTrat.CreateTratFlag();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_TU)) then continue;
   (Blk as TBlkTU).InTrat := Self.GlobalSettings.id;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.RemoveTratFlag();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_TU)) then continue;
   (Blk as TBlkTU).InTrat := -1;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsFirstUvazka(uv:TBlk):boolean;
begin
 if (uv = Self.uvazkaA) then
  Result := true
 else
  Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetUvazkaA():TBlk;
begin
 if (Self.fuvazkaA = nil) then
  Blky.GetBlkByID(Self.TratSettings.uvazkaA, Self.fuvazkaA);
 Result := Self.fuvazkaA;
end;

function TBlkTrat.GetUvazkaB():TBlk;
begin
 if (Self.fuvazkaB = nil) then
  Blky.GetBlkByID(Self.TratSettings.uvazkaB, Self.fuvazkaB);
 Result := Self.fuvazkaB;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetRBP():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (((Blk as TBlkTU).Zaver = TJCType.nouz) and ((Blk as TBlkTU).Stav.Stav = TUsekStav.uvolneno)) then
    Exit(true);
  end;//for i
 Exit(false);
end;//function

////////////////////////////////////////////////////////////////////////////////

// resi, ze pokud je trat odobsazena, tak se smer vrati do bezsouhlaseho
// tohleto tady opravdu musi byt - predavani souprav v BP tuto funkci nenahrazuje !
// jedine tato funkce totiz resi pad zmeru trati v pripade zruseni jizdni cesty vedouci na ni
procedure TBlkTrat.UpdateBezsouhlasSmer();
begin
 if (((Self.GetSettings().zabzar = TTratZZ.bezsouhas))
     and (not Self.Zaver) and (not Self.Obsazeno) and ((Self.Smer = TTratSmer.AtoB) or (Self.Smer = TTratSmer.BtoA)) and (not Self.RBPCan) and (Self.TratStav.soupravy.cnt = 0) and (not Self.nouzZaver)) then
  Self.Smer := TTratSmer.zadny;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// zavedeni / zruseni blokove podminky
// zavest blokovou podminky lze vzdy, zrusit ji lze jen tehdy, kdyz
//  na zadnem tratovem useku neni blokova podminka

procedure TBlkTrat.SetBP(state:boolean);
var i:Integer;
    Blk:TBlk;
begin
 if (Self.BP = state) then Exit();

 if (state) then
  begin
   Self.TratStav.BP := true;
  end else begin
   for i := 0 to Self.TratSettings.Useky.Count-1 do
    begin
     Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
     if (TBlkTU(Blk).bpInBlk) then Exit();
    end;
   Self.TratStav.BP := false;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AddSpr(spr:Integer);
begin
 if (Self.TratStav.soupravy.cnt >= _MAX_TRAT_SPR) then Exit();

 Self.TratStav.soupravy.data[Self.TratStav.soupravy.cnt] := spr;

 if (Assigned(Soupravy.soupravy[spr])) then
  begin
   // nastavit tratovou rychlost soupravy
   if (Soupravy.soupravy[spr].rychlost <> Self.TratSettings.rychlost) then
     Soupravy.soupravy[spr].rychlost := Self.TratSettings.rychlost;
  end;

 Inc(Self.TratStav.soupravy.cnt);
 Self.SprPredict := -1;

 writelog('Traù '+Self.GlobalSettings.name+ ' : p¯id·na souprava '+Soupravy.soupravy[spr].nazev, WR_SPRPREDAT);

 Self.Change();
end;//procedure

procedure TBlkTrat.RemoveSpr(spr:Integer);
var i:Integer;
begin
 i := 0;    // ignorovat varovani, tato hodnota je pouzita v podmince nize (for cyklus se muze rozhodnout do ni nic nedat)
 for i := 0 to Self.TratStav.soupravy.cnt-1 do
   if (Self.TratStav.soupravy.data[i] = spr) then
     break;

 if (i = Self.TratStav.soupravy.cnt) then Exit();

 for i := i to Self.TratStav.soupravy.cnt-2 do
  Self.TratStav.soupravy.data[i] := Self.TratStav.soupravy.data[i];

 Dec(Self.TratStav.soupravy.cnt);
 writelog('Traù '+Self.GlobalSettings.name+ ' : smaz·na souprava '+Soupravy.soupravy[spr].nazev, WR_SPRPREDAT);

 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSprList(separator:Char; hvs:boolean = true):string;
var i, j:Integer;
begin
 Result := '';

 for i := 0 to Self.TratStav.soupravy.cnt-1 do
  begin
   Result := Result + Soupravy.GetSprNameByIndex(Self.TratStav.soupravy.data[i]);
   if (hvs) then
    begin
     Result := Result + '|';
     for j := 0 to Soupravy.soupravy[Self.TratStav.soupravy.data[i]].sdata.HV.cnt-1 do
       Result := Result + HVDb.HVozidla[Soupravy.soupravy[Self.TratStav.soupravy.data[i]].sdata.HV.HVs[j]].Data.Nazev + '|';
    end;
   Result := Result + separator;
  end;

 if (Self.SprPredict > -1) then
  begin
   Result := Result + '$' + Soupravy.GetSprNameByIndex(Self.SprPredict);
   if (hvs) then
    begin
     Result := Result + '|';
     for j := 0 to Soupravy.soupravy[Self.SprPredict].sdata.HV.cnt-1 do
       Result := Result + HVDb.HVozidla[Soupravy.soupravy[Self.SprPredict].sdata.HV.HVs[j]].Data.Nazev + '|';
    end;
   Result := Result + separator;
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.SprChangeOR(spr:Integer);
begin
 case (Self.Smer) of
   TTratSmer.AtoB: begin
      if ((Self.uvazkaB as TBlkUvazka).OblsRizeni.Cnt > 0) then
        Soupravy.soupravy[spr].stanice := (Self.uvazkaB as TBlkUvazka).OblsRizeni.ORs[0]
      else
        Soupravy.soupravy[spr].stanice := nil;
   end;//AtoB
   TTratSmer.BtoA:begin
      if ((Self.uvazkaA as TBlkUvazka).OblsRizeni.Cnt > 0) then
        Soupravy.soupravy[spr].stanice := (Self.uvazkaA as TBlkUvazka).OblsRizeni.ORs[0]
      else
        Soupravy.soupravy[spr].stanice := nil;
   end;//BtoA
 end;//case

 writelog('Traù '+Self.GlobalSettings.name+ ' : souprava '+Soupravy.soupravy[spr].nazev+' : stanice zmÏnÏna na '+(Soupravy.soupravy[spr].stanice as TOR).Name, WR_SPRPREDAT);

 //to-do: zmena smeru soupravy a OR hancich vozidel + sipek LS pokud maji stanice jiny lichy smer
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CheckJC();
var i, start, fin:Integer;
    Blk:TBlk;
begin
 // kontrola zruseni navesti jizdni cety pri obsazeni trati: ignoruji krajni useky, protoze ty si resi jizdni cesta sama
 if (not Self.Zaver) then Exit();
 if (((Self.Smer <> TTratSmer.AtoB) and (Self.Smer <> TTratSmer.BtoA))) then Exit();

 start := 0;
 fin   := 0;

 case (Self.Smer) of
  TTratSmer.AtoB:begin
   start := 1;
   fin   := Self.TratSettings.Useky.Count-1;
  end;

  TTratSmer.BtoA:begin
   start := 0;
   fin   := Self.TratSettings.Useky.Count-2;
  end;
 end;//case

 if (start >= fin) then Exit();

 for i := start to fin do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (Blk = nil) then continue;
   if (Blk.GetGlobalSettings().typ <> _BLK_TU) then continue;
   if ((Blk as TBlkTU).Obsazeno = TUsekStav.obsazeno) then
    begin
     JCDb.RusJC(Self);
     Exit();
    end;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetNouzZaver():boolean;
begin
 Result := (Self.uvazkaA as TBlkUvazka).nouzZaver or (Self.uvazkaB as TBlkUvazka).nouzZaver;
end;//function

////////////////////////////////////////////////////////////////////////////////

// vrati navestidlo bliz zacatku trati
function TBlkTrat.GetNavLichy():TBlk;
var i:Integer;
    Blk:TBlk;
begin
 if ((Self.fNavLichy = nil) or ((Self.fNavLichy as TBlkSCom).UsekID <> Self.uvazkaA.GetGlobalSettings.id)) then
  begin
   for i := 0 to Blky.Cnt-1 do
    begin
     Blky.GetBlkByIndex(i, Blk);
     if (Blk.GetGlobalSettings().typ <> _BLK_SCOM) then continue;
     if ((Blk as TBlkSCom).UsekID = Self.uvazkaA.GetGlobalSettings.id) then
      begin
       Self.fNavLichy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavLichy;
end;//function

// vrati navestidlo blize konci trati
function TBlkTrat.GetNavSudy():TBlk;
var i:Integer;
    Blk:TBlk;
begin
 if ((Self.fNavSudy = nil) or ((Self.fNavSudy as TBlkSCom).UsekID <> Self.uvazkaB.GetGlobalSettings.id)) then
  begin
   for i := 0 to Blky.Cnt-1 do
    begin
     Blky.GetBlkByIndex(i, Blk);
     if (Blk.GetGlobalSettings().typ <> _BLK_SCOM) then continue;
     if ((Blk as TBlkSCom).UsekID = Self.uvazkaB.GetGlobalSettings.id) then
      begin
       Self.fNavSudy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavSudy;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsSpr(spr:Integer; predict:boolean = true):boolean;
var i:Integer;
begin
 for i := 0 to Self.TratStav.soupravy.cnt-1 do
   if (Self.TratStav.soupravy.data[i] = spr) then
     Exit(true);

 if ((predict) and (Self.TratStav.SprPredict = spr)) then
   Exit(true);

 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CheckTUExist();
var i:Integer;
    Blk:TBlk;
begin
 for i := Self.TratSettings.Useky.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_TU)) then
    begin
     writelog('Trat '+Self.GetGlobalSettings().name+' obsahuje referenci na TU ID '+IntToStr(Self.TratSettings.Useky[i])+', tento blok ale bud neexistuje, nebo neni typu TU, odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
    end;
   if (((Blk as TBlkTU).InTrat <> -1) and ((Blk as TBlkTU).InTrat <> Self.GetGlobalSettings().id)) then
    begin
     writelog('Trat '+Self.GetGlobalSettings().name+': TU ID '+IntToStr(Self.TratSettings.Useky[i])+' jiz referuje na trat ID '+IntToStr((Blk as TBlkTU).InTrat)+', odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsSprInAnyTU(spr:Integer):boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (TBlkTU(Blk).Souprava = spr) then Exit(true);
  end;
 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////
// vytvoreni navaznosti mezi tratovymi useky a sekcemi tratovych useku

procedure TBlkTrat.InitTUs();
var i:Integer;
    Blk:TBlk;
    lTU, sTU:TBlk;
begin
 lTU := nil;
 Blky.GetBlkByID(Self.TratSettings.Useky[0], Blk);
 if (Self.TratSettings.Useky.Count > 1) then
   Blky.GetBlkByID(Self.TratSettings.Useky[1], sTU)
 else
   sTU := nil;

 for i := 0 to Self.TratSettings.Useky.Count-2 do
  begin
   TBlkTU(Blk).lTU := TBlkTU(lTU);
   TBlkTU(Blk).sTU := TBlkTU(sTU);
   lTU := Blk;
   Blk := sTU;
   if (i < Self.TratSettings.Useky.Count-2) then
     Blky.GetBlkByID(Self.TratSettings.Useky[i+2], sTU);
  end;

 // posledni TU:
 TBlkTU(Blk).lTU := TBlkTU(lTU);
 TBlkTU(Blk).sTU := nil;
end;

end.//unit

