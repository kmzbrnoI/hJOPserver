unit TBlokTrat;

// definice a obsluha technologickeho bloku Trat
// tento blok by se ve skutecnosti na panelu nemel vyskytnout - slouzi pouze jako rodin dou uvazek

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes,
     RPConst, IdContext;

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
  Useky:TArSmallI;          // reference na ID bloku
  rychlost:Integer;         // v km/h
 end;

 //aktualni stav trati
 TBlkTratStav = record
  zaver:boolean;
  smer:TTratSmer;
  zadost:boolean;
  soupravy:TTratSpr;
  SprPredict:Integer;

  BP : record
   next:Integer;                // blok, do ktereho se souprava chysta vjet (prvni neobsazeny)
   last:Integer;                // blok, ktery se souprava chysta opustit (posledni obsazeny)
  end;//BP
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

    procedure UpdateBP();
    procedure UpdateBezsouhlasSmer();
    procedure CheckJC();

    function GetBP():Boolean;
    procedure SetBP(state:boolean);

    procedure SprChangeOR(spr:Integer);

    function GetNavLichy():TBlk;
    function GetNavSudy():TBlk;

    // vrati hranicni navestidla
    property navLichy:TBlk read GetNavLichy;
    property navSudy:TBlk read GetNavSudy;

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

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;
    procedure ChangeFromUv(Sender:TBlk);
    procedure ChangeUseky();

    //----- trat own functions -----

    function GetSettings():TBlkTratSettings;
    procedure SetSettings(data:TBlkTratSettings);

    function IsFirstUvazka(uv:TBlk):boolean;
    procedure RBP();

    procedure AddSpr(spr:Integer);
    function GetSprList(separator:Char; hvs:boolean = true):string;
    procedure RemoveSpr(spr:Integer);

    function IsSpr(spr:Integer; predict:boolean = true):boolean;    // je souprava v trati? (vcetne predict)

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
    property BP:boolean read GetBP write SetBP;   // blokova podminka - zavedeni a zruseni; blokova podminka se zavadi obsazenim prvniho useku trati z jizdni cesty
    property SprPredict:Integer read TratStav.SprPredict write SetSprPredict;

    //GUI:
 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieMTB, TBloky, TOblRizeni, TBlokSCom, Logging,
    TJCDatabase, Main, TCPServerOR, TBlokUsek, TBlokUvazka, SprDb, THVDatabase;

constructor TBlkTrat.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_TRAT;
 Self.TratStav := _def_trat_stav;

 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.fNavLichy := nil;
 Self.fNavSudy  := nil;
end;//ctor

destructor TBlkTrat.Destroy();
begin
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

 Self.TratStav.BP.last := ini_stat.ReadInteger(section, 'BPlast', -1);
 Self.TratStav.BP.next := ini_stat.ReadInteger(section, 'BPnext', -1);

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
 SetLength(Self.TratSettings.Useky, str.Count);
 for i := 0 to str.Count-1 do
  Self.TratSettings.Useky[i] := StrToInt(str[i]);
 str.Free();

 Self.CreateTratFlag();
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
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  str := str + IntToStr(Self.TratSettings.Useky[i]) + ',';
 ini_tech.WriteString(section, 'useky', str)
end;//procedure

procedure TBlkTrat.SaveStatus(ini_stat:TMemIniFile;const section:string);
var i:Integer;
    str:string;
begin
 ini_stat.WriteInteger(section, 'smer', Integer(Self.file_smer));
 ini_stat.WriteInteger(section, 'BPlast', Self.TratStav.BP.last);
 ini_stat.WriteInteger(section, 'BPnext', Self.TratStav.BP.next);

 str := '';
 for i := 0 to Self.TratStav.soupravy.cnt-1 do
   str := str + Soupravy.GetSprNameByIndex(Self.TratStav.soupravy.data[i]) + ';';
 ini_stat.WriteString(section, 'spr', str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.Enable();
begin
 Self.TratStav.smer := Self.file_smer;
 Self.CreateTratFlag();   // nutne
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
 Self.TratStav.soupravy.cnt := 0;
 Self.SprPredict := -1;
 Self.BP := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkTrat.Update();
begin

end;//procedure

// change je vyvolano i pri zmene obsazenosti jakehokoliv useku v trati
procedure TBlkTrat.Change(now:boolean = false);
begin
 inherited Change(now);

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;

 // zachovat poradi volani techto dvou funkci !
 Self.UpdateBP();
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
 Self.UpdateBP();
 Self.UpdateBezsouhlasSmer();
 Self.CheckJC();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ChangeUseky();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_USEK)) then
    Blk.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetObsazeno():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (Blk = nil) then continue;
   if (Blk.GetGlobalSettings().typ <> _BLK_USEK) then continue;
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.obsazeno) then
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
 Self.TratSettings := data;
 Self.CreateTratFlag();

 // zrusim uvazku, aby se prepocitala
 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

// vsem blokum, ktere jsou u me v trati priradim, ze jsou skutecne v trati
procedure TBlkTrat.CreateTratFlag();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_USEK)) then continue;
   (Blk as TBlkUsek).InTrat := Self.GlobalSettings.id;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.RemoveTratFlag();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_USEK)) then continue;
   (Blk as TBlkUsek).InTrat := -1;
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

// RBP probiha tak, ze pokud najdeme blok v nouzovem zaveru, vsem od nej k vysilajici stanici zrusime soupravy a zavery
procedure TBlkTrat.RBP();
var i:Integer;
    Blk:TBlk;
    found:Integer;
begin
 found := -1;
 for i := Length(Self.TratSettings.Useky)-1 downto 0 do
  begin
   if (found < 0) then
    begin
     Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
     if ((Blk as TBlkUsek).Zaver = TJCType.nouz) then
       found := i;
    end;

   if (found > -1) then
    begin
     (Blk as TBlkUsek).Zaver := TJCType.no;
     if ((Blk as TBlkUsek).Souprava > -1) then
      (Blk as TBlkUsek).Souprava := -1;
    end;
  end;//for i

 if (Self.TratStav.BP.last = Self.TratStav.BP.next-1) then
  begin
   // RBP probehlo na celou soupravu -> zrusit BP a smazat soupravu
   Self.TratStav.BP.next := -1;
   Self.TratStav.BP.last := -1;

   if (Self.TratStav.soupravy.cnt > 0) then
    begin
     if (Blky.GetBlkWithSpr(Self.TratStav.soupravy.data[0]).Count = 0) then
       Soupravy.RemoveSpr(Self.TratStav.soupravy.data[0])
     else
       Self.RemoveSpr(Self.TratStav.soupravy.data[0]);
    end;
  end else begin
   // RBP probehlo uprostred, nebo na konci useku trati -> kaslu na konec -> posunu konec
   Self.TratStav.BP.last := found+1;
  end;

 if ((not Self.Obsazeno) and ((Self.TratSettings.zabzar = TTratZZ.bezsouhas) or (Self.TratSettings.zabzar = TTratZZ.nabidka))) then
  Self.Smer := TTratSmer.zadny;

 Self.Change();
end;//procedure

function TBlkTrat.GetRBP():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (((Blk as TBlkUsek).Zaver = TJCType.nouz) and ((Blk as TBlkUsek).Stav.Stav = TUsekStav.uvolneno)) then
    Exit(true);
  end;//for i
 Exit(false);
end;//function

////////////////////////////////////////////////////////////////////////////////

// aktualizace blokove podminky - projizdeni soupravy jednotlivymi bloky trati
procedure TBlkTrat.UpdateBP();
var i:Integer;
    new, old:TBlk;
begin
 if ((Self.Smer <> TTratSmer.AtoB) and (Self.Smer <> TTratSmer.BtoA)) then Exit();

 old := nil;
 for i := 0 to Length(Self.TratSettings.Useky)-1 do
  begin
   // podle smeru trati postupne vybirame bloky
   case (Self.Smer) of
    TTratSmer.AtoB: Blky.GetBlkByID(Self.TratSettings.Useky[i], new);
    TTratSmer.BtoA: Blky.GetBlkByID(Self.TratSettings.Useky[Length(Self.TratSettings.Useky)-i-1], new);
   end;//case

   if ((new = nil) or (new.GetGlobalSettings().typ <> _BLK_USEK)) then continue;
   if (i = 0) then old := new;

   // dalsi blok obsazen -> predani soupravy do tohoto bloku
   if ((i = Self.TratStav.BP.next) and ((new as TBlkUsek).Obsazeno = TUsekStav.obsazeno)) then
    begin
     if (old <> new) then   // u prvniho bloku nemame odkud priradit - priradi se tam od jizdni cesty
       (new as TBlkUsek).Souprava := (old as TBlkUsek).Souprava;

     // tato podminka musi byt na stejne urovni, jako podminka predchozi pro pripad, kdy ma trat pouze jeden usek
     if ((new as TBlkUsek).Souprava > -1) then
      begin
       (new as TBlkUsek).zpomalovani_ready := true;
       Soupravy.soupravy[(new as TBlkUsek).Souprava].front := new;

       if (i = Length(Self.TratSettings.Useky)-1) then
        begin
         // souprava vstoupila do posledniho bloku trati
         // zmena stanic soupravy a hnacich vozidel v ni
         Self.SprChangeOR((new as TBlkUsek).Souprava);

         // je nutne zmenit smer soupravy?
         if (((Assigned(Self.navLichy)) and (Assigned(Self.navSudy))) and
         ((Self.navLichy as TBlkSCom).Smer = (Self.navSudy as TBlkSCom).Smer)) then
          begin
           // navestidla na koncich trati jsou ve stejnem smeru -> zmenit smer soupravy, hnacich vozidel v ni a sipek
           Soupravy.soupravy[(new as TBlkUsek).Souprava].ChangeSmer();
          end;
        end;
      end;
     Inc(Self.TratStav.BP.next);
    end;

   // mazani soupravy vzadu
   // podminka (Self.BP.next > Self.BP.last) ma vyznam pri reseni uvolneni prvniho useku trati
   if ((i = Self.TratStav.BP.last) and ((new as TBlkUsek).Obsazeno = TUsekStav.uvolneno)) then
    begin
     if (Self.TratStav.BP.next > Self.TratStav.BP.last+1) then
      begin
       // blok nekde uvnitr dlohe trati
       (new as TBlkUsek).Souprava := -1;
       Inc(Self.TratStav.BP.last);
      end else begin
       if ((i = Length(Self.TratSettings.Useky)-1) and ((new as TBlkUsek).Souprava = -1)) then
        begin
         // blok na konci trati: tady mus byt zaroven splneno to, ze v nem neni souprava - soupravu si odstrani jizdni cesta
         // takto se kontroluje vypadek soupravy v poslendim bloku jizdni cesty
         Inc(Self.TratStav.BP.last);
        end;
      end;//else

    end;

    // prvni cast overuje vypadek prostredniho useku, posledni cast overuje vypadek samostatneho (posledniho) useku
    if (((i > Self.TratStav.BP.last) and (i < Self.TratStav.BP.next)) or
        ((i = Self.TratStav.BP.last) and (Self.TratStav.BP.last = Self.TratStav.BP.next-1))) then
     begin
      // blok uprostred uvolnen
      if ((new as TBlkUsek).Obsazeno = TUsekStav.uvolneno) then
         if ((new as TBlkUsek).Zaver <> TJCType.nouz) then
          begin
           (new as TBlkUsek).Zaver := TJCType.nouz;
           Self.Change();
          end;
      // blok uprostred obsazen
      if ((new as TBlkUsek).Obsazeno = TUsekStav.obsazeno) then
         if ((new as TBlkUsek).Zaver = TJCType.nouz) then
          begin
           (new as TBlkUsek).Zaver := TJCType.no;
           Self.Change();
          end;
     end;

   old := new;
  end;//for i

 // souprava vyjela z trati
 if (Self.TratStav.BP.last >= Length(Self.TratSettings.Useky)) then
  begin
   // zrusime potencialni nouzovy zaver na bloku na kraji trati
   case (Self.Smer) of
    TTratSmer.AtoB: Blky.GetBlkByID(Self.TratSettings.Useky[Length(Self.TratSettings.Useky)-1], new);
    TTratSmer.BtoA: Blky.GetBlkByID(Self.TratSettings.Useky[0], new);
   end;//case

  (new as TBlkUsek).Zaver := TJCType.no;

   if (((Self.TratSettings.zabzar = TTratZZ.bezsouhas) or (Self.TratSettings.zabzar = TTratZZ.nabidka)) and
    (not Self.Zaver) and (not Self.nouzZaver) and (not Self.Obsazeno)) then
    Self.Smer := TTratSmer.zadny;

   // zrusime blokovou podminku
   Self.BP := false;

   // souprava opustila trat svym koncem -> smazat soupravu z trati
   if (Self.TratStav.soupravy.cnt > 0) then
     Self.RemoveSpr(Self.TratStav.soupravy.data[0]);
  end;

end;//procedure

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

function TBlkTrat.GetBP():Boolean;
begin
 if ((Self.TratStav.BP.next > -1) or (Self.TratStav.BP.last > -1)) then
  Result := true else Result := false;
end;//function

procedure TBlkTrat.SetBP(state:boolean);
begin
 if (state) then
  begin
   Self.TratStav.BP.next := 0;
   Self.TratStav.BP.last := 0;
  end else begin
   Self.TratStav.BP.next := -1;
   Self.TratStav.BP.last := -1;
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
   fin   := Length(Self.TratSettings.Useky)-1;
  end;

  TTratSmer.BtoA:begin
   start := 0;
   fin   := Length(Self.TratSettings.Useky)-2;
  end;
 end;//case

 if (start >= fin) then Exit();

 for i := start to fin do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if (Blk = nil) then continue;
   if (Blk.GetGlobalSettings().typ <> _BLK_USEK) then continue;
   if ((Blk as TBlkUsek).Obsazeno = TUsekStav.obsazeno) then
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

end.//unit

