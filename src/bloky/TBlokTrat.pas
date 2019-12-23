unit TBlokTrat;

{
 Definice a obsluha technologickeho bloku Trat
 tento blok by se ve skutecnosti na panelu nemel vyskytnout - slouzi pouze jako
 rodic dvou uvazek.

 U bloku trati je zajisteno, ze existuji a jsou typu TBlkTU
 Bloky, ktere tomuto nevyhovuji, jsou po startu odstraneny.
}

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes,
     IdContext, Generics.Collections;

const
  _MAX_TRAT_SPR = 6;

type
 TTratZZ  = (souhlas = 0, bezsouhas = 1, nabidka = 2);                          // typ tratoveho zabezpecovaciho zarizeni
 TTratSmer = (disabled = -1, zadny = 0, AtoB = 1, BtoA = 2);                    // mozne smery trati; disabled = cely blok trati je disabled
 TTratNavestidla = (autoblok = 0, hradlo = 1);

 //technologicka nastaveni trati
 TBlkTratSettings = record
  uvazkaA, uvazkaB:Integer;                                                     // reference na ID bloku trati
  zabzar:TTratZZ;                                                               // typ tratoveho zabezpecovaciho zarizeni
  navestidla:TTratNavestidla;                                                   // typ navestidel v trati
  Useky:TList<integer>;                                                         // reference na ID bloku v trati
 end;

 TBlkTratEFull = class(Exception);
 TBlkTratETimeNotDefined = class(Exception);

 // Jedna souprava v trati.
 TBlkTratSouprava = class
  private
    mTime: TTime;
    mTimeDefined: Boolean;

     function GetTime():TTime;
     procedure SetTime(time:TTime);

  public
    souprava: Integer;                                                          // index soupravy
    predict: Boolean;

     constructor Create(souprava:Integer); overload;
     constructor Create(souprava:Integer; time:TTime; predict:Boolean = false); overload;
     function IsTimeDefined():boolean;
     procedure UndefTime();
     property time: TTime read GetTime write SetTime;

     function SerializeForPanel(trat:TBlk; sprPredict:Boolean = false):string;
 end;

 //aktualni stav trati
 TBlkTratStav = record
  zaver:boolean;                                                                // aktualni stav zaveru na trati
  smer:TTratSmer;                                                               // aktualni smer trati, reprezentuje i enabled bloku
  zadost:boolean;                                                               // flag probihajici zadosti, zadost vzdy probiha proti aktualnimu smeru trati
  soupravy:TObjectList<TBlkTratSouprava>;                                       // seznam souprav v trati
  SprPredict:TBlkTratSouprava;                                                  // predpovidana souprava
  BP:boolean;                                                                   // jestli je v trati zavedena blokova podminka
 end;

 TSprUsek = record
  trat_index:Integer;
  usek:TBlk;
 end;

 TBlkTrat = class(TBlk)
  const
   //defaultni stav
   _def_trat_stav:TBlkTratStav = (
    zaver: false;
    smer: disabled;
    zadost: false;
    SprPredict: nil;
   );

  private
   TratSettings:TBlkTratSettings;
   TratStav:TBlkTratStav;

   file_smer:TTratSmer;

   fuvazkaA, fuvazkaB: TBlk;                                                    // tady si ukladame reference na skutecne bloky, ktere si vytvarime az pri prvnim pristupu k uvazce pres \uvazkaA a \uvazkaB
   fNavLichy, fNavSudy: TBlk;                                                   // analogicky funguji krajni navestidla trati, viz \navLichy a \navSudy
                                                                                // fNavLichy je navestidlo u stanice blize pocatku trati, fNavSudy navestidlo u stanice blize konce trati

    function GetUvazkaA():TBlk;
    function GetUvazkaB():TBlk;

    function GetObsazeno():boolean;
    function GetZAK():boolean;
    function GetRBP():boolean;
    function GetNouzZaver():boolean;

    procedure SetTratSmer(smer:TTratSmer);
    procedure SetTratZaver(Zaver:boolean);
    procedure SetTratZadost(Zadost:boolean);
    procedure SetSprPredict(Spr:TBlkTratSouprava);

    procedure UpdateBezsouhlasSmer();                                           // resi padani smeru trati v pripade bezsouhlsove trati

    procedure SetBP(state:boolean);

    function GetNavLichy():TBlk;
    function GetNavSudy():TBlk;

    procedure CheckTUExist();                                                   // zkontroluje existenci vsech bloku, ktere maji v trati byt; nevalidni bloky z trati smaze a provede o tom zapis do LOGu
    procedure InitTUs();                                                        // inicializuje tratove useky - oznami jim, ze jsou v trati a provede mnoho dalsich veci, viz telo metody
    procedure ResetTUs();                                                       // resetuje stav tratoveho useku; tratovy usek zapomene, ze je v nejake trati a stane se neutralnim tratovym usekem, ktery nic nevi

    function GetReady():boolean;                                                // vrati, jestli jsou vsechny tratove useky pripraveny pro vjezd soupravy, pouziva se pri zjistovani toho, jestli je mozne obratit smer trati
    function GetSprIndex(spr:Integer):Integer;
    function SprTUsCount(spr:Integer):Integer;

    function GetLastUsek():TBlk; overload;
    function GetVyluka():boolean;

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
    procedure SprChangeOR(spr:Integer); overload;
    procedure SprChangeOR(spr:Integer; smer:TTratSmer); overload;

    procedure AddSpr(spr:Integer); overload;
    procedure AddSpr(spr:TBlkTratSouprava); overload;
    function GetSprList(separator:Char):string;
    procedure RemoveSpr(spr:Integer);

    function IsSpr(spr:Integer; predict:boolean = true):boolean;
    function IsSprInAnyTU(spr:Integer):boolean;
    function IsSprInMoreTUs(spr:Integer):boolean;

    procedure CallChangeToTU();
    procedure UpdateSprPredict();
    function NavestProtismer():Integer;

    function SameUserControlsBothUvazka():boolean;                              // vraci true prave tehdy, kdyz obe uvazky kontrlu stejny uzivatel
                                                                                // kdyz je true, do trati neni potreba zadat

    function ChangesSprDir():boolean;                                           // vraci true prave tehdy, kdyz se v trati meni smer soupravy
    function GetSprUsek(spr_id:Integer):TSprUsek;
    function GetLastUsek(smer:TTratSmer):TBlk; overload;

    property uvazkaA:TBlk read GetUvazkaA;                                      // blok uvazky blize zacatku trati
    property uvazkaB:TBlk read GetUvazkaB;                                      // blok uvazky blize konci trati
    property RBPCan:boolean read GetRBP;                                        // vraci, jestli v trati doslo k poruse uplne blokove podminky, resp. jesli je mozno ji zrusit

    property stav:TBlkTratStav read TratStav;                                   // kompletni stav trati
    property Obsazeno:boolean read GetObsazeno;                                 // flag obsazenosti trati
    property Smer:TTratSmer read TratStav.smer write SetTratSmer;               // aktualni smer trati, reprezentuje i enabled celeho bloku
    property Zaver:boolean read TratStav.zaver write SetTratZaver;              // flag klasickeho zaveru trati (typicky od vlakove cesty)
    property ZAK:boolean read GetZAK;                                           // flag zakazu odjezdu do trati
    property nouzZaver:boolean read GetNouzZaver;                               // flag nouzoveho zaveru trati
    property Zadost:boolean read TratStav.zadost write SetTratZadost;           // flag probihajici zadosti o tratovy souhlas
    property BP:boolean read TratStav.BP write SetBP;                           // blokova podminka - zavedeni a zruseni; blokova podminka se zavadi obsazenim prvniho useku trati z jizdni cesty, rusi se pri uvolneni posledni soupravy z trati
    property SprPredict:TBlkTratSouprava read TratStav.SprPredict write SetSprPredict; // predpovidana souprava do trati
    property lastUsek:TBlk read GetLastUsek;                                    // posledni usek trati (smerove zavisle)
    property vyluka:boolean read GetVyluka;

    // vrati hranicni navestidla
    property navLichy:TBlk read GetNavLichy;                                    // hranicni navestidlo trati blize zacatku trati
    property navSudy:TBlk read GetNavSudy;                                      // hranicni navestidlo trati blize konci trati

    property ready:boolean read GetReady;                                       // jsou vsechny tratove useky "ready"? typicky se pouziva jako flag moznosti zmeny smeru trati
    property zabzar: TTratZZ read TratSettings.zabzar;
    property navestidla:TTratNavestidla read TratSettings.navestidla;

 end;//class TBlkTrat

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, TOblRizeni, TBlokNav, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlokUsek, TBlokUvazka, SprDb, THVDatabase,
    TBlokTratUsek, appEv, timeHelper, Prevody, Graphics;

constructor TBlkTrat.Create(index:Integer);
begin
 inherited;

 Self.GlobalSettings.typ := _BLK_TRAT;
 Self.TratStav := _def_trat_stav;

 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.fNavLichy := nil;
 Self.fNavSudy  := nil;

 Self.TratSettings.Useky := TList<Integer>.Create();
 Self.TratStav.soupravy  := TObjectList<TBlkTratSouprava>.Create();
end;//ctor

destructor TBlkTrat.Destroy();
begin
 Self.TratStav.soupravy.Free();
 Self.TratSettings.Useky.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
    i:Integer;
    data:TStrings;
    index:Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.TratSettings.uvazkaA  := ini_tech.ReadInteger(section, 'uvazkaA', -1);
 Self.TratSettings.uvazkaB  := ini_tech.ReadInteger(section, 'uvazkaB', -1);
 Self.TratSettings.zabzar   := TTratZZ(ini_tech.ReadInteger(section, 'zabzar', 0));
 Self.TratSettings.navestidla := TTratNavestidla(ini_tech.ReadInteger(section, 'navestidla', 0));

 Self.file_smer := TTratSmer(ini_stat.ReadInteger(section, 'smer', 1));

 Self.TratStav.BP := ini_stat.ReadBool(section, 'BP', false);

 data := TStringList.Create();
 ExtractStrings([',', ';'], [], PChar(ini_stat.ReadString(section, 'spr', '')), data);
 Self.TratStav.soupravy.Clear();
 for i := 0 to data.Count-1 do
  begin
   index := Soupravy.GetSprIndexByName(data[i]);
   if (index > -1) then Self.TratStav.soupravy.Add(TBlkTratSouprava.Create(index));
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
end;

procedure TBlkTrat.SaveData(ini_tech:TMemIniFile;const section:string);
var str:string;
    i:Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'uvazkaA', Self.TratSettings.uvazkaA);
 ini_tech.WriteInteger(section, 'uvazkaB', Self.TratSettings.uvazkaB);
 ini_tech.WriteInteger(section, 'zabzar', Integer(Self.TratSettings.zabzar));
 ini_tech.WriteInteger(section, 'navestidla', Integer(Self.TratSettings.navestidla));

 str := '';
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  str := str + IntToStr(Self.TratSettings.Useky[i]) + ',';
 ini_tech.WriteString(section, 'useky', str)
end;

procedure TBlkTrat.SaveStatus(ini_stat:TMemIniFile;const section:string);
var i:Integer;
    str:string;
begin
 ini_stat.WriteInteger(section, 'smer', Integer(Self.file_smer));

 if (Self.TratStav.BP) then
   ini_stat.WriteBool(section, 'BP', Self.TratStav.BP);

 str := '';
 for i := 0 to Self.TratStav.soupravy.Count-1 do
   str := str + Soupravy.GetSprNameByIndex(Self.TratStav.soupravy[i].souprava) + ';';

 if (str <> '') then
   ini_stat.WriteString(section, 'spr', str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.Enable();
begin
 Self.TratStav.smer := Self.file_smer;
 Self.Change();
end;

procedure TBlkTrat.Disable();
begin
 if (Self.Smer <> TTratSmer.disabled) then
   Self.file_smer := Self.Smer;
 Self.SprPredict    := nil;
 Self.TratStav.Smer := TTratSmer.disabled;
 Self.Change(true);
end;

procedure TBlkTrat.Reset();
begin
 Self.Zaver  := false;
 Self.Zadost := false;
 Self.SprPredict := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AfterLoad();
begin
 Self.CheckTUExist();
 Self.InitTUs();
end;

// change je vyvolano i pri zmene obsazenosti jakehokoliv useku v trati
procedure TBlkTrat.Change(now:boolean = false);
begin
 inherited Change(now);

 if ((Self.Zadost) and (Self.Obsazeno)) then
   Self.Zadost := false;

 // zachovat poradi volani techto dvou funkci !
 Self.UpdateBezsouhlasSmer();

 (Self.uvazkaA as TBlkUvazka).ChangeFromTrat();
 (Self.uvazkaB as TBlkUvazka).ChangeFromTrat();

 inherited Update();
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ChangeUseky();
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk <> nil) and (Blk.typ = _BLK_TU)) then
    Blk.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetObsazeno():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> _BLK_TU)) then continue;
   if ((Blk as TBlkTU).Obsazeno = TUsekStav.obsazeno) then
    Exit(true);
  end;

 Result := false;
end;

function TBlkTrat.GetZAK():boolean;
begin
 if ((Self.uvazkaA = nil) or (Self.uvazkaB = nil)) then Exit(false); 
 if (((Self.uvazkaA as TBlkUvazka).ZAK) or ((Self.uvazkaB as TBlkUvazka).ZAK)) then
  Result := true
 else
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.SetTratSmer(smer:TTratSmer);
begin
 Self.TratStav.smer := smer;

 // zrusime blokovou podminku
 Self.BP := false;

 Self.Change();
 Self.CallChangeToTU();
end;

procedure TBlkTrat.SetTratZaver(Zaver:boolean);
begin
 if (Self.TratStav.zaver <> Zaver) then
  begin
   Self.TratStav.zaver := zaver;
   Self.SprPredict := nil;
   Self.Change();
  end else begin
   Self.SprPredict := nil;
  end;
end;

procedure TBlkTrat.SetTratZadost(Zadost:boolean);
var uvazka:TBlkUvazka;
    oblr:TOR;
begin
 if (Self.Zadost = Zadost) then Exit(); 

 // tady se resi prehravani zvuku
 try
   uvazka := nil;
   if ((Self.fuvazkaA as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaB as TBlkUvazka)
   else if ((Self.fuvazkaB as TBlkUvazka).zadost) then uvazka := (Self.fuvazkaA as TBlkUvazka);

   if ((uvazka <> nil) and (Zadost <> Self.TratStav.zadost)) then
    begin
     if (Zadost) then
      begin
       for oblr in uvazka.OblsRizeni do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt + 1;
      end else begin
       for oblr in uvazka.OblsRizeni do
         oblr.ZadostBlkCnt := oblr.ZadostBlkCnt - 1;
      end;
    end;
 except
   on E:Exception do
     AppEvents.LogException(E, 'SetTratZadost');
 end;

 Self.TratStav.zadost := zadost;
 Self.Change();
end;

procedure TBlkTrat.SetSprPredict(Spr:TBlkTratSouprava);
begin
 if (Self.TratStav.SprPredict = Spr) then Exit();

 if (Self.TratStav.SprPredict <> nil) then
   FreeAndNil(Self.TratStav.SprPredict);

 if (Spr <> nil) then
   Self.TratStav.SprPredict := Spr;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSettings():TBlkTratSettings;
begin
 Result := Self.TratSettings;
end;

procedure TBlkTrat.SetSettings(data:TBlkTratSettings);
begin
 Self.ResetTUs();
 if (data.Useky <> Self.TratSettings.Useky) then
   Self.TratSettings.Useky.Free();

 Self.TratSettings := data;

 if (not Assigned(data.Useky)) then data.Useky := TList<Integer>.Create();
 Self.CheckTUExist();
 Self.InitTUs();

 // zrusim uvazku, aby se prepocitala
 Self.fuvazkaA := nil;
 Self.fuvazkaB := nil;

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsFirstUvazka(uv:TBlk):boolean;
begin
 if (uv = Self.uvazkaA) then
  Result := true
 else
  Result := false;
end;

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
   if (TBlkTU(Blk).poruchaBP) then Exit(true);
  end;//for i
 Exit(false);
end;

////////////////////////////////////////////////////////////////////////////////

// resi, ze pokud je trat odobsazena, tak se smer vrati do bezsouhlaseho
// tohleto tady opravdu musi byt - predavani souprav v BP tuto funkci nenahrazuje !
// jedine tato funkce totiz resi pad zmeru trati v pripade zruseni jizdni cesty vedouci na ni
procedure TBlkTrat.UpdateBezsouhlasSmer();
begin
 if (((Self.GetSettings().zabzar = TTratZZ.bezsouhas)) and
     (not Self.Zaver) and (not Self.Obsazeno) and (not Self.ZAK) and
     ((Self.Smer = TTratSmer.AtoB) or (Self.Smer = TTratSmer.BtoA)) and
     (not Self.RBPCan) and (Self.TratStav.soupravy.Count = 0) and (not Self.nouzZaver)) then
  Self.Smer := TTratSmer.zadny;
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.AddSpr(spr:Integer);
begin
 Self.AddSpr(TBlkTratSouprava.Create(spr, timeHelper.hJOPnow()));
end;

procedure TBlkTrat.AddSpr(spr:TBlkTratSouprava);
begin
 if (Self.TratStav.soupravy.Count >= _MAX_TRAT_SPR) then
   raise TBlkTratEFull.Create('Trat je plna!');

 Self.TratStav.soupravy.Add(spr);
 if (spr <> Self.SprPredict) then
   Self.SprPredict := nil // will also Free
 else
   Self.TratStav.SprPredict := nil; // will not Free

 if (not spr.IsTimeDefined()) then
   spr.time := timeHelper.hJOPnow();

 writelog('Trať '+Self.GlobalSettings.name+ ' : přidána souprava '+Soupravy[spr.souprava].nazev, WR_SPRPREDAT);

 Self.Change();
end;

procedure TBlkTrat.RemoveSpr(spr:Integer);
var toChange:boolean;
begin
 toChange := false;

 if ((Self.SprPredict <> nil) and (Self.SprPredict.souprava = spr)) then
  begin
   Self.SprPredict := nil;
   toChange := true;
  end;

 if (Self.IsSpr(spr)) then
  begin
   Self.TratStav.soupravy.Delete(Self.GetSprIndex(spr));
   writelog('Trať '+Self.GlobalSettings.name+ ' : smazána souprava '+Soupravy[spr].nazev, WR_SPRPREDAT);
   toChange := true;
  end;

 if (toChange) then
   Self.Change(); 
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSprList(separator:Char):string;
var spr:TBlkTratSouprava;
begin
 Result := '';

 for spr in Self.TratStav.soupravy do
   Result := Result + spr.SerializeForPanel(Self) + separator;

 if (Self.SprPredict <> nil) then
   Result := Result + Self.SprPredict.SerializeForPanel(Self, true) + separator;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.SprChangeOR(spr:Integer);
begin
 Self.SprChangeOR(spr, Self.Smer);
end;

procedure TBlkTrat.SprChangeOR(spr:Integer; smer:TTratSmer);
begin
 case (smer) of
   TTratSmer.AtoB: begin
      if ((Self.uvazkaB as TBlkUvazka).OblsRizeni.Count > 0) then
        Soupravy[spr].stanice := (Self.uvazkaB as TBlkUvazka).OblsRizeni[0]
      else
        Soupravy[spr].stanice := nil;
   end;//AtoB
   TTratSmer.BtoA:begin
      if ((Self.uvazkaA as TBlkUvazka).OblsRizeni.Count > 0) then
        Soupravy[spr].stanice := (Self.uvazkaA as TBlkUvazka).OblsRizeni[0]
      else
        Soupravy[spr].stanice := nil;
   end;//BtoA
 end;//case

 writelog('Trať '+Self.GlobalSettings.name+ ' : souprava '+Soupravy[spr].nazev+' : stanice změněna na '+(Soupravy[spr].stanice as TOR).Name, WR_SPRPREDAT);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetNouzZaver():boolean;
begin
 Result := (Self.uvazkaA as TBlkUvazka).nouzZaver or (Self.uvazkaB as TBlkUvazka).nouzZaver;
end;

////////////////////////////////////////////////////////////////////////////////

// vrati hranicni navestidlo trati na jejim zacatku
// hranicni navestidlo musi
//  - byt ve stejne OR, jako uvazka
//  - mit blok_pred_id hranicni blok trati
//  - nebyl navestidlo autobloku druheho useku trati
function TBlkTrat.GetNavLichy():TBlk;
var i:Integer;
    Blk:TBlk;
    BlkTU:TBlkTU;
begin
 if ((Self.fNavLichy = nil) or ((Self.fNavLichy as TBlkNav).UsekID <> Self.TratSettings.Useky[0])) then
  begin
   if (Self.TratSettings.Useky.Count > 1) then
     Blky.GetBlkByID(Self.TratSettings.Useky[1], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> _BLK_NAV) then continue;
     if ((TBlkNav(Blk).UsekID = Self.TratSettings.Useky[0]) and
         (Blk.OblsRizeni[0] = Self.uvazkaA.OblsRizeni[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.navLid))) then
      begin
       Self.fNavLichy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavLichy;
end;

// vrati hranicni navestidlo trati na jejim konci
function TBlkTrat.GetNavSudy():TBlk;
var i:Integer;
    Blk:TBlk;
    BlkTU:TBlkTU;
begin
 if ((Self.fNavSudy = nil) or ((Self.fNavSudy as TBlkNav).UsekID <> Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1])) then
  begin
   if (Self.TratSettings.Useky.Count > 1) then
     Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-2], TBlk(BlkTU))
   else
     BlkTU := nil;

   for blk in Blky do
    begin
     if (Blk.typ <> _BLK_NAV) then continue;
     if ((TBlkNav(Blk).UsekID = Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1]) and
         (Blk.OblsRizeni[0] = Self.uvazkaB.OblsRizeni[0]) and
         ((BlkTU = nil) or (Blk.id <> BlkTU.GetSettings.navSid))) then
      begin
       Self.fNavSudy := Blk;
       break;
      end;
    end;
  end;

 Result := Self.fNavSudy;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.IsSpr(spr:Integer; predict:boolean = true):boolean;
begin
 Result := ((Self.GetSprIndex(spr) > -1) or
            ((predict) and (Self.SprPredict <> nil) and (Self.SprPredict.souprava = spr)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSprIndex(spr:Integer):Integer;
var i:Integer;
begin
 for i := 0 to Self.TratStav.soupravy.Count-1 do
   if (Self.TratStav.soupravy[i].souprava = spr) then
     Exit(i);
 Exit(-1);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CheckTUExist();
var i:Integer;
    Blk:TBlk;
begin
 for i := Self.TratSettings.Useky.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> _BLK_TU)) then
    begin
     writelog('Trat '+Self.name+' obsahuje referenci na TU ID '+IntToStr(Self.TratSettings.Useky[i])+', tento blok ale bud neexistuje, nebo neni typu TU, odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
     continue;
    end;
   if (((Blk as TBlkTU).InTrat <> -1) and ((Blk as TBlkTU).InTrat <> Self.id)) then
    begin
     writelog('Trat '+Self.name+': TU ID '+IntToStr(Self.TratSettings.Useky[i])+' jiz referuje na trat ID '+IntToStr((Blk as TBlkTU).InTrat)+', odstranuji referenci', WR_ERROR);
     Self.TratSettings.Useky.Delete(i);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.SprTUsCount(spr:Integer):Integer;
var usek:Integer;
    Blk:TBlk;
begin
 Result := 0;
 for usek in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usek, Blk);
   if (TBlkTU(Blk).IsSouprava(spr)) then
     Inc(Result);
  end;
end;

function TBlkTrat.IsSprInAnyTU(spr:Integer):boolean;
begin
 Result := (Self.SprTUsCount(spr) > 0);
end;

function TBlkTrat.IsSprInMoreTUs(spr:Integer):boolean;
begin
 Result := (Self.SprTUsCount(spr) > 1);
end;

////////////////////////////////////////////////////////////////////////////////
// vytvoreni navaznosti mezi tratovymi useky, sekcemi tratovych useku a
// navestidly autobloku

procedure TBlkTrat.InitTUs();
var i:Integer;
    lTU, sTU:TBlkTU;
    useky:TList<TBlkTU>;
    blk, sMaster:TBlkTU;
begin
 // 1) nejprve vytvorime navaznosti mezi tratovymi useky:
 //    Kazdemu TU rekneme, jaky TU je vedle neho v lichem smeru (bliz zacatku trati)
 //    a jaky TU je vedle enho v sudem smeru (bliz konci trati).
 //    Krajni TU maji referenci na dalsi TU "nil".

 if (Self.TratSettings.Useky.Count = 0) then Exit(); 

 lTU := nil;
 Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(blk));
 if (Self.TratSettings.Useky.Count > 1) then
   Blky.GetBlkByID(Self.TratSettings.Useky[1], TBlk(sTU))
 else
   sTU := nil;

 for i := 0 to Self.TratSettings.Useky.Count-2 do
  begin
   Blk.lTU := lTU;
   Blk.sTU := sTU;
   lTU := Blk;
   Blk := sTU;
   if (i < Self.TratSettings.Useky.Count-2) then
     Blky.GetBlkByID(Self.TratSettings.Useky[i+2], TBlk(sTU));
  end;

 // posledni TU:
 Blk.lTU := lTU;
 Blk.sTU := nil;

 /////////////////////////////////////////////////////////////////////////////
 // 2) Kazdemu TU priradime jeho Section Master a Section Masteru priradime
 //    jeho useky.

 //  a) v lichem smeru: jdeme od zacatku trati ke konci
 Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(sMaster));
 useky := sMaster.lsectUseky;
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));

   // useku take priradime, ze je v nasi trati
   (Blk as TBlkTU).InTrat := Self.GlobalSettings.id;

   if (blk.GetSettings().navLid <> -1) then
    begin
     sMaster := blk;
     useky := sMaster.lsectUseky;
    end;
   blk.lsectMaster := sMaster;
   useky.Add(blk);
  end;

 //  b) v sudem smeru: jdeme od konce trati k zacatku
 Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], TBlk(sMaster));
 useky := sMaster.ssectUseky;
 for i := Self.TratSettings.Useky.Count-1 downto 0 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   if (blk.GetSettings().navSid <> -1) then
    begin
     sMaster := blk;
     useky := sMaster.ssectUseky;
    end;
   blk.ssectMaster := sMaster;
   useky.Add(blk);
  end;

 /////////////////////////////////////////////////////////////////////////////
 // 3) inicializujeme navaznosti navestidel

 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.CreateNavRefs();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.ResetTUs();
var i:Integer;
    blk:TBlkTU;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.RemoveTURefs();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkTrat.CallChangeToTU();
var i:Integer;
    blk:TBlkTU;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(blk));
   blk.ChangeFromTrat();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace predpovidane soupravy na posledni usek trati
// volano pri uvolneni posledniho useku trati, nebo RBP

procedure TBlkTrat.UpdateSprPredict();
var Blk, last:TBlkTU;
    i:Integer;
begin
 if ((Self.Smer <> TTratSmer.AtoB) and (Self.Smer <> TTratSmer.BtoA)) then Exit();

 case (Self.Smer) of
  TTratSmer.AtoB: begin
       Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], TBlk(last));
       last.SprPredict := -1;
       if (last.IsSouprava()) then Exit();
       for i := Self.TratSettings.Useky.Count-2 downto 0 do
        begin
         Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(Blk));
         if (Blk.IsSouprava()) then
          begin
           last.SprPredict := Blk.Souprava;
           break;
          end;
         if (Blk.SprPredict > -1) then
          begin
           last.SprPredict := Blk.SprPredict;
           break;
          end;
         if ((Blk.navKryci <> nil) and (TBlkNav(Blk.navKryci).Navest = 0)) then
          begin
           Blky.SprPrediction(Self.navSudy);
           Exit();
          end;
        end;

       if ((last.SprPredict = -1) and (Self.SprPredict <> nil)) then
         last.SprPredict := Self.SprPredict.souprava;
       if (Self.navSudy <> nil) then
         Blky.SprPrediction(Self.navSudy);
  end;

  TTratSmer.BtoA: begin
       Blky.GetBlkByID(Self.TratSettings.Useky[0], TBlk(last));
       last.SprPredict := -1;
       if (last.IsSouprava()) then Exit();
       for i := 1 to Self.TratSettings.Useky.Count-1 do
        begin
         Blky.GetBlkByID(Self.TratSettings.Useky[i], TBlk(Blk));
         if (Blk.IsSouprava()) then
          begin
           last.SprPredict := Blk.Souprava;
           break;
          end;
         if (Blk.SprPredict > -1) then
          begin
           last.SprPredict := Blk.SprPredict;
           break;
          end;
         if ((Blk.navKryci <> nil) and (TBlkNav(Blk.navKryci).Navest = 0)) then
          begin
           Blky.SprPrediction(Self.navLichy);
           Exit();
          end;
        end;

       if ((last.SprPredict = -1) and (Self.SprPredict <> nil)) then
         last.SprPredict := Self.SprPredict.souprava;
       if (Self.navLichy <> nil) then
         Blky.SprPrediction(Self.navLichy);
  end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetReady():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], Blk);
   if ((Blk = nil) or (Blk.typ <> _BLK_TU)) then Exit(false);
   if (not TBlkTU(Blk).ready) then Exit(false);
  end;
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
// Vraci true prave tehdy, pokud je trat na obou koncich rizena stejnym uzivatelem.
// Mazerne nenavazujeme rizeni obou koncu na konkretniho uzivatele, napriklad
// kvuli staveni ze zasobniku.

function TBlkTrat.SameUserControlsBothUvazka():boolean;
var first, second:TORPanel;
begin
 if ((not Assigned(Self.uvazkaA)) or (not Assigned(Self.uvazkaB))) then Exit(false);
 if ((TBlkUvazka(Self.uvazkaA).OblsRizeni.Count <> 1) or (TBlkUvazka(Self.uvazkaB).OblsRizeni.Count <> 1)) then Exit(false);

 for first in TBlkUvazka(Self.uvazkaA).OblsRizeni[0].Connected do
   if (first.Rights >= TORControlRights.write) then
     for second in TBlkUvazka(Self.uvazkaB).OblsRizeni[0].Connected do
       if ((first.user = second.user) and (second.Rights >= TORControlRights.write)) then
         Exit(true);

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.ChangesSprDir():boolean;
begin
 Result := (Assigned(Self.navLichy)) and (Assigned(Self.navSudy)) and
    (TBlkNav(Self.navLichy).Smer = TBlkNav(Self.navSudy).Smer);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetSprUsek(spr_id:Integer):TSprUsek;
var i:Integer;
    blk:TBlk;
begin
 Result.trat_index := -1;
 Result.usek := nil;

 for i := 0 to Self.TratSettings.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.TratSettings.Useky[i], blk);
   if ((blk <> nil) and (blk.typ = _BLK_TU)) then
     if (TBlkUsek(blk).Souprava = spr_id) then
      begin
       Result.trat_index := i;
       Result.usek := blk;
       Exit();
      end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.GetLastUsek():TBlk;
begin
 Result := Self.GetLastUsek(Self.smer);
end;

function TBlkTrat.GetLastUsek(smer:TTratSmer):TBlk;
begin
 if (Self.TratSettings.Useky.Count < 1) then
   raise Exception.Create('Trať nemá žádný úsek!');

 if (smer = TTratSmer.AtoB) then
   Blky.GetBlkByID(Self.TratSettings.Useky[Self.TratSettings.Useky.Count-1], Result)
 else if (smer = TTratSmer.BtoA) then
   Blky.GetBlkByID(Self.TratSettings.Useky[0], Result)
 else
   raise Exception.Create('Trať nemá žádný směr!');
end;

////////////////////////////////////////////////////////////////////////////////


function TBlkTrat.GetVyluka():boolean;
var blkUsek:TBlkUsek;
    usek:Integer;
begin
 for usek in Self.TratSettings.Useky do
  begin
   Blky.GetBlkByID(usek, TBlk(blkUsek));
   if (blkUsek <> nil) then
     if (blkUsek.Vyluka <> '') then
       Exit(true);
  end;
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// TBlkTratSouprava

constructor TBlkTratSouprava.Create(souprava:Integer);
begin
 inherited Create();
 Self.souprava := souprava;
 Self.mTimeDefined := false;
 Self.predict := false;
end;

constructor TBlkTratSouprava.Create(souprava:Integer; time:TTime; predict:Boolean = false);
begin
 inherited Create();
 Self.souprava := souprava;
 Self.time := time;
 Self.predict := predict;
end;

function TBlkTratSouprava.GetTime():TTime;
begin
 if (not Self.IsTimeDefined()) then
   raise TBlkTratETimeNotDefined.Create('Time not defined!');
 Result := Self.mTime;
end;

procedure TBlkTratSouprava.SetTime(time:TTime);
begin
 Self.mTimeDefined := true;
 Self.mTime := time;
end;

function TBlkTratSouprava.IsTimeDefined():boolean;
begin
 Result := Self.mTimeDefined;
end;

procedure TBlkTratSouprava.UndefTime();
begin
 Self.mTimeDefined := false;
end;

function TBlkTratSouprava.SerializeForPanel(trat:TBlk; sprPredict:Boolean = false):string;
var addr, usek:Integer;
    porucha_bp:Boolean;
    blk:TBlk;
begin
 // Pozor, souprava muze byt ve vice usecich a mit poruchu BP jen v jednom z nich
 porucha_bp := false;
 for usek in TBlkTrat(trat).GetSettings().Useky do
  begin
   Blky.GetBlkByID(usek, blk);
   if ((blk <> nil) and (blk.typ = _BLK_TU)) then
     if (TBlkUsek(blk).Souprava = Self.souprava) and (TBlkTU(blk).poruchaBP) then
       porucha_bp := true;
  end;


 Result := Soupravy.GetSprNameByIndex(Self.souprava) + '|';
 if (sprPredict) then
   Result := Result + PrevodySoustav.ColorToStr(clYellow) + '|'
 else if (porucha_bp) then
   Result := Result + PrevodySoustav.ColorToStr(clAqua) + '|'
 else
   Result := Result + PrevodySoustav.ColorToStr(clWhite) + '|';

 if (Self.mTimeDefined) then
   Result := Result + FormatDateTime('nn', Self.mTime);
 Result := Result + '|';
 if (Self.predict) then
   Result := Result + PrevodySoustav.ColorToStr(clYellow) + '|'
 else
   Result := Result + PrevodySoustav.ColorToStr(clAqua) + '|';

 for addr in Soupravy[Self.souprava].HVs do
   Result := Result + HVDb.HVozidla[addr].Data.Nazev + '|';
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkTrat.NavestProtismer():Integer;
begin
 case (Self.navestidla) of
  TTratNavestidla.autoblok: Result := TBlkNav._NAV_ZHASNUTO;
  TTratNavestidla.hradlo: Result := TBlkNav._NAV_STUJ;
 else
  Result := TBlkNav._NAV_ZHASNUTO;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

