unit Souprava;

// trida "TSouprava"

interface

uses IniFiles, SysUtils, Classes, Forms, IBUtils, THnaciVozidlo, houkEvent,
     Generics.Collections, predvidanyOdjezd, TBlok;

const
  _MAX_SPR_HV = 4;

type
  TSoupravaHVs = TList<Integer>; // seznam adres hnacich vozidel na souprave

  TSoupravaData = record
   nazev:string;
   pocet_vozu:Word;
   poznamka:string;
   delka:Integer;     // delka soupravy v centimetrech - podle toho se urcuje zastavovani na blocich
   typ:string;        // MOs, Os, Mn, Pn, ... - podle toho se urcuje zastavovaci udalost

   // ovlivnuje vybarvovani sipecky
   smer_L:boolean;
   smer_S:boolean;

   HVs:TSoupravaHVs; // seznam adres hnacich vozidel na souprave

   OblRizeni:TObject;
   rychlost:Integer;
   chtenaRychlost:Integer;
   smer:THVStanoviste;
   front:TObject;   // nejprednejsi blok, kde je souprava

   vychoziOR:TObject;
   cilovaOR:TObject;
   hlaseni:boolean;

   hlaseniPrehrano:boolean;
   podj:TDictionary<Integer, TPOdj>;  // id useku : predvidany odjezd
  end;//TSoupravaData

  TSouprava = class
   private
    data:TSoupravaData;
    findex:Integer;
    filefront:Integer;
    speedBuffer:PInteger;                                                       // pokud tento ukazatel neni nil, rychlost je nastavovana do promenne, na kterou ukazuje a ne primo souprave; to se hodi napriklad v zastavce v TU

    procedure Init(index:Integer);
    procedure LoadFromFile(ini:TMemIniFile; const section:string);
    procedure LoadFromPanelStr(spr:TStrings; Usek:TObject; OblR:TObject);

    procedure ReleaseAllLoko();

    procedure SetOR(OblRizeni:TObject);

    procedure HVComErr(Sender:TObject; Data:Pointer);
    procedure SetSpeed(speed:Integer);
    procedure SetSmer(smer:THVStanoviste);
    procedure SetFront(front:TObject);

    function IsUkradeno():boolean;
    function GetMaxRychlost():Cardinal;
    function GetMaxRychlostStupen():Cardinal;

   public

    changed:boolean;

    constructor Create(ini:TMemIniFile; const section:string; index:Integer); overload;
    constructor Create(panelStr:TStrings; Usek:TObject; index:Integer; OblR:TObject); overload;
    destructor Destroy(); override;

    procedure SaveToFile(ini:TMemIniFile; const section:string);

    function GetPanelString():string;   // vraci string, kterym je definovana souprava, do panelu
    procedure UpdateSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject);
    procedure SetRychlostSmer(speed:Cardinal; dir:THVStanoviste);
    procedure VezmiVlak();
    procedure UpdateFront();
    procedure ChangeSmer();
    procedure InterChangeStanice(change_ev:Boolean = true);
    procedure SetSpeedBuffer(speedBuffer:PInteger);
    procedure LokDirChanged();
    procedure CheckSH(nav:TObject);

    procedure ToggleHouk(desc:string);
    procedure SetHoukState(desc:string; state:boolean);

    procedure AddOrUpdatePOdj(usekid:Integer; var podj:TPOdj); overload;
    procedure AddOrUpdatePOdj(usek:TBlk; var podj:TPOdj); overload;
    function IsPOdj(usekid:Integer):Boolean; overload;
    function IsPOdj(usek:TBlk):Boolean; overload;
    function GetPOdj(usekid:Integer):TPOdj; overload;
    function GetPOdj(usek:TBlk):TPOdj; overload;
    procedure RemovePOdj(usekid:Integer); overload;
    procedure RemovePOdj(usek:TBlk); overload;
    procedure ClearPOdj();
    function IsAnyLokoInRegulator():Boolean;
    procedure ForceRemoveAllRegulators();

    property nazev:string read data.nazev;
    property sdata:TSoupravaData read data;
    property index:Integer read findex;
    property stanice:TObject read data.OblRizeni write SetOR;
    property rychlost:Integer read data.rychlost write SetSpeed;
    property chtenaRychlost:Integer read data.chtenaRychlost;
    property smer:THVStanoviste read data.smer write SetSmer;
    property ukradeno:boolean read IsUkradeno;
    property front:TObject read data.front write SetFront;
    property delka:Integer read data.delka;
    property typ:string read data.typ;
    property vychoziOR:TObject read data.vychoziOR;
    property cilovaOR:TObject read data.cilovaOR;
    property hlaseniPrehrano:boolean read data.hlaseniPrehrano;
    property hlaseni:boolean read data.hlaseni;
    property HVs:TSoupravaHVs read data.HVs;
    property maxRychlost:Cardinal read GetMaxRychlost; // pozor, muze byt i rychlost, ke tere nemame stupen!
    property maxRychlostStupen:Cardinal read GetMaxRychlostStupen; // je vzdy rychlost, ke ktere mame stupen

    // uvolni stara hnaci vozidla ze soupravy (pri zmene HV na souprave)
    class procedure UvolV(old:TSoupravaHVs; new:TSoupravaHVs);

  end;//TSouprava

implementation

uses THVDatabase, Logging, ownStrUtils, SprDb, TBlokUsek, DataSpr, appEv,
      DataHV, TOblsRizeni, TOblRizeni, TCPServerOR, TBloky, TBlokNav,
      fRegulator, Trakce, fMain, TBlokTratUsek, stanicniHlaseniHelper,
      stanicniHlaseni, TechnologieTrakce, Prevody;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.Init(index:Integer);
begin
 Self.speedBuffer := nil;
 Self.changed := false;
 Self.findex := index;
 Self.data.podj := TDictionary<Integer, TPOdj>.Create();
 Self.data.HVs := TList<Integer>.Create();
 Self.data.hlaseniPrehrano := false;
end;

constructor TSouprava.Create(ini:TMemIniFile; const section:string; index:Integer);
begin
 inherited Create();
 Self.Init(index);
 Self.LoadFromFile(ini, section);
end;//ctor

constructor TSouprava.Create(panelStr:TStrings; Usek:TObject; index:Integer; OblR:TObject);
begin
 inherited Create();
 Self.Init(index);
 Self.LoadFromPanelStr(panelStr, Usek, OblR);
end;//ctor

destructor TSouprava.Destroy();
begin
 Self.ReleaseAllLoko();
 Self.ClearPOdj();
 Self.data.podj.Free();
 Self.data.HVs.Free();

 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.LoadFromFile(ini:TMemIniFile; const section:string);
var addr:Integer;
    data:TStrings;
    s:string;
begin
 Self.data.nazev      := ini.ReadString(section, 'nazev', section);
 Self.data.pocet_vozu := ini.ReadInteger(section, 'vozu', 0);
 Self.data.poznamka   := ini.ReadString(section, 'poznamka', '');
 Self.data.smer_L     := ini.ReadBool(section, 'L', false);
 Self.data.smer_S     := ini.ReadBool(section, 'S', false);
 Self.data.delka      := ini.ReadInteger(section, 'delka', 0);
 Self.data.typ        := ini.ReadString(section, 'typ', '');
 Self.filefront       := ini.ReadInteger(section, 'front', -1);
 Self.data.smer       := THVStanoviste(ini.ReadInteger(section, 'smer', Integer(THVStanoviste.lichy)));

 Self.data.vychoziOR  := ORs.GetORById(ini.ReadString(section, 'z', ''));
 Self.data.cilovaOR   := ORs.GetORById(ini.ReadString(section, 'do', ''));
 Self.data.OblRizeni  := ORs.GetORById(ini.ReadString(section, 'OR', ''));
 Self.data.hlaseni    := ini.ReadBool(section, 'hlaseni', false);

 data := TStringList.Create();
 ExtractStrings([';', ','], [], PChar(ini.ReadString(section, 'HV', '')), data);

 while (data.Count > _MAX_SPR_HV) do
   data.Delete(_MAX_SPR_HV);

 // HV se nacitaji takto prapodivne pro osetreni pripadu, kdy u soupravy je uvedene HV, ktere neexistuje
 Self.data.HVs.Clear();
 try
   for s in data do
    begin
     addr := StrToInt(s);
     if (Assigned(HVDb.HVozidla[addr])) then
      begin
       HVDb.HVozidla[addr].souprava := Self.index;
       Self.data.HVs.Add(addr);
      end;
    end;
 except

 end;

 data.Free();
 Self.changed := true;
end;

procedure TSouprava.SaveToFile(ini:TMemIniFile; const section:string);
var str:string;
    addr: Integer;
begin
 ini.WriteString(section, 'nazev', Self.data.nazev);
 ini.WriteInteger(section, 'vozu', Self.data.pocet_vozu);

 if (Self.data.poznamka <> '') then
   ini.WriteString(section, 'poznamka', Self.data.poznamka)
 else
   ini.DeleteKey(section, 'poznamka');

 ini.WriteInteger(section, 'delka', Self.data.delka);
 ini.WriteString(section, 'typ', Self.data.typ);
 ini.WriteBool(section, 'L', Self.data.smer_L);
 ini.WriteBool(section, 'S', Self.data.smer_S);
 ini.WriteInteger(section, 'smer', Integer(Self.data.smer));

 if (Self.data.vychoziOR <> nil) then
   ini.WriteString(section, 'z', TOR(Self.data.vychoziOR).id)
 else
   ini.DeleteKey(section, 'z');

 if (Self.data.cilovaOR <> nil) then
   ini.WriteString(section, 'do', TOR(Self.data.cilovaOR).id)
 else
   ini.DeleteKey(section, 'do');

 if (Self.data.front <> nil) then
   ini.WriteInteger(section, 'front', (Self.data.front as TBlk).id)
 else
   ini.WriteInteger(section, 'front', -1);

 if (Self.data.OblRizeni <> nil) then
   ini.WriteString(section, 'OR', (Self.data.OblRizeni as TOR).id)
 else
   ini.DeleteKey(section, 'OR');

 ini.WriteBool(section, 'hlaseni', Self.data.hlaseni);

 str := '';
 for addr in Self.HVs do
  str := str + IntToStr(addr) + ';';
 ini.WriteString(section, 'HV', str);
end;

////////////////////////////////////////////////////////////////////////////////

// vraci string, kterym je definovana souprava, do panelu
// format dat soupravy: nazev;pocet_vozu;poznamka;smer_Lsmer_S;delka;typ;hnaci vozidla;vychozi stanice;cilova stanice
function TSouprava.GetPanelString():string;
var addr:Integer;
begin
 Result := Self.data.nazev + ';' + IntToStr(Self.data.pocet_vozu) + ';{' + Self.data.poznamka + '};';

 if (Self.data.smer_L) then
   Result := Result + '1'
 else
   Result := Result + '0';

 if (Self.data.smer_S) then
   Result := Result + '1'
 else
   Result := Result + '0';

 Result := Result + ';' + IntToStr(Self.data.delka) + ';' + Self.data.typ + ';{';

 for addr in Self.HVs do
   Result := Result + '[{' + HVDb.HVozidla[addr].GetPanelLokString() + '}]';
 Result := Result + '};';

 if (Self.vychoziOR <> nil) then
   Result := Result + TOR(Self.vychoziOR).id;
 Result := Result + ';';

 if (Self.cilovaOR <> nil) then
   Result := Result + TOR(Self.cilovaOR).id;
 Result := Result + ';';

 if (Self.data.hlaseni) then
   Result := Result + '1;'
 else
   Result := Result + '0;';
end;

////////////////////////////////////////////////////////////////////////////////

// format dat soupravy: nazev;pocet_vozu;poznamka;smer_Lsmer_S;delka;typ;hnaci vozidla;vychozi stanice;cilova stanice
// format hnaciho vozidla:  nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce
procedure TSouprava.LoadFromPanelStr(spr:TStrings; Usek:TObject; OblR:TObject);
var hvs,hv:TStrings;
    i, j, timeout, addr:Integer;
    new:TSoupravaHVs;
    Func:TFunkce;
    max_func:Integer;
    smer:THVStanoviste;
    s:string;
begin
 hvs  := TStringList.Create();
 hv   := TStringList.Create();

 try
   // zkontrolujeme, jestli nejaka souprava s timto cislem uz nahodou neexistuje
   for i := 0 to _MAX_SPR-1 do
    begin
     if (soupravy.soupravy[i] = nil) then continue;

     if ((Soupravy.soupravy[i].nazev = spr[0]) and (Soupravy.soupravy[i] <> Self)) then
      begin
       if (Soupravy.soupravy[i].stanice <> nil) then
         raise Exception.Create('Souprava '+Soupravy.soupravy[i].nazev+' již existuje v OŘ '+(Soupravy.soupravy[i].stanice as TOR).Name)
       else
         raise Exception.Create('Souprava '+Soupravy.soupravy[i].nazev+' již existuje');

       Exit();
      end;
    end;

   try
    StrToInt(spr[0]);
   except
     on E:EConvertError do
       raise Exception.Create('Číslo soupravy není validní číslo!');
   end;

   Self.changed := true;

   Self.data.nazev := spr[0];
   Self.data.pocet_vozu := StrToInt(spr[1]);
   Self.data.poznamka := spr[2];
   Self.data.smer_L := (spr[3][1] = '1');
   Self.data.smer_S := (spr[3][2] = '1');

   Self.data.delka := StrToInt(spr[4]);
   Self.data.typ   := spr[5];

   Self.data.OblRizeni := OblR;
   Self.data.front     := Usek;

   if (spr.Count > 7) then
     Self.data.vychoziOR := ORs.GetORById(spr[7]);

   if (spr.Count > 8) then
     Self.data.cilovaOR := ORs.GetORById(spr[8]);

   if (spr.Count > 9) then
     Self.data.hlaseni := (spr[9] = '1')
   else
     Self.data.hlaseni := TStanicniHlaseni.HlasitSprTyp(Self.typ);

   ExtractStringsEx([']'], ['['], spr[6], hvs);

   if (hvs.Count > _MAX_SPR_HV) then
     raise Exception.Create('Překročen maximální počet hnacích vozidel na soupravě');

   new := TList<Integer>.Create();

   try
     for s in hvs do
      begin
       hv.Clear();
       ExtractStringsEx(['|'], [], s, hv);
       addr := StrToInt(hv[4]);

       if (not Assigned(HVDb.HVozidla[addr])) then
         Exit();

       if ((HVDb.HVozidla[addr].souprava > -1) and (HVDb.HVozidla[addr].souprava <> Self.index)) then
         raise Exception.Create('Loko '+IntToStr(addr)+' již přiřazena soupravě '+Soupravy.GetSprNameByIndex(HVDb.HVozidla[addr].souprava));

       if (new.Contains(addr)) then
         raise Exception.Create('Duplicitní loko!');

       if ((not HVDb.HVozidla[addr].acquired) or (HVDb.HVozidla[addr].stolen)) then
        begin
         // pripravit funkce:
         max_func := Min(Length(hv[8]), _HV_FUNC_MAX);
         for j := 0 to max_func do
           HVDb.HVozidla[addr].Stav.funkce[j] := (hv[8][j+1] = '1');

         try
           HVDb.HVozidla[addr].TrakceAcquire(TTrakce.Callback(), TTrakce.Callback());
         except
           on E:Exception do
             raise Exception.Create('PrevzitLoko exception : '+E.Message);
         end;

         timeout := 0;
         while (not HVDb.HVozidla[addr].acquired) do
          begin
           Sleep(1);
           timeout := timeout + 1;
           Application.ProcessMessages;

           if (timeout > 1000) then  //timeout 1 sec na kazde hnaci vozidlo
            begin
             raise Exception.Create('Loko '+ IntToStr(addr) +' nepřevzato');
             Exit();
            end;
          end;//while

        end else begin
         // nastavit funkce
         for j := 0 to _HV_FUNC_MAX do
           if (j < Length(hv[8])) then
             Func[j] := (hv[8][j+1] = '1')
           else
             Func[j] := HVDb.HVozidla[addr].Stav.funkce[j];

//          TrakceI.LokSetFunc(Self, HVDb.HVozidla[addr], Func); TODO
        end;

       HVDb.HVozidla[addr].Data.Poznamka := hv[3];
       HVDb.HVozidla[addr].Stav.StanovisteA := THVStanoviste(StrToInt(hv[7]));
       HVDb.HVozidla[addr].souprava := Self.index;

       new.Add(addr);
      end;
   except
     new.Free();
     raise;
   end;

   Self.UvolV(Self.HVs, new);
   Self.data.HVs.Free();
   Self.data.HVs := new;

   if ((Self.chtenaRychlost = 0) and (Self.data.smer_L xor Self.data.smer_S)) then
    begin
     // vypocet smeru ze sipky
     if (Self.data.smer_L) then
       smer := THVStanoviste.lichy
     else
       smer := THVStanoviste.sudy;
    end else
     smer := Self.smer;

   Self.SetRychlostSmer(Self.rychlost, smer);

 finally
   hvs.Free();
   hv.Free();
 end;

 Blky.ChangeSprToTrat(Self.index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.UpdateSprFromPanel(spr:TStrings; Usek:TObject; OblR:TObject);
var nav:TBlk;
begin
 Self.LoadFromPanelStr(spr, Usek, OblR);
 (Usek as TBlkUsek).Change();

 for nav in (Usek as TBlkUsek).NavJCRef do
   (nav as TBlkNav).UpdateRychlostSpr(true);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

class procedure TSouprava.UvolV(old:TSoupravaHVs; new:TSoupravaHVs);
var new_addr, old_addr:Integer;
    keep:TList<Integer>;
begin
 keep := TList<Integer>.Create();

 try
   for new_addr in new do
     for old_addr in old do
       if (new_addr = old_addr) then
          keep.Add(new_addr);

   for old_addr in old do
    begin
     if (not keep.Contains(old_addr)) then
      begin
       // vozidlo, ktere neni v novem seznamu -> uvolnit
       HVDb.HVozidla[old_addr].souprava := -1;
      end;
    end;
 finally
   keep.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

// uvolnit vsechna loko
// pred uvolnenim loko take zastavime
procedure TSouprava.ReleaseAllLoko();
var addr:Integer;
begin
 if ((not Assigned(HVDb)) or (not Assigned(TrakceI))) then Exit();

 for addr in Self.HVs do
  begin
   if (not Assigned(HVDb.HVozidla[addr])) then
     continue;

   HVDb.HVozidla[addr].souprava := -1;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetOR(OblRizeni:TObject);
var addr:Integer;
begin
 Self.data.OblRizeni := OblRizeni;
 for addr in Self.HVs do
   HVDb.HVozidla[addr].PredejStanici(OblRizeni as TOR);
 Self.Data.hlaseniPrehrano := false;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetRychlostSmer(speed:Cardinal; dir:THVStanoviste);
var addr:Integer;
    direction:boolean;
begin
 if ((TBlk(Self.front).typ = _BLK_TU) and (TBlkTU(Self.front).rychUpdate)) then
   TBlkTU(Self.front).rychUpdate := false;

 Self.data.smer := dir;
 if (Self.speedBuffer = nil) then
  begin
   Self.data.chtenaRychlost := speed;
   if (speed > Self.maxRychlost) then
     Self.data.rychlost := Self.maxRychlost
   else
     Self.data.rychlost := speed;
  end else begin
   Self.speedBuffer^ := speed;
   Exit();
  end;

 for addr in Self.HVs do
  begin
   if (HVDb[addr].ruc) then
    begin      // pokud je loko prevzato na rucni rizeni, ignoruji ho
     writelog('LOKO ' + IntToStr(addr) + ' v ručním regulátoru, nenastavuji rychlost', WR_MESSAGE);
     continue;
    end;

   direction := PrevodySoustav.IntToBool(Integer(dir) xor Integer(HVDb[addr].stav.StanovisteA));

   try
     HVDb.HVozidla[addr].SetSpeedDir(Self.data.rychlost, direction,
                                     TTrakce.Callback(), TTrakce.Callback(Self.HVComErr), Self);
   except
     on E:Exception do
       AppEvents.LogException(E, 'TSouprava.SetRychlostSmer');
   end;
  end;

 if ((speed > 0) and (Assigned(Self.front)) and
     ((Self.front as TBlkUsek).IsVlakPresun()) and
      ((Self.front as TBlkUsek).Soupravs[(Self.front as TBlkUsek).vlakPresun] = Self.index)) then
  (Self.front as TBlkUsek).VlakPresun := -1;

 writelog('Souprava ' + Self.nazev + ' : rychlost '+IntToStr(speed)+', směr : '+IntToStr(Integer(dir)), WR_MESSAGE);

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetSpeed(speed:Integer);
begin
 Self.SetRychlostSmer(speed, Self.data.smer);
end;

procedure TSouprava.SetSmer(smer:THVStanoviste);
begin
 Self.SetRychlostSmer(Self.data.rychlost, smer);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.HVComErr(Sender:TObject; Data:Pointer);
begin
 if (Self.data.OblRizeni <> nil) then
   (Self.data.OblRizeni as TOR).BlkWriteError(nil, 'Souprava '+Self.nazev+' nekomunikuje s centrálou', 'CENTRÁLA');
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.IsUkradeno():boolean;
var addr:Integer;
begin
 for addr in Self.HVs do
  if (HVDb.HVozidla[addr].stolen) then
    Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.VezmiVlak();
var addr, timeout:Integer;
    taken:boolean;
begin
 taken := false;
 for addr in Self.HVs do
  begin
   if (HVDb.HVozidla[addr].stolen) then
    begin
     try
       HVDb.HVozidla[addr].TrakceAcquire(TTrakce.Callback(), TTrakce.Callback());
     except
       on E:Exception do
         raise Exception.Create('PrevzitLoko exception : '+E.Message);
     end;
     taken := true;

     timeout := 0;
     while (not HVDb.HVozidla[addr].acquired) do
      begin
       Sleep(1);
       timeout := timeout + 1;
       Application.ProcessMessages;

       if (timeout > 1000) then  //timeout 1 sec
        begin
         raise Exception.Create('Loko '+ IntToStr(addr) +' nepřevzato');
         Exit();
        end;
      end;//while
    end;//stolen
  end;//for i

 if (taken) then
   Self.SetRychlostSmer(Self.rychlost, Self.smer);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetFront(front:TObject);
var addr:Integer;
begin
 if (Self.data.front = front) then Exit();

 if (Assigned(Self.data.front)) then
   (Self.data.front as TBlkUsek).zpomalovani_ready := false;
 Self.data.front := front;

 // pricteme delku tohoto useku k front:
 for addr in Self.HVs do
  begin
   case (HVDb[addr].direction) of
    false : begin
      HVDb[addr].Stav.najeto_vpred.Metru := HVDb[addr].Stav.najeto_vpred.Metru +
                                            (front as TBlkUsek).GetSettings().Lenght/100;
      Inc(HVDb[addr].Stav.najeto_vpred.Bloku);
    end;
    true : begin
      HVDb[addr].Stav.najeto_vzad.Metru := HVDb[addr].Stav.najeto_vzad.Metru +
                                           (front as TBlkUsek).GetSettings().Lenght/100;
      Inc(HVDb[addr].Stav.najeto_vzad.Bloku);
    end;//case 1
   end;//case

   HVDb.HVozidla[addr].changed := true;
  end;//for

 Self.changed := true;
end;

procedure TSouprava.UpdateFront();
var blk:TBlk;
begin
 Blky.GetBlkByID(Self.filefront, blk);
 Self.front := blk;
end;

////////////////////////////////////////////////////////////////////////////////

// zmena smeru pri naslapu na smyckovy blok
procedure TSouprava.ChangeSmer();
var addr:Integer;
    tmp:boolean;
begin
 // zmenit orintaci stanoviste A hnacich vozidel
 for addr in Self.HVs do
  begin
   case (HVDb.HVozidla[addr].Stav.StanovisteA) of
    THVStanoviste.lichy : HVDb.HVozidla[addr].Stav.StanovisteA := THVStanoviste.sudy;
    THVStanoviste.sudy  : HVDb.HVozidla[addr].Stav.StanovisteA := THVStanoviste.lichy;
   end;//case
  end;//for i

 // zmenit orientaci sipky soupravy
 tmp              := Self.data.smer_L;
 Self.data.smer_L := Self.data.smer_S;
 Self.data.smer_S := tmp;

 // zmenit smer suupravy - dulezite pro zastaveni pred navestidlem
 case (Self.data.smer) of
  THVStanoviste.lichy : Self.smer := THVStanoviste.sudy;
  THVStanoviste.sudy  : Self.smer := THVStanoviste.lichy;
 end;//case

 writelog('Souprava '+ Self.nazev + ' : zmena smeru', WR_SPRPREDAT);

 if (Self.front <> nil) then
   (Self.front as TBlkUsek).Change();  // kvuli sipce
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.InterChangeStanice(change_ev:Boolean = true);
var tmp:TObject;
begin
 tmp := Self.data.vychoziOR;
 Self.data.vychoziOR := Self.data.cilovaOR;
 Self.data.cilovaOR := tmp;

 Self.changed := true;
 if ((Self.front <> nil) and (change_ev)) then
   (Self.front as TBlkUsek).Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.SetSpeedBuffer(speedBuffer:PInteger);
begin
 Self.speedBuffer := speedBuffer;
end;

////////////////////////////////////////////////////////////////////////////////
// V pripade, ze vsechna hnaci vozidla soupravy otocim do opacneho smeru,
// nez je smer soupravy, otoci se i smer soupravy. To umoznuje otoceni smeru
// soupravy z Rocomaus.
// Tato zmena je umoznena jen tehdy pokud nema sipka jednoznacne urceny smer
// a pokud souprava stoji.

procedure TSouprava.LokDirChanged();
var i:Integer;
    dir:Integer;
begin
 if ((Self.chtenaRychlost <> 0) or (Self.data.smer_L xor Self.data.smer_S) or
     (Self.HVs.Count = 0) or ((Self.front <> nil) and (not TBlkUsek(Self.front).Stav.stanicni_kolej))) then
   Exit();

 dir := Integer(HVDb[Self.HVs[0]].direction) xor Integer(HVDb[Self.HVs[0]].Stav.StanovisteA); // TODO: test this

 if (dir = Integer(Self.smer)) then Exit();
 for i := 1 to Self.HVs.Count-1 do
   if (dir <> (HVDb.HVozidla[Self.HVs[i]].Slot.smer xor
              Integer(HVDb.HVozidla[Self.HVs[i]].Stav.StanovisteA))) then
     Exit();

 // vsechna hv nastavena do opacneho smeru -> zmenit smer soupravy
 Self.smer := THVStanoviste(dir);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.ToggleHouk(desc:string);
var addr:Integer;
    HV:THV;
begin
 writelog('Souprava ' + Self.nazev + ' : aktivuji houkání ' + desc, WR_MESSAGE);

 for addr in Self.HVs do
  begin
   HV := HVDb.HVozidla[addr];
   if (HV.CanPlayHouk(desc)) then
     TrkSystem.LokFuncToggle(Self, HV, HV.funcDict[desc]);
  end;
end;

procedure TSouprava.SetHoukState(desc:string; state:boolean);
var addr:Integer;
    HV:THV;
    func:TFunkce;
begin
 if (state) then
   writelog('Souprava ' + Self.nazev + ' : aktivuji funkci ' + desc, WR_MESSAGE)
 else
   writelog('Souprava ' + Self.nazev + ' : deaktivuji funkci ' + desc, WR_MESSAGE);

 for addr in Self.HVs do
  begin
   HV := HVDb.HVozidla[addr];

   if (HV.CanPlayHouk(desc)) then
    begin
     func := HV.Stav.funkce;
     func[HV.funcDict[desc]] := state;

     try
       TrkSystem.LokSetFunc(Self, HV, func);
     except

     end;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.CheckSH(nav:TObject);
var mnav:TBlkNav;
    oblr:TOR;
    shPlay:TSHToPlay;
    shSpr:TSHSpr;
begin
 if ((not Self.hlaseni) or (Self.hlaseniPrehrano) or (self.vychoziOR = nil) or
     (self.cilovaOR = nil) or (Self.typ = '')) then Exit();

 mnav := TBlkNav(nav);
 if (mnav.OblsRizeni.Count < 1) then Exit();
 oblr := mnav.OblsRizeni[0];

 if ((not Assigned(oblr.hlaseni)) or (not oblr.hlaseni.available)) then Exit();

 try
   shPlay := stanicniHlaseniHelper.CanPlayPrijezdSH(self, oblr);
 except
   on E:Exception do
     AppEvents.LogException(E, 'CanPlayPrijezdSH');
 end;

 shSpr.cislo := Self.nazev;
 shSpr.typ   := Self.typ;
 shSpr.fromORid := TOR(Self.vychoziOR).id;
 shSpr.toORid := TOR(Self.cilovaOR).id;
 shSpr.timeArrive := 0;
 shSpr.timeDepart := 0;

 if (shPlay.stanicniKolej <> nil) then
  begin
   shSpr.kolej := shPlay.stanicniKolej.Stav.cislo_koleje;

   if ((Self.IsPOdj(shPlay.stanicniKolej)) and (Self.GetPOdj(shPlay.stanicniKolej).abs_enabled)) then
     shSpr.timeDepart := Self.GetPOdj(shPlay.stanicniKolej).abs;
  end;

 try
   if ((shPlay.stanicniKolej <> nil) and ((shPlay.trat = nil) or (Self.IsPOdj(shPlay.stanicniKolej)))) then begin
     oblr.hlaseni.Prijede(shSpr);
     Self.data.hlaseniPrehrano := true;
   end else if (shPlay.trat <> nil) then begin
     oblr.hlaseni.Projede(shSpr);
     Self.data.hlaseniPrehrano := true;
   end;
 except
   on E:Exception do
     AppEvents.LogException(E, 'Prehravani hlaseni');
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// Predvidane odjezdy:

procedure TSouprava.AddOrUpdatePOdj(usekid:Integer; var podj:TPOdj);
begin
 if ((not podj.rel_enabled) and (not podj.abs_enabled)) then
  begin
   if (Self.data.podj.ContainsKey(usekid)) then
    begin
     Self.data.podj[usekid].Free();
     Self.data.podj.Remove(usekid);
    end;
   FreeAndNil(podj);
  end else begin
   if (Self.data.podj.ContainsKey(usekid)) then
     Self.data.podj[usekid].Free();
   Self.data.podj.AddOrSetValue(usekid, podj);
   podj := nil;
  end;
end;

function TSouprava.IsPOdj(usekid:Integer):Boolean;
begin
 Result := Self.data.podj.ContainsKey(usekid);
end;

function TSouprava.GetPOdj(usekid:Integer):TPOdj;
begin
 Result := Self.data.podj[usekid];
end;

procedure TSouprava.RemovePOdj(usekid:Integer);
begin
 Self.data.podj[usekid].Free();
 Self.data.podj.Remove(usekid);
end;

procedure TSouprava.AddOrUpdatePOdj(usek:TBlk; var podj:TPOdj);
begin
 Self.AddOrUpdatePOdj(usek.id, podj);
end;

function TSouprava.IsPOdj(usek:TBlk):Boolean;
begin
 Result := Self.IsPOdj(usek.id);
end;

function TSouprava.GetPOdj(usek:TBlk):TPOdj;
begin
 Result := Self.GetPOdj(usek.id);
end;

procedure TSouprava.RemovePOdj(usek:TBlk);
begin
 Self.RemovePOdj(usek.id);
end;

procedure TSouprava.ClearPOdj();
var podj:TPOdj;
begin
 for podj in Self.data.podj.Values do
   podj.Free();

 Self.data.podj.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.IsAnyLokoInRegulator():Boolean;
var hvaddr:Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSouprava.ForceRemoveAllRegulators();
var hvaddr:Integer;
begin
 for hvaddr in Self.HVs do
   if (HVDb[hvaddr].Stav.regulators.Count > 0) then
     HVDb[hvaddr].ForceRemoveAllRegulators();
end;

////////////////////////////////////////////////////////////////////////////////

function TSouprava.GetMaxRychlost():Cardinal;
var addr:Integer;
    min:Cardinal;
begin
 if (Self.HVs.Count = 0) then
   Exit(THnaciVozidlo._DEFAUT_MAX_SPEED);

 min := HVDb[Self.HVs[0]].Data.maxRychlost;
 for addr in Self.HVs do
   if (HVDb[addr].Data.maxRychlost < min) then
     min := HVDb[addr].Data.maxRychlost;

 Result := min;
end;

function TSouprava.GetMaxRychlostStupen():Cardinal;
begin
 // vraci rychlost <= max rychlosti takovou, ze pro ni mame prirazeni stupne
 // tj. tuto rychlost lze skutene nastavit
 Result := TrkSystem.NearestLowerSpeed(Self.maxRychlost);
end;

////////////////////////////////////////////////////////////////////////////////


end.//unit
