unit TechnologieJC;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Buttons, ComCtrls, Main, RPConst, TBloky, TBlok,
  IniFiles, IdContext, TBlokTrat, Generics.Collections, UPO;

const
  _JC_TIMEOUT_SEC = 20;
  _NC_TIMEOUT_MIN = 1;

type
  // bariery ve staveni jizdni cesty:
  TJCBariera = record
   typ:Integer;
   blok:TBlk;
   param:Integer;
  end;
  TJCBariery = TList<TJCBariera>;

  TJCVyhZaver=record
   Blok:Integer;
   Poloha:TVyhPoloha;
  end;

  TJCOdvratZaver=record
   Blok:Integer;
   Poloha:TVyhPoloha;
   ref_blk:Integer;
  end;

  TJCPrislZaver=record
   Blok:Integer;
   ref_blk:Integer;
  end;

  TJCPrjZaver=record
   Prejezd:Integer;
   uzaviraci:TArSmallI;    // uzaviraci bloky prejezdu
   oteviraci:Integer;      // oteviraci blok prejezdu
  end;

  TJCZamZaver = record
   Blok:Integer;
   ref_blk:Integer;
  end;

  TJCStaveni = record
   Krok:Integer;
   TimeOut:TDateTime;
   SenderOR:TObject;
   SenderPnl:TIdContext;
   RozpadBlok,RozpadRuseniBlok:Integer;     // -5 = cesta neni postavena, -2 = navestidlo na STUJ, -1 = usek pred navestidlem, 0..n = useky JC
   from_stack:TObject;
   nc:boolean;                              // pokud stavime nouzovou cestu
   ncBariery:TJCBariery;                    // baeriery do potvrzovaci sekvence pri staveni nouzove cesty
   ncBarieryCntLast:Integer;
  end;

  TJCprop = record
   Nazev:string;
   NavestidloBlok:Integer;
   TypCesty:TJCType;
   DalsiNNavaznost:Integer;
   DalsiNNavaznostTyp:Byte;

   Vyhybky  : TList<TJCVyhZaver>;
   Useky    : TList<Integer>;
   Odvraty  : TList<TJCOdvratZaver>;
   Prisl    : TList<TJCPrislZaver>;
   Prejezdy : TList<TJCPrjZaver>;
   podminky : record
    vyhybky  : TList<TJCVyhZaver>;
    zamky    : TList<TJCZamZaver>;
   end;
   vb:TList<Integer>;  // variantni body - odkaz na ID bloku

   Trat:Integer;
   TratSmer:TtratSmer;
   RychlostNoDalsiN,RychlostDalsiN:Byte;
  end;

  TJC=class
   public const

    // bariery ve staveni jizdni cesty:

    _JCB_OK                      = 0;
    _JCB_STAVENI                 = 1;
    _JCB_BLOK_DISABLED           = 2;
    _JCB_BLOK_NOT_EXIST          = 3;
    _JCB_BLOK_NOT_TYP            = 4;
    _JCB_PRIVOLAVACKA            = 5;

    _JCB_SCOM_NOT_USEK           = 10;

    _JCB_USEK_OBSAZENO           = 20;
    _JCB_USEK_ZAVER              = 21;
    _JCB_USEK_VYLUKA             = 22;
    _JCB_USEK_SOUPRAVA           = 23;
    _JCB_USEK_STITEK             = 24;

    _JCB_VYHYBKA_KONC_POLOHA     = 30;
    _JCB_VYHYBKA_VYLUKA          = 31;
    _JCB_VYHYBKA_STITEK          = 32;
    _JCB_VYHYBKA_ZAMCENA         = 33;
    _JCB_VYHYBKA_NOUZ_ZAVER      = 34;
    _JCB_VYHYBKA_NESPAVNA_POLOHA = 35;

    _JCB_PREJEZD_NOUZOVE_OTEVREN = 40;
    _JCB_PREJEZD_PORUCHA         = 41;
    _JCB_PREJEZD_STITEK          = 42;
    _JCB_PREJEZD_NEUZAVREN       = 43;

    _JCB_ODVRAT_ZAMCENA          = 60;
    _JCB_ODVRAT_OBSAZENA         = 61;
    _JCB_ODVRAT_KONC_POLOHA      = 62;

    _JCB_TRAT_ZAK                = 70;
    _JCB_TRAT_ZAVER              = 71;
    _JCB_TRAT_OBSAZENO           = 72;
    _JCB_TRAT_ZADOST             = 73;
    _JCB_TRAT_NESOUHLAS          = 74;
    _JCB_TRAT_NO_BP              = 75;

    _JCB_ZAMEK_NEUZAMCEN         = 80;
    _JCB_ZAMEK_NOUZ_ZAVER        = 81;

    _JCB_HV_RUC                  = 100;
    _JCB_HV_NOT_ALL_RUC          = 101;


   private const
    _def_jc_staveni : TJCStaveni = (
     Krok : 0;
     RozpadBlok : -5;
     RozpadRuseniBlok : -5
    );

   private
     fproperties: TJCprop;
     fstaveni:TJCStaveni;

      procedure RusZacatekJC();
      procedure RusKonecJC();
      procedure RusVBJC();
      procedure PredejDataDalsimuBloku();       //predani dat dalsimu bloku v poradi
      procedure CheckSmyckaBlok(blk:TBlk);

      function GetStaveni():boolean;
      function GetPostaveno():boolean;

      procedure PS_vylCallback(Sender:TIdContext; success:boolean);
      procedure UPO_OKCallback(Sender:TObject);
      procedure UPO_EscCallback(Sender:TObject);
      procedure NC_PS_Callback(Sender:TIdContext; success:boolean);

      procedure UsekClosePrj(Sender:TObject; data:integer);

      procedure SetRozpadBlok(RozpadBlok:Integer);
      procedure SetRozpadRuseniBlok(RozpadRuseniBlok:Integer);
      procedure SetKrok(Krok:Integer);
      procedure CritBarieraEsc(Sender:TObject);

      procedure VyhNeprestavenaJCPC(Sender:TObject);
      procedure VyhNeprestavenaNC(Sender:TObject);
      procedure VyhPrestavenaNC(Sender:TObject);

      procedure KontrolaPodminekVCPC(var bariery:TList<TJCBariera>);
      procedure KontrolaPodminekNC(var bariery:TList<TJCBariera>);
      procedure PodminkyNCStaveni(var bariery:TList<TJCBariera>);

      function BarieryNCToPotvr(bariery:TJCBariery):TPSPodminky;

   public

     changed:boolean;

     // tehnologicke jizdnch cest rozeznava nekolik druhu berier pro postaveni jizdnich cest
     //   1) kriticke bariery jsou takove bariery, ktere dispecer nemuze odstranit (v ceste napriklad chybi tecnologicky blok)
     //   2) standartni bariery jsou takove bariery, ktere se odstrani "samy" - napriklad usek pod zaverem, obsazney usek
     //   3) varovne bariery jsou takove bariery, kteer primo nebrani jizdni ceste ve staveni, ale je potreba si je uvedmit a potvrdit je (napr. na useku je stitek, ci vyluka)
     // pak jsou tu bariery, ktere je potreba potvrdit jeste potvrzovaci sekvenci
      class function JCBariera(typ:Integer; Blok:TBlk = nil; param:Integer = 0):TJCBariera;
      class function JCBarieraToMessage(Bariera:TJCBariera):TUPOItem;
      class function CriticalBariera(typ:Integer):boolean;
      class function WarningBariera(typ:Integer):boolean;
      class function PotvrSekvBariera(typ:Integer):boolean;

      class function PotvrSekvBarieraToReason(typ:Integer):string;

      constructor Create(); overload;
      constructor Create(data:TJCprop); overload;
      destructor Destroy(); override;

      procedure NastavSCom();                   //nastavi pozadovanu navest pri postaveni JC
      procedure RusJC();                        //rusi vlakovou cestu
      procedure RusJCWithoutBlk();              //rusi vlakovou cestu
      procedure UsekyRusJC();

      procedure UpdateStaveni();
      procedure UpdateTimeOut();
      procedure CancelStaveni(reason:string = ''; stack_remove:boolean = false);

      function LoadData(ini:TMemIniFile; section:string):Byte;
      procedure SaveData(ini:TMemIniFile; section:string);

      procedure StavJC(SenderPnl:TIdContext; SenderOR:TObject; from_stack:TObject = nil; nc:boolean = false);

      function CanDN():boolean;   // true = je mozno DN; tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
      procedure DN();             // DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
      procedure STUJ();
      procedure RNZ();

      function KontrolaPodminek(NC:boolean = false):TJCBariery;

      function GetRNZ():TPSPodminky;

      property data:TJCprop read fproperties write fproperties;
      property stav:TJCStaveni read fstaveni;
      property staveni:boolean read GetStaveni;
      property nazev:string read fproperties.Nazev;
      property postaveno:boolean read GetPostaveno;   // true pokud je posatvena navest

      property RozpadBlok:Integer read fstaveni.RozpadBlok write SetRozpadBlok;
      property RozpadRuseniBlok:Integer read fstaveni.RozpadRuseniBlok write SetRozpadRuseniBlok;
      property Krok:Integer read fstaveni.Krok write SetKrok;
  end;

implementation

uses GetSystems, TechnologieMTB, Settings,
     TBlokSCom, TBlokUsek, TBlokVyhybka, TOblsRizeni, TOblRizeni,
     TBlokPrejezd, TJCDatabase, Logging, TCPServerOR, SprDb,
     THVDatabase, Zasobnik, TBlokUvazka, TBlokZamek;

////////////////////////////////////////////////////////////////////////////////

constructor TJC.Create();
begin
 inherited Create();

 Self.changed  := true;
 Self.fstaveni := _def_jc_staveni;
 Self.fstaveni.ncBariery := TList<TJCBariera>.Create();

 Self.fproperties.podminky.vyhybky := TList<TJCVyhZaver>.Create();
 Self.fproperties.podminky.zamky   := TList<TJCZamZaver>.Create();
 Self.fproperties.vb               := TList<Integer>.Create();

 Self.fproperties.Vyhybky  := TList<TJCVyhZaver>.Create();
 Self.fproperties.Useky    := TList<Integer>.Create();
 Self.fproperties.Odvraty  := TList<TJCOdvratZaver>.Create();
 Self.fproperties.Prisl    := TList<TJCPrislZaver>.Create();
 Self.fproperties.Prejezdy := TList<TJCPrjZaver>.Create();
end;//ctor

constructor TJC.Create(data:TJCprop);
begin
 inherited Create();

 Self.fproperties := data;
 Self.fstaveni := _def_jc_staveni;
 if (not Assigned(Self.fstaveni.ncBariery)) then Self.fstaveni.ncBariery := TList<TJCBariera>.Create();

 if (not Assigned(Self.fproperties.podminky.vyhybky)) then Self.fproperties.podminky.vyhybky := TList<TJCVyhZaver>.Create();
 if (not Assigned(Self.fproperties.podminky.zamky))   then Self.fproperties.podminky.zamky   := TList<TJCZamZaver>.Create();
 if (not Assigned(Self.fproperties.vb))               then Self.fproperties.vb               := TList<Integer>.Create();
 if (not Assigned(Self.fproperties.Odvraty))          then Self.fproperties.Odvraty  := TList<TJCOdvratZaver>.Create();
 if (not Assigned(Self.fproperties.Prisl))            then Self.fproperties.Prisl    := TList<TJCPrislZaver>.Create();
 if (not Assigned(Self.fproperties.Prejezdy))         then Self.fproperties.Prejezdy := TList<TJCPrjZaver>.Create();
 if (not Assigned(Self.fproperties.Vyhybky))          then Self.fproperties.Vyhybky := TList<TJCVyhZaver>.Create();
 if (not Assigned(Self.fproperties.Useky))            then Self.fproperties.Useky   := TList<Integer>.Create();
end;//ctor

destructor TJC.Destroy();
begin
 if (Assigned(Self.fstaveni.ncBariery)) then FreeAndNil(Self.fstaveni.ncBariery);
 if (Assigned(Self.fproperties.podminky.vyhybky)) then FreeAndNil(Self.fproperties.podminky.vyhybky);
 if (Assigned(Self.fproperties.podminky.zamky)) then FreeAndNil(Self.fproperties.podminky.zamky);
 if (Assigned(Self.fproperties.vb)) then  Self.fproperties.vb.Free();

 if (Assigned(Self.fproperties.Vyhybky))  then Self.fproperties.Vyhybky.Free();
 if (Assigned(Self.fproperties.Useky))    then Self.fproperties.Useky.Free();
 if (Assigned(Self.fproperties.Odvraty))  then Self.fproperties.Odvraty.Free();
 if (Assigned(Self.fproperties.Prisl))    then Self.fproperties.Prisl.Free();
 if (Assigned(Self.fproperties.Prejezdy)) then Self.fproperties.Prejezdy.Free();

 inherited Destroy();
end;//ctor

////////////////////////////////////////////////////////////////////////////////

// kontroluje podminky pro staveni konkretni jizdni cesty
// vraci List prblemu (tzv. bariery), ktere definuji to, proc jizdni cestu nelze postavit (tedy vraci vsechny nesplnene podminky)
// tzv. kriticke bariery jsou vzdy na zacatu Listu
function TJC.KontrolaPodminek(NC:boolean = false):TJCBariery;
var i,j:Integer;
    Blk,blk2:TBlk;
    privol:TBlksList;
begin
  Result := TList<TJCBariera>.Create();

  if (Self.staveni) then
    Result.Add(Self.JCBariera(_JCB_STAVENI));

  // kontrola useku navestidla:
  if (Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk) <> 0) then
   begin
    // blok navestidla neexistuje
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  if (Blk.GetGlobalSettings().typ <> _BLK_SCOM) then
   begin
    // blok navestidla neni typu navestidlo
    Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  // blok disabled
  if ((Blk as TBlkSCom).Navest < 0) then
   begin
    Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
    Exit;
   end;

  if (Blky.GetBlkByID((Blk as TBlkSCom).UsekID, Blk) <> 0) then
   begin
    // blok navestidla pred sebou nema zadny usek
    Result.Add(Self.JCBariera(_JCB_SCOM_NOT_USEK, Blk, Self.fproperties.NavestidloBlok));
    Exit;
   end;

  // vyhybky:
  // kontrolujeme, jestli vyhybky existuji a jestli jsou to vyhybky
  for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   begin
    if (Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Vyhybky[i].Blok));
      Exit;
     end;//if

    if (Blk.GetGlobalSettings().typ <> _BLK_VYH) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.Vyhybky[i].Blok));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;
   end;//for i

  // useky:
  for i := 0 to Self.fproperties.Useky.Count-1 do
   begin
    // zkontrolujeme, jestli useky existuji a jestli jsou to useky
    if (Blky.GetBlkByID(Self.fproperties.Useky[i], Blk) <> 0) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Useky[i]));
      Exit;
     end;//if

    if (Blk.GetGlobalSettings().typ <> _BLK_USEK) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_NOT_TYP, Blk, Self.fproperties.Useky[i]));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkUsek).Stav.Stav = TUsekStav.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;
   end;//for i

  // kontrola existence bloku prislusenstvi
  for i := 0 to Self.fproperties.Prisl.Count-1 do
   begin
    if (Blky.GetBlkByID(Self.fproperties.Prisl[i].ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Prisl[i].ref_blk));
      Exit;
     end;
    if (Blky.GetBlkByID(Self.fproperties.Prisl[i].Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Prisl[i].Blok));
      Exit;
     end;
   end;//for i

  // kontrola prejezdu
  for i := 0 to Self.fproperties.Prejezdy.Count-1 do
   begin
    // kontrola existence bloku prejezdu
    if (Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Prejezdy[i].Prejezd));
      Exit;
     end;

    // kontrola typu bloku prejezdu
    if (blk.GetGlobalSettings.typ <> _BLK_PREJEZD) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Prejezdy[i].Prejezd));
      Exit;
     end;

    // blok disabled
    if ((Blk as TBlkPrejezd).Stav.basicStav = TBlkPrjBasicStav.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;

    // kontrola existence oteviraciho bloku
    if (Blky.GetBlkByID(Self.fproperties.Prejezdy[i].oteviraci, blk2) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, blk, Self.fproperties.Prejezdy[i].oteviraci));
      Exit;
     end;

    // kontrola typu oteviraciho bloku
    if (blk2.GetGlobalSettings().typ <> _BLK_USEK) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Prejezdy[i].oteviraci));
      Exit;
     end;

    // kontrola existence uzaviracich bloku a jejich typu
    for j := 0 to Length(Self.fproperties.Prejezdy[i].uzaviraci)-1 do
     begin
      if (Blky.GetBlkByID(Self.fproperties.Prejezdy[i].uzaviraci[j], blk2) <> 0) then
       begin
        Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, blk, Self.fproperties.Prejezdy[i].uzaviraci[j]));
        Exit;
       end;
      if (blk2.GetGlobalSettings.typ <> _BLK_USEK) then
       begin
        Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Prejezdy[i].uzaviraci[j]));
        Exit;
       end;
     end;//for j

   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.Odvraty.Count-1 do
   begin
    if (Blky.GetBlkByID(Self.fproperties.Odvraty[i].ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Odvraty[i].ref_blk));
      Exit;
     end;
    if (Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Odvraty[i].Blok));
      Exit;
     end;
    if (blk.GetGlobalSettings().typ <> _BLK_VYH) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Odvraty[i].Blok));
      Exit;
     end;
    // blok disabled
    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;
   end;//for i

  // trat
  if (Self.fproperties.Trat > -1) then
   begin
    if (Blky.GetBlkByID(Self.fproperties.Trat, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.Trat));
      Exit;
     end;
    if (blk.GetGlobalSettings().typ <> _BLK_TRAT) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, Self.fproperties.Trat));
      Exit;
     end;
    // blok disabled
    if ((Blk as TBlkTrat).stav.smer = TTratSmer.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;
   end;

  // kontrola podminkovych bloku vyhybek
  for i := 0 to Self.fproperties.podminky.vyhybky.Count-1 do
   begin
    if (Blky.GetBlkByID(Self.fproperties.podminky.vyhybky[i].Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.podminky.vyhybky[i].Blok));
      Exit;
     end;
    if (blk.GetGlobalSettings().typ <> _BLK_VYH) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, blk.GetGlobalSettings().id));
      Exit;
     end;
    // blok disabled
    if ((Blk as TBlkVyhybka).Stav.poloha = TVyhPoloha.disabled) then
     begin
      Result.Add(Self.JCBariera(_JCB_BLOK_DISABLED, Blk, Blk.GetGlobalSettings().id));
      Exit;
     end;
   end;//for i

  // kontrola podminkovych bloku zamku
  for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
   begin
    if (Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.podminky.zamky[i].Blok));
      Exit;
     end;
    if (blk.GetGlobalSettings().typ <> _BLK_ZAMEK) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_TYP, blk, blk.GetGlobalSettings().id));
      Exit;
     end;
    if (Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].ref_blk, blk) <> 0) then
     begin
      Result.Insert(0, Self.JCBariera(_JCB_BLOK_NOT_EXIST, nil, Self.fproperties.podminky.zamky[i].ref_blk));
      Exit;
     end;
   end;//for i

 if (NC) then
  Self.KontrolaPodminekNC(Result)
 else
  Self.KontrolaPodminekVCPC(Result);

 // kontrola zaplych privolavacich navesti
 privol := Blky.GetSComPrivol(Self.fstaveni.SenderOR as TOR);

 for i := 0 to privol.Count-1 do
   Result.Add(Self.JCBariera(_JCB_PRIVOLAVACKA, privol[i] as TBlk, (privol[i] as TBlk).GetGlobalSettings().id));

 if (Assigned(privol)) then privol.Free();
end;//function

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek vlakove a posunove cesty

procedure TJC.KontrolaPodminekVCPC(var bariery:TList<TJCBariera>);
var i:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
    flag:boolean;
begin
  // useky:
  for i := 0 to Self.fproperties.Useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // obsazenost
    if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.TypCesty <> TJCType.posun)) then
     begin
      if ((Blk as TBlkUsek).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.GetGlobalSettings.id));
     end;//if

    // zaver
    if ((Blk as TBlkUsek).Zaver <> TJCType.no) then
      bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // souprava
    if (((Blk as TBlkUsek).Souprava > -1) and (Self.fproperties.TypCesty = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_SOUPRAVA, Blk, Blk.GetGlobalSettings.id));

    // vyluka
    if ((Blk as TBlkUsek).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk, blk.GetGlobalSettings.id));

    // stitek
    if ((Blk as TBlkUsek).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk, blk.GetGlobalSettings.id));
   end;//for i

  // kontrola vyhybek:
  for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Integer((Blk as TBlkVyhybka).poloha) < 0) or (Integer((Blk as TBlkVyhybka).poloha) > 1)) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.GetGlobalSettings.id));

    // zaver nema smysl kontrolovat - zaver vyhybek je prakticky zaver useku
    // proto ho staci zkontrolovat jen u useku

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.GetGlobalSettings.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.GetGlobalSettings.id));

    // kontrola nouzoveho zaveru:
    if (((Blk as TBlkVyhybka).vyhZaver) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    // pokud nemam ja polohu, prespokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku neni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
     begin
      if ((Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.GetGlobalSettings.id));

      if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.GetGlobalSettings.id));
     end;
   end;//for i

  // kontrola prejezdu
  for i := 0 to Self.fproperties.Prejezdy.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);
    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, Self.fproperties.Prejezdy[i].Prejezd));
     end else begin
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, Self.fproperties.Prejezdy[i].Prejezd));
     end;//else NouzoveOtevreni

    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.GetGlobalSettings.id));
   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.Odvraty.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Integer((Blk as TBlkVyhybka).poloha) < 0) or (Integer((Blk as TBlkVyhybka).poloha) > 1)) then
      bariery.Add(Self.JCBariera(_JCB_ODVRAT_KONC_POLOHA, blk, Self.fproperties.Odvraty[i].Blok));

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.GetGlobalSettings.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.GetGlobalSettings.id));

    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Odvraty[i].Poloha) then
     begin
      if (((Blk as TBlkVyhybka).Zaver <> TJCType.no) or ((Blk as TBlkVyhybka).redukce_menu)) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_ZAMCENA, blk, Self.fproperties.Odvraty[i].Blok));

      if ((Blk as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_OBSAZENA, blk, Self.fproperties.Odvraty[i].Blok));

      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));
     end;//if poloha <> Poloha

    // to-do: odvrat spojka
   end;//for i

  // kontrola trati
  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);
    glob := Blk.GetGlobalSettings();
    if ((blk as TBlkTrat).ZAK) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAK, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Zaver) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAVER, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Obsazeno) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_OBSAZENO, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Zadost) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZADOST, blk, Self.fproperties.Trat));
    if ((((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.souhlas) or ((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.nabidka) or (((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.bezsouhas) and ((blk as TBlkTrat).nouzZaver)))
        and (Self.fproperties.TratSmer <> (blk as TBlkTrat).Smer)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
   end;

  // kontrola polohy podminkovych vyhybek:
  for i := 0 to Self.fproperties.podminky.vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.podminky.vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NESPAVNA_POLOHA, blk, blk.GetGlobalSettings().id));
   end;//for i

  // kontrola uzamceni podminkovych zamku:
  for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkZamek).klicUvolnen) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NEUZAMCEN, blk, blk.GetGlobalSettings().id));
   end;//for i

 // kontrola ukradene loko v souprave pred navestidlem
 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk2);
 Blky.GetBlkByID((Blk2 as TBlkSCom).UsekID, Blk);

 if ((Blk as TBlkUsek).Souprava > -1) then
  begin
   flag := false;

   // kontrola rucniho rizeni lokomotiv
   for i := 0 to Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.cnt-1 do
     if (HVDb.HVozidla[Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.HVs[i]].Slot.stolen) then
      begin
       bariery.Add(Self.JCBariera(_JCB_HV_RUC, nil, Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.HVs[i]));
       flag := true;
      end;

   // pokud jsou jen nektere lokomotivy rizene rucne
   if (flag) then
     for i := 0 to Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.cnt-1 do
       if (not HVDb.HVozidla[Soupravy.soupravy[(Blk as TBlkUsek).Souprava].sdata.HV.HVs[i]].Slot.stolen) then
        begin
         bariery.Add(Self.JCBariera(_JCB_HV_NOT_ALL_RUC));
         break;
        end;

  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// kontrola podminek nouzove cesty:

procedure TJC.KontrolaPodminekNC(var bariery:TList<TJCBariera>);
var i:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
begin
  // nouzovou cestu nelze postavit jen pres useky se zaverem, jinak lze vsechy bariery prekonat

  // useky:
  for i := 0 to Self.fproperties.Useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // zaver
    if ((Blk as TBlkUsek).Zaver <> TJCType.no) then
      bariery.Add(Self.JCBariera(_JCB_USEK_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // vyluka
    if ((Blk as TBlkUsek).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_VYLUKA, blk, blk.GetGlobalSettings.id));

    // stitek
    if ((Blk as TBlkUsek).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_USEK_STITEK, blk, blk.GetGlobalSettings.id));
   end;//for i

  // kontrola vyhybek:
  for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.GetGlobalSettings.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.GetGlobalSettings.id));

    // kontrola nouzoveho zaveru:
    if (((Blk as TBlkVyhybka).vyhZaver) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    // pokud nemam ja polohu, prespokladam, ze spojka bude muset byt prestavena -> musi byt volna, bez zaveru, ...
    // kontrolovat zaver z useku eni potreba - pokud je problem se zaverem, vyvstane uz na useku JC, jinak je vyhybka v poloze, ktere zaver nevadi
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
     begin
      if ((Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.GetGlobalSettings.id));
     end;
   end;//for i

  // kontrola prejezdu
  for i := 0 to Self.fproperties.Prejezdy.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);
    // kontrola stitku prejezdu:
    if ((Blk as TBlkPrejezd).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_STITEK, Blk, Blk.GetGlobalSettings.id));
   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.Odvraty.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola vyluky vyhybky:
    if ((Blk as TBlkVyhybka).Vyluka <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_VYLUKA, Blk, Blk.GetGlobalSettings.id));

    // kontrola stitku vyhybky:
    if ((Blk as TBlkVyhybka).Stitek <> '') then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_STITEK, Blk, Blk.GetGlobalSettings.id));

    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Odvraty[i].Poloha) then
     begin
      if (((Blk as TBlkVyhybka).Zaver <> TJCType.no) or ((Blk as TBlkVyhybka).redukce_menu)) then
        bariery.Add(Self.JCBariera(_JCB_ODVRAT_ZAMCENA, blk, Self.fproperties.Odvraty[i].Blok));

      if ((Blk as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));
     end;//if poloha <> Poloha

    // to-do: odvrat spojka
   end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// stavi konkretni jizdni cestu
// tato fce ma za ukol zkontrolovat vstupni podminky jizdni cesty
// tato funkce jeste nic nenastavuje!
procedure TJC.StavJC(SenderPnl:TIdContext; SenderOR:TObject; from_stack:TObject = nil; nc:boolean = false);
var i:Integer;
    bariery:TJCBariery;
    critical:boolean;
    upo:TUPOItems;
    item:TUPOItem;
 begin
  // timeout:
  if (nc) then
   Self.fstaveni.TimeOut := Now + EncodeTime(0, _NC_TIMEOUT_MIN, 0, 0)
  else
   Self.fstaveni.TimeOut := Now + EncodeTime(0, 0, _JC_TIMEOUT_SEC, 0);

  Self.fstaveni.from_stack := from_stack;
  Self.fstaveni.SenderOR   := SenderOR;
  Self.fstaveni.SenderPnl  := SenderPnl;
  Self.fstaveni.nc         := nc;

  writelog('JC '+Self.Nazev+' - po�adavek na stav�n�, kontroluji podm�nky', WR_VC);

  bariery := Self.KontrolaPodminek(Self.fstaveni.nc);
  upo := TList<TUPOItem>.Create;

  // existuji kriticke bariery?
  critical := false;
  for i := 0 to bariery.Count-1 do
   if ((Self.CriticalBariera(bariery[i].typ)) or (not Self.WarningBariera(bariery[i].typ))) then
    begin
     critical := true;
     upo.Add(Self.JCBarieraToMessage(bariery[i]));
    end;
  if (critical) then
   begin
    // kriticke bariey existuji -> oznamim je
    Self.Krok := 1;
    writelog('JC '+Self.Nazev+' : celkem '+IntToStr(bariery.Count)+' bari�r, ukon�uji stav�n�', WR_VC);
    ORTCPServer.UPO(Self.fstaveni.SenderPnl, upo, true, nil, Self.CritBarieraEsc, Self);
    bariery.Free();
    upo.Free();
    Exit();
   end else begin
    // bariery k potvrzeni
    if ((bariery.Count > 0) or ((nc) and (from_stack <> nil))) then
     begin
      writelog('JC '+Self.Nazev+' : celkem '+IntToStr(bariery.Count)+' warning bari�r, ��d�m potvrzen�...', WR_VC);
      for i := 0 to bariery.Count-1 do
       upo.Add(Self.JCBarieraToMessage(bariery[i]));

      // pokud se jedna o NC ze zasobniku, zobrazuji jeste upozorneni na NC
      if ((nc) and (from_stack <> nil)) then
       begin
        item[0] := GetUPOLine('Pozor !', taCenter, clYellow, $A0A0A0);
        item[1] := GetUPOLine('Stav�n� nouzov� cesty.');
        item[2] := GetUPOLine('');
        upo.Add(item);
       end;

      ORTCPServer.UPO(Self.fstaveni.SenderPnl, upo, false, Self.UPO_OKCallback, Self.UPO_EscCallback, Self);
      Self.Krok := 5;
      bariery.Free();
      upo.Free();
      Exit();
     end;
   end;

  // v jzdni ceste nejsou zadne bariery -> stavim
  writelog('JC '+Self.Nazev+' : ��dn� bari�ry, stav�m', WR_VC);

  if (Self.fstaveni.nc) then
    Self.Krok := 100
  else
    Self.Krok := 10;

  bariery.Free();
  upo.Free();
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.PS_vylCallback(Sender:TIdContext; success:boolean);
var
    bariery:TJCBariery;
    critical:boolean;
    i:Integer;
begin
 // pro potvrzovaci sekvenci vyluky by mel byt krok '6'
 if (Self.Krok <> 6) then Exit;

 if (not success) then
  begin
   Self.CancelStaveni('');
   Exit();
  end;

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 bariery := Self.KontrolaPodminek(Self.fstaveni.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to bariery.Count-1 do
  if ((bariery[i].typ <> _JCB_STAVENI) and ((Self.CriticalBariera(bariery[i].typ)) or (not Self.WarningBariera(bariery[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelStaveni('Nelze postavit - kritick� bari�ry');
   ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nelze postavit '+Self.nazev+' - kritick� bari�ry', (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
   bariery.Free();
   Exit();
  end;

 writelog('JC '+Self.Nazev+' : krok 2 : povrzovaci sekvence OK',WR_VC);
 if (Self.fstaveni.nc) then
   Self.Krok := 100
 else
   Self.Krok := 10;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// callbacky z upozornovacich barier:

procedure TJC.UPO_OKCallback(Sender:TObject);
var
    bariery:TJCBariery;
    critical:boolean;
    i:Integer;
    nav, usek:TBlk;
    podm:TList<TPSPodminka>;
begin
 if (Self.Krok <> 5) then Exit();

 writelog('JC '+Self.Nazev+' : krok 1 : upozorn�n� schv�lena, kontroluji znovu bari�ry', WR_VC);

 // znovu zkontrolujeme bariery (behem potvrzovani se mohly vyskytnout)
 bariery := Self.KontrolaPodminek(Self.fstaveni.nc);

 // existuji kriticke bariery?
 critical := false;
 for i := 0 to bariery.Count-1 do
  if ((bariery[i].typ <> _JCB_STAVENI) and ((Self.CriticalBariera(bariery[i].typ)) or (not Self.WarningBariera(bariery[i].typ)))) then
   begin
    critical := true;
    break;
   end;

 // behem potvrzovani se mohly vyskytnout
 if (critical) then
  begin
   Self.CancelStaveni('Nelze postavit - kritick� bari�ry');
   ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nelze postavit '+Self.nazev+' - kritick� bari�ry', (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
   bariery.Free();
   Exit();
  end;

 // existuji bariery na potvrzeni potvrzovaci sekvenci ?
 podm := TList<TPSPodminka>.Create;
 for i := 0 to bariery.Count-1 do
  begin
   if (Self.PotvrSekvBariera(bariery[i].typ)) then
     podm.Add(GetPSPodminka(bariery[i].blok, TJC.PotvrSekvBarieraToReason(bariery[i].typ)));
  end;//for i

 if (podm.Count > 0) then
  begin
   // ano, takoveto bariery existuji -> potvrzovaci sekvence
   writelog('JC '+Self.Nazev+' : bari�ry s potvrzovac� sekvenc�, ��d�m potvrzen�...', WR_VC);
   Blky.GetBlkByID(Self.fproperties.NavestidloBlok, nav);
   Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], usek);
   ORTCPServer.Potvr(Self.fstaveni.SenderPnl, Self.PS_vylCallback, (Self.fstaveni.SenderOR as TOR), 'J�zdn� cesta s potvrzen�m', TBlky.GetBlksList(nav, usek), podm);
   Self.Krok := 6;
  end else begin
   // ne, takoveto bariery neexistuji -> stavim jizdni cestu
   if (Self.fstaveni.nc) then
     Self.Krok := 100
   else
     Self.Krok := 10;
  end;
end;//proceudre

procedure TJC.UPO_EscCallback(Sender:TObject);
begin
 if (Self.Krok = 5) then
  begin
   Self.CancelStaveni();
   Self.Krok := 0;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// jakmile je zavolano StavJC(), tato funkce se stara o to, aby staveni doslo az do konce
// kontroluje prubezne podminky apod.
procedure TJC.UpdateStaveni();
var i,j:Integer;
    aZaver:TJCType;
    Navestidlo, Blk, Blk2, Trat:TBlk;
    uzavren,uzavren_glob:boolean;
    ev:TChangeEvent;
 begin
  if (not Self.Staveni) then Exit;

  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Navestidlo);

  //////////////////////////////////////////////////////////////////////////////
  // staveni vlakovych a posunovych cest:

  case (Self.Krok) of
   10:begin
      // nejprve priradime uvolneni zaveru posledniho bloku uvolneni zaveru perdposledniho bloku
      if (Self.fproperties.Useky.Count > 1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-2], Blk);
        (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Useky[Self.fproperties.Useky.Count-1]);
       end;

      writelog('Krok 10: useky: nastavuji staveci zavery', WR_VC);
      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
        (Blk as TBlkUsek).Zaver := TJCType.staveni;
       end;//for cyklus

      writelog('Krok 10 : vyhybky: zamykam do pozadovanych poloh', WR_VC);
      for i := 0 to Self.fproperties.Vyhybky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
        (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.Vyhybky[i].Poloha), true, false, nil, Self.VyhNeprestavenaJCPC);
       end;
      for i := 0 to Self.fproperties.Odvraty.Count-1 do
       begin
        // pridani zruseni redukce
        Blky.GetBlkByID(Self.fproperties.Odvraty[i].ref_blk, Blk);
        case (Blk.GetGlobalSettings().typ) of
          _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Odvraty[i].Blok);
        end;

        // nastaveni odvratu
        Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
        (Blk as TBlkVyhybka).SetPoloha(TVyhPoloha(Self.fproperties.Odvraty[i].Poloha), true);
       end;

      writelog('Krok 10 : zamky: nastavuji zavery', WR_VC);
      for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].ref_blk, Blk);
        case (Blk.GetGlobalSettings().typ) of
          _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.podminky.zamky[i].Blok);
        end;

        // nastaveni zaveru zamku
        Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, Blk);
        (Blk as TBlkZamek).Zaver := true;
       end;

      Self.Krok := 11;
      writelog('Krok 11 : vyhybky: poloha: detekce',WR_VC);
     end;//case 0


   11:begin
      for i := 0 to Self.fproperties.Vyhybky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
        if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha) then
          Exit;
       end;//for cyklus
      for i := 0 to Self.fproperties.Odvraty.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
        if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Odvraty[i].Poloha) then
          Exit;
       end;//for cyklus

      writelog('Krok 11 : vyhybky: poloha: OK',WR_VC);

      writelog('Krok 11: useky: nastavuji nouzovy zaver', WR_VC);
      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
        (Blk as TBlkUsek).Zaver := TJCType.nouz;
       end;//for cyklus

      Self.Krok := 12;
     end;//case 1


   12:begin
       writelog('Krok 12 : nastavuji redukci menu prislusenstvi',WR_VC);

       for i := 0 to Self.fproperties.Prisl.Count-1 do
        begin
         Blky.GetBlkByID(Self.fproperties.Prisl[i].Blok, Blk);

         case (Blk.GetGlobalSettings().typ) of
           _BLK_SCOM:begin
             //scom
             writelog('Krok 12 : scom '+Blk.GetGlobalSettings().name+' - redukuji menu', WR_VC);
             Blky.GetBlkByID(Self.fproperties.Prisl[i].ref_blk, Blk);
             case (Blk.GetGlobalSettings().typ) of
               _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Prisl[i].Blok);
             end;
           end;// _BLK_SCOM
         end;//case
        end;//for i

       // prejezdy
       uzavren_glob := false;
       for i := 0 to Self.fproperties.Prejezdy.Count-1 do
        begin
         Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);

         uzavren := false;

         // prejezd uzavirame jen v pripade, ze nejaky z jeho aktivacnich bloku je obsazen
         // v pripade posunove cesty uzavirame vzdy

         if (Self.fproperties.TypCesty = TJCType.posun) then
          begin
           // posunova cesta:
           writelog('Krok 12 : prejezd '+Blk.GetGlobalSettings().name+' - uzaviram', WR_VC);

           // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
           Blky.GetBlkByID(Self.fproperties.Prejezdy[i].oteviraci, Blk);
           case (Blk.GetGlobalSettings().typ) of
             _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Prejezdy[i].Prejezd);
           end;

           uzavren := true;
           uzavren_glob := true;
          end else begin

           // vlakova cesta:
           for j := 0 to Length(Self.fproperties.Prejezdy[i].uzaviraci)-1 do
            begin
             Blky.GetBlkByID(Self.fproperties.Prejezdy[i].uzaviraci[j], Blk2);
             if ((Blk2 as TBlkUsek).Obsazeno = TusekStav.obsazeno) then
              begin
               writelog('Krok 12 : prejezd '+Blk.GetGlobalSettings().name+' - aktivacni usek '+Blk2.GetGlobalSettings().name+' obsazen - uzaviram', WR_VC);

               // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
               Blky.GetBlkByID(Self.fproperties.Prejezdy[i].oteviraci, Blk);
               case (Blk.GetGlobalSettings().typ) of
                 _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Prejezdy[i].Prejezd);
               end;

               uzavren := true;
               uzavren_glob := true;
               break;   // overit, ze breakuje jen vnitrni cyklus
              end;
            end;//for j
          end;// else posunova cesta

         if (not uzavren) then
          begin
           ev.func := Self.UsekClosePrj;
           ev.data := i;

           // prejezd neuzaviram -> pridam pozadavek na zavreni pri obsazeni do vsech aktivacnich useku
           for j := 0 to Length(Self.fproperties.Prejezdy[i].uzaviraci)-1 do
            begin
             Blky.GetBlkByID(Self.fproperties.Prejezdy[i].uzaviraci[j], Blk2);
             (Blk2 as TBlkUsek).AddChangeEvent((Blk2 as TBlkUsek).EventsOnObsaz, ev);
            end;

           writelog('Krok 12 : prejezd '+Blk.GetGlobalSettings().name+' - zadny aktivacni usek neobsazen - nechavam otevreny', WR_VC);
          end;
        end;//for i

      if (uzavren_glob) then
       Self.Krok := 13
      else
       Self.Krok := 14;

     end;


   13:begin
       // kontrola stavu prejezdu
       for i := 0 to Self.fproperties.Prejezdy.Count-1 do
        begin
         Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);

         if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then Exit();
         writelog('Krok 13 : prejezd '+Blk.GetGlobalSettings().name+' uzavren', WR_VC);
        end;//for i

      Self.Krok := 14;
     end;


   14:begin
      writelog('Krok 14 : useky: nastavit validni zaver', WR_VC);

      aZaver := Self.fproperties.TypCesty;

      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
        (Blk as TBlkUsek).Zaver := aZaver;

        // kontrola pritomnosti soupravy na usecich - toto je potreba delat pro dodatecne navesti
        if ((Self.fproperties.TypCesty = TJCType.vlak) and ((Blk as TBlkUsek).Souprava > -1)) then
         begin
          if (Blky.GetBlkWithSpr((Blk as TBlkUsek).Souprava).Count = 1) then
           Soupravy.RemoveSpr((Blk as TBlkUsek).Souprava)
          else
           (Blk as TBlkUsek).Souprava := -1;
         end;
       end;//for cyklus

      Self.RusZacatekJC();
      Self.RusVBJC();
      Self.RusKonecJC();

      // nastavit front blok soupravy
      Blky.GetBlkByID((Navestidlo as TBlkSCom).UsekID, Blk);
      if ((Blk as TBlkUsek).Souprava > -1) then
       Soupravy.soupravy[(Blk as TBlkUsek).Souprava].front := (Blk as TBlkUsek);
      (Blk as TBlkUsek).SComJCRef := Navestidlo;

      Self.Krok := 0;
      (Navestidlo as TBlkSCom).DNjc := Self;

      // jeste jednou zkontrolujeme, jeslti jsou vyybky ve spravnych polohach, protoze se teoreticky behem staveni mohly nejak podhodit...

      for i := 0 to Self.fproperties.Vyhybky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
        if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha) then
         begin
          ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nepoloha '+Blk.GetGlobalSettings().name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
          writelog('Krok 14 : Vyhybka '+Blk.GetGlobalSettings().name+' nema spravnou polohu !', WR_VC);
          Exit;
         end;
       end;//for cyklus

      for i := 0 to Self.fproperties.Odvraty.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
        if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Odvraty[i].Poloha) then
         begin
          ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nepoloha '+Blk.GetGlobalSettings().name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
          writelog('Krok 14 : Vyhybka '+Blk.GetGlobalSettings().name+' nema spravnou polohu !', WR_VC);
          Exit;
         end;
       end;//for cyklus

      // trat
      // zruseni redukce posledniho bloku jizdni cesty je navazano na zruseni zaveru trati
      // -> jakmile dojde ke zruseni zaveru posledniho bloku, dojde ke zruseni zaveru trati
      if (Self.fproperties.Trat > -1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);

        // tahleta situace opravdu muze nastat:
        if ((((Blk as TBlkTrat).Smer <> Self.fproperties.TratSmer) and (((Blk as TBlkTrat).GetSettings().zabzar = TTratZZ.souhlas) or ((Blk as TBlkTrat).GetSettings().zabzar = TTratZZ.nabidka) or (((Blk as TBlkTrat).GetSettings().zabzar = TTratZZ.bezsouhas) and ((Blk as TBlkTrat).nouzZaver))))
          or ((Blk as TBlkTrat).Obsazeno) or ((Blk as TBlkTrat).ZAK)) then
         begin
          ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Chyba trati '+Blk.GetGlobalSettings().name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
          writelog('Krok 14 : Trat '+Blk.GetGlobalSettings().name+' nesplnuje podminky pro postaveni JC !', WR_VC);
          Exit;
         end;

        (Blk as TBlkTrat).Zaver  := true;
        (Blk as TBlkTrat).Smer   := Self.fproperties.TratSmer;

        // zruseni zaveru posledniho bloku JC priradime zruseni zaveru trati
        Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
        (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Trat);
       end;

      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);

      if ((Navestidlo as TBlkSCom).ZAM) then
       begin
        writelog('Krok 14 : navestidlo: zamkle na STUJ',WR_VC);
       end else begin
        Self.NastavSCom();
        writelog('Krok 14 : navestidlo: nastaveno na '+TBlkScom.NavestToString((Navestidlo as TBlkSCom).Navest), WR_VC);
       end;

      if ((Navestidlo as TBlkSCom).ZAM) then Self.RozpadBlok := -2 else Self.RozpadBlok := -1;
      Self.RozpadRuseniBlok := -2;

      if (Self.data.TypCesty = TJCType.vlak) then Blky.SprPrediction(Navestidlo);

      // pokud je cesta ze zasobniku, smazeme ji odtam
      if (Self.fstaveni.from_stack <> nil) then
       begin
        (Self.fstaveni.from_stack as TORStack).RemoveJC(Self);
        Self.fstaveni.from_stack := nil;
       end;

      writelog('Postavena JC '+Self.Nazev, WR_VC);
     end;//case 14


     ///////////////////////////////////////////////////////////////////////////
     // staveni nouzovych cest:

     100:begin
      // vsem usekum nastavime staveci zaver:
      writelog('Krok 100: useky: nastavuji staveci zavery', WR_VC);
      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
        (Blk as TBlkUsek).Zaver := TJCType.staveni;
       end;//for cyklus

      // nastavit nouzovy zaver uvazky
      if (Self.fproperties.Trat > -1) then
       begin
        writelog('Krok 100: trat: nastavuji nouzovy zaver uvazky', WR_VC);
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);

        // najdeme si uvazku, ktera je v OR navestidla a te nastavime nouzovy zaver
        if (((Blk as TBlkTrat).uvazkaA as TBlkUvazka).OblsRizeni.cnt > 0) then
         begin
          for i := 0 to (Navestidlo as TBlkSCom).OblsRizeni.Cnt-1 do
            if (((Blk as TBlkTrat).uvazkaA as TBlkUvazka).OblsRizeni.ORs[0] = (Navestidlo as TBlkSCom).OblsRizeni.ORs[i]) then
               ((Blk as TBlkTrat).uvazkaA as TBlkUvazka).nouzZaver := true;

          for i := 0 to (Navestidlo as TBlkSCom).OblsRizeni.Cnt-1 do
            if (((Blk as TBlkTrat).uvazkaB as TBlkUvazka).OblsRizeni.ORs[0] = (Navestidlo as TBlkSCom).OblsRizeni.ORs[i]) then
               ((Blk as TBlkTrat).uvazkaB as TBlkUvazka).nouzZaver := true;
         end;
       end;

      // nastavit vyhybky do pozadovanych poloh:
      writelog('Krok 100: vyhybky: nastavuji do pozadovanych poloh', WR_VC);
      for i := 0 to Self.fproperties.Vyhybky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
        (Blk as TBlkVyhybka).SetPoloha(Self.fproperties.Vyhybky[i].Poloha, true, false, Self.VyhPrestavenaNC);
       end;
      for i := 0 to Self.fproperties.Odvraty.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
        (Blk as TBlkVyhybka).SetPoloha(Self.fproperties.Odvraty[i].Poloha, true, false, Self.VyhPrestavenaNC);
       end;

      writelog('Krok 100: prejezdy: uzaviram', WR_VC);
      for i := 0 to Self.fproperties.Prejezdy.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);
        (Blk as TBlkPrejezd).UZ := true;
       end;

      // nastavit nouzovy zaver zamkum
      for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, Blk);
        (Blk as TBlkZamek).nouzZaver := true;
       end;

      Self.fstaveni.ncBarieryCntLast := -1;   // tady je potreba mit cislo < 0

      Self.Krok := 101;
     end;//case 100

     101:begin
      // prubezne kontroluji podminky a zobrazuji potvrzovaci sekvenci

      // zjistime aktualni bariery:
      Self.fstaveni.ncBariery.Clear();
      Self.PodminkyNCStaveni(Self.fstaveni.ncBariery);

      // kontrolujeme rozdilnost seznamu:
      if (Self.fstaveni.ncBariery.Count <> Self.fstaveni.ncBarieryCntLast) then
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
        writelog('Krok 101: zmena potvr., odesilam aktualni seznam', WR_VC);
        ORTCPServer.Potvr(Self.fstaveni.SenderPnl, Self.NC_PS_Callback, Self.fstaveni.SenderOR as TOR, 'Zapnut� p�ivol�vac� n�v�sti', TBlky.GetBlksList(Navestidlo, Blk), Self.BarieryNCToPotvr(Self.fstaveni.ncBariery));
       end;
      Self.fstaveni.ncBarieryCntLast := Self.fstaveni.ncBariery.Count;

      // nastavovani smeru trati:
      if (Self.fproperties.Trat > -1) then
       begin
        Blky.GetBlkByID(Self.fproperties.Trat, Blk);
        if (((Blk as TBlkTrat).GetSettings.zabzar = TTratZZ.bezsouhas) and (not (blk as TBlkTrat).ZAK) and
          (not (blk as TBlkTrat).Zaver) and (not (blk as TBlkTrat).Obsazeno) and (not (blk as TBlkTrat).Zadost) and
          ((Blk as TBlkTrat).Smer <> Self.fproperties.TratSmer)) then
           begin
            writelog('Krok 101: trat: nastaven smer', WR_VC);
            (Blk as TBlkTrat).Smer := Self.fproperties.TratSmer;
           end;
       end;
     end;

     102:begin
      // potrvzovaci sekvence potvrzena -> stavim navestidlo, ...

      writelog('Krok 102: useky: rusim zavery', WR_VC);
      for i := 0 to Self.fproperties.Useky.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
        (Blk as TBlkUsek).Zaver := TJCType.no;
       end;//for cyklus

      Self.RusZacatekJC();
      Self.RusVBJC();
      Self.RusKonecJC();

      Self.Krok := 0;
      (Navestidlo as TBlkSCom).privol := Self;

      // i pokud je navetidlo ve STUJ, nastavuji navest (to je spravne chovani podle JOP)
      Self.NastavSCom();
      writelog('Krok 102 : navestidlo: nastaveno na '+TBlkScom.NavestToString((Navestidlo as TBlkSCom).Navest), WR_VC);

      // pokud je cesta ze zasobniku, smazeme ji odtam
      if (Self.fstaveni.from_stack <> nil) then
       begin
        (Self.fstaveni.from_stack as TORStack).RemoveJC(Self);
        Self.fstaveni.from_stack := nil;
       end;

      // presun soupravy z useku pred navestidlem do posledniho useku JC
      Blky.GetBlkByID((Navestidlo as TBlkSCom).UsekID, Blk);
      Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk2);
      if (((Blk as TBlkUsek).Souprava > -1) and ((Blk2 as TBlkUsek).Souprava = -1) and ((Blk2 as TBlkUsek).InTrat = -1) and ((Blk2 as TBlkUsek).Stav.stanicni_kolej)) then
       begin
        if ((Blk as TBlkUsek).InTrat > -1) then
         begin
          Blky.GetBlkByID((Blk as TBlkUsek).InTrat, trat);
          (Trat as TBlkTrat).RemoveSpr((Blk as TBlkUsek).Souprava);
         end;

        (Blk2 as TBlkUsek).Souprava := (Blk as TBlkUsek).Souprava;
        (Blk as TBlkUsek).Souprava := -1;
       end;

      writelog('Postavena NC '+Self.Nazev, WR_VC);
     end;//case 102

   end;//case
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

// je volana, pokud behem staveni dojde k vyjimce
// napriklad pri kontrole obsazenosti useku v JC apod.
procedure TJC.CancelStaveni(reason: string = ''; stack_remove:boolean = false);
begin
 if (reason <> '') then
  begin
   ORTCPServer.SendInfoMsg(Self.fstaveni.SenderPnl, reason);
   writelog('Nelze postavit JC '+Self.Nazev+' - '+reason, WR_VC);
  end;

 case (Self.Krok) of
    101:begin
      ORTCPServer.PotvrClose(Self.fstaveni.SenderPnl, reason);
    end   
 end;//case Self.Krok

 Self.Krok := 0;
 Self.fstaveni.nc := false;
 Self.RusZacatekJC();
 Self.RusVBJC();
 Self.RusKonecJC();
 ORTCPServer.CancelUPO(Self.fstaveni.SenderPnl, Self);
 if (Self.fstaveni.from_stack <> nil) then
    if (stack_remove) then (Self.fstaveni.from_stack as TORStack).RemoveJC(Self)
  else
   (Self.fstaveni.SenderOR as TOR).BroadcastData('ZAS;FIRST;1');


 Self.fstaveni.from_stack := nil;
end;

////////////////////////////////////////////////////////////////////////////////

//rusi zacatek jizdni cesty
procedure TJC.RusZacatekJC();
var Blk:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Blk);
  if (Blk = nil) then Exit;
  if (Blk.GetGlobalSettings().typ <> _BLK_SCOM) then Exit;
  if ((Blk as TBlkSCom).ZacatekVolba = TBlkSComVolba.none) then Exit;

  (Blk as TBlkSCom).ZacatekVolba := TBlkSComVolba.none;
  if ((Blk as TBlkSCom).DNjc = Self) then
    (Blk as TBlkSCom).DNjc := nil;

  writelog('Zrusen zacatek staveni VC na bloku '+Blk.GetGlobalSettings().name,WR_VC);
 end;//procedure

//rusi konec jizdni cesty
procedure TJC.RusKonecJC();
var Blk:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
  if (Blk = nil) then Exit;
  (Blk as TBlkUsek).KonecJC := no;
 end;//procedure

procedure TJC.RusVBJC();
var Blk:TBlk;
    i:Integer;
begin
 for i := 0 to Self.data.vb.Count-1 do
  begin
   Blky.GetBlkByID(Self.data.vb[i], Blk);
   if ((Blk <> nil) and (Blk.GetGlobalSettings().typ = _BLK_USEK)) then
     (Blk as TBLkUsek).KonecJC := TJCType.no;
  end; 
end;

////////////////////////////////////////////////////////////////////////////////

//ruseni jizdni cesty
procedure TJC.RusJC();
var cyklus:Integer;
    Blk, Nav:TBlk;
 begin
  Self.RusJCWithoutBlk();

  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
  (Nav as TBlkSCom).DNjc := nil;

  for cyklus := 0 to Self.fproperties.Useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[cyklus], Blk);
    (Blk as TBlkUsek).Zaver := no;
   end;

  // zaver trati se rusi automaticky uvolnenim zaveru posledniho bloku pred trati

  writelog('Zrusena JC '+Self.Nazev, WR_VC);
 end;//procedure

//ruseni jizdni cesty bez ruseni zaveru bloku
procedure TJC.RusJCWithoutBlk();
var Nav:TBlk;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
  writelog('Probiha ruseni navesti JC '+Self.Nazev, WR_VC);

  if ((Nav as TBlkSCom).DNjc = self) then
   begin
    (Nav as TBlkSCom).AB := false;
    if ((Nav as TBlkSCom).Navest > 0) then
      (Nav as TBlkSCom).Navest := 0;
   end;

  Self.Krok             := 0;
  Self.RozpadBlok       := -5;
  Self.RozpadRuseniBlok := -5;
  JCDb.CheckNNavaznost(Self);
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

//RozpadBlok = blok index, kam by mela souprava vjet
//RozpadRuseniBlok = blok index, kde je posledni detekovany vagon soupravy
procedure TJC.UsekyRusJC();
var Nav,Blk,Usek:TBlk;
    i:Integer;
begin
// if (Self.RozpadBlok <= -5) then Exit();   overeno pri volani

 Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);

 // kontrola obsazenosti useku pred navestidlem
 Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Usek);
 if ((Self.RozpadBlok = -1) and (((Usek as TBlkUsek).Obsazeno = TUsekStav.obsazeno) or ((Usek as TBlkUsek).GetSettings.MTBAddrs.Count = 0))) then
  begin
   Self.RozpadBlok       := 0;
   Self.RozpadRuseniBlok := -1;
  end;

 // uvolneni prvniho useku prd navestidlem v posunove ceste je signalem pro zhasnuti navestidla
 if (((Usek as TBlkUsek).GetSettings().MTBAddrs.Count > 0) and ((Usek as TBlkUsek).Obsazeno = TUsekStav.uvolneno) and ((Nav as TBlkSCom).Navest <> 0) and
    (Self.RozpadRuseniBlok = 0) and (Self.data.TypCesty = TJCType.posun)) then
  begin
   writelog('JC '+Self.Nazev+': Uvolnen usek '+Usek.GetGlobalSettings().name+' : navestidlo '+Nav.GetGlobalSettings().name+' nastaveno na STUJ',WR_VC);
   (Nav as TBlkSCom).JCZrusNavest();
  end;


 for i := Self.RozpadBlok to Self.fproperties.Useky.Count-1 do
  begin
   if (i < 0) then continue;    // i = -1 kdyz se kontroluje blok pred navestidlem, -2 pokud je navestidlo na STUJ, nebo zamkle

   Blky.GetBlkByID(Self.fproperties.Useky[i], Usek);

   // druha cast podminky je tu pro pripad, kdy by byl na konci posunove cesty obsazeny usek
   if (((Usek as TBlkUsek).Obsazeno = obsazeno) and ((i < Self.fproperties.Useky.Count-1) or (Self.RozpadBlok > Self.fproperties.Useky.Count-2) or (Self.fproperties.TypCesty <> TJCType.posun))) then
    begin
     if (i = Self.RozpadBlok) then
      begin
       //pokud se tento usek rovna RozpadBloku
       (Usek as TBlkUsek).Zaver := TJCType.nouz;

       if (Self.fproperties.TypCesty = TJCType.vlak) then
        begin
         //posuneme soupravu o blok dal
         Self.PredejDataDalsimuBloku();
        end;//if (Self.TypCesty = 0)

       // obsazeni prvniho useku
       // pozor: toto musi byt na tomto miste kvuli nastavovani Souprava.front
       if ((i = 0) and ((Nav as TBlkSCom).Navest <> 0) and (Self.RozpadBlok = 0)) then
        begin
         Self.RozpadRuseniBlok := 0;
         // navestidlo pri obsazeni prvniho useku rusime v pripade, ze se jedna o VC
         if (Self.data.TypCesty = TJCType.vlak) then
          begin
           writelog('JC '+Self.Nazev+': Obsazen usek '+Usek.GetGlobalSettings().name+' : navestidlo '+Nav.GetGlobalSettings().name+' nastaveno na STUJ',WR_VC);
           (Nav as TBlkSCom).JCZrusNavest();
          end;
        end;

       Self.RozpadBlok := Self.RozpadBlok + 1;

       // pokud jsme v predposlednim useku a posledni je nedetekovany, posuneme RozpadBlok jeste o jeden usek, aby se cesta mohla zrusit
       if (i = Self.fproperties.Useky.Count-2) then
        begin
         Blky.GetBlkByID(Self.fproperties.Useky[Self.fproperties.Useky.Count-1], Blk);
         if ((Blk as TBLkUsek).GetSettings().MTBAddrs.Count = 0) then
           Self.RozpadBlok := Self.RozpadBlok + 1;
        end;

       if ((i = Self.fproperties.Useky.Count-1) and (Self.fproperties.Trat > -1)) then
        begin
         // posledni usek JC obsazen -> trat
         Blky.GetBlkByID(Self.fproperties.Trat, Blk);

         (Blk as TBlkTrat).BP := true;
         if ((Usek as TBlkUsek).Souprava > -1) then
           (Blk as TBlkTrat).AddSpr((Usek as TBlkUsek).Souprava);
         (Blk as TBlkTrat).Zaver := false;
        end;


      end else begin//if Self.RozpadBlok = 0
       if (Integer((Usek as TBlkUsek).Zaver) > 0) then
        begin
         //pokud jsme na jinem useku, nez RozpadBlok
         if (((Nav as TBlkSCom).Navest > 0) and ((Nav as TBlkSCom).DNjc = Self)) then
          begin
           ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Chyba povolovaci navesti '+Nav.GetGlobalSettings().name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
           Self.RusJCWithoutBlk();
          end;

         // v trati zaver nerusime, nesmime tam dat ani nouzovy, ani zadny zaver
         if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.Trat = -1)) then
           (Usek as TBlkUsek).Zaver := nouz;
        end;
      end;
    end;


   // kontrola zruseni jizdni cesty vlivem vynuzovani bloku
   if ((i = Self.RozpadBlok) and (((Usek as TBlkUsek).Zaver = TJCType.no))) then
    begin
     // pokud usek, na ktery se chystam vkrocit, nema zaver, je neco divne -> zrusit JC (predevsim kvuli predavani loko, ktere by mohlo narusit dalsi JC)
     Self.RusJCWithoutBlk();
     Exit();
    end;

  end;//for i

  // jizdni cesta konci uvolnenim predposledniho useku

  // mensitko je dulezite a ma smysl !
  //  kdyby tam bylo <=, mohl by se rozpadnout jediny usek, na kterem je souprava tim, ze se odobsadi
  if ((Self.RozpadRuseniBlok >= 0) and (Self.RozpadRuseniBlok < Self.RozpadBlok-1)) then
   begin
    //ziskani dotazovaneho useku
    Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadRuseniBlok], Usek);

    if (((Usek as TBlkUsek).Zaver = TJCType.nouz) and ((Usek as TBlkUsek).Obsazeno = uvolneno)) then
     begin
      // cesta se rozpada...

      // uvolneni prvniho useku v posunove ceste je signalem pro zhasnuti navestidla
      if (((Nav as TBlkSCom).Navest <> 0) and (Self.RozpadRuseniBlok = 0) and (Self.data.TypCesty = TJCType.posun)) then
       begin
        writelog('JC '+Self.Nazev+': Uvolnen usek '+Usek.GetGlobalSettings().name+' : navestidlo '+Nav.GetGlobalSettings().name+' nastaveno na STUJ',WR_VC);
        (Nav as TBlkSCom).JCZrusNavest();
       end;

      (Usek as TBlkUsek).Zaver := no;
      Self.RozpadRuseniBlok := Self.RozpadRuseniBlok + 1;

      if (Self.fproperties.TypCesty = TJCType.vlak) then
       begin
        if ((Usek as TBlkUsek).Souprava > -1) then
         begin
          writelog('JC '+Self.nazev+': smazana souprava '+Soupravy.GetSprNameByIndex((Usek as TBlkUsek).Souprava)+' z bloku '+Usek.GetGlobalSettings().name, WR_SPRPREDAT, 0);
          (Usek as TBlkUsek).Souprava := -1;
         end;

        // tady se resi pripad, kdy stanicni kolej zustane obsazena (protoze tams toji vagony), ale souprava se z ni musi odstanit uvolnenim prvniho bloku JC
        Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadRuseniBlok], Blk);
        Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Usek);
        if ((Self.RozpadRuseniBlok = 1) and ((Usek as TBlkUsek).Souprava > -1) and ((Usek as TBlkUsek).Souprava = (Blk as TBlkUsek).Souprava)) then
         begin
          writelog('JC '+Self.nazev+': smazana souprava '+Soupravy.GetSprNameByIndex((Usek as TBlkUsek).Souprava)+' z bloku '+Usek.GetGlobalSettings().name, WR_SPRPREDAT, 0);
          (Usek as TBlkUsek).Souprava := -1;
         end;
       end;
     end;//if Self.RozpadBlok >= 1
   end;//if (cyklus2 = Self.RozpadRuseniBlok)

  // mazani soupravy z useku pred navestidlem
  if ((Self.RozpadRuseniBlok = 0) and (Self.fproperties.TypCesty = TJCType.vlak)) then
   begin
    Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Usek);
    if (((Usek as TBlkUsek).Obsazeno = TUsekStav.uvolneno) and ((Usek as TBlkUsek).Souprava > -1)) then
     begin
      (Usek as TBlkUsek).Souprava := -1;
      writelog('JC '+Self.nazev+': smazana souprava '+Soupravy.GetSprNameByIndex((Usek as TBlkUsek).Souprava)+' z bloku '+Usek.GetGlobalSettings().name, WR_SPRPREDAT, 0);

      // pokud je blok v trati, smazeme z soupravu z trati a zrusime zaver bloku
      if ((Usek as TBlkUsek).InTrat > -1) then
       begin
        Blky.GetBlkByID((Usek as TBlkUsek).InTrat, Blk);
        (Blk as TBlkTrat).Change();
       end;
     end;
   end;
  

  // takhleta silenost za AND je tu pro pripad, kdy JC ma jen jeden usek (to se stava napriklad na smyckach)
  if ((Self.RozpadRuseniBlok = Self.fproperties.Useky.Count-1) and ((Self.fproperties.Useky.Count > 1) or (Self.RozpadBlok = 1))) then
   begin
    // vsechny useky az na posledni jsou uvolneny -> rusime JC

    // tady by teoreticky melo prijit ruseni zaveru posledniho bloku, ale to neni poteba, protoze zaver tohoto bloku je primo navazny na zaver predposledniho bloku pres redukce
    // to je napriklad kvuli tratim, ci z toho duvodu, ze na stanicnich kolejich nejde dat NUZ

    // pozor ale na JC, ktere maji jen jeden usek a ten je stanicni koleji:
    if (Self.fproperties.Useky.Count = 1) then
     begin
      Blky.GetBlkByID(Self.fproperties.Useky[0], Usek);
      (Usek as TBlkUsek).Zaver := no;

      // pokud ma cesta jen jeden usek, odstranime soupravu z useku pred navestidlem:
      if (Self.fproperties.TypCesty = TJCType.vlak) then
       begin
        Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
        Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Usek);
        writelog('JC '+Self.nazev+': smazana souprava '+Soupravy.GetSprNameByIndex((Usek as TBlkUsek).Souprava)+' z bloku '+Usek.GetGlobalSettings().name, WR_SPRPREDAT, 0);
        (Usek as TBlkUsek).Souprava := -1;
       end;
     end;

    Self.RozpadBlok       := -5;
    Self.RozpadRuseniBlok := -5;
    writelog('JC '+Self.nazev+' - ruseni: rozpad cesty vlakem', WR_VC);
    if ((Nav as TBlkSCom).DNjc = Self) then
     begin
      if ((Nav as TBlkSCom).Navest > 0) then      // tato situace opravdu muze nastat - predstavte si posunovou cestu s jednim usekem vychazejici z nedetek koleje
        (Nav as TBlkSCom).JCZrusNavest();
      (Nav as TBlkSCom).AB := false;        // zatim vzdy zrusime AB
      (Nav as TBlkSCom).DNjc := nil;
     end;
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//preda soupravu v jizdni ceste dalsimu bloku v poradi
procedure TJC.PredejDataDalsimuBloku();
var UsekActual,UsekDalsi,Nav:TBlk;
 begin
  if (Self.RozpadBlok = 0) then
   begin
    Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);
    Blky.GetBlkByID((Nav as TBlkSCom).UsekID, UsekActual);
    if ((UsekActual as TBlkUsek).Souprava > -1) then
      if (Soupravy.soupravy[(UsekActual as TBlkUsek).Souprava].front <> UsekActual) then
         Exit();
   end else begin
    Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadBlok-1], UsekActual);
   end;
  Blky.GetBlkByID(Self.fproperties.Useky[Self.RozpadBlok], UsekDalsi);
  if ((UsekActual as TBlkUsek).Souprava = -1) then Exit;

  (UsekDalsi as TBlkUsek).zpomalovani_ready := true;
  (UsekDalsi as TBlkUsek).Souprava := (UsekActual as TBlkUsek).Souprava;
  Soupravy.soupravy[(UsekDalsi as TBlkUsek).Souprava].front := UsekDalsi;
  writelog('JC '+Self.nazev+': predana souprava '+Soupravy.GetSprNameByIndex((UsekDalsi as TBlkUsek).Souprava)+' z bloku '+UsekActual.GetGlobalSettings().name+' do bloku '+UsekDalsi.GetGlobalSettings().name,WR_SPRPREDAT, 0);

  Self.CheckSmyckaBlok(UsekDalsi);
 end;//procedure

procedure TJC.CheckSmyckaBlok(blk:TBlk);
begin
 if (((Blk as TBlkUsek).GetSettings().SmcUsek) and ((Blk as TBlkUsek).Souprava > -1)) then
  begin
   Soupravy.soupravy[(Blk as TBlkUsek).Souprava].ChangeSmer();
   writelog('Obsazen smyckovy usek '+Blk.GetGlobalSettings.name+ ' - menim smer loko v souprave '+Soupravy.soupravy[(Blk as TBlkUsek).Souprava].nazev, WR_SPRPREDAT);
  end;//if
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//nastavi navestidlo JC na pozadovanou navest
procedure TJC.NastavSCom();
var Nav,DalsiNav:TBlk;
    Navest:Integer;
 begin
  Blky.GetBlkByID(Self.fproperties.NavestidloBlok, Nav);

  Navest := 0;

  if (Self.fstaveni.nc) then
   begin
    // nouzova cesta
    Navest := 8;
   end else begin

    case (Self.fproperties.TypCesty) of
     TJCType.posun : begin
      // posunova cesta
      Navest := 9;          // dovolen zajisteny posun
     end;//case posun

     TJcType.vlak : begin
      Blky.GetBlkByID(Self.fproperties.DalsiNNavaznost, DalsiNav);
      if ((Self.fproperties.DalsiNNavaznostTyp = 1) or ((DalsiNav <> nil) and ((DalsiNav as TBlkSCom).Navest > 0))) then
       begin
        // na dalsim navestidle lze jet

        if (Self.fproperties.RychlostDalsiN = 4) then
         begin
          // 40 km/h
          Navest := 4;              // 40 km/h a volno
         end else begin
          // ostatni rychlosti
          Navest := 1;              // volno
         end;

       end else begin//if ...SCom.Cesta
        // na dalsim navestidle je na STUJ

        if (Self.fproperties.RychlostNoDalsiN = 4) then
         begin
          // 40 km/h
          Navest := 6;              // 40 km/h a vystraha
         end else begin
          // ostatni rychlosti
          Navest := 2;              // vystraha
         end;
       end;
     end;//case vlak

     end;//case
   end;// else nouzova cesta

  (Nav as TBlkSCom).Navest := Navest;
  JCDb.CheckNNavaznost(Self);
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJC.LoadData(ini:TMemIniFile; section:string):Byte;
var sl,sl2:TStrings;
    i,j, cnt:Integer;
    vyhZaver:TJCVyhZaver;
    odvrat:TJCOdvratZaver;
    prisl:TJCPrislZaver;
    prj:TJCPrjZaver;
    zam:TJCZamZaver;
begin
 Self.fproperties.Nazev               := ini.ReadString (section, 'Nazev', section);
 Self.fproperties.NavestidloBlok      := ini.ReadInteger(section, 'Nav', -1);
 Self.fproperties.TypCesty            := TJCType(ini.ReadInteger(section, 'Typ', -1));
 Self.fproperties.DalsiNNavaznost     := ini.ReadInteger(section, 'DalsiN', 0);
 Self.fproperties.DalsiNNavaznostTyp  := ini.ReadInteger(section, 'DalsiNTyp', 0);
 Self.fproperties.RychlostDalsiN      := ini.ReadInteger(section, 'RychDalsiN', 0);
 Self.fproperties.RychlostNoDalsiN    := ini.ReadInteger(section, 'RychNoDalsiN', 0);
 Self.fproperties.Trat                := ini.ReadInteger(section, 'Trat', -1);
 Self.fproperties.TratSmer            := TTratSmer(ini.ReadInteger(section, 'TratSmer', 0));

 // nacteni zaveru useku:
 sl  := TStringList.Create();
 sl2 := TStringList.Create();
 ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'useky', '')), sl);
 Self.fproperties.Useky.Count := sl.Count;
 for i := 0 to Self.fproperties.Useky.Count-1 do
  begin
   try
     Self.fproperties.Useky[i] := StrToInt(sl[i]);
   except
     Exit(1);
   end;
  end;//for i

 // nacteni zaveru vyhybek:
 sl.Clear();
 ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vyhybky', '')), sl);
 cnt := (sl.Count div 2);
 Self.fproperties.Vyhybky.Clear();
 for i := 0 to cnt-1 do
  begin
   try
     vyhZaver.Blok   := StrToInt(sl[i*2]);
     vyhZaver.Poloha := TVyhPoloha(StrToInt(sl[(i*2)+1]));
     Self.fproperties.Vyhybky.Add(vyhZaver);
   except
     Exit(2);
   end;
  end;//for i

 // nacteni odvratu:
 sl.Clear();
 ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'odvraty', '')), sl);
 cnt := (sl.Count div 3);
 Self.fproperties.Odvraty.Clear();
 for i := 0 to cnt-1 do
  begin
   try
     odvrat.Blok    := StrToInt(sl[i*2]);
     odvrat.Poloha  := TVyhPoloha(StrToInt(sl[(i*2)+1]));
     odvrat.ref_blk := StrToInt(sl[(i*2)+2]);
     Self.fproperties.Odvraty.Add(odvrat);
   except
     Exit(3);
   end;
  end;//for i

 // nacteni prislusenstvi
 sl.Clear();
 ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'prisl', '')), sl);
 cnt := (sl.Count div 2);
 Self.fproperties.Prisl.Clear();
 for i := 0 to cnt-1 do
  begin
   try
     prisl.Blok    := StrToInt(sl[i*2]);
     prisl.ref_blk := StrToInt(sl[(i*2)+1]);
     Self.fproperties.Prisl.Add(prisl);
   except
     Exit(4);
   end;
  end;//for i

 //format dat prejezdu:
 // (...),(...),(...) jsou jednotlive prejezdy
 // konkretni popis toho, co ma byt na miste tecek:
 //  (prj_blk_id,otevreni_blk,uzavreni_blk_1,uzavreni_blk_2,uzavreni_blk_3,..)

 // nacteni prejezdu
 sl.Clear();
 ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'prj', '')), sl);
 for i := 0 to sl.Count-1 do
  begin
   sl2.Clear();
   ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), sl2);

   try
     prj.Prejezd   := StrToInt(sl2[0]);
     prj.oteviraci := StrToInt(sl2[1]);

     SetLength(prj.uzaviraci, sl2.Count-2);
     for j := 2 to sl2.Count-1 do
       prj.uzaviraci[j-2] := StrToInt(sl2[j]);

     Self.fproperties.Prejezdy.Add(prj);
   except
     Exit(5);
   end;
  end;//for i

 // nacteni podminek poloh vyhybek:
 sl.Clear();
 ExtractStrings(['(', ')', ',' , ';'], [], PChar(ini.ReadString(section, 'podm-vyh', '')), sl);
 Self.fproperties.podminky.vyhybky.Clear();
 for i := 0 to (sl.Count div 2)-1 do
  begin
   try
     vyhZaver.Blok   := StrToInt(sl[i*2]);
     vyhZaver.Poloha := TVyhPoloha(StrToInt(sl[(i*2)+1]));
     Self.fproperties.podminky.vyhybky.Add(vyhZaver);
   except
     Exit(2);
   end;
  end;//for i

 // nacteni podminek zamku:
 sl.Clear();
 ExtractStrings(['(', ')'], [], PChar(ini.ReadString(section, 'podm-zamky', '')), sl);
 Self.fproperties.podminky.zamky.Clear();
 for i := 0 to sl.Count-1 do
  begin
   sl2.Clear();
   ExtractStrings([';', ',', '|', '-'], [], PChar(sl[i]), sl2);

   try
     zam.Blok    := StrToInt(sl2[0]);
     zam.ref_blk := StrToInt(sl2[1]);
     Self.fproperties.podminky.zamky.Add(zam);
   except
     Exit(2);
   end;
  end;//for i

 // nacteni variantnich bodu
 sl.Clear();
 ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
 for i := 0 to sl.Count-1 do
  begin
   try
     Self.fproperties.vb.Add(StrToInt(sl[i]));
   except

   end;
  end;//for i

 sl.Free();
 sl2.Free();

 Result := 0;
end;//procedure

procedure TJC.SaveData(ini:TMemIniFile; section:string);
var line:string;
    i,j:Integer;
begin
 ini.WriteString (section, 'Nazev', Self.fproperties.Nazev);
 ini.WriteInteger(section, 'Nav', Self.fproperties.NavestidloBlok);
 ini.WriteInteger(section, 'Typ', Integer(Self.fproperties.TypCesty));
 ini.WriteInteger(section, 'DalsiN', Self.fproperties.DalsiNNavaznost);
 ini.WriteInteger(section, 'DalsiNTyp', Self.fproperties.DalsiNNavaznostTyp);
 ini.WriteInteger(section, 'RychDalsiN', Self.fproperties.RychlostDalsiN);
 ini.WriteInteger(section, 'RychNoDalsiN', Self.fproperties.RychlostNoDalsiN);
 ini.WriteInteger(section, 'Trat', Self.fproperties.Trat);
 ini.WriteInteger(section, 'TratSmer', Integer(Self.fproperties.TratSmer));

 // useky
 line := '';
 for i := 0 to Self.fproperties.Useky.Count-1 do
   line := line + IntToStr(Self.fproperties.Useky[i]) + ',';
 ini.WriteString(section, 'useky', line);

 // vyhybky
 line := '';
 for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.Vyhybky[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.Vyhybky[i].Poloha)) + ')';
 ini.WriteString(section, 'vyhybky', line);

 // odvraty
 line := '';
 for i := 0 to Self.fproperties.Odvraty.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.Odvraty[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.Odvraty[i].Poloha)) + ',' + IntToStr(Self.fproperties.Odvraty[i].ref_blk)+ ')';
 ini.WriteString(section, 'odvraty', line);

 // prislusenstvi
 line := '';
 for i := 0 to Self.fproperties.Prisl.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.Prisl[i].Blok) + ',' + IntToStr(Self.fproperties.Prisl[i].ref_blk)+ ')';
 ini.WriteString(section, 'prisl', line);

 // prejezdy
 line := '';
 for i := 0 to Self.fproperties.Prejezdy.Count-1 do
  begin
   line := line + '(' + IntToStr(Self.fproperties.Prejezdy[i].Prejezd) + ',' + IntToStr(Self.fproperties.Prejezdy[i].oteviraci)+ ',';
   for j := 0 to Length(Self.fproperties.Prejezdy[i].uzaviraci)-1 do
    line := line + IntToStr(Self.fproperties.Prejezdy[i].uzaviraci[j]) + ',';
   line[Length(line)] := ')';
  end;
 ini.WriteString(section, 'prj', line);

 // podminky vyhybky
 line := '';
 for i := 0 to Self.fproperties.podminky.vyhybky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.podminky.vyhybky[i].Blok) + ',' + IntToStr(Integer(Self.fproperties.podminky.vyhybky[i].Poloha))+ ')';
 ini.WriteString(section, 'podm-vyh', line);

 // podminky zamky
 line := '';
 for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
   line := line + '(' + IntToStr(Self.fproperties.podminky.zamky[i].Blok) + ';' + IntToStr(Self.fproperties.podminky.zamky[i].ref_blk) + ')';
 ini.WriteString(section, 'podm-zamky', line);

 // variantni body
 line := '';
 for i := 0 to Self.fproperties.vb.Count-1 do
   line := line + IntToStr(Self.fproperties.vb[i]) + ';';
 ini.WriteString(section, 'vb', line);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// timeout staveni JC = 40 sekund
procedure TJC.UpdateTimeOut();
var i:Integer;
    Blk:TBlk;
begin
 // na zouzovou cestu se nevztahuje timeout
 if (not Self.Staveni) then Exit;

 if (Now > Self.fstaveni.TimeOut) then
  begin
   case (Self.Krok) of
    13:begin
      // prejezd(y) neuzavren
      for i := 0 to Self.fproperties.Prejezdy.Count-1 do
       begin
        Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);
        if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Neuzav�en '+(Blk as TBlkPrejezd).GetGlobalSettings().name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
       end;//for i
    end;//case 13

   else
     ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Timeout '+Self.nazev, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
   end;//else case

   //timeout
   Self.CancelStaveni('P�ekro�en� �asu stav�n� JC', true);    // toto je docasne reseni: cestu vymazeme ze zasobniku
  end;//if timeout
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJC.GetStaveni():boolean;
begin
 if (Self.Krok > 0) then Result := true else Result := false;
end;//function

function TJC.GetPostaveno():boolean;
begin
 if (Self.fstaveni.RozpadBlok > -5) then Result := true else Result := false; 
end;//function

////////////////////////////////////////////////////////////////////////////////

// true = je mozno DN
//tato funkce kontroluje, jestli je mozne znovupostavit cestu i kdyz byla fakticky zrusena = musi zkontrolovat vsechny podminky
function TJC.CanDN():boolean;
var i:Integer;
    Blk:TBlk;
begin
 // zkontrolujeme zavery bloku
 // JC NELZE obnovit z useku, na kterych uplne spadl zaver (do zadneho zaveru)
 // porusily by se reference na redukce menu
 for i := 0 to Self.fproperties.Useky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
   if (((Blk as TBlkUsek).Zaver = TJCType.no) or ((Blk as TBlkUsek).Zaver = TJCType.staveni) or ((Blk as TBlkUsek).NUZ) or
      ((Blk as TBlkUsek).Obsazeno <> TUsekStav.uvolneno)) then Exit(false);
  end;//for i

 // zkontrolujeme polohu vyhybek
 for i := 0 to Self.fproperties.Vyhybky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
   if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha) then Exit(false);
  end;//for i

 // zkontrolujeme polohu odvratu
 for i := 0 to Self.fproperties.Odvraty.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
   if ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Odvraty[i].Poloha) then Exit(false);
  end;//for i

 // zkontrolujeme poruchy prejezdu
 //  prejezdy, na kterych je zaver, by taky mely byt uzavrene
 for i := 0 to Self.fproperties.Prejezdy.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);
   if (((Blk as TBlkPrejezd).Stav.basicStav = TBlkPrjBasicStav.none) or
      ((Blk as TBlkPrejezd).Stav.basicStav = TBlkPrjBasicStav.disabled)) then Exit(false);
   if (((Blk as TBlkPrejezd).Zaver) and ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno)) then Exit(false);
  end;//for i

 //zkontrolujeme trat
 if (Self.fproperties.Trat > -1) then
  begin
   Blky.GetBlkByID(Self.fproperties.Trat, Blk);
   if (((Blk as TBlkTrat).Obsazeno) or ((Blk as TBlkTrat).ZAK) or ((Blk as TBlkTrat).RBPCan)) then
     Exit(false);
  end;

  // kontrola polohy podminkovych vyhybek:
  for i := 0 to Self.fproperties.podminky.vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.vyhybky[i].Blok, Blk);

    // kontrola koncove polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.podminky.vyhybky[i].Poloha) then
      Exit(false);
   end;//for i

  // kontrola uzamceni podminkovych zamku:
  for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, Blk);

    // kontrola uzamceni
    if ((Blk as TBlkZamek).klicUvolnen) then
      Exit(false);
   end;//for i

 Result := true;
end;//function

// DN nastavi zavery vsech bloku na validni a rozsviti navestidlo
// tato procedura predpoklada, ze podminky pro DN jsou splneny
procedure TJC.DN();
begin
 writelog('DN JC '+Self.nazev, WR_VC);

 // tohleto je finta, jak vykonat jen posledni krok staveni JC
 Self.Krok := 14;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// volano z navestidla pri STUJ
// nevolat nidky jindy !
procedure TJC.STUJ();
begin
 Self.RozpadBlok := -2;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.UsekClosePrj(Sender:TObject; data:Integer);
var i:Integer;
    Blk:TBlk;
    func:TChangeEvent;
begin
 if (not Self.postaveno) then Exit(); 

 // pridani zruseni redukce, tim se prejezd automaticky otevre po zruseni zaveru bloku pod nim
 // prejezd se taky prikazem AddToReductionDb automaticky uzavre
 Blky.GetBlkByID(Self.fproperties.Prejezdy[data].oteviraci, Blk);
 case (Blk.GetGlobalSettings().typ) of
   _BLK_USEK: (Blk as TBlkUsek).AddToReductionDB(Self.fproperties.Prejezdy[data].Prejezd);
 end;

 writelog('JC '+Self.nazev+': obsazen '+(Sender as TBlkUsek).GetGlobalSettings().name+' - uzaviram prejezd '+Blk.GetGlobalSettings().name, WR_VC, 0);

 func.func := Self.UsekClosePrj;
 func.data := data;

 for i := 0 to Length(Self.fproperties.Prejezdy[data].uzaviraci)-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Prejezdy[data].uzaviraci[i], Blk);
   (Blk as TBlkUsek).RemoveChangeEvent((Blk as TBlkUsek).EventsOnObsaz, func);
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.SetRozpadBlok(RozpadBlok:Integer);
begin
 Self.fstaveni.RozpadBlok := RozpadBlok;
 Self.changed := true;
end;//procedure

procedure TJC.SetRozpadRuseniBlok(RozpadRuseniBlok:Integer);
begin
 Self.fstaveni.RozpadRuseniBlok := RozpadRuseniBlok;
 Self.changed := true;
end;//procedure

procedure TJC.SetKrok(Krok:Integer);
begin
 Self.fstaveni.Krok := Krok;
 Self.changed := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

class function TJC.JCBariera(typ:Integer; Blok:TBlk = nil; param:Integer = 0):TJCBariera;
begin
 Result.typ   := typ;
 Result.blok  := Blok;
 Result.param := param;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TJC.JCBarieraToMessage(Bariera:TJCBariera):TUPOItem;
var i:Integer;
    lines:TStrings;
begin
 for i := 0 to _UPO_LINES-1 do
  begin
   Result[i].str := '';
   Result[i].fg  := clNone;
   Result[i].bg  := clNone;
  end;


 case (Bariera.typ) of
  _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_TYP, _JCB_SCOM_NOT_USEK,
  _JCB_USEK_OBSAZENO, _JCB_USEK_ZAVER, _JCB_USEK_SOUPRAVA,
  _JCB_VYHYBKA_KONC_POLOHA, _JCB_VYHYBKA_ZAMCENA, _JCB_VYHYBKA_NOUZ_ZAVER,
  _JCB_PREJEZD_NOUZOVE_OTEVREN, _JCB_PREJEZD_PORUCHA,
  _JCB_ODVRAT_ZAMCENA, _JCB_ODVRAT_OBSAZENA, _JCB_ODVRAT_KONC_POLOHA,
  _JCB_TRAT_ZAK, _JCB_TRAT_ZAVER, _JCB_TRAT_OBSAZENO, _JCB_TRAT_ZADOST, _JCB_TRAT_NESOUHLAS,
  _JCB_ZAMEK_NEUZAMCEN, _JCB_VYHYBKA_NESPAVNA_POLOHA:
  begin
    Result[0] := GetUPOLine('NEP��PUSTN�', taCenter, clRed, clWhite);
    if (Assigned(Bariera.blok)) then
      Result[2] := GetUPOLine(Bariera.blok.GetGlobalSettings().name)
    else
      Result[2] := GetUPOLine('ID' + IntToStr(bariera.param));
  end;
 end;//case


 case (Bariera.typ) of
  _JCB_OK                      : Result[0] := GetUPOLine('OK', taCenter, clBlue, $A0A0A0);
  _JCB_STAVENI                 : Result[0] := GetUPOLine('Ji� se stav�', taCenter, clBlue, $A0A0A0);

  _JCB_BLOK_DISABLED           : Result[1] := GetUPOLine('Blok neaktivn�');
  _JCB_BLOK_NOT_EXIST          : Result[1] := GetUPOLine('Blok neexistuje');
  _JCB_BLOK_NOT_TYP            : Result[1] := GetUPOLine('Blok nen� spr�vn�ho typu');

  _JCB_SCOM_NOT_USEK           : Result[1] := GetUPOLine('Nen� �sek p�ed n�v�stidlem');

  _JCB_USEK_OBSAZENO           : Result[1] := GetUPOLine('�sek obsazen');
  _JCB_USEK_ZAVER              : Result[1] := GetUPOLine('�sek zapevn�n');
  _JCB_USEK_SOUPRAVA           : Result[1] := GetUPOLine('Souprava');

  _JCB_VYHYBKA_KONC_POLOHA     : Result[1] := GetUPOLine('Nen� koncov� poloha');
  _JCB_VYHYBKA_ZAMCENA         : Result[1] := GetUPOLine('Zam�ena');
  _JCB_VYHYBKA_NOUZ_ZAVER      : Result[1] := GetUPOLine('Nouzov� z�v�r');
  _JCB_VYHYBKA_NESPAVNA_POLOHA : Result[1] := GetUPOLine('Nespr�vn� poloha');

  _JCB_PREJEZD_NOUZOVE_OTEVREN : Result[1] := GetUPOLine('Nouzov� otev�en');
  _JCB_PREJEZD_PORUCHA         : Result[1] := GetUPOLine('Poruchov� stav');

  _JCB_ODVRAT_ZAMCENA          : Result[1] := GetUPOLine('Zam�ena');
  _JCB_ODVRAT_OBSAZENA         : Result[1] := GetUPOLine('Obsazena');
  _JCB_ODVRAT_KONC_POLOHA      : Result[1] := GetUPOLine('Nen� koncov� poloha');

  _JCB_TRAT_ZAK                : Result[1] := GetUPOLine('Z�kaz odjezdu');
  _JCB_TRAT_ZAVER              : Result[1] := GetUPOLine('Z�v�r');
  _JCB_TRAT_OBSAZENO           : Result[1] := GetUPOLine('Obsazena');
  _JCB_TRAT_ZADOST             : Result[1] := GetUPOLine('Prob�h� ��dost');
  _JCB_TRAT_NESOUHLAS          : Result[1] := GetUPOLine('Nesouhlas');

  _JCB_ZAMEK_NEUZAMCEN         : Result[1] := GetUPOLine('Neuzam�en');
  _JCB_ZAMEK_NOUZ_ZAVER        : Result[1] := GetUPOLine('Nen� nouzov� z�v�r');

  _JCB_USEK_VYLUKA             : begin
    Result[0] := GetUPOLine('V�LUKA '+Bariera.blok.GetGlobalSettings().name, taCenter, clBlack, clOlive);
    lines := GetLines((Bariera.blok as TBlkUsek).Vyluka, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 2) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;
  _JCB_USEK_STITEK             : begin
    Result[0] := GetUPOLine('�T�TEK '+Bariera.blok.GetGlobalSettings().name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkUsek).Stitek, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 2) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;

  _JCB_VYHYBKA_VYLUKA          : begin
    Result[0] := GetUPOLine('V�LUKA '+Bariera.blok.GetGlobalSettings().name, taCenter, clBlack, clOlive);
    lines := GetLines((Bariera.blok as TBlkVyhybka).Vyluka, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 1) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;

  _JCB_VYHYBKA_STITEK          : begin
    Result[0] := GetUPOLine('�T�TEK '+Bariera.blok.GetGlobalSettings().name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkVyhybka).Stitek, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 2) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;

  _JCB_PREJEZD_STITEK          : begin
    Result[0] := GetUPOLine('�T�TEK '+Bariera.blok.GetGlobalSettings().name, taCenter, clBlack, clTeal);
    lines := GetLines((Bariera.blok as TBlkPrejezd).Stitek, _UPO_LINE_LEN);
    Result[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
    if (lines.Count > 2) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    lines.Free();
  end;

  _JCB_PRIVOLAVACKA : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Sv�t� p�ivol�vac� n�v�st');
    Result[2] := GetUPOLine(Bariera.blok.GetGlobalSettings().name);
  end;

  _JCB_HV_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Hnac� vozidlo v ru�n�m ��zen�');
    Result[2] := GetUPOLine(IntToStr(Bariera.param) + ' : ' + HVDb.HVozidla[Bariera.param].Data.Nazev);
  end;

  _JCB_HV_NOT_ALL_RUC : begin
    Result[0] := GetUPOLine('POZOR !', taCenter, clYellow, $A0A0A0);
    Result[1] := GetUPOLine('Ne v�echna HV v ru�n�m ��zen�');
    Result[2] := GetUPOLine('');
  end;

 else
  Result[0] := GetUPOLine('Nezn�m� bari�ra ve stav�n� JC', taCenter, clRed, clWhite);
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

// vraci true, pokud je zadana bariera tzv. kriticka, jinak false
// kriticka bariera je takova bariera, jejiz odstraneni neni berzny uzivatel schopen
//   napr. absence existence urcitych bloku v jizdni ceste apod.
class function TJC.CriticalBariera(typ:Integer):boolean;
begin
 case (typ) of
  _JCB_STAVENI, _JCB_BLOK_DISABLED, _JCB_BLOK_NOT_EXIST, _JCB_BLOK_NOT_TYP :
            Result := true;
 else
  Result := false;
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TJC.WarningBariera(typ:Integer):boolean;
begin
 case (typ) of
  _JCB_USEK_STITEK, _JCB_USEK_VYLUKA, _JCB_VYHYBKA_STITEK, _JCB_VYHYBKA_VYLUKA, _JCB_PREJEZD_STITEK,
  _JCB_PRIVOLAVACKA, _JCB_HV_RUC, _JCB_HV_NOT_ALL_RUC:
            Result := true;
 else
  Result := false;
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TJC.PotvrSekvBariera(typ:Integer):boolean;
begin
 case (typ) of
  _JCB_VYHYBKA_VYLUKA, _JCB_USEK_VYLUKA:
            Result := true;
 else
  Result := false;
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TJC.PotvrSekvBarieraToReason(typ:Integer):string;
begin
 case (typ) of
  _JCB_VYHYBKA_VYLUKA : Result := 'V�luka v�hybkov�ho bloku';
  _JCB_USEK_VYLUKA    : Result := 'V�luka kolejov�ho �seku';
 else
  Result := '';
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TJC.CritBarieraEsc(Sender:TObject);
begin
 Self.CancelStaveni('', true);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaJCPC(Sender:TObject);
begin
 if (not Self.staveni) then Exit();

 ORTCPServer.BottomError(Self.fstaveni.SenderPnl, 'Nep�estavena '+(Sender as TBlkVyhybka).GetGlobalSettings.name, (Self.fstaveni.SenderOR as TOR).ShortName, 'TECHNOLOGIE');
 Self.CancelStaveni();
 Self.RusJC();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.VyhNeprestavenaNC(Sender:TObject);
begin

end;//procedure

procedure TJC.VyhPrestavenaNC(Sender:TObject);
begin
 (Sender as TBlkVyhybka).vyhZaver := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// generuje podminky branici postaveni nouzove posunove ceste
//  tyto podminky jsou prubezne zobrazovany dispecerovi v potvrzovaci sekvenci

procedure TJC.PodminkyNCStaveni(var bariery:TList<TJCBariera>);
var i:Integer;
    Blk,blk2:TBlk;
    glob:TBlkSettings;
begin
  // useky:
  for i := 0 to Self.fproperties.Useky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
    glob := Blk.GetGlobalSettings();

    // obsazenost
    if ((i <> Self.fproperties.Useky.Count-1) or (Self.fproperties.TypCesty <> TJCType.posun)) then
     begin
      if ((Blk as TBlkUsek).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk, Blk.GetGlobalSettings.id));
     end;//if

    // souprava
    if (((Blk as TBlkUsek).Souprava > -1) and (Self.fproperties.TypCesty = TJCType.vlak)) then
      bariery.Add(Self.JCBariera(_JCB_USEK_SOUPRAVA, Blk, Blk.GetGlobalSettings.id));
   end;//for i

  // kontrola vyhybek:
  for i := 0 to Self.fproperties.Vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.GetGlobalSettings.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // kontrola spojky
    Blky.GetBlkByID((Blk as TBlkVyhybka).GetSettings.spojka, Blk2);
    if ((blk2 <> nil) and ((Blk as TBlkVyhybka).Poloha <> Self.fproperties.Vyhybky[i].Poloha)) then
     begin
      if (not (Blk2 as TBlkVyhybka).vyhZaver) then
        bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk2, Blk2.GetGlobalSettings.id));

      if ((Blk2 as TBlkVyhybka).Obsazeno = TUsekStav.obsazeno) then
        bariery.Add(Self.JCBariera(_JCB_USEK_OBSAZENO, Blk2, Blk2.GetGlobalSettings.id));
     end;
   end;//for i

  // kontrola prejezdu
  for i := 0 to Self.fproperties.Prejezdy.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Prejezdy[i].Prejezd, Blk);

    if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.none) then
     begin
      if ((Blk as TBlkPrejezd).Stav.PC_NOT) then
       begin
        bariery.Add(Self.JCBariera(_JCB_PREJEZD_NOUZOVE_OTEVREN, blk, Self.fproperties.Prejezdy[i].Prejezd));
       end else begin
        if ((Blk as TBlkPrejezd).Stav.basicStav <> TBlkPrjBasicStav.uzavreno) then
          bariery.Add(Self.JCBariera(_JCB_PREJEZD_NEUZAVREN, blk, Self.fproperties.Prejezdy[i].Prejezd));
       end;
     end else begin
      bariery.Add(Self.JCBariera(_JCB_PREJEZD_PORUCHA, blk, Self.fproperties.Prejezdy[i].Prejezd));
     end;//else NouzoveOtevreni
   end;//for i

  // kontrola odvratu
  for i := 0 to Self.fproperties.Odvraty.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.Odvraty[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.Vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_KONC_POLOHA, Blk, Blk.GetGlobalSettings.id));

    // kontrola nouzoveho zaveru:
    if (not (Blk as TBlkVyhybka).vyhZaver) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NOUZ_ZAVER, Blk, Blk.GetGlobalSettings.id));

    // to-do: odvrat spojka
   end;//for i

  if (Self.fproperties.Trat > -1) then
   begin
    Blky.GetBlkByID(Self.fproperties.Trat, Blk);
    glob := Blk.GetGlobalSettings();

    if ((blk as TBlkTrat).ZAK) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAK, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Zaver) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZAVER, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Obsazeno) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_OBSAZENO, blk, Self.fproperties.Trat));
    if ((blk as TBlkTrat).Zadost) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_ZADOST, blk, Self.fproperties.Trat));
    if ((((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.souhlas) or ((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.nabidka) or (((blk as TBlkTrat).GetSettings().zabzar = TTratZZ.bezsouhas) and ((blk as TBlkTrat).nouzZaver)))
        and (Self.fproperties.TratSmer <> (blk as TBlkTrat).Smer)) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NESOUHLAS, blk, Self.fproperties.Trat));
    if (not (blk as TBlkTrat).BP) then
      bariery.Add(Self.JCBariera(_JCB_TRAT_NO_BP, blk, Self.fproperties.Trat));
   end;

  // kontrola polohy podminkovych vyhybek:
  for i := 0 to Self.fproperties.podminky.vyhybky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.vyhybky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola koncove polohy:
    if ((Blk as TBlkVyhybka).poloha <> Self.fproperties.podminky.vyhybky[i].Poloha) then
      bariery.Add(Self.JCBariera(_JCB_VYHYBKA_NESPAVNA_POLOHA, blk, blk.GetGlobalSettings().id));
   end;//for i

  // kontrola uzamceni podminkovych zamku:
  for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
   begin
    Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, Blk);
    glob := Blk.GetGlobalSettings();

    // kontrola uzamceni
    if ((Blk as TBlkZamek).klicUvolnen) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NEUZAMCEN, blk, blk.GetGlobalSettings().id));

    // kontrola uzamceni
    if (not (Blk as TBlkZamek).nouzZaver) then
      bariery.Add(Self.JCBariera(_JCB_ZAMEK_NOUZ_ZAVER, blk, blk.GetGlobalSettings().id));
   end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJC.NC_PS_Callback(Sender:TIdContext; success:boolean);
var i:Integer;
    blk:TBlk;
begin
 if (success) then
  begin
   if (Self.Krok = 101) then
     Self.Krok := 102;
  end else begin
   Self.CancelStaveni();

   for i := 0 to Self.fproperties.Useky.Count-1 do
    begin
     Blky.GetBlkByID(Self.fproperties.Useky[i], Blk);
     (Blk as TBlkUsek).Zaver := TJCType.no;
    end;//for cyklus
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJC.BarieryNCToPotvr(bariery:TJCBariery):TPSPodminky;
var i:Integer;
begin
 Result := TList<TPSPodminka>.Create();

 for i := 0 to bariery.Count-1 do
  begin
   case (bariery[i].typ) of
    _JCB_USEK_OBSAZENO           : Result.Add(GetPSPodminka(bariery[i].blok, '�sek obsazen'));
    _JCB_USEK_SOUPRAVA           : Result.Add(GetPSPodminka(bariery[i].blok, '�sek obsahuje soupravu'));

    _JCB_PREJEZD_NOUZOVE_OTEVREN : Result.Add(GetPSPodminka(bariery[i].blok, 'Nouzov� otev�en'));
    _JCB_PREJEZD_PORUCHA         : Result.Add(GetPSPodminka(bariery[i].blok, 'Porucha'));
    _JCB_PREJEZD_NEUZAVREN       : Result.Add(GetPSPodminka(bariery[i].blok, 'Neuzav�en'));

    _JCB_VYHYBKA_KONC_POLOHA     : Result.Add(GetPSPodminka(bariery[i].blok, 'Nen� spr�vn� poloha'));
    _JCB_VYHYBKA_NOUZ_ZAVER      : Result.Add(GetPSPodminka(bariery[i].blok, 'Nen� zaveden nouzov� z�v�r'));
    _JCB_VYHYBKA_NESPAVNA_POLOHA : Result.Add(GetPSPodminka(bariery[i].blok, 'Nen� spr�vn� poloha'));

    _JCB_TRAT_ZAK                : Result.Add(GetPSPodminka(bariery[i].blok, 'Z�kaz odjezdu'));
    _JCB_TRAT_ZAVER              : Result.Add(GetPSPodminka(bariery[i].blok, 'Z�v�r'));
    _JCB_TRAT_OBSAZENO           : Result.Add(GetPSPodminka(bariery[i].blok, 'Obsazeno'));
    _JCB_TRAT_ZADOST             : Result.Add(GetPSPodminka(bariery[i].blok, 'Prob�h� ��dost'));
    _JCB_TRAT_NESOUHLAS          : Result.Add(GetPSPodminka(bariery[i].blok, 'Nesouhlas'));
    _JCB_TRAT_NO_BP              : Result.Add(GetPSPodminka(bariery[i].blok, 'Blokov� podm�nka nezavedena'));

    _JCB_ZAMEK_NEUZAMCEN         : Result.Add(GetPSPodminka(bariery[i].blok, 'Neuzam�en'));
    _JCB_ZAMEK_NOUZ_ZAVER        : Result.Add(GetPSPodminka(bariery[i].blok, 'Nen� zaveden nouzov� z�v�r'));
   end;//case bariera typ
  end;//for i
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TJC.RNZ();
var i:Integer;
    blk, blk2:TBlk;
begin
 for i := 0 to Self.fproperties.Vyhybky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, blk);
   if ((blk as TBlkVyhybka).vyhZaver) then (blk as TBlkVyhybka).vyhZaver := false;
   if ((blk as TBlkVyhybka).GetSettings.spojka > -1) then
    begin
     Blky.GetBlkByID((blk as TBlkVyhybka).GetSettings.spojka, blk2);
     if ((blk2 as TBlkVyhybka).vyhZaver) then (blk2 as TBlkVyhybka).vyhZaver := false;
    end;
  end;//for i

 for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, blk);
   if ((blk as TBlkZamek).nouzZaver) then (blk as TBlkZamek).nouzZaver := false;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TJC.GetRNZ():TPSPodminky;
var i:Integer;
    blk, blk2:TBlk;
begin
 Result := TList<TPSPodminka>.Create();

 for i := 0 to Self.fproperties.Vyhybky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.Vyhybky[i].Blok, blk);
   if ((blk as TBlkVyhybka).vyhZaver) then Result.Add(GetPSPodminka(blk, 'Ru�en� NZ'));
   if ((blk as TBlkVyhybka).GetSettings.spojka > -1) then
    begin
     Blky.GetBlkByID((blk as TBlkVyhybka).GetSettings.spojka, blk2);
     if ((blk2 as TBlkVyhybka).vyhZaver) then Result.Add(GetPSPodminka(blk2, 'Ru�en� NZ'));
    end;
  end;//for i

 for i := 0 to Self.fproperties.podminky.zamky.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.podminky.zamky[i].Blok, blk);
   if ((blk as TBlkZamek).nouzZaver) then Result.Add(GetPSPodminka(blk, 'Ru�en� NZ'));
  end;//for i
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
