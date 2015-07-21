unit RPConst;

// deklarace konstant programu

interface

uses Generics.Collections;

const
 _SB_LOG      = 0;
 _SB_MTB      = 1;
 _SB_INT      = 2;
 _SB_POZICE   = 3;
 _SB_SBERNICE = 4;
 _SB_PROC     = 5;

 _CONSOLE_V   = '1.5';
 _VERSION_PR  = '';

 //konstatny parametru
 _NOSPLASH = '-ns';

 //konstatny MTB typu
 MTB_UNI  = 0;
 MTB_TTL  = 1;
 MTB_TTLO = 2;
 MTB_REG  = 3;
 MTB_POT  = 4;
 MTB_UNIO = 5;

 _MAX_OR = 16;
 _MAX_ORREF = 16;

 // zvuky - musi korespondovat se zvuky klienta
 _SND_TRAT_ZADOST = 4;
 _SND_POTVR_SEKV  = 8;
 _SND_CHYBA       = 10;
 _SND_PRETIZENI   = 7;
 _SND_ZPRAVA      = 9;

type
 //globalni typy
 TORControlRights = (null = 0, read = 1, write = 2, superuser = 3);

 TJCType = (undefinned = -1, no = 0, vlak = 1, posun = 2, nouz = 3, staveni = 4); //vyuzivan v technologii bloku napriklad na zaver apod.

 TVyhPoloha  = (disabled = -5, none = -1, plus = 0, minus = 1, both = 2);

 TPanelButton = (left = 0, middle = 1, right = 2, F2 = 3, F3 = 4);

 THVStanoviste = (lichy = 0, sudy = 1);              // v jakem smeru se nachazi stanoviste A

 PTObject = ^TObject;

 PInt = ^Integer;

 TArSmallI = array of Integer;
 TArStr = array of string;
 PTArSmallI = ^TArSmallI;
 PTArStr = ^TArStr;

 // podminky potvrzovaci sekvence
 TPSPodminka = record
  blok:TObject;
  podminka:string;
 end;

 TPSPodminky = TList<TPSPodminka>;

 function ORRightsToString(rights:TORControlRights):string;

 function GetPSPodminka(blok:TObject; podminka:string):TPSPodminka;
 function GetPSPodminky(podm:TPSPodminka):TPSPodminky;

implementation

function ORRightsToString(rights:TORControlRights):string;
begin
 case (rights) of
  null      : Result := 'žádná oprávnìní';
  read      : Result := 'oprávnìní ke ètení';
  write     : Result := 'oprávnìní k zápisu';
  superuser : Result := 'superuser';
 else
  Result := '';
 end;
end;//function

function GetPSPodminka(blok:TObject; podminka:string):TPSPodminka;
begin
 Result.blok     := blok;
 Result.podminka := podminka;
end;//function

function GetPSPodminky(podm:TPSPodminka):TPSPodminky;
begin
 Result := TList<TPSPodminka>.Create();
 Result.Add(podm);
end;//function

end.//unit
