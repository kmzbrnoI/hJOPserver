unit TechnologieMTB;

// technologie MTB

// pricip:
//  - na zacatku vytvorime tridy pro vsechna existujici MTB
//  - po otevreni MTB zjistime, ktere desky jsou skutecne dostupne a ktere ne

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, OutputDriver;

type
  //events from libs
  TMainEvent = procedure() of object;
  TErrEvent = procedure(errValue: word; errAddr: byte; errMsg:string) of object;

  TMTBBoardOutputs = array[0..15] of Shortint;

  //////////////////////////////////////////////////////////////

  //toto se pouziva pro identifikaci desky a portu VSUDE v technologii
  TMTBAddr = record                                     // jedna fyzicke MTB spojeni
   board:Byte;                                              // cislo desky
   port:Byte;                                               // cislo portu
  end;

  TMTBBoard=record                                      // jedna MTB deska
    StavVyst:TMTBBoardOutputs;                              // stavy vystupu
    needed:boolean;                                         // jestli jed eska potrebna pro technologii (tj. jeslti na ni referuji nejake bloky atp.)
  end;

  //////////////////////////////////////////////////////////////

  //lib: 0 - neznama
  //     1 - mtb.dll
  //     2 - simulator.dll

  // Technologie MTB
  TMTB=class
   public const
     _MAX_MTB = 192;                                        // maximalni pocet MTB desek

   private
     Desky:array [0.._MAX_MTB-1] of TMTBBoard;              // MTB desky, pole je indexovano MTB adresami

     OD:TOutputdriver;                                      // refenrece na knihovnu pro komunikaci s MTB - Michalova knihovna

     alib:string;                                           // aktualni pouzivana knihovna
     aStart,aOpenned:boolean;                               // flag zapnute komunikace a otevreneho zarizeni
     afilename:string;                                      // filename ke konfiguracnimu souboru

     DllVersion:string;                                     // verze DLL knihovny
     DriverVersion:string;                                  // verze MTB driveru v DLL knihovne
     DeviceVersion:string;                                  // verze firmware v MTB-USB

     fGeneralError:boolean;                                 // flag oznamujici nastani "MTB general IO error" -- te nejhorsi veci na svete

     //events to the main program
     FOnOpen:TMainEvent;
     FOnClose:TMainEvent;
     FOnStart:TMainEvent;
     FOnStop:TMainEvent;
     FOnStartErr:TErrEvent;
     FOnStopErr:TErrEvent;
     FOnOpenErr:TErrEvent;
     FOnCloseErr:TErrEvent;

      function LibPossible(Lib:string):boolean;             // jestli je mozne nacist danou knihovnu (zadava se filaname)
      function GetLib():byte;                               // vrati index aktualne nactene knihovny

      //events from libraly
      procedure DllBeforeOpen(Sender:TObject);
      procedure DllAfterOpen(Sender:TObject);
      procedure DllBeforeClose(Sender:TObject);
      procedure DllAfterClose(Sender:TObject);

      procedure DllBeforeStart(Sender:TObject);
      procedure DllAfterStart(Sender:TObject);
      procedure DllBeforeStop(Sender:TObject);
      procedure DllAfterStop(Sender:TObject);

      procedure DllOnError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string);

      function GetMTBCount():Byte;                          // vrati pocet nalezenych MTB desek

   public
      constructor Create();
      destructor Destroy; override;

      function Open():Byte;                                 // otevre MTB zarizeni a naskenuje moduly
      function Close():Byte;                                // zapne komunikaci

      function LoadLib(NewLib:string;Must:Boolean=false):Byte; // nacte MTB knihovnu
      function Go(ShowForm:boolean = true):Byte;           // spusti komunikaci MTB
      function Stop:Byte;                                  // zastavi komunikaci MTB
      procedure InputSim;                                  // pokud je nactena knihovna Simulator.dll, simuluje vstupy (koncove polohy vyhybek atp.)
      procedure SoupravaUsekSim;                           // nastavit MTB vstupy tak, aby useky, n akterych existuje souprava, byly obsazene

      procedure SetOutput(MtbAdr:integer;vystup:integer;state:integer);
      procedure SetInput(MtbAdr:integer;vstup:integer;state:integer);
      function GetInput(MtbAdr:integer;vstup:integer):integer;
      function GetModuleFirmware(MtbAdr:integer):string;
      function GetOutputs(MtbAdr:integer):TMTBBoardOutputs;

      procedure SetNeeded(MtbAdr:Integer; state:boolean = true);
      function GetNeeded(MtbAdr:Integer):boolean;

      procedure ShowCfgDialog;
      procedure HideCfgDialog;
      procedure ShowAboutDialog;

      function LoadFromFile(FileName:string):Byte;
      function SaveToFile(FileName:string):Byte;

      function GetTypeIntMTB(addr:byte):Integer;
      function GetTypeStrMTB(addr:byte):string;
      function GetNameMTB(Addr:integer):string;

      function IsModule(addr:Integer):Boolean;

      procedure NullOutputs;

      property Start:boolean read aStart;
      property lib:byte read GetLib;
      property filename:string read afilename;
      property count:Byte read GetMtbCount;     // pouzivat jen zridka - trva dlouho !
      property Openned:boolean read aOpenned;

      property LibV:string read DllVersion;
      property DriverV:string read DriverVersion;
      property DeviceV:string read DeviceVersion;

      property generalError:boolean read fGeneralError;

      //events
      property OnOpen:TMainEvent read FOnOpen write FOnOpen;
      property OnClose:TMainEvent read FOnClose write FOnClose;
      property OnStart:TMainEvent read FOnStart write FOnStart;
      property OnStop:TMainEvent read FOnStop write FOnStop;

      property OnErrOpen:TErrEvent read FOnOpenErr write FOnOpenErr;
      property OnErrClose:TErrEvent read FOnCloseErr write FOnCloseErr;
      property OnErrStart:TErrEvent read FOnStartErr write FOnStartErr;
      property OnErrStop:TErrEvent read FOnStopErr write FOnStopErr;
  end;

var
  MTB:TMTB;                      //Zarizeni MTB


implementation

uses fMain, fSettings, RPConst, fLoginPozadi, fSystemInfo, fAdminForm,
     GetSystems, fSplash, TechnologieJC, FileSystem, TBLoky, TBlok, TBlokVyhybka,
     TBlokUsek, TBlokIR, TBlokSCom, BoosterDb, TBlokPrejezd,
     TOblsRizeni, Logging, TCPServerOR, SprDb;

constructor TMTB.Create();
begin
 inherited Create;

 Self.aOpenned := false;
 Self.aStart   := false;

 Self.fGeneralError := false;

 OD      := TOutputDriver.Create(nil);
 OD.Name := 'OD';
end;//constructor

destructor TMTB.Destroy();
begin
 //ulozeni lib po zavreni
 Self.SaveToFile(Self.afilename);

 FreeAndNil(OD);

 inherited Destroy;
end;//destructor

function TMTB.GetLib():byte;
var lower:string;
begin
 Result := 0;
 lower := LowerCase(Self.alib);

 if (lower = 'mtb.dll') then Exit(1);
 if (lower = 'simulator.dll') then Exit(2);
end;//fucntoion GetLib

function TMTB.LibPossible(Lib:string):boolean;
var lower:string;
begin
 Result := false;
 lower := LowerCase(Lib);

 if (lower = 'mtb.dll') then Exit(true);
 if (lower = 'simulator.dll') then Exit(true);
end;//function

function TMTB.LoadLib(NewLib:string;Must:Boolean=false):Byte;
 begin
  Result := 0;

  if (not Self.LibPossible(NewLib)) then Exit(1);

  if ((NewLib <> OD.Lib1) or (Must)) then
   begin
    OD.lib1 := NewLib;
    OD.LoadLib;
    Self.alib := LowerCase(NewLib);
    Self.DllVersion    := OD.GetLibVersion(0);
    Self.DriverVersion := OD.GetDriverVersion(0);

    //assign events
    OD.OnBeforeOpen   := Self.DllBeforeOpen;
    OD.OnAfterOpen    := Self.DllAfterOpen;
    OD.OnBeforeClose  := Self.DllBeforeClose;
    OD.OnAfterClose   := Self.DllAfterClose;

    OD.OnBeforeStart  := Self.DllBeforeStart;
    OD.OnAfterStart   := Self.DllAfterStart;
    OD.OnBeforeStop   := Self.DllBeforeStop;
    OD.OnAfterStop    := Self.DllAfterStop;

    OD.OnError        := Self.DllOnError;
   end else begin
    Result := 2;
   end;
 end;//function

function TMTB.Open():Byte;
begin
 if (Self.Start) then Exit(2);

 if (not Self.Openned) then
   Self.OD.Open(0);

 Result := 0;
end;//function

function TMTB.Close():Byte;
begin
 if (Self.Start) then Exit(2);

 if (Self.Openned) then
   Self.OD.Close(0);

 Self.fGeneralError := false;

 Result := 0;
end;//function

function TMTB.Go(ShowForm:boolean=true):Byte;
 begin
  if (not Self.aOpenned) then Exit(1);

  if (not Self.aStart) then
    OD.Start(0);

  Result := 0;
 end;//function

function TMTB.Stop:Byte;
 begin
  if (Self.Start) then
    OD.Stop(0);
  Result := 0;
 end;//function Stop

procedure TMTB.SetOutput(MtbAdr:integer;vystup:integer;state:integer);
 begin
  if (MTB.aStart) then
   begin
    // pokud ma uz vystup tento stav, nenastavuji ho...
    if (Self.Desky[MtbAdr].StavVyst[vystup] = state) then Exit;
      Self.Desky[MtbAdr].StavVyst[vystup] := state;

    OD.SetOutput(0, MtbAdr, vystup, state); //nastaveni fyzickeho vystupu
   end;//aStart
 end;//procedure

procedure TMTB.InputSim;
var cyklus:integer;
    Blk:TBlk;
begin
 if (MTB.alib = 'simulator.dll') then
  begin
   //nastaveni vyhybek do +
   for cyklus := 0 to Blky.Cnt-1 do
    begin
     Blky.GetBlkByIndex(cyklus,Blk);
     if (Blk.GetGlobalSettings.typ = _BLK_VYH) then
       OD.SetInput(0,(Blk as TBlkVyhybka).GetSettings().MTBAddrs.data[0].board, (Blk as TBlkVyhybka).GetSettings().MTBAddrs.data[0].port,1);
     if (Blk.GetGlobalSettings.typ = _BLK_PREJEZD) then
       OD.SetInput(0,(Blk as TBlkPrejezd).GetSettings().MTB, (Blk as TBlkPrejezd).GetSettings().MTBInputs.Otevreno, 1);
     if ((F_Admin.CHB_SimSoupravaUsek.Checked) and ((Blk.GetGlobalSettings.typ = _BLK_USEK) or (Blk.GetGlobalSettings.typ = _BLK_TU)) and ((Blk as TBlkUsek).Souprava > -1)) then
       OD.SetInput(0, (Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].board, (Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].port, 1);
    end;//for cyklus

   //defaultni stav zesilovacu
   for cyklus := 0 to BoostersDb.BoosterCnt-1 do
    begin
     OD.SetInput(0,BoostersDb.GetBooster(cyklus).bSettings.MTB.Napajeni.board,BoostersDb.GetBooster(cyklus).bSettings.MTB.Napajeni.port,0);
     OD.SetInput(0,BoostersDb.GetBooster(cyklus).bSettings.MTB.Zkrat.board,BoostersDb.GetBooster(cyklus).bSettings.MTB.Zkrat.port,0);
    end;
  end;//if alib <> MTB.dll
end;//procedure

//simulace obaszeni useku, na kterem je souprava
procedure TMTB.SoupravaUsekSim;
var i:Integer;
    Blk:TBlk;
 begin
  for i := 0 to Blky.Cnt-1 do
   begin
    Blky.GetBlkByIndex(i,Blk);
    if ((Blk.GetGlobalSettings().typ <> _BLK_USEK) and (Blk.GetGlobalSettings().typ <> _BLK_TU)) then continue;
    if ((Blk as TBlkUsek).Souprava > -1) then
      OD.SetInput(0,(Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].board,(Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].port,1);
   end;//for cyklus
 end;//procedure

function TMTB.GetTypeIntMTB(addr:byte):Integer;
var typ:string;
 begin
  typ := Self.GetTypeStrMTB(addr);

  if (typ = 'MTB-UNI') then Exit(0);
  if (typ = 'MTB-UNI out') then Exit(5);
  if (typ = 'MTB-TTL') then Exit(1);
  if (typ = 'MTB-TTL out') then Exit(2);
  if (typ = 'MTB-REG puls') then Exit(3);
  if (typ = 'MTB-POT') then Exit(4);

  Exit(-1);
 end;//function

function TMTB.GetTypeStrMTB(addr:byte):string;
 begin
  if (Self.Openned) then
    Result := Self.OD.GetModuleType(0, addr)
  else
    Result := 'modul neexistuje';
 end;//function

function TMTB.GetNameMTB(Addr:integer):string;
 begin
  Result := Self.OD.GetModuleName(0, addr);
 end;//function

function TMTB.LoadFromFile(FileName:string):Byte;
var ini:TMemIniFile;
 begin
  Self.afilename := FileName;

  try
    ini := TMemIniFile.Create(FileName);
  except
    Result := 1;
    Exit;
  end;

  Self.LoadLib(ini.ReadString('MTB', 'lib', 'simulator.dll'), true); //nacteni knihovny

  ini.Free();
  Result := 0;
 end;

function TMTB.SaveToFile(FileName:string):Byte;
var ini:TMemIniFile;
 begin
  try
    DeleteFile(FileName);
    ini := TMemIniFile.Create(FileName);
  except
    Exit(1);
  end;

  ini.WriteString('MTB', 'lib', Self.alib);

  ini.UpdateFile();
  ini.Free();

  Result := 0;
 end;//procedure

procedure TMTB.NullOutputs();
var i,j:Integer;
begin
 for i := 0 to _MAX_MTB-1 do
   if (Self.IsModule(i)) then
     for j := 0 to 15 do MTB.SetOutput(i, j, 0);
end;//procedure

function TMTB.GetInput(MtbAdr:integer;vstup:integer):integer;
begin
 if ((MtbAdr < 256) and (MtbAdr >= 0)) then
  begin
   Result := OD.GetInput(0, MtbAdr, vstup);
  end else begin
   Result := -1;
  end;
end;//function

procedure TMTB.SetInput(MtbAdr:integer;vstup:integer;state:integer);
begin
 OD.SetInput(0, MtbAdr, vstup, state)
end;//procedure

function TMTB.GetModuleFirmware(MtbAdr:integer):string;
begin
 result := OD.GetModuleFirmware(0,MtbAdr);
end;//function

function TMTB.GetOutputs(MtbAdr:integer):TMTBBoardOutputs;
begin
 Result := Self.Desky[MtbAdr].StavVyst;
end;//function

procedure TMTB.ShowCfgDialog;
begin
 OD.ShowConfigDialog(0);
end;//procedure

procedure TMTB.HideCfgDialog;
begin
 OD.HideConfigDialog(0);
end;//procedure

procedure TMTB.ShowAboutDialog;
begin
 OD.ShowAboutDialog(0);
end;//procedure

function TMTB.IsModule(addr:Integer):Boolean;
begin
 Result := Self.OD.GetModuleExists(0, addr);
end;//function

//----- events from dll begin -----

procedure TMTB.DllBeforeOpen(Sender:TObject);
begin

end;//rprocedure

procedure TMTB.DllAfterOpen(Sender:TObject);
begin
 Self.aOpenned := true;
 if (Assigned(Self.FOnOpen)) then Self.FOnOpen();
end;//procdure

procedure TMTB.DllBeforeClose(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterClose(Sender:TObject);
begin
 Self.fGeneralError := false;
 Self.aOpenned := false;
 if (Assigned(Self.FOnClose)) then Self.FOnClose();
end;//procdure

procedure TMTB.DllBeforeStart(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterStart(Sender:TObject);
begin
 Self.aStart := true;
 if (Assigned(Self.FOnStart)) then Self.FOnStart();
end;//procdure

procedure TMTB.DllBeforeStop(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterStop(Sender:TObject);
begin
 Self.aStart := false;
 if (Assigned(Self.FOnStop)) then Self.FOnStop();
end;//procdure

procedure TMTB.DllOnError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string);
begin
 writelog('MTB ERR: '+errMsg+' - val:'+IntToStr(errValue)+', board:'+IntToStr(errAddr), WR_MTB, 1);

 if (errAddr = 255) then
  begin
   //errors on main board (MTB-USB)
   case (errValue) of
    1,2,3    : if (Assigned(Self.OnErrOpen)) then Self.OnErrOpen(errValue, errAddr, errMsg);
    11       : if (Assigned(Self.OnErrClose)) then Self.OnErrClose(errValue, errAddr, errMsg);
    21,22,25 : if (Assigned(Self.OnErrStart)) then Self.OnErrStart(errValue, errAddr, errMsg);
    31       : if (Assigned(Self.OnErrStop)) then Self.OnErrStop(errValue, errAddr, errMsg);
    301..306,4: begin
      // general IO error
      F_Main.A_System_Start.Enabled := true;
      F_Main.A_System_Stop.Enabled  := true;
      writelog('MTB FTDI Error - '+IntToStr(errValue), WR_ERROR, 0);
      if (errValue = 4) then
       begin
        Blky.Disable();
        Soupravy.StopAllSpr();
       end;
      ORTCPServer.BroadcastBottomError('MTB FTDI error', 'TECHNOLOGIE');
    end;
   end;//case
  end else begin
   // errors on MTB boards
   case (errValue) of
    141: ORs.MTBFail(errAddr); // communication with module failed
    142:; // communication with module restored, nothing should be here
   end;
  end;//
end;//procedure

//----- events from dll end -----

function TMTB.GetMTBCount():Byte;
var addr:Integer;
begin
 Result := 0;
 for addr := 0 to _MAX_MTB-1 do
   if (Self.IsModule(addr)) then
     Result := Result + 1;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TMTB.SetNeeded(MtbAdr:Integer; state:boolean = true);
begin
 Self.Desky[MtbAdr].needed := state;
end;//procedure

function TMTB.GetNeeded(MtbAdr:Integer):boolean;
begin
 Result := Self.Desky[MtbAdr].needed;
end;//function


end.//unit
