unit TechnologieMTB;

// technologie MTB

{
 Pricip:
  - na zacatku vytvorime tridy pro vsechna existujici MTB
  - po otevreni MTB zjistime, ktere desky jsou skutecne dostupne a ktere ne
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IniFiles, OutputDriver, Generics.Collections;

type
  TErrEvent = procedure(Sender:TObject; errValue: word; errAddr: byte; errMsg:string) of object;
  TMTBReadyEvent = procedure (Sender:TObject; ready:boolean) of object;
  TMTBBoardChangeEvent = procedure (Sender:TObject; board:byte) of object;

  //////////////////////////////////////////////////////////////

  //toto se pouziva pro identifikaci desky a portu VSUDE v technologii
  TMTBAddr = record                                                             // jedno fyzicke MTB spojeni
   board:Byte;                                                                    // cislo desky
   port:Byte;                                                                     // cislo portu
  end;

  TMTBBoard = class                                                               // jedna MTB deska
    needed:boolean;                                                               // jestli jed eska potrebna pro technologii (tj. jeslti na ni referuji nejake bloky atp.)
    inputChangedEv:TList<TMTBBoardChangeEvent>;
    outputChangedEv:TList<TMTBBoardChangeEvent>;

    constructor Create();
    destructor Destroy(); override;
  end;

  //////////////////////////////////////////////////////////////

  // Technologie MTB
  TMTB=class
   public const
     _MAX_MTB = 192;                                        // maximalni pocet MTB desek

   private
     Desky:array [0.._MAX_MTB-1] of TMTBBoard;              // MTB desky, pole je indexovano MTB adresami

     MTBdrv:TMTBIFace;                                      // refenrece na knihovnu pro komunikaci s MTB - Michalova knihovna

     alib:string;                                           // aktualni pouzivana knihovna
     aStart,aOpenned:boolean;                               // flag zapnute komunikace a otevreneho zarizeni
     afilename:string;                                      // filename ke konfiguracnimu souboru
     aReady:boolean;                                        // jestli je nactena knihovna vporadku a tudiz jestli lze zapnout systemy

     DllVersion:string;                                     // verze DLL knihovny
     DriverVersion:string;                                  // verze MTB driveru v DLL knihovne
     DeviceVersion:string;                                  // verze firmware v MTB-USB

     fGeneralError:boolean;                                 // flag oznamujici nastani "MTB general IO error" -- te nejhorsi veci na svete

     //events to the main program
     FOnOpen:TNotifyEvent;
     FOnClose:TNotifyEvent;
     FOnStart:TNotifyEvent;
     FOnStop:TNotifyEvent;
     FOnStartErr:TErrEvent;
     FOnStopErr:TErrEvent;
     FOnOpenErr:TErrEvent;
     FOnCloseErr:TErrEvent;
     FOnReady:TMTBReadyEvent;

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
      procedure DllOnInputChanged(Sender:TObject; module:byte);
      procedure DllOnOutputChanged(Sender:TObject; module:byte);

      function GetMTBCount():Byte;                                              // vrati pocet nalezenych MTB desek

   public
      constructor Create();
      destructor Destroy; override;

      procedure LoadLib(NewLib:string;Must:Boolean=false);                      // nacte knihovnu

      procedure Open();                                                         // otevre MTB zarizeni a naskenuje moduly
      procedure Close();                                                        // zapne komunikaci

      procedure Go();                                                           // spusti komunikaci MTB
      procedure Stop();                                                         // zastavi komunikaci MTB

      procedure InputSim;                                                       // pokud je nactena knihovna Simulator.dll, simuluje vstupy (koncove polohy vyhybek atp.)
      procedure SoupravaUsekSim;                                                // nastavit MTB vstupy tak, aby useky, n akterych existuje souprava, byly obsazene

      procedure SetOutput(MtbAdr:integer;vystup:integer;state:integer);
      procedure SetInput(MtbAdr:integer;vstup:integer;state:integer);
      function GetInput(MtbAdr:integer;vstup:integer):integer;
      function GetOutput(MtbAdr:integer; port:integer):integer;
      function GetModuleFirmware(MtbAdr:integer):string;

      procedure SetNeeded(MtbAdr:Integer; state:boolean = true);
      function GetNeeded(MtbAdr:Integer):boolean;

      procedure ShowCfgDialog;
      procedure HideCfgDialog;
      procedure ShowAboutDialog;

      procedure LoadFromFile(FileName:string);
      function SaveToFile(FileName:string):Byte;

      function GetTypeIntMTB(addr:byte):Integer;
      function GetTypeStrMTB(addr:byte):string;
      function GetNameMTB(Addr:integer):string;

      function IsModule(addr:Integer):Boolean;

      procedure NullOutputs();

      procedure AddInputChangeEvent(board:Integer; event:TMTBBoardChangeEvent);
      procedure RemoveInputChangeEvent(event:TMTBBoardChangeEvent; board:Integer = -1);

      procedure AddOutputChangeEvent(board:Integer; event:TMTBBoardChangeEvent);
      procedure RemoveOutputChangeEvent(event:TMTBBoardChangeEvent; board:Integer = -1);

      property Start:boolean read aStart;
      property filename:string read afilename;
      property lib:string read alib;
      property count:Byte read GetMtbCount;     // pouzivat jen zridka - trva dlouho !
      property Openned:boolean read aOpenned;

      property LibV:string read DllVersion;
      property DriverV:string read DriverVersion;
      property DeviceV:string read DeviceVersion;

      property generalError:boolean read fGeneralError;

      //events
      property OnOpen:TNotifyEvent read FOnOpen write FOnOpen;
      property OnClose:TNotifyEvent read FOnClose write FOnClose;
      property OnStart:TNotifyEvent read FOnStart write FOnStart;
      property OnStop:TNotifyEvent read FOnStop write FOnStop;

      property OnErrOpen:TErrEvent read FOnOpenErr write FOnOpenErr;
      property OnErrClose:TErrEvent read FOnCloseErr write FOnCloseErr;
      property OnErrStart:TErrEvent read FOnStartErr write FOnStartErr;
      property OnErrStop:TErrEvent read FOnStopErr write FOnStopErr;

      property OnReady:TMTBReadyEvent read FOnReady write FOnReady;
      property ready:boolean read aready;
  end;

var
  MTB:TMTB;                      //Zarizeni MTB


implementation

uses fMain, fSettings, RPConst, fLoginPozadi, fSystemInfo, fAdminForm,
     GetSystems, fSplash, TechnologieJC, FileSystem, TBLoky, TBlok, TBlokVyhybka,
     TBlokUsek, TBlokIR, TBlokSCom, BoosterDb, TBlokPrejezd,
     TOblsRizeni, Logging, TCPServerOR, SprDb;

constructor TMTB.Create();
var i:Integer;
begin
 inherited;

 for i := 0 to _MAX_MTB-1 do
   Self.Desky[i] := TMTBBoard.Create();

 Self.aOpenned := false;
 Self.aStart   := false;
 Self.aReady   := false;
 Self.fGeneralError := false;

 MTBdrv := TMTBIFace.Create(nil);
end;//constructor

destructor TMTB.Destroy();
var i:Integer;
begin
 //ulozeni lib po zavreni
 Self.SaveToFile(Self.afilename);
 FreeAndNil(MTBdrv);

 for i := 0 to _MAX_MTB-1 do
   if (Assigned(Self.Desky[i])) then FreeAndNil(Self.Desky[i]);

 inherited;
end;//destructor

procedure TMTB.LoadLib(NewLib:string;Must:Boolean=false);
 begin
  if (not FileExists(NewLib)) then raise Exception.Create('Library file not found, not loading');

  if ((NewLib <> MTBdrv.Lib) or (Must)) then
   begin
    MTBdrv.Lib := NewLib;
    Self.alib := LowerCase(NewLib);

    try
      MTBdrv.LoadLib();
    except
      if (self.aReady) then
       begin
        Self.aReady := false;
        if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
       end;
      raise;
    end;

    try
      Self.DllVersion := MTBdrv.GetLibVersion();
    except
      Self.DllVersion := 'neznámá';
    end;

    try
      Self.DriverVersion := MTBdrv.GetDriverVersion();
    except
      Self.DriverVersion := 'neznámá';
    end;

    //assign events
    MTBdrv.OnBeforeOpen    := Self.DllBeforeOpen;
    MTBdrv.OnAfterOpen     := Self.DllAfterOpen;
    MTBdrv.OnBeforeClose   := Self.DllBeforeClose;
    MTBdrv.OnAfterClose    := Self.DllAfterClose;

    MTBdrv.OnBeforeStart   := Self.DllBeforeStart;
    MTBdrv.OnAfterStart    := Self.DllAfterStart;
    MTBdrv.OnBeforeStop    := Self.DllBeforeStop;
    MTBdrv.OnAfterStop     := Self.DllAfterStop;

    MTBdrv.OnError         := Self.DllOnError;
    MTBdrv.OnInputChanged  := Self.DllOnInputChanged;
    MTBdrv.OnOutputChanged := Self.DllOnOutputChanged;

    writelog('Naètena knihovna '+ Self.lib, WR_MTB);

    if (not self.aReady) then
     begin
      Self.aReady := true;
      if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
     end;
   end;
 end;//function

procedure TMTB.Open();
begin
 if ((not Self.Start) and (not Self.Openned)) then MTBdrv.Open();
end;//function

procedure TMTB.Close();
begin
 if (Self.Start) then Exception.Create('MTB komunikace není zastavena');
 if (Self.Openned) then MTBdrv.Close();
 Self.fGeneralError := false;
end;//function

procedure TMTB.Go();
 begin
  if (not Self.aOpenned) then raise Exception.Create('MTB není otevøeno');
  if (not Self.aStart) then MTBdrv.Start();
 end;//function

procedure TMTB.Stop();
 begin
  if (Self.Start) then MTBdrv.Stop();
 end;//function Stop

procedure TMTB.SetOutput(MtbAdr:integer;vystup:integer;state:integer);
 begin
  if (Self.Start) then
   begin
    try
      MTBdrv.SetOutput(MtbAdr, vystup, state); //nastaveni fyzickeho vystupu
    except

    end;
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
       Self.SetInput((Blk as TBlkVyhybka).GetSettings().MTBAddrs.data[0].board, (Blk as TBlkVyhybka).GetSettings().MTBAddrs.data[0].port,1);
     if (Blk.GetGlobalSettings.typ = _BLK_PREJEZD) then
       Self.SetInput((Blk as TBlkPrejezd).GetSettings().MTB, (Blk as TBlkPrejezd).GetSettings().MTBInputs.Otevreno, 1);
     if ((F_Admin.CHB_SimSoupravaUsek.Checked) and ((Blk.GetGlobalSettings.typ = _BLK_USEK) or (Blk.GetGlobalSettings.typ = _BLK_TU)) and ((Blk as TBlkUsek).Souprava > -1)) then
       Self.SetInput((Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].board, (Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].port, 1);
    end;//for cyklus

   //defaultni stav zesilovacu
   for cyklus := 0 to BoostersDb.BoosterCnt-1 do
    begin
     Self.SetInput(BoostersDb.GetBooster(cyklus).bSettings.MTB.Napajeni.board,BoostersDb.GetBooster(cyklus).bSettings.MTB.Napajeni.port,0);
     Self.SetInput(BoostersDb.GetBooster(cyklus).bSettings.MTB.Zkrat.board,BoostersDb.GetBooster(cyklus).bSettings.MTB.Zkrat.port,0);
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
      Self.SetInput((Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].board,(Blk as TBlkUsek).GetSettings().MTBAddrs.data[0].port,1);
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
   begin
    try
      Result := Self.MTBdrv.GetModuleType(addr)
    except
      Result := '---';
    end;
   end else
     Result := 'modul neexistuje';
 end;//function

function TMTB.GetNameMTB(Addr:integer):string;
 begin
  try
    Result := Self.MTBdrv.GetModuleName(addr);
  except
    Result := '---';
  end;
 end;//function

procedure TMTB.LoadFromFile(FileName:string);
var ini:TMemIniFile;
    lib:string;
 begin
  Self.afilename := FileName;

  ini := nil;
  try
    ini := TMemIniFile.Create(FileName);
    lib := ini.ReadString('MTB', 'lib', 'simulator.dll')
  except
    lib := 'simulator.dll';
  end;

  try
    Self.LoadLib(lib, true); //nacteni knihovny
  except
    on E:Exception do
      writelog('Nelze naèíst knihovnu '+lib+': '+E.Message, WR_ERROR);
  end;

  ini.Free();
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
   try
     Result := MTBdrv.GetInput(MtbAdr, vstup);
   except
     Result := -1;
   end;
  end else begin
   Result := -1;
  end;
end;//function

function TMTB.GetOutput(MtbAdr:integer; port:integer):integer;
begin
 if ((MtbAdr < 256) and (MtbAdr >= 0)) then
  begin
   try
     Result := MTBdrv.GetOutput(MtbAdr, port);
   except
     Result := -1;
   end;
  end else begin
   Result := -1;
  end;
end;

procedure TMTB.SetInput(MtbAdr:integer;vstup:integer;state:integer);
begin
 try
   MTBdrv.SetInput(MtbAdr, vstup, state);
 except

 end;
end;//procedure

function TMTB.GetModuleFirmware(MtbAdr:integer):string;
begin
 try
   Result := MTBdrv.GetModuleFirmware(MtbAdr);
 except
   Result := '-';
 end;
end;//function

procedure TMTB.ShowCfgDialog;
begin
 try
   MTBdrv.ShowConfigDialog();
 except

 end;
end;//procedure

procedure TMTB.HideCfgDialog;
begin
 try
   MTBdrv.HideConfigDialog();
 except

 end;
end;//procedure

procedure TMTB.ShowAboutDialog;
begin
 try
   MTBdrv.ShowAboutDialog();
 except

 end;
end;//procedure

function TMTB.IsModule(addr:Integer):Boolean;
begin
 try
   Result := MTBdrv.GetModuleExists(addr);
 except
   Result := false;
 end;
end;//function

//----- events from dll begin -----

procedure TMTB.DllBeforeOpen(Sender:TObject);
begin

end;//rprocedure

procedure TMTB.DllAfterOpen(Sender:TObject);
begin
 Self.aOpenned := true;
 if (Assigned(Self.OnOpen)) then Self.OnOpen(Self);
end;//procdure

procedure TMTB.DllBeforeClose(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterClose(Sender:TObject);
begin
 Self.fGeneralError := false;
 Self.aOpenned := false;
 if (Assigned(Self.OnClose)) then Self.OnClose(Self);
end;//procdure

procedure TMTB.DllBeforeStart(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterStart(Sender:TObject);
begin
 Self.aStart := true;
 if (Assigned(Self.OnStart)) then Self.OnStart(Self);
end;//procdure

procedure TMTB.DllBeforeStop(Sender:TObject);
begin

end;//procdure

procedure TMTB.DllAfterStop(Sender:TObject);
begin
 Self.aStart := false;
 if (Assigned(Self.OnStop)) then Self.OnStop(Self);
end;//procdure

procedure TMTB.DllOnError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string);
begin
 writelog('MTB ERR: '+errMsg+' - val:'+IntToStr(errValue)+', board:'+IntToStr(errAddr), WR_MTB, 1);

 if (errAddr = 255) then
  begin
   //errors on main board (MTB-USB)
   case (errValue) of
    1,2,3    : if (Assigned(Self.OnErrOpen)) then Self.OnErrOpen(Self, errValue, errAddr, errMsg);
    11       : if (Assigned(Self.OnErrClose)) then Self.OnErrClose(Self, errValue, errAddr, errMsg);
    21,22,25 : if (Assigned(Self.OnErrStart)) then Self.OnErrStart(Self, errValue, errAddr, errMsg);
    31       : if (Assigned(Self.OnErrStop)) then Self.OnErrStop(Self, errValue, errAddr, errMsg);
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

procedure TMTB.DllOnInputChanged(Sender:TObject; module:byte);
var i:Integer;
begin
 for i := Self.Desky[module].inputChangedEv.Count-1 downto 0 do
   if (Assigned(Self.Desky[module].inputChangedEv[i])) then Self.Desky[module].inputChangedEv[i](Self, module)
     else Self.Desky[module].inputChangedEv.Delete(i);
end;

procedure TMTB.DllOnOutputChanged(Sender:TObject; module:byte);
var i:Integer;
begin
 for i := Self.Desky[module].outputChangedEv.Count-1 downto 0 do
   if (Assigned(Self.Desky[module].outputChangedEv[i])) then Self.Desky[module].outputChangedEv[i](Self, module)
     else Self.Desky[module].outputChangedEv.Delete(i);
end;

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

////////////////////////////////////////////////////////////////////////////////

constructor TMTBBoard.Create();
begin
 Self.inputChangedEv  := TList<TMTBBoardChangeEvent>.Create();
 Self.outputChangedEv := TList<TMTBBoardChangeEvent>.Create();
end;//ctor

destructor TMTBBoard.Destroy();
begin
 Self.inputChangedEv.Free();
 Self.outputChangedEv.Free();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TMTB.AddInputChangeEvent(board:Integer; event:TMTBBoardChangeEvent);
begin
 if ((board >= 0) and (board < _MAX_MTB)) then
   if (Self.Desky[board].inputChangedEv.IndexOf(event) = -1) then Self.Desky[board].inputChangedEv.Add(event);
end;

procedure TMTB.RemoveInputChangeEvent(event:TMTBBoardChangeEvent; board:Integer = -1);
var i:Integer;
begin
 if (board = -1) then
  begin
   for i := 0 to _MAX_MTB-1 do
     Self.Desky[i].inputChangedEv.Remove(event);
  end else begin
   if ((board >= 0) and (board < _MAX_MTB)) then
     Self.Desky[board].inputChangedEv.Remove(event);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTB.AddOutputChangeEvent(board:Integer; event:TMTBBoardChangeEvent);
begin
 if ((board >= 0) and (board < _MAX_MTB)) then
   if (Self.Desky[board].outputChangedEv.IndexOf(event) = -1) then Self.Desky[board].outputChangedEv.Add(event);
end;

procedure TMTB.RemoveOutputChangeEvent(event:TMTBBoardChangeEvent; board:Integer = -1);
var i:Integer;
begin
 if (board = -1) then
  begin
   for i := 0 to _MAX_MTB-1 do
     Self.Desky[i].outputChangedEv.Remove(event);
  end else begin
   if ((board >= 0) and (board < _MAX_MTB)) then
     Self.Desky[board].outputChangedEv.Remove(event);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
