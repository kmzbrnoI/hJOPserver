////////////////////////////////////////////////////////////////////////////////
// OutputDriver.pas
// hJOPserver
// TMTBIFace class definition
// (c) Jan Horacek, Michal Petrilak
// jan.horacek@kmz-brno.cz, engineercz@gmail.com
// 01.08.2015
// license: Apache license v2.0
////////////////////////////////////////////////////////////////////////////////

{
 Trida TMTBIFace umoznuje nacist knihovnu pro komunikaci s MTB
 a jednoduse ji pouzivat volanim funkci do teto tridy.

 POZOR:
  Pri volani metod tridy TMTBIFace je nutne osetrovat vyjimky
  AccessViolation a NullPointerException, protoze mohou nastat vlivem
  nenamapovane funkce v knihovne.
}

unit OutputDriver;

interface

uses
  SysUtils, Classes, Windows;

type
  // prototypy funkci volanych do knihovny :
  TODFunc = function(): integer; stdcall;
  TODFuncStr = function(): string; stdcall;
  TODModuleStr = function(board:integer): string; stdcall;
  TODSetOutput = function(board, output: integer; state: Integer): integer; stdcall;
  TODGetInput  = function(board, input: integer): integer; stdcall;
  TODGetOutput = function(board, port: integer): integer; stdcall;
  TODSetInput  = function(board, input: integer; State:integer): integer; stdcall;
  TODModuleExists = function(board:integer):boolean; stdcall;
  TODSetModuleName = function(board:Integer;Name:string):Integer; stdcall;
  TODSetBusSpeed = function(Speed:Integer): integer; stdcall;
  TODSetScanInterval = function(Interval:integer):Integer; stdcall;

  // prototypy eventu z TMTBIFace do rodice:
  TDllErrEvent = procedure (Sender: TObject; errValue: word; errAddr: byte; errMsg:string) of object;
  TDllModuleChanedEvent = procedure (Sender:TObject; module:byte) of object;

  // prototypy callback funkci z knihivny do TMTBIFace:
  TStdNotifyEvent = procedure (Sender: TObject) of object; stdcall;
  TStdDllErrEvent = procedure (Sender: TObject; errValue: word; errAddr: byte; errMsg:string) of object; stdcall;
  TStdDllModuleChanedEvent = procedure (Sender:TObject; module:byte) of object; stdcall;

  // prototypy setteru (funkci naastavujicich callback funkce) z TMTBIFace do dll knihovny:
  TSetDllNotifyEvent = procedure(func:TStdNotifyEvent); stdcall;
  TSetDllErrEvent = function(func:TStdDllErrEvent):Integer; stdcall;
  TSetDllEventChange = procedure (func:TStdDllModuleChanedEvent); stdcall;

  // vlastni vyjimky:
  EFuncNotAssigned = class(Exception);

  //////////////////////////////////////////////////////////////////////////

  // Trida TMTBIFace
  TMTBIFace = class(TComponent)
  private
    FLibname: string;                                                           // cesta k souboru knihovny
    FLib: Cardinal;                                                             // handle knihovny

    // funkce volane z TMTBIFace do knihovny:
    FFuncOnUnload: TODFunc;
    FFuncSetOutput: TODSetOutput;
    FFuncGetInput: TODGetInput;
    FFuncGetOutput: TODGetOutput;
    FFuncSetInput: TODSetInput;
    FFuncGetInfo: TODFunc;
    FFuncShowConfigDialog: TODFunc;
    FFuncHideConfigDialog: TODFunc;
    FFuncShowAboutDialog: TODFunc;
    FFuncStart: TODFunc;
    FFuncStop: TODFunc;
    FFuncGetLibVersion: TODFuncStr;
    FFuncGetDeviceVersion: TODFuncStr;
    FFuncGetDriverVersion: TODFuncStr;
    FFuncSetBusSpeed: TODSetBusSpeed;
    FFuncModuleExists: TODModuleExists;
    FFuncGetModuleType: TODModuleStr;
    FFuncGetModuleName: TODModuleStr;
    FFuncSetModuleName: TODSetModuleName;
    FFuncSetScanInterval: TODSetScanInterval;
    FFuncOpen: TODFunc;
    FFuncClose: TODFunc;
    FFuncGetModuleFirmware: TODModuleStr;

    // eventy z TMTBIFace do rodice:
    FTBeforeOpen: TNotifyEvent;
    FTAfterOpen: TNotifyEvent;
    FTBeforeClose: TNotifyEvent;
    FTAfterClose: TNotifyEvent;

    FTBeforeStart: TNotifyEvent;
    FTAfterStart: TNotifyEvent;
    FTBeforeStop: TNotifyEvent;
    FTAfterStop: TNotifyEvent;

    FTOnError:TDllErrEvent;
    FTInputChanged:TDllModuleChanedEvent;
    FTOutputChanged:TDllModuleChanedEvent;

     procedure SetLibName(s: string);

     // eventy z dll do TMTBIFace:
     procedure OnLibBeforeOpen(Sender:TObject); stdcall;
     procedure OnLibAfterOpen(Sender:TObject); stdcall;
     procedure OnLibBeforeClose(Sender:TObject); stdcall;
     procedure OnLibAfterClose(Sender:TObject); stdcall;
     procedure OnLibBeforeStart(Sender:TObject); stdcall;
     procedure OnLibAfterStart(Sender:TObject); stdcall;
     procedure OnLibBeforeStop(Sender:TObject); stdcall;
     procedure OnLibAfterStop(Sender:TObject); stdcall;
     procedure OnLibError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string); stdcall;
     procedure OnLibInputChanged(Sender: TObject; board:byte); stdcall;
     procedure OnLibOutputChanged(Sender: TObject; board:byte); stdcall;

  protected
    { Protected declarations }

  public

    procedure Open();                                                           // otevrit zarizeni
    procedure Close();                                                          // uzavrit zarizeni

    procedure Start();                                                          // spustit komunikaci
    procedure Stop();                                                           // zastavit komunikaci

    procedure SetOutput(Board, Output: Integer; state: Integer);                // nastavit vystupni port
    function GetInput(Board, Input: Integer): Integer;                          // vratit hodnotu na vstupnim portu
    procedure SetInput(Board, Input: Integer; State : integer);                 // nastavit vstupni port (pro debug ucely)
    function GetOutput(Board, Port:Integer):Integer;                            // ziskani stavu vystupu

    procedure ShowConfigDialog();                                               // zobrazit konfiguracni dialog knihovny
    procedure HideConfigDialog();                                               // skryt konfiguracni dialog knihovny
    procedure ShowAboutDialog();                                                // zobrazit about dialog knihvny

    function GetLibVersion():string;                                            // vrati verzi knihovny
    function GetDeviceVersion():string;                                         // vrati verzi FW v MTB-USB desce
    function GetDriverVersion():string;                                         // vrati verzi MTBdrv drivery v knihovne

    function GetModuleName(Module:Integer):string;                              // vrati jmeno modulu
    procedure SetModuleName(Module:Integer; Name:string);                       // nastavi jmeno modulu

    function GetModuleExists(Module:Integer):boolean;                           // vrati jestli modul existuje
    function GetModuleType(Module:Integer):string;                              // vrati typ modulu
    function GetModuleFirmware(Module:integer):String;                          // vrati verzi FW v modulu

    procedure SetBusSpeed(Speed:Integer);                                       // nastavi rychlost sbernice, mozno volat pouze pri zastavene komunikaci
    procedure SetScanInterval(Interval:integer);                                // nastavi ScanInterval sbernice

    procedure LoadLib();                                                        // nacte knihovnu

    // eventy z TMTBIFace do rodice:
    property OnBeforeOpen:TNotifyEvent read FTbeforeOpen write FTBeforeOpen;
    property OnAfterOpen:TNotifyEvent read FTAfterOpen write FTAfterOpen;
    property OnBeforeClose:TNotifyEvent read FTbeforeClose write FTBeforeClose;
    property OnAfterClose:TNotifyEvent read FTAfterClose write FTAfterClose;

    property OnBeforeStart:TNotifyEvent read FTbeforeStart write FTBeforeStart;
    property OnAfterStart:TNotifyEvent read FTAfterStart write FTAfterStart;
    property OnBeforeStop:TNotifyEvent read FTbeforeStop write FTBeforeStop;
    property OnAfterStop:TNotifyEvent read FTAfterStop write FTAfterStop;

    property OnError:TDllErrEvent read FTOnError write FTOnError;
    property OnInputChanged:TDllModuleChanedEvent read FTInputChanged write FTInputChanged;
    property OnOutputChanged:TDllModuleChanedEvent read FTOutputChanged write FTOutputChanged;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property Lib: string read FLibname write SetLibName;
  end;


procedure Register;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TMTBIFace.Create;
 begin
  inherited;
 end;

destructor TMTBIFace.Destroy;
 begin
  if Assigned(FFuncOnUnload) then FFuncOnUnload();
  inherited;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TMTBIFace.SetLibName(s: string);
 begin
  if FileExists(s) then
    FLibname := s
  else
    raise Exception.Create('Library '+s+' not found');
 end;

////////////////////////////////////////////////////////////////////////////////
// eventy z dll knihovny:

procedure TMTBIFace.OnLibBeforeOpen(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnBeforeOpen)) then Self.OnBeforeOpen(Self);
 end;

procedure TMTBIFace.OnLibAfterOpen(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnAfterOpen)) then Self.OnAfterOpen(Self);
 end;

procedure TMTBIFace.OnLibBeforeClose(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnBeforeClose)) then Self.OnBeforeClose(Self);
 end;

procedure TMTBIFace.OnLibAfterClose(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnAfterClose)) then Self.OnAfterClose(Self);
 end;

procedure TMTBIFace.OnLibBeforeStart(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnBeforeStart)) then Self.OnBeforeStart(Self);
 end;

procedure TMTBIFace.OnLibAfterStart(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnAfterStart)) then Self.OnAfterStart(Self);
 end;

procedure TMTBIFace.OnLibBeforeStop(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnBeforeStop)) then Self.OnBeforeStop(Self);
 end;

procedure TMTBIFace.OnLibAfterStop(Sender:TObject); stdcall;
 begin
  if (Assigned(Self.OnAfterStop)) then Self.OnAfterStop(Self);
 end;

procedure TMTBIFace.OnLibError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string); stdcall;
 begin
  if (Assigned(Self.OnError)) then Self.OnLibError(Self, errValue, errAddr, errMsg);
 end;

procedure TMTBIFace.OnLibInputChanged(Sender: TObject; board:byte); stdcall;
 begin
  if (Assigned(Self.OnInputChanged)) then Self.OnInputChanged(Self, board);
 end;

procedure TMTBIFace.OnLibOutputChanged(Sender: TObject; board:byte); stdcall;
 begin
  if (Assigned(Self.OnOutputChanged)) then Self.OnOutputChanged(Self, board);
 end;

////////////////////////////////////////////////////////////////////////////////
// nacist dll knihovnu

procedure TMTBIFace.LoadLib();
var setterNotify: TSetDllNotifyEvent;
    setterErr: TSetDllErrEvent;
    setterModuleChanged: TSetDllEventChange;
 begin
  FLib := LoadLibrary(PChar(FLibname));
  if (FLib = 0) then
    raise Exception.Create('Library not loaded');

  FFuncOnUnload           := GetProcAddress(FLib, 'onunload');
  FFuncSetOutput          := GetProcAddress(FLib, 'setoutput');
  FFuncSetInput           := GetProcAddress(FLib, 'setinput');
  FFuncGetInput           := GetProcAddress(FLib, 'getinput');
  FFuncGetOutput          := GetProcAddress(FLib, 'getoutput');
  FFuncGetInfo            := GetProcAddress(FLib, 'getinfo');
  FFuncShowConfigDialog   := GetProcAddress(FLib, 'showconfigdialog');
  FFuncHideConfigDialog   := GetProcAddress(FLib, 'hideconfigdialog');
  FFuncShowAboutDialog    := GetProcAddress(FLib, 'showaboutdialog');
  FFuncStart              := GetProcAddress(FLib, 'start');
  FFuncStop               := GetProcAddress(FLib, 'stop');
  FFuncGetLibVersion      := GetProcAddress(FLib, 'getlibversion');
  FFuncGetDeviceVersion   := GetProcAddress(FLib, 'getdeviceversion');
  FFuncGetDriverVersion   := GetProcAddress(FLib,'getdriverversion');
  FFuncGetModuleFirmware  := GetProcAddress(FLib,'getmodulefirmware');
  FFuncModuleExists       := GetProcAddress(FLib, 'getmoduleexists');
  FFuncGetModuleType      := GetProcAddress(FLib, 'getmoduletype');
  FFuncGetModuleName      := GetProcAddress(FLib, 'getmodulename');
  FFuncSetModuleName      := GetProcAddress(FLib, 'setmodulename');
  FFuncSetBusSpeed        := GetProcAddress(FLib, 'setmtbspeed');
  FFuncSetScanInterval    := GetProcAddress(FLib, 'settimerinterval');
  FFuncOpen               := GetProcAddress(FLib, 'open');
  FFuncClose              := GetProcAddress(FLib, 'close');

  // assign events:
  setterNotify := GetProcAddress(FLib, 'setbeforeopen');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeOpen);
  @setterNotify := GetProcAddress(FLib, 'setafteropen');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterOpen);
  @setterNotify := GetProcAddress(FLib, 'setbeforeclose');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeClose);
  @setterNotify := GetProcAddress(FLib, 'setafterclose');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterClose);

  @setterNotify := GetProcAddress(FLib, 'setbeforestart');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeStart);
  @setterNotify := GetProcAddress(FLib, 'setafterstart');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterStart);
  @setterNotify := GetProcAddress(FLib, 'setbeforestop');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeStop);
  @setterNotify := GetProcAddress(FLib, 'setafterstop');
  if (Assigned(setterNotify)) then setterNotify(OnLibafterStop);

  @setterErr := GetProcAddress(FLib, 'setonerror');
  if (Assigned(setterErr)) then setterErr(OnLibError);

  @setterModuleChanged := GetProcAddress(FLib, 'setoninputchange');
  if (Assigned(setterModuleChanged)) then setterModuleChanged(OnLibInputChanged);
  @setterModuleChanged := GetProcAddress(FLib, 'setonoutputchange');
  if (Assigned(setterModuleChanged)) then setterModuleChanged(OnLibOutputChanged);
 end;

////////////////////////////////////////////////////////////////////////////////
// metody volane do knihovny:

procedure TMTBIFace.ShowAboutDialog();
 begin
  if (Assigned(FFuncShowAboutDialog)) then
    FFuncShowAboutDialog()
  else
    raise EFuncNotAssigned.Create('FFuncShowAboutDialog not assigned');
 end;

procedure TMTBIFace.ShowConfigDialog();
 begin
  if (Assigned(FFuncShowConfigDialog)) then
    FFuncShowConfigDialog()
  else
    raise EFuncNotAssigned.Create('FFuncShowConfigDialog not assigned');
 end;

procedure TMTBIFace.HideConfigDialog();
 begin
  if (Assigned(FFuncHideConfigDialog)) then
    FFuncHideConfigDialog()
  else
    raise EFuncNotAssigned.Create('FFuncHideConfigDialog not assigned');
 end;

function TMTBIFace.GetInput(Board, Input: Integer): Integer;
 begin
  if (Assigned(FFuncGetInput)) then
    Result := FFuncGetInput(Board, Input)
  else
    raise EFuncNotAssigned.Create('FFuncGetInput not assigned');
 end;

procedure TMTBIFace.SetOutput(Board, Output: Integer; state: Integer);
 begin
  if (Assigned(FFuncSetOutput)) then
    FFuncSetOutput(Board, Output, state)
  else
    raise EFuncNotAssigned.Create('FFuncSetOutput not assigned');
 end;

procedure TMTBIFace.SetInput(Board, Input: Integer; state: Integer);
 begin
  if (Assigned(FFuncSetInput)) then
    FFuncSetInput(Board, Input, state)
  else
    raise EFuncNotAssigned.Create('FFuncSetInput not assigned');
 end;

function TMTBIFace.GetOutput(Board, Port:Integer):Integer;
begin
  if (Assigned(FFuncGetOutput)) then
    Result := FFuncGetOutput(Board, Port)
  else
    raise EFuncNotAssigned.Create('FFuncGetOutput not assigned');
end;

function TMTBIFace.GetLibVersion():String;
 begin
  if (Assigned(FFuncGetLibVersion)) then
    Result := FFuncGetLibVersion()
  else
    raise EFuncNotAssigned.Create('FFuncGetLibVersion not assigned');
 end;

function TMTBIFace.GetDriverVersion():String;
 begin
  if (Assigned(FFuncGetDriverVersion)) then
    Result := FFuncGetDriverVersion()
  else
    raise EFuncNotAssigned.Create('FFuncGetDriverVersion not assigned');
 end;

function TMTBIFace.GetDeviceVersion():String;
 begin
  if (Assigned(FFuncGetDeviceVersion)) then
    Result := FFuncGetDeviceVersion()
  else
    raise EFuncNotAssigned.Create('FFuncGetDeviceVersion not assigned');
 end;

function TMTBIFace.GetModuleExists(Module:Integer):boolean;
 begin
  if (Assigned(FFuncModuleExists)) then
    Result := FFuncModuleExists(Module)
  else
    raise EFuncNotAssigned.Create('FFuncModuleExists not assigned');
 end;

function TMTBIFace.GetModuleType(Module:Integer):string;
 begin
  if (Assigned(FFuncGetModuleType)) then
    Result := FFuncGetModuleType(Module)
  else
    raise EFuncNotAssigned.Create('FFuncGetModuleType not assigned');
 end;

function TMTBIFace.GetModuleName(Module:Integer):string;
 begin
  if (Assigned(FFuncGetModuleName)) then
    Result := FFuncGetModuleName(Module)
  else
    raise EFuncNotAssigned.Create('FFuncGetModuleName not assigned');
 end;

procedure TMTBIFace.SetModuleName(Module:Integer;Name:string);
 begin
  if (Assigned(FFuncSetModuleName)) then
    FFuncSetModuleName(Module, Name)
  else
    raise EFuncNotAssigned.Create('FFuncSetModuleName not assigned');
 end;

procedure TMTBIFace.SetBusSpeed(Speed:Integer);
 begin
  if (Assigned(FFuncSetBusSpeed)) then
    FFuncSetBusSpeed(Speed)
  else
    raise EFuncNotAssigned.Create('FFuncSetBusSpeed not assigned');
 end;

procedure TMTBIFace.Open();
 begin
  if (Assigned(FFuncOpen)) then
    FFuncOpen()
  else
    raise EFuncNotAssigned.Create('FFuncOpen not assigned');
 end;

procedure TMTBIFace.Close();
 begin
  if (Assigned(FFuncClose)) then
    FFuncClose()
  else
    raise EFuncNotAssigned.Create('FFuncClose not assigned');
 end;

procedure TMTBIFace.Start();
begin
  if (Assigned(FFuncStart)) then
    FFuncStart()
  else
    raise EFuncNotAssigned.Create('FFuncStart not assigned');
end;

procedure TMTBIFace.Stop();
begin
  if (Assigned(FFuncStop)) then
    FFuncStop()
  else
    raise EFuncNotAssigned.Create('FFuncStop not assigned');
end;

function TMTBIFace.GetModuleFirmware(Module:integer):string;
 begin
  if (Assigned(FFuncGetModuleFirmware)) then
    Result := FFuncGetModuleFirmware(Module)
  else
    raise EFuncNotAssigned.Create('FFuncGetModuleFirmware not assigned');
 end;

procedure TMTBIFace.SetScanInterval(Interval:integer);
 begin
  if (Assigned(FFuncSetScanInterval)) then
    FFuncSetScanInterval(Interval)
  else
    raise EFuncNotAssigned.Create('FFuncSetScanInterval not assigned');
 end;

////////////////////////////////////////////////////////////////////////////////

procedure Register;
 begin
  RegisterComponents('KMZ', [TMTBIFace]);
 end;

end.//unit

