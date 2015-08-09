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
  TODProc = procedure(); stdcall;
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
  TStdNotifyEvent = procedure (Sender: TObject; data:Pointer); stdcall;
  TStdDllErrEvent = procedure (Sender: TObject; data:Pointer; errValue: word; errAddr: byte; errMsg:string); stdcall;
  TStdDllModuleChanedEvent = procedure (Sender:TObject; data:Pointer; module:byte); stdcall;

  // prototypy setteru (funkci naastavujicich callback funkce) z TMTBIFace do dll knihovny:
  TSetDllNotifyEvent = procedure(func:TStdNotifyEvent; data:Pointer); stdcall;
  TSetDllErrEvent = procedure(func:TStdDllErrEvent; data:Pointer); stdcall;
  TSetDllEventChange = procedure (func:TStdDllModuleChanedEvent; data:Pointer); stdcall;

  // vlastni vyjimky:
  EFuncNotAssigned = class(Exception);

  //////////////////////////////////////////////////////////////////////////

  // Trida TMTBIFace
  TMTBIFace = class(TComponent)
  private
    FLibname: string;                                                           // cesta k souboru knihovny
    FLib: Cardinal;                                                             // handle knihovny

    // funkce volane z TMTBIFace do knihovny:
    FFuncOnUnload: TODProc;
    FFuncSetOutput: TODSetOutput;
    FFuncGetInput: TODGetInput;
    FFuncGetOutput: TODGetOutput;
    FFuncSetInput: TODSetInput;
    FFuncShowConfigDialog: TODProc;
    FFuncHideConfigDialog: TODProc;
    FFuncShowAboutDialog: TODProc;
    FFuncStart: TODProc;
    FFuncStop: TODProc;
    FFuncGetLibVersion: TODFuncStr;
    FFuncGetDeviceVersion: TODFuncStr;
    FFuncGetDriverVersion: TODFuncStr;
    FFuncSetBusSpeed: TODSetBusSpeed;
    FFuncModuleExists: TODModuleExists;
    FFuncGetModuleType: TODModuleStr;
    FFuncGetModuleName: TODModuleStr;
    FFuncSetModuleName: TODSetModuleName;
    FFuncSetScanInterval: TODSetScanInterval;
    FFuncOpen: TODProc;
    FFuncClose: TODProc;
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

procedure OnLibBeforeOpen(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnBeforeOpen)) then TMTBIFace(data).OnBeforeOpen(TMTBIFace(data));
 end;

procedure OnLibAfterOpen(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnAfterOpen)) then TMTBIFace(data).OnAfterOpen(TMTBIFace(data));
 end;

procedure OnLibBeforeClose(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnBeforeClose)) then TMTBIFace(data).OnBeforeClose(TMTBIFace(data));
 end;

procedure OnLibAfterClose(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnAfterClose)) then TMTBIFace(data).OnAfterClose(TMTBIFace(data));
 end;

procedure OnLibBeforeStart(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnBeforeStart)) then TMTBIFace(data).OnBeforeStart(TMTBIFace(data));
 end;

procedure OnLibAfterStart(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnAfterStart)) then TMTBIFace(data).OnAfterStart(TMTBIFace(data));
 end;

procedure OnLibBeforeStop(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnBeforeStop)) then TMTBIFace(data).OnBeforeStop(TMTBIFace(data));
 end;

procedure OnLibAfterStop(Sender:TObject; data:Pointer); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnAfterStop)) then TMTBIFace(data).OnAfterStop(TMTBIFace(data));
 end;

procedure OnLibError(Sender: TObject; data:Pointer; errValue: word; errAddr: byte; errMsg:string); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnError)) then TMTBIFace(data).OnError(TMTBIFace(data), errValue, errAddr, errMsg);
 end;

procedure OnLibInputChanged(Sender: TObject; data:Pointer; board:byte); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnInputChanged)) then TMTBIFace(data).OnInputChanged(TMTBIFace(data), board);
 end;

procedure OnLibOutputChanged(Sender: TObject; data:Pointer; board:byte); stdcall;
 begin
  if (Assigned(TMTBIFace(data).OnOutputChanged)) then TMTBIFace(data).OnOutputChanged(TMTBIFace(data), board);
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
  FFuncShowConfigDialog   := GetProcAddress(FLib, 'showconfigdialog');
  FFuncHideConfigDialog   := GetProcAddress(FLib, 'hideconfigdialog');
  FFuncShowAboutDialog    := GetProcAddress(FLib, 'showaboutdialog');
  FFuncStart              := GetProcAddress(FLib, 'start');
  FFuncStop               := GetProcAddress(FLib, 'stop');
  FFuncGetLibVersion      := GetProcAddress(FLib, 'getlibversion');
  FFuncGetDeviceVersion   := GetProcAddress(FLib, 'getdeviceversion');
  FFuncGetDriverVersion   := GetProcAddress(FLib, 'getdriverversion');
  FFuncGetModuleFirmware  := GetProcAddress(FLib, 'getmodulefirmware');
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
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeOpen, self);
  @setterNotify := GetProcAddress(FLib, 'setafteropen');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterOpen, self);
  @setterNotify := GetProcAddress(FLib, 'setbeforeclose');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeClose, self);
  @setterNotify := GetProcAddress(FLib, 'setafterclose');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterClose, self);

  @setterNotify := GetProcAddress(FLib, 'setbeforestart');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeStart, self);
  @setterNotify := GetProcAddress(FLib, 'setafterstart');
  if (Assigned(setterNotify)) then setterNotify(OnLibAfterStart, self);
  @setterNotify := GetProcAddress(FLib, 'setbeforestop');
  if (Assigned(setterNotify)) then setterNotify(OnLibBeforeStop, self);
  @setterNotify := GetProcAddress(FLib, 'setafterstop');
  if (Assigned(setterNotify)) then setterNotify(OnLibafterStop, self);

  @setterErr := GetProcAddress(FLib, 'setonerror');
  if (Assigned(setterErr)) then setterErr(OnLibError, self);

  @setterModuleChanged := GetProcAddress(FLib, 'setoninputchange');
  if (Assigned(setterModuleChanged)) then setterModuleChanged(OnLibInputChanged, self);
  @setterModuleChanged := GetProcAddress(FLib, 'setonoutputchange');
  if (Assigned(setterModuleChanged)) then setterModuleChanged(OnLibOutputChanged, self);
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

