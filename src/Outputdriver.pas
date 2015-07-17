////////////////////////////////////////////////////////////////////////////////
// OutputDriver.pas
// hJOPserver
// TOutputDriver class definition
// (c) Jan Horacek, Michal Petrilak
// jan.horacek@kmz-brno.cz, engineercz@gmail.com
// 17.02.2015
// license: Apache license v2.0
////////////////////////////////////////////////////////////////////////////////

// Trida TOutputDriver umoznuje nacist knihovnu pro komunikaci s MTB a
//  jednoduse ji pouzivat volanim funkci do teto tridy.

// OutputDriver umoznuje nacist az 3 komunikacni knihovny zaroven a
//  pracovat s nimi paralerne.

unit OutputDriver;

interface

uses
  SysUtils, Classes, Windows;

type
  // Event prototypes definitions
  TODFunc = function(): integer; stdcall;
  TODFuncStr = function(): string; stdcall;
  TODModuleStr = function(board:integer): string; stdcall;

  // Library functions definition
  TODSetOutput = function(board, output: integer; state: Integer): integer; stdcall;
  TODGetInput  = function(board, input: integer): integer; stdcall;
  TODSetInput  = function(board, input: integer; State:integer): integer; stdcall;
  TODModuleExists = function(board:integer):boolean; stdcall;
  TODSetModuleName = function(board:Integer;Name:string):Integer; stdcall;
  TODSetBusSpeed = function(Speed:Integer): integer; stdcall;
  TODSetScanInterval = function(Interval:integer):Integer; stdcall;

  // Events from OutputDriver to parent
  TDllMainEvent = procedure (Sender:TObject) of object;
  PTDllMainEvent = ^TDllMainEvent;

  TDllErrEvent = procedure (Sender: TObject; errValue: word; errAddr: byte; errMsg:string) of object;
  PTDllErrEvent  = ^TDllErrEvent;

  // Setting events
  TSetDllEvent = function(func:PTDllMainEvent):Integer; stdcall;
  TSetDllErrEvent = function(func:PTDllErrEvent):Integer; stdcall;


  // TOuputDriver class
  TOutputDriver = class(TComponent)
  private
    FLibnames: Array[0..2] of string;
    FLibs: Array[0..2] of Cardinal;

    FFuncOnLoad: Array[0..2] of TODFunc;
    FFuncOnUnload: Array[0..2] of TODFunc;
    FFuncSetOutput: Array[0..2] of TODSetOutput;
    FFuncGetInput: Array[0..2] of TODGetInput;
    FFuncSetInput: Array[0..2] of TODSetInput;
    FFuncGetInfo: Array[0..2] of TODFunc;
    FFuncShowConfigDialog: Array[0..2] of TODFunc;
    FFuncHideConfigDialog: Array[0..2] of TODFunc;
    FFuncShowAboutDialog: Array[0..2] of TODFunc;
    FFuncStart: Array[0..2] of TODFunc;
    FFuncStop: Array[0..2] of TODFunc;
    FFuncGetLibVersion: Array[0..2] of TODFuncStr;
    FFuncGetDeviceVersion: Array[0..2] of TODFuncStr;
    FFuncGetDriverVersion: Array[0..2] of TODFuncStr;
    FFuncSetBusSpeed: Array[0..2] of TODSetBusSpeed;
    FFuncSaveData: Array[0..2] of TODFunc;
    FFuncModuleExists:Array[0..2] of TODModuleExists;
    FFuncGetModuleType:Array[0..2] of TODModuleStr;
    FFuncGetModuleName:Array[0..2] of TODModuleStr;
    FFuncSetModuleName:Array[0..2] of TODSetModuleName;
    FFuncSetScanInterval:Array[0..2] of TODSetScanInterval;
    FFuncOpen:Array[0..2] of TODFunc;
    FFuncClose:Array[0..2] of TODFunc;
    FFuncGetModuleFirmware:Array[0..2] of TODModuleStr;

    //-- events begin --
    FFuncSetBeforeOpenEv:Array[0..2] of TSetDllEvent;
    FFuncSetAfterOpenEv:Array[0..2] of TSetDllEvent;
    FFuncSetBeforeCloseEv:Array[0..2] of TSetDllEvent;
    FFuncSetAfterCloseEv:Array[0..2] of TSetDllEvent;

    FFuncSetBeforeStartEv:Array[0..2] of TSetDllEvent;
    FFuncSetAfterStartEv:Array[0..2] of TSetDllEvent;
    FFuncSetBeforeStopEv:Array[0..2] of TSetDllEvent;
    FFuncSetAfterStopEv:Array[0..2] of TSetDllEvent;

    FFuncSetOnErrorEv:Array[0..2] of TSetDllErrEvent;
    //-- events end --

    //events to the parent
    FTBeforeOpen:TDllMainEvent;
    FTAfterOpen:TDllMainEvent;
    FTBeforeClose:TDllMainEvent;
    FTAfterClose:TDllMainEvent;

    FTBeforeStart:TDllMainEvent;
    FTAfterStart:TDllMainEvent;
    FTBeforeStop:TDllMainEvent;
    FTAfterStop:TDllMainEvent;

    FTOnError:TDllErrEvent;

    procedure SetLibName1(s: string);
    procedure SetLibName2(s: string);
    procedure SetLibName3(s: string);
  protected
    { Protected declarations }

  public

    function Open(bus:integer):Integer;
    function Close(bus:integer):Integer;

    function Start(Bus: Integer): Integer;
    function Stop(Bus: Integer): Integer;

    function SetOutput(Bus, Board, Output: Integer; state: Integer): Integer;
    function GetInput(Bus, Board, Input: Integer): Integer;
    function SetInput(Bus, Board, Input: Integer; State : integer): Integer;

    function ShowConfigDialog(Bus: Integer): Integer;
    function HideConfigDialog(Bus: Integer): Integer;
    function ShowAboutDialog(Bus: Integer): Integer;

    function GetLibVersion(Bus: Integer):string;
    function GetDeviceVersion(Bus: Integer):string;
    function GetDriverVersion(Bus: Integer):string;

    function GetModuleName(Bus:Integer;Module:Integer):string;
    function SetModuleName(Bus:Integer;Module:Integer;Name:string):Integer;

    function GetModuleExists(Bus:Integer;Module:Integer):boolean;
    function GetModuleType(Bus:Integer;Module:Integer):string;
    function GetModuleFirmware(Bus:integer;Module:integer):String;

    function SetBusSpeed(Bus:integer;Speed:Integer):Integer;
    function SaveData(Bus:Integer):Integer;
    function SetScanInterval(Bus:integer;Interval:integer):integer;

    function LoadLib():Integer;

    //events to the program
    property OnBeforeOpen:TDllMainEvent read FTbeforeOpen write FTBeforeOpen;
    property OnAfterOpen:TDllMainEvent read FTAfterOpen write FTAfterOpen;
    property OnBeforeClose:TDllMainEvent read FTbeforeClose write FTBeforeClose;
    property OnAfterClose:TDllMainEvent read FTAfterClose write FTAfterClose;

    property OnBeforeStart:TDllMainEvent read FTbeforeStart write FTBeforeStart;
    property OnAfterStart:TDllMainEvent read FTAfterStart write FTAfterStart;
    property OnBeforeStop:TDllMainEvent read FTbeforeStop write FTBeforeStop;
    property OnAfterStop:TDllMainEvent read FTAfterStop write FTAfterStop;

    property OnError:TDllErrEvent read FTOnError write FTOnError;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property Lib1: string read FLibnames[0] write SetLibName1;
    property Lib2: string read FLibnames[1] write SetLibName2;
    property Lib3: string read FLibnames[2] write SetLibName3;
  end;

  //bacause of callback functions (this is delphi magic)
  PTOutputDriver = ^TOutputDriver;

var
  SelfInstance:TOutputDriver;   // yes, this is delphi magic
                                // we assume, there is just one instance of TOutputDriver and remeber it (vecause of callback functions)

procedure Register;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TOutputDriver.Create;
begin
  inherited;
  SelfInstance := Self;
end;

destructor TOutputDriver.Destroy;
begin
  if Assigned(FFuncOnUnload[0]) then FFuncOnUnload[0]();
  if Assigned(FFuncOnUnload[1]) then FFuncOnUnload[1]();
  if Assigned(FFuncOnUnload[2]) then FFuncOnUnload[2]();

  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TOutputDriver.SetLibName1(s: string);
begin
  if FileExists(s) then FLibnames[0] := s;
end;

procedure TOutputDriver.SetLibName2(s: string);
begin
  if FileExists(s) then FLibnames[1] := s;
end;

procedure TOutputDriver.SetLibName3(s: string);
begin
  if FileExists(s) then FLibnames[2] := s;
end;

////////////////////////////////////////////////////////////////////////////////

function TOutputDriver.Start(Bus: Integer): Integer;
begin
  if Assigned(FFuncStart[Bus]) then Result := FFuncStart[Bus] else Result := -1;
end;

function TOutputDriver.Stop(Bus: Integer): Integer;
begin
  if Assigned(FFuncStop[Bus]) then Result := FFuncStop[Bus] else Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////

//----- events from dll begin -----
function PrgBeforeOpen(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTBeforeOpen)) then SelfInstance.FTBeforeOpen(SelfInstance);
 Result := 0;
end;//function

function PrgAfterOpen(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTAfterOpen)) then SelfInstance.FTAfterOpen(SelfInstance);
 Result := 0;
end;//function

function PrgBeforeClose(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTBeforeClose)) then SelfInstance.FTBeforeClose(SelfInstance);
 Result := 0;
end;//function

function PrgAfterClose(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTAfterClose)) then SelfInstance.FTAfterClose(SelfInstance);
 Result := 0;
end;//function

function PrgBeforeStart(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTBeforeStart)) then SelfInstance.FTBeforeStart(SelfInstance);
 Result := 0;
end;//function

function PrgAfterStart(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTAfterStart)) then SelfInstance.FTAfterStart(SelfInstance);
 Result := 0;
end;//function

function PrgBeforeStop(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTBeforeStop)) then SelfInstance.FTBeforeStop(SelfInstance);
 Result := 0;
end;//function

function PrgAfterStop(Sender:TObject):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTAfterStop)) then SelfInstance.FTAfterStop(SelfInstance);
 Result := 0;
end;//function

function PrgOnError(Sender: TObject; errValue: word; errAddr: byte; errMsg:string):Integer; stdcall;
begin
 if (Assigned(SelfInstance.FTOnError)) then SelfInstance.FTOnError(SelfInstance, errValue, errAddr, errMsg);
 Result := 0;
end;//function

//----- events from dll end -----

////////////////////////////////////////////////////////////////////////////////

// returns error code, otherways zero
function TOutputDriver.LoadLib:Integer;
var
  i: Integer;
begin
  Result := 0;

  FLibs[0] :=  LoadLibrary(PChar(FLibnames[0]));
  FLibs[1] :=  LoadLibrary(PChar(FLibnames[1]));
  FLibs[2] :=  LoadLibrary(PChar(FLibnames[2]));

  for i := 0 to 2 do begin
    if (FLibs[i] <> 0) then begin
      FFuncOnLoad[i] := GetProcAddress(FLibs[i], 'onload');
      FFuncOnUnload[i] := GetProcAddress(FLibs[i], 'onunload');
      FFuncSetOutput[i] := GetProcAddress(FLibs[i], 'setoutput');
      FFuncSetInput[i] := GetProcAddress(FLibs[i], 'setinput');
      FFuncGetInput[i] := GetProcAddress(FLibs[i], 'getinput');
      FFuncGetInfo[i] := GetProcAddress(FLibs[i], 'getinfo');
      FFuncShowConfigDialog[i] := GetProcAddress(FLibs[i], 'showconfigdialog');
      FFuncHideConfigDialog[i] := GetProcAddress(FLibs[i], 'hideconfigdialog');
      FFuncShowAboutDialog[i] := GetProcAddress(FLibs[i], 'showaboutdialog');
      FFuncStart[i] := GetProcAddress(FLibs[i], 'start');
      FFuncStop[i] := GetProcAddress(FLibs[i], 'stop');
      FFuncGetLibVersion[i] := GetProcAddress(FLibs[i], 'getlibversion');
      FFuncGetDeviceVersion[i] := GetProcAddress(FLibs[i], 'getdeviceversion');
      FFuncGetDriverVersion[i] := GetProcAddress(FLibs[i],'getdriverversion');
      FFuncGetModuleFirmware[i] := GetProcAddress(FLibs[i],'getmodulefirmware');
      FFuncModuleExists[i] := GetProcAddress(FLibs[i], 'getmoduleexists');
      FFuncGetModuleType[i] := GetProcAddress(FLibs[i], 'getmoduletype');
      FFuncGetModuleName[i] :=  GetProcAddress(FLibs[i], 'getmodulename');
      FFuncSetModuleName[i] := GetProcAddress(FLibs[i], 'setmodulename');
      FFuncSetBusSpeed[i] := GetProcAddress(FLibs[i], 'setmtbspeed');
      FFuncSetScanInterval[i] := GetProcAddress(FLibs[i], 'settimerinterval');
      FFuncSaveData[i] := GetProcAddress(FLibs[i], 'savedata');
      FFuncOpen[i] := GetProcAddress(FLibs[i], 'open');
      FFuncClose[i] := GetProcAddress(FLibs[i], 'close');

      //-- events begin --
      //assign callback functions
      FFuncSetBeforeOpenEv[i] := GetProcAddress(FLibs[i],'setbeforeopen');
      FFuncSetAfterOpenEv[i] := GetProcAddress(FLibs[i],'setafteropen');
      FFuncSetBeforeCloseEv[i] := GetProcAddress(FLibs[i],'setbeforeclose');
      FFuncSetAfterCloseEv[i] := GetProcAddress(FLibs[i],'setafterclose');

      FFuncSetBeforeStartEv[i] := GetProcAddress(FLibs[i],'setbeforestart');
      FFuncSetAfterStartEv[i] := GetProcAddress(FLibs[i],'setafterstart');
      FFuncSetBeforeStopEv[i] := GetProcAddress(FLibs[i],'setbeforestop');
      FFuncSetAfterStopEv[i] := GetProcAddress(FLibs[i],'setafterstop');

      FFuncSetOnErrorEv[i] := GetProcAddress(FLibs[i],'setonerror');

      //send OD functions pointers
      if (Assigned(FFuncSetBeforeOpenEv[i])) then FFuncSetBeforeOpenEv[i](@PrgBeforeOpen) else Result := 1;
      if (Assigned(FFuncSetAfterOpenEv[i])) then FFuncSetAfterOpenEv[i](@PrgAfterOpen) else Result := 2;
      if (Assigned(FFuncSetBeforeCloseEv[i])) then FFuncSetBeforeCloseEv[i](@PrgBeforeClose) else Result := 3;
      if (Assigned(FFuncSetAfterCloseEv[i])) then FFuncSetAfterCloseEv[i](@PrgAfterClose) else Result := 4;

      if (Assigned(FFuncSetBeforeStartEv[i])) then FFuncSetBeforeStartEv[i](@PrgBeforeStart) else Result := 5;
      if (Assigned(FFuncSetAfterStartEv[i])) then FFuncSetAfterStartEv[i](@PrgAfterStart) else Result := 6;
      if (Assigned(FFuncSetBeforeStopEv[i])) then FFuncSetBeforeStopEv[i](@PrgBeforeStop) else Result := 7;
      if (Assigned(FFuncSetAfterStopEv[i])) then FFuncSetAfterStopEv[i](@PrgAfterStop) else Result := 8;

      if (Assigned(FFuncSetOnErrorEv[i])) then FFuncSetOnErrorEv[i](@PrgOnError) else Result := 9;
      //-- events end --

    end;
  end;

  if Assigned(FFuncOnLoad[0]) then FFuncOnLoad[0]();
  if Assigned(FFuncOnLoad[1]) then FFuncOnLoad[1]();
  if Assigned(FFuncOnLoad[2]) then FFuncOnLoad[2]();
end;

function TOutputDriver.ShowAboutDialog(Bus: Integer): Integer;
begin
  if Assigned(FFuncShowAboutDialog[Bus]) then Result := FFuncShowAboutDialog[Bus] else Result := -1;
end;

function TOutputDriver.ShowConfigDialog(Bus: Integer): Integer;
begin
  if Assigned(FFuncShowConfigDialog[Bus]) then Result := FFuncShowConfigDialog[Bus] else Result := -1;
end;

function TOutputDriver.HideConfigDialog(Bus: Integer): Integer;
begin
  if Assigned(FFuncHideConfigDialog[Bus]) then Result := FFuncHideConfigDialog[Bus] else Result := -1;
end;

function TOutputDriver.GetInput(Bus, Board, Input: Integer): Integer;
begin
  if Assigned(FFuncGetInput[0]) then begin
    Result := FFuncGetInput[0](Board, Input)
   end else begin
    Result := -1;
  end;
end;

function TOutputDriver.SetOutput(Bus, Board, Output: Integer; state: Integer): Integer;
begin
  if Assigned(FFuncSetOutput[Bus]) then Result := FFuncSetOutput[Bus](Board, Output, state) else Result := -1;
end;

function TOutputDriver.SetInput(Bus, Board, Input: Integer; state: Integer):Integer;
 begin
  if Assigned(FFuncSetInput[Bus]) then Result := FFuncSetInput[Bus](Board, Input, state) else Result := -1;
 end;

function TOutputDriver.GetLibVersion(Bus: Integer):String;
 begin
  if Assigned(FFuncGetLibVersion[Bus]) then Result := FFuncGetLibVersion[Bus] else result := 'Neni dostupna';
 end;

function TOutputDriver.GetDriverVersion(Bus: Integer):String;
 begin
  if Assigned(FFuncGetDriverVersion[Bus]) then Result := FFuncGetDriverVersion[Bus] else result := 'Neni dostupna';
 end;

function TOutputDriver.GetDeviceVersion(Bus: Integer):String;
 begin
  if Assigned(FFuncGetDeviceVersion[Bus]) then Result := FFuncGetDeviceVersion[Bus] else result := 'Neni dostupna';
 end;

function TOutputDriver.GetModuleExists(Bus:Integer;Module:Integer):boolean;
 begin
  if Assigned(FFuncModuleExists[Bus]) then Result:=FFuncModuleExists[Bus](Module) else result:=false;
 end;

function TOutputDriver.GetModuleType(Bus:Integer;Module:Integer):string;
 begin
  if Assigned(FFuncGetModuleType[Bus]) then Result:=FFuncGetModuleType[Bus](Module) else result:='';
 end;

function TOutputDriver.GetModuleName(Bus:Integer;Module:Integer):string;
 begin
  if Assigned(FFuncGetModuleName[Bus]) then Result:=FFuncGetModuleName[Bus](Module) else result:='';
 end;

function TOutputDriver.SetModuleName(Bus:Integer;Module:Integer;Name:string):Integer;
 begin
  if Assigned(FFuncSetModuleName[Bus]) then Result:=FFuncSetModuleName[Bus](Module,Name) else result:=-1;
 end;

function TOutputDriver.SetBusSpeed(Bus:Integer;Speed:Integer):Integer;
 begin
  if Assigned(FFuncSetBusSpeed[Bus]) then Result := FFuncSetBusSpeed[Bus](Speed) else Result := -1;
 end;

function TOutputDriver.SaveData(Bus:Integer):Integer;
 begin
  if Assigned(FFuncSaveData[Bus]) then Result := FFuncSaveData[Bus]() else Result := -1;
 end;

function TOutputDriver.Open(bus:integer):Integer;
 begin
  if Assigned(FFuncOpen[bus]) then Result := FFuncOpen[bus] else Result := -1;
 end;

function TOutputDriver.Close(bus:integer):Integer;
 begin
  if Assigned(FFuncClose[bus]) then Result := FFuncClose[bus] else Result := -1;
 end;

function TOutputDriver.GetModuleFirmware(Bus:integer;Module:integer):string;
 begin
  if Assigned(FFuncGetModuleFirmware[Bus]) then Result := FFuncGetModuleFirmware[Bus](Module) else Result:='nenalezeno';
 end;

function TOutputDriver.SetScanInterval(Bus:Integer;Interval:integer):Integer;
 begin
  if Assigned(FFuncSetScanInterval[Bus]) then Result := FFuncSetScanInterval[Bus](Interval) else Result:=-1;
 end;

procedure Register;
begin
  RegisterComponents('KMZ', [TOutputDriver]);
end;

end.//unit

