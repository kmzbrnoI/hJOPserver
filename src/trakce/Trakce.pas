unit Trakce;

{
  Trida TTrakce sjednocuje tridy TXpressNET a TLocoNet do sebe.
  Jeji funkce je umozneni jednotne komunikace s trakcnim systemem nezavisle
  na tom, o jaky trakcni system se jedna.
}

interface

uses
  SysUtils, Classes, CPort;

const
  _HV_FUNC_MAX = 28;                                                            // maximalni funkcni cislo; funkce zacinaji na cisle 0

type
  TTrkLogLevel = (tllNo = 0, tllError = 1, tllWarning = 2, tllCommand = 3, tllData = 4, tllDebug = 5);

  Ttrk_status = (                                                               // stav centraly
    TS_UNKNOWN = -1,
    TS_OFF = 0,
    TS_ON = 1,
    TS_SERVICE = 2
  );

  Ttrk_system = (                                                               // typ komunikacniho protokolu
    TRS_LocoNET   = 0,
    TRS_XpressNET = 1,
    TRS_Simulator = 2
  );

  TConnect_code = (                                                             // stav pristupu k lokomotive
    TC_Connected = 0,
    TC_Disconnected = 1,
    TC_Unavailable = 2,
    TC_Stolen = 3
  );

  TCommandFuncCallback = procedure (Sender:TObject; Data:Pointer) of object;
  EInvalidAddress = class(Exception);

  TCommandCallback = record
    callback:TCommandFuncCallback;
    data:Pointer;
  end;

  TFunkce = array[0.._HV_FUNC_MAX] of boolean;

  TPomStatus = (released = 0, pc = 1, progr = 2, error = 3);

  TSlot=record                                                                  //data slotu (dalsi vysvetleni v manualu k loconetu a xpressnetu)
   adresa:word;                                                                 // adresa LOKO
   smer:ShortInt;                                                               // aktualni smer: [0,1]
   speed:byte;                                                                  // aktualni jizdni stupen
   maxsp:Integer;                                                               // maximalni jizdni stupen (28, 128, ...)
   funkce:TFunkce;                                                              // stav funkci
   stolen:boolean;                                                              // jestli je loko ukradeno jinym ovladacem
   prevzato:boolean;                                                            // jestli loko ridim ja
   prevzato_full:boolean;                                                       // jestli loko ridim ja a jestli uz byly nastaveny vsechny veci, ktere se pri prebirani maji nastavit
   com_err:boolean;                                                             // jestli nastala chyba v komunikaci
   pom:TPomStatus;                                                              // stav POMu (jaky je posedni naprogramovany POM)
  end;

  TCSVersion = record                                                           // verze centraly
    major:byte;
    minor:byte;
    id:byte;
  end;

  TLIVersion = record                                                           // verze FW v LI
    hw_major:byte;
    hw_minor:byte;
    sw_major:byte;
    sw_minor:byte;
  end;

  TLogEvent = procedure(Sender:TObject; lvl:TTrkLogLevel; msg:string) of object;
  TConnectChangeInfo = procedure(Sender: TObject; addr:Integer; code:TConnect_code; data:Pointer) of object;
  TLokComEvent = procedure (Sender:TObject; addr:Integer) of object;
  TGeneralEvent = procedure(Sender: TObject) of object;
  TCSVersionEvent = procedure(Sender:TObject; version:TCSVersion) of object;
  TLIVersionEvent = procedure(Sender:TObject; version:TLIVersion) of object;
  TLIAddressEvent = procedure(Sender:TObject; addr:Byte) of object;

  TTrakce = class
    private const

    protected
      Get:record
       sp_addr:Integer;
      end;

     FOnTrackStatusChange : TGeneralEvent;
     FOnComError          : TGeneralEvent;
     FOnCSVersion         : TCSVersionEvent;
     FOnLIVersion         : TLIVersionEvent;
     FOnLIAddr            : TLIAddressEvent;

     FFtrk_status: Ttrk_status;

      procedure WriteLog(lvl:TTrkLogLevel; msg:string);
      procedure ConnectChange(addr:Integer; code:TConnect_code; data:Pointer);
      procedure LokComError(addr:Integer);
      procedure LokComOK(addr:Integer);
      procedure CSGotVersion(version:TCSVersion);
      procedure LIGotVersion(version:TLIVersion);
      procedure LIGotAddress(addr:Byte);

      procedure SetTrackStatus(NewtrackStatus:Ttrk_status); virtual; abstract;
      procedure SetOwnTrackStatus(New:Ttrk_status);

      property Ftrk_status: Ttrk_status read FFtrk_status write SetOwnTrackStatus;

    private
     FOnLog: TLogEvent;
     FOnConnectChange: TConnectChangeInfo;
     FOnLokComError : TLokComEvent;
     FOnLokComOK    : TLokComEvent;

    public

     callback_err:TCommandCallback;
     callback_ok:TCommandCallback;

     Slot:TSlot;

     ComPort:record
       CPort:TComPort;
     end;

      constructor Create();
      destructor Destroy(); override;
      function Name():string; virtual; abstract;

      procedure LokSetSpeed(Address:Integer; speed:Integer; dir:Integer); virtual; abstract;
      procedure LokSetFunc(Address:Integer; sada:Byte; stav:Byte); virtual; abstract;
      procedure LokGetFunctions(Address:Integer; startFunc:Integer); virtual; abstract;
      procedure EmergencyStop(); virtual; abstract;
      procedure LokEmergencyStop(addr:Integer); virtual; abstract;
      procedure LokGetInfo(Address:Integer); virtual; abstract;
      procedure Lok2MyControl(Address:Integer); virtual; abstract;          // po volani teto funkce musi byt do slotu umistena data (resp. pred zavolanim eventu OnConnectChange)! MUSI!
      procedure LokFromMyControl(Address:Integer); virtual; abstract;
      procedure GetCSVersion(callback:TCSVersionEvent); virtual;
      procedure GetLIVersion(callback:TLIVersionEvent); virtual;
      procedure GetLIAddress(callback:TLIAddressEvent); virtual;
      procedure SetLIAddress(callback:TLIAddressEvent; addr:Byte); virtual;

      procedure GetTrackStatus(); virtual; abstract;
      procedure AfterOpen(); virtual;
      procedure BeforeClose(); virtual; abstract;

      procedure POMWriteCV(Address:Integer; cv:Word; data:byte); virtual; abstract;

      class function GenerateCallback(callback:TCommandFuncCallback; data:Pointer = nil):TCommandCallback;

      property TrackStatus: Ttrk_status read FFtrk_status write SetTrackStatus;
      property GetAddr:Integer read Get.sp_addr;

      //events
      property OnLog: TLogEvent read FOnLog write FOnLog;
      property OnConnectChange: TConnectChangeInfo read FOnConnectChange write FOnConnectChange;
      property OnLokComError: TLokComEvent read FOnLokComError write FOnLokComError;
      property OnLokComOK: TLokComEvent read FOnLokComOK write FOnLokComOK;

      property OnTrackStatusChange: TGeneralEvent read FOnTrackStatusChange write FOnTrackStatusChange;
      property OnComError : TGeneralEvent read fOnComError write fOnComError;
  end;//TTrakce

implementation

constructor TTrakce.Create();
begin
 inherited;
 Self.FFtrk_status := Ttrk_status.TS_UNKNOWN;
end;//ctor

destructor TTrakce.Destroy();
begin
 inherited;
end;//dtor

procedure TTrakce.WriteLog(lvl:TTrkLogLevel; msg: string);
begin
 if (Assigned(Self.FOnLog)) then Self.FOnLog(Self, lvl, msg);
end;

procedure TTrakce.ConnectChange(addr:Integer; code:TConnect_code; data:Pointer);
begin
 if (Assigned(Self.FOnConnectChange)) then Self.FOnConnectChange(self, addr, code, data);
end;

procedure TTrakce.LokComError(addr:Integer);
begin
 if (Assigned(Self.FOnLokComError)) then Self.FOnLokComError(Self, addr);
end;

procedure TTrakce.LokComOK(addr:Integer);
begin
 if (Assigned(Self.FOnLokComOK)) then Self.FOnLokComOK(Self, addr);
end;//prccedure

class function TTrakce.GenerateCallback(callback:TCommandFuncCallback; data:Pointer = nil):TCommandCallback;
begin
 Result.callback := callback;
 Result.data     := data;
end;

procedure TTrakce.SetOwnTrackStatus(New:Ttrk_status);
begin
 if (Self.FFtrk_status <> new) then
  begin
   Self.FFtrk_status := new;
   if (Assigned(Self.FOnTrackStatusChange)) then Self.FOnTrackStatusChange(Self);
  end;
end;

procedure TTrakce.AfterOpen();
begin
 Self.FFtrk_status := TS_UNKNOWN;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.CSGotVersion(version:TCSVersion);
begin
 if (Assigned(Self.FOnCSVersion)) then Self.FOnCSVersion(Self, version);
end;

procedure TTrakce.GetCSVersion(callback:TCSVersionEvent);
begin
 Self.FOnCSVersion := callback;
end;

procedure TTrakce.GetLIVersion(callback:TLIVersionEvent);
begin
 Self.FOnLIVersion := callback;
end;

procedure TTrakce.LIGotVersion(version:TLIVersion);
begin
 if (Assigned(Self.FOnLIVersion)) then Self.FOnLIVersion(Self, version);
end;

procedure TTrakce.GetLIAddress(callback:TLIAddressEvent);
begin
 Self.FOnLIAddr := callback;
end;

procedure TTrakce.SetLIAddress(callback:TLIAddressEvent; addr:Byte);
begin
 Self.FOnLIAddr := callback;
end;

procedure TTrakce.LIGotAddress(addr:Byte);
begin
 if (Assigned(Self.FOnLIAddr)) then Self.FOnLIAddr(Self, addr);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
