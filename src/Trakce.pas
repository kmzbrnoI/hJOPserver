unit Trakce;

//trida TTrakce sjednocuje tridy TXpressNET a TLocoNet do sebe
//jeji funkce je umozneni jednotne komunikace s trakcnim systemem nezavisle na tom, o jaky se jedna

interface

uses
  SysUtils, Classes, CPort;

type
  Ttrk_status = (
    TS_UNKNOWN = -1,
    TS_OFF = 0,
    TS_ON = 1,
    TS_SERVICE = 2
  );

  Ttrk_system = (
    TRS_LocoNET   = 0,
    TRS_XpressNET = 1
  );

  TConnect_code = (
    TC_Connected = 0,
    TC_Disconnected = 1,
    TC_Unavailable = 2,
    TC_Stolen = 3
  );

  TCommandFuncCallback = procedure (Sender:TObject; Data:Pointer) of object;

  TCommandCallback = record
    callback:TCommandFuncCallback;
    data:Pointer;
  end;

  TFunkce = array[0..15] of boolean;

  TPomStatus = (released = 0, pc = 1, progr = 2, error = 3);

  TSlot=record                                         //data slotu (dalsi vysvetleni v manualu k loconetu a xpressnetu)
   adresa:word;
   smer:ShortInt;        // 0,1
   speed:byte;
   maxsp:Integer;        // 28,128, ...
   funkce:TFunkce;       // F0-F12
   stolen:boolean;       // ukaradeno jinym ovladacem
   prevzato:boolean;     // jestli loko ridim ja

   ID:Byte;
   status:byte;
   track:byte;
   fred:word;
   FredID:Integer;
   SmerITData:Byte;
   SND:byte;
   SS2:byte;
   FREDLastSlotID:Integer;

   com_err:boolean;     // jestli nastala chyba v komunikaci

   pom:TPomStatus;
  end;

  TCSVersion = record
    major:byte;
    minor:byte;
    id:byte;
  end;

  TLIVersion = record
    hw_major:byte;
    hw_minor:byte;
    sw_major:byte;
    sw_minor:byte;
  end;

  TLogEvent = procedure(Sender:TObject; lvl:Integer; msg:string) of object;
  TGetSpInfoEvent = procedure(Sender: TObject; Slot:TSlot) of object;
  TGetFInfoEvent = procedure(Sender: TObject; addr:Integer; func:TFunkce) of object;
  TConnectChangeInfo = procedure(Sender: TObject; addr:Integer; code:TConnect_code; data:Pointer) of object;
  TLokComEvent = procedure (Sender:TObject; addr:Integer) of object;
  TGeneralEvent = procedure(Sender: TObject) of object;
  TCSVersionEvent = procedure(Sender:TObject; version:TCSVersion) of object;
  TLIVersionEvent = procedure(Sender:TObject; version:TLIVersion) of object;

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

     FFtrk_status: Ttrk_status;

      procedure WriteLog(lvl:Integer; msg:string);          //will be called from child
      procedure WriteSpInfo(Slot:TSlot);
      procedure WriteFInfo(addr:Integer; func:TFunkce);
      procedure ConnectChange(addr:Integer; code:TConnect_code; data:Pointer);
      procedure LokComError(addr:Integer);
      procedure LokComOK(addr:Integer);
      procedure CSGotVersion(version:TCSVersion);
      procedure LIGotVersion(version:TLIVersion);

      procedure SetTrackStatus(NewtrackStatus:Ttrk_status); virtual; abstract;
      procedure SetOwnTrackStatus(New:Ttrk_status);

      property Ftrk_status: Ttrk_status read FFtrk_status write SetOwnTrackStatus;

    private
     FOnLog: TLogEvent;
     FONGetSpInfo : TGetSpInfoEvent;
     FONGetFInfo : TGetFInfoEvent;
     FOnConnectChange: TConnectChangeInfo;
     FOnLokComError : TLokComEvent;
     FOnLokComOK    : TLokComEvent;

    public

     callback_err:TCommandCallback;
     callback_ok:TCommandCallback;

     ComPort:record
       CPort:TComPort;
     end;

      constructor Create();
      destructor Destroy(); override;

      function LokSetSpeed(Address:Integer; speed:Integer; dir:Integer):Byte; virtual; abstract;
      function LokSetFunc(Address:Integer; sada:Byte; stav:Byte):Byte; virtual; abstract;
      procedure EmergencyStop(); virtual; abstract;
      procedure EmergencyStopLoko(addr:Integer); virtual; abstract;
      function GetLocomotiveInfo(Address:Integer):Byte; virtual; abstract;
      function Lok2MyControl(Address:Integer):Byte; virtual; abstract;          // po volani teto funkce musi byt do slotu umistena data (resp. pred zavolanim eventu OnConnectChange)! MUSI!
      function LokFromMyControl(Address:Integer):Byte; virtual; abstract;
      procedure GetCSVersion(callback:TCSVersionEvent); virtual;
      procedure GetLIVersion(callback:TLIVersionEvent); virtual;

      procedure GetTrackStatus(); virtual; abstract;
      procedure AfterOpen(); virtual;
      procedure BeforeClose(); virtual; abstract;

      procedure POMWriteCV(Address:Integer; cv:Word; data:byte); virtual; abstract;

      class function GenerateCallback(callback:TCommandFuncCallback; data:Pointer = nil):TCommandCallback;

      property TrackStatus: Ttrk_status read FFtrk_status write SetTrackStatus;
      property GetAddr:Integer read Get.sp_addr;

      //events
      property OnLog: TLogEvent read FOnLog write FOnLog;
      property OnGetSpInfo: TGetSpInfoEvent read FONGetSpInfo write FONGetSpInfo;
      property OnGetFInfo: TGetFInfoEvent read FONGetFInfo write FONGetFInfo;
      property OnConnectChange: TConnectChangeInfo read FOnConnectChange write FOnConnectChange;
      property OnLokComError: TLokComEvent read FOnLokComError write FOnLokComError;
      property OnLokComOK: TLokComEvent read FOnLokComOK write FOnLokComOK;

      property OnTrackStatusChange: TGeneralEvent read FOnTrackStatusChange write FOnTrackStatusChange;
      property OnComError : TGeneralEvent read fOnComError write fOnComError;
  end;//TTrakce

implementation

uses XpressNET;

constructor TTrakce.Create();
begin
 inherited Create;
// Self.ComPort.CPort := TComPort.Create(nil);
 Self.FFtrk_status := Ttrk_status.TS_UNKNOWN;
end;//ctor

destructor TTrakce.Destroy();
begin
// FreeAndNil(Self.ComPort.CPort);
 inherited Destroy;
end;//dtor

procedure TTrakce.WriteLog(lvl:Integer; msg: string);
begin
 if (Assigned(Self.FOnLog)) then Self.FOnLog(Self, lvl, msg);
end;//procedure

procedure TTrakce.WriteSpInfo(Slot:TSlot);
begin
 if (Assigned(Self.FOnGetSpInfo)) then Self.FONGetSpInfo(self, Slot);
end;//procedure

procedure TTrakce.WriteFInfo(addr:Integer; func:TFunkce);
begin
 if (Assigned(Self.FOnGetFInfo)) then Self.FONGetFInfo(self, addr, func);
end;//procedure

procedure TTrakce.ConnectChange(addr:Integer; code:TConnect_code; data:Pointer);
begin
 if (Assigned(Self.FOnConnectChange)) then Self.FOnConnectChange(self, addr, code, data);
end;//procedure

procedure TTrakce.LokComError(addr:Integer);
begin
 if (Assigned(Self.FOnLokComError)) then Self.FOnLokComError(Self, addr);
end;//procedure

procedure TTrakce.LokComOK(addr:Integer);
begin
 if (Assigned(Self.FOnLokComOK)) then Self.FOnLokComOK(Self, addr);
end;//prccedure

class function TTrakce.GenerateCallback(callback:TCommandFuncCallback; data:Pointer = nil):TCommandCallback;
begin
 Result.callback := callback;
 Result.data     := data;
end;//function

procedure TTrakce.SetOwnTrackStatus(New:Ttrk_status);
begin
 if (Self.FFtrk_status <> new) then
  begin
   Self.FFtrk_status := new;
   if (Assigned(Self.FOnTrackStatusChange)) then Self.FOnTrackStatusChange(Self);
  end;
end;//procedure

procedure TTrakce.AfterOpen();
begin
 Self.FFtrk_status := TS_UNKNOWN;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTrakce.CSGotVersion(version:TCSVersion);
begin
 if (Assigned(Self.FOnCSVersion)) then Self.FOnCSVersion(Self, version);
end;//procedure

procedure TTrakce.GetCSVersion(callback:TCSVersionEvent);
begin
 Self.FOnCSVersion := callback;
end;//function

procedure TTrakce.GetLIVersion(callback:TLIVersionEvent);
begin
 Self.FOnLIVersion := callback;
end;//procedure

procedure TTrakce.LIGotVersion(version:TLIVersion);
begin
 if (Assigned(Self.FOnLIVersion)) then Self.FOnLIVersion(Self, version);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
