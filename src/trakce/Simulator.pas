unit Simulator;

// Trida Simulator slouzi k simulaci komunikace s centralou.

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, Math, Generics.Collections,
  ExtCtrls, Forms;

type
  // mozne prikazy z PC do centraly:
  Tcmd = (
    XB_TRK_OFF = 10,
    XB_TRK_ON  = 11,
    XB_TRK_STATUS = 12,
    XB_TRK_CS_VERSION = 13,
    XB_TRK_LI_VERSION = 14,
    XB_LOK_SET_SPD = 15,
    XB_TRK_LI_ADDRESS = 16,
    XB_LOK_SET_FUNC = 20,
    XB_LOK_GET_INFO = 50,
    XB_LOK_GET_FUNC = 51,
    XB_LOK_STOP = 53,
    XB_STOP_ALL = 54,
    XB_TRK_FINDLOK = 55,
    XB_POM_WRITEBYTE = 56
  );

  THistoryPacket = record
   cmd: Tcmd;
   callback_err: TCommandCallback;
   callback_ok: TCommandCallback;
  end;

  TSimulator = class(TTrakce)
   private const
    _HIST_CHECK_INTERVAL = 100;

    _CS_VERSION: TCSVersion = (
      major: 1;
      minor: 1;
      id: 5;
    );

    _LI_VERSION: TLIVersion = (
      hw_major: 1;
      hw_minor: 0;
      sw_major: 1;
      sw_minor: 0;
    );

    _LI_ADDRESS = 25;

   private
    loading_addr:Word;                                                          // aktualni adresa lokomotivy, ktera se prebira
                                                                                // POZOR: v jeden okamzik lze prebirat nejvyse jednu lokomotivu

    send_history: TList<THistoryPacket>;                                        // vystupni buffer (data z PC do centraly)
    timer_history:TTimer;                                                       // timer starajici se o vystupni buffer

    procedure OnTimer_history(Sender:TObject);                                  // tick timer_history
    function HistoryPacket(cmd:Tcmd):THistoryPacket;
    procedure CheckLoading(hist_item:THistoryPacket);

   public

     constructor Create();
     destructor Destroy(); override;

     procedure SetTrackStatus(NewtrackStatus:Ttrk_status); override;            // nastav stav centraly: ON, OFF, PROGR

     procedure LokEmergencyStop(addr:Integer); override;                        // nouzove zastav lokomotivu
     procedure LokSetSpeed(Address:Integer; speed:Integer;                      // nastav rychlost lokomotivy
                           dir:Integer); override;
     procedure LokSetFunc(Address:Integer; sada:Byte; stav:Byte); override;     // nastav funkcni sadu \sada do stavu \stav (po bitech)
     procedure LokGetInfo(Address:Integer); override;                           // zjisti informace o lokomotive \Address
     procedure Lok2MyControl(Address:Integer); override;                        // prevezmi lokomotivu \Address
     procedure LokFromMyControl(Address:Integer); override;                     // uvolni lokomotivu \Address
     procedure LokGetFunctions(Address:Integer; startFunc:Integer); override;   // zjisti stav funkci lokomotivy \Address od funkce \startFunc
                                                                                //  \startFunc je typicky 0 anebo 13

     procedure GetTrackStatus(); override;                                      // zjisti stav trakce z centraly
     procedure GetCSVersion(callback:TCSVersionEvent); override;                // zjisti verzi FW hlavniho procesoru v centrale
     procedure GetLIVersion(callback:TLIVersionEvent); override;                // zjisti verzi FW LI
     procedure GetLIAddress(callback:TLIAddressEvent); override;                // zjisti adresu LI
     procedure SetLIAddress(callback:TLIAddressEvent; addr:Byte); override;     // nastavi adresu LI
     procedure POMWriteCV(Address:Integer; cv:Word; data:byte); override;       // zapis CV \cv na hodnotu \data POMem

     procedure EmergencyStop(); override;                                       // nouzove zastav vsechny lokomotivy

     procedure BeforeClose(); override;                                         // event pred zavrenim komunikace

   protected

  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TSimulator.Create();
var i:Integer;
begin
 inherited;

 Self.send_history            := TList<THistoryPacket>.Create();
 Self.timer_history           := TTimer.Create(nil);
 Self.timer_history.Interval  := _HIST_CHECK_INTERVAL;
 Self.timer_history.OnTimer   := Self.OnTimer_history;
 Self.timer_history.Enabled   := true;

 Self.callback_err.callback := nil;
 Self.callback_ok.callback  := nil;

 Slot.funkce[0] := true;
 for i := 1 to _HV_FUNC_MAX do
   Slot.funkce[i] := false;
 Slot.speed := 0;
 Slot.stolen := false;

 Self.Get.sp_addr := -1;
end;

////////////////////////////////////////////////////////////////////////////////

destructor TSimulator.Destroy();
begin
 Self.timer_history.Free();
 if (Assigned(Self.send_history)) then
   FreeAndNil(Self.send_history);

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TSimulator.HistoryPacket(cmd: TCmd):THistoryPacket;
begin
 Result.cmd := cmd;
 Result.callback_err := Self.callback_err;
 Result.callback_ok := Self.callback_ok;
 Self.callback_err.callback := nil;  // WARNING: modifying of global variables!
 Self.callback_ok.callback := nil;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokSetSpeed(Address: Integer; speed: Integer; dir: Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 Self.send_history.Add(Self.HistoryPacket(XB_LOK_SET_SPD));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokSetFunc(Address:Integer; sada:Byte; stav:Byte);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 Self.send_history.Add(Self.HistoryPacket(XB_LOK_SET_FUNC));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.SetTrackStatus(NewTrackStatus: Ttrk_status);
begin
  case NewTrackStatus of
    TS_OFF : Self.send_history.Add(Self.HistoryPacket(XB_TRK_OFF));
    TS_ON  : Self.send_history.Add(Self.HistoryPacket(XB_TRK_ON));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.EmergencyStop();
begin
 Self.send_history.Add(Self.HistoryPacket(XB_STOP_ALL));
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokEmergencyStop(addr:Integer);
begin
 Self.send_history.Add(Self.HistoryPacket(XB_LOK_STOP));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokGetInfo(Address:Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 Self.Get.sp_addr := Address;
 Self.send_history.Add(Self.HistoryPacket(XB_LOK_GET_INFO));
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.Lok2MyControl(Address:Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 Self.loading_addr := Address;
 Self.LokGetInfo(Address);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokFromMyControl(Address:Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 //zavolame rovnou callback
 Self.ConnectChange(Address, TConnect_code.TC_Disconnected, Self.callback_ok.data);

 if (Assigned(Self.callback_ok.callback)) then
  Self.callback_ok.callback(Self, Self.callback_ok.data);

 Self.callback_err.callback := nil;
 Self.callback_ok.callback  := nil;
end;//function

////////////////////////////////////////////////////////////////////////////////

// Kdyz mi prijde INFO o loko, zkontroluje se, jestli o ni nahodou ted nezadam.
// Pokud o ni zadam, volam event.
procedure TSimulator.CheckLoading(hist_item:THistoryPacket);
begin
 if (Self.loading_addr = Slot.adresa) then
  begin
   Self.loading_addr := 0;
   Self.ConnectChange(Slot.adresa, TConnect_code.TC_Connected, hist_item.callback_ok.data)
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.OnTimer_history(Sender:TObject);
var data:THistoryPacket;
begin
 if (Self.send_history.Count > 0) then
  begin
   data := Self.send_history[0];
   Self.send_history.Delete(0);

   case (data.cmd) of
     XB_TRK_OFF: begin
       Self.WriteLog(2, 'GET: STATUS OFF');
       Self.Ftrk_status := TS_OFF;
     end;

     XB_TRK_ON: begin
       Self.WriteLog(2, 'GET: STATUS ON');
       Self.Ftrk_status := TS_ON;
     end;

     XB_TRK_STATUS: begin
      if ((Self.Ftrk_status = TS_ON) or (Self.Ftrk_status = TS_UNKNOWN)) then
        Self.WriteLog(2, 'GET: STATUS ON')
      else if (Self.Ftrk_status = TS_OFF) then
        Self.WriteLog(2, 'GET: STATUS OFF')
      else if (Self.Ftrk_status = TS_SERVICE) then
        Self.WriteLog(2, 'GET: STATUS SERVICE');

      if (Self.Ftrk_status = TS_UNKNOWN) then
        Self.Ftrk_status := TS_ON
      else
        Self.Ftrk_status := Self.Ftrk_status;
     end;

     XB_TRK_CS_VERSION: begin
       Self.WriteLog(2, 'GET: CS VERSION '+IntToStr(_CS_VERSION.major)+'.'+IntToStr(_CS_VERSION.minor)+
                        ', ID: '+IntToStr(_CS_VERSION.id));
       Self.CSGotVersion(_CS_VERSION);
     end;

     XB_TRK_LI_VERSION: begin
       Self.WriteLog(2, 'GET: LI VERSION: HW: '+IntToStr(_LI_VERSION.hw_major)+'.'+IntToStr(_LI_VERSION.hw_minor)+
                        ', SW: '+IntToStr(_LI_VERSION.sw_major)+'.'+IntToStr(_LI_VERSION.sw_minor));
       Self.LIGotVersion(_LI_VERSION);
     end;

     XB_TRK_LI_ADDRESS: begin
       Self.WriteLog(2, 'GET: LI ADDRESS: '+IntToStr(_LI_ADDRESS));
       Self.LIGotAddress(_LI_ADDRESS);
     end;

     XB_LOK_GET_INFO: begin
       Slot.adresa := Self.loading_addr;
       Self.CheckLoading(data);
     end;
   end;

   if (Assigned(data.callback_ok.callback)) then
     data.callback_ok.callback(Self, data.callback_ok.data);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.GetTrackStatus();
begin
 Self.WriteLog(4, 'PUT: GET-TRACK-STATUS');
 Self.send_history.Add(Self.HistoryPacket(XB_TRK_STATUS));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.GetCSVersion(callback:TCSVersionEvent);
begin
 inherited;
 Self.WriteLog(4, 'PUT: GET-CS-VERSION');
 Self.send_history.Add(Self.HistoryPacket(XB_TRK_CS_VERSION));
end;//procedure

procedure TSimulator.GetLIVersion(callback:TLIVersionEvent);
begin
 inherited;
 Self.WriteLog(4, 'PUT: GET-LI-VERSION');
 Self.send_history.Add(Self.HistoryPacket(XB_TRK_LI_VERSION));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.GetLIAddress(callback:TLIAddressEvent);
begin
 inherited;
 Self.WriteLog(4, 'PUT: GET-LI-ADDR');
 Self.send_history.Add(Self.HistoryPacket(XB_TRK_LI_ADDRESS));
end;

procedure TSimulator.SetLIAddress(callback:TLIAddressEvent; addr:Byte);
begin
 inherited;
 Self.WriteLog(4, 'PUT: SET-LI-ADDR');
 Self.send_history.Add(Self.HistoryPacket(XB_TRK_LI_ADDRESS));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.POMWriteCV(Address:Integer; cv:Word; data:byte);
begin
 Self.WriteLog(4, 'PUT: POM '+IntToStr(cv)+':'+IntToStr(data));
 Self.send_history.Add(Self.HistoryPacket(XB_POM_WRITEBYTE));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.BeforeClose();
begin
 Self.send_history.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSimulator.LokGetFunctions(Address:Integer; startFunc:Integer);
begin
 Self.send_history.Add(Self.HistoryPacket(XB_LOK_GET_FUNC));
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
