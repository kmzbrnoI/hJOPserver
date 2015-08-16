unit XpressNET;

// trida TXpressNet slouzi ke komunikaci s centralou zapomoci serioveho portu
// jeji funkce je prekladadni prikazu mezi programem a centralou (resp. XpressNETem)

// pouzivame vzdy 28 jizdnich stupnu

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, Math, Generics.Collections,
  ExtCtrls;

type
  TBuffer = record
   data:array [0..255] of Byte;
   Count:Integer;
  end;

  Tcmd = (
    XB_TRK_OFF,
    XB_TRK_ON,
    XB_TRK_STATUS,
    XB_TRK_CS_VERSION,
    XB_TRK_LI_VERSION,
    XB_LOK_SET_SPD,
    XB_LOK_SET_FUNC_0_4,
    XB_LOK_SET_FUNC_5_8,
    XB_LOK_SET_FUNC_9_12,
    XB_LOK_SET_FUNC_13_20,
    XB_LOK_SET_FUNC_21_28,
    XB_LOK_GET_SPD,
    XB_LOK_GET_FUNC,
    XB_LOK_GET_FUNC_13_28,
    XB_LOK_STOP,
    XB_STOP_ALL,
    XB_TRK_FINDLOK,
    XB_POM_WRITEBYTE
  );


  TParseMsgEvent = procedure(Sender: TObject; msg: TBuffer; var Handled: boolean) of object;

  TXpressNETHistoryPacket = record
   cmd: Tcmd;
   params:array [0..2] of Integer;
   time:TDateTime;
   sent:Integer;
   callback_err:TCommandCallback;
   callback_ok:TCommandCallback;
  end;

  TXpressNET = class(TTrakce)
   private const
    _MAX_HISTORY  = 1024;
    _HIST_CHECK_INTERVAL = 100;
    _TIMEOUT_MSEC = 200;
    _SEND_MAX     = 8;

   public

     constructor Create();
     destructor Destroy(); override;

     procedure SetTrackStatus(NewtrackStatus:Ttrk_status); override;
     function LokSetSpeed(Address:Integer; speed:Integer; dir:Integer):Byte; override;
     function LokSetFunc(Address:Integer; sada:Byte; stav:Byte):Byte; override;
     function GetLocomotiveInfo(Address:Integer):Byte; override;
     function Lok2MyControl(Address:Integer):Byte; override;
     function LokFromMyControl(Address:Integer):Byte; override;
     procedure GetTrackStatus(); override;
     procedure GetCSVersion(callback:TCSVersionEvent); override;
     procedure GetLIVersion(callback:TLIVersionEvent); override;
     procedure POMWriteCV(Address:Integer; cv:Word; data:byte); override;


     procedure EmergencyStop(); override;
     procedure EmergencyStopLoko(addr:Integer); override;

     procedure AfterOpen(); override;
     procedure BeforeClose(); override;

   private
    Fbuf_in: TBuffer;
    Fbuf_in_time: TDateTime;

    FOnParseMsg: TParseMsgEvent;
    loading_addr:Word;                  //aktualni adresa lokomotivy, ktera se nacita

    send_history: TList<TXpressNETHistoryPacket>;
    timer_history:TTimer;

    function LokAddrEncode(addr: Integer): Word;
    function LokAddrDecode(ah, al: byte): Integer;

    procedure DataReceive(Sender: TObject; Count: Integer);
    procedure ParseMsg(msg: TBuffer);
    procedure ParseMsgTry(msg: TBuffer);

    procedure Send(buf: TBuffer);
    procedure SendCommand(cmd: Tcmd; p1:Integer = 0; p2:Integer = 0; p3: Integer = 0; sent_times:Integer = 0);

    function CreateBuf(str:ShortString):TBuffer;

    procedure CheckLoading(Slot:TSlot);

    procedure OnTimer_history(Sender:TObject);
    procedure hist_send(index:Integer);
    procedure hist_ok();
    procedure hist_err();

    //events
    property OnParseMsg: TParseMsgEvent read FOnParseMsg write FOnParseMsg;

   protected
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
constructor TXpressNET.Create();
begin
 inherited Create();

 Self.send_history            := TList<TXpressNETHistoryPacket>.Create();
 Self.timer_history           := TTimer.Create(nil);
 Self.timer_history.Interval  := _HIST_CHECK_INTERVAL;
 Self.timer_history.OnTimer   := Self.OnTimer_history;
 Self.timer_history.Enabled   := true;

 Self.callback_err.callback := nil;
 Self.callback_ok.callback  := nil;

 Self.Get.sp_addr := -1;
end;//ctor

////////////////////////////////////////////////////////////////////////////////
destructor TXpressNET.Destroy();
begin
 Self.timer_history.Free();
 if (Assigned(Self.send_history)) then
   FreeAndNil(Self.send_history);

 inherited Destroy;
end;//dtor

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.LokAddrEncode(addr: Integer): Word;
begin
  if (addr > 99) then begin
    Result := (addr + $C000);
   end else begin
    Result := addr;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.LokAddrDecode(ah, al: byte): Integer;
begin
  Result := al or ((ah AND $3F) shl 8);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.DataReceive(Sender: TObject; Count: Integer);
var
  ok: Boolean;
  msg_len: Integer;
  x: byte;
  i,tmp: Integer;
  Buf:array [0..255] of Byte;
  s:string;
begin
  Self.ComPort.CPort.Read(buf, Count);

  for i := 0 to Count-1 do Fbuf_in.data[Fbuf_in.Count+i] := Buf[i];
  Fbuf_in.Count := Fbuf_in.Count + Count;

  s := 'BUF: ';
  for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
  WriteLog(5, s);

  Fbuf_in_time := Time;

  ok := true;
  while (ok) do
   begin
    if (Fbuf_in.Count >= 1) then
     begin
      msg_len := (Fbuf_in.data[0] AND $0F) + 1;
      if ((msg_len+1) <= Fbuf_in.Count) then
       begin
        // check xor
        x := 0;
        for i := 0 to msg_len-1 do x := x xor Fbuf_in.data[i];

        if (x = Fbuf_in.data[msg_len]) then
         begin
          // parse one message
          tmp := Fbuf_in.Count;
          Fbuf_in.Count := msg_len+1;
          ParseMsgTry(Fbuf_in);
          Fbuf_in.Count := tmp;

          // remove parsed message from buffer
          for i := 0 to msg_len+1 do Fbuf_in.data[i] := Fbuf_in.data[i+msg_len+1];
          Fbuf_in.Count := Fbuf_in.Count - msg_len - 1;

          s := 'BUF: ';
          for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
          if (Fbuf_in.Count > 0) then WriteLog(5, s);
         end else begin
          // xor error
          s := '';
          for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
          WriteLog(1, 'GET: XOR ERROR, removing buffer : '+s);
          Fbuf_in.Count := 0;
         end;

       end else ok := false;
     end else ok := false;
   end;//while
end;//procedur

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.ParseMsgTry(msg: TBuffer);
var Handled: boolean;
begin
  if Assigned(FOnParseMsg) then begin
    FOnParseMsg(Self, msg, Handled);
    if (not Handled) then ParseMsg(msg);
   end else ParseMsg(msg);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.ParseMsg(msg: TBuffer);
var
  i: integer;
  s: string;
  Slot:TSlot;
  cs_version: TCSVersion;
  li_version: TLIVersion;
begin
  s := '';
  for i := 0 to msg.Count-1 do s := s + IntToHex(msg.data[i], 2) + ' ';

  Self.WriteLog(4, 'GET: '+s);

  case (msg.data[0]) of
    $01: begin
      case (msg.data[1]) of
       $01: Self.WriteLog(1, 'GET ERR: Error occurred between the interfaces and the PC');
       $02: Self.WriteLog(1, 'GET ERR: Error occurred between the interfaces and the command station');
       $03: begin
          Self.WriteLog(1, 'GET ERR: Unknown communication error');      // probably send data again ?
//          if (Self.send_history.Count > 0) then Self.hist_send(0);    // odpoved chybou - data radsi neodesilame hned - pockame na timeout
       end;
       $04: begin
          Self.WriteLog(2, 'GET: OK');
          Self.hist_ok();
       end;
       $05: Self.WriteLog(2, 'GET: The Command Station is no longer providing the LI100 a timeslot for communication');
       $06: Self.WriteLog(1, 'GET ERR: Buffer overflow in the LI100');
      end;//case
    end;

    $02: begin
      // LI version
      li_version.hw_major := (msg.data[1] shr 4) and $0F;
      li_version.hw_minor := (msg.data[1]) and $0F;

      li_version.sw_major := (msg.data[2] shr 4) and $0F;
      li_version.sw_minor := (msg.data[2]) and $0F;

      Self.WriteLog(2, 'GET: LI VERSION: HW: '+IntToStr(li_version.hw_major)+'.'+IntToStr(li_version.hw_minor)+
                        ', SW: '+IntToStr(li_version.sw_major)+'.'+IntToStr(li_version.sw_minor));
      Self.LIGotVersion(li_version);
      Self.hist_ok();
    end;

    $61: begin  // broadcast messages
      case (msg.data[1]) of

        $02: begin
          Ftrk_status := TS_SERVICE;
          Self.WriteLog(2, 'GET: STATUS SERVICE');
          Self.hist_ok();
        end;

        $01:begin
          Ftrk_status := TS_ON;
          Self.WriteLog(2, 'GET: STATUS ON');

          // druha cast podminky je dulezita
          //  resi se tu to, ze centralal odpovida na prikaz TRK_ON 3x TRK_ON
          if ((Self.send_history.Count > 0) and (Self.send_history[0].cmd = Tcmd.XB_TRK_ON))  then
            Self.hist_ok();
        end;

        $00: begin
          Ftrk_status := TS_OFF;
          Self.WriteLog(2, 'GET: STATUS OFF');

          // druha cast podminky je dulezita
          //  resi se tu to, ze centralalodpovida na prikaz TRK_OFF 3x TRK_OFF
          if ((Self.send_history.Count > 0) and (Self.send_history[0].cmd = Tcmd.XB_TRK_OFF))  then
            Self.hist_ok();
        end;

        $80: begin
          Self.WriteLog(1, 'GET ERR: transfer'+s);
          if (Self.send_history.Count > 0) then
            Self.hist_send(0);    // odpoved chybou - odesleme data znovu
        end;
        $81: begin
          Self.WriteLog(1, 'GET ERR: busy'+s);
          if (Self.send_history.Count > 0) then
            Self.hist_send(0);    // odpoved chybou - odesleme data znovu
        end;

        $82: begin
          Self.WriteLog(1, 'GET ERR: instruction not supported by command station');
          Self.hist_err();      // -> call error
        end;
      end;//case msg.data[1]
    end;

    $62: begin  // track status
      if (msg.data[1] = $22) then begin
        i := (msg.data[2] AND $CB);
        Ftrk_status := TS_UNKNOWN;
        if (i = 0) then
         begin
          Ftrk_status := TS_ON;
          Self.WriteLog(2, 'GET: STATUS ON');
          Self.hist_ok();
         end;
        if ((i AND 3) > 0) then
         begin
          Ftrk_status := TS_OFF;
          Self.WriteLog(2, 'GET: STATUS OFF');
          Self.hist_ok();
         end;
        if (i = 8) then
         begin
          Ftrk_status := TS_SERVICE;
          Self.WriteLog(2, 'GET: STATUS SERVICE');
          Self.hist_ok();
         end;
       end else begin
        Self.WriteLog(1, 'GET ERR: Status - bad response!');
      end;
    end;

    $63: begin
      // command station version
      if (msg.data[1] = $21) then
       begin
        cs_version.major := (msg.data[2] shr 4) and $0F;   // major
        cs_version.minor := msg.data[2] and $0F;           // minor
        cs_version.id    := msg.data[3];                   // id
        Self.WriteLog(2, 'GET: CS VERSION '+IntToStr(cs_version.major)+'.'+IntToStr(cs_version.minor)+', ID: '+IntToStr(cs_version.id));
        Self.CSGotVersion(cs_version);
        Self.hist_ok();
       end;
    end;

    $E3: begin
      case (msg.data[1]) of
        $30: begin
          Self.WriteLog(4, Format('GET: Found: %d', [LokAddrDecode(msg.data[2], msg.data[3])]));
        end;
        $40: begin
          // somebody took control of our loco (loco has been stolen)
          Slot.adresa := Self.LokAddrDecode(msg.data[2], msg.data[3]);
          Self.WriteLog(4, 'GET: LOKO '+IntToStr(Slot.adresa)+' is being operated by another device');
          Self.ConnectChange(Slot.adresa, TConnect_code.TC_Stolen, nil);
        end;
        $50:begin
          // function status response
          Self.WriteLog(4, 'GET: FUNCTION STATUS F0-F12 - not supported');
        end;

        $52:begin
          // functon status F13-F28 response
          Self.WriteLog(4, 'GET: FUNCTION STATUS F13-F28 - not supported');
        end;
        else Self.WriteLog(4,'GET: function not supported in program');
      end;
    end;

    $E4:begin  // get lok speed info
      if (Self.Get.sp_addr = -1) then
        Exit;

      Slot.adresa := Self.Get.sp_addr;

      //pocet jizdnich kroku
      case (msg.data[1] and $7) of
        0: Slot.maxsp :=  14;
        1: Slot.maxsp :=  27;
        2: Slot.maxsp :=  28;
        4: Slot.maxsp := 128;
      end;//case

      Self.WriteLog(4, 'GET: LOKO STATUS, '+IntToStr(Slot.maxsp)+' speed steps');

      if ((msg.data[1] shr 3) and $1 = 1) then Slot.stolen := true else Slot.stolen := false;

      case ((msg.data[2] shr 7) and $1) of
       0: Slot.smer := 1;
       1: Slot.smer := 0;
      end;

      if ((msg.data[3] shr 4) and $1 = 1) then Slot.funkce[0] := true else Slot.funkce[0] := false;
      for i := 1 to 4 do if ((msg.data[3] shr (i-1)) and $1 = 1) then Slot.funkce[i] := true else Slot.funkce[i] := false;
      for i := 5 to 12 do if ((msg.data[4] shr (i-5)) and $1 = 1) then Slot.funkce[i] := true else Slot.funkce[i] := false;

      case (Slot.maxsp) of
       14: begin
            Slot.speed := (msg.data[2] AND $0F);
            if (Slot.speed = 1) then Slot.speed := 0;
            Slot.speed := (Slot.speed * 2);               // normovani rychlosti (28/14)=2
       end;

       27, 28: begin
            Slot.speed := ((msg.data[2] AND $0F) shl 1) OR ((msg.data[2] AND $10) shr 4);
            if ((Slot.speed >= 1) and (Slot.speed <= 3)) then Slot.speed := 0;
            if (Slot.speed >= 4) then Slot.speed := Slot.speed-3;
       end;

       128:begin
            Slot.speed := (msg.data[2] AND $7F);
            if (Slot.speed = 1) then Slot.speed := 0;
            Slot.speed := Round(Slot.speed * (28/128));   // normovani rychlosti
       end;

       else
        Self.WriteLog(4, 'GET: '+IntToStr(Slot.maxsp)+' speed steps - not supported');
        Exit;
      end;// case speed steps

      Slot.maxsp := 28;   // rychlost normujeme
      Self.WriteSpInfo(Slot);

      // prevezmeme loko tim, ze ji nastavime rychlost
      Self.LokSetSpeed(Slot.adresa, Slot.speed, Slot.smer);

      Self.Get.sp_addr := -1;
      Self.CheckLoading(Slot);

      Self.hist_ok();
    end;//acse $E4

    $E5:begin  // multitrack
      Self.WriteLog(4,'GET: LOKO STATUS multitrack - not supported');
    end;

    $E6:begin  // Double header
      Self.WriteLog(4,'GET: LOKO STATUS loko in Double Header - not supported');
    end;

  end;//case
end;//procedure

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.Send(buf : TBuffer);
var
  x: byte;
  i: integer;
  log:string;
begin
  if ((not Assigned(Self.ComPort.CPort)) or (not Self.ComPort.CPort.Connected)) then
   begin
    Self.WriteLog(1, 'PUT ERR: XpressNet not connected');
    Exit;
   end;
  if (buf.Count > 254) then
   begin
    Self.WriteLog(1, 'PUT ERR: Message too long');
    Exit;
   end;

  //xor
  x := 0;
  for i := 0 to buf.Count-1 do x := x xor buf.data[i];
  buf.Count := buf.Count + 1;
  buf.data[buf.Count-1] := x;

  //get string for log
  log := '';
  for i := 0 to buf.Count-1 do log := log + IntToHex(buf.data[i],2) + ' ';
  Self.WriteLog(4, 'PUT: '+log);

  try
    Self.ComPort.CPort.Write(buf.data, buf.Count);
  except
   on E : Exception do
    begin
     Self.WriteLog(1, 'PUT ERR: com object error : '+E.Message);
     if (Assigned(Self.FOnComError)) then Self.FOnComError(Self);
    end;
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.SendCommand(cmd: Tcmd; p1:Integer = 0; p2:Integer = 0; p3: Integer = 0; sent_times:Integer = 0);
var
  i: Integer;
  buf:TBuffer;
  log:TXpressNETHistoryPacket;
begin
  // zalogovat data pro timeout
  if (Self.send_history.Count < _MAX_HISTORY) then
   begin
    log.cmd := cmd;
    log.params[0] := p1;
    log.params[1] := p2;
    log.params[2] := p3;
    log.sent      := sent_times;
    log.time      := Now;
    log.callback_err := Self.callback_err;
    log.callback_ok  := Self.callback_ok;
    Self.send_history.Add(log);
   end;//if

  Self.callback_err.callback := nil;
  Self.callback_ok.callback  := nil;

  case (cmd) of
    XB_TRK_OFF:    Send(CreateBuf(#$21+#$80));
    XB_TRK_ON:     Send(CreateBuf(#$21+#$81));
    XB_TRK_STATUS: Send(CreateBuf(#$21+#$24));
    XB_TRK_CS_VERSION: Send(CreateBuf(#$21+#$21));
    XB_TRK_LI_VERSION: Send(CreateBuf(#$F0));

    XB_LOK_SET_SPD: begin // addr, speed, dir
      // pouzivame 28 jizdnich stupnu

      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $12;   // 28 speed steps
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      if ((p3 AND $20) = 0) then buf.data[4] := 0 else buf.data[4] := $80;
      case (p2) of
         0: p2 := 0;
         -1: p2 := 2;
         1..29: p2 := p2+3;
         else p2 := 0;
      end;
      buf.data[4] := buf.data[4] + ((p2 AND $1e) shr 1) + ((p2 AND $01) shl 4);
      Self.WriteLog(4, 'PUT SPEED: '+Format('addr=%d; speed=%d; dir=%d', [p1, p2, p3]));
      Send(buf);
    end;

    XB_LOK_SET_FUNC_0_4: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $20;
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := (p2 AND $1F);
      Send(buf);
    end;

    XB_LOK_SET_FUNC_5_8: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $21;
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := (p2 AND $0F);
      Send(buf);
    end;

    XB_LOK_SET_FUNC_9_12: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $22;
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := (p2 AND $0F);
      Send(buf);
    end;

    XB_LOK_SET_FUNC_13_20: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $23;
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := p2;
      Send(buf);
    end;

    XB_LOK_SET_FUNC_21_28: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $28;
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := p2;
      Send(buf);
    end;

    XB_LOK_GET_SPD: begin
      buf.Count := 4;
      buf.data[0] := $E3;
      buf.data[1] := $00;
      i := LokAddrEncode(p1);
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      Send(buf);
    end;

    XB_LOK_GET_FUNC: begin
      buf.Count := 4;
      buf.data[0] := $E3;
      buf.data[1] := $07;
      i := LokAddrEncode(p1);
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      Send(buf);
    end;

    XB_LOK_GET_FUNC_13_28: begin
      buf.Count := 4;
      buf.data[0] := $E3;
      buf.data[1] := $08;
      i := LokAddrEncode(p1);
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      Send(buf);
    end;

    XB_TRK_FINDLOK: begin
      buf.Count := 4;
      buf.data[0] := $E3;
      buf.data[1] := $05;
      buf.data[2] := p1;
      buf.data[3] := p2;
      Send(buf);
    end;

    XB_STOP_ALL:Send(CreateBuf(#$80+#$80));

    XB_LOK_STOP:begin
      buf.Count := 3;
      i := LokAddrEncode(p1);
      buf.data[0] := $92;
      buf.data[1] := Hi(i);
      buf.data[2] := Lo(i);
      Send(buf);
    end;//XB_LOK_STOP

    XB_POM_WRITEBYTE:begin
      i := LokAddrEncode(p1);
      buf.Count := 7;
      buf.data[0] := $E6;
      buf.data[1] := $30;
      buf.data[2] := hi(i);
      buf.data[3] := lo(i);
      buf.data[4] := $EC + (((p2-1) shr 8) and $03);
      buf.data[5] := ((p2-1) and $FF);
      buf.data[6] := p3;
      Send(buf);
    end;

  end;//case
end;//procedure


////////////////////////////////////////////////////////////////////////////////
function TXpressNET.LokSetSpeed(Address: Integer; speed: Integer; dir: Integer):Byte;
begin
 if (Address > 9999) then Exit(1);
 if (Address < 0) then Exit(2);

 if (dir = 0) then dir := -1;
 SendCommand(XB_LOK_SET_SPD , Address, speed, dir, 0);

 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.LokSetFunc(Address:Integer; sada:Byte; stav:Byte):Byte;
var func:Byte;
begin
 if (Address > 9999) then Exit(1);
 if (Address < 0) then Exit(2);

 case (sada) of
  0:begin
     func := (stav shr 1) AND $F;
     func := func OR ((stav AND $1) shl 4);
     SendCommand(XB_LOK_SET_FUNC_0_4, Address, func);
    end;//case 0
  1:begin
     func := (stav and $F);
     SendCommand(XB_LOK_SET_FUNC_5_8, Address, func);
    end;
  2:begin
     func := (stav and $F);
     SendCommand(XB_LOK_SET_FUNC_9_12, Address, func);
    end;//case 2
  3:begin
     func := stav;
     SendCommand(XB_LOK_SET_FUNC_13_20, Address, func);
    end;//case 2
  4:begin
     func := stav;
     SendCommand(XB_LOK_SET_FUNC_21_28, Address, func);
    end;//case 2
 end;//case

 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.SetTrackStatus(NewTrackStatus: Ttrk_status);
begin
  case NewTrackStatus of
    TS_OFF : SendCommand(XB_TRK_OFF);
    TS_ON  : SendCommand(XB_TRK_ON);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.CreateBuf(str:ShortString):TBuffer;
var i:Integer;
begin
 Result.Count := Length(str);
 for i := 0 to Result.Count-1 do
   Result.data[i] := ord(str[i+1]);
end;//function

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.EmergencyStop();
begin
 Self.SendCommand(XB_STOP_ALL);
end;//function

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.EmergencyStopLoko(addr:Integer);
begin
 Self.SendCommand(XB_LOK_STOP, addr);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.GetLocomotiveInfo(Address:Integer):Byte;
begin
 if (Address > 9999) then Exit(1);
 if (Address < 0) then Exit(2);

 Self.Get.sp_addr := Address;
 Self.SendCommand(XB_LOK_GET_SPD, Address);

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////
//timto prevezmu vozidlo - tady se vpodstate nic nedeje, jen ven reknu, ze si
//HV muzou vzit
function TXpressNET.Lok2MyControl(Address:Integer):Byte;
begin
 //zeptame se na informace o lokomotive
 //tato fce se postara, ze po prichodu dat dojde k zavolani eventu ConnectChange

 Self.loading_addr := Address;
 Self.GetLocomotiveInfo(Address);

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////
function TXpressNET.LokFromMyControl(Address:Integer):Byte;
begin
 //zavolame rovnou callback
 Self.ConnectChange(Address, TConnect_code.TC_Disconnected, Self.callback_ok.data);

 if (Assigned(Self.callback_ok.callback)) then
  Self.callback_ok.callback(Self, Self.callback_ok.data);

 Self.callback_err.callback := nil;
 Self.callback_ok.callback  := nil;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

//kdyz mi prijde INFO o loko, zkontroluje se, jestli o ni nahodou ted nezadam
// pokud o ni zadam, volam event
// u XpressNetu je jasne, ze kdyz mi prijdou data, tak je masinka moje
// callback je zde vlastne jen pro osetreni neodpovezeni centraly
procedure TXpressNET.CheckLoading(Slot:TSlot);
begin
 if (Self.loading_addr = Slot.adresa) then
  begin
   Self.loading_addr := 0;
   if (Self.send_history.Count > 0) then
     // zavolame s daty z callbacku
     Self.ConnectChange(Slot.adresa, TConnect_code.TC_Connected, Self.send_history[0].callback_ok.data)
   else
     // zavolame s prazdnymi daty
     Self.ConnectChange(Slot.adresa, TConnect_code.TC_Connected, nil);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.OnTimer_history(Sender:TObject);
begin
 if ((not Assigned(Self.ComPort.CPort)) or (not Self.ComPort.CPort.Connected)) then
  begin
   // pri ukonceni komunikace vymazeme historii
   if (Self.send_history.Count > 0) then
     Self.send_history.Clear();
   Exit();
  end;

 if (Self.send_history.Count > 0) then
   if ((Self.send_history[0].time + EncodeTime(0, 0, Self._TIMEOUT_MSEC div 1000, Self._TIMEOUT_MSEC mod 1000)) < Now) then
     Self.hist_send(0);
end;//procedure

procedure TXpressNET.hist_send(index:Integer);
var data:TXpressNETHistoryPacket;
begin
 if (Self.send_history[index].sent > _SEND_MAX) then
  begin
   // prekrocen timeout
   data := Self.send_history[index];
   Self.send_history.Delete(index);
   WriteLog(1, 'ERR: SEND TIMEOUT');

   case (data.cmd) of
    // tyto prikazy informuji system o primem vypadku lokomotivy
    XB_LOK_SET_SPD, XB_LOK_SET_FUNC_0_4, XB_LOK_SET_FUNC_5_8, XB_LOK_SET_FUNC_9_12,
    XB_LOK_SET_FUNC_13_20, XB_LOK_SET_FUNC_21_28,
    XB_LOK_GET_SPD, XB_LOK_GET_FUNC, XB_POM_WRITEBYTE : begin
      Self.LokComError(data.params[0]);
    end;
   end;//case

   if (Assigned(data.callback_err.callback)) then
    data.callback_err.callback(Self, data.callback_err.data);
  end else begin
   // odeslat data znovu
   WriteLog(2, 'WARN: SENDING AGAIN');
   data := Self.send_history[0];
   Self.send_history.Delete(0);

   Self.callback_err := data.callback_err;
   Self.callback_ok  := data.callback_ok;
   Self.SendCommand(data.cmd, data.params[0], data.params[1], data.params[2], data.sent+1);
  end;

end;//procedure

procedure TXpressNET.hist_ok();
var data:TXpressNETHistoryPacket;
begin
 if (Self.send_history.Count > 0) then
  begin
   data := Self.send_history[0];
   Self.send_history.Delete(0);      // odpoved na data - smazat z historie

   if (Self.send_history.Count = 0) then
    Self.WriteLog(5, 'BUF TIMEOUT EMPTY');

   case (data.cmd) of
    // tyto prikazy informuji system o primem vypadku lokomotivy
    XB_LOK_SET_SPD, XB_LOK_SET_FUNC_0_4, XB_LOK_SET_FUNC_5_8, XB_LOK_SET_FUNC_9_12,
    XB_LOK_SET_FUNC_13_20, XB_LOK_SET_FUNC_21_28,
    XB_LOK_GET_SPD, XB_LOK_GET_FUNC, XB_POM_WRITEBYTE : begin
Self.LokComOK(data.params[0]);
    end;
   end;//case

   if (Assigned(data.callback_ok.callback)) then
     data.callback_ok.callback(Self, data.callback_ok.data);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.hist_err();
var data:TXpressNETHistoryPacket;
begin
 if (Self.send_history.Count > 0) then
  begin
   data := Self.send_history[0];
   Self.send_history.Delete(0);      // odpoved na data - smazat z historie

   if (Self.send_history.Count = 0) then Self.WriteLog(5, 'BUF TIMEOUT REMOVE');
   if (Assigned(data.callback_err.callback)) then data.callback_err.callback(Self, data.callback_err.data);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.GetTrackStatus();
begin
 Self.WriteLog(4, 'PUT: GET-TRACK-STATUS');
 Self.SendCommand(XB_TRK_STATUS);;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.GetCSVersion(callback:TCSVersionEvent);
begin
 inherited;
 Self.WriteLog(4, 'PUT: GET-CS-VERSION');
 Self.SendCommand(XB_TRK_CS_VERSION);
end;//procedure

procedure TXpressNET.GetLIVersion(callback:TLIVersionEvent);
begin
 inherited;
 Self.WriteLog(4, 'PUT: GET-LI-VERSION');
 Self.SendCommand(XB_TRK_LI_VERSION);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.AfterOpen();
begin
 inherited;
 Self.ComPort.CPort.OnRxChar := Self.DataReceive;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.POMWriteCV(Address:Integer; cv:Word; data:byte);
begin
 Self.WriteLog(4, 'PUT: POM '+IntToStr(cv)+':'+IntToStr(data));
 Self.SendCommand(XB_POM_WRITEBYTE, address, cv, data);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.BeforeClose();
begin
 Self.send_history.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
