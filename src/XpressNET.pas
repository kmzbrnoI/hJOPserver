unit XpressNET;

// trida TXpressNet slouzi ke komunikaci s centralou zapomoci serioveho portu
// jeji funkce je prekladadni prikazu mezi programem a centralou (resp. XpressNETem)

// pouzivame vzdy 28 jizdnich stupnu

interface

uses
  SysUtils, Classes, StrUtils, CPort, Trakce, Math, Generics.Collections,
  ExtCtrls, Forms;

type
  // zprava
  //  maximalni delka pro rychlost omezena na astronomickou hodnotu 256 bytu
  TBuffer = record
   data:array [0..255] of Byte;
   Count:Integer;
  end;

  // mozne prikazy z PC do centraly:
  Tcmd = (
    XB_TRK_OFF = 10,
    XB_TRK_ON  = 11,
    XB_TRK_STATUS = 12,
    XB_TRK_CS_VERSION = 13,
    XB_TRK_LI_VERSION = 14,
    XB_LOK_SET_SPD = 15,

    XB_LOK_SET_FUNC_0_4 = 20,
    XB_LOK_SET_FUNC_5_8 = 21,
    XB_LOK_SET_FUNC_9_12 = 22,
    XB_LOK_SET_FUNC_13_20 = 23,
    XB_LOK_SET_FUNC_21_28 = 24,

    XB_LOK_SET_LOCK_13_20 = 34,

    XB_LOK_GET_INFO = 50,
    XB_LOK_GET_FUNC = 51,
    XB_LOK_GET_FUNC_13_28 = 52,
    XB_LOK_STOP = 53,
    XB_STOP_ALL = 54,
    XB_TRK_FINDLOK = 55,
    XB_POM_WRITEBYTE = 56
  );


  TParseMsgEvent = procedure(Sender: TObject; msg: TBuffer;
                              var Handled: boolean) of object;

  TXpressNETHistoryPacket = record                                              // prvek vystupniho bufferu
   cmd: Tcmd;                                                                     // prikaz
   params:array [0..2] of Integer;                                                // parametry prikazu
   time:TDateTime;                                                                // cas odeslani
   sent:Integer;                                                                  // kolikrat odeslan
   callback_err:TCommandCallback;                                                 // chybovy callback
   callback_ok:TCommandCallback;                                                  // ok callback
  end;

  TXpressNET = class(TTrakce)
   private const
    _MAX_HISTORY  = 1024;                                                       // maximalni velikost vystupniho bufferu
    _HIST_CHECK_INTERVAL = 100;                                                 // perioda v ms, po ketre dochazi ke kontrole vystupniho bufferu
    _TIMEOUT_MSEC = 400;                                                        // timeout odpovedi na zpravu
                                                                                // TENTO TIMEOUT NEZKRACOVAT !! Pri timeoutu 200 ms delalo velke potize na starsich NanoX.
    _SEND_MAX     = 3;                                                          // Pocet odeslani prikazu do prohlaseni za neodeslatelny
                                                                                //  tzn. jedna zprava je pred prohlasenim za neodeslatelnou odeslana prave trikrat (poprve a pote 2x opakovane)
    _BUF_IN_TIMEOUT_MS = 300;                                                   // timeout vstupniho bufferu v ms (po uplynuti timeoutu dojde k vymazani bufferu) - DULEZITY SAMOOPRAVNY MECHANISMUS!
                                                                                // pro spravnou funkcnost musi byt < _TIMEOUT_MSEC
   private
    Fbuf_in: TBuffer;                                                           // vstupni buffer (data z centraly do PC)
    Fbuf_in_timeout:TDateTime;

    FOnParseMsg: TParseMsgEvent;                                                // event volany pri prijeti kompletni zpravy
    loading_addr:Word;                                                          // aktualni adresa lokomotivy, ktera se prebira
                                                                                // POZOR: v jeden okamzik lze prebirat nejvyse jednu lokomotivu

    send_history: TList<TXpressNETHistoryPacket>;                               // vystupni buffer (data z PC do centraly)
    timer_history:TTimer;                                                       // timer starajici se o vystupni buffer
    buf:TBuffer;                                                                // lokalni buffer pro vytvareni zprav, vypicke uziti pri vytvareni zpravy k odeslani

    function LokAddrEncode(addr: Integer): Word; inline;                        // ctyrmistna adresa lokomotivy do dvou bytu
    function LokAddrDecode(ah, al: byte): Integer; inline;                      // ctyrmistna adresa lokomotivy ze dvou bajtu do klasickeho cisla
    function LokAddrToBuf(addr: Integer): ShortString;                          // adresa to bufferu

    procedure DataReceive(Sender: TObject; Count: Integer);                     // event z objektu ComPortu o prijeti dat
    procedure ParseMsg(msg: TBuffer);                                           // lokalni parsing kompletni zpravy
    procedure ParseMsgTry(msg: TBuffer);                                        // rozhodunti o parsovatku zpravy (bud lokalni, nebo externi)

    procedure Send(buf: TBuffer);                                               // odesli prikaz do centraly
    procedure SendCommand(cmd: Tcmd; p1:Integer = 0; p2:Integer = 0;            // odesli konkretni prikaz dle terminologie XpressNETu do centraly
                          p3: Integer = 0; sent_times:Integer = 0);

    function CreateBuf(str:ShortString):TBuffer;                                // vytvor vystupni buffer na zaklde stringu (pouziva se pro jednoduche vytvareni zprav)

    procedure CheckLoading();                                                   // zkontroluj, jestli prave ten nacita lokomotiva

    procedure OnTimer_history(Sender:TObject);                                  // tick timer_history
    procedure hist_send(index:Integer);                                         // odesli data z historie (= vystupniho bufferu) na indexu \index
    procedure hist_ok();                                                        // na prvni data z vystupniho vufferu prisla odpoved, smaz data z bufferu
    procedure hist_err();                                                       // na pravni data z vystupniho bufferu neprisla odpoved, odesli data znova

    procedure CheckFbufInTimeout();

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
     procedure POMWriteCV(Address:Integer; cv:Word; data:byte); override;       // zapis CV \cv na hodnotu \data POMem


     procedure EmergencyStop(); override;                                       // nouzove zastav vsechny lokomotivy

     procedure AfterOpen(); override;                                           // event po otevreni komunikace
     procedure BeforeClose(); override;                                         // event pred zavrenim komunikace

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

function TXpressNET.LokAddrToBuf(addr: Integer):ShortString;
var encoded:Word;
begin
  encoded := Self.LokAddrEncode(addr);
  Result := AnsiChar(Hi(encoded)) + AnsiChar(Lo(encoded));
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
  // check timeout
  Self.CheckFbufInTimeout();

  Self.ComPort.CPort.Read(buf, Count);

  for i := 0 to Count-1 do Fbuf_in.data[Fbuf_in.Count+i] := Buf[i];
  Fbuf_in.Count := Fbuf_in.Count + Count;
  Fbuf_in_timeout := Now + EncodeTime(0, 0, _BUF_IN_TIMEOUT_MS div 1000, _BUF_IN_TIMEOUT_MS mod 1000);

  s := 'BUF: ';
  for i := 0 to Fbuf_in.Count-1 do s := s + IntToHex(Fbuf_in.data[i],2)+' ';
  WriteLog(5, s);

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
          // function F0-F12 status response
          Self.WriteLog(4, 'GET: FUNCTION STATUS F0-F12');

          Slot.funkce[0] := (((msg.data[3] shr 4) AND $01) = 1);
          for i := 0 to 3 do Slot.funkce[1+i] := (((msg.data[2] shr i) AND $01) = 1);
          for i := 0 to 7 do Slot.funkce[5+i] := (((msg.data[3] shr i) AND $01) = 1);

          Self.hist_ok();
        end;
        $52:begin
          // functon F13-F28 status response
          Self.WriteLog(4, 'GET: FUNCTION STATUS F13-F28');

          for i := 0 to 7 do Slot.funkce[13+i] := (((msg.data[2] shr i) AND $01) = 1);
          for i := 0 to 7 do Slot.funkce[21+i] := (((msg.data[3] shr i) AND $01) = 1);

          Self.hist_ok();
        end;

        else Self.WriteLog(4,'GET: function not supported in program');
      end;
    end;

    $E4:begin
      case (msg.data[1]) of
        $00..$0F: begin
              // lok response info

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

              Slot.funkce[0] := ((msg.data[3] shr 4) and $1 = 1);
              for i := 1 to 4 do Slot.funkce[i] := ((msg.data[3] shr (i-1)) and $1 = 1);
              for i := 5 to 12 do Slot.funkce[i] := ((msg.data[4] shr (i-5)) and $1 = 1);
              for i := 13 to _HV_FUNC_MAX do Slot.funkce[i] := false;
        

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

              // prevezmeme loko tim, ze ji nastavime rychlost
              Self.LokSetSpeed(Slot.adresa, Slot.speed, Slot.smer);

              Self.hist_ok();

              Self.Get.sp_addr := -1;
              Self.CheckLoading();
        end;//case 0..4
      end;
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
  asp: PAsync;
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
    InitAsync(asp);
    Self.ComPort.CPort.WriteAsync(buf.data, buf.Count, asp);
    while (not Self.ComPort.CPort.IsAsyncCompleted(asp)) do
     begin
      Application.ProcessMessages();
      Sleep(1);
     end;
  except
   on E : Exception do
    begin
     Self.WriteLog(1, 'PUT ERR: com object error : '+E.Message);
     if (Assigned(Self.FOnComError)) then Self.FOnComError(Self);
    end;
  end;

 DoneAsync(asp);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.SendCommand(cmd: Tcmd; p1:Integer = 0; p2:Integer = 0; p3: Integer = 0; sent_times:Integer = 0);
var
  i: Integer;
  log:TXpressNETHistoryPacket;
begin
  // zalogovat data pro timeout (zbytek logovani se provadi dole - az po odeslani dat, muze se totiz cekat na CTS)
  if (Self.send_history.Count < _MAX_HISTORY) then
   begin
    log.cmd := cmd;
    log.params[0] := p1;
    log.params[1] := p2;
    log.params[2] := p3;
    log.sent      := sent_times;
    log.callback_err := Self.callback_err;
    log.callback_ok  := Self.callback_ok;
   end;//if

  Self.callback_err.callback := nil;
  Self.callback_ok.callback  := nil;

  if (Self.send_history.Count < _MAX_HISTORY) then
   begin
    log.time := Now;
    Self.send_history.Add(log);
   end;

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

    XB_LOK_SET_FUNC_0_4, XB_LOK_SET_FUNC_5_8, XB_LOK_SET_FUNC_9_12: begin
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $20 + (Integer(cmd) - Integer(XB_LOK_SET_FUNC_0_4));
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := (p2 AND $1F);
      Send(buf);
    end;

    XB_LOK_SET_FUNC_13_20, XB_LOK_SET_FUNC_21_28: begin // addr, func
      buf.Count := 5;
      i := LokAddrEncode(p1);
      buf.data[0] := $E4;
      buf.data[1] := $23 + 5*(Integer(cmd) - Integer(XB_LOK_SET_FUNC_13_20));
      buf.data[2] := Hi(i);
      buf.data[3] := Lo(i);
      buf.data[4] := p2;
      Send(buf);
    end;

    XB_LOK_GET_INFO       : Send(CreateBuf(#$E3 + #$00 + LokAddrToBuf(p1)));
    XB_LOK_GET_FUNC       : Send(CreateBuf(#$E3 + #$07 + LokAddrToBuf(p1)));
    XB_LOK_GET_FUNC_13_28 : Send(CreateBuf(#$E3 + #$09 + LokAddrToBuf(p1)));
    XB_TRK_FINDLOK        : Send(CreateBuf(#$E3 + AnsiChar(#$05) + AnsiChar(byte(p1)) + AnsiChar(byte(p2))));
    XB_STOP_ALL           : Send(CreateBuf(#$80 + #$80));
    XB_LOK_STOP           : Send(CreateBuf(#$92 + LokAddrToBuf(p1)));

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

    XB_LOK_SET_LOCK_13_20 : Send(CreateBuf(#$E4 + #$27 + LokAddrToBuf(p1) + AnsiChar(byte(p2))));

  end;//case
end;//procedure


////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.LokSetSpeed(Address: Integer; speed: Integer; dir: Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 if (dir = 0) then dir := -1;
 SendCommand(XB_LOK_SET_SPD , Address, speed, dir, 0);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.LokSetFunc(Address:Integer; sada:Byte; stav:Byte);
var func:Byte;
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

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
procedure TXpressNET.LokEmergencyStop(addr:Integer);
begin
 Self.SendCommand(XB_LOK_STOP, addr);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.LokGetInfo(Address:Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 Self.Get.sp_addr := Address;
 Self.SendCommand(XB_LOK_GET_INFO, Address);
end;//function

////////////////////////////////////////////////////////////////////////////////
//timto prevezmu vozidlo - tady se vpodstate nic nedeje, jen ven reknu, ze si
//HV muzou vzit
procedure TXpressNET.Lok2MyControl(Address:Integer);
begin
 if ((Address < 0) or (Address > 9999)) then
    raise EInvalidAddress.Create('Invalid address');

 //zeptame se na informace o lokomotive
 //tato fce se postara, ze po prichodu dat dojde k zavolani eventu ConnectChange

 Self.loading_addr := Address;
 Self.LokGetInfo(Address);
end;//function

////////////////////////////////////////////////////////////////////////////////
procedure TXpressNET.LokFromMyControl(Address:Integer);
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

//kdyz mi prijde INFO o loko, zkontroluje se, jestli o ni nahodou ted nezadam
// pokud o ni zadam, volam event
// u XpressNetu je jasne, ze kdyz mi prijdou data, tak je masinka moje
// callback je zde vlastne jen pro osetreni neodpovezeni centraly
procedure TXpressNET.CheckLoading();
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
 if (Self.send_history[index].sent >= (_SEND_MAX-1)) then
  begin
   // prekrocen timeout
   data := Self.send_history[index];
   Self.send_history.Delete(index);
   WriteLog(1, 'ERR: SEND TIMEOUT');

   case (data.cmd) of
    // tyto prikazy informuji system o primem vypadku lokomotivy
    XB_LOK_SET_SPD, XB_LOK_SET_FUNC_0_4, XB_LOK_SET_FUNC_5_8, XB_LOK_SET_FUNC_9_12,
    XB_LOK_SET_FUNC_13_20, XB_LOK_SET_FUNC_21_28,
    XB_LOK_GET_INFO, XB_LOK_GET_FUNC, XB_POM_WRITEBYTE : begin
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
    XB_LOK_GET_INFO, XB_LOK_GET_FUNC, XB_POM_WRITEBYTE : begin
      Self.LokComOK(data.params[0]);
    end;
   end;//case

   if (Assigned(data.callback_ok.callback)) then
     data.callback_ok.callback(Self, data.callback_ok.data);
  end else begin
   WriteLog(2, 'WARN: HISTORY BUFFER UNDERFLOW (hist_ok)');
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
  end else begin
   WriteLog(2, 'WARN: HISTORY BUFFER UNDERFLOW (hist_err)');
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

procedure TXpressNET.LokGetFunctions(Address:Integer; startFunc:Integer);
begin
 case (startFunc) of
  0:  begin
       Self.WriteLog(4, 'PUT: GET-FUNC 0..12');
       Self.SendCommand(XB_LOK_GET_FUNC, Address);
      end;

  13: begin
       Self.WriteLog(4, 'PUT: GET-FUNC 13..28');
       Self.SendCommand(XB_LOK_GET_FUNC_13_28, Address);
      end;
 end;//case
end;

////////////////////////////////////////////////////////////////////////////////

procedure TXpressNET.CheckFbufInTimeout();
begin
 if ((Self.Fbuf_in_timeout < Now) and (Self.Fbuf_in.Count > 0)) then
  begin
   WriteLog(1, 'INPUT BUFFER TIMEOUT, removing buffer');
   Self.Fbuf_in.Count := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
