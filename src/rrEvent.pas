unit rrEvent;

{
  Trida TRREv definuje udalost na kolejisti, na kterou je mozne reagovat.

  Typy udalosti:
   - obsazeni/uvolneni casti useku
   - zmena stavu IR cidla
   - uplynuti urcite doby

  Definicni string udalosti:
   - usek: "typ;state;part"
   - IR: "typ;state;irid"
   - cas: "typ;timeSec"
}

interface

uses Classes, SysUtils, Prevody, StrUtils;

type
  TRREvType = (rrtUsek = 1, rrtIR = 2, rrtTime = 3);

  TRREvData = record
    case typ : TRREvType of
      rrtUsek: (
        usekPart: Cardinal;
        usekState: boolean;
      );

      rrtIR: (
        irId: Cardinal;
        irState: boolean;
      );

      rrtTime: (
        time: TTime;
      );
  end;

  TRREvState = record
    triggerTime: TDateTime;
    enabled: boolean;
  end;

  TRREv = class
   private
    m_data: TRREvData;
    m_state: TRREvState;

     procedure LoadFromDefStr(data: string);

   public

     constructor Create(data: string); overload;
     constructor Create(data: TRREvData); overload;

     function GetDefStr():string;

     procedure Register();
     procedure Unregister();

     // Sender must be a valid "Usek" blok.
     function IsTriggerred(Sender:TObject; safeState: boolean):boolean;

     property enabled: boolean read m_state.enabled;
     property data: TRREvData read m_data;
     property typ: TRREvType read m_data.typ;

  end;

implementation

uses TBloky, TBlok, TBlokIR, TBlokUsek;

////////////////////////////////////////////////////////////////////////////////

constructor TRREv.Create(data: string);
begin
 inherited Create();
 Self.m_state.enabled := false;
 LoadFromDefStr(data);
end;

constructor TRREv.Create(data: TRREvData);
begin
 inherited Create();
 Self.m_state.enabled := false;
 Self.m_data := data;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRREv.LoadFromDefStr(data: string);
var strs:TStrings;
    tmpTime:Cardinal;
begin
 strs := TStringList.Create();

 try
   ExtractStrings([';'], [], PChar(data), strs);

   Self.m_data.typ := TRREvType(StrToInt(strs[0]));

   case (Self.m_data.typ) of
     rrtUsek: begin
       Self.m_data.usekState := PrevodySoustav.StrToBool(strs[1]);
       Self.m_data.usekPart := StrToInt(strs[2]);
     end;

     rrtIR: begin
       Self.m_data.irState := PrevodySoustav.StrToBool(strs[1]);
       Self.m_data.irId := StrToInt(strs[2]);
     end;

     rrtTime: begin
       if (StrToIntDef(strs[1], -1) <> -1) then
        begin
         tmpTime := StrToInt(strs[1]);
         Self.m_data.time := EncodeTime(0, tmpTime div 60, tmpTime mod 60, 0);
        end else begin
         Self.m_data.time := EncodeTime(0, StrToInt(LeftStr(strs[1], 2)),
               StrToInt(Copy(strs[1], 4, 2)), StrToInt(RightStr(strs[1], 1)));
        end;
     end;
   end;// m_data.typ
 finally
   strs.Free();
 end;
end;

function TRREv.GetDefStr():string;
begin
 Result := IntToStr(Integer(Self.m_data.typ)) + ';';

 case (Self.m_data.typ) of
   rrtUsek: Result := Result + IntToStr(PrevodySoustav.BoolToInt(m_data.usekState)) + ';' +
              IntToStr(m_data.usekPart);

   rrtIR  : Result := Result + IntToStr(PrevodySoustav.BoolToInt(m_data.irState)) + ';' +
              IntToStr(m_data.irId);

   rrtTime: Result := Result + FormatDateTime('nn:ss.z', m_data.time);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRREv.Register();
begin
 if (Self.m_data.typ = rrtTime) then
   Self.m_state.triggerTime := Now + m_data.time;

 Self.m_state.enabled := true;
end;

procedure TRREv.Unregister();
begin
 Self.m_state.enabled := false;
end;

////////////////////////////////////////////////////////////////////////////////

function TRREv.IsTriggerred(Sender:TObject; safeState: boolean):boolean;
var Blk:TBlk;
begin
 if (not Self.enabled) then Exit(false);

 try
   case (Self.m_data.typ) of
     rrtUsek: Result := ((TBlkUsek(Sender).Stav.StavAr[m_data.usekPart] = TUsekStav.obsazeno) and (m_data.usekState)) or
                       ((TBlkUsek(Sender).Stav.StavAr[m_data.usekPart] = TUsekStav.uvolneno) and (not m_data.usekState));

     rrtIR: begin
       Blky.GetBlkByID(m_data.irId, Blk);
       if (Blk = nil) then Exit(safeState);
       if (Blk.typ <> _BLK_IR) then Exit(safeState);
       Result := ((TBlkIR(Blk).Stav = TIRStav.obsazeno) and (m_data.irState)) or
                 ((TBlkIR(Blk).Stav = TIRStav.uvolneno) and (not m_data.irState));
     end;

     rrtTime: begin
       Result := (Now >= Self.m_state.triggerTime);
     end;
   else
     Result := safeState;
   end;

 except
   Result := safeState;
 end;

end;

////////////////////////////////////////////////////////////////////////////////

end.
