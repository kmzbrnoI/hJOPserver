unit ModelovyCas;

{
  Tato unta se stara o modelovy cas.

  Modelovy cas je odesilan jako broadcast vsem panelum (nikoliv oblastem rizeni
  - viz specifikace Protokolu).

  Modelovy cas pocita server i klient separatne. Kazde 3 minuty odesle server
  informaci o aktualnim modelovem casu a tak provede synchonizaci casu serveru
  a klienta.
}

interface

uses Classes, SysUtils, IniFiles, IDContext, Graphics;

type

  TModCasMTB=record                                   //data o pinech modeloveho casu
   MtbAdr:Integer;                                     //Mtb adresa na vysupy hodin

   PORT_H:SmallInt;                                    //port na kazdou hodinu, -1 pokud nevyuzito
   PORT_M:SmallInt;                                    //port na kazdou minutu, -1 pokud nevyuzito
   PORT_S:SmallInt;                                    //port na kazdou sekundu, -1 pokud nevyuzito
  end;

  TModCas = class
   private const
    _INI_SECTION  = 'ModCas';
    _SYNC_REAL_MINUTES = 3;

   private
    ftime:TTime;
    fspeed:Real;
    fstarted:boolean;
    fused:boolean;
    last_sync:TDateTime;
    last_call:TDateTime;

    procedure SetTime(time:TTime); overload;
    procedure SetSpeed(speed:Real);
    procedure SetStarted(started:boolean);
    procedure SetUsed(used:boolean);

    function GetStrSpeed():string;
    procedure SetStrSpeed(speed:string);

    procedure BroadcastTime();

   public
    MTBdata:TModCasMTB;

     procedure LoadData(var ini:TMemIniFile);
     procedure SaveData(var ini:TMemIniFile);

     procedure Update();

     procedure SendTimeToPanel(AContext:TIDContext);
     procedure SetTime(time:TTime; speed:Real); overload;
     procedure UpdateGUIColors();

     property time:TTime read ftime write SetTime;
     property speed:Real read fspeed write SetSpeed;
     property started:boolean read fstarted write SetStarted;
     property used:boolean read fused write SetUsed;
     property strSpeed:string read GetStrSpeed write SetStrSpeed;
  end;//class TModCas

var
  ModCas:TModCas;

implementation

uses TCPServerOR, fMain;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.LoadData(var ini:TMemIniFile);
begin
 with (Self.MTBdata) do
  begin
   MtbAdr := ini.ReadInteger(_INI_SECTION, 'MtbAdr', -1);
   PORT_H := ini.ReadInteger(_INI_SECTION, 'PORT_Hodiny', -1);
   PORT_M := ini.ReadInteger(_INI_SECTION, 'PORT_Minuty', -1);
   PORT_S := ini.ReadInteger(_INI_SECTION, 'PORT_Sekundy', -1);
  end;

 Self.fspeed := ini.ReadFloat('ModCas', 'speed', 5);
 Self.time     := StrToTime(ini.ReadString('ModCas', 'cas', '00:00:00'));
 Self.fused    := ini.ReadBool('ModCas', 'used', true);
end;//procedure

procedure TModCas.SaveData(var ini:TMemIniFile);
begin
 with (Self.MTBdata) do
  begin
   ini.WriteInteger(_INI_SECTION, 'MtbAdr', MtbAdr);
   ini.WriteInteger(_INI_SECTION, 'PORT_Hodiny', PORT_H);
   ini.WriteInteger(_INI_SECTION, 'PORT_Minuty', PORT_M);
   ini.WriteInteger(_INI_SECTION, 'PORT_Sekundy', PORT_S);
  end;

 ini.WriteString('ModCas', 'speed', Self.strSpeed);
 ini.WriteString('ModCas', 'cas', TimeToStr(Self.time));
 ini.WriteBool('ModCas', 'used', Self.used);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetTime(time:TTime);
begin
 if ((Self.ftime <> time) and (not Self.started)) then
  begin
   Self.ftime := time;
   Self.BroadcastTime();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetSpeed(speed:Real);
begin
 if ((Self.fspeed <> speed) and (not Self.started)) then
  begin
   Self.fspeed := speed;
   Self.BroadcastTime();
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetStarted(started:boolean);
begin
 if (started <> Self.fstarted) then
  begin
   Self.fstarted := started;
   Self.BroadcastTime();
   Self.UpdateGUIColors();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetUsed(used:boolean);
begin
 if (used <> Self.fused) then
  begin
   Self.fused := used;
   Self.BroadcastTime();
   Self.UpdateGUIColors();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.Update();
var diff:TTime;
begin
 if (not Self.started) then
  begin
   Self.last_call := Now;
   Exit();
  end;

 // pocitani aktualniho modeloveho casu:
 diff := Now - Self.last_call;
 Self.ftime := Self.ftime + (diff*Self.speed);

 // synchronizace casu (po "_SYNC_REAL_MINUTES" case odesilame aktualni cas klientovi)
 if (Now > Self.last_sync+EncodeTime(0, _SYNC_REAL_MINUTES, 0, 0)) then
  Self.BroadcastTime();

 // zobrazit aktualni cas do F_Main
 F_Main.P_Time_modelovy.Caption := TimeToStr(Self.time);

 Self.last_call := Now;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.BroadcastTime();
begin
 Self.last_sync := Now;

 F_Main.P_Time_modelovy.Caption := TimeToStr(Self.time);
 F_Main.P_Zrychleni.Caption := strSpeed + '×';
 F_Main.CheckNasobicWidth();

 if (Self.started) then
   ORTCPServer.BroadcastData('-;MOD-CAS;1;'+Self.strSpeed+';'+TimeToStr(Self.time))
 else
   ORTCPServer.BroadcastData('-;MOD-CAS;0;'+Self.strSpeed+';'+TimeToStr(Self.time));
end;//procedure

procedure TModCas.SendTimeToPanel(AContext:TIDContext);
begin
 if (Self.started) then
   ORTCPServer.SendLn(AContext, '-;MOD-CAS;1;'+Self.strSpeed+';'+TimeToStr(Self.time))
 else
   ORTCPServer.SendLn(AContext, '-;MOD-CAS;0;'+Self.strSpeed+';'+TimeToStr(Self.time));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetTime(time:TTime; speed:Real);
begin
 if (((Self.ftime <> time) or (Self.fspeed <> speed)) and (not Self.started)) then
  begin
   Self.ftime  := time;
   Self.fspeed := speed;
   Self.BroadcastTime();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TModCas.GetStrSpeed():string;
begin
 Result := FloatToStrF(Self.speed, ffGeneral, 1, 1);
end;

procedure TModCas.SetStrSpeed(speed:string);
begin
 Self.SetSpeed(StrToFloat(speed));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.UpdateGUIColors();
begin
 if (Self.started) then
  begin
   F_Main.P_Time_modelovy.Font.Color := clBlack;
   F_Main.P_Zrychleni.Font.Color     := clBlack;
  end else begin
   F_Main.P_Time_modelovy.Font.Color := clRed;
   F_Main.P_Zrychleni.Font.Color     := clRed;
  end;

 if (Self.used) then
  begin
   F_Main.P_Time_modelovy.Color := clSkyBlue;
   F_Main.P_Zrychleni.Color := clSkyBlue;
  end else begin
   F_Main.P_Time_modelovy.Color := clSilver;
   F_Main.P_Zrychleni.Color := clSilver;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 ModCas := TModCas.Create();

finalization
 FreeAndNil(ModCas);

end.//unit
