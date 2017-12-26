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

   PORT_Hodiny:SmallInt;                               //port na kazdou hodinu, -1 pokud nevyuzito
   PORT_Minuty:SmallInt;                               //port na kazdou minutu, -1 pokud nevyuzito
   PORT_Sekundy:SmallInt;                              //port na kazdou sekundu, -1 pokud nevyuzito
  end;

  TModCas = class
   private const
    _INI_SECTION  = 'ModCas';
    _SYNC_REAL_MINUTES = 3;

   private
    ftime:TTime;
    fnasobic:Real;
    fstarted:boolean;
    last_sync:TDateTime;
    last_call:TDateTime;

    procedure SetTime(time:TTime); overload;
    procedure SetNasobic(nasobic:Real);
    procedure SetStarted(started:boolean);

    function GetStrNasobic():string;
    procedure SetStrNasobic(nasobic:string);

    procedure BroadcastTime();

   public
    MTBdata:TModCasMTB;

     procedure LoadData(var ini:TMemIniFile);
     procedure SaveData(var ini:TMemIniFile);

     procedure Update();

     procedure SendTimeToPanel(AContext:TIDContext);
     procedure SetTime(time:TTime; nasobic:Real); overload;

     property time:TTime read ftime write SetTime;
     property nasobic:Real read fnasobic write SetNasobic;
     property started:boolean read fstarted write SetStarted;
     property strNasobic:string read GetStrNasobic write SetStrNasobic;
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
   MtbAdr       := ini.ReadInteger(_INI_SECTION, 'MtbAdr', -1);
   PORT_Hodiny  := ini.ReadInteger(_INI_SECTION, 'PORT_Hodiny', -1);
   PORT_Minuty  := ini.ReadInteger(_INI_SECTION, 'PORT_Minuty', -1);
   PORT_Sekundy := ini.ReadInteger(_INI_SECTION, 'PORT_Sekundy', -1);
  end;

 Self.nasobic := ini.ReadFloat('ModCas', 'nasobic', 5);
 Self.time    := StrToTime(ini.ReadString('ModCas', 'cas', '00:00:00'));
end;//procedure

procedure TModCas.SaveData(var ini:TMemIniFile);
begin
 with (Self.MTBdata) do
  begin
   ini.WriteInteger(_INI_SECTION, 'MtbAdr', MtbAdr);
   ini.WriteInteger(_INI_SECTION, 'PORT_Hodiny', PORT_Hodiny);
   ini.WriteInteger(_INI_SECTION, 'PORT_Minuty', PORT_Minuty);
   ini.WriteInteger(_INI_SECTION, 'PORT_Sekundy', PORT_Sekundy);
  end;

 ini.WriteString('ModCas', 'nasobic', Self.strNasobic);
 ini.WriteString('ModCas', 'cas', TimeToStr(Self.time));
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

procedure TModCas.SetNasobic(nasobic:Real);
begin
 if ((Self.fnasobic <> nasobic) and (not Self.started)) then
  begin
   Self.fnasobic := nasobic;
   Self.BroadcastTime();
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetStarted(started:boolean);
begin
 if (started <> Self.fstarted) then
  begin
   Self.fstarted   := started;
   Self.BroadcastTime();
  end;

 if (started) then
  begin
   F_Main.P_Time_modelovy.Font.Color := clBlack;
   F_Main.P_Zrychleni.Font.Color     := clBlack;
  end else begin
   F_Main.P_Time_modelovy.Font.Color := clRed;
   F_Main.P_Zrychleni.Font.Color     := clRed;
  end;
end;//procedure

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
 Self.ftime := Self.ftime + (diff*Self.nasobic);

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
 F_Main.P_Zrychleni.Caption := strNasobic + '×';
 F_Main.CheckNasobicWidth();

 if (Self.started) then
   ORTCPServer.BroadcastData('-;MOD-CAS;1;'+Self.strNasobic+';'+TimeToStr(Self.time))
 else
   ORTCPServer.BroadcastData('-;MOD-CAS;0;'+Self.strNasobic+';'+TimeToStr(Self.time));
end;//procedure

procedure TModCas.SendTimeToPanel(AContext:TIDContext);
begin
 if (Self.started) then
   ORTCPServer.SendLn(AContext, '-;MOD-CAS;1;'+Self.strNasobic+';'+TimeToStr(Self.time))
 else
   ORTCPServer.SendLn(AContext, '-;MOD-CAS;0;'+Self.strNasobic+';'+TimeToStr(Self.time));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetTime(time:TTime; nasobic:Real);
begin
 if (((Self.ftime <> time) or (Self.fnasobic <> nasobic)) and (not Self.started)) then
  begin
   Self.ftime    := time;
   Self.fnasobic := nasobic;
   Self.BroadcastTime();
  end;
end;//procedure


////////////////////////////////////////////////////////////////////////////////

function TModCas.GetStrNasobic():string;
begin
 Result := FloatToStrF(Self.nasobic, ffGeneral, 1, 1);
end;

procedure TModCas.SetStrNasobic(nasobic:string);
begin
 Self.SetNasobic(StrToFloat(nasobic));
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 ModCas := TModCas.Create();

finalization
 FreeAndNil(ModCas);

end.//unit
