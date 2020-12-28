﻿unit ModelovyCas;

{
  Tato unta se stara o modelovy cas.

  Modelovy cas je odesilan jako broadcast vsem panelum (nikoliv oblastem rizeni
  - viz specifikace Protokolu).

  Modelovy cas pocita server i klient separatne. Kazde 3 minuty odesle server
  informaci o aktualnim modelovem casu a tak provede synchonizaci casu serveru
  a klienta.

  Modelovy cas pri zapnuti hJOPserveru ma nastvene datum na 0 (pravdepodobne
  UNIX origin), jak cas pribyva, datum take pribyva. To je dulezite zejmena
  pri prechodu pres pulnoc (kvuli predvidanym odjezdum).

  Trida TModCas rozlisuje Date a Time a DateTime.
   * DateTime obsahuje den i cas.
   * Date obsahuje jen den a cas nastaveny na pulnoc.
   * Time obsahuje jen cas a den nastaveny na 0.

  Nektere metody ocekavaji cas, jine DateTime, tak pozor na to!
}

interface

uses Classes, SysUtils, IniFiles, IDContext, Graphics;

type
  TModCas = class
   private const
    _INI_SECTION  = 'ModCas';
    _SYNC_REAL_MINUTES = 3;

   private
    fdateTime:TDateTime;
    fspeed:Real;
    fstarted:Boolean;
    fused:Boolean;
    last_sync:TDateTime;
    last_call:TDateTime;

    procedure SetDateTime(dt:TDateTime);
    procedure mSetTime(time:TTime);
    procedure SetSpeed(speed:Real);
    procedure SetStarted(started:Boolean);
    procedure SetUsed(used:Boolean);

    function GetStrSpeed():string;
    procedure SetStrSpeed(speed:string);

    procedure BroadcastTime();
    function GetTCPString():string;
    function GetDate():TDate;
    function GetTime():TTime;

   public

     constructor Create();

     procedure LoadData(var ini:TMemIniFile);
     procedure SaveData(var ini:TMemIniFile);

     procedure Update();

     procedure SendTimeToPanel(AContext:TIDContext);
     procedure SetTime(time:TTime; speed:Real);
     procedure UpdateGUIColors();
     procedure Parse(parsed:TStrings);

     property dateTime:TDateTime read fdateTime write SetDateTime;
     property date:TDate read GetDate;
     property time:TTime read GetTime write mSetTime;
     property speed:Real read fspeed write SetSpeed;
     property started:Boolean read fstarted write SetStarted;
     property used:Boolean read fused write SetUsed;
     property strSpeed:string read GetStrSpeed write SetStrSpeed;
  end;//class TModCas

var
  ModCas:TModCas;

implementation

uses TCPServerOR, fMain, TrainDb, TBloky;

////////////////////////////////////////////////////////////////////////////////

constructor TModCas.Create();
begin
 Self.fdateTime := 0;
 Self.fspeed := 3;
 Self.fused := false;
 Self.fstarted := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.LoadData(var ini:TMemIniFile);
begin
 Self.fspeed := ini.ReadFloat('ModCas', 'speed', 5);
 Self.dateTime := StrToTime(ini.ReadString('ModCas', 'cas', '00:00:00'));
 Self.fused := ini.ReadBool('ModCas', 'used', true);
end;

procedure TModCas.SaveData(var ini:TMemIniFile);
begin
 ini.WriteString('ModCas', 'speed', Self.strSpeed);
 ini.WriteString('ModCas', 'cas', TimeToStr(Self.time));
 ini.WriteBool('ModCas', 'used', Self.used);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetDateTime(dt:TDateTime);
begin
 if ((Self.fdateTime <> dt) and (not Self.started)) then
  begin
   Self.fdateTime := dt;
   Self.BroadcastTime();
  end;
end;

procedure TModCas.mSetTime(time:TTime);
begin
 if ((Self.fdateTime <> Self.date + time) and (not Self.started)) then
  begin
   Self.fdateTime := Self.date + time;
   Self.BroadcastTime();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetSpeed(speed:Real);
begin
 if ((Self.fspeed <> speed) and (not Self.started)) then
  begin
   Self.fspeed := speed;
   Self.BroadcastTime();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetStarted(started:Boolean);
begin
 if (started <> Self.fstarted) then
  begin
   Self.fstarted := started;
   Self.BroadcastTime();
   Self.UpdateGUIColors();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetUsed(used:Boolean);
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
 Self.fdateTime := Self.fdateTime + (diff*Self.speed);

 // synchronizace casu (po "_SYNC_REAL_MINUTES" case odesilame aktualni cas klientovi)
 if (Now > Self.last_sync+EncodeTime(0, _SYNC_REAL_MINUTES, 0, 0)) then
  Self.BroadcastTime();

 // zobrazit aktualni cas do F_Main
 F_Main.P_Time_modelovy.Caption := TimeToStr(Self.time);

 Self.last_call := Now;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.BroadcastTime();
begin
 Self.last_sync := Now;

 F_Main.P_Time_modelovy.Caption := TimeToStr(Self.time);
 F_Main.P_Zrychleni.Caption := strSpeed + '×';
 F_Main.CheckNasobicWidth();

 ORTCPServer.BroadcastData(Self.GetTCPString());
end;

procedure TModCas.SendTimeToPanel(AContext:TIDContext);
begin
 ORTCPServer.SendLn(AContext, Self.GetTCPString());
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.SetTime(time:TTime; speed:Real);
begin
 if (((Self.time <> time) or (Self.fspeed <> speed)) and (not Self.started)) then
  begin
   Self.fspeed := speed;
   Self.fdateTime := Self.date + time;
   Self.BroadcastTime();
  end;
end;

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

function TModCas.GetTCPString():string;
begin
 if (Self.started) then
  ORTCPServer.BroadcastData('-;MOD-CAS;1;'+Self.strSpeed+';'+TimeToStr(Self.time))
 else begin
  if (Self.used) then
   ORTCPServer.BroadcastData('-;MOD-CAS;0;'+Self.strSpeed+';'+TimeToStr(Self.time))
  else
   ORTCPServer.BroadcastData('-;MOD-CAS;0;'+Self.strSpeed+';'+TimeToStr(Self.time)+';0');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TModCas.Parse(parsed:TStrings);
begin
 parsed[2] := UpperCase(parsed[2]);

 if ((parsed[2] = 'START') or (parsed[2] = 'STOP')) then
   Self.fstarted := (parsed[2] = 'START');

 if ((parsed.Count >= 4) and (parsed[2] = 'TIME')) then
  begin
   ModCas.SetTime(StrToTime(parsed[3]), StrToFloat(parsed[4]));
   if (parsed.Count >= 6) then
    begin
     if (Self.fused <> (parsed[5] = '1')) then
      begin
       // Nejdriv smazeme pres bloky, pripadne pochybne stavy resime natvrdo
       // smazanim u soupravy. Mazani pres bloky zajisti volani Change().
       Blky.ClearPOdj();
       Trains.ClearPOdj();
      end;

     Self.fused := (parsed[5] = '1');
    end;
  end;

 Self.BroadcastTime();
 Self.UpdateGUIColors();
end;

////////////////////////////////////////////////////////////////////////////////

function TModCas.GetDate():TDate;
var dt: TDateTime;
begin
 dt := Self.dateTime;
 ReplaceTime(dt, 0);
 Result := dt;
end;

function TModCas.GetTime():TTime;
var dt: TDateTime;
begin
 dt := Self.dateTime;
 ReplaceDate(dt, 0);
 Result := dt;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 ModCas := TModCas.Create();

finalization
 FreeAndNil(ModCas);

end.//unit
