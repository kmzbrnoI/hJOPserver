unit SnadnSpusteni;

// Implementace podpory krabicky snadneho spusteni.

interface

uses Classes, IniFiles, StrUtils, SysUtils;

type
  TSSData = record
   enabled:boolean;
   MtbAdr:Smallint;                                  //adresa mtb
   AutRezim:Smallint;                                //pri klepnuti na zelene tlacitko spustit aut rezim...
   IN_Start:Smallint;                                //tlacitko na zapnuti
   IN_Pause:Smallint;                                //tlacitko na pozastaveni
   IN_Stop:Smallint;                                 //talcitko na vypnuti
   IN_Repeat:Smallint;                               //tlacitko na opakovani
   IN_Reset:Smallint;                                //tlacitko na reset
   OUT_Ready:Smallint;                               //indikace pripraveno
   OUT_Start:Smallint;                               //indikace spusteno
   OUT_Pause:Smallint;                               //indikace pozastaveno
   OUT_Stop:Smallint;                                //indikace vypnuto
   OUT_Repeat:Smallint;                              //indikace opakovani
  end;

  TSS=class                            //adresy krabicky snadne spusteni
   private
    config:TSSData;
    RepeatDown:Boolean;

   public

     procedure Update();

     //filesystem i/o
     procedure LoadData(ini:TMemIniFile);
     procedure SaveData(ini:TMemIniFile);

     function GetData():TSSData;
     procedure SetData(data:TSSData);
  end;

var
  SS : TSS;

implementation

uses GetSystems, TBlok, AC, TechnologieMTB, fMain, TBloky,
     RPConst, Logging, ACDatabase, Prevody;

////////////////////////////////////////////////////////////////////////////////

procedure TSS.LoadData(ini:TMemIniFile);
begin
 Self.config.enabled  := ini.ReadBool('Snadne Spusteni', 'enabled',false);
 Self.config.MtbAdr   := ini.ReadInteger('Snadne Spusteni', 'MtbAdr',-1);
 Self.config.AutRezim := ini.ReadInteger('Snadne Spusteni', 'AutRezim',-1);

 Self.config.IN_Start       := ini.ReadInteger('Snadne Spusteni', 'IN_Start',-1);
 Self.config.IN_Pause       := ini.ReadInteger('Snadne Spusteni', 'IN_Pause',-1);
 Self.config.IN_Stop        := ini.ReadInteger('Snadne Spusteni', 'IN_Stop',-1);
 Self.config.IN_Repeat      := ini.ReadInteger('Snadne Spusteni', 'IN_Repeat',-1);
 Self.config.IN_Reset       := ini.ReadInteger('Snadne Spusteni', 'IN_Reset',-1);

 Self.config.OUT_Ready       := ini.ReadInteger('Snadne Spusteni', 'OUT_Ready',-1);
 Self.config.OUT_Start       := ini.ReadInteger('Snadne Spusteni', 'OUT_Start',-1);
 Self.config.OUT_Pause       := ini.ReadInteger('Snadne Spusteni', 'OUT_Pause',-1);
 Self.config.OUT_Stop        := ini.ReadInteger('Snadne Spusteni', 'OUT_Stop',-1);
 Self.config.OUT_Repeat      := ini.ReadInteger('Snadne Spusteni', 'OUT_Repeat',-1);
end;//procedure

procedure TSS.SaveData(ini:TMemIniFile);
begin
 ini.WriteBool('Snadne Spusteni', 'enabled',Self.config.enabled);
 ini.WriteInteger('Snadne Spusteni', 'MtbAdr',Self.config.MtbAdr);
 ini.WriteInteger('Snadne Spusteni', 'AutRezim',Self.config.AutRezim);

 ini.WriteInteger('Snadne Spusteni', 'IN_Start',Self.config.IN_Start);
 ini.WriteInteger('Snadne Spusteni', 'IN_Pause',Self.config.IN_Pause);
 ini.WriteInteger('Snadne Spusteni', 'IN_Stop',Self.config.IN_Stop);
 ini.WriteInteger('Snadne Spusteni', 'IN_Repeat',Self.config.IN_Repeat);
 ini.WriteInteger('Snadne Spusteni', 'IN_Reset',Self.config.IN_Reset);

 ini.WriteInteger('Snadne Spusteni', 'OUT_Ready',Self.config.OUT_Ready);
 ini.WriteInteger('Snadne Spusteni', 'OUT_Start',Self.config.OUT_Start);
 ini.WriteInteger('Snadne Spusteni', 'OUT_Stop',Self.config.OUT_Stop);
 ini.WriteInteger('Snadne Spusteni', 'OUT_Repeat',Self.config.OUT_Repeat);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSS.Update();
var AC:TAC;
 begin
  if ((not Self.config.enabled) or (not GetFunctions.GetSystemStart)
      or (Self.config.AutRezim >= ACDb.ACs.Count)) then Exit;

  AC := ACDb.ACs[Self.config.AutRezim];


  //vstupy:

  if ((Self.config.IN_Start > -1) and (not AC.running) and (AC.ready)
    and (MTB.GetInput(Self.config.MtbAdr,Self.config.IN_Start) > 0)) then
      AC.Start();

  if ((Self.config.IN_Pause > -1) and (AC.running)
    and (MTB.GetInput(Self.config.MtbAdr,Self.config.IN_Pause) > 0)) then
      AC.Pause();

  if ((Self.config.IN_Stop > -1) and (AC.running)
    and (MTB.GetInput(Self.config.MtbAdr,Self.config.IN_Stop) > 0)) then
      AC.Stop();

  if (Self.config.IN_Repeat > -1) then
   begin
    if (MTB.GetInput(Self.config.MtbAdr,Self.config.IN_Repeat) = 1) then
     begin
      if (not Self.RepeatDown) then
       begin
        Self.RepeatDown := true;
        AC.repeating := not AC.repeating;
       end;//if not Self.config.RepeatDown
     end else begin
      if (Self.RepeatDown) then Self.RepeatDown := false;
     end;
   end;//if IN_Repeat_Used

  if ((Self.config.IN_Reset > -1) and (MTB.GetInput(Self.config.MtbAdr,Self.config.IN_Reset) > 0)) then
   begin
    // reset zatim neimplementovan
   end;


  // vystupy;

  if (Self.config.OUT_Ready > -1) then
    MTB.SetOutput(Self.config.MtbAdr, Self.config.OUT_Ready, PrevodySoustav.BoolToInt((not AC.running) and (AC.ready)));

  if (Self.config.OUT_Start > -1) then
    MTB.SetOutput(Self.config.MtbAdr, Self.config.OUT_Start, PrevodySoustav.BoolToInt(AC.running));

  if (Self.config.OUT_Pause > -1) then
    MTB.SetOutput(Self.config.MtbAdr, Self.config.OUT_Pause, PrevodySoustav.BoolToInt((not AC.running) and (AC.ACKrok > -1)));

  if (Self.config.OUT_Stop > -1) then
    MTB.SetOutput(Self.config.MtbAdr, Self.config.OUT_Stop, PrevodySoustav.BoolToInt(not AC.running));

  if (Self.config.OUT_Repeat > -1) then
    MTB.SetOutput(Self.config.MtbAdr, Self.config.OUT_Repeat, PrevodySoustav.BoolToInt(AC.repeating));

end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TSS.GetData():TSSData;
begin
 Result := Self.config;
end;//function

procedure TSS.SetData(data:TSSData);
begin
 Self.config := data;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
 SS := TSS.Create();

finalization
 SS.Free;

end.//unit
