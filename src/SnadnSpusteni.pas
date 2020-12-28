unit SnadnSpusteni;

// Implementace podpory krabicky snadneho spusteni.

interface

uses Classes, IniFiles;

const
  _INIFILE_SECTION = 'Snadne Spusteni';

type
  TSSData = record
   enabled: Boolean;
   RCSAdr: Smallint;
   AC_id: Integer;
   IN_Start: Smallint;
   IN_Pause: Smallint;
   IN_Stop: Smallint;
   IN_Repeat: Smallint;
   IN_Reset: Smallint;
   OUT_Ready: Smallint;
   OUT_Start: Smallint;
   OUT_Pause: Smallint;
   OUT_Stop: Smallint;
   OUT_Repeat: Smallint;
  end;

  TSS = class
   private
    config: TSSData;
    RepeatDown: Boolean;

   public

     procedure Update();

     procedure LoadData(ini: TMemIniFile);
     procedure SaveData(ini: TMemIniFile);

     function GetData(): TSSData;
     procedure SetData(data: TSSData);
  end;

var
  SS: TSS;

implementation

uses TechnologieRCS, TBloky, ownConvert, RCS, TBlokAC, TBlok;

////////////////////////////////////////////////////////////////////////////////

procedure TSS.LoadData(ini: TMemIniFile);
begin
 Self.config.enabled := ini.ReadBool(_INIFILE_SECTION, 'enabled', false);
 Self.config.RCSAdr := ini.ReadInteger(_INIFILE_SECTION, 'RCSAdr', -1);
 if (Self.config.RCSAdr = -1) then //backward compatibility
   Self.config.RCSAdr := ini.ReadInteger(_INIFILE_SECTION, 'MtbAdr', -1);
 Self.config.AC_id := ini.ReadInteger(_INIFILE_SECTION, 'AutRezim', -1);

 Self.config.IN_Start := ini.ReadInteger(_INIFILE_SECTION, 'IN_Start', -1);
 Self.config.IN_Pause := ini.ReadInteger(_INIFILE_SECTION, 'IN_Pause', -1);
 Self.config.IN_Stop := ini.ReadInteger(_INIFILE_SECTION, 'IN_Stop', -1);
 Self.config.IN_Repeat := ini.ReadInteger(_INIFILE_SECTION, 'IN_Repeat', -1);
 Self.config.IN_Reset := ini.ReadInteger(_INIFILE_SECTION, 'IN_Reset', -1);

 Self.config.OUT_Ready := ini.ReadInteger(_INIFILE_SECTION, 'OUT_Ready', -1);
 Self.config.OUT_Start := ini.ReadInteger(_INIFILE_SECTION, 'OUT_Start', -1);
 Self.config.OUT_Pause := ini.ReadInteger(_INIFILE_SECTION, 'OUT_Pause', -1);
 Self.config.OUT_Stop := ini.ReadInteger(_INIFILE_SECTION, 'OUT_Stop', -1);
 Self.config.OUT_Repeat := ini.ReadInteger(_INIFILE_SECTION, 'OUT_Repeat', -1);
end;

procedure TSS.SaveData(ini: TMemIniFile);
begin
 ini.WriteBool(_INIFILE_SECTION, 'enabled', Self.config.enabled);
 ini.WriteInteger(_INIFILE_SECTION, 'RCSAdr', Self.config.RCSAdr);
 ini.WriteInteger(_INIFILE_SECTION, 'AutRezim', Self.config.AC_id);

 ini.WriteInteger(_INIFILE_SECTION, 'IN_Start', Self.config.IN_Start);
 ini.WriteInteger(_INIFILE_SECTION, 'IN_Pause', Self.config.IN_Pause);
 ini.WriteInteger(_INIFILE_SECTION, 'IN_Stop', Self.config.IN_Stop);
 ini.WriteInteger(_INIFILE_SECTION, 'IN_Repeat', Self.config.IN_Repeat);
 ini.WriteInteger(_INIFILE_SECTION, 'IN_Reset', Self.config.IN_Reset);

 ini.WriteInteger(_INIFILE_SECTION, 'OUT_Ready', Self.config.OUT_Ready);
 ini.WriteInteger(_INIFILE_SECTION, 'OUT_Start', Self.config.OUT_Start);
 ini.WriteInteger(_INIFILE_SECTION, 'OUT_Stop', Self.config.OUT_Stop);
 ini.WriteInteger(_INIFILE_SECTION, 'OUT_Repeat', Self.config.OUT_Repeat);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSS.Update();
var blk: TBlk;
    AC: TBlkAC;
 begin
  if ((not Self.config.enabled) or (not RCSi.NoExStarted())) then Exit();

  Blky.GetBlkById(Self.config.AC_id, blk);
  if ((blk = nil) or (blk.typ <> btAC)) then Exit();
  AC := TBlkAC(blk);

  if (not RCSi.IsNonFailedModule(Self.config.RCSAdr)) then Exit();

  // inputs

  try
    if ((Self.config.IN_Start > -1) and (not AC.running) and (AC.clientConnected)
      and (RCSi.GetInput(Self.config.RCSAdr, Self.config.IN_Start) = isOn)) then
        AC.Start();

    if ((Self.config.IN_Pause > -1) and (AC.running)
      and (RCSi.GetInput(Self.config.RCSAdr, Self.config.IN_Pause) = isOn)) then
        AC.Pause();

    if ((Self.config.IN_Stop > -1) and (AC.running)
      and (RCSi.GetInput(Self.config.RCSAdr, Self.config.IN_Stop) = isOn)) then
        AC.Stop();

    if (Self.config.IN_Repeat > -1) then
     begin
      if (RCSi.GetInput(Self.config.RCSAdr, Self.config.IN_Repeat) = isOn) then
       begin
        if (not Self.RepeatDown) then
         begin
          Self.RepeatDown := true;
          // TODO: change repeating
          // AC.repeating := not AC.repeating;
         end;
       end else begin
        if (Self.RepeatDown) then Self.RepeatDown := false;
       end;
     end;//if IN_Repeat_Used

    if ((Self.config.IN_Reset > -1) and (RCSi.GetInput(Self.config.RCSAdr, Self.config.IN_Reset) = isOn)) then
     begin
      // TODO: reset
     end;


    // outputs

    if (Self.config.OUT_Ready > -1) then
      RCSi.SetOutput(Self.config.RCSAdr, Self.config.OUT_Ready, ownConvert.BoolToInt((not AC.running) and (AC.clientConnected)));

    if (Self.config.OUT_Start > -1) then
      RCSi.SetOutput(Self.config.RCSAdr, Self.config.OUT_Start, ownConvert.BoolToInt(AC.running));

    if (Self.config.OUT_Pause > -1) then
      RCSi.SetOutput(Self.config.RCSAdr, Self.config.OUT_Pause, ownConvert.BoolToInt((not AC.running) and (not AC.paused)));

    if (Self.config.OUT_Stop > -1) then
      RCSi.SetOutput(Self.config.RCSAdr, Self.config.OUT_Stop, ownConvert.BoolToInt(not AC.running));

    // TODO: repeating
    { if (Self.config.OUT_Repeat > -1) then
      RCSi.SetOutput(Self.config.RCSAdr, Self.config.OUT_Repeat, ownConvert.BoolToInt(AC.repeating)); }

  except

  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TSS.GetData(): TSSData;
begin
 Result := Self.config;
end;

procedure TSS.SetData(data: TSSData);
begin
 Self.config := data;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 SS := TSS.Create();

finalization
 SS.Free;

end.//unit
