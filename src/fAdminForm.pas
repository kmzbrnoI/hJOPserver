unit fAdminForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, Gauges, ExtCtrls, TechnologieJC,
  TBlokTrat;

type
  TJCSimulator = class
    private

     procedure OnTimer(Sender:TObject);
     procedure UpdateJC(JC:TJC);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TTratSimulator = class
    private

     procedure OnTimer(Sender:TObject);
     procedure UpdateTrat(Trat:TBlkTrat);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TVyhSimulator = class
    private

     procedure OnTimer(Sender:TObject);

    public
     timer:TTimer;

      constructor Create();
      destructor Destroy(); override;
  end;

  TF_Admin = class(TForm)
    B_InputSim: TButton;
    B_Save: TButton;
    CHB_SimIntellibox: TCheckBox;
    CHB_SimInput: TCheckBox;
    CHB_SimSoupravaUsek: TCheckBox;
    CHB_SystemStart: TCheckBox;
    B_DCC: TButton;
    CHB_JC_Simulator: TCheckBox;
    CHB_Trat_Sim: TCheckBox;
    CHB_SimVyhybky: TCheckBox;
    procedure B_SaveClick(Sender: TObject);
    procedure B_InputSimClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CHB_JC_SimulatorClick(Sender: TObject);
    procedure CHB_Trat_SimClick(Sender: TObject);
    procedure CHB_SimVyhybkyClick(Sender: TObject);
  public
   procedure LoadData;
   procedure SaveData;
  end;

var
  F_Admin: TF_Admin;
  JCSimulator : TJCSimulator;
  TratSimulator : TTratSimulator;
  VyhSimulator : TVyhSimulator;

implementation

uses fMain, RPConst, TechnologieMTB, FileSystem, fSettings, Trakce,
     Logging, THVDatabase, TJCDatabase, TBlok, TBlokUsek,
    TBloky, TBlokSCom, GetSystems, TBlokVyhybka;

{$R *.dfm}

procedure TF_Admin.LoadData();
 begin
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);

  CHB_SimInput.Checked          := Konfigurace.ini.ReadBool('AdminData', 'InputSim', false);
  CHB_SimIntellibox.Checked     := Konfigurace.ini.ReadBool('AdminData', 'IntelliboxSim', false);
  CHB_SimSoupravaUsek.Checked   := Konfigurace.ini.ReadBool('AdminData', 'SoupravaUsekSim', false);
  CHB_SystemStart.Checked       := Konfigurace.ini.ReadBool('AdminData', 'SystemStart', false);
  CHB_JC_Simulator.Checked      := Konfigurace.ini.ReadBool('AdminData', 'JCsim', false);
  CHB_SimVyhybky.Checked        := Konfigurace.ini.ReadBool('AdminData', 'VYHsim', false);
  Self.CHB_JC_SimulatorClick(Self.CHB_JC_Simulator);

  CHB_Trat_Sim.Checked          := Konfigurace.ini.ReadBool('AdminData', 'TRATsim', false);
  Self.CHB_Trat_SimClick(Self.CHB_Trat_Sim);


  if (Konfigurace.ini.ReadBool('AdminData','show', false)) then
    Self.Show();

 end;//procedure

procedure TF_Admin.SaveData();
 begin
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);
  Konfigurace.ini.WriteInteger('AdminData','FormLeft',F_Admin.Left);
  Konfigurace.ini.WriteInteger('AdminData','FormTop',F_Admin.Top);
  Konfigurace.ini.WriteBool('AdminData','InputSim',CHB_SimInput.Checked);
  Konfigurace.ini.WriteBool('AdminData','IntelliboxSim',CHB_SimIntellibox.Checked);
  Konfigurace.ini.WriteBool('AdminData','SoupravaUsekSim',CHB_SimSoupravaUsek.Checked);
  Konfigurace.ini.WriteBool('AdminData','SystemStart',CHB_SystemStart.Checked);
  Konfigurace.ini.WriteBool('AdminData', 'JCsim', CHB_JC_Simulator.Checked);
  Konfigurace.ini.WriteBool('AdminData', 'TRATsim', CHB_Trat_Sim.Checked);
  Konfigurace.ini.WriteBool('AdminData', 'VYHsim', CHB_SimVyhybky.Checked);

  Konfigurace.ini.UpdateFile;
  Konfigurace.ini.Free;
 end;//procedure

procedure TF_Admin.B_SaveClick(Sender: TObject);
 begin
  SaveData;
 end;

procedure TF_Admin.CHB_JC_SimulatorClick(Sender: TObject);
begin
 if (Self.CHB_JC_Simulator.Checked) then
  JCSimulator.timer.Enabled := true
 else
  JCSimulator.timer.Enabled := false;
end;

procedure TF_Admin.CHB_SimVyhybkyClick(Sender: TObject);
begin
 if (Self.CHB_SimVyhybky.Checked) then
  VyhSimulator.timer.Enabled := true
 else
  VyhSimulator.timer.Enabled := false;
end;

procedure TF_Admin.CHB_Trat_SimClick(Sender: TObject);
begin
 if (Self.CHB_Trat_Sim.Checked) then
  TratSimulator.timer.Enabled := true
 else
  TratSimulator.timer.Enabled := false;
end;

procedure TF_Admin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (Self.Visible) then
  begin
   Self.FormStyle := fsNormal;
   Self.Visible := false;
  end;//if Self.Visible
end;

procedure TF_Admin.FormShow(Sender: TObject);
begin
  Konfigurace.ini := TMemIniFile.Create(F_Options.E_dataload.Text);

  F_Admin.Left := Konfigurace.ini.ReadInteger('AdminData','FormLeft', F_Admin.Left);
  F_Admin.Top  := Konfigurace.ini.ReadInteger('AdminData','FormTop', F_Admin.Top);

  Konfigurace.ini.Free;
end;//procedure

procedure TF_Admin.B_InputSimClick(Sender: TObject);
 begin
  MTB.InputSim;
  writelog('Proveden InputSim',WR_MTB);
 end;//procedure

////////////////////////////////////////////////////////////////////////////////
// simulator obsazovani useku v jizdni ceste
////////////////////////////////////////////////////////////////////////////////

constructor TJCSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 2000;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
end;//procedure

destructor TJCSimulator.Destroy();
begin
 if (Assigned(Self.timer)) then
   FreeAndNil(Self.timer);
 inherited Destroy();
end;//procedure

procedure TJCSimulator.OnTimer(Sender:TObject);
var i:Integer;
    JC:TJC;
begin
 if (not GetFunctions.GetSystemStart()) then Exit;

 for i := 0 to JCDb.Count-1 do
  begin
   JC := JCDb.GetJCByIndex(i);
   if (JC.stav.RozpadBlok > -1) then
     Self.UpdateJC(JC);
  end;
end;//procedure

procedure TJCSimulator.UpdateJC(JC:TJC);
var i:Integer;
    Blk, Nav:TBlk;
    UsekSet:TBlkUsekSettings;
begin
 if (JC.stav.RozpadRuseniBlok = 0) then
  begin
   Blky.GetBlkByID(JC.data.NavestidloBlok, Nav);
   Blky.GetBlkByID((Nav as TBlkSCom).UsekID, Blk);

   if ((Blk as TBlkUsek).Stav.Stav = TUsekStav.obsazeno) then
    begin
     UsekSet := (Blk as TBlkUsek).GetSettings();
     for i := 0 to UsekSet.MTBAddrs.Count-1 do
      MTB.SetInput(UsekSet.MTBAddrs.data[i].board, UsekSet.MTBAddrs.data[i].port, 0);
     Exit();
    end;
  end;//uvolnit usek pred navestidlem

 if (((JC.stav.RozpadBlok-JC.stav.RozpadRuseniBlok >= 2) and (JC.stav.RozpadRuseniBlok >= 0)) or (JC.stav.RozpadBlok = JC.data.Useky.Count)) then
  begin
   // uvolnit RozpadRuseniBlok
   Blky.GetBlkByID(JC.data.Useky[JC.stav.RozpadRuseniBlok], Blk);
   UsekSet := (Blk as TBlkUsek).GetSettings();
   for i := 0 to UsekSet.MTBAddrs.Count-1 do
    MTB.SetInput(UsekSet.MTBAddrs.data[i].board, UsekSet.MTBAddrs.data[i].port, 0);
  end else begin
   // obsadit RopadBlok
   Blky.GetBlkByID(JC.data.Useky[JC.stav.RozpadBlok], Blk);
   UsekSet := (Blk as TBlkUsek).GetSettings();
   if (UsekSet.MTBAddrs.Count > 0) then
    MTB.SetInput(UsekSet.MTBAddrs.data[0].board, UsekSet.MTBAddrs.data[0].port, 1);
  end;//else

end;//procedure

////////////////////////////////////////////////////////////////////////////////
// simulator obsazovani trati
////////////////////////////////////////////////////////////////////////////////

constructor TTratSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 2000;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
end;//procedure

destructor TTratSimulator.Destroy();
begin
 if (Assigned(Self.timer)) then
   FreeAndNil(Self.timer);
 inherited Destroy();
end;//procedure

procedure TTratSimulator.OnTimer(Sender:TObject);
var i:Integer;
    Blk:TBlk;
begin
 if (not GetFunctions.GetSystemStart()) then Exit;

 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, Blk);
   if (Blk.GetGlobalSettings().typ <> _BLK_TRAT) then continue;
   if (((Blk as TBlkTrat).stav.BP.next > -1) and ((Blk as TBlkTrat).Obsazeno)) then
     Self.UpdateTrat(Blk as TBlkTrat);
  end;
end;//procedure

procedure TTratSimulator.UpdateTrat(Trat:TBlkTrat);
var Usek:TBlk;
    TratSet:TBlkTratSettings;
    UsekSet:TBlkUsekSettings;
    i:Integer;
begin
 TratSet := Trat.GetSettings();

 if (Trat.stav.BP.next-Trat.stav.BP.last >= 2) then
  begin
   // zrusit obsazeni posledniho useku
   case (trat.Smer) of
    TTratSmer.AtoB: Blky.GetBlkByID(TratSet.Useky[Trat.stav.BP.last], Usek);
    TTratSmer.BtoA: Blky.GetBlkByID(TratSet.Useky[Length(TratSet.Useky)-Trat.stav.BP.last-1], Usek);
   end;//case

   UsekSet := (Usek as TBlkUsek).GetSettings();
   for i := 0 to UsekSet.MTBAddrs.Count-1 do
    MTB.SetInput(UsekSet.MTBAddrs.data[i].board, UsekSet.MTBAddrs.data[i].port, 0);

  end else begin
   if (Trat.stav.BP.next >= Length(TratSet.Useky)) then Exit();

   // obsadit dalsi usek
   case (trat.Smer) of
    TTratSmer.AtoB: Blky.GetBlkByID(TratSet.Useky[Trat.stav.BP.next], Usek);
    TTratSmer.BtoA: Blky.GetBlkByID(TratSet.Useky[Length(TratSet.Useky)-Trat.stav.BP.next-1], Usek);
   end;//case

   UsekSet := (Usek as TBlkUsek).GetSettings();
   if (UsekSet.MTBAddrs.Count > 0) then
    MTB.SetInput(UsekSet.MTBAddrs.data[0].board, UsekSet.MTBAddrs.data[0].port, 1);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// simulator staveni vyhybek
////////////////////////////////////////////////////////////////////////////////

constructor TVyhSimulator.Create();
begin
 inherited Create();

 Self.timer := TTimer.Create(nil);
 Self.timer.Interval := 500;
 Self.timer.Enabled  := false;
 Self.timer.OnTimer  := Self.OnTimer;
end;//procedure

destructor TVyhSimulator.Destroy();
begin
 if (Assigned(Self.timer)) then
   FreeAndNil(Self.timer);
 inherited Destroy();
end;//procedure

procedure TVyhSimulator.OnTimer(Sender:TObject);
var i:Integer;
    blk:TBlk;
begin
 if (not GetFunctions.GetSystemStart()) then Exit;

 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, blk);
   if (blk.GetGlobalSettings().typ <> _BLK_VYH) then continue;
   if (((blk as TBlkVyhybka).StaveniPlus) or ((blk as TBlkVyhybka).StaveniMinus)) then
    begin
     // po 1 sekunde nastavime vstup aktualni polohy na 0
     if (((blk as TBlkVyhybka).Stav.poloha_real <> TVyhPoloha.none) and ((blk as TBlkVyhybka).Stav.staveniStart+EncodeTime(0, 0, 1, 0) < Now)) then
      begin
       if ((blk as TBlkVyhybka).StaveniPlus) then
        MTB.SetInput((blk as TBlkVyhybka).GetSettings.MTBAddrs.data[1].board, (blk as TBlkVyhybka).GetSettings.MTBAddrs.data[1].port, 0)
       else
        MTB.SetInput((blk as TBlkVyhybka).GetSettings.MTBAddrs.data[0].board, (blk as TBlkVyhybka).GetSettings.MTBAddrs.data[0].port, 0);
      end;//if koncova poloha

     // po 3 sekundach oznamime koncovou polohu
     if ((blk as TBlkVyhybka).Stav.staveniStart+EncodeTime(0, 0, 3, 0) < Now) then
      begin
       if ((blk as TBlkVyhybka).StaveniPlus) then
        MTB.SetInput((blk as TBlkVyhybka).GetSettings.MTBAddrs.data[0].board, (blk as TBlkVyhybka).GetSettings.MTBAddrs.data[0].port, 1)
       else
        MTB.SetInput((blk as TBlkVyhybka).GetSettings.MTBAddrs.data[1].board, (blk as TBlkVyhybka).GetSettings.MTBAddrs.data[1].port, 1);
      end;//if koncova poloha

     Exit();
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  JCSimulator   := TJCSimulator.Create();
  TratSimulator := TTratSimulator.Create();
  VyhSimulator  := TVYhSimulator.Create();

finalization
  FreeAndNil(JCSimulator);
  FreeAndNil(TratSimulator);
  FreeAndNil(VyhSimulator);

end.//unit