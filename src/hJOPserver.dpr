//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     HHHHH            H
//H                                                        H      H      H    H   H    H    H          H H
//H   Název : hJOPserver                                   H      H      H   H     H   H     H        H   H
//H   Jméno tvùrce : Jan Horáèek                           H      HHHHHHHH   H     H   H    H        H     H
//H   Autorská práva : Jan Horáèek 2008-2016               H      H      H   H     H   HHHHH        HHHHHHHHH
//H                                                        H      H      H   H     H   H HH        H         H
//H                                                        H      H      H    H   H    H   HH     H           H
//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     H     HH   H           H
//Zaèátek tvorby : záøí 2008

// JCL_DEBUG_EXPERT_INSERTJDBG ON
program hJOPserver;

uses
  Forms,
  Windows,
  SysUtils,
  RPConst in 'RPConst.pas',
  fTester in 'fTester.pas' {F_Tester},
  fMain in 'fMain.pas' {F_Main},
  fSettings in 'fSettings.pas' {F_Options},
  fLicence in 'fLicence.pas' {F_Licence},
  fNastaveni_Casu in 'fNastaveni_Casu.pas' {F_ModCasSet},
  fRegulator in 'fRegulator.pas' {F_DigiReg},
  fSplash in 'fSplash.pas' {F_splash},
  fAbout in 'fAbout.pas' {F_About},
  fACEdit in 'fACEdit.pas' {F_AutRezEdit},
  fBlkPrejezd in 'fBlkPrejezd.pas' {F_BlkPrejezd},
  Verze in 'Verze.pas',
  fLoginPozadi in 'fLoginPozadi.pas' {F_Pozadi},
  fZesilovacEdit in 'fZesilovacEdit.pas' {F_ZesilovacEdit},
  fBlkTrat in 'fBlkTrat.pas' {F_BlkTrat},
  fHVEdit in 'fHVEdit.pas' {F_HVEdit},
  fSystemInfo in 'fSystemInfo.pas' {F_SystemInfo},
  fBlkUsek in 'fBlkUsek.pas' {F_BlkUsek},
  fBlkVyhybka in 'fBlkVyhybka.pas' {F_BlkVyhybka},
  fBlkIR in 'fBlkIR.pas' {F_BlkIR},
  TBlokPrejezd in 'TBlokPrejezd.pas',
  fBlkNew in 'fBlkNew.pas' {F_BlkNew},
  fAdminForm in 'fAdminForm.pas' {F_Admin},
  fNoRegRun in 'fNoRegRun.pas' {F_NoRegRun},
  fJCEdit in 'fJCEdit.pas' {F_JCEdit},
  fRychlostiEdit in 'fRychlostiEdit.pas' {F_RychlostiEdit},
  fSystemAutoStart in 'fSystemAutoStart.pas' {F_AutoStartSystems},
  fBlkUsekSysVars in 'fBlkUsekSysVars.pas' {F_BlkUsek_tech},
  GetSystems in 'GetSystems.pas',
  Prevody in 'Prevody.pas',
  TechnologieMTB in 'TechnologieMTB.pas',
  TechnologieJC in 'TechnologieJC.pas',
  FileSystem in 'FileSystem.pas',
  fConsole in 'fConsole.pas' {F_Console},
  Trakce in 'Trakce.pas',
  XpressNET in 'XpressNET.pas',
  TrakceGUI in 'TrakceGUI.pas',
  Outputdriver in 'Outputdriver.pas',
  TBlok in 'TBlok.pas',
  TBloky in 'TBloky.pas',
  TBlokVyhybka in 'TBlokVyhybka.pas',
  TBlokUsek in 'TBlokUsek.pas',
  TBlokIR in 'TBlokIR.pas',
  TBlokSCom in 'TBlokSCom.pas',
  TOblsRizeni in 'TOblsRizeni.pas',
  TOblRizeni in 'TOblRizeni.pas',
  Booster in 'Booster.pas',
  BoosterDb in 'BoosterDb.pas',
  fBlkVyhybkaSysVars in 'fBlkVyhybkaSysVars.pas' {F_BlkVyh_tech},
  AC in 'AC.pas',
  SnadnSpusteni in 'SnadnSpusteni.pas',
  fBlkSCom in 'fBlkSCom.pas',
  TJCDatabase in 'TJCDatabase.pas',
  THVDatabase in 'THVDatabase.pas',
  THnaciVozidlo in 'THnaciVozidlo.pas',
  Logging in 'Logging.pas',
  TCPServerOR in 'TCPServerOR.pas',
  DataMTB in 'DataMTB.pas',
  DataHV in 'DataHV.pas',
  DataJC in 'DataJC.pas',
  DataAC in 'DataAC.pas',
  DataBloky in 'DataBloky.pas',
  DataZesilovac in 'DataZesilovac.pas',
  DataORs in 'DataORs.pas',
  TBlokTrat in 'TBlokTrat.pas',
  TBlokUvazka in 'TBlokUvazka.pas',
  Souprava in 'Souprava.pas',
  SprDb in 'SprDb.pas',
  DataSpr in 'DataSpr.pas',
  User in 'User.pas',
  UserDb in 'UserDb.pas',
  DataUsers in 'DataUsers.pas',
  fUserEdit in 'fUserEdit.pas' {F_UserEdit},
  Zasobnik in 'Zasobnik.pas',
  UPO in 'UPO.pas',
  fBlkTratSysVars in 'fBlkTratSysVars.pas' {F_BlkTrat_tech},
  ModelovyCas in 'ModelovyCas.pas',
  fBlkSComEvent in 'fBlkSComEvent.pas' {F_BlkSComEvent},
  CloseTabSheet in 'CloseTabSheet.pas',
  TBlokZamek in 'TBlokZamek.pas',
  fBlkZamek in 'fBlkZamek.pas' {F_BlkZamek},
  TMultiJCDatabase in 'TMultiJCDatabase.pas',
  TechnologieMultiJC in 'TechnologieMultiJC.pas',
  DataMultiJC in 'DataMultiJC.pas',
  fHVPomEdit in 'fHVPomEdit.pas' {F_HV_Pom},
  fMJCEdit in 'fMJCEdit.pas' {F_MJCEdit},
  ACDatabase in 'ACDatabase.pas',
  ownStrUtils in 'ownStrUtils.pas',
  TBlokRozp in 'TBlokRozp.pas',
  fBlkRozp in 'fBlkRozp.pas' {F_BlkRozp},
  RegulatorTCP in 'RegulatorTCP.pas',
  fFuncsSet in 'fFuncsSet.pas' {F_FuncsSet},
  FunkceVyznam in 'FunkceVyznam.pas',
  TBlokTratUsek in 'TBlokTratUsek.pas',
  fBlkTU in 'fBlkTU.pas' {F_BlkTU},
  fBlkTUZastEvent in 'fBlkTUZastEvent.pas' {F_BlkTUZastEvent},
  MTBdebugger in 'MTBdebugger.pas',
  UDPdiscover in 'UDPdiscover.pas',
  USock in 'USock.pas',
  appEv in 'appEv.pas',
  SystemCriticalU in 'SystemCriticalU.pas',
  TBlokVystup in 'TBlokVystup.pas',
  fBlkVystup in 'fBlkVystup.pas' {F_BlkVystup},
  TCPServerPT in 'TCPServerPT.pas';

{$R *.res}

 begin
  Application.OnException := AppEvents.OnAppException;

  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

  DecimalSeparator := '.';

  // povolena je jen jedna instance
  if (ZkontrolujSpusteno) then
   begin
    Application.MessageBox('hJOPserver již spuštìn, povolena pouze jedna instance', 'Již spuštìn', MB_ICONWARNING OR MB_OK);
    halt(0);
   end;

  Application.Initialize;
  Application.Title := 'hJOPserver';
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Splash, F_Splash);
  Application.CreateForm(TF_Console, F_Console);
  Application.CreateForm(TF_BlkVyh_tech, F_BlkVyh_tech);
  Application.CreateForm(TF_UserEdit, F_UserEdit);
  Application.CreateForm(TF_BlkTrat_tech, F_BlkTrat_tech);
  Application.CreateForm(TF_BlkSComEvent, F_BlkSComEvent);
  Application.CreateForm(TF_HV_Pom, F_HV_Pom);
  Application.CreateForm(TF_MJCEdit, F_MJCEdit);
  Application.CreateForm(TF_FuncsSet, F_FuncsSet);
  Application.CreateForm(TF_BlkTUZastEvent, F_BlkTUZastEvent);
  F_Splash.AddStav('Vytváøím hlavní okno');
  F_Main.CreateSystem;
  F_Splash.AddStav('Vytváøím vedlejší okna');
  Application.CreateForm(TF_JCEdit, F_JCEdit);
  Application.CreateForm(TF_RychlostiEdit, F_RychlostiEdit);
  Application.CreateForm(TF_BlkUsek, F_BlkUsek);
  Application.CreateForm(TF_BlkVyhybka, F_BlkVyhybka);
  Application.CreateForm(TF_BlkIR, F_BlkIR);
  Application.CreateForm(TF_BlkSCom, F_BlkSCom);
  Application.CreateForm(TF_BlkNew, F_BlkNew);
  Application.CreateForm(TF_BlkPrejezd, F_BlkPrejezd);
  Application.CreateForm(TF_BlkTU, F_BlkTU);
  Application.CreateForm(TF_BlkVystup, F_BlkVystup);
  Application.CreateForm(TF_NoRegRun, F_NoRegRun);
  Application.CreateForm(TF_AutoStartSystems, F_AutoStartSystems);
  Application.CreateForm(TF_BlkUsek_tech, F_BlkUsek_tech);
  Application.CreateForm(TF_Pozadi, F_Pozadi);
  Application.CreateForm(TF_SystemInfo, F_SystemInfo);
  Application.CreateForm(TF_Admin, F_Admin);
  Application.CreateForm(TF_Options, F_Options);
  F_Splash.AddStav('Vytváøím vedlejší okna');
  Application.CreateForm(TF_Tester, F_Tester);
  Application.CreateForm(TF_ZesilovacEdit, F_ZesilovacEdit);
  Application.CreateForm(TF_BlkTrat, F_BlkTrat);
  Application.CreateForm(TF_BlkZamek, F_BlkZamek);
  Application.CreateForm(TF_BlkRozp, F_BlkRozp);
  Application.CreateForm(TF_HVEdit, F_HVEdit);
  Application.CreateForm(TF_AutRezEdit, F_AutRezEdit);
  Application.CreateForm(TF_Licence, F_Licence);
  Application.CreateForm(TF_ModCasSet, F_ModCasSet);
  Application.CreateForm(TF_About, F_About);
  F_Main.OnStart;
  F_splash.Close;

  F_Main.LogStatus('Program spuštìn');
  SystemCritical.IsCritical := true;

  Application.Run;

  //zniceni Mutexu pri ukonceni
  if (Mutex <> 0) then CloseHandle(Mutex);
 end.
//08.06.2009 -  9 649 radku
//03.07.2009 - 13 006 radku
//16.11.2009 - 19 376 radku
//06.03.2010 - 25 553 radku
//07.05.2010 - 29 622 radku
//02.07.2010 - 31 649 radku
//23.08.2010 - 32 863 radku
//13.03.2011 - 35 496 radku
//06.05.2014 - 30 037 radku
//19.06.2015 - 28 211 radku
//23.06.2016 - 31 931 radku
