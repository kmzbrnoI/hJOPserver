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
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
program hJOPserver;

uses
  Forms,
  Windows,
  SysUtils,
  fTester in 'forms\fTester.pas' {F_Tester},
  fMain in 'forms\fMain.pas' {F_Main},
  fSettings in 'forms\fSettings.pas' {F_Options},
  fNastaveni_Casu in 'forms\fNastaveni_Casu.pas' {F_ModCasSet},
  fRegulator in 'hv\forms\fRegulator.pas' {F_DigiReg},
  fSplash in 'forms\fSplash.pas' {F_splash},
  fAbout in 'forms\fAbout.pas' {F_About},
  fBlkPrejezd in 'bloky\forms\fBlkPrejezd.pas' {F_BlkPrejezd},
  Verze in 'Verze.pas',
  fLoginPozadi in 'forms\fLoginPozadi.pas' {F_Pozadi},
  fZesilovacEdit in 'forms\fZesilovacEdit.pas' {F_ZesilovacEdit},
  fBlkTrat in 'bloky\forms\fBlkTrat.pas' {F_BlkTrat},
  fHVEdit in 'hv\forms\fHVEdit.pas' {F_HVEdit},
  fSystemInfo in 'forms\fSystemInfo.pas' {F_SystemInfo},
  fBlkUsek in 'bloky\forms\fBlkUsek.pas' {F_BlkUsek},
  fBlkVyhybka in 'bloky\forms\fBlkVyhybka.pas' {F_BlkVyhybka},
  fBlkIR in 'bloky\forms\fBlkIR.pas' {F_BlkIR},
  TBlokPrejezd in 'bloky\TBlokPrejezd.pas',
  fBlkNew in 'bloky\forms\fBlkNew.pas' {F_BlkNew},
  fAdminForm in 'forms\fAdminForm.pas' {F_Admin},
  fJCEdit in 'jc\forms\fJCEdit.pas' {F_JCEdit},
  fRychlostiEdit in 'forms\fRychlostiEdit.pas' {F_RychlostiEdit},
  fSystemAutoStart in 'forms\fSystemAutoStart.pas' {F_AutoStartSystems},
  fBlkUsekSysVars in 'bloky\forms\fBlkUsekSysVars.pas' {F_BlkUsek_tech},
  GetSystems in 'GetSystems.pas',
  Prevody in 'Prevody.pas',
  TechnologieRCS in 'TechnologieRCS.pas',
  TechnologieJC in 'jc\TechnologieJC.pas',
  FileSystem in 'FileSystem.pas',
  fConsole in 'forms\fConsole.pas' {F_Console},
  Trakce in 'trakce\Trakce.pas',
  XpressNET in 'trakce\XpressNET.pas',
  TrakceGUI in 'trakce\TrakceGUI.pas',
  TBlok in 'bloky\TBlok.pas',
  TBloky in 'bloky\TBloky.pas',
  TBlokVyhybka in 'bloky\TBlokVyhybka.pas',
  TBlokUsek in 'bloky\TBlokUsek.pas',
  TBlokIR in 'bloky\TBlokIR.pas',
  TBlokSCom in 'bloky\TBlokSCom.pas',
  TOblsRizeni in 'TOblsRizeni.pas',
  TOblRizeni in 'TOblRizeni.pas',
  Booster in 'Booster.pas',
  BoosterDb in 'BoosterDb.pas',
  fBlkVyhybkaSysVars in 'bloky\forms\fBlkVyhybkaSysVars.pas' {F_BlkVyh_tech},
  AC in 'AC.pas',
  SnadnSpusteni in 'SnadnSpusteni.pas',
  fBlkSCom in 'bloky\forms\fBlkSCom.pas',
  TJCDatabase in 'jc\TJCDatabase.pas',
  THVDatabase in 'hv\THVDatabase.pas',
  THnaciVozidlo in 'hv\THnaciVozidlo.pas',
  Logging in 'Logging.pas',
  TCPServerOR in 'TCPServerOR.pas',
  DataRCS in 'tables\DataRCS.pas',
  DataHV in 'tables\DataHV.pas',
  DataJC in 'tables\DataJC.pas',
  DataAC in 'tables\DataAC.pas',
  DataBloky in 'tables\DataBloky.pas',
  DataZesilovac in 'tables\DataZesilovac.pas',
  DataORs in 'tables\DataORs.pas',
  TBlokTrat in 'bloky\TBlokTrat.pas',
  TBlokUvazka in 'bloky\TBlokUvazka.pas',
  Souprava in 'Souprava.pas',
  SprDb in 'SprDb.pas',
  DataSpr in 'tables\DataSpr.pas',
  User in 'User.pas',
  UserDb in 'UserDb.pas',
  DataUsers in 'tables\DataUsers.pas',
  fUserEdit in 'forms\fUserEdit.pas' {F_UserEdit},
  Zasobnik in 'Zasobnik.pas',
  UPO in 'UPO.pas',
  fBlkTratSysVars in 'bloky\forms\fBlkTratSysVars.pas' {F_BlkTrat_tech},
  ModelovyCas in 'ModelovyCas.pas',
  fBlkSComEvent in 'bloky\forms\fBlkSComEvent.pas' {F_BlkSComEvent},
  CloseTabSheet in 'CloseTabSheet.pas',
  TBlokZamek in 'bloky\TBlokZamek.pas',
  fBlkZamek in 'bloky\forms\fBlkZamek.pas' {F_BlkZamek},
  TMultiJCDatabase in 'jc\TMultiJCDatabase.pas',
  TechnologieMultiJC in 'jc\TechnologieMultiJC.pas',
  DataMultiJC in 'tables\DataMultiJC.pas',
  fHVPomEdit in 'hv\forms\fHVPomEdit.pas' {F_HV_Pom},
  fMJCEdit in 'jc\forms\fMJCEdit.pas' {F_MJCEdit},
  ACDatabase in 'ACDatabase.pas',
  ownStrUtils in 'ownStrUtils.pas',
  TBlokRozp in 'bloky\TBlokRozp.pas',
  fBlkRozp in 'bloky\forms\fBlkRozp.pas' {F_BlkRozp},
  RegulatorTCP in 'RegulatorTCP.pas',
  fFuncsSet in 'forms\fFuncsSet.pas' {F_FuncsSet},
  FunkceVyznam in 'FunkceVyznam.pas',
  TBlokTratUsek in 'bloky\TBlokTratUsek.pas',
  fBlkTU in 'bloky\forms\fBlkTU.pas' {F_BlkTU},
  fBlkTUZastEvent in 'bloky\forms\fBlkTUZastEvent.pas' {F_BlkTUZastEvent},
  RCSdebugger in 'RCSdebugger.pas',
  UDPdiscover in 'UDPdiscover.pas',
  USock in 'USock.pas',
  appEv in 'appEv.pas',
  SystemCriticalU in 'SystemCriticalU.pas',
  TBlokVystup in 'bloky\TBlokVystup.pas',
  fBlkVystup in 'bloky\forms\fBlkVystup.pas' {F_BlkVystup},
  TCPServerPT in 'pt\TCPServerPT.pas',
  JsonDataObjects in '..\lib\JsonDataObjects\Source\JsonDataObjects.pas',
  PTEndpoint in 'pt\PTEndpoint.pas',
  PTEndpointBlok in 'pt\PTEndpointBlok.pas',
  PTEndpointBloky in 'pt\PTEndpointBloky.pas',
  PTUtils in 'pt\PTUtils.pas',
  PTEndpointLok in 'pt\PTEndpointLok.pas',
  PTEndpointLoks in 'pt\PTEndpointLoks.pas',
  PTEndpointBlokStav in 'pt\PTEndpointBlokStav.pas',
  PTEndpointLokStav in 'pt\PTEndpointLokStav.pas',
  RCS in 'rcs\RCS.pas',
  RCSErrors in 'rcs\RCSErrors.pas',
  rrEvent in 'rrEvent.pas',
  houkEvent in 'houkEvent.pas',
  frrEv in 'forms\frrEv.pas' {F_RREv},
  fhoukEv in 'bloky\forms\fhoukEv.pas' {F_HoukEv},
  fHoukEvs in 'bloky\forms\fHoukEvs.pas' {F_HoukEvs},
  fHoukEvsUsek in 'bloky\forms\fHoukEvsUsek.pas' {F_HoukEvsUsek},
  stanicniHlaseni in 'stanicniHlaseni.pas',
  stanicniHlaseniHelper in 'stanicniHlaseniHelper.pas',
  changeEvent in 'changeEvent.pas',
  changeEventCaller in 'changeEventCaller.pas',
  TBlokSouctovaHlaska in 'bloky\TBlokSouctovaHlaska.pas',
  fBlkSH in 'bloky\forms\fBlkSH.pas',
  predvidanyOdjezd in 'predvidanyOdjezd.pas',
  timeHelper in 'timeHelper.pas',
  TechnologieAB in 'TechnologieAB.pas',
  DataAB in 'tables\DataAB.pas';

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
  Application.CreateForm(TF_HoukEvsUsek, F_HoukEvsUsek);
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
  Application.CreateForm(TF_BlkSH, F_BlkSH);
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
