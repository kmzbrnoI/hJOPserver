//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     HHHHH            H
//H                                                        H      H      H    H   H    H    H          H H
//H   Název : hJOPserver                                   H      H      H   H     H   H     H        H   H
//H   Jméno tvůrce : Jan Horáček                           H      HHHHHHHH   H     H   H    H        H     H
//H   Autorská práva : Jan Horáček 2008-2020               H      H      H   H     H   HHHHH        HHHHHHHHH
//H                                                        H      H      H   H     H   H HH        H         H
//H                                                        H      H      H    H   H    H   HH     H           H
//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     H     HH   H           H
//Začátek tvorby : září 2008

// JCL_DEBUG_EXPERT_INSERTJDBG ON
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
program hJOPserver;

uses
  Forms,
  Windows,
  SysUtils,
  IniFiles,
  libPreload in 'libPreload.pas',
  fTester in 'forms\fTester.pas' {F_Tester},
  fMain in 'forms\fMain.pas' {F_Main},
  fSettings in 'forms\fSettings.pas' {F_Options},
  fNastaveni_Casu in 'forms\fNastaveni_Casu.pas' {F_ModCasSet},
  fRegulator in 'hv\forms\fRegulator.pas' {F_DigiReg},
  fSplash in 'forms\fSplash.pas' {F_splash},
  fAbout in 'forms\fAbout.pas' {F_About},
  fBlkPrejezd in 'bloky\forms\fBlkPrejezd.pas' {F_BlkPrejezd},
  Verze in 'helpers\Verze.pas',
  adCpuUsage in 'helpers\adCpuUsage.pas',
  fZesilovacEdit in 'forms\fZesilovacEdit.pas' {F_ZesilovacEdit},
  fBlkTrat in 'bloky\forms\fBlkTrat.pas' {F_BlkTrat},
  fHVEdit in 'hv\forms\fHVEdit.pas' {F_HVEdit},
  fSystemInfo in 'forms\fSystemInfo.pas' {F_SystemInfo},
  fBlkUsek in 'bloky\forms\fBlkUsek.pas' {F_BlkUsek},
  fBlkVyhybka in 'bloky\forms\fBlkVyhybka.pas' {F_BlkVyhybka},
  fBlkIR in 'bloky\forms\fBlkIR.pas' {F_BlkIR},
  TBlockCrossing in 'bloky\TBlockCrossing.pas',
  fBlkNew in 'bloky\forms\fBlkNew.pas' {F_BlkNew},
  fAdminForm in 'forms\fAdminForm.pas' {F_Admin},
  fJCEdit in 'jc\forms\fJCEdit.pas' {F_JCEdit},
  fRychlostiEdit in 'forms\fRychlostiEdit.pas' {F_RychlostiEdit},
  fSystemAutoStart in 'forms\fSystemAutoStart.pas' {F_AutoStartSystems},
  fBlkUsekSysVars in 'bloky\forms\fBlkUsekSysVars.pas' {F_BlkUsek_tech},
  GetSystems in 'helpers\GetSystems.pas',
  ownConvert in 'helpers\ownConvert.pas',
  TechnologieRCS in 'TechnologieRCS.pas',
  TechnologieJC in 'jc\TechnologieJC.pas',
  FileSystem in 'FileSystem.pas',
  fConsole in 'forms\fConsole.pas' {F_Console},
  TBlock in 'bloky\TBlock.pas',
  TBlockTurnout in 'bloky\TBlockTurnout.pas',
  TBlockTrack in 'bloky\TBlockTrack.pas',
  TBlockIR in 'bloky\TBlockIR.pas',
  TBlockSignal in 'bloky\TBlockSignal.pas',
  TOblsRizeni in 'TOblsRizeni.pas',
  TOblRizeni in 'TOblRizeni.pas',
  Booster in 'Booster.pas',
  BoosterDb in 'BoosterDb.pas',
  fBlkVyhybkaSysVars in 'bloky\forms\fBlkVyhybkaSysVars.pas' {F_BlkVyh_tech},
  SnadnSpusteni in 'SnadnSpusteni.pas',
  TJCDatabase in 'jc\TJCDatabase.pas',
  THVDatabase in 'hv\THVDatabase.pas',
  THnaciVozidlo in 'hv\THnaciVozidlo.pas',
  Logging in 'Logging.pas',
  TCPServerOR in 'TCPServerOR.pas',
  DataRCS in 'tables\DataRCS.pas',
  DataHV in 'tables\DataHV.pas',
  DataJC in 'tables\DataJC.pas',
  DataBloky in 'tables\DataBloky.pas',
  DataZesilovac in 'tables\DataZesilovac.pas',
  DataORs in 'tables\DataORs.pas',
  TBlockRailway in 'bloky\TBlockRailway.pas',
  TBlockLinker in 'bloky\TBlockLinker.pas',
  Train in 'Train.pas',
  TrainDb in 'TrainDb.pas',
  DataSpr in 'tables\DataSpr.pas',
  User in 'User.pas',
  UserDb in 'UserDb.pas',
  DataUsers in 'tables\DataUsers.pas',
  fUserEdit in 'forms\fUserEdit.pas' {F_UserEdit},
  Zasobnik in 'Zasobnik.pas',
  UPO in 'jc\UPO.pas',
  fBlkTratSysVars in 'bloky\forms\fBlkTratSysVars.pas' {F_BlkTrat_tech},
  ModelovyCas in 'ModelovyCas.pas',
  fBlkNavEvent in 'bloky\forms\fBlkNavEvent.pas' {F_BlkNavEvent},
  CloseTabSheet in 'helpers\CloseTabSheet.pas',
  TBlokZamek in 'bloky\TBlokZamek.pas',
  fBlkZamek in 'bloky\forms\fBlkZamek.pas' {F_BlkZamek},
  TMultiJCDatabase in 'jc\TMultiJCDatabase.pas',
  TechnologieMultiJC in 'jc\TechnologieMultiJC.pas',
  DataMultiJC in 'tables\DataMultiJC.pas',
  fHVPomEdit in 'hv\forms\fHVPomEdit.pas' {F_HV_Pom},
  fMJCEdit in 'jc\forms\fMJCEdit.pas' {F_MJCEdit},
  ownStrUtils in 'helpers\ownStrUtils.pas',
  TBlockDisconnector in 'bloky\TBlockDisconnector.pas',
  fBlkRozp in 'bloky\forms\fBlkRozp.pas' {F_BlkRozp},
  RegulatorTCP in 'RegulatorTCP.pas',
  fFuncsSet in 'forms\fFuncsSet.pas' {F_FuncsSet},
  FunkceVyznam in 'hv\FunkceVyznam.pas',
  TBlockRailwayTrack in 'bloky\TBlockRailwayTrack.pas',
  fBlkTU in 'bloky\forms\fBlkTU.pas' {F_BlkTU},
  fBlkTUZastEvent in 'bloky\forms\fBlkTUZastEvent.pas' {F_BlkTUZastEvent},
  RCSdebugger in 'RCSdebugger.pas',
  UDPdiscover in 'UDPdiscover.pas',
  USock in 'helpers\USock.pas',
  appEv in 'helpers\appEv.pas',
  SystemCriticalU in 'helpers\SystemCriticalU.pas',
  TBlockIO in 'bloky\TBlockIO.pas',
  fBlkIO in 'bloky\forms\fBlkIO.pas' {F_BlkIO},
  TCPServerPT in 'pt\TCPServerPT.pas',
  JsonDataObjects in '..\lib\JsonDataObjects\Source\JsonDataObjects.pas',
  PTEndpoint in 'pt\PTEndpoint.pas',
  PTEndpointBlok in 'pt\PTEndpointBlok.pas',
  PTEndpointBloky in 'pt\PTEndpointBloky.pas',
  PTEndpointTrains in 'pt\PTEndpointTrains.pas',
  PTEndpointJC in 'pt\PTEndpointJC.pas',
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
  TBlockSummary in 'bloky\TBlockSummary.pas',
  fBlkSH in 'bloky\forms\fBlkSH.pas',
  predvidanyOdjezd in 'predvidanyOdjezd.pas',
  timeHelper in 'helpers\timeHelper.pas',
  TechnologieAB in 'TechnologieAB.pas',
  DataAB in 'tables\DataAB.pas',
  TCPORsRef in 'TCPORsRef.pas',
  fBlkNav in 'bloky\forms\fBlkNav.pas',
  Trakce in 'trakce\Trakce.pas',
  TrakceErrors in 'trakce\TrakceErrors.pas',
  TechnologieTrakce in 'TechnologieTrakce.pas' {$R *.res},
  CpuLoad in 'CpuLoad.pas',
  Diagnostics in 'Diagnostics.pas',
  Simulation in 'Simulation.pas',
  PTEndpointJCs in 'pt\PTEndpointJCs.pas',
  PTEndpointJCStav in 'pt\PTEndpointJCStav.pas',
  TBlockAC in 'bloky\TBlockAC.pas',
  fBlkAC in 'bloky\forms\fBlkAC.pas' {F_BlkAC},
  ACBlocks in 'ACBlocks.pas',
  TBlockCrossingLogic in 'bloky\TBlockCrossingLogic.pas',
  TBlockTrackRef in 'bloky\TBlockTrackRef.pas',
  TBlockTrackRefs in 'bloky\TBlockTrackRefs.pas',
  orLighting in 'orLighting.pas',
  PTEndpointTrain in 'pt\PTEndpointTrain.pas',
  IfThenElse in 'helpers\IfThenElse.pas',
  PTEndpointUsers in 'pt\PTEndpointUsers.pas',
  PTEndpointUser in 'pt\PTEndpointUser.pas',
  BlockDb in 'bloky\BlockDb.pas';

{$R *.res}

var
  inidata: TMemIniFile;

 begin
  F_Main := nil;
  Application.OnException := AppEvents.OnAppException;

  Randomize();
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

  DecimalSeparator := '.';

  if (ZkontrolujSpusteno) then
   begin
    Application.MessageBox('hJOPserver již spuštěn, povolena pouze jedna instance', 'Již spuštěn', MB_ICONWARNING OR MB_OK);
    halt(0);
   end;

  Application.Initialize();
  Application.Title := 'hJOPserver';
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Splash, F_Splash);
  Application.CreateForm(TF_BlkAC, F_BlkAC);
  F_Splash.AddStav('Vytvářím okna...');

  Application.CreateForm(TF_Console, F_Console);
  Application.CreateForm(TF_BlkVyh_tech, F_BlkVyh_tech);
  Application.CreateForm(TF_UserEdit, F_UserEdit);
  Application.CreateForm(TF_BlkTrat_tech, F_BlkTrat_tech);
  Application.CreateForm(TF_BlkNavEvent, F_BlkNavEvent);
  Application.CreateForm(TF_HV_Pom, F_HV_Pom);
  Application.CreateForm(TF_MJCEdit, F_MJCEdit);
  Application.CreateForm(TF_FuncsSet, F_FuncsSet);
  Application.CreateForm(TF_BlkTUZastEvent, F_BlkTUZastEvent);
  Application.CreateForm(TF_HoukEvsUsek, F_HoukEvsUsek);
  Application.CreateForm(TF_JCEdit, F_JCEdit);
  Application.CreateForm(TF_RychlostiEdit, F_RychlostiEdit);
  Application.CreateForm(TF_BlkUsek, F_BlkUsek);
  Application.CreateForm(TF_BlkVyhybka, F_BlkVyhybka);
  Application.CreateForm(TF_BlkIR, F_BlkIR);
  Application.CreateForm(TF_BlkNav, F_BlkNav);
  Application.CreateForm(TF_BlkNew, F_BlkNew);
  Application.CreateForm(TF_BlkPrejezd, F_BlkPrejezd);
  Application.CreateForm(TF_BlkTU, F_BlkTU);
  Application.CreateForm(TF_BlkIO, F_BlkIO);
  Application.CreateForm(TF_BlkSH, F_BlkSH);
  Application.CreateForm(TF_AutoStartSystems, F_AutoStartSystems);
  Application.CreateForm(TF_BlkUsek_tech, F_BlkUsek_tech);
  Application.CreateForm(TF_SystemInfo, F_SystemInfo);
  Application.CreateForm(TF_Admin, F_Admin);
  Application.CreateForm(TF_Options, F_Options);
  Application.CreateForm(TF_Tester, F_Tester);
  Application.CreateForm(TF_ZesilovacEdit, F_ZesilovacEdit);
  Application.CreateForm(TF_BlkTrat, F_BlkTrat);
  Application.CreateForm(TF_BlkZamek, F_BlkZamek);
  Application.CreateForm(TF_BlkRozp, F_BlkRozp);
  Application.CreateForm(TF_HVEdit, F_HVEdit);
  Application.CreateForm(TF_ModCasSet, F_ModCasSet);
  Application.CreateForm(TF_About, F_About);

  LogInit();
  if (not FileExists(_INIDATA_FN)) then
    Config.CreateCfgDirs();

  try
    inidata := TMeminifile.Create(_INIDATA_FN, TEncoding.UTF8);
    F_splash.AddStav('Načítám preload knihovny...');
    try
      preload.Preload(inidata, 'LD_Preload');
    except
      on E:Exception do
        AppEvents.LogException(E, 'LoadPreloadLibs');
    end;

    F_splash.AddStav('Načítám data...');
    try
      Config.CompleteLoadFromFile(inidata);
    finally
      inidata.UpdateFile();
      inidata.Free();
    end;
  except
    on E:Exception do
      AppEvents.LogException(E, 'CompleteLoadFromFile');
  end;

  F_Main.OnStart();
  F_splash.Close();

  F_Main.LogStatus('Program spuštěn');
  SystemCritical.IsCritical := true;

  Application.Run();

  FreeAndNil(TrakceI);
  FreeAndNil(RCSi);
  if (Mutex <> 0) then
    CloseHandle(Mutex);
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
