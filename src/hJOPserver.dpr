// Project begin: september 2008

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
// JCL_DEBUG_EXPERT_GENERATEJDBG ON
program hJOPserver;

uses
  Forms,
  Windows,
  SysUtils,
  IniFiles,
  libPreload in 'libPreload.pas',
  fTester in 'forms\fTester.pas' {F_Tester},
  fMain in 'forms\fMain.pas' {F_Main},
  fModelTimeSet in 'forms\fModelTimeSet.pas' {F_ModelTimeSet},
  fRegulator in 'hv\forms\fRegulator.pas' {F_DigiReg},
  fSplash in 'forms\fSplash.pas' {F_splash},
  fAbout in 'forms\fAbout.pas' {F_About},
  fBlkCrossing in 'blocks\forms\data\fBlkCrossing.pas' {F_BlkCrossing},
  version in 'helpers\version.pas',
  fZesilovacEdit in 'forms\fZesilovacEdit.pas' {F_Booster_Edit},
  fBlkRailway in 'blocks\forms\data\fBlkRailway.pas' {F_BlkRailway},
  fHVEdit in 'hv\forms\fHVEdit.pas' {F_HVEdit},
  fSystemInfo in 'forms\fSystemInfo.pas' {F_SystemInfo},
  fBlkTrack in 'blocks\forms\data\fBlkTrack.pas' {F_BlkTrack},
  fBlkTurnout in 'blocks\forms\data\fBlkTurnout.pas' {F_BlkTurnout},
  fBlkIR in 'blocks\forms\data\fBlkIR.pas' {F_BlkIR},
  BlockCrossing in 'blocks\BlockCrossing.pas',
  fBlkNew in 'blocks\forms\fBlkNew.pas' {F_BlkNew},
  fAdminForm in 'forms\fAdminForm.pas' {F_Admin},
  fJCEdit in 'jc\forms\fJCEdit.pas' {F_JCEdit},
  fRychlostiEdit in 'forms\fRychlostiEdit.pas' {F_RychlostiEdit},
  fSystemAutoStart in 'forms\fSystemAutoStart.pas' {F_AutoStartSystems},
  fBlkTrackState in 'blocks\forms\state\fBlkTrackState.pas' {F_BlkTrackState},
  GetSystems in 'helpers\GetSystems.pas',
  ownConvert in 'helpers\ownConvert.pas',
  rcsc in 'rcs\rcsc.pas',
  TechnologieJC in 'jc\TechnologieJC.pas',
  Config in 'Config.pas',
  fConsole in 'forms\fConsole.pas' {F_Console},
  Block in 'blocks\Block.pas',
  BlockTurnout in 'blocks\BlockTurnout.pas',
  BlockTrack in 'blocks\BlockTrack.pas',
  BlockIR in 'blocks\BlockIR.pas',
  BlockSignal in 'blocks\BlockSignal.pas',
  AreaDb in 'areas\AreaDb.pas',
  Area in 'areas\Area.pas',
  Booster in 'boosters\Booster.pas',
  BoosterDb in 'boosters\BoosterDb.pas',
  fBlkTurnoutState in 'blocks\forms\state\fBlkTurnoutState.pas' {F_BlkTurnoutState},
  TJCDatabase in 'jc\TJCDatabase.pas',
  THVDatabase in 'hv\THVDatabase.pas',
  THnaciVozidlo in 'hv\THnaciVozidlo.pas',
  Logging in 'Logging.pas',
  TCPServerPanel in 'net\TCPServerPanel.pas',
  DataRCS in 'tables\DataRCS.pas',
  DataHV in 'tables\DataHV.pas',
  DataJC in 'tables\DataJC.pas',
  DataBloky in 'tables\DataBloky.pas',
  DataZesilovac in 'tables\DataZesilovac.pas',
  DataORs in 'tables\DataORs.pas',
  BlockRailway in 'blocks\BlockRailway.pas',
  BlockLinker in 'blocks\BlockLinker.pas',
  Train in 'trains\Train.pas',
  TrainDb in 'trains\TrainDb.pas',
  DataTrains in 'tables\DataTrains.pas',
  User in 'users\User.pas',
  UserDb in 'users\UserDb.pas',
  DataUsers in 'tables\DataUsers.pas',
  fUserEdit in 'forms\fUserEdit.pas' {F_UserEdit},
  AreaStack in 'areas\AreaStack.pas',
  UPO in 'jc\UPO.pas',
  fBlkRailwayState in 'blocks\forms\state\fBlkRailwayState.pas' {F_BlkRailwayState},
  timeModel in 'timeModel.pas',
  fBlkSignalEvent in 'blocks\forms\data\fBlkSignalEvent.pas' {F_BlkSignalEvent},
  CloseTabSheet in 'helpers\CloseTabSheet.pas',
  BlockLock in 'blocks\BlockLock.pas',
  fBlkLock in 'blocks\forms\data\fBlkLock.pas' {F_BlkLock},
  TMultiJCDatabase in 'jc\TMultiJCDatabase.pas',
  TechnologieMultiJC in 'jc\TechnologieMultiJC.pas',
  DataMultiJC in 'tables\DataMultiJC.pas',
  fHVPomEdit in 'hv\forms\fHVPomEdit.pas' {F_HV_Pom},
  fMJCEdit in 'jc\forms\fMJCEdit.pas' {F_MJCEdit},
  ownStrUtils in 'helpers\ownStrUtils.pas',
  BlockDisconnector in 'blocks\BlockDisconnector.pas',
  fBlkDisconnector in 'blocks\forms\data\fBlkDisconnector.pas' {F_BlkDisconnector},
  RegulatorTCP in 'RegulatorTCP.pas',
  fFuncsSet in 'forms\fFuncsSet.pas' {F_FuncsSet},
  FunkceVyznam in 'hv\FunkceVyznam.pas',
  BlockRailwayTrack in 'blocks\BlockRailwayTrack.pas',
  fBlkRT in 'blocks\forms\data\fBlkRT.pas' {F_BlkRT},
  fBlkRTStopEvent in 'blocks\forms\data\fBlkRTStopEvent.pas' {F_BlkRTStopEvent},
  RCSdebugger in 'net\RCSdebugger.pas',
  UDPdiscover in 'net\UDPdiscover.pas',
  USock in 'helpers\USock.pas',
  appEv in 'helpers\appEv.pas',
  SystemCriticalU in 'helpers\SystemCriticalU.pas',
  BlockIO in 'blocks\BlockIO.pas',
  fBlkIO in 'blocks\forms\data\fBlkIO.pas' {F_BlkIO},
  TCPServerPT in 'pt\TCPServerPT.pas',
  JsonDataObjects in '..\lib\JsonDataObjects\Source\JsonDataObjects.pas',
  PTEndpoint in 'pt\PTEndpoint.pas',
  PTEndpointBlock in 'pt\PTEndpointBlock.pas',
  PTEndpointBlocks in 'pt\PTEndpointBlocks.pas',
  PTEndpointTrains in 'pt\PTEndpointTrains.pas',
  PTEndpointJC in 'pt\PTEndpointJC.pas',
  PTUtils in 'pt\PTUtils.pas',
  PTEndpointLok in 'pt\PTEndpointLok.pas',
  PTEndpointLoks in 'pt\PTEndpointLoks.pas',
  PTEndpointBlockState in 'pt\PTEndpointBlockState.pas',
  PTEndpointLokState in 'pt\PTEndpointLokState.pas',
  RCSIFace in 'rcs\api\RCSIFace.pas',
  RCSErrors in 'rcs\api\RCSErrors.pas',
  rrEvent in 'rrEvent.pas',
  houkEvent in 'houkEvent.pas',
  frrEv in 'forms\frrEv.pas' {F_RREv},
  fhoukEv in 'blocks\forms\fhoukEv.pas' {F_HoukEv},
  fHoukEvs in 'blocks\forms\fHoukEvs.pas' {F_HoukEvs},
  fHoukEvsUsek in 'blocks\forms\fHoukEvsUsek.pas' {F_HoukEvsUsek},
  announcement in 'announcement\announcement.pas',
  announcementHelper in 'announcement\announcementHelper.pas',
  changeEvent in 'changeEvent.pas',
  changeEventCaller in 'changeEventCaller.pas',
  BlockSummary in 'blocks\BlockSummary.pas',
  fBlkSummary in 'blocks\forms\data\fBlkSummary.pas' {F_BlkSummary},
  predvidanyOdjezd in 'predvidanyOdjezd.pas',
  timeHelper in 'helpers\timeHelper.pas',
  TechnologieAB in 'jc\TechnologieAB.pas',
  DataAB in 'tables\DataAB.pas',
  PanelConnData in 'net\PanelConnData.pas',
  fBlkSignal in 'blocks\forms\data\fBlkSignal.pas' {F_BlkSignal},
  TrakceIFace in 'trakce\api\TrakceIFace.pas',
  TrakceErrors in 'trakce\api\TrakceErrors.pas',
  Trakcec in 'trakce\Trakcec.pas' {$R *.res},
  CpuLoad in 'helpers\CpuLoad.pas',
  Diagnostics in 'Diagnostics.pas',
  Simulation in 'Simulation.pas',
  PTEndpointJCs in 'pt\PTEndpointJCs.pas',
  PTEndpointJCStav in 'pt\PTEndpointJCStav.pas',
  BlockAC in 'blocks\BlockAC.pas',
  fBlkAC in 'blocks\forms\data\fBlkAC.pas' {F_BlkAC},
  ACBlocks in 'net\ACBlocks.pas',
  BlockCrossingLogic in 'blocks\BlockCrossingLogic.pas',
  BlockTrackRef in 'blocks\BlockTrackRef.pas',
  BlockTrackRefs in 'blocks\BlockTrackRefs.pas',
  AreaLighting in 'areas\AreaLighting.pas',
  PTEndpointTrain in 'pt\PTEndpointTrain.pas',
  IfThenElse in 'helpers\IfThenElse.pas',
  PTEndpointAreas in 'pt\PTEndpointAreas.pas',
  PTEndpointArea in 'pt\PTEndpointArea.pas',
  BlockDb in 'blocks\BlockDb.pas',
  BlockGroupSignal in 'blocks\BlockGroupSignal.pas',
  fBlkGroupSignal in 'blocks\forms\data\fBlkGroupSignal.pas' {F_BlkGroupSignal},
  PTEndpointUsers in 'pt\PTEndpointUsers.pas',
  PTEndpointUser in 'pt\PTEndpointUser.pas',
  BlockPst in 'blocks\BlockPst.pas',
  fBlkPst in 'blocks\forms\data\fBlkPst.pas' {F_BlkPst},
  JCBarriers in 'jc\JCBarriers.pas',
  TrainSpeed in 'trains\TrainSpeed.pas',
  fTrainSpeed in 'forms\fTrainSpeed.pas' {F_TrainSpeed},
  ConfSeq in 'ConfSeq.pas',
  fBlkCrossingState in 'blocks\forms\state\fBlkCrossingState.pas' {F_BlkCrossingState},
  fBlkSignalState in 'blocks\forms\state\fBlkSignalState.pas' {F_BlkSignalState},
  PTEndpointTime in 'pt\PTEndpointTime.pas',
  colorHelper in 'helpers\colorHelper.pas',
  BlockCrossingPositive in 'blocks\BlockCrossingPositive.pas',
  BlockProxy in 'blocks\BlockProxy.pas',
  ownGuiUtils in 'helpers\ownGuiUtils.pas',
  PTEndpointStatus in 'pt\PTEndpointStatus.pas',
  RCSsc in 'rcs\rcssc.pas';

{$R *.res}

 begin
  F_Main := nil;
  Application.OnException := AppEvents.OnAppException;

  Randomize();
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

  FormatSettings.DecimalSeparator := '.';

  Application.Initialize();
  Application.Title := 'hJOPserver';
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Splash, F_Splash);
  Application.CreateForm(TF_BlkAC, F_BlkAC);
  Application.CreateForm(TF_BlkGroupSignal, F_BlkGroupSignal);
  Application.CreateForm(TF_BlkPst, F_BlkPst);
  Application.CreateForm(TF_TrainSpeed, F_TrainSpeed);
  Application.CreateForm(TF_BlkCrossingState, F_BlkCrossingState);
  Application.CreateForm(TF_BlkSignalState, F_BlkSignalState);
  F_Splash.AddStav('Vytvářím okna...');

  Application.CreateForm(TF_Console, F_Console);
  Application.CreateForm(TF_BlkTurnoutState, F_BlkTurnoutState);
  Application.CreateForm(TF_UserEdit, F_UserEdit);
  Application.CreateForm(TF_BlkRailwayState, F_BlkRailwayState);
  Application.CreateForm(TF_BlkSignalEvent, F_BlkSignalEvent);
  Application.CreateForm(TF_HV_Pom, F_HV_Pom);
  Application.CreateForm(TF_MJCEdit, F_MJCEdit);
  Application.CreateForm(TF_FuncsSet, F_FuncsSet);
  Application.CreateForm(TF_BlkRTStopEvent, F_BlkRTStopEvent);
  Application.CreateForm(TF_HoukEvsUsek, F_HoukEvsUsek);
  Application.CreateForm(TF_JCEdit, F_JCEdit);
  Application.CreateForm(TF_RychlostiEdit, F_RychlostiEdit);
  Application.CreateForm(TF_BlkTrack, F_BlkTrack);
  Application.CreateForm(TF_BlkTurnout, F_BlkTurnout);
  Application.CreateForm(TF_BlkIR, F_BlkIR);
  Application.CreateForm(TF_BlkSignal, F_BlkSignal);
  Application.CreateForm(TF_BlkNew, F_BlkNew);
  Application.CreateForm(TF_BlkCrossing, F_BlkCrossing);
  Application.CreateForm(TF_BlkRT, F_BlkRT);
  Application.CreateForm(TF_BlkIO, F_BlkIO);
  Application.CreateForm(TF_BlkSummary, F_BlkSummary);
  Application.CreateForm(TF_AutoStartSystems, F_AutoStartSystems);
  Application.CreateForm(TF_BlkTrackState, F_BlkTrackState);
  Application.CreateForm(TF_SystemInfo, F_SystemInfo);
  Application.CreateForm(TF_Admin, F_Admin);
  Application.CreateForm(TF_Tester, F_Tester);
  Application.CreateForm(TF_Booster_Edit, F_Booster_Edit);
  Application.CreateForm(TF_BlkRailway, F_BlkRailway);
  Application.CreateForm(TF_BlkLock, F_BlkLock);
  Application.CreateForm(TF_BlkDisconnector, F_BlkDisconnector);
  Application.CreateForm(TF_HVEdit, F_HVEdit);
  Application.CreateForm(TF_ModelTimeSet, F_ModelTimeSet);
  Application.CreateForm(TF_About, F_About);

  LogInit();
  if (not FileExists(_INIDATA_FN)) then
  begin
    Logging.log(_INIDATA_FN + ' neexistuje, vytvářím adresářovou strukturu...', llWarning);
    Config.CreateCfgDirs();
  end;

  var inidata: TMemIniFile := nil;
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
    Config.CompleteLoadFromFile(inidata);
  except
    on E:Exception do
      AppEvents.LogException(E, 'CompleteLoadFromFile');
  end;
  if (inidata <> nil) then
    inidata.Free();

  F_Main.OnStart();
  F_splash.Close();

  F_Main.LogStatus('Spuštěn hJOPserver v' + version.VersionStr());
  SystemCritical.IsCritical := true;

  Application.Run();

  FreeAndNil(trakce);
  FreeAndNIl(RCSs);
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
