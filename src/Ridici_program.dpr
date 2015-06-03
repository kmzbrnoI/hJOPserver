//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     HHHHH            H
//H                                                        H      H      H    H   H    H    H          H H
//H   Název : Øídící program verze 4.0.0.1                 H      H      H   H     H   H     H        H   H
//H   Jméno tvùrce : Jan Horáèek                           H      HHHHHHHH   H     H   H    H        H     H
//H   Autorská práva : Jan Horáèek 2008-2014               H      H      H   H     H   HHHHH        HHHHHHHHH
//H   Datum poslední editace : 01. 09. 2014                H      H      H   H     H   H HH        H         H
//H                                                        H      H      H    H   H    H   HH     H           H
//HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      H      H     HHH     H     HH   H           H
//Zaèátek tvorby : záøí 2008

program Ridici_program;

uses
  Forms,
  Windows,
  SysUtils,
  RPConst in 'RPConst.pas',
  Tester in 'Tester.pas' {F_Tester},
  Main in 'Main.pas' {F_Main},
  Settings in 'Settings.pas' {F_Options},
  Licence in 'Licence.pas' {F_Licence},
  Nastaveni_Casu in 'Nastaveni_Casu.pas' {F_ModCasSet},
  Regulator in 'Regulator.pas' {F_DigiReg},
  Splash in 'Splash.pas' {F_splash},
  About in 'About.pas' {F_About},
  ACEdit in 'ACEdit.pas' {F_AutRezEdit},
  BlkPrejezd in 'BlkPrejezd.pas' {F_BlkPrejezd},
  Verze in 'Verze.pas',
  LoginPozadi in 'LoginPozadi.pas' {F_Pozadi},
  ZesilovacEdit in 'ZesilovacEdit.pas' {F_ZesilovacEdit},
  BlkTrat in 'BlkTrat.pas' {F_BlkTrat},
  HVEdit in 'HVEdit.pas' {F_HVEdit},
  SystemInfo in 'SystemInfo.pas' {F_SystemInfo},
  BlkUsek in 'BlkUsek.pas' {F_BlkUsek},
  BlkVyhybka in 'BlkVyhybka.pas' {F_BlkVyhybka},
  BlkIR in 'BlkIR.pas' {F_BlkIR},
  TBlokPrejezd in 'TBlokPrejezd.pas' {F_BlkSCom},
  BlkNew in 'BlkNew.pas' {F_BlkNew},
  AdminForm in 'AdminForm.pas' {F_Admin},
  NoRegRun in 'NoRegRun.pas' {F_NoRegRun},
  VCEdit in 'VCEdit.pas' {F_JCEdit},
  RychlostiEdit in 'RychlostiEdit.pas' {F_RychlostiEdit},
  SystemAutoStart in 'SystemAutoStart.pas' {F_AutoStartSystems},
  BlkUsekSysVars in 'BlkUsekSysVars.pas' {F_BlkUsek_tech},
  GetSystems in 'GetSystems.pas',
  Prevody in 'Prevody.pas',
  TechnologieMTB in 'TechnologieMTB.pas',
  TechnologieJC in 'TechnologieJC.pas',
  FileSystem in 'FileSystem.pas',
  Console in 'Console.pas' {F_Console},
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
  BlkVyhybkaSysVars in 'BlkVyhybkaSysVars.pas' {F_BlkVyh_tech},
  AC in 'AC.pas',
  SnadnSpusteni in 'SnadnSpusteni.pas',
  BlkSCom in 'BlkSCom.pas',
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
  UserEdit in 'UserEdit.pas' {F_UserEdit},
  Zasobnik in 'Zasobnik.pas',
  UPO in 'UPO.pas',
  BlkTratSysVars in 'BlkTratSysVars.pas' {F_BlkTrat_tech},
  ModelovyCas in 'ModelovyCas.pas',
  BlkSComEvent in 'BlkSComEvent.pas' {F_BlkSComEvent},
  CloseTabSheet in 'CloseTabSheet.pas',
  TBlokZamek in 'TBlokZamek.pas',
  BlkZamek in 'BlkZamek.pas' {F_BlkZamek},
  TMultiJCDatabase in 'TMultiJCDatabase.pas',
  TechnologieMultiJC in 'TechnologieMultiJC.pas',
  DataMultiJC in 'DataMultiJC.pas',
  HVPomEdit in 'HVPomEdit.pas' {F_HV_Pom},
  MJCEdit in 'MJCEdit.pas' {F_MJCEdit},
  ACDatabase in 'ACDatabase.pas',
  ownStrUtils in 'ownStrUtils.pas',
  TBlokRozp in 'TBlokRozp.pas',
  BlkRozp in 'BlkRozp.pas' {F_BlkRozp},
  RegulatorTCP in 'RegulatorTCP.pas';

{$R *.res}

//Hlavni inicializace - vsechno ma svuj rad - nemenit
 begin
  DecimalSeparator := '.';

  // povolena je jen jedna instance RP
  if (ZkontrolujSpusteno) then
   begin
    Application.MessageBox('Øídící program již spuštìn, povolena pouze jedna instance', 'Již spuštìn', MB_ICONWARNING OR MB_OK);
    halt(0);
   end;

  Application.Initialize;
  Application.Title := 'Øídící program';

  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Splash, F_Splash);
  Application.CreateForm(TF_Console, F_Console);
  Application.CreateForm(TF_BlkVyh_tech, F_BlkVyh_tech);
  Application.CreateForm(TF_UserEdit, F_UserEdit);
  Application.CreateForm(TF_BlkTrat_tech, F_BlkTrat_tech);
  Application.CreateForm(TF_BlkSComEvent, F_BlkSComEvent);
  Application.CreateForm(TF_HV_Pom, F_HV_Pom);
  Application.CreateForm(TF_MJCEdit, F_MJCEdit);
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
  Application.CreateForm(TF_NoRegRun, F_NoRegRun);
  Application.CreateForm(TF_AutoStartSystems, F_AutoStartSystems);
  Application.CreateForm(TF_BlkUsek_tech, F_BlkUsek_tech);
  //creatovani zakladnich oken
    Application.CreateForm(TF_Pozadi, F_Pozadi);
    Application.CreateForm(TF_SystemInfo, F_SystemInfo);
    Application.CreateForm(TF_Admin, F_Admin);
    Application.CreateForm(TF_Options, F_Options);
    F_Splash.AddStav('Vytváøím vedlejší okna');    
    Application.CreateForm(TF_Tester, F_Tester);
    //creatovani editacnich oken
    Application.CreateForm(TF_ZesilovacEdit, F_ZesilovacEdit);
    Application.CreateForm(TF_BlkTrat, F_BlkTrat);
    Application.CreateForm(TF_BlkZamek, F_BlkZamek);
    Application.CreateForm(TF_BlkRozp, F_BlkRozp);
    Application.CreateForm(TF_HVEdit, F_HVEdit);
    Application.CreateForm(TF_AutRezEdit, F_AutRezEdit);
    //cretovani oken okolo prihlasovani
    //creatovani dalsich oken
    Application.CreateForm(TF_Licence, F_Licence);
    Application.CreateForm(TF_ModCasSet, F_ModCasSet);
    Application.CreateForm(TF_About, F_About);
    F_Main.OnStart;
    F_splash.Close;

    F_Main.LogStatus('Program spuštìn');

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
