unit fSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, inifiles, Menus, ComCtrls, TabNotBk,
  ExtCtrls, Mask, Gauges, StrUtils, Registry, Grids, jpeg, ShellAPI,
  ShlObj, ToolWin, winsock, Buttons, IdBaseComponent,
  IdComponent, IdIPWatch, fMain, CPort, CPortCtl, AC, THnaciVozidlo,
  TechnologieRCS;

type

  TF_Options = class(TForm)

     //Ukladani
      B_pouzit: TButton;
      B_OK: TButton;

      //Log
       M_log: TMenuItem;

       PM_log_delete: TMenuItem;


    //TPopupMenu/TMainMenu
     MM_Options: TMainMenu;

    //TOpenDisalog/TSaveDialog
     OD_Open: TOpenDialog;
     SD_Save: TSaveDialog;
    PC_1: TPageControl;
    TS_Options: TTabSheet;
    P_ON_Pozadi: TPanel;
    GB_ON_1: TGroupBox;
    E_dataload: TEdit;
    GB_ON_2: TGroupBox;
    TS_SS: TTabSheet;
    P_SS: TPanel;
    L_SS_02: TLabel;
    L_SS_01: TLabel;
    CB_SS_AutRezimy: TComboBox;
    GB_SS_Vystupy: TGroupBox;
    L_SS_Out_1: TLabel;
    L_SS_Out_2: TLabel;
    L_SS_Out_3: TLabel;
    L_SS_Out_4: TLabel;
    SE_SS_Out_Ready: TSpinEdit;
    SE_SS_Out_Start: TSpinEdit;
    SE_SS_Out_Pause: TSpinEdit;
    SE_SS_Out_Stop: TSpinEdit;
    CHB_SS_Out_Ready: TCheckBox;
    CHB_SS_Out_Start: TCheckBox;
    CHB_SS_Out_Pause: TCheckBox;
    CHB_SS_Out_Stop: TCheckBox;
    GB_SS_Vstupy: TGroupBox;
    L_SS_In_1: TLabel;
    L_SS_In_2: TLabel;
    L_SS_In_3: TLabel;
    L_SS_In_4: TLabel;
    SE_SS_In_Start: TSpinEdit;
    SE_SS_In_Pause: TSpinEdit;
    SE_SS_In_Stop: TSpinEdit;
    SE_SS_In_Repeat: TSpinEdit;
    CHB_SS_In_Start: TCheckBox;
    CHB_SS_In_Pause: TCheckBox;
    CHB_SS_In_Stop: TCheckBox;
    CHB_SS_In_Repeat: TCheckBox;
    B_SS_Save: TButton;
    GB_PrijmutaData: TGroupBox;
    LB_Timer: TListBox;
    L_Data1: TLabel;
    TS_DigiRych: TTabSheet;
    LV_DigiRych: TListView;
    GB_Log: TGroupBox;
    CHB_Log_console: TCheckBox;
    GB_OnStart: TGroupBox;
    CHB_povolit_spusteni: TCheckBox;
    L_SS_Out_5: TLabel;
    CHB_SS_OUT_Opakovani: TCheckBox;
    SE_SS_Out_Opakovani: TSpinEdit;
    L_SS_In_5: TLabel;
    CHB_SS_In_Reset: TCheckBox;
    SE_SS_In_Reset: TSpinEdit;
    CHB_SS_Enable: TCheckBox;
    SE_SS_RCSAdr: TSpinEdit;
    TS_Centrala: TTabSheet;
    GB_Centrala: TGroupBox;
    B_Save: TButton;
    GB_TrackSystem: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CB_TrackSystem: TComboBox;
    CCB_BaudRate: TComComboBox;
    CCB_DataBits: TComComboBox;
    CCB_StopBits: TComComboBox;
    CCB_Port: TComComboBox;
    B_PortRefresh: TButton;
    GB_Autosave: TGroupBox;
    CHB_Autosave: TCheckBox;
    ME_autosave_period: TMaskEdit;
    Label1: TLabel;
    CCB_FC: TComComboBox;
    Label2: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure PM_log_deleteClick(Sender: TObject);
    procedure PC_1Change(Sender: TObject);
    procedure B_pouzitClick(Sender: TObject);
    procedure PM_Data_SaveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CHB_SS_Out_ReadyClick(Sender: TObject);
    procedure B_SS_SaveClick(Sender: TObject);
    procedure LB_TimerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_DigiRychDblClick(Sender: TObject);
    procedure CHB_SS_EnableClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure B_PortRefreshClick(Sender: TObject);
    procedure CHB_AutosaveClick(Sender: TObject);
    procedure CB_TrackSystemChange(Sender: TObject);
  private

  public

    procedure StartLogWrite;
    procedure NactiSSDoObjektu;
    procedure UlozDataDoSSzObjektu;

    procedure NactiCentralaData();
    procedure UlozCentralaData();
  end;

var
  F_Options: TF_Options;
  Sounds_znovunacteni:boolean;

implementation

uses  fTester, fNastaveni_Casu, fAbout, Verze, fZesilovacEdit, fHVEdit,
  fSystemInfo, fBlkUsek, fBlkVyhybka, fBlkIR, fBlkSCom, fBlkNew,
  fAdminForm, fJCEdit, fRychlostiEdit, fSplash, GetSystems, Prevody,
  TechnologieJC, FileSystem, TBloky, TBlok,
  TBlokVyhybka, TBlokSCom, TBlokUsek, TBlokIR, TOblsRizeni, BoosterDb,
  SnadnSpusteni, TBlokPrejezd, fBlkPrejezd, TJCDatabase, THVDatabase,
  Logging, DataBloky, DataJC, DataRCS, Trakce,
  ModelovyCas, ACDatabase;

{$R *.dfm}

procedure TF_Options.FormCreate(Sender: TObject);
begin
 //Zapis do logu o startu programu
 StartLogWrite;
 //Nacteni dat ze souboru do promennych programu
 F_splash.AddStav('Naèítám data');
 Data.CompleteLoadFromFile;
 //Nastaveni PageIndexe na 0
 PC_1.ActivePageIndex:=0;
end;

procedure TF_Options.PM_log_deleteClick(Sender: TObject);
var s: TSearchRec;
    i: integer;
begin
 i := FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\program\*.log', 0, s);
 while (i=0) do begin
   DeleteFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\program\'+s.Name);
   i := FindNext(s);
 end;
 FindClose(s);
 i := FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\net\*.log', 0, s);
 while (i=0) do begin
   DeleteFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\net\'+s.Name);
   i := FindNext(s);
 end;
 FindClose(s);
 i := FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\lnet\*.log', 0, s);
 while (i=0) do begin
   DeleteFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\lnet\'+s.Name);
   i := FindNext(s);
 end;
 FindClose(s);
 i := FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\login\*.log', 0, s);
 while (i=0) do begin
   DeleteFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log\login\'+s.Name);
   i := FindNext(s);
 end;
 FindClose(s);
 writelog('Smazány soubory LOG',WR_MESSAGE);
 Application.Messagebox('Soubory log smazany','Informace',MB_OK OR MB_ICONINFORMATION OR MB_DEFBUTTON1);
end;

procedure TF_Options.PC_1Change(Sender: TObject);
begin
 if (PC_1.ActivePage = TS_SS)         then NactiSSDoObjektu;
 if (PC_1.ActivePage = TS_Options)    then
  begin
   Self.ME_autosave_period.Text := FormatDateTime('nn:ss', data.autosave_period);
   Self.CHB_Autosave.Checked := Data.autosave;
  end;
 if (PC_1.ActivePage = TS_Centrala)   then Self.NactiCentralaData();

end;//procedure

procedure TF_Options.B_OKClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_Options.B_PortRefreshClick(Sender: TObject);
begin
 EnumComPorts(Self.CCB_Port.Items);
 Self.CCB_Port.Text := TrkSystem.COM;
end;

procedure TF_Options.B_pouzitClick(Sender: TObject);
 begin
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;

  Data.CompleteSaveToFile;

  Screen.Cursor := crDefault;
  F_Options.ActiveControl := B_OK;
 end;//procedure

procedure TF_Options.PM_Data_SaveClick(Sender: TObject);
 begin
  B_pouzitClick(self);
 end;//procedure

procedure TF_Options.StartLogWrite;
 begin
  WriteLog('$$$$$$$$$$ Spouštím hJOPserver $$$$$$$$$$',WR_MESSAGE);
  WriteLog('Datum ' + FormatDateTime('dd.mm.yyyy', Now), WR_MESSAGE);
  WriteLog('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$',WR_MESSAGE);
 end;//procedure

procedure TF_Options.FormResize(Sender: TObject);
begin
 SE_SS_RCSAdr.Left    := (PC_1.Width div 2) - (SE_SS_RCSAdr.Width div 2);
 CB_SS_AutRezimy.Left := SE_SS_RCSAdr.Left;
 GB_SS_Vstupy.Left    := (PC_1.Width div 2) - (GB_SS_Vstupy.Width)-5;
 GB_SS_Vystupy.Left   := (PC_1.Width div 2) + 5;
 CHB_SS_Enable.Left   := (PC_1.Width div 2) - (CHB_SS_Enable.Width div 2);
 L_SS_01.Left         := SE_SS_RCSAdr.Left;
 L_SS_02.Left         := SE_SS_RCSAdr.Left;
 GB_Centrala.Left     := (PC_1.Width div 2) - (GB_Centrala.Width div 2);

 B_SS_Save.Left := P_SS.Width-B_SS_Save.Width-10;
 B_SS_Save.Top  := P_SS.Height-B_SS_Save.Height-10;
end;

procedure TF_Options.NactiSSDoObjektu();
var IgnoraceRCS:TArI;
    i:Integer;
    data:TSSData;
 begin
  data := SS.GetData();

  SetLength(IgnoraceRCS,2);
  IgnoraceRCS[0] := 3;
  IgnoraceRCS[1] := 4;

  Self.SE_SS_RCSAdr.Value := data.RCSAdr;

  F_Options.CB_SS_AutRezimy.Clear;
  for i := 0 to ACDb.ACs.Count-1 do
    F_Options.CB_SS_AutRezimy.Items.Add(ACDb.ACs[i].name);
  F_Options.CB_SS_AutRezimy.ItemIndex := data.AutRezim;

  if (ACDb.ACs.Count = 0) then
   begin
    Self.CHB_SS_Enable.Enabled := false;
    Self.CHB_SS_Enable.Checked := false
   end else begin
    Self.CHB_SS_Enable.Enabled := true;
    Self.CHB_SS_Enable.Checked := data.enabled;
   end;
  Self.CHB_SS_EnableClick(self);

  //nacitani enablovani IN zacatek
  CHB_SS_In_Start.Checked  := (data.IN_Start > -1);
  CHB_SS_In_Pause.Checked  := (data.IN_Pause > -1);
  CHB_SS_In_Stop.Checked   := (data.IN_Stop > -1);
  CHB_SS_In_Repeat.Checked := (data.IN_Repeat > -1);
  CHB_SS_In_Reset.Checked  := (data.IN_Reset > -1);

  SE_SS_In_Start.Enabled  := (data.IN_Start > -1);
  SE_SS_In_Pause.Enabled  := (data.IN_Pause > -1);
  SE_SS_In_Stop.Enabled   := (data.IN_Stop > -1);
  SE_SS_In_Repeat.Enabled := (data.IN_Repeat > -1);
  SE_SS_In_Reset.Enabled  := (data.IN_Reset > -1);
  //nacitani enablovani IN konec

  //nacitani enablovani OUT zacatek
  CHB_SS_Out_Ready.Checked      := (data.OUT_Ready > -1);
  CHB_SS_Out_Start.Checked      := (data.OUT_Start > -1);
  CHB_SS_Out_Pause.Checked      := (data.OUT_Pause > -1);
  CHB_SS_Out_Stop.Checked       := (data.OUT_Stop > -1);
  CHB_SS_OUT_Opakovani.Checked  := (data.OUT_Repeat > -1);

  SE_SS_Out_Ready.Enabled     := (data.OUT_Ready > -1);
  SE_SS_Out_Start.Enabled     := (data.OUT_Start > -1);
  SE_SS_Out_Pause.Enabled     := (data.OUT_Pause > -1);
  SE_SS_Out_Stop.Enabled      := (data.OUT_Stop > -1);
  SE_SS_Out_Opakovani.Enabled := (data.OUT_Repeat > -1);
  //nacitani enablovani OUT konec

  SE_SS_In_Start.Value   := data.IN_Start;
  SE_SS_In_Pause.Value   := data.IN_Pause;
  SE_SS_In_Stop.Value    := data.IN_Stop;
  SE_SS_In_Repeat.Value  := data.IN_Repeat;
  SE_SS_In_Reset.Value   := data.IN_Reset;

  SE_SS_Out_Ready.Value      := data.OUT_Ready;
  SE_SS_Out_Start.Value      := data.OUT_Start;
  SE_SS_Out_Pause.Value      := data.OUT_Pause;
  SE_SS_Out_Stop.Value       := data.OUT_Stop;
  SE_SS_Out_Opakovani.Value  := data.OUT_Repeat;
 end;//procedure

procedure TF_Options.CB_TrackSystemChange(Sender: TObject);
begin
 Self.CCB_BaudRate.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
 Self.CCB_DataBits.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
 Self.CCB_StopBits.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
 Self.CCB_Port.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
 Self.CCB_FC.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
 Self.B_PortRefresh.Enabled := (Self.CB_TrackSystem.ItemIndex = 0);
end;

procedure TF_Options.CHB_AutosaveClick(Sender: TObject);
begin
 Data.autosave := Self.CHB_Autosave.Checked;
 if (Self.CHB_Autosave.Checked) then
  begin
   try
    Data.autosave_period := EncodeTime(0, StrToInt(LeftStr(ME_autosave_period.Text, 2)), StrToInt(Copy(ME_autosave_period.Text, 4, 2)), 0);
   except
    Data.autosave := false;
   end;
  end;
end;

procedure TF_Options.CHB_SS_EnableClick(Sender: TObject);
begin
 if (Self.CHB_SS_Enable.Checked) then
  begin
   Self.CB_SS_AutRezimy.Enabled := true;
   Self.SE_SS_RCSAdr.Enabled    := true;
   Self.GB_SS_Vstupy.Enabled    := true;
   Self.GB_SS_Vystupy.Enabled   := true;
  end else begin
   Self.CB_SS_AutRezimy.Enabled := false;
   Self.SE_SS_RCSAdr.Enabled    := false;
   Self.GB_SS_Vstupy.Enabled    := false;
   Self.GB_SS_Vystupy.Enabled   := false;
  end;
end;//procedure

procedure TF_Options.CHB_SS_Out_ReadyClick(Sender: TObject);
var data:TSSData;
 begin
  data := SS.GetData();

  if (CHB_SS_In_Start.Checked) then SE_SS_In_Start.Value := data.IN_Start else SE_SS_In_Start.Value := -1;
  if (CHB_SS_In_Pause.Checked) then SE_SS_In_Pause.Value := data.IN_Pause else SE_SS_In_Pause.Value := -1;
  if (CHB_SS_In_Stop.Checked)  then SE_SS_In_Stop.Value := data.IN_Stop else SE_SS_In_Stop.Value := -1;
  if (CHB_SS_In_Repeat.Checked) then SE_SS_In_Repeat.Value := data.IN_Repeat else SE_SS_In_Repeat.Value := -1;
  if (CHB_SS_In_Reset.Checked) then SE_SS_In_Reset.Value := data.IN_Reset else SE_SS_In_Reset.Value := -1;

  if (CHB_SS_Out_Ready.Checked) then SE_SS_Out_Ready.Value := data.OUT_Ready else SE_SS_Out_Ready.Value := -1;
  if (CHB_SS_Out_Start.Checked) then SE_SS_Out_Start.Value := data.OUT_Start else SE_SS_Out_Start.Value := -1;
  if (CHB_SS_Out_Pause.Checked) then SE_SS_Out_Pause.Value := data.OUT_Pause else SE_SS_Out_Pause.Value := -1;
  if (CHB_SS_Out_Stop.Checked)  then SE_SS_Out_Stop.Value  := data.OUT_Stop  else SE_SS_Out_Stop.Value := -1;
  if (CHB_SS_OUT_Opakovani.Checked) then SE_SS_Out_Opakovani.Value := data.OUT_Repeat else SE_SS_Out_Opakovani.Value := -1;

  SE_SS_In_Start.Enabled  := CHB_SS_In_Start.Checked;
  SE_SS_In_Pause.Enabled  := CHB_SS_In_Pause.Checked;
  SE_SS_In_Stop.Enabled   := CHB_SS_In_Stop.Checked;
  SE_SS_In_Repeat.Enabled := CHB_SS_In_Repeat.Checked;
  SE_SS_In_Reset.Enabled  := CHB_SS_In_Reset.Checked;

  SE_SS_Out_Ready.Enabled     := CHB_SS_Out_Ready.Checked;
  SE_SS_Out_Start.Enabled     := CHB_SS_Out_Start.Checked;
  SE_SS_Out_Pause.Enabled     := CHB_SS_Out_Pause.Checked;
  SE_SS_Out_Stop.Enabled      := CHB_SS_Out_Stop.Checked;
  SE_SS_Out_Opakovani.Enabled := CHB_SS_OUT_Opakovani.Checked;
 end;

procedure TF_Options.UlozDataDoSSzObjektu;
var data:TSSData;
 begin
  data.enabled := Self.CHB_SS_Enable.Checked;

  data.RCSAdr         := Self.SE_SS_RCSAdr.Value;
  data.AutRezim       := CB_SS_AutRezimy.ItemIndex;

  if (CHB_SS_In_Start.Checked)  then data.IN_Start  := Self.SE_SS_In_Start.Value else data.IN_Start := -1;
  if (CHB_SS_In_Pause.Checked)  then data.IN_Pause  := Self.SE_SS_In_Pause.Value else data.IN_Pause := -1;
  if (CHB_SS_In_Stop.Checked)   then data.IN_Stop   := Self.SE_SS_In_Stop.Value else data.IN_Stop := -1;
  if (CHB_SS_In_Repeat.Checked) then data.IN_Repeat := Self.SE_SS_In_Repeat.Value else data.IN_Repeat := -1;
  if (CHB_SS_In_Reset.Checked)  then data.IN_Reset  := Self.SE_SS_In_Reset.Value else data.IN_Reset := -1;

  if (CHB_SS_Out_Start.Checked)     then data.OUT_Start  := Self.SE_SS_Out_Start.Value else data.OUT_Start := -1;
  if (CHB_SS_Out_Pause.Checked)     then data.OUT_Pause  := Self.SE_SS_Out_Pause.Value else data.OUT_Pause := -1;
  if (CHB_SS_Out_Stop.Checked)      then data.OUT_Stop   := Self.SE_SS_Out_Stop.Value else data.OUT_Stop := -1;
  if (CHB_SS_Out_Ready.Checked)     then data.OUT_Ready  := Self.SE_SS_Out_Ready.Value else data.OUT_Ready := -1;
  if (CHB_SS_OUT_Opakovani.Checked) then data.OUT_Repeat := Self.SE_SS_Out_Opakovani.Value else data.OUT_Repeat := -1;

  SS.SetData(data);
 end;//procedure

procedure TF_Options.B_SaveClick(Sender: TObject);
begin
 Self.UlozCentralaData();
end;

procedure TF_Options.B_SS_SaveClick(Sender: TObject);
 begin
  UlozDataDoSSzObjektu;
 end;//procedure

procedure TF_Options.LB_TimerClick(Sender: TObject);
 begin
  F_Main.Timer1.Interval := StrToInt(LB_Timer.Items.Strings[LB_Timer.ItemIndex]);
  F_Main.SB1.Panels.Items[_SB_SBERNICE].Text:='Primární smyèka : '+LB_Timer.Items.Strings[LB_Timer.ItemIndex]+' ms';
  writelog('Primární smyèka nastavena na '+LB_Timer.Items.Strings[LB_Timer.ItemIndex]+' ms',WR_MESSAGE);
 end;//procedure

procedure TF_Options.FormShow(Sender: TObject);
 begin
  writelog('Zobrazeno okno nastaveni',WR_MESSAGE);
  Self.PC_1Change(Self);
 end;//procedure

procedure TF_Options.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  writelog('Skryto okno nastaveni',WR_MESSAGE);
 end;//procedure

procedure TF_Options.LV_DigiRychDblClick(Sender: TObject);
 begin
  if (LV_DigiRych.Selected <> nil) then F_RychlostiEdit.OpenForm(LV_DigiRych.ItemIndex);
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_Options.NactiCentralaData();
begin
 Self.CB_TrackSystem.ItemIndex := Integer(TrkSystem.TrkSystem)-1;
 Self.CCB_BaudRate.ItemIndex  := Integer(TrkSystem.BaudRate);
 Self.CCB_DataBits.ItemIndex  := Integer(TrkSystem.DataBits);
 Self.CCB_StopBits.ItemIndex  := Integer(TrkSystem.StopBits);
 Self.CCB_FC.ItemIndex        := Integer(TrkSystem.FlowControl);
 Self.CCB_Port.Text           := TrkSystem.COM;
 Self.CB_TrackSystemChange(Self.CB_TrackSystem);
end;//procedure

procedure TF_Options.UlozCentralaData();
begin
 TrkSystem.TrkSystem := Ttrk_system(Self.CB_TrackSystem.ItemIndex+1);

 if (TrkSystem.TrkSystem <> TRS_Simulator) then
  begin
   TrkSystem.BaudRate    := TBaudRate(Self.CCB_BaudRate.ItemIndex);
   TrkSystem.DataBits    := TDataBits(Self.CCB_DataBits.ItemIndex);
   TrkSystem.StopBits    := TStopBits(Self.CCB_StopBits.ItemIndex);
   TrkSystem.FlowControl := TFlowControl(Self.CCB_FC.ItemIndex);
   TrkSystem.COM         := Self.CCB_Port.Text;
  end;
end;
////////////////////////////////////////////////////////////////////////////////

end.//unit

