unit fSettings;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, inifiles, Menus, ComCtrls, ExtCtrls, Mask, StrUtils, Buttons,
  fMain, TechnologieRCS, BlockDb;

type

  TF_Options = class(TForm)
    //Ukladani
    B_pouzit: TButton;
    B_OK: TButton;
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
    GB_Autosave: TGroupBox;
    CHB_Autosave: TCheckBox;
    ME_autosave_period: TMaskEdit;
    Label1: TLabel;
    GB_Scale: TGroupBox;
    Label2: TLabel;
    E_Scale: TEdit;
    Label3: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure PC_1Change(Sender: TObject);
    procedure B_pouzitClick(Sender: TObject);
    procedure PM_Data_SaveClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CHB_SS_Out_ReadyClick(Sender: TObject);
    procedure B_SS_SaveClick(Sender: TObject);
    procedure LB_TimerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LV_DigiRychDblClick(Sender: TObject);
    procedure CHB_SS_EnableClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure CHB_AutosaveClick(Sender: TObject);
  private
    cb_ac_arri: TArI;

  public

    procedure NactiSSDoObjektu;
    procedure UlozDataDoSSzObjektu;

  end;

var
  F_Options: TF_Options;

implementation

uses fRychlostiEdit, GetSystems, FileSystem, Block,
     AreaDb, BoosterDb, SnadnSpusteni, TJCDatabase, THVDatabase,
     Logging, DataBloky, DataJC, DataRCS, Trakce, ModelovyCas;

{$R *.dfm}

procedure TF_Options.FormCreate(Sender: TObject);
begin
 PC_1.ActivePageIndex := 0;
end;

procedure TF_Options.PC_1Change(Sender: TObject);
begin
 if (PC_1.ActivePage = TS_SS) then NactiSSDoObjektu();
 if (PC_1.ActivePage = TS_Options) then
  begin
   Self.ME_autosave_period.Text := FormatDateTime('nn:ss', GlobalConfig.autosave_period);
   Self.CHB_Autosave.Checked := GlobalConfig.autosave;
   Self.E_Scale.Text := IntToStr(GlobalConfig.scale);
  end;
end;

procedure TF_Options.B_OKClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_Options.B_pouzitClick(Sender: TObject);
var inidata: TMemIniFile;
 begin
  try
    GlobalConfig.scale := StrToInt(Self.E_Scale.Text);
  except
    on E: Exception do
     begin
      Application.MessageBox(PChar('Nepodařilo se načíst měřítko:'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
  end;

  Application.ProcessMessages();
  Screen.Cursor := crHourGlass;

  inidata := TMeminifile.Create(_INIDATA_FN, TEncoding.UTF8);
  try
    Config.CompleteSaveToFile(inidata);
  finally
    inidata.UpdateFile();
    inidata.Free();
  end;

  Screen.Cursor := crDefault;
  F_Options.ActiveControl := B_OK;
 end;

procedure TF_Options.PM_Data_SaveClick(Sender: TObject);
 begin
  B_pouzitClick(self);
 end;

procedure TF_Options.FormResize(Sender: TObject);
begin
 SE_SS_RCSAdr.Left    := (PC_1.Width div 2) - (SE_SS_RCSAdr.Width div 2);
 CB_SS_AutRezimy.Left := SE_SS_RCSAdr.Left;
 GB_SS_Vstupy.Left    := (PC_1.Width div 2) - (GB_SS_Vstupy.Width)-5;
 GB_SS_Vystupy.Left   := (PC_1.Width div 2) + 5;
 CHB_SS_Enable.Left   := (PC_1.Width div 2) - (CHB_SS_Enable.Width div 2);
 L_SS_01.Left         := SE_SS_RCSAdr.Left;
 L_SS_02.Left         := SE_SS_RCSAdr.Left;
 
 B_SS_Save.Left := P_SS.Width-B_SS_Save.Width-10;
 B_SS_Save.Top  := P_SS.Height-B_SS_Save.Height-10;
end;

procedure TF_Options.NactiSSDoObjektu();
var IgnoraceRCS: TArI;
    data: TSSData;
 begin
  data := SS.GetData();

  SetLength(IgnoraceRCS, 2);
  IgnoraceRCS[0] := 3;
  IgnoraceRCS[1] := 4;

  Self.SE_SS_RCSAdr.Value := data.RCSAdr;

  F_Options.CB_SS_AutRezimy.Clear();
  Blocks.FillCB(Self.CB_SS_AutRezimy, @Self.cb_ac_arri, nil, nil, btAC, data.AC_id);

  Self.CHB_SS_Enable.Checked := data.enabled;
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
 end;

procedure TF_Options.CHB_AutosaveClick(Sender: TObject);
begin
 GlobalConfig.autosave := Self.CHB_Autosave.Checked;
 if (Self.CHB_Autosave.Checked) then
  begin
   try
    GlobalConfig.autosave_period := EncodeTime(0, StrToInt(LeftStr(ME_autosave_period.Text, 2)),
                                               StrToInt(Copy(ME_autosave_period.Text, 4, 2)), 0);
   except
    GlobalConfig.autosave := false;
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
end;

procedure TF_Options.CHB_SS_Out_ReadyClick(Sender: TObject);
var data: TSSData;
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
var data: TSSData;
 begin
  data.enabled := Self.CHB_SS_Enable.Checked;

  data.RCSAdr := Self.SE_SS_RCSAdr.Value;
  data.AC_id := Blocks.GetBlkID(Self.cb_ac_arri[Self.CB_SS_AutRezimy.ItemIndex]);

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
 end;

procedure TF_Options.B_SS_SaveClick(Sender: TObject);
 begin
  UlozDataDoSSzObjektu;
 end;

procedure TF_Options.LB_TimerClick(Sender: TObject);
 begin
  F_Main.T_Main.Interval := StrToInt(LB_Timer.Items.Strings[LB_Timer.ItemIndex]);
  F_Main.SB1.Panels.Items[_SB_SBERNICE].Text:='Primární smyčka : '+LB_Timer.Items.Strings[LB_Timer.ItemIndex]+' ms';
  writelog('Primární smyčka nastavena na '+LB_Timer.Items.Strings[LB_Timer.ItemIndex]+' ms', WR_MESSAGE);
 end;

procedure TF_Options.FormShow(Sender: TObject);
 begin
  Self.PC_1Change(Self);
 end;

procedure TF_Options.LV_DigiRychDblClick(Sender: TObject);
 begin
  if (LV_DigiRych.Selected <> nil) then F_RychlostiEdit.OpenForm(LV_DigiRych.ItemIndex);
 end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

