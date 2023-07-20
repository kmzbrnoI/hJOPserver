unit fSettings;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, inifiles, Menus, ComCtrls, ExtCtrls, Mask, StrUtils, Buttons,
  fMain, TechnologieRCS, BlockDb, Generics.Collections;

type

  TF_Options = class(TForm)
    B_pouzit: TButton;
    B_OK: TButton;
    PC_1: TPageControl;
    TS_Options: TTabSheet;
    P_ON_Pozadi: TPanel;
    GB_ON_1: TGroupBox;
    E_dataload: TEdit;
    GB_ON_2: TGroupBox;
    GB_PrijmutaData: TGroupBox;
    LB_Timer: TListBox;
    L_Data1: TLabel;
    TS_DigiRych: TTabSheet;
    LV_DigiRych: TListView;
    GB_Log: TGroupBox;
    CHB_Log_console: TCheckBox;
    GB_OnStart: TGroupBox;
    CHB_povolit_spusteni: TCheckBox;
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
    procedure LB_TimerClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LV_DigiRychDblClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure CHB_AutosaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CB_AC_Ids: TList<Integer>;

  public

  end;

var
  F_Options: TF_Options;

implementation

uses fRychlostiEdit, GetSystems, Config, Block,
  AreaDb, BoosterDb, TJCDatabase, THVDatabase,
  Logging, DataBloky, DataJC, DataRCS, Trakce, ModelovyCas;

{$R *.dfm}

procedure TF_Options.FormCreate(Sender: TObject);
begin
  PC_1.ActivePageIndex := 0;
  Self.CB_AC_Ids := TList<Integer>.Create();
end;

procedure TF_Options.FormDestroy(Sender: TObject);
begin
  Self.CB_AC_Ids.Free();
end;

procedure TF_Options.PC_1Change(Sender: TObject);
begin
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
      Application.MessageBox(PChar('Nepodařilo se načíst měřítko:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  Application.ProcessMessages();
  Screen.Cursor := crHourGlass;

  inidata := TMemIniFile.Create(_INIDATA_FN, TEncoding.UTF8);
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
  B_pouzitClick(Self);
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

procedure TF_Options.LB_TimerClick(Sender: TObject);
begin
  F_Main.T_Main.Interval := StrToInt(LB_Timer.Items.Strings[LB_Timer.ItemIndex]);
  Log('Primární smyčka nastavena na ' + LB_Timer.Items.Strings[LB_Timer.ItemIndex] + ' ms', TLogLevel.llInfo);
end;

procedure TF_Options.FormShow(Sender: TObject);
begin
  Self.PC_1Change(Self);
end;

procedure TF_Options.LV_DigiRychDblClick(Sender: TObject);
begin
  if (LV_DigiRych.Selected <> nil) then
    F_RychlostiEdit.OpenForm(LV_DigiRych.ItemIndex);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
