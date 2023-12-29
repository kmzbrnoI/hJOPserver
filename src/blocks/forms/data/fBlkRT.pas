unit fBlkRT;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, BlockDb, Block, Mask, fTrainSpeed,
  BlockRailwayTrack, StrUtils, BlockTrack, fBlkRTStopEvent, Generics.Collections;

type
  TF_BlkRT = class(TForm)
    B_OK: TButton;
    B_Storno: TButton;
    L_Usek02: TLabel;
    SE_ID: TSpinEdit;
    E_Name: TEdit;
    L_Usek01: TLabel;
    GB_RCS: TGroupBox;
    L_Usek04: TLabel;
    SE_Port0: TSpinEdit;
    L_Usek15: TLabel;
    E_Length: TEdit;
    Label1: TLabel;
    CB_Booster: TComboBox;
    SE_Module0: TSpinEdit;
    CHB_D0: TCheckBox;
    Label2: TLabel;
    CHB_D1: TCheckBox;
    SE_Module1: TSpinEdit;
    SE_Port1: TSpinEdit;
    Label3: TLabel;
    CHB_D2: TCheckBox;
    SE_Module2: TSpinEdit;
    SE_Port2: TSpinEdit;
    Label4: TLabel;
    CHB_D3: TCheckBox;
    SE_Module3: TSpinEdit;
    SE_Port3: TSpinEdit;
    GB_Stop: TGroupBox;
    CHB_Stop_Odd: TCheckBox;
    Label5: TLabel;
    E_Stop_Trains: TEdit;
    Label6: TLabel;
    SE_Stop_Length: TSpinEdit;
    Label7: TLabel;
    ME_Stop_Delay: TMaskEdit;
    CHB_loop: TCheckBox;
    L_Usek33: TLabel;
    GB_Autoblok: TGroupBox;
    CHB_SignalL: TCheckBox;
    CB_SignalL: TComboBox;
    CHB_SignalS: TCheckBox;
    CB_SignalS: TComboBox;
    PC_Stop: TPageControl;
    TS_Zast_lichy: TTabSheet;
    TS_Zast_sudy: TTabSheet;
    CHB_Stop_Even: TCheckBox;
    GB_Speeds: TGroupBox;
    GB_SpeedsL: TGroupBox;
    GB_SpeedsS: TGroupBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D0Click(Sender: TObject);
    procedure CHB_Stop_OddClick(Sender: TObject);
    procedure CHB_SignalLClick(Sender: TObject);
    procedure CHB_SignalSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SE_RCS_BoardExit(Sender: TObject);
  private
    isNewBlock: Boolean;
    block: TBlkRT;
    openIndex: Integer;
    CB_SignalId: TList<Integer>;
    CB_SignalOddIndex, CB_SignalEvenIndex: Integer;
    stopEven, stopOdd: TF_BlkRTStopEvent;

    fTrainSpeedL: TF_TrainSpeed;
    fTrainSpeedS: TF_TrainSpeed;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

  public
    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkRT: TF_BlkRT;

implementation

uses GetSystems, TechnologieRCS, BoosterDb, DataBloky, ownStrUtils,
  AreaDb, Booster, Area, TrainSpeed;

{$R *.dfm}

procedure TF_BlkRT.FormCreate(Sender: TObject);
begin
  Self.CB_SignalId := TList<Integer>.Create();

  Self.stopEven := TF_BlkRTStopEvent.Create(nil);
  Self.stopEven.Parent := Self.TS_Zast_sudy;
  Self.stopEven.Color := clWhite;
  Self.stopEven.Show();

  Self.stopOdd := TF_BlkRTStopEvent.Create(nil);
  Self.stopOdd.Parent := Self.TS_Zast_lichy;
  Self.stopOdd.Color := clWhite;
  Self.stopOdd.Show();

  Self.fTrainSpeedL := TF_TrainSpeed.Create(nil);
  Self.fTrainSpeedL.Parent := Self.GB_SpeedsL;
  Self.fTrainSpeedL.Align := alClient;
  Self.fTrainSpeedL.Show();

  Self.fTrainSpeedS := TF_TrainSpeed.Create(nil);
  Self.fTrainSpeedS.Parent := Self.GB_SpeedsS;
  Self.fTrainSpeedS.Align := alClient;
  Self.fTrainSpeedS.Show();
end;

procedure TF_BlkRT.FormDestroy(Sender: TObject);
begin
  Self.stopEven.Free();
  Self.stopOdd.Free();
  Self.CB_SignalId.Free();

  Self.fTrainSpeedL.Free();
  Self.fTrainSpeedS.Free();
end;

procedure TF_BlkRT.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkRT;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkRT.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkRT.SE_RCS_BoardExit(Sender: TObject);
begin
  Self.SE_Port0.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Module0.Value, Self.SE_Port0.Value);
  Self.SE_Port1.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Module1.Value, Self.SE_Port1.Value);
  Self.SE_Port2.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Module2.Value, Self.SE_Port2.Value);
  Self.SE_Port3.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Module3.Value, Self.SE_Port3.Value);
end;

procedure TF_BlkRT.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.E_Length.Text := '0';
  Self.CHB_loop.Checked := false;
  Self.CB_Booster.ItemIndex := -1;

  Self.SE_Port0.Value := 0;
  Self.SE_Module0.Value := 1;
  Self.SE_Port1.Value := 0;
  Self.SE_Module1.Value := 1;
  Self.SE_Port2.Value := 0;
  Self.SE_Module2.Value := 1;
  Self.SE_Port3.Value := 0;
  Self.SE_Module3.Value := 1;
  Self.SE_RCS_BoardExit(Self);

  Self.CHB_D0.Checked := true;
  Self.CHB_D0Click(Self.CHB_D0);

  Self.CHB_D1.Checked := false;
  Self.CHB_D0Click(Self.CHB_D1);

  Self.CHB_Stop_Odd.Checked := false;
  Self.CHB_Stop_Even.Checked := false;
  Self.CHB_Stop_OddClick(Self);

  Blocks.FillCB(Self.CB_SignalL, Self.CB_SignalId, nil, nil, btSignal);
  Blocks.FillCB(Self.CB_SignalS, Self.CB_SignalId, nil, nil, btSignal);
  Self.CB_SignalOddIndex := -1;
  Self.CB_SignalEvenIndex := -1;

  Self.fTrainSpeedL.Default();
  Self.fTrainSpeedS.Clear();

  Self.stopEven.OpenEmptyForm();
  Self.stopOdd.OpenEmptyForm();

  Self.Caption := 'Nový blok Traťový úsek';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkRT.EditOpenForm();
var glob: TBlkSettings;
  TUsettings: TBlkRTSettings;
  Usettings: TBlkTrackSettings;
begin
  glob := Self.block.GetGlobalSettings();
  TUsettings := Self.block.GetSettings();
  Usettings := TBlkTrack(Self.block).GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.CHB_D0.Checked := false;
  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;

  case (Usettings.RCSAddrs.count) of
    0, 1:
      begin
        Self.CHB_D0.Checked := true;
        Self.CHB_D0Click(Self.CHB_D0);
      end;
    2:
      begin
        Self.CHB_D1.Checked := true;
        Self.CHB_D0Click(Self.CHB_D1);
      end;
    3:
      begin
        Self.CHB_D2.Checked := true;
        Self.CHB_D0Click(Self.CHB_D2);
      end;
    4:
      begin
        Self.CHB_D3.Checked := true;
        Self.CHB_D0Click(Self.CHB_D3);
      end;
  end; // case

  if (Usettings.RCSAddrs.count > 0) then
  begin
    if (Usettings.RCSAddrs[0].board > Cardinal(Self.SE_Module0.MaxValue)) then
      Self.SE_Module0.MaxValue := 0;
    Self.SE_Port0.MaxValue := 0;

    Self.SE_Port0.Value := Usettings.RCSAddrs[0].port;
    Self.SE_Module0.Value := Usettings.RCSAddrs[0].board;
  end else begin
    Self.SE_Port0.Value := 0;
    Self.SE_Module0.Value := 0;
  end;

  if (Usettings.RCSAddrs.count > 1) then
  begin
    if (Usettings.RCSAddrs[1].board > Cardinal(Self.SE_Module1.MaxValue)) then
      Self.SE_Module1.MaxValue := 0;
    Self.SE_Port1.MaxValue := 0;

    Self.SE_Port1.Value := Usettings.RCSAddrs[1].port;
    Self.SE_Module1.Value := Usettings.RCSAddrs[1].board;
  end else begin
    Self.SE_Port1.Value := 0;
    Self.SE_Module1.Value := 0;
  end;

  if (Usettings.RCSAddrs.count > 2) then
  begin
    if (Usettings.RCSAddrs[2].board > Cardinal(Self.SE_Module2.MaxValue)) then
      Self.SE_Module2.MaxValue := 0;
    Self.SE_Port2.MaxValue := 0;

    Self.SE_Port2.Value := Usettings.RCSAddrs[2].port;
    Self.SE_Module2.Value := Usettings.RCSAddrs[2].board;
  end else begin
    Self.SE_Port2.Value := 0;
    Self.SE_Module2.Value := 0;
  end;

  if (Usettings.RCSAddrs.count > 3) then
  begin
    if (Usettings.RCSAddrs[3].board > Cardinal(Self.SE_Module3.MaxValue)) then
      Self.SE_Module3.MaxValue := 0;
    Self.SE_Port3.MaxValue := 0;

    Self.SE_Port3.Value := Usettings.RCSAddrs[3].port;
    Self.SE_Module3.Value := Usettings.RCSAddrs[3].board;
  end else begin
    Self.SE_Port3.Value := 0;
    Self.SE_Module3.Value := 0;
  end;

  Self.SE_RCS_BoardExit(Self);

  Self.CB_Booster.ItemIndex := -1;
  for var i := 0 to Boosters.sorted.count - 1 do
  begin
    if (Boosters.sorted[i].id = Usettings.boosterId) then
    begin
      Self.CB_Booster.ItemIndex := i;
      break;
    end;
  end;

  Self.E_Length.Text := FloatToStr(Usettings.lenght);
  Self.CHB_loop.Checked := Usettings.loop;

  Self.CHB_Stop_Odd.Checked := Assigned(TUsettings.stop) and Assigned(TUsettings.stop.evL);
  Self.CHB_Stop_Even.Checked := Assigned(TUsettings.stop) and Assigned(TUsettings.stop.evS);
  Self.CHB_Stop_OddClick(Self);

  Blocks.FillCB(Self.CB_SignalL, Self.CB_SignalId, nil, nil, btSignal, btAny, TUsettings.signalLid);
  Blocks.FillCB(Self.CB_SignalS, Self.CB_SignalId, nil, nil, btSignal, btAny, TUsettings.signalSid);
  Self.CB_SignalOddIndex := Self.CB_SignalL.ItemIndex;
  Self.CB_SignalEvenIndex := Self.CB_SignalS.ItemIndex;

  Self.CHB_SignalL.Checked := (TUsettings.signalLid <> -1);
  Self.CHB_SignalS.Checked := (TUsettings.signalSid <> -1);
  Self.CHB_SignalLClick(CHB_SignalL);
  Self.CHB_SignalSClick(CHB_SignalS);

  Self.fTrainSpeedL.Fill(TUsettings.speedsL);
  Self.fTrainSpeedS.Fill(TUsettings.speedsS);

  if (TUsettings.stop <> nil) then
  begin
    if (TUsettings.stop.evL <> nil) then
      Self.stopOdd.OpenForm(TUsettings.stop.evL);
    if (TUsettings.stop.evS <> nil) then
      Self.stopEven.OpenForm(TUsettings.stop.evS);
  end;

  Self.Caption := 'Upravit blok ' + glob.name + ' (traťový úsek)';
  Self.ActiveControl := Self.B_OK;
end;

procedure TF_BlkRT.CommonOpenForm;
begin
  Self.SE_Module0.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Module1.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Module2.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Module3.MaxValue := RCSi.maxModuleAddrSafe;

  Self.CB_Booster.Clear();
  for var booster in Boosters.sorted do
    Self.CB_Booster.Items.Add(booster.name + ' (' + booster.id + ')');
end;

procedure TF_BlkRT.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkRT.B_OKClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, openIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Booster.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte zesilovač, kterému patří blok!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_SignalL.Checked) and (Self.CB_SignalL.ItemIndex = -1)) then
  begin
    Application.MessageBox('Vyberte návěstidlo kryjící úsek v lichém směru!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_SignalS.Checked) and (Self.CB_SignalS.ItemIndex = -1)) then
  begin
    Application.MessageBox('Vyberte návěstidlo kryjící úsek v sudém směru!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (CHB_Stop_Even.Checked) then
  begin
    var str := Self.stopEven.Check();
    if (str <> '') then
    begin
      Application.MessageBox(PChar('Zastavovací událost zastávky v sudém směru:' + #13#10 + str), 'Nelze uložit data',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  if (CHB_Stop_Odd.Checked) then
  begin
    var str := Self.stopOdd.Check();
    if (str <> '') then
    begin
      Application.MessageBox(PChar('Zastavovací událost zastávky v lichém směru:' + #13#10 + str), 'Nelze uložit data',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  if (Self.fTrainSpeedL.LV_Speeds.Items.Count = 0) then
  begin
    Application.MessageBox('Nezadány traťové rychlosti (zadejte alespoň pro lichý směr)!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;


  var speedsL: TList<TTrainSpeed> := Self.fTrainSpeedL.Get();
  var speedsS: TList<TTrainSpeed> := Self.fTrainSpeedS.Get();

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btRT;

  if (Self.isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkRT;
    except
      on E: Exception do
      begin
        speedsL.Free();
        speedsS.Free();
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.block.SetGlobalSettings(glob);
  end;

  var settings: TBlkTrackSettings;
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_D0.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Module0.Value, Self.SE_Port0.Value));
  if (Self.CHB_D1.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Module1.Value, Self.SE_Port1.Value));
  if (Self.CHB_D2.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Module2.Value, Self.SE_Port2.Value));
  if (Self.CHB_D3.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Module3.Value, Self.SE_Port3.Value));

  settings.lenght := StrToFloatDef(Self.E_Length.Text, 0);
  settings.loop := Self.CHB_loop.Checked;
  settings.boosterId := Boosters.sorted[Self.CB_Booster.ItemIndex].id;
  settings.maxTrains := 1;

  var TUsettings: TBlkRTSettings;
  TUsettings.speedsL := speedsL;
  TUsettings.speedsS := speedsS;

  if (Self.CHB_SignalL.Checked) then
    TUsettings.signalLid := Self.CB_SignalId[Self.CB_SignalL.ItemIndex]
  else
    TUsettings.signalLid := -1;

  if (Self.CHB_SignalS.Checked) then
    TUsettings.signalSid := Self.CB_SignalId[Self.CB_SignalS.ItemIndex]
  else
    TUsettings.signalSid := -1;

  if ((Self.CHB_Stop_Odd.Checked) or (Self.CHB_Stop_Even.Checked)) then
  begin
    TUsettings.stop := TBlkRTStop.Create();
    TUsettings.stop.trainType := '^' + Self.E_Stop_Trains.Text + '$';
    TUsettings.stop.maxLength := Self.SE_Stop_Length.Value;
    try
      TUsettings.stop.delay := EncodeTime(0, StrToInt(LeftStr(Self.ME_Stop_Delay.Text, 2)),
        StrToInt(RightStr(Self.ME_Stop_Delay.Text, 2)), 0);
    except
      speedsL.Free();
      speedsS.Free();
      TUsettings.stop.Free();
      Application.MessageBox('Nesprávně zadaný čas čekání v zastávce!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    if (Self.CHB_Stop_Odd.Checked) then
      TUsettings.stop.evL := Self.stopOdd.GetEvent()
    else
      TUsettings.stop.evL := nil;

    if (Self.CHB_Stop_Even.Checked) then
      TUsettings.stop.evS := Self.stopEven.GetEvent()
    else
      TUsettings.stop.evS := nil;
  end
  else
    TUsettings.stop := nil;

  settings.houkEvL := TBlkTrack(Self.block).GetSettings().houkEvL;
  settings.houkEvS := TBlkTrack(Self.block).GetSettings().houkEvS;

  Self.block.SetSettings(TUsettings);
  (Self.block as TBlkTrack).SetSettings(settings);

  Self.Close();
  Self.block.Change();
end;

procedure TF_BlkRT.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.openIndex := -1;
  Self.isNewBlock := false;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkRT.CHB_D0Click(Sender: TObject);
begin
  case ((Sender as TCheckBox).Tag) of
    0:
      begin
        Self.SE_Port0.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Module0.Enabled := (Sender as TCheckBox).Checked;
      end;

    1:
      begin
        Self.SE_Port1.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Module1.Enabled := (Sender as TCheckBox).Checked;
      end;

    2:
      begin
        Self.SE_Port2.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Module2.Enabled := (Sender as TCheckBox).Checked;
      end;

    3:
      begin
        Self.SE_Port3.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Module3.Enabled := (Sender as TCheckBox).Checked;
      end;
  end; // case

  if ((Sender as TCheckBox).Checked) then
  begin
    // checked
    case ((Sender as TCheckBox).Tag) of
      1:
        Self.CHB_D0.Checked := true;
      2:
        Self.CHB_D1.Checked := true;
      3:
        Self.CHB_D2.Checked := true;
    end;
  end else begin
    // not checked
    case ((Sender as TCheckBox).Tag) of
      0:
        Self.CHB_D1.Checked := false;
      1:
        Self.CHB_D2.Checked := false;
      2:
        Self.CHB_D3.Checked := false;
    end;
  end;

end;

procedure TF_BlkRT.CHB_SignalLClick(Sender: TObject);
begin
  Self.CB_SignalL.Enabled := Self.CHB_SignalL.Checked;
  if (not Self.CHB_SignalL.Checked) then
    Self.CB_SignalL.ItemIndex := -1
  else
    Self.CB_SignalL.ItemIndex := Self.CB_SignalOddIndex;
end;

procedure TF_BlkRT.CHB_SignalSClick(Sender: TObject);
begin
  Self.CB_SignalS.Enabled := Self.CHB_SignalS.Checked;
  if (not Self.CHB_SignalS.Checked) then
    Self.CB_SignalS.ItemIndex := -1
  else
    Self.CB_SignalS.ItemIndex := Self.CB_SignalEvenIndex;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkRT.CHB_Stop_OddClick(Sender: TObject);
var zast: TBlkRTStop;
begin
  if ((Self.CHB_Stop_Odd.Checked) or (Self.CHB_Stop_Even.Checked)) then
  begin
    Self.E_Stop_Trains.Enabled := true;
    Self.SE_Stop_Length.Enabled := true;
    Self.ME_Stop_Delay.Enabled := true;
    Self.PC_Stop.Enabled := true;

    if ((Assigned(Self.block)) and (Self.block.GetSettings.stop <> nil)) then
    begin
      zast := Self.block.GetSettings.stop;
      Self.E_Stop_Trains.Text := Copy(zast.trainType, 2, Length(zast.trainType) - 2);
      Self.SE_Stop_Length.Value := zast.maxLength;
      Self.ME_Stop_Delay.Text := FormatDateTime('nn:ss', zast.delay);
    end;
  end else begin
    Self.E_Stop_Trains.Enabled := false;
    Self.SE_Stop_Length.Enabled := false;
    Self.ME_Stop_Delay.Enabled := false;
    Self.PC_Stop.Enabled := false;
  end;

  Self.TS_Zast_lichy.TabVisible := Self.CHB_Stop_Odd.Checked;
  Self.TS_Zast_sudy.TabVisible := Self.CHB_Stop_Even.Checked;

  if (((not Self.CHB_Stop_Odd.Checked) and ((not Self.CHB_Stop_Even.Checked))) or (not Assigned(Self.block)))
  then
  begin
    Self.E_Stop_Trains.Text := '.*';
    Self.SE_Stop_Length.Value := 0;
    Self.ME_Stop_Delay.Text := '00:00';
    Self.PC_Stop.Enabled := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
