unit fBlkCrossing;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Spin, StdCtrls, BlockCrossing, BlockDb, Generics.Collections,
  BlockCrossingLogic, Mask, StrUtils;

type
  TF_BlkCrossing = class(TForm)
    L_Name: TLabel;
    E_Name: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_ID: TLabel;
    SE_ID: TSpinEdit;
    GB_RCS: TGroupBox;
    GB_RCS_Out: TGroupBox;
    L_P04: TLabel;
    L_P05: TLabel;
    SE_out_open_port: TSpinEdit;
    SE_out_close_port: TSpinEdit;
    GB_RCS_In: TGroupBox;
    L_P07: TLabel;
    L_P08: TLabel;
    L_P09: TLabel;
    L_P10: TLabel;
    SE_in_close_port: TSpinEdit;
    SE_in_open_port: TSpinEdit;
    SE_in_caution_port: TSpinEdit;
    SE_in_annulation_port: TSpinEdit;
    L_P01: TLabel;
    SE_out_open_board: TSpinEdit;
    SE_out_close_board: TSpinEdit;
    Label1: TLabel;
    SE_in_close_board: TSpinEdit;
    SE_in_open_board: TSpinEdit;
    SE_in_caution_board: TSpinEdit;
    SE_in_annulation_board: TSpinEdit;
    GB_JOP_control: TGroupBox;
    CHB_JOP_control: TCheckBox;
    Label2: TLabel;
    CB_Track: TComboBox;
    B_Track_Delete: TButton;
    GB_Track: TGroupBox;
    Label3: TLabel;
    E_Track_Left_Out: TEdit;
    Label4: TLabel;
    E_Track_Left: TEdit;
    Label5: TLabel;
    E_Track_Middle: TEdit;
    Label6: TLabel;
    E_Track_Right: TEdit;
    Label7: TLabel;
    E_Track_Right_Out: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    ME_Track_Anul_Time: TMaskEdit;
    CHB_RCS_Anullation: TCheckBox;
    CHB_RCS_NOT: TCheckBox;
    Label10: TLabel;
    CHB_RCS_Positive: TCheckBox;
    SE_out_positive_board: TSpinEdit;
    SE_out_positive_port: TSpinEdit;
    CB_Track_Open_RL: TComboBox;
    CB_Track_Open_LR: TComboBox;
    Label11: TLabel;
    CHB_RCS_Close: TCheckBox;
    CHB_RCS_Closed: TCheckBox;
    CHB_RCS_Open: TCheckBox;
    CHB_RCS_Caution: TCheckBox;
    Label12: TLabel;
    CB_Positive_Type: TComboBox;
    Label13: TLabel;
    CHB_RCS_Barriers_Down: TCheckBox;
    SE_out_barriers_down_board: TSpinEdit;
    SE_out_barriers_down_port: TSpinEdit;
    Label14: TLabel;
    CHB_RCS_Barriers_Up: TCheckBox;
    SE_out_barriers_up_board: TSpinEdit;
    SE_out_barriers_up_port: TSpinEdit;
    Label15: TLabel;
    CHB_RCS_Ring: TCheckBox;
    SE_out_ring_board: TSpinEdit;
    SE_out_ring_port: TSpinEdit;
    CHB_Ring_Active_Down: TCheckBox;
    SE_Prering_Time: TSpinEdit;
    Label16: TLabel;
    SE_out_lights_port: TSpinEdit;
    SE_out_lights_board: TSpinEdit;
    CHB_RCS_Lights: TCheckBox;
    Label17: TLabel;
    CHB_Closed_Required: TCheckBox;
    CHB_InfiniteAnul: TCheckBox;
    GB_Positive: TGroupBox;
    M_Positive_Ids: TMemo;
    M_Positive_Names: TMemo;
    Label19: TLabel;
    Label18: TLabel;
    B_Positive_Name_To_Ids: TButton;
    B_Positive_Help: TButton;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RCS_boardExit(Sender: TObject);
    procedure CHB_JOP_controlClick(Sender: TObject);
    procedure CB_TrackChange(Sender: TObject);
    procedure B_Track_DeleteClick(Sender: TObject);
    procedure CHB_RCS_AnullationClick(Sender: TObject);
    procedure CHB_RCS_NOTClick(Sender: TObject);
    procedure CHB_RCS_PositiveClick(Sender: TObject);
    procedure CHB_RCS_CloseClick(Sender: TObject);
    procedure CHB_RCS_ClosedClick(Sender: TObject);
    procedure CHB_RCS_OpenClick(Sender: TObject);
    procedure CHB_RCS_CautionClick(Sender: TObject);
    procedure CHB_RCS_Barriers_DownClick(Sender: TObject);
    procedure CHB_RCS_Barriers_UpClick(Sender: TObject);
    procedure CHB_RCS_RingClick(Sender: TObject);
    procedure CHB_RCS_LightsClick(Sender: TObject);
    procedure CHB_InfiniteAnulClick(Sender: TObject);
    procedure B_Positive_Name_To_IdsClick(Sender: TObject);
    procedure B_Positive_HelpClick(Sender: TObject);
  private
    openIndex: Integer;
    block: TBlkCrossing;
    isNewBlock: Boolean;
    tracks: TObjectList<TBlkCrossingTrack>;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();
    procedure SaveTracks();

  public

    procedure EditBlock(BlokIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkCrossing: TF_BlkCrossing;

implementation

uses GetSystems, TechnologieRCS, AreaDb, Area, Block, DataBloky, Config,
  BlockCrossingPositive, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkCrossing.EditBlock(BlokIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := BlokIndex;
  Self.block := Blocks.GetBlkByIndex(BlokIndex) as TBlkCrossing;
  if (block = nil) then
    raise Exception.Create('Blok #'+IntToStr(BlokIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkCrossing.SE_RCS_boardExit(Sender: TObject);
begin
  Self.SE_out_close_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_close_board.Value, Self.SE_out_close_port.Value);
  Self.SE_out_open_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_open_board.Value, Self.SE_out_open_port.Value);
  Self.SE_out_positive_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_positive_board.Value, Self.SE_out_positive_port.Value);
  Self.SE_out_barriers_down_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_barriers_down_board.Value, Self.SE_out_barriers_down_port.Value);
  Self.SE_out_barriers_up_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_barriers_up_board.Value, Self.SE_out_barriers_up_port.Value);
  Self.SE_out_ring_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_ring_board.Value, Self.SE_out_ring_port.Value);
  Self.SE_out_lights_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_out_lights_board.Value, Self.SE_out_lights_port.Value);

  Self.SE_in_close_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_in_close_board.Value, Self.SE_in_close_port.Value);
  Self.SE_in_open_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_in_open_board.Value, Self.SE_in_open_port.Value);
  Self.SE_in_caution_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_in_caution_board.Value, Self.SE_in_caution_port.Value);
  Self.SE_in_annulation_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_in_annulation_board.Value, Self.SE_in_annulation_port.Value);
end;

procedure TF_BlkCrossing.B_Positive_HelpClick(Sender: TObject);
begin
  Application.MessageBox(PChar(BlockCrossingPositive.HELP), 'Nápověda', MB_OK OR MB_ICONINFORMATION);
end;

procedure TF_BlkCrossing.B_Positive_Name_To_IdsClick(Sender: TObject);
begin
  var rules: TPositiveRules := TPositiveRules.Create();
  try
    try
      for var line: string in Self.M_Positive_Names.Lines do
        rules.Add(TPositiveRule.Create(line, True));
    except
      on E:Exception do
      begin
        Application.MessageBox(PChar(E.Message), 'Chyba převodu', MB_OK OR MB_ICONERROR);
        Exit();
      end;
    end;

    Self.M_Positive_Ids.Clear();
    for var rule: TPositiveRule in rules do
      Self.M_Positive_Ids.Lines.Add(rule.IdStr());
  finally
    rules.Free();
  end;
end;

procedure TF_BlkCrossing.B_save_PClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název přejezdu', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var positiveRules := TPositiveRules.Create();
  for var line: string in Self.M_Positive_Ids.Lines do
  begin
    try
      positiveRules.Add(TPositiveRule.Create(line));
    except
      on E:Exception do
      begin
        ExceptionMessageBox('Nepodařilo se načíst pravidlo pozitivy: '+line, 'Nelze uložit data', E);
        positiveRules.Free();
        Exit();
      end;
    end;
  end;

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btCrossing;

  if (Self.isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkCrossing;
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nepodařilo se přidat blok.', 'Nelze uložit data', E);
        Exit();
      end;
    end;
  end else begin
    try
      Self.block.SetGlobalSettings(glob);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nepodařilo se uložit blok.', 'Nelze uložit data', E);
        Exit();
      end;
    end;
  end;

  var settings: TBlkCrossingSettings;
  var addrs := TList<TRCSAddr>.Create();
  try
    try
      settings.RCSOutputs.close := RCSOptionalFromUI(Self.CHB_RCS_Close, Self.SE_out_close_board, Self.SE_out_close_port, addrs);
      settings.RCSOutputs.emOpen := RCSOptionalFromUI(Self.CHB_RCS_NOT, Self.SE_out_open_board, Self.SE_out_open_port, addrs);
      settings.RCSOutputs.positive := RCSOptionalFromUI(Self.CHB_RCS_Positive, Self.SE_out_positive_board, Self.SE_out_positive_port, addrs);
      settings.RCSOutputs.barriersDown := RCSOptionalFromUI(Self.CHB_RCS_Barriers_Down, Self.SE_out_barriers_down_board, Self.SE_out_barriers_down_port, addrs);
      settings.RCSOutputs.barriersUp := RCSOptionalFromUI(Self.CHB_RCS_Barriers_Up, Self.SE_out_barriers_up_board, Self.SE_out_barriers_up_port, addrs);
      settings.RCSOutputs.bell := RCSOptionalFromUI(Self.CHB_RCS_Ring, Self.SE_out_ring_board, Self.SE_out_ring_port, addrs);
      settings.RCSOutputs.lights := RCSOptionalFromUI(Self.CHB_RCS_Lights, Self.SE_out_lights_board, Self.SE_out_lights_port, addrs);

      settings.RCSInputs.open := RCSOptionalFromUI(Self.CHB_RCS_Open, Self.SE_in_open_board, Self.SE_in_open_port, addrs);
      settings.RCSInputs.closed := RCSOptionalFromUI(Self.CHB_RCS_Closed, Self.SE_in_close_board, Self.SE_in_close_port, addrs);
      settings.RCSInputs.caution := RCSOptionalFromUI(Self.CHB_RCS_Caution, Self.SE_in_caution_board, Self.SE_in_caution_port, addrs);
      settings.RCSInputs.annulation := RCSOptionalFromUI(Self.CHB_RCS_Anullation, Self.SE_in_annulation_board, Self.SE_in_annulation_port, addrs);

      settings.RCSOutputs.positiveInvert := (Self.CB_Positive_Type.ItemIndex = 1);
      settings.RCSOutputs.positiveFlick := (Self.CB_Positive_Type.ItemIndex = 2);
      settings.RCSOutputs.bellActiveDown := Self.CHB_Ring_Active_Down.Checked;

      settings.preringTime := Self.SE_Prering_Time.Value;
      settings.closedRequired := Self.CHB_Closed_Required.Checked;

      Self.block.SetSettings(settings);

      try
        Self.SaveTracks();
      except
        on E: Exception do
        begin
          Application.MessageBox('Nepodařilo se načíst kolej přejezdu!', 'Chyba', MB_OK OR MB_ICONWARNING);
          Exit();
        end;
      end;

      Self.block.tracks.OwnsObjects := false;
      Self.block.tracks.Clear();
      Self.block.tracks.AddRange(Self.tracks);
      Self.block.tracks.OwnsObjects := true;
      Self.tracks.OwnsObjects := false;
      Self.tracks.Clear();
      Self.tracks.OwnsObjects := true;

      Self.block.positiveRules.Free();
      Self.block.positiveRules := positiveRules;

      var messages := '';
      for var i := 0 to addrs.Count - 1 do
      begin
        var typ: TRCSIOType;
        if (i < 7) then
          typ := TRCSIOType.output
        else
          typ := TRCSIOType.input;

        var another := Blocks.AnotherBlockUsesRCS(addrs[i], Self.block, typ);
        if (another <> nil) then
          messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addrs[i].ToString() + '.' + #13#10;
      end;

      if (messages <> '') then
        Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Neočekávaná chyba.', 'Chyba', E);
        Exit();
      end;
    end;

    Self.close();
    Self.block.Change();
  finally
    addrs.Free();
  end;
end;

procedure TF_BlkCrossing.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkCrossing.B_Track_DeleteClick(Sender: TObject);
begin
  if (Application.MessageBox('Opravdu smazat kolej?', 'Otázka', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
    Exit();
  Self.tracks.Delete(Self.CB_Track.ItemIndex);
  Self.CB_Track.Items.Delete(Self.CB_Track.ItemIndex);
  Self.CB_Track.ItemIndex := -1;
  Self.CB_TrackChange(Self);
end;

procedure TF_BlkCrossing.CB_TrackChange(Sender: TObject);
begin
  Self.B_Track_Delete.Enabled := (Self.CB_Track.ItemIndex <> -1) and
    (Self.CB_Track.ItemIndex <> Self.CB_Track.Items.Count - 1);
  Self.E_Track_Left_Out.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.E_Track_Left.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.E_Track_Middle.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.E_Track_Right.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.E_Track_Right_Out.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.CB_Track_Open_LR.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.CB_Track_Open_RL.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.CHB_InfiniteAnul.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.ME_Track_Anul_Time.Enabled := (Self.CB_Track.ItemIndex <> -1);

  if ((Self.CB_Track.ItemIndex < Self.CB_Track.Items.Count - 1) and (Self.CB_Track.ItemIndex <> -1)) then
  begin
    // edit existing track
    var track: TBlkCrossingTrack := Self.tracks[Self.CB_Track.ItemIndex];
    Self.E_Track_Left_Out.Text := track.leftOut.ToStr();
    Self.E_Track_Left.Text := track.left.ToStr();
    Self.E_Track_Middle.Text := track.middle.ToStr();
    Self.E_Track_Right.Text := track.right.ToStr();
    Self.E_Track_Right_Out.Text := track.rightOut.ToStr();
    Self.CB_Track_Open_LR.ItemIndex := Integer(track.openingLR);
    Self.CB_Track_Open_RL.ItemIndex := Integer(track.openingRL);
    Self.CHB_InfiniteAnul.Checked := track.infiniteAnul;
    if (track.infiniteAnul) then
      Self.ME_Track_Anul_Time.Text := '00:00'
    else
      Self.ME_Track_Anul_Time.Text := FormatDateTime('nn:ss', track.anulTime);

    Self.CHB_InfiniteAnulClick(Self.CHB_InfiniteAnul);
  end else begin
    // new track
    Self.E_Track_Left_Out.Text := '';
    Self.E_Track_Left.Text := '';
    Self.E_Track_Middle.Text := '';
    Self.E_Track_Right.Text := '';
    Self.E_Track_Right_Out.Text := '';
    Self.CB_Track_Open_LR.ItemIndex := -1;
    Self.CB_Track_Open_RL.ItemIndex := -1;
    Self.CHB_InfiniteAnul.Checked := False;
    Self.ME_Track_Anul_Time.Text := '00:00';
  end;
end;

procedure TF_BlkCrossing.CHB_InfiniteAnulClick(Sender: TObject);
begin
  Self.ME_Track_Anul_Time.Enabled := (not Self.CHB_InfiniteAnul.Checked);
  if (Self.CHB_InfiniteAnul.Checked) then
    Self.ME_Track_Anul_Time.Text := '00:00';
end;

procedure TF_BlkCrossing.CHB_JOP_controlClick(Sender: TObject);
begin
  Self.CB_Track.Enabled := Self.CHB_JOP_control.Checked;
  if (not Self.CHB_JOP_control.Checked) then
  begin
    Self.CB_Track.ItemIndex := -1;
    Self.CB_TrackChange(Self);
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_AnullationClick(Sender: TObject);
begin
  Self.SE_in_annulation_board.Enabled := Self.CHB_RCS_Anullation.Checked;
  Self.SE_in_annulation_port.Enabled := Self.CHB_RCS_Anullation.Checked;
  if (not Self.CHB_RCS_Anullation.Checked) then
  begin
    Self.SE_in_annulation_board.Value := 0;
    Self.SE_in_annulation_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_Barriers_DownClick(Sender: TObject);
begin
  Self.SE_out_barriers_down_board.Enabled := Self.CHB_RCS_Barriers_Down.Checked;
  Self.SE_out_barriers_down_port.Enabled := Self.CHB_RCS_Barriers_Down.Checked;
  if (not Self.CHB_RCS_Barriers_Down.Checked) then
  begin
    Self.SE_out_barriers_down_board.Value := 0;
    Self.SE_out_barriers_down_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_Barriers_UpClick(Sender: TObject);
begin
  Self.SE_out_barriers_up_board.Enabled := Self.CHB_RCS_Barriers_Up.Checked;
  Self.SE_out_barriers_up_port.Enabled := Self.CHB_RCS_Barriers_Up.Checked;
  if (not Self.CHB_RCS_Barriers_Up.Checked) then
  begin
    Self.SE_out_barriers_up_board.Value := 0;
    Self.SE_out_barriers_up_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_NOTClick(Sender: TObject);
begin
  Self.SE_out_open_board.Enabled := Self.CHB_RCS_NOT.Checked;
  Self.SE_out_open_port.Enabled := Self.CHB_RCS_NOT.Checked;
  if (not Self.CHB_RCS_NOT.Checked) then
  begin
    Self.SE_out_open_board.Value := 0;
    Self.SE_out_open_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_OpenClick(Sender: TObject);
begin
  Self.SE_in_open_board.Enabled := Self.CHB_RCS_Open.Checked;
  Self.SE_in_open_port.Enabled := Self.CHB_RCS_Open.Checked;
  if (not Self.CHB_RCS_Open.Checked) then
  begin
    Self.SE_in_open_board.Value := 0;
    Self.SE_in_open_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_PositiveClick(Sender: TObject);
begin
  Self.SE_out_positive_board.Enabled := Self.CHB_RCS_Positive.Checked;
  Self.SE_out_positive_port.Enabled := Self.CHB_RCS_Positive.Checked;
  Self.CB_Positive_Type.Enabled := Self.CHB_RCS_Positive.Checked;
  if (not Self.CHB_RCS_Positive.Checked) then
  begin
    Self.SE_out_positive_board.Value := 0;
    Self.SE_out_positive_port.Value := 0;
    Self.CB_Positive_Type.ItemIndex := -1;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_RingClick(Sender: TObject);
begin
  Self.SE_out_ring_board.Enabled := Self.CHB_RCS_Ring.Checked;
  Self.SE_out_ring_port.Enabled := Self.CHB_RCS_Ring.Checked;
  Self.CHB_Ring_Active_Down.Enabled := Self.CHB_RCS_Ring.Checked;
  if (not Self.CHB_RCS_Ring.Checked) then
  begin
    Self.SE_out_ring_board.Value := 0;
    Self.SE_out_ring_port.Value := 0;
    Self.CHB_Ring_Active_Down.Checked := False;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_CautionClick(Sender: TObject);
begin
  Self.SE_in_caution_board.Enabled := Self.CHB_RCS_Caution.Checked;
  Self.SE_in_caution_port.Enabled := Self.CHB_RCS_Caution.Checked;
  if (not Self.CHB_RCS_Caution.Checked) then
  begin
    Self.SE_in_caution_board.Value := 0;
    Self.SE_in_caution_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_CloseClick(Sender: TObject);
begin
  Self.SE_out_close_board.Enabled := Self.CHB_RCS_Close.Checked;
  Self.SE_out_close_port.Enabled := Self.CHB_RCS_Close.Checked;
  if (not Self.CHB_RCS_Close.Checked) then
  begin
    Self.SE_out_close_board.Value := 0;
    Self.SE_out_close_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_ClosedClick(Sender: TObject);
begin
  Self.SE_in_close_board.Enabled := Self.CHB_RCS_Closed.Checked;
  Self.SE_in_close_port.Enabled := Self.CHB_RCS_Closed.Checked;
  if (not Self.CHB_RCS_Closed.Checked) then
  begin
    Self.SE_in_close_board.Value := 0;
    Self.SE_in_close_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_LightsClick(Sender: TObject);
begin
  Self.SE_out_lights_board.Enabled := Self.CHB_RCS_Lights.Checked;
  Self.SE_out_lights_port.Enabled := Self.CHB_RCS_Lights.Checked;
  if (not Self.CHB_RCS_Lights.Checked) then
  begin
    Self.SE_out_lights_board.Value := 0;
    Self.SE_out_lights_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CommonOpenForm();
begin
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkCrossing.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkCrossingSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.SE_Prering_Time.Value := settings.preringTime;
  Self.CHB_Closed_Required.Checked := settings.closedRequired;

  RCSOptionalToUI(settings.RCSOutputs.close, Self.CHB_RCS_Close, Self.SE_out_close_board, Self.SE_out_close_port);
  RCSOptionalToUI(settings.RCSOutputs.emOpen, Self.CHB_RCS_NOT, Self.SE_out_open_board, Self.SE_out_open_port);
  RCSOptionalToUI(settings.RCSOutputs.positive, Self.CHB_RCS_Positive, Self.SE_out_positive_board, Self.SE_out_positive_port);
  RCSOptionalToUI(settings.RCSOutputs.barriersDown, Self.CHB_RCS_Barriers_Down, Self.SE_out_barriers_down_board, Self.SE_out_barriers_down_port);
  RCSOptionalToUI(settings.RCSOutputs.barriersUp, Self.CHB_RCS_Barriers_Up, Self.SE_out_barriers_up_board, Self.SE_out_barriers_up_port);
  RCSOptionalToUI(settings.RCSOutputs.bell, Self.CHB_RCS_Ring, Self.SE_out_ring_board, Self.SE_out_ring_port);
  RCSOptionalToUI(settings.RCSOutputs.lights, Self.CHB_RCS_Lights, Self.SE_out_lights_board, Self.SE_out_lights_port);

  RCSOptionalToUI(settings.RCSInputs.open, Self.CHB_RCS_Open, Self.SE_in_open_board, Self.SE_in_open_port);
  RCSOptionalToUI(settings.RCSInputs.closed, Self.CHB_RCS_Closed, Self.SE_in_close_board, Self.SE_in_close_port);
  RCSOptionalToUI(settings.RCSInputs.caution, Self.CHB_RCS_Caution, Self.SE_in_caution_board, Self.SE_in_caution_port);
  RCSOptionalToUI(settings.RCSInputs.annulation, Self.CHB_RCS_Anullation, Self.SE_in_annulation_board, Self.SE_in_annulation_port);

  Self.SE_RCS_boardExit(Self);

  if (settings.RCSOutputs.positive.enabled) then
  begin
    if (settings.RCSOutputs.positiveInvert) then
      Self.CB_Positive_Type.ItemIndex := 1
    else if (settings.RCSOutputs.positiveFlick) then
      Self.CB_Positive_Type.ItemIndex := 2
    else
      Self.CB_Positive_Type.ItemIndex := 0;
  end else begin
    Self.CB_Positive_Type.ItemIndex := -1;
  end;

  Self.CHB_Ring_Active_Down.Checked := settings.RCSOutputs.bellActiveDown;

  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();
  Self.tracks.AddRange(Self.block.tracks);

  Self.CHB_JOP_control.Checked := (Self.tracks.Count > 0);
  Self.CHB_JOP_controlClick(Self);
  Self.CB_Track.Clear();
  for var i := 1 to Self.tracks.Count do
    Self.CB_Track.Items.Add(IntToStr(i));
  Self.CB_Track.Items.Add('Přidat další...');
  if (Self.tracks.Count > 0) then
    Self.CB_Track.ItemIndex := 0;
  Self.CB_TrackChange(Self);

  Self.M_Positive_Ids.Clear();
  for var rule: TPositiveRule in Self.block.positiveRules do
    Self.M_Positive_Ids.Lines.Add(rule.IdStr());

  Self.M_Positive_Names.Clear();
  for var rule: TPositiveRule in Self.block.positiveRules do
    Self.M_Positive_Names.Lines.Add(rule.NameStr());

  Self.Caption := 'Upravit blok ' + glob.name + ' (přejezd)';
end;

procedure TF_BlkCrossing.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.Count - 1) + 1;
  Self.SE_Prering_Time.Value := 0;
  Self.CHB_Closed_Required.Checked := False;

  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Close, Self.SE_out_close_board, Self.SE_out_close_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_NOT, Self.SE_out_open_board, Self.SE_out_open_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Positive, Self.SE_out_positive_board, Self.SE_out_positive_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Barriers_Down, Self.SE_out_barriers_down_board, Self.SE_out_barriers_down_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Barriers_Up, Self.SE_out_barriers_up_board, Self.SE_out_barriers_up_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Ring, Self.SE_out_ring_board, Self.SE_out_ring_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Lights, Self.SE_out_lights_board, Self.SE_out_lights_port);

  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Open, Self.SE_in_open_board, Self.SE_in_open_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Closed, Self.SE_in_close_board, Self.SE_in_close_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Caution, Self.SE_in_caution_board, Self.SE_in_caution_port);
  RCSOptionalToUI(TRCS.RCSOptionalAddrDisabled(), Self.CHB_RCS_Anullation, Self.SE_in_caution_port, Self.SE_in_annulation_port);

  Self.SE_RCS_boardExit(Self);

  Self.CHB_JOP_control.Checked := false;
  Self.CHB_JOP_controlClick(Self);

  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();

  Self.M_Positive_Ids.Clear();
  Self.M_Positive_Names.Clear();

  Self.Caption := 'Nový přejezd';
end;

procedure TF_BlkCrossing.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkCrossing.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkCrossing.SaveTracks();
var track: TBlkCrossingTrack;
begin
  if (Self.CB_Track.ItemIndex = -1) then
    Exit();

  if (Self.CB_Track.ItemIndex = Self.CB_Track.Items.Count - 1) then
  begin
    track := TBlkCrossingTrack.Create();
    Self.tracks.Add(track);
  end
  else
    track := Self.tracks[Self.CB_Track.ItemIndex];

  track.leftOut.Parse(Self.E_Track_Left_Out.Text);
  track.left.Parse(Self.E_Track_Left.Text);
  track.middle.Parse(Self.E_Track_Middle.Text);
  track.right.Parse(Self.E_Track_Right.Text);
  track.rightOut.Parse(Self.E_Track_Right_Out.Text);

  track.openingLR := TBlkCrossingTrackOpening(Self.CB_Track_Open_LR.ItemIndex);
  track.openingRL := TBlkCrossingTrackOpening(Self.CB_Track_Open_RL.ItemIndex);
  track.infiniteAnul := Self.CHB_InfiniteAnul.Checked;
  track.anulTime := EncodeTime(0, StrToInt(LeftStr(Self.ME_Track_Anul_Time.Text, 2)),
    StrToInt(Copy(Self.ME_Track_Anul_Time.Text, 4, 2)), 0);
end;

end.
