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
    CHB_RCS_BP: TCheckBox;
    SE_out_bp_board: TSpinEdit;
    SE_out_bp_port: TSpinEdit;
    CB_Track_Open_RL: TComboBox;
    CB_Track_Open_LR: TComboBox;
    Label11: TLabel;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RCS_boardExit(Sender: TObject);
    procedure CHB_JOP_controlClick(Sender: TObject);
    procedure CB_TrackChange(Sender: TObject);
    procedure B_Track_DeleteClick(Sender: TObject);
    procedure CHB_RCS_AnullationClick(Sender: TObject);
    procedure CHB_RCS_NOTClick(Sender: TObject);
    procedure CHB_RCS_BPClick(Sender: TObject);
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

uses GetSystems, TechnologieRCS, AreaDb, Area, Block, FileSystem, DataBloky;

{$R *.dfm}

procedure TF_BlkCrossing.EditBlock(BlokIndex: Integer);
begin
  Self.openIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.block));
  Self.CommonOpenForm();

  if (Self.isNewBlock) then
    Self.NewOpenForm()
  else
    Self.EditOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkCrossing.SE_RCS_boardExit(Sender: TObject);
begin
  Self.SE_out_close_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_out_close_board.Value,
    Self.SE_out_close_port.Value);
  Self.SE_out_open_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_out_open_board.Value,
    Self.SE_out_open_port.Value);
  Self.SE_out_bp_board.MaxValue := TBlocks.SEPortMaxValue(Self.SE_out_bp_board.Value, Self.SE_out_bp_port.Value);

  Self.SE_in_close_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_in_close_board.Value,
    Self.SE_in_close_port.Value);
  Self.SE_in_open_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_in_open_board.Value, Self.SE_in_open_port.Value);
  Self.SE_in_caution_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_in_caution_board.Value,
    Self.SE_in_caution_port.Value);
  Self.SE_in_annulation_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_in_annulation_board.Value,
    Self.SE_in_annulation_port.Value);
end;

procedure TF_BlkCrossing.B_save_PClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název přejezdu', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, openIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
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
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.block.SetGlobalSettings(glob);
  end;

  var settings: TBlkCrossingSettings;
  var addrs := TList<TRCSAddr>.Create();
  try
    settings.RCSOutputs.close.board := Self.SE_out_close_board.Value;
    settings.RCSOutputs.close.port := Self.SE_out_close_port.Value;
    addrs.Add(settings.RCSOutputs.close);

    settings.RCSOutputs.emOpenUse := Self.CHB_RCS_NOT.Checked;
    settings.RCSOutputs.emOpen.board := Self.SE_out_open_board.Value;
    settings.RCSOutputs.emOpen.port := Self.SE_out_open_port.Value;
    if (Self.CHB_RCS_NOT.Checked) then
      addrs.Add(settings.RCSOutputs.emOpen);

    settings.RCSOutputs.blockPositiveUse := Self.CHB_RCS_BP.Checked;
    settings.RCSOutputs.blockPositive.board := Self.SE_out_bp_board.Value;
    settings.RCSOutputs.blockPositive.port := Self.SE_out_bp_port.Value;
    if (Self.CHB_RCS_BP.Checked) then
      addrs.Add(settings.RCSOutputs.blockPositive);

    settings.RCSInputs.open.board := SE_in_open_board.Value;
    settings.RCSInputs.open.port := SE_in_open_port.Value;
    addrs.Add(settings.RCSInputs.open);

    settings.RCSInputs.closed.board := SE_in_close_board.Value;
    settings.RCSInputs.closed.port := SE_in_close_port.Value;
    addrs.Add(settings.RCSInputs.closed);

    settings.RCSInputs.caution.board := SE_in_caution_board.Value;
    settings.RCSInputs.caution.port := SE_in_caution_port.Value;
    addrs.Add(settings.RCSInputs.caution);

    settings.RCSInputs.annulationUse := Self.CHB_RCS_Anullation.Checked;
    settings.RCSInputs.annulation.board := SE_in_annulation_board.Value;
    settings.RCSInputs.annulation.port := SE_in_annulation_port.Value;
    if (Self.CHB_RCS_Anullation.Checked) then
      addrs.Add(settings.RCSInputs.annulation);

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

    var messages := '';
    for var i := 0 to addrs.Count - 1 do
    begin
      var typ: TRCSIOType;
      if (i < 2) then
        typ := TRCSIOType.output
      else
        typ := TRCSIOType.input;

      var another := Blocks.AnotherBlockUsesRCS(addrs[i], Self.block, typ);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addrs[i].board.ToString() + '.' + #13#10;
    end;

    if (messages <> '') then
      Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);

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
  Self.ME_Track_Anul_Time.Enabled := (Self.CB_Track.ItemIndex <> -1);

  if ((Self.CB_Track.ItemIndex < Self.CB_Track.Items.Count - 1) and (Self.CB_Track.ItemIndex <> -1)) then
  begin
    // edit existing track
    Self.E_Track_Left_Out.Text := Self.tracks[Self.CB_Track.ItemIndex].leftOut.ToStr();
    Self.E_Track_Left.Text := Self.tracks[Self.CB_Track.ItemIndex].left.ToStr();
    Self.E_Track_Middle.Text := Self.tracks[Self.CB_Track.ItemIndex].middle.ToStr();
    Self.E_Track_Right.Text := Self.tracks[Self.CB_Track.ItemIndex].right.ToStr();
    Self.E_Track_Right_Out.Text := Self.tracks[Self.CB_Track.ItemIndex].rightOut.ToStr();
    Self.CB_Track_Open_LR.ItemIndex := Integer(Self.tracks[Self.CB_Track.ItemIndex].openingLR);
    Self.CB_Track_Open_RL.ItemIndex := Integer(Self.tracks[Self.CB_Track.ItemIndex].openingRL);
    Self.ME_Track_Anul_Time.Text := FormatDateTime('nn:ss', Self.tracks[Self.CB_Track.ItemIndex].anulTime)
  end else begin
    // new track
    Self.E_Track_Left_Out.Text := '';
    Self.E_Track_Left.Text := '';
    Self.E_Track_Middle.Text := '';
    Self.E_Track_Right.Text := '';
    Self.E_Track_Right_Out.Text := '';
    Self.CB_Track_Open_LR.ItemIndex := -1;
    Self.CB_Track_Open_RL.ItemIndex := -1;
    Self.ME_Track_Anul_Time.Text := '00:00';
  end;

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

procedure TF_BlkCrossing.CHB_RCS_BPClick(Sender: TObject);
begin
  Self.SE_out_bp_board.Enabled := Self.CHB_RCS_BP.Checked;
  Self.SE_out_bp_port.Enabled := Self.CHB_RCS_BP.Checked;
  if (not Self.CHB_RCS_BP.Checked) then
  begin
    Self.SE_out_bp_board.Value := 0;
    Self.SE_out_bp_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CommonOpenForm();
begin
  Self.SE_out_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_out_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_out_bp_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_in_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_in_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_in_caution_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_in_annulation_board.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_BlkCrossing.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkCrossingSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  if (settings.RCSOutputs.close.board > Cardinal(Self.SE_out_close_board.MaxValue)) then
    Self.SE_out_close_board.MaxValue := 0;
  Self.SE_out_close_port.MaxValue := 0;

  Self.SE_out_close_board.Value := settings.RCSOutputs.close.board;
  Self.SE_out_close_port.Value := settings.RCSOutputs.close.port;

  Self.CHB_RCS_NOT.Checked := settings.RCSOutputs.emOpenUse;
  Self.CHB_RCS_NOTClick(Self);
  if (settings.RCSOutputs.emOpen.board > Cardinal(Self.SE_out_open_board.MaxValue)) then
    Self.SE_out_open_board.MaxValue := 0;
  Self.SE_out_open_port.MaxValue := 0;
  if (settings.RCSOutputs.emOpenUse) then
  begin
    Self.SE_out_open_board.Value := settings.RCSOutputs.emOpen.board;
    Self.SE_out_open_port.Value := settings.RCSOutputs.emOpen.port;
  end;

  Self.CHB_RCS_BP.Checked := settings.RCSOutputs.blockPositiveUse;
  Self.CHB_RCS_BPClick(Self);
  if (settings.RCSOutputs.blockPositive.board > Cardinal(Self.SE_out_bp_board.MaxValue)) then
    Self.SE_out_open_board.MaxValue := 0;
  Self.SE_out_bp_port.MaxValue := 0;
  if (settings.RCSOutputs.blockPositiveUse) then
  begin
    Self.SE_out_bp_board.Value := settings.RCSOutputs.blockPositive.board;
    Self.SE_out_bp_port.Value := settings.RCSOutputs.blockPositive.port;
  end;

  if (settings.RCSInputs.open.board > Cardinal(Self.SE_in_open_board.MaxValue)) then
    Self.SE_in_open_board.MaxValue := 0;
  Self.SE_in_open_port.MaxValue := 0;

  SE_in_open_board.Value := settings.RCSInputs.open.board;
  SE_in_open_port.Value := settings.RCSInputs.open.port;

  if (settings.RCSInputs.closed.board > Cardinal(Self.SE_in_close_board.MaxValue)) then
    Self.SE_in_close_board.MaxValue := 0;
  Self.SE_in_close_port.MaxValue := 0;

  SE_in_close_board.Value := settings.RCSInputs.closed.board;
  SE_in_close_port.Value := settings.RCSInputs.closed.port;

  if (settings.RCSInputs.caution.board > Cardinal(Self.SE_in_caution_board.MaxValue)) then
    Self.SE_in_caution_board.MaxValue := 0;
  Self.SE_in_caution_port.MaxValue := 0;

  Self.SE_in_caution_board.Value := settings.RCSInputs.caution.board;
  Self.SE_in_caution_port.Value := settings.RCSInputs.caution.port;

  Self.CHB_RCS_Anullation.Checked := settings.RCSInputs.annulationUse;
  Self.CHB_RCS_AnullationClick(Self);
  if (settings.RCSInputs.annulation.board > Cardinal(Self.SE_in_annulation_board.MaxValue)) then
    Self.SE_in_annulation_board.MaxValue := 0;
  Self.SE_in_annulation_port.MaxValue := 0;
  if (settings.RCSInputs.annulationUse) then
  begin
    Self.SE_in_annulation_board.Value := settings.RCSInputs.annulation.board;
    Self.SE_in_annulation_port.Value := settings.RCSInputs.annulation.port;
  end;

  Self.SE_RCS_boardExit(Self);

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

  Self.Caption := 'Upravit blok ' + glob.name + ' (přejezd)';
  Self.ActiveControl := Self.B_save_P;
end;

procedure TF_BlkCrossing.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.Count - 1) + 1;

  Self.SE_out_close_board.Value := 0;
  Self.SE_out_close_port.Value := 0;

  Self.CHB_RCS_NOT.Checked := false;
  Self.CHB_RCS_NOTClick(Self);

  Self.CHB_RCS_BP.Checked := false;
  Self.CHB_RCS_BPClick(Self);

  Self.SE_in_open_board.Value := 0;
  Self.SE_in_open_port.Value := 0;
  Self.SE_in_close_board.Value := 0;
  Self.SE_in_close_port.Value := 0;
  Self.SE_in_caution_board.Value := 0;
  Self.SE_in_caution_port.Value := 0;

  Self.CHB_RCS_Anullation.Checked := false;
  Self.CHB_RCS_AnullationClick(Self);

  Self.SE_RCS_boardExit(Self);

  Self.CHB_JOP_control.Checked := false;
  Self.CHB_JOP_controlClick(Self);

  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();

  Self.Caption := 'Nový přejezd';
  Self.ActiveControl := Self.E_Name;
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
  Self.EditBlock(Blocks.Count);
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
  track.anulTime := EncodeTime(0, StrToInt(LeftStr(Self.ME_Track_Anul_Time.Text, 2)),
    StrToInt(Copy(Self.ME_Track_Anul_Time.Text, 4, 2)), 0);
end;

end.
