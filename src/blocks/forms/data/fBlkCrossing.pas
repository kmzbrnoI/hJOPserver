﻿unit fBlkCrossing;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Spin, StdCtrls, BlockCrossing, BlockDb, Generics.Collections,
  BlockCrossingLogic, Mask, StrUtils;

type
  TF_BlkCrossing = class(TForm)
    L_Name: TLabel;
    E_Prj_Nazev: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_ID: TLabel;
    SE_ID: TSpinEdit;
    L_Station: TLabel;
    LB_Stanice: TListBox;
    GB_RCS: TGroupBox;
    GB_Prj_vyst: TGroupBox;
    L_P04: TLabel;
    L_P05: TLabel;
    SE_vyst_open_port: TSpinEdit;
    SE_vyst_close_port: TSpinEdit;
    GB_Prj_vst: TGroupBox;
    L_P07: TLabel;
    L_P08: TLabel;
    L_P09: TLabel;
    L_P10: TLabel;
    SE_vst_close_port: TSpinEdit;
    SE_vst_open_port: TSpinEdit;
    SE_vst_vystraha_port: TSpinEdit;
    SE_vst_anulace_port: TSpinEdit;
    L_P01: TLabel;
    SE_vyst_open_board: TSpinEdit;
    SE_vyst_close_board: TSpinEdit;
    Label1: TLabel;
    SE_vst_close_board: TSpinEdit;
    SE_vst_open_board: TSpinEdit;
    SE_vst_vystraha_board: TSpinEdit;
    SE_vst_anulace_board: TSpinEdit;
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
    CB_Track_Open: TComboBox;
    Label9: TLabel;
    ME_Track_Anul_Time: TMaskEdit;
    CHB_RCS_Anullation: TCheckBox;
    CHB_RCS_NOT: TCheckBox;
    Label10: TLabel;
    CHB_RCS_BP: TCheckBox;
    SE_vyst_bp_board: TSpinEdit;
    SE_vyst_bp_port: TSpinEdit;
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
    OpenIndex: Integer;
    Blk: TBlkCrossing;
    NewBlk: Boolean;
    areas: TArstr;
    tracks: TObjectList<TBlkCrossingTrack>;

    procedure NormalOpenForm();
    procedure HlavniOpenForm();
    procedure NewOpenForm();
    procedure SaveTracks();

  public

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkCrossing: TF_BlkCrossing;

implementation

uses GetSystems, TechnologieRCS, AreaDb, Area, Block, FileSystem, DataBloky;

{$R *.dfm}

procedure TF_BlkCrossing.OpenForm(BlokIndex: Integer);
begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm();

  if (NewBlk) then
  begin
    NewOpenForm();
  end else begin
    NormalOpenForm();
  end;

  Self.ShowModal;
end;

procedure TF_BlkCrossing.SE_RCS_boardExit(Sender: TObject);
begin
  Self.SE_vyst_close_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vyst_close_board.Value,
    Self.SE_vyst_close_port.Value);
  Self.SE_vyst_open_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vyst_open_board.Value,
    Self.SE_vyst_open_port.Value);
  Self.SE_vyst_bp_board.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vyst_bp_board.Value, Self.SE_vyst_bp_port.Value);

  Self.SE_vst_close_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vst_close_board.Value,
    Self.SE_vst_close_port.Value);
  Self.SE_vst_open_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vst_open_board.Value, Self.SE_vst_open_port.Value);
  Self.SE_vst_vystraha_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vst_vystraha_board.Value,
    Self.SE_vst_vystraha_port.Value);
  Self.SE_vst_anulace_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_vst_anulace_board.Value,
    Self.SE_vst_anulace_port.Value);
end;

procedure TF_BlkCrossing.B_save_PClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkCrossingSettings;
  addrs: TList<TRCSAddr>;
  another: TBlk;
  typ: TRCSIOType;
  i: Integer;
  messages: string;
begin
  if (Self.E_Prj_Nazev.Text = '') then
  begin
    Application.MessageBox('Vyplňte název přejezdu', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Prj_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btCrossing;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkCrossing;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
  end;

  addrs := TList<TRCSAddr>.Create();
  try
    settings.RCSOutputs.close.board := Self.SE_vyst_close_board.Value;
    settings.RCSOutputs.close.port := Self.SE_vyst_close_port.Value;
    addrs.Add(settings.RCSOutputs.close);

    settings.RCSOutputs.emOpenUse := Self.CHB_RCS_NOT.Checked;
    settings.RCSOutputs.emOpen.board := Self.SE_vyst_open_board.Value;
    settings.RCSOutputs.emOpen.port := Self.SE_vyst_open_port.Value;
    if (Self.CHB_RCS_NOT.Checked) then
      addrs.Add(settings.RCSOutputs.emOpen);

    settings.RCSOutputs.blockPositiveUse := Self.CHB_RCS_BP.Checked;
    settings.RCSOutputs.blockPositive.board := Self.SE_vyst_bp_board.Value;
    settings.RCSOutputs.blockPositive.port := Self.SE_vyst_bp_port.Value;
    if (Self.CHB_RCS_BP.Checked) then
      addrs.Add(settings.RCSOutputs.blockPositive);

    settings.RCSInputs.open.board := SE_vst_open_board.Value;
    settings.RCSInputs.open.port := SE_vst_open_port.Value;
    addrs.Add(settings.RCSInputs.open);

    settings.RCSInputs.closed.board := SE_vst_close_board.Value;
    settings.RCSInputs.closed.port := SE_vst_close_port.Value;
    addrs.Add(settings.RCSInputs.closed);

    settings.RCSInputs.caution.board := SE_vst_vystraha_board.Value;
    settings.RCSInputs.caution.port := SE_vst_vystraha_port.Value;
    addrs.Add(settings.RCSInputs.caution);

    settings.RCSInputs.annulationUse := Self.CHB_RCS_Anullation.Checked;
    settings.RCSInputs.annulation.board := SE_vst_anulace_board.Value;
    settings.RCSInputs.annulation.port := SE_vst_anulace_port.Value;
    if (Self.CHB_RCS_Anullation.Checked) then
      addrs.Add(settings.RCSInputs.annulation);

    Self.Blk.SetSettings(settings);

    try
      Self.SaveTracks();
    except
      on E: Exception do
      begin
        Application.MessageBox('Nepodařilo se načíst kolej přejezdu!', 'Chyba', MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;

    Self.Blk.tracks.OwnsObjects := false;
    Self.Blk.tracks.Clear();
    Self.Blk.tracks.AddRange(Self.tracks);
    Self.Blk.tracks.OwnsObjects := true;
    Self.tracks.OwnsObjects := false;
    Self.tracks.Clear();
    Self.tracks.OwnsObjects := true;

    messages := '';
    for i := 0 to addrs.Count - 1 do
    begin
      if (i < 2) then
        typ := TRCSIOType.output
      else
        typ := TRCSIOType.input;

      another := Blocks.AnotherBlockUsesRCS(addrs[i], Self.Blk, typ);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + IntToStr(addrs[i].board) + ':' +
          IntToStr(addrs[i].port) + '.' + #13#10;
    end;

    if (messages <> '') then
      Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);

    Self.close();
    Self.Blk.Change();
  finally
    addrs.Free();
  end;
end;

procedure TF_BlkCrossing.B_StornoClick(Sender: TObject);
begin
  Self.close;
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
  Self.CB_Track_Open.Enabled := (Self.CB_Track.ItemIndex <> -1);
  Self.ME_Track_Anul_Time.Enabled := (Self.CB_Track.ItemIndex <> -1);

  if ((Self.CB_Track.ItemIndex < Self.CB_Track.Items.Count - 1) and (Self.CB_Track.ItemIndex <> -1)) then
  begin
    // edit existing track
    Self.E_Track_Left_Out.Text := Self.tracks[Self.CB_Track.ItemIndex].leftOut.ToStr();
    Self.E_Track_Left.Text := Self.tracks[Self.CB_Track.ItemIndex].left.ToStr();
    Self.E_Track_Middle.Text := Self.tracks[Self.CB_Track.ItemIndex].middle.ToStr();
    Self.E_Track_Right.Text := Self.tracks[Self.CB_Track.ItemIndex].right.ToStr();
    Self.E_Track_Right_Out.Text := Self.tracks[Self.CB_Track.ItemIndex].rightOut.ToStr();
    Self.CB_Track_Open.ItemIndex := Integer(Self.tracks[Self.CB_Track.ItemIndex].opening);
    Self.ME_Track_Anul_Time.Text := FormatDateTime('nn:ss', Self.tracks[Self.CB_Track.ItemIndex].anulTime);
  end else begin
    // new track
    Self.E_Track_Left_Out.Text := '';
    Self.E_Track_Left.Text := '';
    Self.E_Track_Middle.Text := '';
    Self.E_Track_Right.Text := '';
    Self.E_Track_Right_Out.Text := '';
    Self.CB_Track_Open.ItemIndex := -1;
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
  Self.SE_vst_anulace_board.Enabled := Self.CHB_RCS_Anullation.Checked;
  Self.SE_vst_anulace_port.Enabled := Self.CHB_RCS_Anullation.Checked;
  if (not Self.CHB_RCS_Anullation.Checked) then
  begin
    Self.SE_vst_anulace_board.Value := 0;
    Self.SE_vst_anulace_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_NOTClick(Sender: TObject);
begin
  Self.SE_vyst_open_board.Enabled := Self.CHB_RCS_NOT.Checked;
  Self.SE_vyst_open_port.Enabled := Self.CHB_RCS_NOT.Checked;
  if (not Self.CHB_RCS_NOT.Checked) then
  begin
    Self.SE_vyst_open_board.Value := 0;
    Self.SE_vyst_open_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.CHB_RCS_BPClick(Sender: TObject);
begin
  Self.SE_vyst_bp_board.Enabled := Self.CHB_RCS_BP.Checked;
  Self.SE_vyst_bp_port.Enabled := Self.CHB_RCS_BP.Checked;
  if (not Self.CHB_RCS_BP.Checked) then
  begin
    Self.SE_vyst_bp_board.Value := 0;
    Self.SE_vyst_bp_port.Value := 0;
  end;
end;

procedure TF_BlkCrossing.HlavniOpenForm();
begin
  SetLength(Self.areas, 0);
  Self.LB_Stanice.Clear();

  Self.SE_vyst_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vyst_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vyst_bp_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_vystraha_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_anulace_board.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_BlkCrossing.NormalOpenForm();
var glob: TBlkSettings;
  settings: TBlkCrossingSettings;
  i: Integer;
  Area: TArea;
begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for Area in Self.Blk.areas do
    Self.LB_Stanice.Items.Add(Area.name);

  SetLength(areas, Self.Blk.areas.Count);
  for i := 0 to Self.Blk.areas.Count - 1 do
    areas[i] := Self.Blk.areas[i].id;

  E_Prj_Nazev.Text := glob.name;
  SE_ID.Value := glob.id;

  if (settings.RCSOutputs.close.board > Cardinal(Self.SE_vyst_close_board.MaxValue)) then
    Self.SE_vyst_close_board.MaxValue := 0;
  Self.SE_vyst_close_port.MaxValue := 0;

  SE_vyst_close_board.Value := settings.RCSOutputs.close.board;
  SE_vyst_close_port.Value := settings.RCSOutputs.close.port;

  Self.CHB_RCS_NOT.Checked := settings.RCSOutputs.emOpenUse;
  Self.CHB_RCS_NOTClick(Self);
  if (settings.RCSOutputs.emOpen.board > Cardinal(Self.SE_vyst_open_board.MaxValue)) then
    Self.SE_vyst_open_board.MaxValue := 0;
  Self.SE_vyst_open_port.MaxValue := 0;
  if (settings.RCSOutputs.emOpenUse) then
  begin
    SE_vyst_open_board.Value := settings.RCSOutputs.emOpen.board;
    SE_vyst_open_port.Value := settings.RCSOutputs.emOpen.port;
  end;

  Self.CHB_RCS_BP.Checked := settings.RCSOutputs.blockPositiveUse;
  Self.CHB_RCS_BPClick(Self);
  if (settings.RCSOutputs.blockPositive.board > Cardinal(Self.SE_vyst_bp_board.MaxValue)) then
    Self.SE_vyst_open_board.MaxValue := 0;
  Self.SE_vyst_bp_port.MaxValue := 0;
  if (settings.RCSOutputs.blockPositiveUse) then
  begin
    SE_vyst_bp_board.Value := settings.RCSOutputs.blockPositive.board;
    SE_vyst_bp_port.Value := settings.RCSOutputs.blockPositive.port;
  end;

  if (settings.RCSInputs.open.board > Cardinal(Self.SE_vst_open_board.MaxValue)) then
    Self.SE_vst_open_board.MaxValue := 0;
  Self.SE_vst_open_port.MaxValue := 0;

  SE_vst_open_board.Value := settings.RCSInputs.open.board;
  SE_vst_open_port.Value := settings.RCSInputs.open.port;

  if (settings.RCSInputs.closed.board > Cardinal(Self.SE_vst_close_board.MaxValue)) then
    Self.SE_vst_close_board.MaxValue := 0;
  Self.SE_vst_close_port.MaxValue := 0;

  SE_vst_close_board.Value := settings.RCSInputs.closed.board;
  SE_vst_close_port.Value := settings.RCSInputs.closed.port;

  if (settings.RCSInputs.caution.board > Cardinal(Self.SE_vst_vystraha_board.MaxValue)) then
    Self.SE_vst_vystraha_board.MaxValue := 0;
  Self.SE_vst_vystraha_port.MaxValue := 0;

  SE_vst_vystraha_board.Value := settings.RCSInputs.caution.board;
  SE_vst_vystraha_port.Value := settings.RCSInputs.caution.port;

  Self.CHB_RCS_Anullation.Checked := settings.RCSInputs.annulationUse;
  Self.CHB_RCS_AnullationClick(Self);
  if (settings.RCSInputs.annulation.board > Cardinal(Self.SE_vst_anulace_board.MaxValue)) then
    Self.SE_vst_anulace_board.MaxValue := 0;
  Self.SE_vst_anulace_port.MaxValue := 0;
  if (settings.RCSInputs.annulationUse) then
  begin
    SE_vst_anulace_board.Value := settings.RCSInputs.annulation.board;
    SE_vst_anulace_port.Value := settings.RCSInputs.annulation.port;
  end;

  Self.SE_RCS_boardExit(Self);

  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();
  Self.tracks.AddRange(Self.Blk.tracks);

  Self.CHB_JOP_control.Checked := (Self.tracks.Count > 0);
  Self.CHB_JOP_controlClick(Self);
  Self.CB_Track.Clear();
  for i := 1 to Self.tracks.Count do
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
  Self.E_Prj_Nazev.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.Count - 1) + 1;

  Self.SE_vyst_close_board.Value := 0;
  Self.SE_vyst_close_port.Value := 0;

  Self.CHB_RCS_NOT.Checked := false;
  Self.CHB_RCS_NOTClick(Self);

  Self.CHB_RCS_BP.Checked := false;
  Self.CHB_RCS_BPClick(Self);

  Self.SE_vst_open_board.Value := 0;
  Self.SE_vst_open_port.Value := 0;
  Self.SE_vst_close_board.Value := 0;
  Self.SE_vst_close_port.Value := 0;
  Self.SE_vst_vystraha_board.Value := 0;
  Self.SE_vst_vystraha_port.Value := 0;

  Self.CHB_RCS_Anullation.Checked := false;
  Self.CHB_RCS_AnullationClick(Self);

  Self.SE_RCS_boardExit(Self);

  Self.CHB_JOP_control.Checked := false;
  Self.CHB_JOP_controlClick(Self);

  Self.tracks := TObjectList<TBlkCrossingTrack>.Create();

  Self.Caption := 'Nový přejezd';
  Self.ActiveControl := Self.E_Prj_Nazev;
end;

procedure TF_BlkCrossing.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

procedure TF_BlkCrossing.NewBlkCreate();
begin
  Self.NewBlk := true;
  OpenForm(Blocks.Count);
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

  track.opening := TBlkCrossingTrackOpening(Self.CB_Track_Open.ItemIndex);
  track.anulTime := EncodeTime(0, StrToInt(LeftStr(Self.ME_Track_Anul_Time.Text, 2)),
    StrToInt(Copy(Self.ME_Track_Anul_Time.Text, 4, 2)), 0);
end;

end.// unit
