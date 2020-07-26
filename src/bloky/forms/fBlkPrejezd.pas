unit fBlkPrejezd;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Spin, StdCtrls, TBlokPrejezd, TBloky, Generics.Collections, IBUtils,
  TBlokPrejezdLogic, Mask, StrUtils;

type
  TF_BlkPrejezd = class(TForm)
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
   OpenIndex:Integer;
   Blk:TBlkPrejezd;
   NewBlk:Boolean;
   obls:TArstr;
   tracks: TObjectList<TBlkPrjTrack>;

    procedure NormalOpenForm();
    procedure HlavniOpenForm();
    procedure NewOpenForm();
    procedure SaveTracks();

  public

    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkPrejezd: TF_BlkPrejezd;

implementation

uses GetSystems, TechnologieRCS, TOblsRizeni, TOblRizeni,
     TBlok, FileSystem, DataBloky;

{$R *.dfm}

procedure TF_BlkPrejezd.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm();

  if (NewBlk) then
   begin
    NewOpenForm();
   end else begin
    NormalOpenForm();
   end;

  Self.ShowModal;
 end;

procedure TF_BlkPrejezd.SE_RCS_boardExit(Sender: TObject);
begin
 Self.SE_vyst_close_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vyst_close_board.Value, Self.SE_vyst_close_port.Value);
 Self.SE_vyst_open_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vyst_open_board.Value, Self.SE_vyst_open_port.Value);
 Self.SE_vyst_bp_board.MaxValue := TBlky.SEPortMaxValue(Self.SE_vyst_bp_board.Value, Self.SE_vyst_bp_port.Value);

 Self.SE_vst_close_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vst_close_board.Value, Self.SE_vst_close_port.Value);
 Self.SE_vst_open_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vst_open_board.Value, Self.SE_vst_open_port.Value);
 Self.SE_vst_vystraha_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vst_vystraha_board.Value, Self.SE_vst_vystraha_port.Value);
 Self.SE_vst_anulace_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_vst_anulace_board.Value, Self.SE_vst_anulace_port.Value);
end;

procedure TF_BlkPrejezd.B_save_PClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
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
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  glob.name := Self.E_Prj_Nazev.Text;
  glob.typ := btPrejezd;
  glob.id := Self.SE_ID.Value;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blky.Add(btPrejezd, glob) as TBlkPrejezd;
    except
      on E:Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  addrs := TList<TRCSAddr>.Create();
  try
    settings.RCSOutputs.Zavrit.board := Self.SE_vyst_close_board.Value;
    settings.RCSOutputs.Zavrit.port := Self.SE_vyst_close_port.Value;
    addrs.Add(settings.RCSOutputs.Zavrit);

    settings.RCSOutputs.NOtevritUse := Self.CHB_RCS_NOT.Checked;
    settings.RCSOutputs.NOtevrit.board := Self.SE_vyst_open_board.Value;
    settings.RCSOutputs.NOtevrit.port := Self.SE_vyst_open_port.Value;
    if (Self.CHB_RCS_NOT.Checked) then
      addrs.Add(settings.RCSOutputs.NOtevrit);

    settings.RCSOutputs.BlokPozUse := Self.CHB_RCS_BP.Checked;
    settings.RCSOutputs.BlokPoz.board := Self.SE_vyst_bp_board.Value;
    settings.RCSOutputs.BlokPoz.port := Self.SE_vyst_bp_port.Value;
    if (Self.CHB_RCS_BP.Checked) then
      addrs.Add(settings.RCSOutputs.BlokPoz);

    settings.RCSInputs.Otevreno.board := SE_vst_open_board.Value;
    settings.RCSInputs.Otevreno.port := SE_vst_open_port.Value;
    addrs.Add(settings.RCSInputs.Otevreno);

    settings.RCSInputs.Zavreno.board := SE_vst_close_board.Value;
    settings.RCSInputs.Zavreno.port := SE_vst_close_port.Value;
    addrs.Add(settings.RCSInputs.Zavreno);

    settings.RCSInputs.Vystraha.board := SE_vst_vystraha_board.Value;
    settings.RCSInputs.Vystraha.port := SE_vst_vystraha_port.Value;
    addrs.Add(settings.RCSInputs.Vystraha);

    settings.RCSInputs.anulaceUse := Self.CHB_RCS_Anullation.Checked;
    settings.RCSInputs.Anulace.board := SE_vst_anulace_board.Value;
    settings.RCSInputs.Anulace.port := SE_vst_anulace_port.Value;
    if (Self.CHB_RCS_Anullation.Checked) then
      addrs.Add(settings.RCSInputs.Anulace);

    Self.Blk.SetSettings(settings);

    try
      Self.SaveTracks();
    except
     on E:Exception do
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
    for i := 0 to addrs.Count-1 do
     begin
      if (i < 2) then
        typ := TRCSIOType.output
      else
        typ := TRCSIOType.input;

      another := Blky.AnotherBlockUsesRCS(addrs[i], Self.Blk, typ);
      if (another <> nil) then
        messages := messages + 'Blok '+another.name+' využívá také RCS adresu '+
                    IntToStr(addrs[i].board)+':'+IntToStr(addrs[i].port)+'.'+#13#10;
     end;

    if (messages <> '') then
      Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);

    Self.Close();
    Self.Blk.Change();
  finally
    addrs.Free();
  end;
 end;

procedure TF_BlkPrejezd.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_BlkPrejezd.B_Track_DeleteClick(Sender: TObject);
begin
 if (Application.MessageBox('Opravdu smazat kolej?', 'Otázka', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
   Exit();
 Self.tracks.Delete(Self.CB_Track.ItemIndex);
 Self.CB_Track.Items.Delete(Self.CB_Track.ItemIndex);
 Self.CB_track.ItemIndex := -1;
 Self.CB_TrackChange(Self);
end;

procedure TF_BlkPrejezd.CB_TrackChange(Sender: TObject);
begin
 Self.B_Track_Delete.Enabled := (Self.CB_Track.ItemIndex <> -1) and (Self.CB_Track.ItemIndex <> Self.CB_Track.Items.Count-1);
 Self.E_Track_Left_Out.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.E_Track_Left.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.E_Track_Middle.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.E_Track_Right.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.E_Track_Right_Out.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.CB_Track_Open.Enabled := (Self.CB_Track.ItemIndex <> -1);
 Self.ME_Track_Anul_Time.Enabled := (Self.CB_Track.ItemIndex <> -1);

 if ((Self.CB_Track.ItemIndex < Self.CB_Track.Items.Count-1) and (Self.CB_Track.ItemIndex <> -1)) then
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

procedure TF_BlkPrejezd.CHB_JOP_controlClick(Sender: TObject);
begin
 Self.CB_Track.Enabled := Self.CHB_JOP_control.Checked;
 if (not Self.CHB_JOP_control.Checked) then
  begin
   Self.CB_Track.ItemIndex := -1;
   Self.CB_TrackChange(Self);
  end;
end;

procedure TF_BlkPrejezd.CHB_RCS_AnullationClick(Sender: TObject);
begin
 Self.SE_vst_anulace_board.Enabled := Self.CHB_RCS_Anullation.Checked;
 Self.SE_vst_anulace_port.Enabled := Self.CHB_RCS_Anullation.Checked;
 if (not Self.CHB_RCS_Anullation.Checked) then
  begin
   Self.SE_vst_anulace_board.Value := 0;
   Self.SE_vst_anulace_port.Value := 0;
  end;
end;

procedure TF_BlkPrejezd.CHB_RCS_NOTClick(Sender: TObject);
begin
 Self.SE_vyst_open_board.Enabled := Self.CHB_RCS_NOT.Checked;
 Self.SE_vyst_open_port.Enabled := Self.CHB_RCS_NOT.Checked;
 if (not Self.CHB_RCS_NOT.Checked) then
  begin
   Self.SE_vyst_open_board.Value := 0;
   Self.SE_vyst_open_port.Value := 0;
  end;
end;

procedure TF_BlkPrejezd.CHB_RCS_BPClick(Sender: TObject);
begin
 Self.SE_vyst_bp_board.Enabled := Self.CHB_RCS_BP.Checked;
 Self.SE_vyst_bp_port.Enabled := Self.CHB_RCS_BP.Checked;
 if (not Self.CHB_RCS_BP.Checked) then
  begin
   Self.SE_vyst_bp_board.Value := 0;
   Self.SE_vyst_bp_port.Value := 0;
  end;
end;

procedure TF_BlkPrejezd.HlavniOpenForm();
 begin
  SetLength(Self.obls,0);
  Self.LB_Stanice.Clear();

  Self.SE_vyst_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vyst_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vyst_bp_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_close_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_open_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_vystraha_board.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_vst_anulace_board.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkPrejezd.NormalOpenForm();
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
    i:Integer;
    oblr:TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls,Self.Blk.OblsRizeni.Count);
  for i := 0 to Self.Blk.OblsRizeni.Count-1 do
    obls[i] := Self.Blk.OblsRizeni[i].id;

  E_Prj_Nazev.Text := glob.name;
  SE_ID.Value := glob.id;


  if (settings.RCSOutputs.Zavrit.board > Cardinal(Self.SE_vyst_close_board.MaxValue)) then
    Self.SE_vyst_close_board.MaxValue := 0;
  Self.SE_vyst_close_port.MaxValue := 0;

  SE_vyst_close_board.Value := settings.RCSOutputs.Zavrit.board;
  SE_vyst_close_port.Value := settings.RCSOutputs.Zavrit.port;


  Self.CHB_RCS_NOT.Checked := settings.RCSOutputs.NOtevritUse;
  Self.CHB_RCS_NOTClick(Self);
  if (settings.RCSOutputs.NOtevrit.board > Cardinal(Self.SE_vyst_open_board.MaxValue)) then
    Self.SE_vyst_open_board.MaxValue := 0;
  Self.SE_vyst_open_port.MaxValue := 0;
  if (settings.RCSOutputs.NOtevritUse) then
   begin
    SE_vyst_open_board.Value := settings.RCSOutputs.NOtevrit.board;
    SE_vyst_open_port.Value := settings.RCSOutputs.NOtevrit.port;
   end;


  Self.CHB_RCS_BP.Checked := settings.RCSOutputs.BlokPozUse;
  Self.CHB_RCS_BPClick(Self);
  if (settings.RCSOutputs.BlokPoz.board > Cardinal(Self.SE_vyst_bp_board.MaxValue)) then
    Self.SE_vyst_open_board.MaxValue := 0;
  Self.SE_vyst_bp_port.MaxValue := 0;
  if (settings.RCSOutputs.BlokPozUse) then
   begin
    SE_vyst_bp_board.Value := settings.RCSOutputs.BlokPoz.board;
    SE_vyst_bp_port.Value := settings.RCSOutputs.BlokPoz.port;
   end;


  if (settings.RCSInputs.Otevreno.board > Cardinal(Self.SE_vst_open_board.MaxValue)) then
    Self.SE_vst_open_board.MaxValue := 0;
  Self.SE_vst_open_port.MaxValue := 0;

  SE_vst_open_board.Value := settings.RCSInputs.Otevreno.board;
  SE_vst_open_port.Value := settings.RCSInputs.Otevreno.port;


  if (settings.RCSInputs.Zavreno.board > Cardinal(Self.SE_vst_close_board.MaxValue)) then
    Self.SE_vst_close_board.MaxValue := 0;
  Self.SE_vst_close_port.MaxValue := 0;

  SE_vst_close_board.Value := settings.RCSInputs.Zavreno.board;
  SE_vst_close_port.Value := settings.RCSInputs.Zavreno.port;


  if (settings.RCSInputs.Vystraha.board > Cardinal(Self.SE_vst_vystraha_board.MaxValue)) then
    Self.SE_vst_vystraha_board.MaxValue := 0;
  Self.SE_vst_vystraha_port.MaxValue := 0;

  SE_vst_vystraha_board.Value := settings.RCSInputs.Vystraha.board;
  SE_vst_vystraha_port.Value := settings.RCSInputs.Vystraha.port;


  Self.CHB_RCS_Anullation.Checked := settings.RCSInputs.anulaceUse;
  Self.CHB_RCS_AnullationClick(Self);
  if (settings.RCSInputs.Anulace.board > Cardinal(Self.SE_vst_anulace_board.MaxValue)) then
    Self.SE_vst_anulace_board.MaxValue := 0;
  Self.SE_vst_anulace_port.MaxValue := 0;
  if (settings.RCSInputs.anulaceUse) then
   begin
    SE_vst_anulace_board.Value := settings.RCSInputs.Anulace.board;
    SE_vst_anulace_port.Value := settings.RCSInputs.Anulace.port;
   end;

  Self.SE_RCS_boardExit(Self);

  Self.tracks := TObjectList<TBlkPrjTrack>.Create();
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

  Self.Caption := 'Přejezd '+glob.name;
  Self.ActiveControl := Self.B_save_P;
 end;

procedure TF_BlkPrejezd.NewOpenForm();
 begin
  Self.E_prj_Nazev.Text := '';
  Self.SE_ID.Value := Blky.GetBlkID(Blky.count-1)+1;

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

  Self.tracks := TObjectList<TBlkPrjTrack>.Create();

  Self.Caption := 'Nový přejezd';
  Self.ActiveControl := Self.E_Prj_Nazev;
 end;

procedure TF_BlkPrejezd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

procedure TF_BlkPrejezd.NewBlkCreate();
 begin
  Self.NewBlk := true;
  OpenForm(Blky.count);
 end;

procedure TF_BlkPrejezd.SaveTracks();
var track: TBlkPrjTrack;
begin
 if (Self.CB_Track.ItemIndex = -1) then
   Exit();

 if (Self.CB_Track.ItemIndex = Self.CB_Track.Items.Count-1) then
  begin
   track := TBlkPrjTrack.Create();
   Self.tracks.Add(track);
  end else
   track := Self.tracks[Self.CB_Track.ItemIndex];

 track.leftOut.Parse(Self.E_Track_Left_Out.Text);
 track.left.Parse(Self.E_Track_Left.Text);
 track.middle.Parse(Self.E_Track_Middle.Text);
 track.right.Parse(Self.E_Track_Right.Text);
 track.rightOut.Parse(Self.E_Track_Right_Out.Text);

 track.opening := TBlkPrjTrackOpening(Self.CB_Track_Open.ItemIndex);
 track.anulTime := EncodeTime(0, StrToInt(LeftStr(Self.ME_Track_Anul_Time.Text, 2)),
                              StrToInt(Copy(Self.ME_Track_Anul_Time.Text, 4, 2)), 0);
end;

end.//unit
