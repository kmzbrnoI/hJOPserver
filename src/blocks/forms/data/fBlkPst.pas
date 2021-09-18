unit fBlkPst;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, BlockPst,
  Vcl.ComCtrls, Generics.Collections;

type
  TF_BlkPst = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    B_Storno: TButton;
    B_Apply: TButton;
    GB_RCS: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    SE_RCS_Take_Module: TSpinEdit;
    SE_RCS_Indication_Module: TSpinEdit;
    SE_RCS_Indication_Port: TSpinEdit;
    SE_RCS_Take_Port: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SE_RCS_Release_Module: TSpinEdit;
    SE_RCS_Release_Port: TSpinEdit;
    SE_RCS_Horn_Port: TSpinEdit;
    SE_RCS_Horn_Module: TSpinEdit;
    Label9: TLabel;
    GB_Tracks: TGroupBox;
    GB_TrackEdit: TGroupBox;
    CB_Track: TComboBox;
    B_Track_Ok: TButton;
    LV_Tracks: TListView;
    GB_Turnouts: TGroupBox;
    LV_Turnouts: TListView;
    GB_TurnoutEdit: TGroupBox;
    CB_Turnout: TComboBox;
    B_Turnout_Ok: TButton;
    GB_Refugees: TGroupBox;
    LV_Refugees: TListView;
    GB_RefEdit: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_Ref_Block: TComboBox;
    CB_Ref_Pos: TComboBox;
    B_Ref_Ok: TButton;
    GB_Signals: TGroupBox;
    GB_Signal_Edit: TGroupBox;
    CB_Signal: TComboBox;
    B_Signal_Ok: TButton;
    LV_Signals: TListView;
    B_Track_Del: TButton;
    B_Ref_Del: TButton;
    B_Turnout_Del: TButton;
    B_Signal_Delete: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure LV_TracksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_Track_OkClick(Sender: TObject);
    procedure B_Track_DelClick(Sender: TObject);
  private
    newBlk: Boolean;
    blk: TBlkPst;
    CB_TrackItems: TList<Integer>;
    openIndex: Integer;

    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();

    procedure FillTrackLI(var LI: TListItem; blockId: Integer);
    function TrackPresent(id: Integer): Boolean;

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;


var
  F_BlkPst: TF_BlkPst;

implementation

uses BlockDb, Block, Area, DataBloky;

{$R *.dfm}

procedure TF_BlkPst.EditBlock(blockIndex: Integer);
begin
  Self.openIndex := blockIndex;
  Blocks.GetBlkByIndex(blockIndex, TBlk(Self.blk));
  Self.CommonOpenForm();

  if (Self.newBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkPst.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Blocks.FillCB(Self.CB_Track, Self.CB_TrackItems, nil, nil, btTrack, btRT, -1);

  Self.Caption := 'Nový blok Pomocné stavědlo';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkPst.NormalOpenForm();
begin
  var glob := Self.blk.GetGlobalSettings();
  var pstSettings := Self.blk.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  for var i := 0 to pstSettings.tracks.Count-1 do
  begin
    var LI := Self.LV_Tracks.Items.Add();
    Self.FillTrackLI(LI, pstSettings.tracks[i]);
  end;

  Blocks.FillCB(Self.CB_Track, Self.CB_TrackItems, nil, Self.blk.areas, btTrack, btRT, -1);

  Self.Caption := 'Upravit blok ' + glob.name + ' (pomocné stavědlo)';
  Self.ActiveControl := Self.B_Apply;
end;

procedure TF_BlkPst.CommonOpenForm();
begin
  Self.LV_Tracks.Clear();
  Self.LV_Turnouts.Clear();
  Self.LV_Refugees.Clear();
  Self.LV_Signals.Clear();
end;

procedure TF_BlkPst.NewBlock();
begin
  Self.newBlk := true;
  Self.EditBlock(Blocks.count);
end;

procedure TF_BlkPst.B_ApplyClick(Sender: TObject);
var glob: TBlkSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btPst;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkPst;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.blk.note;
    Self.blk.SetGlobalSettings(glob);
  end;

  Self.Close();
  Self.blk.Change();
end;

procedure TF_BlkPst.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkPst.B_Track_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané úseky z pomocného stavědla?'), 'Mazání úseků',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for var i := Self.LV_Tracks.Items.Count - 1 downto 0 do
      if (Self.LV_Tracks.Items[i].Selected) then
        Self.LV_Tracks.Items.Delete(i);
  end;
end;

procedure TF_BlkPst.B_Track_OkClick(Sender: TObject);
begin
  if (not Self.CB_Track.Enabled) then
    Exit();

  if (Self.CB_Track.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_TrackItems[Self.CB_Track.ItemIndex];
  if ((Self.LV_Tracks.Selected = nil) and (Self.TrackPresent(id))) then
  begin
    Application.MessageBox('Nelze přidat duplicitní úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Tracks.Selected = nil) then
    LI := Self.LV_Tracks.Items.Add()
  else
    LI := Self.LV_Tracks.Selected;

  Self.FillTrackLI(LI, id);
end;

procedure TF_BlkPst.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.newBlk := false;
  Self.openIndex := -1;
  BlokyTableData.UpdateTable();
end;

procedure TF_BlkPst.FormCreate(Sender: TObject);
begin
  Self.CB_TrackItems := TList<Integer>.Create();
end;

procedure TF_BlkPst.FormDestroy(Sender: TObject);
begin
  Self.CB_TrackItems.Free();
end;

procedure TF_BlkPst.LV_TracksChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Track_Del.Enabled := (Self.LV_Tracks.Selected <> nil);

  if (Self.CB_Track.Enabled) then
  begin
    Self.CB_Track.ItemIndex := -1;
    if (Self.LV_Tracks.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Tracks.Selected.SubItems[0]);
      for var i := 0 to Self.CB_TrackItems.Count-1 do
        if (Self.CB_TrackItems[i] = id) then
          Self.CB_Track.ItemIndex := i;
    end;
  end;
end;

function TF_BlkPst.TrackPresent(id: Integer): Boolean;
begin
  for var item: TListItem in Self.LV_Tracks.Items do
    if (IntToStr(id) = item.SubItems[0]) then
      Exit(true);
  Result := false;
end;

procedure TF_BlkPst.FillTrackLI(var LI: TListItem; blockId: Integer);
begin
  LI.Caption := IntToStr(LI.Index+1);
  LI.SubItems.Clear();
  LI.SubItems.Add(IntToStr(blockId));
  LI.SubItems.Add(Blocks.GetBlkName(blockId));
end;

end.
