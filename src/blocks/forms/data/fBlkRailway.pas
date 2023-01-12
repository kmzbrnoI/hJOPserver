unit fBlkRailway;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, BlockRailway, BlockLinker, Generics.Collections, BlockDb;

type
  TF_BlkRailway = class(TForm)
    B_Save: TButton;
    B_Storno: TButton;
    GB_LinkerA: TGroupBox;
    GB_LinkerB: TGroupBox;
    GB_Trat: TGroupBox;
    SE_Railway_ID: TSpinEdit;
    E_Railway_Name: TEdit;
    L_Name: TLabel;
    L_ID: TLabel;
    Label1: TLabel;
    E_LA_Name: TEdit;
    SE_LA_Id: TSpinEdit;
    Label3: TLabel;
    SE_LB_Id: TSpinEdit;
    E_LB_Name: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    CB_Type: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    CB_Signals: TComboBox;
    GB_TratBlk: TGroupBox;
    GB_Track: TGroupBox;
    B_Track_Add: TButton;
    CB_Track: TComboBox;
    LV_Tracks: TListView;
    B_Track_Del: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure LV_TracksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_Track_AddClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure B_Track_DelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LV_TracksDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LV_TracksDragDrop(Sender, Source: TObject; X, Y: Integer);

  private
    isNewBlock: Boolean;
    railway: TBlkRailway;
    linkerA: TBlkLinker;
    linkerB: TBlkLinker;
    CB_TrackIds: TList<Integer>;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

    procedure FillBlockLI(var LI: TListItem; blockId: Integer);
    function BlockPresent(id: Integer; LV: TListView): Boolean;

  public
    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkRailway: TF_BlkRailway;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, Block, Area;

{$R *.dfm}

procedure TF_BlkRailway.FormCreate(Sender: TObject);
begin
  Self.CB_TrackIds := TList<Integer>.Create();
end;

procedure TF_BlkRailway.FormDestroy(Sender: TObject);
begin
  Self.CB_TrackIds.Free();
end;

procedure TF_BlkRailway.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  var blk := Blocks.GetBlkByIndex(blockIndex);

  if (blk = nil) then
    raise Exception.Create('Blok '+IntToStr(blockIndex)+' neexistuje!');

  Self.railway := nil;
  case (blk.typ) of
    btRailway:
      Self.railway := Blk as TBlkRailway;
    btLinker:
      Self.railway := (Blk as TBlkLinker).parent as TBlkRailway;
  end;

  if (Self.railway = nil) then
    raise Exception.Create('Nelze získat trať!');

  Self.linkerA := Self.railway.linkerA as TBlkLinker;
  Self.linkerB := Self.railway.linkerB as TBlkLinker;

  if (Self.linkerA = nil) then
    raise Exception.Create('Nelze získat úvazku A!');
  if (Self.linkerB = nil) then
    raise Exception.Create('Nelze získat úvazku B!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkRailway.NewBlock();
begin
  Self.isNewBlock := true;
  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkRailway.NewOpenForm();
begin
  Self.E_Railway_Name.Text := '';
  Self.E_LA_Name.Text := '';
  Self.E_LB_Name.Text := '';

  Self.SE_Railway_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_LA_Id.Value := Blocks.GetBlkID(Blocks.count - 1) + 2;
  Self.SE_LB_Id.Value := Blocks.GetBlkID(Blocks.count - 1) + 3;

  Self.CB_Type.ItemIndex := -1;
  Self.CB_Signals.ItemIndex := -1;

  Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, nil, btRT);

  Self.Caption := 'Nový blok Trať';
  Self.ActiveControl := Self.E_Railway_Name;
end;

procedure TF_BlkRailway.EditOpenForm();
begin
  begin
    var glob := Self.railway.GetGlobalSettings();
    Self.E_Railway_Name.Text := glob.name;
    Self.SE_Railway_ID.Value := glob.id;
  end;

  begin
    var glob := Self.linkerA.GetGlobalSettings();
    Self.E_LA_Name.Text := glob.name;
    Self.SE_LA_Id.Value := glob.id;
  end;

  begin
    var glob := Self.linkerB.GetGlobalSettings();
    Self.E_LB_Name.Text := glob.name;
    Self.SE_LB_Id.Value := glob.id;
  end;

  var settings := Self.railway.GetSettings();

  case (settings.rType) of
    TRailwayType.permanent:
      Self.CB_Type.ItemIndex := 0;
    TRailwayType.request:
      Self.CB_Type.ItemIndex := 1;
  end;

  Self.CB_Signals.ItemIndex := Integer(settings.signals);

  for var id in settings.trackIds do
  begin
    var LI: TListItem := Self.LV_Tracks.Items.Add();
    Self.FillBlockLI(LI, id);
  end;

  var areas := TList<TArea>.Create();
  try
    areas.AddRange(Self.linkerA.areas);
    areas.AddRange(Self.linkerB.areas);

    Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, areas, btRT);
  finally
    areas.Free();
  end;

  Self.Caption := 'Upravit blok ' + Self.railway.name + ' (trať)';
  Self.ActiveControl := Self.B_Save;
end;

procedure TF_BlkRailway.CommonOpenForm();
begin
  Self.LV_Tracks.Clear();
  Self.B_Track_Del.Enabled := false;
end;

procedure TF_BlkRailway.LV_TracksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Track_Del.Enabled := (Self.LV_Tracks.Selected <> nil);

  if (Self.CB_Track.Enabled) then
  begin
    Self.CB_Track.ItemIndex := -1;
    if (Self.LV_Tracks.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Tracks.Selected.SubItems[0]);
      for var i := 0 to Self.CB_TrackIds.Count-1 do
        if (Self.CB_TrackIds[i] = id) then
          Self.CB_Track.ItemIndex := i;
    end;
  end;

end;

procedure TF_BlkRailway.LV_TracksDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if (Source = Self.LV_Tracks) then
  begin
    if (Self.LV_Tracks.GetItemAt(X,Y) <> nil) then
    begin
      var myItem := Self.LV_Tracks.Items.Insert(Self.LV_Tracks.GetItemAt(X,Y).Index);
      myItem.Assign(Self.LV_Tracks.Selected);
      Self.LV_Tracks.Selected.Delete();

      for var i: Integer := 0 to Self.LV_Tracks.Items.Count-1 do
        Self.LV_Tracks.Items[i].Caption := IntToStr(i+1);
    end;
  end;
end;

procedure TF_BlkRailway.LV_TracksDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Self.LV_Tracks);
end;

procedure TF_BlkRailway.B_Track_AddClick(Sender: TObject);
begin
  if (not Self.CB_Track.Enabled) then
    Exit();

  if (Self.CB_Track.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_TrackIds[Self.CB_Track.ItemIndex];
  if ((Self.LV_Tracks.Selected = nil) and (Self.BlockPresent(id, Self.LV_Tracks))) then
  begin
    Application.MessageBox('Nelze přidat duplicitní úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Tracks.Selected = nil) then
    LI := Self.LV_Tracks.Items.Add()
  else
    LI := Self.LV_Tracks.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkRailway.B_Track_DelClick(Sender: TObject);
begin
  Self.LV_Tracks.DeleteSelected();
end;

procedure TF_BlkRailway.B_SaveClick(Sender: TObject);
var globRailway, globLinkerA, globLinkerB: TBlkSettings;
  rSettings: TBlkRailwaySettings;
  linkerSettings: TBlkLinkerSettings;
  railway, linkerA, linkerB: Integer;
begin
  if (Self.isNewBlock) then
  begin
    railway := -1;
    linkerA := -1;
    linkerB := -1;
  end else begin
    railway := Blocks.GetBlkIndex(Self.railway.id);
    linkerA := Blocks.GetBlkIndex(Self.linkerA.id);
    linkerB := Blocks.GetBlkIndex(Self.linkerB.id);
  end;

  if ((Self.E_Railway_Name.Text = '') or (Self.E_LA_name.Text = '') or (Self.E_LB_name.Text = '')) then
  begin
    Application.MessageBox('Vyplňte názvy bloků!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(Self.SE_Railway_ID.Value, railway)) then
  begin
    Application.MessageBox('ID trati jiz bylo definovano na jinem bloku !', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(Self.SE_LA_Id.Value, linkerA)) then
  begin
    Application.MessageBox('ID úvazky A jiz bylo definovano na jinem bloku !', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(Self.SE_LB_Id.Value, linkerB)) then
  begin
    Application.MessageBox('ID úvazky B jiz bylo definovano na jinem bloku !', 'Nelze ulozit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Type.ItemIndex < 0) then
  begin
    Application.MessageBox('Vyberte typ zabezpečovacího zařízení trati !', 'Nelze ulozit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Signals.ItemIndex < 0) then
  begin
    Application.MessageBox('Vyberte chování návěstidel trati !', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  globRailway.name := Self.E_Railway_Name.Text;
  globRailway.id := Self.SE_Railway_ID.Value;
  globRailway.typ := btRailway;

  globLinkerA.name := Self.E_LA_Name.Text;
  globLinkerA.id := Self.SE_LA_Id.Value;
  globLinkerA.typ := btLinker;

  globLinkerB.name := Self.E_LB_Name.Text;
  globLinkerB.id := Self.SE_LB_Id.Value;
  globLinkerB.typ := btLinker;

  rSettings.trackIds := TList<Integer>.Create();

  if (isNewBlock) then
  begin
    try
      Self.railway := Blocks.Add(globRailway) as TBlkRailway;
      Self.linkerA := Blocks.Add(globLinkerA) as TBlkLinker;
      Self.linkerB := Blocks.Add(globLinkerB) as TBlkLinker;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.railway.SetGlobalSettings(globRailway);
    Self.linkerA.SetGlobalSettings(globLinkerA);
    Self.linkerB.SetGlobalSettings(globLinkerB);
  end;

  rSettings.linkerA := Self.SE_LA_Id.Value;
  rSettings.linkerB := Self.SE_LB_Id.Value;

  case (Self.CB_Type.ItemIndex) of
    0: rSettings.rType := TRailwayType.permanent;
    1: rSettings.rType := TRailwayType.request;
  end;

  rSettings.signals := TRailwaySignals(Self.CB_Signals.ItemIndex);

  rSettings.trackIds.Clear();
  for var LI in Self.LV_Tracks.Items do
    rSettings.trackIds.Add(StrToInt(LI.SubItems[0]));
  Self.railway.SetSettings(rSettings);

  linkerSettings.parent := Self.SE_Railway_ID.Value;
  Self.linkerA.SetSettings(linkerSettings);

  linkerSettings.parent := Self.SE_Railway_ID.Value;
  Self.linkerB.SetSettings(linkerSettings);

  Self.Close();
  Self.railway.Change();
end;

procedure TF_BlkRailway.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkRailway.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkRailway.FillBlockLI(var LI: TListItem; blockId: Integer);
begin
  LI.Caption := IntToStr(LI.Index+1);
  LI.SubItems.Clear();
  LI.SubItems.Add(IntToStr(blockId));
  LI.SubItems.Add(Blocks.GetBlkName(blockId));
end;

function TF_BlkRailway.BlockPresent(id: Integer; LV: TListView): Boolean;
begin
  for var item: TListItem in LV.Items do
    if (IntToStr(id) = item.SubItems[0]) then
      Exit(true);
  Result := false;
end;

end.
