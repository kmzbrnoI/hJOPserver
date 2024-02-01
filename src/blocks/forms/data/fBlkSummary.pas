unit fBlkSummary;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, BlockSummary, BlockDb, ComCtrls,
  Generics.Collections;

type
  TF_BlkSummary = class(TForm)
    Label1: TLabel;
    E_Name: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    Label2: TLabel;
    SE_ID: TSpinEdit;
    GB_Prejezdy: TGroupBox;
    LV_Crossings: TListView;
    CB_Crossing: TComboBox;
    B_Add: TButton;
    B_Remove: TButton;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_AddClick(Sender: TObject);
    procedure LV_CrossingsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_RemoveClick(Sender: TObject);
  private
    openIndex: Integer;
    block: TBlkSummary;
    isNewBlock: Boolean;
    CB_CrossingId: TList<Integer>;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

    function BlockPresent(id: Integer; LV: TListView): Boolean;
    procedure FillBlockLI(var LI: TListItem; blockId: Integer);

  public

    procedure EditBlock(BlokIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkSummary: TF_BlkSummary;

implementation

uses GetSystems, TechnologieRCS, AreaDb, Area, Block, DataBloky, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkSummary.EditBlock(BlokIndex: Integer);
begin
  Self.openIndex := BlokIndex;
  Self.block := Blocks.GetBlkByIndex(BlokIndex) as TBlkSummary;
  Self.CommonOpenForm();

  if (isNewBlock) then
    Self.NewOpenForm()
  else
    Self.EditOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkSummary.B_AddClick(Sender: TObject);
begin
  if (not Self.CB_Crossing.Enabled) then
    Exit();

  if (Self.CB_Crossing.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte přejezd!', 'Nelze přidat/upravit přejezd', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_CrossingId[Self.CB_Crossing.ItemIndex];
  if ((Self.LV_Crossings.Selected = nil) and (Self.BlockPresent(id, Self.LV_Crossings))) then
  begin
    Application.MessageBox('Nelze přidat duplicitní přejezd!', 'Nelze přidat/upravit přejezd', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Crossings.Selected = nil) then
    LI := Self.LV_Crossings.Items.Add()
  else
    LI := Self.LV_Crossings.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkSummary.B_RemoveClick(Sender: TObject);
begin
  Self.LV_Crossings.DeleteSelected();
end;

procedure TF_BlkSummary.B_save_PClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkSummarySettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název součtové hlásky!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btSummary;

  if (isNewBlock) then
  begin
    try
      block := Blocks.Add(glob) as TBlkSummary;
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

  settings.crossings := TList<Integer>.Create();
  for var LI: TListItem in Self.LV_Crossings.Items do
    settings.crossings.Add(StrToInt(LI.SubItems[0]));
  Self.block.SetSettings(settings);

  Self.Close();
  Self.block.Change();
end;

procedure TF_BlkSummary.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkSummary.CommonOpenForm();
begin
  Self.LV_Crossings.Clear();
  Self.B_Remove.Enabled := false;
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkSummary.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkSummarySettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  for var crossid in settings.crossings do
  begin
    var LI: TListItem := Self.LV_Crossings.Items.Add();
    Self.FillBlockLI(LI, crossid);
  end;

  Blocks.FillCB(Self.CB_Crossing, Self.CB_CrossingId, nil, Self.block.areas, btCrossing);

  Self.Caption := 'Upravit blok ' + glob.name + ' (součtová hláska)';
end;

procedure TF_BlkSummary.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.Count - 1) + 1;
  Blocks.FillCB(Self.CB_Crossing, Self.CB_CrossingId, nil, nil, btCrossing);

  Self.Caption := 'Nový blok Součtová hláska';
end;

procedure TF_BlkSummary.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkSummary.FormCreate(Sender: TObject);
begin
  Self.CB_CrossingId := TList<Integer>.Create();
end;

procedure TF_BlkSummary.FormDestroy(Sender: TObject);
begin
  Self.CB_CrossingId.Free();
end;

procedure TF_BlkSummary.LV_CrossingsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Remove.Enabled := (Self.LV_Crossings.Selected <> nil);

  if (Self.CB_Crossing.Enabled) then
  begin
    Self.CB_Crossing.ItemIndex := -1;
    if (Self.LV_Crossings.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Crossings.Selected.SubItems[0]);
      for var i := 0 to Self.CB_CrossingId.Count-1 do
        if (Self.CB_CrossingId[i] = id) then
          Self.CB_Crossing.ItemIndex := i;
    end;
  end;
end;

procedure TF_BlkSummary.NewBlock();
begin
  Self.isNewBlock := true;
  Self.EditBlock(Blocks.Count);
end;

function TF_BlkSummary.BlockPresent(id: Integer; LV: TListView): Boolean;
begin
  for var item: TListItem in LV.Items do
    if (IntToStr(id) = item.SubItems[0]) then
      Exit(true);
  Result := false;
end;

procedure TF_BlkSummary.FillBlockLI(var LI: TListItem; blockId: Integer);
begin
  LI.Caption := IntToStr(LI.Index+1);
  LI.SubItems.Clear();
  LI.SubItems.Add(IntToStr(blockId));
  LI.SubItems.Add(Blocks.GetBlkName(blockId));
end;

end.
