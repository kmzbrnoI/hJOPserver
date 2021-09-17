unit fBlkSummary;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, BlockSummary, BlockDb, ComCtrls,
  Generics.Collections;

type
  TF_BlkSummary = class(TForm)
    L_P02: TLabel;
    E_Name: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_ID: TLabel;
    SE_ID: TSpinEdit;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    GB_Prejezdy: TGroupBox;
    LV_Prejezdy: TListView;
    B_Remove: TButton;
    CB_Prj_Add: TComboBox;
    B_Add: TButton;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_AddClick(Sender: TObject);
    procedure LV_PrejezdyChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_RemoveClick(Sender: TObject);
  private
    OpenIndex: Integer;
    Blk: TBlkSummary;
    NewBlk: Boolean;
    areas: TArstr; // oblasti rizeni, ve kterych se blok nachazi

    prejezdy: TList<Integer>;
    CB_PrjAddData: TArI;

    procedure NormalOpenForm();
    procedure MainOpenForm();
    procedure NewOpenForm();

    procedure FillNewPrjCB();

  public

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate();
  end;

var
  F_BlkSummary: TF_BlkSummary;

implementation

uses GetSystems, TechnologieRCS, AreaDb, Area, Block, FileSystem,
  DataBloky;

{$R *.dfm}

procedure TF_BlkSummary.OpenForm(BlokIndex: Integer);
begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.MainOpenForm();

  if (NewBlk) then
    Self.NewOpenForm()
  else
    Self.NormalOpenForm();

  Self.FillNewPrjCB();
  Self.ShowModal();
end;

procedure TF_BlkSummary.B_AddClick(Sender: TObject);
var LI: TListItem;
  Blk: TBlk;
begin
  if (Self.CB_Prj_Add.ItemIndex = -1) then
  begin
    Application.MessageBox('Je třeba vybrat přejezd k přidání!', 'Nelze přidat přejezd', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  Blocks.GetBlkByIndex(Self.CB_PrjAddData[Self.CB_Prj_Add.ItemIndex], Blk);
  if ((Blk <> nil) and (Blk.typ = btCrossing)) then
  begin
    Self.prejezdy.Add(Blk.id);

    LI := Self.LV_Prejezdy.Items.Add();
    LI.Caption := Blk.name;
  end;

  Self.FillNewPrjCB();
end;

procedure TF_BlkSummary.B_RemoveClick(Sender: TObject);
begin
  if (Self.LV_Prejezdy.Selected <> nil) then
  begin
    Self.prejezdy.Delete(Self.LV_Prejezdy.ItemIndex);
    Self.LV_Prejezdy.DeleteSelected();
    Self.FillNewPrjCB();
  end;
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
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btSummary;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkSummary;
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

  settings.crossings := TList<Integer>.Create(Self.prejezdy);
  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
end;

procedure TF_BlkSummary.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkSummary.MainOpenForm();
begin
  SetLength(Self.areas, 0);

  Self.LB_Stanice.Clear();
  Self.prejezdy.Clear();
  Self.LV_Prejezdy.Clear();

  Self.B_Remove.Enabled := false;
end;

procedure TF_BlkSummary.NormalOpenForm();
var glob: TBlkSettings;
  settings: TBlkSummarySettings;
  i, prjid: Integer;
  LI: TListItem;
  Area: TArea;
begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for Area in Self.Blk.areas do
    Self.LB_Stanice.Items.Add(Area.name);

  SetLength(areas, Self.Blk.areas.Count);
  for i := 0 to Self.Blk.areas.Count - 1 do
    areas[i] := Self.Blk.areas[i].id;

  E_Name.Text := glob.name;
  SE_ID.Value := glob.id;

  for prjid in settings.crossings do
  begin
    Self.prejezdy.Add(prjid);
    LI := Self.LV_Prejezdy.Items.Add();
    LI.Caption := Blocks.GetBlkName(prjid);
  end;

  Self.Caption := 'Upravit blok ' + glob.name + ' (součtová hláska)';
  Self.ActiveControl := Self.B_save_P;
end;

procedure TF_BlkSummary.NewOpenForm();
begin
  E_Name.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.Count - 1) + 1;

  Self.Caption := 'Nový blok Součtová hláska';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkSummary.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable();
end;

procedure TF_BlkSummary.FormCreate(Sender: TObject);
begin
  Self.prejezdy := TList<Integer>.Create();
end;

procedure TF_BlkSummary.FormDestroy(Sender: TObject);
begin
  Self.prejezdy.Free();
end;

procedure TF_BlkSummary.LV_PrejezdyChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Remove.Enabled := (Self.LV_Prejezdy.Selected <> nil);
end;

procedure TF_BlkSummary.NewBlkCreate();
begin
  Self.NewBlk := true;
  OpenForm(Blocks.Count);
end;

procedure TF_BlkSummary.FillNewPrjCB();
var ignore: TArI;
  i: Integer;
begin
  SetLength(ignore, Self.prejezdy.Count);
  for i := 0 to Self.prejezdy.Count - 1 do
    ignore[i] := Self.prejezdy[i];

  Blocks.FillCB(Self.CB_Prj_Add, @Self.CB_PrjAddData, @ignore, nil, btCrossing);
end;

end.// unit
