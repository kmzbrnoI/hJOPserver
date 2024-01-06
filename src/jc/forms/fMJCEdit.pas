unit fMJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, TechnologieMultiJC, Generics.Collections,
  Spin, BlockDb, Area;

type
  TF_MJCEdit = class(TForm)
    L_VC_01: TLabel;
    CHB_AutoName: TCheckBox;
    E_Name: TEdit;
    GB_JCs: TGroupBox;
    GB_JC_New: TGroupBox;
    CB_JC_Add: TComboBox;
    B_JC_Add: TButton;
    LV_JCs: TListView;
    B_JC_Remove: TButton;
    GB_VB: TGroupBox;
    GroupBox2: TGroupBox;
    CB_VB_New: TComboBox;
    B_VB_New: TButton;
    LV_VBs: TListView;
    B_VB_Remove: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    SE_ID: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure B_JC_AddClick(Sender: TObject);
    procedure B_VB_NewClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure LV_JCsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_VBsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_JC_RemoveClick(Sender: TObject);
    procedure B_VB_RemoveClick(Sender: TObject);
  private
    openMJC: TMultiJC;
    new: Boolean;

    CB_VB_Ids: TList<Integer>;
    CB_JC_Ids: TList<Integer>;

    procedure UpdateJCCb();
    procedure UpdateVBCb();
    procedure FillJCName();
    procedure RecalcJCIndexes();
    procedure RecalcVBIndexes();

    procedure NormalOpenForm();
    procedure EmptyOpenForm();

    function Areas(): TList<TArea>;

  public
    procedure EditMJC(mJC: TMultiJC);
    procedure NewMJC(template: TMultiJC);

  end;

var
  F_MJCEdit: TF_MJCEdit;

implementation

{$R *.dfm}

uses TJCDatabase, TechnologieJC, Block, BlockSignal, TMultiJCDatabase,
  DataMultiJC;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.openMJC := nil;
  Self.new := false;
end;

procedure TF_MJCEdit.FormCreate(Sender: TObject);
begin
  Self.openMJC := nil;
  Self.new := false;
  Self.CB_VB_Ids := TList<Integer>.Create();
  Self.CB_JC_Ids := TList<Integer>.Create();
end;

procedure TF_MJCEdit.FormDestroy(Sender: TObject);
begin
  Self.CB_VB_Ids.Free();
  Self.CB_JC_Ids.Free();
end;

procedure TF_MJCEdit.LV_JCsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_JC_Remove.Enabled := (Self.LV_JCs.Selected <> nil);

  if (Self.LV_JCs.Selected = nil) then
  begin
    Self.CB_JC_Add.ItemIndex := -1;
    Exit();
  end;

  var selectedId := StrToInt(Self.LV_JCs.Selected.SubItems[0]);
  for var i := 0 to Self.CB_JC_Ids.Count - 1 do
    if (Self.CB_JC_ids[i] = selectedId) then
      Self.CB_JC_Add.ItemIndex := i;
end;

procedure TF_MJCEdit.LV_VBsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_VB_Remove.Enabled := (Self.LV_VBs.Selected <> nil);

  if (Self.LV_VBs.Selected = nil) then
  begin
    Self.CB_VB_New.ItemIndex := -1;
    Exit();
  end;

  var selectedId := StrToInt(Self.LV_VBs.Selected.SubItems[0]);
  for var i := 0 to Self.CB_VB_Ids.Count - 1 do
    if (Self.CB_VB_Ids[i] = selectedId) then
      Self.CB_VB_New.ItemIndex := i;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.EditMJC(mJC: TMultiJC);
begin
  Self.openMJC := mJC;

  Self.B_JC_Remove.Enabled := false;
  Self.B_VB_Remove.Enabled := false;

  Self.LV_JCs.Clear();
  Self.LV_VBs.Clear();

  if (mJC = nil) then
    Self.EmptyOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_MJCEdit.NewMJC(template: TMultiJC);
begin
  Self.new := true;
  Self.EditMJC(template);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.NormalOpenForm();
begin
  if (Self.new) then
    Self.SE_ID.Value := Self.openMJC.id + 1
  else
    Self.SE_ID.Value := Self.openMJC.id;

  Self.E_Name.Text := Self.openMJC.name;

  for var jcid in Self.openMJC.data.JCs do
  begin
    var JC := JCDb.GetJCByID(jcid);
    if (JC = nil) then
      continue;
    var LI: TListItem := Self.LV_JCs.Items.Add();
    LI.Caption := IntToStr(Self.LV_JCs.Items.Count-1);
    LI.SubItems.Add(IntToStr(jcid));
    LI.SubItems.Add(JC.name);
  end;

  for var vbId in Self.openMJC.data.vb do
  begin
    var blk := Blocks.GetBlkByID(vbId);
    if (blk = nil) then
      continue;
    var LI: TListItem := Self.LV_VBs.Items.Add();
    LI.Caption := IntToStr(Self.LV_VBs.Items.Count-1);
    LI.SubItems.Add(IntToStr(vbId));
    LI.SubItems.Add(blk.GetGlobalSettings.name);
  end;

  Self.B_VB_New.Enabled := (Self.openMJC.data.JCs.Count > 0);
  Self.CB_VB_New.Enabled := (Self.openMJC.data.JCs.Count > 0);

  Self.UpdateJCCb();
  Self.UpdateVBCb();

  if (Self.new) then
    Self.Caption := 'Nová složená jízdní cesta'
  else
    Self.Caption := 'Složená jízdní cesta ' + Self.openMJC.name;
end;

procedure TF_MJCEdit.EmptyOpenForm();
begin
  if (MultiJCDb.Count > 0) then
    Self.SE_ID.Value := MultiJCDb.Items[MultiJCDb.Count - 1].id + 1
  else
    Self.SE_ID.Value := 1;

  Self.B_VB_New.Enabled := false;
  Self.CB_VB_New.Enabled := false;

  Self.UpdateJCCb();
  Self.UpdateVBCb();

  Self.E_Name.Text := '';
  Self.ActiveControl := CB_JC_Add;
  Self.Caption := 'Nová složená jízdní cesta';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.UpdateJCCb();
begin
  Self.CB_JC_Ids.Clear();
  Self.CB_JC_Add.Clear();
  for var JC in JCDb do
  begin
    Self.CB_JC_Add.Items.Add(JC.name);
    Self.CB_JC_ids.Add(JC.id);
  end;
end;

procedure TF_MJCEdit.UpdateVBCb();
begin
  var areas := Self.Areas();
  try
    Blocks.FillCB(Self.CB_VB_New, Self.CB_VB_Ids, nil, areas, btTrack);
  finally
    areas.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_MJCEdit.Areas(): TList<TArea>;
begin
  Result := TList<TArea>.Create();
  if (Self.LV_JCs.Items.Count < 1) then
    Exit();

  for var item: TListItem in Self.LV_JCs.Items do
  begin
    var jcId := StrToInt(item.SubItems[0]);
    var signal := TBlkSignal(Blocks.GetBlkByID(JCDb.GetJCByID(jcId).data.signalId));
    for var area: TArea in signal.areas do
      if (not Result.Contains(area)) then
        Result.Add(area);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_JC_AddClick(Sender: TObject);
var LI: TListItem;
begin
  if (Self.CB_JC_Add.ItemIndex < 0) then
    Exit();

  if (Self.LV_JCs.Selected = nil) then begin
    LI := Self.LV_JCs.Items.Add();
    LI.SubItems.Add('');
    LI.SubItems.Add('');
  end else
    LI := Self.LV_JCs.Selected;

  var id := Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex];
  LI.SubItems[0] := IntToStr(id);
  LI.SubItems[1] := JCDb.GetJCByID(id).name;
  Self.RecalcJCIndexes();

  if (Self.LV_JCs.Items.Count = 1) then
  begin
    Self.B_VB_New.Enabled := true;
    Self.CB_VB_New.Enabled := true;
    Self.UpdateVBCb();
  end;

  if (Self.CHB_AutoName.Checked) then
    Self.FillJCName();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_VB_NewClick(Sender: TObject);
var LI: TListItem;
begin
  if (Self.CB_VB_New.ItemIndex < 0) then
    Exit();

  if (Self.LV_VBs.Selected = nil) then begin
    LI := Self.LV_VBs.Items.Add();
    LI.SubItems.Add('');
    LI.SubItems.Add('');
  end else
    LI := Self.LV_VBs.Selected;

  var id := Self.CB_VB_Ids[Self.CB_VB_New.ItemIndex];
  LI.SubItems[0] := IntToStr(id);
  LI.SubItems[1] := Blocks.GetBlkName(id);
  Self.RecalcVBIndexes();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_SaveClick(Sender: TObject);
var data: TMultiJCData;
begin
  if (Self.LV_JCs.Items.Count < 2) then
  begin
    Application.MessageBox('Složená JC musí obsahovat alespoň 2 JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název složené JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  begin
    var check := MultiJCDb.GetJCByID(Self.SE_ID.Value);
    if ((check <> nil) and (check <> Self.openMJC)) then
    begin
      Application.MessageBox('Složená jízdní cesta s tímto ID již existuje', 'Nelze uložit data',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  data.id := Self.SE_ID.Value;
  data.name := Self.E_Name.Text;

  var origSignal: TBlkSignal := nil;

  if (Self.new) then
  begin
    data.JCs := nil; // dulezite pro pridavani JC
    data.vb := nil;

    try
      Self.openMJC := MultiJCDb.Add(data);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat složenou JC' + #13#10 + E.Message), 'Chyba',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    data.JCs := Self.openMJC.data.JCs; // dulezite pro pridavani JC
    data.vb := Self.openMJC.data.vb;

    origSignal := Self.openMJC.StartSignal();

    var prevIndex := MultiJCDb.GetJCIndexByID(Self.openMJC.id);
    Self.openMJC.data := data;
    MultiJCDb.IDChanged(prevIndex);
  end;

  Self.openMJC.data.JCs.Clear();
  for var LI: TListItem in Self.LV_JCs.Items do
    Self.openMJC.data.JCs.Add(StrToInt(LI.SubItems[0]));

  Self.openMJC.data.vb.Clear();
  for var LI: TListItem in Self.LV_VBs.Items do
    Self.openMJC.data.vb.Add(StrToInt(LI.SubItems[0]));

  MultiJCDb.SignalChanged(Self.openMJC, origSignal);
  Self.openMJC.changed := true;
  MultiJCTableData.UpdateTable();
  Self.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_JC_RemoveClick(Sender: TObject);
begin
  Self.LV_JCs.DeleteSelected();
  Self.UpdateJCCb();
  Self.UpdateVBCb();
  Self.RecalcJCIndexes();

  if (Self.CHB_AutoName.Checked) then
    Self.FillJCName();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_VB_RemoveClick(Sender: TObject);
begin
  Self.LV_VBs.DeleteSelected();
  Self.UpdateVBCb();
  Self.RecalcVBIndexes();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.FillJCName();
begin
  if (Self.LV_JCs.Items.Count = 0) then
  begin
    Self.E_Name.Text := '';
    Exit();
  end;

  var firstJcId := StrToInt(Self.LV_JCs.Items[0].SubItems[0]);
  var lastJcId := StrToInt(Self.LV_JCs.Items[Self.LV_JCs.Items.Count-1].SubItems[0]);
  Self.E_Name.Text := Blocks.GetBlkName(JCDb.GetJCByID(firstJcId).data.signalId) + ' > ' +
    JCDb.GetJCByID(lastJcId).lastTrack.name;
end;

procedure TF_MJCEdit.RecalcJCIndexes();
begin
  for var i: Integer := 0 to Self.LV_JCs.Items.Count-1 do
    Self.LV_JCs.Items[i].Caption := IntToStr(i);
end;

procedure TF_MJCEdit.RecalcVBIndexes();
begin
  for var i: Integer := 0 to Self.LV_VBs.Items.Count-1 do
    Self.LV_VBs.Items[i].Caption := IntToStr(i);
end;

end.
