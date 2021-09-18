unit fBlkGroupSignal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, BlockGroupSignal, ComCtrls, BlockDb,
  Generics.Collections;

type
  TF_BlkGroupSignal = class(TForm)
    L_IR01: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_Usek03: TLabel;
    LB_Stations: TListBox;
    B_Storno: TButton;
    B_Apply: TButton;
    GB_Signals: TGroupBox;
    LV_Signals: TListView;
    B_BlkDelete: TButton;
    GB_NewSignal: TGroupBox;
    B_BlkAdd: TButton;
    CB_NewSignal: TComboBox;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SE_RCSport1: TSpinEdit;
    CB_Typ: TComboBox;
    SE_RCSmodule1: TSpinEdit;
    CHB_RCS_Output: TCheckBox;
    SE_RCSmodule2: TSpinEdit;
    SE_RCSport2: TSpinEdit;
    CHB_RCS_Second_Output: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_SignalsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_BlkAddClick(Sender: TObject);
    procedure B_BlkDeleteClick(Sender: TObject);
    procedure SE_RCSmodule1Exit(Sender: TObject);
    procedure SE_RCSmodule2Exit(Sender: TObject);
    procedure CHB_RCS_Second_OutputClick(Sender: TObject);
    procedure CHB_RCS_OutputClick(Sender: TObject);

  private
    new: Boolean;
    blk: TBlkGroupSignal;
    CB_NewSignalData: TArI;

    procedure NewBlkOpenForm();
    procedure EditBlkOpenForm();

    procedure FillCBNewSignal();

  public
    blkIndex: Integer;

    procedure EditBlk(blockIndex: Integer);
    procedure NewBlk();

  end;

var
  F_BlkGroupSignal: TF_BlkGroupSignal;

implementation

uses Block, Area, DataBloky, TechnologieRCS, BlockSignal;

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkGroupSignal.EditBlk(blockIndex: Integer);
begin
  Self.blkIndex := blockIndex;
  Blocks.GetBlkByIndex(blockIndex, TBlk(Self.blk));
  Self.LB_Stations.Clear();

  if (Self.new) then
    Self.NewBlkOpenForm()
  else
    Self.EditBlkOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkGroupSignal.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.SE_RCSmodule1.Value := 1;
  Self.SE_RCSmodule1Exit(Self);
  Self.SE_RCSport1.Value := 0;
  Self.CB_Typ.ItemIndex := -1;

  Self.CHB_RCS_Second_Output.Checked := false;
  Self.CHB_RCS_Second_OutputClick(Self);

  Self.CHB_RCS_Output.Checked := true;
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  Self.LV_Signals.Clear();
  Blocks.FillCB(Self.CB_NewSignal, @Self.CB_NewSignalData, nil, nil, btSignal, -1);

  Self.Caption := 'Nový blok Skupinové návìstidlo';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkGroupSignal.SE_RCSmodule1Exit(Sender: TObject);
begin
  Self.SE_RCSport1.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCSmodule1.Value, Self.SE_RCSport1.Value);
end;

procedure TF_BlkGroupSignal.SE_RCSmodule2Exit(Sender: TObject);
begin
  Self.SE_RCSport2.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCSmodule2.Value, Self.SE_RCSport2.Value);
end;

procedure TF_BlkGroupSignal.EditBlkOpenForm();
var glob: TBlkSettings;
  settings: TBlkGSSettings;
  signalSettings: TBlkSignalSettings;
  Area: TArea;
  LI: TListItem;
  id: Integer;
begin
  glob := Self.blk.GetGlobalSettings();
  settings := Self.blk.GetSettings();
  signalSettings := TBlkSignal(Self.blk).GetSettings();

  for Area in Self.blk.areas do
    Self.LB_Stations.Items.Add(Area.name);

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.CHB_RCS_Output.Checked := (signalSettings.RCSAddrs.count > 0);
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  if (signalSettings.RCSAddrs.count > 0) then
  begin
    if (signalSettings.RCSAddrs[0].board > Cardinal(Self.SE_RCSmodule1.MaxValue)) then
      Self.SE_RCSmodule1.MaxValue := 0;
    Self.SE_RCSport1.MaxValue := 0;

    Self.SE_RCSmodule1.Value := signalSettings.RCSAddrs[0].board;
    Self.SE_RCSport1.Value := signalSettings.RCSAddrs[0].port;
    Self.CB_Typ.ItemIndex := Integer(signalSettings.OutputType);
  end;
  Self.CHB_RCS_Second_Output.Checked := (signalSettings.RCSAddrs.count > 1);
  Self.CHB_RCS_Second_OutputClick(Self);
  if (signalSettings.RCSAddrs.count > 1) then
  begin
    if (signalSettings.RCSAddrs[1].board > Cardinal(Self.SE_RCSmodule2.MaxValue)) then
      Self.SE_RCSmodule2.MaxValue := 0;
    Self.SE_RCSport2.MaxValue := 0;

    Self.SE_RCSmodule2.Value := signalSettings.RCSAddrs[1].board;
    Self.SE_RCSport2.Value := signalSettings.RCSAddrs[1].port;
  end;
  Self.SE_RCSmodule1Exit(Self);

  Self.LV_Signals.Clear();
  for id in settings.signalIds do
  begin
    LI := Self.LV_Signals.Items.Add;
    LI.Caption := IntToStr(id);
    LI.SubItems.Add(Blocks.GetBlkName(id));
  end;

  Self.FillCBNewSignal();

  Self.Caption := 'Upravit blok ' + glob.name + ' (skupinové návìstidlo)';
  Self.ActiveControl := Self.B_Apply;
end;

procedure TF_BlkGroupSignal.NewBlk();
begin
  Self.new := true;
  Self.EditBlk(Blocks.count);
end;

procedure TF_BlkGroupSignal.B_BlkAddClick(Sender: TObject);
var LI: TListItem;
begin
  if (Self.CB_NewSignal.ItemIndex < 0) then
  begin
    Application.MessageBox('Vyberte blok!', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  LI := Self.LV_Signals.Items.Add();
  LI.Caption := IntToStr(Blocks.GetBlkID(Self.CB_NewSignalData[Self.CB_NewSignal.ItemIndex]));
  LI.SubItems.Add(Blocks.GetBlkName(Blocks.GetBlkID(CB_NewSignalData[Self.CB_NewSignal.ItemIndex])));

  Self.FillCBNewSignal();
end;

procedure TF_BlkGroupSignal.B_BlkDeleteClick(Sender: TObject);
begin
  Self.LV_Signals.DeleteSelected();
  Self.FillCBNewSignal();
end;

procedure TF_BlkGroupSignal.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkGroupSignal.CHB_RCS_OutputClick(Sender: TObject);
begin
  Self.SE_RCSmodule1.Enabled := Self.CHB_RCS_Output.Checked;
  Self.SE_RCSport1.Enabled := Self.CHB_RCS_Output.Checked;
  Self.CB_Typ.Enabled := Self.CHB_RCS_Output.Checked;

  if (not Self.CHB_RCS_Output.Checked) then
  begin
    Self.SE_RCSmodule1.Value := 1;
    Self.SE_RCSport1.Value := 0;
    Self.CB_Typ.ItemIndex := -1;
    Self.CHB_RCS_Second_Output.Checked := false;
    Self.CHB_RCS_Second_OutputClick(Self);
  end;
end;

procedure TF_BlkGroupSignal.CHB_RCS_Second_OutputClick(Sender: TObject);
begin
  Self.SE_RCSmodule2.Enabled := Self.CHB_RCS_Second_Output.Checked;
  Self.SE_RCSport2.Enabled := Self.CHB_RCS_Second_Output.Checked;
  Self.SE_RCSmodule2Exit(Self);
  if (not Self.CHB_RCS_Second_Output.Checked) then
  begin
    Self.SE_RCSmodule2.Value := 1;
    Self.SE_RCSport2.Value := 0;
  end;
end;

procedure TF_BlkGroupSignal.B_ApplyClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkGSSettings;
  signalSettings: TBlkSignalSettings;
  LI: TListItem;
  another: TBlk;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplòte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, Self.blkIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.CHB_RCS_Output.Checked) then
  begin
    if (CB_Typ.ItemIndex = -1) then
    begin
      Application.MessageBox('Vyberte typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule1.Value, SE_RCSport1.Value), Self.blk,
      TRCSIOType.output);
    if (another <> nil) then
    begin
      if (Application.MessageBox(PChar('První RCS adresa se již používá na bloku ' + another.name +
        ', chcete pokraèovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;
  if (Self.CHB_RCS_Second_Output.Checked) then
  begin
    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule2.Value, SE_RCSport2.Value), Self.blk,
      TRCSIOType.output);
    if (another <> nil) then
    begin
      if (Application.MessageBox(PChar('Druhá RCS adresa se již používá na bloku ' + another.name +
        ', chcete pokraèovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btGroupSignal;

  if (Self.new) then
  begin
    glob.note := '';
    try
      blk := Blocks.Add(glob) as TBlkGroupSignal;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodaøilo se pøidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.blk.note;
    Self.blk.SetGlobalSettings(glob);
  end;

  settings.signalIds := TList<Integer>.Create();
  settings.signalIds.Clear();
  for LI in Self.LV_Signals.Items do
    settings.signalIds.Add(StrToInt(LI.Caption));
  Self.blk.SetSettings(settings);

  signalSettings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_RCS_Output.Checked) then
  begin
    signalSettings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_RCSmodule1.Value, SE_RCSport1.Value));
    signalSettings.OutputType := TBlkSignalOutputType(CB_Typ.ItemIndex);
  end;
  if (Self.CHB_RCS_Second_Output.Checked) then
    signalSettings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_RCSmodule2.Value, SE_RCSport2.Value));

  signalSettings.fallDelay := 0;
  signalSettings.locked := false;
  signalSettings.events := TObjectList<TBlkSignalTrainEvent>.Create();

  TBlkSignal(Self.blk).SetSettings(signalSettings);

  Self.Close();
end;

procedure TF_BlkGroupSignal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.new := false;
  Self.blkIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkGroupSignal.LV_SignalsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_BlkDelete.Enabled := (Self.LV_Signals.ItemIndex > -1);
end;

procedure TF_BlkGroupSignal.FillCBNewSignal();
var signalIgnore: TArI;
  areas: TArStr;
  i: Integer;
begin
  SetLength(signalIgnore, Self.LV_Signals.Items.count);
  for i := 0 to Self.LV_Signals.Items.count - 1 do
    signalIgnore[i] := StrToInt(Self.LV_Signals.Items.Item[i].Caption);

  if (Self.blk <> nil) then
  begin
    SetLength(areas, Self.blk.areas.count);
    for i := 0 to Self.blk.areas.count - 1 do
      areas[i] := Self.blk.areas[i].id;
  end;

  Blocks.FillCB(Self.CB_NewSignal, @Self.CB_NewSignalData, @signalIgnore, areas, btSignal, -1);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
