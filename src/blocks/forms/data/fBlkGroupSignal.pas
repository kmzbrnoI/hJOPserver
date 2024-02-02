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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    isNewBlock: Boolean;
    block: TBlkGroupSignal;
    CB_SignalId: TList<Integer>;
    openIndex: Integer;

    procedure NewOpenForm();
    procedure EditOpenForm();

    procedure FillCBNewSignal();

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkGroupSignal: TF_BlkGroupSignal;

implementation

uses Block, Area, DataBloky, TechnologieRCS, BlockSignal, ownGuiUtils;

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkGroupSignal.FormCreate(Sender: TObject);
begin
  Self.CB_SignalId := TList<Integer>.Create();
end;

procedure TF_BlkGroupSignal.FormDestroy(Sender: TObject);
begin
  Self.CB_SignalId.Free();
end;

procedure TF_BlkGroupSignal.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkGroupSignal;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkGroupSignal.NewOpenForm();
begin
  Self.block := nil;
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
  Blocks.FillCB(Self.CB_NewSignal, Self.CB_SignalId, nil, nil, btSignal);

  Self.Caption := 'Nový blok Skupinové návìstidlo';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkGroupSignal.SE_RCSmodule1Exit(Sender: TObject);
begin
  Self.SE_RCSport1.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCSmodule1.Value, Self.SE_RCSport1.Value);
end;

procedure TF_BlkGroupSignal.SE_RCSmodule2Exit(Sender: TObject);
begin
  Self.SE_RCSport2.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCSmodule2.Value, Self.SE_RCSport2.Value);
end;

procedure TF_BlkGroupSignal.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkGSSettings;
  signalSettings: TBlkSignalSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();
  signalSettings := TBlkSignal(Self.block).GetSettings();

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
  for var id in settings.signalIds do
  begin
    var LI: TListItem := Self.LV_Signals.Items.Add;
    LI.Caption := IntToStr(id);
    LI.SubItems.Add(Blocks.GetBlkName(id));
  end;

  Self.FillCBNewSignal();

  Self.Caption := 'Upravit blok ' + glob.name + ' (skupinové návìstidlo)';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkGroupSignal.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkGroupSignal.B_BlkAddClick(Sender: TObject);
begin
  if (Self.CB_NewSignal.ItemIndex < 0) then
  begin
    StrMessageBox('Vyberte blok!', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem := Self.LV_Signals.Items.Add();
  LI.Caption := IntToStr(Self.CB_SignalId[Self.CB_NewSignal.ItemIndex]);
  LI.SubItems.Add(Blocks.GetBlkName(CB_SignalId[Self.CB_NewSignal.ItemIndex]));

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
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplòte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.CHB_RCS_Output.Checked) then
  begin
    if (CB_Typ.ItemIndex = -1) then
    begin
      StrMessageBox('Vyberte typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    var another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule1.Value, SE_RCSport1.Value), Self.block,
      TRCSIOType.output);
    if (another <> nil) then
    begin
      if (StrMessageBox(PChar('První RCS adresa se již používá na bloku ' + another.name +
        ', chcete pokraèovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;
  if (Self.CHB_RCS_Second_Output.Checked) then
  begin
    var another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule2.Value, SE_RCSport2.Value), Self.block,
      TRCSIOType.output);
    if (another <> nil) then
    begin
      if (StrMessageBox(PChar('Druhá RCS adresa se již používá na bloku ' + another.name +
        ', chcete pokraèovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btGroupSignal;

  if (Self.isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkGroupSignal;
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nepodaøilo se pøidat blok.', E, 'Nelze uložit data');
        Exit();
      end;
    end;
  end else begin
    try
      Self.block.SetGlobalSettings(glob);
    except
      on E: Exception do
      begin
        ExceptionMessageBox('Nepodaøilo se uložit blok.', E, 'Nelze uložit data');
        Exit();
      end;
    end;
  end;

  try
    var settings: TBlkGSSettings;
    settings.signalIds := TList<Integer>.Create();
    settings.signalIds.Clear();
    for var LI: TListItem in Self.LV_Signals.Items do
      settings.signalIds.Add(StrToInt(LI.Caption));
    Self.block.SetSettings(settings);

    var signalSettings: TBlkSignalSettings;
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

    TBlkSignal(Self.block).SetSettings(signalSettings);
  except
    on E: Exception do
    begin
      ExceptionMessageBox('Neoèekávaná chyba.', E);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_BlkGroupSignal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  Self.block := nil;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkGroupSignal.LV_SignalsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_BlkDelete.Enabled := (Self.LV_Signals.ItemIndex > -1);
end;

procedure TF_BlkGroupSignal.FillCBNewSignal();
var ignore: TList<Integer>;
begin
  ignore := TList<Integer>.Create();
  try
    for var LI: TListItem in Self.LV_Signals.Items do
      ignore.Add(StrToInt(LI.Caption));

    if (Self.block <> nil) then
      Blocks.FillCB(Self.CB_NewSignal, Self.CB_SignalId, ignore, Self.block.areas, btSignal)
    else
      Blocks.FillCB(Self.CB_NewSignal, Self.CB_SignalId, ignore, nil, btSignal);
  finally
    ignore.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
