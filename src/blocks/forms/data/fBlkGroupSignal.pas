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
    GB_RCSs: TGroupBox;
    LV_RCSs: TListView;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label11: TLabel;
    CB_RCS_Type: TComboBox;
    SE_RCS_inv_module: TSpinEdit;
    SE_RCS_inv_port: TSpinEdit;
    CHB_RCS_Inverse_Output: TCheckBox;
    SE_RCS_inv_system: TSpinEdit;
    SE_RCS_system: TSpinEdit;
    SE_RCS_module: TSpinEdit;
    SE_RCS_port: TSpinEdit;
    B_RCS_OK: TButton;
    B_RCS_Delete: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_SignalsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure B_BlkAddClick(Sender: TObject);
    procedure B_BlkDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LV_SignalsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_RCSsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CHB_RCS_Inverse_OutputClick(Sender: TObject);
    procedure LV_RCSsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure B_RCS_DeleteClick(Sender: TObject);
    procedure B_RCS_OKClick(Sender: TObject);

  private
    isNewBlock: Boolean;
    block: TBlkGroupSignal;
    CB_SignalId: TList<Integer>;
    openIndex: Integer;

    const LV_RCS_ADDR_UNUSED: string = '-';

    procedure CommonOpenForm();
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

uses Block, Area, DataBloky, RCSc, RCSsc, BlockSignal, ownGuiUtils, ownConvert,
  TRailVehicle;

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

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkGroupSignal.CommonOpenForm();
begin
  Self.SE_RCS_system.MaxValue := RCSs._RCSS_MAX;
  Self.SE_RCS_inv_system.MaxValue := RCSs._RCSS_MAX;
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkGroupSignal.NewOpenForm();
begin
  Self.block := nil;
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.LV_RCSs.Clear();
  Self.LV_RCSsChange(Self.LV_RCSs, nil, TItemChange.ctState);

  Self.LV_Signals.Clear();
  Blocks.FillCB(Self.CB_NewSignal, Self.CB_SignalId, nil, nil, btSignal);

  Self.Caption := 'Nový blok Skupinové návìstidlo';
  Self.ActiveControl := Self.E_Name;
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

  Self.LV_RCSs.Clear();
  Self.LV_RCSsChange(Self.LV_RCSs, nil, TItemChange.ctState);
  for var i: Integer := 0 to signalSettings.rcsOutputs.Count-1 do
  begin
    var li: TListItem := Self.LV_RCSs.Items.Add();
    li.Data := Pointer(signalSettings.rcsOutputs[i].outputType);
    li.Caption := IntToStr(i);
    li.SubItems.Add(TBlkSignal.SignalOutputTypeToStr(signalSettings.rcsOutputs[i].outputType));
    li.SubItems.Add(signalSettings.rcsOutputs[i].rcs.ToString());
    if (signalSettings.rcsOutputs[i].inverseRcs.enabled) then
      li.SubItems.Add(signalSettings.rcsOutputs[i].inverseRcs.addr.ToString())
    else
      li.SubItems.Add(LV_RCS_ADDR_UNUSED)
  end;

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

  Self.CommonOpenForm();
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

procedure TF_BlkGroupSignal.B_RCS_DeleteClick(Sender: TObject);
begin
  if (StrMessageBox('Opravdu chcete smazat vybrané RCS výstupy?', 'Mazání zámkù',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    Self.LV_RCSs.DeleteSelected();
  end;
end;

procedure TF_BlkGroupSignal.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkGroupSignal.CHB_RCS_Inverse_OutputClick(Sender: TObject);
begin
  Self.SE_RCS_inv_system.Enabled := Self.CHB_RCS_Inverse_Output.Checked;
  Self.SE_RCS_inv_module.Enabled := Self.CHB_RCS_Inverse_Output.Checked;
  Self.SE_RCS_inv_port.Enabled := Self.CHB_RCS_Inverse_Output.Checked;
  if (not Self.CHB_RCS_Inverse_Output.Checked) then
  begin
    Self.SE_RCS_inv_system.Value := 0;
    Self.SE_RCS_inv_module.Value := 0;
    Self.SE_RCS_inv_port.Value := 0;
  end;
end;

procedure TF_BlkGroupSignal.B_ApplyClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplòte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  try
    var messages: string := '';
    for var li: TListItem in Self.LV_RCSs.Items do
    begin
      var addr: TRCSsAddr;
      addr.Load(li.SubItems[1]);
      var another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.output);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString() + '.' + #13#10;
      if (li.SubItems[2] <> LV_RCS_ADDR_UNUSED) then
      begin
        addr.Load(li.SubItems[2]);
        another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.output);
        if (another <> nil) then
          messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString() + '.' + #13#10;
      end;
    end;

    if (messages <> '') then
      if (StrMessageBox(messages + '\nPokraèovat?', 'Varování', MB_OK OR MB_ICONWARNING) = mrNo) then
        Exit();


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

    var settings: TBlkGSSettings;
    settings.signalIds := TList<Integer>.Create();
    settings.signalIds.Clear();
    for var LI: TListItem in Self.LV_Signals.Items do
      settings.signalIds.Add(StrToInt(LI.Caption));
    Self.block.SetSettings(settings);

    var signalSettings: TBlkSignalSettings;
    signalSettings.rcsOutputs := TList<TBlkSignalOutput>.Create();

    for var li: TListItem in Self.LV_RCSs.Items do
    begin
      var output: TBlkSignalOutput;
      output.rcs.Load(li.SubItems[1]);
      output.outputType := TBlkSignalOutputType(li.Data);
      output.inverseRcs.enabled := (li.SubItems[2] <> LV_RCS_ADDR_UNUSED);
      if (li.SubItems[2] <> LV_RCS_ADDR_UNUSED) then
        output.inverseRcs.addr.Load(li.SubItems[2]);
      signalSettings.rcsOutputs.Add(output);
    end;

    signalSettings.fallDelay := 0;
    signalSettings.locked := false;
    signalSettings.events := TObjectList<TBlkSignalTrainEvent>.Create();
    signalSettings.changeTime := ownConvert.SecTenthsToTime(TBlkSignal.DefaultChangeTime(Self.LV_RCSs.Items.Count > 0));
    signalSettings.PSt.enabled := False;
    signalSettings.forceDirection := TRVOptionalSite.osNo;

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

procedure TF_BlkGroupSignal.LV_RCSsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (not Self.LV_RCSs.Enabled) then
    Exit();

  Self.B_RCS_Delete.Enabled := (Self.LV_RCSs.ItemIndex <> -1);

  if (Self.LV_RCSs.Selected <> nil) then
  begin
    Self.CB_RCS_Type.ItemIndex := Integer(Self.LV_RCSs.Selected.Data);

    var rcs: TRCSsAddr;
    rcs.Load(Self.LV_RCSs.Selected.SubItems[1]);
    Self.SE_RCS_system.Value := rcs.system;
    Self.SE_RCS_module.Value := rcs.module;
    Self.SE_RCS_port.Value := rcs.port;

    if (Self.LV_RCSs.Selected.SubItems[2] <> LV_RCS_ADDR_UNUSED) then
    begin
      Self.CHB_RCS_Inverse_Output.Checked := True;
      var rcsInv: TRCSsAddr;
      rcsInv.Load(Self.LV_RCSs.Selected.SubItems[2]);
      Self.SE_RCS_inv_system.Value := rcsInv.system;
      Self.SE_RCS_inv_module.Value := rcsInv.module;
      Self.SE_RCS_inv_port.Value := rcsInv.port;
    end else begin
      Self.CHB_RCS_Inverse_Output.Checked := False;
      Self.CHB_RCS_Inverse_OutputClick(Self);
    end;
  end else begin
    Self.CB_RCS_Type.ItemIndex := -1;
    Self.SE_RCS_system.Value := 0;
    Self.SE_RCS_module.Value := 0;
    Self.SE_RCS_port.Value := 0;
    Self.CHB_RCS_Inverse_Output.Checked := False;
    Self.CHB_RCS_Inverse_OutputClick(Self);
  end;
end;

procedure TF_BlkGroupSignal.LV_RCSsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_RCS_Delete.Enabled)) then
    Self.B_RCS_DeleteClick(Self.B_RCS_Delete);
end;

procedure TF_BlkGroupSignal.LV_SignalsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_BlkDelete.Enabled := (Self.LV_Signals.ItemIndex > -1);
end;

procedure TF_BlkGroupSignal.LV_SignalsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_BlkDelete.Enabled)) then
    Self.B_BlkDeleteClick(Self);
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

procedure TF_BlkGroupSignal.B_RCS_OKClick(Sender: TObject);
begin
  if (Self.CB_RCS_Type.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var li: TListItem;
  if (Self.LV_RCSs.Selected <> nil) then
    li := Self.LV_RCSs.Selected
  else
    li := Self.LV_RCSs.Items.Add();

  Self.LV_RCSs.Enabled := False;
  li.Data := Pointer(Self.CB_RCS_Type.ItemIndex);
  li.Caption := IntToStr(li.Index);
  li.SubItems.Clear();
  li.SubItems.Add(TBlkSignal.SignalOutputTypeToStr(TBlkSignalOutputType(Self.CB_RCS_Type.ItemIndex)));
  li.SubItems.Add(IntToStr(Self.SE_RCS_system.Value) + ':' + IntToStr(Self.SE_RCS_module.Value) + ':' + IntToStr(Self.SE_RCS_port.Value));
  if (Self.CHB_RCS_Inverse_Output.Checked) then
    li.SubItems.Add(IntToStr(Self.SE_RCS_inv_system.Value) + ':' + IntToStr(Self.SE_RCS_inv_module.Value) + ':' + IntToStr(Self.SE_RCS_inv_port.Value))
  else
    li.SubItems.Add(LV_RCS_ADDR_UNUSED);
  Self.LV_RCSs.Enabled := True;
end;

end.
