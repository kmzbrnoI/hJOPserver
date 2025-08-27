unit fBlkDisconnector;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockDisconnector, Generics.Collections;

type
  TF_BlkDisconnector = class(TForm)
    E_name: TEdit;
    SE_ID: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    SE_port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_module: TSpinEdit;
    CB_outputType: TComboBox;
    Label5: TLabel;
    GB_Indications: TGroupBox;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SE_Cont_Module: TSpinEdit;
    SE_Cont_Port: TSpinEdit;
    CHB_Contoller: TCheckBox;
    CHB_Contoller_Pst: TCheckBox;
    Label7: TLabel;
    CB_mode: TComboBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_moduleExit(Sender: TObject);
    procedure CHB_ContollerClick(Sender: TObject);
    procedure SE_Cont_ModuleExit(Sender: TObject);
  private
    isNewBlock: Boolean;
    block: TBlkDisconnector;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

  public

    procedure EditBlock(BlokIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkDisconnector: TF_BlkDisconnector;

implementation

uses GetSystems, RCSc, BlockDb, Block, DataBloky, Area, RCSIFace, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkDisconnector.EditBlock(BlokIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := BlokIndex;
  Self.block := Blocks.GetBlkByIndex(BlokIndex) as TBlkDisconnector;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(BlokIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkDisconnector.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkDisconnector.SE_Cont_ModuleExit(Sender: TObject);
begin
  Self.SE_Cont_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Cont_Module.Value, Self.SE_Cont_Port.Value);
end;

procedure TF_BlkDisconnector.SE_moduleExit(Sender: TObject);
begin
  Self.SE_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkDisconnector.NewOpenForm();
begin
  Self.E_name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_module.Value := 1;
  Self.SE_port.Value := 0;
  Self.SE_moduleExit(Self);
  Self.CB_outputType.ItemIndex := 1;
  Self.CB_mode.ItemIndex := 0;

  Self.CHB_Contoller.Checked := false;
  Self.CHB_ContollerClick(Self.CHB_Contoller);

  Self.Caption := 'Nový rozpojovač';
end;

procedure TF_BlkDisconnector.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkDiscSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  if (settings.RCSAddrs.count > 0) then
  begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_module.MaxValue)) then
      Self.SE_module.MaxValue := 0;
    Self.SE_port.MaxValue := 0;

    Self.SE_module.Value := settings.RCSAddrs[0].board;
    Self.SE_port.Value := settings.RCSAddrs[0].port;
  end else begin
    Self.SE_module.Value := 0;
    Self.SE_port.Value := 0;
  end;

  Self.SE_moduleExit(Self);

  Self.E_name.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.CB_mode.ItemIndex := Integer(settings.dscMode);

  case (settings.outputType) of
    osEnabled: Self.CB_outputType.ItemIndex := 0;
    osf60: Self.CB_outputType.ItemIndex := 1;
    osf120: Self.CB_outputType.ItemIndex := 2;
    osf180: Self.CB_outputType.ItemIndex := 3;
    osf240: Self.CB_outputType.ItemIndex := 4;
    osf300: Self.CB_outputType.ItemIndex := 5;
    osf600: Self.CB_outputType.ItemIndex := 6;
    osf33: Self.CB_outputType.ItemIndex := 7;
    osf66: Self.CB_outputType.ItemIndex := 8;
  else
    Self.CB_outputType.ItemIndex := -1;
  end;

  begin
    Self.CHB_Contoller.Checked := settings.rcsController.enabled;
    Self.CHB_ContollerClick(Self.CHB_Contoller);
    if (settings.rcsController.enabled) then
    begin
      if (settings.rcsController.addr.board > Cardinal(Self.SE_Cont_Module.MaxValue)) then
        Self.SE_Cont_Module.MaxValue := 0;
      Self.SE_Cont_Port.MaxValue := 0;

      Self.SE_Cont_Module.Value := settings.rcsController.addr.board;
      Self.SE_Cont_Port.Value := settings.rcsController.addr.port;
      Self.CHB_Contoller_Pst.Checked := settings.rcsController.pstOnly;
    end;
  end;

  Self.Caption := 'Upravit blok ' + glob.name + ' (rozpojovač)';
end;

procedure TF_BlkDisconnector.CHB_ContollerClick(Sender: TObject);
begin
  Self.CHB_Contoller_Pst.Enabled := Self.CHB_Contoller.Checked;
  Self.SE_Cont_Module.Enabled := Self.CHB_Contoller.Checked;
  Self.SE_Cont_Port.Enabled := Self.CHB_Contoller.Checked;

  if (not Self.CHB_Contoller.Checked) then
  begin
    Self.CHB_Contoller_Pst.Checked := false;
    Self.SE_Cont_Module.Value := 0;
    Self.SE_Cont_Port.Value := 0;
  end;
end;

procedure TF_BlkDisconnector.CommonOpenForm;
begin
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.ActiveControl := Self.E_name;
end;

procedure TF_BlkDisconnector.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkDisconnector.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkDiscSettings;
begin
  if (Self.E_name.Text = '') then
  begin
    StrMessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_outputType.ItemIndex < 0) then
  begin
    StrMessageBox('Je třeba vybrat typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_mode.ItemIndex < 0) then
  begin
    StrMessageBox('Vyberte režim aktivace rozpojovače!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  try
    var messages := '';

    begin
      var addr := TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value);
      var another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.output);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString() + '.' + #13#10;
    end;

    glob.name := Self.E_name.Text;
    glob.id := Self.SE_ID.Value;
    glob.typ := btDisconnector;

    if (Self.isNewBlock) then
    begin
      try
        block := Blocks.Add(glob) as TBlkDisconnector;
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se přidat blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end else begin
      try
        Self.block.SetGlobalSettings(glob);
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se uložit blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end;

    settings.RCSAddrs := TList<RCSc.TRCSAddr>.Create();
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value));

    case (Self.CB_outputType.ItemIndex) of
      0: settings.outputType := osEnabled;
      1: settings.outputType := osf60;
      2: settings.outputType := osf120;
      3: settings.outputType := osf180;
      4: settings.outputType := osf240;
      5: settings.outputType := osf300;
      6: settings.outputType := osf600;
      7: settings.outputType := osf33;
      8: settings.outputType := osf66;
    else
      settings.outputType := osEnabled;
    end;

    settings.dscMode := TBlkDiscMode(Self.CB_mode.ItemIndex);

    settings.rcsController.enabled := Self.CHB_Contoller.Checked;
    if (Self.CHB_Contoller.Checked) then
    begin
      settings.rcsController.addr.board := Self.SE_Cont_Module.Value;
      settings.rcsController.addr.port := Self.SE_Cont_Port.Value;
      settings.rcsController.pstOnly := Self.CHB_Contoller_Pst.Checked;

      var another := Blocks.AnotherBlockUsesRCS(settings.rcsController.addr, Self.block, TRCSIOType.input);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.rcsController.addr.ToString() + '.' + #13#10;
    end;

    if (messages <> '') then
      StrMessageBox(messages, 'Varování', MB_OK OR MB_ICONWARNING);

    Self.block.SetSettings(settings);
    Self.block.Change();
  except
    on E: Exception do
    begin
      ExceptionMessageBox('Neočekávaná chyba.', E);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_BlkDisconnector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
