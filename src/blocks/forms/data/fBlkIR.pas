unit fBlkIR;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, fMain, BlockIR, Generics.Collections;

type
  TF_BlkIR = class(TForm)
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    GB_RCS: TGroupBox;
    L_IR04: TLabel;
    L_IR05: TLabel;
    SE_port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_module: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_moduleExit(Sender: TObject);
  private
    isNewBlock: Boolean;
    block: TBlkIR;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkIR: TF_BlkIR;

implementation

uses GetSystems, TechnologieRCS, BlockDb, Block, DataBloky, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkIR.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkIR;
  if (block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkIR.SE_moduleExit(Sender: TObject);
begin
  Self.SE_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkIR.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_module.Value := 1;
  Self.SE_port.Value := 0;
  Self.SE_moduleExit(Self);

  Self.Caption := 'Nový blok IR';
end;

procedure TF_BlkIR.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkIRSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  if (settings.RCSAddr.board > Cardinal(Self.SE_module.MaxValue)) then
    Self.SE_module.MaxValue := 0;
  Self.SE_port.MaxValue := 0;
  Self.SE_module.Value := settings.RCSAddr.board;
  Self.SE_port.Value := settings.RCSAddr.port;

  Self.SE_moduleExit(Self);

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok ' + glob.name + ' (IR)';
end;

procedure TF_BlkIR.CommonOpenForm();
begin
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkIR.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkIR.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkIR.B_SaveClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value), Self.block,
    TRCSIOType.input);
  if (another <> nil) then
  begin
    if (StrMessageBox('RCS adresa se již používá na bloku ' + another.name + ', chcete pokračovat?',
      'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      Exit();
  end;

  try
    var glob: TBlkSettings;
    glob.name := Self.E_Name.Text;
    glob.id := Self.SE_ID.Value;
    glob.typ := btIR;

    if (Self.isNewBlock) then
    begin
      try
        Self.block := Blocks.Add(glob) as TBlkIR;
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

    var settings: TBlkIRSettings;
    settings.RCSAddr := TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value);

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

procedure TF_BlkIR.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
