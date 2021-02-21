unit fBlkIR;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, fMain, BlockIR, Generics.Collections;

type
  TF_BlkIR = class(TForm)
    E_Nazev: TEdit;
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
    NewBlk: Boolean;
    Blk: TBlkIR;

  public
    OpenIndex: Integer;

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure HlavniOpenForm();
    procedure NewBlkCreate();
  end;

var
  F_BlkIR: TF_BlkIR;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky;

{$R *.dfm}

procedure TF_BlkIR.OpenForm(BlokIndex: Integer);
begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
  begin
    NewBlkOpenForm();
  end else begin
    NormalOpenForm();
  end;
  F_BlkIR.ShowModal();
end;

procedure TF_BlkIR.SE_moduleExit(Sender: TObject);
begin
  Self.SE_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkIR.NewBlkOpenForm;
begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_module.Value := 1;
  Self.SE_port.Value := 0;
  Self.SE_moduleExit(Self);

  F_BlkIR.Caption := 'Nový blok IR';
  F_BlkIR.ActiveControl := E_Nazev;
end;

procedure TF_BlkIR.NormalOpenForm;
var glob: TBlkSettings;
  settings: TBlkIRSettings;
begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

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

  E_Nazev.Text := glob.name;
  SE_ID.Value := glob.id;

  F_BlkIR.Caption := 'Upravit blok ' + glob.name + ' (IR)';
  F_BlkIR.ActiveControl := B_Save;
end;

procedure TF_BlkIR.HlavniOpenForm;
begin
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_BlkIR.NewBlkCreate;
begin
  NewBlk := true;
  OpenForm(Blocks.count);
end;

procedure TF_BlkIR.B_StornoClick(Sender: TObject);
begin
  F_BlkIR.Close();
end;

procedure TF_BlkIR.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkIRSettings;
  another: TBlk;
begin
  if (E_Nazev.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value), Self.Blk,
    TRCSIOType.input);
  if (another <> nil) then
  begin
    if (Application.MessageBox(PChar('RCS adresa se již používá na bloku ' + another.name + ', chcete pokračovat?'),
      'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      Exit();
  end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btIR;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkIR;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Blk.note;
    Self.Blk.SetGlobalSettings(glob);
  end;

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value));

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
end;

procedure TF_BlkIR.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

end.// unit
