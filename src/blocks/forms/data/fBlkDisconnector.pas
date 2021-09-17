unit fBlkDisconnector;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockDisconnector, Generics.Collections;

type
  TF_BlkDisconnector = class(TForm)
    E_name: TEdit;
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
    L_Usek03: TLabel;
    LB_areas: TListBox;
    CB_outputType: TComboBox;
    Label1: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_moduleExit(Sender: TObject);
  private
    NewBlk: Boolean;
    Blk: TBlkDisconnector;

  public

    OpenIndex: Integer;

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
    procedure NewBlkCreate;
  end;

var
  F_BlkDisconnector: TF_BlkDisconnector;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, Area, RCS;

{$R *.dfm}

procedure TF_BlkDisconnector.OpenForm(BlokIndex: Integer);
begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkDisconnector.SE_moduleExit(Sender: TObject);
begin
  Self.SE_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkDisconnector.NewBlkOpenForm();
begin
  Self.E_name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_module.Value := 1;
  Self.SE_port.Value := 0;
  Self.SE_moduleExit(Self);
  Self.CB_outputType.ItemIndex := 1;

  Self.Caption := 'Nový rozpojovač';
  Self.ActiveControl := Self.E_name;
end;

procedure TF_BlkDisconnector.NormalOpenForm();
var glob: TBlkSettings;
  settings: TBlkDiscSettings;
  area: TArea;
begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for area in Self.Blk.areas do
    Self.LB_areas.Items.Add(area.name);

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

  Self.Caption := 'Upravit blok ' + glob.name + ' (rozpojovač)';
  Self.ActiveControl := B_Save;
end;

procedure TF_BlkDisconnector.HlavniOpenForm;
begin
  Self.LB_areas.Clear();
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
end;

procedure TF_BlkDisconnector.NewBlkCreate;
begin
  Self.NewBlk := true;
  OpenForm(Blocks.count);
end;

procedure TF_BlkDisconnector.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkDisconnector.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkDiscSettings;
  another: TBlk;
begin
  if (Self.E_name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_outputType.ItemIndex < 0) then
  begin
    Application.MessageBox('Je třeba vybrat typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_port.Value), Self.Blk,
    TRCSIOType.output);
  if (another <> nil) then
  begin
    if (Application.MessageBox(PChar('RCS adresa se již používá na bloku ' + another.name + ', chcete pokračovat?'),
      'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      Exit();
  end;

  glob.name := Self.E_name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btDisconnector;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkDisconnector;
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

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
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

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
end;

procedure TF_BlkDisconnector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

end.// unit
