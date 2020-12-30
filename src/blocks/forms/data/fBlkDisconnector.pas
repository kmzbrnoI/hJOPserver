unit fBlkDisconnector;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockDisconnector, Generics.Collections, IBUtils;

type
  TF_BlkDisconnector = class(TForm)
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
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
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

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, TOblRizeni;

{$R *.dfm}

procedure TF_BlkDisconnector.OpenForm(BlokIndex: Integer);
 begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  Self.ShowModal();
 end;

procedure TF_BlkDisconnector.SE_moduleExit(Sender: TObject);
begin
 Self.SE_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkDisconnector.NewBlkOpenForm;
 begin
  Self.E_Nazev.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  Self.SE_module.Value := 1;
  Self.SE_Port.Value := 0;
  Self.SE_moduleExit(Self);

  Self.Caption := 'Nový ropojovač';
  Self.ActiveControl := E_Nazev;
 end;

procedure TF_BlkDisconnector.NormalOpenForm;
var glob: TBlkSettings;
    settings: TBlkDiscSettings;
    oblr: TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for oblr in Self.Blk.stations do
    Self.LB_Stanice.Items.Add(oblr.Name);

  if (settings.RCSAddrs.Count > 0) then
   begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_module.MaxValue)) then
      Self.SE_module.MaxValue := 0;
    Self.SE_port.MaxValue := 0;

    Self.SE_module.Value := settings.RCSAddrs[0].board;
    Self.SE_Port.Value   := settings.RCSAddrs[0].port;
   end else begin
    Self.SE_module.Value := 0;
    Self.SE_Port.Value   := 0;
   end;

  Self.SE_moduleExit(Self);

  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok '+glob.name+' (rozpojovač)';
  Self.ActiveControl := B_Save;
 end;

procedure TF_BlkDisconnector.HlavniOpenForm;
 begin
  Self.LB_Stanice.Clear();
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkDisconnector.NewBlkCreate;
 begin
  NewBlk := true;
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
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
   begin
    Application.MessageBox('ID jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_Port.Value), Self.Blk, TRCSIOType.output);
  if (another <> nil) then
   begin
    if (Application.MessageBox(PChar('RCS adresa se již používá na bloku '+another.name+', chcete pokračovat?'),
                               'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      Exit();
   end;

  glob.name := Self.E_Nazev.Text;
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
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_Port.Value));

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkDisconnector.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
 end;

end.//unit
