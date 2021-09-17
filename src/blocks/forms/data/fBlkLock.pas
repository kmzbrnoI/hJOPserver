unit fBlkLock;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockLock;

type
  TF_BlkLock = class(TForm)
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    Label3: TLabel;
    LB_Areas: TListBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    newBlk: Boolean;
    blk: TBlkLock;

  public
    openIndex: Integer;

    procedure OpenForm(blockIndex: Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();
    procedure NewBlkCreate();
  end;

var
  F_BlkLock: TF_BlkLock;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, Area;

{$R *.dfm}

procedure TF_BlkLock.OpenForm(blockIndex: Integer);
begin
  Self.openIndex := blockIndex;
  Blocks.GetBlkByIndex(blockIndex, TBlk(Self.Blk));
  Self.CommonOpenForm();

  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkLock.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.Caption := 'Nový blok Zámek';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkLock.NormalOpenForm();
var glob: TBlkSettings;
begin
  glob := Self.Blk.GetGlobalSettings();

  for var area in Self.Blk.areas do
    Self.LB_Areas.Items.Add(Area.name);

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok ' + glob.name + ' (zámek)';
  Self.ActiveControl := Self.B_Save;
end;

procedure TF_BlkLock.CommonOpenForm();
begin
  Self.LB_Areas.Clear();
end;

procedure TF_BlkLock.NewBlkCreate();
begin
  Self.newBlk := true;
  OpenForm(Blocks.count);
end;

procedure TF_BlkLock.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkLock.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btLock;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkLock;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.blk.note;
    Self.blk.SetGlobalSettings(glob);
  end;

  Self.Close();
  Self.blk.Change();
end;

procedure TF_BlkLock.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.newBlk := false;
  Self.openIndex := -1;
  BlokyTableData.UpdateTable();
end;

end.
