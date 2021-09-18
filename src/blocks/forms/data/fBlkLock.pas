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
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    isNewBlock: Boolean;
    block: TBlkLock;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditForm();
    procedure NewOpenForm();

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkLock: TF_BlkLock;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, Area;

{$R *.dfm}

procedure TF_BlkLock.EditBlock(blockIndex: Integer);
begin
  Self.openIndex := blockIndex;
  Blocks.GetBlkByIndex(blockIndex, TBlk(Self.block));
  Self.CommonOpenForm();

  if (isNewBlock) then
    Self.NewOpenForm()
  else
    Self.EditForm();

  Self.ShowModal();
end;

procedure TF_BlkLock.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.Caption := 'Nový blok Zámek';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkLock.EditForm();
var glob: TBlkSettings;
begin
  glob := Self.block.GetGlobalSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok ' + glob.name + ' (zámek)';
  Self.ActiveControl := Self.B_Save;
end;

procedure TF_BlkLock.CommonOpenForm();
begin

end;

procedure TF_BlkLock.NewBlock();
begin
  Self.isNewBlock := true;
  Self.EditBlock(Blocks.count);
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

  if (Self.isNewBlock) then
  begin
    try
      block := Blocks.Add(glob) as TBlkLock;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.block.SetGlobalSettings(glob);
  end;

  Self.Close();
  Self.block.Change();
end;

procedure TF_BlkLock.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
