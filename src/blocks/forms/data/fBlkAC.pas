unit fBlkAC;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockAC;

type
  TF_BlkAC = class(TForm)
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    Label3: TLabel;
    LB_Areas: TListBox;
    Label4: TLabel;
    E_AccessToken: TEdit;
    B_GenToken: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_GenTokenClick(Sender: TObject);

  private
    newBlk: Boolean;
    blk: TBlkAC;

  public
    openIndex: Integer;

    procedure OpenForm(blockIndex: Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();
    procedure NewBlkCreate();
  end;

var
  F_BlkAC: TF_BlkAC;

implementation

uses BlockDb, Block, DataBloky, ownStrUtils;

{$R *.dfm}

procedure TF_BlkAC.OpenForm(blockIndex: Integer);
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

procedure TF_BlkAC.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.E_AccessToken.Text := '';

  Self.Caption := 'Nový blok AC';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkAC.NormalOpenForm();
var glob: TBlkSettings;
  settings: TBlkACSettings;
begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for var area in Self.Blk.areas do
    Self.LB_Areas.Items.Add(Area.name);

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.E_AccessToken.Text := settings.accessToken;

  Self.Caption := 'Upravit blok ' + glob.name + ' (AC)';
  Self.ActiveControl := Self.B_Save;
end;

procedure TF_BlkAC.CommonOpenForm();
begin
  Self.LB_Areas.Clear();
end;

procedure TF_BlkAC.NewBlkCreate();
begin
  Self.newBlk := true;
  Self.OpenForm(Blocks.count);
end;

procedure TF_BlkAC.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkAC.B_GenTokenClick(Sender: TObject);
begin
  Self.E_AccessToken.Text := RandomToken(32);
end;

procedure TF_BlkAC.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkACSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku !', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btAC;
  settings.accessToken := Self.E_AccessToken.Text;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkAC;
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

  Self.blk.SetSettings(settings);
  Self.Close();
end;

procedure TF_BlkAC.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.newBlk := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
