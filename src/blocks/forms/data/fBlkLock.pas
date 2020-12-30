unit fBlkLock;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockLock;

type
  TF_BlkLock = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
   NewBlk: Boolean;
   Blk: TBlkLock;

  public
   OpenIndex: Integer;

   procedure OpenForm(BlokIndex: Integer);
   procedure NewBlkOpenForm();
   procedure NormalOpenForm();
   procedure HlavniOpenForm();
   procedure NewBlkCreate();
  end;

var
  F_BlkLock: TF_BlkLock;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, TOblRizeni;

{$R *.dfm}

procedure TF_BlkLock.OpenForm(BlokIndex: Integer);
 begin
  Self.OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.HlavniOpenForm();

  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
 end;

procedure TF_BlkLock.NewBlkOpenForm();
 begin
  Self.E_Nazev.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;

  Self.Caption := 'Nový blok Zámek';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkLock.NormalOpenForm();
var glob: TBlkSettings;
    oblr: TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();

  for oblr in Self.Blk.stations do
    Self.LB_Stanice.Items.Add(oblr.Name);

  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok '+glob.name+' (zámek)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkLock.HlavniOpenForm();
 begin
  Self.LB_Stanice.Clear();
 end;

procedure TF_BlkLock.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blocks.count);
 end;

procedure TF_BlkLock.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkLock.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
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

  glob.name := E_Nazev.Text;
  glob.id := SE_ID.Value;
  glob.typ := btLock;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blocks.Add(btLock, glob) as TBlkLock;
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

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkLock.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  Self.NewBlk := false;
  Self.OpenIndex := -1;
  BlokyTableData.UpdateTable();
 end;

end.//unit
