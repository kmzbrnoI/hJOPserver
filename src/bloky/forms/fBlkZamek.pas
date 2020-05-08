unit fBlkZamek;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, TBlokZamek;

type
  TF_BlkZamek = class(TForm)
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
   NewBlk:Boolean;
   Blk:TBlkZamek;

  public
   OpenIndex:Integer;

   procedure OpenForm(BlokIndex:Integer);
   procedure NewBlkOpenForm();
   procedure NormalOpenForm();
   procedure HlavniOpenForm();
   procedure NewBlkCreate;
  end;

var
  F_BlkZamek: TF_BlkZamek;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBloky, TBlok, DataBloky, TOblRizeni;

{$R *.dfm}

procedure TF_BlkZamek.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  Self.ShowModal;
 end;

procedure TF_BlkZamek.NewBlkOpenForm;
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blky.GetBlkID(Blky.count-1)+1;

  Self.Caption := 'Editovat data nového bloku';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkZamek.NormalOpenForm;
var glob:TBlkSettings;
    oblr:TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  E_Nazev.Text          := glob.name;
  SE_ID.Value           := glob.id;

  Self.Caption := 'Editovat data bloku '+glob.name+' (zámek)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkZamek.HlavniOpenForm();
 begin
  Self.LB_Stanice.Clear();
 end;

procedure TF_BlkZamek.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blky.count);
 end;

procedure TF_BlkZamek.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkZamek.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  glob.name := E_Nazev.Text;
  glob.id := SE_ID.Value;
  glob.typ := _BLK_ZAMEK;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blky.Add(_BLK_ZAMEK, glob) as TBlkZamek;
    except
      on E:Exception do
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

procedure TF_BlkZamek.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
 end;

end.//unit
