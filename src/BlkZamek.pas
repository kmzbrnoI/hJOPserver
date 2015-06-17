unit BlkZamek;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Main, TBlokZamek, RPConst;

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
   CanMonitor:Boolean;
   OpenIndex:Integer;
   procedure OpenForm(BlokIndex:Integer);
   procedure NewBlkOpenForm;
   procedure NormalOpenForm;
   procedure HlavniOpenForm;
   procedure NewBlkCreate;
  end;

var
  F_BlkZamek: TF_BlkZamek;

implementation

uses GetSystems, FileSystem, TechnologieMTB, TBloky, TBlok, DataBloky;

{$R *.dfm}

procedure TF_BlkZamek.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  Self.ActiveControl := B_Save;
  HlavniOpenForm;

  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  Self.ShowModal;
 end;//procedure

procedure TF_BlkZamek.NewBlkOpenForm;
 begin
  E_Nazev.Text          := '';
  SE_ID.Value           := 0;

  Self.Caption       := 'Editovat data noveho bloku';
 end;//procedure

procedure TF_BlkZamek.NormalOpenForm;
var glob:TBlkSettings;
    i:Integer;
 begin
  glob := Self.Blk.GetGlobalSettings();

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add((Self.Blk.OblsRizeni.ORs[i]).Name);

  E_Nazev.Text          := glob.name;
  SE_ID.Value           := glob.id;

  Self.Caption := 'Editovat data bloku '+glob.name+' (zámek)';
 end;//procedure

procedure TF_BlkZamek.HlavniOpenForm;
 begin
  Self.LB_Stanice.Clear();
 end;//procedure

procedure TF_BlkZamek.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkZamek.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;//procedure

procedure TF_BlkZamek.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (SE_ID.Value = 0) then
   begin
    Application.MessageBox('ID bloku se nesmi rovnat nule !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  glob.name     := E_Nazev.Text;
  glob.id       := SE_ID.Value;
  glob.typ      := _BLK_ZAMEK;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Self.Blk := Blky.Add(_BLK_ZAMEK, glob) as TBlkZamek;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.GetGlobalSettings.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;


  Self.Close;

  Self.Blk.Change();
 end;//procedure

procedure TF_BlkZamek.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  CanMonitor := false;
  BlokyTableData.UpdateTable;
 end;//procedure

end.//unit
