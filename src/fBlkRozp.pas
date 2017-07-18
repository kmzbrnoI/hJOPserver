unit fBlkRozp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, TBlokRozp;

type
  TF_BlkRozp = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    GB_MTB: TGroupBox;
    L_IR04: TLabel;
    L_IR05: TLabel;
    SE_Port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_MTB: TSpinEdit;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   NewBlk:Boolean;
   Blk:TBlkRozp;

  public

   OpenIndex:Integer;

   procedure OpenForm(BlokIndex:Integer);
   procedure NewBlkOpenForm;
   procedure NormalOpenForm;
   procedure HlavniOpenForm;
   procedure NewBlkCreate;
  end;

var
  F_BlkRozp: TF_BlkRozp;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBloky, TBlok, DataBloky;

{$R *.dfm}

procedure TF_BlkRozp.OpenForm(BlokIndex:Integer);
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
  Self.ShowModal();
 end;//procedure

procedure TF_BlkRozp.NewBlkOpenForm;
 begin
  E_Nazev.Text          := '';
  SE_ID.Value           := Blky.GetBlkID(Blky.Cnt-1)+1;
  Self.SE_MTB.Value     := 1;
  Self.SE_Port.Value    := 0;

  Self.Caption       := 'Editovat data noveho bloku';
 end;//procedure

procedure TF_BlkRozp.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkRozpSettings;
    i:Integer;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add((Self.Blk.OblsRizeni.ORs[i]).Name);

  Self.SE_MTB.Value     := settings.RCSAddrs.data[0].board;
  Self.SE_Port.Value    := settings.RCSAddrs.data[0].port;

  E_Nazev.Text          := glob.name;
  SE_ID.Value           := glob.id;

  Self.Caption := 'Editovat data bloku '+glob.name+' (rozpojovaè)';
 end;//procedure

procedure TF_BlkRozp.HlavniOpenForm;
 begin
  Self.LB_Stanice.Clear();
 end;//procedure

procedure TF_BlkRozp.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkRozp.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;//procedure

procedure TF_BlkRozp.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkRozpSettings;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  glob.name     := E_Nazev.Text;
  glob.id       := SE_ID.Value;
  glob.typ      := _BLK_ROZP;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_ROZP, glob) as TBlkRozp;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.GetGlobalSettings.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  settings.RCSAddrs.Count := 1;
  settings.RCSAddrs.data[0].board := Self.SE_MTB.Value;
  settings.RCSAddrs.data[0].port  := Self.SE_Port.Value;

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
 end;//procedure

procedure TF_BlkRozp.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
 end;//procedure

end.//unit
