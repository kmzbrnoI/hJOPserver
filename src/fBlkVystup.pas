unit fBlkVystup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, fMain, TBlokVystup, RPConst;

type
  TF_BlkVystup = class(TForm)
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
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   NewBlk:Boolean;
   Blk:TBlkVystup;

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
  F_BlkVystup: TF_BlkVystup;

implementation

uses GetSystems, FileSystem, TechnologieMTB, TBloky, TBlok, DataBloky;

{$R *.dfm}

procedure TF_BlkVystup.OpenForm(BlokIndex:Integer);
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

procedure TF_BlkVystup.NewBlkOpenForm;
 begin
  E_Nazev.Text          := '';
  SE_ID.Value           := Blky.GetBlkID(Blky.Cnt-1)+1;
  Self.SE_MTB.Value     := 1;
  Self.SE_Port.Value    := 0;

  Self.Caption          := 'Editovat data noveho bloku';
 end;//procedure

procedure TF_BlkVystup.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkVystupSettings;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  Self.SE_MTB.Value     := settings.MTBAddrs.data[0].board;
  Self.SE_Port.Value    := settings.MTBAddrs.data[0].port;

  E_Nazev.Text          := glob.name;
  SE_ID.Value           := glob.id;

  Self.Caption := 'Editovat data bloku '+glob.name+' (logický výstup)';
 end;//procedure

procedure TF_BlkVystup.HlavniOpenForm;
 begin
 end;//procedure

procedure TF_BlkVystup.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkVystup.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;//procedure

procedure TF_BlkVystup.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkVystupSettings;
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
  glob.typ      := _BLK_VYSTUP;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_VYSTUP, glob) as TBlkVystup;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Blk.GetGlobalSettings.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat

  settings.MTBAddrs.Count := 1;
  settings.MTBAddrs.data[0].board := Self.SE_MTB.Value;
  settings.MTBAddrs.data[0].port  := Self.SE_Port.Value;

  Self.Blk.SetSettings(settings);

  Self.Close;

  Self.Blk.Change();
 end;//procedure

procedure TF_BlkVystup.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  CanMonitor := false;
  BlokyTableData.UpdateTable;
 end;//procedure

end.//unit
