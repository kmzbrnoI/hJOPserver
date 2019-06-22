unit fBlkPrejezd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, TBlokPrejezd, TBloky, Generics.Collections;

type
  TF_BlkPrejezd = class(TForm)
    L_P02: TLabel;
    E_Prj_Nazev: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_SCom02: TLabel;
    SE_ID: TSpinEdit;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    GB_RCS: TGroupBox;
    GB_Prj_vyst: TGroupBox;
    L_P04: TLabel;
    L_P05: TLabel;
    SE_vyst_open: TSpinEdit;
    SE_vyst_close: TSpinEdit;
    GB_Prj_vst: TGroupBox;
    L_P07: TLabel;
    L_P08: TLabel;
    L_P09: TLabel;
    L_P10: TLabel;
    SE_vst_close: TSpinEdit;
    SE_vst_open: TSpinEdit;
    SE_vst_vystraha: TSpinEdit;
    SE_vst_anulace: TSpinEdit;
    L_P01: TLabel;
    SE_RCS: TSpinEdit;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   OpenIndex:Integer;
   Blk:TBlkPrejezd;
   NewBlk:Boolean;
   obls:TArstr;   //oblasti rizeni, ve kterych se SCom nachazi

    procedure NormalOpenForm;
    procedure HlavniOpenForm;
    procedure NewOpenForm;

  public

    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkPrejezd: TF_BlkPrejezd;

implementation

uses GetSystems, TechnologieRCS, fSettings, TOblsRizeni, TOblRizeni,
     TBlok, FileSystem, DataBloky;

{$R *.dfm}

procedure TF_BlkPrejezd.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
   begin
    NewOpenForm();
   end else begin
    NormalOpenForm();
   end;

  Self.ShowModal;
 end;

procedure TF_BlkPrejezd.B_save_PClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
 begin
  if (Self.E_Prj_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplòte název pøejezdu','Nelze uložit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  glob.name     := Self.E_Prj_Nazev.Text;
  glob.typ      := _BLK_PREJEZD;
  glob.id       := Self.SE_ID.Value;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_PREJEZD, glob) as TBlkPrejezd;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;


  settings.RCS := Self.SE_RCS.Value;

  settings.RCSOutputs.NOtevrit := Self.SE_vyst_open.Value;
  settings.RCSOutputs.Zavrit   := Self.SE_vyst_close.Value;

  settings.RCSInputs.Otevreno  := SE_vst_open.Value;
  settings.RCSInputs.Zavreno   := SE_vst_close.Value;
  settings.RCSInputs.Vystraha  := SE_vst_vystraha.Value;
  settings.RCSInputs.Anulace   := SE_vst_anulace.Value;

  Self.Blk.SetSettings(settings);

  Self.Close;
  Self.Blk.Change();
 end;

procedure TF_BlkPrejezd.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_BlkPrejezd.HlavniOpenForm;
 begin
  SetLength(Self.obls,0);
  Self.LB_Stanice.Clear();
 end;

procedure TF_BlkPrejezd.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
    i:Integer;
    oblr:TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls,Self.Blk.OblsRizeni.Count);
  for i := 0 to Self.Blk.OblsRizeni.Count-1 do
    obls[i] := Self.Blk.OblsRizeni[i].id;

  Self.SE_RCS.Value := settings.RCS;

  E_Prj_Nazev.Text          := glob.name;
  SE_ID.Value               := glob.id;

  SE_vyst_open.Value        := settings.RCSOutputs.NOtevrit;
  SE_vyst_close.Value       := settings.RCSOutputs.Zavrit;

  SE_vst_open.Value         := settings.RCSInputs.Otevreno;
  SE_vst_close.Value        := settings.RCSInputs.Zavreno;
  SE_vst_vystraha.Value     := settings.RCSInputs.Vystraha;
  SE_vst_anulace.Value      := settings.RCSInputs.Anulace;

  Self.Caption     := 'Pøejezd '+glob.name;
  Self.ActiveControl := Self.B_save_P;
 end;

procedure TF_BlkPrejezd.NewOpenForm;
 begin
  E_prj_Nazev.Text          := '';
  SE_ID.Value               := Blky.GetBlkID(Blky.Cnt-1)+1;
  SE_vyst_open.Value        := 0;
  SE_vyst_close.Value       := 0;
  SE_vst_open.Value         := 0;
  SE_vst_close.Value        := 0;
  SE_vst_vystraha.Value     := 0;
  SE_vst_anulace.Value      := 0;
  Self.SE_RCS.Value         := 1;

  Self.Caption := 'Nový pøejezd';
  Self.ActiveControl := Self.E_Prj_Nazev;
 end;

procedure TF_BlkPrejezd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
end;

procedure TF_BlkPrejezd.NewBlkCreate;
 begin
  Self.NewBlk := true;
  OpenForm(Blky.Cnt);
 end;

end.//unit
