unit fBlkPrejezd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, TBlokPrejezd, RPConst;

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
    GB_MTB: TGroupBox;
    GB_Prj_vyst: TGroupBox;
    L_P04: TLabel;
    L_P05: TLabel;
    L_P06: TLabel;
    SE_vyst_open: TSpinEdit;
    SE_vyst_close: TSpinEdit;
    SE_vyst_vyluka: TSpinEdit;
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
    SE_MTB: TSpinEdit;
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

uses GetSystems, TechnologieMTB, fSettings, TOblsRizeni, TOblRizeni,
      TBloky, TBlok, FileSystem, DataBloky;

{$R *.dfm}

procedure TF_BlkPrejezd.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.ActiveControl := Self.E_Prj_Nazev;
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
    glob.poznamka := Self.Blk.GetGlobalSettings().poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;


  settings.MTB := Self.SE_MTB.Value;

  settings.MTBOutputs.NOtevrit := Self.SE_vyst_open.Value;
  settings.MTBOutputs.Zavrit   := Self.SE_vyst_close.Value;
  settings.MTBOutputs.Vyluka   := Self.SE_vyst_vyluka.Value;

  settings.MTBInputs.Otevreno  := SE_vst_open.Value;
  settings.MTBInputs.Zavreno   := SE_vst_close.Value;
  settings.MTBInputs.Vystraha  := SE_vst_vystraha.Value;
  settings.MTBInputs.Anulace   := SE_vst_anulace.Value;

  Self.Blk.SetSettings(settings);

  Self.Close;
  Self.Blk.Change();
 end;

procedure TF_BlkPrejezd.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;//procedure

procedure TF_BlkPrejezd.HlavniOpenForm;
 begin
  SetLength(Self.obls,0);
  Self.LB_Stanice.Clear();
 end;//procedure

procedure TF_BlkPrejezd.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
    i:Integer;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add(Self.Blk.OblsRizeni.ORs[i].Name);

  SetLength(obls,Self.Blk.OblsRizeni.Cnt);
  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do obls[i] := Self.Blk.OblsRizeni.ORs[i].id;

  Self.SE_MTB.Value := settings.MTB;

  E_Prj_Nazev.Text          := glob.name;
  SE_ID.Value               := glob.id;

  SE_vyst_open.Value        := settings.MTBOutputs.NOtevrit;
  SE_vyst_close.Value       := settings.MTBOutputs.Zavrit;
  SE_vyst_vyluka.Value      := settings.MTBOutputs.Vyluka;

  SE_vst_open.Value         := settings.MTBInputs.Otevreno;
  SE_vst_close.Value        := settings.MTBInputs.Zavreno;
  SE_vst_vystraha.Value     := settings.MTBInputs.Vystraha;
  SE_vst_anulace.Value      := settings.MTBInputs.Anulace;

  Self.Caption     := 'Pøejezd '+glob.name;
 end;//procedure

procedure TF_BlkPrejezd.NewOpenForm;
 begin
  E_prj_Nazev.Text          := '';
  SE_ID.Value               := Blky.GetBlkID(Blky.Cnt-1)+1;
  SE_vyst_open.Value        := 0;
  SE_vyst_close.Value       := 0;
  SE_vyst_vyluka.Value      := 0;
  SE_vst_open.Value         := 0;
  SE_vst_close.Value        := 0;
  SE_vst_vystraha.Value     := 0;
  SE_vst_anulace.Value      := 0;
  Self.SE_MTB.Value         := 1;

  Self.Caption := 'Nový pøejezd';
 end;//procedure

procedure TF_BlkPrejezd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
end;//procedure

procedure TF_BlkPrejezd.NewBlkCreate;
 begin
  Self.NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

end.//unit
