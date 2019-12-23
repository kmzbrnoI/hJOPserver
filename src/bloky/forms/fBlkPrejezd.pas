unit fBlkPrejezd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, TBlokPrejezd, TBloky, Generics.Collections,
  IBUtils;

type
  TF_BlkPrejezd = class(TForm)
    L_Name: TLabel;
    E_Prj_Nazev: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_ID: TLabel;
    SE_ID: TSpinEdit;
    L_Station: TLabel;
    LB_Stanice: TListBox;
    GB_RCS: TGroupBox;
    GB_Prj_vyst: TGroupBox;
    L_P04: TLabel;
    L_P05: TLabel;
    SE_vyst_open_port: TSpinEdit;
    SE_vyst_close_port: TSpinEdit;
    GB_Prj_vst: TGroupBox;
    L_P07: TLabel;
    L_P08: TLabel;
    L_P09: TLabel;
    L_P10: TLabel;
    SE_vst_close_port: TSpinEdit;
    SE_vst_open_port: TSpinEdit;
    SE_vst_vystraha_port: TSpinEdit;
    SE_vst_anulace_port: TSpinEdit;
    L_P01: TLabel;
    SE_vyst_open_board: TSpinEdit;
    SE_vyst_close_board: TSpinEdit;
    Label1: TLabel;
    SE_vst_close_board: TSpinEdit;
    SE_vst_open_board: TSpinEdit;
    SE_vst_vystraha_board: TSpinEdit;
    SE_vst_anulace_board: TSpinEdit;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RCS_boardExit(Sender: TObject);
  private
   OpenIndex:Integer;
   Blk:TBlkPrejezd;
   NewBlk:Boolean;
   obls:TArstr;

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

procedure TF_BlkPrejezd.SE_RCS_boardExit(Sender: TObject);
begin
 Self.SE_vyst_open_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vyst_open_board.Value))-1, 0);
 Self.SE_vyst_close_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vyst_close_board.Value))-1, 0);

 Self.SE_vst_close_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vst_close_board.Value))-1, 0);
 Self.SE_vst_open_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vst_open_board.Value))-1, 0);
 Self.SE_vst_vystraha_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vst_vystraha_board.Value))-1, 0);
 Self.SE_vst_anulace_port.MaxValue := Max(Integer(RCSi.GetModuleOutputsCountSafe(Self.SE_vst_anulace_board.Value))-1, 0);
end;

procedure TF_BlkPrejezd.B_save_PClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkPrjSettings;
 begin
  if (Self.E_Prj_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název přejezdu', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  glob.name := Self.E_Prj_Nazev.Text;
  glob.typ := _BLK_PREJEZD;
  glob.id  := Self.SE_ID.Value;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_PREJEZD, glob) as TBlkPrejezd;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodařilo se přidat blok!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end else begin
    glob.poznamka := Self.Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  settings.RCSOutputs.NOtevrit.board := Self.SE_vyst_open_board.Value;
  settings.RCSOutputs.NOtevrit.port := Self.SE_vyst_open_port.Value;

  settings.RCSOutputs.Zavrit.board := Self.SE_vyst_close_board.Value;
  settings.RCSOutputs.Zavrit.port := Self.SE_vyst_close_port.Value;

  settings.RCSInputs.Otevreno.board := SE_vst_open_board.Value;
  settings.RCSInputs.Otevreno.port := SE_vst_open_port.Value;

  settings.RCSInputs.Zavreno.board := SE_vst_close_board.Value;
  settings.RCSInputs.Zavreno.port := SE_vst_close_port.Value;

  settings.RCSInputs.Vystraha.board := SE_vst_vystraha_board.Value;
  settings.RCSInputs.Vystraha.port := SE_vst_vystraha_port.Value;

  settings.RCSInputs.Anulace.board := SE_vst_anulace_board.Value;
  settings.RCSInputs.Anulace.port := SE_vst_anulace_port.Value;

  Self.Blk.SetSettings(settings);

  Self.Close();
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

  Self.SE_vyst_open_board.MaxValue := RCSi.maxModuleAddr;
  Self.SE_vyst_close_board.MaxValue := RCSi.maxModuleAddr;
  Self.SE_vst_close_board.MaxValue := RCSi.maxModuleAddr;
  Self.SE_vst_open_board.MaxValue := RCSi.maxModuleAddr;
  Self.SE_vst_vystraha_board.MaxValue := RCSi.maxModuleAddr;
  Self.SE_vst_anulace_board.MaxValue := RCSi.maxModuleAddr;
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

  E_Prj_Nazev.Text := glob.name;
  SE_ID.Value := glob.id;

  SE_vyst_open_board.Value := settings.RCSOutputs.NOtevrit.board;
  SE_vyst_open_port.Value := settings.RCSOutputs.NOtevrit.port;

  SE_vyst_close_board.Value := settings.RCSOutputs.Zavrit.board;
  SE_vyst_close_port.Value := settings.RCSOutputs.Zavrit.port;

  SE_vst_open_board.Value := settings.RCSInputs.Otevreno.board;
  SE_vst_open_port.Value := settings.RCSInputs.Otevreno.port;

  SE_vst_close_board.Value := settings.RCSInputs.Zavreno.board;
  SE_vst_close_port.Value := settings.RCSInputs.Zavreno.port;

  SE_vst_vystraha_board.Value := settings.RCSInputs.Vystraha.board;
  SE_vst_vystraha_port.Value := settings.RCSInputs.Vystraha.port;

  SE_vst_anulace_board.Value  := settings.RCSInputs.Anulace.board;
  SE_vst_anulace_port.Value  := settings.RCSInputs.Anulace.port;

  Self.SE_RCS_boardExit(Self);

  Self.Caption := 'Přejezd '+glob.name;
  Self.ActiveControl := Self.B_save_P;
 end;

procedure TF_BlkPrejezd.NewOpenForm;
 begin
  E_prj_Nazev.Text := '';
  SE_ID.Value := Blky.GetBlkID(Blky.count-1)+1;
  SE_vyst_open_board.Value  := 0;
  SE_vyst_open_port.Value  := 0;
  SE_vyst_close_board.Value := 0;
  SE_vyst_close_port.Value := 0;
  SE_vst_open_board.Value := 0;
  SE_vst_open_port.Value := 0;
  SE_vst_close_board.Value := 0;
  SE_vst_close_port.Value := 0;
  SE_vst_vystraha_board.Value := 0;
  SE_vst_vystraha_port.Value := 0;
  SE_vst_anulace_board.Value := 0;
  SE_vst_anulace_port.Value := 0;
  Self.SE_RCS_boardExit(Self);

  Self.Caption := 'Nový přejezd';
  Self.ActiveControl := Self.E_Prj_Nazev;
 end;

procedure TF_BlkPrejezd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

procedure TF_BlkPrejezd.NewBlkCreate;
 begin
  Self.NewBlk := true;
  OpenForm(Blky.count);
 end;

end.//unit
