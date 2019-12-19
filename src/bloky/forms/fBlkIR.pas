unit fBlkIR;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, fMain, TBlokIR, Generics.Collections,
  IBUtils;

type
  TF_BlkIR = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    GB_RCS: TGroupBox;
    L_IR04: TLabel;
    L_IR05: TLabel;
    SE_port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_module: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_moduleExit(Sender: TObject);
  private
   NewBlk:Boolean;
   Blk:TBlkIR;
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
  F_BlkIR: TF_BlkIR;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBloky, TBlok, DataBloky;

{$R *.dfm}

procedure TF_BlkIR.OpenForm(BlokIndex:Integer);
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
  F_BlkIR.ShowModal;
 end;

procedure TF_BlkIR.SE_moduleExit(Sender: TObject);
begin
 Self.SE_port.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_module.Value))-1, 0);
end;

procedure TF_BlkIR.NewBlkOpenForm;
 begin
  E_Nazev.Text          := '';
  SE_ID.Value           := Blky.GetBlkID(Blky.Cnt-1)+1;
  Self.SE_module.Value  := 1;
  Self.SE_Port.Value    := 0;
  Self.SE_moduleExit(Self);

  F_BlkIR.Caption       := 'Editovat data nového bloku';
  F_BlkIR.ActiveControl := E_Nazev;
 end;

procedure TF_BlkIR.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkIRSettings;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  if (settings.RCSAddrs.Count > 0) then
   begin
    Self.SE_module.Value := settings.RCSAddrs[0].board;
    Self.SE_Port.Value := settings.RCSAddrs[0].port;
   end else begin
    Self.SE_module.Value := 0;
    Self.SE_Port.Value := 0;
   end;

  Self.SE_moduleExit(Self);

  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  F_BlkIR.Caption := 'Editovat data bloku '+glob.name+' (IR)';
  F_BlkIR.ActiveControl := B_Save;
 end;

procedure TF_BlkIR.HlavniOpenForm;
 begin
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkIR.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;

procedure TF_BlkIR.B_StornoClick(Sender: TObject);
 begin
  F_BlkIR.Close;
 end;

procedure TF_BlkIR.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkIRSettings;
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
  glob.typ      := _BLK_IR;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_IR, glob) as TBlkIR;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_Port.Value));

  Self.Blk.SetSettings(settings);

  F_BlkIR.Close;

  Self.Blk.Change();
 end;

procedure TF_BlkIR.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  CanMonitor := false;
  BlokyTableData.UpdateTable;
 end;

end.//unit
