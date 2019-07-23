unit fBlkUsek;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, fSettings,
  fBlkUsekSysVars, TBloky, TBlok, TBlokUsek, Mask, StrUtils, Generics.Collections;

type
  TF_BlkUsek = class(TForm)
    B_OK: TButton;
    B_Storno: TButton;
    L_Usek02: TLabel;
    SE_ID: TSpinEdit;
    L_Usek03: TLabel;
    E_Nazev: TEdit;
    L_Usek01: TLabel;
    GB_RCS: TGroupBox;
    L_Usek04: TLabel;
    SE_Port1: TSpinEdit;
    L_Usek15: TLabel;
    E_Delka: TEdit;
    CHB_SmycBlok: TCheckBox;
    L_Usek33: TLabel;
    LB_Stanice: TListBox;
    Label1: TLabel;
    CB_Zesil: TComboBox;
    SE_Board1: TSpinEdit;
    CHB_D1: TCheckBox;
    Label2: TLabel;
    CHB_D2: TCheckBox;
    SE_Board2: TSpinEdit;
    SE_Port2: TSpinEdit;
    Label3: TLabel;
    CHB_D3: TCheckBox;
    SE_Board3: TSpinEdit;
    SE_Port3: TSpinEdit;
    Label4: TLabel;
    CHB_D4: TCheckBox;
    SE_Board4: TSpinEdit;
    SE_Port4: TSpinEdit;
    SE_SprCnt: TSpinEdit;
    Label5: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D1Click(Sender: TObject);
    procedure SE_RCS_BoardExit(Sender: TObject);
  private
   NewBlk:Boolean;
   Blk:TBlkUsek;
   OpenIndex:Integer;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkUsek: TF_BlkUsek;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, ownStrUtils,
     Booster, TOblRizeni;

{$R *.dfm}

procedure TF_BlkUsek.OpenForm(BlokIndex:Integer);
 begin
  Self.OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  HlavniOpenForm;
  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;//else NewBlk
  F_BlkUsek.ShowModal;
 end;

procedure TF_BlkUsek.SE_RCS_BoardExit(Sender: TObject);
begin
 Self.SE_Port1.MaxValue := RCSi.GetModuleInputsCountSafe(Self.SE_Board1.Value)-1;
 Self.SE_Port2.MaxValue := RCSi.GetModuleInputsCountSafe(Self.SE_Board2.Value)-1;
 Self.SE_Port3.MaxValue := RCSi.GetModuleInputsCountSafe(Self.SE_Board3.Value)-1;
 Self.SE_Port4.MaxValue := RCSi.GetModuleInputsCountSafe(Self.SE_Board4.Value)-1;
end;

procedure TF_BlkUsek.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;

procedure TF_BlkUsek.NewBlkOpenForm;
 begin
  E_Nazev.Text               := '';
  SE_ID.Value                := Blky.GetBlkID(Blky.Cnt-1)+1;
  E_Delka.Text               := '0';
  CHB_SmycBlok.Checked       := false;
  Self.CB_Zesil.ItemIndex    := -1;
  Self.SE_SprCnt.Enabled := true;
  Self.SE_SprCnt.Value := 1;

  Self.SE_Port1.Value  := 0;
  Self.SE_Board1.Value := 1;
  Self.SE_Port2.Value  := 0;
  Self.SE_Board2.Value := 1;
  Self.SE_Port3.Value  := 0;
  Self.SE_Board3.Value := 1;
  Self.SE_Port4.Value  := 0;
  Self.SE_Board4.Value := 1;
  Self.SE_RCS_BoardExit(Self);

  Self.CHB_D1.Checked := false;
  Self.CHB_D1Click(Self.CHB_D1);

  F_BlkUsek.Caption := 'Editace noveho bloku';
  F_BlkUsek.ActiveControl := E_Nazev;
 end;

procedure TF_BlkUsek.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkUsekSettings;
    i:Integer;
    obls:TArstr;
    oblr:TOR;
 begin
  if (Assigned(Self.Blk)) then glob := Self.Blk.GetGlobalSettings();
  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls,Self.Blk.OblsRizeni.Count);
  for i := 0 to Self.Blk.OblsRizeni.Count-1 do
    obls[i] := Self.Blk.OblsRizeni[i].id;

  if (Assigned(Self.Blk)) then settings := Self.Blk.GetSettings();

  Self.SE_SprCnt.Value := settings.maxSpr;
  Self.SE_SprCnt.Enabled := Self.Blk.Stav.stanicni_kolej;

  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;
  Self.CHB_D4.Checked := false;

  case (settings.RCSAddrs.Count) of
    0: begin
      Self.CHB_D1.Checked := false;
      Self.CHB_D1Click(Self.CHB_D1);
    end;
    1: begin
      Self.CHB_D1.Checked := true;
      Self.CHB_D1Click(Self.CHB_D1);
    end;
    2: begin
      Self.CHB_D2.Checked := true;
      Self.CHB_D1Click(Self.CHB_D2);
    end;
    3: begin
      Self.CHB_D3.Checked := true;
      Self.CHB_D1Click(Self.CHB_D3);
    end;
    4: begin
      Self.CHB_D4.Checked := true;
      Self.CHB_D1Click(Self.CHB_D4);
    end;
   end;//case

  if (settings.RCSAddrs.Count > 0) then
   begin
    Self.SE_Port1.Value  := settings.RCSAddrs[0].port;
    Self.SE_Board1.Value := settings.RCSAddrs[0].board;
   end else begin
    Self.SE_Port1.Value  := 0;
    Self.SE_Board1.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 1) then
   begin
    Self.SE_Port2.Value  := settings.RCSAddrs[1].port;
    Self.SE_Board2.Value := settings.RCSAddrs[1].board;
   end else begin
    Self.SE_Port2.Value  := 0;
    Self.SE_Board2.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 2) then
   begin
    Self.SE_Port3.Value  := settings.RCSAddrs[2].port;
    Self.SE_Board3.Value := settings.RCSAddrs[2].board;
   end else begin
    Self.SE_Port3.Value  := 0;
    Self.SE_Board3.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 3) then
   begin
    Self.SE_Port4.Value  := settings.RCSAddrs[3].port;
    Self.SE_Board4.Value := settings.RCSAddrs[3].board;
   end else begin
    Self.SE_Port4.Value  := 0;
    Self.SE_Board4.Value := 0;
   end;

  Self.CB_Zesil.ItemIndex := -1;
  for i := 0 to Boosters.sorted.Count-1 do
   begin
    if (Boosters.sorted[i].id = settings.Zesil) then
     begin
      Self.CB_Zesil.ItemIndex := i;
      break;
     end;
   end;

  Self.SE_RCS_BoardExit(Self);

  E_Delka.Text := FloatToStr(settings.Lenght);
  CHB_SmycBlok.Checked := settings.SmcUsek;

  F_BlkUsek.Caption := 'Editovat data bloku '+glob.name+' (úsek)';
  F_BlkUsek.ActiveControl := B_OK;
 end;

procedure TF_BlkUsek.HlavniOpenForm;
var booster:TBooster;
 begin
  Self.LB_Stanice.Clear();

  Self.SE_Board1.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board2.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board3.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board4.MaxValue := RCSi.maxModuleAddrSafe;

  //nacteni zesilovacu
  Self.CB_Zesil.Clear();
  for booster in Boosters.sorted do Self.CB_Zesil.Items.Add(booster.name + ' (' + booster.id + ')');
 end;

procedure TF_BlkUsek.B_StornoClick(Sender: TObject);
 begin
  F_BlkUsek.Close;
 end;

procedure TF_BlkUsek.B_OKClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkUsekSettings;
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
  if (Self.CB_Zesil.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte zesilovac, kteremu patri blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (E_Delka.Text = '0') then
   begin
    Application.MessageBox('Delka useku nemuze byt nulova !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  glob.name := E_Nazev.Text;
  glob.id   := SE_ID.Value;
  glob.typ  := _BLK_USEK;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_USEK, glob) as TBlkUsek;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_D1.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board1.Value, Self.SE_Port1.Value));
  if (Self.CHB_D2.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board2.Value, Self.SE_Port2.Value));
  if (Self.CHB_D3.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board3.Value, Self.SE_Port3.Value));
  if (Self.CHB_D4.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board4.Value, Self.SE_Port4.Value));

  settings.Lenght  := StrToFloatDef(Self.E_Delka.Text,0);
  settings.SmcUsek := Self.CHB_SmycBlok.Checked;
  settings.Zesil   := Boosters.sorted[Self.CB_Zesil.ItemIndex].id;

  settings.houkEvL := Self.Blk.GetSettings().houkEvL;
  settings.houkEvS := Self.Blk.GetSettings().houkEvS;

  settings.maxSpr := Self.SE_SprCnt.Value;

  Self.Blk.SetSettings(settings);

  F_BlkUsek.Close;
  Self.Blk.Change();
 end;

procedure TF_BlkUsek.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;

procedure TF_BlkUsek.CHB_D1Click(Sender: TObject);
 begin
  case ((Sender as TCheckBox).Tag) of
   1:begin
    Self.SE_Port1.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board1.Enabled := (Sender as TCheckBox).Checked;
   end;

   2:begin
    Self.SE_Port2.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board2.Enabled := (Sender as TCheckBox).Checked;
   end;

   3:begin
    Self.SE_Port3.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board3.Enabled := (Sender as TCheckBox).Checked;
   end;

   4:begin
    Self.SE_Port4.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board4.Enabled := (Sender as TCheckBox).Checked;
   end;
  end;//case

  if ((Sender as TCheckBox).Checked) then
   begin
    // checked
    case ((Sender as TCheckBox).Tag) of
     2: Self.CHB_D1.Checked := true;
     3: Self.CHB_D2.Checked := true;
     4: Self.CHB_D3.Checked := true;
    end;
   end else begin
    //not checked
    case ((Sender as TCheckBox).Tag) of
     1: Self.CHB_D2.Checked := false;
     2: Self.CHB_D3.Checked := false;
     3: Self.CHB_D4.Checked := false;
    end;
   end;

 end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
