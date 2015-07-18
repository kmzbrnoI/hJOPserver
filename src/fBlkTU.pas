unit fBlkTU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, fSettings,
  fBlkUsekSysVars, TBloky, TBlok, TBlokTratUsek, RPConst, Mask, StrUtils,
  TBlokUsek;

type
  TF_BlkTU = class(TForm)
    B_OK: TButton;
    B_Storno: TButton;
    L_Usek02: TLabel;
    SE_ID: TSpinEdit;
    L_Usek03: TLabel;
    E_Nazev: TEdit;
    L_Usek01: TLabel;
    GB_MTB: TGroupBox;
    L_Usek04: TLabel;
    SE_Port1: TSpinEdit;
    L_Usek15: TLabel;
    E_Delka: TEdit;
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
    GB_Zastavka: TGroupBox;
    CHB_Zastavka: TCheckBox;
    Label5: TLabel;
    E_Zast_Spr: TEdit;
    Label6: TLabel;
    SE_Zast_DelkaSpr: TSpinEdit;
    Label7: TLabel;
    ME_Zast_Delay: TMaskEdit;
    Label8: TLabel;
    CB_Zast_IR_lichy: TComboBox;
    CB_Zast_IR_sudy: TComboBox;
    Label9: TLabel;
    CHB_SmycBlok: TCheckBox;
    L_Usek33: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure E_DelkaKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D1Click(Sender: TObject);
    procedure CHB_ZastavkaClick(Sender: TObject);
  private
   NewBlk:Boolean;
   Blk:TBlkTU;
   OpenIndex:Integer;
   CB_ZastIRData:TArSmallI;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkTU: TF_BlkTU;

implementation

uses GetSystems, FileSystem, TechnologieMTB, BoosterDb, DataBloky, ownStrUtils;

{$R *.dfm}

procedure TF_BlkTU.OpenForm(BlokIndex:Integer);
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
  Self.ShowModal;
 end;//procedure

procedure TF_BlkTU.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkTU.NewBlkOpenForm;
 begin
  E_Nazev.Text               := '';
  SE_ID.Value                := 0;
  E_Delka.Text               := '0';
  CHB_SmycBlok.Checked       := false;
  Self.CB_Zesil.ItemIndex    := -1;

  Self.SE_Port1.Value  := 0;
  Self.SE_Board1.Value := 1;
  Self.SE_Port2.Value  := 0;
  Self.SE_Board2.Value := 1;
  Self.SE_Port3.Value  := 0;
  Self.SE_Board3.Value := 1;
  Self.SE_Port4.Value  := 0;
  Self.SE_Board4.Value := 1;

  Self.CHB_D1.Checked := true;
  Self.CHB_D1Click(Self.CHB_D1);

  Self.CHB_D2.Checked := false;
  Self.CHB_D1Click(Self.CHB_D2);

  Self.CHB_Zastavka.Checked := false;
  Self.CHB_ZastavkaClick(Self);

  Self.Caption := 'Editace noveho tratoveho useku';
 end;//procedure

procedure TF_BlkTU.NormalOpenForm;
var glob:TBlkSettings;
    TUsettings:TBlkTUSettings;
    Usettings:TBlkUsekSettings;
    i:Integer;
    obls:TArstr;
 begin
  if (Assigned(Self.Blk)) then glob := Self.Blk.GetGlobalSettings();
  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add((Self.Blk.OblsRizeni.ORs[i]).Name);

  SetLength(obls,Self.Blk.OblsRizeni.Cnt);
  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do obls[i] := Self.Blk.OblsRizeni.ORs[i].id;

  if (Assigned(Self.Blk)) then
   begin
    TUsettings := Self.Blk.GetSettings();
    Usettings  := Self.Blk.GetUSettings();
   end;

  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;
  Self.CHB_D4.Checked := false;

  case (Usettings.MTBAddrs.Count) of
    0, 1: begin
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


  Self.SE_Port1.Value  := Usettings.MTBAddrs.data[0].port;
  Self.SE_Board1.Value := Usettings.MTBAddrs.data[0].board;

  Self.SE_Port2.Value  := Usettings.MTBAddrs.data[1].port;
  Self.SE_Board2.Value := Usettings.MTBAddrs.data[1].board;

  Self.SE_Port3.Value  := Usettings.MTBAddrs.data[2].port;
  Self.SE_Board3.Value := Usettings.MTBAddrs.data[2].board;

  Self.SE_Port4.Value  := Usettings.MTBAddrs.data[3].port;
  Self.SE_Board4.Value := Usettings.MTBAddrs.data[3].board;

  Self.CB_Zesil.ItemIndex := Usettings.Zesil;
  E_Delka.Text := FloatToStr(Usettings.Lenght);
  CHB_SmycBlok.Checked := Usettings.SmcUsek;

  Self.CHB_Zastavka.Checked := TUsettings.Zastavka.enabled;
  Self.CHB_ZastavkaClick(Self);

  Self.Caption := 'Edititace dat bloku '+glob.name+' (tratovy usek)';
 end;//procedure

procedure TF_BlkTU.HlavniOpenForm;
var i:Integer;
 begin
  Self.LB_Stanice.Clear();
  Self.ActiveControl := B_OK;

  //nacteni zesilovacu
  Self.CB_Zesil.Clear();
  for i := 0 to BoostersDb.BoosterCnt-1 do Self.CB_Zesil.Items.Add((BoostersDb.GetBooster(i)).bSettings.Name);
 end;//procedure

procedure TF_BlkTU.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;//procedure

procedure TF_BlkTU.B_OKClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkUsekSettings;
    TUsettings:TBlkTUSettings;
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

  if (CHB_Zastavka.Checked) then
   begin
    if (CB_Zast_IR_lichy.ItemIndex = -1) then
     begin
      Application.MessageBox('Vyberte lichý IR zastávky !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
    if (CB_Zast_IR_sudy.ItemIndex = -1) then
     begin
      Application.MessageBox('Vyberte sudý IR zastávky !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end;

  glob.name := E_Nazev.Text;
  glob.id   := SE_ID.Value;
  glob.typ  := _BLK_TU;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_TU, glob) as TBlkTU;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.GetGlobalSettings().poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat
  settings.MTBAddrs.data[0].board := Self.SE_Board1.Value;
  settings.MTBAddrs.data[0].port  := Self.SE_Port1.Value;

  settings.MTBAddrs.data[1].board := Self.SE_Board2.Value;
  settings.MTBAddrs.data[1].port  := Self.SE_Port2.Value;

  settings.MTBAddrs.data[2].board := Self.SE_Board3.Value;
  settings.MTBAddrs.data[2].port  := Self.SE_Port3.Value;

  settings.MTBAddrs.data[3].board := Self.SE_Board4.Value;
  settings.MTBAddrs.data[3].port  := Self.SE_Port4.Value;

  if (Self.CHB_D4.Checked) then
   settings.MTBAddrs.Count := 4
  else if (Self.CHB_D3.Checked) then
   settings.MTBAddrs.Count := 3
  else if (Self.CHB_D2.Checked) then
   settings.MTBAddrs.Count := 2
  else if (Self.CHB_D1.Checked) then
   settings.MTBAddrs.Count := 1
  else settings.MTBAddrs.Count := 0;

  settings.Lenght  := StrToFloatDef(Self.E_Delka.Text,0);
  settings.SmcUsek := Self.CHB_SmycBlok.Checked;
  settings.Zesil   := Self.CB_Zesil.ItemIndex;

  TUsettings.Zastavka.enabled  := Self.CHB_Zastavka.Checked;
  TUsettings.Zastavka.soupravy := TStringList.Create();

  if (Self.CHB_Zastavka.Checked) then
   begin
    TUsettings.Zastavka.IR_lichy  := Blky.GetBlkID(Self.CB_ZastIRData[Self.CB_Zast_IR_lichy.ItemIndex]);
    TUsettings.Zastavka.IR_sudy   := Blky.GetBlkID(Self.CB_ZastIRData[Self.CB_Zast_IR_sudy.ItemIndex]);
    ExtractStringsEx([','], [' '], Self.E_Zast_Spr.Text, TUsettings.Zastavka.soupravy);
    TUsettings.Zastavka.max_delka := Self.SE_Zast_DelkaSpr.Value;
    try
      TUsettings.Zastavka.delay := EncodeTime(0, StrToInt(LeftStr(Self.ME_Zast_Delay.Text, 2)), StrToInt(RightStr(Self.ME_Zast_Delay.Text, 2)), 0);
    except
      Application.MessageBox('Nesprávnì zadaný èas èekání v zastávce', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
   end else begin
    TUsettings.Zastavka.IR_lichy := -1;
    TUsettings.Zastavka.IR_sudy  := -1;
   end;

  Self.Blk.SetSettings(TUsettings);
  Self.Blk.SetUSettings(settings);

  Self.Close;
  Self.Blk.Change();
 end;//procedure

procedure TF_BlkTU.E_DelkaKeyPress(Sender: TObject; var Key: Char);
 begin
  Key := Key;
  case Key of
   '0'..'9',#9,#8:begin
                  end else begin
                   Key := #0;
                  end;
   end;//case
 end;//procedure

procedure TF_BlkTU.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;//procedure

procedure TF_BlkTU.CHB_D1Click(Sender: TObject);
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

procedure TF_BlkTU.CHB_ZastavkaClick(Sender: TObject);
var i:Integer;
    zast:TBlkTUZastavka;
begin
 if (Self.CHB_Zastavka.Checked) then
  begin
   Self.E_Zast_Spr.Enabled       := true;
   Self.SE_Zast_DelkaSpr.Enabled := true;
   Self.ME_Zast_Delay.Enabled    := true;
   Self.CB_Zast_IR_lichy.Enabled := true;
   Self.CB_Zast_IR_sudy.Enabled  := true;

   if (Assigned(Self.Blk)) then
    begin
     zast := Self.Blk.GetSettings.Zastavka;
     Self.E_Zast_Spr.Text := '';

     if (Assigned(zast.soupravy)) then
       for i := 0 to zast.soupravy.Count-1 do
         Self.E_Zast_Spr.Text := Self.E_Zast_Spr.Text + zast.soupravy[i] + ', ';

     Self.SE_Zast_DelkaSpr.Value     := zast.max_delka;
     Self.ME_Zast_Delay.Text         := FormatDateTime('nn:ss', zast.delay);

     Blky.NactiBlokyDoObjektu(Self.CB_Zast_IR_lichy, nil, nil, nil, _BLK_IR, zast.IR_lichy);
     Blky.NactiBlokyDoObjektu(Self.CB_Zast_IR_sudy , @CB_ZastIRData, nil, nil, _BLK_IR, zast.IR_sudy);
    end;
  end else begin
   Self.E_Zast_Spr.Enabled       := false;
   Self.SE_Zast_DelkaSpr.Enabled := false;
   Self.ME_Zast_Delay.Enabled    := false;
   Self.CB_Zast_IR_lichy.Enabled := false;
   Self.CB_Zast_IR_sudy.Enabled  := false;
  end;

 if ((not Self.CHB_Zastavka.Checked) or (not Assigned(Self.Blk))) then
  begin
   Self.E_Zast_Spr.Text            := '';
   Self.SE_Zast_DelkaSpr.Value     := 0;
   Self.ME_Zast_Delay.Text         := '00:00';
   Self.CB_Zast_IR_lichy.ItemIndex := -1;
   Self.CB_Zast_IR_sudy.ItemIndex  := -1;
  end;
end;//procedure

end.//unit
