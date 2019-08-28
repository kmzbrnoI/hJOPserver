unit fBlkTU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, fSettings, IBUtils,
  fBlkUsekSysVars, TBloky, TBlok, TBlokTratUsek, Mask, StrUtils,
  TBlokUsek, fBlkTUZastEvent, Generics.Collections;

type
  TF_BlkTU = class(TForm)
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
    CHB_Zastavka_Lichy: TCheckBox;
    Label5: TLabel;
    E_Zast_Spr: TEdit;
    Label6: TLabel;
    SE_Zast_DelkaSpr: TSpinEdit;
    Label7: TLabel;
    ME_Zast_Delay: TMaskEdit;
    CHB_SmycBlok: TCheckBox;
    L_Usek33: TLabel;
    GB_Autoblok: TGroupBox;
    Label10: TLabel;
    CHB_NavL: TCheckBox;
    CB_NavL: TComboBox;
    Label11: TLabel;
    CHB_NavS: TCheckBox;
    CB_NavS: TComboBox;
    L_Trat3: TLabel;
    CB_Speed: TComboBox;
    PC_Zastavka: TPageControl;
    TS_Zast_lichy: TTabSheet;
    TS_Zast_sudy: TTabSheet;
    CHB_Zastavka_Sudy: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D1Click(Sender: TObject);
    procedure CHB_Zastavka_LichyClick(Sender: TObject);
    procedure CHB_NavLClick(Sender: TObject);
    procedure CHB_NavSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SE_RCS_BoardExit(Sender: TObject);
  private
   NewBlk:Boolean;
   Blk:TBlkTU;
   OpenIndex:Integer;
   CB_NavData:TArI;
   CB_NavLindex, CB_NavSindex: Integer;
   zastLichy, zastSudy: TF_BlkTUZastEvent;

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

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, ownStrUtils,
      TOblsRizeni, Booster, TOblRizeni;

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
 end;

procedure TF_BlkTU.SE_RCS_BoardExit(Sender: TObject);
begin
 Self.SE_Port1.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Board1.Value))-1, 0);
 Self.SE_Port2.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Board2.Value))-1, 0);
 Self.SE_Port3.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Board3.Value))-1, 0);
 Self.SE_Port4.MaxValue := Max(Integer(RCSi.GetModuleInputsCountSafe(Self.SE_Board4.Value))-1, 0);
end;

procedure TF_BlkTU.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;

procedure TF_BlkTU.NewBlkOpenForm;
 begin
  E_Nazev.Text               := '';
  SE_ID.Value                := Blky.GetBlkID(Blky.Cnt-1)+1;
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
  Self.SE_RCS_BoardExit(Self);

  Self.CHB_D1.Checked := true;
  Self.CHB_D1Click(Self.CHB_D1);

  Self.CHB_D2.Checked := false;
  Self.CHB_D1Click(Self.CHB_D2);

  Self.CHB_Zastavka_Lichy.Checked := false;
  Self.CHB_Zastavka_Sudy.Checked := false;
  Self.CHB_Zastavka_LichyClick(Self);

  Blky.NactiBlokyDoObjektu(Self.CB_NavL, @Self.CB_NavData, nil, nil, _BLK_NAV, -1);
  Blky.NactiBlokyDoObjektu(Self.CB_NavS, nil, nil, nil, _BLK_NAV, -1);
  Self.CB_NavLindex := -1;
  Self.CB_NavSindex := -1;

  Self.CB_Speed.ItemIndex := -1;

  Self.zastLichy.OpenEmptyForm();
  Self.zastSudy.OpenEmptyForm();

  Self.Caption := 'Editace noveho tratoveho useku';
  Self.ActiveControl := E_Nazev;
 end;

procedure TF_BlkTU.NormalOpenForm;
var glob:TBlkSettings;
    TUsettings:TBlkTUSettings;
    Usettings:TBlkUsekSettings;
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

  if (Assigned(Self.Blk)) then
   begin
    TUsettings := Self.Blk.GetSettings();
    Usettings  := TBlkUsek(Self.Blk).GetSettings();
   end;

  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;
  Self.CHB_D4.Checked := false;

  case (Usettings.RCSAddrs.Count) of
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

  if (Usettings.RCSAddrs.Count > 0) then
   begin
    Self.SE_Port1.Value  := Usettings.RCSAddrs[0].port;
    Self.SE_Board1.Value := Usettings.RCSAddrs[0].board;
   end else begin
    Self.SE_Port1.Value  := 0;
    Self.SE_Board1.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 1) then
   begin
    Self.SE_Port2.Value  := Usettings.RCSAddrs[1].port;
    Self.SE_Board2.Value := Usettings.RCSAddrs[1].board;
   end else begin
    Self.SE_Port2.Value  := 0;
    Self.SE_Board2.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 2) then
   begin
    Self.SE_Port3.Value  := Usettings.RCSAddrs[2].port;
    Self.SE_Board3.Value := Usettings.RCSAddrs[2].board;
   end else begin
    Self.SE_Port3.Value  := 0;
    Self.SE_Board3.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 3) then
   begin
    Self.SE_Port4.Value  := Usettings.RCSAddrs[3].port;
    Self.SE_Board4.Value := Usettings.RCSAddrs[3].board;
   end else begin
    Self.SE_Port4.Value  := 0;
    Self.SE_Board4.Value := 0;
   end;

  Self.SE_RCS_BoardExit(Self);

  Self.CB_Zesil.ItemIndex := -1;
  for i := 0 to Boosters.sorted.Count-1 do
   begin
    if (Boosters.sorted[i].id = Usettings.Zesil) then
     begin
      Self.CB_Zesil.ItemIndex := i;
      break;
     end;
   end;

  E_Delka.Text := FloatToStr(Usettings.Lenght);
  CHB_SmycBlok.Checked := Usettings.SmcUsek;

  Self.CHB_Zastavka_Lichy.Checked := TUsettings.Zastavka.ev_lichy.enabled;
  Self.CHB_Zastavka_Sudy.Checked  := TUsettings.Zastavka.ev_sudy.enabled;
  Self.CHB_Zastavka_LichyClick(Self);

  Blky.NactiBlokyDoObjektu(Self.CB_NavL, @Self.CB_NavData, nil, nil, _BLK_NAV, TUsettings.navLid);
  Blky.NactiBlokyDoObjektu(Self.CB_NavS, nil, nil, nil, _BLK_NAV, TUsettings.navSid);
  Self.CB_NavLindex := Self.CB_NavL.ItemIndex;
  Self.CB_NavSindex := Self.CB_NavS.ItemIndex;

  Self.CHB_NavL.Checked := (TUsettings.navLid <> -1);
  Self.CHB_NavS.Checked := (TUsettings.navSid <> -1);
  Self.CHB_NavLClick(CHB_NavL);
  Self.CHB_NavSClick(CHB_NavS);

  if (TUsettings.rychlost >= 20) then
    Self.CB_Speed.ItemIndex := (TUsettings.rychlost div 10)-2
  else
    Self.CB_Speed.ItemIndex := -1;

  Self.zastLichy.OpenForm(TUsettings.zastavka.ev_lichy);
  Self.zastSudy.OpenForm(TUsettings.zastavka.ev_sudy);

  Self.Caption := 'Editovat data bloku '+glob.name+' (tra�ov� �sek)';
  Self.ActiveControl := Self.B_OK;
 end;

procedure TF_BlkTU.HlavniOpenForm;
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

procedure TF_BlkTU.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_BlkTU.B_OKClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkUsekSettings;
    TUsettings:TBlkTUSettings;
    str:string;
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
  if (Self.CB_Speed.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte rychlost v tratovem useku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if ((Self.CHB_NavL.Checked) and (Self.CB_NavL.ItemIndex = -1)) then
   begin
    Application.MessageBox('Vyberte navestidlo kryjici usek v lichem smeru !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if ((Self.CHB_NavS.Checked) and (Self.CB_NavS.ItemIndex = -1)) then
   begin
    Application.MessageBox('Vyberte navestidlo kryjici usek v sudem smeru !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  if (CHB_Zastavka_Lichy.Checked) then
   begin
    str := Self.zastLichy.Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar('Zastavovaci udalost zastavky v lichem smeru: '+#13#10+str),'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end;

  if (CHB_Zastavka_Sudy.Checked) then
   begin
    str := Self.zastSudy.Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar('Zastavovaci udalost zastavky v sudem smeru: '+#13#10+str),'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
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
  settings.maxSpr  := 1;

  TUsettings.Zastavka.ev_lichy.enabled  := Self.CHB_Zastavka_Lichy.Checked;
  TUsettings.Zastavka.ev_sudy.enabled   := Self.CHB_Zastavka_Sudy.Checked;

  TUSettings.rychlost := (Self.CB_Speed.ItemIndex + 2) * 10;

  if (Self.CHB_NavL.Checked) then
    TUsettings.navLid := Blky.GetBlkID(Self.CB_NavData[Self.CB_NavL.ItemIndex])
  else
    TUsettings.navLid := -1;

  if (Self.CHB_NavS.Checked) then
    TUsettings.navSid := Blky.GetBlkID(Self.CB_NavData[Self.CB_NavS.ItemIndex])
  else
    TUsettings.navSid := -1;

  if ((Self.CHB_Zastavka_Lichy.Checked) or (Self.CHB_Zastavka_Sudy.Checked)) then
   begin
    TUsettings.zastavka.soupravy := TStringList.Create();
    ExtractStringsEx([','], [' '], Self.E_Zast_Spr.Text, TUsettings.Zastavka.soupravy);
    TUsettings.Zastavka.max_delka := Self.SE_Zast_DelkaSpr.Value;
    try
      TUsettings.Zastavka.delay := EncodeTime(0, StrToInt(LeftStr(Self.ME_Zast_Delay.Text, 2)), StrToInt(RightStr(Self.ME_Zast_Delay.Text, 2)), 0);
    except
      TUsettings.zastavka.soupravy.Free();
      Application.MessageBox('Nespr�vn� zadan� �as �ek�n� v zast�vce', 'Nelze ulo�it data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
   end;

  if (Self.CHB_Zastavka_Lichy.Checked) then
    TUsettings.Zastavka.ev_lichy := Self.zastLichy.GetEvent();

  if (Self.CHB_Zastavka_Sudy.Checked) then
    TUsettings.Zastavka.ev_sudy := Self.zastSudy.GetEvent();

  TUsettings.Zastavka.ev_lichy.enabled := Self.CHB_Zastavka_Lichy.Checked;
  TUsettings.Zastavka.ev_sudy.enabled  := Self.CHB_Zastavka_Sudy.Checked;

  settings.houkEvL := TBlkUsek(Self.Blk).GetSettings().houkEvL;
  settings.houkEvS := TBlkUsek(Self.Blk).GetSettings().houkEvS;

  Self.Blk.SetSettings(TUsettings);
  (Self.Blk as TBlkUsek).SetSettings(settings);

  Self.Close;
  Self.Blk.Change();
 end;

procedure TF_BlkTU.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;

procedure TF_BlkTU.FormCreate(Sender: TObject);
begin
 Self.zastLichy := TF_BlkTUZastEvent.Create(Self.TS_Zast_lichy);
 Self.zastLichy.Parent := Self.TS_Zast_lichy;
 Self.zastLichy.Show();

 Self.zastSudy := TF_BlkTUZastEvent.Create(Self.TS_Zast_lichy);
 Self.zastSudy.Parent := Self.TS_Zast_sudy;
 Self.zastSudy.Show();
end;

procedure TF_BlkTU.FormDestroy(Sender: TObject);
begin
 Self.zastLichy.Free();
 Self.zastSudy.Free();
end;

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

procedure TF_BlkTU.CHB_NavLClick(Sender: TObject);
begin
 Self.CB_NavL.Enabled := Self.CHB_NavL.Checked;
 if (not Self.CHB_NavL.Checked) then
   Self.CB_NavL.ItemIndex := -1
 else
   Self.CB_NavL.ItemIndex := Self.CB_NavLindex;
end;

procedure TF_BlkTU.CHB_NavSClick(Sender: TObject);
begin
 Self.CB_NavS.Enabled := Self.CHB_NavS.Checked;
 if (not Self.CHB_NavS.Checked) then
   Self.CB_NavS.ItemIndex := -1
 else
   Self.CB_NavS.ItemIndex := Self.CB_NavSindex;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTU.CHB_Zastavka_LichyClick(Sender: TObject);
var i:Integer;
    zast:TBlkTUZastavka;
begin
 if ((Self.CHB_Zastavka_Lichy.Checked) or (Self.CHB_Zastavka_Sudy.Checked)) then
  begin
   Self.E_Zast_Spr.Enabled       := true;
   Self.SE_Zast_DelkaSpr.Enabled := true;
   Self.ME_Zast_Delay.Enabled    := true;
   Self.PC_Zastavka.Enabled      := true;

   if (Assigned(Self.Blk)) then
    begin
     zast := Self.Blk.GetSettings.Zastavka;
     Self.E_Zast_Spr.Text := '';

     if (Assigned(zast.soupravy)) then
       for i := 0 to zast.soupravy.Count-1 do
         Self.E_Zast_Spr.Text := Self.E_Zast_Spr.Text + zast.soupravy[i] + ', ';

     Self.SE_Zast_DelkaSpr.Value     := zast.max_delka;
     Self.ME_Zast_Delay.Text         := FormatDateTime('nn:ss', zast.delay);
    end;
  end else begin
   Self.E_Zast_Spr.Enabled       := false;
   Self.SE_Zast_DelkaSpr.Enabled := false;
   Self.ME_Zast_Delay.Enabled    := false;
   Self.PC_Zastavka.Enabled      := false;
  end;

 Self.TS_Zast_lichy.TabVisible := Self.CHB_Zastavka_Lichy.Checked;
 Self.TS_Zast_sudy.TabVisible  := Self.CHB_Zastavka_Sudy.Checked;

 if (((not Self.CHB_Zastavka_Lichy.Checked) and ((not Self.CHB_Zastavka_Sudy.Checked))) or (not Assigned(Self.Blk))) then
  begin
   Self.E_Zast_Spr.Text            := '';
   Self.SE_Zast_DelkaSpr.Value     := 0;
   Self.ME_Zast_Delay.Text         := '00:00';
   Self.PC_Zastavka.Enabled        := false;
  end;
end;


end.//unit
