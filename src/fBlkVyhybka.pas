unit fBlkVyhybka;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, StdCtrls, ExtCtrls, fMain, fBlkUsek, TBlokVyhybka,
  TBloky;

type
  TF_BlkVyhybka = class(TForm)
    L_Vyh01: TLabel;
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_Vyh02: TLabel;
    GB_MTB: TGroupBox;
    L_Vyh05: TLabel;
    L_Vyh06: TLabel;
    SE_VystPlusPort: TSpinEdit;
    SE_VystMinusPort: TSpinEdit;
    L_Vyh07: TLabel;
    L_Vyh08: TLabel;
    SE_VstPlusPort: TSpinEdit;
    SE_VstMinusPort: TSpinEdit;
    L_Vyh09: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    CHB_Spojka: TCheckBox;
    CB_Spojka: TComboBox;
    SE_VystPlusMTB: TSpinEdit;
    SE_VystMinusMTB: TSpinEdit;
    SE_VstPlusMTB: TSpinEdit;
    SE_VstMinusMTB: TSpinEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    CB_Zamek: TComboBox;
    CHB_Zamek: TCheckBox;
    Label2: TLabel;
    CB_Zamek_Poloha: TComboBox;
    GB_Neprofil: TGroupBox;
    CHB_npPlus: TCheckBox;
    CB_npPlus: TComboBox;
    CHB_npMinus: TCheckBox;
    CB_npMinus: TComboBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_SpojkaClick(Sender: TObject);
    procedure CHB_ZamekClick(Sender: TObject);
    procedure CB_SpojkaChange(Sender: TObject);
    procedure CHB_npPlusClick(Sender: TObject);
    procedure CHB_npMinusClick(Sender: TObject);
  private
   OpenIndex:Integer;
   Blk:TBlkVyhybka;
   NewBlk:Boolean;
   CB_SpojkaData:TArI;
   CB_ZamekData:TArI;
   CB_NeprofilData:TArI;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkVyhybka: TF_BlkVyhybka;

implementation

uses Prevody, GetSystems, FileSystem, TechnologieRCS, TBlok, fBlkVyhybkaSysVars,
    DataBloky;

{$R *.dfm}

procedure TF_BlkVyhybka.OpenForm(BlokIndex:Integer);
 begin
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  OpenIndex := BlokIndex;
  F_BlkVyhybka.ActiveControl := B_Save;

  HlavniOpenForm;
  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  F_BlkVyhybka.ShowModal;
 end;//procedure

procedure TF_BlkVyhybka.NewBlkOpenForm;
 begin
  E_Nazev.Text := '';
  SE_ID.Value  := Blky.GetBlkID(Blky.Cnt-1)+1;

  SE_VystPlusPort.Value  := 0;
  SE_VystPlusMTB.Value   := 1;
  SE_VystMinusPort.Value := 0;
  SE_VystMinusMTB.Value  := 1;
  SE_VstPlusPort.Value   := 0;
  SE_VstPlusMTB.Value    := 1;
  SE_VstMinusPort.Value  := 0;
  SE_VstMinusMTB.Value   := 1;

  Self.CHB_Spojka.Checked  := false;
  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Self.CHB_Zamek.Checked  := false;
  Self.CHB_ZamekClick(Self.CHB_Spojka);

  Self.CHB_npPlus.Checked := false;
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := false;
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  F_BlkVyhybka.Caption := 'Editovat data nového bloku výhybka';
 end;//procedure

procedure TF_BlkVyhybka.NormalOpenForm;
var glob:TBlkSettings;
    i:Integer;
    settings:TBlkVyhSettings;
    obls:TArStr;
 begin
  glob := Self.Blk.GetGlobalSettings();

  E_Nazev.Text  := glob.name;
  SE_ID.Value   := glob.id;

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add(Self.Blk.OblsRizeni.ORs[i].Name);

  settings := Blk.GetSettings();

  if (settings.spojka > -1) then
    Self.CHB_Spojka.Checked := true
   else
    Self.CHB_Spojka.Checked := false;

  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Self.CHB_Zamek.Checked := (settings.zamek > -1);
  if (settings.zamek > -1) then
    Self.CB_Zamek_Poloha.ItemIndex := Integer(settings.zamekPoloha);
  Self.CHB_ZamekClick(Self.CHB_Zamek);

  //poradi(0..3): vst+,vst-,vyst+,vyst- (referencni MTB_board = [0])
  SE_VstPlusMTB.Value    := settings.RCSAddrs.data[0].board;
  SE_VstPlusPort.Value   := settings.RCSAddrs.data[0].port;

  SE_VstMinusMTB.Value   := settings.RCSAddrs.data[1].board;
  SE_VstMinusPort.Value  := settings.RCSAddrs.data[1].port;

  SE_VystPlusMTB.Value   := settings.RCSAddrs.data[2].board;
  SE_VystPlusPort.Value  := settings.RCSAddrs.data[2].port;

  SE_VystMinusMTB.Value  := settings.RCSAddrs.data[3].board;
  SE_VystMinusPort.Value := settings.RCSAddrs.data[3].port;

  Self.CHB_npPlus.Checked := (settings.npPlus > -1);
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := (settings.npMinus > -1);
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  SetLength(obls,Self.Blk.OblsRizeni.Cnt);
  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do obls[i] := Self.Blk.OblsRizeni.ORs[i].id;

  F_BlkVyhybka.Caption := 'Editovat data bloku : '+glob.name+' (výhybka)';
 end;//procedure

procedure TF_BlkVyhybka.HlavniOpenForm;
var spojka_vypust:TArI;
    obls:TArStr;
    i:Integer;
 begin
  Self.LB_Stanice.Clear();

  if (Self.Blk <> nil) then
   begin
    SetLength(obls,Self.Blk.OblsRizeni.Cnt);
    for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do obls[i] := Self.Blk.OblsRizeni.ORs[i].id;
    SetLength(spojka_vypust, 1);
    spojka_vypust[0] := Self.Blk.id;

    // spojka
    Blky.NactiBlokyDoObjektu(Self.CB_Spojka, @Self.CB_SpojkaData, @spojka_vypust, obls, _BLK_VYH, Self.Blk.GetSettings().spojka);
    Self.CHB_Spojka.Enabled := (Length(Self.CB_SpojkaData) > 0) or (Self.Blk.GetSettings.spojka > -1);

    // zamek
    Blky.NactiBlokyDoObjektu(Self.CB_Zamek, @Self.CB_ZamekData, nil, obls, _BLK_ZAMEK, Self.Blk.GetSettings().zamek);
    Self.CHB_Zamek.Enabled := (Length(Self.CB_ZamekData) > 0) or (Self.Blk.GetSettings.zamek > -1);

    // neprofilove styky +
    Blky.NactiBlokyDoObjektu(Self.CB_npPlus, @Self.CB_NeprofilData, nil, obls, _BLK_USEK, Self.Blk.GetSettings().npPlus, _BLK_TU);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npPlus > -1);

    // neprofilove styky -
    Blky.NactiBlokyDoObjektu(Self.CB_npMinus, @Self.CB_NeprofilData, nil, obls, _BLK_USEK, Self.Blk.GetSettings().npMinus, _BLK_TU);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npMinus > -1);

   end else begin
    Blky.NactiBlokyDoObjektu(Self.CB_Spojka, @Self.CB_SpojkaData, nil, nil, _BLK_VYH, -1);
    Self.CHB_Spojka.Enabled := (Length(Self.CB_SpojkaData) > 0);

    Blky.NactiBlokyDoObjektu(Self.CB_Zamek, @Self.CB_ZamekData, nil, nil, _BLK_ZAMEK, -1);
    Self.CHB_Zamek.Enabled := (Length(Self.CB_ZamekData) > 0);

    Blky.NactiBlokyDoObjektu(Self.CB_npPlus, @Self.CB_NeprofilData, nil, nil, _BLK_USEK, -1, _BLK_TU);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0);

    Blky.NactiBlokyDoObjektu(Self.CB_npMinus, @Self.CB_NeprofilData, nil, nil, _BLK_USEK, -1, _BLK_TU);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0);
   end;

 end;//procedure

procedure TF_BlkVyhybka.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkVyhybka.B_StornoClick(Sender: TObject);
 begin
  F_BlkVyhybka.Close;
 end;

procedure TF_BlkVyhybka.CB_SpojkaChange(Sender: TObject);
var Blk:TBlk;
    mtbs:TRCSAddrs;
begin
 if ((not Self.CHB_Spojka.Checked) or (Self.CB_Spojka.ItemIndex = -1)) then Exit();

 Blky.GetBlkByIndex(Self.CB_SpojkaData[Self.CB_Spojka.ItemIndex], Blk);
 if ((Blk = nil) or (Blk.GetGlobalSettings.typ <> _BLK_VYH)) then Exit();

 mtbs := TBlkVyhybka(Blk).GetSettings().RCSAddrs;

 SE_VstPlusMTB.Value    := mtbs.data[0].board;
 SE_VstPlusPort.Value   := mtbs.data[0].port;

 SE_VstMinusMTB.Value   := mtbs.data[1].board;
 SE_VstMinusPort.Value  := mtbs.data[1].port;

 SE_VystPlusMTB.Value   := mtbs.data[2].board;
 SE_VystPlusPort.Value  := mtbs.data[2].port;

 SE_VystMinusMTB.Value  := mtbs.data[3].board;
 SE_VystMinusPort.Value := mtbs.data[3].port;
end;

procedure TF_BlkVyhybka.CHB_npMinusClick(Sender: TObject);
begin
 Self.CB_npMinus.Enabled := Self.CHB_npMinus.Checked;
 if (not Self.CHB_npMinus.Checked) then
   Self.CB_npMinus.ItemIndex := -1;
end;

procedure TF_BlkVyhybka.CHB_npPlusClick(Sender: TObject);
begin
 Self.CB_npPlus.Enabled := Self.CHB_npPlus.Checked;
 if (not Self.CHB_npPlus.Checked) then
   Self.CB_npPlus.ItemIndex := -1;
end;

procedure TF_BlkVyhybka.CHB_SpojkaClick(Sender: TObject);
begin
 Self.CB_Spojka.Enabled := (Sender as TCheckBox).Checked;
 if (not (Sender as TCheckBox).Checked) then
   Self.CB_Spojka.ItemIndex := -1;
end;

procedure TF_BlkVyhybka.CHB_ZamekClick(Sender: TObject);
begin
 Self.CB_Zamek.Enabled := (Sender as TCheckBox).Checked;
 Self.CB_Zamek_Poloha.Enabled := (Sender as TCheckBox).Checked;
 if (not (Sender as TCheckBox).Checked) then
  begin
   Self.CB_Zamek.ItemIndex        := -1;
   Self.CB_Zamek_Poloha.ItemIndex := -1;
  end;
end;

procedure TF_BlkVyhybka.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkVyhSettings;
    return:Byte;
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
  if ((Self.CHB_Spojka.Checked) and (Self.CB_Spojka.ItemIndex < 0)) then
   begin
    Application.MessageBox('Vyberte spojku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_Zamek.Checked) then
   begin
    if (Self.CB_Zamek.ItemIndex < 0) then
     begin
      Application.MessageBox('Vyberte zámek !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
    if (Self.CB_Zamek_Poloha.ItemIndex < 0) then
     begin
      Application.MessageBox('Vyberte polohu výhybky pro uzamèení zámku!','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end;
  if ((Self.CHB_npPlus.Checked) and (Self.CB_npPlus.ItemIndex < 0)) then
   begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu plus!',
      'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if ((Self.CHB_npMinus.Checked) and (Self.CB_npMinus.ItemIndex < 0)) then
   begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu mínus!',
      'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;


  glob.name     := E_Nazev.Text;
  glob.id       := SE_ID.Value;
  glob.typ      := _BLK_VYH;

  if (NewBlk) then
   begin
    Blk := Blky.Add(_BLK_VYH, glob) as TBlkVyhybka;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat

  settings.RCSAddrs.Count := 4;

  settings.RCSAddrs.data[0].board := SE_VstPlusMTB.Value;
  settings.RCSAddrs.data[0].port  := SE_VstPlusPort.Value;

  settings.RCSAddrs.data[1].board := SE_VstMinusMTB.Value;
  settings.RCSAddrs.data[1].port  := SE_VstMinusPort.Value;

  settings.RCSAddrs.data[2].board := SE_VystPlusMTB.Value;
  settings.RCSAddrs.data[2].port  := SE_VystPlusPort.Value;

  settings.RCSAddrs.data[3].board := SE_VystMinusMTB.Value;
  settings.RCSAddrs.data[3].port  := SE_VystMinusPort.Value;

  if (Self.CHB_Spojka.Checked) then
   settings.spojka := Blky.GetBlkID(Self.CB_SpojkaData[Self.CB_Spojka.ItemIndex])
  else
   settings.spojka := -1;

  if (Self.CHB_Zamek.Checked) then
   begin
     settings.zamek := Blky.GetBlkID(Self.CB_ZamekData[Self.CB_Zamek.ItemIndex]);
     settings.zamekPoloha := TVyhPoloha(Self.CB_Zamek_Poloha.ItemIndex);
   end else begin
     settings.zamek := -1;
     settings.zamekPoloha := TVyhPoloha.none;
   end;

  if (Self.CHB_npPlus.Checked) then
   settings.npPlus := Blky.GetBlkID(Self.CB_NeprofilData[Self.CB_npPlus.ItemIndex])
  else
   settings.npPlus := -1;

  if (Self.CHB_npMinus.Checked) then
   settings.npMinus := Blky.GetBlkID(Self.CB_NeprofilData[Self.CB_npMinus.ItemIndex])
  else
   settings.npMinus := -1;

  return := Self.Blk.SetSettings(settings);

  if (return = 2) then
   begin
    Application.MessageBox(PChar('Výhybka, se kterou se chcete spojit, má již jinou výhybku ve spojce.'+#13#10+'Pro vytvoøení nové spojky odstraòte spojku starou.'), 'Nelze uložit', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  F_BlkVyhybka.Close;
  Self.Blk.Change();
 end;//procedure

procedure TF_BlkVyhybka.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
 end;//procedure

end.//unit
