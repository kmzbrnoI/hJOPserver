unit fBlkVyhybka;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, ExtCtrls, fMain, TBlokVyhybka, IBUtils, TBloky,
  Generics.Collections;

type
  TF_BlkVyhybka = class(TForm)
    L_Vyh01: TLabel;
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_Vyh02: TLabel;
    GB_RCS: TGroupBox;
    L_Vyh05: TLabel;
    L_Vyh06: TLabel;
    SE_VystPlus_port: TSpinEdit;
    SE_VystMinus_port: TSpinEdit;
    L_Vyh07: TLabel;
    L_Vyh08: TLabel;
    SE_VstPlus_port: TSpinEdit;
    SE_VstMinus_port: TSpinEdit;
    L_Vyh09: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    SE_VystPlus_module: TSpinEdit;
    SE_VystMinus_module: TSpinEdit;
    SE_VstPlus_module: TSpinEdit;
    SE_VstMinus_module: TSpinEdit;
    Label1: TLabel;
    GB_Zamek: TGroupBox;
    CB_Zamek: TComboBox;
    CHB_Zamek: TCheckBox;
    Label2: TLabel;
    CB_Zamek_Poloha: TComboBox;
    GB_Neprofil: TGroupBox;
    CHB_npPlus: TCheckBox;
    CB_npPlus: TComboBox;
    CHB_npMinus: TCheckBox;
    CB_npMinus: TComboBox;
    GB_Spojka: TGroupBox;
    CHB_Spojka: TCheckBox;
    CB_Spojka: TComboBox;
    CHB_Spojka_Common_In: TCheckBox;
    CHB_Spojka_Common_Out: TCheckBox;
    CHB_Feedback: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_SpojkaClick(Sender: TObject);
    procedure CHB_ZamekClick(Sender: TObject);
    procedure CHB_npPlusClick(Sender: TObject);
    procedure CHB_npMinusClick(Sender: TObject);
    procedure SE_moduleExit(Sender: TObject);
    procedure CHB_FeedbackClick(Sender: TObject);
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

uses Prevody, GetSystems, FileSystem, TechnologieRCS, TBlok,
    DataBloky, TOblRizeni;

{$R *.dfm}

procedure TF_BlkVyhybka.OpenForm(BlokIndex:Integer);
 begin
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  OpenIndex := BlokIndex;

  HlavniOpenForm;
  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  F_BlkVyhybka.ShowModal;
 end;

procedure TF_BlkVyhybka.SE_moduleExit(Sender: TObject);
begin
 Self.SE_VystPlus_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_VystPlus_module.Value, Self.SE_VystPlus_port.Value);
 Self.SE_VystMinus_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_VystMinus_module.Value, Self.SE_VystMinus_port.Value);
 Self.SE_VstPlus_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_VstPlus_module.Value, Self.SE_VstPlus_port.Value);
 Self.SE_VstMinus_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_VstMinus_module.Value, Self.SE_VstMinus_port.Value);
end;

procedure TF_BlkVyhybka.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value  := Blky.GetBlkID(Blky.count-1)+1;

  SE_VystPlus_port.Value := 0;
  SE_VystPlus_module.Value := 1;
  SE_VystMinus_port.Value := 0;
  SE_VystMinus_module.Value := 1;
  SE_VstPlus_port.Value := 0;
  SE_VstPlus_module.Value  := 1;
  SE_VstMinus_port.Value := 0;
  SE_VstMinus_module.Value := 1;
  Self.SE_moduleExit(Self);

  Self.CHB_Spojka.Checked := false;
  Self.CHB_Spojka_Common_In.Checked := false;
  Self.CHB_Spojka_Common_Out.Checked := false;
  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Self.CHB_Zamek.Checked  := false;
  Self.CHB_ZamekClick(Self.CHB_Spojka);

  Self.CHB_npPlus.Checked := false;
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := false;
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  Self.CHB_Feedback.Checked := true;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  F_BlkVyhybka.Caption := 'Editovat data nového bloku výhybka';
  F_BlkVyhybka.ActiveControl := E_Nazev;
 end;

procedure TF_BlkVyhybka.NormalOpenForm();
var glob:TBlkSettings;
    i:Integer;
    spojkaSettings, settings:TBlkVyhSettings;
    obls:TArStr;
    oblr:TOR;
    vyh:TBlkVyhybka;
 begin
  glob := Self.Blk.GetGlobalSettings();

  E_Nazev.Text  := glob.name;
  SE_ID.Value   := glob.id;

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  settings := Blk.GetSettings();

  Self.CHB_Spojka.Checked := (settings.spojka > -1);
  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Blky.GetBlkByID(settings.spojka, TBlk(vyh));
  if ((vyh <> nil) and (vyh.typ = _BLK_VYH)) then
   begin
    spojkaSettings := vyh.GetSettings();

    Self.CHB_Spojka_Common_In.Checked := (spojkaSettings.RCSAddrs.Count >= 2) and (settings.RCSAddrs.Count >= 2) and
      (spojkaSettings.RCSAddrs[0] = settings.RCSAddrs[0]) and (spojkaSettings.RCSAddrs[1] = settings.RCSAddrs[1]);
    Self.CHB_Spojka_Common_Out.Checked := (spojkaSettings.RCSAddrs.Count >= 4) and (settings.RCSAddrs.Count >= 4) and
      (spojkaSettings.RCSAddrs[2] = settings.RCSAddrs[2]) and (spojkaSettings.RCSAddrs[3] = settings.RCSAddrs[3]);
   end;

  Self.CHB_Zamek.Checked := (settings.zamek > -1);
  if (settings.zamek > -1) then
    Self.CB_Zamek_Poloha.ItemIndex := Integer(settings.zamekPoloha);
  Self.CHB_ZamekClick(Self.CHB_Zamek);

  //poradi(0..3): vst+,vst-,vyst+,vyst- (referencni RCS_board = [0])
  if (settings.RCSAddrs.Count > 0) then
   begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_VstPlus_module.MaxValue)) then
      Self.SE_VstPlus_module.MaxValue := 0;
    Self.SE_VstPlus_port.MaxValue := 0;

    SE_VstPlus_module.Value := settings.RCSAddrs[0].board;
    SE_VstPlus_port.Value := settings.RCSAddrs[0].port;
   end else begin
    SE_VstPlus_module.Value  := 0;
    SE_VstPlus_port.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 1) then
   begin
    if (settings.RCSAddrs[1].board > Cardinal(Self.SE_VstMinus_module.MaxValue)) then
      Self.SE_VstMinus_module.MaxValue := 0;
    Self.SE_VstMinus_port.MaxValue := 0;

    SE_VstMinus_module.Value := settings.RCSAddrs[1].board;
    SE_VstMinus_port.Value := settings.RCSAddrs[1].port;
   end else begin
    SE_VstMinus_module.Value  := 0;
    SE_VstMinus_port.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 2) then
   begin
    if (settings.RCSAddrs[2].board > Cardinal(Self.SE_VystPlus_module.MaxValue)) then
      Self.SE_VystPlus_module.MaxValue := 0;
    Self.SE_VystPlus_port.MaxValue := 0;

    SE_VystPlus_module.Value := settings.RCSAddrs[2].board;
    SE_VystPlus_port.Value := settings.RCSAddrs[2].port;
   end else begin
    SE_VystPlus_module.Value  := 0;
    SE_VystPlus_port.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 3) then
   begin
    if (settings.RCSAddrs[3].board > Cardinal(Self.SE_VystMinus_module.MaxValue)) then
      Self.SE_VystMinus_module.MaxValue := 0;
    Self.SE_VystMinus_port.MaxValue := 0;

    SE_VystMinus_module.Value := settings.RCSAddrs[3].board;
    SE_VystMinus_port.Value := settings.RCSAddrs[3].port;
   end else begin
    SE_VystMinus_module.Value  := 0;
    SE_VystMinus_port.Value := 0;
   end;

  Self.SE_moduleExit(Self);

  Self.CHB_Feedback.Checked := settings.detekcePolohy;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  Self.CHB_npPlus.Checked := (settings.npPlus > -1);
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := (settings.npMinus > -1);
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  SetLength(obls,Self.Blk.OblsRizeni.Count);
  for i := 0 to Self.Blk.OblsRizeni.Count-1 do
    obls[i] := Self.Blk.OblsRizeni[i].id;

  F_BlkVyhybka.Caption := 'Editovat data bloku : '+glob.name+' (výhybka)';
  F_BlkVyhybka.ActiveControl := B_Save;
 end;

procedure TF_BlkVyhybka.HlavniOpenForm;
var spojka_vypust:TArI;
    obls:TArStr;
    i:Integer;
 begin
  Self.LB_Stanice.Clear();

  Self.SE_VystPlus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VystMinus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VstPlus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VstMinus_module.MaxValue := RCSi.maxModuleAddrSafe;

  if (Self.Blk <> nil) then
   begin
    SetLength(obls,Self.Blk.OblsRizeni.Count);
    for i := 0 to Self.Blk.OblsRizeni.Count-1 do
      obls[i] := Self.Blk.OblsRizeni[i].id;
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

 end;

procedure TF_BlkVyhybka.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.count);
 end;

procedure TF_BlkVyhybka.B_StornoClick(Sender: TObject);
 begin
  F_BlkVyhybka.Close;
 end;

procedure TF_BlkVyhybka.CHB_FeedbackClick(Sender: TObject);
begin
 Self.SE_VstPlus_module.Enabled := Self.CHB_Feedback.Checked;
 Self.SE_VstPlus_port.Enabled := Self.CHB_Feedback.Checked;
 Self.SE_VstMinus_module.Enabled := Self.CHB_Feedback.Checked;
 Self.SE_VstMinus_port.Enabled := Self.CHB_Feedback.Checked;

 if (not Self.CHB_Feedback.Checked) then
  begin
   Self.SE_VstPlus_module.Value := 0;
   Self.SE_VstPlus_port.Value := 0;
   Self.SE_VstMinus_module.Value := 0;
   Self.SE_VstMinus_port.Value := 0;
  end;
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
 Self.CB_Spojka.Enabled := Self.CHB_Spojka.Checked;
 Self.CHB_Spojka_Common_In.Enabled := Self.CHB_Spojka.Checked;
 Self.CHB_Spojka_Common_Out.Enabled := Self.CHB_Spojka.Checked;
 Self.CHB_Spojka_Common_In.Checked := Self.CHB_Spojka.Checked;
 Self.CHB_Spojka_Common_Out.Checked := Self.CHB_Spojka.Checked;

 if (not Self.CHB_Spojka.Checked) then
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
    settings, spojkaSettings:TBlkVyhSettings;
    vyh:TBlkVyhybka;
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
      Application.MessageBox('Vyberte polohu výhybky pro uzamčení zámku!','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
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


  glob.name := E_Nazev.Text;
  glob.id := SE_ID.Value;
  glob.typ := _BLK_VYH;

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
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VstPlus_module.Value, SE_VstPlus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VstMinus_module.Value, SE_VstMinus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VystPlus_module.Value, SE_VystPlus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VystMinus_module.Value, SE_VystMinus_port.Value));

  if (Self.CHB_Spojka.Checked) then
   begin
    settings.spojka := Blky.GetBlkID(Self.CB_SpojkaData[Self.CB_Spojka.ItemIndex]);

    Blky.GetBlkByID(settings.spojka, TBlk(vyh));
    if ((blk = nil) or (blk.typ <> _BLK_VYH)) then
     begin
      Application.MessageBox('Blok spojky neexistuje nebo není výhybka', 'Chyba', MB_OK OR MB_ICONWARNING);
      Exit();
     end;

    spojkaSettings := vyh.GetSettings();

    if ((spojkaSettings.spojka <> -1) and (spojkaSettings.spojka <> glob.id)) then
     begin
      Application.MessageBox('Na spojkové výhybce je již jiná spojka!', 'Varování', MB_OK OR MB_ICONWARNING);
      Exit();
     end;

    while (spojkaSettings.RCSAddrs.Count < 4) do
      spojkaSettings.RCSAddrs.Add(TRCS.RCSAddr(0, 0));

    if (Self.CHB_Spojka_Common_Out.Checked) then
     begin
      spojkaSettings.RCSAddrs[2] := settings.RCSAddrs[2];
      spojkaSettings.RCSAddrs[3] := settings.RCSAddrs[3];
     end;
    if (Self.CHB_Spojka_Common_In.Checked) then
     begin
      spojkaSettings.RCSAddrs[0] := settings.RCSAddrs[0];
      spojkaSettings.RCSAddrs[1] := settings.RCSAddrs[1];
     end;

    vyh.SetSettings(spojkaSettings);
   end else begin
    settings.spojka := -1;
   end;

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

  settings.detekcePolohy := Self.CHB_Feedback.Checked;

  try
    Self.Blk.SetSettings(settings);
  except
    on E:Exception do
     begin
      Application.MessageBox(PChar(E.Message), 'Nelze uložit', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
  end;

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkVyhybka.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;
 end;

end.//unit
