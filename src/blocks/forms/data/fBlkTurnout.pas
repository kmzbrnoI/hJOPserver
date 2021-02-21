unit fBlkTurnout;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, ExtCtrls, fMain, BlockTurnout, BlockDb,
  Generics.Collections;

type
  TF_BlkTurnout = class(TForm)
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
    OpenIndex: Integer;
    Blk: TBlkTurnout;
    NewBlk: Boolean;
    CB_SpojkaData: TArI;
    CB_ZamekData: TArI;
    CB_NeprofilData: TArI;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkTurnout: TF_BlkTurnout;

implementation

uses GetSystems, FileSystem, TechnologieRCS, Block, DataBloky, Area;

{$R *.dfm}

procedure TF_BlkTurnout.OpenForm(BlokIndex: Integer);
begin
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  OpenIndex := BlokIndex;

  HlavniOpenForm;
  if (NewBlk) then
  begin
    NewBlkOpenForm;
  end else begin
    NormalOpenForm;
  end;
  F_BlkTurnout.ShowModal;
end;

procedure TF_BlkTurnout.SE_moduleExit(Sender: TObject);
begin
  Self.SE_VystPlus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_VystPlus_module.Value, Self.SE_VystPlus_port.Value);
  Self.SE_VystMinus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_VystMinus_module.Value,
    Self.SE_VystMinus_port.Value);
  Self.SE_VstPlus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_VstPlus_module.Value, Self.SE_VstPlus_port.Value);
  Self.SE_VstMinus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_VstMinus_module.Value, Self.SE_VstMinus_port.Value);
end;

procedure TF_BlkTurnout.NewBlkOpenForm();
begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  SE_VystPlus_port.Value := 0;
  SE_VystPlus_module.Value := 1;
  SE_VystMinus_port.Value := 0;
  SE_VystMinus_module.Value := 1;
  SE_VstPlus_port.Value := 0;
  SE_VstPlus_module.Value := 1;
  SE_VstMinus_port.Value := 0;
  SE_VstMinus_module.Value := 1;
  Self.SE_moduleExit(Self);

  Self.CHB_Spojka.Checked := false;
  Self.CHB_Spojka_Common_In.Checked := false;
  Self.CHB_Spojka_Common_Out.Checked := false;
  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Self.CHB_Zamek.Checked := false;
  Self.CHB_ZamekClick(Self.CHB_Spojka);

  Self.CHB_npPlus.Checked := false;
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := false;
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  Self.CHB_Feedback.Checked := true;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  F_BlkTurnout.Caption := 'Nový blok Výhybka';
  F_BlkTurnout.ActiveControl := E_Nazev;
end;

procedure TF_BlkTurnout.NormalOpenForm();
var glob: TBlkSettings;
  i: Integer;
  spojkaSettings, settings: TBlkTurnoutSettings;
  areas: TArStr;
  Area: TArea;
  turnout: TBlkTurnout;
begin
  glob := Self.Blk.GetGlobalSettings();

  E_Nazev.Text := glob.name;
  SE_ID.Value := glob.id;

  for Area in Self.Blk.areas do
    Self.LB_Stanice.Items.Add(Area.name);

  settings := Blk.GetSettings();

  Self.CHB_Spojka.Checked := (settings.coupling > -1);
  Self.CHB_SpojkaClick(Self.CHB_Spojka);

  Blocks.GetBlkByID(settings.coupling, TBlk(turnout));
  if ((turnout <> nil) and (turnout.typ = btTurnout)) then
  begin
    spojkaSettings := turnout.GetSettings();

    Self.CHB_Spojka_Common_In.Checked := (spojkaSettings.RCSAddrs.count >= 2) and (settings.RCSAddrs.count >= 2) and
      (spojkaSettings.RCSAddrs[0] = settings.RCSAddrs[0]) and (spojkaSettings.RCSAddrs[1] = settings.RCSAddrs[1]);
    Self.CHB_Spojka_Common_Out.Checked := (spojkaSettings.RCSAddrs.count >= 4) and (settings.RCSAddrs.count >= 4) and
      (spojkaSettings.RCSAddrs[2] = settings.RCSAddrs[2]) and (spojkaSettings.RCSAddrs[3] = settings.RCSAddrs[3]);
  end;

  Self.CHB_Zamek.Checked := (settings.lock > -1);
  if (settings.lock > -1) then
    Self.CB_Zamek_Poloha.ItemIndex := Integer(settings.lockPosition);
  Self.CHB_ZamekClick(Self.CHB_Zamek);

  if (settings.RCSAddrs.count > 0) then
  begin
    if (Self.Blk.rcsInPlus.board > Cardinal(Self.SE_VstPlus_module.MaxValue)) then
      Self.SE_VstPlus_module.MaxValue := 0;
    Self.SE_VstPlus_port.MaxValue := 0;

    SE_VstPlus_module.Value := Self.Blk.rcsInPlus.board;
    SE_VstPlus_port.Value := Self.Blk.rcsInPlus.port;
  end else begin
    SE_VstPlus_module.Value := 0;
    SE_VstPlus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 1) then
  begin
    if (Self.Blk.rcsInMinus.board > Cardinal(Self.SE_VstMinus_module.MaxValue)) then
      Self.SE_VstMinus_module.MaxValue := 0;
    Self.SE_VstMinus_port.MaxValue := 0;

    SE_VstMinus_module.Value := Self.Blk.rcsInMinus.board;
    SE_VstMinus_port.Value := Self.Blk.rcsInMinus.port;
  end else begin
    SE_VstMinus_module.Value := 0;
    SE_VstMinus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 2) then
  begin
    if (Self.Blk.rcsOutPlus.board > Cardinal(Self.SE_VystPlus_module.MaxValue)) then
      Self.SE_VystPlus_module.MaxValue := 0;
    Self.SE_VystPlus_port.MaxValue := 0;

    SE_VystPlus_module.Value := Self.Blk.rcsOutPlus.board;
    SE_VystPlus_port.Value := Self.Blk.rcsOutPlus.port;
  end else begin
    SE_VystPlus_module.Value := 0;
    SE_VystPlus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 3) then
  begin
    if (Self.Blk.rcsOutMinus.board > Cardinal(Self.SE_VystMinus_module.MaxValue)) then
      Self.SE_VystMinus_module.MaxValue := 0;
    Self.SE_VystMinus_port.MaxValue := 0;

    SE_VystMinus_module.Value := Self.Blk.rcsOutMinus.board;
    SE_VystMinus_port.Value := Self.Blk.rcsOutMinus.port;
  end else begin
    SE_VystMinus_module.Value := 0;
    SE_VystMinus_port.Value := 0;
  end;

  Self.SE_moduleExit(Self);

  Self.CHB_Feedback.Checked := settings.posDetection;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  Self.CHB_npPlus.Checked := (settings.npPlus > -1);
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := (settings.npMinus > -1);
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  SetLength(areas, Self.Blk.areas.count);
  for i := 0 to Self.Blk.areas.count - 1 do
    areas[i] := Self.Blk.areas[i].id;

  F_BlkTurnout.Caption := 'Uprabit blok ' + glob.name + ' (výhybka)';
  F_BlkTurnout.ActiveControl := B_Save;
end;

procedure TF_BlkTurnout.HlavniOpenForm;
var spojka_vypust: TArI;
  areas: TArStr;
  i: Integer;
begin
  Self.LB_Stanice.Clear();

  Self.SE_VystPlus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VystMinus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VstPlus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_VstMinus_module.MaxValue := RCSi.maxModuleAddrSafe;

  if (Self.Blk <> nil) then
  begin
    SetLength(areas, Self.Blk.areas.count);
    for i := 0 to Self.Blk.areas.count - 1 do
      areas[i] := Self.Blk.areas[i].id;
    SetLength(spojka_vypust, 1);
    spojka_vypust[0] := Self.Blk.id;

    // spojka
    Blocks.FillCB(Self.CB_Spojka, @Self.CB_SpojkaData, @spojka_vypust, areas, btTurnout,
      Self.Blk.GetSettings().coupling);
    Self.CHB_Spojka.Enabled := (Length(Self.CB_SpojkaData) > 0) or (Self.Blk.GetSettings.coupling > -1);

    // zamek
    Blocks.FillCB(Self.CB_Zamek, @Self.CB_ZamekData, nil, areas, btLock, Self.Blk.GetSettings().lock);
    Self.CHB_Zamek.Enabled := (Length(Self.CB_ZamekData) > 0) or (Self.Blk.GetSettings.lock > -1);

    // neprofilove styky +
    Blocks.FillCB(Self.CB_npPlus, @Self.CB_NeprofilData, nil, areas, btTrack, Self.Blk.GetSettings().npPlus, btRT);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npPlus > -1);

    // neprofilove styky -
    Blocks.FillCB(Self.CB_npMinus, @Self.CB_NeprofilData, nil, areas, btTrack, Self.Blk.GetSettings().npMinus, btRT);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npMinus > -1);

  end else begin
    Blocks.FillCB(Self.CB_Spojka, @Self.CB_SpojkaData, nil, nil, btTurnout, -1);
    Self.CHB_Spojka.Enabled := (Length(Self.CB_SpojkaData) > 0);

    Blocks.FillCB(Self.CB_Zamek, @Self.CB_ZamekData, nil, nil, btLock, -1);
    Self.CHB_Zamek.Enabled := (Length(Self.CB_ZamekData) > 0);

    Blocks.FillCB(Self.CB_npPlus, @Self.CB_NeprofilData, nil, nil, btTrack, -1, btRT);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0);

    Blocks.FillCB(Self.CB_npMinus, @Self.CB_NeprofilData, nil, nil, btTrack, -1, btRT);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0);
  end;

end;

procedure TF_BlkTurnout.NewBlkCreate;
begin
  NewBlk := true;
  OpenForm(Blocks.count);
end;

procedure TF_BlkTurnout.B_StornoClick(Sender: TObject);
begin
  F_BlkTurnout.Close;
end;

procedure TF_BlkTurnout.CHB_FeedbackClick(Sender: TObject);
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

procedure TF_BlkTurnout.CHB_npMinusClick(Sender: TObject);
begin
  Self.CB_npMinus.Enabled := Self.CHB_npMinus.Checked;
  if (not Self.CHB_npMinus.Checked) then
    Self.CB_npMinus.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_npPlusClick(Sender: TObject);
begin
  Self.CB_npPlus.Enabled := Self.CHB_npPlus.Checked;
  if (not Self.CHB_npPlus.Checked) then
    Self.CB_npPlus.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_SpojkaClick(Sender: TObject);
begin
  Self.CB_Spojka.Enabled := Self.CHB_Spojka.Checked;
  Self.CHB_Spojka_Common_In.Enabled := Self.CHB_Spojka.Checked;
  Self.CHB_Spojka_Common_Out.Enabled := Self.CHB_Spojka.Checked;
  Self.CHB_Spojka_Common_In.Checked := Self.CHB_Spojka.Checked;
  Self.CHB_Spojka_Common_Out.Checked := Self.CHB_Spojka.Checked;

  if (not Self.CHB_Spojka.Checked) then
    Self.CB_Spojka.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_ZamekClick(Sender: TObject);
begin
  Self.CB_Zamek.Enabled := (Sender as TCheckBox).Checked;
  Self.CB_Zamek_Poloha.Enabled := (Sender as TCheckBox).Checked;
  if (not(Sender as TCheckBox).Checked) then
  begin
    Self.CB_Zamek.ItemIndex := -1;
    Self.CB_Zamek_Poloha.ItemIndex := -1;
  end;
end;

procedure TF_BlkTurnout.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings, spojkaSettings: TBlkTurnoutSettings;
  vyh: TBlkTurnout;
  another: TBlk;
  typ: TRCSIOType;
  i: Integer;
  messages: string;
begin
  if (E_Nazev.Text = '') then
  begin
    Application.MessageBox('Vyplnte nazev bloku !', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID jiz bylo definovano na jinem bloku !', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_Spojka.Checked) and (Self.CB_Spojka.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte spojku !', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CHB_Zamek.Checked) then
  begin
    if (Self.CB_Zamek.ItemIndex < 0) then
    begin
      Application.MessageBox('Vyberte zámek !', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
    if (Self.CB_Zamek_Poloha.ItemIndex < 0) then
    begin
      Application.MessageBox('Vyberte polohu výhybky pro uzamčení zámku!', 'Nelze ulozit data',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if ((Self.CHB_npPlus.Checked) and (Self.CB_npPlus.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu plus!', 'Nelze ulozit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_npMinus.Checked) and (Self.CB_npMinus.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu mínus!', 'Nelze ulozit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btTurnout;

  if (NewBlk) then
  begin
    try
      Blk := Blocks.Add(glob) as TBlkTurnout;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
  end;

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VstPlus_module.Value, SE_VstPlus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VstMinus_module.Value, SE_VstMinus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VystPlus_module.Value, SE_VystPlus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_VystMinus_module.Value, SE_VystMinus_port.Value));

  if (Self.CHB_Spojka.Checked) then
  begin
    settings.coupling := Blocks.GetBlkID(Self.CB_SpojkaData[Self.CB_Spojka.ItemIndex]);

    Blocks.GetBlkByID(settings.coupling, TBlk(vyh));
    if ((Blk = nil) or (Blk.typ <> btTurnout)) then
    begin
      Application.MessageBox('Blok spojky neexistuje nebo není výhybka', 'Chyba', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    spojkaSettings := vyh.GetSettings();

    if ((spojkaSettings.coupling <> -1) and (spojkaSettings.coupling <> glob.id)) then
    begin
      Application.MessageBox('Na spojkové výhybce je již jiná spojka!', 'Varování', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    while (spojkaSettings.RCSAddrs.count < 4) do
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
    settings.coupling := -1;
  end;

  if (Self.CHB_Zamek.Checked) then
  begin
    settings.lock := Blocks.GetBlkID(Self.CB_ZamekData[Self.CB_Zamek.ItemIndex]);
    settings.lockPosition := TTurnoutPosition(Self.CB_Zamek_Poloha.ItemIndex);
  end else begin
    settings.lock := -1;
    settings.lockPosition := TTurnoutPosition.none;
  end;

  if (Self.CHB_npPlus.Checked) then
    settings.npPlus := Blocks.GetBlkID(Self.CB_NeprofilData[Self.CB_npPlus.ItemIndex])
  else
    settings.npPlus := -1;

  if (Self.CHB_npMinus.Checked) then
    settings.npMinus := Blocks.GetBlkID(Self.CB_NeprofilData[Self.CB_npMinus.ItemIndex])
  else
    settings.npMinus := -1;

  settings.posDetection := Self.CHB_Feedback.Checked;

  try
    Self.Blk.SetSettings(settings);
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar(E.Message), 'Nelze uložit', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  messages := '';
  for i := 0 to settings.RCSAddrs.count - 1 do
  begin
    if (i < 2) then
      typ := TRCSIOType.input
    else
      typ := TRCSIOType.output;

    another := Blocks.AnotherBlockUsesRCS(settings.RCSAddrs[i], Self.Blk, typ);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + IntToStr(settings.RCSAddrs[i].board)
        + ':' + IntToStr(settings.RCSAddrs[i].port) + '.' + #13#10;
  end;

  if (messages <> '') then
    Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);

  Self.Close();
  Self.Blk.Change();
end;

procedure TF_BlkTurnout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable;
end;

end.// unit
