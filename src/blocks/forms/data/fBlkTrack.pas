unit fBlkTrack;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, BlockDb, Block, BlockTrack,
  Generics.Collections;

type
  TF_BlkTrack = class(TForm)
    B_OK: TButton;
    B_Storno: TButton;
    Label2: TLabel;
    SE_ID: TSpinEdit;
    E_Name: TEdit;
    Label1: TLabel;
    GB_RCS: TGroupBox;
    L_det1: TLabel;
    SE_Port1: TSpinEdit;
    Label3: TLabel;
    E_Length: TEdit;
    CHB_Loop: TCheckBox;
    Label5: TLabel;
    Label4: TLabel;
    CB_Booster: TComboBox;
    SE_Board1: TSpinEdit;
    CHB_D1: TCheckBox;
    L_det2: TLabel;
    CHB_D2: TCheckBox;
    SE_Board2: TSpinEdit;
    SE_Port2: TSpinEdit;
    L_det3: TLabel;
    CHB_D3: TCheckBox;
    SE_Board3: TSpinEdit;
    SE_Port3: TSpinEdit;
    L_det4: TLabel;
    CHB_D4: TCheckBox;
    SE_Board4: TSpinEdit;
    SE_Port4: TSpinEdit;
    SE_Max_Trains: TSpinEdit;
    Label6: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D1Click(Sender: TObject);
    procedure SE_RCS_BoardExit(Sender: TObject);
  private
    isNewBlock: Boolean;
    block: TBlkTrack;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();
  public
    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkTrack: TF_BlkTrack;

implementation

uses GetSystems, TechnologieRCS, BoosterDb, DataBloky,
  Booster, Area;

{$R *.dfm}

procedure TF_BlkTrack.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkTrack;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkTrack.NewBlock;
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkTrack.SE_RCS_BoardExit(Sender: TObject);
begin
  Self.SE_Port1.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Board1.Value, Self.SE_Port1.Value);
  Self.SE_Port2.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Board2.Value, Self.SE_Port2.Value);
  Self.SE_Port3.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Board3.Value, Self.SE_Port3.Value);
  Self.SE_Port4.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Board4.Value, Self.SE_Port4.Value);
end;

procedure TF_BlkTrack.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.E_Length.Text := '0';
  Self.CHB_Loop.Checked := false;
  Self.CB_Booster.ItemIndex := -1;
  Self.SE_Max_Trains.Enabled := true;
  Self.SE_Max_Trains.Value := 1;

  Self.SE_Port1.Value := 0;
  Self.SE_Board1.Value := 1;
  Self.SE_Port2.Value := 0;
  Self.SE_Board2.Value := 1;
  Self.SE_Port3.Value := 0;
  Self.SE_Board3.Value := 1;
  Self.SE_Port4.Value := 0;
  Self.SE_Board4.Value := 1;
  Self.SE_RCS_BoardExit(Self);

  Self.CHB_D1.Checked := false;
  Self.CHB_D1Click(Self.CHB_D1);

  Self.Caption := 'Nový blok Úsek';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkTrack.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkTrackSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.SE_Max_Trains.Value := settings.maxTrains;
  Self.SE_Max_Trains.Enabled := Self.block.spnl.stationTrack or Self.block.spnl.trainPos;

  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;
  Self.CHB_D4.Checked := false;

  case (settings.RCSAddrs.count) of
    0:
      begin
        Self.CHB_D1.Checked := false;
        Self.CHB_D1Click(Self.CHB_D1);
      end;
    1:
      begin
        Self.CHB_D1.Checked := true;
        Self.CHB_D1Click(Self.CHB_D1);
      end;
    2:
      begin
        Self.CHB_D2.Checked := true;
        Self.CHB_D1Click(Self.CHB_D2);
      end;
    3:
      begin
        Self.CHB_D3.Checked := true;
        Self.CHB_D1Click(Self.CHB_D3);
      end;
    4:
      begin
        Self.CHB_D4.Checked := true;
        Self.CHB_D1Click(Self.CHB_D4);
      end;
  end; // case

  if (settings.RCSAddrs.count > 0) then
  begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_Board1.MaxValue)) then
      Self.SE_Board1.MaxValue := 0;
    Self.SE_Port1.MaxValue := 0;

    Self.SE_Port1.Value := settings.RCSAddrs[0].port;
    Self.SE_Board1.Value := settings.RCSAddrs[0].board;
  end else begin
    Self.SE_Port1.Value := 0;
    Self.SE_Board1.Value := 0;
  end;

  if (settings.RCSAddrs.count > 1) then
  begin
    if (settings.RCSAddrs[1].board > Cardinal(Self.SE_Board2.MaxValue)) then
      Self.SE_Board2.MaxValue := 0;
    Self.SE_Port2.MaxValue := 0;

    Self.SE_Port2.Value := settings.RCSAddrs[1].port;
    Self.SE_Board2.Value := settings.RCSAddrs[1].board;
  end else begin
    Self.SE_Port2.Value := 0;
    Self.SE_Board2.Value := 0;
  end;

  if (settings.RCSAddrs.count > 2) then
  begin
    if (settings.RCSAddrs[2].board > Cardinal(Self.SE_Board3.MaxValue)) then
      Self.SE_Board3.MaxValue := 0;
    Self.SE_Port3.MaxValue := 0;

    Self.SE_Port3.Value := settings.RCSAddrs[2].port;
    Self.SE_Board3.Value := settings.RCSAddrs[2].board;
  end else begin
    Self.SE_Port3.Value := 0;
    Self.SE_Board3.Value := 0;
  end;

  if (settings.RCSAddrs.count > 3) then
  begin
    if (settings.RCSAddrs[3].board > Cardinal(Self.SE_Board4.MaxValue)) then
      Self.SE_Board4.MaxValue := 0;
    Self.SE_Port4.MaxValue := 0;

    Self.SE_Port4.Value := settings.RCSAddrs[3].port;
    Self.SE_Board4.Value := settings.RCSAddrs[3].board;
  end else begin
    Self.SE_Port4.Value := 0;
    Self.SE_Board4.Value := 0;
  end;

  Self.CB_Booster.ItemIndex := -1;
  for var i := 0 to Boosters.sorted.count - 1 do
  begin
    if (Boosters.sorted[i].id = settings.boosterId) then
    begin
      Self.CB_Booster.ItemIndex := i;
      break;
    end;
  end;

  Self.SE_RCS_BoardExit(Self);

  Self.E_Length.Text := FloatToStr(settings.lenght);
  Self.CHB_Loop.Checked := settings.loop;

  Self.Caption := 'Upravit blok ' + glob.name + ' (úsek)';
  Self.ActiveControl := Self.B_OK;
end;

procedure TF_BlkTrack.CommonOpenForm();
begin
  Self.SE_Board1.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board2.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board3.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Board4.MaxValue := RCSi.maxModuleAddrSafe;

  Self.CB_Booster.Clear();
  for var booster in Boosters.sorted do
    Self.CB_Booster.Items.Add(booster.name + ' (' + booster.id + ')');
end;

procedure TF_BlkTrack.B_StornoClick(Sender: TObject);
begin
  F_BlkTrack.Close();
end;

procedure TF_BlkTrack.B_OKClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, openIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Booster.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte zesilovač, kterému patří blok!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btTrack;

  if (isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkTrack;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.block.SetGlobalSettings(glob);
  end;

  // save block-specific data
  var settings: TBlkTrackSettings;
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_D1.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board1.Value, Self.SE_Port1.Value));
  if (Self.CHB_D2.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board2.Value, Self.SE_Port2.Value));
  if (Self.CHB_D3.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board3.Value, Self.SE_Port3.Value));
  if (Self.CHB_D4.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_Board4.Value, Self.SE_Port4.Value));

  settings.lenght := StrToFloatDef(Self.E_Length.Text, 0);
  settings.loop := Self.CHB_Loop.Checked;
  settings.boosterId := Boosters.sorted[Self.CB_Booster.ItemIndex].id;

  settings.houkEvL := Self.block.GetSettings().houkEvL;
  settings.houkEvS := Self.block.GetSettings().houkEvS;

  settings.maxTrains := Self.SE_Max_Trains.Value;

  Self.block.SetSettings(settings);

  for var addr in settings.RCSAddrs do
  begin
    var another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.input);
    if (another <> nil) then
      Application.MessageBox(PChar('Varování: blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString()), 'Varování', MB_OK OR MB_ICONWARNING);
  end;

  Self.Close();
  Self.block.Change();
end;

procedure TF_BlkTrack.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.openIndex := -1;
  Self.isNewBlock := false;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkTrack.CHB_D1Click(Sender: TObject);
begin
  case ((Sender as TCheckBox).Tag) of
    1:
      begin
        Self.SE_Port1.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Board1.Enabled := (Sender as TCheckBox).Checked;
      end;

    2:
      begin
        Self.SE_Port2.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Board2.Enabled := (Sender as TCheckBox).Checked;
      end;

    3:
      begin
        Self.SE_Port3.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Board3.Enabled := (Sender as TCheckBox).Checked;
      end;

    4:
      begin
        Self.SE_Port4.Enabled := (Sender as TCheckBox).Checked;
        Self.SE_Board4.Enabled := (Sender as TCheckBox).Checked;
      end;
  end; // case

  if ((Sender as TCheckBox).Checked) then
  begin
    // checked
    case ((Sender as TCheckBox).Tag) of
      2:
        Self.CHB_D1.Checked := true;
      3:
        Self.CHB_D2.Checked := true;
      4:
        Self.CHB_D3.Checked := true;
    end;
  end else begin
    // not checked
    case ((Sender as TCheckBox).Tag) of
      1:
        Self.CHB_D2.Checked := false;
      2:
        Self.CHB_D3.Checked := false;
      3:
        Self.CHB_D4.Checked := false;
    end;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
