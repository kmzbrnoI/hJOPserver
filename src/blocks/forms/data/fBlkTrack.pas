unit fBlkTrack;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, IBUtils,
  BlockDb, Block, BlockTrack, Generics.Collections;

type
  TF_BlkTrack = class(TForm)
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
   NewBlk: Boolean;
   Blk: TBlkTrack;
   OpenIndex: Integer;

    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure HlavniOpenForm();
  public
    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkTrack: TF_BlkTrack;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky,
     Booster, TOblRizeni;

{$R *.dfm}

procedure TF_BlkTrack.OpenForm(BlokIndex: Integer);
 begin
  Self.OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.HlavniOpenForm();

  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  F_BlkTrack.ShowModal();
 end;

procedure TF_BlkTrack.SE_RCS_BoardExit(Sender: TObject);
begin
 Self.SE_Port1.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board1.Value, Self.SE_Port1.Value);
 Self.SE_Port2.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board2.Value, Self.SE_Port2.Value);
 Self.SE_Port3.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board3.Value, Self.SE_Port3.Value);
 Self.SE_Port4.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board4.Value, Self.SE_Port4.Value);
end;

procedure TF_BlkTrack.NewBlkCreate;
 begin
  Self.NewBlk := true;
  Self.OpenForm(Blocks.count);
 end;

procedure TF_BlkTrack.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  E_Delka.Text := '0';
  CHB_SmycBlok.Checked := false;
  Self.CB_Zesil.ItemIndex := -1;
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

  Self.Caption := 'Nový blok Úsek';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkTrack.NormalOpenForm();
var glob: TBlkSettings;
    settings: TBlkTrackSettings;
    i: Integer;
    obls: TArstr;
    oblr: TOR;
 begin
  if (Assigned(Self.Blk)) then glob := Self.Blk.GetGlobalSettings();
  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value  := glob.id;

  for oblr in Self.Blk.stations do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls, Self.Blk.stations.Count);
  for i := 0 to Self.Blk.stations.Count-1 do
    obls[i] := Self.Blk.stations[i].id;

  if (Assigned(Self.Blk)) then settings := Self.Blk.GetSettings();

  Self.SE_SprCnt.Value := settings.maxTrains;
  Self.SE_SprCnt.Enabled := Self.Blk.spnl.stationTrack or Self.Blk.spnl.trainPos;

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
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_Board1.MaxValue)) then
      Self.SE_Board1.MaxValue := 0;
    Self.SE_Port1.MaxValue := 0;

    Self.SE_Port1.Value  := settings.RCSAddrs[0].port;
    Self.SE_Board1.Value := settings.RCSAddrs[0].board;
   end else begin
    Self.SE_Port1.Value  := 0;
    Self.SE_Board1.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 1) then
   begin
    if (settings.RCSAddrs[1].board > Cardinal(Self.SE_Board2.MaxValue)) then
      Self.SE_Board2.MaxValue := 0;
    Self.SE_Port2.MaxValue := 0;

    Self.SE_Port2.Value  := settings.RCSAddrs[1].port;
    Self.SE_Board2.Value := settings.RCSAddrs[1].board;
   end else begin
    Self.SE_Port2.Value  := 0;
    Self.SE_Board2.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 2) then
   begin
    if (settings.RCSAddrs[2].board > Cardinal(Self.SE_Board3.MaxValue)) then
      Self.SE_Board3.MaxValue := 0;
    Self.SE_Port3.MaxValue := 0;

    Self.SE_Port3.Value  := settings.RCSAddrs[2].port;
    Self.SE_Board3.Value := settings.RCSAddrs[2].board;
   end else begin
    Self.SE_Port3.Value  := 0;
    Self.SE_Board3.Value := 0;
   end;

  if (settings.RCSAddrs.Count > 3) then
   begin
    if (settings.RCSAddrs[3].board > Cardinal(Self.SE_Board4.MaxValue)) then
      Self.SE_Board4.MaxValue := 0;
    Self.SE_Port4.MaxValue := 0;

    Self.SE_Port4.Value  := settings.RCSAddrs[3].port;
    Self.SE_Board4.Value := settings.RCSAddrs[3].board;
   end else begin
    Self.SE_Port4.Value  := 0;
    Self.SE_Board4.Value := 0;
   end;

  Self.CB_Zesil.ItemIndex := -1;
  for i := 0 to Boosters.sorted.Count-1 do
   begin
    if (Boosters.sorted[i].id = settings.boosterId) then
     begin
      Self.CB_Zesil.ItemIndex := i;
      break;
     end;
   end;

  Self.SE_RCS_BoardExit(Self);

  Self.E_Delka.Text := FloatToStr(settings.lenght);
  Self.CHB_SmycBlok.Checked := settings.loop;

  F_BlkTrack.Caption := 'Upravit blok '+glob.name+' (úsek)';
  F_BlkTrack.ActiveControl := B_OK;
 end;

procedure TF_BlkTrack.HlavniOpenForm();
var booster: TBooster;
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

procedure TF_BlkTrack.B_StornoClick(Sender: TObject);
 begin
  F_BlkTrack.Close();
 end;

procedure TF_BlkTrack.B_OKClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkTrackSettings;
    addr: TRCSAddr;
    another: TBlk;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Self.CB_Zesil.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte zesilovač, kterému patří blok!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btTrack;

  if (NewBlk) then
   begin
    glob.note := '';

    try
      Blk := Blocks.Add(glob) as TBlkTrack;
    except
      on E: Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
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

  settings.lenght := StrToFloatDef(Self.E_Delka.Text,0);
  settings.loop := Self.CHB_SmycBlok.Checked;
  settings.boosterId  := Boosters.sorted[Self.CB_Zesil.ItemIndex].id;

  settings.houkEvL := Self.Blk.GetSettings().houkEvL;
  settings.houkEvS := Self.Blk.GetSettings().houkEvS;

  settings.maxTrains := Self.SE_SprCnt.Value;

  Self.Blk.SetSettings(settings);

  for addr in settings.RCSAddrs do
   begin
    another := Blocks.AnotherBlockUsesRCS(addr, Self.Blk, TRCSIOType.input);
    if (another <> nil) then
      Application.MessageBox(PChar('Varování: blok '+another.name+' využívá také RCS adresu '+IntToStr(addr.board)+':'+IntToStr(addr.port)),
                             'Varování', MB_OK OR MB_ICONWARNING);
   end;

  F_BlkTrack.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkTrack.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  Self.OpenIndex := -1;
  Self.NewBlk := false;
  BlokyTableData.UpdateTable();
 end;

procedure TF_BlkTrack.CHB_D1Click(Sender: TObject);
 begin
  case ((Sender as TCheckBox).Tag) of
   1: begin
    Self.SE_Port1.Enabled := (Sender as TCheckBox).Checked;
    Self.SE_Board1.Enabled := (Sender as TCheckBox).Checked;
   end;

   2: begin
    Self.SE_Port2.Enabled := (Sender as TCheckBox).Checked;
    Self.SE_Board2.Enabled := (Sender as TCheckBox).Checked;
   end;

   3: begin
    Self.SE_Port3.Enabled := (Sender as TCheckBox).Checked;
    Self.SE_Board3.Enabled := (Sender as TCheckBox).Checked;
   end;

   4: begin
    Self.SE_Port4.Enabled := (Sender as TCheckBox).Checked;
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
