﻿unit fBlkPst;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, BlockPst,
  Vcl.ComCtrls, Generics.Collections;

type
  TF_BlkPst = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    B_Storno: TButton;
    B_Apply: TButton;
    GB_RCS: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    SE_RCS_Take_Module: TSpinEdit;
    SE_RCS_Indication_Module: TSpinEdit;
    SE_RCS_Indication_Port: TSpinEdit;
    SE_RCS_Take_Port: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SE_RCS_Release_Module: TSpinEdit;
    SE_RCS_Release_Port: TSpinEdit;
    SE_RCS_Horn_Port: TSpinEdit;
    SE_RCS_Horn_Module: TSpinEdit;
    Label9: TLabel;
    GB_Tracks: TGroupBox;
    GB_TrackEdit: TGroupBox;
    CB_Track: TComboBox;
    B_Track_Ok: TButton;
    LV_Tracks: TListView;
    GB_Turnouts: TGroupBox;
    LV_Turnouts: TListView;
    GB_TurnoutEdit: TGroupBox;
    CB_Turnout: TComboBox;
    B_Turnout_Ok: TButton;
    GB_Refugees: TGroupBox;
    LV_Refugees: TListView;
    GB_RefEdit: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_Ref_Block: TComboBox;
    CB_Ref_Pos: TComboBox;
    B_Ref_Ok: TButton;
    GB_Signals: TGroupBox;
    GB_Signal_Edit: TGroupBox;
    CB_Signal: TComboBox;
    B_Signal_Ok: TButton;
    LV_Signals: TListView;
    B_Track_Del: TButton;
    B_Ref_Del: TButton;
    B_Turnout_Del: TButton;
    B_Signal_Del: TButton;
    GB_Disconnectors: TGroupBox;
    GB_disc_edit: TGroupBox;
    CB_Disconnector: TComboBox;
    B_Disc_Ok: TButton;
    B_Disc_Delete: TButton;
    LV_Disconnectors: TListView;
    Label3: TLabel;
    SE_RCS_Active_Module: TSpinEdit;
    SE_RCS_Active_Port: TSpinEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure LV_TracksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_Track_OkClick(Sender: TObject);
    procedure B_Track_DelClick(Sender: TObject);
    procedure LV_TurnoutsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_Turnout_OkClick(Sender: TObject);
    procedure B_Turnout_DelClick(Sender: TObject);
    procedure LV_SignalsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_Signal_OkClick(Sender: TObject);
    procedure LV_RefugeesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_Ref_OkClick(Sender: TObject);
    procedure B_Ref_DelClick(Sender: TObject);
    procedure B_Signal_DelClick(Sender: TObject);
    procedure SE_RCS_Take_ModuleExit(Sender: TObject);
    procedure SE_RCS_Release_ModuleExit(Sender: TObject);
    procedure SE_RCS_Indication_ModuleExit(Sender: TObject);
    procedure SE_RCS_Horn_ModuleExit(Sender: TObject);
    procedure LV_DisconnectorsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_Disc_DeleteClick(Sender: TObject);
    procedure B_Disc_OkClick(Sender: TObject);
    procedure SE_RCS_Active_ModuleExit(Sender: TObject);
    procedure LV_DisconnectorsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_TracksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_TurnoutsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_RefugeesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_SignalsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    isNewBlock: Boolean;
    block: TBlkPst;
    CB_TrackItems: TList<Integer>;
    CB_TurnoutItems: TList<Integer>;
    CB_RefugeeItems: TList<Integer>;
    CB_SignalItems: TList<Integer>;
    CB_DiscItems: TList<Integer>;
    openIndex: Integer;

    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();

    procedure FillBlockLI(var LI: TListItem; blockId: Integer);
    procedure FillRefugeeLI(var LI: TListItem; blockId: Integer; pos: string);
    function BlockPresent(id: Integer; LV: TListView): Boolean;

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;


var
  F_BlkPst: TF_BlkPst;

implementation

uses BlockDb, Block, Area, DataBloky, IfThenElse, BlockTurnout, TechnologieRCS,
  ownGuiUtils;

{$R *.dfm}

procedure TF_BlkPst.FormCreate(Sender: TObject);
begin
  Self.CB_TrackItems := TList<Integer>.Create();
  Self.CB_TurnoutItems := TList<Integer>.Create();
  Self.CB_RefugeeItems := TList<Integer>.Create();
  Self.CB_SignalItems := TList<Integer>.Create();
  Self.CB_DiscItems := TList<Integer>.Create();
end;

procedure TF_BlkPst.FormDestroy(Sender: TObject);
begin
  Self.CB_TrackItems.Free();
  Self.CB_TurnoutItems.Free();
  Self.CB_RefugeeItems.Free();
  Self.CB_SignalItems.Free();
  Self.CB_DiscItems.Free();
end;

procedure TF_BlkPst.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkPst;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.NormalOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkPst.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Blocks.FillCB(Self.CB_Track, Self.CB_TrackItems, nil, nil, btTrack, btRT);
  Self.B_Track_Ok.Enabled := Self.CB_Track.Enabled;
  Blocks.FillCB(Self.CB_Turnout, Self.CB_TurnoutItems, nil, nil, btTurnout);
  Self.B_Turnout_Ok.Enabled := Self.CB_Turnout.Enabled;
  Blocks.FillCB(Self.CB_Ref_Block, Self.CB_RefugeeItems, nil, nil, btTurnout);
  Self.B_Ref_Ok.Enabled := Self.CB_Ref_Block.Enabled;
  Blocks.FillCB(Self.CB_Signal, Self.CB_SignalItems, nil, nil, btSignal);
  Self.B_Signal_Ok.Enabled := Self.CB_Signal.Enabled;
  Blocks.FillCB(Self.CB_Disconnector, Self.CB_DiscItems, nil, nil, btDisconnector);
  Self.B_Disc_Ok.Enabled := Self.CB_Disconnector.Enabled;

  Self.SE_RCS_Take_Module.Value := 1;
  Self.SE_RCS_Take_Port.Value := 0;
  Self.SE_RCS_Release_Module.Value := 1;
  Self.SE_RCS_Release_Port.Value := 0;
  Self.SE_RCS_Indication_Module.Value := 1;
  Self.SE_RCS_Indication_Port.Value := 0;
  Self.SE_RCS_Horn_Module.Value := 1;
  Self.SE_RCS_Horn_Port.Value := 0;
  Self.SE_RCS_Active_Module.Value := 1;
  Self.SE_RCS_Active_Port.Value := 0;

  Self.Caption := 'Nový blok Pomocné stavědlo';
end;

procedure TF_BlkPst.NormalOpenForm();
begin
  var glob := Self.block.GetGlobalSettings();
  var pstSettings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  for var i := 0 to pstSettings.tracks.Count-1 do
  begin
    var LI := Self.LV_Tracks.Items.Add();
    Self.FillBlockLI(LI, pstSettings.tracks[i]);
  end;
  Blocks.FillCB(Self.CB_Track, Self.CB_TrackItems, nil, Self.block.areas, btTrack, btRT);
  Self.B_Track_Ok.Enabled := Self.CB_Track.Enabled;

  for var i := 0 to pstSettings.turnouts.Count-1 do
  begin
    var LI := Self.LV_Turnouts.Items.Add();
    Self.FillBlockLI(LI, pstSettings.turnouts[i]);
  end;
  Blocks.FillCB(Self.CB_Turnout, Self.CB_TurnoutItems, nil, Self.block.areas, btTurnout);
  Self.B_Turnout_Ok.Enabled := Self.CB_Turnout.Enabled;

  for var i := 0 to pstSettings.refugees.Count-1 do
  begin
    var LI := Self.LV_Refugees.Items.Add();
    case (pstSettings.refugees[i].position) of
      TTurnoutPosition.plus: Self.FillRefugeeLI(LI, pstSettings.refugees[i].block, '+');
      TTurnoutPosition.minus: Self.FillRefugeeLI(LI, pstSettings.refugees[i].block, '-');
    end;
  end;
  Blocks.FillCB(Self.CB_Ref_Block, Self.CB_RefugeeItems, nil, Self.block.areas, btTurnout);
  Self.B_Ref_Ok.Enabled := Self.CB_Ref_Block.Enabled;

  for var i := 0 to pstSettings.signals.Count-1 do
  begin
    var LI := Self.LV_Signals.Items.Add();
    Self.FillBlockLI(LI, pstSettings.signals[i]);
  end;
  Blocks.FillCB(Self.CB_Signal, Self.CB_SignalItems, nil, Self.block.areas, btSignal);
  Self.B_Signal_Ok.Enabled := Self.CB_Signal.Enabled;

  for var i := 0 to pstSettings.disconnectors.Count-1 do
  begin
    var LI := Self.LV_Disconnectors.Items.Add();
    Self.FillBlockLI(LI, pstSettings.disconnectors[i]);
  end;
  Blocks.FillCB(Self.CB_Disconnector, Self.CB_DiscItems, nil, Self.block.areas, btDisconnector);
  Self.B_Disc_Ok.Enabled := Self.CB_Disconnector.Enabled;

  if (pstSettings.rcsInTake.board > Cardinal(Self.SE_RCS_Take_Module.MaxValue)) then
    Self.SE_RCS_Take_Module.MaxValue := 0;
  Self.SE_RCS_Take_Port.MaxValue := 0;
  Self.SE_RCS_Take_Module.Value := pstSettings.rcsInTake.board;
  Self.SE_RCS_Take_Port.Value := pstSettings.rcsInTake.port;

  if (pstSettings.rcsInRelease.board > Cardinal(Self.SE_RCS_Release_Module.MaxValue)) then
    Self.SE_RCS_Release_Module.MaxValue := 0;
  Self.SE_RCS_Release_Port.MaxValue := 0;
  Self.SE_RCS_Release_Module.Value := pstSettings.rcsInRelease.board;
  Self.SE_RCS_Release_Port.Value := pstSettings.rcsInRelease.port;

  if (pstSettings.rcsOutTaken.board > Cardinal(Self.SE_RCS_Indication_Module.MaxValue)) then
    Self.SE_RCS_Indication_Module.MaxValue := 0;
  Self.SE_RCS_Indication_Port.MaxValue := 0;
  Self.SE_RCS_Indication_Module.Value := pstSettings.rcsOutTaken.board;
  Self.SE_RCS_Indication_Port.Value := pstSettings.rcsOutTaken.port;

  if (pstSettings.rcsOutHorn.board > Cardinal(Self.SE_RCS_Horn_Module.MaxValue)) then
    Self.SE_RCS_Horn_Module.MaxValue := 0;
  Self.SE_RCS_Horn_Port.MaxValue := 0;
  Self.SE_RCS_Horn_Module.Value := pstSettings.rcsOutHorn.board;
  Self.SE_RCS_Horn_Port.Value := pstSettings.rcsOutHorn.port;

  if (pstSettings.rcsOutActive.board > Cardinal(Self.SE_RCS_Active_Module.MaxValue)) then
    Self.SE_RCS_Active_Module.MaxValue := 0;
  Self.SE_RCS_Active_Port.MaxValue := 0;
  Self.SE_RCS_Active_Module.Value := pstSettings.rcsOutActive.board;
  Self.SE_RCS_Active_Port.Value := pstSettings.rcsOutActive.port;

  Self.Caption := 'Upravit blok ' + glob.name + ' (pomocné stavědlo)';
end;

procedure TF_BlkPst.SE_RCS_Active_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Active_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCS_Active_Module.Value,
    Self.SE_RCS_Active_Port.Value);
end;

procedure TF_BlkPst.SE_RCS_Horn_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Horn_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCS_Horn_Module.Value,
    Self.SE_RCS_Horn_Port.Value);
end;

procedure TF_BlkPst.SE_RCS_Indication_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Indication_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCS_Indication_Module.Value,
    Self.SE_RCS_Indication_Port.Value);
end;

procedure TF_BlkPst.SE_RCS_Release_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Release_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_RCS_Release_Module.Value,
    Self.SE_RCS_Release_Port.Value);
end;

procedure TF_BlkPst.SE_RCS_Take_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Take_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_RCS_Take_Module.Value,
    Self.SE_RCS_Take_Port.Value);
end;

procedure TF_BlkPst.CommonOpenForm();
begin
  Self.LV_Tracks.Clear();
  Self.LV_Turnouts.Clear();
  Self.LV_Refugees.Clear();
  Self.LV_Signals.Clear();
  Self.LV_Disconnectors.Clear();

  Self.B_Track_Del.Enabled := false;
  Self.B_Turnout_Del.Enabled := false;
  Self.B_Ref_Del.Enabled := false;
  Self.B_Signal_Del.Enabled := false;
  Self.B_Disc_Delete.Enabled := false;

  Self.SE_RCS_Take_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCS_Indication_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCS_Release_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCS_Horn_Module.MaxValue := RCSi.maxModuleAddrSafe;

  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkPst.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewBlkOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkPst.B_ApplyClick(Sender: TObject);
var glob: TBlkSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  try
    glob.name := Self.E_Name.Text;
    glob.id := Self.SE_ID.Value;
    glob.typ := btPst;

    if (Self.isNewBlock) then
    begin
      try
        Self.block := Blocks.Add(glob) as TBlkPst;
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se přidat blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end else begin
      try
        Self.block.SetGlobalSettings(glob);
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se uložit blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end;

    var pstSettings: TBlkPstSettings;
    pstSettings.tracks := TList<Integer>.Create();
    for var LI: TListItem in Self.LV_Tracks.Items do
      pstSettings.tracks.Add(StrToInt(LI.SubItems[0]));

    pstSettings.turnouts := TList<Integer>.Create();
    for var LI: TListItem in Self.LV_Turnouts.Items do
      pstSettings.turnouts.Add(StrToInt(LI.SubItems[0]));

    pstSettings.signals := TList<Integer>.Create();
    for var LI: TListItem in Self.LV_Signals.Items do
      pstSettings.signals.Add(StrToInt(LI.SubItems[0]));

    pstSettings.refugees := TList<TPstRefugeeZav>.Create();
    for var LI: TListItem in Self.LV_Refugees.Items do
    begin
      var zav : TPstRefugeeZav;
      zav.block := StrToInt(LI.SubItems[0]);
      if (LI.SubItems[2] = '-') then
        zav.position := TTurnoutPosition.minus
      else
        zav.position := TTurnoutPosition.plus;
      pstSettings.refugees.Add(zav);
    end;

    pstSettings.disconnectors := TList<Integer>.Create();
    for var LI: TListItem in Self.LV_Disconnectors.Items do
      pstSettings.disconnectors.Add(StrToInt(LI.SubItems[0]));

    pstSettings.rcsInTake.board := Self.SE_RCS_Take_Module.Value;
    pstSettings.rcsInTake.port := Self.SE_RCS_Take_Port.Value;

    pstSettings.rcsInRelease.board := Self.SE_RCS_Release_Module.Value;
    pstSettings.rcsInRelease.port := Self.SE_RCS_Release_Port.Value;

    pstSettings.rcsOutTaken.board := Self.SE_RCS_Indication_Module.Value;
    pstSettings.rcsOutTaken.port := Self.SE_RCS_Indication_Port.Value;

    pstSettings.rcsOutHorn.board := Self.SE_RCS_Horn_Module.Value;
    pstSettings.rcsOutHorn.port := Self.SE_RCS_Horn_Port.Value;

    pstSettings.rcsOutActive.board := Self.SE_RCS_Active_Module.Value;
    pstSettings.rcsOutActive.port := Self.SE_RCS_Active_Port.Value;

    Self.block.SetSettings(pstSettings);
    Self.block.Change();
  except
    on E: Exception do
    begin
      ExceptionMessageBox('Neočekávaná chyba.', E);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_BlkPst.B_Disc_DeleteClick(Sender: TObject);
begin
  Self.LV_Disconnectors.DeleteSelected();
end;

procedure TF_BlkPst.B_Disc_OkClick(Sender: TObject);
begin
  if (not Self.CB_Disconnector.Enabled) then
    Exit();

  if (Self.CB_Disconnector.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte rozpojovač!', 'Nelze přidat/upravit rozpojovač', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_DiscItems[Self.CB_Disconnector.ItemIndex];
  if ((Self.LV_Disconnectors.Selected = nil) and (Self.BlockPresent(id, Self.LV_Disconnectors))) then
  begin
    StrMessageBox('Nelze přidat duplicitní rozpojovač!', 'Nelze přidat/upravit rozpojovač', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Disconnectors.Selected = nil) then
    LI := Self.LV_Disconnectors.Items.Add()
  else
    LI := Self.LV_Disconnectors.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkPst.B_Ref_DelClick(Sender: TObject);
begin
  Self.LV_Refugees.DeleteSelected();
end;

procedure TF_BlkPst.B_Ref_OkClick(Sender: TObject);
begin
  if (not Self.CB_Ref_Block.Enabled) then
    Exit();

  if (Self.CB_Ref_Block.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte výhybku!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_RefugeeItems[Self.CB_Ref_Block.ItemIndex];
  if ((Self.LV_Refugees.Selected = nil) and (Self.BlockPresent(id, Self.LV_Refugees))) then
  begin
    StrMessageBox('Nelze přidat duplicitní výhybku!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.CB_Ref_Pos.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte polohu!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.BlockPresent(id, Self.LV_Turnouts)) then
  begin
    StrMessageBox('Nelze přidat odvrat, který je zároveň ovládaný!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Refugees.Selected = nil) then
    LI := Self.LV_Refugees.Items.Add()
  else
    LI := Self.LV_Refugees.Selected;

  Self.FillRefugeeLI(LI, id, Self.CB_Ref_Pos.Items[Self.CB_Ref_Pos.ItemIndex]);
end;

procedure TF_BlkPst.B_Signal_DelClick(Sender: TObject);
begin
  Self.LV_Signals.DeleteSelected();
end;

procedure TF_BlkPst.B_Signal_OkClick(Sender: TObject);
begin
  if (not Self.CB_Signal.Enabled) then
    Exit();

  if (Self.CB_Signal.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte návěstidlo!', 'Nelze přidat/upravit návěstidlo', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_SignalItems[Self.CB_Signal.ItemIndex];
  if ((Self.LV_Signals.Selected = nil) and (Self.BlockPresent(id, Self.LV_Signals))) then
  begin
    StrMessageBox('Nelze přidat duplicitní návěstidlo!', 'Nelze přidat/upravit návěstidlo', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Signals.Selected = nil) then
    LI := Self.LV_Signals.Items.Add()
  else
    LI := Self.LV_Signals.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkPst.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkPst.B_Track_DelClick(Sender: TObject);
begin
  Self.LV_Tracks.DeleteSelected();
end;

procedure TF_BlkPst.B_Track_OkClick(Sender: TObject);
begin
  if (not Self.CB_Track.Enabled) then
    Exit();

  if (Self.CB_Track.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_TrackItems[Self.CB_Track.ItemIndex];
  if ((Self.LV_Tracks.Selected = nil) and (Self.BlockPresent(id, Self.LV_Tracks))) then
  begin
    StrMessageBox('Nelze přidat duplicitní úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Tracks.Selected = nil) then
    LI := Self.LV_Tracks.Items.Add()
  else
    LI := Self.LV_Tracks.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkPst.B_Turnout_DelClick(Sender: TObject);
begin
  Self.LV_Turnouts.DeleteSelected();
end;

procedure TF_BlkPst.B_Turnout_OkClick(Sender: TObject);
begin
  if (not Self.CB_Turnout.Enabled) then
    Exit();

  if (Self.CB_Turnout.ItemIndex = -1) then
  begin
    StrMessageBox('Vyberte výhybku!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_TurnoutItems[Self.CB_Turnout.ItemIndex];
  if ((Self.LV_Turnouts.Selected = nil) and (Self.BlockPresent(id, Self.LV_Turnouts))) then
  begin
    StrMessageBox('Nelze přidat duplicitní výhybku!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.BlockPresent(id, Self.LV_Refugees)) then
  begin
    StrMessageBox('Nelze přidat výhybku, která je na odvratu!', 'Nelze přidat/upravit výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Turnouts.Selected = nil) then
    LI := Self.LV_Turnouts.Items.Add()
  else
    LI := Self.LV_Turnouts.Selected;

  Self.FillBlockLI(LI, id);
end;

procedure TF_BlkPst.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

procedure TF_BlkPst.LV_DisconnectorsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Disc_Delete.Enabled := (Self.LV_Disconnectors.Selected <> nil);

  if (Self.CB_Disconnector.Enabled) then
  begin
    Self.CB_Disconnector.ItemIndex := -1;
    if (Self.LV_Disconnectors.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Disconnectors.Selected.SubItems[0]);
      for var i := 0 to Self.CB_DiscItems.Count-1 do
        if (Self.CB_DiscItems[i] = id) then
          Self.CB_Disconnector.ItemIndex := i;
    end;
  end;
end;

procedure TF_BlkPst.LV_DisconnectorsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Disc_Delete.Enabled)) then
    Self.B_Disc_DeleteClick(Self);
end;

procedure TF_BlkPst.LV_RefugeesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Ref_Del.Enabled := (Self.LV_Refugees.Selected <> nil);

  if (Self.CB_Ref_Block.Enabled) then
  begin
    Self.CB_Ref_Block.ItemIndex := -1;
    Self.CB_Ref_Pos.ItemIndex := -1;
    if (Self.LV_Refugees.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Refugees.Selected.SubItems[0]);
      for var i := 0 to Self.CB_RefugeeItems.Count-1 do
      begin
        if (Self.CB_RefugeeItems[i] = id) then
        begin
          Self.CB_Ref_Block.ItemIndex := i;
          Self.CB_Ref_Pos.ItemIndex := ite(Self.LV_Refugees.Selected.SubItems[2] = '+', 0, 1);
        end;
      end;
    end;
  end;
end;

procedure TF_BlkPst.LV_RefugeesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Ref_Del.Enabled)) then
    Self.B_Ref_DelClick(Self);
end;

procedure TF_BlkPst.LV_SignalsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Signal_Del.Enabled := (Self.LV_Signals.Selected <> nil);

  if (Self.CB_Signal.Enabled) then
  begin
    Self.CB_Signal.ItemIndex := -1;
    if (Self.LV_Signals.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Signals.Selected.SubItems[0]);
      for var i := 0 to Self.CB_SignalItems.Count-1 do
        if (Self.CB_SignalItems[i] = id) then
          Self.CB_Signal.ItemIndex := i;
    end;
  end;
end;

procedure TF_BlkPst.LV_SignalsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Signal_Del.Enabled)) then
    Self.B_Signal_DelClick(Self);
end;

procedure TF_BlkPst.LV_TracksChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Track_Del.Enabled := (Self.LV_Tracks.Selected <> nil);

  if (Self.CB_Track.Enabled) then
  begin
    Self.CB_Track.ItemIndex := -1;
    if (Self.LV_Tracks.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Tracks.Selected.SubItems[0]);
      for var i := 0 to Self.CB_TrackItems.Count-1 do
        if (Self.CB_TrackItems[i] = id) then
          Self.CB_Track.ItemIndex := i;
    end;
  end;
end;

procedure TF_BlkPst.LV_TracksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Track_Del.Enabled)) then
    Self.B_Track_DelClick(Self);
end;

procedure TF_BlkPst.LV_TurnoutsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Turnout_Del.Enabled := (Self.LV_Turnouts.Selected <> nil);

  if (Self.CB_Turnout.Enabled) then
  begin
    Self.CB_Turnout.ItemIndex := -1;
    if (Self.LV_Turnouts.Selected <> nil) then
    begin
      var id := StrToInt(Self.LV_Turnouts.Selected.SubItems[0]);
      for var i := 0 to Self.CB_TurnoutItems.Count-1 do
        if (Self.CB_TurnoutItems[i] = id) then
          Self.CB_Turnout.ItemIndex := i;
    end;
  end;
end;

procedure TF_BlkPst.LV_TurnoutsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Turnout_Del.Enabled)) then
    Self.B_Turnout_DelClick(Self);
end;

function TF_BlkPst.BlockPresent(id: Integer; LV: TListView): Boolean;
begin
  for var item: TListItem in LV.Items do
    if (IntToStr(id) = item.SubItems[0]) then
      Exit(true);
  Result := false;
end;

procedure TF_BlkPst.FillBlockLI(var LI: TListItem; blockId: Integer);
begin
  LI.Caption := IntToStr(LI.Index);
  LI.SubItems.Clear();
  LI.SubItems.Add(IntToStr(blockId));
  LI.SubItems.Add(Blocks.GetBlkName(blockId));
end;

procedure TF_BlkPst.FillRefugeeLI(var LI: TListItem; blockId: Integer; pos: string);
begin
  Self.FillBlockLI(LI, blockId);
  LI.SubItems.Add(pos);
end;

end.
