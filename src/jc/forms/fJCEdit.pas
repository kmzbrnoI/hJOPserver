unit fJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin, TechnologieJC, Generics.Collections, BlockDb,
  StrUtils, Math, Area, fTrainSpeed;

type
  TF_JCEdit = class(TForm)
    L_VC_01: TLabel;
    E_Name: TEdit;
    GB_Turnouts: TGroupBox;
    LV_Turnouts: TListView;
    GB_Turnout_New: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_Turnout: TComboBox;
    CB_Turnout_Pos: TComboBox;
    B_Turnout_Ok: TButton;
    GB_Tracks: TGroupBox;
    GB_Track_New: TGroupBox;
    CB_Track: TComboBox;
    B_Track_Ok: TButton;
    LV_Tracks: TListView;
    B_Save: TButton;
    B_Storno: TButton;
    L_VC_02: TLabel;
    CB_Signal: TComboBox;
    CB_Typ: TComboBox;
    L_VC_11: TLabel;
    CHB_AutoName: TCheckBox;
    GB_Railway: TGroupBox;
    CHB_Railway: TCheckBox;
    Label1: TLabel;
    CB_Railway: TComboBox;
    Label2: TLabel;
    CB_Railway_Dir: TComboBox;
    Label3: TLabel;
    SE_ID: TSpinEdit;
    GB_Advanced: TGroupBox;
    Label4: TLabel;
    M_Crossings: TMemo;
    E_VB: TEdit;
    Label8: TLabel;
    Label6: TLabel;
    SE_SignalFallTrackI: TSpinEdit;
    B_Track_Del: TButton;
    B_Turnout_Del: TButton;
    GB_Speeds: TGroupBox;
    GB_SpeedsStop: TGroupBox;
    GB_SpeedsGo: TGroupBox;
    GB_Signal: TGroupBox;
    Label9: TLabel;
    CB_Signal_Signal: TComboBox;
    CHB_Odbocka: TCheckBox;
    CHB_NZV: TCheckBox;
    L_VC_07: TLabel;
    CB_Next_Signal: TComboBox;
    GB_Locks: TGroupBox;
    LV_Locks: TListView;
    GB_Lock: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    CB_Lock: TComboBox;
    CB_Lock_Ref: TComboBox;
    B_Lock_Ok: TButton;
    B_Lock_Del: TButton;
    GB_Refugees: TGroupBox;
    LV_Refugees: TListView;
    GB_Refugee: TGroupBox;
    Label7: TLabel;
    Label14: TLabel;
    CB_Refugee: TComboBox;
    CB_Refugee_Ref: TComboBox;
    B_Refugee_Ok: TButton;
    B_Refugee_Del: TButton;
    Label15: TLabel;
    CB_Refugee_Pos: TComboBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_Turnout_OkClick(Sender: TObject);
    procedure B_Track_OkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_SaveClick(Sender: TObject);
    procedure B_Track_DelClick(Sender: TObject);
    procedure B_Turnout_DelClick(Sender: TObject);
    procedure LV_TurnoutsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LV_TracksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure CB_SignalChange(Sender: TObject);
    procedure CB_TypChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CHB_RailwayClick(Sender: TObject);
    procedure LV_TracksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LV_TurnoutsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CB_RailwayChange(Sender: TObject);
    procedure CB_Next_SignalChange(Sender: TObject);
    procedure B_Lock_OkClick(Sender: TObject);
    procedure LV_LocksChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_LocksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure B_Lock_DelClick(Sender: TObject);
    procedure B_Refugee_DelClick(Sender: TObject);
    procedure B_Refugee_OkClick(Sender: TObject);
    procedure LV_RefugeesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_RefugeesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    OpenIndex: Integer;
    mNewJC: Boolean;
    CB_SignalIds: TList<Integer>;
    CB_NextSignalIds: TList<Integer>;
    CB_TrackIds: TList<Integer>;
    CB_TurnoutIds: TList<Integer>;
    CB_LockIds: TList<Integer>;
    CB_RailwayIds: TList<Integer>;
    JCData: TJCdata;

    fTrainSpeedGo: TF_TrainSpeed;
    fTrainSpeedStop: TF_TrainSpeed;

    procedure EmptyOpenForm();
    procedure EditOpenForm();
    procedure CommonOpenForm();

    procedure UpdateJCName();
    procedure UpdateNextSignal();
    procedure UpdateTurnoutsFromTracks();
    procedure FillBlocksCB();
    function LVFindLine(const LV: TListView; subitemi: Integer; content: string): Integer;
    function IsAnyTurnoutMinus(): Boolean;

    function Areas(): TList<TArea>;
    procedure FillBlockLI(var LI: TListItem; blockId: Integer);
    procedure FillTurnoutLI(var LI: TListItem; blockId: Integer; pos: string);
    procedure FillRefugeeLI(var LI: TListItem; blockId: Integer; pos: string; refId: Integer);
    procedure FillRefLI(var LI: TListItem; blockId: Integer; refId: Integer);
    procedure FillRefTracks();
    function BlockPresent(id: Integer; LV: TListView): Boolean;
    procedure SetBlocksCbItemIndex(var cb: TComboBox; ids: TList<Integer>; id: Integer);

  public
    procedure EditJC(JCIndex: Integer);
    procedure NewJC(templateIndex: Integer);

  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, Block, AreaDb, TrainSpeed,
  BlockSignal, TJCDatabase, DataJC, BlockRailway, BlockTurnout;

{$R *.dfm}

procedure TF_JCEdit.FormCreate(Sender: TObject);
begin
  Self.CB_SignalIds := TList<Integer>.Create();
  Self.CB_NextSignalIds := TList<Integer>.Create();
  Self.CB_RailwayIds := TList<Integer>.Create();
  Self.CB_TurnoutIds := TList<Integer>.Create();
  Self.CB_TrackIds := TList<Integer>.Create();
  Self.CB_LockIds := TList<Integer>.Create();

  Self.fTrainSpeedGo := TF_TrainSpeed.Create(nil);
  Self.fTrainSpeedGo.Parent := Self.GB_SpeedsGo;
  Self.fTrainSpeedGo.Align := alClient;
  Self.fTrainSpeedGo.Show();

  Self.fTrainSpeedStop := TF_TrainSpeed.Create(nil);
  Self.fTrainSpeedStop.Parent := Self.GB_SpeedsStop;
  Self.fTrainSpeedStop.Align := alClient;
  Self.fTrainSpeedStop.Show();
end;

procedure TF_JCEdit.FormDestroy(Sender: TObject);
begin
  Self.CB_SignalIds.Free();
  Self.CB_NextSignalIds.Free();
  Self.CB_RailwayIds.Free();
  Self.CB_TurnoutIds.Free();
  Self.CB_TrackIds.Free();
  Self.CB_LockIds.Free();

  Self.fTrainSpeedGo.Free();
  Self.fTrainSpeedStop.Free();
end;

procedure TF_JCEdit.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_JCEdit.EmptyOpenForm();
begin
  Self.OpenIndex := -1;

  Self.JCData.id := -1;
  Self.JCData.signalCode := Integer(ncPosunZaj);
  Self.JCData.railwayId := -1;

  Self.JCData.turnouts := nil;
  Self.JCData.tracks := nil;
  Self.JCData.refuges := nil;
  Self.JCData.crossings := nil;
  Self.JCData.vb := nil;
  Self.JCData.locks := nil;
  Self.JCData.speedsGo := nil;
  Self.JCData.speedsStop := nil;

  Self.E_Name.Text := '';
  if (JCDb.Count > 0) then
    SE_ID.Value := JCDb[JCDb.Count - 1].id
  else
    SE_ID.Value := 1;
  Blocks.FillCB(Self.CB_Signal, Self.CB_SignalIds, nil, nil, btSignal);

  Self.CB_Typ.ItemIndex := -1;

  Self.CB_Signal_Signal.Clear();
  Self.CB_Signal_Signal.Enabled := false;

  Self.CB_Next_Signal.ItemIndex := -1;

  Self.fTrainSpeedGo.Clear();
  Self.fTrainSpeedStop.Default();

  Self.CB_SignalChange(Self);
  Self.Caption := 'Vytvořit novou jízdní cestu';
  Self.LV_Tracks.Clear();
  Self.FillBlocksCB();

  Self.M_Crossings.Clear();
  Self.E_VB.Text := '';
  Self.SE_SignalFallTrackI.Value := 0;
  Self.SE_SignalFallTrackI.MaxValue := 0;
  Self.CHB_Odbocka.Checked := false;
  Self.CHB_NZV.Checked := false;

  Self.CHB_Railway.Checked := false;
  Self.CHB_RailwayClick(Self.CHB_Railway);
end;

procedure TF_JCEdit.EditOpenForm();
begin
  Self.JCData := JCDb.GetJCByIndex(OpenIndex).data;

  Blocks.FillCB(Self.CB_Signal, Self.CB_SignalIds, nil, nil, btSignal, btAny, JCData.signalId);
  Self.CB_SignalChange(Self);

  Self.E_Name.Text := JCData.name;
  Self.SE_ID.Value := JCData.id;

  Self.CB_Typ.ItemIndex := Integer(JCData.typ) - 1;

  // Filling of fTrainSpeedGo & fTrainSpeedGo done in CB_TypChange
  Self.CB_TypChange(Self.CB_Typ);

  Self.CHB_Railway.Checked := (JCData.railwayId > -1);
  Self.CHB_RailwayClick(Self.CHB_Railway);

  for var trackId in JCData.tracks do
  begin
    var LI := Self.LV_Tracks.Items.Add();
    Self.FillBlockLI(LI, trackId);
  end;
  Self.FillRefTracks();

  for var turnoutZav in JCData.turnouts do
  begin
    var LI := Self.LV_Turnouts.Items.Add();
    Self.FillTurnoutLI(LI, turnoutZav.Block, TBlkTurnout.PositionToStr(turnoutZav.position));
  end;

  Self.CHB_Odbocka.Checked := JCData.turn;
  Self.CHB_NZV.Checked := JCData.nzv;

  Self.M_Crossings.Clear();
  for var crossingZav in JCData.crossings do
  begin
    var tmp := IntToStr(crossingZav.crossingId);
    if (crossingZav.openTrack <> -1) then
    begin
      tmp := tmp + ', ' + IntToStr(crossingZav.openTrack);
      for var blokid in crossingZav.closeTracks do
        tmp := tmp + ', ' + IntToStr(blokid);
    end;

    Self.M_Crossings.Lines.Add(tmp);
  end;

  for var refugeeZav in JCData.refuges do
  begin
    var LI := Self.LV_Refugees.Items.Add();
    Self.FillRefugeeLI(LI, refugeeZav.block, TBlkTurnout.PositionToStr(refugeeZav.position), refugeeZav.ref_blk);
  end;

  for var lockZav in JCData.locks do
  begin
    var LI := Self.LV_Locks.Items.Add();
    Self.FillRefLI(LI, lockZav.block, lockZav.ref_blk);
  end;

  Self.E_VB.Text := '';
  for var vb in JCData.vb do
    Self.E_VB.Text := Self.E_VB.Text + IntToStr(vb) + ', ';
  Self.E_VB.Text := LeftStr(Self.E_VB.Text, Length(Self.E_VB.Text) - 2);

  Self.SE_SignalFallTrackI.MaxValue := Max(JCData.tracks.Count - 1, JCData.signalFallTrackI);
  Self.SE_SignalFallTrackI.Value := JCData.signalFallTrackI;

  Self.CB_SignalChange(Self);
  if (Self.mNewJC) then
    Self.Caption := 'Nová jízdní cesta'
  else
    Self.Caption := 'Jízdní cesta ' + JCData.name;
end;

procedure TF_JCEdit.CommonOpenForm();
begin
  Self.LV_Turnouts.Clear();
  Self.LV_Tracks.Clear();
  Self.LV_Locks.Clear();
  Self.LV_Refugees.Clear();
  Self.FillRefTracks(); // will clear ref tracks
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_JCEdit.B_Turnout_OkClick(Sender: TObject);
begin
  if (not Self.CB_Turnout.Enabled) then
    Exit();

  if (Self.CB_Turnout.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte výhybku!', 'Nelze přídat výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Turnout_Pos.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte polohu výhybky!', 'Nelze přidat výhybku', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var updateSide := (Self.CHB_Odbocka.Checked = Self.IsAnyTurnoutMinus());
  var turnoutId := Self.CB_TurnoutIds[Self.CB_Turnout.ItemIndex];
  var turnoutIndex := Self.LVFindLine(Self.LV_Turnouts, 0, IntToStr(turnoutId));
  var LI: TListItem;
  if (turnoutIndex > -1) then
    LI := Self.LV_Turnouts.Items[turnoutIndex]
  else
    LI := Self.LV_Turnouts.Items.Add();

  Self.FillTurnoutLI(LI, turnoutId, Self.CB_Turnout_Pos.Text);
  if (updateSide) then
    Self.CHB_Odbocka.Checked := Self.IsAnyTurnoutMinus();
end;

procedure TF_JCEdit.B_Track_OkClick(Sender: TObject);
begin
  if (Self.CB_Track.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte úsek!', 'Nelze přidat úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var id := Self.CB_TrackIds[Self.CB_Track.ItemIndex];
  if ((Self.LV_Tracks.Selected = nil) and (Self.BlockPresent(id, Self.LV_Tracks))) then
  begin
    Application.MessageBox('Nelze přidat duplicitní úsek!', 'Nelze přidat/upravit úsek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var LI: TListItem;
  if (Self.LV_Tracks.Selected = nil) then
    LI := Self.LV_Tracks.Items.Add()
  else
    LI := Self.LV_Tracks.Selected;

  Self.FillBlockLI(LI, id);

  if (Self.CHB_AutoName.Checked) then
    Self.UpdateJCName();
  Self.UpdateNextSignal();
  Self.UpdateTurnoutsFromTracks();
  Self.SE_SignalFallTrackI.MaxValue := Max(Self.LV_Tracks.Items.Count, Self.SE_SignalFallTrackI.Value);
  Self.FillRefTracks();
end;

procedure TF_JCEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Self.OpenIndex := -1;
  Self.mNewJC := false;
  CanClose := true;
end;

procedure TF_JCEdit.EditJC(JCIndex: Integer);
begin
  Self.OpenIndex := JCIndex;

  Self.CommonOpenForm();
  if (JCIndex = -1) then
    Self.EmptyOpenForm()
  else
    Self.EditOpenForm();

  if (mNewJC) then
    Self.SE_ID.Value := Self.SE_ID.Value + 1;
  Self.ShowModal();
end;

procedure TF_JCEdit.NewJC(templateIndex: Integer);
begin
  Self.mNewJC := true;
  Self.EditJC(templateIndex);
end;

procedure TF_JCEdit.B_Lock_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané zámky z jízdní cesty?'), 'Mazání zámků',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.LV_Locks.DeleteSelected();
end;

procedure TF_JCEdit.B_Lock_OkClick(Sender: TObject);
begin
  if ((not Self.CB_Lock.Enabled) or (Self.CB_Lock.ItemIndex = -1)) then
  begin
    Application.MessageBox('Vyberte zámek!', 'Nelze přídat zámek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Lock_Ref.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte referenční úsek!', 'Nelze přidat zámek', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var lockId := Self.CB_LockIds[Self.CB_Lock.ItemIndex];
  var lockIndex := Self.LVFindLine(Self.LV_Locks, 0, IntToStr(lockId));
  var LI: TListItem;
  if (lockIndex > -1) then
    LI := Self.LV_Locks.Items[lockIndex]
  else
    LI := Self.LV_Locks.Items.Add();

  Self.FillRefLI(LI, lockId, StrToInt(Self.LV_Tracks.Items[Self.CB_Lock_Ref.ItemIndex].SubItems.Strings[0]));
end;

procedure TF_JCEdit.B_Refugee_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané odvraty z jízdní cesty?'), 'Mazání odvratů',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.LV_Refugees.DeleteSelected();
end;

procedure TF_JCEdit.B_Refugee_OkClick(Sender: TObject);
begin
  if ((not Self.CB_Refugee.Enabled) or (Self.CB_Refugee.ItemIndex = -1)) then
  begin
    Application.MessageBox('Vyberte odvrat!', 'Nelze přídat odvrat', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Refugee_Pos.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte polohu odvratu!', 'Nelze přidat odvrat', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Refugee_Ref.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte referenční úsek!', 'Nelze přidat odvrat', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var refugeeId := Self.CB_TurnoutIds[Self.CB_Refugee.ItemIndex];
  var refugeeIndex := Self.LVFindLine(Self.LV_Refugees, 0, IntToStr(refugeeId));
  var LI: TListItem;
  if (refugeeIndex > -1) then
    LI := Self.LV_Refugees.Items[refugeeIndex]
  else
    LI := Self.LV_Refugees.Items.Add();

  Self.FillRefugeeLI(LI, refugeeId, Self.CB_Refugee_Pos.Text, StrToInt(Self.LV_Tracks.Items[Self.CB_Refugee_Ref.ItemIndex].SubItems.Strings[0]));
end;

procedure TF_JCEdit.B_SaveClick(Sender: TObject);
label Fail;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (JCDb.IsJC(Self.SE_ID.Value, Self.OpenIndex)) then
  begin
    Application.MessageBox('JC s tímto ID již existuje!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Signal.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte návestidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Typ.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte typ jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Signal_Signal.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte návěst!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Next_Signal.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte další návěstidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CHB_Railway.Checked) and (Self.CB_Railway.ItemIndex < 0) then
  begin
    Application.MessageBox('Vyberte trať!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CHB_Railway.Checked) and (Self.CB_Railway_Dir.ItemIndex < 0) then
  begin
    Application.MessageBox('Vyberte směr trati!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  for var LI: TListItem in Self.LV_Turnouts.Items do
  begin
    if ((LI.SubItems.Count < 3) or ((LI.SubItems.Strings[2] <> '+') and (LI.SubItems.Strings[2] <> '-'))) then
    begin
      Application.MessageBox('Je třeba vybrat polohy všech výhybek!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if (Self.SE_SignalFallTrackI.Value >= Self.LV_Tracks.Items.Count) then
  begin
    Application.MessageBox('Číslo úseku, při jehož obsazení se má zrušit návěst, přesahuje rozmezi 0..počet úseků-1!',
      'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var JCsaveData: TJCdata := TechnologieJC.NewJCData();

  try
    try
      JCsaveData.speedsGo := Self.fTrainSpeedGo.Get();
      JCsaveData.speedsStop := Self.fTrainSpeedStop.Get();
    except
      on E:Exception do
      begin
        TechnologieJC.FreeJCData(JCsaveData);
        Application.MessageBox(PChar('Rychlosti:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;

    JCsaveData.name := Self.E_Name.Text;
    JCsaveData.id := Self.SE_ID.Value;
    JCsaveData.signalId := Self.CB_SignalIds[Self.CB_Signal.ItemIndex];
    JCsaveData.typ := TJCType(Self.CB_Typ.ItemIndex + 1);

    if (JCsaveData.typ = TJCType.shunt) then begin
      case (Self.CB_Signal_Signal.ItemIndex) of
       0: JCsaveData.signalCode := Integer(ncPosunZaj);
       1: JCsaveData.signalCode := Integer(ncPosunNezaj);
      end;
    end else
      JCsaveData.signalCode := Integer(ncDisabled);

    JCsaveData.turn := Self.CHB_Odbocka.Checked;
    JCsaveData.nzv := Self.CHB_NZV.Checked;
    JCsaveData.signalFallTrackI := Self.SE_SignalFallTrackI.Value;

    if (Self.CHB_Railway.Checked) then
    begin
      JCsaveData.railwayId := Self.CB_RailwayIds[Self.CB_Railway.ItemIndex];
      JCsaveData.railwayDir := TRailwayDirection(Self.CB_Railway_Dir.ItemIndex + 1);
    end else begin
      JCsaveData.railwayId := -1;
      JCsaveData.railwayDir := TRailwayDirection.no;
    end;

    Self.CB_Next_SignalChange(Self.CB_Next_Signal); // fills JCData.nextSinagl*
    JCsaveData.nextSignalType := JCData.nextSignalType;
    JCsaveData.nextSignalId := JCData.nextSignalId;

    // turnouts
    for var LI: TListItem in Self.LV_Turnouts.Items do
    begin
      var zav: TJCTurnoutZav;
      zav.Block := StrToInt(LI.SubItems.Strings[0]);
      zav.position := TBlkTurnout.StrToPosition(LI.SubItems.Strings[2]);
      JCsaveData.turnouts.Add(zav);
    end;

    // tracks
    for var LI: TListItem in Self.LV_Tracks.Items do
      JCsaveData.tracks.Add(StrToInt(LI.SubItems.Strings[0]));

    // locks
    for var LI: TListItem in Self.LV_Locks.Items do
    begin
      var zav: TJCRefZav;
      zav.block := StrToInt(LI.SubItems.Strings[0]);
      zav.ref_blk := StrToInt(LI.SubItems.Strings[2]);
      JCsaveData.locks.Add(zav);
    end;

    // refugees
    for var LI: TListItem in Self.LV_Refugees.Items do
    begin
      var zav: TJCRefugeeZav;
      zav.block := StrToInt(LI.SubItems.Strings[0]);
      zav.position := TBlkTurnout.StrToPosition(LI.SubItems.Strings[2]);
      zav.ref_blk := StrToInt(LI.SubItems.Strings[3]);
      JCsaveData.refuges.Add(zav);
    end;

    var parsed := TStringList.Create();
    try
      // crossings
      for var line in Self.M_Crossings.Lines do
      begin
        parsed.Clear();
        ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

        var crossingZav: TJCCrossingZav;
        try
          crossingZav.closeTracks := nil;
          crossingZav.crossingId := StrToInt(parsed[0]);
          if (parsed.Count > 1) then
            crossingZav.openTrack := StrToInt(parsed[1])
          else
            crossingZav.openTrack := -1;

          crossingZav.closeTracks := TList<Integer>.Create();
          for var i := 2 to parsed.Count - 1 do
            crossingZav.closeTracks.Add(StrToInt(parsed[i]));

          JCsaveData.crossings.Add(crossingZav);
        except
          on E: Exception do
          begin
            if (Assigned(crossingZav.closeTracks)) then
              crossingZav.closeTracks.Free();

            TechnologieJC.FreeJCData(JCsaveData);
            Application.MessageBox(PChar('Napodařilo se naparsovat přejezd "' + line + '":' + #13#10 + E.Message),
              'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;
      end;

      // variant points
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(Self.E_VB.Text, ' ', '', [rfReplaceAll])), parsed);
      for var item in parsed do
      begin
        try
          JCsaveData.vb.Add(StrToInt(Item));
        except
          on E: Exception do
          begin
            TechnologieJC.FreeJCData(JCsaveData);
            Application.MessageBox(PChar('Napodařilo se naparsovat variantní bod ' + Item + ':' + #13#10 + E.Message),
              'Chyba', MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;
      end;
    finally
      parsed.Free()
    end;

    if (mNewJC) then
    begin
      try
        JCDb.Add(JCsaveData);
      except
        on E: Exception do
        begin
          TechnologieJC.FreeJCData(JCsaveData);
          Application.MessageBox(PChar('Přidávání JC skončilo s chybou' + #13#10 + E.Message), 'Chyba',
            MB_OK OR MB_ICONWARNING);
          Exit();
        end;
      end;

    end else begin
      // update existing path
      var JC := JCDb.GetJCByIndex(OpenIndex);
      JC.data := JCsaveData;
      JCTableData.UpdateLine(JC.index);
    end;

  except
    on E:Exception do
    begin
      TechnologieJC.FreeJCData(JCsaveData);
      Application.MessageBox(PChar('Neočekávaná chyba:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_JCEdit.B_Track_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané úseky z JC?'), 'Mazání úseku',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.LV_Tracks.DeleteSelected();

  if (Self.CHB_AutoName.Checked) then
    Self.UpdateJCName();
  Self.UpdateNextSignal();
  Self.SE_SignalFallTrackI.MaxValue := Max(Self.LV_Tracks.Items.Count, Self.SE_SignalFallTrackI.Value);
  Self.FillRefTracks();
end;

procedure TF_JCEdit.B_Turnout_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané výhybky z JC?'), 'Mazání výhybek',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
    Self.LV_Turnouts.DeleteSelected();
end;

procedure TF_JCEdit.LV_TurnoutsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Turnout_Del.Enabled := (LV_Turnouts.ItemIndex <> -1);

  if (Self.LV_Turnouts.Selected = nil) then
  begin
    Self.CB_Turnout.ItemIndex := -1;
    Self.CB_Turnout_Pos.ItemIndex := -1;
    Exit();
  end;

  var blockId := StrToInt(Self.LV_Turnouts.Selected.SubItems.Strings[0]);
  Self.SetBlocksCbItemIndex(Self.CB_Turnout, Self.CB_TurnoutIds, blockId);

  var pos := '';
  if (Self.LV_Turnouts.Selected.SubItems.Count > 2) then
    pos := Self.LV_Turnouts.Selected.SubItems.Strings[2];

  if (pos = '+') then
    Self.CB_Turnout_Pos.ItemIndex := 0
  else if (pos = '-') then
    Self.CB_Turnout_Pos.ItemIndex := 1
  else
    Self.CB_Turnout_Pos.ItemIndex := -1;
end;

procedure TF_JCEdit.LV_TurnoutsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Turnout_Del.Enabled)) then
    B_Turnout_DelClick(B_Turnout_Del);
end;

procedure TF_JCEdit.LV_LocksChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Lock_Del.Enabled := (Self.LV_Locks.Selected <> nil);

  if (Self.LV_Locks.Selected = nil) then
  begin
    Self.CB_Lock.ItemIndex := -1;
    Self.CB_Lock_Ref.ItemIndex := -1;
    Exit();
  end;

  var blockId: Integer := StrToInt(Self.LV_Locks.Selected.SubItems.Strings[0]);
  Self.SetBlocksCbItemIndex(Self.CB_Lock, Self.CB_LockIds, blockId);

  var refId: Integer := -1;
  if (Self.LV_Locks.Selected.SubItems.Count > 2) then
    refId := StrToIntDef(Self.LV_Locks.Selected.SubItems.Strings[2], -1);
  Self.CB_Lock_Ref.ItemIndex := -1;
  for var i := 0 to Self.LV_Tracks.Items.Count - 1 do
    if (Self.LV_Tracks.Items[i].SubItems.Strings[0] = IntToStr(refId)) then
      Self.CB_Lock_Ref.ItemIndex := i;
end;

procedure TF_JCEdit.LV_LocksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Lock_Del.Enabled)) then
    Self.B_Lock_DelClick(Self.B_Lock_Del);
end;

procedure TF_JCEdit.LV_RefugeesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  Self.B_Refugee_Del.Enabled := (Self.LV_Refugees.Selected <> nil);

  if (Self.LV_Refugees.Selected = nil) then
  begin
    Self.CB_Refugee.ItemIndex := -1;
    Self.CB_Refugee_Pos.ItemIndex := -1;
    Self.CB_Refugee_Ref.ItemIndex := -1;
    Exit();
  end;

  var blockId: Integer := StrToInt(Self.LV_Refugees.Selected.SubItems.Strings[0]);
  Self.SetBlocksCbItemIndex(Self.CB_Refugee, Self.CB_TurnoutIds, blockId);

  var pos := '';
  if (Self.LV_Refugees.Selected.SubItems.Count > 2) then
    pos := Self.LV_Refugees.Selected.SubItems.Strings[2];
  if (pos = '+') then
    Self.CB_Refugee_Pos.ItemIndex := 0
  else if (pos = '-') then
    Self.CB_Refugee_Pos.ItemIndex := 1
  else
    Self.CB_Refugee_Pos.ItemIndex := -1;

  var refId: Integer := -1;
  if (Self.LV_Refugees.Selected.SubItems.Count > 3) then
    refId := StrToIntDef(Self.LV_Refugees.Selected.SubItems.Strings[3], -1);

  Self.CB_Refugee_Ref.ItemIndex := -1;
  for var i := 0 to Self.LV_Tracks.Items.Count - 1 do
    if (Self.LV_Tracks.Items[i].SubItems.Strings[0] = IntToStr(refId)) then
      Self.CB_Refugee_Ref.ItemIndex := i;
end;

procedure TF_JCEdit.LV_RefugeesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Refugee_Del.Enabled)) then
    Self.B_Refugee_DelClick(Self.B_Refugee_Del);
end;

procedure TF_JCEdit.LV_TracksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Track_Del.Enabled := (LV_Tracks.ItemIndex <> -1);

  if (Self.LV_Tracks.Selected = nil) then
  begin
    Self.CB_Track.ItemIndex := -1;
    Exit();
  end;

  var blockId := StrToInt(Self.LV_Tracks.Selected.SubItems.Strings[0]);
  Self.SetBlocksCbItemIndex(Self.CB_Track, Self.CB_TrackIds, blockId);
end;

procedure TF_JCEdit.LV_TracksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((Key = VK_DELETE) and (Self.B_Track_Del.Enabled)) then
    B_Track_DelClick(B_Track_Del);
end;

procedure TF_JCEdit.CB_Next_SignalChange(Sender: TObject);
begin
  if (Self.CB_Next_Signal.ItemIndex = 0) then
  begin
    JCData.nextSignalType := TJCNextSignalType.no;
  end else if (Self.CB_Next_Signal.ItemIndex = 1) then
  begin
    JCData.nextSignalType := TJCNextSignalType.railway;
  end else begin
    JCData.nextSignalType := TJCNextSignalType.signal;
    JCData.nextSignalId := Self.CB_NextSignalIds[Self.CB_Next_Signal.ItemIndex - 2];
  end;
end;

procedure TF_JCEdit.CB_SignalChange(Sender: TObject);
begin
  if (Self.CB_Signal.ItemIndex <> -1) then
  begin
    JCData.signalId := Self.CB_SignalIds[Self.CB_Signal.ItemIndex];
    var signal: TBlkSignal := TBlkSignal(Blocks.GetBlkByID(JCData.signalId));

    Self.FillBlocksCB();

    if ((Self.mNewJC) and (Self.OpenIndex = -1)) then
    begin
      case (signal.SymbolType) of
        TBlkSignalSymbol.main:
          Self.CB_Typ.ItemIndex := 0;
        TBlkSignalSymbol.shunting:
          Self.CB_Typ.ItemIndex := 1;
      end;
      Self.CB_TypChange(Self);
    end;
  end;

  if (Self.CHB_AutoName.Checked) then
    Self.UpdateJCName();
  Self.UpdateNextSignal();
end;

procedure TF_JCEdit.CB_RailwayChange(Sender: TObject);
begin
  if (Self.CHB_Railway.Checked) then
    Self.JCData.railwayId := Self.CB_RailwayIds[Self.CB_Railway.ItemIndex];
  Self.UpdateNextSignal();
end;

procedure TF_JCEdit.CB_TypChange(Sender: TObject);
begin
  Self.JCData.typ := TJCType(Self.CB_Typ.ItemIndex + 1);

  Self.fTrainSpeedGo.LV_Speeds.Enabled := (Self.JCData.typ = TJCType.train);
  Self.fTrainSpeedStop.LV_Speeds.Enabled := (Self.JCData.typ = TJCType.train);
  if (Self.JCData.typ = TJCType.shunt) then
  begin
    Self.fTrainSpeedGo.Clear();
    Self.fTrainSpeedStop.Clear();
  end else begin
    if ((Self.JCData.speedsGo <> nil) and (Self.JCData.speedsStop <> nil)) then
    begin
      Self.fTrainSpeedGo.Fill(Self.JCData.speedsGo);
      Self.fTrainSpeedStop.Fill(Self.JCData.speedsStop);
    end;
  end;

  Self.CB_Signal_Signal.Clear();
  Self.CB_Signal_Signal.Enabled := (Self.JCData.typ = TJCType.shunt);
  if (Self.JCData.typ = TJCType.train) then begin
    Self.CB_Signal_Signal.Items.Add('automaticky');
    Self.CB_Signal_Signal.ItemIndex := 0;
  end else begin
    Self.CB_Signal_Signal.Items.Add('Dovolen zajištěný posun');
    Self.CB_Signal_Signal.Items.Add('Dovolen nezajištěný posun');

    case (JCData.signalCode) of
      Integer(ncPosunZaj): Self.CB_Signal_Signal.ItemIndex := 0;
      Integer(ncPosunNezaj): Self.CB_Signal_Signal.ItemIndex := 1;
    else
      Self.CB_Signal_Signal.ItemIndex := 1;
    end;
  end;
end;

procedure TF_JCEdit.CHB_RailwayClick(Sender: TObject);
begin
  Self.CB_Railway.Enabled := Self.CHB_Railway.Checked;
  Self.CB_Railway_Dir.Enabled := Self.CHB_Railway.Checked;

  if (Self.CHB_Railway.Checked) then
  begin
    var areas := Self.Areas();
    try
      Blocks.FillCB(Self.CB_Railway, Self.CB_RailwayIds, nil, areas, btRailway, btAny, Self.JCData.railwayId);
      Self.CB_Railway_Dir.ItemIndex := Integer(Self.JCData.railwayDir) - 1;
    finally
      areas.Free();
    end;
  end else begin
    Self.CB_Railway.ItemIndex := -1;
    Self.CB_Railway_Dir.ItemIndex := -1;
  end;
end;

function TF_JCEdit.Areas(): TList<TArea>;
begin
  Result := TList<TArea>.Create();
  var signal := TBlkSignal(Blocks.GetBlkByID(JCData.signalId));
  if ((signal <> nil) and (signal.typ = btSignal)) then
    Result.AddRange(signal.areas);
end;

procedure TF_JCEdit.UpdateJCName();
begin
  if ((Self.CB_Signal.ItemIndex <> -1)) then
    Self.E_Name.Text := Blocks.GetBlkName(Self.JCData.signalId) + ' > ';

  if (Self.LV_Tracks.Items.Count <> 0) then
    Self.E_Name.Text := Self.E_Name.Text + Self.LV_Tracks.Items[Self.LV_Tracks.Items.Count - 1].SubItems.Strings[1];
end;

procedure TF_JCEdit.UpdateNextSignal();
begin
  Self.CB_Next_Signal.Clear();
  Self.CB_Next_Signal.Items.Add('Žádné návěstidlo');
  Self.CB_Next_Signal.Items.Add('Trať');

  if (JCData.signalId = -1) then
  begin
    Self.CB_Next_Signal.ItemIndex := -1;
    Self.CB_Next_Signal.Enabled := false;
    Exit();
  end;

  Self.CB_Next_Signal.Enabled := true;

  if (Self.LV_Tracks.Items.Count > 0) then
  begin
    var railway: TBlkRailway := nil;
    if (Self.JCData.railwayId > -1) then
      railway := TBlkRailway(Blocks.GetBlkByID(Self.JCData.railwayId));

    Self.CB_NextSignalIds.Clear();
    var lastTrackId := StrToInt(Self.LV_Tracks.Items[Self.LV_Tracks.Items.Count - 1].SubItems.Strings[0]);
    for var block in Blocks do
    begin
      if ((block.typ = btSignal) and ((TBlkSignal(block).track = nil) or
        (TBlkSignal(block).track.id = lastTrackId) or
        ((railway <> nil) and ((railway.HasAutoblokSignal(block)) or (block = railway.signalA) or (block = railway.signalB))))) then
      begin
        Self.CB_Next_Signal.Items.Add(block.name);
        Self.CB_NextSignalIds.Add(block.id);
        if (block.id = JCData.nextSignalId) then
          Self.CB_Next_Signal.ItemIndex := Self.CB_Next_Signal.Items.Count - 1;
      end;
    end;

    if (Self.CB_Next_Signal.ItemIndex = -1) then
    begin
      var signal := TBlkSignal(Blocks.GetBlkByID(JCData.nextSignalId));
      if (signal <> nil) then
      begin
        Self.CB_Next_Signal.Items.Add(signal.name);
        Self.CB_NextSignalIds.Add(JCData.nextSignalId);
        Self.CB_Next_Signal.ItemIndex := Self.CB_Next_Signal.Items.Count - 1;
      end;
    end;
  end else begin

    var areas := Self.Areas();
    var ignore := TList<Integer>.Create();
    try
      ignore.Add(Self.JCData.signalId);
      Blocks.FillCB(Self.CB_Next_Signal, Self.CB_NextSignalIds, ignore, areas, btSignal, btAny, JCData.nextSignalId);
    finally
      areas.Free();
      ignore.Free();
    end;
    Self.CB_Next_Signal.Items.Insert(0, 'Žádné návěstidlo');
    Self.CB_Next_Signal.Items.Insert(1, 'Trať');
  end;

  if (JCData.nextSignalType = TJCNextSignalType.no) then
    Self.CB_Next_Signal.ItemIndex := 0
  else if (JCData.nextSignalType = TJCNextSignalType.railway) then
    Self.CB_Next_Signal.ItemIndex := 1;
end;

procedure TF_JCEdit.UpdateTurnoutsFromTracks();
var toAdd: TList<Integer>;
begin
  toAdd := TList<Integer>.Create();
  try
    for var blk in Blocks do
    begin
      if (blk.typ <> btTurnout) then
        continue;
      if ((Self.BlockPresent(TBlkTurnout(blk).trackID, Self.LV_Tracks)) and
          (not Self.BlockPresent(blk.id, Self.LV_Turnouts))) then
        toAdd.Add(blk.id);
    end;

    for var blkid in toAdd do
    begin
      var LI := Self.LV_Turnouts.Items.Add();
      Self.FillTurnoutLI(LI, blkid, '');
    end;
  finally
    toAdd.Free();
  end;
end;

function TF_JCEdit.LVFindLine(const LV: TListView; subitemi: Integer; content: string): Integer;
begin
  for var i := 0 to LV.Items.Count - 1 do
    if (LV.Items[i].SubItems.Strings[subitemi] = content) then
      Exit(i);
  Result := -1;
end;

function TF_JCEdit.IsAnyTurnoutMinus(): Boolean;
begin
  for var LI: TListItem in Self.LV_Turnouts.Items do
    if (LI.SubItems.Strings[2] = '-') then
      Exit(true);
  Result := false;
end;

procedure TF_JCEdit.FillBlockLI(var LI: TListItem; blockId: Integer);
begin
  LI.Caption := IntToStr(LI.Index+1);
  LI.SubItems.Clear();
  LI.SubItems.Add(IntToStr(blockId));
  LI.SubItems.Add(Blocks.GetBlkName(blockId));
end;

procedure TF_JCEdit.FillTurnoutLI(var LI: TListItem; blockId: Integer; pos: string);
begin
  Self.FillBlockLI(LI, blockId);
  LI.SubItems.Add(pos);
end;

procedure TF_JCEdit.FillRefLI(var LI: TListItem; blockId: Integer; refId: Integer);
begin
  Self.FillBlockLI(LI, blockId);
  LI.SubItems.Add(IntToStr(refId));
  LI.SubItems.Add(Blocks.GetBlkName(refId));
end;

procedure TF_JCEdit.FillRefugeeLI(var LI: TListItem; blockId: Integer; pos: string; refId: Integer);
begin
  Self.FillBlockLI(LI, blockId);
  LI.SubItems.Add(pos);
  LI.SubItems.Add(IntToStr(refId));
  LI.SubItems.Add(Blocks.GetBlkName(refId));
end;

procedure TF_JCEdit.FillBlocksCB();
begin
  var areas := Self.Areas();
  try
    Blocks.FillCB(Self.CB_Refugee, Self.CB_TurnoutIds, nil, areas, btTurnout);
    Blocks.FillCB(Self.CB_Turnout, Self.CB_TurnoutIds, nil, areas, btTurnout);
    Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, areas, btTrack, btRT);
    Blocks.FillCB(Self.CB_Lock, Self.CB_LockIds, nil, areas, btLock);
  finally
    areas.Free();
  end;
end;

function TF_JCEdit.BlockPresent(id: Integer; LV: TListView): Boolean;
begin
  for var item: TListItem in LV.Items do
    if (IntToStr(id) = item.SubItems[0]) then
      Exit(true);
  Result := false;
end;

procedure TF_JCEdit.FillRefTracks();
begin
  Self.CB_Lock_Ref.Clear();
  Self.CB_Refugee_Ref.Clear();
  for var LI: TListItem in Self.LV_Tracks.Items do
  begin
    Self.CB_Lock_Ref.Items.Add(LI.SubItems.Strings[1]);
    Self.CB_Refugee_Ref.Items.Add(LI.SubItems.Strings[1]);
  end;
end;

procedure TF_JCEdit.SetBlocksCbItemIndex(var cb: TComboBox; ids: TList<Integer>; id: Integer);
begin
  cb.ItemIndex := -1;
  for var i := 0 to ids.Count - 1 do
    if (ids[i] = id) then
      cb.ItemIndex := i;
end;

end.
