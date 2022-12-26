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
    L_VC_07: TLabel;
    CB_Next_Signal: TComboBox;
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
    Label5: TLabel;
    M_Refugees: TMemo;
    Label7: TLabel;
    E_VB: TEdit;
    Label8: TLabel;
    CHB_Advanced: TCheckBox;
    M_Locks: TMemo;
    CHB_Odbocka: TCheckBox;
    CHB_NZV: TCheckBox;
    Label6: TLabel;
    SE_SignalFallTrackI: TSpinEdit;
    CB_Signal_Signal: TComboBox;
    Label9: TLabel;
    B_Track_Del: TButton;
    B_Turnout_Del: TButton;
    GB_Speeds: TGroupBox;
    GB_SpeedsStop: TGroupBox;
    GB_SpeedsGo: TGroupBox;
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
    procedure CHB_AdvancedClick(Sender: TObject);
    procedure LV_TracksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LV_TurnoutsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CB_RailwayChange(Sender: TObject);
    procedure CB_Next_SignalChange(Sender: TObject);
  private
    OpenIndex: Integer;
    mNewJC: Boolean;
    CB_SignalIds: TList<Integer>;
    CB_NextSignalIds: TList<Integer>;
    CB_TrackIds: TList<Integer>;
    CB_TurnoutIds: TList<Integer>;
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
    function TurnoutIndex(id: Integer): Integer;
    function IsAnyTurnoutMinus(): Boolean;

    function Areas(): TList<TArea>;
    procedure FillBlockLI(var LI: TListItem; blockId: Integer);
    procedure FillTurnoutLI(var LI: TListItem; blockId: Integer; pos: string);
    function BlockPresent(id: Integer; LV: TListView): Boolean;

  public
    procedure EditJC(JCIndex: Integer);
    procedure NewJC(templateIndex: Integer);

  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, FileSystem, Block, AreaDb, TrainSpeed,
  BlockSignal, TJCDatabase, DataJC, BlockRailway, BlockTurnout;

{$R *.dfm}

procedure TF_JCEdit.FormCreate(Sender: TObject);
begin
  Self.CB_SignalIds := TList<Integer>.Create();
  Self.CB_NextSignalIds := TList<Integer>.Create();
  Self.CB_RailwayIds := TList<Integer>.Create();
  Self.CB_TurnoutIds := TList<Integer>.Create();
  Self.CB_TrackIds := TList<Integer>.Create();

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

  Self.fTrainSpeedGo.Default();
  Self.fTrainSpeedStop.Default();

  Self.CB_SignalChange(Self);
  Self.Caption := 'Vytvořit novou jízdní cestu';
  Self.LV_Tracks.Clear();
  Self.FillBlocksCB();

  Self.M_Crossings.Clear();
  Self.M_Refugees.Clear();
  Self.M_Locks.Clear();
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

  Self.M_Refugees.Clear();
  for var refugee in JCData.refuges do
  begin
    var tmp := IntToStr(refugee.Block) + ', ';
    if (refugee.position = TTurnoutPosition.plus) then
      tmp := tmp + '+, '
    else
      tmp := tmp + '-, ';
    tmp := tmp + IntToStr(refugee.ref_blk);

    Self.M_Refugees.Lines.Add(tmp);
  end;

  Self.M_Locks.Clear();
  for var jcref in JCData.locks do
    Self.M_Locks.Lines.Add(IntToStr(jcref.Block) + ', ' + IntToStr(jcref.ref_blk));

  Self.E_VB.Text := '';
  for var vb in JCData.vb do
    Self.E_VB.Text := Self.E_VB.Text + IntToStr(vb) + ', ';
  Self.E_VB.Text := LeftStr(Self.E_VB.Text, Length(Self.E_VB.Text) - 2);

  Self.SE_SignalFallTrackI.MaxValue := Max(JCData.tracks.Count - 1, JCData.signalFallTrackI);
  Self.SE_SignalFallTrackI.Value := JCData.signalFallTrackI;

  Self.CB_SignalChange(Self);
  if (Self.mNewJC) then
    Self.Caption := 'Vytvořit novou jízdní cestu'
  else
    Self.Caption := 'Upravit jízdní cestu ' + JCData.name;
end;

procedure TF_JCEdit.CommonOpenForm;
begin
  Self.LV_Turnouts.Clear();
  Self.LV_Tracks.Clear();
  Self.CHB_AdvancedClick(Self.CHB_Advanced);
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
  var turnoutIndex := Self.TurnoutIndex(turnoutId);
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

    for var LI: TListItem in Self.LV_Turnouts.Items do
    begin
      var zav: TJCTurnoutZav;
      zav.Block := StrToInt(LI.SubItems.Strings[0]);
      zav.position := TBlkTurnout.StrToPosition(LI.SubItems.Strings[2]);
      JCsaveData.turnouts.Add(zav);
    end;

    for var LI: TListItem in Self.LV_Tracks.Items do
      JCsaveData.tracks.Add(StrToInt(LI.SubItems.Strings[0]));

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

      // refugees
      for var line in Self.M_Refugees.Lines do
      begin
        parsed.Clear();
        ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

        try
          var refugeeZav: TJCRefugeeZav;
          refugeeZav.Block := StrToInt(parsed[0]);
          if (parsed[1] = '+') then
            refugeeZav.position := TTurnoutPosition.plus
          else
            refugeeZav.position := TTurnoutPosition.minus;
          refugeeZav.ref_blk := StrToInt(parsed[2]);
          JCsaveData.refuges.Add(refugeeZav);
        except
          on E: Exception do
          begin
            TechnologieJC.FreeJCData(JCsaveData);
            Application.MessageBox(PChar('Napodařilo se naparsovat odvrat "' + line + '":' + #13#10 + E.Message), 'Chyba',
              MB_OK OR MB_ICONWARNING);
            Exit();
          end;
        end;
      end;

      // locks
      for var line in Self.M_Locks.Lines do
      begin
        parsed.Clear();
        ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

        try
          var refz: TJCRefZav;
          refz.Block := StrToInt(parsed[0]);
          refz.ref_blk := StrToInt(parsed[1]);
          JCsaveData.locks.Add(refz);
        except
          on E: Exception do
          begin
            TechnologieJC.FreeJCData(JCsaveData);
            Application.MessageBox(PChar('Napodařilo se naparsovat zámek ' + line + ':' + #13#10 + E.Message), 'Chyba',
              MB_OK OR MB_ICONWARNING);
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
  Self.B_Track_Del.Enabled := false;

  if (Self.LV_Turnouts.Selected = nil) then
  begin
    Self.CB_Turnout.ItemIndex := -1;
    Self.CB_Turnout_Pos.ItemIndex := -1;
    Exit();
  end;

  var blockId := StrToInt(Self.LV_Turnouts.Selected.SubItems.Strings[0]);
  var pos := '';
  if (Self.LV_Turnouts.Selected.SubItems.Count > 2) then
    pos := Self.LV_Turnouts.Selected.SubItems.Strings[2];

  Self.CB_Turnout.ItemIndex := -1;
  for var i := 0 to Self.CB_TurnoutIds.Count - 1 do
    if (Self.CB_TurnoutIds[i] = blockId) then
      Self.CB_Turnout.ItemIndex := i;

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

procedure TF_JCEdit.LV_TracksChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Track_Del.Enabled := (LV_Tracks.ItemIndex <> -1);
  Self.B_Turnout_Del.Enabled := false;

  if (Self.LV_Tracks.Selected = nil) then
  begin
    Self.CB_Track.ItemIndex := -1;
    Exit();
  end;

  var blockId := StrToInt(Self.LV_Tracks.Selected.SubItems.Strings[0]);

  Self.CB_Track.ItemIndex := -1;
  for var i := 0 to Self.CB_TrackIds.Count - 1 do
    if (Self.CB_TrackIds[i] = blockId) then
      Self.CB_Track.ItemIndex := i;
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
    Self.fTrainSpeedGo.Fill(Self.JCData.speedsGo);
    Self.fTrainSpeedStop.Fill(Self.JCData.speedsStop);
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

procedure TF_JCEdit.CHB_AdvancedClick(Sender: TObject);
var gb: TGroupBox;
begin
  if (Self.CHB_Advanced.Checked) then
    gb := Self.GB_Advanced
  else
    gb := Self.GB_Turnouts;

  Self.Width := gb.Left + gb.Width + 10;
  Self.B_Save.Left := gb.Left + gb.Width - Self.B_Save.Width;
  Self.B_Storno.Left := Self.B_Save.Left - Self.B_Storno.Width - 5;
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
        ((railway <> nil) and (railway.HasAutoblokSignal(block))))) then
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

function TF_JCEdit.TurnoutIndex(id: Integer): Integer;
begin
  for var i := 0 to Self.LV_Turnouts.Items.Count - 1 do
    if (StrToInt(Self.LV_Turnouts.Items[i].SubItems.Strings[0]) = id) then
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

procedure TF_JCEdit.FillBlocksCB();
begin
  var areas := Self.Areas();
  try
    Blocks.FillCB(Self.CB_Turnout, Self.CB_TurnoutIds, nil, areas, btTurnout);
    Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, areas, btTrack, btRT);
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

end.
