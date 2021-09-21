unit fJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin, TechnologieJC, Generics.Collections, BlockDb,
  StrUtils, Math, Area;

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
    B_Turnout_Del: TButton;
    B_Track_Del: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    L_VC_02: TLabel;
    CB_Signal: TComboBox;
    CB_Typ: TComboBox;
    L_VC_11: TLabel;
    L_VC_07: TLabel;
    CB_Next_Signal: TComboBox;
    L_VC_10: TLabel;
    L_VC_12: TLabel;
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
    SE_Speed_Stop: TSpinEdit;
    SE_Speed_Go: TSpinEdit;
    CB_Signal_Signal: TComboBox;
    Label9: TLabel;
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

    m_tracks: TList<Integer>;
    m_turnouts: TList<TJCTurnoutZav>;

    procedure EmptyOpenForm();
    procedure EditOpenForm();
    procedure CommonOpenForm();

    procedure UpdateJCName();
    procedure UpdateNextSignal();
    procedure UpdateTurnoutsFromTracks();
    procedure FillTurnouts();
    procedure FillTracks();
    function TurnoutIndex(id: Integer): Integer;
    function IsAnyTurnoutMinus(): Boolean;

    function Areas(): TList<TArea>;

  public
    procedure EditJC(JCIndex: Integer);
    procedure NewJC(templateIndex: Integer);

  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, FileSystem, Block, AreaDb,
  BlockSignal, TJCDatabase, DataJC, BlockRailway, BlockTurnout;

{$R *.dfm}

procedure TF_JCEdit.FormCreate(Sender: TObject);
begin
  Self.m_tracks := TList<Integer>.Create();
  Self.m_turnouts := TList<TJCTurnoutZav>.Create();
  Self.CB_SignalIds := TList<Integer>.Create();
  Self.CB_NextSignalIds := TList<Integer>.Create();
  Self.CB_RailwayIds := TList<Integer>.Create();
  Self.CB_TurnoutIds := TList<Integer>.Create();
  Self.CB_TrackIds := TList<Integer>.Create();
end;

procedure TF_JCEdit.FormDestroy(Sender: TObject);
begin
  Self.m_tracks.Free();
  Self.m_turnouts.Free();
  Self.CB_SignalIds.Free();
  Self.CB_NextSignalIds.Free();
  Self.CB_RailwayIds.Free();
  Self.CB_TurnoutIds.Free();
  Self.CB_TrackIds.Free();
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

  Self.m_tracks.Clear();
  Self.m_turnouts.Clear();

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
  Self.SE_Speed_Stop.Value := 40;
  Self.SE_Speed_Go.Value := 40;
  Self.CB_SignalChange(Self);
  Self.Caption := 'Vytvořit novou jízdní cestu';
  Self.LV_Tracks.Clear();
  Self.FillTurnouts();

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
  JCData := JCDb.GetJCByIndex(OpenIndex).data;

  Blocks.FillCB(Self.CB_Signal, Self.CB_SignalIds, nil, nil, btSignal, btAny, JCData.signalId);
  Self.CB_SignalChange(Self);

  Self.E_Name.Text := JCData.name;
  Self.SE_ID.Value := JCData.id;

  Self.CB_Typ.ItemIndex := Integer(JCData.typ) - 1;
  Self.SE_Speed_Go.Value := JCData.speedGo;
  Self.SE_Speed_Stop.Value := JCData.speedStop;

  Self.CB_TypChange(Self.CB_Typ);

  Self.CHB_Railway.Checked := (JCData.railwayId > -1);
  Self.CHB_RailwayClick(Self.CHB_Railway);

  Self.m_turnouts.Clear();
  Self.m_turnouts.AddRange(JCData.turnouts);
  Self.FillTurnouts();

  Self.m_tracks.Clear();
  Self.m_tracks.AddRange(JCData.tracks);
  Self.FillTracks();
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
  Self.CHB_AdvancedClick(Self.CHB_Advanced);
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_JCEdit.B_Turnout_OkClick(Sender: TObject);
begin
  if (Self.CB_Turnout.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte výhybku!', 'Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CB_Turnout_Pos.ItemIndex = -1) then
  begin
    Application.MessageBox('Vyberte polohu výhybky!', 'Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var updateSide := (Self.CHB_Odbocka.Checked = Self.IsAnyTurnoutMinus());

  var turnoutZav: TJCTurnoutZav;
  turnoutZav.Block := Self.CB_TurnoutIds[Self.CB_Turnout.ItemIndex];
  turnoutZav.position := TTurnoutPosition(Self.CB_Turnout_Pos.ItemIndex);

  var turnoutIndex := Self.TurnoutIndex(turnoutZav.Block);
  if (turnoutIndex > -1) then
    Self.m_turnouts[turnoutIndex] := turnoutZav
  else
    Self.m_turnouts.Add(turnoutZav);

  Self.FillTurnouts();
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

  if (Self.LV_Tracks.ItemIndex = -1) then
    Self.m_tracks.Add(Self.CB_TrackIds[Self.CB_Track.ItemIndex])
  else
    Self.m_tracks[Self.LV_Tracks.ItemIndex] := Self.CB_TrackIds[Self.CB_Track.ItemIndex];

  Self.FillTracks();
  if (Self.CHB_AutoName.Checked) then
    Self.UpdateJCName();
  Self.UpdateNextSignal();
  Self.UpdateTurnoutsFromTracks();
  Self.SE_SignalFallTrackI.MaxValue := Max(Self.m_tracks.Count, Self.SE_SignalFallTrackI.Value);
end;

procedure TF_JCEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  OpenIndex := -1;
  mNewJC := false;
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
  if (Self.SE_Speed_Go.Value = 0) then
  begin
    Application.MessageBox('Vyplňte, jaká bude rychlost lokomotivy při projiždění JC při postaveném dalším návěstidle!',
      'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.SE_Speed_Stop.Value = 0) then
  begin
    Application.MessageBox('Vyplňte, jaká bude rychlost lokomotivy při projiždění JC při dalším návěstidle na stůj!',
      'Nelze uložit data', MB_OK OR MB_ICONWARNING);
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

  for var turnoutZav in Self.m_turnouts do
  begin
    if ((turnoutZav.position <> TTurnoutPosition.plus) and ((turnoutZav.position <> TTurnoutPosition.minus))) then
    begin
      Application.MessageBox('Je třeba vybrat polohy všech výhybek!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if (Self.SE_SignalFallTrackI.Value >= Self.m_tracks.Count) then
  begin
    Application.MessageBox('Číslo úseku, při jehož obsazení se má zrušit návěst, přesahuje rozmezi 0..počet úseků-1!',
      'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  JCData.name := Self.E_Name.Text;
  JCData.id := Self.SE_ID.Value;
  JCData.signalId := Self.CB_SignalIds[Self.CB_Signal.ItemIndex];
  JCData.typ := TJCType(Self.CB_Typ.ItemIndex + 1);

  if (JCData.typ = TJCType.shunt) then begin
    case (Self.CB_Signal_Signal.ItemIndex) of
     0: JCData.signalCode := Integer(ncPosunZaj);
     1: JCData.signalCode := Integer(ncPosunNezaj);
    end;
  end else
    JCData.signalCode := Integer(ncDisabled);

  JCData.speedGo := Self.SE_Speed_Go.Value;
  JCData.speedStop := Self.SE_Speed_Stop.Value;
  JCData.turn := Self.CHB_Odbocka.Checked;
  JCData.nzv := Self.CHB_NZV.Checked;
  JCData.signalFallTrackI := Self.SE_SignalFallTrackI.Value;

  if (Self.CHB_Railway.Checked) then
  begin
    Self.JCData.railwayId := Self.CB_RailwayIds[Self.CB_Railway.ItemIndex];
    Self.JCData.railwayDir := TRailwayDirection(Self.CB_Railway_Dir.ItemIndex + 1);
  end else begin
    Self.JCData.railwayId := -1;
    Self.JCData.railwayDir := TRailwayDirection.no;
  end;

  Self.CB_Next_SignalChange(Self.CB_Next_Signal);

  if (not Assigned(JCData.turnouts) or (mNewJC)) then
    JCData.turnouts := TList<TJCTurnoutZav>.Create();
  JCData.turnouts.Clear();
  JCData.turnouts.AddRange(Self.m_turnouts);

  if (not Assigned(JCData.tracks) or (mNewJC)) then
    JCData.tracks := TList<Integer>.Create();
  JCData.tracks.Clear();
  JCData.tracks.AddRange(Self.m_tracks);

  var parsed := TStringList.Create();
  try
    // crossings
    if (not Assigned(JCData.crossings) or (mNewJC)) then
      JCData.crossings := TList<TJCCrossingZav>.Create();
    for var crossingZav in JCData.crossings do
      crossingZav.closeTracks.Free();
    JCData.crossings.Clear();
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

        JCData.crossings.Add(crossingZav);
      except
        on E: Exception do
        begin
          if (Assigned(crossingZav.closeTracks)) then
            crossingZav.closeTracks.Free();

          Application.MessageBox(PChar('Napodařilo se naparsovat přejezd "' + line + '":' + #13#10 + E.Message),
            'Chyba', MB_OK OR MB_ICONWARNING);
          Exit();
        end;
      end;
    end;

    // refugees
    if (not Assigned(JCData.refuges) or (mNewJC)) then
      JCData.refuges := TList<TJCRefugeeZav>.Create();
    JCData.refuges.Clear();
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
        JCData.refuges.Add(refugeeZav);
      except
        on E: Exception do
        begin
          Application.MessageBox(PChar('Napodařilo se naparsovat odvrat "' + line + '":' + #13#10 + E.Message), 'Chyba',
            MB_OK OR MB_ICONWARNING);
          Exit();
        end;
      end;
    end;

    // locks
    if (not Assigned(JCData.locks) or (mNewJC)) then
      JCData.locks := TList<TJCRefZav>.Create();
    JCData.locks.Clear();
    for var line in Self.M_Locks.Lines do
    begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        var refz: TJCRefZav;
        refz.Block := StrToInt(parsed[0]);
        refz.ref_blk := StrToInt(parsed[1]);
        JCData.locks.Add(refz);
      except
        on E: Exception do
        begin
          Application.MessageBox(PChar('Napodařilo se naparsovat zámek ' + line + ':' + #13#10 + E.Message), 'Chyba',
            MB_OK OR MB_ICONWARNING);
          Exit();
        end;
      end;
    end;

    // variant points
    if (not Assigned(JCData.vb) or (mNewJC)) then
      JCData.vb := TList<Integer>.Create();
    JCData.vb.Clear();
    parsed.Clear();
    ExtractStrings([','], [], PChar(StringReplace(Self.E_VB.Text, ' ', '', [rfReplaceAll])), parsed);

    for var item in parsed do
    begin
      try
        JCData.vb.Add(StrToInt(Item));
      except
        on E: Exception do
        begin
          Application.MessageBox(PChar('Napodařilo se naparsovat variatní bod ' + Item + ':' + #13#10 + E.Message),
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
      JCDb.Add(Self.JCData);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Přidávání JC skončilo s chybou' + #13#10 + E.Message), 'Chyba',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;

  end else begin
    // update existing path
    var JC := JCDb.GetJCByIndex(OpenIndex);
    JC.data := Self.JCData;
    JCTableData.UpdateLine(JC.index);
  end;

  Self.Close();
end;

procedure TF_JCEdit.B_Track_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané úseky z JC?'), 'Mazání úseku',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for var i := Self.LV_Tracks.Items.Count - 1 downto 0 do
      if (Self.LV_Tracks.Items[i].Selected) then
        Self.m_tracks.Delete(i);
    Self.FillTracks();
  end;

  if (Self.CHB_AutoName.Checked) then
    Self.UpdateJCName();
  Self.UpdateNextSignal();
  Self.SE_SignalFallTrackI.MaxValue := Max(Self.m_tracks.Count, Self.SE_SignalFallTrackI.Value);
end;

procedure TF_JCEdit.B_Turnout_DelClick(Sender: TObject);
begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané výhybky z JC?'), 'Mazání výhybek',
    MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
    for var i := Self.LV_Turnouts.Items.Count - 1 downto 0 do
      if (Self.LV_Turnouts.Items[i].Selected) then
        Self.m_turnouts.Delete(i);
    Self.FillTurnouts();
  end;
end;

procedure TF_JCEdit.LV_TurnoutsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.B_Turnout_Del.Enabled := (LV_Turnouts.ItemIndex <> -1);
  Self.B_Track_Del.Enabled := false;

  if ((Self.LV_Turnouts.Selected = nil) or (Self.LV_Turnouts.ItemIndex >= Self.m_turnouts.Count)) then
  begin
    Self.CB_Turnout.ItemIndex := -1;
    Self.CB_Turnout_Pos.ItemIndex := 0;
    Exit();
  end;

  var turnout := Self.m_turnouts[Self.LV_Turnouts.ItemIndex];

  for var i := 0 to Self.CB_TurnoutIds.Count - 1 do
    if (Self.CB_TurnoutIds[i] = turnout.Block) then
      Self.CB_Turnout.ItemIndex := i;

  case (turnout.position) of
    TTurnoutPosition.plus:
      Self.CB_Turnout_Pos.ItemIndex := 0;
    TTurnoutPosition.minus:
      Self.CB_Turnout_Pos.ItemIndex := 1;
  else
    Self.CB_Turnout_Pos.ItemIndex := -1;
  end;
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

  if ((Self.LV_Tracks.Selected = nil) or (Self.LV_Tracks.ItemIndex >= Self.m_tracks.Count)) then
  begin
    Self.CB_Track.ItemIndex := -1;
    Exit();
  end;

  for var i := 0 to Self.CB_TrackIds.Count - 1 do
    if (Self.CB_TrackIds[i] = Self.m_tracks[Self.LV_Tracks.ItemIndex]) then
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

    Self.FillTracks();
    Self.FillTurnouts();

    if (Self.mNewJC) then
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
  Self.SE_Speed_Go.Enabled := (JCData.typ <> TJCType.shunt);
  Self.SE_Speed_Stop.Enabled := (JCData.typ <> TJCType.shunt);

  if (JCData.typ = TJCType.shunt) then
  begin
    Self.SE_Speed_Go.Value := 40;
    Self.SE_Speed_Stop.Value := 40;
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

  if (Self.m_tracks.Count <> 0) then
    Self.E_Name.Text := Self.E_Name.Text + Blocks.GetBlkName(Self.m_tracks[Self.m_tracks.Count - 1]);
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

  if (Self.m_tracks.Count > 0) then
  begin
    var railway: TBlkRailway := nil;
    if (Self.JCData.railwayId > -1) then
      railway := TBlkRailway(Blocks.GetBlkByID(Self.JCData.railwayId));

    Self.CB_NextSignalIds.Clear();
    for var block in Blocks do
    begin
      if ((block.typ = btSignal) and ((TBlkSignal(block).track = nil) or
        (TBlkSignal(block).track.id = Self.m_tracks[Self.m_tracks.Count - 1]) or
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
      if (Self.m_tracks.Contains(TBlkTurnout(blk).trackID)) then
        toAdd.Add(blk.id);
    end;

    for var turnoutZav in Self.m_turnouts do
      if (toAdd.Contains(turnoutZav.Block)) then
        toAdd.Remove(turnoutZav.Block);

    for var blkid in toAdd do
    begin
      var turnoutZav: TJCTurnoutZav;
      turnoutZav.Block := blkid;
      turnoutZav.position := TTurnoutPosition.none;
      Self.m_turnouts.Add(turnoutZav);
    end;

    Self.FillTurnouts();
  finally
    toAdd.Free();
  end;
end;

procedure TF_JCEdit.FillTurnouts();
begin
  Self.LV_Turnouts.Clear();
  for var i := 0 to Self.m_turnouts.Count - 1 do
  begin
    var zaver := Self.m_turnouts[i];
    var LI: TListItem := LV_Turnouts.Items.Add();
    LI.Caption := IntToStr(i + 1);
    LI.SubItems.Add(Blocks.GetBlkName(zaver.Block));
    case (zaver.position) of
      TTurnoutPosition.plus:
        LI.SubItems.Add('+');
      TTurnoutPosition.minus:
        LI.SubItems.Add('-');
    else
      LI.SubItems.Add('?');
    end;
  end;

  var areas := Self.Areas();
  try
    Blocks.FillCB(Self.CB_Turnout, Self.CB_TurnoutIds, nil, areas, btTurnout);
  finally
    areas.Free();
  end;
end;

function TF_JCEdit.TurnoutIndex(id: Integer): Integer;
begin
  for var i := 0 to Self.m_turnouts.Count - 1 do
    if (Self.m_turnouts[i].Block = id) then
      Exit(i);
  Result := -1;
end;

procedure TF_JCEdit.FillTracks();
begin
  Self.LV_Tracks.Clear();
  for var i := 0 to Self.m_tracks.Count - 1 do
  begin
    var LI: TListItem := LV_Tracks.Items.Add();
    LI.Caption := IntToStr(i + 1);
    LI.SubItems.Add(Blocks.GetBlkName(Self.m_tracks[i]));
  end;

  var areas := Self.Areas();
  try
    Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, areas, btTrack, btRT);
  finally
    areas.Free();
  end;
end;

function TF_JCEdit.IsAnyTurnoutMinus(): Boolean;
begin
  for var turnoutZav in Self.m_turnouts do
    if (turnoutZav.position = TTurnoutPosition.minus) then
      Exit(true);
  Result := false;
end;

end.
