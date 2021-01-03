unit fJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin, TechnologieJC, Generics.Collections, BlockDb,
  StrUtils;

type
  TF_JCEdit = class(TForm)
    L_VC_01: TLabel;
    E_Name: TEdit;
    GB_ZaveryVyhybek: TGroupBox;
    LV_Vyhybky: TListView;
    CHB_NewZaver: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_NewZaverBlok: TComboBox;
    CB_NewZaverPoloha: TComboBox;
    B_Vyh_Add: TButton;
    GB_Useky: TGroupBox;
    CHB_NewBlok: TGroupBox;
    CB_NewUsek: TComboBox;
    B_Usek_Add: TButton;
    LV_Useky: TListView;
    B_Vyh_Del: TButton;
    B_Usek_Del: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    L_VC_02: TLabel;
    CB_Navestidlo: TComboBox;
    CB_Typ: TComboBox;
    L_VC_11: TLabel;
    L_VC_07: TLabel;
    CB_Dalsi_Nav: TComboBox;
    L_VC_10: TLabel;
    CB_Rychlost_Volno: TComboBox;
    CB_Rychlost_Stuj: TComboBox;
    L_VC_12: TLabel;
    CHB_AutoName: TCheckBox;
    GB_trat: TGroupBox;
    CHB_Trat: TCheckBox;
    Label1: TLabel;
    CB_TratBlok: TComboBox;
    Label2: TLabel;
    CB_TratSmer: TComboBox;
    Label3: TLabel;
    SE_ID: TSpinEdit;
    GB_Advanced: TGroupBox;
    Label4: TLabel;
    M_Prj: TMemo;
    Label5: TLabel;
    M_Odvraty: TMemo;
    Label7: TLabel;
    E_VB: TEdit;
    Label8: TLabel;
    CHB_Advanced: TCheckBox;
    M_Zamky: TMemo;
    CHB_Odbocka: TCheckBox;
    CHB_NZV: TCheckBox;
    Label6: TLabel;
    SE_SignalFallTrackI: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_Vyh_AddClick(Sender: TObject);
    procedure B_Usek_AddClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_SaveClick(Sender: TObject);
    procedure B_Usek_DelClick(Sender: TObject);
    procedure B_Vyh_DelClick(Sender: TObject);
    procedure LV_VyhybkyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_UsekyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CB_NavestidloChange(Sender: TObject);
    procedure CB_TypChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CHB_TratClick(Sender: TObject);
    procedure CHB_AdvancedClick(Sender: TObject);
    procedure LV_UsekyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LV_VyhybkyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CB_TratBlokChange(Sender: TObject);
    procedure CB_Dalsi_NavChange(Sender: TObject);
  private
   OpenIndex: Integer;
   mNewJC: Boolean;
   CB_NavestidloPolozky: TArI;
   CB_DalsiNavPolozky: TArI;
   CB_NewUsekPolozky: TArI;
   CB_NewVyhybkaPolozky: TArI;
   CB_TratPolozky: TArI;
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

    procedure MakeObls(var areas: TArStr);

  public
    procedure EditJC(JCIndex: Integer);
    procedure NewJC(templateIndex: Integer);

  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, FileSystem, Block, TOblsRizeni, IBUtils,
      BlockSignal, TJCDatabase, DataJC, BlockRailway, BlockTurnout;

{$R *.dfm}

procedure TF_JCEdit.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_JCEdit.EmptyOpenForm();
 begin
  Self.OpenIndex := -1;

  Self.JCData.railwayId := -1;

  // reset JC data:
  Self.JCData.railwayId := -1;
  Self.JCData.id   := -1;

  Self.JCData.turnouts  := nil;
  Self.JCData.tracks    := nil;
  Self.JCData.refuges  := nil;
  Self.JCData.crossings := nil;
  Self.JCData.vb       := nil;
  Self.JCData.locks    := nil;

  Self.m_tracks.Clear();
  Self.m_turnouts.Clear();

  E_Name.Text := '';
  if (JCDb.Count > 0) then
    SE_ID.Value := JCDb[JCDb.Count-1].id
  else
    SE_ID.Value := 1;
  Blocks.FillCB(CB_Navestidlo, @Self.CB_NavestidloPolozky, nil, nil, btSignal, -1);

  CB_Typ.ItemIndex := -1;
  CB_Dalsi_Nav.ItemIndex := -1;
  CB_Rychlost_Stuj.ItemIndex := 4;
  CB_Rychlost_Volno.ItemIndex := 4;
  CB_NavestidloChange(Self);
  Self.Caption := 'Vytvořit novou jízdní cestu';
  LV_Useky.Clear();
  Self.FillTurnouts();

  Self.M_Prj.Clear();
  Self.M_Odvraty.Clear();
  Self.M_Zamky.Clear();
  Self.E_VB.Text := '';
  Self.SE_SignalFallTrackI.Value := 0;
  Self.SE_SignalFallTrackI.MaxValue := 0;
  Self.CHB_Odbocka.Checked := false;
  Self.CHB_NZV.Checked := false;

  Self.CHB_Trat.Checked := false;
  Self.CHB_TratClick(Self.CHB_Trat);
 end;

procedure TF_JCEdit.EditOpenForm();
var prjz: TJCCrossingZav;
    tmp: string;
    blokid: integer;
    odvrat: TJCRefugeeZav;
    jcref: TJCRefZav;
    vb: Integer;
 begin
  JCData := JCDb.GetJCByIndex(OpenIndex).data;

  Blocks.FillCB(CB_Navestidlo,@CB_NavestidloPolozky, nil, nil, btSignal, JCData.signalId);
  CB_NavestidloChange(Self);

  Self.E_Name.Text:= JCData.name;
  Self.SE_ID.Value := JCData.id;

  Self.CB_Typ.ItemIndex := Integer(JCData.typ)-1;
  Self.CB_Rychlost_Volno.ItemIndex := JCData.speedGo div 10;
  Self.CB_Rychlost_Stuj.ItemIndex := JCData.speedStop div 10;

  Self.CB_TypChange(Self.CB_Typ);

  Self.CHB_Trat.Checked := (JCData.railwayId > -1);
  Self.CHB_TratClick(Self.CHB_Trat);

  Self.m_turnouts.Clear();
  Self.m_turnouts.AddRange(JCData.turnouts);
  Self.FillTurnouts();

  Self.m_tracks.Clear();
  Self.m_tracks.AddRange(JCData.tracks);
  Self.FillTracks();
  Self.CHB_Odbocka.Checked := JCData.turn;
  Self.CHB_NZV.Checked := JCData.nzv;

  Self.M_Prj.Clear();
  for prjz in JCData.crossings do
   begin
    tmp := IntToStr(prjz.crossingId);
    if (prjz.openTrack <> -1) then
     begin
      tmp := tmp + ', ' + IntToStr(prjz.openTrack);
      for blokid in prjz.closeTracks do
        tmp := tmp + ', ' + IntToStr(blokid);
     end;

    Self.M_Prj.Lines.Add(tmp);
   end;

  Self.M_Odvraty.Clear();
  for odvrat in JCData.refuges do
   begin
    tmp := IntToStr(odvrat.block) + ', ';
    if (odvrat.position = TTurnoutPosition.plus) then
      tmp := tmp + '+, '
    else
      tmp := tmp + '-, ';
    tmp := tmp + IntToStr(odvrat.ref_blk);

    Self.M_Odvraty.Lines.Add(tmp);
   end;

  Self.M_Zamky.Clear();
  for jcref in JCData.locks do
    Self.M_Zamky.Lines.Add(IntToStr(jcref.block) + ', ' + IntToStr(jcref.ref_blk));

  Self.E_VB.Text := '';
  for vb in JCData.vb do
    Self.E_VB.Text := Self.E_VB.Text + IntToStr(vb) + ', ';
  Self.E_VB.Text := LeftStr(Self.E_VB.Text, Length(Self.E_VB.Text) - 2);

  Self.SE_SignalFallTrackI.MaxValue := Max(JCData.tracks.Count-1, JCData.signalFallTrackI);
  Self.SE_SignalFallTrackI.Value := JCData.signalFallTrackI;

  CB_NavestidloChange(Self);
  if (Self.mNewJC) then
    Self.Caption := 'Vytvořit novou jízdní cestu'
  else
    Self.Caption := 'Upravit jízdní cestu '+JCData.name;
 end;

procedure TF_JCEdit.CommonOpenForm;
 begin
  Self.CHB_AdvancedClick(Self.CHB_Advanced);
  Self.ActiveControl := Self.E_Name;
 end;

procedure TF_JCEdit.B_Vyh_AddClick(Sender: TObject);
var vyh: TJCTurnoutZav;
    vyhIndex: Integer;
    updateOdbocka: Boolean;
 begin
  if (CB_NewZaverBlok.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte výhybku!','Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_NewZaverPoloha.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte polohu výhybky!','Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  updateOdbocka := (Self.CHB_Odbocka.Checked = Self.IsAnyTurnoutMinus());

  vyh.block   := Blocks.GetBlkID(CB_NewVyhybkaPolozky[CB_NewZaverBlok.ItemIndex]);
  vyh.position := TTurnoutPosition(CB_NewZaverPoloha.ItemIndex);

  vyhIndex := Self.TurnoutIndex(vyh.block);
  if (vyhIndex > -1) then
    Self.m_turnouts[vyhIndex] := vyh
  else
    Self.m_turnouts.Add(vyh);

  Self.FillTurnouts();
  if (updateOdbocka) then
    Self.CHB_Odbocka.Checked := Self.IsAnyTurnoutMinus();
 end;

procedure TF_JCEdit.B_Usek_AddClick(Sender: TObject);
 begin
  if (CB_NewUsek.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte úsek!', 'Nelze přidat úsek', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  if (Self.LV_Useky.ItemIndex = -1) then
    Self.m_tracks.Add(Blocks.GetBlkID(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]))
  else
    Self.m_tracks[Self.LV_Useky.ItemIndex] := Blocks.GetBlkID(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]);

  Self.FillTracks();
  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
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

procedure TF_JCEdit.FormCreate(Sender: TObject);
begin
 Self.m_tracks := TList<Integer>.Create();
 Self.m_turnouts := TList<TJCTurnoutZav>.Create();
end;

procedure TF_JCEdit.FormDestroy(Sender: TObject);
begin
 Self.m_tracks.Free();
 Self.m_turnouts.Free();
end;

procedure TF_JCEdit.EditJC(JCIndex: Integer);
 begin
  OpenIndex := JCIndex;
  CommonOpenForm;
  if (JCIndex = -1) then
   begin
    EmptyOpenForm();
   end else begin
    EditOpenForm();
   end;
  if (mNewJC) then
    Self.SE_ID.Value := Self.SE_ID.Value + 1;
  Self.ShowModal();
 end;

procedure TF_JCEdit.NewJC(templateIndex: Integer);
 begin
  mNewJC := true;
  EditJC(templateIndex);
 end;

procedure TF_JCEdit.B_SaveClick(Sender: TObject);
var JC: TJC;
    i: Integer;
    line, item: string;
    parsed: TStrings;
    odvrat: TJCRefugeeZav;
    prejezd: TJCCrossingZav;
    refz: TJCRefZav;
    vyhZaver: TJCTurnoutZav;
 begin
  if (E_Name.Text = '') then
   begin
    Application.MessageBox('Vyplňte název jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (JCDb.IsJC(Self.SE_ID.Value, Self.OpenIndex)) then
   begin
    Application.MessageBox('JC s tímto ID již existuje!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_Navestidlo.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte návestidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_Typ.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte typ jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_Dalsi_Nav.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte další návěstidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_Rychlost_Volno.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaká bude rychlost lokomotivy při projiždění JC při postaveném dalším návěstidle!',
                           'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (CB_Rychlost_Stuj.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaká bude rychlost lokomotivy při projiždění JC při dalším návěstidle na stůj!',
                           'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratBlok.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte trať!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratSmer.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte směr trati!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  for vyhZaver in Self.m_turnouts do
   begin
    if ((vyhZaver.position <> TTurnoutPosition.plus) and ((vyhZaver.position <> TTurnoutPosition.minus))) then
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

  //samotne ukladani dat
  JCData.name := Self.E_Name.Text;
  JCData.id := Self.SE_ID.Value;
  JCData.signalId := Blocks.GetBlkID(Self.CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
  JCData.typ := TJCType(Self.CB_Typ.ItemIndex+1);

  JCData.speedGo := Self.CB_Rychlost_Volno.ItemIndex*10;
  JCData.speedStop := Self.CB_Rychlost_Stuj.ItemIndex*10;
  JCData.turn := Self.CHB_Odbocka.Checked;
  JCData.nzv := Self.CHB_NZV.Checked;
  JCData.signalFallTrackI := Self.SE_SignalFallTrackI.Value;

  if (Self.CHB_Trat.Checked) then
   begin
    Self.JCData.railwayId := Blocks.GetBlkID(Self.CB_TratPolozky[Self.CB_TratBlok.ItemIndex]);
    Self.JCData.railwayDir := TRailwayDirection(Self.CB_TratSmer.ItemIndex + 1);
   end else begin
    Self.JCData.railwayId := -1;
    Self.JCData.railwayDir := TRailwayDirection.no;
   end;

  Self.CB_Dalsi_NavChange(Self.CB_Dalsi_Nav);

  if (not Assigned(JCData.turnouts) or (mNewJC)) then JCData.turnouts := TList<TJCTurnoutZav>.Create();
  JCData.turnouts.Clear();
  JCData.turnouts.AddRange(Self.m_turnouts);

  if (not Assigned(JCData.tracks) or (mNewJC)) then JCData.tracks := TList<Integer>.Create();
  JCData.tracks.Clear();
  JCData.tracks.AddRange(Self.m_tracks);

  parsed := TStringList.Create();
  try
    // Prejezdy
    if (not Assigned(JCData.crossings) or (mNewJC)) then JCData.crossings := TList<TJCCrossingZav>.Create();
    for prejezd in JCData.crossings do
      prejezd.closeTracks.Free();
    JCData.crossings.Clear();
    for line in Self.M_Prj.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        prejezd.closeTracks := nil;
        prejezd.crossingId := StrToInt(parsed[0]);
        if (parsed.Count > 1) then
          prejezd.openTrack := StrToInt(parsed[1])
        else
          prejezd.openTrack := -1;

        prejezd.closeTracks := TList<Integer>.Create();
        for i := 2 to parsed.Count-1 do
          prejezd.closeTracks.Add(StrToInt(parsed[i]));

        JCData.crossings.Add(prejezd);
      except
       on E: Exception do
        begin
         if (Assigned(prejezd.closeTracks)) then
           prejezd.closeTracks.Free();

         Application.MessageBox(PChar('Napodařilo se naparsovat přejezd "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit();
        end;
      end;
     end;

    // Odvraty
    if (not Assigned(JCData.refuges) or (mNewJC)) then JCData.refuges := TList<TJCRefugeeZav>.Create();
    JCData.refuges.Clear();
    for line in Self.M_Odvraty.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        odvrat.block := StrToInt(parsed[0]);
        if (parsed[1] = '+') then
          odvrat.position := TTurnoutPosition.plus
        else
          odvrat.position := TTurnoutPosition.minus;
        odvrat.ref_blk := StrToInt(parsed[2]);
        JCData.refuges.Add(odvrat);
      except
       on E: Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat odvrat "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit();
        end;
      end;
     end;

    // Zamky
    if (not Assigned(JCData.locks) or (mNewJC)) then JCData.locks := TList<TJCRefZav>.Create();
    JCData.locks.Clear();
    for line in Self.M_Zamky.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        refz.block := StrToInt(parsed[0]);
        refz.ref_blk := StrToInt(parsed[1]);
        JCData.locks.Add(refz);
      except
       on E: Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat zámek ' + line + ':'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit();
        end;
      end;
     end;

    // Variantní body
    if (not Assigned(JCData.vb) or (mNewJC)) then JCData.vb := TList<Integer>.Create();
    JCData.vb.Clear();
    parsed.Clear();
    ExtractStrings([','], [], PChar(StringReplace(Self.E_VB.Text, ' ', '', [rfReplaceAll])), parsed);

    for item in parsed do
     begin
      try
        JCData.vb.Add(StrToInt(item));
      except
       on E: Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat variatní bod ' + item + ':'+#13#10+E.Message),
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
       Application.MessageBox(PChar('Přidávání JC skončilo s chybou'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
       Exit();
      end;
    end;

   end else begin
    // update existujici JC
    JC := JCDb.GetJCByIndex(OpenIndex);
    JC.data := Self.JCData;
    JCTableData.UpdateLine(JC.index);
   end;

  Self.Close();
 end;

procedure TF_JCEdit.B_Usek_DelClick(Sender: TObject);
var i: Integer;
 begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané úseky z JC?'),
                             'Mazání úseku', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
   begin
    for i := Self.LV_Useky.Items.Count-1 downto 0 do
      if (Self.LV_Useky.Items[i].Selected) then
        Self.m_tracks.Delete(i);
    Self.FillTracks();
   end;

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
  Self.UpdateNextSignal();
  Self.SE_SignalFallTrackI.MaxValue := Max(Self.m_tracks.Count, Self.SE_SignalFallTrackI.Value);
 end;

procedure TF_JCEdit.B_Vyh_DelClick(Sender: TObject);
var i: Integer;
 begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané výhybky z JC?'),
      'Mazání výhybek', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
   begin
    for i := Self.LV_Vyhybky.Items.Count-1 downto 0 do
      if (Self.LV_Vyhybky.Items[i].Selected) then
        Self.m_turnouts.Delete(i);
    Self.FillTurnouts();
   end;
 end;

procedure TF_JCEdit.LV_VyhybkyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var i: Integer;
    vyh: TJCTurnoutZav;
 begin
  B_Vyh_Del.Enabled := (LV_Vyhybky.ItemIndex <> -1);
  B_Usek_Del.Enabled := false;

  if ((Self.LV_Vyhybky.Selected = nil) or (Self.LV_Vyhybky.ItemIndex >= Self.m_turnouts.Count)) then
   begin
    Self.CB_NewZaverBlok.ItemIndex := -1;
    Self.CB_NewZaverPoloha.ItemIndex := 0;
    Exit();
   end;

  vyh := Self.m_turnouts[Self.LV_Vyhybky.ItemIndex];

  for i := 0 to Length(Self.CB_NewVyhybkaPolozky)-1 do
    if (Blocks.GetBlkID(Self.CB_NewVyhybkaPolozky[i]) = vyh.block) then
      Self.CB_NewZaverBlok.ItemIndex := i;
  case (vyh.position) of
   TTurnoutPosition.plus: Self.CB_NewZaverPoloha.ItemIndex := 0;
   TTurnoutPosition.minus: Self.CB_NewZaverPoloha.ItemIndex := 1;
  else
   Self.CB_NewZaverPoloha.ItemIndex := -1;
  end;
 end;

procedure TF_JCEdit.LV_VyhybkyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if ((Key = VK_DELETE) and (Self.B_Vyh_Del.Enabled)) then
   B_Vyh_DelClick(B_Vyh_Del);
end;

procedure TF_JCEdit.LV_UsekyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var i: Integer;
 begin
  B_Usek_Del.Enabled := (LV_Useky.ItemIndex <> -1);
  B_Vyh_Del.Enabled := false;

  if ((Self.LV_Useky.Selected = nil) or (Self.LV_Useky.ItemIndex >= Self.m_tracks.Count)) then
   begin
    Self.CB_NewZaverBlok.ItemIndex := -1;
    Exit();
   end;

  for i := 0 to Length(Self.CB_NewUsekPolozky)-1 do
    if (Blocks.GetBlkID(Self.CB_NewUsekPolozky[i]) = Self.m_tracks[Self.LV_Useky.ItemIndex]) then
      Self.CB_NewUsek.ItemIndex := i;
 end;

procedure TF_JCEdit.LV_UsekyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if ((Key = VK_DELETE) and (Self.B_Usek_Del.Enabled)) then
   B_Usek_DelClick(B_Usek_Del);
end;

procedure TF_JCEdit.CB_Dalsi_NavChange(Sender: TObject);
begin
 if (CB_Dalsi_Nav.ItemIndex = 0) then begin
  JCData.nextSignalType := TJCNextSignalType.no;
 end else if (CB_Dalsi_Nav.ItemIndex = 1) then begin
  JCData.nextSignalType := TJCNextSignalType.railway;
 end else begin
  JCData.nextSignalType := TJCNextSignalType.signal;
  JCData.nextSignalId := Blocks.GetBlkID(CB_DalsiNavPolozky[CB_Dalsi_Nav.ItemIndex-2]);
 end;
end;

procedure TF_JCEdit.CB_NavestidloChange(Sender: TObject);
var navestidlo: TBlkSignal;
 begin
  if (CB_Navestidlo.ItemIndex <> -1) then
   begin
    JCData.signalId := Blocks.GetBlkID(CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
    Blocks.GetBlkByID(JCData.signalId, TBlk(navestidlo));

    Self.FillTracks();
    Self.FillTurnouts();

    if (Self.mNewJC) then
     begin
      case (navestidlo.SymbolType) of
        TBlkSignalSymbol.main : Self.CB_Typ.ItemIndex := 0;
        TBlkSignalSymbol.shunting : Self.CB_Typ.ItemIndex := 1;
      end;
     end;

   end;//if CB_Navestidlo.ItemIndex <> -1

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
  Self.UpdateNextSignal();
 end;

procedure TF_JCEdit.CB_TratBlokChange(Sender: TObject);
begin
 if (Self.CHB_Trat.Checked) then
   Self.JCData.railwayId := Blocks.GetBlkID(Self.CB_TratPolozky[Self.CB_TratBlok.ItemIndex]);
 UpdateNextSignal();
end;

procedure TF_JCEdit.CB_TypChange(Sender: TObject);
 begin
  Self.JCData.typ := TJCType(Self.CB_Typ.ItemIndex+1);
  CB_Rychlost_Volno.Enabled := (JCData.typ <> TJCType.shunt);
  CB_Rychlost_Stuj.Enabled := (JCData.typ <> TJCType.shunt);

  if (JCData.typ = TJCType.shunt) then
   begin
    CB_Rychlost_Volno.ItemIndex := 4;
    CB_Rychlost_Stuj.ItemIndex := 4;
   end;
 end;

procedure TF_JCEdit.CHB_AdvancedClick(Sender: TObject);
var gb: TGroupBox;
begin
 if (Self.CHB_Advanced.Checked) then
  gb := Self.GB_Advanced
 else
  gb := Self.GB_ZaveryVyhybek;

 Self.Width := gb.Left + gb.Width + 10;
 Self.B_Save.Left := gb.Left + gb.Width - Self.B_Save.Width;
 Self.B_Storno.Left := Self.B_Save.Left - Self.B_Storno.Width - 5;
end;

procedure TF_JCEdit.CHB_TratClick(Sender: TObject);
var areas: TArStr;
begin
 Self.CB_TratBlok.Enabled := Self.CHB_Trat.Checked;
 Self.CB_TratSmer.Enabled := Self.CHB_Trat.Checked;

 if (Self.CHB_Trat.Checked) then
  begin
   Self.MakeObls(areas);
   Blocks.FillCB(Self.CB_TratBlok, @Self.CB_TratPolozky, nil, areas, btRailway, Self.JCData.railwayId);
   Self.CB_TratSmer.ItemIndex := Integer(Self.JCData.railwayDir)-1;
  end else begin
   Self.CB_TratBlok.ItemIndex := -1;
   Self.CB_TratSmer.ItemIndex := -1;
  end;
end;

procedure TF_JCEdit.MakeObls(var areas: TArStr);
var Blk: TBlk;
    i: Integer;
begin
 Blocks.GetBlkByID(JCData.signalId, Blk);

 if (Blk = nil) then Exit();
 if (Blk.typ <> btSignal) then Exit();

 SetLength(areas, (Blk as TBlkSignal).areas.Count);
 for i := 0 to (Blk as TBlkSignal).areas.Count-1 do
  areas[i] := (Blk as TBlkSignal).areas[i].id;
end;

procedure TF_JCEdit.UpdateJCName();
begin
 if ((Self.CB_Navestidlo.ItemIndex <> -1)) then
   Self.E_Name.Text := Blocks.GetBlkName(Self.JCData.signalId) + ' > ';

 if (Self.m_tracks.Count <> 0) then
   Self.E_Name.Text := Self.E_Name.Text + Blocks.GetBlkName(Self.m_tracks[Self.m_tracks.Count-1]);
end;

procedure TF_JCEdit.UpdateNextSignal();
var ignore: TArI;
    areas: TArStr;
    blk: TBlk;
    i: Integer;
    navestidlo: TBlkSignal;
    trat: TBlkRailway;
begin
 Self.CB_Dalsi_Nav.Clear();
 Self.CB_Dalsi_Nav.Items.Add('Žádné návěstidlo');
 Self.CB_Dalsi_Nav.Items.Add('Trať');

 if (JCData.signalId = -1) then
  begin
   Self.CB_Dalsi_Nav.ItemIndex := -1;
   Self.CB_Dalsi_Nav.Enabled := false;
   Exit();
  end;

 Self.CB_Dalsi_Nav.Enabled := true;
 Blocks.GetBlkByID(JCData.signalId, TBlk(navestidlo));

 SetLength(ignore, 1);
 ignore[0] := JCData.signalId;
 Self.MakeObls(areas);

 if (Self.m_tracks.Count > 0) then
  begin
   if (Self.JCData.railwayId > -1) then
     Blocks.GetBlkByID(Self.JCData.railwayId, TBlk(trat))
   else
     trat := nil;

   SetLength(CB_DalsiNavPolozky, 0);
   for i := 0 to Blocks.count-1 do
    begin
     blk := Blocks[i];
     if ((blk.typ = btSignal) and
         ((TBlkSignal(blk).track = nil) or (TBlkSignal(blk).track.id = Self.m_tracks[Self.m_tracks.Count-1]) or
          ((trat <> nil) and (trat.HasAutoblokSignal(blk))))) then
      begin
       Self.CB_Dalsi_Nav.Items.Add(blk.name);
       SetLength(CB_DalsiNavPolozky, Length(CB_DalsiNavPolozky)+1);
       CB_DalsiNavPolozky[Length(CB_DalsiNavPolozky)-1] := i;
       if (blk.id = JCData.nextSignalId) then
         Self.CB_Dalsi_Nav.ItemIndex := Self.CB_Dalsi_Nav.Items.Count-1;
      end;
    end;

   if (Self.CB_Dalsi_Nav.ItemIndex = -1) then
    begin
     Blocks.GetBlkByID(JCData.nextSignalId, blk);
     if (blk <> nil) then
      begin
       Self.CB_Dalsi_Nav.Items.Add(blk.name);
       SetLength(CB_DalsiNavPolozky, Length(CB_DalsiNavPolozky)+1);
       CB_DalsiNavPolozky[Length(CB_DalsiNavPolozky)-1] := JCData.nextSignalId;
       Self.CB_Dalsi_Nav.ItemIndex := Self.CB_Dalsi_Nav.Items.Count-1;
      end;
    end;
  end else begin
   Blocks.FillCB(CB_Dalsi_Nav, @CB_DalsiNavPolozky, @ignore, areas, btSignal, JCData.nextSignalId);
   Self.CB_Dalsi_Nav.Items.Insert(0, 'Žádné návěstidlo');
   Self.CB_Dalsi_Nav.Items.Insert(1, 'Trať');
  end;

 if (JCData.nextSignalType = TJCNextSignalType.no) then
   Self.CB_Dalsi_Nav.ItemIndex := 0
 else if (JCData.nextSignalType = TJCNextSignalType.railway) then
   Self.CB_Dalsi_Nav.ItemIndex := 1;
end;

procedure TF_JCEdit.UpdateTurnoutsFromTracks();
var toAdd: TList<Integer>;
    blkid: Integer;
    blk: TBlk;
    vyhZaver: TJCTurnoutZav;
begin
 toAdd := TList<Integer>.Create();
 try
   for blk in Blocks do
    begin
     if (blk.typ <> btTurnout) then continue;
     if (Self.m_tracks.Contains(TBlkTurnout(blk).trackID)) then
       toAdd.Add(blk.id);
    end;

   for vyhZaver in Self.m_turnouts do
     if (toAdd.Contains(vyhZaver.block)) then
       toAdd.Remove(vyhZaver.block);

   for blkid in toAdd do
    begin
     vyhZaver.block := blkid;
     vyhZaver.position := TTurnoutPosition.none;
     Self.m_turnouts.Add(vyhZaver);
    end;

   Self.FillTurnouts();
 finally
   toAdd.Free();
 end;
end;

procedure TF_JCEdit.FillTurnouts();
var i: Integer;
    areas: TArStr;
    zaver: TJCTurnoutZav;
    LI: TListItem;
begin
 Self.LV_Vyhybky.Clear();
 for i := 0 to Self.m_turnouts.Count-1 do
  begin
   zaver := Self.m_turnouts[i];
   LI := LV_Vyhybky.Items.Add();
   LI.Caption := IntToStr(i+1);
   LI.SubItems.Add(Blocks.GetBlkName(zaver.block));
   case (zaver.position) of
    TTurnoutPosition.plus: LI.SubItems.Add('+');
    TTurnoutPosition.minus: LI.SubItems.Add('-');
   else
    LI.SubItems.Add('?');
   end;
  end;

 Self.MakeObls(areas);
 Blocks.FillCB(CB_NewZaverBlok, @CB_NewVyhybkaPolozky, nil, areas, btTurnout);
end;

function TF_JCEdit.TurnoutIndex(id: Integer): Integer;
var i: Integer;
begin
 for i := 0 to Self.m_turnouts.Count-1 do
  if (Self.m_turnouts[i].block = id) then
    Exit(i);
 Result := -1;
end;

procedure TF_JCEdit.FillTracks();
var i: Integer;
    LI: TListItem;
    areas: TArStr;
begin
 Self.LV_Useky.Clear();
 for i := 0 to Self.m_tracks.Count-1 do
  begin
   LI := LV_Useky.Items.Add();
   LI.Caption := IntToStr(i+1);
   LI.SubItems.Add(Blocks.GetBlkName(Self.m_tracks[i]));
  end;

 Self.MakeObls(areas);
 Blocks.FillCB(CB_NewUsek, @CB_NewUsekPolozky, nil, areas, btTrack, -1, btRT);
end;

function TF_JCEdit.IsAnyTurnoutMinus(): Boolean;
var vyhZaver: TJCTurnoutZav;
begin
 for vyhZaver in Self.m_turnouts do
   if (vyhZaver.position = TTurnoutPosition.minus) then
     Exit(true);
 Result := false;
end;

end.//unit

