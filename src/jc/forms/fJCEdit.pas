unit fJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin, TechnologieJC, Generics.Collections, TBloky,
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
   JCData: TJCprop;

   Useky: TList<Integer>;
   Vyhybky: TList<TJCVyhZaver>;

   procedure EmptyJCOpenForm();
   procedure NormalOpenForm();
   procedure HlavniOpenForm();

   procedure UpdateJCName();
   procedure UpdateNextNav();
   procedure UpdateVyhybkyFromUseky();
   procedure FillVyhybky();
   procedure FillUseky();
   function VyhybkaIndex(id: Integer): Integer;
   function IsAnyVyhMinus(): Boolean;

   procedure MakeObls(var obls: TArStr);
  public
   procedure EditJC(JCIndex: Integer);
   procedure NewJC(templateIndex: Integer);

  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, FileSystem, TBlok, TOblsRizeni,
      TBlockSignal, TJCDatabase, DataJC, TBlockRailway, TBlockTurnout;

{$R *.dfm}

procedure TF_JCEdit.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_JCEdit.EmptyJCOpenForm();
 begin
  Self.OpenIndex := -1;

  Self.JCData.Trat := -1;

  // reset JC data:
  Self.JCData.Trat := -1;
  Self.JCData.id   := -1;

  Self.JCData.Vyhybky  := nil;
  Self.JCData.Useky    := nil;
  Self.JCData.Odvraty  := nil;
  Self.JCData.Prejezdy := nil;
  Self.JCData.vb       := nil;
  Self.JCData.zamky    := nil;

  Self.Useky.Clear();
  Self.Vyhybky.Clear();

  E_Name.Text := '';
  if (JCDb.Count > 0) then
    SE_ID.Value := JCDb[JCDb.Count-1].id
  else
    SE_ID.Value := 1;
  Blky.NactiBlokyDoObjektu(CB_Navestidlo, @Self.CB_NavestidloPolozky, nil, nil, btSignal, -1);

  CB_Typ.ItemIndex := -1;
  CB_Dalsi_Nav.ItemIndex := -1;
  CB_Rychlost_Stuj.ItemIndex := 4;
  CB_Rychlost_Volno.ItemIndex := 4;
  CB_NavestidloChange(Self);
  Self.Caption := 'Vytvořit novou jízdní cestu';
  LV_Useky.Clear();
  Self.FillVyhybky();

  Self.M_Prj.Clear();
  Self.M_Odvraty.Clear();
  Self.M_Zamky.Clear();
  Self.E_VB.Text := '';
  Self.CHB_Odbocka.Checked := false;
  Self.CHB_NZV.Checked := false;

  Self.CHB_Trat.Checked := false;
  Self.CHB_TratClick(Self.CHB_Trat);
 end;

procedure TF_JCEdit.NormalOpenForm();
var prjz: TJCPrjZaver;
    tmp: string;
    blokid: integer;
    odvrat: TJCOdvratZaver;
    jcref: TJCRefZaver;
    vb: Integer;
 begin
  JCData := JCDb.GetJCByIndex(OpenIndex).data;

  Blky.NactiBlokyDoObjektu(CB_Navestidlo,@CB_NavestidloPolozky, nil, nil, btSignal, JCData.NavestidloBlok);
  CB_NavestidloChange(Self);

  Self.E_Name.Text:= JCData.name;
  Self.SE_ID.Value := JCData.id;

  Self.CB_Typ.ItemIndex := Integer(JCData.typ)-1;
  Self.CB_Rychlost_Volno.ItemIndex := JCData.speedGo div 10;
  Self.CB_Rychlost_Stuj.ItemIndex := JCData.speedStop div 10;

  Self.CB_TypChange(Self.CB_Typ);

  Self.CHB_Trat.Checked := (JCData.Trat > -1);
  Self.CHB_TratClick(Self.CHB_Trat);

  Self.Vyhybky.Clear();
  Self.Vyhybky.AddRange(JCData.Vyhybky);
  Self.FillVyhybky();

  Self.Useky.Clear();
  Self.Useky.AddRange(JCData.Useky);
  Self.FillUseky();
  Self.CHB_Odbocka.Checked := JCData.odbocka;
  Self.CHB_NZV.Checked := JCData.nzv;

  Self.M_Prj.Clear();
  for prjz in JCData.Prejezdy do
   begin
    tmp := IntToStr(prjz.Prejezd);
    if (prjz.oteviraci <> -1) then
     begin
      tmp := tmp + ', ' + IntToStr(prjz.oteviraci);
      for blokid in prjz.uzaviraci do
        tmp := tmp + ', ' + IntToStr(blokid);
     end;

    Self.M_Prj.Lines.Add(tmp);
   end;

  Self.M_Odvraty.Clear();
  for odvrat in JCData.Odvraty do
   begin
    tmp := IntToStr(odvrat.Blok) + ', ';
    if (odvrat.Poloha = TTurnoutPosition.plus) then
      tmp := tmp + '+, '
    else
      tmp := tmp + '-, ';
    tmp := tmp + IntToStr(odvrat.ref_blk);

    Self.M_Odvraty.Lines.Add(tmp);
   end;

  Self.M_Zamky.Clear();
  for jcref in JCData.zamky do
    Self.M_Zamky.Lines.Add(IntToStr(jcref.Blok) + ', ' + IntToStr(jcref.ref_blk));

  Self.E_VB.Text := '';
  for vb in JCData.vb do
    Self.E_VB.Text := Self.E_VB.Text + IntToStr(vb) + ', ';
  Self.E_VB.Text := LeftStr(Self.E_VB.Text, Length(Self.E_VB.Text) - 2);

  CB_NavestidloChange(Self);
  if (Self.mNewJC) then
    Self.Caption := 'Vytvořit novou jízdní cestu'
  else
    Self.Caption := 'Upravit jízdní cestu '+JCData.name;
 end;

procedure TF_JCEdit.HlavniOpenForm;
 begin
  Self.CHB_AdvancedClick(Self.CHB_Advanced);
  Self.ActiveControl := Self.E_Name;
 end;

procedure TF_JCEdit.B_Vyh_AddClick(Sender: TObject);
var vyh: TJCVyhZaver;
    vyhIndex: Integer;
    updateOdbocka: Boolean;
 begin
  if (CB_NewZaverBlok.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte výhybku!','Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_NewZaverPoloha.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte polohu výhybky!','Nelze pridat zaver', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  updateOdbocka := (Self.CHB_Odbocka.Checked = Self.IsAnyVyhMinus());

  vyh.Blok   := Blky.GetBlkID(CB_NewVyhybkaPolozky[CB_NewZaverBlok.ItemIndex]);
  vyh.Poloha := TTurnoutPosition(CB_NewZaverPoloha.ItemIndex);

  vyhIndex := Self.VyhybkaIndex(vyh.Blok);
  if (vyhIndex > -1) then
    Self.Vyhybky[vyhIndex] := vyh
  else
    Self.Vyhybky.Add(vyh);

  Self.FillVyhybky();
  if (updateOdbocka) then
    Self.CHB_Odbocka.Checked := Self.IsAnyVyhMinus();
 end;

procedure TF_JCEdit.B_Usek_AddClick(Sender: TObject);
 begin
  if (CB_NewUsek.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte úsek!', 'Nelze přidat úsek', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  if (Self.LV_Useky.ItemIndex = -1) then
    Self.Useky.Add(Blky.GetBlkID(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]))
  else
    Self.Useky[Self.LV_Useky.ItemIndex] := Blky.GetBlkID(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]);

  Self.FillUseky();
  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
  Self.UpdateNextNav();
  Self.UpdateVyhybkyFromUseky();
 end;

procedure TF_JCEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
 begin
  OpenIndex := -1;
  mNewJC := false;
  CanClose := true;
 end;

procedure TF_JCEdit.FormCreate(Sender: TObject);
begin
 Self.Useky   := TList<Integer>.Create();
 Self.Vyhybky := TList<TJCVyhZaver>.Create();
end;

procedure TF_JCEdit.FormDestroy(Sender: TObject);
begin
 Self.Useky.Free();
 Self.Vyhybky.Free();
end;

procedure TF_JCEdit.EditJC(JCIndex: Integer);
 begin
  OpenIndex := JCIndex;
  HlavniOpenForm;
  if (JCIndex = -1) then
   begin
    EmptyJCOpenForm();
   end else begin
    NormalOpenForm();
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
    odvrat: TJCOdvratZaver;
    prejezd: TJCPrjZaver;
    refz: TJCRefZaver;
    vyhZaver: TJCVyhZaver;
 begin
  if (E_Name.Text = '') then
   begin
    Application.MessageBox('Vyplňte název jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (JCDb.IsJC(Self.SE_ID.Value, Self.OpenIndex)) then
   begin
    Application.MessageBox('JC s tímto ID již existuje!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Navestidlo.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte návestidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Typ.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte typ jízdní cesty!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Dalsi_Nav.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte dalěá návěstidlo!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Rychlost_Volno.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaká bude rychlost lokomotivy při projiždění JC při postaveném dalším návěstidle!',
                           'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Rychlost_Stuj.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaká bude rychlost lokomotivy při projiždění JC při dalším návěstidle na stůj!',
                           'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratBlok.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte trať!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratSmer.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte směr trati!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  for vyhZaver in Self.Vyhybky do
   begin
    if ((vyhZaver.Poloha <> TTurnoutPosition.plus) and ((vyhZaver.Poloha <> TTurnoutPosition.minus))) then
     begin
      Application.MessageBox('Je třeba vybrat polohy všech výhybek!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;

  //samotne ukladani dat
  JCData.name := Self.E_Name.Text;
  JCData.id := Self.SE_ID.Value;
  JCData.NavestidloBlok := Blky.GetBlkID(Self.CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
  JCData.typ := TJCType(Self.CB_Typ.ItemIndex+1);

  JCData.speedGo := Self.CB_Rychlost_Volno.ItemIndex*10;
  JCData.speedStop := Self.CB_Rychlost_Stuj.ItemIndex*10;
  JCData.odbocka := Self.CHB_Odbocka.Checked;
  JCData.nzv := Self.CHB_NZV.Checked;

  if (Self.CHB_Trat.Checked) then
   begin
    Self.JCData.Trat := Blky.GetBlkID(Self.CB_TratPolozky[Self.CB_TratBlok.ItemIndex]);
    Self.JCData.TratSmer := TRailwayDirection(Self.CB_TratSmer.ItemIndex + 1);
   end else begin
    Self.JCData.Trat := -1;
    Self.JCData.TratSmer := TRailwayDirection.no;
   end;

  Self.CB_Dalsi_NavChange(Self.CB_Dalsi_Nav);

  if (not Assigned(JCData.Vyhybky) or (mNewJC)) then JCData.Vyhybky := TList<TJCVyhZaver>.Create();
  JCData.Vyhybky.Clear();
  JCData.Vyhybky.AddRange(Self.Vyhybky);

  if (not Assigned(JCData.Useky) or (mNewJC)) then JCData.Useky := TList<Integer>.Create();
  JCData.Useky.Clear();
  JCData.Useky.AddRange(Self.Useky);

  parsed := TStringList.Create();
  try
    // Prejezdy
    if (not Assigned(JCData.Prejezdy) or (mNewJC)) then JCData.Prejezdy := TList<TJCPrjZaver>.Create();
    for prejezd in JCData.Prejezdy do
      prejezd.uzaviraci.Free();
    JCData.Prejezdy.Clear();
    for line in Self.M_Prj.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        prejezd.uzaviraci := nil;
        prejezd.Prejezd := StrToInt(parsed[0]);
        if (parsed.Count > 1) then
          prejezd.oteviraci := StrToInt(parsed[1])
        else
          prejezd.oteviraci := -1;

        prejezd.uzaviraci := TList<Integer>.Create();
        for i := 2 to parsed.Count-1 do
          prejezd.uzaviraci.Add(StrToInt(parsed[i]));

        JCData.Prejezdy.Add(prejezd);
      except
       on E: Exception do
        begin
         if (Assigned(prejezd.uzaviraci)) then
           prejezd.uzaviraci.Free();

         Application.MessageBox(PChar('Napodařilo se naparsovat přejezd "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
        end;
      end;
     end;

    // Odvraty
    if (not Assigned(JCData.Odvraty) or (mNewJC)) then JCData.Odvraty := TList<TJCOdvratZaver>.Create();
    JCData.Odvraty.Clear();
    for line in Self.M_Odvraty.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        odvrat.Blok := StrToInt(parsed[0]);
        if (parsed[1] = '+') then
          odvrat.Poloha := TTurnoutPosition.plus
        else
          odvrat.Poloha := TTurnoutPosition.minus;
        odvrat.ref_blk := StrToInt(parsed[2]);
        JCData.Odvraty.Add(odvrat);
      except
       on E: Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat odvrat "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
        end;
      end;
     end;

    // Zamky
    if (not Assigned(JCData.zamky) or (mNewJC)) then JCData.zamky := TList<TJCRefZaver>.Create();
    JCData.zamky.Clear();
    for line in Self.M_Zamky.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        refz.Blok := StrToInt(parsed[0]);
        refz.ref_blk := StrToInt(parsed[1]);
        JCData.zamky.Add(refz);
      except
       on E: Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat zámek ' + line + ':'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
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
         Exit;
        end;
      end;
     end;

  finally
    parsed.Free()
  end;


  if (mNewJC) then
   begin
    try
     JCDb.AddJC(Self.JCData);
    except
     on E: Exception do
      begin
       Application.MessageBox(PChar('Přidávání JC skončilo s chybou'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
       Exit;
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
        Self.Useky.Delete(i);
    Self.FillUseky();
   end;

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
  Self.UpdateNextNav();
 end;

procedure TF_JCEdit.B_Vyh_DelClick(Sender: TObject);
var i: Integer;
 begin
  if (Application.MessageBox(PChar('Opravdu chcete smazat vybrané výhybky z JC?'),
      'Mazání výhybek', MB_YESNO OR MB_ICONQUESTION) = mrYes) then
   begin
    for i := Self.LV_Vyhybky.Items.Count-1 downto 0 do
      if (Self.LV_Vyhybky.Items[i].Selected) then
        Self.Vyhybky.Delete(i);
    Self.FillVyhybky();
   end;
 end;

procedure TF_JCEdit.LV_VyhybkyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var i: Integer;
    vyh: TJCVyhZaver;
 begin
  B_Vyh_Del.Enabled := (LV_Vyhybky.ItemIndex <> -1);
  B_Usek_Del.Enabled := false;

  if ((Self.LV_Vyhybky.Selected = nil) or (Self.LV_Vyhybky.ItemIndex >= Self.Vyhybky.Count)) then
   begin
    Self.CB_NewZaverBlok.ItemIndex := -1;
    Self.CB_NewZaverPoloha.ItemIndex := 0;
    Exit();
   end;

  vyh := Self.Vyhybky[Self.LV_Vyhybky.ItemIndex];

  for i := 0 to Length(Self.CB_NewVyhybkaPolozky)-1 do
    if (Blky.GetBlkID(Self.CB_NewVyhybkaPolozky[i]) = vyh.Blok) then
      Self.CB_NewZaverBlok.ItemIndex := i;
  case (vyh.Poloha) of
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

  if ((Self.LV_Useky.Selected = nil) or (Self.LV_Useky.ItemIndex >= Self.Useky.Count)) then
   begin
    Self.CB_NewZaverBlok.ItemIndex := -1;
    Exit();
   end;

  for i := 0 to Length(Self.CB_NewUsekPolozky)-1 do
    if (Blky.GetBlkID(Self.CB_NewUsekPolozky[i]) = Self.Useky[Self.LV_Useky.ItemIndex]) then
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
  JCData.DalsiNavaznost := TJCNextNavType.zadna;
 end else if (CB_Dalsi_Nav.ItemIndex = 1) then begin
  JCData.DalsiNavaznost := TJCNextNavType.trat;
 end else begin
  JCData.DalsiNavaznost := TJCNextNavType.blok;
  JCData.DalsiNavestidlo := Blky.GetBlkID(CB_DalsiNavPolozky[CB_Dalsi_Nav.ItemIndex-2]);
 end;
end;

procedure TF_JCEdit.CB_NavestidloChange(Sender: TObject);
var navestidlo: TBlkSignal;
 begin
  if (CB_Navestidlo.ItemIndex <> -1) then
   begin
    JCData.NavestidloBlok := Blky.GetBlkID(CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
    Blky.GetBlkByID(JCData.NavestidloBlok, TBlk(navestidlo));

    Self.FillUseky();
    Self.FillVyhybky();

    if (Self.mNewJC) then
     begin
      case (navestidlo.SymbolType) of
        TBlkSignalSymbol.main : Self.CB_Typ.ItemIndex := 0;
        TBlkSignalSymbol.shunting : Self.CB_Typ.ItemIndex := 1;
      end;
     end;

   end;//if CB_Navestidlo.ItemIndex <> -1

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
  Self.UpdateNextNav();
 end;

procedure TF_JCEdit.CB_TratBlokChange(Sender: TObject);
begin
 if (Self.CHB_Trat.Checked) then
   Self.JCData.Trat := Blky.GetBlkID(Self.CB_TratPolozky[Self.CB_TratBlok.ItemIndex]);
 UpdateNextNav();
end;

procedure TF_JCEdit.CB_TypChange(Sender: TObject);
 begin
  Self.JCData.typ := TJCType(Self.CB_Typ.ItemIndex+1);
  CB_Rychlost_Volno.Enabled := (JCData.typ <> TJCType.posun);
  CB_Rychlost_Stuj.Enabled := (JCData.typ <> TJCType.posun);

  if (JCData.typ = TJCType.posun) then
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
var obls: TArStr;
begin
 Self.CB_TratBlok.Enabled := Self.CHB_Trat.Checked;
 Self.CB_TratSmer.Enabled := Self.CHB_Trat.Checked;

 if (Self.CHB_Trat.Checked) then
  begin
   Self.MakeObls(obls);
   Blky.NactiBlokyDoObjektu(Self.CB_TratBlok, @Self.CB_TratPolozky, nil, obls, btRailway, Self.JCData.Trat);
   Self.CB_TratSmer.ItemIndex := Integer(Self.JCData.TratSmer)-1;
  end else begin
   Self.CB_TratBlok.ItemIndex := -1;
   Self.CB_TratSmer.ItemIndex := -1;
  end;
end;

procedure TF_JCEdit.MakeObls(var obls: TArStr);
var Blk: TBlk;
    i: Integer;
begin
 Blky.GetBlkByID(JCData.NavestidloBlok, Blk);

 if (Blk = nil) then Exit;
 if (Blk.typ <> btSignal) then Exit;

 SetLength(obls, (Blk as TBlkSignal).stations.Count);
 for i := 0 to (Blk as TBlkSignal).stations.Count-1 do
  obls[i] := (Blk as TBlkSignal).stations[i].id;
end;

procedure TF_JCEdit.UpdateJCName();
begin
 if ((Self.CB_Navestidlo.ItemIndex <> -1)) then
   Self.E_Name.Text := Blky.GetBlkName(Self.JCData.NavestidloBlok) + ' > ';

 if (Self.Useky.Count <> 0) then
   Self.E_Name.Text := Self.E_Name.Text + Blky.GetBlkName(Self.Useky[Self.Useky.Count-1]);
end;

procedure TF_JCEdit.UpdateNextNav();
var vypustit: TArI;
    obls: TArStr;
    blk: TBlk;
    i: Integer;
    navestidlo: TBlkSignal;
    trat: TBlkRailway;
begin
 Self.CB_Dalsi_Nav.Clear();
 Self.CB_Dalsi_Nav.Items.Add('Žádné návěstidlo');
 Self.CB_Dalsi_Nav.Items.Add('Trať');

 if (JCData.NavestidloBlok = -1) then
  begin
   Self.CB_Dalsi_Nav.ItemIndex := -1;
   Self.CB_Dalsi_Nav.Enabled := false;
   Exit();
  end;

 Self.CB_Dalsi_Nav.Enabled := true;
 Blky.GetBlkByID(JCData.NavestidloBlok, TBlk(navestidlo));

 SetLength(vypustit, 1);
 vypustit[0] := JCData.NavestidloBlok;
 Self.MakeObls(obls);

 if (Self.Useky.Count > 0) then
  begin
   if (Self.JCData.Trat > -1) then
     Blky.GetBlkByID(Self.JCData.Trat, TBlk(trat))
   else
     trat := nil;

   SetLength(CB_DalsiNavPolozky, 0);
   for i := 0 to Blky.count-1 do
    begin
     blk := Blky[i];
     if ((blk.typ = btSignal) and
         ((TBlkSignal(blk).track = nil) or (TBlkSignal(blk).track.id = Self.Useky[Self.Useky.Count-1]) or
          ((trat <> nil) and (trat.HasAutoblokSignal(blk))))) then
      begin
       Self.CB_Dalsi_Nav.Items.Add(blk.name);
       SetLength(CB_DalsiNavPolozky, Length(CB_DalsiNavPolozky)+1);
       CB_DalsiNavPolozky[Length(CB_DalsiNavPolozky)-1] := i;
       if (blk.id = JCData.DalsiNavestidlo) then
         Self.CB_Dalsi_Nav.ItemIndex := Self.CB_Dalsi_Nav.Items.Count-1;
      end;
    end;

   if (Self.CB_Dalsi_Nav.ItemIndex = -1) then
    begin
     Blky.GetBlkByID(JCData.DalsiNavestidlo, blk);
     if (blk <> nil) then
      begin
       Self.CB_Dalsi_Nav.Items.Add(blk.name);
       SetLength(CB_DalsiNavPolozky, Length(CB_DalsiNavPolozky)+1);
       CB_DalsiNavPolozky[Length(CB_DalsiNavPolozky)-1] := JCData.DalsiNavestidlo;
       Self.CB_Dalsi_Nav.ItemIndex := Self.CB_Dalsi_Nav.Items.Count-1;
      end;
    end;
  end else begin
   Blky.NactiBlokyDoObjektu(CB_Dalsi_Nav, @CB_DalsiNavPolozky, @vypustit, obls, btSignal, JCData.DalsiNavestidlo);
   Self.CB_Dalsi_Nav.Items.Insert(0, 'Žádné návěstidlo');
   Self.CB_Dalsi_Nav.Items.Insert(1, 'Trať');
  end;

 if (JCData.DalsiNavaznost = TJCNextNavType.zadna) then
   Self.CB_Dalsi_Nav.ItemIndex := 0
 else if (JCData.DalsiNavaznost = TJCNextNavType.trat) then
   Self.CB_Dalsi_Nav.ItemIndex := 1;
end;

procedure TF_JCEdit.UpdateVyhybkyFromUseky();
var toAdd: TList<Integer>;
    blkid: Integer;
    blk: TBlk;
    vyhZaver: TJCVyhZaver;
begin
 toAdd := TList<Integer>.Create();
 try
   for blk in blky do
    begin
     if (blk.typ <> btTurnout) then continue;
     if (Self.Useky.Contains(TBlkTurnout(blk).trackID)) then
       toAdd.Add(blk.id);
    end;

   for vyhZaver in Self.Vyhybky do
     if (toAdd.Contains(vyhZaver.Blok)) then
       toAdd.Remove(vyhZaver.Blok);

   for blkid in toAdd do
    begin
     vyhZaver.Blok := blkid;
     vyhZaver.Poloha := TTurnoutPosition.none;
     Self.Vyhybky.Add(vyhZaver);
    end;

   Self.FillVyhybky();
 finally
   toAdd.Free();
 end;
end;

procedure TF_JCEdit.FillVyhybky();
var i: Integer;
    obls: TArStr;
    zaver: TJCVyhZaver;
    LI: TListItem;
begin
 Self.LV_Vyhybky.Clear();
 for i := 0 to Self.Vyhybky.Count-1 do
  begin
   zaver := Self.Vyhybky[i];
   LI := LV_Vyhybky.Items.Add();
   LI.Caption := IntToStr(i+1);
   LI.SubItems.Add(Blky.GetBlkName(zaver.Blok));
   case (zaver.Poloha) of
    TTurnoutPosition.plus: LI.SubItems.Add('+');
    TTurnoutPosition.minus: LI.SubItems.Add('-');
   else
    LI.SubItems.Add('?');
   end;
  end;

 Self.MakeObls(obls);
 Blky.NactiBlokyDoObjektu(CB_NewZaverBlok, @CB_NewVyhybkaPolozky, nil, obls, btTurnout);
end;

function TF_JCEdit.VyhybkaIndex(id: Integer): Integer;
var i: Integer;
begin
 for i := 0 to Self.Vyhybky.Count-1 do
  if (Self.Vyhybky[i].Blok = id) then
    Exit(i);
 Result := -1;
end;

procedure TF_JCEdit.FillUseky();
var i: Integer;
    LI: TListItem;
    obls: TArStr;
begin
 Self.LV_Useky.Clear();
 for i := 0 to Self.Useky.Count-1 do
  begin
   LI := LV_Useky.Items.Add();
   LI.Caption := IntToStr(i+1);
   LI.SubItems.Add(Blky.GetBlkName(Self.Useky[i]));
  end;

 Self.MakeObls(obls);
 Blky.NactiBlokyDoObjektu(CB_NewUsek, @CB_NewUsekPolozky, nil, obls, btUsek, -1, btTU);
end;

function TF_JCEdit.IsAnyVyhMinus(): Boolean;
var vyhZaver: TJCVyhZaver;
begin
 for vyhZaver in Self.Vyhybky do
   if (vyhZaver.Poloha = TTurnoutPosition.minus) then
     Exit(true);
 Result := false;
end;

end.//unit

