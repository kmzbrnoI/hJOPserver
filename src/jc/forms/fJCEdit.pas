unit fJCEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Spin, TechnologieJC,
  Generics.Collections, TBloky, StrUtils;

type
  TF_JCEdit = class(TForm)
    L_VC_01: TLabel;
    E_VCNazev: TEdit;
    GB_ZaveryVyhybek: TGroupBox;
    LV_Zavery: TListView;
    CHB_NewZaver: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_NewZaverBlok: TComboBox;
    CB_NewZaverPoloha: TComboBox;
    B_NewZaverAdd: TButton;
    GB_Useky: TGroupBox;
    CHB_NewBlok: TGroupBox;
    Label12: TLabel;
    CB_NewUsek: TComboBox;
    B_NewUsek: TButton;
    LV_Useky: TListView;
    B_ZaveryVyhybek_Delete: TButton;
    B_ZaveryUseku_Delete: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    L_VC_02: TLabel;
    CB_Navestidlo: TComboBox;
    CB_TypCesty: TComboBox;
    L_VC_11: TLabel;
    L_VC_07: TLabel;
    CB_DalsiNNavaznost: TComboBox;
    L_VC_10: TLabel;
    CB_Rychlost_DalsiN: TComboBox;
    CB_Rychlost_NoDalsiN: TComboBox;
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
    Label6: TLabel;
    Label7: TLabel;
    E_VB: TEdit;
    Label8: TLabel;
    CHB_Advanced: TCheckBox;
    M_Redukce: TMemo;
    M_Zamky: TMemo;
    procedure B_StornoClick(Sender: TObject);
    procedure B_NewZaverAddClick(Sender: TObject);
    procedure B_NewUsekClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_SaveClick(Sender: TObject);
    procedure B_ZaveryUseku_DeleteClick(Sender: TObject);
    procedure B_ZaveryVyhybek_DeleteClick(Sender: TObject);
    procedure LV_ZaveryChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_UsekyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure CB_NavestidloChange(Sender: TObject);
    procedure CB_TypCestyChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CHB_TratClick(Sender: TObject);
    procedure CHB_AdvancedClick(Sender: TObject);
  private
   OpenIndex:Integer;
   NewVC:Boolean;
   CB_NavestidloPolozky:TArI;
   CB_DalsiNNavaznostPolozky:TArI;
   CB_NewUsekPolozky:TArI;
   CB_NewVyhybkaPolozky:TArI;
   CB_TratPolozky:TArI;
   JCData:TJCprop;

   Useky:TList<Integer>;
   Vyhybky:TList<TJCVyhZaver>;

   procedure NewVCOpenForm;
   procedure NormalOpenForm;
   procedure HlavniOpenForm;

   procedure UpdateJCName();

   procedure MakeObls(var obls:TArStr);
  public
   procedure OpenForm(VCIndex:Integer);
   procedure NewVCCreate;
  end;

var
  F_JCEdit: TF_JCEdit;

implementation

uses GetSystems, FileSystem, TBlok, TOblsRizeni,
      TBlokNav, fSettings, TJCDatabase, DataJC, TBlokTrat, TBlokVyhybka;

{$R *.dfm}

procedure TF_JCEdit.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_JCEdit.NewVCOpenForm;
 begin
  Self.OpenIndex := -1;

  Self.JCData.Trat := -1;

  // reset JC data:
  Self.JCData.Trat := -1;
  Self.JCData.id   := -1;

  Self.JCData.Vyhybky  := nil;
  Self.JCData.Useky    := nil;
  Self.JCData.Odvraty  := nil;
  Self.JCData.Prisl    := nil;
  Self.JCData.Prejezdy := nil;
  Self.JCData.vb       := nil;
  Self.JCData.zamky    := nil;

  Self.Useky.Clear();
  Self.Vyhybky.Clear();

  E_VCNazev.Text                  := '';
  if (JCDb.Count > 0) then
    SE_ID.Value := JCDb.GetJCByIndex(JCDb.Count-1).id + 1
  else
    SE_ID.Value := 1;
  Blky.NactiBlokyDoObjektu(CB_Navestidlo, @Self.CB_NavestidloPolozky, nil, nil, _BLK_NAV, -1);

  CB_TypCesty.ItemIndex           := -1;
  CB_DalsiNNavaznost.ItemIndex    := -1;
  CB_Rychlost_NoDalsiN.ItemIndex  := 4;
  CB_Rychlost_DalsiN.ItemIndex    := 4;
  CB_NavestidloChange(Self);
  Self.Caption  := 'Editovat data nové jízdní cesty';
  LV_Zavery.Clear;
  LV_Useky.Clear;

  Self.M_Prj.Clear();
  Self.M_Odvraty.Clear();
  Self.M_Redukce.Clear();
  Self.M_Zamky.Clear();
  Self.E_VB.Text := '';

  Self.CHB_Trat.Checked := false;
  Self.CHB_TratClick(Self.CHB_Trat);
 end;

procedure TF_JCEdit.NormalOpenForm();
var cyklus:Integer;
    LI:TListItem;
    prjz:TJCPrjZaver;
    tmp:string;
    blokid:integer;
    odvrat:TJCOdvratZaver;
    jcref:TJCRefZaver;
    vb:Integer;
 begin
  JCData := JCDb.GetJCByIndex(OpenIndex).data;

  Blky.NactiBlokyDoObjektu(CB_Navestidlo,@CB_NavestidloPolozky, nil, nil, _BLK_NAV, JCData.NavestidloBlok);
  CB_NavestidloChange(Self);

  E_VCNazev.Text := JCData.Nazev;
  SE_ID.Value    := JCData.id;

  CB_TypCesty.ItemIndex           := Integer(JCData.TypCesty)-1;
  CB_Rychlost_DalsiN.ItemIndex    := JCData.RychlostDalsiN;
  CB_Rychlost_NoDalsiN.ItemIndex  := JCData.RychlostNoDalsiN;

  if (JCData.TypCesty = TJCType.posun) then
   begin
    CB_Rychlost_DalsiN.Enabled      := false;
    CB_Rychlost_NoDalsiN.Enabled    := false;
    CB_Rychlost_DalsiN.ItemIndex    := 4;
    CB_Rychlost_NoDalsiN.ItemIndex  := 4;
   end else begin
    CB_Rychlost_DalsiN.Enabled      := true;
    CB_Rychlost_NoDalsiN.Enabled    := true;
   end;

  Self.CHB_Trat.Checked := (JCData.Trat > -1);
  Self.CHB_TratClick(Self.CHB_Trat);

  LV_Zavery.Clear;
  Self.Vyhybky.Clear();
  for cyklus := 0 to JCData.Vyhybky.Count-1 do
   begin
    LI := LV_Zavery.Items.Add;
    LI.Caption := Blky.GetBlkName(JCData.Vyhybky[cyklus].Blok);

    case (JCData.Vyhybky[cyklus].Poloha) of
     TVyhPoloha.plus  : LI.SubItems.Add('+');
     TVyhPoloha.minus : LI.SubItems.Add('-');
    end;
    Self.Vyhybky.Add(JCData.Vyhybky[cyklus]);
   end;//for cyklus

  LV_Useky.Clear;
  Self.Useky.Clear();
  for cyklus := 0 to JCData.Useky.Count-1 do
   begin
    LI := LV_Useky.Items.Add;
    LI.Caption := Blky.GetBlkName(JCData.Useky[cyklus]);
    Self.Useky.Add(JCData.Useky[cyklus]);
   end;//for cyklus

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
    if (odvrat.Poloha = TVyhPoloha.plus) then
      tmp := tmp + '+, '
    else
      tmp := tmp + '-, ';
    tmp := tmp + IntToStr(odvrat.ref_blk);

    Self.M_Odvraty.Lines.Add(tmp);
   end;

  Self.M_Redukce.Clear();
  for jcref in JCData.Prisl do
    Self.M_Redukce.Lines.Add(IntToStr(jcref.Blok) + ', ' + IntToStr(jcref.ref_blk));

  Self.M_Zamky.Clear();
  for jcref in JCData.zamky do
    Self.M_Zamky.Lines.Add(IntToStr(jcref.Blok) + ', ' + IntToStr(jcref.ref_blk));

  Self.E_VB.Text := '';
  for vb in JCData.vb do
    Self.E_VB.Text := Self.E_VB.Text + IntToStr(vb) + ', ';
  Self.E_VB.Text := LeftStr(Self.E_VB.Text, Length(Self.E_VB.Text) - 2);

  Self.Caption := 'Editovat data jízdní cesty '+JCData.nazev;
 end;

procedure TF_JCEdit.HlavniOpenForm;
 begin
  Self.CHB_AdvancedClick(Self.CHB_Advanced);

  Self.ActiveControl := Self.E_VCNazev;
 end;

procedure TF_JCEdit.B_NewZaverAddClick(Sender: TObject);
var LI:TListItem;
    Vypustit:TArI;
    cyklus:Integer;
    obls:TArStr;
    vyh:TJCVyhZaver;
 begin
  if (CB_NewZaverBlok.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte blok !','Nelze pridat zaver',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_NewZaverPoloha.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte polohu vyhybky !','Nelze pridat zaver',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  //samotne pridavani zaveru
  vyh.Blok   := Blky.GetBlkID(CB_NewVyhybkaPolozky[CB_NewZaverBlok.ItemIndex]);
  vyh.Poloha := TVyhPoloha(CB_NewZaverPoloha.ItemIndex);
  Self.Vyhybky.Add(vyh);

  LI := LV_Zavery.Items.Add;
  LI.Caption := Blky.GetBlkIndexName(CB_NewVyhybkaPolozky[CB_NewZaverBlok.ItemIndex]);

  case (CB_NewZaverPoloha.ItemIndex) of
    0 : LI.SubItems.Add('+');
    1 : LI.SubItems.Add('-');
  end;

  SetLength(Vypustit, 0);
  for cyklus := 0 to Self.Vyhybky.Count-1 do
   begin
    SetLength(Vypustit, Length(Vypustit)+1);
    Vypustit[cyklus] := Self.Vyhybky[cyklus].Blok;
   end;//for cyklus

  Self.MakeObls(obls);
  Blky.NactiBlokyDoObjektu(CB_NewZaverBlok, @CB_NewVyhybkaPolozky, @Vypustit, obls, 0, -1);
 end;

procedure TF_JCEdit.B_NewUsekClick(Sender: TObject);
var LI:TListItem;
    obls:TArStr;
    Useky:TArI;
    i:Integer;
 begin
  if (CB_NewUsek.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte blok !','Nelze pridat usek',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  //samotne pridavani zaveru
  Self.Useky.Add(Blky.GetBlkID(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]));

  LI := Self.LV_Useky.Items.Add();
  LI.Caption := Blky.GetBlkIndexName(CB_NewUsekPolozky[CB_NewUsek.ItemIndex]);

  SetLength(Useky, Self.Useky.Count);
  for i := 0 to Self.Useky.Count-1 do
    Useky[i] := Self.Useky[i];

  Self.MakeObls(obls);
  Blky.NactiBlokyDoObjektu(CB_NewUsek, @CB_NewUsekPolozky, @Useky, obls, _BLK_USEK, -1, _BLK_TU);

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
 end;

procedure TF_JCEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
 begin
  OpenIndex := -1;
  NewVC     := false;
  CanClose  := true;
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

procedure TF_JCEdit.OpenForm(VCIndex:Integer);
 begin
  OpenIndex := VCIndex;
  HlavniOpenForm;
  if (NewVC) then
   begin
    NewVCOpenForm;
   end else begin
    NormalOpenForm;
   end;
  Self.ShowModal();
 end;

procedure TF_JCEdit.NewVCCreate;
 begin
  NewVC := true;
  OpenForm(-1);
 end;

procedure TF_JCEdit.B_SaveClick(Sender: TObject);
var JC:TJC;
    i:Integer;
    line, item:string;
    parsed:TStrings;
    odvrat:TJCOdvratZaver;
    prejezd:TJCPrjZaver;
    refz:TJCRefZaver;
 begin
  if (E_VCNazev.Text = '') then
   begin
    Application.MessageBox('Vyplnte nazev vlakove cesty !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (JCDb.IsJC(Self.SE_ID.Value, Self.OpenIndex)) then
   begin
    Application.MessageBox('JC s tímto ID již existuje !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Navestidlo.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte blok navestidla !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_TypCesty.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte typ vlakove cesty !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_DalsiNNavaznost.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte blok navaznosti na dalsi navestidlo !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Rychlost_DalsiN.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaka bude rychlost lokomotivy pri projizdeni VC pri postavenem dalsim navestidle!','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (CB_Rychlost_NoDalsiN.ItemIndex = -1) then
   begin
    Application.MessageBox('Vyberte, jaka bude rychlost lokomotivy pri projizdeni VC pri zrusenem dalsim navestidle!','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratBlok.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte trať!', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_Trat.Checked) and (Self.CB_TratSmer.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte směr trati!', 'Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  //samotne ukladani dat
  JCData.Nazev             := E_VCNazev.Text;
  JCData.id                := SE_ID.Value;
  JCData.NavestidloBlok    := Blky.GetBlkID(CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
  JCData.TypCesty          := TJCType(CB_TypCesty.ItemIndex+1);

  JCData.RychlostDalsiN    := CB_Rychlost_DalsiN.ItemIndex;
  JCData.RychlostNoDalsiN  := CB_Rychlost_NoDalsiN.ItemIndex;

  if (Self.CHB_Trat.Checked) then
   begin
    Self.JCData.Trat     := Blky.GetBlkID(Self.CB_TratPolozky[Self.CB_TratBlok.ItemIndex]);
    Self.JCData.TratSmer := TTratSmer(Self.CB_TratSmer.ItemIndex + 1);
   end else begin
    Self.JCData.Trat     := -1;
    Self.JCData.TratSmer := TTratSmer.zadny;
   end;

  if (CB_DalsiNNavaznost.ItemIndex < Length(CB_DalsiNNavaznostPolozky)) then
   begin
    JCData.DalsiNNavaznostTyp  := 2;
    JCData.DalsiNNavaznost     := Blky.GetBlkID(CB_DalsiNNavaznostPolozky[CB_DalsiNNavaznost.ItemIndex]);
   end else begin
    JCData.DalsiNNavaznostTyp  := CB_DalsiNNavaznost.ItemIndex-Length(CB_DalsiNNavaznostPolozky);
    JCData.DalsiNNavaznost     := 0;
   end;

  if (not Assigned(JCData.Vyhybky)) then JCData.Vyhybky := TList<TJCVyhZaver>.Create();
  JCData.Vyhybky.Clear();
  for i := 0 to Self.Vyhybky.Count-1 do JCData.Vyhybky.Add(Self.Vyhybky[i]);

  if (not Assigned(JCData.Useky)) then JCData.Useky := TList<Integer>.Create();
  JCData.Useky.Clear();
  for i := 0 to Self.Useky.Count-1 do JCData.Useky.Add(Self.Useky[i]);

  parsed := TStringList.Create();
  try
    // Prejezdy
    if (not Assigned(JCData.Prejezdy)) then JCData.Prejezdy := TList<TJCPrjZaver>.Create();
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
       on E:Exception do
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
    if (not Assigned(JCData.Odvraty)) then JCData.Odvraty := TList<TJCOdvratZaver>.Create();
    JCData.Odvraty.Clear();
    for line in Self.M_Odvraty.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        odvrat.Blok := StrToInt(parsed[0]);
        if (parsed[1] = '+') then
          odvrat.Poloha := TVyhPoloha.plus
        else
          odvrat.Poloha := TVyhPoloha.minus;
        odvrat.ref_blk := StrToInt(parsed[2]);
        JCData.Odvraty.Add(odvrat);
      except
       on E:Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat odvrat "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
        end;
      end;
     end;

    // Redukce
    if (not Assigned(JCData.Prisl)) then JCData.Prisl := TList<TJCRefZaver>.Create();
    JCData.Prisl.Clear();
    for line in Self.M_Redukce.Lines do
     begin
      parsed.Clear();
      ExtractStrings([','], [], PChar(StringReplace(line, ' ', '', [rfReplaceAll])), parsed);

      try
        refz.Blok := StrToInt(parsed[0]);
        refz.ref_blk := StrToInt(parsed[1]);
        JCData.Prisl.Add(refz);
      except
       on E:Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat redukci "' + line + '":'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
        end;
      end;
     end;

    // Zamky
    if (not Assigned(JCData.zamky)) then JCData.zamky := TList<TJCRefZaver>.Create();
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
       on E:Exception do
        begin
         Application.MessageBox(PChar('Napodařilo se naparsovat zámek ' + line + ':'+#13#10+E.Message),
                                'Chyba', MB_OK OR MB_ICONWARNING);
         Exit;
        end;
      end;
     end;

    // Variantní body
    if (not Assigned(JCData.vb)) then JCData.vb := TList<Integer>.Create();
    JCData.vb.Clear();
    parsed.Clear();
    ExtractStrings([','], [], PChar(StringReplace(Self.E_VB.Text, ' ', '', [rfReplaceAll])), parsed);

    for item in parsed do
     begin
      try
        JCData.vb.Add(StrToInt(item));
      except
       on E:Exception do
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


  if (OpenIndex < 0) then
   begin
    // nova JC
    try
     JC := JCDb.AddJC(Self.JCData);
     if (JC = nil) then raise Exception.Create('JC nevytvořena');
    except
     on E:Exception do
      begin
       Application.MessageBox(PChar('Přidávání JC skončilo s chybou'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
       Exit;
      end;
    end;

   end else begin
    // update existujici JC
    JC := JCDb.GetJCByIndex(OpenIndex);
    JC.data  := Self.JCData;
    JCTableData.UpdateLine(JC.index);
   end;

  Self.Close;
 end;

procedure TF_JCEdit.B_ZaveryUseku_DeleteClick(Sender: TObject);
var Pozice,i:Integer;
    obls:TArStr;
    Useky:TArI;
 begin
  Pozice := LV_Useky.ItemIndex;
  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazat zaver useku '+Blky.GetBlkName(Self.Useky[Pozice])+'?'),'Mazání zaveru useku', MB_YESNO OR MB_ICONQUESTION) = mrYes then
   begin
    Self.Useky.Delete(pozice);
    LV_Useky.Items.Delete(Pozice);

    Self.MakeObls(obls);

    SetLength(Useky, Self.Useky.Count);
    for i := 0 to Self.Useky.Count-1 do
      Useky[i] := Self.Useky[i];

    Blky.NactiBlokyDoObjektu(CB_NewUsek, @CB_NewUsekPolozky, @Useky, obls, _BLK_USEK, -1, _BLK_TU);
   end;//if MessageBox

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();
 end;

procedure TF_JCEdit.B_ZaveryVyhybek_DeleteClick(Sender: TObject);
var cyklus,Pozice:Integer;
    Vypustit:TArI;
    obls:TArStr;
 begin
  Pozice := LV_Zavery.ItemIndex;
  Beep;
  if Application.MessageBox(PChar('Opravdu chcete smazat zaver vyybky '+Blky.GetBlkName(Self.Vyhybky[Pozice].Blok)+'?'),'Mazání zaveru vyhybky', MB_YESNO OR MB_ICONQUESTION) = mrYes then
   begin
    Self.Vyhybky.Delete(pozice);
    LV_Zavery.Items.Delete(Pozice);

    SetLength(Vypustit,0);
    for cyklus := 0 to Self.Vyhybky.Count-1 do
     begin
      SetLength(Vypustit, Length(Vypustit)+1);
      Vypustit[cyklus] := Self.Vyhybky[cyklus].Blok;
     end;//for cyklus

    Self.MakeObls(obls);
    Blky.NactiBlokyDoObjektu(CB_NewZaverBlok, @CB_NewVyhybkaPolozky, @Vypustit, obls, 0, -1);

   end;//if MessageBox
 end;

procedure TF_JCEdit.LV_ZaveryChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
 begin
  if (LV_Zavery.ItemIndex <> -1) then
   begin
    B_ZaveryVyhybek_Delete.Enabled := true;
   end else begin
    B_ZaveryVyhybek_Delete.Enabled := false;
   end;
  B_ZaveryUseku_Delete.Enabled := false;
 end;

procedure TF_JCEdit.LV_UsekyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
 begin
  if (LV_Useky.ItemIndex <> -1) then
   begin
    B_ZaveryUseku_Delete.Enabled := true;
   end else begin
    B_ZaveryUseku_Delete.Enabled := false;
   end;
  B_ZaveryVyhybek_Delete.Enabled := false;
 end;

procedure TF_JCEdit.CB_NavestidloChange(Sender: TObject);
var Vypustit:TArI;
    cyklus, i:Integer;
    obls:TArStr;
    Useky:TArI;
 begin
  if (CB_Navestidlo.ItemIndex <> -1) then
   begin
    JCData.NavestidloBlok := Blky.GetBlkID(CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
    Self.MakeObls(obls);
    SetLength(Vypustit,1);
    Vypustit[0] := Blky.GetBlkID(CB_NavestidloPolozky[CB_Navestidlo.ItemIndex]);
    if (JCData.DalsiNNavaznostTyp > 1) then
     begin
      Blky.NactiBlokyDoObjektu(CB_DalsiNNavaznost,@CB_DalsiNNavaznostPolozky, @Vypustit, obls, 3, JCData.DalsiNNavaznost);
      CB_DalsiNNavaznost.Items.Add('Žádná návaznost');
      CB_DalsiNNavaznost.Items.Add('Trať');
     end else begin
      Blky.NactiBlokyDoObjektu(CB_DalsiNNavaznost,@CB_DalsiNNavaznostPolozky, @Vypustit, obls, 3, -1);
      CB_DalsiNNavaznost.Items.Add('Žádná návaznost');
      CB_DalsiNNavaznost.Items.Add('Trať');
      CB_DalsiNNavaznost.ItemIndex := JCData.DalsiNNavaznostTyp+Length(CB_DalsiNNavaznostPolozky);
     end;

    SetLength(Useky, Self.Useky.Count);
    for i := 0 to Self.Useky.Count-1 do
      Useky[i] := Self.Useky[i];

    Blky.NactiBlokyDoObjektu(CB_NewUsek, @CB_NewUsekPolozky, @Useky, obls, _BLK_USEK, -1, _BLK_TU);

    SetLength(Vypustit,0);
    for cyklus := 0 to Self.Vyhybky.Count-1 do
     begin
      SetLength(Vypustit, Length(Vypustit)+1);
      Vypustit[cyklus] := Self.Vyhybky[cyklus].Blok;
     end;//for cyklus
    Blky.NactiBlokyDoObjektu(CB_NewZaverBlok, @CB_NewVyhybkaPolozky, @Vypustit, obls, 0, -1);
   end;//if CB_Navestidlo.ItemIndex <> -1

  if (Self.CHB_AutoName.Checked) then Self.UpdateJCName();  
 end;

procedure TF_JCEdit.CB_TypCestyChange(Sender: TObject);
 begin
  if (CB_TypCesty.ItemIndex = 1) then
   begin
    CB_Rychlost_DalsiN.Enabled      := false;
    CB_Rychlost_NoDalsiN.Enabled    := false;
    CB_Rychlost_DalsiN.ItemIndex    := 4;
    CB_Rychlost_NoDalsiN.ItemIndex  := 4;
   end else begin
    CB_Rychlost_DalsiN.Enabled      := true;
    CB_Rychlost_NoDalsiN.Enabled    := true;
   end;
 end;

procedure TF_JCEdit.CHB_AdvancedClick(Sender: TObject);
var gb:TGroupBox;
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
var obls:TArStr;
begin
 Self.CB_TratBlok.Enabled := Self.CHB_Trat.Checked;
 Self.CB_TratSmer.Enabled := Self.CHB_Trat.Checked;

 if (Self.CHB_Trat.Checked) then
  begin
   Self.MakeObls(obls);
   Blky.NactiBlokyDoObjektu(Self.CB_TratBlok, @Self.CB_TratPolozky, nil, obls, _BLK_TRAT, Self.JCData.Trat);
   Self.CB_TratSmer.ItemIndex := Integer(Self.JCData.TratSmer)-1;
  end else begin
   Self.CB_TratBlok.ItemIndex := -1;
   Self.CB_TratSmer.ItemIndex := -1;
  end;
end;

procedure TF_JCEdit.MakeObls(var obls:TArStr);
var Blk:TBlk;
    i:Integer;
begin
 Blky.GetBlkByID(JCData.NavestidloBlok, Blk);

 if (Blk = nil) then Exit;
 if (Blk.typ <> _BLK_NAV) then Exit;

 SetLength(obls, (Blk as TBlkNav).OblsRizeni.Count);
 for i := 0 to (Blk as TBlkNav).OblsRizeni.Count-1 do
  obls[i] := (Blk as TBlkNav).OblsRizeni[i].id;
end;

procedure TF_JCEdit.UpdateJCName();
begin
 if ((Self.CB_Navestidlo.ItemIndex <> -1)) then
   Self.E_VCNazev.Text := Blky.GetBlkName(Self.JCData.NavestidloBlok) + ' > ';

 if (Self.Useky.Count <> 0) then
   Self.E_VCNazev.Text := Self.E_VCNazev.Text + Blky.GetBlkName(Self.Useky[Self.Useky.Count-1]);
end;

end.//unit

