unit fBlkRT;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, fMain, BlockDb, Block, Mask,
  BlockRailwayTrack, StrUtils, BlockTrack, fBlkRTStopEvent, Generics.Collections;

type
  TF_BlkRT = class(TForm)
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
    GB_Zastavka: TGroupBox;
    CHB_Zastavka_Lichy: TCheckBox;
    Label5: TLabel;
    E_Zast_Spr: TEdit;
    Label6: TLabel;
    SE_Zast_DelkaSpr: TSpinEdit;
    Label7: TLabel;
    ME_Zast_Delay: TMaskEdit;
    CHB_SmycBlok: TCheckBox;
    L_Usek33: TLabel;
    GB_Autoblok: TGroupBox;
    Label10: TLabel;
    CHB_NavL: TCheckBox;
    CB_NavL: TComboBox;
    Label11: TLabel;
    CHB_NavS: TCheckBox;
    CB_NavS: TComboBox;
    PC_Zastavka: TPageControl;
    TS_Zast_lichy: TTabSheet;
    TS_Zast_sudy: TTabSheet;
    CHB_Zastavka_Sudy: TCheckBox;
    GB_Speed: TGroupBox;
    LV_Speeds: TListView;
    SE_prechodnost: TSpinEdit;
    B_speed_apply: TButton;
    SE_speed: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_D1Click(Sender: TObject);
    procedure CHB_Zastavka_LichyClick(Sender: TObject);
    procedure CHB_NavLClick(Sender: TObject);
    procedure CHB_NavSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SE_RCS_BoardExit(Sender: TObject);
    procedure B_speed_applyClick(Sender: TObject);
    procedure LV_SpeedsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
   NewBlk: Boolean;
   Blk: TBlkRT;
   OpenIndex: Integer;
   CB_NavData: TArI;
   CB_NavLindex, CB_NavSindex: Integer;
   zastLichy, zastSudy: TF_BlkRTStopEvent;

    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure HlavniOpenForm();

    function GetSpeeds(): TDictionary<Cardinal, Cardinal>;

  public
    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkRT: TF_BlkRT;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, ownStrUtils,
      AreaDb, Booster, Area;

{$R *.dfm}

procedure TF_BlkRT.OpenForm(BlokIndex: Integer);
 begin
  Self.OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.HlavniOpenForm();
  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();
  Self.ShowModal();
 end;

procedure TF_BlkRT.SE_RCS_BoardExit(Sender: TObject);
begin
 Self.SE_Port1.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board1.Value, Self.SE_Port1.Value);
 Self.SE_Port2.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board2.Value, Self.SE_Port1.Value);
 Self.SE_Port3.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board3.Value, Self.SE_Port1.Value);
 Self.SE_Port4.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Board4.Value, Self.SE_Port1.Value);
end;

procedure TF_BlkRT.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blocks.count);
 end;

procedure TF_BlkRT.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  E_Delka.Text := '0';
  CHB_SmycBlok.Checked := false;
  Self.CB_Zesil.ItemIndex := -1;

  Self.SE_Port1.Value  := 0;
  Self.SE_Board1.Value := 1;
  Self.SE_Port2.Value  := 0;
  Self.SE_Board2.Value := 1;
  Self.SE_Port3.Value  := 0;
  Self.SE_Board3.Value := 1;
  Self.SE_Port4.Value  := 0;
  Self.SE_Board4.Value := 1;
  Self.SE_RCS_BoardExit(Self);

  Self.CHB_D1.Checked := true;
  Self.CHB_D1Click(Self.CHB_D1);

  Self.CHB_D2.Checked := false;
  Self.CHB_D1Click(Self.CHB_D2);

  Self.CHB_Zastavka_Lichy.Checked := false;
  Self.CHB_Zastavka_Sudy.Checked := false;
  Self.CHB_Zastavka_LichyClick(Self);

  Blocks.FillCB(Self.CB_NavL, @Self.CB_NavData, nil, nil, btSignal, -1);
  Blocks.FillCB(Self.CB_NavS, nil, nil, nil, btSignal, -1);
  Self.CB_NavLindex := -1;
  Self.CB_NavSindex := -1;

  Self.LV_Speeds.Clear();
  Self.SE_speed.Value := 0;
  Self.SE_prechodnost.Value := 0;

  Self.zastLichy.OpenEmptyForm();
  Self.zastSudy.OpenEmptyForm();

  Self.Caption := 'Nový blok Traťový úsek';
  Self.ActiveControl := E_Nazev;
 end;

procedure TF_BlkRT.NormalOpenForm();
var glob: TBlkSettings;
    TUsettings: TBlkRTSettings;
    Usettings: TBlkTrackSettings;
    i: Integer;
    areas: TArstr;
    area: TArea;
    LI: TListItem;
 begin
  if (Assigned(Self.Blk)) then glob := Self.Blk.GetGlobalSettings();
  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for area in Self.Blk.areas do
    Self.LB_Stanice.Items.Add(area.name);

  SetLength(areas, Self.Blk.areas.Count);
  for i := 0 to Self.Blk.areas.Count-1 do
    areas[i] := Self.Blk.areas[i].id;

  if (Assigned(Self.Blk)) then
   begin
    TUsettings := Self.Blk.GetSettings();
    Usettings  := TBlkTrack(Self.Blk).GetSettings();
   end;

  Self.CHB_D1.Checked := false;
  Self.CHB_D2.Checked := false;
  Self.CHB_D3.Checked := false;
  Self.CHB_D4.Checked := false;

  case (Usettings.RCSAddrs.Count) of
    0, 1: begin
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

  if (Usettings.RCSAddrs.Count > 0) then
   begin
    if (Usettings.RCSAddrs[0].board > Cardinal(Self.SE_Board1.MaxValue)) then
      Self.SE_Board1.MaxValue := 0;
    Self.SE_Port1.MaxValue := 0;

    Self.SE_Port1.Value  := Usettings.RCSAddrs[0].port;
    Self.SE_Board1.Value := Usettings.RCSAddrs[0].board;
   end else begin
    Self.SE_Port1.Value  := 0;
    Self.SE_Board1.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 1) then
   begin
    if (Usettings.RCSAddrs[1].board > Cardinal(Self.SE_Board2.MaxValue)) then
      Self.SE_Board2.MaxValue := 0;
    Self.SE_Port2.MaxValue := 0;

    Self.SE_Port2.Value  := Usettings.RCSAddrs[1].port;
    Self.SE_Board2.Value := Usettings.RCSAddrs[1].board;
   end else begin
    Self.SE_Port2.Value  := 0;
    Self.SE_Board2.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 2) then
   begin
    if (Usettings.RCSAddrs[2].board > Cardinal(Self.SE_Board3.MaxValue)) then
      Self.SE_Board3.MaxValue := 0;
    Self.SE_Port3.MaxValue := 0;

    Self.SE_Port3.Value  := Usettings.RCSAddrs[2].port;
    Self.SE_Board3.Value := Usettings.RCSAddrs[2].board;
   end else begin
    Self.SE_Port3.Value  := 0;
    Self.SE_Board3.Value := 0;
   end;

  if (Usettings.RCSAddrs.Count > 3) then
   begin
    if (Usettings.RCSAddrs[3].board > Cardinal(Self.SE_Board4.MaxValue)) then
      Self.SE_Board4.MaxValue := 0;
    Self.SE_Port4.MaxValue := 0;

    Self.SE_Port4.Value  := Usettings.RCSAddrs[3].port;
    Self.SE_Board4.Value := Usettings.RCSAddrs[3].board;
   end else begin
    Self.SE_Port4.Value  := 0;
    Self.SE_Board4.Value := 0;
   end;

  Self.SE_RCS_BoardExit(Self);

  Self.CB_Zesil.ItemIndex := -1;
  for i := 0 to Boosters.sorted.Count-1 do
   begin
    if (Boosters.sorted[i].id = Usettings.boosterId) then
     begin
      Self.CB_Zesil.ItemIndex := i;
      break;
     end;
   end;

  E_Delka.Text := FloatToStr(Usettings.lenght);
  CHB_SmycBlok.Checked := Usettings.loop;

  Self.CHB_Zastavka_Lichy.Checked := Assigned(TUsettings.stop) and Assigned(TUsettings.stop.evL);
  Self.CHB_Zastavka_Sudy.Checked  := Assigned(TUsettings.stop) and Assigned(TUsettings.stop.evS);
  Self.CHB_Zastavka_LichyClick(Self);

  Blocks.FillCB(Self.CB_NavL, @Self.CB_NavData, nil, nil, btSignal, TUsettings.signalLid);
  Blocks.FillCB(Self.CB_NavS, nil, nil, nil, btSignal, TUsettings.signalSid);
  Self.CB_NavLindex := Self.CB_NavL.ItemIndex;
  Self.CB_NavSindex := Self.CB_NavS.ItemIndex;

  Self.CHB_NavL.Checked := (TUsettings.signalLid <> -1);
  Self.CHB_NavS.Checked := (TUsettings.signalSid <> -1);
  Self.CHB_NavLClick(CHB_NavL);
  Self.CHB_NavSClick(CHB_NavS);

  Self.LV_Speeds.Clear();
  for i in TUsettings.speeds.Keys do
   begin
    LI := Self.LV_Speeds.Items.Add();
    LI.Caption := IntToStr(i);
    LI.SubItems.Add(IntToStr(TUsettings.speeds[i]) + ' km/h');
   end;

  if (TUsettings.stop <> nil) then
   begin
    if (TUsettings.stop.evL <> nil) then
      Self.zastLichy.OpenForm(TUsettings.stop.evL);
    if (TUsettings.stop.evS <> nil) then
      Self.zastSudy.OpenForm(TUsettings.stop.evS);
   end;

  Self.Caption := 'Upravit blok '+glob.name+' (traťový úsek)';
  Self.ActiveControl := Self.B_OK;
 end;

procedure TF_BlkRT.HlavniOpenForm;
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

procedure TF_BlkRT.LV_SpeedsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var str: string;
begin
 if (Self.LV_Speeds.Selected = nil) then
  begin
   Self.SE_prechodnost.Value := 0;
   Self.SE_speed.Value := 0;
  end else begin
   Self.SE_prechodnost.Value := StrToInt(Self.LV_Speeds.Selected.Caption);
   str := Self.LV_Speeds.Selected.SubItems.Strings[0];
   Self.SE_speed.Value := StrToInt(LeftStr(str, Length(str)-5));
  end;
end;

procedure TF_BlkRT.B_speed_applyClick(Sender: TObject);
var LI: TListItem;
    i: Integer;
    prechodnost, speed: Integer;
begin
 prechodnost := Self.SE_prechodnost.Value;
 speed := Self.SE_speed.Value;

 if (speed <= 0) then
  begin
   // delete speed
   for i := 0 to Self.LV_Speeds.Items.Count-1 do
    begin
     if (Self.LV_Speeds.Items[i].Caption = IntToStr(prechodnost)) then
      begin
       Self.LV_Speeds.Items.Delete(i);
       Exit();
      end;
    end;
   Exit();
  end;

 for LI in Self.LV_Speeds.Items do
  begin
   if (LI.Caption = IntToStr(prechodnost)) then
    begin
     LI.SubItems.Strings[0] := IntToStr(speed) + ' km/h';
     Exit();
    end;
  end;

 LI := Self.LV_Speeds.Items.Add();
 LI.Caption := IntToStr(prechodnost);
 LI.SubItems.Add(IntToStr(speed) + ' km/h');
end;

procedure TF_BlkRT.B_StornoClick(Sender: TObject);
 begin
  Self.Close;
 end;

procedure TF_BlkRT.B_OKClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkTrackSettings;
    TUsettings: TBlkRTSettings;
    str: string;
    speeds: TDictionary<Cardinal, Cardinal>;
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
  if ((Self.CHB_NavL.Checked) and (Self.CB_NavL.ItemIndex = -1)) then
   begin
    Application.MessageBox('Vyberte návěstidlo kryjící úsek v lichém směru!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if ((Self.CHB_NavS.Checked) and (Self.CB_NavS.ItemIndex = -1)) then
   begin
    Application.MessageBox('Vyberte návěstidlo kryjící úsek v sudém směru!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  if (CHB_Zastavka_Lichy.Checked) then
   begin
    str := Self.zastLichy.Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar('Zastavovací událost zastávky v lichém směru:'+#13#10+str), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;

  if (CHB_Zastavka_Sudy.Checked) then
   begin
    str := Self.zastSudy.Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar('Zastavovací událost zastávky v sudém směru:'+#13#10+str), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;

  try
   speeds := Self.GetSpeeds();
  except
   on E: Exception do
    begin
     Application.MessageBox(PChar('Nepodařilo se načíst rychlosti:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
  end;

  if (speeds.Keys.Count = 0) then
   begin
    speeds.Free();
    Application.MessageBox('Rychlostní tabulka musí mít alespoň jednu rychlost!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btRT;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkRT;
    except
      on E: Exception do
       begin
        speeds.Free();
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  // ukladani dat
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
  settings.boosterId := Boosters.sorted[Self.CB_Zesil.ItemIndex].id;
  settings.maxTrains := 1;

  TUSettings.speeds := speeds;

  if (Self.CHB_NavL.Checked) then
    TUsettings.signalLid := Blocks.GetBlkID(Self.CB_NavData[Self.CB_NavL.ItemIndex])
  else
    TUsettings.signalLid := -1;

  if (Self.CHB_NavS.Checked) then
    TUsettings.signalSid := Blocks.GetBlkID(Self.CB_NavData[Self.CB_NavS.ItemIndex])
  else
    TUsettings.signalSid := -1;


  if ((Self.CHB_Zastavka_Lichy.Checked) or (Self.CHB_Zastavka_Sudy.Checked)) then
   begin
    TUsettings.stop := TBlkRTStop.Create();
    TUsettings.stop.trainTypeRe.Compile('^'+Self.E_Zast_Spr.Text+'$', false);
    TUsettings.stop.maxLength := Self.SE_Zast_DelkaSpr.Value;
    try
      TUsettings.stop.delay := EncodeTime(0, StrToInt(LeftStr(Self.ME_Zast_Delay.Text, 2)),
                                              StrToInt(RightStr(Self.ME_Zast_Delay.Text, 2)), 0);
    except
      speeds.Free();
      TUsettings.stop.Free();
      Application.MessageBox('Nesprávně zadaný čas čekání v zastávce!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    if (Self.CHB_Zastavka_Lichy.Checked) then
      TUsettings.stop.evL := Self.zastLichy.GetEvent()
    else
      TUsettings.stop.evL := nil;

    if (Self.CHB_Zastavka_Sudy.Checked) then
      TUsettings.stop.evS := Self.zastSudy.GetEvent()
    else
      TUsettings.stop.evS := nil;
   end else
     TUsettings.stop := nil;

  settings.houkEvL := TBlkTrack(Self.Blk).GetSettings().houkEvL;
  settings.houkEvS := TBlkTrack(Self.Blk).GetSettings().houkEvS;

  Self.Blk.SetSettings(TUsettings);
  (Self.Blk as TBlkTrack).SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkRT.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;

procedure TF_BlkRT.FormCreate(Sender: TObject);
begin
 Self.zastLichy := TF_BlkRTStopEvent.Create(Self.TS_Zast_lichy);
 Self.zastLichy.Parent := Self.TS_Zast_lichy;
 Self.zastLichy.Show();

 Self.zastSudy := TF_BlkRTStopEvent.Create(Self.TS_Zast_lichy);
 Self.zastSudy.Parent := Self.TS_Zast_sudy;
 Self.zastSudy.Show();
end;

procedure TF_BlkRT.FormDestroy(Sender: TObject);
begin
 Self.zastLichy.Free();
 Self.zastSudy.Free();
end;

procedure TF_BlkRT.CHB_D1Click(Sender: TObject);
 begin
  case ((Sender as TCheckBox).Tag) of
   1: begin
    Self.SE_Port1.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board1.Enabled := (Sender as TCheckBox).Checked;
   end;

   2: begin
    Self.SE_Port2.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board2.Enabled := (Sender as TCheckBox).Checked;
   end;

   3: begin
    Self.SE_Port3.Enabled  := (Sender as TCheckBox).Checked;
    Self.SE_Board3.Enabled := (Sender as TCheckBox).Checked;
   end;

   4: begin
    Self.SE_Port4.Enabled  := (Sender as TCheckBox).Checked;
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

procedure TF_BlkRT.CHB_NavLClick(Sender: TObject);
begin
 Self.CB_NavL.Enabled := Self.CHB_NavL.Checked;
 if (not Self.CHB_NavL.Checked) then
   Self.CB_NavL.ItemIndex := -1
 else
   Self.CB_NavL.ItemIndex := Self.CB_NavLindex;
end;

procedure TF_BlkRT.CHB_NavSClick(Sender: TObject);
begin
 Self.CB_NavS.Enabled := Self.CHB_NavS.Checked;
 if (not Self.CHB_NavS.Checked) then
   Self.CB_NavS.ItemIndex := -1
 else
   Self.CB_NavS.ItemIndex := Self.CB_NavSindex;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkRT.CHB_Zastavka_LichyClick(Sender: TObject);
var zast: TBlkRTStop;
begin
 if ((Self.CHB_Zastavka_Lichy.Checked) or (Self.CHB_Zastavka_Sudy.Checked)) then
  begin
   Self.E_Zast_Spr.Enabled := true;
   Self.SE_Zast_DelkaSpr.Enabled := true;
   Self.ME_Zast_Delay.Enabled := true;
   Self.PC_Zastavka.Enabled := true;

   if ((Assigned(Self.Blk)) and (Self.Blk.GetSettings.stop <> nil)) then
    begin
     zast := Self.Blk.GetSettings.stop;
     Self.E_Zast_Spr.Text := Copy(zast.trainTypeRe.Pattern, 2, Length(zast.trainTypeRe.Pattern)-2);
     Self.SE_Zast_DelkaSpr.Value := zast.maxLength;
     Self.ME_Zast_Delay.Text := FormatDateTime('nn:ss', zast.delay);
    end;
  end else begin
   Self.E_Zast_Spr.Enabled := false;
   Self.SE_Zast_DelkaSpr.Enabled := false;
   Self.ME_Zast_Delay.Enabled := false;
   Self.PC_Zastavka.Enabled := false;
  end;

 Self.TS_Zast_lichy.TabVisible := Self.CHB_Zastavka_Lichy.Checked;
 Self.TS_Zast_sudy.TabVisible  := Self.CHB_Zastavka_Sudy.Checked;

 if (((not Self.CHB_Zastavka_Lichy.Checked) and ((not Self.CHB_Zastavka_Sudy.Checked))) or (not Assigned(Self.Blk))) then
  begin
   Self.E_Zast_Spr.Text := '.*';
   Self.SE_Zast_DelkaSpr.Value := 0;
   Self.ME_Zast_Delay.Text := '00:00';
   Self.PC_Zastavka.Enabled := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TF_BlkRT.GetSpeeds(): TDictionary<Cardinal, Cardinal>;
var LI: TListItem;
    speed: string;
begin
 Result := TDictionary<Cardinal, Cardinal>.Create();
 for LI in Self.LV_Speeds.Items do
  begin
   speed := LeftStr(LI.SubItems.Strings[0], Length(LI.SubItems.Strings[0])-5);
   Result.Add(StrToInt(LI.Caption), StrToInt(speed));
  end;
end;

end.//unit
