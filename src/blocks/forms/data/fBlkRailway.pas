﻿unit fBlkRailway;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, BlockRailway, BlockLinker, Generics.Collections, BlockDb;

type
  TF_BlkRailway = class(TForm)
    B_Save: TButton;
    B_Storno: TButton;
    GB_UvazkaA: TGroupBox;
    GB_UvazkaB: TGroupBox;
    GB_Trat: TGroupBox;
    SE_Trat_ID: TSpinEdit;
    E_Trat_Name: TEdit;
    L_Name: TLabel;
    L_ID: TLabel;
    GB_TratBlk: TGroupBox;
    B_Blk_Delete: TButton;
    GB_NewBlk: TGroupBox;
    B_blk_Add: TButton;
    CB_NewTratBlok: TComboBox;
    Label1: TLabel;
    E_UA_name: TEdit;
    SE_UA_id: TSpinEdit;
    LB_UA_St: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    LB_UB_St: TListBox;
    SE_UB_id: TSpinEdit;
    E_UB_name: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CB_Trat_ZabZar: TComboBox;
    Label7: TLabel;
    LV_Useky: TListView;
    Label8: TLabel;
    CB_Navestidla: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure LV_UsekyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_blk_AddClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure B_Blk_DeleteClick(Sender: TObject);
  private
   NewBlk: Boolean;
   Trat: TBlkRailway;
   UvazkaA: TBlkLinker;
   UvazkaB: TBlkLinker;
   OpenIndex: Integer;
   CB_NewTratBlokData: TArI;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkRailway: TF_BlkRailway;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, Block, TOblRizeni;

{$R *.dfm}

procedure TF_BlkRailway.OpenForm(BlokIndex: Integer);
var Blk: TBlk;
 begin
  Self.OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Blk));

  if (Blk <> nil) then
   begin
    // tato situace nastava v pripade tvorby noveho bloku
    case (Blk.typ) of
     btRailway   : Self.Trat := Blk as TBlkRailway;
     btLinker : Self.Trat := (Blk as TBlkLinker).parent as TBlkRailway;
    end;
    Self.UvazkaA := Self.Trat.linkerA as TBlkLinker;
    Self.UvazkaB := Self.Trat.linkerB as TBlkLinker;
   end;//if Blk <> nil

  HlavniOpenForm;
  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;//else NewBlk

  Self.ShowModal;
 end;

procedure TF_BlkRailway.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blocks.count);
 end;

procedure TF_BlkRailway.NewBlkOpenForm;
 begin
  Self.E_Trat_Name.Text  := '';
  Self.E_UA_name.Text    := '';
  Self.E_UB_name.Text    := '';

  Self.SE_Trat_ID.Value  := Blocks.GetBlkID(Blocks.count-1)+1;
  Self.SE_UA_id.Value    := Blocks.GetBlkID(Blocks.count-1)+2;
  Self.SE_UB_id.Value    := Blocks.GetBlkID(Blocks.count-1)+3;

  Self.CB_Trat_ZabZar.ItemIndex := -1;
  Self.CB_Navestidla.ItemIndex := -1;

  Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, nil, nil, btRT, -1);

  Self.Caption := 'Editace nového bloku';
  Self.ActiveControl := Self.E_Trat_Name;
 end;

procedure TF_BlkRailway.NormalOpenForm;
var glob: TBlkSettings;
    settings: TBlkRailwaySettings;
    i: Integer;
    obls: TArstr;
    LI: TListItem;
    vypust: TArI;
    oblr: TOR;
 begin
  glob := Self.Trat.GetGlobalSettings();
  Self.E_Trat_Name.Text := glob.name;
  Self.SE_Trat_ID.Value := glob.id;

  glob := Self.UvazkaA.GetGlobalSettings();
  Self.E_UA_name.Text := glob.name;
  Self.SE_UA_id.Value := glob.id;
  for oblr in Self.UvazkaA.stations do
    Self.LB_UA_St.Items.Add(oblr.Name);

  glob := Self.UvazkaB.GetGlobalSettings();
  Self.E_UB_name.Text := glob.name;
  Self.SE_UB_id.Value := glob.id;
  for oblr in Self.UvazkaB.stations do
    Self.LB_UB_St.Items.Add(oblr.Name);

  settings := Self.Trat.GetSettings();

  SetLength(obls, Self.UvazkaA.stations.Count + Self.UvazkaB.stations.Count);
  for i := 0 to Self.UvazkaA.stations.Count-1 do
    obls[i] := Self.UvazkaA.stations[i].id;
  for i := 0 to Self.UvazkaB.stations.Count-1 do
    obls[i+Self.UvazkaA.stations.Count] := Self.UvazkaB.stations[i].id;
  SetLength(vypust, settings.trackIds.Count);
  for i := 0 to settings.trackIds.Count-1 do
    vypust[i] := settings.trackIds[i];
  Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @vypust, obls, btRT, -1);

  case (settings.rType) of
   TRailwayType.permanent : Self.CB_Trat_ZabZar.ItemIndex := 0;
   TRailwayType.request : Self.CB_Trat_ZabZar.ItemIndex := 1;
  end;

  Self.CB_Navestidla.ItemIndex := Integer(settings.signals);

  for i := 0 to settings.trackIds.Count-1 do
   begin
    LI := Self.LV_Useky.Items.Add;
    LI.Caption := IntToStr(settings.trackIds[i]);
    LI.SubItems.Add(Blocks.GetBlkName(settings.trackIds[i]));
   end;

  Self.Caption := 'Editace dat bloku '+Self.Trat.name+' (trať)';
 Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkRailway.HlavniOpenForm;
begin
 Self.LV_Useky.Clear();

 Self.LB_UA_St.Clear();
 Self.LB_UB_St.Clear();

 Self.B_Blk_Delete.Enabled := false;
end;

procedure TF_BlkRailway.LV_UsekyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if (Self.LV_Useky.ItemIndex < 0) then
  Self.B_Blk_Delete.Enabled := false
 else
  Self.B_Blk_Delete.Enabled := true;
end;

procedure TF_BlkRailway.B_blk_AddClick(Sender: TObject);
var LI: TListItem;
    useky_vypust: TArI;
    obls: TArStr;
    i: Integer;
begin
 if (F_BlkRailway.CB_NewTratBlok.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte blok!', 'Nelze pokračovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 LI := Self.LV_Useky.Items.Add;
 LI.Caption := IntToStr(Blocks.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex]));
 LI.SubItems.Add(Blocks.GetBlkName(Blocks.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex])));

 SetLength(useky_vypust, Self.LV_Useky.Items.Count);
 for i := 0 to Self.LV_Useky.Items.Count-1 do
  useky_vypust[i] := StrToInt(Self.LV_Useky.Items.Item[i].Caption);

 if ((Self.UvazkaA <> nil) and (Self.UvazkaB <> nil)) then
  begin
   SetLength(obls, Self.UvazkaA.stations.Count + Self.UvazkaB.stations.Count);
   for i := 0 to Self.UvazkaA.stations.Count-1 do
     obls[i] := Self.UvazkaA.stations[i].id;
   for i := 0 to Self.UvazkaB.stations.Count-1 do
     obls[i+Self.UvazkaA.stations.Count] := Self.UvazkaB.stations[i].id;
  end;

 Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @useky_vypust, obls, btRT, -1);
end;

procedure TF_BlkRailway.B_Blk_DeleteClick(Sender: TObject);
var useky_vypust: TArI;
    obls: TArStr;
    i: Integer;
begin
 if (Self.LV_Useky.Selected <> nil) then
   Self.LV_Useky.Items.Delete(Self.LV_Useky.ItemIndex);

 SetLength(useky_vypust, Self.LV_Useky.Items.Count);
 for i := 0 to Self.LV_Useky.Items.Count-1 do
  useky_vypust[i] := StrToInt(Self.LV_Useky.Items.Item[i].Caption);

 SetLength(obls, Self.UvazkaA.stations.Count + Self.UvazkaB.stations.Count);
 for i := 0 to Self.UvazkaA.stations.Count-1 do
   obls[i] := Self.UvazkaA.stations[i].id;
 for i := 0 to Self.UvazkaB.stations.Count-1 do
   obls[i+Self.UvazkaA.stations.Count] := Self.UvazkaB.stations[i].id;

 Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @useky_vypust, obls, btRT, -1);
end;

procedure TF_BlkRailway.B_SaveClick(Sender: TObject);
var glob_trat, glob_uvA, glob_uvB: TBlkSettings;
    TratSettings: TBlkRailwaySettings;
    UvazkaSettings: TBlkLinkerSettings;
    trat, uvazkaA, uvazkaB: Integer;
    LI: TListItem;
 begin
  if (Self.NewBlk) then
   begin
    trat    := -1;
    uvazkaA := -1;
    uvazkaB := -1;
   end else begin
    trat    := Blocks.GetBlkIndex(Self.Trat.id);
    uvazkaA := Blocks.GetBlkIndex(Self.UvazkaA.id);
    uvazkaB := Blocks.GetBlkIndex(Self.UvazkaB.id);
   end;

  if ((Self.E_Trat_Name.Text = '') or (Self.E_UA_name.Text = '') or (Self.E_UB_name.Text = '')) then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blocks.IsBlok(Self.SE_Trat_ID.Value, trat)) then
   begin
    Application.MessageBox('ID trati jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blocks.IsBlok(Self.SE_UA_id.Value, uvazkaA)) then
   begin
    Application.MessageBox('ID úvazky A jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blocks.IsBlok(Self.SE_UB_id.Value,  uvazkaB)) then
   begin
    Application.MessageBox('ID úvazky B jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CB_Trat_ZabZar.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte typ zabezpečovacího zařízení trati !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CB_Navestidla.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte chování návěstidel trati !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  // trat
  glob_trat.name := Self.E_Trat_Name.Text;
  glob_trat.id   := Self.SE_Trat_ID.Value;
  glob_trat.typ  := btRailway;

  // uvazka A
  glob_uvA.name := Self.E_UA_name.Text;
  glob_uvA.id   := Self.SE_UA_id.Value;
  glob_uvA.typ  := btLinker;

  // uvazka B
  glob_uvB.name := Self.E_UB_name.Text;
  glob_uvB.id   := Self.SE_UB_id.Value;
  glob_uvB.typ  := btLinker;

  TratSettings.trackIds := TList<Integer>.Create();

  if (NewBlk) then
   begin
    try
      Self.Trat := Blocks.Add(btRailway, glob_trat) as TBlkRailway;
      Self.UvazkaA := Blocks.Add(btLinker, glob_uvA) as TBlkLinker;
      Self.UvazkaB  := Blocks.Add(btLinker, glob_uvB) as TBlkLinker;
    except
      on E: Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob_trat.note := Self.Trat.note;
    glob_uvA.note  := Self.UvazkaA.note;
    glob_uvB.note  := Self.UvazkaB.note;

    Self.Trat.SetGlobalSettings(glob_trat);
    Self.UvazkaA.SetGlobalSettings(glob_uvA);
    Self.UvazkaB.SetGlobalSettings(glob_uvB);
   end;

  TratSettings.linkerA := Self.SE_UA_id.Value;
  TratSettings.linkerB := Self.SE_UB_id.Value;

  case (Self.CB_Trat_ZabZar.ItemIndex) of
   0 : TratSettings.rType := TRailwayType.permanent;
   1 : TratSettings.rType := TRailwayType.request;
  end;

  TratSettings.signals := TRailwaySignals(Self.CB_Navestidla.ItemIndex);

  TratSettings.trackIds.Clear();
  for LI in Self.LV_Useky.Items do
    TratSettings.trackIds.Add(StrToInt(LI.Caption));
  Self.Trat.SetSettings(TratSettings);

  UvazkaSettings.parent := Self.SE_Trat_ID.Value;
  Self.UvazkaA.SetSettings(UvazkaSettings);

  UvazkaSettings.parent := Self.SE_Trat_ID.Value;
  Self.UvazkaB.SetSettings(UvazkaSettings);

  Self.Close();
  Self.Trat.Change();
end;

procedure TF_BlkRailway.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_BlkRailway.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;

end.//unit