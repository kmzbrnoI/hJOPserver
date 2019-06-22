unit fBlkTrat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls, TBlokTrat, TBlokUvazka,
  Generics.Collections, TBloky;

type
  TF_BlkTrat = class(TForm)
    B_Save: TButton;
    B_Storno: TButton;
    GB_UvazkaA: TGroupBox;
    GB_UvazkaB: TGroupBox;
    GB_Trat: TGroupBox;
    SE_Trat_ID: TSpinEdit;
    E_Trat_Name: TEdit;
    L_P02: TLabel;
    L_SCom02: TLabel;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure LV_UsekyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_blk_AddClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure B_Blk_DeleteClick(Sender: TObject);
  private
   NewBlk:Boolean;
   Trat:TBlkTrat;
   UvazkaA:TBlkUvazka;
   UvazkaB:TBlkUvazka;
   OpenIndex:Integer;
   CB_NewTratBlokData:TArI;

    procedure NewBlkOpenForm;
    procedure NormalOpenForm;
    procedure HlavniOpenForm;
  public
    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkTrat: TF_BlkTrat;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, TBlok, TOblRizeni;

{$R *.dfm}

procedure TF_BlkTrat.OpenForm(BlokIndex:Integer);
var Blk:TBlk;
 begin
  Self.OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex, TBlk(Blk));

  if (Blk <> nil) then
   begin
    // tato situace nastava v pripade tvorby noveho bloku
    case (Blk.typ) of
     _BLK_TRAT   : Self.Trat := Blk as TBlkTrat;
     _BLK_UVAZKA : Self.Trat := (Blk as TBlkUvazka).parent as TBlkTrat;
    end;
    Self.UvazkaA := Self.Trat.uvazkaA as TBlkUvazka;
    Self.UvazkaB := Self.Trat.uvazkaB as TBlkUvazka;
   end;//if Blk <> nil

  HlavniOpenForm;
  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;//else NewBlk

  Self.ShowModal;
 end;//procedure

procedure TF_BlkTrat.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkTrat.NewBlkOpenForm;
 begin
  Self.E_Trat_Name.Text  := '';
  Self.E_UA_name.Text    := '';
  Self.E_UB_name.Text    := '';

  Self.SE_Trat_ID.Value  := Blky.GetBlkID(Blky.Cnt-1)+1;
  Self.SE_UA_id.Value    := Blky.GetBlkID(Blky.Cnt-1)+2;
  Self.SE_UB_id.Value    := Blky.GetBlkID(Blky.Cnt-1)+3;

  Self.CB_Trat_ZabZar.ItemIndex := -1;

  Blky.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, nil, nil, _BLK_TU, -1);

  Self.Caption := 'Editace noveho bloku';
  Self.ActiveControl := Self.E_Trat_Name;
 end;//procedure

procedure TF_BlkTrat.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkTratSettings;
    i:Integer;
    obls:TArstr;
    LI:TListItem;
    vypust:TArI;
    oblr:TOR;
 begin
  glob := Self.Trat.GetGlobalSettings();
  Self.E_Trat_Name.Text := glob.name;
  Self.SE_Trat_ID.Value := glob.id;

  glob := Self.UvazkaA.GetGlobalSettings();
  Self.E_UA_name.Text := glob.name;
  Self.SE_UA_id.Value := glob.id;
  for oblr in Self.UvazkaA.OblsRizeni do
    Self.LB_UA_St.Items.Add(oblr.Name);

  glob := Self.UvazkaB.GetGlobalSettings();
  Self.E_UB_name.Text := glob.name;
  Self.SE_UB_id.Value := glob.id;
  for oblr in Self.UvazkaB.OblsRizeni do
    Self.LB_UB_St.Items.Add(oblr.Name);

  settings := Self.Trat.GetSettings();

  SetLength(obls, Self.UvazkaA.OblsRizeni.Count + Self.UvazkaB.OblsRizeni.Count);
  for i := 0 to Self.UvazkaA.OblsRizeni.Count-1 do
    obls[i] := Self.UvazkaA.OblsRizeni[i].id;
  for i := 0 to Self.UvazkaB.OblsRizeni.Count-1 do
    obls[i+Self.UvazkaA.OblsRizeni.Count] := Self.UvazkaB.OblsRizeni[i].id;
  SetLength(vypust, settings.Useky.Count);
  for i := 0 to settings.Useky.Count-1 do
    vypust[i] := settings.Useky[i];
  Blky.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @vypust, obls, _BLK_TU, -1);

  Self.CB_Trat_ZabZar.ItemIndex := Integer(settings.zabzar);

  for i := 0 to settings.Useky.Count-1 do
   begin
    LI := Self.LV_Useky.Items.Add;
    LI.Caption := IntToStr(settings.Useky[i]);
    LI.SubItems.Add(Blky.GetBlkName(settings.Useky[i]));
   end;

  Self.Caption := 'Editace dat bloku '+Self.Trat.name+' (traù)';
 Self.ActiveControl := Self.B_Save;
 end;//procedure

procedure TF_BlkTrat.HlavniOpenForm;
begin
 Self.LV_Useky.Clear();

 Self.LB_UA_St.Clear();
 Self.LB_UB_St.Clear();

 Self.B_Blk_Delete.Enabled := false;
end;

procedure TF_BlkTrat.LV_UsekyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if (Self.LV_Useky.ItemIndex < 0) then
  Self.B_Blk_Delete.Enabled := false
 else
  Self.B_Blk_Delete.Enabled := true;
end;

procedure TF_BlkTrat.B_blk_AddClick(Sender: TObject);
var LI:TListItem;
    useky_vypust:TArI;
    obls:TArStr;
    i:Integer;
begin
 if (F_BlkTrat.CB_NewTratBlok.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte blok!', 'Nelze pokraËovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 LI := Self.LV_Useky.Items.Add;
 LI.Caption := IntToStr(Blky.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex]));
 LI.SubItems.Add(Blky.GetBlkName(Blky.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex])));

 SetLength(useky_vypust, Self.LV_Useky.Items.Count);
 for i := 0 to Self.LV_Useky.Items.Count-1 do
  useky_vypust[i] := StrToInt(Self.LV_Useky.Items.Item[i].Caption);

 if ((Self.UvazkaA <> nil) and (Self.UvazkaB <> nil)) then
  begin
   SetLength(obls, Self.UvazkaA.OblsRizeni.Count + Self.UvazkaB.OblsRizeni.Count);
   for i := 0 to Self.UvazkaA.OblsRizeni.Count-1 do
     obls[i] := Self.UvazkaA.OblsRizeni[i].id;
   for i := 0 to Self.UvazkaB.OblsRizeni.Count-1 do
     obls[i+Self.UvazkaA.OblsRizeni.Count] := Self.UvazkaB.OblsRizeni[i].id;
  end;

 Blky.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @useky_vypust, obls, _BLK_TU, -1);
end;

procedure TF_BlkTrat.B_Blk_DeleteClick(Sender: TObject);
var useky_vypust:TArI;
    obls:TArStr;
    i:Integer;
begin
 if (Self.LV_Useky.Selected <> nil) then
   Self.LV_Useky.Items.Delete(Self.LV_Useky.ItemIndex);

 SetLength(useky_vypust, Self.LV_Useky.Items.Count);
 for i := 0 to Self.LV_Useky.Items.Count-1 do
  useky_vypust[i] := StrToInt(Self.LV_Useky.Items.Item[i].Caption);

 SetLength(obls, Self.UvazkaA.OblsRizeni.Count + Self.UvazkaB.OblsRizeni.Count);
 for i := 0 to Self.UvazkaA.OblsRizeni.Count-1 do
   obls[i] := Self.UvazkaA.OblsRizeni[i].id;
 for i := 0 to Self.UvazkaB.OblsRizeni.Count-1 do
   obls[i+Self.UvazkaA.OblsRizeni.Count] := Self.UvazkaB.OblsRizeni[i].id;

 Blky.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, @useky_vypust, obls, _BLK_TU, -1);
end;

procedure TF_BlkTrat.B_SaveClick(Sender: TObject);
var glob_trat, glob_uvA, glob_uvB:TBlkSettings;
    TratSettings:TBlkTratSettings;
    UvazkaSettings:TBlkUvazkaSettings;
    i:Integer;
    trat,uvazkaA,uvazkaB:Integer;
 begin
  if (Self.NewBlk) then
   begin
    trat    := -1;
    uvazkaA := -1;
    uvazkaB := -1;
   end else begin
    trat    := Blky.GetBlkIndex(Self.Trat.id);
    uvazkaA := Blky.GetBlkIndex(Self.UvazkaA.id);
    uvazkaB := Blky.GetBlkIndex(Self.UvazkaB.id);
   end;

  if ((Self.E_Trat_Name.Text = '') or (Self.E_UA_name.Text = '') or (Self.E_UB_name.Text = '')) then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(Self.SE_Trat_ID.Value, trat)) then
   begin
    Application.MessageBox('ID trati jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(Self.SE_UA_id.Value, uvazkaA)) then
   begin
    Application.MessageBox('ID ˙vazky A jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(Self.SE_UB_id.Value,  uvazkaB)) then
   begin
    Application.MessageBox('ID ˙vazky B jiz bylo definovano na jinem bloku !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CB_Trat_ZabZar.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte typ zabezpeËovacÌho za¯ÌzenÌ trati !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  // trat
  glob_trat.name := Self.E_Trat_Name.Text;
  glob_trat.id   := Self.SE_Trat_ID.Value;
  glob_trat.typ  := _BLK_TRAT;

  // uvazka A
  glob_uvA.name := Self.E_UA_name.Text;
  glob_uvA.id   := Self.SE_UA_id.Value;
  glob_uvA.typ  := _BLK_UVAZKA;

  // uvazka B
  glob_uvB.name := Self.E_UB_name.Text;
  glob_uvB.id   := Self.SE_UB_id.Value;
  glob_uvB.typ  := _BLK_UVAZKA;

  TratSettings.Useky := TList<Integer>.Create();

  if (NewBlk) then
   begin
    Self.Trat := Blky.Add(_BLK_TRAT, glob_trat) as TBlkTrat;
    if (Self.Trat = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok traù !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    Self.UvazkaA  := Blky.Add(_BLK_UVAZKA, glob_uvA) as TBlkUvazka;
    if (Self.UvazkaA = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok ˙vazka A !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    Self.UvazkaB  := Blky.Add(_BLK_UVAZKA, glob_uvB) as TBlkUvazka;
    if (Self.UvazkaB = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok ˙vazka B !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob_trat.poznamka := Self.Trat.poznamka;
    glob_uvA.poznamka  := Self.UvazkaA.poznamka;
    glob_uvB.poznamka  := Self.UvazkaB.poznamka;

    Self.Trat.SetGlobalSettings(glob_trat);
    Self.UvazkaA.SetGlobalSettings(glob_uvA);
    Self.UvazkaB.SetGlobalSettings(glob_uvB);
   end;

  TratSettings.uvazkaA  := Self.SE_UA_id.Value;
  TratSettings.uvazkaB  := Self.SE_UB_id.Value;
  TratSettings.zabzar   := TTratZZ(Self.CB_Trat_ZabZar.ItemIndex);

  TratSettings.Useky.Clear();
  for i := 0 to Self.LV_Useky.Items.Count-1 do
   TratSettings.Useky.Add(StrToInt(Self.LV_Useky.Items.Item[i].Caption));
  Self.Trat.SetSettings(TratSettings);

  UvazkaSettings.parent := Self.SE_Trat_ID.Value;
  Self.UvazkaA.SetSettings(UvazkaSettings);

  UvazkaSettings.parent := Self.SE_Trat_ID.Value;
  Self.UvazkaB.SetSettings(UvazkaSettings);

  Self.Close;
  Self.Trat.Change();
end;

procedure TF_BlkTrat.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_BlkTrat.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  OpenIndex  := -1;
  NewBlk     := false;
  BlokyTableData.UpdateTable();
 end;

end.//unit
