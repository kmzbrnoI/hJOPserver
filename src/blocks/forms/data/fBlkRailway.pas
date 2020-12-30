unit fBlkRailway;

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
   new: Boolean;
   railway: TBlkRailway;
   linkerA: TBlkLinker;
   linkerB: TBlkLinker;
   blkIndex: Integer;
   CB_NewTratBlokData: TArI;

    procedure NewBlkOpenForm();
    procedure EditBlkOpenForm();
    procedure CommonOpenForm();

    procedure FillCBNewTratBlok();

  public
    procedure EditBlk(BlokIndex: Integer);
    procedure NewBlk();

  end;

var
  F_BlkRailway: TF_BlkRailway;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BoosterDb, DataBloky, Block, TOblRizeni;

{$R *.dfm}

procedure TF_BlkRailway.EditBlk(BlokIndex: Integer);
var Blk: TBlk;
 begin
  Self.blkIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Blk));

  if (Blk <> nil) then
   begin
    // tato situace nastava v pripade tvorby noveho bloku
    case (Blk.typ) of
     btRailway: Self.railway := Blk as TBlkRailway;
     btLinker: Self.railway := (Blk as TBlkLinker).parent as TBlkRailway;
    end;
    Self.linkerA := Self.railway.linkerA as TBlkLinker;
    Self.linkerB := Self.railway.linkerB as TBlkLinker;
   end;

  Self.CommonOpenForm();
  if (new) then
    Self.NewBlkOpenForm()
  else
    Self.EditBlkOpenForm();

  Self.ShowModal();
 end;

procedure TF_BlkRailway.NewBlk();
 begin
  Self.new := true;
  Self.EditBlk(Blocks.count);
 end;

procedure TF_BlkRailway.NewBlkOpenForm();
 begin
  Self.E_Trat_Name.Text := '';
  Self.E_UA_name.Text := '';
  Self.E_UB_name.Text := '';

  Self.SE_Trat_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  Self.SE_UA_id.Value := Blocks.GetBlkID(Blocks.count-1)+2;
  Self.SE_UB_id.Value := Blocks.GetBlkID(Blocks.count-1)+3;

  Self.CB_Trat_ZabZar.ItemIndex := -1;
  Self.CB_Navestidla.ItemIndex := -1;

  Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @CB_NewTratBlokData, nil, nil, btRT, -1);

  Self.Caption := 'Nový blok Trať';
  Self.ActiveControl := Self.E_Trat_Name;
 end;

procedure TF_BlkRailway.EditBlkOpenForm();
var glob: TBlkSettings;
    settings: TBlkRailwaySettings;
    id: Integer;
    LI: TListItem;
    oblr: TOR;
 begin
  glob := Self.railway.GetGlobalSettings();
  Self.E_Trat_Name.Text := glob.name;
  Self.SE_Trat_ID.Value := glob.id;

  glob := Self.linkerA.GetGlobalSettings();
  Self.E_UA_name.Text := glob.name;
  Self.SE_UA_id.Value := glob.id;
  for oblr in Self.linkerA.stations do
    Self.LB_UA_St.Items.Add(oblr.Name);

  glob := Self.linkerB.GetGlobalSettings();
  Self.E_UB_name.Text := glob.name;
  Self.SE_UB_id.Value := glob.id;
  for oblr in Self.linkerB.stations do
    Self.LB_UB_St.Items.Add(oblr.Name);

  settings := Self.railway.GetSettings();

  Self.FillCBNewTratBlok();

  case (settings.rType) of
   TRailwayType.permanent : Self.CB_Trat_ZabZar.ItemIndex := 0;
   TRailwayType.request : Self.CB_Trat_ZabZar.ItemIndex := 1;
  end;

  Self.CB_Navestidla.ItemIndex := Integer(settings.signals);

  for id in settings.trackIds do
   begin
    LI := Self.LV_Useky.Items.Add;
    LI.Caption := IntToStr(id);
    LI.SubItems.Add(Blocks.GetBlkName(id));
   end;

  Self.Caption := 'Upravit blok '+Self.railway.name+' (trať)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkRailway.FillCBNewTratBlok();
var trackIgnore: TArI;
    obls: TArStr;
    i: Integer;
begin
 SetLength(trackIgnore, Self.LV_Useky.Items.Count);
 for i := 0 to Self.LV_Useky.Items.Count-1 do
   trackIgnore[i] := StrToInt(Self.LV_Useky.Items.Item[i].Caption);

 if ((Self.linkerA <> nil) and (Self.linkerB <> nil)) then
  begin
   SetLength(obls, Self.linkerA.stations.Count + Self.linkerB.stations.Count);
   for i := 0 to Self.linkerA.stations.Count-1 do
     obls[i] := Self.linkerA.stations[i].id;
   for i := 0 to Self.linkerB.stations.Count-1 do
     obls[i+Self.linkerA.stations.Count] := Self.linkerB.stations[i].id;
  end;

 Blocks.NactiBlokyDoObjektu(Self.CB_NewTratBlok, @Self.CB_NewTratBlokData, @trackIgnore, obls, btRT, -1);
end;

procedure TF_BlkRailway.CommonOpenForm();
begin
 Self.LV_Useky.Clear();
 Self.LB_UA_St.Clear();
 Self.LB_UB_St.Clear();
 Self.B_Blk_Delete.Enabled := false;
end;

procedure TF_BlkRailway.LV_UsekyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_Blk_Delete.Enabled := (Self.LV_Useky.ItemIndex > -1);
end;

procedure TF_BlkRailway.B_blk_AddClick(Sender: TObject);
var LI: TListItem;
begin
 if (F_BlkRailway.CB_NewTratBlok.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte blok!', 'Nelze pokračovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 LI := Self.LV_Useky.Items.Add();
 LI.Caption := IntToStr(Blocks.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex]));
 LI.SubItems.Add(Blocks.GetBlkName(Blocks.GetBlkID(CB_NewTratBlokData[Self.CB_NewTratBlok.ItemIndex])));

 Self.FillCBNewTratBlok();
end;

procedure TF_BlkRailway.B_Blk_DeleteClick(Sender: TObject);
begin
 Self.LV_Useky.DeleteSelected();
 Self.FillCBNewTratBlok();
end;

procedure TF_BlkRailway.B_SaveClick(Sender: TObject);
var globRailway, globLinkerA, globLinkerB: TBlkSettings;
    rSettings: TBlkRailwaySettings;
    linkerSettings: TBlkLinkerSettings;
    railway, linkerA, linkerB: Integer;
    LI: TListItem;
 begin
  if (Self.new) then
   begin
    railway := -1;
    linkerA := -1;
    linkerB := -1;
   end else begin
    railway := Blocks.GetBlkIndex(Self.railway.id);
    linkerA := Blocks.GetBlkIndex(Self.linkerA.id);
    linkerB := Blocks.GetBlkIndex(Self.linkerB.id);
   end;

  if ((Self.E_Trat_Name.Text = '') or (Self.E_UA_name.Text = '') or (Self.E_UB_name.Text = '')) then
   begin
    Application.MessageBox('Vyplnte nazev bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(Self.SE_Trat_ID.Value, railway)) then
   begin
    Application.MessageBox('ID trati jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(Self.SE_UA_id.Value, linkerA)) then
   begin
    Application.MessageBox('ID úvazky A jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(Self.SE_UB_id.Value,  linkerB)) then
   begin
    Application.MessageBox('ID úvazky B jiz bylo definovano na jinem bloku !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Self.CB_Trat_ZabZar.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte typ zabezpečovacího zařízení trati !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Self.CB_Navestidla.ItemIndex < 0) then
   begin
    Application.MessageBox('Vyberte chování návěstidel trati !','Nelze ulozit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  // trat
  globRailway.name := Self.E_Trat_Name.Text;
  globRailway.id := Self.SE_Trat_ID.Value;
  globRailway.typ := btRailway;

  // uvazka A
  globLinkerA.name := Self.E_UA_name.Text;
  globLinkerA.id := Self.SE_UA_id.Value;
  globLinkerA.typ := btLinker;

  // uvazka B
  globLinkerB.name := Self.E_UB_name.Text;
  globLinkerB.id := Self.SE_UB_id.Value;
  globLinkerB.typ := btLinker;

  rSettings.trackIds := TList<Integer>.Create();

  if (new) then
   begin
    try
      Self.railway := Blocks.Add(globRailway) as TBlkRailway;
      Self.linkerA := Blocks.Add(globLinkerA) as TBlkLinker;
      Self.linkerB := Blocks.Add(globLinkerB) as TBlkLinker;
    except
      on E: Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    globRailway.note := Self.railway.note;
    globLinkerA.note  := Self.linkerA.note;
    globLinkerB.note  := Self.linkerB.note;

    Self.railway.SetGlobalSettings(globRailway);
    Self.linkerA.SetGlobalSettings(globLinkerA);
    Self.linkerB.SetGlobalSettings(globLinkerB);
   end;

  rSettings.linkerA := Self.SE_UA_id.Value;
  rSettings.linkerB := Self.SE_UB_id.Value;

  case (Self.CB_Trat_ZabZar.ItemIndex) of
   0: rSettings.rType := TRailwayType.permanent;
   1: rSettings.rType := TRailwayType.request;
  end;

  rSettings.signals := TRailwaySignals(Self.CB_Navestidla.ItemIndex);

  rSettings.trackIds.Clear();
  for LI in Self.LV_Useky.Items do
    rSettings.trackIds.Add(StrToInt(LI.Caption));
  Self.railway.SetSettings(rSettings);

  linkerSettings.parent := Self.SE_Trat_ID.Value;
  Self.linkerA.SetSettings(linkerSettings);

  linkerSettings.parent := Self.SE_Trat_ID.Value;
  Self.linkerB.SetSettings(linkerSettings);

  Self.Close();
  Self.railway.Change();
 end;

procedure TF_BlkRailway.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkRailway.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  Self.blkIndex := -1;
  Self.new := false;
  BlokyTableData.UpdateTable();
 end;

end.//unit
