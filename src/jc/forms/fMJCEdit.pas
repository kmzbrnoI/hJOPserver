unit fMJCEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, TechnologieMultiJC, Generics.Collections,
  Spin, TBloky;

type
  TF_MJCEdit = class(TForm)
    L_VC_01: TLabel;
    CHB_AutoName: TCheckBox;
    E_VCNazev: TEdit;
    GB_JCs: TGroupBox;
    GB_JC_New: TGroupBox;
    CB_JC_Add: TComboBox;
    B_JC_Add: TButton;
    LV_JCs: TListView;
    B_JC_Remove: TButton;
    GB_VB: TGroupBox;
    GroupBox2: TGroupBox;
    CB_VB_New: TComboBox;
    B_VB_New: TButton;
    LV_VBs: TListView;
    B_VB_Remove: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    SE_ID: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure B_JC_AddClick(Sender: TObject);
    procedure B_VB_NewClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure LV_JCsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LV_VBsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_JC_RemoveClick(Sender: TObject);
    procedure B_VB_RemoveClick(Sender: TObject);
  private
    openMJC: TMultiJC;
    new: Boolean;

     JCs: TList<Integer>;
     vb: TList<Integer>;

     CB_VB_indexes: TArI;
     CB_JC_ids: TArI;

    procedure UpdateJCCb();
    procedure UpdateVBCb();

    procedure NormalOpenForm();
    procedure EmptyOpenForm();

    procedure MakeObls(var obls: TArStr);

  public
    procedure EditMJC(mJC: TMultiJC);
    procedure NewMJC(template: TMultiJC);
  end;

var
  F_MJCEdit: TF_MJCEdit;

implementation

{$R *.dfm}

uses TJCDatabase, TechnologieJC, TBlok, TOblRizeni,
      TBlockSignal, TMultiJCDatabase, DataMultiJC;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Self.openMJC := nil;
 Self.new := false;
 Self.JCs.Clear();
 Self.vb.Clear();
end;

procedure TF_MJCEdit.FormCreate(Sender: TObject);
begin
 Self.openMJC := nil;
 Self.new := false;
 Self.JCs := TList<Integer>.Create();
 Self.vb  := TList<Integer>.Create();
end;

procedure TF_MJCEdit.FormDestroy(Sender: TObject);
begin
 Self.JCs.Free();
 Self.vb.Free();
end;

procedure TF_MJCEdit.LV_JCsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var i: Integer;
begin
 Self.B_JC_Remove.Enabled := (Self.LV_JCs.Selected <> nil);

 if ((Self.LV_JCs.Selected = nil) or (Self.LV_JCs.ItemIndex >= Self.JCs.Count)) then
  begin
   Self.CB_JC_Add.ItemIndex := -1;
   Exit();
  end;

 for i := 0 to Length(Self.CB_JC_ids)-1 do
   if (Self.CB_JC_ids[i] = Self.JCs[Self.LV_JCs.ItemIndex]) then
     Self.CB_JC_Add.ItemIndex := i;
end;

procedure TF_MJCEdit.LV_VBsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var i: Integer;
begin
 Self.B_VB_Remove.Enabled := (Self.LV_VBs.Selected <> nil);

 if ((Self.LV_VBs.Selected = nil) or (Self.LV_VBs.ItemIndex >= Self.vb.Count)) then
  begin
   Self.CB_VB_New.ItemIndex := -1;
   Exit();
  end;

 for i := 0 to Length(Self.CB_VB_indexes)-1 do
   if (Blky.GetBlkID(Self.CB_VB_indexes[i]) = Self.vb[Self.LV_VBs.ItemIndex]) then
     Self.CB_VB_New.ItemIndex := i;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.EditMJC(mJC: TMultiJC);
begin
 Self.openMJC := mJC;

 Self.B_JC_Remove.Enabled := false;
 Self.B_VB_Remove.Enabled := false;

 Self.JCs.Clear();
 Self.vb.Clear();

 Self.LV_JCs.Clear();
 Self.LV_VBs.Clear();

 if (mJC = nil) then
  Self.EmptyOpenForm()
 else
  Self.NormalOpenForm();

 Self.ShowModal();
end;

procedure TF_MJCEdit.NewMJC(template: TMultiJC);
begin
 Self.new := true;
 Self.EditMJC(template);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.NormalOpenForm();
var jcid, vbi: Integer;
    LI: TListItem;
    JC: TJC;
    Blk: TBlk;
begin
 if (Self.new) then
   Self.SE_ID.Value := Self.openMJC.id+1
 else
   Self.SE_ID.Value := Self.openMJC.id;

 Self.E_VCNazev.Text := Self.openMJC.Nazev;

 for jcid in Self.openMJC.data.JCs do
  begin
   JC := JCDb.GetJCByID(jcid);
   if (JC = nil) then continue;
   LI := Self.LV_JCs.Items.Add;
   LI.Caption := JC.name;
   Self.JCs.Add(jcid);
  end;

 for vbi in Self.openMJC.data.vb do
  begin
   Blky.GetBlkByID(vbi, Blk);
   if (Blk = nil) then continue;
   LI := Self.LV_VBs.Items.Add;
   LI.Caption := Blk.GetGlobalSettings.name;
   Self.vb.Add(vbi);
  end;

 Self.B_VB_New.Enabled  := (Self.JCs.Count > 0);
 Self.CB_VB_New.Enabled := (Self.JCs.Count > 0);

 Self.UpdateJCCb();
 Self.UpdateVBCb();

 if (Self.new) then
   Self.Caption := 'Nová složená jízdní cesta'
 else
   Self.Caption := 'Upravit složenou jízdní cestu '+Self.openMJC.Nazev;
end;

procedure TF_MJCEdit.EmptyOpenForm();
begin
 if (MultiJCDb.Count > 0) then
   Self.SE_ID.Value := MultiJCDb.Items[MultiJCDb.Count-1].id + 1
 else
   Self.SE_ID.Value := 1;

 Self.B_VB_New.Enabled  := false;
 Self.CB_VB_New.Enabled := false;

 Self.UpdateJCCb();
 Self.UpdateVBCb();

 Self.E_VCNazev.Text := '';
 Self.ActiveControl := CB_JC_Add;
 Self.Caption := 'Nová složená jízdní cesta';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.UpdateJCCb();
var JC: TJC;
begin
 SetLength(CB_JC_ids, JCDb.Count);
 Self.CB_JC_Add.Clear();
 for JC in JCDb do
  begin
   Self.CB_JC_Add.Items.Add(JC.name);
   Self.CB_JC_ids[Self.CB_JC_Add.Items.Count-1] := JC.id;
  end;
end;

procedure TF_MJCEdit.UpdateVBCb();
var obls: TArStr;
begin
 Self.MakeObls(obls);
 Blky.NactiBlokyDoObjektu(Self.CB_VB_New, @CB_VB_indexes, nil, obls, btTrack, -1);
end;

////////////////////////////////////////////////////////////////////////////////

// vytvoreni seznamu oblasti rizeni pro pridani variantnich bodu:
// oblasti rizeni vytvarime podle 0. JC
procedure TF_MJCEdit.MakeObls(var obls: TArStr);
var Blk: TBlk;
    i: Integer;
begin
 if (Self.JCs.Count < 1) then obls := nil
 else begin
   try
     Blky.GetBlkByID(JCDb.GetJCByID(Self.JCs[0]).data.NavestidloBlok, Blk);

     if ((Blk = nil) or (Blk.typ <> btSignal)) then
      begin
       obls := nil;
       Exit();
      end;

     SetLength(obls, (Blk as TBlkSignal).stations.Count);
     for i := 0 to (Blk as TBlkSignal).stations.Count-1 do
      obls[i] := (Blk as TBlkSignal).stations[i].id;
   except
     obls := nil;
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_JC_AddClick(Sender: TObject);
var LI: TListItem;
begin
 if (Self.CB_JC_Add.ItemIndex < 0) then Exit();

 if (Self.LV_JCs.ItemIndex = -1) then
  begin
   Self.JCs.Add(Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex]);
   LI := Self.LV_JCs.Items.Add();
  end else begin
   Self.JCs[Self.LV_JCs.ItemIndex] := Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex];
   LI := Self.LV_JCs.Items[Self.LV_JCs.ItemIndex];
  end;

 LI.Caption := JCDb.GetJCByID(Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex]).name;
 if (Self.JCs.Count = 1) then
  begin
   Self.B_VB_New.Enabled := true;
   Self.CB_VB_New.Enabled := true;
   Self.UpdateVBCb();    // protoze se vytvori oblasti rizeni
  end;

 // doplneni nazvu JC:
 if (Self.CHB_AutoName.Checked) then
  begin
   Self.E_VCNazev.Text := Blky.GetBlkName(JCDb.GetJCByID(Self.JCs[0]).data.NavestidloBlok) + ' > '+
   Blky.GetBlkName(JCDb.GetJCByID(Self.JCs[Self.JCs.Count-1]).data.Useky[JCDb.GetJCByID(Self.JCs[Self.JCs.Count-1]).data.Useky.Count-1]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_VB_NewClick(Sender: TObject);
var LI: TListItem;
begin
 if (Self.CB_VB_New.ItemIndex < 0) then Exit();

 if (Self.LV_VBs.ItemIndex = -1) then
  begin
   Self.vb.Add(Blky.GetBlkID(Self.CB_VB_indexes[Self.CB_VB_New.ItemIndex]));
   LI := Self.LV_VBs.Items.Add();
  end else begin
   Self.vb[Self.LV_VBs.ItemIndex] := Blky.GetBlkID(Self.CB_VB_indexes[Self.CB_VB_New.ItemIndex]);
   LI := Self.LV_VBs.Items[Self.LV_VBs.ItemIndex];
  end;

 LI.Caption := Blky.GetBlkIndexName(Self.CB_VB_indexes[Self.CB_VB_New.ItemIndex]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_SaveClick(Sender: TObject);
var data: TMultiJCProp;
    i, prevIndex: Integer;
    check: TMultiJC;
    origNav: TBlkSignal;
begin
 if (Self.JCs.Count < 2) then
  begin
   Application.MessageBox('Složená JC musí obsahovat alespoň 2 JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 if (Self.E_VCNazev.Text = '') then
  begin
   Application.MessageBox('Vyplňte název složené JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 check := MultiJCDb.GetJCByID(Self.SE_ID.Value);
 if ((check <> nil) and (check <> Self.openMJC)) then
  begin
   Application.MessageBox('Složená jízdní cesta s tímto ID již existuje', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 data.id := Self.SE_ID.Value;
 data.Nazev := Self.E_VCNazev.Text;

 if (Self.new) then
  begin
   data.JCs := nil;          // dulezite pro pridavani JC
   data.vb := nil;

   try
     Self.openMJC := MultiJCDb.Add(data);
   except
     on E: Exception do
      begin
       Application.MessageBox(PChar('Nepodařilo se přidat složenou JC'+#13#10+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
       Exit();
      end;
   end;

   origNav := nil;
  end else begin
   data.JCs := Self.openMJC.data.JCs;          // dulezite pro pridavani JC
   data.vb := Self.openMJC.data.vb;

   origNav := Self.openMJC.StartNav();

   prevIndex := MultiJCDb.GetJCIndexByID(Self.openMJC.id);
   Self.openMJC.data := data;
   MultiJCDb.IDChanged(prevIndex);
  end;

 Self.openMJC.data.JCs.Clear();
 for i := 0 to Self.JCs.Count-1 do
   Self.openMJC.data.JCs.Add(Self.JCs[i]);

 Self.openMJC.data.vb.Clear();
 for i := 0 to Self.vb.Count-1 do
   Self.openMJC.data.vb.Add(Self.vb[i]);

 MultiJCDb.NavChanged(Self.openMJC, origNav);
 Self.openMJC.changed := true;
 MultiJCTableData.UpdateTable();
 Self.Close();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_JC_RemoveClick(Sender: TObject);
begin
 if (Self.LV_JCs.Selected <> nil) then
  begin
   Self.JCs.Delete(Self.LV_JCs.ItemIndex);
   Self.LV_JCs.Items.Delete(Self.LV_JCs.ItemIndex);
   Self.UpdateJCCb();
   Self.UpdateVBCb();

   // doplneni nazvu JC:
   if (Self.CHB_AutoName.Checked) then
    begin
     if (Self.JCs.Count > 0) then
       Self.E_VCNazev.Text := Blky.GetBlkName(JCDb.GetJCByID(Self.JCs[0]).data.NavestidloBlok) + ' > '+
       Blky.GetBlkName(JCDb.GetJCByID(Self.JCs[Self.JCs.Count-1]).data.Useky[JCDb.GetJCByID(Self.JCs[Self.JCs.Count-1]).data.Useky.Count-1])
      else
       Self.E_VCNazev.Text := '';
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_VB_RemoveClick(Sender: TObject);
begin
 if (Self.LV_VBs.Selected <> nil) then
  begin
   Self.vb.Delete(Self.LV_VBs.ItemIndex);
   Self.LV_VBs.Items.Delete(Self.LV_VBs.ItemIndex);
   Self.UpdateVBCb();
  end;
end;


////////////////////////////////////////////////////////////////////////////////

end.//unit
