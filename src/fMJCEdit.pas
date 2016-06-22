unit fMJCEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, TechnologieMultiJC, Generics.Collections,
  RPConst, Spin;

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
    openMJC:TMultiJC;

     JCs:TList<Integer>;
     vb:TList<Integer>;

     CB_VB_indexes:TArSmallI;
     CB_JC_ids:TArSmallI;

    procedure UpdateJCCb();
    procedure UpdateVBCb();

    procedure NormalOpenForm();
    procedure NewOpenForm();

    procedure MakeObls(var obls:TArStr);

  public
    procedure OpenForm(mJC:TMultiJC);
  end;

var
  F_MJCEdit: TF_MJCEdit;

implementation

{$R *.dfm}

uses TJCDatabase, TechnologieJC, TBlok, TBlokUsek, TBloky,
      TBlokSCom, TMultiJCDatabase, DataMultiJC;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Self.openMJC := nil;
 Self.JCs.Clear();
 Self.vb.Clear();
end;

procedure TF_MJCEdit.FormCreate(Sender: TObject);
begin
 Self.openMJC := nil;
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
begin
 Self.B_JC_Remove.Enabled := (Self.LV_JCs.Selected <> nil);
end;

procedure TF_MJCEdit.LV_VBsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_VB_Remove.Enabled := (Self.LV_VBs.Selected <> nil);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.OpenForm(mJC:TMultiJC);
begin
 Self.openMJC := mJC;

 Self.B_JC_Remove.Enabled := false;
 Self.B_VB_Remove.Enabled := false;

 Self.JCs.Clear();
 Self.vb.Clear();

 Self.LV_JCs.Clear();
 Self.LV_VBs.Clear();

 if (mJC = nil) then
  Self.NewOpenForm()
 else
  Self.NormalOpenForm();

 Self.ShowModal();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.NormalOpenForm();
var i:Integer;
    LI:TListItem;
    JC:TJC;
    Blk:TBlk;
begin
 Self.SE_ID.Value    := Self.openMJC.id;
 Self.E_VCNazev.Text := Self.openMJC.Nazev;

 for i := 0 to Self.openMJC.data.JCs.Count-1 do
  begin
   JC := JCDb.GetJCByID(Self.openMJC.data.JCs[i]);
   if (JC = nil) then continue;
   LI := Self.LV_JCs.Items.Add;
   LI.Caption := JC.nazev;
   Self.JCs.Add(Self.openMJC.data.JCs[i]);
  end;

 for i := 0 to Self.openMJC.data.vb.Count-1 do
  begin
   Blky.GetBlkByID(Self.openMJC.data.vb[i], Blk);
   if (Blk = nil) then continue;
   LI := Self.LV_VBs.Items.Add;
   LI.Caption := Blk.GetGlobalSettings.name;
   Self.vb.Add(Self.openMJC.data.vb[i]);
  end;

 Self.B_VB_New.Enabled  := (Self.JCs.Count > 0);
 Self.CB_VB_New.Enabled := (Self.JCs.Count > 0);

 Self.UpdateJCCb();
 Self.UpdateVBCb();

 Self.Caption := 'Editovat složenou jízdní cestu '+Self.openMJC.Nazev;
end;//procedure

procedure TF_MJCEdit.NewOpenForm();
begin
 Self.SE_ID.Value       := MultiJCDb.Items[MultiJCDb.Count-1].id + 1;
 Self.B_VB_New.Enabled  := false;
 Self.CB_VB_New.Enabled := false;

 Self.UpdateJCCb();
 Self.UpdateVBCb();

 Self.E_VCNazev.Text := '';
 Self.ActiveControl := CB_JC_Add;
 Self.Caption := 'Nová složená jízdní cesta';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.UpdateJCCb();
var i, j:Integer;
    inList:Boolean;
    JC:TJC;
begin
 SetLength(CB_JC_ids, JCDb.Count);    // velikost pole trochu prezeneme, ale to nevadi
 Self.CB_JC_Add.Clear();
 for i := 0 to JCDb.Count-1 do
  begin
   JC := JCDb.GetJCByIndex(i);
   inList := false;
   for j := 0 to Self.JCs.Count-1 do
     if (JC.id = Self.JCs[j]) then
      begin
       inList := true;
       break;
      end;

   if (inList) then continue;

   Self.CB_JC_Add.Items.Add(JC.nazev);
   Self.CB_JC_ids[Self.CB_JC_Add.Items.Count-1] := JC.id;
  end;
end;//procedure

procedure TF_MJCEdit.UpdateVBCb();
var obls:TArStr;
    Vypustit:TArSmallI;
    i:Integer;
begin
 SetLength(Vypustit, Self.vb.Count);
 for i := 0 to Self.vb.Count-1 do vypustit[i] := Self.vb[i];
 Self.MakeObls(obls);
 Blky.NactiBlokyDoObjektu(Self.CB_VB_New, @CB_VB_indexes, @Vypustit, obls, _BLK_USEK, -1);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vytvoreni seznamu oblasti rizeni pro pridani variantnich bodu:
// oblasti rizeni vytvarime podle 0. JC
procedure TF_MJCEdit.MakeObls(var obls:TArStr);
var Blk:TBlk;
    i:Integer;
begin
 if (Self.JCs.Count < 1) then obls := nil
 else begin
   try
     Blky.GetBlkByID(JCDb.GetJCByID(Self.JCs[0]).data.NavestidloBlok, Blk);

     if ((Blk = nil) or (Blk.GetGlobalSettings().typ <> _BLK_SCOM)) then
      begin
       obls := nil;
       Exit();
      end;

     SetLength(obls, (Blk as TBlkScom).OblsRizeni.Cnt);
     for i := 0 to (Blk as TBlkScom).OblsRizeni.Cnt-1 do
      obls[i] := (Blk as TBlkScom).OblsRizeni.ORs[i].id;
   except
     obls := nil;
   end;
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_JC_AddClick(Sender: TObject);
var LI:TListItem;
begin
 if (Self.CB_JC_Add.ItemIndex < 0) then Exit();
 Self.JCs.Add(Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex]);
 LI := Self.LV_JCs.Items.Add;
 LI.Caption := JCDb.GetJCByID(Self.CB_JC_ids[Self.CB_JC_Add.ItemIndex]).nazev;
 Self.UpdateJCCb();
 if (Self.JCs.Count = 1) then
  begin
   Self.B_VB_New.Enabled  := true;
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
var LI:TListItem;
begin
 if (Self.CB_VB_New.ItemIndex < 0) then Exit();
 Self.vb.Add(Blky.GetBlkID(Self.CB_VB_indexes[Self.CB_VB_New.ItemIndex]));
 LI := Self.LV_VBs.Items.Add;
 LI.Caption := Blky.GetBlkIndexName(Self.CB_VB_indexes[Self.CB_VB_New.ItemIndex]);
 Self.UpdateVBCb();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_MJCEdit.B_SaveClick(Sender: TObject);
var data:TMultiJCProp;
    i, prevIndex:Integer;
    check:TMultiJC;
begin
 if (Self.JCs.Count < 2) then
  begin
   Application.MessageBox('Složená JC musí obsahovat alespoò 2 JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 if (Self.E_VCNazev.Text = '') then
  begin
   Application.MessageBox('Vyplòte název složené JC', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 check := MultiJCDb.GetJCByID(Self.SE_ID.Value);
 if ((check <> nil) and (check <> Self.openMJC)) then
  begin
   Application.MessageBox('Složená jízdní cesta s tímto ID již existuje', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 data.id    := Self.SE_ID.Value;
 data.Nazev := Self.E_VCNazev.Text;

 if (Self.openMJC = nil) then
  begin
   data.JCs   := nil;          // dulezite pro pridavani JC
   data.vb    := nil;

   try
     Self.openMJC := MultiJCDb.Add(data);
   except
     on E:Exception do
      begin
       Application.MessageBox(PChar('Nepodaøilo se pøidat složenou JC'+#13#10+e.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
       Exit();
      end;
   end;
  end else begin
   data.JCs := Self.openMJC.data.JCs;          // dulezite pro pridavani JC
   data.vb  := Self.openMJC.data.vb;

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
