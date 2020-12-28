unit fBlkSH;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Spin, StdCtrls, TBlokSouctovaHlaska, TBloky, ComCtrls,
  Generics.Collections;

type
  TF_BlkSH = class(TForm)
    L_P02: TLabel;
    E_Name: TEdit;
    B_save_P: TButton;
    B_Storno: TButton;
    L_ID: TLabel;
    SE_ID: TSpinEdit;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    GB_Prejezdy: TGroupBox;
    LV_Prejezdy: TListView;
    B_Remove: TButton;
    CB_Prj_Add: TComboBox;
    B_Add: TButton;
    procedure B_save_PClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_AddClick(Sender: TObject);
    procedure LV_PrejezdyChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_RemoveClick(Sender: TObject);
  private
   OpenIndex: Integer;
   Blk: TBlkSH;
   NewBlk: Boolean;
   obls: TArstr;   //oblasti rizeni, ve kterych se blok nachazi

   prejezdy: TList<Integer>;
   CB_PrjAddData: TArI;

    procedure NormalOpenForm();
    procedure MainOpenForm();
    procedure NewOpenForm();

    procedure FillNewPrjCB();

  public

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate();
  end;

var
  F_BlkSH: TF_BlkSH;

implementation

uses GetSystems, TechnologieRCS, TOblsRizeni, TOblRizeni, TBlok, FileSystem,
     DataBloky;

{$R *.dfm}

procedure TF_BlkSH.OpenForm(BlokIndex: Integer);
begin
 OpenIndex := BlokIndex;
 Blky.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
 Self.MainOpenForm();

 if (NewBlk) then
   Self.NewOpenForm()
 else
   Self.NormalOpenForm();

 Self.FillNewPrjCB();
 Self.ShowModal();
end;

procedure TF_BlkSH.B_AddClick(Sender: TObject);
var LI: TListItem;
    blk: TBlk;
begin
 if (Self.CB_Prj_Add.ItemIndex = -1) then
  begin
   Application.MessageBox('Je třeba vybrat přejezd k přidání!',
                          'Nelze přidat přejezd', MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 Blky.GetBlkByIndex(Self.CB_PrjAddData[Self.CB_Prj_Add.ItemIndex], blk);
 if ((blk <> nil) and (blk.typ = btPrejezd)) then
  begin
   Self.prejezdy.Add(blk.id);

   LI := Self.LV_Prejezdy.Items.Add();
   LI.Caption := blk.name;
  end;

 Self.FillNewPrjCB();
end;

procedure TF_BlkSH.B_RemoveClick(Sender: TObject);
begin
 if (Self.LV_Prejezdy.Selected <> nil) then
  begin
   Self.prejezdy.Delete(Self.LV_Prejezdy.ItemIndex);
   Self.LV_Prejezdy.DeleteSelected();
   Self.FillNewPrjCB();
  end;
end;

procedure TF_BlkSH.B_save_PClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkSHSettings;
begin
 if (Self.E_Name.Text = '') then
  begin
   Application.MessageBox('Vyplňte název součtové hlásky!','Nelze uložit data',
                          MB_OK OR MB_ICONWARNING);
   Exit;
  end;
 if (Blky.IsBlok(SE_ID.Value, OpenIndex)) then
  begin
   Application.MessageBox('ID již bylo definováno na jiném bloku!',
                          'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 glob.name := Self.E_Name.Text;
 glob.typ  := btSH;
 glob.id   := Self.SE_ID.Value;

 if (NewBlk) then
  begin
   glob.note := '';
   try
     Blk := Blky.Add(btSH, glob) as TBlkSH;
   except
     on E: Exception do
      begin
       Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
       Exit();
      end;
   end;
  end else begin
   glob.note := Self.Blk.note;
   Self.Blk.SetGlobalSettings(glob);
  end;

 settings.prejezdy := TList<Integer>.Create(Self.prejezdy);
 Self.Blk.SetSettings(settings);

 Self.Close();
 Self.Blk.Change();
end;

procedure TF_BlkSH.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_BlkSH.MainOpenForm();
begin
 SetLength(Self.obls, 0);

 Self.LB_Stanice.Clear();
 Self.prejezdy.Clear();
 Self.LV_Prejezdy.Clear();

 Self.B_Remove.Enabled := false;
end;

procedure TF_BlkSH.NormalOpenForm();
var glob: TBlkSettings;
    settings: TBlkSHSettings;
    i, prjid: Integer;
    LI: TListItem;
    oblr: TOR;
begin
 glob := Self.Blk.GetGlobalSettings();
 settings := Self.Blk.GetSettings();

 for oblr in Self.Blk.OblsRizeni do
   Self.LB_Stanice.Items.Add(oblr.Name);

 SetLength(obls, Self.Blk.OblsRizeni.Count);
 for i := 0 to Self.Blk.OblsRizeni.Count-1 do
   obls[i] := Self.Blk.OblsRizeni[i].id;

 E_Name.Text := glob.name;
 SE_ID.Value := glob.id;

 for prjid in settings.prejezdy do
  begin
   Self.prejezdy.Add(prjid);
   LI := Self.LV_Prejezdy.Items.Add();
   LI.Caption := Blky.GetBlkName(prjid);
  end;

 Self.Caption := 'Součtová hláska '+glob.name;
 Self.ActiveControl := Self.B_save_P;
end;

procedure TF_BlkSH.NewOpenForm();
begin
 E_Name.Text := '';
 SE_ID.Value := Blky.GetBlkID(Blky.count-1)+1;

 Self.Caption := 'Nová součtová hláska';
 Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkSH.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 NewBlk     := false;
 OpenIndex  := -1;
 BlokyTableData.UpdateTable();
end;

procedure TF_BlkSH.FormCreate(Sender: TObject);
begin
 Self.prejezdy := TList<Integer>.Create();
end;

procedure TF_BlkSH.FormDestroy(Sender: TObject);
begin
 Self.prejezdy.Free();
end;

procedure TF_BlkSH.LV_PrejezdyChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_Remove.Enabled := (Self.LV_Prejezdy.Selected <> nil);
end;

procedure TF_BlkSH.NewBlkCreate();
begin
 Self.NewBlk := true;
 OpenForm(Blky.count);
end;

procedure TF_BlkSH.FillNewPrjCB();
var ignore: TArI;
    i: Integer;
begin
 SetLength(ignore, Self.prejezdy.Count);
 for i := 0 to Self.prejezdy.Count-1 do
   ignore[i] := Self.prejezdy[i];

 Blky.NactiBlokyDoObjektu(Self.CB_Prj_Add, @Self.CB_PrjAddData, @ignore, nil, btPrejezd);
end;

end.//unit
