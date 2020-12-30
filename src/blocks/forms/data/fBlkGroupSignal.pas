unit fBlkGroupSignal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, BlockGroupSignal, ComCtrls, BlockDb;

type
  TF_BlkGroupSignal = class(TForm)
    L_IR01: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_Usek03: TLabel;
    LB_Stations: TListBox;
    B_Storno: TButton;
    B_Apply: TButton;
    GB_Signals: TGroupBox;
    LV_Signals: TListView;
    B_BlkDelete: TButton;
    GB_NewSignal: TGroupBox;
    B_BlkAdd: TButton;
    CB_NewSignal: TComboBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_SignalsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure B_BlkAddClick(Sender: TObject);
    procedure B_BlkDeleteClick(Sender: TObject);

  private
   new: Boolean;
   blk: TBlkGroupSignal;
   CB_NewSignalData: TArI;

    procedure NewBlkOpenForm();
    procedure EditBlkOpenForm();
    procedure CommonOpenForm();

    procedure FillCBNewSignal();

  public
   blkIndex: Integer;

    procedure EditBlk(blockIndex: Integer);
    procedure NewBlk();

  end;

var
  F_BlkGroupSignal: TF_BlkGroupSignal;

implementation

uses Block, TOblRizeni, DataBloky;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkGroupSignal.EditBlk(blockIndex: Integer);
begin
 Self.blkIndex := blockIndex;
 Blocks.GetBlkByIndex(blockIndex, TBlk(Self.Blk));
 Self.CommonOpenForm();

 if (Self.new) then
   Self.NewBlkOpenForm()
 else
   Self.EditBlkOpenForm();

 Self.ShowModal();
end;

procedure TF_BlkGroupSignal.NewBlkOpenForm();
begin
 Self.E_name.Text := '';
 Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;

 Self.Caption := 'Nový blok Skupinové návìstidlo';
 Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkGroupSignal.EditBlkOpenForm();
var glob: TBlkSettings;
    settings: TBlkGSSettings;
    oblr: TOR;
    LI: TListItem;
    id: Integer;
begin
 glob := Self.Blk.GetGlobalSettings();
 settings := Self.Blk.GetSettings();

 for oblr in Self.Blk.stations do
   Self.LB_Stations.Items.Add(oblr.Name);

 Self.E_Name.Text := glob.name;
 Self.SE_ID.Value := glob.id;

 for id in settings.signalIds do
  begin
   LI := Self.LV_Signals.Items.Add;
   LI.Caption := IntToStr(id);
   LI.SubItems.Add(Blocks.GetBlkName(id));
  end;

 Self.Caption := 'Upravit blok '+glob.name+' (skupinové návìstidlo)';
 Self.ActiveControl := Self.B_Apply;
end;

procedure TF_BlkGroupSignal.CommonOpenForm();
begin
 Self.LB_Stations.Clear();
end;

procedure TF_BlkGroupSignal.NewBlk();
begin
 Self.new := true;
 Self.EditBlk(Blocks.count);
end;

procedure TF_BlkGroupSignal.B_BlkAddClick(Sender: TObject);
var LI: TListItem;
begin
 if (Self.CB_NewSignal.ItemIndex < 0) then
  begin
   Application.MessageBox('Vyberte blok!', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 LI := Self.LV_Signals.Items.Add();
 LI.Caption := IntToStr(Blocks.GetBlkID(Self.CB_NewSignalData[Self.CB_NewSignal.ItemIndex]));
 LI.SubItems.Add(Blocks.GetBlkName(Blocks.GetBlkID(CB_NewSignalData[Self.CB_NewSignal.ItemIndex])));

 Self.FillCBNewSignal();
end;

procedure TF_BlkGroupSignal.B_BlkDeleteClick(Sender: TObject);
begin
 Self.LV_Signals.DeleteSelected();
 Self.FillCBNewSignal();
end;

procedure TF_BlkGroupSignal.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_BlkGroupSignal.B_ApplyClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkGSSettings;
    LI: TListItem;
begin
 if (Self.E_Name.Text = '') then
  begin
   Application.MessageBox('Vyplòte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 if (Blocks.IsBlok(SE_ID.Value, Self.blkIndex)) then
  begin
   Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 glob.name := Self.E_Name.Text;
 glob.id := Self.SE_ID.Value;
 glob.typ := btGroupSignal;

 if (Self.new) then
  begin
   glob.note := '';
   try
     Blk := Blocks.Add(glob) as TBlkGroupSignal;
   except
     on E: Exception do
      begin
       Application.MessageBox(PChar('Nepodaøilo se pøidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
       Exit();
      end;
   end;
  end else begin
   glob.note := Self.Blk.note;
   Self.Blk.SetGlobalSettings(glob);
  end;

 settings.signalIds.Clear();
 for LI in Self.LV_Signals.Items do
   settings.signalIds.Add(StrToInt(LI.Caption));

 Self.Blk.SetSettings(settings);
 Self.Close();
end;

procedure TF_BlkGroupSignal.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Self.new := false;
 Self.blkIndex := -1;
 BlokyTableData.UpdateTable();
end;

procedure TF_BlkGroupSignal.LV_SignalsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 Self.B_BlkDelete.Enabled := (Self.LV_Signals.ItemIndex > -1);
end;

procedure TF_BlkGroupSignal.FillCBNewSignal();
var signalIgnore: TArI;
    obls: TArStr;
    i: Integer;
begin
 SetLength(signalIgnore, Self.LV_Signals.Items.Count);
 for i := 0 to Self.LV_Signals.Items.Count-1 do
   signalIgnore[i] := StrToInt(Self.LV_Signals.Items.Item[i].Caption);

 SetLength(obls, Self.blk.stations.Count);
 for i := 0 to Self.blk.stations.Count-1 do
   obls[i] := Self.blk.stations[i].id;

 Blocks.NactiBlokyDoObjektu(Self.CB_NewSignal, @Self.CB_NewSignalData, @signalIgnore, obls, btSignal, -1);
end;

////////////////////////////////////////////////////////////////////////////////

end.
