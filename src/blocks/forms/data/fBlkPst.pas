unit fBlkPst;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, BlockPst,
  Vcl.ComCtrls;

type
  TF_BlkPst = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    B_Storno: TButton;
    B_Apply: TButton;
    LB_Areas: TListBox;
    GB_RCS: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    SE_RCS_Take_Module: TSpinEdit;
    SE_RCS_Indication_Module: TSpinEdit;
    SE_RCS_Indication_Port: TSpinEdit;
    SE_RCS_Take_Port: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SE_RCS_Release_Module: TSpinEdit;
    SE_RCS_Release_Port: TSpinEdit;
    SE_RCS_Horn_Port: TSpinEdit;
    SE_RCS_Horn_Module: TSpinEdit;
    Label9: TLabel;
    GB_Tracks: TGroupBox;
    GB_TrackEdit: TGroupBox;
    CB_Track: TComboBox;
    B_Track_Ok: TButton;
    LV_Tracks: TListView;
    GB_Turnouts: TGroupBox;
    LV_Turnouts: TListView;
    GB_TurnoutEdit: TGroupBox;
    CB_Turnout: TComboBox;
    B_Turnout_Ok: TButton;
    GB_Refugees: TGroupBox;
    LV_Refugees: TListView;
    GB_RefEdit: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CB_Ref_Block: TComboBox;
    CB_Ref_Pos: TComboBox;
    B_Ref_Ok: TButton;
    GB_Signals: TGroupBox;
    GB_Signal_Edit: TGroupBox;
    CB_Signal: TComboBox;
    B_Signal_Ok: TButton;
    LV_Signals: TListView;
    B_Track_Del: TButton;
    B_Ref_Del: TButton;
    B_Turnout_Del: TButton;
    B_Signal_Delete: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private
    newBlk: Boolean;
    blk: TBlkPst;

  public
    openIndex: Integer;

    procedure OpenForm(blockIndex: Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();
    procedure NewBlkCreate();
  end;


var
  F_BlkPst: TF_BlkPst;

implementation

uses BlockDb, Block, Area, DataBloky;

{$R *.dfm}

procedure TF_BlkPst.OpenForm(blockIndex: Integer);
begin
  Self.openIndex := blockIndex;
  Blocks.GetBlkByIndex(blockIndex, TBlk(Self.blk));
  Self.CommonOpenForm();

  if (Self.newBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkPst.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.Caption := 'Nový blok Pomocné stavědlo';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkPst.NormalOpenForm();
var glob: TBlkSettings;
begin
  glob := Self.blk.GetGlobalSettings();

  for var area in Self.blk.areas do
    Self.LB_Areas.Items.Add(Area.name);

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.Caption := 'Upravit blok ' + glob.name + ' (pomocné stavědlo)';
  Self.ActiveControl := Self.B_Apply;
end;

procedure TF_BlkPst.CommonOpenForm();
begin
  Self.LB_Areas.Clear();
end;

procedure TF_BlkPst.NewBlkCreate();
begin
  Self.newBlk := true;
  Self.OpenForm(Blocks.count);
end;

procedure TF_BlkPst.B_ApplyClick(Sender: TObject);
var glob: TBlkSettings;
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btPst;

  if (NewBlk) then
  begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkPst;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.blk.note;
    Self.blk.SetGlobalSettings(glob);
  end;

  Self.Close();
  Self.blk.Change();
end;

procedure TF_BlkPst.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkPst.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.newBlk := false;
  Self.openIndex := -1;
  BlokyTableData.UpdateTable();
end;

end.
