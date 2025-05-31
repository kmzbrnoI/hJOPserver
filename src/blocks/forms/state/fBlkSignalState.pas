unit fBlkSignalState;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Samples.Spin, BlockSignal;

type
  TF_BlkSignalState = class(TForm)
    Label1: TLabel;
    CB_Aspect: TComboBox;
    Label2: TLabel;
    CB_Selected: TComboBox;
    Label3: TLabel;
    L_ABJC: TLabel;
    CHB_ZAM: TCheckBox;
    Label4: TLabel;
    L_DNJC: TLabel;
    Label6: TLabel;
    L_PrivolJC: TLabel;
    CHB_Falling: TCheckBox;
    CHB_PN: TCheckBox;
    L_PN_Start_Time: TLabel;
    CHB_Autoblok: TCheckBox;
    GB_RNZ: TGroupBox;
    LV_RNZ: TListView;
    B_Apply: TButton;
    B_Refresh: TButton;
    CHB_Hradlo: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_RefreshClick(Sender: TObject);
  private
    signal: TBlkSignal;

    procedure Refresh();
    procedure Apply();

  public
    procedure Open(signal: TBlkSignal);

  end;

var
  F_BlkSignalState: TF_BlkSignalState;

implementation

{$R *.dfm}

uses BlockDb;

procedure TF_BlkSignalState.B_ApplyClick(Sender: TObject);
begin
  Self.Apply();
  Self.Refresh();
end;

procedure TF_BlkSignalState.B_RefreshClick(Sender: TObject);
begin
  Self.Refresh();
end;

procedure TF_BlkSignalState.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Self.signal := nil;
end;

procedure TF_BlkSignalState.FormCreate(Sender: TObject);
begin
  Self.signal := nil;
end;

procedure TF_BlkSignalState.Open(signal: TBlkSignal);
begin
  Self.signal := signal;
  Self.Refresh();
  Self.Caption := 'Stav návìstidla ' + signal.name;
  Self.Show();
end;

procedure TF_BlkSignalState.Refresh();
begin
  if (Self.signal = nil) then
    raise Exception.Create('F_BlkSignalState: návìstidlo není');

  var state: TBlkSignalState := signal.state;

  Self.CB_Aspect.ItemIndex := Integer(state.signal) + 2;
  Self.CB_Selected.ItemIndex := Integer(state.selected);
  Self.CHB_ZAM.Checked := state.ZAM;

  if (state.ABJC <> nil) then
    Self.L_ABJC.Caption := state.ABJC.name
  else
    Self.L_ABJC.Caption := '-';

  if (state.dnJC <> nil) then
    Self.L_DNJC.Caption := state.dnJC.name
  else
    Self.L_DNJC.Caption := '-';

  if (state.privolJC <> nil) then
    Self.L_PrivolJC.Caption := state.privolJC.name
  else
    Self.L_PrivolJC.Caption := '-';

  Self.CHB_PN.Checked := (state.privolJC <> nil);
  if (state.privolJC <> nil) then
    Self.L_PN_Start_Time.Caption := FormatDateTime('hh:nn:ss', state.privolStart)
  else
    Self.L_PN_Start_Time.Caption := '-';

  Self.CHB_Autoblok.Checked := (Self.signal.inRailway = rwAutoblok);
  Self.CHB_Hradlo.Checked := (Self.signal.inRailway = rwHradlo);
  Self.CHB_Falling.Checked := state.falling;

  Self.LV_RNZ.Clear();
  for var blkId: Integer in state.toRnz.Keys do
  begin
    var LI: TListItem := Self.LV_RNZ.Items.Add();
    LI.Caption := IntToStr(Self.LV_RNZ.Items.Count);
    LI.SubItems.Add(IntToStr(blkId));
    LI.SubItems.Add(Blocks.GetBlkName(blkId));
    LI.SubItems.Add(IntToStr(state.toRnz[blkId]));
  end;
end;

procedure TF_BlkSignalState.Apply();
begin
  if (Self.signal = nil) then
    raise Exception.Create('F_BlkSignalState: návìstidlo není');

  Self.signal.selected := TBlkSignalSelection(Self.CB_Selected.ItemIndex);
  Self.signal.signal := TBlkSignalCode(Self.CB_Aspect.ItemIndex-2);
  Self.signal.ZAM := Self.CHB_ZAM.Checked;
end;

end.
