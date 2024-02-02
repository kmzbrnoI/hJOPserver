unit fBlkTrackState;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BlockDb, Block, BlockTrack, Spin;

type
  TF_BlkTrackState = class(TForm)
    L_Usek21: TLabel;
    L_Usek25: TLabel;
    L_Usek27: TLabel;
    CB_KonecVC: TComboBox;
    CB_Zaver: TComboBox;
    B_Apply: TButton;
    B_Refresh: TButton;
    Label1: TLabel;
    Label3: TLabel;
    M_Note: TMemo;
    Label6: TLabel;
    M_Lockout: TMemo;
    SE_Souprava_Predict: TSpinEdit;
    SE_NavJCRef: TSpinEdit;
    Label2: TLabel;
    Label7: TLabel;
    CB_Booster_Short: TComboBox;
    CB_Booster_Power: TComboBox;
    GB_Trains: TGroupBox;
    LB_Trains: TListBox;
    B_Train_Delete: TButton;
    GB_Train_Add: TGroupBox;
    SE_Train_Add_Index: TSpinEdit;
    Label5: TLabel;
    B_Train_Add: TButton;
    CHB_NUZ: TCheckBox;
    CHB_DCC: TCheckBox;
    CHB_SlowingReady: TCheckBox;
    CHB_PathCancelZaver: TCheckBox;
    procedure B_RefreshClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_Train_DeleteClick(Sender: TObject);
    procedure B_Train_AddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    track: TBlkTrack;
    procedure Refresh();
    procedure Apply();

  public
    procedure Open(track: TBlkTrack);

  end;

var
  F_BlkTrackState: TF_BlkTrackState;

implementation

uses fMain, ownConvert, TrainDb, Booster, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkTrackState.Refresh();
begin
  Self.CB_Zaver.ItemIndex := Integer(Self.track.Zaver);

  Self.CHB_NUZ.Checked := Self.track.NUZ;

  Self.CB_Booster_Short.ItemIndex := Integer(Self.track.shortCircuit) + 1;
  Self.CB_Booster_Power.ItemIndex := Integer(Self.track.power) + 1;

  Self.LB_Trains.Clear();
  for var train in Self.track.trains do
    Self.LB_Trains.Items.Add(IntToStr(train));

  if (track.trainPredict = nil) then
    Self.SE_Souprava_Predict.Value := -1
  else
    Self.SE_Souprava_Predict.Value := Self.track.trainPredict.index;
  Self.SE_NavJCRef.Value := Self.track.signalJCRef.Count;
  Self.CB_KonecVC.ItemIndex := Integer(Self.track.jcEnd);

  Self.M_Note.Text := Self.track.note;
  Self.M_Lockout.Text := Self.track.lockout;

  Self.CHB_DCC.Checked := Self.track.DCC;
  Self.CHB_SlowingReady.Checked := Self.track.slowingReady;
  Self.CHB_PathCancelZaver.Checked := Self.track.state.pathCancelZaverTimer.running;
end;

procedure TF_BlkTrackState.Apply();
begin
  if (Self.track = nil) then
    Exit();

  Self.track.Zaver := TZaver(Self.CB_Zaver.ItemIndex);
  Self.track.NUZ := Self.CHB_NUZ.Checked;
  Self.track.jcEnd := TZaver(Self.CB_KonecVC.ItemIndex);
  if (SE_Souprava_Predict.Value > -1) then
    Self.track.trainPredict := trains[Self.SE_Souprava_Predict.Value]
  else
    Self.track.trainPredict := nil;
  if (Self.track.signalJCRef.Count = 0) then
    Self.track.signalJCRef.Clear();
  Self.track.shortCircuit := TBoosterSignal(Self.CB_Booster_Short.ItemIndex - 1);
  Self.track.power := TBoosterSignal(Self.CB_Booster_Power.ItemIndex - 1);
  Self.track.lockout := Self.M_Lockout.Text;
  Self.track.note := Self.M_Note.Text;
end;

procedure TF_BlkTrackState.Open(track: TBlkTrack);
begin
  Self.track := track;
  Self.Refresh();
  Self.Caption := 'Stav úseku ' + track.name;
  Self.Show();
end;

procedure TF_BlkTrackState.B_RefreshClick(Sender: TObject);
begin
  Self.Refresh();
end;

procedure TF_BlkTrackState.B_ApplyClick(Sender: TObject);
begin
  Self.Apply();
end;

procedure TF_BlkTrackState.B_Train_AddClick(Sender: TObject);
begin
  if ((Self.track = nil) or (Self.SE_Train_Add_Index.Value < 0) or (Self.SE_Train_Add_Index.Value >= _MAX_TRAIN) or
    (trains[Self.SE_Train_Add_Index.Value] = nil)) then
    Exit();

  try
    Self.track.AddTrainS(Self.SE_Train_Add_Index.Value);
    Self.LB_Trains.Items.Add(IntToStr(Self.SE_Train_Add_Index.Value));
  except
    on E: Exception do
      ExceptionMessageBox(E);
  end;
end;

procedure TF_BlkTrackState.B_Train_DeleteClick(Sender: TObject);
begin
  if ((Self.LB_Trains.ItemIndex = -1) or (Self.track = nil)) then
    Exit();
  if (StrMessageBox('Opravdu?', 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
    Exit();

  try
    Self.track.RemoveTrain(StrToInt(Self.LB_Trains.Items[Self.LB_Trains.ItemIndex]));
    Self.LB_Trains.Items.Delete(Self.LB_Trains.ItemIndex);
  except
    on E: Exception do
      ExceptionMessageBox(E);
  end;
end;

procedure TF_BlkTrackState.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.track := nil;
end;

procedure TF_BlkTrackState.FormCreate(Sender: TObject);
begin
  Self.track := nil;
end;

end.// unit
