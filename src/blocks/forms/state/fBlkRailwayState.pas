unit fBlkRailwayState;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, BlockRailway;

type
  TF_BlkRailwayState = class(TForm)
    L_Usek24: TLabel;
    L_Usek20: TLabel;
    CB_Direction: TComboBox;
    B_Apply: TButton;
    B_Refresh: TButton;
    SE_Train: TSpinEdit;
    Label1: TLabel;
    E_Trains: TEdit;
    B_BP_Enable: TButton;
    B_BP_Disable: TButton;
    SE_Train_Add: TSpinEdit;
    B_Train_Delete: TButton;
    Label2: TLabel;
    B_Train_Add: TButton;
    Label3: TLabel;
    L_BP: TLabel;
    CHB_Zaver: TCheckBox;
    CHB_Request: TCheckBox;
    procedure B_RefreshClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_BP_EnableClick(Sender: TObject);
    procedure B_BP_DisableClick(Sender: TObject);
    procedure B_Train_DeleteClick(Sender: TObject);
    procedure B_Train_AddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    railway: TBlkRailway;

    procedure Refresh();
    procedure Apply();

  public
    procedure Open(railway: TBlkRailway);

  end;

var
  F_BlkRailwayState: TF_BlkRailwayState;

implementation

{$R *.dfm}

uses ownConvert, TrainDb, IfThenElse, ownGuiUtils;

procedure TF_BlkRailwayState.B_Train_AddClick(Sender: TObject);
begin
  try
    if (Trains[Self.SE_Train_Add.Value] <> nil) then
      Self.railway.AddTrain(Trains[Self.SE_Train_Add.Value]);
  except
    on E: Exception do
      ExceptionMessageBox(E);
  end;

  Self.Refresh();
end;

procedure TF_BlkRailwayState.B_BP_DisableClick(Sender: TObject);
begin
  Self.railway.BP := false;
  Self.Refresh();
end;

procedure TF_BlkRailwayState.B_BP_EnableClick(Sender: TObject);
begin
  Self.railway.BP := true;
  Self.Refresh();
end;

procedure TF_BlkRailwayState.B_RefreshClick(Sender: TObject);
begin
  Self.Refresh();
end;

procedure TF_BlkRailwayState.B_Train_DeleteClick(Sender: TObject);
begin
  if (Trains[Self.SE_Train_Add.Value] <> nil) then
    Self.railway.RemoveTrain(Trains[Self.SE_Train_Add.Value]);
  Self.Refresh();
end;

procedure TF_BlkRailwayState.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Self.railway := nil;
end;

procedure TF_BlkRailwayState.FormCreate(Sender: TObject);
begin
  Self.railway := nil;
end;

procedure TF_BlkRailwayState.B_ApplyClick(Sender: TObject);
begin
  Self.Apply();
end;

procedure TF_BlkRailwayState.Open(railway: TBlkRailway);
begin
  Self.railway := railway;
  Self.Refresh();
  Self.Caption := 'Stav trati ' + railway.name;
  Self.Show();
end;

procedure TF_BlkRailwayState.Refresh();
var train: TBlkRailwayTrain;
begin
  Self.CHB_Zaver.Checked := railway.zaver;
  Self.CB_Direction.ItemIndex := Integer(railway.direction) + 1;
  Self.CHB_Request.Checked := railway.request;

  if (railway.trainPredict <> nil) then
    Self.SE_Train.Value := railway.trainPredict.traini
  else
    Self.SE_Train.Value := -1;

  Self.E_Trains.Text := '';
  for train in railway.state.Trains do
    Self.E_Trains.Text := Self.E_Trains.Text + IntToStr(train.train.index) + ',';

  Self.L_BP.Caption := ite(railway.BP, 'zavedena', 'nezavedena');
  Self.B_BP_Enable.Enabled := not railway.BP;
  Self.B_BP_Disable.Enabled := railway.BP;
end;

procedure TF_BlkRailwayState.Apply();
begin
  if (Self.railway = nil) then
    Exit();

  Self.railway.Zaver := Self.CHB_Zaver.Checked;
  Self.railway.direction := TRailwayDirection(Self.CB_Direction.ItemIndex - 1);
  Self.railway.request := Self.CHB_Request.Checked;

  if (Self.SE_Train.Value = -1) then
    Self.railway.trainPredict := nil
  else
    Self.railway.trainPredict := TBlkRailwayTrain.Create(Self.SE_Train.Value);
end;

end.// unit
