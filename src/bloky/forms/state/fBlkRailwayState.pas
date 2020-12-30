unit fBlkRailwayState;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, BlockRailway;

type
  TF_BlkRailwayState = class(TForm)
    L_Usek21: TLabel;
    L_Usek24: TLabel;
    L_Usek25: TLabel;
    L_Usek20: TLabel;
    CB_Zadost: TComboBox;
    CB_Smer: TComboBox;
    CB_Zaver: TComboBox;
    B_SaveData: TButton;
    B_Obnovit: TButton;
    SE_Souprava: TSpinEdit;
    Label1: TLabel;
    E_Soupravy: TEdit;
    B_BP_Enable: TButton;
    B_BP_Disable: TButton;
    SE_Spr_Add: TSpinEdit;
    B_RmSpr: TButton;
    Label2: TLabel;
    B_AddSpr: TButton;
    procedure B_ObnovitClick(Sender: TObject);
    procedure B_SaveDataClick(Sender: TObject);
    procedure B_BP_EnableClick(Sender: TObject);
    procedure B_BP_DisableClick(Sender: TObject);
    procedure B_RmSprClick(Sender: TObject);
    procedure B_AddSprClick(Sender: TObject);
  private
   railway: TBlkRailway;

    procedure Update(); reintroduce;
    procedure Save();
  public
    procedure OpenForm(Blk: TBlkRailway);
  end;

var
  F_BlkRailwayState: TF_BlkRailwayState;

implementation

{$R *.dfm}

uses ownConvert, TrainDb;

procedure TF_BlkRailwayState.B_AddSprClick(Sender: TObject);
begin
 try
   if (Trains[Self.SE_Spr_Add.Value] <> nil) then
     Self.railway.AddTrain(Trains[Self.SE_Spr_Add.Value]);
 except
   on E: Exception do
     Application.MessageBox(PChar('Výjimka:'+#13#10+E.Message), 'Výjimka', MB_OK OR MB_ICONERROR);
 end;

 Self.Update();
end;

procedure TF_BlkRailwayState.B_BP_DisableClick(Sender: TObject);
begin
 Self.railway.BP := false;
 Self.Update();
end;

procedure TF_BlkRailwayState.B_BP_EnableClick(Sender: TObject);
begin
 Self.railway.BP := true;
 Self.Update();
end;

procedure TF_BlkRailwayState.B_ObnovitClick(Sender: TObject);
begin
 Self.Update();
end;

procedure TF_BlkRailwayState.B_RmSprClick(Sender: TObject);
begin
 if (Trains[Self.SE_Spr_Add.Value] <> nil) then
   Self.railway.RemoveTrain(Trains[Self.SE_Spr_Add.Value]);
 Self.Update();
end;

procedure TF_BlkRailwayState.B_SaveDataClick(Sender: TObject);
begin
 Self.Save();
end;

procedure TF_BlkRailwayState.OpenForm(Blk: TBlkRailway);
begin
 Self.railway := Blk;
 Self.Update();
 Self.Caption := 'Trať '+Blk.name;
 Self.Show();
end;

procedure TF_BlkRailwayState.Update();
var train: TBlkRailwayTrain;
begin
 Self.CB_Zaver.ItemIndex  := ownConvert.BoolToInt(railway.Zaver);
 Self.CB_Smer.ItemIndex   := Integer(railway.direction)+1;
 Self.CB_Zadost.ItemIndex := ownConvert.BoolToInt(railway.request);

 if (railway.trainPredict <> nil) then
   Self.SE_Souprava.Value := railway.trainPredict.traini
 else
   Self.SE_Souprava.Value := -1;

 Self.E_Soupravy.Text := '';
 for train in railway.state.trains do
   Self.E_Soupravy.Text := Self.E_Soupravy.Text + IntToStr(train.train.index)+',';
end;

procedure TF_BlkRailwayState.Save();
begin
 railway.Zaver  := ownConvert.IntToBool(CB_Zaver.ItemIndex);
 railway.direction   := TRailwayDirection(Self.CB_Smer.ItemIndex-1);
 railway.request := ownConvert.IntToBool(CB_Zadost.ItemIndex);

 if (Self.SE_Souprava.Value = -1) then
   railway.trainPredict := nil
 else
   railway.trainPredict := TBlkRailwayTrain.Create(Self.SE_Souprava.Value);
end;

end.//unit
