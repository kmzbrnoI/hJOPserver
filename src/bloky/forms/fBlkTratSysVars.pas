unit fBlkTratSysVars;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, TBlokTrat;

type
  TF_BlkTrat_tech = class(TForm)
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
   trat:TBlkTrat;

    procedure Update(); reintroduce;
    procedure Save();
  public
    procedure OpenForm(Blk:TBlkTrat);
  end;

var
  F_BlkTrat_tech: TF_BlkTrat_tech;

implementation

{$R *.dfm}

uses ownConvert, TrainDb;

procedure TF_BlkTrat_tech.B_AddSprClick(Sender: TObject);
begin
 try
   if (Trains[Self.SE_Spr_Add.Value] <> nil) then
     Self.trat.AddTrain(Trains[Self.SE_Spr_Add.Value]);
 except
   on E:Exception do
     Application.MessageBox(PChar('Výjimka:'+#13#10+E.Message), 'Výjimka', MB_OK OR MB_ICONERROR);
 end;

 Self.Update();
end;

procedure TF_BlkTrat_tech.B_BP_DisableClick(Sender: TObject);
begin
 Self.trat.BP := false;
 Self.Update();
end;

procedure TF_BlkTrat_tech.B_BP_EnableClick(Sender: TObject);
begin
 Self.trat.BP := true;
 Self.Update();
end;

procedure TF_BlkTrat_tech.B_ObnovitClick(Sender: TObject);
begin
 Self.Update();
end;

procedure TF_BlkTrat_tech.B_RmSprClick(Sender: TObject);
begin
 if (Trains[Self.SE_Spr_Add.Value] <> nil) then
   Self.trat.RemoveTrain(Trains[Self.SE_Spr_Add.Value]);
 Self.Update();
end;

procedure TF_BlkTrat_tech.B_SaveDataClick(Sender: TObject);
begin
 Self.Save();
end;

procedure TF_BlkTrat_tech.OpenForm(Blk:TBlkTrat);
begin
 Self.trat := Blk;
 Self.Update();
 Self.Caption := 'Trať '+Blk.name;
 Self.Show();
end;

procedure TF_BlkTrat_tech.Update();
var train:TBlkTratTrain;
begin
 Self.CB_Zaver.ItemIndex  := ownConvert.BoolToInt(trat.Zaver);
 Self.CB_Smer.ItemIndex   := Integer(trat.Smer)+1;
 Self.CB_Zadost.ItemIndex := ownConvert.BoolToInt(trat.Zadost);

 if (trat.trainPredict <> nil) then
   Self.SE_Souprava.Value := trat.trainPredict.traini
 else
   Self.SE_Souprava.Value := -1;

 Self.E_Soupravy.Text := '';
 for train in trat.stav.trains do
   Self.E_Soupravy.Text := Self.E_Soupravy.Text + IntToStr(train.train.index)+',';
end;

procedure TF_BlkTrat_tech.Save();
begin
 trat.Zaver  := ownConvert.IntToBool(CB_Zaver.ItemIndex);
 trat.Smer   := TTratSmer(Self.CB_Smer.ItemIndex-1);
 trat.Zadost := ownConvert.IntToBool(CB_Zadost.ItemIndex);

 if (Self.SE_Souprava.Value = -1) then
   trat.trainPredict := nil
 else
   trat.trainPredict := TBlkTratTrain.Create(Self.SE_Souprava.Value);
end;

end.//unit
