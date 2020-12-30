unit fBlkTrackState;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, BlockDb, Block, BlockTrack, Spin;

type
  TF_BlkTrackState = class(TForm)
    L_Usek21: TLabel;
    L_Usek24: TLabel;
    L_Usek25: TLabel;
    L_Usek27: TLabel;
    CB_KonecVC: TComboBox;
    CB_NUZ: TComboBox;
    CB_Zaver: TComboBox;
    B_SaveData: TButton;
    B_Obnovit: TButton;
    Label1: TLabel;
    Label3: TLabel;
    M_Stitek: TMemo;
    Label6: TLabel;
    M_Vyluka: TMemo;
    SE_Souprava_Predict: TSpinEdit;
    SE_NavJCRef: TSpinEdit;
    Label2: TLabel;
    Label7: TLabel;
    CB_Zes_Zkrat: TComboBox;
    CB_Zes_Napajeni: TComboBox;
    Label4: TLabel;
    S_DCC: TShape;
    GB_Soupravy: TGroupBox;
    LB_Soupravy: TListBox;
    B_SprDelete: TButton;
    GB_SprAdd: TGroupBox;
    SE_SprAdd_Index: TSpinEdit;
    Label5: TLabel;
    B_SprAdd: TButton;
    procedure B_ObnovitClick(Sender: TObject);
    procedure B_SaveDataClick(Sender: TObject);
    procedure B_SprDeleteClick(Sender: TObject);
    procedure B_SprAddClick(Sender: TObject);
  private
   Blk: TBlkTrack;
    procedure LoadPrmnFromProgram;
    procedure SavePrmnToProgram;
  public
   procedure OpenForm(Blok: TBlkTrack);
  end;

var
  F_BlkTrackState: TF_BlkTrackState;

implementation

uses fMain, ownConvert, TrainDb, Booster;

{$R *.dfm}

procedure TF_BlkTrackState.LoadPrmnFromProgram;
var spr: Integer;
 begin
  CB_Zaver.ItemIndex := Integer(Self.Blk.Zaver);

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  CB_Zes_Zkrat.ItemIndex := Integer(Self.Blk.shortCircuit)+1;
  CB_Zes_Napajeni.ItemIndex := Integer(Self.Blk.power)+1;

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  Self.LB_Soupravy.Clear();
  for spr in Self.Blk.trains do
    Self.LB_Soupravy.Items.Add(IntToStr(spr));

  if (Blk.trainPredict = nil) then
    SE_Souprava_Predict.Value := -1
  else
    SE_Souprava_Predict.Value := Blk.trainPredict.index;
  SE_NavJCRef.Value := Blk.signalJCRef.Count;
  CB_KonecVC.ItemIndex  := Integer(Self.Blk.jcEnd);

  M_Stitek.Text := Blk.note;
  M_Vyluka.Text := Blk.lockout;

  case (Self.Blk.DCC) of
    false: Self.S_DCC.Brush.Color := clRed;
    true : Self.S_DCC.Brush.Color := clLime;
  end;
 end;

procedure TF_BlkTrackState.SavePrmnToProgram;
var Blk: TBlk;
 begin
  Self.Blk.Zaver := TZaver(CB_Zaver.ItemIndex);
  Self.Blk.NUZ := ownConvert.IntToBool(CB_NUZ.ItemIndex);
  Self.Blk.jcEnd := TZaver(CB_KonecVC.ItemIndex);
  if (SE_Souprava_Predict.Value > -1) then
    Self.Blk.trainPredict := Trains[SE_Souprava_Predict.Value]
  else
    Self.Blk.trainPredict := nil;
  Blocks.GetBlkByID(Self.SE_NavJCRef.Value, Blk);
  if (Self.Blk.signalJCRef.Count = 0) then
    Self.Blk.signalJCRef.Clear();
  Self.Blk.shortCircuit := TBoosterSignal(CB_Zes_Zkrat.ItemIndex-1);
  Self.Blk.power := TBoosterSignal(CB_Zes_Napajeni.ItemIndex-1);
  Self.Blk.lockout := M_Vyluka.Text;
  Self.Blk.note := M_Stitek.Text;
 end;

procedure TF_BlkTrackState.OpenForm(Blok: TBlkTrack);
 begin
  Self.Blk := Blok;
  LoadPrmnFromProgram();
  Self.Caption := 'Vlastnosti úseku '+Self.Blk.name;
  Self.Show;
 end;

procedure TF_BlkTrackState.B_ObnovitClick(Sender: TObject);
 begin
  LoadPrmnFromProgram;
 end;

procedure TF_BlkTrackState.B_SaveDataClick(Sender: TObject);
 begin
  SavePrmnToProgram();
 end;

procedure TF_BlkTrackState.B_SprAddClick(Sender: TObject);
begin
 if ((Self.SE_SprAdd_Index.Value < 0) or (Self.SE_SprAdd_Index.Value >= _MAX_TRAIN) or
     (Trains[Self.SE_SprAdd_Index.Value] = nil)) then Exit();
 
 try
   Self.Blk.AddTrainS(Self.SE_SprAdd_Index.Value);
   Self.LB_Soupravy.Items.Add(IntToStr(Self.SE_SprAdd_Index.Value));
 except
   on E: Exception do
     Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

procedure TF_BlkTrackState.B_SprDeleteClick(Sender: TObject);
begin
 if (Self.LB_Soupravy.ItemIndex = -1) then Exit();
 if (Application.MessageBox('Opravdu?', 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then Exit();

 try
   Self.Blk.RemoveTrain(StrToInt(Self.LB_Soupravy.Items[Self.LB_Soupravy.ItemIndex]));
   Self.LB_Soupravy.Items.Delete(Self.LB_Soupravy.ItemIndex);
 except
   on E: Exception do
     Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

end.//unit
