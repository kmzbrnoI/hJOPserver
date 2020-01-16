unit fBlkUsekSysVars;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TBloky, TBlok, TBlokUsek, Spin;

type
  TF_BlkUsek_tech = class(TForm)
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
   Blk:TBlkUsek;
    procedure LoadPrmnFromProgram;
    procedure SavePrmnToProgram;
  public
   procedure OpenForm(Blok:TBlkUsek);
  end;

var
  F_BlkUsek_tech: TF_BlkUsek_tech;

implementation

uses fMain, Prevody, SprDb, Booster;

{$R *.dfm}

procedure TF_BlkUsek_tech.LoadPrmnFromProgram;
var spr:Integer;
 begin
  CB_Zaver.ItemIndex := Integer(Self.Blk.Zaver);

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  CB_Zes_Zkrat.ItemIndex := Integer(Self.Blk.zkrat)+1;
  CB_Zes_Napajeni.ItemIndex := Integer(Self.Blk.napajeni)+1;

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  Self.LB_Soupravy.Clear();
  for spr in Self.Blk.Soupravs do
    Self.LB_Soupravy.Items.Add(IntToStr(spr));

  SE_Souprava_Predict.Value := Blk.SprPredict;
  SE_NavJCRef.Value := Blk.NavJCRef.Count;
  CB_KonecVC.ItemIndex  := Integer(Self.Blk.KonecJC);

  M_Stitek.Text := Blk.Stitek;
  M_Vyluka.Text := Blk.Vyluka;

  case (Self.Blk.DCC) of
    false: Self.S_DCC.Brush.Color := clRed;
    true : Self.S_DCC.Brush.Color := clLime;
  end;
 end;

procedure TF_BlkUsek_tech.SavePrmnToProgram;
var Blk:TBlk;
 begin
  Self.Blk.Zaver := TZaver(CB_Zaver.ItemIndex);
  Self.Blk.NUZ := PrevodySoustav.IntToBool(CB_NUZ.ItemIndex);
  Self.Blk.KonecJC := TZaver(CB_KonecVC.ItemIndex);
  Self.Blk.SprPredict := SE_Souprava_Predict.Value;
  Blky.GetBlkByID(Self.SE_NavJCRef.Value, Blk);
  if (Self.Blk.NavJCRef.Count = 0) then
    Self.Blk.NavJCRef.Clear();
  Self.Blk.zkrat := TBoosterSignal(CB_Zes_Zkrat.ItemIndex-1);
  Self.Blk.napajeni := TBoosterSignal(CB_Zes_Napajeni.ItemIndex-1);
  Self.Blk.Vyluka := M_Vyluka.Text;
  Self.Blk.Stitek := M_Stitek.Text;
 end;

procedure TF_BlkUsek_tech.OpenForm(Blok:TBlkUsek);
 begin
  Self.Blk := Blok;
  LoadPrmnFromProgram();
  Self.Caption := 'Vlastnosti úseku '+Self.Blk.name;
  Self.Show;
 end;

procedure TF_BlkUsek_tech.B_ObnovitClick(Sender: TObject);
 begin
  LoadPrmnFromProgram;
 end;

procedure TF_BlkUsek_tech.B_SaveDataClick(Sender: TObject);
 begin
  SavePrmnToProgram();
 end;

procedure TF_BlkUsek_tech.B_SprAddClick(Sender: TObject);
begin
 if ((Self.SE_SprAdd_Index.Value < 0) or (Self.SE_SprAdd_Index.Value >= _MAX_SPR) or
     (Soupravy[Self.SE_SprAdd_Index.Value] = nil)) then Exit();
 
 try
   Self.Blk.AddSoupravaS(Self.SE_SprAdd_Index.Value);
   Self.LB_Soupravy.Items.Add(IntToStr(Self.SE_SprAdd_Index.Value));
 except
   on E:Exception do
     Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

procedure TF_BlkUsek_tech.B_SprDeleteClick(Sender: TObject);
begin
 if (Self.LB_Soupravy.ItemIndex = -1) then Exit();
 if (Application.MessageBox('Opravdu?', 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then Exit();

 try
   Self.Blk.RemoveSouprava(StrToInt(Self.LB_Soupravy.Items[Self.LB_Soupravy.ItemIndex]));
   Self.LB_Soupravy.Items.Delete(Self.LB_Soupravy.ItemIndex);
 except
   on E:Exception do
     Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;

end.//unit
