unit fBlkUsekSysVars;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TBloky, TBlok, TBlokUsek, Spin;

type
  TF_BlkUsek_tech = class(TForm)
    L_Usek21: TLabel;
    L_Usek24: TLabel;
    L_Usek25: TLabel;
    L_Usek20: TLabel;
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
    SE_Souprava: TSpinEdit;
    SE_Souprava_Predict: TSpinEdit;
    SE_SComJCRef: TSpinEdit;
    Label2: TLabel;
    Label7: TLabel;
    CB_Zes_Zkrat: TComboBox;
    CB_Zes_Napajeni: TComboBox;
    Label4: TLabel;
    S_DCC: TShape;
    procedure B_ObnovitClick(Sender: TObject);
    procedure B_SaveDataClick(Sender: TObject);
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

uses fSettings, fMain, Prevody, fBlkUsek, TechnologieJC, SprDb, Booster;

{$R *.dfm}

procedure TF_BlkUsek_tech.LoadPrmnFromProgram;
 begin
  CB_Zaver.ItemIndex := Integer(Self.Blk.Zaver);

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  CB_Zes_Zkrat.ItemIndex    := Integer(Self.Blk.ZesZkrat)+1;
  CB_Zes_Napajeni.ItemIndex := Integer(Self.Blk.ZesNapajeni)+1;

  case (Self.Blk.NUZ) of
   false : CB_NUZ.ItemIndex := 0;
   true  : CB_NUZ.ItemIndex := 1;
  end;

  SE_Souprava.Value           := Blk.Souprava;
  SE_Souprava_Predict.Value   := Blk.SprPredict;
  if (Blk.SComJCRef <> nil) then
    SE_SComJCRef.Value        := (Blk.SComJCRef as TBlk).GetGlobalSettings.id
  else
    SE_SComJCRef.Value        := -1;
  CB_KonecVC.ItemIndex        := Integer(Self.Blk.KonecJC);

  M_Stitek.Text               := Blk.Stitek;
  M_Vyluka.Text               := Blk.Vyluka;

  case (Self.Blk.DCC) of
    false: Self.S_DCC.Brush.Color := clRed;
    true : Self.S_DCC.Brush.Color := clLime;
  end;
 end;//procedure

procedure TF_BlkUsek_tech.SavePrmnToProgram;
var Blk:TBlk;
 begin
  Self.Blk.Zaver       := TZaver(CB_Zaver.ItemIndex);
  Self.Blk.NUZ         := PrevodySoustav.IntToBool(CB_NUZ.ItemIndex);
  Self.Blk.KonecJC     := TZaver(CB_KonecVC.ItemIndex);
  Self.Blk.Souprava    := SE_Souprava.Value;
  Self.Blk.SprPredict  := SE_Souprava_Predict.Value;
  Blky.GetBlkByID(Self.SE_SComJCRef.Value, Blk);
  Self.Blk.SComJCRef   := Blk;
  Self.Blk.ZesZkrat    := TBoosterSignal(CB_Zes_Zkrat.ItemIndex-1);
  Self.Blk.ZesNapajeni := TBoosterSignal(CB_Zes_Napajeni.ItemIndex-1);
  Self.Blk.Vyluka      := M_Vyluka.Text;
  Self.Blk.Stitek      := M_Stitek.Text;
 end;//procedure

procedure TF_BlkUsek_tech.OpenForm(Blok:TBlkUsek);
 begin
  Self.Blk := Blok;
  LoadPrmnFromProgram();
  Self.Caption := 'Vlastnosti úseku '+Self.Blk.GetGlobalSettings.name;
  Self.Show;
 end;//procedure

procedure TF_BlkUsek_tech.B_ObnovitClick(Sender: TObject);
 begin
  LoadPrmnFromProgram;
 end;//procedure

procedure TF_BlkUsek_tech.B_SaveDataClick(Sender: TObject);
 begin
  SavePrmnToProgram();
 end;

end.//unit
