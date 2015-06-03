unit BlkVyhybkaSysVars;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TBlokVyhybka, RPConst, Spin;

type
  TF_BlkVyh_tech = class(TForm)
    B_Update: TButton;
    B_Apply: TButton;
    M_Vyluka: TMemo;
    Label6: TLabel;
    M_Stitek: TMemo;
    Label1: TLabel;
    L_Usek21: TLabel;
    L_Usek25: TLabel;
    L_Usek20: TLabel;
    Label2: TLabel;
    Label7: TLabel;
    CB_Locked: TComboBox;
    SE_Redukce: TSpinEdit;
    CB_Stav_Plus: TComboBox;
    CB_Stav_Minus: TComboBox;
    SE_Zaver: TSpinEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_UpdateClick(Sender: TObject);
  private
    OpenBlk:TBlkVyhybka;

     procedure myUpdate();
     procedure myApply();

  private const

  public
     procedure OpenForm(blk:TBlkVyhybka);
  end;

var
  F_BlkVyh_tech: TF_BlkVyh_tech;

implementation

uses TBlokUsek, Prevody;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkVyh_tech.B_ApplyClick(Sender: TObject);
begin
 Self.myApply();
end;//procedure

procedure TF_BlkVyh_tech.B_UpdateClick(Sender: TObject);
begin
 Self.myUpdate();
end;//procedure

procedure TF_BlkVyh_tech.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Self.OpenBlk := nil;
end;//procedure

procedure TF_BlkVyh_tech.FormCreate(Sender: TObject);
begin
 Self.OpenBlk := nil;
end;//procedure

procedure TF_BlkVyh_tech.OpenForm(blk:TBlkVyhybka);
begin
 Self.OpenBlk := blk;
 Self.myUpdate();

 Self.Caption := 'Technologické vlastnosti bloku '+blk.GetGlobalSettings().name;
 Self.Show();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkVyh_tech.myUpdate();
begin
 Self.M_Stitek.Text := Self.OpenBlk.Stitek;
 Self.M_Vyluka.Text := Self.OpenBlk.Vyluka;

 Self.SE_Zaver.Value := Self.OpenBlk.Stav.vyhZaver;
 Self.CB_Stav_Plus.ItemIndex  := PrevodySoustav.BoolToInt(Self.OpenBlk.StaveniPlus);
 Self.CB_Stav_Minus.ItemIndex := PrevodySoustav.BoolToInt(Self.OpenBlk.StaveniMinus);

 Self.CB_Locked.ItemIndex     := PrevodySoustav.BoolToInt(Self.OpenBlk.Stav.locked);
 Self.SE_Redukce.Value        := Self.OpenBlk.Stav.redukce_menu;
end;//procedure

procedure TF_BlkVyh_tech.myApply();
begin
 Self.OpenBlk.Stitek := Self.M_Stitek.Text;
 Self.OpenBlk.Vyluka := Self.M_Vyluka.Text;

 if (Self.SE_Zaver.Value = 0) then
   Self.OpenBlk.NullVyhZaver();

 Self.OpenBlk.StaveniPlus  := PrevodySoustav.IntToBool(CB_Stav_Plus.ItemIndex);
 Self.OpenBlk.StaveniMinus := PrevodySoustav.IntToBool(CB_Stav_Minus.ItemIndex);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
