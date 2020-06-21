unit fBlkNew;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Menus, ComCtrls, ExtCtrls, Buttons;

type
  TF_BlkNew = class(TForm)
    RG_NewTyp: TRadioGroup;
    B_Save: TButton;
    procedure B_SaveClick(Sender: TObject);
  private
    { Private declarations }
  public
   procedure OpenForm;
  end;

var
  F_BlkNew: TF_BlkNew;

implementation

uses fBlkVyhybka, fBlkUsek, fBlkIR, fBlkNav, fMain, fBlkSH, fBlkAC,
      TBloky, fBlkPrejezd, fBlkTrat, fBlkZamek, fBlkRozp, fBlkTU, fBlkIO;

{$R *.dfm}

procedure TF_BlkNew.OpenForm;
begin
 Self.ActiveControl := Self.RG_NewTyp;
 F_BlkNew.ShowModal();
end;

procedure TF_BlkNew.B_SaveClick(Sender: TObject);
begin
 if (RG_NewTyp.ItemIndex = -1) then
  begin
   Application.MessageBox('Vyberte typ bloku', 'Nelze vytvořit blok', MB_OK OR MB_ICONWARNING);
   Exit;
  end;

 Self.Close();

 case RG_NewTyp.ItemIndex of
  0 : F_BlkVyhybka.NewBlkCreate();
  1 : F_BlkUsek.NewBlkCreate();
  2 : F_BlkIR.NewBlkCreate();
  3 : F_BlkNav.NewBlkCreate();
  4 : F_BlkPrejezd.NewBlkCreate();
  5 : F_BlkTrat.NewBlkCreate();
  6 : F_BlkZamek.NewBlkCreate();
  7 : F_BlkRozp.NewBlkCreate();
  8 : F_BlkTU.NewBlkCreate();
  9 : F_BlkIO.NewBlkCreate();
  10: F_BlkSH.NewBlkCreate();
  11: F_BlkAC.NewBlkCreate();
 end;
end;

end.//unit
