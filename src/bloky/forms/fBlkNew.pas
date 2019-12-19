﻿unit fBlkNew;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, inifiles, Menus, ComCtrls, TabNotBk,
  ExtCtrls, Mask, Gauges, StrUtils, Registry, Grids, jpeg, ShellAPI,
  ShlObj, ToolWin, CPortCtl, CPort, winsock, Buttons;

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

uses fBlkVyhybka, fBlkUsek, fBlkIR, fBlkNav, fMain, fSettings, fBlkSH,
      TBloky, fBlkPrejezd, fBlkTrat, fBlkZamek, fBlkRozp, fBlkTU, fBlkVystup;

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
  9 : F_BlkVystup.NewBlkCreate();
  10: F_BlkSH.NewBlkCreate();
 end;
end;

end.//unit
