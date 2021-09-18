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

uses fBlkTurnout, fBlkTrack, fBlkIR, fBlkSignal, fMain, fBlkSummary, fBlkAC,
  BlockDb, fBlkCrossing, fBlkRailway, fBlkLock, fBlkDisconnector, fBlkRT,
  fBlkIO, fBlkGroupSignal, fBlkPst;

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
    Exit();
  end;

  Self.Close();

  case RG_NewTyp.ItemIndex of
    0:
      F_BlkTurnout.NewBlkCreate();
    1:
      F_BlkTrack.NewBlkCreate();
    2:
      F_BlkIR.NewBlkCreate();
    3:
      F_BlkSignal.NewBlkCreate();
    4:
      F_BlkGroupSignal.NewBlk();
    5:
      F_BlkCrossing.NewBlkCreate();
    6:
      F_BlkRailway.NewBlk();
    7:
      F_BlkLock.NewBlkCreate();
    8:
      F_BlkDisconnector.NewBlkCreate();
    9:
      F_BlkRT.NewBlkCreate();
    10:
      F_BlkIO.NewBlkCreate();
    11:
      F_BlkSummary.NewBlkCreate();
    12:
      F_BlkAC.NewBlkCreate();
    13:
      F_BlkPst.NewBlock();
  end;
end;

end.// unit
