unit fBlkTurnoutState;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  BlockTurnout, Spin;

type
  TF_BlkTurnoutState = class(TForm)
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
    SE_Locks: TSpinEdit;
    CB_Stav_Plus: TComboBox;
    CB_Stav_Minus: TComboBox;
    SE_Zaver: TSpinEdit;
    B_Unlock: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_UpdateClick(Sender: TObject);
    procedure B_UnlockClick(Sender: TObject);
  private
    OpenBlk: TBlkTurnout;

     procedure myUpdate();
     procedure myApply();

  private const

  public
     procedure OpenForm(blk: TBlkTurnout);
  end;

var
  F_BlkTurnoutState: TF_BlkTurnoutState;

implementation

uses ownConvert;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTurnoutState.B_ApplyClick(Sender: TObject);
begin
 Self.myApply();
 Self.myUpdate();
end;

procedure TF_BlkTurnoutState.B_UnlockClick(Sender: TObject);
begin
 Self.OpenBlk.IntentionalUnlock();
 Self.myUpdate();
end;

procedure TF_BlkTurnoutState.B_UpdateClick(Sender: TObject);
begin
 Self.myUpdate();
end;

procedure TF_BlkTurnoutState.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Self.OpenBlk := nil;
end;

procedure TF_BlkTurnoutState.FormCreate(Sender: TObject);
begin
 Self.OpenBlk := nil;
end;

procedure TF_BlkTurnoutState.OpenForm(blk: TBlkTurnout);
begin
 Self.OpenBlk := blk;
 Self.myUpdate();

 Self.Caption := 'Technologické vlastnosti bloku '+blk.name;
 Self.Show();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTurnoutState.myUpdate();
begin
 Self.M_Stitek.Text := Self.OpenBlk.note;
 Self.M_Vyluka.Text := Self.OpenBlk.lockout;

 Self.SE_Zaver.Value := Self.OpenBlk.state.locks;
 Self.CB_Stav_Plus.ItemIndex := ownConvert.BoolToInt(Self.OpenBlk.movingPlus);
 Self.CB_Stav_Minus.ItemIndex := ownConvert.BoolToInt(Self.OpenBlk.movingMinus);

 Self.CB_Locked.ItemIndex := ownConvert.BoolToInt(Self.OpenBlk.outputLocked);
 Self.SE_Locks.Value := Self.OpenBlk.state.intentionalLocks;

 Self.B_Unlock.Enabled := Self.OpenBlk.intentionalLocked;
end;

procedure TF_BlkTurnoutState.myApply();
begin
 Self.OpenBlk.note := Self.M_Stitek.Text;
 Self.OpenBlk.lockout := Self.M_Vyluka.Text;

 if (Self.SE_Zaver.Value = 0) then
   Self.OpenBlk.ResetEmLocks();

 Self.OpenBlk.movingPlus := ownConvert.IntToBool(CB_Stav_Plus.ItemIndex);
 Self.OpenBlk.movingMinus := ownConvert.IntToBool(CB_Stav_Minus.ItemIndex);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
