unit fBlkTurnoutState;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  BlockTurnout, Spin;

type
  TF_BlkTurnoutState = class(TForm)
    B_Refresh: TButton;
    B_Apply: TButton;
    M_Lockout: TMemo;
    Label6: TLabel;
    M_Note: TMemo;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    SE_Locks: TSpinEdit;
    SE_Zaver: TSpinEdit;
    B_Unlock: TButton;
    CHB_Moving_Plus: TCheckBox;
    CHB_Moving_Minus: TCheckBox;
    CHB_Locked: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_RefreshClick(Sender: TObject);
    procedure B_UnlockClick(Sender: TObject);
  private
    turnout: TBlkTurnout;

    procedure Refresh();
    procedure Apply();

  public
    procedure Open(turnout: TBlkTurnout);

  end;

var
  F_BlkTurnoutState: TF_BlkTurnoutState;

implementation

uses ownConvert;

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTurnoutState.B_ApplyClick(Sender: TObject);
begin
  Self.Apply();
  Self.Refresh();
end;

procedure TF_BlkTurnoutState.B_UnlockClick(Sender: TObject);
begin
  Self.turnout.IntentionalUnlock();
  Self.Refresh();
end;

procedure TF_BlkTurnoutState.B_RefreshClick(Sender: TObject);
begin
  Self.Refresh();
end;

procedure TF_BlkTurnoutState.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.turnout := nil;
end;

procedure TF_BlkTurnoutState.FormCreate(Sender: TObject);
begin
  Self.turnout := nil;
end;

procedure TF_BlkTurnoutState.Open(turnout: TBlkTurnout);
begin
  Self.turnout := turnout;
  Self.Refresh();
  Self.Caption := 'Stav výhybky ' + turnout.name;
  Self.Show();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTurnoutState.Refresh();
begin
  Self.M_Note.Text := Self.turnout.note;
  Self.M_Lockout.Text := Self.turnout.lockout;

  Self.SE_Zaver.Value := Self.turnout.state.locks;
  Self.CHB_Moving_Plus.Checked := Self.turnout.movingPlus;
  Self.CHB_Moving_Minus.Checked := Self.turnout.movingMinus;

  Self.CHB_Locked.Checked := Self.turnout.outputLocked;
  Self.SE_Locks.Value := Self.turnout.state.intentionalLocks;

  Self.B_Unlock.Enabled := Self.turnout.intentionalLocked;
end;

procedure TF_BlkTurnoutState.Apply();
begin
  if (Self.turnout = nil) then
    Exit();

  Self.turnout.note := Self.M_Note.Text;
  Self.turnout.lockout := Self.M_Lockout.Text;

  if (Self.SE_Zaver.Value = 0) then
    Self.turnout.ResetEmLocks();

  Self.turnout.movingPlus := Self.CHB_Moving_Plus.Checked;
  Self.turnout.movingMinus := Self.CHB_Moving_Minus.Checked;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
