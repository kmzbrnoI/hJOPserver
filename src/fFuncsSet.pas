unit fFuncsSet;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TF_FuncsSet = class(TForm)
    Label1: TLabel;
    CB_Vyznam: TComboBox;
    RG_Stav: TRadioGroup;
    Label2: TLabel;
    B_Apply: TButton;
    L_Status: TLabel;
    procedure B_ApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     procedure FuncsSetOK(Sender:TObject; Data:Pointer);
  public
    { Public declarations }
  end;

var
  F_FuncsSet: TF_FuncsSet;

implementation

uses Main, Trakce;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// nastavit funkce:

procedure TF_FuncsSet.B_ApplyClick(Sender: TObject);
begin
 if (not TrkSystem.openned) then
  begin
   Application.MessageBox('Aplikace není pøipojena k centrále', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;
 if (Self.CB_Vyznam.Text = '') then
  begin
   Application.MessageBox('Vyplòte význam funkce', 'Nelze pokraèovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 Self.B_Apply.Enabled   := false;
 Self.RG_Stav.Enabled   := false;
 Self.CB_Vyznam.Enabled := false;

 Self.L_Status.Font.Color := clGray;
 Self.L_Status.Caption    := 'Nastavuji funkci...';

 Application.ProcessMessages();

 TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.FuncsSetOK);
 TrkSystem.callback_err := TTrakce.GenerateCallback(Self.FuncsSetOK);
 TrkSystem.LoksSetFunc(Self.CB_Vyznam.Text, (Self.RG_Stav.ItemIndex = 1));
end;

////////////////////////////////////////////////////////////////////////////////
// callback uspesneho nastaveni funkci:

procedure TF_FuncsSet.FuncsSetOK(Sender:TObject; Data:Pointer);
begin
 Self.B_Apply.Enabled   := true;
 Self.RG_Stav.Enabled   := true;
 Self.CB_Vyznam.Enabled := true;

 Self.L_Status.Font.Color := clGreen;
 Self.L_Status.Caption    := 'Funkce nastaveny';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_FuncsSet.FormShow(Sender: TObject);
begin
 if (Self.L_Status.Caption = 'Funkce nastaveny') then Self.L_Status.Caption := '';
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
