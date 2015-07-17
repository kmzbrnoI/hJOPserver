unit fHVPomEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TF_HV_Pom = class(TForm)
    Label1: TLabel;
    SE_CV: TSpinEdit;
    Label2: TLabel;
    SE_Value: TSpinEdit;
    B_OK: TButton;
    B_Storno: TButton;
    procedure B_OKClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure SE_ValueKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    saved:boolean;

    procedure OpenForm(cv:Integer; val:Byte);
  end;

var
  F_HV_Pom: TF_HV_Pom;

implementation

{$R *.dfm}

procedure TF_HV_Pom.B_OKClick(Sender: TObject);
begin
 Self.saved := true;
 Self.Close();
end;

procedure TF_HV_Pom.B_StornoClick(Sender: TObject);
begin
 Self.saved := false;
 Self.Close();
end;

procedure TF_HV_Pom.OpenForm(cv:Integer; val:Byte);
begin
 Self.saved := false;
 Self.SE_CV.Value    := cv;
 Self.SE_Value.Value := val;
 Self.SE_CV.Enabled  := (cv = -1);
 if (cv = -1) then
  Self.ActiveControl := Self.SE_CV
 else
  Self.ActiveControl := Self.SE_Value;
 Self.ShowModal();
end;

procedure TF_HV_Pom.SE_ValueKeyPress(Sender: TObject; var Key: Char);
begin
 case (key) of
  #13 : Self.B_OKClick(Self);
  #$1B: Self.B_StornoClick(Self);
 end;//case
end;//procedure

end.//unit
