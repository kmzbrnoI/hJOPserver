unit fSystemAutoStart;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TF_AutoStartSystems = class(TForm)
    B_Abort: TButton;
    L_Systems1: TLabel;
    L_Systems2: TLabel;
    L_Cas: TLabel;
    L_Systems3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure B_AbortClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_AutoStartSystems: TF_AutoStartSystems;

implementation

uses fMain, Logging;

{$R *.dfm}

procedure TF_AutoStartSystems.FormShow(Sender: TObject);
begin
  F_AutoStartSystems.Show;
end;

procedure TF_AutoStartSystems.B_AbortClick(Sender: TObject);
begin
  Log('Automaticke pripojovani k systemum selhalo - vstup uzivatele', ltMessage);
  F_Main.KomunikacePocitani := 0;
  F_AutoStartSystems.Close;
end;

end.// unit
