unit NoRegRun;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TF_NoRegRun = class(TForm)
    B_Register: TButton;
    P_1: TPanel;
    B_1: TButton;
    B_2: TButton;
    B_3: TButton;
    L_1: TLabel;
    ST_1: TStaticText;
    ST_2: TStaticText;
    procedure B_RegisterClick(Sender: TObject);
    procedure B_1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   FuncButton:Byte;
  public
   procedure CheckStartLic;
  end;

var
  F_NoRegRun: TF_NoRegRun;

implementation

uses Logging, Licence, LoginPozadi, RPConst, Main;

{$R *.dfm}

procedure TF_NoRegRun.CheckStartLic;
 begin
  writelog('Program neni registrovan - oteviram okno Vitejte',WR_MESSAGE,9);
  FuncButton    := Random(3)+1;
  L_1.Caption   := 'Klepnutím na tlaèítko '+IntToStr(FuncButton)+' spustíte program';
  F_Main.Enabled := false;
  F_Pozadi.OpenForm(false);
  F_NoRegRun.ShowModal;
 end;//procedure

procedure TF_NoRegRun.B_RegisterClick(Sender: TObject);
 begin
  writelog('Program neni registrovan - system prechazi do registracniho okna',WR_MESSAGE,9);
  F_NoRegRun.Close;
  F_Main.Enabled := true;
  F_Licence.ShowModal;
 end;//procedure

procedure TF_NoRegRun.B_1Click(Sender: TObject);
 begin
  F_Main.Enabled := true;
  if ((Sender as TButton).Tag = FuncButton) then
   begin
    writelog('Program neni registrovan - zavreno okno Vitejte - demo',WR_MESSAGE,9);
    F_NoRegRun.Close;
   end else begin//if
    writelog('Program neni registrovan - zavreno okno Vitejte - spatne tlacitko',WR_MESSAGE,9);   
    F_NoRegRun.Close;
    CloseMessage := false;
   end;
 end;//procedure

procedure TF_NoRegRun.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  F_Pozadi.CloseForm; 
 end;//proedure

end.//unit
