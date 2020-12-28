unit fSystemInfo;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TF_SystemInfo = class(TForm)
    L_Info: TLabel;
  private
    function GetWidthLabel(Nazev: string; Font: string): Integer; // zjisti Width Labelu po zadani prislusneho textu na defaultnim fontu

  public
    procedure OpenForm(System: string);

  end;

var
  F_SystemInfo: TF_SystemInfo;

implementation

uses fMain, GetSystems;

{$R *.dfm}

procedure TF_SystemInfo.OpenForm(System: string);
 begin
  F_SystemInfo.Width := GetWidthLabel(System,'MS Sans Serif')+50;
  F_SystemInfo.Height := Round((GetWidthLabel(System,'MS Sans Serif')+50)/3);
  L_Info.Caption := System;
  F_SystemInfo.Show;
 end;

function TF_SystemInfo.GetWidthLabel(Nazev: string; Font: string): Integer;
var L_Test: TLabel;
 begin
  L_Test            := TLabel.Create(F_Main);
  L_Test.Caption    := Nazev;
  L_Test.AutoSize   := true;
  L_Test.Visible    := false;
  L_Test.Font.Name  := Font;
  Result            := L_Test.Width;
  L_Test.Free;
 end;

end.
