unit fSystemInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_SystemInfo = class(TForm)
    L_Info: TLabel;
  private
    { Private declarations }
  public
    procedure OpenForm(System:String);
  end;

var
  F_SystemInfo: TF_SystemInfo;

implementation

uses fMain, GetSystems;

{$R *.dfm}

procedure TF_SystemInfo.OpenForm(System:String);
 begin
  F_SystemInfo.Width := GetFunctions.GetWidthLabel(System,'MS Sans Serif')+50;
  F_SystemInfo.Height := Round((GetFunctions.GetWidthLabel(System,'MS Sans Serif')+50)/3);
  L_Info.Caption := System;
  F_SystemInfo.Show;
 end;//procedure

end.
