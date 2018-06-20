unit fLoginPozadi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TF_Pozadi = class(TForm)
    T_Ztmavovani: TTimer;
    procedure T_ZtmavovaniTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
   ZavritOkno:Boolean;
  public
   procedure OpenForm(Postupne:Boolean);
   procedure CloseForm;
  end;

var
  F_Pozadi: TF_Pozadi;

implementation

uses StdConvs, fMain;

{$R *.dfm}

procedure TF_Pozadi.T_ZtmavovaniTimer(Sender: TObject);
begin
 if F_Pozadi.AlphaBlendValue <= 150 then
  begin
   F_Pozadi.AlphaBlendValue := F_Pozadi.AlphaBlendValue+5;
  end else begin
   T_Ztmavovani.Enabled:=false;
  end;
end;

procedure TF_Pozadi.OpenForm(Postupne:Boolean);
 begin
  T_Ztmavovani.Enabled := Postupne;
  if (Postupne) then
   begin
    F_Pozadi.AlphaBlendValue := 0;
   end else begin
    F_Pozadi.AlphaBlendValue := 150;    
   end;

  Self.Width  := F_Main.Width;
  Self.Height := F_Main.Height;
  Self.Left   := F_Main.Left;
  Self.Top    := F_Main.Top;
  Self.Show;
 end;//procedure

procedure TF_Pozadi.CloseForm;
 begin
  ZavritOkno := true;
  F_Pozadi.Close;
 end;//procedure

procedure TF_Pozadi.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
 begin
  CanClose := ZavritOkno;
  if (ZavritOkno) then ZavritOkno := false;
 end;//procedure

end.//unit
