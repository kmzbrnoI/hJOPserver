unit fRychlostiEdit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TF_RychlostiEdit = class(TForm)
    Label1: TLabel;
    SE_Rychlost: TSpinEdit;
    B_Save: TButton;
    B_Storno: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RychlostKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
   openStep: Integer;

  public
   procedure OpenForm(step: Cardinal);
  end;

var
  F_RychlostiEdit: TF_RychlostiEdit;

implementation

uses fMain, fSettings, TechnologieTrakce;

{$R *.dfm}

procedure TF_RychlostiEdit.OpenForm(step: Cardinal);
 begin
  Self.openStep := step;
  SE_Rychlost.Value := TrakceI.Speed(step);
  F_RychlostiEdit.Caption := 'Editovat stupeň '+IntToStr(step);
  Self.ActiveControl := Self.SE_Rychlost;
  F_RychlostiEdit.ShowModal;
 end;

procedure TF_RychlostiEdit.SE_RychlostKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (key = 13) then Self.B_SaveClick(Self);
end;

procedure TF_RychlostiEdit.B_StornoClick(Sender: TObject);
 begin
  F_RychlostiEdit.Close;
 end;

procedure TF_RychlostiEdit.B_SaveClick(Sender: TObject);
 begin
  try
    TrakceI.SetStepSpeed(Self.openStep, SE_Rychlost.Value);
  except
    on E:Exception do
     begin
      Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
  end;
  F_Options.LV_DigiRych.Items[Self.openStep].SubItems[0] := IntToStr(Self.SE_Rychlost.Value)+' km/h';
  Self.Close();
 end;

procedure TF_RychlostiEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  Self.openStep := -1;
 end;

end.//unit
