unit fRychlostiEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

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
   OpenIndex:Integer;
  public
   procedure OpenForm(Stupen:Integer);
  end;

var
  F_RychlostiEdit: TF_RychlostiEdit;

implementation

uses fMain, fSettings;

{$R *.dfm}

procedure TF_RychlostiEdit.OpenForm(Stupen:Integer);
 begin
  OpenIndex := Stupen;
  SE_Rychlost.Value := TrkSystem.GetStepSpeed(stupen);
  F_RychlostiEdit.Caption := 'Editovat stupeò '+IntToStr(Stupen);
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
 end;//procedure

procedure TF_RychlostiEdit.B_SaveClick(Sender: TObject);
 begin
  TrkSystem.SetSpetSpeed(OpenIndex,SE_Rychlost.Value);
  F_Options.LV_DigiRych.Items[OpenIndex].SubItems.Strings[0] := IntToStr(SE_Rychlost.Value)+' km/h';
  F_RychlostiEdit.Close;
 end;//procedure

procedure TF_RychlostiEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  OpenIndex := -1;
 end;//procedure

end.//unit
