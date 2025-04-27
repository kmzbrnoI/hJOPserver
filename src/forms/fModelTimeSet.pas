unit fModelTimeSet;

{
  Okno nastaveni modeloveho casu.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mask, StrUtils;

type
  TF_ModelTimeSet = class(TForm)
    ME_start_time: TMaskEdit;
    L_time_start: TLabel;
    B_OK: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    ME_Nasobic: TMaskEdit;
    CHB_Used: TCheckBox;
    procedure B_OKClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure ME_start_timeKeyPress(Sender: TObject; var Key: Char);
    procedure CHB_UsedClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OpenForm;
  end;

var
  F_ModelTimeSet: TF_ModelTimeSet;

implementation

uses TimeModel, ownGuiUtils;

{$R *.dfm}

procedure TF_ModelTimeSet.B_OKClick(Sender: TObject);
begin
  try
    if (StrToInt(Copy(ME_start_time.Text, 4, 2)) > 59) then
    begin
      StrMessageBox('Minuty zadejte v rozsahu 0-59', 'Nelze nastavit čas', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    if (StrToInt(LeftStr(ME_start_time.Text, 2)) > 23) then
    begin
      StrMessageBox('Hodiny zadejte v rozsahu 0-23', 'Nelze nastavit čas', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    if (StrToFloat(ME_Nasobic.Text) >= 10) then
    begin
      StrMessageBox('Násobič zadejte v rozsahu 0-9.9', 'Nelze nastavit čas', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    modelTime.used := Self.CHB_Used.Checked;
    modelTime.time := EncodeTime(StrToInt(LeftStr(ME_start_time.Text, 2)), StrToInt(Copy(ME_start_time.Text, 4, 2)), 0, 0);
    modelTime.strSpeed := Self.ME_Nasobic.Text;

    Self.Close();
  except
    on E:Exception do
      ExceptionMessageBox('Zadána neplatná data', E);
  end;

end;

procedure TF_ModelTimeSet.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_ModelTimeSet.CHB_UsedClick(Sender: TObject);
begin
  Self.ME_start_time.Enabled := Self.CHB_Used.Checked;
  Self.ME_Nasobic.Enabled := Self.CHB_Used.Checked;
end;

procedure TF_ModelTimeSet.ME_start_timeKeyPress(Sender: TObject; var Key: Char);
begin
  Key := Key;
  case Key of
    '0' .. '9', #9, #8:
      begin
      end
  else
    begin
      Key := #0;
    end;
  end; // case
end;

procedure TF_ModelTimeSet.OpenForm;
begin
  Self.CHB_Used.Checked := modelTime.used;
  Self.ME_start_time.Text := FormatDateTime('hh:nn', modelTime.time);
  Self.ME_Nasobic.Text := FloatToStrF(modelTime.speed, ffNumber, 1, 1);

  Self.CHB_UsedClick(Self.CHB_Used);

  Self.ActiveControl := Self.CHB_Used;
  Self.Show();
end;

end.// unit
