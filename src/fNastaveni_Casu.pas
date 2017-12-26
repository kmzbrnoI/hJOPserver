unit fNastaveni_Casu;

{
  Okno nastaveni modeloveho casu.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, DateUtils, StrUtils;

type
  TF_ModCasSet = class(TForm)
    ME_start_time: TMaskEdit;
    L_time_start: TLabel;
    B_OK: TButton;
    B_Storno: TButton;
    Label1: TLabel;
    ME_Nasobic: TMaskEdit;
    procedure B_OKClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure ME_start_timeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
   procedure OpenForm;
  end;

var
  F_ModCasSet: TF_ModCasSet;

implementation

uses ModelovyCas;

{$R *.dfm}

procedure TF_ModCasSet.B_OKClick(Sender: TObject);
 begin
  try
    if (StrToInt(Copy(ME_start_time.Text, 4, 2)) > 59) then
     begin
      Application.MessageBox('Minuty zadejte v rozsahu 0-59','Nelze nastavit cas',MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    if (StrToInt(LeftStr(ME_start_time.Text, 2)) > 23) then
     begin
      Application.MessageBox('Hodiny zadejte v rozsahu 0-23','Nelze nastavit cas',MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    if (StrToFloat(ME_Nasobic.Text) >= 10) then
     begin
      Application.MessageBox('Násobiè zadejte v rozsahu 0-9.9','Nelze nastavit cas',MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    ModCas.time       := EncodeTime(StrToInt(LeftStr(ME_start_time.Text, 2)), StrToInt(Copy(ME_start_time.Text, 4, 2)), 0, 0);
    ModCas.strNasobic := Self.ME_Nasobic.Text;

    Self.Close();
  except
   Application.MessageBox('Zadána neplatná data', 'Nelze nasatvit èas', MB_OK OR MB_ICONWARNING);
  end;

 end;//procedure

procedure TF_ModCasSet.B_StornoClick(Sender: TObject);
begin
 Self.Close();
end;

procedure TF_ModCasSet.ME_start_timeKeyPress(Sender: TObject; var Key: Char);
begin
 Key := Key;
 case Key of
  '0'..'9',#9,#8:begin
              end else begin
               Key := #0;
              end;
  end;//case
end;//procedure

procedure TF_ModCasSet.OpenForm;
 begin
  Self.ME_start_time.Text := FormatDateTime('hh:nn', ModCas.time);
  Self.ME_Nasobic.Text    := FloatToStrF(ModCas.nasobic, ffNumber, 1, 1);

  Self.ActiveControl := Self.ME_start_time;
  Self.Show();
 end;//procedure


end.//unix
