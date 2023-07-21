unit fAbout;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, fMain, ShellAPI, pngimage;

type
  TF_About = class(TForm)
    ST_about1: TStaticText;
    ST_about2: TStaticText;
    ST_about3: TStaticText;
    ST_about4: TStaticText;
    ST_about5: TStaticText;
    B_OK: TButton;
    I_Horasystems: TImage;
    GB_Info: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    L_VApp: TLabel;
    L_VRCSLib: TLabel;
    L_VRCSUSB: TLabel;
    Label6: TLabel;
    L_VRCSDriver: TLabel;
    I_AppIcon: TImage;
    procedure FormShow(Sender: TObject);
    procedure B_OKClick(Sender: TObject);
    procedure ST_about5Click(Sender: TObject);
    procedure ST_about3Click(Sender: TObject);
    procedure B_RegistraceClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_About: TF_About;

implementation

uses version, TechnologieRCS, Logging, appEv;

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
begin
  Self.ST_about5.Font.Color := clBlue;
  Self.ST_about3.Font.Color := clBlue;

  Self.L_VApp.Caption := VersionStr(Application.ExeName) + ' (' + FormatDateTime('dd.mm.yyyy hh:nn:ss', BuildDateTime()) + ')';

  Self.L_VRCSLib.Caption := RCSi.Lib;

  try
    Self.L_VRCSDriver.Caption := RCSi.GetDllVersion();
  except
    on E: Exception do
    begin
      Self.L_VRCSDriver.Caption := 'nelze získat';
      AppEvents.LogException(E, 'RCS.GetDllVersion');
    end;
  end;

  try
    if (RCSi.Opened) then
      Self.L_VRCSUSB.Caption := RCSi.GetDeviceVersion()
    else
      Self.L_VRCSUSB.Caption := 'zařízení uzavřeno';
  except
    on E: Exception do
    begin
      Self.L_VRCSUSB.Caption := 'nelze získat';
      AppEvents.LogException(E, 'RCS.GetDeviceVersion');
    end;
  end;
end;

procedure TF_About.B_OKClick(Sender: TObject);
begin
  F_About.Close;
end;

procedure TF_About.ST_about5Click(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  ShellExecute(0, nil, PChar(ST_about5.Caption), nil, nil, 0);
  Screen.Cursor := crDefault;
  ST_about5.Font.Color := clPurple;
end;

procedure TF_About.ST_about3Click(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  ShellExecute(0, nil, PChar('mailto:' + ST_about3.Caption), nil, nil, 0);
  Screen.Cursor := crDefault;
  ST_about3.Font.Color := clPurple;
end;

procedure TF_About.B_RegistraceClick(Sender: TObject);
begin
  F_About.Close;
end;

end.// unit
