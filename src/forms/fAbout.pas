unit fAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, fMain, ShellAPI, pngimage;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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

uses Verze, TechnologieRCS, Logging, appEv;

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
 begin
  writelog('Zobrazeno okno O programu',WR_MESSAGE);
  Self.ST_about5.Font.Color := clBlue;
  Self.ST_about3.Font.Color := clBlue;

  Self.L_VApp.Caption       := NactiVerzi(Application.ExeName)+' ('+GetLastBuildDate+' '+GetLastBuildTime+')';

  Self.L_VRCSLib.Caption    := RCSi.Lib;

  try
    Self.L_VRCSDriver.Caption := RCSi.GetDllVersion();
  except
    on E:Exception do
     begin
      Self.L_VRCSDriver.Caption := 'nelze získat';
      AppEvents.LogException(e, 'RCS.GetDllVersion');
     end;
  end;

  try
    if (RCSi.Opened) then
      Self.L_VRCSUSB.Caption := RCSi.GetDeviceVersion()
    else
      Self.L_VRCSUSB.Caption := 'zaøízení uzavøeno';
  except
    on E:Exception do
     begin
      Self.L_VRCSUSB.Caption := 'nelze získat';
      AppEvents.LogException(e, 'RCS.GetDeviceVersion');
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
  ShellExecute(0,nil,PChar(ST_about5.Caption),nil,nil,0);
  Screen.Cursor := crDefault;
  ST_about5.Font.Color := clPurple;
 end;

procedure TF_About.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  writelog('Skryto okno O programu',WR_MESSAGE);
 end;

procedure TF_About.ST_about3Click(Sender: TObject);
 begin
  Screen.Cursor := crAppStart;
  ShellExecute(0,nil,PChar('mailto:'+ST_about3.Caption),nil,nil,0);
  Screen.Cursor := crDefault;
  ST_about3.Font.Color := clPurple;
 end;

procedure TF_About.B_RegistraceClick(Sender: TObject);
 begin
  F_About.Close;
 end;

end.//unit
