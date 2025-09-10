unit fSplash;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, pngimage;

type
  TF_splash = class(TForm)
    ST_nazev: TStaticText;
    ST_Version: TStaticText;
    L_Created: TLabel;
    L_BuildTime: TLabel;
    I_Horasystems: TImage;
    PB_Prubeh: TProgressBar;
    L_1: TLabel;
    L_Nacitani: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure AddStav(Text: string);
  end;

var
  F_splash: TF_splash;

implementation

{$R *.dfm}

uses fMain, version, SysUtils;

procedure TF_splash.FormCreate(Sender: TObject);
begin
  Self.Show();
  Application.ProcessMessages();
end;

procedure TF_splash.FormShow(Sender: TObject);
begin
  Self.ST_Version.Caption := 'Verze ' + version.VersionStr();
  Self.L_BuildTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', BuildDateTime());
end;

procedure TF_splash.AddStav(Text: string);
begin
  Self.L_Nacitani.Caption := Text;
  Self.PB_Prubeh.Position := F_splash.PB_Prubeh.Position + 1;
  Self.Refresh();
end;

end.// unit
