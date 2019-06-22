unit fSplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, pngimage;

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
   procedure AddStav(Text:String);
  end;

var
  F_splash: TF_splash;

implementation

{$R *.dfm}

uses fMain, Verze;

procedure TF_splash.FormCreate(Sender: TObject);
 begin
  F_splash.Show;
  Application.ProcessMessages;
 end;

procedure TF_splash.FormShow(Sender: TObject);
 begin
  ST_Version.Caption  := 'Verze '+NactiVerzi(Application.ExeName);
  L_BuildTime.Caption := GetLastBuildDate+'  '+GetLastBuildTime;
 end;

procedure TF_Splash.AddStav(Text:String);
 begin
  F_splash.L_Nacitani.Caption := Text;
  F_splash.PB_Prubeh.Position := F_splash.PB_Prubeh.Position + 1;
  F_splash.Refresh;
 end;

end.//unit
