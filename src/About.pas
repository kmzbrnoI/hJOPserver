unit About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, Main, ShellAPI;

type
  TF_About = class(TForm)
    ST_about1: TStaticText;
    ST_about2: TStaticText;
    ST_about3: TStaticText;
    ST_about4: TStaticText;
    ST_about5: TStaticText;
    B_OK: TButton;
    GB_Registrace: TGroupBox;
    I_Horasystems: TImage;
    B_Registrace: TButton;
    GB_Info: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    L_VApp: TLabel;
    L_VMTBLib: TLabel;
    L_VMTBUSB: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    L_RegUser: TLabel;
    L_RegCompany: TLabel;
    Label6: TLabel;
    L_VMTBDriver: TLabel;
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

uses Licence, Verze, RPConst, LoginPozadi, TechnologieMTB, Logging;

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
 begin
  writelog('Zobrazeno okno O programu',WR_MESSAGE);
  Self.ST_about5.Font.Color := clBlue;
  Self.ST_about3.Font.Color := clBlue;

  Self.L_VApp.Caption       := NactiVerzi(Application.ExeName)+' ('+GetLastBuildDate+' '+GetLastBuildTime+')';
  Self.L_VMTBLib.Caption    := MTB.LibV;
  Self.L_VMTBDriver.Caption := MTB.DriverV;
  Self.L_VMTBUSB.Caption    := MTB.DeviceV;

  if (Lic.Prijato) then
   begin
    Self.L_RegUser.Caption := Lic.UserJmeno+' '+Lic.UserPrijmeni;
    if (Lic.Company = 'nil') then Self.L_RegCompany.Caption := '' else Self.L_RegCompany.Caption := Lic.Company;
   end else begin
    Self.L_RegUser.Caption    := 'Nezaregistrováno';
    Self.L_RegCompany.Caption := 'Nezaregistrováno';
   end;
 end;//procedure

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
 end;//procedure

procedure TF_About.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  writelog('Skryto okno O programu',WR_MESSAGE);
 end;//procedure

procedure TF_About.ST_about3Click(Sender: TObject);
 begin
  Screen.Cursor := crAppStart;
  ShellExecute(0,nil,PChar('mailto:'+ST_about3.Caption),nil,nil,0);
  Screen.Cursor := crDefault;
  ST_about3.Font.Color := clPurple;
 end;//procedure

procedure TF_About.B_RegistraceClick(Sender: TObject);
 begin
  F_About.Close;
  F_Licence.OpenFormPrereg;
 end;//procedure

end.//unit
