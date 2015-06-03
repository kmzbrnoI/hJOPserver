unit Licence;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, Spin, ExtCtrls, ComCtrls;

type
  TLicence=class                                      //data o licenci
   UserJmeno:String;                                   //jmeno uzivatele
   UserPrijmeni:String;                                //prijmeni uzivatele
   Company:String;                                     //registrovano na spolecnost
   Cislo:String;                                       //licencni cislo
   Prijato:Boolean;                                    //jestli je prijato lic. cislo
   Demo:Boolean;                                       //jestli se jedna o DEMO verzi programu
   ZbyvaDnu:Byte;                                      //kolik dni zbyva do vyprseni DEMO verze
   function KontrolaCisla(Cislo:String):Integer;       //funkce, ktera zkontroluje licenci
  end;

  TF_Licence = class(TForm)
    B_Demo: TButton;
    PC_1: TPageControl;
    TS_Licence: TTabSheet;
    TS_Spolecnost: TTabSheet;
    TS_User: TTabSheet;
    L_Lic4: TLabel;
    ME_Licence: TMaskEdit;
    B_Submit: TButton;
    B_MutiApply: TButton;
    L_Lic1: TLabel;
    E_Multi_UserFName: TEdit;
    E_Multi_UserSName: TEdit;
    L_Lic2: TLabel;
    L_Lic3: TLabel;
    E_Multi_Company: TEdit;
    L_Lic5: TLabel;
    E_User_UserFName: TEdit;
    E_User_UserSName: TEdit;
    L_Lic6: TLabel;
    B_UzApply: TButton;
    procedure B_SubmitClick(Sender: TObject);
    procedure B_DemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure B_MutiApplyClick(Sender: TObject);
    procedure B_UzApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
   procedure ZkontrolujStartLic;
   procedure LoadLicFile(LicFile:String);
   procedure SaveLicFile(LicFile:String);
   procedure OpenFormPrereg;
  end;

var
  F_Licence: TF_Licence;
  Lic:TLicence;                            //Data o licenci

implementation

uses Main,About,Verze,RPConst, NoRegRun, LoginPozadi, Settings, Logging;

{$R *.dfm}

procedure TF_Licence.B_SubmitClick(Sender: TObject);
var Typ:Byte;
 begin
  Typ := Lic.KontrolaCisla(ME_licence.Text);
  if (Typ > 0) then
   begin
    B_Demo.Enabled := false;
    Application.MessageBox('Licenèní èíslo bylo pøijato','Registrace',MB_OK OR MB_ICONINFORMATION);
    Lic.Prijato := true;
    TS_Licence.TabVisible := false;
    if (Typ = 1) then
     begin
      TS_User.TabVisible := true;
      E_User_UserFName.SetFocus;
     end else begin
      TS_Spolecnost.TabVisible := true;
      E_Multi_UserFName.SetFocus;
     end;
    if (F_Licence.BorderIcons = [biSystemMenu]) then
     begin
      F_Licence.BorderIcons := [];
     end;//if F_Licence.BorderIcons = [biSystemMenu]
   end else begin
    Application.MessageBox('Licenèní èíslo nebylo pøijato, opakujte prosím zadání','Registrace',MB_OK OR MB_ICONINFORMATION);
    Lic.Prijato := false;
   end;//if Lic.KontrolaCisla...
 end;//procedure

procedure TF_Licence.ZkontrolujStartLic;
 begin
  LoadLicFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\Reg.cac');
  if (Lic.KontrolaCisla(Lic.Cislo) >= 1) then
   begin
    Lic.Prijato := true;
   end else begin
    Lic.Prijato := false;   
   end;

  if not (Lic.Prijato) then
   begin
    F_Main.Caption := F_Main.Caption +' - NOT REGISTERED';
    F_NoRegRun.CheckStartLic;
   end else begin
    if (Lic.Company = 'nil') then
     begin
      F_Main.Caption := F_Main.Caption +' - Registered to '+ Lic.UserJmeno+' '+lic.UserPrijmeni;
     end else begin
      F_Main.Caption := F_Main.Caption +' - Registered to '+ Lic.Company;
     end;
   end;
 end;//procedure


procedure TF_Licence.B_DemoClick(Sender: TObject);
 begin
  Lic.ZbyvaDnu := 15;
  SaveLicFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\Reg.cac');//525-959-261-565
  Application.MessageBox(PChar('Zbývá Vám '+IntToStr(Lic.ZbyvaDnu)+' dní'),'Registrace',MB_OK OR MB_ICONINFORMATION);
  Lic.Demo := true;
  F_Licence.Close;
 end;//procedure

procedure TF_Licence.LoadLicFile(LicFile:String);
var M_Vysvetlivky:TMemo;
    PoleDat:TStrings;
    BufferStr:TStrings;
    cyklus:Integer;
    Buffer:String;
    PrazdnaPolozka:Boolean;
 begin
  writelog('Naèítám zdroj registrace - '+LicFile,WR_DATA);

  if (not FileExists(LicFile)) then
   begin
    writelog('Chyba pri nacitani registracniho souboru - soubor neexistuje'+LicFile,WR_ERROR,9);
    Exit;
   end;
  PoleDat := TStringList.Create;
  M_Vysvetlivky := TMemo.Create(F_Main);
  M_Vysvetlivky.Parent := F_Main;
  M_Vysvetlivky.Visible := false;
  M_Vysvetlivky.Width := 1000;
  M_Vysvetlivky.Lines.LoadFromFile(LicFile);

  PoleDat.Clear;
  ExtractStringsEx([';'],M_Vysvetlivky.Text,PoleDat);

  PrazdnaPolozka := false;
  for cyklus := 0 to 5 do
   begin
    if (PoleDat.Count <= cyklus) then
     begin
      PrazdnaPolozka := true;
      Break;
     end;
    if (PoleDat[cyklus] = '') then
     begin
      PrazdnaPolozka := true;
      Break;
     end;//if PoleDat[cyklus] = ''
   end;//for cyklus
  if (M_Vysvetlivky.Text = '') or (PoleDat.Count <> 6) or (PoleDat[0] = '') or (PrazdnaPolozka) then
   begin
    Lic.UserJmeno := '';
    Lic.UserPrijmeni := '';
    Lic.Company := '';
    Lic.Cislo := '';
    Lic.Prijato := false;
    writelog('Chyba pri nacitani souboru z licencnimi udaji',WR_ERROR,9);
    Exit;
   end;

  BufferStr := TStringList.Create;

  //nacitani jmena zacatek
  Buffer := '';
  BufferStr.Clear;
  ExtractStringsEx(['-'],PoleDat[0],BufferStr);
  for cyklus := 0 to BufferStr.Count-1 do
   begin
    Buffer := Buffer + chr(StrToInt(BufferStr[cyklus]));
   end;//for cyklus
  Lic.UserJmeno := Buffer;
  //nacitani jmena konec
  //nacitani prijmeni zacatek
  Buffer := '';
  BufferStr.Clear;
  ExtractStringsEx(['-'],PoleDat[1],BufferStr);
  for cyklus := 0 to BufferStr.Count-1 do
   begin
    Buffer := Buffer + chr(StrToInt(BufferStr[cyklus]));
   end;//for cyklus
  Lic.UserPrijmeni := Buffer;
  //nacitani prijmeni konec
  //nacitani spolecnosti zacatek
  Buffer := '';
  BufferStr.Clear;
  ExtractStringsEx(['-'],PoleDat[2],BufferStr);
  for cyklus := 0 to BufferStr.Count-1 do
   begin
    Buffer := Buffer + chr(StrToInt(BufferStr[cyklus]));
   end;//for cyklus
  Lic.Company := Buffer;
  //nacitani spolecnosti konec
  Lic.Cislo    := PoleDat[3];
  Lic.Demo     := StrToBool(IntToStr(StrToIntDef(PoleDat[4],64)-64));
  Lic.ZbyvaDnu := StrToIntDef(PoleDat[5],32)-32;

  Buffer := '';
  BufferStr.Free;
  PoleDat.Free;
  M_Vysvetlivky.Free;
  writelog('Uspesne nacteny licencni udaje',WR_MESSAGE);
 end;//procedure

procedure TF_Licence.SaveLicFile(LicFile:String);
var M_Vysvetlivky:TMemo;
    cyklus:Integer;
    Buffer:String;
    SaveString:String;
 begin
  writelog('Ukladam soubor registrace - '+LicFile,WR_DATA);

  SaveString := '';

  //ukladani jmena zacatek
  Buffer := '';
  for cyklus := 0 to Length(Lic.UserJmeno)-1 do
   begin
    if Buffer <> '' then
     begin
      Buffer := Buffer + '-' + IntToStr(ord(Lic.UserJmeno[cyklus+1]));
     end else begin
      Buffer := IntToStr(ord(Lic.UserJmeno[cyklus+1]));
     end;
   end;//for cyklus
  SaveString := Buffer;
  //ukladani jmena konec
  //ukladani prijmeni zacatek
  Buffer := '';
  for cyklus := 0 to Length(Lic.UserPrijmeni)-1 do
   begin
    if Buffer <> '' then
     begin
      Buffer := Buffer + '-' + IntToStr(ord(Lic.UserPrijmeni[cyklus+1]));
     end else begin
      Buffer := IntToStr(ord(Lic.UserPrijmeni[cyklus+1]));
     end;
   end;//for cyklus
  SaveString := SaveString + ';' + Buffer;
  //ukladani prijmeni konec
  //ukladani spolecnosti zacatek
  Buffer := '';
  for cyklus := 0 to Length(Lic.Company)-1 do
   begin
    if Buffer <> '' then
     begin
      Buffer := Buffer + '-' + IntToStr(ord(Lic.Company[cyklus+1]));
     end else begin
      Buffer := IntToStr(ord(Lic.Company[cyklus+1]));
     end;
   end;//for cyklus
  SaveString := SaveString + ';' + Buffer;
  //ukladani spolecnosti konec
  SaveString := SaveString + ';' + Lic.Cislo;
  SaveString := SaveString + ';' + IntToStr(StrToInt(BoolToStr(Lic.Demo))+64);
  SaveString := SaveString + ';' + IntToStr(Lic.ZbyvaDnu+32);

  M_Vysvetlivky := TMemo.Create(F_Main);
  M_Vysvetlivky.Parent := F_Main;
  M_Vysvetlivky.Visible := false;
  M_Vysvetlivky.Width := 1000;
  M_Vysvetlivky.Text := SaveString;
  SaveString := '';
  M_Vysvetlivky.Lines.SaveToFile(LicFile);
  M_Vysvetlivky.Free;
  writelog('Uspesne ulozeny licencni udaje',WR_MESSAGE);
 end;//procedure

procedure TF_Licence.FormCreate(Sender: TObject);
 begin
  TS_Spolecnost.TabVisible := false;
  TS_User.TabVisible := false;
 end;//procedure

procedure TF_Licence.B_MutiApplyClick(Sender: TObject);
 begin
  if (E_Multi_UserFName.Text = '') then
   begin
    Application.MessageBox('Vyplòte jméno uživatele, na kterého je provádìna registarace','Nelze pøijmout',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (E_Multi_UserSName.Text = '') then
   begin
    Application.MessageBox('Vyplòte pøijmení uživatele, na kterého je provádìna registarace','Nelze pøijmout',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (E_Multi_Company.Text = '') then
   begin
    Application.MessageBox('Vyplòte spoleènost, na kterou je provádìna registarace','Nelze pøijmout',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  Lic.UserJmeno     := E_Multi_UserFName.Text;
  Lic.UserPrijmeni  := E_Multi_UserSName.Text;
  Lic.Company       := E_Multi_Company.Text;
  SaveLicFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\Reg.cac');//525-959-261-565
  F_Licence.Close;
  F_Main.SetFocus;
  F_Main.Caption:='Øídící program         v'+NactiVerzi(Application.ExeName)+' (build '+GetLastBuildDate+_VERSION_PR+')'+' - Registered to '+lic.Company;
 end;//procedure

procedure TF_Licence.B_UzApplyClick(Sender: TObject);
 begin
  if (E_User_UserFName.Text = '') then
   begin
    Application.MessageBox('Vyplòte jméno uživatele, na kterého je provádìna registarace','Nelze pøijmout',MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (E_User_UserSName.Text = '') then
   begin
    Application.MessageBox('Vyplòte pøijmení uživatele, na kterého je provádìna registarace','Nelze pøijmout',MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  Lic.UserJmeno     := E_User_UserFName.Text;
  Lic.UserPrijmeni  := E_User_UserSName.Text;
  Lic.Company       := 'nil';
  SaveLicFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'data\Reg.cac');//525-959-261-565
  F_Licence.Close;
  F_Main.SetFocus;
  F_Main.Caption:='Øídící program         v'+NactiVerzi(Application.ExeName)+' (build '+GetLastBuildDate+_VERSION_PR+')'+' - Registered to '+lic.UserJmeno+' '+lic.UserPrijmeni;
 end;//procedure

procedure TF_Licence.OpenFormPrereg;
 begin
  TS_Licence.TabVisible     := true;
  TS_Spolecnost.TabVisible  := false;
  TS_User.TabVisible        := false;
  B_Demo.Enabled            := false;
  ME_Licence.Text           := '000-000-000-000';
  F_Licence.BorderIcons     := [biSystemMenu];

  F_Licence.ShowModal;
 end;//procedure

procedure TF_Licence.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  F_Pozadi.CloseForm;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

//kontrola licencniho cisla
function TLicence.KontrolaCisla(Cislo:String):Integer;
var Cislice:array[0..11] of SmallInt;
    Cyklus:Integer;
    Pom:Integer;
    OK:Boolean;
 begin
  if (Length(Cislo) = 15) then
   begin
    OK := true;
    Pom := 0;
    for cyklus := 1 to 15 do
     begin
      case Cislo[cyklus] of
       '0'..'9':begin
                 Cislice[Pom] := StrToIntDef(Cislo[cyklus],0);
                 Pom := Pom + 1;
                end;
      end;//case
     end;//for cyklus

    if (Cislice[1]+Cislice[2] <> Cislice[3]-Cislice[6]) then OK := false;
    if (Cislice[5] = 0) then OK := false;

    if (OK) then
     begin
      Result := 1+(Cislice[9] mod 2);
      Lic.Cislo := Cislo;
     end else begin
      Result := 0;
     end;
   end else begin//if (Length(Cislo) = 15)
    Result := 0;
   end;
 end;//function


end.//unit

