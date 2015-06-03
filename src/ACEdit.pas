unit ACEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ComCtrls, AC;

type
  TF_AutRezEdit = class(TForm)
    GB_AutRez_Statistika: TGroupBox;
    L_AutRez_StSpusteno: TLabel;
    L_AutRez_stDokonceno: TLabel;
    B_AutRez_StDelete: TButton;
    B_Save: TButton;
    B_Storno: TButton;
    E_Soubor: TEdit;
    L_AR7: TLabel;
    B_Prochazet: TButton;
    OD_File: TOpenDialog;
    procedure B_SaveClick(Sender: TObject);
    procedure B_AutRez_StDeleteClick(Sender: TObject);
    procedure B_StornoClick(Sender: TObject);
    procedure B_ProchazetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    NewAutRez:Boolean;
    OpenIndex:integer;
    OpenAC:TAC;
     procedure NormalOpenForm;
     procedure NewOpenForm;
     procedure HlavniOpenForm;
  public
     procedure OpenForm(AutIndex:integer);
     procedure NewAutRezCreate;
  end;

var
  F_AutRezEdit: TF_AutRezEdit;

implementation

uses Main, GetSystems, Prevody,
     FileSystem, Settings, TBloky, TBlok, TBlokUsek, TBlokVyhybka, TBlokSCom,
     ACDatabase, DataAC;

{$R *.dfm}

procedure TF_AutRezEdit.OpenForm(AutIndex:integer);
 begin
  OpenIndex := AutIndex;

  if (OpenIndex >= 0) then
    Self.OpenAC := ACDb.ACs[AutIndex];

  HlavniOpenForm;
  if (NewAutRez) then
   begin
    NewOpenForm;
   end else begin
    NormalOpenForm;
   end;//NewAutRez

  F_AutRezEdit.ShowModal;
 end;//procedure

procedure TF_AutRezEdit.B_SaveClick(Sender: TObject);
begin
 if (Self.NewAutRez) then
  begin
   Self.OpenAC := ACDb.AddAC();
   if (Self.OpenAC = nil) then Exit();
  end;

 Self.OpenAC.krk_filename := Self.E_Soubor.Text;

 ACTableData.LoadToTable();

 if (Self.NewAutRez) then
  Self.OpenAC.LoadKrkFile(Self.OpenAC.krk_filename);

 Self.Close;
end;//procedure

procedure TF_AutRezEdit.B_AutRez_StDeleteClick(Sender: TObject);
begin
 Self.OpenAC.stat_run := 0;
 Self.OpenAC.stat_end := 0;

 L_AutRez_StSpusteno.Caption  := 'Spusteno : 0';
 L_AutRez_stDokonceno.Caption := 'Dokonceno : 0';

 Self.Enabled := false;
end;

procedure TF_AutRezEdit.NewAutRezCreate;
 begin
  NewAutRez := true;
  Self.OpenForm(-1);
 end;

procedure TF_AutRezEdit.B_StornoClick(Sender: TObject);
 begin
  F_AutRezEdit.Close;
 end;//procedure

procedure TF_AutRezEdit.B_ProchazetClick(Sender: TObject);
 begin
  OD_File.InitialDir := ExtractFileDir(E_Soubor.Text);
  if (OD_File.Execute) then
    E_Soubor.Text := ExtractRelativePath(ExtractFilePath(Application.ExeName),Self.OD_File.FileName);
 end;//procedure

procedure TF_AutRezEdit.HlavniOpenForm;
 begin
  //prazdno
 end;

procedure TF_AutRezEdit.NormalOpenForm;
 begin
  Self.L_AutRez_StSpusteno.Caption  := 'Spusteno : '+IntToStr(Self.OpenAC.stat_run);
  Self.L_AutRez_stDokonceno.Caption := 'Dokonceno : '+IntToStr(Self.OpenAC.stat_end);

  if ((Self.OpenAC.stat_run = 0) and (Self.OpenAC.stat_end = 0)) then
    Self.B_AutRez_StDelete.Enabled := false else Self.B_AutRez_StDelete.Enabled := true;

  Self.E_Soubor.Text                := Self.OpenAC.krk_filename;

  GB_AutRez_Statistika.Visible  := true;
  F_AutRezEdit.Caption := Self.OpenAC.name;
 end;//procedure

procedure TF_AutRezEdit.NewOpenForm;
 begin
  GB_AutRez_Statistika.Visible  := false;
  E_Soubor.Text                 := '';
  F_AutRezEdit.Caption := 'Nový automatický režim';
 end;//procedure

procedure TF_AutRezEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
 begin
  OpenIndex := -1;
  Self.OpenAC := nil;
  NewAutRez := false;
 end;//procedure

end.//unit
