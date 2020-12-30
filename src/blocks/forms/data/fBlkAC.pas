unit fBlkAC;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockAC;

type
  TF_BlkAC = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    Label1: TLabel;
    E_AccessToken: TEdit;
    B_GenToken: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_GenTokenClick(Sender: TObject);

  private
   NewBlk: Boolean;
   Blk: TBlkAC;

  public
   OpenIndex: Integer;

   procedure OpenForm(BlokIndex: Integer);
   procedure NewBlkOpenForm();
   procedure NormalOpenForm();
   procedure HlavniOpenForm();
   procedure NewBlkCreate();
  end;

var
  F_BlkAC: TF_BlkAC;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky, TOblRizeni,
     ownStrUtils;

{$R *.dfm}

procedure TF_BlkAC.OpenForm(BlokIndex: Integer);
 begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm();

  if (NewBlk) then
    NewBlkOpenForm()
  else
    NormalOpenForm();

  Self.ShowModal();
 end;

procedure TF_BlkAC.NewBlkOpenForm();
 begin
  Self.E_Nazev.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  Self.E_AccessToken.Text := '';

  Self.Caption := 'Nový blok AC';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkAC.NormalOpenForm();
var glob: TBlkSettings;
    settings: TBlkACSettings;
    oblr: TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  for oblr in Self.Blk.stations do
    Self.LB_Stanice.Items.Add(oblr.Name);

  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.E_AccessToken.Text := settings.accessToken;

  Self.Caption := 'Upravit blok '+glob.name+' (AC)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkAC.HlavniOpenForm();
 begin
  Self.LB_Stanice.Clear();
 end;

procedure TF_BlkAC.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blocks.count);
 end;

procedure TF_BlkAC.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkAC.B_GenTokenClick(Sender: TObject);
begin
 Self.E_AccessToken.Text := RandomToken(32);
end;

procedure TF_BlkAC.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkACSettings;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku !', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btAC;
  settings.accessToken := Self.E_AccessToken.Text;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkAC;
    except
      on E: Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  Self.Blk.SetSettings(settings);
  Self.Close();
 end;

procedure TF_BlkAC.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable();
 end;

end.//unit
