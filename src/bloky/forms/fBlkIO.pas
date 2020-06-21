unit fBlkIO;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, fMain, TBlokIO, Generics.Collections,
  IBUtils;

type
  TF_BlkIO = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    GB_RCS: TGroupBox;
    L_IR04: TLabel;
    L_IR05: TLabel;
    SE_port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_module: TSpinEdit;
    CHB_RCS_Enabled: TCheckBox;
    CHB_Activate_On_Start: TCheckBox;
    CHB_Nullable: TCheckBox;
    SE_Null_Time: TSpinEdit;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_moduleExit(Sender: TObject);
    procedure CHB_RCS_EnabledClick(Sender: TObject);
    procedure CHB_NullableClick(Sender: TObject);
  private
   NewBlk: Boolean;
   Blk: TBlkIO;

  public
   OpenIndex: Integer;

    procedure OpenForm(BlokIndex:Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure HlavniOpenForm();
    procedure NewBlkCreate();
  end;

var
  F_BlkIO: TF_BlkIO;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBloky, TBlok, DataBloky;

{$R *.dfm}

procedure TF_BlkIO.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm();

  if (NewBlk) then
   begin
    NewBlkOpenForm();
   end else begin
    NormalOpenForm();
   end;
  Self.ShowModal();
 end;

procedure TF_BlkIO.SE_moduleExit(Sender: TObject);
begin
 Self.SE_port.MaxValue := TBlky.SEPortMaxValue(Self.SE_module.Value, Self.SE_port.Value);
end;

procedure TF_BlkIO.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blky.GetBlkID(Blky.count-1)+1;
  Self.SE_module.Value  := 1;
  Self.SE_port.Value := 0;
  Self.SE_moduleExit(Self);
  Self.CHB_RCS_Enabled.Checked := false;
  Self.CHB_RCS_EnabledClick(Self);
  Self.CHB_Activate_On_Start.Checked := false;
  Self.CHB_Nullable.Checked := false;
  Self.CHB_NullableClick(Self);
  Self.SE_Null_Time.Value := 0;

  Self.Caption := 'Nový blok Výstup';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkIO.NormalOpenForm();
var glob:TBlkSettings;
    settings:TBlkIOsettings;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  if (settings.RCSAddrs.Count > 0) then
   begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_module.MaxValue)) then
      Self.SE_module.MaxValue := 0;
    Self.SE_port.MaxValue := 0;
    Self.SE_module.Value := settings.RCSAddrs[0].board;
    Self.SE_port.Value   := settings.RCSAddrs[0].port;
   end else begin
    Self.SE_module.Value := 0;
    Self.SE_Port.Value := 0;
   end;

  Self.SE_moduleExit(Self);

  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.CHB_RCS_Enabled.Checked := (settings.RCSAddrs.Count > 0);
  Self.CHB_RCS_EnabledClick(Self);
  Self.CHB_Activate_On_Start.Checked := settings.setOutputOnStart;
  Self.CHB_Nullable.Checked := Self.Blk.nullable;
  Self.SE_Null_Time.Value := settings.nullAfterSec;
  Self.CHB_NullableClick(Self);

  Self.Caption := 'Upravit blok '+glob.name+' (IO)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkIO.HlavniOpenForm;
 begin
  Self.SE_module.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkIO.NewBlkCreate;
 begin
  NewBlk := true;
  OpenForm(Blky.count);
 end;

procedure TF_BlkIO.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkIO.CHB_NullableClick(Sender: TObject);
begin
 Self.SE_Null_Time.Enabled := Self.CHB_Nullable.Checked;
end;

procedure TF_BlkIO.CHB_RCS_EnabledClick(Sender: TObject);
begin
 Self.SE_module.Enabled := Self.CHB_RCS_Enabled.Checked;
 Self.SE_port.Enabled := Self.CHB_RCS_Enabled.Checked;
end;

procedure TF_BlkIO.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkIOsettings;
    another: TBlk;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blky.IsBlok(SE_ID.Value, OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  another := Blky.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_Port.Value), Self.Blk, TRCSIOType.output);
  if (another <> nil) then
   begin
    if (Application.MessageBox(PChar('RCS adresa se již používá na bloku '+another.name+', chcete pokračovat?'),
                               'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
      Exit();
   end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := _BLK_IO;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blky.Add(_BLK_IO, glob) as TBlkIO;
    except
      on E:Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  // ukladani dat

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_RCS_Enabled.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_module.Value, Self.SE_Port.Value));
  settings.setOutputOnStart := Self.CHB_Activate_On_Start.Checked;
  if (Self.CHB_Nullable.Checked) then
    settings.nullAfterSec := Self.SE_Null_Time.Value
  else
    settings.nullAfterSec := 0;

  Self.Blk.SetSettings(settings);
  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkIO.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  NewBlk := false;
  OpenIndex := -1;
  BlokyTableData.UpdateTable();
 end;

end.//unit
