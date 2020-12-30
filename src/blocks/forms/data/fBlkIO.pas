unit fBlkIO;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, fMain, BlockIO, Generics.Collections,
  IBUtils;

type
  TF_BlkIO = class(TForm)
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_IR02: TLabel;
    L_IR01: TLabel;
    GB_RCS_Input: TGroupBox;
    L_IR04: TLabel;
    L_IR05: TLabel;
    SE_RCS_Input_Port: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    SE_RCS_Input_Module: TSpinEdit;
    CHB_RCS_Input: TCheckBox;
    CHB_Activate_On_Start: TCheckBox;
    CHB_Nullable: TCheckBox;
    SE_Null_Time: TSpinEdit;
    GB_RCS_Output: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SE_RCS_Output_Port: TSpinEdit;
    SE_RCS_Output_Module: TSpinEdit;
    CHB_RCS_Output: TCheckBox;
    CHB_RCS_Output_Needed: TCheckBox;
    CHB_RCS_Input_Needed: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RCS_Input_ModuleExit(Sender: TObject);
    procedure SE_RCS_Output_ModuleExit(Sender: TObject);
    procedure CHB_RCS_InputClick(Sender: TObject);
    procedure CHB_RCS_OutputClick(Sender: TObject);
    procedure CHB_NullableClick(Sender: TObject);
  private
   NewBlk: Boolean;
   Blk: TBlkIO;

  public
   OpenIndex: Integer;

    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure HlavniOpenForm();
    procedure NewBlkCreate();
  end;

var
  F_BlkIO: TF_BlkIO;

implementation

uses GetSystems, FileSystem, TechnologieRCS, BlockDb, Block, DataBloky;

{$R *.dfm}

procedure TF_BlkIO.OpenForm(BlokIndex: Integer);
 begin
  OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  HlavniOpenForm();

  if (NewBlk) then
   begin
    NewBlkOpenForm();
   end else begin
    NormalOpenForm();
   end;
  Self.ShowModal();
 end;

procedure TF_BlkIO.SE_RCS_Input_ModuleExit(Sender: TObject);
begin
 Self.SE_RCS_Input_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCS_Input_Module.Value, Self.SE_RCS_Input_Port.Value);
end;

procedure TF_BlkIO.SE_RCS_Output_ModuleExit(Sender: TObject);
begin
 Self.SE_RCS_Output_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCS_Output_Module.Value, Self.SE_RCS_Output_Port.Value);
end;

procedure TF_BlkIO.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;

  Self.SE_RCS_Input_Module.Value := 1;
  Self.SE_RCS_Input_Port.Value := 0;
  Self.SE_RCS_Input_ModuleExit(Self);
  Self.CHB_RCS_Input.Checked := false;
  Self.CHB_RCS_InputClick(Self);
  Self.CHB_RCS_Input_Needed.Checked := true;

  Self.SE_RCS_Output_Module.Value := 1;
  Self.SE_RCS_Output_Port.Value := 0;
  Self.SE_RCS_Output_ModuleExit(Self);
  Self.CHB_RCS_Output.Checked := false;
  Self.CHB_RCS_OutputClick(Self);
  Self.CHB_RCS_Output_Needed.Checked := true;

  Self.CHB_Activate_On_Start.Checked := false;
  Self.CHB_Nullable.Checked := false;
  Self.CHB_NullableClick(Self);
  Self.SE_Null_Time.Value := 0;

  Self.Caption := 'Nový blok Výstup';
  Self.ActiveControl := Self.E_Nazev;
 end;

procedure TF_BlkIO.NormalOpenForm();
var glob: TBlkSettings;
    settings: TBlkIOsettings;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  Self.CHB_RCS_Input.Checked := settings.isRCSinput;
  Self.CHB_RCS_InputClick(Self);
  if (settings.isRCSinput) then
   begin
    if (settings.RCSinput.board > Cardinal(Self.SE_RCS_Input_Module.MaxValue)) then
      Self.SE_RCS_Input_Module.MaxValue := 0;
    Self.SE_RCS_Input_Port.MaxValue := 0;
    Self.SE_RCS_Input_Module.Value := settings.RCSinput.board;
    Self.SE_RCS_Input_Port.Value := settings.RCSinput.port;
    Self.CHB_RCS_Input_Needed.Checked := settings.RCSinputNeeded;
   end else begin
    Self.SE_RCS_Input_Module.Value := 0;
    Self.SE_RCS_Input_Port.Value := 0;
    Self.CHB_RCS_Input_Needed.Checked := false;
   end;
  Self.SE_RCS_Input_ModuleExit(Self);

  Self.CHB_RCS_Output.Checked := settings.isRCSOutput;
  Self.CHB_RCS_OutputClick(Self);
  if (settings.isRCSOutput) then
   begin
    if (settings.RCSOutput.board > Cardinal(Self.SE_RCS_Output_Module.MaxValue)) then
      Self.SE_RCS_Output_Module.MaxValue := 0;
    Self.SE_RCS_Output_Port.MaxValue := 0;
    Self.SE_RCS_Output_Module.Value := settings.RCSOutput.board;
    Self.SE_RCS_Output_Port.Value := settings.RCSOutput.port;
    Self.CHB_RCS_Output_Needed.Checked := settings.RCSoutputNeeded;
   end else begin
    Self.SE_RCS_Output_Module.Value := 0;
    Self.SE_RCS_Output_Port.Value := 0;
    Self.CHB_RCS_Output_Needed.Checked := false;
   end;
  Self.SE_RCS_Output_ModuleExit(Self);


  Self.E_Nazev.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.CHB_Activate_On_Start.Checked := settings.setOutputOnStart;
  Self.CHB_Nullable.Checked := Self.Blk.nullable;
  Self.SE_Null_Time.Value := settings.nullAfterSec;
  Self.CHB_NullableClick(Self);

  Self.Caption := 'Upravit blok '+glob.name+' (IO)';
  Self.ActiveControl := Self.B_Save;
 end;

procedure TF_BlkIO.HlavniOpenForm();
 begin
  Self.SE_RCS_Input_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCS_Output_Module.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkIO.NewBlkCreate();
 begin
  NewBlk := true;
  Self.OpenForm(Blocks.count);
 end;

procedure TF_BlkIO.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkIO.CHB_NullableClick(Sender: TObject);
begin
 Self.SE_Null_Time.Enabled := Self.CHB_Nullable.Checked;
end;

procedure TF_BlkIO.CHB_RCS_InputClick(Sender: TObject);
begin
 Self.CHB_RCS_Input_Needed.Enabled := Self.CHB_RCS_Input.Checked;
 Self.SE_RCS_Input_Module.Enabled := Self.CHB_RCS_Input.Checked;
 Self.SE_RCS_Input_Port.Enabled := Self.CHB_RCS_Input.Checked;
end;

procedure TF_BlkIO.CHB_RCS_OutputClick(Sender: TObject);
begin
 Self.CHB_RCS_Output_Needed.Enabled := Self.CHB_RCS_Output.Checked;
 Self.SE_RCS_Output_Module.Enabled := Self.CHB_RCS_Output.Checked;
 Self.SE_RCS_Output_Port.Enabled := Self.CHB_RCS_Output.Checked;
end;

procedure TF_BlkIO.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkIOsettings;
    another: TBlk;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
   end;

  if (Self.CHB_RCS_Input.Checked) then
   begin
    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCS_Input_Module.Value, Self.SE_RCS_Input_Port.Value),
                                        Self.Blk, TRCSIOType.input);
    if (another <> nil) then
     begin
      if (Application.MessageBox(PChar('RCS adresa vstupu se již používá na bloku '+another.name+', chcete pokračovat?'),
                                 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
     end;
   end;

  if (Self.CHB_RCS_Output.Checked) then
   begin
    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCS_Output_Module.Value, Self.SE_RCS_Output_Port.Value),
                                        Self.Blk, TRCSIOType.output);
    if (another <> nil) then
     begin
      if (Application.MessageBox(PChar('RCS adresa výstupu se již používá na bloku '+another.name+', chcete pokračovat?'),
                                 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
     end;
   end;

  glob.name := Self.E_Nazev.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btIO;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blocks.Add(glob) as TBlkIO;
    except
      on E: Exception do
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

  settings.isRCSoutput := Self.CHB_RCS_Output.Checked;
  if (Self.CHB_RCS_Output.Checked) then
   begin
    settings.RCSoutput := TRCS.RCSAddr(Self.SE_RCS_Output_Module.Value, Self.SE_RCS_Output_Port.Value);
    settings.RCSoutputNeeded := Self.CHB_RCS_Output_Needed.Checked;
   end;

  settings.isRCSinput := Self.CHB_RCS_Input.Checked;
  if (Self.CHB_RCS_Input.Checked) then
   begin
    settings.RCSInput := TRCS.RCSAddr(Self.SE_RCS_Input_Module.Value, Self.SE_RCS_Input_Port.Value);
    settings.RCSinputNeeded := Self.CHB_RCS_Input_Needed.Checked;
   end;

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
  Self.NewBlk := false;
  Self.OpenIndex := -1;
  BlokyTableData.UpdateTable();
 end;

end.//unit
