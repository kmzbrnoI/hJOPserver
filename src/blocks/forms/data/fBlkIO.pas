unit fBlkIO;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, fMain, BlockIO, Generics.Collections;

type
  TF_BlkIO = class(TForm)
    E_Name: TEdit;
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
    CHB_AllowOutChange: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SE_RCS_Input_ModuleExit(Sender: TObject);
    procedure SE_RCS_Output_ModuleExit(Sender: TObject);
    procedure CHB_RCS_InputClick(Sender: TObject);
    procedure CHB_RCS_OutputClick(Sender: TObject);
    procedure CHB_NullableClick(Sender: TObject);
  private
    isNewBlock: Boolean;
    block: TBlkIO;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

  public

    procedure EditBlock(BlokIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkIO: TF_BlkIO;

implementation

uses GetSystems, TechnologieRCS, BlockDb, Block, DataBloky;

{$R *.dfm}

procedure TF_BlkIO.EditBlock(BlokIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := BlokIndex;
  Self.block := Blocks.GetBlkByIndex(BlokIndex) as TBlkIO;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(BlokIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkIO.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkIO.SE_RCS_Input_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Input_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_RCS_Input_Module.Value,
    Self.SE_RCS_Input_Port.Value);
end;

procedure TF_BlkIO.SE_RCS_Output_ModuleExit(Sender: TObject);
begin
  Self.SE_RCS_Output_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_RCS_Output_Module.Value,
    Self.SE_RCS_Output_Port.Value);
end;

procedure TF_BlkIO.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

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
  Self.CHB_AllowOutChange.Checked := false;

  Self.Caption := 'Nový blok Výstup';
end;

procedure TF_BlkIO.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkIOsettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

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

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.CHB_Activate_On_Start.Checked := settings.setOutputOnStart;
  Self.CHB_Nullable.Checked := Self.block.nullable;
  Self.SE_Null_Time.Value := settings.nullAfterSec;
  Self.CHB_NullableClick(Self);
  Self.CHB_AllowOutChange.Checked := settings.allowOutChange;

  Self.Caption := 'Upravit blok ' + glob.name + ' (IO)';
end;

procedure TF_BlkIO.CommonOpenForm();
begin
  Self.SE_RCS_Input_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCS_Output_Module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.ActiveControl := Self.E_Name;
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
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.CHB_RCS_Input.Checked) then
  begin
    var another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCS_Input_Module.Value, Self.SE_RCS_Input_Port.Value),
      Self.block, TRCSIOType.input);
    if (another <> nil) then
    begin
      if (Application.MessageBox(PChar('RCS adresa vstupu se již používá na bloku ' + another.name +
        ', chcete pokračovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;

  if (Self.CHB_RCS_Output.Checked) then
  begin
    var another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCS_Output_Module.Value, Self.SE_RCS_Output_Port.Value),
      Self.block, TRCSIOType.output);
    if (another <> nil) then
    begin
      if (Application.MessageBox(PChar('RCS adresa výstupu se již používá na bloku ' + another.name +
        ', chcete pokračovat?'), 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
    end;
  end;

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btIO;

  if (Self.isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkIO;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    try
      Self.block.SetGlobalSettings(glob);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se uložit blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end;


  var settings: TBlkIOsettings;
  settings.isRCSOutput := Self.CHB_RCS_Output.Checked;
  if (Self.CHB_RCS_Output.Checked) then
  begin
    settings.RCSOutput := TRCS.RCSAddr(Self.SE_RCS_Output_Module.Value, Self.SE_RCS_Output_Port.Value);
    settings.RCSoutputNeeded := Self.CHB_RCS_Output_Needed.Checked;
  end;

  settings.isRCSinput := Self.CHB_RCS_Input.Checked;
  if (Self.CHB_RCS_Input.Checked) then
  begin
    settings.RCSinput := TRCS.RCSAddr(Self.SE_RCS_Input_Module.Value, Self.SE_RCS_Input_Port.Value);
    settings.RCSinputNeeded := Self.CHB_RCS_Input_Needed.Checked;
  end;

  settings.setOutputOnStart := Self.CHB_Activate_On_Start.Checked;
  if (Self.CHB_Nullable.Checked) then
    settings.nullAfterSec := Self.SE_Null_Time.Value
  else
    settings.nullAfterSec := 0;

  settings.allowOutChange := Self.CHB_AllowOutChange.Checked;

  Self.block.SetSettings(settings);
  Self.Close();
  Self.block.Change();
end;

procedure TF_BlkIO.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
