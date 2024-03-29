﻿unit fBlkAC;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, BlockAC;

type
  TF_BlkAC = class(TForm)
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    Label4: TLabel;
    E_AccessToken: TEdit;
    B_GenToken: TButton;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_GenTokenClick(Sender: TObject);

  private
    isNewBlock: Boolean;
    block: TBlkAC;
    openIndex: Integer;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkAC: TF_BlkAC;

implementation

uses BlockDb, Block, DataBloky, ownStrUtils, ownGuiUtils;

{$R *.dfm}

procedure TF_BlkAC.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkAC;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkAC.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkAC.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.E_AccessToken.Text := '';

  Self.Caption := 'Nový blok AC';
end;

procedure TF_BlkAC.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkACSettings;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;
  Self.E_AccessToken.Text := settings.accessToken;

  Self.Caption := 'Upravit blok ' + glob.name + ' (AC)';
end;

procedure TF_BlkAC.CommonOpenForm();
begin
  Self.ActiveControl := Self.E_Name;
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
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  try
    var glob: TBlkSettings;
    glob.name := Self.E_Name.Text;
    glob.id := Self.SE_ID.Value;
    glob.typ := btAC;

    var settings: TBlkACSettings;
    settings.accessToken := Self.E_AccessToken.Text;

    if (Self.isNewBlock) then
    begin
      try
        Self.block := Blocks.Add(glob) as TBlkAC;
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se přidat blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end else begin
      try
        Self.block.SetGlobalSettings(glob);
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se uložit blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end;

    Self.block.SetSettings(settings);
  except
    on E: Exception do
    begin
      ExceptionMessageBox('Neočekávaná chyba.', E);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_BlkAC.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
