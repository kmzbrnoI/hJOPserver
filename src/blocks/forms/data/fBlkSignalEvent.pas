﻿unit fBlkSignalEvent;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin,
  BlockSignal, AreaDb, BlockDb, ExtCtrls, frrEv, BlockTrack;

type
  TF_BlkSignalEvent = class(TForm)
    GB_DetekceZastaveni: TGroupBox;
    GB_DetekceZpomalenii: TGroupBox;
    CHB_Zpomalit: TCheckBox;
    Label1: TLabel;
    E_Spr: TEdit;
    Label2: TLabel;
    SE_MinLength: TSpinEdit;
    Label3: TLabel;
    SE_MaxLength: TSpinEdit;
    P_ZpomForm: TPanel;
    P_ZastForm: TPanel;
    SE_Slow_Speed: TSpinEdit;
    procedure CHB_ZpomalitClick(Sender: TObject);
  private
    first: Boolean;

    fStop: TF_RREv;
    fSlow: TF_RREv;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure OpenForm(event: TBlkSignalTrainEvent; first: Boolean; parentTrack: TBlkTrack);
    procedure OpenEmptyForm(first: Boolean);
    function Check(): string;

    function GetEvent(): TBlkSignalTrainEvent; // returns new object!

  end;

var
  F_BlkSignalEvent: TF_BlkSignalEvent;

implementation

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

constructor TF_BlkSignalEvent.Create(AOwner: TComponent);
begin
  inherited;

  Self.fStop := TF_RREv.Create(nil);
  Self.fStop.Parent := Self.P_ZastForm;
  Self.fStop.trackEnabled := false;
  Self.fStop.Show();

  Self.fSlow := TF_RREv.Create(nil);
  Self.fSlow.Parent := Self.P_ZpomForm;
  Self.fSlow.trackEnabled := true;
  Self.fSlow.Show();
end;

destructor TF_BlkSignalEvent.Destroy();
begin
  Self.fStop.Free();
  Self.fSlow.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignalEvent.OpenForm(event: TBlkSignalTrainEvent; first: Boolean; parentTrack: TBlkTrack);
begin
  Self.first := first;

  if (first) then
  begin
    Self.E_Spr.Enabled := false;
    Self.SE_MinLength.Value := -1;
    Self.SE_MaxLength.Value := -1;
    Self.SE_MinLength.Enabled := false;
    Self.SE_MaxLength.Enabled := false;
    Self.E_Spr.Text := '.*';
  end else begin
    Self.E_Spr.Enabled := true;
    Self.SE_MinLength.Value := event.length.min;
    Self.SE_MaxLength.Value := event.length.max;
    Self.SE_MinLength.Enabled := true;
    Self.SE_MaxLength.Enabled := true;
    Self.E_Spr.Text := Copy(event.train_type_re, 2, length(event.train_type_re) - 2);
  end;

  Self.fStop.FillFromRR(event.stop, parentTrack);

  if (event.slow.Enabled) then
  begin
    Self.fSlow.FillFromRR(event.slow.ev, parentTrack);
    Self.SE_Slow_Speed.Value := event.slow.speed;
  end else begin
    Self.fSlow.ShowEmpty();
    Self.SE_Slow_Speed.Value := 0;
  end;

  Self.CHB_Zpomalit.Checked := event.slow.Enabled;
  Self.CHB_ZpomalitClick(CHB_Zpomalit);
end;

procedure TF_BlkSignalEvent.OpenEmptyForm(first: Boolean);
begin
  Self.first := first;

  if (first) then
  begin
    Self.E_Spr.Enabled := false;
    Self.SE_MinLength.Value := -1;
    Self.SE_MaxLength.Value := -1;
    Self.SE_MinLength.Enabled := false;
    Self.SE_MaxLength.Enabled := false;
  end else begin
    Self.E_Spr.Enabled := true;
    Self.SE_MinLength.Value := 0;
    Self.SE_MaxLength.Value := 100;
    Self.SE_MinLength.Enabled := true;
    Self.SE_MaxLength.Enabled := true;
    Self.E_Spr.Text := '';
  end;

  Self.E_Spr.Text := '.*';
  Self.SE_Slow_Speed.Value := 0;

  Self.fStop.ShowEmpty();
  Self.fSlow.ShowEmpty();

  Self.CHB_Zpomalit.Checked := false;
  Self.CHB_ZpomalitClick(CHB_Zpomalit);
end;
/// ////////////////////////////////////////////////////////////////////////////

function TF_BlkSignalEvent.GetEvent(): TBlkSignalTrainEvent;
begin
  Result := TBlkSignalTrainEvent.Create();
  if ((not Self.first) and (Self.E_Spr.Text <> '')) then
    Result.train_type_re := '^' + Self.E_Spr.Text + '$'
  else
    Result.train_type_re := '^.*$';
  Result.length.min := Self.SE_MinLength.Value;
  Result.length.max := Self.SE_MaxLength.Value;

  Result.stop := fStop.GetRREv();
  Result.slow.Enabled := Self.CHB_Zpomalit.Checked;

  if (Self.CHB_Zpomalit.Checked) then
  begin
    Result.slow.ev := fSlow.GetRREv();
    Result.slow.speed := Self.SE_Slow_Speed.Value;
  end else begin
    Result.slow.ev := nil;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignalEvent.CHB_ZpomalitClick(Sender: TObject);
begin
  Self.fSlow.Enabled := Self.CHB_Zpomalit.Checked;
  Self.SE_Slow_Speed.Enabled := Self.CHB_Zpomalit.Checked;
  if (not Self.CHB_Zpomalit.Checked) then
    Self.SE_Slow_Speed.Value := 0;
end;

function TF_BlkSignalEvent.Check(): string;
begin
  if (not Self.fStop.InputValid()) then
    Exit('Vyberte zastavovací událost!');

  if ((Self.CHB_Zpomalit.Checked) and (not Self.fSlow.InputValid())) then
    Exit('Vyberte zpomalovací událost!');

  if ((Self.SE_Slow_Speed.Value = 0) and (Self.CHB_Zpomalit.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit!');

  Result := '';
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
