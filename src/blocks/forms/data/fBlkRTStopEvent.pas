unit fBlkRTStopEvent;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, BlockRailwayTrack, AreaDb, BlockDb, frrEv;

type
  TF_BlkRTStopEvent = class(TForm)
    GB_DetekceZastaveni: TGroupBox;
    GB_DetekceZpomalenii: TGroupBox;
    CB_ZpomalitKmH: TComboBox;
    CHB_Zpomal: TCheckBox;
    P_Stop: TPanel;
    P_Zpom: TPanel;
    procedure CHB_ZpomalClick(Sender: TObject);
  private
    fZast: TF_RREv;
    fZpom: TF_RREv;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure OpenForm(events: TBlkRTStopEvents);
    procedure OpenEmptyForm();
    function GetEvent(): TBlkRTStopEvents;
    function Check(): string;

  end;

var
  F_BlkRTStopEvent: TF_BlkRTStopEvent;

implementation

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

constructor TF_BlkRTStopEvent.Create(AOwner: TComponent);
begin
  inherited;

  Self.fZast := TF_RREv.Create(nil);
  Self.fZast.Parent := Self.P_Stop;
  Self.fZast.Show();

  Self.fZpom := TF_RREv.Create(nil);
  Self.fZpom.Parent := Self.P_Zpom;
  Self.fZpom.Show();
end;

destructor TF_BlkRTStopEvent.Destroy();
begin
  Self.fZast.Free();
  Self.fZpom.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkRTStopEvent.OpenForm(events: TBlkRTStopEvents);
begin
  Self.fZast.FillFromRR(events.stop);

  Self.CHB_Zpomal.Checked := events.slow.enabled;
  if (events.slow.enabled) then
  begin
    Self.CB_ZpomalitKmH.ItemIndex := (events.slow.speed - 1) div 10;
    Self.fZpom.FillFromRR(events.slow.ev);
  end else begin
    Self.fZpom.ShowEmpty();
  end;

  Self.CHB_ZpomalClick(Self.CHB_Zpomal);
end;

procedure TF_BlkRTStopEvent.OpenEmptyForm();
begin
  Self.fZast.ShowEmpty();
  Self.CB_ZpomalitKmH.ItemIndex := -1;
  Self.CHB_Zpomal.Checked := false;
  Self.CHB_ZpomalClick(Self.CHB_Zpomal);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_BlkRTStopEvent.Check(): string;
begin
  if (not Self.fZast.InputValid()) then
    Exit('Vyberte zastavovací událost!');

  if ((Self.CHB_Zpomal.Checked) and (not Self.fZpom.InputValid())) then
    Exit('Vyberte zpomalovací událost!');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomal.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit!');

  Result := '';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkRTStopEvent.CHB_ZpomalClick(Sender: TObject);
begin
  Self.fZpom.enabled := Self.CHB_Zpomal.Checked;
  Self.CB_ZpomalitKmH.enabled := Self.CHB_Zpomal.Checked;
  if (not Self.CHB_Zpomal.Checked) then
    Self.CB_ZpomalitKmH.ItemIndex := -1;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_BlkRTStopEvent.GetEvent(): TBlkRTStopEvents;
begin
  Result := TBlkRTStopEvents.Create();
  Result.stop := fZast.GetRREv();
  Result.slow.enabled := Self.CHB_Zpomal.Checked;

  if (Self.CHB_Zpomal.Checked) then
  begin
    Result.slow.ev := fZpom.GetRREv();
    Result.slow.speed := (Self.CB_ZpomalitKmH.ItemIndex + 1) * 10;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
