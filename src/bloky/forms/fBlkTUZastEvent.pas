unit fBlkTUZastEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TBlokTratUsek, TOblsRizeni, TBloky, frrEv;

type
  TF_BlkTUZastEvent = class(TForm)
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
     constructor Create(AOwner:TComponent); override;
     destructor Destroy(); override;

     procedure OpenForm(events: TBlkTUZastEvents);
     procedure OpenEmptyForm();
     function GetEvent():TBlkTUZastEvents;
     function Check():string;

  end;

var
  F_BlkTUZastEvent: TF_BlkTUZastEvent;

implementation

uses TBlok, TBlokIR;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

constructor TF_BlkTUZastEvent.Create(AOwner:TComponent);
begin
 inherited;

 Self.fZast := TF_RREv.Create(nil);
 Self.fZast.Parent := Self.P_Stop;
 Self.fZast.Show();

 Self.fZpom := TF_RREv.Create(nil);
 Self.fZpom.Parent := Self.P_Zpom;
 Self.fZpom.Show();
end;

destructor TF_BlkTUZastEvent.Destroy();
begin
 Self.fZast.Free();
 Self.fZpom.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.OpenForm(events: TBlkTUZastEvents);
begin
 if (events.enabled) then
   Self.fZast.FillFromRR(events.zastaveni)
 else
   Self.fZast.ShowEmpty();

 Self.CHB_Zpomal.Checked := events.zpomaleni.enabled;
 if (events.zpomaleni.enabled) then
  begin
   Self.CB_ZpomalitKmH.ItemIndex := (events.zpomaleni.speed - 1) div 10;
   Self.fZpom.FillFromRR(events.zpomaleni.ev);
  end else begin
   Self.fZpom.ShowEmpty();
  end;

 Self.CHB_ZpomalClick(Self.CHB_Zpomal);
end;

procedure TF_BlkTUZastEvent.OpenEmptyForm();
begin
 Self.fZast.ShowEmpty();
 Self.CB_ZpomalitKmH.ItemIndex := -1;
 Self.CHB_Zpomal.Checked := false;
 Self.CHB_ZpomalClick(Self.CHB_Zpomal);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_BlkTUZastEvent.Check():string;
begin
  if (not Self.fZast.InputValid()) then
    Exit('Vyberte zastavovací událost!');

  if ((Self.CHB_Zpomal.Checked) and (not Self.fZpom.InputValid())) then
    Exit('Vyberte zpomalovací událost!');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomal.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit!');

 Result := '';
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.CHB_ZpomalClick(Sender: TObject);
begin
 Self.fZpom.Enabled := Self.CHB_Zpomal.Checked;
 Self.CB_ZpomalitKmH.Enabled := Self.CHB_Zpomal.Checked;
 if (not Self.CHB_Zpomal.Checked) then
   Self.CB_ZpomalitKmH.ItemIndex := -1;
end;

////////////////////////////////////////////////////////////////////////////////

function TF_BlkTUZastEvent.GetEvent():TBlkTUZastEvents;
begin
 Result.zastaveni := fZast.GetRREv();
 Result.zpomaleni.enabled := Self.CHB_Zpomal.Checked;

 if (Self.CHB_Zpomal.Checked) then
  begin
   Result.zpomaleni.ev := fZpom.GetRREv();
   Result.zpomaleni.speed := (Self.CB_ZpomalitKmH.ItemIndex+1) * 10;
  end else begin
   Result.zpomaleni.ev := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
