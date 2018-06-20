unit fBlkSComEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, TBlokSCom, TBlok, TOblRizeni, TOblsRizeni,
  TBloky, ExtCtrls, frrEv;

type
  TF_BlkSComEvent = class(TForm)
    GB_DetekceZastaveni: TGroupBox;
    GB_DetekceZpomalenii: TGroupBox;
    CB_ZpomalitKmH: TComboBox;
    CHB_Zpomalit: TCheckBox;
    Label1: TLabel;
    E_Spr: TEdit;
    Label2: TLabel;
    SE_MinLength: TSpinEdit;
    Label3: TLabel;
    SE_MaxLength: TSpinEdit;
    P_ZpomForm: TPanel;
    P_ZastForm: TPanel;
    procedure CHB_ZpomalitClick(Sender: TObject);
  private
   obls:TArstr;
   first:boolean;

   fZast: TF_RREv;
   fZpom: TF_RREv;

    function GetEvent():TBlkSComSprEvent;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy(); override;

    procedure OpenForm(event:TBlkSComSprEvent; first:boolean; obls:TArstr);
    procedure OpenEmptyForm(first:boolean; obls:TArstr);
    function Check():string;

    property event:TBlkSComSprEvent read GetEvent;
  end;

var
  F_BlkSComEvent: TF_BlkSComEvent;

implementation

uses TBlokUsek;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

constructor TF_BlkSComEvent.Create(AOwner:TComponent);
begin
 inherited;

 Self.fZast := TF_RREv.Create(nil);
 Self.fZast.Parent := Self.P_ZastForm;
 Self.fZast.Show();

 Self.fZpom := TF_RREv.Create(nil);
 Self.fZpom.Parent := Self.P_ZpomForm;
 Self.fZpom.Show();
end;

destructor TF_BlkSComEvent.Destroy();
begin
 Self.fZast.Free();
 Self.fZpom.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSComEvent.OpenForm(event:TBlkSComSprEvent; first:boolean; obls:TArstr);
var i:Integer;
begin
 Self.obls  := obls;
 Self.first := first;

 if (first) then
  begin
   Self.E_Spr.Enabled        := false;
   Self.E_Spr.Text           := 'globální událost';
   Self.SE_MinLength.Value   := -1;
   Self.SE_MaxLength.Value   := -1;
   Self.SE_MinLength.Enabled := false;
   Self.SE_MaxLength.Enabled := false;
  end else begin
   Self.E_Spr.Enabled        := true;
   Self.E_Spr.Text           := '';
   Self.SE_MinLength.Value   := event.delka.min;
   Self.SE_MaxLength.Value   := event.delka.max;
   Self.SE_MinLength.Enabled := true;
   Self.SE_MaxLength.Enabled := true;

   for i := 0 to event.spr_typ.Count-1 do
    Self.E_Spr.Text := Self.E_Spr.Text + event.spr_typ[i] + ',';
  end;

 Self.fZast.FillFromRR(event.zastaveni);

 if (event.zpomaleni.enabled) then
  begin
   Self.fZpom.FillFromRR(event.zpomaleni.ev);
   Self.CB_ZpomalitKmH.ItemIndex := (event.zpomaleni.speed - 1) div 10;
  end else begin
   Self.fZpom.ShowEmpty();
   Self.CB_ZpomalitKmH.ItemIndex := -1;
  end;

 Self.CHB_Zpomalit.Checked := event.zpomaleni.enabled;
 Self.CHB_ZpomalitClick(CHB_Zpomalit);
end;

procedure TF_BlkSComEvent.OpenEmptyForm(first:boolean; obls:TArstr);
begin
 Self.obls  := obls;
 Self.first := first;

 if (first) then
  begin
   Self.E_Spr.Enabled        := false;
   Self.E_Spr.Text           := 'globální událost';
   Self.SE_MinLength.Value   := -1;
   Self.SE_MaxLength.Value   := -1;
   Self.SE_MinLength.Enabled := false;
   Self.SE_MaxLength.Enabled := false;
  end else begin
   Self.E_Spr.Enabled        := true;
   Self.E_Spr.Text           := '';
   Self.SE_MinLength.Value   := 0;
   Self.SE_MaxLength.Value   := 100;
   Self.SE_MinLength.Enabled := true;
   Self.SE_MaxLength.Enabled := true;
   Self.E_Spr.Text           := '';
  end;

 Self.CB_ZpomalitKmH.ItemIndex := -1;

 Self.fZast.ShowEmpty();
 Self.fZpom.ShowEmpty();

 Self.CHB_Zpomalit.Checked := false;
 Self.CHB_ZpomalitClick(CHB_Zpomalit);
end;
///////////////////////////////////////////////////////////////////////////////

function TF_BlkSComEvent.GetEvent():TBlkSComSprEvent;
begin
 Result.spr_typ := TStringList.Create();
 if (not Self.first) then
   ExtractStrings([','], [' '], PChar(Self.E_Spr.Text), Result.spr_typ);
 Result.delka.min := Self.SE_MinLength.Value;
 Result.delka.max := Self.SE_MaxLength.Value;

 Result.zastaveni := fZast.GetRREv();
 Result.zpomaleni.enabled := Self.CHB_Zpomalit.Checked;

 if (Self.CHB_Zpomalit.Checked) then
  begin
   Result.zpomaleni.ev := fZpom.GetRREv();
   Result.zpomaleni.speed := (Self.CB_ZpomalitKmH.ItemIndex+1) * 10;
  end else begin
   Result.zpomaleni.ev := nil;
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSComEvent.CHB_ZpomalitClick(Sender: TObject);
begin
 Self.fZpom.Enabled := Self.CHB_Zpomalit.Checked;
 Self.CB_ZpomalitKmH.Enabled := Self.CHB_Zpomalit.Checked;
 if (not Self.CHB_Zpomalit.Checked) then
   Self.CB_ZpomalitKmH.ItemIndex := -1;
end;

function TF_BlkSComEvent.Check():string;
begin
  if (not Self.fZast.InputValid()) then
    Exit('Vyberte zastavovací událost!');

  if ((Self.CHB_Zpomalit.Checked) and (not Self.fZpom.InputValid())) then
    Exit('Vyberte zpomalovací událost!');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomalit.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit!');

 Result := '';
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
