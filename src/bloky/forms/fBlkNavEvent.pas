unit fBlkNavEvent;

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin,
  TBlokNav, TOblsRizeni, TBloky, ExtCtrls, frrEv;

type
  TF_BlkNavEvent = class(TForm)
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

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy(); override;

    procedure OpenForm(event:TBlkNavTrainEvent; first:boolean; obls:TArstr);
    procedure OpenEmptyForm(first:boolean; obls:TArstr);
    function Check():string;

    function GetEvent():TBlkNavTrainEvent; // returns new object!

  end;

var
  F_BlkNavEvent: TF_BlkNavEvent;

implementation



{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

constructor TF_BlkNavEvent.Create(AOwner:TComponent);
begin
 inherited;

 Self.fZast := TF_RREv.Create(nil);
 Self.fZast.Parent := Self.P_ZastForm;
 Self.fZast.Show();

 Self.fZpom := TF_RREv.Create(nil);
 Self.fZpom.Parent := Self.P_ZpomForm;
 Self.fZpom.Show();
end;

destructor TF_BlkNavEvent.Destroy();
begin
 Self.fZast.Free();
 Self.fZpom.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkNavEvent.OpenForm(event:TBlkNavTrainEvent; first:boolean; obls:TArstr);
begin
 Self.obls  := obls;
 Self.first := first;

 if (first) then
  begin
   Self.E_Spr.Enabled        := false;
   Self.SE_MinLength.Value   := -1;
   Self.SE_MaxLength.Value   := -1;
   Self.SE_MinLength.Enabled := false;
   Self.SE_MaxLength.Enabled := false;
   Self.E_Spr.Text := '.*';
  end else begin
   Self.E_Spr.Enabled        := true;
   Self.SE_MinLength.Value   := event.delka.min;
   Self.SE_MaxLength.Value   := event.delka.max;
   Self.SE_MinLength.Enabled := true;
   Self.SE_MaxLength.Enabled := true;
   Self.E_Spr.Text := Copy(event.train_typ_re.Pattern, 2, Length(event.train_typ_re.Pattern)-2);
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

procedure TF_BlkNavEvent.OpenEmptyForm(first:boolean; obls:TArstr);
begin
 Self.obls  := obls;
 Self.first := first;

 if (first) then
  begin
   Self.E_Spr.Enabled        := false;
   Self.SE_MinLength.Value   := -1;
   Self.SE_MaxLength.Value   := -1;
   Self.SE_MinLength.Enabled := false;
   Self.SE_MaxLength.Enabled := false;
  end else begin
   Self.E_Spr.Enabled        := true;
   Self.SE_MinLength.Value   := 0;
   Self.SE_MaxLength.Value   := 100;
   Self.SE_MinLength.Enabled := true;
   Self.SE_MaxLength.Enabled := true;
   Self.E_Spr.Text           := '';
  end;

 Self.E_Spr.Text := '.*';
 Self.CB_ZpomalitKmH.ItemIndex := -1;

 Self.fZast.ShowEmpty();
 Self.fZpom.ShowEmpty();

 Self.CHB_Zpomalit.Checked := false;
 Self.CHB_ZpomalitClick(CHB_Zpomalit);
end;
///////////////////////////////////////////////////////////////////////////////

function TF_BlkNavEvent.GetEvent():TBlkNavTrainEvent;
begin
 Result := TBlkNavTrainEvent.Create();
 if ((not Self.first) and (Self.E_Spr.Text <> '')) then
   Result.train_typ_re.Compile('^'+Self.E_Spr.Text+'$', false);
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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkNavEvent.CHB_ZpomalitClick(Sender: TObject);
begin
 Self.fZpom.Enabled := Self.CHB_Zpomalit.Checked;
 Self.CB_ZpomalitKmH.Enabled := Self.CHB_Zpomalit.Checked;
 if (not Self.CHB_Zpomalit.Checked) then
   Self.CB_ZpomalitKmH.ItemIndex := -1;
end;

function TF_BlkNavEvent.Check():string;
begin
  if (not Self.fZast.InputValid()) then
    Exit('Vyberte zastavovací událost!');

  if ((Self.CHB_Zpomalit.Checked) and (not Self.fZpom.InputValid())) then
    Exit('Vyberte zpomalovací událost!');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomalit.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit!');

 Result := '';
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
