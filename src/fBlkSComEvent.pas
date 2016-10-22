unit fBlkSComEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, TBlokSCom, TBlok, TOblRizeni, TOblsRizeni,
  TBloky;

type
  TF_BlkSComEvent = class(TForm)
    GB_DetekceZastaveni: TGroupBox;
    GB_DetekceZpomalenii: TGroupBox;
    L_SCom14: TLabel;
    CB_ZpomalitKmH: TComboBox;
    CHB_Zpomalit: TCheckBox;
    Label1: TLabel;
    E_Spr: TEdit;
    Label2: TLabel;
    SE_MinLength: TSpinEdit;
    Label3: TLabel;
    SE_MaxLength: TSpinEdit;
    RB_ZastaveniUsek: TRadioButton;
    RB_ZastaveniIR: TRadioButton;
    CB_ZastavitIR: TComboBox;
    CB_ZastavitUsek: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    CB_ZpomalitIR: TComboBox;
    RB_ZpomaleniIR: TRadioButton;
    Label6: TLabel;
    CB_ZpomalitUsek: TComboBox;
    Label7: TLabel;
    RB_ZpomaleniUsek: TRadioButton;
    procedure RB_ZastaveniUsekClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RB_ZpomaleniUsekClick(Sender: TObject);
    procedure CHB_ZpomalitClick(Sender: TObject);
  private
   openevent:TBlkSComSprEvent;
   CB_IR:TArI;
   obls:TArstr;
   first:boolean;

    function GetEvent():TBlkSComSprEvent;

  public
    procedure OpenForm(event:TBlkSComSprEvent; first:boolean; obls:TArstr);
    function Check():string;

    property event:TBlkSComSprEvent read GetEvent;
  end;

var
  F_BlkSComEvent: TF_BlkSComEvent;

implementation

uses TBlokUsek;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSComEvent.OpenForm(event:TBlkSComSprEvent; first:boolean; obls:TArstr);
var i:Integer;
begin
 Self.openevent := event;
 Self.obls      := obls;
 Self.first     := first;

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
   Self.SE_MinLength.Value   := Self.openevent.delka.min;
   Self.SE_MaxLength.Value   := Self.openevent.delka.max;
   Self.SE_MinLength.Enabled := true;
   Self.SE_MaxLength.Enabled := true;

   for i := 0 to Self.openevent.spr_typ.Count-1 do
    Self.E_Spr.Text := Self.E_Spr.Text + Self.openevent.spr_typ[i] + ',';
  end;

 case (Self.openevent.zastaveni.signal) of
  TBlkSComSignal.usek : Self.RB_ZastaveniUsek.Checked := true;
  TBlkSComSignal.ir   : Self.RB_ZastaveniIR.Checked := true;
 else
  Self.RB_ZastaveniUsekClick(Self);
 end;

 if (Self.openevent.zpomaleni.signal > TBlkSCOmSignal.disabled) then
   Self.CHB_Zpomalit.Checked := true
 else
   Self.CHB_Zpomalit.Checked := false;
end;

procedure TF_BlkSComEvent.RB_ZastaveniUsekClick(Sender: TObject);
var i:Integer;
    Blk:TBlk;
    set2:TBlkUsekSettings;
 begin
  Self.CB_ZastavitUsek.Enabled   := false;
  Self.CB_ZastavitIR.Enabled     := false;
  Self.CB_ZastavitUsek.Clear();
  Self.CB_ZastavitIR.Clear();

  if (Self.RB_ZastaveniUsek.Checked) then
   begin
    Self.CB_ZastavitUsek.Enabled  := true;
    CB_ZastavitUsek.Clear;

    if (Blky.GetBlkByID(Self.openevent.zastaveni.usekid, Blk) <> 0) then
     begin
      CB_ZastavitUsek.Items.Add('1');
     end else begin
      set2 := (Blk as TBlkUsek).GetSettings();
      for i := 0 to set2.MTBAddrs.Count-1 do
        CB_ZastavitUsek.Items.Add(IntToStr(i)+': '+IntToStr(set2.MTBAddrs.data[i].board)+':'+IntToStr(set2.MTBAddrs.data[i].port));
     end;
    CB_ZastavitUsek.ItemIndex := Self.openevent.zastaveni.usekpart;
   end;

  if (Self.RB_ZastaveniIR.Checked) then
   begin
    Self.CB_ZastavitIR.Enabled := true;
    Blky.NactiBlokyDoObjektu(CB_ZastavitIR, @CB_IR, nil, Self.obls, _BLK_IR, openevent.zastaveni.irid);
   end;
end;

procedure TF_BlkSComEvent.RB_ZpomaleniUsekClick(Sender: TObject);
var i:Integer;
    Blk:TBlk;
    set2:TBlkUsekSettings;
 begin
  Self.CB_ZpomalitUsek.Enabled   := false;
  Self.CB_ZpomalitIR.Enabled     := false;
  Self.CB_ZpomalitUsek.Clear();
  Self.CB_ZpomalitIR.Clear();

  if (Self.RB_ZpomaleniUsek.Checked) then
   begin
    Self.CB_ZpomalitUsek.Enabled  := true;
    CB_ZpomalitUsek.Clear;

    if (Blky.GetBlkByID(Self.openevent.zpomaleni.usekid, Blk) <> 0) then
     begin
      CB_ZpomalitUsek.Items.Add('1');
     end else begin
      set2 := (Blk as TBlkUsek).GetSettings();
      for i := 0 to set2.MTBAddrs.Count-1 do
        CB_ZpomalitUsek.Items.Add(IntToStr(i)+': '+IntToStr(set2.MTBAddrs.data[i].board)+':'+IntToStr(set2.MTBAddrs.data[i].port));
     end;
    CB_ZpomalitUsek.ItemIndex := Self.openevent.zpomaleni.usekpart;
   end;

  if (Self.RB_ZpomaleniIR.Checked) then
   begin
    Self.CB_ZpomalitIR.Enabled := true;
    Blky.NactiBlokyDoObjektu(CB_ZpomalitIR, @CB_IR, nil, Self.obls, _BLK_IR, openevent.zpomaleni.irid);
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSComEvent.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (Assigned(Self.openevent.spr_typ)) then
  Self.openevent.spr_typ.Free();
end;

function TF_BlkSComEvent.GetEvent():TBlkSComSprEvent;
begin
 Result.spr_typ := TStringList.Create();
 if (not Self.first) then
   ExtractStrings([','], [' '], PChar(Self.E_Spr.Text), Result.spr_typ);
 Result.delka.min := Self.SE_MinLength.Value;
 Result.delka.max := Self.SE_MaxLength.Value;

 Result.zastaveni.signal := TBlkSComSignal.disabled;
 Result.zpomaleni.signal := TBlkSComSignal.disabled;
 Result.zastaveni.speed  := 0;

 if (Self.RB_ZastaveniUsek.Checked) then
  begin
   Result.zastaveni.signal   := TBlkSComSignal.usek;
   Result.zastaveni.usekpart := Self.CB_ZastavitUsek.ItemIndex;
  end;
 if (Self.RB_ZastaveniIR.Checked) then
  begin
   Result.zastaveni.signal   := TBlkSComSignal.IR;
   try
     Result.zastaveni.irid   := Blky.GetBlkID(Self.CB_IR[Self.CB_ZastavitIR.ItemIndex]);
   except
     Result.zastaveni.irid   := -1;
   end;
  end;

 if (Self.CHB_Zpomalit.Checked) then
  begin
   Result.zpomaleni.speed := (Self.CB_ZpomalitKmH.ItemIndex+1) * 10;

   if (Self.RB_ZpomaleniUsek.Checked) then
    begin
     Result.zpomaleni.signal   := TBlkSComSignal.usek;
     Result.zpomaleni.usekpart := Self.CB_ZpomalitUsek.ItemIndex;
    end;
    if (Self.RB_ZpomaleniIR.Checked) then
    begin
     Result.zpomaleni.signal   := TBlkSComSignal.IR;
     try
       Result.zpomaleni.irid   := Blky.GetBlkID(Self.CB_IR[Self.CB_ZpomalitIR.ItemIndex]);
     except
       Result.zpomaleni.irid   := -1;
     end;
    end;
  end;//if CHB_Zpomalit.Checked

end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSComEvent.CHB_ZpomalitClick(Sender: TObject);
begin
 if (Self.CHB_Zpomalit.Checked) then
  begin
   Self.CB_ZpomalitKmH.Enabled   := true;
   Self.CB_ZpomalitKmH.ItemIndex := (Self.openevent.zpomaleni.speed div 10) - 1;
   Self.RB_ZpomaleniIR.Enabled   := true;
   Self.RB_ZpomaleniUsek.Enabled := true;

   case (Self.openevent.zpomaleni.signal) of
    TBlkSComSignal.usek : Self.RB_ZpomaleniUsek.Checked := true;
    TBlkSComSignal.ir   : Self.RB_ZpomaleniIR.Checked := true;
   end;
  end else begin
   Self.CB_ZpomalitKmH.ItemIndex := -1;
   Self.CB_ZpomalitKmH.Enabled   := false;
   Self.RB_ZpomaleniIR.Checked   := false;
   Self.RB_ZpomaleniUsek.Checked := false;
   Self.RB_ZpomaleniIR.Enabled   := false;
   Self.RB_ZpomaleniUsek.Enabled := false;
   Self.RB_ZpomaleniUsekClick(Self);
  end;
end;

function TF_BlkSComEvent.Check():string;
begin
  if ((not Self.RB_ZastaveniUsek.Checked) and (not Self.RB_ZastaveniIR.Checked)) then
    Exit('Vyberte zastavovaci typ !');

  if (((not Self.RB_ZpomaleniUsek.Checked) and (not Self.RB_ZpomaleniIR.Checked)) and (CHB_Zpomalit.Checked)) then
    Exit('Vyberte zpomalovaci typ !');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomalit.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit !');

  if ((CB_ZpomalitIR.ItemIndex = -1) and (RB_ZpomaleniIR.Checked)) then
    Exit('Vyberte IR, na kterem se bude zpomalovat !');

  if ((CB_ZpomalitUsek.ItemIndex = -1) and (RB_ZpomaleniUsek.Checked)) then
    Exit('Vyberte cast useku, na kterem se bude zpomalovat!');

  if ((CB_ZastavitIR.ItemIndex = -1) and (RB_ZastaveniIR.Checked)) then
    Exit('Vyberte IR, na kterem se bude zastavovat !');

  if ((CB_ZastavitUsek.ItemIndex = -1) and (RB_ZastaveniUsek.Checked)) then
    Exit('Vyberte cast useku, na kterem se bude zastavovat!');

 Result := '';
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
