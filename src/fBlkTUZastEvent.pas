unit fBlkTUZastEvent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TBlokTratUsek, RPConst, TOblsRizeni;

type
  TF_BlkTUZastEvent = class(TForm)
    GB_DetekceZastaveni: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    RB_Zast_Usek: TRadioButton;
    RB_Zast_IR: TRadioButton;
    CB_Zast_IR: TComboBox;
    CB_Zast_UsekPart: TComboBox;
    GB_DetekceZpomalenii: TGroupBox;
    L_SCom14: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    CB_ZpomalitKmH: TComboBox;
    CHB_Zpomal: TCheckBox;
    CB_Zpom_IR: TComboBox;
    RB_Zpom_IR: TRadioButton;
    CB_Zpom_UsekPart: TComboBox;
    RB_Zpom_Usek: TRadioButton;
    Label1: TLabel;
    CB_Zast_Stav: TComboBox;
    Label2: TLabel;
    CB_Zpom_stav: TComboBox;
    procedure RB_Zast_UsekClick(Sender: TObject);
    procedure CHB_ZpomalClick(Sender: TObject);
    procedure RB_Zpom_UsekClick(Sender: TObject);
  private
   events:TBlkTUZastEvents;
   CB_IR:TArSmallI;

  public
     procedure OpenForm(events: TBlkTUZastEvents);
     function GetEvent():TBlkTUZastEvents;
     function Check():string;

  end;

var
  F_BlkTUZastEvent: TF_BlkTUZastEvent;

implementation

uses TBlok, TBlokIR, TBloky;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.OpenForm(events: TBlkTUZastEvents);
begin
 Self.events    := events;

 case (Self.events.zastaveni.signal) of
  TBlkTUSignal.usek : Self.RB_Zast_Usek.Checked := true;
  TBlkTUSignal.ir   : Self.RB_Zast_IR.Checked   := true;
 end;
 Self.RB_Zast_UsekClick(Self);
 if (events.zastaveni.stav) then
   Self.CB_Zast_Stav.ItemIndex := 1
 else
   Self.CB_Zast_Stav.ItemIndex := 0;

 if (Self.events.zpomaleni.signal > TBlkTUSignal.disabled) then
   Self.CHB_Zpomal.Checked := true
 else
   Self.CHB_Zpomal.Checked := false;
 Self.CHB_ZpomalClick(Self.CHB_Zpomal);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_BlkTUZastEvent.Check():string;
begin
  if ((not Self.RB_Zast_Usek.Checked) and (not Self.RB_Zast_IR.Checked)) then
    Exit('Vyberte zastavovaci typ !');

  if (((not Self.RB_Zpom_Usek.Checked) and (not Self.RB_Zpom_IR.Checked)) and (CHB_Zpomal.Checked)) then
    Exit('Vyberte zpomalovaci typ !');

  if ((CB_ZpomalitKmH.ItemIndex = -1) and (CHB_Zpomal.Checked)) then
    Exit('Vyberte rychlost, na kterou se ma zpomalit !');

  if ((CB_Zpom_IR.ItemIndex = -1) and (RB_Zpom_IR.Checked)) then
    Exit('Vyberte IR, na kterem se bude zpomalovat !');

  if ((CB_Zpom_UsekPart.ItemIndex = -1) and (RB_Zpom_Usek.Checked)) then
    Exit('Vyberte cast useku, na kterem se bude zpomalovat!');

  if ((CB_Zast_IR.ItemIndex = -1) and (RB_Zast_IR.Checked)) then
    Exit('Vyberte IR, na kterem se bude zastavovat !');

  if ((CB_Zast_UsekPart.ItemIndex = -1) and (RB_Zast_Usek.Checked)) then
    Exit('Vyberte cast useku, na kterem se bude zastavovat!');

  if ((CB_Zpom_stav.ItemIndex = -1) and (CHB_Zpomal.Checked)) then
    Exit('Vyberte stav, ktery spousti zpomalovaci udalost!');

  if (CB_Zast_stav.ItemIndex = -1) then
    Exit('Vyberte stav, ktery spousti zastavovaci udalost!');

 Result := '';
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.CHB_ZpomalClick(Sender: TObject);
begin
 if (Self.CHB_Zpomal.Checked) then
  begin
   Self.CB_ZpomalitKmH.Enabled   := true;
   Self.CB_ZpomalitKmH.ItemIndex := (Self.events.zpomaleni.speed div 10) - 1;
   Self.RB_Zpom_IR.Enabled       := true;
   Self.RB_Zpom_Usek.Enabled     := true;
   Self.CB_Zpom_Stav.Enabled     := true;

   case (Self.events.zpomaleni.signal) of
    TBlkTUSignal.usek : Self.RB_Zpom_Usek.Checked := true;
    TBlkTUSignal.ir   : Self.RB_Zpom_IR.Checked := true;
   end;
   Self.RB_Zpom_UsekClick(Self);

   if (events.zpomaleni.stav) then
     Self.CB_Zpom_Stav.ItemIndex := 1
   else
     Self.CB_Zpom_Stav.ItemIndex := 0;

  end else begin
   Self.CB_Zpom_Stav.Enabled     := false;
   Self.CB_Zpom_Stav.ItemIndex   := -1;
   Self.CB_ZpomalitKmH.ItemIndex := -1;
   Self.CB_ZpomalitKmH.Enabled   := false;
   Self.RB_Zpom_IR.Checked       := false;
   Self.RB_Zpom_Usek.Checked     := false;
   Self.RB_Zpom_IR.Enabled       := false;
   Self.RB_Zpom_Usek.Enabled     := false;
   Self.RB_Zpom_UsekClick(Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.RB_Zast_UsekClick(Sender: TObject);
 begin
  Self.CB_Zast_UsekPart.Enabled   := (Self.RB_Zast_Usek.Checked);
  Self.CB_Zast_IR.Enabled         := (Self.RB_Zast_IR.Checked);
  Self.CB_Zast_IR.ItemIndex       := -1;
  Self.CB_Zast_UsekPart.ItemIndex := -1;

  if (Self.RB_Zast_Usek.Checked) then
    CB_Zast_UsekPart.ItemIndex := Self.events.zastaveni.usekpart;

  if (Self.RB_Zast_IR.Checked) then
    Blky.NactiBlokyDoObjektu(CB_Zast_IR, @CB_IR, nil, nil, _BLK_IR, events.zastaveni.irid);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkTUZastEvent.RB_Zpom_UsekClick(Sender: TObject);
 begin
  Self.CB_Zpom_UsekPart.Enabled   := (Self.RB_Zpom_Usek.Checked);
  Self.CB_Zpom_IR.Enabled         := (Self.RB_Zpom_IR.Checked);
  Self.CB_Zpom_IR.ItemIndex       := -1;
  Self.CB_Zpom_UsekPart.ItemIndex := -1;

  if (Self.RB_Zpom_Usek.Checked) then
    CB_Zpom_UsekPart.ItemIndex := events.zpomaleni.usekpart;

  if (Self.RB_Zpom_IR.Checked) then
    Blky.NactiBlokyDoObjektu(CB_Zpom_IR, @CB_IR, nil, nil, _BLK_IR, events.zpomaleni.irid);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_BlkTUZastEvent.GetEvent():TBlkTUZastEvents;
begin
 Result.zastaveni.signal := TBlkTUSignal.disabled;
 Result.zpomaleni.signal := TBlkTUSignal.disabled;
 Result.zastaveni.speed  := 0;
 Result.zastaveni.stav   := (Self.CB_Zast_Stav.ItemIndex = 1);
 Result.zpomaleni.stav   := (Self.CB_Zpom_Stav.ItemIndex = 1);

 if (Self.RB_Zast_Usek.Checked) then
  begin
   Result.zastaveni.signal   := TBlkTUSignal.usek;
   Result.zastaveni.usekpart := Self.CB_Zast_UsekPart.ItemIndex;
  end;
 if (Self.RB_Zast_IR.Checked) then
  begin
   Result.zastaveni.signal := TBlkTUSignal.IR;
   try
     Result.zastaveni.irid := Blky.GetBlkID(Self.CB_IR[Self.CB_Zast_IR.ItemIndex]);
   except
     Result.zastaveni.irid := -1;
   end;
  end;

 if (Self.CHB_Zpomal.Checked) then
  begin
   Result.zpomaleni.speed := (Self.CB_ZpomalitKmH.ItemIndex+1) * 10;

   if (Self.RB_Zpom_Usek.Checked) then
    begin
     Result.zpomaleni.signal   := TBlkTUSignal.usek;
     Result.zpomaleni.usekpart := Self.CB_Zpom_UsekPart.ItemIndex;
    end;
    if (Self.RB_Zpom_IR.Checked) then
    begin
     Result.zpomaleni.signal   := TBlkTUSignal.IR;
     try
       Result.zpomaleni.irid   := Blky.GetBlkID(Self.CB_IR[Self.CB_Zpom_IR.ItemIndex]);
     except
       Result.zpomaleni.irid   := -1;
     end;
    end;
  end;//if CHB_Zpomalit.Checked
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
