unit frrEv;

{
  Okno TF_RREv umoznuje definovat udalost ve smyslu tridy TRREv (Railroad event).
  Toto male okynko se pouziva napriklad pri definici houkacich udalosti,
  planovane vyuziti je take na zastavky a zastavovani a zpomalovani pred
  navestidly.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, rrEvent, TBloky;

type
  TF_RREv = class(TForm)
    Label1: TLabel;
    CB_Typ: TComboBox;
    GB_Usek: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    CB_UsekStav: TComboBox;
    CB_UsekPart: TComboBox;
    GB_IR: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    CB_IRStav: TComboBox;
    CB_IRId: TComboBox;
    GB_Cas: TGroupBox;
    Label6: TLabel;
    SE_Cas: TSpinEdit;
    procedure CB_TypChange(Sender: TObject);
  private
    CB_IR:TArI;

  public
    procedure FillFromRR(ev:TRREv);
    procedure ShowEmpty();
    function GetRREv():TRREv;
    function InputValid():boolean;

  end;

implementation

uses TBlok, Prevody;

{$R *.dfm}

procedure TF_RREv.CB_TypChange(Sender: TObject);
begin
 Self.GB_Usek.Visible := false;
 Self.GB_IR.Visible := false;
 Self.GB_Cas.Visible := false;

 case (Self.CB_Typ.ItemIndex) of
  0: Self.GB_Usek.Visible := true;
  1: Self.GB_IR.Visible := true;
  2: Self.GB_Cas.Visible := true;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.ShowEmpty();
begin
 Blky.NactiBlokyDoObjektu(CB_IRId, @CB_IR, nil, nil, _BLK_IR);
 Self.CB_UsekPart.ItemIndex := 0;
 Self.CB_IRId.ItemIndex := 0;
 Self.CB_UsekStav.ItemIndex := 1;
 Self.CB_IRStav.ItemIndex := 1;
 Self.SE_Cas.Value := 0;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.FillFromRR(ev:TRREv);
begin
 Self.ShowEmpty();

 case (ev.typ) of
   rrtUsek: begin
     Self.CB_Typ.ItemIndex := 0;
     Self.CB_UsekStav.ItemIndex := PrevodySoustav.BoolToInt(ev.data.usekState);
     Self.CB_UsekPart.ItemIndex := ev.data.usekPart;
   end;

   rrtIR: begin
     Self.CB_Typ.ItemIndex := 1;
     Blky.NactiBlokyDoObjektu(CB_IRId, @CB_IR, nil, nil, _BLK_IR, ev.data.irId);
     Self.CB_IRStav.ItemIndex := PrevodySoustav.BoolToInt(ev.data.irState);
   end;

   rrtTime: begin
     Self.CB_Typ.ItemIndex := 2;
     Self.SE_Cas.Value := ev.data.timeSec;
   end;
 end;

 Self.CB_TypChange(Self.CB_Typ);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_RREv.GetRREv():TRREv;
var data:TRREvData;
begin
 case (Self.CB_Typ.ItemIndex) of
  0: begin
    data.typ := rrtUsek;
    data.usekPart := Self.CB_UsekPart.ItemIndex;
    data.usekState := PrevodySoustav.IntToBool(Self.CB_UsekStav.ItemIndex);
  end;

  1: begin
    data.typ := rrtIR;
    data.irId := CB_IR[Self.CB_IRId.ItemIndex];
    data.irState := PrevodySoustav.IntToBool(Self.CB_IRStav.ItemIndex);
  end;

  2: begin
    data.typ := rrtTime;
    data.timeSec := Self.SE_Cas.Value;
  end;
 end;

 Result := TRREv.Create(data);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_RREv.InputValid():boolean;
begin
 if (Self.CB_Typ.ItemIndex < 0) then Exit(false);

 case (Self.CB_Typ.ItemIndex) of
  0: if ((Self.CB_UsekPart.ItemIndex < 0) or (Self.CB_UsekStav.ItemIndex < 0)) then Exit(false);
  1: if ((Self.CB_IRId.ItemIndex < 0) or (Self.CB_IRStav.ItemIndex < 0)) then Exit(false);
 end;

 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

end.
