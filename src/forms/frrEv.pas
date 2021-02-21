unit frrEv;

{
  Okno TF_RREv umoznuje definovat udalost ve smyslu tridy TRREv (Railroad event).
  Toto male okynko se pouziva napriklad pri definici houkacich udalosti,
  zpomalovani a zastavovani pred navestidly, planovane vyuziti je take na
  zastavky.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rrEvent, BlockDb, Mask, StrUtils;

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
    ME_Cas: TMaskEdit;
    procedure CB_TypChange(Sender: TObject);
  private
    CB_IR: TArI;

  public
    procedure FillFromRR(ev: TRREv);
    procedure ShowEmpty();
    function GetRREv(): TRREv;
    function InputValid(): Boolean;

  protected
    procedure SetEnabled(state: Boolean); override;

  end;

implementation

uses Block, ownConvert;

{$R *.dfm}

procedure TF_RREv.CB_TypChange(Sender: TObject);
begin
  Self.GB_Usek.Visible := false;
  Self.GB_IR.Visible := false;
  Self.GB_Cas.Visible := false;

  case (Self.CB_Typ.ItemIndex) of
    0:
      Self.GB_Usek.Visible := true;
    1:
      Self.GB_IR.Visible := true;
    2:
      Self.GB_Cas.Visible := true;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.ShowEmpty();
begin
  Blocks.FillCB(CB_IRId, @CB_IR, nil, nil, btIR);
  Self.CB_UsekPart.ItemIndex := 0;
  Self.CB_IRId.ItemIndex := 0;
  Self.CB_UsekStav.ItemIndex := 1;
  Self.CB_IRStav.ItemIndex := 1;
  Self.ME_Cas.Text := '00:00.0';
  Self.CB_TypChange(Self.CB_Typ);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.FillFromRR(ev: TRREv);
begin
  Self.ShowEmpty();

  case (ev.typ) of
    rrtUsek:
      begin
        Self.CB_Typ.ItemIndex := 0;
        Self.CB_UsekStav.ItemIndex := ownConvert.BoolToInt(ev.data.usekState);
        Self.CB_UsekPart.ItemIndex := ev.data.usekPart;
      end;

    rrtIR:
      begin
        Self.CB_Typ.ItemIndex := 1;
        Blocks.FillCB(CB_IRId, @CB_IR, nil, nil, btIR, ev.data.irId);
        Self.CB_IRStav.ItemIndex := ownConvert.BoolToInt(ev.data.irState);
      end;

    rrtTime:
      begin
        Self.CB_Typ.ItemIndex := 2;
        Self.ME_Cas.Text := FormatDateTime('nn:ss.z', ev.data.time);
      end;
  end;

  Self.CB_TypChange(Self.CB_Typ);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.GetRREv(): TRREv;
var data: TRREvData;
begin
  case (Self.CB_Typ.ItemIndex) of
    0:
      begin
        data.typ := rrtUsek;
        data.usekPart := Self.CB_UsekPart.ItemIndex;
        data.usekState := ownConvert.IntToBool(Self.CB_UsekStav.ItemIndex);
      end;

    1:
      begin
        data.typ := rrtIR;
        data.irId := Blocks.GetBlkID(CB_IR[Self.CB_IRId.ItemIndex]);
        data.irState := ownConvert.IntToBool(Self.CB_IRStav.ItemIndex);
      end;

    2:
      begin
        data.typ := rrtTime;
        data.time := EncodeTime(0, StrToInt(LeftStr(Self.ME_Cas.Text, 2)), StrToInt(Copy(Self.ME_Cas.Text, 4, 2)),
          StrToInt(RightStr(Self.ME_Cas.Text, 1)));
      end;
  end;

  Result := TRREv.Create(data);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.InputValid(): Boolean;
begin
  if (Self.CB_Typ.ItemIndex < 0) then
    Exit(false);

  case (Self.CB_Typ.ItemIndex) of
    0:
      if ((Self.CB_UsekPart.ItemIndex < 0) or (Self.CB_UsekStav.ItemIndex < 0)) then
        Exit(false);
    1:
      if ((Self.CB_IRId.ItemIndex < 0) or (Self.CB_IRStav.ItemIndex < 0)) then
        Exit(false);
  end;

  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.SetEnabled(state: Boolean);
begin
  inherited;

  Self.CB_Typ.Enabled := state;
  Self.CB_UsekPart.Enabled := state;
  Self.CB_IRId.Enabled := state;
  Self.CB_UsekStav.Enabled := state;
  Self.CB_IRStav.Enabled := state;
  Self.ME_Cas.Enabled := state;

  if (not state) then
  begin
    Self.CB_Typ.ItemIndex := -1;
    Self.CB_UsekPart.ItemIndex := -1;
    Self.CB_IRId.ItemIndex := -1;
    Self.CB_UsekStav.ItemIndex := -1;
    Self.CB_IRStav.ItemIndex := -1;
    Self.ME_Cas.Text := '00:00.0';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
