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
  Dialogs, StdCtrls, rrEvent, BlockDb, Mask, StrUtils, Generics.Collections;

type
  TF_RREv = class(TForm)
    Label1: TLabel;
    CB_Type: TComboBox;
    GB_Track: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    CB_Track_State: TComboBox;
    CB_Track_Part: TComboBox;
    GB_IR: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    CB_IR_State: TComboBox;
    GB_Time: TGroupBox;
    Label6: TLabel;
    ME_Time: TMaskEdit;
    CB_IR: TComboBox;
    procedure CB_TypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CB_IRId: TList<Integer>;

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

procedure TF_RREv.FormCreate(Sender: TObject);
begin
  Self.CB_IRId := TList<Integer>.Create();
end;

procedure TF_RREv.FormDestroy(Sender: TObject);
begin
  Self.CB_IRId.Free();
end;

procedure TF_RREv.CB_TypeChange(Sender: TObject);
begin
  Self.GB_Track.Visible := false;
  Self.GB_IR.Visible := false;
  Self.GB_Time.Visible := false;

  case (Self.CB_Type.ItemIndex) of
    0:
      Self.GB_Track.Visible := true;
    1:
      Self.GB_IR.Visible := true;
    2:
      Self.GB_Time.Visible := true;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.ShowEmpty();
begin
  Blocks.FillCB(Self.CB_IR, Self.CB_IRId, nil, nil, btIR);
  Self.CB_Track_Part.ItemIndex := 0;
  Self.CB_IR.ItemIndex := 0;
  Self.CB_Track_State.ItemIndex := 1;
  Self.CB_IR_State.ItemIndex := 1;
  Self.ME_Time.Text := '00:00.0';
  Self.CB_TypeChange(Self.CB_Type);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.FillFromRR(ev: TRREv);
begin
  Self.ShowEmpty();

  case (ev.typ) of
    rrtTrack:
      begin
        Self.CB_Type.ItemIndex := 0;
        Self.CB_Track_State.ItemIndex := ownConvert.BoolToInt(ev.data.trackState);
        Self.CB_Track_Part.ItemIndex := ev.data.trackPart;
      end;

    rrtIR:
      begin
        Self.CB_Type.ItemIndex := 1;
        Blocks.FillCB(Self.CB_IR, Self.CB_IRId, nil, nil, btIR, btIR, ev.data.irId);
        Self.CB_IR_State.ItemIndex := ownConvert.BoolToInt(ev.data.irState);
      end;

    rrtTime:
      begin
        Self.CB_Type.ItemIndex := 2;
        Self.ME_Time.Text := FormatDateTime('nn:ss.z', ev.data.time);
      end;
  end;

  Self.CB_TypeChange(Self.CB_Type);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.GetRREv(): TRREv;
var data: TRREvData;
begin
  case (Self.CB_Type.ItemIndex) of
    0:
      begin
        data.typ := rrtTrack;
        data.trackPart := Self.CB_Track_Part.ItemIndex;
        data.trackState := ownConvert.IntToBool(Self.CB_Track_State.ItemIndex);
      end;

    1:
      begin
        data.typ := rrtIR;
        data.irId := Self.CB_IRId[Self.CB_IR.ItemIndex];
        data.irState := ownConvert.IntToBool(Self.CB_IR_State.ItemIndex);
      end;

    2:
      begin
        data.typ := rrtTime;
        data.time := EncodeTime(0, StrToInt(LeftStr(Self.ME_Time.Text, 2)), StrToInt(Copy(Self.ME_Time.Text, 4, 2)),
          StrToInt(RightStr(Self.ME_Time.Text, 1)));
      end;
  end;

  Result := TRREv.Create(data);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.InputValid(): Boolean;
begin
  if (Self.CB_Type.ItemIndex < 0) then
    Exit(false);

  case (Self.CB_Type.ItemIndex) of
    0:
      if ((Self.CB_Track_Part.ItemIndex < 0) or (Self.CB_Track_State.ItemIndex < 0)) then
        Exit(false);
    1:
      if ((Self.CB_IR.ItemIndex < 0) or (Self.CB_IR_State.ItemIndex < 0)) then
        Exit(false);
  end;

  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.SetEnabled(state: Boolean);
begin
  inherited;

  Self.CB_Type.Enabled := state;
  Self.CB_Track_Part.Enabled := state;
  Self.CB_IR.Enabled := state;
  Self.CB_Track_State.Enabled := state;
  Self.CB_IR_State.Enabled := state;
  Self.ME_Time.Enabled := state;

  if (not state) then
  begin
    Self.CB_Type.ItemIndex := -1;
    Self.CB_Track_Part.ItemIndex := -1;
    Self.CB_IR.ItemIndex := -1;
    Self.CB_Track_State.ItemIndex := -1;
    Self.CB_IR_State.ItemIndex := -1;
    Self.ME_Time.Text := '00:00.0';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
