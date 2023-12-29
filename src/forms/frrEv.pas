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
  Dialogs, StdCtrls, rrEvent, BlockDb, Mask, StrUtils, Generics.Collections,
  BlockTrack;

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
    CB_Track: TComboBox;
    CHB_Track: TCheckBox;
    GB_Distance: TGroupBox;
    E_Distance: TEdit;
    Label7: TLabel;
    procedure CB_TypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CB_TrackChange(Sender: TObject);
    procedure CHB_TrackClick(Sender: TObject);
  private
    CB_IRId: TList<Integer>;
    CB_TrackIds: TList<Integer>;
    _trackEnabled: Boolean;
    parentTrack: TBlkTrack;

    procedure SetTrackEnabled(new: Boolean);
    procedure UpdateTrackParts();

  public

    procedure FillFromRR(ev: TRREv; track: TBlkTrack = nil);
    procedure ShowEmpty();
    function GetRREv(): TRREv;
    function InputValid(): Boolean;

    property trackEnabled: Boolean read _trackEnabled write SetTrackEnabled;

  protected
    procedure SetEnabled(state: Boolean); override;

  end;

implementation

uses Block, ownConvert;

{$R *.dfm}

procedure TF_RREv.FormCreate(Sender: TObject);
begin
  Self.CB_IRId := TList<Integer>.Create();
  Self.CB_TrackIds := TList<Integer>.Create();
  Self.trackEnabled := False;

  Self.CB_Track_Part.Clear();
  for var i := 0 to TBlkTrack._MAX_PARTS-1 do
    Self.CB_Track_Part.Items.Add(IntToStr(i));
end;

procedure TF_RREv.FormDestroy(Sender: TObject);
begin
  Self.CB_IRId.Free();
  Self.CB_Track.Free();
end;

procedure TF_RREv.CB_TrackChange(Sender: TObject);
begin
  Self.UpdateTrackParts();
end;

procedure TF_RREv.UpdateTrackParts();
begin
  var itemIndex: Integer := Self.CB_Track_Part.ItemIndex;

  for var i: Integer := 0 to TBlkTrack._MAX_PARTS-1 do
    Self.CB_Track_Part.Items[i] := IntToStr(i);

  var track: TBlkTrack := nil;

  if ((Self.trackEnabled) and (Self.CHB_Track.Checked)) then
  begin
    if (Self.CB_Track.ItemIndex > -1) then
    begin
      var blkId := Self.CB_TrackIds[Self.CB_Track.ItemIndex];
      track := Blocks.GetBlkTrackOrRTByID(blkId);
    end;
  end else begin
    track := Self.parentTrack;
  end;

  if (track <> nil) then
  begin
    var blkTrackSettings := track.GetSettings();
    for var i: Integer := 0 to TBlkTrack._MAX_PARTS-1 do
      if (blkTrackSettings.RCSAddrs.Count > i) then
        Self.CB_Track_Part.Items[i] := IntToStr(i)+' (' + blkTrackSettings.RCSAddrs[i].ToString() + ')';
  end;

  Self.CB_Track_Part.ItemIndex := itemIndex;
end;

procedure TF_RREv.CB_TypeChange(Sender: TObject);
begin
  Self.GB_Track.Visible := (Self.CB_Type.ItemIndex = 0);
  Self.GB_IR.Visible := (Self.CB_Type.ItemIndex = 1);
  Self.GB_Time.Visible := (Self.CB_Type.ItemIndex = 2);
  Self.GB_Distance.Visible := (Self.CB_Type.ItemIndex = 3);
end;

procedure TF_RREv.CHB_TrackClick(Sender: TObject);
begin
  Self.CB_Track.Enabled := Self.CHB_Track.Checked;
  if (not Self.CHB_Track.Checked) then
    Self.CB_Track.ItemIndex := -1;
  Self.UpdateTrackParts();
end;

procedure TF_RREv.SetTrackEnabled(new: Boolean);
begin
  Self._trackEnabled := new;

  Self.CHB_Track.Visible := Self._trackEnabled;
  Self.CB_Track.Visible := Self._trackEnabled;

  if (Self._trackEnabled) then
    Self.GB_Track.Top := Self.CB_Track.Top + Self.CB_Track.Height + 6
  else
    Self.GB_Track.Top := Self.CHB_Track.Top;

  Self.GB_IR.Top := Self.GB_Track.Top;
  Self.GB_Time.Top := Self.GB_Track.Top;
  Self.GB_Distance.Top := Self.GB_Track.Top;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.ShowEmpty();
begin
  Self.parentTrack := nil;
  Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, nil, btTrack, btRT);
  Blocks.FillCB(Self.CB_IR, Self.CB_IRId, nil, nil, btIR);
  Self.CHB_Track.Checked := false;
  Self.CHB_TrackClick(Self);

  Self.CB_Track_Part.ItemIndex := 0;
  Self.CB_IR.ItemIndex := 0;
  Self.CB_Track_State.ItemIndex := 1;
  Self.CB_IR_State.ItemIndex := 1;
  Self.ME_Time.Text := '00:00.0';
  Self.CB_TypeChange(Self.CB_Type);
  Self.E_Distance.Text := '0';
  Self.UpdateTrackParts();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.FillFromRR(ev: TRREv; track: TBlkTrack = nil);
begin
  Self.ShowEmpty();
  Self.parentTrack := track;

  Self.CHB_Track.Checked := (ev.trackId > -1);
  Blocks.FillCB(Self.CB_Track, Self.CB_TrackIds, nil, nil, btTrack, btRT, ev.trackId);
  Self.CHB_TrackClick(Self);
  Self.UpdateTrackParts();

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

    rrtDist:
      begin
        Self.CB_Type.ItemIndex := 3;
        Self.E_Distance.Text := IntToStr(ev.data.distanceCm);
      end;
  end;

  Self.CB_TypeChange(Self.CB_Type);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.GetRREv(): TRREv;
var data: TRREvData;
begin
  if ((Self.trackEnabled) and (Self.CHB_Track.Checked)) then
    data.trackId := Self.CB_TrackIds[Self.CB_Track.ItemIndex]
  else
    data.trackId := -1;

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

    3:
      begin
        data.typ := rrtDist;
        data.distanceCm := StrToInt(Self.E_Distance.Text);
      end;
  end;

  Result := TRREv.Create(Self.trackEnabled, data);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_RREv.InputValid(): Boolean;
begin
  if (Self.CB_Type.ItemIndex < 0) then
    Exit(false);
  if ((Self.CHB_Track.Checked) and (Self.CB_Track.ItemIndex < 0)) then
    Exit(false);

  case (Self.CB_Type.ItemIndex) of
    0:
      if ((Self.CB_Track_Part.ItemIndex < 0) or (Self.CB_Track_State.ItemIndex < 0)) then
        Exit(false);
    1:
      if ((Self.CB_IR.ItemIndex < 0) or (Self.CB_IR_State.ItemIndex < 0)) then
        Exit(false);

    3: Exit(StrToIntDef(Self.E_Distance.Text, -1) <> -1);
  end;

  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_RREv.SetEnabled(state: Boolean);
begin
  inherited;

  Self.CHB_Track.Enabled := state;
  Self.CHB_TrackClick(Self);
  Self.CB_Track_Part.Enabled := state;
  Self.CB_IR.Enabled := state;
  Self.CB_Track_State.Enabled := state;
  Self.CB_IR_State.Enabled := state;
  Self.ME_Time.Enabled := state;
  Self.E_Distance.Enabled := state;

  if (not state) then
  begin
    Self.CB_Type.ItemIndex := -1;
    Self.CB_Track.ItemIndex := -1;
    Self.CB_Track_Part.ItemIndex := -1;
    Self.CB_IR.ItemIndex := -1;
    Self.CB_Track_State.ItemIndex := -1;
    Self.CB_IR_State.ItemIndex := -1;
    Self.ME_Time.Text := '00:00.0';
    Self.E_Distance.Text := '0';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
