unit fBlkCrossingState;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, BlockCrossing,
  BlockCrossingLogic;

type
  TF_BlkCrossingState = class(TForm)
    Label1: TLabel;
    M_Note: TMemo;
    Label6: TLabel;
    M_Lockout: TMemo;
    B_Refresh: TButton;
    B_Apply: TButton;
    CHB_PcEmOpen: TCheckBox;
    CHB_PcClosed: TCheckBox;
    CHB_Zaver: TCheckBox;
    CHB_Annulation: TCheckBox;
    LV_Tracks: TListView;
    Label2: TLabel;
    CB_State: TComboBox;
    procedure B_ApplyClick(Sender: TObject);
    procedure B_RefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    crossing: TBlkCrossing;

    procedure Refresh();
    procedure Apply();

  public
    procedure Open(crossing: TBlkCrossing);

  end;

var
  F_BlkCrossingState: TF_BlkCrossingState;

implementation

uses ownConvert;

{$R *.dfm}

procedure TF_BlkCrossingState.B_ApplyClick(Sender: TObject);
begin
  Self.Apply();
  Self.Update();
end;

procedure TF_BlkCrossingState.B_RefreshClick(Sender: TObject);
begin
  Self.Refresh();
end;

procedure TF_BlkCrossingState.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Self.crossing := nil;
end;

procedure TF_BlkCrossingState.FormCreate(Sender: TObject);
begin
  Self.crossing := nil;
end;

procedure TF_BlkCrossingState.Open(crossing: TBlkCrossing);
begin
  Self.crossing := crossing;
  Self.Refresh();
  Self.Caption := 'Stav pøejezdu ' + crossing.name;
  Self.Show();
end;

procedure TF_BlkCrossingState.Refresh();
begin
  var state: TBlkCrossingState := Self.crossing.fullState;

  case (state.state) of
    TBlkCrossingBasicState.disabled: Self.CB_State.ItemIndex := 0;
    TBlkCrossingBasicState.none: Self.CB_State.ItemIndex := 1;
    TBlkCrossingBasicState.open: Self.CB_State.ItemIndex := 2;
    TBlkCrossingBasicState.caution: Self.CB_State.ItemIndex := 3;
    TBlkCrossingBasicState.closed: Self.CB_State.ItemIndex := 4;
  else
    Self.CB_State.ItemIndex := -1;
  end;

  Self.M_Note.Text := state.note;
  Self.M_Lockout.Text := state.lockout;
  Self.CHB_PcEmOpen.Checked := state.pcEmOpen;
  Self.CHB_PcClosed.Checked := state.pcClosed;
  Self.CHB_Zaver.Checked := crossing.zaver;
  Self.CHB_Annulation.Checked := crossing.annulation;

  Self.LV_Tracks.Clear();
  for var track: TBlkcrossingTrack in crossing.tracks do
  begin
    var LI: TListItem := Self.LV_Tracks.Items.Add();
    LI.Caption := IntToStr(Self.LV_Tracks.Items.Count);


    case (track.state) of
      TBlkCrossingTrackState.tsFree: LI.SubItems.Add('tsFree');
      TBlkCrossingTrackState.tsException: LI.SubItems.Add('tsException');
      TBlkCrossingTrackState.tsUnexpectedOccupation: LI.SubItems.Add('tsUnexpectedOccupation');

      TBlkCrossingTrackState.tsLRLeftOccupied: LI.SubItems.Add('tsLRLeftOccupied');
      TBlkCrossingTrackState.tsLRLeftMidOccupied: LI.SubItems.Add('tsLRLeftMidOccupied');
      TBlkCrossingTrackState.tsLRMidOccupied: LI.SubItems.Add('tsLRMidOccupied');
      TBlkCrossingTrackState.tsLROnlyRightOccupied: LI.SubItems.Add('tsLROnlyRightOccupied');
      TBlkCrossingTrackState.tsLRRightOutOccupied: LI.SubItems.Add('tsLRRightOutOccupied');

      TBlkCrossingTrackState.tsRLRightOccupied: LI.SubItems.Add('tsRLRightOccupied');
      TBlkCrossingTrackState.tsRLRightMidOccupied: LI.SubItems.Add('tsRLRightMidOccupied');
      TBlkCrossingTrackState.tsRLMidOccupied: LI.SubItems.Add('tsRLMidOccupied');
      TBlkCrossingTrackState.tsRLOnlyLeftOccupied: LI.SubItems.Add('tsRLOnlyLeftOccupied');
      TBlkCrossingTrackState.tsRLLeftOutOccupied: LI.SubItems.Add('tsRLLeftOutOccupied');
    else
      LI.SubItems.Add('?');
    end;

    LI.SubItems.Add(BoolToYesNo(track.shouldBeClosed));
    LI.SubItems.Add(BoolToYesNo(track.positiveLight));
    LI.SubItems.Add(BoolToYesNo(track.anullation));
  end;
end;

procedure TF_BlkCrossingState.Apply();
begin
  if (Self.crossing = nil) then
    Exit();

  Self.crossing.note := Self.M_Note.Text;
  Self.crossing.lockout := Self.M_Lockout.Text;
  Self.crossing.pcEmOpen := Self.CHB_PcEmOpen.Checked;
  Self.crossing.pcClosed := Self.CHB_PcClosed.Checked;
end;

end.
