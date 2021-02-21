unit fHoukEvsUsek;

{
  Okno TF_HoukEvsUsek umoznuje editovat houkaci udalosti technologickeho
  bloku usek.

  Okno vyuziva 2 okna TF_HoukEvs, jedno na houkaci udalosti v lichem smeru
  jizdy soupravy a druhe na udalosti v sudem smeru jizdy soupravy.
}

interface

uses
  Windows, Variants, Classes, Graphics, Controls, Forms, Dialogs, BlockTrack,
  fhoukEvs, StdCtrls, ExtCtrls;

type
  TF_HoukEvsUsek = class(TForm)
    B_Apply: TButton;
    B_Storno: TButton;
    P_HoukL: TPanel;
    Label1: TLabel;
    P_HoukS: TPanel;
    Label2: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
  private
    blk: TBlkTrack;
    formL: TF_HoukEvs;
    Forms: TF_HoukEvs;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Open(blk: TBlkTrack);

  end;

var
  F_HoukEvsUsek: TF_HoukEvsUsek;

implementation

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

constructor TF_HoukEvsUsek.Create(AOwner: TComponent);
begin
  inherited;

  Self.blk := nil;

  formL := TF_HoukEvs.Create(nil);
  formL.Parent := Self.P_HoukL;
  formL.Left := 20;
  formL.Show();

  Forms := TF_HoukEvs.Create(nil);
  Forms.Parent := Self.P_HoukS;
  Forms.Left := 20;
  Forms.Show();
end;

destructor TF_HoukEvsUsek.Destroy();
begin
  formL.Free();
  Forms.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvsUsek.Open(blk: TBlkTrack);
begin
  Self.blk := blk;

  formL.FillFromHouks(blk.GetSettings().houkEvL);
  Forms.FillFromHouks(blk.GetSettings().houkEvS);

  Self.Caption := 'Houkací události úseku ' + blk.name;
  Self.ActiveControl := B_Apply;
  Self.ShowModal();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvsUsek.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvsUsek.B_ApplyClick(Sender: TObject);
var s: TBlkTrackSettings;
begin
  if (not formL.InputValid()) then
  begin
    Application.MessageBox('Nějaká událost v lichém směru je špatně zadaná!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (not Forms.InputValid()) then
  begin
    Application.MessageBox('Nějaká událost v sudém směru je špatně zadaná!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  if (Self.blk <> nil) then
  begin
    s := Self.blk.GetSettings();
    s.houkEvL := formL.GetHoukEvs();
    s.houkEvS := Forms.GetHoukEvs();
    Self.blk.SetSettings(s); // destructors of the old data should be called manually
  end;

  Self.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
