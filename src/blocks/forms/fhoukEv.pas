unit fhoukEv;

{
  TF_HoukEv je okno, ktere umoznuje definovat jednu houkaci udalost ve smyslu
  tridy THoukEv.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, BlockTrack,
  Dialogs, StdCtrls, houkEvent, frrEv, ExtCtrls, Generics.Collections;

type
  TF_HoukEv = class(TForm)
    Label1: TLabel;
    CB_Action: TComboBox;
    Label2: TLabel;
    P_Ev: TPanel;
    CB_Func: TComboBox;
  private
    frrEv: TF_RREv;

    procedure FillFuncs();

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure FillFromHouk(ev: THoukEv; parentTrack: TBlkTrack);
    procedure ShowEmpty();
    function GetHoukEv(): THoukEv;
    function InputValid(): Boolean;

  end;

implementation

uses FunkceVyznam, TRailVehicle;

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

constructor TF_HoukEv.Create(AOwner: TComponent);
begin
  inherited;
  Self.frrEv := TF_RREv.Create(nil);
  Self.frrEv.Parent := Self.P_Ev;
  Self.frrEv.Show();
end;

destructor TF_HoukEv.Destroy();
begin
  Self.frrEv.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEv.FillFromHouk(ev: THoukEv; parentTrack: TBlkTrack);
begin
  Self.FillFuncs();
  Self.CB_Func.Text := ev.sound;
  Self.CB_Action.ItemIndex := Integer(ev.funcType);
  Self.frrEv.FillFromRR(ev.event, parentTrack);
end;

procedure TF_HoukEv.ShowEmpty();
begin
  Self.FillFuncs();
  Self.CB_Func.Text := '';
  Self.CB_Action.ItemIndex := 0;
  Self.frrEv.ShowEmpty();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEv.FillFuncs();
begin
  Self.CB_Func.Clear();
  var all: TList<TPair<string, TRVFuncType>> := FuncNames.All();
  try
    for var entry in all do
      Self.CB_Func.Items.Add(entry.Key);
  finally
    all.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_HoukEv.GetHoukEv(): THoukEv;
begin
  Result := THoukEv.Create(Self.frrEv.GetRREv(), Self.CB_Func.Text, THoukFuncType(Self.CB_Action.ItemIndex));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_HoukEv.InputValid(): Boolean;
begin
  Result := Self.frrEv.InputValid() and (Self.CB_Action.ItemIndex > -1);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
