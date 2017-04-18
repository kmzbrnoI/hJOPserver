unit fhoukEv;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, houkEvent, frrEv, ExtCtrls;

type
  TF_HoukEv = class(TForm)
    Label1: TLabel;
    CB_Action: TComboBox;
    Label2: TLabel;
    P_Ev: TPanel;
    CB_Func: TComboBox;
  private
    fRREv:TF_RREv;

    procedure FillFuncs();

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy(); override;

    procedure FillFromHouk(ev:THoukEv);
    procedure ShowEmpty();
    function GetHoukEv():THoukEv;
    function InputValid():boolean;

  end;

implementation

uses FunkceVyznam;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////

constructor TF_HoukEv.Create(AOwner:TComponent);
begin
 inherited;
 Self.fRREv := TF_RREv.Create(nil);
 Self.fRREv.Parent := Self.P_Ev;
 Self.fRREv.Show();
end;

destructor TF_HoukEv.Destroy();
begin
 Self.fRREv.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEv.FillFromHouk(ev:THoukEv);
begin
 Self.FillFuncs();
 Self.CB_Func.Text := ev.sound;
 Self.CB_Action.ItemIndex := Integer(ev.funcType);
 Self.fRREv.FillFromRR(ev.event);
end;

procedure TF_HoukEv.ShowEmpty();
begin
 Self.FillFuncs();
 Self.CB_Func.Text := '';
 Self.CB_Action.ItemIndex := 0;
 Self.fRREv.ShowEmpty();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEv.FillFuncs();
var str:string;
begin
 Self.CB_Func.Clear();
 for str in FuncsFyznam.Items do
   Self.CB_Func.Items.Add(str);
end;

////////////////////////////////////////////////////////////////////////////////

function TF_HoukEv.GetHoukEv():THoukEv;
begin
 Result := THoukEv.Create(Self.fRREv.GetRREv(), Self.CB_Func.Text,
                           THoukFuncType(Self.CB_Action.ItemIndex));
end;

////////////////////////////////////////////////////////////////////////////////

function TF_HoukEv.InputValid():boolean;
begin
 Result := Self.fRREv.InputValid() and (Self.CB_Action.ItemIndex > -1);
end;

////////////////////////////////////////////////////////////////////////////////

end.
