unit fTester;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, Buttons, ComCtrls, fMain, BlockDb,
  Generics.Collections, UITypes;

type
  TMouseUpEvent = procedure (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TIOGUI = class
  const
    _PADDING: Integer = 5;

  public
    panel: TPanel;
    shape: TShape;
    description: TLabel;
    value: TLabel;

    constructor Create(parent: TWinControl; i: Integer; onMouseUp: TMouseUpEvent); // must call resize after create to properly show
    destructor Destroy(); override;
    procedure Resize(top, width, height: Integer);
  end;

  TF_Tester = class(TForm)
    GB_Inputs: TGroupBox;
    L_1: TLabel;
    CB_Addr: TComboBox;
    GB_Outputs: TGroupBox;
    CB_AddrIn: TComboBox;
    CB_AddrOut: TComboBox;
    P_Inputs: TPanel;
    P_Outputs: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure CB_AddrChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CB_AddrInChange(Sender: TObject);
    procedure CB_AddrOutChange(Sender: TObject);

  private const
    _S_TOP = 18;
    _S_HEIGHT = 12;
    _S_WIDTH = 25;
    _S_LEFT = 10;
    _L_LEFT = 40;
    _NO_INPUTS = 16;
    _NO_OUTPUTS = 16;

  private
    RCSAddr: Integer;
    RCSAddrInputs: Integer;
    RCSAddrOutputs: Integer;

    CB_RCSAdrData: TList<Cardinal>; // always sorted!
    inputs: TObjectList<TIOGUI>;
    outputs: TObjectList<TIOGUI>;

    procedure CreateInputs(rcsAddr: Integer);
    procedure CreateOutputs(rcsAddr: Integer);
    procedure SOutputMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FillRCSAddrs();

  public
    procedure UpdateOutputs();
    procedure UpdateInputs();
    procedure UpdateState();

    procedure AfterRCSOpen();
    procedure RCSModuleChanged(addr: Cardinal);
    procedure RCSModuleInputsChanged(addr: Cardinal);
    procedure RCSModuleOutputsChanged(addr: Cardinal);

  end;

var
  F_Tester: TF_Tester;

implementation

uses RCSc, Logging, RCSIFace, BlockSignal, RCSErrors, IfThenElse, ownGuiUtils;

{$R *.dfm}

constructor TIOGUI.Create(parent: TWinControl; i: Integer; onMouseUp: TMouseUpEvent);
begin
  inherited Create();

  Self.panel := TPanel.Create(nil);
  Self.panel.Parent := parent;
  Self.panel.Left := 0;
  Self.panel.BevelOuter := bvNone;
  Self.panel.ParentColor := false;
  Self.panel.ParentBackground := false;

  Self.shape := TShape.Create(nil);
  Self.shape.Parent := Self.panel;
  Self.shape.Left := _PADDING;
  Self.shape.Top := _PADDING;
  Self.shape.Tag := i;
  Self.shape.Brush.Color := clGray;
  Self.shape.OnMouseUp := onMouseUp;

  Self.description := TLabel.Create(nil);
  Self.description.Parent := Self.panel;
  Self.description.AutoSize := true;
  Self.description.Font.Size := 10;
  Self.description.Caption := IntToStr(i);

  Self.value := TLabel.Create(nil);
  Self.value.Parent := Self.panel;
  Self.value.AutoSize := true;
  Self.value.Font.Size := 10;
  Self.value.Caption := '-';
end;

destructor TIOGUI.Destroy();
begin
  Self.shape.Free();
  Self.description.Free();
  Self.value.Free();
  Self.panel.Free();
  inherited Destroy();
end;

procedure TIOGUI.Resize(top, width, height: Integer);
begin
  Self.panel.Top := top;
  Self.panel.Width := width;
  Self.panel.Height := height;
  Self.shape.Top := (height div 8);
  Self.shape.Width := (width div 2);
  Self.shape.Height := height - (2*Self.shape.Top);
  Self.description.Left := (width div 2) + 3*_PADDING;
  Self.description.Top := (height div 2) - (Self.description.Height div 2);
  Self.value.Left := Round(width/4*3) + _PADDING;
  Self.value.Top := (height div 2) - (Self.value.Height div 2);
end;

procedure TF_Tester.UpdateState();
begin
  if (Self.RCSAddrInputs > -1) then
  begin
    try
      if (RCSi.IsModuleFailure(Self.RCSAddrInputs)) then begin
        Self.GB_Inputs.Color := clSilver;
      end else if (RCSi.IsModuleError(Self.RCSAddrInputs)) then begin
        Self.GB_Inputs.Color := clRed;
      end else if (RCSi.IsModuleWarning(Self.RCSAddrInputs)) then begin
        Self.GB_Inputs.Color := clYellow;
      end else begin
        Self.GB_Inputs.Color := clBtnFace;
      end;
    except
      Self.GB_Inputs.Color := clRed;
    end;
  end else begin
    Self.GB_Inputs.Color := clBtnFace;
  end;

  if (Self.RCSAddrOutputs > -1) then
  begin
    try
      if (RCSi.IsModuleFailure(Self.RCSAddrOutputs)) then begin
        Self.GB_Outputs.Color := clSilver;
      end else if (RCSi.IsModuleError(Self.RCSAddrOutputs)) then begin
        Self.GB_Outputs.Color := clRed;
      end else if (RCSi.IsModuleWarning(Self.RCSAddrOutputs)) then begin
        Self.GB_Outputs.Color := clYellow;
      end else begin
        Self.GB_Outputs.Color := clBtnFace;
      end;
    except
      Self.GB_Outputs.Color := clRed;
    end;
  end else begin
    Self.GB_Outputs.Color := clBtnFace;
  end;
end;

procedure TF_Tester.UpdateInputs();
begin
  if ((not RCSi.NoExStarted()) or (Self.RCSAddrInputs < 0)) then
  begin
    for var i := 0 to Self.inputs.Count-1 do
      Self.inputs[i].shape.Brush.Color := clGray;
    Exit();
  end;

  for var i := 0 to Self.inputs.Count-1 do
  begin
    var state: TRCSInputState;
    try
      state := RCSi.GetInput(Self.RCSAddrInputs, i);
    except
      state := failure;
    end;

    if (state = TRCSInputState.isOn) then begin
      Self.inputs[i].panel.Color := ite(Self.inputs[i].shape.Brush.Color = clRed, clYellow, clBtnFace);
      Self.inputs[i].shape.Brush.Color := clLime;
      Self.inputs[i].value.Caption := 'on';
    end else if (state = TRCSInputState.isOff) then begin
      Self.inputs[i].panel.Color := ite(Self.inputs[i].shape.Brush.Color = clLime, clYellow, clBtnFace);
      Self.inputs[i].shape.Brush.Color := clRed;
      Self.inputs[i].value.Caption := 'off';
    end else if (state = TRCSInputState.notYetScanned) then begin
      Self.inputs[i].panel.Color := ite(Self.inputs[i].shape.Brush.Color <> clSilver, clYellow, clBtnFace);
      Self.inputs[i].shape.Brush.Color := clSilver;
      Self.inputs[i].value.Caption := 'NYS';
    end else begin
      Self.inputs[i].panel.Color := ite(Self.inputs[i].shape.Brush.Color <> clGray, clYellow, clBtnFace);
      Self.inputs[i].shape.Brush.Color := clGray;
      Self.inputs[i].value.Caption := '-';
    end;
  end; // for i
end;

procedure TF_Tester.UpdateOutputs();
begin
  if ((not RCSi.NoExStarted()) or (Self.RCSAddrOutputs < 0)) then
  begin
    for var i := 0 to Self.outputs.Count-1 do
      Self.outputs[i].shape.Brush.Color := clGray;
    Exit();
  end;

  for var i := 0 to Self.outputs.Count-1 do
  begin
    var state: TRCSOutputState := osFailure;
    var typ: TRCSOPortType := optPlain;
    try
      state := RCSi.GetOutputState(Self.RCSAddrOutputs, i);
      typ := RCSI.GetOutputType(Self.RCSAddrOutputs, i);
    except

    end;

    if (typ = optSCom) then
    begin
      Self.outputs[i].shape.Brush.Color := ite(state = osDisabled, clBlue, clWhite);
      Self.outputs[i].value.Caption := IntToStr(Integer(state));
    end else begin
      Self.outputs[i].shape.Brush.Color := ite(state = osDisabled, clRed, clLime);
      if (state = osDisabled) then
        Self.outputs[i].value.Caption := 'off'
      else if (state = osEnabled) then
        Self.outputs[i].value.Caption := 'on';
    end;

    case (state) of
      osf60:
        Self.outputs[i].value.Caption := 'f60';
      osf120:
        Self.outputs[i].value.Caption := 'f120';
      osf180:
        Self.outputs[i].value.Caption := 'f180';
      osf240:
        Self.outputs[i].value.Caption := 'f240';
      osf300:
        Self.outputs[i].value.Caption := 'f300';
      osf600:
        Self.outputs[i].value.Caption := 'f600';
      osf33:
        Self.outputs[i].value.Caption := 'f33';
      osf66:
        Self.outputs[i].value.Caption := 'f66';

      osFailure, osNotYetScanned, osUnavailableModule, osUnavailablePort:
      begin
        Self.outputs[i].shape.Brush.Color := clGray;
        Self.outputs[i].value.Caption := '-';
      end;
    end;
  end; // for i
end;

procedure TF_Tester.FormCreate(Sender: TObject);
begin
  Self.CB_RCSAdrData := TList<Cardinal>.Create();
  Self.inputs := TObjectList<TIOGUI>.Create();
  Self.outputs := TObjectList<TIOGUI>.Create();

  Self.RCSAddr := -1;
  Self.RCSAddrInputs := -1;
  Self.RCSAddrOutputs := -1;
end;

procedure TF_Tester.FormDestroy(Sender: TObject);
begin
  Self.CB_RCSAdrData.Free();
  Self.inputs.Free();
  Self.outputs.Free();
end;

procedure TF_Tester.FormResize(Sender: TObject);
begin
  Self.CB_Addr.Width := Self.ClientWidth - (2*Self.CB_Addr.Left);
  Self.GB_Inputs.Width := (Self.ClientWidth div 2) - Round(1.5*Self.GB_Inputs.Left);
  Self.GB_Outputs.Left := (Self.ClientWidth div 2) + (Self.GB_Inputs.Left div 2);
  Self.GB_Outputs.Width := Self.GB_Inputs.Width;
  Self.GB_Inputs.Height := Self.ClientHeight - Self.GB_Inputs.Top - 5;
  Self.GB_Outputs.Height := Self.ClientHeight - Self.GB_Outputs.Top - 5;

  begin
    var aTop: Integer := 0;
    for var i: Integer := 0 to Self.inputs.Count-1 do
    begin
      Self.inputs[i].Resize(aTop, Self.P_Inputs.ClientWidth, Self.P_Inputs.ClientHeight div Self.inputs.Count);
      aTop := aTop + (Self.P_Inputs.ClientHeight div Self.inputs.Count);
    end;
  end;

  begin
    var aTop: Integer := 0;
    for var i: Integer := 0 to Self.outputs.Count-1 do
    begin
      Self.outputs[i].Resize(aTop, Self.P_Outputs.ClientWidth, Self.P_Outputs.ClientHeight div Self.outputs.Count);
      aTop := aTop + (Self.P_Outputs.ClientHeight div Self.outputs.Count);
    end;
  end;
end;

procedure TF_Tester.CB_AddrInChange(Sender: TObject);
begin
  Self.inputs.Clear();

  if (Self.CB_AddrIn.ItemIndex > -1) then
  begin
    Self.RCSAddrInputs := Self.CB_RCSAdrData[Self.CB_AddrIn.ItemIndex];
    Self.CreateInputs(Self.RCSAddrInputs);
    Self.UpdateInputs();
  end else
    Self.RCSAddrInputs := -1;

  Self.UpdateState();
end;

procedure TF_Tester.CB_AddrOutChange(Sender: TObject);
begin
  Self.outputs.Clear();

  if (Self.CB_AddrOut.ItemIndex > -1) then
  begin
    Self.RCSAddrOutputs := Self.CB_RCSAdrData[Self.CB_AddrOut.ItemIndex];
    Self.CreateOutputs(Self.RCSAddrOutputs);
    Self.UpdateOutputs();
  end else
    Self.RCSAddrOutputs := -1;

  Self.UpdateState();
end;

procedure TF_Tester.CB_AddrChange(Sender: TObject);
begin
  Self.CB_AddrIn.ItemIndex := Self.CB_Addr.ItemIndex;
  Self.CB_AddrInChange(Self);
  Self.CB_AddrOut.ItemIndex := Self.CB_Addr.ItemIndex;
  Self.CB_AddrOutChange(Self);
end;

procedure TF_Tester.CreateInputs(rcsAddr: Integer);
begin
  Self.inputs.Clear();
  for var i := 0 to Integer(RCSi.GetModuleInputsCountSafe(rcsAddr))-1 do
    Self.inputs.Add(TIOGUI.Create(Self.P_Inputs, i, nil));
  Self.FormResize(Self);
end;

procedure TF_Tester.CreateOutputs(rcsAddr: Integer);
begin
  Self.outputs.Clear();
  for var i := 0 to Integer(RCSi.GetModuleOutputsCountSafe(rcsAddr))-1 do
    Self.outputs.Add(TIOGUI.Create(Self.P_Outputs, i, Self.SOutputMouseUp));
  Self.FormResize(Self);
end;

procedure TF_Tester.SOutputMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var oldColor: TColor;
begin
  if (Self.RCSAddrOutputs < 0) then
    Exit();
  oldColor := (Sender as TShape).Brush.Color;
  (Sender as TShape).Brush.Color := clYellow;

  try
    if (oldColor = clRed) then
    begin
      RCSi.SetOutput(Self.RCSAddrOutputs, (Sender as TShape).Tag, 1);
    end else if (oldColor = clBlue) then
    begin
      RCSi.SetOutput(Self.RCSAddrOutputs, (Sender as TShape).Tag, Integer(ncVse));
    end else begin
      RCSi.SetOutput(Self.RCSAddrOutputs, (Sender as TShape).Tag, 0);
    end;
  except
    on E: Exception do
      ExceptionMessageBox('Nelze nastavít výstup:', E);
  end;
end;

procedure TF_Tester.FillRCSAddrs();
begin
  Self.CB_RCSAdrData.Clear();
  Self.CB_Addr.Clear();
  Self.CB_AddrIn.Clear();
  Self.CB_AddrOut.Clear();

  for var i := 0 to RCSi.maxModuleAddr do
  begin
    try
      if (not RCSi.IsModule(i)) then
        continue;
    except
      continue;
    end;

    Self.CB_RCSAdrData.Add(i);
    var name: string := '-';

    try
      name := RCSi.GetModuleName(i);
    except
      name := IntToStr(i);
    end;

    var addrAndName: string := IntToStr(i) + ': ' + name;
    Self.CB_Addr.Items.Add(addrAndName);
    Self.CB_AddrIn.Items.Add(addrAndName);
    Self.CB_AddrOut.Items.Add(addrAndName);
  end;

  Self.CB_Addr.ItemIndex := -1;
  Self.CB_AddrChange(Self);
end;

procedure TF_Tester.AfterRCSOpen();
begin
  Self.FillRCSAddrs();
end;

procedure TF_Tester.RCSModuleChanged(addr: Cardinal);
begin
  if ((Integer(addr) = Self.RCSAddr) and (Self.CB_Addr.ItemIndex > -1)) then
  begin
    try
      Self.CB_Addr.Text := IntToStr(addr) + ': ' + RCSi.GetModuleName(addr);
    except
      Self.CB_Addr.Text := IntToStr(addr) + ': -';
    end;
  end;
  if ((Integer(addr) = Self.RCSAddrInputs) and (Self.CB_AddrIn.ItemIndex > -1)) then
  begin
    Self.UpdateState();
    Self.UpdateInputs();
    try
      Self.CB_AddrIn.Text := IntToStr(addr) + ': ' + RCSi.GetModuleName(addr);
    except
      Self.CB_AddrIn.Text := IntToStr(addr) + ': -';
    end;
  end;
  if ((Integer(addr) = Self.RCSAddrOutputs) and (Self.CB_AddrOut.ItemIndex > -1)) then
  begin
    Self.UpdateState();
    Self.UpdateOutputs();
    try
      Self.CB_AddrOut.Text := IntToStr(addr) + ': ' + RCSi.GetModuleName(addr);
    except
      Self.CB_AddrOut.Text := IntToStr(addr) + ': -';
    end;
  end;

  begin
    var i: Integer;
    if (not Self.CB_RCSAdrData.BinarySearch(addr, i)) then
    begin
      // address not in addrs list -> add
      var name: string := '-';
      try
        name := RCSi.GetModuleName(addr);
      except

      end;

      var addrAndName: string := IntToStr(addr) + ': ' + name;

      Self.CB_Addr.Items.Insert(i, addrAndName);
      Self.CB_AddrIn.Items.Insert(i, addrAndName);
      Self.CB_AddrOut.Items.Insert(i, addrAndName);
      Self.CB_RCSAdrData.Insert(i, addr);
    end;
  end;
end;

procedure TF_Tester.RCSModuleInputsChanged(addr: Cardinal);
begin
  if (Integer(addr) = Self.RCSAddrInputs) then
    Self.UpdateInputs();
end;

procedure TF_Tester.RCSModuleOutputsChanged(addr: Cardinal);
begin
  if (Integer(addr) = Self.RCSAddrOutputs) then
    Self.UpdateOutputs();
end;

end.
