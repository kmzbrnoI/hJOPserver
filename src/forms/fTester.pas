﻿unit fTester;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, Buttons, ComCtrls, fMain, BlockDb,
  Generics.Collections, UITypes;

type
  TF_Tester = class(TForm)
    GB_vstupy: TGroupBox;
    L_1: TLabel;
    CB_RCSAdr: TComboBox;
    CHB_LogZmeny: TCheckBox;
    GB_Change: TGroupBox;
    LB_Changes: TListBox;
    B_Clear: TButton;
    GB_vystupy: TGroupBox;
    L_state: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CB_RCSAdrChange(Sender: TObject);
    procedure B_ClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private const
    _S_TOP = 18;
    _S_HEIGHT = 12;
    _S_WIDTH = 25;
    _S_INCR = 15;
    _S_LEFT = 10;
    _L_LEFT = 40;
    _NO_INPUTS = 16;
    _NO_OUTPUTS = 16;

  private
    RCSAddr: Integer;
    CB_RCSAdrData: TList<Cardinal>; // always sorted!
    SInput: array [0 .. _NO_INPUTS - 1] of TShape;
    SOutput: array [0 .. _NO_OUTPUTS - 1] of TShape;

    LInput: array [0 .. _NO_INPUTS - 1] of TLabel;
    LOutput: array [0 .. _NO_OUTPUTS - 1] of TLabel;

    procedure CreateSInput();
    procedure CreateSOutput();
    procedure SOutputMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FillRCSAddrs();

  public
    procedure UpdateOut();
    procedure UpdateIn();
    procedure UpdateState();

    procedure AfterRCSOpen();
    procedure RCSModuleChanged(addr: Cardinal);
    procedure RCSModuleInputsChanged(addr: Cardinal);
    procedure RCSModuleOutputsChanged(addr: Cardinal);

  end;

var
  F_Tester: TF_Tester;

implementation

uses TechnologieRCS, Logging, RCS, BlockSignal, RCSErrors;

{$R *.dfm}

procedure TF_Tester.UpdateOut();
var val: Integer;
  outCnt: Cardinal;
begin
  if ((not RCSi.NoExStarted()) or (RCSAddr < 0) or (RCSi.IsModuleFailure(RCSAddr))) then
  begin
    for var i := 0 to _NO_OUTPUTS - 1 do
    begin
      SOutput[i].Brush.Color := clGray;
      SOutput[i].Visible := true;
    end;
    Exit();
  end;

  try
    outCnt := RCSi.GetModuleOutputsCount(RCSAddr);
  except
    outCnt := _NO_OUTPUTS;
  end;

  for var i := 0 to _NO_OUTPUTS - 1 do
  begin
    SOutput[i].Visible := (i < Integer(outCnt));
    LOutput[i].Visible := (i < Integer(outCnt));

    if (i < Integer(outCnt)) then
    begin
      try
        val := RCSi.GetOutput(RCSAddr, i);
        if (val >= RCS_ERRORS_START) then
          val := -1;
      except
        val := -1;
      end;

      if (val < 0) then
        SOutput[i].Brush.Color := clGray
      else
      begin
        if (RCSi.GetOutputType(RCSAddr, i) = TRCSOPortType.optSCom) then
        begin
          if (val = 0) then
            SOutput[i].Brush.Color := clBlue
          else
            SOutput[i].Brush.Color := clWhite;
        end else begin
          if (val = 0) then
            SOutput[i].Brush.Color := clRed
          else
            SOutput[i].Brush.Color := clLime;
        end;
      end;
    end;
  end; // for i
end;

procedure TF_Tester.UpdateIn();
var InputState: TRCSInputState;
  LastState: TRCSInputState;
  stateStr: string;
  inCnt: Cardinal;
begin
  if ((not RCSi.NoExStarted()) or (RCSAddr < 0) or (RCSi.IsModuleFailure(RCSAddr))) then
  begin
    for var i := 0 to _NO_INPUTS - 1 do
    begin
      SInput[i].Brush.Color := clGray;
      SInput[i].Visible := true;
    end;
    Exit();
  end;

  try
    inCnt := RCSi.GetModuleInputsCount(RCSAddr);
  except
    inCnt := _NO_INPUTS;
  end;

  for var i := 0 to _NO_INPUTS - 1 do
  begin
    SInput[i].Visible := (i < Integer(inCnt));
    LInput[i].Visible := (i < Integer(inCnt));

    if (i < Integer(inCnt)) then
    begin
      try
        InputState := RCSi.GetInput(RCSAddr, i);
      except
        InputState := failure;
      end;

      if (Self.CHB_LogZmeny.Checked) then
      begin
        case (SInput[i].Brush.Color) of
          clRed:
            LastState := isOff;
          clLime:
            LastState := isOn;
          clGray:
            LastState := failure;
          clSilver:
            LastState := notYetScanned;
        else
          LastState := failure;
        end;

        if (InputState <> LastState) then
        begin
          case (InputState) of
            isOff:
              stateStr := '0';
            isOn:
              stateStr := '1';
            failure:
              stateStr := 'F';
            notYetScanned:
              stateStr := '?';
          end;

          Self.LB_Changes.Items.Add('Změna:: RCS:' + Format('%3d', [RCSAddr]) + ', port: ' + Format('%2d', [i]) +
            ', state:' + stateStr);
          Beep;
        end; // if (InputState <> LastState)
      end; // if F_Tester.CHB_LogZmeny.Checked

      if (InputState = isOn) then
        SInput[i].Brush.Color := clLime
      else if (InputState = isOff) then
        SInput[i].Brush.Color := clRed
      else if (InputState = notYetScanned) then
        SInput[i].Brush.Color := clSilver
      else
        SInput[i].Brush.Color := clGray;
    end; // if i < inCnt
  end; // for i
end;

procedure TF_Tester.UpdateState();
begin
  if (Self.RCSAddr < 0) then
  begin
    Self.L_state.Caption := '-';
    Self.L_state.Font.Color := clBlack;
    Exit();
  end;

  try
    if (RCSi.IsModuleFailure(Self.RCSAddr)) then begin
      Self.L_state.Caption := 'fail';
      Self.L_state.Font.Color := clRed;
    end else if (RCSi.IsModuleError(Self.RCSAddr)) then begin
      Self.L_state.Caption := 'error';
      Self.L_state.Font.Color := clRed;
    end else if (RCSi.IsModuleWarning(Self.RCSAddr)) then begin
      Self.L_state.Caption := 'warning';
      Self.L_state.Font.Color := clOlive;
    end else begin
      Self.L_state.Caption := 'ok';
      Self.L_state.Font.Color := clGreen;
    end;
  except
    Self.L_state.Caption := 'exc';
    Self.L_state.Font.Color := clRed;
  end;
end;

procedure TF_Tester.FormCreate(Sender: TObject);
begin
  Self.CB_RCSAdrData := TList<Cardinal>.Create();
  Self.RCSAddr := -1;
  Self.CreateSInput();
  Self.CreateSOutput();
end;

procedure TF_Tester.FormDestroy(Sender: TObject);
begin
  Self.CB_RCSAdrData.Free();
end;

procedure TF_Tester.CB_RCSAdrChange(Sender: TObject);
begin
  if ((CB_RCSAdr.ItemIndex > -1) and (CB_RCSAdr.ItemIndex < CB_RCSAdrData.Count)) then
    RCSAddr := CB_RCSAdrData[CB_RCSAdr.ItemIndex]
  else
    RCSAddr := -1;

  Self.UpdateOut();
  Self.UpdateIn();
  Self.UpdateState();
end;

procedure TF_Tester.CreateSInput();
var aTop: Integer;
begin
  aTop := _S_TOP;

  for var i := 0 to _NO_INPUTS - 1 do
  begin
    SInput[i] := TShape.Create(Self.GB_vstupy);

    SInput[i].Parent := Self.GB_vstupy;
    SInput[i].Top := aTop;
    SInput[i].Left := _S_LEFT;
    SInput[i].Width := _S_WIDTH;
    SInput[i].Height := _S_HEIGHT;
    SInput[i].Shape := stRectangle;
    SInput[i].Tag := i;
    SInput[i].Brush.Color := clRed;

    var L := TLabel.Create(Self.GB_vstupy);
    L.Parent := Self.GB_vstupy;
    L.Caption := IntToStr(i);
    L.Left := _L_LEFT;
    L.Top := aTop;
    LInput[i] := L;

    aTop := aTop + _S_INCR;
  end;
end;

procedure TF_Tester.CreateSOutput();
var aTop: Integer;
begin
  aTop := _S_TOP;

  for var i := 0 to _NO_OUTPUTS - 1 do
  begin
    SOutput[i] := TShape.Create(Self.GB_vystupy);

    SOutput[i].Parent := Self.GB_vystupy;
    SOutput[i].Top := aTop;
    SOutput[i].Left := _S_LEFT;
    SOutput[i].Width := _S_WIDTH;
    SOutput[i].Height := _S_HEIGHT;
    SOutput[i].Shape := stRectangle;
    SOutput[i].Tag := i;
    SOutput[i].Brush.Color := clRed;
    SOutput[i].OnMouseUp := SOutputMouseUp;

    var L := TLabel.Create(Self.GB_vystupy);
    L.Parent := Self.GB_vystupy;
    L.Caption := IntToStr(i);
    L.Left := _L_LEFT;
    L.Top := aTop;
    LOutput[i] := L;

    aTop := aTop + _S_INCR;
  end; // for i
end;

procedure TF_Tester.SOutputMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var oldColor: TColor;
begin
  if (RCSAddr < 0) then
    Exit();
  oldColor := (Sender as TShape).Brush.Color;
  (Sender as TShape).Brush.Color := clYellow;

  try
    if (oldColor = clRed) then
    begin
      RCSi.SetOutput(RCSAddr, (Sender as TShape).Tag, 1);
    end else if (oldColor = clBlue) then
    begin
      RCSi.SetOutput(RCSAddr, (Sender as TShape).Tag, Integer(ncVse));
    end else begin
      RCSi.SetOutput(RCSAddr, (Sender as TShape).Tag, 0);
    end;
  except
    on E: Exception do
      Application.MessageBox(PChar('Nelze nastavít výstup:' + #13#10 + E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Tester.B_ClearClick(Sender: TObject);
begin
  Self.LB_Changes.Clear();
end;

procedure TF_Tester.FillRCSAddrs();
begin
  Self.CB_RCSAdrData.Clear();
  Self.CB_RCSAdr.Clear();

  for var i := 0 to RCSi.maxModuleAddr do
  begin
    try
      if (not RCSi.IsModule(i)) then
        continue;
    except
      continue;
    end;

    Self.CB_RCSAdrData.Add(i);

    try
      Self.CB_RCSAdr.Items.Add(IntToStr(i) + ': ' + RCSi.GetModuleName(i));
    except
      Self.CB_RCSAdr.Items.Add(IntToStr(i) + ': -');
    end;
  end;

  Self.CB_RCSAdr.ItemIndex := -1;
  Self.CB_RCSAdrChange(Self);
end;

procedure TF_Tester.AfterRCSOpen();
begin
  Self.FillRCSAddrs();
end;

procedure TF_Tester.RCSModuleChanged(addr: Cardinal);
begin
  if (Integer(addr) = Self.RCSAddr) then
  begin
    Self.UpdateOut();
    Self.UpdateIn();
    Self.UpdateState();

    if (Self.CB_RCSAdr.ItemIndex > -1) then
    begin
      try
        Self.CB_RCSAdr.Text := IntToStr(addr) + ': ' + RCSi.GetModuleName(addr);
      except
        Self.CB_RCSAdr.Text := IntToStr(addr) + ': -';
      end;
    end;
  end;

  begin
    var i: Integer;
    if (not Self.CB_RCSAdrData.BinarySearch(addr, i)) then
    begin
      // address not in addrs list -> add
      try
        Self.CB_RCSAdr.Items.Insert(i, IntToStr(addr) + ': ' + RCSi.GetModuleName(addr));
      except
        Self.CB_RCSAdr.Items.Insert(i, IntToStr(addr) + ': -');
      end;

      Self.CB_RCSAdrData.Insert(i, addr);
    end;
  end;
end;

procedure TF_Tester.RCSModuleInputsChanged(addr: Cardinal);
begin
  if (Integer(addr) = Self.RCSAddr) then
    Self.UpdateIn();
end;

procedure TF_Tester.RCSModuleOutputsChanged(addr: Cardinal);
begin
  if (Integer(addr) = Self.RCSAddr) then
    Self.UpdateOut();
end;

end.
