unit fTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, Spin, ExtCtrls, Buttons, ColorGrd,
  ComCtrls, TabNotBk, Gauges, fMain, TBloky;

type
  TF_Tester = class(TForm)
    T_tester: TTimer;
    GB_vstupy: TGroupBox;
    L_1: TLabel;
    CB_MtbAdr: TComboBox;
    CHB_LogZmeny: TCheckBox;
    GB_Change: TGroupBox;
    LB_Changes: TListBox;
    B_Clear: TButton;
    GB_vystupy: TGroupBox;
    procedure T_testerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SE_OutMtbChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CB_MtbAdrChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_ClearClick(Sender: TObject);
  private const
    _S_TOP    = 18;
    _S_HEIGHT = 12;
    _S_WIDTH  = 25;
    _S_INCR   = 15;
    _S_LEFT   = 10;
    _L_LEFT   = 40;

  private
   MTBAddr:Smallint;
   CB_MTBAdrData:TArI;
   SInput:array [0..15] of TShape;
   SOutput:array [0..15] of TShape;   

   LInput:array [0..15] of TLabel;
   LOutput:array [0..15] of TLabel;

    procedure CreateSInput;
    procedure CreateSOutput;
    procedure SOutputMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
   procedure UpdateOut;
   procedure UpdateIn;

   procedure AfterMTBOpen();

  end;

var
  F_Tester: TF_Tester;

implementation

uses TechnologieRCS, Logging, RCS;

{$R *.dfm}

procedure TF_Tester.T_testerTimer(Sender: TObject);
 begin
  UpdateOut;
  UpdateIn;
 end;//procedure

procedure TF_Tester.FormShow(Sender: TObject);
var cyklus:Integer;
 begin
  for cyklus := 0 to 15 do
   begin
    SInput[cyklus].Brush.Color  := clGray;
    SOutput[cyklus].Brush.Color := clGray;
   end;//for cyklus

  T_tester.Enabled := true; 
  writelog('Zobrazeno okno Testeru',0,0);
 end;//procedure

procedure TF_Tester.UpdateOut;
var i, val:Integer;
    outCnt:Cardinal;
 begin
  if ((not RCSi.NoExStarted()) or (MTBAddr < 0) or (RCSi.IsModuleFailure(MTBAddr))) then
   begin
    for i := 0 to 15 do
     begin
      SOutput[i].Brush.Color := clGray;
      SOutput[i].Visible := true;
     end;
    Exit();
   end;

  try
    outCnt := RCSi.GetModuleOutputsCount(MTBAddr);
  except
    outCnt := 16;
  end;

  for i := 0 to 15 do
   begin
    SOutput[i].Visible := (i < Integer(outCnt));
    LOutput[i].Visible := (i < Integer(outCnt));

    if (i < Integer(outCnt)) then
     begin
      try
        val := RCSi.GetOutput(MTBAddr, i);
      except
        val := -1;
      end;

      if (val < 0) then
        SOutput[i].Brush.Color := clGray
      else begin
        if (RCSi.GetOutputType(MtbAddr, i) = TRCSOPortType.optSCom) then
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
   end;//for i
 end;//procedure

procedure TF_Tester.SE_OutMtbChange(Sender: TObject);
 begin
  UpdateOut;
 end;//procedure

procedure TF_Tester.UpdateIn;
var i:Integer;
    InputState:TRCSInputState;
    LastState:TRCSInputState;
    stateStr:string;
    inCnt:Cardinal;
 begin
  if ((not RCSi.NoExStarted()) or (MTBAddr < 0) or (RCSi.IsModuleFailure(MTBAddr))) then
   begin
    for i := 0 to 15 do
     begin
      SInput[i].Brush.Color := clGray;
      SInput[i].Visible := true;
     end;
    Exit();
   end;

  try
    inCnt := RCSi.GetModuleInputsCount(MTBAddr);
  except
    inCnt := 16;
  end;

  for i := 0 to 15 do
   begin
    SInput[i].Visible := (i < Integer(inCnt));
    LInput[i].Visible := (i < Integer(inCnt));

    if (i < Integer(inCnt)) then
     begin
      try
        InputState := RCSi.GetInput(MTBAddr, i);
      except
        InputState := failure;
      end;

      if (F_Tester.CHB_LogZmeny.Checked) then
       begin
        case (SInput[i].Brush.Color) of
         clRed :LastState := isOff;
         clLime:LastState := isOn;
         clGray:LastState := failure;
         clSilver:LastState := notYetScanned;
        else
         LastState := failure;
        end;

        if (InputState <> LastState) then
         begin
          case (InputState) of
           isOff         : stateStr := '0';
           isOn          : stateStr := '1';
           failure       : stateStr := 'F';
           notYetScanned : stateStr := '?';
          end;

          F_Tester.LB_Changes.Items.Add('Zmena:: MTB:'+Format('%3d' ,[MTBAddr])+', port: '+Format('%2d' ,[i])+', state:'+stateStr);
          Beep;
         end;//if (InputState <> LastState)
       end;//if F_Tester.CHB_LogZmeny.Checked

      if (InputState = isOn) then
        SInput[i].Brush.Color := clLime
      else if (InputState = isOff) then
        SInput[i].Brush.Color := clRed
      else if (InputState = notYetScanned) then
        SInput[i].Brush.Color := clSilver
      else
        SInput[i].Brush.Color := clGray;
     end;//if i < inCnt
   end;//for i
 end;//procedure

procedure TF_Tester.FormCreate(Sender: TObject);
 begin
  SetLength(Self.CB_MTBAdrData, TRCS._MAX_RCS);  // pole indexu vytvorime tak, aby bylo co nejvetsi
  Self.MTBAddr := -1;
  Self.CreateSInput;
  Self.CreateSOutput;
 end;//procedure

procedure TF_Tester.CB_MtbAdrChange(Sender: TObject);
var i:Integer;
 begin
  for i := 0 to 15 do
   begin
    SInput[i].Brush.Color  := clGray;
    SOutput[i].Brush.Color := clGray;
   end;//for i

  if ((CB_MtbAdr.ItemIndex > -1) and (CB_MtbAdr.ItemIndex < Length(CB_MTBAdrData))) then
   begin
    MTBAddr := CB_MtbAdrData[CB_MtbAdr.ItemIndex];
    try
      Self.T_tester.Enabled := RCSi.IsModule(MTBAddr);
    except
     Self.T_tester.Enabled := false;
    end;
   end;
 end;//procedure

procedure TF_Tester.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  T_tester.Enabled := false;
  writelog('Skryto okno Testeru',0,0);
 end;//procedure

procedure TF_Tester.CreateSInput;
var i:Integer;
    aTop:Integer;
    L:TLabel;
 begin
  aTop := _S_TOP;

  for i := 0 to 15 do
   begin
    SInput[i] := TShape.Create(F_Tester.GB_vstupy);

    SInput[i].Parent := F_Tester.GB_vstupy;
    SInput[i].Top    := aTop;
    SInput[i].Left   := _S_LEFT;
    SInput[i].Width  := _S_WIDTH;
    SInput[i].Height := _S_HEIGHT;
    SInput[i].Shape  := stRectangle;
    SInput[i].Tag    := i;
    SInput[i].Brush.Color := clRed;

    L            := TLabel.Create(F_Tester.GB_vstupy);
    L.Parent     := Self.GB_vstupy;
    L.Font.Color := clWhite;
    L.Caption    := IntToStr(i);
    L.Left       := _L_LEFT;
    L.Top        := aTop;
    LInput[i] := L;

    aTop := aTop + _S_INCR;
   end;//for i
 end;//procedure

procedure TF_Tester.CreateSOutput;
var i:Integer;
    aTop:Integer;
    L:TLabel;
 begin
  aTop := _S_TOP;

  for i := 0 to 15 do
   begin
    SOutput[i] := TShape.Create(F_Tester.GB_vystupy);

    SOutput[i].Parent := F_Tester.GB_vystupy;
    SOutput[i].Top    := aTop;
    SOutput[i].Left   := _S_LEFT;
    SOutput[i].Width  := _S_WIDTH;
    SOutput[i].Height := _S_HEIGHT;
    SOutput[i].Shape  := stRectangle;
    SOutput[i].Tag    := i;
    SOutput[i].Brush.Color := clRed;
    SOutput[i].OnMouseUp := SOutputMouseUp;

    L            := TLabel.Create(F_Tester.GB_vystupy);
    L.Parent     := Self.GB_vystupy;
    L.Font.Color := clWhite;
    L.Caption    := IntToStr(i);
    L.Left       := _L_LEFT;
    L.Top        := aTop;
    LOutput[i] := L;

    aTop := aTop + _S_INCR;
   end;//for i
 end;//procedure

procedure TF_Tester.SOutputMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
 begin
  try
    if (MTBAddr < 0) then Exit;

    if ((Sender as TShape).Brush.Color = clRed) then begin
      RCSi.SetOutput(MTBAddr, (Sender as TShape).Tag, 1);
      (Sender as TShape).Brush.Color := clLime;
    end else if ((Sender as TShape).Brush.Color = clBlue) then begin
      RCSi.SetOutput(MTBAddr, (Sender as TShape).Tag, 5);
      (Sender as TShape).Brush.Color := clWhite;
    end else begin
      RCSi.SetOutput(MTBAddr, (Sender as TShape).Tag, 0);
      (Sender as TShape).Brush.Color := clRed;
    end;

    (Sender as TShape).Brush.Color := clYellow;
  except
    // stav modulu se zaktualizuje sam
  end;
 end;//procedure

procedure TF_Tester.B_ClearClick(Sender: TObject);
 begin
  F_Tester.LB_Changes.Clear;
 end;//procedure

procedure TF_Tester.AfterMTBOpen();
var i:Integer;
begin
 Self.CB_MtbAdr.Clear();

 for i := 0 to TRCS._MAX_RCS-1 do
  begin
   try
     if (not RCSi.IsModule(i)) then continue;
   except
     continue;
   end;

   Self.CB_MTBAdrData[Self.CB_MtbAdr.Items.Count] := i;

   try
     Self.CB_MtbAdr.Items.Add(IntToStr(i) + ' : ' + RCSi.GetModuleName(i));
   except
     Self.CB_MtbAdr.Items.Add(IntToStr(i) + ' : -');
   end;
  end;//for i

 Self.CB_MtbAdr.ItemIndex := -1;
 Self.CB_MtbAdrChange(self);
end;//procedure

end.//unit
