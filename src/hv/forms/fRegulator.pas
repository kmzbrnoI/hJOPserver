unit fRegulator;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, THnaciVozidlo;

type
  TF_DigiReg = class(TForm)
    CHB_Lights: TCheckBox;
    CHB_f1: TCheckBox;
    CHB_f2: TCheckBox;
    CHB_f4: TCheckBox;
    CHB_f3: TCheckBox;
    CHB_f5: TCheckBox;
    CHB_f6: TCheckBox;
    CHB_f8: TCheckBox;
    CHB_f7: TCheckBox;
    Label5: TLabel;
    RG_Smer: TRadioGroup;
    Label6: TLabel;
    B_Acquire: TButton;
    B_Release: TButton;
    B_STOP: TButton;
    Label7: TLabel;
    Label8: TLabel;
    CHB_Manual: TCheckBox;
    CHB_f9: TCheckBox;
    CHB_f10: TCheckBox;
    CHB_f12: TCheckBox;
    CHB_f11: TCheckBox;
    L_address: TLabel;
    L_mine: TLabel;
    L_stupen: TLabel;
    L_speed: TLabel;
    Label1: TLabel;
    B_Idle: TButton;
    S_Status: TShape;
    T_Speed: TTimer;
    L_ComStatus: TLabel;
    Label2: TLabel;
    L_POM: TLabel;
    TB_reg: TTrackBar;
    procedure CHB_LightsClick(Sender: TObject);
    procedure B_AcquireClick(Sender: TObject);
    procedure B_ReleaseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_STOPClick(Sender: TObject);
    procedure RG_SmerClick(Sender: TObject);
    procedure B_IdleClick(Sender: TObject);
    procedure S_StatusMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure T_SpeedTimer(Sender: TObject);
    procedure CHB_ManualClick(Sender: TObject);
  private

    speed: Integer;

    procedure SetElemntsState(state: Boolean);
    procedure Acquired(Sender: TObject; data: Pointer);
    procedure AcquireFailed(Sender: TObject; data: Pointer);

  public
    OpenHV: THV;

    procedure OpenForm(HV: THV);
    procedure LocoChanged(Sender: TObject);
    procedure MyKeyPress(key: Integer; var handled: Boolean);
  end;

  /// ////////////////////////////////////////////////////////////////////////////

  ERCMaxWindows = class(Exception);

  TRegulatorCollector = class
  private const
    _MAX_FORMS = 4;

  private
    Forms: record
      data: array [0 .. _MAX_FORMS - 1] of TF_DigiReg;
    end;

    function GetForm(addr: Word): TF_DigiReg;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure Open(HV: THV);

    procedure LocoChanged(Sender: TObject; addr: Word);
    function IsLoko(HV: THV): Boolean;

    procedure KeyPress(key: Integer; var handled: Boolean);

    procedure CloseAll();

  end;

var
  RegCollector: TRegulatorCollector;

implementation

{$R *.dfm}

uses fMain, TrakceC, ownConvert, ownGuiUtils;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.CHB_ManualClick(Sender: TObject);
begin
  try
    if (Self.OpenHV <> nil) then
      Self.OpenHV.manual := Self.CHB_Manual.Checked;
  except
    on E: Exception do
      ExceptionMessageBox('Nepodařilo se nastavit RUČ:', E);
  end;
end;

procedure TF_DigiReg.CHB_LightsClick(Sender: TObject);
begin
  OpenHV.SetSingleFunc(TCheckBox(Sender).Tag, TCheckBox(Sender).Checked, TTrakce.Callback(), TTrakce.Callback(), Self);
end;

procedure TF_DigiReg.OpenForm(HV: THV);
begin
  Self.CHB_Manual.Checked := HV.manual;
  Self.OpenHV := HV;
  Self.LocoChanged(nil);
  Self.T_Speed.Enabled := true;

  Self.Show();
  if (Self.TB_reg.Enabled) then
    Self.TB_reg.SetFocus();
end;

procedure TF_DigiReg.B_AcquireClick(Sender: TObject);
begin
  Self.B_Acquire.Enabled := false;
  Self.OpenHV.TrakceAcquire(TTrakce.Callback(Self.Acquired), TTrakce.Callback(Self.AcquireFailed));
end;

procedure TF_DigiReg.B_IdleClick(Sender: TObject);
begin
  Self.TB_reg.Position := 0;
  Self.T_SpeedTimer(Self);
end;

procedure TF_DigiReg.B_ReleaseClick(Sender: TObject);
begin
  Self.B_Release.Enabled := false;
  Self.OpenHV.TrakceRelease(TTrakce.Callback());
end;

procedure TF_DigiReg.FormCreate(Sender: TObject);
begin
  Self.OpenHV := nil;
end;

procedure TF_DigiReg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var tmp: THV;
begin
  if (Self.OpenHV <> nil) then
  begin
    tmp := Self.OpenHV;
    Self.OpenHV := nil;

    try
      if ((tmp.Acquired) and (tmp.state.regulators.Count = 0)) then
      begin
        tmp.manual := false;
        tmp.CheckRelease();
      end;
    except
      on E: Exception do
      begin
        Self.OpenHV := tmp;
        ExceptionMessageBox('Lokomotivu se nepodařilo odhlásit:', E);
      end;
    end;
  end;

  Self.T_Speed.Enabled := false;
end;

procedure TF_DigiReg.Acquired(Sender: TObject; data: Pointer);
begin
  Self.TB_reg.SetFocus();
end;

procedure TF_DigiReg.AcquireFailed(Sender: TObject; data: Pointer);
begin
  Self.B_Acquire.Enabled := true;
  ErrorMessageBox('Převezetí lokomotivy se nedařilo!');
end;

procedure TF_DigiReg.B_STOPClick(Sender: TObject);
begin
  Self.OpenHV.EmergencyStop(TTrakce.Callback(), TTrakce.Callback(), Self);
end;

procedure TF_DigiReg.RG_SmerClick(Sender: TObject);
begin
  Self.speed := -1;
  Self.T_SpeedTimer(Self);
end;

procedure TF_DigiReg.LocoChanged(Sender: TObject);
begin
  Self.SetElemntsState(((Self.OpenHV.Acquired) and ((Self.OpenHV.pom = TPomStatus.automat) or (Self.OpenHV.pom = TPomStatus.manual))));

  if (Self.OpenHV.Acquired) then
  begin
    if (Self.OpenHV.trakceError) then
    begin
      Self.L_ComStatus.Font.Color := clRed;
      Self.L_ComStatus.Caption := 'loko NEKOMUNIKUJE';
    end else begin
      Self.L_ComStatus.Font.Color := clGreen;
      Self.L_ComStatus.Caption := 'loko KOMUNIKUJE';
    end;
  end else begin
    Self.L_ComStatus.Font.Color := clSIlver;
    Self.L_ComStatus.Caption := 'loko odhlášeno';
  end;

  if (Sender <> Self) then
    TB_reg.Position := OpenHV.speedStep;

  Self.L_stupen.Caption := IntToStr(OpenHV.speedStep) + ' / ' + IntToStr(OpenHV.slot.maxSpeed);
  Self.L_speed.Caption := IntToStr(OpenHV.realSpeed);

  RG_Smer.ItemIndex := Integer(OpenHV.direction);
  Self.L_address.Caption := IntToStr(OpenHV.addr);
  Self.Caption := OpenHV.name + ' (' + OpenHV.data.designation + ') : ' + IntToStr(OpenHV.addr);
  Self.B_Acquire.Enabled := not OpenHV.Acquired or OpenHV.state.trakceError;
  Self.B_Release.Enabled := OpenHV.Acquired;
  Self.CHB_Manual.Checked := OpenHV.manual;
  Self.L_mine.Caption := ownConvert.BoolToYesNo(OpenHV.Acquired);

  if ((OpenHV.Acquired) and ((OpenHV.pom = TPomStatus.automat) or (OpenHV.pom = TPomStatus.manual))) then
  begin
    Self.S_Status.Brush.Color := clGreen;
  end else begin
    if ((OpenHV.stolen) or (OpenHV.pom = progr) or (OpenHV.acquiring)) then
    begin
      Self.S_Status.Brush.Color := clYellow;
    end else begin
      Self.S_Status.Brush.Color := clRed;
    end;
  end;

  case (OpenHV.pom) of
    TPomStatus.unknown:
      Self.L_POM.Caption := '?';
    TPomStatus.progr:
      Self.L_POM.Caption := 'progr';
    TPomStatus.error:
      Self.L_POM.Caption := 'error';
    TPomStatus.automat:
      Self.L_POM.Caption := 'automat';
    TPomStatus.manual:
      Self.L_POM.Caption := 'ruční';
  end;

  if (Sender <> Self) then
  begin
    var functions: TFunctions := OpenHV.slotFunctions;
    Self.CHB_Lights.Checked := functions[0];
    Self.CHB_f1.Checked := functions[1];
    Self.CHB_f2.Checked := functions[2];
    Self.CHB_f3.Checked := functions[3];
    Self.CHB_f4.Checked := functions[4];
    Self.CHB_f5.Checked := functions[5];
    Self.CHB_f6.Checked := functions[6];
    Self.CHB_f7.Checked := functions[7];
    Self.CHB_f8.Checked := functions[8];
    Self.CHB_f9.Checked := functions[9];
    Self.CHB_f10.Checked := functions[10];
    Self.CHB_f11.Checked := functions[11];
    Self.CHB_f12.Checked := functions[12];
  end;
end;

procedure TF_DigiReg.SetElemntsState(state: Boolean);
begin
  Self.TB_reg.Enabled := state;
  Self.RG_Smer.Enabled := state;
  Self.B_STOP.Enabled := state;
  Self.B_Idle.Enabled := state;
  Self.CHB_Lights.Enabled := state;
  Self.CHB_f1.Enabled := state;
  Self.CHB_f2.Enabled := state;
  Self.CHB_f3.Enabled := state;
  Self.CHB_f4.Enabled := state;
  Self.CHB_f5.Enabled := state;
  Self.CHB_f6.Enabled := state;
  Self.CHB_f7.Enabled := state;
  Self.CHB_f8.Enabled := state;
  Self.CHB_f9.Enabled := state;
  Self.CHB_f10.Enabled := state;
  Self.CHB_f11.Enabled := state;
  Self.CHB_f12.Enabled := state;
  Self.CHB_Manual.Enabled := state;
end;

procedure TF_DigiReg.S_StatusMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Self.B_Acquire.Enabled) then
    Self.B_AcquireClick(Self);
end;

procedure TF_DigiReg.T_SpeedTimer(Sender: TObject);
begin
  if (Self.OpenHV = nil) then
    Exit();
  if (Self.speed = Self.TB_reg.Position) then
    Exit();

  OpenHV.SetSpeedStepDir(TB_reg.Position, RG_Smer.ItemIndex = 1, TTrakce.Callback(), TTrakce.Callback(), Self);

  Self.L_stupen.Caption := IntToStr(TB_reg.Position) + ' / ' + IntToStr(OpenHV.slot.maxSpeed);
  Self.L_speed.Caption := IntToStr(OpenHV.realSpeed);
  Self.speed := Self.TB_reg.Position;
end;

// vyvola se, pokud je me okynko aktivni a je nad nim stiskla klavesa
procedure TF_DigiReg.MyKeyPress(key: Integer; var handled: Boolean);
begin
  if (not Self.OpenHV.Acquired) then
  begin
    if (key = VK_RETURN) then
      if (Self.ActiveControl <> Self.B_Acquire) then
        Self.B_AcquireClick(Self);
    Exit();
  end;

  handled := true;
  case (key) of
    VK_NUMPAD0:
      Self.CHB_Lights.Checked := not Self.CHB_Lights.Checked;
    VK_NUMPAD1:
      Self.CHB_f1.Checked := not Self.CHB_f1.Checked;
    VK_NUMPAD2:
      Self.CHB_f2.Checked := not Self.CHB_f2.Checked;
    VK_NUMPAD3:
      Self.CHB_f3.Checked := not Self.CHB_f3.Checked;
    VK_NUMPAD4:
      Self.CHB_f4.Checked := not Self.CHB_f4.Checked;
    VK_NUMPAD5:
      Self.CHB_f5.Checked := not Self.CHB_f5.Checked;
    VK_NUMPAD6:
      Self.CHB_f6.Checked := not Self.CHB_f6.Checked;
    VK_NUMPAD7:
      Self.CHB_f7.Checked := not Self.CHB_f7.Checked;
    VK_NUMPAD8:
      Self.CHB_f8.Checked := not Self.CHB_f8.Checked;
    VK_NUMPAD9:
      Self.CHB_f9.Checked := not Self.CHB_f9.Checked;

    VK_ADD:
      Self.RG_Smer.ItemIndex := 0;
    VK_SUBTRACT:
      Self.RG_Smer.ItemIndex := 1;

    83:
      Self.B_STOPClick(Self); // 's'
    73:
      Self.B_IdleClick(Self); // 'i'
  else
    handled := false;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRegulatorCollector.Create();
begin
  for var i := 0 to Self._MAX_FORMS - 1 do
    Self.Forms.data[i] := TF_DigiReg.Create(nil);
end;

destructor TRegulatorCollector.Destroy();
begin
  for var i := 0 to Self._MAX_FORMS - 1 do
    if (Assigned(Self.Forms.data[i])) then
      FreeAndNil(Self.Forms.data[i]);
end;

procedure TRegulatorCollector.LocoChanged(Sender: TObject; addr: Word);
var frm: TF_DigiReg;
begin
  frm := Self.GetForm(addr);
  if (frm = nil) then
    Exit();
  frm.LocoChanged(Sender);
end;

procedure TRegulatorCollector.Open(HV: THV);
begin
  for var i := 0 to Self._MAX_FORMS - 1 do
  begin
    if ((Self.Forms.data[i].Showing) and (Self.Forms.data[i].OpenHV = HV)) then
    begin
      Self.Forms.data[i].SetFocus;
      Exit();
    end;
  end;

  var i: Integer := 0;
  for i := 0 to Self._MAX_FORMS - 1 do
    if (not Self.Forms.data[i].Showing) then
      break;

  if (i = Self._MAX_FORMS) then
    raise ERCMaxWindows.Create('Otevřen maximální počet oken regulátorů!');

  Self.Forms.data[i].OpenForm(HV);
end;

function TRegulatorCollector.GetForm(addr: Word): TF_DigiReg;
begin
  Result := nil;
  for var i := 0 to Self._MAX_FORMS - 1 do
  begin
    if (Self.Forms.data[i].OpenHV = nil) then
      continue;
    if (Self.Forms.data[i].OpenHV.addr = addr) then
      Exit(Self.Forms.data[i]);
  end; // for
end;

procedure TRegulatorCollector.CloseAll();
begin
  for var i := 0 to Self._MAX_FORMS - 1 do
    Self.Forms.data[i].Close;
end;

procedure TRegulatorCollector.KeyPress(key: Integer; var handled: Boolean);
begin
  if (handled) then
    Exit();

  for var i := 0 to _MAX_FORMS - 1 do
  begin
    if ((Self.Forms.data[i] <> nil) and (Self.Forms.data[i].Active)) then
    begin
      Self.Forms.data[i].MyKeyPress(key, handled);
      Exit();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRegulatorCollector.IsLoko(HV: THV): Boolean;
begin
  if (Self = nil) then
    Exit(false);

  for var i := 0 to _MAX_FORMS - 1 do
    if ((Self.Forms.data[i] <> nil) and (Self.Forms.data[i].OpenHV = HV)) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

RegCollector := TRegulatorCollector.Create();

finalization

FreeAndNil(RegCollector);

end.// unit
