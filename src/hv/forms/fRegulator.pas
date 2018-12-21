unit fRegulator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, THnaciVozidlo;

type
  TF_DigiReg = class(TForm)
    CHB_svetla: TCheckBox;
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
    B_PrevzitLoko: TButton;
    B_OdhlLoko: TButton;
    B_STOP: TButton;
    Label7: TLabel;
    Label8: TLabel;
    CHB_DojezdIgnorate: TCheckBox;
    CHB_f9: TCheckBox;
    CHB_f10: TCheckBox;
    CHB_f12: TCheckBox;
    CHB_f11: TCheckBox;
    P_Speed: TPanel;
    TB_reg: TTrackBar;
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
    procedure CHB_svetlaClick(Sender: TObject);
    procedure B_PrevzitLokoClick(Sender: TObject);
    procedure B_OdhlLokoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure B_STOPClick(Sender: TObject);
    procedure RG_SmerClick(Sender: TObject);
    procedure B_IdleClick(Sender: TObject);
    procedure S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure T_SpeedTimer(Sender: TObject);
    procedure CHB_DojezdIgnorateClick(Sender: TObject);
  private

   speed:Integer;

   procedure SetElemntsState(state:boolean);

   procedure LokoComOK(Sender:TObject; data:Pointer);
   procedure LokoComErr(Sender:TObject; data:Pointer);

  public
   OpenHV:THV;

   procedure OpenForm(HV:THV);

   procedure ConnectChange();
   procedure Stolen();
   procedure UpdateElements();

   procedure MyKeyPress(key:Integer);
  end;

 ///////////////////////////////////////////////////////////////////////////////

 ERCMaxWindows = class(Exception);

 TRegulatorCollector = class
   private const
    _MAX_FORMS = 4;

   private
    forms:record
     data:array [0.._MAX_FORMS-1] of TF_DigiReg;
    end;

     function GetForm(addr:Word):TF_DigiReg;

   public
    constructor Create();
    destructor Destroy(); override;

    procedure Open(HV:THV);

    procedure UpdateElements(Sender:TObject; addr:Word);
    procedure ConnectChange(addr:Word);
    procedure Stolen(addr:Word);
    function IsLoko(HV:THV):boolean;

    procedure KeyPress(key:Integer; var handled:boolean);

    procedure CloseAll();

 end;

var
  RegCollector:TRegulatorCollector;

implementation

{$R *.dfm}

uses fMain, Trakce;

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.CHB_DojezdIgnorateClick(Sender: TObject);
begin
 try
  if (Self.OpenHV <> nil) then
    Self.OpenHV.ruc := Self.CHB_DojezdIgnorate.Checked;
 except
   on E:Exception do
    begin
     Application.MessageBox(PChar('Nepodaøilo se nastavit RUÈ:'+#13#10+E.Message),
         'Varování', MB_OK OR MB_ICONWARNING);
    end;
 end;
end;

procedure TF_DigiReg.CHB_svetlaClick(Sender: TObject);
var func:TFunkce;
 begin
  if (not OpenHV.slot.Prevzato) then Exit;

  TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.LokoComOK);
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.LokoComErr);

  Func := OpenHV.Slot.funkce;
  Func[(Sender as TCheckBox).Tag] := (Sender as TCheckBox).Checked;

  try
    TrkSystem.LokSetFunc(Self, OpenHV, Func);
  except
    Self.LokoComErr(Self, nil);
  end;
 end;//procedure

procedure TF_DigiReg.OpenForm(HV:THV);
 begin
  CHB_DojezdIgnorate.Checked := HV.ruc;
  Self.OpenHV := HV;

  case (TrkSystem.TrkSystem) of
   TTrk_system.TRS_XpressNET:begin
     //u XpressNetu nema smysl odhlasovat loko
     Self.B_OdhlLoko.Visible := false;
   end;
  else
   Self.B_OdhlLoko.Visible := true;
  end;

  Self.UpdateElements();
  Self.SetElemntSState(((Self.OpenHV.Slot.prevzato) and ((Self.OpenHV.Slot.pom = pc) or (Self.OpenHV.Slot.pom = released))));

  if (HV.Slot.prevzato) then
   begin
    if (HV.Slot.com_err) then
      Self.LokoComErr(Self, nil)
     else
      Self.LokoComOK(Self, nil);
   end else begin
    Self.L_ComStatus.Font.Color := clSIlver;
    Self.L_ComStatus.Caption    := 'loko odhlášeno';
   end;

  Self.Show();
  Self.T_Speed.Enabled := true;
 end;//procedure

procedure TF_DigiReg.B_PrevzitLokoClick(Sender: TObject);
begin
 TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.LokoComOK);
 TrkSystem.callback_err := TTrakce.GenerateCallback(Self.LokoComErr);
 try
  TrkSystem.PrevzitLoko(Self.OpenHV);
 except
  on E:Exception do
    Application.MessageBox(PChar('Pøevzetí HV se nezdaøilo !'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
 end;
end;//procedure

procedure TF_DigiReg.B_IdleClick(Sender: TObject);
begin
 Self.TB_reg.Position := 0;
 Self.T_SpeedTimer(Self);
end;

procedure TF_DigiReg.B_OdhlLokoClick(Sender: TObject);
 begin
  TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.LokoComOK);
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.LokoComErr);
  try
   TrkSystem.OdhlasitLoko(Self.OpenHV);
  except
   on E:Exception do
    begin
     Application.MessageBox(PChar('Odhlášení HV se nezdaøilo !'+#13#10+E.Message), 'Chyba', MB_OK OR MB_ICONWARNING);
     Exit();
    end;
  end;

  Self.SetElemntsState(false);
  Self.UpdateElements();
 end;//procedure

procedure TF_DigiReg.FormCreate(Sender: TObject);
 begin
  Self.OpenHV := nil;
 end;

procedure TF_DigiReg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var tmp:THV;
 begin
  if (Self.OpenHV <> nil) then
   begin
    tmp := self.OpenHV;
    Self.OpenHV := nil;

    try
      if ((tmp.Slot.prevzato) and (tmp.Stav.regulators.Count = 0)) then
       begin
        tmp.ruc := false;
        tmp.CheckRelease();
       end;
    except
     on E:Exception do
      begin
       Self.OpenHV := tmp;
       Application.MessageBox(PChar('Lokomotivu se nepodaøilo odhlásit:'+#13#10+E.Message),
           'Varování', MB_OK OR MB_ICONWARNING);
      end;
    end;
   end;

  Self.T_Speed.Enabled := false;
 end;//procedure

procedure TF_DigiReg.B_STOPClick(Sender: TObject);
 begin
  TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.LokoComOK);
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.LokoComErr);

  try
    TrkSystem.EmergencyStopLoko(Self, Self.OpenHV);
  except
    Self.LokoComErr(Self, nil);
  end;

  Self.UpdateElements();
 end;//procedure

procedure TF_DigiReg.RG_SmerClick(Sender: TObject);
 begin
  Self.speed := -1;
  Self.T_SpeedTimer(Self);
 end;//procedure

//zavola se po zadosti o prevezeti, pokud je lokomotiva volna
procedure TF_DigiReg.ConnectChange();
begin
 Self.SetElemntsState(((Self.OpenHV.Slot.prevzato) and ((Self.OpenHV.Slot.pom = pc) or (Self.OpenHV.Slot.pom = released))));
 Self.UpdateElements();
end;//procedure

procedure TF_DigiReg.SetElemntsState(state:boolean);
 begin
  TB_reg.Enabled  := state;
  RG_Smer.Enabled := state;
  B_STOP.Enabled  := state;
  B_idle.Enabled  := state;
  CHB_svetla.Enabled := state;
  CHB_f1.Enabled  := state;
  CHB_f2.Enabled  := state;
  CHB_f3.Enabled  := state;
  CHB_f4.Enabled  := state;
  CHB_f5.Enabled  := state;
  CHB_f6.Enabled  := state;
  CHB_f7.Enabled  := state;
  CHB_f8.Enabled  := state;
  CHB_f9.Enabled  := state;
  CHB_f10.Enabled := state;
  CHB_f11.Enabled := state;
  CHB_f12.Enabled := state;
  CHB_DojezdIgnorate.Enabled := state;
 end;//procedure

procedure TF_DIgiReg.UpdateElements();
var Slot:TSlot;
    data:THVData;
begin
 data := Self.OpenHV.data;
 Slot := Self.OpenHV.Slot;

 TB_reg.Position := Slot.speed;

 Self.L_stupen.Caption := IntToStr(Slot.speed)+' / '+IntToStr(Slot.maxsp);
 Self.L_speed.Caption  := IntToStr(TrkSystem.GetStepSpeed(Slot.speed));

 RG_Smer.ItemIndex      := Slot.Smer;
 Self.L_address.Caption := IntToStr(OpenHV.Adresa);
 Self.Caption           := data.Nazev+' ('+data.Oznaceni+') : '+IntToStr(OpenHV.Adresa);
 B_PrevzitLoko.Enabled  := ((not Slot.prevzato) or (Slot.pom = error));
 B_OdhlLoko.Enabled     := Slot.Prevzato;
 CHB_DojezdIgnorate.Checked := Self.OpenHV.ruc;

 if (Slot.prevzato) then
   Self.L_mine.Caption  := 'ano'
 else
   Self.L_mine.Caption := 'ne';

 if ((Slot.prevzato) and ((Slot.pom = pc) or (Slot.pom = released))) then
  begin
   Self.S_Status.Brush.Color := clLime;
  end else begin
   if ((Slot.stolen) or (Slot.pom = progr)) then
    begin
     Self.S_Status.Brush.Color := clYellow;
    end else begin
     Self.S_Status.Brush.Color := clRed;
    end;
  end;

 case (slot.pom) of
  TPomStatus.progr    : Self.L_POM.Caption := 'progr';
  TPomStatus.error    : Self.L_POM.Caption := 'error';
  TPomStatus.pc       : Self.L_POM.Caption := 'automat';
  TPomStatus.released : Self.L_POM.Caption := 'ruèní';
 end;//case

 CHB_svetla.Checked := Slot.funkce[0];
 CHB_f1.Checked  := Slot.funkce[1];
 CHB_f2.Checked  := Slot.funkce[2];
 CHB_f3.Checked  := Slot.funkce[3];
 CHB_f4.Checked  := Slot.funkce[4];
 CHB_f5.Checked  := Slot.funkce[5];
 CHB_f6.Checked  := Slot.funkce[6];
 CHB_f7.Checked  := Slot.funkce[7];
 CHB_f8.Checked  := Slot.funkce[8];
 CHB_f9.Checked  := Slot.funkce[9];
 CHB_f10.Checked := Slot.funkce[10];
 CHB_f11.Checked := Slot.funkce[11];
 CHB_f12.Checked := Slot.funkce[12];
end;//procedure

procedure TF_DIgiReg.Stolen();
begin
 // nekdo mi ukradl hnaci vozidlo
 Self.SetElemntsState(false);
 Self.UpdateElements();
end;

procedure TF_DigiReg.S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (not Self.OpenHV.Slot.prevzato) then
  Self.B_PrevzitLokoClick(self);
end;

procedure TF_DigiReg.T_SpeedTimer(Sender: TObject);
begin
  if (Self.OpenHV = nil) then Exit();
  if (not OpenHV.slot.Prevzato) then Exit();
  if (Self.speed = Self.TB_reg.Position) then Exit();

  TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.LokoComOK);
  TrkSystem.callback_err := TTrakce.GenerateCallback(Self.LokoComErr);

  try
    TrkSystem.LokSetDirectSpeed(Self, OpenHV, TB_reg.Position, RG_Smer.ItemIndex);
  except
    Self.LokoComErr(Self, nil);
  end;

  Self.L_stupen.Caption := IntToStr(TB_Reg.Position)+' / '+IntToStr(OpenHV.Slot.maxsp);
  Self.L_speed.Caption  := IntToStr(TrkSystem.GetStepSpeed(TB_reg.Position));

  Self.speed := Self.TB_reg.Position;
end;//procedure

// vyvola se, pokud je me okynko aktivni a je nad nim stiskla klavesa
procedure TF_DigiReg.MyKeyPress(key:Integer);
begin
 if (not Self.OpenHV.Slot.prevzato) then
  begin
   if (key = VK_RETURN) then
     if (Self.ActiveControl <> Self.B_PrevzitLoko) then
       Self.B_PrevzitLokoClick(Self);
   Exit;
  end;

 Self.TB_reg.SetFocus();

 case (key) of
  VK_NUMPAD0 : Self.CHB_svetla.Checked := not Self.CHB_svetla.Checked;
  VK_NUMPAD1 : Self.CHB_f1.Checked := not Self.CHB_f1.Checked;
  VK_NUMPAD2 : Self.CHB_f2.Checked := not Self.CHB_f2.Checked;
  VK_NUMPAD3 : Self.CHB_f3.Checked := not Self.CHB_f3.Checked;
  VK_NUMPAD4 : Self.CHB_f4.Checked := not Self.CHB_f4.Checked;
  VK_NUMPAD5 : Self.CHB_f5.Checked := not Self.CHB_f5.Checked;
  VK_NUMPAD6 : Self.CHB_f6.Checked := not Self.CHB_f6.Checked;
  VK_NUMPAD7 : Self.CHB_f7.Checked := not Self.CHB_f7.Checked;
  VK_NUMPAD8 : Self.CHB_f8.Checked := not Self.CHB_f8.Checked;
  VK_NUMPAD9 : Self.CHB_f9.Checked := not Self.CHB_f9.Checked;

  VK_ADD : Self.RG_Smer.ItemIndex := 0;
  VK_SUBTRACT : Self.RG_Smer.ItemIndex := 1;

  83: Self.B_STOPClick(Self);   // 's'
  73: Self.B_IdleClick(Self);   // 'i'
 end;

end;//procedure

procedure TF_DigiReg.LokoComOK(Sender:TObject; data:Pointer);
begin
 Self.L_ComStatus.Font.Color := clLime;
 Self.L_ComStatus.Caption := 'loko KOMUNIKUJE';
end;//procedure

procedure TF_DigiReg.LokoComErr(Sender:TObject; data:Pointer);
begin
 Self.L_ComStatus.Font.Color := clRed;
 Self.L_ComStatus.Caption := 'loko NEKOMUNIKUJE';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

constructor TRegulatorCollector.Create();
var i:Integer;
begin
 for i := 0 to Self._MAX_FORMS-1 do
   Self.forms.data[i] := TF_DigiReg.Create(nil);
end;//ctor

destructor TRegulatorCollector.Destroy();
var i:Integer;
begin
 for i := 0 to Self._MAX_FORMS-1 do
   if (Assigned(Self.forms.data[i])) then
     FreeAndNil(Self.forms.data[i]);
end;//dtor

procedure TRegulatorCollector.UpdateElements(Sender:TObject; addr:Word);
var frm:TF_DigiReg;
begin
 frm := Self.GetForm(addr);
 if ((frm = nil) or (Sender = frm)) then Exit;
 frm.UpdateElements();
end;//procedure

procedure TRegulatorCollector.ConnectChange(addr:Word);
var frm:TF_DigiReg;
begin
 frm := Self.GetForm(addr);
 if (frm = nil) then Exit;
 frm.ConnectChange();
end;//procedure

procedure TRegulatorCollector.Open(HV:THV);
var i:Integer;
begin
 for i := 0 to Self._MAX_FORMS-1 do
   if ((Self.forms.data[i].Showing) and (Self.forms.data[i].OpenHV = HV)) then
    begin
     Self.forms.data[i].SetFocus;
     Exit();
    end;

 for i := 0 to Self._MAX_FORMS-1 do
   if (not Self.forms.data[i].Showing) then
     break;

 if (i = Self._MAX_FORMS) then
   raise ERCMaxWindows.Create('Otevøen maximální poèet oken regulátorù!');

 Self.forms.data[i].OpenForm(HV);
end;//procedure

function TRegulatorCollector.GetForm(addr:Word):TF_DigiReg;
var i:Integer;
begin
 Result := nil;
 for i := 0 to Self._MAX_FORMS-1 do
  begin
   if (Self.forms.data[i].OpenHV = nil) then continue;
   if (Self.forms.data[i].OpenHV.Slot.adresa = addr) then
     Exit(Self.forms.data[i]);
  end;//for
end;//function

procedure TRegulatorCollector.CloseAll();
var i:Integer;
begin
 for i := 0 to Self._MAX_FORMS-1 do
   Self.forms.data[i].Close;
end;//procedure

procedure TRegulatorCollector.Stolen(addr:Word);
var frm:TF_DigiReg;
begin
 frm := Self.GetForm(addr);
 if (frm = nil) then Exit;

 frm.Stolen();
end;//procedure

procedure TRegulatorCollector.KeyPress(key:Integer; var handled:boolean);
var i:Integer;
begin
 if (handled) then Exit;

 for i := 0 to _MAX_FORMS-1 do
  begin
   if ((Self.forms.data[i] <> nil) and (Self.forms.data[i].Active)) then
    begin
     Self.forms.data[i].MyKeyPress(key);
     handled := true;
     Exit;
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TRegulatorCollector.IsLoko(HV:THV):boolean;
var i:Integer;
begin
 for i := 0 to _MAX_FORMS-1 do
   if ((Self.forms.data[i] <> nil) and (Self.forms.data[i].OpenHV = HV)) then
     Exit(true);
 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
  RegCollector := TRegulatorCollector.Create();

finalization
  RegCollector.Free;

end.//unit
