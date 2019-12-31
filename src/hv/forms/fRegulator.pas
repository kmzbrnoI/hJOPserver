﻿unit fRegulator;

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
    CHB_Total: TCheckBox;
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
    procedure CHB_TotalClick(Sender: TObject);
  private

   speed:Integer;

   procedure SetElemntsState(state:boolean);
   procedure AcquireFailed(Sender:TObject; data:Pointer);

  public
   OpenHV:THV;

   procedure OpenForm(HV:THV);

   procedure LocoChanged();
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
    procedure LocoChanged(addr:Word);
    procedure Stolen(addr:Word);
    function IsLoko(HV:THV):boolean;

    procedure KeyPress(key:Integer; var handled:boolean);

    procedure CloseAll();

 end;

var
  RegCollector:TRegulatorCollector;

implementation

{$R *.dfm}

uses fMain, Trakce, Prevody, TechnologieTrakce;

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.CHB_TotalClick(Sender: TObject);
begin
 try
  if (Self.OpenHV <> nil) then
    Self.OpenHV.ruc := Self.CHB_Total.Checked;
 except
   on E:Exception do
    begin
     Application.MessageBox(PChar('Nepodařilo se nastavit RUČ:'+#13#10+E.Message),
         'Varování', MB_OK OR MB_ICONWARNING);
    end;
 end;
end;

procedure TF_DigiReg.CHB_svetlaClick(Sender: TObject);
 begin
  OpenHV.SetSingleFunc(TCheckBox(Sender).Tag, TCheckBox(Sender).Checked,
                       TTrakce.Callback(), TTrakce.Callback());
 end;

procedure TF_DigiReg.OpenForm(HV:THV);
 begin
  CHB_Total.Checked := HV.ruc;
  Self.OpenHV := HV;
  Self.LocoChanged();
  Self.T_Speed.Enabled := true;

  Self.Show();
 end;

procedure TF_DigiReg.B_PrevzitLokoClick(Sender: TObject);
begin
 Self.OpenHV.TrakceAcquire(TTrakce.Callback(), TTrakce.Callback(Self.AcquireFailed));
end;

procedure TF_DigiReg.B_IdleClick(Sender: TObject);
begin
 Self.TB_reg.Position := 0;
 Self.T_SpeedTimer(Self);
end;

procedure TF_DigiReg.B_OdhlLokoClick(Sender: TObject);
 begin
  Self.OpenHV.TrakceRelease(TTrakce.Callback());
 end;

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
      if ((tmp.acquired) and (tmp.Stav.regulators.Count = 0)) then
       begin
        tmp.ruc := false;
        tmp.CheckRelease();
       end;
    except
     on E:Exception do
      begin
       Self.OpenHV := tmp;
       Application.MessageBox(PChar('Lokomotivu se nepodařilo odhlásit:'+#13#10+E.Message),
           'Varování', MB_OK OR MB_ICONWARNING);
      end;
    end;
   end;

  Self.T_Speed.Enabled := false;
 end;

procedure TF_DigiReg.AcquireFailed(Sender:TObject; data:Pointer);
begin
 Application.MessageBox('Převezetí lokomotivy se nedařilo!', 'Chyba', MB_OK OR MB_ICONWARNING);
end;

procedure TF_DigiReg.B_STOPClick(Sender: TObject);
 begin
  Self.OpenHV.EmergencyStop(TTrakce.Callback(), TTrakce.Callback());
  Self.UpdateElements();
 end;

procedure TF_DigiReg.RG_SmerClick(Sender: TObject);
 begin
  Self.speed := -1;
  Self.T_SpeedTimer(Self);
 end;

//zavola se po zadosti o prevezeti, pokud je lokomotiva volna
procedure TF_DigiReg.LocoChanged();
begin
 Self.SetElemntsState(((Self.OpenHV.acquired) and ((Self.OpenHV.pom = pc) or (Self.OpenHV.pom = released))));

 if (Self.OpenHV.acquired) then
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
   Self.L_ComStatus.Caption    := 'loko odhlášeno';
  end;

 Self.UpdateElements();
end;

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
  CHB_Total.Enabled := state;
 end;

procedure TF_DIgiReg.UpdateElements();
var funkce:TFunkce;
begin
 TB_reg.Position := OpenHV.speedStep;

 Self.L_stupen.Caption := IntToStr(OpenHV.speedStep)+' / '+IntToStr(OpenHV.slot.maxSpeed);
 Self.L_speed.Caption  := IntToStr(OpenHV.realSpeed);

 RG_Smer.ItemIndex := Integer(OpenHV.direction);
 Self.L_address.Caption := IntToStr(OpenHV.Adresa);
 Self.Caption := OpenHV.Nazev+' ('+OpenHV.data.Oznaceni+') : '+IntToStr(OpenHV.adresa);
 B_PrevzitLoko.Enabled := ((not OpenHV.acquired) or (OpenHV.pom = error));
 B_OdhlLoko.Enabled := OpenHV.acquired;
 CHB_Total.Checked := OpenHV.ruc;
 Self.L_mine.Caption := PrevodySoustav.BoolToStr(OpenHV.acquired);

 if ((OpenHV.acquired) and ((OpenHV.pom = pc) or (OpenHV.pom = released))) then
  begin
   Self.S_Status.Brush.Color := clGreen;
  end else begin
   if ((OpenHV.stolen) or (OpenHV.pom = progr)) then
    begin
     Self.S_Status.Brush.Color := clYellow;
    end else begin
     Self.S_Status.Brush.Color := clRed;
    end;
  end;

 case (OpenHV.pom) of
  TPomStatus.progr    : Self.L_POM.Caption := 'progr';
  TPomStatus.error    : Self.L_POM.Caption := 'error';
  TPomStatus.pc       : Self.L_POM.Caption := 'automat';
  TPomStatus.released : Self.L_POM.Caption := 'ruční';
 end;//case

 funkce := OpenHV.slotFunkce;
 CHB_svetla.Checked := funkce[0];
 CHB_f1.Checked  := funkce[1];
 CHB_f2.Checked  := funkce[2];
 CHB_f3.Checked  := funkce[3];
 CHB_f4.Checked  := funkce[4];
 CHB_f5.Checked  := funkce[5];
 CHB_f6.Checked  := funkce[6];
 CHB_f7.Checked  := funkce[7];
 CHB_f8.Checked  := funkce[8];
 CHB_f9.Checked  := funkce[9];
 CHB_f10.Checked := funkce[10];
 CHB_f11.Checked := funkce[11];
 CHB_f12.Checked := funkce[12];
end;

procedure TF_DIgiReg.Stolen();
begin
 // nekdo mi ukradl hnaci vozidlo
 Self.SetElemntsState(false);
 Self.UpdateElements();
end;

procedure TF_DigiReg.S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Self.B_PrevzitLoko.Enabled) then
  Self.B_PrevzitLokoClick(self);
end;

procedure TF_DigiReg.T_SpeedTimer(Sender: TObject);
begin
 if (Self.OpenHV = nil) then Exit();
 if (Self.speed = Self.TB_reg.Position) then Exit();

 OpenHV.SetSpeedStepDir(TB_reg.Position, RG_Smer.ItemIndex = 1,
                        TTrakce.Callback(), TTrakce.Callback());

 Self.L_stupen.Caption := IntToStr(TB_Reg.Position)+' / '+IntToStr(OpenHV.slot.maxSpeed);
 Self.L_speed.Caption  := IntToStr(OpenHV.realSpeed);
 Self.speed := Self.TB_reg.Position;
end;

// vyvola se, pokud je me okynko aktivni a je nad nim stiskla klavesa
procedure TF_DigiReg.MyKeyPress(key:Integer);
begin
 if (not Self.OpenHV.acquired) then
  begin
   if (key = VK_RETURN) then
     if (Self.ActiveControl <> Self.B_PrevzitLoko) then
       Self.B_PrevzitLokoClick(Self);
   Exit();
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

end;

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
end;

procedure TRegulatorCollector.LocoChanged(addr:Word);
var frm:TF_DigiReg;
begin
 frm := Self.GetForm(addr);
 if (frm = nil) then Exit;
 frm.LocoChanged();
end;

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
   raise ERCMaxWindows.Create('Otevřen maximální počet oken regulátorů!');

 Self.forms.data[i].OpenForm(HV);
end;

function TRegulatorCollector.GetForm(addr:Word):TF_DigiReg;
var i:Integer;
begin
 Result := nil;
 for i := 0 to Self._MAX_FORMS-1 do
  begin
   if (Self.forms.data[i].OpenHV = nil) then continue;
   if (Self.forms.data[i].OpenHV.adresa = addr) then
     Exit(Self.forms.data[i]);
  end;//for
end;

procedure TRegulatorCollector.CloseAll();
var i:Integer;
begin
 for i := 0 to Self._MAX_FORMS-1 do
   Self.forms.data[i].Close;
end;

procedure TRegulatorCollector.Stolen(addr:Word);
var frm:TF_DigiReg;
begin
 frm := Self.GetForm(addr);
 if (frm = nil) then Exit;

 frm.Stolen();
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

function TRegulatorCollector.IsLoko(HV:THV):boolean;
var i:Integer;
begin
 if (Self = nil) then Exit(false);
 
 for i := 0 to _MAX_FORMS-1 do
   if ((Self.forms.data[i] <> nil) and (Self.forms.data[i].OpenHV = HV)) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  RegCollector := TRegulatorCollector.Create();

finalization
  FreeAndNil(RegCollector);

end.//unit
