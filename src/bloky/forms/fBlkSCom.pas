unit fBlkSCom;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ComCtrls, fMain, TBlokSCom,
  fBlkSComEvent, Generics.Collections, Themes, CloseTabSheet, Buttons,
  StrUtils, TBloky;

type
  TF_BlkSCom = class(TForm)
    L_SCom01: TLabel;
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_SCom02: TLabel;
    GB_RCS: TGroupBox;
    L_SCom05: TLabel;
    L_SCom06: TLabel;
    SE_RCSport: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    LB_Stanice: TListBox;
    L_Usek03: TLabel;
    L_SCom04: TLabel;
    CB_Typ: TComboBox;
    Label1: TLabel;
    L_UsekID: TLabel;
    SE_RCSmodule: TSpinEdit;
    Label2: TLabel;
    SE_Delay: TSpinEdit;
    CHB_Zamknuto: TCheckBox;
    PC_Events: TPageControl;
    BB_Event_Add: TBitBtn;
    CHB_RCS_Output: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BB_Event_AddClick(Sender: TObject);
     procedure PageControlCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure PageControlCloseButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
     procedure PageControlCloseButtonMouseLeave(Sender: TObject);
     procedure PageControlCloseButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     procedure PageControlCloseButtonDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure CHB_RCS_OutputClick(Sender: TObject);

  private
   OpenIndex:Integer;
   Blk:TBlkSCom;
   NewBlk:Boolean;
   obls:TArstr;   //oblasti rizeni, ve kterych se SCom nachazi

    eventForms:TList<TF_BlkSComEvent>;
    eventTabSheets:TList<TTabSheet>;

    FCloseButtonMouseDownTab: TCloseTabSheet;
    FCloseButtonShowPushed: Boolean;

     procedure NormalOpenForm();
     procedure HlavniOpenForm();
     procedure NewBlkOpenForm();


     procedure OnTabClose(Sender:TObject);
  public
     procedure OpenForm(BlokIndex:Integer);
     procedure NewBlkCreate;
  end;

var
  F_BlkSCom: TF_BlkSCom;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBlok,
      TBLokUsek, DataBloky, fSettings, TBlokPrejezd;

{$R *.dfm}

procedure TF_BlkSCom.OpenForm(BlokIndex:Integer);
 begin
  OpenIndex := BlokIndex;
  Blky.GetBlkByIndex(BlokIndex,TBlk(Self.Blk));
  HlavniOpenForm;

  if (NewBlk) then
   begin
    NewBlkOpenForm;
   end else begin
    NormalOpenForm;
   end;
  F_BlkSCom.ShowModal;
 end;//procedure

procedure TF_BlkSCom.NewBlkOpenForm();
 begin
  E_Nazev.Text             := '';
  SE_ID.Value              := Blky.GetBlkID(Blky.Cnt-1)+1;
  SE_Delay.Value           := TBlkScom._NAV_DEFAULT_DELAY;
  CHB_Zamknuto.Checked     := false;
  Self.L_UsekID.Caption    := 'bude zobrazen priste';

  Self.SE_RCSmodule.Value  := 1;
  Self.SE_RCSPort.Value := 0;
  Self.CB_Typ.ItemIndex := -1;

  Self.CHB_RCS_Output.Checked := true;
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  // prvni udalost nepridavame, protoze muze byt navestidlo cestove,
  // ktere ji nepotrebuje

  F_BlkSCom.Caption := 'Editovat data nov�ho bloku n�v�stidlo';
  F_BlkSCom.ActiveControl := E_Nazev;
 end;//procedure

procedure TF_BlkSCom.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkSComSettings;
    i:Integer;
    eventForm:TF_BlkSComEvent;
    ts:TCloseTabSheet;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do Self.LB_Stanice.Items.Add(Self.Blk.OblsRizeni.ORs[i].Name);

  SetLength(obls, Self.Blk.OblsRizeni.Cnt);
  for i := 0 to Self.Blk.OblsRizeni.Cnt-1 do obls[i] := Self.Blk.OblsRizeni.ORs[i].id;

  SE_Delay.Value := settings.ZpozdeniPadu;

  Self.CHB_RCS_Output.Checked := (settings.RCSAddrs.Count > 0);
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  if (settings.RCSAddrs.Count > 0) then
   begin
    Self.SE_RCSmodule.Value := settings.RCSAddrs[0].board;
    SE_RCSPort.Value     := settings.RCSAddrs[0].port;
    CB_Typ.ItemIndex     := Integer(settings.OutputType);
   end;

  CHB_Zamknuto.Checked := settings.zamknuto;

  for i := 0 to settings.events.Count-1 do
   begin
    ts             := TCloseTabSheet.Create(Self.PC_Events);
    ts.PageControl := Self.PC_Events;
    ts.OnClose     := Self.OnTabClose;
    if (i = 0) then
      ts.Caption  := 'glob�ln�'
    else
      ts.Caption  := IntToStr(settings.events[i].delka.min)+'-'+IntToStr(settings.events[i].delka.max)+'      ';
    eventForm      := TF_BlkSComEvent.Create(ts);

    eventForm.OpenForm(settings.events[i], (i = 0), Self.obls);
    eventForm.Parent := ts;
    eventForm.Show();

    Self.eventForms.Add(eventForm);
    Self.eventTabSheets.Add(ts);
   end;

  Self.L_UsekID.Caption := Blky.GetBlkName((Self.Blk as TBlkSCom).UsekID);

  F_BlkSCom.Caption := 'Editovat data bloku '+glob.name+' (n�v�stidlo)';
  F_BlkSCom.ActiveControl := B_Save;
 end;//procedure

procedure TF_BlkSCom.HlavniOpenForm();
 begin
  SetLength(Self.obls, 0);
  Self.LB_Stanice.Clear();
 end;//procedure

procedure TF_BlkSCom.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blky.Cnt);
 end;//procedure

procedure TF_BlkSCom.B_StornoClick(Sender: TObject);
 begin
  F_BlkSCom.Close;
 end;

procedure TF_BlkSCom.CHB_RCS_OutputClick(Sender: TObject);
begin
 Self.SE_RCSmodule.Enabled  := Self.CHB_RCS_Output.Checked;
 Self.SE_RCSPort.Enabled := Self.CHB_RCS_Output.Checked;
 Self.CB_Typ.Enabled     := Self.CHB_RCS_Output.Checked;

 if (not Self.CHB_RCS_Output.Checked) then
  begin
   Self.SE_RCSmodule.Value  := 1;
   Self.SE_RCSPort.Value := 0;
   Self.CB_Typ.ItemIndex := -1;
  end;
end;

procedure TF_BlkSCom.BB_Event_AddClick(Sender: TObject);
var
    eventForm:TF_BlkSComEvent;
    ts:TCloseTabSheet;
begin
  ts             := TCloseTabSheet.Create(Self.PC_Events);
  if (Self.eventForms.Count = 0) then
    ts.Caption   := 'glob�ln�'
  else
    ts.Caption   := IntToStr(Self.eventForms.Count);

  ts.PageControl := Self.PC_Events;
  ts.OnClose     := Self.OnTabClose;
  eventForm      := TF_BlkSComEvent.Create(ts);
  Self.PC_Events.ActivePage := ts;

  eventForm.OpenEmptyForm((Self.eventForms.Count = 0), Self.obls);
  eventForm.Parent := ts;
  eventForm.Show();

  Self.eventForms.Add(eventForm);
  Self.eventTabSheets.Add(ts);
end;//procedure

procedure TF_BlkSCom.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkSComSettings;
    i: Integer;
    str:string;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vypl�te n�zev bloku!', 'Nelze ulo�it data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID ji� bylo definov�no na jin�m bloku!', 'Nelze ulo�it data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if ((Self.CHB_RCS_Output.Checked) and (CB_Typ.ItemIndex = -1)) then
   begin
    Application.MessageBox('Vyberte typ v�stupu!', 'Nelze ulo�it data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;

  for i := 0 to Self.eventForms.Count-1 do
   begin
    str := Self.eventForms[i].Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar(str), 'Nelze ulo�it data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;//for i

  glob.name     := E_Nazev.Text;
  glob.id       := SE_ID.Value;
  glob.typ      := _BLK_SCOM;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    Blk := Blky.Add(_BLK_SCOM, glob) as TBlkSCom;
    if (Blk = nil) then
     begin
      Application.MessageBox('Nepodarilo se pridat blok !','Nelze ulozit data',MB_OK OR MB_ICONWARNING);
      Exit;
     end;
   end else begin
    glob.poznamka := Self.Blk.poznamka;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_RCS_Output.Checked) then
   begin
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_RCSmodule.Value, SE_RCSPort.Value));
    settings.OutputType := TBlkSComOutputType(CB_Typ.ItemIndex);
   end;

  settings.ZpozdeniPadu := Self.SE_Delay.Value;

  settings.zamknuto := CHB_Zamknuto.Checked;
  settings.events := TList<TBlkSComSprEvent>.Create();
  for i := 0 to Self.eventForms.Count-1 do
    settings.events.Add(Self.eventForms[i].event);

  Self.Blk.SetSettings(settings);

  F_BlkSCom.Close;
  Self.Blk.Change();
 end;//procedure

procedure TF_BlkSCom.FormClose(Sender: TObject; var Action: TCloseAction);
var i:Integer;
 begin
  NewBlk     := false;
  OpenIndex  := -1;
  BlokyTableData.UpdateTable;

  for i := 0 to Self.eventForms.Count-1 do
   begin
    Self.eventForms[i].Free();
    Self.eventTabSheets[i].Free();
   end;
  Self.eventForms.Clear();
  Self.eventTabSheets.Clear();
 end;

procedure TF_BlkSCom.FormCreate(Sender: TObject);
begin
 eventForms     := TList<TF_BlkSComEvent>.Create();
 eventTabSheets := TList<TTabSheet>.Create();;
end;

procedure TF_BlkSCom.FormDestroy(Sender: TObject);
begin
 eventForms.Free();
 eventTabSheets.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSCom.PageControlCloseButtonDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  PageControl: TPageControl;
  TabSheet:TCloseTabSheet;
  TabCaption: TPoint;
  CloseBtnRect: TRect;
  CloseBtnDrawState: Cardinal;
  CloseBtnDrawDetails: TThemedElementDetails;
begin
  PageControl := Control as TPageControl;
  TabCaption.Y := Rect.Top + 3;

  if Active then
  begin
    CloseBtnRect.Top := Rect.Top + 4;
    CloseBtnRect.Right := Rect.Right - 5;
    TabCaption.X := Rect.Left + 6;
  end
  else
  begin
    CloseBtnRect.Top := Rect.Top + 3;
    CloseBtnRect.Right := Rect.Right - 5;
    TabCaption.X := Rect.Left + 3;
  end;

  if (PageControl.Pages[TabIndex] is TCloseTabSheet) then
  begin
    TabSheet:=PageControl.Pages[TabIndex] as TCloseTabSheet;
    CloseBtnSize := 14;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    TabSheet.FCloseButtonRect := CloseBtnRect;

    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y,
            PageControl.Pages[TabIndex].Caption);

    if not ThemeServices.ThemesEnabled then
    begin
      if (FCloseButtonMouseDownTab = TabSheet) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      Windows.DrawFrameControl(PageControl.Canvas.Handle,
        TabSheet.FCloseButtonRect, DFC_CAPTION, CloseBtnDrawState);
    end
    else
    begin
      Dec(TabSheet.FCloseButtonRect.Left);

      if (FCloseButtonMouseDownTab = TabSheet) and FCloseButtonShowPushed then
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twCloseButtonPushed)
      else
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twCloseButtonNormal);

      ThemeServices.DrawElement(PageControl.Canvas.Handle, CloseBtnDrawDetails,
                TabSheet.FCloseButtonRect);
    end;
  end else begin
    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y,
                 PageControl.Pages[TabIndex].Caption);
  end;
end;

procedure TF_BlkSCom.PageControlCloseButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  PageControl: TPageControl;
  TabSheet:TCloseTabSheet;
begin
  PageControl := Sender as TPageControl;

  if Button = mbLeft then
  begin
    for I := 0 to PageControl.PageCount - 1 do
    begin
      if not (PageControl.Pages[i] is TCloseTabSheet) then Continue;
      TabSheet:=PageControl.Pages[i] as TCloseTabSheet;
      if PtInRect(TabSheet.FCloseButtonRect, Point(X, Y)) then
      begin
        FCloseButtonMouseDownTab := TabSheet;
        FCloseButtonShowPushed := True;
        PageControl.Repaint;
      end;
    end;
  end;
end;

procedure TF_BlkSCom.PageControlCloseButtonMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := False;
  PageControl.Repaint;
end;

procedure TF_BlkSCom.PageControlCloseButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
  Inside: Boolean;
begin
  PageControl := Sender as TPageControl;

  if (ssLeft in Shift) and Assigned(FCloseButtonMouseDownTab) then
  begin
    Inside := PtInRect(FCloseButtonMouseDownTab.FCloseButtonRect, Point(X, Y));

    if FCloseButtonShowPushed <> Inside then
    begin
      FCloseButtonShowPushed := Inside;
      PageControl.Repaint;
    end;
  end;
end;

procedure TF_BlkSCom.PageControlCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if (Button = mbLeft) and Assigned(FCloseButtonMouseDownTab) then
  begin
    if PtInRect(FCloseButtonMouseDownTab.FCloseButtonRect, Point(X, Y)) then
    begin
      FCloseButtonMouseDownTab.DoClose;
      FCloseButtonMouseDownTab := nil;
      PageControl.Repaint;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSCom.OnTabClose(Sender:TObject);
var i:Integer;
begin
 if (Self.eventTabSheets.Count <= 1) then
  begin
   if (Application.MessageBox(PChar('Maz�n� glob�ln� ud�losti zp�sob� nezastaven� vlaku p�ed n�v�stidlem, '+
      'proto je doporu�eno jen u se�a�ovac�ch n�v�stidel!'+#13#10+'Opravdu smazat glob�ln� ud�lost?'),
      'Opravdu?', MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
     Exit();
  end;

 for i := 0 to Self.eventTabSheets.Count-1 do
  begin
   if (Self.eventTabSheets[i] = Sender) then
    begin
     Self.eventForms[i].Free();
     Self.eventForms.Delete(i);
     Self.eventTabSheets[i].Free();
     Self.eventTabSheets.Delete(i);
     Exit();
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
