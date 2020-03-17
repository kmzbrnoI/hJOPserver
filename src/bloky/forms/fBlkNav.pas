unit fBlkNav;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ComCtrls, fMain, TBlokNav, IBUtils,
  fBlkNavEvent, Generics.Collections, Themes, CloseTabSheet, Buttons,
  TBloky;

type
  TF_BlkNav = class(TForm)
    L_Name: TLabel;
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_ID: TLabel;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    SE_RCSport: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    LB_Stanice: TListBox;
    L_Station: TLabel;
    Label5: TLabel;
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
    procedure SE_RCSmoduleExit(Sender: TObject);

  private
   OpenIndex:Integer;
   Blk:TBlkNav;
   NewBlk:Boolean;
   obls:TArstr;   //oblasti rizeni, ve kterych se SCom nachazi

    eventForms:TList<TF_BlkNavEvent>;
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
  F_BlkNav: TF_BlkNav;

implementation

uses GetSystems, FileSystem, TechnologieRCS, TBlok, TOblRizeni, DataBloky;

{$R *.dfm}

procedure TF_BlkNav.OpenForm(BlokIndex:Integer);
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
  Self.ShowModal;
 end;

procedure TF_BlkNav.NewBlkOpenForm();
 begin
  E_Nazev.Text             := '';
  SE_ID.Value              := Blky.GetBlkID(Blky.count-1)+1;
  SE_Delay.Value           := TBlkNav._NAV_DEFAULT_DELAY;
  CHB_Zamknuto.Checked     := false;
  Self.L_UsekID.Caption    := 'bude zobrazen priste';

  Self.SE_RCSmodule.Value := 1;
  Self.SE_RCSmoduleExit(Self);
  Self.SE_RCSPort.Value := 0;
  Self.CB_Typ.ItemIndex := -1;

  Self.CHB_RCS_Output.Checked := true;
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  // prvni udalost nepridavame, protoze muze byt navestidlo cestove,
  // ktere ji nepotrebuje

  Self.Caption := 'Editovat data nového bloku návěstidlo';
  Self.ActiveControl := E_Nazev;
 end;

procedure TF_BlkNav.NormalOpenForm;
var glob:TBlkSettings;
    settings:TBlkNavSettings;
    i:Integer;
    eventForm:TF_BlkNavEvent;
    ts:TCloseTabSheet;
    oblr:TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for oblr in Self.Blk.OblsRizeni do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls, Self.Blk.OblsRizeni.Count);
  for i := 0 to Self.Blk.OblsRizeni.Count-1 do
    obls[i] := Self.Blk.OblsRizeni[i].id;

  SE_Delay.Value := settings.ZpozdeniPadu;

  Self.CHB_RCS_Output.Checked := (settings.RCSAddrs.Count > 0);
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  if (settings.RCSAddrs.Count > 0) then
   begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_RCSmodule.MaxValue)) then
      Self.SE_RCSmodule.MaxValue := 0;
    Self.SE_RCSPort.MaxValue := 0;

    Self.SE_RCSmodule.Value := settings.RCSAddrs[0].board;
    SE_RCSPort.Value := settings.RCSAddrs[0].port;
    CB_Typ.ItemIndex := Integer(settings.OutputType);
   end;
  Self.SE_RCSmoduleExit(Self);

  CHB_Zamknuto.Checked := settings.zamknuto;

  for i := 0 to settings.events.Count-1 do
   begin
    ts             := TCloseTabSheet.Create(Self.PC_Events);
    ts.PageControl := Self.PC_Events;
    ts.OnClose     := Self.OnTabClose;
    if (i = 0) then
      ts.Caption  := 'globální'
    else
      ts.Caption  := IntToStr(settings.events[i].delka.min)+'-'+IntToStr(settings.events[i].delka.max)+'      ';
    eventForm  := TF_BlkNavEvent.Create(ts);

    eventForm.OpenForm(settings.events[i], (i = 0), Self.obls);
    eventForm.Parent := ts;
    eventForm.Show();

    Self.eventForms.Add(eventForm);
    Self.eventTabSheets.Add(ts);
   end;

  Self.L_UsekID.Caption := Blky.GetBlkName((Self.Blk as TBlkNav).UsekID);

  Self.Caption := 'Editovat data bloku '+glob.name+' (návěstidlo)';
  Self.ActiveControl := B_Save;
 end;

procedure TF_BlkNav.HlavniOpenForm();
 begin
  SetLength(Self.obls, 0);
  Self.LB_Stanice.Clear();
  Self.SE_RCSmodule.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkNav.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blky.count);
 end;

procedure TF_BlkNav.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkNav.CHB_RCS_OutputClick(Sender: TObject);
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

procedure TF_BlkNav.BB_Event_AddClick(Sender: TObject);
var eventForm:TF_BlkNavEvent;
    ts:TCloseTabSheet;
begin
  ts             := TCloseTabSheet.Create(Self.PC_Events);
  if (Self.eventForms.Count = 0) then
    ts.Caption   := 'globální'
  else
    ts.Caption   := IntToStr(Self.eventForms.Count);

  ts.PageControl := Self.PC_Events;
  ts.OnClose     := Self.OnTabClose;
  eventForm      := TF_BlkNavEvent.Create(ts);
  Self.PC_Events.ActivePage := ts;

  eventForm.OpenEmptyForm((Self.eventForms.Count = 0), Self.obls);
  eventForm.Parent := ts;
  eventForm.Show();

  Self.eventForms.Add(eventForm);
  Self.eventTabSheets.Add(ts);
end;

procedure TF_BlkNav.B_SaveClick(Sender: TObject);
var glob:TBlkSettings;
    settings:TBlkNavSettings;
    i: Integer;
    str:string;
    another: TBlk;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blky.IsBlok(SE_ID.Value,OpenIndex)) then
   begin
    Application.MessageBox('ID již bylo definováno na jiném bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Self.CHB_RCS_Output.Checked) then
   begin
    if (CB_Typ.ItemIndex = -1) then
     begin
      Application.MessageBox('Vyberte typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit;
     end;

    another := Blky.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule.Value, SE_RCSPort.Value), Self.Blk, TRCSIOType.output);
    if (another <> nil) then
     begin
      if (Application.MessageBox(PChar('RCS adresa se již používá na bloku '+another.name+', chcete pokračovat?'),
                                 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
     end;
   end;

  for i := 0 to Self.eventForms.Count-1 do
   begin
    str := Self.eventForms[i].Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar(str), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;//for i

  glob.name     := E_Nazev.Text;
  glob.id       := SE_ID.Value;
  glob.typ      := _BLK_NAV;

  if (NewBlk) then
   begin
    glob.poznamka := '';
    try
      Blk := Blky.Add(_BLK_USEK, glob) as TBlkNav;
    except
      on E:Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
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
    settings.OutputType := TBlkNavOutputType(CB_Typ.ItemIndex);
   end;

  settings.ZpozdeniPadu := Self.SE_Delay.Value;

  settings.zamknuto := CHB_Zamknuto.Checked;
  settings.events := TList<TBlkNavSprEvent>.Create();
  for i := 0 to Self.eventForms.Count-1 do
    settings.events.Add(Self.eventForms[i].event);

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkNav.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TF_BlkNav.FormCreate(Sender: TObject);
begin
 eventForms     := TList<TF_BlkNavEvent>.Create();
 eventTabSheets := TList<TTabSheet>.Create();;
end;

procedure TF_BlkNav.FormDestroy(Sender: TObject);
begin
 eventForms.Free();
 eventTabSheets.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkNav.PageControlCloseButtonDrawTab(Control: TCustomTabControl;
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

procedure TF_BlkNav.PageControlCloseButtonMouseDown(Sender: TObject;
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

procedure TF_BlkNav.PageControlCloseButtonMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := False;
  PageControl.Repaint;
end;

procedure TF_BlkNav.PageControlCloseButtonMouseMove(Sender: TObject;
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

procedure TF_BlkNav.PageControlCloseButtonMouseUp(Sender: TObject;
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

procedure TF_BlkNav.SE_RCSmoduleExit(Sender: TObject);
begin
 Self.SE_RCSport.MaxValue := TBlky.SEPortMaxValue(Self.SE_RCSmodule.Value, Self.SE_RCSport.Value);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkNav.OnTabClose(Sender:TObject);
var i:Integer;
begin
 if (Self.eventTabSheets.Count <= 1) then
  begin
   if (Application.MessageBox(PChar('Mazání globální události způsobí nezastavení vlaku před návěstidlem, '+
      'proto je doporučeno jen u seřaďovacích návěstidel!'+#13#10+'Opravdu smazat globální událost?'),
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
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
