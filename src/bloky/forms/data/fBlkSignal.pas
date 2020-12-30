unit fBlkSignal;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ComCtrls, fMain, BlockSignal, IBUtils,
  fBlkSignalEvent, Generics.Collections, Themes, CloseTabSheet, Buttons,
  BlockDb;

type
  TF_BlkSignal = class(TForm)
    L_Name: TLabel;
    E_Nazev: TEdit;
    SE_ID: TSpinEdit;
    L_ID: TLabel;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    SE_RCSport1: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    LB_Stanice: TListBox;
    L_Station: TLabel;
    Label5: TLabel;
    CB_Typ: TComboBox;
    Label1: TLabel;
    L_UsekID: TLabel;
    SE_RCSmodule1: TSpinEdit;
    Label2: TLabel;
    SE_Delay: TSpinEdit;
    CHB_Zamknuto: TCheckBox;
    PC_Events: TPageControl;
    BB_Event_Add: TBitBtn;
    CHB_RCS_Output: TCheckBox;
    SE_RCSmodule2: TSpinEdit;
    SE_RCSport2: TSpinEdit;
    CHB_RCS_Second_Output: TCheckBox;
    Label6: TLabel;
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
    procedure SE_RCSmodule1Exit(Sender: TObject);
    procedure SE_RCSmodule2Exit(Sender: TObject);
    procedure CHB_RCS_Second_OutputClick(Sender: TObject);

  private
    OpenIndex: Integer;
    Blk: TBlkSignal;
    NewBlk: Boolean;
    obls: TArstr;   // oblasti rizeni, ve kterych se navestidlo nachazi

    eventForms: TObjectList<TF_BlkSignalEvent>;
    eventTabSheets: TObjectList<TTabSheet>;

    FCloseButtonMouseDownTab: TCloseTabSheet;
    FCloseButtonShowPushed: Boolean;

     procedure NormalOpenForm();
     procedure HlavniOpenForm();
     procedure NewBlkOpenForm();

     procedure OnTabClose(Sender: TObject);
  public
     procedure OpenForm(BlokIndex: Integer);
     procedure NewBlkCreate;
  end;

var
  F_BlkSignal: TF_BlkSignal;

implementation

uses GetSystems, FileSystem, TechnologieRCS, Block, TOblRizeni, DataBloky;

{$R *.dfm}

procedure TF_BlkSignal.OpenForm(BlokIndex: Integer);
 begin
  Self.OpenIndex := BlokIndex;
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  Self.HlavniOpenForm();

  if (Self.NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
 end;

procedure TF_BlkSignal.NewBlkOpenForm();
 begin
  E_Nazev.Text := '';
  SE_ID.Value := Blocks.GetBlkID(Blocks.count-1)+1;
  SE_Delay.Value := TBlkSignal._SIG_DEFAULT_DELAY;
  CHB_Zamknuto.Checked := false;
  Self.L_UsekID.Caption := 'bude zobrazen příště';

  Self.SE_RCSmodule1.Value := 1;
  Self.SE_RCSmodule1Exit(Self);
  Self.SE_RCSPort1.Value := 0;
  Self.CB_Typ.ItemIndex := -1;

  Self.CHB_RCS_Second_Output.Checked := false;
  Self.CHB_RCS_Second_OutputClick(Self);

  Self.CHB_RCS_Output.Checked := true;
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  // prvni udalost nepridavame, protoze muze byt navestidlo seradovaci,
  // ktere ji nepotrebuje

  Self.Caption := 'Editovat data nového bloku návěstidlo';
  Self.ActiveControl := E_Nazev;
 end;

procedure TF_BlkSignal.NormalOpenForm();
var glob: TBlkSettings;
    settings: TBlkSignalSettings;
    i: Integer;
    eventForm: TF_BlkSignalEvent;
    ts: TCloseTabSheet;
    oblr: TOR;
 begin
  glob := Self.Blk.GetGlobalSettings();
  settings := Self.Blk.GetSettings();

  E_Nazev.Text := glob.name;
  SE_ID.Value  := glob.id;

  for oblr in Self.Blk.stations do
    Self.LB_Stanice.Items.Add(oblr.Name);

  SetLength(obls, Self.Blk.stations.Count);
  for i := 0 to Self.Blk.stations.Count-1 do
    obls[i] := Self.Blk.stations[i].id;

  SE_Delay.Value := settings.fallDelay;

  Self.CHB_RCS_Output.Checked := (settings.RCSAddrs.Count > 0);
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  if (settings.RCSAddrs.Count > 0) then
   begin
    if (settings.RCSAddrs[0].board > Cardinal(Self.SE_RCSmodule1.MaxValue)) then
      Self.SE_RCSmodule1.MaxValue := 0;
    Self.SE_RCSPort1.MaxValue := 0;

    Self.SE_RCSmodule1.Value := settings.RCSAddrs[0].board;
    SE_RCSPort1.Value := settings.RCSAddrs[0].port;
    CB_Typ.ItemIndex := Integer(settings.OutputType);
   end;
  Self.CHB_RCS_Second_Output.Checked := (settings.RCSAddrs.Count > 1);
  Self.CHB_RCS_Second_OutputClick(Self);
  if (settings.RCSAddrs.Count > 1) then
   begin
    if (settings.RCSAddrs[1].board > Cardinal(Self.SE_RCSmodule2.MaxValue)) then
      Self.SE_RCSmodule2.MaxValue := 0;
    Self.SE_RCSPort2.MaxValue := 0;

    Self.SE_RCSmodule2.Value := settings.RCSAddrs[1].board;
    SE_RCSPort2.Value := settings.RCSAddrs[1].port;
   end;
  Self.SE_RCSmodule1Exit(Self);

  Self.CHB_Zamknuto.Checked := settings.locked;

  for i := 0 to settings.events.Count-1 do
   begin
    ts := TCloseTabSheet.Create(Self.PC_Events);
    ts.PageControl := Self.PC_Events;
    ts.OnClose := Self.OnTabClose;
    if (i = 0) then
      ts.Caption := 'globální'
    else
      ts.Caption := IntToStr(settings.events[i].length.min)+'-'+IntToStr(settings.events[i].length.max)+'      ';
    eventForm  := TF_BlkSignalEvent.Create(ts);

    eventForm.OpenForm(settings.events[i], (i = 0), Self.obls);
    eventForm.Parent := ts;
    eventForm.Show();

    Self.eventForms.Add(eventForm);
    Self.eventTabSheets.Add(ts);
   end;

  Self.L_UsekID.Caption := Blocks.GetBlkName((Self.Blk as TBlkSignal).trackId);

  Self.Caption := 'Editovat data bloku '+glob.name+' (návěstidlo)';
  Self.ActiveControl := B_Save;
 end;

procedure TF_BlkSignal.HlavniOpenForm();
 begin
  SetLength(Self.obls, 0);
  Self.LB_Stanice.Clear();
  Self.SE_RCSmodule1.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_RCSmodule2.MaxValue := RCSi.maxModuleAddrSafe;
 end;

procedure TF_BlkSignal.NewBlkCreate();
 begin
  NewBlk := true;
  OpenForm(Blocks.count);
 end;

procedure TF_BlkSignal.B_StornoClick(Sender: TObject);
 begin
  Self.Close();
 end;

procedure TF_BlkSignal.CHB_RCS_OutputClick(Sender: TObject);
begin
 Self.SE_RCSmodule1.Enabled  := Self.CHB_RCS_Output.Checked;
 Self.SE_RCSPort1.Enabled := Self.CHB_RCS_Output.Checked;
 Self.CB_Typ.Enabled := Self.CHB_RCS_Output.Checked;

 if (not Self.CHB_RCS_Output.Checked) then
  begin
   Self.SE_RCSmodule1.Value := 1;
   Self.SE_RCSPort1.Value := 0;
   Self.CB_Typ.ItemIndex := -1;
   Self.CHB_RCS_Second_Output.Checked := false;
   Self.CHB_RCS_Second_OutputClick(Self);
  end;
end;

procedure TF_BlkSignal.CHB_RCS_Second_OutputClick(Sender: TObject);
begin
 Self.SE_RCSmodule2.Enabled := Self.CHB_RCS_Second_Output.Checked;
 Self.SE_RCSport2.Enabled := Self.CHB_RCS_Second_Output.Checked;
 Self.SE_RCSmodule2Exit(Self);
 if (not Self.CHB_RCS_Second_Output.Checked) then
  begin
   Self.SE_RCSmodule2.Value := 1;
   Self.SE_RCSport2.Value := 0;
  end;
end;

procedure TF_BlkSignal.BB_Event_AddClick(Sender: TObject);
var eventForm: TF_BlkSignalEvent;
    ts: TCloseTabSheet;
begin
  ts := TCloseTabSheet.Create(Self.PC_Events);
  if (Self.eventForms.Count = 0) then
    ts.Caption := 'globální'
  else
    ts.Caption := IntToStr(Self.eventForms.Count);

  ts.PageControl := Self.PC_Events;
  ts.OnClose := Self.OnTabClose;
  eventForm := TF_BlkSignalEvent.Create(ts);
  Self.PC_Events.ActivePage := ts;

  eventForm.OpenEmptyForm((Self.eventForms.Count = 0), Self.obls);
  eventForm.Parent := ts;
  eventForm.Show();

  Self.eventForms.Add(eventForm);
  Self.eventTabSheets.Add(ts);
end;

procedure TF_BlkSignal.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
    settings: TBlkSignalSettings;
    str: string;
    another: TBlk;
    fBlkNavEvent: TF_BlkSignalEvent;
 begin
  if (E_Nazev.Text = '') then
   begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit;
   end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
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

    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule1.Value, SE_RCSPort1.Value), Self.Blk, TRCSIOType.output);
    if (another <> nil) then
     begin
      if (Application.MessageBox(PChar('První RCS adresa se již používá na bloku '+another.name+', chcete pokračovat?'),
                                 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
     end;
   end;
  if (Self.CHB_RCS_Second_Output.Checked) then
   begin
    another := Blocks.AnotherBlockUsesRCS(TRCS.RCSAddr(Self.SE_RCSmodule2.Value, SE_RCSPort2.Value), Self.Blk, TRCSIOType.output);
    if (another <> nil) then
     begin
      if (Application.MessageBox(PChar('Druhá RCS adresa se již používá na bloku '+another.name+', chcete pokračovat?'),
                                 'Otázka', MB_YESNO OR MB_ICONQUESTION) = mrNo) then
        Exit();
     end;
   end;


  for fBlkNavEvent in Self.eventForms do
   begin
    str := fBlkNavEvent.Check();
    if (str <> '') then
     begin
      Application.MessageBox(PChar(str), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
     end;
   end;

  glob.name := E_Nazev.Text;
  glob.id := SE_ID.Value;
  glob.typ := btSignal;

  if (NewBlk) then
   begin
    glob.note := '';
    try
      Blk := Blocks.Add(btSignal, glob) as TBlkSignal;
    except
      on E: Exception do
       begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:'+#13#10+E.Message), 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
        Exit();
       end;
    end;
   end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
   end;

  //ukladani dat
  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  if (Self.CHB_RCS_Output.Checked) then
   begin
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_RCSmodule1.Value, SE_RCSPort1.Value));
    settings.OutputType := TBlkSignalOutputType(CB_Typ.ItemIndex);
   end;
  if (Self.CHB_RCS_Second_Output.Checked) then
    settings.RCSAddrs.Add(TRCS.RCSAddr(Self.SE_RCSmodule2.Value, SE_RCSPort2.Value));

  settings.fallDelay := Self.SE_Delay.Value;

  settings.locked := CHB_Zamknuto.Checked;
  settings.events := TObjectList<TBlkSignalTrainEvent>.Create();
  for fBlkNavEvent in Self.eventForms do
    settings.events.Add(fBlkNavEvent.GetEvent());

  Self.Blk.SetSettings(settings);

  Self.Close();
  Self.Blk.Change();
 end;

procedure TF_BlkSignal.FormClose(Sender: TObject; var Action: TCloseAction);
 begin
  Self.NewBlk := false;
  Self.OpenIndex := -1;
  BlokyTableData.UpdateTable();

  Self.eventForms.Clear();
  Self.eventTabSheets.Clear();
 end;

procedure TF_BlkSignal.FormCreate(Sender: TObject);
begin
 Self.eventForms := TObjectList<TF_BlkSignalEvent>.Create();
 Self.eventTabSheets := TObjectList<TTabSheet>.Create();;
end;

procedure TF_BlkSignal.FormDestroy(Sender: TObject);
begin
 Self.eventForms.Free();
 Self.eventTabSheets.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignal.PageControlCloseButtonDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  PageControl: TPageControl;
  TabSheet: TCloseTabSheet;
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

procedure TF_BlkSignal.PageControlCloseButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  PageControl: TPageControl;
  TabSheet: TCloseTabSheet;
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

procedure TF_BlkSignal.PageControlCloseButtonMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := False;
  PageControl.Repaint;
end;

procedure TF_BlkSignal.PageControlCloseButtonMouseMove(Sender: TObject;
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

procedure TF_BlkSignal.PageControlCloseButtonMouseUp(Sender: TObject;
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

procedure TF_BlkSignal.SE_RCSmodule1Exit(Sender: TObject);
begin
 Self.SE_RCSport1.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCSmodule1.Value, Self.SE_RCSport1.Value);
end;

procedure TF_BlkSignal.SE_RCSmodule2Exit(Sender: TObject);
begin
 Self.SE_RCSport2.MaxValue := TBlocks.SEPortMaxValue(Self.SE_RCSmodule2.Value, Self.SE_RCSport2.Value);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignal.OnTabClose(Sender: TObject);
var i: Integer;
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
     Self.eventForms.Delete(i);
     Self.eventTabSheets.Delete(i);
     Exit();
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
