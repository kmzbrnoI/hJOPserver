unit fBlkSignal;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Types,
  Dialogs, StdCtrls, Spin, ExtCtrls, ComCtrls, fMain, BlockSignal,
  fBlkSignalEvent, Generics.Collections, Themes, CloseTabSheet, Buttons,
  BlockDb, Vcl.NumberBox;

type
  TF_BlkSignal = class(TForm)
    L_Name: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    L_ID: TLabel;
    GB_RCS: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    SE_RCSport1: TSpinEdit;
    B_Storno: TButton;
    B_Save: TButton;
    Label5: TLabel;
    CB_Typ: TComboBox;
    Label1: TLabel;
    L_Track_Id: TLabel;
    SE_RCSmodule1: TSpinEdit;
    Label2: TLabel;
    SE_Delay: TSpinEdit;
    CHB_Locked: TCheckBox;
    PC_Events: TPageControl;
    BB_Event_Add: TBitBtn;
    CHB_RCS_Output: TCheckBox;
    SE_RCSmodule2: TSpinEdit;
    SE_RCSport2: TSpinEdit;
    CHB_RCS_Second_Output: TCheckBox;
    Label6: TLabel;
    GB_PSt: TGroupBox;
    CHB_PSt: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    SE_Cont_Module: TSpinEdit;
    SE_Ind_Module: TSpinEdit;
    Label9: TLabel;
    Label10: TLabel;
    SE_Ind_Port: TSpinEdit;
    SE_Cont_Port: TSpinEdit;
    CHB_changeTime: TCheckBox;
    NB_ChangeTime: TNumberBox;
    Label11: TLabel;
    SE_RCSsystem1: TSpinEdit;
    SE_RCSsystem2: TSpinEdit;
    Label12: TLabel;
    SE_Ind_System: TSpinEdit;
    SE_Cont_System: TSpinEdit;
    CHB_ForceDirection: TCheckBox;
    CB_ForceDirection: TComboBox;
    Label13: TLabel;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BB_Event_AddClick(Sender: TObject);
    procedure PageControlCloseButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseLeave(Sender: TObject);
    procedure PageControlCloseButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
      Active: Boolean);
    procedure CHB_RCS_OutputClick(Sender: TObject);
    procedure SE_RCSmodule2Exit(Sender: TObject);
    procedure CHB_RCS_Second_OutputClick(Sender: TObject);
    procedure CHB_PStClick(Sender: TObject);
    procedure CHB_changeTimeClick(Sender: TObject);
    procedure CHB_ForceDirectionClick(Sender: TObject);

  private
    openIndex: Integer;
    block: TBlkSignal;
    isNewBlock: Boolean;

    eventForms: TObjectList<TF_BlkSignalEvent>;
    eventTabSheets: TObjectList<TTabSheet>;

    FCloseButtonMouseDownTab: TCloseTabSheet;
    FCloseButtonShowPushed: Boolean;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();

    procedure OnTabClose(Sender: TObject);

  public
    procedure EditBlock(BlokIndex: Integer);
    procedure NewBlock();

  end;

var
  F_BlkSignal: TF_BlkSignal;

implementation

uses GetSystems, RCSc, RCSsc, Block, Area, DataBloky, BlockTrack, ownGuiUtils,
  ownConvert, THnaciVozidlo;

{$R *.dfm}

procedure TF_BlkSignal.EditBlock(BlokIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := BlokIndex;
  Self.block := Blocks.GetBlkByIndex(BlokIndex) as TBlkSignal;
  if (block = nil) then
    raise Exception.Create('Blok #'+IntToStr(BlokIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkSignal.NewOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;
  Self.SE_Delay.Value := TBlkSignal._SIG_DEFAULT_DELAY;
  Self.CHB_Locked.Checked := false;
  Self.L_Track_Id.Caption := 'bude zobrazen příště';

  Self.SE_RCSsystem1.Value := 0;
  Self.SE_RCSmodule1.Value := 0;
  Self.SE_RCSport1.Value := 0;
  Self.CB_Typ.ItemIndex := -1;

  Self.CHB_RCS_Second_Output.Checked := false;
  Self.CHB_RCS_Second_OutputClick(Self);

  Self.CHB_RCS_Output.Checked := true;
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  Self.CHB_changeTime.Checked := False;
  Self.CHB_changeTimeClick(Self.CHB_changeTime);

  Self.CHB_PSt.Checked := false;
  Self.CHB_PStClick(Self.CHB_PSt);

  Self.CHB_ForceDirection.Checked := False;
  Self.CHB_ForceDirectionClick(Self.CHB_ForceDirection);

  // prvni udalost nepridavame, protoze muze byt navestidlo seradovaci,
  // ktere ji nepotrebuje

  Self.Caption := 'Nový blok Návěstidlo';
end;

procedure TF_BlkSignal.EditOpenForm();
var glob: TBlkSettings;
  settings: TBlkSignalSettings;
  track: TBlkTrack;
begin
  glob := Self.block.GetGlobalSettings();
  settings := Self.block.GetSettings();
  track := Blocks.GetBlkByID((Self.block as TBlkSignal).trackId) as TBlkTrack;

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  Self.SE_Delay.Value := settings.fallDelay;

  Self.CHB_RCS_Output.Checked := (settings.RCSAddrs.count > 0);
  Self.CHB_RCS_OutputClick(Self.CHB_RCS_Output);

  if (settings.RCSAddrs.count > 0) then
  begin
    Self.SE_RCSsystem1.Value := settings.RCSAddrs[0].system;
    Self.SE_RCSmodule1.Value := settings.RCSAddrs[0].module;
    SE_RCSport1.Value := settings.RCSAddrs[0].port;
    CB_Typ.ItemIndex := Integer(settings.OutputType);
  end;
  Self.CHB_RCS_Second_Output.Checked := (settings.RCSAddrs.count > 1);
  Self.CHB_RCS_Second_OutputClick(Self);

  if (settings.RCSAddrs.count > 1) then
  begin
    Self.SE_RCSsystem2.Value := settings.RCSAddrs[1].system;
    Self.SE_RCSmodule2.Value := settings.RCSAddrs[1].module;
    SE_RCSport2.Value := settings.RCSAddrs[1].port;
  end;

  Self.CHB_Locked.Checked := settings.locked;

  Self.CHB_changeTime.Checked := Self.block.IsSpecificChangeTime();
  Self.NB_ChangeTime.Text := ownConvert.TimeToSecTenths(settings.changeTime);
  Self.CHB_changeTimeClick(Self.CHB_changeTime);

  for var i := 0 to settings.events.count - 1 do
  begin
    var ts := TCloseTabSheet.Create(Self.PC_Events);
    ts.PageControl := Self.PC_Events;
    ts.OnClose := Self.OnTabClose;
    if (i = 0) then
      ts.Caption := 'globální'
    else
      ts.Caption := IntToStr(settings.events[i].length.min) + '-' + IntToStr(settings.events[i].length.max) + '      ';
    var eventForm := TF_BlkSignalEvent.Create(ts);

    eventForm.OpenForm(settings.events[i], (i = 0), track);
    eventForm.Parent := ts;
    eventForm.Show();

    Self.eventForms.Add(eventForm);
    Self.eventTabSheets.Add(ts);
  end;

  Self.L_Track_Id.Caption := Blocks.GetBlkName((Self.block as TBlkSignal).trackId);

  begin
    Self.CHB_PSt.Checked := settings.PSt.enabled;
    Self.CHB_PStClick(Self.CHB_PSt);
    if (settings.PSt.enabled) then
    begin
      if (settings.PSt.rcsIndicationShunt.module > Cardinal(Self.SE_Ind_Module.MaxValue)) then
        Self.SE_Ind_Module.MaxValue := 0;
      Self.SE_Ind_Port.MaxValue := 0;

      Self.SE_Ind_Module.Value := settings.PSt.rcsIndicationShunt.module;
      Self.SE_Ind_Port.Value := settings.PSt.rcsIndicationShunt.port;

      if (settings.PSt.rcsControllerShunt.module > Cardinal(Self.SE_Cont_Module.MaxValue)) then
        Self.SE_Cont_Module.MaxValue := 0;
      Self.SE_Cont_Port.MaxValue := 0;

      Self.SE_Cont_Module.Value := settings.PSt.rcsControllerShunt.module;
      Self.SE_Cont_Port.Value := settings.PSt.rcsControllerShunt.port;
    end;
  end;

  Self.CHB_ForceDirection.Checked := (settings.forceDirection <> THVOptionalSite.osNo);
  Self.CB_ForceDirection.ItemIndex := Integer(settings.forceDirection);
  Self.CHB_ForceDirectionClick(Self.CHB_ForceDirection);

  Self.Caption := 'Upravit blok ' + glob.name + ' (návěstidlo)';
end;

procedure TF_BlkSignal.CommonOpenForm();
begin
  Self.SE_RCSsystem1.MaxValue := RCSs._RCSS_MAX;
  Self.SE_RCSsystem2.MaxValue := RCSs._RCSS_MAX;
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkSignal.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkSignal.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkSignal.CHB_changeTimeClick(Sender: TObject);
begin
  Self.NB_ChangeTime.Enabled := Self.CHB_changeTime.Checked;
  if (not Self.CHB_changeTime.Checked) then
    Self.NB_ChangeTime.Text := TBlkSignal.DefaultChangeTime(Self.CHB_RCS_Output.Checked);
end;

procedure TF_BlkSignal.CHB_ForceDirectionClick(Sender: TObject);
begin
  Self.CB_ForceDirection.Enabled := Self.CHB_ForceDirection.Checked;
  if (not Self.CHB_ForceDirection.Checked) then
    Self.CB_ForceDirection.ItemIndex := -1;
end;

procedure TF_BlkSignal.CHB_PStClick(Sender: TObject);
begin
  Self.SE_Ind_System.Enabled := Self.CHB_PSt.Checked;
  Self.SE_Ind_Module.Enabled := Self.CHB_PSt.Checked;
  Self.SE_Ind_Port.Enabled := Self.CHB_PSt.Checked;
  Self.SE_Cont_System.Enabled := Self.CHB_PSt.Checked;
  Self.SE_Cont_Module.Enabled := Self.CHB_PSt.Checked;
  Self.SE_Cont_Port.Enabled := Self.CHB_PSt.Checked;

  if (not Self.CHB_PSt.Checked) then
  begin
    Self.SE_Ind_System.Value := 0;
    Self.SE_Ind_Module.Value := 0;
    Self.SE_Ind_Port.Value := 0;
    Self.SE_Cont_System.Value := 0;
    Self.SE_Cont_Module.Value := 0;
    Self.SE_Cont_Port.Value := 0;
  end;
end;

procedure TF_BlkSignal.CHB_RCS_OutputClick(Sender: TObject);
begin
  Self.SE_RCSsystem1.Enabled := Self.CHB_RCS_Output.Checked;
  Self.SE_RCSmodule1.Enabled := Self.CHB_RCS_Output.Checked;
  Self.SE_RCSport1.Enabled := Self.CHB_RCS_Output.Checked;
  Self.CB_Typ.Enabled := Self.CHB_RCS_Output.Checked;
  Self.CHB_RCS_Second_Output.Enabled := Self.CHB_RCS_Output.Checked;

  if (not Self.CHB_RCS_Output.Checked) then
  begin
    Self.SE_RCSsystem1.Value := 0;
    Self.SE_RCSmodule1.Value := 0;
    Self.SE_RCSport1.Value := 0;
    Self.CB_Typ.ItemIndex := -1;
    Self.CHB_RCS_Second_Output.Checked := false;
    Self.CHB_RCS_Second_OutputClick(Self);
  end;

  Self.CHB_changeTimeClick(Self.CHB_changeTime);
end;

procedure TF_BlkSignal.CHB_RCS_Second_OutputClick(Sender: TObject);
begin
  Self.SE_RCSsystem2.Enabled := Self.CHB_RCS_Second_Output.Checked;
  Self.SE_RCSmodule2.Enabled := Self.CHB_RCS_Second_Output.Checked;
  Self.SE_RCSport2.Enabled := Self.CHB_RCS_Second_Output.Checked;
  Self.SE_RCSmodule2Exit(Self);
  if (not Self.CHB_RCS_Second_Output.Checked) then
  begin
    Self.SE_RCSsystem2.Value := 0;
    Self.SE_RCSmodule2.Value := 0;
    Self.SE_RCSport2.Value := 0;
  end;
end;

procedure TF_BlkSignal.BB_Event_AddClick(Sender: TObject);
var ts: TCloseTabSheet;
begin
  ts := TCloseTabSheet.Create(Self.PC_Events);
  if (Self.eventForms.count = 0) then
    ts.Caption := 'globální'
  else
    ts.Caption := IntToStr(Self.eventForms.count);

  ts.PageControl := Self.PC_Events;
  ts.OnClose := Self.OnTabClose;
  var eventForm := TF_BlkSignalEvent.Create(ts);
  Self.PC_Events.ActivePage := ts;

  eventForm.OpenEmptyForm((Self.eventForms.count = 0));
  eventForm.Parent := ts;
  eventForm.Show();

  Self.eventForms.Add(eventForm);
  Self.eventTabSheets.Add(ts);
end;

procedure TF_BlkSignal.B_SaveClick(Sender: TObject);
begin
  if (Self.E_Name.Text = '') then
  begin
    StrMessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_ForceDirection.Checked) and (Self.CB_ForceDirection.ItemIndex < 0)) then
  begin
    StrMessageBox('Vyberte vnucený směr návěstidla!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  var messages := '';

  if (Self.CHB_RCS_Output.Checked) then
  begin
    if (CB_Typ.ItemIndex = -1) then
    begin
      StrMessageBox('Vyberte typ výstupu!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    var addr := TRCSs.RCSsAddr(Self.SE_RCSsystem1.Value, Self.SE_RCSmodule1.Value, SE_RCSport1.Value);
    var another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.output);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString() + '.' + #13#10;
  end;
  if (Self.CHB_RCS_Second_Output.Checked) then
  begin
    var addr := TRCSs.RCSsAddr(Self.SE_RCSsystem2.Value, Self.SE_RCSmodule2.Value, SE_RCSport2.Value);
    var another := Blocks.AnotherBlockUsesRCS(addr, Self.block, TRCSIOType.output);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + addr.ToString() + '.' + #13#10;
  end;

  for var fBlkNavEvent in Self.eventForms do
  begin
    var str := fBlkNavEvent.Check();
    if (str <> '') then
    begin
      StrMessageBox(str, 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  try
    var glob: TBlkSettings;
    glob.name := Self.E_Name.Text;
    glob.id := Self.SE_ID.Value;
    glob.typ := btSignal;

    if (Self.isNewBlock) then
    begin
      try
        Self.block := Blocks.Add(glob) as TBlkSignal;
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se přidat blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end else begin
      try
        Self.block.SetGlobalSettings(glob);
      except
        on E: Exception do
        begin
          ExceptionMessageBox('Nepodařilo se uložit blok.', E, 'Nelze uložit data');
          Exit();
        end;
      end;
    end;

    var settings: TBlkSignalSettings;
    settings.RCSAddrs := TList<RCSsc.TRCSsAddr>.Create();
    if (Self.CHB_RCS_Output.Checked) then
    begin
      settings.RCSAddrs.Add(TRCSs.RCSsAddr(Self.SE_RCSsystem1.Value, Self.SE_RCSmodule1.Value, SE_RCSport1.Value));
      settings.OutputType := TBlkSignalOutputType(CB_Typ.ItemIndex);
    end;
    if (Self.CHB_RCS_Second_Output.Checked) then
      settings.RCSAddrs.Add(TRCSs.RCSsAddr(Self.SE_RCSsystem2.Value, Self.SE_RCSmodule2.Value, SE_RCSport2.Value));

    settings.fallDelay := Self.SE_Delay.Value;
    settings.changeTime := ownConvert.SecTenthsToTime(Self.NB_ChangeTime.Text);

    settings.locked := Self.CHB_Locked.Checked;
    settings.events := TObjectList<TBlkSignalTrainEvent>.Create();
    for var fBlkNavEvent in Self.eventForms do
      settings.events.Add(fBlkNavEvent.GetEvent());

    settings.forceDirection := THVOptionalSite(Self.CB_ForceDirection.ItemIndex);

    settings.PSt.enabled := Self.CHB_PSt.Checked;
    if (Self.CHB_PSt.Checked) then
    begin
      settings.PSt.rcsIndicationShunt := TRCSs.RCSsAddr(Self.SE_Ind_System.Value, Self.SE_Ind_Module.Value, Self.SE_Ind_Port.Value);
      settings.PSt.rcsControllerShunt := TRCSs.RCSsAddr(Self.SE_Cont_System.Value, Self.SE_Cont_Module.Value, Self.SE_Cont_Port.Value);

      var another := Blocks.AnotherBlockUsesRCS(settings.PSt.rcsIndicationShunt, Self.block, TRCSIOType.output);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.PSt.rcsIndicationShunt.ToString() + '.' + #13#10;

      another := Blocks.AnotherBlockUsesRCS(settings.PSt.rcsControllerShunt, Self.block, TRCSIOType.output);
      if (another <> nil) then
        messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.PSt.rcsControllerShunt.ToString() + '.' + #13#10;
    end;

    if (messages <> '') then
      StrMessageBox(messages, 'Varování', MB_OK OR MB_ICONWARNING);

    Self.block.SetSettings(settings);
    Self.block.Change();
  except
    on E: Exception do
    begin
      ExceptionMessageBox('Neočekávaná chyba.', E);
      Exit();
    end;
  end;

  Self.Close();
end;

procedure TF_BlkSignal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();

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

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignal.PageControlCloseButtonDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
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
  end else begin
    CloseBtnRect.Top := Rect.Top + 3;
    CloseBtnRect.Right := Rect.Right - 5;
    TabCaption.X := Rect.Left + 3;
  end;

  if (PageControl.Pages[TabIndex] is TCloseTabSheet) then
  begin
    TabSheet := PageControl.Pages[TabIndex] as TCloseTabSheet;
    CloseBtnSize := 14;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    TabSheet.FCloseButtonRect := CloseBtnRect;

    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y, PageControl.Pages[TabIndex].Caption);

    if not StyleServices.Enabled then
    begin
      if (FCloseButtonMouseDownTab = TabSheet) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      Windows.DrawFrameControl(PageControl.Canvas.Handle, TabSheet.FCloseButtonRect, DFC_CAPTION, CloseBtnDrawState);
    end else begin
      Dec(TabSheet.FCloseButtonRect.Left);

      if (FCloseButtonMouseDownTab = TabSheet) and FCloseButtonShowPushed then
        CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonPushed)
      else
        CloseBtnDrawDetails := StyleServices.GetElementDetails(twCloseButtonNormal);

      StyleServices.DrawElement(PageControl.Canvas.Handle, CloseBtnDrawDetails, TabSheet.FCloseButtonRect);
    end;
  end else begin
    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y, PageControl.Pages[TabIndex].Caption);
  end;
end;

procedure TF_BlkSignal.PageControlCloseButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
  PageControl: TPageControl;
  TabSheet: TCloseTabSheet;
begin
  PageControl := Sender as TPageControl;

  if Button = mbLeft then
  begin
    for i := 0 to PageControl.PageCount - 1 do
    begin
      if not(PageControl.Pages[i] is TCloseTabSheet) then
        Continue;
      TabSheet := PageControl.Pages[i] as TCloseTabSheet;
      if PtInRect(TabSheet.FCloseButtonRect, Point(X, Y)) then
      begin
        FCloseButtonMouseDownTab := TabSheet;
        FCloseButtonShowPushed := true;
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
  FCloseButtonShowPushed := false;
  PageControl.Repaint;
end;

procedure TF_BlkSignal.PageControlCloseButtonMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TF_BlkSignal.PageControlCloseButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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

procedure TF_BlkSignal.SE_RCSmodule2Exit(Sender: TObject);
begin
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_BlkSignal.OnTabClose(Sender: TObject);
begin
  if (Self.eventTabSheets.count <= 1) then
  begin
    if (StrMessageBox('Mazání globální události způsobí nezastavení vlaku před návěstidlem, ' +
      'proto je doporučeno jen u seřaďovacích návěstidel!' + #13#10 + 'Opravdu smazat globální událost?', 'Opravdu?',
      MB_YESNO OR MB_ICONQUESTION) <> mrYes) then
      Exit();
  end;

  for var i := 0 to Self.eventTabSheets.count - 1 do
  begin
    if (Self.eventTabSheets[i] = Sender) then
    begin
      Self.eventForms.Delete(i);
      Self.eventTabSheets.Delete(i);
      Exit();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
