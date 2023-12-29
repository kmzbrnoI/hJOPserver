unit fHoukEvs;

{
  TF_HoukEvs je okno, ktere umoznuje definovat libovolne mnozstvi houkacich
  udalosti ve smysl tridy THoukEv v libovolne poradi.
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Types,
  Dialogs, ComCtrls, CloseTabSheet, Themes, houkEvent, Generics.Collections,
  fhoukEv, Buttons, StdCtrls, UITypes, BlockTrack;

type
  TF_HoukEvs = class(TForm)
    PC_Events: TPageControl;
    BB_Add: TBitBtn;
    procedure PC_EventsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PC_EventsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PC_EventsDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure PC_EventsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PC_EventsMouseLeave(Sender: TObject);
    procedure PC_EventsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PC_EventsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BB_AddClick(Sender: TObject);
  private
    FCloseButtonMouseDownTab: TCloseTabSheet;
    FCloseButtonShowPushed: Boolean;
    Forms: TList<TF_HoukEv>;

    procedure CloseAllTabs();
    procedure OnTabClose(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure FillFromHouks(evs: TList<THoukEv>; parentTrack: TBlkTrack);
    function GetHoukEvs(): TObjectList<THoukEv>;
    function InputValid(): Boolean;

  end;

implementation

{$R *.dfm}
/// /////////////////////////////////////////////////////////////////////////////

constructor TF_HoukEvs.Create(AOwner: TComponent);
begin
  inherited;

  Self.Forms := TList<TF_HoukEv>.Create();
end;

destructor TF_HoukEvs.Destroy();
begin
  Self.CloseAllTabs();
  Self.Forms.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvs.FillFromHouks(evs: TList<THoukEv>; parentTrack: TBlkTrack);
var i: Integer;
  form: TF_HoukEv;
  ts: TCloseTabSheet;
begin
  Self.CloseAllTabs();

  for i := 0 to evs.Count - 1 do
  begin
    ts := TCloseTabSheet.Create(Self.PC_Events);
    ts.PageControl := Self.PC_Events;
    ts.Caption := IntToStr(i + 1);
    ts.OnClose := OnTabClose;

    form := TF_HoukEv.Create(nil);
    form.Parent := ts;
    form.FillFromHouk(evs[i], parentTrack);
    form.Show();

    Self.Forms.Add(form);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_HoukEvs.GetHoukEvs(): TObjectList<THoukEv>;
var form: TF_HoukEv;
begin
  Result := TObjectList<THoukEv>.Create();
  for form in Self.Forms do
    Result.Add(form.GetHoukEv());
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_HoukEvs.InputValid(): Boolean;
var form: TF_HoukEv;
begin
  for form in Self.Forms do
    if (not form.InputValid()) then
      Exit(false);
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvs.PC_EventsDragDrop(Sender, Source: TObject; X, Y: Integer);
const
  TCM_GETITEMRECT = $130A;
var
  TabRect: TRect;
  j: Integer;
  tmp: TF_HoukEv;
begin
  if (Sender is TPageControl) then
    for j := 0 to TPageControl(Sender).PageCount - 1 do
    begin
      TPageControl(Sender).Perform(TCM_GETITEMRECT, j, LParam(@TabRect));
      if (PtInRect(TabRect, Point(X, Y))) then
      begin
        if TPageControl(Sender).ActivePage.PageIndex <> j then
        begin
          // vlozime okno na novou spravnou pozici
          tmp := Self.Forms[TPageControl(Sender).ActivePageIndex];
          Self.Forms.Delete(TPageControl(Sender).ActivePageIndex);
          Self.Forms.Insert(j, tmp);

          TPageControl(Sender).ActivePage.PageIndex := j;
        end;
        Exit();
      end;
    end;
end;

procedure TF_HoukEvs.PC_EventsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Sender is TPageControl) then
    Accept := true;
end;

procedure TF_HoukEvs.PC_EventsDrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect;
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

  // coloured caption
  if (PageControl.Pages[TabIndex].ShowHint) then
    Control.Canvas.Brush.Color := clYellow
  else
    Control.Canvas.Brush.Color := clBtnFace;
  PageControl.Pages[TabIndex].Brush.Color := Control.Canvas.Brush.Color;

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

procedure TF_HoukEvs.PC_EventsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

  PageControl.BeginDrag(false);
end;

procedure TF_HoukEvs.PC_EventsMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := false;
  PageControl.Repaint;
end;

procedure TF_HoukEvs.PC_EventsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

procedure TF_HoukEvs.PC_EventsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvs.OnTabClose(Sender: TObject);
var i: Integer;
begin
  for i := 0 to Self.PC_Events.PageCount - 1 do
    if (Self.PC_Events.Pages[i] = Sender) then
    begin
      Self.Forms[i].Free();
      Self.Forms.Delete(i);
      Sender.Free();
      Self.PC_Events.Repaint();
      Exit();
    end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvs.CloseAllTabs();
var i: Integer;
  form: TF_HoukEv;
begin
  for form in Self.Forms do
    form.Free();
  Self.Forms.Clear();

  for i := Self.PC_Events.PageCount - 1 downto 0 do
    Self.PC_Events.Pages[i].Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_HoukEvs.BB_AddClick(Sender: TObject);
var form: TF_HoukEv;
  ts: TCloseTabSheet;
begin
  ts := TCloseTabSheet.Create(Self.PC_Events);
  ts.PageControl := Self.PC_Events;
  ts.Caption := IntToStr(Self.PC_Events.PageCount);
  ts.OnClose := OnTabClose;

  form := TF_HoukEv.Create(nil);
  form.Parent := ts;
  form.ShowEmpty();
  form.Show();

  Self.Forms.Add(form);
  Self.PC_Events.ActivePage := ts;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
