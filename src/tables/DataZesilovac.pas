unit DataZesilovac;

// TZesTableData - trida resici tvypisovani tabulky zesilovacu

interface

uses ComCtrls, SysUtils;

type
  TZesTableData = class
  private
    LV: TListView;
    changed: Boolean;

  public

    procedure LoadToTable();
    procedure UpdateTable();
    procedure ZesChange();

    constructor Create(LV: TListView);
  end;

var
  ZesTableData: TZesTableData;

implementation

uses Booster, BoosterDb, IfThenElse;

/// /////////////////////////////////////////////////////////////////////////////

constructor TZesTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
  Self.changed := true;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.LoadToTable();
var LI: TListItem;
  Booster: TBooster;
begin
  Self.LV.Clear();

  for Booster in Boosters.sorted do
  begin
    LI := Self.LV.Items.Add;

    LI.Caption := Booster.id;
    LI.SubItems.Add(Booster.name);
    if (Booster.isOverloadDetection) then
      LI.SubItems.Add(ite(Booster.settings.rcs.overload.reversed, '! ', '') + Booster.settings.rcs.overload.addr.ToString())
    else
      LI.SubItems.Add('-');
    if (Booster.isPowerDetection) then
      LI.SubItems.Add(ite(Booster.settings.rcs.power.reversed, '! ', '') + Booster.settings.rcs.power.addr.ToString())
    else
      LI.SubItems.Add('-');
    if (Booster.isDCCdetection) then
      LI.SubItems.Add(ite(Booster.settings.rcs.DCC.reversed, '! ', '') + Booster.settings.rcs.DCC.addr.ToString())
    else
      LI.SubItems.Add('-');
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.UpdateTable();
begin
  if (not Self.changed) then
    Exit();

  Self.LV.Repaint();
  Self.changed := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.ZesChange();
begin
  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

ZesTableData.Free();

end.// unit
