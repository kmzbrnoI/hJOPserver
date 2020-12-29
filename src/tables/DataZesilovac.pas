unit DataZesilovac;

// TZesTableData - trida resici tvypisovani tabulky zesilovacu

interface

uses ComCtrls, SysUtils;

type
  TZesTableData=class
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
  ZesTableData : TZesTableData;

implementation

uses Booster, BoosterDb;

////////////////////////////////////////////////////////////////////////////////

constructor TZesTableData.Create(LV: TListView);
begin
 inherited Create();
 Self.LV := LV;
 Self.changed := true;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.LoadToTable();
var LI: TListItem;
    booster: TBooster;
begin
 Self.LV.Clear();

 for booster in Boosters.sorted do
  begin
   LI := Self.LV.Items.Add;

   LI.Caption := booster.id;
   LI.SubItems.Add(booster.name);
   if (booster.isOverloadDetection) then
     LI.SubItems.Add(booster.settings.RCS.overload.ToString())
   else
     LI.SubItems.Add('-');
   if (booster.isPowerDetection) then
     LI.SubItems.Add(booster.settings.RCS.power.ToString())
   else
     LI.SubItems.Add('-');
   if (booster.isDCCdetection) then
     LI.SubItems.Add(booster.settings.RCS.DCC.ToString())
   else
     LI.SubItems.Add('-');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.UpdateTable();
begin
 if (not Self.changed) then Exit();

 Self.LV.Repaint();
 Self.changed := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.ZesChange();
begin
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ZesTableData.Free();

end.//unit
