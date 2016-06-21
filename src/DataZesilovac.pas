unit DataZesilovac;

// TZesTableData - trida resici tvypisovani tabulky zesilovacu

interface

uses ComCtrls, SysUtils, RPConst;

type
  TZesTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();

      constructor Create(LV:TListView);
  end;

var
  ZesTableData : TZesTableData;

implementation

uses Booster, BoosterDb;

////////////////////////////////////////////////////////////////////////////////

constructor TZesTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TZesTableData.LoadToTable();
var LI:TListItem;
    booster:TBooster;
begin
 Self.LV.Clear();

 for booster in Boosters.sorted do
  begin
   LI := Self.LV.Items.Add;

   LI.Caption := booster.id;
   LI.SubItems.Add(booster.name);
   LI.SubItems.Add(TBooster.GetBClassString(booster.bSettings.bclass));
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ZesTableData.Free();

end.//unit
