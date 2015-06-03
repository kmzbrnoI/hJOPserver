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
var i:Integer;
    LI:TListItem;
    cnt:Integer;
    bstr:TBooster;
begin
 Self.LV.Clear();

 cnt := BoostersDb.BoosterCnt;

 for i := 0 to cnt-1 do
  begin
   LI := Self.LV.Items.Add;

   bstr := BoostersDb.GetBooster(i);

   LI.Caption := IntToStr(i);
   LI.SubItems.Add(bstr.bSettings.Name);
   LI.SubItems.Add(TBooster.GetBClassString(bstr.bSettings.bclass));
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ZesTableData.Free();

end.//unit
