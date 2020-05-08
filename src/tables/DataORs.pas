unit DataORs;

// TORsTableData - trida resici vypisovani oblasti rizeni do tabulky

interface

uses ComCtrls, SysUtils;

type
  TORsTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable(force:boolean = false);

      constructor Create(LV:TListView);
  end;

var
  ORsTableData : TORsTableData;

implementation

uses TOblsRizeni, TOblRizeni;

////////////////////////////////////////////////////////////////////////////////

constructor TORsTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TORsTableData.LoadToTable();
var LI:TListItem;
    i, j:Integer;
begin
 Self.LV.Clear;

 for i := 0 to ORs.Count-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);
   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('---');
  end;//for i

 Self.UpdateTable(true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORsTableData.UpdateTable(force:boolean = false);
var OblR: TOR;
begin
 for OblR in ORs do
  begin
   if ((OblR.changed) or (force)) then
    begin
     OblR.UpdateLine(Self.LV.Items[OblR.index]);
     OblR.changed := false;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ORsTableData.Free();

end.//unit
