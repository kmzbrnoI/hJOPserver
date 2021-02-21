unit DataORs;

// TORsTableData - trida resici vypisovani oblasti rizeni do tabulky

interface

uses ComCtrls, SysUtils, Classes;

type
  TORsTableData = class
  private
    LV: TListView;

  public

    procedure LoadToTable();
    procedure UpdateTable(force: Boolean = false);

    constructor Create(LV: TListView);
  end;

var
  ORsTableData: TORsTableData;

implementation

uses AreaDb, Area;

/// /////////////////////////////////////////////////////////////////////////////

constructor TORsTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TORsTableData.LoadToTable();
var LI: TListItem;
  i, j: Integer;
begin
  Self.LV.Clear;

  for i := 0 to Areas.Count - 1 do
  begin
    LI := Self.LV.Items.Add;
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('---');
  end; // for i

  Self.UpdateTable(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORsTableData.UpdateTable(force: Boolean = false);
var Area: TArea;
begin
  for Area in Areas do
  begin
    if ((Area.changed) or (force)) then
    begin
      Area.UpdateLine(Self.LV.Items[Area.index]);
      Area.changed := false;
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

ORsTableData.Free();

end.// unit
