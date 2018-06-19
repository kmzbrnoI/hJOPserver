unit DataAB;

{
  Trida TABTableData se stara o vykreslovani tabulky automaticky stavenych
  jizdnich cest.
}

interface

uses ComCtrls, SysUtils, StrUtils, TechnologieJC;

type
  TABTableData=class
    private
      LV:TListView;

    public
      procedure AddJC(JC:TJC);
      procedure RemoveJC(index:Integer);
      procedure Clear();

      constructor Create(LV:TListView);
  end;

var
  ABTableData : TABTableData;

implementation

uses TechnologieAB;

////////////////////////////////////////////////////////////////////////////////

constructor TABTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABTableData.AddJC(JC:TJC);
var LI:TListItem;
    j:Integer;
begin
 LI := Self.LV.Items.Add();
 LI.Caption := JC.nazev;
end;

procedure TABTableData.RemoveJC(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABTableData.Clear();
begin
 Self.LV.Clear();
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ABTableData.Free();

end.//unit
