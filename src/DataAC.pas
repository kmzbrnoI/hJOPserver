unit DataAC;

// TACTableData - trida resici vypis AC do tabulky

interface

uses ComCtrls, SysUtils;

type
  TACTableData = class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable();

      constructor Create(LV:TListView);

      procedure AddAC();
      procedure RemoveAC(index:Integer);
  end;

var
  ACTableData : TACTableData;

implementation

uses AC, ACDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor TACTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.LoadToTable();
var i, j:Integer;
    LI:TListItem;
begin
 Self.LV.Clear();

 for i := 0 to ACDb.ACs.Count-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);

   for j := 0 to Self.LV.Columns.Count-2 do
    LI.SubItems.Add('');
 end;//for i

 Self.UpdateTable();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.UpdateTable();
var i:Integer;
    AC:TAC;
begin
 for i := 0 to ACDb.ACs.Count-1 do
  begin
   AC := ACDb.ACs[i];
   Self.LV.Items.Item[i].SubItems.Strings[0] := AC.name;

   if (AC.running) then
     Self.LV.Items.Item[i].SubItems.Strings[1] := 'running'
   else if (AC.ready) then
     Self.LV.Items.Item[i].SubItems.Strings[1] := 'ready'
   else
     Self.LV.Items.Item[i].SubItems.Strings[1] := 'not ready';

   Self.LV.Items.Item[i].SubItems.Strings[2] := AC.krk_filename;
  end;//for i
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.AddAC();
var LI:TListItem;
begin
 LI := Self.LV.Items.Add;
 LI.Caption := ACDb.ACs[ACDb.ACs.Count-1].name;
 LI.SubItems.Add(ACDb.ACs[ACDb.ACs.Count-1].krk_filename);
end;//procedure

procedure TACTableData.RemoveAC(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ACTableData.Free();

end.//unit
