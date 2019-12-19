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
      procedure UpdateTable(force:boolean = false);

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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.UpdateTable(force:boolean = false);
var i:Integer;
    AC:TAC;
begin
 for i := 0 to ACDb.ACs.Count-1 do
  begin
   AC := ACDb.ACs[i];

   if ((Self.LV.Items[i].SubItems[0] = 'nepřipraven') and (AC.ready)) then
     Self.LV.Items[i].SubItems[0] := 'připraven';
   if ((Self.LV.Items[i].SubItems[0] = 'připraven') and (not AC.ready)) then
     Self.LV.Items[i].SubItems[0] := 'nepřipraven';

   if ((not AC.changed) and (not force)) then continue;

   AC.changed := false;
   Self.LV.Items[i].Caption := AC.name;

   if (AC.running) then
     Self.LV.Items[i].SubItems[0] := 'spuštěn'
   else if (AC.paused) then
     Self.LV.Items[i].SubItems[0] := 'pozastaven'
   else if (AC.ready) then
     Self.LV.Items[i].SubItems[0] := 'připraven'
   else
     Self.LV.Items[i].SubItems[0] := 'nepřipraven';

   Self.LV.Items[i].SubItems[1] := AC.krk_filename;

   Self.LV.Items[i].SubItems[2] := IntToStr(AC.stat_run);
   Self.LV.Items[i].SubItems[3] := IntToStr(AC.stat_end);
  end;//for i
end;

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.AddAC();
var LI:TListItem;
begin
 LI := Self.LV.Items.Add;
 LI.Caption := ACDb.ACs[ACDb.ACs.Count-1].name;
 LI.SubItems.Add(ACDb.ACs[ACDb.ACs.Count-1].krk_filename);
end;

procedure TACTableData.RemoveAC(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ACTableData.Free();

end.//unit
