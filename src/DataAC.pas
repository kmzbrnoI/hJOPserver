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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TACTableData.UpdateTable(force:boolean = false);
var i:Integer;
    AC:TAC;
begin
 for i := 0 to ACDb.ACs.Count-1 do
  begin
   AC := ACDb.ACs[i];

   if ((Self.LV.Items.Item[i].SubItems.Strings[0] = 'nepøipraven') and (AC.ready)) then
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'pøipraven';
   if ((Self.LV.Items.Item[i].SubItems.Strings[0] = 'pøipraven') and (not AC.ready)) then
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'nepøipraven';

   if ((not AC.changed) and (not force)) then continue;

   AC.changed := false;
   Self.LV.Items.Item[i].Caption := AC.name;

   if (AC.running) then
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'spuštìn'
   else if (AC.paused) then
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'pozastaven'
   else if (AC.ready) then
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'pøipraven'
   else
     Self.LV.Items.Item[i].SubItems.Strings[0] := 'nepøipraven';

   Self.LV.Items.Item[i].SubItems.Strings[1] := AC.krk_filename;

   Self.LV.Items.Item[i].SubItems.Strings[2] := IntToStr(AC.stat_run);
   Self.LV.Items.Item[i].SubItems.Strings[3] := IntToStr(AC.stat_end);
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
