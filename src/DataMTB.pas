unit DataMTB;

// TMTBTableData - trida starajici se o vyplnovani tabulky MTB

interface

uses ComCtrls, SysUtils;

type
  TMTBTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateLine(Board:integer);
      procedure UpdateTable();

      constructor Create(LV:TListView);
  end;//TMTBData

var
   MTBTableData:TMTBTableData;


implementation

uses TechnologieMTB;

////////////////////////////////////////////////////////////////////////////////

constructor TMTBTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.LoadToTable();
var i, j:integer;
    LI:TListItem;
 begin
  Self.LV.Clear();

  for i := 0 to TMTB._MAX_MTB-1 do
   begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
   end;//for i
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to TMTB._MAX_MTB-1 do
  Self.UpdateLine(i);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.UpdateLine(board:integer);
var i,j:integer;
    output:Integer;
 begin
  i := board;

  Self.LV.Items.Item[i].ImageIndex := MTB.GetTypeIntMTB(board);
  Self.LV.Items.Item[i].Caption    := IntToStr(board);

  if (MTB.GetNeeded(board)) then
    Self.LV.Items.Item[i].SubItems.Strings[0] := 'X'
  else
    Self.LV.Items.Item[i].SubItems.Strings[0] := '';

  Self.LV.Items.Item[i].SubItems.Strings[1] := MTB.GetNameMTB(board);
  Self.LV.Items.Item[i].SubItems.Strings[2] := MTB.GetTypeStrMTB(board);

  if (MTB.Openned) then
   begin
    // mtb openned
    if (MTB.IsModule(board)) then
     begin
      // existuje

      if (MTB.Start) then
       begin
        Self.LV.Items.Item[i].SubItems.Strings[3] := '';
        Self.LV.Items.Item[i].SubItems.Strings[4] := '';

        for j := 0 to 15 do
         begin
          Self.LV.Items.Item[i].SubItems.Strings[3] := Self.LV.Items.Item[i].SubItems.Strings[3]+IntToStr(MTB.GetInput(board, j));

          output := MTB.GetOutput(board, j);
          if (output > 1) then
            Self.LV.Items.Item[i].SubItems.Strings[4] := Self.LV.Items.Item[i].SubItems.Strings[4]+'S'
          else
            Self.LV.Items.Item[i].SubItems.Strings[4] := Self.LV.Items.Item[i].SubItems.Strings[4]+IntToStr(output);

          if (j = 7) then
           begin
            Self.LV.Items.Item[i].SubItems.Strings[3] := Self.LV.Items.Item[i].SubItems.Strings[3] + ' ';
            Self.LV.Items.Item[i].SubItems.Strings[4] := Self.LV.Items.Item[i].SubItems.Strings[4] + ' ';
           end;//if
         end;//for
       end else begin
        Self.LV.Items.Item[i].SubItems.Strings[3] := '-------- --------';
        Self.LV.Items.Item[i].SubItems.Strings[4] := '-------- --------';
       end;

      Self.LV.Items.Item[i].SubItems.Strings[5] := 'Ano';
      Self.LV.Items.Item[i].SubItems.Strings[6] := MTB.GetModuleFirmware(board);
     end else begin
      // neexistuje
      Self.LV.Items.Item[i].SubItems.Strings[3] := '-------- --------';
      Self.LV.Items.Item[i].SubItems.Strings[4] := '-------- --------';
      Self.LV.Items.Item[i].SubItems.Strings[5] := 'Ne';
      Self.LV.Items.Item[i].SubItems.Strings[6] := '-';
     end;
   end else begin
    // mtb closed
    Self.LV.Items.Item[i].SubItems.Strings[3] := '-------- --------';
    Self.LV.Items.Item[i].SubItems.Strings[4] := '-------- --------';
    Self.LV.Items.Item[i].SubItems.Strings[5] := '-';
    Self.LV.Items.Item[i].SubItems.Strings[6] := '-';
   end;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 MTBTableData.Free();

end.//unit
