unit DataRCS;

// TMTBTableData - trida starajici se o vyplnovani tabulky MTB

interface

uses ComCtrls, SysUtils;

type
  TRCSTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateLine(Board:integer);
      procedure UpdateTable();

      constructor Create(LV:TListView);
  end;//TMTBData

var
   RCSTableData:TRCSTableData;


implementation

uses TechnologieRCS, RCS, Prevody;

////////////////////////////////////////////////////////////////////////////////

constructor TRCSTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.LoadToTable();
var i, j:integer;
    LI:TListItem;
 begin
  Self.LV.Clear();

  for i := 0 to TRCS._MAX_RCS-1 do
   begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
   end;//for i
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to TRCS._MAX_RCS-1 do
  Self.UpdateLine(i);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateLine(board:integer);
var j:integer;
    output:Integer;
    LI:TListItem;
 begin
  LI := Self.LV.Items[board];

  Self.LV.Items.Item[board].Caption := IntToStr(board);

  if (not RCSi.ready) then
   begin
    LI.SubItems.Strings[0] := '';
    LI.SubItems.Strings[1] := '';
    LI.SubItems.Strings[2] := '';
    LI.SubItems.Strings[3] := '-------- --------';
    LI.SubItems.Strings[4] := '-------- --------';
    LI.SubItems.Strings[5] := '-';
    LI.SubItems.Strings[6] := '-';

    Exit();
   end;

  try
    if (RCSi.IsModule(board)) then
     begin
      if (RCSi.GetModuleType(board) = 'MTB-UNI') then
        LI.ImageIndex := 0
      else if (RCSi.GetModuleType(board) = 'MTB-TTL') then
        LI.ImageIndex := 1
      else if (RCSi.GetModuleType(board) = 'MTB-TTLo') then
        LI.ImageIndex := 2
      else if (RCSi.GetModuleType(board) = 'MTB-UNIo') then
        LI.ImageIndex := 5
      else
       Self.LV.Items.Item[board].ImageIndex := -1;
     end else
       Self.LV.Items.Item[board].ImageIndex := -1;
  except
    Self.LV.Items.Item[board].ImageIndex := -1;
  end;

  if (RCSi.GetNeeded(board)) then
    LI.SubItems.Strings[0] := 'X'
  else
    LI.SubItems.Strings[0] := '';

  try
    LI.SubItems.Strings[1] := RCSi.GetModuleName(board);
  except
    on E:Exception do
      LI.SubItems.Strings[1] := E.Message;
  end;

  try
    if (RCSi.IsModule(board)) then
      LI.SubItems.Strings[2] := RCSi.GetModuleType(board)
    else
      LI.SubItems.Strings[2] := '-';
  except
    on E:Exception do
      LI.SubItems.Strings[2] := E.Message;
  end;

  try
    if (RCSi.Opened) then
     begin
      if (RCSi.IsModule(board) and (not RCSi.IsModuleFailure(board))) then
       begin
        if (RCSi.Started) then
         begin
          LI.SubItems.Strings[3] := '';
          LI.SubItems.Strings[4] := '';

          for j := 0 to 15 do
           begin
            case (RCSi.GetInput(board, j)) of
              isOn          : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '1';
              isOff         : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '0';
              failure       : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + 'X';
              notYetScanned : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '?';
              unavailable   : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '-';
            end;

            output := RCSi.GetOutput(board, j);
            if (output > 1) then
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4]+'S'
            else
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4]+IntToStr(output);

            if (j = 7) then
             begin
              LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + ' ';
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4] + ' ';
             end;//if
           end;//for
         end else begin
          LI.SubItems.Strings[3] := '-------- --------';
          LI.SubItems.Strings[4] := '-------- --------';
         end;

        LI.SubItems.Strings[5] := 'Ano';
        LI.SubItems.Strings[6] := RCSi.GetModuleFW(board);
       end else begin
        // neexistuje
        LI.SubItems.Strings[3] := '-------- --------';
        LI.SubItems.Strings[4] := '-------- --------';
        if (RCSi.IsModuleFailure(board)) then
          LI.SubItems.Strings[5] := 'Fail'
        else
          LI.SubItems.Strings[5] := 'Ne';
        LI.SubItems.Strings[6] := '-';
       end;
     end else begin
      // mtb closed
      LI.SubItems.Strings[3] := '-------- --------';
      LI.SubItems.Strings[4] := '-------- --------';
      LI.SubItems.Strings[5] := '-';
      LI.SubItems.Strings[6] := '-';
     end;
  except
    on E:Exception do
     begin
      LI.SubItems.Strings[3] := 'Exception';
      LI.SubItems.Strings[4] := E.Message;
      LI.SubItems.Strings[5] := 'Ex';
      LI.SubItems.Strings[6] := 'Ex';
     end;
  end;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 RCSTableData.Free();

end.//unit
