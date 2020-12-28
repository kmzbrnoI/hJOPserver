unit DataRCS;

// TRCSTableData - trida starajici se o vyplnovani tabulky RCS

interface

uses ComCtrls, SysUtils, Generics.Collections;

type
  TRCSTableData=class
    private
      LV: TListView;
      AddrToLine: TDictionary<Integer, Integer>;

    public

      procedure LoadToTable(load_all: Boolean = false);
      procedure UpdateBoard(addr: Integer);
      procedure UpdateTable();
      function CreateLineForNewBoard(addr: Integer): Integer;
      function GetLineForNewBoard(addr: Integer): Integer;

      constructor Create(LV: TListView);
      destructor Destroy(); override;
  end;

var
   RCSTableData: TRCSTableData;


implementation

uses TechnologieRCS, RCS;

////////////////////////////////////////////////////////////////////////////////

constructor TRCSTableData.Create(LV: TListView);
begin
 inherited Create();
 Self.LV := LV;
 Self.AddrToLine := TDictionary<Integer, Integer>.Create();
end;//ctor

destructor TRCSTableData.Destroy();
begin
 Self.AddrToLine.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.LoadToTable(load_all: Boolean = false);
var i, j: integer;
    LI: TListItem;
 begin
  Self.LV.Clear();
  Self.AddrToLine.Clear();

  if (not RCSi.ready) then
    Exit();

  for i := 0 to RCSi.maxModuleAddr do
   begin
    if (RCSi.ready) then
      if ((not RCSi.IsModule(i)) and (not RCSi.GetNeeded(i)) and (not load_all)) then
        continue;

    LI := Self.LV.Items.Add();
    LI.Data := Pointer(i);
    LI.Caption := IntToStr(i) + ' (0x' + IntToHex(i, 2) + ')';
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.AddrToLine.Add(i, Self.LV.Items.Count-1);
    Self.UpdateBoard(i);
   end;//for i
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateTable();
var item: TListItem;
begin
 for item in Self.LV.Items do
   Self.UpdateBoard(Integer(item.Data));
end;

////////////////////////////////////////////////////////////////////////////////

function TRCSTableData.GetLineForNewBoard(addr: Integer): Integer;
var i: Integer;
begin
 for i := 0 to Self.LV.Items.Count-1 do
   if (addr < Integer(Self.LV.Items[i].Data)) then
     Exit(i);
 Result := Self.LV.Items.Count;
end;

function TRCSTableData.CreateLineForNewBoard(addr: Integer): Integer;
var LI: TListItem;
    i, line: Integer;
begin
 line := Self.GetLineForNewBoard(addr);
 LI := Self.LV.Items.Insert(line);
 LI.Data := Pointer(addr);
 LI.Caption := IntToStr(addr) + ' (0x' + IntToHex(addr, 2) + ')';
 for i := 0 to Self.LV.Columns.Count-1 do
   LI.SubItems.Add('');

 Self.AddrToLine.Add(addr, line);
 Result := line;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateBoard(addr: Integer);
var j, cnt: integer;
    output: Integer;
    LI: TListItem;
    start: integer;
 begin
  if (not Self.AddrToLine.ContainsKey(addr)) then
    Self.CreateLineForNewBoard(addr);

  LI := Self.LV.Items[Self.AddrToLine[addr]];

  if (not RCSi.ready) then
   begin
    LI.SubItems[0] := '';
    LI.SubItems[1] := '';
    LI.SubItems[2] := '';
    LI.SubItems[3] := '-------- --------';
    LI.SubItems[4] := '-------- --------';
    LI.SubItems[5] := '-';
    LI.SubItems[6] := '-';

    Exit();
   end;

  try
    if (RCSi.IsModule(addr)) then
     begin
      if (RCSi.GetModuleType(addr) = 'MTB-UNI') then
        LI.ImageIndex := 0
      else if (RCSi.GetModuleType(addr) = 'MTB-TTL') then
        LI.ImageIndex := 1
      else if (RCSi.GetModuleType(addr) = 'MTB-TTLo') then
        LI.ImageIndex := 2
      else if (RCSi.GetModuleType(addr) = 'MTB-UNIo') then
        LI.ImageIndex := 5
      else
       LI.ImageIndex := -1;
     end else
       LI.ImageIndex := -1;
  except
    LI.ImageIndex := -1;
  end;

  if (RCSi.GetNeeded(addr)) then
    LI.SubItems[0] := 'X'
  else
    LI.SubItems[0] := '';

  try
    LI.SubItems[1] := RCSi.GetModuleName(addr);
  except
    on E: Exception do
      LI.SubItems[1] := E.Message;
  end;

  try
    if (RCSi.IsModule(addr)) then
      LI.SubItems[2] := RCSi.GetModuleType(addr)
    else
      LI.SubItems[2] := '-';
  except
    on E: Exception do
      LI.SubItems[2] := E.Message;
  end;

  try
    if (RCSi.Opened) then
     begin
      if (RCSi.IsNonFailedModule(addr)) then
       begin
        if (RCSi.Started) then
         begin
          LI.SubItems[3] := '';
          LI.SubItems[4] := '';

          cnt := RCSi.GetModuleInputsCount(addr);
          if (RCSi.GetInput(addr, 0) = unavailablePort) then
            start := 1
          else
            start := 0;

          for j := start to cnt-1 do
           begin
            if (j = ((cnt+start) div 2)) then
              LI.SubItems[3] := LI.SubItems[3] + ' ';

            case (RCSi.GetInput(addr, j)) of
              isOn : LI.SubItems[3] := LI.SubItems[3] + '1';
              isOff : LI.SubItems[3] := LI.SubItems[3] + '0';
              failure : LI.SubItems[3] := LI.SubItems[3] + 'X';
              notYetScanned : LI.SubItems[3] := LI.SubItems[3] + '?';
              unavailableModule: LI.SubItems[3] := LI.SubItems[3] + '-';
            end;
           end;

          cnt := RCSi.GetModuleOutputsCount(addr);
          for j := 0 to cnt-1 do
           begin
            if (j = cnt div 2) then
              LI.SubItems[4] := LI.SubItems[4] + ' ';

            output := RCSi.GetOutput(addr, j);
            if (output > 1) then
              LI.SubItems[4] := LI.SubItems[4]+'S'
            else
              LI.SubItems[4] := LI.SubItems[4]+IntToStr(output);
           end;//for

         end else begin
          LI.SubItems[3] := '-';
          LI.SubItems[4] := '-';
         end;

        LI.SubItems[5] := '✓';
        LI.SubItems[6] := RCSi.GetModuleFW(addr);
       end else begin
        // neexistuje
        LI.SubItems[3] := '-';
        LI.SubItems[4] := '-';
        if (RCSi.IsModuleFailure(addr)) then
          LI.SubItems[5] := 'Fail'
        else
          LI.SubItems[5] := '';
        LI.SubItems[6] := '-';
       end;
     end else begin
      // mtb closed
      LI.SubItems[3] := '-';
      LI.SubItems[4] := '-';
      LI.SubItems[5] := '-';
      LI.SubItems[6] := '-';
     end;
  except
    on E: Exception do
     begin
      LI.SubItems[3] := 'Exception';
      LI.SubItems[4] := E.Message;
      LI.SubItems[5] := 'Ex';
      LI.SubItems[6] := 'Ex';
     end;
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 RCSTableData.Free();

end.//unit
