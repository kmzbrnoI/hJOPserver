unit DataRCS;

// TRCSTableData - trida starajici se o vyplnovani tabulky RCS

interface

uses ComCtrls, SysUtils, Generics.Collections, Classes;

type
  TRCSTableData = class
  private
    LV: TListView;
    AddrToLine: TDictionary<Integer, Integer>;

  public

    procedure LoadToTable(load_all: Boolean = false);
    procedure UpdateBoard(addr: Integer);
    procedure UpdateBoardInputs(addr: Integer);
    procedure UpdateBoardOutputs(addr: Integer);
    procedure UpdateTable();
    function CreateLineForNewBoard(addr: Integer): Integer;
    function GetLineForNewBoard(addr: Integer): Integer;

    constructor Create(LV: TListView);
    destructor Destroy(); override;
  end;

var
  RCSTableData: TRCSTableData;

implementation

uses TechnologieRCS, RCS, IfThenElse;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRCSTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
  Self.AddrToLine := TDictionary<Integer, Integer>.Create();
end;

destructor TRCSTableData.Destroy();
begin
  Self.AddrToLine.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.LoadToTable(load_all: Boolean = false);
var LI: TListItem;
begin
  Self.LV.Clear();
  Self.AddrToLine.Clear();

  if (not RCSi.ready) then
    Exit();

  for var i := 0 to RCSi.maxModuleAddr do
  begin
    if (RCSi.ready) then
      if ((not RCSi.IsModule(i)) and (not RCSi.GetNeeded(i)) and (not load_all)) then
        continue;

    LI := Self.LV.Items.Add();
    LI.Data := Pointer(i);
    LI.Caption := IntToStr(i) + ' (0x' + IntToHex(i, 2) + ')';
    for var j := 0 to Self.LV.Columns.Count - 1 do
      LI.SubItems.Add('');

    Self.AddrToLine.Add(i, Self.LV.Items.Count - 1);
    Self.UpdateBoard(i);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateTable();
begin
  for var item in Self.LV.Items do
    Self.UpdateBoard(Integer(item.Data));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TRCSTableData.GetLineForNewBoard(addr: Integer): Integer;
begin
  for var i := 0 to Self.LV.Items.Count - 1 do
    if (addr < Integer(Self.LV.Items[i].Data)) then
      Exit(i);
  Result := Self.LV.Items.Count;
end;

function TRCSTableData.CreateLineForNewBoard(addr: Integer): Integer;
var LI: TListItem;
  line: Integer;
begin
  line := Self.GetLineForNewBoard(addr);
  LI := Self.LV.Items.Insert(line);
  LI.Data := Pointer(addr);
  LI.Caption := IntToStr(addr) + ' (0x' + IntToHex(addr, 2) + ')';
  for var i := 0 to Self.LV.Columns.Count - 1 do
    LI.SubItems.Add('');

  Self.AddrToLine.Add(addr, line);
  Result := line;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRCSTableData.UpdateBoard(addr: Integer);
var LI: TListItem;
begin
  if (not Self.AddrToLine.ContainsKey(addr)) then
    Self.CreateLineForNewBoard(addr);

  LI := Self.LV.Items[Self.AddrToLine[addr]];

  Self.UpdateBoardInputs(addr);
  Self.UpdateBoardOutputs(addr);

  if (not RCSi.ready) then
  begin
    LI.SubItems[0] := '';
    LI.SubItems[1] := '';
    LI.SubItems[2] := '';
    LI.SubItems[5] := '-';
    LI.SubItems[6] := '-';

    Exit();
  end;

  try
    if (RCSi.IsModule(addr)) then
    begin
      var type_: string := RCSi.GetModuleType(addr);
      if (type_.Contains('UNI')) then
        LI.ImageIndex := 0
      else if (type_.Contains('TTLo')) then
        LI.ImageIndex := 2
      else if (type_.Contains('UNIo')) then
        LI.ImageIndex := 5
      else if (type_.Contains('TTL')) then
        LI.ImageIndex := 1
      else
        LI.ImageIndex := -1;
    end
    else
      LI.ImageIndex := -1;
  except
    LI.ImageIndex := -1;
  end;


  LI.SubItems[0] := ite(RCSi.GetNeeded(addr), 'X', '');

  try
    if (RCSi.IsModule(addr)) then
      LI.SubItems[1] := RCSi.GetModuleName(addr)
    else
      LI.SubItems[1] := '-';
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
    if (not RCSi.Opened) then
    begin
      LI.SubItems[5] := '-';
      LI.SubItems[6] := '-';
      Exit();
    end;

    if (not RCSi.IsNonFailedModule(addr)) then
    begin
      if (RCSi.IsModuleFailure(addr)) then
        LI.SubItems[5] := 'Fail'
      else
        LI.SubItems[5] := '';
      LI.SubItems[6] := '-';
      Exit();
    end;

    if (RCSi.IsModuleError(addr)) then
      LI.SubItems[5] := 'Error'
    else if (RCSi.IsModuleWarning(addr)) then
      LI.SubItems[5] := 'Warning'
    else
      LI.SubItems[5] := '✓';

    LI.SubItems[6] := RCSi.GetModuleFW(addr);

  except
    on E: Exception do
    begin
      LI.SubItems[5] := 'Exception';
      LI.SubItems[6] := E.Message;
    end;
  end;
end;

procedure TRCSTableData.UpdateBoardInputs(addr: Integer);
begin
  if (not Self.AddrToLine.ContainsKey(addr)) then
    Exit();

  var LI: TListItem := Self.LV.Items[Self.AddrToLine[addr]];

  try
    if ((not RCSi.ready) or (not RCSi.Opened) or (not RCSi.IsNonFailedModule(addr)) or (not RCSi.Started)) then
    begin
      LI.SubItems[3] := '-';
      Exit();
    end;

    LI.SubItems[3] := '';

    var cnt := RCSi.GetModuleInputsCount(addr);
    if (cnt = 0) then
      Exit(); // Cannot decrement cnt-1, because cnt is Cardinal

    var start: Integer;
    if (RCSi.GetInput(addr, 0) = unavailablePort) then
      start := 1
    else
      start := 0;

    for var j := start to cnt - 1 do
    begin
      if (j = ((cnt + start) div 2)) then
        LI.SubItems[3] := LI.SubItems[3] + ' ';

      case (RCSi.GetInput(addr, j)) of
        isOn:
          LI.SubItems[3] := LI.SubItems[3] + '1';
        isOff:
          LI.SubItems[3] := LI.SubItems[3] + '0';
        failure:
          LI.SubItems[3] := LI.SubItems[3] + 'X';
        notYetScanned:
          LI.SubItems[3] := LI.SubItems[3] + '?';
        unavailableModule:
          LI.SubItems[3] := LI.SubItems[3] + '-';
      end;
    end;

  except
    on E: Exception do
      LI.SubItems[3] := E.Message;
  end;

end;

procedure TRCSTableData.UpdateBoardOutputs(addr: Integer);
begin
  if (not Self.AddrToLine.ContainsKey(addr)) then
    Exit();

  var LI: TListItem := Self.LV.Items[Self.AddrToLine[addr]];

  try
    if ((not RCSi.ready) or (not RCSi.Opened) or (not RCSi.IsNonFailedModule(addr)) or (not RCSi.Started)) then
    begin
      LI.SubItems[4] := '-';
      Exit();
    end;

    LI.SubItems[4] := '';

    var cnt := RCSi.GetModuleOutputsCount(addr);
    if (cnt = 0) then
      Exit(); // Cannot decrement cnt-1, because cnt is Cardinal

    for var j := 0 to cnt - 1 do
    begin
      if (j = cnt div 2) then
        LI.SubItems[4] := LI.SubItems[4] + ' ';

      var output := RCSi.GetOutput(addr, j);
      if (output > 1) then
        LI.SubItems[4] := LI.SubItems[4] + 'S'
      else
        LI.SubItems[4] := LI.SubItems[4] + IntToStr(output);
    end;

  except
    on E: Exception do
      LI.SubItems[4] := E.Message;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

RCSTableData.Free();

end.
