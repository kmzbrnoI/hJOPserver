unit DataRV;

// TRVTableData - trida starajici se o vyplnovani tabulky vozidel

interface

uses TRailVehicle, ComCtrls, SysUtils, Classes;

type
  TRVTableData = class
  private
    LV: TListView;

  public

    reload: Boolean;

    procedure LoadToTable();
    procedure UpdateLine(vehicle: TRV);
    procedure UpdateTable();

    procedure AddRV(line: Integer; vehicle: TRV);
    procedure RemoveRV(line: Integer);

    constructor Create(LV: TListView);
  end;

var
  RVTableData: TRVTableData;

implementation

uses TRvDatabase, ownConvert, fMain, TrakceC, TrainDb;

/// /////////////////////////////////////////////////////////////////////////////

constructor TRVTableData.Create(LV: TListView);
begin
  inherited Create;
  Self.LV := LV;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVTableData.LoadToTable();
begin
  Self.LV.Clear();

  for var i := 0 to _MAX_ADDR - 1 do
  begin
    if (RVDb[i] = nil) then
      continue;

    var LI: TListItem := Self.LV.Items.Add;
    LI.Caption := IntToStr(i);

    for var j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('');

    Self.UpdateLine(RVDb[i]);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVTableData.UpdateTable();
begin
  for var i := 0 to _MAX_ADDR - 1 do
  begin
    if (not Assigned(RVDb[i])) then
      continue;
    if ((RVDb[i].changed) or (Self.reload)) then
    begin
      Self.UpdateLine(RVDb[i]);
      Self.LV.UpdateItems(RVDb[i].index, RVDb[i].index);
    end;
  end;

  Self.reload := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVTableData.UpdateLine(vehicle: TRV);
var str: string;
begin
  if (vehicle = nil) then
    Exit();

  vehicle.changed := false;

  var line := vehicle.index;
  var data := vehicle.Data;
  var state := vehicle.state;
  var slot := vehicle.slot;

  Self.LV.Items[line].Caption := IntToStr(vehicle.addr);
  Self.LV.Items[line].SubItems[0] := data.name;
  Self.LV.Items[line].SubItems[1] := data.designation;
  Self.LV.Items[line].SubItems[2] := data.owner;
  Self.LV.Items[line].SubItems[3] := data.note;

  case (data.typ) of
    TRVType.other:
      Self.LV.Items[line].SubItems[4] := 'jiný';
    TRVType.steam:
      Self.LV.Items[line].SubItems[4] := 'parní';
    TRVType.diesel:
      Self.LV.Items[line].SubItems[4] := 'diesel';
    TRVType.motor:
      Self.LV.Items[line].SubItems[4] := 'motor';
    TRVType.electro:
      Self.LV.Items[line].SubItems[4] := 'elektro';
    TRVType.car:
      Self.LV.Items[line].SubItems[4] := 'vůz';
  end; // case

  Self.LV.Items[line].SubItems[5] := IntToStr(data.transience);

  case (state.siteA) of
    odd:
      Self.LV.Items[line].SubItems[6] := 'lichý';
    even:
      Self.LV.Items[line].SubItems[6] := 'sudý';
  end;

  Self.LV.Items[line].SubItems[20] := Format('%.2f', [state.traveled_forward]);
  Self.LV.Items[line].SubItems[21] := Format('%.2f', [state.traveled_backward]);

  if (state.area <> nil) then
    Self.LV.Items[line].SubItems[7] := state.area.name
  else
    Self.LV.Items[line].SubItems[7] := '';

  Self.LV.Items[line].SubItems[8] := IntToStr(data.maxSpeed) + ' km/h';

  if (state.train > -1) then
    Self.LV.Items[line].SubItems[19] := Trains.GetTrainNameByIndex(state.train)
  else
    Self.LV.Items[line].SubItems[19] := '-';

  case (state.pom) of
    TPomStatus.unknown:
      Self.LV.Items[line].SubItems[18] := '?';
    TPomStatus.progr:
      Self.LV.Items[line].SubItems[18] := 'progr';
    TPomStatus.error:
      Self.LV.Items[line].SubItems[18] := 'error';
    TPomStatus.automat:
      Self.LV.Items[line].SubItems[18] := 'automat';
    TPomStatus.manual:
      Self.LV.Items[line].SubItems[18] := 'ruční';
  end; // case

  if ((not state.acquired) and (not state.stolen)) then
  begin
    // neprevzato
    Self.LV.Items[line].SubItems[9] := '---';
    Self.LV.Items[line].SubItems[10] := '---';
    Self.LV.Items[line].SubItems[11] := '?';
    Self.LV.Items[line].SubItems[12] := '????';
    Self.LV.Items[line].SubItems[13] := '????';
    Self.LV.Items[line].SubItems[14] := '????';
    Self.LV.Items[line].SubItems[15] := '???? ????';
    Self.LV.Items[line].SubItems[16] := '???? ????';
    Self.LV.Items[line].SubItems[17] := '---';
  end else begin
    // prevzato

    Self.LV.Items[line].SubItems[9] := IntToStr(vehicle.realSpeed) + 'km/h / ' + IntToStr(slot.step) + ' st';
    Self.LV.Items[line].SubItems[10] := ownConvert.BoolToStr10(slot.direction);
    Self.LV.Items[line].SubItems[11] := ownConvert.BoolToStr10(vehicle.slotFunctions[0]);

    str := '';
    for var i := 1 to 4 do
      str := str + ownConvert.BoolToStr10(vehicle.slotFunctions[i]);
    Self.LV.Items[line].SubItems[12] := str;

    str := '';
    for var i := 5 to 8 do
      str := str + ownConvert.BoolToStr10(vehicle.slotFunctions[i]);
    Self.LV.Items[line].SubItems[13] := str;

    str := '';
    for var i := 9 to 12 do
      str := str + ownConvert.BoolToStr10(vehicle.slotFunctions[i]);
    Self.LV.Items[line].SubItems[14] := str;

    str := '';
    for var i := 13 to 20 do
    begin
      str := str + ownConvert.BoolToStr10(vehicle.slotFunctions[i]);
      if (i = 16) then
        str := str + ' ';
    end;
    Self.LV.Items[line].SubItems[15] := str;

    str := '';
    for var i := 21 to 28 do
    begin
      str := str + ownConvert.BoolToStr10(vehicle.slotFunctions[i]);
      if (i = 24) then
        str := str + ' ';
    end;
    Self.LV.Items[line].SubItems[16] := str;

    if (state.trakceError) then
      Self.LV.Items[line].SubItems[17] := 'COM ERROR!'
    else if (state.stolen) then
      Self.LV.Items[line].SubItems[17] := 'ukradeno'
    else
      Self.LV.Items[line].SubItems[17] := 'PC';
  end; // else not prevzato

  if (state.train > -1) then
    Self.LV.Items[line].SubItems[22] := 'teď'
  else if (state.last_used > 0) then
    Self.LV.Items[line].SubItems[22] := FormatDateTime('yyyy-mm-dd hh:nn:ss', state.last_used)
  else
    Self.LV.Items[line].SubItems[22] := '-';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TRVTableData.AddRV(line: Integer; vehicle: TRV);
var LI: TListItem;
  addr: Pointer;
begin
  LI := Self.LV.Items.Insert(line);

  GetMem(addr, 3);
  Integer(addr^) := vehicle.addr;
  LI.Caption := IntToStr(vehicle.addr);

  for var i := 0 to Self.LV.Columns.Count - 1 do
    LI.SubItems.Add('');

  Self.UpdateLine(vehicle);
end;

procedure TRVTableData.RemoveRV(line: Integer);
begin
  Self.LV.Items.Delete(line);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

RVTableData.Free();

end.// unit
