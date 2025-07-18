unit DataHV;

// THVTableData - trida starajici se o vyplnovani tabulky hnacich vozidel

interface

uses THnaciVozidlo, ComCtrls, SysUtils, Classes;

type
  THVTableData = class
  private
    LV: TListView;

  public

    reload: Boolean;

    procedure LoadToTable();
    procedure UpdateLine(HV: THV);
    procedure UpdateTable();

    procedure AddHV(line: Integer; HV: THV);
    procedure RemoveHV(line: Integer);

    constructor Create(LV: TListView);
  end;

var
  HVTableData: THVTableData;

implementation

uses THvDatabase, ownConvert, fMain, Trakce, TrainDb;

/// /////////////////////////////////////////////////////////////////////////////

constructor THVTableData.Create(LV: TListView);
begin
  inherited Create;
  Self.LV := LV;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVTableData.LoadToTable();
begin
  Self.LV.Clear();

  for var i := 0 to _MAX_ADDR - 1 do
  begin
    if (HVDb[i] = nil) then
      continue;

    var LI: TListItem := Self.LV.Items.Add;
    LI.Caption := IntToStr(i);

    for var j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('');

    Self.UpdateLine(HVDb[i]);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateTable();
begin
  for var i := 0 to _MAX_ADDR - 1 do
  begin
    if (not Assigned(HVDb[i])) then
      continue;
    if ((HVDb[i].changed) or (Self.reload)) then
    begin
      Self.UpdateLine(HVDb[i]);
      Self.LV.UpdateItems(HVDb[i].index, HVDb[i].index);
    end;
  end;

  Self.reload := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateLine(HV: THV);
var str: string;
begin
  if (HV = nil) then
    Exit();

  HV.changed := false;

  var line := HV.index;
  var data := HV.Data;
  var state := HV.state;
  var slot := HV.slot;

  Self.LV.Items[line].Caption := IntToStr(HV.addr);
  Self.LV.Items[line].SubItems[0] := data.name;
  Self.LV.Items[line].SubItems[1] := data.designation;
  Self.LV.Items[line].SubItems[2] := data.owner;
  Self.LV.Items[line].SubItems[3] := data.note;

  case (data.typ) of
    THVType.other:
      Self.LV.Items[line].SubItems[4] := 'jiný';
    THVType.steam:
      Self.LV.Items[line].SubItems[4] := 'parní';
    THVType.diesel:
      Self.LV.Items[line].SubItems[4] := 'diesel';
    THVType.motor:
      Self.LV.Items[line].SubItems[4] := 'motor';
    THVType.electro:
      Self.LV.Items[line].SubItems[4] := 'elektro';
    THVType.car:
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

    Self.LV.Items[line].SubItems[9] := IntToStr(HV.realSpeed) + 'km/h / ' + IntToStr(slot.step) + ' st';
    Self.LV.Items[line].SubItems[10] := IntToStr(ownConvert.BoolToInt(slot.direction));
    Self.LV.Items[line].SubItems[11] := IntToStr(ownConvert.BoolToInt(HV.slotFunctions[0]));

    str := '';
    for var i := 1 to 4 do
      str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunctions[i]));
    Self.LV.Items[line].SubItems[12] := str;

    str := '';
    for var i := 5 to 8 do
      str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunctions[i]));
    Self.LV.Items[line].SubItems[13] := str;

    str := '';
    for var i := 9 to 12 do
      str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunctions[i]));
    Self.LV.Items[line].SubItems[14] := str;

    str := '';
    for var i := 13 to 20 do
    begin
      str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunctions[i]));
      if (i = 16) then
        str := str + ' ';
    end;
    Self.LV.Items[line].SubItems[15] := str;

    str := '';
    for var i := 21 to 28 do
    begin
      str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunctions[i]));
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

procedure THVTableData.AddHV(line: Integer; HV: THV);
var LI: TListItem;
  addr: Pointer;
begin
  LI := Self.LV.Items.Insert(line);

  GetMem(addr, 3);
  Integer(addr^) := HV.addr;
  LI.Caption := IntToStr(HV.addr);

  for var i := 0 to Self.LV.Columns.Count - 1 do
    LI.SubItems.Add('');

  Self.UpdateLine(HV);
end;

procedure THVTableData.RemoveHV(line: Integer);
begin
  Self.LV.Items.Delete(line);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

HVTableData.Free();

end.// unit
