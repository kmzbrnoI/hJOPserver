unit DataSpr;

// TTrainTableData - trida starajici se o vyplnovani tabulky souprav

interface

uses ComCtrls, SysUtils, StrUtils, Classes;

type
  TTrainTableData = class
  private
    LV: TListView;

  public

    reload: Boolean;

    procedure LoadToTable();
    procedure UpdateLine(line: integer);
    procedure UpdateTable();

    constructor Create(LV: TListView);
  end;

var
  TrainTableData: TTrainTableData;

implementation

uses TrainDb, Train, THVDatabase, AreaDb, Area, fMain,
  BlockDb, Block, THnaciVozidlo, ownConvert;

/// /////////////////////////////////////////////////////////////////////////////

constructor TTrainTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.reload := false;
  Self.LV := LV;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.LoadToTable();
var i, j: integer;
  LI: TListItem;
begin
  Self.LV.Clear();

  for i := 0 to _MAX_TRAIN - 1 do
  begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count - 1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
  end; // for i
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.UpdateTable();
var i: integer;
begin
  for i := 0 to _MAX_TRAIN - 1 do
  begin
    if (Self.reload) then
    begin
      Self.UpdateLine(i);
      Self.LV.UpdateItems(i, i);
    end else begin
      if ((Assigned(Trains[i])) and (Trains[i].changed)) then
      begin
        Self.UpdateLine(i);
        Self.LV.UpdateItems(i, i);
      end;
    end;
  end;

  Self.reload := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.UpdateLine(line: integer);
var Train: TTrainData;
  i: integer;
  str: string;
begin
  if (not Assigned(Trains[line])) then
  begin
    Self.LV.Items[line].Caption := '';

    for i := 0 to Self.LV.Items[line].SubItems.Count - 1 do
      Self.LV.Items[line].SubItems[i] := '';

    Exit();
  end;

  Trains[line].changed := false;

  Train := Trains[line].sdata;

  Self.LV.Items[line].Caption := IntToStr(line);

  Self.LV.Items[line].SubItems[0] := Train.name;

  if (Train.HVs.Count > 0) then
    Self.LV.Items[line].SubItems[1] := IntToStr(HVDb[Train.HVs[0]].addr) + ' : ' + HVDb[Train.HVs[0]].name + ' (' +
      HVDb[Train.HVs[0]].data.designation + ')'
  else
    Self.LV.Items[line].SubItems[1] := '-';

  str := '';
  for i := 1 to Train.HVs.Count - 1 do
    str := str + IntToStr(Train.HVs[i]) + ', ';

  if (str <> '') then
    Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str) - 2)
  else
    Self.LV.Items[line].SubItems[2] := '-';

  Self.LV.Items[line].SubItems[3] := Train.note;
  if (Train.dir_L) then
    Self.LV.Items[line].SubItems[4] := 'L'
  else
    Self.LV.Items[line].SubItems[4] := '';

  if (Train.dir_S) then
    Self.LV.Items[line].SubItems[4] := Self.LV.Items[line].SubItems[4] + 'S';

  Self.LV.Items[line].SubItems[5] := IntToStr(Train.carsCount);

  if (Train.maxSpeed <> 0) then
    Self.LV.Items[line].SubItems[6] := IntToStr(Train.maxSpeed) + ' km/h'
  else
    Self.LV.Items[line].SubItems[6] := '-';

  Self.LV.Items[line].SubItems[7] := IntToStr(Train.speed) + ' km/h';
  if (Train.speed <> Train.wantedSpeed) then
    Self.LV.Items[line].SubItems[7] := Self.LV.Items[line].SubItems[7] + ' (' + IntToStr(Train.wantedSpeed) + ' km/h)';

  case (Train.direction) of
    THVSite.odd:
      Self.LV.Items[line].SubItems[8] := 'lichý';
    THVSite.even:
      Self.LV.Items[line].SubItems[8] := 'sudý';
  end;

  try
    if (Train.Area <> nil) then
      Self.LV.Items[line].SubItems[9] := (Train.Area as TArea).name
    else
      Self.LV.Items[line].SubItems[9] := '-';
  except
    Self.LV.Items[line].SubItems[9] := '-';
  end;

  if (Train.front <> nil) then
    Self.LV.Items[line].SubItems[10] := (Train.front as TBlk).name
  else
    Self.LV.Items[line].SubItems[10] := '-';

  Self.LV.Items[line].SubItems[11] := IntToStr(Train.Length);
  Self.LV.Items[line].SubItems[12] := Train.typ;

  if (Train.areaFrom <> nil) then
    Self.LV.Items[line].SubItems[13] := TArea(Train.areaFrom).name
  else
    Self.LV.Items[line].SubItems[13] := '-';

  if (Train.areaTo <> nil) then
    Self.LV.Items[line].SubItems[14] := TArea(Train.areaTo).name
  else
    Self.LV.Items[line].SubItems[14] := '-';

  Self.LV.Items[line].SubItems[15] := ownConvert.BoolToTick(Train.announcement);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

TrainTableData.Free();

end.// unit
