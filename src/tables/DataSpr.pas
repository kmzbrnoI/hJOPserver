unit DataSpr;

// TTrainTableData - trida starajici se o vyplnovani tabulky souprav

interface

uses ComCtrls, SysUtils, StrUtils, Classes;

type
  TTrainTableData=class
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

////////////////////////////////////////////////////////////////////////////////

constructor TTrainTableData.Create(LV: TListView);
begin
 inherited Create();
 Self.reload := false;
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.LoadToTable();
var i, j: integer;
    LI: TListItem;
 begin
  Self.LV.Clear();

  for i := 0 to _MAX_TRAIN-1 do
   begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
   end;//for i
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.UpdateTable();
var i: Integer;
begin
 for i := 0 to _MAX_TRAIN-1 do
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

////////////////////////////////////////////////////////////////////////////////

procedure TTrainTableData.UpdateLine(line: integer);
var train: TTrainData;
    i: Integer;
    str: string;
 begin
  if (not Assigned(Trains[line])) then
   begin
    Self.LV.Items[line].Caption := '';

    for i := 0 to Self.LV.Items[line].SubItems.Count-1 do
      Self.LV.Items[line].SubItems[i] := '';

    Exit();
   end;

  Trains[line].changed := false;

  train := Trains[line].sdata;

  Self.LV.Items[line].Caption := IntToStr(line);

  Self.LV.Items[line].SubItems[0] := train.name;

  if (train.HVs.Count > 0) then
   Self.LV.Items[line].SubItems[1] := IntToStr(HVDb[train.HVs[0]].addr) + ' : ' +
       HVDb[train.HVs[0]].name + ' ('+HVDb[train.HVs[0]].data.designation+')'
  else
   Self.LV.Items[line].SubItems[1] := '-';

  str := '';
  for i := 1 to train.HVS.Count-1 do
      str := str + IntToStr(train.HVs[i]) + ', ';

  if (str <> '') then
    Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2)
  else
    Self.LV.Items[line].SubItems[2] := '-';

  Self.LV.Items[line].SubItems[3] := train.note;
  if (train.dir_L) then
    Self.LV.Items[line].SubItems[4] := 'L'
  else
    Self.LV.Items[line].SubItems[4] := '';

  if (train.dir_S) then
    Self.LV.Items[line].SubItems[4] := Self.LV.Items[line].SubItems[4] + 'S';

  Self.LV.Items[line].SubItems[5] := IntToStr(train.carsCount);

  if (train.maxSpeed <> 0) then
    Self.LV.Items[line].SubItems[6] := IntToStr(train.maxSpeed) + ' km/h'
  else
    Self.LV.Items[line].SubItems[6] := '-';

  Self.LV.Items[line].SubItems[7] := IntToStr(train.speed) + ' km/h';
  if (train.speed <> train.wantedSpeed) then
    Self.LV.Items[line].SubItems[7] := Self.LV.Items[line].SubItems[7] + ' (' + IntToStr(train.wantedSpeed) + ' km/h)';

  case (train.direction) of
   THVSite.odd: Self.LV.Items[line].SubItems[8] := 'lichý';
   THVSite.even : Self.LV.Items[line].SubItems[8] := 'sudý';
  end;

  try
    if (train.area <> nil) then
      Self.LV.Items[line].SubItems[9] := (train.area as TArea).name
    else
      Self.LV.Items[line].SubItems[9] := '-';
  except
   Self.LV.Items[line].SubItems[9] := '-';
  end;

  if (train.front <> nil) then
    Self.LV.Items[line].SubItems[10] := (train.front as TBlk).name
  else
    Self.LV.Items[line].SubItems[10] := '-';

  Self.LV.Items[line].SubItems[11] := IntToStr(train.length);
  Self.LV.Items[line].SubItems[12] := train.typ;

  if (train.areaFrom <> nil) then
    Self.LV.Items[line].SubItems[13] := TArea(train.areaFrom).name
  else
    Self.LV.Items[line].SubItems[13] := '-';

  if (train.areaTo <> nil) then
    Self.LV.Items[line].SubItems[14] := TArea(train.areaTo).name
  else
    Self.LV.Items[line].SubItems[14] := '-';

  Self.LV.Items[line].SubItems[15] := ownConvert.BoolToTick(train.announcement);
 end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 TrainTableData.Free();

end.//unit
