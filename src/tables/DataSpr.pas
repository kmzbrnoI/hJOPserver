unit DataSpr;

// TSprTableData - trida starajici se o vyplnovani tabulky souprav

interface

uses ComCtrls, SysUtils, StrUtils;

type
  TSprTableData=class
    private
      LV:TListView;

    public

     reload:boolean;

      procedure LoadToTable();
      procedure UpdateLine(line:integer);
      procedure UpdateTable();

      constructor Create(LV:TListView);
  end;

var
   SprTableData:TSprTableData;


implementation

uses SprDb, Souprava, THVDatabase, TOblsRizeni, TOblRizeni, fMain,
       TBloky, TBlok, THnaciVozidlo;

////////////////////////////////////////////////////////////////////////////////

constructor TSprTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.reload := false;
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TSprTableData.LoadToTable();
var i, j:integer;
    LI:TListItem;
 begin
  Self.LV.Clear();

  for i := 0 to _MAX_SPR-1 do
   begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
   end;//for i
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to _MAX_SPR-1 do
  begin
   if (Self.reload) then
    begin
     Self.UpdateLine(i);
     Self.LV.UpdateItems(i, i);
    end else begin
     if ((Assigned(Soupravy[i])) and (Soupravy[i].changed)) then
      begin
       Self.UpdateLine(i);
       Self.LV.UpdateItems(i, i);
      end;
    end;
  end;

 Self.reload := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TSprTableData.UpdateLine(line:integer);
var spr:TSoupravaData;
    i:Integer;
    str:string;
 begin
  if (not Assigned(Soupravy[line])) then
   begin
    Self.LV.Items[line].Caption := '';

    for i := 0 to Self.LV.Items[line].SubItems.Count-1 do
      Self.LV.Items[line].SubItems[i] := '';

    Exit();
   end;

  Soupravy[line].changed := false;

  spr := Soupravy[line].sdata;

  Self.LV.Items[line].Caption := IntToStr(line);

  Self.LV.Items[line].SubItems[0] := spr.name;

  if (spr.HVs.Count > 0) then
   Self.LV.Items[line].SubItems[1] := IntToStr(HVDb[spr.HVs[0]].adresa) + ' : ' +
       HVDb[spr.HVs[0]].Data.Nazev + ' ('+HVDb[spr.HVs[0]].Data.Oznaceni+')'
  else
   Self.LV.Items[line].SubItems[1] := '-';

  str := '';
  for i := 1 to spr.HVS.Count-1 do
      str := str + IntToStr(spr.HVs[i]) + ', ';

  if (str <> '') then
    Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2)
  else
    Self.LV.Items[line].SubItems[2] := '-';

  Self.LV.Items[line].SubItems[3] := spr.note;
  if (spr.dir_L) then
    Self.LV.Items[line].SubItems[4] := 'L'
  else
    Self.LV.Items[line].SubItems[4] := '';

  if (spr.dir_S) then
    Self.LV.Items[line].SubItems[4] := Self.LV.Items[line].SubItems[4] + 'S';

  Self.LV.Items[line].SubItems[5] := IntToStr(spr.carsCount);

  if (spr.maxSpeed <> 0) then
    Self.LV.Items[line].SubItems[6] := IntToStr(spr.maxSpeed) + ' km/h'
  else
    Self.LV.Items[line].SubItems[6] := '-';

  Self.LV.Items[line].SubItems[7] := IntToStr(spr.speed) + ' km/h';
  if (spr.speed <> spr.wantedSpeed) then
    Self.LV.Items[line].SubItems[7] := Self.LV.Items[line].SubItems[7] + ' (' + IntToStr(spr.wantedSpeed) + ' km/h)';

  case (spr.direction) of
   THVStanoviste.lichy: Self.LV.Items[line].SubItems[8] := 'lichý';
   THVStanoviste.sudy : Self.LV.Items[line].SubItems[8] := 'sudý';
  end;

  try
    if (spr.station <> nil) then
      Self.LV.Items[line].SubItems[9] := (spr.station as TOR).Name
    else
      Self.LV.Items[line].SubItems[9] := '-';
  except
   Self.LV.Items[line].SubItems[9] := '-';
  end;

  if (spr.front <> nil) then
    Self.LV.Items[line].SubItems[10] := (spr.front as TBlk).name
  else
    Self.LV.Items[line].SubItems[10] := '-';

  Self.LV.Items[line].SubItems[11] := IntToStr(spr.length);
  Self.LV.Items[line].SubItems[12] := spr.typ;

  if (spr.stationFrom <> nil) then
    Self.LV.Items[line].SubItems[13] := TOR(spr.stationFrom).Name
  else
    Self.LV.Items[line].SubItems[13] := '-';

  if (spr.stationTo <> nil) then
    Self.LV.Items[line].SubItems[14] := TOR(spr.stationTo).Name
  else
    Self.LV.Items[line].SubItems[14] := '-';

  if (spr.announcement) then
    Self.LV.Items[line].SubItems[15] := 'Ano'
  else
    Self.LV.Items[line].SubItems[15] := 'Ne';
 end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 SprTableData.Free();

end.//unit
