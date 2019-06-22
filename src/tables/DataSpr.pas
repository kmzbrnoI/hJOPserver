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
     if ((Assigned(Soupravy.soupravy[i])) and (Soupravy.soupravy[i].changed)) then
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
  if (not Assigned(Soupravy.soupravy[line])) then
   begin
    Self.LV.Items.Item[line].Caption := '';

    for i := 0 to Self.LV.Items.Item[line].SubItems.Count-1 do
      Self.LV.Items.Item[line].SubItems.Strings[i] := '';

    Exit();
   end;

  Soupravy.soupravy[line].changed := false;

  spr := Soupravy.soupravy[line].sdata;

  Self.LV.Items.Item[line].Caption := IntToStr(line);

  Self.LV.Items.Item[line].SubItems.Strings[0] := spr.nazev;

  if (spr.HVs.Count > 0) then
   Self.LV.Items.Item[line].SubItems.Strings[1] := IntToStr(HVDb.HVozidla[spr.HVs[0]].adresa) + ' : ' +
       HVDb.HVozidla[spr.HVs[0]].Data.Nazev + ' ('+HVDb.HVozidla[spr.HVs[0]].Data.Oznaceni+')'
  else
   Self.LV.Items.Item[line].SubItems.Strings[1] := '-';

  str := '';
  for i := 1 to spr.HVS.Count-1 do
      str := str + IntToStr(spr.HVs[i]) + ', ';

  if (str <> '') then
    Self.LV.Items.Item[line].SubItems.Strings[2] := LeftStr(str, Length(str)-2)
  else
    Self.LV.Items.Item[line].SubItems.Strings[2] := '-';

  Self.LV.Items.Item[line].SubItems.Strings[3] := spr.poznamka;
  if (spr.smer_L) then
    Self.LV.Items.Item[line].SubItems.Strings[4] := 'L'
  else
    Self.LV.Items.Item[line].SubItems.Strings[4] := '';

  if (spr.smer_S) then
    Self.LV.Items.Item[line].SubItems.Strings[4] := Self.LV.Items.Item[line].SubItems.Strings[4] + 'S';

  Self.LV.Items.Item[line].SubItems.Strings[5] := IntToStr(spr.pocet_vozu);

  Self.LV.Items.Item[line].SubItems.Strings[6] := IntToStr(spr.rychlost) + ' km/h';

  case (spr.smer) of
   THVStanoviste.lichy: Self.LV.Items.Item[line].SubItems.Strings[7] := 'lich�';
   THVStanoviste.sudy : Self.LV.Items.Item[line].SubItems.Strings[7] := 'sud�';
  end;

  try
    if (spr.OblRizeni <> nil) then
      Self.LV.Items.Item[line].SubItems.Strings[8] := (spr.OblRizeni as TOR).Name
    else
      Self.LV.Items.Item[line].SubItems.Strings[8] := '-';
  except
   Self.LV.Items.Item[line].SubItems.Strings[8] := '-';
  end;

  if (spr.front <> nil) then
    Self.LV.Items.Item[line].SubItems.Strings[9] := (spr.front as TBlk).name
  else
    Self.LV.Items.Item[line].SubItems.Strings[9] := '-';

  Self.LV.Items.Item[line].SubItems.Strings[10] := IntToStr(spr.delka);
  Self.LV.Items.Item[line].SubItems.Strings[11] := spr.typ;

  if (spr.vychoziOR <> nil) then
    Self.LV.Items.Item[line].SubItems.Strings[12] := TOR(spr.vychoziOR).Name
  else
    Self.LV.Items.Item[line].SubItems.Strings[12] := '-';

  if (spr.cilovaOR <> nil) then
    Self.LV.Items.Item[line].SubItems.Strings[13] := TOR(spr.cilovaOR).Name
  else
    Self.LV.Items.Item[line].SubItems.Strings[13] := '-';

  if (spr.hlaseni) then
    Self.LV.Items.Item[line].SubItems.Strings[14] := 'Ano'
  else
    Self.LV.Items.Item[line].SubItems.Strings[14] := 'Ne';
 end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 SprTableData.Free();

end.//unit
