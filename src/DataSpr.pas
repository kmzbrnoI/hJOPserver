unit DataSpr;

// TSprTableData - trida starajici se o vyplnovani tabulky souprav

interface

uses ComCtrls, SysUtils;

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
  end;//TMTBData

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
 end;//procedure

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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TSprTableData.UpdateLine(line:integer);
var spr:TSoupravaData;
    i:Integer;
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

  if (spr.HV.cnt > 0) then
   Self.LV.Items.Item[line].SubItems.Strings[1] := IntToStr(HVDb.HVozidla[spr.HV.HVs[0]].adresa) + ' : ' +
       HVDb.HVozidla[spr.HV.HVs[0]].Data.Nazev + ' ('+HVDb.HVozidla[spr.HV.HVs[0]].Data.Oznaceni+')'
  else
   Self.LV.Items.Item[line].SubItems.Strings[1] := '-';

  if ((spr.HV.cnt > 1) and (Assigned(HVDb.HVozidla[spr.HV.HVs[1]]))) then
   Self.LV.Items.Item[line].SubItems.Strings[2] := IntToStr(HVDb.HVozidla[spr.HV.HVs[1]].adresa) + ' : ' +
       HVDb.HVozidla[spr.HV.HVs[1]].Data.Nazev + ' ('+HVDb.HVozidla[spr.HV.HVs[1]].Data.Oznaceni+')'
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
   THVStanoviste.lichy: Self.LV.Items.Item[line].SubItems.Strings[7] := 'lichý';
   THVStanoviste.sudy : Self.LV.Items.Item[line].SubItems.Strings[7] := 'sudý';
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
    Self.LV.Items.Item[line].SubItems.Strings[9] := (spr.front as TBlk).GetGlobalSettings().name
  else
    Self.LV.Items.Item[line].SubItems.Strings[9] := '-';

  Self.LV.Items.Item[line].SubItems.Strings[10] := IntToStr(spr.delka);
  Self.LV.Items.Item[line].SubItems.Strings[11] := spr.typ;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 SprTableData.Free();

end.//unit
