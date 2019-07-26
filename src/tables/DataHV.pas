unit DataHV;

// THVTableData - trida starajici se o vyplnovani tabulky hnacich vozidel

interface

uses THnaciVozidlo, ComCtrls, SysUtils;

type
  THVTableData=class
    private
      LV:TListView;

    public

     reload:boolean;

      procedure LoadToTable();
      procedure UpdateLine(HV:THV);
      procedure UpdateTable();

      procedure AddHV(line:Integer; HV:THV);
      procedure RemoveHV(line:Integer);

      constructor Create(LV:TListView);
  end;

var
   HVTableData:THVTableData;

implementation

uses THvDatabase, Prevody, fMain, Trakce, SprDb;

////////////////////////////////////////////////////////////////////////////////

constructor THVTableData.Create(LV:TListView);
begin
 inherited Create;
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.LoadToTable();
var i, j:Integer;
    LI:TListItem;
    addr:Pointer;
begin
 Self.LV.Clear();

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb.HVozidla[i] = nil) then continue;

   GetMem(addr, 3);
   Integer(addr^) := HVDb.HVozidla[i].adresa;
   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);
   LI.Data := addr;

   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('');

   Self.UpdateLine(HVDb.HVozidla[i]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (not Assigned(HVDb.HVozidla[i])) then continue;
   if ((HVDb.HVozidla[i].changed) or (Self.reload)) then
    begin
     Self.UpdateLine(HVDb.HVozidla[i]);
     Self.LV.UpdateItems(HVDb.HVozidla[i].index, HVDb.HVozidla[i].index);
    end;
  end;//for i

 Self.reload := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateLine(HV:THV);
var line:Integer;
    data:THVData;
    stav:THVStav;
    slot:TSlot;
    i:Integer;
    str:string;
 begin
  if (HV = nil) then Exit;

  HV.changed := false;

  line := HV.index;
  data := HV.data;
  stav := HV.stav;
  slot := HV.Slot;

  Self.LV.Items[line].Caption := IntToStr(HV.adresa);
  Self.LV.Items[line].SubItems[0] := data.Nazev;
  Self.LV.Items[line].SubItems[1] := data.Oznaceni;
  Self.LV.Items[line].SubItems[2] := data.Majitel;
  Self.LV.Items[line].SubItems[3] := data.Poznamka;

  case (data.Trida) of
   THVClass.parni   : Self.LV.Items[line].SubItems[4] := 'parní';
   THVClass.diesel  : Self.LV.Items[line].SubItems[4] := 'diesel';
   THVClass.motor   : Self.LV.Items[line].SubItems[4] := 'motor';
   THVClass.elektro : Self.LV.Items[line].SubItems[4] := 'elektro';
  end;//case

  case (stav.StanovisteA) of
   lichy : Self.LV.Items[line].SubItems[5] := 'lichý';
   sudy  : Self.LV.Items[line].SubItems[5] := 'sudý';
  end;//case

  Self.LV.Items[line].SubItems[19]  := Format('%5.2f',[stav.najeto_vpred.Metru]);
  Self.LV.Items[line].SubItems[20]  := IntToStr(stav.najeto_vpred.Bloku);
  Self.LV.Items[line].SubItems[21]  := Format('%5.2f',[stav.najeto_vzad.Metru]);
  Self.LV.Items[line].SubItems[22]  := IntToStr(stav.najeto_vzad.Bloku);

  if (stav.stanice <> nil) then
    Self.LV.Items[line].SubItems[6] := stav.stanice.Name
  else
    Self.LV.Items[line].SubItems[6] := '';

  Self.LV.Items[line].SubItems[7] := IntToStr(data.maxRychlost) + ' km/h';

  if (stav.souprava > -1) then
    Self.LV.Items[line].SubItems[18] := Soupravy.GetSprNameByIndex(stav.souprava)
  else
    Self.LV.Items[line].SubItems[18] := '-';

  case (slot.pom) of
   TPomStatus.progr    : Self.LV.Items[line].SubItems[17] := 'progr';
   TPomStatus.error    : Self.LV.Items[line].SubItems[17] := 'error';
   TPomStatus.pc       : Self.LV.Items[line].SubItems[17] := 'automat';
   TPomStatus.released : Self.LV.Items[line].SubItems[17] := 'ruèní';
  end;//case

 if (not HV.Slot.Prevzato) then
  begin
   // neprevzato
   Self.LV.Items[line].SubItems[8]  := '---';
   Self.LV.Items[line].SubItems[9]  := '---';
   Self.LV.Items[line].SubItems[10]  := '?';
   Self.LV.Items[line].SubItems[11]  := '????';
   Self.LV.Items[line].SubItems[12]  := '????';
   Self.LV.Items[line].SubItems[13]  := '????';
   Self.LV.Items[line].SubItems[14]  := '???? ????';
   Self.LV.Items[line].SubItems[15]  := '???? ????';

   if (slot.stolen) then
     Self.LV.Items[line].SubItems[16] := 'ukradeno'
   else
     Self.LV.Items[line].SubItems[16] := '---';
  end else begin
   // prevzato

   Self.LV.Items[line].SubItems[8] := IntToStr(TrkSystem.GetStepSpeed(Slot.speed)) + 'km/h / '+IntToStr(Slot.speed)+' st';
   Self.LV.Items[line].SubItems[9] := IntToStr(Slot.smer);
   Self.LV.Items[line].SubItems[10] := IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[0]));

   str := '';
   for i := 1 to 4 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items[line].SubItems[11] := str;

   str := '';
   for i := 5 to 8 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items[line].SubItems[12] := str;

   str := '';
   for i := 9 to 12 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items[line].SubItems[13] := str;

   str := '';
   for i := 13 to 20 do
    begin
     str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
     if (i = 16) then str := str + ' ';
    end;
   Self.LV.Items[line].SubItems[14] := str;

   str := '';
   for i := 21 to 28 do
    begin
     str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
     if (i = 24) then str := str + ' ';
    end;
   Self.LV.Items[line].SubItems[15] := str;

   if (slot.com_err) then
     Self.LV.Items[line].SubItems[16] := 'COM ERROR!'
   else
     Self.LV.Items[line].SubItems[16] := 'PC';
  end;//else not prevzato

  if (stav.souprava > -1) then
    Self.LV.Items[line].SubItems[23] := 'teï'
  else if (stav.last_used > 0) then
    Self.LV.Items[line].SubItems[23] := FormatDateTime('yyyy-mm-dd hh:nn:ss', stav.last_used)
  else
    Self.LV.Items[line].SubItems[23] := '-';
 end;

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.AddHV(line:Integer; HV:THV);
var i:Integer;
    LI:TListItem;
    addr:Pointer;
begin
 LI := Self.LV.Items.Insert(line);

 GetMem(addr, 3);
 Integer(addr^) := HV.adresa;
 LI.Data := addr;
 LI.Caption := IntToStr(HV.adresa);    // = adresa

 for i := 0 to Self.LV.Columns.Count-1 do
  LI.Subitems.Add('');

 Self.UpdateLine(HV);
end;

procedure THVTableData.RemoveHV(line:Integer);
begin
 Self.LV.Items.Delete(line);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 HVTableData.Free();

end.//unit
