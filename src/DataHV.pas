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

uses THvDatabase, Prevody, fMain, Trakce, SprDb, RPConst;

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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateTable();
var i:Integer;
    repaint:boolean;
begin
 repaint := false;
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (not Assigned(HVDb.HVozidla[i])) then continue;
   if ((HVDb.HVozidla[i].changed) or (Self.reload)) then
    begin
     Self.UpdateLine(HVDb.HVozidla[i]);
     repaint := true;
    end;
  end;//for i

 Self.reload := false;
 if (repaint) then
   Self.LV.Repaint();
end;//procedure

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

  Self.LV.Items.Item[line].Caption := IntToStr(HV.adresa);
  Self.LV.Items.Item[line].SubItems.Strings[0] := data.Nazev;
  Self.LV.Items.Item[line].SubItems.Strings[1] := data.Oznaceni;
  Self.LV.Items.Item[line].SubItems.Strings[2] := data.Majitel;
  Self.LV.Items.Item[line].SubItems.Strings[3] := data.Poznamka;

  case (data.Trida) of
   THVClass.parni   : Self.LV.Items.Item[line].SubItems.Strings[4] := 'parní';
   THVClass.diesel  : Self.LV.Items.Item[line].SubItems.Strings[4] := 'diesel';
   THVClass.motor   : Self.LV.Items.Item[line].SubItems.Strings[4] := 'motor';
   THVClass.elektro : Self.LV.Items.Item[line].SubItems.Strings[4] := 'elektro';
  end;//case

  case (stav.StanovisteA) of
   lichy : Self.LV.Items.Item[line].SubItems.Strings[5] := 'lichý';
   sudy  : Self.LV.Items.Item[line].SubItems.Strings[5] := 'sudý';
  end;//case

  Self.LV.Items.Item[line].SubItems.Strings[16]  := Format('%5.2f',[stav.najeto_vpred.Metru]);
  Self.LV.Items.Item[line].SubItems.Strings[17]  := IntToStr(stav.najeto_vpred.Bloku);
  Self.LV.Items.Item[line].SubItems.Strings[18]  := Format('%5.2f',[stav.najeto_vzad.Metru]);
  Self.LV.Items.Item[line].SubItems.Strings[19]  := IntToStr(stav.najeto_vzad.Bloku);

  if (stav.stanice <> nil) then
    Self.LV.Items.Item[line].SubItems.Strings[6] := stav.stanice.Name
  else
    Self.LV.Items.Item[line].SubItems.Strings[6] := '';

  if (stav.souprava > -1) then
    Self.LV.Items.Item[line].SubItems.Strings[15] := Soupravy.GetSprNameByIndex(stav.souprava)
  else
    Self.LV.Items.Item[line].SubItems.Strings[15] := '-';

  case (slot.pom) of
   TPomStatus.progr    : Self.LV.Items.Item[line].SubItems.Strings[14] := 'progr';
   TPomStatus.error    : Self.LV.Items.Item[line].SubItems.Strings[14] := 'error';
   TPomStatus.pc       : Self.LV.Items.Item[line].SubItems.Strings[14] := 'automat';
   TPomStatus.released : Self.LV.Items.Item[line].SubItems.Strings[14] := 'ruèní';
  end;//case

 if (not HV.Slot.Prevzato) then
  begin
   // neprevzato
   Self.LV.Items.Item[line].SubItems.Strings[7]  := '---';
   Self.LV.Items.Item[line].SubItems.Strings[8]  := '---';
   Self.LV.Items.Item[line].SubItems.Strings[9]  := '?';
   Self.LV.Items.Item[line].SubItems.Strings[10]  := '????';
   Self.LV.Items.Item[line].SubItems.Strings[11]  := '????';
   Self.LV.Items.Item[line].SubItems.Strings[12]  := '????';

   if (slot.stolen) then
     Self.LV.Items.Item[line].SubItems.Strings[13] := 'ukradeno'
   else
     Self.LV.Items.Item[line].SubItems.Strings[13] := '---';
  end else begin
   // prevzato

   Self.LV.Items.Item[line].SubItems.Strings[7] := IntToStr(TrkSystem.GetStepSpeed(Slot.speed)) + 'km/h / '+IntToStr(Slot.speed)+' st';
   Self.LV.Items.Item[line].SubItems.Strings[8] := IntToStr(Slot.smer);
   Self.LV.Items.Item[line].SubItems.Strings[9] := IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[0]));

   str := '';
   for i := 1 to 4 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items.Item[line].SubItems.Strings[10] := str;

   str := '';
   for i := 5 to 8 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items.Item[line].SubItems.Strings[11] := str;

   str := '';
   for i := 9 to 12 do str := str + IntToStr(PrevodySoustav.BoolToInt(Slot.funkce[i]));
   Self.LV.Items.Item[line].SubItems.Strings[12] := str;

   if (slot.com_err) then
     Self.LV.Items.Item[line].SubItems.Strings[13] := 'COM ERROR!'
   else
     Self.LV.Items.Item[line].SubItems.Strings[13] := 'PC';
  end;//else not prevzato

 end;//procedure

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
end;//procedure

procedure THVTableData.RemoveHV(line:Integer);
begin
 Self.LV.Items.Delete(line);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 HVTableData.Free();

end.//unit
