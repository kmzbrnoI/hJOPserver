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

uses THvDatabase, ownConvert, fMain, Trakce, TrainDb;

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
begin
 Self.LV.Clear();

 for i := 0 to _MAX_ADDR-1 do
  begin
   if (HVDb[i] = nil) then continue;

   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);
   LI.Data := Pointer(Integer(HVDb[i].adresa));

   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('');

   Self.UpdateLine(HVDb[i]);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to _MAX_ADDR-1 do
  begin
   if (not Assigned(HVDb[i])) then continue;
   if ((HVDb[i].changed) or (Self.reload)) then
    begin
     Self.UpdateLine(HVDb[i]);
     Self.LV.UpdateItems(HVDb[i].index, HVDb[i].index);
    end;
  end;//for i

 Self.reload := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THVTableData.UpdateLine(HV:THV);
var line:Integer;
    data:THVData;
    stav:THVStav;
    slot:TTrkLocoInfo;
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

  case (data.typ) of
   THVType.other   : Self.LV.Items[line].SubItems[4] := 'jiný';
   THVType.steam   : Self.LV.Items[line].SubItems[4] := 'parní';
   THVType.diesel  : Self.LV.Items[line].SubItems[4] := 'diesel';
   THVType.motor   : Self.LV.Items[line].SubItems[4] := 'motor';
   THVType.electro : Self.LV.Items[line].SubItems[4] := 'elektro';
   THVType.car     : Self.LV.Items[line].SubItems[4] := 'vůz';
  end;//case

  Self.LV.Items[line].SubItems[5] := IntToStr(data.prechodnost);

  case (stav.StanovisteA) of
   lichy : Self.LV.Items[line].SubItems[6] := 'lichý';
   sudy  : Self.LV.Items[line].SubItems[6] := 'sudý';
  end;//case

  Self.LV.Items[line].SubItems[20] := Format('%5.2f',[stav.traveled_forward]);
  Self.LV.Items[line].SubItems[21] := Format('%5.2f',[stav.traveled_backward]);

  if (stav.stanice <> nil) then
    Self.LV.Items[line].SubItems[7] := stav.stanice.Name
  else
    Self.LV.Items[line].SubItems[7] := '';

  Self.LV.Items[line].SubItems[8] := IntToStr(data.maxRychlost) + ' km/h';

  if (stav.train > -1) then
    Self.LV.Items[line].SubItems[19] := Trains.GetTrainNameByIndex(stav.train)
  else
    Self.LV.Items[line].SubItems[19] := '-';

  case (stav.pom) of
   TPomStatus.progr    : Self.LV.Items[line].SubItems[18] := 'progr';
   TPomStatus.error    : Self.LV.Items[line].SubItems[18] := 'error';
   TPomStatus.pc       : Self.LV.Items[line].SubItems[18] := 'automat';
   TPomStatus.released : Self.LV.Items[line].SubItems[18] := 'ruční';
  end;//case

 if ((not stav.acquired) and (not stav.stolen)) then
  begin
   // neprevzato
   Self.LV.Items[line].SubItems[9]  := '---';
   Self.LV.Items[line].SubItems[10]  := '---';
   Self.LV.Items[line].SubItems[11]  := '?';
   Self.LV.Items[line].SubItems[12]  := '????';
   Self.LV.Items[line].SubItems[13]  := '????';
   Self.LV.Items[line].SubItems[14]  := '????';
   Self.LV.Items[line].SubItems[15]  := '???? ????';
   Self.LV.Items[line].SubItems[16]  := '???? ????';
   Self.LV.Items[line].SubItems[17] := '---';
  end else begin
   // prevzato

   Self.LV.Items[line].SubItems[9] := IntToStr(hv.realSpeed) + 'km/h / '+IntToStr(Slot.step)+' st';
   Self.LV.Items[line].SubItems[10] := IntToStr(ownConvert.BoolToInt(Slot.direction));
   Self.LV.Items[line].SubItems[11] := IntToStr(ownConvert.BoolToInt(HV.slotFunkce[0]));

   str := '';
   for i := 1 to 4 do str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunkce[i]));
   Self.LV.Items[line].SubItems[12] := str;

   str := '';
   for i := 5 to 8 do str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunkce[i]));
   Self.LV.Items[line].SubItems[13] := str;

   str := '';
   for i := 9 to 12 do str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunkce[i]));
   Self.LV.Items[line].SubItems[14] := str;

   str := '';
   for i := 13 to 20 do
    begin
     str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunkce[i]));
     if (i = 16) then str := str + ' ';
    end;
   Self.LV.Items[line].SubItems[15] := str;

   str := '';
   for i := 21 to 28 do
    begin
     str := str + IntToStr(ownConvert.BoolToInt(HV.slotFunkce[i]));
     if (i = 24) then str := str + ' ';
    end;
   Self.LV.Items[line].SubItems[16] := str;

   if (stav.trakceError) then
     Self.LV.Items[line].SubItems[17] := 'COM ERROR!'
   else if (stav.stolen) then
     Self.LV.Items[line].SubItems[17] := 'ukradeno'
   else
     Self.LV.Items[line].SubItems[17] := 'PC';
  end;//else not prevzato

  if (stav.train > -1) then
    Self.LV.Items[line].SubItems[22] := 'teď'
  else if (stav.last_used > 0) then
    Self.LV.Items[line].SubItems[22] := FormatDateTime('yyyy-mm-dd hh:nn:ss', stav.last_used)
  else
    Self.LV.Items[line].SubItems[22] := '-';
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
