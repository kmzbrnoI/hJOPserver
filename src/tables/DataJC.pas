unit DataJC;

// TJCTableData - trida starajici se o vyplneni tabulky jizdnich cest

interface

uses ComCtrls, SysUtils, StrUtils;

type
  TJCTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable();

      procedure UpdateLine(line:Integer);

      procedure AddJC(index:Integer);
      procedure RemoveJC(index:Integer);
      procedure MoveJC(source, target:Integer);

      constructor Create(LV:TListView);
  end;

var
  JCTableData : TJCTableData;

implementation

uses TBlokUsek, TBlokVyhybka, TJCDatabase, TechnologieJC, TBlok, TBloky, fMain,
     TBlokNav;

////////////////////////////////////////////////////////////////////////////////

constructor TJCTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.LoadToTable();
var i, j:Integer;
    LI:TListItem;
begin
 F_Main.E_dataload_JC.text := JCDb.filename;
 Self.LV.Clear();

 for i := 0 to JCDb.Count-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := IntToStr(i);
   for j := 0 to Self.LV.Columns.Count-2 do
    LI.SubItems.Add('');

   try
     Self.UpdateLine(i);
   except

   end;
 end;//for i

end;

procedure TJCTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to JCDb.Count-1 do
   if (JCDb.GetJCByIndex(i).changed) then
    begin
     try
       Self.UpdateLine(i);
       Self.LV.UpdateItems(i, i);
     except

     end;
    end;
end;

procedure TJCTableData.UpdateLine(line:Integer);
var JCData:TJCprop;
    JC:TJC;
    j:Integer;
    str:string;
    Blk:TBlk;
begin
 JC     := JCDb.GetJCByIndex(line);
 JCData := JC.data;
 JC.changed := false;

 Self.LV.Items[line].Caption := IntToStr(JCData.id);
 Self.LV.Items[line].SubItems[0] := JCData.Nazev;
 Self.LV.Items[line].SubItems[4] := Blky.GetBlkName(JCData.NavestidloBlok);

 // stav:
 Self.LV.Items[line].SubItems[1] := IntToStr(JC.stav.RozpadBlok);
 Self.LV.Items[line].SubItems[2] := IntToStr(JC.stav.RozpadRuseniBlok);
 Self.LV.Items[line].SubItems[3] := IntToStr(JC.stav.Krok);

 case (JCData.TypCesty) of
  TJCType.vlak  : Self.LV.Items[line].SubItems[7] := 'VC';
  TJCType.posun : Self.LV.Items[line].SubItems[7] := 'PC';
  TJCType.nouz  : Self.LV.Items[line].SubItems[7] := 'NC';
 end;

 if (JCData.DalsiNavaznost = TJCNextNavType.zadna) then
  begin
   Self.LV.Items[line].SubItems[8] := 'žádné';
  end else begin
   if (JCData.DalsiNavaznost = TJCNextNavType.trat) then
    begin
     Self.LV.Items[line].SubItems[8] := 'tra';
    end else begin
     Self.LV.Items[line].SubItems[8] := Blky.GetBlkName(JCData.DalsiNavestidlo);
    end;//else DalsiNNavaznostTyp = 1
  end;//else DalsiNNavaznostTyp = 0

 // vyhybky
 str := '';
 for j := 0 to JCData.Vyhybky.Count-1 do
  begin
   case (JCData.Vyhybky[j].Poloha) of
     TVyhPoloha.plus  : str := str + '(' + Blky.GetBlkName(JCData.Vyhybky[j].Blok)+', +)';
     TVyhPoloha.minus : str := str + '(' + Blky.GetBlkName(JCData.Vyhybky[j].Blok)+', -)';
   end;
  end;//for j
 Self.LV.Items[line].SubItems[5] := str;

 // useky
 str := '';
 for j := 0 to JCData.Useky.Count-1 do
   str := str + Blky.GetBlkName(JCData.Useky[j])+'; ';
 Self.LV.Items[line].SubItems[6] := LeftStr(str, Length(str)-2);

 Self.LV.Items[line].SubItems[9]  := IntToStr(JCData.RychlostDalsiN * 10)+' km/h';
 Self.LV.Items[line].SubItems[10] := IntToStr(JCData.RychlostNoDalsiN * 10)+' km/h';

 // odvraty
 str := '';
 for j := 0 to JCData.Odvraty.Count-1 do
  begin
   case (JCData.Odvraty[j].Poloha) of
     TVyhPoloha.plus  : str := str + '(' + Blky.GetBlkName(JCData.Odvraty[j].Blok)+', +, '+Blky.GetBlkName(JCData.Odvraty[j].ref_blk)+')';
     TVyhPoloha.minus : str := str + '(' + Blky.GetBlkName(JCData.Odvraty[j].Blok)+', -, '+Blky.GetBlkName(JCData.Odvraty[j].ref_blk)+')';
   end;
  end;//for j
 Self.LV.Items[line].SubItems[11] := str;

 // prislusenstvi
 str := '';
 for j := 0 to JCData.Prisl.Count-1 do
   str := str + '(' + Blky.GetBlkName(JCData.Prisl[j].Blok)+', '+Blky.GetBlkName(JCData.Prisl[j].ref_blk)+' )';
 Self.LV.Items[line].SubItems[12] := str;

 if (JCData.Trat > -1) then
  Self.LV.Items[line].SubItems[13] := Blky.GetBlkName(JCData.Trat)
 else
  Self.LV.Items[line].SubItems[13] := '';

 // prejezdy
 str := '';
 for j := 0 to JCData.Prejezdy.Count-1 do
   str := str + Blky.GetBlkName(JCData.Prejezdy[j].Prejezd)+'; ';
 Self.LV.Items[line].SubItems[14] := LeftStr(str, Length(str)-2);

 // podminky zamky
 str := '';
 for j := 0 to JCData.zamky.Count-1 do
   str := str + '('+Blky.GetBlkName(JCData.zamky[j].Blok)+' : ' + Blky.GetBlkName(JCData.zamky[j].ref_blk) + ')';
 Self.LV.Items[line].SubItems[15] := str;

 // neprofilove useky
 str := '';
 for j := 0 to JCData.Vyhybky.Count-1 do
  begin
   Blky.GetBlkByID(JCData.Vyhybky[j].Blok, Blk);
   if (Blk <> nil) and (Blk.typ = _BLK_VYH) then
    begin
     if ((JCData.Vyhybky[j].Poloha = TVyhPoloha.plus) and (TBlkVyhybka(Blk).npBlokPlus <> nil)) then
       str := str + TBlkVyhybka(Blk).npBlokPlus.name + ', '
     else if ((JCData.Vyhybky[j].Poloha = TVyhPoloha.minus) and (TBlkVyhybka(Blk).npBlokMinus <> nil)) then
       str := str + TBlkVyhybka(Blk).npBlokMinus.name + ', ';
    end else
     str := str + '?, ';
  end;
 Self.LV.Items[line].SubItems[16] := LeftStr(str, Length(str)-2);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.AddJC(index:Integer);
var LI:TListItem;
    j:Integer;
begin
 LI := Self.LV.Items.Insert(index);
 LI.Caption := IntToStr(Self.LV.Items.Count);
 for j := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');

 Self.UpdateLine(index);
end;

procedure TJCTableData.RemoveJC(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.MoveJC(source, target:Integer);
var LI:TListItem;
    i:Integer;
begin
 Self.LV.Items.Delete(source);
 LI := Self.LV.Items.Insert(target);
 for i := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');
 Self.UpdateLine(target);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 JCTableData.Free();

end.//unit
