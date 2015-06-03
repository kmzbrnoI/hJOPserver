unit DataJC;

// TJCTableData - trida starajici se o vyplneni tabulky jizdnich cest

interface

uses ComCtrls, SysUtils, RPConst;

type
  TJCTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateTable();

      procedure UpdateLine(line:Integer);

      procedure AddJC();
      procedure RemoveJC(index:Integer);

      constructor Create(LV:TListView);
  end;

var
  JCTableData : TJCTableData;

implementation

uses TBlokUsek, TBlokVyhybka, TJCDatabase, TechnologieJC, TBlok, TBloky, Main,
     TBlokSCom;

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

end;//procedure

procedure TJCTableData.UpdateTable();
var i:Integer;
    repaint:boolean;
begin
 repaint := false;
 for i := 0 to JCDb.Count-1 do
   if (JCDb.GetJCByIndex(i).changed) then
    begin
     repaint := true;
     try
       Self.UpdateLine(i);
     except

     end;
    end;

 if (repaint) then
  Self.LV.Repaint(); 
end;//procedure

procedure TJCTableData.UpdateLine(line:Integer);
var JCData:TJCprop;
    JC:TJC;
    j:Integer;
    str:string;
begin
 JC     := JCDb.GetJCByIndex(line);
 JCData := JC.data;
 JC.changed := false;

 Self.LV.Items.Item[line].Caption := IntToStr(line);
 Self.LV.Items.Item[line].SubItems.Strings[0] := JCData.Nazev;
 Self.LV.Items.Item[line].SubItems.Strings[4] := Blky.GetBlkName(JCData.NavestidloBlok);

 // stav:
 Self.LV.Items.Item[line].SubItems.Strings[1] := IntToStr(JC.stav.RozpadBlok);
 Self.LV.Items.Item[line].SubItems.Strings[2] := IntToStr(JC.stav.RozpadRuseniBlok);
 Self.LV.Items.Item[line].SubItems.Strings[3] := IntToStr(JC.stav.Krok);

 case (JCData.TypCesty) of
  TJCType.vlak  : Self.LV.Items.Item[line].SubItems.Strings[7] := 'VC';
  TJCType.posun : Self.LV.Items.Item[line].SubItems.Strings[7] := 'PC';
  TJCType.nouz  : Self.LV.Items.Item[line].SubItems.Strings[7] := 'NC';
 end;

 if (JCData.DalsiNNavaznostTyp = 0) then
  begin
   Self.LV.Items.Item[line].SubItems.Strings[8] := 'Zadna navaznost';
  end else begin
   if (JCData.DalsiNNavaznostTyp = 1) then
    begin
     Self.LV.Items.Item[line].SubItems.Strings[8] := 'Trat';
    end else begin
     Self.LV.Items.Item[line].SubItems.Strings[8] := Blky.GetBlkName(JCData.DalsiNNavaznost);
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
 Self.LV.Items.Item[line].SubItems.Strings[5] := str;

 // useky
 str := '';
 for j := 0 to JCData.Useky.Count-1 do
   str := str + Blky.GetBlkName(JCData.Useky[j])+'; ';
 Self.LV.Items.Item[line].SubItems.Strings[6] := str;

 Self.LV.Items.Item[line].SubItems.Strings[9]  := IntToStr(JCData.RychlostDalsiN * 10)+' km/h';
 Self.LV.Items.Item[line].SubItems.Strings[10] := IntToStr(JCData.RychlostNoDalsiN * 10)+' km/h';

 // odvraty
 str := '';
 for j := 0 to JCData.Odvraty.Count-1 do
  begin
   case (JCData.Odvraty[j].Poloha) of
     TVyhPoloha.plus  : str := str + '(' + Blky.GetBlkName(JCData.Odvraty[j].Blok)+', +, '+Blky.GetBlkName(JCData.Odvraty[j].ref_blk)+' )';
     TVyhPoloha.minus : str := str + '(' + Blky.GetBlkName(JCData.Odvraty[j].Blok)+', -, '+Blky.GetBlkName(JCData.Odvraty[j].ref_blk)+' )';
   end;
  end;//for j
 Self.LV.Items.Item[line].SubItems.Strings[11] := str;

 // prislusenstvi
 str := '';
 for j := 0 to JCData.Prisl.Count-1 do
   str := str + '(' + Blky.GetBlkName(JCData.Prisl[j].Blok)+', '+Blky.GetBlkName(JCData.Prisl[j].ref_blk)+' )';
 Self.LV.Items.Item[line].SubItems.Strings[12] := str;

 if (JCData.Trat > -1) then
  Self.LV.Items.Item[line].SubItems.Strings[13] := Blky.GetBlkName(JCData.Trat)
 else
  Self.LV.Items.Item[line].SubItems.Strings[13] := '';

 // prejezdy
 str := '';
 for j := 0 to JCData.Prejezdy.Count-1 do
   str := str + Blky.GetBlkName(JCData.Prejezdy[j].Prejezd)+'; ';
 Self.LV.Items.Item[line].SubItems.Strings[14] := str;

 // podminky vyhybky
 str := '';
 for j := 0 to JCData.podminky.vyhybky.Count-1 do
  begin
   case (JCData.podminky.vyhybky[j].Poloha) of
     TVyhPoloha.plus  : str := str + '(' + Blky.GetBlkName(JCData.podminky.vyhybky[j].Blok)+', +)';
     TVyhPoloha.minus : str := str + '(' + Blky.GetBlkName(JCData.podminky.vyhybky[j].Blok)+', -)';
   end;
  end;//for j
 Self.LV.Items.Item[line].SubItems.Strings[15] := str;

 // podminky zamky
 str := '';
 for j := 0 to JCData.podminky.zamky.Count-1 do
   str := str + '('+Blky.GetBlkName(JCData.podminky.zamky[j].Blok)+' : ' + Blky.GetBlkName(JCData.podminky.zamky[j].ref_blk) + ')';
 Self.LV.Items.Item[line].SubItems.Strings[16] := str;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.AddJC();
var LI:TListItem;
    j:Integer;
begin
 LI := Self.LV.Items.Add;
 LI.Caption := IntToStr(Self.LV.Items.Count);
 for j := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');

 Self.UpdateLine(Self.LV.Items.Count-1);
end;//procedure

procedure TJCTableData.RemoveJC(index:Integer);
begin
 Self.LV.Items.Delete(index);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 JCTableData.Free();

end.//unit
