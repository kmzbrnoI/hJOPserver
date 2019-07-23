unit DataBloky;

// TBlkTableData - trida resici zobrazovani tavulky bloku

interface

uses ComCtrls, SysUtils, StrUtils;

type
  TBlokyTableData=class
    private
      LV:TListView;

      changed:array of boolean;

    public

     reload:boolean;

      procedure LoadTable();
      procedure UpdateTable();
      procedure UpdateLine(line:Integer);
      procedure BlkChange(line:Integer);

      procedure BlkRemove(line:Integer);
      procedure BlkAdd(index:Integer);

      procedure BlkMove(source, target:Integer);

      constructor Create(LV:TListView);
  end;

var
  BlokyTableData : TBlokyTableData;

implementation

uses TBloky, TBlok, TBlokVyhybka, TBlokUsek, TBlokNav, TBlokIR, TBlokPrejezd,
      fMain, TBlokTrat, TBlokUvazka, SprDb, TBlokZamek, TBlokRozp, TBlokVystup,
      TBlokSouctovaHlaska;

////////////////////////////////////////////////////////////////////////////////

constructor TBlokyTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
 SetLength(Self.changed, Blky.Cnt);
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.LoadTable();
var i,j :Integer;
    LI:TListItem;
begin
 Self.LV.Clear();

 for i := 0 to Blky.Cnt-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := '---';
   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('---');
  end;//for i

 Self.reload := true;
 Self.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.UpdateTable();
var i:Integer;
begin
  F_Main.L_BlkPocet.Caption := 'Celkem '+IntToStr(Blky.Cnt)+' blok�';

  for i := 0 to Blky.Cnt-1 do
   if ((Self.changed[i]) or (Self.reload)) then
    begin
     Self.UpdateLine(i);
     Self.changed[i] := false;
     Self.LV.UpdateItems(i, i);
    end;

  Self.reload := false;
end;//procedyre

procedure TBlokyTableData.UpdateLine(line:Integer);
var j, spr:integer;
    Blk:TBlk;
    glob:TBlkSettings;
    s_vyh:TBlkVyhSettings;
    s_nav:TBlkNavSettings;
    str:string;
 begin
  Blky.GetBlkByIndex(line, Blk);
  if (Blk = nil) then Exit();

  glob := Blk.GetGlobalSettings();

  Self.LV.Items[line].Caption             := glob.name;
  Self.LV.Items[line].SubItems.Strings[1] := IntToStr(glob.id);

  str := '';
  if (Blk.OblsRizeni.Count > 1) then
    for j := 0 to Blk.OblsRizeni.Count-2 do str := str + Blk.OblsRizeni[j].Name+', ';
  if (Blk.OblsRizeni.Count > 0) then
    str := str + Blk.OblsRizeni[Blk.OblsRizeni.Count-1].Name;
  Self.LV.Items[line].SubItems.Strings[4] := str;

  case (glob.typ) of
   _BLK_VYH:begin
      Self.LV.Items[line].ImageIndex := 0;
      s_vyh := (Blk as TBlkVyhybka).GetSettings();
      Self.LV.Items[line].SubItems.Strings[0] := 'V�hybka';

      case ((Blk as TBlkVyhybka).Poloha) of
       TVyhPoloha.disabled:Self.LV.Items[line].SubItems[3] := 'disabled';
       TVyhPoloha.none    :Self.LV.Items[line].SubItems[3] := 'none';
       TVyhPoloha.plus    :Self.LV.Items[line].SubItems[3] := '+';
       TVyhPoloha.minus   :Self.LV.Items[line].SubItems[3] := '-';
       TVyhPoloha.both    :Self.LV.Items[line].SubItems[3] := '+-';
      end;//case poloha

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkVyhybka).Stitek;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkVyhybka).Vyluka;
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_USEK:begin
      if ((Blk as TBlkUsek).Stav.cislo_koleje <> '') then
        Self.LV.Items[line].ImageIndex := 1
      else
        Self.LV.Items[line].ImageIndex := 3;

      Self.LV.Items[line].SubItems[0] := '�sek';

      str := '';
      for spr in (Blk as TBlkUsek).Soupravs do
        str := str + Soupravy.GetSprNameByIndex(spr) + ', ';
      Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2);

      case ((Blk as TBlkUsek).Obsazeno) of
        TUsekStav.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TUsekStav.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TUsekStav.uvolneno : Self.LV.Items[line].SubItems[3] := '---';
        TUsekStav.obsazeno : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkUsek).Stitek;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkUsek).Vyluka;

      if ((Blk as TBlkUsek).SprPredict > -1) then Self.LV.Items[line].SubItems[7] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).SprPredict) else
        Self.LV.Items[line].SubItems[7] := '--#--';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_IR:begin
      Self.LV.Items[line].ImageIndex := 4;
      Self.LV.Items[line].SubItems[0] := 'IR';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkIR).Stav) of
        TIRStav.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TIRStav.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TIRStav.uvolneno : Self.LV.Items[line].SubItems[3] := '---';
        TIRStav.obsazeno : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case

      Self.LV.Items[line].SubItems[4] := '---';
      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_NAV:begin
      Self.LV.Items[line].ImageIndex := 5;
      s_nav := (Blk as TBlkNav).GetSettings();
      Self.LV.Items[line].SubItems[0] := 'N�v�stidlo';

      Self.LV.Items[line].SubItems[2] := '---';

      Self.LV.Items[line].SubItems[3] := TBlkNav.NavestToString((Blk as TBlkNav).Navest);

      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_PREJEZD:begin
      Self.LV.Items[line].ImageIndex := 7;
      Self.LV.Items[line].SubItems[0] := 'P�ejezd';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkPrejezd).Stav.basicStav) of
        TBlkPrjBasicStav.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TBlkPrjBasicStav.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TBlkPrjBasicStav.otevreno : Self.LV.Items[line].SubItems[3] := 'otev�eno';
        TBlkPrjBasicStav.vystraha : Self.LV.Items[line].SubItems[3] := 'v�straha';
        TBlkPrjBasicStav.uzavreno : Self.LV.Items[line].SubItems[3] := 'uzav�eno';
        TBlkPrjBasicStav.anulace  : Self.LV.Items[line].SubItems[3] := 'anulace';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkPrejezd).Stitek;
      Self.LV.Items[line].SubItems[6] := '';

      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_TRAT:begin
      Self.LV.Items[line].ImageIndex := 8;
      Self.LV.Items[line].SubItems[0] := 'Tra�';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkTrat).Obsazeno) then begin
        Self.LV.Items[line].SubItems[3] := 'obsazeno';
      end else begin
        if ((Blk as TBlkTrat).Zaver) then begin
          Self.LV.Items[line].SubItems[3] := 'z�v�r'
        end else begin
          if ((Blk as TBlkTrat).ZAK) then begin
            Self.LV.Items[line].SubItems[3] := 'ZAK'
          end else begin

           if ((Blk as TBlkTrat).Zadost) then
            Self.LV.Items[line].SubItems[3] := '��dost'
           else
            case ((Blk as TBlkTrat).Smer) of
             TTratSmer.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
             TTratSmer.AtoB     : Self.LV.Items[line].SubItems[3] := 'sm�r A->B';
             TTratSmer.BtoA     : Self.LV.Items[line].SubItems[3] := 'sm�r B->A';
             TTratSmer.zadny    : Self.LV.Items[line].SubItems[3] := 'sm�r ��dn�'
            end;//case
          end;
        end;
      end;

    Self.LV.Items[line].SubItems[5] := '';
    Self.LV.Items[line].SubItems[6] := '';

    if (Assigned((Blk as TBlkTrat).SprPredict)) then
      Self.LV.Items[line].SubItems[7] := Soupravy.GetSprNameByIndex((Blk as TBlkTrat).SprPredict.souprava)
    else
      Self.LV.Items[line].SubItems[7] := '--#--';
   end;//_BLK_TRAT

 /////////////////////////////////////////////////////
   _BLK_UVAZKA:begin
      Self.LV.Items[line].ImageIndex := 9;
      Self.LV.Items[line].SubItems[0] := '�vazka';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkUvazka).enabled) then
        Self.LV.Items[line].SubItems[3] := 'enabled'
      else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkUvazka).Stitek;

      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_UVAZKA

 /////////////////////////////////////////////////////
   _BLK_ZAMEK:begin
      Self.LV.Items[line].ImageIndex := 10;
      Self.LV.Items[line].SubItems[0] := 'Z�mek';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkZamek).Stav.enabled) then
       begin
        if ((Blk as TBlkZamek).klicUvolnen) then
          Self.LV.Items[line].SubItems[3] := 'kl�� uvoln�n'
        else
          Self.LV.Items[line].SubItems[3] := 'kl�� zamknut';
       end else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkZamek).Stitek;

      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_ZAMEK

 /////////////////////////////////////////////////////
   _BLK_ROZP:begin
      Self.LV.Items[line].ImageIndex := 11;
      Self.LV.Items[line].SubItems[0] := 'Rozpojova�';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkRozp).status) of
        TRozpStatus.disabled     : Self.LV.Items[line].SubItems[3] := 'disabled';
        TRozpStatus.not_selected : Self.LV.Items[line].SubItems[3] := 'ok';
        TRozpStatus.mounting     : Self.LV.Items[line].SubItems[3] := 'mounting';
        TRozpStatus.active       : Self.LV.Items[line].SubItems[3] := 'active';
      end;//case

      Self.LV.Items[line].SubItems[5] := '';
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_ROZP

 /////////////////////////////////////////////////////
   _BLK_TU:begin
      Self.LV.Items[line].ImageIndex := 2;
      Self.LV.Items[line].SubItems[0] := 'Tra�ov� �sek';

      str := '';
      for spr in (Blk as TBlkUsek).Soupravs do
        str := str + Soupravy.GetSprNameByIndex(spr) + ', ';
      Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2);

      case ((Blk as TBlkUsek).Obsazeno) of
        TUsekStav.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TUsekStav.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TUsekStav.uvolneno : Self.LV.Items[line].SubItems[3] := '---';
        TUsekStav.obsazeno : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkUsek).Stitek;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkUsek).Vyluka;

      if ((Blk as TBlkUsek).SprPredict > -1) then Self.LV.Items[line].SubItems[7] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).SprPredict) else
        Self.LV.Items[line].SubItems[7] := '--#--';
   end;//_BLK_TU

 /////////////////////////////////////////////////////
   _BLK_VYSTUP:begin
      Self.LV.Items[line].ImageIndex := 12;
      Self.LV.Items[line].SubItems[0] := 'Logick� v�stup';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkVystup).enabled) of
        false : Self.LV.Items[line].SubItems[3] := 'disabled';
        true  : Self.LV.Items[line].SubItems[3] := 'enabled';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[4] := '---';
      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_TU

 /////////////////////////////////////////////////////
   _BLK_SH:begin
      Self.LV.Items[line].ImageIndex := -1;
      Self.LV.Items[line].SubItems[0] := 'Sou�tov� hl�ska';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkSH).enabled) then
        Self.LV.Items[line].SubItems[3] := 'ok'
      else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := '';
      Self.LV.Items[line].SubItems[7] := '---';
   end;//_BLK_SH

  end;//case BLOK_VYSTUP

 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkChange(line:Integer);
begin
 if ((line > -1) and (line < Blky.Cnt)) then
  Self.changed[line] := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkRemove(line:Integer);
begin
 Self.LV.Items.Delete(line);
 Self.reload := true;
 Self.UpdateTable();
end;

procedure TBlokyTableData.BlkAdd(index:Integer);
var LI:TListItem;
    j:Integer;
begin
 SetLength(changed, Length(changed)+1);

 LI := Self.LV.Items.Insert(index);
 LI.Caption := '---';
 for j := 0 to Self.LV.Columns.Count-2 do
   LI.SubItems.Add('---');
 Self.UpdateLine(index);

 F_Main.L_BlkPocet.Caption := 'Pocet bloku : '+IntToStr(Blky.Cnt);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkMove(source, target:Integer);
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
 BlokyTableData.Free();

end.//unit
