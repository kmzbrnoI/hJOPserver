unit DataBloky;

// TBlkTableData - trida resici zobrazovani tavulky bloku

interface

uses ComCtrls, SysUtils, RPConst;

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

      constructor Create(LV:TListView);
  end;

var
  BlokyTableData : TBlokyTableData;

implementation

uses TBloky, TBlok, TBlokVyhybka, TBlokUsek, TBlokSCom, TBlokIR, TBlokPrejezd,
      fMain, TBlokTrat, TBlokUvazka, SprDb, TBlokZamek, TBlokRozp;

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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.UpdateTable();
var i:Integer;
    achanged:boolean;
begin
  F_Main.L_BlkPocet.Caption := 'Pocet bloku : '+IntToStr(Blky.Cnt);

  achanged := false;
  for i := 0 to Blky.Cnt-1 do
   if ((Self.changed[i]) or (Self.reload)) then
    begin
     Self.UpdateLine(i);
     Self.changed[i] := false;
     achanged := true;
    end;

  Self.reload := false;
  if (achanged) then
    Self.LV.Repaint();
end;//procedyre

procedure TBlokyTableData.UpdateLine(line:Integer);
var j:integer;
    Blk:TBlk;
    glob:TBlkSettings;
    s_vyh:TBlkVyhSettings;
    s_scom:TBlkSComSettings;
    str:string;
 begin
  Blky.GetBlkByIndex(line, Blk);
  if (Blk = nil) then Exit();

  glob := Blk.GetGlobalSettings();

  Self.LV.Items.Item[line].Caption             := glob.name;
  Self.LV.Items.Item[line].SubItems.Strings[1] := IntToStr(glob.id);

  str := '';
  if (Blk.OblsRizeni.Cnt > 1) then
    for j := 0 to Blk.OblsRizeni.Cnt-2 do str := str + Blk.OblsRizeni.ORs[j].Name+', ';
  if (Blk.OblsRizeni.Cnt > 0) then
    str := str + Blk.OblsRizeni.ORs[Blk.OblsRizeni.Cnt-1].Name;
  Self.LV.Items.Item[line].SubItems.Strings[4] := str;

  case (glob.typ) of
   _BLK_VYH:begin
      Self.LV.Items.Item[line].ImageIndex := 0;
      s_vyh := (Blk as TBlkVyhybka).GetSettings();
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Vyhıbka';

      case ((Blk as TBlkVyhybka).Poloha) of
       TVyhPoloha.disabled:Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
       TVyhPoloha.none    :Self.LV.Items.Item[line].SubItems.Strings[3] := 'none';
       TVyhPoloha.plus    :Self.LV.Items.Item[line].SubItems.Strings[3] := '+';
       TVyhPoloha.minus   :Self.LV.Items.Item[line].SubItems.Strings[3] := '-';
       TVyhPoloha.both    :Self.LV.Items.Item[line].SubItems.Strings[3] := '+-';
      end;//case poloha

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkVyhybka).Stitek;
      Self.LV.Items.Item[line].SubItems.Strings[6] := (Blk as TBlkVyhybka).Vyluka;
      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_USEK:begin
      Self.LV.Items.Item[line].ImageIndex := 1;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Úsek';

      if ((Blk as TBlkUsek).Souprava > -1) then Self.LV.Items.Item[line].SubItems.Strings[2] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).Souprava) else
            Self.LV.Items.Item[line].SubItems.Strings[2] := '--#--';

      case ((Blk as TBlkUsek).Obsazeno) of
        TUsekStav.disabled : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
        TUsekStav.none     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'none';
        TUsekStav.uvolneno : Self.LV.Items.Item[line].SubItems.Strings[3] := '---';
        TUsekStav.obsazeno : Self.LV.Items.Item[line].SubItems.Strings[3] := '+++';
      end;//case obsazeno

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkUsek).Stitek;
      Self.LV.Items.Item[line].SubItems.Strings[6] := (Blk as TBlkUsek).Vyluka;

      if ((Blk as TBlkUsek).SprPredict > -1) then Self.LV.Items.Item[line].SubItems.Strings[7] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).SprPredict) else
        Self.LV.Items.Item[line].SubItems.Strings[7] := '--#--';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_IR:begin
      Self.LV.Items.Item[line].ImageIndex := 4;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'IR';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      case ((Blk as TBlkIR).Stav) of
        TIRStav.disabled : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
        TIRStav.none     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'none';
        TIRStav.uvolneno : Self.LV.Items.Item[line].SubItems.Strings[3] := '---';
        TIRStav.obsazeno : Self.LV.Items.Item[line].SubItems.Strings[3] := '+++';
      end;//case

      Self.LV.Items.Item[line].SubItems.Strings[4] := '---';
      Self.LV.Items.Item[line].SubItems.Strings[5] := '---';
      Self.LV.Items.Item[line].SubItems.Strings[6] := '---';
      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_SCOM:begin
      Self.LV.Items.Item[line].ImageIndex := 5;
      s_scom := (Blk as TBlkSCom).GetSettings();
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'S-com';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      Self.LV.Items.Item[line].SubItems.Strings[3] := TBlkScom.NavestToString((Blk as TBlkSCom).Navest);

      Self.LV.Items.Item[line].SubItems.Strings[5] := '---';
      Self.LV.Items.Item[line].SubItems.Strings[6] := '---';
      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_PREJEZD:begin
      Self.LV.Items.Item[line].ImageIndex := 7;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Pøejezd';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      case ((Blk as TBlkPrejezd).Stav.basicStav) of
        TBlkPrjBasicStav.disabled : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
        TBlkPrjBasicStav.none     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'none';
        TBlkPrjBasicStav.otevreno : Self.LV.Items.Item[line].SubItems.Strings[3] := 'otevøeno';
        TBlkPrjBasicStav.vystraha : Self.LV.Items.Item[line].SubItems.Strings[3] := 'vıstraha';
        TBlkPrjBasicStav.uzavreno : Self.LV.Items.Item[line].SubItems.Strings[3] := 'uzavøeno';
        TBlkPrjBasicStav.anulace  : Self.LV.Items.Item[line].SubItems.Strings[3] := 'anulace';
      end;//case obsazeno

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkPrejezd).Stitek;
      Self.LV.Items.Item[line].SubItems.Strings[6] := (Blk as TBlkPrejezd).Vyluka;

      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_VYH

 /////////////////////////////////////////////////////
   _BLK_TRAT:begin
      Self.LV.Items.Item[line].ImageIndex := 8;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Tra';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      if ((Blk as TBlkTrat).Obsazeno) then begin
        Self.LV.Items.Item[line].SubItems.Strings[3] := 'obsazeno';
      end else begin
        if ((Blk as TBlkTrat).Zaver) then begin
          Self.LV.Items.Item[line].SubItems.Strings[3] := 'závìr'
        end else begin
          if ((Blk as TBlkTrat).ZAK) then begin
            Self.LV.Items.Item[line].SubItems.Strings[3] := 'ZAK'
          end else begin

           if ((Blk as TBlkTrat).Zadost) then
            Self.LV.Items.Item[line].SubItems.Strings[3] := 'ádost'
           else
            case ((Blk as TBlkTrat).Smer) of
             TTratSmer.disabled : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
             TTratSmer.AtoB     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'smìr A->B';
             TTratSmer.BtoA     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'smìr B->A';
             TTratSmer.zadny    : Self.LV.Items.Item[line].SubItems.Strings[3] := 'smìr ádnı'
            end;//case
          end;
        end;
      end;

    Self.LV.Items.Item[line].SubItems.Strings[5] := '';
    Self.LV.Items.Item[line].SubItems.Strings[6] := '';

    if ((Blk as TBlkTrat).SprPredict > -1) then Self.LV.Items.Item[line].SubItems.Strings[7] := Soupravy.GetSprNameByIndex((Blk as TBlkTrat).SprPredict) else
      Self.LV.Items.Item[line].SubItems.Strings[7] := '--#--';
   end;//_BLK_TRAT

 /////////////////////////////////////////////////////
   _BLK_UVAZKA:begin
      Self.LV.Items.Item[line].ImageIndex := 9;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Úvazka';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      if ((Blk as TBlkUvazka).enabled) then
        Self.LV.Items.Item[line].SubItems.Strings[3] := 'enabled'
      else
        Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkUvazka).Stitek;

      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_UVAZKA

 /////////////////////////////////////////////////////
   _BLK_ZAMEK:begin
      Self.LV.Items.Item[line].ImageIndex := 10;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Zámek';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      if ((Blk as TBlkZamek).Stav.enabled) then
       begin
        if ((Blk as TBlkZamek).klicUvolnen) then
          Self.LV.Items.Item[line].SubItems.Strings[3] := 'klíè uvolnìn'
        else
          Self.LV.Items.Item[line].SubItems.Strings[3] := 'klíè zamknut';
       end else
        Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkZamek).Stitek;

      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_ZAMEK

 /////////////////////////////////////////////////////
   _BLK_ROZP:begin
      Self.LV.Items.Item[line].ImageIndex := 11;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Rozpojovaè';

      Self.LV.Items.Item[line].SubItems.Strings[2] := '---';

      case ((Blk as TBlkRozp).status) of
        TRozpStatus.disabled     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
        TRozpStatus.not_selected : Self.LV.Items.Item[line].SubItems.Strings[3] := 'ok';
        TRozpStatus.mounting     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'mounting';
        TRozpStatus.active       : Self.LV.Items.Item[line].SubItems.Strings[3] := 'active';
      end;//case

      Self.LV.Items.Item[line].SubItems.Strings[5] := '';
      Self.LV.Items.Item[line].SubItems.Strings[7] := '---';
   end;//_BLK_ROZP

 /////////////////////////////////////////////////////
   _BLK_TU:begin
      Self.LV.Items.Item[line].ImageIndex := 2;
      Self.LV.Items.Item[line].SubItems.Strings[0] := 'Traovı úsek';

      if ((Blk as TBlkUsek).Souprava > -1) then Self.LV.Items.Item[line].SubItems.Strings[2] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).Souprava) else
            Self.LV.Items.Item[line].SubItems.Strings[2] := '--#--';

      case ((Blk as TBlkUsek).Obsazeno) of
        TUsekStav.disabled : Self.LV.Items.Item[line].SubItems.Strings[3] := 'disabled';
        TUsekStav.none     : Self.LV.Items.Item[line].SubItems.Strings[3] := 'none';
        TUsekStav.uvolneno : Self.LV.Items.Item[line].SubItems.Strings[3] := '---';
        TUsekStav.obsazeno : Self.LV.Items.Item[line].SubItems.Strings[3] := '+++';
      end;//case obsazeno

      Self.LV.Items.Item[line].SubItems.Strings[5] := (Blk as TBlkUsek).Stitek;
      Self.LV.Items.Item[line].SubItems.Strings[6] := (Blk as TBlkUsek).Vyluka;

      if ((Blk as TBlkUsek).SprPredict > -1) then Self.LV.Items.Item[line].SubItems.Strings[7] := Soupravy.GetSprNameByIndex((Blk as TBlkUsek).SprPredict) else
        Self.LV.Items.Item[line].SubItems.Strings[7] := '--#--';
   end;//_BLK_ROZP

  end;//case BLOK_TYPE


 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkChange(line:Integer);
begin
 if ((line > -1) and (line < Blky.Cnt)) then
  Self.changed[line] := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkRemove(line:Integer);
begin
 Self.LV.Items.Delete(line);
 Self.reload := true;
 Self.UpdateTable();
end;//procedure

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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 BlokyTableData.Free();

end.//unit
