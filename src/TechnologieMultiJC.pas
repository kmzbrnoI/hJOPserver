unit TechnologieMultiJC;

{
  Technologie slozenych jizdnich cest.
}

interface

uses
  IniFiles, TechnologieJC, Generics.Collections, TBloky, IdContext, SysUtils,
  Classes, RPConst;

type

  TMultiJCStaveni = record
   JCIndex:Integer;
   SenderOR:TObject;
   SenderPnl:TIdContext;
  end;

  TMultiJCprop = record
   Nazev:string;
   JCs:TList<Integer>;
   vb:TList<Integer>;
  end;

  TMultiJC=class

   private const
    _def_mutiJC_staveni : TMultiJCStaveni = (
     JCIndex : -1;
     SenderOR : nil;
     SenderPnl : nil;
    );

   private
     fproperties: TMultiJCprop;
     fstaveni: TMultiJCStaveni;
     staveno:TJC;     // zde je ulozena JC, ktera se aktualne stavi

     function GetStaveni():boolean;

   public

     changed:boolean;

      constructor Create(); overload;
      constructor Create(data:TMultiJCprop); overload;
      destructor Destroy(); override;

      procedure UpdateStaveni();

      procedure LoadData(ini:TMemIniFile; section:string);
      procedure SaveData(ini:TMemIniFile; section:string);

      procedure StavJC(SenderPnl:TIdContext; SenderOR:TObject);
      procedure RusStaveni();


      property data:TMultiJCprop read fproperties write fproperties;
      property stav:TMultiJCStaveni read fstaveni;
      property Nazev:string read fproperties.Nazev;
      property staveni:boolean read GetStaveni;
  end;

implementation

uses TBlok, TJCDatabase, TBlokSCom, TBlokUsek, TOblRizeni;

////////////////////////////////////////////////////////////////////////////////

constructor TMultiJC.Create();
begin
 inherited Create();

 Self.changed  := true;
 Self.fstaveni := _def_mutiJC_staveni;

 Self.fproperties.JCs := TList<Integer>.Create();
 Self.fproperties.vb  := TList<Integer>.Create();

 Self.staveno := nil;
end;//ctor

constructor TMultiJC.Create(data:TMultiJCprop);
begin
 inherited Create();

 Self.changed  := true;
 Self.fstaveni := _def_mutiJC_staveni;

 Self.fproperties := data;

 if (not Assigned(data.JCs)) then Self.fproperties.JCs := TList<Integer>.Create();
 if (not Assigned(data.vb))  then Self.fproperties.vb  := TList<Integer>.Create();
end;//ctor

destructor TMultiJC.Destroy();
begin
 if (Assigned(Self.fproperties.JCs)) then FreeAndNil(Self.fproperties.JCs);
 if (Assigned(Self.fproperties.vb)) then FreeAndNil(Self.fproperties.vb);

 inherited Destroy();
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.LoadData(ini:TMemIniFile; section:string);
var sl:TStrings;
    i:Integer;
begin
 Self.fproperties.Nazev := ini.ReadString (section, 'Nazev', section);

 Self.fproperties.JCs.Clear();
 Self.fproperties.vb.Clear();

 // nacteni jizdnich cest ve slozene jizdni ceste:
 sl  := TStringList.Create();
 ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'JCs', '')), sl);
 for i := 0 to sl.Count-1 do
   Self.fproperties.JCs.Add(StrToInt(sl[i]));

 // nacteni variantnich bodu
 sl.Clear();
 ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
 for i := 0 to sl.Count-1 do
   Self.fproperties.vb.Add(StrToInt(sl[i]));
end;//procedure

procedure TMultiJC.SaveData(ini:TMemIniFile; section:string);
var i:Integer;
    str:string;
begin
 ini.WriteString(section, 'Nazev', Self.fproperties.Nazev);

 str := '';
 for i := 0 to Self.data.JCs.Count-1 do
   str := str + IntToStr(Self.data.JCs[i]) + ';';
 ini.WriteString(section, 'JCs', str);

 str := '';
 for i := 0 to Self.data.vb.Count-1 do
   str := str + IntToStr(Self.data.vb[i]) + ';';
 ini.WriteString(section, 'vb', str);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.UpdateStaveni();
var JC:TJC;
begin
 if (Self.staveno.postaveno) then
  begin
   // aktualni cesta postavena
   Inc(Self.fstaveni.JCIndex);

   if (Self.fstaveni.JCIndex >= Self.fproperties.JCs.Count) then
    begin
     // vsechny cesty postaveny
     Self.RusStaveni();
    end else begin
     // vsechny cesty nepostaveny -> stavime dalsi cestu
     JC := JCDb.GetJCByID(Self.fproperties.JCs[Self.fstaveni.JCIndex]);
     if (JC = nil) then
       Self.RusStaveni()
      else begin
       Self.staveno := JC;
       Self.staveno.StavJC(Self.fstaveni.SenderPnl, Self.fstaveni.SenderOR);
      end;
     Self.changed := true;
    end;
  end else begin
   if (not Self.staveno.staveni) then
    begin
     // cesta byla stavena, ale uz se nestavi -> evidentne nastala chyba -> ukoncime staveni slozene jizdni cesty
     Self.RusStaveni();
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.StavJC(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 for i := 0 to Self.fproperties.JCs.Count-1 do
   if (JCDb.GetJCByID(Self.fproperties.JCs[i]) = nil) then
     raise Exception.Create('JC ve slozene jizdni ceste neexistuje');

 Self.fstaveni.SenderOR  := SenderOR;
 Self.fstaveni.SenderPnl := SenderPnl;

 Self.fstaveni.JCIndex   := 0;
 Self.staveno := JCDb.GetJCByID(Self.fproperties.JCs[Self.fstaveni.JCIndex]);
 Self.staveno.StavJC(SenderPnl, SenderOR);

 (SenderOR as TOR).vb.Clear();

 Self.changed := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.RusStaveni();
var JC:TJC;
    Blk:TBlk;
    i:Integer;
begin
 Self.fstaveni.JCIndex := -1;
 Self.staveno          := nil;

 // zrusime zacatek staveni na navestidle
 JC := JCDb.GetJCByID(Self.fproperties.JCs[0]);
 Blky.GetBlkByID(JC.data.NavestidloBlok, Blk);
 (Blk as TBLkSCom).ZacatekVolba := TBlkSCOmVolba.none;

 // zrusime konec staveni na poslednim useku posledni JC
 JC := JCDb.GetJCByID(Self.fproperties.JCs[Self.fproperties.JCs.Count-1]);
 Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Blk);
 (Blk as TBLkUsek).KonecJC := TJCType.no;

 // zrusime konec staveni na vsech variantnich bodech
 for i := 0 to Self.fproperties.vb.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.vb[i], Blk);
   (Blk as TBlkUsek).KonecJC := TJCType.no;
  end;

 Self.changed := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TMultiJC.GetStaveni():boolean;
begin
 Result := (Self.fstaveni.JCIndex > -1);
end;
////////////////////////////////////////////////////////////////////////////////

end.//unit
