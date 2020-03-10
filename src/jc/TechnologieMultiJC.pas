unit TechnologieMultiJC;

{
  Technologie slozenych jizdnich cest.
}

interface

uses
  IniFiles, TechnologieJC, Generics.Collections, TBloky, IdContext, SysUtils,
  Classes, Generics.Defaults, Math, TBlok, TBlokNav;

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
   id:Integer;
  end;

  EInvalidID = class(Exception);

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
      procedure SaveData(ini:TMemIniFile);

      procedure StavJC(SenderPnl:TIdContext; SenderOR:TObject);
      procedure RusStaveni();

      function Match(startNav:TBlkNav; vb: TList<TObject>; endBlk:TBlk):Boolean;

      property data:TMultiJCprop read fproperties write fproperties;
      property stav:TMultiJCStaveni read fstaveni;
      property Nazev:string read fproperties.Nazev;
      property staveni:boolean read GetStaveni;
      property id:Integer read fproperties.id;

      class function IdComparer():IComparer<TMultiJC>;
  end;

implementation

uses TJCDatabase, TBlokUsek, TOblRizeni;

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
 try
   Self.fproperties.id    := StrToInt(section);
 except
   on E:EConvertError do
     raise EInvalidID.Create('Neplatné id mJC : '+section);
 end;

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
end;

procedure TMultiJC.SaveData(ini:TMemIniFile);
var i:Integer;
    str:string;
    section:string;
begin
 section := IntToStr(Self.id);

 ini.WriteString(section, 'Nazev', Self.fproperties.Nazev);

 str := '';
 for i := 0 to Self.data.JCs.Count-1 do
   str := str + IntToStr(Self.data.JCs[i]) + ';';
 if (str <> '') then
   ini.WriteString(section, 'JCs', str);

 str := '';
 for i := 0 to Self.data.vb.Count-1 do
   str := str + IntToStr(Self.data.vb[i]) + ';';
 if (str <> '') then
   ini.WriteString(section, 'vb', str);
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.StavJC(SenderPnl:TIdContext; SenderOR:TObject);
var i:Integer;
begin
 for i := 0 to Self.fproperties.JCs.Count-1 do
   if (JCDb.GetJCByID(Self.fproperties.JCs[i]) = nil) then
     raise Exception.Create('JC ve slozene jizdni ceste neexistuje');

 Self.fstaveni.SenderOR  := SenderOR;
 Self.fstaveni.SenderPnl := SenderPnl;

 Self.staveno := JCDb.GetJCByID(Self.fproperties.JCs[0]);
 Self.staveno.StavJC(SenderPnl, SenderOR);
 Self.fstaveni.JCIndex := 0;

 (SenderOR as TOR).vb.Clear();

 Self.changed := true;
end;

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
 (Blk as TBlkNav).ZacatekVolba := TBlkNavVolba.none;

 // zrusime konec staveni na poslednim useku posledni JC
 JC := JCDb.GetJCByID(Self.fproperties.JCs[Self.fproperties.JCs.Count-1]);
 Blky.GetBlkByID(JC.data.Useky[JC.data.Useky.Count-1], Blk);
 (Blk as TBLkUsek).KonecJC := TZaver.no;

 // zrusime konec staveni na vsech variantnich bodech
 for i := 0 to Self.fproperties.vb.Count-1 do
  begin
   Blky.GetBlkByID(Self.fproperties.vb[i], Blk);
   (Blk as TBlkUsek).KonecJC := TZaver.no;
  end;

 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJC.GetStaveni():boolean;
begin
 Result := (Self.fstaveni.JCIndex > -1);
end;

////////////////////////////////////////////////////////////////////////////////

class function TMultiJC.IdComparer():IComparer<TMultiJC>;
begin
 Result := TComparer<TMultiJC>.Construct(
   function(const mJC1, mJC2:TMultiJC):Integer
    begin
     Result := CompareValue(mJC1.id, mJC2.id);
    end
 );
end;

////////////////////////////////////////////////////////////////////////////////

function TMultiJC.Match(startNav:TBlkNav; vb: TList<TObject>; endBlk:TBlk):Boolean;
var jc: TJC;
    j: Integer;
begin
 if (Self.data.JCs.Count < 2) then Exit(false);

 jc := JCDb.GetJCByID(Self.data.JCs[0]);
 if (JC = nil) then Exit(false);

 if (JC.data.NavestidloBlok <> startNav.id) then
   Exit(false);

 // posledni blok musi byt posledni blok posledni jizdni cesty
 JC := JCDb.GetJCByID(Self.data.JCs[Self.data.JCs.Count-1]);
 if (JC.data.Useky[JC.data.Useky.Count-1] <> endBlk.id) then Exit(false);

 // kontrola variantnich bodu
 if (vb.Count <> Self.data.vb.Count) then Exit(false);
 for j := 0 to vb.Count-1 do
   if (Self.data.vb[j] <> (vb[j] as TBlk).id) then Exit(false);

 JC := JCDb.GetJCByID(Self.data.JCs[0]);
 if (Integer(startNav.ZacatekVolba) <> Integer(JC.data.TypCesty)) then
   Exit(false);

 for j := 0 to Self.data.JCs.Count-1 do
   if (JCDb.GetJCByID(Self.data.JCs[j]) = nil) then Exit(false);

 Exit(true);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
