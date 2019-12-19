unit TechnologieAB;

{
  Trida TABlist udrzuje seznam automaticky stavenych jizdnich cest a postupne
  se pokousi je stavet.
}

interface

uses SysUtils, Generics.Collections, TechnologieJC;

type
  EABJCAlreadyInList = class(Exception);
  EABJCNotInList = class(Exception);

  TABlist = class
   private
    JCs: TList<TJC>;

     function GetItem(index:Integer):TJC;
     procedure TryJC(jc:TJC);

   public

     constructor Create();
     destructor Destroy(); override;

     procedure Add(jc:TJC);
     procedure Remove(jc:TJC);
     function Contains(jc:TJC):boolean;

     procedure Update();

     function IsUsekInAnyABJC(usekid:Integer):boolean;

     property Items[index : integer]: TJC read GetItem; default;
  end;

var
  ABlist: TABlist;

implementation

uses DataAB, TBlok, TBloky, TBlokUsek, logging, TBlokNav;

////////////////////////////////////////////////////////////////////////////////

constructor TABlist.Create();
begin
 inherited;
 Self.JCs := TList<TJC>.Create();
end;

destructor TABlist.Destroy();
begin
 Self.JCs.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABlist.Add(jc:TJC);
begin
 if (Self.JCs.Contains(jc)) then
   raise EABJCAlreadyInList.Create('This JC is already in AB list!');

 Self.JCs.Add(jc);
 ABTableData.AddJC(jc);
 writelog('AB: JC '+jc.nazev+' přidána do seznamu AB JC', WR_VC);
end;

procedure TABlist.Remove(jc:TJC);
var i:Integer;
    usek:Integer;
    blk:TBlk;
begin
 if (not Self.JCs.Contains(jc)) then
   raise EABJCNotInList.Create('This JC is not in AB list!');

 for usek in jc.data.Useky do
  begin
   Blky.GetBlkByID(usek, blk);
   if ((blk <> nil) and
       ((blk.typ = _BLK_USEK) or (blk.typ = _BLK_TU)) and
       (TBlkUsek(blk).Zaver = TZaver.ab)) then
     TBlkUsek(blk).Zaver := TZaver.no;
  end;

 i := Self.JCs.IndexOf(jc);
 writelog('AB: JC '+Self.JCs[i].nazev+' odstraněna ze seznamu AB JC', WR_VC);
 Self.JCs.Delete(i);
 ABTableData.DeleteJC(i);
end;

function TABlist.Contains(jc:TJC):boolean;
begin
 Result := Self.JCs.Contains(jc);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABlist.TryJC(jc:TJC);
var bariery:TJCBariery;
    blk:TBlk;
    bariera:TJCBariera;
begin
  bariery := JC.KontrolaPodminek();
  try
    // v jizdni ceste jsou urcite bariery (musi tam byt minimalne zavery AB cesty)
    for bariera in bariery do
     begin
      if ((bariera.typ <> TJC._JCB_USEK_AB) and
          ((TJC.CriticalBariera(bariera.typ)) or (not JC.WarningBariera(bariera.typ)))) then
        Exit();
     end;

    // Tady mame zajisteno, ze v jizdni ceste nejsou kriticke ani nevarovne bariery
    //  (KontrolaPodminek() zarucuje, ze tyto typy barier jsou na zacatku seznamu).
    // Upozornovaci bariery ignorujeme a stavime JC.

    writelog('DN JC '+JC.nazev+' : podmínky splněny, stavím', WR_STACK);

    Blky.GetBlkByID(JC.data.NavestidloBlok, blk);
    if ((blk = nil) or (blk.typ <> _BLK_NAV) or (TBlkNav(blk).OblsRizeni.Count = 0)) then
      Self.Remove(jc);

    JC.StavJC(nil, TBlkNav(blk).OblsRizeni[0], nil, false, true);
  finally
    bariery.Free();
  end;
end;

// Zkousi stavet kazdou z jizdnich cest v seznamu Self.JCs.
procedure TABlist.Update();
var jc:TJC;
begin
 for jc in Self.JCs do
  begin
   if ((jc.postaveno) or (jc.staveni)) then continue;
   Self.TryJC(jc);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TABlist.GetItem(index:Integer):TJC;
begin
 Result := Self.JCs[index];
end;

////////////////////////////////////////////////////////////////////////////////

function TABlist.IsUsekInAnyABJC(usekid:Integer):boolean;
var jc:TJC;
    id:Integer;
begin
 for jc in Self.JCs do
   for id in jc.data.Useky do
     if (id = usekid) then
       Exit(true);

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ABlist := TABlist.Create();

finalization
  FreeAndNil(ABlist);

end.
