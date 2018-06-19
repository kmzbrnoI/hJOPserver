unit TechnologieAB;

{
  Trida TABlist udrzuje seznam automaticky stavenych jizdnich cest a postupne
  se pokousi je stavet.
}

interface

uses SysUtils, Generics.Collections, TechnologieJC;

type
  EJCAlreadyInList = class(Exception);
  EJCNotInList = class(Exception);

  TABlist = class
   private
    JCs: TList<TJC>;
   public

     constructor Create();
     destructor Destroy(); override;

     procedure Add(jc:TJC);
     procedure Remove(jc:TJC);
     function Contains(jc:TJC):boolean;

     procedure Update();
  end;

var
  ABlist: TABlist;

implementation

uses DataAB, TBlok, TBloky, TBlokUsek;

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
   raise EJCAlreadyInList.Create('This JC is already in AB list!');

 Self.JCs.Add(jc);
 ABTableData.AddJC(jc);
end;

procedure TABlist.Remove(jc:TJC);
var i:Integer;
    usek:Integer;
    blk:TBlk;
begin
 if (not Self.JCs.Contains(jc)) then
   raise EJCNotInList.Create('This JC is not in AB list!');


 for usek in jc.data.Useky do
  begin
   Blky.GetBlkByID(usek, blk);
   if ((blk <> nil) and
       ((blk.GetGlobalSettings().typ = _BLK_USEK) or (blk.GetGlobalSettings().typ = _BLK_TU)) and
       (TBlkUsek(blk).Zaver = TZaver.ab)) then
     TBlkUsek(blk).Zaver := TZaver.no;
  end;

 i := Self.JCs.IndexOf(jc);
 Self.JCs.Delete(i);
 ABTableData.RemoveJC(i);
end;

function TABlist.Contains(jc:TJC):boolean;
begin
 Result := Self.JCs.Contains(jc);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABlist.Update();
begin

end;

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

initialization
  ABlist := TABlist.Create();

finalization
  FreeAndNil(ABlist);

end.
