unit FunkceVyznam;

{
  Trida TFuncsVyznam, resp. jeji instance FuncsFyznam si udrzuje seznam vyznamu
  funkci hnacich vozidel platnych pro cele kolejiste. Polozkami senzmau jsou
  napriklad "houkaèka dlouhá", "trubka vlakvedoucího", ...

  Vyznamy jsou udrzovany v jednoduchem senzamu "vyznamy".
}

interface

uses Generics.Collections, Classes;

type
  TGeneralEvent = procedure(Sender: TObject) of object;

  TFuncsVyznam = class
    private const
      _MAX_FUNCS_VYZNAM = 24;

    private
      vyznamy: TStrings;
      fOnChange: TGeneralEvent;

    public
       constructor Create();
       destructor Destroy(); override;

       procedure ParseWholeList(data:string);
       procedure ParseNewItems(data:string);
       function GetFuncsVyznam():string;

       property Items:TStrings read vyznamy;
       property OnChange:TGeneralEvent read fOnChange write fOnChange;
  end;

var
  FuncsFyznam : TFuncsVyznam;

implementation

uses ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TFuncsVyznam.Create();
begin
 inherited;
 Self.vyznamy := TStringList.Create();
end;//ctor

destructor TFuncsVyznam.Destroy();
begin
 Self.vyznamy.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TFuncsVyznam.ParseWholeList(data:string);
begin
 Self.vyznamy.Clear();
 ExtractStringsEx([';'], [], data, Self.vyznamy);
 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

procedure TFuncsVyznam.ParseNewItems(data:string);
var sl:TStrings;
    str:string;
begin
 sl := TStringList.Create();
 try
   ExtractStringsEx([';'], [], data, sl);

   for str in sl do
    if ((Self.vyznamy.Count < _MAX_FUNCS_VYZNAM) and (Self.vyznamy.IndexOf(str) = -1)) then
      Self.vyznamy.Add(str);
 finally
   sl.Free();
 end;

 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

////////////////////////////////////////////////////////////////////////////////

function TFuncsVyznam.GetFuncsVyznam():string;
var i:Integer;
begin
 Result := '';
 for i := 0 to Self.vyznamy.Count-1 do
   Result := Result + '{' + Self.vyznamy[i] + '};';
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  FuncsFyznam := TFuncsVyznam.Create();

finalization
  FuncsFyznam.Free();

end.//unit
