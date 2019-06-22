unit FunkceVyznam;

{
  Trida TFuncsVyznam, resp. jeji instance FuncsFyznam si udrzuje seznam vyznamu
  funkci hnacich vozidel platnych pro cele kolejiste. Polozkami senzmau jsou
  napriklad "houkaèka dlouhá", "trubka vlakvedoucího", ...

  Vyznamy jsou udrzovany v jednoduchem senzamu "vyznamy".
}

interface

uses Generics.Collections, Classes, THnaciVozidlo;

type
  TGeneralEvent = procedure(Sender: TObject) of object;

  TFuncVyznam = class
    popis:string;
    typ:THVFuncType;

    constructor Create(str:string); overload;
    constructor Create(popis:string; typ:THVFuncType); overload;
    function GetPanelStr():string;
  end;

  TFuncsVyznam = class
    private const
      _MAX_FUNCS_VYZNAM = 24;

    private
      vyznamy: TObjectList<TFuncVyznam>;
      fOnChange: TGeneralEvent;

    public
       constructor Create();
       destructor Destroy(); override;

       procedure ParseWholeList(data:string);
       procedure ParseNewItems(data:string);
       function GetFuncsVyznam():string;
       function IsVyznam(vyznam:string):Boolean;
       function GetVyznamIndex(vyznam:string):Integer;

       property Items:TObjectList<TFuncVyznam> read vyznamy;
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
 Self.vyznamy := TObjectList<TFuncVyznam>.Create();
end;//ctor

destructor TFuncsVyznam.Destroy();
begin
 Self.vyznamy.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TFuncsVyznam.ParseWholeList(data:string);
var strs:TStrings;
    str:string;
begin
 Self.vyznamy.Clear();
 strs := TStringList.Create();
 try
   ExtractStringsEx([';'], [], data, strs);
   for str in strs do
     if (Self.vyznamy.Count < _MAX_FUNCS_VYZNAM) then
       Self.vyznamy.Add(TFuncVyznam.Create(str));
 finally
   strs.Free();
 end;

 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

procedure TFuncsVyznam.ParseNewItems(data:string);
var sl:TStrings;
    str:string;
    i:Integer;
begin
 sl := TStringList.Create();
 try
   ExtractStringsEx([';'], [], data, sl);

   for str in sl do
    begin
     if (Self.vyznamy.Count < _MAX_FUNCS_VYZNAM) then
      begin
        if (not Self.IsVyznam(str)) then
          Self.vyznamy.Add(TFuncVyznam.Create(str))
        else begin
          i := Self.GetVyznamIndex(str);
          Self.vyznamy.Delete(i);
          Self.vyznamy.Insert(i, TFuncVyznam.Create(str));
        end;
      end;
    end;
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
   Result := Result + '{' + Self.vyznamy[i].GetPanelStr() + '};';
end;

////////////////////////////////////////////////////////////////////////////////

function TFuncsVyznam.GetVyznamIndex(vyznam:string):Integer;
var i:Integer;
begin
 for i := 0 to Self.vyznamy.Count-1 do
   if (Self.vyznamy[i].popis = vyznam) then
     Exit(i);
 Result := -1;
end;

function TFuncsVyznam.IsVyznam(vyznam:string):Boolean;
begin
 Result := (Self.GetVyznamIndex(vyznam) > -1);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TFuncVyznam.Create(str:string);
var strings:TStrings;
begin
 strings := TStringList.Create();
 try
   ExtractStringsEx([':'], [], str, strings);
   Self.popis := strings[0];
   if (strings.Count > 1) then
     Self.typ := THV.CharToHVFuncType(strings[1][1])
   else
     Self.typ := THVFuncType.permanent;
 finally
   strings.Free();
 end;
end;

constructor TFuncVyznam.Create(popis:string; typ:THVFuncType);
begin
 Self.popis := popis;
 Self.typ := typ;
end;

function TFuncVyznam.GetPanelStr():string;
begin
 Result := Self.popis + ':' + THV.HVFuncTypeToChar(Self.typ);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  FuncsFyznam := TFuncsVyznam.Create();

finalization
  FuncsFyznam.Free();

end.//unit
