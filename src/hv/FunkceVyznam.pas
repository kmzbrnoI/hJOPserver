unit FunkceVyznam;

{
  Trida TFuncsVyznam, resp. jeji instance FuncsFyznam si udrzuje seznam vyznamu
  funkci hnacich vozidel platnych pro cele kolejiste. Polozkami senzmau jsou
  napriklad "houkačka dlouhá", "trubka vlakvedoucího", ...

  Vyznamy jsou udrzovany v jednoduchem senzamu "vyznamy".
}

interface

uses Generics.Collections, Classes, THnaciVozidlo, Generics.Defaults, SysUtils;

type
  TGeneralEvent = procedure(Sender: TObject) of object;

  TFuncName = class
    name: string;
    typ: THVFuncType;

    constructor Create(str: string); overload;
    constructor Create(name: string; typ: THVFuncType); overload;
    function GetPanelStr(): string;

    class function Comparer(): IComparer<TFuncName>;
  end;

  TFuncNames = class
    private const
      _MAX_FUNCS_VYZNAM = 50;

    private
      names: TObjectList<TFuncName>;
      fOnChange: TGeneralEvent;

    public
       constructor Create();
       destructor Destroy(); override;

       procedure ParseWholeList(data: string);
       procedure ParseNewItems(data: string);
       function AllNames(separator: string = ','): string;
       function IsName(name: string): Boolean;
       function GetNameIndex(name: string): Integer;

       property Items: TObjectList<TFuncName> read names;
       property OnChange: TGeneralEvent read fOnChange write fOnChange;
  end;

var
  FuncNames : TFuncNames;

implementation

uses ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TFuncNames.Create();
begin
 inherited;
 Self.names := TObjectList<TFuncName>.Create();
end;

destructor TFuncNames.Destroy();
begin
 Self.names.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFuncNames.ParseWholeList(data: string);
var strs: TStrings;
    str: string;
begin
 Self.names.Clear();
 strs := TStringList.Create();
 try
   ExtractStringsEx([';', ','], [], data, strs);
   for str in strs do
     if (Self.names.Count < _MAX_FUNCS_VYZNAM) then
       Self.names.Add(TFuncName.Create(str));
 finally
   strs.Free();
 end;
 Self.names.Sort(TFuncName.Comparer());

 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

procedure TFuncNames.ParseNewItems(data: string);
var sl: TStrings;
    str: string;
begin
 sl := TStringList.Create();
 try
   ExtractStringsEx([';', ','], [], data, sl);

   for str in sl do
    begin
     if (Self.names.Count < _MAX_FUNCS_VYZNAM) then
      begin
        if (not Self.IsName(str)) then
          Self.names.Add(TFuncName.Create(str))
        else begin
          Self.names.Delete(Self.GetNameIndex(str));
          Self.names.Add(TFuncName.Create(str));
        end;
      end;
    end;
 finally
   sl.Free();
 end;

 Self.names.Sort(TFuncName.Comparer());
 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

////////////////////////////////////////////////////////////////////////////////

function TFuncNames.AllNames(separator: string = ','): string;
begin
 Result := '';
 for var i := 0 to Self.names.Count-1 do
   Result := Result + '{' + Self.names[i].GetPanelStr() + '}' + separator;
end;

////////////////////////////////////////////////////////////////////////////////

function TFuncNames.GetNameIndex(name: string): Integer;
begin
 for var i := 0 to Self.names.Count-1 do
   if (Self.names[i].name = name) then
     Exit(i);
 Result := -1;
end;

function TFuncNames.IsName(name: string): Boolean;
begin
 Result := (Self.GetNameIndex(name) > -1);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TFuncName.Create(str: string);
var strings: TStrings;
begin
 strings := TStringList.Create();
 try
   ExtractStringsEx([':'], [], str, strings);
   Self.name := strings[0];
   if (strings.Count > 1) then
     Self.typ := THV.CharToHVFuncType(strings[1][1])
   else
     Self.typ := THVFuncType.permanent;
 finally
   strings.Free();
 end;
end;

constructor TFuncName.Create(name: string; typ: THVFuncType);
begin
 Self.name := name;
 Self.typ := typ;
end;

function TFuncName.GetPanelStr(): string;
begin
 Result := Self.name + ':' + THV.HVFuncTypeToChar(Self.typ);
end;

class function TFuncName.Comparer(): IComparer<TFuncName>;
begin
 Result := TComparer<TFuncName>.Construct(
  function(const Left, Right: TFuncName): Integer
   begin
    Result := CompareStr(Left.name, Right.name, loUserLocale);
   end
 );
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  FuncNames := TFuncNames.Create();

finalization
  FuncNames.Free();

end.//unit
