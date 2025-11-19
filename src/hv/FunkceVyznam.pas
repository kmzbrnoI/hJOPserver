unit FunkceVyznam;

{
  Trida TFuncsVyznam, resp. jeji instance FuncsFyznam si udrzuje seznam vyznamu
  funkci hnacich vozidel platnych pro cele kolejiste. Polozkami seznamu jsou
  napriklad "houkačka dlouhá", "trubka vlakvedoucího", ...

  Vyznamy jsou udrzovany v jednoduchem senzamu "vyznamy".
}

interface

uses Generics.Collections, Classes, TRailVehicle, Generics.Defaults, SysUtils;

type
  TGeneralEvent = procedure(Sender: TObject) of object;

  TFuncNames = class
    private const
      _MAX = 50;

    private
      data: TDictionary<string, TRVFuncType>;
      fOnChange: TGeneralEvent;

       class function Entry(data: string): TPair<string, TRVFuncType>;

    public
       constructor Create();
       destructor Destroy(); override;

       procedure Add(data: string);
       function IsName(name: string): Boolean;
       procedure Clear();
       function All(): TList<TPair<string, TRVFuncType>>;
       function PanelStr(separator: string = ','): string;

       property dict: TDictionary<string, TRVFuncType> read data;
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
 Self.data := TDictionary<string, TRVFuncType>.Create();
end;

destructor TFuncNames.Destroy();
begin
 Self.data.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFuncNames.Clear();
begin
  Self.data.Clear();
end;

procedure TFuncNames.Add(data: string);
var strs: TStrings;
begin
 strs := TStringList.Create();
 try
   ExtractStringsEx([';', ','], [], data, strs);
   for var str: string in strs do
   begin
     if (Self.data.Count < _MAX) then
     begin
       var pair := TFuncNames.Entry(str);
       Self.data.AddOrSetValue(pair.Key, pair.Value);
     end;
   end;
 finally
   strs.Free();
 end;

 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;

function TFuncNames.IsName(name: string): Boolean;
begin
 Result := Self.data.ContainsKey(name);
end;

function TFuncNames.All(): TList<TPair<string, TRVFuncType>>;
begin
  Result := TList<TPair<string, TRVFuncType>>.Create(Self.data.ToArray());
  Result.Sort(TComparer<TPair<string, TRVFuncType>>.Construct(
    function(const Left, Right: TPair<string, TRVFuncType>): Integer
    begin
      Result := CompareStr(Left.Key, Right.Key, loUserLocale);
    end
  ));
end;

////////////////////////////////////////////////////////////////////////////////

function TFuncNames.PanelStr(separator: string = ','): string;
begin
  var _all: TList<TPair<string, TRVFuncType>> := Self.All();
  try
    Result := '';
    for var entry in _all do
      Result := Result + '{' + entry.Key + ':' + TRV.RVFuncTypeToChar(entry.Value) + '}' + separator;
  finally
    _all.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

class function TFuncNames.Entry(data: string): TPair<string, TRVFuncType>;
var strings: TStrings;
begin
 strings := TStringList.Create();
 try
   ExtractStringsEx([':'], [], data, strings);
   if (strings.Count > 0) then
     Result.Key := strings[0]
   else
     Result.Key := '';

   if (strings.Count > 1) and (Length(strings[1]) > 0) then
     Result.Value := TRV.CharToRVFuncType(strings[1][1])
   else
     Result.Value := TRVFuncType.permanent;
 finally
   strings.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  FuncNames := TFuncNames.Create();

finalization
  FuncNames.Free();

end.
