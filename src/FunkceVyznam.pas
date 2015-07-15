unit FunkceVyznam;

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
 inherited Create();

 Self.vyznamy := TStringList.Create();
end;//ctor

destructor TFuncsVyznam.Destroy();
begin
 Self.vyznamy.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TFuncsVyznam.ParseWholeList(data:string);
begin
 Self.vyznamy.Clear();
 ExtractStringsEx([';'], [], data, Self.vyznamy);
 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;//procedure

procedure TFuncsVyznam.ParseNewItems(data:string);
var sl:TStrings;
    str:string;
begin
 sl := TStringList.Create();
 ExtractStringsEx([';'], [], data, sl);

 for str in sl do
  if ((Self.vyznamy.Count < _MAX_FUNCS_VYZNAM) and (Self.vyznamy.IndexOf(str) = -1)) then
    Self.vyznamy.Add(str);

 sl.Free();

 if (Assigned(Self.OnChange)) then Self.OnChange(Self);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TFuncsVyznam.GetFuncsVyznam():string;
var i:Integer;
begin
 Result := '';
 for i := 0 to Self.vyznamy.Count-1 do
   Result := Result + '{' + Self.vyznamy[i] + '};';
end;//function

////////////////////////////////////////////////////////////////////////////////

initialization
  FuncsFyznam := TFuncsVyznam.Create();
finalization
  FuncsFyznam.Free();

end.//unit
