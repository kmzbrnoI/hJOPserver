unit ownConvert;

interface

uses Windows, SysUtils, Graphics, StrUtils, Generics.Collections;

function BoolToYesNo(bool: Boolean): string;
function BoolToTick(bool: Boolean): string;
function BoolToInt(bool: Boolean): Integer;
function IntToBool(int: Integer): Boolean;
function StrToBool(str: string): Boolean;

function StrToColor(str: string): TColor;
function ColorToStr(color: TColor): string;

function SerializeIntList(l: TList<Integer>): string;

function GetObjsList(first: TObject = nil; second: TObject = nil; third: TObject = nil): TList<TObject>;

implementation

function BoolToYesNo(bool: Boolean): string;
 begin
  if (Bool) then
    Result := 'Ano'
  else
    Result := 'Ne';
 end;

function BoolToTick(bool: Boolean): string;
 begin
  if (bool) then
    Result := '✓'
  else
    Result := '';
 end;

function BoolToInt(bool: Boolean): Integer;
 begin
  if (Bool) then
    Result := 1
  else
    Result := 0;
 end;

function IntToBool(int: Integer): Boolean;
 begin
  Result := (Int > 0);
 end;

function StrToBool(str: string): Boolean;
begin
 Result := (str = '1');
end;

function StrToColor(str: string): TColor;
begin
 Result := RGB(StrToInt('$'+LeftStr(str, 2)), StrToInt('$'+Copy(str, 3, 2)), StrToInt('$'+RightStr(str, 2)));
end;

function ColorToStr(color: TColor): string;
begin
 Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2);
end;

function SerializeIntList(l: TList<Integer>): string;
begin
  Result := '';
  for var i := 0 to l.Count-2 do
    Result := Result + IntToStr(l[i]) + ',';
  if (l.Count > 0) then
    Result := Result + IntToStr(l[l.Count-1]);
end;

function GetObjsList(first: TObject = nil; second: TObject = nil; third: TObject = nil): TList<TObject>;
begin
  Result := TList<TObject>.Create();
  try
    if (first <> nil) then
      Result.Add(first);
    if (second <> nil) then
      Result.Add(second);
    if (third <> nil) then
      Result.Add(third);
  except
    Result.Free();
    raise;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
