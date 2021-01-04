unit ownConvert;

interface

uses Windows, SysUtils, Graphics, StrUtils;

function BoolToYesNo(bool: Boolean): string;
function BoolToTick(bool: Boolean): string;
function BoolToInt(bool: Boolean): Integer;
function IntToBool(int: Integer): Boolean;
function StrToBool(str: string): Boolean;

function StrToColor(str: string): TColor;
function ColorToStr(color: TColor): string;


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

////////////////////////////////////////////////////////////////////////////////

end.
