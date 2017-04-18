unit Prevody;

interface

uses Windows, SysUtils, Graphics, StrUtils;

type
 TPrevody=class
    function DecToBin(Cislo:Integer;Mista:Integer):String;
    function BoolToStr(Bool:Boolean):String;
    function BoolToInt(Bool:Boolean):Integer;
    function IntToBool(Int:Integer):boolean;
    function StrToBool(str:string):boolean;

    function StrToColor(str:string):TColor;
    function ColorToStr(color:TColor):string;
 end;

var
 PrevodySoustav:TPrevody;
 
implementation

function TPrevody.DecToBin(Cislo:Integer;Mista:Integer): string;
var s: string;
    i:Integer;
 begin
  s := '';
  for i := 0 to Mista-1 do begin
   if ((cislo mod 2) = 1) then s := '1' + s else s := '0' + s;
    cislo := cislo shr 1;
   end;
  Result := s;
 end;//function

function TPrevody.BoolToStr(Bool:Boolean):String;
 begin
  if (Bool) then
   begin
    Result := 'Ano';
   end else begin
    Result := 'Ne';
   end;//else Bool
 end;//function

function TPrevody.BoolToInt(Bool:Boolean):Integer;
 begin
  if (Bool) then
   begin
    Result := 1;
   end else begin
    Result := 0;
   end;
 end;//function

function TPrevody.IntToBool(Int:Integer):Boolean;
 begin
  if (Int = 1) then
   begin
    Result := true;
   end else begin
    Result := false
   end;
 end;//function

function TPrevody.StrToBool(str:string):boolean;
begin
 if (str = '1') then Result := true
 else Result := false;
end;//function

function TPrevody.StrToColor(str:string):TColor;
begin
 Result := RGB(StrToInt('$'+LeftStr(str, 2)), StrToInt('$'+Copy(str, 2, 2)), StrToInt('$'+RightStr(str, 2)));
end;//function

function TPrevody.ColorToStr(Color:TColor):string;
begin
 Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2);
end;//function


end.//uses
