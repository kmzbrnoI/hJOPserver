unit UPO;

// Tato unita implementuje struktury a par funkci okolo upozorneni odesilaneho do klienta

interface

uses Graphics, Generics.Collections, Classes, IBUtils;

const
  _UPO_LINES = 3;
  _UPO_LINE_LEN = 36;

type

  TUPOLine = record
   str:string;
   fg:TColor;
   bg:TColor;
   align:TAlignment;
  end;

  TUPOItem = array [0.._UPO_LINES-1] of TUPOLine;
  TUPOItems = TList<TUPOItem>;

  function GetUPOLine(str:string; align:TAlignment = taCenter; fg:TColor = clNone; bg:TColor = clNone):TUPOLine;
  function GetLines(str:string; line_length:Integer):TStrings;

implementation

function GetUPOLine(str:string; align:TAlignment = taCenter; fg:TColor = clNone; bg:TColor = clNone):TUPOLine;
begin
 Result.str   := str;
 Result.fg    := fg;
 Result.bg    := bg;
 Result.align := align;
end;//function

function GetLines(str:string; line_length:Integer):TStrings;
var i:Integer;
begin
 Result := TStringList.Create();

 i := 0;
 while (i < Length(str)) do
  begin
   Result.Add(copy(str, i, Min(Length(str)-i, line_length)));
   i := i + Min(Length(str), line_length);
  end;//while
end;//function

end.//unit
