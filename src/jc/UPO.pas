unit UPO;

{
  Tato unita implementuje struktury a nekolik funkci okolo upozorneni odesilanych
  do panelu. Upozorneni je standardne hlaska v leve dolni casti panelu
  zobrazujici bariery pri staveni jizdnich cest.
}

interface

uses Graphics, Generics.Collections, Classes, System.Math, colorHelper;

const
  _UPO_LINES = 3; // pocet radek upozorneni
  _UPO_LINE_LEN = 36; // delka radku upozorneni ve znacich

type

  // jeden radek upozorneni
  TUPOLine = record
    str: string; // text upozorneni
    fg: TColor; // barva popredi (tj. textu)
    bg: TColor; // barva pozadi
    align: TAlignment; // zarovnani textu
  end;

  TUPOItem = array [0 .. _UPO_LINES - 1] of TUPOLine; // jedno upozorneni = pole 3 radku
  TUPOItems = TList<TUPOItem>; // vice upozorneni = senzma upozorneni

function GetUPOLine(str: string; align: TAlignment = taCenter;
  // vrati TUPOLine na zaklade parametru funkce, slouzi pro jednoduche vytvareni TUPOLine
  fg: TColor = clNone; bg: TColor = clNone): TUPOLine;
function GetLines(str: string; line_length: Integer): TStrings; // vrati pcet radku stringu \str v uporneni

function NoteUPO(blockName: string; note: string): TUPOItem;
procedure AddNoteUPO(blockName: string; note: string; var items: TUPOItems);
function PNUPO(signalName: string): TUPOItem;
function NCUPO(name: string): TUPOItem;

implementation

function GetUPOLine(str: string; align: TAlignment = taCenter; fg: TColor = clNone; bg: TColor = clNone): TUPOLine;
begin
  Result.str := str;
  Result.fg := fg;
  Result.bg := bg;
  Result.align := align;
end;

function GetLines(str: string; line_length: Integer): TStrings;
begin
  Result := TStringList.Create();
  try
    var i: Integer := 1;
    while (i <= Length(str)) do
    begin
      Result.Add(copy(str, i, Min(Length(str) - i + 1, line_length)));
      i := i + Min(Length(str), line_length);
    end;
  except
    Result.Free();
    raise;
  end;
end;

function NoteUPO(blockName: string; note: string): TUPOItem;
begin
  Result[0] := GetUPOLine('ŠTÍTEK ' + blockName, taCenter, TJopColor.black, TJopColor.turqDark);
  var lines := GetLines(note, _UPO_LINE_LEN);

  try
    Result[1] := GetUPOLine(lines[0], taLeftJustify, TJopColor.yellow, TJopColor.grayDark);
    if (lines.Count > 1) then
      Result[2] := GetUPOLine(lines[1], taLeftJustify, TJopColor.yellow, TJopColor.grayDark)
    else
      Result[2] := GetUPOLine('', taLeftJustify, TJopColor.yellow, TJopColor.grayDark);
  finally
    lines.Free();
  end;
end;

procedure AddNoteUPO(blockName: string; note: string; var items: TUPOItems);
begin
  if (note <> '') then
    items.Add(NoteUPO(blockName, note));
end;

function PNUPO(signalName: string): TUPOItem;
begin
  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Svítí pøivolávací návìst');
  Result[2] := GetUPOLine(signalName);
end;

function NCUPO(name: string): TUPOItem;
begin
  Result[0] := GetUPOLine('POZOR !', taCenter, TJopColor.black, TJopColor.yellow);
  Result[1] := GetUPOLine('Postavená nouzová cesta');
  Result[2] := GetUPOLine(name);
end;

end.
