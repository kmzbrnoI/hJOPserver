unit ownStrUtils;

interface

uses SysUtils, Classes, StrUtils, System.Character;

function RemoveWhiteSpace(const s: string): string;
procedure ExtractStringsEx(Separators: TSysCharSet; Ignore: TSysCharSet; Content: string; var Strings: TStrings);
function RandomToken(len: Cardinal): string;

function EscapeNewline(const orig: string): string;
function DescapeNewline(const orig: string): string;

function StrTillChar(str: string; char: Char): string;

implementation

////////////////////////////////////////////////////////////////////////////////

function RemoveWhiteSpace(const s: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s));
  j := 0;
  for i := 1 to Length(s) do begin
    if not s[i].IsWhiteSpace() then begin
      inc(j);
      Result[j] := s[i];
    end;
  end;
  SetLength(Result, j);
end;

////////////////////////////////////////////////////////////////////////////////
// Vlastni parsovani stringu predevsim pro TCP komunikaci.
// Toto parsovani oproti systemovemu ExtractStrings oddeluje i pradzne stringy.
// Navic cokoliv ve znacich "{" a "}" je povazovano jako plaintext bez oddelnovacu.
// Tyto znaky mohou by i zanorene.
// Napr. text: ahoj;ja;jsem;{Honza;Horacek}
//    vrati: ["ahoj", "ja", "jsem", "Honza;Horacek"]

procedure ExtractStringsEx(Separators: TSysCharSet; Ignore: TSysCharSet; Content: string; var Strings: TStrings);
var i: word;
    s: string;
    plain_cnt: Integer;
 begin
  s := '';
  plain_cnt := 0;
  if (Length(Content) = 0) then Exit();

  for i := 1 to Length(Content) do
   begin
    if (Content[i] = '{') then
     begin
      if (plain_cnt > 0) then s := s + Content[i];
      Inc(plain_cnt);
     end
    else if ((Content[i] = '}') and (plain_cnt > 0)) then
     begin
      Dec(plain_cnt);
      if (plain_cnt > 0) then s := s + Content[i];
     end
    else begin
      if ((CharInSet(Content[i], Separators)) and (plain_cnt = 0)) then
       begin
        Strings.Add(s);
        s := '';
       end else
        if (not CharInSet(Content[i], Ignore) or (plain_cnt > 0)) then
          s := s + Content[i];
    end;// else Content[i]
   end;

  if (s <> '') then Strings.Add(s);
end;

////////////////////////////////////////////////////////////////////////////////

function RandomToken(len: Cardinal): string;
var i: Integer;
const _ALL: string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTVUWXYZ0123456789';
begin
 Result := '';
 for i := 0 to len-1 do
   Result := Result + _ALL[Random(Length(_ALL))+1];
end;

////////////////////////////////////////////////////////////////////////////////

function EscapeNewline(const orig: string): string;
begin
 Result := StringReplace(orig, #13#10, '\n', [rfReplaceAll]);
end;

function DescapeNewline(const orig: string): string;
begin
 Result := StringReplace(orig, '\n', #13#10, [rfReplaceAll]);
end;

////////////////////////////////////////////////////////////////////////////////

function StrTillChar(str: string; char: Char): string;
var endi: Integer;
begin
 endi := 1;
 while ((endi <= Length(str)) and (str[endi] <> char)) do
   Inc(endi);

 Result := LeftStr(str, endi-1);
end;

end.//unit
