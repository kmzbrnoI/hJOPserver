unit PTUtils;

{
  Tato unita obsahuje uzitecne funkce pro PTserver a unity vyuzivajici
  funkcionalitu PTserveru.
}

interface

uses JsonDataObjects, Generics.Collections, Classes, Character;

procedure PtErrorToJson(json:TJsonObject; httpCode:string; title:string; detail:string = '');
procedure HttpParametersToDict(params:TStrings; var dict:TDictionary<string, string>);
function HttpParamToBool(value:string):Boolean;

implementation

////////////////////////////////////////////////////////////////////////////////

procedure PtErrorToJson(json:TJsonObject; httpCode:string; title:string; detail:string = '');
begin
 json['code']  := httpCode;
 json['title'] := title;
 if (detail <> '') then json['detail'] := detail;
end;

////////////////////////////////////////////////////////////////////////////////

procedure HttpParametersToDict(params:TStrings; var dict:TDictionary<string, string>);
var param:string;
    splitted:TStrings;
begin
 splitted := TStringList.Create();
 try
   dict.Clear();
   for param in params do
    begin
     splitted.Clear();
     ExtractStrings(['='], [], PChar(param), splitted);
     if (splitted.Count = 2) then dict.Add(splitted[0], splitted[1]);
    end;
 finally
   splitted.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function HttpParamToBool(value:string):Boolean;
begin
 Result := (ToLower(value) = 'true') or (value = '1');
end;

////////////////////////////////////////////////////////////////////////////////

end.
