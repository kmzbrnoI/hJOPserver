unit IfThenElse;

interface

function ite(cond: Boolean; tt: string; ff: string): string; overload;
function ite(cond: Boolean; tt: Integer; ff: Integer): Integer; overload;

implementation

function ite(cond: Boolean; tt: string; ff: string): string; overload;
begin
 if (cond) then
   Result := tt
 else
   Result := ff;
end;

function ite(cond: Boolean; tt: Integer; ff: Integer): Integer; overload;
begin
 if (cond) then
   Result := tt
 else
   Result := ff;
end;

end.
