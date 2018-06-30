unit timeHelper;

{
  TimeHelper poskytuje uzitecne funkce pojici se s casem.
}

interface

// Vrati skutecny cas zbyvajici do casu "future". Pokud je zaply modelovy cas,
// "future" bere v modelovem casu, jinak ve skutecnem.
function RealDelta(future:TTime):TTime;

// Vrati aktualni cas podle toho, jestli je aktivni modelovy/skutecny.
function hJOPnow():TTime;

implementation

uses SysUtils, ModelovyCas;

function RealDelta(future:TTime):TTime;
begin
 if (ModCas.used) then
  begin
   Result := (future - ModCas.dateTime) / ModCas.speed;
  end else begin
   Result := future - Now;
  end;
end;

function hJOPnow():TTime;
begin
 if (ModCas.used) then
   Result := ModCas.dateTime
 else
   Result := Now;
end;

end.
