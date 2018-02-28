unit timeHelper;

{
  TimeHelper poskytuje uzitecne funkce pojici se s casem.
}

interface

// Vrati skutecny cas zbyvajici do casu "future". Pokud je zaply modelovy cas,
// "future" bere v modelovem casu, jinak ve skutecnem.
function RealDelta(future:TTime):TTime;

implementation

uses SysUtils, ModelovyCas;

function RealDelta(future:TTime):TTime;
begin
 if (ModCas.used) then
  begin
   Result := (future - ModCas.time) / ModCas.speed;
  end else begin
   Result := future - Now;
  end;
end;

end.
