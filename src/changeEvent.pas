unit changeEvent;

interface

uses Generics.Collections;

type
  // databaze funkci k zavolani pri urcite udalosti (napr. pri obsazeni useku se vola uzavreni prejezdu do jizdni cesty)

  TChangeEventFunc = procedure(Sender: TObject; data: Integer) of object;

  TChangeEvent = record
    func: TChangeEventFunc;
    data: Integer;

    class operator Equal(a, b: TChangeEvent): Boolean;
  end;

  TChangeEvents = TList<TChangeEvent>;
function CreateChangeEvent(func: TChangeEventFunc; data: Integer): TChangeEvent;

implementation

function CreateChangeEvent(func: TChangeEventFunc; data: Integer): TChangeEvent;
begin
  Result.func := func;
  Result.data := data;
end;

class operator TChangeEvent.Equal(a, b: TChangeEvent): Boolean;
begin
  Result := ((@a.func = @b.func) and (a.data = b.data));
end;

end.
