unit changeEvent;

interface

uses Generics.Collections;

type
  // databaze funkci k zavolani pri urcite udalosti (napr. pri obsazeni useku se vola uzavreni prejezdu do jizdni cesty)

  TChangeEventFuncPtr = procedure(Sender: TObject; data: Pointer) of object;
  TChangeEventFuncInt = procedure(Sender: TObject; data: Integer) of object;

  TChangeEvent = record
    func: TChangeEventFuncPtr;
    data: Pointer;

    class operator Equal(a, b: TChangeEvent): Boolean;
  end;

  TChangeEvents = TList<TChangeEvent>;

function CreateChangeEvent(func: TChangeEventFuncPtr; data: Pointer): TChangeEvent;
function CreateChangeEventInt(func: TChangeEventFuncInt; data: Integer): TChangeEvent;

implementation

function CreateChangeEvent(func: TChangeEventFuncPtr; data: Pointer): TChangeEvent;
begin
  Result.func := func;
  Result.data := data;
end;

function CreateChangeEventInt(func: TChangeEventFuncInt; data: Integer): TChangeEvent;
begin
  Result.func := TChangeEventFuncPtr(func);
  Result.data := Pointer(data);
end;

class operator TChangeEvent.Equal(a, b: TChangeEvent): Boolean;
begin
  Result := ((@a.func = @b.func) and (a.data = b.data));
end;

end.
