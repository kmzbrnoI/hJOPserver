unit changeEvent;

interface

uses Generics.Collections;

type
 // databaze funkci k zavolani pri urcite udalosti (napr. pri obsazeni useku se vola uzavreni prejezdu do jizdni cesty)

 TChangeEventFunc = procedure(Sender:TObject; data:Integer) of object;

 TChangeEvent = record
  func:TChangeEventFunc;
  data:Integer;
 end;

 TChangeEvents = TList<TChangeEvent>;
 function CreateChangeEvent(func:TChangeEventFunc; data:Integer):TChangeEvent;

implementation

function CreateChangeEvent(func:TChangeEventFunc; data:Integer):TChangeEvent;
begin
 Result.func := func;
 Result.data := data;
end;

end.
