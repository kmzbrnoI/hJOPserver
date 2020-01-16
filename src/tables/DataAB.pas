unit DataAB;

{
  Trida TABTableData se stara o vykreslovani tabulky automaticky stavenych
  jizdnich cest.
}

interface

uses ComCtrls, SysUtils, TechnologieJC, Generics.Collections;

type
  TABGuiOpType = (opAdd, opDelete, opClear);
  TABGuiOp = record
    typ: TABGuiOpType;
    index: Integer;
    jc: TJC;
  end;

  TABTableData = class
    private
     LV:TListView;
     q:TQueue<TABGuiOp>;

      function GetOp(typ:TABGuiOpType; index:Integer; jc:TJC):TABGuiOp;
      function GetOpAdd(jc:TJC):TABGuiOp;
      function GetOpDelete(index:Integer):TABGuiOp;
      function GetOpClear():TABGuiOP;

    public
      constructor Create(LV:TListView);
      destructor Destroy(); override;

      procedure AddJC(JC:TJC);
      procedure DeleteJC(index:Integer);
      procedure Clear();
      procedure Update();

  end;

var
  ABTableData : TABTableData;

implementation

uses TechnologieAB;

////////////////////////////////////////////////////////////////////////////////

constructor TABTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
 Self.q := TQueue<TABGuiOp>.Create()
end;

destructor TABTableData.Destroy();
begin
 Self.q.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABTableData.AddJC(JC:TJC);
begin
 Self.q.Enqueue(Self.GetOpAdd(JC));
end;

procedure TABTableData.DeleteJC(index:Integer);
begin
 Self.q.Enqueue(Self.GetOpDelete(index));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABTableData.Clear();
begin
 Self.q.Enqueue(Self.GetOpClear());
end;

////////////////////////////////////////////////////////////////////////////////

procedure TABTableData.Update();
var op:TABGuiOp;
    LI:TListItem;
begin
 while (Self.q.Count > 0) do
  begin
   op := Self.q.Dequeue;

   case (op.typ) of
    opAdd: begin
      LI := Self.LV.Items.Add();
      LI.Caption := op.jc.nazev;
    end;

    opDelete: if (op.index < Self.LV.Items.Count) then
                Self.LV.Items.Delete(op.index);
    opClear: Self.LV.Clear();

   end;
  end;

end;

////////////////////////////////////////////////////////////////////////////////

function TABTableData.GetOp(typ:TABGuiOpType; index:Integer; jc:TJC):TABGuiOp;
begin
 Result.typ := typ;
 Result.index := index;
 Result.jc := jc;
end;

function TABTableData.GetOpAdd(jc:TJC):TABGuiOp;
begin
 Result := Self.GetOp(opAdd, 0, jc);
end;

function TABTableData.GetOpDelete(index:Integer):TABGuiOp;
begin
 Result := Self.GetOp(opDelete, index, nil);
end;

function TABTableData.GetOpClear():TABGuiOP;
begin
 Result := Self.GetOp(opClear, 0, nil);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 ABTableData.Free();

end.//unit
