unit DataMultiJC;

// TMultiJCTableData - trida starajici se o vyplneni tabulky slozenych jizdnich
// cest

interface

uses ComCtrls, SysUtils, Classes;

type
  TMultiJCTableData = class
  private
    LV: TListView;

  public

    procedure LoadToTable();
    procedure UpdateTable();

    procedure UpdateLine(line: Integer);

    procedure AddJC(pos: Integer);
    procedure RemoveJC(index: Integer);

    constructor Create(LV: TListView);
  end;

var
  MultiJCTableData: TMultiJCTableData;

implementation

uses TMultiJCDatabase, TechnologieMultiJC, BlockDb, fMain, TJCDatabase, StrUtils;

/// /////////////////////////////////////////////////////////////////////////////

constructor TMultiJCTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJCTableData.LoadToTable();
var LI: TListItem;
begin
  Self.LV.Clear();

  for var i := 0 to MultiJCDb.Count - 1 do
  begin
    LI := Self.LV.Items.Add;
    LI.Caption := IntToStr(i);
    for var j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('');

    try
      Self.UpdateLine(i);
    except

    end;
  end; // for i

end;

procedure TMultiJCTableData.UpdateTable();
begin
  for var i := 0 to MultiJCDb.Count - 1 do
    if (MultiJCDb[i].changed) then
    begin
      try
        Self.UpdateLine(i);
        Self.LV.UpdateItems(i, i);
      except

      end;
    end;
end;

procedure TMultiJCTableData.UpdateLine(line: Integer);
var mJCData: TMultiJCData;
  mJC: TMultiJC;
begin
  mJC := MultiJCDb[line];
  mJCData := mJC.data;
  mJC.changed := false;

  Self.LV.Items[line].Caption := IntToStr(mJCData.id);
  Self.LV.Items[line].SubItems[0] := mJCData.name;

  // krok ( = aktualne stavena JC)
  Self.LV.Items[line].SubItems[1] := IntToStr(mJC.state.JCIndex);

  // jednotlive jizdni cesty
  begin
    var str := '';
    for var i := 0 to mJCData.JCs.Count - 1 do
    begin
      if (JCDb.GetJCByID(mJCData.JCs[i]) <> nil) then
        str := str + '(' + JCDb.GetJCByID(mJCData.JCs[i]).name + '), '
      else
        str := str + '(neexistujici JC), ';
    end;
    Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2);
  end;

  // variantni body
  begin
    var str := '';
    for var i := 0 to mJCData.vb.Count - 2 do
      str := str + Blocks.GetBlkName(mJCData.vb[i]) + ', ';
    if (mJCData.vb.Count > 0) then
      str := str + Blocks.GetBlkName(mJCData.vb[mJCData.vb.Count - 1]);
    Self.LV.Items[line].SubItems[3] := str;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJCTableData.AddJC(pos: Integer);
var LI: TListItem;
  j: Integer;
begin
  LI := Self.LV.Items.Insert(pos);
  LI.Caption := IntToStr(Self.LV.Items.Count);
  for j := 0 to Self.LV.Columns.Count - 2 do
    LI.SubItems.Add('');

  Self.UpdateLine(pos);
end;

procedure TMultiJCTableData.RemoveJC(index: Integer);
begin
  Self.LV.Items.Delete(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

MultiJCTableData.Free();

end.// unit
