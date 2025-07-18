unit DataJC;

// TJCTableData - trida starajici se o vyplneni tabulky jizdnich cest

interface

uses ComCtrls, SysUtils, StrUtils, Classes;

type
  TJCTableData = class
  private
    LV: TListView;

  public

    procedure LoadToTable();
    procedure UpdateTable();

    procedure UpdateLine(line: Integer);

    procedure AddJC(index: Integer);
    procedure RemoveJC(index: Integer);
    procedure MoveJC(source, target: Integer);

    constructor Create(LV: TListView);
  end;

var
  JCTableData: TJCTableData;

implementation

uses BlockTurnout, TJCDatabase, TechnologieJC, Block, BlockDb, fMain, ownConvert, TrainSpeed;

/// /////////////////////////////////////////////////////////////////////////////

constructor TJCTableData.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.LoadToTable();
var i, j: Integer;
  LI: TListItem;
begin
  F_Main.E_dataload_JC.text := JCDb.filename;
  Self.LV.Clear();

  for i := 0 to JCDb.Count - 1 do
  begin
    LI := Self.LV.Items.Add;
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count - 2 do
      LI.SubItems.Add('');

    try
      Self.UpdateLine(i);
    except

    end;
  end; // for i

end;

procedure TJCTableData.UpdateTable();
var i: Integer;
begin
  for i := 0 to JCDb.Count - 1 do
    if (JCDb.GetJCByIndex(i).changed) then
    begin
      try
        Self.UpdateLine(i);
        Self.LV.UpdateItems(i, i);
      except

      end;
    end;
end;

procedure TJCTableData.UpdateLine(line: Integer);
var JCData: TJCdata;
  JC: TJC;
begin
  JC := JCDb.GetJCByIndex(line);
  JCData := JC.data;
  JC.changed := false;

  Self.LV.Items[line].Caption := IntToStr(JCData.id);
  Self.LV.Items[line].SubItems[0] := JCData.name;
  Self.LV.Items[line].SubItems[4] := Blocks.GetBlkName(JCData.signalId);

  // stav:
  Self.LV.Items[line].SubItems[1] := IntToStr(JC.state.destroyBlock);
  Self.LV.Items[line].SubItems[2] := IntToStr(JC.state.destroyEndBlock);
  Self.LV.Items[line].SubItems[3] := IntToStr(Integer(JC.state.step));

  case (JCData.typ) of
    TJCType.train:
      Self.LV.Items[line].SubItems[7] := 'VC';
    TJCType.shunt:
      Self.LV.Items[line].SubItems[7] := 'PC';
    TJCType.emergency:
      Self.LV.Items[line].SubItems[7] := 'NC';
  end;
  if (JCData.emOnly) then
    Self.LV.Items[line].SubItems[7] := Self.LV.Items[line].SubItems[7] + ' (NC)';

  if (JCData.nextSignalType = TJCNextSignalType.no) then
  begin
    Self.LV.Items[line].SubItems[8] := '��dn�';
  end else begin
    if (JCData.nextSignalType = TJCNextSignalType.railway) then
    begin
      Self.LV.Items[line].SubItems[8] := 'tra�';
    end else begin
      Self.LV.Items[line].SubItems[8] := Blocks.GetBlkName(JCData.nextSignalId);
    end; // else DalsiNNavaznostTyp = 1
  end; // else DalsiNNavaznostTyp = 0

  // vyhybky
  begin
    var str: string := '';
    for var j := 0 to JCData.turnouts.Count - 1 do
    begin
      case (JCData.turnouts[j].position) of
        TTurnoutPosition.plus:
          str := str + '(' + Blocks.GetBlkName(JCData.turnouts[j].Block) + ', +)';
        TTurnoutPosition.minus:
          str := str + '(' + Blocks.GetBlkName(JCData.turnouts[j].Block) + ', -)';
      end;
    end; // for j
    Self.LV.Items[line].SubItems[5] := str;
  end;

  // useky
  begin
    var str: string := '';
    for var j := 0 to JCData.tracks.Count - 1 do
      str := str + Blocks.GetBlkName(JCData.tracks[j]) + '; ';
    Self.LV.Items[line].SubItems[6] := LeftStr(str, Length(str) - 2);

    Self.LV.Items[line].SubItems[9] := TTrainSpeed.DefaultSpeedStr(JCData.speedsGo);
    Self.LV.Items[line].SubItems[10] := TTrainSpeed.DefaultSpeedStr(JCData.speedsStop);
  end;

  // odvraty
  begin
    var str: string := '';
    for var j := 0 to JCData.refuges.Count - 1 do
    begin
      case (JCData.refuges[j].position) of
        TTurnoutPosition.plus:
          str := str + '(' + Blocks.GetBlkName(JCData.refuges[j].Block) + ', +, ' +
            Blocks.GetBlkName(JCData.refuges[j].ref_blk) + ')';
        TTurnoutPosition.minus:
          str := str + '(' + Blocks.GetBlkName(JCData.refuges[j].Block) + ', -, ' +
            Blocks.GetBlkName(JCData.refuges[j].ref_blk) + ')';
      end;
    end; // for j
    Self.LV.Items[line].SubItems[11] := str;
  end;

  if (JCData.railwayId > -1) then
    Self.LV.Items[line].SubItems[12] := Blocks.GetBlkName(JCData.railwayId)
  else
    Self.LV.Items[line].SubItems[12] := '';

  // prejezdy
  begin
    var str: string := '';
    for var j := 0 to JCData.crossings.Count - 1 do
      str := str + Blocks.GetBlkName(JCData.crossings[j].crossingId) + '; ';
    Self.LV.Items[line].SubItems[13] := LeftStr(str, Length(str) - 2);
  end;

  // podminky zamky
  begin
    var str: string := '';
    for var j := 0 to JCData.locks.Count - 1 do
      str := str + '(' + Blocks.GetBlkName(JCData.locks[j].Block) + ' : ' +
        Blocks.GetBlkName(JCData.locks[j].ref_blk) + ')';
    Self.LV.Items[line].SubItems[14] := str;
  end;

  // neprofilove useky
  begin
    var str: string := '';
    for var j := 0 to JCData.turnouts.Count - 1 do
    begin
      var blk: TBlkTurnout := Blocks.GetBlkTurnoutByID(JCData.turnouts[j].Block);
      if (blk <> nil) then
      begin
        if ((JCData.turnouts[j].position = TTurnoutPosition.plus) and (blk.npBlokPlus <> nil)) then
          str := str + blk.npBlokPlus.name + ', '
        else if ((JCData.turnouts[j].position = TTurnoutPosition.minus) and (blk.npBlokMinus <> nil)) then
          str := str + blk.npBlokMinus.name + ', ';
      end
      else
        str := str + '?, ';
    end;
    Self.LV.Items[line].SubItems[15] := LeftStr(str, Length(str) - 2);
  end;

  Self.LV.Items[line].SubItems[16] := ownConvert.BoolToTick(JCData.nzv);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.AddJC(index: Integer);
var LI: TListItem;
  j: Integer;
begin
  LI := Self.LV.Items.Insert(index);
  LI.Caption := IntToStr(Self.LV.Items.Count);
  for j := 0 to Self.LV.Columns.Count - 2 do
    LI.SubItems.Add('');

  Self.UpdateLine(index);
end;

procedure TJCTableData.RemoveJC(index: Integer);
begin
  Self.LV.Items.Delete(index);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TJCTableData.MoveJC(source, target: Integer);
var LI: TListItem;
  i: Integer;
begin
  Self.LV.Items.Delete(source);
  LI := Self.LV.Items.Insert(target);
  for i := 0 to Self.LV.Columns.Count - 2 do
    LI.SubItems.Add('');
  Self.UpdateLine(target);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

JCTableData.Free();

end.// unit
