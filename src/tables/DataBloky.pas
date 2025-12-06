unit DataBloky;

// TBlkTableData - trida resici zobrazovani tavulky bloku

interface

uses ComCtrls, SysUtils, StrUtils, Classes;

type
  TBlocksTablePainter = class
  private const
    _LVI_TYPE = 0;
    _LVI_ID = 1;
    _LVI_TRAIN = 2;
    _LVI_STATE = 3;
    _LVI_AREA = 4;
    _LVI_NOTE = 5;
    _LVI_LOCKOUT = 6;
    _LVI_TRAIN_PREDICT = 7;

  private
    LV: TListView;

    changed: array of Boolean;

  public

    reload: Boolean;

    procedure LoadTable();
    procedure UpdateTable();
    procedure UpdateLine(line: Integer);
    procedure BlkChange(line: Integer);

    procedure BlkRemove(line: Integer);
    procedure BlkAdd(index: Integer);

    procedure BlkMove(source, target: Integer);

    constructor Create(LV: TListView);
  end;

var
  BlocksTablePainter: TBlocksTablePainter;

implementation

uses BlockDb, Block, BlockTurnout, BlockTrack, BlockSignal, BlockIR, BlockCrossing,
  fMain, BlockRailway, BlockLinker, TrainDb, BlockLock, BlockDisconnector, BlockIO,
  BlockSummary, BlockAC, ownConvert, BlockPst;

/// /////////////////////////////////////////////////////////////////////////////

constructor TBlocksTablePainter.Create(LV: TListView);
begin
  inherited Create();
  Self.LV := LV;
  SetLength(Self.changed, Blocks.count);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocksTablePainter.LoadTable();
begin
  Self.LV.Clear();

  for var i := 0 to Blocks.count - 1 do
  begin
    var LI: TListItem := Self.LV.Items.Add;
    LI.Caption := '---';
    for var j := 0 to Self.LV.Columns.count - 2 do
      LI.SubItems.Add('---');
  end;

  Self.reload := true;
  Self.UpdateTable();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocksTablePainter.UpdateTable();
begin
  F_Main.L_BlkPocet.Caption := 'Celkem ' + IntToStr(Blocks.count) + ' bloků';

  for var i := 0 to Blocks.count - 1 do
    if ((Self.changed[i]) or (Self.reload)) then
    begin
      Self.UpdateLine(i);
      Self.changed[i] := false;
      Self.LV.UpdateItems(i, i);
    end;

  Self.reload := false;
end;

procedure TBlocksTablePainter.UpdateLine(line: Integer);
var blk: TBlk;
    glob: TBlkSettings;
begin
  blk := Blocks.GetBlkByIndex(line);
  if (blk = nil) then
    Exit();

  glob := blk.GetGlobalSettings();

  Self.LV.Items[line].Caption := glob.name;
  Self.LV.Items[line].SubItems.Strings[1] := IntToStr(glob.id);

  begin
    var str := '';
    if (blk.areas.count > 1) then
      for var j := 0 to blk.areas.count - 2 do
        str := str + blk.areas[j].name + ', ';
    if (blk.areas.count > 0) then
      str := str + blk.areas[blk.areas.count - 1].name;
    Self.LV.Items[line].SubItems.Strings[4] := str;
  end;

  case (glob.typ) of
    btTurnout:
      begin
        Self.LV.Items[line].ImageIndex := 0;
        var s_vyh := (blk as TBlkTurnout).GetSettings();
        Self.LV.Items[line].SubItems.Strings[0] := 'Výhybka';

        case ((blk as TBlkTurnout).position) of
          TTurnoutPosition.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TTurnoutPosition.none:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'none';
          TTurnoutPosition.plus:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '+';
          TTurnoutPosition.minus:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '-';
          TTurnoutPosition.both:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '+-';
        end;

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkTurnout).note;
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := (blk as TBlkTurnout).lockout;
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btTrack:
      begin
        if ((blk as TBlkTrack).spnl.trackName <> '') then
          Self.LV.Items[line].ImageIndex := 1
        else
          Self.LV.Items[line].ImageIndex := 3;

        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Úsek';

        var str := '';
        for var train in (blk as TBlkTrack).trains do
          str := str + trains.GetTrainNameByIndex(train) + ', ';
        Self.LV.Items[line].SubItems[_LVI_TRAIN] := LeftStr(str, Length(str) - 2);

        case ((blk as TBlkTrack).occupied) of
          TTrackState.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TTrackState.none:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'none';
          TTrackState.free:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '---';
          TTrackState.occupied:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '+++';
        end; // case obsazeno

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkTrack).note;
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := (blk as TBlkTrack).lockout;

        if ((blk as TBlkTrack).trainPredict <> nil) then
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := (blk as TBlkTrack).trainPredict.name
        else
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btIR:
      begin
        Self.LV.Items[line].ImageIndex := 4;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'IR';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        case ((blk as TBlkIR).occupied) of
          TIROccupationState.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TIROccupationState.none:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'none';
          TIROccupationState.free:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '---';
          TIROccupationState.occupied:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '+++';
        end; // case

        Self.LV.Items[line].SubItems[_LVI_AREA] := '---';
        Self.LV.Items[line].SubItems[_LVI_NOTE] := '---';
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := '---';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btSignal, btGroupSignal:
      begin
        Self.LV.Items[line].ImageIndex := 5;
        var s_signal := (blk as TBlkSignal).GetSettings();
        if (glob.typ = btGroupSignal) then
          Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Návěstidlo skupinové'
        else
          Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Návěstidlo';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        Self.LV.Items[line].SubItems[_LVI_STATE] := TBlkSignal.SignalToString((blk as TBlkSignal).signal);

        Self.LV.Items[line].SubItems[_LVI_NOTE] := '---';
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := '---';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btCrossing:
      begin
        Self.LV.Items[line].ImageIndex := 7;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Přejezd';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        case ((blk as TBlkCrossing).state) of
          TBlkCrossingBasicState.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TBlkCrossingBasicState.unknown:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'unknown';
          TBlkCrossingBasicState.error:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'error';
          TBlkCrossingBasicState.open:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'otevřen';
          TBlkCrossingBasicState.caution:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'výstraha';
          TBlkCrossingBasicState.closed:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'uzavřen';
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := '???';
        end;

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (Blk as TBlkCrossing).note;
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := '';

        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btRailway:
      begin
        Self.LV.Items[line].ImageIndex := 8;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Trať';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if ((blk as TBlkRailway).occupied) then
        begin
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'obsazeno';
        end else begin
          if ((blk as TBlkRailway).Zaver) then
          begin
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'závěr'
          end else begin
            if ((blk as TBlkRailway).departureForbidden) then
            begin
              Self.LV.Items[line].SubItems[_LVI_STATE] := 'ZAK'
            end else begin

              if ((blk as TBlkRailway).request) then
                Self.LV.Items[line].SubItems[_LVI_STATE] := 'žádost'
              else
                case ((blk as TBlkRailway).direction) of
                  TRailwayDirection.disabled:
                    Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
                  TRailwayDirection.AtoB:
                    Self.LV.Items[line].SubItems[_LVI_STATE] := 'směr A->B';
                  TRailwayDirection.BtoA:
                    Self.LV.Items[line].SubItems[_LVI_STATE] := 'směr B->A';
                  TRailwayDirection.no:
                    Self.LV.Items[line].SubItems[_LVI_STATE] := 'směr žádný'
                end; // case
            end;
          end;
        end;

        Self.LV.Items[line].SubItems[_LVI_NOTE] := '';
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := '';

        if (Assigned((blk as TBlkRailway).trainPredict)) then
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := (blk as TBlkRailway).trainPredict.train.name
        else
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btLinker:
      begin
        Self.LV.Items[line].ImageIndex := 9;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Úvazka';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if ((blk as TBlkLinker).enabled) then
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'enabled'
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkLinker).note;
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btLock:
      begin
        Self.LV.Items[line].ImageIndex := 10;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Zámek';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if ((blk as TBlkLock).state.enabled) then
        begin
          if ((blk as TBlkLock).keyReleased) then
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'klíč uvolněn'
          else
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'klíč zamknut';
        end
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkLock).note;
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btDisconnector:
      begin
        Self.LV.Items[line].ImageIndex := 11;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Rozpojovač';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        case ((blk as TBlkDisconnector).state) of
          TBlkDiscBasicState.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TBlkDiscBasicState.inactive:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'ok';
          TBlkDiscBasicState.active, TBlkDiscBasicState.activeInfinite:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'active';
        end; // case

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkDisconnector).note;
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btRT:
      begin
        Self.LV.Items[line].ImageIndex := 2;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Traťový úsek';

        var str := '';
        for var train in (blk as TBlkTrack).trains do
          str := str + trains.GetTrainNameByIndex(train) + ', ';
        Self.LV.Items[line].SubItems[_LVI_TRAIN] := LeftStr(str, Length(str) - 2);

        case ((blk as TBlkTrack).occupied) of
          TTrackState.disabled:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          TTrackState.none:
            Self.LV.Items[line].SubItems[_LVI_STATE] := 'none';
          TTrackState.free:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '---';
          TTrackState.occupied:
            Self.LV.Items[line].SubItems[_LVI_STATE] := '+++';
        end; // case obsazeno

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkTrack).note;
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := (blk as TBlkTrack).lockout;

        if ((blk as TBlkTrack).trainPredict <> nil) then
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := (blk as TBlkTrack).trainPredict.name
        else
          Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btIO:
      begin
        Self.LV.Items[line].ImageIndex := 12;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'IO';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if (TBlkIO(Blk).enabled) then
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'I: ' + ownConvert.BoolToYesNo(TBlkIO(Blk).activeInput) + ', O: ' +
            ownConvert.BoolToYesNo(TBlkIO(blk).activeOutput)
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';

        Self.LV.Items[line].SubItems[_LVI_NOTE] := (blk as TBlkIO).note;
        Self.LV.Items[line].SubItems[_LVI_LOCKOUT] := '---';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btSummary:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Součtová hláska';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if ((blk as TBlkSummary).enabled) then
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'ok'
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';

        Self.LV.Items[line].SubItems[_LVI_NOTE] := '';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    /// //////////////////////////////////////////////////
    btAC:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'AC';

        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        if (TBlkAC(blk).enabled) then
        begin
          case (TBlkAC(blk).acState) of
            TACState.stopped:
              begin
                if (TBlkAC(blk).clientConnected) then
                  Self.LV.Items[line].SubItems[_LVI_STATE] := 'zastaven'
                else
                  Self.LV.Items[line].SubItems[_LVI_STATE] := 'klient nepřipojen';
              end;
            TACState.running:
              Self.LV.Items[line].SubItems[_LVI_STATE] := 'běží';
            TACState.paused:
              Self.LV.Items[line].SubItems[_LVI_STATE] := 'pozastaven';
          end;
        end
        else
          Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';

        Self.LV.Items[line].SubItems[_LVI_NOTE] := '---';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

    btPst:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[_LVI_TYPE] := 'Pomocné stavědlo';
        Self.LV.Items[line].SubItems[_LVI_TRAIN] := '---';

        case (TBlkPst(blk).status) of
          pstDisabled: Self.LV.Items[line].SubItems[_LVI_STATE] := 'disabled';
          pstOff: Self.LV.Items[line].SubItems[_LVI_STATE] := 'základní stav';
          pstTakeReady: Self.LV.Items[line].SubItems[_LVI_STATE] := 'připraveno k převzetí';
          pstRefuging: Self.LV.Items[line].SubItems[_LVI_STATE] := 'nastavování boční ochrany...';
          pstActive: Self.LV.Items[line].SubItems[_LVI_STATE] := 'aktivní';
        end;

        Self.LV.Items[line].SubItems[_LVI_NOTE] := '---';
        Self.LV.Items[line].SubItems[_LVI_TRAIN_PREDICT] := '---';
      end;

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocksTablePainter.BlkChange(line: Integer);
begin
  if ((line > -1) and (line < Blocks.count)) then
    Self.changed[line] := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocksTablePainter.BlkRemove(line: Integer);
begin
  Self.LV.Items.Delete(line);
  Self.reload := true;
  Self.UpdateTable();
end;

procedure TBlocksTablePainter.BlkAdd(index: Integer);
begin
  SetLength(changed, Length(changed) + 1);

  var LI: TListItem := Self.LV.Items.Insert(index);
  LI.Caption := '---';
  for var j := 0 to Self.LV.Columns.count - 2 do
    LI.SubItems.Add('---');
  Self.UpdateLine(index);

  F_Main.L_BlkPocet.Caption := 'Pocet bloku : ' + IntToStr(Blocks.count);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlocksTablePainter.BlkMove(source, target: Integer);
begin
  Self.LV.Items.Delete(source);
  var LI: TListItem := Self.LV.Items.Insert(target);
  for var i := 0 to Self.LV.Columns.count - 2 do
    LI.SubItems.Add('');
  Self.UpdateLine(target);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

BlocksTablePainter.Free();

end.
