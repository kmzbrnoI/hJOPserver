unit DataBloky;

// TBlkTableData - trida resici zobrazovani tavulky bloku

interface

uses ComCtrls, SysUtils, StrUtils, Classes;

type
  TBlocksTablePainter = class
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
  Blocks.GetBlkByIndex(line, blk);
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
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TTurnoutPosition.none:
            Self.LV.Items[line].SubItems[3] := 'none';
          TTurnoutPosition.plus:
            Self.LV.Items[line].SubItems[3] := '+';
          TTurnoutPosition.minus:
            Self.LV.Items[line].SubItems[3] := '-';
          TTurnoutPosition.both:
            Self.LV.Items[line].SubItems[3] := '+-';
        end;

        Self.LV.Items[line].SubItems[5] := (blk as TBlkTurnout).note;
        Self.LV.Items[line].SubItems[6] := (blk as TBlkTurnout).lockout;
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btTrack:
      begin
        if ((blk as TBlkTrack).spnl.trackName <> '') then
          Self.LV.Items[line].ImageIndex := 1
        else
          Self.LV.Items[line].ImageIndex := 3;

        Self.LV.Items[line].SubItems[0] := 'Úsek';

        var str := '';
        for var train in (blk as TBlkTrack).trains do
          str := str + trains.GetTrainNameByIndex(train) + ', ';
        Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str) - 2);

        case ((blk as TBlkTrack).occupied) of
          TTrackState.disabled:
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TTrackState.none:
            Self.LV.Items[line].SubItems[3] := 'none';
          TTrackState.free:
            Self.LV.Items[line].SubItems[3] := '---';
          TTrackState.occupied:
            Self.LV.Items[line].SubItems[3] := '+++';
        end; // case obsazeno

        Self.LV.Items[line].SubItems[5] := (blk as TBlkTrack).note;
        Self.LV.Items[line].SubItems[6] := (blk as TBlkTrack).lockout;

        if ((blk as TBlkTrack).trainPredict <> nil) then
          Self.LV.Items[line].SubItems[7] := (blk as TBlkTrack).trainPredict.name
        else
          Self.LV.Items[line].SubItems[7] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btIR:
      begin
        Self.LV.Items[line].ImageIndex := 4;
        Self.LV.Items[line].SubItems[0] := 'IR';

        Self.LV.Items[line].SubItems[2] := '---';

        case ((blk as TBlkIR).occupied) of
          TIROccupationState.disabled:
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TIROccupationState.none:
            Self.LV.Items[line].SubItems[3] := 'none';
          TIROccupationState.free:
            Self.LV.Items[line].SubItems[3] := '---';
          TIROccupationState.occupied:
            Self.LV.Items[line].SubItems[3] := '+++';
        end; // case

        Self.LV.Items[line].SubItems[4] := '---';
        Self.LV.Items[line].SubItems[5] := '---';
        Self.LV.Items[line].SubItems[6] := '---';
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btSignal, btGroupSignal:
      begin
        Self.LV.Items[line].ImageIndex := 5;
        var s_signal := (blk as TBlkSignal).GetSettings();
        if (glob.typ = btGroupSignal) then
          Self.LV.Items[line].SubItems[0] := 'Návěstidlo skupinové'
        else
          Self.LV.Items[line].SubItems[0] := 'Návěstidlo';

        Self.LV.Items[line].SubItems[2] := '---';

        Self.LV.Items[line].SubItems[3] := TBlkSignal.SignalToString((blk as TBlkSignal).signal);

        Self.LV.Items[line].SubItems[5] := '---';
        Self.LV.Items[line].SubItems[6] := '---';
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btCrossing:
      begin
        Self.LV.Items[line].ImageIndex := 7;
        Self.LV.Items[line].SubItems[0] := 'Přejezd';

        Self.LV.Items[line].SubItems[2] := '---';

        case ((blk as TBlkCrossing).state) of
          TBlkCrossingBasicState.disabled:
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TBlkCrossingBasicState.none:
            Self.LV.Items[line].SubItems[3] := 'none';
          TBlkCrossingBasicState.open:
            Self.LV.Items[line].SubItems[3] := 'otevřeno';
          TBlkCrossingBasicState.caution:
            Self.LV.Items[line].SubItems[3] := 'výstraha';
          TBlkCrossingBasicState.closed:
            Self.LV.Items[line].SubItems[3] := 'uzavřeno';
        end; // case obsazeno

        Self.LV.Items[line].SubItems[5] := (Blk as TBlkCrossing).note;
        Self.LV.Items[line].SubItems[6] := '';

        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btRailway:
      begin
        Self.LV.Items[line].ImageIndex := 8;
        Self.LV.Items[line].SubItems[0] := 'Trať';

        Self.LV.Items[line].SubItems[2] := '---';

        if ((blk as TBlkRailway).occupied) then
        begin
          Self.LV.Items[line].SubItems[3] := 'obsazeno';
        end else begin
          if ((blk as TBlkRailway).Zaver) then
          begin
            Self.LV.Items[line].SubItems[3] := 'závěr'
          end else begin
            if ((blk as TBlkRailway).departureForbidden) then
            begin
              Self.LV.Items[line].SubItems[3] := 'ZAK'
            end else begin

              if ((blk as TBlkRailway).request) then
                Self.LV.Items[line].SubItems[3] := 'žádost'
              else
                case ((blk as TBlkRailway).direction) of
                  TRailwayDirection.disabled:
                    Self.LV.Items[line].SubItems[3] := 'disabled';
                  TRailwayDirection.AtoB:
                    Self.LV.Items[line].SubItems[3] := 'směr A->B';
                  TRailwayDirection.BtoA:
                    Self.LV.Items[line].SubItems[3] := 'směr B->A';
                  TRailwayDirection.no:
                    Self.LV.Items[line].SubItems[3] := 'směr žádný'
                end; // case
            end;
          end;
        end;

        Self.LV.Items[line].SubItems[5] := '';
        Self.LV.Items[line].SubItems[6] := '';

        if (Assigned((blk as TBlkRailway).trainPredict)) then
          Self.LV.Items[line].SubItems[7] := (blk as TBlkRailway).trainPredict.train.name
        else
          Self.LV.Items[line].SubItems[7] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btLinker:
      begin
        Self.LV.Items[line].ImageIndex := 9;
        Self.LV.Items[line].SubItems[0] := 'Úvazka';

        Self.LV.Items[line].SubItems[2] := '---';

        if ((blk as TBlkLinker).enabled) then
          Self.LV.Items[line].SubItems[3] := 'enabled'
        else
          Self.LV.Items[line].SubItems[3] := 'disabled';

        Self.LV.Items[line].SubItems[5] := (blk as TBlkLinker).note;
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btLock:
      begin
        Self.LV.Items[line].ImageIndex := 10;
        Self.LV.Items[line].SubItems[0] := 'Zámek';

        Self.LV.Items[line].SubItems[2] := '---';

        if ((blk as TBlkLock).state.enabled) then
        begin
          if ((blk as TBlkLock).keyReleased) then
            Self.LV.Items[line].SubItems[3] := 'klíč uvolněn'
          else
            Self.LV.Items[line].SubItems[3] := 'klíč zamknut';
        end
        else
          Self.LV.Items[line].SubItems[3] := 'disabled';

        Self.LV.Items[line].SubItems[5] := (blk as TBlkLock).note;
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btDisconnector:
      begin
        Self.LV.Items[line].ImageIndex := 11;
        Self.LV.Items[line].SubItems[0] := 'Rozpojovač';

        Self.LV.Items[line].SubItems[2] := '---';

        case ((blk as TBlkDisconnector).state) of
          TBlkDiscBasicState.disabled:
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TBlkDiscBasicState.inactive:
            Self.LV.Items[line].SubItems[3] := 'ok';
          TBlkDiscBasicState.active, TBlkDiscBasicState.activeInfinite:
            Self.LV.Items[line].SubItems[3] := 'active';
        end; // case

        Self.LV.Items[line].SubItems[5] := (blk as TBlkDisconnector).note;
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btRT:
      begin
        Self.LV.Items[line].ImageIndex := 2;
        Self.LV.Items[line].SubItems[0] := 'Traťový úsek';

        var str := '';
        for var train in (blk as TBlkTrack).trains do
          str := str + trains.GetTrainNameByIndex(train) + ', ';
        Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str) - 2);

        case ((blk as TBlkTrack).occupied) of
          TTrackState.disabled:
            Self.LV.Items[line].SubItems[3] := 'disabled';
          TTrackState.none:
            Self.LV.Items[line].SubItems[3] := 'none';
          TTrackState.free:
            Self.LV.Items[line].SubItems[3] := '---';
          TTrackState.occupied:
            Self.LV.Items[line].SubItems[3] := '+++';
        end; // case obsazeno

        Self.LV.Items[line].SubItems[5] := (blk as TBlkTrack).note;
        Self.LV.Items[line].SubItems[6] := (blk as TBlkTrack).lockout;

        if ((blk as TBlkTrack).trainPredict <> nil) then
          Self.LV.Items[line].SubItems[7] := (blk as TBlkTrack).trainPredict.name
        else
          Self.LV.Items[line].SubItems[7] := '--#--';
      end;

    /// //////////////////////////////////////////////////
    btIO:
      begin
        Self.LV.Items[line].ImageIndex := 12;
        Self.LV.Items[line].SubItems[0] := 'IO';

        Self.LV.Items[line].SubItems[2] := '---';

        if (TBlkIO(Blk).enabled) then
          Self.LV.Items[line].SubItems[3] := 'I: ' + ownConvert.BoolToYesNo(TBlkIO(Blk).activeInput) + ', O: ' +
            ownConvert.BoolToYesNo(TBlkIO(blk).activeOutput)
        else
          Self.LV.Items[line].SubItems[3] := 'disabled';

        Self.LV.Items[line].SubItems[5] := (blk as TBlkIO).note;
        Self.LV.Items[line].SubItems[6] := '---';
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btSummary:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[0] := 'Součtová hláska';

        Self.LV.Items[line].SubItems[2] := '---';

        if ((blk as TBlkSummary).enabled) then
          Self.LV.Items[line].SubItems[3] := 'ok'
        else
          Self.LV.Items[line].SubItems[3] := 'disabled';

        Self.LV.Items[line].SubItems[5] := '';
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    /// //////////////////////////////////////////////////
    btAC:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[0] := 'AC';

        Self.LV.Items[line].SubItems[2] := '---';

        if (TBlkAC(blk).enabled) then
        begin
          case (TBlkAC(blk).acState) of
            TACState.stopped:
              begin
                if (TBlkAC(blk).clientConnected) then
                  Self.LV.Items[line].SubItems[3] := 'zastaven'
                else
                  Self.LV.Items[line].SubItems[3] := 'klient nepřipojen';
              end;
            TACState.running:
              Self.LV.Items[line].SubItems[3] := 'běží';
            TACState.paused:
              Self.LV.Items[line].SubItems[3] := 'pozastaven';
          end;
        end
        else
          Self.LV.Items[line].SubItems[3] := 'disabled';

        Self.LV.Items[line].SubItems[5] := '---';
        Self.LV.Items[line].SubItems[7] := '---';
      end;

    btPst:
      begin
        Self.LV.Items[line].ImageIndex := -1;
        Self.LV.Items[line].SubItems[0] := 'Pomocné stavědlo';
        Self.LV.Items[line].SubItems[2] := '---';

        case (TBlkPst(blk).status) of
          pstDisabled: Self.LV.Items[line].SubItems[3] := 'disabled';
          pstOff: Self.LV.Items[line].SubItems[3] := 'základní stav';
          pstTakeReady: Self.LV.Items[line].SubItems[3] := 'připraveno k převzetí';
          pstRefuging: Self.LV.Items[line].SubItems[3] := 'nastavování boční ochrany...';
          pstActive: Self.LV.Items[line].SubItems[3] := 'aktivní';
        end;

        Self.LV.Items[line].SubItems[5] := '---';
        Self.LV.Items[line].SubItems[7] := '---';
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
