unit DataBloky;

// TBlkTableData - trida resici zobrazovani tavulky bloku

interface

uses ComCtrls, SysUtils, StrUtils;

type
  TBlokyTableData=class
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
  BlokyTableData : TBlokyTableData;

implementation

uses BlockDb, Block, BlockTurnout, BlockTrack, BlockSignal, BlockIR, BlockCrossing,
      fMain, BlockRailway, BlockLinker, TrainDb, BlockLock, BlockDisconnector, BlockIO,
      BlockSummary, BlockAC, ownConvert;

////////////////////////////////////////////////////////////////////////////////

constructor TBlokyTableData.Create(LV: TListView);
begin
 inherited Create();
 Self.LV := LV;
 SetLength(Self.changed, Blocks.count);
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.LoadTable();
var i, j : Integer;
    LI: TListItem;
begin
 Self.LV.Clear();

 for i := 0 to Blocks.count-1 do
  begin
   LI := Self.LV.Items.Add;
   LI.Caption := '---';
   for j := 0 to Self.LV.Columns.Count-2 do
     LI.SubItems.Add('---');
  end;//for i

 Self.reload := true;
 Self.UpdateTable();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.UpdateTable();
var i: Integer;
begin
  F_Main.L_BlkPocet.Caption := 'Celkem '+IntToStr(Blocks.count)+' bloků';

  for i := 0 to Blocks.count-1 do
   if ((Self.changed[i]) or (Self.reload)) then
    begin
     Self.UpdateLine(i);
     Self.changed[i] := false;
     Self.LV.UpdateItems(i, i);
    end;

  Self.reload := false;
end;//procedyre

procedure TBlokyTableData.UpdateLine(line: Integer);
var j, train: integer;
    Blk: TBlk;
    glob: TBlkSettings;
    s_vyh: TBlkTurnoutSettings;
    s_signal: TBlkSignalSettings;
    str: string;
 begin
  Blocks.GetBlkByIndex(line, Blk);
  if (Blk = nil) then Exit();

  glob := Blk.GetGlobalSettings();

  Self.LV.Items[line].Caption := glob.name;
  Self.LV.Items[line].SubItems.Strings[1] := IntToStr(glob.id);

  str := '';
  if (Blk.stations.Count > 1) then
    for j := 0 to Blk.stations.Count-2 do str := str + Blk.stations[j].Name+', ';
  if (Blk.stations.Count > 0) then
    str := str + Blk.stations[Blk.stations.Count-1].Name;
  Self.LV.Items[line].SubItems.Strings[4] := str;

  case (glob.typ) of
   btTurnout: begin
      Self.LV.Items[line].ImageIndex := 0;
      s_vyh := (Blk as TBlkTurnout).GetSettings();
      Self.LV.Items[line].SubItems.Strings[0] := 'Výhybka';

      case ((Blk as TBlkTurnout).position) of
       TTurnoutPosition.disabled: Self.LV.Items[line].SubItems[3] := 'disabled';
       TTurnoutPosition.none    : Self.LV.Items[line].SubItems[3] := 'none';
       TTurnoutPosition.plus    : Self.LV.Items[line].SubItems[3] := '+';
       TTurnoutPosition.minus   : Self.LV.Items[line].SubItems[3] := '-';
       TTurnoutPosition.both    : Self.LV.Items[line].SubItems[3] := '+-';
      end;//case poloha

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkTurnout).note;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkTurnout).lockout;
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btTrack: begin
      if ((Blk as TBlkTrack).spnl.trackName <> '') then
        Self.LV.Items[line].ImageIndex := 1
      else
        Self.LV.Items[line].ImageIndex := 3;

      Self.LV.Items[line].SubItems[0] := 'Úsek';

      str := '';
      for train in (Blk as TBlkTrack).trains do
        str := str + Trains.GetTrainNameByIndex(train) + ', ';
      Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2);

      case ((Blk as TBlkTrack).occupied) of
        TTrackState.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TTrackState.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TTrackState.free : Self.LV.Items[line].SubItems[3] := '---';
        TTrackState.occupied : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkTrack).note;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkTrack).lockout;

      if ((Blk as TBlkTrack).trainPredict <> nil) then Self.LV.Items[line].SubItems[7] := (Blk as TBlkTrack).trainPredict.name else
        Self.LV.Items[line].SubItems[7] := '--#--';
   end;

 /////////////////////////////////////////////////////
   btIR: begin
      Self.LV.Items[line].ImageIndex := 4;
      Self.LV.Items[line].SubItems[0] := 'IR';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkIR).occupied) of
        TIROccupationState.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TIROccupationState.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TIROccupationState.free     : Self.LV.Items[line].SubItems[3] := '---';
        TIROccupationState.occupied : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case

      Self.LV.Items[line].SubItems[4] := '---';
      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btSignal: begin
      Self.LV.Items[line].ImageIndex := 5;
      s_signal := (Blk as TBlkSignal).GetSettings();
      Self.LV.Items[line].SubItems[0] := 'Návěstidlo';

      Self.LV.Items[line].SubItems[2] := '---';

      Self.LV.Items[line].SubItems[3] := TBlkSignal.SignalToString((Blk as TBlkSignal).signal);

      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btCrossing: begin
      Self.LV.Items[line].ImageIndex := 7;
      Self.LV.Items[line].SubItems[0] := 'Přejezd';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkCrossing).state) of
        TBlkCrossingBasicState.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TBlkCrossingBasicState.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TBlkCrossingBasicState.open     : Self.LV.Items[line].SubItems[3] := 'otevřeno';
        TBlkCrossingBasicState.caution  : Self.LV.Items[line].SubItems[3] := 'výstraha';
        TBlkCrossingBasicState.closed   : Self.LV.Items[line].SubItems[3] := 'uzavřeno';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkCrossing).note;
      Self.LV.Items[line].SubItems[6] := '';

      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btRailway: begin
      Self.LV.Items[line].ImageIndex := 8;
      Self.LV.Items[line].SubItems[0] := 'Trať';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkRailway).occupied) then begin
        Self.LV.Items[line].SubItems[3] := 'obsazeno';
      end else begin
        if ((Blk as TBlkRailway).Zaver) then begin
          Self.LV.Items[line].SubItems[3] := 'závěr'
        end else begin
          if ((Blk as TBlkRailway).departureForbidden) then begin
            Self.LV.Items[line].SubItems[3] := 'ZAK'
          end else begin

           if ((Blk as TBlkRailway).request) then
            Self.LV.Items[line].SubItems[3] := 'žádost'
           else
            case ((Blk as TBlkRailway).direction) of
             TRailwayDirection.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
             TRailwayDirection.AtoB     : Self.LV.Items[line].SubItems[3] := 'směr A->B';
             TRailwayDirection.BtoA     : Self.LV.Items[line].SubItems[3] := 'směr B->A';
             TRailwayDirection.no    : Self.LV.Items[line].SubItems[3] := 'směr žádný'
            end;//case
          end;
        end;
      end;

    Self.LV.Items[line].SubItems[5] := '';
    Self.LV.Items[line].SubItems[6] := '';

    if (Assigned((Blk as TBlkRailway).trainPredict)) then
      Self.LV.Items[line].SubItems[7] := (Blk as TBlkRailway).trainPredict.train.name
    else
      Self.LV.Items[line].SubItems[7] := '--#--';
   end;

 /////////////////////////////////////////////////////
   btLinker: begin
      Self.LV.Items[line].ImageIndex := 9;
      Self.LV.Items[line].SubItems[0] := 'Úvazka';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkLinker).enabled) then
        Self.LV.Items[line].SubItems[3] := 'enabled'
      else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkLinker).note;
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btLock: begin
      Self.LV.Items[line].ImageIndex := 10;
      Self.LV.Items[line].SubItems[0] := 'Zámek';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkLock).state.enabled) then
       begin
        if ((Blk as TBlkLock).keyReleased) then
          Self.LV.Items[line].SubItems[3] := 'klíč uvolněn'
        else
          Self.LV.Items[line].SubItems[3] := 'klíč zamknut';
       end else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkLock).note;
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btDisconnector: begin
      Self.LV.Items[line].ImageIndex := 11;
      Self.LV.Items[line].SubItems[0] := 'Rozpojovač';

      Self.LV.Items[line].SubItems[2] := '---';

      case ((Blk as TBlkDisconnector).state) of
        TBlkDiscBasicState.disabled     : Self.LV.Items[line].SubItems[3] := 'disabled';
        TBlkDiscBasicState.not_selected : Self.LV.Items[line].SubItems[3] := 'ok';
        TBlkDiscBasicState.mounting     : Self.LV.Items[line].SubItems[3] := 'mounting';
        TBlkDiscBasicState.active       : Self.LV.Items[line].SubItems[3] := 'active';
      end;//case

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkDisconnector).note;
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btRT: begin
      Self.LV.Items[line].ImageIndex := 2;
      Self.LV.Items[line].SubItems[0] := 'Traťový úsek';

      str := '';
      for train in (Blk as TBlkTrack).trains do
        str := str + Trains.GetTrainNameByIndex(train) + ', ';
      Self.LV.Items[line].SubItems[2] := LeftStr(str, Length(str)-2);

      case ((Blk as TBlkTrack).occupied) of
        TTrackState.disabled : Self.LV.Items[line].SubItems[3] := 'disabled';
        TTrackState.none     : Self.LV.Items[line].SubItems[3] := 'none';
        TTrackState.free : Self.LV.Items[line].SubItems[3] := '---';
        TTrackState.occupied : Self.LV.Items[line].SubItems[3] := '+++';
      end;//case obsazeno

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkTrack).note;
      Self.LV.Items[line].SubItems[6] := (Blk as TBlkTrack).lockout;

      if ((Blk as TBlkTrack).trainPredict <> nil) then Self.LV.Items[line].SubItems[7] := (Blk as TBlkTrack).trainPredict.name else
        Self.LV.Items[line].SubItems[7] := '--#--';
   end;

 /////////////////////////////////////////////////////
   btIO: begin
      Self.LV.Items[line].ImageIndex := 12;
      Self.LV.Items[line].SubItems[0] := 'IO';

      Self.LV.Items[line].SubItems[2] := '---';

      if (TBlkIO(Blk).enabled) then
        Self.LV.Items[line].SubItems[3] := 'I: ' + ownConvert.BoolToYesNo(TBlkIO(Blk).activeInput) +
                                           ', O: ' + ownConvert.BoolToYesNo(TBlkIO(Blk).activeOutput)
      else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := (Blk as TBlkIO).note;
      Self.LV.Items[line].SubItems[6] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btSummary: begin
      Self.LV.Items[line].ImageIndex := -1;
      Self.LV.Items[line].SubItems[0] := 'Součtová hláska';

      Self.LV.Items[line].SubItems[2] := '---';

      if ((Blk as TBlkSummary).enabled) then
        Self.LV.Items[line].SubItems[3] := 'ok'
      else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := '';
      Self.LV.Items[line].SubItems[7] := '---';
   end;

 /////////////////////////////////////////////////////
   btAC: begin
      Self.LV.Items[line].ImageIndex := -1;
      Self.LV.Items[line].SubItems[0] := 'AC';

      Self.LV.Items[line].SubItems[2] := '---';

      if (TBlkAC(Blk).enabled) then
       begin
        case (TBlkAC(Blk).acState) of
          TACState.stopped: begin
            if (TBlkAC(Blk).clientConnected) then
              Self.LV.Items[line].SubItems[3] := 'zastaven'
            else
              Self.LV.Items[line].SubItems[3] := 'klient nepřipojen';
          end;
          TACState.running: Self.LV.Items[line].SubItems[3] := 'běží';
          TACState.paused: Self.LV.Items[line].SubItems[3] := 'pozastaven';
        end;
       end else
        Self.LV.Items[line].SubItems[3] := 'disabled';

      Self.LV.Items[line].SubItems[5] := '---';
      Self.LV.Items[line].SubItems[7] := '---';
   end;

  end;//case BLOK_VYSTUP

 end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkChange(line: Integer);
begin
 if ((line > -1) and (line < Blocks.count)) then
  Self.changed[line] := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkRemove(line: Integer);
begin
 Self.LV.Items.Delete(line);
 Self.reload := true;
 Self.UpdateTable();
end;

procedure TBlokyTableData.BlkAdd(index: Integer);
var LI: TListItem;
    j: Integer;
begin
 SetLength(changed, Length(changed)+1);

 LI := Self.LV.Items.Insert(index);
 LI.Caption := '---';
 for j := 0 to Self.LV.Columns.Count-2 do
   LI.SubItems.Add('---');
 Self.UpdateLine(index);

 F_Main.L_BlkPocet.Caption := 'Pocet bloku : '+IntToStr(Blocks.count);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlokyTableData.BlkMove(source, target: Integer);
var LI: TListItem;
    i: Integer;
begin
 Self.LV.Items.Delete(source);
 LI := Self.LV.Items.Insert(target);
 for i := 0 to Self.LV.Columns.Count-2 do
  LI.SubItems.Add('');
 Self.UpdateLine(target);
end;

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 BlokyTableData.Free();

end.//unit
