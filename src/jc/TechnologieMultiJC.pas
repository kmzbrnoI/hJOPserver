﻿unit TechnologieMultiJC;

{
  Technologie slozenych jizdnich cest.
}

interface

uses
  IniFiles, TechnologieJC, Generics.Collections, BlockDb, IdContext, SysUtils,
  Classes, Generics.Defaults, Math, Block, BlockSignal;

type

  TMultiJCState = record
    JCIndex: Integer; // index v 'activatingJCs'
    SenderOR: TObject;
    SenderPnl: TIdContext;
    activatingJCs: TList<Integer>; // JCs in stack=VZ are activated all at once at the beginning, here is the rest
    abAfter: Boolean;
  end;

  TMultiJCData = record
    name: string;
    JCs: TList<Integer>;
    vb: TList<Integer>;
    id: Integer;
  end;

  EInvalidID = class(Exception);

  TMultiJC = class

  private const
    _def_mutiJC_staveni: TMultiJCState = (JCIndex: - 1; SenderOR: nil; SenderPnl: nil; activatingJCs: nil;);

  private
    m_data: TMultiJCData;
    m_state: TMultiJCState;
    activatingJC: TJC; // zde je ulozena JC, ktera se aktualne stavi

    function IsActivating(): Boolean;

  public

    changed: Boolean;

    constructor Create(); overload;
    constructor Create(data: TMultiJCData); overload;
    destructor Destroy(); override;

    procedure UpdateActivating();

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile);

    procedure Activate(SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
    procedure CancelActivation();

    function Match(blocks: TList<TBlk>): Boolean;
    function StartSignal(): TBlkSignal;

    property data: TMultiJCData read m_data write m_data;
    property state: TMultiJCState read m_state;
    property name: string read m_data.name;
    property activating: Boolean read IsActivating;
    property id: Integer read m_data.id;

    class function IdComparer(): IComparer<TMultiJC>;
  end;

implementation

uses TJCDatabase, BlockTrack, Area, ownConvert, TCPAreasRef, AreaStack;

/// /////////////////////////////////////////////////////////////////////////////

constructor TMultiJC.Create();
begin
  inherited Create();

  Self.changed := true;
  Self.m_state := _def_mutiJC_staveni;

  Self.m_data.JCs := TList<Integer>.Create();
  Self.m_data.vb := TList<Integer>.Create();
  Self.m_state.activatingJCs := TList<Integer>.Create();

  Self.activatingJC := nil;
end;

constructor TMultiJC.Create(data: TMultiJCData);
begin
  inherited Create();

  Self.changed := true;
  Self.m_state := _def_mutiJC_staveni;

  Self.m_data := data;

  if (not Assigned(data.JCs)) then
    Self.m_data.JCs := TList<Integer>.Create();
  if (not Assigned(data.vb)) then
    Self.m_data.vb := TList<Integer>.Create();

  Self.m_state.activatingJCs := TList<Integer>.Create();
end;

destructor TMultiJC.Destroy();
begin
  if (Assigned(Self.m_data.JCs)) then
    FreeAndNil(Self.m_data.JCs);
  if (Assigned(Self.m_data.vb)) then
    FreeAndNil(Self.m_data.vb);
  if (Assigned(Self.m_state.activatingJCs)) then
    Self.m_state.activatingJCs.Free();

  inherited Destroy();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.LoadData(ini: TMemIniFile; section: string);
begin
  try
    Self.m_data.id := StrToInt(section);
  except
    on E: EConvertError do
      raise EInvalidID.Create('Neplatné id mJC : ' + section);
  end;

  Self.m_data.name := ini.ReadString(section, 'nazev', section);

  Self.m_data.JCs.Clear();
  Self.m_data.vb.Clear();

  // nacteni jizdnich cest ve slozene jizdni ceste:
  begin
    var sl: TStrings := TStringList.Create();
    try
      ExtractStrings([';', ',', '|', '-', '('], [')'], PChar(ini.ReadString(section, 'JCs', '')), sl);
      for var i: Integer := 0 to sl.Count - 1 do
        Self.m_data.JCs.Add(StrToInt(sl[i]));
    finally
      sl.Free();
    end;
  end;

  // nacteni variantnich bodu
  begin
    var sl: TStrings := TStringList.Create();
    try
      ExtractStrings([';', ',', '|', '-', '(', ')'], [], PChar(ini.ReadString(section, 'vb', '')), sl);
      for var i: Integer := 0 to sl.Count - 1 do
        Self.m_data.vb.Add(StrToInt(sl[i]));
    finally
      sl.Free();
    end;
  end;
end;

procedure TMultiJC.SaveData(ini: TMemIniFile);
var section: string;
begin
  section := IntToStr(Self.id);

  ini.WriteString(section, 'nazev', Self.m_data.name);

  begin
    var str: string := SerializeIntList(Self.data.JCs);
    if (str <> '') then
      ini.WriteString(section, 'JCs', str);
  end;

  begin
    var str: string := SerializeIntList(Self.data.vb);
    if (str <> '') then
      ini.WriteString(section, 'vb', str);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.UpdateActivating();
begin
  if (Self.activatingJC.active) then
  begin
    // aktualni cesta postavena
    Inc(Self.m_state.JCIndex);

    if (Self.m_state.JCIndex >= Self.m_state.activatingJCs.Count) then
    begin
      // vsechny cesty postaveny
      Self.CancelActivation();
    end else begin
      // vsechny cesty nepostaveny -> stavime dalsi cestu
      var JC := JCDb.GetJCByID(Self.m_state.activatingJCs[Self.m_state.JCIndex]);
      if (JC = nil) then
        Self.CancelActivation()
      else
      begin
        Self.activatingJC := JC;
        Self.activatingJC.Activate(Self.m_state.SenderPnl, Self.m_state.SenderOR, nil, False, False, Self.m_state.abAfter);
      end;
      Self.changed := true;
    end;
  end else begin
    if (not Self.activatingJC.activating) then
    begin
      // cesta byla stavena, ale uz se nestavi -> evidentne nastala chyba -> ukoncime staveni slozene jizdni cesty
      Self.CancelActivation();
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.Activate(SenderPnl: TIdContext; SenderOR: TObject; abAfter: Boolean);
begin
  for var jcId in Self.m_data.JCs do
    if (JCDb.GetJCByID(jcId) = nil) then
      raise Exception.Create('JC ve slozene jizdni ceste neexistuje');

  Self.m_state.SenderOR := SenderOR;
  Self.m_state.SenderPnl := SenderPnl;
  Self.m_state.abAfter := abAfter;

  Self.m_state.activatingJCs.Clear();
  for var jcId in Self.m_data.JCs do
  begin
    var jc: TJC := JCDb.GetJCByID(jcId);
    if ((jc.signal.areas.Count > 0) and (jc.signal.areas[0].stack.mode = TORStackMode.VZ)) then
    begin
      jc.signal.areas[0].stack.AddJC(jc, SenderPnl, false, abAfter);
    end else
      Self.m_state.activatingJCs.Add(jcId);
  end;

  if (Self.m_state.activatingJCs.Count <> Self.m_data.JCs.Count) then
    TPanelConnData(SenderPnl.Data).ClearAndHidePathBlocks()
  else
    TPanelConnData(SenderPnl.Data).pathBlocks.Clear();

  if (Self.m_state.activatingJCs.Count = 0) then
    Exit();

  Self.activatingJC := JCDb.GetJCByID(Self.m_state.activatingJCs[0]);
  Self.activatingJC.Activate(SenderPnl, SenderOR, nil, false, false, Self.m_state.abAfter);
  Self.m_state.JCIndex := 0;

  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.CancelActivation();
begin
  Self.m_state.JCIndex := -1;
  Self.activatingJC := nil;

  // zrusime zacatek staveni na navestidle
  begin
    var JC := JCDb.GetJCByID(Self.m_data.JCs[0]);
    var signal := Blocks.GetBlkSignalByID(JC.data.signalId);
    signal.selected := TBlkSignalSelection.none;
  end;

  // zrusime konec staveni na poslednim useku posledni JC
  begin
    var JC := JCDb.GetJCByID(Self.m_data.JCs[Self.m_data.JCs.Count - 1]);
    var track := Blocks.GetBlkTrackOrRTByID(JC.data.tracks[JC.data.tracks.Count - 1]);
    track.jcEnd := TZaver.no;
  end;

  // zrusime konec staveni na vsech variantnich bodech
  for var i: Integer := 0 to Self.m_data.vb.Count - 1 do
  begin
    var track := Blocks.GetBlkTrackOrRTByID(Self.m_data.vb[i]);
    track.jcEnd := TZaver.no;
  end;

  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.IsActivating(): Boolean;
begin
  Result := (Self.m_state.JCIndex > -1);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TMultiJC.IdComparer(): IComparer<TMultiJC>;
begin
  Result := TComparer<TMultiJC>.Construct(
    function(const mJC1, mJC2: TMultiJC): Integer
    begin
      Result := CompareValue(mJC1.id, mJC2.id);
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.Match(blocks: TList<TBlk>): Boolean;
begin
  if ((Self.data.JCs.Count < 2) or (blocks.Count < 2)) then
    Exit(false);

  var JC := JCDb.GetJCByID(Self.data.JCs[0]);
  if (JC = nil) then
    Exit(false);

  if (blocks[0].typ <> btSignal) then
    Exit(false);
  var startSignal: TBlkSignal := TBlkSignal(blocks[0]);

  if (JC.data.signalId <> blocks[0].id) then
    Exit(false);
  if (Integer(startSignal.selected) <> Integer(JC.typ)) then
    Exit(false);

  // posledni blok musi byt posledni blok posledni jizdni cesty
  JC := JCDb.GetJCByID(Self.data.JCs[Self.data.JCs.Count - 1]);
  if (JC = nil) then
    Exit(false);
  if (JC.data.tracks[JC.data.tracks.Count - 1] <> blocks[blocks.Count-1].id) then
    Exit(false);

  // kontrola variantnich bodu
  if (blocks.Count <> Self.data.vb.Count+2) then
    Exit(false);

  for var j: Integer := 0 to Self.data.vb.Count - 1 do
    if (Self.data.vb[j] <> blocks[j+1].id) then
      Exit(false);

  for var j: Integer := 0 to Self.data.JCs.Count - 1 do
    if (JCDb.GetJCByID(Self.data.JCs[j]) = nil) then
      Exit(false);

  Exit(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TMultiJC.StartSignal(): TBlkSignal;
var JC: TJC;
begin
  if (Self.data.JCs.Count > 0) then
  begin
    JC := JCDb.GetJCByID(Self.data.JCs[0]);
    if (JC = nil) then
      Result := nil
    else
      Result := JC.signal as TBlkSignal;
  end
  else
    Result := nil
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
