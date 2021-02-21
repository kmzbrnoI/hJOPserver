unit TechnologieMultiJC;

{
  Technologie slozenych jizdnich cest.
}

interface

uses
  IniFiles, TechnologieJC, Generics.Collections, BlockDb, IdContext, SysUtils,
  Classes, Generics.Defaults, Math, Block, BlockSignal;

type

  TMultiJCState = record
    JCIndex: Integer;
    SenderOR: TObject;
    SenderPnl: TIdContext;
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
    _def_mutiJC_staveni: TMultiJCState = (JCIndex: - 1; SenderOR: nil; SenderPnl: nil;);

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

    procedure Activate(SenderPnl: TIdContext; SenderOR: TObject);
    procedure CancelActivation();

    function Match(startNav: TBlkSignal; vb: TList<TObject>; endBlk: TBlk): Boolean;
    function StartSignal(): TBlkSignal;

    property data: TMultiJCData read m_data write m_data;
    property state: TMultiJCState read m_state;
    property name: string read m_data.name;
    property activating: Boolean read IsActivating;
    property id: Integer read m_data.id;

    class function IdComparer(): IComparer<TMultiJC>;
  end;

implementation

uses TJCDatabase, BlockTrack, Area;

/// /////////////////////////////////////////////////////////////////////////////

constructor TMultiJC.Create();
begin
  inherited Create();

  Self.changed := true;
  Self.m_state := _def_mutiJC_staveni;

  Self.m_data.JCs := TList<Integer>.Create();
  Self.m_data.vb := TList<Integer>.Create();

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
end;

destructor TMultiJC.Destroy();
begin
  if (Assigned(Self.m_data.JCs)) then
    FreeAndNil(Self.m_data.JCs);
  if (Assigned(Self.m_data.vb)) then
    FreeAndNil(Self.m_data.vb);

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
    var str: string := '';
    for var i: Integer := 0 to Self.data.JCs.Count - 1 do
      str := str + IntToStr(Self.data.JCs[i]) + ';';
    if (str <> '') then
      ini.WriteString(section, 'JCs', str);
  end;

  begin
    var str: string := '';
    for var i: Integer := 0 to Self.data.vb.Count - 1 do
      str := str + IntToStr(Self.data.vb[i]) + ';';
    if (str <> '') then
      ini.WriteString(section, 'vb', str);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.UpdateActivating();
var JC: TJC;
begin
  if (Self.activatingJC.active) then
  begin
    // aktualni cesta postavena
    Inc(Self.m_state.JCIndex);

    if (Self.m_state.JCIndex >= Self.m_data.JCs.Count) then
    begin
      // vsechny cesty postaveny
      Self.CancelActivation();
    end else begin
      // vsechny cesty nepostaveny -> stavime dalsi cestu
      JC := JCDb.GetJCByID(Self.m_data.JCs[Self.m_state.JCIndex]);
      if (JC = nil) then
        Self.CancelActivation()
      else
      begin
        Self.activatingJC := JC;
        Self.activatingJC.Activate(Self.m_state.SenderPnl, Self.m_state.SenderOR);
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

procedure TMultiJC.Activate(SenderPnl: TIdContext; SenderOR: TObject);
begin
  for var i: Integer := 0 to Self.m_data.JCs.Count - 1 do
    if (JCDb.GetJCByID(Self.m_data.JCs[i]) = nil) then
      raise Exception.Create('JC ve slozene jizdni ceste neexistuje');

  Self.m_state.SenderOR := SenderOR;
  Self.m_state.SenderPnl := SenderPnl;

  Self.activatingJC := JCDb.GetJCByID(Self.m_data.JCs[0]);
  Self.activatingJC.Activate(SenderPnl, SenderOR);
  Self.m_state.JCIndex := 0;

  (SenderOR as TArea).vb.Clear();

  Self.changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TMultiJC.CancelActivation();
var JC: TJC;
  Blk: TBlk;
begin
  Self.m_state.JCIndex := -1;
  Self.activatingJC := nil;

  // zrusime zacatek staveni na navestidle
  JC := JCDb.GetJCByID(Self.m_data.JCs[0]);
  Blocks.GetBlkByID(JC.data.signalId, Blk);
  (Blk as TBlkSignal).selected := TBlkSignalSelection.none;

  // zrusime konec staveni na poslednim useku posledni JC
  JC := JCDb.GetJCByID(Self.m_data.JCs[Self.m_data.JCs.Count - 1]);
  Blocks.GetBlkByID(JC.data.tracks[JC.data.tracks.Count - 1], Blk);
  (Blk as TBlkTrack).jcEnd := TZaver.no;

  // zrusime konec staveni na vsech variantnich bodech
  for var i: Integer := 0 to Self.m_data.vb.Count - 1 do
  begin
    Blocks.GetBlkByID(Self.m_data.vb[i], Blk);
    (Blk as TBlkTrack).jcEnd := TZaver.no;
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

function TMultiJC.Match(startNav: TBlkSignal; vb: TList<TObject>; endBlk: TBlk): Boolean;
var JC: TJC;
begin
  if (Self.data.JCs.Count < 2) then
    Exit(false);

  JC := JCDb.GetJCByID(Self.data.JCs[0]);
  if (JC = nil) then
    Exit(false);

  if (JC.data.signalId <> startNav.id) then
    Exit(false);
  if (Integer(startNav.selected) <> Integer(JC.typ)) then
    Exit(false);

  // posledni blok musi byt posledni blok posledni jizdni cesty
  JC := JCDb.GetJCByID(Self.data.JCs[Self.data.JCs.Count - 1]);
  if (JC = nil) then
    Exit(false);
  if (JC.data.tracks[JC.data.tracks.Count - 1] <> endBlk.id) then
    Exit(false);

  // kontrola variantnich bodu
  if (vb.Count <> Self.data.vb.Count) then
    Exit(false);
  for var j: Integer := 0 to vb.Count - 1 do
    if (Self.data.vb[j] <> (vb[j] as TBlk).id) then
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
