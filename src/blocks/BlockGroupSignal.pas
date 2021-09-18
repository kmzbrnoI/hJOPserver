unit BlockGroupSignal;

{ GROUP SIGNAL technological block definition. }

interface

uses IniFiles, Block, AreaDb, BlockSignal, IdContext, Generics.Collections,
  JsonDataObjects, Area;

type

  TBlkGSSettings = record
    signalIds: TList<Integer>;
  end;

  TBlkGroupSignal = class(TBlkSignal)
  private
    m_gs_settings: TBlkGSSettings;

    function GetSignals(): TList<TBlkSignal>;
    procedure RefillSignals();
    procedure SendGroupMasters();

  public

    m_signals: TList<TBlkSignal>;

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure AfterLoad(); override;

    // ----- Group Signal specific functions -----

    function GetSettings(): TBlkGSSettings;
    procedure SetSettings(data: TBlkGSSettings);

    property signals: TList<TBlkSignal> read GetSignals;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;

  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses Classes, ownStrUtils, SysUtils, BlockDb, ownConvert;

constructor TBlkGroupSignal.Create(index: Integer);
begin
  inherited Create(index);
  Self.m_globSettings.typ := btGroupSignal;
  Self.m_gs_settings.signalIds := TList<Integer>.Create();
  Self.m_signals := TList<TBlkSignal>.Create();
end;

destructor TBlkGroupSignal.Destroy();
begin
  Self.m_gs_settings.signalIds.Free();
  Self.m_signals.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);
  Self.m_spnl.trackId := -1;

  Self.m_gs_settings.signalIds.Clear();

  var strs: TStrings := TStringList.Create();
  try
    ExtractStringsEx([','], [], ini_tech.ReadString(section, 'navestidla', ''), strs);
    for var str in strs do
      Self.m_gs_settings.signalIds.Add(StrToInt(str));
  finally
    strs.Free();
  end;
end;

procedure TBlkGroupSignal.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);
  ini_tech.WriteString(section, 'navestidla', SerializeIntList(Self.m_gs_settings.signalIds));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkGroupSignal.GetSettings(): TBlkGSSettings;
begin
  Result := Self.m_gs_settings;
end;

procedure TBlkGroupSignal.SetSettings(data: TBlkGSSettings);
begin
  for var signal: TBlkSignal in Self.signals do
    signal.groupMaster := nil;

  if (Self.m_gs_settings.signalIds <> data.signalIds) then
    Self.m_gs_settings.signalIds.Free();

  Self.SendGroupMasters();

  Self.m_gs_settings := data;
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkGroupSignal.GetSignals(): TList<TBlkSignal>;
begin
  if (Self.m_gs_settings.signalIds.Count <> Self.m_signals.Count) then
  begin
    Self.RefillSignals();
    Exit(Self.m_signals);
  end;

  for var i: Integer := 0 to Self.m_signals.Count - 1 do
  begin
    if (Self.m_gs_settings.signalIds[i] <> Self.m_signals[i].id) then
    begin
      Self.RefillSignals();
      Exit(Self.m_signals);
    end;
  end;

  Result := Self.m_signals;
end;

procedure TBlkGroupSignal.RefillSignals();
begin
  Self.m_signals.Clear();
  for var sigId in Self.m_gs_settings.signalIds do
  begin
    var blk: TBlk;
    Blocks.GetBlkByID(sigId, blk);
    if ((blk <> nil) and (blk.typ = TBlkType.btSignal)) then
      Self.m_signals.Add(TBlkSignal(blk));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkGroupSignal.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
begin
  Result := TBlk(Self).ShowPanelMenu(SenderPnl, SenderOR, rights);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton;
  rights: TAreaRights; params: string = '');
begin
  // This should be empty
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  // This should be empty
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  json.A['signals'];
  for var sigId in Self.m_gs_settings.signalIds do
    json.A['signals'].Add(sigId);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.AfterLoad();
begin
  Self.SendGroupMasters();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkGroupSignal.SendGroupMasters();
begin
  for var signal in Self.signals do
    signal.groupMaster := Self;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
