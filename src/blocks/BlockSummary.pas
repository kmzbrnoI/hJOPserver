unit BlockSummary;

{ SUMMARY technological block definition. (Souètová hláska) }

interface

uses IniFiles, Block, Menus, SysUtils, Classes, IdContext, Generics.Collections,
  Area, TCPServerPanel;

type
  TBlkSummarySettings = record
    crossings: TList<Integer>;
  end;

  TBlkSummaryState = record
    enabled: Boolean;
  end;

  TBlkSummary = class(TBlk)
  const
    _def_summary_state: TBlkSummaryState = (enabled: false;);

  protected
    m_settings: TBlkSummarySettings;
    m_state: TBlkSummaryState;

    function IsCommunication(): Boolean;
    function IsAnnulation(): Boolean;
    function IsPcClosed(): Boolean;
    function IsClosed(): Boolean;
    function IsError(): Boolean;
    function IsEmOpen(): Boolean;

    procedure CreateReferences();
    procedure RemoveReferences();

  public

    constructor Create(index: Integer);
    destructor Destroy(); override;

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    function GetSettings(): TBlkSummarySettings;
    procedure SetSettings(data: TBlkSummarySettings);

    // ----- Sumary block specific functions -----

    property state: TBlkSummaryState read m_state;
    property enabled: Boolean read m_state.enabled;

    property communication: Boolean read IsCommunication;
    property annulation: Boolean read IsAnnulation;
    property pcClosed: Boolean read IsPcClosed;
    property closed: Boolean read IsClosed;
    property error: Boolean read IsError;
    property emOpen: Boolean read IsEmOpen;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;

  end; // class TBlkUsek

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses BlockCrossing, BlockDb, AreaDb, Graphics, ownConvert;

constructor TBlkSummary.Create(index: Integer);
begin
  inherited;

  Self.m_state := Self._def_summary_state;
  Self.m_settings.crossings := TList<Integer>.Create();
  Self.m_globSettings.typ := btSummary;
end;

destructor TBlkSummary.Destroy();
begin
  Self.m_settings.crossings.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
var data: TStrings;
  str: string;
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.crossings.Clear();
  data := TStringList.Create();
  try
    ExtractStrings([','], [], PChar(ini_tech.ReadString(section, 'prejezdy', '')), data);

    for str in data do
      Self.m_settings.crossings.Add(StrToInt(str));

    Self.LoadORs(ini_rel, 'T').Free();
  finally
    data.Free();
  end;
end;

procedure TBlkSummary.SaveData(ini_tech: TMemIniFile; const section: string);
var str: string;
begin
  inherited;

  str := '';
  for var n: Integer in Self.m_settings.crossings do
    str := str + IntToStr(n) + ',';

  if (str <> '') then
    ini_tech.WriteString(section, 'prejezdy', str);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.Enable();
begin
  Self.m_state.enabled := true;
  Self.CreateReferences();
end;

procedure TBlkSummary.Disable();
begin
  Self.m_state.enabled := false;
  Self.Change(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSummary.GetSettings(): TBlkSummarySettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkSummary.SetSettings(data: TBlkSummarySettings);
begin
  if (Self.enabled) then
    Self.RemoveReferences();
  Self.m_settings.crossings.Free();

  Self.m_settings := data;

  if (Self.enabled) then
    Self.CreateReferences();

  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkSummary.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
var prjid: Integer;
  crossing: TBlk;
begin
  Result := inherited;

  for prjid in Self.m_settings.crossings do
  begin
    Blocks.GetBlkByID(prjid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      Result := Result + crossing.name + ','
    else
      Result := Result + '#???,';
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSummary.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
var crossing: TBlk;
begin
  if (not Self.enabled) then
    Exit();

  if ((itemindex - 2 >= 0) and (itemindex - 2 < Self.m_settings.crossings.Count)) then
  begin
    Blocks.GetBlkByID(Self.m_settings.crossings[itemindex - 2], crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      PanelServer.Menu(SenderPnl, crossing, SenderOR as TArea, crossing.ShowPanelMenu(SenderPnl, SenderOR,
        TAreaRights.write));
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSummary.IsCommunication(): Boolean;
begin
  Result := true;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
    begin
      if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.disabled) then
        Exit(false);
    end
    else
      Exit(false);
  end;
end;

function TBlkSummary.IsAnnulation(): Boolean;
begin
  Result := false;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      if (TBlkCrossing(crossing).annulation) then
        Exit(true);
  end;
end;

function TBlkSummary.IsPcClosed(): Boolean;
begin
  Result := false;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      if (TBlkCrossing(crossing).pcClosed) then
        Exit(true);
  end;
end;

function TBlkSummary.IsClosed(): Boolean;
begin
  Result := false;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.closed) then
        Exit(true);
  end;
end;

function TBlkSummary.IsError(): Boolean;
begin
  Result := false;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.none) then
        Exit(true);
  end;
end;

function TBlkSummary.IsEmOpen(): Boolean;
begin
  Result := false;
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      if (TBlkCrossing(crossing).pcEmOpen) then
        Exit(true);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.CreateReferences();
begin
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      TBlkCrossing(crossing).AddSH(Self);
  end;
end;

procedure TBlkSummary.RemoveReferences();
begin
  for var crossingid: Integer in Self.m_settings.crossings do
  begin
    var crossing: TBlk;
    Blocks.GetBlkByID(crossingid, crossing);
    if ((crossing <> nil) and (crossing.typ = btCrossing)) then
      TBlkCrossing(crossing).RemoveSH(Self);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkSummary.PanelStateString(): string;
var fg: TColor;
begin
  Result := inherited;

  // n.o. rail
  fg := clTeal;
  if (Self.annulation) then
    fg := clWhite;
  Result := Result + ownConvert.ColorToStr(fg) + ';';
  Result := Result + ownConvert.ColorToStr(clBlack) + ';0;';

  // left rectangle
  fg := clGreen;
  if ((Self.error) or (Self.emOpen)) then
    fg := clRed;
  if (not Self.communication) then
    fg := clFuchsia;
  Result := Result + ownConvert.ColorToStr(fg) + ';';

  // right rectangle
  fg := clBlack;
  if (Self.closed) then
    fg := $A0A0A0;
  if (Self.pcClosed) then
    fg := clWhite;
  Result := Result + ownConvert.ColorToStr(fg) + ';';
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
