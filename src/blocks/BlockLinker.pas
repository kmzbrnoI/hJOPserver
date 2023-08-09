unit BlockLinker;

{ LINKER technological block definition. (úvazka) }

interface

uses IniFiles, Block, Menus, AreaDb, SysUtils, Classes, JsonDataObjects,
  IdContext, StrUtils, Area, Generics.Collections;

type
  TBlkLinkerSettings = record
    parent: Integer; // reference na matersky blok (typu TTrat)
  end;

  TBlkLinkerState = record
    enabled: Boolean;
    departureForbidden: Boolean;
    note: string;
    emLock: Boolean;
  end;

  TBlkLinker = class(TBlk)
  const
    _def_linker_state: TBlkLinkerState = (enabled: false; departureForbidden: false; note: ''; emLock: false;);

  private
    m_settings: TBlkLinkerSettings;
    m_state: TBlkLinkerState;
    m_parent: TBlk;
    m_request: Boolean;

    function GetParent(): TBlk;

    procedure SetNote(stit: string);
    procedure SetDepForb(ZAK: Boolean);
    procedure SetEmLock(nouz: Boolean);

    procedure MenuZTSOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZTSOffClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuUTSClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuOTSClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAKOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAKOffClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVOnClick(SenderPnl: TIdContext; SenderOR: TObject);
    procedure MenuZAVOffClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure MenuTrainInfoClick(SenderPnl: TIdContext; SenderOR: TObject);

    procedure PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);
    procedure PanelPotvrSekvZAK(Sender: TIdContext; success: Boolean);

    procedure UPOZTSOnClick(Sender: TObject);
    procedure UPOUTSClick(Sender: TObject);
    procedure UPOOTSClick(Sender: TObject);
    procedure UPOZAKOnClick(Sender: TObject);

    procedure SetRequest(zadost: Boolean);
    procedure NoteUPO(SenderPnl: TIdContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
      UPO_EscCallback: TNotifyEvent);
    procedure MenuTrainPredicted(SenderPnl: TIdContext; SenderOR: TObject);

  public
    constructor Create(index: Integer);

    procedure LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;
    procedure SaveState(ini_stat: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;
    procedure AfterLoad(); override;

    procedure Change(now: Boolean = false); override;
    procedure ChangeFromTrat();

    // ----- Linker specific functions -----

    procedure DoZTS(SenderPnl: TIdContext; SenderOR: TObject);
    procedure DoUTS(SenderPnl: TIdContext; SenderOR: TObject);

    function GetSettings(): TBlkLinkerSettings;
    procedure SetSettings(data: TBlkLinkerSettings);

    procedure GetPtData(json: TJsonObject; includeState: Boolean); override;
    procedure GetPtState(json: TJsonObject); override;

    procedure ApproveRequest();
    function CanZTS(): Boolean;
    function CanUTS(): Boolean;
    function CanZAKOn(): Boolean;
    function CanZAKOff(): Boolean;

    property note: string read m_state.note write SetNote;
    property departureForbidden: Boolean read m_state.departureForbidden write SetDepForb;
    property enabled: Boolean read m_state.enabled;

    property parent: TBlk read GetParent;
    property request: Boolean read m_request write SetRequest;
    property emLock: Boolean read m_state.emLock write SetEmLock;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string; override;
    procedure ShowUvazkaTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; train_index: Integer);
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
      params: string = ''); override;
    function PanelStateString(): string; override;
    function AcceptsMenuClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; item: string): Boolean; override;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, BlockDb, UPO, Graphics, Train, ownConvert, TrainDb,
  TJCDatabase, fMain, TCPServerPanel, BlockRailway, AreaStack, BlockTrack, TCPAreasRef,
  Logging;

constructor TBlkLinker.Create(index: Integer);
begin
  inherited Create(index);

  Self.m_globSettings.typ := btLinker;
  Self.m_state := _def_linker_state;
  Self.m_parent := nil;
  Self.m_request := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.LoadData(ini_tech: TMemIniFile; const section: string; ini_rel, ini_stat: TMemIniFile);
begin
  inherited LoadData(ini_tech, section, ini_rel, ini_stat);

  Self.m_settings.parent := ini_tech.ReadInteger(section, 'parent', -1);
  Self.m_state.note := ini_stat.ReadString(section, 'stit', '');

  Self.LoadAreas(ini_rel, 'Uv').Free();
end;

procedure TBlkLinker.SaveData(ini_tech: TMemIniFile; const section: string);
begin
  inherited SaveData(ini_tech, section);

  ini_tech.WriteInteger(section, 'parent', Self.m_settings.parent);
end;

procedure TBlkLinker.SaveState(ini_stat: TMemIniFile; const section: string);
begin
  if (Self.m_state.note <> '') then
    ini_stat.WriteString(section, 'stit', Self.m_state.note);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.Enable();
begin
  Self.m_state.enabled := true;
  Self.Change();
end;

procedure TBlkLinker.Disable();
begin
  Self.m_state.enabled := false;
  Self.m_state.emLock := false;
  Self.Change(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.AfterLoad();
begin
  if (Self.parent = nil) then
    Self.Log('Není návaznost na trať', llWarning);
  if (Self.areas.Count <> 1) then
    Self.Log('Není v právě jedné dopravně', llWarning);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.Change(now: Boolean = false);
begin
  inherited Change(now);
  (Self.parent as TBlkRailway).ChangeFromLinker(Self);
end;

procedure TBlkLinker.ChangeFromTrat();
begin
  if (Self.parent = nil) then
    Exit();
  if (not(Self.parent as TBlkRailway).request) then
    Self.m_request := false;

  inherited Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.SetNote(stit: string);
begin
  Self.m_state.note := stit;
  Self.Change();
end;

procedure TBlkLinker.SetDepForb(ZAK: Boolean);
var old: Boolean;
begin
  old := Self.m_state.departureForbidden;
  Self.m_state.departureForbidden := ZAK;
  Self.Change();

  if (old <> ZAK) then
    (Self.parent as TBlkRailway).ChangeTracks();
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkLinker.GetSettings(): TBlkLinkerSettings;
begin
  Result := Self.m_settings;
end;

procedure TBlkLinker.SetSettings(data: TBlkLinkerSettings);
begin
  Self.m_settings := data;
  Self.m_parent := nil; // timto se zajisti prepocitani parent pri pristi zadost i nej
  Self.Change();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.MenuZTSOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  case ((SenderOR as TArea).stack.mode) of
    TORStackMode.VZ:
      (SenderOR as TArea).stack.AddZTS(Self, SenderPnl);
    TORStackMode.PV:
      Self.DoZTS(SenderPnl, SenderOR)
  end;
end;

procedure TBlkLinker.MenuZTSOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  Self.request := false;
end;

procedure TBlkLinker.MenuUTSClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  case ((SenderOR as TArea).stack.mode) of
    TORStackMode.VZ:
      (SenderOR as TArea).stack.AddUTS(Self, SenderPnl);
    TORStackMode.PV:
      Self.DoUTS(SenderPnl, SenderOR)
  end;
end;

procedure TBlkLinker.ApproveRequest();
begin
  if (not(Self.parent as TBlkRailway).request) then
    Exit();

  case ((Self.parent as TBlkRailway).direction) of
    TRailwayDirection.AtoB:
      (Self.parent as TBlkRailway).direction := TRailwayDirection.BtoA;
    TRailwayDirection.BtoA:
      (Self.parent as TBlkRailway).direction := TRailwayDirection.AtoB;
    TRailwayDirection.no:
      begin
        if ((Self.parent as TBlkRailway).IsFirstLinker(Self)) then
          (Self.parent as TBlkRailway).direction := TRailwayDirection.BtoA
        else
          (Self.parent as TBlkRailway).direction := TRailwayDirection.AtoB;
      end; // case
  end;

  Self.request := false;
end;

procedure TBlkLinker.MenuOTSClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOOTSClick, nil)
  else
    Self.UPOOTSClick(SenderPnl);
end;

procedure TBlkLinker.MenuZAKOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOZAKOnClick, nil)
  else
    Self.UPOZAKOnClick(SenderPnl);
end;

procedure TBlkLinker.MenuZAKOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.ConfirmationSequence(SenderPnl, Self.PanelPotvrSekvZAK, SenderOR as TArea,
    'Zrušení zákazu odjezdu na trať', GetObjsList(Self), nil);
end;

procedure TBlkLinker.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.note(SenderPnl, Self, Self.m_state.note);
end;

procedure TBlkLinker.MenuZAVOnClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if ((Self.parent as TBlkRailway).request) then
    (Self.parent as TBlkRailway).request := false;
  Self.emLock := true;
end;

procedure TBlkLinker.MenuZAVOffClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  PanelServer.ConfirmationSequence(SenderPnl, Self.PanelPotvrSekvZAV, SenderOR as TArea, 'Zrušení nouzového závěru',
    GetObjsList(Self), nil);
end;

procedure TBlkLinker.PanelPotvrSekvZAK(Sender: TIdContext; success: Boolean);
begin
  if (success) then
    Self.departureForbidden := false;
end;

procedure TBlkLinker.PanelPotvrSekvZAV(Sender: TIdContext; success: Boolean);
begin
  if (success) then
    Self.emLock := false;
end;

procedure TBlkLinker.UPOZTSOnClick(Sender: TObject);
begin
  Self.request := true;

  if (Self.m_areas[0].stack.mode = TORStackMode.VZ) then
    Self.m_areas[0].stack.RemoveZTS(Self);
end;

procedure TBlkLinker.UPOUTSClick(Sender: TObject);
begin
  Self.ApproveRequest();

  if (Self.m_areas[0].stack.mode = TORStackMode.VZ) then
    Self.m_areas[0].stack.RemoveUTS(Self);
end;

procedure TBlkLinker.UPOOTSClick(Sender: TObject);
begin
  if ((Self.parent as TBlkRailway).GetSettings.rType = TRailwayType.request) then
    (Self.parent as TBlkRailway).direction := TRailwayDirection.no;
  Self.request := false;
end;

procedure TBlkLinker.UPOZAKOnClick(Sender: TObject);
begin
  if ((Self.parent as TBlkRailway).request) then
    (Self.parent as TBlkRailway).request := false;
  Self.departureForbidden := true;
end;

procedure TBlkLinker.MenuTrainInfoClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var railway := TBlkRailway(Self.parent);
  if (railway.trainPredict = nil) then
    Exit();

  var train: TTrain := TrainDb.trains[railway.trainPredict.traini];
  var csItems := train.InfoWindowItems();
  try
    PanelServer.InfoWindow(SenderPnl, nil, TArea(SenderOR), 'Vlak ' + train.name, GetObjsList(Self), csItems, True, False);
  finally
    csItems.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// vytvoreni menu pro potreby konkretniho bloku:
function TBlkLinker.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights): string;
var railway: TBlkRailway;
begin
  railway := TBlkRailway(Self.parent);
  if (railway = nil) then
    Exit('-');

  Result := inherited;

  if (Self.CanUTS()) then
    Result := Result + 'UTS,OTS,';

  if (Self.request) then
    Result := Result + 'ZTS<,';

  if ((Self.CanZTS()) or ((SenderOR as TArea).stack.mode = TORStackMode.VZ)) then
    Result := Result + 'ZTS>,';

  if ((not Self.CanUTS()) and ((SenderOR as TArea).stack.mode = TORStackMode.VZ)) then
    Result := Result + 'UTS,';

  if (RightStr(Result, 2) <> '-,') then
    Result := Result + '-,';

  if (Self.emLock) then
    Result := Result + '!ZAV<,'
  else
    Result := Result + 'ZAV>,';

  if (Self.CanZAKOff()) then
    Result := Result + '!ZAK<,'
  else if (Self.CanZAKOn()) then
    Result := Result + 'ZAK>,';

  Result := Result + 'STIT,';
end;

function TBlkLinker.AcceptsMenuClick(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights; item: string): Boolean;
begin
  var menu: string := Self.ShowPanelMenu(SenderPnl, SenderOR, rights);
  Result := menu.Contains(item) or (item = 'INFO vlak'); // check of validity of 'INFO vlak' is performed in callback
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.ShowUvazkaTrainMenu(SenderPnl: TIdContext; SenderOR: TObject; rights: TAreaRights;
  train_index: Integer);
var railway: TBlkRailway;
begin
  railway := TBlkRailway(Self.parent);
  if (railway = nil) then
    Exit();

  if (train_index < railway.state.trains.Count) then
  begin
    var train: TTrain := railway.state.trains[train_index].Train;
    var blk: TBlk := railway.GetTrainTrack(train);
    if (blk = nil) then
      Exit();
    TBlkTrack(blk).MenuSOUPRAVA(SenderPnl, SenderOR, 0); // it must be 0th train in the track, because it's railway track
  end else if (railway.trainPredict <> nil) then begin
    Self.MenuTrainPredicted(SenderPnl, SenderOR);
  end;
end;

procedure TBlkLinker.MenuTrainPredicted(SenderPnl: TIdContext; SenderOR: TObject);
begin
  var railway := TBlkRailway(Self.parent);
  if (railway.trainPredict = nil) then
    Exit();

  var train: TTrain := TrainDb.Trains[railway.trainPredict.traini];
  var menu := '$' + Self.name + ',';
  menu := menu + '$Souprava ' + train.name + ',-,';
  menu := menu + train.MenuRailwayPredicted();

  PanelServer.menu(SenderPnl, Self, (SenderOR as TArea), menu);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.PanelClick(SenderPnl: TIdContext; SenderOR: TObject; Button: TPanelButton; rights: TAreaRights;
  params: string = '');
begin
  if (TBlkRailway(Self.parent).direction < TRailwayDirection.no) then
    Exit();
  if (Button = TPanelButton.ESCAPE) then
    Exit();

  if (params <> '') then
    Self.ShowUvazkaTrainMenu(SenderPnl, SenderOR, rights, StrToInt(params))
  else
    PanelServer.Menu(SenderPnl, Self, (SenderOR as TArea), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

/// /////////////////////////////////////////////////////////////////////////////

// toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkLinker.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject; item: string; itemindex: Integer);
begin
  if (item = 'ZTS>') then
    Self.MenuZTSOnClick(SenderPnl, SenderOR)
  else if (item = 'ZTS<') then
    Self.MenuZTSOffClick(SenderPnl, SenderOR)
  else if (item = 'UTS') then
    Self.MenuUTSClick(SenderPnl, SenderOR)
  else if (item = 'OTS') then
    Self.MenuOTSClick(SenderPnl, SenderOR)
  else if (item = 'ZAK>') then
    Self.MenuZAKOnClick(SenderPnl, SenderOR)
  else if (item = 'ZAK<') then
    Self.MenuZAKOffClick(SenderPnl, SenderOR)
  else if (item = 'STIT') then
    Self.MenuStitClick(SenderPnl, SenderOR)
  else if (item = 'ZAV>') then
    Self.MenuZAVOnClick(SenderPnl, SenderOR)
  else if (item = 'ZAV<') then
    Self.MenuZAVOffClick(SenderPnl, SenderOR)
  else if (item = 'INFO vlak') then
    Self.MenuTrainInfoClick(SenderPnl, SenderOR);
end;

/// ////////////////////////////////////////////////////////////////////////////

function TBlkLinker.GetParent(): TBlk;
begin
  if (Self.m_parent = nil) then
    Self.m_parent := Blocks.GetBlkRailwayByID(Self.m_settings.parent);
  Result := Self.m_parent;
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.SetRequest(zadost: Boolean);
begin
  // tohleto poradi nastvovani je dulezite
  if (zadost) then
    Self.m_request := zadost;
  (Self.parent as TBlkRailway).request := zadost;
  if (not zadost) then
    Self.m_request := zadost;
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.SetEmLock(nouz: Boolean);
begin
  if (Self.m_state.emLock = nouz) then
    Exit();

  Self.m_state.emLock := nouz;
  Self.Change();
end;

/// ////////////////////////////////////////////////////////////////////////////

// Takto zasobnik zjistuje, jestli muze zacit zadost:
function TBlkLinker.CanZTS(): Boolean;
begin
  var railway := TBlkRailway(Self.parent);
  if ((railway = nil) or (not railway.railwayFree) or (Self.request)) then
    Exit(false);

  if (railway.IsFirstLinker(Self)) then
    Result := (railway.direction <> TRailwayDirection.AtoB)
  else
    Result := (railway.direction <> TRailwayDirection.BtoA);
end;

function TBlkLinker.CanUTS(): Boolean;
begin
  var railway := TBlkRailway(Self.parent);
  Result := ((railway <> nil) and (not Self.request) and (railway.request));
end;

function TBlkLinker.CanZAKOn(): Boolean;
begin
  var railway := TBlkRailway(Self.parent);
  if (railway = nil) then
    Exit(false);

  Result := ((not railway.departureForbidden) and (not railway.Zaver) and (not railway.occupied));
end;

function TBlkLinker.CanZAKOff(): Boolean;
begin
  var railway := TBlkRailway(Self.parent);
  if (railway = nil) then
    Exit(false);

  // zruseni ZAK je podmineno tim, ze na krajnich usecich trati nejsou zavery
  // to zajistuje, ze nelze zrusit ZAK u trati, do ktere je postaven PMD
  var first := Blocks.GetBlkByID(railway.GetSettings().trackIds[0]);
  var last := Blocks.GetBlkByID(railway.GetSettings().trackIds[railway.GetSettings().trackIds.Count - 1]);
  Result := ((Self.departureForbidden) and (first <> nil) and (last <> nil) and (TBlkTrack(first).Zaver = TZaver.no) and (TBlkTrack(last).Zaver = TZaver.no));
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.NoteUPO(SenderPnl: TIdContext; SenderOR: TObject; UPO_OKCallback: TNotifyEvent;
  UPO_EscCallback: TNotifyEvent);
var upos: TUPOItems;
begin
  upos := TList<TUPOItem>.Create;
  try
    upos.Add(UPO.NoteUPO(Self.name, Self.note));
    PanelServer.UPO(SenderPnl, upos, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
  finally
    upos.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.DoZTS(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOZTSOnClick, nil)
  else
    Self.UPOZTSOnClick(SenderPnl);
end;

procedure TBlkLinker.DoUTS(SenderPnl: TIdContext; SenderOR: TObject);
begin
  if (Self.note <> '') then
    Self.NoteUPO(SenderPnl, SenderOR, Self.UPOUTSClick, nil)
  else
    Self.UPOUTSClick(SenderPnl);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TBlkLinker.PanelStateString(): string;
var fg, bg: TColor;
  railway: TBlkRailway;
begin
  Result := inherited;

  railway := TBlkRailway(Self.parent);
  if (railway.direction = TRailwayDirection.disabled) then
  begin
    fg := clBlack;
    bg := clFuchsia;
  end else begin
    if (Self.note <> '') then
      bg := clTeal
    else
      bg := clBlack;

    if (railway.RBPCan) then
      fg := clRed
    else if (railway.Zaver) then
      fg := clBlue
    else if (railway.emLock) then
      fg := clAqua
    else if (railway.occupied) then
      fg := clBlue
    else
      fg := $A0A0A0;
  end;

  Result := Result + ownConvert.ColorToStr(fg) + ';' + ownConvert.ColorToStr(bg) + ';' +
    IntToStr(ownConvert.BoolToInt(railway.request)) + ';';

  case (railway.direction) of
    TRailwayDirection.disabled, TRailwayDirection.no:
      Result := Result + '0;';
    TRailwayDirection.AtoB:
      Result := Result + '1;';
    TRailwayDirection.BtoA:
      Result := Result + '2;';
  end;

  Result := Result + '{' + railway.GetTrainsList(',') + '}';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TBlkLinker.GetPtData(json: TJsonObject; includeState: Boolean);
begin
  inherited;

  json['parent'] := Self.m_settings.parent;

  if (includeState) then
    Self.GetPtState(json['blockState']);
end;

procedure TBlkLinker.GetPtState(json: TJsonObject);
begin
  json['enabled'] := Self.enabled;
  json['departureForbidden'] := Self.departureForbidden;
  json['emLock'] := Self.emLock;
  json['request'] := Self.request;

  if (Self.note <> '') then
    json['note'] := Self.note;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
