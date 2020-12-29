unit TBlockSummary;

{ SUMMARY technological block definition. (Souètová hláska) }

interface

uses IniFiles, TBlock, Menus, SysUtils, Classes, IdContext, Generics.Collections,
     TOblRizeni, TCPServerOR;

type
 TBlkSummarySettings = record
  crossings: TList<Integer>;
 end;

 TBlkSummaryState = record
  enabled: Boolean;
 end;

 TBlkSummary = class(TBlk)
  const
   _def_summary_state: TBlkSummaryState = (
     enabled: false;
   );

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

    procedure LoadData(ini_tech: TMemIniFile; const section: string;
                       ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    function GetSettings(): TBlkSummarySettings;
    procedure SetSettings(data: TBlkSummarySettings);

    //----- Sumary block specific functions -----

    property state: TBlkSummaryState read m_state;
    property enabled: Boolean read m_state.enabled;

    property communication: Boolean read IsCommunication;
    property annulation: Boolean read IsAnnulation;
    property pcClosed: Boolean read IsPcClosed;
    property closed: Boolean read IsClosed;
    property error: Boolean read IsError;
    property emOpen: Boolean read IsEmOpen;

    procedure PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject;
                             item: string; itemindex: Integer); override;
    function ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject;
                           rights: TORCOntrolRights): string; override;
    procedure PanelClick(SenderPnl: TIdContext; SenderOR: TObject;
                         Button: TPanelButton; rights: TORCOntrolRights;
                         params: string = ''); override;
    function PanelStateString(): string; override;

 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBlockCrossing, TBloky, TOblsRizeni, Graphics, ownConvert;

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

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.LoadData(ini_tech: TMemIniFile; const section: string;
                          ini_rel, ini_stat: TMemIniFile);
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
    n: Integer;
begin
 inherited;

 str := '';
 for n in Self.m_settings.crossings do
   str := str + IntToStr(n) + ',';

 if (str <> '') then
   ini_tech.WriteString(section, 'prejezdy', str);
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkSummary.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject;
                              rights: TORCOntrolRights): string;
var prjid: Integer;
    crossing: TBlk;
begin
 Result := inherited;

 for prjid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(prjid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     Result := Result + crossing.name + ','
   else
     Result := Result + '#???,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.PanelClick(SenderPnl: TIdContext; SenderOR: TObject;
                            Button: TPanelButton; rights: TORCOntrolRights;
                            params: string = '');
begin
 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR),
                  Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSummary.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject;
                                item: string; itemindex: Integer);
var crossing: TBlk;
begin
 if (not Self.enabled) then Exit();

 if ((itemindex-2 >= 0) and (itemindex-2 < Self.m_settings.crossings.Count)) then
  begin
   Blky.GetBlkByID(Self.m_settings.crossings[itemindex-2], crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     ORTCPServer.Menu(SenderPnl, crossing, SenderOR as TOR,
                      crossing.ShowPanelMenu(SenderPnl, SenderOR, TORControlRights.write));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSummary.IsCommunication(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := true;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
    begin
     if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.disabled) then
       Exit(false);
    end else Exit(false);
  end;
end;

function TBlkSummary.IsAnnulation(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := false;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     if (TBlkCrossing(crossing).annulation) then
       Exit(true);
  end;
end;

function TBlkSummary.IsPcClosed(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := false;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     if (TBlkCrossing(crossing).pcClosed) then
       Exit(true);
  end;
end;

function TBlkSummary.IsClosed(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := false;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.closed) then
       Exit(true);
  end;
end;

function TBlkSummary.IsError(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := false;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     if (TBlkCrossing(crossing).state = TBlkCrossingBasicState.none) then
       Exit(true);
  end;
end;

function TBlkSummary.IsEmOpen(): Boolean;
var crossingid: Integer;
    crossing: TBlk;
begin
 Result := false;
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     if (TBlkCrossing(crossing).pcEmOpen) then
       Exit(true);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSummary.CreateReferences();
var crossingid: Integer;
    crossing: TBlk;
begin
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     TBlkCrossing(crossing).AddSH(Self);
  end;
end;

procedure TBlkSummary.RemoveReferences();
var crossingid: Integer;
    crossing: TBlk;
begin
 for crossingid in Self.m_settings.crossings do
  begin
   Blky.GetBlkByID(crossingid, crossing);
   if ((crossing <> nil) and (crossing.typ = btCrossing)) then
     TBlkCrossing(crossing).RemoveSH(Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

end.//unit

