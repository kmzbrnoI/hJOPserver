unit TBlokSouctovaHlaska;

//definice a obsluha technologickeho bloku Souctova hlaska

interface

uses IniFiles, TBlok, Menus, SysUtils, Classes, IdContext, Generics.Collections,
     TOblRizeni, TCPServerOR;

type
 TBlkSHStav = record
  enabled: Boolean;
 end;

 TBlkSHSettings = record
  prejezdy: TList<Integer>; // seznam id prejezdu, ktere jsou v souctove hlasce
 end;

 TBlkSH = class(TBlk)
  const
   _def_sh_stav: TBlkSHStav = (
     enabled: false;
   );

  protected
   settings: TBlkSHSettings;
   shStav: TBlkSHStav;

    function GetKomunikace(): Boolean;
    function GetAnulace(): Boolean;
    function GetUZ(): Boolean;
    function GetUzavreno(): Boolean;
    function GetPorucha(): Boolean;
    function GetNOT(): Boolean;

    procedure CreateReferences();
    procedure RemoveReferences();

  public

    constructor Create(index: Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech: TMemIniFile; const section: string;
                       ini_rel, ini_stat: TMemIniFile); override;
    procedure SaveData(ini_tech: TMemIniFile; const section: string); override;

    procedure Enable(); override;
    procedure Disable(); override;

    function GetSettings(): TBlkSHSettings;
    procedure SetSettings(data: TBlkSHSettings);

    //----- souctova hlaska own functions -----

    property stav: TBlkSHStav read shStav;
    property enabled: Boolean read shStav.enabled;

    property komunikace: Boolean read GetKomunikace;
    property anulace: Boolean read GetAnulace;
    property UZ: Boolean read GetUZ;
    property uzavreno: Boolean read GetUzavreno;
    property porucha: Boolean read GetPorucha;
    property nouzoveOT: Boolean read GetNOT;

    //GUI:
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

uses TBlokPrejezd, TBloky, TOblsRizeni, Graphics, ownConvert;

constructor TBlkSH.Create(index: Integer);
begin
 inherited;

 Self.shStav := Self._def_sh_stav;
 Self.settings.prejezdy := TList<Integer>.Create();
 Self.GlobalSettings.typ := btSH;
end;

destructor TBlkSH.Destroy();
begin
 Self.settings.prejezdy.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSH.LoadData(ini_tech: TMemIniFile; const section: string;
                          ini_rel, ini_stat: TMemIniFile);
var data: TStrings;
    str: string;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.settings.prejezdy.Clear();
 data := TStringList.Create();
 try
   ExtractStrings([','], [], PChar(ini_tech.ReadString(section, 'prejezdy', '')), data);

   for str in data do
     Self.settings.prejezdy.Add(StrToInt(str));

   Self.LoadORs(ini_rel, 'T').Free();
 finally
   data.Free();
 end;
end;

procedure TBlkSH.SaveData(ini_tech: TMemIniFile; const section: string);
var str: string;
    n: Integer;
begin
 inherited;

 str := '';
 for n in Self.settings.prejezdy do
   str := str + IntToStr(n) + ',';

 if (str <> '') then
   ini_tech.WriteString(section, 'prejezdy', str);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSH.Enable();
begin
 Self.shStav.enabled := true;
 Self.CreateReferences();
end;

procedure TBlkSH.Disable();
begin
 Self.shStav.enabled := false;
 Self.Change(true);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSH.GetSettings(): TBlkSHSettings;
begin
 Result := Self.settings;
end;

procedure TBlkSH.SetSettings(data: TBlkSHSettings);
begin
 if (Self.enabled) then
   Self.RemoveReferences();
 Self.settings.prejezdy.Free();

 Self.settings := data;

 if (Self.enabled) then
   Self.CreateReferences();

 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkSH.ShowPanelMenu(SenderPnl: TIdContext; SenderOR: TObject;
                              rights: TORCOntrolRights): string;
var prjid: Integer;
    prj: TBlk;
begin
 Result := inherited;

 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     Result := Result + prj.name + ','
   else
     Result := Result + '#???,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSH.PanelClick(SenderPnl: TIdContext; SenderOR: TObject;
                            Button: TPanelButton; rights: TORCOntrolRights;
                            params: string = '');
begin
 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR),
                  Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkSH.PanelMenuClick(SenderPnl: TIdContext; SenderOR: TObject;
                                item: string; itemindex: Integer);
var prj: TBlk;
begin
 if (not Self.enabled) then Exit();

 if ((itemindex-2 >= 0) and (itemindex-2 < Self.settings.prejezdy.Count)) then
  begin
   Blky.GetBlkByID(Self.settings.prejezdy[itemindex-2], prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     ORTCPServer.Menu(SenderPnl, prj, SenderOR as TOR,
                      prj.ShowPanelMenu(SenderPnl, SenderOR, TORControlRights.write));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSH.GetKomunikace(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := true;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
    begin
     if (TBlkPrejezd(prj).Stav.basicStav = TBlkPrjBasicStav.disabled) then
       Exit(false);
    end else Exit(false);
  end;
end;

function TBlkSH.GetAnulace(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := false;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     if (TBlkPrejezd(prj).Anulace) then
       Exit(true);
  end;
end;

function TBlkSH.GetUZ(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := false;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     if (TBlkPrejezd(prj).UZ) then
       Exit(true);
  end;
end;

function TBlkSH.GetUzavreno(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := false;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     if (TBlkPrejezd(prj).Stav.basicStav = TBlkPrjBasicStav.uzavreno) then
       Exit(true);
  end;
end;

function TBlkSH.GetPorucha(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := false;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     if (TBlkPrejezd(prj).Stav.basicStav = TBlkPrjBasicStav.none) then
       Exit(true);
  end;
end;

function TBlkSH.GetNOT(): Boolean;
var prjid: Integer;
    prj: TBlk;
begin
 Result := false;
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     if (TBlkPrejezd(prj).NOtevreni) then
       Exit(true);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkSH.CreateReferences();
var prjid: Integer;
    prj: TBlk;
begin
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     TBlkPrejezd(prj).AddSH(Self);
  end;
end;

procedure TBlkSH.RemoveReferences();
var prjid: Integer;
    prj: TBlk;
begin
 for prjid in Self.settings.prejezdy do
  begin
   Blky.GetBlkByID(prjid, prj);
   if ((prj <> nil) and (prj.typ = btPrejezd)) then
     TBlkPrejezd(prj).RemoveSH(Self);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkSH.PanelStateString(): string;
var fg: TColor;
begin
 Result := inherited;

 // n.o. rail
 fg := clTeal;
 if (Self.anulace) then
   fg := clWhite;
 Result := Result + ownConvert.ColorToStr(fg) + ';';
 Result := Result + ownConvert.ColorToStr(clBlack) + ';0;';

 // left rectangle
 fg := clGreen;
 if ((Self.porucha) or (Self.nouzoveOT)) then
   fg := clRed;
 if (not Self.komunikace) then
   fg := clFuchsia;
 Result := Result + ownConvert.ColorToStr(fg) + ';';

 // right rectangle
 fg := clBlack;
 if (Self.uzavreno) then
   fg := $A0A0A0;
 if (Self.UZ) then
   fg := clWhite;
 Result := Result + ownConvert.ColorToStr(fg) + ';';
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

