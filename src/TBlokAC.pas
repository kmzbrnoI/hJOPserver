unit TBlokAC;

// definice a obsluha technologickeho bloku AC

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, IdContext,
     Generics.Collections, TOblRizeni;

type

 TBlkACStav = record

 end;

 // zamek ma zaver, pokud jakakoliv vyhybka, kterou obsluhuje, ma zaver

 TBlkAC = class(TBlk)
  const
   //defaultni stav
   _def_ac_stav:TBlkACStav = (

   );

  private
   ACStav:TBlkACStav;

    procedure MenuSTARTClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTOPClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPAUZAClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuPOKRACClick(SenderPnl:TIdContext; SenderOR:TObject);

  public

    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;               // change do zamku je volano pri zmene zaveru jakekoliv vyhybky, kterou zaver obsluhuje

    //----- Ac own functions -----

    property Stav:TBlkACStav read ACStav;

    //GUI:
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieRCS, TBloky, Graphics, Prevody, Diagnostics,
    TJCDatabase, fMain, TCPServerOR, SprDb, THVDatabase, TBlokVyhybka;

constructor TBlkAC.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_AC;
 Self.ACStav := _def_ac_stav;
end;//ctor

destructor TBlkAC.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 // TODO load data here

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();
   try
     ExtractStrings([';'],[],PChar(ini_rel.ReadString('AC', IntToStr(Self.GlobalSettings.id), '')),str);
     if (str.Count < 1) then Exit;
     if (Self.ORsRef <> nil) then
       Self.ORsRef.Free();
     Self.ORsRef := ORs.ParseORs(str[0]);
   finally
     str.Free();
   end;
  end else begin
   Self.ORsRef.Clear();
  end;
end;

procedure TBlkAC.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech, section);
end;

procedure TBlkAC.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 // TODO
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Enable();
begin
 inherited Change();
end;

procedure TBlkAC.Disable();
begin
 Self.Change(true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.Update();
begin
 inherited Update();
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkAC.Change(now:boolean = false);
begin
 inherited Change(now);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.MenuSTARTClick(SenderPnl:TIdContext; SenderOR:TObject);
begin

end;

procedure TBlkAC.MenuSTOPClick(SenderPnl:TIdContext; SenderOR:TObject);
begin

end;

procedure TBlkAC.MenuPAUZAClick(SenderPnl:TIdContext; SenderOR:TObject);
begin

end;

procedure TBlkAC.MenuPOKRACClick(SenderPnl:TIdContext; SenderOR:TObject);
begin

end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkAC.PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
// if (Self.Stav.enabled) then
//   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkAC.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
// if (not Self.Stav.enabled) then Exit();

{ if      (item = 'UK')   then Self.MenuUKClick(SenderPnl, SenderOR)
 else if (item = 'ZUK')  then Self.MenuZUKClick(SenderPnl, SenderOR) }
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkAC.PanelStateString():string;
var fg, bg: TColor;
begin
 Result := inherited;

 bg := clBlack;
 fg := $A0A0A0;

 Result := Result + PrevodySoustav.ColorToStr(fg) + ';';
 Result := Result + PrevodySoustav.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

