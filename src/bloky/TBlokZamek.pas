unit TBlokZamek;

//definice a obsluha technologickeho bloku Zamek

interface

uses IniFiles, TBlok, Menus, TOblsRizeni, SysUtils, Classes, IdContext,
     Generics.Collections, TOblRizeni, JsonDataObjects;

type

 TBlkZamekStav = record
  enabled:boolean;
  klicUvolnen:boolean;
  nouzZaver:Cardinal;    // tady je ulozeny pocet bloku, ktere mi daly nouzovy zaver
  Zaver:Integer;        // tady je ulozeny pocet bloku, ktere mi daly zaver
  stit:string;
  porucha:boolean;
 end;

 // zamek ma zaver, pokud jakakoliv vyhybka, kterou obsluhuje, ma zaver

 TBlkZamek = class(TBlk)
  const
   //defaultni stav
   _def_zamek_stav:TBlkZamekStav = (
    enabled : false;
    klicUvolnen : false;
    nouzZaver : 0;
    zaver : 0;
    stit : '';
    porucha : false;
   );


  private
   ZamekStav:TBlkZamekStav;
   last_zaver:boolean;      // tady je ulozena posledni hodnota zaveru (aby mohlo byt rozponano, kdy volat Change)

    procedure MenuUKClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZUKClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTITClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVEnableClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVDisableClick(SenderPnl:TIdContext; SenderOR:TObject);

    function GetZaver():boolean;
    function GetNouzZaver():boolean;
    function IsRightPoloha():boolean;   // vraci true, pokud jsou vyhybky s timto zamkem v poloze pro zamknuti

    procedure SetNouzZaver(new:boolean);
    procedure SetStit(stit:string);
    procedure SetPorucha(new:boolean);
    procedure SetZaver(new:boolean);

    procedure SetKlicUvolnen(new:boolean);
    procedure PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);

    procedure CallChangeToVyh();

  public

    constructor Create(index:Integer);

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


    //----- zamek own functions -----

    procedure DecreaseNouzZaver(amount:Cardinal);

    property Stav:TBlkZamekStav read ZamekStav;
    property Zaver:boolean read GetZaver write SetZaver;
    property nouzZaver:boolean read GetNouzZaver write SetNouzZaver;
    property stitek:string read ZamekStav.stit write SetStit;
    property klicUvolnen:boolean read ZamekStav.klicUvolnen write SetKlicUvolnen;
    property porucha:boolean read ZamekStav.porucha write SetPorucha;

    //GUI:
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    procedure GetPtData(json: TJsonObject; includeState: boolean); override;
    procedure GetPtState(json: TJsonObject); override;

 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TBloky, Graphics, Diagnostics, ownConvert,
    TJCDatabase, fMain, TCPServerOR, SprDb, THVDatabase, TBlokVyhybka;

constructor TBlkZamek.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := btZamek;
 Self.ZamekStav := _def_zamek_stav;
 Self.last_zaver := false;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);
 Self.ZamekStav.Stit := ini_stat.ReadString(section, 'stit', '');
 Self.LoadORs(ini_rel, 'Z').Free();
end;

procedure TBlkZamek.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);
end;

procedure TBlkZamek.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 ini_stat.WriteString (section, 'stit', Self.ZamekStav.Stit);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.Enable();
begin
 Self.ZamekStav.enabled := true;

 // nezamykat zamek tady
 // zamek se zamyka v Disable(), tady se totiz muze stat, ze uz ho nejaka vyhybka
 // nouzove odemkla (vyhybka, ktere Enable() se vola driv)

 inherited Change();
end;

procedure TBlkZamek.Disable();
begin
 Self.ZamekStav.enabled     := false;
 Self.ZamekStav.klicUvolnen := false;
 Self.ZamekStav.nouzZaver   := 0;
 Self.ZamekStav.porucha     := false;

 Self.Change(true);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.Update();
begin
 inherited Update();
end;

// change je volan z vyhybky pri zmene zaveru
procedure TBlkZamek.Change(now:boolean = false);
begin
 // porucha zamku -> zrusit postavenou JC
 if ((Self.Zaver) and ((Self.klicUvolnen) or (Self.porucha))) then
   JCDb.RusJC(Self);

 inherited Change(now);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.MenuUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((not Self.Zaver) and (not Self.nouzZaver)) then
   Self.klicUvolnen := true;
end;

procedure TBlkZamek.MenuZUKClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.klicUvolnen := false;
end;

procedure TBlkZamek.MenuSTITClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;

procedure TBlkZamek.MenuZAVEnableClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.nouzZaver := true;
end;

procedure TBlkZamek.MenuZAVDisableClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, (SenderOR as TOR), 'Zrušení nouzového závěru', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkZamek.PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
begin
 if (success) then
  begin
   Self.ZamekStav.nouzZaver := 0;
   Self.SetNouzZaver(false);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkZamek.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;

 if ((Self.Stav.klicUvolnen) and (Self.IsRightPoloha())) then
   Result := Result + 'ZUK,';

 if ((not Self.Zaver) and (not Self.nouzZaver)) then
   if (not Self.Stav.klicUvolnen) then
     Result := Result + 'UK,';

 if (Self.nouzZaver) then
   Result := Result + '!ZAV<,'
 else
   Result := Result + 'ZAV>,';

 Result := Result + 'STIT,';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Self.Stav.enabled) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkZamek.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (not Self.Stav.enabled) then Exit();

 if      (item = 'UK')   then Self.MenuUKClick(SenderPnl, SenderOR)
 else if (item = 'ZUK')  then Self.MenuZUKClick(SenderPnl, SenderOR)
 else if (item = 'STIT') then Self.MenuSTITClick(SenderPnl, SenderOR)
 else if (item = 'ZAV>') then Self.MenuZAVEnableClick(SenderPnl, SenderOR)
 else if (item = 'ZAV<') then Self.MenuZAVDisableClick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkZamek.GetZaver():boolean;
begin
 Result := (Self.ZamekStav.Zaver > 0);
end;

procedure TBlkZamek.SetZaver(new:boolean);
begin
 if (new) then
  begin
   Inc(Self.ZamekStav.Zaver);
   if (Self.ZamekStav.Zaver = 1) then
     Self.Change();
  end else begin
    if (Self.ZamekStav.Zaver > 0) then
     begin
      Dec(Self.ZamekStav.Zaver);
      if (Self.ZamekStav.Zaver = 0) then
        Self.Change();
     end;
  end;
end;

function TBlkZamek.GetNouzZaver():boolean;
begin
 Result := (Self.Stav.nouzZaver > 0);
end;

procedure TBlkZamek.SetNouzZaver(new:boolean);
begin
 if (new) then
  begin
   Inc(Self.ZamekStav.nouzZaver);
   if (Self.ZamekStav.nouzZaver = 1) then
    begin
     if (Self.klicUvolnen) then
      begin
       Self.ZamekStav.klicUvolnen := false;
       Self.CallChangeToVyh();
      end;
     inherited Change();
    end;
  end else begin
   if (Self.ZamekStav.nouzZaver > 0) then Dec(Self.ZamekStav.nouzZaver);
   if (Self.ZamekStav.nouzZaver = 0) then
    begin
     // pokud vyhybky nejsou ve spravne poloze, automaticky uvolnujeme klic
     if (not Self.IsRightPoloha()) then
       Self.klicUvolnen := true
      else begin
       Self.CallChangeToVyh();
       inherited Change();
      end;
     Blky.NouzZaverZrusen(Self);
    end;
  end;
end;

procedure TBlkZamek.SetStit(stit:string);
begin
 if (stit <> Self.ZamekStav.stit) then
  begin
   Self.ZamekStav.stit := stit;
   inherited Change();
  end;
end;

procedure TBlkZamek.SetKlicUvolnen(new:boolean);
begin
 if (new <> Self.klicUvolnen) then
  begin
   Self.ZamekStav.klicUvolnen := new;
   if (not new) then Self.ZamekStav.porucha := false;
   Self.CallChangeToVyh();
   inherited Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.CallChangeToVyh();
var list:TBlksList;
    i:Integer;
begin
 list := Blky.GetVyhWithZamek(Self.id);

 for i := 0 to list.Count-1 do
  (list[i] as TBlkVyhybka).Change();

 list.Free();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.SetPorucha(new:boolean);
begin
 if (Self.ZamekStav.porucha <> new) then
  begin
   Self.ZamekStav.porucha := new;
   if (new) then                      // vyvolani poruchy vlivem odpadeni vyhybky zpusobi uvolneni klice; VZDYCKY !!
    Self.ZamekStav.klicUvolnen := true;
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkZamek.IsRightPoloha():boolean;
var list:TBlksList;
    i:Integer;
begin
 list := Blky.GetVyhWithZamek(Self.id);

 for i := 0 to list.Count-1 do
   if ((list[i] as TBlkVyhybka).Poloha <> (list[i] as TBlkVyhybka).GetSettings().zamekPoloha) then
    begin
     list.Free();
     Exit(false);
    end;

 list.Free();
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.DecreaseNouzZaver(amount:Cardinal);
begin
 if (Self.ZamekStav.nouzZaver = 0) then Exit();

 if (amount > Self.ZamekStav.nouzZaver) then
   Self.ZamekStav.nouzZaver := 0
 else
   Self.ZamekStav.nouzZaver := Self.ZamekStav.nouzZaver - amount;

 if (not Self.nouzZaver) then
   Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkZamek.PanelStateString():string;
var fg, bg: TColor;
begin
 Result := inherited;

 if (Self.stitek <> '') then bg := clTeal
 else bg := clBlack;

 if ((diag.showZaver) and (Self.Zaver)) then
   bg := clGreen;

 if (Self.porucha) then begin
  fg := bg;
  bg := clBlue;
 end
 else if (Self.klicUvolnen) then fg := clBlue
 else if (Self.nouzZaver) then fg := clAqua
 else fg := $A0A0A0;

 Result := Result + ownConvert.ColorToStr(fg) + ';';
 Result := Result + ownConvert.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkZamek.GetPtData(json: TJsonObject; includeState: boolean);
begin
 inherited;
 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkZamek.GetPtState(json: TJsonObject);
begin
 json['enabled'] := Self.ZamekStav.enabled;
 json['klicUvolnen'] := Self.ZamekStav.klicUvolnen;
 json['nouzZaver'] := Self.ZamekStav.nouzZaver;
 json['zaver'] := Self.ZamekStav.Zaver;
 json['stit'] := Self.ZamekStav.stit;
 json['porucha'] := Self.ZamekStav.porucha;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

