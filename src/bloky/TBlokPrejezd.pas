unit TBlokPrejezd;

// definice a obsluha technologickeho bloku Prejezd

interface

uses IniFiles, TBlok, SysUtils, Menus, TOblsRizeni, Classes, TechnologieRCS,
     IdContext, TOblRizeni, Generics.Collections, JsonDataObjects,
     TBlokPrejezdLogic;

type
 TBlkPrjRCSInputs = record
  Zavreno: TRCSAddr;
  Otevreno: TRCSAddr;
  Vystraha: TRCSAddr;
  Anulace: TRCSAddr;
  anulaceUse: Boolean;
 end;

 TBlkPrjRCSOutputs = record
  Zavrit: TRCSAddr;
  NOtevrit: TRCSAddr;
  NOtevritUse: Boolean;
  BlokPoz: TRCSAddr;
  BlokPozUse: Boolean;
 end;

 TBlkPrjSettings = record
  RCSInputs: TBlkPrjRCSInputs;
  RCSOutputs: TBlkPrjRCSOutputs;
 end;

 TBlkPrjBasicStav = (disabled = -5, none = -1, otevreno = 0, vystraha = 1, uzavreno = 2);

 TBlkPrjStav = record
  basicStav: TBlkPrjBasicStav;
  anulaceOld: Boolean;
  stit,vyl:string;
  PC_NOT, PC_UZ: boolean;                           // uzavreni prejezdu z pocitace (tj z technologie), prejezd muze byt uzavren taky z pultu
  zaver:Integer;                                    // pocet bloku, ktere mi daly zaver (pokud > 0, mam zaver; jinak zaver nemam)
  uzavStart:TDateTime;
  shs:TList<TBlk>;                                  // seznam souctovych hlasek, kam hlasi prejezd stav
  rcsModules:TList<Cardinal>;                       // seznam RCS modulu, ktere vyuziva prejezd
 end;

 EPrjNOT = class(Exception);

 TBlkPrejezd = class(TBlk)
  const
   //defaultni stav
   _def_prj_stav:TBlkPrjStav = (
    basicStav : disabled;
    stit : '';
    vyl : '';
    PC_NOT : false;
    PC_UZ : false;
    zaver: 0;
   );


   _UZ_UPOZ_MIN = 4;      // po 4 minutach uzavreneho prejezdu zobrazim upozorneni na uzavreni prilis dlouho

  private
   PrjSettings:TBlkPrjSettings;
   PrjStav:TBlkPrjStav;

    procedure SetStit(stit:string);
    procedure SetVyl(vyl:string);

    function UpdateInputs():TBlkPrjBasicStav;
    procedure UpdateOutputs();
    procedure UpdateTracks();

    procedure SetNOT(state:boolean);
    procedure SetUZ(state:boolean);

    procedure SetZaver(zaver:boolean);
    function GetZaver():boolean;

    function TrackClosed():boolean;
    function TrackPozitiva():boolean;
    function GetAnulace():boolean;

    procedure MenuUZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZUZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuNOTClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZNOTClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuSTITClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure UPOUZClick(Sender:TObject);
    procedure UPOZUZClick(Sender:TObject);
    procedure UPONOTClick(Sender:TObject);
    procedure UPOZNOTClick(Sender:TObject);

    // DEBUG volby:
    procedure MenuAdminZavreno(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminOtevreno(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminVystraha(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminAnulaceStart(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminAnulaceStop(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminNUZClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure SetSimInputs(uzavreno, vystraha, otevreno: Boolean; SenderPnl:TIdContext; SenderOR:TObject);

    procedure StitUPO(SenderPnl:TIdContext; SenderOR:TObject;
        UPO_OKCallback: TNotifyEvent; UPO_EscCallback:TNotifyEvent);
    procedure FillRCSModules();

  public
   tracks: TObjectList<TBlkPrjTrack>;

    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string; ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;
    procedure Change(now:boolean = false); override;

    //----- prejezd own functions -----

    function GetSettings():TBlkPrjSettings;
    procedure SetSettings(data:TBlkPrjSettings);

    procedure AddSH(Sender:TBlk);
    procedure RemoveSH(Sender:TBlk);

    property Stav:TBlkPrjStav read PrjStav;

    property NOtevreni:boolean read PrjStav.PC_NOT write SetNOT;
    property UZ:boolean read PrjStav.PC_UZ write SetUZ;

    property Stitek:string read PrjStav.stit write SetStit;
    property Vyluka:string read PrjStav.vyl write SetVyl;

    property Zaver:boolean read GetZaver write SetZaver;
    property Anulace:boolean read GetAnulace;

    //GUI:
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    procedure PanelZUZCallBack(Sender:TIdContext; success:boolean);
    procedure PanelZNOTCallBack(Sender:TIdContext; success:boolean);

    procedure GetPtData(json: TJsonObject; includeState: boolean); override;
    procedure GetPtState(json: TJsonObject); override;
 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, GetSystems, ownStrUtils, TJCDatabase, TCPServerOR, RCS, UPO,
     Graphics, TCPORsRef, Diagnostics, appEv, ownConvert;

constructor TBlkPrejezd.Create(index:Integer);
begin
 inherited;

 Self.GlobalSettings.typ := btPrejezd;
 Self.PrjStav := Self._def_prj_stav;
 Self.PrjStav.shs := TList<TBlk>.Create();
 Self.PrjStav.rcsModules := TList<Cardinal>.Create();
 Self.tracks := TObjectList<TBlkPrjTrack>.Create();
end;//ctor

destructor TBlkPrejezd.Destroy();
begin
 Self.PrjStav.shs.Free();
 Self.PrjStav.rcsModules.Free();
 Self.tracks.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
var oblr:TOR;
    defaultModule: Integer;
    module:Cardinal;
    i, notracks: Integer;
    track: TBlkPrjTrack;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.PrjStav.Stit := '';
 Self.PrjStav.Vyl  := '';

 defaultModule := ini_tech.ReadInteger(section, 'RCS', -1); // old file specs for backward compatibility

 Self.PrjSettings.RCSInputs.Zavreno := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSIzm', defaultModule),
                                                    ini_tech.ReadInteger(section, 'RCSIz', 0));
 Self.PrjSettings.RCSInputs.Otevreno := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSIom', defaultModule),
                                                     ini_tech.ReadInteger(section, 'RCSIo', 0));
 Self.PrjSettings.RCSInputs.Vystraha := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSIvm', defaultModule),
                                                     ini_tech.ReadInteger(section, 'RCSIv', 0));
 Self.PrjSettings.RCSInputs.anulaceUse := (ini_tech.ReadInteger(section, 'RCSam', defaultModule) <> -1);
 if (Self.PrjSettings.RCSInputs.anulaceUse) then
  begin
   Self.PrjSettings.RCSInputs.Anulace := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSam', defaultModule),
                                                      ini_tech.ReadInteger(section, 'RCSa', 0));
  end;

 Self.PrjSettings.RCSOutputs.Zavrit := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSOzm', defaultModule),
                                                    ini_tech.ReadInteger(section, 'RCSOz', 0));
 Self.PrjSettings.RCSOutputs.NOtevritUse := (ini_tech.ReadInteger(section, 'RCSOnotm', defaultModule) <> -1);
 if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
  begin
   Self.PrjSettings.RCSOutputs.NOtevrit := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSOnotm', defaultModule),
                                                        ini_tech.ReadInteger(section, 'RCSOnot', 0));
  end;

 Self.PrjSettings.RCSOutputs.BlokPozUse := (ini_tech.ReadInteger(section, 'RCSObpm', defaultModule) <> -1);
 if (Self.PrjSettings.RCSOutputs.BlokPozUse) then
  begin
   Self.PrjSettings.RCSOutputs.BlokPoz := RCSi.RCSAddr(ini_tech.ReadInteger(section, 'RCSObpm', defaultModule),
                                                       ini_tech.ReadInteger(section, 'RCSObp', 0));
  end;

 Self.tracks.Clear();
 notracks := ini_tech.ReadInteger(section, 'tracks', 0);
 for i := 0 to notracks-1 do
  begin
   track := TBlkPrjTrack.Create();
   try
     track.Load(ini_tech, section, 'T'+IntToStr(i));
     Self.tracks.Add(track);
   except
     on E:Exception do
       AppEvents.LogException(E, 'LoadTracks');
   end;
  end;

 Self.PrjStav.stit := ini_stat.ReadString(section, 'stit', '');

 Self.LoadORs(ini_rel, 'PRJ').Free();

 Self.FillRCSModules();
 for module in Self.PrjStav.rcsModules do
   RCSi.SetNeeded(module);
 for oblr in Self.ORsRef do
   for module in Self.PrjStav.rcsModules do
     oblr.RCSAdd(module);
end;

procedure TBlkPrejezd.SaveData(ini_tech:TMemIniFile;const section:string);
var i: Integer;
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'RCSIzm', Self.PrjSettings.RCSInputs.Zavreno.board);
 ini_tech.WriteInteger(section, 'RCSIz', Self.PrjSettings.RCSInputs.Zavreno.port);
 ini_tech.WriteInteger(section, 'RCSIom', Self.PrjSettings.RCSInputs.Otevreno.board);
 ini_tech.WriteInteger(section, 'RCSIo', Self.PrjSettings.RCSInputs.Otevreno.port);
 ini_tech.WriteInteger(section, 'RCSIvm', Self.PrjSettings.RCSInputs.Vystraha.board);
 ini_tech.WriteInteger(section, 'RCSIv', Self.PrjSettings.RCSInputs.Vystraha.port);
 if (Self.PrjSettings.RCSInputs.anulaceUse) then
  begin
   ini_tech.WriteInteger(section, 'RCSam', Self.PrjSettings.RCSInputs.Anulace.board);
   ini_tech.WriteInteger(section, 'RCSa', Self.PrjSettings.RCSInputs.Anulace.port);
  end;

 ini_tech.WriteInteger(section, 'RCSOzm', Self.PrjSettings.RCSOutputs.Zavrit.board);
 ini_tech.WriteInteger(section, 'RCSOz', Self.PrjSettings.RCSOutputs.Zavrit.port);
 if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
  begin
   ini_tech.WriteInteger(section, 'RCSOnotm', Self.PrjSettings.RCSOutputs.NOtevrit.board);
   ini_tech.WriteInteger(section, 'RCSOnot', Self.PrjSettings.RCSOutputs.NOtevrit.port);
  end;
 if (Self.PrjSettings.RCSOutputs.BlokPozUse) then
  begin
   ini_tech.WriteInteger(section, 'RCSObpm', Self.PrjSettings.RCSOutputs.BlokPoz.board);
   ini_tech.WriteInteger(section, 'RCSObp', Self.PrjSettings.RCSOutputs.BlokPoz.port);
  end;

 if (Self.tracks.Count > 0) then
   ini_tech.WriteInteger(section, 'tracks', Self.tracks.Count);
 for i := 0 to Self.tracks.Count-1 do
   Self.tracks[i].Save(ini_tech, section, 'T'+IntToStr(i));
end;

procedure TBlkPrejezd.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 if (Self.PrjStav.stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.PrjStav.stit);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.Enable();
var module:Cardinal;
begin
 try
   for module in Self.PrjStav.rcsModules do
     if (not RCSi.IsNonFailedModule(module)) then
       Exit();
 except
   Exit();
 end;

 Self.PrjStav.basicStav := TBlkPrjBasicStav.none;
 Self.PrjStav.anulaceOld := Self.Anulace;
 Self.Change();
end;

procedure TBlkPrejezd.Disable();
begin
 Self.PrjStav.basicStav := disabled;
 Self.PrjStav.shs.Clear();
 Self.Change(true);
end;

function TBlkPrejezd.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := (((portType = TRCSIOType.input) and ((addr = Self.PrjSettings.RCSInputs.Zavreno) or
                                                (addr = Self.PrjSettings.RCSInputs.Otevreno) or
                                                (addr = Self.PrjSettings.RCSInputs.Vystraha)))
            or
            ((portType = TRCSIOType.output) and (addr = Self.PrjSettings.RCSOutputs.Zavrit))
           );

 if (Self.PrjSettings.RCSInputs.anulaceUse) then
   Result := Result or (portType = TRCSIOType.input) and (addr = Self.PrjSettings.RCSInputs.Anulace);
 if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
   Result := Result or (portType = TRCSIOType.output) and (addr = Self.PrjSettings.RCSOutputs.NOtevrit);
 if (Self.PrjSettings.RCSOutputs.BlokPozUse) then
   Result := Result or (portType = TRCSIOType.output) and (addr = Self.PrjSettings.RCSOutputs.BlokPoz);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.Update();
var new_stav:TBlkPrjBasicStav;
    available:boolean;
    oblr:TOR;
    module:Cardinal;
begin
 available := true;
 try
   for module in Self.PrjStav.rcsModules do
     available := available and RCSi.IsNonFailedModule(module);
 except
   available := false;
 end;

 if ((not available) and (Self.PrjStav.basicStav <> TBlkPrjBasicStav.disabled)) then
  begin
   Self.PrjStav.basicStav := TBlkPrjBasicStav.disabled;
   JCDb.RusJC(Self);
   Self.Change(true);
  end;

 new_stav := Self.UpdateInputs();

 if ((Self.PrjStav.basicStav <> new_stav) and
     ((Self.PrjStav.basicStav <> TBlkPrjBasicStav.disabled) or (new_stav <> TBlkPrjBasicStav.none))) then
  begin
   // kontrola necekaneho otevreni prejezdu, pokud je v JC
   // necekaniy stav = prejezd je pod zaverem a na vstupu se objevi cokoliv jineho, nez "uzavreno"
   if ((Self.Zaver) and (Self.PrjStav.basicStav = TBlkPrjBasicStav.uzavreno)) then
    begin
     for oblr in Self.OblsRizeni do
      oblr.BlkWriteError(Self, 'Ztráta dohledu na přejezdu : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
     JCDb.RusJC(Self);
    end;

   if ((new_stav = TBlkPrjBasicStav.none) and (Self.PrjStav.basicStav <> TBlkPrjBasicStav.disabled)) then
    begin
     for oblr in Self.OblsRizeni do
      oblr.BlkWriteError(Self, 'Porucha přejezdu : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
     JCDb.RusJC(Self);
    end;

   if (Self.PrjStav.basicStav = disabled) then
    begin
     // wake-up
     Self.UpdateOutputs();
    end;

   Self.PrjStav.basicStav := new_stav;
   Self.Change();
  end;

 if (Self.PrjStav.anulaceOld <> Self.Anulace) then
  begin
   Self.PrjStav.anulaceOld := Self.Anulace;
   Self.Change();
  end;

 // kontrola prilis dlouho uzavreneho prejezdu
 if ((Self.Zaver) or (Self.PrjStav.PC_UZ)) then
  begin
   if (Now > Self.PrjStav.uzavStart+EncodeTime(0, _UZ_UPOZ_MIN, 0, 0)) then
    begin
     for oblr in Self.OblsRizeni do
      oblr.BlkWriteError(Self, Self.GlobalSettings.name+' uzavřen déle, jak '+IntToStr(_UZ_UPOZ_MIN)+' min', 'VAROVÁNÍ');
     Self.PrjStav.uzavStart := now;
    end;
  end;

 Self.UpdateTracks();

 inherited Update();
end;

procedure TBlkPrejezd.UpdateTracks();
var track: TBlkPrjTrack;
    changed: boolean;
begin
 changed := false;
 for track in Self.tracks do
  begin
   track.Update();
   if (track.stateChanged) then
    begin
     changed := true;
     track.stateChanged := false;
    end;
  end;

 if (changed) then
  begin
   Self.UpdateOutputs();
   if (diag.showZaver) then
     Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.Change(now:boolean = false);
var sh:TBlk;
begin
 inherited;

 try
   for sh in Self.Stav.shs do
     sh.Change();
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.UpdateInputs():TBlkPrjBasicStav;
var tmpInputs: record
      Zavreno:Boolean;
      Otevreno:Boolean;
      Vystraha:Boolean;
    end;
begin
 // get data from RCS
 try
   tmpInputs.Zavreno  := (RCSi.GetInput(Self.PrjSettings.RCSInputs.Zavreno) = isOn);
   tmpInputs.Otevreno := (RCSi.GetInput(Self.PrjSettings.RCSInputs.Otevreno) = isOn);
   tmpInputs.Vystraha := (RCSi.GetInput(Self.PrjSettings.RCSInputs.Vystraha) = isOn);
 except
   // prejezd prejde do poruchoveho stavu
   tmpInputs.Zavreno  := false;
   tmpInputs.Otevreno := false;
   tmpInputs.Vystraha := false;
 end;

 if (tmpInputs.Zavreno)  then Exit(TBlkPrjBasicStav.uzavreno);
 if (tmpInputs.Vystraha) then Exit(TBlkPrjBasicStav.vystraha);
 if (tmpInputs.Otevreno) then Exit(TBlkPrjBasicStav.otevreno);

 // without data
 Result := none;
end;

procedure TBlkPrejezd.UpdateOutputs();
begin
 try
   RCSi.SetOutput(
     Self.PrjSettings.RCSOutputs.Zavrit,
     ownConvert.BoolToInt(((Self.PrjStav.PC_UZ) or (Self.Zaver) or (Self.TrackClosed)) and (not Self.PrjStav.PC_NOT))
   );

   if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
     RCSi.SetOutput(Self.PrjSettings.RCSOutputs.NOtevrit, ownConvert.BoolToInt(Self.PrjStav.PC_NOT));
   if (Self.PrjSettings.RCSOutputs.BlokPozUse) then
     RCSi.SetOutput(Self.PrjSettings.RCSOutputs.BlokPoz, ownConvert.BoolToInt(not Self.TrackPozitiva));
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.GetSettings():TBlkPrjSettings;
begin
 Result := Self.PrjSettings;
end;

procedure TBlkPrejezd.SetSettings(data:TBlkPrjSettings);
begin
 Self.PrjSettings := data;
 Self.FillRCSModules();
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.SetStit(stit:string);
begin
 Self.PrjStav.stit := Stit;
 Self.Change();
end;

procedure TBlkPrejezd.SetVyl(vyl:string);
begin
 Self.PrjStav.vyl := vyl;
 Self.Change();
 Self.UpdateOutputs();
end;

procedure TBlkPrejezd.SetNOT(state:boolean);
begin
 if ((Self.Zaver) and (state)) then Exit();

 if (state) then
  begin
   // NOT rusi jizdni cesty vedouci pres prejezd
   JCDb.RusJC(Self);
  end;

 Self.PrjStav.PC_NOT := state;
 Self.Change();
 Self.UpdateOutputs();
end;

procedure TBlkPrejezd.SetUZ(state:boolean);
begin
 if (state) then
  begin
   if (Self.NOtevreni) then
     raise EPrjNot.Create('Prejezd nouzove otevren, nelze uzavrit!');
   Self.PrjStav.uzavStart := now;
  end;

 Self.PrjStav.PC_UZ := state;
 Self.Change();
 Self.UpdateOutputs();
end;

////////////////////////////////////////////////////////////////////////////////
//gui: menu
//dynamicke funkce

procedure TBlkPrejezd.MenuUZClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Stitek <> '') then
   Self.StitUPO(SenderPnl, SenderOR, Self.UPOUZClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOUZClick(SenderPnl);
 end;
end;

procedure TBlkPrejezd.MenuZUZClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Stitek <> '') then
   Self.StitUPO(SenderPnl, SenderOR, Self.UPOZUZClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOZUZClick(SenderPnl);
 end;
end;

procedure TBlkPrejezd.MenuNOTClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Stitek <> '') then
   Self.StitUPO(SenderPnl, SenderOR, Self.UPONOTClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPONOTClick(SenderPnl);
 end;
end;

procedure TBlkPrejezd.MenuZNOTClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if (Self.Stitek <> '') then
   Self.StitUPO(SenderPnl, SenderOR, Self.UPOZNOTClick, nil)
 else begin
   TTCPOrsRef(SenderPnl.Data).UPO_ref := SenderOR;
   Self.UPOZNOTClick(SenderPnl);
 end;
end;

procedure TBlkPrejezd.UPOUZClick(Sender:TObject);
begin
 Self.UZ := true;
end;

procedure TBlkPrejezd.UPOZUZClick(Sender:TObject);
begin
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelZUZCallBack,
    (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
    'Zrušení uzavření přejezdu', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkPrejezd.UPONOTClick(Sender:TObject);
begin
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelZNOTCallBack,
    (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
    'Nouzové otevření přejezdu', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkPrejezd.UPOZNOTClick(Sender:TObject);
begin
 Self.NOtevreni := false;
end;

procedure TBlkPrejezd.MenuSTITClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;

procedure TBlkPrejezd.MenuAdminZavreno(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.SetSimInputs(true, false, false, SenderPnl, SenderOR);
end;

procedure TBlkPrejezd.MenuAdminOtevreno(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.SetSimInputs(false, false, true, SenderPnl, SenderOR);
end;

procedure TBlkPrejezd.MenuAdminVystraha(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.SetSimInputs(false, true, false, SenderPnl, SenderOR);
end;

procedure TBlkPrejezd.MenuAdminAnulaceStart(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.PrjSettings.RCSInputs.Anulace, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkPrejezd.MenuAdminAnulaceStop(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.PrjSettings.RCSInputs.Anulace, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkPrejezd.MenuAdminNUZClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.PrjStav.zaver := 0;
 Self.UpdateOutputs();
 Self.Change();
end;

procedure TBlkPrejezd.SetSimInputs(uzavreno, vystraha, otevreno: Boolean; SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.PrjSettings.RCSInputs.Zavreno, ownConvert.BoolToInt(uzavreno));
   RCSi.SetInput(Self.PrjSettings.RCSInputs.Vystraha, ownConvert.BoolToInt(vystraha));
   RCSi.SetInput(Self.PrjSettings.RCSInputs.Otevreno, ownConvert.BoolToInt(otevreno));
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení RCS vstupů!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkPrejezd.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited;

 if (Self.PrjStav.basicStav <> TBlkPrjBasicStav.disabled) then
  begin
   if (not Self.PrjStav.PC_NOT) then
    begin
     if (Self.PrjStav.PC_UZ) then
       Result := Result + '!ZUZ,'
      else
       Result := Result + 'UZ,';
    end;

   if (not Self.Zaver) then
    begin
     if (not Self.PrjStav.PC_UZ) then
      begin
       if (Self.PrjStav.PC_NOT) then
         Result := Result + 'NOT<,'
        else
         Result := Result + '!NOT>,';
      end;
    end;
  end;

 Result := Result + 'STIT,';

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (RCSi.simulation) then
  begin
   Result := Result + '-,*ZAVŘENO,*OTEVŘENO,*VÝSTRAHA,';
   if (Self.PrjSettings.RCSInputs.anulaceUse) then
    begin
     if (RCSi.GetInput(Self.PrjSettings.RCSInputs.Anulace) = TRCSInputState.isOn) then
       Result := Result + '*ANULACE<'
     else
       Result := Result + '*ANULACE>';
    end;
  end;

 if (rights >= TORControlRights.superuser) then
  begin
   Result := Result + '-,';
   if (Self.Zaver) then Result := Result + '*NUZ>,';
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Button <> TPanelButton.ESCAPE) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkPrejezd.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if      (item = 'UZ')       then Self.MenuUZClick  (SenderPnl, SenderOR)
 else if (item = 'ZUZ')      then Self.MenuZUZClick (SenderPnl, SenderOR)
 else if (item = 'NOT>')     then Self.MenuNOTClick (SenderPnl, SenderOR)
 else if (item = 'NOT<')     then Self.MenuZNOTClick(SenderPnl, SenderOR)
 else if (item = 'STIT')     then Self.MenuSTITClick (SenderPnl, SenderOR)
 else if (item = 'NUZ>')     then Self.MenuAdminNUZClick(SenderPnl, SenderOR)
 else if (item = 'ZAVŘENO')  then Self.MenuAdminZavreno(SenderPnl, SenderOR)
 else if (item = 'OTEVŘENO') then Self.MenuAdminOtevreno(SenderPnl, SenderOR)
 else if (item = 'VÝSTRAHA') then Self.MenuAdminVystraha(SenderPnl, SenderOR)
 else if (item = 'ANULACE>') then Self.MenuAdminAnulaceStart(SenderPnl, SenderOR)
 else if (item = 'ANULACE<') then Self.MenuAdminAnulaceStop(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

//zavola se pri uspesnem zvladnuti potvrzovaci sekvence
procedure TBlkPrejezd.PanelZNOTCallBack(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.Notevreni := true;
end;

procedure TBlkPrejezd.PanelZUZCallBack(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.UZ := false; 
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.SetZaver(zaver:boolean);
begin
 if (zaver) then
  begin
   Inc(Self.PrjStav.zaver);

   if (Self.NOtevreni) then
     raise EPrjNot.Create('Prejezd nouzove otevren, nelze udelit zaver!');

   if (Self.PrjStav.zaver = 1) then
    begin
     // prvni udeleni zaveru
     Self.PrjStav.uzavStart := now;
     Self.SetNOT(false);

     Self.UpdateOutputs();
     Self.Change();
    end;
  end else begin
   Dec(Self.PrjStav.zaver);

   if (Self.PrjStav.zaver <= 0) then
    begin
     // posledni odstraneni zaveru
     Self.UpdateOutputs();
     Self.Change();
    end;
  end;
end;

function TBlkPrejezd.GetZaver():boolean;
begin
 Result := (Self.PrjStav.zaver > 0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.StitUPO(SenderPnl:TIdContext; SenderOR:TObject;
      UPO_OKCallback: TNotifyEvent; UPO_EscCallback:TNotifyEvent);
var upo:TUPOItems;
    item:TUPOItem;
    lines:TStrings;
begin
 upo := TList<TUPOItem>.Create;
 try
  if (Self.Stitek <> '') then
   begin
    item[0] := GetUPOLine('ŠTÍTEK '+Self.GlobalSettings.name, taCenter, clBlack, clTeal);
    lines := GetLines(Self.Stitek, _UPO_LINE_LEN);

    try
      item[1] := GetUPOLine(lines[0], taLeftJustify, clYellow, $A0A0A0);
      if (lines.Count > 1) then
        item[2] := GetUPOLine(lines[1], taLeftJustify, clYellow, $A0A0A0);
    finally
      lines.Free();
    end;

   upo.Add(item);
  end;

  ORTCPServer.UPO(SenderPnl, upo, false, UPO_OKCallback, UPO_EscCallback, SenderOR);
 finally
   upo.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.AddSH(Sender:TBlk);
begin
 if (not Self.PrjStav.shs.Contains(Sender)) then
   Self.PrjStav.shs.Add(Sender);
end;

procedure TBlkPrejezd.RemoveSH(Sender:TBlk);
begin
 if (Self.PrjStav.shs.Contains(Sender)) then
   Self.PrjStav.shs.Remove(Sender);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.FillRCSModules();
begin
 Self.PrjStav.rcsModules.Clear();

 Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSInputs.Zavreno.board);
 if (not Self.PrjStav.rcsModules.Contains(Self.PrjSettings.RCSInputs.Otevreno.board)) then
   Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSInputs.Otevreno.board);
 if (not Self.PrjStav.rcsModules.Contains(Self.PrjSettings.RCSInputs.Vystraha.board)) then
   Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSInputs.Vystraha.board);
 if (Self.PrjSettings.RCSInputs.anulaceUse) then
   if (not Self.PrjStav.rcsModules.Contains(Self.PrjSettings.RCSInputs.Anulace.board)) then
     Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSInputs.Anulace.board);
 if (not Self.PrjStav.rcsModules.Contains(Self.PrjSettings.RCSOutputs.Zavrit.board)) then
   Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSOutputs.Zavrit.board);
 if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
   if (not Self.PrjStav.rcsModules.Contains(Self.PrjSettings.RCSOutputs.NOtevrit.board)) then
     Self.PrjStav.rcsModules.Add(Self.PrjSettings.RCSOutputs.NOtevrit.board);

 Self.PrjStav.rcsModules.Sort();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.PanelStateString():string;
var fg, bg: TColor;
begin
 Result := inherited;

 bg := clBlack;
 if (Self.Stitek <> '') then bg := clTeal;

 if (diag.showZaver) then
  begin
   if (Self.Zaver) then
     bg := clGreen
   else if (Self.TrackClosed()) then
     bg := clBlue;
  end;

 if (Self.NOtevreni) then fg := clRed
 else if (Self.UZ) then fg := clWhite
 else fg := $A0A0A0;

 case (Self.Stav.basicStav) of
   TBlkPrjBasicStav.disabled : begin
     fg := bg;
     bg := clFuchsia;
   end;

   TBlkPrjBasicStav.none : begin
     fg := bg;
     bg := clRed;
   end;
 end;

 Result := Result + ownConvert.ColorToStr(fg) + ';' +
                    ownConvert.ColorToStr(bg) + ';0;' +
                    IntToStr(Integer(Self.Stav.basicStav)) + ';';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.GetPtData(json: TJsonObject; includeState: boolean);
begin
 inherited;

 TBlk.RCStoJSON(Self.PrjSettings.RCSInputs.Zavreno, json['rcs']['zavreno']);
 TBlk.RCStoJSON(Self.PrjSettings.RCSInputs.Otevreno, json['rcs']['otevreno']);
 TBlk.RCStoJSON(Self.PrjSettings.RCSInputs.Vystraha, json['rcs']['vystraha']);
 if (Self.PrjSettings.RCSInputs.anulaceUse) then
   TBlk.RCStoJSON(Self.PrjSettings.RCSInputs.Anulace, json['rcs']['anulace']);
 TBlk.RCStoJSON(Self.PrjSettings.RCSOutputs.Zavrit, json['rcs']['zavrit']);
 if (Self.PrjSettings.RCSOutputs.NOtevritUse) then
   TBlk.RCStoJSON(Self.PrjSettings.RCSOutputs.NOtevrit, json['rcs']['notevrit']);

 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkPrejezd.GetPtState(json: TJsonObject);
begin
 case (Self.PrjStav.basicStav) of
   TBlkPrjBasicStav.disabled: json['stav'] := 'disabled';
   TBlkPrjBasicStav.none: json['stav'] := 'none';
   TBlkPrjBasicStav.otevreno: json['stav'] := 'otevreno';
   TBlkPrjBasicStav.vystraha: json['stav'] := 'vystraha';
   TBlkPrjBasicStav.uzavreno: json['stav'] := 'uzavreno';
 end;

 json['anulace'] := Self.anulace;
 json['stit'] := Self.PrjStav.stit;
 json['vyl'] := Self.PrjStav.vyl;
 json['PC_NOT'] := Self.PrjStav.PC_NOT;
 json['PC_UZ'] := Self.PrjStav.PC_UZ;
 json['zaver'] := Self.PrjStav.zaver;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.TrackClosed():boolean;
var track: TBlkPrjTrack;
begin
 for track in Self.tracks do
   if (track.shouldBeClosed) then
     Exit(true);
 Result := false;
end;

function TBlkPrejezd.TrackPozitiva():boolean;
var track: TBlkPrjTrack;
begin
 for track in Self.tracks do
   if (not track.pozitiva) then
     Exit(false);
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.GetAnulace():boolean;
var track: TBlkPrjTrack;
begin
 Result := false;
 if (Self.PrjSettings.RCSInputs.anulaceUse and RCSi.Started) then
  begin
   try
     Result := (RCSi.GetInput(Self.PrjSettings.RCSInputs.Anulace) = isOn);
   except

   end;
  end;

 if (not Result) then
   for track in Self.tracks do
     if (track.anullation) then
       Exit(true);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

