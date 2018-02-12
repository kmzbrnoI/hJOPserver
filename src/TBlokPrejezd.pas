unit TBlokPrejezd;

// definice a obsluha technologickeho bloku Prejezd

interface

uses IniFiles, TBlok, TechnologieJC, SysUtils, Menus, TOblsRizeni,
     Classes, TechnologieRCS, IdContext, TOblRizeni, Generics.Collections;

type
 TBlkPrjMTBInputs = record
  Zavreno:Byte;
  Otevreno:Byte;
  Vystraha:Byte;
  Anulace:Byte;
 end;

 TBlkPrjMTBOutputs = record
  Zavrit:Byte;
  NOtevrit:Byte;
 end;

 TBlkPrjSettings = record
  MTB:Byte;
  MTBInputs:TBlkPrjMTBInputs;
  MTBOutputs:TBlkPrjMTBOutputs;
 end;

 TBlkPrjBasicStav = (disabled = -5, none = -1, otevreno = 0, vystraha = 1, uzavreno = 2, anulace = 3);

 TBlkPrjStav = record
  basicStav: TBlkPrjBasicStav;
  stit,vyl:string;
  PC_NOT, PC_UZ: boolean;                           // uzavreni prejezdu z pocitace (tj z technologie), prejezd muze byt uzavren taky z pultu
  zaver:Integer;                                    // pocet bloku, ktere mi daly zaver (pokud > 0, mam zaver; jinak zaver nemam)
  uzavStart:TDateTime;
  shs:TList<TBlk>;                                  // seznam souctovych hlasek, kam hlasi prejezd stav
 end;

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

    procedure SetNOT(state:boolean);
    procedure SetUZ(state:boolean);

    procedure SetZaver(zaver:boolean);
    function GetZaver():boolean;

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
    procedure MenuAdminZAVRENOStartClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminZAVRENOStopClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuAdminNUZClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure StitUPO(SenderPnl:TIdContext; SenderOR:TObject;
        UPO_OKCallback: TNotifyEvent; UPO_EscCallback:TNotifyEvent);

  public
    constructor Create(index:Integer);
    destructor Destroy(); override;

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string; ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;

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

    property Stitek:string read PrjStav.Stit write SetStit;

    property Zaver:boolean read GetZaver write SetZaver;

    //GUI:
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;

    procedure PanelZUZCallBack(Sender:TIdContext; success:boolean);
    procedure PanelZNOTCallBack(Sender:TIdContext; success:boolean);
 end;

////////////////////////////////////////////////////////////////////////////////

implementation

uses TBloky, GetSystems, ownStrUtils, TJCDatabase, TCPServerOR, RCS, UPO,
     Graphics, TBlokSouctovaHlaska;

constructor TBlkPrejezd.Create(index:Integer);
begin
 inherited;

 Self.GlobalSettings.typ := _BLK_PREJEZD;
 Self.PrjStav := Self._def_prj_stav;
 Self.PrjStav.shs := TList<TBlk>.Create();
end;//ctor

destructor TBlkPrejezd.Destroy();
begin
 Self.PrjStav.shs.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.LoadData(ini_tech:TMemIniFile; const section:string; ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
    i:Integer;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.PrjStav.Stit := '';
 Self.PrjStav.Vyl  := '';

 Self.PrjSettings.MTB := ini_tech.ReadInteger(section, 'MTB', 0);

 Self.PrjSettings.MTBInputs.Zavreno   := ini_tech.ReadInteger(section, 'MTBIz', 0);
 Self.PrjSettings.MTBInputs.Otevreno  := ini_tech.ReadInteger(section, 'MTBIo', 0);
 Self.PrjSettings.MTBInputs.Vystraha  := ini_tech.ReadInteger(section, 'MTBIv', 0);
 Self.PrjSettings.MTBInputs.Anulace   := ini_tech.ReadInteger(section, 'MTBa', 0);

 Self.PrjSettings.MTBOutputs.Zavrit   := ini_tech.ReadInteger(section, 'MTBOz', 0);
 Self.PrjSettings.MTBOutputs.NOtevrit := ini_tech.ReadInteger(section, 'MTBOnot', 0);

 Self.PrjStav.stit := ini_stat.ReadString(section, 'stit', '');

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   ExtractStringsEx([';'], [], ini_rel.ReadString('PRJ', IntToStr(Self.GlobalSettings.id), ''), str);
   if (str.Count > 0) then
     Self.ORsRef := ORs.ParseORs(str[0]);

   str.Free();
  end else begin
    Self.ORsRef.Cnt := 0;
  end;

 for i := 0 to Self.ORsRef.Cnt-1 do
  Self.ORsRef.ORs[i].MTBAdd(Self.PrjSettings.MTB);
end;//procedure

procedure TBlkPrejezd.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech, section);

 ini_tech.WriteInteger(section, 'MTB', Self.PrjSettings.MTB);

 ini_tech.WriteInteger(section, 'MTBIz', Self.PrjSettings.MTBInputs.Zavreno);
 ini_tech.WriteInteger(section, 'MTBIo', Self.PrjSettings.MTBInputs.Otevreno);
 ini_tech.WriteInteger(section, 'MTBIv', Self.PrjSettings.MTBInputs.Vystraha);
 ini_tech.WriteInteger(section, 'MTBa', Self.PrjSettings.MTBInputs.Anulace);

 ini_tech.WriteInteger(section, 'MTBOz', Self.PrjSettings.MTBOutputs.Zavrit);
 ini_tech.WriteInteger(section, 'MTBOnot', Self.PrjSettings.MTBOutputs.NOtevrit);
end;//procedure

procedure TBlkPrejezd.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 if (Self.PrjStav.stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.PrjStav.stit);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.Enable();
begin
 try
   if (not RCSi.IsModule(Self.PrjSettings.MTB)) then
    Exit();
 except
   Exit();
 end;

 Self.PrjStav.basicStav := TBlkPrjBasicStav.none;
 Self.Change();
end;//procedure

procedure TBlkPrejezd.Disable();
begin
 Self.PrjStav.basicStav := disabled;
 Self.PrjStav.shs.Clear();
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.Update();
var new_stav:TBlkPrjBasicStav;
    i:Integer;
    available:boolean;
begin
 if (not (GetFunctions.GetSystemStart())) then Exit;

 try
   available := (RCSi.IsModule(Self.PrjSettings.MTB) and
                 (not RCSi.IsModuleFailure(Self.PrjSettings.MTB)));
 except
   available := false;
 end;

 if (not available) then
  begin
   if (Self.PrjStav.basicStav <> TBlkPrjBasicStav.disabled) then
    begin
     Self.PrjStav.basicStav := TBlkPrjBasicStav.disabled;
     JCDb.RusJC(Self);
     Self.Change(true);
    end;
   Exit();
  end;

 new_stav := Self.UpdateInputs();

 if (Self.PrjStav.basicStav <> new_stav) then
  begin
   // kontrola necekaneho otevreni prejezdu, pokud je v JC
   // necekaniy stav = prejezd je pod zaverem a na vstupu se objevi cokoliv jineho, nez "uzavreno"
   if ((Self.Zaver) and (Self.PrjStav.basicStav = TBlkPrjBasicStav.uzavreno)) then
    begin
     for i := 0 to Self.OblsRizeni.Cnt-1 do
      Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Ztráta dohledu na pøejezdu : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
     JCDb.RusJC(Self);
    end;

   if (new_stav = none) then
    begin
     for i := 0 to Self.OblsRizeni.Cnt-1 do
      Self.OblsRizeni.ORs[i].BlkWriteError(Self, 'Porucha pøejezdu : '+Self.GlobalSettings.name, 'TECHNOLOGIE');
    end;

   Self.PrjStav.basicStav := new_stav;
   Self.Change();
  end;

 // kontrola prilis dlouho uzavreneho prejezdu
 if ((Self.Zaver) or (Self.PrjStav.PC_UZ)) then
  begin
   if (Now > Self.PrjStav.uzavStart+EncodeTime(0, _UZ_UPOZ_MIN, 0, 0)) then
    begin
     for i := 0 to Self.OblsRizeni.Cnt-1 do
      Self.OblsRizeni.ORs[i].BlkWriteError(Self, Self.GlobalSettings.name+' otevøen déle, jak '+IntToStr(_UZ_UPOZ_MIN)+' min', 'VAROVÁNÍ');
     Self.PrjStav.uzavStart := now;
    end;
  end;

 inherited Update();
end;//procedure

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
      Anulace:Boolean;
    end;
begin
 // get data from mtb
 try
   tmpInputs.Zavreno  := (RCSi.GetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Zavreno) = isOn);
   tmpInputs.Otevreno := (RCSi.GetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Otevreno) = isOn);
   tmpInputs.Vystraha := (RCSi.GetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Vystraha) = isOn);
   tmpInputs.Anulace  := (RCSi.GetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Anulace) = isOn);
 except
   // prejezd prejde do poruchoveho stavu
   tmpInputs.Zavreno  := false;
   tmpInputs.Otevreno := false;
   tmpInputs.Vystraha := false;
   tmpInputs.Anulace  := false;
 end;

 if (tmpInputs.Zavreno)  then Exit(TBlkPrjBasicStav.uzavreno);
 if (tmpInputs.Vystraha) then Exit(TBlkPrjBasicStav.vystraha);
 if (tmpInputs.Anulace)  then Exit(TBlkPrjBasicStav.anulace);
 if (tmpInputs.Otevreno) then Exit(TBlkPrjBasicStav.otevreno);

 // without data
 Result := none;
end;//function

procedure TBlkPrejezd.UpdateOutputs();
begin
 try
   if ((Self.PrjStav.PC_UZ) or (Self.Zaver)) then
    begin
     RCSi.SetOutput(Self.PrjSettings.MTB, Self.PrjSettings.MTBOutputs.Zavrit, 1);
    end else begin
     RCSi.SetOutput(Self.PrjSettings.MTB, Self.PrjSettings.MTBOutputs.Zavrit, 0);
    end;

   if (Self.PrjStav.PC_NOT) then
    begin
     RCSi.SetOutput(Self.PrjSettings.MTB, Self.PrjSettings.MTBOutputs.NOtevrit, 1);
    end else begin
     RCSi.SetOutput(Self.PrjSettings.MTB, Self.PrjSettings.MTBOutputs.NOtevrit, 0);
    end;
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkPrejezd.GetSettings():TBlkPrjSettings;
begin
 Result := Self.PrjSettings;
end;//function

procedure TBlkPrejezd.SetSettings(data:TBlkPrjSettings);
begin
 Self.PrjSettings := data;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.SetStit(stit:string);
begin
 Self.PrjStav.stit := Stit;
 Self.Change();
end;//procedure

procedure TBlkPrejezd.SetVyl(vyl:string);
begin
 Self.PrjStav.vyl := vyl;
 Self.Change();
 Self.UpdateOutputs();
end;//procedure

procedure TBlkPrejezd.SetNOT(state:boolean);
begin
 if ((Self.Zaver) and (state)) then Exit();

 Self.PrjStav.PC_NOT := state;
 Self.Change();
 Self.UpdateOutputs();
end;//procedure

procedure TBlkPrejezd.SetUZ(state:boolean);
begin
 if (state) then Self.PrjStav.uzavStart := now;

 Self.PrjStav.PC_UZ := state;
 Self.Change();
 Self.UpdateOutputs();
end;//procedure

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
    'Zrušení uzavøení pøejezdu', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkPrejezd.UPONOTClick(Sender:TObject);
begin
 ORTCPServer.Potvr(TIdContext(Sender), Self.PanelZNOTCallBack,
    (TTCPORsRef(TIdContext(Sender).Data).UPO_ref as TOR),
    'Nouzové otevøení pøejezdu', TBlky.GetBlksList(Self), nil);
end;

procedure TBlkPrejezd.UPOZNOTClick(Sender:TObject);
begin
 Self.NOtevreni := false;
end;

procedure TBlkPrejezd.MenuSTITClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;

procedure TBlkPrejezd.MenuAdminZAVRENOStartClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Zavreno, 1);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení MTB vstupù!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkPrejezd.MenuAdminZAVRENOStopClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 try
   RCSi.SetInput(Self.PrjSettings.MTB, Self.PrjSettings.MTBInputs.Zavreno, 0);
 except
   ORTCPServer.BottomError(SenderPnl, 'Simulace nepovolila nastavení MTB vstupù!', TOR(SenderOR).ShortName, 'SIMULACE');
 end;
end;

procedure TBlkPrejezd.MenuAdminNUZClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.PrjStav.zaver := 0;
 Self.UpdateOutputs();
 Self.Change();
end;//procedure

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
  end;//if not zaver

 Result := Result + 'STIT,';

 // pokud mame knihovnu simulator, muzeme ridit stav useku
 //  DEBUG nastroj
 if (RCSi.IsSimulatorMode()) then
  begin
   Result := Result + '-,';
   if ((Self.Stav.basicStav = TBlkPrjBasicStav.uzavreno) or (Self.Stav.basicStav = TBlkPrjBasicStav.vystraha)) then
     Result := Result + '*ZAVRENO<'
   else
     Result := Result + '*ZAVRENO>';
  end;//if MTB.lib = 2

 if (rights >= TORControlRights.superuser) then
  begin
   Result := Result + '-,';
   if (Self.Zaver) then Result := Result + '*NUZ>,';
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.PanelClick(SenderPnl:TIdCOntext; SenderOR:TObject; Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if ((Button <> TPanelButton.ESCAPE) and (Self.Stav.basicStav <> TBlkPrjBasicStav.disabled)) then
   ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkPrejezd.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.Stav.basicStav = TBlkPrjBasicStav.disabled) then Exit();

 if      (item = 'UZ')       then Self.MenuUZClick  (SenderPnl, SenderOR)
 else if (item = 'ZUZ')      then Self.MenuZUZClick (SenderPnl, SenderOR)
 else if (item = 'NOT>')     then Self.MenuNOTClick (SenderPnl, SenderOR)
 else if (item = 'NOT<')     then Self.MenuZNOTClick(SenderPnl, SenderOR)
 else if (item = 'STIT')     then Self.MenuSTITClick (SenderPnl, SenderOR)
 else if (item = 'NUZ>')     then Self.MenuAdminNUZClick(SenderPnl, SenderOR)
 else if (item = 'ZAVRENO>') then Self.MenuAdminZAVRENOStartClick(SenderPnl, SenderOR)
 else if (item = 'ZAVRENO<') then Self.MenuAdminZAVRENOStopClick(SenderPnl, SenderOR);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//zavola se pri uspesnem zvladnuti potvrzovaci sekvence
procedure TBlkPrejezd.PanelZNOTCallBack(Sender:TIdContext; success:boolean);
begin
 if (not success) then Exit();
 Self.Notevreni := true;
end;//procedure

procedure TBlkPrejezd.PanelZUZCallBack(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.UZ := false; 
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkPrejezd.SetZaver(zaver:boolean);
begin
 if (zaver) then
  begin
   Inc(Self.PrjStav.zaver);

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
end;//procedure

function TBlkPrejezd.GetZaver():boolean;
begin
 Result := (Self.PrjStav.zaver > 0);
end;//function

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

end.//unit

