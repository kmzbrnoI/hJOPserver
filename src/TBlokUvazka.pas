unit TBlokUvazka;

//definice a obsluha technologickeho bloku Usek

interface

uses IniFiles, TBlok, TechnologieJC, Menus, TOblsRizeni, SysUtils, Classes,
     IdContext, StrUtils, TOblRizeni;

type

 //technologicka nastaveni useku (delka, MTB, ...)
 TBlkUvazkaSettings = record
  parent:Integer;       // reference na matersky blok (typu TTrat)
 end;

 //aktualni stav useku (obsazeno, ...)
 TBlkUvazkaStav = record
  enabled:boolean;
  ZAK:boolean;
  stit:string;
  nouzZaver:boolean;
 end;

 TBlkUvazka = class(TBlk)
  const
   //defaultni stav
   _def_uvazka_stav:TBlkUvazkaStav = (
    enabled: false;
    ZAK : false;
    stit : '';
   );

  private
   UvazkaSettings:TBlkUvazkaSettings;
   UvazkaStav:TBlkUvazkaStav;
   fparent:TBlk;
   fzadost:boolean;

    function GetParent():TBlk;

    procedure SetUvazkaStit(stit:string);
    procedure SetUvazkaZAK(ZAK:boolean);
    procedure SetNouzZaver(nouz:boolean);

    procedure MenuZTSOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZTSOffClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuUTSClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuOTSClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAKOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAKOffClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVOffClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
    procedure PanelPotvrSekvZAK(Sender:TIdContext; success:boolean);

    procedure SetZadost(zadost:boolean);

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
    procedure Change(now:boolean = false); override;
    procedure ChangeFromTrat();

    //----- usek own functions -----

    function GetSettings():TBlkUvazkaSettings;
    procedure SetSettings(data:TBlkUvazkaSettings);

    procedure UdelSouhlas();
    function CanZTS():boolean;

    property Stitek:string read UvazkaStav.Stit write SetUvazkaStit;
    property ZAK:boolean read UvazkaStav.ZAK write SetUvazkaZAK;
    property enabled:boolean read UvazkaStav.enabled;

    property parent:TBlk read GetParent;
    property zadost:boolean read fzadost write SetZadost;
    property nouzZaver:boolean read UvazkaStav.nouzZaver write SetNouzZaver;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string); override;
    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights); override;
 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieMTB, TBloky, TBlokSCom, Logging,
    TJCDatabase, fMain, TCPServerOR, TBlokTrat, Zasobnik, TBlokUsek;

constructor TBlkUvazka.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_UVAZKA;
 Self.UvazkaStav := _def_uvazka_stav;
 Self.fparent := nil;
 Self.fzadost := false;
end;//ctor

destructor TBlkUvazka.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.UvazkaSettings.parent := ini_tech.ReadInteger(section, 'parent', -1);
 Self.UvazkaStav.Stit       := ini_stat.ReadString(section, 'stit', '');

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   ExtractStrings([';'],[],PChar(ini_rel.ReadString('Uv',IntToStr(Self.GlobalSettings.id),'')),str);
   if (str.Count < 1) then Exit;

   Self.ORsRef := ORs.ParseORs(str[0]);

   str.Free();
  end else begin
   Self.ORsRef.Cnt := 0;
  end;

end;//procedure

procedure TBlkUvazka.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 ini_tech.WriteInteger(section, 'parent', Self.UvazkaSettings.parent);
end;//procedure

procedure TBlkUvazka.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 if (Self.UvazkaStav.stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.UvazkaStav.Stit);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.Enable();
begin
 Self.UvazkaStav.enabled := true;
 Self.Change();
end;//procedure

procedure TBlkUvazka.Disable();
begin
 Self.UvazkaStav.enabled   := false;
 Self.UvazkaStav.nouzZaver := false;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//update all local variables
procedure TBlkUvazka.Update();
begin
 inherited Update();
end;//procedure

procedure TBlkUvazka.Change(now:boolean = false);
begin
 inherited Change(now);
 (Self.parent as TBlkTrat).ChangeFromUv(Self);
end;//procedure

procedure TBlkUvazka.ChangeFromTrat();
begin
 if (Self.parent = nil) then Exit(); 
 if (not (Self.parent as TBlkTrat).Zadost) then
  Self.fzadost := false;

 inherited Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.SetUvazkaStit(stit:string);
begin
 Self.UvazkaStav.Stit := stit;
 Self.Change();
end;//procedure

procedure TBlkUvazka.SetUvazkaZAK(ZAK:boolean);
var old:boolean;
begin
 old := Self.UvazkaStav.ZAK;
 Self.UvazkaStav.ZAK := ZAK;
 Self.Change();

 if (old <> ZAK) then
  (Self.parent as TBlkTrat).ChangeUseky();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkUvazka.GetSettings():TBlkUvazkaSettings;
begin
 Result := Self.UvazkaSettings;
end;//function

procedure TBlkUvazka.SetSettings(data:TBlkUvazkaSettings);
begin
 Self.UvazkaSettings := data;
 Self.fparent := nil;   // timto se zajisti prepocitani parent pri pristi zadost i nej
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////
//dynamicke funkce:

procedure TBlkUvazka.MenuZTSOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 case ((SenderOR as TOR).stack.volba) of
  TORStackVolba.VZ : (SenderOR as TOR).stack.AddZTS(self, SenderPnl);
  TORStackVolba.PV : Self.zadost := true;
 end;
end;//procedure

procedure TBlkUvazka.MenuZTSOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.zadost := false;
end;//procedure

procedure TBlkUvazka.MenuUTSClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 case ((SenderOR as TOR).stack.volba) of
  TORStackVolba.VZ : (SenderOR as TOR).stack.AddUTS(self, SenderPnl);
  TORStackVolba.PV : Self.UdelSouhlas();
 end;
end;//procedure

procedure TBlkUvazka.UdelSouhlas();
begin
 if (not (Self.parent as TBlkTrat).Zadost) then Exit();

 case ((Self.parent as TBlkTrat).smer) of
  TTratSmer.AtoB : (Self.parent as TBlkTrat).smer := TTratSmer.BtoA;
  TTratSmer.BtoA : (Self.parent as TBlkTrat).smer := TTratSmer.AtoB;
  TTratSmer.zadny: begin
   if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
    (Self.parent as TBlkTrat).smer := TTratSmer.BtoA
   else
    (Self.parent as TBlkTrat).smer := TTratSmer.AtoB;
  end;//case
 end;

 Self.zadost := false;
end;

procedure TBlkUvazka.MenuOTSClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.parent as TBlkTrat).GetSettings.zabzar = TTratZZ.nabidka) then
   (Self.parent as TBlkTrat).smer := TTratSmer.zadny;
 Self.zadost := false;
end;//procedure

procedure TBlkUvazka.MenuZAKOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 if ((Self.parent as TBlkTrat).Zadost) then
  (Self.parent as TBlkTrat).Zadost := false;
 Self.ZAK := true;
end;//procedure

procedure TBlkUvazka.MenuZAKOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAK, SenderOR as TOR, 'ZruöenÌ z·kazu odjezdu na traù', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUvazka.MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.UvazkaStav.Stit);
end;//procedure

procedure TBlkUvazka.MenuZAVOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.nouzZaver := true;
end;//procedure

procedure TBlkUvazka.MenuZAVOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, SenderOR as TOR, 'ZruöenÌ nouzovÈho z·vÏru', TBlky.GetBlksList(Self), nil);
end;//procedure


procedure TBlkUvazka.PanelPotvrSekvZAK(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.ZAK := false;
end;//procedure

procedure TBlkUvazka.PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.nouzZaver := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
function TBlkUvazka.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
var Blk, Blk2:TBlk;
begin
 Result := inherited;

 // tratovy zabezpecovaci system
 case ((Self.parent as TBlkTrat).GetSettings().zabzar) of
  TTratZZ.souhlas:begin

   if ((not Self.zadost) and ((Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'UTS,';

   if ((Self.parent as TBlkTrat).Zadost) then
     Result := Result + 'ZTS<,';

   if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
    begin
     // prvni uvazka

     if (((not Self.zadost) and ((Self.parent as TBlkTrat).Smer = TTratSmer.BtoA) and (not (Self.parent as TBlkTrat).Zadost) and
          (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver) and
          (not (Self.parent as TBlkTrat).Obsazeno) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).ZAK)) or
          ((SenderOR as TOR).stack.volba = TORStackVolba.VZ)) then
       Result := Result + 'ZTS>,';

    end else begin
     // druha uvazka

     if (((not Self.zadost) and ((Self.parent as TBlkTrat).Smer = TTratSmer.AtoB) and (not (Self.parent as TBlkTrat).Zadost) and
          (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver) and
          (not (Self.parent as TBlkTrat).Obsazeno) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).ZAK)) or
          ((SenderOR as TOR).stack.volba = TORStackVolba.VZ)) then
       Result := Result + 'ZTS>,';

    end;// else IsFirstUvazka

   if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) and ((Self.zadost) or (not (Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'UTS,';

   if (((not Self.zadost) and (Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'OTS,';

   if (RightStr(Result, 2) <> '-,') then
     Result := Result + '-,';
  end;// case TTratZZ.souhlas

  TTratZZ.bezsouhas:begin
  end;

  TTratZZ.nabidka:begin

   if ((not Self.zadost) and ((Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'UTS,';

   if (Self.zadost) then
     Result := Result + 'ZTS<,';

   if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
    begin
     // prvni uvazka

     if (((not Self.zadost) and((Self.parent as TBlkTrat).Smer <> TTratSmer.AtoB) and (not (Self.parent as TBlkTrat).Zadost) and
          (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver) and
          (not (Self.parent as TBlkTrat).Obsazeno) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).ZAK)) or
          ((SenderOR as TOR).stack.volba = TORStackVolba.VZ)) then
       Result := Result + 'ZTS>,';

    end else begin
     // druha uvazka

     if (((not Self.zadost) and ((Self.parent as TBlkTrat).Smer <> TTratSmer.BtoA) and (not (Self.parent as TBlkTrat).Zadost) and
          (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver) and
          (not (Self.parent as TBlkTrat).Obsazeno) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).ZAK)) or
          ((SenderOR as TOR).stack.volba = TORStackVolba.VZ)) then
       Result := Result + 'ZTS>,';

    end;// else IsFirstUvazka

   if ((SenderOR as TOR).stack.volba = TORStackVolba.VZ) and ((Self.zadost) or (not (Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'UTS,';

   if (((not Self.zadost) and (Self.parent as TBlkTrat).Zadost)) then
     Result := Result + 'OTS,';

   if (RightStr(Result, 2) <> '-,') then
     Result := Result + '-,';
  end;

 end;//case

 if (Self.nouzZaver) then
   Result := Result + '!ZAV<,'
 else
   Result := Result + 'ZAV>,';

 if (Self.ZAK) then
  begin
   // zruseni ZAK je podmineno tim, ze na krajnich usecich trati nejsou zavery
   // to zajistuje, ze njelze zrusit ZAK u trati, do ktere je postaven PMD
   Blky.GetBlkByID(TBlkTrat(Self.parent).GetSettings().Useky[0], Blk);
   Blky.GetBlkByID(TBlkTrat(Self.parent).GetSettings().Useky[TBlkTrat(Self.parent).GetSettings().Useky.Count-1], Blk2);
   if ((Blk <> nil) and (Blk2 <> nil) and (TBlkUsek(Blk).Zaver = TZaver.no) and (TBlkUsek(Blk2).Zaver = TZaver.no)) then
     Result := Result + '!ZAK<,'
  end else
  if ((not (Self.parent as TBlkTrat).ZAK) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).Obsazeno)) then
   Result := Result + 'ZAK>,';

 Result := Result + 'STIT,';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
begin
 if (TBlkTrat(Self.parent).Smer < TTratSmer.zadny) then Exit();
 ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkUvazka.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if      (item = 'ZTS>') then Self.MenuZTSOnClick(SenderPnl, SenderOR)
 else if (item = 'ZTS<') then Self.MenuZTSOffClick(SenderPnl, SenderOR)
 else if (item = 'UTS')  then Self.MenuUTSClick(SenderPnl, SenderOR)
 else if (item = 'OTS')  then Self.MenuOTSClick(SenderPnl, SenderOR)
 else if (item = 'ZAK>') then Self.MenuZAKOnClick(SenderPnl, SenderOR)
 else if (item = 'ZAK<') then Self.MenuZAKOffClick(SenderPnl, SenderOR)
 else if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR)
 else if (item = 'ZAV>') then Self.MenuZAVOnClick(SenderPnl, SenderOR)
 else if (item = 'ZAV<') then Self.MenuZAVOffClick(SenderPnl, SenderOR);
end;//procedure

///////////////////////////////////////////////////////////////////////////////

function TBlkUvazka.GetParent():TBlk;
begin
 if (Self.fparent = nil) then
   Blky.GetBlkByID(Self.UvazkaSettings.parent, Self.fparent);
 Result := Self.fparent;
end;//function

///////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.SetZadost(zadost:boolean);
begin
 // tohleto poradi nastvovani je dulezite
 if (zadost) then Self.fzadost := zadost;
 (Self.parent as TBlkTrat).Zadost := zadost;
 if (not zadost) then Self.fzadost := zadost;
end;//procedure

///////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.SetNouzZaver(nouz:boolean);
begin
 if (Self.UvazkaStav.nouzZaver = nouz) then Exit();

 Self.UvazkaStav.nouzZaver := nouz;
 Self.Change();
end;//procedure

///////////////////////////////////////////////////////////////////////////////

// Takto zasobnik zjistuje, jestli muze zacit zadost:
function TBlkUvazka.CanZTS():boolean;
begin
 if (((Self.parent as TBlkTrat).Obsazeno) or ((Self.parent as TBlkTrat).Zaver) or
    ((Self.parent as TBlkTrat).ZAK) or ((Self.parent as TBlkTrat).nouzZaver) or
    ((Self.parent as TBlkTrat).RBPCan) or ((Self.parent as TBlkTrat).GetSettings().zabzar = TTratZZ.bezsouhas)) then Exit(false);

 if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
  begin
   Result := ((not Self.zadost) and ((Self.parent as TBlkTrat).Smer <> TTratSmer.AtoB) and (not (Self.parent as TBlkTrat).Zadost));
  end else begin
   Result := ((not Self.zadost) and ((Self.parent as TBlkTrat).Smer <> TTratSmer.BtoA) and (not (Self.parent as TBlkTrat).Zadost));
  end;
end;//function

end.//unit

