unit TBlokUvazka;

//definice a obsluha technologickeho bloku Usek

interface

uses IniFiles, TBlok, TechnologieJC, Menus, TOblsRizeni, SysUtils, Classes,
     RPConst, IdContext;

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
   ORsRef:TORsRef;    //ve kterych OR se blok nachazi
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
    procedure MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVOnClick(SenderPnl:TIdContext; SenderOR:TObject);
    procedure MenuZAVOffClick(SenderPnl:TIdContext; SenderOR:TObject);

    procedure PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);
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

    property Stitek:string read UvazkaStav.Stit write SetUvazkaStit;
    property ZAK:boolean read UvazkaStav.ZAK write SetUvazkaZAK;
    property enabled:boolean read UvazkaStav.enabled;

    property OblsRizeni:TORsRef read ORsRef;
    property parent:TBlk read GetParent;
    property zadost:boolean read fzadost write SetZadost;
    property nouzZaver:boolean read UvazkaStav.nouzZaver write SetNouzZaver;

    //GUI:

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);

    procedure ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
 end;//class TBlkUsek

////////////////////////////////////////////////////////////////////////////////

implementation

uses GetSystems, TechnologieMTB, TBloky, TOblRizeni, TBlokSCom, Logging,
    TJCDatabase, Main, TCPServerOR, TBlokTrat;

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
 Self.zadost := true;
end;//procedure

procedure TBlkUvazka.MenuZTSOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.zadost := false;
end;//procedure

procedure TBlkUvazka.MenuUTSClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
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
end;//procedure

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
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAK, SenderOR as TOR, 'Zrušení zákazu odjezdu na tra', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUvazka.MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.UvazkaStav.Stit);
end;//procedure

procedure TBlkUvazka.MenuRBPClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvRBP, SenderOR as TOR, 'Zrušení poruchy úplné blokové podmínky', TBlky.GetBlksList(Self), nil);
end;//procedure

procedure TBlkUvazka.MenuZAVOnClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 Self.nouzZaver := true;
end;//procedure

procedure TBlkUvazka.MenuZAVOffClick(SenderPnl:TIdContext; SenderOR:TObject);
begin
 ORTCPServer.Potvr(SenderPnl, Self.PanelPotvrSekvZAV, SenderOR as TOR, 'Zrušení nouzového závìru', TBlky.GetBlksList(Self), nil);
end;//procedure


procedure TBlkUvazka.PanelPotvrSekvZAK(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.ZAK := false;
end;//procedure

procedure TBlkUvazka.PanelPotvrSekvRBP(Sender:TIdContext; success:boolean);
begin
 if (success) then (Self.parent as TBlkTrat).RBP();
end;//procedure

procedure TBlkUvazka.PanelPotvrSekvZAV(Sender:TIdContext; success:boolean);
begin
 if (success) then Self.nouzZaver := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//vytvoreni menu pro potreby konkretniho bloku:
procedure TBlkUvazka.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights);
var menu:string;
begin
 menu := '$'+Self.GlobalSettings.name+',-,';

 if ((not (Self.parent as TBlkTrat).Obsazeno) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).ZAK)) then
  begin
   // pokud je trat volna a neni na ni zaver
   // tratovy zabezpecovaci system
   case ((Self.parent as TBlkTrat).GetSettings().zabzar) of
    TTratZZ.souhlas:begin
     if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
      begin
       // prvni uvazka
       if (((Self.parent as TBlkTrat).Smer = TTratSmer.BtoA) and (not (Self.parent as TBlkTrat).RBPCan)) then
        begin
         // smer B->A
         if ((Self.parent as TBlkTrat).Zadost) then
           menu := menu + 'ZTS<,'
         else
          if (not (Self.parent as TBlkTrat).nouzZaver) then
           menu := menu + 'ZTS>,'
        end else begin
         // smer A->B
         if ((Self.parent as TBlkTrat).Zadost) then
           menu := menu + 'UTS,';
        end;

      end else begin
       // druha uvazka

       if ((Self.parent as TBlkTrat).Smer = TTratSmer.AtoB) then
        begin
         // smer A->B
         if (((Self.parent as TBlkTrat).Zadost) and (not (Self.parent as TBlkTrat).RBPCan)) then
           menu := menu + 'ZTS<,'
         else
          if (not (Self.parent as TBlkTrat).nouzZaver) then
           menu := menu + 'ZTS>,'
        end else begin
         // smer B->A
         if ((Self.parent as TBlkTrat).Zadost) then
           menu := menu + 'UTS,OTS,';
        end;
      end;// else IsFirstUvazka
     menu := menu + '-,';
    end;// case TTratZZ.souhlas

    TTratZZ.bezsouhas:begin
    end;

    TTratZZ.nabidka:begin
     if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
      begin
       // prvni uvazka

       if (Self.zadost) then
        begin
         menu := menu + 'ZTS<,';
        end else begin
         // not Self.zadost
         if (((Self.parent as TBlkTrat).Smer <> TTratSmer.AtoB) and (not (Self.parent as TBlkTrat).Zadost) and (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver)) then
           menu := menu + 'ZTS>,';

         if ((Self.parent as TBlkTrat).Zadost) then
           menu := menu + 'UTS,OTS,';
        end;

      end else begin
       // druha uvazka

       if (Self.zadost) then
        begin
         menu := menu + 'ZTS<,';
        end else begin
         // not Self.zadost
         if (((Self.parent as TBlkTrat).Smer <> TTratSmer.BtoA) and (not (Self.parent as TBlkTrat).Zadost) and (not (Self.parent as TBlkTrat).RBPCan) and (not (Self.parent as TBlkTrat).nouzZaver)) then
           menu := menu + 'ZTS>,';

         if ((Self.parent as TBlkTrat).Zadost) then
           menu := menu + 'UTS,OTS,';
        end;
      end;// else IsFirstUvazka
     menu := menu + '-,';
    end;

   end;//case
  end;//if not obsazeno and not zaver

 if (Self.nouzZaver) then
   menu := menu + '!ZAV<,'
 else
   menu := menu + 'ZAV>,';

 if ((Self.parent as TBlkTrat).RBPCan) then
  begin
   if ((Self.parent as TBlkTrat).IsFirstUvazka(Self)) then
    begin
     // prvni uvazka
     if (((Self.parent as TBlkTrat).Smer = TTratSmer.AtoB) or ((Self.parent as TBlkTrat).Smer = TTratSmer.zadny)) then
       menu := menu + '!RBP,';
    end else begin
     if (((Self.parent as TBlkTrat).Smer = TTratSmer.BtoA) or ((Self.parent as TBlkTrat).Smer = TTratSmer.zadny)) then
       menu := menu + '!RBP,';
    end;
  end;

 if (Self.ZAK) then
  menu := menu + '!ZAK<,'
 else
  if ((not (Self.parent as TBlkTrat).ZAK) and (not (Self.parent as TBlkTrat).Zaver) and (not (Self.parent as TBlkTrat).Obsazeno)) then
   menu := menu + 'ZAK>,';

 menu := menu + 'STIT,';

 ORTCPServer.Menu(SenderPnl, Self, SenderOR as TOR, menu);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkUvazka.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights);
begin
 Self.ShowPanelMenu(SenderPnl, SenderOR, rights);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkUvazka.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string);
begin
 if (item = 'ZTS>') then Self.MenuZTSOnClick(SenderPnl, SenderOR);
 if (item = 'ZTS<') then Self.MenuZTSOffClick(SenderPnl, SenderOR);
 if (item = 'UTS')  then Self.MenuUTSClick(SenderPnl, SenderOR);
 if (item = 'OTS')  then Self.MenuOTSClick(SenderPnl, SenderOR);
 if (item = 'ZAK>') then Self.MenuZAKOnClick(SenderPnl, SenderOR);
 if (item = 'ZAK<') then Self.MenuZAKOffClick(SenderPnl, SenderOR);
 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR);
 if (item = 'RBP')  then Self.MenuRBPClick(SenderPnl, SenderOR);
 if (item = 'ZAV>') then Self.MenuZAVOnClick(SenderPnl, SenderOR);
 if (item = 'ZAV<') then Self.MenuZAVOffClick(SenderPnl, SenderOR);
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

end.//unit

