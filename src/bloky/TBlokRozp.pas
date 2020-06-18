unit TBlokRozp;

//definice a obsluha technologickeho bloku Rozpojovac

interface

uses IniFiles, TBlok, Classes, TOblsRizeni, SysUtils, JsonDataObjects,
     IdContext, TOblRizeni, TechnologieRCS;

type
 TRozpStatus = (disabled = -5, not_selected = 0, mounting = 1, active = 2);

 TBlkRozpSettings = record
  RCSAddrs:TRCSAddrs;     //only 1 address
 end;

 TBlkRozpStav = record
  status: TRozpStatus;
  finish: TDateTime;
  rcsFailed: Boolean;
  stit: string;
 end;

 TBlkRozp = class(TBlk)
  const
   //defaultni stav
   _def_rozp_stav:TBlkRozpStav = (
      status : disabled;
      rcsFailed: false;
      stit : '';
   );

  private const
    _MOUNT_TO_ACTIVE_TIME_SEC = 3;
    _ACTIVE_TO_DISABLE_TIME_SEC = 30;

  private
   RozpSettings:TBlkRozpSettings;
   RozpStav:TBlkRozpStav;

   procedure SetStatus(status:TRozpStatus);
   procedure UpdateOutput();

   procedure SetStit(stit:string);

   procedure Mount();
   procedure Activate();
   procedure Prolong();

   procedure MenuStitClick(SenderPnl:TIdContext; SenderOR:TObject);

  public
    constructor Create(index:Integer);

    //load/save data
    procedure LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile); override;
    procedure SaveData(ini_tech:TMemIniFile;const section:string); override;
    procedure SaveStatus(ini_stat:TMemIniFile;const section:string); override;

    //enable or disable symbol on relief
    procedure Enable(); override;
    procedure Disable(); override;
    function UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean; override;

    //update states
    procedure Update(); override;

    //----- Rozpojovac own functions -----

    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject; Button:TPanelButton;
                         rights:TORCOntrolRights; params:string = ''); override;
    function PanelStateString():string; override;

    function GetSettings():TBlkRozpSettings;
    procedure SetSettings(data:TBlkRozpSettings);

    function ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string; override;
    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;

    property stav:TBlkRozpStav read RozpStav;
    property status:TRozpStatus read RozpStav.status write SetStatus;
    property stit:string read RozpStav.stit write SetStit;

    //PT:
    procedure GetPtData(json:TJsonObject; includeState:boolean); override;
    procedure GetPtState(json:TJsonObject); override;
    procedure PostPtState(reqJson:TJsonObject; respJson:TJsonObject); override;

 end;//class TBlkRozp

////////////////////////////////////////////////////////////////////////////////

implementation

uses TCPServerOR, Prevody, Graphics, PTUtils;

constructor TBlkRozp.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_ROZP;
 Self.RozpStav           := Self._def_rozp_stav;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.RozpSettings.RCSAddrs := Self.LoadRCS(ini_tech,section);
 Self.LoadORs(ini_rel, 'R').Free();
 PushRCStoOR(Self.ORsRef, Self.RozpSettings.RCSAddrs);

 Self.RozpStav.Stit := ini_stat.ReadString(section, 'stit', '');
end;

procedure TBlkRozp.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 Self.SaveRCS(ini_tech,section,Self.RozpSettings.RCSAddrs);
end;

procedure TBlkRozp.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 if (Self.RozpStav.Stit <> '') then
   ini_stat.WriteString(section, 'stit', Self.RozpStav.Stit);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.Enable();
begin
 if ((RCSi.IsModule(Self.RozpSettings.RCSAddrs[0].board)) and
     (not RCSi.IsModuleFailure(Self.RozpSettings.RCSAddrs[0].board))) then
 begin
  Self.RozpStav.rcsFailed := false;
  Self.status := TRozpStatus.not_selected;
 end else begin
  Self.RozpStav.rcsFailed := true;
 end;
end;

procedure TBlkRozp.Disable();
begin
 Self.status := TRozpStatus.disabled;
 Self.RozpStav.rcsFailed := false;
 Self.Change(true);
end;

function TBlkRozp.UsesRCS(addr: TRCSAddr; portType: TRCSIOType): Boolean;
begin
 Result := ((portType = TRCSIOType.output) and (Self.RozpSettings.RCSAddrs.Contains(addr)));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.Update();
begin
 if ((Self.status <> TRozpStatus.disabled) and ((RCSi.IsModuleFailure(Self.RozpSettings.RCSAddrs[0].board)) or
                                                (not RCSi.IsModule(Self.RozpSettings.RCSAddrs[0].board)))) then
  begin
   Self.status := TRozpStatus.disabled;
   Self.RozpStav.rcsFailed := true;
  end;

 case (Self.status) of
   TRozpStatus.disabled : begin
      if ((Self.RozpStav.rcsFailed) and (RCSi.IsModule(Self.RozpSettings.RCSAddrs[0].board)) and
          (not RCSi.IsModuleFailure(Self.RozpSettings.RCSAddrs[0].board))) then
       begin
        Self.RozpStav.rcsFailed := false;
        Self.status := TRozpStatus.not_selected;
       end;
   end;
   TRozpStatus.mounting : begin
      if (Now > Self.RozpStav.finish) then
       begin
        Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
        Self.status := TRozpStatus.active;
       end;
   end;
   TRozpStatus.active   : if (Now > Self.RozpStav.finish) then Self.status := TRozpStatus.not_selected;
 end;//case

 inherited Update();
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRozp.GetSettings():TBlkRozpSettings;
begin
 Result := Self.RozpSettings;
end;

procedure TBlkRozp.SetSettings(data:TBlkRozpSettings);
begin
 if (Self.RozpSettings.RCSAddrs <> data.RCSAddrs) then
   Self.RozpSettings.RCSAddrs.Free();

 Self.RozpSettings := data;
 Self.Change();
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Self.status = TRozpStatus.disabled) then Exit();

 case (Button) of
   F2: ORTCPServer.Menu(SenderPnl, Self, (SenderOR as TOR), Self.ShowPanelMenu(SenderPnl, SenderOR, rights));

   ENTER: begin
     case (Self.status) of
       TRozpStatus.not_selected: Self.Mount();
       TRozpStatus.mounting: Self.Activate();
       TRozpStatus.active: Self.Prolong();
     end;
    end;

   ESCAPE: begin
     case (Self.status) of
       TRozpStatus.mounting: Self.status := TRozpStatus.not_selected;
       TRozpStatus.active: Self.status := TRozpStatus.not_selected;
     end;
   end;
 end;//case

end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRozp.ShowPanelMenu(SenderPnl:TIdContext; SenderOR:TObject; rights:TORCOntrolRights):string;
begin
 Result := inherited + 'STIT,';
end;

procedure TBlkRozp.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.status = TRozpStatus.disabled) then Exit();

 if (item = 'STIT') then Self.MenuStitClick(SenderPnl, SenderOR);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.SetStatus(status:TRozpStatus);
begin
 if (Self.status <> status) then
  begin
   Self.RozpStav.status := status;
   Self.UpdateOutput();
   Self.Change();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.UpdateOutput();
begin
 try
   if (Self.status = TRozpStatus.active) then
     RCSi.SetOutput(Self.RozpSettings.RCSAddrs[0].board, Self.RozpSettings.RCSAddrs[0].port, 1)
   else
     RCSi.SetOutput(Self.RozpSettings.RCSAddrs[0].board, Self.RozpSettings.RCSAddrs[0].port, 0);
 except
  Self.RozpStav.rcsFailed := true;
  Self.status := TRozpStatus.disabled;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TBlkRozp.PanelStateString():string;
var fg, bg: TColor;
begin
 Result := inherited;

 bg := clBlack;
 case (Self.status) of
   TRozpStatus.disabled     : fg := clFuchsia;
   TRozpStatus.not_selected : fg := $A0A0A0;
   TRozpStatus.mounting     : fg := clYellow;
   TRozpStatus.active       : fg := clLime;
 else
   fg := clFuchsia;
 end;

 if (Self.stit <> '') then bg := clTeal;

 Result := Result + PrevodySoustav.ColorToStr(fg) + ';' +
                    PrevodySoustav.ColorToStr(bg) + ';0;';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.Mount();
begin
 Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._MOUNT_TO_ACTIVE_TIME_SEC, 0);
 Self.status := TRozpStatus.mounting;
end;

procedure TBlkRozp.Activate();
begin
 Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
 Self.status := TRozpStatus.active;
end;

procedure TBlkRozp.Prolong();
begin
 Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.GetPtData(json:TJsonObject; includeState:boolean);
begin
 inherited;
 TBlk.RCStoJSON(Self.RozpSettings.RCSAddrs[0], json['rcs']);
 if (includeState) then
   Self.GetPtState(json['blokStav']);
end;

procedure TBlkRozp.GetPtState(json:TJsonObject);
begin
 case (Self.status) of
  TRozpStatus.disabled: json['stav'] := 'vypnuto';
  TRozpStatus.not_selected: json['stav'] := 'not_selected';
  TRozpStatus.mounting: json['stav'] := 'mounting';
  TRozpStatus.active: json['stav'] := 'active';
 end;
end;

procedure TBlkRozp.PostPtState(reqJson:TJsonObject; respJson:TJsonObject);
begin
 if (reqJson.Contains('stav')) then
  begin
   if (Self.status = TRozpStatus.disabled) then
    begin
     PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '403', 'Forbidden', 'Nelze nastavit neaktivni rozpojovac');
     inherited;
     Exit();
    end;

   if (reqJson.S['stav'] = 'mounting') then
     Self.Mount()
   else if (reqJson.S['stav'] = 'active') then
     Self.Activate()
   else if (reqJson.S['stav'] = 'not_selected') then
     Self.status := TRozpStatus.not_selected;
  end;

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.SetStit(stit:string);
begin
 Self.RozpStav.Stit := stit;
 Self.Change();
end;

procedure TBlkRozp.MenuStitClick(SenderPnl: TIdContext; SenderOR: TObject);
begin
 ORTCPServer.Stitek(SenderPnl, Self, Self.Stav.Stit);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
