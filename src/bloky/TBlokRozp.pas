unit TBlokRozp;

//definice a obsluha technologickeho bloku Rozpojovac

interface

uses IniFiles, TBlok, StrUtils, Classes, TOblsRizeni, SysUtils,
      IdContext, TOblRizeni;

type
 TRozpStatus = (disabled = -5, not_selected = 0, mounting = 1, active = 2);

 TBlkRozpSettings = record
  RCSAddrs:TRCSAddrs;     //only 1 address
 end;

 TBlkRozpStav = record
  status:TRozpStatus;
  finish:TDateTime;
 end;

 TBlkRozp = class(TBlk)
  const
   //defaultni stav
   _def_rozp_stav:TBlkRozpStav = (
      status : disabled;
   );

  private const
    _MOUNT_TO_ACTIVE_TIME_SEC   = 2;
    _ACTIVE_TO_DISABLE_TIME_SEC = 5;

  private
   RozpSettings:TBlkRozpSettings;
   RozpStav:TBlkRozpStav;

   procedure SetStatus(status:TRozpStatus);
   procedure UpdateOutput();

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

    //----- SCom own functions -----

    procedure PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer); override;
    procedure PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = ''); override;

    function GetSettings():TBlkRozpSettings;
    procedure SetSettings(data:TBlkRozpSettings);

    property stav:TBlkRozpStav read RozpStav;
    property status:TRozpStatus read RozpStav.status write SetStatus;

 end;//class TBlkRozp

////////////////////////////////////////////////////////////////////////////////

implementation

uses TechnologieRCS, TCPServerOR;

constructor TBlkRozp.Create(index:Integer);
begin
 inherited Create(index);

 Self.GlobalSettings.typ := _BLK_ROZP;
 Self.RozpStav           := Self._def_rozp_stav;
end;//ctor

destructor TBlkRozp.Destroy();
begin
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.LoadData(ini_tech:TMemIniFile;const section:string;ini_rel,ini_stat:TMemIniFile);
var str:TStrings;
begin
 inherited LoadData(ini_tech, section, ini_rel, ini_stat);

 Self.RozpSettings.RCSAddrs := Self.LoadRCS(ini_tech,section);

 if (ini_rel <> nil) then
  begin
   //parsing *.spnl
   str := TStringList.Create();

   ExtractStrings([';'],[],PChar(ini_rel.ReadString('R', IntToStr(Self.GlobalSettings.id), '')), str);
   if (str.Count < 1) then Exit;

   Self.ORsRef := ORs.ParseORs(str[0]);

   str.Free();
  end else begin
   Self.ORsRef.Cnt := 0;
  end;

end;//procedure

procedure TBlkRozp.SaveData(ini_tech:TMemIniFile;const section:string);
begin
 inherited SaveData(ini_tech,section);

 Self.SaveRCS(ini_tech,section,Self.RozpSettings.RCSAddrs);
end;//procedure

procedure TBlkRozp.SaveStatus(ini_stat:TMemIniFile;const section:string);
begin
 //
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.Enable();
begin
 Self.status := TRozpStatus.not_selected;
end;//procedure

procedure TBlkRozp.Disable();
begin
 Self.status := TRozpStatus.disabled;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.Update();
begin
 case (Self.status) of
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TBlkRozp.GetSettings():TBlkRozpSettings;
begin
 Result := Self.RozpSettings;
end;//function

procedure TBlkRozp.SetSettings(data:TBlkRozpSettings);
begin
 if (Self.RozpSettings.RCSAddrs <> data.RCSAddrs) then
   Self.RozpSettings.RCSAddrs.Free();

 Self.RozpSettings := data;
 Self.Change();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.PanelClick(SenderPnl:TIdContext; SenderOR:TObject ;Button:TPanelButton; rights:TORCOntrolRights; params:string = '');
begin
 if (Self.status = TRozpStatus.disabled) then Exit();

 case (Button) of
   F2: Self.ShowPanelMenu(SenderPnl, SenderOR, rights);

   ENTER: begin
     case (Self.status) of
       TRozpStatus.not_selected : begin
         // vybrat rozpojovac
         Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._MOUNT_TO_ACTIVE_TIME_SEC, 0);
         Self.status := TRozpStatus.mounting;
       end;
       TRozpStatus.mounting     : begin
         // zapnout rozpojovac
         Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
         Self.status := TRozpStatus.active;
       end;
       TRozpStatus.active       : begin
         // prodlouzit dobu rozpojovace
         Self.RozpStav.finish := Now + EncodeTime(0, 0, Self._ACTIVE_TO_DISABLE_TIME_SEC, 0);
       end;
     end;//case
    end;//case TPanelButton.left

   ESCAPE: begin
     case (Self.status) of
       TRozpStatus.mounting     : Self.status := TRozpStatus.not_selected;
       TRozpStatus.active       : Self.status := TRozpStatus.not_selected;
     else //case
       Self.ShowPanelMenu(SenderPnl, SenderOR, rights);
     end;
   end;
 end;//case

end;//procedure

////////////////////////////////////////////////////////////////////////////////

//toto se zavola pri kliku na jakoukoliv itemu menu tohoto bloku
procedure TBlkRozp.PanelMenuClick(SenderPnl:TIdContext; SenderOR:TObject; item:string; itemindex:Integer);
begin
 if (Self.status = TRozpStatus.disabled) then Exit();
end;//procedure

procedure TBlkRozp.SetStatus(status:TRozpStatus);
begin
 if (Self.status <> status) then
  begin
   Self.RozpStav.status := status;
   Self.UpdateOutput();
   Self.Change();
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TBlkRozp.UpdateOutput();
begin
 try
   if (Self.status = TRozpStatus.active) then
     RCSi.SetOutput(Self.RozpSettings.RCSAddrs[0].board, Self.RozpSettings.RCSAddrs[0].port, 1)
   else
     RCSi.SetOutput(Self.RozpSettings.RCSAddrs[0].board, Self.RozpSettings.RCSAddrs[0].port, 0);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
