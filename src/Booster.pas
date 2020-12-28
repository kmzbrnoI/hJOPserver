unit Booster;

{
  Definice a obsluha technologie zesilovace (napr. SPAX).
}

interface

uses IniFiles, TechnologieRCS, SysUtils, Generics.Defaults;

type
 TBoosterSignal = (undef = -1, error = 0, ok = 1);
 TBoosterChangeEvent = procedure (Sender:TObject; state:TBoosterSignal) of object;

 TBoosterSettings = record
   Name:string;                                                                 // booster name
   RCS:record                                                                   // RCS inputs
    Zkrat:TRCSAddr;                                                             // overload input
    Napajeni:TRCSAddr;                                                          // power input
    DCC:TRCSAddr;                                                               // DCC input; DCC nemusi byt detekovano, to se pozna tak, ze .board = 0
   end;
   id:string;
 end;//TBoosterSettings

 TBooster = class
  private
   Settings:TBoosterSettings;

   ZkratOld, NapajeniOld, DCCOld:TBoosterSignal;                                // old states (used to call events)

   //events
   FOnNapajeniChange : TBoosterChangeEvent;
   FOnZkratChange    : TBoosterChangeEvent;
   FOnDCCChange      : TBoosterChangeEvent;

    function GetZkrat():TBoosterSignal;
    function GetNapajeni():TBoosterSignal;
    function GetDCC():TBoosterSignal;

    function GetRCSPresent():Boolean;
    function GetDCCDetection():Boolean;
    function GetShortcutDetection():Boolean;
    function GetPowerDetection():Boolean;

  public

    constructor Create(); overload;
    constructor Create(var ini:TMemIniFile;const section:string); overload;
    destructor Destroy(); override;

    procedure Update();                              //update data; events are controlled only here

    procedure LoadDataFromFile(var ini:TMemIniFile;const section:string);
    procedure SaveDataToFile(var ini:TMemIniFile;const section:string);

    property zkrat:TBoosterSignal read GetZkrat;
    property napajeni:TBoosterSignal read GetNapajeni;
    property DCC:TBoosterSignal read GetDCC;

    property rcsPresent:Boolean read GetRCSPresent;
    property isShortcutDetection:Boolean read GetShortcutDetection;
    property isPowerDetection:Boolean read GetPowerDetection;
    property isDCCdetection:Boolean read GetDCCDetection;

    property bSettings:TBoosterSettings read settings write settings;

    property id:string read Settings.id;
    property name:string read Settings.name;

    property OnNapajeniChange:TBoosterChangeEvent read FOnNapajeniChange write FOnNapajeniChange;
    property OnZkratChange:TBoosterChangeEvent read FOnZkratChange write FOnZkratChange;
    property OnDCCChange:TBoosterChangeEvent read FOnDCCChange write FONDCCChange;

    class function IdComparer():IComparer<TBooster>;
 end;//TBooster

implementation

uses GetSystems, fMain, RCS, TechnologieTrakce, Trakce;

{
  Format datoveho souboru: .ini soubor, kazdy SPAX ma svou sekci
  [id]
    name
    class
    zkr_RCS
    zkr_port
    nap_RCS
    nap_port
    dcc_RCS
    dcc_port
}

////////////////////////////////////////////////////////////////////////////////

//ctor
constructor TBooster.Create();
begin
 inherited;

 Self.ZkratOld    := TBoosterSignal.undef;
 Self.NapajeniOld := TBoosterSignal.undef;
 Self.DCCOld      := TBoosterSignal.undef;
end;//ctor

//ctor
constructor TBooster.Create(var ini:TMemIniFile;const section:string);
begin
 Self.Create();

 Self.LoadDataFromFile(ini,section);
end;//ctor

//dtor
destructor TBooster.Destroy();
begin
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBooster.Update();
var state:TBoosterSignal;
begin
 //update DCC
 state := Self.GetDCC();
 if (state <> Self.DCCOld) then
  begin
   if (Assigned(Self.OnDCCChange)) then Self.OnDCCChange(Self, state);
   Self.DCCOld := state;
  end;

 //update napajeni
 state := Self.GetNapajeni();
 if (state <> Self.NapajeniOld) then
  begin
   if (Assigned(Self.OnNapajeniChange)) then Self.OnNapajeniChange(Self, state);
   Self.NapajeniOld := state;
  end;

 //update zkrat
 state := Self.GetZkrat();
 if (state <> Self.ZkratOld) then
  begin
   if (Assigned(Self.OnZkratChange)) then Self.OnZkratChange(Self, state);
   Self.ZkratOld := state;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//load data from the file
procedure TBooster.LoadDataFromFile(var ini:TMemIniFile;const section:string);
begin
 Self.Settings.id     := section;
 Self.Settings.Name   := ini.ReadString(section, 'name', 'booster');

 Self.Settings.RCS.Zkrat.board := ini.ReadInteger(section, 'zkr_module', 0);
 if (Self.Settings.RCS.Zkrat.board = 0) then
   Self.Settings.RCS.Zkrat.board := ini.ReadInteger(section, 'zkr_mtb', 0);
 Self.Settings.RCS.Zkrat.port := ini.ReadInteger(section, 'zkr_port', 0);

 Self.Settings.RCS.Napajeni.board := ini.ReadInteger(section, 'nap_module', 0);
 if (Self.Settings.RCS.Napajeni.board = 0) then
   Self.Settings.RCS.Napajeni.board := ini.ReadInteger(section, 'nap_mtb', 0);
 Self.Settings.RCS.Napajeni.port := ini.ReadInteger(section, 'nap_port', 0);

 Self.Settings.RCS.DCC.board := ini.ReadInteger(section, 'dcc_module', 0);
 if (Self.Settings.RCS.DCC.board = 0) then
   Self.Settings.RCS.DCC.board := ini.ReadInteger(section, 'dcc_mtb', 0);
 Self.Settings.RCS.DCC.port := ini.ReadInteger(section, 'dcc_port', 0);

 if (Self.isPowerDetection) then
   RCSi.SetNeeded(Self.Settings.RCS.Napajeni.board);
 if (Self.isShortcutDetection) then
   RCSi.SetNeeded(Self.Settings.RCS.Zkrat.board);
end;

//save data to the file
procedure TBooster.SaveDataToFile(var ini:TMemIniFile;const section:string);
begin
 ini.WriteString(section, 'name', Self.Settings.Name);

 if (Self.isShortcutDetection) then
  begin
   ini.WriteInteger(section, 'zkr_module', Self.Settings.RCS.Zkrat.board);
   ini.WriteInteger(section, 'zkr_port', Self.Settings.RCS.Zkrat.port);
  end;

 if (Self.isPowerDetection) then
  begin
   ini.WriteInteger(section, 'nap_module', Self.Settings.RCS.Napajeni.board);
   ini.WriteInteger(section, 'nap_port', Self.Settings.RCS.Napajeni.port);
  end;

 if (Self.isDCCdetection) then
  begin
   ini.WriteInteger(section, 'dcc_module', Self.Settings.RCS.DCC.board);
   ini.WriteInteger(section, 'dcc_port', Self.Settings.RCS.DCC.port);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//gets data from RCS

function TBooster.GetZkrat():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not RCSi.ready) or (not RCSi.Started)) then Exit(TBoosterSignal.undef);

 //if not a power, not a overload
 if (Self.napajeni = TBoosterSignal.error) then
   Exit(TBoosterSignal.undef);

 if (not Self.isShortcutDetection) then
   Exit(TBoosterSignal.ok);

 try
   val := RCSi.GetInput(Self.Settings.RCS.Zkrat);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;

function TBooster.GetNapajeni():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not RCSi.ready) or (not RCSi.Started)) then Exit(TBoosterSignal.undef);

 if (not Self.isPowerDetection) then
   Exit(TBoosterSignal.ok);

 try
   val := RCSi.GetInput(Self.Settings.RCS.Napajeni);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;

function TBooster.GetDCC():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not RCSi.ready) or (not RCSi.Started)) then Exit(TBoosterSignal.undef);

 // DCC nemusi byt detekovano (to se pozna tak, ze RCS board = 0)
 if (not Self.isDCCdetection) then
  begin
   case (TrakceI.TrackStatusSafe()) of
     TTrkStatus.tsUnknown: Exit(TBoosterSignal.undef);
     TTrkStatus.tsOff: Exit(TBoosterSignal.error);
     TTrkStatus.tsOn: Exit(TBoosterSignal.ok);
     TTrkStatus.tsProgramming: Exit(TBoosterSignal.error);
   else
     Exit(TBoosterSignal.undef);
   end;
  end;

 try
   val := RCSi.GetInput(Self.Settings.RCS.DCC);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailableModule) or (val = unavailablePort)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;

////////////////////////////////////////////////////////////////////////////////

function TBooster.GetRCSPresent():Boolean;
begin
 Result := (((not Self.isShortcutDetection) or RCSi.IsModule(Self.Settings.RCS.Zkrat.board)) and
            ((not Self.isPowerDetection) or RCSi.IsModule(Self.Settings.RCS.Napajeni.board)) and
            ((not Self.isDCCdetection) or RCSi.IsModule(Self.Settings.RCS.DCC.board)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBooster.GetDCCDetection():Boolean;
begin
 Result := (Self.Settings.RCS.DCC.board > 0);
end;

function TBooster.GetShortcutDetection():Boolean;
begin
 Result := (Self.Settings.RCS.Zkrat.board > 0);
end;

function TBooster.GetPowerDetection():Boolean;
begin
 Result := (Self.Settings.RCS.Napajeni.board > 0);
end;

////////////////////////////////////////////////////////////////////////////////

class function TBooster.IdComparer():IComparer<TBooster>;
begin
 Result := TComparer<TBooster>.Construct(
  function(const Left, Right: TBooster): Integer
   begin
    Result := CompareStr(Left.id, Right.id, loUserLocale);
   end
 );
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
