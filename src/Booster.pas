unit Booster;

{
  Definice a obsluha technologie zesilovace (napr. SPAX).
}

interface

uses IniFiles, TechnologieMTB, SysUtils;

type
 TBoosterSignal = (undef = -1, error = 0, ok = 1);

 TBoosterClass = (undefinned = -1, default = 0, SPAX = 1);

 TBoosterChangeEvent = procedure (Sender:TObject; state:TBoosterSignal) of object;

 TBoosterSettings = record
   bclass:TBoosterClass;                                                        // booster class (spax, bz100)
   Name:string;                                                                 // booster name
   MTB:record                                                                   // MTB inputs
    Zkrat:TMTBAddr;                                                             // overload input
    Napajeni:TMTBAddr;                                                          // power input
    DCC:TMTBAddr;                                                               // DCC input; DCC nemusi byt detekovano, to se pozna tak, ze .board = 0
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

    function GetDefined():boolean;
    function GetDCCDetection():boolean;

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

    property defined:boolean read GetDefined;
    property isDCCdetection:boolean read GetDCCDetection;

    property bSettings:TBoosterSettings read settings write settings;

    property id:string read Settings.id;
    property name:string read Settings.name;
    property bclass:TBoosterClass read Settings.bclass;

    property OnNapajeniChange:TBoosterChangeEvent read FOnNapajeniChange write FOnNapajeniChange;
    property OnZkratChange:TBoosterChangeEvent read FOnZkratChange write FOnZkratChange;
    property OnDCCChange:TBoosterChangeEvent read FOnDCCChange write FONDCCChange;

    class function GetBClassString(b_type:TBoosterClass):string;          //get booster name as a string
 end;//TBooster

implementation

uses GetSystems, fMain, RCS;

{
  Format datoveho souboru: .ini soubor, kazdy SPAX ma svou sekci
  [id]
    name
    class
    zkr_mtb
    zkr_port
    nap_mtb
    nap_port
    dcc_mtb
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//load data from the file
procedure TBooster.LoadDataFromFile(var ini:TMemIniFile;const section:string);
begin
 Self.Settings.id     := section;
 Self.Settings.Name   := ini.ReadString(section,'name','booster');
 Self.Settings.bclass := TBoosterClass(ini.ReadInteger(section,'class',0));

 Self.Settings.MTB.Zkrat.board  := ini.ReadInteger(section,'zkr_mtb',0);
 Self.Settings.MTB.Zkrat.port   := ini.ReadInteger(section,'zkr_port',0);

 Self.Settings.MTB.Napajeni.board  := ini.ReadInteger(section,'nap_mtb',0);
 Self.Settings.MTB.Napajeni.port   := ini.ReadInteger(section,'nap_port',0);

 Self.Settings.MTB.DCC.board  := ini.ReadInteger(section,'dcc_mtb',0);
 Self.Settings.MTB.DCC.port   := ini.ReadInteger(section,'dcc_port',0);

 MTB.SetNeeded(Self.Settings.MTB.Napajeni.board);
 MTB.SetNeeded(Self.Settings.MTB.Zkrat.board);
end;//procedure

//save data to the file
procedure TBooster.SaveDataToFile(var ini:TMemIniFile;const section:string);
begin
 ini.WriteString(section, 'name', Self.Settings.Name);
 ini.WriteInteger(section, 'class', Integer(Self.Settings.bclass));

 ini.WriteInteger(section, 'zkr_mtb', Self.Settings.MTB.Zkrat.board);
 ini.WriteInteger(section, 'zkr_port', Self.Settings.MTB.Zkrat.port);

 ini.WriteInteger(section, 'nap_mtb', Self.Settings.MTB.Napajeni.board);
 ini.WriteInteger(section, 'nap_port', Self.Settings.MTB.Napajeni.port);

 ini.WriteInteger(section, 'dcc_mtb', Self.Settings.MTB.DCC.board);
 ini.WriteInteger(section, 'dcc_port', Self.Settings.MTB.DCC.port);

 ini.UpdateFile();
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//gets data from MTB

function TBooster.GetZkrat():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not MTB.ready) or (not MTB.Started)) then Exit(TBoosterSignal.undef);

 //if not a power, not a overload
 if (Self.napajeni = TBoosterSignal.error) then Exit(TBoosterSignal.undef);

 try
   val := MTB.GetInput(Self.Settings.MTB.Zkrat.board, Self.Settings.MTB.Zkrat.port);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailable)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;//function

function TBooster.GetNapajeni():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not MTB.ready) or (not MTB.Started)) then Exit(TBoosterSignal.undef);

 try
   val := MTB.GetInput(Self.Settings.MTB.Napajeni.board,Self.Settings.MTB.Napajeni.port);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailable)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;//function

function TBooster.GetDCC():TBoosterSignal;
var val:TRCSInputState;
begin
 if ((not MTB.ready) or (not MTB.Started)) then Exit(TBoosterSignal.undef);

 // DCC nemusi byt detekovano (to se pozna tak, ze MTB board = 0)
 if (Self.Settings.MTB.DCC.board = 0) then Exit(TBoosterSignal.undef);
 try
   val := MTB.GetInput(Self.Settings.MTB.DCC.board, Self.Settings.MTB.DCC.port);
 except
   Exit(TBoosterSignal.undef);
 end;

 if ((val = failure) or (val = notYetScanned) or (val = unavailable)) then
   Result := TBoosterSignal.undef
 else if (val = isOn) then
   Result := TBoosterSignal.error
 else
   Result := TBoosterSignal.ok;
end;

////////////////////////////////////////////////////////////////////////////////

function TBooster.GetDefined():boolean;
begin
 Result := ((MTB.IsModule(Self.Settings.MTB.Zkrat.board)) and (MTB.IsModule(Self.Settings.MTB.Napajeni.board)));
end;

////////////////////////////////////////////////////////////////////////////////

function TBooster.GetDCCDetection():boolean;
begin
 Result := (Self.Settings.MTB.DCC.board > 0);
end;

////////////////////////////////////////////////////////////////////////////////

class function TBooster.GetBClassString(b_type:TBoosterClass):string;
begin
 case (b_type) of
  TBoosterClass.SPAX : Result := 'SPAX';
 else
  Result := '';
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
