unit Booster;

//unit, that defines booter (for example SPAX) as a class

interface

uses IniFiles, TechnologieMTB, SysUtils;

type
 TBoosterChangeEvent = procedure (Sender:TObject;state:boolean) of object;

 TBoosterClass = (undefinned = -1, default = 0, SPAX = 1);

 TBoosterSettings = record
   bclass:TBoosterClass;                              //booster class
   Name:string;                                       //booster name
   MTB:record                                         //MTB inputs
    Zkrat:TMTBAddr;                                     //overload input
    Napajeni:TMTBAddr;                                  //power input
   end;
 end;//TBoosterSettings

 TBooster = class
  private
   Settings:TBoosterSettings;

   ZkratOld,NapajeniOld:boolean;                      //old states (used to calling events)

   //events
   FOnNapajeniChange : TBoosterChangeEvent;
   FOnZkratChange    : TBoosterChangeEvent;

    function GetZkrat():boolean;
    function GetNapajeni():boolean;

  public

    constructor Create(); overload;
    constructor Create(var ini:TMemIniFile;const section:string); overload;
    destructor Destroy(); override;

    procedure Update();                              //update data; events are controlled only here

    procedure LoadDataFromFile(var ini:TMemIniFile;const section:string);
    procedure SaveDataToFile(var ini:TMemIniFile;const section:string);

    property zkrat:boolean read GetZkrat;
    property napajeni:boolean read GetNapajeni;

    property bSettings:TBoosterSettings read settings write settings;

    property OnNapajeniChange:TBoosterChangeEvent read FOnNapajeniChange write FOnNapajeniChange;
    property OnZkratChange:TBoosterChangeEvent read FOnZkratChange write FOnZkratChange;

    class function GetBClassString(b_type:TBoosterClass):string;          //get booster name as a string
 end;//TBooster

implementation

uses GetSystems;

//file format:
//  ini file
//  [B0, B1, ... Bn]
//    name
//    class
//    zkr_mtb
//    zkr_port
//    nap_mtb
//    nap_port

////////////////////////////////////////////////////////////////////////////////

//ctor
constructor TBooster.Create();
begin
 inherited Create;
end;//ctor

//ctor
constructor TBooster.Create(var ini:TMemIniFile;const section:string);
begin
 inherited Create;

 Self.LoadDataFromFile(ini,section);
end;//ctor

//dtor
destructor TBooster.Destroy();
begin
 inherited Destroy;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TBooster.Update();
var state:boolean;
begin
 if (not GetFunctions.GetSystemStart()) then
  begin
   Self.ZkratOld := false;
   Exit();
  end;

 //update napajeni
 state := Self.GetNapajeni();
 if (state <> Self.NapajeniOld) then
  begin
   if (Assigned(Self.FOnNapajeniChange)) then Self.FOnNapajeniChange(Self,state);
   Self.NapajeniOld := state;
  end;

 //if not napajeni, zkrat is not updated
 if (not Self.NapajeniOld) then Exit;

 //update zkrat
 state := Self.GetZkrat();
 if (state <> Self.ZkratOld) then
  begin
   if (Assigned(Self.FOnZkratChange)) then Self.FOnZkratChange(Self, state);
   Self.ZkratOld := state;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//load data from the file
procedure TBooster.LoadDataFromFile(var ini:TMemIniFile;const section:string);
begin
 Self.Settings.Name   := ini.ReadString(section,'name','booster');
 Self.Settings.bclass := TBoosterClass(ini.ReadInteger(section,'class',0));

 Self.Settings.MTB.Zkrat.board  := ini.ReadInteger(section,'zkr_mtb',0);
 Self.Settings.MTB.Zkrat.port   := ini.ReadInteger(section,'zkr_port',0);

 Self.Settings.MTB.Napajeni.board  := ini.ReadInteger(section,'nap_mtb',0);
 Self.Settings.MTB.Napajeni.port   := ini.ReadInteger(section,'nap_port',0);

 MTB.SetNeeded(Self.Settings.MTB.Napajeni.board);
 MTB.SetNeeded(Self.Settings.MTB.Zkrat.board);
end;//procedure

//save data to the file
procedure TBooster.SaveDataToFile(var ini:TMemIniFile;const section:string);
begin
 ini.WriteString(section,'name',Self.Settings.Name);
 ini.WriteInteger(section,'class',Integer(Self.Settings.bclass));

 ini.WriteInteger(section,'zkr_mtb',Self.Settings.MTB.Zkrat.board);
 ini.WriteInteger(section,'zkr_port',Self.Settings.MTB.Zkrat.port);

 ini.WriteInteger(section,'nap_mtb',Self.Settings.MTB.Napajeni.board);
 ini.WriteInteger(section,'nap_port',Self.Settings.MTB.Napajeni.port);

 ini.UpdateFile();
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//gets data from MTB

function TBooster.GetZkrat():boolean;
begin
 //if not a power, not a overload
 if (not Self.NapajeniOld) then Exit(false);

 case (MTB.GetInput(Self.Settings.MTB.Zkrat.board, Self.Settings.MTB.Zkrat.port)) of
  1:Result := true;
 else
  Result := false;
 end;
end;//function

function TBooster.GetNapajeni():boolean;
begin
 case (MTB.GetInput(Self.Settings.MTB.Napajeni.board,Self.Settings.MTB.Napajeni.port)) of
  1:Result := false;
 else
  Result := true;
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TBooster.GetBClassString(b_type:TBoosterClass):string;
begin
 case (b_type) of
  TBoosterClass.SPAX : Result := 'SPAX KMŽ Brno I';
 else
  Result := '';
 end;
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
