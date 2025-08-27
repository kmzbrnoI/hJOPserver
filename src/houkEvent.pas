unit houkEvent;

{
  Trida THoukEv reprezentuje jednu houkaci udalost.

  Definicni string:
  "udalost;typ_funkce;zvuk;"
}

interface

uses rrEvent, Classes, SysUtils, ExtCtrls;

type
  THoukFuncType = (hftNothihg = 0, // udalost nema zadny efekt, je jen prerekvizitou pro dalsi udalost
    hftToggle = 1, // udalost vypne a zapne urcity zvuk
    hftOn = 2, // udalost zapne zvuk a necha jej zaply
    hftOff = 3 // udelat vypne zvuk a necha jej vyply
    );

  THoukEv = class
  private
    m_event: TRREv;
    m_sound: string;
    m_funcType: THoukFuncType;

    procedure LoadFromDefString(data: string);
    function IsEnabled(): Boolean;

    procedure FireEvent(train: TObject);

  public

    constructor Create(data: string); overload;
    constructor Create(event: TRREv; sound: string; funcType: THoukFuncType); overload;
    destructor Destroy(); override;

    function GetDefString(): string;

    procedure Register(traini: Cardinal);
    procedure Unregister();
    function CheckTriggerred(Sender: TObject): Boolean; // returns true when event triggerred

    property enabled: Boolean read IsEnabled;
    property sound: string read m_sound;
    property funcType: THoukFuncType read m_funcType;
    property event: TRREv read m_event;

  end;

implementation

uses ownStrUtils, TrakceC, train, BlockTrack, TrainDb, fMain,
  THVDatabase;

/// /////////////////////////////////////////////////////////////////////////////

constructor THoukEv.Create(data: string);
begin
  inherited Create();

  try
    Self.LoadFromDefString(data);
  except
    if (Assigned(m_event)) then
      m_event.Free();
    raise;
  end;
end;

constructor THoukEv.Create(event: TRREv; sound: string; funcType: THoukFuncType);
begin
  inherited Create();

  Self.m_event := event;
  Self.m_sound := sound;
  Self.m_funcType := funcType;
end;

destructor THoukEv.Destroy();
begin
  Self.m_event.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THoukEv.LoadFromDefString(data: string);
var str: TStrings;
begin
  str := TStringList.Create();

  try
    ExtractStringsEx([';', ','], [], data, str);

    m_event := TRREv.Create(false, str[0]);

    if (str.Count > 2) then
      m_sound := str[2]
    else
      m_sound := '';

    try
      m_funcType := THoukFuncType(StrToInt(str[1]));
    except
      m_funcType := hftToggle;
    end;

  finally
    str.Free();
  end;
end;

function THoukEv.GetDefString(): string;
begin
  Result := '{' + Self.m_event.GetDefStr() + '},' + IntToStr(Integer(Self.m_funcType)) + ',' + Self.m_sound;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THoukEv.Register(traini: Cardinal);
begin
  m_event.Register(traini);
end;

procedure THoukEv.Unregister();
begin
  m_event.Unregister();
end;

/// /////////////////////////////////////////////////////////////////////////////

function THoukEv.CheckTriggerred(Sender: TObject): Boolean;
begin
  if (Self.m_event.IsTriggerred(Sender, false)) then
  begin
    Self.m_event.Unregister();
    if (TBlkTrack(Sender).IsTrain()) then
      Self.FireEvent(TBlkTrack(Sender).trainL);
    Result := true;
  end
  else
    Result := false
end;

/// /////////////////////////////////////////////////////////////////////////////

function THoukEv.IsEnabled(): Boolean;
begin
  Result := Self.m_event.enabled;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THoukEv.FireEvent(train: TObject);
begin
  case (Self.m_funcType) of
    hftToggle:
      TTrain(train).ToggleHouk(Self.m_sound);
    hftOn:
      TTrain(train).SetHoukState(Self.m_sound, true);
    hftOff:
      TTrain(train).SetHoukState(Self.m_sound, false);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
