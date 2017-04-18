unit houkEvent;

{
  Trida THoukEv reprezentuje jednu houkaci udalost.

  Definicni string:
   "udalost;typ_funkce;zvuk;"
}

interface

uses rrEvent, Classes, SysUtils, ExtCtrls;

type
  THoukFuncType = (
    hftNothihg = 0,   // udalost nema zadny efekt, je jen prerekvizitou pro dalsi udalost
    hftToggle = 1,    // udalost vypne a zapne urcity zvuk
    hftOn = 2,        // udalost zapne zvuk a necha jej zaply
    hftOff = 3        // udelat vypne zvuk a necha jej vyply
  );

  THoukEv = class
   private
    m_event: TRREv;
    m_sound: string;
    m_funcType: THoukFuncType;
    m_sprref: TObject;

     procedure LoadFromDefString(data:string);
     function IsEnabled():boolean;

     procedure FireEvent(Souprava:TObject);

   public

     constructor Create(data:string);
     destructor Destroy(); override;

     function GetDefString():string;

     procedure Register();
     procedure Unregister();
     function CheckTriggerred(Sender:TObject):boolean; // returns true when event triggerred

     property enabled: boolean read IsEnabled;

  end;

implementation

uses ownStrUtils, TrakceGUI, Souprava, TBlokUsek, SprDb, fMain, THnaciVozidlo,
      Trakce, THVDatabase;

////////////////////////////////////////////////////////////////////////////////

constructor THoukEv.Create(data:string);
begin
 inherited Create();

 Self.m_sprref := nil;

 try
   Self.LoadFromDefString(data);
 except
   if (Assigned(m_event)) then m_event.Free();
   raise;
 end;
end;

destructor THoukEv.Destroy();
begin
 Self.m_event.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THoukEv.LoadFromDefString(data:string);
var str:TStrings;
begin
 str := TStringList.Create();

 try
   ExtractStringsEx([';'], [], data, str);

   m_event := TRREv.Create(str[0]);

   if (str.Count > 1) then
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

function THoukEv.GetDefString():string;
begin
 Result := '{' + Self.m_event.GetDefStr() + '};' + IntToStr(Integer(Self.m_funcType)) +
           ';' + Self.m_sound;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THoukEv.Register();
begin
 m_event.Register();
end;

procedure THoukEv.Unregister();
begin
 m_event.Unregister();
end;

////////////////////////////////////////////////////////////////////////////////

function THoukEv.CheckTriggerred(Sender:TObject):boolean;
begin
 if (Self.m_event.IsTriggerred(Sender, false)) then
  begin
   Self.m_event.Unregister();
   if (TBlkUsek(Sender).Souprava >= 0) then
     Self.FireEvent(Soupravy[TBlkUsek(Sender).Souprava]);
   Result := true;
  end else
   Result := false
end;

////////////////////////////////////////////////////////////////////////////////

function THoukEv.IsEnabled():boolean;
begin
 Result := Self.m_event.enabled;
end;

////////////////////////////////////////////////////////////////////////////////

procedure THoukEv.FireEvent(Souprava:TObject);
var i:Integer;
    HV:THV;
    func:TFunkce;
begin
 case (Self.m_funcType) of
   hftToggle: begin
     for i := 0 to TSouprava(Souprava).sdata.HV.cnt-1 do
      begin
       HV := HVDb.HVozidla[TSouprava(Souprava).sdata.HV.HVs[i]];
       if ((HV.Stav.regulators.Count > 0) or (HV.Slot.stolen) or
           ((HV.funcDict.ContainsKey('zvuk')) and (not HV.Stav.funkce[HV.funcDict['zvuk']])) or
           (not HV.funcDict.ContainsKey(Self.m_sound))) then continue;

       TrkSystem.LokFuncToggle(Self, HV, HV.funcDict[Self.m_sound]);
      end;
   end;

   hftOn: begin
     for i := 0 to TSouprava(Souprava).sdata.HV.cnt-1 do
      begin
       HV := HVDb.HVozidla[TSouprava(Souprava).sdata.HV.HVs[i]];
       if ((HV.Stav.regulators.Count > 0) or (HV.Slot.stolen) or
           ((HV.funcDict.ContainsKey('zvuk')) and (not HV.Stav.funkce[HV.funcDict['zvuk']])) or
           (not HV.funcDict.ContainsKey(Self.m_sound))) then continue;

       func := HV.Stav.funkce;
       func[HV.funcDict[Self.m_sound]] := true;
       TrkSystem.LokSetFunc(Self, HV, func);
      end;
   end;

   hftOff: begin
     for i := 0 to TSouprava(Souprava).sdata.HV.cnt-1 do
      begin
       HV := HVDb.HVozidla[TSouprava(Souprava).sdata.HV.HVs[i]];
       if ((HV.Stav.regulators.Count > 0) or (HV.Slot.stolen) or
           ((HV.funcDict.ContainsKey('zvuk')) and (not HV.Stav.funkce[HV.funcDict['zvuk']])) or
           (not HV.funcDict.ContainsKey(Self.m_sound))) then continue;

       func := HV.Stav.funkce;
       func[HV.funcDict[Self.m_sound]] := false;
       TrkSystem.LokSetFunc(Self, HV, func);
      end;
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
