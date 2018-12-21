unit RegulatorTCP;

// Trida TTCPRegulator se stara o komunikaci s regulatory -- klienty.
//  napr. regulator Jerry.

interface

uses Classes, IdContext, AnsiStrings, StrUtils, SysUtils, Forms, THnaciVozidlo;

type

TLokResponseData = record
 addr:Word;
 conn:TIDContext;
end;


TTCPRegulator = class
  private
    procedure ParseGlobal(Sender:TIdContext; parsed:TStrings);
    procedure ParseLoko(Sender:TIdContext; parsed:TStrings);

    procedure ClientAuthorise(conn:TIdContext; state:boolean; user:TObject; comment:string='');
    procedure ClientError(conn:TIdContext; error:string);

    procedure PanelLOKResponseOK(Sender:TObject; Data:Pointer);
    procedure PanelLOKResponseErr(Sender:TObject; Data:Pointer);

  public

    procedure Parse(Sender:TIdContext; parsed:TStrings);

    procedure LokUpdateFunc(HV:THV; exclude:TObject = nil);
    procedure LokUpdateSpeed(HV:THV; exclude:TObject = nil);
    procedure LokStolen(HV:THV; exclude:TObject = nil);
    procedure LokUpdateRuc(HV:THV);

    procedure LokToRegulator(Regulator:TIDContext; HV:THV);
    procedure RegDisconnect(reg:TIdContext);

end;

var
  TCPRegulator : TTCPRegulator;

implementation

uses UserDb, User, TCPServerOR,  Trakce, THVDatabase, SprDb,
     Souprava, fRegulator, TrakceGUI, fMain, Prevody, TOblRizeni, TOblsRizeni;


////////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;"

procedure TTCPRegulator.Parse(Sender:TIdContext; parsed:TStrings);
begin
 parsed[2] := UpperCase(parsed[2]);
 if (parsed[2] = 'G') then
  Self.ParseGlobal(Sender, parsed)
 else
  Self.ParseLoko(Sender, parsed);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;G;"

procedure TTCPRegulator.ParseGlobal(Sender:TIdContext; parsed:TStrings);
var user:TUser;
    OblR:TOR;
begin
 parsed[3] := UpperCase(parsed[3]);

 if (parsed[3] = 'AUTH') then
  begin
   // pozadavek na autorizaci klienta
   try
     if (parsed[4] = '') then
      begin
       Self.ClientAuthorise(Sender, false, nil, 'Uživatel odhlášen');
       Exit();
      end;

     user := UsrDb.GetUser(parsed[4]);

     // kontrola existence uzivatele
     if (not Assigned(user)) then
      begin
       Self.ClientAuthorise(Sender, false, nil, 'Uživatel '+parsed[4]+' neexistuje !');
       Exit();
      end;

     // kontrola BANu uzivatele
     if (user.ban) then
      begin
       Self.ClientAuthorise(Sender, false, nil, 'Uživatel '+user.id+' má BAN !');
       Exit();
      end;

     if (not user.regulator) then
      begin
       Self.ClientAuthorise(Sender, false, nil, 'Uživatel '+user.id+' nemá právo k øízení lokomotiv !');
       Exit();
      end;

     // kontrola hesla
     if (TUser.ComparePasswd(parsed[5], user.password, user.salt)) then
      begin
       Self.ClientAuthorise(Sender, true, user);
      end else begin
       Self.ClientAuthorise(Sender, false, nil, 'Špatné heslo');
      end;
   except
    // error pri parsovani -> oznamime chybu
    Self.ClientAuthorise(Sender, false, nil, 'Neplatné argumenty');
   end;
  end;

 // kontrola autorizace (dalsi prikazy jsou podmineny existujicim pristupem)
 if (not (Sender.Data as TTCPORsRef).regulator) then
  begin
   ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Uživatel neuatorizován!');
   Exit();
  end;

 // regulator zacina zadost o lokomotivu ze stanice
 //   -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
 // odpoved od serveru:
 //   -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
 if (parsed[3] = 'PLEASE') then
  begin
   try
     if ((Sender.Data as TTCPORsRef).regulator_zadost <> nil) then
      begin
       ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Žádost již probíhá');
       Exit();
      end;
     ORs.GetORByIndex(ORs.GetORIndex(parsed[4]), OblR);
     if (OblR = nil) then
      begin
       ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Tato oblast øízení neexistuje');
       Exit();
      end;
     if (OblR.reg_please <> nil) then
      begin
       ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Do oblasti øízení již probíhá žádost');
       Exit();
      end;

     (Sender.Data as TTCPORsRef).regulator_zadost := OblR;
     if (parsed.Count > 5) then
       OblR.LokoPlease(Sender, (Sender.Data as TTCPORsRef).regulator_user, parsed[5])
     else
       OblR.LokoPlease(Sender, (Sender.Data as TTCPORsRef).regulator_user, '');

     ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok');
   except
    ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;err;Nesprávný formát pøíkazu');
   end;
  end

 // regulator rusi zadost o lokomotivu ze stanice
 else if (parsed[3] = 'CANCEL') then
  begin
   if ((Sender.Data as TTCPORsRef).regulator_zadost = nil) then Exit();

   (Sender.Data as TTCPORsRef).regulator_zadost.LokoCancel(Sender);
   (Sender.Data as TTCPORsRef).regulator_zadost := nil;
   ORTCPServer.SendLn(Sender, '-;LOK;G;PLEASE-RESP;ok;Žádost zrušena');
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// parsing dat s prefixem "-;LOK;addr;"

procedure TTCPRegulator.ParseLoko(Sender:TIdContext; parsed:TStrings);
var HV:THV;
    left, right, i:Integer;
    data:TStrings;
    Func:TFunkce;
    LokResponseData:Pointer;
begin
{
 Prikazy od klienta:
  -:LOK;addr;PLEASE;token               - zadost o rizeni konkertni lokomotivy; token neni potreba pripojovat v pripade, kdy loko uz mame autorizovane a bylo nam ukradeno napriklad mysi
  -;LOK;addr;RELEASE                    - uvolneni lokomotivy z rizeni regulatoru
  -;LOK;addr;SP;sp_km/h                 - nastaveni rychlosti lokomotivy
  -;LOK;addr;SPD;sp_km/h;dir ()         - nastaveni rychlosti a smeru lokomotivy
  -;LOK;addr;D;dir ()                   - nastaveni smeru lokomotivy
  -;LOK;addr;F;F_left-F_right;states    - nastaveni funkci lokomotivy
    napr.; or;LOK;F;0-4;00010 nastavi F3 a ostatni F vypne
  -;LOK;addr;STOP;                      - nouzove zastaveni
  -;LOK;addr;TOTAL;[0,1]                - nastaveni totalniho rizeni hnaciho vozidla
  -;LOK;addr:ASK                        - tazani se na existenci HV s adresou \addr; pokud existuje, chci o nem vedet data
}

 parsed[3] := UpperCase(parsed[3]);

 try
   HV := HVDb.HVozidla[StrToInt(parsed[2])];
 except
   Self.ClientError(Sender, 'Nesprávný formát adresy loko');
   Exit();
 end;

 if (parsed[3] = 'ASK') then
  begin
   // dotaz na existenci hnaciho vozidla
   if (HV <> nil) then
    begin
      ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';FOUND;{'+HV.GetPanelLokString()+'}');
    end else begin
      ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';NOT-FOUND');
    end;

   Exit();
  end;

 if (not Assigned(HV)) then
  begin
   Self.ClientError(Sender, 'Loko neexistuje');
   Exit();
  end;

 // kontrola opravneni
 if (not (Sender.Data as TTCPORsRef).regulator) then Exit();

 if (parsed[3] = 'RELEASE') then
  begin
   // regulator ukoncuje rizeni LOKO
   HV.RemoveRegulator(Sender);
   TTCPORsRef(Sender.Data).regulator_loks.Remove(HV);
   ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';AUTH;release;Loko odhlášeno');
   ORTCPServer.GUIQueueLineToRefresh(TTCPORsRef(Sender.Data).index);
   Exit();
  end;

 if (parsed[3] = 'PLEASE') then
  begin
   // zadost o prime prevzeti loko -> kontrola nalezitosti

   try
     // autorizovany regulator, ci regulator uzivatele root se nemusi prokazovat tokenem
     if ((not (Sender.Data as TTCPORsRef).regulator_user.root) and (not HV.IsReg(Sender))) then
      begin
       // je loko uz na nejakem nerootovskem (!) ovladaci -> odmitnout
       for i := 0 to HV.Stav.regulators.Count-1 do
         if (not HV.Stav.regulators[i].root) then
          begin
           ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';AUTH;not;Loko je otevøené v jiném regulátoru');
           Exit();
          end;

       // kontrola tokenu
       if (not HV.IsToken(parsed[4])) then
        begin
         ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';AUTH;not;Špatný token');
         Exit();
        end;
      end;//if regulator_user_root


     if (parsed.Count > 4) then HV.RemoveToken(parsed[4]);

     Self.LokToRegulator(Sender, HV);

     Exit();

   except
    ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';AUTH;not;Nesprávný formát pøíkazu');
   end;
  end;//PLEASE

 // DALSI PRIKAZY JSOU PODMINENY PREVZETIM HNACIHO VOZIDLA :

 // je tento regulator uz v seznamu regulatoru?
 if (not HV.IsReg(Sender)) then
  begin
   ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';RESP;err;Loko '+parsed[2]+' neautorizováno');
   Exit();
  end;

 if (HV.Slot.stolen) then
  begin
   ORTCPServer.SendLn(Sender, '-;LOK;'+parsed[2]+';RESP;err;Loko '+parsed[2]+' ukradeno, nenastavuji');
   Exit();
  end;

 // tady mame jisto, ze je loko autorizovano a ze je mone ho ridit

 if (parsed[3] = 'SP') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetSpeed(Sender, HV, StrToInt(parsed[4]));
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'SPD') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetSpeed(Sender, HV, StrToInt(parsed[4]), StrToInt(parsed[5]));
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'SP-S') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetDirectSpeed(Sender, HV, StrToInt(parsed[4]));
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'SPD-S') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetDirectSpeed(Sender, HV, StrToInt(parsed[4]), StrToInt(parsed[5]));
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'D') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetDirectSpeed(Sender, HV, HV.Slot.speed, StrToInt(parsed[4]));
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'F') then
  begin
   data := TStringList.Create();
   ExtractStrings(['-'], [], PChar(parsed[4]), data);
   left := StrToInt(data[0]);
   if (data.Count > 1) then
    right := StrToInt(data[1])
   else
    right := left;
   data.Free();

   Func := HV.Slot.funkce;
   for i := left to right do
     Func[i] := PrevodySoustav.StrToBool(parsed[5][i-left+1]);

   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.LokSetFunc(Sender, HV, Func);
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'STOP') then
  begin
   GetMem(LokResponseData, SizeOf(TLokResponseData));
   TLokResponseData(LokResponseData^).addr := HV.adresa;
   TLokResponseData(LokResponseData^).conn := Sender;
   TrkSystem.callback_ok  := TTrakce.GenerateCallback(Self.PanelLOKResponseOK, LokResponseData);
   TrkSystem.callback_err := TTrakce.GenerateCallback(Self.PanelLOKResponseErr, LokResponseData);

   try
     TrkSystem.EmergencyStopLoko(Sender, HV);
   except
     Self.PanelLOKResponseErr(Self, LokResponseData);
   end;
  end

 else if (parsed[3] = 'TOTAL') then
   HV.ruc := (parsed[4] = '1');

end;//procedure

////////////////////////////////////////////////////////////////////////////////

// je volano, pokud chceme rict klientovi, ze jsme mu zmenili stav autorizace
procedure TTCPRegulator.ClientAuthorise(conn:TIdContext; state:boolean; user:TObject; comment:string='');
var str:string;
begin
 (conn.Data as TTCPORsRef).regulator := state;
 (conn.Data as TTCPORsRef).regulator_user := TUser(user);

 if (state) then
  begin
   if (Assigned(user)) then
     str := TUser(user).id
   else
     str := 'ano';

   F_Main.LV_Clients.Items.Item[(conn.Data as TTCPORsRef).index].SubItems.Strings[10] := str;
   ORTCPServer.SendLn(conn, '-;LOK;G;AUTH;ok;'+comment)
  end else begin
   (conn.Data as TTCPORsRef).regulator_user := nil;
   F_Main.LV_Clients.Items.Item[(conn.Data as TTCPORsRef).index].SubItems.Strings[10] := '';
   ORTCPServer.SendLn(conn, '-;LOK;G;AUTH;not;'+comment);

   // odhlasime vsechny prihlasene regulatory
   TTCPORsRef(conn.Data).regulator_loks.Clear();
   HVDb.RemoveRegulator(conn);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.ClientError(conn:TIdContext; error:string);
begin
 try
   ORTCPServer.SendLn(conn, '-;LOK;G;ERR;'+error)
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.PanelLOKResponseOK(Sender:TObject; Data:Pointer);
begin
//  -;LOK;RESP;ADDR;[ok, err]; info
 try
  ORTCPServer.SendLn(TLokResponseData(Data^).conn, '-;LOK;'+IntToStr(TLokResponseData(Data^).addr)+
      ';RESP;ok;'+IntToStr(TrkSystem.GetStepSpeed(HVDb.HVozidla[TLokResponseData(Data^).addr].Slot.speed)));
  FreeMem(data);
 except

 end;
end;//procedure

procedure TTCPRegulator.PanelLOKResponseErr(Sender:TObject; Data:Pointer);
begin
 try
  ORTCPServer.SendLn(TLokResponseData(Data^).conn, '-;LOK;'+IntToStr(TLokResponseData(Data^).addr)+';RESP;err;Command error;');
  FreeMem(data);
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//  or;LOK;ADDR;F;F_left-F_right;states          - informace o stavu funkci lokomotivy
//    napr.; or;LOK;ADDR;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
procedure TTCPRegulator.LokUpdateFunc(HV:THV; exclude:TObject = nil);
var i:Integer;
    func:string;
begin
 func := '';
 for i := 0 to _HV_FUNC_MAX do
  case (HV.Slot.funkce[i]) of
   false : func := func + '0';
   true  : func := func + '1';
  end;//case

 for i := 0 to HV.Stav.regulators.Count-1 do
   if (HV.Stav.regulators[i].conn <> exclude) then
     ORTCPServer.SendLn(HV.Stav.regulators[i].conn, '-;LOK;'+IntToStr(HV.adresa)+';F;0-'+IntToStr(_HV_FUNC_MAX)+';'+func+';');
end;//procedure

//  or;LOK;ADDR;SPD;sp_km/h;sp_stupne;dir
procedure TTCPRegulator.LokUpdateSpeed(HV:THV; exclude:TObject = nil);
var i:Integer;
begin
 for i := 0 to HV.Stav.regulators.Count-1 do
   if (HV.Stav.regulators[i].conn <> exclude) then
     ORTCPServer.SendLn(HV.Stav.regulators[i].conn, '-;LOK;'+IntToStr(HV.adresa)+';SPD;'+IntToStr(TrkSystem.GetStepSpeed(HV.Slot.speed))+';'+IntToStr(HV.Slot.speed)+';'+IntToStr(HV.Slot.smer)+';');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.LokStolen(HV:THV; exclude:TObject = nil);
var i:Integer;
begin
 for i := 0 to HV.Stav.regulators.Count-1 do
   if (HV.Stav.regulators[i].conn <> exclude) then
     ORTCPServer.SendLn(HV.Stav.regulators[i].conn, '-;LOK;'+IntToStr(HV.adresa)+';AUTH;stolen;Loko ukradeno ovladaèem');
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TTCPRegulator.LokUpdateRuc(HV:THV);
var i:Integer;
    state:string;
begin
 if (HV.ruc) then
   state := '1'
 else
   state := '0';

 for i := 0 to HV.Stav.regulators.Count-1 do
   ORTCPServer.SendLn(HV.Stav.regulators[i].conn, '-;LOK;'+IntToStr(HV.adresa)+';TOTAL;'+state);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// prirazeni lokomotivy regulatoru
procedure TTCPRegulator.LokToRegulator(Regulator:TIDContext; HV:THV);
var pom:boolean;
    i:Integer;
    reg:THVRegulator;
    timeout:Integer;
    tmpHV:THV;
begin
 // je tento regulator uz v seznamu regulatoru?
 pom := false;
 for i := 0 to HV.Stav.regulators.Count-1 do
   if (HV.Stav.regulators[i].conn = Regulator) then
    begin
     pom := true;
     reg := HV.Stav.regulators[i];
     break;
    end;

 // ne -> pridat do seznamu regulatoru
 if (not pom) then
  begin
   reg.conn := Regulator;
   HV.ruc   := HV.ruc or (HV.Stav.souprava = -1);
   reg.root := (Regulator.Data as TTCPORsRef).regulator_user.root;
   HV.Stav.regulators.Add(reg);
  end;

 // Je loko prevzato?
 if (not HV.Slot.prevzato) then
  begin
   // ne -> prevzit loko
   try
     TrkSystem.PrevzitLoko(HV);
   except
     on E:Exception do
      begin
       ORTCPServer.SendLn(Regulator, '-;LOK;'+IntToStr(HV.adresa)+';AUTH;not;Pøevzetí z centrály se nezdaøilo :'+E.Message);
       HV.Stav.regulators.Remove(reg);
      end;
   end;

   // timeout 3000ms = 3s
   timeout := 0;
   while ((not HV.Slot.Prevzato) or (HV.Slot.pom = TPomStatus.progr) or (HV.Slot.pom = TPomStatus.error)) do
    begin
     Sleep(1);
     timeout := timeout + 1;
     Application.ProcessMessages;

     if (timeout > 3000) then
      begin
       ORTCPServer.SendLn(Regulator, '-;LOK;'+IntToStr(HV.adresa)+';AUTH;not;Pøevzetí z centrály se nezdaøilo');
       HV.Stav.regulators.Remove(reg);
       Exit();
      end;
    end;//while
  end else begin
   // odpoved na pozadavek o autorizaci rizeni hnaciho vozidla
   // kdyz loko prebirame, je odesilana automaticky
   if (HV.ruc) then
     ORTCPServer.SendLn(Regulator, '-;LOK;'+IntToStr(HV.adresa)+';AUTH;total;{'+HV.GetPanelLokString()+'}')
   else
     ORTCPServer.SendLn(Regulator, '-;LOK;'+IntToStr(HV.adresa)+';AUTH;ok;{'+HV.GetPanelLokString()+'}');
  end;

  // pridani loko do seznamu autorizovanych loko klientem

  pom := false;
  for tmpHV in TTCPORsRef(Regulator.Data).regulator_loks do
   begin
    if (tmpHV = HV) then
     begin
      pom := true;
      break;
     end;
   end;

  if (not pom) then
   begin
    // pridani nove loko do seznamu
    TTCPORsRef(Regulator.Data).regulator_loks.Add(HV);
    ORTCPServer.GUIQueueLineToRefresh(TTCPORsRef(Regulator.Data).index);
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// odhlasit vsechna hnaci vozidla regulatoru

procedure TTCPRegulator.RegDisconnect(reg:TIdContext);
var addr:Integer;
begin
 for addr := 0 to _MAX_ADDR-1 do
   if ((HVDb.HVozidla[addr] <> nil) and (HVDb.HVozidla[addr].Stav.regulators.Count > 0)) then
     HVDb.HVozidla[addr].RemoveRegulator(reg);

 TTCPORsRef(reg.Data).regulator := false;
 TTCPORsRef(reg.Data).regulator_user := nil;
 TTCPORsRef(reg.Data).regulator_loks.Clear();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  TCPRegulator := TTCPRegulator.Create();

finalization
  TCPRegulator.Free();

end.//unit
