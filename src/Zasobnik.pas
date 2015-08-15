unit Zasobnik;

// Tato unita implementuje tridu TORStack, ktera resi zasobnik jizdnich cest pro
//   jednu oblast rizeni
// Kazda oblast rizeni ma svuj zasobnik

interface

uses Generics.Collections, Classes, IdContext, SysUtils, UPO, RPConst;

type
  TORStackVolba = (PV = 0, VZ = 1);

  // 1 povel v zasobniku
  // Z teto abstratni tridy dedi konkretni povely.
  TORStackCmd = class abstract
   id:Integer;
  end;

  // povel ke staveni jizdni cesty
  TORStackCmdJC = class(TORStackCmd)
   JC:TObject;
   nouz:boolean;
   Pnl:TIDContext;
  end;

  // povel k zapnuti zadosti o tratovy souhlas
  TORStackCmdZTS = class(TORStackCmd)
   uvazka:TObject;    // TBlkUvazka
  end;

  // povel k udeleni tratoveho souhlasu
  TORStackCmdUTS = class(TORStackCmd)
   uvazka:TObject;    // TBlkUvazka
  end;

  TORStack = class
   private const
    _MAX_STACK_JC = 12;

   private
    OblR:TObject;
    findex:Integer;
    fvolba:TORStackVolba;
    fhint:string;
    stack:TList<TORStackCmd>;
    fUPOenabled:boolean;

      // obsluzne funkce jednotlivych pozadavku z panelu
      procedure ORCmdPV(SenderPnl:TIdContext);
      procedure ORCmdVZ(SenderPnl:TIdContext);
      procedure ORCmdEZ(SenderPnl:TIdContext; show:boolean);
      procedure ORCmdRM(SenderPnl:TIdContext; id:Integer);
      procedure ORCmdUPO(SenderPnl:TIdContext);

      // zmena volby a hintu
      procedure SetVolba(volba:TORStackVolba);
      procedure SetHint(hint:string);
      procedure SetUPOEnabled(enabled:boolean);

      // odeslani seznamu jizdnich cest v zasobniku do prislusne oblasti rizeni
      procedure SendList(connection:TIdContext);

      procedure RemoveFromStack(index:Integer; SenderPnl:TIDContext = nil);

      function GetStackString(cmd:TORStackCmd):string;
      function GetCount():Integer;

      procedure AddCmd(cmd:TORStackCmd);

      procedure ZpracujJC(cmd:TORStackCmdJC);
      procedure ZpracujZTS(cmd:TORStackCmdZTS);
      procedure ZpracujUTS(cmd:TORStackCmdUTS);

   public


      constructor Create(index:Integer; OblR:TObject);
      destructor Destroy(); override;

      procedure ParseCommand(SenderPnl:TIdContext; data:TStrings);
      procedure AddJC(JC:TObject; SenderPnl:TIDContext; nouz:boolean);
      procedure AddZTS(uvazka:TObject; SenderPnl:TIDContext);
      procedure AddUTS(uvazka:TObject; SenderPnl:TIDContext);

      procedure Update();
      procedure NewConnection(SenderPnl:TIdContext);

      procedure RemoveJC(JC:TObject);           // maze prvni nalezenou cestu - tuto metodu vyuziva jizdni cesta pri dokonceni staveni

      procedure ClearStack();                   // mazani zasobniku je volano pri vypnuti systemu
      function GetList():string;

      function IsJCInStack(JC:TObject):boolean;

      property volba:TORStackVolba read fvolba write SetVolba;
      property hint:string read fhint write SetHint;
      property UPOenabled:boolean read fUPOenabled write SetUPOEnabled;
      property Count:Integer read GetCount;

  end;//TORStack

implementation

uses TOblRizeni, TCPServerOR, Logging, TechnologieJC, TBlok, TBloky,
      TBlokUvazka, TBlokTrat;

////////////////////////////////////////////////////////////////////////////////

constructor TORStack.Create(index:Integer; OblR:TObject);
begin
 inherited Create();

 Self.OblR       := OblR;
 Self.findex     := index;
 Self.fvolba     := TORStackVolba.PV;
 Self.UPOenabled := false;

 Self.stack  := TList<TORStackCmd>.Create();
end;//ctor

destructor TORStack.Destroy();
begin
 Self.stack.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ParseCommand(SenderPnl:TIdContext; data:TStrings);
begin
 try
   data[2] := UpperCase(data[2]);

   if (data[2] = 'VZ') then
    Self.ORCmdVZ(SenderPnl)
   else if (data[2] = 'PV') then
    Self.ORCmdPV(SenderPnl)
   else if (data[2] = 'EZ') then
    begin
     if (data[3] = '1') then
      Self.ORCmdEZ(SenderPnl, true)
     else
      Self.ORCmdEZ(SenderPnl, false);
    end
   else if (data[2] = 'RM') then
    Self.ORCmdRM(SenderPnl, StrToInt(data[3]))
   else if (data[2] = 'UPO') then
    Self.ORCmdUPO(SenderPnl);
 except
  on e:Exception do
    writelog('Server: stack data parse error : '+e.Message, WR_ERROR);
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ORCmdPV(SenderPnl:TIdContext);
begin
 Self.volba := PV;
end;//procedure

procedure TORStack.ORCmdVZ(SenderPnl:TIdContext);
begin
 Self.volba := VZ;
end;//procedure

procedure TORStack.ORCmdEZ(SenderPnl:TIdContext; show:boolean);
begin
 if (show) then
   Self.SendList(SenderPnl);
end;//procedure

procedure TORStack.ORCmdRM(SenderPnl:TIdContext; id:Integer);
var i:Integer;
begin
 for i := 0 to Self.stack.Count-1 do
  if (Self.stack[i].id = id) then
   begin
    try
      if ((i = 0) and (Self.stack[i].ClassType = TORStackCmdJC) and (((Self.stack[i] as TORSTackCmdJC).JC as TJC).staveni)) then
       begin
        ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze smazat JC, kter� se stav�');
        Exit();
       end;
    except

    end;

    Self.RemoveFromStack(i, SenderPnl);
    Exit();
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// klik na UPO -> zobrazit upozorneni proc JC nelze postavit
procedure TORStack.ORCmdUPO(SenderPnl:TIdContext);
var cmd:TORStackCmdJC;
begin
 if ((Self.stack.Count = 0) or (Self.stack[0].ClassType <> TORStackCmdJC)) then Exit();

 // ted jsou v ceste jen bariery na potvrzeni -> cestu muzu klasicky zacit stavet pres StavJC:
 (Self.OblR as TOR).BroadcastData('ZAS;FIRST;0');
 Self.UPOenabled := false;
 cmd := (Self.stack[0] as TORSTackCmdJC);
 (cmd.JC as TJC).StavJC(cmd.Pnl, Self.OblR, Self, cmd.nouz);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// Pridani obecneho prikazu do zasobniku:

procedure TORStack.AddCmd(cmd:TORStackCmd);
var description:string;
begin
 if (Self.stack.Count >= _MAX_STACK_JC) then
  begin
   writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - z�sobn�k je pln�, nelze p�idat dal�� p��kaz', WR_STACK);
   raise Exception.Create('Z�sobn�k je pln�');
  end;

 if (Self.stack.Count > 0) then
  cmd.id := Self.stack[Self.stack.Count-1].id + 1
 else
  cmd.id := 0;

 description := Self.GetStackString(cmd);
 Self.stack.Add(cmd);
 (Self.OblR as TOR).BroadcastData('ZAS;ADD;'+IntToStr(cmd.id)+'|'+description);
 writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - : p�id�n p��kaz ' + description + ', id = '+IntToStr(cmd.id), WR_STACK);
 (Self.OblR as TOR).changed := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// Pridani jizdni cesty do zasobniku:
procedure TORStack.AddJC(JC:TObject; SenderPnl:TIDContext; nouz:boolean);
var cmd:TORStackCmdJC;
begin
 cmd := TORStackCmdJC.Create();
 cmd.JC   := JC;
 cmd.Pnl  := SenderPnl;
 cmd.nouz := nouz;

 try
  Self.AddCmd(cmd);
 except
  on E:Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;//procedure

// Pridani zdosti o tratovy souhlas do zasobniku
procedure TORStack.AddZTS(uvazka:TObject; SenderPnl:TIDContext);
var cmd:TORStackCmdZTS;
begin
 cmd := TORStackCmdZTS.Create();
 cmd.uvazka := uvazka;

 try
  Self.AddCmd(cmd);
 except
  on E:Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;//procedure

// Pridani udeleni tratoveho souhlasu do zasobniku:
procedure TORStack.AddUTS(uvazka:TObject; SenderPnl:TIDContext);
var cmd:TORStackCmdUTS;
begin
 cmd := TORStackCmdUTS.Create();
 cmd.uvazka := uvazka;

 try
  Self.AddCmd(cmd);
 except
  on E:Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetVolba(volba:TORStackVolba);
begin
 Self.fvolba := volba;
 case (volba) of
  PV : begin
    (Self.OblR as TOR).BroadcastData('ZAS;PV');
    writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - PV', WR_STACK);
  end;
  VZ : begin
    (Self.OblR as TOR).BroadcastData('ZAS;VZ');
    writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - VZ', WR_STACK);
  end;
 end;//case

 (Self.OblR as TOR).changed := true;
end;//procedure

procedure TORStack.SetHint(hint:string);
begin
 if (hint <> Self.fhint) then
  begin
   Self.fhint := hint;
   (Self.OblR as TOR).BroadcastData('ZAS;HINT;'+hint);
   (Self.OblR as TOR).changed := true;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// tady se resi zpracovani prikazu v zasobniku
procedure TORStack.Update();
begin
 if (Self.stack.Count = 0) then Exit();

 try
   if (Self.stack[0].ClassType = TORStackCmdJC) then
     Self.ZpracujJC(Self.stack[0] as TORStackCmdJC)
   else if (Self.stack[0].ClassType = TORStackCmdZTS) then
     Self.ZpracujZTS(Self.stack[0] as TORStackCmdZTS)
   else if (Self.stack[0].ClassType = TORStackCmdUTS) then
     Self.ZpracujUTS(Self.stack[0] as TORStackCmdUTS);
 except
  on e:Exception do
   begin
    writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - update exception '+e.Message+'; ma�u p��kaz ze z�sobn�ku', WR_ERROR);
    Self.RemoveFromStack(0);
   end;
 end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ZpracujJC(cmd:TORStackCmdJC);
var JC:TJC;
    bariery:TJCBariery;
    i:Integer;
begin
 JC := (cmd.JC as TJC);

 if (JC.staveni) then
  begin
   Self.hint := '';
   Exit();
  end;

  bariery := JC.KontrolaPodminek(cmd.nouz);

  if ((bariery.Count > 0) or (cmd.nouz)) then
   begin
    // v jizdni ceste jsou bariery
    if ((bariery.Count > 0) and (TJC.CriticalBariera(bariery[0].typ))) then
     begin
      // kriticka bariera -> hint := CRIT, kritickou barieru na pozadani zobrazim dispecerovi (pres klik na UPO zasobniku)
      Self.hint       := 'CRIT';
      Self.UPOenabled := true;
      Exit();
     end;

    // tady mame zajisteno, ze v jizdni ceste nejsou kriticke bariery
    //  (KontrolaPodminek() zarucuje, ze kriticke bariery jsou na zacatku seznamu)

    // nyni oznamime nekriticke bariery ktere nemaji upozorneni
    for i := 0 to bariery.Count-1 do
     begin
      if (not JC.WarningBariera(bariery[i].typ)) then
       begin
        // tyto bariery nelze rozkliknout pomoci UPO
        Self.hint := bariery[i].blok.GetGlobalSettings().name;
        Self.UPOenabled := false;
        Exit();
       end;
     end;//for i

    // neupozornovaci bariery nejsou -> podivam se na zbytek barier (ty by mely byt upozornovaci a melo byt se u nich dat kliknout na UPO)
    Self.UPOenabled := true;
    if ((bariery.Count > 0) and (bariery[0].blok <> nil)) then
      Self.hint := bariery[0].blok.GetGlobalSettings().name  // tohleto si muzeme dovolit, protoze mame zajiteno, ze v JC je alespon jedna bariera (viz podminka vyse)
    else
      Self.hint := '';

    Exit();
   end;//if bariery.Count > 0

 // zadne bariery -> stavim jizdni cestu

 writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - JC '+JC.nazev+' : podm�nky spln�ny, stav�m', WR_STACK);

 // pokud nejsou zadne bariery, stavime jizdni cestu
 (Self.OblR as TOR).BroadcastData('ZAS;FIRST;0');
 JC.StavJC(cmd.Pnl, Self.OblR, Self);
 Self.UPOenabled := false;
 bariery.Free();
end;//procedure

procedure TORStack.ZpracujZTS(cmd:TORStackCmdZTS);
var uv:TBlkUvazka;
begin
 uv := (cmd.uvazka as TBlkUvazka);

 Self.UPOenabled := false;
 Self.hint := (uv as TBlk).GetGlobalSettings.name;

 if (uv.CanZTS()) then
  begin
   uv.zadost := true;
   Self.RemoveFromStack(0);
  end;
end;//procedure

procedure TORStack.ZpracujUTS(cmd:TORStackCmdUTS);
var uv:TBlkUvazka;
begin
 uv := (cmd.uvazka as TBlkUvazka);

 Self.UPOenabled := false;
 Self.hint := (uv as TBlk).GetGlobalSettings.name;

 if ((not uv.zadost) and ((uv.parent as TBlkTrat).Zadost)) then
  begin
   uv.UdelSouhlas();
   Self.RemoveFromStack(0);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// odeslani cest v zasobniku
// format dat: {id|name}{id|name} ...
procedure TORStack.SendList(connection:TIdContext);
var str:string;
    i:Integer;
    first_enabled:string;
begin
 if ((Self.stack.Count > 0) and (Self.stack[0].ClassType = TORStackCmdJC) and (not ((Self.stack[0] as TORStackCmdJC).JC as TJC).staveni)) then
  first_enabled := '1'
 else
  first_enabled := '0';

 str := '';
 for i := 0 to Self.stack.Count-1 do
  str := str + '[' + IntToStr(Self.stack[i].id) + '|' + Self.GetStackString(Self.stack[i]) + ']';
 ORTCPServer.SendLn(connection, (Self.OblR as TOR).id+';ZAS;LIST;'+first_enabled+';'+str);
end;//procedure

function TORStack.GetList():string;
var i:Integer;
begin
 Result := '';
 for i := 0 to Self.stack.Count-1 do
   Result := Result + '[' + IntToStr(Self.stack[i].id) + '|' + Self.GetStackString(Self.stack[i]) + ']';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ClearStack();
begin
 Self.stack.Clear();
 (Self.OblR as TOR).BroadcastData('ZAS;LIST;1;;');
 Self.hint := '';
 Self.UPOenabled := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.NewConnection(SenderPnl:TIdContext);
begin
 ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;INDEX;'+IntToStr(Self.findex));
 case (Self.volba) of
  PV : ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;PV');
  VZ : ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;VZ');
 end;
 ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;HINT;'+Self.hint);

 if (Self.UPOenabled) then
  ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;UPO;1')
 else
  ORTCPServer.SendLn(SenderPnl, (Self.OblR as TOR).id+';ZAS;UPO;0');

 Self.SendList(SenderPnl);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveJC(JC:TObject);
var i:Integer;
begin
 for i := 0 to Self.stack.Count-1 do
  if ((Self.stack[i].ClassType = TORStackCmdJC) and ((Self.stack[i] as TORStackCmdJC).JC = JC)) then
   begin
    writelog('Z�sobn�k O� '+(Self.OblR as TOR).id+' - JC '+((Self.stack[i] as TORStackCmdJC).JC as TJC).nazev+' : smaz�na ze z�sobn�ku, id = '+IntToStr(Self.stack[i].id), WR_STACK);
    Self.RemoveFromStack(i);
    Exit();
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORStack.IsJCInStack(JC:TObject):boolean;
var i:Integer;
begin
 for i := 0 to Self.stack.Count-1 do
  if ((Self.stack[i].ClassType = TORStackCmdJC) and ((Self.stack[i] as TORStackCmdJC).JC = JC)) then
   Exit(true);
 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetUPOEnabled(enabled:boolean);
begin
 if (Self.fUPOenabled = enabled) then Exit();

 if (enabled) then
  (Self.OblR as TOR).BroadcastData('ZAS;UPO;1')
 else
  (Self.OblR as TOR).BroadcastData('ZAS;UPO;0');

 Self.fUPOenabled := enabled;
end;//procedure

///////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveFromStack(index:Integer; SenderPnl:TIDContext = nil);
begin
 if (index < Self.stack.Count) then
  begin
   (Self.OblR as TOR).BroadcastData('ZAS;RM;'+IntToStr(Self.stack[index].id));
   Self.stack.Delete(index);
   Self.hint := '';
  end;

 if (index = 0) then
   Self.UPOenabled := false;
end;//procedure

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetStackString(cmd:TORStackCmd):string;
begin
 try
   if (cmd.ClassType = TORStackCmdJC) then
    begin
     if ((cmd as TORStackCmdJC).nouz) then
        Result := 'NC  '+ ((cmd as TORStackCmdJC).JC as TJC).nazev
     else
       case (((cmd as TORStackCmdJC).JC as TJC).data.TypCesty) of
        TJCType.vlak  : Result := 'VC  '+ ((cmd as TORStackCmdJC).JC as TJC).nazev;
        TJCType.posun : Result := 'PC  '+ ((cmd as TORStackCmdJC).JC as TJC).nazev;
       end;//case
    end

   else if (cmd.ClassType = TORStackCmdZTS) then
    begin
     Result := 'ZTS ' + ((cmd as TORStackCmdZTS).uvazka as TBlk).GetGlobalSettings.name;
    end

   else if (cmd.ClassType = TORStackCmdUTS) then
    begin
     Result := 'UTS ' + ((cmd as TORStackCmdUTS).uvazka as TBlk).GetGlobalSettings.name;
    end;

 except
  Result := 'neexistuj�c� p��kaz';
 end;
end;//function

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetCount():Integer;
begin
 Result := Self.stack.Count;
end;//function

///////////////////////////////////////////////////////////////////////////////

end.//unit
