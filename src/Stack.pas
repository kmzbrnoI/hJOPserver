unit Stack;

// Tato unita implementuje tridu TORStack, ktera resi zasobnik jizdnich cest pro
//   jednu oblast rizeni
// Kazda oblast rizeni ma svuj zasobnik

interface

uses Generics.Collections, Classes, IdContext, SysUtils;

type
  TORStackMode = (PV = 0, VZ = 1);

  // 1 povel v zasobniku
  // Z teto abstratni tridy dedi konkretni povely.
  TORStackCmd = class abstract
   id: Integer;
  end;

  // povel ke staveni jizdni cesty
  TORStackCmdJC = class(TORStackCmd)
   JC: TObject;
   nouz: Boolean;
   Pnl: TIDContext;
   ab: Boolean;
  end;

  // povel k zapnuti zadosti o tratovy souhlas
  TORStackCmdZTS = class(TORStackCmd)
   uvazka: TObject;    // TBlkUvazka
  end;

  // povel k udeleni tratoveho souhlasu
  TORStackCmdUTS = class(TORStackCmd)
   uvazka: TObject;    // TBlkUvazka
  end;

  TORStack = class
   private const
    _MAX_STACK_JC = 12;

   private
    m_oblr: TObject;
    m_index: Integer;
    m_mode: TORStackMode;
    m_hint: string;
    m_stack: TList<TORStackCmd>;
    m_UPOenabled: Boolean;
    m_EZs: TList<TIdContext>; // klienti, kteri maji otevrenou editaci zasobniku

      // obsluzne funkce jednotlivych pozadavku z panelu
      procedure ORCmdPV(SenderPnl: TIdContext);
      procedure ORCmdVZ(SenderPnl: TIdContext);
      procedure ORCmdEZ(SenderPnl: TIdContext; show: Boolean);
      procedure ORCmdRM(SenderPnl: TIdContext; id: Integer);
      procedure ORCmdSWITCH(SenderPnl: TIdContext; fromId: Integer; toId: Integer; listend: Boolean = false);
      procedure ORCmdUPO(SenderPnl: TIdContext);

      procedure SetMode(mode: TORStackMode);
      procedure SetHint(hint: string);
      procedure SetUPOEnabled(enabled: Boolean);

      // odeslani seznamu jizdnich cest v zasobniku do prislusne oblasti rizeni
      procedure SendList(connection: TIdContext);

      procedure RemoveFromStack(index: Integer; SenderPnl: TIDContext = nil);

      function GetStackString(cmd: TORStackCmd): string;
      function GetCount(): Integer;

      procedure AddCmd(cmd: TORStackCmd);

      procedure ProcessJC(cmd: TORStackCmdJC);
      procedure ProcessZTS(cmd: TORStackCmdZTS);
      procedure ProcessUTS(cmd: TORStackCmdUTS);

      procedure SetFirstEnabled(enabled: Boolean);

      function FindCmdIndexById(id: Integer): Integer;

   public


      constructor Create(index: Integer; OblR: TObject);
      destructor Destroy(); override;

      procedure ParseCommand(SenderPnl: TIdContext; data: TStrings);
      procedure AddJC(JC: TObject; SenderPnl: TIDContext; nouz: Boolean; ab: Boolean);
      procedure AddZTS(uvazka: TObject; SenderPnl: TIDContext);
      procedure AddUTS(uvazka: TObject; SenderPnl: TIDContext);

      procedure Update();
      procedure NewConnection(SenderPnl: TIdContext);
      procedure OnDisconnect(SenderPnl: TIdContext);
      procedure OnWriteToRead(SenderPnl: TIdContext);

      procedure RemoveJC(JC: TObject);           // maze prvni nalezenou cestu - tuto metodu vyuziva jizdni cesta pri dokonceni staveni
      procedure RemoveZTS(linker: TObject);      // maze ZTS pokud je na prvni pozici v zasobniku
      procedure RemoveUTS(linker: TObject);      // maze UTS pokud je na prvni pozici v zasobniku

      procedure Clear();
      function GetList(): string;

      function IsJCInStack(JC: TObject): Boolean;

      property mode: TORStackMode read m_mode write SetMode;
      property hint: string read m_hint write SetHint;
      property UPOenabled: Boolean read m_UPOenabled write SetUPOEnabled;
      property count: Integer read GetCount;
      property firstEnabled: Boolean write SetFirstEnabled;
      property index: Integer read m_index write m_index;

  end;//TORStack

implementation

uses TOblRizeni, TCPServerOR, Logging, TechnologieJC, Block, BlockDb,
      BlockLinker, BlockRailway, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TORStack.Create(index: Integer; OblR: TObject);
begin
 inherited Create();

 Self.m_oblr := OblR;
 Self.m_index := index;
 Self.m_mode := TORStackMode.PV;
 Self.UPOenabled := false;

 Self.m_EZs := TList<TIdContext>.Create();
 Self.m_stack := TList<TORStackCmd>.Create();
end;

destructor TORStack.Destroy();
begin
 Self.Clear();
 Self.m_stack.Free();
 Self.m_EZs.Free();

 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ParseCommand(SenderPnl: TIdContext; data: TStrings);
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
    Self.ORCmdUPO(SenderPnl)
   else if (data[2] = 'SWITCH') then begin
     if (UpperCase(data[4]) = 'END') then
       Self.ORCmdSWITCH(SenderPnl, StrToInt(data[3]), 0, true)
     else
       Self.ORCmdSWITCH(SenderPnl, StrToInt(data[3]), StrToInt(data[4]), false);
   end;
 except
  on e: Exception do
    writelog('Server: stack data parse error : '+e.Message, WR_ERROR);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ORCmdPV(SenderPnl: TIdContext);
begin
 Self.mode := TORStackMode.PV;
end;

procedure TORStack.ORCmdVZ(SenderPnl: TIdContext);
begin
 Self.mode := TORStackMode.VZ;
end;

procedure TORStack.ORCmdEZ(SenderPnl: TIdContext; show: Boolean);
begin
 if (show) then
  begin
   if (not Self.m_EZs.Contains(SenderPnl)) then
     Self.m_EZs.Add(SenderPnl);

   Self.SendList(SenderPnl);
  end else begin
   if (Self.m_EZs.Contains(SenderPnl)) then
     Self.m_EZs.Remove(SenderPnl);
  end;
end;

procedure TORStack.ORCmdRM(SenderPnl: TIdContext; id: Integer);
var i: Integer;
begin
 i := Self.FindCmdIndexById(id);
 if (i = -1) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Povel s tímto ID v zásobníku neexistuje!');
   Exit();
  end;

 try
  if ((i = 0) and (Self.m_stack[i].ClassType = TORStackCmdJC) and (((Self.m_stack[i] as TORSTackCmdJC).JC as TJC).activating)) then
   begin
    ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze smazat JC, která se staví');
    Exit();
   end;
 except

 end;

 Self.RemoveFromStack(i, SenderPnl);
end;

procedure TORStack.ORCmdSWITCH(SenderPnl: TIdContext; fromId: Integer; toId: Integer; listend: Boolean = false);
var i, j: Integer;
    tmp: TORStackCmd;
begin
 if ((fromId = toId) and (not listend)) then
  begin
   Self.SendList(SenderPnl);
   Exit();
  end;

 i := Self.FindCmdIndexById(fromId);
 if (i = -1) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Povel s výchozím ID v zásobníku neexistuje!');
   Exit();
  end;

 tmp := Self.m_stack[i];
 Self.m_stack.Delete(i);

 try
   if (listend) then
    begin
     Self.m_stack.Add(tmp);
    end else begin
     j := Self.FindCmdIndexById(toId);
     if (j = -1) then
      begin
       ORTCPServer.SendInfoMsg(SenderPnl, 'Povel s cílovým ID v zásobníku neexistuje!');
       Self.m_stack.Insert(i, tmp);
       Exit();
      end;

     Self.m_stack.Insert(j, tmp);
    end;
 except
   Self.m_stack.Insert(i, tmp);
 end;

 (Self.m_oblr as TOR).BroadcastData('ZAS; LIST;'+Self.GetList());
 (Self.m_oblr as TOR).changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

// klik na UPO -> zobrazit upozorneni proc JC nelze postavit
procedure TORStack.ORCmdUPO(SenderPnl: TIdContext);
var cmd: TORStackCmdJC;
begin
 if ((Self.m_stack.Count = 0)) then Exit();

 // ted jsou v ceste jen bariery na potvrzeni -> cestu muzu klasicky zacit stavet pres StavJC:
 (Self.m_oblr as TOR).BroadcastData('ZAS;FIRST;0');
 Self.UPOenabled := false;

 if (Self.m_stack[0].ClassType = TORStackCmdJC) then begin
   cmd := (Self.m_stack[0] as TORSTackCmdJC);
   (cmd.JC as TJC).Activate(SenderPnl, Self.m_oblr, Self, cmd.nouz, false, cmd.ab);
  end else if (Self.m_stack[0].ClassType = TORStackCmdZTS) then
   ((Self.m_stack[0] as TORStackCmdZTS).uvazka as TBlkLinker).DoZTS(SenderPnl, Self.m_oblr)
  else if (Self.m_stack[0].ClassType = TORStackCmdUTS) then
   ((Self.m_stack[0] as TORStackCmdZTS).uvazka as TBlkLinker).DoUTS(SenderPnl, Self.m_oblr)
end;

////////////////////////////////////////////////////////////////////////////////
// Pridani obecneho prikazu do zasobniku:

procedure TORStack.AddCmd(cmd: TORStackCmd);
var description: string;
    i, max: Integer;
begin
 if (Self.m_stack.Count >= _MAX_STACK_JC) then
  begin
   writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - zásobník je plný, nelze přidat další příkaz', WR_STACK);
   raise Exception.Create('Zásobník je plný');
  end;

 max := 0;
 for i := 0 to Self.m_stack.Count-1 do
   if (Self.m_stack[i].id > max) then
     max := Self.m_stack[i].id;

 cmd.id := max + 1;

 description := Self.GetStackString(cmd);
 Self.m_stack.Add(cmd);
 (Self.m_oblr as TOR).BroadcastData('ZAS;ADD;'+IntToStr(cmd.id)+'|'+description);
 writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - : přidán příkaz ' + description + ', id = '+IntToStr(cmd.id), WR_STACK);
 (Self.m_oblr as TOR).changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

// Pridani jizdni cesty do zasobniku:
procedure TORStack.AddJC(JC: TObject; SenderPnl: TIDContext; nouz: Boolean; ab: Boolean);
var cmd: TORStackCmdJC;
begin
 cmd := TORStackCmdJC.Create();
 cmd.JC := JC;
 cmd.Pnl := SenderPnl;
 cmd.nouz := nouz;
 cmd.ab := ab;

 try
  Self.AddCmd(cmd);
 except
  on E: Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;

// Pridani zdosti o tratovy souhlas do zasobniku
procedure TORStack.AddZTS(uvazka: TObject; SenderPnl: TIDContext);
var cmd: TORStackCmdZTS;
begin
 cmd := TORStackCmdZTS.Create();
 cmd.uvazka := uvazka;

 try
  Self.AddCmd(cmd);
 except
  on E: Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;

// Pridani udeleni tratoveho souhlasu do zasobniku:
procedure TORStack.AddUTS(uvazka: TObject; SenderPnl: TIDContext);
var cmd: TORStackCmdUTS;
begin
 cmd := TORStackCmdUTS.Create();
 cmd.uvazka := uvazka;

 try
  Self.AddCmd(cmd);
 except
  on E: Exception do
    ORTCPServer.SendInfoMsg(SenderPnl, E.Message);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetMode(mode: TORStackMode);
begin
 if (Self.mode = mode) then
   Exit();

 Self.m_mode := mode;

 case (mode) of
  TORStackMode.PV : begin
    (Self.m_oblr as TOR).BroadcastData('ZAS;PV');
    Self.UPOenabled := false;
    writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - PV', WR_STACK);
  end;
  TORStackMode.VZ : begin
    (Self.m_oblr as TOR).BroadcastData('ZAS;VZ');
    writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - VZ', WR_STACK);
  end;
 end;//case

 (Self.m_oblr as TOR).changed := true;
end;

procedure TORStack.SetHint(hint: string);
begin
 if (hint <> Self.m_hint) then
  begin
   Self.m_hint := hint;
   (Self.m_oblr as TOR).BroadcastData('ZAS;HINT;'+hint);
   (Self.m_oblr as TOR).changed := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// tady se resi zpracovani prikazu v zasobniku
procedure TORStack.Update();
begin
 if (Self.m_stack.Count = 0) then Exit();
 if (Self.m_EZs.Count > 0) then Exit();

 try
   if (Self.m_stack[0].ClassType = TORStackCmdJC) then
     Self.ProcessJC(Self.m_stack[0] as TORStackCmdJC)
   else if (Self.m_stack[0].ClassType = TORStackCmdZTS) then
     Self.ProcessZTS(Self.m_stack[0] as TORStackCmdZTS)
   else if (Self.m_stack[0].ClassType = TORStackCmdUTS) then
     Self.ProcessUTS(Self.m_stack[0] as TORStackCmdUTS);
 except
  on e: Exception do
   begin
    AppEvents.LogException(E, 'Zásobník OŘ '+(Self.m_oblr as TOR).id+' - update exception, mažu příkaz ze zásobníku');
    Self.RemoveFromStack(0);
   end;
 end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.ProcessJC(cmd: TORStackCmdJC);
var JC: TJC;
    barriers: TJCBarriers;
    i: Integer;
begin
 JC := (cmd.JC as TJC);

 if ((JC.activating) or (Self.mode = TORStackMode.PV)) then
  begin
   Self.hint := '';
   Exit();
  end;

  barriers := JC.Barriers(cmd.nouz);

  if ((barriers.Count > 0) or (cmd.nouz)) then
   begin
    // v jizdni ceste jsou bariery
    if ((barriers.Count > 0) and (TJC.CriticalBarrier(barriers[0].typ))) then
     begin
      // kriticka bariera -> hint := CRIT, kritickou barieru na pozadani zobrazim dispecerovi (pres klik na UPO zasobniku)
      Self.hint       := 'CRIT';
      Self.UPOenabled := true;
      Exit();
     end;

    // tady mame zajisteno, ze v jizdni ceste nejsou kriticke bariery
    //  (KontrolaPodminek() zarucuje, ze kriticke bariery jsou na zacatku seznamu)

    // nyni oznamime nekriticke bariery ktere nemaji upozorneni
    for i := 0 to barriers.Count-1 do
     begin
      if (not JC.WarningBarrier(barriers[i].typ)) then
       begin
        // tyto bariery nelze rozkliknout pomoci UPO
        Self.hint := barriers[i].block.name;
        Self.UPOenabled := false;
        Exit();
       end;
     end;//for i

    // neupozornovaci bariery nejsou -> podivam se na zbytek barier (ty by mely byt upozornovaci a melo byt se u nich dat kliknout na UPO)
    Self.UPOenabled := true;
    if ((barriers.Count > 0) and (barriers[0].block <> nil)) then
      Self.hint := barriers[0].block.name  // tohleto si muzeme dovolit, protoze mame zajiteno, ze v JC je alespon jedna bariera (viz podminka vyse)
    else
      Self.hint := '';

    Exit();
   end;//if bariery.Count > 0

 // zadne bariery -> stavim jizdni cestu

 writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - JC '+JC.name+' : podmínky splněny, stavím', WR_STACK);

 // pokud nejsou zadne bariery, stavime jizdni cestu
 (Self.m_oblr as TOR).BroadcastData('ZAS;FIRST;0');
 JC.Activate(cmd.Pnl, Self.m_oblr, Self, false, false, cmd.ab);
 Self.UPOenabled := false;
 barriers.Free();
end;

procedure TORStack.ProcessZTS(cmd: TORStackCmdZTS);
var uv: TBlkLinker;
begin
 uv := (cmd.uvazka as TBlkLinker);

 Self.hint := (uv as TBlk).name;

 if ((Self.mode = TORStackMode.VZ) and (uv.CanZTS())) then
  begin
   Self.UPOenabled := (uv.note <> '');
   if (uv.note = '') then
    begin
     uv.request := true;
     Self.RemoveFromStack(0);
    end;
  end else begin
   Self.UPOenabled := false;
  end;
end;

procedure TORStack.ProcessUTS(cmd: TORStackCmdUTS);
var uv: TBlkLinker;
begin
 uv := (cmd.uvazka as TBlkLinker);

 Self.hint := (uv as TBlk).name;

 if ((Self.mode = TORStackMode.VZ) and (not uv.request) and ((uv.parent as TBlkRailway).request)) then
  begin
   Self.UPOenabled := (uv.note <> '');
   if (uv.note = '') then
    begin
     uv.ApproveRequest();
     Self.RemoveFromStack(0);
    end;
  end else begin
   Self.UPOenabled := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

// odeslani cest v zasobniku
// format dat: {id|name}{id|name} ...
procedure TORStack.SendList(connection: TIdContext);
begin
 ORTCPServer.SendLn(connection, (Self.m_oblr as TOR).id+';ZAS;LIST;'+Self.GetList());
end;

function TORStack.GetList(): string;
var i: Integer;
begin
 if ((Self.m_stack.Count > 0) and (Self.m_stack[0].ClassType = TORStackCmdJC) and
     (not ((Self.m_stack[0] as TORStackCmdJC).JC as TJC).activating)) then
  Result := '1;'
 else
  Result := '0;';

 for i := 0 to Self.m_stack.Count-1 do
   Result := Result + '[' + IntToStr(Self.m_stack[i].id) + '|' + Self.GetStackString(Self.m_stack[i]) + ']';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.Clear();
var cmd: TORStackCmd;
begin
 for cmd in Self.m_stack do
   cmd.Free();
 Self.m_stack.Clear();

 Self.m_EZs.Clear();
 (Self.m_oblr as TOR).BroadcastData('ZAS;LIST;1;;');
 Self.hint := '';
 Self.UPOenabled := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.NewConnection(SenderPnl:TIdContext);
begin
 ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;INDEX;'+IntToStr(Self.m_index));
 case (Self.mode) of
  TORStackMode.PV : ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;PV');
  TORStackMode.VZ : ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;VZ');
 end;
 ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;HINT;'+Self.hint);

 if (Self.UPOenabled) then
  ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;UPO;1')
 else
  ORTCPServer.SendLn(SenderPnl, (Self.m_oblr as TOR).id+';ZAS;UPO;0');

 Self.SendList(SenderPnl);
end;

procedure TORStack.OnDisconnect(SenderPnl: TIdContext);
begin
 if (Self.m_EZs.Contains(SenderPnl)) then
   Self.m_EZs.Remove(SenderPnl);
end;

procedure TORStack.OnWriteToRead(SenderPnl: TIdContext);
begin
 if (Self.m_EZs.Contains(SenderPnl)) then
   Self.m_EZs.Remove(SenderPnl);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveJC(JC: TObject);
var i: Integer;
begin
 for i := 0 to Self.m_stack.Count-1 do
  if ((Self.m_stack[i].ClassType = TORStackCmdJC) and ((Self.m_stack[i] as TORStackCmdJC).JC = JC)) then
   begin
    writelog('Zásobník OŘ '+(Self.m_oblr as TOR).id+' - JC '+((Self.m_stack[i] as TORStackCmdJC).JC as TJC).name+
        ' : smazána ze zásobníku, id = '+IntToStr(Self.m_stack[i].id), WR_STACK);
    Self.RemoveFromStack(i);
    Exit();
   end;
end;

procedure TORStack.RemoveZTS(linker: TObject);
begin
 if ((Self.m_stack.Count > 0) and (Self.m_stack[0].ClassType = TORStackCmdZTS) and
     ((Self.m_stack[0] as TORStackCmdZTS).uvazka = linker)) then
   Self.RemoveFromStack(0);
end;

procedure TORStack.RemoveUTS(linker: TObject);
begin
 if ((Self.m_stack.Count > 0) and (Self.m_stack[0].ClassType = TORStackCmdUTS) and
     ((Self.m_stack[0] as TORStackCmdUTS).uvazka = linker)) then
   Self.RemoveFromStack(0);
end;

////////////////////////////////////////////////////////////////////////////////

function TORStack.IsJCInStack(JC: TObject): Boolean;
var i: Integer;
begin
 for i := 0 to Self.m_stack.Count-1 do
  if ((Self.m_stack[i].ClassType = TORStackCmdJC) and ((Self.m_stack[i] as TORStackCmdJC).JC = JC)) then
   Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetUPOEnabled(enabled: Boolean);
begin
 if (Self.m_UPOenabled = enabled) then Exit();

 if (enabled) then
  (Self.m_oblr as TOR).BroadcastData('ZAS;UPO;1')
 else
  (Self.m_oblr as TOR).BroadcastData('ZAS;UPO;0');

 Self.m_UPOenabled := enabled;
end;

///////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveFromStack(index: Integer; SenderPnl: TIDContext = nil);
begin
 if (index < Self.m_stack.Count) then
  begin
   (Self.m_oblr as TOR).BroadcastData('ZAS;RM;'+IntToStr(Self.m_stack[index].id));
   Self.m_stack.Delete(index);
   Self.hint := '';
  end;

 if (index = 0) then
   Self.UPOenabled := false;

 if (Self.m_stack.Count = 0) then
   Self.m_EZs.Clear();

 (Self.m_oblr as TOR).changed := true;
end;

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetStackString(cmd: TORStackCmd): string;
begin
 try
   if (cmd.ClassType = TORStackCmdJC) then
    begin
     if ((cmd as TORStackCmdJC).nouz) then
        Result := 'NC  '+ ((cmd as TORStackCmdJC).JC as TJC).name
     else
       case (((cmd as TORStackCmdJC).JC as TJC).typ) of
        TJCType.train  : Result := 'VC  '+ ((cmd as TORStackCmdJC).JC as TJC).name;
        TJCType.shunt : Result := 'PC  '+ ((cmd as TORStackCmdJC).JC as TJC).name;
       end;//case
    end

   else if (cmd.ClassType = TORStackCmdZTS) then
    begin
     Result := 'ZTS ' + ((cmd as TORStackCmdZTS).uvazka as TBlk).name;
    end

   else if (cmd.ClassType = TORStackCmdUTS) then
    begin
     Result := 'UTS ' + ((cmd as TORStackCmdUTS).uvazka as TBlk).name;
    end;

 except
  Result := 'neexistující příkaz';
 end;
end;

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetCount(): Integer;
begin
 Result := Self.m_stack.Count;
end;

///////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetFirstEnabled(enabled: Boolean);
begin
 if (enabled) then
  (Self.m_oblr as TOR).BroadcastData('ZAS;FIRST;1')
 else
  (Self.m_oblr as TOR).BroadcastData('ZAS;FIRST;0');
end;

///////////////////////////////////////////////////////////////////////////////

function TORStack.FindCmdIndexById(id: Integer): Integer;
var i: Integer;
begin
 for i := 0 to Self.m_stack.Count-1 do
  if (Self.m_stack[i].id = id) then
   Exit(i);

 Result := -1;
end;

///////////////////////////////////////////////////////////////////////////////

end.//unit
