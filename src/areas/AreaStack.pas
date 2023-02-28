unit AreaStack;

{ Area's stack of commands. }

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
    linker: TObject; // TBlkLinker
  end;

  // povel k udeleni tratoveho souhlasu
  TORStackCmdUTS = class(TORStackCmd)
    linker: TObject; // TBlkLinker
  end;

  TORStack = class
  private const
    _MAX_STACK_JC = 12;

  private
    m_area: TObject;
    m_index: Integer;
    m_mode: TORStackMode;
    m_hint: string;
    m_stack: TList<TORStackCmd>;
    m_UPOenabled: Boolean;
    m_EZs: TList<TIDContext>; // klienti, kteri maji otevrenou editaci zasobniku

    // obsluzne funkce jednotlivych pozadavku z panelu
    procedure ORCmdPV(SenderPnl: TIDContext);
    procedure ORCmdVZ(SenderPnl: TIDContext);
    procedure ORCmdEZ(SenderPnl: TIDContext; show: Boolean);
    procedure ORCmdRM(SenderPnl: TIDContext; id: Integer);
    procedure ORCmdSWITCH(SenderPnl: TIDContext; fromId: Integer; toId: Integer; listend: Boolean = false);
    procedure ORCmdUPO(SenderPnl: TIDContext);

    procedure SetMode(mode: TORStackMode);
    procedure SetHint(hint: string);
    procedure SetUPOEnabled(enabled: Boolean);

    // odeslani seznamu jizdnich cest v zasobniku do prislusne oblasti rizeni
    procedure SendList(connection: TIDContext);

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

    constructor Create(index: Integer; area: TObject);
    destructor Destroy(); override;

    procedure ParseCommand(SenderPnl: TIDContext; data: TStrings);
    procedure AddJC(JC: TObject; SenderPnl: TIDContext; nouz: Boolean; ab: Boolean);
    procedure AddZTS(linker: TObject; SenderPnl: TIDContext);
    procedure AddUTS(linker: TObject; SenderPnl: TIDContext);

    procedure Update();
    procedure NewConnection(SenderPnl: TIDContext);
    procedure OnDisconnect(SenderPnl: TIDContext);
    procedure OnWriteToRead(SenderPnl: TIDContext);

    procedure RemoveJC(JC: TObject);
    // maze prvni nalezenou cestu - tuto metodu vyuziva jizdni cesta pri dokonceni staveni
    procedure RemoveZTS(linker: TObject); // maze ZTS pokud je na prvni pozici v zasobniku
    procedure RemoveUTS(linker: TObject); // maze UTS pokud je na prvni pozici v zasobniku

    procedure Clear();
    function GetList(): string;

    function IsJCInStack(JC: TObject): Boolean;

    property mode: TORStackMode read m_mode write SetMode;
    property hint: string read m_hint write SetHint;
    property UPOenabled: Boolean read m_UPOenabled write SetUPOEnabled;
    property count: Integer read GetCount;
    property firstEnabled: Boolean write SetFirstEnabled;
    property index: Integer read m_index write m_index;

  end; // TORStack

implementation

uses area, TCPServerPanel, Logging, TechnologieJC, Block, BlockDb,
  BlockLinker, BlockRailway, appEv, JCBarriers;

/// /////////////////////////////////////////////////////////////////////////////

constructor TORStack.Create(index: Integer; area: TObject);
begin
  inherited Create();

  Self.m_area := area;
  Self.m_index := index;
  Self.m_mode := TORStackMode.PV;
  Self.UPOenabled := false;

  Self.m_EZs := TList<TIDContext>.Create();
  Self.m_stack := TList<TORStackCmd>.Create();
end;

destructor TORStack.Destroy();
begin
  Self.Clear();
  Self.m_stack.Free();
  Self.m_EZs.Free();

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.ParseCommand(SenderPnl: TIDContext; data: TStrings);
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
    end else if (data[2] = 'RM') then
      Self.ORCmdRM(SenderPnl, StrToInt(data[3]))
    else if (data[2] = 'UPO') then
      Self.ORCmdUPO(SenderPnl)
    else if (data[2] = 'SWITCH') then
    begin
      if (UpperCase(data[4]) = 'END') then
        Self.ORCmdSWITCH(SenderPnl, StrToInt(data[3]), 0, true)
      else
        Self.ORCmdSWITCH(SenderPnl, StrToInt(data[3]), StrToInt(data[4]), false);
    end;
  except
    on e: Exception do
      Log('Server: stack data parse error : ' + e.Message, llError, lsStack);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.ORCmdPV(SenderPnl: TIDContext);
begin
  Self.mode := TORStackMode.PV;
end;

procedure TORStack.ORCmdVZ(SenderPnl: TIDContext);
begin
  Self.mode := TORStackMode.VZ;
end;

procedure TORStack.ORCmdEZ(SenderPnl: TIDContext; show: Boolean);
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

procedure TORStack.ORCmdRM(SenderPnl: TIDContext; id: Integer);
var i: Integer;
begin
  i := Self.FindCmdIndexById(id);
  if (i = -1) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Povel s tímto ID v zásobníku neexistuje!');
    Exit();
  end;

  try
    if ((i = 0) and (Self.m_stack[i].ClassType = TORStackCmdJC) and
      (((Self.m_stack[i] as TORStackCmdJC).JC as TJC).activating)) then
    begin
      PanelServer.SendInfoMsg(SenderPnl, 'Nelze smazat JC, která se staví');
      Exit();
    end;
  except

  end;

  Self.RemoveFromStack(i, SenderPnl);
end;

procedure TORStack.ORCmdSWITCH(SenderPnl: TIDContext; fromId: Integer; toId: Integer; listend: Boolean = false);
var i, j: Integer;
begin
  if ((fromId = toId) and (not listend)) then
  begin
    Self.SendList(SenderPnl);
    Exit();
  end;

  i := Self.FindCmdIndexById(fromId);
  if (i = -1) then
  begin
    PanelServer.SendInfoMsg(SenderPnl, 'Povel s výchozím ID v zásobníku neexistuje!');
    Exit();
  end;

  var tmp: TORStackCmd := Self.m_stack[i];
  Self.m_stack.Delete(i);

  try
    if (listend) then
    begin
      Self.m_stack.Add(tmp);
    end else begin
      j := Self.FindCmdIndexById(toId);
      if (j = -1) then
      begin
        PanelServer.SendInfoMsg(SenderPnl, 'Povel s cílovým ID v zásobníku neexistuje!');
        Self.m_stack.Insert(i, tmp);
        Exit();
      end;

      Self.m_stack.Insert(j, tmp);
    end;
  except
    Self.m_stack.Insert(i, tmp);
  end;

  (Self.m_area as TArea).BroadcastData('ZAS; LIST;' + Self.GetList());
  (Self.m_area as TArea).changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

// klik na UPO -> zobrazit upozorneni proc JC nelze postavit
procedure TORStack.ORCmdUPO(SenderPnl: TIDContext);
begin
  if ((Self.m_stack.count = 0)) then
    Exit();

  // ted jsou v ceste jen bariery na potvrzeni -> cestu muzu klasicky zacit stavet pres StavJC:
  (Self.m_area as TArea).BroadcastData('ZAS;FIRST;0');
  Self.UPOenabled := false;

  if (Self.m_stack[0].ClassType = TORStackCmdJC) then
  begin
    var cmd: TORStackCmdJC := (Self.m_stack[0] as TORStackCmdJC);
    (cmd.JC as TJC).Activate(SenderPnl, Self.m_area, Self, cmd.nouz, false, cmd.ab);
  end else if (Self.m_stack[0].ClassType = TORStackCmdZTS) then
    ((Self.m_stack[0] as TORStackCmdZTS).linker as TBlkLinker).DoZTS(SenderPnl, Self.m_area)
  else if (Self.m_stack[0].ClassType = TORStackCmdUTS) then
    ((Self.m_stack[0] as TORStackCmdZTS).linker as TBlkLinker).DoUTS(SenderPnl, Self.m_area)
end;

/// /////////////////////////////////////////////////////////////////////////////
// Pridani obecneho prikazu do zasobniku:

procedure TORStack.AddCmd(cmd: TORStackCmd);
begin
  if (Self.m_stack.count >= _MAX_STACK_JC) then
  begin
    Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - zásobník je plný, nelze přidat další příkaz', llWarning, lsStack);
    raise Exception.Create('Zásobník je plný');
  end;

  var max: Integer := 0;
  for var i: Integer := 0 to Self.m_stack.count - 1 do
    if (Self.m_stack[i].id > max) then
      max := Self.m_stack[i].id;

  cmd.id := max + 1;

  var description: string := Self.GetStackString(cmd);
  Self.m_stack.Add(cmd);
  (Self.m_area as TArea).BroadcastData('ZAS;ADD;' + IntToStr(cmd.id) + '|' + description);
  Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - : přidán příkaz ' + description + ', id = ' +
    IntToStr(cmd.id), llInfo, lsStack);
  (Self.m_area as TArea).changed := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

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
    on e: Exception do
      PanelServer.SendInfoMsg(SenderPnl, e.Message);
  end;
end;

// Pridani zdosti o tratovy souhlas do zasobniku
procedure TORStack.AddZTS(linker: TObject; SenderPnl: TIDContext);
var cmd: TORStackCmdZTS;
begin
  cmd := TORStackCmdZTS.Create();
  cmd.linker := linker;

  try
    Self.AddCmd(cmd);
  except
    on e: Exception do
      PanelServer.SendInfoMsg(SenderPnl, e.Message);
  end;
end;

// Pridani udeleni tratoveho souhlasu do zasobniku:
procedure TORStack.AddUTS(linker: TObject; SenderPnl: TIDContext);
var cmd: TORStackCmdUTS;
begin
  cmd := TORStackCmdUTS.Create();
  cmd.linker := linker;

  try
    Self.AddCmd(cmd);
  except
    on e: Exception do
      PanelServer.SendInfoMsg(SenderPnl, e.Message);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetMode(mode: TORStackMode);
begin
  if (Self.mode = mode) then
    Exit();

  Self.m_mode := mode;

  case (mode) of
    TORStackMode.PV:
      begin
        (Self.m_area as TArea).BroadcastData('ZAS;PV');
        Self.UPOenabled := false;
        Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - PV', llInfo, lsStack);
      end;
    TORStackMode.VZ:
      begin
        (Self.m_area as TArea).BroadcastData('ZAS;VZ');
        Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - VZ', llInfo, lsStack);
      end;
  end; // case

  (Self.m_area as TArea).changed := true;
end;

procedure TORStack.SetHint(hint: string);
begin
  if (hint <> Self.m_hint) then
  begin
    Self.m_hint := hint;
    (Self.m_area as TArea).BroadcastData('ZAS;HINT;' + hint);
    (Self.m_area as TArea).changed := true;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// tady se resi zpracovani prikazu v zasobniku
procedure TORStack.Update();
begin
  if (Self.m_stack.count = 0) then
    Exit();
  if (Self.m_EZs.count > 0) then
    Exit();

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
      AppEvents.LogException(e, 'Zásobník OŘ ' + (Self.m_area as TArea).id +
        ' - update exception, mažu příkaz ze zásobníku');
      Self.RemoveFromStack(0);
    end;
  end;

end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.ProcessJC(cmd: TORStackCmdJC);
var JC: TJC;
  barriers: TJCBarriers;
begin
  JC := (cmd.JC as TJC);

  if ((JC.activating) or (Self.mode = TORStackMode.PV)) then
  begin
    Self.hint := '';
    Exit();
  end;

  barriers := JC.barriers(cmd.nouz);

  if ((barriers.count > 0) or (cmd.nouz)) then
  begin
    // v jizdni ceste jsou bariery
    if ((barriers.count > 0) and (JCBarriers.CriticalBarrier(barriers[0].typ))) then
    begin
      // kriticka bariera -> hint := CRIT, kritickou barieru na pozadani zobrazim dispecerovi (pres klik na UPO zasobniku)
      Self.hint := 'CRIT';
      Self.UPOenabled := true;
      Exit();
    end;

    // tady mame zajisteno, ze v jizdni ceste nejsou kriticke bariery
    // (KontrolaPodminek() zarucuje, ze kriticke bariery jsou na zacatku seznamu)

    // nyni oznamime nekriticke bariery ktere nemaji upozorneni
    for var i: Integer := 0 to barriers.count - 1 do
    begin
      if (not JCBarriers.JCWarningBarrier(barriers[i].typ)) then
      begin
        // tyto bariery nelze rozkliknout pomoci UPO
        Self.hint := barriers[i].Block.name;
        Self.UPOenabled := false;
        Exit();
      end;
    end; // for i

    // neupozornovaci bariery nejsou -> podivam se na zbytek barier (ty by mely byt upozornovaci a melo byt se u nich dat kliknout na UPO)
    Self.UPOenabled := true;
    if ((barriers.count > 0) and (barriers[0].Block <> nil)) then
      Self.hint := barriers[0].Block.name
      // tohleto si muzeme dovolit, protoze mame zajiteno, ze v JC je alespon jedna bariera (viz podminka vyse)
    else
      Self.hint := '';

    Exit();
  end; // if bariery.Count > 0

  // zadne bariery -> stavim jizdni cestu

  Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - JC ' + JC.name + ' : podmínky splněny, stavím', llInfo, lsStack);

  // pokud nejsou zadne bariery, stavime jizdni cestu
  (Self.m_area as TArea).BroadcastData('ZAS;FIRST;0');
  JC.Activate(cmd.Pnl, Self.m_area, Self, false, false, cmd.ab);
  Self.UPOenabled := false;
  barriers.Free();
end;

procedure TORStack.ProcessZTS(cmd: TORStackCmdZTS);
var linker: TBlkLinker;
begin
  linker := (cmd.linker as TBlkLinker);

  Self.hint := (linker as TBlk).name;

  if ((Self.mode = TORStackMode.VZ) and (linker.CanZTS())) then
  begin
    Self.UPOenabled := (linker.note <> '');
    if (linker.note = '') then
    begin
      linker.request := true;
      Self.RemoveFromStack(0);
    end;
  end else begin
    Self.UPOenabled := false;
  end;
end;

procedure TORStack.ProcessUTS(cmd: TORStackCmdUTS);
var linker: TBlkLinker;
begin
  linker := (cmd.linker as TBlkLinker);

  Self.hint := (linker as TBlk).name;

  if ((Self.mode = TORStackMode.VZ) and (not linker.request) and ((linker.parent as TBlkRailway).request)) then
  begin
    Self.UPOenabled := (linker.note <> '');
    if (linker.note = '') then
    begin
      linker.ApproveRequest();
      Self.RemoveFromStack(0);
    end;
  end else begin
    Self.UPOenabled := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// odeslani cest v zasobniku
// format dat: {id|name}{id|name} ...
procedure TORStack.SendList(connection: TIDContext);
begin
  PanelServer.SendLn(connection, (Self.m_area as TArea).id + ';ZAS;LIST;' + Self.GetList());
end;

function TORStack.GetList(): string;
begin
  if ((Self.m_stack.count > 0) and (Self.m_stack[0].ClassType = TORStackCmdJC) and
    (not((Self.m_stack[0] as TORStackCmdJC).JC as TJC).activating)) then
    Result := '1;'
  else
    Result := '0;';

  for var i: Integer := 0 to Self.m_stack.count - 1 do
    Result := Result + '[' + IntToStr(Self.m_stack[i].id) + '|' + Self.GetStackString(Self.m_stack[i]) + ']';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.Clear();
begin
  for var cmd: TORStackCmd in Self.m_stack do
    cmd.Free();
  Self.m_stack.Clear();

  Self.m_EZs.Clear();
  (Self.m_area as TArea).BroadcastData('ZAS;LIST;1;;');
  Self.hint := '';
  Self.UPOenabled := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.NewConnection(SenderPnl: TIDContext);
begin
  PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;INDEX;' + IntToStr(Self.m_index));
  case (Self.mode) of
    TORStackMode.PV:
      PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;PV');
    TORStackMode.VZ:
      PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;VZ');
  end;
  PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;HINT;' + Self.hint);

  if (Self.UPOenabled) then
    PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;UPO;1')
  else
    PanelServer.SendLn(SenderPnl, (Self.m_area as TArea).id + ';ZAS;UPO;0');

  Self.SendList(SenderPnl);
end;

procedure TORStack.OnDisconnect(SenderPnl: TIDContext);
begin
  if (Self.m_EZs.Contains(SenderPnl)) then
    Self.m_EZs.Remove(SenderPnl);
end;

procedure TORStack.OnWriteToRead(SenderPnl: TIDContext);
begin
  if (Self.m_EZs.Contains(SenderPnl)) then
    Self.m_EZs.Remove(SenderPnl);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveJC(JC: TObject);
begin
  for var i: Integer := 0 to Self.m_stack.count - 1 do
    if ((Self.m_stack[i].ClassType = TORStackCmdJC) and ((Self.m_stack[i] as TORStackCmdJC).JC = JC)) then
    begin
      Log('Zásobník OŘ ' + (Self.m_area as TArea).id + ' - JC ' + ((Self.m_stack[i] as TORStackCmdJC).JC as TJC)
        .name + ' : smazána ze zásobníku, id = ' + IntToStr(Self.m_stack[i].id), llInfo, lsStack);
      Self.RemoveFromStack(i);
      Exit();
    end;
end;

procedure TORStack.RemoveZTS(linker: TObject);
begin
  if ((Self.m_stack.count > 0) and (Self.m_stack[0].ClassType = TORStackCmdZTS) and
    ((Self.m_stack[0] as TORStackCmdZTS).linker = linker)) then
    Self.RemoveFromStack(0);
end;

procedure TORStack.RemoveUTS(linker: TObject);
begin
  if ((Self.m_stack.count > 0) and (Self.m_stack[0].ClassType = TORStackCmdUTS) and
    ((Self.m_stack[0] as TORStackCmdUTS).linker = linker)) then
    Self.RemoveFromStack(0);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TORStack.IsJCInStack(JC: TObject): Boolean;
begin
  for var i: Integer := 0 to Self.m_stack.count - 1 do
    if ((Self.m_stack[i].ClassType = TORStackCmdJC) and ((Self.m_stack[i] as TORStackCmdJC).JC = JC)) then
      Exit(true);
  Result := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetUPOEnabled(enabled: Boolean);
begin
  if (Self.m_UPOenabled = enabled) then
    Exit();

  if (enabled) then
    (Self.m_area as TArea).BroadcastData('ZAS;UPO;1')
  else
    (Self.m_area as TArea).BroadcastData('ZAS;UPO;0');

  Self.m_UPOenabled := enabled;
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveFromStack(index: Integer; SenderPnl: TIDContext = nil);
begin
  if (index < Self.m_stack.count) then
  begin
    (Self.m_area as TArea).BroadcastData('ZAS;RM;' + IntToStr(Self.m_stack[index].id));
    Self.m_stack.Delete(index);
    Self.hint := '';
  end;

  if (index = 0) then
    Self.UPOenabled := false;

  if (Self.m_stack.count = 0) then
    Self.m_EZs.Clear();

  (Self.m_area as TArea).changed := true;
end;

/// ////////////////////////////////////////////////////////////////////////////

function TORStack.GetStackString(cmd: TORStackCmd): string;
begin
  try
    if (cmd.ClassType = TORStackCmdJC) then
    begin
      if ((cmd as TORStackCmdJC).nouz) then
        Result := 'NC  ' + ((cmd as TORStackCmdJC).JC as TJC).name
      else
        case (((cmd as TORStackCmdJC).JC as TJC).typ) of
          TJCType.train:
            Result := 'VC  ' + ((cmd as TORStackCmdJC).JC as TJC).name;
          TJCType.shunt:
            Result := 'PC  ' + ((cmd as TORStackCmdJC).JC as TJC).name;
        end; // case
    end

    else if (cmd.ClassType = TORStackCmdZTS) then
    begin
      Result := 'ZTS ' + ((cmd as TORStackCmdZTS).linker as TBlk).name;
    end

    else if (cmd.ClassType = TORStackCmdUTS) then
    begin
      Result := 'UTS ' + ((cmd as TORStackCmdUTS).linker as TBlk).name;
    end;

  except
    Result := 'neexistující příkaz';
  end;
end;

/// ////////////////////////////////////////////////////////////////////////////

function TORStack.GetCount(): Integer;
begin
  Result := Self.m_stack.count;
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetFirstEnabled(enabled: Boolean);
begin
  if (enabled) then
    (Self.m_area as TArea).BroadcastData('ZAS;FIRST;1')
  else
    (Self.m_area as TArea).BroadcastData('ZAS;FIRST;0');
end;

/// ////////////////////////////////////////////////////////////////////////////

function TORStack.FindCmdIndexById(id: Integer): Integer;
begin
  for var i: Integer := 0 to Self.m_stack.count - 1 do
    if (Self.m_stack[i].id = id) then
      Exit(i);

  Result := -1;
end;

/// ////////////////////////////////////////////////////////////////////////////

end.// unit
