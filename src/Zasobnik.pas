unit Zasobnik;

// Tato unita implementuje tridu TORStack, ktera resi zasobnik jizdnich cest pro
//   jednu oblast rizeni
// Kazda oblast rizeni ma svuj zasobnik

interface

uses Generics.Collections, Classes, IdContext, SysUtils, UPO, RPConst;

type
  TORStackVolba = (PV = 0, VZ = 1);

  TORStackJC = record
   JC:TObject;
   id:Integer;
   Pnl:TIDContext;
   nouz:boolean;
  end;

  TORStack = class
   private const
    _MAX_STACK_JC = 12;

   private
    OblR:TObject;
    findex:Integer;
    fvolba:TORStackVolba;
    fhint:string;
    stack:TList<TORStackJC>;
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

      function GetJCStackString(JC:TORStackJC):string;
      function GetCount():Integer;

   public


      constructor Create(index:Integer; OblR:TObject);
      destructor Destroy(); override;

      procedure ParseCommand(SenderPnl:TIdContext; data:TStrings);
      procedure AddJC(JC:TObject; SenderPnl:TIDContext; nouz:boolean);
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

uses TOblRizeni, TCPServerOR, Logging, TechnologieJC, TBlok, TBloky;

////////////////////////////////////////////////////////////////////////////////

constructor TORStack.Create(index:Integer; OblR:TObject);
begin
 inherited Create();

 Self.OblR       := OblR;
 Self.findex     := index;
 Self.fvolba     := TORStackVolba.PV;
 Self.UPOenabled := false;

 Self.stack  := TList<TORStackJC>.Create();
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
      if ((i = 0) and ((Self.stack[i].JC as TJC).staveni)) then
       begin
        ORTCPServer.SendInfoMsg(SenderPnl, 'Nelze smazat JC, která se staví');
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
begin
 if (Self.stack.Count = 0) then Exit();

 // ted jsou v ceste jen bariery na potvrzeni -> cestu muzu klasicky zacit stavet pres StavJC:
 (Self.OblR as TOR).BroadcastData('ZAS;FIRST;0');
 Self.UPOenabled := false;
 (Self.stack[0].JC as TJC).StavJC(Self.stack[0].Pnl, Self.OblR, Self, Self.stack[0].nouz);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.AddJC(JC:TObject; SenderPnl:TIDContext; nouz:boolean);
var stackJC:TORStackJC;
begin
 if (Self.stack.Count >= _MAX_STACK_JC) then
  begin
   ORTCPServer.SendInfoMsg(SenderPnl, 'Zásobník jízdních cest je plný!');
   writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - zásobník je plný, nelze pøidat další JC', WR_STACK);
   Exit();
  end;

 if (Self.stack.Count > 0) then
  stackJC.id := Self.stack[Self.stack.Count-1].id + 1
 else
  stackJC.id := 0;

 stackJC.JC   := JC;
 stackJC.Pnl  := SenderPnl;
 stackJC.nouz := nouz;
 Self.stack.Add(stackJC);
 (Self.OblR as TOR).BroadcastData('ZAS;ADD;'+IntToStr(stackJC.id)+'|'+Self.GetJCStackString(stackJC));

// ORTCPServer.SendInfoMsg(SenderPnl, (JC as TJC).nazev+' pøidána do zásobníku');
 writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - JC '+(JC as TJC).nazev+' : pøidána do zásobníku, id = '+IntToStr(stackJC.id), WR_STACK);

 (Self.OblR as TOR).changed := true;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetVolba(volba:TORStackVolba);
begin
 Self.fvolba := volba;
 case (volba) of
  PV : begin
    (Self.OblR as TOR).BroadcastData('ZAS;PV');
    writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - PV', WR_STACK);
  end;
  VZ : begin
    (Self.OblR as TOR).BroadcastData('ZAS;VZ');
    writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - VZ', WR_STACK);
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

// tady se resi staveni cest v zasobniku
procedure TORStack.Update();
var JC:TJC;
    bariery:TJCBariery;
    i:Integer;
begin
 if (Self.stack.Count = 0) then Exit();

 try
   JC := (Self.stack[0].JC as TJC);
 except
   Self.RemoveFromStack(0);
   Exit()
 end;

 try
   if (JC.staveni) then
    begin
     Self.hint := '';
     Exit();
    end;

    bariery := JC.KontrolaPodminek(Self.stack[0].nouz);

    if ((bariery.Count > 0) or (Self.stack[0].nouz)) then
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
        if (not TJC.WarningBariera(bariery[i].typ)) then
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

   writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - JC '+JC.nazev+' : podmínky splnìny, stavím', WR_STACK);

   // pokud nejsou zadne bariery, stavime jizdni cestu
   (Self.OblR as TOR).BroadcastData('ZAS;FIRST;0');
   JC.StavJC(Self.stack[0].Pnl, Self.OblR, Self);
   Self.UPOenabled := false;
   bariery.Free();
 except
  on e:Exception do
   begin
    writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - update exception '+e.Message+'; mažu cestu ze zásobníku', WR_ERROR);
    Self.RemoveFromStack(0);
   end;
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// odeslani cest v zasobniku
// format dat: (id|name)(id|name) ...
procedure TORStack.SendList(connection:TIdContext);
var str:string;
    i:Integer;
    first_enabled:string;
begin
 if ((Self.stack.Count > 0) and ((Self.stack[0].JC as TJC).staveni)) then
  first_enabled := '0'
 else
  first_enabled := '1';

 str := '';
 for i := 0 to Self.stack.Count-1 do
  str := str + '(' + IntToStr(Self.stack[i].id) + '|' + Self.GetJCStackString(Self.stack[i]) + ')';
 ORTCPServer.SendLn(connection, (Self.OblR as TOR).id+';ZAS;LIST;'+first_enabled+';'+str);
end;//procedure

function TORStack.GetList():string;
var i:Integer;
begin
 Result := '';
 for i := 0 to Self.stack.Count-1 do
  Result := Result + '(' + IntToStr(Self.stack[i].id) + '|' + (Self.stack[i].JC as TJC).nazev + ')';
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
  if (Self.stack[i].JC = JC) then
   begin
    writelog('Zásobník OØ '+(Self.OblR as TOR).id+' - JC '+(Self.stack[i].JC as TJC).nazev+' : smazána ze zásobníku, id = '+IntToStr(Self.stack[i].id), WR_STACK);
    Self.RemoveFromStack(i);
    Exit();
   end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TORStack.IsJCInStack(JC:TObject):boolean;
var i:Integer;
begin
 for i := 0 to Self.stack.Count-1 do
  if (Self.stack[i].JC = JC) then
   Exit(true);
 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TORStack.SetUPOEnabled(enabled:boolean);
begin
 if (Self.fUPOenabled = enabled) then Exit();

 if (enabled) then
  (Self.OblR as TOR).BroadcastData(';ZAS;UPO;1')
 else
  (Self.OblR as TOR).BroadcastData(';ZAS;UPO;0');

 Self.fUPOenabled := enabled;
end;//procedure

///////////////////////////////////////////////////////////////////////////////

procedure TORStack.RemoveFromStack(index:Integer; SenderPnl:TIDContext = nil);
begin
 if (index < Self.stack.Count) then
  begin
   (Self.OblR as TOR).BroadcastData('ZAS;RM;'+IntToStr(Self.stack[index].id));
//   if (SenderPnl = nil) then
//     ORTCPServer.SendInfoMsg(Self.stack[index].Pnl, 'JC '+(Self.stack[index].JC as TJC).nazev+' smazána ze zásobníku')
//   else
//     ORTCPServer.SendInfoMsg(SenderPnl, 'JC '+(Self.stack[index].JC as TJC).nazev+' smazána ze zásobníku');
   Self.stack.Delete(index);
   Self.hint := '';
  end;

 if (index = 0) then
   Self.UPOenabled := false;
end;//procedure

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetJCStackString(JC:TORStackJC):string;
begin
 if (JC.nouz) then
    Result := 'NC  '+ (JC.JC as TJC).nazev
 else
   case ((JC.JC as TJC).data.TypCesty) of
    TJCType.vlak  : Result := 'VC  '+ (JC.JC as TJC).nazev;
    TJCType.posun : Result := 'PC  '+ (JC.JC as TJC).nazev;
   end;//case

end;//function

///////////////////////////////////////////////////////////////////////////////

function TORStack.GetCount():Integer;
begin
 Result := Self.stack.Count;
end;//function

///////////////////////////////////////////////////////////////////////////////

end.//unit
