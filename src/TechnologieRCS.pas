unit TechnologieRCS;

{
 Technologie RCS: rozhrani pro pouzivani Railroad Control System.

 RCS je obecny nazev pro sbernici resici rizeni prislusenstvi, napriklad MTB,
 touto sbernici ale muze byt klidne i XpressNET.

 Vsechny ostatni casti programu by mely volat metody tridy TRCS, ktera interaguje
 s RCS. Trida TRCS v sobe skryva interakci s TCSIFace. Trida TRCS pomerne <= TODO: tohle neni pravda
 intenzivne interaguje s dalsimi technologickymi prvku hJOPserveru -- je tedy
 nedilnou soucasti serveru.

 Pricip:
  - na zacatku vytvorime tridy pro vsechny existujici moduly RCS
  - po otevreni RCS zjistime, ktere desky jsou skutecne dostupne a ktere ne
}

interface

uses SysUtils, Classes, IniFiles, Generics.Collections, RCS;

type
  TErrEvent = procedure(Sender:TObject; errValue: word; errAddr: byte; errMsg:string) of object;
  TRCSReadyEvent = procedure (Sender:TObject; ready:boolean) of object;
  TRCSBoardChangeEvent = procedure (Sender:TObject; board:byte) of object;

  //////////////////////////////////////////////////////////////

  //toto se pouziva pro identifikaci desky a portu VSUDE v technologii
  TRCSAddr = record                                                             // jedno fyzicke RCS spojeni
   board:Byte;                                                                    // cislo desky
   port:Byte;                                                                     // cislo portu
  end;

  TRCSBoard = class                                                               // jedna RCS deska
    needed:boolean;                                                               // jestli jed eska potrebna pro technologii (tj. jeslti na ni referuji nejake bloky atp.)
    inputChangedEv:TList<TRCSBoardChangeEvent>;
    outputChangedEv:TList<TRCSBoardChangeEvent>;

    constructor Create();
    destructor Destroy(); override;
  end;

  //////////////////////////////////////////////////////////////

  // Technologie RCS
  TRCS = class(TRCSIFace)
   public const
     _MAX_RCS = 192;                                        // maximalni pocet RCS desek

   private const
     _DEFAULT_LIB = 'simulator.dll';
     _INIFILE_SECTNAME = 'RCS';
     _CONFIG_PATH = 'rcs';

   private
     boards:TObjectDictionary<Cardinal, TRCSBoard>;
     aReady:boolean;                                        // jestli je nactena knihovna vporadku a tudiz jestli lze zapnout systemy
     fGeneralError:boolean;                                 // flag oznamujici nastani "RCS general IO error" -- te nejhorsi veci na svete
     fLibDir:string;

     //events to the main program
     fOnReady : TRCSReadyEvent;
     fAfterClose : TNotifyEvent;

      //events from libraly
      procedure DllAfterClose(Sender:TObject);

      procedure DllOnLog(Sender: TObject; logLevel:TRCSLogLevel; msg:string);
      procedure DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg:PChar);
      procedure DllOnInputChanged(Sender:TObject; module:Cardinal);
      procedure DllOnOutputChanged(Sender:TObject; module:Cardinal);

   public
     log:boolean;

      constructor Create();
      destructor Destroy; override;

      procedure LoadLib(filename:string);                                       // nacte knihovnu

      procedure InputSim();                                                     // pokud je nactena knihovna Simulator.dll, simuluje vstupy (koncove polohy vyhybek atp.)
      procedure SoupravaUsekSim();                                              // nastavit RCS vstupy tak, aby useky, n akterych existuje souprava, byly obsazene

      function NoExStarted():boolean;
      function NoExOpened():boolean;

      procedure SetNeeded(RCSAdr:Cardinal; state:boolean = true);
      function GetNeeded(RCSAdr:Cardinal):boolean;

      procedure LoadFromFile(ini:TMemIniFile);
      procedure SaveToFile(ini:TMemIniFile);

      procedure AddInputChangeEvent(board:Cardinal; event:TRCSBoardChangeEvent);
      procedure RemoveInputChangeEvent(event:TRCSBoardChangeEvent; board:Integer = -1);

      procedure AddOutputChangeEvent(board:Cardinal; event:TRCSBoardChangeEvent);
      procedure RemoveOutputChangeEvent(event:TRCSBoardChangeEvent; board:Integer = -1);

      function IsSimulatorMode():boolean;

      property generalError:boolean read fGeneralError;
      class function RCSAddr(board:Byte; port:Byte):TRCSAddr;

      //events
      property AfterClose:TNotifyEvent read fAfterClose write fAfterClose;

      property OnReady:TRCSReadyEvent read fOnReady write fOnReady;
      property ready:boolean read aready;
      property libDir:string read fLibDir;
  end;

var
  RCSi:TRCS;


implementation

uses fMain, fAdminForm, GetSystems, TBloky, TBlok, TBlokVyhybka, TBlokUsek,
     TBlokIR, TBlokSCom, BoosterDb, TBlokPrejezd, RCSErrors, TOblsRizeni,
     Logging, TCPServerOR, SprDb, DataRCS, appEv, Booster, StrUtils;

constructor TRCS.Create();
begin
 inherited;

 Self.boards := TObjectDictionary<Cardinal, TRCSBoard>.Create();

 if not DirectoryExists(_CONFIG_PATH) then
   CreateDir(_CONFIG_PATH);

 Self.log := false;
 Self.aReady := false;
 Self.fGeneralError := false;

 //assign events
 TRCSIFace(Self).AfterClose := Self.DllAfterClose;
 TRCSIFace(Self).OnError    := Self.DllOnError;
 TRCSIFace(Self).OnLog      := Self.DllOnLog;
 TRCSIFace(Self).OnInputChanged  := Self.DllOnInputChanged;
 TRCSIFace(Self).OnOutputChanged := Self.DllOnOutputChanged;
end;

destructor TRCS.Destroy();
begin
 Self.boards.Free();
 inherited;
end;

procedure TRCS.LoadLib(filename:string);
var str, tmp, libName:string;
begin
 libName := ExtractFileName(filename);

 if (not FileExists(filename)) then
   raise Exception.Create('Library file not found, not loading');

 if (Self.ready) then
  begin
   Self.aReady := false;
   if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
  end;

 TRCSIFace(Self).LoadLib(filename, _CONFIG_PATH + '\' + ChangeFileExt(libName, '.ini'));

 writelog('Naètena knihovna '+ libName, WR_RCS);

 // kontrola bindnuti vsech eventu

 // bind SetInput neni striktne vyzadovan
 if (Self.unbound.Contains('SetInput')) then
   Self.unbound.Remove('SetInput');

 if (Self.unbound.Count = 0) then
  begin
   Self.aReady := true;
   if (Assigned(Self.OnReady)) then Self.OnReady(Self, Self.aReady);
  end else begin
   str := '';
   for tmp in Self.unbound do
     str := str + tmp + ', ';
   str := LeftStr(str, Length(str)-2);
   F_Main.LogStatus('ERR: RCS: nepodaøilo se svázat následující funkce : ' + str);
  end;
end;

procedure TRCS.InputSim();
var i:integer;
    Blk:TBlk;
    booster:TBooster;
begin
 //nastaveni vyhybek do +
 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i, Blk);
   if ((Blk.GetGlobalSettings.typ = _BLK_VYH) and ((Blk as TBlkVyhybka).GetSettings().RCSAddrs.Count > 0)) then
     Self.SetInput((Blk as TBlkVyhybka).GetSettings().RCSAddrs[0].board, (Blk as TBlkVyhybka).GetSettings().RCSAddrs[0].port,1);
   if (Blk.typ = _BLK_PREJEZD) then
     Self.SetInput((Blk as TBlkPrejezd).GetSettings().RCS, (Blk as TBlkPrejezd).GetSettings().RCSInputs.Otevreno, 1);
   if ((F_Admin.CHB_SimSoupravaUsek.Checked) and ((Blk.typ = _BLK_USEK) or (Blk.typ = _BLK_TU)) and ((Blk as TBlkUsek).IsSouprava()) and
       ((Blk as TBlkUsek).GetSettings().RCSAddrs.Count > 0)) then
     Self.SetInput((Blk as TBlkUsek).GetSettings().RCSAddrs[0].board, (Blk as TBlkUsek).GetSettings().RCSAddrs[0].port, 1);
  end;//for cyklus

 //defaultni stav zesilovacu
 for booster in Boosters.sorted do
  begin
   Self.SetInput(booster.bSettings.RCS.Napajeni.board, booster.bSettings.RCS.Napajeni.port, 0);
   Self.SetInput(booster.bSettings.RCS.Zkrat.board, booster.bSettings.RCS.Zkrat.port, 0);
  end;
end;

//simulace obaszeni useku, na kterem je souprava
procedure TRCS.SoupravaUsekSim;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Blky.Cnt-1 do
  begin
   Blky.GetBlkByIndex(i,Blk);
   if ((Blk.typ <> _BLK_USEK) and (Blk.typ <> _BLK_TU)) then continue;
   if (((Blk as TBlkUsek).IsSouprava()) and ((Blk as TBlkUsek).GetSettings().RCSAddrs.Count > 0)) then
     Self.SetInput((Blk as TBlkUsek).GetSettings().RCSAddrs[0].board, (Blk as TBlkUsek).GetSettings().RCSAddrs[0].port,1);
  end;
end;

procedure TRCS.LoadFromFile(ini:TMemIniFile);
var lib:string;
begin
  fLibDir := ini.ReadString(_INIFILE_SECTNAME, 'dir', '.');
  lib := ini.ReadString(_INIFILE_SECTNAME, 'lib', _DEFAULT_LIB);

  try
    Self.LoadLib(fLibDir + '\' + lib);
  except
    on E:Exception do
      writeLog('Nelze naèíst knihovnu ' + fLibDir + '\' + lib + ', ' + E.Message, WR_ERROR);
  end;
end;

procedure TRCS.SaveToFile(ini:TMemIniFile);
begin
  if (Self.Lib <> '') then
    ini.WriteString(_INIFILE_SECTNAME, 'lib', ExtractFileName(Self.Lib));
end;

procedure TRCS.DllAfterClose(Sender:TObject);
begin
 Self.fGeneralError := false;
 if (Assigned(Self.fAfterClose)) then Self.fAfterClose(Self);
end;//procdure

procedure TRCS.DllOnError(Sender: TObject; errValue: word; errAddr: Cardinal; errMsg:PChar);
begin
 writelog('RCS ERR: '+errMsg+' ('+IntToStr(errValue)+':'+IntToStr(errAddr)+')', WR_RCS, 1);

 if (errAddr = 255) then
  begin
   //errors on main board (RCS-USB)
   case (errValue) of
    RCS_FT_EXCEPTION: begin
      // general IO error
      F_Main.A_System_Start.Enabled := true;
      F_Main.A_System_Stop.Enabled  := true;
      writelog('RCS FTDI Error - '+IntToStr(errValue), WR_ERROR, 0);
      ORTCPServer.BroadcastBottomError('RCS FTDI error', 'TECHNOLOGIE');
    end;
   end;//case
  end else begin
   // errors on RCS boards
   case (errValue) of
    RCS_MODULE_FAIL: ORs.RCSFail(errAddr); // communication with module failed
    RCS_MODULE_RESTORED:; // communication with module restored, nothing should be here
   end;
  end;//
end;

procedure TRCS.DllOnLog(Sender: TObject; logLevel:TRCSLogLevel; msg:string);
begin
 if (Self.log) then
   writelog(UpperCase(Self.LogLevelToString(logLevel)) + ': ' + msg, WR_RCS);
end;

procedure TRCS.DllOnInputChanged(Sender:TObject; module:Cardinal);
var i:Integer;
begin
 if (Self.boards.ContainsKey(module)) then
   for i := Self.boards[module].inputChangedEv.Count-1 downto 0 do
     if (Assigned(Self.boards[module].inputChangedEv[i])) then Self.boards[module].inputChangedEv[i](Self, module)
       else Self.boards[module].inputChangedEv.Delete(i);
 RCSTableData.UpdateBoard(module);
end;

procedure TRCS.DllOnOutputChanged(Sender:TObject; module:Cardinal);
var i:Integer;
begin
 if (Self.boards.ContainsKey(module)) then
   for i := Self.boards[module].outputChangedEv.Count-1 downto 0 do
     if (Assigned(Self.boards[module].outputChangedEv[i])) then Self.boards[module].outputChangedEv[i](Self, module)
       else Self.boards[module].outputChangedEv.Delete(i);
 RCSTableData.UpdateBoard(module);
end;

//----- events from dll end -----
////////////////////////////////////////////////////////////////////////////////

procedure TRCS.SetNeeded(RCSAdr:Cardinal; state:boolean = true);
begin
 if (not Self.boards.ContainsKey(RCSAdr)) then
   Self.boards.Add(RCSAdr, TRCSBoard.Create());
 Self.boards[RCSAdr].needed := state
end;

function TRCS.GetNeeded(RCSAdr:Cardinal):boolean;
begin
 if (Self.boards.ContainsKey(RCSAdr)) then
   Result := Self.boards[RCSAdr].needed
 else
   Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TRCSBoard.Create();
begin
 inherited;
 Self.inputChangedEv  := TList<TRCSBoardChangeEvent>.Create();
 Self.outputChangedEv := TList<TRCSBoardChangeEvent>.Create();
end;

destructor TRCSBoard.Destroy();
begin
 Self.inputChangedEv.Free();
 Self.outputChangedEv.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddInputChangeEvent(board:Cardinal; event:TRCSBoardChangeEvent);
begin
 if (not Self.boards.ContainsKey(board)) then
   Self.boards.Add(board, TRCSBoard.Create());
 if (Self.boards[board].inputChangedEv.IndexOf(event) = -1) then
   Self.boards[board].inputChangedEv.Add(event);
end;

procedure TRCS.RemoveInputChangeEvent(event:TRCSBoardChangeEvent; board:Integer = -1);
var rcsBoard:TRCSBoard;
begin
 if (board = -1) then
  begin
   for rcsBoard in Self.boards.Values do
     rcsBoard.inputChangedEv.Remove(event);
  end else begin
   if (Self.boards.ContainsKey(board)) then
     Self.boards[board].inputChangedEv.Remove(event);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TRCS.AddOutputChangeEvent(board:Cardinal; event:TRCSBoardChangeEvent);
begin
 if (not Self.boards.ContainsKey(board)) then
   Self.boards.Add(board, TRCSBoard.Create());
 if (Self.boards[board].outputChangedEv.IndexOf(event) = -1) then
   Self.boards[board].outputChangedEv.Add(event);
end;

procedure TRCS.RemoveOutputChangeEvent(event:TRCSBoardChangeEvent; board:Integer = -1);
var rcsBoard:TRCSBoard;
begin
 if (board = -1) then
  begin
   for rcsBoard in Self.boards.Values do
     rcsBoard.outputChangedEv.Remove(event);
  end else begin
   if (Self.boards.ContainsKey(board)) then
     Self.boards[board].outputChangedEv.Remove(event);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TRCS.NoExStarted():boolean;
begin
 try
   Result := Self.Started();
 except
   Result := false;
 end;
end;

function TRCS.NoExOpened():boolean;
begin
 try
   Result := Self.Opened();
 except
   Result := false;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TRCS.IsSimulatorMode():boolean;
begin
 Result := (LowerCase(ExtractFileName(Self.Lib)) = 'simulator.dll');
end;

////////////////////////////////////////////////////////////////////////////////

class function TRCS.RCSAddr(board:Byte; port:Byte):TRCSAddr;
begin
 Result.board := board;
 Result.port := port;
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
