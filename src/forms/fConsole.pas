unit fConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls,
  inifiles, ActnList, AppEvnts, Mask, ScktComp, ToolWin, adCpuUsage,
  ExtDlgs,  Gauges, Registry, StrUtils, mmsystem, Grids, Spin, ValEdit,
  DateUtils, ShellApi, ActiveX, ShlObj, ComObj;

const
 _CONSOLE_V = '1.6';

type
  TF_Console = class(TForm)
    M_console: TMemo;
    E_console: TEdit;
    B_OK_console: TButton;
    PM_Console: TPopupMenu;
    PM_DeleteConsole: TMenuItem;
    procedure E_consoleKeyPress(Sender: TObject; var Key: Char);
    procedure B_OK_consoleClick(Sender: TObject);
    procedure PM_DeleteConsoleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  F_Console: TF_Console;

implementation

uses fMain, fSettings, TechnologieRCS, GetSystems, Verze,
     Logging, TBlok, TBlokUsek, TBLokVyhybka, TBLoky;

{$R *.dfm}

procedure TF_Console.E_consoleKeyPress(Sender: TObject; var Key: Char);
 begin
  if (Key = #13) then B_OK_consoleClick(self);
 end;

procedure TF_Console.FormCreate(Sender: TObject);
begin
 Self.M_console.Lines.Add('Spustena konzole - v '+_CONSOLE_V);
 Self.M_console.Lines.Add('------------------------');
 Self.M_console.Lines.Add('> ');
end;//procedure

procedure TF_Console.B_OK_consoleClick(Sender: TObject);
var pole_dat:TStrings;
    return:Byte;
    Blk:TBlk;
begin
 pole_dat := TStringList.Create;

 try
   ExtractStrings([' ','(',')'], [],PChar(LowerCase(E_console.Text)),pole_dat);

   M_console.Lines.Strings[M_console.Lines.Count-1] := M_console.Lines.Strings[M_console.Lines.Count-1] + E_console.Text;

   if (F_Options.CHB_Log_console.Checked) then
     writelog('Console: '+E_console.Text,WR_CONSOLE);

   if (pole_dat.Count <> 0) then
    begin
     if (pole_dat[0] = 'help') then
      begin
       M_console.Lines.Add('Napoveda:');
       M_console.Lines.Add('centrala [open/close]       Pripojeni k centrale');
       M_console.Lines.Add('rcs [start/stop/open/close] Povely pro RCS');
       M_console.Lines.Add('clear                       Smaze konzoli');
       M_console.Lines.Add('exit                        Zavre okno konzole');
       M_console.Lines.Add('app-exit                    Nouzove regulerni ukonceni aplikace');
       M_console.Lines.Add('nuz [id bloku useku]        Nouzove uvolneni zaveru useku');
      end;

     if (pole_dat[0] = 'clear') then PM_DeleteConsoleClick(Self);

     if (pole_dat[0] = 'exit') then
      begin
       M_console.Lines.Add('Zavreno okno konzole');
       F_Console.Close;
      end;

     if (pole_dat[0] = 'centrala') and (pole_dat.Count >= 2) then
      begin
       if (pole_dat[1] = 'open') then
        begin
         M_console.Lines.Add('Pripojuji se k centrale...');
         return := TrkSystem.Open();
         if (return <> 0) then M_console.Lines.Add('Chyba pri pripojovani: chyba '+IntToStr(return))
          else M_console.Lines.Add('Pripojeno k centrale');
        end;

       if (pole_dat[1] = 'close') then
        begin
         M_console.Lines.Add('Odpojuji se od cntraly...');
         return := TrkSystem.Close();
         if (return <> 0) then M_console.Lines.Add('Chyba pri odpojovani: chyba '+IntToStr(return))
          else M_console.Lines.Add('Odpojeno od centraly');
        end;
      end;

     if (pole_dat[0] = 'rcs') and (pole_dat.Count >= 2) then
      begin
       if (pole_dat[1] = 'start') then
         F_Main.A_RCS_GoExecute(Self);

       if (pole_dat[1] = 'stop') then
         F_Main.A_RCS_StopExecute(Self);

       if (pole_dat[1] = 'open') then
         F_Main.A_RCS_OpenExecute(Self);

       if (pole_dat[1] = 'close') then
         F_Main.A_RCS_CloseExecute(Self);
      end;

     if (pole_dat[0] = 'app-exit') then
      begin
       NUZClose     := true;
       CloseMessage := false;
       F_Main.Close;
       Exit;
      end;

      if (pole_dat[0] = 'nuz') then
       begin
        if (pole_dat.Count < 2) then Exit;

        Blky.GetBlkByID(StrToInt(pole_dat[1]), Blk);

        if (Blk = nil) then
         begin
          M_console.Lines.Add('Blok s timto id neexistuje');
         end else begin
          case (Blk.typ) of
           _BLK_USEK, _BLK_TU: begin
             (Blk as TBlkUsek).Zaver := TZaver.no;
             M_console.Lines.Add('Zrusen zaver useku '+Blk.name);
           end;
          else
            M_console.Lines.Add('Na bloku s timto typem neumim provest NUZ');
          end;
         end;
       end;
    end;//Count <> 0
 except
   on E:Exception do
     M_Console.Lines.Add('Exception : ' + E.Message);
 end;

 M_console.Lines.Add('> ');
 E_console.Text := '';
 pole_dat.Free;
end;//procedure

procedure TF_Console.PM_DeleteConsoleClick(Sender: TObject);
 begin
  M_console.Clear();
 end;//procedure

end.//unit
