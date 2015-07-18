unit fConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Outputdriver, ExtCtrls, StdCtrls, Menus, ImgList, Buttons, ComCtrls,
  inifiles, ActnList, AppEvnts, Mask, ScktComp, ToolWin, adCpuUsage,
  ExtDlgs,  Gauges, Registry, StrUtils, fLicence, mmsystem, Grids, Spin, ValEdit,
  DateUtils, ShellApi, ActiveX, ShlObj, ComObj;

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

uses fMain, fSettings, RPConst, TechnologieMTB, GetSystems, Verze,
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
 Self.M_console.Lines.Add('InCommandRP'+GetZkrVersion(Application.ExeName)+'\>');
end;//procedure

procedure TF_Console.B_OK_consoleClick(Sender: TObject);
var cyklus,cyklus1:integer;
    pole_dat:TStrings;
    return:Byte;
    Blk:TBlk;
begin
 pole_dat := TStringList.Create;
 ExtractStrings([' ','(',')'], [],PChar(LowerCase(E_console.Text)),pole_dat);

 M_console.Lines.Strings[M_console.Lines.Count-1] := M_console.Lines.Strings[M_console.Lines.Count-1] + '['+E_console.Text+']';

 if (F_Options.CHB_Log_console.Checked) then
   writelog('Console: '+E_console.Text,WR_CONSOLE);

 if (pole_dat.Count <> 0) then
  begin
   if (pole_dat[0] = 'help') then
    begin
     M_console.Lines.Add('InCommandRP404\>Napoveda :');
     M_console.Lines.Add('centrala [open/close]               Pripojeni k centrale');
     M_console.Lines.Add('mtb [start/stop/open/close] [hard]  Povely pro MTB');
     M_console.Lines.Add('clear                               Smaze konzoli');
     M_console.Lines.Add('lic-accept                          Akceptuje licenci');
     M_console.Lines.Add('exit                                Zavre okno konzole');
     M_console.Lines.Add('app-exit                            Nouzove regulerni ukonceni aplikace');
     M_console.Lines.Add('log-stop                            Zastaveni vypisu do centralniho logu');
     M_console.Lines.Add('getobj [form]                       Vypise vsechny objekty z formulare v parametru');
    end;

   if (pole_dat[0] = 'clear') then PM_DeleteConsoleClick(Self);

   if (pole_dat[0] = 'lic-accept') then
    begin
     Lic.Prijato := true;
     M_console.Lines.Add('InCommandRP167\>Licence prijimuta');
    end;

   if (pole_dat[0] = 'getobj') and (pole_dat.Count = 2) then
    begin
     for cyklus := 0 to Application.ComponentCount-1 do
       if (LowerCase(Application.Components[cyklus].ClassName) = 't'+pole_dat[1]) then
         for cyklus1 := 0 to Application.Components[cyklus].ComponentCount-1 do
           M_console.Lines.Add('InCommandRP177\>'+pole_dat[1]+':'+Application.Components[cyklus].Components[cyklus1].Name+'('+Application.Components[cyklus].Components[cyklus1].ClassName+')');
    end;

   if (pole_dat[0] = 'exit') then
    begin
     M_console.Lines.Add('InCommandRP207\>Zavreno okno konzole');
     F_Console.Close;
    end;

   if (pole_dat[0] = 'centrala') and (pole_dat.Count >= 2) then
    begin
     if (pole_dat[1] = 'open') then
      begin
       M_console.Lines.Add('InCommandRP404\>Pripojuji se k centrale');
       return := TrkSystem.Open();
       if (return <> 0) then M_console.Lines.Add('InCommandRP262\>Chyba pri pripojovani: chyba '+IntToStr(return))
        else M_console.Lines.Add('InCommandRP404\>Pripojeno k centrale');
      end;

     if (pole_dat[1] = 'close') then
      begin
       M_console.Lines.Add('InCommandRP262\>Odpojuji se od cntraly');
       return := TrkSystem.Close();
       if (return <> 0) then M_console.Lines.Add('InCommandRP262\>Chyba pri odpojovani: chyba '+IntToStr(return))
        else M_console.Lines.Add('InCommandRP262\>Odpojeno od centraly');
      end;
    end;

   if (pole_dat[0] = 'mtb') and (pole_dat.Count >= 2) then
    begin
     if (pole_dat[1] = 'start') then
      begin
       if (pole_dat.Count >= 3) then
        begin
         if (pole_dat[2] = 'hard') then MTB.Go;
        end else begin
         F_Main.A_MTB_GoExecute(Self);
        end;
      end;
     if (pole_dat[1] = 'stop') then
      begin
       if (pole_dat.Count >= 3) then
        begin
         if (pole_dat[2] = 'hard') then MTB.Stop;
        end else begin
         F_Main.A_MTB_StopExecute(Self);
        end;
      end;

     if (pole_dat[1] = 'open') then
      begin
       if (pole_dat.Count >= 3) then
        begin
         if (pole_dat[2] = 'hard') then MTB.Open;
        end else begin
         F_Main.A_MTB_OpenExecute(Self);
        end;
      end;
     if (pole_dat[1] = 'close') then
      begin
       if (pole_dat.Count >= 3) then
        begin
         if (pole_dat[2] = 'hard') then MTB.Close;
        end else begin
         F_Main.A_MTB_CloseExecute(Self);
        end;
      end;

     M_console.Lines.Add('InCommandRP271\Hotovo');
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
        M_console.Lines.Add('InCommandRP265\>Blok s timto id neexistuje');
        Exit;
       end;

      case (Blk.GetGlobalSettings().typ) of
       _BLK_USEK, _BLK_TU: begin
         (Blk as TBlkUsek).Zaver := TJCType.no;
         M_console.Lines.Add('InCommandRP265\>Zrusen zaver useku '+Blk.GetGlobalSettings().name);
       end;

       _BLK_VYH:begin
//         (Blk as TBlkVyhybka).Unlock(true);
//         M_console.Lines.Add('InCommandRP265\>Uvolnena vyhybka '+Blk.GetGlobalSettings().name);
       end;

      else
        M_console.Lines.Add('InCommandRP265\>Na bloku s timto typem neumim provest NUZ');
      end;
     end;

  end;//Count <> 0

 M_console.Lines.Add('InCommandRP'+GetZkrVersion(Application.ExeName)+'\>');
 E_console.Text := '';
 pole_dat.Free;
end;//procedure

procedure TF_Console.PM_DeleteConsoleClick(Sender: TObject);
 begin
  M_console.Clear;
  M_console.Lines.Add('------------------------');
  M_console.Lines.Add('InCommandRP'+GetZkrVersion(Application.ExeName)+'\>');
 end;//procedure

end.//unit
