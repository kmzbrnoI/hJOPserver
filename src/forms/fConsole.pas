unit fConsole;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, Buttons, ComCtrls,
  ActnList, AppEvnts, ComObj;

const
  _CONSOLE_V = '2.0';

type
  TF_Console = class(TForm)
    M_console: TMemo;
    E_console: TEdit;
    B_ok: TButton;
    PM_Console: TPopupMenu;
    PM_DeleteConsole: TMenuItem;
    procedure B_okClick(Sender: TObject);
    procedure PM_DeleteConsoleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure Print(str: string);

  public
    { Public declarations }
  end;

var
  F_Console: TF_Console;

implementation

uses fMain, RCSc, GetSystems, TrakceC, appEv, Logging, Block, BlockTrack, BlockDb, Config;

{$R *.dfm}

procedure TF_Console.FormCreate(Sender: TObject);
begin
  Self.Print('hJOPserver console v' + _CONSOLE_V);
end;

procedure TF_Console.FormResize(Sender: TObject);
begin
  Self.E_console.Width := Self.ClientWidth - Self.B_ok.Width - 25;
  Self.E_console.Top := Self.ClientHeight - Self.E_console.Height - 10;
  Self.B_ok.Top := Self.E_console.Top;
  Self.B_ok.Left := Self.E_console.Left + Self.E_console.Width + 10;
  Self.M_console.Height := Self.E_console.Top - 10;
end;

procedure TF_Console.Print(str: string);
begin
  Self.M_console.Lines.Add(str);
end;

procedure TF_Console.B_okClick(Sender: TObject);
var
  strings: TStrings;
begin
  strings := TStringList.Create();
  try
    try
      ExtractStrings([' ', '(', ')'], [], PChar(LowerCase(E_console.Text)), strings);

      Self.Print('> ' + Self.E_console.Text);

      if (GlobalConfig.consoleLog) then
        Log('Console: ' + E_console.Text, llInfo, lsConsole);

      if (strings.Count < 1) then
        Exit();

      if (strings[0] = 'help') then
      begin
        Self.Print('Help:');
        Self.Print('trakce [open/close]         Trakce control');
        Self.Print('rcs [start/stop/open/close] RCS control');
        Self.Print('clear                       Clear console window');
        Self.Print('app-exit                    Emergency exit of hJOPserver');
        Self.Print('nuz [blockid]               Emergency zaver release');
        Self.Print('supress-exception           Supress critical system exception blocking system start');
      end

      else if (strings[0] = 'clear') then
        Self.PM_DeleteConsoleClick(self)

      else if (strings[0] = 'trakce') and (strings.Count >= 2) then
      begin
        if (strings[1] = 'open') then
        begin
          Self.Print('Connecting to Trakce...');
          trakce.Connect();
          Self.Print('Done');
        end;

        if (strings[1] = 'close') then
        begin
          Self.Print('Disconnecting from Trakce...');
          trakce.Disconnect();
          Self.Print('Done');
        end;
      end

      else if (strings[0] = 'rcs') and (strings.Count >= 2) then
      begin
        if (strings[1] = 'start') then
          F_Main.A_RCS_GoExecute(self);

        if (strings[1] = 'stop') then
          F_Main.A_RCS_StopExecute(self);

        if (strings[1] = 'open') then
          F_Main.A_RCS_OpenExecute(self);

        if (strings[1] = 'close') then
          F_Main.A_RCS_CloseExecute(self);
      end

      else if (strings[0] = 'app-exit') then
      begin
        F_Main.NUZClose := true;
        F_Main.CloseMessage := false;
        F_Main.Close();
        Exit();
      end

      else if (strings[0] = 'nuz') then
      begin
        if (strings.Count < 2) then
        begin
          Self.Print('Missing argument!');
          Exit();
        end;

        var blk := Blocks.GetBlkByID(StrToInt(strings[1]));

        if (blk = nil) then
        begin
          Self.Print('Block '+strings[1] + ' does not exist!');
        end else begin
          case (blk.typ) of
            btTrack, btRT:
              begin
                (blk as TBlkTrack).Zaver := TZaver.no;
                Self.Print('NUZ done on ' + blk.name);
              end;
          else
            Self.Print('Cannot perform NUZ on this block type!');
          end;
        end;
      end

      else if (strings[0] = 'supress-exception') then
      begin
        AppEvents.lastException := nil;
        Self.Print('Exception successfully supressed.');
      end

      else begin
        Self.Print('Unknown command: '+strings[0]);
      end;

      Self.E_console.Text := '';
    except
      on E: Exception do
        Self.Print('Exception : ' + E.Message);
    end;
  finally
    strings.Free();
  end;
end;

procedure TF_Console.PM_DeleteConsoleClick(Sender: TObject);
begin
  M_console.Clear();
end;

end.
