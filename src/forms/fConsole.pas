unit fConsole;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, Buttons, ComCtrls,
  ActnList, AppEvnts, ComObj;

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

uses fMain, fSettings, TechnologieRCS, GetSystems, TechnologieTrakce,
  Logging, Block, BlockTrack, BlockDb;

{$R *.dfm}

procedure TF_Console.E_consoleKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    B_OK_consoleClick(self);
end;

procedure TF_Console.FormCreate(Sender: TObject);
begin
  self.M_console.Lines.Add('Spustena konzole - v ' + _CONSOLE_V);
  self.M_console.Lines.Add('------------------------');
  self.M_console.Lines.Add('> ');
end;

procedure TF_Console.B_OK_consoleClick(Sender: TObject);
var
  strings: TStrings;
begin
  strings := TStringList.Create();

  try
    ExtractStrings([' ', '(', ')'], [], PChar(LowerCase(E_console.Text)), strings);

    M_console.Lines.strings[M_console.Lines.Count - 1] := M_console.Lines.strings[M_console.Lines.Count - 1] +
      E_console.Text;

    if (F_Options.CHB_Log_console.Checked) then
      Log('Console: ' + E_console.Text, ltConsole);

    if (strings.Count <> 0) then
    begin
      if (strings[0] = 'help') then
      begin
        M_console.Lines.Add('Napoveda:');
        M_console.Lines.Add('centrala [open/close]       Pripojeni k centrale');
        M_console.Lines.Add('rcs [start/stop/open/close] Povely pro RCS');
        M_console.Lines.Add('clear                       Smaze konzoli');
        M_console.Lines.Add('exit                        Zavre okno konzole');
        M_console.Lines.Add('app-exit                    Nouzove regulerni ukonceni aplikace');
        M_console.Lines.Add('nuz [id bloku useku]        Nouzove uvolneni zaveru useku');
      end;

      if (strings[0] = 'clear') then
        PM_DeleteConsoleClick(self);

      if (strings[0] = 'exit') then
      begin
        M_console.Lines.Add('Zavreno okno konzole');
        F_Console.Close;
      end;

      if (strings[0] = 'centrala') and (strings.Count >= 2) then
      begin
        if (strings[1] = 'open') then
        begin
          M_console.Lines.Add('Pripojuji se k centrale...');
          try
            TrakceI.Connect();
          except
            on E: Exception do
            begin
              M_console.Lines.Add(E.Message);
              Exit();
            end;
          end;

          M_console.Lines.Add('Pripojeno k centrale');
        end;

        if (strings[1] = 'close') then
        begin
          M_console.Lines.Add('Odpojuji se od cntraly...');

          try
            TrakceI.Disconnect();
          except
            on E: Exception do
            begin
              M_console.Lines.Add(E.Message);
              Exit();
            end;
          end;
          M_console.Lines.Add('Odpojeno od centraly');
        end;
      end;

      if (strings[0] = 'rcs') and (strings.Count >= 2) then
      begin
        if (strings[1] = 'start') then
          F_Main.A_RCS_GoExecute(self);

        if (strings[1] = 'stop') then
          F_Main.A_RCS_StopExecute(self);

        if (strings[1] = 'open') then
          F_Main.A_RCS_OpenExecute(self);

        if (strings[1] = 'close') then
          F_Main.A_RCS_CloseExecute(self);
      end;

      if (strings[0] = 'app-exit') then
      begin
        F_Main.NUZClose := true;
        F_Main.CloseMessage := false;
        F_Main.Close();
        Exit();
      end;

      if (strings[0] = 'nuz') then
      begin
        if (strings.Count < 2) then
          Exit();

        var blk := Blocks.GetBlkByID(StrToInt(strings[1]));

        if (blk = nil) then
        begin
          M_console.Lines.Add('Blok s timto id neexistuje');
        end else begin
          case (blk.typ) of
            btTrack, btRT:
              begin
                (blk as TBlkTrack).Zaver := TZaver.no;
                M_console.Lines.Add('Zrusen zaver useku ' + blk.name);
              end;
          else
            M_console.Lines.Add('Na bloku s timto typem neumim provest NUZ');
          end;
        end;
      end;
    end; // Count <> 0
  except
    on E: Exception do
      M_console.Lines.Add('Exception : ' + E.Message);
  end;

  M_console.Lines.Add('> ');
  E_console.Text := '';
  strings.Free;
end;

procedure TF_Console.PM_DeleteConsoleClick(Sender: TObject);
begin
  M_console.Clear();
end;

end.
