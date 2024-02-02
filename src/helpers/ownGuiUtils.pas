unit ownGuiUtils;

interface

uses SysUtils, Forms, Windows;

function StrMessageBox(text: string; caption: string; flags: Integer = 0): Integer;
procedure ErrorMessageBox(msg: string; error: string = ''; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING);
procedure ExceptionMessageBox(msg: string; e: Exception; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING); overload;
procedure ExceptionMessageBox(e: Exception; msg: string = 'Chyba:'; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING); overload;

implementation

function StrMessageBox(text: string; caption: string; flags: Integer = 0): Integer;
begin
  Result := Application.MessageBox(PChar(text), PChar(caption), flags);
end;

procedure ErrorMessageBox(msg: string; error: string = ''; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING);
begin
  var content: string := '';
  if ((msg <> '') and (error <> '')) then
    content := msg + #13#10 + error
  else
    content := msg + error;

  StrMessageBox(content, caption, flags);
end;

procedure ExceptionMessageBox(msg: string; e: Exception; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING);
begin
  ErrorMessageBox(msg, e.Message, caption, flags);
end;

procedure ExceptionMessageBox(e: Exception; msg: string = 'Chyba:'; caption: string = 'Chyba'; flags: Integer = MB_OK OR MB_ICONWARNING);
begin
  ErrorMessageBox(msg, e.Message, caption, flags);
end;

end.
