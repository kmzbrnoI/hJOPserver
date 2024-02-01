unit ownGuiUtils;

interface

uses SysUtils, Forms, Windows;

procedure ExceptionMessageBox(msg: string; caption: string; e: Exception);

implementation

procedure ExceptionMessageBox(msg: string; caption: string; e: Exception);
begin
  Application.MessageBox(PChar(msg + #13#10 + e.Message), PChar(caption), MB_OK OR MB_ICONERROR);
end;

end.
