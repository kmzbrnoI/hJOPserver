unit version;

interface

uses Windows, SysUtils;

 function VersionStr(const FileName: string; release: Boolean = True): string; overload;
 function VersionStr(): string; overload;
 function BuildDateTime(): TDateTime;
 function StandardVersionBuildStr(): string;

 const _RELEASE: Boolean = False;

implementation

uses DateUtils, Forms;

function VersionStr(): string;
begin
  Result := VersionStr(Application.ExeName);
  if (not _RELEASE) then
    Result := Result + '-dev';
end;

function VersionStr(const FileName: string; release: Boolean): string;
var
  size, len: longword;
  handle: Cardinal;
begin
  Result := '?.?.?';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if (size > 0) then
   begin
    var buffer: PChar;
    var pinfo: ^VS_FIXEDFILEINFO;
    GetMem(buffer, size);

    try
      if ((GetFileVersionInfo(Pointer(FileName), 0, size, buffer)) and
          (VerQueryValue(buffer, '\', pointer(pinfo), len))) then
       begin
        var _major: Word := HiWord(pinfo.dwFileVersionMS);
        var _minor: Word := LoWord(pinfo.dwFileVersionMS);
        var _release: Word := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d', [_major, _minor]);
        if (release) then
          Result := Result + '.' + IntToStr(_release);
       end;
    finally
      FreeMem(buffer);
    end;
  end;
end;

function BuildDateTime(): TDateTime;
begin
  Result := (TTimeZone.Local.ToLocalTime(PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp / SecsPerDay) + UnixDateDelta);
end;

function StandardVersionBuildStr(): string;
begin
  Result := 'v' + VersionStr() + ' (build ' + FormatDateTime('dd.mm.yyyy', BuildDateTime()) + ')';
end;

end.//unit
