unit RCSsc;

interface

uses Generics.Defaults;

type
  // Single RCS address with system too
  TRCSsAddr = record
    system: Cardinal;
    module: Cardinal;
    port: Byte;
    class operator Equal(a, b: TRCSsAddr): Boolean;
    procedure Load(str: string);
    function ToString(): string;
  end;

  // Access to multiple Railroad Control Systems
  TRCSs = class
  private

  public
    constructor Create();
    destructor Destroy(); override;

  end;

var
  RCSs: TRCSs;

function RCSAddrComparer(): IComparer<TRCSsAddr>;

////////////////////////////////////////////////////////////////////////////////

implementation

uses SysUtils, Classes;

constructor TRCSs.Create();
begin
  inherited;
end;

destructor TRCSs.Destroy();
begin
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

class operator TRCSsAddr.Equal(a, b: TRCSsAddr): Boolean;
begin
  Result := ((a.system = b.system) and (a.module = b.module) and (a.port = b.port));
end;

function TRCSsAddr.ToString(): string;
begin
  Result := IntToStr(system) + ':' + IntToStr(module) + ':' + IntToStr(port);
end;

procedure TRCSsAddr.Load(str: string);
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStrings([':'], [], PChar(str), strs);
    if (strs.Count = 2) then
    begin
      system := 0;
      module := StrToInt(strs[0]);
      port := StrToInt(strs[1]);
    end else if (strs.Count = 3) then begin
      system := StrToInt(strs[0]);
      module := StrToInt(strs[1]);
      port := StrToInt(strs[2]);
    end else begin
      raise Exception.Create('Unable to load RCS: '+str);
    end;
  finally
    strs.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function RCSAddrComparer(): IComparer<TRCsSAddr>;
begin
  Result := TComparer<TRCsSAddr>.Construct(
    function(const Left, Right: TRCsSAddr): Integer
    begin
      if (Left.system < Right.system) then
        Exit(-1);
      if (Left.system > Right.system) then
        Exit(1);
      if (Left.module < Right.module) then
        Exit(-1);
      if (Left.module > Right.module) then
        Exit(1);
      if (Left.port < Right.port) then
        Exit(-1);
      if (Left.port > Right.port) then
        Exit(1);
      Result := 0;
    end);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  RCSs := TRCSs.Create();

finalization
  // Free in hJOPserver.dpr, because we must gurantee preload gets destructed after all shared libraries

end.
