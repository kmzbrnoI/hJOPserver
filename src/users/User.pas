unit User;

{
  Trida TUser reprezentuje jednoho uzivatele.

  Uzivatel se typicky vyznacuje
   - id (unikatni identifikacni string, napr. "root"),
   - krestnim jmenem a prijmenim,
   - flagy opravnujici ho k jednotlivym cinnostem (napr. \root, \regulator),
   - seznamem oblasti rizeni, ke kterym ma pristup, vcetne nejvyssi pristuppove
     urovne u kazde oblasti rizeni, kterou je schopen autorizovat.

  Uzivatel muze dostat ban, v takovem pripad je okamzite odpojen ze serveru
  a neni mu umoznen dalsi pristup.
}

interface

uses IniFiles, Generics.Collections, SysUtils, Classes,
  Generics.Defaults, Area, Windows, JsonDataObjects;

const
  _SALT_LEN = 24;

type
  TUser = class
  private
    fusername: string;
    fpasswd: string; // SHA256 hash
    fban: Boolean;
    freg: Boolean;
    fsalt: string;

    procedure SetPasswd(passwd: string);
    procedure SetBan(state: Boolean);
    procedure SetReg(state: Boolean);

    procedure SetUserName(new: string);

    function GetFullName(): string;
    class function GenSalt(): string;

  public

    firstname: string;
    lastname: string;
    root: Boolean;
    areas: TDictionary<string, TAreaRights>;
    note: string;
    lastlogin: TDateTime;

    constructor Create(); overload;
    constructor Create(iniData, iniStat: TMemIniFile; section: string); overload;
    destructor Destroy(); override;

    procedure LoadData(ini: TMemIniFile; section: string);
    procedure LoadStat(ini: TMemIniFile; section: string);
    procedure SaveData(ini: TMemIniFile; section: string);
    procedure SaveStat(ini: TMemIniFile; section: string);

    function GetRights(areaId: string): TAreaRights;
    procedure SetRights(areaId: string; rights: TAreaRights);
    function PasswordMatch(hash: string): Boolean;
    function LoginMatch(username: string; passwordhash: string): Boolean;

    property username: string read fusername write SetUserName;
    property password: string read fpasswd write SetPasswd;
    property ban: Boolean read fban write SetBan;
    property regulator: Boolean read freg write SetReg;
    property fullName: string read GetFullName;
    property salt: string read fsalt;

    procedure GetPtData(json: TJsonObject);

    class function ComparePasswd(plain: string; hash: string; salt: string): Boolean;
    // check password match; return true iff match
    class function GenerateHash(plain: AnsiString): string;
    class function NameComparer(): IComparer<TUser>;
  end; // class TUser

implementation

uses AreaDb, TCPServerPanel, UserDb, ownStrUtils, System.hash;

/// /////////////////////////////////////////////////////////////////////////////

constructor TUser.Create(iniData, iniStat: TMemIniFile; section: string);
begin
  inherited Create();
  Self.areas := TDictionary<string, TAreaRights>.Create();
  Self.LoadData(iniData, section);
  Self.LoadStat(iniStat, section);
end;

constructor TUser.Create();
begin
  Self.areas := TDictionary<string, TAreaRights>.Create();
  inherited Create();
end;

destructor TUser.Destroy();
begin
  Self.areas.Free();
  inherited Destroy();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.LoadData(ini: TMemIniFile; section: string);
var data: TStrings;
begin
  Self.areas.Clear();

  Self.fusername := section;
  Self.fpasswd := ini.ReadString(section, 'passwd', '');
  Self.root := ini.ReadBool(section, 'root', false);
  Self.firstname := ini.ReadString(section, 'fname', '');
  Self.lastname := ini.ReadString(section, 'lname', '');
  Self.fsalt := ini.ReadString(section, 'salt', '');
  Self.fban := ini.ReadBool(section, 'ban', false);
  Self.freg := ini.ReadBool(section, 'reg', false);
  Self.note := DescapeNewline(ini.ReadString(section, 'note', ''));

  try
    if (ini.ValueExists(section, 'lastlogin')) then // backward compatibility
      Self.lastlogin := StrToDateTime(ini.ReadString(section, 'lastlogin', ''));
  except
    Self.lastlogin := 0;
  end;

  data := TStringList.Create();
  try
    ExtractStrings(['(', ')', ',', ';'], [], PChar(ini.ReadString(section, 'ORs', '')), data);
    for var i: Integer := 0 to (data.Count div 2) - 1 do
    begin
      try
        Self.areas.Add(data[i * 2], TAreaRights(StrToInt(data[i * 2 + 1])));
      except

      end;
    end;
  finally
    data.Free();
  end;
end;

procedure TUser.LoadStat(ini: TMemIniFile; section: string);
begin
  try
    if (ini.ValueExists(section, 'lastlogin')) then
      Self.lastlogin := StrToDateTime(ini.ReadString(section, 'lastlogin', ''));
  except
    Self.lastlogin := 0;
  end;
end;

procedure TUser.SaveData(ini: TMemIniFile; section: string);
var rights: TAreaRights;
begin
  ini.WriteString(section, 'passwd', Self.fpasswd);

  if (Self.root) then
    ini.WriteBool(section, 'root', Self.root);

  if (Self.firstname <> '') then
    ini.WriteString(section, 'fname', Self.firstname);

  if (Self.lastname <> '') then
    ini.WriteString(section, 'lname', Self.lastname);

  if (Self.note <> '') then
    ini.WriteString(section, 'note', EscapeNewline(Self.note));

  if (Self.ban) then
    ini.WriteBool(section, 'ban', Self.ban);

  if (Self.regulator) then
    ini.WriteBool(section, 'reg', Self.regulator);

  if (Self.salt <> '') then
    ini.WriteString(section, 'salt', Self.fsalt);

  var str: string := '';
  for var area: TArea in AreaDb.areas do
    if (Self.areas.TryGetValue(area.id, rights)) then
      str := str + '(' + area.id + ';' + IntToStr(Integer(rights)) + ')';

  if (str <> '') then
    ini.WriteString(section, 'ORs', str);
end;

procedure TUser.SaveStat(ini: TMemIniFile; section: string);
begin
  ini.WriteString(section, 'lastlogin', DateTimeToStr(Self.lastlogin));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.SetPasswd(passwd: string);
begin
  // hash password 2 times
  Self.fsalt := Self.GenSalt();
  Self.fpasswd := TUser.GenerateHash(AnsiString(TUser.GenerateHash(AnsiString(passwd)) + Self.fsalt));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TUser.GetRights(areaId: string): TAreaRights;
var rights: TAreaRights;
begin
  if (Self.root) then
    Exit(TAreaRights.superuser);

  if (Self.areas.TryGetValue(areaId, rights)) then
    Result := rights
  else
    Result := null;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TUser.ComparePasswd(plain: string; hash: string; salt: string): Boolean;
begin
  Result := (hash = TUser.GenerateHash(AnsiString(LowerCase(plain + salt))));
end;

class function TUser.GenerateHash(plain: AnsiString): string;
begin
  Result := LowerCase(System.hash.THashSHA2.GetHashString(plain, SHA256));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.SetRights(areaId: string; rights: TAreaRights);
var Area: TArea;
begin
  Self.areas.AddOrSetValue(areaId, rights);
  Area := AreaDb.areas.Get(areaId);
  if (Area <> nil) then
    Area.UserUpdateRights(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.SetBan(state: Boolean);
begin
  if (Self.fban = state) then
    Exit();
  Self.fban := state;
  if (Self.fban) then
  begin
    // user prave dostal BAN -> aktualizovat oblasti rizeni
    for var areaId: string in Self.areas.Keys do
    begin
      var area: TArea := AreaDb.areas.Get(areaId);
      if (Area <> nil) then
        Area.UserUpdateRights(Self);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.SetReg(state: Boolean);
begin
  if (Self.freg = state) then
    Exit();
  Self.freg := state;
  if (not Self.freg) then
    PanelServer.DisconnectRegulatorUser(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.SetUserName(new: string);
begin
  if (Self.username <> new) then
  begin
    Self.fusername := new;
    UsrDB.Sort();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TUser.GetFullName(): string;
begin
  Result := Self.firstname + ' ' + Self.lastname;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TUser.GenSalt(): string;
var all: string;
  len: Integer;
begin
  all := 'abcdefghijklmnopqrstuvwxyz0123456789';
  len := Length(all);

  Result := '';
  for var i: Integer := 0 to _SALT_LEN - 1 do
    Result := Result + all[Random(len) + 1];
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TUser.NameComparer(): IComparer<TUser>;
begin
  Result := TComparer<TUser>.Construct(
    function(const Left, Right: TUser): Integer
    begin
      Result := CompareStr(Left.username, Right.username, loUserLocale);
    end);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TUser.PasswordMatch(hash: string): Boolean;
begin
  Result := TUser.ComparePasswd(hash, Self.password, Self.salt);
end;

function TUser.LoginMatch(username: string; passwordhash: string): Boolean;
begin
  Result := (username = Self.username) and (Self.PasswordMatch(passwordhash));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUser.GetPtData(json: TJsonObject);
var orrights: TPair<string, TAreaRights>;
begin
  json['username'] := Self.username;
  json['firstname'] := Self.firstname;
  json['lastname'] := Self.lastname;
  json['root'] := Self.root;
  json['note'] := Self.note;
  json['lastlogin'] := Self.lastlogin;
  json['ban'] := Self.ban;
  json['regulator'] := Self.regulator;

  json.O['ors'];
  for orrights in Self.areas do
  begin
    case (orrights.Value) of
      TAreaRights.read:
        json['ors'].O[orrights.Key] := 'read';
      TAreaRights.write:
        json['ors'].O[orrights.Key] := 'write';
      TAreaRights.superuser:
        json['ors'].O[orrights.Key] := 'superuser';
    else
      json['ors'].O[orrights.Key] := 'null';
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
