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

uses IniFiles, Generics.Collections, DCPsha256, SysUtils, Classes,
     Generics.Defaults, TOblRizeni, Windows;

const
  _SALT_LEN = 24;

type
  TUser = class
   private
    fusername: string;
    fpasswd: string;  // SHA256 hash
    fban: boolean;
    freg: boolean;
    fsalt: string;

     procedure SetPasswd(passwd: string);
     procedure SetBan(state: boolean);
     procedure SetReg(state: boolean);

     procedure SetUserName(new: string);

     function GetFullName(): string;
     class function GenSalt(): string;

   public

    firstname: string;
    lastname: string;
    root: boolean;
    OblR: TDictionary<string, TORControlRights>;
    note: string;
    lastlogin: TDateTime;

     constructor Create(); overload;
     constructor Create(iniData, iniStat: TMemIniFile; section: string); overload;
     destructor Destroy(); override;

     procedure LoadData(ini: TMemIniFile; section: string);
     procedure LoadStat(ini: TMemIniFile; section: string);
     procedure SaveData(ini: TMemIniFile; section: string);
     procedure SaveStat(ini: TMemIniFile; section: string);

     function GetRights(OblR: string): TORCOntrolRights;
     procedure SetRights(OblR: string; rights: TORControlRights);
     function PasswordMatch(hash: string): Boolean;
     function LoginMatch(username: string; passwordhash: string): Boolean;

     property username: string read fusername write SetUserName;
     property password: string read fpasswd write SetPasswd;
     property ban: boolean read fban write SetBan;
     property regulator: boolean read freg write SetReg;
     property fullName: string read GetFullName;
     property salt: string read fsalt;

     class function ComparePasswd(plain: string; hash: string; salt: string):boolean;
        // check password match; return true iff match
     class function GenerateHash(plain:AnsiString):string;
     class function NameComparer():IComparer<TUser>;
  end;//class TUser

implementation

uses TOblsRizeni, TCPServerOR, UserDb, ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

constructor TUser.Create(iniData, iniStat:TMemIniFile; section:string);
begin
 inherited Create();
 Self.OblR := TDictionary<string, TORControlRights>.Create();
 Self.LoadData(iniData, section);
 Self.LoadStat(iniStat, section);
end;//ctor

constructor TUser.Create();
begin
 Self.OblR := TDictionary<string, TORControlRights>.Create();
 inherited Create();
end;//ctor

destructor TUser.Destroy();
begin
 Self.OblR.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TUser.LoadData(ini:TMemIniFile; section:string);
var i:Integer;
    data:TStrings;
begin
 Self.OblR.Clear();

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
   for i := 0 to (data.Count div 2)-1 do
    begin
     try
      Self.OblR.Add(data[i*2], TORControlRights(StrToInt(data[i*2 + 1])));
     except

     end;
    end;//for i
 finally
   data.Free();
 end;
end;

procedure TUser.LoadStat(ini:TMemIniFile; section:string);
begin
 try
   if (ini.ValueExists(section, 'lastlogin')) then
     Self.lastlogin := StrToDateTime(ini.ReadString(section, 'lastlogin', ''));
 except
   Self.lastlogin := 0;
 end;
end;

procedure TUser.SaveData(ini:TMemIniFile; section:string);
var rights:TORControlRights;
    str:string;
    oblr: TOR;
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

 if (self.salt <> '') then
   ini.WriteString(section, 'salt', Self.fsalt);

 str := '';
 for oblr in ORs do
   if (Self.OblR.TryGetValue(oblr.id, rights)) then
     str := str + '(' + oblr.id + ';' + IntToStr(Integer(rights)) + ')';

 if (str <> '') then
   ini.WriteString(section, 'ORs', str);
end;

procedure TUser.SaveStat(ini:TMemIniFile; section:string);
begin
 ini.WriteString(section, 'lastlogin', DateTimeToStr(Self.lastlogin));
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetPasswd(passwd:string);
begin
 // hash password 2 times
 Self.fsalt := Self.GenSalt();
 Self.fpasswd := TUser.GenerateHash(AnsiString(TUser.GenerateHash(AnsiString(passwd)) + self.fsalt));
end;

////////////////////////////////////////////////////////////////////////////////

function TUser.GetRights(OblR:string):TORCOntrolRights;
var rights:TORControlRights;
begin
 if (Self.root) then Exit(TORCOntrolRights.superuser);

 if (Self.OblR.TryGetValue(OblR, rights)) then
  Result := rights
 else
  Result := null;
end;

////////////////////////////////////////////////////////////////////////////////

class function TUser.ComparePasswd(plain:string; hash:string; salt:string):boolean;
begin
 Result := (hash = TUser.GenerateHash(AnsiString(LowerCase(plain + salt))));
end;

class function TUser.GenerateHash(plain:AnsiString):string;
var hash: TDCP_sha256;
    Digest: array[0..31] of byte;  // RipeMD-160 produces a 160bit digest (20bytes)
    i:Integer;
begin
 hash := TDCP_sha256.Create(nil);
 hash.Init();
 hash.UpdateStr(plain);
 hash.Final(digest);
 hash.Free();

 Result := '';
 for i := 0 to 31 do
   Result := Result + IntToHex(Digest[i], 2);
 Result := LowerCase(Result);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetRights(OblR:string; rights:TORControlRights);
var OblRRef:TOR;
begin
 Self.OblR.AddOrSetValue(OblR, rights);
 OblRRef := ORs.Get(OblR);
 if (OblRRef <> nil) then
   OblRRef.UserUpdateRights(Self);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetBan(state:boolean);
var oblr:string;
    OblRRef:TOR;
begin
 if (Self.fban = state) then Exit();
 Self.fban := state;
 if (Self.fban) then
  begin
   // user prave dostal BAN -> aktualizovat oblasti rizeni
   for oblr in Self.OblR.Keys do
    begin
     OblRRef := ORs.Get(oblr);
     if (OblRRef <> nil) then
       OblRRef.UserUpdateRights(Self);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetReg(state:boolean);
begin
 if (Self.freg = state) then Exit();
 Self.freg := state;
 if (not Self.freg) then ORTCPServer.DisconnectRegulatorUser(self);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetUserName(new:string);
begin
 if (Self.username <> new) then
  begin
   Self.fusername := new;
   UsrDB.Sort();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TUser.GetFullName():string;
begin
 Result := Self.firstname + ' ' + Self.lastname;
end;

////////////////////////////////////////////////////////////////////////////////

class function TUser.GenSalt():string;
var all:string;
    i, len:Integer;
begin
 all := 'abcdefghijklmnopqrstuvwxyz0123456789';
 len := Length(all);

 Result := '';
 for i := 0 to _SALT_LEN-1 do
   Result := Result + all[Random(len)+1];
end;

////////////////////////////////////////////////////////////////////////////////

class function TUser.NameComparer():IComparer<TUser>;
begin
 Result := TComparer<TUser>.Construct(
    function (const Left, Right: TUser): Integer
      begin
        Result := CompareStr(Left.username, Right.username, loUserLocale);
      end
 );
end;

////////////////////////////////////////////////////////////////////////////////

function TUser.PasswordMatch(hash: string): Boolean;
begin
 Result := TUser.ComparePasswd(hash, Self.password, Self.salt);
end;

function TUser.LoginMatch(username: string; passwordhash: string): Boolean;
begin
 Result := (username = Self.username) and (Self.PasswordMatch(passwordhash));
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
