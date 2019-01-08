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
  TUser = class                                                                 // trida reprezentujici uzivatele
   private
    fpasswd:string;                                                             // heslo - hash SHA256
    fban:boolean;                                                               // flag urcujici ban uzivate
    freg:boolean;                                                               // flag urcijici moznost autorizovat regulator
    fid:string;                                                                 // unikatni id
    fsalt:string;                                                               // sul hesla

      procedure SetPasswd(passwd:string);                                       // nastavi heslo, viz \passwd
      procedure SetBan(state:boolean);                                          // nastavi ban, viz \ban
      procedure SetReg(state:boolean);                                          // nastavi regulator, viz \regulator

      procedure SetId(new:string);

      function GetFullName():string;                                            // varti jsmeno uzivatele ve formatu: first_name [mezera] lastname
      class function GenSalt():string;

   public

    firstname:string;                                                           // krestni jmeno
    lastname:string;                                                            // prijmeni
    root:boolean;                                                               // flag opravneni root
    OblR: TDictionary<string, TORControlRights>;                                // seznam oblasti rizeni vcetne opravneni
    lastlogin:TDateTime;                                                        // cas posledniho loginu

    class var comparer: TComparison<TUser>;

      constructor Create(); overload;
      constructor Create(iniData, iniStat:TMemIniFile; section:string); overload;
      destructor Destroy(); override;

      procedure LoadData(ini:TMemIniFile; section:string);
      procedure LoadStat(ini:TMemIniFile; section:string);
      procedure SaveData(ini:TMemIniFile; section:string);
      procedure SaveStat(ini:TMemIniFile; section:string);

      function GetRights(OblR:string):TORCOntrolRights;                         // vrati opravneni k dane oblasti rizeni
      procedure SetRights(OblR:string; rights:TORControlRights);                // nastavi opravneni dane oblasti rizeni

      property id:string read fid write SetId;
      property password:string read fpasswd write SetPasswd;
      property ban:boolean read fban write SetBan;
      property regulator:boolean read freg write SetReg;
      property fullName:string read GetFullName;
      property salt:string read fsalt;
      property login:string read fid write SetId;

      class function ComparePasswd(plain:string; hash:string; salt:string):boolean; // kontroluje shodu hesel; true poku hesla sedi, jinak false
      class function GenerateHash(plain:AnsiString):string;                     // generuje hash hesla
  end;//class TUser

implementation

uses TOblsRizeni, TCPServerOR, UserDb;

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

 Self.fid       := section;
 Self.fpasswd   := ini.ReadString(section, 'passwd', '');
 Self.root      := ini.ReadBool(section, 'root', false);
 Self.firstname := ini.ReadString(section, 'fname', '');
 Self.lastname  := ini.ReadString(section, 'lname', '');
 Self.fsalt     := ini.ReadString(section, 'salt', '');
 Self.fban      := ini.ReadBool(section, 'ban', false);
 Self.freg      := ini.ReadBool(section, 'reg', false);

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
end;//procedure

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
var i:Integer;
    rights:TORControlRights;
    str:string;
begin
 ini.WriteString(section, 'passwd', Self.fpasswd);

 if (Self.root) then
   ini.WriteBool(section, 'root', Self.root);

 if (Self.firstname <> '') then
   ini.WriteString(section, 'fname', Self.firstname);

 if (Self.lastname <> '') then
   ini.WriteString(section, 'lname', Self.lastname);

 if (Self.ban) then
   ini.WriteBool(section, 'ban', Self.ban);

 if (Self.regulator) then
   ini.WriteBool(section, 'reg', Self.regulator);

 if (self.salt <> '') then
   ini.WriteString(section, 'salt', Self.fsalt);

 str := '';
 for i := 0 to ORs.Count-1 do
   if (Self.OblR.TryGetValue(ORs.GetORIdByIndex(i), rights)) then
     str := str + '(' + ORs.GetORIdByIndex(i) + ';' + IntToStr(Integer(rights)) + ')';

 if (str <> '') then
   ini.WriteString(section, 'ORs', str);
end;//procedure

procedure TUser.SaveStat(ini:TMemIniFile; section:string);
begin
 ini.WriteString(section, 'lastlogin', DateTimeToStr(Self.lastlogin));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetPasswd(passwd:string);
begin
 // heslo je 2x zahashovane
 Self.fsalt := Self.GenSalt();
 Self.fpasswd := TUser.GenerateHash(AnsiString(TUser.GenerateHash(AnsiString(passwd)) + self.fsalt));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TUser.GetRights(OblR:string):TORCOntrolRights;
var rights:TORControlRights;
begin
 if (Self.root) then Exit(TORCOntrolRights.superuser);

 if (Self.OblR.TryGetValue(OblR, rights)) then
  Result := rights
 else
  Result := null;
end;//function

////////////////////////////////////////////////////////////////////////////////

class function TUser.ComparePasswd(plain:string; hash:string; salt:string):boolean;
begin
 Result := (hash = TUser.GenerateHash(AnsiString(LowerCase(plain + salt))));
end;//function

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
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetRights(OblR:string; rights:TORControlRights);
var OblRRef:TOR;
begin
 Self.OblR.AddOrSetValue(OblR, rights);
 ORs.GetORByIndex(ORs.GetORIndex(OblR), OblRRef);
 if (OblRRef <> nil) then OblRRef.UserUpdateRights(Self);
end;//procedure

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
     ORs.GetORByIndex(ORs.GetORIndex(oblr), OblRRef);
     if (OblRRef <> nil) then OblRRef.UserUpdateRights(Self);
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetReg(state:boolean);
begin
 if (Self.freg = state) then Exit();
 Self.freg := state;
 if (not Self.freg) then ORTCPServer.DisconnectRegulatorUser(self);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetId(new:string);
begin
 if (Self.id <> new) then
  begin
   Self.fid := new;
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

initialization
 TUser.comparer :=
    function (const Left, Right: TUser): Integer
      begin
        Result := AnsiCompareStr(Left.id, Right.id);
      end;

finalization

end.//unit
