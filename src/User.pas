unit User;

interface

uses IniFiles, Generics.Collections, RPConst, DCPsha256, SysUtils, Classes;

type
  TUser = class
   private
    fpasswd:string;
    fban:boolean;
    freg:boolean;

      procedure SetPasswd(passwd:string);
      procedure SetBan(state:boolean);
      procedure SetReg(state:boolean);

   public

    id:string;
    firstname:string;
    lastname:string;
    root:boolean;
    OblR: TDictionary<string, TORControlRights>;
    lastlogin:TDateTime;


      constructor Create(); overload;
      constructor Create(ini:TMemIniFile; section:string); overload;
      destructor Destroy(); override;

      procedure LoadData(ini:TMemIniFile; section:string);
      procedure SaveData(ini:TMemIniFile; section:string);
      procedure SaveStat(ini:TMemIniFile; section:string);

      function GetRights(OblR:string):TORCOntrolRights;
      procedure SetRights(OblR:string; rights:TORControlRights);

      property password:string read fpasswd write SetPasswd;
      property ban:boolean read fban write SetBan;
      property regulator:boolean read freg write SetReg;

      class function ComparePasswd(plain:string; hash:string):boolean;      // true if ok, false if not same
      class function GenerateHash(plain:string):string;
  end;//class TUser

implementation

uses TOblsRizeni, TOblRizeni;

////////////////////////////////////////////////////////////////////////////////

constructor TUser.Create(ini:TMemIniFile; section:string);
begin
 inherited Create();
 Self.OblR := TDictionary<string, TORControlRights>.Create();
 Self.LoadData(ini, section);
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

 Self.id        := section;
 Self.fpasswd   := ini.ReadString(section, 'passwd', '');
 Self.root      := ini.ReadBool(section, 'root', false);
 Self.firstname := ini.ReadString(section, 'fname', '');
 Self.lastname  := ini.ReadString(section, 'lname', '');
 Self.lastlogin := StrToDateTime(ini.ReadString(section, 'lastlogin', ''));
 Self.fban      := ini.ReadBool(section, 'ban', false);
 Self.freg      := ini.ReadBool(section, 'reg', false);

 data := TStringList.Create();
 ExtractStrings(['(', ')', ',', ';'], [], PChar(ini.ReadString(section, 'ORs', '')), data);

 for i := 0 to (data.Count div 2)-1 do
  begin
   try
    Self.OblR.Add(data[i*2], TORControlRights(StrToInt(data[i*2 + 1])));
   except

   end;
  end;//for i

 data.Free();
end;//procedure

procedure TUser.SaveData(ini:TMemIniFile; section:string);
var i:Integer;
    rights:TORControlRights;
    str:string;
begin
 Self.SaveStat(ini, section);

 ini.WriteString(section, 'passwd', Self.fpasswd);
 ini.WriteBool(section, 'root', Self.root);
 ini.WriteString(section, 'fname', Self.firstname);
 ini.WriteString(section, 'lname', Self.lastname);
 ini.WriteBool(section, 'ban', Self.ban);
 ini.WriteBool(section, 'reg', Self.regulator);

 str := '';
 for i := 0 to ORs.ORcnt-1 do
   if (Self.OblR.TryGetValue(ORs.GetORIdByIndex(i), rights)) then
     str := str + '(' + ORs.GetORIdByIndex(i) + ';' + IntToStr(Integer(rights)) + ')';

 ini.WriteString(section, 'ORs', str);
end;//procedure

procedure TUser.SaveStat(ini:TMemIniFile; section:string);
begin
 ini.WriteString(section, 'lastlogin', DateTimeToStr(Self.lastlogin));
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TUser.SetPasswd(passwd:string);
begin
 Self.fpasswd := TUser.GenerateHash(passwd);
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

class function TUser.ComparePasswd(plain:string; hash:string):boolean;
begin
 if (hash = TUser.GenerateHash(plain)) then
  Result := true
 else
  Result := false;
end;//function

class function TUser.GenerateHash(plain:string):string;
var hash: TDCP_sha256;
    Digest: array[0..31] of byte;  // RipeMD-160 produces a 160bit digest (20bytes)
    i:Integer;
begin
 hash := TDCP_sha256.Create(nil);
 hash.Init();
 hash.UpdateStr(plain);
 hash.Final(Digest);
 hash.Free();

 Result := '';
 for i:= 0 to 31 do
   Result := Result + IntToHex(Digest[i],2);
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
 if (Self.freg) then
  begin
   // odpojit vsechny regulatory uzivatele
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit
