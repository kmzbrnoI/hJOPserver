unit PTEndpointUser;

{ PTserver endpoints /users/id, /users/id/auth. }

interface

uses IdContext, IdCustomHTTPServer, JsonDataObjects, PTEndpoint, SysUtils,
  Generics.Collections;

type
  TPTEndpointUser = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/users/[^/]*/?$';

    public
      procedure OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;

  end;

  TPTEndpointUserAuth = class(TPTEndpoint)
    private const
      _ENDPOINT_MATCH_REGEX = '^/users/.*?/auth$';

    public
      procedure OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject); override;

      function EndpointMatch(path:string):Boolean; override;
      function AuthRequired(cmdType: THTTPCommandType): Boolean; override;

  end;

implementation

uses PTUtils, StrUtils, ownStrUtils, UserDb, User;

////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointUser.OnGET(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject);
var urlSuffix, username: string;
    user: TUser;
begin
 urlSuffix := RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/users/'));
 username := strTillChar(urlSuffix, '/');
 user := UsrDB.GetUser(username);

 if (user = nil) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404',
      'Uzivatel neexistuje', 'Uzivatel '+username+' neexistuje');
   Exit();
  end;

 user.GetPtData(respJson.O['user']);
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointUser.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TPTEndpointUserAuth.OnPOST(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
        var respJson:TJsonObject; const reqJson:TJsonObject);
var urlSuffix, username: string;
    user: TUser;
begin
 urlSuffix := RightStr(ARequestInfo.Document, Length(ARequestInfo.Document)-Length('/users/'));
 username := strTillChar(urlSuffix, '/');
 user := UsrDB.GetUser(username);

 if (user = nil) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '404',
      'Uzivatel neexistuje', 'Uzivatel '+username+' neexistuje');
   Exit();
  end;

 if (not reqJson.Contains('passwordHash')) then
  begin
   PTUtils.PtErrorToJson(respJson.A['errors'].AddObject, '400', 'Chybi polozka "passwordHash"');
   Exit();
  end;

 respJson['success'] := user.PasswordMatch(reqJson['passwordHash']);
end;

////////////////////////////////////////////////////////////////////////////////

function TPTEndpointUserAuth.EndpointMatch(path:string):Boolean;
begin
 Result := TPTEndpoint.PatternMatch(path, _ENDPOINT_MATCH_REGEX);
end;

function TPTEndpointUserAuth.AuthRequired(cmdType: THTTPCommandType): Boolean;
begin
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

end.

