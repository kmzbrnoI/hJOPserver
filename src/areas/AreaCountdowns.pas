unit AreaCountdowns;

interface

uses Classes, Generics.Collections, SysUtils;

type
  TAreaCountdown = record
    start: TDateTime;
    duration: TTime;
    callback: TNotifyEvent;
  end;

  TAreaCountdowns = class
  private
    m_countdowns: TDictionary<Integer, TAreaCountdown>;
    m_next_countdown_id: Integer;
    m_parent: TObject; // actually TArea

  public
    constructor Create(parent: TObject);
    destructor Destroy(); override;

    function Add(callback: TNotifyEvent; len: TDateTime): Integer; // returns id of added countdown
    procedure Remove(id: Integer);
    function IsCd(id: Integer): Boolean;
    function RemainingTime(id: Integer): TTime;

    procedure Update();

  end;

implementation

uses Area;

////////////////////////////////////////////////////////////////////////////////

constructor TAreaCountdowns.Create(parent: TObject);
begin
  inherited Create();

  Self.m_parent := parent;
  Self.m_countdowns := TDictionary<Integer, TAreaCountdown>.Create();
  Self.m_next_countdown_id := 0;
end;

destructor TAreaCountdowns.Destroy();
begin
  Self.m_countdowns.Free();
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TAreaCountdowns.Add(callback: TNotifyEvent; len: TDateTime): Integer;
begin
  var id := Self.m_next_countdown_id;

  (Self.m_parent as TArea).BroadcastData('CAS;START;' + IntToStr(id) + ';' + FormatDateTime('s', len) + ';');

  var mc: TAreaCountdown;
  mc.start := Now;
  mc.duration := len;
  mc.callback := callback;
  Self.m_countdowns.Add(id, mc);

  Result := id;
  Inc(Self.m_next_countdown_id); // Inc ignores overflows
  if (Self.m_next_countdown_id < 0) then // do not use negative number, let '-1' as 'unused timer'
    Self.m_next_countdown_id := 0;
end;

procedure TAreaCountdowns.Remove(id: Integer);
begin
  if (not Self.m_countdowns.ContainsKey(id)) then
    Exit();

  Self.m_countdowns.Remove(id);
  (Self.m_parent as TArea).BroadcastData('CAS;STOP;' + IntToStr(id) + ';');
end;

function TAreaCountdowns.IsCd(id: Integer): Boolean;
begin
  Result := Self.m_countdowns.ContainsKey(id);
end;

function TAreaCountdowns.RemainingTime(id: Integer): TTime;
begin
  if (not Self.m_countdowns.ContainsKey(id)) then
    Exit();

  var cd: TAreaCountdown := Self.m_countdowns[id];
  Result := cd.start+cd.duration-Now;
end;

procedure TAreaCountdowns.Update();
begin
  var toRemove := TList<Integer>.Create();
  try
    for var id: Integer in Self.m_countdowns.Keys do
    begin
      var countdown := Self.m_countdowns[id];
      if (Now >= (countdown.start + countdown.duration)) then
        toRemove.Add(id);
    end;

    for var idToRemove: Integer in toRemove do
    begin
      var callback: TNotifyEvent := Self.m_countdowns[idToRemove].callback;
      Self.m_countdowns.Remove(idToRemove);
      if (Assigned(callback)) then
        callback(Self);
    end;
  finally
    toRemove.Free();
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
