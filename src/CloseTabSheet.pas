unit CloseTabSheet;

interface

uses Controls, Graphics, Types, ComCtrls, Classes;

type

  TCloseTabSheet=class(TTabSheet)
    private
    protected
    public
      FCloseButtonRect: TRect;
      FOnClose: TNotifyEvent;
      procedure DoClose; virtual;

      constructor Create(AOwner:TComponent); override;
      destructor Destroy; override;
  property OnClose:TNotifyEvent read FOnClose write FOnClose;
  end;// class TCloseTabSheet


implementation

constructor TCloseTabSheet.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FCloseButtonRect := Rect(0, 0, 0, 0);
end;

destructor TCloseTabSheet.Destroy;
begin
  inherited Destroy;
end;

procedure TCloseTabSheet.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

end.//unit
