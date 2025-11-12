unit CpuLoad;

interface

uses Gauges, Graphics, Windows, Classes;

type
  TCpuLoad = class
    gauge: TGauge;
    graphPos: Integer;
    prevTime: TThread.TSystemTimes;

    procedure RefreshCPUGauge();
    procedure CreateCPUGauge();
    procedure ResizeCPUGauge();
  end;

implementation

uses fMain;

procedure TCpuLoad.RefreshCPUGauge();
begin
  Self.gauge.Progress := TThread.GetCPUUsage(Self.prevTime);
end;

procedure TCpuLoad.CreateCPUGauge();
begin
  gauge := TGauge.Create(F_Main.SB1);
  gauge.Parent := F_Main.SB1;
  gauge.Visible := true;
  gauge.Left := 0;
  for var i: Integer := 0 to F_Main._SB_PROC - 1 do
    gauge.Left := gauge.Left + F_Main.SB1.Panels.Items[i].Width;
  gauge.Left := gauge.Left + 30;
  gauge.Top := 3;
  gauge.Height := 16;
  gauge.Width := F_Main.SB1.Panels.Items[F_Main._SB_PROC].Width - 30;
  gauge.Color := clWhite;
  gauge.ForeColor := clLime;
end;

procedure TCpuLoad.ResizeCPUGauge();
begin
  gauge.Parent := F_Main.SB1;
  var Left: Integer := 0;
    for var i: Integer := 0 to F_Main._SB_PROC - 1 do Left := Left + F_Main.SB1.Panels.Items[i].Width;
  Left := Left + 30;
  gauge.Left := Left;
end;

end.
