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
var i: Integer;
begin
 gauge := TGauge.Create(F_Main.SB1);
 gauge.Parent := F_Main.SB1;
 gauge.Visible := true;
 gauge.Left := 0;
 for i := 0 to _SB_PROC-1 do
   gauge.Left := gauge.Left+F_Main.SB1.Panels.Items[i].Width;
 gauge.Left := gauge.Left + 30;
 gauge.Top := 3;
 gauge.Height := 16;
 gauge.Width := F_Main.SB1.Panels.Items[_SB_PROC].Width-30;
 gauge.Color := clWhite;
 gauge.ForeColor := clLime;
end;

procedure TCpuLoad.ResizeCPUGauge();
var i, left: Integer;
begin
 gauge.Parent := F_Main.SB1;
 left := 0;
 for i := 0 to _SB_PROC-1 do
   left := left + F_Main.SB1.Panels.Items[i].Width;
 left := left + 30;
 gauge.Left := left;
end;

end.
