unit CpuLoad;

interface

uses Gauges, Graphics, adCpuUsage, Windows;

type
 TCpuLoad = class
  Gauge:TGauge;
  GraphPos:Integer;
  LPa,LPb,LPc:Int64;

   constructor Create();
   procedure Refresh();
   procedure DrawCPUGauge();
   procedure ResizeCPUGauge();
 end;

implementation

uses fMain;

constructor TCpuLoad.Create();
begin
 inherited;
 QueryPerformanceFrequency(Self.LPc);
end;

procedure TCpuLoad.Refresh();
begin
 CollectCPUData();
 Self.Gauge.Progress := Round(GetCPUUsage(GetCPUCount-1)*100);
end;

procedure TCpuLoad.DrawCPUGauge();
var cyklus:Integer;
begin
 Gauge := TGauge.Create(F_Main.SB1);
 Gauge.Parent := F_Main.SB1;
 Gauge.Visible := true;
 Gauge.Left := 0;
 for cyklus := 0 to _SB_PROC-1 do
  begin
   Gauge.Left := Gauge.Left+F_Main.SB1.Panels.Items[cyklus].Width;
  end;//for cyklus
 Gauge.Left := Gauge.Left + 30;
 Gauge.Top := 3;
 Gauge.Height := 16;
 Gauge.Width := F_Main.SB1.Panels.Items[_SB_PROC].Width-30;
 Gauge.Color := clWhite;
 Gauge.ForeColor := clLime;
end;

procedure TCpuLoad.ResizeCPUGauge();
var cyklus,Zleva:Integer;
begin
 Gauge.Parent  := F_Main.SB1;
 Zleva := 0;
 for cyklus := 0 to _SB_PROC-1 do
  begin
   Zleva := Zleva + F_Main.SB1.Panels.Items[cyklus].Width;
  end;//for cyklus
 Zleva := Zleva + 30;
 Gauge.Left := Zleva;
end;

end.
