with Ada.Text_IO; use Ada.Text_IO;
with Radar_Systems;

procedure Test_Radar_Scan is
begin
   Put_Line("Starting Radar Scan Simulation...");
   Radar_Systems.Radar_Scan_Simulation;
   Put_Line("Radar Scan Simulation Completed.");
end Test_Radar_Scan;
