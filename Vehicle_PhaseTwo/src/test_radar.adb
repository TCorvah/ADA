with Ada.Text_IO; use Ada.Text_IO;
with Radar_Systems; use Radar_Systems;
with Road_ProfileConfig; use Road_ProfileConfig;


procedure Test_Radar is
begin
   Put_Line("Starting Radar Scan Simulation...");
   Put_Line ("[INFO] Starting radar test for garage environment...");
   Radar_Systems.Radar_Scan_Garage_Simulation(Road_ProfileConfig.Garage_Profile);
   Put_Line ("[INFO] Test complete.");
   Put_Line("Radar Scan Simulation Completed.");
end Test_Radar;





