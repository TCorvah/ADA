with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Standard_Vehicle, Sensor_System, Radar_Systems;
with System_Interface;



procedure Main is

   L : Luxury_Vehicle.Luxury_Car;
   S : Standard_Vehicle.Standard;

begin
   Put_Line("Vehicle Project - Phase One");
   Put_Line("=====================================");
   Put_Line("Luxury Vehicle Simulation");
   Put_Line("=====================================");
   Put_Line("Initializing vehicle systems...");
   System_Interface.Run_System_Interface(L);
      


end Main;
