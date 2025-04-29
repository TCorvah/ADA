with Ada.Text_IO; use Ada.Text_IO;
with Vehicle_System; use Vehicle_System;
with Radar_Systems; use Radar_Systems;
with Sensor_System; use Sensor_System;
with Luxury_Vehicle; use Luxury_Vehicle;
with Standard_Vehicle; use Standard_Vehicle;
with Vehicle_Constants; use Vehicle_Constants;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package System_Interface is

   -- Procedure to run the system interface
   procedure Run_System_Interface( Vehicles : in out Luxury_Vehicle.Luxury_Car);

end System_Interface;



