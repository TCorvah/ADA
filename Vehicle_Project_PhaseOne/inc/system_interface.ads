with Ada.Text_IO; use Ada.Text_IO;
with Vehicle_System; use Vehicle_System;
with Radar_Systems; use Radar_Systems;
with Sensor_System; use Sensor_System;
with Luxury_Vehicle; use Luxury_Vehicle;
with Standard_Vehicle; use Standard_Vehicle;
with Vehicle_Constants; use Vehicle_Constants;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Vehicle_Types; use Vehicle_Types;

package System_Interface is

   -- Declare the vehicle type
   -- Luxury vehicle type
   Selected_Vehicle : Vehicle_Types.Vehicle_Type;


   -- Procedure to run the system interface
   procedure Run_System_Interface( Selected_Type : in out Vehicle_Types.Vehicle_Type);
   -- Procedure to run the luxury vehicle interface
   procedure Run_Luxury_Scenario( Vehicles : in out Luxury_Vehicle.Luxury_Car);
   -- Procedure to run the standard vehicle interface    
   procedure Run_Standard_Scenario( Vehicles : in out Standard_Vehicle.Standard);

   -- Procedure to activate the time of day
   procedure Activate_TOD(Time : in out Sensor_System.Sensor);

end System_Interface;



