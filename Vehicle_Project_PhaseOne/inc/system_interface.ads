with Vehicle_System; use Vehicle_System;
with Radar_Systems; use Radar_Systems;
with Sensor_System; use Sensor_System;
with Luxury_Vehicle; use Luxury_Vehicle;
with Standard_Vehicle; use Standard_Vehicle;
with Vehicle_Constants; use Vehicle_Constants;
with Vehicle_Types; use Vehicle_Types;

package System_Interface is

   -- This package provides the system interface for the vehicle project.
   -- It includes procedures for running the luxury vehicle interface, and 
   -- the standard vehicle interface, and activating the time of day.
   -- It also includes a procedure for running the system interface.
   -- The package uses Ada.Text_IO for input and output operations.
   -- The package is designed to be used in the main program of the vehicle project.
   
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



