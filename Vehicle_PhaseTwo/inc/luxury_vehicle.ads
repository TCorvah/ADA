with Vehicle_Constants; use Vehicle_Constants;
with Radar_Systems, Sensor_System, Vehicle_System, Vehicle_Types;
with Road_ProfileConfig; use Road_ProfileConfig;

-- This package defines the Luxury Vehicle type and its associated procedures.
-- It extends the base Vehicle class and includes additional features such as
-- radar and sensor systems for enhanced functionality.
-- The package provides procedures for updating door status, enabling object detection,
-- reducing speed based on object distance, and checking if the door is closed.
-- It also includes a procedure to attempt moving the vehicle and a function to turn off the engine.
-- The Luxury Vehicle type is defined as a record that includes the base vehicle attributes
-- along with additional attributes specific to luxury vehicles, such as cost and miles per gallon.
-- The package is designed to be used in conjunction with the Vehicle_System and Vehicle_Types packages.
package Luxury_Vehicle is


   type Luxury_Car is new Vehicle_System.Vehicle with record
      Car_Sensor : Sensor_System.Sensor;
      Car_Radar : Radar_Systems.Radar;
      luxury_cost : Float := Vehicle_Constants.Luxury_Rental_Cost;
      miles_gallon : Float := Vehicle_Constants.Luxury_MPG;
      Lux_Model : Vehicle_Types.Vehicle_Type := Vehicle_Types.Luxury_cars;
      Min_Speed : Float := Vehicle_Constants.Vehicle_Min_Speed; -- Speed of the luxury car
      Max_Speed : Float := Vehicle_Constants.Vehicle_Max_Speed; -- Maximum speed of the luxury car
   end record;
   ------------------------------------------------------------
   -- Procedure : Update_Door_Status
   -- Purpose  : Gives a status if doors are open or close 
   -- Input : Uses internal state of sensor
   -- Output : status of doors
   ------------------------------------------------------------
   procedure Update_Door_Status(V : in out Luxury_Car);


   --------------------------------------------------------------
   -- Procedure : Enable_Object_Detection
   -- Purpose  : uses radar to detect object in certain range
   -- Input : Uses internal state of threshold detection
   -- Output : alert driver of object in view of certain range
   ------------------------------------------------------------
   --procedure Enable_Object_Detection(V : in out Luxury_Car);

   


   --------------------------------------------------------------
   -- function : is_Door_Closed
   -- Purpose  : checks if the door is closed
   -- Input : Uses internal state of sensor
   -- Output : status of doors
   overriding function is_Door_Closed(V : in out Luxury_Car) return Boolean;

   --------------------------------------------------------------
   -- procedure : Attempt_Move
   -- Purpose  : checks if the car is in motion
   -- Input : Uses internal state of sensor
   -- Output : 
   overriding procedure Attempt_Move(V : in out Luxury_Car; Threshold : in Float);


   --------------------------------------------------------------
   -- function : Vehicle_Mobile
   -- Purpose  : checks if the car is in motion
   -- Input : Uses internal state of sensor
   -- Output : status of doors
   overriding function Vehicle_Mobile(V : in out Luxury_Car; Path_Clear : Boolean) return Boolean;

   --overriding function Get_Door_Status(V : Vehicle_System.Vehicle) return Vehicle_System.Door_Status_Type;


   --------------------------------------------------------------
   -- procedure : Turn_Off_Engine
   -- Purpose  : checks if the car is in motion
   -- Input : Uses internal state of sensor
   -- Output : status of door
   procedure Turn_Off_Engine(V : in out Luxury_Car);

   

   

end Luxury_Vehicle;