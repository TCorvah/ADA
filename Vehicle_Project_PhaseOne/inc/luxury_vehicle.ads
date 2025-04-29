with Radar_Systems, Sensor_System, Vehicle_System;


package Luxury_Vehicle is


   -- Luxury Vehicle Type (Extends the base class)
   type Luxury_Car is new Vehicle_System.Vehicle with record
      Car_Sensor : Sensor_System.Sensor;
      Car_Radar : Radar_Systems.Radar;
   end record;


   use Luxury_Vehicle;
   ------------------------------------------------------------
   -- Procedure : Update_Door_Status
   -- Purpose  : Gives a status if doors are open or close 
   -- Input : Uses internal state of sensor
   -- Output : status of doors
   ------------------------------------------------------------
   procedure Update_Door_Status(Lux_Car: in out Luxury_Car);


   --------------------------------------------------------------
   -- Procedure : Enable_Object_Detection
   -- Purpose  : uses radar to detect object in certain range
   -- Input : Uses internal state of threshold detection
   -- Output : alert driver of object in view of certain range
   ------------------------------------------------------------
   procedure Enable_Object_Detection(Lux_Car : in out Luxury_Car; Object_Threshold : in Float);

   
   --------------------------------------------------------------------------------
   -- function : Reduce_Speed
   -- Purpose  : adjust vehicle speed based on object distance from safe distance
   -- Input : takes in the current speed and compare with threshold for detection
   -- Output : alert driver of object in view of certain range
   ---------------------------------------------------------------------------------
   procedure Reduce_Speed(Lux_Car : in out Luxury_Car; Current_Speed : in Float);

   function is_Door_Closed(Lux_Car : in out Luxury_Car) return Boolean;
   procedure Attempt_Move(Lux_Car : in out Luxury_Car; Threshold : in Float);

   overriding function Vehicle_Mobile(Lux_Car : in out Luxury_Car) return Boolean;

   procedure Turn_Off_Engine(Lux_Car : in out Luxury_Car);


end Luxury_Vehicle;