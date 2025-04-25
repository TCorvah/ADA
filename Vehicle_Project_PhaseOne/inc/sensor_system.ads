with Vehicle_System;
use Vehicle_System;
package Sensor_System is

   type Sensor_Status is (Off, On);

-- Sensor has two states and will be use for standard and luxury cars
   type Sensor is tagged record
      Status : Sensor_Status := Off;
      Door_Open : Boolean := False;
      Detected_Weight : Float := 0.0;
      Seatbelt_On : Boolean := False;
      Visibility : Float := 1.0; -- from 0.0 (dark) to 1.0 (bright)
      Headlights_On : Boolean := False;
   end record;


   -- Procedure 
   procedure Activate_Sensor(S : in out Sensor);
   procedure Deactivate_Sensor(S : in out Sensor);
   function  Is_Door_Open (S :in Sensor) return  Boolean;
   function Seat_Occupied(S : in Sensor) return Boolean;
   function Should_Turn_On_Headlights(S : in Sensor) return Boolean;
   procedure Toggle_Door(S : in out Sensor);
   procedure Check_Seatbelt(S : Sensor);
   procedure Check_Seat(S : Sensor);
   procedure Check_Visibility(S : in out Sensor);
   procedure Update_Headlights(S : in out Sensor);
   procedure Handle_Object_Detection(V : in out Vehicle_System.Vehicle; Current_Speed : in Float);

end Sensor_System;
