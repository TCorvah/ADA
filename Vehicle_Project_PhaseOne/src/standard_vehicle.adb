with Ada.Text_IO; use Ada.Text_IO;
with Sensor_System, Vehicle_System;
use Vehicle_System;

package body Standard_Vehicle is 


   procedure SeatBelt_Warning(V : in out  Standard) is
   begin
      Vehicle_System.Start_Engine(Vehicle_System.Vehicle(V));
      Sensor_System.Activate_Sensor(V.Car_Sensor);
      Sensor_System.Check_Seat(V.Car_Sensor);
      if Sensor_System.Is_Door_Open(V.Car_Sensor) then
         Put_Line(" Door is open");
      else
         Put_Line(" Door is closed");
      end if;
      if Sensor_System.Seat_Occupied (V.Car_Sensor) and then not V.Car_Sensor.Seatbelt_On then 
         Put_Line(" seatbelt is not on");
      end if;  
   end SeatBelt_Warning;

   procedure Check_Sensors(V : in out Standard) is   
   begin
      Sensor_System.Activate_Sensor(V.Car_Sensor);
      if Sensor_System.Is_Door_Open(V.Car_Sensor) then
         Put_Line(" Door is open");
      else
         Put_Line(" Door is closed");
      end if;
   end Check_Sensors;

   function Vehicle_Mobile(V : in out Standard) return Boolean is
   begin
     return V.Engine_On and V.Speed > 0.0;
   end Vehicle_Mobile;

   function is_Door_Closed(V : in out Standard) return Boolean is
   begin
      return V.Current_Door_Status = Vehicle_System.Door_Closed;
   end is_Door_Closed;
   
   


end Standard_Vehicle;