with Ada.Text_IO; use Ada.Text_IO;
with Vehicle_Constants;
use Vehicle_Constants;
with Vehicle_System;
use Vehicle_System;

package body Sensor_System is
   
   procedure Activate_Sensor(S : in out Sensor) is
   begin
      S.Status := Sensor_System.On;
      Put_Line ("Sensor: Activated");
   end Activate_Sensor;

   procedure Deactivate_Sensor(S : in out Sensor) is
   begin
      S.Status := Sensor_System.Off;
      Put_Line ("Sensor: Deactivated");
   end Deactivate_Sensor;

   function Is_Door_Open(S : in Sensor) return  Boolean is
   begin
      return S.Door_Open;
   end Is_Door_Open;

   function Seat_Occupied(S : Sensor) return Boolean is
   begin
      return S.Detected_Weight > 20.0; -- payload threshold for seatbelt warning is 20kg
   end Seat_Occupied;

   procedure Toggle_Door(S : in out Sensor) is
   begin
      S.Door_Open := not S.Door_Open;
      if S.Door_Open then
         Put_Line ("Vehicle door is open" );
      else
         Put_Line ("Vehicle door is now close");
      end if;
   end Toggle_Door;

   
   procedure Check_Seatbelt(S : in Sensor) is 
   begin
      if S.Seatbelt_On then
         Put_Line ("Sensor: Seatbelt is fastened");
      else
          Put_Line ("Sensor: Seatbelt is not  fastened");
      end if;
   end Check_Seatbelt;


   procedure Check_Seat(S : in Sensor) is
   begin
      if Seat_Occupied (S) then
         Put_Line ("Sensor: Seat is occupied (weight: " & Float 'Image(S.Detected_Weight) & "kg).");
         Check_Seatbelt (S);
      else
         Put_Line ("Sensor: Seat is empty.");
      end if;
   end Check_Seat;

   procedure Check_Visibility(S : in out Sensor) is
   begin
      if S.Visibility < 0.5 then 
         S.Headlights_On := True;
         Put_Line("Sensor: Headlights turned ON, Time of Day is night.");
      else
         S.Headlights_On := False;
         Put_Line("Sensor: Headlights OFF, daylight detected.");
      end if;
   end Check_Visibility;

   function Should_Turn_On_Headlights(S : in Sensor) return Boolean is
   begin
      return S.Visibility < 0.5;
   end Should_Turn_On_Headlights;


   procedure Update_Headlights(S : in out Sensor) is 
   begin
      if Should_Turn_On_Headlights(S) then
         S.Headlights_On := True;
         Put_Line("Sensor: Headlights turned ON due to low light.");
      else
         S.Headlights_On := False;
         Put_Line("Headlights turn Off; visibility is suficient.");
      end if;
   end Update_Headlights;

   procedure Handle_Object_Detection(V : in out Vehicle_System.Vehicle; Current_Speed : in Float) is
   -- Example threshold for object detection
   begin
      if V.Engine_On and Current_Speed > Vehicle_Constants.Threshold then
         V.Set_Speed(V.Speed - Vehicle_Constants.Threshold); -- Example: decrease speed if object detected
      -- Placeholder for object detection logic
         Put_Line("Sensor: Object detected while vehicle is moving, SLOWING DOWN.");
      else
         Put_Line("Sensor: Deaccelerating Vehicle.");
         V.Is_Moving := False; -- Example: stop the vehicle if object detected
         V.Set_Speed(0.0); -- Example: stop the vehicle if object detected
      end if;

      -- Implement logic to handle object detection
   end Handle_Object_Detection;


end Sensor_System;