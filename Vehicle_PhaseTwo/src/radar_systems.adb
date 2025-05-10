with Ada.Text_IO; use Ada.Text_IO;
with Radar_Systems;

package body Radar_Systems is
   -- This procedure is used to activate the radar system 
   -- and set its status to "On".
   -- It also prints a message indicating that the radar has been activated.
   -- The procedure takes a parameter of type Radar, which is passed by reference.
   -- The procedure does not return any value.
   procedure Activate_Radar(R : in out Radar) is 
   begin
      R.Status := Radar_Systems.On;
      Put_Line ("Radar: Activated");
   end Activate_Radar;

   -- This procedure is used to deactivate the radar system
   -- and set its status to "Off".
   -- It also prints a message indicating that the radar has been deactivated.
   -- The procedure takes a parameter of type Radar, which is passed by reference.
   -- The procedure does not return any value.
   procedure Deactivate_Radar(R : in out Radar) is
   begin
      R.Status := Radar_Systems.Off;
      Put_Line ("Radar: Deactivated");
   end Deactivate_Radar;

   -- This procedure is used to detect objects using the radar system.
   -- It checks the distance of the object and determines if it is within
   -- the detection range.
   -- If the object is too close, it prints a message to stop the vehicle.
   -- If the object is within the detection range, it prints a message to slow down.
   -- If no object is detected, it prints a message to drive safely.
   -- The procedure takes a parameter of type Radar, which is passed by reference.
   -- The procedure does not return any value.
   -- The procedure also takes a threshold parameter to determine the detection range
   procedure Detect_Object(Radar_Data : in out Radar; Threshold : Float) is
   begin
      Activate_Radar(Radar_Data);
      if Radar_Data.Object_Distance <= Min_Detection_Range then
         Put_Line ("Radar: Object very close : (" & Float'Image(Radar_Data.Object_Distance) & " ft). STOP Vehicle");
         Radar_Data.Object_In_Motion := False;
      elsif Radar_Data.Object_Distance <= Max_Detection_Range and
         Radar_Data.Object_Distance > Min_Detection_Range  then
         Put_Line ("Radar: Object detected at distance: (" & Float'Image(Radar_Data.Object_Distance) & " ft). SLOW DOWN");
      else
         Put_Line ("No close object, drive safely");
         Radar_Data.Object_In_Motion := True;
         Radar_Data.Object_Detected := False;
      end if;
   end Detect_Object;

   -- This function is used to adjust the speed of the vehicle based on the radar data.
   -- It takes the radar data and the current speed as parameters.
   -- It checks the distance of the object detected by the radar and adjusts the speed accordingly.
   -- If the object is too close, it stops the vehicle.
   -- If the object is within a certain distance, it reduces the speed.
   -- If no object is detected, it maintains the current speed.
   -- The function returns the adjusted speed.
   -- The function takes a parameter of type Radar, which is passed by reference.
   -- The function also takes a current speed parameter to determine the current speed of the vehicle.
   -- The function returns an integer value representing the adjusted speed.
   -- The function also takes a threshold parameter to determine the detection range
   function Adjust_Speed(Radar_Data : in Radar; Current_Speed : in Integer) return Integer is
      Safe_Distance : constant Float := 2.0;
      Reduce_Speed  : constant Integer := 10;
      Stopped_Speed : constant Integer := 0;
   begin
      if Radar_Data.Object_Distance < Safe_Distance then
         Put_Line("Object too close ahead! Stopping Vehicle");
         return Stopped_Speed;
      elsif Radar_Data.Object_Distance < (Safe_Distance * 2.0) then
         Put_Line("Object detected ahead. Reducing speed.");
         return Reduce_Speed;
      else
         Put_Line("No obstacle detected. Maintaining current speed.");
         return Current_Speed;
      end if;
   end Adjust_Speed;

   -- This function is used to check if the vehicle is clear to move based on the radar data.
   -- It takes the radar data and a threshold distance as parameters.
   -- It checks if the object distance is greater than the threshold.
   -- If the object distance is greater than the threshold, it returns true.
   -- If the object distance is less than or equal to the threshold, it returns false.
   -- The function takes a parameter of type Radar, which is passed by reference.
   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean is
   begin
      return Radar_Data.Object_Distance > Threshold;
   end Is_Clear_To_Move;


end Radar_Systems;   
   