with Ada.Text_IO; use Ada.Text_IO;
with Radar_Systems;

package body Radar_Systems is
   
   procedure Activate_Radar(R : in out Radar) is 
   begin
      R.Status := Radar_Systems.On;
      Put_Line ("Radar: Activated");
   end Activate_Radar;

   procedure Deactivate_Radar(R : in out Radar) is
   begin
      R.Status := Radar_Systems.Off;
      Put_Line ("Radar: Deactivated");
   end Deactivate_Radar;

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


   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean is
   begin
      return Radar_Data.Object_Distance > Threshold;
   end Is_Clear_To_Move;


end Radar_Systems;   
   