
package Radar_Systems is

 type Radar_Status is (Off, On);

   type Radar is tagged record
      Status  : Radar_Status := Off;
      Min_Detection_Range : Float := 0.05;
      Max_Detection_Range : Float := 100.0;
      Object_Distance   : Float   := 0.0;
      Object_In_Motion : Boolean := False;
   end record;

   -- procedure to detect objects based on proximity
   procedure Activate_Radar(R : in out Radar);
   procedure Detect_Object(Radar_Data : in out Radar; Threshold : Float);

   -- Function to adjust the vehicle's speed based on radar input
   function Adjust_Speed(Radar_Data : in Radar;  Current_Speed : in Integer) return Integer;

   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean;

end Radar_Systems;
