
package Radar_Systems is
   Min_Detection_Range : constant Float := 0.5; -- Minimum distance to detect an object, Vehicle will stop
   Max_Detection_Range : constant Float := 100.0; -- Maximum distance to detect an object, Vehicle will slow down
   Detection_Angle :  constant Float := 45.0; -- Angle of detection in degrees

 type Radar_Status is (Off, On);

   type Radar is tagged record
      Status  : Radar_Status := Off;
      Object_Detected : Boolean := False;
      Object_Distance   : Float   := 0.0;
      Object_In_Motion : Boolean := False;
   end record;

   -- procedure to detect objects based on proximity
   procedure Activate_Radar(R : in out Radar);
   procedure Deactivate_Radar(R : in out Radar);
   procedure Detect_Object(Radar_Data : in out Radar; Threshold : Float);

   -- Function to adjust the vehicle's speed based on radar input
   function Adjust_Speed(Radar_Data : in Radar;  Current_Speed : in Integer) return Integer;

   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean;

end Radar_Systems;
