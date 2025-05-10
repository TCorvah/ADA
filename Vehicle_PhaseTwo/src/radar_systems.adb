with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Radar_Systems;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

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

   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean is
   begin
      return Radar_Data.Object_Distance > Threshold;
   end Is_Clear_To_Move;


   procedure Radar_Scan_Simulation is
      Scan_Count : Float := 0.0;
      Max_Scans : constant Float := 5.0;
      Gen : Generator;
      Distance : Float;
      Angle : Float;
       --simulate the radar scan for random distance and angle
      function Random_Angle return Float is
      begin
         return -45.0 + (Random(Gen) * 90.0); -- Random distance between 0 and 100 meters
      end Random_Angle;

      --simulate the radar scan for random distance and angle
      function Random_Distance return Float is
      begin
         return Random(Gen) * 100.0; -- Random distance between 0 and 100 meters
      end Random_Distance;
   begin
      Reset (Gen);
      Put_Line ("Radar: Scanning...");
      loop
         exit when Scan_Count = Max_Scans;
         Angle := Random_Angle; -- Scan across Line of Sight
         Distance := Random_Distance;
         Put("Angle: ");
         Put(Angle);
         Put("Degree,  Distance to object: ");
         Put(Distance);
         Put_Line ("meters");
         if Distance <=  10.0 then
            Put_Line ("Radar: Emergency brake! Object too close");
         elsif Distance <= 30.0 then
            Put_Line ("Radar: Slow down! Object detected at distance: " & Float'Image(Distance) & " meters, angle: " & Float'Image(Angle));
         elsif Distance > 30.0 and Distance <= 45.0 then
            Put_Line ("Radar: Caution! Object detected at distance: " & Float'Image(Distance) & " meters, angle: " & Float'Image(Angle));
         else
            Put_Line ("Radar: >> Path Clear: ");
         end if;
         delay 2.5; -- Simulate time delay for radar scan
         Scan_Count := Scan_Count + 1.0;
         if Scan_Count = Max_Scans then
            Put_Line ("Radar: Scan completed.");
         end if;
      end loop;
      Put_Line ("Radar: Scan completed.");
   end Radar_Scan_Simulation;


end Radar_Systems;   
   