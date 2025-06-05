with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Radar_Systems;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Vehicle_Constants; use Vehicle_Constants;

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

   function Normalize_Angle(Angle : Float) return Float is
      Norm : Float := Angle;
   begin
      while Norm < 0.0 loop
         Norm := Norm + 360.0;
      end loop;
      while Norm >= 360.0 loop
         Norm := Norm - 360.0;
      end loop;
      return Norm;
   end Normalize_Angle;

   function Get_Sector_Angle(Sector : Float) return Radar_Sector is
      A: Float := Normalize_Angle(Sector);
   begin
      if A >= 337.5 or A < 22.5 then
         return Radar_Systems.Front; -- Front sector
      elsif A >= 22.5 and A < 67.5 then
         return Radar_Systems.Right; -- Right sector
      elsif A >= 67.5 and A < 112.5 then
         return Radar_Systems.Rear; -- Rear sector
      elsif A >= 112.5 and A < 157.5 then
         return Radar_Systems.Left; -- Left sector
      else
         return Radar_Systems.Front; -- Default to Front sector if angle is not recognized
      end if;
   end Get_Sector_Angle;
     

   function Sector_Center_Angle(Sector : Radar_Sector) return Float is
   begin
      case Sector is
         when Front =>
            return 0.0; -- Front sector angle range
         when Rear =>
            return 180.0; -- Rear sector angle range
         when Left =>
            return 270.0; -- Left sector angle range
         when Right =>
            return 90.0; -- Right sector angle range
      end case;
   end Sector_Center_Angle;


   procedure Radar_Scan_Highway_Simulation is
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
         if Distance <=  Vehicle_Constants.Minimum_Constant_Distance then
            Put_Line ("Radar: Emergency brake! Object too close");
         elsif Distance <= Vehicle_Constants.Min_Caution_Distance then
            Put_Line ("Radar: Slow down! Object detected at distance: " & Float'Image(Distance) & " meters, angle: " & Float'Image(Angle));
         elsif Distance > Vehicle_Constants.Minimum_Constant_Distance and Distance <= Vehicle_Constants.MAX_Speed then
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
   end Radar_Scan_Highway_Simulation;

   procedure Radar_Scan_Garage_Simulation is
      Scan_Count : Integer := 0;
      Max_Scans : constant Integer := 5;
      Gen : Generator;
      Distance : Float;
      Angle : Float;
      Sector : Radar_Sector;
       --simulate random angle from -45 to 315 degrees(full scan)
      function Random_Angle return Float is
      begin
         return -45.0 + (Random(Gen) * 360.0); -- Random angle between -45 and 315 degrees
      end Random_Angle;

      --simulate the radar scan for random distance
      function Random_Distance return Float is
      begin
         return Random(Gen) * 100.0; -- Random distance between 0 and 100 meters
      end Random_Distance;
   begin
      Reset (Gen);
      Put_Line ("Radar: starting Garage scan...");
      while Scan_Count < Max_Scans loop
         Angle := Random_Angle; -- Generate a random angle for the scan
         Distance := Random_Distance; -- Generate a random distance for the scan
         Put_Line ("Radar: Scanning at Angle: " & Float'Image(Angle) & " degrees, Distance: " & Float'Image(Distance) & " meters");
         -- Determine the sector based on the random angle
         Sector := Get_Sector_Angle(Angle); -- Get the sector based on the random angle
         Put(", Sector Center Angle: ");
         Put(Float'Image(Sector_Center_Angle(Sector)));
         New_Line;
       
         Put_Line ("Scan #" & Integer'Image(Scan_Count + 1) &
          ": Angle = " & Float'Image(Angle) &
          ", Distance = " & Float'Image(Distance) &
          ", Sector = " & Radar_Sector'Image(Sector));

         case Sector is
            when Radar_Systems.Front =>
               Put_Line ("Front Sector Scan for objects:");
            when Radar_Systems.Rear =>
               Put_Line ("Rear Sector Scan for objects: ");
            when Radar_Systems.Left =>
               Put_Line ("Left Sector Scan for objects: ");
            when Radar_Systems.Right =>
               Put_Line ("Right Sector Scan for objects: ");
         end case;
         --Distance checks
         if Distance <=  10.0 then
            Put_Line ("Radar: Emergency brake! Object too close");
         elsif Distance <= 30.0 then
            Put_Line ("Radar: Slow down! Object detected at distance: " & Float'Image(Distance) & " meters, angle: " & Float'Image(Angle));
         elsif Distance > 30.0 and Distance <= 45.0 then
            Put_Line ("Radar: Caution! Object detected at distance: " & Float'Image(Distance) & " meters, angle: " & Float'Image(Angle));
         else
            Put_Line ("Radar: >> Path Clear: ");
            Put_Line ("No objects detected in the " & Radar_Sector'Image(Sector) & " sector.");
            Put_Line ("Vehicle can move freely");
         end if;
         delay 2.5; -- Simulate time delay for radar scan
         Scan_Count := Scan_Count + 1; -- Increment the scan count
         if Scan_Count = Max_Scans then
            Put_Line ("Radar: Garage scan completed.");
         end if;
      end loop;
   end Radar_Scan_Garage_Simulation;
      
   


end Radar_Systems;   
   