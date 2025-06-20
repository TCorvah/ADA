with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Radar_Systems; use Radar_Systems;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Road_ProfileConfig; use Road_ProfileConfig;
with Vehicle_Constants; use Vehicle_Constants;


package body Radar_Systems is
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
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
         Norm := Norm + Vehicle_Constants.Full_Circle_Angle;
      end loop;
      while Norm >= Vehicle_Constants.Full_Circle_Angle loop
         Norm := Norm - Vehicle_Constants.Full_Circle_Angle;
      end loop;
      return Norm;
   end Normalize_Angle;

   function Get_Sector_Angle(Sector : Float) return Radar_Sector is
      A : Float := Normalize_Angle(Sector);
   begin
      if (A >= 315.0 or A < 45.0) then
         return Radar_Systems.Front;
      elsif A >= 45.0 and A < 135.0 then
         return Radar_Systems.Right;
      elsif A >= 135.0 and A < 225.0 then
         return Radar_Systems.Rear;
      elsif A >= 225.0 and A < 315.0 then
         return Radar_Systems.Left;
      else
         return Radar_Systems.Front; -- Should never reach here if normalization works
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

   function Analyze_Radar_Data(Distance: Float; Road_Profile : Road_ProfileConfig.Road_Profile) return Radar_Data is
   begin
      if Distance <= Road_Profile.Min_Detection_Range then
         return Emergency_Stop; -- Emergency stop if object is too close
      elsif Distance <= Road_Profile.Caution_Distance then
         return Slow_Down; -- Slow down if object is detected within caution distance
      elsif Distance <= Road_Profile.Max_Detection_Range then
         return Caution; -- Caution if object is detected within caution distance
      else
         return Clear_To_Move; -- Clear to move if no objects are detected
      end if;
   end Analyze_Radar_Data;


   procedure Radar_Scan_Highway_Simulation is
      Scan_Count : Float := 0.0;
      Max_Scans : constant Float := 5.0;
      Gen : Generator;
      Object_Distance : Float;

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
         Object_Distance := Random_Distance;
         Put("Angle: ");
         Put(Angle);
         Put("Degree,  Distance to object: ");
         Put(Object_Distance);
         Put_Line ("meters");
         if Object_Distance <=  Vehicle_Constants.Minimum_Detection_Range then
            Put_Line ("Radar: Emergency brake! Object too close");
         elsif Object_Distance <= Vehicle_Constants.Min_Caution_Distance then
            Put_Line ("Radar: Slow down! Object detected at distance: " & Float'Image(Object_Distance) & " meters, angle: " & Float'Image(Angle));
         elsif Object_Distance > Vehicle_Constants.Min_Caution_Distance and Object_Distance <= Vehicle_Constants.Maximum_Detection_Range then
            Put_Line ("Radar: Caution! Object detected at distance: " & Float'Image (Object_Distance) & " meters, angle: " & Float'Image(Angle));
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

   procedure Radar_Scan_Garage_Simulation(Road : Road_ProfileConfig.Road_Profile) is
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
         return Random(Gen) * Road.Max_Detection_Range; -- Random distance between 0 and 40 meters
      end Random_Distance;
   begin
      Reset (Gen);
      Put_Line ("Radar: starting Garage scan...");
      while Scan_Count < Max_Scans loop
         Angle := Random_Angle; -- Generate a random angle for the scan
         Distance := Random_Distance; -- Generate a random distance for the scan
         Sector := Get_Sector_Angle(Angle); -- Determine the sector based on the angle
         Put_Line("");
         Put_Line("Scan #" & Integer'Image(Scan_Count + 1) & ":");

         Put("  Angle    : ");
         Float_IO.Put(Angle, Fore => 6, Aft => 1, Exp => 0);
         Put_Line("°");

         Put("  Distance : ");
         Float_IO.Put(Distance, Fore => 6, Aft => 1, Exp => 0);
         Put_Line(" m");

         Put("  Sector   : " & Radar_Sector'Image(Sector) & 
               " (center: ");
         Float_IO.Put(Sector_Center_Angle(Sector), Fore => 4, Aft => 1, Exp => 0);
         Put_Line("°)");


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
         declare
            Status : Radar_Data := Analyze_Radar_Data(Distance, Road);
         begin
            case Status is
               when Emergency_Stop =>
                  Put("Radar: Emergency brake! Object detected too close: ");
                  Float_IO.Put(Distance, Fore => 6, Aft => 1, Exp => 0);
                  Put(" m, angle: ");
                  Float_IO.Put(Angle, Fore => 6, Aft => 1, Exp => 0);
                  Put_Line("°");

               when Slow_Down =>
                  Put("Radar: Slow down! Object detected at distance: ");
                  Float_IO.Put(Distance, Fore => 6, Aft => 1, Exp => 0);
                  Put(" m, angle: ");
                  Float_IO.Put(Angle, Fore => 6, Aft => 1, Exp => 0);
                  Put_Line("°");

               when Caution =>
                  Put("Radar: Caution! Object detected at distance: ");
                  Float_IO.Put(Distance, Fore => 6, Aft => 1, Exp => 0);
                  Put(" m, angle: ");
                  Float_IO.Put(Angle, Fore => 6, Aft => 1, Exp => 0);
                  Put_Line("°");

               when Clear_To_Move =>
                  Put_Line("Radar: >> Path Clear: ");
                  Put_Line("No objects detected in the " & Radar_Sector'Image(Sector) & " sector.");
                  Put_Line("Vehicle can move freely");
            end case;
            if Status /= Clear_To_Move then
               Float_IO.Put(Distance, Fore => 6, Aft => 1, Exp => 0);
               Put(" m, angle: ");
               Float_IO.Put(Angle, Fore => 6, Aft => 1, Exp => 0);
               Put_Line("°");
               --Put_Line("Radar: >> Path Blocked: ");
               --Put_Line("Object detected in the " & Radar_Sector'Image(Sector) & " sector.");
            end if;
         end;
            
         delay 2.5; -- Simulate time delay for radar scan
         Scan_Count := Scan_Count + 1; 
         if Scan_Count = Max_Scans then
            Put_Line ("Radar: Garage scan completed.");
         end if;
      
      end loop;
   end Radar_Scan_Garage_Simulation;
      
   


end Radar_Systems;   
   