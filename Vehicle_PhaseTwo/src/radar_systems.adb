with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Radar_Systems; use Radar_Systems;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
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

   --- Function to get the center angle of a radar sector based on its quadrants.
   --- The sector center angle is returned  return the midpoint angle of 
   --  the specified radar sector in each quadrant.
   function Sector_Center_Angle(Sector : Radar_Sector) return Float is
   begin
      case Sector is
         when Front =>
            return  Vehicle_Constants.Front_Sector_Midpoint; -- 0 degrees midpoint angle for Front sector (315 to 45 degrees)
         when Rear =>
            return Vehicle_Constants.Rear_Sector_Midpoint; -- 180 degrees midpoint angle for Rear sector(135 to 225 degrees)
         when Left =>
            return Vehicle_Constants.Left_Sector_Midpoint; -- 270 degrees midpoint  angle for Left sector (225 to 315 degrees)
         when Right =>
            return Vehicle_Constants.Right_Sector_Midpoint; -- 90 degrees midpoint angle for Right sector( 45 to 135 degrees)
      end case;
   end Sector_Center_Angle;
   

   --- Function to get the radar sector based on the given angle for each quadrant.
   -- The function normalizes the angle to ensure it falls within the range of 0 to 360 degrees.
   -- EaCch sector represents a 90-degree quadrant of the 360-degree field of view.
   function Get_Sector_Angle(Sector : Float) return Radar_Sector is
      A : Float := Normalize_Angle(Sector);
   begin
      if (A >= Vehicle_Constants.Left_Sector_End) or else  (A < Vehicle_Constants.Front_Sector_End) then
         return Radar_Systems.Front;
      elsif A >= Vehicle_Constants.Right_Sector_Start and then A < Vehicle_Constants.Right_Sector_End then
         return Radar_Systems.Right;
      elsif A >= Vehicle_Constants.Rear_Sector_Start and then A < Vehicle_Constants.Rear_Sector_End then
         return Radar_Systems.Rear;
      elsif A >= Vehicle_Constants.Left_Sector_Start and then A < Vehicle_Constants.Left_Sector_End   then
         return Radar_Systems.Left;
      else
         return Radar_Systems.Front; -- Should never reach here if normalization works
      end if;
   end Get_Sector_Angle;



 -- Math helper to normalize the angle to be in compliance of the geometric circle.
   function Normalize_Angle(Angle : Float) return Float is
      Norm : Float := Angle;
   begin
      while Norm < Vehicle_Constants.Front_Sector_Midpoint loop
         Norm := Norm + Vehicle_Constants.Full_Circle_Angle;
      end loop;
      while Norm >= Vehicle_Constants.Full_Circle_Angle loop
         Norm := Norm - Vehicle_Constants.Full_Circle_Angle;
      end loop;
      return Norm;
   end Normalize_Angle;

   --simulate the radar scan for random distance between min and max detection range
   -- A random number generator is used to generate random distances within the radar's detection range.
   -- A radar can only detect objects within its specified minimum and maximum detection range.
   Gen : Generator;
   function Random_Distance return Float is
      Rand : Float;
      begin
         Rand := Vehicle_Constants.Minimum_Detection_Range + (Random(Gen) * (Vehicle_Constants.Maximum_Detection_Range - Vehicle_Constants.Minimum_Detection_Range));
         return Rand;
      end Random_Distance;

   -- Simulates a radar detection angle within a single radar sector.
   -- The generated angle is uniformly distributed within the sector's
   -- field of view, defined as ±Detection_Angle degrees from the
   -- sector's center angle.
   -- This ensures detections remain fully contained within the sector
   -- and do not overlap adjacent sectors.
   function Random_Angle return Float is
      Center_Angle : Radar;
   begin
      return Center_Angle.Center_Angle - Vehicle_Constants.Detection_Angle  + Random(Gen) * (2.0 * Vehicle_Constants.Detection_Angle);     
   end Random_Angle;

   -- the function is to determine the angular accuracy of the detected object within the radar sector.
   -- A high accuracy of detection is when the object angle is within one-third of the detection angle from the sector midpoint.    
   function Angular_Accuracy_Zone(Angle_Diff: Float) return Radar_Angular_Zone is
   begin 
      if Angle_Diff <= Vehicle_Constants.Detection_Angle / 3.0 then
         return Strong;
      elsif Angle_Diff <= (2.0 * Vehicle_Constants.Detection_Angle) / 3.0 then
         return Medium;
      elsif Angle_Diff <= Vehicle_Constants.Detection_Angle then
         return Weak;
      else
         return Outside_FOV;
      end if;
   end Angular_Accuracy_Zone;


   function Range_Accuracy_Zone(Distance : Float) return Radar_Range_Zone is
   begin
       if Distance < (Vehicle_Constants.Minimum_Detection_Range) then
         return Outside_Range;
      elsif Distance <= (Vehicle_Constants.Maximum_Detection_Range / 4.0) then   
         return Close;
      elsif Distance <= Vehicle_Constants.Maximum_Detection_Range then
         return Far;
      else
         return Outside_Range;
      end if;
   end Range_Accuracy_Zone;

   -- Function to simulate radar object detection based on object distance and angle.
   -- The function determines the radar sector, calculates the angle difference from the sector center,
   function Detect_Object(Object_Distance : Float; Object_Angle : Float) return Radar is
      Sector : Radar_Sector;
      Center_Angle : Float;
      Angle_Diff : Float;
      Angular_Zone : Radar_Angular_Zone;
      Distance_Zone : Radar_Range_Zone;
      Data : Radar;
   begin
      -- Determine the radar sector based on the object angle
      Sector := Get_Sector_Angle(Object_Angle);

      -- Get the center angle of the determined sector
      Center_Angle := Sector_Center_Angle(Sector);

      -- Calculate the absolute difference between the object angle and the sector center angle
      Angle_Diff  := abs(Normalize_Angle(Object_Angle) - Center_angle);
      if Angle_Diff > (Vehicle_Constants.Angle_Sector_FOV * 2.0) then
         Angle_Diff := Vehicle_Constants.Full_Circle_Angle - Angle_Diff;
      end if;

      -- Determine the detection zone based on the angle difference
      Angular_Zone  := Angular_Accuracy_Zone(Angle_Diff);

      if Angular_Zone  = Outside_FOV then
         Data.Object_Detected := False;
         return Data;
      end if;

      -- Determine the distance zone based on the object distance
      Distance_Zone := Range_Accuracy_Zone(Object_Distance);
      if Distance_Zone = Outside_Range then
         Data.Object_Detected := False;
         return Data;
      end if;

      -- fill detection record
      Data.Object_Detected := True;
      Data.Object_Distance := Object_Distance;
      Data.Object_Angle := Object_Angle;     
      Data.Center_Angle := Center_angle;
      Data.Angular_Zone := Angular_Zone;
      Data.Sector := Sector;
      return Data;
   end Detect_Object;
        

  
  


end Radar_Systems;   
   