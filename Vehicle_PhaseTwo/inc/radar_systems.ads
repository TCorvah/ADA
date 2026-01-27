
with Vehicle_Constants;use Vehicle_Constants;

-- This package implements the vehicle radar subsystem.
-- The radar measures object distance within the vehicle’s field of view,
-- classifies detected objects based on proximity, and reports structured
-- data to the vehicle system to support safe and informed decision-making.

package Radar_Systems is
   -- Define the radar status enumeration
   -- This enumeration defines the possible states of the radar system.
   -- The states include Off and On, indicating whether the radar is active or inactive.
   -- The enumeration is used to control the radar system's functionality and behavior.
   -- The radar status is used to determine if the radar is currently detecting objects or not.
   -- The radar status is a critical component of the vehicle's safety and performance.
 type Radar_Status is (Off, On);
 type Radar_Sector is (Front, Rear, Left, Right);
 type Radar_Range_Zone is (Close, Medium, Far, Outside_Range);
 type Radar_Angular_Zone is (Strong, Medium, Weak, Outside_FOV);

   -- Define the radar record type
   -- This record holds the details of the radar system, including its status, object detection status, distance to the object, and motion status.
   -- The record is used to store the radar system's state and pass it between procedures.
   type Radar is tagged record
      Status  : Radar_Status := Off;
      Object_Detected : Boolean := False;
      Object_Distance   : Float   := 0.0;
      Object_Angle      : Float   := 0.0;
      Object_In_Motion  : Boolean := False;
      Field_Of_View : Float := Vehicle_Constants.Angle_Sector_FOV; -- 90 degrees, the cone shape where the radar can detect objects;
      Center_Angle : Float; -- The center angle of the radar sector where the object is detected
      Sector : Radar_Sector;
      Angular_Zone : Radar_Angular_Zone;
      Distance_Zone : Radar_Range_Zone;
   end record;

   -----------------------------------------------------------
   -- Procedures and Functions for Radar System
   -----------------------------------------------------------
   -- Procedure to activate the radar system
   -- This procedure sets the radar status to On and initializes the object detection status.
   -- The procedure is called to start the radar system and begin detecting objects.
   -- The procedure is used to control the radar system's functionality and behavior.
   -- The procedure is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   -- The procedure is written in Ada and follows best practices for software development.
   procedure Activate_Radar(R : in out Radar);

   -- Procedure to deactivate the radar system     
   -- This procedure sets the radar status to Off and clears the object detection status.
   -- The procedure is called to stop the radar system and cease object detection.
   -- The procedure is used to control the radar system's functionality and behavior.
   -- The procedure is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   procedure Deactivate_Radar(R : in out Radar);


-- Returns the geometric center angle (in degrees) of a radar sector.
-- Each sector represents a 90-degree quadrant of the 360-degree field of view.
-- There are four sectors: Front (0°), Right (90°), Rear (180°), and Left (270°) with their respective midpoints.
function Sector_Center_Angle(Sector : Radar_Sector) return Float;

   -- Function to normalize an angle to the range [0, 360) degrees
   -- This function takes an angle in degrees and normalizes it to the range of 0 to 360 degrees.
   -- The function is used to ensure that angles are consistently represented within the specified range.
   -- The function is called to support calculations and comparisons involving angles in the vehicle's
   function Normalize_Angle(Angle : Float) return Float;

   --- Function to get the radar sector based on a given angle
   -- The normalize angle is called in here to ensure the returned angle is within the range of 0 to 360 degrees.
   -- the function returns the radar sector (Front, Rear, Left, Right) corresponding to the given angle of the sector.
   function Get_Sector_Angle(Sector : Float) return Radar_Sector;

   -- Function to determine angular closeness of the detected object to the center of the radar sector
   -- The function takes the angle difference between the detected object and the sector center angle as input
   -- It returns the detection zone classification (Strong, Medium, Weak, Outside_FOV)
   function Angular_Accuracy_Zone(Angle_Diff: Float) return Radar_Angular_Zone;

   --function returns the range accuracy of an object based on its distance from the radar.
   -- a smaller distance of detection implies a higher accuracy of the radar reading.
   -- the function classifies the distance into zones: Close, Medium, Far, Outside_Range.
   function Range_Accuracy_Zone(Distance : Float) return Radar_Range_Zone;

   -- Function to detect an object based on its distance and angle
   -- The function determines the radar sector, calculates the angle difference from the sector center,
   -- and classifies the detection zone and distance zone of the detected object.
   function Detect_Object(Object_Distance : Float; Object_Angle : Float) return Radar;

end Radar_Systems;
