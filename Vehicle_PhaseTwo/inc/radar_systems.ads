with Road_ProfileConfig;

-- This file is part of the Vehicle Project Phase One
-- This package provides the radar system functionality for the vehicle project.
-- It includes procedures and functions for detecting objects, adjusting speed, and checking if the vehicle can move.
-- The radar system is designed to work with the vehicle's sensor system and vehicle types.
-- The package is used to enhance the vehicle's safety and performance by providing real-time data on the surrounding environment.
-- The radar system is a critical component of the vehicle's overall functionality.
-- The package is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
-- The package is written in Ada and follows best practices for software development.
-- The package is designed to be easy to understand and maintain, with clear documentation and comments.
-- The package is intended for use in a vehicle simulation project and may be used in other projects as well.

package Radar_Systems is
   Min_Detection_Range : constant Float := 0.5; -- Minimum distance to detect an object, Vehicle will stop
   Max_Detection_Range : constant Float := 100.0; -- Maximum distance to detect an object, Vehicle will slow down
   Detection_Angle :  constant Float := 45.0; -- Angle of detection in degrees

   -- Define the radar status enumeration
   -- This enumeration defines the possible states of the radar system.
   -- The states include Off and On, indicating whether the radar is active or inactive.
   -- The enumeration is used to control the radar system's functionality and behavior.
   -- The radar status is used to determine if the radar is currently detecting objects or not.
   -- The radar status is a critical component of the vehicle's safety and performance.
 type Radar_Status is (Off, On);
 type Radar_Sector is (Front, Rear, Left, Right);
 type Radar_Data is (Emergency_Stop, Slow_Down, Caution, Clear_To_Move);

   -- Define the radar record type
   -- This record holds the details of the radar system, including its status, object detection status, distance to the object, and motion status.
   -- The record is used to store the radar system's state and pass it between procedures.
   type Radar is tagged record
      Status  : Radar_Status := Off;
      Object_Detected : Boolean := False;
      Object_Distance   : Float   := 0.0;
      Object_In_Motion : Boolean := False;
      Field_Of_View : Radar_Sector := Front;
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

   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean;
   function Sector_Center_Angle(Sector : Radar_Sector) return Float;
   function Normalize_Angle(Angle : Float) return Float;
   function Get_Sector_Angle(Sector : Float) return Radar_Sector;
   function Analyze_Radar_Data(Distance: Float; Road_Profile: Road_ProfileConfig.Road_Profile) return Radar_Data;

      -- This function normalizes the angle to be within the range of -180 to 180 degrees.
      -- The function is used to ensure that angles are consistent and can be compared easily.
      -- The function is designed to be modular and reusable, allowing for easy integration with other vehicle systems.

   -- Procedure to simulate radar scan
   -- This procedure simulates the radar scan by generating random distances and angles.
   -- The procedure is called to test the radar system's functionality and behavior.
   -- The procedure is used to control the radar system's functionality and behavior.
   -- The procedure is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   -- The procedure is written in Ada and follows best practices for software development.
   procedure Radar_Scan_Highway_Simulation;
   procedure Radar_Scan_Garage_Simulation(Road : Road_ProfileConfig.Road_Profile);
 

end Radar_Systems;
