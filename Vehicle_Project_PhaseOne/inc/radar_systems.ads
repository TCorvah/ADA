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
   -- Define the radar record type
   -- This record holds the details of the radar system, including its status, object detection status, distance to the object, and motion status.
   -- The record is used to store the radar system's state and pass it between procedures.
   type Radar is tagged record
      Status  : Radar_Status := Off;
      Object_Detected : Boolean := False;
      Object_Distance   : Float   := 0.0;
      Object_In_Motion : Boolean := False;
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

   -- Procedure to detect objects using the radar system
   -- This procedure checks the distance to the detected object and sets the object detection status accordingly.
   -- The procedure is called to determine if the vehicle can move based on the radar input.
   -- The procedure is used to control the radar system's functionality and behavior.
   -- The procedure is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   procedure Detect_Object(Radar_Data : in out Radar; Threshold : Float);

   -- Function to adjust the vehicle's speed based on the radar data
   -- This function checks the distance to the detected object and adjusts the speed accordingly.
   -- The function returns the adjusted speed based on the radar input.
   -- The function is called to determine the vehicle's speed based on the radar data.
   -- The function is used to control the radar system's functionality and behavior.
   function Adjust_Speed(Radar_Data : in Radar;  Current_Speed : in Integer) return Integer;

   -- Function to check if the vehicle is clear to move based on the radar data
   -- This function checks the distance to the detected object and returns a boolean value indicating if the vehicle can move.
   -- The function is called to determine if the vehicle can move based on the radar input.
   function Is_Clear_To_Move(Radar_Data : in Radar; Threshold : Float) return Boolean;

end Radar_Systems;
