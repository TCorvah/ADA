with Vehicle_Constants;
with Vehicle_System;

package Sensor_System is
-- This package provides procedures for managing the vehicle sensor system.               
-- It includes functions to activate and deactivate sensors, check seatbelt status,
-- check visibility, and handle object detection.
-- The package is designed to work with the Vehicle_System package and is used to enhance vehicle safety and performance.
-- The package is written in Ada and follows best practices for software development.
   type Sensor_Status is (Off, On);

   -- Define the time of day enumeration
   -- This enumeration defines the possible states of the time of day.
   -- The states include Day and Night, indicating the current visibility conditions.
   -- The enumeration is used to control the sensor system's functionality and behavior.
   -- The time of day is used to determine if the headlights should be turned on or off.
   type Time_of_Day is (Day, Night);

-- Define the sensor record type
   -- This record holds the details of the vehicle sensor system, including its status, door status, detected weight, seatbelt status, visibility, and headlights status.
   -- The record is used to store the sensor system's state and pass it between procedures.
   -- The record is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   -- The record is written in Ada and follows best practices for software development.
   type Sensor is tagged record
      Status : Sensor_Status := Off;
      Door_Open : Boolean := False;
      Detected_Weight : Float := 20.0;   -- Weight of the detected object
      Seatbelt_On : Boolean := False;
      Visibility : Time_of_Day := Night; -- from 0.0 (dark) to 1.0 (bright)
      Headlights_On : Boolean := False;
   end record;


   -- -----------------------------------------------------------
   -- Procedures and Functions for Sensor System
   -- -----------------------------------------------------------
   -- Procedure to activate the sensor system
   -- This procedure sets the sensor status to On and initializes the sensor system.
   -- The procedure is called to start the sensor system and begin monitoring the vehicle's status.
   -- The procedure is used to control the sensor system's functionality and behavior.
   procedure Activate_Sensor(S : in out Sensor);

   -- Procedure to deactivate the sensor system
   -- This procedure sets the sensor status to Off and clears the sensor system's state.
   -- The procedure is called to stop the sensor system and cease monitoring the vehicle's status.
   -- The procedure is used to control the sensor system's functionality and behavior.
   procedure Deactivate_Sensor(S : in out Sensor);

   -- Function to check if the door is open
   -- This function checks the door status and returns True if the door is open, False otherwise.
   -- The function is called to determine if the door is open or closed
   function Is_Door_Open(S :in Sensor) return  Boolean;

   -- Function to check if the seat is occupied
   -- This function checks the seat status and returns True if the seat is occupied, False otherwise.
   -- The function is called to determine if the seat is occupied or not.
   -- The function is used to control the seatbelt system and ensure passenger safety.
   function Seat_Occupied(S : in Sensor) return Boolean;

   -- Function to check if the headlights should be turned on  
   -- This function checks the visibility conditions and returns True if the headlights should be turned on, False otherwise.
   -- The function is called to determine if the headlights should be activated based on the time of day.
   -- The function is used to control the headlights system and ensure visibility during night conditions.
   -- The function is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   function Should_Turn_On_Headlights(S : in Sensor) return Boolean;
  

   -- The procedure is called to determine if the door is open or closed.
   -- The procedure is used to control the door system and ensure passenger safety.
   procedure Toggle_Door(S : in out Sensor);

   -- Procedure to check the seatbelt status 
   -- This procedure checks the seatbelt status and returns True if the seatbelt is on, False otherwise.
   -- The procedure is called to determine if the seatbelt is fastened or not.
   -- The procedure is used to control the seatbelt system and ensure passenger safety.
   procedure Check_Seatbelt(S : Sensor);

   -- Procedure to check the seat status
   -- This procedure checks the seat status and returns the state of ythe seat being occupied or not.
   -- The procedure is called to determine if the seat is occupied or not.
   procedure Check_Seat(S : Sensor);


   -- Procedure to check the visibility conditions
   -- This procedure checks the visibility conditions and returns True if the headlights should be turned on, False otherwise.
   -- The procedure is called to determine if the headlights should be activated based on the time of day.
   -- The procedure is used to control the headlights system and ensure visibility during night conditions.
   procedure Check_Visibility(S : in out Sensor);

   -- Procedure to update the headlights status
   -- This procedure checks the visibility conditions and updates the headlights status accordingly.
   -- The procedure is called to determine if the headlights should be activated based on the time of day.
   -- The procedure is used to control the headlights system and ensure visibility during night conditions
   procedure Update_Headlights(S : in out Sensor);

   -- Procedure to handle object detection
   -- This procedure checks the distance to the detected object and updates the object detection status accordingly.    
   -- The procedure is called to determine if the vehicle can move based on the sensor input.
   -- The procedure is used to control the sensor system's functionality and behavior.
   procedure Handle_Object_Detection(V : in out Vehicle_System.Vehicle; Current_Speed : in Float);

end Sensor_System;
