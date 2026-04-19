
with Vehicle_Constants;
with Vehicle_System;
with Scenario_Type;

package Sensor_System is
-- This package provides procedures for managing the vehicle sensor system and is a composition class of the Vehicle_System package.              
-- It includes functions to activate and deactivate sensors, check seatbelt status,
-- check visibility, and handle object detection.
-- The package is designed to work with the Vehicle_System package and is used to enhance vehicle safety and performance.
-- The package is written in Ada and follows best practices for software development.
   type Sensor_Status is (OFF, ON);

-- Define the sensor record type
   -- This record holds the details of the vehicle sensor system, including its status, door status, detected weight, seatbelt status, visibility, and headlights status.
   -- The record is used to store the sensor system's state and pass it between procedures.
   -- The record is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   -- The record is written in Ada and follows best practices for software development.
   type Sensor is tagged record
      Status : Sensor_Status;
      Door_Status : Vehicle_System.Door_Status_Type;
      Detected_Weight : Float := 20.0;  
      Seatbelt_Status : Vehicle_System.SeatBelt_Status_Type;
      Visibility : Scenario_Type.Time_of_Day;
      Headlights_On : Boolean;
      Engine : Vehicle_System.Engine_Status_Type;
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
  
   -- Function to check if the engine is running
   -- This function checks the engine status and returns True if the engine is running, False otherwise
   function Engine_Status return Vehicle_System.Engine_Status_Type;

   
   -- Function to check if the door is open
   -- This function checks the door status and returns True if the door is open, False otherwise.
   -- The function is called to determine if the door is open or closed
   function Is_Door_Open return Vehicle_System.Door_Status_Type;

   -- Function to check if the seat is occupied
   -- This function checks the seat status and returns True if the seat is occupied, False otherwise.
   -- The function is called to determine if the seat is occupied or not.
   -- The function is used to control the seatbelt system and ensure passenger safety.
   function Is_Seat_Occupied return Vehicle_System.SeatBelt_Status_Type;

   -- Function to check if the headlights should be turned on  
   -- This function checks the visibility conditions and returns True if the headlights should be turned on, False otherwise.
   -- The function is called to determine if the headlights should be activated based on the time of day.
   -- The function is used to control the headlights system and ensure visibility during night conditions.
   -- The function is designed to be modular and reusable, allowing for easy integration with other vehicle systems.
   function Should_Turn_On_Headlights return Scenario_Type.Time_of_Day;
  

   -- The procedure is called to determine if the door is open or closed.
   -- The procedure is used to control the door system and ensure passenger safety.
   procedure Toggle_Door(S : in out Sensor);

   -- Procedure to check the seatbelt status 
   -- This procedure checks the seatbelt status and returns True if the seatbelt is on, False otherwise.
   -- The procedure is called to determine if the seatbelt is fastened or not.
   -- The procedure is used to control the seatbelt system and ensure passenger safety.
   procedure Check_Seatbelt(S : Sensor);

   function Is_Seatbelt_On(S : in Sensor) return Vehicle_System.SeatBelt_Status_Type;


   -- Procedure to check the seat status
   -- This procedure checks the seat status and returns the state of the seat being occupied or not.
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

end Sensor_System;
