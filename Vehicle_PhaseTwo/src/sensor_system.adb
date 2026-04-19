with Ada.Text_IO; use Ada.Text_IO;
with Scenario_Type; use Scenario_Type;
with Timing_Controller; use Timing_Controller;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Vehicle_Constants;use Vehicle_Constants;
with Vehicle_System;


package body Sensor_System is

   -- Sensor System Package Body 
   -- This package implements the functionality of the sensor system for the vehicle.
   -- It includes procedures and functions to manage the sensor state, check seat occupancy,
   -- handle door status, and manage visibility conditions.
   -- The package provides a set of procedures to activate/deactivate sensors, check seatbelt status,
   -- and manage the vehicle's headlights based on visibility conditions.
   -- The package also includes a procedure to handle object detection while the vehicle is in motion.
   -- The package is designed to be used in conjunction with the Vehicle_System package.
   -- The Sensor type is defined in the Vehicle_Constants package and includes attributes such as
   -- detected weight, seatbelt status, door status, and visibility conditions.
   -- The package provides a set of procedures to manage the sensor system and ensure the vehicle's safety 

      -- This procedure activates the sensor system by setting its status to "On".
      -- It also prints a message indicating that the sensor has been activated.
      -- The procedure takes a Sensor object as an input parameter and modifies its status.
      -- The procedure does not return any value.
      procedure Activate_Sensor(S : in out Sensor) is
      begin
         S.Status := Sensor_System.ON;
         Put_Line ("Sensor: Activated");
      end Activate_Sensor;

      -- This procedure deactivates the sensor system by setting its status to "Off".
      -- It also prints a message indicating that the sensor has been deactivated.  
      -- The procedure takes a Sensor object as an input parameter and modifies its status.
      -- The procedure does not return any value.
      procedure Deactivate_Sensor(S : in out Sensor) is
      begin
         S.Status := Sensor_System.OFF;
         Put_Line ("Sensor: Deactivated");
      end Deactivate_Sensor;

      -- This function checks if the vehicle door is open by evaluating 
      -- the Door_Open attribute of the Sensor object.
      function Is_Door_Open(S : in Sensor) return  Vehicle_System.Door_Status_Type is
      begin
         if S.Door_Status = Vehicle_System.Door_Open then
            Put_Line("Sensor: Door is open");
            return S.Door_Status;
         else
            S.Door_Status := Vehicle_System.Closed;
            Put_Line("Sensor: Door is closed");
            return S.Door_Status;
         end if;
      end Is_Door_Open;

      -- This function checks the door status and prints a message 
      --  indicating whether the engine is running
      function Engine_Status(S : in Sensor) return Vehicle_System.Engine_Status is
      begin
         if S.Engine = Vehicle_System.Engine_Off then
            Put_Line ("Sensor: Engine is Stopped");
            return S.Engine;      
         else 
            S.Engine := Vehicle_System.Engine_Off;
            Put_Line ("Sensor: Engine is Running");
            return S.Engine;
         end if;
      end Engine_Status;
   

      -- This function checks if the seat is occupied by evaluating the Detected_Weight attribute of the Sensor object.
      -- It returns a Boolean value indicating the seat occupancy status.
      -- The function takes a Sensor object as an input parameter and returns a Boolean value.
      function Seat_Occupied(S : Sensor) return Boolean is
      begin
         if S.Detected_Weight >= Vehicle_Constants.Minimum_Weight_Occupancy then
            Put_Line("Sensor: Seat is occupied");
            return True;
         else
            Put_Line("Sensor: Seat is not occupied");
            return False;
         end if;
      end Seat_Occupied;

      -- This procedure toggles the door status by changing the Door_Open attribute of the Sensor object.
      -- It also prints a message indicating the door status.
      -- The procedure takes a Sensor object as an input parameter and modifies its status.
      -- The procedure does not return any value.
      procedure Toggle_Door(S : in out Sensor) is
      begin
         if Is_Door_Open(S) = Vehicle_System.Door_Open then
            Put_Line ("human object detected, Vehicle door is open" );
         else
            Put_Line ("Vehicle door is now close");
         end if;
      end Toggle_Door;


   function Is_Seatbelt_On(S : in Sensor) return Vehicle_System.SeatBelt_Status is
   begin
      if Seat_Occupied(S) and S.Seatbelt_Status = Vehicle_System.SeatBelt_On then
         Put_Line("Sensor: Seatbelt is fastened");
         return S.Seatbelt_Status;        
      else
         S.Seatbelt_Status := Vehicle_System.SeatBelt_Off;
         Put_Line("Sensor: Seatbelt is fastened");
         return S.Seatbelt_Status;
      end if;
   end Is_Seatbelt_On;
   

   -- This procedure checks the visibility condition by evaluating the Visibility attribute of the Sensor object.
   -- It prints a message indicating whether the headlights should be turned on or off.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the Visibility attribute to determine if the time of day is night or day.
   -- If the time of day is night, it turns on the headlights and prints "Sensor: Headlights turned ON, Time of Day is night".
   -- If the time of day is day, it turns off the headlights and prints "Sensor: Headlights OFF, daylight detected".
   procedure Check_Visibility(S : in out Sensor) is
   begin
      if Should_Turn_On_Headlights = Scenario.Night then  
         Put_Line("Sensor: Initializing headlight timer.");
         Put_Line("Sensor: scanning for night time visibility.");
         Timing_Controller.Headlight_Timer_task.Set_Headlight_Timer(0.1);
         -- Wait for a duration longer than the timer to observe the output
         delay 0.2; 
         Put_Line("Sensor: Night Time detected.");    
         Timing_Controller.Headlight_Timer_task.Shutdown;
         -- Allow time for the shutdown message to be displayed
         delay 0.10;
         S.Headlights_On := True;
         Put_Line("Sensor: Headlights turned ON, Time of Day is night.");
      else
         if Should_Turn_On_Headlights = Scenario.Day then
             S.Headlights_On := False;
             Put_Line("Sensor: Day Time detected.");
             Put_Line("Sensor: Headlights OFF, daylight detected.");
            end if;
      end if;
   end Check_Visibility;

   -- This function checks if the headlights should be turned on based on the visibility condition.
   -- It evaluates the Visibility attribute of the Sensor object.
   -- the function updates the visibility condition to night and updates the headlights status.
   function Should_Turn_On_Headlights return Scenario.Time_of_Day is
   begin
      if S.Visibility = Scenario.Night then
         return S.Visibility;
      else
         S.Visibility := Scenario.Day;
         return S.Visibility;
      end if;
   end Should_Turn_On_Headlights;

   -- This procedure updates the headlights status based on the visibility condition.
   -- It evaluates the Visibility attribute of the Sensor object.
   -- It prints a message indicating whether the headlights are turned on or off.
   procedure Update_Headlights(S : in out Sensor) is 
   begin
      if Should_Turn_On_Headlights = Scenario.Night then
         Put_Line("Sensor: Headlights turned ON due to low light.");
      else
         Should_Turn_On_Headlights := Scenario.Day;
         Put_Line("Headlights turn Off; visibility is suficient.");
      end if;
   end Update_Headlights;

end Sensor_System;