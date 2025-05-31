with Ada.Text_IO; use Ada.Text_IO;
with Timing_Controller; use Timing_Controller;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Vehicle_Constants;use Vehicle_Constants;
with Vehicle_System;use Vehicle_System;



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
      S.Status := Sensor_System.On;
      Put_Line ("Sensor: Activated");
   end Activate_Sensor;

   -- This procedure deactivates the sensor system by setting its status to "Off".
   -- It also prints a message indicating that the sensor has been deactivated.  
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   procedure Deactivate_Sensor(S : in out Sensor) is
   begin
      S.Status := Sensor_System.Off;
      Put_Line ("Sensor: Deactivated");
   end Deactivate_Sensor;

   -- This function checks if the vehicle door is open by evaluating the Door_Open attribute of the Sensor object.
   -- It returns a Boolean value indicating the door status.
   -- The function takes a Sensor object as an input parameter and returns a Boolean value.
   -- The function does not modify the Sensor object.
   function Is_Door_Open(S : in Sensor) return  Boolean is
   begin
      return S.Door_Open;
   end Is_Door_Open;

   -- This function checks if the seat is occupied by evaluating the Detected_Weight attribute of the Sensor object.
   -- It returns a Boolean value indicating the seat occupancy status.
   -- The function takes a Sensor object as an input parameter and returns a Boolean value.
   -- The function does not modify the Sensor object.
   -- The function uses a threshold of 20kg to determine if the seat is occupied.
   -- The function returns True if the detected weight is greater than 20kg, indicating that the seat is occupied.
   function Seat_Occupied(S : Sensor) return Boolean is
   begin
      return S.Detected_Weight >= Vehicle_Constants.Minimum_Weight_Occupancy; -- payload threshold for seatbelt warning is 20kg
   end Seat_Occupied;

   -- This procedure toggles the door status by changing the Door_Open attribute of the Sensor object.
   -- It also prints a message indicating the door status.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the not operator to toggle the Door_Open attribute.
   -- If the door is open, it prints "Vehicle door is open", otherwise it prints "Vehicle door is now close".
   -- The procedure does not return any value.
   procedure Toggle_Door(S : in out Sensor) is
   begin
      S.Door_Open := not S.Door_Open;
      if S.Door_Open then
         Put_Line ("Vehicle door is open" );
      else
         Put_Line ("Vehicle door is now close");
      end if;
   end Toggle_Door;

   -- This procedure checks the seatbelt status by evaluating the Seatbelt_On attribute of the Sensor object.
   -- It prints a message indicating whether the seatbelt is fastened or not.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the Seatbelt_On attribute to determine if the seatbelt is fastened.
   -- If the seatbelt is fastened, it prints "Sensor: Seatbelt is fastened", otherwise it prints "Sensor: Seatbelt is not fastened".
   procedure Check_Seatbelt(S : in Sensor) is 
   begin
      if S.Seatbelt_On = True then
       
         Put_Line ("Sensor: Seatbelt is fastened");
      else
          Put_Line ("Sensor: Seatbelt is not  fastened");
      end if;
   end Check_Seatbelt;

   -- This procedure checks the seat occupancy status by evaluating the Detected_Weight attribute of the Sensor object.
   -- It prints a message indicating whether the seat is occupied or not.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the Seat_Occupied function to determine if the seat is occupied.
   -- If the seat is occupied, it prints "Sensor: Seat is occupied (weight: X kg)", where X is the detected weight.
   -- It also calls the Check_Seatbelt procedure to check the seatbelt status.
   -- If the seat is empty, it prints "Sensor: Seat is empty".
   procedure Check_Seat(S : in Sensor) is
   begin
      if Seat_Occupied(S) then
         Check_Seatbelt(S);
         Put_Line ("Sensor: Seat is occupied , human detected");
      else
         Put_Line ("Sensor: Seat is empty.");
      end if;
   end Check_Seat;

   -- This procedure checks the visibility condition by evaluating the Visibility attribute of the Sensor object.
   -- It prints a message indicating whether the headlights should be turned on or off.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the Visibility attribute to determine if the time of day is night or day.
   -- If the time of day is night, it turns on the headlights and prints "Sensor: Headlights turned ON, Time of Day is night".
   -- If the time of day is day, it turns off the headlights and prints "Sensor: Headlights OFF, daylight detected".
   procedure Check_Visibility(S : in out Sensor) is
   begin
      if S.Visibility = Night then 
         -- Example: turn on headlights if visibility is night
         -- Placeholder for visibility check logic
         Put_Line("Sensor: Initializing headlight timer.");
         Put_Line("Sensor: scanning for night time visibility.");
         Timing_Controller.Headlight_Timer_task.Set_Headlight_Timer(5.0);
         -- Wait for a duration longer than the timer to observe the output
         delay 6.0;

         Put_Line("Sensor: Night Time detected.");
         Timing_Controller.Headlight_Timer_task.Shutdown;
         -- Allow time for the shutdown message to be displayed
         delay 1.0;
         S.Headlights_On := True;
         Put_Line("Sensor: Headlights turned ON, Time of Day is night.");
      else
         S.Headlights_On := False;
         Put_Line("Sensor: Headlights OFF, daylight detected.");
      end if;
   end Check_Visibility;

   -- This function checks if the headlights should be turned on based on the visibility condition.
   -- It evaluates the Visibility attribute of the Sensor object.
   -- the function updates the visibility condition to night and updates the headlights status.
   function Should_Turn_On_Headlights(S : in Sensor) return Boolean is
   begin
      return S.Visibility = Night;
   end Should_Turn_On_Headlights;

   -- This procedure updates the headlights status based on the visibility condition.
   -- It evaluates the Visibility attribute of the Sensor object.
   -- It prints a message indicating whether the headlights are turned on or off.
   -- The procedure takes a Sensor object as an input parameter and modifies its status.
   -- The procedure does not return any value.
   -- The procedure uses the Should_Turn_On_Headlights function to determine if the headlights should be turned on.
   -- If the visibility condition is night, it turns on the headlights and prints "Sensor: Headlights turned ON due to low light".
   -- If the visibility condition is day, it turns off the headlights and prints "Headlights turn Off; visibility is sufficient".
   procedure Update_Headlights(S : in out Sensor) is 
   begin
      if Should_Turn_On_Headlights(S) then
         S.Headlights_On := True;
         Put_Line("Sensor: Headlights turned ON due to low light.");
      else
         S.Headlights_On := False;
         Put_Line("Headlights turn Off; visibility is suficient.");
      end if;
   end Update_Headlights;

   -- This procedure handles object detection while the vehicle is in motion.
   -- It evaluates the Engine_On and Current_Speed attributes of the Vehicle object.
   -- It prints a message indicating whether the vehicle is moving or not.
   -- The procedure takes a Vehicle object and the current speed as input parameters.
   -- The procedure modifies the Vehicle object and does not return any value.
   -- The procedure uses the Engine_On attribute to determine if the engine is on.
   -- If the engine is on and the current speed is greater than the threshold, it decreases the speed.
   -- It also prints "Sensor: Object detected while vehicle is moving, SLOWING DOWN".
   -- If the engine is off or the current speed is less than or equal to the threshold, it stops the vehicle.
   -- It also prints "Sensor: Deaccelerating Vehicle".
   -- The procedure does not return any value.
   procedure Handle_Object_Detection(V : in out Vehicle_System.Vehicle; Current_Speed : in Float) is
   -- Example threshold for object detection
   begin
      if V.Engine_On and Current_Speed > Vehicle_Constants.Threshold then
         V.Set_Speed(V.Speed - Vehicle_Constants.Threshold); -- Example: decrease speed if object detected
      -- Placeholder for object detection logic
         Put_Line("Sensor: Object detected while vehicle is moving, SLOWING DOWN.");
      else
         Put_Line("Sensor: Deaccelerating Vehicle.");
         V.Is_Moving := False; -- Example: stop the vehicle if object detected
         V.Set_Speed(0.0); -- Example: stop the vehicle if object detected
      end if;

      -- Implement logic to handle object detection
   end Handle_Object_Detection;


end Sensor_System;