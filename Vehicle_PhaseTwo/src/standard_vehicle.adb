with Ada.Text_IO; use Ada.Text_IO;
with Sensor_System, Vehicle_System;
use Vehicle_System;

package body Standard_Vehicle is 

   -- This package implements the functionality of the standard vehicle.
   -- It includes procedures and functions to manage the vehicle's engine, speed, and sensor system.
   -- The package provides a set of procedures to start/stop the engine, check seatbelt status,
   -- and manage the vehicle's headlights based on visibility conditions.
   -- The package also includes a procedure to handle object detection while the vehicle is in motion.
   -- The package is designed to be used in conjunction with the Sensor_System package.
   -- The Standard type is defined in the Vehicle_Constants package and includes attributes such as
   -- engine status, speed, and sensor system.
   -- The package provides a set of procedures to manage the vehicle system and ensure the vehicle's safety.
   -- The package also includes a procedure to handle object detection while the vehicle is in motion.
   -- The package is designed to be used in conjunction with the Vehicle_System package.
   -- The Standard type is defined in the Vehicle_Constants package and includes attributes such as
   -- engine status, speed, and sensor system.


   -- This procedure starts the vehicle engine by setting the Engine_On attribute to True.
   -- It also prints a message indicating that the engine has been started.
   -- The procedure takes a Standard object as an input parameter and modifies its status.
   -- It starts the engine, activate the sensor, and checks the seatbelt status and door.
   procedure SeatBelt_Warning(V : in out  Standard) is
   begin
      Vehicle_System.Start_Engine(Vehicle_System.Vehicle(V));
      Sensor_System.Activate_Sensor(V.Car_Sensor);
      Sensor_System.Check_Seat(V.Car_Sensor);
      if Sensor_System.Is_Door_Open(V.Car_Sensor) then
         Put_Line(" Door is open");
      else
         Put_Line(" Door is closed");
      end if;
      if Sensor_System.Seat_Occupied(V.Car_Sensor) and then not V.Car_Sensor.Seatbelt_On then 
         Put_Line(" seatbelt is not on");
      end if;  
   end SeatBelt_Warning;

   
   -- This procedure activates the sensor system by setting its status to "On".
   -- It also checks the door status and prints a message indicating the door status.
   procedure Check_Sensors(V : in out Standard) is   
   begin
      Sensor_System.Activate_Sensor(V.Car_Sensor);
      if Sensor_System.Is_Door_Open(V.Car_Sensor) then
         Put_Line(" Door is open");
      else
         Put_Line(" Door is closed");
      end if;
   end Check_Sensors;

   -- This Procedure checks the vehicle status is it in motion or not
   -- It also prints a message indicating the vehicle status
   function Vehicle_Mobile(V : in out Standard) return Boolean is
   begin
     return V.Engine_On and V.Speed > 0.0;
   end Vehicle_Mobile;


   -- this function checks if the door is closed by evaluating the Current_Door_Status attribute of the Vehicle object.
   function is_Door_Closed(V : in out Standard) return Boolean is
   begin
      return V.Current_Door_Status = Vehicle_System.Door_Closed;
   end is_Door_Closed;
   

end Standard_Vehicle;