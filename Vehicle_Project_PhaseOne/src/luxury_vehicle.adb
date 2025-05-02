with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Radar_Systems, Sensor_System, Vehicle_System;
use Vehicle_System;
package body  Luxury_Vehicle is

   -- Function to check if the door is closed         
   -- The function takes a Luxury_Car object as input
   -- and returns true if the door is closed, false otherwise
   -- The function checks the current door status of the car
   -- The function uses the Vehicle_System package to access the door status
   function is_Door_Closed(Lux_Car : in out  Luxury_Car) return Boolean is
   begin
      return Lux_Car.Current_Door_Status = Door_Closed;         
   end is_Door_Closed;

   -- Function to check if the vehicle is mobile
   -- A vehicle is considered mobile if the engine is on, speed is greater than 0,
   -- the door is closed, and the seatbelt is fastened
   -- The function returns true if the vehicle is mobile, false otherwise
   -- The function checks the engine status, speed, door status, and seatbelt status
   function Vehicle_Mobile(Lux_Car : in out Luxury_Car) return Boolean is
   begin
      return Lux_Car.Engine_On and Lux_Car.Speed > 0.0 and Lux_Car.Current_Door_Status = Door_Closed
             and Lux_Car.Car_Sensor.Seatbelt_On;
   end Vehicle_Mobile;
   

   -- Procedure to check the door status  
   -- This procedure updates the door status of the luxury car
   -- It takes a Luxury_Car object as input and checks the door status
   -- The procedure uses the Sensor_System package to access the door status
   -- The procedure updates the Current_Door_Status attribute of the Luxury_Car object
   -- The procedure prints the door status to the console
   -- The procedure is called when the vehicle is started or when the door status changes
   procedure Update_Door_Status(Lux_Car : in out Luxury_Car) is
   begin
      if Sensor_System.Is_Door_Open(Lux_Car.Car_Sensor) then
         Lux_Car.Current_Door_Status := Vehicle_System.Door_Open;
         Put_Line("Door is open");
      else
         Lux_Car.Current_Door_Status := Vehicle_System.Door_Closed;
         Put_Line("Door is closed");
      end if;      
   end Update_Door_Status;

   --- Procedure to check if the vehicle can move
   -- This procedure checks if the vehicle can move based on the door status, seatbelt status, and object detection
   -- It takes a Luxury_Car object and a threshold value as input
   -- The procedure uses the Sensor_System package to check the door status and seatbelt status
   -- The procedure uses the Radar_Systems package to check for objects in the vehicle's path
   -- The procedure prints the status of the vehicle to the console
   -- The procedure is called when the vehicle is started or when the door status changes
   procedure Attempt_Move(Lux_Car : in out Luxury_Car; Threshold : Float) is
   begin
      Sensor_System.Toggle_Door(Lux_Car.Car_Sensor);
      Sensor_System.Toggle_Door(Lux_Car.Car_Sensor); 
      -- Start the engine
      Vehicle_System.Start_Engine(Vehicle_System.Vehicle(Lux_Car));
   
      -- activate Sensor
      Sensor_System.Activate_Sensor(Lux_Car.Car_Sensor);   

      -- update door status
      Update_Door_Status(Lux_Car);

      -- check if all doors are close
      if is_Door_Closed(Lux_Car) then
         -- check if the vehicle is occupied
         if Sensor_System.Seat_Occupied(Lux_Car.Car_Sensor) then
            Put_Line("Vehicle is occupied");
         else
            Put_Line("Vehicle is not occupied");
         end if;
         
         -- check if visibility is good
         Sensor_System.Check_Visibility(Lux_Car.Car_Sensor);
      
      -- check if seatbelt fasten
         if Sensor_System.Seat_Occupied(Lux_Car.Car_Sensor) then
            Put_Line ("Sensor: Seat is detected with (weight: " & Float 'Image(Lux_Car.Car_Sensor.Detected_Weight) & "kg).");
            Sensor_System.Check_Seatbelt(Lux_Car.Car_Sensor); 
         else
            Put_Line("Seatbelt is not fastened");
         end if;     
            Radar_Systems.Detect_Object(Lux_Car.Car_Radar, Threshold);
         if Radar_Systems.Is_Clear_To_Move(Lux_Car.Car_Radar,Threshold ) then  
            -- Check if the vehicle is mobile 
            if Vehicle_Mobile(Lux_Car) then
               Put_Line("Vehicle is moving");
            else
               Lux_Car.Speed := 0.0;
               Lux_Car.Is_Moving := False;
               Put_Line("Vehicle is not moving");
            end if;   
            Lux_Car.Is_Moving := True;
            Put_Line(" Vehicle is now safe to move");
         end if;
      else
      Put_Line("Cannot move.");
      end if;
   end Attempt_Move;

   -- Procedure to enable object detection
   -- This procedure enables object detection using the radar system
   -- It takes a Luxury_Car object and a threshold value as input
   -- The procedure uses the Radar_Systems package to enable object detection
   -- The procedure updates the Car_Radar attribute of the Luxury_Car object
   -- The procedure is called when the vehicle is started or when the radar system is activated
   -- The procedure prints the status of the radar system to the console
   -- The procedure is called when the vehicle is started or when the radar system is activated
   procedure Enable_Object_Detection(Lux_Car : in out Luxury_Car; Object_Threshold : in Float) is
   Radars : Radar_Systems.Radar := Lux_Car.Car_Radar;
   begin
      Radar_Systems.Detect_Object(Radars, Object_Threshold);
      Lux_Car.Car_Radar := Radars;  
   end Enable_Object_Detection;

   -- Procedure to reduce speed based on object detection
   -- This procedure uses the radar system to detect objects and adjust speed accordingly
   -- It takes the current speed as input and uses the radar system to determine if the vehicle should slow down
   -- The procedure is called when the vehicle is in motion and an object is detected within a certain range
   procedure Reduce_Speed(Lux_Car : in out Luxury_Car; Current_Speed : in Float) is
   begin
      Sensor_System.Handle_Object_Detection(Vehicle_System.Vehicle(Lux_Car),Current_Speed );
   end Reduce_Speed;

   -- Procedure to turn off the engine and deactivate the sensor
   -- This procedure stops the engine and deactivates the sensor system
   -- It takes a Luxury_Car object as input and uses the Vehicle_System package to stop the engine
   -- The procedure uses the Sensor_System package to deactivate the sensor
   -- The procedure updates the Engine_On attribute of the Luxury_Car object
   -- The procedure is called when the vehicle is turned off or when the engine is stopped
   procedure Turn_Off_Engine(Lux_Car : in out Luxury_Car) is
   begin
      Vehicle_System.Stop_Engine(Vehicle_System.Vehicle(Lux_Car));
      Sensor_System.Deactivate_Sensor(Lux_Car.Car_Sensor);
      Radar_Systems.Deactivate_Radar(Lux_Car.Car_Radar);
   end Turn_Off_Engine;


end Luxury_Vehicle;