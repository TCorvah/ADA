with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Radar_Systems, Sensor_System, Vehicle_System;
use Vehicle_System;  use Radar_Systems;  use Sensor_System;
with Vehicle_Types;use Vehicle_Types;
with Road_ProfileConfig; use Road_ProfileConfig;
with Radar_Systems; use Radar_Systems;
with Vehicle_Types;
package body  Luxury_Vehicle is

   -- Function to check if the door is closed         
   -- The function takes a Luxury_Car object as input
   -- and returns true if the door is closed, false otherwise
   -- The function checks the current door status of the car
   -- The function uses the Vehicle_System package to access the door status
   function is_Door_Closed(V : Luxury_Car) return Boolean is
   begin
      return not Sensor_System.Is_Door_Open(V.Car_Sensor);     
   end is_Door_Closed;

   

    overriding
   function Get_Door_Status (V : Luxury_Car) return Vehicle_System.Door_Status_Type is
   begin
      if Is_Door_Closed (V) then
         return Vehicle_System.Door_Closed;
      else
         return Vehicle_System.Door_Open;
      end if;
   end Get_Door_Status;

   -- Function to check if the vehicle is mobile
   -- A vehicle is considered mobile if the engine is on, speed is greater than 0,
   -- the door is closed, and the seatbelt is fastened
   -- The function returns true if the vehicle is mobile, false otherwise
   -- The function checks the engine status, speed, door status, and seatbelt status
   overriding function Vehicle_Mobile(V : Luxury_Car) return Boolean is
   begin
      Sensor_System.Check_Seat(V.Car_Sensor);
      return
       Get_Engine_Status(V) and then
       Get_Door_Status(V) = Door_Closed and then
      (Get_Speed(V) > 0.0);
                
   end Vehicle_Mobile;
   


   -- Procedure to check the door status  
   -- This procedure updates the door status of the luxury car
   -- It takes a Luxury_Car object as input and checks the door status
   -- The procedure uses the Sensor_System package to access the door status
   -- The procedure updates the Current_Door_Status attribute of the Luxury_Car object
   -- The procedure prints the door status to the console
   -- The procedure is called when the vehicle is started or when the door status changes
   procedure Update_Door_Status(V : in out Luxury_Car) is
    Status : Vehicle_System.Door_Status_Type := Get_Door_Status(V);
   begin
       Vehicle_System.Set_Door_Status(Vehicle_System.Vehicle(V), Status);
       case Status is
          when Vehicle_System.Door_Closed => Put_Line ("Door is closed.");
          when Vehicle_System.Door_Open => Put_Line ("Door is open.");
       end case;
   end Update_Door_Status;

   --- Procedure to check if the vehicle can move
   -- This procedure checks if the vehicle can move based on the door status, seatbelt status, and object detection
   -- It takes a Luxury_Car object and a threshold value as input
   -- The procedure uses the Sensor_System package to check the door status and seatbelt status
   -- The procedure uses the Radar_Systems package to check for objects in the vehicle's path
   -- The procedure prints the status of the vehicle to the console
   -- The procedure is called when the vehicle is started or when the door status changes

   procedure Attempt_Move(V : in out Luxury_Car; Threshold : Float) is
   begin
   -- Simulate door toggle
   Sensor_System.Toggle_Door(V.Car_Sensor);
   Sensor_System.Toggle_Door(V.Car_Sensor); 

   -- Start engine
   Vehicle_System.Start_Engine(Vehicle_System.Vehicle(V));
   Sensor_System.Activate_Sensor(V.Car_Sensor);
   Update_Door_Status(V);

   -- Check conditions
   declare
      Door_Closed  : Boolean := is_Door_Closed(V);
      Occupied     : Boolean := Sensor_System.Seat_Occupied(V.Car_Sensor);
      Seatbelt_On  : Boolean := V.Car_Sensor.Seatbelt_On;
      Clear_Path   : Boolean := Radar_Systems.Is_Clear_To_Move(V.Car_Radar, Threshold);
      --Sensor_Check_Seat : Boolean := Sensor_System.Check_Seat(V.Car_Sensor);
   begin

      -- Visibility check (applies in both modes)
      Sensor_System.Check_Visibility(V.Car_Sensor);
      Radar_Systems.Activate_Radar(V.Car_Radar);

      if is_Door_Closed(V) then
         Put_Line("Door is closed.");
      else
          Put_Line("Warning: Door is open. Vehicle will not move.");
          Vehicle_NotMobile(Vehicle_System.Vehicle(V));
        
         return;
      end if;
      if not Occupied then
         Put_Line("Warning: Seat not occupied. Vehicle will not move.");
         Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(V));
         return;
      end if;
      if Occupied and not Seatbelt_On then
         Put_Line("Warning: Seatbelt is not fastened. Vehicle will not move.");
         Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(V));
         return;
      end if;
      if not Clear_Path then
         Put_Line("Warning: Obstacle detected too close. Vehicle will not move.");  
         Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(V));
         return;
      end if;

      Vehicle_System.Set_Speed(Vehicle_System.Vehicle(V), V.Min_Speed);
      Put_Line("All conditions met. Vehicle is attempting to move.");
     
      -- Final movement decision
      if Vehicle_Mobile(V) then
         Put_Line("Vehicle is moving.");
      else
        Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(V));
         Put_Line("Vehicle failed to start moving.");
      end if;
      Put_Line ("[Luxury Car Status] Engine On: " & Boolean'Image(Get_Engine_Status(V)) &
                ", Speed: " & Float'Image(Get_Speed(V)) &
                ", Door Status: " & Vehicle_System.Door_Status_Type'Image(Get_Door_Status(V)) &
                ", Is Moving: " & Boolean'Image(Get_Is_Moving(V)));
   end;
end Attempt_Move;




   -- Procedure to enable object detection
   -- This procedure enables object detection using the radar system
   -- It takes a Luxury_Car object and a threshold value as input
   -- The procedure uses the Radar_Systems package to enable object detection
   -- The procedure updates the Car_Radar attribute of the Luxury_Car object
   -- The procedure is called when the vehicle is started or when the radar system is activated
   -- The procedure prints the status of the radar system to the console
   -- The procedure is called when the vehicle is started or when the radar system is activated
   --procedure Enable_Object_Detection(V : in out Luxury_Car) is
   --Object_Threshold : constant Float := 10.0; -- Example threshold for object detection
   --begin
      --Radar_Systems.Detect_Object(Radars, Object_Threshold);
   --   Put_Line("Object detection enabled.");
   --   Radar_Scan_Garage_Simulation(Road_ProfileConfig.Road_Profile);
   --   if Vehicle_Mobile(V, Radar_Systems.Is_Clear_To_Move(V.Car_Radar, Object_Threshold)) then
   --      Put_Line("Vehicle is mobile and driving to destination.");
   --   else
   --      Put_Line("Vehicle is not mobile, but object detection is enabled.");
   --      Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(V));
   --   end if;

   --end Enable_Object_Detection;

   -- Procedure to reduce speed based on object detection
   -- This procedure uses the radar system to detect objects and adjust speed accordingly
   -- It takes the current speed as input and uses the radar system to determine if the vehicle should slow down
   -- The procedure is called when the vehicle is in motion and an object is detected within a certain range
   --procedure Reduce_Speed(Lux_Car : in out Luxury_Car; Current_Speed : in Float) is
   --begin
      --Sensor_System.Handle_Object_Detection(Vehicle_System.Vehicle(Lux_Car),Current_Speed );
   --end Reduce_Speed;

   -- Procedure to turn off the engine and deactivate the sensor
   -- This procedure stops the engine and deactivates the sensor system
   -- It takes a Luxury_Car object as input and uses the Vehicle_System package to stop the engine
   -- The procedure uses the Sensor_System package to deactivate the sensor
   -- The procedure updates the Engine_On attribute of the Luxury_Car object
   -- The procedure is called when the vehicle is turned off or when the engine is stopped
   procedure Turn_Off_Engine(V : in out Luxury_Car) is
   begin
      Vehicle_System.Stop_Engine(Vehicle_System.Vehicle(V));
      Sensor_System.Deactivate_Sensor(V.Car_Sensor);
      Radar_Systems.Deactivate_Radar(V.Car_Radar);
   end Turn_Off_Engine;


end Luxury_Vehicle;