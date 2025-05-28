with Ada.Text_IO; use Ada.Text_IO;
with Sensor_System, Vehicle_System;
with Vehicle_Types;use Vehicle_Types;
with Radar_Systems; use Radar_Systems;
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

   -- Procedure to check the door status  
   -- This procedure updates the door status of the luxury car
   -- It takes a Luxury_Car object as input and checks the door status
   -- The procedure uses the Sensor_System package to access the door status
   -- The procedure updates the Current_Door_Status attribute of the Luxury_Car object
   -- The procedure prints the door status to the console
   -- The procedure is called when the vehicle is started or when the door status changes
   procedure Update_Door_Status(Standard_Car : in out Standard) is
   begin
      if Sensor_System.Is_Door_Open(Standard_Car.Car_Sensor) then
         Standard_Car.Current_Door_Status := Vehicle_System.Door_Open;
         Put_Line("vehicle door is open");
      else
         Standard_Car.Current_Door_Status := Vehicle_System.Door_Closed;
         Put_Line("Door is closed");
      end if;      
   end Update_Door_Status;

   procedure Attempt_Move(Standard_Car : in out Standard; Threshold : Float) is
   begin
   -- Simulate door toggle
   Sensor_System.Toggle_Door(Standard_Car.Car_Sensor);
   Sensor_System.Toggle_Door(Standard_Car.Car_Sensor); 

   -- Start engine
   Vehicle_System.Start_Engine(Vehicle_System.Vehicle(Standard_Car));
   Sensor_System.Activate_Sensor(Standard_Car.Car_Sensor);
   Update_Door_Status(Standard_Car);

   -- Check conditions
   declare
      Door_Closed  : Boolean := is_Door_Closed(Standard_Car);
      Occupied     : Boolean := Sensor_System.Seat_Occupied(Standard_Car.Car_Sensor);
      Seatbelt_On  : Boolean := Standard_Car.Car_Sensor.Seatbelt_On;
      Clear_Path   : Boolean := Radar_Systems.Is_Clear_To_Move(Standard_Car.Car_Radar, Threshold);
   begin

      -- Visibility check (applies in both modes)
      Sensor_System.Check_Visibility(Standard_Car.Car_Sensor);

      if not Door_Closed then
         Put_Line("Warning: Door is open.");
      end if;
      if not Occupied then
         Put_Line("Warning: Seat not occupied.");
      end if;
      if not Seatbelt_On then
         Put_Line("Warning: Seatbelt is not fastened.");
      end if;
      if not Clear_Path then
         Put_Line("Warning: Obstacle detected.");
      end if;
        
            delay 3.0;
            Put_Line("Standard: Vehicle is in motion regardless of above warnings.");
      end;

      -- Final movement decision
      if Vehicle_Mobile(Standard_Car) then
         Standard_Car.Is_Moving := True;
         Put_Line("Vehicle is moving.");
      else
         Standard_Car.Speed := 0.0;
         Standard_Car.Is_Moving := False;
         Put_Line("Vehicle failed to start moving.");
      end if;
   
   end Attempt_Move;


   

end Standard_Vehicle;