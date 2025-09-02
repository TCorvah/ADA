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
  overriding function Vehicle_Mobile(V : Standard) return Boolean is
   Engine_Status : Boolean := Get_Engine_Status(V);
   Speed : Float := Get_Speed(V);
   begin
     return Engine_Status and then (Speed > 0.0);
   end Vehicle_Mobile;


   -- this function checks if the door is closed by evaluating the Current_Door_Status attribute of the Vehicle object.
   function is_Door_Closed(V : Standard) return Boolean is
   begin
      return not Sensor_System.Is_Door_Open(V.Car_Sensor);
   end is_Door_Closed;

   -- Procedure to check the door status  
   -- This procedure updates the door status of the luxury car
   -- It takes a Luxury_Car object as input and checks the door status
   -- The procedure uses the Sensor_System package to access the door status
   -- The procedure updates the Current_Door_Status attribute of the Luxury_Car object
   -- The procedure prints the door status to the console
   -- The procedure is called when the vehicle is started or when the door status changes
   


   overriding
   function Get_Door_Status (V : Standard) return Vehicle_System.Door_Status_Type is
   begin
      if Is_Door_Closed (V) then
         return Vehicle_System.Door_Closed;
      else
         return Vehicle_System.Door_Open;
      end if;
   end Get_Door_Status;


   procedure Update_Door_Status(V : in out Standard) is
    Status : Vehicle_System.Door_Status_Type := Get_Door_Status(V);
   begin
       Vehicle_System.Set_Door_Status(Vehicle_System.Vehicle(V), Status);
       case Status is
          when Vehicle_System.Door_Closed => Put_Line ("Door is closed.");
          when Vehicle_System.Door_Open => Put_Line ("Door is open.");
       end case;
   end Update_Door_Status;

   procedure Attempt_Move(Standard_Car : in out Standard; Threshold : Float) is
      Door_Closed  : Boolean := is_Door_Closed(Standard_Car);
      Occupied     : Boolean := Sensor_System.Seat_Occupied(Standard_Car.Car_Sensor);
      Seatbelt_On  : Boolean := Standard_Car.Car_Sensor.Seatbelt_On;
      Speed : Float := Vehicle_System.Get_Speed(Vehicle_System.Vehicle(Standard_Car));
   begin
     Put_Line ("[Standard ] attempt Move begin");
              
      -- Simulate door toggle
      Sensor_System.Toggle_Door(Standard_Car.Car_Sensor);
      Sensor_System.Toggle_Door(Standard_Car.Car_Sensor); 

      -- Start engine
      Vehicle_System.Start_Engine(Vehicle_System.Vehicle(Standard_Car));
      Sensor_System.Activate_Sensor(Standard_Car.Car_Sensor);

      -- Enviromemnt Sensing
      -- Visibility check (applies in both modes)
      Sensor_System.Check_Visibility(Standard_Car.Car_Sensor);
      Update_Door_Status(Standard_Car);
 

   -- Check conditions

      if not Door_Closed then
        Put_Line("Warning: Door is open.");

        
      end if;
      if not Occupied then
         Put_Line("Warning: Seat not occupied.");
      end if;
      if not Seatbelt_On then
         Put_Line("Warning: Seatbelt is not fastened.");
      end if;
      Sensor_System.Check_Visibility(Standard_Car.Car_Sensor);
   
     
        
      delay 3.0;
      Put_Line("Standard: Vehicle is in motion regardless of above warnings.");
      -- Set speed to a safe value if all conditions are met
      Vehicle_System.Set_Speed(Vehicle_System.Vehicle(Standard_Car), Standard_Car.Min_Speed);
      
      -- Final movement decision
      if Vehicle_Mobile(Standard_Car) then
         if Speed >=  Vehicle_Constants.Vehicle_Min_Speed then
            Put_Line("Vehicle is moving.");
         end if;
      else
         if Speed < Vehicle_Constants.Vehicle_Min_Speed then
            Put_Line("Warning: Speed is below minimum threshold.");
            Vehicle_System.Vehicle_NotMobile(Vehicle_System.Vehicle(Standard_Car));
            Put_Line("Vehicle failed to start moving.");
         else
            Put_Line("Vehicle failed to start moving.");
         end if;       
        
      end if;
   
   end Attempt_Move;


   

end Standard_Vehicle;