with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle;
with Radar_Systems;
with Sensor_System;
with Vehicle_System;
use Vehicle_System;
package body  Luxury_Vehicle is



   function is_Door_Closed(Lux_Car : in out  Luxury_Car) return Boolean is
   begin
      return Lux_Car.Current_Door_Status = Door_Closed;         
   end is_Door_Closed;

   function Vehicle_Mobile(Lux_Car : in out Luxury_Car) return Boolean is
   begin
      return Lux_Car.Engine_On and Lux_Car.Speed > 0.0 and Lux_Car.Current_Door_Status = Door_Closed
             and Lux_Car.Car_Sensor.Seatbelt_On;
   end Vehicle_Mobile;

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



   procedure Attempt_Move(Lux_Car : in out Luxury_Car) is
   Threshold : constant Float := 2.0;
   begin
      -- Start the engine
      Vehicle_System.Start_Engine(Vehicle_System.Vehicle(Lux_Car));
   

      -- activate Sensor
      Sensor_System.Activate_Sensor(Lux_Car.Car_Sensor);   
      -- activate radar
      Radar_Systems.Activate_Radar(Lux_Car.Car_Radar);

      -- update door status
      Update_Door_Status(Lux_Car);

      -- check if all doors are close
      if is_Door_Closed(Lux_Car) then

      -- check if seatbelt fasten
         Sensor_System.Check_Seatbelt(Lux_Car.Car_Sensor); 
         Radar_Systems.Detect_Object(Lux_Car.Car_Radar, Threshold);
         if Radar_Systems.Is_Clear_To_Move(Lux_Car.Car_Radar,Threshold ) then  
            -- Check if the vehicle is mobile 
            if Vehicle_Mobile(Lux_Car) then
               Put_Line("Vehicle is moving");
            else
               Put_Line("Vehicle is not moving");
            end if;   
            Lux_Car.Is_Moving := True;
            Put_Line(" Vehicle is now safe to move");
         end if;
      else
      Put_Line("Cannot move.");
         -- Optionally, engage parking brake or alert the driver
      end if;
   end Attempt_Move;

   
   procedure Enable_Object_Detection(Lux_Car : in out Luxury_Car) is
   Object_Threshold : constant Float := 2.0;
   begin
      Radar_Systems.Detect_Object(Lux_Car.Car_Radar,Object_Threshold);
   end Enable_Object_Detection;


   function Reduce_Speed(Lux_Car : in Luxury_Car; Current_Speed : in Integer) return Integer is
   begin
      return Radar_Systems.Adjust_Speed(Lux_Car.Car_Radar,Current_Speed );
   end Reduce_Speed;


end Luxury_Vehicle;