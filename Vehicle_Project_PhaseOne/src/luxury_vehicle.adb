with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Radar_Systems, Sensor_System, Vehicle_System;
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
         -- Optionally, engage parking brake or alert the driver
      end if;
   end Attempt_Move;

   
   procedure Enable_Object_Detection(Lux_Car : in out Luxury_Car; Object_Threshold : in Float) is
   Radars : Radar_Systems.Radar := Lux_Car.Car_Radar;
   begin
      Radar_Systems.Detect_Object(Radars, Object_Threshold);
      Lux_Car.Car_Radar := Radars;  
   end Enable_Object_Detection;


   procedure Reduce_Speed(Lux_Car : in out Luxury_Car; Current_Speed : in Float) is
   begin
      Sensor_System.Handle_Object_Detection(Vehicle_System.Vehicle(Lux_Car),Current_Speed );
   end Reduce_Speed;

   procedure Turn_Off_Engine(Lux_Car : in out Luxury_Car) is
   begin
      Vehicle_System.Stop_Engine(Vehicle_System.Vehicle(Lux_Car));

      Sensor_System.Deactivate_Sensor(Lux_Car.Car_Sensor);
      Radar_Systems.Deactivate_Radar(Lux_Car.Car_Radar);
   end Turn_Off_Engine;


end Luxury_Vehicle;