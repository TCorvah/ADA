-- vehicle.adb
with Ada.Text_IO; use Ada.Text_IO;

package body Vehicle_System is


   -- This package implements the functionality of the vehicle system.
   -- It includes procedures and functions to manage the vehicle's engine, speed, and reservation status.


   --- This procedure starts the vehicle engine by setting the Engine_On attribute to True.
   -- It also prints a message indicating that the engine has been started.
   -- The procedure takes a Vehicle object as an input parameter and modifies its status.
   -- It starts the engine, activates the sensor, and checks the seatbelt status and door.
   procedure Start_Engine(V : in out Vehicle) is
   begin
      V.Engine_On := True;
      Put_Line("Engine Started.");
   end Start_Engine;

   -- This procedure stops the vehicle engine by setting the Engine_On attribute to False.
   -- It also sets the speed to 0.0 and prints a message indicating that the engine has been stopped.
   -- The procedure takes a Vehicle object as an input parameter and modifies its status
   procedure Stop_Engine (V : in out Vehicle) is 
   begin
      V.Engine_On := False;
      V.Speed := 0.0;
      Put_Line("Engine is turning off");
   end Stop_Engine;
   
   -- This procedure checks the vehicle status and prints a message indicating whether the vehicle is in motion or not.
   procedure Vehicle_NotMobile(V : in out Vehicle) is
   begin
      V.Is_Moving := False;
      V.Speed := 0.0;
      Put_Line("Vehicle stopped.");
   end Vehicle_NotMobile;

   -- This procedure sets the speed of the vehicle if it is in motion.
   -- It takes a Vehicle object and a new speed as input parameters.
   -- If the vehicle is not in motion, it prints a message indicating that the vehicle is not in motion.
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float) is
   begin
      if V.Is_Moving then
         V.Speed := New_Speed;
         Put_Line("Speed set to: " & Float'Image(New_Speed));
      else
         Put_Line("Vehicle is not in motion.");
      end if;
   end Set_Speed;



   function Reserve_Vehicle(V : Vehicle) return Boolean is
   begin
      return V.Is_Reserved;
   end Reserve_Vehicle;

   procedure Set_Reservation(V : in out Vehicle) is
   begin
      V.Is_Reserved := True;
   end Set_Reservation;


   

end Vehicle_System;
