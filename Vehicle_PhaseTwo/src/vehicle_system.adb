-- vehicle.adb
with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle;


-- This package implements the functionality of the vehicle system.
-- It includes procedures and functions to manage the vehicle's engine, speed, and reservation status.
package body Vehicle_System is


   function Get_Vehicle_Type(V : Vehicle) return Vehicle_CarType is
   begin
      if V.Vehicle_Type = Vehicle_CarType.Luxury_Car then
         return V.Vehicle_Type;
      else
         V.Vehicle_Type := Vehicle_CarType.Standard_Car;
         return V.Vehicle_Type;
      end if;
      return V.Vehicle_Type;
   end Get_Vehicle_Type;   

   --- This procedure starts the vehicle engine by setting the Engine_On attribute to True.
   -- It also prints a message indicating that the engine has been started.
   -- The procedure takes a Vehicle object as an input parameter and modifies its status.
   -- It starts the engine of the vehicle.
   procedure Start_Engine(V : in out Vehicle) is
   begin
      V.Engine_Status := Engine_On;
   end Start_Engine;

   -- This procedure stops the vehicle engine by setting the Engine_On attribute to False.
   -- It also sets the speed to 0.0 and prints a message indicating that the engine has been stopped.
   -- The procedure takes a Vehicle object as an input parameter and modifies its status
   procedure Stop_Engine (V : in out Vehicle) is 
   begin
      V.Engine_Status := Engine_Off;
      Vehicle_NotMobile(V);
   end Stop_Engine;

   -- This function returns the current status of the vehicle engine.
   -- It takes a Vehicle object as an input parameter and returns a Boolean value indicating whether the engine is on or off.
   -- The function is used to check the status of the vehicle engine
   function Get_Engine_Status(V : Vehicle) return Engine_Status_Type is
   begin
   if V.Engine_Status = Engine_Status_Type.Engine_On then
      return V.Engine_Status;
   else
       V.Engine_Status := Engine_Status_Type.Engine_Off;
      return V.Engine_Status;
   end if;
   end Get_Engine_Status;

   -- This procedure sets the speed of the vehicle if it is in motion.
   -- It takes a Vehicle object and a new speed as input parameters.
   -- If the vehicle is not in motion, it prints a message indicating that the vehicle is not in motion.
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float) is
   begin
      --if New_Speed < V.Current_Road.Min_Speed or else New_Speed > V.Current_Road.Max_Speed then
      if Get_Engine_Status = Engine_Status_Type.Engine_Off then
         Vehicle_NotMobile(V);
         return;
       end if;
       if Get_Engine_Status = Engine_Status_Type.Engine_On and Vehicle_Mobile then
          V.Speed := new_Speed; 
       end if;
   end Set_Speed;

   -- This function returns the current speed of the vehicle.
   function Get_Speed return Float is
   begin
      return V.Speed;
   end Get_Speed;
   
   -- This procedure checks the vehicle status and prints a 
   --  message indicating whether the vehicle is in motion or not.
   procedure Vehicle_NotMobile(V : in out Vehicle) is
   begin
      V.Is_Moving := False;
      V.Speed := 0.0;
   end Vehicle_NotMobile;

   function Get_Seatbelt_Status(V : Vehicle) return SeatBelt_Status_Type is
   begin
      if V.SeatBelt_Status = SeatBelt_On then
         return V.SeatBelt_Status;
      else
         V.SeatBelt_Status := SeatBelt_Off;
         return V.SeatBelt_Status;
      end if;
   end Get_Seatbelt_Status;


   -- might remove this function later
   procedure Set_Reservation(V : in out Vehicle) is
   begin
      V.Is_Reserved := True;
   end Set_Reservation;


   -- might remove this function later
   function Reserve_Vehicle(V : Vehicle) return Boolean is
   begin
      return V.Is_Reserved;
   end Reserve_Vehicle;

 
   -- sets the door status of the vehicle and prints a message indicating the new status.
   procedure Set_Door_Status(V : in out Vehicle; Status : Door_Status_Type) is
   begin
      V.Current_Door_Status := Status;
      if Status = Door_Open then
         Put_Line("Door is now open.");
      else
         Put_Line("Door is now closed.");
      end if;
   end Set_Door_Status;                   
     

   -- This procedure sets the current status of the vehicle engine
   procedure Set_Engine_Status(V : in out Vehicle; On : Engine_Status_Type) is
   begin
      V.Engine_Status := On;
      if On = Engine_On then
         Start_Engine(V);
      else 
         V.Engine_Status := Engine_Off;
         Vehicle_NotMobile(V);      
      end if;
   end Set_Engine_Status;

   -- This function returns the current status of the vehicle engine.
   function Get_Is_Moving return Boolean is
   begin
      return Vehicle_Mobile;
   end Get_Is_Moving;

end Vehicle_System;
