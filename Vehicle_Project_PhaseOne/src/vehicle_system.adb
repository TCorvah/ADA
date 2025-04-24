-- vehicle.adb
with Ada.Text_IO; use Ada.Text_IO;

package body Vehicle_System is

   procedure Start_Engine(V : in out Vehicle) is
   begin
      V.Engine_On := True;
      Put_Line("Engine Started.");
   end Start_Engine;

   procedure Stop_Engine (V : in out Vehicle) is 
   begin
      V.Engine_On := False;
      V.Speed := 0.0;
      Put_Line("Engine is turning off");
   end Stop_Engine;
   

   procedure Vehicle_NotMobile(V : in out Vehicle) is
   begin
      V.Is_Moving := False;
      V.Speed := 0.0;
      Put_Line("Vehicle stopped.");
   end Vehicle_NotMobile;

   procedure Set_Speed(V : in out Vehicle; New_Speed : Float) is
   begin
      if V.Is_Moving then
         V.Speed := New_Speed;
         Put_Line("Speed set to: " & Float'Image(New_Speed));
      else
         Put_Line("Vehicle is not in motion.");
      end if;
   end Set_Speed;

   procedure Vehicle_Mobile(V : in out Vehicle) is
   begin
      if V.Speed > 0.0 then
         V.Is_Moving := True;
         Put_Line("Vehicle is in motion.");
      end if;
   end Vehicle_Mobile;

   function Reserve_Vehicle(V : Vehicle) return Boolean is
   begin
      return V.Is_Reserved;
   end Reserve_Vehicle;

   procedure Set_Reservation(V : in out Vehicle) is
   begin
      V.Is_Reserved := True;
   end Set_Reservation;
   

end Vehicle_System;
