package Vehicle_System is

   type Door_Status_Type is (Door_Closed,Door_Open); 
   type Vehicle is abstract tagged record
      Engine_On : Boolean := False;
      Is_Reserved : Boolean := False;
      Is_Moving : Boolean := False;
      Doors_Closed : Boolean := True;
      MPG : Float := 0.0;
      Speed : Float := 0.0;
      Current_Door_Status : Door_Status_Type := Door_Closed;
   end record;
  
   -- Procedures to control the Engine
   procedure Start_Engine (V : in out Vehicle);
   procedure Stop_Engine (V : in out Vehicle);
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float);

   -- Procedures to control the movement
   function Vehicle_Mobile (V : in out Vehicle) return Boolean is abstract;
   function is_Door_Closed(V : in out Vehicle) return Boolean is abstract;
   procedure Vehicle_NotMobile (V : in out Vehicle);

   -- Procedures to reserve the vehicle
   function Reserve_Vehicle(V :  Vehicle) return Boolean;

   procedure Set_Reservation(V : in out Vehicle);


end Vehicle_System;
