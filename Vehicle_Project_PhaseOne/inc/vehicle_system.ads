package Vehicle_System is
   
   type Vehicle is abstract tagged record
      Engine_On : Boolean := False;
      Is_Reserved : Boolean := False;
      Is_Moving : Boolean := False;
      Number_Of_Doors : Integer;
      MPG : Float := 0.0;
      Speed : Float := 0.0;
   end record;
  
   -- Procedures to control the Engine
   procedure Start_Engine (V : in out Vehicle);
   procedure Stop_Engine (V : in out Vehicle);
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float);

   -- Procedures to control the movement
   procedure Vehicle_Mobile (V : in out Vehicle);
   procedure Vehicle_NotMobile (V : in out Vehicle);

   -- Procedures to reserve the vehicle
   function Reserve_Vehicle(V :  Vehicle) return Boolean;

   procedure Set_Reservation(V : in out Vehicle);


end Vehicle_System;
