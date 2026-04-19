with Vehicle_Constants; use Vehicle_Constants;
package Vehicle_System is
   
   -- enums
   type Vehicle_CarType is (Luxury_Car, Standard_Car);
   type Engine_Status_Type is (Engine_Off, Engine_On);
   type SeatBelt_Status_Type is (Seatbelt_Off, Seatbelt_On);
   type Door_Status_Type is (Door_Closed, Door_Open);

   -- Abstract Vehicle type with private fields
   type Vehicle is abstract tagged private;

   function Get_Vehicle_Type(V : Vehicle) return Vehicle_CarType;
   -- getters, the functions are for queries of the vehicle state
   function Get_Speed return Float;
   function Get_Engine_Status(V : Vehicle) return Engine_Status_Type;
   function Get_Is_Moving return Boolean;
   function Get_Seatbelt_Status return SeatBelt_Status_Type;
   -- vehicle movement (polymorphic) as each vehicle type has different movement rules
   function Vehicle_Mobile return Boolean is abstract;
      -- Abstract class that must be overridden in derived types
   function is_Door_Closed return Boolean is abstract;
   function Get_Door_Status return Door_Status_Type is abstract;
     -- Reservation controls
   function Reserve_Vehicle return Boolean;

   -- Engine controls 
   procedure Start_Engine(V : in out Vehicle);
   procedure Stop_Engine(V : in out Vehicle);
   -- Setters, the procedure are for mutation of the vehicle state
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float);
   --- procedure Set_Road_Profile(V : in out Vehicle; Profile : Road_Profile);
   procedure Set_Engine_Status(V : in out Vehicle; On : Engine_Status_Type);
   procedure Set_Door_Status(V : in out Vehicle; Status : Door_Status_Type);
   -- use across derived types
   procedure Vehicle_NotMobile(V : in out Vehicle);

   procedure Set_Reservation(V : in out Vehicle);
   -- Attempt to move (must be overridden), polymorhic in nature
   procedure Attempt_Move(V : in out Vehicle; Threshold : in Float) is abstract;



private
   -- Hidden implementation details
   type Vehicle is abstract tagged record
      Vehicle_Type     : Vehicle_CarType;
      Engine_Status    : Engine_Status_Type;
      SeatBelt_Status  : SeatBelt_Status_Type;
      Is_Reserved  : Boolean;
      Is_Moving      : Boolean;
      Doors_Closed    : Boolean;
      Speed               : Float;
      Current_Door_Status : Door_Status_Type;
      Vehicle_Occupied    : Boolean;      
   end record;
 
end Vehicle_System;
