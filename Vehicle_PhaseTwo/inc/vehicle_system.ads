with Road_ProfileConfig; use Road_ProfileConfig;
package Vehicle_System is

   -- Status type for door
   type Door_Status_Type is (Door_Closed, Door_Open);

   -- Abstract Vehicle type with private fields
   type Vehicle is abstract tagged private;

   -- Engine controls
   procedure Start_Engine(V : in out Vehicle);
   procedure Stop_Engine(V : in out Vehicle);

   -- Speed controls
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float);
   procedure Set_Road_Profile(V : in out Vehicle; Profile : Road_Profile);

   -- Movement and door checks (polymorphic)
   function Vehicle_Mobile(V : in out Vehicle) return Boolean is abstract;
   function is_Door_Closed(V : in out Vehicle) return Boolean is abstract;

   -- Speed validity enforcement
   procedure Vehicle_NotMobile(V : in out Vehicle);

   -- Reservation controls
   function Reserve_Vehicle(V : Vehicle) return Boolean;
   procedure Set_Reservation(V : in out Vehicle);

   -- Attempt to move (must be overridden)
   procedure Attempt_Move(V : in out Vehicle; Threshold : in Float) is abstract;

   -- === Getters ===
   function Get_Engine_Status(V : Vehicle) return Boolean;
   function Get_Is_Moving(V : Vehicle) return Boolean;
   function Get_Door_Status(V : Vehicle) return Door_Status_Type;
   function Get_Speed(V : Vehicle) return Float;


   -- === Setters ===
   procedure Set_Engine_Status(V : in out Vehicle; On : Boolean);

   procedure Set_Door_Status(V : in out Vehicle; Status : Door_Status_Type);


private

   -- Hidden implementation details
   type Vehicle is abstract tagged record
      Engine_On            : Boolean := False;
      Is_Reserved          : Boolean := False;
      Is_Moving            : Boolean := False;
      Doors_Closed         : Boolean := True;
      Speed               : Float   := 0.0;
      Current_Door_Status : Door_Status_Type := Door_Closed;
      Current_Road : Road_Profile;
   end record;
 
end Vehicle_System;
