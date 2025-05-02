package Vehicle_System is

   -- This package provides procedures for controlling the vehicle system.
   -- It includes procedures for starting and stopping the engine,   
   -- setting the speed, and checking the vehicle's status.
   -- It also includes procedures for reserving the vehicle and checking the reservation status.
   -- The package uses Ada.Text_IO for input and output operations.
   -- The package is designed to be used in a vehicle reservation system.
   -- The package defines a tagged record type for the vehicle, which includes attributes such as
   -- engine status, reservation status, and door status.
   -- The package also defines an abstract tagged record type for the vehicle, which allows for
   -- polymorphism and inheritance in the vehicle system.
   -- The package includes procedures for controlling the engine, movement, and reservation status.
   -- The package is designed to be used in a vehicle reservation system.
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
  
   -- This procedure starts the engine of the vehicle.
   -- It takes a Vehicle record as input and modifies its Engine_On attribute.
   -- The procedure checks if the vehicle is already reserved and if the doors are closed before starting the engine.
   -- If the engine is successfully started, it sets the Is_Moving attribute to True.
   procedure Start_Engine (V : in out Vehicle);

   -- This procedure stops the engine of the vehicle.
   -- It takes a Vehicle record as input and modifies its Engine_On attribute.
   -- The procedure checks if the vehicle is already reserved and if the doors are closed before stopping the engine.
   -- If the engine is successfully stopped, it sets the Is_Moving attribute to False.
   -- It also sets the Speed attribute to 0.0.
   procedure Stop_Engine (V : in out Vehicle);

   -- This procedure sets the speed of the vehicle.
   -- It takes a Vehicle record as input and modifies its Speed attribute.
   -- If the speed is successfully set, it sets the Is_Moving attribute to True.
   -- The procedure also checks if the speed is within a valid range (0.0 to 100.0).
   -- If the speed is not within the valid range, it raises a Constraint_Error exception.
   procedure Set_Speed(V : in out Vehicle; New_Speed : Float);

   -- This function checks if the vehicle is mobile.
   -- It takes a Vehicle record as input and returns a Boolean value indicating
   -- whether the vehicle is mobile or not.
   -- The function returns the state of the vehicle based on the Engine_On and Is_Moving attributes.
   function Vehicle_Mobile (V : in out Vehicle) return Boolean is abstract;

   -- This function returns the state of the vehicle doors.
   -- It takes a Vehicle record as input and returns a Boolean value indicating
   -- whether the doors are closed or not.
   -- The function checks the Current_Door_Status attribute of the vehicle.
   -- If the doors are closed, it returns True; otherwise, it returns False.
   function is_Door_Closed(V : in out Vehicle) return Boolean is abstract;

   -- This procedure checks the speed of the vehicle.
   -- It takes a Vehicle record as input and checks if the speed is within a valid range (0.0 to 100.0).
   -- If the speed is not within the valid range, it raises a Constraint_Error exception.
   procedure Vehicle_NotMobile (V : in out Vehicle);

   -- Procedures to reserve the vehicle
   function Reserve_Vehicle(V :  Vehicle) return Boolean;

   procedure Set_Reservation(V : in out Vehicle);


end Vehicle_System;
