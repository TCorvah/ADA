with Vehicle_Constants; use Vehicle_Constants;
with Vehicle_Types; use Vehicle_Types;
package Vehicle_Reservation is
   -- This package provides procedures for making and managing vehicle reservations.
   -- It includes input validation and reservation confirmation.

   -- Define the Reservation record type
   -- This record holds the details of a vehicle reservation
   -- including the name of the customer, credit card number, and vehicle type.
   -- The record is used to store the reservation details and pass them between procedures.
   -- The record is defined as a fixed-length string for the name and credit card number.
   -- The vehicle type is defined using the Vehicle_Types package.
   type Reservation is record
      Name         : String (1 .. 50);
      Last        : Natural;
      Credit_Card  : String (1 .. 10);
      Car_Type     : Vehicle_Types.Vehicle_Type;
      Rental_Fee   : Float; 
      MPG          : Float;
   end record;

   procedure Input_Driver_Name(Res : in out Reservation);
   procedure Input_Credit_Card(Res : in out Reservation);

   --procedure Input_Car_Type(Reservation : in out Reservation);
   procedure Display_Reservation(Res : in out Reservation);
   --procedure Confirm_Reservation(Reservation : in Reservation);
   --procedure Cancel_Reservation(Reservation : in out Reservation);
   procedure Make_Reservation(Res : in out Reservation);


end Vehicle_Reservation;
