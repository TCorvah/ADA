
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
      Last        : Natural := 0;
      Credit_Card  : String (1 .. 10);
      Card_Last : Natural := 0;
      Car_Type     : Vehicle_Type;
      Rental_Fee   : Float := 0.0; 
      MPG          : Float := 0.0;
   end record;
   -- This procedure prompts the user for their name and stores it in the Reservation record.
   -- It uses Ada.Text_IO for input and output operations.
   procedure Input_Driver_Name(Res : in out Reservation);

   -- This procedure prompts the user for their credit card number and validates the input.
   -- It ensures that the credit card number is exactly 10 digits long and contains only numeric characters.
   -- The procedure uses a loop to repeatedly prompt the user until valid input is provided.
   procedure Input_Credit_Card(Res : in out Reservation);

   --procedure Input_Car_Type(Reservation : in out Reservation);
   --procedure Display_Reservation(Res : in out Reservation);
   --procedure Confirm_Reservation(Reservation : in Reservation);
   --procedure Cancel_Reservation(Reservation : in out Reservation);
   --procedure Make_Reservation(Res : in out Reservation);


end Vehicle_Reservation;
