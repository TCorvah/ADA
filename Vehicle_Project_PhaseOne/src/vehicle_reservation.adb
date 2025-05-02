with Ada.Text_IO;  use Ada.Text_IO;     
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Standard_Vehicle; use Standard_Vehicle;
with Luxury_Vehicle; use Luxury_Vehicle;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Vehicle_Constants; use Vehicle_Constants;
with Vehicle_Types; use Vehicle_Types;


package body Vehicle_Reservation is
   package Float_IO is new Ada.Text_IO.Float_IO(Float);
   -- This package provides procedures for making and managing vehicle reservations.
   -- It includes input validation and reservation confirmation.
   -- This procedure prompts the user for their name and stores it in the Reservation record.
   -- It uses Ada.Text_IO for input and output operations.
   -- The procedure is called from the Make_Reservation procedure to gather user information.
  
   procedure Input_Driver_Name(Res : in out Reservation) is
   begin  
      -- Prompt for Name
      loop           
         Put("Enter your name (max 50 characters): ");
         Get_Line(Res.Name, Res.Last);
         if Res.Last > 50 then
            Put_Line("Name too long. Please enter a name with a maximum of 50 characters.");
         else
            exit;
         end if;
      end loop;
   end Input_Driver_Name;


   procedure Input_Credit_Card(Res : in out Reservation) is
   begin
      -- Prompt for Credit Card Number with validation
      loop
         Put("Enter your 10-digit credit card number: ");
         Get_Line(Res.Credit_Card, Res.Last);
         if Res.Last = 10 then
            for I in 1 .. 10 loop
               if Res.Credit_Card(I) not in '0' .. '9' then
                  Put_Line("Invalid input. Please enter exactly 10 digits.");
                  exit;
               end if;
            end loop;
            exit;
         else
            Put_Line("Invalid input. Please enter exactly 10 digits.");
         end if;
      end loop;
   end Input_Credit_Card;

   

   -- Define the Reservation record type
   -- This record holds the details of a vehicle reservation
   -- including the name of the customer, credit card number, and vehicle type.
   -- The record is used to store the reservation details and pass them between procedures.
   -- The record is defined as a fixed-length string for the name and credit card number.
   -- The vehicle type is defined using the Vehicle_Types package.

   procedure Make_Reservation(Res : in out Reservation) is
      Input_Str : String (1 .. 50);
      Last      : Natural;
      Valid_CC  : Boolean := False;
      Choice    : Integer := 0;
      -- Trimmed_Name : String;   
   begin
     Input_Driver_Name(Res);
     Input_Credit_Card(Res);
     if Choice = 1 then
        Res.Car_Type := Standard_cars;
        Res.Rental_Fee := Vehicle_Constants.Standard_Rental_Cost;
        Res.MPG := Vehicle_Constants.Standard_MPG;
        Put_Line("Standard Car Type: " & Trim(Vehicle_Type'Image(Res.Car_Type),Right));
      elsif Choice = 2 then 
        Res.Car_Type := Vehicle_Types.Luxury_cars;
        Res.Rental_Fee := Vehicle_Constants.Luxury_Rental_Cost;
        Res.MPG := Vehicle_Constants.Luxury_MPG;
        Put_Line("Luxury Car Type: " & Trim(Vehicle_Type'Image(Luxury_cars),Right));
      else
        Put_Line("Invalid vehicle type selected.");
        return;
      end if;   
   end Make_Reservation;

   procedure Display_Reservation(Res : in out  Reservation) is
   begin
      Make_Reservation(Res);
      Put_Line("Customer: " & Res.Name(1 .. Res.Last));
      --Put_Line("Credit Card: " & Res.Credit_Card);
      --Put_Line("Car Type: " & Trim(Vehicle_Type'Image(Res.Car_Type),Right));
      --Put("Rental Fee: $");
      -- Display the rental fee with 2 decimal places
      -- Use Float_IO to format the output
      --Res.Rental_Fee := Vehicle_Constants.Get_Rental_Cost(Res.Car_Type);
      --Float_IO.Put(Res.Rental_Fee, Fore => 1, Aft => 2, Exp => 0);
      --New_Line;
      --Put("MPG: ");
      --Float_IO.Put(Res.MPG, Fore => 1, Aft => 1, Exp => 0);
      --New_Line;
   end Display_Reservation;

   


end Vehicle_Reservation;

