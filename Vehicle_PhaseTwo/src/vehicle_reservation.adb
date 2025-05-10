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

  
  --- This procedure prompts the user for their name and stores it in the Reservation record.
   -- It uses Ada.Text_IO for input and output operations.
   -- The procedure checks the length of the name to ensure it does not exceed 50 characters.
   procedure Input_Driver_Name(Res : in out Reservation) is
   begin  
      -- Prompt for Name
      loop           
         Put("Enter your name (max 50 characters): ");
         Get_Line(Res.Name, Res.Last);
         Put_Line("DEBUG: Name_Last = " & Integer'Image(Res.Last));
         if Res.Last > 50 then
            Put_Line("Name too long. Please enter a name with a maximum of 50 characters.");
         else
            exit;
         end if;
      end loop;
   end Input_Driver_Name;

   -- This procedure prompts the user for their credit card number and validates it.
   -- It uses Ada.Text_IO for input and output operations.
   -- The procedure checks the length of the credit card number to ensure it is exactly 10 digits.
   -- It also checks if the input contains only digits.
   -- If the input is valid, it stores the credit card number in the Reservation record.
   procedure Input_Credit_Card(Res : in out Reservation) is
   begin
      -- Prompt for Credit Card Number with validation
      loop
         Put("Enter your 10-digit credit card number: ");
         Get_Line(Res.Credit_Card, Res.Card_Last);
         if Res.Card_Last = 10 then
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



end Vehicle_Reservation;

