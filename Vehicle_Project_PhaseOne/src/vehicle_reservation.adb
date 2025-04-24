with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Make_Reservation is
   Res       : Reservation;
   Input_Str : String (1 .. 50);
   Last      : Natural;
   Valid_CC  : Boolean := False;
   Choice    : Integer;
begin
   -- Prompt for Name
   Put("Enter your name: ");
   Get_Line(Input_Str, Last);
   Res.Name := Input_Str(1 .. Last);

   -- Prompt for Credit Card Number with validation
   loop
      Put("Enter your 10-digit credit card number: ");
      Get_Line(Input_Str, Last);
      if Last = 10 then
         Valid_CC := True;
         for I in 1 .. 10 loop
            if not Input_Str(I) in '0' .. '9' then
               Valid_CC := False;
               exit;
            end if;
         end loop;
      end if;
      exit when Valid_CC;
      Put_Line("Invalid input. Please enter exactly 10 digits.");
   end loop;
   Res.Credit_Card := Input_Str(1 .. 10);

   -- Prompt for Vehicle Type
   loop
      Put_Line("Select vehicle type:");
      Put_Line("1. Standard");
      Put_Line("2. Luxury");
      Put("Enter choice (1 or 2): ");
      Get(Choice);
      if Choice = 1 then
         Res.Car_Type := Standard;
         Res.Rental_Fee := 50.0;
         Res.MPG := 30.0;
         exit;
      elsif Choice = 2 then
         Res.Car_Type := Luxury;
         Res.Rental_Fee := 100.0;
         Res.MPG := 20.0;
         exit;
      else
         Put_Line("Invalid choice. Please select 1 or 2.");
      end if;
   end loop;

   -- Display Reservation Summary
   Put_Line("Reservation Summary:");
   Put_Line("Name: " & Res.Name);
   Put_Line("Credit Card: " & Res.Credit_Card);
   Put_Line("Vehicle Type: " & Vehicle_Type'Image(Res.Car_Type));
   Put_Line("Rental Fee: $" & Float'Image(Res.Rental_Fee));
   Put_Line("MPG: " & Float'Image(Res.MPG));
end Make_Reservation;
