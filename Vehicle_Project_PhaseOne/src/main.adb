with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Standard_Vehicle, Sensor_System, Radar_Systems;
with System_Interface;
with Vehicle_Types;
with Vehicle_Reservation;


procedure Main is
   -- Declare the vehicle types
   User_Choice : Vehicle_Types.Vehicle_Type;
   Res : Vehicle_Reservation.Reservation;
   -- Declare user input variables
   Input_Choice : Integer;
begin
   Put_Line("Vehicle Project - Phase One");
   Put_Line("=====================================");
   Put_Line("Luxury Vehicle Simulation");
   Put_Line("=====================================");
   Put_Line("Initializing vehicle systems...");
   System_Interface.Run_System_Interface(User_Choice);

   --Vehicle_Reservation.Display_Reservation(Res);
   -- Display the reservation details

      


end Main;
