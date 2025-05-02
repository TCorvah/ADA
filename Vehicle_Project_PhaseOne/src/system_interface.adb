with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Vehicle_System; use Vehicle_System;
with Radar_Systems; use Radar_Systems;
with Sensor_System; use Sensor_System;
with Luxury_Vehicle; use Luxury_Vehicle;
with Standard_Vehicle; use Standard_Vehicle;
with Vehicle_Constants; use Vehicle_Constants;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Vehicle_Types; use Vehicle_Types;
with Ada.Strings; use Ada.Strings;
with Vehicle_Reservation; use Vehicle_Reservation;


package body System_Interface is
   -- This package implements the system interface for the vehicle project.
   -- It includes procedures to run the vehicle simulation, handle user input,
   -- and manage the vehicle's sensors and scenarios.
   -- The package provides a set of procedures to manage the vehicle system and ensure the vehicle's safety.
   -- The package also includes a procedure to handle object detection while the vehicle is in motion.
   -- The package is designed to be used in conjunction with the Vehicle_System and Sensor_System packages.
   -- The package provides a set of procedures to manage the vehicle system and ensure the vehicle's safety.

   package Float_IO is new Ada.Text_IO.Float_IO(Float);

   -- Procedure to activate the time of day
   -- This procedure prompts the user for the time of day and sets the visibility accordingly.
   -- It takes a Sensor object as an input parameter and modifies its visibility attribute.
   -- The procedure also prints a message indicating the time of day.
   -- The procedure is designed to be used in conjunction with the Sensor_System package.
   procedure Activate_TOD(Time : in out Sensor_System.Sensor ) is
      -- Declare variables
      TOD : Integer; -- Time of day
   begin
      -- Prompt the user for the time of day
      Put_Line("=====================================");
      Put_Line("Vehicle Project - Phase One");
      Put_Line("Enter the time of day: ");
      Put_Line("1 = Night, 2 = Day");
      Get(TOD);
      case TOD is
         when 1 =>
            Time.Visibility := Night; -- Night
            Put_Line("Time of day: Night");
         when 2 =>
            Time.Visibility := Day; -- Day
            Put_Line("Time of day: Day");
         when others =>
            Put_Line("Invalid time of day. Please enter a value between 0.0 and 1.0.");
            return;
      end case;
   end Activate_TOD;

   -- Procedure to run the luxury vehicle interface
   procedure Run_Luxury_Scenario( Vehicles : in out Luxury_Vehicle.Luxury_Car) is
      -- Declare variables
      Scenario : Integer;
      Detected_Weight : Float;  
      -- MPG : Float;    
   begin
      Put_Line("Car Type: " & Trim(Vehicle_Type'Image(Vehicles.Lux_Model),Right));
      Put("Rental Fee: $");
      Float_IO.Put(Vehicles.luxury_cost, Fore => 1, Aft => 2, Exp => 0);
      New_Line;
      Put("MPG: ");
      Float_IO.Put(Vehicles.miles_gallon, Fore => 1, Aft => 1, Exp => 0);
      New_Line;
      Activate_TOD(Vehicles.Car_Sensor);
      -- Set visibility based on the time of day
      Sensor_System.Check_Visibility(Vehicles.Car_Sensor);
 
      Sensor_System.Update_Headlights(Vehicles.Car_Sensor);
     
      -- Prompt the user for the detected weight
      Put_Line("Enter the detected weight (in kg): ");
      Get(Detected_Weight);
      -- Set detected weight
      Vehicles.Car_Sensor.Detected_Weight := Detected_Weight;
      Vehicles.Car_Sensor.Seatbelt_On := True; -- Simulate seatbelt status

      -- Prompt the user for the scenario
      Put_Line("Select the Scenario: ");
      Put_Line("1. Parking Garage");
      Put_Line("2. Quiet Country Road");
      Put_Line("3. Busy City Street");
      Put_Line("4. Highway");
      Get(Scenario);
      -- Set the scenario
      case Scenario is
         when 1 =>
            Vehicles.Car_Radar.Object_Distance := 0.2; -- 5 meters
            Put_Line("Scenario: Parking Garage");
            Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
            Luxury_Vehicle.Turn_Off_Engine (Vehicles);
            Luxury_Vehicle.Update_Door_Status(Vehicles);
         when 2 =>
            Vehicles.Car_Radar.Object_Distance := 110.0; -- 110 meters
            Put_Line("Scenario: Quiet Country Road");
            Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when 3 =>
            Vehicles.Car_Radar.Object_Distance := 10.0; -- 10 meters
            Put_Line("Scenario: Busy City Street");
            Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when 4 =>
            Vehicles.Car_Radar.Object_Distance := 80.8; -- 80 meters
            Put_Line("Scenario: Highway");
            Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when others =>
            Put_Line("Invalid scenario selected.");
            return;
      end case;

   end Run_Luxury_Scenario;

   -- Procedure to run the standard vehicle interface
   procedure Run_Standard_Scenario( Vehicles : in out Standard_Vehicle.Standard) is
      -- Declare variables
      Detected_Weight : Float;  
      TOD : Integer; -- Time of day
      Scenario : Integer;
   begin
      -- Prompt the user for the vehicle type
      Put_Line("Car Type: " & Trim(Vehicle_Type'Image(Vehicles.Model),Right));
      Put("Rental Fee: $");
      Float_IO.Put(Vehicles.Cost, Fore => 1, Aft => 2, Exp => 0);
      New_Line;
      Put("MPG: ");
      Float_IO.Put(Vehicles.Miles_Per_Gallon, Fore => 1, Aft => 1, Exp => 0);
      New_Line;
      Activate_TOD(Vehicles.Car_Sensor);
        -- Set visibility based on the time of day
      Sensor_System.Check_Visibility(Vehicles.Car_Sensor);
 
      Sensor_System.Update_Headlights(Vehicles.Car_Sensor);
     
      -- Prompt the user for the detected weight
      Put_Line("Enter the detected weight (in kg): ");
      Get(Detected_Weight);
      -- Set detected weight
      Vehicles.Car_Sensor.Detected_Weight := Detected_Weight;
      Vehicles.Car_Sensor.Seatbelt_On := True; -- Simulate seatbelt status

      -- Prompt the user for the scenario
      Put_Line("Select the Scenario: ");
      Put_Line("1. Parking Garage");
      Put_Line("2. Quiet Country Road");
      Put_Line("3. Busy City Street");
      Put_Line("4. Highway");
      Get(Scenario);
      -- Set the scenario
  
      case Scenario is
         when 1 =>
            Put_Line("Scenario: Parking Garage");
            Standard_Vehicle.SeatBelt_Warning(Vehicles);
            Standard_Vehicle.Check_Sensors(Vehicles);
        
         when 2 =>
            Put_Line("Scenario: Quiet Country Road");
         when 3 =>
            Put_Line("Scenario: Busy City Street");
         when 4 =>
            Put_Line("Scenario: Highway");
         when others =>
            Put_Line("Invalid scenario selected.");
            return;
      end case;

   end Run_Standard_Scenario;

   -- Procedure to run the system interface
   -- This procedure prompts the user for the vehicle type, runs the appropriate scenario,
   -- and handles the reservation process.
   -- It takes a Vehicle_Type object as an input parameter and modifies its status.
   -- The procedure also includes a reservation process for the selected vehicle type.
   -- The procedure is designed to be used in conjunction with the Vehicle_Reservation package.
   -- The procedure also includes a reservation process for the selected vehicle type.
   procedure Run_System_Interface(Selected_Type : in out Vehicle_Types.Vehicle_Type) is
      -- Declare variables
      Scenario     : Integer;
      User_Choice  : Integer;
      std          : Standard_Vehicle.Standard;
      lux          : Luxury_Vehicle.Luxury_Car;
      Res          : Vehicle_Reservation.Reservation;
   begin
      -- Prompt the user for the vehicle type
      Put_Line("Select the Vehicle Type: ");
      Put_Line("1. Standard Vehicle");
      Put_Line("2. Luxury Vehicle");
      Get(Scenario);
      Skip_Line;

      -- Set the vehicle type and run simulation
      case Scenario is
         when 1 =>
            Selected_Type := Vehicle_Types.Standard_cars;
            Run_Standard_Scenario(std);
         when 2 =>  
            Selected_Type := Vehicle_Types.Luxury_cars;  
            Run_Luxury_Scenario(lux);
         when others =>
            Put_Line("Invalid vehicle type selected.");
            return;
         end case;
      -- Prompt for reservation
      Put_Line("Would you like to reserve a vehicle? (1 = Yes, 2 = No)");
      Get(User_Choice);
      Skip_Line;
   if User_Choice = 1 then
      -- Input name only
      Vehicle_Reservation.Input_Driver_Name(Res);
      -- Input credit card number
      Vehicle_Reservation.Input_Credit_Card(Res);
      Skip_Line;
      -- Set car type based on what user selected
      Res.Car_Type := Selected_Type;
      if Selected_Type = Vehicle_Types.Standard_cars then
         Res.Rental_Fee := Vehicle_Constants.Standard_Rental_Cost;
         New_Line;
         Res.MPG := Vehicle_Constants.Standard_MPG;
         Put_Line("Customer: " & Res.Name(1 .. Res.Last));
         New_Line;
         Put_Line("Credit Card: " & Res.Credit_Card);
         Put_Line("Car Type: " & Vehicle_Types.Vehicle_Type'Image(Res.Car_Type));
         Put("Rental Fee: $");
         Float_IO.Put(Res.Rental_Fee, Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put("MPG: ");
         Float_IO.Put(Res.MPG, Fore => 1, Aft => 1, Exp => 0);
         New_Line;
         Put_Line("Congratulation!"  & Res.Name(1 .. Res.Last) & ", your reservation is complete.");
      elsif Selected_Type = Vehicle_Types.Luxury_cars then
         Res.Rental_Fee := Vehicle_Constants.Luxury_Rental_Cost;
         New_Line;
         Res.MPG := Vehicle_Constants.Luxury_MPG;
         Put_Line("Customer: " & Res.Name(1 .. Res.Last));
         New_Line;
         Put_Line("Credit Card: " & Res.Credit_Card);
         Put_Line("Car Type: " & Vehicle_Types.Vehicle_Type'Image(Res.Car_Type));
         Put("Rental Fee: $");
         Float_IO.Put(Res.Rental_Fee, Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put("MPG: ");
         Float_IO.Put(Res.MPG, Fore => 1, Aft => 1, Exp => 0);
         New_Line;
         Put_Line("Congratulation!"  & Res.Name(1 .. Res.Last) & ", your reservation is complete.");
      else
         Put_Line("Invalid vehicle type selected.");
         return;
      end if;
      -- Output confirmatio     
   else
      Put_Line("Exiting the program.");
      return;
   end if;
end Run_System_Interface;

 
end System_Interface;
