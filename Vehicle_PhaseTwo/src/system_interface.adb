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
with Timing_Controller; use Timing_Controller;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;


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
      Put_Line("Vehicle Project - Phase Two");
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
            Put_Line("Invalid time of day. Please enter option 1 or 2.");
            return;
      end case;
   end Activate_TOD;

   -- Procedure to run the luxury vehicle interface
   procedure Run_Luxury_Scenario( Vehicles : in out Luxury_Vehicle.Luxury_Car) is
      -- Declare variables
      Scenario : Integer;
      SeatBelt : Integer;
      my_radar : Radar_Systems.Radar;
      my_sensor : Sensor_System.Sensor;
      Radar_Data : Radar_Systems.Radar_Data;
   
      -- Random distance for object detection
      Random_Speed : Float;
      Random_Distance : Float;
 
   begin
      Put_Line("Car Type: " & Trim(Vehicle_Type'Image(Vehicles.Lux_Model),Right));
      Put("Rental Fee: $");
      Float_IO.Put(Vehicles.luxury_cost, Fore => 1, Aft => 2, Exp => 0);
      New_Line;

      Put("MPG: ");
      Float_IO.Put(Vehicles.miles_gallon, Fore => 1, Aft => 1, Exp => 0);
      New_Line;
      Sensor_System.Activate_Sensor(Vehicles.Car_Sensor);
      if Sensor_System.Seat_Occupied(Vehicles.Car_Sensor) then
         Put_Line("Seat is occupied. Seatbelt confirmation will confirm human.");
      else
         Put_Line("Seat is not occupied. Vehicle will not move.");
         Vehicles.Speed := 0.0;
         Vehicles.Is_Moving := False;
         return;
      end if;

      Put_Line ("User enters Vehicle. Weight detected");
      Put_Line ("Engine is running");
      Activate_TOD(Vehicles.Car_Sensor);
  
      -- Prompt the user for the Seatbelt status
      Put_Line ("Enter the seatbelt status : ");
      Put_Line("1. Fastened");
      Put_Line("2. Not Fastened");
      Put_Line("3. Unknown");
      Put_Line("=====================================");
      Get(SeatBelt);
      -- Set the seatbelt status based on user input
      case SeatBelt is
         when 1 =>
            Vehicles.Car_Sensor.Seatbelt_On := True; -- Seatbelt fastened
            Vehicles.Engine_On := True; -- Engine is running
            my_sensor.Door_Open := False; -- Door is closed

            Put_Line("Seatbelt status: Fastened");
            Put_Line ("Sensor: Human weight confirmed");
            
         -- Seatbelt fastened
         when 2 =>
            Vehicles.Car_Sensor.Seatbelt_On := False; -- Seatbelt not fastened
            Put_Line("Seatbelt status: Not Fastened");
         when 3 =>
            Vehicles.Car_Sensor.Seatbelt_On := False; -- Unknown status, assume not fastened
            Put_Line("Seatbelt status: Unknown");
         when others =>
            Put_Line("Invalid seatbelt status selected.");
            return;
      end case;
          -- Set visibility based on the time of day
      if not Vehicles.Car_Sensor.Seatbelt_On then
         Put_Line("Warning: Seatbelt is not fastened. Vehicle will not move.");
         Vehicles.Speed := 0.0;
         Vehicles.Is_Moving := False;
         return;
      end if;

      Sensor_System.Check_Visibility(Vehicles.Car_Sensor);
 
      Sensor_System.Update_Headlights(Vehicles.Car_Sensor);
    

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
            Activate_Radar(Vehicles.Car_Radar);
            declare
               Gen : Ada.Numerics.Float_Random.Generator;
               speed_min : Float := 5.0; -- Speed of the vehicle
               speed_max : Float := 60.0; -- Speed of the vehicle


            begin
              -- Get radar data:
               -- Initialize the random number generator
               Ada.Numerics.Float_Random.Reset(Gen);
               -- Generate a random distance between 0.5 and 100 meters
               Random_Distance := Ada.Numerics.Float_Random.Random(Gen) * 99.5 + 0.5;
               Put_Line("Random Distance: " & Float'Image(Random_Distance));
               Vehicles.Car_Radar.Object_Distance := Random_Distance;
               Ada.Numerics.Float_Random.Reset(Gen); -- Reset the generator for speed
               Random_Speed := Ada.Numerics.Float_Random.Random(Gen) * (speed_max - speed_min) + speed_min;
               Put("Initial Speed: ");
               Float_IO.Put(Random_Speed, Fore => 1, Aft => 2, Exp => 0);
               New_Line;
  
               Vehicles.Speed := Random_Speed; -- Set a random speed for the vehicle
               Radar_Data := Radar_Systems.Analyze_Radar_Data(Vehicles.Car_Radar.Object_Distance); 
            end;
            --Ada.Numerics.Float_Random.Reset(Gen); 

            --Random_Distance := Ada.Numerics.Float_Random.Random(Gen) * 99.5 + 0.5; -- Random distance between 0.5 and 100 meters
           -- Put_Line ( "Random Distance: " & Float'Image(Random_Distance));
            --Vehicles.Car_Radar.Object_Distance := Random_Distance;
            
            Enable_Object_Detection(Vehicles);
            -- Vehicles.Car_Radar.Object_Distance := Random_Distance; -- Set the random distance for object detection
            -- random messgage that car is now parked
            Put_Line("Radar Decision: " & Radar_Systems.Radar_Data'Image(Radar_Data));

            case Radar_Data is
               when Emergency_Stop =>
                  Put_Line("Emergency Stop: Object detected too close. Vehicle will not move.");
                  Vehicles.Speed := 0.0; -- Stop the vehicle
                  Vehicles.Is_Moving := False;
               when Slow_Down =>
                  Put_Line("Slow Down: Object detected within threshold. Reducing speed.");
                  Vehicles.Speed := Vehicles.Speed * 0.5; -- Reduce speed by half
               when Caution =>
                  Put_Line("Caution: Object detected at a safe distance. Proceed with caution.");
                  Vehicles.Speed := Vehicles.Speed * 0.75; -- Reduce speed by 25%
                  Put_Line (" Caution: Object detected at a safe distance. Proceed with caution.");
                  Vehicles.Is_Moving := True; -- Vehicle can proceed
               when Clear_To_Move =>
                  Put_Line("Clear to Move: No obstacles detected. Vehicle can proceed.");
                  Vehicles.Speed := Random_Speed; -- Maintain current speed
                  Vehicles.Is_Moving := True; -- Vehicle can proceed
                   Put ("Vehicle is moving at speed: "); 
               when others =>
                  Put_Line("Unknown radar data. Vehicle will not move.");
                  Vehicles.Min_Speed := 0.0; -- Stop the vehicle
                  Vehicles.Is_Moving := False;
            end case;
            if Vehicles.Is_Moving then
               Timing_Controller.Garage_Movement_Controller_task.Start_Garage_Movement_Timer(Vehicles.Max_Speed, Vehicles.Min_Speed); -- Start the parking garage timer
               delay 2.0; -- Simulate time delay for parking garage scenario
               Put_Line("Vehicle navigating movement in garage.");
               --shutdown and clean up
               Timing_Controller.Garage_Movement_Controller_task.Shutdown;
               Turn_Off_Engine(Vehicles);
            else
               Put_Line("Vehicle is not moving.");
            end if;
        
           

         when 2 =>
            --Vehicles.Car_Radar.Object_Distance := 110.0; -- 110 meters
            Put_Line("Scenario: Quiet Country Road");
            --Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            --Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when 3 =>
            Put_Line("Scenario: Busy City Street");
            Enable_Object_Detection(Vehicles);
            --Vehicles.Car_Radar.Object_Distance := 10.0; -- 10 meters
        
            --Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            --Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when 4 =>
            Put_Line("Scenario: Highway");
         
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
