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

package body System_Interface is

   procedure Activate_TOD(Time : in out Sensor_System.Sensor ) is
      -- Declare variables
      TOD : Integer; -- Time of day
   begin
      -- Prompt the user for the time of day
      Put_Line("=====================================");
      Put_Line("Vehicle Project - Phase One");
      Put_Line("Enter the time of day (0.0 - 1.0): ");
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

      Activate_TOD (Vehicles.Car_Sensor);
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
            Vehicles.Car_Radar.Object_Distance := 80.8; -- 8 meters
            Put_Line("Scenario: Highway");
            Luxury_Vehicle.Attempt_Move (Vehicles, Vehicles.Car_Radar.Object_Distance);
            Luxury_Vehicle.Reduce_Speed (Vehicles, Vehicle_Constants.Threshold);
         when others =>
            Put_Line("Invalid scenario selected.");
            return;
      end case;

   end Run_Luxury_Scenario;
  
   procedure Run_Standard_Scenario( Vehicles : in out Standard_Vehicle.Standard) is
      -- Declare variables
      Detected_Weight : Float;  
      TOD : Integer; -- Time of day
      Scenario : Integer;
   begin
      --Put_Line(Trim(Vehicles.Model 'Image(Model)) & ": $ " & Trim(Float'Image(Cost)) & " MPG: " & Trim(Float'Image(Miles_Per_Gallon)));
      -- Prompt the user for the time of day
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


   procedure Run_System_Interface( Selected_Type : in out Vehicle_Types.Vehicle_Type) is
      -- Declare variables
      Scenario : Integer;
      Detected_Distance : Float;  
      std: Standard_Vehicle.Standard;
      lux: Luxury_Vehicle.Luxury_Car;
   begin
      -- Prompt the user for the vehicle type
      Put_Line("Select the Vehicle Type: ");
      Put_Line("1. Luxury Vehicle");
      Put_Line("2. Standard Vehicle");
      Get(Scenario);
      -- Set the vehicle type
      case Scenario is
         when 1 =>
            Run_Luxury_Scenario(lux);
         when 2 =>
            Run_Standard_Scenario(std);
         when others =>
            Put_Line("Invalid vehicle type selected.");
            return;
      end case;



   end Run_System_Interface;
   
 
end System_Interface;
