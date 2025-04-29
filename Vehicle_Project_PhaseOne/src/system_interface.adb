with Ada.Text_IO; use Ada.Text_IO;
with Vehicle_System; use Vehicle_System;
with Radar_Systems; use Radar_Systems;
with Sensor_System; use Sensor_System;
with Luxury_Vehicle; use Luxury_Vehicle;
with Standard_Vehicle; use Standard_Vehicle;
with Vehicle_Constants; use Vehicle_Constants;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body System_Interface is

   procedure Run_System_Interface( Vehicles : in out Luxury_Vehicle.Luxury_Car) is
      -- Declare variables
      Scenario : Integer;
      Detected_Distance : Float;   
   begin
      -- Prompt the user for the time of day
      Put_Line("Enter the time of day (0.0 - 1.0): ");
      Get(Vehicles.Car_Sensor.Visibility);

      if Vehicles.Car_Sensor.visibility < 0.0 or Vehicles.Car_Sensor.visibility > 1.0 then
         Put_Line("Invalid time of day. Please enter a value between 0.0 and 1.0.");
         return;
      end if;
      -- Set visibility based on the time of day
      Sensor_System.Check_Visibility(Vehicles.Car_Sensor);
 
      Sensor_System.Update_Headlights(Vehicles.Car_Sensor);
     
      -- Prompt the user for the detected weight
      Put_Line("Enter the detected weight (in kg): ");
      Get(Detected_Distance);
      -- Set detected weight
      Vehicles.Car_Sensor.Detected_Weight := Detected_Distance;
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
   end Run_System_Interface;
  
   
 
end System_Interface;
