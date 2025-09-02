
with Sensor_System, Vehicle_System;
with Vehicle_Constants; use Vehicle_Constants;

with Vehicle_Types;

package Standard_Vehicle is
   -- This package provides procedures and functions for managing standard vehicles.
   -- It includes procedures for checking seatbelt status, sensor status, and vehicle mobility.
   -- It also defines the Standard vehicle type and its attributes.
   -- The Standard vehicle type is a record that includes attributes such as the number of passengers,
   -- model, cost, and miles per gallon.
   -- The package uses the Vehicle_System and Sensor_System packages for vehicle and sensor management.
   -- The package also uses the Vehicle_Types package for vehicle type definitions.
   -- The Standard vehicle type is defined as a record with the following attributes:
   -- Car_Sensor: Sensor_System.Sensor;
   -- Number_Of_Passengers: Integer;
   -- Model: Vehicle_Types.Vehicle_Type;
   -- Cost: Float;
   -- Miles_Per_Gallon: Float;
   -- The package provides the following procedures and functions:
   -- SeatBelt_Warning: This procedure checks the seatbelt status and displays a warning if not fastened.
   -- Check_Sensors: This procedure checks the status of the sensors and updates the vehicle's systems.
   -- Vehicle_Mobile: This function checks if the vehicle is mobile based on the detected weight and seatbelt status.
   -- is_Door_Closed: This function checks if the door is closed based on the sensor status.
   -- The package uses the Sensor_System package for sensor management and the Vehicle_System package for vehicle management.
   -- The package also uses the Vehicle_Types package for vehicle type definitions.
   -- The package is designed to be used in a vehicle simulation system.

   
   type Standard is new Vehicle_System.Vehicle with record
      Car_Sensor : Sensor_System.Sensor;
      Number_Of_Passengers : Integer := 0;
      Model : Vehicle_Types.Vehicle_Type := Vehicle_Types.Standard_cars;
      Cost : Float := 50.0;
      Miles_Per_Gallon : Float := 20.0; 
         Min_Speed : Float := Vehicle_Constants.Vehicle_Min_Speed; -- Speed of the luxury car
      Max_Speed : Float := Vehicle_Constants.Vehicle_Max_Speed; -- Maximum speed of the luxury car
end record;
   -- This procedure checks the seatbelt status and displays a warning if not fastened.
   -- It uses the Car_Sensor attribute of the Standard vehicle type to check the seatbelt status.
   -- The procedure is called from the Run_Standard_Scenario procedure to check the seatbelt status.
   -- The procedure uses the Sensor_System package to check the seatbelt status.
   -- The procedure is designed to be used in a vehicle simulation system.
   -- The procedure checks the seatbelt status and displays a warning if not fastened.
   -- The procedure uses the Car_Sensor attribute of the Standard vehicle type to check the seatbelt status.
   procedure SeatBelt_Warning(V : in out Standard);

   -- This procedure checks the status of the sensors and updates the vehicle's systems.
   -- It uses the Car_Sensor attribute of the Standard vehicle type to check the sensor status.
   -- The procedure is called from the Run_Standard_Scenario procedure to check the sensor status.
   -- The procedure uses the Sensor_System package to check the sensor status.
   procedure Check_Sensors(V : in out Standard);

   -- This function checks if the vehicle is mobile based on the detected weight and seatbelt status.
   -- It uses the Car_Sensor attribute of the Standard vehicle type to check the detected weight and seatbelt status.
   -- The function is called from the Run_Standard_Scenario procedure to check if the vehicle is mobile.
   -- The function uses the Sensor_System package to check the detected weight and seatbelt status.
   -- The function returns True if the vehicle is mobile and False otherwise.
   overriding function Vehicle_Mobile(V : Standard) return Boolean;

   -- This function checks if the door is closed based on the sensor status.     
   -- It uses the Car_Sensor attribute of the Standard vehicle type to check the door status.
   -- The function is called from the Run_Standard_Scenario procedure to check if the door is closed.
   -- The function uses the Sensor_System package to check the door status.
   -- The function returns True if the door is closed and False otherwise.
   -- The function is designed to be used in a vehicle simulation system.
   -- The function checks the door status and returns True if the door is closed and False otherwise.
   -- The function uses the Car_Sensor attribute of the Standard vehicle type to check the door status.
   procedure Update_Door_Status(V : in out Standard); 
   function is_Door_Closed(V : Standard) return Boolean;
     overriding
   function Get_Door_Status (V : Standard) return Vehicle_System.Door_Status_Type;
   overriding procedure Attempt_Move(Standard_Car : in out Standard; Threshold : in Float);
   

end Standard_Vehicle;


