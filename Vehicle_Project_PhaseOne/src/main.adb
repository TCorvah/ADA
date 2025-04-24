with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle;
with Sensor_System;

procedure Main is
   L : Luxury_Vehicle.Luxury_Car;
   S : Sensor_System.Sensor;

begin

   -- Activate the sensor
   Sensor_System.Activate_Sensor(S);

   -- Simulate door toggle
   Sensor_System.Toggle_Door(S);
   Sensor_System.Toggle_Door(S);

   -- Simulate seat occupancy and seatbelt status
   S.Detected_Weight := 75.0;
   S.Seatbelt_On := True;
   Sensor_System.Check_Seat(S);

   -- Simulate visibility conditions
   S.Visibility := 0.3;
   Sensor_System.Update_Headlights(S);
   Luxury_Vehicle.Attempt_Move(L);
end Main;
