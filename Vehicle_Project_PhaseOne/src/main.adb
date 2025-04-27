with Ada.Text_IO; use Ada.Text_IO;
with Luxury_Vehicle, Standard_Vehicle, Sensor_System, Radar_Systems;


procedure Main is
   L : Luxury_Vehicle.Luxury_Car;
   S : Sensor_System.Sensor;
   R : Radar_Systems.Radar;

begin

   -- Activate the sensor
   ---- Sensor_System.Activate_Sensor(S);
      -- Simulate seat occupancy and seatbelt status
   L.Car_Sensor.Detected_Weight := 75.0;
   L.Car_Sensor.Seatbelt_On := True;
     -- Simulate visibility conditions, this is a night drive
   L.Car_Sensor.Visibility := 0.3;
   L.Car_Radar.Object_Distance := 0.04;


 
   Luxury_Vehicle.Attempt_Move(L);
   Luxury_Vehicle.Reduce_Speed(L, 0.0);
   Luxury_Vehicle.Turn_Off_Engine(L);
   Luxury_Vehicle.Update_Door_Status(L);

end Main;
