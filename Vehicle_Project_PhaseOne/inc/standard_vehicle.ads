
with Sensor_System, Vehicle_System;

package Standard_Vehicle is

type Standard is new Vehicle.Vehicle_Type with record
   Sensors : Sensor.Sensor_System;
   Number_Of_Passengers : Integer := 0;
   
   
end record;

procedure SeatBelt_Warning(V : Standard);
procedure Check_Sensors(V : in Standard);



end Standard_Vehicle;


